;;; elfmt.el --- Code formatter for elisp *- lexical-binding: t -*-

;; Authors: Chris Rayner (dchrisrayner@gmail.com)
;; Created: Sep 5 2020
;; Keywords: lisp
;; URL: https://github.com/riscy/elfmt
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "24.3"))
;; Version: 0.0.0

;;; Commentary:

;; Simple code formatter for Emacs Lisp.  Features:
;;
;; - Won't format lines that end in '; nofmt'
;; - Won't format sexps in its exclusion list (see `elfmt-nofmt-sexps')
;; - Focuses on the placement of lists and (mostly) ignores atoms
;; - Tries to break at `fill-column', but lines may exceed this number
;;   due to inline comments, long literals, trailing sequences of closed
;;   parens, or matches on widows (see `elfmt-type-1-widows', etc.)
;; - Prefers "modern" elisp (old-style backquotes will cause it to halt)
;;
;; Usage:
;; - Type M-x elfmt to format the current buffer
;; - Type M-x elfmt-sexp to format the current sexp.
;; - Type M-x elfmt-mode to automatically format the buffer on save
;; - Type M-x elfmt-global-mode to enable it `elfmt-mode' everywhere

;;; Code:

(require 'cl-lib)
(declare-function doom-modeline-update-buffer-file-name "ext:doom-modeline" nil t)
(declare-function doom-modeline-update-buffer-file-state-icon "ext:doom-modeline" nil t)

(defgroup elfmt nil
  "Code formatter for elisp"
  :prefix "elfmt-"
  :group 'elfmt
  :link '(url-link :tag "URL" "https://github.com/riscy/elfmt")
  :link '(emacs-commentary-link :tag "Commentary" "shx.el"))

(defcustom elfmt-nofmt-sexps
  '()
  "List of sexps that `elfmt' won't apply formatting to."
  :link '(function-link elfmt-sexp)
  :type '(repeat string))

(defconst elfmt-type-1-widows
  (regexp-opt
   '("(declare\n"
     "(dolist\n"
     "(dotimes\n"
     "(if-let*\n"
     "(if-let\n"
     "(if\n"
     "#'(lambda\n"
     "(not\n"
     "(unless\n"
     "(when-let*\n"
     "(when-let\n"
     "(when\n"
     "(while\n"
     "(with-suppressed-warnings\n"
     "(with-current-buffer\n"))
  "To e.g. join '(while ...)' to its condition.")

(defconst elfmt-type-2-widows
  (format "%s [[:graph:]]+$"
          (regexp-opt
           '("(cl-defmethod"
             "(cl-defmacro"
             "(cl-defgeneric"
             "(cl-defsubst"
             "(cl-defun"
             "(defadvice"
             "(defclass"
             "(defmacro"
             "(defadvice"
             "(defsubst"
             "(defun")))
  "To e.g. join '(defun <name> ...)' to its argument list.")

(defconst elfmt-type-3-widows
  (format "%s [[:graph:]]+ [[:graph:]]+$" (regexp-opt '("(declare-function\n")))
  "To e.g. join '(declare-function <name> <file> ...)' to its argument list.")

;;;###autoload
(define-minor-mode elfmt-mode
  "Toggle elfmt-mode on or off."
  :lighter " fmt"
  (cond
   (elfmt-mode (add-hook 'before-save-hook #'elfmt))
   (t (remove-hook 'before-save-hook #'elfmt))))

;;;###autoload
(define-globalized-minor-mode elfmt-global-mode elfmt-mode elfmt--global-on)

(defun elfmt--global-on ()
  "Call the function `elfmt-mode' on appropriate buffers."
  (when (eq major-mode 'emacs-lisp-mode) (elfmt-mode +1)))

(defun elfmt ()
  "Format the current buffer."
  (interactive)
  (barf-if-buffer-read-only)
  (let ((start-time (current-time))
        (inhibit-modification-hooks t)            ; speedup
        (gc-cons-threshold most-positive-fixnum)) ; speedup
    (save-excursion
      (goto-char (point-max))
      (while (not (bobp))
        (backward-sexp)
        (elfmt--sexp)
        (syntax-ppss-flush-cache (point))) ; in lieu of modification hooks
      (while (re-search-forward "\n\\{3,\\}" nil t) (replace-match "\n\n")))
    (when (bound-and-true-p doom-modeline-mode) ; in lieu of modification hooks
      (doom-modeline-update-buffer-file-name)
      (doom-modeline-update-buffer-file-state-icon))
    (message "`elfmt' took %dms" (* 1000 (float-time (time-since start-time))))))

(defun elfmt-sexp ()
  "Format the current (top level) sexp."
  (interactive)
  (let ((start-time (current-time)))
    (save-excursion
      (forward-char 1)
      (while (ignore-errors (or (up-list) t))) (backward-sexp)
      (elfmt--sexp))
    (message "`elfmt-sexp' took %dms"
             (* 1000 (float-time (time-since start-time))))))

(defun elfmt--sexp ()
  "Format the sexp starting on the current line.
Only formats lists whose whose car is in `elfmt-nofmt-sexps'."
  ;; precond: point is on an sexp
  (let ((original-sexp (sexp-at-point)))
    (when (and
           original-sexp
           (listp original-sexp)
           (not (member (car original-sexp) elfmt-nofmt-sexps)))
      (goto-char (point-at-bol))
      (elfmt--map-sexp-lines #'elfmt--break-line)
      (elfmt--map-sexp-lines #'elfmt--join-line)
      (elfmt--map-sexp-lines #'elfmt--postprocess-line)
      (or
       (equal (sexp-at-point) original-sexp)
       (error "`elfmt' made a mistake on: %S" original-sexp)))))

(defun elfmt--map-sexp-lines (formatting-func)
  "Apply FORMATTING-FUNC to every line of the sexp at point."
  ;; precond: point is on an sexp
  (let ((start-of-sexp (point)))
    (while (< (point) (scan-sexps start-of-sexp 1))
      (or (elfmt--nofmt-line-p) (funcall formatting-func))
      (forward-line 1))
    (goto-char start-of-sexp)))

(defun elfmt--nofmt-line-p (&optional n)
  "Whether the line should not be formatted.
If N is specified, move the point up or down that many lines."
  (declare (side-effect-free t))
  (save-excursion
    (when n (forward-line n))
    (or
     (= (point-at-bol) (point-at-eol))
     (string=
      (buffer-substring (- (point-at-eol) 7) (point-at-eol))
      "; nofmt"))))

(defun elfmt--break-line ()
  "Break the line down at points where lists begin or end.
This step behaves a lot like Emacs's builtin `pp-buffer'."
  (elfmt--goto-eol-cleanup-whitespace)
  ;; break the line on open parentheses:
  (while (and
          ;; the following matches _any_ open paren, including ones in strings
          ;; and characters (i.e. ?\(); it also assumes all the elements are
          ;; separated by spaces. It would be more correct to use `scan-lists'
          ;; but that would make it harder to constrain the point to one line,
          ;; and would also be slower; `scan-lists' is also not foolproof e.g.
          ;; when the point is inside a string!
          (re-search-backward "[^(](" (point-at-bol) t)
          (or (forward-char 1) t)
          (skip-chars-backward "^[:space:]" (point-at-bol))
          (skip-chars-backward "[:space:]" (point-at-bol))
          (not (bolp)))
    (let ((state (syntax-ppss)))
      (and
       (not (nth 3 state))
       (not (nth 4 state))
       (open-line 1))))
  (elfmt--goto-eol-cleanup-whitespace)
  ;; break the line on close parentheses:
  (while (and
          (not (nth 4 (syntax-ppss)))
          (re-search-backward ")[^)]" (point-at-bol) t)
          (not (looking-back "^[ \t]*" (point-at-bol))))
    (unless (nth 3 (syntax-ppss))
      (forward-char 1)
      (open-line 1))))

(defun elfmt--goto-eol-cleanup-whitespace ()
  "Move point to the end of the line; cleanup trailing whitespace."
  (and
   (re-search-forward "[[:space:]]+$" (point-at-eol) t)
   (not (nth 3 (syntax-ppss)))
   (replace-match ""))
  (goto-char (point-at-eol)))

(defun elfmt--join-line ()
  "Join the current line up with the lines beneath it, when feasible."
  ;; precond: (eq (point) (point-at-bol))
  (funcall indent-line-function)
  (and  ; move to the innermost sexp
   (> (skip-chars-forward "(" (point-at-eol)) 0)
   (backward-char))
  (while (elfmt--join-line-p) (save-excursion (join-line 1))))

(defun elfmt--join-line-p ()
  "Whether to join the current line with the next.
This uses heuristics that disregard the contributions of trailing
comments, closing parentheses, and backslash abbreviations like
\\n, so lines may end up longer than `fill-column'."
  ;; precond: point is on an sexp
  (let ((sexp-str (thing-at-point 'sexp)))
    (and
     sexp-str
     (string-match "\n" sexp-str)  ; sexp crosses to the next line
     (setq sexp-str (format "%S" (car (read-from-string sexp-str))))
     (< (length sexp-str) (- fill-column (current-column))) ; mostly fits
     ;; NOTE: `join-line' on a line that ends with ?\( will delete whitespace,
     ;; so this line is at risk of containing "?\(sexp-str". elfmt currently has
     ;; no problem with this, but it could become a problem in the future:
     (< (cl-count ?\( sexp-str) 5)
     (save-excursion
       (and
        (not (elfmt--trailing-syntax))     ; not inside a string/comment
        (eq (forward-line 1) 0)            ; and the next line, if any:
        (not (looking-at "[[:space:]]*;")) ; ...is not a comment
        (not (elfmt--nofmt-line-p)))))))   ; ...is not nofmt

(defun elfmt--trailing-syntax ()
  "Does the end of the line terminate the syntax field?
Modifies the point due to calls to `syntax-ppss'."
  (let ((state (syntax-ppss (point-at-eol))))
    (or (nth 3 state) (nth 4 state))))

(defun elfmt--postprocess-line ()
  "Run postprocessors on the line at point.
Postprocessors will do things like fix single-line comments,
join widowed lines with the next line, and fix indentation."
  (goto-char (point-at-bol))
  (skip-chars-forward "[:space:]" (point-at-eol))
  (cond
   ((looking-at ";[^;]")
    (save-excursion (insert ";")))
   ((or
     (looking-at elfmt-type-1-widows)
     (looking-at elfmt-type-2-widows)
     (looking-at elfmt-type-3-widows)
     (looking-at "[`']?(+$")
     (looking-at "[`']?(let\\*?$")
     (looking-at "[`']?(lambda$")
     (looking-at ":[[:graph:]]+$")  ; symbols
     (elfmt--looking-at-orphan-parens))
    (elfmt--postprocess-join 1)))
  (funcall indent-line-function))

(defun elfmt--postprocess-join (n)
  "Join the current line with the next, N times, if possible."
  (dotimes (_ (or n 1))
    (or
     (elfmt--nofmt-line-p +1)
     (elfmt--trailing-syntax)
     (join-line 1))))

(defun elfmt--looking-at-orphan-parens ()
  "Return non-nil if there are orphan parens on the next line."
  (declare (side-effect-free t))
  (save-excursion (forward-line 1) (looking-at-p "[[:space:]]*)")))

(provide 'elfmt)
;;; elfmt.el ends here
