;;; elfmt.el --- Code formatter for Elisp -*- lexical-binding: t -*-

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
;; - Focuses on the placement of lists and (mostly) ignores atoms
;; - Tries to break at `fill-column', but lines may exceed this number
;;   due to inline comments, long literals, trailing sequences of closed
;;   parens, or postprocessing (see `elfmt-autojoin-1' for example)
;; - Prefers "modern" Elisp (old-style backquotes will cause it to halt)
;;
;; Usage:
;; - Use M-x elfmt to format the current buffer
;; - Use M-x elfmt-sexp to format the current sexp.
;; - Use M-x elfmt-mode to automatically format the buffer on save
;; - Use M-x elfmt-global-mode to enable `elfmt-mode' everywhere

;;; Code:

(require 'cl-lib)

(defgroup elfmt nil
  "Code formatter for Elisp"
  :prefix "elfmt-"
  :group 'elfmt
  :link '(url-link :tag "URL" "https://github.com/riscy/elfmt")
  :link '(emacs-commentary-link :tag "Commentary" "shx.el"))

(defconst elfmt-autojoin-1
  (regexp-opt
   '( ; keep this list sorted
     "(advice-add\n"
     "(declare-function\n"
     "(declare\n"
     "(defcustom\n"
     "(defun\n"
     "(defvar\n"
     "(dolist\n"
     "(dotimes\n"
     "(if-let*\n"
     "(if-let\n"
     "(if\n"
     "(lambda\n"
     "(let*\n"
     "(let\n"
     "(not\n"
     "(pcase-let*\n"
     "(pcase-let\n"
     "(pcase\n"
     "(unless\n"
     "(when-let*\n"
     "(when-let\n"
     "(when\n"
     "(while\n"
     "(with-current-buffer\n"
     "(with-suppressed-warnings\n"
     ;; keep this list sorted
     ))
  "To e.g. join '(while' to its condition.")

(defconst elfmt-autojoin-2
  (format "%s [[:graph:]]+$"
          (regexp-opt
           '( ; keep this list sorted
             "(cl-defgeneric"
             "(cl-defmacro"
             "(cl-defmethod"
             "(cl-defsubst"
             "(cl-defun"
             "(defadvice"
             "(defclass"
             "(defmacro"
             "(defsubst"
             "(defun"
             ;; keep this list sorted
             )))
  "To e.g. join '(defun <name>' to its argument list.")

(defconst elfmt-autojoin-3
  (format "%s [[:graph:]]+ [[:graph:]]+$" (regexp-opt '("^(declare-function")))
  "To e.g. join '(declare-function <name> <file>' to its argument list.")

;;;###autoload
(define-minor-mode elfmt-mode
  "Toggle elfmt-mode on or off.
This formats buffers before saving via a local hook."
  :lighter " fmt"
  (if elfmt-mode
      (add-hook 'before-save-hook #'elfmt-buffer 0 t)
    (remove-hook 'before-save-hook #'elfmt-buffer t)))

;;;###autoload
(define-globalized-minor-mode elfmt-global-mode elfmt-mode elfmt--global-on)

(defun elfmt--global-on ()
  "Call the function `elfmt-mode' on appropriate buffers."
  (when (eq major-mode 'emacs-lisp-mode) (elfmt-mode +1)))

(defun elfmt ()
  "Format the current buffer.
Interactive version of `elfmt-buffer' that reports timing."
  (interactive)
  (let ((start-time (current-time)))
    (elfmt-buffer)
    (message "`elfmt' took %dms" (* 1000 (float-time (time-since start-time))))))

(defun elfmt-buffer ()
  "Format the current buffer."
  (barf-if-buffer-read-only)
  (let* ((gc-cons-threshold most-positive-fixnum)
         (unformatted (buffer-substring (point-min) (point-max)))
         (formatted
          (with-temp-buffer
            (insert unformatted)
            (emacs-lisp-mode)
            (save-excursion
              (goto-char (point-max))
              (while (not (bobp))
                (backward-sexp)
                (elfmt--sexp)))
            (buffer-substring (point-min) (point-max)))))
    ;; TODO: fix whitespace between top-level sexps
    (unless (string= unformatted formatted)
      (let ((point (point)))
        (erase-buffer)
        (insert formatted)
        (goto-char point)
        (recenter)))))

(defun elfmt-sexp ()
  "Format the current (top level) sexp."
  (interactive)
  (let ((start-time (current-time))
        (gc-cons-threshold most-positive-fixnum)) ; speedup
    (save-excursion
      (forward-char 1) ; ensure we format what's front of us
      ;; if we're inside a string, `up-list' will misbehave:
      (while (nth 3 (syntax-ppss)) (backward-char))
      (while (ignore-errors (or (up-list) t)))
      (backward-sexp)
      (elfmt--sexp))
    (message "`elfmt-sexp' took %dms"
             (* 1000 (float-time (time-since start-time))))))

(defun elfmt--sexp ()
  "Format the sexp starting at the point."
  ;; precond: point is on an sexp
  (let ((original-sexp (sexp-at-point)))
    (when (and original-sexp (listp original-sexp))
      (goto-char (point-at-bol))
      (elfmt--map-sexp-lines #'elfmt--break-line)
      (elfmt--map-sexp-lines #'elfmt--mend-line)
      (elfmt--map-sexp-lines #'elfmt--postprocess-line)
      (or
       (equal (sexp-at-point) original-sexp)
       (error "`elfmt' could not parse: %S" original-sexp)))))

(defun elfmt--map-sexp-lines (formatting-func)
  "Apply FORMATTING-FUNC to every line of the sexp at point."
  ;; precond: point is on an sexp
  (let ((start-of-sexp (point)))
    (while (< (point) (scan-sexps start-of-sexp 1))
      (or (elfmt--nofmt-line-p) (funcall formatting-func))
      (forward-line 1))
    (goto-char start-of-sexp)))

(defun elfmt--nofmt-line-p (&optional n)
  "Non-nil if N lines forward (backward if N is negative) is 'nofmt'."
  (declare (side-effect-free t))
  (save-excursion
    (when n (forward-line n))
    (or
     (and (bolp) (eolp)) ; the line is empty
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
          ;; and characters [i.e. ?\(]; it also assumes all the elements are
          ;; separated by spaces. It would be more correct to use `scan-lists'
          ;; but that would make it harder to constrain the point to one line,
          ;; and would also be slower; `scan-lists' is also not foolproof e.g.
          ;; when the point is inside a string!
          (re-search-backward "[^(](" (point-at-bol) t)
          (or (forward-char 1) t)
          ;; `backward-prefix-chars' fails on ?\( so just look for whitespace:
          (skip-chars-backward "[:graph:]" (point-at-bol))
          (skip-chars-backward "[:space:]" (point-at-bol))
          (not (bolp)))
    (when (elfmt--can-format-p (point)) (open-line 1)))
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

(defun elfmt--mend-line ()
  "Join the current line up with the lines beneath it, when feasible."
  ;; precond: (bolp)
  (funcall indent-line-function)
  (elfmt--goto-innermost-sexp)
  (while (elfmt--mend-line-p) (save-excursion (elfmt--join-line))))

(defun elfmt--goto-innermost-sexp ()
  "Move the point to the innermost sexp on the current line."
  (skip-chars-forward "`#'@^, (" (point-at-eol)) ; prefix chars
  (and (eq (char-before) ?\() (backward-char)))

(defun elfmt--join-line ()
  "Patches `join-line' to not remove when lines end in \\(."
  (join-line 1)
  (and
   (eq (char-before (1- (point))) ?\\)
   (eq (char-before) ?\( )
   (insert " ")))

(defun elfmt--mend-line-p ()
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
     (< (cl-count ?\( sexp-str) 5) ; visual complexity (i.e. nested parens)
     (save-excursion
       (and
        (elfmt--can-format-p (point-at-eol)) ; it's safe to reformat here
        (eq (forward-line 1) 0)              ; and the next line, if any:
        (not (looking-at "[[:space:]]*;"))   ; ...is not a comment
        (not (elfmt--nofmt-line-p)))))))     ; ...is not nofmt

(defun elfmt--can-format-p (pos)
  "Is the Elisp code at POS formatable?
Code inside strings and comments can't be reformated.
This may modify the point due to calls to `syntax-ppss'."
  (let ((state (syntax-ppss pos)))
    (not (or (nth 3 state) (nth 4 state)))))

(defun elfmt--postprocess-line ()
  "Run postprocessors on the line at point.
Postprocessors will do things like fix single-line comments,
join widowed lines with the next line, and fix indentation."
  (goto-char (point-at-bol))
  (skip-chars-forward "[:space:]" (point-at-eol))
  (cond
   ((looking-at ";[^;]")
    (save-excursion (insert ";"))) ; start single-line comments with ;;
   ((or
     (ignore (elfmt--goto-innermost-sexp))
     (looking-at elfmt-autojoin-1)
     (looking-at elfmt-autojoin-2)
     (looking-at elfmt-autojoin-3)
     (looking-at ":[[:graph:]]+\\_>$") ; :keywords e.g. for `plist-get'
     (elfmt--looking-at-orphan-parens))
    (elfmt--postprocess-join)))
  (when (eq (char-before (point-at-eol)) ?\()
    (elfmt--postprocess-join))
  (funcall indent-line-function))

(defun elfmt--postprocess-join ()
  "Join the current line with the next."
  (or
   (elfmt--nofmt-line-p +1)
   (not (elfmt--can-format-p (point-at-eol)))
   (elfmt--join-line)))

(defun elfmt--looking-at-orphan-parens ()
  "Return non-nil if there are orphan parens on the next line."
  (declare (side-effect-free t))
  (save-excursion (forward-line 1) (looking-at-p "[[:space:]]*)")))

(provide 'elfmt)
;;; elfmt.el ends here
