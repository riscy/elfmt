;;; test-elfmt.el --- Regression tests for elfmt -*- lexical-binding: t -*-

;;; Commentary:

;; Regression tests for elfmt.

;;; Code:

(defvar-local defvar-nil
  '()
  "Defvar nil with a very very long docstring attached to the end.")

(defun long-function-name---------------------------------------------------------------
    ()
  "Long documentation string----------------------------------------------------------."
  "Short string."
  "Long return string-----------------------------------------------------------------.")

(defun long-list ()
  "Long forms that don't exceed `fill-column' don't get reformatted.
But if a user let this list exceed `fill-column' it would get reformatted."
  (list
   '------------------------------ '------------------------------
   '------------------------------
   '------------------------------ '------------------------------
   '------------------------------
   '------------------------------))

(defun short-list ()
  "A short list with inline nofmt comments."
  (insert "x"
          "x"                           ; nofmt
          "x"                           ; nofmt
          ))

"(Don't format strings, even if they look like sexps.)"

(defun parentheses-inside-strings ()
  "(Parentheses, inside a string)."
  '('(10)
    "((Could be on one line, but too many parens.))"
    '(10)))

(defun backquoted-lambda (regexp)
  "Backquoted lambda form taking a REGEXP."
  (declare (side-effect-free t))
  `(lambda (bound)
     (let ((inhibit-field-text-motion t))
       (when (eq (point-max) (point-at-eol))
         (re-search-forward ,regexp bound t)))))



(defun internal--thread-argument (first? &rest forms)
  "Internal implementation for `thread-first' and `thread-last'.
When Argument FIRST? is non-nil argument is threaded first, else
last.  FORMS are the expressions to be threaded."
  (pcase forms
    (`(,x
       (,f . ,args)
       . ,rest)
     `(internal--thread-argument
       ,first?
       ,(if first? `(,f ,x ,@args) `(,f ,@args ,x))
       ,@rest))
    (`(,x ,f . ,rest) `(internal--thread-argument ,first? (,f ,x) ,@rest))
    (_ (car forms))))

(defun thing-at-point--beginning-of-sexp ()
  "Move point to the beginning of the current sexp."
  (let ((char-syntax (char-syntax (char-before))))
    (if (or
         (eq char-syntax ?\()
         (and (eq char-syntax ?\") (nth 3 (syntax-ppss))))
	(forward-char -1)
      (forward-sexp -1))))

;; this string shouldn't be touched:
" \
(defun thing-at-point--beginning-of-sexp ()
  (let ((char-syntax (char-syntax (char-before))))



    (if (or
       (eq char-syntax ?\()
         (and (eq char-syntax ?\") (nth 3 (syntax-ppss))))
	(forward-char -1)
      (forward-sexp -1))))                               "

;; NOTE: checkdoc recommends \( at the first column of docstrings,
;; but doing so can throw a wrench into (thing-at-point 'sexp):
;; (defun parentheses-inside-strings-2 ()
;;   "Parentheses, inside a string."
;; "This open paren can match to the docstring in the next defun:
;; (
;; ") ;; ... in fact (thing-at-point 'sexp) on that paren gives the defun!
;; (defun parentheses-inside-strings-3 ()
;;   "Hello world.
;; \()."
;;   nil)

(defvar git-commit-elisp-text-mode-keywords
  `((,(concat
       "[`‘]\\(" lisp-mode-symbol-regexp "\\)['’]")
     (1 font-lock-constant-face prepend))
    ("\"[^\"]*\"" (0 font-lock-string-face prepend))))



(defmacro and-let* (varlist &rest body)
  "Bind variables according to VARLIST and conditionally evaluate BODY.
Like `when-let*', except if BODY is empty and all the bindings
are non-nil, then the result is non-nil."
  (declare (indent 1)
           (debug
            ((&rest [&or symbolp (symbolp form) (form)])
             body)))
  (let (res)
    (if varlist
        `(let* ,(setq varlist (car varlist))
           (when ,(setq res (caar (last varlist)))
             ,@(or body `(,res))))
      `(let* () ,@(or body '(t))))))

(defun test-tricky-comment (filename)
  "Show scatter plot of FILENAME.
\nFor example, \":plotscatter file.dat\", where file.dat contains:
  1 2\n  2 4\n  4 8\n
Or just a single column:
  1\n  2\n  3\n  5" filename)

(defun this-is-a-test () "Hello."(car '(1 2 3)))

(defun occur-engine-add-prefix (lines &optional prefix-face)
  "Comment; LINES, PREFIX-FACE."
  (mapcar
   #'(lambda (line)
       (concat
        (if prefix-face
            (propertize "       :" 'font-lock-face prefix-face)
          "       :")
        line "\n"))
   lines))

(defun package-quickstart-refresh ()
  "(Re)Generate the `package-quickstart-file'."
  (insert "
;; Local\sVariables:
;; version-control: never
;;\sno-byte-compile: t
;; no-update-autoloads: t
;; End:
"))

"An apparent symbol at the end of a string is left alone:
:test"

(defun testing-2342 ()
  "Known spacing issue due to `fixup-whitespace'."
  (if (eq ?\((char-after)) (message "Mashed")))

(defun elfmt--postprocess-line-join ; (unless there's a trailing comment)
    ()
  "As part of a postprocessing step, join current line with the next."
  (when (not (or t nil)) (join-line 1)
    )) ; (or a 'nofmt' at the end of the line)  ; nofmt

(provide 'elfmt-test)
;;; elfmt-test.el ends here
