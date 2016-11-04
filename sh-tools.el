;;; sh-tools --- 
;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (defvar company-backends))
(require 'shell-tools)

;;; Font-lock

;; Add additional font-locking to quoted variables
;; Non-nil if point in inside a double-quoted string.
(defun sh-tools-font-lock--quoted-string-p ()
  (let ((state (syntax-ppss)))
    (eq (nth 3 state) ?\")))

;; Search for variables in double-quoted strings bounded by `LIMIT'.
(defun sh-tools-font-lock--quoted-string (limit)
  (let (res)
    (while
        (and
         (setq res
               (re-search-forward
                "\\$\\({#?\\)?\\([[:alpha:]_][[:alnum:]_]*\\|[-#?@!]\\)"
                limit t))
         (not (sh-tools-font-lock--quoted-string-p))))
    res))

(defvar sh-tools-font-lock--keywords
  '((sh-tools-font-lock--quoted-string
     (2 font-lock-variable-name-face prepend))))

(defun sh-tools-font-lock ()
  (font-lock-add-keywords nil sh-tools-font-lock--keywords)
  (if (fboundp #'font-lock-flush)
      (font-lock-flush)
    (when font-lock-mode
      (font-lock-ensure (point-min) (point-max)))))

;; cleanup on save
;; enforce uft-8-unix on save
(defun sh-tools-cleanup-buffer ()
  (unless (eq 'utf-8-unix buffer-file-coding-system)
    (set-buffer-file-coding-system 'utf-8-unix)))

;; company completion
(defun sh-tools-company-setup ()
  (make-local-variable 'company-backends)
  (cl-pushnew '(company-bash :with company-shell) company-backends)
  (setq-local company-transformers '(company-sort-by-backend-importance)))

(defun sh-tools-company-bash (arg)
  (interactive "P")
  (if arg
      (call-interactively 'company-bash)
    (company-complete)))

;; ------------------------------------------------------------

(declare-function company-bash "company-bash")
(declare-function company-shell "company-shell")
(declare-function company-sort-by-backend-importance "company")
(declare-function company-complete "company")

(provide 'sh-tools)
;;; sh-tools.el ends here
