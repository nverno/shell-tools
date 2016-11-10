;;; sh-tools --- sh script -*- lexical-binding: t; -*-
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib)
  (defvar company-backends))
(require 'shell-tools)

;; ------------------------------------------------------------
;;; Completion

(defvar sh-tools-company-backends '(company-bash :with company-shell))

(nvp-with-gnu
  ;; `bash-completion' is awesome
  ;; just need to redefine `comint-line-beginning-position' so
  ;; `bash-completion-dynamic-complete' can work in sh-script
  (defun sh-tools-bash-completion ()
    (cl-letf (((symbol-function 'comint-line-beginning-position)
               #'(lambda ()
                   (save-excursion
                     (sh-beginning-of-command)
                     (point)))))
      (let ((syntax (syntax-ppss)))
        (and (not (or (nth 3 syntax)
                      (nth 4 syntax)))
             (bash-completion-dynamic-complete)))))
  
  (when (require 'bash-completion nil t)
    (setq sh-tools-company-backends '(company-bash :with company-capf))))

;; setup company backends with company-bash and either company-shell
;; or bash-completion
(defun sh-tools-company-setup ()
  (make-local-variable 'company-backends)
  (nvp-with-gnu
    (when (require 'bash-completion nil t)
      (delq 'company-capf company-backends)
      (add-hook 'completion-at-point-functions
                'sh-tools-bash-completion nil 'local)))
  (cl-pushnew sh-tools-company-backends company-backends)
  (setq-local company-transformers '(company-sort-by-backend-importance)))

;; with prefix, only complete for sourced / local functions
(defun sh-tools-company-bash (arg)
  (interactive "P")
  (if arg
      (call-interactively 'company-bash)
    (company-complete)))

;; ------------------------------------------------------------
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

;; ------------------------------------------------------------
;;; Cleanup

;; enforce uft-8-unix on save
(defun sh-tools-cleanup-buffer ()
  (unless (eq 'utf-8-unix buffer-file-coding-system)
    (set-buffer-file-coding-system 'utf-8-unix)))

;; ------------------------------------------------------------

(declare-function company-bash "company-bash")
(declare-function company-shell "company-shell")
(declare-function company-sort-by-backend-importance "company")
(declare-function company-complete "company")
(declare-function bash-completion-dynamic-complete "bash-completion")

(provide 'sh-tools)
;;; sh-tools.el ends here
