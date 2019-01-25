;;; nvp-sh.el --- sh script helpers -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/shell-tools
;; Last modified: <2019-01-25 17:42:15>
;; Package-Requires: 
;; Created:  5 December 2016

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib)
  (require 'subr-x)
  (defvar explicit-shell-file-name))
(require 'nvp-shell)
(require 'sh-help)
(require 'company)
(require 'company-quickhelp)
(require 'company-bash)
(require 'sh-script)

(declare-function company-shell "company-shell")
(declare-function bash-completion-dynamic-complete "bash-completion")
(declare-function bash-completion-dynamic-complete-nocomint "bash-completion")
(declare-function sp-wrap-with-pair "smartparens")

(autoload 'string-trim "subr-x")

;; -------------------------------------------------------------------
;;; Variables

;; for jumping b/w functions -- see `sh-imenu-generic-expression'
(defvar nvp-sh-function-re
  (nvp-concat
   "\\(?:"
   ;; function FOO()
   "^\\s-*function\\s-+\\([[:alpha:]_][[:alnum:]_]*\\)\\s-*\\(?:()\\)?"
   "\\|"
   ;; FOO()
   "^\\s-*\\([[:alpha:]_][[:alnum:]_]*\\)\\s-*()"
   "\\)"))

;; imenu header comment regexp
(defvar nvp-sh-comment-headers-re '((nil "^###\\s-*\\(.+\\)\\s-*$" 1)))

;; additional imenu regexps
(defvar nvp-sh-imenu-extra-regexps
  '(("Sources" "^\\(?:\\\.\\|source\\)\\s-+\\(.+\\)\\s-*$" 1)
    ("Globals" "^\\([A-Za-z_][^=\n]*\\)=" 1)))

;; -------------------------------------------------------------------
;;; Utils

;; true if point is in heredoc
(defsubst nvp-sh--here-doc-p (point)
  (eq (get-text-property point 'face) 'sh-heredoc))

;; get the here doc marker for block
(defsubst nvp-sh--here-doc-marker (&optional point)
  (let ((ppss (syntax-ppss point)))
    (when (eq t (nth 3 ppss))
      (get-text-property (nth 8 ppss) 'sh-here-doc-marker))))

;; position at beginning of first line of here-doc if point is
;; currently in a here-doc
(defun nvp-sh-here-doc-p (point)
  (save-excursion
    (goto-char point)
    (back-to-indentation)
    (when (looking-back "[^<]<<.*" (line-beginning-position)) ;skip opening line
      (forward-line))
    (let ((in-heredoc (nvp-sh--here-doc-p (point))))
      (when in-heredoc                      ;search back until no marker
        (while (and (not (bobp))
                    (nvp-sh--here-doc-p (point)))
          (forward-line -1)
          (back-to-indentation))
        (point)))))

;; -------------------------------------------------------------------
;;; Commands

(defun nvp-sh-beginning-of-defun (&optional arg)
  (interactive "^p")
  (or (not (eq this-command 'nvp-sh-beginning-of-defun))
      (eq last-command 'nvp-sh-beginning-of-defun)
      (and transient-mark-mode mark-active)
      (push-mark))
  (unless arg (setq arg 1))
  (cond
   ((> arg 0)
    (while (and (> arg 0)
                (re-search-backward nvp-sh-function-re nil 'move))
      (setq arg (1- arg))))
   (t (while (and (< arg 0)
                  (re-search-forward nvp-sh-function-re nil))
        (setq arg (1+ arg))))))

;; move to beginning of next function if there is one
(defun nvp-sh-next-defun ()
  (interactive)
  (condition-case nil
      (progn
        (forward-line 1)
        (nvp-sh-beginning-of-defun -1)
        (beginning-of-line))
    (error (forward-line -1))))

;; (defun nvp-sh-wrap-quotes (&optional _arg)
;;   (interactive "P")
;;   (let ((tab (copy-syntax-table sh-mode-syntax-table)))
;;     (with-syntax-table tab
;;       (sp-wrap-with-pair "\""))))

;;; Toggle

;; toggle here-doc indentation:
;; open with '<<-' and use only leading tabs for indentation
(defun nvp-sh-toggle-here-doc-indent (point)
  (interactive "d")
  (let ((start-pos (nvp-sh-here-doc-p point)))
    (when start-pos
      (save-excursion
        (goto-char start-pos)
        (search-forward-regexp "[^<]<<" (line-end-position) 'move)
        (let ((indent (not (eq ?- (char-after))))
              marker)
          (if indent                    ;toggle preceding '-'
              (insert "-")
            (delete-char 1))
          (forward-to-indentation)      ;skip past opening line
          (setq marker (nvp-sh--here-doc-marker))
          (while (and (nvp-sh--here-doc-p (point))
                      (not (looking-at-p marker)))
            (delete-horizontal-space)
            (and indent                  ;toggle indentation
                 (insert "\t"))
            (forward-to-indentation)))))))

;; ------------------------------------------------------------
;;; Completion

(defvar nvp-sh-company-backends '(company-bash :with company-shell))
(when (require 'bash-completion nil t)
  (setq nvp-sh-company-backends '(company-bash :with company-capf)))

(defun nvp-sh-dynamic-complete-vars ()
  "Complete local variables, but fail if none match to delegate to bash completion."
  (nvp-unless-in-comment
    (save-excursion
        (skip-chars-forward "[:alnum:]_")
        (let ((end (point))
              (_ (skip-chars-backward "[:alnum:]_"))
              (start (point)))
          (when (eq (char-before start) ?$)
            (let ((vars (sh--vars-before-point)))
              (and (not (null (try-completion
                               (buffer-substring-no-properties start end) vars)))
                   (list start end vars))))))))

(defun nvp-sh-dynamic-complete-bash ()
  "Bash dynamic completion for sh-script (doesn't get local variables)."
  (nvp-unless-in-comment
    (bash-completion-dynamic-complete-nocomint
     (save-excursion (sh-beginning-of-command)) (point) 'dynamic-table)))

;;; company-quickhelp
;; Since no doc-buffer is returned by company-capf, rewrite
;; company-quickhelp doc retrieval method to just call man on the
;; current candidates
;; return a company-documentation buffer with either Man output or bash help
;; for a builtin
(defun nvp-sh-doc-buffer (cmd)
  (let ((doc-str (if (sh-help-bash-builtin-p cmd)
                     (sh-help-bash-builtin-sync cmd)
                   (shell-command-to-string (format "man %s" cmd)))))
    (and (not (or (member doc-str '(nil ""))
                  (string-prefix-p "No manual entry" doc-str)))
         (company-doc-buffer doc-str))))

(defun nvp-sh-quickhelp-doc (selected)
  (cl-letf (((symbol-function 'completing-read)
             #'company-quickhelp--completing-read))
    (let* ((doc-buff (nvp-sh-doc-buffer selected))
           (doc-and-meta
            (with-current-buffer doc-buff
              (company-quickhelp--docstring-from-buffer (point-min))))
           (truncated (plist-get doc-and-meta :truncated))
           (doc (plist-get doc-and-meta :doc)))
      (unless (member doc '(nil ""))
        (if truncated
            (concat doc "\n\n[...]")
          doc)))))

;; re-bind to this in `company-active-map'
(defun nvp-sh-quickhelp-toggle ()
  (interactive)
  (let ((x-gtk-use-system-tooltips nil)
        ;; (company-quickhelp-delay 0.1)
        )
    (or (x-hide-tip)
        (cl-letf (((symbol-function 'company-quickhelp--doc)
                   #'nvp-sh-quickhelp-doc))
          ;; flickers the screen - cant use the timer, since it seems
          ;; that lexical binding doesn't work in that case
          (company-quickhelp--show)))))

;; show help buffer in other window from company-active-map
(defun nvp-sh-company-show-doc-buffer ()
  (interactive)
  (cl-letf (((symbol-function 'company-call-backend)
             #'(lambda (_type selected)
                 (nvp-sh-doc-buffer selected) "*company-documentation*")))
    (company-show-doc-buffer)))

;; setup company backends with company-bash and either company-shell
;; or bash-completion
(defun nvp-sh-completion-setup ()
  (make-local-variable 'company-backends)
  (nvp-with-gnu
    (when (require 'bash-completion nil t)
      (delq 'company-capf company-backends)
      (add-hook 'completion-at-point-functions 'nvp-sh-dynamic-complete-bash
                nil 'local)
      ;; allow completion of local variables as well
      (add-hook 'completion-at-point-functions 'nvp-sh-dynamic-complete-vars
                nil 'local))
    ;; use local version of `company-active-map' to rebind
    ;; functions to show popup help and jump to help buffer
    (nvp-use-local-keymap company-active-map
      ("M-h" . nvp-sh-quickhelp-toggle)
      ("C-h" . nvp-sh-company-show-doc-buffer)))
  (cl-pushnew nvp-sh-company-backends company-backends)
  (setq-local company-transformers '(company-sort-by-backend-importance)))

;; with prefix, only complete for sourced / local functions
(defun nvp-sh-company-bash (arg)
  "Temporarily use only sourced / local functions for completion."
  (interactive "P")
  (if arg
      (call-interactively 'company-bash)
    (company-complete)))

;; ------------------------------------------------------------
;;; Font-lock

;; Add additional font-locking to quoted variables
;; Non-nil if point in inside a double-quoted string.
(defun nvp-sh-font-lock--quoted-string-p ()
  (let ((state (syntax-ppss)))
    (eq (nth 3 state) ?\")))

;; Search for variables in double-quoted strings bounded by `LIMIT'.
(defun nvp-sh-font-lock--quoted-string (limit)
  (let (res)
    (while
        (and
         (setq res
               (re-search-forward
                "\\$\\({#?\\)?\\([[:alpha:]_][[:alnum:]_]*\\|[-#?@!]\\)"
                limit t))
         (not (nvp-sh-font-lock--quoted-string-p))))
    res))

(defvar nvp-sh-font-lock--keywords
  '((nvp-sh-font-lock--quoted-string (2 font-lock-variable-name-face prepend))))

(defun nvp-sh-font-lock ()
  (font-lock-add-keywords nil nvp-sh-font-lock--keywords)
  (if (fboundp #'font-lock-flush)
      (font-lock-flush)
    (when font-lock-mode
      (font-lock-ensure (point-min) (point-max)))))

;; -------------------------------------------------------------------
;;; REPL

;; replacement for `sh-shell-process'
(defun nvp-sh-get-process (&optional this-buffer)
  "Replacement for `sh-shell-process'.
Optionally return process specific to THIS-BUFFER."
  (let ((buffname (and this-buffer
                       (concat "*shell: " (buffer-name (current-buffer)) "*"))))
    (if (and (process-live-p sh-shell-process)
             (if buffname
                 (string= (buffer-name
                           (process-buffer sh-shell-process))
                          buffname)
               t))
       sh-shell-process
     (setq sh-shell-process
           (let ((proc (nvp-shell-get-process nil buffname)))
             (or proc
                 (get-buffer-process
                  (let ((explicit-shell-file-name sh-shell-file))
                    (shell buffname)))))))))

;; FIXME: do I always want a sh file to sending to its own shell?
(setf (symbol-function 'sh-shell-process) 'nvp-sh-get-process)

;; switch to shell REPL, specific to this buffer with a prefix arg
(nvp-repl-switch "sh"
    (:repl-mode 'shell-mode
     :repl-find-fn
     #'(lambda ()
         (process-buffer (nvp-sh-get-process current-prefix-arg)))
     :repl-switch-fn 'pop-to-buffer)
  (process-buffer
   (setq sh-shell-process (nvp-sh-get-process current-prefix-arg))))

;; send selected region and step
(defun nvp-sh-send-region (beg end)
  "Send selected region from BEG to END to associated shell process."
  (interactive "r")
  (comint-send-string (nvp-sh-get-process)
                      (concat (buffer-substring beg end) "\n"))
  (goto-char end))

;; ------------------------------------------------------------
;;; Cleanup / Align

(require 'align)

;; alignment rules to align '\' not in strings/comments and
;; align end-of-line comments
(defvar nvp-sh-align-rules-list
  `((sh-line-continuation
     (regexp . "\\(\\s-*\\)\\\\\\s-*$")
     (modes  . '(sh-mode))
     (valid  . ,(function
                 (lambda () (save-excursion
                         (not (sh-in-comment-or-string (point))))))))
    (sh-eol-comments
     (regexp . "[^ #\t\n\\\\]\\(\\s-+\\)#+.*$")
     (group  . 1)
     (modes  . '(sh-mode))
     (valid  . ,(function
                 (lambda () (save-excursion
                         (goto-char (match-beginning 1))
                         (and (not (bolp))
                              (not (nth 3 (syntax-ppss)))))))))))

;; align region or buffer
(defun nvp-sh-align (beg end &optional all)
  (interactive "r\np")
  (if all (align (point-min) (point-max))
    (align beg end)))

;; enforce uft-8-unix and align when killing buffer
(defun nvp-sh-cleanup-buffer ()
  (unless (or buffer-read-only (not (buffer-modified-p)))
    (unless (eq 'utf-8-unix buffer-file-coding-system)
      (set-buffer-file-coding-system 'utf-8-unix))
    (align (point-min) (point-max))
    (and (buffer-modified-p)
         (save-buffer))))

;; -------------------------------------------------------------------
;;; Yas / Snippets

(defun nvp-sh-current-defun ()
  "Find name of function containing point.
Like `sh-current-defun-name' but ignore variables."
  (save-excursion
    (end-of-line)
    (when (re-search-backward nvp-sh-function-re nil 'move)
      (or (match-string-no-properties 1)
          (match-string-no-properties 2)))))

(defun nvp-sh-yas-defun-or-script ()
  (or (nvp-sh-current-defun) (nvp-bfn)))

(provide 'nvp-sh)
;;; nvp-sh.el ends here
