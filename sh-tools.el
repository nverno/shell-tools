;;; sh-tools ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/shell-tools
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
  (defvar company-backends)
  (defvar company-transformers))
(require 'shell-tools)

;; -------------------------------------------------------------------
;;; Variables

;; sh-script imenu
(defvar sh-tools-function-re
  (nvp-concat
   "\\(?:"
   ;; function FOO()
   "^\\s-*function\\s-+\\([[:alpha:]_][[:alnum:]_]*\\)\\s-*\\(?:()\\)?"
   "\\|"
   ;; FOO()
   "^\\s-*\\([[:alpha:]_][[:alnum:]_]*\\)\\s-*()"
   "\\)"))

;; imenu header comment regexp
(defvar sh-tools-comment-headers-re '((nil "^###\\s-*\\(.+\\)\\s-*$" 1)))

;; -------------------------------------------------------------------
;;; Utils

;; name of current function
(defun sh-tools-function-name ()
  (let ((ppss (syntax-ppss))
        (start (or (cdr (bounds-of-thing-at-point 'symbol)) (point))))
    ;; ignore comments
    (when (not (nth 4 ppss))
      (save-excursion
        (catch 'done
          (while t
            (skip-chars-backward "^:<>)(|&\`;\["
                                 (line-beginning-position))
            (if (nth 3 (syntax-ppss))
                ;; move backward out of enclosing string
                (up-list -1 t t)
              (throw 'done nil))))
        (skip-syntax-forward " " start)
        (cond
         ;; '[[' or '['
         ((looking-back "\\(?:^\\|[^[]\\)\\(\\[+\\)[ \t]*"
                        (line-beginning-position))
          (match-string 1))
         ;; 'if' => if in situation like 'if ! hash', then
         ;; return 'hash'
         ((looking-at-p "if\\_>")
          (if (looking-at "if[ \t]+!?[ \t]*\\([-+[:alnum:]]+\\)")
              (match-string 1)
            "if"))
         ;; otherwise, return first symbol
         (t (and (looking-at "[:+_\[\.[:alnum:]-]+")
                 (match-string 0))))))))

;; get conditional switch
(defun sh-tools-conditional-switch ()
  (save-excursion
    (skip-chars-backward "^\[" (line-beginning-position))
    (and (not (bolp))
         (looking-at "[ !]*\\(-[[:alpha:]]+\\)")
         (match-string 1))))

;; true if point is in heredoc
(defsubst sh-tools--here-doc-p (point)
  (eq (get-text-property point 'face) 'sh-heredoc))

;; get the here doc marker for block
(defsubst sh-tools--here-doc-marker (&optional point)
  (let ((ppss (syntax-ppss point)))
    (when (eq t (nth 3 ppss))
      (get-text-property (nth 8 ppss) 'sh-here-doc-marker))))

;; position at beginning of first line of here-doc if point is
;; currently in a here-doc
(defun sh-tools-here-doc-p (point)
  (save-excursion
    (goto-char point)
    (back-to-indentation)
    (when (looking-back "[^<]<<.*" (line-beginning-position)) ;skip opening line
      (forward-line))
    (let ((in-heredoc (sh-tools--here-doc-p (point))))
      (when in-heredoc                      ;search back until no marker
        (while (and (not (bobp))
                    (sh-tools--here-doc-p (point)))
          (forward-line -1)
          (back-to-indentation))
        (point)))))

;; ------------------------------------------------------------
;;; Completion

(declare-function company-doc-buffer "company")
(declare-function company-quickhelp--doc "company-quickhelp")
(declare-function company-quickhelp--doc-and-meta "company-quickhelp")

(autoload 'string-trim "subr-x")

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
    (setq sh-tools-company-backends
          '(company-bash :with company-capf))))

;;; company-quickhelp
;; Since no doc-buffer is returned by company-capf, rewrite
;; company-quickhelp doc retrieval method to just call man on the
;; current candidates
(defun sh-tools-doc-buffer (cmd)
  (company-doc-buffer
   (let ((man-page (shell-command-to-string (format "man %s" cmd))))
     (if (or (null man-page)
             (string= man-page "")
             (string-prefix-p "No manual entry" man-page))
         (shell-command-to-string
          (format "echo \"timeout 1 %s --help\" | %s --restricted"
                  cmd
                  (string-trim (shell-command-to-string "which bash"))))
       man-page))))

(defun sh-tools-quickhelp-doc (selected)
  (cl-letf (((symbol-function 'completing-read)
             #'company-quickhelp--completing-read))
    (let* ((doc (sh-tools-doc-buffer selected))
           (doc-and-meta (when doc
                           (company-quickhelp--doc-and-meta doc)))
           (truncated (plist-get doc-and-meta :truncated))
           (doc (plist-get doc-and-meta :doc)))
      (unless (string= doc "")
        (if truncated
            (concat doc "\n\n[...]")
          doc)))))

;; re-bind to this in `company-active-map'
(defun sh-tools-quickhelp-toggle ()
  (interactive)
  (let ((x-gtk-use-system-tooltips nil))
    (or (x-hide-tip)
        (cl-letf (((symbol-function 'company-quickhelp--doc)
                   #'sh-tools-quickhelp-doc))
          ;; flickers the screen - cant use the timer, since it seems
          ;; that lexical binding doesn't work in that case
          (company-quickhelp--show)))))


;; show help buffer in other window from company-active-map
(defun sh-tools-company-show-doc-buffer ()
  (interactive)
  (cl-letf (((symbol-function 'company-call-backend)
             #'(lambda (_type selected)
                 (sh-tools-doc-buffer selected)
                 "*company-documentation*")))
    (company-show-doc-buffer)))

;; setup company backends with company-bash and either company-shell
;; or bash-completion
(defun sh-tools-company-setup ()
  (make-local-variable 'company-backends)
  (nvp-with-gnu
    (when (require 'bash-completion nil t)
      (delq 'company-capf company-backends)
      (add-hook 'completion-at-point-functions
                'sh-tools-bash-completion nil 'local))
    ;; use local version of `company-active-map' to rebind
    ;; functions to show popup help and jump to help buffer
    (nvp-with-local-keymap company-active-map
      ("M-h" . sh-tools-quickhelp-toggle)
      ("C-h" . sh-tools-company-show-doc-buffer)))
  (cl-pushnew sh-tools-company-backends company-backends)
  (setq-local company-transformers
              '(company-sort-by-backend-importance)))

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

;; -------------------------------------------------------------------
;;; Commands

(defun sh-tools-beginning-of-defun (&optional arg)
  (interactive "^p")
  (or (not (eq this-command 'sh-tools-beginning-of-defun))
      (eq last-command 'sh-tools-beginning-of-defun)
      (and transient-mark-mode mark-active)
      (push-mark))
  (unless arg (setq arg 1))
  (cond
   ((> arg 0)
    (while (and (> arg 0)
                (re-search-backward sh-tools-function-re nil 'move))
      (setq arg (1- arg))))
   (t (while (and (< arg 0)
                  (re-search-forward sh-tools-function-re nil))
        (setq arg (1+ arg))))))

;; move to beginning of next function if there is one
(defun sh-tools-next-defun ()
  (interactive)
  (condition-case nil
      (progn
        (forward-line 1)
        (sh-tools-beginning-of-defun -1)
        (beginning-of-line))
    (error (forward-line -1))))

;;; Toggle

;; toggle here-doc indentation:
;; open with '<<-' and use only leading tabs for indentation
(defun sh-tools-toggle-here-doc-indent (point)
  (interactive "d")
  (let ((start-pos (sh-tools-here-doc-p point)))
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
          (setq marker (sh-tools--here-doc-marker))
          (while (and (sh-tools--here-doc-p (point))
                      (not (looking-at-p marker)))
            (delete-horizontal-space)
            (and indent                  ;toggle indentation
                 (insert "\t"))
            (forward-to-indentation)))))))

;; -------------------------------------------------------------------
;;; REPL

(declare-function sh-send-text "sh-script")

;; send selected region and step
(defun sh-tools-send-region (beg end)
  (interactive "r")
  (sh-send-text (buffer-substring-no-properties beg end))
  (goto-char end))

;; ------------------------------------------------------------
;;; Cleanup

;; enforce uft-8-unix on save
(defun sh-tools-cleanup-buffer ()
  (unless (eq 'utf-8-unix buffer-file-coding-system)
    (set-buffer-file-coding-system 'utf-8-unix))
  ;; align backslashes
  (align-regexp (point-min) (point-max) "\\(\\s-*\\)\\\\\\s-*$"))

;; ------------------------------------------------------------

(declare-function company-bash "company-bash")
(declare-function company-shell "company-shell")
(declare-function company-sort-by-backend-importance "company")
(declare-function company-complete "company")
(declare-function bash-completion-dynamic-complete "bash-completion")
(declare-function sh-beginning-of-command "sh-script")

(provide 'sh-tools)
;;; sh-tools.el ends here
