;;; hippie-expand-shell.el --- expand from history -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/shell-tools
;; Last modified: <2019-01-25 00:24:39>
;; Package-Requires: 
;; Created:  7 December 2016

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

;; Hippie expansion for shell history rings

;;; Code:
(eval-when-compile
  (require 'cl-lib))
(require 'hippie-exp)
(declare-function eshell-beginning-of-input "esh-mode")
(declare-function comint-line-beginning-position "comint")

;; mapping for shells to history rings
(defvar he-shell-history-alist
  '((shell-mode  . comint-input-ring)
    (eshell-mode . eshell-history-ring)
    ;; fallback - if comint derived mode
    (comint-mode . comint-input-ring))
  "Mapping of modes to history rings.")

;; functions to return point at beginning of input line
(defvar-local he-shell-bol nil)

;; for eshell
(defsubst he-shell-eshell-bol ()
  (marker-position (eshell-beginning-of-input)))

(defvar he-shell-bol-alist
  '((shell-mode  . comint-line-beginning-position)
    (eshell-mode . he-shell-eshell-bol)
    (comint-mode . comint-line-beginning-position))
  "Mapping of modes to functions returning line beginning position.")

;; history candidates, eg eshell-history-ring, comint-input-ring
(defvar-local he-shell-history-ring nil)

;; Hippie expansion from shell (comint) history
;; OLD must be nil on first call to function, and t for
;; successive expansions . 
;;;###autoload
(defun try-expand-shell-history (old)
  (unless old
    (he-init-string (funcall he-shell-bol) (point))
    (unless (he-string-member he-search-string he-tried-table)
      (setq he-tried-table (cons he-search-string he-tried-table)))
    (setq he-expand-list                ;build expansion list
          (and (not (equal "" he-search-string))
               (cl-remove-duplicates
                (all-completions
                 he-search-string
                 (ring-elements (symbol-value he-shell-history-ring)))
                :test 'string=
                :from-end t))))
  (while (and he-expand-list            ;remove seen candidates
              (he-string-member (car he-expand-list) he-tried-table))
    (setq he-expand-list (cdr he-expand-list)))
  (prog1 (not (null he-expand-list))
    (if (not he-expand-list)
        (and old (he-reset-string))
      (he-substitute-string (pop he-expand-list) 'trans-case))))

;; -------------------------------------------------------------------
;;; Expansion using function to retrieve history elements

(defsubst he-shell-get-history-default-fn (&optional input-ring)
  (ring-elements (or input-ring
                     (symbol-value he-shell-history-ring))))

;; remove trailing ')' if first char of STR is '(', for expanding in lispy shells
(defsubst he-shell-remove-trailing-paren (str)
  (and str
       (if (and (string= "(" (substring str 0 1))
                (string= ")" (substring str -1)))
           (substring str 0 -1)
         str)))

;; call this function to return history elements to match against
(defvar-local he-shell-get-history-fn 'he-shell-get-history-default-fn)

;; after expanding candidate, call this function on the expansion.
;; e.g for expanding in a lisp shell, remove a trailing ')'
(defvar-local he-shell-post-expand-fn 'identity)

;; Expand from shell history using function to retrieve history and calling
;; function on expanded candidate
;;;###autoload
(defun try-expand-shell-history-2 (old)
  (unless old
    (he-init-string (funcall he-shell-bol) (point))
    (unless (he-string-member he-search-string he-tried-table)
      (setq he-tried-table (cons he-search-string he-tried-table)))
    (setq he-expand-list                ;build expansion list
          (and (not (equal "" he-search-string))
               (cl-remove-duplicates
                (all-completions
                 he-search-string
                 (funcall he-shell-get-history-fn))
                :test 'string=
                :from-end t))))
  (while (and he-expand-list            ;remove seen candidates
              (he-string-member (car he-expand-list) he-tried-table))
    (setq he-expand-list (cdr he-expand-list)))
  (prog1 (not (null he-expand-list))
    (if (not he-expand-list)
        (and old (he-reset-string))
      (he-substitute-string
       (funcall he-shell-post-expand-fn (pop he-expand-list)) 'trans-case))))

;; -------------------------------------------------------------------
;;; Expand shell aliases, eg. bash shell-expand-alias C-M-e 

(autoload 'nvp-shell-get-alias "nvp-shell")

;;;###autoload
(defun try-expand-shell-alias (old)
  "Expand shell alias, like bash shell-expand-alias."
  (if (not old)
      (progn
        (he-init-string (comint-line-beginning-position) (point))
        (if (not (he-string-member he-search-string he-tried-table))
            (setq he-tried-table (cons he-search-string he-tried-table)))
        (let ((alias (nvp-shell-get-alias he-search-string)))
          (when alias                   ;substitute once and return nil
            (he-substitute-string alias)
            (setq he-expand-list nil)
            t)))
    (progn
      (he-reset-string)
      nil)))

;; -------------------------------------------------------------------
;;; Setup 

;;;###autoload
(cl-defun hippie-expand-shell-setup (&key history bol-fn history-fn expand-fn)
  "Setup shell to use hippie expansion for shell history."
  (interactive)
  (let* ((comint-p (derived-mode-p 'comint-mode))
         (history (or history
                      (cdr (assoc major-mode he-shell-history-alist))
                      (and comint-p
                           (cdr (assoc 'comint-mode he-shell-history-alist)))))
         (bol (or bol-fn
                  (cdr (assoc major-mode he-shell-bol-alist))
                  (and comint-p (cdr (assoc 'comint-mode he-shell-bol-alist))))))
    (when (and (fboundp bol) (not (null history)))
      (setq-local he-shell-history-ring history)
      (setq-local he-shell-bol bol)
      (make-local-variable 'hippie-expand-try-functions-list)
      (if (not (or expand-fn history-fn))
          (add-to-list 'hippie-expand-try-functions-list 'try-expand-shell-history)
        (and expand-fn (setq-local he-shell-post-expand-fn expand-fn))
        (and history-fn (setq-local he-shell-get-history-fn history-fn))
        (add-to-list 'hippie-expand-try-functions-list
                     'try-expand-shell-history-2)))))

(provide 'hippie-expand-shell)
;;; hippie-expand-shell.el ends here
