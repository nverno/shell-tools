;;; hippie-expand-shell --- expand from history -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/shell-tools
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
  '((shell-mode . comint-input-ring)
    (eshell-mode . eshell-history-ring)))

;; functions to return point at beginning of input line
(defvar-local he-shell-bol nil)

;; for eshell
(defsubst he-shell-eshell-bol ()
  (marker-position (eshell-beginning-of-input)))

(defvar he-shell-bol-alist
  '((shell-mode . comint-line-beginning-position)
    (eshell-mode . he-shell-eshell-bol)))

;; history candidates, eg eshell-history-ring, comint-input-ring
(defvar-local he-shell-history-ring nil)

;; current matches for candidate, to cycle through
(defvar-local he-shell--matches ())
(defvar-local he-shell--index 0)

;; Hippie expansion from shell (comint) history
;; OLD must be nil on first call to function, and t for
;; successive expansions.
;;;###autoload
(defun try-expand-shell-history (old)
  (and he-shell-history-ring
       (let (expansion)
         (unless old
           (he-init-string (funcall he-shell-bol) (point))
           (setq he-shell--index 0)
           (setq he-shell--matches
                 (and (not (equal "" he-search-string))
                      (cl-remove-duplicates
                       (all-completions
                        he-search-string
                        (ring-elements (symbol-value
                                        he-shell-history-ring)))
                       :test 'string=
                       :from-end t))))
         (when he-shell--matches
           (setq expansion (nth he-shell--index he-shell--matches))
           (setq he-shell--index (1+ he-shell--index))
           ;; Setting this cycles it, so other try- functions aren't
           ;; called
           ;; (and (> he-shell--index (1- (length he-shell--matches)))
           ;;      (setq he-shell--index 0))
           )
         (if (not expansion)
             (ignore (and old (he-reset-string)))
           (he-substitute-string expansion t)))))

;;;###autoload
(defun hippie-expand-shell-setup (&optional history bol-fn)
  (interactive)
  (let ((history (or history
                     (cdr (assoc major-mode he-shell-history-alist))))
        (bol (or bol-fn (cdr (assoc major-mode he-shell-bol-alist)))))
    (when (and bol history)
      (setq-local he-shell-history-ring history)
      (setq-local he-shell-bol bol)
      (make-local-variable 'hippie-expand-try-functions-list)
      (add-to-list 'hippie-expand-try-functions-list
                   'try-expand-shell-history))))

(provide 'hippie-expand-shell)
;;; hippie-expand-shell.el ends here
