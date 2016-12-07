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

;; Hippie expansion from shell (comint) history

;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib))
(require 'hippie-exp)
(require 'comint)

;; current matches for candidate, to cycle through
(defvar-local he-comint-matches ())
(defvar-local he-comint-index 0)

;; Hippie expansion from shell (comint) history
;; OLD must be nil on first call to function, and t for
;; successive expansions.
;;;###autoload
(defun try-expand-comint-history (old)
  (and comint-input-ring
       (let (expansion)
         (unless old
           (he-init-string (comint-line-beginning-position) (point))
           (setq he-comint-index 0)
           (setq he-comint-matches
                 (and (not (equal "" he-search-string))
                      (cl-remove-duplicates
                       (all-completions
                        he-search-string
                        (ring-elements comint-input-ring))
                       :test 'string=))))
         (when he-comint-matches
           (setq expansion (nth he-comint-index he-comint-matches))
           (setq he-comint-index (1+ he-comint-index))
           ;; Setting this cycles it, so other try- functions aren't
           ;; called
           ;; (and (> he-comint-index (1- (length he-comint-matches)))
           ;;      (setq he-comint-index 0))
           )
         (if (not expansion)
             (ignore (and old (he-reset-string)))
           (he-substitute-string expansion t)))))

;;;###autoload
(defun hippie-expand-shell-setup ()
  (interactive)
  (make-local-variable 'hippie-expand-try-functions-list)
  (add-to-list 'hippie-expand-try-functions-list
               'try-expand-comint-history))

(provide 'hippie-expand-shell)
;;; hippie-expand-shell.el ends here
