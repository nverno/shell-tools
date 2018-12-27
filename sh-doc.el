;;; sh-doc --- documentation for sh functions -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/sh-tools
;; Package-Requires: 
;; Created: 17 August 2018

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
  (require 'cl-lib))
(require 'sh-script)

(defvar sh-doc-types '("info" "param" "return" "note" "usage"))
(defvar sh-doc-offset-column 16)

(defsubst sh-doc-active-p ()
  (or (nth 4 (syntax-ppss))
      (and (bolp) (looking-at-p "#"))))

(defun sh-doc-insert (type)
  (interactive (list (completing-read "doc: " sh-doc-types)))
  (and (bolp) (insert "# "))
  (insert "@" type)
  (indent-to-column sh-doc-offset-column))

;;;###autoload
(defun sh-doc-indent-dwim ()
  (interactive)
  (when (and (sh-doc-active-p)
             ;; indent documentation to offset column if necessary
             (or (not (eq (move-to-column sh-doc-offset-column)
                          sh-doc-offset-column))
                 (not (or (eolp)
                          (looking-at-p "\\s-*$")))))
    (beginning-of-line)
    (if (not (looking-at-p (eval-when-compile
                             (concat "#\\s-*@" (regexp-opt sh-doc-types)))))
        (progn (end-of-line)            ;no info keyword, go to end of line
               (call-interactively 'sh-doc-insert))
      (forward-word)
      (delete-horizontal-space)
      (indent-to-column sh-doc-offset-column))))

;;;###autoload
(defun sh-doc-newline-dwim ()
  (interactive)
  (end-of-line)
  (if (sh-doc-active-p)
      (progn
        (newline)
        (call-interactively 'sh-doc-insert))
    (newline)
    (insert "# ")))

(provide 'sh-doc)
;;; sh-doc.el ends here
