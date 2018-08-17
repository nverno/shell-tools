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

(defvar sh-doc-types '(info param return note usage))
(defvar sh-doc-offset-column 24)

(defsubst sh-doc-active-p ()
  (nth 4 (syntax-ppss)))

;;;###autoload
(defun sh-doc-insert (type)
  (interactive
   (list (completing-read "doc: " sh-doc-types)))
  (and (bolp) (insert "# "))
  (insert "@" type)
  (indent-to-column sh-doc-offset-column))

;; (defun sh-doc-get-info ()
;;   (sh-in-comment-or-string)
;;   )

(provide 'sh-doc)
;;; sh-doc.el ends here
