;;; sh-eldoc --- eldoc  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/shell-tools
;; Package-Requires: 
;; Created:  4 December 2016

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
  (require 'nvp-macro))
(require 'eldoc)
(autoload 'sh-beginning-of-command "sh-script")

;; ignore ':'
(defvar sh-eldoc-builtins
  (nvp-re-opt
   '("." "[" "alias" "bg" "bind" "break" "builtin" "case" "cd"
     "command" "compgen" "complete" "continue" "declare" "dirs"
     "disown" "echo" "enable" "eval" "exec" "exit" "export" "fc" "fg"
     "getopts" "hash" "help" "history" "if" "jobs" "kill" "let"
     "local" "logout" "popd" "printf" "pushd" "pwd" "read" "readonly"
     "return" "set" "shift" "shopt" "source" "suspend" "test" "times"
     "trap" "type" "typeset" "ulimit" "umask" "unalias" "unset"
     "until" "wait" "while")))
  
(defvar sh-eldoc-cache (make-hash-table :test 'equal))

;; return formatted doc string for minibuffer
(defun sh-eldoc-doc-string (cmd)
  (or (gethash cmd sh-eldoc-cache)
      (let ((str (shell-command-to-string
                  (concat "bash -c 'help -s " cmd "'"))))
        ;; remove 'cmd: ' and trailing newline
        (setq str (substring str (+ 2 (length cmd))
                             (1- (length str))))
        ;; propertize CMD
        (add-text-properties
         0 (length cmd)
         (list 'face 'font-lock-function-name-face) str)
        (puthash cmd str sh-eldoc-cache))))

(provide 'sh-eldoc)
;;; sh-eldoc.el ends here
