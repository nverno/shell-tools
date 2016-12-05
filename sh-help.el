;;; sh-help --- help-at-point for sh -*- lexical-binding: t; -*-

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

;; - Within '[' or '[[' => show switches
;; - Formatted strings?
;; - On shell builtin   => bash -c 'help %s'
;; - Otherwise          => try man or whatis?

;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib))
(autoload 'sh-tools-function-name "sh-tools")

;; ignore ':', not symbolized to match strings
(defvar sh-help-bash-builtins
  (eval-when-compile
    (concat
     (nvp-re-opt
      '("." "[" "[[" "alias" "bg" "bind" "break" "builtin" "case" "cd"
        "command" "compgen" "complete" "continue" "declare" "dirs"
        "disown" "echo" "enable" "eval" "exec" "exit" "export" "fc" "fg"
        "getopts" "hash" "help" "history" "if" "jobs" "kill" "let"
        "local" "logout" "popd" "printf" "pushd" "pwd" "read" "readonly"
        "return" "set" "shift" "shopt" "source" "suspend" "test" "times"
        "trap" "type" "typeset" "ulimit" "umask" "unalias" "unset"
        "until" "wait" "while")
      'no-symbol)
     "\\_>")))
  
;; -------------------------------------------------------------------
;;; Utilities

;; synopsis: bash -c 'help -s %s'
;; help:     bash -c 'help %s'
(defsubst sh-help-bash-builtin-sync (cmd &optional synopsis)
  (shell-command-to-string
   (concat "bash -c 'help " (and synopsis "-s ") cmd "'")))

(defsubst sh-help-bash-builtin (cmd &optional buffer)
  (start-process-shell-command
   "bash" (or buffer "*sh-help*")
   (concat "bash -c 'help " (regexp-quote cmd) "'")))

;; output to BUFFER, return process
;; man --names-only %s | col -b
(defsubst sh-help-man (cmd &optional buffer)
  (start-process-shell-command
   "man" (or buffer "*sh-help*")
   (concat "man --names-only " (regexp-quote cmd) " | col -b")))

(defmacro sh-with-man-help (cmd &optional buffer &rest body)
  (declare (indent defun))
  `(set-process-sentinel
    (sh-help-man ,cmd ,buffer)
    #'(lambda (p _m)
        (when (zerop (process-exit-status p))
          (with-current-buffer ,buffer
            ,@body)))))

(defmacro sh-with-bash/man (cmd bash &rest man)
  (declare (indent 2) (indent 1))
  `(if (string-match-p sh-help-bash-builtins ,cmd)
       ,bash
     ,@man))

;; process BODY in output of help for CMD async. Help output is
;; either from 'bash help' for bash builtins or 'man'.
(defmacro sh-with-help (cmd &optional buffer &rest body)
  (declare (indent defun) (debug t))
  `(set-process-sentinel
    (apply (sh-with-bash/man ,cmd 'sh-help-bash-builtin 'sh-help-man)
           ,cmd ,buffer)
    #'(lambda (p _m)
        (when (zerop (process-exit-status p))
          (with-current-buffer ,(or buffer "*sh-help*")
            ,@body)))))

;; -------------------------------------------------------------------
;;; Help at point

(provide 'sh-help)
;;; sh-help.el ends here
