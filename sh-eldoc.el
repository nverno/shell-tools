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
(autoload 'sh-tools-function-name "sh-tools")

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

;; return formatted doc string for bash builtins
(defun sh-eldoc-builtin-string (cmd)
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

;; get synopsis from man output
(defun sh-eldoc--man (cmd)
  (set-process-sentinel 
   (start-process-shell-command
    "man" "*sh-eldoc*" (concat "man " cmd " | col -b"))
   #'(lambda (p _m)
       (when (zerop (process-exit-status p))
         ;; parse man output to get synopsis
         (with-current-buffer "*sh-eldoc*"
           (goto-char (point-min))
           (when (search-forward "SYNOPSIS" nil 'move)
             (forward-line)
             (skip-chars-forward " \t")
             ;; put result in cache
             (puthash
              cmd 
              (concat
               (propertize cmd 'face 'font-lock-function-name-face) ":"
               (buffer-substring
                (+ (length cmd) (point)) (point-at-eol)))
              sh-eldoc-cache)
             (erase-buffer)))))))

;; get doc string from man
(defun sh-eldoc-man-string (cmd)
  (or (gethash cmd sh-eldoc-cache)
      (ignore (sh-eldoc--man cmd))))

;;;###autoload
(defun sh-eldoc-function ()
  "Return eldoc string for bash functions (builtins and those avaliable
from `man %s'."
  (let ((func (sh-tools-function-name)))
    (and func
         (if (string-match sh-eldoc-builtins func)
             (sh-eldoc-builtin-string func)
           (sh-eldoc-man-string func)))))

(provide 'sh-eldoc)
;;; sh-eldoc.el ends here
