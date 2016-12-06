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
     "\\'")))
  
;; -------------------------------------------------------------------
;;; Utilities

;; synopsis: bash -c 'help -s %s'
;; help:     bash -c 'help %s'
(defsubst sh-help-bash-builtin-sync (cmd &optional synopsis)
  (shell-command-to-string
   (concat "bash -c 'help " (and synopsis "-s ") cmd "'")))

(defsubst sh-help-bash-builtin (cmd &optional sync buffer)
  (let ((cmd (concat "bash -c 'help " cmd "'"))
        (buffer (or buffer "*sh-help*")))
    (if sync
        (call-process-shell-command cmd nil buffer)
      (start-process-shell-command "bash" buffer cmd))))

;; output to BUFFER, return process
;; man --names-only %s | col -b
(defsubst sh-help-man (cmd &optional sync buffer)
  (let ((cmd (concat "man --names-only "
                     (regexp-quote cmd) " | col -b")))
    (if sync
        (call-process-shell-command
         cmd nil (or buffer "*sh-help*"))
      (start-process-shell-command
       "man" (or buffer "*sh-help*") cmd))))

;; do BODY in buffer with man output
(defmacro sh-with-man-help (cmd &optional sync buffer &rest body)
  (declare (indent defun))
  `(let ((buffer (or ,buffer "*sh-help*")))
     ,(if sync
          `(with-current-buffer buffer
             (sh-help-man ,cmd 'sync (current-buffer)))
        `(set-process-sentinel
          (sh-help-man ,cmd nil buffer)
          #'(lambda (p _m)
              (when (zerop (process-exit-status p))
                (with-current-buffer buffer
                  ,@body)))))))

;; if bash builtin do BASH else MAN
(defmacro sh-with-bash/man (cmd bash &rest man)
  (declare (indent 2) (indent 1))
  `(if (string-match-p sh-help-bash-builtins ,cmd)
       ,bash
     ,@man))

;; process BODY in output of help for CMD. Help output is
;; either from 'bash help' for bash builtins or 'man'.
(defmacro sh-with-help (cmd &optional sync buffer &rest body)
  (declare (indent defun) (debug t))
  `(let ((buffer (get-buffer-create (or ,buffer "*sh-help*"))))
     (if ,sync
         (with-current-buffer buffer
             (apply (sh-with-bash/man ,cmd
                      'sh-help-bash-builtin
                      'sh-help-man)
                    ,cmd 'sync `(,buffer))
             ,@body)
       (set-process-sentinel
        (apply (sh-with-bash/man ,cmd 'sh-help-bash-builtin 'sh-help-man)
               ,cmd nil `(,buffer))
        #'(lambda (p _m)
            (when (zerop (process-exit-status p))
              (with-current-buffer buffer
                ,@body)))))))

;;; Parse output

(defsubst sh-help--builtin-string ()
  (goto-char (point-min))
  ;; skip synopsis
  ;; (forward-line 1)
  (buffer-substring (point) (point-max)))

;; make indentation based regexp
(defsubst sh-help--indent-re ()
  (concat "^\\(?:[ \t]*$\\|"
          (buffer-substring
           (point)
           (save-excursion
             (progn (back-to-indentation) (point))))
          "\\)"))

;; return section from man doc, default "DESCRIPTION"
(defsubst sh-help--man-string (&optional section)
  (goto-char (point-min))
  (when (re-search-forward
         (concat "^" (or section "DESCRIPTION")) nil 'move)
    (forward-line)
    (let ((start (point))
          (indent-re (sh-help--indent-re)))
      (while (looking-at-p indent-re)
        (forward-line))
      (buffer-substring start (1- (point))))))

;; condense conditional expression switches
(defsubst sh-help--cond-switches ()
  (goto-char (point-min))
  (when (re-search-forward "^CONDITIONAL")
    (forward-line)
    (let* ((indent-re (sh-help--indent-re))
           (flag-re (concat indent-re "\\([^ \t][^ \\{2,\\}\t\n\r]*\\)"))
           (cont-re "\t[ \t]*\\|^$")
           res key start)
      (when (re-search-forward (concat indent-re "-"))
        (beginning-of-line)
        (while (not (looking-at-p "^[[:alpha:]]"))
          (if (not (looking-at flag-re))
              (forward-line)
            (setq key (match-string 1))
            ;; get description for key
            (setq start (point))
            (while (looking-at-p cont-re)
              (forward-line))
            (push (cons key (buffer-substring start (1- (point))))
                  res))))
      (nreverse res))))

;; -------------------------------------------------------------------
;;; Help at point

(defvar sh-help-cache (make-hash-table :test 'equal))

;; return help string for CMD synchronously, cache result
(defun sh-help--function-string (cmd &optional section recache)
  (or (and (not recache)
           (gethash cmd sh-help-cache))
      (sh-with-help cmd 'sync "*sh-help*"
        (prog1
            (let ((res (sh-with-bash/man cmd
                         (sh-help--builtin-string)
                         (sh-help--man-string section))))
              (puthash cmd res sh-help-cache)
              res)
          (erase-buffer)))))

;; show help in popup tooltip for CMD
;; show SECTION from 'man', prompting with prefix
(defun sh-help-command-at-point (cmd &optional prompt section recache)
  (interactive (list (thing-at-point 'symbol)))
  (when cmd
    (nvp-with-toggled-tip
      (or (sh-help--function-string
           cmd
           (if prompt
               (read-from-minibuffer "Man Section: " "DESCRIPTION")
             section)
           recache)
          (format "No help found for %s" cmd)))))

;; conditional expressions: '[[' '['
(defun sh-help-conditional ()
  (interactive)
  (or (gethash "conditionals" sh-help-cache)
      (sh-help--function-string "bash" "CONDITIONAL EXPRESSIONS")))

;; popup help for thing at point
;; with prefix, show help for thing directly at point
;; otherwise, determine current function and find help for that
;;;###autoload
(defun sh-help-at-point (arg)
  (interactive "P")
  (if (equal arg '(4))
      (call-interactively 'sh-help-command-at-point)
    (let ((cmd (sh-tools-function-name)))
      (cond
       ((member cmd '("[[" "["))
        (sh-help-command-at-point "bash" nil "CONDITIONAL EXPRESSIONS"))
       (t
        ;; with C-u C-u prompt for 'man' section and recache
        (sh-help-command-at-point
         cmd (equal arg '(16)) nil 'recache))))))

(provide 'sh-help)
;;; sh-help.el ends here
