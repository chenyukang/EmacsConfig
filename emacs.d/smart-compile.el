;;; smart-compile.el --- an interface to `compile' 改造版

;; Copyright (C) 1998-2004  Seiji Zenitani <zenitani@mac.com>

;; Author: Seiji Zenitani <zenitani@mac.com>
;; Version: v20040730
;; Keywords: tools, unix
;; Created: 1998-12-27
;; Compatibility: Emacs 20, 21
;; URL(en): http://home.att.ne.jp/alpha/z123/elisp-e.html
;; URL(jp): http://home.att.ne.jp/alpha/z123/elisp-j.html#smart-compile

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package provides `smart-compile' function.
;; You can associates a particular file with a paticular compile functions,
;; by editing `smart-compile-alist'.
;;
;; To use this package, add these lines to your .emacs file:
;;     (require 'smart-compile)
;;
;; Note that it requires emacs 20 or later.

;;; Code:

;; user customize variables
(defgroup smart-compile nil
  "smart-compile"
  :group 'languages
  :prefix "smarct-compile-")

(defcustom smart-compile-alist '(
  ("\\.c\\'"          . "gcc -O2 %f -lm -o %n")
  ("\\.[Cc]+[Pp]*\\'" . "g++ -O2 %f -lm -o %n")
  ("\\.asm\\'"        . "nasm %f -o %n")
  ("\\.java\\'"       . "javac %f")
  ("\\.f90\\'"        . "f90 %f -o %n")
  ("\\.[Ff]\\'"       . "f77 %f -o %n")
  ("\\.tex\\'"        . (tex-file))
  ("\\.pl\\'"         . "perl -cw %f")
  ("\\.m\\'"          . "gcc -O2 %f -lobjc -lpthread -o %n")
  ("\\.py\\'"         . "python %f")
  (emacs-lisp-mode    . (emacs-lisp-byte-compile))
)  "List of compile commands. In argument,
some keywords beginning with '%' will be replaced by:
  smart-compile-replace-alist
"
   :type '(repeat
           (cons :tag  "Association: mode/compilation functions"
                 (choice :tag "file suffix or major-mode"
                         (string)
                         (function))
                 (choice :tag "command or function"
                         (string)
                         (function))))
   :group 'smart-compile)

(defcustom smart-compile-run-alist '(
  ("\\.c\\'"          . "./%n")
  ("\\.[Cc]+[Pp]*\\'" . "./%n")
  ("\\.asm\\'"        . "./%n")
  ("\\.java\\'"       . "java -cp . %n")
  ("\\.f90\\'"        . "./%n")
  ("\\.[Ff]\\'"       . "./%n")
  ("\\.tex\\'"        . (tex-file))
  ("\\.pl\\'"         . "perl -cw %f")
  ("\\.m\\'"          . "./%n")
  (emacs-lisp-mode    . (emacs-lisp-byte-compile))
)  "List of compile commands. In argument,
some keywords beginning with '%' will be replaced by:
  smart-compile-replace-alist
"
   :type '(repeat
           (cons :tag  "Association: mode/compilation functions"
                 (choice :tag "file suffix or major-mode"
                         (string)
                         (function))
                 (choice :tag "command or function"
                         (string)
                         (function))))
   :group 'smart-compile)

(defcustom smart-compile-replace-alist '(
  ("%F" . (buffer-file-name))
  ("%f" . (file-name-nondirectory (buffer-file-name)))
  ("%n" . (file-name-sans-extension
           (file-name-nondirectory (buffer-file-name))))
  ("%e" . (file-name-extension (buffer-file-name)))
  ) "
    %F  absolute pathname            ( /usr/local/bin/netscape.bin )
    %f  file name without directory  ( netscape.bin )
    %n  file name without extention  ( netscape )
    %e  extention of file name       ( bin )
  "
    :type '(repeat
            (cons :tag "Association: pattern/replace functions"
                  :tag "pattern"
                  (string)
                  :tag "replace function"
                  (function)))
   :group 'smart-compile)

(defcustom smart-compile-make-program "make "
  "*The `make' program used to process makefiles.
If you have GNU make installed with name \"gmake\" use it."
  :type 'string
  :group 'smart-compile)

(defcustom smart-compile-makefile-regexp
  "\\(^[Mm]akefile\\|.*\\.[mM][aA]?[kK][eE]?\\.?.*$\\)"
  "Regexp matching 'could be' makefiles filenames."
  :type 'regexp
  :group 'smart-compile)

(defvar smart-compile-notdef-localval t)
(make-variable-buffer-local 'smart-compile-notdef-localval)
(defvar smart-compile-chek-localval nil)
(make-variable-buffer-local 'smart-compile-chek-localval)
(defvar smart-compile-cmmand nil)
(make-variable-buffer-local 'smart-compile-cmmand)

;;;###autoload
(defun smart-compile ()
  "An interface to `compile'.
It calls `compile' or other compile functions.
If you set `smart-compile-alist' in your .emacs,
you can define your own compile commands."
  (interactive)
  (let ((name (buffer-file-name))
        (not-yet t))

    (if (not name)
        (error "cannot get filename."))

    (smart-compile-chek-localval)

    (if smart-compile-notdef-localval
        (progn
          (kill-local-variable 'compile-command)

          ;; compile
          (setq not-yet
                (smart-compile-command-make smart-compile-alist))

          ;; If compile-command is not defined and the contents begins with "#!",
          ;; set compile-command to fikename.
          (if not-yet
              (smart-compile-shell))
          ))

    ;; compile
    (if not-yet
        (call-interactively 'compile))

    ))

(defun smart-compile-chek-localval ()
  (if (not smart-compile-chek-localval)
      (progn
        (if (and smart-compile-notdef-localval
                 (local-variable-p 'compile-command)
                 compile-command)
            (progn
              (setq smart-compile-notdef-localval nil)
              (setq smart-compile-chek-localval t))
          ))
    ))

(defun smart-compile-buildfile ()
  "buildfileの存在判定関数"
  (interactive)
  (smart-compile-chek-localval)
  (if (and smart-compile-notdef-localval
           (or (file-readable-p "Makefile")
               (file-readable-p "makefile")))
      (progn
        (set (make-local-variable 'compile-command) smart-compile-make-program)
        (call-interactively 'compile))
    (error "buildfile not foud.")
    ))

(defun smart-compile-in-tex-mode ()
  "Returns t if in latex or tex mode, nil otherwise"
  (or
   (eq major-mode 'tex-mode)
   (eq major-mode 'latex-mode)))

(defun smart-compile-next-error ()
  "Goes to the next error in a smart way."
  (interactive)
  (if (smart-compile-in-tex-mode)
      (call-interactively 'TeX-next-error)
    (next-error)))

(defun smart-compile-command-make (arg)
  "compile-commandを作成する"
  (let ((alist arg)
        (rlist smart-compile-replace-alist)
        (case-fold-search nil)
        (function nil)
        (not-yet t))
    (while (and alist)
      (if (or
           (and (symbolp (caar alist))
                (eq (caar alist) major-mode))
           (and (stringp (caar alist))
                (string-match (caar alist) (buffer-file-name))))
          (progn
            (setq function (cdar alist))
            (if (stringp function)
                (progn
                  (let ((command function))
                    (while rlist
                      (while (string-match (caar rlist) command)
                        (setq command
                              (replace-match
                               (eval (cdar rlist)) t nil command)))
                      (setq rlist (cdr rlist)))
                    (set (make-local-variable 'compile-command) command))
                  (call-interactively 'compile)
                  )
              (eval function))
            (setq alist nil)
            (setq not-yet nil))
        (setq alist (cdr alist)))
      )
    not-yet))

(defun smart-compile-shell ()
  " 一行目に記述のある #! の指定に従い実行"
  (interactive)
  (let ((name (buffer-file-name)))
    (if (and
;         (not (memq system-type '(windows-nt ms-dos)))
         (not (string-match "/\\.[^/]+$" name)))
        (save-excursion
          (save-restriction
            (widen)
            (beginning-of-buffer)
            (let ((bol (progn (beginning-of-line) (point)))
                  (eol (progn (end-of-line) (point))))
              (let ((firstline (buffer-substring bol eol)))
                (if (equal "#!" (substring firstline 0 2))
                    (set (make-local-variable 'compile-command)
                         (concat (substring firstline 3) " " name))
                  ))))))))

(defun smart-compile-run ()
  "プログラムの走行"
  (interactive)
  (smart-compile-chek-localval)
  (if smart-compile-notdef-localval
      (progn
        (kill-local-variable 'compile-command)
        (smart-compile-command-make smart-compile-run-alist))
    (call-interactively 'compile))
  )

;;; smart-compile.el ends here
(provide 'smart-compile)
