;;;; History mode for Emacs
;;;; Author: Dai Yuwen 
;;;; $Date: 2006/04/09 02:58:45 $
;; This file is not yet part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301 USA

;;; Change Log:
;; 2006-04-09 add minibuffer completion when add to or grep in a project 
;;            fix a bug: history-excluded-list doesn't have effect when 
;;            user write a non-absolute path

;; 2006-04-08 add a new feature: grep in a project

;; 2006-04-05 rewrite history-add-to-project, let the user choose what
;; action they want. 

;; 2006-04-04 fix a bug: add files to a project that already exists,
;; a duplicated project will be created.



;;; global variable
(defcustom history-file "~/.emacs-history.el"
  "User can define the history file."
  :type 'string
  :group 'history)

(defcustom project-file "~/.emacs-projects.el"
  "User can define the project file."
  :type 'string
  :group 'history)

(defcustom history-excluded-list (list history-file project-file)
  "User can define the history file."
  :type 'list
  :group 'history)

(defvar history nil)
(defvar history-buffer nil)
(defvar history-mode-hook nil)
(defvar history-marked-files nil)
(defvar history-projects nil)
(defvar history-current-project nil)
(defvar history-project-changed nil)
(defvar overlay-table (make-hash-table :test 'equal :size 32))

(defvar history-grep-history nil)
(defvar history-add-project-history nil)
(defvar history-excluded-list2 nil
  "This list is correspondint to history-excluded-list customized by user,
but contains absolute path.")

;; this is list is evaluated when the package is loaded
(setq history-excluded-list2 (mapcar #'expand-file-name history-excluded-list))


;;; help functions
(defun left-trim (str)
  "Remove the left blanks of a string."
  (let ((i 0)
        (len (length str)))
    (while (and (< i len)
                (or (eq (aref str i) 32)
                    (eq (aref str i) 9)))
      (setq i (1+ i)))
    (substring str i len)))

(defun right-trim (str)
  "Remove the right blanks of a string."
  (let ((i (1- (length str))))
    (while (and (>= i 0)
                (or (eq (aref str i) 32)
                    (eq (aref str i) 9)))
      (setq i (1- i)))
    (substring str 0 (1+ i))))

(defun trim (str)
  (left-trim (right-trim str)))

(defun history-today ()
  (let ((l (decode-time (current-time))))
    (list (nth 5 l) (nth 4 l) (nth 3 l))))

(defun history-combine-lists (a b)
  (let ((result a))
    (dolist (e b)
      (unless (member e result)
        (push e result)))
    result))

;;; history mode related functions
(defun history-end-of-line ()
  "Get the position of the end of the current line."
  (save-excursion
    (let ((junk (end-of-line)))
      (point))))

(defun history-beginning-of-line ()
  "Get the position of the end of the current line."
  (save-excursion
    (let ((junk (beginning-of-line)))
      (point))))

(defun history-hight-region (name begin end)
  "Mark the region from BEGIN to END with the HIGHLIGHT face."
  (let ((ov (make-overlay begin end)))
    (overlay-put ov 'face 'highlight)
    (puthash name ov overlay-table))) ; remember the file overlay pair

(defun history-un-highlight (name begin end)
  "Delete the highlight overlay."
  (delete-overlay (gethash name overlay-table)))

(defvar history-mode-map
  (let ((history-mode-map (make-keymap)))
    (define-key history-mode-map " " 'history-mark-file)
    (define-key history-mode-map [double-down-mouse-1] 'history-mark-file)
    (define-key history-mode-map "d" 'history-delete-project)
    (define-key history-mode-map "g" 'history-grep-project)
    (define-key history-mode-map "\C-m" 'history-open-file)
    (define-key history-mode-map "o" 'history-open-file-other-window)
    (define-key history-mode-map "O" 'history-open-marked-files)
    (define-key history-mode-map "w" 'history-open-file-other-frame)
    (define-key history-mode-map "q" 'quit-window)
    (define-key history-mode-map "j" 'forward-line)
    (define-key history-mode-map "k" #'(lambda () (interactive) (forward-line -1)))
    (define-key history-mode-map "p" 'history-add-to-project)
    (define-key history-mode-map "s" 'history-save-projects)
    (define-key history-mode-map "\C-xk" 'history-confirm-kill)
    history-mode-map)
  "Keymap for HISTORY major mode")

(defun history-mark-file ()
  "HISTORY-MARK-FILE toggles marking the file."
  (interactive)
  (let ((file (history-get-file-name)))
    (cond ((and (file-readable-p file)
             (not (equal file ""))
             (not (member file history-marked-files)))
           ;; mark it
           (push file history-marked-files)
           (history-hight-region file
                                 (history-beginning-of-line)
                                 (history-end-of-line)))
          ;; already marked, unmark-it
          ((member file history-marked-files)
           (setq history-marked-files (delete file history-marked-files))
           (history-un-highlight file
                                 (history-beginning-of-line)
                                 (history-end-of-line)))
          (t
           (message "not a valid file")))
    (forward-line)))

(defun history-add-to-project (s)
  "HISTORY-ADD-TO-PROJECT adds the marked files to a project named S."
;  (interactive (list (read-string "Add to project: " history-current-project  nil)))
  (interactive (list (completing-read "Add to project: "
                                      history-projects  ; collection
                                      nil nil
                                      (car history-add-project-history)  ; init value, most current project
                                      'history-add-project-history)))    ; minibuffer history list
  (when
      (catch 'abort
        (cond ((not history-marked-files)
               (message "No files marked")
               (throw 'abort nil))
              ;; new project name specified
              ((assoc s history-projects)  ; it already exists
               (let ((answer (read-from-minibuffer "Project exists.[a]dd/[c]ancel/[o]verwrite?")))
                 (cond ((equal answer "a") 
                        (history-add-to-exists s))
                       ((equal answer "c")
                        (message "adding canceled")
                        (throw 'abort nil))
                       ((equal answer "o")
                        (message "overwrite")
                        (setcdr (assoc s history-projects) history-marked-files))
                       (t
                        (message "Please type a, c or o")
                        (throw 'abort nil)))))
              ;; a new project
              ((not (equal s ""))
               (push (cons s history-marked-files) history-projects))
              (t
               (message "No project specified")
               (throw 'abort nil))))
    ;; project created or modified, reset these values
    (setq history-current-project s) ; set as current project
    (setq history-project-changed t)    ; set flag 
    (dolist (file history-marked-files) ; delete the overlays
      (delete-overlay (gethash file overlay-table)))
    (setq history-marked-files nil)     ; reset the marked files list
    (history-save-projects)
    (history-revert-buffer)))

(defun history-add-to-exists (p)
  "Add files to an already exists project P."
  (let* ((exists-files
          (cdr (assoc p history-projects)))
         ;; append is not proper, use history-combine-lists
         (all-files (history-combine-lists exists-files history-marked-files)))
    (setcdr (assoc p history-projects) all-files)))

(defun history-open-file ()
  "Open the file in the current buffer."
  (interactive)
  (let ((file (history-get-file-name)))
    (cond ((and (file-readable-p file) (not (equal file "")))
           (find-file file))            ; only one file
          ((assoc file history-projects) ; it's a project name
           (let ((files (cdr (assoc file history-projects))))
             (dolist (f files)
               (find-file f))))         ; open all the file
          (t (message "invalid file name")))))

(defun history-open-marked-files ()
  "Open the marked files."
  (interactive)
  (if (not history-marked-files)
      (message "No files marked.")
    (dolist (file history-marked-files)
      (find-file file))))

(defun history-open-file-other-window ()
  "Open the file in the other window."
  (interactive)
  (let ((file (history-get-file-name)))
    (if (and (file-readable-p file) (not (equal file "")))
        (find-file-other-window file))))

(defun history-open-file-other-frame ()
  "Open the file in the other frame."
  (interactive)
  (let ((file (history-get-file-name)))
    (if (and (file-readable-p file) (not (equal file "")))
        (find-file-other-frame file))))

(defun history-get-file-name ()
  "get the file name in the current line."
  (save-excursion
    ;; Get the current line
    (beginning-of-line)
    (let* ((beg (point))
           (junk (end-of-line))
           (end (point))
           (line (buffer-substring beg end))
           (file (trim line)))          ; get file name
      ;; return file name
      file)))

(defun history-mode ()
  "Major mode for Emacs history.
Save the history of opened files. User can open files quickly from the history buffer.
User can also define PROJECT that includes  several files, and when the user opens the
project, all files in the project can be opened.

\\{history-mode-map}"
  (kill-all-local-variables)
  (use-local-map history-mode-map)
  (setq major-mode 'history-mode)
  (setq mode-name "History")
  (run-hooks 'history-mode-hook))

(defun history-read ()
  "Read history from a disk file and store it to a object."
  (save-excursion
    (let ((buffer (find-file-noselect history-file)))
      (set-buffer buffer)
      (goto-char (point-min))
      (condition-case nil               ; catch end-of-file error
          (while (setq obj (read buffer))
            ;; files
            (let ((date (car obj))
                  (files (cdr obj)))   ; file list here

              (dolist (f files) ; add file name to the history list
                (if (assoc date history)
                    (if (not (member f (assoc date history)))
                        (setcdr (assoc date history)
                                (cons f (cdr (assoc date history)))))
                  (setq history (cons (list date f) history))))))
        (end-of-file (message "in history-read...") (kill-buffer buffer) (message "after killing buffer ..."))))))

(defun history-read-project ()
  "Read project from a disk file."
  (save-excursion
    (let ((buffer (find-file-noselect project-file)))
      (set-buffer buffer)
      (goto-char (point-min))
      (condition-case nil ; catch end-of-file error
          (setq history-projects (read buffer))
        (end-of-file (message "Project file empty")
                     (kill-buffer buffer))))))

(defun history-display (h)
  "Display the history in a buffer."
  (if (not (buffer-live-p history-buffer))
      (setq history-buffer (get-buffer-create "*History*")))
  (save-excursion
    (set-buffer history-buffer)
    (setq buffer-read-only nil)
    (let ((w h))
      (while w
        (let* ((e (car w))
               (d (car e))              ; date
               (f (cdr e)))             ; files
          (insert (format "%d-%02d-%02d" (nth 0 d) (nth 1 d) (nth 2 d)) "\n") ; print date
          (dolist (l f)                 ; print file list
            (insert "\t" l "\n")))
        (setq w (cdr w))))
    (goto-char (point-min))
    (setq buffer-read-only t)
    (history-mode))
  (switch-to-buffer history-buffer))

(defun history-display-projects (p)
  "Display the projects."
  (save-excursion
    (set-buffer history-buffer)
    (setq buffer-read-only nil)
    (dolist (e p)
      (let ((name (car e))
            (files (cdr e)))
        (insert name "\n")              ; project name
        (dolist (f files)
          (insert "\t" f "\n"))))
    (goto-char (point-min))
    (setq buffer-read-only t))
  (switch-to-buffer history-buffer))

(defun history ()
  (interactive)
  (if (buffer-live-p history-buffer)    ; if exists, switch to it
      (switch-to-buffer history-buffer)
    (setq history-marked-files nil)     ; reset it
    (setq history-projects nil)         ; reset it
    (setq history-current-project nil)  ; reset it
    (setq history-project-changed nil)  ; reset it
    (history-read)
    (history-read-project)
    (history-display history)
    (history-display-projects history-projects)))

(defun history-revert-buffer ()
;  (setq history-marked-files nil)       ; reset it
;  (setq history-projects nil)           ; reset it
;  (setq history-current-project nil)    ; reset it
  (history-read)
;  (history-read-project)
  (setq buffer-read-only nil)
  (erase-buffer)
  (history-display history)
  (history-display-projects history-projects))

(defun history-record-open ()
  "Write the date and file in the history file."
  (let ((name (expand-file-name
               (cdr (assoc 'buffer-file-truename (buffer-local-variables))))))
    (unless (member name history-excluded-list2)
      (save-excursion
        (let ((buffer (find-file-noselect history-file)))
          (set-buffer buffer)
          (goto-char (point-max))
          (print (list (history-today) name) buffer)
          (save-buffer))))))

(defun history-save-projects ()
  "Save the projects to a file."
  (interactive)
  (save-excursion
    (let ((buffer (find-file-noselect project-file)))
      (set-buffer buffer)
      (erase-buffer)
      (goto-char (point-max))
      (print history-projects buffer)
      (save-buffer)
      (kill-buffer buffer)))
  (setq history-project-changed nil))

(defun history-delete-project ()
  "Delete the project."
  (interactive)
  (let ((p (history-get-file-name)))
    (when (and p (assoc p history-projects) ; if it's a project
               (yes-or-no-p (format "Delete the project %s?" p)))
      (let ((e (assoc p history-projects)))
        (setq history-projects (delete e history-projects))
        (setq history-project-changed t)
        (history-revert-buffer)))))

(defun history-confirm-kill ()
  "The history-project has been modified. Ask the user if he wants to save it."
  (interactive )
  (if (and history-project-changed
           (yes-or-no-p "Project(s) has been modified. Do you want to save it?"))
      (history-save-projects))
  (kill-buffer history-buffer)
  (setq history-project-changed nil)) ; reset it, for next using

(add-hook 'find-file-hooks 'history-record-open)

;;; search in a project
(defun history-separat-list (list separator)
  (let ((result ()))
    (dolist (e list)
      (setq result (nconc result (list separator e))))
    result))

(defun history-grep-project (args p)
  "Run GREP with options ARGS and files in project P."
  (interactive
   (let ((args (read-string "Run grep (like this): " "grep -n -e " nil))
         ;; use completion feature of the minibuffer
         ;; fortunately, history-project is an alist
         ;; that's suitable for completing-read
         (p (completing-read "Project: " history-projects nil nil (car history-grep-history) 'history-grep-history)))
     (list args p)))
  (cond ((assoc p history-projects)
         (let* ((files (cdr (assoc p history-projects)))
                (s (history-separat-list files " "))
                (final-args (concat args (apply #'concat s))))
           ;; invoke grep
           (grep final-args)))
        (t
         (message "Project doesn't exist"))))

(provide 'history)



