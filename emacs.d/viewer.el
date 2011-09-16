
(defvar view-mode-setted nil)
(defun view-mode-settings ()
  (unless view-mode-setted
    (setq view-mode-setted t)
    (define-key view-mode-map "a" 'move-beginning-of-line)
    (define-key view-mode-map "e" 'move-end-of-line)
    (define-key view-mode-map "n" 'next-line)
    (define-key view-mode-map "p" 'previous-line)
    (define-key view-mode-map "f" 'forward-word)
    (define-key view-mode-map "b" 'backward-word)
    (define-key view-mode-map "v" 'set-mark-command)
    (define-key view-mode-map "o" 'other-window)
    (define-key view-mode-map "G" 'end-of-buffer)
    (define-key view-mode-map "i" 'view-mode)
    (define-key view-mode-map "c" 'view-forward-to-char)
    (define-key view-mode-map "C" 'view-backward-to-char)
    (define-key view-mode-map "l" 'View-scroll-line-backward)
    (define-key view-mode-map "m" 'View-scroll-line-forward)
    (defun view-forward-to-char (n char)
      "Move forward to Nth occurence of CHAR.
Typing CHAR again will move forwad to the next Nth occurence of CHAR."
      (interactive "p\ncForward to char: ")
      (search-forward (string char) nil nil n)
      (while (char-equal (read-char) char)
	(search-forward (string char) nil nil n))
      (setq unread-command-events (list last-input-event)))

;;;###autoload
    (defun view-backward-to-char (n char)
      "Move backward to Nth occurence of CHAR.
Typing CHAR again will move backward to the next Nth occurence of CHAR."
      (interactive "p\ncBackward to char: ")
      (search-backward (string char) nil nil n)
i      (while (char-equal (read-char) char)
	(search-backward (string char) nil nil n))
      (setq unread-command-events (list last-input-event)))

    (defface view-mode-mode-line-face
      '((((type tty pc)) :bold t :background "red" :foreground "white") (t (:background "red" :foreground "white")))
      "Face used highlight `view-mode-line-format'.")

    (defvar view-mode-line-format
      (propertize "View"
		  ;; 'local-map mode-line-minor-mode-keymap
		  ;; 'help-echo "mouse-3: minor mode menu"
		  'face 'view-mode-mode-line-face)
      "*Mode line format of `view-mode'.")

    (put 'view-mode-line-format 'risky-local-variable t)

    (setq minor-mode-alist
	  (append
	   `((view-mode " ") (view-mode ,view-mode-line-format))
	   (delq (assq 'view-mode minor-mode-alist) minor-mode-alist) ))
    )
)

;; eval-after-load里的view-mode-setting总是执行，所以改为add-hook方式，估计是因为emacs的默认主页就是view mode的关系吧
(add-hook 'view-mode-hook 'view-mode-settings)
(defun view-exist-file ()
  "Only when variable `buffer-file-name' is exist, enter function `emaci-mode'."
  (when (file-exists-p (buffer-file-name))
    (view-mode)))
(add-hook 'find-file-hook 'view-exist-file)
(global-set-key (kbd "<f4>") 'view-mode)
