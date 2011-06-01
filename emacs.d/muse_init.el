(setq load-path (add-to-list 'load-path "/home/heipang/.emacs.d/muse"))
(setq load-path (add-to-list 'load-path "~/.emacs.d/plugins"))

(require 'muse-mode)
(require 'muse-colors)
(require 'muse-mode)
(require 'muse-html)
(require 'muse-latex)
(require 'muse-texinfo)
(require 'muse-docbook)
(require 'muse-wiki)
(require 'muse-project)
(require 'htmlize)
(require 'tp-muse-highlight nil t)


(setq muse-project-alist
      '(("MainPage" 
	 ("~/document/wiki" :default "index")
         (:base "html" :path "~/document/wiki/Home_Page"))
	("Computer"
	 ("~/document/wiki/computer/" :default "index")
	 (:base "html" :path "~/document/wiki/Home_Page/computer"))))


(setq
 ;; 使用 ido 来补全
 muse-completing-read-function 'ido-completing-read
 ;; 使用 lisp 标签时不在打开 muse 文件时求值
 muse-colors-evaluate-lisp-tags nil
 ;; 设置 (muse-publishing-directive "date") 的格式
 muse-publish-date-format "%Y 年 %m 月 %d 日"
 ;; 单独的项目名字不会成为 wiki 链接
 muse-wiki-ignore-bare-project-names t)

(define-key global-map "\C-c\C-f" 'muse-project-find-file)

					;;==设置编码方式为utf-8
(setq muse-html-meta-content-type (concat "text/html; charset=utf-8"))

					;;==新建一个wiki工程
(setq muse-project-alist
      '(("MyWiki"
         ("~/documents/wiki" :default "index")
         (:base "html" :path "~/document/wiki/publish"))))


(add-hook 'muse-mode-hook
          '(lambda ()
             (setq outline-regexp "\\*+ ")
             (outline-minor-mode)))

(defvar muse-colors-overlays nil)
(defun muse-colors-src-tag (beg end)
  "Strip properties and mark as literal."
  (let (face)
    (muse-unhighlight-region beg end)
     (save-excursion
      (goto-char beg)
      (let ((fs 1) content face-list fe mode attrs number ov ovs
	    (font-lock-verbose nil))
	(when (re-search-forward "<src\\(.*\\)>" nil t)
	  (setq beg (match-end 0)
		attrs (mapcar
		       (lambda (pair)
			 (setq pair (split-string pair "="))
			 (setcdr pair (substring (cadr pair) 1 -1))
			 pair)
		       (split-string (match-string 1)))
		mode (and (assoc "lang" attrs)
			  (intern-soft (concat (cdr (assoc "lang" attrs))
					       "-mode"))))
	  (when (and mode (fboundp mode))
	    (goto-char end)
	    (setq end
		  (if (re-search-backward "</src>" nil t)
		      (match-beginning 0)
		    (point-max))
		  content (buffer-substring-no-properties beg end))
	    (with-current-buffer (get-buffer-create "*muse-temp*")
	      (funcall mode)
	      (insert content)
	      (font-lock-fontify-buffer)
	      (htmlize-ensure-fontified)
	      (or (get-text-property fs 'face)
		  (setq fs (next-single-property-change fs 'face)))
	      (while (and fs (< fs (point-max)))
		(setq fe (or (next-single-property-change fs 'face)
			     (point-max))
		      face (get-text-property fs 'face))
		(and face fe (setq face-list (cons (list (1- fs) (1- fe) face) face-list)))
		(setq fs fe))
	      (kill-buffer (current-buffer)))
	    (when face-list
	      ;; (message "%S" face-list)
	      (dolist (f (nreverse face-list))
		(put-text-property (+ beg (car f)) (+ beg (cadr f))
				   'face (nth 2 f)))))
	  (when (and (assoc "number" attrs)
		     (setq number (string-to-number (cdr (assoc "number" attrs)))))
	    (mapc (lambda (o)
		    (let ((pos (overlay-start o)))
		      (if (or (null pos)
			      (and (> pos beg) (< pos end)))
			  (delete-overlay o)
			(push o ovs))))
		  muse-colors-overlays)
	    (setq muse-colors-overlays ovs)
	    (goto-char beg)
	    (forward-line 1)
	    (while (and (not (eobp)) (< (point) end))
	      (when (not (looking-at "</src>"))
		(setq ov (make-overlay (point) (point)))
		(push ov muse-colors-overlays)
		(overlay-put ov 'before-string (format "%4d " number))
		(setq number (1+ number)))
	      (forward-line 1))))))))

