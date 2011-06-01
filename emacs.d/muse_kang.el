
(add-to-list 'load-path "~/.emacs.d/plugins")
(add-to-list 'load-path "~/.emacs.d/muse")
(require 'muse-mode)     ; load authoring mode
(require 'muse-html)     ; load publishing styles I use
(require 'muse-latex)
(require 'muse-texinfo)
(require 'muse-docbook)
(require 'muse-project)  ; publish files in projects

(setq muse-project-alist
      '(("MainPage" 
	 ("~/document/blog" :default "index")
         (:base "html" :path "~/document/blog/Home_Page"))
	("Computer"
	 ("~/document/wiki/computer/" :default "index")
	 (:base "html" :path "~/document/wiki/Home_Page/computer"))))

(add-hook 'muse-mode-hook
          '(lambda ()
             (setq outline-regexp "\\*+ ")
             (outline-minor-mode)))

(require 'muse-wiki)
(require 'muse-project)
(require 'htmlize)
(require 'tp-muse-highlight nil t)

;;==设置编码方式为utf-8
(setq muse-html-meta-content-type (concat "text/html; charset=utf-8"))

;;==新建一个wiki工程
(setq muse-project-alist
      '(("MainPage"
	 ("~/document/blog/Home" :default "index")
	 (:base "html" :path "~/document/blog/Home_Page"))
	("Computer"
	 ("~/document/blog/Home/Computer/" :default "index")
	 (:base "html" :path "~/document/blog/Home_Page/Computer"))
	("MovieMusic"
	 ("~/document/blog/Home/MovieMusic" :default "index")
	 (:base "html" :path "~/document/blog/Home_Page/MovieMusic"))
	("Sport"
	 ("~/document/blog/Home/Sport" :default "index")
	 (:base "html" :path "~/document/blog/Home_Page/Sports"))
	("Other"
	 ("~/document/blog/Home/Other" :defualt "index")
	 (:base "html" :path "~/document/blog/Home_Page/Other"))
	("Photo"
	 ("~/document/blog/Home/Photo" :default "index")
	 (:base "html" :path "~/document/blog/Home_Page/Photo"))))


(add-hook 'muse-mode-hook
          '(lambda ()
             (setq outline-regexp "\\*+ ")
             (outline-minor-mode)))

(defvar muse-colors-overlays nil)
;;能识别代码
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

(add-to-list 'muse-colors-tags '("src" t nil nil muse-colors-src-tag))

(unless (assoc "my-blosxom" muse-publishing-styles)
  (muse-derive-style "wiki-xhtml" "xhtml"     ;定义一个派生的style为wiki-xhtml
		     :header 'muse-xhtml-header
		     :footer 'muse-xhtml-footer
		     )
  (muse-derive-style "default-xhtml" "xhtml"  ;定义另一个派生的style
		     :header 'muse-blog-header
		     :footer 'muse-blog-footer
		     )
  (muse-derive-style "default-html" "html"
		     :header 'muse-xhtml-header
		     :footer 'muse-xhtml-footer
		     ))

;; 增加 publish-project 的 --all 选项
					;  命令行发布时候需要
(defun muse-project-batch-publish ()
  "Publish Muse files in batch mode."
  (let ((muse-batch-publishing-p t)
        force)
    (if (string= "--force" (or (car command-line-args-left) ""))
        (setq force t
              command-line-args-left (cdr command-line-args-left)))
    (if (string= "--all" (or (car command-line-args-left) ""))
        (setq command-line-args-left (nconc (cdr command-line-args-left)
                                            (mapcar 'car muse-project-alist))))
    (if command-line-args-left
        (dolist (project (delete-dups command-line-args-left))
          (message "Publishing project %s ..." project)
          (muse-project-publish project force))
      (message "No projects specified."))))

;; 自定义 footer.html
(setq muse-html-footer "~/.emacs.d/plugins/muse-html-footer.html")
(setq muse-html-header "~/.emacs.d/plugins/muse-html-header.html")

;; CSS类型
(setq muse-xhtml-style-sheet
      "<link rel=\"stylesheet\" type=\"text/css\" href=\"./stylesheets/style.css\" />")

(setq muse-image-regexp
      "\\.\\(eps\\|gif\\|bmp\\|jp\\(e?g\\)\\|p\\(bm\\|ng\\)\\|tiff\\|x\\([bp]m\\)\\)\\'")



;;=====================the muse-mode======================;
