;;smart 编译，自动查找makefile，并不是smartcompile插件
(defun smart-compile-is-root-dir(try-dir)
  (or
   ;; windows root dir for a driver or Unix root
   (string-match "\\`\\([a-zA-Z]:\\)?/$" try-dir)
   ;; tramp root-dir
   (and (featurep 'tramp)
        (string-match (concat tramp-file-name-regexp ".*:/$") try-dir))))

(defun smart-compile-throw-final-path(try-dir)
  (cond
   ;; tramp root-dir
   ((and (featurep 'tramp)
         (string-match tramp-file-name-regexp try-dir))
    (with-parsed-tramp-file-name try-dir foo
        foo-localname))
   (t try-dir)))

(defun smart-compile-find-make-dir( try-dir)
  "return a directory contain makefile. try-dir is absolute path."
  (if (smart-compile-is-root-dir try-dir)
      nil ;; return nil if failed to find such directory.
    (let ((candidate-make-file-name '("GNUmakefile" "makefile" "Makefile")))
      (or (catch 'break
            (mapc (lambda (f)
                    (if (file-readable-p (concat (file-name-as-directory try-dir) f))
                        (throw 'break (smart-compile-throw-final-path try-dir))))
                  candidate-make-file-name)
            nil)
          (smart-compile-find-make-dir
           (expand-file-name (concat (file-name-as-directory try-dir) "..")))))))

(defun wcy-tramp-compile (arg-cmd)
  "reimplement the remote compile."
  (interactive "scompile:")
  (with-parsed-tramp-file-name default-directory foo
    (let* ((key (format "/plink:%s@%s:" foo-user foo-host))
           (passwd (password-read "PASS:" key))
           (cmd (format "plink %s -l %s -pw %s \"(cd %s ; %s)\""
                         foo-host foo-user
                         passwd
                         (file-name-directory foo-localname)
                         arg-cmd)))
      (password-cache-add key passwd)
      (save-some-buffers nil nil)
      (compile-internal cmd "No more errors")
      ;; Set comint-file-name-prefix in the compilation buffer so
      ;; compilation-parse-errors will find referenced files by ange-ftp.
      (with-current-buffer compilation-last-buffer
        (set (make-local-variable 'comint-file-name-prefix)
             (format "/plink:%s@%s:" foo-user foo-host))))))

(defun smart-compile-test-tramp-compile ()
  (or (and (featurep 'tramp)
           (string-match tramp-file-name-regexp (buffer-file-name))
           (progn
             (if (not (featurep 'tramp-util)) (require 'tramp-util))
             'wcy-tramp-compile))
      'compile))

(defun smart-compile-get-local-file-name (file-name)
  (if (and
       (featurep 'tramp)
       (string-match tramp-file-name-regexp file-name))
      (with-parsed-tramp-file-name file-name foo
        foo-localname)
    file-name))

(defun smart-compile ()
  (interactive)
  (let* ((compile-func (smart-compile-test-tramp-compile))
         (dir (smart-compile-find-make-dir (expand-file-name "."))))
    (funcall compile-func
             (if dir
                 (concat "make -C " dir (if (eq compile-func 'tramp-compile) "&" ""))
               (concat
                (cond
                 ((eq major-mode 'c++-mode) "g++ -g -o ")
                 ((eq major-mode 'c-mode) "gcc -g -o "))
                (smart-compile-get-local-file-name (file-name-sans-extension
             (buffer-file-name)))
                " "
                (smart-compile-get-local-file-name (buffer-file-name)))))))
       


       
