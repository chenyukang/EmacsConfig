;;/**
;* @file   beautify-line.el
;* @author moorekang <moorekang@gmail.com>
;* @date   Sat Jul 17 13:38:17 2010
;* 
;* @brief  C/C++编程中行排版 在操作符两边加上适当的空白
;* 
;* 
;*/

(setq beautifly-line-list
      '(
	("+" . " + ")
        ("-" . " - ")
	("=" . " = ")
	("*" . " * ")
	("/" . " / ")
	("%" . " % ")
	("<" . " < ")
	(">" . " > ")
	("^" . " ^ ")
	("," . ", ")
	(";" . "; ")
	("|" . " | ")
	("&" . " & ")
	(":" . " : ")
	("?" . " ? ")
	("+=" . " += ")
	("*=" . " *= ")
	("/=" . " /= ")
	("%=" . " %= ")
	("^=" . " ^= ")
	("||" . " || ")
	("&&" . " && ")
	("==" . " == ")))

(defun beautifly-line() (interactive)
  (setq list beautifly-line-list)
  (while (not (equal list '()))
    (let ((now (car list)))
      (replace-char (car now) (cdr now))
      (setq list (cdr list))))
  (move-end-of-line 1))

(defun replace-char (src dest)
  (interactive)
  (move-end-of-line 1)
  (setq end-pos (point))
  (move-beginning-of-line 1)
  (while (search-forward src end-pos t)
    (if (and (not (equal (char-after (point)) 32))  ;;" "
	     (not (equal (char-after (point)) 62))  ;;">"
	     (not (equal (char-after (point)) 61))  ;;"+=" "-="...
	     (not (equal (char-after (point)) 41))      ;; ) 
	     (not (equal (char-after (- (point) 2)) 61))
	     (not (equal (char-after (- (point) 2)) 40)) ;; (
	     (not (equal (char-after (- (point) 2)) 32)) ;;" "
	     (not (equal (char-after (- (point) 1))
			 (char-after (- (point) 2)))) ;++ --
	     (not (equal (char-after (- (point) 1))   ;++ --
			 (char-after (point))))
	     (not (equal (char-after (point)) 59)) ;   ;
	     (not (equal (point) end-pos)) ;not the end <stdio.h>
	     (not (test-valid (char-after (- (point) 2))))
	     (not (test-in-quote (point))))
	((lambda()
	   (replace-match dest)
	   (move-end-of-line 1)
	   (setq end-pos (point))
	   (move-beginning-of-line 1)))
      (if (and 
	   (or (and (equal (char-after (- (point) 1)) 61)
		    (or 
		     (test-valid (char-after (-  (point) 2)))
		     (equal (char-after (- (point) 2)) 61) ;==
		     (equal (char-after (- (point) 2)) 33))) ;!= += -=
	       (and (equal (char-after (- (point) 1)) 124)
		    (equal (char-after (- (point) 2)) 124))
	       (and (equal (char-after (- (point) 1)) 38)	;;||
		    (equal (char-after (- (point) 2)) 38)))	;;&&
	   ;(not (equal (char-after (- (point) 3)) 32))
	   (not (equal (char-after (point)) 32)))
	  ((lambda()
	     (setq old-pos (point))
	     (goto-char (- (point) 2))
	     (insert " ")
	     (goto-char (+ (point ) 2))
	     (insert " ")
	     (move-end-of-line 1)
	     (setq end-pos (point))
	     (goto-char old-pos))))))
  (move-end-of-line 1))

;;从point-pos位置开始 到这一行的尾部，检测是否有"，即检测是否在" "内部
(defun test-in-quote (point-pos)
  (interactive)
  (move-end-of-line 1)
  (setq end-pos (point))
  (goto-char point-pos)
  (setq ret-value nil)
  (if (search-forward "\"" end-pos t)
      (setq ret-value t)
    )
  (goto-char point-pos)
  ret-value)


(defun test-valid(dest)
  (interactive)
  (if(or (equal dest 43)
	 (equal dest 45)
	 (equal dest 42)
	 (equal dest 47)
	 (equal dest 37)
	 (equal dest 62)  ;;
	 (equal dest 33)  ;;!=
	 (equal dest 94)  ;;^=
	 (equal dest 60)) ;;<
      t
    nil))

(defun print-pos-char ()
  (interactive)
  (setq value (char-after (point)))
  (print value)) 

(defun my-new-line-and-beautyfly ()
  (interactive)
  (setq pos (point))
  (move-beginning-of-line 1)
  (setq beg (point))
  (move-end-of-line 1)
  (setq end (point))
  (goto-char pos)
  (if (equal pos end)
      ((lambda()
       (beautifly-line)
       (newline-and-indent)))
    (newline-and-indent)))



