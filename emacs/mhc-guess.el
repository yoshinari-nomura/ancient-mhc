;;; mhc-guess.el -- Guess the important date from a Japanese mail article.

;; Author:  Yoshinari Nomura <nom@quickhack.net>
;;
;; Created: 1999/04/13
;; Revised: $Date: 2000/06/23 08:29:17 $
;;

;;;
;;; Commentary:
;;;

;;
;; $B%P%C%U%!$+$i(B mhc-guess-{time,date}: $BF|IU!";~4V$r=8$a$F!"(B
;; $BM=Dj$NF|IU$1$rI=$o$7$F$$$k$H;W$o$l$k2DG=@-$N9b$$=g$KJB$Y$F(B
;; $BJV$9!#(B
;;
;; $B0J2<$N$h$&$J(B GUESS-CANDIDATE $B$N%j%9%H$rJV$9(B
;;   ([mhc-date point-begin point-end score]..)
;;    
;; $BF|IU?dB,$N<j=g(B
;;
;; 1. $BF|IU(B/$B;~9o$rI=$9%-!<%o!<%I8+IU$1$F!"H/8+8D=j%j%9%H$r:n$k!#(B
;;
;;    (mhc-guess/gather-date-list mhc-guess-date-regexp-list)
;;    (mhc-guess/gather-time-list mhc-guess-time-regexp-list)
;;
;;    $B$N(B 2$B$D$N4X?t$G!"(B
;;  
;;    ([found-date found-point-begin found-point-end nil] ...)
;;
;;    $B$N$h$&$J(B candidate-list $B$rF@$k!#(B
;;
;; 2. $B$_$D$+$C$?F|IU;~9o$KE@?t$r$D$1$k!#(B
;;
;;    (mhc-guess/score candidate-list mhc-guess-keyword-score-alist)
;;
;;    ([found-date found-point-begin found-point-end score] ...)
;;
;;      $B%-!<%o!<%I$,0zMQ9TCf$K$"$k!"(B
;;      $BF10l9T$KFCDj$NJ8;zNs$,$"$k!"(B
;;      $B$"$kHO0O$NA0J}(B/$B8eJ}$KFCDj$NJ8;zNs$,$"$k!#(B
;;
;;    $B$N$h$&$J>r7o$HF@E@$rI=$9(B mhc-guess-keyword-score-alist $B$K4p$E$$$F(B
;;    $B:NE@$r$9$k!#(B
;;
;; 3. $BF@E@=g$K!"(Bsort $B$7$FJV$9(B

;;;
;;; Code:
;;;

(require 'mhc-date)
(provide 'mhc-guess)

;;
;; regexp for get date strings.
;;

(defvar mhc-guess-date-regexp-list
  '(
    ("\\(\\([$BMh:##0(B-$B#9(B0-9]+\\)[\n ]*$B7n(B\\)[\n ]*$B$N(B?[\n ]*\\([$B#0(B-$B#9(B0-9]+\\)"
     mhc-guess/make-date-from-mmdd 2 3)
    ("\\([$B#0(B-$B#9(B0-9]+\\) *[$B!?(B/] *\\([$B#0(B-$B#9(B0-9]+\\)"
     mhc-guess/make-date-from-mmdd 1 2)
    throw
    ("\\($B:#EY(B\\|[$B:#Mh<!(B]$B=5(B\\|$B:FMh=5(B\\)[\n ]*$B$N(B?[\n ]*\\([$B7n2P?eLZ6bEZF|(B]\\)$BMK(B"
     mhc-guess/make-date-from-relative-week 1 2)
    throw
    ("\\([$B#0(B-$B#9(B0-9]+\\)[\n ]*$BF|(B"
     mhc-guess/make-date-from-mmdd nil 1)
    ("\\([$B7n2P?eLZ6bEZF|(B]\\)\n?$BMK(B"
     mhc-guess/make-date-from-relative-week nil 1)
    ("\\($BK\F|(B\\|$B:#F|(B\\|$B$"$9(B\\|$B$"$7$?(B\\|$B$"$5$C$F(B\\|$BL@F|(B\\|$BL@8eF|(B\\)"
     mhc-guess/make-date-from-relative-day 1)
    ))

(defvar mhc-guess-time-regexp-list
  '("\\([$B#0(B-$B#9(B0-9]+\\) *[$B!'(B:$B;~(B] *\\([$B#0(B-$B#9(B0-9]+\\|$BH>(B\\)?"))

;; keyword to score-alist:
;;    each element consists of (regexp relative-boundary sameline? score)
(defvar mhc-guess-keyword-score-alist
  '(
    ;; positive factor
    ("^[\t ][\t ]+"                                 -200 t   +5)
    ("$B<!2s(B"                                         -200 nil +10)
    ("\\($BF|Dx(B\\|$B;~4VBS(B\\|$BF|;~(B\\|$B3+;O;~4V(B\\)"        -150 nil +5)
    ("\\($BF|Dx(B\\|$B;~4VBS(B\\|$BF|;~(B\\|$B3+;O;~4V(B\\)[:$B!'(B]"   -150 t   +5)
    ("\\($B$+$i(B\\|$B!A(B\\|$BJQ99(B\\|$B1d4|(B\\|$B=g1d(B\\|$B3+;O(B\\)"   +80 nil +5)
    ;; negative factor
    ("\\($B5Y$_(B\\|$B=|$/(B\\|$BCf;_(B\\|$B$^$G$K(B\\)"             +80 t   -10)
    ("$B=P7g(B"                                          -80 nil -5)
    ("^\\(On\\|At\\|Date:\\) "                      -200 t   -20)
    ("\\(Jan\\|Feb\\|Mar\\|Apr\\|May\\|Jun\\)"      -200 t   -20)
    ("\\(Jul\\|Aug\\|Sep\\|Oct\\|Nov\\|Dec\\)"      -200 t   -20)
    ("^\\([ a-zA-Z]*>\\)+ *"                        -200 t   -15)
    ))

;;
;; manipulate guess-candidate structure.
;;

(defmacro mhc-guess-get-date          (obj)   `(aref ,obj 0))
(defmacro mhc-guess-get-time          (obj)   `(aref ,obj 0))
(defmacro mhc-guess-get-date-or-time  (obj)   `(aref ,obj 0))
(defmacro mhc-guess-get-begin         (obj)   `(aref ,obj 1))
(defmacro mhc-guess-get-end           (obj)   `(aref ,obj 2))
(defmacro mhc-guess-get-score         (obj)   `(aref ,obj 3))

(defmacro mhc-guess-set-date  (obj val) `(aset ,obj 0 ,val))
(defmacro mhc-guess-set-time  (obj val) `(aset ,obj 0 ,val))
(defmacro mhc-guess-set-begin (obj val) `(aset ,obj 1 ,val))
(defmacro mhc-guess-set-end   (obj val) `(aset ,obj 2 ,val))
(defmacro mhc-guess-set-score (obj val) `(aset ,obj 3 ,val))

(defun mhc-guess/new (&optional date-or-time begin end score)
  (vector date-or-time begin end score))

;;
;; public entry
;;

;; returns nil or list of guess-candidate.
;;
(defun mhc-guess-date (&optional hint1)
  (let ((score-list (mhc-guess/score
		     (mhc-guess/gather-date-list  mhc-guess-date-regexp-list)
		     mhc-guess-keyword-score-alist
		     hint1)))
    (sort score-list (function (lambda (a b) (< (mhc-guess-get-score b)
						(mhc-guess-get-score a)))))))

;; returns nil or list of guess-candidate.
;;

(defun mhc-guess-time (&optional hint1)
  (let ((score-list (mhc-guess/score
		     (mhc-guess/gather-time-list mhc-guess-time-regexp-list)
		     mhc-guess-keyword-score-alist
		     hint1)))
    (sort score-list (function (lambda (a b) (< (mhc-guess-get-score b)
						(mhc-guess-get-score a)))))))

;;
;; gather time
;;

;; return a list of guess-candidate
;;
(defun mhc-guess/gather-time-list (regexp-lst)
  (let ((ret nil))
    (while regexp-lst
      (setq ret (nconc ret (mhc-guess/gather-time-list2 (car regexp-lst))))
      (setq regexp-lst (cdr regexp-lst)))
    ret))

;; return a list of guess-candidate
(defun mhc-guess/gather-time-list2 (regexp)
  (let* (lst xHH xMM time)
    (save-excursion
      ;; skip Header
      (goto-char (point-min))
      (re-search-forward "^-*$" nil t)
      ;; search candities.
      (while (re-search-forward regexp nil t)
	(setq xHH (mhc-guess/string-to-int
		   (buffer-substring (match-beginning 1) (match-end 1))))
	(if (< xHH 8) (setq xHH (+ xHH 12))) ;; 8 depends on my life style.
	(setq xMM 
	      (cond
	       ((not (match-beginning 2))
		0)
	       ((string= 
		 (buffer-substring (match-beginning 2) (match-end 2)) "$BH>(B")
		30)
	       (t
		(mhc-guess/string-to-int
		 (buffer-substring (match-beginning 2) (match-end 2))))))
	(if (setq time (mhc-time-new xHH xMM t)) ;; noerror is t
	    (setq lst
		  (cons 
		   (mhc-guess/new time (match-beginning 0) (match-end 0) nil)
		   lst)))))
    (nreverse lst)))

;;
;; gather date
;;

(defun mhc-guess/gather-date-list (regexp-lst)
  (let ((ret nil)
 	(now (or (mhc-date-new-from-string3 (mhc-header-get-value "Date:"))
 		 (mhc-date-now)))
 	date-list)
    (while regexp-lst
      (cond
       ((listp (car regexp-lst))
 	(if (setq date-list
 		  (mhc-guess/gather-date-list2
 		   (car (car regexp-lst))       ;; regexp
 		   now                          ;; current date
 		   (car (cdr (car regexp-lst))) ;; convfunc
 		   (cdr (cdr (car regexp-lst))) ;; posision list
 		   ))
 	    (setq ret (nconc ret date-list))))
       ((and (string= "throw" (symbol-name (car regexp-lst))) ret)
 	(setq regexp-lst nil)))
      (setq regexp-lst (cdr regexp-lst)))
    ret))

(defun mhc-guess/gather-date-list2 (regexp now convfunc pos-list)
  (let* (lst date param-list p)
    (save-excursion
      ;; skip Header
      (goto-char (point-min))
      (re-search-forward "^-*$" nil t)
      ;; search candities.
      (while (re-search-forward regexp nil t)
 	(setq  p pos-list
 	       param-list nil)
 	(while p
 	  (setq param-list 
 		(cons
 		 (if (and (car p) (match-beginning (car p)))
 		     (buffer-substring (match-beginning (car p))
 				       (match-end       (car p)))
 		   nil)
 		 param-list))
 	  (setq p (cdr p)))
 	(if (setq date (apply 'funcall convfunc now (nreverse param-list)))
 	    (setq lst 
		  (cons 
		   (mhc-guess/new date(match-beginning 0) (match-end 0) nil)
		   lst)))))
    (nreverse lst)))

;;
;; make date from string.
;;

(defun mhc-guess/make-date-from-mmdd (now mm-str dd-str)
  (let ((data (match-data))
 	(mm (if mm-str (mhc-guess/string-to-int mm-str) 0))
 	(dd (if dd-str (mhc-guess/string-to-int dd-str) 0))
 	(year-offset 0)
 	date)
    (cond 
     ((string= mm-str "$BMh(B")
      (setq mm (mhc-date-mm (mhc-date-mm++ now))))
     ((string= mm-str "$B:#(B")
      (setq mm (mhc-date-mm now)))
     ((= mm 0)
      (setq mm (mhc-date-mm now))))
    (if (not 
	 (setq date
	       (mhc-date-new (mhc-date-yy now) mm dd t))) ;; noerror is t
 	()
      ;; if date is past, assume the next year.
      (if (mhc-date< date now)
 	  (setq year-offset (1+ year-offset)))
      ;; if date is far future, assume the last year.
      (if (< 300 (+ (mhc-date- date now) (* year-offset 365)))
 	  (setq year-offset (1- year-offset)))
      (setq date (mhc-date-yy+ date year-offset)))
    (store-match-data data)
    date))

(defun mhc-guess/make-date-from-relative-day (now rel-word)
  (cond 
   ((null rel-word)
    nil)
   ((or (string= rel-word "$B:#F|(B") (string= rel-word "$BK\F|(B"))
    now)
   ((or (string= rel-word "$B$"$9(B")
 	(string= rel-word "$B$"$7$?(B")
 	(string= rel-word "$BL@F|(B"))
    (mhc-date++ now))
   ((or (string= rel-word "$B$"$5$C$F(B")
 	(string= rel-word "$BL@8eF|(B"))
    (mhc-date+ now 2))))

(defun mhc-guess/make-date-from-relative-week (now rel-word week)
  (let ((data (match-data))
 	(ww (string-match week "$BF|7n2P?eLZ6bEZ(B"))
 	(date (or now (mhc-date-now)))
	off)
    (setq off  (- ww (mhc-date-ww date)))
    (if (string= week "$BF|(B") (setq off (+ 7 off)))
    (setq off
 	  (cond
 	   ((or (null rel-word)
 		(string= rel-word "$B:#EY(B")
 		(string= rel-word "$B<!(B"))
 	    (if (<= off 0) (+ 7 off) off))
 	   ((string= rel-word "$B:#=5(B") off)
 	   ((string= rel-word "$BMh=5(B")
 	    (+ off 7))
 	   ((string= rel-word "$B:FMh=5(B")
 	    (+ off 14))))
    (store-match-data data)
    (mhc-date+ date off)
    ))

;;
;; scoring
;;

(defun mhc-guess/score (candidate-lst score-alist &optional hint1)
  (let ((clist candidate-lst)
	total-score candidate regexp boundary sameline score slist)
    (while clist
      (setq candidate   (car clist)
	    slist       score-alist
	    total-score 0)
      (while slist
	(setq regexp   (nth 0 (car slist))
	      boundary (nth 1 (car slist))
	      sameline (nth 2 (car slist))
	      score    (nth 3 (car slist)))
	(if (mhc-guess/search-in-boundary
	     regexp
	     (mhc-guess-get-begin candidate)
	     boundary 
	     sameline)
	    (setq total-score (+ total-score score)))
	(if (and hint1
		 (<  hint1 (mhc-guess-get-begin candidate))
		 (<  (- (mhc-guess-get-begin candidate) hint1) 40))
	    (setq total-score (+ total-score 10)))
	(setq slist (cdr slist)))
      (mhc-guess-set-score candidate total-score)
      (setq clist (cdr clist)))
    candidate-lst))

(defun mhc-guess/search-in-boundary (regexp ptr rel-boundary sameline)
  (let ((pmin (+ ptr rel-boundary)) (pmax (+ ptr rel-boundary)))
    (save-excursion
      (goto-char ptr)
      (if sameline
	  (setq pmax (min pmax (save-excursion (end-of-line)       (point)))
		pmin (max pmin (save-excursion (beginning-of-line) (point)))))
      (if (< 0 rel-boundary)
	  (search-forward-regexp regexp pmax t)
	(search-backward-regexp regexp pmin t)))))

;;
;; string-to-int with code conversion.
;;

(defconst mhc-guess/zenkaku-hankaku-alist
  '(("$B#0(B" . "0") ("$B#1(B" . "1") ("$B#2(B" . "2") ("$B#3(B" . "3") ("$B#4(B" . "4")
    ("$B#5(B" . "5") ("$B#6(B" . "6") ("$B#7(B" . "7") ("$B#8(B" . "8") ("$B#9(B" . "9")
    ("$B!?(B" . "/") ("$B!'(B" . ":")))

(defun mhc-guess/string-to-int (str)
  (let ((chr "") (ret "") (data (match-data))
	(z2h-alist 
	 '(("$B#0(B" . "0") ("$B#1(B" . "1") ("$B#2(B" . "2") ("$B#3(B" . "3") ("$B#4(B" . "4")
	   ("$B#5(B" . "5") ("$B#6(B" . "6") ("$B#7(B" . "7") ("$B#8(B" . "8") ("$B#9(B" . "9")
	   ("$B!?(B" . "/") ("$B!'(B" . ":"))))
    (while (string-match "^." str)
      (setq chr (substring str (match-beginning 0) (match-end 0)))
      (setq ret (concat ret (or (cdr (assoc chr z2h-alist)) chr)))
      (setq str (substring str (match-end 0))))
    (store-match-data data)
    (string-to-int ret)))


;;; Copyright Notice:

;; Copyright (C) 1999, 2000 Yoshinari Nomura. All rights reserved.
;; Copyright (C) 2000 MHC developing team. All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. Neither the name of the team nor the names of its contributors
;;    may be used to endorse or promote products derived from this software
;;    without specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE TEAM AND CONTRIBUTORS ``AS IS''
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL
;; THE TEAM OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
;; INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
;; OF THE POSSIBILITY OF SUCH DAMAGE.

;;; mhc-guess.el ends here


