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
;; バッファから mhc-guess-{time,date}: 日付、時間を集めて、
;; 予定の日付けを表わしていると思われる可能性の高い順に並べて
;; 返す。
;;
;; 以下のような GUESS-CANDIDATE のリストを返す
;;   ([mhc-date point-begin point-end score]..)
;;    
;; 日付推測の手順
;;
;; 1. 日付/時刻を表すキーワード見付けて、発見個所リストを作る。
;;
;;    (mhc-guess/gather-date-list mhc-guess-date-regexp-list)
;;    (mhc-guess/gather-time-list mhc-guess-time-regexp-list)
;;
;;    の 2つの関数で、
;;  
;;    ([found-date found-point-begin found-point-end nil] ...)
;;
;;    のような candidate-list を得る。
;;
;; 2. みつかった日付時刻に点数をつける。
;;
;;    (mhc-guess/score candidate-list mhc-guess-keyword-score-alist)
;;
;;    ([found-date found-point-begin found-point-end score] ...)
;;
;;      キーワードが引用行中にある、
;;      同一行に特定の文字列がある、
;;      ある範囲の前方/後方に特定の文字列がある。
;;
;;    のような条件と得点を表す mhc-guess-keyword-score-alist に基づいて
;;    採点をする。
;;
;; 3. 得点順に、sort して返す

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
    ("\\(\\([来今０-９0-9]+\\)[\n ]*月\\)[\n ]*の?[\n ]*\\([０-９0-9]+\\)"
     mhc-guess/make-date-from-mmdd 2 3)
    ("\\([０-９0-9]+\\) *[／/] *\\([０-９0-9]+\\)"
     mhc-guess/make-date-from-mmdd 1 2)
    throw
    ("\\(今度\\|[今来次]週\\|再来週\\)[\n ]*の?[\n ]*\\([月火水木金土日]\\)曜"
     mhc-guess/make-date-from-relative-week 1 2)
    throw
    ("\\([０-９0-9]+\\)[\n ]*日"
     mhc-guess/make-date-from-mmdd nil 1)
    ("\\([月火水木金土日]\\)\n?曜"
     mhc-guess/make-date-from-relative-week nil 1)
    ("\\(本日\\|今日\\|あす\\|あした\\|あさって\\|明日\\|明後日\\)"
     mhc-guess/make-date-from-relative-day 1)
    ))

(defvar mhc-guess-time-regexp-list
  '("\\([０-９0-9]+\\) *[：:時] *\\([０-９0-9]+\\|半\\)?"))

;; keyword to score-alist:
;;    each element consists of (regexp relative-boundary sameline? score)
(defvar mhc-guess-keyword-score-alist
  '(
    ;; positive factor
    ("^[\t ][\t ]+"                                 -200 t   +5)
    ("次回"                                         -200 nil +10)
    ("\\(日程\\|時間帯\\|日時\\|開始時間\\)"        -150 nil +5)
    ("\\(日程\\|時間帯\\|日時\\|開始時間\\)[:：]"   -150 t   +5)
    ("\\(から\\|〜\\|変更\\|延期\\|順延\\|開始\\)"   +80 nil +5)
    ;; negative factor
    ("\\(休み\\|除く\\|中止\\|までに\\)"             +80 t   -10)
    ("出欠"                                          -80 nil -5)
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
		 (buffer-substring (match-beginning 2) (match-end 2)) "半")
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
     ((string= mm-str "来")
      (setq mm (mhc-date-mm (mhc-date-mm++ now))))
     ((string= mm-str "今")
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
   ((or (string= rel-word "今日") (string= rel-word "本日"))
    now)
   ((or (string= rel-word "あす")
 	(string= rel-word "あした")
 	(string= rel-word "明日"))
    (mhc-date++ now))
   ((or (string= rel-word "あさって")
 	(string= rel-word "明後日"))
    (mhc-date+ now 2))))

(defun mhc-guess/make-date-from-relative-week (now rel-word week)
  (let ((data (match-data))
 	(ww (string-match week "日月火水木金土"))
 	(date (or now (mhc-date-now)))
	off)
    (setq off  (- ww (mhc-date-ww date)))
    (if (string= week "日") (setq off (+ 7 off)))
    (setq off
 	  (cond
 	   ((or (null rel-word)
 		(string= rel-word "今度")
 		(string= rel-word "次"))
 	    (if (<= off 0) (+ 7 off) off))
 	   ((string= rel-word "今週") off)
 	   ((string= rel-word "来週")
 	    (+ off 7))
 	   ((string= rel-word "再来週")
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
  '(("０" . "0") ("１" . "1") ("２" . "2") ("３" . "3") ("４" . "4")
    ("５" . "5") ("６" . "6") ("７" . "7") ("８" . "8") ("９" . "9")
    ("／" . "/") ("：" . ":")))

(defun mhc-guess/string-to-int (str)
  (let ((chr "") (ret "") (data (match-data))
	(z2h-alist 
	 '(("０" . "0") ("１" . "1") ("２" . "2") ("３" . "3") ("４" . "4")
	   ("５" . "5") ("６" . "6") ("７" . "7") ("８" . "8") ("９" . "9")
	   ("／" . "/") ("：" . ":"))))
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


