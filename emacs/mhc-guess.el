;;; mhc-guess.el -- Guess the important date from a Japanese mail article.

;; Author:  Yoshinari Nomura <nom@quickhack.net>
;;
;; Created: 1999/04/13
;; Revised: $Date: 2000/05/30 15:04:57 $
;;

;;;
;;; Commentary:
;;;

;;
;; gdate-guess-{time,date}: $BF|IU!";~4V$r?dB,$9$k(B:
;;
;;  ( (score (point-begin . point-end)  yy mm dd)
;;    (score (point-begin . point-end)  yy mm dd) ...)
;;
;;  ( (score (point-begin . point-end)  HH MM)
;;    (score (point-begin . point-end)  HH MM) ...)
;;
;;  (setq guess (car (gdate-guess-date)))
;;      => (score (point-begin . point-end)  yy mm dd)
;; 
;;     (car guess)       => score
;;     (cdr guess)       => ((point-begin . point-end)  yy mm dd)
;;     (car (cdr guess)) => (point-begin . point-end)
;;     (cdr (cdr guess)) => (yy mm dd)
;;
;; $BF|IU?dB,$N<j=g(B
;;
;; 1. $BF|IU(B/$B;~9o$rI=$9%-!<%o!<%I8+IU$1$F!"H/8+8D=j%j%9%H$r:n$k!#(B
;;
;;    (gdate-gather-date-list gdate-date-regex-list)
;;    (gdate-gather-time-list gdate-time-regex-list)
;;
;;  $B$N(B 2$B$D$N4X?t$G!"(B
;;  
;;   ( (found-point  found-yy found-mm found-dd) ...)
;;   ( (found-point  found-HH found-MM) ...)
;;
;; $B$N$h$&$J(B candidate-list $B$rF@$k!#(B
;; found-point = (found-point-begin . found-point-end)
;;
;; gdate-date-regex-list $B$OF|IU$rI=$9@55,I=8=$N(B list
;; gdate-time-regex-list $B$O;~4V$rI=$9@55,I=8=$N(B list
;;
;; 2. $B$_$D$+$C$?F|IU;~9o$KE@?t$r$D$1$k!#(B
;;
;; $BF@$i$l$?%j%9%H$N3FMWAG$KE@?t$rIU$1$k!#(B
;;
;; (gdate-score candidate-list  gdate-keyword-score-alist)
;;
;; ((score found-point found-yy found-mm found-dd) ...)
;; ((score found-point found-HH found-MM)          ...)
;;
;;   $B%-!<%o!<%I$,0zMQ9TCf$K$"$k!"(B
;;   $BF10l9T$KFCDj$NJ8;zNs$,$"$k!"(B
;;   $B$"$kHO0O$NA0J}(B/$B8eJ}$KFCDj$NJ8;zNs$,$"$k!#(B
;;
;; $B$N$h$&$J>r7o$HF@E@$rI=$9(B gdate-keyword-score-alist $B$K4p$E$$$F:NE@$r$9$k!#(B
;;
;; 3. $BF@E@=g$K!"(Bsort $B$7$FJV$9(B

;;;
;;; Code:
;;;

(require 'mhc-date)
(provide 'mhc-guess)

(defvar gdate-date-regex-list
  '(
    "\\([$B#0(B-$B#9(B0-9]+\\) *[$B!?(B/$B7n(B] *\\([$B#0(B-$B#9(B0-9]+\\)"
    ;; "\\([^$B#0(B-$B#9(B0-9$B!?(B/$B7n(B ]\\) *\\([$B#0(B-$B#9(B0-9]+\\) *$BF|(B"
    "\\([$B#0(B-$B#9(B0-9]+\\) *[$B!?(B/$B7n(B][$B!!(B ]*.*[^$B#0(B-$B#9(B0-9$B!?(B/$B7n(B ]+\\([$B#0(B-$B#9(B0-9]+\\) *$BF|(B"
    ))

;;(defvar gdate-date-regex-list
;;  '("\\([$B#0(B-$B#9(B0-9]+\\) *[$B!?(B/$B7n(B] *\\([$B#0(B-$B#9(B0-9]+\\)"
;;    "\\([^$B#0(B-$B#9(B0-9$B!?(B/$B7n(B ]\\) *\\([$B#0(B-$B#9(B0-9]+\\) *$BF|(B"
;;    "\\([$B#0(B-$B#9(B0-9]+\\) *[$B!?(B/$B7n(B][$B!!(B ]*[^$B#0(B-$B#9(B0-9$B!?(B/$B7n(B ]+\\([$B#0(B-$B#9(B0-9]+\\)"
;;    ))

(defvar gdate-time-regex-list
  '("\\([$B#0(B-$B#9(B0-9]+\\) *[$B!'(B:$B;~(B] *\\([$B#0(B-$B#9(B0-9]+\\|$BH>(B\\)?"))

;; gdate-keyword-score-alist:
;;    each element consists of (regex relative-boundary sameline? score)
;;

(defvar gdate-keyword-score-alist
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

(defvar gdate-z2h-alist
  '(("$B#0(B" . "0") ("$B#1(B" . "1") ("$B#2(B" . "2") ("$B#3(B" . "3") ("$B#4(B" . "4")
    ("$B#5(B" . "5") ("$B#6(B" . "6") ("$B#7(B" . "7") ("$B#8(B" . "8") ("$B#9(B" . "9")
    ("$B!?(B" . "/") ("$B!'(B" . ":")))

;;
;; public entry
;;

;;
;; guess date and time.
;;

;; returns nil or list of (score (ptr-begin . ptr-end) yy mm dd)
;;
(defun gdate-guess-date (&optional hint1)
  (let ((score-list (gdate-score 
		     (gdate-gather-date-list  gdate-date-regex-list)
		     gdate-keyword-score-alist
		     hint1)))
    (sort score-list (function (lambda (a b) (< (car b) (car a)))))))

; (defun gdate-guess-date ()
;   (let* ((score -1000)
; 	 (now   (ddate-now))
; 	 (ddate now)
; 	 (ptr   0)
; 	 (score-list
; 	  (gdate-score (gdate-gather-date-list gdate-date-regex-list)
; 		       gdate-keyword-score-alist))
; 	 entry-score entry-ptr entry-date entry)
;     (while score-list
;       (setq entry       (car score-list)
; 	    entry-score (car entry)
; 	    entry-ptr   (car (cdr entry))
;             entry-date  (cdr (cdr entry)))
;       (if (and (< score entry-score) (ddate<= now entry-date))
; 	  (setq score  entry-score
; 		ddate  entry-date
; 		ptr    entry-ptr))
;       (setq score-list (cdr score-list)))
;     (cons ptr ddate)))


;; returns nil or list of (score (ptr-begin . ptr-end) HH MM)
;;

(defun gdate-guess-time (&optional hint1)
  (let ((score-list (gdate-score 
		     (gdate-gather-time-list  gdate-time-regex-list)
		     gdate-keyword-score-alist
		     hint1)))
    (sort score-list (function (lambda (a b) (< (car b) (car a)))))))

; (defun gdate-guess-time ()
;   (let* ((score -1000)
; 	 (dtime nil)
; 	 (ptr   0)
; 	 (score-list
; 	  (gdate-score (gdate-gather-time-list gdate-time-regex-list)
; 		       gdate-keyword-score-alist))
; 	 entry-score entry-ptr entry-time entry)
;     (while score-list
;       (setq entry       (car score-list)
; 	    entry-score (car entry)
; 	    entry-ptr   (car (cdr entry))
;             entry-time  (cdr (cdr entry)))
;       (if (< score entry-score)
; 	  (setq score  entry-score
; 		dtime  entry-time
; 		ptr    entry-ptr))
;       (setq score-list (cdr score-list)))
;     (if dtime (cons ptr dtime) nil)))

;;
;; gather time
;;

;; returns (((ptr-begin . ptr-end) . dtime) ...)
;;
(defun gdate-gather-time-list (regexp-lst)
  (let ((ret nil))
    (while regexp-lst
      (setq ret (nconc ret (gdate-gather-time-list2 (car regexp-lst))))
      (setq regexp-lst (cdr regexp-lst)))
    ret))


;; returns (((ptr-begin . ptr-end) . dtime) ...)
;;
(defun gdate-gather-time-list2 (regexp)
  (let* (lst xHH xMM dtime)
    (save-excursion
      ;; skip Header
      (goto-char (point-min))
      (re-search-forward "^-*$" nil t)
      ;; search candities.
      (while (re-search-forward regexp nil t)
	(setq xHH (gdate-string-to-int
		   (buffer-substring (match-beginning 1) (match-end 1))))
	(if (< xHH 8) (setq xHH (+ xHH 12))) ;; 8 depends on my life style.
	(setq xMM 
	      (cond
	       ((not (match-beginning 2)) 
		0)
	       ((eq (buffer-substring (match-beginning 2) (match-end 2)) "$BH>(B")
		30)
	       (t
		(gdate-string-to-int
		 (buffer-substring (match-beginning 2) (match-end 2))))))
	(if (setq dtime (dtime-new xHH xMM t)) ;; noerror is t
	    (setq lst (cons (cons (cons (match-beginning 0) (match-end 0))
				  dtime) lst))))
      (nreverse lst))))

;;
;; gather date
;;

;; returns (((ptr-begin . ptr-end) . ddate) ...)
;;
(defun gdate-gather-date-list (regexp-lst)
  (let ((ret nil))
    (while regexp-lst
      (setq ret (nconc ret (gdate-gather-date-list2 (car regexp-lst))))
      (setq regexp-lst (cdr regexp-lst)))
    ret))

;; returns (((ptr-begin . ptr-end) . ddate) ...)
;;
(defun gdate-gather-date-list2 (regexp)
  (let* ((now (ddate-now))
	 (yy (ddate-yy now)) (mm (ddate-mm now)) (dd (ddate-dd now))
	 (year-offset 0)
	 lst xmm xdd ddate)
    (save-excursion
      ;; skip Header
      (goto-char (point-min))
      (re-search-forward "^-*$" nil t)
      ;; search candities.
      (while (re-search-forward regexp nil t)
	(setq xmm (gdate-string-to-int
		   (buffer-substring (match-beginning 1) (match-end 1)))
	      xdd (gdate-string-to-int
		   (buffer-substring (match-beginning 2) (match-end 2))))
	(if (= xmm 0) (setq xmm mm))
	(if (not (setq ddate (ddate-new yy xmm xdd t))) ;; noerror is t
	    ()
	  ;; if ddate is past, assume the next year.
	  ;; But, don't ddate-yy-inc immediately
	  ;; to do well in case 2/29.
	  (if (ddate< ddate (ddate-now))
	      (setq year-offset (1+ year-offset)))
	  ;; if ddate is far future, assume the last year.
	  (if (< 300 (+ (ddate- ddate (ddate-now)) (* year-offset 365)))
	      (setq year-offset (1- year-offset)))
	  (setq ddate (ddate-yy-inc ddate year-offset))
	  (setq lst (cons (cons 
			   (cons (match-beginning 0) (match-end 0))
			   ddate) lst))))
      (nreverse lst))))

;;
;; scoring
;;

;; returns ((score ptr . ddate-or-dtime) ...)

(defun gdate-score (candidate-lst score-alist &optional hint1)
  (let ((ret nil) s-alist score ptr ddate regex boundary sameline s)
    (while candidate-lst
      (setq s-alist score-alist
	    score   0
	    ptr     (car (car candidate-lst))
	    ddate   (cdr (car candidate-lst)))
      (while s-alist
	(setq regex    (nth 0 (car s-alist))
	      boundary (nth 1 (car s-alist))
	      sameline (nth 2 (car s-alist))
	      s        (nth 3 (car s-alist)))
	(if (gdate-search-in-boundary regex (car ptr) boundary sameline)
	    (setq score (+ score s)))
	(if (and hint1
		 (<  hint1 (car ptr))
		 (<  (- (car ptr) hint1) 40))
	    (setq score (+ score 10)))
	(setq s-alist (cdr s-alist)))
      (setq ret (cons (cons score (cons ptr ddate)) ret))
      (setq candidate-lst (cdr candidate-lst)))
    (nreverse ret)))

(defun gdate-search-in-boundary (regex ptr rel-boundary sameline)
  (let ((pmin (+ ptr rel-boundary)) (pmax (+ ptr rel-boundary)))
    (save-excursion
      (goto-char ptr)
      (if sameline
	  (setq pmax (min pmax (save-excursion (end-of-line)       (point)))
		pmin (max pmin (save-excursion (beginning-of-line) (point)))))
      (if (< 0 rel-boundary)
	  (search-forward-regexp regex pmax t)
	(search-backward-regexp regex pmin t)))))

;;
;; string-to-int with code conversion.
;;

(defun gdate-string-to-int (str)
  (let ((chr "") (ret "") (data (match-data)))
    (while (string-match "^." str)
      (setq chr (substring str (match-beginning 0) (match-end 0)))
      (setq ret (concat ret (or (cdr (assoc chr gdate-z2h-alist)) chr)))
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
