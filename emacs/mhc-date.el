;;; mhc-date.el -- Digit style Date Calculation Lib.

;; Author:  Yoshinari Nomura <nom@mew.org>
;;
;; Created: 1999/04/07
;; Revised: 2000/05/09 09:14:20

;;;
;;; Commentary:
;;;

;; 
;; ddate is very simple. It expresses a date by a list of integer.
;; for example:
;;
;; 1999-04-30 corresponds to  (1999 4 30)
;;
;; ddate is also simple. It expresses a time by a list of integer.
;; for example:
;;
;; 12:30 (12 30)

;;;
;;; Code:
;;;

(defconst ddate-regex
  "^\\([0-9][0-9][0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)$")

(defconst dtime-regex
  "^\\([0-9][0-9]\\):\\([0-9][0-9]\\)$")

(defconst ddate-max-const '(2037 12 31))
(defconst ddate-min-const '(1971  1  1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dtime

(defsubst dtime-now (&optional need-sec time)
  (let ((now (current-time-string time)) (match (match-data))
	hour min sec)
    ;; current-time-string is "Thu Jun 30 01:58:16 1994"
    (string-match "\\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\)" now)
    (setq 
     hour (string-to-int (substring now (match-beginning 1) (match-end 1)))
     min  (string-to-int (substring now (match-beginning 2) (match-end 2)))
     sec  (string-to-int (substring now (match-beginning 3) (match-end 3))))
    (store-match-data match)
    (if need-sec
	(list  hour min sec)
      (list  hour min))))

(defsubst dtime-new (HH &optional MM noerror)
  (if (stringp HH)
      (dtime-new-from-string HH noerror)
    (dtime-new-from-digit HH MM noerror)))

(defsubst dtime-new-from-string (str &optional noerror regex)
  (let (HH MM (match (match-data)))
    (if (string-match (or regex dtime-regex) str)
	(setq HH (ddate-substring-to-int str 1)
	      MM (ddate-substring-to-int str 2)))
    (store-match-data match)
    (if HH
	(dtime-new-from-digit HH MM noerror)
      (if noerror nil (error "Time format error (%s)" str)))))

(defsubst dtime-new-from-digit (HH MM &optional noerror)
  (let ((dtime (list HH MM)))
    (if (dtime-parse dtime)
	dtime
      (if noerror nil (error "Time format error (%s %s)" HH MM)))))

(defsubst dtime-parse (dtime)
  (let (HH MM)
    (if (not (and dtime (listp dtime)))
	nil
      (setq HH (dtime-HH dtime) MM (dtime-MM dtime))
      (and (integerp HH) (>= HH 0) (<= HH 99)
	   (integerp MM) (>= MM 0) (<= MM 59)
	   (format "%02d:%02d" HH MM)))))

(defmacro dtime-HH (dtime) (` (nth 0 (, dtime))))
(defmacro dtime-MM (dtime) (` (nth 1 (, dtime))))

(defsubst dtime-max (t1 t2)
  (if (dtime< t1 t2) t2 t1))

(defsubst dtime-min (t1 t2)
  (if (dtime< t1 t2) t1 t2))

(defmacro dtime< (t1 t2)
  (` (string<
      (apply (quote format) "%02d%02d" (or (, t1) (quote (-1 -1))))
      (apply (quote format) "%02d%02d" (or (, t2) (quote (-1 -1)))))))

(fset 'dtime= (symbol-function 'equal))

(defsubst dtime<= (t1 t2)
  (or
   (dtime< t1 t2)
   (dtime= t1 t2)))

(defsubst dtime-to-s (dtime &optional need-sec)
  (if need-sec
      (apply 'format "%02d:%02d:%02d" dtime)
    (apply 'format "%02d:%02d" dtime)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ddate

;;
;; create new object, access method to each element.
;;
(defsubst ddate-new (yy &optional mm dd noerror)
  (if (stringp yy)
      (ddate-new-from-string yy noerror)
    (ddate-new-from-digit yy mm dd noerror)))

(defsubst ddate-new-from-string (str &optional noerror regex)
  (let (yy mm dd (match (match-data)))
    (if (string-match (or regex ddate-regex) str)
	(setq yy (ddate-substring-to-int str 1)
	      mm (ddate-substring-to-int str 2)
	      dd (ddate-substring-to-int str 3)))
    (store-match-data match)
    (if yy
	(ddate-new-from-digit yy mm dd noerror)
      (if  noerror nil (error "Date format error (%s)" str)))))

(defsubst ddate-new-from-string2 (str &optional base-date noerror)
  (let* ((now (or base-date (ddate-now)))
	 (yy (ddate-yy now)) (mm (ddate-mm now)) ret)
    (setq ret
	  (cond
	   ((string-match "^\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\)$" str)
	    (ddate-new (ddate-substring-to-int str 1)
		       (ddate-substring-to-int str 2)
		       (ddate-substring-to-int str 3) noerror))
	   ((string-match "^\\([0-9]+\\)/\\([0-9]+\\)$" str)
	    (ddate-new yy
		       (ddate-substring-to-int str 1)
		       (ddate-substring-to-int str 2) noerror))
	   ((string-match "^\\([0-9]+\\)$" str)
	    (ddate-new yy
		       mm
		       (ddate-substring-to-int str 1) noerror))))
    ;; if ret is past, assume the next year.
    ;; (if (and ret (ddate< ret now))
    ;; (ddate-yy-inc ret)
    ret))
  
(defsubst ddate-new-from-digit (yy mm dd &optional noerror)
  (let ((ddate (list yy mm dd)))
    (if (ddate-parse ddate)
	ddate
      (if noerror
	  nil
	(error "Date format error (%s %s %s)" yy mm dd)))))


;; emacs 19.2x doesn't have format-time-string
;;
;;(defun ddate-now ()
;;  (let (yy mm dd HH MM (now (current-time)))
;;    (setq 
;;     yy (string-to-int (format-time-string "%Y" now))
;;     mm (string-to-int (format-time-string "%m" now))
;;     dd (string-to-int (format-time-string "%d" now))
;;     HH (string-to-int (format-time-string "%H" now))
;;     MM (string-to-int (format-time-string "%M" now)))
;;    (list yy mm dd HH MM )))
;;

(defsubst ddate-now (&optional date)
  (let ((now (current-time-string date)) (match (match-data))
	week mon day hour min year)
    ;; current-time-string is "Thu Jun 30 01:58:16 1994"
    (string-match (concat 
		   "^\\([^ ]+\\) +\\([^ ]+\\) +\\([^ ]+\\) +"
		   "\\([^: ]+\\):\\([^: ]+\\):[^ ]+ +"
		   "\\([^ ]+\\)$")
		  (current-time-string))
    (setq 
     week (substring now (match-beginning 1) (match-end 1))
     mon  (substring now (match-beginning 2) (match-end 2))
     day  (substring now (match-beginning 3) (match-end 3))
     hour (substring now (match-beginning 4) (match-end 4))
     min  (substring now (match-beginning 5) (match-end 5))
     year (substring now (match-beginning 6) (match-end 6))
     week (/ (string-match week "SunMonTueWedThuFriSat") 3)
     mon  (/ (string-match mon "   JanFebMarAprMayJunJulAugSepOctNovDec") 3)
     year (string-to-int year))
    (store-match-data match)
    (list  year mon (string-to-int day))))

(defmacro ddate-yy (ddate) (` (nth 0 (, ddate))))
(defmacro ddate-mm (ddate) (` (nth 1 (, ddate))))
(defmacro ddate-dd (ddate) (` (nth 2 (, ddate))))
(defsubst ddate-ww (ddate)
  ;;  (0:Sun, 2:Mon, ... , 6:Sat)
  (let ((xx (1- (ddate-yy ddate))))
    (% (+ (ddate-day-of-yy ddate)
	  (* xx  365)
	  (/ xx    4)
	  (/ xx -100)
	  (/ xx  400))
       7)))

(defmacro ddate-oo (ddate) (` (/ (1- (ddate-dd (, ddate))) 7)))

;;
;; to-string
;;

;; for X-SC-*


(defsubst ddate-to-s (ddate)
  (apply 'format "%04d%02d%02d" ddate))

(defmacro ddate-yymm-s (ddate)
  (` (apply (quote format) "%04d%02d" (, ddate))))

(defmacro ddate-yy-s (date)
  (format "%04d" (ddate-yy ,date)))

(defsubst ddate-mm-s (date)
  (let ((from (* 3 (1- (ddate-mm date)))))
    (substring "JanFebMarAprMayJunJulAugSepOctNovDec"
	       from (+ 3 from))))

(defmacro ddate-dd-s (date)
  (` (format "%02d" (ddate-dd (, date)))))

(defsubst ddate-ww-s (date)
  (let ((from (* 3 (ddate-ww date))))
    (substring "SunMonTueWedThuFriSat"
	       from (+ 3 from))))

(defsubst ddate-oo-s (date)
  (let ((from (* 3 (ddate-oo date))))
    (substring "1st2nd3rd4th5th"
	       from (+ 3 from))))

;;
;; expression using digits and separators
;; ex. 1999-10-12, 1999/10/12
;;

(defsubst ddate-to-s1 (ddate &optional d)
  (format "%04d%s%02d%s%02d"
	  (ddate-yy ddate)
	  (or d "")
	  (ddate-mm ddate)
	  (or d "")
	  (ddate-dd ddate)))

(fset 'ddate-yy-s1 (symbol-function 'ddate-yy-s))

(defmacro ddate-mm-s1 (ddate) (` (format "%02d" (ddate-mm (, ddate)))))

(defmacro ddate-dd-s1 (ddate) (` (format "%02d" (ddate-dd (, ddate)))))

(defsubst ddate-yymm-s1 (ddate &optional d)
  (format "%04d%s%02d"
	  (ddate-yy ddate) (or d "") (ddate-mm ddate)))

(defsubst ddate-mmdd-s1 (ddate &optional d)
  (format "%02d%s%02d" (ddate-mm ddate) (or d "") (ddate-dd ddate)))


;;
;; expression using digits and separators
;; ex. April 1999
;;

(defsubst ddate-yymm-sj (ddate)
  (format "%s %04d" (ddate-mm-sj ddate) (ddate-yy ddate)))

(defsubst ddate-mm-sj (ddate)
  (aref 
   '["January" "February" "March"     "April"   "May"      "June"
     "July"    "August"   "September" "October" "November" "December"]
   (1- (ddate-mm ddate))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compare, increment, decrement

(defmacro ddate-sort (dlist) (` (sort (, dlist) (function ddate<))))

(defsubst ddate-max (d1 d2)
  (if (ddate< d1 d2) d2 d1))

(defsubst ddate-min (d1 d2)
  (if (ddate< d1 d2) d1 d2))

(fset 'ddate= (symbol-function 'equal))

(defsubst ddate< (d1 d2)
  (string<
   (apply 'format "%04d%02d%02d" d1)
   (apply 'format "%04d%02d%02d" d2)))

(defsubst ddate<= (d1 d2)
  (or
   (string<
    (apply 'format "%04d%02d%02d" d1)
    (apply 'format "%04d%02d%02d" d2))
   (string=
    (apply 'format "%04d%02d%02d" d1)
    (apply 'format "%04d%02d%02d" d2))))

(defmacro ddate-yy< (d1 d2)
  (` (< (ddate-yy (, d1)) (ddate-yy (, d2)))))

(defmacro ddate-yymm< (d1 d2) 
  (` (string< (apply (quote format) "%04d%02d" (, d1))
	      (apply (quote format) "%04d%02d" (, d2)))))

;;
;; succ and dec.
;;

(defsubst ddate-yy-inc (ddate &optional c)
  (let ((new-yy (+ (or c 1) (ddate-yy ddate)))
	(mm (ddate-mm ddate))
	(dd (ddate-dd ddate)))
    (if (and (not (ddate-leap-year-p new-yy))
	     (= mm 2)
	     (= dd 29))
	(ddate-new new-yy 3 1)
      (ddate-new new-yy mm dd))))

(defsubst ddate-yy-dec (ddate &optional c)
  (ddate-yy-inc ddate (if c (- c) -1)))

(defsubst ddate-mm-inc (ddate &optional c)
  (let ((yy (ddate-yy ddate)) (mm (ddate-mm ddate)) (c (or c 1)) xx pp)
    (setq xx (+ mm c))
    (setq pp (if (< 0 xx ) (/ (- xx  1) 12) (/ (- xx 12) 12)))
    (setq yy (+ yy pp) mm (- xx (* 12 pp)))
    (if (ddate-parse (list yy mm (nth 2 ddate)))
	(list yy mm (nth 2 ddate))
      (list yy mm (ddate-days-of-mm (list yy mm 1))))))

(defsubst ddate-mm-dec (ddate &optional c)
  (ddate-mm-inc ddate (if c (- c) -1)))

(defsubst ddate-inc (date)
  (let ((yy (ddate-yy date)) (mm (ddate-mm date)) (dd (ddate-dd date)))
    (if (= dd (ddate-days-of-mm date))
	(if (= mm 12) (setq yy (1+ yy) mm 1 dd 1)
	  (setq mm (1+ mm) dd 1))
      (setq dd (1+ dd)))
    (ddate-new yy mm dd)))

(defsubst ddate-dec (date)
  (let ((yy (ddate-yy date)) (mm (ddate-mm date)) (dd (ddate-dd date)))
    (if (= dd 1)
	(if (= mm 1)
	    (setq yy (1- yy) mm 12 dd 31)
	  (setq mm (1- mm))
	  (setq dd (ddate-days-of-mm (ddate-new yy mm dd))))
      (setq dd (1- dd)))
    (ddate-new yy mm dd)))

(defsubst ddate-mm-first-day (date)
  (ddate-new (ddate-yy date) (ddate-mm date) 1))

(defsubst ddate-mm-last-day (date)
  (ddate-new (ddate-yy date) (ddate-mm date) (ddate-days-of-mm date)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; date calculation

;;
;; days between d1 to d2
;;
(defsubst ddate- (d1 d2)
  (- (ddate-days d1) (ddate-days d2)))

  
;;
;; days of the month.
;;
(defsubst ddate-days-of-mm (ddate)
  (let ((yy (ddate-yy ddate)) (mm (ddate-mm ddate)))
    (if (ddate-leap-year-p yy)
	(nth (1- mm) '(31 29 31 30 31 30 31 31 30 31 30 31))
      (nth (1- mm) '(31 28 31 30 31 30 31 31 30 31 30 31)))))
  
;;
;; returns t if the the year is leapyear.
;;
(defsubst ddate-leap-year-p (ddate-or-yy)
  (let ((yy (if (listp ddate-or-yy) (ddate-yy ddate-or-yy) ddate-or-yy)))
    (or (= (% yy 400) 0)
	(and (= (% yy 4) 0)
	     (not (= (% yy 100) 0))))))

;;
;; nth day of the year.
;;
;; example: 
;;   (1994 7 1 xx xx xx) -> 182 
;;      ; 1994-7-1 is the 182nd day of the year.
;;
(defsubst ddate-day-of-yy (ddate)
  (let (yy mm dd xx)
    (setq yy (ddate-yy ddate) mm (ddate-mm ddate) dd (ddate-dd ddate)
	  xx (if (and (ddate-leap-year-p yy) (> mm 2)) 1 0))
    (- (+ dd (* (1- mm) 31) xx)
       (nth (1- mm) '(0 0 3 3 4 4 5 5 5 6 6 7)))))


;;
;; days from 1970-01-01
;;
(defsubst ddate-days (ddate)
  (let ((xx (1- (ddate-yy ddate))))
    (- (+ (ddate-day-of-yy ddate)
	  (* xx  365)
	  (/ xx    4)
	  (/ xx -100)
	  (/ xx  400))
       719163)))

;;
;; check if date is the last week of the month.
;;
(defsubst ddate-oo-last-p (date)
  (< (- (ddate-days-of-mm date) 7) (ddate-dd date)))

;;
;; check ddate validity (both syntax and semantics.)
;;
;; returns "YYYYMMDD" if ddate is valid else nil.
;;
(defsubst ddate-parse (ddate)
  (let (yy mm dd)
    (if (not (and ddate (listp ddate)))
	nil
      (setq yy (ddate-yy ddate) mm (ddate-mm ddate) dd (ddate-dd ddate))
      (and (integerp yy) (>= yy 1000)
	   (integerp mm) (>= mm 1) (<= mm 12)
	   (integerp dd) (>= dd 1) (<= dd (ddate-days-of-mm ddate))
	   (format "%04d%02d%02d" yy mm dd)))))

(fset 'ddate-p (symbol-function 'ddate-parse))

;;
;; misc
;;

(defsubst ddate-substring-to-int (str pos)
  (cond
   ((stringp str)
    (string-to-int
     (substring str (match-beginning pos) (match-end pos))))
   (t
    (string-to-int
     (buffer-substring (match-beginning pos) (match-end pos))))))


(provide 'mhc-date)


;;; Copyright Notice:

;; Copyright (C) 1999, 2000 Yoshinari Nomura.
;; All rights reserved.

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
;; THIS SOFTWARE IS PROVIDED BY Yoshinari Nomura AND CONTRIBUTORS ``AS IS''
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL
;; Yoshinari Nomura OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
;; INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
;; OF THE POSSIBILITY OF SUCH DAMAGE.

;;; mhc-date.el ends here
