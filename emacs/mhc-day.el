;;; -*- mode: Emacs-Lisp; coding: euc-japan -*-

;; Author:  Yoshinari Nomura <nom@quickhack.net>,
;;          TSUCHIYA Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>
;; Created: 2000/05/04
;; Reviesd: $Date$


;;; Commentary:

;; This file is a part of MHC, and includes functions to manipulate
;; MHC-DAY structure.


;;; About MHC-DAY structure:

;; Each MHC-DAY structure is a cons cell has a construction as
;; follows:
;;
;;     MHC-DAY      ::= ( KEY . VALUE )
;;     KEY          ::= ( YEAR MONTH DAY-OF-MONTH )
;;     YEAR         ::= integer, larger than 1900.
;;     MONTH        ::= integer, between 1 and 12.
;;     DAY-OF-MONTH ::= integer, between 1 and 31.
;;     VALUE        ::= [ DAY-OF-WEEK HOLIDAY SCHEDULES ]
;;     DAY-OF-WEEK  ::= integer, between 0 and 6.
;;     HOLIDAY      ::= nil or t.  t stands for holiday.
;;     SCHEDULES    ::= MHC-SCHEDULE*


;;; Code:

(require 'timezone)


;; Function and macros to manipulate MHC-DAY structure:

(defun mhc-day-new (year month day-of-month &optional day-of-week holiday schedules)
  "Constructor of MHC-DAY structure."
  (cons (list year month day-of-month)
	(vector (or day-of-week
		    (% (timezone-absolute-from-gregorian month year day-of-month) 7))
		holiday
		schedules)))

(defmacro mhc-day/key (dayinfo)
  `(car ,dayinfo))
(defmacro mhc-day/value (dayinfo)
  `(cdr ,dayinfo))

(defmacro mhc-day-year (dayinfo)
  `(car (mhc-day/key ,dayinfo)))
(defmacro mhc-day-month (dayinfo)
  `(nth 1 (mhc-day/key ,dayinfo)))
(defmacro mhc-day-day-of-month (dayinfo)
  `(nth 2 (mhc-day/key ,dayinfo)))
(defmacro mhc-day-day-of-week (dayinfo)
  `(aref (mhc-day/value ,dayinfo) 0))
(defmacro mhc-day-holiday (dayinfo)
  `(aref (mhc-day/value ,dayinfo) 1))
(defmacro mhc-day-schedules (dayinfo)
  `(aref (mhc-day/value ,dayinfo) 2))

(defmacro mhc-day-set-holiday (dayinfo holiday)
  `(aset (mhc-day/value ,dayinfo) 1 ,holiday))
(defmacro mhc-day-set-schedules (dayinfo schedules)
  `(aset (mhc-day/value ,dayinfo) 2 ,schedules))

(defun mhc-day-day-of-week-as-string (dayinfo)
  "Return three letter code of the day of week."
  (aref ["Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"]
	(mhc-day-day-of-week dayinfo)))

(defun mhc-day-busy-p (dayinfo)
  (let ((schedules (mhc-day-schedules dayinfo)))
    (catch 'busy
      (while schedules
	(or (mhc-schedule-in-category-p (car schedules) "holiday")
	    (throw 'busy t))
	(setq schedules (cdr schedules))))))

(defun mhc-day-today-p (dayinfo)
  (let ((time (decode-time (current-time))))
    (and
     (eq (mhc-day-day-of-month dayinfo) (nth 3 time))
     (eq (mhc-day-month dayinfo) (nth 4 time))
     (eq (mhc-day-year dayinfo) (nth 5 time)))))


;; Utility functions:

(defun mhc-day-encode-time (day) "\
Convert DAY, as the number of days since 1970/01/01, \
to the time expression of Emacs.
Note: encode-time or current-time."
  (let (high low)
    ;; 整数が 28 bit の場合を考慮し、overflow しないよう小細工している
    (setq high (/ (setq day (* day 24)) 65536)
	  low (% day 65536))
    (setq high (+ (* high 60) (/ (setq day (* low 60)) 65536))
	  low (% day 65536))
    (setq high (+ (* high 60) (/ (setq day (* low 60)) 65536))
	  low (% day 65536))
    (list high low 0)))


(defmacro mhc-day-let (day &rest form) "\
This special form converts DAY, as the number of days since
1970/01/01, to following local variables, and evaluates FORM.

     year
          The year, an integer typically greater than 1900.

     month
          The month of the year, as an integer between 1 and 12.

     day-of-month
          The day of the month, as an integer between 1 and 31.

     day-of-week
          The day of week, as an integer between 0 and 6, where 0
          stands for Sunday.
"
  (let ((tempvar (make-symbol "decode-time")))
    `(let* ((,tempvar (decode-time (mhc-day-encode-time ,day)))
	    (day-of-month (nth 3 ,tempvar))
	    (month (nth 4 ,tempvar))
	    (year (nth 5 ,tempvar))
	    (day-of-week (nth 6 ,tempvar)))
       ,@form)))
(put 'mhc-day-let 'lisp-indent-function 1)
(put 'mhc-day-let 'edebug-form-spec '(form body))



(provide 'mhc-day)

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

;;; mhc-day.el ends here.
