;;; mhc-schedule.el -- schedule item manager.

;; Author:  Yoshinari Nomura <nom@quickhack.net>
;;
;; Created: 1997/10/12
;; Revised: $Date: 2000/05/29 14:59:25 $

;;;
;;; Commentay:
;;;

;; mhc-schedule is schedule item manipulation package.

;;;
;;; Code:
;;;

(defconst mhc-sch-subject:   "X-SC-Subject:")
(defconst mhc-sch-location: "X-SC-Location:")
(defconst mhc-sch-alarm:     "X-SC-Alarm:")
(defconst mhc-sch-category:  "X-SC-Category:")
(defconst mhc-sch-cond:      "X-SC-Cond:")
(defconst mhc-sch-duration:  "X-SC-Duration:")
(defconst mhc-sch-time:      "X-SC-Time:")
(defconst mhc-sch-day:       "X-SC-Day:")
(defconst mhc-sch-date:      "X-SC-Date:")
(defconst mhc-sch-record-id: "X-SC-Record-Id:")

(defconst mhc-sch-header-list
  (list mhc-sch-subject:
	mhc-sch-location:
	mhc-sch-alarm:
	mhc-sch-category:
	mhc-sch-cond:
	mhc-sch-duration:
	mhc-sch-time:
	mhc-sch-day:
	mhc-sch-date:
	mhc-sch-record-id:))

(defconst mhc-sch-tmp-buffer-name " *mhc-sch-tmp*")

;;
;; make new skeleton schedule item.
;;
(defun mhc-sch-new ()
  (let ((obj (copy-alist 
	      '((cond-mon    . ())
		(cond-ord    . ())
		(cond-wek    . ())
		(cond-num    . ())
		(day         . ())
		(exception   . ())
		(category    . ())
		(pilot-id    . ())
		(subject     . nil)
		(location    . nil)
		(record-id   . nil)
		(description . nil)
		(path        . nil)
		(time-b      . nil)
		(time-e      . nil)
		(duration-b  . nil)
		(duration-e  . nil)
		(alarm       . nil)
		(modified    . nil)))))
    obj
    (mhc-sch-set-record-id obj (mhc-sch-create-record-id))))

;;
;; new schedule item from current buffer.
;;

(defun mhc-sch-new-from-file (path)
  (save-excursion
    (get-buffer-create mhc-sch-tmp-buffer-name)
    (set-buffer mhc-sch-tmp-buffer-name)
    (erase-buffer)
    (insert-file-contents path nil 0 3000)
    (goto-char (point-min))
    (mhc-sch-set-path (mhc-sch-new-from-buffer) path)))

(defun mhc-sch-new-from-string (string)
  (save-excursion
    (get-buffer-create mhc-sch-tmp-buffer-name)
    (set-buffer mhc-sch-tmp-buffer-name)
    (erase-buffer)
    (insert string)
    (goto-char (point-min))
    (mhc-sch-new-from-buffer)))

(defun mhc-sch-new-from-buffer (&optional buffer)
  (let ((subject  (mhc-misc-hdr-value mhc-sch-subject:))
	(location (mhc-misc-hdr-value mhc-sch-location:))
	(alarm    (mhc-misc-hdr-value mhc-sch-alarm:))
	(category (mhc-misc-hdr-value mhc-sch-category:))
	(cond     (mhc-misc-hdr-value mhc-sch-cond:))
	(duration (mhc-misc-hdr-value mhc-sch-duration:))
	(time     (mhc-misc-hdr-value mhc-sch-time:))
	(day      (mhc-misc-hdr-value mhc-sch-day:))
	(id       (mhc-misc-hdr-value mhc-sch-record-id:))
	(odate    (mhc-misc-hdr-value mhc-sch-date:)) ;; backward compatibility
	(obj      (mhc-sch-new))
	(date) yy mm dd tt beg end)
    (if buffer (set-buffer buffer))
    (mhc-sch-set-path obj (buffer-file-name))
    (if odate
      (if (string-match
	   (concat "^\\([0-9]+\\)[\t ]+\\([A-Z][a-z][a-z]\\)[\t ]+"
		   "\\([0-9]+\\)[\t ]+\\([0-9][0-9]:[0-9][0-9]\\)")
	   odate
	   )
	  (progn
	    (setq dd  (ddate-substring-to-int odate 1)
		  mm  (substring odate (match-beginning 2) (match-end 2))
		  yy  (+ 1900 (ddate-substring-to-int odate 3))
		  tt  (dtime-new
		       (substring odate (match-beginning 4) (match-end 4))))
	    (setq mm 
		  (/ 
		   (string-match 
		    mm 
		    "   JanFebMarAprMayJunJulAugSepOctNovDec") 3))
	    (mhc-sch-set-time obj tt nil)
	    (mhc-sch-add-day  obj (ddate-new yy mm dd)))))
    (if id
	(mhc-sch-set-record-id obj id)
      (mhc-sch-set-record-id obj (mhc-sch-create-record-id)))
    (if subject
	(mhc-sch-set-subject obj subject))
    (if location
	(mhc-sch-set-location obj location))
    (if alarm
	(mhc-sch-set-alarm-by-string obj alarm))
    (if category
	(mapcar '(lambda (x) (mhc-sch-add-category obj x))
		(mhc-misc-split category)))
    (if cond
	(mapcar '(lambda (x) (mhc-sch-add-cond obj x))
		(mhc-misc-split cond)))
    (if (not duration)
	()
      (cond
       ((string-match "^\\([0-9]+\\)-\\([0-9]+\\)$" duration)
	(setq beg (substring duration (match-beginning 1) (match-end 1))
	      end (substring duration (match-beginning 2) (match-end 2))))
       ((string-match "^\\([0-9]+\\)-$" duration)
	(setq beg (substring duration (match-beginning 1) (match-end 1))
	      end ddate-max-const))
       ((string-match "^-\\([0-9]+\\)$" duration)
	(setq beg ddate-min-const
	      end (substring duration (match-beginning 1) (match-end 1))))
       (t
	(error "duration format error")))
      (mhc-sch-set-duration obj 
			    (if (stringp beg) (ddate-new beg) beg)
			    (if (stringp end) (ddate-new end) end)))
    (if time 
	(if (string-match
	     "^\\([0-9]+:[0-9]+\\)\\(-\\([0-9]+:[0-9]+\\)\\)?$"
	     time)
	    (mhc-sch-set-time
	     obj
	     (dtime-new (substring time (match-beginning 1) (match-end 1)))
	     (and 
	      (match-beginning 2)
	      (dtime-new (substring time (match-beginning 3) (match-end 3)))))
	  (error "Time format error")))
    (if day
	(while (string-match "^\\(!\\)?\\([0-9]+\\) *" day)
	  (setq date
		(ddate-new (substring day (match-beginning 2) (match-end 2))))
	  (if (match-beginning 1)
	      (mhc-sch-del-day obj date)
	    (mhc-sch-add-day  obj date))
	  (setq day (substring day (match-end 0)))))
    (if (and day (not (string-match "^ *$" day)))
	(error "Date format error '%s'" day))
    obj))

;;
;; subject
;;

(defun mhc-sch-subject     (obj)     (mhc-sch-get obj 'subject))
(defun mhc-sch-set-subject (obj val) (mhc-sch-set obj 'subject val))
(defun mhc-sch-subject-as-string (obj)
  (or (mhc-sch-get obj 'subject) ""))


;;
;; location
;;

(defun mhc-sch-location           (obj)     (mhc-sch-get obj 'location))
(defun mhc-sch-set-location       (obj val) (mhc-sch-set obj 'location val))
(defun mhc-sch-location-as-string (obj)    
  (or (mhc-sch-get obj 'location) ""))

;;
;; record-id
;;
(defun mhc-sch-record-id (obj) (mhc-sch-get obj 'record-id))
(defun mhc-sch-set-record-id (obj val) (mhc-sch-set obj 'record-id val))
(defun mhc-sch-record-id-as-string (obj)
  (or (mhc-sch-get obj 'record-id) ""))


;;
;; day
;;

(defun mhc-sch-day     (obj)      (mhc-sch-get obj 'day))
(defun mhc-sch-set-day (obj dlst) (mhc-sch-set obj 'day (ddate-sort dlst)))

(defun mhc-sch-add-day (obj date)
  ;; First, check to see if Duration: encumbers
  (if (not (mhc-sch-in-duration-p obj date))
      (error "mhc-sch-add-day error (duration encumbers %s)" date))
  (cond
   ((mhc-sch-in-exception-p obj date)
    ;; if date is in exception, remove, it.
    (mhc-sch-del obj 'exception date))
   ((mhc-sch-in-cond-p obj date)
    ;; It is happy if Cond: covers the date.
    obj)
   ((not (mhc-sch-in-day-p obj date))
    (mhc-sch-add obj 'day date)
    (mhc-sch-set-day obj (mhc-sch-day obj))))
  obj)

(defun mhc-sch-del-day (obj date)
  (cond
   ((not (mhc-sch-occur-on-p obj date))
    obj)
   (t
    (mhc-sch-del obj 'day date)
    (if (mhc-sch-in-cond-p obj date)
	(progn
	  (mhc-sch-add obj 'exception date)
	  (mhc-sch-set-exception obj (mhc-sch-exception obj))))))
  obj)

(defun mhc-sch-day-as-string (obj) ;; exceptions are included.
  (let (day exc)
    (setq day (mapconcat (function ddate-to-s)
			 (mhc-sch-day obj) " "))
    (setq exc (mapconcat '(lambda (x) (concat "!" (ddate-to-s x)))
			 (mhc-sch-exception obj) " "))
    (concat day (if (string= day "") "" " ") exc)))

;;
;; exception -- del_exception and add_exception are
;;              covered by del_day and add_day.
;;

(defun mhc-sch-exception (obj) (mhc-sch-get obj 'exception))
(defun mhc-sch-set-exception (obj dlst)
  (mhc-sch-set obj 'exception (ddate-sort dlst)))

;;
;; time
;;

(defun mhc-sch-time (obj)
  (let ((time-b (mhc-sch-time-b obj)))
    (if time-b
	(cons time-b (mhc-sch-time-e obj))
      nil)))

(defun mhc-sch-time-as-string (obj)
  (let ((b (mhc-sch-time-b obj))
	(e (mhc-sch-time-e obj)))
    (if (and b e)
	(concat (dtime-to-s b) "-" (dtime-to-s e))
      (if b (dtime-to-s b) ""))))

(defun mhc-sch-time-b (obj) (mhc-sch-get obj 'time-b))
(defun mhc-sch-time-e (obj) (mhc-sch-get obj 'time-e))

(defun mhc-sch-set-time (obj b e)
  (mhc-sch-set obj 'time-b b)
  (mhc-sch-set obj 'time-e e))

;;
;; alarm
;;

(defun mhc-sch-alarm (obj) (mhc-sch-get obj 'alarm))
(defun mhc-sch-alarm-as-string (obj) 
  (let ((alarm (mhc-sch-alarm obj)))
    (cond 
     ((null alarm)
      "")
     ((and (= (% alarm 86400) 0) (<= alarm 8553600)) ;; 86400 * 99
      (format "%d day" (/ alarm 86400)))
     ((and (= (% alarm 3600) 0) (<= alarm 356400)) ;; 3600 * 99
      (format "%d hour" (/ alarm 3600)))
     ((and (= (% alarm 60) 0) (<= alarm 5940)) ;; 60 * 99
      (format "%d minute" (/ alarm 60)))
     (t
      (error "Alarm format error (%s)" alarm)))))

(defun mhc-sch-set-alarm (obj sec) (mhc-sch-set obj 'alarm sec))

(defun mhc-sch-set-alarm-by-string (obj num &optional unit)
  (let* ((n (string-to-int num))
	 (unit (or unit (nth 1 (mhc-misc-split num))))
	 sec)
    (cond
     ((string= unit "minute")
      (setq sec (* n 60)))
     ((string= unit "hour")
      (setq sec (* n 3600)))
     ((string= unit "day")
      (setq sec (* n 86400))))
    (if sec
	(mhc-sch-set-alarm obj sec)
      (error "Alarm format error (%s %s)" num unit))))

;;
;; category ... a list of string.
;;

(defun mhc-sch-category (obj)   (mhc-sch-get obj 'category))
(defun mhc-sch-category-as-string (obj)
  (mhc-misc-join (mhc-sch-category obj) " "))

(defun mhc-sch-set-category (obj val)
  (mhc-sch-set obj 'category (mapcar 'capitalize val)))

(defun mhc-sch-add-category (obj val)
  (mhc-sch-add obj 'category (capitalize val)))

(defun mhc-sch-del-category (obj val)
  (mhc-sch-del obj 'category (capitalize val)))

;;
;; cond
;;

(defun mhc-sch-cond (obj)
  (append
   (mhc-sch-cond-mon obj)
   (mhc-sch-cond-ord obj)
   (mhc-sch-cond-wek obj)
   (mhc-sch-cond-num obj)))

(defun mhc-sch-cond-as-string (obj)
  (mhc-misc-strip 
   (concat 
    (mhc-misc-join (mhc-sch-cond-mon obj) " ") " "
    (mhc-misc-join (mhc-sch-cond-ord obj) " ") " "
    (mhc-misc-join (mhc-sch-cond-wek obj) " ") " "
    (mhc-misc-join (mhc-sch-cond-num obj) " "))))

(defun mhc-sch-cond-mon (obj) (mhc-sch-get obj 'cond-mon))
(defun mhc-sch-cond-ord (obj) (mhc-sch-get obj 'cond-ord))
(defun mhc-sch-cond-wek (obj) (mhc-sch-get obj 'cond-wek))
(defun mhc-sch-cond-num (obj) (mhc-sch-get obj 'cond-num))

(defun mhc-sch-set-cond (obj string-lst)
  (while (car string-lst)
    (mhc-sch-add-cond obj (car string-lst))
    (setq string-lst (cdr string-lst)))
  obj)

(defun mhc-sch-add-cond (obj string)
  (cond
   ((string-match "^\\(1st\\|2nd\\|3rd\\|4th\\|5th\\|Last\\)$" string)
    (mhc-sch-add obj 'cond-ord string))
   ((string-match (concat "\\(Jan\\|Feb\\|Mar\\|Apr\\|May\\|Jun\\|"
			  "Jul\\|Aug\\|Sep\\|Oct\\|Nov\\|Dec\\)$") string)
    (mhc-sch-add obj 'cond-mon string))
   ((string-match "^\\(Sun\\|Mon\\|Tue\\|Wed\\|Thu\\|Fri\\|Sat\\)$" string)
    (mhc-sch-add obj 'cond-wek string))
   ((string-match "^[0-9]+$" string)
    (mhc-sch-add obj 'cond-num (format "%02d" (string-to-int string))))
   (t
    (error "Condition format error (%s)" string))))


(defun mhc-sch-del-cond (obj string)
  (cond
   ((string-match "^\\(1st\\|2nd\\|3rd\\|4th\\|5th\\|Last\\)$" string)
    (mhc-sch-del  obj 'cond-ord string))
   ((string-match (concat "\\(Jan\\|Feb\\|Mar\\|Apr\\|May\\|Jun\\|"
			  "Jul\\|Aug\\|Sep\\|Oct\\|Nov\\|Dec\\)$") string)
    (mhc-sch-del obj 'cond-mon string))
   ((string-match "^\\(Sun\\|Mon\\|Tue\\|Wed\\|Thu\\|Fri\\|Sat\\)$" string)
    (mhc-sch-del obj 'cond-wek string))
   ((string-match "^[0-9]+$" string)
    (mhc-sch-del obj 'cond-num (format "%02d" (string-to-int string))))
   (t
    (error "Condition format error (%s)" string))))

;;
;; duration
;;
(defun mhc-sch-duration (obj)
  (let ((duration-b (mhc-sch-duration-b obj)))
    (if duration-b (cons duration-b (mhc-sch-duration-b obj)) nil)))

(defun mhc-sch-duration-as-string (obj)
  (let ((b (mhc-sch-duration-b obj))
	(e (mhc-sch-duration-e obj)))
    (if (and b e)
	(concat (ddate-to-s b) "-" (ddate-to-s e))
      (if (and (null b) (null e)) ""
	(error "mhc-sch-duration-as-string error (%s %s)" b e)))))

(defun mhc-sch-duration-b (obj) (mhc-sch-get obj 'duration-b))
(defun mhc-sch-duration-e (obj) (mhc-sch-get obj 'duration-e))
(defun mhc-sch-set-duration (obj b e)
  (mhc-sch-set obj 'duration-b b)
  (mhc-sch-set obj 'duration-e e))

;;
;; description
;;

(defun mhc-sch-description (obj) (mhc-sch-get obj 'description))
(defun mhc-sch-set-description (obj desc) (mhc-sch-set obj 'description desc))

;;
;; path
;;
(defun mhc-sch-path (obj) (mhc-sch-get obj 'path))
(defun mhc-sch-set-path (obj path) (mhc-sch-set obj 'path path))

;;
;; dump
;;

(defun mhc-sch-dump (obj)) ;; xxx

(defun mhc-sch-dump-header (obj)
  (format 
   (concat
    "X-SC-Subject: %s\n"
    "X-SC-Location: %s\n"
    "X-SC-Day: %s\n"
    "X-SC-Time: %s\n"
    "X-SC-Category: %s\n"
    "X-SC-Cond: %s\n"
    "X-SC-Duration: %s\n"
    "X-SC-Alarm: %s\n"
    "X-SC-Record-Id: %s\n")
   (mhc-sch-subject-as-string obj)
   (mhc-sch-location-as-string obj)
   (mhc-sch-day-as-string obj)
   (mhc-sch-time-as-string obj)
   (mhc-sch-category-as-string obj)
   (mhc-sch-cond-as-string obj)
   (mhc-sch-duration-as-string obj)
   (mhc-sch-alarm-as-string obj)
   (mhc-sch-record-id obj)))

;;
;; predicates.
;;

(defun mhc-sch-in-day-p (obj date)
  (member date (mhc-sch-day obj)))

(defun mhc-sch-in-exception-p (obj date)
  (member date (mhc-sch-exception obj)))

(defun mhc-sch-in-duration-p (obj date)
  (cond
   ((and (mhc-sch-duration-b obj) (ddate< date (mhc-sch-duration-b obj)))
    nil)
   ((and (mhc-sch-duration-e obj) (ddate< (mhc-sch-duration-e obj) date))
    nil)
   (t
    t)))

(defun mhc-sch-in-cond-p (obj date)
  (let ((mon (ddate-mm-s date)) (wek (ddate-ww-s date))
	(ord (ddate-oo-s date)) (num (ddate-dd-s date))
	(cond-mon (mhc-sch-cond-mon obj))
	(cond-wek (mhc-sch-cond-wek obj))
	(cond-ord (mhc-sch-cond-ord obj))
	(cond-num (mhc-sch-cond-num obj)))
    (cond
     ((not (or (null cond-mon) (member mon cond-mon)))
      nil)
     ((member num cond-num)
      t)
     ((and (not (or (null cond-ord) (member ord cond-ord)))
	   (not (and (ddate-oo-last-p date) (member "Last" cond-ord))))
      nil)
     ((member wek cond-wek)
      t)
     (t
      nil))))

(defun mhc-sch-in-category-p (obj category)
  (if (listp category)
      (catch 'found
	(while category
	  (if (member (capitalize (car category)) (mhc-sch-category obj))
	      (throw 'found t))
	  (setq category (cdr category))))
    (member (capitalize category) (mhc-sch-category obj))))

;;
;; occurency calculations.
;;

(defun mhc-sch-occur-max (obj)
  (let ((day-lst (mhc-sch-day obj))
	(duration-e (mhc-sch-duration-e obj))
	(max nil))
    (if (not (and (null (mhc-sch-cond-wek obj))
		  (null (mhc-sch-cond-num obj))))
	(setq max (ddate-new 2037 12 31))
      (while (car day-lst)
	(setq max (if max (ddate-max max (car day-lst)) (car day-lst)))
	(setq day-lst (cdr day-lst))))
    (if (and max duration-e  (ddate< duration-e max))
	(setq max duration-e))
    max))

(defun mhc-sch-occur-min (obj)
  (let ((day-lst (mhc-sch-day obj))
	(duration-b (mhc-sch-duration-b obj))
	(min nil))
    (if (not (and (null (mhc-sch-cond-wek obj))
		  (null (mhc-sch-cond-num obj))))
	(setq min (ddate-new 1970 1 2))
      (while (car day-lst)
	(setq min (if min (ddate-min min (car day-lst)) (car day-lst)))
	(setq day-lst (cdr day-lst))))
    (if (and min duration-b  (ddate< min duration-b))
	(setq min duration-b))
    min))

(defun mhc-sch-occur-on-p (obj date)
  (and (or (mhc-sch-in-day-p obj date)
	   (mhc-sch-in-cond-p obj date))
       (and (not (mhc-sch-in-exception-p obj date))
	    (mhc-sch-in-duration-p obj date))))

(defun mhc-sch-occur-inter-month-p (obj)
  (let ((min (mhc-sch-occur-min obj)) (max (mhc-sch-occur-max obj)))
    (if (and min max)
	(cond 
	 ((< (ddate-yy min) (ddate-yy max))
	  t)
	 ((< (ddate-mm min) (ddate-mm max))
	  t)
	 (t
	  nil)) ;; means all occurences are in one month.
      nil))) ;; means no occurence exists.

(defun mhc-sch-occur-multiple-p (obj)
  (let ((min (mhc-sch-occur-min obj)) (max (mhc-sch-occur-max obj)))
    (if (and min max)
	(cond 
	 ((not (ddate= min max))
	  t)
	 (t
	  nil)) ;; means: no occurences specified.
      nil))) ;; means: there is only one occurence.

(defun mhc-sch-occur-any-p (obj)
  (mhc-sch-occur-min obj))

;;
;; error check
;;

(defun mhc-sch-error-p (obj)
  (or (string= (mhc-sch-subject-as-string obj) "")
      (not (mhc-sch-occur-any-p obj))))

(defun mhc-sch-error-message (obj)
  (let ((ret ""))
    (if (string= (mhc-sch-subject-as-string obj) "")
	(setq ret (concat ret " no subject")))
    (if (not (mhc-sch-occur-any-p obj))
	(setq ret (concat ret " no occurences")))
    ret))

;;
;; private household functions.
;;

(defvar mhc-sch-record-id-counter 0)

(defun mhc-sch-create-record-id ()
  (let ((uid (user-login-name))
	(time 
	 (concat
	  (apply 'format "%04d%02d%02d" (ddate-now))
	  (apply 'format "%02d%02d%02d" (dtime-now t))))
	(sequence (format "%04d" mhc-sch-record-id-counter))
	(host (system-name)))
    (setq mhc-sch-record-id-counter (1+ mhc-sch-record-id-counter))
    (concat "<" time sequence "." uid "@" host ">")))

(defun mhc-sch-get (obj attr)
  (cdr (assq attr obj)))

(defun mhc-sch-set (obj attr value)
  (setcdr (assq attr obj) value)
  obj)

(defun mhc-sch-add (obj attr value)
  (let ((p (assq attr obj)))
    (if (cdr p)
	(setcdr p (cons value (cdr p)))
      (setcdr p (list value)))
    obj))

(defun mhc-sch-del (obj attr value)
  (mhc-sch-set obj attr (delete value (mhc-sch-get obj attr))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; schedule DB 

;; (defvar mhc-db-rc-file     (expand-file-name "~/.schedule"))
;; (defvar mhc-db-base-dir    (expand-file-name "~/Mail/schedule"))

(defvar mhc-db-rc-file          nil)
(defvar mhc-db-base-dir         nil)
(defvar mhc-db-slot-mtime-cache nil)
(defvar mhc-db-log-file         nil)
(defvar mhc-db-alist ())

(defun mhc-db-setup (rcfile base-dir)
  ;; (message "Setup mhc DB ...")
  (if rcfile   (setq mhc-db-rc-file  rcfile))
  (if base-dir (setq mhc-db-base-dir base-dir))
  (if base-dir (setq mhc-db-log-file
		     (expand-file-name ".mhc-db-log" base-dir)))

  (setq mhc-db-alist ())
  (setq mhc-db-slot-mtime-cache ())
  ;; (mhc-db-scan (ddate-now))
  ;; (mhc-db-scan (ddate-mm-inc (ddate-now)))
  ;; (message "Setup mhc DB ... done.")
  )

(defun mhc-db-date-to-slot (date)
  (list (expand-file-name (ddate-yymm-s1 date "/") mhc-db-base-dir)
	(expand-file-name "intersect" mhc-db-base-dir)
	mhc-db-rc-file))

(defun mhc-db-busy-p (date)
  (mhc-db-search1 date nil nil t))

(defun mhc-db-busy-on-p (date btime &optional etime exc-path)
  (let ((sch-list (mhc-db-search1 date nil nil t))
	(ret nil) (sch nil) btime2 etime2
	(etime  (or etime btime)))
    (while sch-list
      (setq sch    (car sch-list)
	    btime2 (mhc-sch-time-b sch)
	    etime2 (or (mhc-sch-time-e sch) btime2))
      (if (not (or (not (and btime btime2))
		   (dtime<=  etime btime2)
		   (dtime<=  etime2 btime)
		   (and exc-path (string= exc-path (mhc-sch-path sch)))))
	  (setq ret (cons sch ret)))
      (setq sch-list (cdr sch-list)))
    ret))

(defun mhc-db-search1 (date &optional category category-is-invert update-slot)
  (let ((mon (ddate-mm-s date)) (wek (ddate-ww-s date))
	(ord (ddate-oo-s date)) (num (ddate-dd-s date))
	(all "all") (last "Last") (ret nil) (keys nil) (sch nil) (sch-list nil)
	(search-key nil) (slot-list (mhc-db-date-to-slot date)))
    (setq search-key (list (ddate-to-s date)
			   (concat mon ord wek) (concat mon all wek)
			   (concat all ord wek) (concat all all wek)
			   (concat mon num) (concat all num)))
    (if (ddate-oo-last-p date)
	(setq search-key
	      (cons (concat all last wek)
		    (cons (concat mon last wek) search-key))))
    (while slot-list
      (if update-slot (mhc-db-update-slot (car slot-list)))
      (setq keys search-key)
      (while keys
	(setq sch-list
	      (cdr (assoc 
		    (car keys)
		    (cdr (assoc (car slot-list) mhc-db-alist)))))
	(while sch-list
	  (setq sch (car sch-list))
	  (if (and (mhc-sch-in-duration-p sch date)
		   (not (mhc-sch-in-exception-p sch date))
		   (or (null category)
		       (and category-is-invert
			    (not (mhc-sch-in-category-p sch category)))
		       (and (not category-is-invert)
			    (mhc-sch-in-category-p sch category))))
 	      (setq ret (cons sch ret)))
	  (setq sch-list (cdr sch-list)))
	(setq keys (cdr keys)))
      (setq slot-list (cdr slot-list)))
    (sort ret (function 
	       (lambda (a b)
		 (dtime< (mhc-sch-time-b a) (mhc-sch-time-b b)))))
    ))
    
;;
;; scan dir and regist schedule articles.
;; (makeup mhc-db-alist)
;;

(defun mhc-db-regist-schedule (sch slot)
  (let ((day (mhc-sch-day  sch))
	(mon (mhc-sch-cond-mon  sch))
	(ord (mhc-sch-cond-ord  sch))
	(wek (mhc-sch-cond-wek  sch))
	(num (mhc-sch-cond-num  sch))
	tmp-wek)
    (if (null mon) (setq mon (list "all")))
    (if (null ord) (setq ord (list "all")))
    (while day
      (mhc-db-regist-schedule2 slot (ddate-to-s (car day)) sch)
      (setq day (cdr day)))
    (while mon
      (while ord
	(setq tmp-wek wek)
	(while tmp-wek
	  (mhc-db-regist-schedule2
	   slot (concat (car mon) (car ord) (car tmp-wek)) sch)
	  (setq tmp-wek (cdr tmp-wek)))
	(setq ord (cdr ord)))
      (while num
	(mhc-db-regist-schedule2 slot (concat (car mon) (car num)) sch)
	(setq num (cdr num)))
      (setq mon (cdr mon)))))

(defun mhc-db-regist-schedule2 (slot key sch)
  (let ((key-sch nil) sch-list)
    ;; (insert (concat slot " " key " " (mhc-sch-subject sch) "\n"))
    (setq sch-list
	  (cons sch (cdr (assoc key (cdr (assoc slot mhc-db-alist))))))
    (if (assoc key (cdr (assoc slot mhc-db-alist)))
	(setcdr (assoc key (cdr (assoc slot mhc-db-alist))) sch-list)
      (if (assoc slot mhc-db-alist)
	  (setcdr (assoc slot mhc-db-alist)
		  (cons (cons key sch-list) (cdr (assoc slot mhc-db-alist))))
	(setq mhc-db-alist
	      (cons (cons slot (list (cons key sch-list))) mhc-db-alist))))))

(defun mhc-db-scan (date &optional force)
  (let ((slot-list (mhc-db-date-to-slot date)) sch-list)
    (while slot-list
      (if (or force (mhc-db-slot-modified-p (car slot-list)))
	  (setq sch-list (mhc-db-scan-slot (car slot-list))))
      (setq slot-list (cdr slot-list)))
    sch-list))

(defun mhc-db-scan-slot-dir (dir)
  (let ((dirent (directory-files dir))
	(ret nil) (sch nil) (path nil))
    (if (assoc dir mhc-db-alist) (setcdr (assoc dir mhc-db-alist) nil))
    (while dirent
      (if (not (string-match "^[0-9]+$" (car dirent)))
	  ()
	(setq path (expand-file-name (car dirent) dir))
	(setq sch  (mhc-sch-new-from-file path))
	(mhc-db-regist-schedule sch dir))
      (setq dirent (cdr dirent)))
    ret))

(defconst mhc-sch-rc-buffer-name " *mhc-sch-rc-file*")

(defun mhc-db-scan-slot-file (file)
  (let* ((ret nil) (sch nil) (path nil) (begin nil))
    (if (assoc file mhc-db-alist) (setcdr (assoc file mhc-db-alist) nil))
    (save-excursion
      (get-buffer-create mhc-sch-rc-buffer-name)
      (set-buffer mhc-sch-rc-buffer-name)
      (erase-buffer)
      (insert-file-contents file nil)
      (goto-char (point-min))
      (while (re-search-forward "^X-SC-" nil t)
	(beginning-of-line)
	(setq begin (point))
	(if (not (re-search-forward "^$" nil t))
	    ()
	  (mhc-db-regist-schedule
	   (setq sch (mhc-sch-new-from-string
		      (buffer-substring begin (point))))
	   file))))))

(defun mhc-db-add-sch (sch &optional buffer maildir)
  (let ((old-slot (mhc-sch-old-slot sch))
	(old-path (mhc-sch-path sch))
	(new-slot (mhc-sch-new-slot sch))
	new-path
	(maildir (file-name-as-directory maildir)))
    (if (and old-slot (equal old-slot new-slot))
	(setq new-path old-path)
      (setq new-path (mhc-misc-get-new-path new-slot)))
    ;; save buffer contents to the new-path
    (if (not (y-or-n-p
	      (if maildir
		  (format "Refile %s to %s "
			  (mhc-misc-sub (or old-slot "") maildir "+") 
			  (mhc-misc-sub new-slot maildir "+"))
		(format "Refile %s to %s " (or old-slot "") new-slot))))
	nil ;; return value
      (if buffer (mhc-misc-copy-buffer-to-file buffer new-path))
      (mhc-misc-touch-directory new-slot)
      (mhc-db-scan-slot new-slot)
      (mhc-sch-set-path sch new-path)
      ;; if old-path != new-path trash the old-path.
      (if (and old-path (file-exists-p old-path)
	       (not (equal old-path new-path)))
	  (progn 
	    (rename-file old-path (mhc-misc-get-new-path
				   (expand-file-name "trash" mhc-db-base-dir)))
	    (mhc-misc-touch-directory old-slot)
	    (mhc-db-scan-slot old-slot)))
      (mhc-db-append-log 'modify sch)
      sch ;; return value
      )))

(defun mhc-db-del-sch (sch)
  (let ((old-path   (mhc-sch-path sch))
	(old-slot (mhc-sch-old-slot sch))
	(trash-path (mhc-misc-get-new-path
		     (expand-file-name "trash" mhc-db-base-dir))))
    (if (and old-path (file-exists-p old-path))
	(progn
	  (rename-file old-path trash-path)
	  (mhc-db-append-log 'delete sch)
	  (mhc-misc-touch-directory old-slot)
	  (mhc-db-scan-slot old-slot)))))


(defun mhc-db-slot-cache-update (slot mtime)
  (setq mhc-db-slot-mtime-cache 
	(cons (cons slot mtime)
	      (delete 
	       (assoc slot mhc-db-slot-mtime-cache)
	       mhc-db-slot-mtime-cache))))

(defun mhc-db-slot-modified-p (slot)
  (let* ((mtime    (mhc-misc-get-mtime slot))
	 (mtime-ms (car mtime))
	 (mtime-ls (car (cdr mtime)))
	 (cache    (cdr (assoc slot mhc-db-slot-mtime-cache)))
	 (cache-ms (car cache))
	 (cache-ls (car (cdr cache))))
    (cond
     ((null mtime) ;; slot is not exist yet.
      nil)
     ((null cache)
      t)           ;; there is not the slot in the cache.
     ((< cache-ms mtime-ms)
      t)
     ((= cache-ms mtime-ms)
      (if (< cache-ls mtime-ls) t nil)) ;; nil if same
     (t            ;; cache is old.
      nil))))

(defun mhc-db-update-slot (slot)
  (if (not (mhc-db-slot-modified-p slot))
      ()
    (mhc-db-scan-slot slot)))


(defun mhc-db-scan-slot (slot)
  (let ((obj slot))
    (cond
     ((file-accessible-directory-p obj)
      (mhc-db-scan-slot-dir obj)
      (mhc-db-slot-cache-update slot 
				(mhc-misc-get-mtime obj)))
     ((file-readable-p obj)
      (mhc-db-scan-slot-file obj)
      (mhc-db-slot-cache-update slot
				(mhc-misc-get-mtime obj)))
     (t nil))))

(defun mhc-db-append-log (status sch)
  (if mhc-db-log-file
      (let ((tmp-buffer (get-buffer-create " *mhc-spool*")))
	(buffer-disable-undo tmp-buffer)
	(save-excursion
	  (set-buffer tmp-buffer)
	  (kill-region (point-min) (point-max))
	  (insert (format "%c %s %s %s %s\n"
			  (cond
			   ((eq status 'add) ?A)
			   ((eq status 'delete) ?D)
			   ((eq status 'modify) ?M)
			   (t ??))
			  (let ((now (current-time)))
			    (concat (ddate-to-s1 (ddate-now now) "-") " "
				    (dtime-to-s (dtime-now t now) t)))
			  (mhc-sch-record-id sch)
			  (mhc-sch-path sch)
			  (mhc-sch-subject sch)))
	  (mhc-misc-copy-buffer-to-file tmp-buffer
					mhc-db-log-file	t)))))

(defun mhc-sch-new-slot (sch)
  (if (mhc-sch-occur-inter-month-p sch)
      (expand-file-name "intersect" mhc-db-base-dir)
    (expand-file-name (ddate-yymm-s1 (mhc-sch-occur-min sch) "/")
		      mhc-db-base-dir)))

(defun mhc-sch-old-slot (sch)
  (if (mhc-sch-path sch)
      (directory-file-name (file-name-directory (mhc-sch-path sch)))))

(defun mhc-db-holiday-p (ddate)
  (let ((sch-lst (mhc-db-search1 ddate)))
    (catch 'true
      (while sch-lst
	(if (mhc-sch-in-category-p (car sch-lst) "Holiday")
	    (throw 'true t))
	(setq sch-lst (cdr sch-lst))))))

(provide 'mhc-schedule)

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

;;; mhc-schedule.el ends here
