;;; -*- mode: Emacs-Lisp; coding: euc-japan -*-

;; Author:  Yoshinari Nomura <nom@quickhack.net>,
;;          TSUCHIYA Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>
;; Created: 2000/05/01
;; Revised: $Date$


;;; Commentary:

;; This file is a part of MHC.

;; This file consists of two parts: the first part contains MUA
;; backend functions, and the second part contains functions to make
;; summary contents.


;;; About MUA Backend:

;; In order to define new MUA backend, it is required to define these
;; methods.
;;
;;     (mhc-foo-summary-filename)
;;         Return the file name of the article on the current line in
;;         this summary buffer.
;;
;;     (mhc-foo-summary-display-article)
;;         Display the article on the current line in this buffer.
;;
;;     (mhc-foo-get-import-buffer GET-ORIGINAL)
;;         Return buffer visiting import article.  If GET-ORIGINAL,
;;         return it without MIME decode.
;;
;;     (mhc-foo-generate-summary-buffer DDATE)
;;         Generate summary buffer of mailer, and change current
;;         buffer to it.  This function will be called at the top of
;;         mhc-scan-month.
;;
;;     (mhc-foo-insert-summary-contents SCHEDULE CONTENTS)
;;         Insert string CONTENTS to the current buffer as SCHEDULE.
;;
;;     (mhc-foo-summary-mode-setup DDATE)
;;         Setup buffer as summary of mailer.  This function will be
;;         called at the end of mhc-scan-month.
;;
;; Define these methods appropriately, and put definitions as follows:
;;
;;    (provide 'mhc-foo)
;;    (put 'mhc-foo 'summary-filename        'mhc-foo-summary-filename)
;;    (put 'mhc-foo 'summary-display-article 'mhc-foo-summary-display-article)
;;    (put 'mhc-foo 'get-import-buffer       'mhc-foo-get-import-buffer)
;;    (put 'mhc-foo 'generate-summary-buffer 'mhc-foo-generate-summary-buffer)
;;    (put 'mhc-foo 'insert-summary-contents 'mhc-foo-insert-summary-contents)
;;    (put 'mhc-foo 'summary-mode-setup      'mhc-foo-summary-mode-setup)


;;; Global Variables:

(defcustom mhc-summary-string-conflict "[C]"
  "*String which indicates conflicts in summary buffer."
  :group 'mhc
  :type 'string)

(defcustom mhc-summary-string-secret "[SECRET]"
  "*String which hides private subjects in summary buffer."
  :group 'mhc
  :type 'string)

(defcustom mhc-use-icon t
  "*If non-nil, schedule icon is used."
  :group 'mhc
  :type 'boolean)

(defcustom mhc-icon-path (if (fboundp 'locate-data-directory)
			     (locate-data-directory "mhc"))
  "*Icon path for MHC."
  :group 'mhc
  :type 'directory)

;;; Internal Variable:

(defconst mhc-summary-major-mode-alist
  '((mew-summary-mode  . mhc-mew)
    (mew-virtual-mode  . mhc-mew)
    (wl-folder-mode    . mhc-wl)
    (wl-summary-mode   . mhc-wl)
    (gnus-group-mode   . mhc-gnus)
    (gnus-summary-mode . mhc-gnus)))

(defconst mhc-summary-icon-position 24) ; Insert position of icon.

;;; MUA Backend Functions:

(defun mhc-summary-mailer-type ()
  "Return mailer backend symbol using currently."
  (or (cdr (assq major-mode mhc-summary-major-mode-alist))
      (intern (concat "mhc-" (symbol-name mhc-mailer-package)))))

(defun mhc-summary/true (&rest args)
  "This is the dummy backend function, which always returns t."
  t)

(defsubst mhc-summary-get-function (operation &optional mailer)
  "Return appropriate function to do OPERATION for MAILER."
  (or (get (require (or mailer (mhc-summary-mailer-type))) operation)
      'mhc-summary/true))

(defsubst mhc-summary-filename (&optional mailer)
  "Return file name of article on current line."
  (funcall (mhc-summary-get-function 'summary-filename mailer)))

(defsubst mhc-summary-display-article (&optional mailer)
  "Display article on current line."
  (funcall (mhc-summary-get-function 'summary-display-article mailer)))

(defsubst mhc-summary-get-import-buffer (&optional get-original mailer)
  "Return buffer to import article."
  (funcall (mhc-summary-get-function 'get-import-buffer mailer) get-original))

(defsubst mhc-summary-generate-buffer (ddate &optional mailer)
  "Generate buffer with summary mode of MAILER."
  (funcall (mhc-summary-get-function 'generate-summary-buffer mailer) ddate))

(defsubst mhc-summary-insert-contents (schedule contents icon &optional mailer)
  "Insert CONTENTS for SCHEDULE in summary buffer of MAILER."
  (if (eq 'direct mailer)
      (insert contents "\n") ; don't put icon.
    (funcall (mhc-summary-get-function 'insert-summary-contents mailer)
	     schedule contents icon)))

(defsubst mhc-summary-mode-setup (ddate &optional mailer)
  "Setup buffer as summary mode of MAILER."
  (funcall (mhc-summary-get-function 'summary-mode-setup mailer) ddate))

(defun mhc-summary-record (&optional mailer)
  "Return record on current line."
  (let ((filename (mhc-summary-filename mailer)))
    (if filename
	(let ((key (mhc-slot-directory-to-key 
		    (directory-file-name (file-name-directory filename)))))
	  (assoc filename (mhc-slot-records (mhc-slot-get-month-schedule key)))))))

(defun mhc-summary-folder-to-path (folder &optional msg)
  (let ((fld
	 (if (eq (string-to-char folder) ?+)
	     (substring mhc-base-folder 1) folder)))
    (if msg
	(format "%s/%s/%s" mhc-mail-path fld msg)
      (format "%s/%s" mhc-mail-path fld))))


;;; Codes:
(defun mhc-summary/insert-separator ()
  (let ((hr (make-string (- (window-width) 24) ?-)))
    (mhc-face-put hr 'mhc-summary-face-separator)
    (insert hr "\n")))


(defun mhc-summary/insert-strings (schedule xstrings icon mailer)
  (let (str)
    (mhc-summary-insert-contents
     schedule
     (mapconcat (lambda (xstr)
		  (if (consp xstr)
		      (if (cdr xstr)
			  (progn
			    (setq str (concat (car xstr)))
			    (mhc-face-put str (cdr xstr))
			    str)
			(car xstr))
		    xstr))
		xstrings
		"")
     icon
     mailer)))


;; Returns a cons cell of (XSTRING . ICON).
(defun mhc-summary/schedule-to-contents (schedule &optional conflict secret)
  (let ((category (car (mhc-schedule-categories schedule)))
	space icon use-icon)
    (if (setq use-icon (mhc-use-icon-p))
	(and category
	     (setq icon (mhc-get-icon category))))
    (cons
     (delq nil
	   (if (and secret (mhc-schedule-in-category-p schedule "private"))
	       ;; In the case when private schedules are hidden.
	       (list
		" "
		(make-string 11 ? )
		" "
		(if use-icon
		    (progn
		      ;; XX icon must have 2 character width.
		      (setq space "  ")
		      (put-text-property 0 2 'invisible
					 (and category icon t)
					 space)
		      (cons space nil)))
		(if conflict
		    (cons mhc-summary-string-conflict
			  'mhc-summary-face-conflict))
		(cons mhc-summary-string-secret
		      'mhc-summary-face-secret))
	     (list
	      " "
	      (cons (format "%-11s" (mhc-schedule-time-as-string schedule))
		    'mhc-summary-face-time)
	      " "
	      (if use-icon
		  (progn
		    ;; XX icon must have 2 character width.
		    (setq space "  ")
		    (put-text-property 0 2 'invisible
				       (and category icon t)
				       space)
		    (cons space nil)))
	      (if conflict
		  (cons mhc-summary-string-conflict
			'mhc-summary-face-conflict))
	      (cons (mhc-schedule-subject schedule)
		    (and category (mhc-face-category-to-face category)))
	      " "
	      (if (not (zerop (length (mhc-schedule-location schedule))))
		  (cons (concat "[" (mhc-schedule-location schedule) "]")
			'mhc-summary-face-location)))))
     icon)))



(defun mhc-summary/insert-dayinfo-internal
  (dayinfo schedule mailer &optional require-date-string conflict secret)
  (let (contents icon)
    (if schedule
	(progn
	  (setq contents (mhc-summary/schedule-to-contents
			  schedule conflict secret))
	  (setq icon (cdr contents))
	  (setq contents (car contents))))
    (let ((week-color (cond
		       ((mhc-schedule-in-category-p schedule "holiday")
			'mhc-category-face-holiday)
		       ((eq (mhc-day-day-of-week dayinfo) 0)
			'mhc-summary-face-sunday)
		       ((eq (mhc-day-day-of-week dayinfo) 6)
			'mhc-summary-face-saturday)))
	  date day-of-week)
      (if require-date-string
	  (setq date (cons (format "%02d/%02d"
				   (mhc-day-month dayinfo)
				   (mhc-day-day-of-month dayinfo))
			   (if (mhc-day-today-p dayinfo)
			       'mhc-summary-face-today
			     week-color))
		day-of-week (cons (mhc-day-day-of-week-as-string dayinfo)
				  week-color))
	(setq date (make-string 5 ? )
	      day-of-week (make-string 3 ? )))
      (mhc-summary/insert-strings
       schedule
       (cons date
	     (cons
	      " "
	      (cons day-of-week
		    (if mhc-face-week-color-paint-thick
			(if contents
			    (if week-color
				(mapcar (lambda (xstr)
					  (if (consp xstr)
					      (progn (setcdr xstr week-color) xstr)
					    (cons xstr week-color)))
					contents)
			      contents))
		      contents))))
       icon
       mailer))))


(defun mhc-summary/insert-dayinfo
  (dayinfo mailer category category-is-invert secret)
  (let ((schedules (mhc-day-schedules dayinfo)))
    (if schedules
	(let ((require-date-string t)
	      (time-max -1)
	      time-b1 time-e1 time-b2 conflict)
	  (while schedules
	    (if (or (null category)
		    (if category-is-invert
			(not (mhc-schedule-in-category-p (car schedules) category))
		      (mhc-schedule-in-category-p (car schedules) category)))
		(progn
		  (setq time-b1 (mhc-schedule-time-begin (car schedules))
			time-e1 (mhc-schedule-time-end (car schedules))
			time-b2 (if (car (cdr schedules))
				    (mhc-schedule-time-begin (car (cdr schedules))))
			conflict (or (and time-e1 time-b2 (< time-b2 time-e1))
				     (and time-b1 time-max (< time-b1 time-max))))
		  (if time-e1 (setq time-max (max time-e1 time-max)))
		  (mhc-summary/insert-dayinfo-internal dayinfo (car schedules) mailer
						       require-date-string conflict secret)
		  (setq require-date-string nil)))
	    (setq schedules (cdr schedules))))
      (mhc-summary/insert-dayinfo-internal dayinfo nil mailer t nil secret))))


(defun mhc-summary-make-contents
  (from to mailer &optional category category-is-invert secret)
  (let ((dayinfo-list (mhc-db-scan from to)))
    (while dayinfo-list
      (mhc-summary/insert-dayinfo
       (car dayinfo-list) mailer category category-is-invert secret)
      (and (eq (mhc-day-day-of-week (car dayinfo-list)) mhc-use-week-separator)
	   (mhc-summary/insert-separator))
      (setq dayinfo-list (cdr dayinfo-list)))))

(defcustom mhc-todo-string-remaining-day "(あと %d 日)"
  "*String format which is displayed in TODO entry.
'%d' is replaced with remaining day."
  :group 'mhc
  :type 'string)

(defcustom mhc-todo-string-deadline-day "(〆切日)"
  "*String which indicates deadline day in TODO."
  :group 'mhc
  :type 'string)

(defcustom mhc-todo-string-heading "TODO(s) at %04d/%02d/%02d"
  "*String which is displayed as heading of TODO.
First %d is replaced with year, second one is replaced with month,
third one is replaced with day of month."
  :group 'mhc
  :type 'string)

(defcustom mhc-todo-mergin 1
  "*Mergin line number between TODO and schedule."
  :group 'mhc
  :type 'integer)

(defun mhc-summary-make-todo-list
  (day mailer &optional category category-is-invert secret)
  (let ((schedules (mhc-db-scan-todo day))
	contents deadline)
    (if schedules
	(progn
	  (insert (make-string mhc-todo-mergin ?\n))
	  (mhc-summary/insert-separator)
	  (insert (mhc-day-let day
		    (format mhc-todo-string-heading
			    year month day-of-month))
		  "\n")
	  (while schedules
	    (if (or (null category)
		    (if category-is-invert
			(not (mhc-schedule-in-category-p
			      (car schedules) category))
		      (mhc-schedule-in-category-p (car schedules) category)))
		(progn
		  (setq deadline (mhc-schedule-todo-deadline (car schedules)))
		  (mhc-summary/insert-strings
		   (car schedules)
		   (cons (let ((lank (mhc-schedule-todo-lank (car schedules))))
			   (cons (format "%9s" (format "[%d]" lank))
				 (cond
				  ((>= lank 80) 'mhc-summary-face-sunday)
				  ((>= lank 50) 'mhc-summary-face-saturday))))
			 (append
			  (car (setq contents
				     (mhc-summary/schedule-to-contents
				      (car schedules) nil secret)))
			  (list (if deadline
				    (if (eq deadline day)
					(cons mhc-todo-string-deadline-day
					      'mhc-summary-face-sunday)
				      (format mhc-todo-string-remaining-day
					      (- deadline day)))
				  ""))))
		   (and contents (cdr contents))
		   mailer)))
	    (setq schedules (cdr schedules)))))))


(provide 'mhc-summary)

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

;;; mhc-summary.el ends here.
