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
;;     (mhc-foo-generate-summary-buffer DATE)
;;         Generate summary buffer of mailer, and change current
;;         buffer to it.  This function will be called at the top of
;;         mhc-scan-month.
;;
;;     (mhc-foo-insert-summary-contents INSERTER)
;;         Insert schedule with INSERTER.
;;
;;     (mhc-foo-summary-mode-setup DATE)
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

(require 'mhc-day)
(require 'mhc-compat)
(require 'mhc-schedule)
(require 'bytecomp)

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

(defcustom mhc-summary-line-format "%M%/%D %W %b%e %c%i%s %l"
  "*A format string for summary line of MHC.
It may include any of the following format specifications
which are replaced by the given information:

%Y The year of the line if first line of the day.
%M The month of the line if first line of the day.
%D The day of the line if first line of the day.
%W The weekday name of the line if first line of the day.
%b Begin time.
%e End time (includes '-').
%c Warning string for conflict (See also `mhc-summary-string-conflict').
%i The icon for the schedule.
%s The subject of the schedule.
%l The location of the schedule.

%/ A slash character if first line of the day.
"
  :group 'mhc
  :type 'string)

(defcustom mhc-summary-todo-line-format "     %L %i%s %l%d"
  "*A format string for summary todo line of MHC.
It may include any of the following format specifications
which are replaced by the given information:

%i The icon for the schedule.
%s The subject of the schedule.
%l The location of the schedule.
%L The lank of the schedule.
%d The deadline of the schedule.
(`mhc-todo-string-remaining-day' or `mhc-todo-string-deadline-day' is used)
"
  :group 'mhc
  :type 'string)

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

;;; Internal Variable:

(defconst mhc-summary-major-mode-alist
  '((mew-summary-mode  . mhc-mew)
    (mew-virtual-mode  . mhc-mew)
    (wl-folder-mode    . mhc-wl)
    (wl-summary-mode   . mhc-wl)
    (gnus-group-mode   . mhc-gnus)
    (gnus-summary-mode . mhc-gnus)))

;; Internal Variables which are bound while inserting line:
(defvar mhc-tmp-day-face nil "a face for the day.")
(defvar mhc-tmp-dayinfo  nil "a dayinfo for the day.")
(defvar mhc-tmp-schedule nil "a schedule structure.")
(defvar mhc-tmp-begin    nil "begin time.")
(defvar mhc-tmp-end      nil "end time.")
(defvar mhc-tmp-conflict nil "non-nil if conflicted schedule.")
(defvar mhc-tmp-first    nil "non-nil if first schedule.")
(defvar mhc-tmp-private  nil "non-nil if private display mode.")
;; For TODO.
(defvar mhc-tmp-day      nil "the day.")
(defvar mhc-tmp-lank     nil "a dayinfo for the day.")
(defvar mhc-tmp-deadline nil "a schedule structure.")

;; Inserter (internal variable)
(defvar mhc-summary/line-inserter nil)

(defvar mhc-summary-todo/line-inserter nil)

(defvar mhc-summary-line-format-alist
  '((?Y (mhc-summary/line-year-string)
	face mhc-tmp-day-face)
    (?/ (if mhc-tmp-first "/" " ")
	face mhc-tmp-day-face)
    (?M (mhc-summary/line-month-string)
	face mhc-tmp-day-face)
    (?D (mhc-summary/line-day-string)
	face mhc-tmp-day-face)
    (?W (mhc-summary/line-day-of-week-string)
	face mhc-tmp-day-face)
    (?b (if (or (and mhc-tmp-private
		     (string= (car (mhc-schedule-categories mhc-tmp-schedule))
			      "private"))
		(null mhc-tmp-begin))
	    (make-string 5 ? )
	  (format "%02d:%02d" (/ mhc-tmp-begin 60) (% mhc-tmp-begin 60)))
	face (quote mhc-summary-face-time))
    (?e (if (or (and mhc-tmp-private
		     (string= (car (mhc-schedule-categories mhc-tmp-schedule))
			      "private"))
		(null mhc-tmp-end))
	    (make-string 6 ? )
	  (format "-%02d:%02d" (/ mhc-tmp-end 60) (% mhc-tmp-end 60)))
	face (quote mhc-summary-face-time))
    (?c (if mhc-tmp-conflict
	    mhc-summary-string-conflict
	  "")
	face (quote mhc-summary-face-conflict))
    (?i nil icon (mhc-schedule-categories mhc-tmp-schedule))
    (?s (mhc-summary/line-subject-string)
	face (mhc-face-category-to-face 
	      (car (mhc-schedule-categories mhc-tmp-schedule))))
    (?l (mhc-summary/line-location-string)
	face (quote mhc-summary-face-location)))
  "An alist of format specifications that can appear in summary lines.
Each element is a list of following:
\(SPEC STRING-EXP PROP-TYPE PROP-VALUE\)
SPEC is a character for format specification.
STRING is an expression to get string to insert.
PROP-TYPE is one of the two symbols `face' or `icon'.
It indicates a type of the property to put on the inserted string.
PROP-VALUE is the property value correspond to PROP-TYPE.
")

(defvar mhc-summary-todo-line-format-alist
  '((?  " ")
    (?i nil icon (mhc-schedule-categories mhc-tmp-schedule))
    (?s (mhc-summary/line-subject-string)
	face (mhc-face-category-to-face 
	      (car (mhc-schedule-categories mhc-tmp-schedule))))
    (?l (mhc-summary/line-location-string)
	face (quote mhc-summary-face-location))
    (?L (format "%5s" (format "[%d]" mhc-tmp-lank))
	face (cond ((>= mhc-tmp-lank 80) (quote mhc-summary-face-sunday))
		   ((>= mhc-tmp-lank 50) (quote mhc-summary-face-saturday))))
    (?d (mhc-summary-todo/line-deadline-string)
	face (mhc-summary-todo/line-deadline-face)))
  "An alist of format specifications that can appear in todo lines.
Each element is a list of following:
\(SPEC STRING-EXP PROP-TYPE PROP-VALUE\)
SPEC is a character for format specification.
STRING is an expression to get string to insert.
PROP-TYPE is one of the two symbols `face' or `icon'.
It indicates a type of the property to put on the inserted string.
PROP-VALUE is the property value correspond to PROP-TYPE.
")

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

(defsubst mhc-summary-generate-buffer (date &optional mailer)
  "Generate buffer with summary mode of MAILER."
  (funcall (mhc-summary-get-function 'generate-summary-buffer mailer) date))

(defsubst mhc-summary-insert-contents (mhc-tmp-schedule
				       mhc-tmp-private
				       inserter
				       &optional mailer)
  (if (eq 'direct mailer)
      (mhc-summary-line-insert)
    (funcall (mhc-summary-get-function 'insert-summary-contents mailer)
	     inserter)))

(defsubst mhc-summary-search-date (date)
  "Search day in the current buffer."
  (goto-char (point-min))
  (while (and (not (eobp))
	      (not (eq (mhc-day-date
			(get-text-property (point) 'mhc-dayinfo)) date)))
    (goto-char (next-single-property-change (point) 'mhc-dayinfo))))

(defsubst mhc-summary-mode-setup (date &optional mailer)
  "Setup buffer as summary mode of MAILER."
  (funcall (mhc-summary-get-function 'summary-mode-setup mailer) date))

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


(defvar mhc-summary/today nil)

(defun mhc-summary/insert-dayinfo (mhc-tmp-dayinfo mailer category
						   category-is-invert secret)
  (let ((time-max -1)
	(schedules (mhc-day-schedules mhc-tmp-dayinfo))
	(mhc-tmp-first t)
	mhc-tmp-begin mhc-tmp-end
	mhc-tmp-location mhc-tmp-schedule
	mhc-tmp-conflict
	next-begin)
    (if schedules
	(while schedules
	  (if (or (null category)
		  (if category-is-invert
		      (not (mhc-schedule-in-category-p
			    (car schedules) category))
		    (mhc-schedule-in-category-p
		     (car schedules) category)))
	      (progn
		(setq mhc-tmp-begin (mhc-schedule-time-begin (car schedules))
		      mhc-tmp-end (mhc-schedule-time-end (car schedules))
		      next-begin (if (car (cdr schedules))
				     (mhc-schedule-time-begin
				      (car (cdr schedules))))
		      mhc-tmp-conflict (or (and mhc-tmp-end next-begin
						(< next-begin mhc-tmp-end))
					   (and mhc-tmp-begin time-max 
						(< mhc-tmp-begin time-max))))
		(if mhc-tmp-end (setq time-max (max mhc-tmp-end time-max)))
		(mhc-summary-insert-contents
		 (car schedules)
		 secret
		 'mhc-summary-line-insert
		 mailer)
		(setq mhc-tmp-first nil)))
	  (setq schedules (cdr schedules)))
      (mhc-summary-insert-contents nil secret
				   'mhc-summary-line-insert
				   mailer))))


(defun mhc-summary-make-contents
  (from to mailer &optional category category-is-invert secret)
  (let ((dayinfo-list (mhc-db-scan from to)))
    (setq mhc-summary/today (mhc-date-now))
    (while dayinfo-list
      (mhc-summary/insert-dayinfo
       (car dayinfo-list) mailer category category-is-invert secret)
      (and (eq (mhc-day-day-of-week (car dayinfo-list)) mhc-use-week-separator)
	   (mhc-summary/insert-separator))
      (setq dayinfo-list (cdr dayinfo-list)))))


(defun mhc-summary-make-todo-list
  (day mailer &optional category category-is-invert secret)
  (let ((schedules (mhc-db-scan-todo day))
	(mhc-tmp-day day))
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
		(mhc-summary-insert-contents
		 (car schedules)
		 secret
		 'mhc-summary-todo-line-insert
		 mailer))
	    (setq schedules (cdr schedules)))))))


(defmacro mhc-summary/line-insert (string)
  (` (and (, string) (insert (, string)))))


(defun mhc-summary/line-year-string ()
  (if mhc-tmp-first
      (format "%4d" (mhc-day-year mhc-tmp-dayinfo))
    (make-string 2 ? )))


(defun mhc-summary/line-month-string ()
  (if mhc-tmp-first
      (format "%02d" (mhc-day-month mhc-tmp-dayinfo))
    (make-string 2 ? )))


(defun mhc-summary/line-day-string ()
  (if mhc-tmp-first
      (format "%02d" (mhc-day-day-of-month mhc-tmp-dayinfo))
    (make-string 2 ? )))


(defun mhc-summary/line-day-of-week-string ()
  (if mhc-tmp-first
      (format "%s" (mhc-day-day-of-week-as-string mhc-tmp-dayinfo))
    (make-string 3 ? )))


(defun mhc-summary/line-subject-string ()
  (if (and mhc-tmp-private
	   (string= (car (mhc-schedule-categories mhc-tmp-schedule))
		    "private"))
      mhc-summary-string-secret
    (or (mhc-schedule-subject mhc-tmp-schedule) "")))


(defun mhc-summary/line-location-string ()
  (let ((location (mhc-schedule-location mhc-tmp-schedule)))
    (and location
	 (> (length location) 0)
	 (concat "[" location "]"))))


(defun mhc-summary-todo/line-deadline-string ()
  (and mhc-tmp-deadline
       (if (mhc-date= mhc-tmp-deadline mhc-tmp-day)
	   mhc-todo-string-deadline-day
	 (format mhc-todo-string-remaining-day 
		 (mhc-date- mhc-tmp-deadline mhc-tmp-day)))))


(defun mhc-summary-todo/line-deadline-face ()
  (and mhc-tmp-deadline
       (if (mhc-date= mhc-tmp-deadline mhc-tmp-day)
	   'mhc-summary-face-sunday
	 'default)))


(defun mhc-summary/line-parse-format (format spec-alist)
  (let ((f (mhc-string-to-char-list format))
	inserter entry)
    (setq inserter (list 'let (list 'pos)))
    (while f
      (if (eq (car f) ?%)
	  (progn
	    (setq f (cdr f))
	    (if (eq (car f) ?%)
		(setq inserter (append inserter (list (list 'insert ?%))))
	      (setq entry (assq (car f) spec-alist))
	      (unless entry
		(error "Unknown format spec %%%c" (car f)))
	      (setq inserter
		    (append inserter
			    (list (list 'setq 'pos (list 'point)))
			    (list (list 'mhc-summary/line-insert
					(nth 1 entry)))
			    (and
			     (nth 2 entry)
			     (list
			      (append (cond
				       ((eq (nth 2 entry) 'face)
					(list 'put-text-property
					      'pos (list 'point)
					      (list 'quote 'face)
					      (nth 3 entry)))
				       ((eq (nth 2 entry) 'icon)
					(list 'and (list 'mhc-use-icon-p)
					      (list 'mhc-put-icon
						    (nth 3 entry))))))))))))
	(setq inserter (append inserter (list (list 'insert (car f))))))
      (setq f (cdr f)))
    inserter))


(defmacro mhc-summary/line-inserter-setup-internal (inserter format alist)
  (` (let (byte-compile-warnings)
       (setq (, inserter)
	     (byte-compile
	      (list 'lambda ()
		    (mhc-summary/line-parse-format (, format) (, alist)))))
       (when (get-buffer "*Compile-Log*")
	 (bury-buffer "*Compile-Log*"))
       (when (get-buffer "*Compile-Log-Show*")
	     (bury-buffer "*Compile-Log-Show*")))))


(defun mhc-summary-line-inserter-setup ()
  "Setup MHC summary and todo line inserter."
  (interactive)
  (mhc-summary/line-inserter-setup-internal
   mhc-summary/line-inserter
   mhc-summary-line-format
   mhc-summary-line-format-alist)
  (mhc-summary/line-inserter-setup-internal
   mhc-summary-todo/line-inserter
   mhc-summary-todo-line-format
   mhc-summary-todo-line-format-alist))
  

(defun mhc-summary-line-insert ()
  "Insert summary line."
  (let ((mhc-tmp-day-face (cond
			   ((mhc-schedule-in-category-p
			     mhc-tmp-schedule "holiday")
			    'mhc-category-face-holiday)
			   ((eq (mhc-day-day-of-week 
				 mhc-tmp-dayinfo) 0)
			    'mhc-summary-face-sunday)
			   ((eq (mhc-day-day-of-week mhc-tmp-dayinfo) 6)
			    'mhc-summary-face-saturday)))
	(pos (point)))
    (funcall mhc-summary/line-inserter)
    (put-text-property pos (point) 'mhc-dayinfo mhc-tmp-dayinfo)))


(defun mhc-summary-todo-line-insert ()
  "Insert todo line."
  (let ((mhc-tmp-lank (mhc-schedule-todo-lank mhc-tmp-schedule))
	(mhc-tmp-deadline (mhc-schedule-todo-deadline mhc-tmp-schedule)))
    (funcall mhc-summary-todo/line-inserter)))


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
