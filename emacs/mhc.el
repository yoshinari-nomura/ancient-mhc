;;; mhc.el -- MH Calendar.

;; Author:  Yoshinari Nomura <nom@quickhack.net>
;;
;; Created: 1994/07/04
;; Revised: $Date: 2000/06/19 04:06:13 $

;;;
;;; Commentay:
;;;

;; Mhc is the personal schedule management package cooperating
;;  with Mew, Wanderlust or Gnus.
;;
;; Minimum setup:
;;
;; for Mew user:
;;   (autoload 'mhc-mew-setup "mhc-mew")
;;   (add-hook 'mew-init-hook 'mhc-mew-setup)
;;
;; for Wanderlust user:
;;   (autoload 'mhc-wl-setup "mhc-wl")
;;   (add-hook 'wl-init-hook 'mhc-wl-setup)
;;
;; for Gnus user:
;;   (autoload 'mhc-gnus-setup "mhc-gnus")
;;   (add-hook 'gnus-startup-hook 'mhc-gnus-setup)

(require 'mhc-vars)
(require 'mhc-record)
(require 'mhc-file)
(require 'mhc-db)
(require 'mhc-misc)
(require 'mhc-date)
(require 'mhc-guess)
(require 'mhc-schedule)
(require 'mhc-minibuf)
(require 'mhc-face)
(require 'mhc-calendar)

(cond
 ((eval-when-compile
    (condition-case nil
	(require 'bitmap)
      (error nil)))
  (require 'mhc-bm))
 ((eval-when-compile (featurep 'xemacs))
  (require 'mhc-xmas))
 (t (defun mhc-use-icon-p ())))
  
(require 'mhc-summary)
(provide 'mhc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menu setup
;;
(defvar mhc-mode-menu-spec
      '("Mhc"
	["This month"   mhc-goto-this-month t]
	["Next month"   mhc-goto-next-month t]
	["Prev month"   mhc-goto-prev-month t]
	["Goto month"   mhc-goto-month t]
	["Import"       mhc-import t]
	["Set category" mhc-set-default-category t]
	"----"
	["Goto today"   mhc-goto-today (mhc-summary-buffer-p)]
	["Modify"       mhc-modify (mhc-summary-buffer-p)]
	["Edit"         mhc-edit (mhc-summary-buffer-p)]
	["Rescan"       mhc-rescan-month (mhc-summary-buffer-p)]
	["Delete"       mhc-delete (mhc-summary-buffer-p)]
	["Insert Schedule" mhc-insert-schedule (not buffer-read-only)]
	["3 months Mini calendar" mhc-calendar t]
	["Toggle 3 months calendar" mhc-cal-toggle-insert-rectangle
	 (mhc-summary-buffer-p)]))

(defvar mhc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-cg" 'mhc-goto-month)
    (define-key map "\C-c." 'mhc-goto-this-month)
    (define-key map "\C-cn" 'mhc-goto-next-month)
    (define-key map "\C-cp" 'mhc-goto-prev-month)
    (define-key map "\C-cf" 'mhc-goto-today)
    (define-key map "\C-c|" 'mhc-import)
    (define-key map "\C-cm" 'mhc-modify)
    (define-key map "\C-ce" 'mhc-edit)
    (define-key map "\C-cs" 'mhc-rescan-month)
    (define-key map "\C-cd" 'mhc-delete)
    (define-key map "\C-cc" 'mhc-set-default-category)
    (define-key map "\C-ci" 'mhc-insert-schedule)
    (define-key map "\C-c?" 'mhc-calendar)
    (define-key map "\C-ct" 'mhc-cal-toggle-insert-rectangle)
    map)
  "Keymap for `mhc-mode'.")

(defvar mhc-mode nil "Non-nil when in mhc-mode.")

(defcustom mhc-mode-hook nil
  "Hook run in when entering MHC mode."
  :group 'mhc
  :type 'hook)

;; Avoid warning of byte-compiler.
(defvar mhc-mode-menu)

(defun mhc-mode (&optional arg) "\
   MHC is the mode for registering schdule directly from email.  
   Requres Mew or Wanderlust or Gnus.

   Key assinment on mhc-mode.

   C-c .  Review the schedule of this month
   C-c n  Review the schedule of next month
   C-c p  Review the schedule of previous month
   C-c g  Jump to your prefer month
   C-c s  Rescan the buffer of the month
   C-c f  Move cursor to today (Only available reviewing this month)
   C-c |  Register the reviewing mail to schdule
   C-c d  Delete the schdule on the cursor line
   C-c m  Edit the schdule on the cursor line
   C-c e  Create new schdule file
   C-c c  Change default category
   C-c ?  Display 3 months mini calendar
   C-c t  Toggle 3 months calendar

   C-u prefix is available on using C-cs C-c. C-cg, it works to
   assign the category (see below)

   The prefix arg C-cn C-cp is also available and you can indicate
   the number of months to forward/back.

   Field names using by MHC.

   X-SC-Category: 
   Space-seperated Keywords. You can set default category to scan.
   You can also indicate keywords by typing C-cs C-c. C-cg with C-u.
"
  (interactive "P")
  (make-local-variable 'mhc-mode)
  (setq mhc-mode
	(if (null arg)
	    (not mhc-mode)
	  (> (prefix-numeric-value arg) 0)))
  (if (featurep 'xemacs) (easy-menu-add mhc-mode-menu))
  (force-mode-line-update)
  (run-hooks 'mhc-mode-hook))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; category
;;

(defvar mhc-default-category nil)

(defun mhc-set-default-category ()
  (interactive)
  (setq mhc-default-category (mhc-input-category "Default Category: "
			      mhc-default-category)))

(defun mhc-category-convert (lst)
  (let (ret inv)
    ;; preceding `!' means invert logic.
    (if (and lst (string-match "^!" (car lst)))
	(setq lst (cons (substring (car lst) (match-end 0)) (cdr lst))
	      inv t))
    (cons inv lst)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; goto-*

(defvar mhc-month-hist nil)

(defun mhc-input-month (prompt)
  (let ((ret nil)
	(month-str (mhc-date-format (mhc-date-now) "%04d/%02d" yy mm)))
    (while (null ret)
      (setq month-str 
	    (read-from-minibuffer
	     (concat prompt "(yyyy/mm) : ") month-str nil nil 'mhc-month-hist))
      (if (string-match "\\([0-9]+\\)/\\([0-9]+\\)" month-str)
	  (setq ret (mhc-date-new
		     (string-to-number (match-string 1 month-str))
		     (string-to-number (match-string 2 month-str))
		     1 t))))
    ret))

(defun mhc-goto-month (&optional date hide-private)
  "*Show schedules of specified month.
If HIDE-PRIVATE, priavate schedules are suppressed."
  (interactive
   (list
    (mhc-input-month "Month ")
    (if mhc-default-hide-private-schedules
	(not current-prefix-arg)
      current-prefix-arg)))
  (let ((category (mhc-category-convert mhc-default-category)))
    (mhc-scan-month date
		    (mhc-summary-mailer-type)
		    (cdr category)
		    (car category)
		    hide-private)))

(defun mhc-goto-this-month (&optional hide-private)
  "*Show schedules of this month.
If HIDE-PRIVATE, private schedules are suppressed."
  (interactive
   (list
    (if mhc-default-hide-private-schedules
	(not current-prefix-arg)
      current-prefix-arg)))
  (mhc-goto-month (mhc-date-now) hide-private))

(defun mhc-goto-next-month (&optional arg)
  (interactive "p")
  (mhc-goto-month (mhc-date-mm+ (or (mhc-current-date-month) (mhc-date-now)) arg)
		  mhc-default-hide-private-schedules))

(defun mhc-goto-prev-month (&optional arg)
  (interactive "p")
  (mhc-goto-next-month (- arg)))

(defun mhc-goto-today (&optional no-display)
  "*Go to the line of today's schedule.
Unless NO-DISPLAY, display it."
  (interactive "P")
  (let ((now (mhc-date-now))
	(buf-date (mhc-current-date-month)))
    (goto-char (point-min))
    (mhc-date-let now
      (and 
       (= yy (mhc-date-yy buf-date))
       (= mm (mhc-date-mm buf-date))
       (mhc-summary-search-day yy mm dd)
       (progn 
	 (forward-line 0)
	 (or (pos-visible-in-window-p (point))
	     (recenter))
	 (or no-display
	     (mhc-summary-display-article)))))))

(defun mhc-rescan-month (&optional hide-private)
  "*Rescan schedules of this buffer.
If HIDE-PRIVATE, private schedules are suppressed."
  (interactive
   (list
    (if mhc-default-hide-private-schedules
	(not current-prefix-arg)
      current-prefix-arg)))
  (let ((category (mhc-category-convert mhc-default-category))
	(line (+ (count-lines (point-min) (point))
		 (if (= (current-column) 0) 1 0))))
    (mhc-scan-month (mhc-current-date-month)
		    (mhc-summary-mailer-type)
		    (cdr category)
		    (car category)
		    hide-private)
    (goto-line line)
    (beginning-of-line)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make scan form.

(defvar mhc-face-week-color-paint-thick nil)

(defvar mhc-use-week-separator 6
  "*if number 0 .. 6, insert separator in summary buffer.")

(defun mhc-scan-month (date mailer cat inv-cat secret)
  (let ((from  (mhc-date-mm-first date))
	(to    (mhc-date-mm-last date))
	(today (mhc-date-now)))
    (or (eq 'direct mailer)
	(mhc-summary-generate-buffer date mailer))
    (message (mhc-date-format date "Scanning %04d/%02d ..." yy mm))
    (mhc-summary-make-contents from to mailer cat inv-cat secret)
    (or (eq 'direct mailer)
	(progn
	  (if mhc-insert-todo-list
	      (mhc-summary-make-todo-list (if (and (>= today from) (<= today to))
					      today
					    from)
					  mailer cat inv-cat secret))
	  (if mhc-insert-calendar
	      (mhc-cal-insert-rectangle-at date (- (window-width) 24))) ;; xxx
	  (mhc-goto-today t)
	  (mhc-summary-mode-setup date mailer)
	  (mhc-mode 1)
	  (setq inhibit-read-only nil)
	  (setq buffer-read-only t)))
    (message (mhc-date-format date "Scanning %04d/%02d ... done." yy mm))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; import, edit, delete, modify

(defun mhc-edit (&optional import-buffer calendar)
  "Edit a new schedule.
If optional argument IMPORT-BUFFER is specified, import its content.
Returns t if the importation was succeeded."
  (interactive
   (if current-prefix-arg
       (list (get-buffer (read-buffer "Import buffer: "
				      (current-buffer))))))
  (let ((draft-buffer (generate-new-buffer mhc-draft-buffer-name))
	(current-date (if calendar (mhc-calendar-get-ddate) (mhc-current-date)))
	(succeed t)
	date time subject location category)
    (and (interactive-p)
	 (mhc-window-push))
    (set-buffer draft-buffer)
    (if import-buffer
	(progn
	  (insert-buffer import-buffer)
	  (mhc-header-narrowing
	    (mhc-header-delete-header (concat "^\\("
					      (mhc-regexp-opt mhc-draft-unuse-hdr-list)
					      "\\)")
				      'regexp))
	  (switch-to-buffer draft-buffer t)))
    (condition-case ()
	(if import-buffer
	    (progn
	      (delete-other-windows)
	      (if (y-or-n-p "Do you want to import this article? ")
		  (let* ((original (save-excursion
				     (set-buffer import-buffer)
				     (mhc-parse-buffer)))
			 (schedule (car (mhc-record-schedules original))))
		    ;; input date
		    (setq date
			  (mhc-input-day "Date: "
					 current-date
					 (gdate-guess-date)))
		    ;; input time
		    (setq time
			  (mhc-input-time "Time: "
					  (mhc-schedule-time-as-string schedule)
					  (gdate-guess-time
					   (mhc-minibuf-candidate-nth-begin))))
		    ;; input subject
		    (setq subject
			  (mhc-input-subject "Subject: "
					     (mhc-misc-sub
					      (or (mhc-record-subject original)
						  (mhc-header-narrowing
						    (mhc-header-get-value "subject")))
					      "^\\(Re:\\)? *\\(\\[[^\]]+\\]\\)? *"
					      "")))
		    ;; input location
		    (setq location
			  (mhc-input-location "Location: "
					      (mhc-schedule-location schedule)))
		    ;; input category
		    (setq category
			  (mhc-input-category "Category: "
					      (mhc-schedule-categories-as-string schedule)))
		    (mhc-header-narrowing
		      (mhc-header-delete-header (concat "^\\("
							(mhc-regexp-opt (mhc-header-list))
							"\\)")
						'regexp)))
		;; Answer was no.
		(message "") ; flush minibuffer.
		(and (interactive-p)
		     (mhc-window-pop))
		(setq succeed nil)
		(kill-buffer draft-buffer)))
	  ;; No import (it succeeds).
	  (setq date (mhc-input-day "Date: " current-date)
		time (mhc-input-time "Time: ")
		subject (mhc-input-subject "Subject: ")
		location (mhc-input-location "Location: ")
		category (mhc-input-category "Category: ")))
      ;; Quit.
      (quit 
       (and (interactive-p)
	    (mhc-window-pop))
       (setq succeed nil)
       (kill-buffer draft-buffer)))
    (if succeed
	(progn
	  (switch-to-buffer draft-buffer t)
	  (goto-char (point-min))
	  (insert "X-SC-Subject: " subject
		  "\nX-SC-Location: " location
		  "\nX-SC-Day: " (mapconcat
				  (lambda (day)
				    (mhc-date-format day "%04d%02d%02d" yy mm dd))
				  date " ")
		  "\nX-SC-Time: " (if time
				      (let ((begin (car time))
					    (end (nth 1 time)))
					(concat (if begin (mhc-time-to-string begin) "")
						(if end (concat "-" (mhc-time-to-string end)) "")))
				    "")
		  "\nX-SC-Category: " (mapconcat (function capitalize) category " ")
		  "\nX-SC-Cond: "
		  "\nX-SC-Duration: "
		  "\nX-SC-Alarm: "
		  "\nX-SC-Record-Id: " (mhc-record-create-id) "\n")
	  (unless import-buffer
	    (goto-char (point-max))
	    (insert "----\n"))
	  (mhc-draft-mode)
	  succeed))))

(defun mhc-import (&optional get-original)
  (interactive "P")
  (mhc-window-push)
  (unless (mhc-edit (mhc-summary-get-import-buffer get-original))
    ;; failed.
    (mhc-window-pop)))

(defun mhc-delete ()
  (interactive)
  (mhc-delete-file (mhc-summary-record)))

(defun mhc-delete-file (record)
  (interactive)
  (if (not (and record (file-exists-p (mhc-record-name record))))
      (message "File does not exist (%s)." (mhc-record-name record))
    (if (not (y-or-n-p (format "Do you delete %s ?" (mhc-record-subject-as-string record))))
	(message "Never mind..")
      (if (and
	   (mhc-record-occur-multiple-p record)
	   (not (y-or-n-p (format
			   "%s has multiple occurrences. Delete all(=y) or one(=n) ?"
			   (mhc-record-subject-as-string record)))))
	  (mhc-db-add-exception-rule record (or (mhc-current-date)
						(and (eq major-mode 'mhc-calendar-mode)
						     mhc-calendar-view-ddate)))
	(mhc-db-delete-file record))
      (or (and (mhc-summary-buffer-p)
	       (mhc-rescan-month mhc-default-hide-private-schedules))
	  (and (eq major-mode 'mhc-calendar-mode) (mhc-calendar-rescan))))))

(defun mhc-modify ()
  (interactive)
  (mhc-modify-file (mhc-summary-filename)))

(defun mhc-modify-file (file)
  (if (and (stringp file) (file-exists-p file))
      (let* ((name (format
		    "*mhc draft %s/%s*"
		    mhc-base-folder
		    (file-relative-name
		     file
		     (file-name-as-directory
		      (mhc-summary-folder-to-path mhc-base-folder)))))
	     (buffer (get-buffer name)))
	(if (buffer-live-p buffer)
	    (progn
	      (message "Specified file(%s) has already been opened.")
	      (switch-to-buffer-other-window buffer))
	  (mhc-window-push)
	  (set-buffer (setq buffer (get-buffer-create name)))
	  (mhc-insert-file-contents-as-coding-system mhc-default-coding-system file)
	  (set-buffer-modified-p nil)
	  (switch-to-buffer-other-window buffer)
	  (mhc-draft-mode)
	  (set (make-local-variable 'mhc-draft-buffer-file-name) file)))
    (message "Specified file(%s) does not exist." file)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; draft
;;

(defconst mhc-draft-buffer-name "*mhc draft*")

(defcustom mhc-draft-unuse-hdr-list
  '(">From " "From " "Delivered-To:" "Errors-To:" "Lines:"
    "Posted:" "Precedence:" "Received:" "Replied:" "Return-Path:"
    "Sender:" "User-Agent:" "X-Dispatcher:" "X-Filter:"
    "X-Gnus-Mail-Source:" "X-Mailer:" "X-Received:" "X-Sender:"
    "X-Seqno:" "Xref:")
  "*These headers are removed when article is imported."
  :group 'mhc
  :type '(repeat string))

(defcustom mhc-draft-mode-hook nil
  "*Hook run in mhc draft mode buffers."
  :group 'mhc
  :type 'hook)

;; Avoid warning of byte-compiler.
(defvar mhc-draft-buffer-file-name nil)

(defvar mhc-draft-mode-map)

(define-derived-mode mhc-draft-mode
  text-mode
  "MHC-Draft"
  "Major mode for editing schdule files of MHC.
Like Text Mode but with these additional commands:
C-c C-c  mhc-draft-finish
C-c C-k  mhc-draft-kill
C-c C-q  mhc-draft-kill
C-c ?    mhc-draft-insert-calendar
.
"
  (define-key mhc-draft-mode-map "\C-c\C-c" 'mhc-draft-finish)
  (define-key mhc-draft-mode-map "\C-c\C-q" 'mhc-draft-kill)
  (define-key mhc-draft-mode-map "\C-c\C-k" 'mhc-draft-kill)
  (define-key mhc-draft-mode-map "\C-c?" 'mhc-draft-insert-calendar)
  (make-local-variable 'adaptive-fill-regexp)
  (setq adaptive-fill-regexp
	(concat "[ \t]*[-a-z0-9A-Z]*\\(>[ \t]*\\)+[ \t]*\\|"
		adaptive-fill-regexp))
  (unless (boundp 'adaptive-fill-first-line-regexp)
    (setq adaptive-fill-first-line-regexp nil))
  (make-local-variable 'adaptive-fill-first-line-regexp)
  (setq adaptive-fill-first-line-regexp
	(concat "[ \t]*[-a-z0-9A-Z]*\\(>[ \t]*\\)+[ \t]*\\|"
		adaptive-fill-first-line-regexp))
  (set (make-local-variable 'indent-tabs-mode) nil))

(defun mhc-draft-kill (&optional no-confirm)
  (interactive "P")
  (if (or no-confirm (y-or-n-p "Kill draft buffer ?"))
      (progn
	(setq mhc-calendar-separator nil)
	(setq mhc-calendar-call-buffer nil)
	(kill-buffer (current-buffer))
	(mhc-window-pop))))

(defvar mhc-draft-finish-hook nil
  "Hook run after mhc-draft-finish.")

(defun mhc-draft-finish ()
  (interactive)
  (let ((record (mhc-parse-buffer (mhc-record-new mhc-draft-buffer-file-name))))
    (setq mhc-calendar-separator nil)
    (setq mhc-calendar-call-buffer nil)
    (mhc-header-delete-separator)
    (if (mhc-db-add-record-from-buffer record (current-buffer))
	(progn
	  (kill-buffer (current-buffer))
	  (mhc-window-pop)
	  (or (and (mhc-summary-buffer-p)
		   (mhc-rescan-month mhc-default-hide-private-schedules))
	      (and (eq major-mode 'mhc-calendar-mode) (mhc-calendar-rescan)))
	  (run-hooks 'mhc-draft-finish-hook)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; input x-sc- schedule data from minibuffer.

(defconst mhc-input-time-regex "^\\([0-9]+\\):\\([0-9]+\\)$")

(defvar mhc-subject-hist nil)

(defun mhc-input-subject (&optional prompt default)
  (interactive)
  (read-from-minibuffer  (or prompt "Subject: ")
			 (or default "")
			 nil nil 'mhc-subject-hist))

(defvar mhc-location-hist nil)

(defun mhc-input-location (&optional prompt default)
  (interactive)
  (read-from-minibuffer  (or prompt "Location: ")
			 (or default "")
			 nil nil 'mhc-location-hist))

(defvar mhc-category-hist nil)

(defun mhc-input-category (&optional prompt default)
  (interactive)
  (let (in)
    (and default
	 (listp default)
	 (setq default (mapconcat 'identity default " ")))
    (if (string= "" (setq in (read-from-minibuffer 
			      (or prompt "Category: ")
			      (or default "")
			      nil nil 'mhc-category-hist)))
	nil
      (mhc-misc-split in))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Window stack
;;

(defvar mhc-window-stack       nil)

(defun mhc-window-push ()
  (interactive)
  (setq mhc-window-stack
	(cons (current-window-configuration) mhc-window-stack)))

(defun mhc-window-pop ()
  (interactive)
  (if mhc-window-stack
      (set-window-configuration (car-safe mhc-window-stack)))
  (setq mhc-window-stack (cdr-safe mhc-window-stack)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; (Category . (parent-face fg bg))
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; manipulate data from mhc-summary-buffer.

(defconst mhc-summary-day-regex  "\\([^|]+| +\\)?[0-9]+/\\([0-9]+\\)")
(defconst mhc-summary-buf-regex   
  (concat mhc-base-folder "/\\([0-9]+\\)/\\([0-9]+\\)"))

(defun mhc-summary-buffer-p (&optional buffer)
  (string-match mhc-summary-buf-regex 
		(buffer-name 
		 (or buffer (current-buffer)))))

(defun mhc-current-date ()
  (let ((buf (buffer-name)) yy mm dd)
    (if (not (string-match mhc-summary-buf-regex buf))
	nil
      (setq yy (string-to-number (match-string 1 buf))
	    mm (string-to-number (match-string 2 buf)))
      (save-excursion
	(forward-line 0)
	(while (if (looking-at mhc-summary-day-regex)
		   (progn
		     (setq dd (string-to-number (match-string 2)))
		     nil) ;; exit loop.
		 (not (bobp)))
	  (forward-line -1))
	(if dd (mhc-date-new yy mm dd) nil)))))

(defun mhc-current-date-month ()
  (let ((buf (buffer-name)) yy mm dd)
    (if (not (string-match mhc-summary-buf-regex buf))
	nil
      (mhc-date-new (string-to-number (match-string 1 buf))
		    (string-to-number (match-string 2 buf))
		    1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make rectangle like calendar.el

(defun mhc-cal-toggle-insert-rectangle ()
  (interactive)
  (setq mhc-insert-calendar (not mhc-insert-calendar))
  (when (mhc-summary-buffer-p)
    (mhc-rescan-month mhc-default-hide-private-schedules)))

(defconst mhc-cal-week-header "| Su Mo Tu We Th Fr Sa")

(defun mhc-cal-insert-rectangle-at (date col)
  (save-excursion
    (put-text-property (point-min) (point-max) 'rear-nonsticky t)
    (goto-char (point-min))
    (mhc-misc-move-to-column col)
    (mhc-misc-insert-rectangle
     (nconc (mhc-cal-make-rectangle (mhc-date-mm-- date))
	    (list "| ")
	    (mhc-cal-make-rectangle date)
	    (list "| ")
	    (mhc-cal-make-rectangle (mhc-date-mm++ date))))))

(defun mhc-cal-make-rectangle (&optional date)
  (interactive)
  (let* ((today (mhc-date-now))
	 (days (mhc-db-scan-month (mhc-date-yy (or date today)) (mhc-date-mm (or date today)) t))
	 (month (list mhc-cal-week-header
		      (mhc-date-format (or date today)
				       "|    %s %04d"
				       (mhc-date-digit-to-mm-string mm t)
				       yy)))
	 (i (mhc-day-day-of-week (car days)))
	 week color)
    (while (> i 0)
      (setq week (cons "  " week)
	    i (1- i)))
    (while days
      (setq color
	    (cond
	     ((= 0 (mhc-day-day-of-week (car days))) 'mhc-calendar-face-sunday)
	     ((mhc-day-holiday (car days)) (mhc-face-category-to-face "Holiday"))
	     ((= 6 (mhc-day-day-of-week (car days))) 'mhc-calendar-face-saturday)
	     (t 'default)))
      (and (= (mhc-date-dd today) (mhc-day-day-of-month (car days)))
	   (= (mhc-date-mm today) (mhc-day-month (car days)))
	   (= (mhc-date-yy today) (mhc-day-year (car days)))
	   (setq color (mhc-face-get-gray-face color)))
      (if (mhc-day-busy-p (car days))
	  (setq color (mhc-face-get-underline-face color)))
      (setq week (cons (format "%2d" (mhc-day-day-of-month (car days)))
		       week))
      (if color
	  (mhc-face-put (car week) color))
      (if (= 6 (mhc-day-day-of-week (car days)))
	  (setq month (cons (mapconcat
			     (function identity)
			     (cons "|" (nreverse week))
			     " ")
			    month)
		week nil))
      (setq days (cdr days)))
    (if week
	(setq month (cons (mapconcat
			   (function identity)
			   (cons "|" (nreverse week))
			   " ")
			  month)))
    (nreverse month)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc.

;;
;; Convinient function when you want to insert your schedule into an
;; editing buffer.
;;
(defun mhc-insert-schedule (&optional hide-private)
  (interactive "P")
  (set-mark (point))
  (let ((category (mhc-category-convert mhc-default-category)))
    (mhc-scan-month (mhc-input-month "Month ")
		    'direct ;; insert into current buffer.
		    (cdr category)
		    (car category)
		    hide-private))
  (exchange-point-and-mark))

(defvar mhc-date-hist nil)

(defun mhc-input-date (&optional prompt default)
  (interactive)
  (let* ((date (or default (mhc-date-now)))
	 yy mm in dlst tlst tstr dstr ret err)
    ;; input date string.
    (setq in (read-from-minibuffer
	      (concat (or prompt "") " (yyyy/mm/dd) : ")
	      (mhc-date-format date "%04d/%02d/%02d" yy mm dd)
	      nil nil 'mhc-date-hist))
    ;; check format
    (if (not (string-match
	      "\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\)"
	      in))
	nil
      (mhc-date-new (string-to-number (match-string 1 in))
		    (string-to-number (match-string 2 in))
		    (string-to-number (match-string 3 in))))))

(defun mhc-view-file ()
  "View the schedule on the current line in View mode in another window."
  (interactive)
  (let ((path (mhc-summary-filename)))
    (view-file-other-window path)))


;;; Temporary buffers

(defvar mhc-tmp-buffer-list nil)

(defun mhc-get-buffer-create (name)
  "Return buffer for temporary use of MHC."
  (let ((buf (get-buffer name)))
    (or (and buf (buffer-name buf))
	(progn
	  (setq buf (get-buffer-create name)
		mhc-tmp-buffer-list (cons buf mhc-tmp-buffer-list))
	  (buffer-disable-undo buf)))
    buf))

(defun mhc-kill-all-buffers ()
  "Kill all buffers for temporary use of MHC."
  (while mhc-tmp-buffer-list
    (if (buffer-name (car mhc-tmp-buffer-list))
	(kill-buffer (car mhc-tmp-buffer-list)))
    (setq mhc-tmp-buffer-list
	  (cdr mhc-tmp-buffer-list))))


;;; Setup and exit

(defcustom mhc-setup-hook nil
  "Run hook after mhc-setup."
  :group 'mhc
  :type 'hook)

(defvar mhc-setup-p nil)

(defun mhc-setup ()
  (unless mhc-setup-p
    (condition-case nil
	(progn
	  (or (featurep 'easymenu) (require 'easymenu))
	  (easy-menu-define mhc-mode-menu
			    mhc-mode-map
			    "Menu used in mhc mode." 
			    mhc-mode-menu-spec)
	  (easy-menu-define mhc-calendar-mode-menu
			    mhc-calendar-mode-map
			    "Menu used in mhc calendar mode."
			    mhc-calendar-mode-menu-spec))
      (error nil))
    (or (assq 'mhc-mode minor-mode-alist)
	(setq minor-mode-alist
	      (cons (list 'mhc-mode (mhc-file-line-status))
		    minor-mode-alist)))
    (or (assq 'mhc-mode minor-mode-map-alist)
	(setq minor-mode-map-alist 
	      (cons (cons 'mhc-mode mhc-mode-map)
		    minor-mode-map-alist)))
    (mhc-face-setup)
    (put-text-property 2 4 'face 'mhc-calendar-face-sunday  mhc-cal-week-header)
    (put-text-property 20 22 'face 'mhc-calendar-face-saturday mhc-cal-week-header)
    (mhc-file-setup)
    (and (mhc-use-icon-p) (mhc-icon-setup))
    (and mhc-calendar-link-hnf (mhc-calendar-hnf-face-setup))
    (autoload 'mhc-ps-preview "mhc-ps" "*Preview PostScript calendar." t)
    (autoload 'mhc-ps-print "mhc-ps" "*Print PostScript calendar." t)
    (setq mhc-setup-p t)
    (run-hooks mhc-setup-hook)))
  
(defcustom mhc-exit-hook nil
  "Run hook after mhc-exit."
  :group 'mhc
  :type 'hook)

(defun mhc-exit ()
  (setq mhc-setup-p nil)
  (mhc-file-exit)
  (mhc-slot-clear-cache)
  (mhc-kill-all-buffers)
  (run-hooks mhc-exit-hook))


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

;;; mhc.el ends here
