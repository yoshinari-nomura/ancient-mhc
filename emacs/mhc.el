;;; mhc.el -- MH Calendar.

;; Author:  Yoshinari Nomura <nom@mew.org>
;;
;; Created: 1994/07/04
;; Revised: $Date: 2000/06/12 15:09:45 $

;;;
;;; Commentay:
;;;

;; Mhc is the personal schedule management package cooperating
;;  with Mew, Wanderlust or Gnus.
;;
;; Minimum setup:
;;
;; for Mew user:
;;   (setq mhc-mailer-package 'mew)
;;   (autoload 'mhc-mode "mhc" nil t)
;;   (add-hook 'mew-summary-mode-hook 'mhc-mode)
;;   (add-hook 'mew-virtual-mode-hook 'mhc-mode)
;;   (add-hook 'mew-message-hook      'mhc-misc-hdr-decode)
;;
;; for Wanderlust user:
;;   (setq mhc-mailer-package 'wl)
;;   (autoload 'mhc-mode "mhc" nil t)
;;   (add-hook 'wl-summary-mode-hook 'mhc-mode)
;;   (add-hook 'wl-folder-mode-hook 'mhc-mode)
;;
;; for Gnus user:
;;   (setq mhc-mailer-package 'gnus)
;;   (autoload 'mhc-mode "mhc" nil t)
;;   (add-hook 'gnus-summary-mode-hook 'mhc-mode)
;;   (add-hook 'gnus-group-mode-hook 'mhc-mode)

(require 'mhc-misc)
(require 'mhc-date)
(require 'mhc-guess)
(require 'mhc-schedule)
(require 'mhc-minibuf)
(require 'mhc-face)
(provide 'mhc)

(defconst mhc-version "mhc version 0.25pre4")

(if (fboundp 'defgroup)
    (defalias 'mhc-defgroup 'defgroup)
  (defmacro mhc-defgroup (symbol members doc &rest args) nil))
(put 'mhc-defgroup 'lisp-indent-function 'defun)

(if (fboundp 'defcustom)
    (defalias 'mhc-defcustom 'defcustom)
  (defmacro mhc-defcustom (symbol value doc &rest args) 
    (` (defvar (, symbol) (, value) (, doc)))))
(put 'mhc-defcustom 'lisp-indent-function 'defun)

;;; Configration Variables:
(mhc-defgroup mhc
  nil
  "Various sorts of MH Calender."
  :group 'mail)

(mhc-defcustom mhc-mailer-package 'mew
  "*Variable to set your favorite mailer."
  :group 'mhc
  :type '(radio (const :tag "Mew" mew)
		(const :tag "Wanderlust" wl)
		(const :tag "Gnus" gnus)))


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
    (define-key map "\C-ct" 'mhc-cal-toggle-insert-rectangle)
    map)
  "Keymap for `mhc-mode'.")

(easy-menu-define mhc-mode-menu
		  mhc-mode-map
		  "Menu used in mhc mode." 
		  mhc-mode-menu-spec)

(defvar mhc-mode nil
  "Non-nil when in mhc-mode.")

(make-variable-buffer-local 'mhc-mode)

(or (assq 'mhc-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(mhc-mode " mhc") minor-mode-alist)))

(or (assq 'mhc-mode minor-mode-map-alist)
    (setq minor-mode-map-alist 
	  (cons (cons 'mhc-mode mhc-mode-map) minor-mode-map-alist)))

(defun mhc-mode (&optional arg)
  "MHC is the mode for registering schdule directly from email.  
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
  (setq mhc-mode (if (null arg)
		     (not mhc-mode)
		   (> (prefix-numeric-value arg) 0)))
  (if (featurep 'xemacs) (easy-menu-add mhc-mode-menu))
  (force-mode-line-update))


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
  (let ((ret nil) (month-str (ddate-yymm-s1 (ddate-now) "/")))
    (while (null ret)
      (setq month-str 
	    (read-from-minibuffer
	     (concat prompt "(yyyy/mm) : ") month-str nil nil 'mhc-month-hist))
      (if (string-match "\\([0-9]+\\)/\\([0-9]+\\)" month-str)
	  (setq ret (ddate-new (ddate-substring-to-int month-str 1)
			       (ddate-substring-to-int month-str 2) 1 t))))
    ret))

(defun mhc-goto-month (&optional date hide-private)
  (interactive)
  (let ((category (mhc-category-convert mhc-default-category)))
    (mhc-scan-month 
     (if (ddate-p date) date (mhc-input-month "Month "))
     mhc-mailer-package
     (cdr category)
     (car category)
     hide-private
     )))

(defun mhc-goto-this-month ()
  (interactive)
  (mhc-goto-month (ddate-now)))

(defun mhc-goto-next-month (&optional arg)
  (interactive "p")
  (mhc-goto-month (ddate-mm-inc (or (mhc-current-ddate-month)
				    (ddate-now)) arg)))

(defun mhc-goto-prev-month (&optional arg)
  (interactive "p")
  (mhc-goto-next-month (- arg)))

(defun mhc-goto-today ()
  (interactive)
  (let ((now (ddate-now)) (buf-date (mhc-current-ddate-month)))
    (goto-char (point-min))
    (if (and (= (ddate-yy now) (ddate-yy buf-date))
	     (= (ddate-mm now) (ddate-mm buf-date))
	     (re-search-forward
	      (format "^\\([0-9]+\\)? | %s" (ddate-mmdd-s1 (ddate-now) "/"))
	      nil t))
	(progn 
	  (beginning-of-line)
	  (if (not (pos-visible-in-window-p (point)))
	      (recenter)))
      )))

(defun mhc-rescan-month (&optional hide-private)
  (interactive "P")
  (let ((category (mhc-category-convert mhc-default-category))
	(line (+ (count-lines (point-min) (point))
		 (if (= (current-column) 0) 1 0))))
    (mhc-scan-month
     (mhc-current-ddate-month)
     mhc-mailer-package
     (cdr category)
     (car category)
     hide-private)
    (goto-line line)
    (beginning-of-line)
    ))

(defun mhc-clear-cache ()
  (interactive)
  (mhc-db-setup mhc-schedule-file
		(mhc-summary-folder-to-path mhc-base-folder)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make scan form.

(defvar mhc-summary-week-string-list
  '("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))

(defun ddate-ww-s2 (date)
  (format "%s" (nth (ddate-ww date) mhc-summary-week-string-list)))

(defvar mhc-face-week-color-paint-thick nil)
(defvar mhc-header-string-mew  "0 | ")

(eval-after-load "gnus"
  '(progn
     (if (string-match "SEMI" gnus-version)
	 (progn
	   (require 'eword-encode)
	   (defalias 'mhc-eword-encode-string 'eword-encode-string))
       (defun mhc-eword-encode-string (string)
	 "Alternative function of `eword-encode-string' for pure Gnus."
	 (with-temp-buffer
	   (insert string)
	   (rfc2047-encode-region (point-min) (point-max))
	   (buffer-substring (point-min) (point-max)))))))

(defun mhc-sch-scan1 (sch type &optional date conf secret first)
  (let ((subject  (mhc-sch-subject sch))
	(category (car (mhc-sch-category sch)))
	date-color week-color insert 
	head-string date-string week-string time-string
	conf-string subj-string foot-string location-string)
    (setq week-color
	  (cond
	   ((mhc-sch-in-category-p sch "Holiday") 'mhc-category-face-holiday)
	   ((eq (ddate-ww date) 0)                'mhc-summary-face-sunday)
	   ((eq (ddate-ww date) 6)                'mhc-summary-face-saturday)
	   (t                                      nil )))
    (setq date-color
	  (cond
	   ((ddate= (ddate-now) date) 'mhc-summary-face-today)
	   (t                          week-color)))
    (setq head-string
	  (cond
	   ((eq type 'mew)  mhc-header-string-mew)
	   ((eq type 'wl)  (concat (mhc-sch-foldermsg-wl sch) " | "))
	   ((eq type 'gnus) " | ")	; dummy for regexp
	   (t               "")))
    (put-text-property 0 (length head-string) 'invisible t head-string)
    (setq date-string
	  (cond
	   ((not date)  "")
	   (first      (ddate-mmdd-s1 date "/"))
	   (t          "     ")))
    (setq week-string
	  (cond
	   ((not date) "")
	   (first      (ddate-ww-s2 date))
	   (t          "   ")))
    (if (and first date-color (not mhc-face-week-color-paint-thick))
	(mhc-face-put date-string date-color))
    (if (and first week-color (not mhc-face-week-color-paint-thick))
	(mhc-face-put week-string week-color))
    (setq time-string
	  (format "%-11s" (mhc-sch-time-as-string sch)))
    (mhc-face-put time-string 'mhc-summary-face-time)
    (setq conf-string
	  (if conf mhc-summary-string-conflict))
    (setq subj-string
	  (cond
	   ((and secret (mhc-sch-in-category-p sch "Private"))
	    mhc-summary-string-secret)
	   ((and category (mhc-face-category-to-face category))
	    (mhc-face-put subject (mhc-face-category-to-face category))
	    subject)
	   (t subject)))
    (if (mhc-sch-location sch)
	(progn
	  (setq location-string
		(concat  "[" (mhc-sch-location sch) "]"))
	  (mhc-face-put location-string 'mhc-summary-face-location)))
    (setq foot-string
	  (cond
	   ((eq type 'mew) (concat "\r " (mhc-sch-foldermsg sch)))
	   ((eq type 'wl)   "")
	   ((eq type 'gnus) "")
	   (t               "")))
    (setq insert (concat
		  head-string
		  date-string " "
		  week-string " "
		  time-string " "
		  conf-string (if conf-string " ")
		  subj-string " "
		  location-string
		  foot-string
		  "\n"))
    (if (and week-color mhc-face-week-color-paint-thick)
	(mhc-face-put insert week-color))
    (cond
     ((eq type 'gnus)
      (let ((num (mhc-sch-foldermsg-gnus sch)) header)
	(if num
	    (progn
	      (put-text-property 0 (length insert) 'gnus-number num insert)
	      (setq header
		    (make-full-mail-header 0 (mhc-eword-encode-string
					      (mhc-sch-subject sch))))
	      (push (gnus-data-make num 0 0 header 0) gnus-newsgroup-data))
	  (remove-text-properties 0 (length insert)
				  '(gnus-number nil) insert)))))
    (insert insert)
    (set-buffer-modified-p nil)))

(defun mhc-sch-scan-date (date type &optional cat inv-cat secret update)
  (let ((first t)
	(sch-list (mhc-db-search1 date cat inv-cat update))
	time-b1 time-e1 time-b2 time-max conf)
    (if sch-list
	(progn
	  (while sch-list
	    ;; check conflictions
	    (setq time-b1 (mhc-sch-time-b (car sch-list))
		  time-e1 (mhc-sch-time-e (car sch-list))
		  time-b2 (mhc-sch-time-b (car (cdr sch-list))))
	    (setq conf
		  (if (or (and time-e1 time-b2  (dtime< time-b2 time-e1))
			  (and time-b1 time-max (dtime< time-b1 time-max)))
		      t))
	    (if (or (null time-max) (and time-e1 (dtime< time-max time-e1)))
		(setq time-max time-e1))
	    (mhc-sch-scan1 (car sch-list) type date conf secret first)
	    (setq first nil)
	    (setq sch-list (cdr sch-list)))
	  t)
      ;; If today is free, insert only date headings.
      (mhc-sch-scan1 nil type date conf secret first)
      nil)))
      
(defvar mhc-use-week-separator 6
  "*if number 0 .. 6, insert separator in summary buffer.")

(defun mhc-insert-separator ()
  (let* ((width (- (window-width) 24)) ;; xxx
	 (hr (make-string width ?-)))
    (mhc-face-put hr 'mhc-summary-face-separator)
    (insert hr "\n")))

(defun mhc-sch-scan (from to type &optional cat inv-cat secret)
  (let ((ddate from) (update t))
    (while (ddate<= ddate to)
      (mhc-sch-scan-date ddate type cat inv-cat secret update)
      (if (and mhc-use-week-separator
	       (= (ddate-ww ddate) mhc-use-week-separator))
	  (mhc-insert-separator))
      (setq update nil)
      (setq ddate (ddate-inc ddate)))))

(defvar mhc-mode-hook nil)

(defun mhc-gnus-ddate-to-vgroup-name (ddate)
  (format "nnvirtual:%s/%02d/%02d"
	  mhc-base-folder (ddate-yy ddate) (ddate-mm ddate)))

(defun mhc-gnus-prepare-summary-buffer (ddate)
  (let ((vgroup (mhc-gnus-ddate-to-vgroup-name ddate))
	;; initialize ephemeral nndir groups.
	(groups (delq nil
		      (mapcar
		       (lambda (dir)
			 (setq dir (file-name-as-directory dir))
			 (let* ((method `(nndir ,dir (nndir-directory ,dir)))
				(group (gnus-group-prefixed-name dir method)))
			   (if (gnus-gethash group gnus-newsrc-hashtb)
			       group
			     (mhc-misc-mkdir-or-higher dir)
			     (gnus-group-read-ephemeral-group
			      group method t nil t))))
		       (list (mhc-gnus-ddate-to-folder ddate)
			     (expand-file-name "intersect"
					       (mhc-summary-folder-to-path
						mhc-base-folder)))))))
    ;; initialize ephemeral nnvirtual group.
    (gnus-group-read-ephemeral-group
     vgroup `(nnvirtual ,vgroup (nnvirtual-component-groups ,groups))
     t
     (if (buffer-live-p gnus-summary-buffer)
	 (cons gnus-summary-buffer 'summary)
       (cons (current-buffer) 'group))
     t)
    (if (null (gnus-group-read-group 0 t vgroup))
	(let (buffer)
	  (set-buffer (setq buffer (get-buffer-create
				    (mhc-ddate-to-buffer ddate))))
	  (switch-to-buffer buffer)))
    (gnus-summary-make-local-variables)
    (setq inhibit-read-only t)
    (kill-region (point-min) (point-max))))

(defun mhc-scan-month (ddate type cat inv-cat secret)
  (let ((buffer) (insert-current (not (memq type '(mew wl gnus)))))
    (or insert-current
	(if (eq mhc-mailer-package 'gnus)
	    (mhc-gnus-prepare-summary-buffer ddate)
	  (set-buffer (setq buffer (get-buffer-create
				    (mhc-ddate-to-buffer ddate))))
	  (switch-to-buffer buffer)
	  (setq inhibit-read-only t)
	  (setq buffer-read-only nil)
	  (widen)
	  (erase-buffer)
	  (setq selective-display t)
	  (setq selective-display-ellipses nil)
	  (setq indent-tabs-mode nil)))
    (message "Scanning %s ..." (ddate-yymm-s1 ddate "/"))
    (mhc-sch-scan (ddate-mm-first-day ddate)
		  (ddate-mm-last-day  ddate)
		  type
		  cat
		  inv-cat
		  secret)
    (if insert-current
	()
      (if mhc-insert-calendar
	  (mhc-cal-insert-rectangle-at ddate (- (window-width) 24))) ;; xxx
      (mhc-goto-today)
      (cond
       ((eq type 'mew)
	(make-local-variable 'mew-use-cursor-mark)
	(make-local-variable 'mew-use-highlight-cursor-line)
	(setq mew-use-cursor-mark nil)
	(setq mew-use-highlight-cursor-line nil)
	(let ((mew-virtual-mode-hook nil))
	  (mew-virtual-mode))
	(mew-buffers-setup (buffer-name))
	(and (mew-buffer-message)
	     (get-buffer-window (mew-buffer-message))
	     (window-live-p (get-buffer-window (mew-buffer-message)))
	     (delete-window (get-buffer-window (mew-buffer-message))))
	(mew-summary-toggle-disp-msg 'off))
       ((eq type 'wl)
	(wl-summary-mode)
	(wl-summary-buffer-set-folder (mhc-wl-ddate-to-folder ddate))
	(make-local-variable 'wl-summary-highlight)
	(setq wl-summary-highlight nil)
	(make-local-variable 'wl-summary-buffer-name)
	(setq wl-summary-buffer-name (buffer-name))
	(setq wl-summary-buffer-number-regexp "[0-9]+")
	(setq wl-summary-buffer-msgdb '(nil)))
       ((eq type 'gnus)
	(let (gnus-newsgroup-data)
	  (gnus-summary-mode (mhc-gnus-ddate-to-vgroup-name ddate)))
	(if (fboundp 'gnus-summary-setup-default-charset)
	    (gnus-summary-setup-default-charset)) ; for Nana7
	(make-local-variable 'mhc-gnus-skip-cursor-jump)
	(setq mhc-gnus-skip-cursor-jump t)
	(make-local-variable 'gnus-visual)
	(setq gnus-visual nil)))
      (mhc-mode 1)
      (run-hooks 'mhc-mode-hook)
      (setq inhibit-read-only nil)
      (setq buffer-read-only t))
    (message "Scanning %s ... done." (ddate-yymm-s1 ddate "/"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mhc-sch-foldermsg (sch)
  (let ((path (mhc-sch-path sch)) fld-msg)
    (setq fld-msg
	  (if (and path 
		   (string-match 
		    (concat
		     "^" 
		     (regexp-quote
		      (file-name-as-directory mhc-mail-path))) path))
	      (concat "+" (substring path (match-end 0)))
	    (concat mhc-base-folder "/1970/01/0")))
    (concat
     (directory-file-name (file-name-directory fld-msg)) " "
     (file-name-nondirectory fld-msg))))

(defun mhc-sch-foldermsg-wl (sch)
  (let ((path (mhc-sch-path sch)))
    (cond
     ((not path)
      "100000")
     ((string-match "/intersect/" path)
      (format "1%05d" (string-to-int (file-name-nondirectory path))))
     (t 
      (format "2%05d" (string-to-int (file-name-nondirectory path)))))))

(defun mhc-wl-ddate-to-folder (ddate)
  (concat "*" 
	  mhc-base-folder "/intersect"
	  ","
	  mhc-base-folder "/" (ddate-yymm-s1 ddate "/")))

(defun mhc-sch-foldermsg-gnus (sch)
  (let ((path (mhc-sch-path sch)))
    (if path
	(nnvirtual-reverse-map-article
	 (concat "nndir:" (file-name-directory path))
	 (string-to-int (file-name-nondirectory path))))))
		 
(defun mhc-gnus-ddate-to-folder (ddate)
  (expand-file-name
   (ddate-yymm-s1 ddate "/")
   (mhc-summary-folder-to-path mhc-base-folder)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; import, edit, delete, modify

(defvar mhc-summary-message-alist
  '((mew-summary-mode . mew-message-mode)
    (mew-virtual-mode . mew-message-mode)))



(defun mhc-edit (&optional import-buffer)
  "Edit a new schedule.
If optional argument IMPORT-BUFFER is specified, import its content.
Returns t if the importation was succeeded."
  (interactive (if current-prefix-arg
		   (list (get-buffer (read-buffer
				      (format 
				       "Import buffer: ")
				      (current-buffer))))))
  (let ((draft-buffer (generate-new-buffer mhc-draft-buffer-name))
	(current-date (mhc-current-ddate))
	(succeed t)
	sch old)
    (and (interactive-p)
	 (mhc-window-push))
    (set-buffer draft-buffer)
    (if import-buffer
	(progn
	  (insert-buffer import-buffer)
	  (mhc-misc-hdr-delete-list mhc-draft-unuse-hdr-list)
	  (switch-to-buffer draft-buffer t)))
    (setq sch (mhc-sch-new))
    (condition-case ()
	(if import-buffer
	    (progn
	      (delete-other-windows)
	      (if (y-or-n-p "Do you want to import this article? ")
		  (progn
		    (setq old (mhc-sch-new-from-buffer))
		    ;; input date
		    (mhc-sch-set-day sch (mhc-input-day
					  "Date: "
					  (mhc-sch-day old)
					  (gdate-guess-date)))
		    ;; input time
		    (apply 'mhc-sch-set-time
			   sch
			   (mhc-input-time
			    "Time: "
			    (if (mhc-sch-time old)
				(mhc-sch-time-as-string old))
			    (gdate-guess-time
			     (mhc-minibuf-candidate-nth-begin)
			     )))
		    ;; input subject
		    (mhc-sch-set-subject sch
					 (mhc-input-subject 
					  "Subject: "
					  (mhc-misc-sub
					   (or (mhc-sch-subject old)
					       (mhc-misc-hdr-value "Subject:"))
					   "^\\(Re:\\)? *\\(\\[[^\]]+\\]\\)? *"
					   "")))
		    ;; input location
		    (mhc-sch-set-location sch
					  (mhc-input-location 
					   "Location: "
					   (mhc-sch-location old)))
		    ;; input category
		    (mhc-sch-set-category
		     sch
		     (mhc-input-category 
		      "Category: "
		      (mhc-sch-category old)))
		    (mhc-misc-hdr-delete-list mhc-sch-header-list))
		;; Answer was no.
		(message "") ; flush minibuffer.
		(and (interactive-p)
		     (mhc-window-pop))
		(setq succeed nil)
		(kill-buffer draft-buffer)))
	  ;; No import (it succeeds).
	  (mhc-sch-set-day  sch (mhc-input-day "Date: " current-date))
	  (apply 'mhc-sch-set-time sch (mhc-input-time "Time: "))
	  (mhc-sch-set-subject sch (mhc-input-subject "Subject: "))
	  (mhc-sch-set-location sch (mhc-input-location "Location: "))
	  (mhc-sch-set-category sch (mhc-input-category "Category: ")))
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
	  (insert (mhc-sch-dump-header sch))
	  (if import-buffer
	      ()
	    (goto-char (point-max))
	    (insert "----\n"))
	  (mhc-draft-mode)
	  succeed))))

(defun mhc-edit-old ()
  (interactive)
  (mhc-window-push)
  (let (sch)
    (condition-case ()
	(progn
	  (setq sch (mhc-sch-new))
	  (mhc-sch-set-day  sch (mhc-input-day "Date: " (mhc-current-ddate)))
	  (apply 'mhc-sch-set-time sch (mhc-input-time "Time: "))
	  (mhc-sch-set-subject sch (mhc-input-subject "Subject: "))
	  (mhc-sch-set-location sch (mhc-input-location "Location: "))
	  (mhc-sch-set-category sch (mhc-input-category "Category: "))
	  (mhc-prepare-draft sch))
      (quit (mhc-window-pop)))))

(defun mhc-mew-set-import-buffer (get-original)
  (let (mode)
    (if get-original (mew-summary-display-asis))
    (setq mode (assq major-mode mhc-summary-message-alist))
    (if (eq (cdr mode)
	    (save-window-excursion (other-window 1) major-mode))
	(other-window 1))))

(defun mhc-gnus-set-import-buffer (get-original)
  (if get-original
      (let ((gnus-break-pages nil) (gnus-show-mime nil))
	(gnus-summary-select-article 'all-headers 'force))
    (gnus-summary-select-article))
  (select-window (get-buffer-window gnus-article-buffer)))

(defun mhc-wl-set-import-buffer (get-original)
  (if get-original
      (wl-summary-redisplay-no-mime)
    (let (wl-highlight-x-face-func)
      (wl-summary-redisplay-all-header)))
  (wl-summary-jump-to-current-message))


(defun mhc-import (&optional get-original)
  (interactive "P")
  (mhc-window-push)
  (funcall (intern (format "mhc-%s-set-import-buffer"
			   (symbol-name mhc-mailer-package)))
	   get-original)
  (if (not (mhc-edit (current-buffer)))
      ;; failed.
      (mhc-window-pop)))
   

(defun mhc-import-old (&optional get-original)
  (interactive "P")
  (mhc-window-push)
  (let (guess ptr ddate dtime mode sch old)
    (cond
     ((eq mhc-mailer-package 'mew)
      (if get-original (mew-summary-display-asis))
      (setq mode (assq major-mode mhc-summary-message-alist))
      (if (eq (cdr mode)
	      (save-window-excursion (other-window 1) major-mode))
	  (other-window 1)))
     ((eq mhc-mailer-package 'gnus)
      (if get-original
	  (let ((gnus-break-pages nil) (gnus-show-mime nil))
	    (gnus-summary-select-article 'all-headers 'force))
	(gnus-summary-select-article))
      (select-window (get-buffer-window gnus-article-buffer)))
     ((eq mhc-mailer-package 'wl)
      (if get-original (wl-summary-redisplay-no-mime))
      (wl-summary-jump-to-current-message)))
    (delete-other-windows)
    (condition-case ()
	(if (y-or-n-p "Do you want to import this buffer ? ")
	    (progn
	      (setq sch (mhc-sch-new))
	      (setq old (mhc-sch-new-from-buffer))
	      ;; input date
	      (mhc-sch-set-day sch
			       (mhc-input-day
				"Date: "
				(mhc-sch-day old)
				(gdate-guess-date)))
	      ;; input time
	      (apply 'mhc-sch-set-time
		     sch
		     (mhc-input-time
		      "Time: "
		      (if (mhc-sch-time old) (mhc-sch-time-as-string old))
		      (gdate-guess-time
		       (mhc-minibuf-candidate-nth-begin)
		       )))
	      ;; input subject
	      (mhc-sch-set-subject sch
				   (mhc-input-subject 
				    "Subject: "
				    (mhc-misc-sub
				     (or (mhc-sch-subject old)
					 (mhc-misc-hdr-value "Subject:"))
				     "^\\(Re:\\)? *\\(\\[[^\]]+\\]\\)? *"
				     "")))
	      ;; input location
	      (mhc-sch-set-location sch
				    (mhc-input-location 
				     "Location: "
				     (mhc-sch-location old)))
	      ;; input category
	      (mhc-sch-set-category
	       sch
	       (mhc-input-category 
		"Category: "
		(mhc-sch-category old)))
	      ;; prepare new draft
	      (mhc-prepare-draft sch (current-buffer)))
	  (mhc-window-pop))
      (quit (mhc-window-pop)))))


(defun mhc-delete ()
  (interactive)
  (let ((filename (mhc-summary-filename)) sch)
    (if (not (and (stringp filename) (file-exists-p filename)))
	(message "File does not exist (%s)." filename)
      (setq sch (mhc-sch-new-from-file filename))
      (if  (not (y-or-n-p (format "Do you delete %s ?"
				  (mhc-sch-subject-as-string sch))))
	  (message "Never mind..")
	(if (and (mhc-sch-occur-multiple-p sch)
		 (not
		  (y-or-n-p 
		   (format
		    "%s has multiple occurrences. Delete all(=y) or one(=n) ?"
		    (mhc-sch-subject-as-string sch)))))
	    (progn
	      (mhc-window-push)
	      (mhc-sch-del-day sch (mhc-current-ddate))
	      (find-file-other-window filename)
	      (mhc-misc-hdr-delete-list mhc-sch-header-list)
	      (insert (mhc-sch-dump-header sch))
	      (mhc-draft-mode))
	  (mhc-db-del-sch sch)
	  (mhc-rescan-month))))))

(defun mhc-modify ()
  (interactive)
  (mhc-modify-file (mhc-summary-filename)))

(defun mhc-modify-file (filename)
  (let (sch)
    (if (not (and (stringp filename) (file-exists-p filename)))
	(message "File does not exist (%s)." filename)
      (mhc-window-push)
      (setq sch (mhc-sch-new-from-file filename))
      (find-file-other-window filename)
      (mhc-draft-mode))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; draft
;;

(defvar mhc-draft-mode-map nil)
(defvar mhc-draft-buffer-name  "mhc-draft")
(defvar mhc-draft-unuse-hdr-list 
  '(
    "Return-Path:" "Received:" "X-Dispatcher:"
    "Lines:" "X-Filter:" "Replied:" "X-Mailer:"
    "Errors-To:" "Sender:" "X-Seqno:" "X-Received:"
    "X-Sender:" "From " ">From " "Precedence:" "Posted:"
    ))

(if mhc-draft-mode-map
    ()
  (setq mhc-draft-mode-map (make-sparse-keymap))
  (define-key mhc-draft-mode-map "\C-c\C-c" 'mhc-draft-finish)
  (define-key mhc-draft-mode-map "\C-c\C-q" 'mhc-draft-kill))

(defvar mhc-draft-mode-hook nil)

(defun mhc-draft-mode ()
  (interactive)
  (kill-all-local-variables)
  (use-local-map mhc-draft-mode-map)
  (setq mode-name "MHC-Draft")
  (setq major-mode 'mhc-draft-mode)
  (run-hooks 'text-mode-hook 'mhc-draft-mode-hook))

(defun mhc-draft-kill ()
  (interactive)
  (if (y-or-n-p "Kill draft buffer ?")
      (progn (kill-buffer (current-buffer))
	     (mhc-window-pop))))

(defun mhc-prepare-draft-old (&optional sch import-buffer)
  (let  ((unuse-hdr-list mhc-draft-unuse-hdr-list) buf)
    (setq buf (generate-new-buffer mhc-draft-buffer-name))
    (if import-buffer
	(save-excursion
	  (set-buffer import-buffer)
	  (copy-to-buffer buf (point-min) (point-max))))
    (switch-to-buffer buf t)
    (mhc-misc-hdr-delete-list unuse-hdr-list)
    (mhc-misc-hdr-delete-list mhc-sch-header-list)
    (insert (mhc-sch-dump-header sch))
    (if import-buffer
	()
      (goto-char (point-max))
      (insert "----\n"))
    (mhc-draft-mode)))

(defun mhc-draft-finish (&optional intersect)
  (interactive "P")
  (let (folder sch conflicts)
    (setq sch  (mhc-sch-new-from-buffer))
    (if (or (not sch) (mhc-sch-error-p sch))
	(error "Invalid Header Value... quit")
      (mhc-misc-hdr-delete-separator)
      ;; sch has no err. next, check conflictions with others.
      (if (or (not (and (ddate= (mhc-sch-occur-min sch)
				(mhc-sch-occur-max sch))
			(mhc-sch-time-b sch)
			(setq conflicts (mhc-db-busy-on-p 
					 (mhc-sch-occur-min sch)
					 (mhc-sch-time-b sch)
					 (mhc-sch-time-e sch)
					 (mhc-sch-path sch)))))
	      (yes-or-no-p
	       (format "This article will conflict with %s. OK?"
		       ;; xxx: do you need all subjects?
		       (mhc-sch-subject-as-string (car conflicts)))))
	  (if (not (mhc-db-add-sch sch (current-buffer) mhc-mail-path))
	      ()
	    (kill-buffer (current-buffer))
	    (mhc-window-pop)
	    (and (mhc-summary-buffer-p)
		 (mhc-rescan-month)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; input x-sc- schedule data from minibuffer.

(defconst mhc-input-date-regex "^\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\)$")
(defconst mhc-input-time-regex "^\\([0-9]+\\):\\([0-9]+\\)$")


(defun mhc-input-sch (&optional buf)
  (interactive)
  (let ((sch (if buf (mhc-sch-new-from-buffer buf) (mhc-sch-new))))
    (mhc-sch-set-subject   sch (mhc-input-subject))
    (mhc-sch-set-location  sch (mhc-input-location))
    (mhc-sch-set-day       sch (mhc-input-day))
    (apply 'mhc-sch-set-time sch (mhc-input-time))
    (mhc-sch-set-category  sch (mhc-input-category))
    ;; (mhc-sch-set-exception sch (mhc-input-day))
    ;;(mhc-sch-set-cond      sch (mhc-input-cond))
    ;;(mhc-sch-set-duration  sch (mhc-input-duration))
  sch))

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
    (if (and default (listp default))
	(setq default (mhc-misc-join default " ")))
    (if (string= "" (setq in (read-from-minibuffer 
			      (or prompt "Category: ")
			      (or default "")
			      nil nil 'mhc-category-hist)))
	nil
      (mhc-misc-split in))))

(defvar mhc-duration-hist nil)

(defun mhc-input-duration (&optional prompt default)
  (interactive)
  (let ((str (or default "")) date-b date-e)
    (catch 'ok
      (while t
	(setq str (read-from-minibuffer
		   (concat (or prompt "") "Date: ")
		   str nil nil 'mhc-duration-hist))
	(cond 
	 ((and (string-match
		"^\\([0-9]+/[0-9]+/[0-9]+\\)-\\([0-9]+/[0-9]+/[0-9]+\\)$" str)
	       (setq date-b (ddate-new-from-string
			     (substring str (match-beginning 1) (match-end 1))
			     t mhc-input-date-regex))
	       (setq date-e (ddate-new-from-string
			     (substring str (match-beginning 2) (match-end 2))
			     t mhc-input-date-regex))
	       (ddate<= date-b date-e))
	  (throw 'ok (list date-b date-e)))
	 ((string= "none" str)
	  (throw 'ok (list nil nil))))
	(beep)))))


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

(defvar mhc-base-folder "+schedule"
  "* Base foler of mhc")

(defvar mhc-mail-path
  (expand-file-name
   (if (and (boundp 'mew-mail-path) mew-mail-path)
       mew-mail-path "~/Mail"))
  "* Base directory your mailer recognized as `+'")

(defvar mhc-schedule-file (expand-file-name "~/.schedule")
  "* mhc DB file which contains holiday and anniversary settings.")

(defconst mhc-summary-day-regex  "^[^|]+| +[0-9]+/\\([0-9]+\\)")
(defconst mhc-summary-buf-regex   
  (concat mhc-base-folder "/\\([0-9]+\\)/\\([0-9]+\\)"))
(defconst mhc-summary-filename-regex 
  ".*\r *\\+\\([^ \t]+\\)[ \t]+\\([^ \t\n]+\\)")

(defun mhc-summary-buffer-p (&optional buffer)
  (string-match mhc-summary-buf-regex 
		(buffer-name 
		 (or buffer (current-buffer)))))

(defun mhc-current-ddate ()
  (let ((buf (buffer-name)) yy mm dd)
    (if (not (string-match mhc-summary-buf-regex buf))
	nil
      (setq yy (ddate-substring-to-int buf 1)
	    mm (ddate-substring-to-int buf 2))
      (save-excursion
	(beginning-of-line)
	(catch 'found
	  (while t
	    (cond
	     ((looking-at mhc-summary-day-regex)
	      (throw 'found (setq dd (ddate-substring-to-int t 1))))
	     ((= (point) (point-min))
	      (throw 'found nil)))
	    (forward-line -1)))
	(if dd (ddate-new yy mm dd) nil)))))

(defun mhc-summary-folder-to-path (folder &optional msg)
  (let ((fld
	 (if (eq (string-to-char folder) ?+)
	     (substring mhc-base-folder 1) folder)))
    (if msg
	(format "%s/%s/%s" mhc-mail-path fld msg)
      (format "%s/%s" mhc-mail-path fld))))

(cond
 ((eq mhc-mailer-package 'mew)
  ;; for mew-virtual-mode
  (defun mhc-summary-filename ()
    (let (folder number)
      (save-excursion
	(beginning-of-line)
	(if (not (looking-at mhc-summary-filename-regex))
	    ()
	  (setq folder (buffer-substring (match-beginning 1) (match-end 1))
		number (buffer-substring (match-beginning 2) (match-end 2)))
	  (mhc-summary-folder-to-path folder number))))))
  ;; for wl-summary-mode
 ((eq mhc-mailer-package 'wl)
  (defun mhc-summary-filename ()
    (let* ((fld-num (elmo-multi-get-real-folder-number
		     wl-summary-buffer-folder-name
		     (wl-summary-message-number)))
	   (fld (car fld-num))
	   (num (cdr fld-num)))
      (expand-file-name
       (int-to-string num)
       (elmo-localdir-get-folder-directory
	(elmo-folder-get-spec fld))))))
 ;; for gnus-summary-mode
 ((eq mhc-mailer-package 'gnus)
  (defun mhc-summary-filename ()
    (let ((num (get-text-property (point) 'gnus-number)) cell dir)
      (if num
	  (progn
	    (setq cell (nnvirtual-map-article num))
	    (if (string-match "^nndir:\\(.+\\)$" (car cell))
		(progn
		  (setq dir (match-string 1 (car cell)))
		  (format "%s%d" dir (cdr cell))))))))
  ;; modify Gnus original functions for cursor control.
  (if (not (fboundp 'gnus-summary-goto-subject-original))
      (progn
	(fset 'gnus-summary-goto-subject-original
	      (symbol-function 'gnus-summary-goto-subject))
	(defun gnus-summary-goto-subject (article &optional force silent)
	  (if (not (and (boundp 'mhc-gnus-skip-cursor-jump)
			mhc-gnus-skip-cursor-jump))
	      (gnus-summary-goto-subject-original article force silent)))))
  (if (not (fboundp 'gnus-summary-position-point-original))
      (progn
	(fset 'gnus-summary-position-point-original
	      (symbol-function 'gnus-summary-position-point))
	(defun gnus-summary-position-point ()
	  (if (not (and (boundp 'mhc-gnus-skip-cursor-jump)
			mhc-gnus-skip-cursor-jump))
	      (gnus-summary-position-point-original)))))))

(defun mhc-current-ddate-month ()
  (let ((buf (buffer-name)) yy mm dd)
    (if (not (string-match mhc-summary-buf-regex buf))
	nil
      (ddate-new (ddate-substring-to-int buf 1)
		 (ddate-substring-to-int buf 2)
		 1))))

(defun mhc-ddate-to-buffer (ddate)
  (concat mhc-base-folder "/" (ddate-yymm-s1 ddate "/")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make rectangle like calendar.el

(defvar mhc-insert-calendar t)

(defun mhc-cal-toggle-insert-rectangle ()
  (interactive)
  (setq mhc-insert-calendar (not mhc-insert-calendar))
  (mhc-rescan-month))

(defconst mhc-cal-week-header "| Su Mo Tu We Th Fr Sa")

(defun mhc-cal-insert-rectangle-at (ddate col)
  (save-excursion
    (put-text-property (point-min) (point-max) 'rear-nonsticky t)
    (goto-char (point-min))
    (mhc-misc-move-to-column col)
    (mhc-misc-insert-rectangle
     (nconc (mhc-cal-make-rectangle (ddate-mm-dec ddate))
	    (list "| ")
	    (mhc-cal-make-rectangle ddate)
	    (list "| ")
	    (mhc-cal-make-rectangle (ddate-mm-inc ddate))))))

(defun mhc-cal-make-rectangle (&optional ddate)
  (interactive)
  (let (last dd ww month sch dstr (i 0) (week "|") (update t) today color)
    (setq last (ddate-mm-last-day  (or ddate (ddate-now)))
	  dd   (ddate-mm-first-day (or ddate (ddate-now)))
	  ww   (ddate-ww dd)
	  today (ddate-now)
	  month (list
		 mhc-cal-week-header
		 (format "|    %s" (ddate-yymm-sj dd))))
    (while (< i ww) (setq week (concat week "   ")
			  i    (1+ i)))
    (while (ddate<= dd last)
      (setq color
	    (cond
	     ((eq (ddate-ww dd) 0) 'mhc-calendar-face-sunday)
	     ((mhc-db-holiday-p dd update)
	      (mhc-face-category-to-face "Holiday"))
	     ((eq (ddate-ww dd) 6) 'mhc-calendar-face-saturday)
	     (t 'default)))
      (if (equal dd today)
	  (setq color (mhc-face-get-gray-face color)))
      (if (mhc-db-busy-p dd update)
	  (setq color (mhc-face-get-underline-face color)))
      (if (and (not (string= week "|")) (= (ddate-ww dd) 0))
	  (setq month (cons week month)
		week  "|"))
      (setq dstr (format "%2d" (ddate-dd dd)))
      (if color
	  (mhc-face-put  dstr color))
      (setq week (concat week " " dstr))
      (setq update nil)
      (setq dd (ddate-inc dd)))
    (setq month (cons week month))
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
    (mhc-scan-month 
     (mhc-input-month "Month ")
     'normal ;; insert into current buffer.
     (cdr category)
     (car category)
     hide-private
     ))
  (exchange-point-and-mark))

(defvar mhc-date-hist nil)

(defun mhc-input-date (&optional prompt default)
  (interactive)
  (let* ((ddate (or default (ddate-now)))
	 yy mm in dlst tlst tstr dstr ret err)
    ;; input ddate string.
    (setq in (read-from-minibuffer
	      (concat (or prompt "") " (yyyy/mm/dd) : ")
	      (ddate-to-s1 ddate "/")
	      nil nil 'mhc-date-hist))
    ;; check format
    (if (not (string-match
	      "\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\)"
	      in))
	nil
      (ddate-new (ddate-substring-to-int in 1)
		 (ddate-substring-to-int in 2)
		 (ddate-substring-to-int in 3)))))
  
(defun mhc-view-file ()
  (interactive)
  (let ((path (mhc-summary-filename)))
    (view-file-other-window path)))

(mhc-db-setup mhc-schedule-file (mhc-summary-folder-to-path mhc-base-folder))
(mhc-face-setup)
(put-text-property 2 4 'face 'mhc-calendar-face-sunday  mhc-cal-week-header)
(put-text-property 20 22 'face 'mhc-calendar-face-saturday mhc-cal-week-header)

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

;;; mhc.el ends here
