;;; -*- emacs-lisp -*-
;; mhc-calendar.el -- MHC Mini calendar
;;
;; Author:  Hideyuki SHIRAI <shirai@quickhack.net>
;;          MIYOSHI Masanori <miyoshi@ask.ne.jp>
;;
;; Created: 05/12/2000
;; Reviesd: $Date: 2000/07/24 03:46:33 $

;;; Configration Variables:

(defcustom mhc-calendar-separator ?|
  "*Character of the separator between Summary and Vertical calendar."
  :group 'mhc
  :type 'character)

(defcustom mhc-calendar-day-strings ["Su" "Mo" "Tu" "We" "Th" "Fr" "Sa"]
  "*Vector of \"day of week\" for 3-month calendar header.
This vector must have seven elements
and each element must have \"two columns strings\"."
  :group 'mhc
  :type '(vector string))

(defcustom mhc-calendar-header-function 'mhc-calendar-make-header
  "*Function of \"make calendar header\" for 3-month calendar.
Assigned function must have one option \"date\"
and must return string like \"   December 2000\"."
  :group 'mhc
  :type 'function)

(defcustom mhc-calendar-mode-hook nil
  "*Hook called in mhc-calendar-mode."
  :group 'mhc
  :type 'hook)

(defcustom mhc-calendar-create-buffer-hook nil
  "*Hook called in mhc-calendar-create-buffer."
  :group 'mhc
  :type 'hook)

(defcustom mhc-calendar-start-column 4
  "*Size of left margin."
  :group 'mhc
  :type 'integer)

(defcustom mhc-calendar-next-offset 24
  "*Offset of next month start column (greater or equal 23)."
  :group 'mhc
  :type 'integer)

(defcustom mhc-calendar-height (if (and (featurep 'xemacs) window-system) 12 9)
  "*Offset of next month start column (greater or equal 9)."
  :group 'mhc
  :type 'integer)

(defcustom mhc-calendar-view-summary nil
  "*View day's summary if *non-nil*."
  :group 'mhc
  :type 'boolean)

(defcustom mhc-calendar-link-hnf nil
  "*Support HNF(Hyper Nikki File) mode if *non-nil*."
  :group 'mhc
  :type 'boolean)

(defcustom mhc-calendar-use-mouse-highlight t
  "*Highlight mouse pointer."
  :group 'mhc
  :type 'boolean)

(defcustom mhc-calendar-view-file-hook nil
  "*Hook called in mhc-calendar-view-file."
  :group 'mhc
  :type 'hook)

;; internal variables
(defvar mhc-calendar/buffer "*mhc-calendar*")
(defvar mhc-calendar-date nil)
(defvar mhc-calendar-view-date nil)
(defvar mhc-calendar-call-buffer nil)
(defvar mhc-calendar-date-separator nil)
(defvar mhc-calendar-mode-map nil)
(defvar mhc-calendar-mode-menu-spec nil)

;; requires
(if (locate-library "hnf-mode")
    (require 'hnf-mode))

;; mhc-calendar functions
;; macros
(defmacro  mhc-calendar-p ()
  (` (eq major-mode 'mhc-calendar-mode)))

(defmacro mhc-calendar/in-date-p () ;; return 'date from 01/01/1970'
  (` (get-text-property (point) 'mhc-calendar/date-prop)))

(defmacro mhc-calendar/in-summary-p () ;; return 'schedule filename'
  (` (or (get-text-property (point) 'mhc-calendar/summary-prop)
	 (save-excursion
	   (beginning-of-line)
	   (get-text-property (point) 'mhc-calendar/summary-prop)))))

(defmacro mhc-calendar/in-summary-hnf-p () ;; return 'title count'
  (` (or (get-text-property (point) 'mhc-calendar/summary-hnf-prop)
	 (save-excursion
	   (beginning-of-line)
	   (get-text-property (point) 'mhc-calendar/summary-hnf-prop)))))

(defmacro mhc-calendar/get-date-colnum (col)
  (` (cond
      ((< (, col) (+ mhc-calendar-next-offset mhc-calendar-start-column)) -1)
      ((< (, col) (+ (* mhc-calendar-next-offset 2) mhc-calendar-start-column)) 0)
      (t 1))))

(defmacro mhc-calendar/buffer-substring-to-num (pos)
  (` (string-to-number
      (buffer-substring (match-beginning (, pos)) (match-end (, pos))))))

;; Avoid warning of byte-compiler.
(eval-when-compile
  (defvar view-exit-action)
  (defvar mhc-calendar-mode-menu)
  (unless (featurep 'xemacs)
    (defun easy-menu-add (menu) ())))

;; Compatibilities between emacsen
(if (fboundp 'text-property-any)
    (defalias 'mhc-calendar/tp-any 'text-property-any)
  (defsubst mhc-calendar/tp-any (beg end prop value)
    (while (and beg (< beg end)
		(not (eq value (get-text-property beg prop))))
      (setq beg (next-single-property-change beg prop nil end)))
    (if (eq beg end) nil beg)))

(if (fboundp 'event-buffer)
    (defalias 'mhc-calendar/event-buffer 'event-buffer)
  (defun mhc-calendar/event-buffer (event)
      (window-buffer (posn-window (event-start event)))))

(if (fboundp 'event-point)
    (defalias 'mhc-calendar/event-point 'event-point)
  (defun mhc-calendar/event-point (event)
    (posn-point (event-start event))))

;; map/menu
(if mhc-calendar-mode-map
    ()
  (setq mhc-calendar-mode-map (make-sparse-keymap))
  (define-key mhc-calendar-mode-map "."    'mhc-calendar-goto-today)
  (define-key mhc-calendar-mode-map "g"    'mhc-calendar-goto-month)
  (define-key mhc-calendar-mode-map "r"    'mhc-calendar-rescan)
  (define-key mhc-calendar-mode-map " "    'mhc-calendar-get-day-insert)
  (define-key mhc-calendar-mode-map "\C-m" 'mhc-calendar-get-day-insert-quit)
  (define-key mhc-calendar-mode-map "/"    'mhc-calendar-get-day-with-slash)
  (define-key mhc-calendar-mode-map ","    'mhc-calendar-get-day-with-character)
  (define-key mhc-calendar-mode-map "\\"   'mhc-calendar-get-day-with-japanese)
  (define-key mhc-calendar-mode-map "s"    'mhc-calendar-scan)
  (define-key mhc-calendar-mode-map "E"    'mhc-calendar-edit)
  (define-key mhc-calendar-mode-map "M"    'mhc-calendar-modify)
  (define-key mhc-calendar-mode-map "D"    'mhc-calendar-delete)
  (define-key mhc-calendar-mode-map "H"    'mhc-calendar-hnf-edit)
  (define-key mhc-calendar-mode-map "v"    'mhc-calendar-toggle-view)
  (define-key mhc-calendar-mode-map "V"    'mhc-calendar-goto-view)
  (define-key mhc-calendar-mode-map "h"    'mhc-calendar-day-position)
  (define-key mhc-calendar-mode-map "f"    'mhc-calendar-next-day)
  (define-key mhc-calendar-mode-map "b"    'mhc-calendar-prev-day)
  (define-key mhc-calendar-mode-map "n"    'mhc-calendar-next-week)
  (define-key mhc-calendar-mode-map "p"    'mhc-calendar-prev-week)
  (define-key mhc-calendar-mode-map "N"    'mhc-calendar-next-month)
  (define-key mhc-calendar-mode-map "P"    'mhc-calendar-prev-month)
  (define-key mhc-calendar-mode-map ">"    'mhc-calendar-inc-month)
  (define-key mhc-calendar-mode-map "<"    'mhc-calendar-dec-month)
  (define-key mhc-calendar-mode-map "\M-\C-n" 'mhc-calendar-next-year)
  (define-key mhc-calendar-mode-map "\M-\C-p" 'mhc-calendar-prev-year)
  (define-key mhc-calendar-mode-map "q"    'mhc-calendar-quit)
  (define-key mhc-calendar-mode-map "Q"    'mhc-calendar-exit)
  (define-key mhc-calendar-mode-map "?"    'describe-mode)
  (cond
   ((featurep 'xemacs)
    (define-key mhc-calendar-mode-map [(button2)] 'mhc-calendar-day-at-mouse))
   (t
    (define-key mhc-calendar-mode-map [mouse-2] 'mhc-calendar-day-at-mouse))))

(if mhc-calendar-mode-menu-spec
    ()
  (setq mhc-calendar-mode-menu-spec
	'("Mhc-Calendar"
	  ["Goto today" mhc-calendar-goto-today t]
	  ["Goto next month" mhc-calendar-inc-month t]
	  ["Goto prev month" mhc-calendar-dec-month t]
	  ["Goto month" mhc-calendar-goto-month t]
	  ("Goto"
	   ["Recover position" mhc-calendar-day-position t]
	   ["Next day" mhc-calendar-next-day t]
	   ["Prev day" mhc-calendar-prev-day t]
	   ["Next week" mhc-calendar-next-week t]
	   ["Prev week" mhc-calendar-prev-week t]
	   ["Next month" mhc-calendar-next-month t]
	   ["Prev month" mhc-calendar-prev-month t]
	   ["Next year" mhc-calendar-next-year t]
	   ["Prev year" mhc-calendar-prev-year t])
	  ["Rescan" mhc-calendar-rescan t]
	  ["MHC summary scan" mhc-calendar-scan (mhc-calendar/mua-ready-p)]
	  "----"
	  ("Insert/Get"
	   ["Insert" mhc-calendar-get-day-insert t]
	   ["Insert/Quit" mhc-calendar-get-day-insert-quit t]
	   ["Get day" mhc-calendar-get-day-with-character t]
	   ["Get day [/]" mhc-calendar-get-day-with-slash t]
	   ["Get day [japanese]" mhc-calendar-get-day-with-japanese t])
	  "----"
	  ["Toggle view" mhc-calendar-toggle-view t]
	  ["Goto view area" mhc-calendar-goto-view
	   (not (or (mhc-calendar/in-summary-p) (mhc-calendar/in-summary-hnf-p)))]
	  "----"
	  ["Schedule view" mhc-calendar-goto-view
	   (or (mhc-calendar/in-summary-p) (mhc-calendar/in-summary-hnf-p))]
	  ("Schedule edit"
	   ["Schedule addition" mhc-calendar-edit
	    (or (mhc-calendar/in-date-p) (mhc-calendar/in-summary-p))]
	   ["Schedule modify" mhc-calendar-modify (mhc-calendar/in-summary-p)]
	   ["Schedule delete" mhc-calendar-delete (mhc-calendar/in-summary-p)]
	   ["HNF file edit" mhc-calendar-hnf-edit
	    (and mhc-calendar-link-hnf
		 (or (mhc-calendar/in-date-p) (mhc-calendar/in-summary-p)
		     (mhc-calendar/in-summary-hnf-p)))])
	  "----"
	  ("Misc"
	   ["Quit" mhc-calendar-quit t]
	   ["Kill" mhc-calendar-exit t]
	   ["Help" describe-mode t]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make rectangle like calendar.el

(defvar mhc-calendar/week-header nil)
(defvar mhc-calendar/separator-str nil)

(defun mhc-calendar-toggle-insert-rectangle ()
  (interactive)
  (setq mhc-insert-calendar (not mhc-insert-calendar))
  (when (mhc-summary-buffer-p)
    (mhc-rescan-month mhc-default-hide-private-schedules)))

(defun mhc-calendar-setup ()
  (setq mhc-calendar/week-header nil)
  (setq mhc-calendar/separator-str (char-to-string mhc-calendar-separator))
  (mhc-face-put mhc-calendar/separator-str 'mhc-summary-face-separator)
  (let ((i 0) day)
    (while (< i 7)
      (setq day (aref mhc-calendar-day-strings i))
      (cond
       ((= i 0)
	(mhc-face-put day 'mhc-calendar-face-sunday))
       ((= i 6)
	(mhc-face-put day 'mhc-calendar-face-saturday))
       (t (mhc-face-put day 'mhc-calendar-face-default)))
      (setq mhc-calendar/week-header
	    (concat mhc-calendar/week-header
		    (if mhc-calendar/week-header " ") day))
      (setq i (1+ i)))))

(defun mhc-calendar-insert-rectangle-at (date col)
  (save-excursion
    (put-text-property (point-min) (point-max) 'rear-nonsticky t)
    (goto-char (point-min))
    (mhc-misc-move-to-column col)
    (mhc-misc-insert-rectangle
     (nconc (mhc-calendar/make-rectangle (mhc-date-mm-- date)
					 mhc-calendar/separator-str)
	    (list (concat mhc-calendar/separator-str " "))
	    (mhc-calendar/make-rectangle date mhc-calendar/separator-str)
	    (list (concat mhc-calendar/separator-str " "))
	    (mhc-calendar/make-rectangle (mhc-date-mm++ date)
					 mhc-calendar/separator-str)))))

(defun mhc-calendar-make-header (date)
  (mhc-date-format date "   %s %04d"
		   (mhc-date-digit-to-mm-string mm t) yy))

(defun mhc-calendar/make-rectangle (&optional date separator)
  (let* ((today (mhc-date-now))
	 (days (mhc-db-scan-month (mhc-date-yy (or date today))
				  (mhc-date-mm (or date today)) t))
	 (separator (if separator separator mhc-calendar/separator-str))
	 (month (list (concat separator " " mhc-calendar/week-header)
		      (concat separator " "
			      (funcall mhc-calendar-header-function
				       (or date today)))))
	 (i (mhc-day-day-of-week (car days)))
	 week color)
    (while (> i 0)
      (setq week (cons "  " week)
	    i (1- i)))
    (while days
      (setq color
	    (cond
	     ((= 0 (mhc-day-day-of-week (car days)))
	      'mhc-calendar-face-sunday)
	     ((mhc-day-holiday (car days)) 
	      (mhc-face-category-to-face "Holiday"))
	     ((= 6 (mhc-day-day-of-week (car days))) 
	      'mhc-calendar-face-saturday)
	     (t 'mhc-calendar-face-default)))
      (if (mhc-date= today (mhc-day-date (car days)))
	  (setq color (mhc-face-get-today-face color)))
      (if (mhc-day-busy-p (car days))
	  (setq color (mhc-face-get-busy-face color)))
      (setq week (cons (format "%2d" (mhc-day-day-of-month (car days))) week))
      (if color
	  (mhc-face-put (car week) color))
      (if (= 6 (mhc-day-day-of-week (car days)))
	  (setq month (cons (mapconcat
			     (function identity)
			     (cons separator (nreverse week))
			     " ")
			    month)
		week nil))
      (setq days (cdr days)))
    (if week
	(setq month (cons (mapconcat
			   (function identity)
			   (cons separator (nreverse week))
			   " ")
			  month)))
    (nreverse month)))

;; function
(defun mhc-calendar-mode ()
  "\\<mhc-calendar-mode-map>
MHC Calendar mode:: major mode to view calendar and select day.

The keys that are defined for mhc-calendar-mode are:
\\[mhc-calendar-goto-today]	Jump to today.
\\[mhc-calendar-inc-month]	Slide to the next month.
\\[mhc-calendar-dec-month]	Slide to the previous month.
\\[mhc-calendar-goto-month]	Jump to your prefer month.
\\[mhc-calendar-rescan]	Rescan current calendar.
\\[mhc-calendar-scan]	Scan the point day's schedule summary with MUA.
  If '\\[mhc-calendar-scan]' executed with 'prefix argument', hide private category.

\\[mhc-calendar-get-day-insert]	Get day at point to insert call buffer.
\\[mhc-calendar-get-day-insert-quit]	Get day at point to insert call buffer, quit.
\\[mhc-calendar-get-day-with-slash]	Get day separated \"/\" to save kill-ring.
\\[mhc-calendar-get-day-with-japanese]	Get day \"yyyy年mm月dd日(ww曜日)\" to save kill-ring.
\\[mhc-calendar-get-day-with-character]	Get day separated input character to save kill-ring.
  '\\[mhc-calendar-get-day-with-character]' Special characters: 'n' means NULL. '\' means Japanese.
  Return 'Duration' in the region
  if '\\[mhc-calendar-get-day-insert]' '\\[mhc-calendar-get-day-insert-quit]' '\\[mhc-calendar-get-day-with-slash]' '\\[mhc-calendar-get-day-with-japanese]' '\\[mhc-calendar-get-day-with-character]' executed with 'prefix argument'.
  '\\[mhc-calendar-get-day-insert]' '\\[mhc-calendar-get-day-insert-quit]' '\\[mhc-calendar-get-day-with-character]' has two SPECIAL characters: 'n' means NULL. '\' means Japanese.

\\[mhc-calendar-toggle-view]	Toggle summary view mode.
\\[mhc-calendar-goto-view]	Goto summary view position or view schedule file.
\\[mhc-calendar-edit]	Create new schdule file.
  If optional argument IMPORT-BUFFER is specified, import its content.
\\[mhc-calendar-modify]	Edit the schdule on the cursor point.
\\[mhc-calendar-delete]	Delete the schdule on the cursor point.
\\[mhc-calendar-hnf-edit] Edit the Hyper Nikki File.

\\[mhc-calendar-day-position]	Recover positioning.
\\[mhc-calendar-next-day]	Goto the next day.
\\[mhc-calendar-prev-day]	Goto the previous day.
\\[mhc-calendar-next-week]	Goto the next week or goto the next summary.
\\[mhc-calendar-prev-week]	Goto previous week or goto the previous summary.
\\[mhc-calendar-next-month]	Goto next month.
\\[mhc-calendar-prev-month]	Goto previous month.
\\[mhc-calendar-next-year]	Goto next year.
\\[mhc-calendar-prev-year]	Goto previous year.
  '\\[mhc-calendar-next-day]' '\\[mhc-calendar-prev-day]' '\\[mhc-calendar-next-week]' '\\[mhc-calendar-prev-week]' '\\[mhc-calendar-next-month]' '\\[mhc-calendar-prev-month]' '\\[mhc-calendar-inc-month]' '\\[mhc-calendar-dec-month]' '\\[mhc-calendar-next-year]' '\\[mhc-calendar-prev-year]'
  effected by 'prefix argument(integer number)'.
\\[mhc-calendar-day-at-mouse]	Day positioning or view schedule file.

\\[mhc-calendar-quit]	Quit and calendar buffer bury.
\\[mhc-calendar-exit]	Quit and calendar buffer kill.
\\[describe-mode]	Show mode help.
"
  (interactive)
  (kill-all-local-variables)
  (use-local-map mhc-calendar-mode-map)
  (make-local-variable 'mhc-calendar-date)
  (make-local-variable 'mhc-calendar-view-date)
  (make-local-variable 'indent-tabs-mode)
  (setq major-mode 'mhc-calendar-mode)
  (setq mode-name "mhc-calendar")
  (setq indent-tabs-mode nil)
  (setq truncate-lines t)
  (if (featurep 'xemacs) (easy-menu-add mhc-calendar-mode-menu))
  (run-hooks 'mhc-calendar-mode-hook))

;;;###autoload
(defun mhc-calendar (&optional date)
  "Display 3-month mini calender."
  (interactive)
  (or mhc-setup-p (mhc-setup))
  (setq date (or date (mhc-current-date) (mhc-calendar-get-date)))
  (when (and (get-buffer mhc-calendar/buffer) (set-buffer mhc-calendar/buffer))
    (setq date (or date mhc-calendar-view-date))
    (unless (mhc-date-yymm= date mhc-calendar-date)
      (mhc-calendar/create-buffer date)))
  (mhc-calendar/goto-date (or date (mhc-date-now))))

(defun mhc-calendar-goto-today ()
  (interactive)
  (mhc-calendar (mhc-date-now)))

(defun mhc-calendar/goto-date (date)
  (let ((mhc-calendar-view-summary nil) pos)
    (if (not (get-buffer mhc-calendar/buffer))
	(mhc-calendar/create-buffer date))
    (set-buffer (get-buffer mhc-calendar/buffer))
    (pop-to-buffer mhc-calendar/buffer)
    (while (not pos)
      (setq pos (mhc-calendar/tp-any (point-min) (point-max)
				     'mhc-calendar/date-prop date))
      (or pos (mhc-calendar/create-buffer date)))
    (goto-char (1+ pos))
    (setq mhc-calendar-view-date date))
  (save-excursion
    (mhc-calendar/view-summary-delete)
    (if mhc-calendar-view-summary
	(progn
	  (mhc-calendar/view-summary-insert)
	  (and mhc-calendar-link-hnf
	       (mhc-calendar/hnf-summary-insert))
	  (mhc-calendar/put-property-summary))))
  (mhc-calendar/shrink-window))

(defun mhc-calendar/view-summary-delete ()
  (goto-char (point-min))
  (if (re-search-forward "^--" nil t)
      (let ((buffer-read-only nil))
	(beginning-of-line)
	(forward-char -1)
	(set-text-properties (point) (point-max) nil)
	(delete-region (point) (point-max))
	(set-buffer-modified-p nil))))

(defun mhc-calendar/view-summary-insert ()
  (let ((date mhc-calendar-view-date)
	(buffer-read-only nil)
	(mhc-use-week-separator nil))
    (goto-char (point-max))
    (insert "\n")
    (mhc-summary/insert-separator)
    (mhc-summary-make-contents date date 'mhc-calendar)
    (delete-char -1)
    (set-buffer-modified-p nil)))

(defun mhc-calendar/put-property-summary ()
  (condition-case nil
      (if mhc-calendar-use-mouse-highlight
	  (let ((buffer-read-only nil))
	    (goto-char (point-min))
	    (if (re-search-forward "^-" nil t)
		(let (beg)
		  (forward-line)
		  (while (not (eobp))
		    (setq beg (point))
		    (end-of-line)
		    (put-text-property beg (point) 'mouse-face 'highlight)
		    (forward-line))))
	    (set-buffer-modified-p nil)))
    (error nil)))

(defun mhc-calendar/shrink-window ()
  (or (one-window-p t)
      (/= (frame-width) (window-width))
      (let ((winh
	     (+ (count-lines (point-min) (point-max))
		(if (and (featurep 'xemacs) window-system) 4 1)))) ;; FIX XEmacs
	(cond
	 ((< winh mhc-calendar-height)
	  (setq winh mhc-calendar-height))
	 ((< winh window-min-height)
	  (setq winh window-min-height)))
	(shrink-window (- (window-height) winh)))))

(defun mhc-calendar/create-buffer (date)
  (set-buffer (get-buffer-create mhc-calendar/buffer))
  (setq buffer-read-only t)
  (if (eq major-mode 'mhc-calendar-mode)
      ()
    (mhc-calendar-mode)
    (buffer-disable-undo))
  (or (integerp date) (setq date (mhc-date-now)))
  (let ((buffer-read-only nil)
	(caldate (mhc-date-mm+ date -1))
	(col mhc-calendar-start-column)
	(prefix " +|")
	(i 3))
    (set-text-properties (point-min) (point-max) nil)
    (erase-buffer)
    (message "mhc-calendar create ...")
    (while (> i 0)
      (goto-char (point-min))
      (mhc-misc-move-to-column col)
      (mhc-misc-insert-rectangle (mhc-calendar/make-rectangle caldate "|"))
      (setq caldate (mhc-date-mm+ caldate 1))
      (setq col (+ col mhc-calendar-next-offset))
      (setq i (1- i)))
    (goto-char (point-min))
    (while (re-search-forward prefix nil t)
      (delete-region (match-end 0) (match-beginning 0))
      (insert (make-string (- (match-end 0) (match-beginning 0)) ?\ )))
    (setq mhc-calendar-date date)
    (mhc-calendar/put-property-date)
    (and mhc-calendar-link-hnf (mhc-calendar/hnf-mark-diary-entries))
    (run-hooks 'mhc-calendar-create-buffer-hook)
    (set-buffer-modified-p nil)
    (message "mhc-calendar create ... done.")))

(defun mhc-calendar/put-property-date ()
  (interactive)
  (condition-case nil
      (let ((cdate (mhc-date-let mhc-calendar-date (mhc-date-new yy mm 1)))
	    beg end yymm dd)
	(goto-char (point-min))
	(while (re-search-forward "\\([ 123][0-9]\\)[ \n]" nil 'end)
	  (setq beg (match-beginning 1) end (match-end 1))
	  (setq dd (1- (string-to-number (buffer-substring beg end))))
	  (goto-char end)
	  (setq yymm (mhc-date-mm+
		      cdate
		      (mhc-calendar/get-date-colnum (current-column))))
	  (put-text-property beg end 'mhc-calendar/date-prop (+ yymm dd))
	  (if mhc-calendar-use-mouse-highlight
	      (put-text-property beg end 'mouse-face 'highlight)))
	(setq beg (- (point) 2) end (point))
	(goto-char beg)
	(setq dd (1- (string-to-number (buffer-substring beg end))))
	(setq yymm (mhc-date-mm+
		    cdate
		    (mhc-calendar/get-date-colnum (current-column))))
	(put-text-property beg end 'mhc-calendar/date-prop (+ yymm dd))
	(if mhc-calendar-use-mouse-highlight
	    (put-text-property beg end 'mouse-face 'highlight)))
    (error nil)))

(defun mhc-calendar-edit ()
  (interactive)
  (if (or (mhc-calendar/in-date-p)
	  (mhc-calendar/in-summary-p))
      (progn
	(mhc-window-push)
	(mhc-edit nil)
	(delete-other-windows))
    (message "Nothing to do in this point.")))

(defun mhc-calendar-delete ()
  (interactive)
  (let ((filename (mhc-calendar/in-summary-p)) key)
    (if (null filename)
	(message "Nothing to do in this point.")
      (setq key (mhc-slot-directory-to-key 
		 (directory-file-name (file-name-directory filename))))
      (mhc-delete-file
       (assoc filename (mhc-slot-records (mhc-slot-get-month-schedule key)))))))

(defun mhc-calendar-modify ()
  (interactive)
  (if (mhc-calendar/in-summary-p)
      (mhc-modify-file (mhc-calendar/in-summary-p))
    (message "Nothing to do in this point.")))

(defun mhc-calendar-toggle-view ()
  (interactive)
  (setq mhc-calendar-view-summary (not mhc-calendar-view-summary))
  (mhc-calendar/goto-date (mhc-calendar-get-date)))

(defun mhc-calendar-goto-view ()
  (interactive)
  (if (mhc-calendar/in-summary-p)
      (mhc-calendar/view-file (mhc-calendar/in-summary-p))
    (if (mhc-calendar/in-summary-hnf-p)
	(mhc-calendar/hnf-view)
      (setq mhc-calendar-view-summary t)
      (mhc-calendar/goto-date (mhc-calendar-get-date))
      (goto-char (next-single-property-change (point) 'mhc-calendar/summary-prop)))))

(defun mhc-calendar/view-file (file)
  (if (and (stringp file) (file-exists-p file))
      (let ((newname (mhc-date-format
		      mhc-calendar-view-date "+%04d/%02d/%02d" yy mm dd)))
	(mhc-window-push)
	(view-file-other-window file)
	(setq view-exit-action 'mhc-calendar-view-exit-action)
	(set-visited-file-name nil)
	(rename-buffer newname 'unique)
	(run-hooks 'mhc-calendar-view-file-hook))
    (message "File does not exist (%s)." file)))

(defun mhc-calendar-get-day-insert (&optional arg)
  (interactive "P")
  (let ((separator mhc-calendar-date-separator)
	(callbuf mhc-calendar-call-buffer)
	str bufalist)
    (if (and (stringp separator)
	     callbuf (get-buffer callbuf) (buffer-name callbuf))
	()
      (message "Input separator(n=null, \\=japanese, others=itself): ")
      (setq separator (char-to-string (read-char)))
      (setq bufalist (mhc-calendar/get-buffer-alist))
      (setq callbuf (completing-read "Insert buffer: " bufalist
				     nil nil (car (car bufalist)))))
    ;; in mhc-calendar/buffer
    (if (null arg)
	(setq str (mhc-calendar/get-day separator))
      (setq str (mhc-calendar/get-day-region
		 (region-beginning) (region-end) separator)))
    ;; in mhc-clendar-call-buffer
    (set-buffer (get-buffer callbuf))
    (pop-to-buffer callbuf)
    (cond
     ((window-minibuffer-p)
      (insert str) t)
     (t (condition-case err
	    (progn
	      (insert str)
	      (message "\"%s\" insert done." str) t)
	  (error
	   (pop-to-buffer (get-buffer mhc-calendar/buffer))
	   (message "\"%s\" insert failed." str) nil))))))

(defun mhc-calendar-get-day-insert-quit (&optional arg)
  (interactive "P")
  (if (mhc-calendar-get-day-insert arg)
      (mhc-calendar-quit)))

(defun mhc-calendar/get-day (&optional separator)
  (let ((date (mhc-calendar-get-date)))
    (if (stringp separator)
	(if (string= separator "n")
	    (setq separator ""))
      (if (stringp mhc-calendar-date-separator)
	  (setq separator mhc-calendar-date-separator)
	(setq separator "")))
    (if (string= separator "\\")
	(mhc-date-format date
			 "%04d年%02d月%02d日(%s)" yy mm dd
			 (mhc-date-digit-to-ww-japanese-string ww))
      (mhc-date-format date "%04d%s%02d%s%02d" yy separator mm separator dd))))

(defun mhc-calendar/get-day-region (beg end &optional separator)
  (let ((cat (if (string= separator "\\") "〜" "-"))
	str-beg str-end)
    (setq str-beg (save-excursion
		    (goto-char beg)
		    (mhc-calendar/get-day separator)))
    (setq str-end (save-excursion
		    (goto-char end)
		    (mhc-calendar/get-day separator)))
    (if (string< str-beg str-end)
	(concat str-beg cat str-end)
      (concat str-end cat str-beg))))

(defun mhc-calendar-get-day-with-character (separator &optional arg)
  (interactive "cInput separator(n=null, \\=japanese, others=itself): \nP")
  (let (str)
    (setq separator (char-to-string separator))
    (if (null arg)
	(setq str (mhc-calendar/get-day separator))
      (setq str (mhc-calendar/get-day-region
		 (region-beginning) (region-end) separator)))
    (kill-new str)
    (message "\"%s\" to the latest kill in the kill ring." str)))

(defun mhc-calendar-get-day-with-slash (&optional arg)
  (interactive "P")
  (mhc-calendar-get-day-with-character ?/ arg))

(defun mhc-calendar-get-day-with-japanese (&optional arg)
  (interactive "P")
  (mhc-calendar-get-day-with-character ?\\ arg))

(defun mhc-calendar-scan (&optional hide-private)
  (interactive "P")
  (if (mhc-calendar/mua-ready-p)
      (let ((date (mhc-calendar-get-date)))
	(mhc-calendar-quit)
	(mhc-goto-month date hide-private)
	(goto-char (point-min))
	(if (mhc-summary-search-date date)
	    (progn
	      (beginning-of-line)
	      (if (not (pos-visible-in-window-p (point)))
		  (recenter)))))
    (message "MUA is not ready.")))

(defun mhc-calendar-quit ()
  (interactive)
  (let ((win (get-buffer-window mhc-calendar/buffer))
	(buf (get-buffer mhc-calendar/buffer)))
    (setq mhc-calendar-date-separator nil)
    (setq mhc-calendar-call-buffer nil)
    (if (null win)
	()
      (bury-buffer buf)
      (if (null (one-window-p))
	  (delete-windows-on buf)
	(set-window-buffer win (other-buffer))
	(select-window (next-window))))))

(defun mhc-calendar-exit ()
  (interactive)
  (mhc-calendar-quit)
  (kill-buffer (get-buffer mhc-calendar/buffer)))

(defun mhc-calendar-goto-month (&optional date)
  (interactive)
  (mhc-calendar/goto-date (if (integerp date) date (mhc-input-month "Month "))))

(defun mhc-calendar-rescan ()
  (interactive)
  (set-buffer (get-buffer mhc-calendar/buffer))
  (let ((cdate mhc-calendar-date)
	(pdate (mhc-calendar-get-date)))
    (setq mhc-calendar-date nil)
    (mhc-calendar/create-buffer cdate)
    (mhc-calendar/goto-date pdate)))

(defun mhc-calendar-day-position ()
  (interactive)
  (mhc-calendar/goto-date (mhc-calendar-get-date))
  (let ((pos (point)))
    (goto-char (point-min))
    (sit-for 0)
    (goto-char pos)))

(defun mhc-calendar-next-day (&optional arg)
  (interactive "p")
  (let ((date (mhc-calendar-get-date)))
    (mhc-calendar/goto-date (+ date arg))))

(defun mhc-calendar-prev-day (&optional arg)
  (interactive "p")
  (mhc-calendar-next-day (- arg)))

(defun mhc-calendar-next-week (&optional arg)
  (interactive "p")
  (if (or (mhc-calendar/in-summary-p) (mhc-calendar/in-summary-hnf-p))
      (let ((pos (point)))
	(forward-line)
	(if (eobp) (goto-char pos)))
    (mhc-calendar-next-day (* arg 7))))

(defun mhc-calendar-prev-week (&optional arg)
  (interactive "p")
  (if (or (mhc-calendar/in-summary-p) (mhc-calendar/in-summary-hnf-p))
      (let ((pos (point)))
	(forward-line -1)
	(if (or (mhc-calendar/in-summary-p) (mhc-calendar/in-summary-hnf-p))
	    ()
	  (goto-char pos)))
    (mhc-calendar-next-day (- (* arg 7)))))

(defun mhc-calendar-next-month (&optional arg)
  (interactive "p")
  (mhc-calendar/goto-date (mhc-date-mm+ (mhc-calendar-get-date) arg)))

(defun mhc-calendar-prev-month (&optional arg)
  (interactive "p")
  (mhc-calendar-next-month (- arg)))

(defun mhc-calendar-next-year (&optional arg)
  (interactive "p")
  (mhc-calendar/goto-date (mhc-date-yy+ (mhc-calendar-get-date) arg)))

(defun mhc-calendar-prev-year  (&optional arg)
  (interactive "p")
  (mhc-calendar-next-year (- arg)))

(defun mhc-calendar-inc-month (&optional arg)
  (interactive "p")
  (set-buffer (get-buffer mhc-calendar/buffer))
  (let* ((dnew (mhc-date-mm+ mhc-calendar-date arg))
	 (ddold (mhc-date-dd (mhc-calendar-get-date)))
	 (dnew2 (mhc-date-let dnew
		  (if (mhc-date/check yy mm ddold)
		      (mhc-date-new yy mm ddold)
		    (mhc-date-new  yy mm (mhc-date/last-day-of-month yy mm))))))
    (mhc-calendar/create-buffer dnew)
    (mhc-calendar/goto-date dnew2)))

(defun mhc-calendar-dec-month (&optional arg)
  (interactive "p")
  (mhc-calendar-inc-month (- arg)))

(defun mhc-calendar-get-date ()
  (when (mhc-calendar-p)
    (if (mhc-calendar/in-date-p)
	(mhc-calendar/in-date-p)
      (if (or (mhc-calendar/in-summary-p) (mhc-calendar/in-summary-hnf-p))
	  mhc-calendar-view-date
	(let* ((pos (point))
	       (col (current-column))
	       (colnum (mhc-calendar/get-date-colnum col))
	       (line (+ (count-lines (point-min) (point)) (if (= col 0) 1 0)))
	       (date (mhc-date-mm+ mhc-calendar-date colnum))
	       (date1 (mhc-date-mm-first date))
	       (datelast (mhc-date-mm-last date))
	       daypos)
	  (cond
	   ((< line 3) date1)
	   ((> line 9) datelast)
	   (t
	    (setq daypos (next-single-property-change (point) 'mhc-calendar/date-prop))
	    (if daypos
		(progn
		  (goto-char daypos)
		  (if (= colnum (mhc-calendar/get-date-colnum (current-column)))
		      (mhc-calendar/in-date-p)
		    (goto-char pos)
		    (if (or (and (goto-char (previous-single-property-change
					     (point) 'mhc-calendar/date-prop))
				 (mhc-calendar/in-date-p))
			    (and (goto-char (previous-single-property-change
					     (point) 'mhc-calendar/date-prop))
				 (mhc-calendar/in-date-p)))
			(if (= colnum (mhc-calendar/get-date-colnum (current-column)))
			    (mhc-calendar/in-date-p)
			  datelast)
		      datelast)))
	      datelast))))))))

(defun mhc-calendar-view-date ()
  (and (mhc-calendar-p) mhc-calendar-view-date))

;; mouse function
(defun mhc-calendar-day-at-mouse(event)
  (interactive "e")
  (set-buffer (mhc-calendar/event-buffer event))
  (pop-to-buffer (mhc-calendar/event-buffer event))
  (goto-char (mhc-calendar/event-point event))
  (cond
   ((mhc-calendar/in-date-p)
    (mhc-calendar-day-position))
   ((mhc-calendar/in-summary-p)
    (mhc-calendar/view-file (mhc-calendar/in-summary-p)))
   ((mhc-calendar/in-summary-hnf-p)
    (mhc-calendar/hnf-view))
   (t (message "Nothing to do in this point."))))

;; misc
(defun mhc-calendar/delete-region (yy mm dd pos)
  (condition-case err
      (if (mhc-date/check yy mm dd)
	  (progn
	    (delete-region (point) pos)
	    (mhc-date-new yy mm dd))
	nil)
    (error nil)))

(defun mhc-calendar/get-buffer-alist ()
  (let ((bl (cdr (buffer-list)))
	(callbufn (if (and mhc-calendar-call-buffer
			   (get-buffer mhc-calendar-call-buffer)
			   (buffer-name mhc-calendar-call-buffer))
		      (buffer-name mhc-calendar-call-buffer)))
	bfal1 bfal2 bn)
    (if callbufn (setq bfal1 (list (list callbufn callbufn))))
    (while bl
      (if (setq bn (buffer-name (car bl)))
	  (if (and callbufn (string= callbufn bn))
	      ()
	    (if (string-match "^ " bn)
		(setq bfal2 (cons (list bn bn) bfal2))
	      (setq bfal1 (cons (list bn bn) bfal1)))))
      (setq bl (cdr bl)))
    (nreverse (cons bfal2 bfal1))))

(defun mhc-calendar-view-exit-action (buff)
  (kill-buffer buff)
  (and (get-buffer mhc-calendar/buffer) (mhc-window-pop)))

;; mhc-minibuffer support
(defun mhc-minibuf-insert-calendar ()
  (interactive)
  (let ((yy 1) (mm 1) (dd 1) date pos)
    (setq mhc-calendar-date-separator "/")
    (setq mhc-calendar-call-buffer (current-buffer))
    (save-excursion
      (setq pos (point))
      (skip-chars-backward "0-9/")
      (cond
       ((looking-at "\\([12][0-9][0-9][0-9]\\)/\\([0-2][0-9]\\)/\\([0-3][0-9]\\)")
	(setq yy (mhc-calendar/buffer-substring-to-num 1))
	(setq mm (mhc-calendar/buffer-substring-to-num 2))
	(setq dd (mhc-calendar/buffer-substring-to-num 3))
	(setq date (mhc-calendar/delete-region yy mm dd pos)))
       ((looking-at "\\([12][0-9][0-9][0-9]\\)/\\([0-2][0-9]\\)/?")
	(setq yy (mhc-calendar/buffer-substring-to-num 1))
	(setq mm (mhc-calendar/buffer-substring-to-num 2))
	(setq date (mhc-calendar/delete-region yy mm dd pos)))
       ((looking-at "\\([12][0-9][0-9][0-9]\\)/?")
	(setq yy (mhc-calendar/buffer-substring-to-num 1))
	(setq date (mhc-calendar/delete-region yy mm dd pos)))))
    (mhc-calendar date)))

;; mhc-draft support
(defun mhc-draft-insert-calendar ()
  (interactive)
  (let ((yy 1) (mm 1) (dd 1) date pos)
    (setq mhc-calendar-call-buffer (current-buffer))
    (save-excursion
      (setq pos (point))
      (goto-char (point-min))
      (if (and (re-search-forward "^$" nil t)
	       (< pos (point)))
	  (setq mhc-calendar-date-separator "")
	(setq mhc-calendar-date-separator nil))
      (goto-char pos)
      (skip-chars-backward "0-9")
      (cond
       ((looking-at "\\([12][0-9][0-9][0-9]\\)\\([0-2][0-9]\\)\\([0-3][0-9]\\)")
	(setq yy (mhc-calendar/buffer-substring-to-num 1))
	(setq mm (mhc-calendar/buffer-substring-to-num 2))
	(setq dd (mhc-calendar/buffer-substring-to-num 3))
	(setq date (mhc-calendar/delete-region yy mm dd pos)))
       ((looking-at "\\([12][0-9][0-9][0-9]\\)\\([0-2][0-9]\\)")
	(setq yy (mhc-calendar/buffer-substring-to-num 1))
	(setq mm (mhc-calendar/buffer-substring-to-num 2))
	(setq date (mhc-calendar/delete-region yy mm dd pos)))
       ((looking-at "\\([12][0-9][0-9][0-9]\\)")
	(setq yy (mhc-calendar/buffer-substring-to-num 1))
	(setq date (mhc-calendar/delete-region yy mm dd pos)))))
    (mhc-calendar date)))

;; hnf-mode interface
(defun mhc-calendar/hnf-get-filename (date)
  (expand-file-name
   (mhc-date-format date "d%04d%02d%02d.hnf" yy mm dd)
   (if hnf-diary-year-directory-flag
       (expand-file-name (mhc-date-format date "%04d" yy) hnf-diary-dir)
     hnf-diary-dir)))

(defun mhc-calendar/hnf-file-list (date)
  (let ((i -1) flst)
    (setq date (mhc-date-mm+ date -1))
    (while (< i 2)
      (let* ((dir (if hnf-diary-year-directory-flag
		      (expand-file-name (mhc-date-format date "%04d" yy) hnf-diary-dir)
		    (expand-file-name hnf-diary-dir)))
	     (fnexp (mhc-date-format date "d%04d%02d[0-3][0-9]\\.hnf" yy mm)))
	(if (file-directory-p dir)
	    (setq flst (append (directory-files dir nil fnexp 'no-sort) flst))
	  (setq flst nil))
	(setq date (mhc-date-mm+ date 1))
	(setq i (1+ i))))
    flst))

(defun mhc-calendar-hnf-edit ()
  (interactive)
  (if mhc-calendar-link-hnf
      (let ((find-file-not-found-hooks nil)
	    (count (mhc-calendar/in-summary-hnf-p)))
	(if (functionp hnf-initial-function) ;; hns-mode require APEL :-)
	    (add-hook 'find-file-not-found-hooks
		      '(lambda () (funcall hnf-initial-function))))
	(find-file-other-window
	 (mhc-calendar/hnf-get-filename (mhc-calendar-get-date)))
	(and (integerp count) (mhc-calendar/hnf-search-title count)))))

(defun mhc-calendar/hnf-view ()
  (interactive)
  (let ((fname (mhc-calendar/hnf-get-filename (mhc-calendar-get-date)))
	(count (mhc-calendar/in-summary-hnf-p)))
    (if (not (file-readable-p fname))
	(message "File does not exist (%s)." fname)
      (mhc-window-push)
      (view-file-other-window fname)
      (setq view-exit-action 'mhc-calendar-view-exit-action)
      (and (integerp count) (mhc-calendar/hnf-search-title count)))))

(defun mhc-calendar/hnf-search-title (count)
  (goto-char (point-min))
  (while (and (> count 0) (not (eobp)))
    (re-search-forward "^\\(L?NEW\\|L?SUB\\)[ \t]+" nil t)
    (setq count (1- count)))
  (beginning-of-line)
  (recenter (/ (window-height) 4)))

(defun mhc-calendar/hnf-mark-diary-entries ()
  (let ((cdate (mhc-date-mm-first (mhc-date-mm+ mhc-calendar-date -1)))
	(edate (mhc-date-mm-last (mhc-date-mm+ mhc-calendar-date 1)))
	(flst (mhc-calendar/hnf-file-list mhc-calendar-date))
	(mark "'"))
    (mhc-face-put mark 'mhc-calendar-hnf-face-mark)
    (while (<= cdate edate)
      (if (member (mhc-date-format cdate "d%04d%02d%02d.hnf" yy mm dd) flst)
	  (progn
	    (goto-char (+ 2 (mhc-calendar/tp-any (point-min) (point-max)
						 'mhc-calendar/date-prop cdate)))
	    (insert mark)
	    (if (eq (char-after (point)) ?\ )
		(delete-char 1))))
      (setq cdate (1+ cdate)))))

(defun mhc-calendar/hnf-summary-insert ()
  (let ((fname (mhc-calendar/hnf-get-filename mhc-calendar-view-date))
	(buffer-read-only nil)
	(newmark "#") (sub "＠") (cat "")
	(count 1) (ncount 1)
	new summary str uri)
    (if (not (file-readable-p fname))
	()
      (goto-char (point-max))
      (with-temp-buffer              ;; hnf-mode.el require APEL :-)
	(insert-file-contents fname)
	(goto-char (point-min))
	(mhc-face-put sub 'mhc-calendar-hnf-face-subtag)
	(while (not (eobp))
	  (cond
	   ;; CAT
	   ((looking-at "^CAT[ \t]+\\(.*\\)$")
	    (setq cat (buffer-substring (match-beginning 1) (match-end 1)))
	    (while (string-match "[ \t]+" cat)
	      (setq cat (concat (substring cat 0 (match-beginning 0))
				"]["
				(substring cat (match-end 0)))))
	    (setq cat (concat "[" cat "]"))
	    (mhc-face-put cat 'mhc-calendar-hnf-face-cat)
	    (setq cat (concat cat " ")))
	   ;; NEW
	   ((looking-at "^NEW[ \t]+\\(.*\\)$")
	    (setq str (buffer-substring (match-beginning 1) (match-end 1)))
	    (mhc-face-put str 'mhc-calendar-hnf-face-new)
	    (setq new (format "%s%d" newmark ncount))
	    (mhc-face-put new 'mhc-calendar-hnf-face-newtag)
	    (setq str (concat "     " new " " cat str "\n"))
	    (put-text-property 0 (length str) 'mhc-calendar/summary-hnf-prop count str)
	    (setq summary (concat summary str)
		  count (1+ count)
		  ncount (1+ ncount)
		  cat ""))
	   ;; LNEW
	   ((looking-at "^LNEW[ \t]+\\([^ \t]+\\)[ \t]+\\(.*\\)$")
	    (setq uri (concat "<"
			      (buffer-substring (match-beginning 1) (match-end 1))
			      ">"))
	    (mhc-face-put uri 'mhc-calendar-hnf-face-uri)
	    (setq str (buffer-substring (match-beginning 2) (match-end 2)))
	    (mhc-face-put str 'mhc-calendar-hnf-face-new)
	    (setq new (format "%s%d" newmark ncount))
	    (mhc-face-put new 'mhc-calendar-hnf-face-newtag)
	    (setq str (concat "     " new " " cat str " " uri "\n"))
	    (put-text-property 0 (length str) 'mhc-calendar/summary-hnf-prop count str)
	    (setq summary (concat summary str)
		  count (1+ count)
		  ncount (1+ ncount)
		  cat ""))
	   ;; SUB
	   ((looking-at "^SUB[ \t]+\\(.*\\)$")
	    (setq str (buffer-substring (match-beginning 1) (match-end 1)))
	    (mhc-face-put str 'mhc-calendar-hnf-face-sub)
	    (setq str (concat "       " sub " " cat str "\n"))
	    (put-text-property 0 (length str) 'mhc-calendar/summary-hnf-prop count str)
	    (setq summary (concat summary str)
		  count (1+ count)
		  cat ""))
	   ;; LSUB
	   ((looking-at "^LSUB[ \t]+\\([^ \t]+\\)[ \t]+\\(.*\\)$")
	    (setq uri (concat "<"
			      (buffer-substring (match-beginning 1) (match-end 1))
			      ">"))
	    (mhc-face-put uri 'mhc-calendar-hnf-face-uri)
	    (setq str (buffer-substring (match-beginning 2) (match-end 2)))
	    (mhc-face-put str 'mhc-calendar-hnf-face-sub)
	    (setq str (concat "       " sub " " cat str " " uri "\n"))
	    (put-text-property 0 (length str) 'mhc-calendar/summary-hnf-prop count str)
	    (setq summary (concat summary str)
		  count (1+ count)
		  cat "")))
	  (forward-line)))
      (if summary (insert "\n" summary))
      (delete-char -1)
      (set-buffer-modified-p nil))))

(defun mhc-calendar-hnf-face-setup ()
  (interactive)
  (let ((ow (interactive-p)))
    (mhc-face-setup-internal mhc-calendar-hnf-face-alist ow)
    (mhc-face-setup-internal mhc-calendar-hnf-face-alist-internal nil)))

;; MUA ready?
(defun mhc-calendar/mua-ready-p ()
  (cond
   ((eq mhc-mailer-package 'mew)
    (featurep 'mhc-mew))
   ((eq mhc-mailer-package 'wl)
    (featurep 'mhc-wl))
   ((eq mhc-mailer-package 'gnus)
    (featurep 'mhc-gnus))
   ((eq mhc-mailer-package 'cmail)
    (featurep 'mhc-cmail))
   (t nil)))

;;; Pseudo MUA Backend Methods:
(defun mhc-calendar-insert-summary-contents (inserter)
  (let ((beg (point))
	(name (or (mhc-record-name
		   (mhc-schedule-record mhc-tmp-schedule))
		  "Dummy")))
    (funcall inserter)
    (put-text-property beg (point) 'mhc-calendar/summary-prop name)
    (insert "\n")))


(provide 'mhc-calendar)
(put 'mhc-calendar 'insert-summary-contents 'mhc-calendar-insert-summary-contents)


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

;; mhc-calendar.el ends here
