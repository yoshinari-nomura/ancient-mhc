;;; -*- emacs-lisp -*-
;; mhc-calendar.el -- MHC Mini calendar
;;
;; Author:  Hideyuki SHIRAI <shirai@rdmg.mgcs.mei.co.jp>
;;          MIYOSHI Masanori <miyoshi@ask.ne.jp>
;;
;; Created: 05/12/2000
;; Revised: 06/14/2000

(condition-case nil
    (require 'hnf-mode)
  (error))

;; internal variables
(defvar mhc-calendar-buffer "*mhc-calendar*")
(defvar mhc-calendar-ddate '(1 1 1))
(defvar mhc-calendar-view-ddate '(1 1 1))
(defvar mhc-calendar-call-buffer nil)
(defvar mhc-calendar-separator nil)
(defvar mhc-calendar-mode-map nil)
(defvar mhc-calendar-mode-menu-spec nil)

;; mhc-calendar functions
;; macros
(defmacro mhc-calendar-in-day-p () ;; return 'ddate'
  (` (get-text-property (point) 'mhc-calendar-day-prop)))

(defmacro mhc-calendar-in-summary-p () ;; return 'schedule filename'
  (` (save-excursion
       (beginning-of-line)
       (get-text-property (point) 'mhc-calendar-summary-prop))))

(defmacro mhc-calendar-in-summary-hnf-p () ;; return 'title count'
  (` (save-excursion
       (beginning-of-line)
       (get-text-property (point) 'mhc-calendar-summary-hnf-prop))))

(defmacro mhc-calendar-get-ddate-colnum (col)
  (` (cond
      ((< (, col) (+ mhc-calendar-next-offset mhc-calendar-start-column)) -1)
      ((< (, col) (+ (* mhc-calendar-next-offset 2) mhc-calendar-start-column)) 0)
      (t 1))))

;; map/menu
(if mhc-calendar-mode-map
    ()
  (setq mhc-calendar-mode-map (make-sparse-keymap))
  (define-key mhc-calendar-mode-map "."    'mhc-calendar)
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
	  ["Goto today" mhc-calendar t]
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
	  ["MHC summary scan" mhc-calendar-scan (mhc-mua-ready-p)]
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
	   (not (or (mhc-calendar-in-summary-p) (mhc-calendar-in-summary-hnf-p)))]
	  "----"
	  ["Schedule view" mhc-calendar-goto-view
	   (or (mhc-calendar-in-summary-p) (mhc-calendar-in-summary-hnf-p))]
	  ("Schedule edit"
	   ["Schedule addition" mhc-calendar-edit
	    (or (mhc-calendar-in-day-p) (mhc-calendar-in-summary-p))]
	   ["Schedule modify" mhc-calendar-modify (mhc-calendar-in-summary-p)]
	   ["Schedule delete" mhc-calendar-delete (mhc-calendar-in-summary-p)]
	   ["HNF file edit" mhc-calendar-hnf-edit
	    (and mhc-calendar-link-hnf
		 (or (mhc-calendar-in-day-p) (mhc-calendar-in-summary-p)
		     (mhc-calendar-in-summary-hnf-p)))])
	  "----"
	  ("Misc"
	   ["Quit" mhc-calendar-quit t]
	   ["Kill" mhc-calendar-exit t]
	   ["Help" describe-mode t]))))

;; function
(defun mhc-calendar-mode ()
  "\\<mhc-calendar-mode-map>
MHC Calendar mode:: major mode to view calendar and select day.

The keys that are defined for mhc-calendar-mode are:
\\[mhc-calendar]	Jump to today.
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
  (make-local-variable 'mhc-calendar-ddate)
  (make-local-variable 'mhc-calendar-view-ddate)
  (make-local-variable 'indent-tabs-mode)
  (setq major-mode 'mhc-calendar-mode)
  (setq mode-name "mhc-calendar")
  (setq indent-tabs-mode nil)
  (setq truncate-lines t)
  (if (featurep 'xemacs) (easy-menu-add mhc-calendar-mode-menu))
  (run-hooks 'mhc-calendar-mode-hook))

;;;###autoload
(defun mhc-calendar (&optional ddate)
  "MHC 3-month mini calender."
  (interactive)
  (or mhc-setup-p (mhc-setup))
  (mhc-calendar-goto-day (or ddate (mhc-current-ddate) (ddate-now))))

(defun mhc-calendar-goto-day (ddate)
  (let ((mhc-calendar-view-summary nil)
	pos)
    (if (not (get-buffer mhc-calendar-buffer))
	(mhc-calendar-create-buffer ddate))
    (set-buffer (get-buffer mhc-calendar-buffer))
    (pop-to-buffer mhc-calendar-buffer)
    (while (not pos)
      (setq pos (mhc-calendar-tp-any (point-min) (point-max)
				     'mhc-calendar-day-prop ddate))
      (or pos (mhc-calendar-create-buffer ddate)))
    (goto-char (1+ pos))
    (setq mhc-calendar-view-ddate ddate))
  (save-excursion
    (mhc-calendar-view-summary-delete)
    (if mhc-calendar-view-summary
	(progn
	  (mhc-calendar-view-summary-insert)
	  (if mhc-calendar-link-hnf
	      (mhc-calendar-hnf-summary-insert))
	  (mhc-calendar-put-property-summary))))
  (mhc-calendar-shrink-window))

(defun mhc-calendar-tp-any (beg end prop value)
  "Modify text-property-any use 'equal' instated 'eq'."
  (while (and beg (< beg end)
	      (not (equal value (get-text-property beg prop))))
    (setq beg (next-single-property-change beg prop nil end)))
  (if (eq beg end) nil beg))

(defun mhc-calendar-view-summary-delete ()
  (goto-char (point-min))
  (if (re-search-forward "^--" nil t)
      (let ((buffer-read-only nil))
	(beginning-of-line)
	(forward-char -1)
	(set-text-properties (point) (point-max) nil)
	(delete-region (point) (point-max))
	(set-buffer-modified-p nil))))

(defun mhc-calendar-view-summary-insert ()
  (let ((ddate mhc-calendar-view-ddate)
	(buffer-read-only nil))
    (goto-char (point-max))
    (insert "\n")
    (mhc-summary/insert-separator)
    (let ((day (ddate-days ddate))
	  (mhc-use-week-separator))
      (mhc-summary-make-contents day day 'mhc-calendar))
    (delete-char -1)
    (set-buffer-modified-p nil)))

(defun mhc-calendar-put-property-summary ()
  (interactive)
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

(defun mhc-calendar-shrink-window ()
  (or (one-window-p t)
      (/= (frame-width) (window-width))
      (let ((winh (+ (count-lines (point-min) (point-max))
		     (if (and (featurep 'xemacs) window-system) 4 1))))
	(cond
	 ((< winh mhc-calendar-height)
	  (setq winh mhc-calendar-height))
	 ((< winh window-min-height)
	  (setq winh window-min-height)))
	(shrink-window (- (window-height) winh)))))

(defun mhc-calendar-create-buffer (ddate)
  (set-buffer (get-buffer-create mhc-calendar-buffer))
  (setq buffer-read-only t)
  (if (eq major-mode 'mhc-calendar-mode)
      ()
    (mhc-calendar-mode)
    (buffer-disable-undo))
  (if (null (ddate-dd ddate))
      (setq ddate (ddate-new (ddate-yy ddate) (ddate-mm ddate) 1)))
  (let ((buffer-read-only nil)
	(caldate (ddate-mm-dec ddate))
	(col mhc-calendar-start-column)
	(prefix " +|") (spcchar (string-to-char " ")) (i 3))
    (set-text-properties (point-min) (point-max) nil)
    (erase-buffer)
    (message "mhc-calendar create ...")
    (while (> i 0)
      (goto-char (point-min))
      (mhc-misc-move-to-column col)
      (mhc-misc-insert-rectangle (mhc-cal-make-rectangle caldate))
      (setq caldate (ddate-mm-inc caldate))
      (setq col (+ col mhc-calendar-next-offset))
      (setq i (1- i)))
    (goto-char (point-min))
    (while (re-search-forward prefix nil t)
      (delete-region (match-end 0) (match-beginning 0))
      (insert (make-string (- (match-end 0) (match-beginning 0)) spcchar)))
    (setq mhc-calendar-ddate ddate)
    (mhc-calendar-put-property-day)
    (and mhc-calendar-link-hnf (mhc-calendar-hnf-mark-diary-entries))
    (run-hooks 'mhc-calendar-create-buffer-hook)
    (set-buffer-modified-p nil)
    (message "mhc-calendar create ... done.")))

(defun mhc-calendar-put-property-day ()
  (interactive)
  (condition-case nil
      (let (beg end yymm dd)
	(goto-char (point-min))
	(while (re-search-forward "\\([ 123][0-9]\\)[ \n]" nil 'end)
	  (setq beg (match-beginning 1) end (match-end 1))
	  (setq dd (string-to-number (buffer-substring beg end)))
	  (goto-char end)
	  (setq yymm (ddate-mm-inc mhc-calendar-ddate
				   (mhc-calendar-get-ddate-colnum (current-column))))
	  (put-text-property beg end 'mhc-calendar-day-prop
			     (list (ddate-yy yymm) (ddate-mm yymm) dd))
	  (if mhc-calendar-use-mouse-highlight
	      (put-text-property beg end 'mouse-face 'highlight)))
	(setq beg (- (point) 2) end (point))
	(goto-char beg)
	(setq dd (string-to-number (buffer-substring beg end)))
	(setq yymm (ddate-mm-inc mhc-calendar-ddate
				 (mhc-calendar-get-ddate-colnum (current-column))))
	(put-text-property beg end 'mhc-calendar-day-prop
			   (list (ddate-yy yymm) (ddate-mm yymm) dd))
	(if mhc-calendar-use-mouse-highlight
	    (put-text-property beg end 'mouse-face 'highlight)))
    (error nil)))

(defun mhc-calendar-edit ()
  (interactive)
  (if (or (mhc-calendar-in-day-p)
	  (mhc-calendar-in-summary-p))
      (progn
	(mhc-window-push)
	(mhc-edit nil 'calendar)
	(delete-other-windows))
    (message "Nothing to do in this point.")))

(defun mhc-calendar-delete ()
  (interactive)
  (let ((filename (mhc-calendar-in-summary-p)) key)
    (if (null filename)
	(message "Nothing to do in this point.")
      (setq key (mhc-slot-directory-to-key 
		 (directory-file-name (file-name-directory filename))))
      (mhc-delete-file
       (assoc filename (mhc-slot-records (mhc-slot-get-month-schedule key)))))))

(defun mhc-calendar-modify ()
  (interactive)
  (if (mhc-calendar-in-summary-p)
      (mhc-modify-file (mhc-calendar-in-summary-p))
    (message "Nothing to do in this point.")))

(defun mhc-calendar-toggle-view ()
  (interactive)
  (setq mhc-calendar-view-summary (not mhc-calendar-view-summary))
  (mhc-calendar-goto-day (mhc-calendar-get-ddate)))

(defun mhc-calendar-goto-view ()
  (interactive)
  (if (mhc-calendar-in-summary-p)
      (mhc-calendar-view-file (mhc-calendar-in-summary-p))
    (if (mhc-calendar-in-summary-hnf-p)
	(mhc-calendar-hnf-view)
      (setq mhc-calendar-view-summary t)
      (mhc-calendar-goto-day (mhc-calendar-get-ddate))
      (goto-char (next-single-property-change (point) 'mhc-calendar-summary-prop)))))

(defun mhc-calendar-view-file (file)
  (if (and (stringp file) (file-exists-p file))
      (let ((newname (mapconcat (lambda (x) (format "%02d" x))
				mhc-calendar-view-ddate "/")))
	(mhc-window-push)
	(view-file-other-window file)
	(setq view-exit-action 'mhc-calendar-view-exit-action)
	(set-visited-file-name nil)
	(rename-buffer newname 'unique)
	(run-hooks 'mhc-calendar-view-file-hook))
    (message "File does not exist (%s)." file)))

(defun mhc-calendar-get-day-insert (&optional arg)
  (interactive "P")
  (let ((separator mhc-calendar-separator)
	(callbuf mhc-calendar-call-buffer)
	str bufalist)
    (if (and (stringp separator)
	     callbuf (get-buffer callbuf) (buffer-name callbuf))
	()
      (message "Input separator(n=null, \\=japanese, others=itself): ")
      (setq separator (char-to-string (read-char)))
      (setq bufalist (mhc-calendar-get-buffer-alist))
      (setq callbuf (completing-read "Insert buffer: " bufalist
				     nil nil (car (car bufalist)))))
    ;; in mhc-calendar-buffer
    (if (null arg)
	(setq str (mhc-calendar-get-day separator))
      (setq str (mhc-calendar-get-day-region
		 (region-beginning) (region-end) separator)))
    ;; in mhc-clendar-call-buffer
    (set-buffer (get-buffer callbuf))
    (pop-to-buffer callbuf)
    (cond
     ((window-minibuffer-p)
      (insert str) t)
     (t
      (condition-case err
	  (progn
	    (insert str)
	    (message "\"%s\" insert done." str) t)
	(error
	 (pop-to-buffer (get-buffer mhc-calendar-buffer))
	 (message "\"%s\" insert failed." str) nil))))))

(defun mhc-calendar-get-day-insert-quit (&optional arg)
  (interactive "P")
  (if (mhc-calendar-get-day-insert arg)
      (mhc-calendar-quit)))

(defun mhc-calendar-get-day (&optional separator)
  (let ((ddate (mhc-calendar-get-ddate)))
    (if (stringp separator)
	(if (string= separator "n")
	    (setq separator ""))
      (if (stringp mhc-calendar-separator)
	  (setq separator mhc-calendar-separator)
	(setq separator "")))
    (if (string= separator "\\")
	(format "%d年%d月%d日(%s)"
		(ddate-yy ddate) (ddate-mm ddate) (ddate-dd ddate)
		(nth (ddate-ww ddate) (list "日" "月" "火" "水" "木" "金" "土")))
      (mapconcat (lambda (x) (format "%02d" x)) ddate separator))))

(defun mhc-calendar-get-day-region (beg end &optional separator)
  (let (str-beg str-end)
    (setq str-beg (save-excursion
		    (goto-char beg)
		    (mhc-calendar-get-day separator)))
    (setq str-end (save-excursion
		    (goto-char end)
		    (mhc-calendar-get-day separator)))
    (if (string< str-beg str-end)
	(concat str-beg "-" str-end)
      (concat str-end "-" str-beg))))

(defun mhc-calendar-get-day-with-character (separator &optional arg)
  (interactive "cInput separator(n=null, \\=japanese, others=itself): \nP")
  (let (str)
    (setq separator (char-to-string separator))
    (if (null arg)
	(setq str (mhc-calendar-get-day separator))
      (setq str (mhc-calendar-get-day-region
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
  (if (mhc-mua-ready-p)
      (let ((ddate (mhc-calendar-get-ddate)))
	(mhc-calendar-quit)
	(mhc-goto-month ddate hide-private)
	(goto-char (point-min))
	(if (re-search-forward
	     (format "^\\([0-9]+\\)? | %s" (ddate-mmdd-s1 ddate "/")) nil t)
	    (progn
	      (beginning-of-line)
	      (if (not (pos-visible-in-window-p (point)))
		  (recenter)))))
    (message "MUA is not ready.")))

(defun mhc-calendar-quit ()
  (interactive)
  (let ((win (get-buffer-window mhc-calendar-buffer))
	(buf (get-buffer mhc-calendar-buffer)))
    (setq mhc-calendar-separator nil)
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
  (kill-buffer (get-buffer mhc-calendar-buffer)))

(defun mhc-calendar-goto-month (&optional ddate)
  (interactive)
  (mhc-calendar-goto-day (if (ddate-parse ddate) ddate (mhc-input-month "Month "))))

(defun mhc-calendar-rescan ()
  (interactive)
  (set-buffer (get-buffer mhc-calendar-buffer))
  (let ((cddate mhc-calendar-ddate)
	(pddate (mhc-calendar-get-ddate)))
    (setq mhc-calendar-ddate '(1 1 1))
    (mhc-calendar-create-buffer cddate)
    (mhc-calendar-goto-day pddate)))

(defun mhc-calendar-day-position ()
  (interactive)
  (mhc-calendar-goto-day (mhc-calendar-get-ddate))
  (let ((pos (point)))
    (goto-char (point-min))
    (sit-for 0)
    (goto-char pos)))

(defun mhc-calendar-next-day (&optional arg)
  (interactive "p")
  (let ((ddate (mhc-calendar-get-ddate)))
    (if (< arg 0)
	(while (< arg 0)
	  (setq ddate (ddate-dec ddate))
	  (setq arg (1+ arg)))
      (while (> arg 0)
	(setq ddate (ddate-inc ddate))
	(setq arg (1- arg))))
    (mhc-calendar-goto-day ddate)))

(defun mhc-calendar-prev-day (&optional arg)
  (interactive "p")
  (mhc-calendar-next-day (- arg)))

(defun mhc-calendar-next-week (&optional arg)
  (interactive "p")
  (if (or (mhc-calendar-in-summary-p)  (mhc-calendar-in-summary-hnf-p))
      (let (pos)
	(setq pos (point))
	(forward-line)
	(if (eobp) (goto-char pos)))
    (mhc-calendar-next-day (* arg 7))))

(defun mhc-calendar-prev-week (&optional arg)
  (interactive "p")
  (if (or (mhc-calendar-in-summary-p)  (mhc-calendar-in-summary-hnf-p))
      (let (pos)
	(setq pos (point))
	(forward-line -1)
	(if (or (mhc-calendar-in-summary-p)  (mhc-calendar-in-summary-hnf-p))
	    ()
	  (goto-char pos)))
    (mhc-calendar-next-day (- (* arg 7)))))

(defun mhc-calendar-next-month (&optional arg)
  (interactive "p")
  (mhc-calendar-goto-day (ddate-mm-inc (mhc-calendar-get-ddate) arg)))

(defun mhc-calendar-prev-month (&optional arg)
  (interactive "p")
  (mhc-calendar-next-month (- arg)))

(defun mhc-calendar-next-year (&optional arg)
  (interactive "p")
  (mhc-calendar-goto-day (ddate-yy-inc (mhc-calendar-get-ddate) arg)))

(defun mhc-calendar-prev-year  (&optional arg)
  (interactive "p")
  (mhc-calendar-next-year (- arg)))

(defun mhc-calendar-inc-month (&optional arg)
  (interactive "p")
  (set-buffer (get-buffer mhc-calendar-buffer))
  (let* ((dnew (ddate-mm-inc mhc-calendar-ddate arg))
	 (dold (mhc-calendar-get-ddate))
	 (yynew (ddate-yy dnew))
	 (mmnew (ddate-mm dnew)))
    (setq dnew (ddate-new yynew mmnew (ddate-dd dold) 'noerror))
    (or dnew
	(setq dnew (ddate-new yynew mmnew
			      (ddate-days-of-mm (list yynew mmnew 1)))))
    (setq mhc-calendar-ddate '(1 1 1))
    (mhc-calendar-create-buffer dnew)
    (mhc-calendar-goto-day dnew)))

(defun mhc-calendar-dec-month (&optional arg)
  (interactive "p")
  (mhc-calendar-inc-month (- arg)))

(defun mhc-calendar-get-ddate ()
  (if (mhc-calendar-in-day-p)
      (mhc-calendar-in-day-p)
    (if (or (mhc-calendar-in-summary-p) (mhc-calendar-in-summary-hnf-p))
	mhc-calendar-view-ddate
      (let* ((pos (point))
	     (col (current-column))
	     (colnum (mhc-calendar-get-ddate-colnum col))
	     (line (+ (count-lines (point-min) (point)) (if (= col 0) 1 0)))
	     (yymm (ddate-mm-inc mhc-calendar-ddate colnum))
	     (yymmlast (list (ddate-yy yymm) (ddate-mm yymm) (ddate-days-of-mm yymm)))
	     daypos)
	(cond
	 ((< line 3) (list (ddate-yy yymm) (ddate-mm yymm) 1))
	 ((> line 9) yymmlast)
	 (t
	  (setq daypos (next-single-property-change (point) 'mhc-calendar-day-prop))
	  (if daypos
	      (progn
		(goto-char daypos)
		(if (= colnum (mhc-calendar-get-ddate-colnum (current-column)))
		    (mhc-calendar-in-day-p)
		  (goto-char pos)
		  (if (or (and (goto-char (previous-single-property-change
					   (point) 'mhc-calendar-day-prop))
			       (mhc-calendar-in-day-p))
			  (and (goto-char (previous-single-property-change
					   (point) 'mhc-calendar-day-prop))
			       (mhc-calendar-in-day-p)))
		      (if (= colnum (mhc-calendar-get-ddate-colnum (current-column)))
			  (mhc-calendar-in-day-p)
			yymmlast)
		    yymmlast)))
	    yymmlast)))))))

;; mouse action control for FSF emacs
(eval-and-compile
  (or (fboundp 'event-buffer)
      (defun event-buffer (event)
	(window-buffer (posn-window (event-start event))))))

(eval-and-compile
  (or (fboundp 'event-point)
      (defun event-point (event)
	(posn-point (event-start event)))))

;; mouse function
(defun mhc-calendar-day-at-mouse(event)
  (interactive "e")
  (set-buffer (event-buffer event))
  (pop-to-buffer (event-buffer event))
  (goto-char (event-point event))
  (cond
   ((mhc-calendar-in-day-p)
    (mhc-calendar-day-position))
   ((mhc-calendar-in-summary-p)
    (mhc-calendar-view-file (mhc-calendar-in-summary-p)))
   ((mhc-calendar-in-summary-hnf-p)
    (mhc-calendar-hnf-view))
   (t (message "Nothing to do in this point."))))

;; misc
(defun mhc-calendar-delete-region (yy mm dd pos)
  (condition-case err
      (if (ddate-parse (ddate-new yy mm dd))
	  (progn
	    (delete-region (point) pos)
	    (ddate-new yy mm dd))
	nil)
    (error nil)))

(defun mhc-calendar-get-buffer-alist ()
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
  (and (get-buffer mhc-calendar-buffer) (mhc-window-pop)))

;; mhc-minibuffer support
(defun mhc-minibuf-insert-calendar ()
  (interactive)
  (let ((yy 1) (mm 1) (dd 1) ddate pos)
    (setq mhc-calendar-separator "/")
    (setq mhc-calendar-call-buffer (current-buffer))
    (save-excursion
      (setq pos (point))
      (skip-chars-backward "0-9/")
      (cond
       ((looking-at "\\([0-9][0-9][0-9][0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9]\\)")
	(setq yy (ddate-substring-to-int nil 1))
	(setq mm (ddate-substring-to-int nil 2))
	(setq dd (ddate-substring-to-int nil 3))
	(setq ddate (mhc-calendar-delete-region yy mm dd pos)))
       ((looking-at "\\([0-9][0-9][0-9][0-9]\\)/\\([0-9][0-9]\\)/?")
	(setq yy (ddate-substring-to-int nil 1))
	(setq mm (ddate-substring-to-int nil 2))
	(setq ddate (mhc-calendar-delete-region yy mm dd pos)))
       ((looking-at "\\([0-9][0-9][0-9][0-9]\\)/?")
	(setq yy (ddate-substring-to-int nil 1))
	(setq ddate (mhc-calendar-delete-region yy mm dd pos)))))
    (mhc-calendar ddate)))

;; mhc-draft support
(defun mhc-draft-insert-calendar ()
  (interactive)
  (let ((yy 1) (mm 1) (dd 1) ddate pos)
    (setq mhc-calendar-call-buffer (current-buffer))
    (save-excursion
      (setq pos (point))
      (goto-char (point-min))
      (if (and (re-search-forward "^$" nil t)
	       (< pos (point)))
	  (setq mhc-calendar-separator "")
	(setq mhc-calendar-separator nil))
      (goto-char pos)
      (skip-chars-backward "0-9")
      (cond
       ((looking-at "\\([0-9][0-9][0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)")
	(setq yy (ddate-substring-to-int nil 1))
	(setq mm (ddate-substring-to-int nil 2))
	(setq dd (ddate-substring-to-int nil 3))
	(setq ddate (mhc-calendar-delete-region yy mm dd pos)))
       ((looking-at "\\([0-9][0-9][0-9][0-9]\\)\\([0-9][0-9]\\)")
	(setq yy (ddate-substring-to-int nil 1))
	(setq mm (ddate-substring-to-int nil 2))
	(setq ddate (mhc-calendar-delete-region yy mm dd pos)))
       ((looking-at "\\([0-9][0-9][0-9][0-9]\\)")
	(setq yy (ddate-substring-to-int nil 1))
	(setq ddate (mhc-calendar-delete-region yy mm dd pos)))))
    (mhc-calendar ddate)))

;; hnf-mode interface
(defun mhc-calendar-hnf-get-filename (ddate)
  (expand-file-name
   (format "d%s.hnf" (ddate-to-s ddate))
   (if hnf-diary-year-directory-flag
       (expand-file-name (ddate-yy-s ddate) hnf-diary-dir)
     hnf-diary-dir)))

(defun mhc-calendar-hnf-file-list (ddate)
  (let ((i -1) flst)
    (setq ddate (ddate-mm-inc ddate -1))
    (while (< i 2)
      (let* ((dir (if hnf-diary-year-directory-flag
		      (expand-file-name (ddate-yy-s ddate) hnf-diary-dir)
		    (expand-file-name hnf-diary-dir)))
	     (fnexp (format "d%s[0-3][0-9]\\.hnf" (ddate-yymm-s ddate))))
	(if (file-directory-p dir)
	    (setq flst (append (directory-files dir nil fnexp 'no-sort) flst))
	  (setq flst nil))
	(setq ddate (ddate-mm-inc ddate))
	(setq i (1+ i))))
    flst))

(defun mhc-calendar-hnf-edit ()
  (interactive)
  (if mhc-calendar-link-hnf
      (let ((find-file-not-found-hooks nil)
	    (count (mhc-calendar-in-summary-hnf-p)))
	(if (functionp hnf-initial-function) ;; hns-mode require APEL :-)
	    (add-hook 'find-file-not-found-hooks
		      '(lambda () (funcall hnf-initial-function))))
	(find-file-other-window
	 (mhc-calendar-hnf-get-filename (mhc-calendar-get-ddate)))
	(and (integerp count) (mhc-calendar-hnf-search-title count)))))

(defun mhc-calendar-hnf-view ()
  (interactive)
  (let ((fname (mhc-calendar-hnf-get-filename (mhc-calendar-get-ddate)))
	(count (mhc-calendar-in-summary-hnf-p)))
    (if (file-readable-p fname)
	(progn
	  (mhc-window-push)
	  (view-file-other-window fname)
	  (setq view-exit-action 'mhc-calendar-view-exit-action)
	  (and (integerp count) (mhc-calendar-hnf-search-title count))))))

(defun mhc-calendar-hnf-search-title (count)
  (goto-char (point-min))
  (while (and (> count 0) (not (eobp)))
    (re-search-forward "^\\(L?NEW\\|L?SUB\\)[ \t]+" nil t)
    (setq count (1- count)))
  (beginning-of-line)
  (recenter (/ (window-height) 4)))

(defun mhc-calendar-hnf-mark-diary-entries ()
  (interactive)
  (let ((cdate (ddate-mm-first-day (ddate-mm-dec mhc-calendar-ddate)))
	(edate (ddate-mm-last-day (ddate-mm-inc mhc-calendar-ddate)))
	(flst (mhc-calendar-hnf-file-list mhc-calendar-ddate))
	(mark "'") (spcchar (string-to-char " ")))
    (mhc-face-put mark 'mhc-calendar-hnf-face-mark)
    (while (ddate<= cdate edate)
      (if (member (format "d%s.hnf" (ddate-to-s cdate)) flst)
	  (progn
	    (goto-char (+ 2 (mhc-calendar-tp-any (point-min) (point-max)
						 'mhc-calendar-day-prop cdate)))
	    (insert mark)
	    (if (eq (char-after (point)) spcchar)
		(delete-char 1))))
      (setq cdate (ddate-inc cdate)))))

(defun mhc-calendar-hnf-summary-insert ()
  (let* ((ddate mhc-calendar-view-ddate)
	 (fname (mhc-calendar-hnf-get-filename ddate))
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
	    (put-text-property 0 (length str) 'mhc-calendar-summary-hnf-prop count str)
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
	    (put-text-property 0 (length str) 'mhc-calendar-summary-hnf-prop count str)
	    (setq summary (concat summary str)
		  count (1+ count)
		  ncount (1+ ncount)
		  cat ""))
	   ;; SUB
	   ((looking-at "^SUB[ \t]+\\(.*\\)$")
	    (setq str (buffer-substring (match-beginning 1) (match-end 1)))
	    (mhc-face-put str 'mhc-calendar-hnf-face-sub)
	    (setq str (concat "       " sub " " cat str "\n"))
	    (put-text-property 0 (length str) 'mhc-calendar-summary-hnf-prop count str)
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
	    (put-text-property 0 (length str) 'mhc-calendar-summary-hnf-prop count str)
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
(defun mhc-mua-ready-p ()
  (cond
   ((eq mhc-mailer-package 'mew)
    (or (and (boundp 'mew-init-p) mew-init-p)
	(and (boundp 'mew-mail-path) mew-mail-path)))
   ((eq mhc-mailer-package 'wl)
    (and (boundp 'wl-init) wl-init))
   ((eq mhc-mailer-package 'gnus)
    (and (fboundp 'gnus-alive-p) (gnus-alive-p)))
   (t nil)))

;;; Pseudo MUA Backend Methods:
(defun mhc-calendar-insert-summary-contents (schedule contents icon)
  (let ((pos (point)))
    (put-text-property 0 (length contents)
		       'mhc-calendar-summary-prop
		       (or (mhc-record-name
			    (mhc-schedule-record schedule))
			   "Dummy")
		       contents)
  (insert contents "\n")
  (if icon (mhc-put-icon icon (+ pos mhc-summary-icon-position)))))


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
