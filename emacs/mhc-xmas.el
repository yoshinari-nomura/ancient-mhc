;;; mhc-xmas.el -- XEmacs stuff for MHC.

;; Author:  Yuuichi Teranishi <teranisi@gohome.org>
;;
;; Created: 1999/12/02
;; Time-stamp: <00/05/29 15:01:13 teranisi>

(defvar mhc-xmas-category-icon-alist nil
  "*Alist to rule the category-to-icon conversion.
Each element should have the form
 (CATEGORY-STRING . ICON)
mhc puts ICON on the schedule which have category of CATEGORY-STRING.
Example:
  '((\"Holiday\"     . \"Holiday.xpm\")
    (\"Work\"        . \"Business.xpm\")
    (\"Private\"     . \"Private.xpm\")
    (\"Anniversary\" . \"Anniversary.xpm\")
    (\"Birthday\"    . \"Birthday.xpm\")
    (\"Other\"       . \"Other.xpm\"))")

;; internal variable.
(defvar mhc-xmas-category-glyph-alist nil)

(defun mhc-xmas-setup-icons ()
  (let ((alist mhc-xmas-category-icon-alist))
    (setq mhc-xmas-category-glyph-alist nil)
    (while alist
      (setq mhc-xmas-category-glyph-alist
	    (cons
	     (cons (car (car alist))
		   (make-glyph 
		    (make-image-instance
		     (vector 'xpm :file
			     (expand-file-name (cdr (car alist))
					       mhc-icon-path))
		     nil nil 'no-error)))
	     mhc-xmas-category-glyph-alist))
      (setq alist (cdr alist)))))

;; Icon interface
(defun mhc-icon-setup ()
  "Initialize MHC icons."
  (if (interactive-p)
      (setq mhc-bm-category-glyph-alist nil))
  (or mhc-xmas-category-glyph-alist
      (progn
	(message "Initializing MHC icons...")
	(mhc-xmas-setup-icons)
	(message "Initializing MHC icons...done."))))

(defun mhc-use-icon-p ()
  "Returns t if MHC displays icon."
  (and (device-on-window-system-p)
       (featurep 'xpm)
       mhc-use-icon))

(defun mhc-get-icon (category)
  "Get icon glyph for GATEGORY."
  (cdr (assoc category mhc-xmas-category-glyph-alist)))

(defun mhc-put-icon (icon position)
  "Put ICON at POSITION of the current buffer."
  (set-extent-end-glyph (make-extent position position) icon))

(provide 'mhc-xmas)

;;; mhc-xmas.el ends here
