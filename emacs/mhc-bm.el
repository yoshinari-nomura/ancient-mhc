;;; mhc-bm.el -- Bitmap stuff for MHC.

;; Author:  Yuuichi Teranishi <teranisi@gohome.org>
;;
;; Created: 2000/05/27
;; Time-stamp: <2000-05-29 11:54:45 teranisi>

(require 'mhc-face)

(defvar mhc-bm-category-icon-alist nil
  "*Alist to rule the category-to-icon conversion.
Each element should have the form
 (CATEGORY-STRING . (ICON-FILE FG BG))
MHC puts an icon with FG and BG color created from ICON-FILE
on the schedule which have category of CATEGORY-STRING.
FG and BG can be omitted (default color is used).
Example:
  '((\"Holiday\"     . (\"Holiday.xbm\" \"OrangeRed\" \"White\"))
    (\"Work\"        . (\"Business.xbm\" \"Tan\"))
    (\"Private\"     . (\"Private.xbm\" \"HotPink\"))
    (\"Anniversary\" . (\"Anniversary.xbm\" \"SkyBlue\"))
    (\"Birthday\"    . (\"Birthday.xbm\"))
    (\"Other\"       . (\"Other.xbm\" \"Red\")))")

;; internal variable.
(defvar mhc-bm-category-bmstr-alist nil)

(defun mhc-bm-create-rectangle (file)
  (with-temp-buffer
    (insert-file-contents file)
    (let* ((cmp (bitmap-decode-xbm (bitmap-read-xbm-buffer (current-buffer))))
	   (len (length cmp))
	   (i 0)
	   bitmap)
      (while (< i len)
	(setq bitmap (cons (bitmap-compose (aref cmp i)) bitmap))
	(setq i (+ i 1)))
      (nreverse bitmap))))

(defun mhc-bm-setup-icons ()
  (let ((alist mhc-bm-category-icon-alist)
	bmstr)
    (while alist
      ;; Only the first element of the rectangle is used.
      (setq bmstr (car (mhc-bm-create-rectangle
			(expand-file-name (car (cdr (car alist)))
					  mhc-icon-path))))
      (put-text-property 0 (length bmstr)
			 'face 
			 (mhc-face-make-face-from-string
			  (concat "mhc-bm-category-icon-"
				  (downcase (car (car alist)))
				  "-face")
			  (list nil
				(nth 0 (cdr (cdr (car alist))))
				(nth 1 (cdr (cdr (car alist))))))
			 bmstr)
      (setq mhc-bm-category-bmstr-alist
	    (cons
	     (cons (car (car alist))
		   bmstr)
	     mhc-bm-category-bmstr-alist))
      (setq alist (cdr alist)))))
       
;; Icon interface
(defun mhc-icon-setup ()
  "Initialize MHC icons."
  (interactive)
  (if (interactive-p)
      (setq mhc-bm-category-bmstr-alist nil))
  (or mhc-bm-category-bmstr-alist
      (progn
	(message "Initializing MHC icons...")
	(mhc-bm-setup-icons)
	(message "Initializing MHC icons...done."))))

(defun mhc-use-icon-p ()
  "Returns t if MHC displays icon."
  (and window-system mhc-use-icon))

(defun mhc-get-icon (category)
  "Get icon glyph for GATEGORY."
  (cdr (assoc category mhc-bm-category-bmstr-alist)))

(defun mhc-put-icon (icon position)
  "Put ICON at POSITION of the current buffer."
  (save-excursion
    (goto-char position)
    (insert icon)))

(provide 'mhc-bm)

;;; mhc-bm.el ends here
