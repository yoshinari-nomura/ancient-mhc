;;; mhc-bm.el -- Bitmap stuff for MHC.

;; Author:  Yuuichi Teranishi <teranisi@gohome.org>
;;
;; Created: 2000/05/27
;; Revised: $Date: 2000/07/03 00:50:17 $

(require 'bitmap)
(require 'mhc-face)

(defcustom mhc-bm-icon-alist nil
  "*Alist to define icons.
Each element should have the form
 (NAME . (ICON-FILE FG BG))
It defines icon named NAME with FG and BG color created from ICON-FILE.
FG and BG can be omitted (default color is used).
Example:
  '((\"Holiday\"     . (\"Holiday.xbm\" \"OrangeRed\" \"White\"))
    (\"Work\"        . (\"Business.xbm\" \"Tan\"))
    (\"Private\"     . (\"Private.xbm\" \"HotPink\"))
    (\"Anniversary\" . (\"Anniversary.xbm\" \"SkyBlue\"))
    (\"Birthday\"    . (\"Birthday.xbm\"))
    (\"Other\"       . (\"Other.xbm\" \"Red\")))"
  :group 'mhc
  :type '(repeat
	  :inline t
	  (cons (string :tag "Icon Name")
		(list (string :tag "XBM File Name")
		      (choice (string :tag "Set FG Color")
			      (const :tag "Default FG Color" nil))
		      (choice (string :tag "Set BG Color")
			      (const :tag "Default BG Color" nil))))))

;; internal variable.
(defvar mhc-bm/bmstr-alist nil)

(defun mhc-bm/create-rectangle (file)
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

(defsubst mhc-bm/setup-icons ()
  (let ((alist mhc-bm-icon-alist)
	bmstr)
    (while alist
      ;; Only the first element of the rectangle is used.
      (setq bmstr (car (mhc-bm/create-rectangle
			(expand-file-name (car (cdr (car alist)))
					  mhc-icon-path))))
      (put-text-property 0 (length bmstr)
			 'face 
			 (mhc-face-make-face-from-string
			  (concat "mhc-bm-icon-"
				  (downcase (car (car alist)))
				  "-face")
			  (list nil
				(nth 0 (cdr (cdr (car alist))))
				(nth 1 (cdr (cdr (car alist))))))
			 bmstr)
      (setq mhc-bm/bmstr-alist
	    (cons
	     (cons (downcase (car (car alist)))
		   bmstr)
	     mhc-bm/bmstr-alist))
      (setq alist (cdr alist)))))
       
;; Icon interface
(defun mhc-icon-setup ()
  "Initialize MHC icons."
  (interactive)
  (if (interactive-p)
      (setq mhc-bm/bmstr-alist nil))
  (or mhc-bm/bmstr-alist
      (progn
	(message "Initializing MHC icons...")
	(mhc-bm/setup-icons)
	(message "Initializing MHC icons...done."))))

(defun mhc-use-icon-p ()
  "Returns t if MHC displays icon."
  (and window-system mhc-use-icon))

(defun mhc-put-icon (icons)
  "Put ICONS on current buffer.
Icon is defined by `mhc-bm-icon-alist'."  
  (let (icon)
    (while icons
      (setq icon (cdr (assoc (downcase (car icons))
			     mhc-bm/bmstr-alist)))
      (and icon (insert icon))
      (setq icons (cdr icons)))))

(provide 'mhc-bm)

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

;;; mhc-bm.el ends here
