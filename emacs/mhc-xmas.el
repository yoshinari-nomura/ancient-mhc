;;; mhc-xmas.el -- XEmacs stuff for MHC.

;; Author:  Yuuichi Teranishi <teranisi@gohome.org>
;;
;; Created: 1999/12/02
;; Time-stamp: <00/06/27 09:42:29 teranisi>

(defcustom mhc-xmas-category-icon-alist nil
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
    (\"Other\"       . \"Other.xpm\"))"
  :group 'mhc
  :type '(repeat 
	  :inline t
	  (cons (string :tag "Category Name")
		(string :tag "XPM File Name"))))

;; internal variable.
(defvar mhc-xmas/category-glyph-alist nil)

(defsubst mhc-xmas/setup-icons ()
  (let ((alist mhc-xmas-category-icon-alist))
    (setq mhc-xmas/category-glyph-alist nil)
    (while alist
      (setq mhc-xmas/category-glyph-alist
	    (cons
	     (cons (downcase (car (car alist)))
		   (make-glyph 
		    (make-image-instance
		     (vector 'xpm :file
			     (expand-file-name (cdr (car alist))
					       mhc-icon-path))
		     nil nil 'no-error)))
	     mhc-xmas/category-glyph-alist))
      (setq alist (cdr alist)))))

;; Icon interface
(defun mhc-icon-setup ()
  "Initialize MHC icons."
  (if (interactive-p)
      (setq mhc-xmas/category-glyph-alist nil))
  (or mhc-xmas/category-glyph-alist
      (progn
	(message "Initializing MHC icons...")
	(mhc-xmas/setup-icons)
	(message "Initializing MHC icons...done."))))

(defun mhc-use-icon-p ()
  "Returns t if MHC displays icon."
  (and (device-on-window-system-p)
       (featurep 'xpm)
       mhc-use-icon))

(defun mhc-put-icon (categories)
  "Put an icon on current buffer.
Icon is decided by CATEGORIES and `mhc-xmas-category-icon-alist'."
  (let (start space)
    (setq categories
	  (delq nil
		(mapcar (lambda (category)
			  (cdr (assoc category mhc-xmas/category-glyph-alist)))
			categories)))
    (when categories
      (setq space (make-string (length categories) ? ))
      (setq start (point))
      (insert space space) ; Icon is 2 character width.
      (put-text-property start (point) 'invisible t)
      (while categories
	(set-extent-begin-glyph (make-extent (point) (point)) (car categories))
	(setq categories (cdr categories))))))

(provide 'mhc-xmas)

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

;;; mhc-xmas.el ends here
