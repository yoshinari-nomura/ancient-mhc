;;; mhc-xmas.el -- XEmacs stuff for MHC.

;; Author:  Yuuichi Teranishi <teranisi@gohome.org>
;;
;; Created: 1999/12/02
;; Revised: $Date: 2000/07/27 04:16:10 $

(defcustom mhc-xmas-icon-alist nil
  "*Alist to define icons.
Each element should have the form
 (NAME . ICON-FILE)
It defines icon named NAME created from ICON-FILE.
Example:
  '((\"Holiday\"     . \"Holiday.xpm\")
    (\"Work\"        . \"Business.xpm\")
    (\"Private\"     . \"Private.xpm\")
    (\"Anniversary\" . \"Anniversary.xpm\")
    (\"Birthday\"    . \"Birthday.xpm\")
    (\"Other\"       . \"Other.xpm\")
    (\"Conflict\"    . \"Conflict.xpm\"))"
  :group 'mhc
  :type '(repeat 
	  :inline t
	  (cons (string :tag "Icon Name")
		(string :tag "XPM File Name"))))

;; internal variable.
(defvar mhc-xmas/glyph-alist nil)

(defsubst mhc-xmas/setup-icons ()
  (let ((alist mhc-xmas-icon-alist))
    (setq mhc-xmas/glyph-alist nil)
    (while alist
      (setq mhc-xmas/glyph-alist
	    (cons
	     (cons (downcase (car (car alist)))
		   (make-glyph 
		    (make-image-instance
		     (vector 'xpm :file
			     (expand-file-name (cdr (car alist))
					       mhc-icon-path))
		     nil nil 'no-error)))
	     mhc-xmas/glyph-alist))
      (setq alist (cdr alist)))))

;; Icon interface
(defun mhc-icon-setup ()
  "Initialize MHC icons."
  (interactive)
  (if (interactive-p)
      (setq mhc-xmas/glyph-alist nil))
  (or mhc-xmas/glyph-alist
      (progn
	(message "Initializing MHC icons...")
	(mhc-xmas/setup-icons)
	(message "Initializing MHC icons...done."))))

(defun mhc-use-icon-p ()
  "Returns t if MHC displays icon."
  (and (device-on-window-system-p)
       (featurep 'xpm)
       mhc-use-icon))

(defun mhc-icon-exists-p (name)
  "Returns non-nil if icon with NAME exists."
  (cdr (assoc (downcase name) mhc-xmas/glyph-alist)))

(defun mhc-put-icon (icons)
  "Put ICONS on current buffer.
Icon is decided by `mhc-xmas-icon-alist'."
  (let (start space extent glyphs)
    (setq icons
	  (delq nil
		(mapcar (lambda (icon)
			  (cdr (assoc (downcase icon) mhc-xmas/glyph-alist)))
			icons)))
    (when icons
      (setq space (make-string (length icons) ? ))
      (setq start (point))
      (while (setq extent (extent-at (point) nil 'mhc-icon extent 'at))
	(setq glyphs (cons (extent-end-glyph extent) glyphs)))
      (insert space space)
      (setq extent (make-extent start (point)))
      (set-extent-property extent 'invisible t))
    (setq icons (nreverse icons))
    (while icons
      (setq extent (make-extent (point) (point)))
      (set-extent-properties extent '(mhc-icon t))
      (set-extent-end-glyph extent (car icons))
      (setq icons (cdr icons)))
    (while glyphs
      (setq extent (make-extent (point)(point)))
      (set-extent-properties extent '(mhc-icon t))
      (set-extent-end-glyph extent (car glyphs))
      (setq glyphs (cdr glyphs)))))

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
