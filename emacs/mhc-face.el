;;; mhc-face.el

;; Author:  Yoshinari Nomura <nom@quickhack.net>
;;
;; Created: 2000/02/08
;; Revised: $Date: 2000/06/27 13:07:02 $

;;;
;;; Commentay:
;;;

;;;
;;; Code:
;;;

(defvar mhc-symbol-face-alist nil
  "*Alist which is used in setup time to define required faces.
Each element should have the form
  (FACE-SYMBOL . (PARENT FG BG UNDERLINED FONT STIPPLE))
If this variable does't have necessary face definitions for mhc,
mhc will lookup them from mhc-symbol-face-alist-internal instead.
So, this variable doesn't have to cover all the face definitions.")

(defvar mhc-category-face-alist nil
  "*Alist to rule the catgegory-to-face conversion.
Each element should have the form
  (CATEGORY-STRING . (PARENT FG BG UNDERLINED FONT STIPPLE))
mhc will define mhc-summary-category-face-(downcase CATEGORY-STRING)
in setup time.")

(defvar mhc-calendar-hnf-face-alist nil
  "*Alist of HNS faces. Each element should have the form
  (FACE-SYMBOL . (PARENT FG BG UNDERLINED FONT STIPPLE)).
refer to mhc-calendar-hnf-face-alist-internal.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; for necessary faces.

(defconst mhc-symbol-face-alist-internal
  '((mhc-calendar-face-saturday . (nil "blue"        nil))
    (mhc-calendar-face-sunday   . (nil "red"         nil))
    ;;
    (mhc-summary-face-saturday  . (nil "blue"        nil))
    (mhc-summary-face-sunday    . (nil "red"         nil))
    (mhc-summary-face-today     . (nil "black"       "chocolate"))
    ;;
    (mhc-summary-face-separator . (nil "gray"        nil))
    (mhc-summary-face-time      . (nil "yellowgreen" nil))
    (mhc-summary-face-location  . (nil "black"       "paleturquoise"))
    (mhc-summary-face-conflict  . (nil "white"       "purple"))
    (mhc-summary-face-secret    . (nil "gray"        nil))
    ;;
    (mhc-minibuf-face-candidate . (nil nil           "yellow"))
    ;;
    (mhc-category-face-holiday  . (nil "red"         nil))))

(defconst mhc-calendar-hnf-face-alist-internal
  '((mhc-calendar-hnf-face-mark . (nil    "MediumSeaGreen" nil))
    (mhc-calendar-hnf-face-newtag  . (italic "red" "paleturquoise"))
    (mhc-calendar-hnf-face-subtag  . (italic "blue" nil))
    (mhc-calendar-hnf-face-cat  . (nil    "DarkGreen" nil))
    (mhc-calendar-hnf-face-new  . (bold   "DarkGreen" nil))
    (mhc-calendar-hnf-face-sub  . (nil   "DarkGreen" nil))
    (mhc-calendar-hnf-face-uri  . (italic "blue" nil))))

(defmacro mhc-face-put (symbol face)
  (` (put-text-property 0 (length (, symbol)) 'face (, face) (, symbol))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make faces from string/symbol

(defun mhc-face-category-to-face (category)
  (if category
      (or (intern-soft (format "mhc-category-face-%s" (downcase category)))
	  'default)
    'default))

(defun mhc-face-make-face-from-string (string prop &optional overwrite prefix)
  (let ((symbol-name (concat prefix (if prefix "-") string)))
    (mhc-face-make-face-from-symbol (intern symbol-name) prop overwrite)))

(defun mhc-face-make-face-from-symbol (symbol prop &optional overwrite)
  (let ((parent  (nth 0 prop))
	(fg      (nth 1 prop))
	(bg      (nth 2 prop))
	(uline   (nth 3 prop))
	(font    (nth 4 prop))
	(stipple (nth 5 prop))
	(face    nil))
    (if (and (mhc-facep symbol) (not overwrite))
	symbol
      (setq face (if parent (copy-face parent symbol) (make-face symbol)))
      (if fg      (set-face-foreground  face fg))
      (if bg      (set-face-background  face bg))
      (set-face-underline-p face uline)
      (if font    (set-face-font        face font))
      (if stipple (set-face-stipple     face stipple))
      face)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make faces arrange.

(defun mhc-face-get-gray-face (face)
  (let ((gray-symbol (intern (concat (symbol-name face) "-gray"))))
    (if (mhc-facep gray-symbol)
	()
      (copy-face face gray-symbol)
      (set-face-background gray-symbol "gray"))
    gray-symbol))

(defun mhc-face-get-busy-face (face)
  (let ((busy-symbol (intern (concat (symbol-name face) "-busy"))))
    (if (mhc-facep busy-symbol)
	()
      (copy-face face busy-symbol)
      (or (make-face-bold busy-symbol nil t)
	  (and (fboundp 'set-face-bold-p)
	       (set-face-bold-p busy-symbol t))))
    busy-symbol))

(defun mhc-face-get-underline-face (face)
  (let ((busy-symbol (intern (concat (symbol-name face) "-uline"))))
    (if (mhc-facep busy-symbol)
	()
      (copy-face face busy-symbol)
      (set-face-underline-p busy-symbol t))
    busy-symbol))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setup faces.

(defun mhc-face-setup ()
  (interactive)
  (let ((ow (interactive-p)))
    ;;
    (mhc-face-setup-internal mhc-symbol-face-alist          ow)
    (mhc-face-setup-internal mhc-category-face-alist        ow)
    ;;
    (mhc-face-setup-internal mhc-symbol-face-alist-internal nil)
    ))

(defun mhc-face-setup-internal (alist &optional overwrite)
  (let (lst)
    (while (setq lst (car alist))
      (cond 
       ((stringp (car lst))
	(mhc-face-make-face-from-string 
	 (format "mhc-category-face-%s" (downcase (car lst)))
	 (cdr lst)
	 overwrite))
       ((symbolp (car lst))
	(mhc-face-make-face-from-symbol
	 (car lst)
	 (cdr lst)
	 overwrite)))
      (setq alist (cdr alist)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; facep for emacs 19.28

(cond
 ((fboundp 'find-face)
  (defalias 'mhc-facep 'find-face))
 ((fboundp 'facep)
  (defalias 'mhc-facep 'facep))
 (t
  ;; Introduced in Emacs 19.29.
  (defun mhc-facep (x)
    "Return t if X is a face name or an internal face vector."
    (and (or (and (fboundp 'internal-facep) (internal-facep x))
	     (and 
	      (symbolp x) 
	      (assq x (and (boundp 'global-face-data) global-face-data))))
	 t))))
 
(provide 'mhc-face)

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

;;; mhc-face.el ends here
