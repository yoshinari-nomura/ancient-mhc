;;; mhc-misc.el -- miscellaneous functions for mhc.

;; Author:  Yoshinari Nomura <nom@mew.org>
;;
;; Created: 1997/10/12
;; Revised: 2000/04/26 16:56:57

;;;
;;; Commentay:
;;;

;;;
;;; Code:
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; header

(defun mhc-misc-hdr-value (hdr)
  (let ((ret nil))
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "^-*$" nil t)
      (beginning-of-line)
      (narrow-to-region (point-min) (point))
      (goto-char (point-min))
      (if (re-search-forward
	   (format  
	    "^%s[\t ]*\\([^\t \n][^\n]*\\(\n[ \t]+[^\n]*\\)*\\)" hdr) nil t)
	  (setq ret 
		(mhc-misc-strip
		 (mhc-misc-gsub 
		  (buffer-substring (match-beginning 1) (match-end 1))
		  "\n[\t ]*" " "))))
      (widen))
    ret))

(defun mhc-misc-hdr-delete-list(hdr-list)
  (while hdr-list
    (mhc-misc-hdr-delete (car hdr-list))
    (setq hdr-list (cdr hdr-list))))


(defun mhc-misc-hdr-delete (hdr)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^-*$" nil t)
    (beginning-of-line)
    (narrow-to-region (point-min) (point))
    (goto-char (point-min))
    (delete-matching-lines 
     (format  "^%s[^\n]*\\(\n[ \t]+[^\n]*\\)*" hdr))
    (widen)))

(defun mhc-misc-hdr-delete-separator ()
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^-*$" nil t)
	(progn
          (beginning-of-line)
	  (if (not (looking-at "^--*$"))
	      ()
	    (kill-line 1)
	    (insert "\n"))))))

;; Add header and its value to a buffer, if already exists the header,
;; erase it before the addtion.
;;
(defun mhc-misc-hdr-replace (hdr value)
  (save-excursion
    (mhc-misc-hdr-delete hdr)
    (goto-char (point-min))
    (re-search-forward "^-*$" nil t)
    (beginning-of-line)
    (insert (concat hdr " " value "\n"))))

;; Add header and its value to a buffer, if already exists the header,
;; add the value to it.
;;
(defun mhc-misc-hdr-add (hdr value)
  (if (not (mhc-misc-hdr-value hdr))
      (mhc-misc-hdr-replace hdr value)
    ;(save-excursion
      (goto-char (point-min))
      (re-search-forward (format "^%s *" hdr) nil t)
      (insert (concat value " "))
      ;)
    )
  )


;; 
;; This function was orignally written by 
;; Mr. Shun-ichi Goto <gotoh@taiyo.co.jp> (cf. http://www.imasy.org/~gotoh/)
;; Arranged by  Mr. Hideyuki SHIRAI <shirai@rdmg.mgcs.mei.co.jp>
;;
(defun mhc-misc-hdr-decode ()
  "mew-message-hook function to decode RAW JIS subject in header"
  (condition-case e
      (if (mew-current-get 'cache)
	  (let* ((cache (mew-current-get 'cache))
		 (part (mew-current-get 'part))
		 (syntax (mew-cache-decode-syntax cache))
		 (ent (mew-syntax-get-entry syntax part))
		 (ct (mew-syntax-get-ct ent))
		 (buffer-read-only nil))
	    (if (not (equal "Message/Rfc822" (car ct)))
		()			; nothing to do
	      ;; do only Message/Rfc822 contents
	      (save-excursion
		(save-restriction
		  (widen)
		  (goto-char 1)
		  (if (not (re-search-forward "\r?\n\r?\n" nil t))
		      ()		; no header
		    (narrow-to-region (point-min) (point))
		    (goto-char (point-min))
		    (if (not (re-search-forward "^X-SC-Subject:" nil t))
			()
		      (goto-char (point-min))
		      ;; decode raw JIS string
		      (while (< (point) (point-max))
			(if (looking-at "[^:]+:? *")
			    (goto-char (match-end 0)))
			(if (and (not (looking-at "[\t\x20-\x7e]+$"))
				 (equal (mew-find-cs-region 
					 (point)
					 (save-excursion (end-of-line)
							 (point)))
					(list mew-lc-ascii)))
			    ;; decode!
			    (mew-cs-decode-region (point) 
						  (save-excursion
						    (end-of-line)
						    (point))
						  mew-cs-scan))
			(beginning-of-line)
			(forward-line 1))
		      ;; re-highlight
		      (mew-highlight-header)
		      (save-excursion
			(mew-highlight-x-face (point-min) (point-max))))))))))
    (error
     (ding t)
     (message "mhc-message-decode-header: %s" (or (cdr e) "some error!")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; string

(defun mhc-misc-join (lst &optional sep)
  (mapconcat (function identity) lst sep))


(defun mhc-misc-sub (str regex replace)
  (if (and (stringp str) (string-match regex str))
      (concat (substring str 0 (match-beginning 0))
	      replace
	      (substring str (match-end 0)))
    str))

(defun mhc-misc-gsub (str regex replace)
  (if (and (stringp str) (string-match regex str))
      (concat (substring str 0 (match-beginning 0))
	      replace
	      (mhc-misc-gsub (substring str (match-end 0)) regex replace))
    str))

(defun mhc-misc-split (str &optional sep)
  (let ((ret ()))
    (while (string-match (or sep "[\t ]+") str)
      (setq ret (cons (substring str 0 (match-beginning 0)) ret))
      (setq str (substring str (match-end 0))))
    (nreverse (cons str ret))))

(defun mhc-misc-strip (str)
  (mhc-misc-sub 
   (mhc-misc-sub str "^[\t ]+" "") "[\t ]+$" ""))

(defun mhc-misc-capitalize (str)
  (capitalize str))

(defun mhc-misc-uniq (lst)
  (let ((tmp lst))
    (while tmp (setq tmp (setcdr tmp (delete (car tmp) (cdr tmp))))))
  lst)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; file & path

(defun mhc-misc-copy-buffer-to-file (buffer file &optional append)
  (save-excursion
    (set-buffer buffer)
    (cond
     ((string< "20.3" emacs-version) ;; emacs 20.3.x or higher
      (let ((coding-system-for-write 'iso-2022-jp))
	(write-region (point-min) (point-max) file append
		      'silence nil nil)))
     ((string< "20" emacs-version) ;; emacs 20.x
      (let ((coding-system-for-write 'iso-2022-jp))
	(write-region (point-min) (point-max) file append
		      'silence nil )))
     ((string< "19.3" emacs-version) ;; emacs 19.34
      (write-region (point-min) (point-max) file append
		    'silence nil *iso-2022-jp*))
     (t ;; 19.28
      (write-region (point-min) (point-max) file append
		    'silence *iso-2022-jp*)))
    (set-buffer-modified-p nil)))


(defun mhc-misc-get-new-path (dir)
  (let (dirent	(max 0) (num nil))
    (mhc-misc-mkdir-or-higher dir)
    (setq dirent (directory-files dir))
    (while dirent
      (if (and (string-match "^[0-9]+$" (car dirent))
	       (< max (setq num (string-to-int (car dirent)))))
	  (setq max num))
      (setq dirent (cdr dirent)))
    (expand-file-name (int-to-string (1+ max)) dir)))

;;
;; touch directory and files.
;;

(defvar mhc-mtime-file ".mhc-mtime")

(defun mhc-misc-get-mtime (obj)
  (let ((mtime-file (expand-file-name mhc-mtime-file obj)))
    (cond
     ((not (stringp obj))
      nil)
     ((file-exists-p mtime-file)
      (nth 5 (file-attributes mtime-file)))
     ((file-exists-p obj)
      (nth 5 (file-attributes obj)))
     (t
      nil))))

(defun mhc-misc-touch-directory (dir)
  (let ((mtime-file (expand-file-name mhc-mtime-file dir)))
    (if (file-writable-p mtime-file)
	;; (write-region (point-min) (point-min) mtime-file nil 'silence))
	(write-region 1 2 mtime-file nil 'silence))
    ))

;; almost same as (make-directory dir t)
;;
(defun mhc-misc-mkdir-or-higher (dir)
  (let (parent)
    (if (file-directory-p dir)
	t ;; return value
      (setq parent (directory-file-name 
		    (file-name-directory 
		     (directory-file-name dir))))
      (if (not (mhc-misc-mkdir-or-higher parent))
	  nil
	(make-directory dir) t ;; return value
	))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rectangle

(get-char-property (point) 'invisible)

;;
;; Does (current-column) count visible character only?
;;   emacs  19.34, 20.4, 20.5 -- yes
;;   emacs  19.28             -- no
;;  xemacs                    -- no
;;
(defvar mhc-misc-column-count-visible-only
  (and (not (featurep 'xemacs)) (string< "19.3" emacs-version)))

(defun mhc-misc-move-to-column (column)
  "Move point to column COLUMN rigidly in the current line, considering 
   invisible charracters."
  (if mhc-misc-column-count-visible-only
      ()
    (beginning-of-line)
    (let* ((bol (point))
	   (vis (if (get-char-property bol 'invisible)
		    (next-single-property-change bol 'invisible)
		  bol)))
      (setq column (+ column (- vis bol)))))
  (if (< column (move-to-column column t))
      (progn (delete-char -1) (insert ?\ ))))

(defun mhc-misc-current-column ()
  "Return current column in a visible field."
  (if mhc-misc-column-count-visible-only
      (current-column)
    (let* ((bol (save-excursion (beginning-of-line) (point)))
	   (vis (if (get-char-property bol 'invisible)
		    (next-single-property-change bol 'invisible)
		  bol)))
      (- (current-column) (- vis bol)))))

(defun mhc-misc-insert-rectangle (rectangle)
  (let ((lines rectangle)
        (insertcolumn (mhc-misc-current-column))
        (first t))
    ;; (push-mark)
    (while lines
      (or first
          (progn
	    (forward-line 1)
	    (or (bolp) (insert ?\n))
	    (mhc-misc-move-to-column insertcolumn)))
      (setq first nil)
      (if (looking-at "[^\r\n]+")
	  (delete-region (point) (match-end 0)))
      (insert (car lines))
      (setq lines (cdr lines)))))

(provide 'mhc-misc)

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

;;; mhc-misc.el ends here
