;;; -*- mode: Emacs-Lisp; coding: euc-japan -*-

;; Author:  Yoshinari Nomura <nom@quickhack.net>,
;; Created: 2000/05/10
;; Revised: $Date$


;;; Commentary:

;; This file is a part of MHC, includes MUA backend methods for Mew.


;;; TODO:

;; (1) Define mhc-mew-summary-display-article.


;;; Code:

(require 'mew)

;; Internal Variables:

(defconst mhc-mew/summary-filename-regex 
  ".*\r *\\+\\([^ \t]+\\)[ \t]+\\([^ \t\n]+\\)")

(defconst mhc-mew/header-string
  (let ((str "0 | ")) (put-text-property 0 (length str) 'invisible t str) str))

(defconst mhc-mew/header-string-review
  (let ((str (concat "0" (char-to-string mew-mark-review) "| ")))
    (put-text-property 0 (length str) 'invisible t str) str))

(defconst mhc-mew/summary-message-alist
  '((mew-summary-mode . mew-message-mode)
    (mew-virtual-mode . mew-message-mode)))


;; Setup function:

;;;###autoload
(defun mhc-mew-setup ()
  (require 'mhc)
  (mhc-setup)
  (setq mhc-mailer-package 'mew)
  (add-hook 'mew-summary-mode-hook 'mhc-mode)
  (add-hook 'mew-virtual-mode-hook 'mhc-mode)
  (add-hook 'mew-message-hook 'mhc-mew-decode-header)
  (add-hook 'mew-quit-hook 'mhc-exit))


;; Backend methods:

(defun mhc-mew-summary-filename ()
  (let (folder number)
    (save-excursion
      (beginning-of-line)
      (if (not (looking-at mhc-mew/summary-filename-regex))
	  ()
	(setq folder (buffer-substring (match-beginning 1) (match-end 1))
	      number (buffer-substring (match-beginning 2) (match-end 2)))
	(mhc-summary-folder-to-path folder number)))))


(defun mhc-mew-get-import-buffer (get-original)
  (if get-original (mew-summary-display-asis))
  (save-window-excursion
    (if (eq (cdr (assq major-mode mhc-mew/summary-message-alist))
	    (progn (other-window 1) major-mode))
	(current-buffer))))


(defun mhc-mew/date-to-buffer (date)
  (mhc-date-format date "%s/%02d/%02d" mhc-base-folder yy mm))


(defun mhc-mew-generate-summary-buffer (date)
  (switch-to-buffer
   (set-buffer
    (mhc-get-buffer-create (mhc-mew/date-to-buffer date))))
  (setq inhibit-read-only t
	buffer-read-only nil
	selective-display t
	selective-display-ellipses nil
	indent-tabs-mode nil)
  (widen)
  (delete-region (point-min) (point-max)))


(defun mhc-mew/schedule-foldermsg (schedule)
  (let ((path (mhc-record-name (mhc-schedule-record schedule))) fld-msg)
    (setq fld-msg
	  (if (and
	       path
	       (string-match (concat "^" (regexp-quote (file-name-as-directory mhc-mail-path)))
			     path))
	      (concat "+" (substring path (match-end 0)))
	    (concat mhc-base-folder "/1970/01/0")))
    (concat
     "\r "
     (directory-file-name (file-name-directory fld-msg))
     " "
     (file-name-nondirectory fld-msg))))


(defun mhc-mew-insert-summary-contents (schedule contents icon)
  (let (pos)
    (insert (if schedule mhc-mew/header-string-review mhc-mew/header-string))
    (setq pos (point))
    (insert contents
	    (mhc-mew/schedule-foldermsg schedule)
	    "\n")
    (if icon
	(mhc-put-icon icon (+ pos mhc-summary-icon-position)))))


(defun mhc-mew-summary-search-date (date)
  (re-search-forward
   (mhc-date-format date "^0[ %c]| %02d/%02d" mew-mark-review mm dd) nil t))


(defun mhc-mew-summary-mode-setup (date)
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


;; This function was orignally written by 
;; Shun-ichi Goto <gotoh@taiyo.co.jp> (cf. http://www.imasy.org/~gotoh/)
;; Arranged by Hideyuki SHIRAI <shirai@rdmg.mgcs.mei.co.jp>.
(defun mhc-mew-decode-header ()
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

;; For Backward Compatibility
(defalias 'mhc-misc-hdr-decode 'mhc-mew-decode-header)


(provide 'mhc-mew)
(put 'mhc-mew 'summary-filename 'mhc-mew-summary-filename)
(put 'mhc-mew 'get-import-buffer 'mhc-mew-get-import-buffer)
(put 'mhc-mew 'draft-mode 'mhc-mew-draft-mode)
(put 'mhc-mew 'generate-summary-buffer 'mhc-mew-generate-summary-buffer)
(put 'mhc-mew 'insert-summary-contents 'mhc-mew-insert-summary-contents)
(put 'mhc-mew 'summary-search-date 'mhc-mew-summary-search-date)
(put 'mhc-mew 'summary-mode-setup 'mhc-mew-summary-mode-setup)

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

;;; mhc-mew.el ends here.
