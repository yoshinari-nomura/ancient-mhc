;;; -*- mode: Emacs-Lisp; coding: euc-japan -*-

;; Author:  Yoshinari Nomura <nom@quickhack.net>,
;; Created: 2000/05/10
;; Revised: $Date$


;;; Commentary:

;; This file is a part of MHC, includes MUA backend methods for
;; Wanderlust.

;;; Code:

(require 'wl-summary)
(require 'elmo-localdir)
(require 'mhc-mime)


;; Setup function:

;;;###autoload
(defun mhc-wl-setup ()
  (require 'mhc)
  (mhc-setup)
  (setq mhc-mailer-package 'wl)
  (autoload 'mhc-mode "mhc" nil t)
  (add-hook 'wl-summary-mode-hook 'mhc-mode)
  (add-hook 'wl-folder-mode-hook 'mhc-mode)
  (add-hook 'wl-exit-hook 'mhc-exit))


;; Backend methods:

(defun mhc-wl-summary-filename ()
  "Return FILENAME on current line."
  (let* ((fld-num (elmo-multi-get-real-folder-number
		   wl-summary-buffer-folder-name
		   (wl-summary-message-number)))
	 (fld (car fld-num))
	 (num (cdr fld-num)))
    (expand-file-name
     (number-to-string num)
     (elmo-localdir-get-folder-directory
      (elmo-folder-get-spec fld)))))


(defun mhc-wl-summary-display-article ()
  "Display the article on the current."
  (wl-summary-redisplay))


(defun mhc-wl-mime-get-raw-buffer ()
  (wl-summary-set-message-buffer-or-redisplay)
  (wl-message-get-original-buffer))


(defun mhc-wl-mime-get-mime-structure ()
  (wl-summary-set-message-buffer-or-redisplay)
  (get-text-property (point) 'mime-view-entity))


(defun mhc-wl-highlight-message (for-draft)
  (let ((wl-highlight-x-face-func (unless for-draft wl-highlight-x-face-func)))
    (wl-highlight-message (point-min) (point-max) t)))


;; mhc-tmp-schedule is already bound.
(defun mhc-wl-insert-summary-contents (inserter)
  (let (head path pos)
    (setq path (mhc-record-name (mhc-schedule-record mhc-tmp-schedule))
	  head
	  (cond
	   ((or (not path) (equal path mhc-schedule-file))
	    "100000")
	   ((string-match "/intersect/" path)
	    (format "1%05d"
		    (string-to-number (file-name-nondirectory path))))
	   (t 
	    (format "2%05d"
		    (string-to-number (file-name-nondirectory path)))))
	  head (concat head (if path "*| " " | ")))
    (put-text-property 0 (length head) 'invisible t head)
    (insert head)
    (funcall inserter)
    (insert "\n")))


(defsubst mhc-wl/date-to-folder (date)
  (mhc-date-format date
		   "*%s/intersect,%s/%04d/%02d"
		   mhc-base-folder
		   mhc-base-folder
		   yy
		   mm))


(defvar mhc-wl-exit-buffer nil)
(make-variable-buffer-local 'mhc-wl-exit-buffer)


(defun mhc-wl-summary-exit ()
  (let ((buffer mhc-wl-exit-buffer))
    (kill-buffer (current-buffer))
    (when (and buffer
	       (buffer-live-p buffer))
      (if (get-buffer-window buffer)
	  (unless (eq (current-buffer) buffer)
	    (delete-window)))
      (switch-to-buffer buffer))))


(defun mhc-wl-summary-mode-setup (date)
  (let ((original mhc-wl-exit-buffer))
    (wl-summary-mode) ; buffer local variables are killed.
    (setq mhc-wl-exit-buffer original)
    (wl-summary-buffer-set-folder (mhc-wl/date-to-folder date))
    (make-local-variable 'wl-summary-highlight)
    (setq wl-summary-highlight nil)
    (setq wl-summary-buffer-next-folder-func
	  (lambda () (mhc-goto-next-month 1)))
    (setq wl-summary-buffer-prev-folder-func
	  (lambda () (mhc-goto-prev-month 1)
	    (goto-char (point-max))))
    (setq wl-summary-buffer-exit-func 'mhc-wl-summary-exit)
    (setq wl-summary-buffer-target-mark-list '(nil))
    (setq wl-summary-buffer-number-regexp "[0-9]+")
    (setq wl-summary-buffer-msgdb '(nil))))


(defun mhc-wl-generate-summary-buffer (date)
  (wl-summary-toggle-disp-msg 'off)
  (let ((original (and (or (eq major-mode 'wl-summary-mode)
			   (eq major-mode 'wl-folder-mode))
		       (or mhc-wl-exit-buffer (current-buffer)))))
    (switch-to-buffer
     (set-buffer
      (mhc-get-buffer-create
       (mhc-date-format date "%s/%02d/%02d" mhc-base-folder yy mm))))
    (and original
	 (setq mhc-wl-exit-buffer original))
    (setq inhibit-read-only t
	  buffer-read-only nil
	  selective-display t
	  selective-display-ellipses nil
	  indent-tabs-mode nil)
    (widen)
    (delete-region (point-min) (point-max))))


(provide 'mhc-wl)
(put 'mhc-wl 'summary-filename 'mhc-wl-summary-filename)
(put 'mhc-wl 'summary-display-article 'mhc-wl-summary-display-article)
(put 'mhc-wl 'generate-summary-buffer 'mhc-wl-generate-summary-buffer)
(put 'mhc-wl 'insert-summary-contents 'mhc-wl-insert-summary-contents)
(put 'mhc-wl 'summary-mode-setup 'mhc-wl-summary-mode-setup)
(put 'mhc-wl 'get-import-buffer 'mhc-mime-get-import-buffer)
(put 'mhc-wl 'mime-get-raw-buffer 'mhc-wl-mime-get-raw-buffer)
(put 'mhc-wl 'mime-get-mime-structure 'mhc-wl-mime-get-mime-structure)
(put 'mhc-wl 'highlight-message 'mhc-wl-highlight-message)
(put 'mhc-wl 'draft-setup-new 'mhc-mime-draft-setup-new)
(put 'mhc-wl 'draft-reedit-buffer 'mhc-mime-draft-reedit-buffer)
(put 'mhc-wl 'draft-reedit-file 'mhc-mime-draft-reedit-file)
(put 'mhc-wl 'draft-translate 'mhc-mime-draft-translate)
(put 'mhc-wl 'eword-decode-string 'mhc-mime-eword-decode-string)
(put 'mhc-wl 'decode-header 'mhc-mime-decode-header)

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

;;; mhc-wl.el ends here.
