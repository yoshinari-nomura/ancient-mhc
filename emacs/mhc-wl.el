;;; -*- mode: Emacs-Lisp; coding: euc-japan -*-

;; Author:  Yoshinari Nomura <nom@quickhack.net>,
;; Created: 2000/05/10
;; Revised: $Date$


;;; Commentary:

;; This file is a part of MHC, includes MUA backend methods for
;; Wanderlust.


;;; TODO:

;; (1) Define mhc-wl-summary-display-article.


;;; Code:

(require 'wl-summary)
(require 'elmo-localdir)


;; Setup function:

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


(defun mhc-wl-get-import-buffer (get-original)
  (save-excursion
    (if get-original
	(wl-summary-redisplay-no-mime)
      (let (wl-highlight-x-face-func)
	(wl-summary-redisplay-all-header)))
    (wl-summary-jump-to-current-message)
    (current-buffer)))


(defsubst mhc-wl/schedule-foldermsg (schedule)
  (let ((path (mhc-record-name (mhc-schedule-record schedule))))
    (cond
     ((or (not path) (equal path mhc-schedule-file))
      "100000")
     ((string-match "/intersect/" path)
      (format "1%05d" (string-to-number (file-name-nondirectory path))))
     (t 
      (format "2%05d" (string-to-number (file-name-nondirectory path)))))))


(defun mhc-wl-insert-summary-contents (schedule contents icon)
  (let ((head (concat (mhc-wl/schedule-foldermsg schedule) " | "))
	pos)
    (put-text-property 0 (length head) 'invisible t head)
    (insert head)
    (setq pos (point))
    (insert contents "\n")
    (if icon
	(mhc-put-icon icon (+ pos mhc-summary-icon-position)))))


(defsubst mhc-wl/ddate-to-folder (ddate)
  (concat "*" 
	  mhc-base-folder "/intersect"
	  ","
	  mhc-base-folder "/" (ddate-yymm-s1 ddate "/")))


(defun mhc-wl-summary-mode-setup (ddate)
  (wl-summary-mode)
  (wl-summary-buffer-set-folder (mhc-wl/ddate-to-folder ddate))
  (make-local-variable 'wl-summary-highlight)
  (setq wl-summary-highlight nil)
  (setq wl-summary-buffer-number-regexp "[0-9]+")
  (setq wl-summary-buffer-msgdb '(nil)))


(defun mhc-wl-generate-summary-buffer (ddate)
  (switch-to-buffer
   (set-buffer
    (mhc-get-buffer-create
     (concat mhc-base-folder "/" (ddate-yymm-s1 ddate "/")))))
  (setq inhibit-read-only t
	buffer-read-only nil
	selective-display t
	selective-display-ellipses nil
	indent-tabs-mode nil)
  (widen)
  (delete-region (point-min) (point-max)))

(provide 'mhc-wl)
(put 'mhc-wl 'summary-filename 'mhc-wl-summary-filename)
(put 'mhc-wl 'get-import-buffer 'mhc-wl-get-import-buffer)
(put 'mhc-wl 'generate-summary-buffer 'mhc-wl-generate-summary-buffer)
(put 'mhc-wl 'insert-summary-contents 'mhc-wl-insert-summary-contents)
(put 'mhc-wl 'summary-mode-setup 'mhc-wl-summary-mode-setup)

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