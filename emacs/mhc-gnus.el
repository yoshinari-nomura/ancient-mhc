;;; -*- mode: Emacs-Lisp; coding: euc-japan -*-

;; Author:  Yoshinari Nomura <nom@quickhack.net>,
;;          MIYOSHI Masanori <miyoshi@hrl.hitachi.co.jp>,
;;          TSUCHIYA Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>
;; Created: 2000/05/10
;; Revised: $Date$


;;; Commentary:

;; This file is a part of MHC, and includes MUA backend methods for
;; Gnus.


;;; Code:

(eval-when-compile (require 'cl))
(require 'gnus-sum)
(require 'nnmhc)


;; Internal Variables:

(defvar mhc-gnus/mhc-is-running nil)


(if (string-match "SEMI" gnus-version)
    (progn
      (require 'eword-encode)
      (defalias 'mhc-gnus-encode-string 'eword-encode-string))
  (defun mhc-gnus-encode-string (string)
    "Alternative function of `eword-encode-string' for pure Gnus."
    (with-temp-buffer
      (insert string)
      (rfc2047-encode-region (point-min) (point-max))
      (buffer-substring (point-min) (point-max)))))


;; Setup function:

(defun mhc-gnus-setup ()
  (require 'mhc)
  (mhc-setup)
  (setq mhc-mailer-package 'gnus)
  (add-hook 'gnus-group-mode-hook 'mhc-mode)
  (add-hook 'gnus-summary-mode-hook 'mhc-mode)
  (add-hook 'gnus-exit-gnus-hook 'mhc-exit))


;; Backend methods:

(defun mhc-gnus-summary-filename ()
  "Return the file name of the article on the current line."
  (let ((num (get-text-property (point) 'gnus-number)))
    (if num (nnmhc-get-article num))))


(defun mhc-gnus-summary-display-article ()
  (let ((num (get-text-property (point) 'gnus-number)))
    (if num (gnus-summary-display-article num))))
  
  
(defun mhc-gnus-get-import-buffer (get-original)
  (gnus-summary-select-article)
  (gnus-copy-article-buffer))


(defsubst mhc-gnus/ddate-to-group-name (ddate)
  (format "%s/%02d/%02d"
	  mhc-base-folder (ddate-yy ddate) (ddate-mm ddate)))


(defun mhc-gnus-generate-summary-buffer (ddate)
  (let* ((group (mhc-gnus/ddate-to-group-name ddate))
	 (method `(nnmhc ,group))
	 (vgroup (gnus-group-prefixed-name group method)))
    ;; initialize ephemeral nnmhc group.
    (gnus-group-read-ephemeral-group vgroup method t
				     (if (buffer-live-p gnus-summary-buffer)
					 (cons gnus-summary-buffer 'summary)
				       (cons (current-buffer) 'group))
				     t)
    (gnus-group-read-group 0 t vgroup)
    (gnus-summary-make-local-variables)
    (setq inhibit-read-only t)
    (delete-region (point-min) (point-max))))


(defun mhc-gnus-insert-summary-contents (schedule contents icon)
  (let ((x (mhc-record-name (mhc-schedule-record schedule)))
	(pos (point)))
    (when x
      (push x nnmhc-article-list)
      (setq x (length nnmhc-article-list)))
    (if x
	(let ((header (make-full-mail-header x (mhc-gnus-encode-string
						(mhc-schedule-subject-as-string schedule)))))
	  (put-text-property 0 (length contents) 'gnus-number x contents)
	  (push (gnus-data-make x 0 0 header 0) gnus-newsgroup-data))
      (remove-text-properties 0 (length contents) '(gnus-number nil) contents))
    (insert contents "\n")
    (if icon
	(mhc-put-icon icon (+ pos mhc-summary-icon-position)))))


(defun mhc-gnus-summary-mode-setup (ddate)
  (setq gnus-newsgroup-data (nreverse gnus-newsgroup-data)
	nnmhc-article-list (nreverse nnmhc-article-list))
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let ((num (get-text-property (point) 'gnus-number)))
	(if num (gnus-data-set-pos (assoc num gnus-newsgroup-data) (point))))
      (forward-line 1)))
  (let ((gnus-newsgroup-data))
    (gnus-summary-mode (gnus-group-prefixed-name
			(mhc-gnus/ddate-to-group-name ddate)
			'(nnmhc))))
  (when (fboundp 'gnus-summary-setup-default-charset)
    (gnus-summary-setup-default-charset)) ; for Nana7
  (set (make-local-variable 'mhc-gnus/mhc-is-running) t)
  (set (make-local-variable 'gnus-visual) nil)
  (setq gnus-newsgroup-begin 1
	gnus-newsgroup-end (length nnmhc-article-list)))


;; modify Gnus original functions for cursor control.
(eval-after-load "gnus"
  '(defadvice gnus-summary-position-point
     (around mhc-gnus-summary-position-point activate compile)
     (or mhc-gnus/mhc-is-running ad-do-it)))

(eval-after-load "gnus-sum"
  '(defadvice gnus-summary-update-mark
     (around mhc-gnus-summary-update-mark activate compile)
     (or mhc-gnus/mhc-is-running ad-do-it)))

;; modify Gnus original commands for manipulate articles.
(eval-after-load "gnus-sum"
  '(defadvice gnus-summary-edit-article
     (around mhc-gnus-draft-edit-message activate compile)
     "If MHC is running, exec mhc-modify."
     (if mhc-gnus/mhc-is-running
	 (mhc-modify)
       ad-do-it)))

(eval-after-load "gnus-sum"
  '(defadvice gnus-summary-delete-article
     (around mhc-gnus-summary-delete-article activate compile)
     "If MHC is running, exec mhc-delete."
     (if mhc-gnus/mhc-is-running
	 (mhc-delete)
       ad-do-it)))



(provide 'mhc-gnus)
(put 'mhc-gnus 'summary-filename 'mhc-gnus-summary-filename)
(put 'mhc-gnus 'summary-display-article 'mhc-gnus-summary-display-article)
(put 'mhc-gnus 'get-import-buffer 'mhc-gnus-get-import-buffer)
(put 'mhc-gnus 'generate-summary-buffer 'mhc-gnus-generate-summary-buffer)
(put 'mhc-gnus 'insert-summary-contents 'mhc-gnus-insert-summary-contents)
(put 'mhc-gnus 'summary-mode-setup 'mhc-gnus-summary-mode-setup)

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

;;; mhc-gnus.el ends here.
