;;; mhc-mime.el -- MIME module backend for MHC.

;; Author:  Yuuichi Teranishi <teranisi@quickhack.net>
;;
;; Created: 2000/07/26
;; Revised: $Date: 2000/07/31 11:35:54 $

;;; Commentary:

;; This file is a part of MHC, includes MUA backend methods for
;; MIME module (SEMI).

;;; Code:

(require 'mime-edit)


(defsubst mhc-mime-get-raw-buffer ()
  "Get raw buffer of the current message for `mhc-mime-get-import-buffer'."
  (funcall (mhc-get-function 'mime-get-raw-buffer)))


(defvar mhc-mime-import-buffer " *MHC MIME import*")
(defun mhc-mime-get-import-buffer (get-original)
  (let ((raw-buffer (mhc-mime-get-raw-buffer))
	mime-view-ignored-field-list)
    (with-current-buffer (get-buffer-create mhc-mime-import-buffer)
      (mime-view-buffer raw-buffer (current-buffer))
      (let (buffer-read-only)
	(mhc-highlight-message))
      (if get-original
	  (cons raw-buffer (current-buffer))
	(current-buffer)))))


(defalias 'mhc-mime-eword-decode-string 'eword-decode-string)


(defun mhc-mime-draft-translate ()
  (let (mime-edit-insert-user-agent-field)
    (mime-edit-translate-buffer)
    (save-excursion
      (if (re-search-forward
	   (concat "^" (regexp-quote mail-header-separator) "$")
	   nil t)
	  (replace-match "")))))


(defun mhc-mime-draft-setup-new ()
  (goto-char (point-min))
  (insert mail-header-separator "\n"))


(defsubst mhc-mime/draft-reedit ()
  (save-excursion
    (let (ct cte start)
      (mhc-header-narrowing
	(mhc-header-delete-header
	 (concat "^\\("
		 (mhc-regexp-opt mhc-draft-unuse-hdr-list)
		 "\\)")
	 'regexp)
	(mime-decode-header-in-buffer 'decode)
	(setq ct (std11-fetch-field "content-type")
	      cte (std11-fetch-field "content-transfer-encoding"))
	(mhc-header-delete-header
	 mime-edit-again-ignored-field-regexp
	 'regexp))
      (goto-char (point-min))
      (when (re-search-forward "^$" nil t)
	(setq start (point))
	(and ct (insert "Content-type: " ct "\n"))
	(and cte (insert "Content-Transfer-Encoding: " cte "\n")))
      (save-restriction
	(narrow-to-region start (point-max))
	(mime-edit-decode-message-in-buffer)
	(widen)
	(goto-char start)
	(insert mail-header-separator "\n")))))


(defun mhc-mime-draft-reedit-buffer (buffer original)
  ;; If current buffer is specified as buffer, no need to replace.
  (unless (eq (current-buffer) buffer)
    (erase-buffer)
    (insert-buffer buffer))
  (if original
      ;; buffer is raw buffer.
      (mhc-mime/draft-reedit)
    (mhc-header-narrowing
      (mhc-header-delete-header mime-edit-again-ignored-field-regexp 'regexp))
    (goto-char (point-min))
    (when (re-search-forward "^$" nil t)
      (insert mail-header-separator))))


(defun mhc-mime-draft-reedit-file (file)
  (erase-buffer)
  (insert-file-contents-as-raw-text-CRLF file)
  (mhc-mime/draft-reedit))


(provide 'mhc-mime)

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

;;; mhc-mime.el ends here.