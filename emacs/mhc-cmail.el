;;; -*- mode: Emacs-Lisp; coding: euc-japan -*-

;; Author:  Yoshinari Nomura <nom@quickhack.net>
;; Created: 2000/07/18
;; Revised: $Date: 2000/07/18 04:29:13 $

;; (autoload 'mhc-cmail-setup "mhc-cmail")
;; (add-hook 'cmail-startup-hook 'mhc-cmail-setup)

;;; Commentary:

;; This file is a part of MHC, includes MUA backend methods for cmail.

;;; Code:

(require 'cmail)

;; Internal Variables:

(defconst mhc-cmail/summary-filename-regex   ".*\r *\\([^ \t\r\n]+\\)")

(defconst mhc-cmail/header-string
  (let ((str "0 | ")) (put-text-property 0 (length str) 'invisible t str) str))

;; (defconst mhc-cmail/header-string-review
;;   (let ((str (concat "0" (char-to-string cmail-mark-review) "| ")))
;;     (put-text-property 0 (length str) 'invisible t str) str))

(defconst mhc-cmail/summary-message-alist
  '((cmail-summary-mode . cmail-readmail-mode)))

;; Setup function:

;;;###autoload
(defun mhc-cmail-setup ()
  (interactive)
  (require 'mhc)
  (mhc-setup)
  (setq mhc-mailer-package 'cmail)
  (add-hook 'cmail-summary-mode-hook 'mhc-mode))

;; Backend methods:

(defun mhc-cmail-summary-filename ()
  (save-excursion
    (beginning-of-line)
    (if (looking-at mhc-cmail/summary-filename-regex)
	(buffer-substring-no-properties (match-beginning 1) (match-end 1)))))

(defun mhc-cmail-summary-display-article ()
  "Display the article on the current."
  (cmail-read-contents))

(defun mhc-cmail-get-import-buffer (get-original)
  ;; (if get-original (cmail-summary-display-asis)) ;; xxx
  (save-window-excursion
    (if (eq (cdr (assq major-mode mhc-cmail/summary-message-alist))
	    (progn (other-window 1) major-mode))
	(current-buffer))))

(defun mhc-cmail/date-to-buffer (date)
  "**cmail-summary**")

(defun mhc-cmail-generate-summary-buffer (date)
  (switch-to-buffer
   (set-buffer
    (mhc-get-buffer-create (mhc-cmail/date-to-buffer date))))
  (kill-all-local-variables)
  (setq inhibit-read-only t
	buffer-read-only nil
	selective-display t
	selective-display-ellipses nil
	indent-tabs-mode nil)
  (widen)
  (delete-region (point-min) (point-max)))

(defun mhc-cmail/schedule-foldermsg (schedule)
  (let ((path (mhc-record-name (mhc-schedule-record schedule))))
    (concat "\r " (or path "/dev/null"))))

(defun mhc-cmail-insert-summary-contents (inserter)
  (insert mhc-cmail/header-string)
  (funcall inserter)
  (insert (mhc-cmail/schedule-foldermsg mhc-tmp-schedule) "\n"))

(defun mhc-cmail-summary-mode-setup (date)
  (setq cmail-current-folder (mhc-date-format date "MHC:%04d-%02d" yy mm))
  (setq *cmail-disp-thread nil)
  (let ((cmail-highlight-mode  nil))
    (cmail-summary-mode)
    ;; moved code partially from cmail-mode-line-update 
    (setq mode-line-buffer-identification
	  (format "cmail: << %s >>" cmail-current-folder)))
  (setq selective-display t
	selective-display-ellipses nil
	indent-tabs-mode nil)
  (make-local-variable 'cmail-highlight-mode)
  (setq cmail-highlight-mode nil)
  (delete-other-windows))

(provide 'mhc-cmail)
(put 'mhc-cmail 'summary-filename 'mhc-cmail-summary-filename)
(put 'mhc-cmail 'summary-display-article 'mhc-cmail-summary-display-article)
(put 'mhc-cmail 'get-import-buffer 'mhc-cmail-get-import-buffer)
(put 'mhc-cmail 'draft-mode 'mhc-cmail-draft-mode)
(put 'mhc-cmail 'generate-summary-buffer 'mhc-cmail-generate-summary-buffer)
(put 'mhc-cmail 'insert-summary-contents 'mhc-cmail-insert-summary-contents)
(put 'mhc-cmail 'summary-search-date 'mhc-cmail-summary-search-date)
(put 'mhc-cmail 'summary-mode-setup 'mhc-cmail-summary-mode-setup)

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

;;; mhc-cmail.el ends here.
