;;; -*- emacs-lisp -*-
;; mhc-sync.el -- mhc-sync (ruby script) interface
;;
;; Author:  Hideyuki SHIRAI <shirai@rdmg.mgcs.mei.co.jp>
;;
;; Created: 06/12/2000
;; Revised: 06/12/2000

;;; interanal variabile
(defconst mhc-sync-passwd-regex "password:\\|passphrase:\\|Enter passphrase")
(defvar mhc-sync-process nil)
(defvar mhc-sync-req-passwd nil)

;;; function
(defun mhc-sync ()
  "Execute mhc-sync."
  (interactive)
  (if (and (stringp mhc-sync-remote) (stringp mhc-sync-id))
      (if (processp mhc-sync-process)
	  (message "another mhc-sync running.")
	(let ((buf (get-buffer-create "*mhc-sync*"))
	      (ldir (expand-file-name (or mhc-sync-localdir "~/Mail/schedule"))))
	  (pop-to-buffer buf)
	  (setq buffer-read-only nil)
	  (erase-buffer)
	  (setq buffer-read-only t)
	  (message "mhc-sync ...")
	  (setq mhc-sync-req-passwd t)
	  (setq mhc-sync-process
		(apply (function start-process)
		       "mhc-sync" buf "mhc-sync"
		       (list "-x" mhc-sync-id "-r" ldir mhc-sync-remote)))
	  (set-process-coding-system mhc-sync-process mhc-sync-coding-system)
	  (set-process-filter mhc-sync-process 'mhc-sync-filter)
	  (set-process-sentinel mhc-sync-process 'mhc-sync-sentinel)))
    (message "No remote server specified.")))

(defun mhc-sync-filter (process string)
  (if (bufferp (process-buffer process))
      (let ((obuf (buffer-name)))
	(unwind-protect
	    (progn
	      (set-buffer (process-buffer process))
	      (let ((buffer-read-only nil)
		    passwd)
		(goto-char (point-max))
		(insert string)
		(cond
		 ((and mhc-sync-req-passwd
		       (string-match mhc-sync-passwd-regex string))
		  (setq passwd (mhc-misc-read-passwd string))
		  (process-send-string process (concat passwd "\n")))
		 ((string-match "---------------------" string)
		  (setq mhc-sync-req-passwd nil)))))
	  (if (get-buffer obuf)
	      (set-buffer obuf))))))

(defun mhc-sync-sentinel (process event)
  (if (bufferp (process-buffer process))
      (progn
	(pop-to-buffer (process-buffer process))
	(let ((buffer-read-only nil))
	  (goto-char (point-max))
	  (insert "<<<transfer finish>>>"))))
  (setq mhc-sync-process nil)
  (message "mhc-sync ... done."))

(provide 'mhc-sync)

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

;; mhc-calendar.el ends here
