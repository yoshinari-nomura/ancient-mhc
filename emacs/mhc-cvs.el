;;; -*- mode: Emacs-Lisp; coding: euc-japan -*-

;; Author:  TSUCHIYA Masatoshi <tsuchiya@namazu.org>,
;;          Hideyuki SHIRAI <shirai@quickhack.net>
;; Created: 2000/04/25
;; Revised: $Date$


;;; Commentary:

;; This file is a part of MHC, includes backend functions to
;; manipulate schedule files with CVS.

;; ����ϡ��������塼��ե������ CVS ���̤��ƴ������뤿��ΥХå�����
;; �ɤǤ��롣CVS ���̤��ƴ������뤳�Ȥˤ�äơ�ʣ���� PC ��ʬ�����Ƥ�
;; �륹�����塼��ե������Ʊ�����ưפ˼�뤳�Ƚ���롣

;;; Usage:

;; �������塼��ե����뤬����¸�ߤ��Ƥ�����ϡ���¸�Υ������塼��ե�
;; ������������ץ������Ȥ���������������塼��ե������������
;; ������
;;
;;     % cd ~/Mail/schedule
;;     % cvs import -m 'Initial Schdule' -I '.*' -I trash schedule name start
;;     % cd ..
;;     % rm -rf schedule
;;
;; �������塼��ե����뤬¸�ߤ��ʤ����ϡ����Υץ������Ȥ��äƤ�����
;;
;;     % mkdir schedule
;;     % cd schedule
;;     % cvs import -m "Initial Schdule" schedule name start
;;     % cd ..
;;     % rmdir schedule
;;
;; ���ˡ��ʲ�������� ~/.emacs �ʤɤ�Ŭ���ʾ����ɲä��Ƥ�����
;;
;;     (setq mhc-file-method 'mhc-cvs)
;;
;; �����ν�����ԤäƤ��顢���̤� mhc ��ƤӽФ�����������ȡ����
;; ư���� CVS ��ݥ��ȥ�ν�ߤ��䤤��碌��Τǡ�Ŭ�ڤ����Ϥ���ȡ���
;; �����塼��ե������ CVS ���̤��ƴ�������褦�ˤʤ롣
;; �⤷����ޤä� CVS ��ݥ��ȥ꤬���ꡢɸ��Ū�ʾ��Ǥʤ��Τʤ�
;;
;;     (setq mhc-cvs-repository-path ":ext:user@server:/cvsroot")
;;
;; �Τ褦�� ~/.emacs �˽񤤤Ƥ����С������餬ͥ�褹�롣�ޤ���CVS ��
;; module ̾�� "schedule" (mhc-base-folder ����) �Ǥʤ��Τʤ顢����̾����
;;
;;     (setq mhc-cvs-module-name "foo/schedule")
;;
;; �Τ褦�����ꤷ�Ƥ����Ȥ褤��

;;; Customize Variables:
(defcustom mhc-cvs-rsh
  (or (getenv "CVS_RSH") "ssh")
  "*The name of the remote shell command to use when starting a CVS server."
  :group 'mhc
  :type '(choice
	  (const :tag "No specification" nil)
	  (const :tag "Use SSH" "ssh")
	  (const :tag "Use RSH" "rsh")
	  (string :tag "Alternative program")))

(defcustom mhc-cvs-global-options
  (if mhc-default-network-status '("-f" "-w") '("-f" "-w" "-z9"))
  "*Global options are used when calling CVS."
  :group 'mhc
  :type '(repeat (string)))

(defcustom mhc-cvs-directory-separator
  '("/" . "_")
  "*Separator string for directories"
  :group 'mhc
  :type '(cons (string :tag "Directory Separator ")
	       (string :tag "Escape Character    ")))

(defcustom mhc-cvs-default-update-duration nil
  "*Default update months duration for mhc-cvs.
If 'nil', update all directories. '(-2 . 11) means the duration
from 'month before last' to 'this month next year'."
  :group 'mhc
  :type '(choice
	  (const :tag "All directories" nil)
	  (cons :tag "Duration"
		(integer :tag "start month offset" -2)
		(integer :tag "end month offset  " 11))))

(defcustom mhc-cvs-repository-path nil
  "*CVS repository path."
  :group 'mhc
  :type 'string)

(defcustom mhc-cvs-module-name nil
  "*MHC module name."
  :group 'mhc
  :type 'string)


;;; Internal Variable:
(defvar mhc-cvs/default-directory nil)


;;; Codes:

(defconst mhc-cvs/tmp-buffer-name " *mhc-cvs*")

(defsubst mhc-cvs/backend (options)
  "���ꤵ�줿���ץ������դ��ä��� CVS ��¹Ԥ���ؿ�"
  (let* ((buffer (mhc-get-buffer-create mhc-cvs/tmp-buffer-name))
	 (current-buffer (current-buffer))
	 (ret
	  (unwind-protect
	      (progn
		(set-buffer buffer)
		(delete-region (point-min) (point-max))
		(let ((default-directory (file-name-as-directory
					  mhc-cvs/default-directory))
		      (process-environment (copy-sequence process-environment)))
		  (setenv "CVS_RSH" mhc-cvs-rsh)
		  (apply #'call-process "cvs" nil t nil
			 (append mhc-cvs-global-options options))))
	    (set-buffer current-buffer))))
    (if (numberp ret)
	ret
      (message "error: mhc-cvs/backend: %s" ret)
      -1)))

(defun mhc-cvs/open (&optional offline)
  "�ͥåȥ���ξ��֤˰�¸���볫�Ͻ����ؿ�"
  (setq mhc-cvs/default-directory (mhc-summary-folder-to-path mhc-base-folder))
  (if offline
      (and (file-directory-p mhc-cvs/default-directory)
	   (file-directory-p (expand-file-name "CVS" mhc-cvs/default-directory)))
    (if (file-directory-p mhc-cvs/default-directory)
	(mhc-cvs/sync)
      (let ((module (file-name-nondirectory (mhc-summary-folder-to-path mhc-base-folder)))
	    (mhc-cvs/default-directory (mhc-summary-folder-to-path "")))
	(if mhc-cvs-module-name
	    (mhc-cvs/backend
	     (list"-d" (mhc-cvs/read-repository-path) "checkout"
		  "-d" module mhc-cvs-module-name))
	  (mhc-cvs/backend
	   (list "-d" (mhc-cvs/read-repository-path) "checkout" module)))))))

(defun mhc-cvs/read-repository-path ()
  "CVS��ݥ��ȥ�Υѥ�̾�����Ϥ���ؿ�"
  (or mhc-cvs-repository-path
      (let* ((default (catch 'found
			(mapcar (lambda (dir)
				  (and (stringp dir)
				       (throw 'found dir)))
				(list
				 (getenv "CVSROOT")
				 (expand-file-name "~/cvsroot")
				 (expand-file-name "~/CVS")))
			nil)) ; ���䤬���Ĥ���ʤ��ä����
	     (dir (read-from-minibuffer
		   (if default
		       (format "Input CVS repository path (default %s): " default)
		     "Input CVS repository path: "))))
	(if (not (string< "" dir))
	    default
	  dir))))

(defun mhc-cvs/shrink-file-name (file)
  "�ե�����̾�����Хѥ�������ؿ�"
  (file-relative-name
   (expand-file-name file)
   (mhc-summary-folder-to-path mhc-base-folder)))

(defun mhc-cvs/close (&optional offline)
  "�ͥåȥ���ξ��֤˰�¸���뽪λ�����ؿ�"
  (or offline (= 0 (mhc-cvs/backend (list "commit" "-m" "")))))

(defun mhc-cvs/sync (&optional full)
  "��⡼�ȤΥ������塼��ե�����ȥ�����Υ������塼��ե������Ʊ������ؿ�"
  (mhc-cvs/delay-add-and-remove (mhc-summary-folder-to-path mhc-base-folder))
  (mhc-cvs/update full)
  ;; rescan if mhc
  (or (and (mhc-summary-buffer-p)
	   (mhc-rescan-month mhc-default-hide-private-schedules))
      (and (mhc-calendar-p) (mhc-calendar-rescan)))
  t) ; return value

(defun mhc-cvs/delay-add-and-remove (directory)
  (let* ((entries (directory-files directory nil nil t))
	 (dir-entries entries))
    (while dir-entries
      ;; ���ե饤����֤λ����ɲä��줿�ǥ��쥯�ȥ��ºݤ��ɲä���
      (when (string-match (concat "^\\.mhc-cvs-added-\\(.*"
				  (regexp-quote (cdr mhc-cvs-directory-separator))
				  "\\)$")
			  (car dir-entries))
	(mhc-cvs/add (expand-file-name
		      (mhc-cvs/recover-directory-separator
		       (substring (car dir-entries)
				  (match-beginning 1) (match-end 1)))
		      directory))
	(setq entries (delete (car dir-entries) entries)))
      (setq dir-entries (cdr dir-entries)))
    (while entries
      (cond
       ;; ���ե饤����֤λ����ɲä��줿�ե������ºݤ��ɲä���
       ((string-match "^\\.mhc-cvs-added-" (car entries))
	(mhc-cvs/add (expand-file-name (mhc-cvs/recover-directory-separator
					(substring (car entries) (match-end 0)))
				       directory)))
       ;; ���ե饤����֤λ��˺�����줿�ե������ºݤ˺������
       ((string-match "^\\.mhc-cvs-removed-" (car entries))
	(mhc-cvs/remove (expand-file-name (mhc-cvs/recover-directory-separator
					   (substring (car entries) (match-end 0)))
					  directory))))
      (setq entries (cdr entries)))))

(defun mhc-cvs/recover-directory-separator (filename)
  (mapconcat 'identity
	     (mapcar (lambda (s)
		       (mapconcat 'identity
				  (mhc-misc-split s (cdr mhc-cvs-directory-separator))
				  (car mhc-cvs-directory-separator)))
		     (mhc-misc-split filename
				     (concat (cdr mhc-cvs-directory-separator)
					     (cdr mhc-cvs-directory-separator))))
	     (cdr mhc-cvs-directory-separator)))

(defun mhc-cvs/escape-directory-separator (filename)
  (mapconcat 'identity
	     (mapcar (lambda (s)
		       (mapconcat 'identity
				  (mhc-misc-split s (car mhc-cvs-directory-separator))
				  (cdr mhc-cvs-directory-separator)))
		     (mhc-misc-split filename (cdr mhc-cvs-directory-separator)))
	     (concat (cdr mhc-cvs-directory-separator)
		     (cdr mhc-cvs-directory-separator))))

(defun mhc-cvs/get-added-flag-file-name (filename)
  (let ((dir (mhc-summary-folder-to-path mhc-base-folder)))
    (expand-file-name
     (format ".mhc-cvs-added-%s"
	     (mhc-cvs/escape-directory-separator (file-relative-name filename dir)))
     dir)))

(defun mhc-cvs/get-removed-file-name (filename)
  (let ((dir (mhc-summary-folder-to-path mhc-base-folder)))
    (expand-file-name
     (format ".mhc-cvs-removed-%s"
	     (mhc-cvs/escape-directory-separator (file-relative-name filename dir)))
     dir)))

(defun mhc-cvs/add (filename &optional offline)
  "�ե�������ɲä���ؿ�"
  (let ((added (mhc-cvs/get-added-flag-file-name filename))
	(removed (mhc-cvs/get-removed-file-name filename)))
    (if (file-exists-p removed) (delete-file removed))
    (if offline
	(if (file-directory-p filename)
	    (not (write-region "add directory" nil added nil 'nomsg))
	  (not (copy-file filename added t)))
      (if (file-exists-p added) (delete-file added))
      (and (= 0 (mhc-cvs/backend (list "add" (mhc-cvs/shrink-file-name filename))))
	   (mhc-cvs/modify filename)))))

(defun mhc-cvs/remove (filename &optional offline)
  "�ե������������ؿ�"
  (let ((added (mhc-cvs/get-added-flag-file-name filename))
	(removed (mhc-cvs/get-removed-file-name filename))
	(new-path (expand-file-name
		   "trash"
		   (mhc-summary-folder-to-path mhc-base-folder))))
    (or (file-directory-p new-path)
	(make-directory new-path))
    (if offline
	(progn
	  (if (file-exists-p added)
	      (progn
		(delete-file added)
		(rename-file filename (mhc-misc-get-new-path new-path)))
	    (copy-file filename (mhc-misc-get-new-path new-path))
	    (rename-file filename removed t))
	  t)	;; return value
      (if (file-exists-p added) (delete-file added))
      (if (file-exists-p removed) (delete-file removed))
      (if (file-exists-p filename)
	  (rename-file filename (mhc-misc-get-new-path new-path)))
      (and (= 0 (mhc-cvs/backend (list "remove"
				       (mhc-cvs/shrink-file-name filename))))
	   (mhc-cvs/modify filename)))))

(defun mhc-cvs/modify (filename &optional offline)
  "�ե�������ѹ�����ؿ�"
  (or offline
      (prog1 (= 0 (mhc-cvs/backend
		   (list "commit" "-m" ""
			 (mhc-cvs/shrink-file-name filename))))
	(let ((modes (file-modes filename)))
	  (when modes
	    (set-file-modes filename (logior ?\200 modes)))))))


;;; CVS Backend Function

(defsubst mhc-cvs/touch-directory (directory)
  (mhc-misc-touch-directory directory)
  (mhc-slot-destruct-cache directory))


(defun mhc-cvs/update-dirs ()
  "mhc-cvs-default-update-duration �ǻ��ꤵ�줿�ϰϤ� directory ���֤�"
  (when mhc-cvs-default-update-duration
    (let ((cdate (or (mhc-current-date) (mhc-calendar-get-date) (mhc-date-now)))
	  (i (- (cdr mhc-cvs-default-update-duration)
		(car mhc-cvs-default-update-duration)))
	  dirs schdir)
      (when (file-directory-p
	     (expand-file-name "intersect" mhc-cvs/default-directory))
	(setq dirs (cons "intersect" dirs)))
      (setq cdate (mhc-date-mm+ cdate (car mhc-cvs-default-update-duration)))
      (while (>= i 0)
	(setq schdir (mhc-date-format cdate "%04d/%02d" yy mm))
	(when (file-directory-p
	       (expand-file-name
		schdir (mhc-summary-folder-to-path mhc-base-folder)))
	  (setq dirs (cons schdir dirs)))
	(setq cdate (mhc-date-mm++ cdate))
	(setq i (1- i)))
      (nreverse dirs))))

(defun mhc-cvs/update (&optional full)
  "cvs update ��¹Ԥ�����̤���Ϥ���ؿ�"
  ;; ������Υ������塼��ե������ update ����
  (prog1 (mhc-cvs/backend (append (list "update" "-d" "-I" ".*" "-I" "trash")
				  (and (null full) (mhc-cvs/update-dirs))))
    (let (modified-files conflict-files updated-files
			 commit-fault-files unknown-files)
      ;; update �η�̤���Ϥ���
      (let ((buffer (get-buffer mhc-cvs/tmp-buffer-name))
	    (current-buffer (current-buffer)))
	(unwind-protect
	    (let ((current-folder))
	      (set-buffer buffer)
	      (goto-char (point-min))
	      (while (not (eobp))
		(cond
		 ((looking-at "\\? ")
		  (setq unknown-files
			(cons (buffer-substring (match-end 0) (progn (end-of-line) (point)))
			      unknown-files)))
		 ((looking-at "[AMR] ")
		  (setq modified-files
			(cons (buffer-substring (match-end 0) (progn (end-of-line) (point)))
			      modified-files)))
		 ((looking-at "C ")
		  (setq conflict-files
			(cons (buffer-substring (match-end 0) (progn (end-of-line) (point)))
			      conflict-files)))
		 ((looking-at "[UP] ")
		  (setq updated-files
			(cons (buffer-substring (match-end 0) (progn (end-of-line) (point)))
			      updated-files)))
		 ((looking-at "cvs\\(\.exe\"\\)? \\(update:\\|server:\\) Updating ")
		  (setq current-folder
			(buffer-substring (match-end 0) (progn (end-of-line) (point)))))
		 ((looking-at "cvs\\(\.exe\"\\)? \\(update:\\|server:\\) warning:")
		  (mhc-cvs/touch-directory
		   (expand-file-name current-folder
				     (mhc-summary-folder-to-path mhc-base-folder)))))
		(forward-line 1)))
	  (set-buffer current-buffer)))
      ;; �ѹ��Τ��ä��ǥ��쥯�ȥ�� .mhc-mtime �򹹿����Ƥ���
      (while updated-files
	(mhc-cvs/touch-directory
	 (file-name-directory
	  (expand-file-name (car updated-files)
			    (mhc-summary-folder-to-path mhc-base-folder))))
	(setq updated-files (cdr updated-files)))
      ;; ��������Ƥ���ե�����ϡ�¨�¤� commit ����
      (while modified-files
	(or (= 0 (mhc-cvs/backend (list "commit" "-m" "" (car modified-files))))
	    (setq commit-fault-files (cons (car modified-files) commit-fault-files)))
	(setq modified-files (cdr modified-files)))
      ;; ��ư�ǽ񤤤��Ȼפ���ե�����򰷤���MHC �Υǡ����Ȥ��ƴ������Ƥ��ʤ��Ȥ����ʤ���
      (when unknown-files
	(mhc-cvs/unknown-file unknown-files))
      (if commit-fault-files
	  (message "File(s) are fault to commit: %s"
		   (mapconcat (lambda (s) s) commit-fault-files ",")))
      ;; ������ conflict �򵯤����Ƥ���ե�����Ͻ��������㤦
      (if conflict-files
	  (mhc-cvs-edit-conflict-file
	   (mapcar (lambda (file)
		     (expand-file-name file (mhc-summary-folder-to-path mhc-base-folder)))
		   conflict-files))))))


(defun mhc-cvs-edit-conflict-file (&optional files)
  (if (setq files (or files (get 'mhc-cvs-edit-conflict-file 'conflict-files)))
      (progn
	(put 'mhc-cvs-edit-conflict-file 'conflict-files (cdr files))
	(message "Conflict has been occured. file=%s" (car files))
	(mhc-modify-file (car files)))
    (put 'mhc-cvs-edit-conflict-file 'conflict-files nil)))


(defun mhc-cvs/unknown-file (unknowns)
  (let (dirs files dir file expf char loop mhcp record)
    (while unknowns
      (setq expf (expand-file-name
		  (car unknowns)
		  (mhc-summary-folder-to-path mhc-base-folder)))
      (cond
       ((and (file-directory-p expf)
	     (or (string-match
		  "^[12][0-9][0-9][0-9]/[01][0-9]$" (car unknowns))
		 (string-match
		  "^[12][0-9][0-9][0-9]$" (car unknowns))
		 (string-match
		  "^intersect$" (car unknowns))))
	(setq dirs (cons (car unknowns) dirs)))
       ((and (file-regular-p expf)
	     (or (string-match
		  "^[12][0-9][0-9][0-9]/[01][0-9]/[1-9][0-9]*$" (car unknowns))
		 (string-match
		  "^intersect/[1-9][0-9]*$" (car unknowns))))
	(setq files (cons (car unknowns) files))))
      (setq unknowns (cdr unknowns)))
    (while (setq dir (car dirs))
      (setq dirs (cdr dirs))
      (mhc-cvs/backend (list "add" dir))
      (save-excursion
	(set-buffer mhc-cvs/tmp-buffer-name)
	(goto-char (point-min))
	(when (looking-at "\\? ")
	  (setq file (buffer-substring (match-end 0) (progn (end-of-line) (point))))
	  (setq expf (expand-file-name
		      file
		      (mhc-summary-folder-to-path mhc-base-folder)))
	  (cond
	   ((and (file-directory-p expf)
		 (string-match
		  "^[12][0-9][0-9][0-9]/[01][0-9]$" file))
	    (setq dirs (cons file dirs)))
	   ((and (file-regular-p expf)
		 (or (string-match
		      "^[12][0-9][0-9][0-9]/[01][0-9]/[1-9][0-9]*$" file)
		     (string-match
		      "^intersect/[1-9][0-9]*$" file)))
	    (setq files (cons file files)))))))
    (while (setq file (car files))
      (setq expf (expand-file-name
		  file (mhc-summary-folder-to-path mhc-base-folder)))
      (with-temp-buffer
	(insert-file-contents expf)
	(setq mhcp
	      (mhc-header-narrowing
		(and (mhc-header-valid-p "x-sc-subject")
		     (mhc-header-valid-p "x-sc-record-id")
		     (or (mhc-header-valid-p "x-sc-day")
			 (mhc-header-valid-p "x-sc-cond")))))
	(when mhcp
	  (setq record (mhc-parse-buffer (mhc-record-new expf)))))
      (when mhcp
	(setq loop t)
	(while loop
	  (message "[file: %s] ? A)dd CVS repository, R)emove immediately, M)ove to trash"
		   file)
	  (condition-case nil
	      (setq char (read-char))
	    (error (setq char ?Z)))	;; dummy set
	  (cond
	   ((memq char '(?a ?A))
	    (setq loop nil)
	    (message "[file: %s]  Add CVS repository..." file)
	    (mhc-record/append-log record 'add)
	    (and (= 0 (mhc-cvs/backend (list "add" file)))
		 (mhc-cvs/modify expf))
	    (message "[file: %s]  Add CVS repository...done" file))
	   ((memq char '(?r ?R))
	    (setq loop nil)
	    (message "")
	    (delete-file expf))
	   ((memq char '(?m ?M))
	    (setq loop nil)
	    (message "")
	    (rename-file
	     expf
	     (mhc-misc-get-new-path
	      (expand-file-name "trash"
				(mhc-summary-folder-to-path mhc-base-folder))))))))
      (setq files (cdr files)))))


(add-hook 'mhc-draft-finish-hook 'mhc-cvs-edit-conflict-file)


(provide 'mhc-cvs)
(put 'mhc-cvs 'open   'mhc-cvs/open)
(put 'mhc-cvs 'close  'mhc-cvs/close)
(put 'mhc-cvs 'sync   'mhc-cvs/sync)
(put 'mhc-cvs 'add    'mhc-cvs/add)
(put 'mhc-cvs 'modify 'mhc-cvs/modify)
(put 'mhc-cvs 'remove 'mhc-cvs/remove)

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

;;; mhc-cvs.el ends here.
