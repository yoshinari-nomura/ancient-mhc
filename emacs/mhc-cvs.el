;;; -*- mode: Emacs-Lisp; coding: euc-japan -*-

;; Author:  TSUCHIYA Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>,
;;          Hideyuki SHIRAI <shirai@quickhack.net>
;; Created: 2000/04/25
;; Revised: $Date$


;;; Commentary:

;; This file is a part of MHC, includes backend functions to
;; manipulate schedule files with CVS.

;; これは、スケジュールファイルを CVS を通して管理するためのバックエン
;; ドである。CVS を通して管理することによって、複数の PC に分散してい
;; るスケジュールファイルの同期を容易に取ること出来る。

;;; Usage:

;; スケジュールファイルが既に存在している場合は、既存のスケジュールファ
;; イルを管理するプロジェクトを作成し、スケジュールファイルを削除して
;; おく。
;;
;;     % cd ~/Mail/schedule
;;     % cvs import -m 'Initial Schdule' -I '.*' -I trash schedule name start
;;     % cd ..
;;     % rm -rf schedule
;;
;; スケジュールファイルが存在しない場合は、空のプロジェクトを作っておく。
;;
;;     % mkdir schedule
;;     % cd schedule
;;     % cvs import -m "Initial Schdule" schedule name start
;;     % cd ..
;;     % rmdir schedule
;;
;; 更に、以下の設定を ~/.emacs などの適当な場所に追加しておく。
;;
;;     (setq mhc-file-method 'mhc-cvs)
;;
;; これらの準備を行ってから、普通に mhc を呼び出す。そうすると、初回起
;; 動時に CVS レポジトリの所在を問い合わせるので、適切に入力すると、ス
;; ケジュールファイルを CVS を通して管理するようになる。
;; もし、決まった CVS レポジトリがあり、標準的な場所でないのなら
;;
;;     (setq mhc-cvs-repository-path ":ext:user@server:/cvsroot")
;;
;; のように ~/.emacs に書いておけば、そちらが優先する。また、CVS の
;; module 名が "schedule" (mhc-base-folder 参照) でないのなら、その名前を
;;
;;     (setq mhc-cvs-module-name "foo/schedule")
;;
;; のように設定しておくとよい。

;;; Customize Variables:
(defcustom mhc-cvs-rsh
  (and (getenv "CVS_RSH") "ssh")
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

(defsubst mhc-cvs/backend (&rest options)
  "指定されたオプションを付け加えて CVS を実行する関数"
  (let ((buffer (mhc-get-buffer-create mhc-cvs/tmp-buffer-name))
	(current-buffer (current-buffer)))
    (unwind-protect
	(progn
	  (set-buffer buffer)
	  (delete-region (point-min) (point-max))
	  (let ((default-directory (file-name-as-directory mhc-cvs/default-directory))
		(process-environment process-environment))
	    (setenv "CVS_RSH" mhc-cvs-rsh)
	    (apply #'call-process "cvs" nil t nil
		   (append mhc-cvs-global-options options))))
      (set-buffer current-buffer))))

(defun mhc-cvs/open (&optional offline)
  "ネットワークの状態に依存する開始処理関数"
  (setq mhc-cvs/default-directory (mhc-summary-folder-to-path mhc-base-folder))
  (if offline
      (and (file-directory-p mhc-cvs/default-directory)
	   (file-directory-p (expand-file-name "CVS" mhc-cvs/default-directory)))
    (if (file-directory-p mhc-cvs/default-directory)
	(mhc-cvs/sync)
      (let ((module (file-name-nondirectory (mhc-summary-folder-to-path mhc-base-folder)))
	    (mhc-cvs/default-directory (mhc-summary-folder-to-path "")))
	(if mhc-cvs-module-name
	    (mhc-cvs/backend "-d" (mhc-cvs/read-repository-path) "checkout"
			     "-d" module mhc-cvs-module-name)
	  (mhc-cvs/backend "-d" (mhc-cvs/read-repository-path) "checkout" module))))))

(defun mhc-cvs/read-repository-path ()
  "CVSレポジトリのパス名を入力する関数"
  (or mhc-cvs-repository-path
      (let* ((default (catch 'found
			(mapcar (lambda (dir)
				  (and (stringp dir)
				       (throw 'found dir)))
				(list
				 (getenv "CVSROOT")
				 (expand-file-name "~/cvsroot")
				 (expand-file-name "~/CVS")))
			nil)) ; 候補が見つからなかった場合
	     (dir (read-from-minibuffer
		   (if default
		       (format "Input CVS repository path (default %s): " default)
		     "Input CVS repository path: "))))
	(if (not (string< "" dir))
	    default
	  dir))))

(defun mhc-cvs/shrink-file-name (file)
  "ファイル名の相対パスを得る関数"
  (file-relative-name
   (expand-file-name file)
   (mhc-summary-folder-to-path mhc-base-folder)))

(defun mhc-cvs/close (&optional offline)
  "ネットワークの状態に依存する終了処理関数"
  (or offline (= 0 (mhc-cvs/backend "commit" "-m" ""))))

(defun mhc-cvs/sync ()
  "リモートのスケジュールファイルとローカルのスケジュールファイルの同期を取る関数"
  (mhc-cvs/delay-add-and-remove (mhc-summary-folder-to-path mhc-base-folder))
  (mhc-cvs/update)
  ;; rescan if mhc
  (or (and (mhc-summary-buffer-p)
	   (mhc-rescan-month mhc-default-hide-private-schedules))
      (and (mhc-calendar-p) (mhc-calendar-rescan)))
  t) ; return value

(defun mhc-cvs/delay-add-and-remove (directory)
  (let ((entries (directory-files directory nil nil t)))
    (while entries
      (cond
       ;; オフライン状態の時に追加されたファイルを実際に追加する
       ((string-match "^\\.mhc-cvs-added-" (car entries))
	(mhc-cvs/add (expand-file-name (mhc-cvs/recover-directory-separator
					(substring (car entries) (match-end 0)))
				       directory)))
       ;; オフライン状態の時に削除されたファイルを実際に削除する
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
  "ファイルを追加する関数"
  (let ((added (mhc-cvs/get-added-flag-file-name filename))
	(removed (mhc-cvs/get-removed-file-name filename)))
    (if (file-exists-p removed) (delete-file removed))
    (if offline
	(not (copy-file filename added t))
      (if (file-exists-p added) (delete-file added))
      (setq filename (mhc-cvs/shrink-file-name filename))
      (and (= 0 (mhc-cvs/backend "add" filename))
	   (mhc-cvs/modify filename)))))

(defun mhc-cvs/remove (filename &optional offline)
  "ファイルを削除する関数"
  (let ((added (mhc-cvs/get-added-flag-file-name filename))
	(removed (mhc-cvs/get-removed-file-name filename)))
    (if offline
	(if (file-exists-p added)
	    (progn (delete-file added) (delete-file filename) t)
	  (not (rename-file filename removed t)))
      (if (file-exists-p added) (delete-file added))
      (if (file-exists-p removed) (delete-file removed))
      (if (file-exists-p filename) (delete-file filename))
      (setq filename (mhc-cvs/shrink-file-name filename))
      (and (= 0 (mhc-cvs/backend "remove" filename))
	   (mhc-cvs/modify filename)))))

(defun mhc-cvs/modify (filename &optional offline)
  "ファイルを変更する関数"
  (or offline (= 0 (mhc-cvs/backend "commit" "-m" "" filename))))


;;; CVS Backend Function

(defsubst mhc-cvs/touch-directory (directory)
  (mhc-misc-touch-directory directory)
  (mhc-slot-destruct-cache directory))

(defun mhc-cvs/update ()
  "cvs update を実行した結果を解析する関数"
  ;; ローカルのスケジュールファイルを update する
  (prog1 (mhc-cvs/backend "update" "-d" "-I" ".*" "-I" "trash")
    (let (modified-files conflict-files updated-files commit-fault-files)
      ;; update の結果を解析する
      (let ((buffer (get-buffer mhc-cvs/tmp-buffer-name))
	    (current-buffer (current-buffer)))
	(unwind-protect
	    (let ((current-folder))
	      (set-buffer buffer)
	      (goto-char (point-min))
	      (while (not (eobp))
		(cond
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
      ;; 変更のあったディレクトリの .mhc-mtime を更新しておく
      (while updated-files
	(mhc-cvs/touch-directory
	 (file-name-directory
	  (expand-file-name (car updated-files)
			    (mhc-summary-folder-to-path mhc-base-folder))))
	(setq updated-files (cdr updated-files)))
      ;; 修正されているファイルは、即座に commit する
      (while modified-files
	(or (= 0 (mhc-cvs/backend "commit" "-m" "" (car modified-files)))
	    (setq commit-fault-files (cons (car modified-files) commit-fault-files)))
	(setq modified-files (cdr modified-files)))
      (if commit-fault-files
	  (message "File(s) are fault to commit: %s"
		   (mapconcat (lambda (s) s) commit-fault-files ",")))
      ;; 修正が conflict を起こしているファイルは修正して貰う
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
