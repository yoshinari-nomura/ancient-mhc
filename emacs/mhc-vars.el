;;; -*- mode: Emacs-Lisp; coding: euc-japan -*-

;; Author:  Yoshinari Nomura <nom@quickhack.net>,
;; Created: 2000/04/30
;; Revised: $Date$


;;; Commentary:

;; This file is a part of MHC, and includes defintions of global
;; confiration variables.


;;; Code:
(require 'mhc-compat)


;;; Constants:
(defconst mhc-version "mhc version unknown")


;;; Configration Variables:
(defgroup mhc nil
  "Various sorts of MH Calender."
  :group 'mail)

(defcustom mhc-mailer-package 'mew
  "*Variable to set your favorite mailer."
  :group 'mhc
  :type '(radio (const :tag "Mew" mew)
		(const :tag "Wanderlust" wl)
		(const :tag "Gnus" gnus)))

(defcustom mhc-base-folder "+schedule"
  "*Base foler of MHC"
  :group 'mhc
  :type 'string)

(defcustom mhc-mail-path
  (expand-file-name
   (if (and (boundp 'mew-mail-path) mew-mail-path) mew-mail-path "~/Mail"))
  "*Base directory your mailer recognized as `+'"
  :group 'mhc
  :type 'directory)

(defcustom mhc-schedule-file (expand-file-name "~/.schedule")
  "*MHC DB file which contains holiday and anniversary settings."
  :group 'mhc
  :type 'file)

(defcustom mhc-insert-calendar t
  "*If non nil value, display vertical calender."
  :group 'mhc
  :type 'boolean)

(defcustom mhc-insert-todo-list t
  "*If non nil value, display TODO list."
  :group 'mhc
  :type 'boolean)

(defcustom mhc-default-coding-system
  (if (>= emacs-major-version 20) 'iso-2022-jp '*iso-2022-jp*)
  "*Default coding system for MHC schedule files."
  :group 'mhc
  :type 'symbol)

(defcustom mhc-default-hide-private-schedules nil
  "*If non-nil value, hide private schedules."
  :group 'mhc
  :type 'boolean)

(defcustom mhc-default-network-status t
  "*Flag of the default network status."
  :group 'mhc
  :type 'boolean)

(defcustom mhc-show-network-status t
  "*Flag to show the network status."
  :group 'mhc
  :type 'boolean)

(defcustom mhc-use-cache t
  "*Flag to decide whether to use cache or not."
  :group 'mhc
  :type '(radio (const :tag "Use" t)
		(const :tag "Lazy check" 0)
		(const :tag "No use" nil)))

(defcustom mhc-calendar-mode-hook nil
  "*Hook called in mhc-calendar-mode."
  :group 'mhc
  :type 'hook)

(defcustom mhc-calendar-create-buffer-hook nil
  "*Hook called in mhc-calendar-create-buffer."
  :group 'mhc
  :type 'hook)

(defcustom mhc-calendar-start-column 4
  "*Size of left margin."
  :group 'mhc
  :type 'integer)

(defcustom mhc-calendar-next-offset 24
  "*Offset of next month start column (greater or equal 23)."
  :group 'mhc
  :type 'integer)

(defcustom mhc-calendar-height (if (and (featurep 'xemacs) window-system) 12 9)
  "*Offset of next month start column (greater or equal 9)."
  :group 'mhc
  :type 'integer)

(defcustom mhc-calendar-view-summary nil
  "*View day's summary if *non-nil*."
  :group 'mhc
  :type 'boolean)

(defcustom mhc-calendar-link-hnf nil
  "*Support HNF(Hyper Nikki File) mode if *non-nil*."
  :group 'mhc
  :type 'boolean)

(defcustom mhc-calendar-use-mouse-highlight t
  "*Highlight mouse pointer."
  :group 'mhc
  :type 'boolean)

(defcustom mhc-calendar-view-file-hook nil
  "*Hook called in mhc-calendar-view-file."
  :group 'mhc
  :type 'hook)

(provide 'mhc-vars)

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

;;; mhc-vars.el ends here
