;;; -*- mode: Emacs-Lisp; coding: euc-japan -*-

;; Author:  TSUCHIYA Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>
;;          Hideyuki SHIRAI <shirai@quickhack.net>
;; Created: 2000/06/18
;; Revised: $Date: 2000/07/19 00:00:06 $


;;; Commentary:

;; This file is a part of MHC and includes functions to make
;; PostScrpit calendar.


;;; History:

;; Original PostScript program was written
;; by Patrick Wood <patwood@unirot.UUCP> in 1987.
;;
;; Shell stuff added by King Ables at Sep 3, 1987.
;;
;; Made pretty by tjt in 1988.
;;
;; Holiday and printer flag passing hacks added by
;; smann@june.cs.washington.edu in Dec 1988.
;;
;; Used the better looking version with 5 rows of days rather than 6
;; hacked together with holiday and banner/footnotes added
;; by Joe Wood <jlw@lzga.ATT.COM> in Dec 1989.
;;
;; Fixed "-R" (didn't work at all; now it at least works on 8.5x11)
;; and also fixed handling of unrecognized arguments
;; by Jeff Mogul <mogul@decwrl.dec.com> in Jan 1990.
;;
;; Japanized and improved handling holidays
;; by SUZUKI Shingo <ichimal@takopen.cs.uec.ac.jp> in Feb 2000.
;;
;; Stuffs rewritten with Emacs Lisp
;; by TSUCHIYA Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>
;; in Jun 2000.


;;; Bugs:

;; This program doesn't work for months before 1753 (weird stuff
;; happened in September, 1752).


;;; Code:

(require 'mhc)


;;; Customize variables:

(defcustom mhc-ps-preview-command "gv"
  "*Command to preview PostScript calendar."
  :group 'mhc
  :type 'string)

(defcustom mhc-ps-preview-command-arguments '()
  "*Argument of previewer"
  :group 'mhc
  :type '(repeat string))

(defcustom mhc-ps-print-command "lp"
  "*Command to print PostScript calendar."
  :group 'mhc
  :type 'string)

(defcustom mhc-ps-print-command-arguments '()
  "*Argument of print command."
  :group 'mhc
  :type '(repeat string))

(defcustom mhc-ps-paper-type t
  "*Calendar paper type."
  :group 'mhc
  :type '(radio (const :tag "Landscape" t)
		(const :tag "Portrait" nil)))

(defcustom mhc-ps-paper-fill-print nil
  "*Fill printing just in Landscape paper size."
  :group 'mhc
  :type 'boolean)

(defcustom mhc-ps-truncate-lines nil
  "*Truncate line."
  :group 'mhc
  :type 'boolean)

(defcustom mhc-ps-left-margin 2
  "*Left margin of the each schedule."
  :group 'mhc
  :type 'integer)

(defcustom mhc-ps-string-width 20
  "*Width of the each schedule."
  :group 'mhc
  :type 'integer)

(defcustom mhc-ps-string-column 7
  "*Column of the each schedule."
  :group 'mhc
  :type 'integer)

(defcustom mhc-ps-title-font "Times-Bold"
  "*PostScript Font used for title."
  :group 'mhc
  :type 'string)

(defcustom mhc-ps-day-font "Helvetica-Bold"
  "*PostScript Font used for days."
  :group 'mhc
  :type 'string)

(defcustom mhc-ps-event-font "Times-Roman"
  "*PostScript Font used for events."
  :group 'mhc
  :type 'string)

(defcustom mhc-ps-japanese-font "Ryumin-Light-EUC-H"
  "*PostScript Font used for Japanese characters."
  :group 'mhc
  :type 'string)

(defcustom mhc-ps-coding-system
  (if (boundp 'MULE) '*euc-japan*unix 'euc-japan-unix)
  "*Coding system of PostScript data."
  :group 'mhc
  :type 'symbol)


;;; Internal Variables:

(defconst mhc-ps/string "\
%!
% PostScript program to draw calendar
% Copyright \(C\) 1987 by Pipeline Associates, Inc.
% Permission is granted to modify and distribute this free of charge.

% The number after /month should be set to a number from 1 to 12.
% The number after /year should be set to the year you want.
% You can change the title and date fonts, if you want.
% We figure out the rest.
% This program won't produce valid calendars before 1800 due to the switch
% from Julian to Gregorian calendars in September of 1752 wherever English
% was spoken.

%% For Japanese. Added by ichimal, 2000/2/6.
%% Original code is generated by k2ps.
/copyfont {     % font-dic extra-entry-count  copyfont  font-dic
        1 index maxlength add dict begin
        {       1 index /FID ne 2 index /UniqueID ne and
                {def}{pop pop} ifelse
        } forall
        currentdict
        end
} bind def

%% For Japanese. Added by ichimal, 2000/2/6.
%% Original code is generated by k2ps.
/narrowfont {   % ASCIIFontName EUCFontName  compositefont  font'
    findfont dup /FontType get 0 eq {
        12 dict begin
            %
            % 7+8 bit EUC font
            %
            12 dict begin
                /EUCFont exch def
                /FontInfo \(7+8 bit EUC font\) readonly def
                /PaintType 0 def
                /FontType 0 def
                /FontMatrix matrix def
                % /FontName
                /Encoding \[
                    16#00 1 16#20 { pop 0 } for
                    16#21 1 16#28 { 16#20 sub } for
                    16#29 1 16#2F { pop 0 } for
                    16#30 1 16#74 { 16#27 sub } for
                    16#75 1 16#FF { pop 0 } for
                \] def
                /FMapType 2 def
                EUCFont /WMode known
                { EUCFont /WMode get /WMode exch def }
                { /WMode 0 def } ifelse
                /FDepVector \[
                    EUCFont /FDepVector get 0 get
                    \[ 16#21 1 16#28 {} for 16#30 1 16#74 {} for \]
                    {
                        13 dict begin
                            /EUCFont EUCFont def
                            /UpperByte exch 16#80 add def       
                            % /FontName
                            /FontInfo \(EUC lower byte font\) readonly def
                            /PaintType 0 def
                            /FontType 3 def
                            /FontMatrix matrix def
                            /FontBBox {0 0 0 0} def
                            /Encoding \[
                                16#00 1 16#A0 { pop /.notdef } for
                                16#A1 1 16#FE { 16#80 sub 16 2 string cvrs
                                                \(cXX\) dup 1 4 -1 roll
                                                putinterval cvn } for
                                /.notdef
                            \] def
                            % /UniqueID
                            % /WMode
                            /BuildChar {
                                gsave
                                exch dup /EUCFont get setfont
                                /UpperByte get
                                2 string
                                dup 0 4 -1 roll put
                                dup 1 4 -1 roll put
                                dup stringwidth setcharwidth
                                0 0 moveto show
                                grestore
                            } bind def
                            currentdict
                        end
                        /lowerbytefont exch definefont
                    } forall
                \] def
                currentdict
            end
            /eucfont exch definefont
            exch
            findfont 1 copyfont dup begin
                /FontMatrix FontMatrix \[.83 0 0 1 0 0.05\] matrix concatmatrix def
            end
            /asciifont exch definefont
            exch
            /FDepVector \[ 4 2 roll \] def
            /FontType 0 def
            /WMode 0 def
            /FMapType 4 def
            /FontMatrix matrix def
            /Encoding \[0 1\] def
            /FontBBox {0 0 0 0} def
            currentdict
        end
    }{
        pop findfont 0 copyfont
    } ifelse
} def

/month @MONTH@ def
/year @YEAR@ def
/titlefont /@TFONT@ def
/dayfont /@DFONT@ def
%% For Japanese. Changed by ichimal, 2000/2/6.
%% Original code is generated by k2ps.
% /eventfont /@EFONT@ def
/Courier-Ryumin
    /Courier /@JFONT@ narrowfont definefont pop
/eventfont /Courier-Ryumin def

/holidays \[ @HOLIDAYS@ \] def
/schedules \[ @SCHEDULES@ \] def
/Bannerstring \(@BANNER@\) def
/Lfootstring \(@LFOOT@\) def
/Rfootstring \(@RFOOT@\) def
/Cfootstring \(@CFOOT@\) def

% calendar names - change these if you don't speak english
% \"August\", \"April\" and \"February\" could stand to be kerned even if you do

/month_names
\[ \(January\) \(February\) \(March\) \(April\) \(May\) \(June\) \(July\)
\(August\) \(September\) \(October\) \(November\) \(December\) \]
def

/day_names
\[ \(Sunday\) \(Monday\) \(Tuesday\) \(Wednesday\) \(Thursday\) \(Friday\) \(Saturday\) \]
def

% layout parameters - you can change these, but things may not look nice

/daywidth 100 def
/dayheight 95 def

/titlefontsize 48 def
/weekdayfontsize 10 def
/datefontsize 24 def
/footfontsize 20 def

/topgridmarg 35 def
/leftmarg 35 def
/daytopmarg 14 def
/dayleftmarg 5 def

% layout constants - don't change these, things probably won't work

/mainrows @WEEKS@ def
/subrows 6 def

% calendar constants - change these if you want a French revolutionary calendar

/days_week 7 def

/days_month \[ 31 28 31 30 31 30 31 31 30 31 30 31 \] def

/isleap {				% is this a leap year?
	year 4 mod 0 eq			% multiple of 4
	year 100 mod 0 ne 		% not century
	year 1000 mod 0 eq or and	% unless it's a millenia
} def

/ndays {				% number of days in this month
	days_month month 1 sub get
	month 2 eq			% February
	isleap and
	{
		1 add
	} if
} def

/weekday {				% weekday \(range 0-6\) for integer date
	days_week mod
} def

/startday {				% starting day-of-week for this month
	/off year 2000 sub def		% offset from start of \"epoch\"
	off
	off 4 idiv add			% number of leap years
	off 100 idiv sub		% number of centuries
	off 1000 idiv add		% number of millenia
	6 add weekday days_week add 	% offset from Jan 1 2000
	/off exch def
	1 1 month 1 sub {
		/idx exch def
		days_month idx 1 sub get
		idx 2 eq
		isleap and
		{
			1 add
		} if
		/off exch off add def
	} for
	off weekday			% 0--Sunday, 1--monday, etc.
} def

/prtevent {		% event-string day prtevent
			%  print out an event
	/start startday def
	/day 2 1 roll def
	day start add 1 sub 7 mod daywidth mul
	day start add 1 sub 7 div truncate dayheight neg mul 
	-5
	numevents day start add get -10 mul add
	numevents
	day start add 
	numevents day start add get 1 add
	put
	add moveto
	show
} def

/drawevents {		% read in a file full of events; print
			%  the events for this month
	/numevents
	\[0 0 0 0 0 0 0
	 0 0 0 0 0 0 0
	 0 0 0 0 0 0 0
	 0 0 0 0 0 0 0
	 0 0 0 0 0 0 0
	 0 0 0 0 0 0 0\] def 
	 eventfont findfont 9 scalefont setfont
	 0 2 holidays length 2 sub { % for the \"Holidays\"
		dup
		1 add holidays 2 1 roll get
		2 1 roll holidays 2 1 roll get
		prtevent
	} for
	 0 2 schedules length 2 sub { % for the \"Schedules\"
		dup
		1 add schedules 2 1 roll get
		2 1 roll schedules 2 1 roll get
		prtevent
	} for
		
} def

% ------------------------------------------------------------------------

/prtnum { 3 string cvs show } def

/center {				% center string in given width
	/width exch def
	/str exch def width str 
	stringwidth pop sub 2 div 0 rmoveto str show
} def

/centernum { exch 3 string cvs exch center } def

/drawgrid {				% draw calendar boxes
	titlefont findfont weekdayfontsize scalefont setfont
	currentpoint /y0 exch def /x0 exch def
	0 1 days_week 1 sub {
		submonth 0 eq
		{
			x0 y0 moveto
			dup dup daywidth mul 40 rmoveto
			day_names exch get
			daywidth center
		} if
		x0 y0 moveto
		daywidth mul topgridmarg rmoveto
		1.0 setlinewidth
		submonth 0 eq
		{
			/rowsused mainrows 1 sub def
		}
		{
			/rowsused subrows 1 sub def
		}
		ifelse
		0 1 rowsused {
			gsave
			daywidth 0 rlineto 
			0 dayheight neg rlineto
			daywidth neg 0 rlineto
			closepath stroke
			grestore
			0 dayheight neg rmoveto
		} for
	} for
} def

/drawnums {				% place day numbers on calendar
	dayfont findfont datefontsize
	submonth 0 ne
	{
		2.5 mul
	} if scalefont setfont
	/start startday def
	/days ndays def
	start daywidth mul dayleftmarg add daytopmarg rmoveto
	submonth 0 ne
	{
		dayleftmarg neg dayheight -2 div rmoveto
	} if
	1 1 days {
		/day exch def
		gsave
		day start add weekday 0 eq
		{
			submonth 0 eq
			{
				.7 setgray
			} if
		} if
		day start add weekday 1 eq
		{
			submonth 0 eq
			{
				.7 setgray
			} if
		} if
		%% Added by ichimal, 2000.2
        submonth 0 eq {
            0 2 holidays length 2 sub {
                holidays 2 1 roll get day eq {
                    .7 setgray
                    exit
                } if
            } for
        } if
		submonth 0 eq
		{
			day prtnum
		}
		{
			day daywidth centernum
		} ifelse
		grestore
		day start add weekday 0 eq
		{
			currentpoint exch pop dayheight sub 0 exch moveto
			submonth 0 eq
			{
				dayleftmarg 0 rmoveto
			} if
		}
		{
			daywidth 0 rmoveto
		} ifelse
	} for
} def

/drawfill {				% place fill squares on calendar
	/start startday def
	/days ndays def
	currentpoint /y0 exch def /x0 exch def
	submonth 0 eq
	{
		usefirst
		{
			/fillstart 2 def
		}
		{
			/fillstart 0 def
		}
		ifelse
	}
	{
		/fillstart 0 def
	}
	ifelse
	fillstart daywidth mul topgridmarg rmoveto
	1.0 setlinewidth
	fillstart 1 start 1 sub {
		gsave
		.9 setgray
		daywidth 0 rlineto 
		0 dayheight neg rlineto
		daywidth neg 0 rlineto
		closepath fill
		grestore
		daywidth 0 rmoveto
	} for
	x0 y0 moveto
	submonth 0 ne
	{
		/lastday subrows days_week mul def
		days_week 1 sub daywidth mul -440 rmoveto
	}
	{
		/lastday mainrows days_week mul 2 sub fillstart add def
		days_week 3 sub fillstart add daywidth mul
		@FOFFSET@ dayheight add rmoveto
	} ifelse
	lastday -1 ndays start 1 add add
	{
		/day exch def
		gsave
		.9 setgray
		daywidth 0 rlineto 
		0 dayheight neg rlineto
		daywidth neg 0 rlineto
		closepath fill
		grestore
		day weekday 1 eq
		{
			submonth 0 ne
			{
				x0 y0 moveto
				days_week 1 sub daywidth mul
				-440 dayheight add rmoveto
			}
			{
				x0 y0 moveto
				days_week 1 sub daywidth mul
				@FOFFSET@ dayheight add rmoveto
			} ifelse
		}
		{
			daywidth neg 0 rmoveto
		} ifelse
	} for
} def

/usefirst {				% are last two boxes used by days?
	start ndays add mainrows days_week mul 3 sub gt
	start 2 ge and
	mainrows 6 eq or
} def

/calendar
{
	titlefont findfont titlefontsize scalefont setfont
	0 60 moveto
	/month_name month_names month 1 sub get def
	month_name show
	/yearstring year 10 string cvs def
	daywidth days_week mul yearstring stringwidth pop sub 60 moveto
	yearstring show

	eventflag {
		% Show a centered Banner if any at the Top
		daywidth days_week mul 2 div
		Bannerstring stringwidth pop 2 div sub
		60 moveto
		Bannerstring show
		% Show footnotes left-center-right
		eventfont findfont footfontsize scalefont setfont
		/bottomrow { dayheight mainrows mul 5 sub neg } def
		0 bottomrow moveto
		Lfootstring show
		daywidth days_week mul Rfootstring stringwidth pop sub
		bottomrow moveto
		Rfootstring show
		daywidth days_week mul Cfootstring stringwidth pop sub 2 div
		bottomrow moveto
		Cfootstring show
		
	} if

	0 -5 moveto
	drawnums

	0 -5 moveto
	drawfill

	eventflag {
		0 0 moveto
		drawevents
	} if

	0 -5 moveto
	drawgrid
} def

/eventflag true def

@SCALE@	scale
@ROTATE@ rotate
@TRANSLATE@ translate
/submonth 0 def
calendar
/eventflag false def
month 1 sub 0 eq
{
	/lmonth 12 def
	/lyear year 1 sub def
}
{
	/lmonth month 1 sub def
	/lyear year def
} ifelse
month 1 add 13 eq
{
	/nmonth 1 def
	/nyear year 1 add def
} 
{
	/nmonth month 1 add def
	/nyear year def
} ifelse
usefirst
{
	0 30 translate
}
{
	days_week 2 sub daywidth mul -350 translate
}
ifelse
/submonth 1 def
/year lyear def
/month lmonth def
gsave
.138 .138 scale
12 -120 translate
calendar
grestore
/submonth 1 def
/year nyear def
/month nmonth def
daywidth 0 translate
gsave
.138 .138 scale
12 -120 translate
calendar
grestore

showpage
")

(defconst mhc-ps/replace-table
      '(("@MONTH@"     . (format "%d" month))
	("@YEAR@"      . (format "%d" year))
	("@TFONT@"     . mhc-ps-title-font)
	("@DFONT@"     . mhc-ps-day-font)
	("@EFONT@"     . mhc-ps-event-font)
	("@JFONT@"     . mhc-ps-japanese-font)
	("@HOLIDAYS@"  . holidays-buffer)
	("@SCHEDULES@" . schedules-buffer)
	("@WEEKS@"     . (number-to-string weeks))
	("@FOFFSET@"   . (if (eq weeks 6) "-535" "-440"))
	("@BANNER@"    . (user-login-name))
	("@LFOOT@"     . "")
	("@RFOOT@"     . "")
	("@CFOOT@"     . "")
	("@SCALE@"     . (cond
			  ((and mhc-ps-paper-type
				(or (not mhc-ps-paper-fill-print)
				    (eq weeks 6)))
			   "0.85 0.85")
			  (mhc-ps-paper-type "1.0 1.0")
			  (t "0.75 0.75")))
	("@ROTATE@"    . (if mhc-ps-paper-type "90" "0"))
	("@TRANSLATE@" . (cond
			  ((and mhc-ps-paper-type
				(or (not mhc-ps-paper-fill-print)
				    (eq weeks 6)))
			   "120 -120")
			  (mhc-ps-paper-type "50 -120")
			  (t "50 900")))))

(defun mhc-ps/weeks (date)
  (if (> (+ (mhc-date-dd (mhc-date-mm-last date))
	    (mhc-date-ww (mhc-date-mm-first date)))
	 35) 6 5))

(defun mhc-ps/substring (str width)
  (let ((clist (mhc-string-to-char-list str))
	cw (i 0) (w 0) (ow 0) (spc ?\ ))
    (catch 'loop
      (while clist
	(setq w (+ w (char-width (car clist))))
	(if (> w width) (throw 'loop nil))
	(setq i (+ i (length (char-to-string (car clist)))))
	(setq clist (cdr clist))))
    (substring str 0 i)))

(defun mhc-ps/compose-subject (time subject margin)
  (let ((mstr (make-string margin ?\ ))
	pos str)
    ;; Delete characters to emphasize subject.
    (and (string-match "^\\*+[ \t\r\f\n]*" subject)
	 (setq pos (match-end 0))
	 (string-match "[ \t\r\f\n]*\\*+$" subject)
	 (setq subject (substring subject pos (match-beginning 0))))
    (if time
	(setq str (concat time " " subject))
      (setq str subject))
    (cond
     ((<= (string-width str) mhc-ps-string-width)
      (list str))
     (mhc-ps-truncate-lines
      (if (null time)
	  (list
	   (if (= (string-width
		   (setq subject (mhc-ps/substring subject mhc-ps-string-width)))
		  mhc-ps-string-width)
	       (concat subject "$")
	     subject))
	(setq subject (concat mstr subject))
	(if (= (string-width
		(setq subject (mhc-ps/substring subject mhc-ps-string-width)))
	       mhc-ps-string-width)
	    (setq subject (concat subject "$")))
	(list time subject)))
     (t
      (with-temp-buffer
	(let ((fill-column mhc-ps-string-width)
	      (left-margin 0)
	      ret)
	  (insert str)
	  (fill-region (point-min) (point-max))
	  (goto-char (point-min))
	  (if (= (forward-line 1) 0)
	      (let ((fill-column (- mhc-ps-string-width margin)))
		(fill-region (point) (point-max))))
	  (delete-region (goto-char (point-max))
			 (progn (skip-chars-backward " \t\n") (point)))
	  (goto-char (point-min))
	  (setq ret (list (buffer-substring
			   (point) (progn (end-of-line) (point)))))
	  (forward-line 1)
	  (while (not (eobp))
	    (setq ret (cons
		       (concat
			mstr
			(buffer-substring
			 (point) (progn (end-of-line) (point))))
		       ret))
	    (forward-line 1))
	  (nreverse ret)))))))

(defun mhc-ps/encode-string (string)
  (let ((start 0) buf ch)
    (while (string-match "[()\\\\]" string start)
      (setq ch (aref string (match-beginning 0))
	    buf (cons (if (eq ch ?\() "\\("
			(if (eq ch ?\)) "\\)"
			  "\\\\"))
		      (cons (substring string start (match-beginning 0)) buf))
	    start (match-end 0)))
    (eval (cons 'concat (nreverse (cons (substring string start) buf))))))


(defun mhc-ps/schedule-to-string (dayinfo schedule)
  (let ((begin (mhc-schedule-time-begin schedule))
	(end (mhc-schedule-time-end schedule))
	(day (mhc-day-day-of-month dayinfo)))
    (if (or begin end)
	(mapconcat (lambda (str)
		     (format "%d ( %s)" day (mhc-ps/encode-string str)))
		   (mhc-ps/compose-subject
		    (concat
		     (if begin (mhc-time-to-string begin) "")
		     (if end (concat "-" (mhc-time-to-string end)) ""))
		    (mhc-schedule-subject-as-string schedule)
		    mhc-ps-left-margin)
		   " ")
      (mapconcat (lambda (str)
		   (format "%d ( %s)" day (mhc-ps/encode-string str)))
		 (mhc-ps/compose-subject
		  nil
		  (mhc-schedule-subject-as-string schedule) 0)
		 " "))))


(defun mhc-ps/make-contents (file year month &optional category category-is-invert)
  (let ((weeks (mhc-ps/weeks (mhc-date-new year month 1)))
	schedules-buffer holidays-buffer)
    (let ((dayinfo-list (mhc-db-scan-month year month)))
      (while dayinfo-list
	(let ((schedules (mhc-day-schedules (car dayinfo-list))))
	  (while schedules
	    (when (or (null category)
		      (if category-is-invert
			  (not (mhc-schedule-in-category-p (car schedules) category))
			(mhc-schedule-in-category-p (car schedules) category)))
	      (if (mhc-schedule-in-category-p (car schedules) "holiday")
		  (setq holidays-buffer
			(cons (mhc-ps/schedule-to-string
			       (car dayinfo-list) (car schedules))
			      holidays-buffer))
		(setq schedules-buffer
		      (cons (mhc-ps/schedule-to-string
			     (car dayinfo-list) (car schedules))
			    schedules-buffer))))
	    (setq schedules (cdr schedules))))
	(setq dayinfo-list (cdr dayinfo-list))))
    (setq schedules-buffer (mapconcat 'identity (nreverse schedules-buffer) " ")
	  holidays-buffer (mapconcat 'identity (nreverse holidays-buffer) " "))
    (with-temp-buffer
      (insert mhc-ps/string)
      (let ((case-fold-search nil)
	    (alist mhc-ps/replace-table)
	    key value)
	(while alist
	  (setq key   (car (car alist))
		value (eval (cdr (car alist)))
		alist (cdr alist))
	  (goto-char (point-min))
	  (while (search-forward key nil t)
	    (delete-region (- (point) (length key)) (point))
	    (insert value))))
      (and file
	  (mhc-write-region-as-coding-system
	   mhc-ps-coding-system (point-min) (point-max) (expand-file-name file)
	   nil 'nomsg))
      (buffer-substring (point-min) (point-max)))))

(defvar mhc-ps/process-file-alist '())

(defun mhc-ps/process (command arguments file buffer year month
			       category category-is-invert)
  (mhc-setup)
  (message "PostScript creating ...")
  (let ((contents
	 (mhc-ps/make-contents file year month category category-is-invert)))
    (if (null contents)
	(message "No PostScript create.")
      (cond
       ((stringp command)
	(let ((process
	       (apply (function start-process)
		      (format "mhc-ps-%s" command)
		      (mhc-get-buffer-create (format " *mhc-ps-%s*" command))
		      command (append arguments (list (expand-file-name file))))))
	  (set-process-coding-system
	   process mhc-ps-coding-system mhc-ps-coding-system)
	  (set-process-sentinel process 'mhc-ps/process-sentinel)
	  (setq mhc-ps/process-file-alist
		(cons (cons process (expand-file-name file)) mhc-ps/process-file-alist))
	  (message "PostScript creating ... done.")))
       ((eq command 'save)
	(message "PostScript saving ... (%s) done." file))
       ((eq command 'buffer)
	(pop-to-buffer (get-buffer-create buffer))
	(cond
	 ((boundp 'buffer-file-coding-system)
	  (setq buffer-file-coding-system mhc-ps-coding-system))
	 ((boundp 'file-coding-system)
	  (setq file-coding-system mhc-ps-coding-system)))
	(save-excursion
	  (insert contents))
	(message "PostScript inserting ... (%s) done." buffer))))))


(defun mhc-ps/process-sentinel (process event)
  (let ((al (assoc process mhc-ps/process-file-alist)))
    (and (cdr al) (file-writable-p (cdr al)) (delete-file (cdr al)))
    (setq mhc-ps/process-file-alist
	  (delete al mhc-ps/process-file-alist))))


;;;###autoload
(defun mhc-ps-preview (year month &optional category category-is-invert)
  "*Preview PostScript calendar."
  (interactive
   (let* ((cdate (or (mhc-current-date) (mhc-calendar-get-date)))
	  (date (mhc-input-month "Month: " cdate))
	  (category (mhc-category-convert mhc-default-category)))
     (list
      (mhc-date-yy date)
      (mhc-date-mm date)
      (cdr category)
      (car category))))
  (mhc-ps/process mhc-ps-preview-command mhc-ps-preview-command-arguments
		  (expand-file-name
		   (format "mhc%04d%02d.ps" year month)
		   (mhc-summary-folder-to-path mhc-base-folder))
		  nil
		  year month
		  category category-is-invert))


;;;###autoload
(defun mhc-ps-print (year month &optional category category-is-invert)
  "*Print PostScript calendar."
  (interactive
   (let* ((cdate (or (mhc-current-date) (mhc-calendar-get-date)))
	  (date (mhc-input-month "Month: " cdate))
	  (category (mhc-category-convert mhc-default-category)))
     (list
      (mhc-date-yy date)
      (mhc-date-mm date)
      (cdr category)
      (car category))))
  (mhc-ps/process mhc-ps-print-command mhc-ps-print-command-arguments
		  (expand-file-name
		   (format "mhc%04d%02d.ps" year month)
		   (mhc-summary-folder-to-path mhc-base-folder))
		  nil
		  year month
		  category category-is-invert))

;;;###autoload
(defun mhc-ps-save (year month file &optional category category-is-invert)
  "*Save PostScript calendar."
  (interactive
   (let* ((cdate (or (mhc-current-date) (mhc-calendar-get-date)))
	  (date (mhc-input-month "Month: " cdate))
	  (category (mhc-category-convert mhc-default-category))
	  (file (read-file-name "Save file: "
				(expand-file-name
				 (mhc-date-format date "mhc%04d%02d.ps"
						  yy mm)
				 (mhc-summary-folder-to-path mhc-base-folder)))))
     (list
      (mhc-date-yy date)
      (mhc-date-mm date)
      file
      (cdr category)
      (car category))))
  (mhc-ps/process 'save nil
		  file nil
		  year month
		  category category-is-invert))


;;;###autoload
(defun mhc-ps-insert-buffer (year month buffer &optional category category-is-invert)
  "*Insert PostScript calendar."
  (interactive
   (let* ((cdate (or (mhc-current-date) (mhc-calendar-get-date)))
	  (date (mhc-input-month "Month: " cdate))
	  (category (mhc-category-convert mhc-default-category))
	  (buffer (read-buffer "Insert buffer: "
			       "*mhc-postscript*")))
     (list
      (mhc-date-yy date)
      (mhc-date-mm date)
      buffer
      (cdr category)
      (car category))))
  (mhc-ps/process 'buffer nil
		  nil buffer
		  year month
		  category category-is-invert))


(provide 'mhc-ps)


;;; Copyright Notice of the PostScript programs.

;; Copyright (C) 1987 by Pipeline Associates, Inc.
;; Copyright (C) 2000 by SUZUKI Shingo <ichimal@takopen.cs.uec.ac.jp>.

;; Permission is granted to modify and distribute this free of charge.


;;; Copyright Notice of the Emacs Lisp programs.

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

;;; mhc-ps.el ends here.
