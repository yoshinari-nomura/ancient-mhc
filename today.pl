#!/usr/local/bin/perl

### today
##
## Author:  Yoshinari Nomura <nom@mew.org>
##          OHARA Shigeki <os@iij.ad.jp>
##
## Created: 1999/07/16
## Revised: 2000/02/09 08:43:13
##

$IMPUT  = 'imput';
$MSCAN = 'mscan';
$PERL   = 'perl';    ## for only WIN95/98.
#$KCONV  = 'nkf -s';  ## convert to the code your *terminal* supports.
#$USER   = 'user';

$WIN = 1 if ($^O =~ /win/i);
$IMPUT  = "$PERL -S $IMPUT" if ($WIN);

($sec, $min, $hour, $day, $mon, $year) = localtime(time);
$year += 1900; $mon++;

foreach (@ARGV) {
    $tilltomorrow = 1 if (/^--tilltomorrow$/);
    $address = $1 if (/^--mail=(.+)/);
}

$USER = $USER ||
    $ENV{'USERNAME'} || $ENV{'USER'} || $ENV{'LOGNAME'} || $address;

if ($WIN) {
    $schedule  = `$PERL -S $MSCAN --date=today`;
    $schedule2 = `$PERL -S $MSCAN --date=tomorrow` if ($tilltomorrow);
} else {
    $schedule  = `$MSCAN --date=today`;
    $schedule2 = `$MSCAN --date=tomorrow` if ($tilltomorrow);
}

exit(0) if (($schedule eq '') && ($schedule2 eq ''));

if ($address) {
    open(STDOUT, "| $IMPUT $address");
    select STDOUT; $| = 1;

    print "To: $address\n";
    print "From: secretary-of-$address\n";
    print "Subject: Today's schedule ($year-$mon-$day).\n";
    print "\n";
    print "${USER}'s schedule ($year-$mon-$day):\n\n";
} else {
    open(STDOUT, "| $KCONV") if ($KCONV);
}

print $schedule if ($schedule);

if ($schedule2) {
    ($schedule) ? print "\n" : print "Tomorrow ... :\n\n";
    print $schedule2;
}

close(STDOUT);

exit 0;

### Copyright Notice:

## Copyright (C) 1999, 2000 Yoshinari Nomura.
## All rights reserved.

## Redistribution and use in source and binary forms, with or without
## modification, are permitted provided that the following conditions
## are met:
## 
## 1. Redistributions of source code must retain the above copyright
##    notice, this list of conditions and the following disclaimer.
## 2. Redistributions in binary form must reproduce the above copyright
##    notice, this list of conditions and the following disclaimer in the
##    documentation and/or other materials provided with the distribution.
## 3. Neither the name of the team nor the names of its contributors
##    may be used to endorse or promote products derived from this software
##    without specific prior written permission.
## 
## THIS SOFTWARE IS PROVIDED BY Yoshinari Nomura AND CONTRIBUTORS ``AS IS''
## AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
## LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
## FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL
## Yoshinari Nomura OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
## INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
## (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
## SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
## HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
## STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
## ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
## OF THE POSSIBILITY OF SUCH DAMAGE.

### today ends here
