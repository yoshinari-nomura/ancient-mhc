# -*- Makefile -*-

### Common Macros
EMACS = emacs
OPT   = -batch -q -no-site-file \
        -eval '(setq load-path (cons "'`pwd`'" load-path))'
SRC   = mhc.el mhc-date.el mhc-face.el mhc-guess.el \
        mhc-minibuf.el mhc-misc.el mhc-schedule.el
ELC   = $(SRC:.el=.elc)

## 1. set MEW_DIR and/or WL_DIR and/or GNUS_DIR
## 2. set MUA_OPT
## make

### For Mew
MEW_DIR = /u/nom/lib/elisp/mew/current
MEW_OPT = -eval '(setq load-path (cons "$(MEW_DIR)" load-path))'   -l mew

### For Wanderlust
WL_DIR =
WL_OPT = -eval '(setq load-path (cons "$(WL_DIR)" load-path))'     -l wl

### For Gnus
GNUS_DIR =
GNUS_OPT = -eval '(setq load-path (cons "$(GNUS_DIR)" load-path))' -l gnus

MUA_OPT  = ${MEW_OPT}
#MUA_OPT  = ${WL_OPT}
#MUA_OPT  = ${GNUS_OPT}
#MUA_OPT  = ${MEW_OPT} ${WL_OPT} ${GNUS_OPT}

### Suffix rules
.SUFFIXES: .elc .el

.el.elc:
	-rm -f $<c
	$(EMACS) $(OPT) -l mhc.el ${MUA_OPT} -f batch-byte-compile $<

### For main program
default: $(ELC)

clean:
	rm -f *~ *.elc
