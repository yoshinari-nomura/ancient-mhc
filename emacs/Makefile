# -*- Makefile -*-

### Common Macros
EMACS = emacs
OPT   = -batch -q -no-site-file -eval '(setq load-path (cons "'`pwd`'" load-path))'
SRC   = mhc.el mhc-compat.el mhc-cvs.el mhc-date.el mhc-day.el mhc-db.el mhc-face.el \
	mhc-file.el mhc-guess.el mhc-header.el mhc-logic.el mhc-minibuf.el mhc-misc.el \
	mhc-parse.el mhc-record.el mhc-schedule.el mhc-slot.el mhc-summary.el mhc-vars.el
ELC   = $(SRC:.el=.elc)

### For Mew
MEW_DIR =
MEW_OPT = -eval '(setq load-path (cons "$(MEW_DIR)" load-path))'
MEW_SRC = mhc-mew.el
MEW_ELC = $(MEW_SRC:.el=.elc)

### For Wanderlust
WL_DIR =
WL_OPT = -eval '(setq load-path (cons "$(WL_DIR)" load-path))'
WL_SRC = mhc-wl.el
WL_ELC = $(WL_SRC:.el=.elc)

### For Gnus
GNUS_DIR =
GNUS_OPT = -eval '(setq load-path (cons "$(GNUS_DIRECTORY)" load-path))'
GNUS_SRC = mhc-gnus.el nnmhc.el
GNUS_ELC = $(GNUS_SRC:.el=.elc)


### Suffix rules
.SUFFIXES: .elc .el

.el.elc:
	-rm -f $<c
	$(EMACS) $(OPT) -l mhc.el -f batch-byte-compile $<


### For main program
default: $(ELC)

mhc-record.elc: mhc-record.el
	rm -f $@
	$(EMACS) $(OPT) -l mhc.el -l mhc-file.el -f batch-byte-compile $?


### For mew
mew: $(MEW_ELC)

mhc-mew.elc: mhc-mew.el
	rm -f $@
	$(EMACS) $(OPT) $(MEW_OPT) -l mhc.el -f batch-byte-compile $?


### For Wanderlust
wl: $(WL_ELC) mew

mhc-wl.elc: mhc-wl.el
	rm -f $@
	$(EMACS) $(OPT) $(WL_OPT) -l mhc.el -f batch-byte-compile $?


### For Gnus
gnus: $(GNUS_ELC)

mhc-gnus.elc: mhc-gnus.el
	rm -f $@
	$(EMACS) $(OPT) $(GNUS_OPT) -l mhc.el -f batch-byte-compile $?

nnmhc.elc: nnmhc.el
	rm -f $@
	$(EMACS) $(OPT) $(GNUS_OPT) -l mhc.el -f batch-byte-compile $?


all: default mew wl gnus

clean:
	rm -f *~ *.elc
