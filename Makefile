# Makefile --- Makefile of EGG V4.0

# Copyright (C) 1999, 2000 Free Software Foundation, Inc
# Author: NIIBE Yutaka <gniibe@chroot.org>
#	  TOMURA Satoru <tomura@etl.go.jp>
# Maintaner: Satoru Tomura <tomura@etl.go.jp>

#---------------------------------------
# (1) make
#	;; compile *.el files and make leim-list.el
# (2) make install-system
#	;; install files into ${INSDIR} and ${LEIMDIR}
# (3) make install-user
#	;; install your initialization files at your home directory
#------------------------------------------------
#  Configuration parameters
#------------------------------------------------
# Emacs initialization file at your home directory
	DOTEMACS= ${HOME}/.emacs
# egg initialization file at your home directory
	STARTUP	= .eggrc
	EGGRC	= ${HOME}/${START}
# emacs you use
	EMACS	= /usr/local/bin/emacs
# Egg does not depend on the emacs version (We hope...)
	INSDIR	= /usr/local/share/emacs/site-lisp/egg
# The directory where there is leim-list.el in it.
	LEIMDIR	= /usr/local/share/emacs/20.4/leim
# Programs
	INSTALL	= /usr/sbin/install
	CP	= /bin/cp
	MV	= /bin/mv
	RM	= /bin/rm
	MKDIR	= /usr/bin/mkdir
#------------------------------------------------


DEPS = -l ./docomp.el
BATCHFLAGS = -batch -q -no-site-file

.SUFFIXES: .el .elc

ETCS =	Makefile docomp.el eggrc leim-list-egg.el egg-dotemacs \
	AUTHORS ChangeLog README TODO PROBLEMS

SRCS = menudiag.el its.el egg-edep.el \
       its/ascii.el \
       its/bixing.el \
       its/erpin.el \
       its/hankata.el \
       its/hira.el \
       its/jeonkak.el \
       its/pinyin.el \
       its/hangul.el \
       its/kata.el \
       its/thai.el \
       its/quanjiao.el \
       its/zenkaku.el \
       its/zhuyin.el \
       its-keydef.el \
       egg-mlh.el egg-cnv.el egg-com.el \
       egg.el \
       egg/cannarpc.el egg/canna.el \
       egg/sj3rpc.el egg/sj3.el \
       egg/wnnrpc.el egg/wnn.el 

ELCS = ${SRCS:.el=.elc}

DIST = ${ETCS} ${SRCS}

.el.elc:
	${EMACS} ${BATCHFLAGS} ${DEPS} -f batch-byte-compile $<

all: ${ELCS} leim-list.el

leim-list.el: leim-list-egg.el
	@if (grep ";;; leim-list-egg.el" \
	          ${LEIMDIR}/leim-list.el 2>&1) >/dev/null; then \
	  echo Egg setup already exists in ${LEIMDIR}/leim-list.el; \
	  cat ${LEIMDIR}/leim-list.el  >leim-list.el; \
	else \
	  cat ${LEIMDIR}/leim-list.el leim-list-egg.el >leim-list.el; \
	fi

clean: 
	${RM} -f ${ELCS} leim-list.el

install: install-system

install-system: all
	if [ ! -d ${INSDIR} ]; then mkdir -p ${INSDIR}; fi
	tar cf - ${SRCS} ${ELCS} | (cd ${INSDIR} && tar xvf -)
	${CP} leim-list.el ${INSDIR}

uninstall-system:
	if [ -d ${INSDIR} ]; then \
	  ${RM} -rf ${INSDIR}; \
	fi


install-user: dotemacs ${EGGRC}

dotemacs: egg-dotemacs
	@if (grep "^;;; Emacs/Egg Configuration" \
                  $(DOTEMACS) 2>&1) >/dev/null; then \
          echo Emacs/Egg setup already exists in $(DOTEMACS); \
	else \
	  cat egg-dotemacs >> ${DOTEMACS} ; \
	  echo "(setq egg-startup-file \"${STARTUP}\")" >>${DOTEMACS} ; \
	  echo "Added Emacs/Egg setup to $(DOTEMACS)"; \
	fi

${EGGRC}: eggrc
	$(CP) eggrc ${EGGRC}

# DEPENDENCIES
egg/sj3rpc.elc: egg-com.elc egg/sj3.elc
egg/wnnrpc.elc: egg-com.elc egg/wnn.elc

egg.elc its/ascii.elc its/erpin.elc its/hankata.elc \
       its/hira.elc its/jeonkak.elc its/pinyin.elc \
       its/hangul.elc its/kata.elc its/quanjiao.elc \
       its/zenkaku.elc its/zhuyin.elc: its-keydef.elc

distclean:
	rm -f ${ELCS} leim-list.el *~

###  Source code maintainance
DATE=$(shell date "+%y%m%d")

dist: distclean
	rm -rf ../egg-${DATE}
	mkdir ../egg-${DATE}
	tar -c -f - ${DIST} | tar Cxf ../egg-${DATE} -
	(cd ../egg-${DATE}; \
	 sed "/^### Source code maintainance/,\$$d" <Makefile >Makefile.dist; \
	 mv -f Makefile.dist Makefile)
	(cd ..; tar cvzf egg-${DATE}.tar.gz egg-${DATE})

working-ss: distclean
	rm -rf ../egg-snap-${DATE}
	mkdir ../egg-snap-${DATE}
	tar -c -f - . | tar Cxf ../egg-snap-${DATE} -
	(cd ..; tar cvzf egg-snap-${DATE}.tar.gz egg-snap-${DATE})
