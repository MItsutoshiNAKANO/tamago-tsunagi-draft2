# Makefile --- Makefile of EGG V4.0

# Copyright (C) 1999, 2000 Free Software Foundation, Inc
# Author: NIIBE Yutaka <gniibe@chroot.org>
#         TOMURA Satoru <tomura@etl.go.jp>
# Maintaner: Satoru Tomura <tomura@etl.go.jp>

#---------------------------------------
# (1) make
#       ;; compile *.el files
# (2) make install
#       ;; install files into the emacs site-lisp directory
#       ;; ex. /usr/local/share/emacs/site-lisp/egg
#------------------------------------------------
#  Configuration parameters
#------------------------------------------------
# emacs you use
EMACS	= /usr/local/bin/emacs
# Programs
INSTALL	= /usr/sbin/install
CP	= /bin/cp
MV	= /bin/mv
RM	= /bin/rm
MKDIR	= /usr/bin/mkdir
#------------------------------------------------

DEPS	= -l ./docomp.el
BATCHFLAGS	= -batch -q -no-site-file -no-init-file

ETCS	= Makefile docomp.el make-insdirs.el \
	AUTHORS ChangeLog README TODO PROBLEMS

ELS	= eggrc leim-list.el

SRCS	= egg-util.el \
	menudiag.el its.el egg-edep.el \
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

ELCS	= ${SRCS:.el=.elc}

DIST	= ${ETCS} ${SRCS} ${ELS}

.SUFFIXES: .el .elc

.el.elc:
	${EMACS} ${BATCHFLAGS} ${DEPS} -f batch-byte-compile $<

all: ${ELCS} insdirs

insdirs:   ${EMACS}
	@${EMACS} ${BATCHFLAGS} -l ./make-insdirs.el 2> /dev/null

clean: 
	${RM} -f ${ELCS} insdirs *~ */*~

install: install-site

install-site: all
	@. ./insdirs; \
	echo "Egg system will be installed in $${INSDIR}/egg...."; \
	if [ -d $${INSDIR}/egg ]; then \
	  echo "Clean up the previsous installation...."; \
	  ${RM} -rf $${INSDIR}/egg; fi ; \
	mkdir -p $${INSDIR}/egg; \
	tar cf - ${SRCS} ${ELCS} ${ELS} | (cd $${INSDIR}/egg && tar xpBf -)

uninstall-site:
	@. ./insdirs; \
	if [ -d $${INSDIR}/egg ]; then \
	  ${RM} -rf $${INSDIR}/egg; \
	fi

# DEPENDENCIES
egg/sj3rpc.elc: egg-com.elc egg/sj3.elc
egg/wnnrpc.elc: egg-com.elc egg/wnn.elc

egg.elc its/ascii.elc its/erpin.elc its/hankata.elc \
its/hira.elc its/jeonkak.elc its/pinyin.elc \
its/hangul.elc its/kata.elc its/quanjiao.elc \
its/zenkaku.elc its/zhuyin.elc: its-keydef.elc


