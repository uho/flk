#  FLK Makefile
#
#  Copyright (C) 1998 Lars Krueger 
#
#  This file is part of FLK.
#
#  FLK is free software; you can redistribute it and/or
#  modify it under the terms of the GNU General Public License
#  as published by the Free Software Foundation; either version 2
#  of the License, or (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

# $Id: Makefile,v 1.34 1998/09/21 11:35:31 root Exp root $
# $Log: Makefile,v $
# Revision 1.34  1998/09/21 11:35:31  root
# added xinstall target
#
# Revision 1.33  1998/09/21 11:31:10  root
# fixed dist target (no srcdir.fs bug )
#
# Revision 1.32  1998/09/21 11:28:37  root
# removed xflk from install target
#
# Revision 1.31  1998/09/13 16:19:39  root
# moved to version 1.3
#
# Revision 1.30  1998/09/13 15:42:04  root
# introduced separate control flow stack
#
# Revision 1.29  1998/08/30 10:50:59  root
# new optimizing algorithm
#
# Revision 1.28  1998/07/18 10:49:59  root
# added flkkhiex.fs
#
# Revision 1.27  1998/07/13 18:08:54  root
# added flktopt.fs
#
# Revision 1.26  1998/07/03 20:57:50  root
# switched to version 1.2
#
# Revision 1.25  1998/06/08 22:18:09  root
# flklevel2.fs added
#
# Revision 1.24  1998/06/08 22:14:51  root
# literals cache (preparation to level 2 optimizer)
#
# Revision 1.23  1998/06/03 17:32:19  root
# removed srcdir.fs before tarring in target dist
#
# Revision 1.22  1998/06/03 17:28:13  root
# linuxsc.fs added
#
# Revision 1.21  1998/06/01 18:05:39  root
# changed to version 1.1
# added BIN_DIR define
#
# Revision 1.20  1998/06/01 17:51:42  root
# targets clean, srcdir.fs added
# Delayed flushing turned off when debugging.
#
# Revision 1.19  1998/05/24 18:43:16  root
# delayed flush corrected
#
# Revision 1.18  1998/05/24 15:41:26  root
# Delayed flush can be turned off
#
# Revision 1.17  1998/05/23 17:52:02  root
# X-Windows files added, xflk rule
#
# Revision 1.16  1998/05/21 19:24:49  root
# XForms support
#
# Revision 1.15  1998/05/17 08:27:09  root
# script mode, ODOES>
#
# Revision 1.14  1998/05/09 21:47:05  root
# S, renamed to ,C
# ,S included
#
# Revision 1.13  1998/05/04 06:13:46  root
# Version logging when creating distribution archives
#
# Revision 1.12  1998/05/02 15:08:05  root
# f added to check-in list
#
# Revision 1.11  1998/05/02 15:05:55  root
# added f to distribution file list
#
# Revision 1.10  1998/05/02 14:27:58  root
# changed to dynamic libraries
#
# Revision 1.9  1998/05/01 18:11:25  root
# GNU license text added
# comments checked
#
# Revision 1.8  1998/04/30 09:42:25  root
# installation support
#
# Revision 1.7  1998/04/25 11:03:28  root
# added flk.c to check in list
#
# Revision 1.6  1998/04/24 20:25:00  root
# added flktools.fs to source list
#
# Revision 1.4  1998/04/17 06:23:29  root
# target all added
#
# Revision 1.3  1998/04/11 11:58:50  root
# extend.fs added
#
# Revision 1.2  1998/04/10 14:42:50  root
# added target extension
#
# Revision 1.1  1998/04/07 20:16:22  root
# Initial revision
# 

# a 32-bit Forth system
FORTH32=vfxlin
#FORTH32=gforth -e

# Change to match your system.
INSTALL_LIBDIR=/usr/local/lib/flk
INSTALL_BINDIR=/usr/local/bin

# If DEBUGGING is not true, loader messages are suppressed and the loader is
# optimized.
DEBUGGING=false
#DEBUGGING=true

# If DELAY_FLUSH is true, the experimental terminal flushing code is compiled.
DELAY_FLUSH=true

# If MDEBUG is true, the malloc-debugging library efence is linked.
MDEBUG=false

# Don't modify anything below this line. ---------------------------------------
ifeq (${DEBUGGING},true)
DELAY_FLUSH=false
else
MDEBUG=false
endif

CFLAGS=-m32 -Wall -pipe -D__UNIX__ -D__LINUX__ \
       -DINSTALL_DIR=\"${INSTALL_LIBDIR}/\" \
       -DINSTALL_BIN_DIR=\"${INSTALL_BINDIR}/\" 

ifeq (${DEBUGGING},true)
CFLAGS+= -g
else
CFLAGS+= -DNDEBUG -O2 
endif

ifeq (${DELAY_FLUSH},true)
CFLAGS+=-DDELAY_FLUSH
endif

INCLUDE=-I.
LDFLAGS=-m32 -L/usr/lib -lncurses -rdynamic -ldl

ifeq (${MDEBUG},true)
LDFLAGS+=-lefence
endif

# Version setting
VERSION=1.3
SRCS_C=flk.c

SRCDIRCMD=': SRCDIR S"' `pwd`'/" ; '

OBJECTS=${SRCS_C:%.c=%.o}

SRCS=flkasm.fs flkdict.fs flkenv.fs flkexcep.fs flkhcomp.fs flkhigh.fs \
     flkinput.fs flkio.fs flkkern.fs flkkey_unix.fs flkmeta.fs \
     flkprim.fs flksys.fs flktcomp.fs flkstring.fs extend.fs flktools.fs \
     flkfloat.fs array.fs defer.fs flklib.fs flklevel2.fs flkopt.fs \
     flktopt.fs flkhiex.fs flkcfstack.fs

RCSfiles=${SRCS} Makefile ${SRCS_C} f flkXforms.fs flkcontrol.fs 

all: flk

flk default.flk: flkkern flk.flk ${SRCS}
	cp flkkern flk
	./flkkern extend.fs

xflk: flk flkXforms.fs flkcontrol.fs 
	./flk flkXforms.fs flkcontrol.fs 

flkkern: ${OBJECTS}
	${LINK.o} $^ -o $@

flk.flk: ${SRCS} srcdir.fs linuxsc.fs
	$(FORTH32) 's" f.fs" included'

srcdir.fs: 
	echo ${SRCDIRCMD} > srcdir.fs

clean: 
	rm srcdir.fs flk.flk default.flk flk xflk flkkern ${OBJECTS}

depend: 
	rm .depends
	make .depends

.depends:
	${CC} -M ${CFLAGS} ${INCLUDE} ${SRCS_C} > .depends

ci: 
	ci -l ${RCSfiles}

install: default.flk flk 
	install -d ${INSTALL_LIBDIR}
	cp default.flk ${INSTALL_LIBDIR}
	install -d ${INSTALL_BINDIR}
	cp flk ${INSTALL_BINDIR}

xinstall: install xflk
	cp xflk ${INSTALL_BINDIR}

dist:	ci
	-rm srcdir.fs
	( cd .. ; tar cvfz flk.${VERSION}.tar.gz flk/*.fs flk/flk.c flk/Makefile \
        flk/html/*.html flk/html/flk.gif flk/README flk/COPYING \
        flk/makefile.wat flk/f flk/mksc flk/linuxsc.fs)
	echo Version ${VERSION} >version.${VERSION}.log
	rlog -h ${RCSfiles} >>version.${VERSION}.log

include .depends
