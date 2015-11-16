#!/bin/csh -f
#
#       Filenameme:       makefile
#
#       Purpose:        Install the Bit 3 SBus device driver, bts.
#
#       Copyright (c) 1990, 1991 by Bit 3 Computer Corporation.
#       All rights reserved.
#
#       $Revision:   2.1  $
#

#DEBUG_FLAGS=-DDEBUG -g
DEVICE=-DBTS_MODEL=466
DEFNS=-Dsun $(DEVICE)
#CFLAGS= -O2 -I.						# Solaris 1
CFLAGS= -Xa -xO2 -I. -DSOL2 -dn -L/usr/ucblib -lucb		# Solaris 2
LNTFLGS=-auxwPd


HFILE=/usr/include/sys/btio.h
HMMAP=/usr/include/sys/mman.h

EXAMPLES=dumpport dumpram readram readport btcat inthdlr \
	 bt_reset bt_snd_intr bt_xyint 

ALLFILES=$(EXAMPLES)

all:	$(EXAMPLES)

lint:	dumpport.lint dumpram.lint readram.lint readport.lint btcat.lint \
	inthdlr.lint bt_reset.lint 

bit3_perror.c:
	@mv bit3_per.c bit3_perror.c
 
bt_devname.c:
	@mv bt_devna.c bt_devname.c
 
bt_snd_intr.c:
	@mv bt_snd_i.c bt_snd_intr.c
 
bt_xypoll.h:
	@mv bt_xypol.h bt_xypoll.h

clean:
	rm -f *.o
	rm -f $(ALLFILES)

lint_clean:
	rm -f *.ln
	rm -f *.lint 

bit3_perror.o:	bit3_perror.c $(HFILE)
	cc bit3_perror.c $(CFLAGS) $(DEFNS) $(DEBUG_FLAGS) -o bit3_perror.o -c

bt_devname.o:  bt_devname.c $(HFILE)
	cc bt_devname.c $(CFLAGS) $(DEFNS) $(DEBUG_FLAGS) -o bt_devname.o -c

dumpport: dumpport.c bit3_perror.o bt_devname.o $(HFILE) $(HMMAP)
	cc dumpport.c bit3_perror.o bt_devname.o $(CFLAGS) $(DEFNS) $(DEBUG_FLAGS) -o dumpport

dumpram: dumpram.c bit3_perror.o bt_devname.o $(HFILE) $(HMMAP)
	cc dumpram.c bit3_perror.o bt_devname.o $(CFLAGS) $(DEFNS) $(DEBUG_FLAGS) -o dumpram

readram: readram.c bit3_perror.o bt_devname.o $(HFILE)
	cc readram.c bit3_perror.o bt_devname.o $(CFLAGS) $(DEFNS) $(DEBUG_FLAGS) -o readram

readport: readport.c bit3_perror.o bt_devname.o $(HFILE) 
	cc readport.c bit3_perror.o bt_devname.o $(CFLAGS) $(DEFNS) $(DEBUG_FLAGS) -o readport

btcat: btcat.c bit3_perror.o bt_devname.o $(HFILE) 
	cc btcat.c bit3_perror.o bt_devname.o $(CFLAGS) $(DEFNS) $(DEBUG_FLAGS) -o btcat

inthdlr: inthdlr.c bt_devname.o $(HFILE)
	cc inthdlr.c bt_devname.o $(CFLAGS) $(DEFNS) $(DEBUG_FLAGS) -o inthdlr

bt_xyint: bt_xyint.c bit3_perror.o bt_devname.o $(HFILE) bt_xypoll.h
	cc bt_xyint.c bit3_perror.o bt_devname.o $(CFLAGS) $(DEFNS) $(DEBUG_FLAGS) -o bt_xyint

bt_reset: bt_reset.c bt_devname.o $(HFILE)
	cc bt_reset.c bt_devname.o $(CFLAGS) $(DEFNS) $(DEBUG_FLAGS) -o bt_reset

bt_snd_intr: bt_snd_intr.c bt_devname.o $(HFILE)
	cc bt_snd_intr.c bt_devname.o $(CFLAGS) $(DEFNS) $(DEBUG_FLAGS) -o bt_snd_intr

#
#	Lint Checks
#
bt_devname.ln:	bt_devname.lint

bit3_perror.ln:	bit3_perror.lint

bit3_perror.lint: bit3_perror.c $(HFILE)
	lint -c $(LNTFLGS) $(DEFNS) bit3_perror.c >bit3_perror.lint
	@echo "____________________________________________" >>bit3_perror.lint
	lint -c $(LNTFLGS) $(DEFNS) $(DEBUG_FLAGS) bit3_perror.c >>bit3_perror.lint

bt_devname.lint: bt_devname.c $(HFILE) 
	lint -c $(LNTFLGS) $(DEFNS) bt_devname.c >bt_devname.lint
	@echo "____________________________________________" >>bt_devname.lint
	lint -c $(LNTFLGS) $(DEFNS) $(DEBUG_FLAGS) bt_devname.c >>bt_devname.lint

dumpport.lint:	dumpport.c $(HFILE) $(HMMAP) bit3_perror.ln mmap.ln bt_devname.ln
	lint  $(LNTFLGS) $(DEFNS) dumpport.c  bt_devname.ln bit3_perror.ln mmap.ln >dumpport.lint
	@echo "____________________________________________" >>dumpport.lint
	lint  $(LNTFLGS) $(DEFNS) $(DEBUG_FLAGS) dumpport.c bt_devname.ln bit3_perror.ln mmap.ln >>dumpport.lint

dumpram.lint:	dumpram.c $(HFILE) $(HMMAP) bit3_perror.ln bt_devname.ln mmap.ln
	lint  $(LNTFLGS) $(DEFNS) dumpram.c bit3_perror.ln bt_devname.ln mmap.ln >dumpram.lint
	@echo "____________________________________________" >>dumpram.lint
	lint  $(LNTFLGS) $(DEFNS) $(DEBUG_FLAGS) dumpram.c bit3_perror.ln bt_devname.ln mmap.ln >>dumpram.lint

readport.lint:	readport.c $(HFILE) bit3_perror.ln bt_devname.ln
	lint $(LNTFLGS) $(DEFNS) readport.c bit3_perror.ln bt_devname.ln >readport.lint
	@echo "____________________________________________" >>readport.lint
	lint $(LNTFLGS) $(DEFNS) $(DEBUG_FLAGS) readport.c bit3_perror.ln bt_devname.ln >>readport.lint

readram.lint:	readram.c $(HFILE) bit3_perror.ln bt_devname.ln
	lint $(LNTFLGS) $(DEFNS) readram.c bit3_perror.ln bt_devname.ln >readram.lint
	@echo "____________________________________________" >>readram.lint
	lint $(LNTFLGS) $(DEFNS) $(DEBUG_FLAGS) readram.c bit3_perror.ln bt_devname.ln >>readram.lint

btcat.lint:	btcat.c $(HFILE) bit3_perror.ln bt_devname.ln
	lint $(LNTFLGS) $(DEFNS) btcat.c bit3_perror.ln bt_devname.ln >btcat.lint
	@echo "____________________________________________" >>btcat.lint
	lint $(LNTFLGS) $(DEFNS) $(DEBUG_FLAGS) btcat.c bit3_perror.ln bt_devname.ln >>btcat.lint

inthdlr.lint:	inthdlr.c $(HFILE) bt_devname.ln
	lint $(LNTFLGS) $(DEFNS) inthdlr.c bt_devname.ln >inthdlr.lint
	@echo "____________________________________________" >>inthdlr.lint
	lint $(LNTFLGS) $(DEFNS) $(DEBUG_FLAGS) inthdlr.c bt_devname.ln >>inthdlr.lint

bt_reset.lint:	bt_reset.c $(HFILE) bit3_perror.ln bt_devname.ln
	lint $(LNTFLGS) $(DEFNS) bt_reset.c bit3_perror.ln bt_devname.ln >bt_reset.lint
	@echo "____________________________________________" >>bt_reset.lint
	lint $(LNTFLGS) $(DEFNS) $(DEBUG_FLAGS) bt_reset.c bit3_perror.ln bt_devname.ln >>bt_reset.lint

