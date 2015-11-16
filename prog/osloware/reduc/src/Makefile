# Makefile for reduc, modified for opal.nscl.msu.edu by A. Schiller
    FC = f77
FFLAGS = -O3 -cg92 -e
    CC = cc
CFLAGS = -Xa -xO3 -xcg92 -I/user/schiller/osloware/include -I/usr/openwin/share/include -DSOL2 -L/usr/ucblib -L/usr/openwin/lib -lXm -lXt -lX11
    CP = mv
CP_BIN = $(CP) reduc_sorter reduc_master /user/schiller/osloware/bin/.
    RM = rm -f
RM_CMD = $(RM) *.o core *~ 

EXE = sort \
      gui \
      install \
      clean
SSRCS = attshared.c \
        detshared.c \
        read_daisy.c \
        read_exa.c \
        readwrite.f \
        reduc_gaincalc.f \
        reduc_gainformat.f \
        reduc_gainsort.f \
        reduc_main.f \
        reduc_winformat.f \
        reduc_winsort.f \
        write_exa.c 
SOBJS = attshared.o \
        detshared.o \
        read_daisy.o \
        read_exa.o \
        readwrite.o \
        reduc_gaincalc.o \
        reduc_gainformat.o \
        reduc_gainsort.o \
        reduc_main.o \
        reduc_winformat.o \
        reduc_winsort.o \
        write_exa.o 
GSRCS = reduc_gainfac.c \
        reduc_gui.c \
        reduc_init.c \
        reduc_message.c \
        reduc_position.c \
        reduc_startgain.c \
        reduc_startwin.c \
        reduc_status.c \
        reduc_stop.c \
        reduc_storage.c \
        reduc_unload.c 
GOBJS = reduc_gainfac.o \
        reduc_gui.o \
        reduc_init.o \
        reduc_message.o \
        reduc_position.o \
        reduc_startgain.o \
        reduc_startwin.o \
        reduc_status.o \
        reduc_stop.o \
        reduc_storage.o \
        reduc_unload.o 

.f.o:
	$(RM) $@
	$(FC) -c $(FFLAGS) $*.f
.c.o:
	$(RM) $@
	$(CC) -c $(CFLAGS) $*.c

sort:   $(SOBJS)
	$(FC) $(FFLAGS) $(SOBJS) -o reduc_sorter

gui:    $(GOBJS)
	$(CC) $(CFLAGS) $(GOBJS) -o reduc_master

install: 
	$(CP_BIN)

all::   $(EXE)

clean: 
	$(RM_CMD)

