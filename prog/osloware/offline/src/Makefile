# Makefile for offline, modified for opal.nscl.msu.edu by A. Schiller
    FC = f77
FFLAGS = -O2 -cg92 -e -C
    CC = cc
CFLAGS = -Xa -xO2 -xcg92 -I/user/schiller/osloware/include -I/usr/openwin/share/include -DSOL2 -L/usr/ucblib -L/usr/openwin/lib -lXm -lXt -lX11 
    CP = mv
CP_BIN = $(CP) offline_sorter offline_master offline_specclear offline_specdump /user/schiller/osloware/bin/.
    RM = rm -f
RM_CMD = $(RM) *.o core *~ 
EXE = sort \
      dump \
      clear \
      gui \
      install \
      clean
SSRCS = attshared.c \
        attspec.c \
        detshared.c \
        user_routine.f \
        readwrite.f \
        offline_format.f \
        read_exa.c \
        read_daisy.c \
        offline_main.f
SOBJS = attshared.o \
        attspec.o \
        detshared.o \
        user_routine.o \
        readwrite.o \
        offline_format.o \
        read_exa.o \
        read_daisy.o \
        offline_main.o
DSRCS = specdump.f
DOBJS = specdump.o \
        readwrite.o \
        attspec.o \
        attshared.o \
        detshared.o
CSRCS = specclear.f
COBJS = specclear.o \
        attspec.o \
        attshared.o \
        detshared.o
GSRCS = offline_gui.c \
        offline_message.c \
        offline_storage.c \
        offline_start.c \
        offline_stop.c \
        offline_status.c \
	offline_clear.c \
        offline_dump.c \
        offline_position.c \
        offline_unload.c \
        offline_init.c
GOBJS = offline_gui.o \
        offline_message.o \
        offline_storage.o \
        offline_start.o \
        offline_stop.o \
        offline_status.o \
	offline_clear.o \
        offline_dump.o \
        offline_position.o \
        offline_unload.o \
        offline_init.o

.f.o:
	$(RM) $@
	$(FC) -c $(FFLAGS) $*.f
.c.o:
	$(RM) $@
	$(CC) -c $(CFLAGS) $*.c

sort:   $(SOBJS)
	$(FC) $(FFLAGS) $(SOBJS) -o offline_sorter

dump:   $(DOBJS)
	$(FC) $(FFLAGS) $(DOBJS) -o offline_specdump

clear:  $(COBJS)
	$(FC) $(FFLAGS) $(COBJS) -o offline_specclear

gui:    $(GOBJS)
	$(CC) $(CFLAGS) $(GOBJS) -o offline_master

install: 
	$(CP_BIN)

all::   $(EXE)

clean: 
	$(RM_CMD)

