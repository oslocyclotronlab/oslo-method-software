# Makefile for spyfiles directory
          AR = ar rv
          CC = cc
      CFLAGS = -Xa -xO2 -I/user/schiller/osloware/include -DSOL2 -L/usr/ucblib -R/usr/ucblib -lucb 
          CP = mv
      CP_DIR = ../..
         F77 = f77
      FFLAGS = -O2 -e -C
          RM = rm -f
      RM_CMD = $(RM) *.o core *~


ARCHIVE = spyfiles.a
SRCS = attshared2mama.f \
       attspec.c \
       datetime.f \
       detshared.c \
       loop.f \
       offspec.c \
       outlaySPY.f

OBJS = attshared2mama.o \
       attspec.o \
       datetime.o \
       detshared.o \
       loop.o \
       offspec.o \
       outlaySPY.o 

.f.o:
	$(RM) $@
	$(F77) -c $(FFLAGS) $*.f
.c.o:
	$(RM) $@
	$(CC) -c $(CFLAGS) $*.c

all:: $(ARCHIVE)
	$(RM_CMD) $(ARCHIVE)

$(ARCHIVE): $(OBJS)
	$(AR) $@ $(OBJS)
	$(CP) $(ARCHIVE) $(CP_DIR)

clean: 
	$(RM_CMD)

very-clean:
	$(RM_CMD) $(ARCHIVE)

