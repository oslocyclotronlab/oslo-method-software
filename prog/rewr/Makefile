# Makefile for rewr directory
          AR = ar cru
          CC = cc
      CFLAGS = -O3
         F95 = gfortran
      FFLAGS = -W -Wall -ffixed-line-length-132 -std=legacy
      RANLIB = ranlib
          RM = rm -f

ARCHIVE = rewr.a
SRCS =  rewr.f
OBJS =  rewr.o

.f.o:
	$(F95) $(FFLAGS) -c $*.f
.c.o:
	$(CC)  $(CFLAGS) -c $*.c

all:: $(ARCHIVE)

$(ARCHIVE):  $(OBJS) 
	$(AR) $@ $(OBJS)
	$(RANLIB) $@

clean: 
	$(RM) $(OBJS) core *~

very-clean: clean
	$(RM) $(ARCHIVE)

