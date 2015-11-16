# Makefile for graph directory
          AR = ar rv
          CC = cc
      CFLAGS = -xO2 -I/usr/openwin/include
          CP = mv
      CP_DIR = ../..
         F77 = f77
      FFLAGS =  -O2 -e -C 
          RM = rm -f
      RM_CMD = $(RM) *.o core *~

ARCHIVE = graph.a
SRCS = cvxy.f \
       erase.f \
       getcur.f \
       grax.f \
       initg.f \
       ktras.f \
       limg.f \
       lineup.f \
       minig_x_c.c \
       mspot.f \
       pspot.f \
       putg.f \
       qio_put_text.f \
       retic.f \
       symbg.f \
       trax.f \
       txtclr.f \
       txtmod.f

OBJS = cvxy.o \
       erase.o \
       getcur.o \
       grax.o \
       initg.o \
       ktras.o \
       limg.o \
       lineup.o \
       minig_x_c.o \
       mspot.o \
       pspot.o \
       putg.o \
       qio_put_text.o \
       retic.o \
       symbg.o \
       trax.o \
       txtclr.o \
       txtmod.o

.c.o:
	$(RM) $@
	$(CC) -c $(CFLAGS) $*.c
.f.o:
	$(RM) $@
	$(F77) -c $(FFLAGS) $*.f


all:: $(ARCHIVE)
	$(RM_CMD) $(ARCHIVE)

$(ARCHIVE): $(OBJS) minig_x_c.o
	$(AR) $@ $(OBJS)
	$(CP) $(ARCHIVE) $(CP_DIR)

clean: 
	$(RM_CMD)

very-clean:
	$(RM_CMD) $(ARCHIVE)
