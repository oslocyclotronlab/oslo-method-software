# Makefile for milib directory
          AR = ar rv
          CP = mv
      CP_DIR = ../..
         F77 = f77
      FFLAGS = -O3 -C -e 
          RM = rm -f
      RM_CMD = $(RM) *.o core *~

ARCHIVE = milib.a
SRCS = date.f \
       get_decf77err.f \
       ifio.f \
       ioferr.f \
       lext.f \
       locid.f \
       lodup.f \
       messlog.f \
       spkerr.f \
       spkio.f \
       spkman.f \
       squezl.f

OBJS = date.o \
       get_decf77err.o \
       ifio.o \
       ioferr.o \
       lext.o \
       locid.o \
       lodup.o \
       messlog.o \
       spkerr.o \
       spkio.o \
       spkman.o \
       squezl.o

.f.o:
	$(RM) $@
	$(F77) -c $(FFLAGS) $*.f

all:: $(ARCHIVE) 
	$(RM_CMD) $(ARCHIVE)

$(ARCHIVE): $(OBJS)
	$(RM) $@
	$(AR) $@ $(OBJS)
	$(CP) $(ARCHIVE) $(CP_DIR)

clean:
	$(RM_CMD)

very-clean:
	$(RM_CMD) $(ARCHIVE)
