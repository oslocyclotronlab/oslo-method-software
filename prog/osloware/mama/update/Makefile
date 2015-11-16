#    Makefile for update, modified for opal.nscl.msu.edu by A. Schiller
          CP = mv
      CP_DIR = ../../bin
         F77 = f77
      FFLAGS = -O3 -e
          RM = -rm -f
      RM_CMD = $(RM) *.o core *~
        LIBS = -lX11
   LIB_PATHS = -L/usr/openwin/lib -R

ARCHIVES = upfiles.a graph.a 
EXE = mamaupdate
SRCS = update.f
OBJS = update.o

.f.o:
	$(RM) $@
	$(F77) -c $(FFLAGS) $*.f

mamaupdate: $(OBJS)
	$(F77) $(FFLAGS) -o $(EXE) $(OBJS) $(ARCHIVES) $(LIB_PATHS) $(LIBS)

all:    $(EXE)
	$(CP) $(EXE) $(CP_DIR)
	$(RM_CMD) $(ARCHIVES)

clean: 
	$(RM_CMD)

very-clean:
	$(RM_CMD) $(EXE) $(ARCHIVES)
