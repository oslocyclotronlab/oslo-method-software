#    Makefile for spy
          CP = mv
      CP_DIR = ../../bin
         F77 = f77
      FFLAGS = -O3 -e
          RM = -rm -f
      RM_CMD = $(RM) *.o core *~
        LIBS = -lX11
   LIB_PATHS = -L/usr/openwin/lib -R

ARCHIVES = spyfiles.a graph.a 
EXE = spy
SRCS = spy.f
OBJS = spy.o

.f.o:
	$(RM) $@
	$(F77) -c $(FFLAGS) $*.f

spy:    $(OBJS)
	$(F77) $(FFLAGS) -o $(EXE) $(OBJS) $(ARCHIVES) $(LIB_PATHS) $(LIBS)

all:    $(EXE)
	$(CP) $(EXE) $(CP_DIR)
	$(RM_CMD) $(ARCHIVES)

clean: 
	$(RM_CMD)

very-clean:
	$(RM_CMD) $(EXE) $(ARCHIVES)
