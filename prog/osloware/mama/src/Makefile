#    Makefile for mama
          CP = mv
      CP_DIR = ../../bin
         F77 = f77
      FFLAGS = -C -O3 -e
          RM = rm -f
      RM_CMD = $(RM) *.o core *~
        LIBS = -lX11
   LIB_PATHS = -L/usr/openwin/lib -R

ARCHIVES = gf2.a milib.a minig_x.a util.a
EXE = mama
SRCS = mama.f
OBJS = mama.o

.f.o:
	$(RM) $@
	$(F77) -c $(FFLAGS) $*.f

mama:   $(OBJS)
	$(F77) $(FFLAGS) -o $(EXE) $(OBJS) $(ARCHIVES) $(LIB_PATHS) $(LIBS)

all:    $(EXE)
	$(CP) $(EXE) $(CP_DIR)
	$(RM_CMD) $(ARCHIVES)

clean: 
	$(RM_CMD)

very-clean:
	$(RM_CMD) $(EXE) $(ARCHIVES)
