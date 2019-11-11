# Makefile for minig_x directory
#    Magne Guttormsen & Alexander Bürger
#    December 2008

# most settings like F95, CC, ... are in a common file 'Makeconf'
BASEDIR = ../../..
include $(BASEDIR)/Makeconf

          CP = cp
      CP_DIR = ../..
   LIB_PATHS = -L. $(LIB_PATHS)

EXE     = minig_x
ARCHIVE = minig_x.a

F_SRCS  = txtclr.f \
	qio_put_text.f

C_SRCS  = set_focus.c \
	minigc.c \
	miniga.c \
	minigb.c \
	report_curs.c \
	cask.c \
	caskyn.c \
	util.c \
	write_no_advance.c

F_OBJS = $(F_SRCS:%.f=%.o)
C_OBJS = $(C_SRCS:%.c=%.o)
OBJS = $(C_OBJS) $(F_OBJS)

all:: $(ARCHIVE) $(EXE)

.c.o:
	@echo "    [CC] $@"
	@$(RM) $@
	@$(CC) -c $(C_INCLUDE) $*.c
.f.o:
	@echo "   [F95] $@"
	@$(RM) $@
	@$(F95) $(FFLAGS) -c $*.f

$(EXE): minig_x.o $(ARCHIVE)
	@echo "  [LINK] $@"
	@$(F95) $(FFLAGS) -o $@ minig_x.o $(C_INCLUDE) $(LIB_PATHS) $(LIBS) $(ARCHIVE)

$(ARCHIVE): minig_x.o $(OBJS) 
	@$(RM) $@
	@echo "    [AR] $@"
	@$(AR) $@ $(OBJS)
	@echo "[RANLIB] $@"
	@$(RANLIB) $@
	$(CP) $(ARCHIVE) $(CP_DIR)
# the - tells make to ignore an error.
# If running ranlib fails, there is probably not a ranlib for this system.
# so don't stop the build.

FORCE: 

clean: 
	$(RM) *.o core *~

very-clean: clean
	$(RM) $(ARCHIVE) $(EXE)
