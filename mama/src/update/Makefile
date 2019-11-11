# Makefile for upfiles directory
#    Magne Guttormsen & Alexander Bürger
#    December 2008

# most settings like F95, CC, ... are in a common file 'Makeconf'
BASEDIR = ../../..
include $(BASEDIR)/Makeconf

          CP = cp
      CP_DIR = ../..
     CFLAGS += -I$(BASEDIR)/../sirius/include

ARCHIVE = main.a

F_SRCS = dspmaUD.f \
       dspspUD.f \
       outlayUD.f \
       attshared2mama.f \
       main.f \
       loop.f \
       ask.f \
       cask.f \
       askyn.f \
       caskyn.f \
       grax.f \
       trax.f

C_SRCS = attspec2mama.c \
       offspec2mama.c \
       zzzz.c \
       grabchar.c

F_OBJS = $(F_SRCS:%.f=%.o)
C_OBJS = $(C_SRCS:%.c=%.o)
OBJS = $(C_OBJS) $(F_OBJS)

.f.o:
	@echo "   [F95] $@"
	@$(RM) $@
	@$(F95) -c $(FFLAGS) $*.f
.c.o:
	@echo "    [CC] $@"
	@$(RM) $@
	@$(CC) -c $(CFLAGS) $*.c

all:: $(ARCHIVE) main.o

# main.f not needed in main.a but we can keep the source here,
# compile it and copy it to $(CP_DIR)

main.o: FORCE
	@echo "   [F95] $@"
	@$(F95) $(FFLAGS) -c main.f
	$(CP) main.o $(CP_DIR)

$(ARCHIVE): $(OBJS)
	@$(RM) $@
	@echo "    [AR] $@"
	@$(AR) $@ $(OBJS)
	@echo "[RANLIB] $@"
	@$(RANLIB) $@
	$(CP) $(ARCHIVE) $(CP_DIR)
# the - tells make to ignore an error.
# If running ranlib fails, there is probably not one.
# so don't stop the build.

FORCE:

clean: 
	$(RM) *.o core *~

very-clean: clean
	$(RM) $(ARCHIVE)
