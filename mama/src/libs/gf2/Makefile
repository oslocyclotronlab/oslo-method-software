# Makefile for gf2 directory
#    Magne Guttormsen & Alexander Bürger
#    December 2008

# most settings like F95, CC, ... are in a common file 'Makeconf'
BASEDIR = ../../..
include $(BASEDIR)/Makeconf

     CFLAGS += -I$(BASEDIR)/../sirius/include
          CP = cp
      CP_DIR = ../..

ARCHIVE = gf2.a
F_SRCS = addcomment.f \
       adddelpk.f \
       addwin.f \
       ask.f \
       askyn.f \
       cask.f \
       caskyn.f \
       changecol.f \
       chngmark.f \
       cleanup.f \
       comfil.f \
       curse.f \
       dofit.f \
       dspfit.f \
       dspma.f \
       dspmkr.f \
       dspsp.f \
       environment.f \
       energy.f \
       eval.f \
       ffin.f \
       inin.f \
       figega.f \
       fitter.f \
       fix_para.f \
       fixorfree.f \
       folding.f \
       funcfit.f \
       getmkrchnl.f \
       gfexec.f \
       gffin.f \
       gfhelp.f \
       gfinit.f \
       gfset.f \
       gridls.f \
       grax.f \
       inin_flg.f \
       kelvin.f \
       matinv.f \
       norway.f \
       outlay.f \
       para2num.f \
       parset.f \
       peakfind.f \
       polyfit.f \
       rewr.f \
       setcts.f \
       setmarker.f \
       setext.f \
       sumcts.f \
       trax.f \
       typeit.f \
       lineup.f \
       attshared2mama.f\
       removepeak.f\
       uncorr.f
# not in OBJS of original Makefile: readsp.f

C_SRCS = grabchar.c \
       attspec2mama.c \
       offspec2mama.c

F_OBJS = $(F_SRCS:%.f=%.o)
C_OBJS = $(C_SRCS:%.c=%.o)
OBJS = $(C_OBJS) $(F_OBJS)

all:: $(ARCHIVE) gf2.o

.f.o:
	@echo "   [F95] $@"
	@$(RM) $@ # @ at start is not to show the command when running make
	@$(F95) -c $(FFLAGS) $*.f
.c.o:
	@echo "    [CC] $@"
	@$(RM) $@
	@$(CC) -c $(CFLAGS) $*.c

# gf2.f not needed in gf2.a but we can keep the source here,
# compile it and copy it to $(CP_DIR)

gf2.o: FORCE
	@echo "   [F95] $@"
	@$(F95) $(FFLAGS) -c gf2.f
	$(CP) gf2.o $(CP_DIR)

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
