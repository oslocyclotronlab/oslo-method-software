# Makefile for gf2 directory
          AR = ar rv
          CC = cc
      CFLAGS = -Xa -xO3 -I/user/schiller/osloware/include -DSOL2 -L/usr/ucblib -R/usr/ucblib -lucb 
          CP = mv
      CP_DIR = ../..
         F77 = f77
      FFLAGS = -O3 -C -e
          RM = rm -f
      RM_CMD = $(RM) *.o core *~


ARCHIVE = gf2.a
SRCS = addcomment.f \
       adddelpk.f \
       addwin.f \
       ask.f \
       attshared2mama.f \
       attspec.c \
       changecol.f \
       chngmark.f \
       cleanup.f \
       comfil.f \
       curse.f \
       detshared.c \
       dispwin.f \
       diveff.f \
       dofit.f \
       dspfit.f \
       dspma.f \
       dspmkr.f \
       dspsp.f \
       energy.f \
       environment.f \
       eval.f \
       figega.f \
       fitter.f \
       fix_para.f \
       fixorfree.f \
       folding.f \
       funcfit.f \
       getmkrchnl.f \
       getsp.f \
       gfexec.f \
       gffin.f \
       gfhelp.f \
       gfinit.f \
       gfset.f \
       gridls.f \
       kelvin.f \
       lookup.f \
       matinv.f \
       matread.f \
       norway.f \
       offspec.c \
       outlay.f \
       para2num.f \
       parset.f \
       peakfind.f \
       polyfit.f \
       readsp.f \
       rewr.f \
       setcts.f \
       setmarker.f \
       slice.f \
       spkread.f \
       startwid.f \
       sumcts.f \
       typeit.f \
       uncorr.f \
       weight.f \
       wrtlook.f \
       wrtsp.f

OBJS = addcomment.o \
       adddelpk.o \
       addwin.o \
       ask.o \
       attshared2mama.o \
       attspec.o \
       changecol.o \
       chngmark.o \
       cleanup.o \
       comfil.o \
       curse.o \
       detshared.o \
       dispwin.o \
       diveff.o \
       dofit.o \
       dspfit.o \
       dspma.o \
       dspmkr.o \
       dspsp.o \
       energy.o \
       environment.o \
       eval.o \
       figega.o \
       fitter.o \
       fix_para.o \
       fixorfree.o \
       folding.o \
       funcfit.o \
       getmkrchnl.o \
       getsp.o \
       gfexec.o \
       gffin.o \
       gfhelp.o \
       gfinit.o \
       gfset.o \
       gridls.o \
       kelvin.o \
       lookup.o \
       matinv.o \
       matread.o \
       norway.o \
       offspec.o \
       outlay.o \
       para2num.o \
       parset.o \
       peakfind.o \
       polyfit.o \
       readsp.o \
       rewr.o \
       setcts.o \
       setmarker.o \
       slice.o \
       spkread.o \
       startwid.o \
       sumcts.o \
       typeit.o \
       uncorr.o \
       weight.o \
       wrtlook.o \
       wrtsp.o 

.f.o:
	$(RM) $@
	$(F77) -c $(FFLAGS) $*.f
.c.o:
	$(RM) $@
	$(CC) -c $(CFLAGS) $*.c

all:: $(ARCHIVE)
	$(RM_CMD) $(ARCHIVE)

$(ARCHIVE):  $(OBJS) 
	$(AR) $@ $(OBJS)
	$(CP) $(ARCHIVE) $(CP_DIR)

clean: 
	$(RM_CMD)

very-clean:
	$(RM_CMD) $(ARCHIVE)
