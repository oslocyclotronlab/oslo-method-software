#************************************************************
# Makefile
# written   January 2005 Magne Guttormsen
# modified  December 2008 Alexander Buerger
#************************************************************

         F95 = gfortran -W -Wall -O3 -ffixed-line-length-132
         CC = gcc -W -Wall -O3 -std=gnu99
         LIBS = -lm
         RWLIB = rewr/rewr.a
	     INSTALL_BIN = install -m 0755
         INSTALL_DIR = $(UIO_APPLICATIONS)/prog
         RM = -rm -f

TARGET  = shiva2paw waiting funcfit gameff autofit onemev mama2pc vigs isotrop \
          zigzag legendre decay range thetaangle xmeasure rhosigchi scatter \
          kin readlog hex rocket neutron neutroneb thermo euler gbf gbfbranch \
          toro fgtheo splinetest egidy sea spline2mama normalization  \
          f_model rotor nilsson  normrhof cm2lab grazing \
          spincut rutherford fmodel tave binomial binomialc siriangle sfec alfnasort \
          alfnamatrix webrho webstrf fitone micro robin counting gammex d2rho combi \
          pet elise extendspin radex scissors pjolter primex diablo
# mtcp mag2mag mag2disc disc2mag

C_SOURCES = shiva2paw.c waiting.c kin.c hex.c rocket.c normalization.c \
    binomialc.c micro.c counting.c gammex.c d2rho.c combi.c scissors.c\
    extendspin.c pjolter.c diablo.c
# mtcp.c mag2mag.c mag2disc.c disc2mag.c
C_TARGETS = $(C_SOURCES:%.c=%)

F_SOURCES = beep.f doz.f polar.f funcfit.f gameff.f autofit.f onemev.f mama2pc.f \
   vigs.f isotrop.f zigzag.f legendre.f decay.f range.f thetaangle.f xmeasure.f rhosigchi.f \
   scatter.f readlog.f readtel.f neutron.f neutroneb.f thermo.f euler.f gbf.f gbfbranch.f \
   toro.f fgtheo.f splinetest.f egidy.f sea.f spline2mama.f f_model.f \
   rotor.f nilsson.f normrhof.f cm2lab.f grazing.f spincut.f rutherford.f fmodel.f tave.f \
   binomial.f siriangle.f sfec.f webstrf.f webrho.f alfnasort.f alfnamatrix.f fitone.f \
   robin.f pet.f elise.f radex.f primex.f
# angle.f
F_TARGETS = $(F_SOURCES:%.f=%)

# if make needs to do something in kinz, KINZ_IF_NEEDED will be
# 'kinz', otherwise it will be empty
KINZ_IF_NEEDED = $(shell make -q -C kinz > /dev/null || echo kinz)

REWR_IF_NEEDED = $(shell make -q -C rewr rewr.a > /dev/null || echo rewr)


all: $(KINZ_IF_NEEDED) $(REWR_IF_NEEDED) $(TARGET)

#kinz:
#	make -C kinz all

cprogs: $(C_TARGETS)

fprogs: $(F_TARGETS)

install:
	-mkdir $(INSTALL_DIR)/bin
	for program in $(TARGET) ; \
	    do $(INSTALL_BIN) $$program $(INSTALL_DIR)/bin/$$program; \
	done

$(F_TARGETS): %: %.f $(RWLIB)
	$(F95) -o  $* $*.f $(RWLIB)

$(C_TARGETS): %: %.c
	$(CC) -o  $* $*.c -lm

rewr $(RWLIB):
	make -C rewr rewr.a

clean: 
	make -C rewr clean
#	make -C kinz clean
	$(RM) *.o core *[~%]

very-clean: clean
	make -C rewr very-clean
#	make -C kinz very-clean
	$(RM) $(C_TARGETS) $(F_TARGETS) bin/$(TARGET)

remove:
	for target in $(TARGET) ; \
		do rm -f $(UIO_APPLICATIONS)/prog/bin/$$target ; \
	done

.PHONY: kinz rewr

.SUFFIXES:
