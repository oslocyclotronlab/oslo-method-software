# Makefile for dwuck4
          CP = mv
      CP_DIR = ../../bin
         F77 = g77
      FFLAGS = -w 
     LDFLAGS = 

SRCS= adwuck.f bdwuck.f betafn.f cathen.f dwuck4.f elsig.f fnloc.f formf.f \
insig.f intde.f integ.f numint.f poten.f radint.f readi.f sbesm1.f tails.f \
tailyy.f taped.f cudate.f phasef.f dwplot.f vcc.f winej.f yxfct.f racah.f \
slatr.f radin.f dstrip.f lgndr.f polfct.f bind.f cou.f delr.f
OBJS= adwuck.o bdwuck.o betafn.o cathen.o dwuck4.o elsig.o fnloc.o formf.o \
insig.o intde.o integ.o numint.o poten.o radint.o readi.o sbesm1.o tails.o \
tailyy.o taped.o cudate.o phasef.o dwplot.o vcc.f winej.f yxfct.o racah.o \
slatr.o radin.o dstrip.o lgndr.o polfct.o bind.o cou.o delr.o

all:	dwuck4
	
dwuck4:	$(OBJS)
	g77 $(LDFLAGS) -o dwuck4 $(OBJS)
	$(CP) dwuck4 $(CP_DIR)	
	
clean: 
	-rm -f *~ *.o core
	
%.o:	%.f
	$(F77) $(FFLAGS) -c $*.f

