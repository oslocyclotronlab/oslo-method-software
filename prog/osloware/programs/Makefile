# Makefile for util directory
          CP = mv
      CP_DIR = ../bin
          CC = cc
         F77 = f77 -e
      FFLAGS = -O3 
      CFLAGS = -O
          RM = rm -f
      RM_CMD = $(RM) *.o core *~
   C_INCLUDE = -I/usr/openwin/include    
         LIB = Libraries/rewr.a
        MATH = Libraries/libmathlib.a
         EXE = angle \
               anglerand \
               autofit \
               calfermi \
               caloric \
               caltwo \
               cm2lab \
               decay \
               dobaczewski \
               doz \
               egidy \
               euler \
               extendspin \
               f_model \
               fgtheo \
               finalsort \
               finalsort2 \
               finalsort3 \
               funcfit \
               gameff \
               gbf \
               gbfbranch \
               grazing \
               isotrop \
               kin \
               legendre \
               leveldens \
               mainsort \
               mama2paw \
               mama2pc \
               neutron \
               normalization \
               normrhof \
               onefit \
               onemev \
               pairing \
               partition \
               party \
               polar \
               presort \
               range \
               realpartition \
               rhorhosig \
               rhosig \
               rhosigchi \
               rutherford \
               scatter \
               scissors \
               sea \
               sfec \
               stripheader \
               tave \
               testme \
               thermo \
               thetaangle \
               toro \
               tradcc \
               vigs \
               xmeasure \
               zigzag
angle: angle.o $(LIB) $(MATH)
	$(F77) $(FFLAGS) -o angle angle.o $(LIB) $(MATH)
anglerand: anglerand.o $(LIB) $(MATH)
	$(F77) $(FFLAGS) -o anglerand anglerand.o $(LIB) $(MATH)
autofit: autofit.o $(LIB)
	$(F77) $(FFLAGS) -o autofit autofit.o $(LIB)
calfermi: calfermi.o $(LIB)
	$(F77) $(FFLAGS) -o calfermi calfermi.o $(LIB)
caloric: caloric.o $(LIB)
	$(F77) $(FFLAGS) -o caloric caloric.o $(LIB)
caltwo: caltwo.o $(LIB)
	$(F77) $(FFLAGS) -o caltwo caltwo.o $(LIB)
cm2lab: cm2lab.o $(LIB)
	$(F77) $(FFLAGS) -o cm2lab cm2lab.o $(LIB)
decay: decay.o $(LIB)
	$(F77) $(FFLAGS) -o decay decay.o $(LIB)
dobaczewski: dobaczewski.o $(LIB)
	$(F77) $(FFLAGS) -o dobaczewski dobaczewski.o $(LIB)
doz: doz.o $(LIB)
	$(F77) $(FFLAGS) -o doz doz.o $(LIB)
egidy: egidy.o $(LIB)
	$(F77) $(FFLAGS) -o egidy egidy.o $(LIB)
euler: euler.o $(LIB)
	$(F77) $(FFLAGS) -o euler euler.o $(LIB)
extendspin: extendspin.o 
	$(CC) $(CFLAGS) -o extendspin extendspin.o $(C_INCLUDE) -lm
f_model: f_model.o $(LIB)
	$(F77) $(FFLAGS) -o f_model f_model.o $(LIB)
fgtheo: fgtheo.o $(LIB)
	$(F77) $(FFLAGS) -o fgtheo fgtheo.o $(LIB)
finalsort: finalsort.o $(LIB)
	$(F77) $(FFLAGS) -o finalsort finalsort.o $(LIB)
finalsort2: finalsort2.o $(LIB)
	$(F77) $(FFLAGS) -o finalsort2 finalsort2.o $(LIB)
finalsort3: finalsort3.o $(LIB)
	$(F77) $(FFLAGS) -o finalsort3 finalsort3.o $(LIB)
funcfit: funcfit.o $(LIB)
	$(F77) $(FFLAGS) -o funcfit funcfit.o $(LIB)
gameff: gameff.o $(LIB)
	$(F77) $(FFLAGS) -o gameff gameff.o $(LIB)
gbf: gbf.o $(LIB)
	$(F77) $(FFLAGS) -o gbf gbf.o $(LIB)
gbfbranch: gbfbranch.o $(LIB)
	$(F77) $(FFLAGS) -o gbfbranch gbfbranch.o $(LIB)
grazing: grazing.o $(LIB)
	$(F77) $(FFLAGS) -o grazing grazing.o $(LIB)
isotrop: isotrop.o $(LIB)
	$(F77) $(FFLAGS) -o isotrop isotrop.o $(LIB)
kin: kin.o
	$(CC) $(CFLAGS) -o kin kin.o $(C_INCLUDE) -lm
legendre: legendre.o $(LIB)
	$(F77) $(FFLAGS) -o legendre legendre.o $(LIB)
leveldens: leveldens.o $(LIB)
	$(F77) $(FFLAGS) -o leveldens leveldens.o $(LIB)
mainsort: mainsort.o $(LIB)
	$(F77) $(FFLAGS) -o mainsort mainsort.o $(LIB)
mama2paw: mama2paw.o $(LIB)
	$(F77) $(FFLAGS) -o mama2paw mama2paw.o $(LIB)
mama2pc: mama2pc.o $(LIB)
	$(F77) $(FFLAGS) -o mama2pc mama2pc.o $(LIB)
neutron: neutron.o $(LIB)
	$(F77) $(FFLAGS) -o neutron neutron.o $(LIB)
normalization: normalization.o $(LIB)
	$(F77) $(FFLAGS) -o normalization normalization.o $(LIB)
normrhof: normrhof.o $(LIB)
	$(F77) $(FFLAGS) -o normrhof normrhof.o $(LIB)
onefit: onefit.o $(LIB)
	$(F77) $(FFLAGS) -o onefit onefit.o $(LIB)
onemev: onemev.o $(LIB)
	$(F77) $(FFLAGS) -o onemev onemev.o $(LIB)
pairing: pairing.o $(LIB)
	$(F77) $(FFLAGS) -o pairing pairing.o $(LIB)
partition: partition.o $(LIB)
	$(F77) $(FFLAGS) -o partition partition.o $(LIB)
party: party.o $(LIB)
	$(F77) $(FFLAGS) -o party party.o $(LIB)
polar: polar.o $(LIB) $(MATH)
	$(F77) $(FFLAGS) -o polar polar.o $(LIB) $(MATH)
presort: presort.o $(LIB)
	$(F77) $(FFLAGS) -o presort presort.o $(LIB)
range: range.o $(LIB)
	$(F77) $(FFLAGS) -o range range.o $(LIB)
realpartition: realpartition.o $(LIB)
	$(F77) $(FFLAGS) -o realpartition realpartition.o $(LIB)
rhorhosig: rhorhosig.o $(LIB)
	$(F77) $(FFLAGS) -o rhorhosig rhorhosig.o $(LIB)
rhosig: rhosig.o $(LIB)
	$(F77) $(FFLAGS) -o rhosig rhosig.o $(LIB)
rhosigchi: rhosigchi.o $(LIB)
	$(F77) $(FFLAGS) -o rhosigchi rhosigchi.o $(LIB)
rutherford: rutherford.o $(LIB)
	$(F77) $(FFLAGS) -o rutherford rutherford.o $(LIB)
scatter: scatter.o $(LIB)
	$(F77) $(FFLAGS) -o scatter scatter.o $(LIB)
scissors: scissors.o $(LIB)
	$(F77) $(FFLAGS) -o scissors scissors.o $(LIB)
sea: sea.o $(LIB)
	$(F77) $(FFLAGS) -o sea sea.o $(LIB)
sfec: sfec.o $(LIB)
	$(F77) $(FFLAGS) -o sfec sfec.o $(LIB)
stripheader: stripheader.o $(LIB)
	$(F77) $(FFLAGS) -o stripheader stripheader.o $(LIB)
tave: tave.o $(LIB)
	$(F77) $(FFLAGS) -o tave tave.o $(LIB)
testme: testme.o $(LIB)
	$(F77) $(FFLAGS) -o testme testme.o $(LIB)
thermo: thermo.o $(LIB)
	$(F77) $(FFLAGS) -o thermo thermo.o $(LIB)
thetaangle: thetaangle.o $(LIB)
	$(F77) $(FFLAGS) -o thetaangle thetaangle.o $(LIB)
toro: toro.o $(LIB)
	$(F77) $(FFLAGS) -o toro toro.o $(LIB)
tradcc: tradcc.o $(LIB)
	$(F77) $(FFLAGS) -o tradcc tradcc.o $(LIB)
vigs: vigs.o $(LIB)
	$(F77) $(FFLAGS) -o vigs vigs.o $(LIB)
xmeasure: xmeasure.o $(LIB)
	$(F77) $(FFLAGS) -o xmeasure xmeasure.o $(LIB)
zigzag: zigzag.o $(LIB)
	$(F77) $(FFLAGS) -o zigzag zigzag.o $(LIB)
all: $(EXE)
	$(CP) $(EXE) $(CP_DIR) 
	$(RM_CMD)  
.f.o:
	$(RM) $@
	$(F77) $(FFLAGS) -c $*.f
.c.o:
	$(RM) $@
	$(CC) $(CFLAGS) -c $*.c
clean: 
	$(RM_CMD)
very-clean:
	$(RM_CMD) $(EXE) 
  

