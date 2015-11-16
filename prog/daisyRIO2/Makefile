#************************************************************
# file: Makefile
# May 1996 Magne Guttormsen
#
# Aquisition programs for DAISY 
#************************************************************
#
# Rules
#
.c:
	$(CC) -o  $* $*.c $(CAMLIB) $(VMELIB) -L/lib/ces -lvme -luio

#
# Configuration
#
CC	= gcc -O3
TARGET = eventbuilder+ campari+ hex peekdaisy peekcamac peeksiri waiting slave peekreg vpeek8 vpeek16 vpeek32 vmem pokeA32slave vmejump siri slottest
VMELIB = /usr/src/ces/examples/vme/vlib.o
CAMLIB  = /usr/src/ces/examples/cbd/cbdclib.o

all:	$(TARGET) 

clean:
	rm -f *.o

clobber: clean
	rm -f $(TARGET) *.o
#
# Dependencies
#
eventbuilder+: eventbuilder+.c
campari+:      campari+.c 
hex:           hex.c
peekdaisy:     peekdaisy.c
vpeek8:        vpeek8.c
vpeek16:       vpeek16.c
vpeek32:       vpeek32.c
peekcamac:     peekcamac.c
peeksiri:      peeksiri.c
waiting:       waiting.c
slave:         slave.c
peekreg:       peekreg.c
peek32:        peek32.c
vmem:          vmem.c
pokeA32slave:  pokeA32slave.c
vmejump:       vmejump.c
siri:          siri.c
slottest:      slottest.c
