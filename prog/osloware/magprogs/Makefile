# Makefile for magnetic tape programs
# modified for opal.nscl.msu.edu by A. Schiller, September 2003
          CC = cc
      CFLAGS = -xO2
          CP = mv
      CP_BIN = $(CP) mag2mag mag2disc /user/schiller/bin/.
          RM = rm -f
      RM_CMD = $(RM) *.o core *~
   C_INCLUDE = -I/usr/openwin/include    
         EXE = mag2mag \
               mag2disc \
               install \
               clean

.c.o:
	$(RM) $@
	$(CC) $(CFLAGS) -c $(C_INCLUDE) $*.c

mag2mag: mag2mag.o
	$(CC) $(CFLAGS) -o mag2mag  mag2mag.o  

mag2disc: mag2disc.o
	$(CC) $(CFLAGS) -o mag2disc mag2disc.o 
 
install: 
	$(CP_BIN)

all::   $(EXE)

clean: 
	$(RM_CMD)

