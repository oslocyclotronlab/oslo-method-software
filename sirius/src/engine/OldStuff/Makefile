CFLAGS = -O3 -I/Applications/sirius/include
    CC = cc
 EOBJS = engine.o  vme_init.o bt_devname.o bit3_perror.o
    RM = rm -f
    CP = mv
RM_CMD = $(RM) *.o a.out core
CP_BIN = $(CP) acq_engine /Applications/sirius/bin/acq_engine
 
 
engine: $(EOBJS)
	$(CC) $(CFLAGS) $(EOBJS) -o acq_engine

clean:  
	@$(RM_CMD)

install:
	@$(CP_BIN)
