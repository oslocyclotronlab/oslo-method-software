CFLAGS = -O3 -I/Applications/sirius/include -I/usr/X11R6/include -I/sw/include/ -L/usr/X11R6/lib -L/sw/lib -lXm -lXt -lX11
    CC = cc
 TOBJS = acq_gui.o acq_message.o acq_storage.o acq_start.o acq_stop.o acq_status.o acq_clear.o acq_dump.o acq_position.o acq_unload.o acq_init.o number2string.o
    RM = rm -f
    CP = mv
RM_CMD = $(RM) *.o a.out core
CP_BIN = $(CP) acq_master /Applications/sirius/bin/.

master:	$(TOBJS)
	$(CC) $(CFLAGS) $(TOBJS) -o acq_master

clean:  
	@$(RM_CMD)

install:
	@$(CP_BIN)











