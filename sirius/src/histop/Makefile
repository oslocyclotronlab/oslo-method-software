CFLAGS = -O3 -I/Applications/sirius/include
    CC = cc
 DOBJS = specdump.o rw.o attspec.o attshared.o detshared.o
 COBJS = specclear.o attspec.o attshared.o detshared.o
    RM = rm -f
    CP = mv
RM_CMD = $(RM) *.o a.out core *~ *%
CP_B1N = $(CP) acq_specdump /Applications/sirius/bin/.
CP_B3N = $(CP) acq_specclear /Applications/sirius/bin/.
 
dump:	$(DOBJS)
	$(CC) $(CFLAGS) $(DOBJS) -o acq_specdump

clear:	$(COBJS)
	$(CC) $(CFLAGS) $(COBJS) -o acq_specclear
	
clean:  
	@$(RM_CMD)
installdump:  
	@$(CP_B1N)
installclear:  
	@$(CP_B3N)

