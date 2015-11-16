CFLAGS = -O3 -I/Applications/sirius/include
    CC = cc
 SOBJS = getsemid.o attshared.o attspec.o detshared.o user_routine.o rw.o sort_format.o cleansymbols.o box_chn.o wait4zero.o sort_main.o
    RM = rm -f
    CP = mv
RM_CMD = $(RM) *.o a.out core
CP_BIN = $(CP) acq_sort /Applications/sirius/bin/acq_sort
 
sort:	$(SOBJS)
	$(CC) $(CFLAGS) $(SOBJS) -o acq_sort

wait4zero.c:	
	$(CC) $(CFLAGS) wait4zero.o 

setsemaphore.c:	
	$(CC) $(CFLAGS) setsemaphore.o 

getsemaphore.c:	
	$(CC) $(CFLAGS) getsemaphore.o

attshared.c:	
	$(CC) $(CFLAGS) attshared.o 

attspec.c:	
	$(CC) $(CFLAGS) attspec.o 

detshared.c:	
	$(CC) $(CFLAGS) detshared.o 

getsemid.c:	
	$(CC) $(CFLAGS) getsemid.o
	
cleansymbols.c:	
	$(CC) $(CFLAGS) cleansymbols.o

box_chn.c:	
	$(CC) $(CFLAGS) box_chn.o

 
install:
	@$(CP_BIN)

clean:  
	@$(RM_CMD)