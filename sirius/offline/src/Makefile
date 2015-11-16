CFLAGS = -O3 -I/Applications/sirius/include
    CC = cc
 SOBJS = attshared.o attspec.o detshared.o user_routine.o readwrite.o read_exa.o read_daisy.o read_disc.o offline_main.o
    RM = rm -f
    CP = cp
RM_CMD = $(RM) *.o a.out core *~ *% fort.* *.tmp
CP_BIN = $(CP) off_sort /Applications/sirius/bin/.

sort:	$(SOBJS)
	$(CC) $(CFLAGS) $(SOBJS) -o off_sort

attshared.c:	
	$(CC) $(CFLAGS) attshared.o 

attspec.c:	
	$(CC) $(CFLAGS) attspec.o 

detshared.c:	
	$(CC) $(CFLAGS) detshared.o
 
read_exa.c:	
	$(CC) $(CFLAGS) read_exa.o 

read_daisy.c:	
	$(CC) $(CFLAGS) read_daisy.o 

read_disc.c:	
	$(CC) $(CFLAGS) read_disc.o 

install:
	@$(CP_BIN)

clean:  
	@$(RM_CMD)