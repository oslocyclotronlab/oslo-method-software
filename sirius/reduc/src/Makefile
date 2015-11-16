CFLAGS = -O3 -I/Applications/sirius/include
    CC = cc
 GOBJS = attshared.o detshared.o reduc_shiftformat.o reduc_winformat.o reduc_shiftsort.o reduc_winsort.o reduc_shiftcalc.o read_exa.o read_daisy.o read_disc.o write_exa.o readwrite.o reduc_main.o
    RM = rm -f
    CP = cp
RM_CMD = $(RM) *.o a.out core *~ *% fort.* *.tmp
CP_BIN = $(CP) reduc_sort /Applications/sirius/bin/.
 
sort:	$(GOBJS)
	$(CC) $(CFLAGS) $(GOBJS) -o reduc_sort

attshared.c:	
	$(CC) $(CFLAGS) attshared.o 

detshared.c:	
	$(CC) $(CFLAGS) detshared.o
 
read_exa.c:	
	$(CC) $(CFLAGS) read_exa.o 

read_disc.c:	
	$(CC) $(CFLAGS) read_disc.o 

read_daisy.c:	
	$(CC) $(CFLAGS) read_daisy.o 

write_exa.c:	
	$(CC) $(CFLAGS) write_exa.o 

install:
	@$(CP_BIN)

clean:  
	@$(RM_CMD)













