#include	<stdio.h>
#include	<stdlib.h>
#include	<errno.h>
#include	<fcntl.h>
#include	<sys/types.h>
#include	<sys/ipc.h>
#include	<sys/sem.h>
#include	<signal.h>
#include	<sys/shm.h>
#include	<sys/time.h>
#include	<math.h>
#include	<unistd.h>
#include	<redmem_defs.h>
#include	<buffer_defs.h>
#include	<gsdims.h>
#include	<specdims.h>

#define		MAXSHIFTS     10000				/* Max number of shifts */

int cleansymbols(FILE *fp1, FILE *fp2);		/* Remove commas and strange symbols from input data */
int *attshared( int mode );					/* Attach shared memory */
int detshared( void *shmptr );				/* Detach shared memory */
int read_exa( int *bufp, int *messp );		/* Read data from Exabyte tape, take care of filemarks and EOM (SIRIUS) */
int read_daisy( int *bufp, int *messp );	/* Read data from Exabyte tape, take care of filemarks and EOM (DAISY) */
int read_disc( int *bufp, int *messp);		/* Read data from Disk file, take care of filemarks and EOF (DISK) */
int norr1dim(FILE *fp, char *comment, int *pxdim, float ax[8192], float cal[6]);
int shift_format( int *bufp, int *pesp, int *pdesp, int *pedesp);
int shift_calc(float goalp[64], float shift[64], int telescopeno, int *pesp, int *pdesp,int *pedesp, float centroid[64]);
int reduc_winformat(int *bufp, int *outp, int *messp, int *pesp, int *pdesp, int *pedesp, float final_edeshift[64], int *pwritestat, int *perrstat,  int newoutlength);


int main()
{
	/* ****************************************************************	*/
	/*    PROGRAM    : reduc											*/
	/*    FILE       : ~/sirius/reduc/src/reduc_main					*/
	/*    WRITTEN BY : Magne Guttormsen									*/
	/*    DATE       : June 2008										*/ 
	/*    OS         : MacOS X 10.5.5									*/
	/*    COMPILER   : gcc 4.0.1										*/
	/* **************************************************************** */
	/*    PURPOSE: The program runs through the event files twice.		*/
	/*				Pass 1 calculates the centroid of the elastic		*/
	/*				scattering peak in the E-counter and adjust the		*/
	/*				shift towards a "target" value.						*/
	/*				Pass 2 makes a datareduction putting windows in		*/
	/*				the thickness spectrum. There is one fixed window	*/
	/*				for p,d,t and one variable for the alpha-particles	*/
	/*				The accepted events are multiplied by the adjusted	*/
	/*				shift and written to a target device.				*/
	/*																	*/
	/* MESSAGE_BOX:														*/
	/*              ------------------------							*/
	/*         (0)  I       RED_FLAG       I							*/
	/*              ------------------------							*/
	/*         (1)  I        Time          I							*/
	/*              ------------------------							*/
	/*         (2)  I      in-exades       I     <- stor1p				*/
	/*              ------------------------							*/
	/*         (3)  I   Tape Record Type   I							*/
	/*              ------------------------							*/
	/*         (4)  I     reduc status     I							*/
	/*              ------------------------							*/
	/*         (5)  I    Buffers sorted    I							*/
	/*              ------------------------							*/
	/*         (6)  I     Bad records      I							*/
	/*              ------------------------							*/
	/*         (7)  I       Files2do       I							*/
	/*              ------------------------							*/
	/*         (8)  I        Recs2do       I							*/
	/*              ------------------------							*/
	/*         (9)  I     Returnstatus     I							*/
	/*              ------------------------							*/
	/*         (10) I        shift         I      <- shiftp				*/
	/*              ------------------------							*/
	/*         (11) I    No Of Telescopes  I      <- telep				*/
	/*              ------------------------							*/
	/*         (12) I      out-exades      I      <- stor2p				*/
	/*              ------------------------							*/
	/*         (13) I     filerecp         I  file+rec for input disk	*/
	/*              ------------------------							*/
	/*         (14) I  % of events accep.  I							*/
	/*              ------------------------							*/
	/*         (15) I   Records extracted  I							*/
	/*              ------------------------							*/
	/*         (16) I    Max shiftpointer  I							*/
	/*              ------------------------							*/
	/*         (17) I       Startfile      I							*/
	/*              ------------------------							*/
	/*         (18) I       Startrec       I							*/
	/*              ------------------------							*/
	/*         (19) I        Pass #        I							*/
	/*              ------------------------							*/
	/*																	*/
	/*																	*/
	/* INPUT:															*/
	/* message_box(0) : SO_FLAG         = 9  Stop Sorting				*/
	/* message_box(1) : Time			Sorting started at "time"		*/   
	/* message_box(2) : exades			file descriptor for exabyte		*/   
	/* message_box(3) : Tape Rec.Type	SIRIUS=0, DAISY=1, DISK=3		*/   
	/* message_box(7) : Files2do        0 = don't care					*/
	/* message_box(8) : Recs2do         0 = don't care					*/
	/* message_box(19): Pass # 1 = Shiftext, 2 = Data red.				*/
	/*																	*/ 
	/* OUTPUT:															*/
	/* message_box(5) : Records sorted									*/
	/* message_box(6) : Bad Records										*/
	/* message_box(7) : Fraction sorted									*/
	/* message_box(9) : Returnstatus    = 0  Running					*/
	/*                                  = 1  Stopped by user			*/  
	/*                                  = 2  File count reached			*/
	/*                                  = 3  EOM encountered in read	*/
	/*									= 4  Record count reached		*/
	/*									= 5  End of Data  reached		*/
	/*									= 6  Repositioning tape			*/
	/*									= 8  Error in read				*/
	/*                                  = 9  Error in write				*/
	/*									= 11 Shiftmatrix full			*/
	/*																	*/
	/* **************************************************************** */

	int *bufp;				/* Shared databuffer segment */
	int *messp;				/* Shared message box segment */
	char line[1024];
	char cdum[128];
	char comm[60];
	float cal[6];
	int xdim;
	int specno, attmode, errstat, writestat;
	int i, j, dum;
	int bad_buffer;
	int errcount;
	int files2do, recs2do;
	int fileno, recordno;
	int readstatus;
	int fd;
	int passno;								/* Pass no 1 = shift ext, pass no 2 = Data reduction */
	int telescopeno;						/* Number of telescopes used (8 or 64) */
	float goalp[64];						/* The goal peak values, read from file */
	float shift[64];						/* Shift adjustments, returned from shiftcalc */
	float centroid[64];
	float shift_matrix[64][MAXSHIFTS];		/* The extracted shift values, extended to 64 telescopes */
	float final_edeshift[64];				/* Shift values to be applied to accepted events */
	int maxshift;							/* Max number of shiftsets, Current value MAXSHIFTS */
	int AdjustShift;						/* Adjust shift for every n.th record  */
	int shift_pointer;
	int max_shift_pointer;	
	FILE *fp, *fp0;
	int newoutlength;
	int outlength;
	int *outp;
	int *pesp, *pdesp, *pedesp;
	
/* Check if the reduc sort lock-file is present, create as SCRATCH if not */
/* This is a security precaution, multiple processes may cause system crash */
   if(fd = open("/Applications/sirius/reduc/system/reduc.lock",O_CREAT|O_EXCL,PERMS) == -1 ){
      printf("Lock file present: /Applications/sirius/reduc/system/reduc.lock\n");
      exit(errno);
   }
	close(fd);

/* Attach shared memory databuffer segment */
	attmode = 1;
	bufp = attshared( attmode); 	
	if (bufp == NULL){
		printf("Could not attach memory to sorting task\n");
		exit(0);
	}

/* Attach shared memory message box segment */
	attmode = 2;
	messp = attshared( attmode);
	if (messp == NULL){
		printf("Could not attach message box to sorting task\n");
		exit(0);
	}
 
/* Initialize variables */
	files2do			= *(messp  +7);
	recs2do				= *(messp  +8);
	AdjustShift			= *(messp +10);		/* Adjust shift for every Adjustshift.th record */
	telescopeno			= *(messp +11);		/* Number of telescopes used */
	max_shift_pointer	= *(messp +16);		/* Max valid entry in shiftset */
	passno				= *(messp +19);		/* Pass no (1=Shift extraction, 2=data reduction) */
	*(messp + 9)		= 0;						/* Return status "Running" */
	*(messp + 15)		= 0;						/* Record extracted */
	*(messp + 5)		= 0;						/* Sorted */
	*(messp + 6)		= 0;						/* Bad recs */
	*(messp + 14)		= 0;						/* percent events accepted */
	fileno				= 0;
	recordno			= 0;
	shift_pointer		= 0;
	bad_buffer			= 0;
	errcount			= 0;
	maxshift			= MAXSHIFTS;				/* Max no. shift sets */
      

/* Read gain and shift values from file /Applications/sirius+/reduc/data/gainshift.tmp */
	fp = fopen("/Applications/sirius/reduc/data/gainshift.tmp", "r");
	if(fp == NULL){
		printf("Cannot open /Applications/sirius/reduc/data/gainshift.tmp \n");
		printf("Execute STOP command \n");
   }
	else{
		fp0 = fopen("scratch.tmp", "w");
		if(fp0 == NULL){
			printf("Could not create scratch.tmp file for cleaning gainshift.tmp\n");
			exit(0);
		}
	}
	if(cleansymbols(fp,fp0) == -1){
		printf("Could not clean gainshift.tmp \n");
		exit(0);
	}
	fclose(fp);
	fclose(fp0);
	fp0 = fopen("scratch.tmp", "r");
	if(fp0 == NULL){
		printf("Could not open scratch.tmp file \n");
   }
	else{
		printf("Gain and shifts");
		printf("\ng_e: \n");
		for(i = 0; i < 64; i++){
			fscanf(fp0,"%f",&gaine[i]);
			if(i < telescopeno){
				if((i/8)*8 == i && i > 0)printf("\n");
				printf(" %7.4f", gaine[i]);
			}
		}
		printf("\ng_de: \n");
		for(i = 0; i < 64; i++){
			fscanf(fp0,"%f",&gainde[i]);
			if(i < telescopeno){
				if((i/8)*8 == i && i > 0)printf("\n");
				printf(" %7.4f", gainde[i]);
			}
		}
		printf("\ng_ge: \n");
		for(i = 0; i < 6; i++){
			fscanf(fp0,"%f",&gainge[i]);
			printf(" %7.4f", gainge[i]);
		}
		printf("\ng_na: \n");
		for(i = 0; i < 32; i++){
			fscanf(fp0,"%f",&gainna[i]);
			if((i/8)*8 == i && i > 0)printf("\n");
			printf(" %7.4f", gainna[i]);
		}
		printf("\ns_e: \n");
		for(i = 0; i < 64; i++){
			fscanf(fp0,"%f",&shifte[i]);
			if(i < telescopeno){
				if((i/8)*8 == i && i > 0)printf("\n");
				printf(" %7.1f", shifte[i]);
			}
		}
		printf("\ns_de: \n");
		for(i = 0; i < 64; i++){
			fscanf(fp0,"%f",&shiftde[i]);
			if(i < telescopeno){
				if((i/8)*8 == i && i > 0)printf("\n");
				printf(" %7.1f", shiftde[i]);
			}
		}
		printf("\ns_ge: \n");
		for(i = 0; i < 6; i++){
			fscanf(fp0,"%f",&shiftge[i]);
			printf(" %7.1f", shiftge[i]);
		}
		printf("\ns_na: \n");
		for(i = 0; i < 32; i++){
			fscanf(fp0,"%f",&shiftna[i]);
			if((i/8)*8 == i && i > 0)printf("\n");
			printf(" %7.1f", shiftna[i]);
		}
		printf("\ns_tge: \n");
		for(i = 0; i < 6; i++){
			fscanf(fp0,"%f",&shifttge[i]);
			printf(" %7.1f", shifttge[i]);
		}
		printf("\ns_tna: \n");
		for(i = 0; i < 32; i++){
			fscanf(fp0,"%f",&shifttna[i]);
			if((i/8)*8 == i && i > 0)printf("\n");
			printf(" %7.1f", shifttna[i]);
		}
		printf("\n");
	}
	fclose(fp0);
	remove("scratch.tmp");


/* ******************************************************/
/*        S T A R T   S H I F T   E X T R A C T I O N	*/
/* ******************************************************/
	if ( passno == 1 ){
	
/* Reset temp result file */
		remove("/Applications/sirius/reduc/data/result.out");
		if(fd = open("/Applications/sirius/reduc/data/result.out",O_CREAT|O_EXCL,PERMS) == -1 ){
			printf("Could not reset /Applications/sirius/reduc/data/result.out \n");
			exit(errno);
		}
		close (fd);

/* Read goal peak values from file /Applications/sirius+/reduc/data/peak.tmp */
		fp = fopen("/Applications/sirius/reduc/data/peak.tmp", "r");
		if(fp == NULL){
			printf("Cannot open /Applications/sirius/reduc/data/peak.tmp \n");
			printf("Execute STOP command \n");
		}
		else{
			for( i = 0; i < telescopeno; i++){
				fgets(line,1024,fp);
				sscanf(line,"%f",&goalp[i]);
			}
		}
		fclose(fp);

/* Initialize shift matrix */
		for (j = 0; j < maxshift; j++){
			for(i = 0; i < telescopeno; i++){
				shift_matrix[i][j] = 0.0;
			}
		} 

/* ******************************************************/
/*              Main sorting loop starts				*/
/* ******************************************************/
		printf("\n");
		printf("Congratulation, you are now extracting shifts for\n");
		printf("the E detectors using the original data set (INPUT tape/disk).\n");
		printf("After this, you have to POSITION the INPUT tape/disk at the\n");
		printf("POSITION where the shift extraction started. Then, you \n");
		printf("choose DATA REDUCTION from the pull down menu, and you \n");
		printf("choose the same number of files/records as previously.\n");
		printf("The corrected E and dE values are now written on the \n");
		printf("OUTPUT tape/disk, and when you sort these data next time,\n");
		printf("you only need gain = 1 and shift = 0 for the dE and E detectors.\n");
		printf("\n");
		printf("Files from disk must have filenames:   file_0,  file_1, ...\n");
		printf("Files written to disk have filenames: reduc_0, reduc_1, ...\n");
		printf("The in/out event-files are in the directory where reduc is started.\n");
		printf("Good luck!\n");
		printf("magne.guttormsen@fys.uio.no\n");
		printf("\n");

		for(;;){
			if(*(messp+0) == 9){					/* Check for STOP command from _gui */
				*(messp+9) = 1;						/* Return status: Stopped by user */
				goto L777;
			}

/* Read data buffer from exatape or file */
/* Call read_exa (SIRIUS), read_daisy (DAISY) or read_disk (DISK)*/
			if(*(messp+3)/10 == 0 ){
				readstatus = read_exa(bufp, messp);
			}
			else if(*(messp+3)/10 == 1 ){
				readstatus = read_daisy(bufp, messp);
			}
			else if(*(messp+3)/10  == 3 ){
				readstatus = read_disc(bufp, messp);
			}

/* Handle exception in read */
			if ( readstatus != 0 ) {								/* Exception deteced */
				if ( readstatus == 2 ) fileno++;					/* EOF */      	
				if ( files2do - fileno == 0){						/* Filecount reached */
					*(messp+9) = 2;			
					goto L777;
				}
				if ( readstatus == 3 ){								/* EOM */
					*(messp+9) = 3;
					goto L777;
				}
				if ( readstatus == 9 ){								/* ERROR */
					*(messp+9) = 9;
					errcount++;
					if ( errcount >= 100){							/* Give up after 100 read errors */
						*(messp+9) = 5;								/* End-Of-Recorded-Data reached */
						goto L777;			           
					}												/* ... else try again */
				}
			}
			else{													/* OK, Data ready in memory */

/* ******************************************************/
/*        Start processing, find e-counter centroid		*/
/* ******************************************************/
				errcount = 0;
				recordno++;											/* Increment rec. count	*/
				errstat = shift_format(bufp, pesp, pdesp, pedesp);

/*	Update Message_Box */
				*(messp+5) = recordno;
				if ( errstat == -1 ){
					bad_buffer++;
					*(messp+6) = bad_buffer;
				}

/* Calculate E-counter shift for every nth record */
				if ((recordno/AdjustShift)*AdjustShift == recordno){
					shift_calc(goalp, shift, telescopeno, pesp, pdesp, pedesp, centroid);
					for (i = 0; i < telescopeno; i++){
						shift_matrix[i][shift_pointer] = shifte[i] + shift[i];
					}

/* Write gain and shift values to file /Applications/sirius+/reduc/data/result.out */
					fp = fopen("/Applications/sirius/reduc/data/result.out", "a+");
					if(fp == NULL){
						printf("Cannot open /Applications/sirius/reduc/data/result.out \n");
						printf("Execute STOP command \n");
					}
					else{
						fprintf(fp,"Resultset no %d\n",shift_pointer);
						for (i = 0; i < telescopeno; i++){
							fprintf(fp," %8.1f",centroid[i]);
						}
						fprintf(fp,"\n");
						for (i = 0; i < telescopeno; i++){
							fprintf(fp," %8.4f",shift_matrix[i][shift_pointer]);
						}
						fprintf(fp,"\n");
					}
					fclose(fp);

					if (shift_pointer >= maxshift) {
						*(messp+9) = 11;								/* Shift matrix full */
						goto L777;
					}
					shift_pointer++;

/* Clear E-spectrum before next fit */
					for (j = 0; j < telescopeno; j++){
						for (i = 0; i < 2048; i++){
							esp[i][j] = 0;
						}
					}
				}
				if ( (recs2do - recordno) == 0 ){						/* Recordcount reached */
					*(messp+9) = 4;
					goto L777;
				}
			}															/* Exception test */
		}																/* End Of Shift Extraction loop */ 
		L777:
		max_shift_pointer = shift_pointer;								/* Highest valid entry */


/* ***********************************	*/
/* Write extracted shift values to file	*/
/* ***********************************	*/
		fp = fopen("/Applications/sirius/reduc/data/extracted.shift", "w");
	
		if(fp == NULL){
			printf("Cannot open /Applications/sirius/reduc/data/extracted.shift \n");
			printf("Execute STOP command \n");
		}
		else{
			fprintf(fp,"%d\n",max_shift_pointer);
			fprintf(fp,"ShiftNo    Det.(0)  Det.(1)  Det.(2) ...");
			for( j = 0; j < max_shift_pointer; j++){
				fprintf(fp,"\n %6d", j);
				for (i = 0; i < telescopeno; i++){
					fprintf(fp," %8.4f",shift_matrix[i][j]);
				}
			}
		}
		fclose(fp);

		*(messp +16)  = max_shift_pointer;							/* Save in message box */
		shift_pointer = 0;
		*(messp + 4)  = 0;											/* Stopped */
		*(messp + 5)  = recordno;
		goto L999;
   }																/* End of Pass1 - Shift extraction */ 
/* ***********************************	*/
/* Shift extraction session terminated	*/
/* ***********************************	*/

/* ******************************************************/
/*       S T A R T   D A T A   R E D U C T I O N		*/
/* ******************************************************/
	if ( passno == 2 ){

/* Read extracted shift-matrix values from /Applications/sirius/reduc/data/extracted.shift */
		fp = fopen("/Applications/sirius/reduc/data/extracted.shift", "r");
		if(fp == NULL){
			printf("Cannot open /Applications/sirius/reduc/data/extracted.shift \n");
			printf("Execute STOP command \n");
		}
		else{
			fgets(line,1024,fp);
			sscanf(line,"%d",&max_shift_pointer);
			printf("%d\n",max_shift_pointer);
			fgets(line,1024,fp);
			printf("%s",line);
			for(j = 0; j < max_shift_pointer; j++){
				fscanf(fp,"%d", &dum);
				printf("\n%6d",dum);
				for(i = 0; i < telescopeno; i++){
					shift_matrix[i][j] = 0.;
					fscanf(fp,"%f \t", &shift_matrix[i][j]);
					printf("%8.4f",  shift_matrix[i][j]);
				}
			}
			printf("\n");			
		}
		fclose(fp);
	
		*(messp+16) = max_shift_pointer;
		outp = malloc(131072);						/* Allocate 128 kB = BUFFER_LENGTH for the buffer */

/* Read range curve from file /Applications/sirius/reduc/data/RANGE.DATA */
		fp = fopen("/Applications/sirius/reduc/data/RANGE.DATA", "r");
		if(fp == NULL){
			printf("Cannot open /Applications/sirius/reduc/data/RANGE.DATA \n");
			printf("Execute STOP command \n");
		}
		else{
			norr1dim(fp, comm, &xdim, range, cal);
		}
		fclose(fp);
		if(xdim != 2048){
			printf("Warning, RANGE.DATA has dimension %d (should have 2048) \n", xdim);
		}
		
/* Read window (gate) values from file /Applications/sirius/reduc/data/win.tmp */
		fp = fopen("/Applications/sirius/reduc/data/win.tmp", "r");
		if(fp == NULL){
			printf("Cannot open /Applications/sirius/reduc/data/win.tmp \n");
			printf("Execute STOP command \n");
		}
		else{
			fp0 = fopen("scratch.tmp", "w");
			if(fp0 == NULL){
				printf("Could not create scratch.tmp file for cleaning win.tmp\n");
				exit(0);
			}
		}
		if(cleansymbols(fp,fp0) == -1){
			printf("Could not clean win.tmp \n");
			exit(0);
		}
		fclose(fp);
		fclose(fp0);
		fp0 = fopen("scratch.tmp", "r");
		if(fp0 == NULL){
			printf("Could not open scratch.tmp file \n");
		}
		else{
			for(i = 0; i < telescopeno; i++){
				fscanf(fp0,"%d",&alfa_low[i]);
			}
			for(i = 0; i < telescopeno; i++){
				fscanf(fp0,"%d",&alfa_high[i]);
			}
			fscanf(fp0,"%d",&h_low);
			fscanf(fp0,"%d",&h_high);
		}
		fclose(fp0);
		remove("scratch.tmp");
		
		printf("\nAccepting events with common thickness\n[low,high] = [%3d,%3d] for all telescopes and:\n",h_low, h_high);
		for (i = 0; i < telescopeno; i++){
			printf(" %2d[%3d,%3d] ", i, alfa_low[i], alfa_high[i]);
			if(((i+1)/4)*4 == i+1 && i > 0){
				printf("\n");
			}
		}

		outlength = 0;
		
/* ******************************************************/
/*              Main sorting loop starts				*/
/* ******************************************************/
		for(;;){
			if(*(messp+0) == 9){					/* Check for STOP command from _gui */
				*(messp+9) = 1;						/* Return status: Stopped by user */
				goto L888;
			}

/* Read data buffer from exatape or file */
/* Call read_exa (SIRIUS), read_daisy (DAISY) or read_disc (DISK)*/
			if(*(messp+3)/10 == 0 ){
				readstatus = read_exa(bufp, messp);
			}
			else if(*(messp+3)/10 == 1 ){
				readstatus = read_daisy(bufp, messp);
			}
			else if(*(messp+3)/10  == 3 ){
				readstatus = read_disc(bufp, messp); 
			}

/* Handle exception in read */
			if ( readstatus != 0 ) {								/* Exception deteced */
				if ( readstatus == 2 ) fileno++;					/* EOF */      	
				if ( files2do - fileno == 0){						/* Filecount reached */
					*(messp+9) = 2;			
					goto L888;
				}
				if ( readstatus == 3 ){								/* EOM */
					*(messp+9) = 3;
					goto L888;
				}
				if ( readstatus == 9 ){								/* ERROR */
					*(messp+9) = 9;
					errcount++;
					if ( errcount > 100){							/* Give up after 100 read errors */
						*(messp+9) = 5;								/* End-Of-Recorded-Data reached */
						goto L888;			           
					}												/* ... else try again */
				}
			}
			else{													/* OK, data ready in memory */
				errcount = 0;

/* Change shift factors for every n.th record, save in vector final_eshift */
				if ((recordno/AdjustShift)*AdjustShift == recordno){
					for(i = 0;  i < telescopeno; i++){
						final_edeshift[i] = shift_matrix[i][shift_pointer];
					}
					shift_pointer++;
					if (shift_pointer >= max_shift_pointer-1){		/* Check that entry is valid */
						shift_pointer = max_shift_pointer-1;
					}
				}

/* Sort databuffer, accept events within the window and shift adjust */
				newoutlength = reduc_winformat(bufp, outp, messp, pesp, pdesp, pedesp, final_edeshift, &writestat, &errstat, outlength);
				outlength = newoutlength;
				recordno++;											/* Increment rec. count	*/
																	/* Update Message_Box */
				*(messp + 5) = recordno;							/* Number of sorted buffers */
				if ( errstat == -1 ){
					bad_buffer++;
					*(messp + 6) = bad_buffer;
				}
				if (writestat == -1){
					*(messp + 9) = 9;								/* Return status = write error */
					goto L888;                                      	
				} 
				if ( (recs2do - recordno) == 0 ){					/* Recordcount reached */
					*(messp + 9) = 4;
					goto L888;
				}
			}														/* Exception test */
		}															/* End Of Shift Extraction loop */
	
/* ********************************* */
/* Data reduction session terminated */
/* ********************************* */
		L888:
		*(messp + 4) = 0;											/* Stopped */
		free(outp); 
		goto L999;
	}																/* End of Pass2 - Data reduction */
	L999: 
 
/* Detach shared memory before exit */
	if (detshared(messp) == -1){
		printf("Detach Shared Databuffer Failed\n");
	}
	if (detshared(bufp) == -1){
		printf("Detach Shared Message_Box Failed\n");
	}

/* Close and remove lock-file */
	remove("/Applications/sirius/reduc/system/reduc.lock");
	return 0;
}