#include	<stdio.h>
#include	<stdlib.h>
#include	<errno.h>
#include	<sys/file.h>
#include	<sys/types.h>
#include	<sys/ipc.h>
#include	<sys/sem.h>
#include	<fcntl.h>
#include	<signal.h>
#include	<sys/shm.h>
#include	<sys/time.h>
#include	<offmem_defs.h>
#include	<buffer_defs.h>
#include	<specpointers.h>
#include	<gsdims.h>

int cleansymbols(FILE *fp1, FILE *fp2);		/* Remove commas and strange symbols from input data */
int *attshared( int mode );					/* Attach shared memory */
int detshared( void *shmptr );				/* Detach shared memory */
int *attspec( int specno );					/* Attach spectrum area shared memory */
int read_exa( int *bufp, int *messp );		/* Read data from Exabyte tape, take care of filemarks and EOM (SIRIUS) */
int read_daisy( int *bufp, int *messp );	/* Read data from Exabyte tape, take care of filemarks and EOM (DAISY) */
int read_disc( int *bufp, int *messp);		/* Read data from Disk file, take care of filemarks and EOF (DISK) */
int norr1dim(FILE *fp, char *comment, int *pxdim, float ax[8192], float cal[6]);
/*int format_event(int *rawdata, int *message_box, int *singles, int *esp, int *desp, 
int *edesp, int *thicksp, int *gesp, int *tgesp, int *nasp, int *tnasp, int *alfna, 
int *alfge, int *mat, int errstat);*/

int format_event(	int *bufp, int *messp, int *psingles, int *pesp, int *pdesp, int *pedesp, int *pthicksp, 
					int *pgesp, int *ptgesp, int *pnasp, int *ptnasp, int *pansp, int *pagsp, int *pmtsp);

int main()
{

	/* **************************************************************** */
	/*    PROGRAM    : offline                                          */
	/*    FILE       : ~/sirius/offline/src/offline_main				*/
	/*    WRITTEN BY : Magne Guttormsen                                 */
	/*    DATE       : February 2008                                    */ 
	/*    OS         : MacOS X 10.5.2                                   */
	/*    COMPILER   : gcc 4.0.1										*/
	/* **************************************************************** */
	/*    PURPOSE    : Off-line sorting of data.                        */       
	/*                                                                  */
	/*    MESSAGE_BOX:                                                  */
	/*                 ------------------------                         */
	/*            (0)  I       SO_FLAG        I                         */
	/*                 ------------------------                         */
	/*            (1)  I        Time          I                         */
	/*                 ------------------------                         */
	/*            (2)  I        exades        I                         */
	/*                 ------------------------                         */
	/*            (3)  I   Tape Record Type   I                         */
	/*                 ------------------------                         */
	/*            (4)  I    offline status    I                         */
	/*                 ------------------------ 			            */
	/*            (5)  I    Records sorted    I                         */
	/*                 ------------------------ 			            */
	/*            (6)  I     Bad records      I                         */
	/*                 ------------------------                         */
	/*            (7)  I       Files2do       I                         */
	/*                 ------------------------                         */
	/*            (8)  I       Recs2do        I                         */
	/*                 ------------------------                         */
	/*            (9)  I     Returnstatus     I                         */
	/*                 ------------------------                         */
	/*            (10) I                      I                         */
	/*                 ------------------------                         */
	/*            (11) I    No Of Telescopes  I                         */
	/*                 ------------------------                         */
	/*            (12) I                      I                         */
	/*                 ------------------------                         */
	/*            (13) I     filerecp         I  file + re number		*/
	/*                 ------------------------                         */
	/*            (14) I                      I                         */
	/*                 ------------------------ 			            */
	/*            (15) I                      I                         */
	/*                 ------------------------ 			            */
	/*            (16) I                      I                         */
	/*                 ------------------------                         */
	/*            (17) I                      I                         */
	/*                 ------------------------                         */
	/*            (18) I                      I                         */
	/*                 ------------------------                         */
	/*            (19) I                      I                         */
	/*                 ------------------------                         */
	/*                                                                  */
	/*    INPUT:                                                        */
	/*    message_box(0) : SO_FLAG = 9  Stop Sorting                    */
	/*    message_box(1) : Time for sorting started                     */   
	/*    message_box(2) : exades	file descriptor for file		    */   
	/*    message_box(3) : Tape Rec.Type	SIRIUS=0, DAISY=1, DISK=3    */   
	/*    message_box(7) : Files2do       0 = don't care                */
	/*    message_box(8) : Recs2do        0 = don't care                */
	/*                                                                  */ 
	/*    OUTPUT:                                                       */
	/*    message_box(5) : Records sorted                               */
	/*    message_box(6) : Bad Records                                  */
	/*    message_box(7) : Fraction sorted                              */
	/*    message_box(9) : Returnstatus     = 0  Running ...            */
	/*                                      = 1  Stopped by user        */  
	/*                                      = 2  File count reached     */
	/*                                      = 3  EOM encountered        */
	/*										= 4  Record count reached   */
	/*										= 5  End of Data  reached   */
	/*                                      = 9  Error in read          */
	/*                                                                  */
	/* **************************************************************** */

	int		*bufp;				/* Shared databuffer segment */
	int		*messp;				/* Shared message box segment */
	char	line[1024];
	char	cdum[128];
	char	comm[60];
	float	cal[6];
	int		xdim;
	FILE	*fp, *fp0;
	int		specno, attmode, errstat, running;
	int		i;
	int		bad_buffer;
	int		errcount;
	int		files2do, recs2do;
	int		fileno, recordno;
	int		readstatus;
	int		telescopeno;
	int		fd;
	char	err1[1024] = "Lock file present: /Applications/sirius/offline/system/offline.lock";

/* Check if the offline sort lock-file is present, create as SCRATCH if not */
/* This is a security precaution, multiple processes may cause system crash */
   if(fd=open("/Applications/sirius/offline/system/offline.lock",O_CREAT|O_EXCL,PERMS)==-1 )
   {
      printf("%s\n",err1);
      exit(errno);
   }
	close (fd);

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

/* ***********************************************	*/
/* Attach the spectrum areas in shared memory		*/
/* Maximum number of segments for process: 50		*/
/* Maximum segment size       : 8 MB				*/
/* Set in the file /etc/system :					*/
/*			set shmsys:shminfo_shmmax=8388608		*/
/*			set shmsys:shminfo_shmseg=50			*/
/*			set shmsys:shminfo_shmmni=100			*/
/* ***********************************************	*/

/* Attach  singles spectra */
	specno = 1;
	psingles = attspec(specno); 	
	if ( psingles == NULL){		
		printf("Attach Singles spectra failed\n");
		exit(0);
	}
/* Attach  E-spectra */
	specno = 2;
	pesp = attspec(specno); 	
	if ( pesp == NULL){
		printf("Attach E-spectra failed\n");
		exit(0);
	}
/* Attach  E-DE-spectra */
	specno = 3;
	pdesp = attspec(specno); 	
	if ( pdesp == NULL){
		printf("Attach DE-spectra failed\n");
		exit(0);
	}
/* Attach  E-DE-spectra */
	specno = 4;
	pedesp = attspec(specno); 	
	if ( pedesp == NULL){
		printf("Attach E-DE-spectra failed\n");
		exit(0);
	}
/* Attach  Thickness-spectra */
	specno = 5;
	pthicksp = attspec(specno); 	
	if ( pthicksp == NULL){
		printf("Attach Thickness-spectra failed\n");
		exit(0);
	}
/* Attach  Ge-spectra */
	specno = 6;
	pgesp = attspec(specno); 	
	if ( pgesp == NULL){
		printf("Attach Ge-spectra failed\n");
		exit(0);
	}
/* Attach  Ge-T-spectra */
	specno = 7;
	ptgesp = attspec(specno); 	
	if ( ptgesp == NULL){
		printf("Attach Ge-T-spectra failed\n");
		exit(0);
	}
/*  Attach  NaI spectra */
	specno = 8;
	pnasp = attspec(specno);
	if ( pnasp == NULL){
		printf("Attach NaI spectra failed\n");
		exit(0);
	}
/*  Attach  NaI-T spectra */
	specno = 9;
	ptnasp = attspec(specno);
	if ( ptnasp == NULL){
		printf("Attach NaI-T spectra failed\n");
		exit(0);
	}
/* Attach  Alpha-NaI spectra */
	specno = 10;
	pansp = attspec(specno); 	
	if ( pansp == NULL){
		printf("Attach Alpha-NaI spectra failed\n");
		exit(0);
	}
/* Attach  Alpha-Ge spectra */
	specno = 11;
	pagsp = attspec(specno); 	
	if ( pagsp == NULL){
		printf("Attach Alpha-Ge spectra failed\n");
		exit(0);
	}
/* Attach  GP matrix spectra */
	specno = 12;
	pmtsp = attspec(specno); 
	if ( pmtsp == NULL){
		printf("Attach General Purpose Matrix failed\n");
		exit(0);
	}
	
/* Initialize variables */
	telescopeno		= *(messp+11);				/* Number of telescopes used */
	bad_buffer		= 0;
	*(messp+9)		= 0;						/* Return status = Running */
	errcount		= 0;
	files2do		= *(messp+7);
	recs2do			= *(messp+8);
	fileno			= 0;
	recordno		= 0;

/* Initialization of gain and shift variables (written by offline_gui) */
	for( i = 0; i < 64; i++){
		gaine[i]	= 1.0;
		gainde[i]	= 1.0;
		shifte[i]	= 0.0;
		shiftde[i]	= 0.0;
	}
                            
	for( i = 0; i < 6; i++){ 
		gainge[i]	= 1.0;
		shiftge[i]	= 0.0;
		shifttge[i]	= 0.0;
	}

	for( i = 0; i < 32; i++){ 
		gainna[i]	= 1.0;
		shiftna[i]	= 0.0;
		shifttna[i]	= 0.0;
	}

/* Read gain and shift values from file /Applications/sirius/offline/data/gainshift.tmp */
	fp = fopen("/Applications/sirius/offline/data/gainshift.tmp", "r");
	if(fp == NULL){
		printf("Cannot open /Applications/sirius/offline/data/gainshift.tmp \n");
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


/* Initialization of low and high markers on thickness spectra */
	for( i = 0; i < 64; i++){
		ml[i] = 0;
		mh[i] = 511;
	}
/* Read low and high markers from file /Applications/sirius/offline/data/telewin.tmp */
	fp = fopen("/Applications/sirius/offline/data/telewin.tmp", "r");
	if(fp == NULL){
		printf("Cannot open /Applications/sirius/offline/data/telewin.tmp \n");
		printf("Execute STOP command \n");
   }
	else{
		fp0 = fopen("scratch.tmp", "w");
		if(fp0 == NULL){
			printf("Could not create scratch.tmp file for cleaning telewin.tmp\n");
			exit(0);
		}
	}
	if(cleansymbols(fp,fp0) == -1){
		printf("Could not clean telewin.tmp \n");
		exit(0);
	}
	fclose(fp);
	fclose(fp0);
	fp0 = fopen("scratch.tmp", "r");
	if(fp0 == NULL){
		printf("Could not open scratch.tmp file \n");
	}
	else{
		sleep(1);
		printf("Telescope[low,high]:\n");
		for(i = 0; i < 64; i++){
			fscanf(fp0,"%d",&ml[i]);
			fscanf(fp0,"%d",&mh[i]);
			if(i < telescopeno){
				printf(" %2d[%3d,%3d] ", i, ml[i], mh[i]);
				if(((i+1)/4)*4 == i+1 && i > 0){
					printf("\n");
				}
			}
		}
	}
	printf("\n");
	fclose(fp0);
	remove("scratch.tmp");

/* Read range curve from file /Applications/sirius/offline/data/RANGE.DATA */
	fp = fopen("/Applications/sirius/offline/data/RANGE.DATA", "r");
	if(fp == NULL){
		printf("Cannot open /Applications/sirius/offline/data/RANGE.DATA \n");
		printf("Execute STOP command \n");
   }
	else{
 		norr1dim(fp, comm, &xdim, range, cal);
	}
	fclose(fp);
	if(xdim != 2048){
		printf("Warning, RANGE.DATA has dimension %d (should have 2048) \n", xdim);
	}

/* ******************************************************/
/*              Main sorting loop starts				*/
/* ******************************************************/

	for(;;){
		if (*(messp+0) == 9){				/* Check for STOP command from offline_gui */
			*(messp+9) = 1;					/* Return status: Stopped by user */
			goto L999;
		}

/* Read data buffer from exatape or file */
/* Call read_exa (SIRIUS), read_daisy (DAISY) or read_disc (DISK)*/
		if(*(messp+3) == 0 ){
			readstatus = read_exa(bufp, messp);
		}
		else if(*(messp+3) == 1 ){
			readstatus = read_daisy(bufp, messp);
		}
		else if(*(messp+3) == 3 ){
			readstatus = read_disc(bufp, messp);
		}

/* Handle exception in read */
		if ( readstatus != 0 ) {								/* Exception deteced */
			if ( readstatus == 2 ) fileno++;					/* EOF */    
			
			if ( files2do - fileno == 0){						/* Filecount reached */
				*(messp+9) = 2;			
				goto L999;
			}
			if ( readstatus == 3 ){								/* EOM */
				*(messp+9) = 3;
				goto L999;
			}
			if ( readstatus == 9 ){								/* ERROR */
				*(messp+9) = 9;
				errcount++;
				if ( errcount >= 100){							/* Give up after 100 read errors */
					*(messp+9) = 5;								/* End-Of-Recorded-Data reached */
					goto L999;			           
				}												/* ... else try again */
			}
		}
		else{													/* OK, Data ready in memory */
			errcount = 0;
			recordno++;											/* Increment rec. count	*/
			if ( (recs2do - recordno) == 0 ){					/* Recordcount reached */
				*(messp+9) = 4;
				goto L999;
			}

/* Start processing */
			
			errstat = format_event(	bufp, messp, psingles, pesp, pdesp, pedesp, pthicksp, 
									pgesp, ptgesp, pnasp, ptnasp, pansp, pagsp, pmtsp); 
/*			recordno++;											/* Increment rec. count	*/
		
/*	Update Message_Box */
			*(messp+5) = recordno;
			if ( errstat == -1 ){
				bad_buffer++;
				*(messp+6) = bad_buffer;
			}
		}      
	}															/* End of infinite sorting loop */            
	L999: 
	
/* Clean up before exit */
	*(messp+4) = 0;												/* Stopped */
	*(messp+5) = recordno;

/* Detach shared memory before exit */
	if ( detshared(messp) == -1){
			printf("Detach Shared Databuffer Failed\n");
	}
	if ( detshared(bufp) == -1){
		printf("Detach Shared Message_Box Failed\n");
	}

/* Detach spectra in shared memory before exit */
	if ( detshared( psingles) == -1){
		printf("Detach Singles spectra failed\n");
	}
	if ( detshared( pesp) == -1){
		printf("Detach E-spectra failed\n");
	}
	if ( detshared( pdesp) == -1){
		printf("Detach DE-spectra failed\n");
	}
	if ( detshared( pedesp) == -1){
		printf("Detach E-DE spectra failed\n");
	}
	if ( detshared( pthicksp) == -1){
		printf("Detach Thick spectra failed\n");
	}
	if ( detshared( pgesp) == -1){
		printf("Detach Ge spectra failed\n");
	}
	if ( detshared( ptgesp) == -1){
		printf("Detach Ge-T spectra failed\n");
	}
	if ( detshared( pnasp) == -1){
		printf("Detach NaI spectra failed\n");
	}
	if ( detshared( ptnasp) == -1){
		printf("Detach NaI-T spectra failed\n");
	}
	if ( detshared( pansp) == -1){
			printf("Detach Alpha-NaI spectra failed\n");
	}
	if ( detshared( pagsp) == -1){
		printf("Detach Alpha-Ge spectra failed\n");
	}
	if ( detshared( pmtsp) == -1){
		printf(" Detach General Purpose Matrix failed\n");
	}

/* Close and remove lock-file */
	remove("/Applications/sirius/offline/system/offline.lock");
	remove("/Applications/sirius/offline/system/specdump.lock");
	remove("/Applications/sirius/offline/system/specclear.lock");

	return 0;
}
