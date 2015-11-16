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
#include	<ipc_defs.h>
#include	<buffer_defs.h>
#include	<specpointers.h>
#include	<gsdims.h>

int cleansymbols(FILE *fp1, FILE *fp2);		/* Remove commas and strange symbols from input data */
int *attshared( int mode );					/* Attach shared memory */
int detshared( void *shmptr );				/* Detach shared memory */
int *attspec( int specno );					/* Attach spectrum area shared memory */
int norr1dim(FILE *fp, char *comment, int *pxdim, float ax[8192], float cal[6]);
int wait4zero(int semid, int semno);
int getsemaphore(int semid, int semno);
int getsemid();
int format_event(int *bufp, int *messp, int *psingles, int *pesp, int *pdesp,int *pedesp, int *pthicksp, 
	int *pgesp, int *ptgesp, int *pnasp, int *ptnasp, int *pansp, int *pagsp, int *pmtsp); 


int main()
{

	/* **************************************************************** */
	/*    PROGRAM    : acq_sort                                         */
	/*    FILE       : ~/sirius/src/sort_main.c							*/
	/*    WRITTEN BY : Magne Guttormsen                                 */
	/*    DATE       : March 2008                                       */ 
	/*    OS         : MacOS X 10.5.2                                   */
	/*    COMPILER   : gcc 4.0.1										*/
	/* **************************************************************** */
	/*    PURPOSE    : On-line sorting of data.                         */       
	/*                                                                  */
	/*    MESSAGE_BOX:                                                  */
	/*                 ------------------------                         */
	/*            (0)  I       SO_FLAG        I                         */
	/*                 ------------------------                         */
	/*            (1)  I       EN_FLAG        I                         */
	/*                 ------------------------                         */
	/*            (2)  I       STG_FLAG       I                         */
	/*                 ------------------------                         */
	/*            (3)  I      VME_status      I                         */
	/*                 ------------------------                         */
	/*            (4)  I       ACQ_status     I                         */
	/*                 ------------------------							*/
	/*            (5)  I     Record count     I                         */
	/*                 ------------------------							*/
	/*            (6)  I     Bad records      I                         */
	/*                 ------------------------                         */
	/*            (7)  I    Fraction sorted   I                         */
	/*                 ------------------------                         */
	/*            (8)  I       Event Rate     I                         */
	/*                 ------------------------                         */
	/*            (9)  I  Event length * 100  I                         */
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
	/*    message_box(0) : SO_FLAG            = 3  Stop Sorting         */
	/*                                        = 9  Exit program         */   
	/*                                                                  */ 
	/*    OUTPUT:                                                       */
	/*    message_box(5) : Record count                                 */
	/*    message_box(6) : Bad Records                                  */
	/*    message_box(7) : Fraction sorted                              */
	/*    message_box(9) : Mean Event length (* 100)                    */
	/*                                                                  */
	/* **************************************************************** */

	int *bufp;				/* Shared databuffer segment */
	int *messp;				/* Shared message box segment */
	char line[1024];
	char cdum[128];
	char comm[60];
	float cal[6];
	int xdim;
	FILE *fp, *fp0;
	int specno, attmode, errstat;
	int semid, semno;
	int i;
	int bad_buffer;
	int telescopeno;
	int fd;
	char err1[1024] = "Lock file present: /Applications/sirius/system/sort.lock";

/* Wait 1 sec until engine has written status to screen */
	sleep(1);

/* Check if the online sort lock-file is present, create as SCRATCH if not */
/* This is a security precaution, multiple processes may cause system crash */
   if(fd=open("/Applications/sirius/system/sort.lock",O_CREAT|O_EXCL,PERMS)==-1 )
   {
      printf("%s\n",err1);
      exit(errno);
   }
	close (fd);
	
/* Get semaphore ID for set */
	semid = getsemid();
	if (semid == -1){
		printf("Could not obtain semaphore set\n");
		exit(0);
	}

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
	telescopeno		= *(messp+11);			/* Number of telescopes used */
	bad_buffer		= 0;

/* Initialization of gain and shift variables (written by sirius_gui) */
	for( i = 0; i < telescopeno; i++){
		gaine[i]   = 1.0;
		gainde[i]  = 1.0;
		shifte[i]  = 0.0;
		shiftde[i] = 0.0;
	}
                            
	for( i = 0; i < 6; i++){ 
		gainge[i]  = 1.0;
		shiftge[i] = 0.0;
		shifttge[i]= 0.0;
	}

	for( i = 0; i < 32; i++){ 
		gainna[i]  = 1.0;
		shiftna[i] = 0.0;
		shifttna[i]= 0.0;
	}

/* Read gain and shift values from file /Applications/sirius/data/gainshift.tmp */
	fp = fopen("/Applications/sirius/data/gainshift.tmp", "r");
	if(fp == NULL){
		printf("Cannot open /Applications/sirius/data/gainshift.tmp \n");
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
	

/* Read range curve from file /Applications/sirius/data/RANGE.DATA */
	fp = fopen("/Applications/sirius/data/RANGE.DATA", "r");
	if(fp == NULL){
		printf("Cannot open /Applications/sirius/data/RANGE.DATA \n");
		printf("Execute STOP command \n");
   }
	else{
 		norr1dim(fp, comm, &xdim, range, cal);
	}
	fclose(fp);
	if(xdim != 2048){
		printf("Warning, RANGE.DATA has dimension %d (should have 2048) \n", xdim);
	}


/* *****************************************************	*/
/*              Main sorting loop starts						*/
/* ***************************************************** */
	for(;;){
	
/* Wait for sort semaphore (semno = 0 to become zero */
/* Call function wait4zero with semid and semno=0 as parameter */
		if ( wait4zero( semid, semno) == -1){
				printf("Sort_main ERROR: wait4zero - Test Semaphore 1 Failed\n");
				printf("QUIT and RESTART Sirius!!!\n");
				*(messp+0) = 9;
				goto L999;
		}
		
/* Check for STOP command from acq_master */
		if (*(messp+0) == 9){				
			goto L999;
		}

/* Buffer is ready in data segment, start processing */
		errstat = format_event(	bufp, messp, psingles, pesp, pdesp, pedesp, pthicksp, 
									pgesp, ptgesp, pnasp, ptnasp, pansp, pagsp, pmtsp); 

/*	Update Message_Box */
		if ( errstat == -1 ){
			++bad_buffer;
			*(messp+6) = bad_buffer;
		}
	}																	/* End of infinite acquision loop */
	            
	L999: 

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
	remove("/Applications/sirius/system/sort.lock");
	remove("/Applications/sirius/system/specdump.lock");
	remove("/Applications/sirius/system/specclear.lock");

	return 0;
}
