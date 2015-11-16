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
#include	<specpointers.h>
#include	<ipc_defs.h>

int *attshared( int pmode );				/* Attach shared memory */
int detshared( void *shmptr );				/* Detach shared memory */
int *attspec( int specno );					/* Attach spectrum area shared memory */

int main()
{
	/* *****************************************************	*/
	/*    PROGRAM    : specclear										*/
	/*    FILE       : ~/sirius/src/specclear.c					*/
	/*    WRITTEN BY : Magne Guttormsen								*/
	/*    DATE       : March 2008										*/ 
	/*    OS         : MacOS X 10.5.2								*/
	/*    COMPILER   : gcc 4.0.1										*/
	/* *****************************************************	*/

	int *messp;				/* Shared message boxr segment */
	int i, j;
	int specno;
	int attmode;
	int telescopeno;
	int fd;
	FILE *fp;
	int xdim, ydim;
	char err1[1024] = "Lock file present: /Applications/sirius/system/specclear.lock";
	

/* Check if the offline specdump lock-file is present, create as SCRATCH if not */
/* This is a security precaution, multiple processes may cause system crash */
   if(fd=open("/Applications/sirius/system/specclear.lock",O_CREAT|O_EXCL,PERMS)==-1 )
   {
      printf("%s\n",err1);
      exit(errno);
   }

/* Attach shared memory message box segment */
	attmode = 2;
	messp = attshared( attmode);
	if (messp == NULL){
		printf("Could not attach message box to sorting task\n");
		exit(0);
	}
	
	telescopeno	= *(messp+11);			/* Number of telescopes used */

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
		printf("Attach Alpha-NaI spectra failed \n");
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

/* *****************************************************	*/
/*      Clear the spectrum areas in shared memory			*/
/* ***************************************************** */
/*  Clear all particle spectra */
		xdim = 2048;
		ydim = telescopeno;
      for ( j = 0; j < telescopeno; j++){
         for ( i = 0; i < xdim; i++){
            *(pesp + i + j * xdim)     = 0;
            *(pdesp + i + j * xdim)    = 0;
            *(pedesp + i + j * xdim)   = 0;
            *(pthicksp + i + j * xdim) = 0;
         }
      }
/*  Clear Ge-spectrum */
		xdim = 4096;
		ydim = 6;
      for ( j = 0; j < ydim; j++){
         for ( i = 0; i < xdim; i++){
            *(pgesp + i + j * xdim) = 0;
         }
      }
/*  Clear Ge-T-spectrum */
		xdim = 512;
		ydim = 6;
      for ( j = 0; j < ydim; j++){
         for ( i = 0; i < xdim; i++){
            *(ptgesp + i + j * xdim) = 0;
         }
      }
/*  Clear NaI-spectrum */
		xdim = 2048;
		ydim = 32;
      for ( j = 0; j < ydim; j++){
         for ( i = 0; i < xdim; i++){
            *(pnasp + i + j * xdim) = 0;
         }
      }
/*  Clear NaI-T-spectrum */
		xdim = 512;
		ydim = 32;
      for ( j = 0; j < ydim; j++){
         for ( i = 0; i < xdim; i++){
            *(ptnasp + i + j * xdim) = 0;
         }
      }
/*  Clear singles spectrum */
		xdim = 4096;
		ydim = 10;
      for ( j = 0; j < ydim; j++){
         for ( i = 0; i < xdim; i++){
            *(psingles + i + j * xdim) = 0;
         }
      }
/*  Clear Alpha-NaI-spectrum */
		xdim = 2048;
		ydim = 512;
      for ( j = 0; j < ydim; j++){
         for ( i = 0; i < xdim; i++){
            *(pansp + i + j * xdim) = 0;
         }
      }
/*  Clear Alpha-Ge-spectrum */
		xdim = 2048;
		ydim = 512;
      for ( j = 0; j < ydim; j++){
         for ( i = 0; i < xdim; i++){
            *(pagsp + i + j * xdim) = 0;
         }
      }
/*  Clear GP matrix */
		xdim = 2048;
		ydim = 64;
      for ( j = 0; j < ydim; j++){
         for ( i = 0; i < xdim; i++){
            *(pmtsp + i + j * xdim) = 0;
         }
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
	remove("/Applications/sirius/system/specclear.lock");
	return 0;
}
