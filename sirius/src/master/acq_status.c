#include	<stdio.h>
#include	<stdlib.h>
#include	<errno.h>
#include	<sys/ipc.h>
#include	<sys/sem.h>
#include	<fcntl.h>
#include	<signal.h>
#include	<string.h>
#include	<ctype.h>
#include	<time.h>

#include	<sys/types.h>           /*                                  */ 
#include	<sys/mtio.h>            /* header files for tape operations */ 
#include	<sys/ioctl.h>           /*                                  */

#include	<ipc_defs.h>
#include	<buffer_defs.h>

#define ATTACH 1
#define DETTAC 2

/* Define global variables */    
int		exades;			/* File descriptor for Exbyte */
int		exano;			/* Exabyte drive number */
int		semid;			/* Semaphore set ID */
int		*messp;			/* Pointer to shared memory message_box */
int		Is_Started;
int     outfileno;
int     tapetype;

int acq_status( )
{
	int		sort_off = 0;
	int		engn_off = 1;
	int		stor_off = 2;
	int		vmes_off = 3;
	int 	acqs_off = 4;
	int		recc_off = 5;
	int		badr_off = 6;
	int     frac_off = 7;
	int		evrt_off = 8;
	int     evln_off = 9;
	int		notel_off= 11;

	int		*sortp;
	int		*engnp;
	int		*storp;
	int		*vmesp;
	int		*acqsp;
	int		*reccp;
	int		*badrp;
	int		*fracp;
	int		*evrtp;
	int		*evlnp;
	int		*notelp;

	int		current_file;
	int		current_rec;
	struct	mtop 	mt_command;
	struct	mtget 	mt_status;

	FILE		*strm;
	char		buf[1024];
	char		c;
	int			i,j;
	int			buflength = 1024;
	int			blockfac;
	long int	bytepos;

	time_t		*xp;
	time_t		syst;

/* Reset text buffer with blanks */	
	for (i=0; i < buflength-1; i++){
	   buf[i] = ' ';
	}

	sortp = messp + sort_off;
	engnp = messp + engn_off;
	storp = messp + stor_off;
	vmesp = messp + vmes_off;
	acqsp = messp + acqs_off;
	reccp = messp + recc_off;
	badrp = messp + badr_off;
	fracp = messp + frac_off;
	evrtp = messp + evrt_off;
	evlnp = messp + evln_off;
	notelp= messp + notel_off;

	xp = (void *) malloc(4);
	syst = time(xp); 

	blockfac = REC_IN_BUF;        /* Default 4 32k blocks per databuffer */

/* Determining current file and record for Disk */
	if ( tapetype == 3 ) {                      /* Disk files */
		if (exades > 0) {
			bytepos = lseek(exades, 0L, SEEK_CUR);
			current_rec  = bytepos / (blockfac * 32 * 1024); 
			current_file = outfileno;
		} else {
			outfileno    =  -1;
			current_file =  outfileno;
			current_rec  =  0;
			*storp       = -1;
			return -1;
			}
		}

	/* Read Exabyte status structure */
   	if ( tapetype != 3 ) {                     /* Exabyte files */
	   if ( exades >  0 ) {
	      mt_command.mt_op = MTNOP;
	      if (ioctl( exades, MTIOCTOP, &mt_command) == -1) {
	         return -1;
              }  
	      if (ioctl( exades, MTIOCGET, (char *)&mt_status) == -1) {
	         return -1;
              }
	      current_file = mt_status.mt_fileno;
	      current_rec  = mt_status.mt_blkno / REC_IN_BUF ;
	   } 
	}
	/* Write status to file /help/status.tmp */
	strm = fopen("/Applications/sirius/help/status.tmp","w");

	fprintf(strm,"\n STATUS (v.1.02): %s",asctime(localtime(xp)));
	fprintf(strm," ---------------------------------------------\n");
	fprintf(strm," Number of telescopes\t: %d\n",*notelp);
	/* Tape part */
	if ( exades > 0 ) {
		if(tapetype == 0) fprintf(strm," Data output\t\t\t: Exabyte %d\n", exano);
		if(tapetype == 3) fprintf(strm," Data output\t\t: Disk files\n");
	   	fprintf(strm,"	       ->  File\t: %d \n", current_file);
	   	fprintf(strm,"	       ->  Record\t: %d \n", current_rec);
	}
	else {
		fprintf(strm," Data storage\t\t: NONE\n");
	}
	
	if ( *acqsp == 1) {
		fprintf(strm," SIRIUS system status\t: RUNNING\n");
	}
	else if ( *acqsp == 0 ) {
		fprintf(strm," SIRIUS system status\t: STOPPED\n");
	}
	else {
		fprintf(strm," SIRIUS system status\t: UNKNOWN ?\n");
	}
	fprintf(strm," Records accumulated\t: %d\n",*reccp);
	fprintf(strm," Bad records\t\t: %d\n",*badrp);
	fprintf(strm," Fraction sorted\t\t: %d\n",*fracp);
	fprintf(strm," Average event length *100\t: %d\n",*evlnp);
	fprintf(strm," ---------------------------------------------\n");
    fflush( strm);
	strm = freopen( "/Applications/sirius/help/status.tmp", "r", strm );
	for (i=0; i < buflength-1 && (c = getc( strm )) && c != EOF; i++){
	   buf[i] = toascii(c);
	}
	fclose( strm );	

	j = i;

	if ( Is_Started == 1) {
		strm = fopen("/Applications/sirius/help/vmestatus.tmp","r");
		for (i = j; i < buflength-1 && (c = getc( strm )) && c != EOF; i++){
			buf[i] = toascii(c);
		}
		fclose( strm );
	} else {
		buf[j] 		= '\n';
		buf[j+1] 	= '\0';
	}
  	wprint("%s", buf);
	free(xp);
	return 0;
}
