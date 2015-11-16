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
/*#include	<sys/mtio.h>            /* header files for tape operations */ 
#include	<sys/ioctl.h>           /*                                  */ 

#include	<offmem_defs.h>
#include	<buffer_defs.h>

#define		ATTACH 1
#define		DETTAC 2

/* Define global variables */    
	int		exades;			/* File descriptor for Exbyte */
	int		exano;			/* Exabyte drive number */
	int		*messp;			/* Pointer to shared memory message_box */
	int		Is_Started;

int offline_status( )
{
	int		time_off		= 1;
	int		tapetype_off	= 3;
	int		offl_off		= 4;
	int		recc_off		= 5;
	int		badr_off		= 6;
	int		file2do_off		= 7;
	int		recs2do_off		= 8;
	int		retstat_off		= 9;
	int		notel_off		= 11;

	int		*timep;
	int		*offlp;
	int		*typep;
	int		*reccp;
	int		*badrp;
	int		*file2dop;
	int		*recs2dop;
	int		*retstatp;
	int		*notelp;
	int		*filerecp;

	int		current_file;
	int		current_rec;
	
	#ifdef HAVE_MTIO
	struct mtop 	mt_command;
	struct mtget 	mt_status;
	#endif
	
	FILE		*strm;
	char		buf[1024];
	char		c;
	int			i,j;
	int			buflength = 1024;
	time_t		*tp;
	time_t		*xp;
	time_t		syst;
	int			blockfac;
	long int	bytepos;

	xp = (void *) malloc(4);

	timep		= messp + time_off;
	typep		= messp + tapetype_off;
	offlp		= messp + offl_off;
	reccp		= messp + recc_off;
	badrp		= messp + badr_off;
	file2dop	= messp + file2do_off;
	recs2dop	= messp + recs2do_off;
	retstatp	= messp + retstat_off;
	notelp		= messp + notel_off;
	typep		= messp + tapetype_off;
	filerecp	= messp + 13;

	tp     = (void *)*timep;
	syst   = time(xp);

	if ( *typep == 0 ) {
	   blockfac = REC_IN_BUF; /* SIRIUS format, 4 32k blocks per databuffer */
	}
	if ( *typep == 1 ) {
	   blockfac = 64;	  /* DAISY format, 64 1k blocks per databuffer */
	}
	if ( *typep == 3 ) {
	   blockfac = REC_IN_BUF; /* DISK format = SIRIUS format, see above */
	}
	
#ifdef HAVE_MTIO
/* Read Exabyte/file status structure */
  	if ( exades >  0 && *typep !=3) {
	   mt_command.mt_op = MTNOP;
	   if (ioctl( exades, MTIOCTOP, &mt_command) == -1) {
	      return -1;
		}  
	   if (ioctl( exades, MTIOCGET, (char *)&mt_status) == -1) {
	      return -1;
		}
		current_file = mt_status.mt_fileno;
		current_rec  = mt_status.mt_blkno / blockfac ;
	}
#endif

  	if ( exades >  0 && *typep ==3) {
		current_file = *filerecp/1000000;
		current_rec  = *filerecp - current_file * 1000000;
	}
	if ( current_file <  0 ) exades = -1; 

	/* Write status to file /help/status.tmp */
	strm = fopen("/Applications/sirius/offline/help/status.tmp","w");

	fprintf(strm," \n  STATUS (v.1.02):\n");
	fprintf(strm," --------------------------------------------------\n");
	
	/* Medium part */
	if ( *typep == 0 ) {
		fprintf(strm," Input tape format\t\t: SIRIUS type\n");
	}
	if ( *typep == 1 ) {
	   fprintf(strm," Input tape format\t\t: DAISY type\n");
	}
	if ( *typep == 3 ) {
	   fprintf(strm," Input data format\t\t: DISK type\n");
	}

	fprintf(strm," Number of telescopes\t: %d\n",*notelp);
	if ( exades > 0 ) {
		if(*typep != 3) fprintf(strm," Data source\t\t: Exabyte %d\n",exano);
		if(*typep == 3) fprintf(strm," Data source\t\t: Disk files\n");
	   fprintf(strm,"	       ->  File\t: %d \n", current_file);
	   fprintf(strm,"	       ->  Record\t: %d \n", current_rec);
	}
	else {
		fprintf(strm," Data storage\t\t: NONE\n");
	}

	/* Task Status */
	if ( *offlp == 0 && *retstatp < 1) {
		fprintf(strm," OFFLINE system status\t: NOT STARTED\n");
	} 
	if ( *offlp == 0 && *retstatp == 1) {
		fprintf(strm," OFFLINE system status\t: STOPPED BY USER\n");
	}
	if ( *offlp == 0 && *retstatp == 2) {
		fprintf(strm," OFFLINE system status\t: FILECOUNT REACHED\n");
	}
	if ( *offlp == 0 && *retstatp == 3) {
		fprintf(strm," OFFLINE system status\t: END OF MEDIA REACHED\n");
	}
	if ( *offlp == 0 && *retstatp == 4) {
		fprintf(strm," OFFLINE system status\t: RECORDCOUNT REACHED\n");
	}
	if ( *offlp == 0 && *retstatp == 5) {
		fprintf(strm," OFFLINE system status\t: END OF DATA REACHED\n");
	}
	if ( *offlp == 1 ) {
		fprintf(strm," OFFLINE system status\t: RUNNING\n");
		fprintf(strm," Sorting started\t\t: %s\n",asctime(localtime(tp)));
	if ( *recs2dop == 0 && *file2dop == 0) {
		fprintf(strm," Data to sort\t\t: WHOLE TAPE\n");
	}
	if ( *recs2dop != 0 ) {
		fprintf(strm," Data to sort\t\t: %d records\n",*recs2dop);
	}
	if ( *file2dop != 0 ) {
		fprintf(strm," Data to sort\t\t: %d file(s)\n",*file2dop);
	}
}

/* Record status */
	fprintf(strm," Records Sorted\t\t: %d\n",*reccp);
	fprintf(strm," Bad records\t\t: %d\n",*badrp);
	fprintf(strm," --------------------------------------------------\n");
	fprintf(strm," Updated %s\n\n",asctime(localtime(xp)));

	fflush( strm );

	strm = freopen( "/Applications/sirius/offline/help/status.tmp", "r", strm );
	for (i=0; i < buflength-1 && (c = getc( strm )) && c != EOF; i++)
	   buf[i] = toascii(c);
	fclose( strm );	
	j = i;
	buf[j] = '\0';
	wprint("%s", buf);
	free(xp);	
	return 0;
}
