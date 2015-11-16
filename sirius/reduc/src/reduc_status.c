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

#include	<redmem_defs.h>
#include	<buffer_defs.h>


/* Define global variables */    
	int		Input_drive_no;		/* Drive number for data input exabyte/disk */
	int		Output_drive_no;	/* Drive number for data input exabyte/disk */
	int		*messp;				/* Pointer to shared memory message_box */
	int		Is_Started;
	int     outfileno;

int reduc_status( )
{
	int		time_off		= 1;
	int		stor1_off		= 2;
	int		tapetype_off	= 3;
	int 	offl_off		= 4;
	int		recc_off		= 5;
	int		badr_off		= 6;
	int     file2do_off		= 7;
	int		recs2do_off		= 8;
	int     retstat_off		= 9;
	int		shift_off		= 10;
	int		notel_off		= 11;
	int		stor2_off		= 12;
	int     filerec_off		= 13;
	int		evfrac_off		= 14;
	int		recex_off		= 15;
	int		pass_off		= 19;

	int		*timep;
	int		*stor1p;
	int		*stor2p;
	int		*offlp;
	int		*typep;
	int		*reccp;
	int		*badrp;
	int		*file2dop;
	int		*recs2dop;
	int		*retstatp;
	int		*shiftp;
	int		*notelp;
	int		*evfracp;
	int		*recexp;
	int		*passp;
	int     *filerecp;

	int		current1_file;
	int		current1_rec;
	int		current2_file;
	int		current2_rec;
	struct mtop 	mt_command;
	struct mtget 	mt_status;

	FILE	*strm;
	char	buf[1024];
	char	c;
	int		i,j;
	int		buflength = 1024;
	time_t	*tp;
	time_t	*xp;
	time_t	syst;
	int		blockfac;
	long int bytepos;

	xp = (void *) malloc(4);

	timep		= messp + time_off;
	stor1p		= messp + stor1_off;
	stor2p		= messp + stor2_off;
	typep		= messp + tapetype_off;
	offlp		= messp + offl_off;
	reccp		= messp + recc_off;
	badrp		= messp + badr_off;
	file2dop	= messp + file2do_off;
	recs2dop	= messp + recs2do_off;
	retstatp	= messp + retstat_off;
	shiftp		= messp + shift_off;
	notelp		= messp + notel_off;
	evfracp		= messp + evfrac_off;
	recexp		= messp + recex_off;
	passp		= messp + pass_off;
	filerecp	= messp + filerec_off;

	tp			= (void *)*timep;
	syst		= time(xp); 
	
	blockfac = REC_IN_BUF;						/* Default 4 32k blocks per databuffer */
	if ( *typep/10 == 1 ) {
	   blockfac = 64;							/* DAISY format, 64 1k blocks per databuffer */
	}


/* Determining current file and record for disk */
	if ( *typep/10 == 3 ) {						/* Input disk files */
           if (*stor1p > 0) {
              current1_file = *filerecp / 1000000;
      	      current1_rec  = *filerecp - current1_file * 1000000;
           } else {
					current1_file	= -1;
					current1_rec	=  0;
					*filerecp		=  0;
					*stor1p			= -1;
           }
        }
		  
	if ( (*typep - (*typep/10)*10) == 3 ) {		/* Output disk files */
           if (*stor2p > 0) {
					
/*  bytepos     = ftell((void *) *stor2p );   skulle vaere  tell( *stor2p ) ??? 
    lseek(*stor2p, bytepos, SEEK_CUR);
*/
			  bytepos = lseek(*stor2p, 0L, SEEK_CUR);
              current2_rec  = bytepos / (blockfac * 32 * 1024); 
              current2_file = outfileno;
           } else {
              outfileno    =  -1;
              current2_file =  outfileno;
      	      current2_rec  =  0;
              *stor2p      = -1;
           }
        }

	/* Read Input Drive Exabyte status structure */
  	if ( *stor1p >  0 && *typep/10 != 3) {
	   mt_command.mt_op = MTNOP;
	   if (ioctl( *stor1p, MTIOCTOP, &mt_command) == -1) {
	      return -1;
           }  
	   if (ioctl( *stor1p, MTIOCGET, (char *)&mt_status) == -1) {
	      return -1;
           }
	   	current1_file = mt_status.mt_fileno;
		current1_rec  = mt_status.mt_blkno / blockfac ;
	}
	if ( current1_file <  0 ) *stor1p = -1; 

	/* Read Output Drive Exabyte status structure, always SIRIUS type */
  	if ( *stor2p > 0 && (*typep - (*typep/10)*10) != 3) {
	   mt_command.mt_op = MTNOP;
	   if (ioctl( *stor2p, MTIOCTOP, &mt_command) == -1) {
	      return -1;
           }  
	   if (ioctl( *stor2p, MTIOCGET, (char *)&mt_status) == -1) {
	      return -1;
           }
	   	current2_file = mt_status.mt_fileno;
		current2_rec  = mt_status.mt_blkno / REC_IN_BUF;
	}
	if ( current2_file <  0 ) *stor2p = -1; 

	/* Write status to file /help/status.tmp */
	strm = fopen("/Applications/sirius/reduc/help/status.tmp","w");

        fprintf(strm," \n  STATUS (v.1.01) :\n");
        fprintf(strm," --------------------------------------------------\n");
	/* Medium part */
	if ( *typep/10 == 1 ) {
	   fprintf(strm," Input tape format \t\t: DAISY type\n");
	}  
	if ( *typep/10 == 0 ) {
		fprintf(strm," Input tape format \t\t: SIRIUS type\n");
	}
	if ( *typep/10 == 3 ) {
	   fprintf(strm," Input data format \t\t: DISK type\n");
	}
	if ( (*typep - (*typep/10)*10) == 0 ) {
		fprintf(strm," Output tape format\t\t: SIRIUS type\n");
	}
	if ( (*typep - (*typep/10)*10) == 3 ) {
	   fprintf(strm," Output data format\t\t: DISK type\n");
	}


	fprintf(strm," Number of telescopes\t: %d\n",*notelp);
	if ( *passp == 1) {
  	   fprintf(strm," Pass\t\t\t: Shift extraction\n");
	}
	if ( *passp == 2) {
  	   fprintf(strm," Pass\t\t\t: Data reduction\n");
	}

	if ( *stor1p > 0 ) {
		if(*typep/10 == 0) fprintf(strm," Data input\t\t\t: Exabyte %d\n", Input_drive_no);
		if(*typep/10 == 3) fprintf(strm," Data input\t\t\t: Disk files\n");
	   fprintf(strm,"	       ->  File\t: %d \n", current1_file);
	   fprintf(strm,"	       ->  Record\t: %d \n", current1_rec);
	}
	else {
		fprintf(strm," Data input\t\t\t: NONE\n");
	}

	if ( *stor2p > 0 ) {
		if((*typep - (*typep/10)*10) == 0) fprintf(strm," Data output\t\t: Exabyte %d\n", Output_drive_no);
		if((*typep - (*typep/10)*10) == 3) fprintf(strm," Data output\t\t: Disk files\n");
	   fprintf(strm,"	       ->  File\t: %d \n", current2_file);
	   fprintf(strm,"	       ->  Record\t: %d \n", current2_rec);
	}
	else {
		fprintf(strm," Data output\t\t: NONE\n");
	}

	/* Task Status */
	if ( *offlp == 0 && *retstatp < 1) {
		fprintf(strm," REDUC system status\t: NOT STARTED\n");
	} 
	if ( *offlp == 0 && *retstatp == 1) {
		fprintf(strm," REDUC system status\t: STOPPED BY USER\n");
	}
	if ( *offlp == 0 && *retstatp == 2) {
		fprintf(strm," REDUC system status\t: FILECOUNT REACHED\n");
	}
	if ( *offlp == 0 && *retstatp == 3) {
		fprintf(strm," REDUC system status\t: END OF MEDIA REACHED\n");
	}
	if ( *offlp == 0 && *retstatp == 4) {
		fprintf(strm," REDUC system status\t: RECORDCOUNT REACHED\n");
	}
	if ( *offlp == 0 && *retstatp == 5) {
		fprintf(strm," REDUC system status\t: END OF DATA REACHED\n");
	}
	if ( *offlp == 0 && *retstatp == 9) {
		fprintf(strm," REDUC system status\t: ERROR WRITING DATA\n");
	}
	if ( *offlp == 1 ) {
		fprintf(strm," REDUC system status\t: RUNNING\n");
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
fprintf(strm," Shift adjustment every \t: %d.th record\n",*shiftp);
	

	/* Record status */
	fprintf(strm," Records Sorted\t\t: %d\n",*reccp);

	if ( *passp == 2 ) {
 	   fprintf(strm," Records Extracted\t\t: %d\n",*recexp);
		fprintf(strm," Fraction of events accepted\t: %d %% \n",*evfracp);         
	}
       
	fprintf(strm," Bad records\t\t: %d\n",*badrp);
	fprintf(strm," --------------------------------------------------\n");

/*
        fprintf(strm," Exades 1 \t %d\n",*stor1p);
        fprintf(strm," Exades 2 \t %d\n",*stor2p);
*/

	fprintf(strm," Updated %s\n",asctime(localtime(xp)));

	fflush( strm );

	strm = freopen( "/Applications/sirius/reduc/help/status.tmp", "r", strm );
	for (i=0; i < buflength-1 && (c = getc( strm )) && c != EOF; i++)
	   buf[i] = toascii(c);
	fclose( strm );
	j = i;
	buf[j] = '\0';
	wprint("%s", buf);
	free(xp);
	return 0;
}