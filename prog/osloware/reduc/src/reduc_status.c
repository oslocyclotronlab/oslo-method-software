#include        <stdio.h>
#include        <errno.h>
#include        <sys/types.h>
#include	<sys/ipc.h>
#include	<sys/sem.h>
#include        <fcntl.h>
#include	<signal.h>
#include	<string.h>
#include	<ctype.h>
#include	<sys/ioctl.h>
#include	<sys/mtio.h>
#include	<time.h>

#include	<redmem_defs.h>
#include	<buffer_defs.h>


#define ATTACH 1
#define DETTAC 2


    	/* Define global variables */    
	int		Input_drive_no;		/* Drive number for data input exabyte */
	int		Output_drive_no;	/* Drive number for data input exabyte */
	int		*messp;			/* Pointer to shared memory message_box */
	int		Is_Started;

int reduc_status( )
{
	int		time_off 	= 1;
        int		stor1_off 	= 2;
	int		tapetype_off 	= 3;
        int 		offl_off 	= 4;
	int		recc_off 	= 5;
        int		badr_off 	= 6;
        int             file2do_off 	= 7;
	int		recs2do_off 	= 8;
        int             retstat_off 	= 9;
	int		gainf_off   	= 10;
        int		notel_off 	= 11;
        int		stor2_off	= 12;
        int		evfrac_off	= 14;
        int		recex_off	= 15;
        int		pass_off  	= 19;

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
	int		*gainfp;
        int		*notelp;
        int		*evfracp;
        int		*recexp;
	int		*passp;

	int		current1_file;
	int		current1_rec;
	int		current2_file;
	int		current2_rec;
	struct mtop 	mt_command;
	struct mtget 	mt_status;

	FILE		*strm;
    	char		buf[1024];
	char		c;
	int		i,j;
	int		buflength = 1024;
	time_t		*tp;
	time_t		*xp;
	time_t		syst;
	int		blockfac;

	xp = (void *) malloc(4);

	timep = messp + time_off;
	stor1p = messp + stor1_off;
	stor2p = messp + stor2_off;
	typep = messp + tapetype_off;
	offlp = messp + offl_off;
	reccp = messp + recc_off;
	badrp = messp + badr_off;
	file2dop = messp + file2do_off;
	recs2dop = messp + recs2do_off;
	retstatp = messp + retstat_off;
	gainfp   = messp + gainf_off;
	notelp   = messp + notel_off;
	evfracp	 = messp + evfrac_off;
        recexp   = messp + recex_off;
        passp    = messp + pass_off;

	tp = *timep;
	syst = time(xp); 

	if ( *typep == 0 ) {
	   blockfac = REC_IN_BUF;
	}
	if ( *typep == 1 ) {
	   blockfac = 64;	/* DAISY format, 64 1k blocks per databuffer */
	}


	/* Read Input Drive Exabyte status structure */
  	if ( *stor1p >  0 ) {
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
  	if ( *stor2p >  0 ) {
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
	strm = fopen("/user/schiller/osloware/reduc/help/status.tmp","w");

        fprintf(strm," \n  S T A T U S :\n");
        fprintf(strm," --------------------------------------------------\n");
	/* Tape part */
	if ( *typep == 0 ) {
      	   fprintf(strm," Input tape format\t\t: SIRIUS type\n");
	}
	if ( *typep == 1 ) {
	   fprintf(strm," Input tape format\t\t: DAISY type\n");
	}
        fprintf(strm," Number of telescopes\t: %d\n",*notelp);
        if ( *passp == 1) {
  	   fprintf(strm," Pass\t\t\t: Gain extraction\n");
        }
        if ( *passp == 2) {
  	   fprintf(strm," Pass\t\t\t: Data reduction\n");
        }

        if ( *stor1p > 0 ) {
           fprintf(strm," Data input\t\t\t: tape %d\n", Input_drive_no);
	   fprintf(strm,"	       ->  File\t: %d \n", current1_file);
	   fprintf(strm,"	       ->  Record\t: %d \n", current1_rec);
        }
	else {
           fprintf(strm," Data input\t\t\t: NONE\n");
	}

        if ( *stor2p > 0 ) {
           fprintf(strm," Data output\t\t: tape %d\n", Output_drive_no);
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
        fprintf(strm," Gain adjustment every \t: %d.th record\n",*gainfp);
	

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

	strm = freopen( "/user/schiller/osloware/reduc/help/status.tmp", "r", strm );
	for (i=0; i < buflength-1 && (c = getc( strm )) && c != EOF; i++)
	   buf[i] = toascii(c);
        fclose( strm );	
	j = i;
		

  	wprint("%s", buf);

        return 0;
}
