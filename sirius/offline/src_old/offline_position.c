#include	<stdio.h>
#include	<ctype.h>
#include	<fcntl.h>
#include	<sys/types.h>           /*                                  */ 
#include	<sys/mtio.h>            /* header files for tape operations */ 
#include	<sys/ioctl.h>           /*                                  */ 
#include	<stdlib.h>
#include	<unistd.h>

#include	<buffer_defs.h>


/* Define global variables */  
	int		exades;
	int		*messp;			/* Pointer to shared memory message_box */

int offline_position( int cup, int bot, int eod, int fileno, int recno)
{
	int		*typep;
	int     *filerecp;
	int		tapetype_off = 3;
	int		current_file;
	int		current_rec;
	int		files2skip;
	int		recs2skip;
	int		blockfac;
	long int bytepos;
	char     s1[4];
	char     in_file[128];         /* file names */ 
 
	struct mtop 	mt_command;
	struct mtget 	mt_status;
	char		msg1[1024] = "Position is - File/Record : ";
	char		msg2[1024] = "Positioning Exabyte to beginning of tape ...";
	char		msg3[1024] = "Positioning Exabyte to end of recorded data ...";
	char		msg4[1024] = "Positioning Exabyte to file : ";
	char		err0d[1024] = "Use the position at file/record no... ";
	char		msg2d[1024] = "Positioning to file : ";
	char		err1d[1024] = "Could not open file in present directory: file_";
	char		err2d[1024] = "Could not position to record : ";

	typep    = messp + tapetype_off;
	filerecp = messp + 13;

	if ( *typep == 0 ) {
	   blockfac = REC_IN_BUF; /* SIRIUS format, 4 32k blocks per databuffer */
	}
	if ( *typep == 1 ) {
	   blockfac = 64;	  /* DAISY format, 64 1k blocks per databuffer */
	}
	if ( *typep == 3 ) {
	   blockfac = REC_IN_BUF; /* DISK format = SIRIUS format, see above */
	} 

	/* -------------------------------------------------- */
	/* CALL OPTION - Current position */
	if ( cup == 1 && *typep != 3) {
	   mt_command.mt_op = MTNOP;
	   if (ioctl( exades, MTIOCTOP, &mt_command) == -1) {
	      return -1;
		}  
	   if (ioctl( exades, MTIOCGET, (char *)&mt_status) == -1) {
	      return -1;
		} 
	   wprint( "%s%d/%d\n", msg1, mt_status.mt_fileno, mt_status.mt_blkno/blockfac ); 
		return 0;
	}
	if ( cup == 1 && *typep == 3) {
		current_file= *filerecp / 1000000;
		current_rec = *filerecp - current_file * 1000000;
	   wprint( "%s%d/%d\n", msg1, current_file, current_rec ); 
		return 0;
	}


	/* -------------------------------------------------- */
	/* CALL OPTION - BOT */
	/* Go to beginning of tape */
	if ( bot == 1 && *typep != 3) {
	   wprint( "%s\n",msg2 );
		sleep(1);
	   mt_command.mt_op = MTREW;
	   if (ioctl( exades, MTIOCTOP, &mt_command) == -1) {
	      return -1;
		}          
		return 0;
	}

	/* Not in use */
	if ( bot == 1 && *typep == 3) {
	   errprint( "%s\n",err0d );
		return 0;
	}


	/* -------------------------------------------------- */
	/* CALL OPTION - EOD */
	/* Go to end of recorded data */
	if ( eod == 1 && *typep != 3) {
	   wprint( "%s\n",msg3 );
		sleep(1);
	   mt_command.mt_op = MTEOM;
	   if (ioctl( exades, MTIOCTOP, &mt_command) == -1) {
	      return -1;
		}          
		return 0;
	}

	/* Go to end of recorded data */
	if ( eod == 1 && *typep == 3) {
	   errprint( "%s\n",err0d );
		return 0;
	}


	/* -------------------------------------------------- */
	/* CALL OPTION - POS FILE/REC */
	/* Read the status */

	if ( *typep != 3) {              /* Case of exabyte-files */
	   mt_command.mt_op = MTNOP;
	   if (ioctl( exades, MTIOCTOP, &mt_command) == -1) {
	      return -1;
		}  
	   if (ioctl( exades, MTIOCGET, (char *)&mt_status) == -1) {
	      return -1;
		} 
	   current_file = mt_status.mt_fileno;
	   current_rec  = mt_status.mt_blkno;           
		if (fileno  == -1) fileno = current_file;
	   files2skip   = fileno - current_file;
	   recs2skip    = (recno * blockfac) - current_rec;


	   /* Case 1: Move to File x, Record 0 */
	   if ( files2skip != 0 && recno == 0 ) {
	      wprint( "%s%d\n",msg4, fileno );
			sleep(1);
	      mt_command.mt_op = MTFSF;
	      mt_command.mt_count = files2skip;
	      if (ioctl( exades, MTIOCTOP, &mt_command) == -1) {
	         return -1;
			}           
			return 0;
	   }


	   /* Case 2: Move first to File x Rec 0, Then to Record y */
	   /* Not Used in current version !                        */
	   if ( files2skip != 0 && recno != 0 ) {
	      mt_command.mt_op = MTFSF;
	      mt_command.mt_count = files2skip;
	      if (ioctl( exades, MTIOCTOP, &mt_command) == -1) {
	         return -1;
			}           
	      mt_command.mt_op = MTFSR;
	      mt_command.mt_count = recno * blockfac;
	      if (ioctl( exades, MTIOCTOP, &mt_command) == -1) {
	         return -1;
			}    
	      return 0;       
	   }

	
	   /* Case 3: Move to Record x in current file */
	   /* OK for offline sort !                    */
	   if ( files2skip == 0 && recs2skip != 0 ) {
	      mt_command.mt_op = MTFSR;
	      mt_command.mt_count = recs2skip;
	      if (ioctl( exades, MTIOCTOP, &mt_command) == -1) {
	         return -1;
			}           
			return 0;
	   }


	   /* Case 4: Don't do anything at all ... */
	   if ( files2skip == 0 && recs2skip == 0 ) {
	      wprint( "%s%d/%d\n", msg1, mt_status.mt_fileno, mt_status.mt_blkno/blockfac );
	   }
	}   


	if ( *typep == 3) {              /* Case of disk-files */
		current_file= *filerecp / 1000000;
		current_rec = *filerecp - current_file * 1000000;
		if (fileno == -1) fileno = current_file;
		files2skip   = fileno - current_file;
		recs2skip    = recno  - current_rec;

		if ( files2skip == 0 && recs2skip == 0 ){
	      wprint( "%s%d/%d\n", msg1, fileno, recno );
		} else {
			number2string(fileno, s1);    /* Move to File: fileno and Record: recno */
			sprintf(in_file,"file_%s",s1);
			if ((exades = open(in_file,O_RDONLY)) == -1) { /* file do not exist */
				errprint("%s%d\n",err1d,fileno);
				return -1;
			} else {
				bytepos = (blockfac*32*1024) * recno;
	         if(lseek(exades,bytepos,0) == -1){      /* record does not exist */
					errprint("%s %d\n",err2d,recno);
					exades=0;
					return -1;
				}  else {
					wprint( "%s%d/%d\n", msg1, fileno, recno ); 
					*filerecp  = fileno * 1000000 + recno;
					return 0;
				}
			}
		}
	}
}
