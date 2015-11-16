#include	<ctype.h>
#include        <fcntl.h>
#include	<sys/ioctl.h>
#include	<sys/mtio.h>

#include	<buffer_defs.h>


    	/* Define global variables */    
	int		exades;
	int		*messp;			/* Pointer to shared memory message_box */

int offline_position( int cup, int bot, int eod, int fileno, int recno)
{

	int		*typep;
	int		tapetype_off = 3;
    	int		current_file;
    	int		current_rec;
	int		files2skip;
	int		recs2skip;
	int		blockfac;

	struct mtop 	mt_command;
	struct mtget 	mt_status;
	char		msg1[1024] = "Start at current position - File/Record : ";
	char		msg2[1024] = "Positioning tape to beginning of tape ...";
	char		msg3[1024] = "Positioning tape to end of recorded data ...";
	char		msg4[1024] = "Positioning tape to file : ";
	

	typep = messp + tapetype_off;
	if ( *typep == 0 ) {
	   blockfac = REC_IN_BUF;
	}
	if ( *typep == 1 ) {
	   blockfac = 64;	/* DAISY format, 64 1k blocks per databuffer */
	}


	/* -------------------------------------------------- */
	/* CALL OPTION - Current position */
	if ( cup == 1 ) {
	   mt_command.mt_op = MTNOP;
	   if (ioctl( exades, MTIOCTOP, &mt_command) == -1) {
	      return -1;
           }  
	   if (ioctl( exades, MTIOCGET, (char *)&mt_status) == -1) {
	      return -1;
           } 

	   wprint( "%s%d/%d\n", msg1, mt_status.mt_fileno, mt_status.mt_blkno ); 

      	   return 0;
	}


	/* -------------------------------------------------- */
	/* CALL OPTION - BOT */
	/* Go to beginning of tape */
	if ( bot == 1 ) {
	   wprint( "%s\n",msg2 );
	   mt_command.mt_op = MTREW;
	   if (ioctl( exades, MTIOCTOP, &mt_command) == -1) {
	      return -1;
           }          
      	   return 0;
	}


	/* -------------------------------------------------- */
	/* CALL OPTION - EOD */
	/* Go to end of recorded data */
	if ( eod == 1 ) {
	   wprint( "%s\n",msg3 );
	   mt_command.mt_op = MTEOM;
	   if (ioctl( exades, MTIOCTOP, &mt_command) == -1) {
	      return -1;
           }          
      	   return 0;
	}


	/* -------------------------------------------------- */
	/* CALL OPTION - POS FILE/REC */
	/* Read the status */
	mt_command.mt_op = MTNOP;
	if (ioctl( exades, MTIOCTOP, &mt_command) == -1) {
	   return -1;
        }  
	if (ioctl( exades, MTIOCGET, (char *)&mt_status) == -1) {
	   return -1;
        } 

	current_file = mt_status.mt_fileno;
	current_rec  = mt_status.mt_blkno;
	files2skip   = fileno - current_file;
	recs2skip    = (recno  * blockfac) - current_rec;


	/* Case 1: Move to File x, Record 0 */
	if ( files2skip != 0 && recno == 0 ) {
	   wprint( "%s%d\n",msg4, fileno );
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
	   mt_command.mt_count = recno;
	   if (ioctl( exades, MTIOCTOP, &mt_command) == -1) {
	      return -1;
           }    
	   return 0;       
	}

	
	/* Case 3: Move to Record x in current file */
	/* OK for offline sort !                                */
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
	   wprint( "%s%d %d\n", msg1, mt_status.mt_fileno, mt_status.mt_blkno );
	}


}
