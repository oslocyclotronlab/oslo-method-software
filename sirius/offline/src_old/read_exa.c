#include	<stdio.h>
#include	<errno.h>
#include	<fcntl.h>
#include	<stdlib.h>
#include	<sys/types.h>           /*                                  */ 
#include	<sys/mtio.h>            /* header files for tape operations */ 
#include	<sys/ioctl.h>           /*                                  */ 
#include	<unistd.h>
#include	<buffer_defs.h>

int read_exa( int *bufp, int *messp )
{
	int		exades;	
	int		*buf1p;
	int		*buf2p;
	int		*buf3p;
	int		*buf4p;
	int		*filedesp;

	int		bytes_read;
	int		part_offset		= 8192;
	int		status			= 0;
	int		eof_reached		= 2;
	int		eom_reached		= 3;
	int		error_in_read	= 9;

	struct	mtop 	mt_command;
      
	buf1p = bufp;					/* Points to part 1 of databuffer */
	buf2p = buf1p + part_offset;	/* Points to part 2 of databuffer */
	buf3p = buf1p + part_offset*2;	/* Points to part 3 of databuffer */
	buf4p = buf1p + part_offset*3;	/* Points to part 4 of databuffer */

	filedesp = messp + 2;			/* Points to exabyte file descriptor */
	exades = *filedesp;

        bytes_read = read(exades, buf1p, RECORD_LENGTH);
       	if ( bytes_read != RECORD_LENGTH ) {
           if ( bytes_read == 0 ) {					/* EOF or EOM */
        	bytes_read = read(exades, buf1p, RECORD_LENGTH);	/* Read next record to check */
           	if ( bytes_read == 0 )  {
		   return eom_reached;					/* EOM */
		}
		if ( bytes_read == -1 ) {				/* EOF identified */
	           mt_command.mt_op = MTFSF;
	           mt_command.mt_count = 1;
	   	   if (ioctl( exades, MTIOCTOP, &mt_command) == -1) {	/* Jump over filemark */
	      	      return -1;
           	   }           
		   return eof_reached;
		}
	   } else {
	     return error_in_read;
	   }
       	}
        bytes_read = read(exades, buf2p, RECORD_LENGTH);
       	if ( bytes_read != RECORD_LENGTH ) {
           if ( bytes_read == 0 ) {
       		bytes_read = read(exades, buf1p, RECORD_LENGTH);	/* Read next record to check*/
           	if ( bytes_read == 0 )  {
		   return eom_reached;					/* EOM */
                }
		if ( bytes_read == -1 ) {				/* EOF identified */
	           mt_command.mt_op = MTFSF;
	           mt_command.mt_count = 1;
	   	   if (ioctl( exades, MTIOCTOP, &mt_command) == -1) {	/* Jump over filemark */
	      	      return -1;
           	   }           
 		   return eof_reached;
		}					
	   } else {
	     return error_in_read;
	   }
       	}
        bytes_read = read(exades, buf3p, RECORD_LENGTH);
       	if ( bytes_read != RECORD_LENGTH ) {
           if ( bytes_read == 0 ) {
       		bytes_read = read(exades, buf1p, RECORD_LENGTH);	/* Read next record to check*/
           	if ( bytes_read == 0 )  {
		   return eom_reached;					/* EOM */
		}
		if ( bytes_read == -1 ) {				/* EOF identified */
	           mt_command.mt_op = MTFSF;
	           mt_command.mt_count = 1;
	   	   if (ioctl( exades, MTIOCTOP, &mt_command) == -1) {	/* Jump over filemark */
	      	      return -1;
           	   }           
		   return eof_reached;					
		}
	   } else { 
	        return error_in_read;
	   }
       	}
        bytes_read = read(exades, buf4p, RECORD_LENGTH);
       	if ( bytes_read != RECORD_LENGTH ) {
           if ( bytes_read == 0 ) {
       		bytes_read = read(exades, buf1p, RECORD_LENGTH);	/* Read next record to check*/
           	if ( bytes_read == 0 )  {
		   return eom_reached;					/* EOM */
		}
		if ( bytes_read == -1 ) {				/* EOF identified */
	           mt_command.mt_op = MTFSF;
	           mt_command.mt_count = 1;
	   	   if (ioctl( exades, MTIOCTOP, &mt_command) == -1) {	/* Jump over filemark */
	      	      return -1;
           	   }           
		return eof_reached;					
		}
	   }  else  { 
	      return error_in_read;
	   }
       	}

	return 0;
}
