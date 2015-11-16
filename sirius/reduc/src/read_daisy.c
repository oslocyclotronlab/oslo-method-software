/*	Routine for reading of databuffers from "old" DAISY format tapes 	*/
/*	The tapes, written on the ND-5800 computer, have a fixed block   	*/
/*	length of 1024 bytes. The databuffer is 32 kW long, wordlength 16	*/
/*	bytes.																*/


#include <stdio.h>
#include <errno.h>
#include <sys/file.h>
#include <stdlib.h>
#include <sys/types.h>				/*                                  */ 
#include <sys/mtio.h>				/* header files for tape operations */ 
#include <sys/ioctl.h>				/*                                  */ 
#include <unistd.h>

#define  RECORD_LENGTH	 	1024		
#define  REC_IN_BUF  		64		/* 64 records on exabyte per databuffer */

int read_daisy( int *bufp, int *messp )
{


	int			exades;	
	int			*filedesp;

	int			*buf1p;
	int			buf1inc=512;	

	int			bytes_read;
	int			status		= 0;
	int			eof_reached	= 2;
	int			eom_reached	= 3;
	int			error_in_read	= 9;
	int			i,j;

	short			tempbuf16[512];
	short			*tempp;
	struct mtop 	mt_command;
      

	buf1p = bufp;					/* Set local pointer to start of shared buffer */
	tempp = &tempbuf16[0];			/* Set temp pointer to start of temp buffer (16 bit) */


	filedesp = messp + 2;			/* Points to exabyte file descriptor */
	exades = *filedesp;


	for ( i=0; i < 64; i++) {

           bytes_read = read(exades, tempp, RECORD_LENGTH);
       	   if ( bytes_read != RECORD_LENGTH ) {
              if ( bytes_read == 0 ) {					/* EOF or EOM */
        	   bytes_read = read(exades, tempp, RECORD_LENGTH);	/* Read next record to check */
           	   if ( bytes_read == 0 )  {
		      return eom_reached;						/* EOM */
		   }
		   if ( bytes_read == -1 ) {					/* EOF identified */
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

	   /* Now, put the temp buffer (16 bit) into appropriate slot in shared memory buffer (32 bit)  */
	   for (j=0; j<512; j++) {
  	      *buf1p = tempbuf16[j];
	       buf1p++;
	   }
	}
	return 0;
}
