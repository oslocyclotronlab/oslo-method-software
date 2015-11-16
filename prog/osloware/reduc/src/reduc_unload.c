#include        <sys/types.h>
#include	<ctype.h>
#include        <fcntl.h>
#include	<sys/ioctl.h>
#include	<sys/mtio.h>

    	/* Define global variables */    
	int		*messp;			/* Pointer to shared memory message_box */

int reduc_unload( int inout )
{

	struct mtop 	mt_command;
	char		msg1[1024] = "Unloading Data Input Exabyte tape ...";
	char		msg2[1024] = "Unloading Data Output DLT tape ...";
	char		err[1024] = "*** ERROR *** Unload operation failed";
        int		stor1_off = 2;
        int		stor2_off = 12;
	int		*stor1p;
	int		*stor2p;

	stor1p = messp + stor1_off;
	stor2p = messp + stor2_off;
	
	if ( inout == 1 ) {
	   wprint("%s\n",msg1);
	   mt_command.mt_op = MTOFFL;

	   if (ioctl( *stor1p, MTIOCTOP, &mt_command) == -1) {
	      errprint("%s\n",err);
	      return -1;
           } 
           close ( *stor1p );
  	   *stor1p = 0;
	}

	if ( inout == 2 ) {
	   wprint("%s\n",msg2);
	   mt_command.mt_op = MTOFFL;

	   if (ioctl( *stor2p, MTIOCTOP, &mt_command) == -1) {
	      errprint("%s\n",err);
	      return -1;
           } 
           close ( *stor2p );
  	   *stor2p = 0;
	}

      	return 0;

}
