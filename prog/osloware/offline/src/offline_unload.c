#include        <sys/types.h>
#include	<ctype.h>
#include        <fcntl.h>
#include	<sys/ioctl.h>
#include	<sys/mtio.h>

    	/* Define global variables */    
	int		exades;			/* File descriptor for Exabyte */
	int		*messp;			/* Pointer to shared memory message_box */

int offline_unload( )
{

	struct mtop 	mt_command;
	char		msg[1024] = "Unloading tape ...";
	char		err[1024] = "*** ERROR *** Unload operation failed";
        int		stor_off = 2;
	int		*storp;

	storp = messp + stor_off;
	
	wprint("%s\n",msg);
	mt_command.mt_op = MTOFFL;

	if (ioctl( exades, MTIOCTOP, &mt_command) == -1) {
	   errprint("%s\n",err);
	   return -1;
        } 

        close ( exades );

	exades = 0;  
	*storp = exades;

      	return 0;

}
