#include	<ctype.h>
#include	<fcntl.h>

#include	<sys/types.h>           /*                                  */ 
#include	<sys/mtio.h>            /* header files for tape operations */ 
#include	<sys/ioctl.h>           /*                                  */ 

/* Define global variables */    
	int		exades;			/* File descriptor for Exabyte */
	int		*messp;			/* Pointer to shared memory message_box */

int offline_unload( )
{
	struct	mtop 	mt_command;
	char	msg[1024]	= "Unloading tape ...";
	char	msgd[1024]	= "Closing file ...";
	char	err[1024]	= "*** ERROR *** Unload operation failed";
	int		*storp;
	int		*typep;
	int		tapetype_off = 3;
	int		stor_off     = 2;

	storp = messp + stor_off;
	typep = messp + tapetype_off;

	if(*typep !=3){
	   wprint("%s\n",msg);
		sleep(1);                       /* Waiting 1 sec to write message */
	   mt_command.mt_op = MTOFFL;
		if (ioctl( exades, MTIOCTOP, &mt_command) == -1) {
			errprint("%s\n",err);
			return -1;
		}
	} else {
		wprint("%s\n",msgd);
	}
	close ( exades );
	exades = 0;  
	*storp = 0;
	*typep = -1;
	return 0;
}
