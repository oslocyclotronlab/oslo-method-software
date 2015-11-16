#include	<ctype.h>
#include	<fcntl.h>

#include	<sys/types.h>           /*                                  */ 
#include	<sys/mtio.h>            /* header files for tape operations */ 
#include	<sys/ioctl.h>           /*                                  */ 

int			exades;
int			outfileno, tapetype;
int			*messp;			/* Pointer to shared memory message_box */
int			Media_On;
int			acq_unload( )
{
	int		stor_off = 2;
	int		*storp;
	struct	mtop 	mt_command;
	char	msg[1024] = "Unloading Exabyte tape ...";
	char	err[1024] = "*** ERROR *** Unload operation failed";
	char	msgd[1024] = "Closing Disk File ...";

	storp = messp + stor_off;

	if( tapetype != 3) {
	   wprint("%s\n",msg);
	   mt_command.mt_op = MTOFFL;
	   if (ioctl( exades, MTIOCTOP, &mt_command) == -1) {
	      errprint("%s\n",err);
	      return -1;
		}
		close( exades );
		exades    = 0;
		*storp    = -1;
	} else {
	   wprint("%s\n",msgd);
		close( exades );
		exades    = 0;
		*storp    = -1;
		outfileno = 0;
		tapetype  = 0;
	}
	return 0;
}