#include	<ctype.h>
#include	<sys/file.h>

#include	<sys/types.h>           /*                                  */ 
#include	<sys/mtio.h>            /* header files for tape operations */ 
#include	<sys/ioctl.h>           /*                                  */ 

int	*messp;							/* Pointer to shared memory message_box */

int reduc_unload( int inout )
{

	struct		mtop 	mt_command;
	char		msg1[1024] = "Unloading Data Input Exabyte tape ...";
	char		msg2[1024] = "Unloading Data Output Exabyte tape ...";
	char		msgd1[1024] = "Closing Input Disc file ...";
	char		msgd2[1024] = "Closing Output Disc File ...";

	char		err[1024] = "*** ERROR *** Unload operation failed";
	int			stor1_off = 2;
	int			stor2_off = 12;
	int			*stor1p;
	int			*stor2p;
	int			*typep;
	int			tapetype_off = 3;

	stor1p = messp + stor1_off;
	stor2p = messp + stor2_off;
	typep  = messp + tapetype_off;
	
	if ( inout == 1 && *stor1p > 0) {
		if (*typep/10 != 3){
	      wprint("%s\n",msg1);
	      mt_command.mt_op = MTOFFL;
	      if (ioctl( *stor1p, MTIOCTOP, &mt_command) == -1) {
	         errprint("%s\n",err);
	         return -1;
			}
		} else {
	      wprint("%s\n",msgd1);
		}
		close ( *stor1p );
  	   *stor1p = 0;
		*typep  = 0*10 + (*typep - (*typep /10)*10);
	}

	if ( inout == 2  && *stor2p > 0) {
		if((*typep - (*typep /10)*10) != 3) {
	      wprint("%s\n",msg2);
	      mt_command.mt_op = MTOFFL;
	      if (ioctl( *stor2p, MTIOCTOP, &mt_command) == -1) {
	         errprint("%s\n",err);
	         return -1;
			}
		} else {
		wprint("%s\n",msgd2);
	}
	close ( *stor2p );
	*stor2p = 0;
	*typep = (*typep/10) * 10;
	}
	return 0;
}
