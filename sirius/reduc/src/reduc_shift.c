#include	<stdio.h>
#include	<errno.h>
#include	<sys/types.h>
#include	<sys/ipc.h>
#include	<sys/sem.h>
#include	<sys/file.h>
#include	<signal.h>
#include	<string.h>
#include	<ctype.h>
#include	<sys/ioctl.h>
#include	<sys/mtio.h>

/* Define global variables */    
int		*messp;			/* Pointer to shared memory message_box */

int reduc_shift( int shift )
{
	int		shift_off = 10;
	int		*shiftp;

	shiftp	= messp + shift_off;
	*shiftp = shift;		 
         
	return 0;
}
