#include	<stdio.h>
#include	<errno.h>
#include	<sys/ipc.h>
#include	<sys/sem.h>
#include	<sys/file.h>
#include	<signal.h>
#include	<string.h>
#include	<ctype.h>

#include	<sys/types.h>           /*                                  */ 
#include	<sys/mtio.h>            /* header files for tape operations */ 
#include	<sys/ioctl.h>           /*                                  */ 

#include	<redmem_defs.h>
#include	<buffer_defs.h>

	int		exades;			/* File descriptor for Exabyte */
	int		*messp;			/* Pointer to shared memory message_box */

int reduc_stop( )
{
	int		sort_off = 0;
	int		acqs_off = 4;
	int		recc_off = 5;

	int		*sortp;
	int		*engnp;
	int		*acqsp;
	int		*reccp;

	int		*savep;
	int		j;
	FILE	*fp;

	struct mtop 	mt_command;

	char		msg[1024] = "Number of records accumulated: ";

	sortp = messp + sort_off;
	acqsp = messp + acqs_off;
	reccp = messp + recc_off;

 	/* Print number of records read */
	wprint("%s %d\n",msg,*reccp);

	/* Stop the reduc sorting program */
	/* This is done by setting message_box(0) to the value '9' */
	*sortp = 9;

	/* Set acq status to stopped */
	*acqsp = 0;  
	return 0;
}
