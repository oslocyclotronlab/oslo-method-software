#include        <stdio.h>
#include        <errno.h>
#include        <sys/types.h>
#include	<sys/ipc.h>
#include	<sys/sem.h>
#include        <fcntl.h>
#include	<signal.h>
#include	<string.h>
#include	<ctype.h>
#include	<sys/ioctl.h>
#include	<sys/mtio.h>

#include	<redmem_defs.h>


#define ATTACH 1
#define DETTAC 2


    	/* Define global variables */    
	int		*messp;			/* Pointer to shared memory message_box */


int reduc_gainfac( int gainfac )
{

   
	int		gainf_off = 10;
	int		*gainfp;

	gainfp = messp + gainf_off;
	*gainfp = gainfac;		 
         
      	return 0;
}
