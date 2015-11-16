#include	<stdio.h>
#include	<stdlib.h>
#include	<errno.h>
#include	<sys/ipc.h>
#include	<sys/sem.h>
#include	<sys/file.h>
#include	<signal.h>
#include	<string.h>
#include	<ctype.h>
#include	<time.h>
#include	<unistd.h>
#include	<sys/types.h>           /*                                  */ 
#include	<sys/mtio.h>            /* header files for tape operations */ 
#include	<sys/ioctl.h>           /*                                  */ 

#include	<redmem_defs.h>
#include	<buffer_defs.h>

int	*messp;			/* Pointer to shared memory message_box */

int reduc_startshift( ) 
{
	const char	*startgain = "/Applications/sirius/bin/startgain &";

	int		sort_off		= 0;
	int		time_off		= 1;
	int		offl_off		= 4;
	int		recc_off		= 5;
	int		badr_off		= 6;
	int		retstat_off		= 9;
	int 	pass_off		= 19;

	int		*sortp;
	int		*timep;
 	int		*offlp;
	int		*reccp;
	int		*badrp;
	int		*retstatp;
	int		*passp;

	char		msg1[1024] = "Shift extraction started ";
	char		err2[1024]= "*** ERROR *** Start of sorting failed"; 

	time_t		*tp;
	time_t		syst;

	tp = (void *) malloc(4);
	syst = time(tp);

	sortp		= messp + sort_off;
	timep		= messp + time_off;
	offlp		= messp + offl_off;
	reccp		= messp + recc_off;
	badrp		= messp + badr_off;
	retstatp	= messp + retstat_off;
	passp		= messp + pass_off;
	
/* Put message box flags to start values */
	*sortp = 0;
	*timep = (int)tp;
	*offlp = 1;
	*reccp = 0;
	*badrp = 0;
	*retstatp = 0;
	*passp = 1;	

/*  Activate the sorting task */
	if ( system( startgain ) == -1) {
		errprint("%s\n",err2);
		return -1;
	}
  
	wprint("%s %s",msg1,asctime(localtime(tp)));
	return 0;
}
