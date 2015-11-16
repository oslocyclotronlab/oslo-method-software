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
#include	<time.h>

#include	<offmem_defs.h>
#include	<buffer_defs.h>



#define ATTACH 1
#define DETTAC 2

    	/* Define global variables */    
	int		exades;			/* File descriptor for Exbyte */
	int		*messp;			/* Pointer to shared memory message_box */

int offline_start( ) 
{


        const char	*startsort = "/user/schiller/osloware/bin/startoffsort &";

	int		sort_off = 0;
	int		time_off = 1;
	int		offl_off = 4;
	int		recc_off = 5;
        int		badr_off = 6;
	int		retstat_off = 9;

	int		*sortp;
	int		*timep;
 	int             *offlp;
	int		*reccp;
	int		*badrp;
	int		*retstatp;

	char		msg[1024] = "Off-line sorting started ";
	char		err2[1024]= "*** ERROR *** Start of offline_sort failed"; 

	time_t		*tp;
	time_t		syst;

	tp = (void *) malloc(4);
	syst = time(tp);


	sortp = messp + sort_off;
	timep = messp + time_off;
	offlp = messp + offl_off;
	reccp = messp + recc_off;
	badrp = messp + badr_off;
	retstatp = messp + retstat_off;
	

	/* Put message box flags to start values */
	*sortp = 0;
	*timep = tp;
	*offlp = 1;
	*reccp = 0;
	*badrp = 0;
	*retstatp = 0;



        /*  Activate the sorting task  (f77) */
        if ( system( startsort ) == -1) {
           errprint("%s\n",err2);
           return -1;
        }  

	wprint("%s %s",msg,asctime(localtime(tp)));


                return 0;
}

