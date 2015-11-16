#include	<stdio.h>
#include	<stdlib.h>
#include	<errno.h>
#include	<sys/ipc.h>
#include	<sys/sem.h>
#include	<fcntl.h>
#include	<signal.h>
#include	<string.h>
#include	<ctype.h>
#include	<time.h>
#include	<unistd.h>
#include	<sys/types.h>           /*                                  */ 
#include	<sys/mtio.h>            /* header files for tape operations */ 
#include	<sys/ioctl.h>           /*                                  */ 

#include	<ipc_defs.h>
#include	<buffer_defs.h>

#define ATTACH 1
#define DETTAC 2

/* Define global variables */    
int		exades;			/* File descriptor for Exbyte */
int     semid;		/* Semaphore set ID */
int		*messp;			/* Pointer to shared memory message_box */

int acq_start( ){ 

	int    semno;
	union  semun {
		int val;
		struct semid_ds *sbuf;
		ushort *array;
	} arg;

	const char	*starteng  = "/Applications/sirius/bin/startengine &";
	const char	*startsort = "/Applications/sirius/bin/startsort &";

	int		sort_off = 0;
	int		engn_off = 1;
	int		acqs_off = 4;
	int		recc_off = 5;
	int		badr_off = 6;

	int		*sortp;
	int		*engnp;
 	int      *acqsp;
	int		*reccp;
	int		*badrp;

	char	msg[1024] = "Acquisition started ";
	char	err1[1024]= "*** ERROR *** Start of acq_engine failed"; 
	char	err2[1024]= "*** ERROR *** Start of acq_sort failed"; 

	time_t	*tp;
	time_t	syst;

	tp		= (void *) malloc(4);
	syst	= time(tp);

	sortp	= messp + sort_off;
	engnp	= messp + engn_off;
	acqsp	= messp + acqs_off;
	reccp	= messp + recc_off;
	badrp	= messp + badr_off;

/* Put message box flags to start values */
	*sortp	= 0;
	*engnp	= 0;
	*acqsp	= 1;
	*reccp	= 0;
	*badrp	= 0;

/* Activate the acq_engine task*/
	semno	= 1;   
	arg.val = 0;                   
	if ( semctl(semid, semno, SETVAL, arg) == -1) {                          
	   perror("\n **** ERROR **** MASTER : setval failed \n");
		return -1;
	} 

	if ( system( starteng ) == -1) {
		errprint("%s\n",err1);
		return -1;
	}

/*  Activate the sorting task */
	semno = 0;
	arg.val = 1;
	if ( semctl(semid, semno, SETVAL, arg) == -1) {
		perror("\n **** ERROR **** SETVAL 0 failed \n"); 
		return -1;
	}

	if ( system( startsort ) == -1) {
		errprint("%s\n",err2);
		return -1;
	}  

	wprint("\n%s %s",msg,asctime(localtime(tp)));
	return 0;
}

