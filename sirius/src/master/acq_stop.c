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

#include	<sys/types.h>           /*                                  */ 
#include	<sys/mtio.h>            /* header files for tape operations */ 
#include	<sys/ioctl.h>           /*                                  */ 

#include	<ipc_defs.h>
#include	<buffer_defs.h>

#define ATTACH 1
#define DETTAC 2

/* Define global variables */    
int		exades;			/* File descriptor for Exbyte */
int		semid;			/* Semaphore set ID */
int		*messp;			/* Pointer to shared memory message_box */
int     outfileno, tapetype;
int     created;

int acq_stop( )
{
	int		semno;
	union	semun {
		int val;
		struct semid_ds *sbuf;
		ushort *array;
	} arg;
	int		sort_off = 0;
	int		engn_off = 1;
	int		acqs_off = 4;
	int		recc_off = 5;        
	int		stor_off = 2;
	int		*storp;
	int		*sortp;
	int		*engnp;
	int		*acqsp;
	int		*reccp;
	int		*savep;
	int		j;
	pid_t	*pid;
	pid_t	mypid;
	FILE	*fp;
	int     sig = SIGKILL;

	struct mtop 	mt_command;

	char	msg[1024] =		"Number of records accumulated: ";
	char	err1[1024]=		"*** ERROR *** Stop of acq_sort failed";
	char	errd[1024]=		"*** ERROR *** Could not open disk file: sirius_xxx";
	char	discout[1024]=	"New output file created, filename:  ";
	int     n;
	char    s1[4];
	char    out_file[128];         /* file names */ 

	time_t	*tp;
	time_t	syst;

	tp = (void *) malloc(4);
	syst = time(tp);

	sortp = messp + sort_off;
	engnp = messp + engn_off;
	acqsp = messp + acqs_off;
	reccp = messp + recc_off;
	storp = messp + stor_off;

/* Print number of records read */
	wprint("\nAcquisition stopped %s",asctime(localtime(tp)));
	wprint("%s %d\n",msg,*reccp);

/* Stop the engine program */
/* This is done by setting message_box(1) to the value '9' */
	*engnp = 9;

/* Stop the sorting program */
/* This is done by setting message_box(0) to the value '9' */
	*sortp = 9;
	
	semno = 0;
	arg.val = 0;                       /* Kick sorter out of a possible sem wait */
	if ( semctl(semid, semno, SETVAL, arg) == -1) {                          
		errprint("%s\n",err1);
		exit(errno);
	} 

/* Set acq status to stopped */
	*acqsp = 0;	

/* Write an EOF mark on Exabyte tape if this is used */
	if ( exades >  0 && tapetype !=3) {
	   mt_command.mt_op = MTWEOF;
	   mt_command.mt_count = 1;
	   if (ioctl( exades, MTIOCTOP, &mt_command) == -1) {
	      exit(errno);
		}
	} 

/* Closing and opening new disk file if this is used*/
	if ( exades >  0 && tapetype ==3) {
		sleep(1);
		if ( close( exades ) == -1) {
			errprint("%s\n",err1);
		}
        
		created = -1;
		for (n = 0; n < 1000; n++){      /*Finding last filenumber used*/
			number2string(n, s1);
			sprintf(out_file,"sirius_%s",s1);
			if ((exades = open(out_file,O_RDWR)) == -1) { /* file non-exists */
				created  = 1;
				break;
			}
		}
		if ( created == -1){
			errprint("%s\n",errd);
			exades   = 0;
			close(*storp);
			*storp   = exades;
			tapetype = 0;                    /* disk is not output device  */
			return -1;
		} else {                            /* create a new file sirius_0 */
			if ((exades = creat(out_file,PERMS)) == -1) {
				wprint("Could not create new outfile %s\n",out_file);
				exades   = 0;
				close(*storp);
				*storp   = exades;
				tapetype = 0;                   /* disk is not output device  */
				return -1;
			} 
			exades = open(out_file,O_RDWR);
			wprint("%s %s\n",discout,out_file);
			close(*storp);
			*storp    = exades;
			tapetype  = 3;                       /* disk is output device  */
			outfileno = n;
		} 
	}
	free(tp);
	return 0;
}