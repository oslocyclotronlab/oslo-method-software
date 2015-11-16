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

#include	<offmem_defs.h>
#include	<buffer_defs.h>

#define ATTACH 1
#define DETTAC 2

/* Define global variables */    
	int		exades;			/* File descriptor for Exbyte */
	int		*messp;			/* Pointer to shared memory message_box */

int offline_start( ) 
{
	const char	*startsort = "/Applications/sirius/bin/startoffsort &";

	int		sort_off     = 0;
	int		time_off     = 1;
	int		offl_off     = 4;
	int		recc_off     = 5;
	int		badr_off     = 6;
	int		retstat_off  = 9;
 	int		tapetype_off = 3;
	int		filerec_off  = 13;
	int		filedes_off  = 2;

	int		*sortp;
	int		*timep;
 	int		*offlp;
	int		*reccp;
	int		*badrp;
	int		*retstatp;
	int		*typep;
	int		*filerecp;
	int		*filedesp;

	int			blockfac;
	int			current_file, current_rec;
	long int	bytepos;
	char		s1[4];
	char		in_file[128];         /* file names */ 
	char		err1d[1024] = "Could not open file in present directory: file_";
	char		err2d[1024] = "Could not position to record : ";
	char		msg[1024] = "Off-line sorting started ";
	char		err2[1024]= "*** ERROR *** Start of offline_sort failed"; 

	time_t		*tp;
	time_t		syst;

	tp			= (void *) malloc(4);
	syst		= time(tp);

	typep		= messp + tapetype_off;
	filerecp	= messp + filerec_off;
	sortp		= messp + sort_off;
	timep		= messp + time_off;
	offlp		= messp + offl_off;
	reccp		= messp + recc_off;
	badrp		= messp + badr_off;
	retstatp	= messp + retstat_off;
	filedesp	= messp + filedes_off;

	/* Put message box flags to start values */
	*sortp		= 0;
	*timep		= (int) tp;
	*offlp		= 1;
	*reccp		= 0;
	*badrp		= 0;
	*retstatp	= 0;
      
/* Precaution made for disk-file in case exades not defined */
	if (*typep == 3){
		close(exades);   
		blockfac = REC_IN_BUF;               /* DISK format = SIRIUS format */
		current_file = *filerecp/1000000;
		current_rec  = *filerecp - current_file*1000000;
		number2string(current_file, s1);                     /* Move to File*/
		sprintf(in_file,"file_%s",s1);
		if ((exades = open(in_file,O_RDONLY)) == -1) { /* file do not exist */
			errprint("%s%d\n",err1d,current_file);
			exades    = 0;
			*filedesp = exades;
			return -1;
		} else {
			bytepos = (blockfac*32*1024)*current_rec;
	      if(lseek(exades,bytepos,0) == -1){        /* record do not exist */
				errprint("%s %d\n",err2d,current_rec);
				exades    = 0;
				*filedesp = exades;
				return -1;
			} 
		}
		*filedesp = exades;
	}

/*  Activate the sorting task */
	if ( system( startsort ) == -1) {
		errprint("%s\n",err2);
		return -1;
	}  

	wprint("%s %s",msg,asctime(localtime(tp)));
	return 0;
}
