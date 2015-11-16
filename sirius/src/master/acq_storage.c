#include	<stdio.h>
#include	<errno.h>
#include	<fcntl.h>
#include	<string.h>
#include	<ctype.h>

#include	<sys/types.h>           /*                                  */ 
#include	<sys/mtio.h>            /* header files for tape operations */ 
#include	<sys/ioctl.h>           /*                                  */ 

#include	<buffer_defs.h>
#define		PERMS 0666              /* read/write for owner user and others */

/* Define global variables */    
int		exades;			/* File descriptor for Exabyte */
int     semid;			/* Semaphore set ID */
int		*messp;			/* Pointer to shared memory message_box */
int     outfileno, tapetype;
int     created;

int acq_storage( int device )
{
	int		stor_off = 2;
	int		*storp;
	char	none[1024]="Data storage NONE selected";
	char	exb1[1024]="Data storage on Exabyte 1 (dev/rmt/0) selected";
	char	exb2[1024]="Data storage on Exabyte 2 (dev/rmt/1) selected";
	char	err[1024]="*** ERROR *** Could not open Exabyte device file";
	char	err1[1024]="*** ERROR *** Could not close Exabyte 1 device file";
	char	err2[1024]="*** ERROR *** Could not close Exabyte 2 device file";
	char	errd[1024]="*** ERROR *** Could not open disk file: sirius_xxx";
	char	discout[1024]="Data output to disk selected, filename:  ";
	int		n, created;
	char	s1[4];
	char	out_file[128];          /* file name from disk sirius_0, sirius_1, ... */

	storp = messp + stor_off;

	if ( device == 0 ) {
		if ( exades > 0 ) {		 
		   wprint("Closing files/rewinding...\n");
			sleep(1);
			if ( close( *storp ) == -1) {
				errprint("%s\n",err1);
			}
		}        
		*storp = -1;
		exades = 0;
		tapetype = 0;               /*included by magne*/
		wprint("%s\n",none);
	} 

	if ( device == 1 ) {
		if ( exades > 0){
			wprint("Closing files/rewinding...\n");
			sleep(1);
			if ( close(exades) == -1) {
		      errprint("%s\n",err1);
			}
		}
		if ((exades = open("/dev/rmt/0n",O_RDWR)) == -1) {
			errprint("%s\n",err);
		   *storp = -1;
		   exades = 0;
			return -1;
			}  else {
				wprint("%s\n",exb1);
				*storp = exades;
				return 0;
			}
		}			


	     if ( device == 2 ) {
                if ( exades > 0){
                   wprint("Closing files/rewinding...\n");
                   sleep(1);
                   if ( close(exades) == -1) {
		      errprint("%s\n",err2);
                   }
                }
  	        if ((exades = open("/dev/rmt/1n",O_RDWR)) == -1) {
	           errprint("%s\n",err);
		   *storp = -1;
		   exades = 0;
	           return -1;
                }  else {
	           wprint("%s\n",exb2);
                   *storp = exades;
                   return 0;
                }
             }			
 
	if ( device == 3 ) {
		if ( exades > 0){
			wprint("Closing files/rewinding...\n");
			sleep(1);
			if ( close( exades ) == -1) {
		      errprint("%s\n",err1);
			}
		}
		created = -1;
		for (n = 0; n < 1000; n++){      /*Finding last filenumber used*/
			number2string(n, s1);
			sprintf(out_file,"sirius_%s",s1);
			if ((exades = open(out_file,O_RDWR)) == -1) { /* file non-exists */
				created  = 1;
				break;
			}
			close(exades);
		}
		if ( created == -1){
			errprint("%s\n",errd);
			exades   = 0;
			*storp   = exades;
			tapetype = 0;                   /* disk is not output device  */
			return -1;
		} else {                            /* create a new file sirius_0 */
			if ((exades = creat(out_file,PERMS)) == -1) {
				wprint("Could not create outfile %s\n",out_file);
				exades   = 0;
				close(exades);
				*storp   = exades;
				tapetype = 0;                  /* disk is not output device  */
				return -1;
			} 
			exades = open(out_file,O_RDWR);
			wprint("%s %s\n",discout,out_file);
			*storp    = exades;
			tapetype  = 3;                    /* disk is output device  */
			outfileno = n;
		} 
	}

	if ( exades > 0 ) {
		return 0;
	} else {
		return -1;
	} 
}