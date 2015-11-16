#include	<stdio.h>
#include	<errno.h>
#include	<fcntl.h>
#include	<string.h>
#include	<ctype.h>

#include	<sys/types.h>           /*                                  */ 
/* #include	<sys/mtio.h>            /* header files for tape operations */ 
#include	<sys/ioctl.h>           /*                                  */ 

int			exades;			/* File descriptor for Exabyte */
int			*messp;			/* Pointer to shared memory message_box */
int			number2string(int n,char *s1);

int offline_storage( int device )
{
	int		*typep;
	int		tapetype_off = 3;
	int		stor_off = 2;
	int		*storp;
	int		*filerecp;
	char	none[1024]="Data input NONE, select Exabyte 1/2 or Disk before start";
	char	exb1[1024]="Data input from Exabyte 1 (dev/rmt/0) selected";
	char	exb2[1024]="Data input from Exabyte 2 (dev/rmt/1) selected";
	char	err[1024] ="*** ERROR *** Could not open Exabyte device file";
	char	err1[1024]="*** ERROR *** Could not close Exabyte/Disk device file";
	char	disk[1024]="Data input from disk selected, filename: ";
	char	errd[1024]="*** ERROR *** Could not open any disk file_0-999 in present directory";

	int		n, created;
	char	s1[4];
	char	in_file[128];					/* file name from disk file_0, file_1, ... */ 
	storp    = messp + stor_off;
	typep    = messp + tapetype_off;
	filerecp = messp + 13;

	if ( device == 0 ) {
		if (exades > 0){
			wprint("Closing files/rewinding...\n");
			sleep(1);
			if ( close( exades ) == -1) {
				errprint("%s\n",err1);
			}
		}
		wprint("%s\n",none);
		exades = 0;
		*storp = exades;
		*typep = 0;
	} 

	if ( device == 1 ) {
      if (exades > 0){
         wprint("Closing files/rewinding...\n");
         sleep(1);
			if ( close( exades ) == -1) {
				errprint("%s\n",err1);
			}
		}
  		if ((exades = open("/dev/rmt/0n",O_RDONLY)) == -1) {
		   exades = 0;
		   errprint("%s\n",err);
     		}  else {
				wprint("%s\n",exb1);
			} 
			*storp = exades;
			*typep = 0;
			if ( exades > 0 ) {
				return 0;
			}  else {
			return -1;
		} 
	}

	if ( device == 2 ) {
		if (exades > 0){
			wprint("Closing files/rewinding...\n");
			sleep(1);
			if ( close( exades ) == -1) {
				errprint("%s\n",err1);
			}
		}
		if ((exades = open("/dev/rmt/1n",O_RDONLY)) == -1) {
			exades = 0;
			errprint("%s\n",err);
		} else {
			wprint("%s\n",exb2);
		} 
		*storp = exades;
		*typep = 0;

		if ( exades > 0 ) {
			return 0;
			}  else {
			return -1;
		} 
	}
		  
	if ( device == 3 ) {
		if (exades > 0){
			wprint("Closing files/rewinding...\n");
			sleep(1);
			if ( close( exades ) == -1) {
				errprint("%s\n",err1);
			}
		}
		created = -1;
		for( n = 0; n < 1000; n++){                  /*Finding first filenumber used*/
			number2string(n, s1);
			sprintf(in_file,"file_%s",s1);
			if ((exades = open(in_file,O_RDONLY)) != -1) {       /* file exists */
				created   = 1;
				break;
				}
				close(exades);
			}
			if(created == -1){
				errprint("%s\n",errd);
				exades = 0;
				*storp = exades;
				*typep = 0;
			} else {
			wprint("%s %s\n",disk,in_file);
				*filerecp = n*1000000;
				*storp    = exades;
				*typep    = 3;                                 /* disk is input device  */
			} 
		if ( exades > 0 ) {
			return 0;
		}  else {
		return -1;
		} 
	}
	return 0;
}
