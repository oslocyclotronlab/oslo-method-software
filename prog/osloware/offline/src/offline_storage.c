#include        <stdio.h>
#include        <errno.h>
#include        <sys/types.h>
#include        <fcntl.h>
#include	<string.h>
#include	<ctype.h>
#include	<sys/ioctl.h>
#include	<sys/mtio.h>



    	/* Define global variables */    
	int		exades;			/* File descriptor for Exabyte */
	int		*messp;			/* Pointer to shared memory message_box */


int offline_storage( int device )
{

        int		stor_off = 2;
	int		*storp;
	char		none[1024]="Data input NONE selected, Select Exabyte/DLT before Start";
	char		exb1[1024]="Data input from Exabyte (dev/rmt/0) selected";
	char		exb2[1024]="Data input from DLT (dev/rmt/5) selected";
	char		err[1024]="*** ERROR *** Could not open tape device file";
	char		err1[1024]="*** ERROR *** Could not close tape device file";

	storp = messp + stor_off;



	     if ( device == 0 ) {
		if ( close( exades ) == -1) {
		   errprint("%s\n",err1);
		}
		*storp = -1;
		exades = 0;
		wprint("%s\n",none);
             } 


	     if ( device == 1 ) {
  		if ((exades = open("/dev/rmt/0",O_RDONLY,0)) == -1) {
		   exades = 0;
		   errprint("%s\n",err);
     		}  else {
		   wprint("%s\n",exb1);
		} 
		*storp = exades;

	        if ( exades > 0 ) {
                   return 0;
	        }  else {
	           return -1;
                } 

             }			


	     if ( device == 2 ) {
  		if ((exades = open("/dev/rmt/5",O_RDONLY,0)) == -1) {
		   exades = 0;
		   errprint("%s\n",err);
     		} else {
		   wprint("%s\n> ",exb2);
		} 
		*storp = exades;

	        if ( exades > 0 ) {
                   return 0;
	        }  else {
	           return -1;
                } 

	     }

        return 0;
}
