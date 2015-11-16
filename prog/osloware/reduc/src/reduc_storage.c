#include        <stdio.h>
#include        <errno.h>
#include        <sys/types.h>
#include        <fcntl.h>
#include	<string.h>
#include	<ctype.h>
#include	<sys/ioctl.h>
#include	<sys/mtio.h>



    	/* Define global variables */    
	int		*messp;			/* Pointer to shared memory message_box */


int reduc_storage( int device, int inout )
{
	int		exades;
        int		stor1_off = 2;
        int		stor2_off = 12;
	int		*stor1p;
	int		*stor2p;
	char		none[1024]="Data I/O NONE selected, Select Exabyte/DLT before Start";
	char		exb1[1024]="Data I/O from Exabyte (dev/rmt/0) selected";
	char		exb2[1024]="Data I/O from DLT (dev/rmt/5) selected";
	char		err[1024]="*** ERROR *** Could not open tape device file";
	char		err1[1024]="*** ERROR *** Could not close tape device file";

	stor1p = messp + stor1_off;
	stor2p = messp + stor2_off;



	     if ( device == 0 ) {			/* Device NONE selected */
                if ( inout == 1 ) {			/* Close input device */
		   if ( close( *stor1p ) == -1) {
		      errprint("%s\n",err1);
		   }
		   *stor1p = 0;
		   wprint("%s\n",none);
		}
                if ( inout == 2 ) {			/* Close output device */
		   if ( close( *stor2p ) == -1) {
		      errprint("%s\n",err1);
		   }
		   *stor2p = 0;
		   wprint("%s\n",none);
		}
             } 


             /* Exabyte, /dev/rmt/0 */
	     if ( device == 1 ) {
                if ( inout == 1 ) {	/* Data input, READ ONLY */
  		   if ((exades = open("/dev/rmt/0n",O_RDONLY)) == -1) {
		      errprint("%s\n",err);
	              return -1;
     		   }  else {
		      wprint("%s\n",exb1);
                      *stor1p = exades;
                      return 0;
                   }
                }

                if ( inout == 2 ) {	/* Data output, WRITE */
  		   if ((exades = open("/dev/rmt/0n",O_RDWR)) == -1) {
		      errprint("%s\n",err);
	              return -1;
     		   }  else {
		      wprint("%s\n",exb1);
                      *stor2p = exades;
                      return 0;
                   }
                }
             }			

             /* DLT, /dev/rmt/5 */
	     if ( device == 2 ) {
                if ( inout == 1 ) {	/* Data input, READ ONLY */
  		   if ((exades = open("/dev/rmt/5n",O_RDONLY)) == -1) {
		      errprint("%s\n",err);
	              return -1;
     		   }  else {
		      wprint("%s\n",exb2);
                      *stor1p = exades;
                      return 0;
                   }
                }

                if ( inout == 2 ) {	/* Data output, WRITE */
  		   if ((exades = open("/dev/rmt/5n",O_RDWR)) == -1) {
		      errprint("%s\n",err);
	              return -1;
     		   }  else {
		      wprint("%s\n",exb2);
                      *stor2p = exades;
                      return 0;
                   }
                }
             }			





}
