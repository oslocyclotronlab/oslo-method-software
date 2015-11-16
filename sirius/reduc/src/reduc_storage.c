#include	<stdio.h>
#include	<errno.h>
#include	<sys/types.h>
#include	<fcntl.h>
#include	<string.h>
#include	<ctype.h>
#include	<sys/ioctl.h>
#include	<sys/mtio.h>
#define		PERMS 0666               /* read/write for owner user and others */

int			*messp;			/* Pointer to shared memory message_box */
int			outfileno;

int reduc_storage( int device, int inout )
{
	int		exades;
	int		stor1_off = 2;
	int		stor2_off = 12;
	int		*stor1p;
	int		*stor2p;
	int     *filerecp;
	int     *typep;
	int     type;
	char	none[1024]="Data I/O NONE selected, Select Exabyte 1/2 before Start";
	char	exb1[1024]="Data I/O from Exabyte 1 (dev/rmt/0) selected";
	char	exb2[1024]="Data I/O from Exabyte 2 (dev/rmt/1) selected";
	char	err[1024]="*** ERROR *** Could not open Exabyte device file";
	char	err1[1024]="*** ERROR *** Could not close Exabyte/Disc device file";
	char	discin[1024] ="Data input from disk selected, filename: ";
	char	discout[1024]="Data output to disk selected, filename:  ";
	char	errd[1024]="*** ERROR *** Could not open any disk file_0-999 in present directory";

     int	n, created;
     char	s1[4];
     char  in_file[128];				/* file name from disk file_0, file_1, ... */
     char	out_file[128];				/* file name from disk file_0, file_1, ... */

     stor1p   = messp + stor1_off;
     stor2p   = messp + stor2_off;
     filerecp = messp + 13;
     typep    = messp + 3;

     if ( device == 0 ) {				/* Device NONE selected */
        if ( inout == 1 ) {				/* Close input device */
           if (*stor1p > 0) {
              wprint("Closing files/rewinding...\n");
              sleep(1);
			  if ( close( *stor1p ) == -1) {
						errprint("%s\n",err1);
			  }
           }
           exades  = 0;
           *stor1p = exades;
           *typep  = 0*10 + (*typep - (*typep /10)*10);
           wprint("%s\n",none);
        } 
        if ( inout == 2 ) {			    /* Close output device */
           if (*stor2p > 0) {
              wprint("Closing files/rewinding...\n");
              sleep(1);
					if ( close( *stor2p ) == -1) {
						errprint("%s\n",err1);
              }
           }
           exades  = 0;
           *stor2p = exades;
           *typep = (*typep/10) * 10;
				wprint("%s\n",none);
			}
     }
 
             
     if ( device == 1 ) {				/* Exabyte 1, /dev/rmt/0 */
        if ( inout == 1 ) {				/* Data input, READ ONLY */
           if (*stor1p > 0) {
              wprint("Closing files/rewinding...\n");
              sleep(1);
              if ( close(*stor1p ) == -1) {
						errprint("%s\n",err1);
              }
				}
				if ((exades = open("/dev/rmt/0n",O_RDONLY)) == -1) {
					errprint("%s\n",err);
					return -1;
				}  else {
					wprint("%s\n",exb1);
					*stor1p = exades;
					return 0;
				}
        }
        if ( inout == 2 ) {				/* Data output, WRITE */
           if (*stor2p > 0){
              wprint("Closing files/rewinding...\n");
              sleep(1);
					if ( close(*stor2p ) == -1) {
						errprint("%s\n",err1);
					}
           }
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

      if ( device == 1 ) {				/* Exabyte 2, /dev/rmt/1 */
        if ( inout == 1 ) {				/* Data input, READ ONLY */
           if (*stor1p > 0){
              wprint("Closing files/rewinding...\n");
              sleep(1);
              if ( close(*stor1p ) == -1) {
						errprint("%s\n",err1);
              }
			}
			if ((exades = open("/dev/rmt/1n",O_RDONLY)) == -1) {
				errprint("%s\n",err);
				return -1;
			}  else {
				wprint("%s\n",exb2);
				*stor1p = exades;
				return 0;
           }
        }
        if ( inout == 2 ) {				/* Data output, WRITE */
           if (*stor2p > 0){
              wprint("Closing files/rewinding...\n");
              sleep(1);
              if ( close(*stor2p ) == -1) {
						errprint("%s\n",err1);
              }
			}
			if ((exades = open("/dev/rmt/1n",O_RDWR)) == -1) {
				errprint("%s\n",err);
				return -1;
			}  else {
	      wprint("%s\n",exb2);
			*stor2p = exades;
			return 0;
			}
		}
	}			


      if ( device == 3 ) {						/* Disk files */
         if ( inout == 1 ) {					/* Data input, READ ONLY */
            if ( *stor1p > 0){
               wprint("Closing files/rewinding...\n");
               sleep(1);
               if ( close( *stor1p ) == -1) {
						errprint("%s\n",err1);
               }
				}
            created = -1;
            for (n = 0; n < 1000; n++){			/*Finding first filenumber used*/
               number2string(n, s1);
               sprintf(in_file,"file_%s",s1);
					exades = open(in_file,O_RDONLY);
               if (exades != -1) {				/* file exists */
                  created  = 1;
                  break;
               }
					close(exades);
            }
            if ( created == -1){
               errprint("%s\n",errd);
               exades  = 0;
					*stor1p = exades;
               type      = *typep; 
               *typep    = 10*0 + ( type - (type/10)*10);  /* disk is not input device  */ 
               return -1;
            } else {
					wprint("%s %s\n",discin,in_file);
               *filerecp = n * 1000000;
					exades = open(in_file,O_RDONLY);
					*stor1p   = exades;
               type      = *typep; 
               *typep    = 10*3 + ( type - (type/10)*10);  /* disk is input device  */
            }  
         }
			
			
         if ( inout == 2 ) {	          /* Data output, WRITE */
            if ( *stor2p > 0){
               wprint("Closing files/rewinding...\n");
               sleep(1);
               if ( close( *stor2p ) == -1) {
						errprint("%s\n",err1);
               }
            }
            created = -1;
            for (n = 0; n < 1000; n++){      /*Finding last filenumber used*/
               number2string(n, s1);
               sprintf(out_file,"reduc_%s",s1);
					exades = open(out_file,O_RDWR);
               if (exades == -1) {					/* file non-exists */
                  created  = 1;
                  break;
               }
					close(exades);
            }
            if ( created == -1){
               errprint("%s\n",errd);
               exades  = 0;
					*stor2p = exades;
               type      = *typep; 
               *typep    = (type/10)*10 + 3;  /* disk is not output device  */
               return -1;
            } else {                            /* create a new file reduc_0 */
               if (creat(out_file,PERMS) == -1) {
                  wprint("Could not create outfile %s\n",out_file);
               } 
               exades = open(out_file,O_RDWR);
					wprint("%s %s\n",discout,out_file);
					*stor2p   = exades;
               type      = *typep;
               *typep    = (type/10)*10 + 3;       /* disk is output device  */
               outfileno = n;
            } 
			}
		if ( exades > 0 ) {
            return 0;
			}  else {
			return -1;
		} 
	}
}