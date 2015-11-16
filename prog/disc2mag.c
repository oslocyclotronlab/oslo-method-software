#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>    
#include <fcntl.h>                        /* header for open/read/write */ 
#include <errno.h> 
#include <time.h>                             /* header for system time */ 
 
#include <sys/types.h>          
#include <sys/mtio.h>               /* header files for tape operations */ 
#include <sys/ioctl.h>          
#define RECORD_LENGTH  32768     /* default exabyte buffer size = 32768 */ 
#define MAX 80

struct mtop 	mt_command;
struct mtget 	mt_status;

main()
{
   char    *pexa,exa[MAX];                   /* exabyte station to open */
   int     ch1;
   char    *pfilename,filename[MAX];               /* file name to open */
   int     ch2;

   int     i_file, o_file;                      /* I/O file descriptors */ 
   int     num_files         = 0;               /* number of files read */ 
   int     tot_num_blks      = 0;        /* total number of blocks read */ 
   int     fil_num_blks      = 0;    /* number of blocks read from file */ 
   unsigned long tot_num_byt = 0;         /* total number of bytes read */ 
   unsigned long fil_num_byt = 0;     /* number of bytes read from file */
   static char ibuffer[RECORD_LENGTH];                  /* input buffer */ 
   static int  n_read        = 0;               /* number of bytes read */ 
   static int  cibuffer      = 0;         /* number of bytes in ibuffer */

   printf("\n");
   printf("  ________________________________________ \r\n");
   printf(" |                                        |\r\n");
   printf(" |               DISC2MAG 1.0             |\r\n");
   printf(" |                                        |\r\n");
   printf(" |    Program to copy event files from    |\r\n");
   printf(" |        hard disk to Exabyte tape.      |\r\n");
   printf(" |        The files are put on tape       |\r\n");
   printf(" |  with EOF in between each event file.  |\r\n");
   printf(" |  Oslo records are 4x32768 bytes long.  |\r\n");
   printf(" |  Exabyte records are only 32768 bytes, |\r\n");
   printf(" |       so we write on tape 4 times      |\r\n");
   printf(" |              per Oslo record.          |\r\n");
   printf(" |   Do not use files greater than 2Gb.   |\r\n");
   printf(" |   If you mess with files on tape, it   |\r\n");
   printf(" | is no way (in this program) to go back |\r\n");
   printf(" |   to a certain EOF mark on tape: You   |\r\n");
   printf(" |    have to redo everything once more.  |\r\n");
   printf(" |                                        |\r\n");
   printf(" | E-mail  : magne.guttormsen@fys.uio.no  |\r\n");
   printf(" | Created : 20-02-2003                   |\r\n");
   printf(" | Modified:                              |\r\n");
   printf(" |________________________________________|\r\n");
   printf("                                           \r\n");

   pexa=exa; /*set ptr to addr of array*/
   printf(" Enter name of tape station (e.g. /dev/rmt/0): ");
   while ((ch1 = getchar()) != '\n'){
      *pexa = ch1;
      pexa++;
      if (pexa == &exa[MAX])
         break;
   }
   *pexa = '\0';                                  /* terminate with null */

   if ((o_file = open(exa,O_WRONLY,0)) == -1) { 
       printf("disc2mag: cannot open %s\n", exa); 
       exit(1); 
   }
 
   nextfile:

   pfilename=filename;                         /*set ptr to addr of array*/
   printf("\n Enter name of disk file to be copied (e.g. sirius_0) \n or type ^C (to unload tape and exit program): ");
   while ((ch2 = getchar()) != '\n'){
      *pfilename = ch2;
      pfilename++;
      if (pfilename == &filename[MAX])
         break;
   }
   *pfilename = '\0';                             /* terminate with null */

   if ((i_file = open(filename,O_RDONLY)) == -1) { 
       printf("disc2mag: cannot open %s\n", filename); 
       goto nextfile; 
   }

   printf(" Disk file %s  -> tape file %d at %s\n",filename,num_files,exa); 

   for(;;){
      n_read = read(i_file, &ibuffer[cibuffer], RECORD_LENGTH);
      if ( n_read != RECORD_LENGTH ) {
         close(i_file);  /* EOF or EOM */ 
         printf(" disc2mag: file number        = %10d\n", num_files);
         printf(" disc2mag: number of Oslo rec = %10u\n", tot_num_blks/4); 
         printf(" disc2mag: number of blocks   = %10u\n", tot_num_blks); 
         printf(" file_disc2mag: number of bytes    = %10u\n", tot_num_byt); 
         num_files++;
         tot_num_blks=0;
         tot_num_byt=0;


         mt_command.mt_op = MTWEOF;
         mt_command.mt_count = 1;
         if (ioctl( o_file, MTIOCTOP, &mt_command) == -1) { /* Write EOF */
         printf(" Could not write EOF on exabyte (EOM?)\n");
	 printf(" disc2mag: file number        = %10d\n", num_files);
         printf(" disc2mag: number of Oslo rec = %10u\n", tot_num_blks/4); 
         printf(" disc2mag: number of blocks   = %10u\n", tot_num_blks); 
         printf(" file_disc2mag: number of bytes    = %10u\n", tot_num_byt);
         goto stop;

         }  


         goto nextfile;
      }

      if (write(o_file, ibuffer, n_read) != n_read) {
	 printf(" Could not write more data to exabyte (EOM?)\n");
	 printf(" disc2mag: file number        = %10d\n", num_files);
         printf(" disc2mag: number of Oslo rec = %10u\n", tot_num_blks/4); 
         printf(" disc2mag: number of blocks   = %10u\n", tot_num_blks); 
         printf(" disc2mag: number of bytes    = %10u\n", tot_num_byt);
         goto stop;
      }

      tot_num_blks++;
      tot_num_byt=tot_num_byt+n_read;
   }
   stop:

   close(i_file); 
   close(o_file); 

   return;
}
