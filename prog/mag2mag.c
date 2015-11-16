/**************************************************************************** 
 
     mag2mag is based on (identical) mtcp.c
     copy tape to tape, preserving file marks                 
     original version 1.1 J.R. Hughes LLNL 10/93 
 
     usage: mag2mag [-aAs] [-b x] [-B x] [-f x] [-h x] [-n x] inTape [outTape]  
 
     -a     leave output tape at position suitable for                
             appending further files def=FALSE                        
     -A     position output tape for appending prior to any           
             writes def=FALSE                                         
     -b     input blocksize in bytes def=32768 
     -B     output blocksize in bytes def=same-as-input 
     -H     prints out help                                           
     -s     swap bytes def=FALSE                                      
     -f x   reads and writes x files default=all 
     -h x   number of file header blocks to skip before starting blocksize  
             conversion default=1                  
     -n x   reads and writes x blocks of data default is all ie end of tape 
 
     if no output is specified mag2mag reads input, and                  
     reports file number and sizes                                    
 
                                  
    *************************************************
    For use with Oslo tapes, write simply:
    mag2mag /dev/rmt/0 /dev/rmt/1 (or opposite sequence)
  


 
 version 1.1 11/93 
-------------------  
  removed maximum number of files (100), and maximum number of 
  blocks to read (0x80000). max for each is now undefined. 
  
  removed default block_size of output (16k), replaced with output 
  block_size of same-as-input 
  
  added -H option 
  
  changed default buffer size IBUFSIZ to 32768. 
  added -bB options to choose input/output block sizes 
 
  3/94 - added close and reopen input file on EOF on input, so reading 
  beyond 2Gb on input is possible 
 
  8/94 - added read retry (READ_ERR) 


version 2.0 February 2001:
--------------------------
Adopted to the Oslo Cyclotron Laboratory. Removed close and open
at EOF (since else a rewind starts). Included a "jump over filemark" on 
in-file. Defaults is 32768 blocks. We call 1 (Oslo)record = 4x32768 bytes.
Since exabyte records are only 32768, we read 4 times per (Oslo)records.
(These things are because our VME system operates with buffers of 32kWords
=4x32kbytes.)
 
****************************************************************************/ 
 
#include <stdio.h> 
#include <string.h> 
#include <fcntl.h>               /* header for open/read/write */ 
#include <errno.h> 
#include <time.h>                /* header for system time */ 
#include <stdlib.h>
#include <unistd.h>
 
#include <sys/types.h>           /*                                  */ 
#include <sys/mtio.h>            /* header files for tape operations */ 
#include <sys/ioctl.h>           /*                                  */ 
 
#define FALSE              0 
#define TRUE               1 
 
#define READ_ERR          10     /* number of read retrys before exit */ 
 
#define IBUFSIZ        32768     /* default size of input buffer=32768 */ 
#define DEF_BLK_SZ     32768     /* default block size for read and write */ 


	struct mtop 	mt_command;
	struct mtget 	mt_status;
	char		msg1[1024] = "Start at  File: ";
 
static char ibuffer[IBUFSIZ];     /* input buffer */ 
static int  n_read    =    0;     /* number of bytes read */ 
static int  cibuffer  =    0;     /* number of bytes in ibuffer */ 
 
int     i_file, o_file;           /* I/O file descriptors */ 
int     num_files         = 0;    /* number of files read */ 
int     tot_num_blks      = 0;    /* total number of blocks read */ 
int     fil_num_blks      = 0;    /* number of blocks read from file */ 
int     irecords          = 0;    /* # records = # blocks / 4 */

unsigned long tot_num_byt = 0;   /* total number of bytes read */ 
unsigned long fil_num_byt = 0;   /* number of bytes read from file */ 
 
int      OUTPUT = FALSE;         /* output to file flag */ 
int      APPEND = FALSE;         /* future-append flag */ 
 
void t_exit(int cond); 
void t_print(int cond); 
int  t_ioctl(int fd, short request, daddr_t count); 
void t_help(int cond); 
 
char *datim(void); 
void swapbytes(void); 
 
/******************************* main ****************************** 
 *******************************************************************/ 
 
int main(int argc, char *argv[]) { 
 
  char *version = "version 2.0"; 
 
  int     files_to_read;           /* max number of files to read */ 
  int     blks_to_read;            /* max number of bufs to read */ 
 
  int     num_eof    =       0; /* number of consecutive EOFs read */ 
  int     num_read_err =     0; /* number of consecutive read errors */ 
  int     in_blk_sz  =       0; /* default block size for reads    */ 
  int     out_blk_sz =       0; /* default block size for writes   */ 
  int     block_skip =       1; /* number of blocks at bof to skip */ 
                                /* before starting bs conv def=1   */ 
 
  int     OPTARG  = FALSE;         /* argument following option */ 
 
  int     FILOPT  = FALSE;         /* number of files specified */ 
  int     BLOOPT  = FALSE;         /* number of blocks specified */ 
  int     POSAPP  = FALSE;         /* pos tape for append prior to write */ 
  int     SWAPBYT = FALSE;         /* byte swap flag */ 
  int     BLOCON  = FALSE;         /* block conversion flag */ 
 
  int i;                           /* loop variable */ 
 
  char c;                          /* command-line option char */ 
 
  char *in_file,*out_file;         /* file names */ 
 
/* check commmand line args data file must be passed on command line */ 
 
  while (--argc > 0 && (*++argv)[0] == '-') { 
    while ( (c = *++argv[0]) && !OPTARG) { 
      switch (c) { 
 
      case 'a': 
	APPEND = TRUE;          /* position tape for append after writting */ 
	break; 
 
      case 'A': 
	POSAPP = TRUE;          /* position tape for append prior to write */ 
	break; 
 
      case 'H': 
	t_help(0); 
	break; 
 
      case 's': 
	SWAPBYT = TRUE;         /* byte swap flag */ 
	break; 
 
      case 'b': 
	in_blk_sz = atoi(*++argv);    /* blocksize of input */ 
	--argc; 
	OPTARG = TRUE;          /* next argv is argument to option */ 
	break; 
 
      case 'B': 
	out_blk_sz = atoi(*++argv);    /* blocksize of output */ 
	--argc; 
	OPTARG = TRUE;          /* next argv is argument to option */ 
	break; 
 
      case 'f': 
	files_to_read = atoi(*++argv); /* number of files to read */ 
	--argc; 
	FILOPT = TRUE;          /* set file option flag */ 
	OPTARG = TRUE;          /* next argv is argument to option */ 
	break; 
 
      case 'h': 
	block_skip = atoi(*++argv); /* number of file header blocks to */ 
	--argc;                     /* skip before starting blocksize */ 
	OPTARG = TRUE;              /* conversion */ 
	break; 
 
      case 'n': 
	blks_to_read = atoi(*++argv);  /* number of blocks to read */ 
	--argc; 
	BLOOPT = TRUE;          /* set block option flag */ 
	OPTARG = TRUE;          /* next argv is argument to option */ 
	break; 
 
      default: 
        printf("\nmag2mag: illegal option %c\n", c); 
        argc =0; 
        break; 
      } 
    } 
    OPTARG = FALSE;                /* clear option argument flag */ 
  } 
 
  switch (argc) { 
 
  case 2:                 /* open output file */ 
    OUTPUT = TRUE; 
    out_file=*(argv+1); 
    if ((o_file = open(out_file,O_WRONLY,0)) == -1) { 
      printf("mag2mag: cannot open %s\n", out_file); 
      exit(1); 
    } 
    /* fall through */ 
 
  case 1:                 /* open input file */ 
 
    in_file=*argv; 
    if ((i_file = open(in_file,O_RDONLY,0)) == -1) { 
      printf("mag2mag: cannot open %s\n", in_file); 
      if (OUTPUT) close(o_file); 
      exit(1); 
    } 
    break; 
 
  default: 
    t_help(1); 
    break; 
  } 
 
  /* determine block sizes for input and output, and set BLOCON */ 
 
  if (out_blk_sz && out_blk_sz != in_blk_sz) { 
    BLOCON = TRUE; 
    if(!in_blk_sz) in_blk_sz = DEF_BLK_SZ; 
  } 
  else { 
    BLOCON = FALSE; 
    if(!in_blk_sz) in_blk_sz = DEF_BLK_SZ; 
    if(!out_blk_sz) out_blk_sz = DEF_BLK_SZ; 
  } 
 
  /* position tape at EOM if requested. Give error if fail */ 
 
  if (OUTPUT && POSAPP)  
    if (t_ioctl(o_file,MTEOM,1) && t_ioctl(o_file,MTNBSF,1)) t_exit(8);  
 
  printf("mag2mag: %s started %s", version, datim()); 
  mt_command.mt_op = MTNOP;
  if (ioctl( i_file, MTIOCTOP, &mt_command) == -1) {
  return -1;
  }  
  if (ioctl( i_file, MTIOCGET, (char *)&mt_status) == -1) {
  return -1;
  } 

  printf("mag2mag: %s%d\n", msg1, mt_status.mt_fileno); 

  /* main control loop - if neither FILOPT or BLOOPT are TRUE, continue 
   * loop until EOM induced break occurs. If either are TRUE, then evaluate 
   * file or block counters to determine whether to continue 
   */ 
  while( (!FILOPT || (num_files   < files_to_read)) &&  
	 (!BLOOPT || (tot_num_blks < blks_to_read )) ) {  
 
    n_read = read(i_file, &ibuffer[cibuffer], in_blk_sz); 
 
    if (n_read > 0) { 
 
      if(SWAPBYT) swapbytes(); 
 
      cibuffer += n_read;        /* update buffer counter */ 
      num_read_err = 0;          /* successful read so clear */ 
      num_eof = 0;               /* EOF and error counter */ 
 
      /* if no block_size specified for output, or if block number is less 
       * than number of blocks to skip, then write out buffer, else go to 
       * check of buffer length */ 
 
      if ( (fil_num_blks < block_skip) || !BLOCON) { 
	if (OUTPUT && (write(o_file, ibuffer, n_read)) != n_read) 
	  t_exit(2); 
 
	cibuffer -= n_read;      /* update buffer counter */ 
      } 
 
      /* if cibuffer is equal or greater than requested output blocksize 
       * then write out as many blocks as possible */ 
 
      else if (cibuffer >= out_blk_sz) { 
	while (cibuffer >= out_blk_sz) { 
	  if (OUTPUT && (write(o_file, ibuffer, out_blk_sz)) != out_blk_sz) 
	    t_exit(3); 
 
	  cibuffer -= out_blk_sz; 
	} 
 
	if (!cibuffer)               /* normalise buffer */ 
	  for (i=0; i<cibuffer; i++) 
	    ibuffer[i] = ibuffer[i+out_blk_sz]; 
      } 
 
      tot_num_byt += n_read;         /* update counting stats */ 
      fil_num_byt += n_read; 
      tot_num_blks++; 
      fil_num_blks++; 
    } 
 
    else if (!n_read) {          /* end-of-file encountered */ 
 
      if (++num_eof == 2) t_exit(1);  /* double EOF encountered*/ 
 
      /* flush buffer, close and reopen output file to reset file offset */ 
      /* counter, and ensure maximum files size (2Gb) is not exceeded  */ 
 
      if (OUTPUT) { 
 
	if ((write(o_file, ibuffer, cibuffer)) != cibuffer) t_exit(4); 
 
	cibuffer = 0; 

/*	close(o_file);  
	if ((o_file = open(out_file,O_WRONLY,0)) == -1) { 
	  printf("mag2mag: cannot open %s\n", out_file); 
	  exit(1);
	} 
*/
        mt_command.mt_op = MTWEOF;
        mt_command.mt_count = 1;
        if (ioctl( o_file, MTIOCTOP, &mt_command) == -1) { /* Write EOF */
        return -1;
        }  


      } 
 
      /* close and reopen input file to reset file offset */ 
      /* counter, and ensure maximum files size (2Gb) is not exceeded  */ 
      /* The next lines result in rewind of tape, therefore corrected below*/ 
/*       close(i_file); 
 
      if ((i_file = open(in_file,O_RDONLY,0)) == -1) { 
	printf("mag2mag: cannot open %s\n", in_file); 
	exit(1);
      } 
*/

/*Magne trying to correct, begin*/
      mt_command.mt_op = MTNOP;
      if (ioctl( i_file, MTIOCTOP, &mt_command) == -1) {
      return -1;
      }  
      if (ioctl( i_file, MTIOCGET, (char *)&mt_status) == -1) {
      return -1;
      } 
      irecords=mt_status.mt_blkno/4;
      printf("mag2mag: File %3d completed, %10u bytes (%d records) \n", num_files, fil_num_byt,irecords); 
      printf("mag2mag: %s%d\n", msg1, mt_status.mt_fileno+1); 
      mt_command.mt_op = MTFSF;
      mt_command.mt_count = 1;
      if (ioctl( i_file, MTIOCTOP, &mt_command) == -1) { /* Jump over filemark */
      return -1;
      }           
/*Magne trying to correct, end*/

      num_files++; 
 
      fil_num_byt = 0;   /* EOF, so clear file stat counters */ 
      fil_num_blks = 0; 
    } 
 
    else {      /* read error */ 
      if ( num_read_err++ >= READ_ERR ) 
	t_exit(6); 
    } 
  } 
 
 /* write EOF for BLOOPT read */ 
  if( BLOOPT && OUTPUT && t_ioctl(o_file, MTWEOF, 1)) { 
    printf("mag2mag: EOM write error\n"); 
    exit(1); 
  } 
 
  t_exit(0); 
} 
 
void t_exit(int cond) { 
 
  switch (cond) { 
 
  case 1:              /* EOM on read */ 
    t_print(cond); 
    cond = 0;          /* set exit condition of 0,1 to 0 */ 
    /* fall through */ 
 
  case 0:              /* no EOM on read */ 
    t_print(cond); 
    break; 
 
  default:             /* some other error */ 
    t_print(cond); 
    break; 
  } 
 
/* print statistics */ 
 
  printf("mag2mag: number of files  = %10d\n", num_files); 
  printf("mag2mag: number of blocks = %10u\n", tot_num_blks); 
  printf("mag2mag: number of bytes  = %10u\n", tot_num_byt); 
 
 /* write 2nd EOF */ 
 
  if(OUTPUT && t_ioctl(o_file, MTWEOF, 1)) { 
    printf("mag2mag: EOM write error\n"); 
    exit(1); 
  } 
 
 /* backspace over last EOF, to enable appending */ 
 
  if(OUTPUT && APPEND && t_ioctl(o_file, MTNBSF, 1)) { 
    printf("mag2mag: positioning error\n"); 
    exit(1); 
  } 
 
  close(i_file); 
  close(o_file); 
   
  exit(cond); 
} 
 
int t_ioctl(int fd, short request, daddr_t count) { 
 
  struct mtop mt_com; 
 
  mt_com.mt_op = request;   /* operation to be performed */ 
  mt_com.mt_count = count;  /* count of operations */ 
 
  return ioctl(fd, MTIOCTOP, &mt_com); 
} 
 
void t_print(int cond) { 
 
  static char *text[] = { 
/* 0 */    "complete", 
/* 1 */    "EOM encountered", 
/* 2 */    "header block write error", 
/* 3 */    "data block write error", 
/* 4 */    "flush write error", 
/* 5 */    "EOF write error", 
/* 6 */    "read error", 
/* 7 */    "EOM write error", 
/* 8 */    "tape positioning error" 
  }; 
 
  printf("%s %s\n", "mag2mag:", text[cond]); 
  return; 
} 
 
void t_help(int cond) { 
 
  printf("\nusage: mag2mag [-aAHs] [-b x] [-B x] [-f x] [-h x] [-n x]");  
  printf(" intape [outtape]\n"); 
 
  if (cond == 1) { /* illegal command line syntax */ 
    printf("type   mag2mag -H for help\n\n"); 
  } 
 
  else { /* -H option specifies */ 
    printf("\n -a   at the end of the current operation, -a leaves the"); 
    printf(" output tape at a\n\t position suitable for appending further"); 
    printf(" files default=FALSE\n"); 
    printf(" -A   position output tape for appending prior to any"); 
    printf(" writes default=FALSE\n"); 
    printf(" -b x input block size = x (in bytes) default=32768\n"); 
    printf(" -B x output block size = x (in bytes) default=same-as-input\n"); 
    printf(" -H   prints out this text\n"); 
    printf(" -s   swap bytes prior to output default=FALSE\n"); 
    printf(" -f x reads and writes x files default=all\n"); 
    printf(" -h x number of file header blocks to skip before"); 
    printf(" starting blocksize conversion\n\t default=1\n"); 
    printf(" -n x reads and writes x blocks of data"); 
    printf(" default=all ie end of tape\n\n"); 
    printf(" if no outtape is specified mag2mag reads input, and"); 
    printf(" reports file number and sizes\n\n"); 

    printf(" *************************************************\n");
    printf(" For use with Oslo tapes, write simply:\n");
    printf(" mag2mag /dev/rmt/0 /dev/rmt/1 (or opposite sequence)\n\n");

  } 
  exit(cond); 
} 
 
char *datim(void) { 
   
  time_t tm; 
  tm = time(NULL); 
  return (ctime(&tm)); 
} 
 
void swapbytes(void) { 
 
  char tmp;                        /* temp byte for swap */ 
  int i; 
 
  for(i = cibuffer; i < cibuffer+n_read; i += 2) { 
    tmp          = ibuffer[i]; 
    ibuffer[i]   = ibuffer[i+1]; 
    ibuffer[i+1] = tmp; 
  } 
} 
