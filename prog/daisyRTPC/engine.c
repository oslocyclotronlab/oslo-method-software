/* A communication program between RTPC VME acquisition computer and the      */
/* Sun SparcStation (lynx). The program fetches buffer 1 and 2 according to   */
/* semaphors given from the RTPC message box via the bit3 interface           */
/* Created  26 July      1994 by Tore Ramsoy      (for FIC)                   */
/* Modified 16 October   1998 by Magne Guttormsen (for RTPC)                  */
#include    <stdio.h>
#include    <errno.h>
#include    <sys/file.h>
#include    <sys/types.h>
#include    <sys/ipc.h>
#include    <sys/sem.h>
#include    <fcntl.h>
#include    <signal.h>
#include    <sys/shm.h>
#include    <sys/time.h>
#include    <ipc_defs.h>
#include    <buffer_defs.h>
#include    <sys/ioctl.h>
#include    <sys/mtio.h> 
#include    <sys/btio.h>

#define     WAIT10MS 10000                            /* Suspend time = 10 ms */

main()
{
   char     *cpbuf;                      /* These 7 lines for RTPC A32 memory */
   char     *device = BT_DEVNAME;
   char     *bt_devname();
   bt_status_t status_flags;
   int      chanbuf;
   int      unit = 0;
   int      typebuf = BT_AXSRR;

   key_t     skey=SEMAPHORE_KEY;
   int       nsems=NSEM;
   int       nsops=NSEM;
   int       semid;
   int       semno;
   int       semval;
   int       reply; 
   union  semun {
      int val;
      struct semid_ds *buf;
      ushort *array;
   } arg;
   struct sembuf sops[NSEM];
   int       shmid1, shmid2;
   struct shmid_ds *mbuf;
   struct shminfo  *inf; 
   int       shmemsize=BUFFER_LENGTH;
   int       *shm1ptr, *shm2ptr;
   key_t     bkey=SHAREDMEM_KEY;
   key_t     mkey=MESSAGE_KEY;
   int       i, j, c;
   pid_t     mypid;
   pid_t     *pid;
   FILE      *fp, *strm;
   int       fd;
   int       exades;                /* File descriptor for Exabyte            */
   int       opendes;               /* File descripter of opened file         */
   int       new_des_min = 10;      /* Offset for new file descriptor ?       */
   int       write_data = 0;        /* Write raw data to tape or not          */
   ssize_t   bytes_written;
   off_t     offset=0;
   int       nbytes;
   int       bufs=BUFFER_LENGTH;
   int       chan;                  /* VME bit3 link device channel           */
   int       *bufp;                 /* Pointer to databuffer in shared memory */
   int       *messp;                /* Pointer to message_box in shared memory*/
   int       *buf1p, *buf2p, *buf3p, *buf4p;
   u_long    message_length = 40;   /* VME Message_Box is 10 Words long       */
   u_long    *mess_p;               /* Pointer to Message_Box                 */
   u_long    Message_Box[10];       /* Vector to hold Message_Box             */
   u_long    buffer1_address;       /* Data buffer 1 address (variable)       */
   u_long    buffer2_address;       /* Data buffer 2 address (variable)       */

   int       *sem1ptr;              /* Pointer to VME "semaphore" 1           */
   int       *sem2ptr;              /* Pointer to VME "semaphore" 2           */
   int       sem1val, sem2val;
   int       *engstatptr;
   int       engstatval;
   int       record_count=0;

   int       *sortp;               /* Pointer to message_box(0) - sort flag   */
   int       *engp;                /* Pointer to message_box(1) - engine flag */
   int       *stgp;                /* Pointer to message_box(2) - storage     */
   int       *vmesp;               /* Pointer to message_box(3) - VME status  */
   int       *recp;                /* Pointer to message_box(5) - record count*/
   int       sort_off      = 0;    /* Offset for pointer to message_box(0)    */
   int       engine_off    = 1;    /* Offset for pointer to message_box(1)    */
   int       stgflag_off   = 2;    /* Offset for pointer to message_box(2)    */
   int       vmes_off      = 3;    /* Offset for pointer to message_box(3)    */
   int       reccount_off  = 5;    /* Offset for pointer to message_box(5)    */
   int       part_offset   = 8192; /* Pointer off in databuffer part          */

   char err1[1024] = "ERROR: ENGINE: Cannot start acq_engine, lock file present ";
   char err2[1024] = "ERROR: ENGINE: Get semaphore identifier failed            ";
   char err3[1024] = "ERROR: ENGINE: Initialise VME front-end system failed     ";
   char err4[1024] = "ERROR: ENGINE: Shared data buffer memory allocation failed";
   char err5[1024] = "ERROR: ENGINE: Attach shared data buffer to process failed";
   char err6[1024] = "ERROR: ENGINE: Shared message_box memory allocation failed";
   char err7[1024] = "ERROR: ENGINE: Attach shared message_box to process failed";
   char err8[1024] = "ERROR: ENGINE: Could not position to status location      ";
   char err9[1024] = "ERROR: ENGINE: Could not set RUNNING status               ";
   char err10[1024]= "ERROR: ENGINE: Could not position to VMEbus message_box   ";
   char err11[1024]= "ERROR: ENGINE: Could not read data from VMEbus message_box";
   char err12[1024]= "ERROR: ENGINE: lseek failed                               ";
   char err13[1024]= "ERROR: ENGINE: Error getting VME sempahore                ";
   char err14[1024]= "ERROR: ENGINE: Error reading VME databuffer 1             ";
   char err15[1024]= "ERROR: ENGINE: Error resetting VME sempahore              ";
   char err16[1024]= "ERROR: ENGINE: Set Sort Semaphore to Zero Failed          ";
   char err17[1024]= "ERROR: ENGINE: Writing databuffer to tape failed          ";
   char err18[1024]= "ERROR: ENGINE: Could not position to VME status location  ";
   char err19[1024]= "ERROR: ENGINE: Could not set STOPPED status               ";
   char err20[1024]= "ERROR: ENGINE: Could not detach databuffer shared memory  ";
   char err21[1024]= "ERROR: ENGINE: Could not detach message_box shared memory ";
   char err22[1024]= "ERROR: ENGINE: Could not remove lock-file                 ";
   char err23[1024]= "ERROR: ENGINE: Cannot open bit3 device driver             ";
   char err24[1024]= "ERROR: ENGINE: Cannot generate file name                  ";
   char err25[1024]= "ERROR: ENGINE: Unknown MESSAGE_ADDRESS in buffer_defs.h   ";
   char err26[1024]= "ERROR: ENGINE: Error reading VME databuffer 2             ";


   arg.buf = (void *) malloc (100); 
   mess_p  = &Message_Box[0];
   sem1ptr = &sem1val;
   sem2ptr = &sem2val;
   engstatptr = &engstatval;
    
   /* Check if the engine lock-file is present, create if not                   */
   /* This is a security precaution, multiple engine process causes system crash*/
   if(fd=open("/usr/local/sirius+/system/engine.lock",O_CREAT|O_EXCL,PERMS)==-1 )
   {
      printf("%s\n",err1);
      exit(errno);
   }

   /* Get semaphore identifier */
   semid = semget(skey, nsems, PERMS );
   if (semid == -1) {
      printf("%s\n",err2);
      exit(errno);
   }  

   /* Initialize VME front-end system link */
   chan = vme_init();
   if (chan == -1) {
      printf("%s\n",err3);
      exit(errno);
   }

   /* Attach shared data buffer segment to process */
   shmid1 = shmget(bkey, shmemsize, PERMS);
   if (shmid1 == -1) {
      printf("%s\n",err4);
      exit(errno);
   }

   shm1ptr = shmat(shmid1, NULL, 0);
   if (*shm1ptr == -1) {
      printf("%s\n",err5);
      exit(errno);
   } 

   /* Attach shared message_box segment to process */
   shmemsize = 80;
   shmid2 = shmget(mkey, shmemsize, PERMS);
   if (shmid2 == -1) {
      printf("%s\n",err6);
      exit(errno);
   }

   shm2ptr = shmat(shmid2, NULL, 0);
   if (*shm2ptr == -1) {
      printf("%s\n",err7);
      exit(errno);
   } 

   /* Take a copy of the pointers to shared memory segments           */
   bufp  = shm1ptr;          /* Points to databuffer in shared memory */
   messp = shm2ptr;          /* Points to message_box n shared memory */

   /* Pointers used in write to Exabyte, buffer is divided into 4     */
   buf1p = shm1ptr;                 /* Points to part 1 of databuffer */
   buf2p = shm1ptr + part_offset;   /* Points to part 2 of databuffer */
   buf3p = shm1ptr + part_offset*2; /* Points to part 3 of databuffer */
   buf4p = shm1ptr + part_offset*3; /* Points to part 4 of databuffer */

   /* Initialize pointers to message_box variables */
   sortp = messp + sort_off;
   recp  = messp + reccount_off;
   stgp  = messp + stgflag_off;
   engp  = messp + engine_off;
   vmesp = messp + vmes_off;

   /* Clear data buffer segment */
   for (j=0; j < 32768; j++)  {  
      *bufp = 0;
      bufp++;
   } 
   bufp = shm1ptr;                                   /* Restore pointer */

   /* Signal to VME front-end system that the acquisition is running    */
   /* This is done by setting the VME Message_Box(x) to '1'             */
   if (lseek(chan, ENGSTATUS, SEEK_SET) == -1) {
      printf("%s\n",err8);
      exit(errno);
   }

   engstatval = 1;
   if (write(chan, engstatptr, 4) != 4) {
      printf("%s\n",err9);
      exit(errno);
   }


   /* Get the VMEbus Message_box, defines data buffer address in VME A32*/
   /* WARNING: This address is dynamically allocated by the RTPC        */
   if (lseek(chan, MESSAGE_ADDRESS, SEEK_SET) == -1) {
      printf("%s\n",err10);
      exit(errno);
   }

   if (read(chan, mess_p, message_length) != message_length) {
      printf("%s\n",err11);
      exit(errno);
   }

   if ( Message_Box[5] == 1) {
      *vmesp = 1;                     /* VME system is in running state */
   }else {
      *vmesp = 0;
   }

   buffer1_address = Message_Box[0];
   buffer2_address = buffer1_address + BUFFER_LENGTH;

   /* Write status to file /help/status.tmp */
   strm = fopen("/usr/local/sirius+/help/vmestatus.tmp","w");
   if ( Message_Box[5] == 1 ) {
      fprintf(strm," VME system status\t\t: RUNNING\n");
   }
   else if ( Message_Box[5] == 0 ) {
      fprintf(strm," VME system status\t\t: STOPPED\n");
   }else {
      fprintf(strm," VME system status\t\t: UNKNOWN ?\n");
   }
   fprintf(strm, " Buffer1 address\t\t: %x (hex) \n", buffer1_address);
   fprintf(strm, " Buffer length\t\t: %x (hex) \n\n", Message_Box[1]);
   fclose( strm );

   /* Take a copy of the file descriptor for storage unit if used */
   if ( *stgp > 0 ) {
      opendes = *stgp;
      exades = fcntl( opendes, F_DUPFD, new_des_min );
      write_data = 1;
      if ( exades == -1) {
         perror("\n ERROR: Could not copy file descriptor \n");
         write_data = 0;
      }
   } 

   /* Establish the right channel for bit3 according to A24 or A32         */
   /* for the two buffers. RTPC uses A32 and FIC (old CPU) uses A24        */
   /* This is tested by the fixed address of the message box given         */
   /* by the MESSAGE_ADDRESS in /sirius/include/buffer_defs.h              */
   /* MESSAGE_ADDRESS = 0x201FFFD0 is for FIC  using A24 (buffer uses A24) */
   /* MESSAGE_ADDRESS = 0x00850000 is for RTPC using A24 (buffer uses A32) */
   /* I hope this works, Magne                                             */

   if(MESSAGE_ADDRESS == 0x201FFFD0) {
      chanbuf = chan;                   /* Already determined by vme_init()*/
   }
   else if(MESSAGE_ADDRESS == 0x00850000) {
      if((cpbuf = bt_devname(unit, &buffer1_address, typebuf, device)) != NULL) {
         if ((chanbuf = open(cpbuf, O_RDWR)) < 0 ) {
            printf("%s\n",err23);
            exit(errno);
         }
      }else{
         printf("%s\n",err24);
         exit(errno);
      }
      /* Make sure the bit3 adaptor is connected*/
      (void) ioctl(chanbuf, BIOC_CLR_STATUS, &status_flags);
      if (status_flags & BT_STATUS_MASK) {
         bit3_perror(chanbuf);
         exit(errno);
      }
   }else{
       printf("%s\n",err25);
       exit(errno);
   }

   printf("\n Device number for VME message box       %d",chan);
   printf("\n Device number for VME buffers           %d",chanbuf);
   printf("\n Buffer 1 starts at                      0x%x",buffer1_address);
   printf("\n Buffer 2 starts at                      0x%x\n ",buffer2_address);
   printf("\n Start of buffer address (messagebox 0) : 0x%x",Message_Box[0]);
   printf("\n Buffer length (words)   (messagebox 1) : %d",Message_Box[1]);
   printf("\n Semaphore 1             (messagebox 2) : %d",Message_Box[2]);
   printf("\n Semaphore 2             (messagebox 3) : %d",Message_Box[3]);
   printf("\n Not used                (messagebox 4) : %d",Message_Box[4]);
   printf("\n VME-CPU ready or not    (messagebox 5) : %d",Message_Box[5]);
   printf("\n SUN-CPU ready or not    (messagebox 6) : 0x%x",Message_Box[6]);
   printf("\n Not used                (messagebox 7) : %d",Message_Box[7]);
   printf("\n Not used                (messagebox 8) : %d",Message_Box[8]);
   printf("\n Not used                (messagebox 9) : %d",Message_Box[9]);


   for(;;) {                               /*MAIN DATA ACQUISITION LOOP*/
      if ( *engp == 9 ) goto exitengine;   /*STOP COMMAND FROM MASTER  */

/***********************************************************************/
/*                        B U F F E R  # 1                             */
/***********************************************************************/ 
/* 1. Check if buffer # 1 is ready                  */
/*    Loop on VME "semaphore" #1 until it becomes 1 */
/*    The call to usleep suspends the process 10 ms */
       
      for (;;) {
         if (lseek(chan, SEM_1, SEEK_SET) == -1) {
            printf("%s\n",err12);
            exit(errno);
         }
         if (read(chan, sem1ptr, 4) != 4) {
            printf("%s\n",err13);
            exit(errno);
         }
         if (sem1val == 1 ) break;	/* OK buffer ready */
         if (  *engp == 9) goto exitengine;
         usleep(WAIT10MS);
      }

/* 2. Signal the sorting task to disrupt sorting    */
/*    This is done by setting message_box(0) to '3' */
      *sortp = 3;

/* 3. Fetch data buffer #1 from the VMEbus system */
      if (lseek(chanbuf, buffer1_address, SEEK_SET) == -1) {
         printf("%s\n",err12);
         exit(errno);
     }

      if (read(chanbuf, bufp, BUFFER_LENGTH) != BUFFER_LENGTH) {
         printf("%s\n",err14);
         exit(errno);
      }

/* 4. Reset VMEbus semaphore # 1 */
      if (lseek(chan, SEM_1, SEEK_SET) == -1) {
         printf("%s\n",err12);
         exit(errno);
      }

      sem1val=0; 
      if (write(chan, sem1ptr, 4) != 4) {
         printf("%s\n",err15);
         exit(errno);
      }
     
/* 5. Activate sorting task                           */
/*    This is done by setting the sort semaphore to 0 */
      *sortp = 0;              /* Remove disrupt flag */
      semno  = 0;
      arg.val = 0;
      if ( semctl(semid, semno, SETVAL, arg) == -1) {
         printf("%s\n",err16);
         exit(errno);
      }

/* 6. Start dump of buffer 1 to exabyte tape              */
/*    Buffer is segmented into 4 records, each 32 kB long */
      if ( write_data == 1 ) {
         bytes_written = write(exades, buf1p, RECORD_LENGTH);
         bytes_written = write(exades, buf2p, RECORD_LENGTH);
         bytes_written = write(exades, buf3p, RECORD_LENGTH);
         bytes_written = write(exades, buf4p, RECORD_LENGTH);
         if (bytes_written != RECORD_LENGTH) {
            printf("%s\n",err17);
         }
      } 

/* Update record count message_box(9) variable */
      *recp = *recp + 1;

/*********************************************************************/
/*                        B U F F E R  # 2                           */
/*********************************************************************/
/* 1. Check if buffer # 2 is ready                  */
/*    Loop on VME "semaphore" #2 until it becomes 1 */
/*    The call to usleep suspends the process 10 ms */
       
      for (;;) {
         if (lseek(chan, SEM_2, SEEK_SET) == -1) {
            printf("%s\n",err12);
            exit(errno);
         }
         if (read(chan, sem2ptr, 4) != 4) {
            printf("%s\n",err13);
            exit(errno);
         }
         if (sem2val == 1 ) break;	/* OK buffer ready */
         if (  *engp == 9) goto exitengine;
         usleep(WAIT10MS);
      }

/* 2. Signal the sorting task to disrupt sorting of current buffer */
/*    This is done by setting message_box(0) to '3'                */
      *sortp = 3;

/* 3. Fetch data buffer #2 from the VMEbus system */
      if (lseek(chanbuf, buffer2_address, SEEK_SET) == -1) {
         printf("%s\n",err12);
         exit(errno);
      }
      if (read(chanbuf, bufp, BUFFER_LENGTH) != BUFFER_LENGTH) {
         printf("%s\n",err26);
         exit(errno);
      }

/* 4. Reset VMEbus semaphore # 2 */
      if (lseek(chan, SEM_2, SEEK_SET) == -1) {
         printf("%s\n",err12);
         exit(errno);
      }

      sem2val = 0; 
      if (write(chan, sem2ptr, 4) != 4) {
         printf("%s\n",err15);
         exit(errno);
      }
     
/* 5. Activate sorting task                           */
/*    This is done by setting the sort semaphore to 0 */
      *sortp  = 0; 	/* Remove disrupt flag */
      semno   = 0;
      arg.val = 0;
      if ( semctl(semid, semno, SETVAL, arg) == -1) {
         printf("%s\n",err16);
         exit(errno);
      }

/* 6. Start dump of buffer 2 to exabyte tape              */
/*    Buffer is segmented into 4 records, each 32 kB long */
      if ( write_data == 1 ) {
         bytes_written = write(exades, buf1p, RECORD_LENGTH);
         bytes_written = write(exades, buf2p, RECORD_LENGTH);
         bytes_written = write(exades, buf3p, RECORD_LENGTH);
         bytes_written = write(exades, buf4p, RECORD_LENGTH);
 
         if (bytes_written != RECORD_LENGTH) {
            printf("%s\n",err17);
            exit(errno);
         }
      } 

/* Update record count message_box(9) variable */
      *recp = *recp + 1;
   }                           /*END OF MAIN DATA ACQUISITION LOOP*/

   exitengine:

/* Signal to VME front-end system that the acquisition is stopped */
/* This is done by setting the VME Message_Box(x) to '0' */
   if (lseek(chan, ENGSTATUS, SEEK_SET) == -1) {
      printf("%s\n",err18);
      exit(errno);
   }
   engstatval = 0;
   if (write(chan, engstatptr, 4) != 4) {
      printf("%s\n",err19);
      exit(errno);
   }

/* Detach shared memory */
   if ( shmdt( shm1ptr ) == -1) {
      printf("%s\n",err20);
   }
   if ( shmdt( shm2ptr ) == -1) {
      printf("%s\n",err21);
   } 

/* Remove the lock file */
   if ( system("rm -f /usr/local/sirius+/system/engine.lock") == -1) {
      printf("%s\n",err22);
      exit(errno);      
   }
   exit(0);
} 
