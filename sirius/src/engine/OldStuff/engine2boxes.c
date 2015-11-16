/* A communication program between RTPC VME acquisition computer and the   */
/* Sun SparcStation (lynx). The program fetches buffer 1 and 2 according to*/
/* semaphors given from the RTPC message box via the bit3 interface        */
/* Created  26 July      1994 by Tore Ramsoy      (for FIC)                */
/* Modified 16 December  1998 by Magne Guttormsen (for RTPC)               */

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
   int       exades;                /* File descriptor for Exabyte*/
   int       opendes;               /* File descripter of opened file*/
   int       new_des_min = 10;      /* Offset for new file descriptor ?*/
   int       write_data = 0;        /* Write raw data to tape or not*/
   ssize_t   bytes_written;
   off_t     offset=0;
   int       nbytes;
   int       bufs=BUFFER_LENGTH;
   int       chan;                  /* VME bit3 link device channel*/
   int       *bufp;                 /* Pointer to databuffer in shared memory */
   int       *messp;                /* Pointer to message_box in shared memory*/
   int       *buf1p, *buf2p, *buf3p, *buf4p;

   u_long    *pADDRESS;             /* Address for eventbuffers */
   u_long    *pLENGTH;              /* Length of buffers*/
   u_long    *pSEM1;                /* Pointer to semaphore 1*/
   u_long    *pSEM2;                /* Pointer to semaphore 2*/
   u_long    *pVMESTATUS;           /* Pointer to VME: ready(1) or not(0)*/
   u_long    *pSUNSTATUS;           /* Pointer to SUN: ready(1) or not(0)*/

   u_long buffer1_address, buffer2_address;
   u_long sem1_address, sem2_address;
   u_long vmestatus_address, sunstatus_address;

   u_long ADDRESS;
   u_long LENGTH;
   u_long SEM1;
   u_long SEM2;
   u_long vmeSTATUS;
   u_long sunSTATUS;


   int       record_count=0;

   int       *sortp;               /* Pointer to message_box(0) - sort flag    */
   int       *engp;                /* Pointer to message_box(1) - engine flag  */
   int       *stgp;                /* Pointer to message_box(2) - storage      */
   int       *vmesp;               /* Pointer to message_box(3) - VME status   */
   int       *recp;                /* Pointer to message_box(5) - record count */
   int       sort_off      = 0;    /* Offset for pointer to message_box(0)     */
   int       engine_off    = 1;    /* Offset for pointer to message_box(1)     */
   int       stgflag_off   = 2;    /* Offset for pointer to message_box(2)     */
   int       vmes_off      = 3;    /* Offset for pointer to message_box(3)     */
   int       reccount_off  = 5;    /* Offset for pointer to message_box(5)     */
   int       part_offset   = 8192; /* Pointer off in databuffer part           */

   char err1[1024] = "ERROR: ENGINE: Cannot start acq_engine, lock file present ";
   char err2[1024] = "ERROR: ENGINE: Get semaphore identifier failed";
   char err3[1024] = "ERROR: ENGINE: Initialise VME front-end system failed";
   char err4[1024] = "ERROR: ENGINE: Shared data buffer memory allocation failed";
   char err5[1024] = "ERROR: ENGINE: Attach shared data buffer to process failed";
   char err6[1024] = "ERROR: ENGINE: Shared message_box memory allocation failed";
   char err7[1024] = "ERROR: ENGINE: Attach shared message_box to process failed";
   char err8[1024] = "ERROR: ENGINE: Could not position to status location";
   char err9[1024] = "ERROR: ENGINE: Could not set RUNNING status";
   char err10[1024]= "ERROR: ENGINE: Could not position to VMEbus message_box   ";
   char err11[1024]= "ERROR: ENGINE: Could not read data from VMEbus message_box";
   char err12[1024]= "ERROR: ENGINE: lseek failed";
   char err13[1024]= "ERROR: ENGINE: Error getting VME sempahore";
   char err14[1024]= "ERROR: ENGINE: Error reading VME databuffer 1";
   char err15[1024]= "ERROR: ENGINE: Error resetting VME sempahore";
   char err16[1024]= "ERROR: ENGINE: Set Sort Semaphore to Zero Failed";
   char err17[1024]= "ERROR: ENGINE: Writing databuffer to tape failed";
   char err18[1024]= "ERROR: ENGINE: Could not position to VME status location  ";
   char err19[1024]= "ERROR: ENGINE: Could not set STOPPED status";
   char err20[1024]= "ERROR: ENGINE: Could not detach databuffer shared memory  ";
   char err21[1024]= "ERROR: ENGINE: Could not detach message_box shared memory ";
   char err22[1024]= "ERROR: ENGINE: Could not remove lock-file";
   char err23[1024]= "ERROR: ENGINE: Cannot open bit3 device driver";
   char err24[1024]= "ERROR: ENGINE: Cannot generate file name";
   char err25[1024]= "ERROR: ENGINE: Unknown MESSAGE_ADDRESS in buffer_defs.h   ";
   char err26[1024]= "ERROR: ENGINE: Error reading VME databuffer 2";



   pADDRESS = &ADDRESS;
   pLENGTH=&LENGTH;
   pSEM1=&SEM1;
   pSEM2=&SEM2;
   pVMESTATUS=&vmeSTATUS;
   pSUNSTATUS=&sunSTATUS;




   arg.buf = (void *) malloc (100);


   /* MESSAGE_ADDRESS = 0x201FFFD0 is for FIC  using A24 (buffer uses A24) */
   /* MESSAGE_ADDRESS = 0x00850000 is for RTPC using A24 (buffer uses A32) */
   if(MESSAGE_ADDRESS == 0x201FFFD0) {
      printf("You use the FIC cpu. Please, copy: \n");
      printf("/local/sirius+/src/engine/engineFIC.c -> engine.c \n");
      printf("Type (as root): make engine \n");
      printf("Type (as root): make install \n");
      printf("To go back to RTPC, please copy back again: \n");
      printf("/local/sirius+/src/engine/engineRTPC.c -> engine.c etc...\n");
   }

   /* Check if the engine lock-file is present, create if not*/
   /* This is a security precaution, multiple engine process causes system crash*/

if(fd=open("/usr/local/sirius+/system/engine.lock",O_CREAT|O_EXCL,PERMS)==-1)
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
   for (j = 0; j < 32768; j++)  {
      *bufp = 0;
      bufp++;
   }
   bufp = shm1ptr;                                   /* Restore pointer */


   /* ---------------------------------------------------------------------*/
   /* Get the VMEbus message_boxes, defines eventsbuffer address in VME A32*/
   /* WARNING: This address is dynamically allocated by the RTPC           */
   /* ---------------------------------------------------------------------*/

   if (lseek(chan, MESSAGE_ADDRESS, SEEK_SET) == -1) {
      printf("%s\n",err10);
      exit(errno);
   }

   if (read(chan, pADDRESS, 4) != 4) {
      printf("%s\n",err11);
      exit(errno);
   }

   if (lseek(chan, MESSAGE_ADDRESS+4, SEEK_SET) == -1) {
      printf("%s\n",err10);
      exit(errno);
   }
   if (read(chan, pLENGTH, 4) != 4) {
      printf("%s\n",err11);
      exit(errno);
   }

   buffer1_address   = ADDRESS;
   buffer2_address   = buffer1_address + BUFFER_LENGTH;
   sem1_address      = buffer2_address + BUFFER_LENGTH;
   sem2_address      = sem1_address + 4;
   vmestatus_address = sem2_address + 4;
   sunstatus_address = vmestatus_address + 4;

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


   /* Signal to VME front-end system that the acquisition is running    */
   /* This is done by setting the VME Message_Box(x) to '1'             */

   if (lseek(chanbuf, sunstatus_address, SEEK_SET) == -1) {
      printf("%s\n",err8);
      exit(errno);
   }
   sunSTATUS = 1;
   if (write(chanbuf, pSUNSTATUS, 4) != 4) {
      printf("%s\n",err9);
      exit(errno);
   }

   if (lseek(chanbuf, vmestatus_address, SEEK_SET) == -1) {
      printf("%s\n",err8);
      exit(errno);
   }
   if (read(chanbuf, pVMESTATUS, 4) != 4) {
      printf("%s\n",err9);
      exit(errno);
   }

   if (vmeSTATUS == 1) {
      *vmesp = 1;                     /* VME system is in running state */
   }else{
      *vmesp = 0;
   }

   /* Write status to file /help/status.tmp */
   strm = fopen("/usr/local/sirius+/help/vmestatus.tmp","w");
   if (vmeSTATUS == 1 ) {
      fprintf(strm," VME system status\t\t: RUNNING\n");
   }
   else if (vmeSTATUS == 0 ) {
      fprintf(strm," VME system status\t\t: STOPPED\n");
   }else {
      fprintf(strm," VME system status\t\t: UNKNOWN ?\n");
   }
   fprintf(strm, " Buffer1 address\t\t: %x (hex) \n", buffer1_address);
   fprintf(strm, " Buffer length\t\t: %x (hex) \n\n", LENGTH);
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

   printf("\n Device number for VME A24 message box  : %d",chan);
   printf("\n Device number for VME A32 buffers+box  : %d",chanbuf);
   printf("\n Buffer 1 starts at                     : 0x%x",buffer1_address);
   printf("\n Buffer 2 starts at                     : 0x%x",buffer2_address);
   printf("\n Start of buffer address (A24messagebox 0) : 0x%x",ADDRESS);
   printf("\n Buffer length (words)   (A24messagebox 1) : 0x%x",LENGTH);
   printf("\n Semaphore 1             (A32messagebox 0) : %d",SEM1);
   printf("\n Semaphore 2             (A32messagebox 1) : %d",SEM2);
   printf("\n VME-CPU ready or not    (A32messagebox 2) : %d",vmeSTATUS);
   printf("\n SUN-CPU ready or not    (A32messagebox 3) : %d\n",sunSTATUS);

   for(;;) {                               /*MAIN DATA ACQUISITION LOOP*/
      if ( *engp == 9 ) goto exitengine;   /*STOP COMMAND FROM MASTER  */

/***********************************************************************/
/*                        B U F F E R  # 1                             */
/***********************************************************************/
/* 1. Check if buffer # 1 is ready                  */
/*    Loop on VME "semaphore" #1 until it becomes 1 */
/*    The call to usleep suspends the process 10 ms */

      for (;;) {

/*

         if (lseek(chanbuf, vmestatus_address, SEEK_SET) == -1) {
            printf("%s\n",err10);
            exit(errno);
         }
         if (read(chanbuf, pVMESTATUS, 4) != 4) {
            printf("%s\n",err11);
            exit(errno);
         }
         if ( vmeSTATUS == 1) {
            *vmesp = 1;                  
         }else {
            *vmesp = 0;
         }

*/


         if (lseek(chanbuf, sem1_address, SEEK_SET) == -1) {
            printf("%s\n",err12);
            exit(errno);
         }
         if (read(chanbuf, pSEM1, 4) != 4) {
            printf("%s\n",err13);
            exit(errno);
         }
         if (SEM1 == 1 ) break;	/* OK buffer ready */
         if ( *engp == 9) goto exitengine;
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
      if (lseek(chanbuf, sem1_address, SEEK_SET) == -1) {
         printf("%s\n",err12);
         exit(errno);
      }

      SEM1=0;
      if (write(chanbuf, pSEM1, 4) != 4) {
         printf("%s\n",err15);
         exit(errno);
      }

/* 5. Activate sorting task                            */
/*    This is done by setting the sort semaphore to 0  */
      *sortp  = 0;              /* Remove disrupt flag */
      semno   = 0;
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

/*
         if (lseek(chanbuf, vmestatus_address, SEEK_SET) == -1) {
            printf("%s\n",err10);
            exit(errno);
         }
         if (read(chanbuf, pVMESTATUS, 4) != 4) {
            printf("%s\n",err11);
            exit(errno);
         }
         if ( vmeSTATUS == 1) {
            *vmesp = 1;                    
         }else {
            *vmesp = 0;
         }

*/

         if (lseek(chanbuf, sem2_address, SEEK_SET) == -1) {
            printf("%s\n",err12);
            exit(errno);
         }
         if (read(chanbuf, pSEM2, 4) != 4) {
            printf("%s\n",err13);
            exit(errno);
         }
         if (SEM2 == 1 ) break;	/* OK buffer ready */
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
      if (lseek(chanbuf, sem2_address, SEEK_SET) == -1) {
         printf("%s\n",err12);
         exit(errno);
      }

      SEM2 = 0;
      if (write(chanbuf, pSEM2, 4) != 4) {
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
   if (lseek(chanbuf, sunstatus_address, SEEK_SET) == -1) {
      printf("%s\n",err18);
      exit(errno);
   }
   sunSTATUS = 0;
   if (write(chanbuf, pSUNSTATUS, 4) != 4) {
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
