/* A communication program between RIO2 VME acquisition computer and the      		*/
/* main computer (tiger). The program fetches buffers  according to         		*/
/* semaphors given from the RIO2 message box via the SBS interface           		*/
/* Created  26 July     1994 by Tore Ramsoy      (for FIC)                   		*/
/* Modified 22 December 1998 by Magne Guttormsen (for RTPC)                  		*/
/* Modified 18 July 	1999 (MG) Eventbuffer moved to SRAM, 4*32kb small buffers	*/
/* Modified 10 April  	2008 (MG) RIO2 with Tiger and SBS VME-PCI bus interface		*/
/* Present version: engine 1.0 should by used with eventbuilder 1.0 or higher  		*/

#include		<stdio.h>
#include		<stdlib.h>
#include		<errno.h>
#include		<sys/file.h>
#include		<sys/types.h>
#include		<sys/ipc.h>
#include		<sys/sem.h>
#include		<fcntl.h>
#include		<signal.h>
#include		<sys/shm.h>
#include		<sys/time.h>
#include		<ipc_defs.h>
#include		<buffer_defs.h>
#include		<sys/ioctl.h>
#include		<sys/mtio.h> 
#include		<ctype.h>
#include		"btapi.h"

bt_error_t      status;        				/* SBS library error return type */
bt_desc_t       btd;            			/* Unit descriptor for SBS library */
int             unit = 0;
bt_dev_t        type = BT_DEV_A24;
char            devname[BT_MAX_DEV_NAME];	/* Device to open */
char            *base_buffer_p = NULL;

int 			vme_open(void);
int 			vme_close(void);
int             usleep(useconds_t usec);	

int main(void) 
{
 	key_t		skey=SEMAPHORE_KEY;
	int			nsems=NSEM;
	int			semid;
	int			semno;

   	union		semun {
      	int 			val;
      	struct 			semid_ds *buf;
      	unsigned short 	*array;
   	} arg;
	int       shmid1, shmid2;
	int       shmemsize=BUFFER_LENGTH;
	int       *shm1ptr, *shm2ptr;
	key_t     bkey=SHAREDMEM_KEY;
	key_t     mkey=MESSAGE_KEY;
	int       i;
	FILE      *strm;
	int       fd;
	int       exades;                /* File descriptor for Exabyte            */
	int       opendes;               /* File descripter of opened file         */
	int       new_des_min = 10;      /* Offset for new file descriptor ?       */
	int       write_data = 0;        /* Write raw data to tape or not          */
	ssize_t   bytes_written;

   	int       	*bufp;                 	/* Pointer to databuffer in shared memory	*/
   	int       	*messp;                	/* Pointer to message_box in shared memory	*/
   	int       	*buf1p, *buf2p, *buf3p, *buf4p;

   	long      	message_length = 40;   	/* VME Message_Box is 10 Words long			*/
   	long      	*mess_p;               	/* Pointer to Message_Box					*/

   	long      	movebytes;            	/* Length (bytes) of movebuffer from RIO3	*/
   	long      	Message_Box[10];      	/* Vector to hold Message_Box				*/
   	unsigned long moveaddress;     		/* Data buffer address in VME				*/
   	long      	*sem1ptr;             	/* Pointer to VME semaphore 1				*/
   	long      	*sem2ptr;             	/* Pointer to VME semaphore 2				*/
   	long      	*sem3ptr;             	/* Pointer to VME semaphore 3				*/
   	long      	*sem4ptr;             	/* Pointer to VME semaphore 4				*/
   	long      	sem1val, sem2val, sem3val, sem4val;
  	long      	*engstatptr;
   	long      	engstatval;
   	long      	semph1, semph2, semph3, semph4, tiger, stbox;

   	int       	*sortp;               	/* Pointer to message_box(0) - sort flag	*/
   	int       	*engp;                	/* Pointer to message_box(1) - engine flag	*/
   	int       	*stgp;                	/* Pointer to message_box(2) - storage		*/
   	int       	*vmesp;               	/* Pointer to message_box(3) - VME status	*/
   	int       	*recp;                	/* Pointer to message_box(5) - record count	*/
   	int       	sort_off      = 0;    	/* Offset for pointer to message_box[0]		*/
   	int       	engine_off    = 1;    	/* Offset for pointer to message_box[1]		*/
   	int       	stgflag_off   = 2;    	/* Offset for pointer to message_box[2]		*/
   	int       	vmes_off      = 3;    	/* Offset for pointer to message_box[3]		*/
   	int       	reccount_off  = 5;    	/* Offset for pointer to message_box[5]		*/
   	int       	part_offset   = 8192; 	/* Pointer off in databuffer part			*/

    size_t		amt_read;				/* Amount of data read from device			*/
 
   	arg.buf 	= (void *) malloc (100); 
   	mess_p  	= &Message_Box[0];
   	sem1ptr 	= &sem1val;
   	sem2ptr 	= &sem2val;
   	sem3ptr 	= &sem3val;
   	sem4ptr 	= &sem4val;
    engstatptr 	= &engstatval;
	
   	/* Check if the engine lock-file is present, create if not. This is */
   	/* a security precaution, multiple engine processes cause system crash*/
   	if((fd = open("/Applications/sirius/system/engine.lock",O_CREAT|O_EXCL,PERMS)) == -1 )
   	{
      	printf("Engine ERROR: Could not start engine due to lock file /Applications/sirius/system/engine.lock\n");
      	exit(errno);
   	}

   	/* Get semaphore identifier */
   	semid = semget(skey, nsems, PERMS );
   	if (semid == -1) {
      	printf("Engine ERROR: Get semaphore identifier failed \n");
      	exit(errno);
   	}  

   	/* Initialize VME front-end system link */
   	if (vme_open() != 0) {
      	printf("Engine ERROR: Could not initialize and open SBS VME-PCI interface\n");
      	exit(errno);
   	}
	
	/* Attach shared message_box segment to process */
   	shmemsize = 80;
   	shmid2 = shmget(mkey, shmemsize, PERMS);
   	if (shmid2 == -1) {
      	printf("Engine ERROR: Shared message_box memory allocation failed\n");
      	exit(errno);
  		}

   	shm2ptr = shmat(shmid2, NULL, 0);
   	if (*shm2ptr == -1) {
      	printf("Engine ERROR: Attach shared message_box to process failed\n");
      	exit(errno);
   	} 
	
   	/* Attach shared data buffer segment to process */
   	shmemsize = BUFFER_LENGTH;
   	shmid1 = shmget(bkey, shmemsize, IPC_CREAT | PERMS);
   	if (shmid1 == -1) {
      	printf("Engine ERROR: Shared data buffer memory allocation failed %d\n", errno);
      	exit(errno);
   	}

   	shm1ptr = shmat(shmid1, NULL, 0);
   	if (*shm1ptr == -1) {
      	printf("Engine ERROR: Attach shared data buffer to process failed\n");
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
   	for (i = 0; i < 32768; i++)  {  
      	*(bufp+i) = 0;
   	} 
   	bufp = shm1ptr;									/* Restore pointer	*/

   	/* Signal to VME front-end system that the acquisition is running	*/
   	/* This is done by setting the VME Message_Box[1] to '1'            */
   	engstatval = 1;
   	status = bt_write(btd, engstatptr, ENGSTATUS, 4, &amt_read);
	if ( BT_SUCCESS != status) { /* At first VME write failure, we print out a lot of errors*/
        if (amt_read != 4) {
            fprintf(stderr, "Engine ERROR: Only %d bytes written to SBS VME-PCI.\n", (int) amt_read);
        }
        else {
            fprintf(stderr, "Engine ERROR: Could not write to %s.\n", &devname[0]);
        }
        bt_perror(btd, status, "Engine ERROR: Could not write engine status to SBS VME-PCI");
        vme_close();
        return EXIT_FAILURE;
    }
   

   	/* Read the whole VME Message_box to get info from bobcat (installed in the VME crate) */
   	status = bt_read(btd, mess_p, MESSAGE_ADDRESS, message_length, &amt_read);
	if ( BT_SUCCESS != status) { /* At first VME read error, we print out a lot of messages*/
        if (amt_read != message_length) {
            fprintf(stderr, "Engine ERROR: Only %d bytes read from device.\n", (int) amt_read);
        }
        else {
            fprintf(stderr, "Engine ERROR: Could not read from %s.\n", &devname[0]);
        }
        bt_perror(btd, status, "Engine ERROR: Could not read VME Message_box from VME A24 memory");
        vme_close();
        return EXIT_FAILURE;
    }

   	if ( Message_Box[6] == 1) {
      	*vmesp = 1;                     /* VME system is in running state */
   	}else {
      	*vmesp = 0;
   	}
   	movebytes = BUFFER_LENGTH/4;      	/* Bufferlength is in bytes */
   	moveaddress = Message_Box[0];		/* Address of VME A24 event buffer */

   	/* Write status to file /help/vmestatus.tmp */
   	strm = fopen("/Applications/sirius/help/vmestatus.tmp","w");
   	if ( Message_Box[6] == 1 ) {
      	fprintf(strm," VME system status\t\t: RUNNING\n");
   	}
   	else if ( Message_Box[6] == 0 ) {
      	fprintf(strm," VME system status\t\t: STOPPED\n");
   	}else {
      	fprintf(strm," VME system status\t\t: UNKNOWN ?\n");
   	}
   	fprintf(strm, " Buffer address\t\t: %lx (hex) \n", moveaddress);
   	fprintf(strm, " Buffer length\t\t: %lx (hex) \n", Message_Box[1]);
	fprintf(strm," ---------------------------------------------\n");
   	fclose( strm );

   	/* Take a copy of the file descriptor for storage unit if used */
   	if (*stgp > 0) {
      	opendes = *stgp;
      	exades = fcntl( opendes, F_DUPFD, new_des_min );
      	write_data = 1;
      	if ( exades == -1) {
         	perror("\n Engine ERROR: Could not copy file descriptor \n");
         	write_data = 0;
      	}
   	}

   	semph1 = Message_Box[2]&0x00000001;               /*Test on the last 4 bits*/
   	semph2 = Message_Box[3]&0x00000001;
   	semph3 = Message_Box[4]&0x00000001;               /*Test on the last 4 bits*/
   	semph4 = Message_Box[5]&0x00000001;
   	tiger  = Message_Box[7]&0x00000001;
   	stbox  = MESSAGE_ADDRESS;
	
	printf("\n --------------------------------------------------------- \n");
   	printf(  "|                    E N G I N E 1.0                      |\n");
	printf(  "|                     Apr. 10. 2008                       |\n");
	printf(  "|---------------------------------------------------------|\n");
   	printf(  "| VME message box starts at                  : 0x%8lx |\n",stbox);
   	printf(  "| Buffer starts at                           : 0x%8lx |\n",moveaddress);
   	printf(  "| Start of buffer address     (messagebox 0) : 0x%8lx |\n",Message_Box[0]);
   	printf(  "| Buffer length (words)       (messagebox 1) :   %8ld |\n",Message_Box[1]);
   	printf(  "| Semaphore # 1               (messagebox 2) :   %8ld |\n",semph1);
   	printf(  "| Semaphore # 2               (messagebox 3) :   %8ld |\n",semph2);
   	printf(  "| Semaphore # 3               (messagebox 4) :   %8ld |\n",semph3);
   	printf(  "| Semaphore # 4               (messagebox 5) :   %8ld |\n",semph4);
   	printf(  "| VME ready(1) or not(0)      (messagebox 6) :   %8ld |\n",Message_Box[6]);
   	printf(  "| Linux ready(1) or not(0)    (messagebox 7) :   %8ld |\n",tiger);
   	printf(  "| Not used                    (messagebox 8) :   %8ld |\n",Message_Box[8]);
   	printf(  "| Not used                    (messagebox 9) :   %8ld |\n",Message_Box[9]);
	printf(  " --------------------------------------------------------- \n");

   	for(;;) {                               /*MAIN DATA ACQUISITION LOOP*/
		if ( *engp == 9 ) goto exitengine;   /*STOP COMMAND FROM MASTER */

/**********************************************************************	*/
/*                            B U F F E R                              	*/
/*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::	*/
/* 	Since A32 slave DRAM gives crash 									*/
/*  after typically 2 - 5 hours, we use A24 SRAM. However, this space  	*/
/*  is only 128 kbytes. Thus, we cannot use the 2 buffer technique   	*/
/*  from the original version designed by Tore Ramsoey. We				*/
/*  have to segment 1 eventbuffer (32 kwords) into 4 movebuffers,      	*/
/*  each 32 kbytes. Tests show that this is almost as fast as before,  	*/
/*  and no more crashes of the eventbuilder running on bobcat RIO2!!!  	*/
/*  First buffer is called for every 100 us, the next three other		*/
/*  buffers is called for every 1 us.									*/
/*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::	*/

/************************************************************************/ 
/*1.0 Check if movebuffer is ready                                     	*/
/*		Loop on VME semaphore #	1 until it becomes 1 (FULL)				*/
/*		The call to usleep suspends the process 100 us                  */
		for (;;) {
			status = bt_read(btd, sem1ptr, SEM_1, 4, &amt_read);
			if ( BT_SUCCESS != status) {
				bt_perror(btd, status, "Engine ERROR: Could not read VME semaphore # 1");
				vme_close();
				return EXIT_FAILURE;
			}
			if (sem1val == 1 ) break;				/* Buffer in VME is FULL, go and fetch it		*/    
			if (*engp	== 9) goto exitengine;		/* User has requested to stop acquisition		*/
			usleep(100);							/* Buffer is EMPTY, continue asking (polling)	*/
		}

/*1.1 Signal the sorting task to disrupt sorting						*/
/*		This is done by setting message_box[0] to '3'                   */
		*sortp = 3;

/*1.2 Fetch movebuffer #1 from the VMEbus system						*/
		status = bt_read(btd, bufp + 0x0000, moveaddress, movebytes, &amt_read);
		if ( BT_SUCCESS != status) {
			bt_perror(btd, status, "Engine ERROR: Could not read VME event buffer 1");
			vme_close();
			return EXIT_FAILURE;
		}

/*1.3 Reset VMEbus semaphore # 1 to empty								*/
		sem1val = 0;
		status = bt_write(btd, sem1ptr, SEM_1, 4, &amt_read);
		if ( BT_SUCCESS != status) {
			bt_perror(btd, status, "Engine ERROR: Could not reset VME semaphore # 1 to empty");
			vme_close();
			return EXIT_FAILURE;
		}

/************************************************************************/ 
/*2.0 Check if movebuffer is ready                                     	*/
/*		Loop on VME semaphore # 2 until it becomes 1 (FULL)				*/
/*		The call to usleep suspends the process 1 us                    */
		for (;;) {
			status = bt_read(btd, sem2ptr, SEM_2, 4, &amt_read);
			if ( BT_SUCCESS != status) {
				bt_perror(btd, status, "Engine ERROR: Could not read VME semaphore # 2");
				vme_close();
				return EXIT_FAILURE;
			}
			if (sem2val == 1 ) break;				/* Buffer in VME is FULL, go and fetch it		*/    
			if (*engp	== 9) goto exitengine;		/* User has requested to stop acquisition		*/
			usleep(1);								/* Buffer is EMPTY, continue asking (polling)	*/
		}

/*2.1 Signal the sorting task to disrupt sorting						*/
/*		This is done by setting message_box[0] to	3                   */
		*sortp = 3;

/*2.2 Fetch movebuffer #2 from the VMEbus system						*/
		status = bt_read(btd, bufp + 0x2000, moveaddress, movebytes, &amt_read);
		if ( BT_SUCCESS != status) {
			bt_perror(btd, status, "Engine ERROR: Could not read VME event buffer 2");
			vme_close();
			return EXIT_FAILURE;
		}

/*2.3 Reset VMEbus semaphore # 2 to empty								*/
		sem2val = 0;
		status = bt_write(btd, sem2ptr, SEM_2, 4, &amt_read);
		if ( BT_SUCCESS != status) {
			bt_perror(btd, status, "Engine ERROR: Could not reset VME semaphore # 2 to empty");
			vme_close();
			return EXIT_FAILURE;
		}
       
/************************************************************************/ 
/*3.0 Check if movebuffer is ready                                     	*/
/*		Loop on VME semaphore # 3 until it becomes 1 (FULL)				*/
/*		The call to usleep suspends the process 1 us                    */
		for (;;) {
			status = bt_read(btd, sem3ptr, SEM_3, 4, &amt_read);
			if ( BT_SUCCESS != status) {
				bt_perror(btd, status, "Engine ERROR: Could not read VME semaphore # 3");
				vme_close();
				return EXIT_FAILURE;
			}
			if (sem3val == 1 ) break;				/* Buffer in VME is FULL, go and fetch it		*/    
			if (*engp	== 9) goto exitengine;		/* User has requested to stop acquisition		*/
			usleep(1);								/* Buffer is EMPTY, continue asking (polling)	*/
		}

/*3.1 Signal the sorting task to disrupt sorting						*/
/*		This is done by setting message_box[0] to 3						*/
		*sortp = 3;

/*3.2 Fetch movebuffer #3 from the VMEbus system						*/
		status = bt_read(btd, bufp + 0x4000, moveaddress, movebytes, &amt_read);
		if ( BT_SUCCESS != status) {
			bt_perror(btd, status, "Engine ERROR: Could not read VME event buffer 3");
			vme_close();
			return EXIT_FAILURE;
		}

/*3.3 Reset VMEbus semaphore # 3 to empty								*/
		sem3val = 0;
		status = bt_write(btd, sem3ptr, SEM_3, 4, &amt_read);
		if ( BT_SUCCESS != status) {
			bt_perror(btd, status, "Engine ERROR: Could not reset VME semaphore #3 to empty");
			vme_close();
			return EXIT_FAILURE;
		}
		
/************************************************************************/ 
/*4.0 Check if movebuffer is ready                                     	*/
/*		Loop on VME semaphore # 4 until it becomes 1 (FULL)				*/
/*		The call to usleep suspends the process 1 us                    */
		for (;;) {
			status = bt_read(btd, sem4ptr, SEM_4, 4, &amt_read);
			if ( BT_SUCCESS != status) {
				bt_perror(btd, status, "Engine ERROR: Could not read VME semaphore # 4");
				vme_close();
				return EXIT_FAILURE;
			}
			if (sem4val == 1 ) break;				/* Buffer in VME is FULL, go and fetch it		*/    
			if (*engp	== 9) goto exitengine;		/* User has requested to stop acquisition		*/
			usleep(1);								/* Buffer is EMPTY, continue asking (polling)	*/
		}

/*4.1 Signal the sorting task to disrupt sorting						*/
/*		This is done by setting message_box[0] to 3						*/
		*sortp = 3;

/*4.2 Fetch movebuffer # 4 from the VMEbus system						*/
		status = bt_read(btd, bufp + 0x6000, moveaddress, movebytes, &amt_read);
		if ( BT_SUCCESS != status) {
			bt_perror(btd, status, "Engine ERROR: Could not read VME event buffer 4");
			vme_close();
			return EXIT_FAILURE;
		}

/*4.3 Reset VMEbus semaphore # 4 to empty								*/
		sem4val = 0;
		status = bt_write(btd, sem4ptr, SEM_4, 4, &amt_read);
		if ( BT_SUCCESS != status) {
			bt_perror(btd, status, "Engine ERROR: Could not reset VME semaphore # 4 to empty");
			vme_close();
			return EXIT_FAILURE;
		}


/* testing, magne
   for (i = 0; i < 32768; i++)  {  
   if(*(bufp+i) < 40000 ){
   printf(" no %d buffer read = %d \n",i, *(bufp+i) );}
   } 
   exit(0);
*/

/***********************************************************************/ 
/*5.0 Activate sorting task                                            */
/*		This is done by setting the sort semaphore to 0                */
		*sortp  = 0;                              /*Remove disrupt flag*/
		semno   = 0;
		arg.val = 0;
		if ( semctl(semid, semno, SETVAL, arg) == -1) {
			printf("Engine ERROR: Set Sort Semaphore to Zero Failed\n");
			exit(errno);
		}

/*6.0 Start dump of total eventbuffer to exabyte tape or disk			*/
/*		Buffer is segmented into 4 records, each 32 kbytes long			*/
		if ( write_data == 1 ) {
			bytes_written = write(exades, buf1p, RECORD_LENGTH);
			bytes_written = write(exades, buf2p, RECORD_LENGTH);
			bytes_written = write(exades, buf3p, RECORD_LENGTH);
			bytes_written = write(exades, buf4p, RECORD_LENGTH);
			if (bytes_written != RECORD_LENGTH) {
				printf("Engine ERROR: Could not write event buffer to disk (or tape)\n");
			}
		} 

/*7.0 Update record count message_box[9] variable						*/
		*recp = *recp + 1;
	}                               /*END OF MAIN DATA ACQUISITION LOOP	*/

	exitengine:

/*	Signal to VME front-end system that the acquisition is stopped		*/
/*	This is done by setting the VME Message_Box[1] to 0					*/
  	engstatval = 0;
   	status = bt_write(btd, engstatptr, ENGSTATUS, 4, &amt_read);
	if ( BT_SUCCESS != status) { 
		if (amt_read != 4) {
            fprintf(stderr, "Engine ERROR: Only %d bytes written to SBS VME-PCI\n", (int) amt_read);
        }
        else {
            fprintf(stderr, "Engine ERROR: Could not write to %s\n", &devname[0]);
        }
        bt_perror(btd, status, "Engine ERROR: Could not write engine status to SBS VME-PCI");
	}

/*	Detach shared memory */
	if ( shmdt( shm1ptr ) == -1) {
		printf("Engine ERROR: Could not detach databuffer shared memory\n");
	}
	if ( shmdt( shm2ptr ) == -1) {
		printf("Engine ERROR: Could not detach message_box shared memory\n");
	} 

/*	Remove the lock file */
	if ( remove("/Applications/sirius/system/engine.lock") == -1) {
		printf("Engine ERROR: Could not remove engine.lock file\n");      
	}

/* Closing VME front-end system */
	if (vme_close() != 0) {
		printf("Engine ERROR: Problems to close SBS VME_PCI\n");
		exit(errno);
	}
	exit(0);
} 


int vme_open()
{
   /*  Open SBS VME-PCI  */
    status = bt_open(&btd, bt_gen_name(unit, type, &devname[0], BT_MAX_DEV_NAME), BT_RD | BT_WR);
    if (BT_SUCCESS != status) {
        bt_perror(btd, status, "Engine ERROR: Could not open SBS VME-PCI");
		remove("/Applications/sirius/system/engine.lock");
        return -1;
    }
	
    /* Clear any outstanding errors */
    status = bt_clrerr(btd);
    if (BT_SUCCESS != status) {
        bt_perror(btd, status, "Engine ERROR: Could not clear errors from SBS VME-PCI");
		remove("/Applications/sirius/system/engine.lock");
        (void) bt_close(btd);
        return -1;
    }
	return 0;
}


int vme_close()
{
    /* Check for status errors */
    status = bt_chkerr(btd);
    if ( BT_SUCCESS != status) {
        bt_perror(btd, status, "Engine ERROR: Status error from SBS VME-PCI");
    }

    /* Close SBS VME-PCI */
    status = bt_close(btd);
    if ( BT_SUCCESS != status) {
        bt_perror(btd, status, "Engine ERROR: Could not close SBS VME-PCI");
    }
	
	/* Remove the lock file (if it is not done already) */
	remove("/Applications/sirius/system/engine.lock");

   	return 0;
}
