#include	<stdio.h>
#include	<stdlib.h>
#include	<errno.h>
#include	<sys/ipc.h>
#include	<sys/sem.h>
#include	<sys/shm.h>

#include	<fcntl.h>
#include	<signal.h>
#include	<string.h>
#include	<ctype.h>

#include	<sys/types.h>           /*                                  */ 
#include	<sys/mtio.h>            /* header files for tape operations */ 
#include	<sys/ioctl.h>           /*                                  */ 

#include	<ipc_defs.h>
#include	<buffer_defs.h>
#include	<spec_defs.h>

#define		ATTACH 1
#define		DETTAC 2

/* Define global variables */
int     semid;			/* Semaphore set ID */
int		*messp;			/* Pointer to shared memory message_box */
int		singid;
int		eid;
int		deid;
int		edeid;
int		thid;
int		geid;
int		tgeid;
int		naid;
int		tnaid;
int		anaid;
int		ageid;
int		matid; 

int init_sirius()
{
	int		 	shmid;
	struct		shmid_ds  	*mbuf;
	struct		shminfo   	*inf; 
	int		 	shmemsize	= BUFFER_LENGTH;
	key_t		mkey		=MESSAGE_KEY;
	
	int         semno		= 1;
	int		 	nsems		= NSEM;
	key_t		skey		= SEMAPHORE_KEY;
	int         semval;
	union  semun {
		int val;
		struct semid_ds *sbuf;
		ushort *array;
	} arg;
	struct	sembuf sops[NSEM];
	ushort	myvec[NSEM]; 
 	int		*savep;
	int		j;
	int		*telep;
	int		tele_off = 11;

/* Attach shared message_box segment to process */
	shmemsize = 80;
	shmid = shmget(mkey, shmemsize, IPC_CREAT | PERMS);		
	if (shmid == -1) {
		perror("\n **** ERROR ****  Shared Memory Allocation Failed  \n");
		return -1;
	}
	messp = shmat(shmid, NULL, 0);				
	if (*messp == -1) {
		perror("\n **** ERROR **** Attach segment to process failed  \n");
		return -1;
	} 
	if (shared_hist ( ATTACH ) == -1) {
		perror("\n **** ERROR **** Could not create shared histogram area \n");
		return -1;
	}

/* Clear message_box segment */
	savep = messp;
	for (j = 0; j < 20; j++)  {  
		*messp = 0;        
		messp++;                                             
	}                          
	messp	= savep;			/* Restore pointer */
	telep   = messp + tele_off;	
	*telep  = NO_OF_TELESCOPES;

/* Create the semaphore set */
	arg.sbuf	= (void *) malloc (100); 
	arg.array	= &myvec[0];
	semid		= semget(skey, nsems, IPC_CREAT | PERMS  );
	if (semid == -1) {
		perror("\n **** ERROR ****  MASTER: Create Semaphore Set Failed  \n");
		return -1;     
	}  
	myvec[0] = 0;
	myvec[1] = 0;
	if ( semctl(semid, 0, SETALL, arg) == -1) {
		perror("\n **** ERROR **** SETALL failed \n");
		return -1;
	} 

/* Initialize master semaphore to 0 */    
	arg.val = 0;                       
	if ( semctl(semid, semno, SETVAL, arg) == -1) {                          
		perror("\n **** ERROR **** MASTER : setval failed \n");
		return -1;
	} 
	return 0;
}


int shared_hist( int mode ) 
{
	int		shmemsize;
	key_t	mkey;

/*	int		singid;
	int		eid;
	int		deid;
	int		edeid;
	int		thid;
	int		geid;
	int		tgeid;
	int		naid;
	int		tnaid;
	int		anaid;
	int		ageid;
	int		matid; */

	if ( mode == 1 ) {

/* Create singles area shared segment */
		shmemsize = SINGLES_SIZE;
		mkey = SINGLES_KEY;
		singid = shmget(mkey, shmemsize, IPC_CREAT | PERMS);		
		if (singid == -1) {
			perror("\n **** ERROR ****  Shared Memory Allocation Failed (SINGLES) \n");
		return -1;           
		}

/* Create E shared segment */
		shmemsize = ESP_SIZE;
		mkey = ESP_KEY;
		eid = shmget(mkey, shmemsize, IPC_CREAT | PERMS);		
		if (eid == -1) {
			perror("\n **** ERROR ****  Shared Memory Allocation Failed (ESP) \n");
			return -1;           
		}

/* Create DE shared segment */
		shmemsize = DESP_SIZE;
		mkey = DESP_KEY;
		deid = shmget(mkey, shmemsize, IPC_CREAT | PERMS);		
		if (deid == -1) {
			perror("\n **** ERROR ****  Shared Memory Allocation Failed (DESP) \n");
			return -1;           
		}

/* Create E-DE shared segment */
		shmemsize = EDESP_SIZE;
		mkey = EDESP_KEY;
		edeid = shmget(mkey, shmemsize, IPC_CREAT | PERMS);		
		if (edeid == -1) {
			perror("\n **** ERROR ****  Shared Memory Allocation Failed (EDESP) \n");
			return -1;           
		}

/* Create Thickness shared segment */
		shmemsize = THICKSP_SIZE;
		mkey = THICKSP_KEY;
		thid = shmget(mkey, shmemsize, IPC_CREAT | PERMS);		
		if (thid == -1) {
			perror("\n **** ERROR ****  Shared Memory Allocation Failed (THICKSP) \n");
			return -1;           
		}

/* Create Ge shared segment */
		shmemsize = GESP_SIZE;
		mkey = GESP_KEY;
		geid = shmget(mkey, shmemsize, IPC_CREAT | PERMS);		
		if (geid == -1) {
			perror("\n **** ERROR ****  Shared Memory Allocation Failed (GESP) \n");
			return -1;           
		}

/* Create Ge-T shared segment */
		shmemsize = TGESP_SIZE;
		mkey = TGESP_KEY;
		tgeid = shmget(mkey, shmemsize, IPC_CREAT | PERMS);		
		if (tgeid == -1) {
			perror("\n **** ERROR ****  Shared Memory Allocation Failed (TGESP) \n");
			return -1;           
		}

/* Create NaI shared segment */
		shmemsize = NASP_SIZE;
		mkey = NASP_KEY;
		naid = shmget(mkey, shmemsize, IPC_CREAT | PERMS);		
		if (naid == -1) {
			perror("\n **** ERROR ****  Shared Memory Allocation Failed (NASP) \n");
			return -1;           
		}

/* Create NaI-T shared segment */
		shmemsize = TNASP_SIZE;
		mkey = TNASP_KEY;
		tnaid = shmget(mkey, shmemsize, IPC_CREAT | PERMS);		
		if (tnaid == -1) {
			perror("\n **** ERROR ****  Shared Memory Allocation Failed (TNASP) \n");
			return -1;           
		}
		   		   		   
/* Create Alpha-NaI shared segment */
		shmemsize = ALFNA_SIZE;
		mkey = ALFNA_KEY;
		anaid = shmget(mkey, shmemsize, IPC_CREAT| PERMS);		
		if (anaid == -1) {
			perror("\n **** ERROR ****  Shared Memory Allocation Failed (ALFNA) \n");
			perror("shmget");
			return -1;           
		}
	
/* Create Alpha-Ge shared segment */
		shmemsize = ALFGE_SIZE;
		mkey = ALFGE_KEY;
		ageid = shmget(mkey, shmemsize, IPC_CREAT | PERMS);		
		if (ageid == -1) {
			perror("\n **** ERROR ****  Shared Memory Allocation Failed (ALFGE) \n");
			ferror;
			return -1;           
		}

/* Create GP matrix shared segment */
		shmemsize = MAT_SIZE;
		mkey = MAT_KEY;
		matid = shmget(mkey, shmemsize, IPC_CREAT | PERMS);		
		if (matid == -1) {
			perror("\n **** ERROR ****  Shared Memory Allocation Failed (MAT) \n");
			return -1;           
		}
	}

	if ( mode == 2 ) {		/* Remove spectrum area shared memory */
			 
		if ( shmctl( singid, IPC_RMID,0) == -1) {
			perror("\n **** ERROR ****  Remove shared memory ID failed  \n");
			return -1;           
		} 
		if ( shmctl( eid, IPC_RMID,0) == -1) {
			perror("\n **** ERROR ****  Remove shared memory ID failed  \n");
			return -1;           
		} 
		if ( shmctl( deid, IPC_RMID,0) == -1) {
			perror("\n **** ERROR ****  Remove shared memory ID failed  \n");
			return -1;           
		} 
		if ( shmctl( edeid, IPC_RMID,0) == -1) {
			perror("\n **** ERROR ****  Remove shared memory ID failed  \n");
			return -1;           
		} 
		if ( shmctl( thid, IPC_RMID,0) == -1) {
			perror("\n **** ERROR ****  Remove shared memory ID failed  \n");
			return -1;           
		} 
		if ( shmctl( geid, IPC_RMID,0) == -1) {
			perror("\n **** ERROR ****  Remove shared memory ID failed  \n");
			return -1;           
		} 
		if ( shmctl( tgeid, IPC_RMID,0) == -1) {
			perror("\n **** ERROR ****  Remove shared memory ID failed  \n");
			return -1;           
		} 
		if ( shmctl( naid, IPC_RMID,0) == -1) {
			perror("\n **** ERROR ****  Remove shared memory ID failed  \n");
			return -1;           
		} 
		if ( shmctl( tnaid, IPC_RMID,0) == -1) {
			perror("\n **** ERROR ****  Remove shared memory ID failed  \n");
			return -1;           
		}
		if ( shmctl( anaid, IPC_RMID,0) == -1) {
			perror("\n **** ERROR ****  Remove shared memory ID failed  \n");
			return -1;           
		}
		if ( shmctl( ageid, IPC_RMID,0) == -1) {
			perror("\n **** ERROR ****  Remove shared memory ID failed  \n");
			return -1;           
		}
		if ( shmctl( matid, IPC_RMID,0) == -1) {
			perror("\n **** ERROR ****  Remove shared memory ID failed  \n");
			return -1;           
		}
	}
	return 0;
}