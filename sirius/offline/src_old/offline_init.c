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

#include	<offmem_defs.h>
#include	<buffer_defs.h>
#include	<offspec_defs.h>

#define		ATTACH 1
#define		DETTAC 2

/* Define global variables */
int			*messp;					/* Pointer to shared memory message_box */
int			singid;
int			eid;
int			deid;
int			edeid;
int			thid;
int			geid;
int			tgeid;
int			naid;
int			tnaid;
int			anaid;
int			ageid;
int			matid;

int offline_init( )
{

	int		shmid1;
	int		shmid2;
	struct	shmid_ds  	*mbuf;
	struct	shminfo   	*inf; 
	int		shmemsize	= BUFFER_LENGTH;
	key_t	mkey		= MESSAGE_KEY;
	key_t	bkey		= SHAREDMEM_KEY;
	
 	int		*savep;
	int		j;
	int		*telep;
	int		tele_off	= 11;

/* Get a Shared memory identifier for data buffer*/
	shmid1 = shmget(bkey, shmemsize, IPC_CREAT | PERMS);
	if (shmid1 == -1) {
		perror("\n **** ERROR ****  Shared Memory Databuffer Allocation Failed  \n");
		exit(errno);
	}

/* Get a Shared memory identifier for message box */
	shmemsize = 80;
	shmid2 = shmget(mkey, shmemsize, IPC_CREAT | PERMS);
	if (shmid2 == -1) {
		perror("\n **** ERROR ****  Shared Memory MessageBox Allocation Failed  \n");
		exit(errno);
	}
	messp = shmat(shmid2, NULL, 0);				
	if (*messp == -1) {
		perror("\n **** ERROR **** Attach data segment to process INIT failed  \n");
		return -1;
	}
	
	/* Clear message_box segment */
	savep = messp;
	for (j=0; j < 20; j++)  {  
		*messp = 0;        
		messp++;                                             
	}                          
	messp = savep;			/* Restore pointer */
	
 	/* Set other start values */
	telep  =  messp + tele_off;
	*telep  = NO_OF_TELESCOPES;

	/* Attach histogram area to task */
	if (shared_hist ( ATTACH ) == -1) {
	   perror("\n **** ERROR **** Could not create shared histogram area \n");
	   return -1;
	}
	return 0;
}


int shared_hist( int mode ) 
{
	int		shmemsize;
	key_t	mkey;

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
