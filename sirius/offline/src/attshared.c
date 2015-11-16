#include	<stdio.h>
#include	<stdlib.h>
#include	<errno.h>
#include	<sys/file.h>
#include	<sys/types.h>
#include	<sys/ipc.h>
#include	<sys/sem.h>
#include	<fcntl.h>
#include	<signal.h>
#include	<sys/shm.h>
#include	<sys/time.h>
#include	<offmem_defs.h>
#include	<buffer_defs.h>

int *attshared( int mode )
{
	struct	shmid_ds  *mbuf;
	struct	shminfo   *inf; 
	int		shmid;
	int		shmemsize=BUFFER_LENGTH;
	int     *shmptr;  
	key_t	mkey=SHAREDMEM_KEY;

	if ( mode == 1) {						/* Attach shared databuffer */
		mkey = SHAREDMEM_KEY;
		shmemsize = BUFFER_LENGTH;
	}

	if ( mode == 2) {						/* Attach shared message box */
		mkey = MESSAGE_KEY;
		shmemsize = 80;
	}
/* Attach shared memory to process */
	shmid = shmget(mkey, shmemsize,  PERMS);		
	if (shmid == -1) {
		perror("\n **** ERROR ****  Shared Memory Allocation Failed  \n");
		return NULL;
	}
	shmptr = shmat(shmid, NULL, 0);			
	if (*shmptr == -1) {
		perror("\n **** ERROR **** Attach memory segment to process failed  \n");
		return NULL;
	}
	return shmptr;
}
