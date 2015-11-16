#include	<errno.h>
#include	<sys/types.h>
#include	<sys/ipc.h>
#include	<sys/sem.h>
#include	<ipc_defs.h>

int getsemid()

{
	int		nsems=NSEM;
	int      semid;
	key_t	skey=SEMAPHORE_KEY;

/* Get the semaphore identifier for set */
	semid = semget(skey, nsems, PERMS );
	if (semid == -1) {
		perror("\n **** ERROR ****  semget - Semaphore Creation Failed  \n");
		return -1;
	}        
	return  semid;  
}
