#include	<errno.h>
#include	<sys/types.h>
#include	<sys/ipc.h>
#include	<sys/sem.h>
#include	<ipc_defs.h>

int getsemaphore(int semid, int semno)
{
	int		nsems=NSEM;
	size_t   nsops=NSEM; 
	union  semun {
		int val;
		struct semid_ds *buf;
		ushort *array;
	} arg;
	struct sembuf    sops[NSEM];
	int              semval;

	semval = semctl(semid, semno, GETVAL); 
	if ( semval == -1) {
		return -1;
	} 
	return  semval;  
}
