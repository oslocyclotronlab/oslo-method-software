#include	<stdio.h>
#include	<stdlib.h>
#include	<errno.h>
#include	<sys/types.h>
#include	<unistd.h>
#include	<fcntl.h>
#include	<sys/ipc.h>
#include	<sys/sem.h>
#include	<ipc_defs.h>

int wait4zero(int semid, int semno)
{
	int		 nsems=NSEM;
	size_t           nsops=NSEM; 
	union  semun {
          int val;
          struct semid_ds *buf;
          ushort *array;
	} arg;
	struct sembuf    sops[NSEM];
	int		semval = 1;

	arg.buf = (void *) malloc (100); 
	arg.val = semval;
     
     
    /* Test semaphore: start if zero, suspend process if 1 */
     sops[0].sem_num = 0;       /* Semaphore # 1 : SORT */
     sops[0].sem_op  = 0;       /* Test for zero */
     sops[0].sem_flg = WAIT;    /* Block */
     sops[1].sem_num = 0;       /* Semaphore # 2 : NU */
     sops[1].sem_op  = 0;       /* Test for zero */
     sops[1].sem_flg = NOWAIT;  /* No-block */
     if ( semop(semid, sops, nsops) == -1) {
        perror("\n **** ERROR ****  Wait4Zero: Semaphore Test Failed  \n");
        return -1;
     } 

    /* OK, set the semaphore to 1 */
    if ( semctl(semid, semno, SETVAL, arg) == -1) {
       perror("\n **** ERROR **** SETVAL failed \n");
       return -1;
    } 
     return  0;  
}
