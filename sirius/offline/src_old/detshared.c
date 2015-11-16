#include	<stdio.h>
#include	<errno.h>
#include	<sys/file.h>
#include	<sys/types.h>
#include	<sys/ipc.h>
#include	<sys/sem.h>
#include	<fcntl.h>
#include	<signal.h>
#include	<sys/shm.h>
#include	<sys/time.h>

int detshared( void *shmptr )
{
	int	result;

/* Detach shared memory from process */
	result = shmdt( shmptr );
	return result;
}
