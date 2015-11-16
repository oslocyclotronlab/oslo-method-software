#include <sys/ipc.h>
#include <sys/shm.h>
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[])
{
  int id, key, size;

  if( argc != 3 ) {
    printf("Use: %s shm-key shm-size\n", argv[0]);
    exit(-1);
  }

  key = atoi(argv[1]);
  size = atoi(argv[2]);

  id = shmget( key, size, 0 );
  if( shmctl( id, IPC_RMID, 0 )==-1 ) {
    perror("shm_destroy");
    exit(-1);
  }

  return 0;
}
