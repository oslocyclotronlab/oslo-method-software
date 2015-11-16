/*shm_server.c*/
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <stdio.h>

#define SHMSZ 100000 /*8388608*/


main()
{
    char c;
    int shmid;
    key_t key;
    int *shm, *s;

    key = 7000;
     printf("hei1");

    if ((shmid = shmget(key, SHMSZ, IPC_CREAT | 0666)) < 0) {
        perror("shmget");
        exit(1);
    }
        printf("hei2");
    if ((shm = shmat(shmid,0, 0666)) == NULL) {
        perror("shmat");
        exit(1);
    }

     s = shm;

    for (c = 'a'; c <= 'z'; c++)
        *s++ = c;
    *s = NULL;

		
    while (*shm != '*')
        sleep(1);

    exit(0);
}
