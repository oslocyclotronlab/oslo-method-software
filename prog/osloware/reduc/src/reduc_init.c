#include        <stdio.h>
#include        <errno.h>
#include        <sys/types.h>
#include	<sys/ipc.h>
#include	<sys/sem.h>
#include        <fcntl.h>
#include	<signal.h>
#include	<string.h>
#include	<ctype.h>
#include	<sys/ioctl.h>
#include	<sys/mtio.h>

#include	<redmem_defs.h>
#include	<buffer_defs.h>


#define ATTACH 1
#define DETTAC 2


	/* Define global variables */
	int		*messp;			/* Pointer to shared memory message_box */


int reduc_init( )
{


	int		 	shmid1;
	int		 	shmid2;
	int		 	*shmat();
	struct shmid_ds  	*mbuf;
        struct shminfo   	*inf; 
    	int		 	shmemsize=BUFFER_LENGTH;
	key_t		 	mkey=MESSAGE_KEY;
	key_t		 	bkey=SHAREDMEM_KEY;

 	int			*savep;
	int			*gainfp;
	int			*telep;
	int			gainf_off  = 10;
	int			tele_off   = 11;
	int			j;

    	/* Get a Shared memory identifier for data buffer*/
    	shmid1 = shmget(bkey, shmemsize, IPC_CREAT | PERMS);
    	if (shmid1 == -1) {
      	   perror("\n **** ERROR ****  Shared Memory Databuffer Allocation Failed  \n");
      	exit(errno);
    	}

    	/* Get a Shared memory identifier for message box */
    	shmemsize = 80 ;
    	shmid2 = shmget(mkey, shmemsize, IPC_CREAT | PERMS);
    	if (shmid2 == -1) {
      	perror("\n **** ERROR ****  Shared Memory MessageBox Allocation Failed  \n");
      	   exit(errno);
    	}
        messp = shmat(shmid2, NULL, 0);				
        if (*messp == -1) {
           perror("\n **** ERROR **** Attach segment to process failed  \n");
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
	gainfp =  messp + gainf_off;
	telep  =  messp + tele_off;
	*gainfp = 20;
	*telep  = NO_OF_TELESCOPES;

	return 0;
}
