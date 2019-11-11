#define  MESSAGE_ADDRESS 	0x00850000 		/* VMEbus Message_Box address  */
#define  SEM_1				0x00850008   	/* Databuffer # 1 semaphore */
#define  SEM_2				0x0085000c		/* Databuffer # 2 semaphore */
#define  SEM_3				0x00850010   	/* Databuffer # 3 semaphore */
#define  SEM_4				0x00850014		/* Databuffer # 4 semaphore */
#define  VMESTATUS			0x00850018		/* Running status for VME system */
#define  ENGSTATUS			0x0085001c		/* Running status for acq_engine task */
#define  BUFFER_LENGTH   	0x20000 		/* Data buffer length 32 kW = 128 kBytes*/
#define  MESSAGE_LENGTH		0x50			/* Message Box length 10 W = 40 bytes*/
#define  RECORD_LENGTH	 	0x8000			/* Exabyte record length = 32 kBytes */
#define  REC_IN_BUF  		4				/* 4 records on exabyte per databuffer */
#define  NO_OF_TELESCOPES	64				/* Number of particle telescopes 8/64  */
