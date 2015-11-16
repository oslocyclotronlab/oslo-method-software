/***********************************************************************/
/*                                                                     */
/*           The semaphores are located in a MESSAGE_BOX in the        */
/*           FIC8230 memory. The MESSAGE_BOX is fixed in memory        */
/*           and the addresses are engraved in this program.           */
/*                                                                     */
/*                                                                     */
/*                      FIC8230             VME addr.                  */
/*                --------------------                                 */
/*                |  BUFFER_ADDRESS  |      201F FFD0                  */
/*                --------------------                                 */
/*                |  BUFFER_LENGTH   |      201F FFD4                  */
/*                --------------------                                 */
/*                |   SEMAPHORE_1    |      201F FFD8                  */
/*                --------------------                                 */
/*                |   SEMAPHORE_2    |      201F FFDC                  */
/*                --------------------                                 */
/*                |       N.U.       |      201F FFE0                  */
/*                --------------------                                 */
/*                |    VMESTATUS     |      201F FFE4                  */
/*                --------------------                                 */
/*                |    ACQSTATUS     |      201F FFE8                  */
/*                --------------------                                 */
/*                |       N.U.       |      201F FFEC                  */
/*                --------------------                                 */
/*                |       N.U.       |      201F FFF0                  */
/*                --------------------                                 */
/*                |       N.U.       |      201F FFF4                  */
/*                --------------------                                 */
/***********************************************************************/


#include        <stdio.h>
#include	<unistd.h>
#include        <ctype.h>
#include        <string.h>
#include        <errno.h>
#include        <limits.h>
#include        <sys/file.h>
#include        <sys/types.h>
#include        <sys/stat.h>

#include        <sys/btio.h>
 

#define BUFFER_ADDRESS   0x08000000
#define BUFFER_LENGTH    0x08000004
#define SEMAPHORE_1      0x08000008
#define SEMAPHORE_2      0x0800000C


#define NUM_WORDS        32768

/******************************************************************************
**
**      Local function prototypes
**
******************************************************************************/
 
  void usage(char *progname);
  int  bit3_perror(int chan);
  char *bt_devname(int unit, u_long *addr, int axstype, char *devname);
 
 


/******************************************************************************
**
**         main program test
**
*******************************************************************************/

int             main()

{
    char           *device = BT_DEVNAME;                 /* device type */
    char           *cp;                                  /* Device name */
    bt_status_t     status_flags;                        /* Status flag */
    int             chan;                                /* Device channel */
    int             unit = 0;                            /* default unit no */
    int             type = BT_AXSRR;                     /* use 32 bits address */
    int             my_index;                            
    u_long          *mess_p;
    u_long          *buff_p;  
    u_long          *st_p;       
    u_long          data;
    u_long          Message_Box[10];
    u_long          message_address = BUFFER_ADDRESS;    /* VMEbus Message_Box address */
    u_long          buffer1_address;                     /* Data buffer 1 address (variable) */
    u_long          buffer2_address;                     /* Data buffer 2 address (variable) */
    u_long          buffer_length;                       /* Length of data buffer in bytes */
    u_long          message_length = 40;                 /* Length of Message_Box in bytes */
    u_long          loop;
    u_long          upper;
 
    /***************************************************************************
    **   Open the BIT 3 Adaptor
    ***************************************************************************/
  
    if ((cp = bt_devname(unit, &message_address, type, device)) != NULL) {
        fprintf(stderr, " ******************************************\n");
        fprintf(stderr, " *  Opening device %s            *\n", cp);
        fprintf(stderr, " *  for logical device of type %i          *\n", type);
        fprintf(stderr, " *  at remote address %x            *\n", message_address);
        fprintf(stderr, " *  default device name %s          *\n", device);
        fprintf(stderr, " ******************************************\n");
      
     
        if ((chan = open(cp, O_RDWR)) < 0 ) {
           perror("\n **** ERROR ****  Cannot Open Device Driver\n\n");
           exit(errno);
        }
   
    } else {
        perror("\n **** ERROR ****  Cannot Generate File Name\n\n");
        exit(0);
    }
  
    
    /***************************************************************************
    **  Make sure the Adaptor is connected
    ***************************************************************************/

    (void) ioctl(chan, BIOC_CLR_STATUS, &status_flags);
    if (status_flags & BT_STATUS_MASK) {
        bit3_perror(chan);
        fprintf(stderr, "\n **** ERROR ****  Could not initialize Bit 3 Adaptor.\n\n");
        exit(1);
      }



    /***************************************************************************
    **  Position to Message_Box start address
    ***************************************************************************/
    if (lseek(chan, BUFFER_ADDRESS, SEEK_SET) == -1) {
      perror("\n **** ERROR ****  lseek failed\n\n");
      exit(errno);
    } else {                                                                                           
      fprintf(stderr, "  >> Position for read operation at remote VMEbus address %x \n", message_address);
    }


   /***************************************************************************
    **  Allocate memory for Message_Box
    ***************************************************************************/
   
    mess_p = (u_long *) malloc(message_length);



    /***************************************************************************
    **  Read Message_Box, 40 bytes
    ***************************************************************************/
    
     if (read(chan, mess_p, message_length) != message_length) {
       perror("\n *** ERROR *** Error reading data\n\n");
       exit(errno);
     }

    /***************************************************************************
    **  Display Message_Box
    ***************************************************************************/
  

    for ( loop = 0; loop  < message_length/4 ; loop++) {
      Message_Box[loop] = *mess_p;
      printf("Message_Box(%d): \t %x (hex)\n",loop,Message_Box[loop]);
      ++mess_p;
    }


   
    buffer1_address = Message_Box[0];
    buffer_length = Message_Box[1];
/**********PASS PAA, JUKSER HER********************/
    buffer1_address = BUFFER_ADDRESS;
    buffer_length = 32768;


    buffer2_address = buffer1_address + ((buffer_length+1)*4);

    printf(" Buffer1: %x   Buffer2: %x\n\n",buffer1_address, buffer2_address); 

    /***************************************************************************
    **  Position to Data buffer 1 start address
    ***************************************************************************/
    if (lseek(chan, buffer1_address, SEEK_SET) == -1) {
      perror("\n **** ERROR ****  lseek failed\n\n");
      exit(errno);
    } else {                                                                                           
      fprintf(stderr, "  >> Position for read operation at remote VMEbus address %x \n", buffer1_address);
    }


   /***************************************************************************
    **  Allocate memory for data buffer
    ***************************************************************************/
    buff_p = (void *) malloc(buffer_length);



    /***************************************************************************
    **  Read Data Buffer 1, 32kW
    ***************************************************************************/
    
     if (read(chan, buff_p, buffer_length) != buffer_length) {
       perror("\n *** ERROR *** Error reading data\n\n");
       exit(errno);
     }

    /***************************************************************************
    **  Display (Partly) Data buffer
    ***************************************************************************/
 
    upper = 100;
    for ( loop = 0; loop  < upper ; loop++) {
      data = *buff_p;
      printf("Data(%d): \t %d\n",loop,data);
      ++buff_p;
    }



   /***************************************************************************
    **  Position to Data buffer 2 start address
    ***************************************************************************/
    if (lseek(chan, buffer2_address, SEEK_SET) == -1) {
      perror("\n **** ERROR ****  lseek failed\n\n");
      exit(errno);
    } else {                                                                                           
      fprintf(stderr, "  >> Position for read operation at remote VMEbus address %x \n", buffer1_address);
    }



    /***************************************************************************
    **  Read Data Buffer 2, 32kW
    ***************************************************************************/
    
     if (read(chan, buff_p, buffer_length) != buffer_length) {
       perror("\n *** ERROR *** Error reading data\n\n");
       exit(errno);
     }

    /***************************************************************************
    **  Display (Partly) Data buffer
    ***************************************************************************/
 
    upper = 100;
/*  upper = NUM_WORDS; */   /* Print whole buffer */
    for ( loop = 0; loop  < upper ; loop++) {
      data = *buff_p;
      printf("Data(%d): \t %d\n",loop,data);
      ++buff_p;
    }





    /***************************************************************************
    **  Check for status errors
    ***************************************************************************/

    if (bit3_perror(chan)) {
        exit(1);
    }
    exit(0);



}   /* end of main() */
