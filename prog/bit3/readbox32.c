/***********************************************************************/
/*         Testing buffercontents in RTPC A32 slave memory, Magne      */
/***********************************************************************/

#include        <stdio.h>
#include        <unistd.h>
#include        <ctype.h>
#include        <string.h>
#include        <errno.h>
#include        <limits.h>
#include        <sys/file.h>
#include        <sys/types.h>
#include        <sys/stat.h>
#include        <sys/btio.h>
 
#define LIST_DATA        0x10000
#define BUFFER_BYTES     0x40000
#define BUFFER_ADDRESS   0x08000000

/******************************************************************************
**      Local function prototypes, max LIST_DATA 0x10000
******************************************************************************/
void usage(char *progname);
int  bit3_perror(int chan);
char *bt_devname(int unit, u_long *addr, int axstype, char *devname);

int            main()
{
    char       *device = BT_DEVNAME;              /* device type = 3 for AXSRR*/
    char       *cp;                               /* Device name */
    bt_status_t status_flags;                     /* Status flag */
    int         chan;                             /* Device channel */
    int         unit = 0;                         /* default unit no */
    int         type = BT_AXSRR;                  /* use 32 bits address */
    int         my_index;                            
    u_long      *buff_p;  
    u_long      *st_p;       
    u_long      data, buffer_bytes, test;
    u_long      buffer_address;               /* Data buffer address (var) */
    u_long      loop;
    u_long      upper;

    /***************************************************************************
    **   Open the BIT 3 Adaptor
    ***************************************************************************/
    printf("Type VME address 0x08000000 + RTPC physical address, where \n");
    printf("the number look like 8c60000 (the 6 latest digits may change):\n");
    scanf("%Lx",&buffer_address);
    printf("Remote VME address =0x%08x \n",buffer_address);
/*    buffer_address  = BUFFER_ADDRESS;*/
    buffer_bytes    = BUFFER_BYTES;
    printf("Bufferbytes 0x%x (%d) \n", buffer_bytes, buffer_bytes);
    if ((cp = bt_devname(unit, & buffer_address, type, device)) != NULL) {
        fprintf(stderr, "Opening device %s            \n", cp);
        fprintf(stderr, "for logical device of type %i\n", type);
        fprintf(stderr, "at remote address %x         \n", buffer_address);
        fprintf(stderr, "default device name %s       \n", device);
     
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
    **  Position to Data buffer start address
    ***************************************************************************/
    if (lseek(chan, buffer_address, SEEK_SET) == -1) {
      perror("\n **** ERROR ****  lseek failed\n\n");
      exit(errno);
    } else { 
      fprintf(stderr, "  >> Position for read operation at remote VMEbus address %x \n", buffer_address);
    }

   /***************************************************************************
    **  Allocate memory for data buffer
    ***************************************************************************/
    buff_p = (u_long *) malloc(buffer_bytes);

    /***************************************************************************
    **  Read Data Buffer, 64kW
    ***************************************************************************/
     test = read(chan, buff_p,buffer_bytes);
 
     if (test != buffer_bytes) {
       perror("\n *** ERROR *** Error reading data\n\n");
       exit(errno);
     }

    /***************************************************************************
    **  Display (Partly) Data buffer
    ***************************************************************************/
    upper = LIST_DATA;
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
