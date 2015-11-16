/****************************************************************************
**
**      Filename:    readram.c
**
**      Purpose:     The readram program uses the device read() function to
**                   read from the VMEbus memory.  The program is similar to
**                   dumpram but uses a different method to transfer data.
**
**      Functions:   usage()
**
**      $Revision:   1.15  $
**
*****************************************************************************/
/*****************************************************************************
**
**        Copyright (c) 1990,1991 by Bit 3 Computer Corporation.
**                     All Rights Reserved.
**              License governs use and distribution.
**
*****************************************************************************/
  
#ifndef LINT
#ifdef __STDC__

  static const char revcntrl[] = "@(#)"__FILE__"  $Revision:   1.15  $ "__DATE__;

#else   /* __STDC__ */

  static char revcntrl[] = "@(#) $Workfile:   readram.c  $  $Revision:   1.15  $";

#endif  /* __STDC__ */
#endif  /* LINT */

  
#include        <stdio.h>
#include        <stdlib.h>
#include	<unistd.h>
#include        <ctype.h>
#include        <string.h>
#include        <errno.h>
#include        <limits.h>
#include        <sys/file.h>
#include        <sys/types.h>
#include        <sys/ioctl.h>
#include        <sys/uio.h>

#include        <sys/btio.h>
 
#define NUM_LINES       20
#define NUM_BYTES       4

/******************************************************************************
**
**      Local function prototypes
**
******************************************************************************/

#ifdef  PROTO_DEF
 
  void usage(char *progname);
  int  bit3_perror(int chan);
  char *bt_devname(int unit, u_long *addr, int axstype, char *devname);
 
#else
 
  void usage();
  int  bit3_perror();
  char *bt_devname();

#endif
  

/*****************************************************************************
**
**      Function:       readram()
**
**      Purpose:        Prints the first 256 bytes starting at 0x80000000
**                      as both hexadecimal bytes and ASCII characters.   
**
**      Args:
**          readram  [-]  [-t logical | -f /dev/bt?]  [-u unit]  [-i]  [-h]  [-a address]
**
**              -    Display the command summary.
**              -t   The logical device to access. (ie. BT_AXSIO, BT_AXDP, etc)
**              -f   Device to open.  Default is BT_DEVNAME.
**              -u   Unit number to open.  Default is unit 0.
**              -h   Set handshake mode before attempting data transfer.
**              -a   Start data transfer from a given address.
**
**
**      Modifies:
**
**      Returns:
**
**      Calls:          
**
******************************************************************************/

int             main(argc, argv)
int             argc;
char           *argv[];
{
    char           *program = argv[0];
    char           *device = BT_DEVNAME;
    char           *cp;
    int             my_index;
    int             nlines;
    bt_status_t     status_flags;
    caddr_t         addr;
    int             chan;
    u_char          data;
    char            buff[NUM_BYTES + 1];
    short           offset;
    u_long          remote_address = 0x2000;
    int             unit = 0;
    int             type = BT_AXSRR;
    int             handshake = FALSE;
    char           *ram_buffer[NUM_LINES];
    struct iovec    iov[NUM_LINES];
    int             iovcnt = NUM_LINES;

    /*
    **   Parse the command line
    */

    argc--;

    while (argv++, argc--) {

        if (!strcmp(*argv, "-t")) {
            argc--; argv++;
            if (isdigit(**argv)) {
                type = (int) strtol(*argv, NULL, 0);
                if (type == 24) {
                    type = BT_AXS24;
                }
            } else {
                if (       (strcasecmp(*argv, "BT_AXSIO")==0) ||
                           (strcasecmp(*argv, "AXSIO")==0) ||
                           (strcasecmp(*argv, "IO")==0) ) {
                    type = BT_AXSIO;
                } else if ((strcasecmp(*argv, "BT_AXSDP")==0) ||
                           (strcasecmp(*argv, "AXSDP")==0) ||
                           (strcasecmp(*argv, "DP")==0) ) {
                    type = BT_AXSDP;
                } else if ((strcasecmp(*argv, "BT_AXSRI")==0) ||
                           (strcasecmp(*argv, "AXSRI")==0) ||
                           (strcasecmp(*argv, "RI")==0) ) {
                    type = BT_AXSRI;
                } else if ((strcasecmp(*argv, "BT_AXSRR")==0) ||
                           (strcasecmp(*argv, "AXSRR")==0) ||
                           (strcasecmp(*argv, "RR")==0) ) {
                    type = BT_AXSRR;
                } else if ((strcasecmp(*argv, "BT_AXSRE")==0) ||
                           (strcasecmp(*argv, "AXSRE")==0) ||
                           (strcasecmp(*argv, "RE")==0) ) {
                    type = BT_AXSRE;
                } else if ((strcasecmp(*argv, "BT_AXS24")==0) ||
                           (strcasecmp(*argv, "AXS24")==0) ||
                           (strcasecmp(*argv, "24")==0) ) {
                    type = BT_AXS24;
                } else if ((strcasecmp(*argv, "BT_AXSLM")==0) ||
                           (strcasecmp(*argv, "AXSLM")==0) ||
                           (strcasecmp(*argv, "LM")==0) ) {
                    type = BT_AXSLM;
                }
            }

        } else if (!strcmp(*argv, "-")) {
            usage(program);
            exit(0);

        } else if (!strcmp("-a", *argv)) {
            argv++; argc--;
            remote_address = (u_long) strtoul(*argv, NULL, 0);

        } else if (!strcmp("-u", *argv)) {
            argv++; argc--;
            unit = strtol(*argv, NULL, 0);

        } else if (!strcmp("-h", *argv)) {
            handshake = TRUE;

        } else if (!strcmp(*argv, "-f")) {
            argv++; argc--;
            device = *argv;

        } else {
            usage(program);
            exit(0);
        }
    }
  
    /*
    **  Default address is 0x80000000
    */
  
  
    /*
    **   Open the device
    */
  
    if ((cp = bt_devname(unit, &remote_address, type, device)) != NULL) {
        fprintf(stderr, "Opening %s.\n", cp);
        if ((chan = open(cp, O_RDWR)) < 0 ) {
            perror("\n **** ERROR ****  Cannot Open Device Driver\n\n");
            exit(errno);
        }
    } else {
        perror("\n **** ERROR ****  Cannot Generate File Name\n\n");
        exit(0);
    }
  
    /*
    **  Make sure the Adaptor is connected
    */
  
    (void) ioctl(chan, BIOC_CLR_STATUS, &status_flags);
    if (status_flags & BT_STATUS_MASK) {
        bit3_perror(chan);
        fprintf(stderr, "\n **** ERROR ****  Could not initialize Bit 3 Adaptor.\n\n");
        exit(1);
    }
  
    /*
    **  Initialize the memory buffer and struct iovec that data is read into
    */
  
    for (nlines = 0; nlines < NUM_LINES; nlines++) {
        if ((ram_buffer[nlines] = (char *) malloc(NUM_BYTES+1)) == NULL) {
            perror("\n **** ERROR ****  Out of memory\n\n");
            exit(1);
        }
        memset(ram_buffer[nlines], 0, NUM_BYTES);
        iov[nlines].iov_base = ram_buffer[nlines];
        iov[nlines].iov_len = NUM_BYTES;
    }
  
    /*
    **   If it doesn't start at the beginning of the line
    */
  
    if ( (offset = remote_address % NUM_BYTES) != 0) {
        for (my_index = 0; my_index < offset; my_index++) {
            printf("   ");
            buff[my_index] = ' ';
        }
        iov[0].iov_base += offset;
        iov[0].iov_len -= offset;
    }
  
    /*
    **  Initialize handshake mode if was requested
    */
  
    if (handshake) {
        if (ioctl(chan, BIOC_SET_HNDSHK)) {
            perror("\n **** ERROR ****  Could not set handshake mode\n\n");
            exit(errno);
        }
    }
  
    /*
    **  Position to requested remote address
    */
  
    if (lseek(chan, remote_address, SEEK_SET) == -1) {
        perror("\n **** ERROR ****  lseek failed\n\n");
        exit(errno);
    };
  
    /*
    **  Read in the data
    */
  
    if ( (my_index = readv(chan, iov, iovcnt)) != NUM_BYTES * NUM_LINES - offset ) {
        if (my_index == -1) {
            perror("\n **** ERROR ****  readv failed\n\n");
            (void) bit3_perror(chan);
            exit(errno);
        } else {
            fprintf(stderr, "\n **** WARNING ****  Only Read In %d Bytes Of Data.\n\n", my_index);
        }
    }
  
    /*
    **  Display the data.
    */
  
    for (nlines = 0; nlines < NUM_LINES; nlines++) {
        addr = ram_buffer[nlines] + offset;     /* First line may be partial */
        for (my_index = offset; my_index < NUM_BYTES; my_index++) {
            data = *addr;
            *addr = (isprint(data)) ? data : '.';
            addr++;
            printf("%02x ", data);
        }
        offset = 0;                     /* All remaining lines are complete */
        printf(" %s\n", ram_buffer[nlines]);
    }
  
    /*
    **  Clear handshake mode if we set it
    */

    if (handshake) {
        (void) ioctl(chan, BIOC_CLR_HNDSHK);
    }
  
    /*
    **  Check for status errors
    */

    if (bit3_perror(chan)) {
        exit(1);
    }
  
    exit(0);
}                               /* end of main() */
  

/******************************************************************************
**
**      Function:       usage()
**
**      Purpose:        Prints command line arguments.
**
**      Args:           progname        Name of the program.  Usually argv[0].
**
**      Modifies:       None.
**
**      Returns:        None.
**
**      Calls:          fprintf()
**
*****************************************************************************/
  
void    usage(progname)
char   *progname;
{
    fprintf(stderr,"\n    This example program uses the device read() function to read from");
    fprintf(stderr,"\n    the VMEbus memory.  This program is similar to dumpram.c, but uses");
    fprintf(stderr,"\n    a different method to transfer data.\n\n");

    fprintf(stderr,"    USAGE: readram   [-]  [-t logical | -f /dev/bt?]  [-u unit]  [-h]\n");
    fprintf(stderr,"                     [-a address]\n\n");


    fprintf(stderr,"\t-\t\tDisplay this command summary.\n");

    fprintf(stderr,"\t-t\t\tThe logical device to access. (BT_AXSLM, BT_AXSDP)\n");

    fprintf(stderr,"\t-f </dev/bt?>\tDevice to open. Default is %s.\n", BT_DEVNAME);

    fprintf(stderr,"\t-u <unit>\tUnit Number to open.  Default is unit 0.\n");

    fprintf(stderr,"\t-h\t\tSet handshake mode before attempting data transfer.\n");

    fprintf(stderr,"\t-a <addr>\tAddress where data transfer is to begin.\n");

    fprintf(stderr,"\t\t\t(Program accepts radix using C notation.)\n");

    fprintf(stderr,"\n    EXAMPLE: Read the 1st 256 bytes of memory starting at address 0x80000000:\n");
    fprintf(stderr,"\n             readram -a 0x80000000 \n\n");

    return;
}
