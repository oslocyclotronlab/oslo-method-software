  
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
 
#define NUM_LINES       16
#define NUM_BYTES       16

#ifdef  PROTO
 
  void usage(char *progname);
  int  bit3_perror(int chan);
  char *bt_devname(int unit, u_long *addr, int axstype, char *devname);
 
#else
 
  void usage();
  int  bit3_perror();
  char *bt_devname();

#endif


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
    u_long          remote_address = 0x2010A70;
    int             unit = 0;
    int             type = BT_AXS24;
    char           *ram_buffer[NUM_LINES];
    struct iovec    iov[NUM_LINES];
    int             iovcnt = NUM_LINES;



/***************************************************/
/*     Open the device an print file name          */
/***************************************************/

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




}   /* End of main() */
