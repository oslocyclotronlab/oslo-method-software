/*****************************************************************************
**
**      Filename:   readmem.c
**
**      Purpose:    This example program uses the SBS Mirror API
**                  routines to access memory using the bt_read() function.
**                  The program is similar to dumpmem but uses a different
**                  method to transfer data.
**               
**                  The length argument specifies the number of bytes to read.
**                  The first of up to 256 bytes of data are displayed.
**
**      $Revision: 2.25 $
**
*****************************************************************************/
/*****************************************************************************
**
**        Copyright (c) 1997-2005 by SBS Technologies, Inc.
**        Copyright (c) 1996 by Bit 3 Computer Corporation.
**                     All Rights Reserved.
**              License governs use and distribution.
**
*****************************************************************************/

#ifndef LINT
static const char revcntrl[] = "@(#)"__FILE__"  $Revision: 2.25 $ "__DATE__;
#endif  /* LINT */

#include    <stdio.h>
#include    <string.h>
#include    <stdlib.h>
#include    <ctype.h>
#include    <errno.h>

#include    "btapi.h"

#define NUM_LINES  16   /* number of lines of memory to display on screen */
#define NUM_BYTES  16   /* number of bytes to display on each line of screen */
#define DEF_LENGTH 256  /* default length of the read */
#define DEF_ADDR   0x00000000 /* default address */
#define DEF_UNIT   0          /* default unit number */


/*****************************************************************************
**
**      Function prototypes
**
*****************************************************************************/

static void usage(char *prog_p);


/*****************************************************************************
**
**      Program:    readmem
**
**      Purpose:    To read and display data as both
**                  hexadecimal bytes and ASCII characters.
**
**      Args:
**          -               Displays the command summary.
**          -a  <addr>      Address to start data transfer at.
**          -l  <length>    Number of bytes to read.
**          -t  <logdev>    Logical device. (BT_AXSLM, BT_AXSDP, etc.)
**          -u  <unit>      Unit number to open.  Default is unit 0.
**
*****************************************************************************/
int main(
    int argc, 
    char **argv
    )
{
    bt_error_t      status;        /* Bit 3 library error return type */
    char            *prog_p = argv[0];
    size_t          my_index = 0;
    bt_desc_t       btd;            /* Unit descriptor for Bit 3 library */
    bt_data8_t      data;
    size_t          offset;
    unsigned long   remote_address = DEF_ADDR;
    int             unit = DEF_UNIT;
    bt_dev_t        type = BT_DEV_DEFAULT;
    char            *buffer_p = NULL;
    char            buff[NUM_BYTES+1];
    int             errval;         /* temp spot for errno */
    size_t          amt_read;       /* Amount of data read from device */
    size_t          buffer_size = DEF_LENGTH;    /* Amount we want to read */
    size_t          byte_count;
    char            devname[BT_MAX_DEV_NAME];    /* Device to open */
    char            *base_buffer_p = NULL;

    /*  Parse the command line */
    argc--;
    while (argv++, argc--) {

        /* Check to see if this is a command line option */
        if (**argv == '-') {

            switch (*(*argv+1)) {

              case 'a':  /* remote address */
                  argv++;
                  argc--;
                  remote_address = strtoul(*argv, NULL, 0);
                  break;

              case 'l':  /* length */
                  argv++;
                  argc--;
                  buffer_size = strtoul(*argv, NULL, 0);
                  break;

              case 't':  /* Logical device type */
                  argv++;
                  argc--;
                  type = bt_str2dev(*argv);
                  if (type >= BT_MAX_DEV) {
                      fprintf(stderr, "Invalid access type: %s\n", *argv);
                      usage(prog_p);
                      return EXIT_FAILURE;
                  }
                  break;

              case 'u': /* unit number */
                  argv++;
                  argc--;
                  unit = strtol(*argv, NULL, 0);
                  break;

              default:
                  usage(prog_p);
                  return EXIT_FAILURE;

            } /*  end switch on argument type */

        } else {
            usage(prog_p);
            return EXIT_FAILURE;
        } /* end else bad argument */

    } /* end while more arguments */

    /*  Open the device  */
    status = bt_open(&btd,
        bt_gen_name(unit, type, &devname[0], BT_MAX_DEV_NAME),
        BT_RD | BT_WR);
    if (BT_SUCCESS != status) {
        bt_perror(btd, status, "Could not open the device");
        return EXIT_FAILURE;
    }

    /* Clear any outstanding errors */
    status = bt_clrerr(btd);
    if (BT_SUCCESS != status) {
        bt_perror(btd, status, "Could not clear errors from the device");
        (void) bt_close(btd);
        return EXIT_FAILURE;
    }

    /* get a buffer that is aligned on an 8 byte boudary. */
    base_buffer_p = (char *) malloc(buffer_size + 8);
    if (NULL == base_buffer_p) {
        errval = errno;
        perror("Could not allocate buffer");
        bt_close(btd);
        return(errval);
    }

    /* get to the next highest 8 byte address boundary. */
    buffer_p = (char *)((long) base_buffer_p + ((long) 8 - 
                ((long) base_buffer_p & 0x00000007)));

    memset(buffer_p, 0, buffer_size);

    /* Read in the data */
    status = bt_read(btd, buffer_p, remote_address, buffer_size, &amt_read);
    if ( BT_SUCCESS != status) {

        /* check if all the message was transfered */
        if (amt_read != buffer_size) {
            fprintf(stderr, "Warning: Only %d bytes read from device.\n", (int) amt_read);
        }
        else {
            fprintf(stderr, "Could not read from %s.\n", &devname[0]);
        }

        bt_perror(btd, status, "Could not read from the device");
        free(base_buffer_p);
        (void) bt_close(btd);
        return EXIT_FAILURE;
    }

    printf("Address = 0x%08lx\n", remote_address);

    /* Display the data */
    offset = remote_address % NUM_BYTES;
    byte_count = 0;
    while ( byte_count < NUM_LINES*NUM_BYTES && byte_count < buffer_size ) {

        /* Print spaces if it doesn't start at the beginning of the line. */
        for (my_index = 0; my_index < offset; my_index++) {
            buff[my_index] = ' ';
            printf("   ");
        }

        /* Print the data in hexadecimal format. */
        for ( ;
              my_index < NUM_BYTES && byte_count < NUM_LINES*NUM_BYTES && byte_count < buffer_size;
              my_index++, byte_count++ ) {
                    data = buffer_p[byte_count];
                    buff[my_index] = (isprint(data)) ? data : '.';
                    printf("%02x ", data);
        }

        /* Print spaces until the end of the line. */
        for (; my_index < NUM_BYTES; my_index++) {
            buff[my_index] = ' ';
            printf("   ");
        }

        buff[my_index] = '\0';

        /* Print the data as ASCII text */
        printf(" %s\n", &buff[0]);

        offset = 0;     /* All remaining lines are complete */

    } /* end while ( byte_count < NUM_LINES*NUM_BYTES && byte_count < buffer_size ) */

    free(base_buffer_p);

    /* Check for status errors */
    status = bt_chkerr(btd);
    if ( BT_SUCCESS != status) {
        bt_perror(btd, status, "Status error from the device");
        (void) bt_close(btd);
        return EXIT_FAILURE;
    }

    /* Close the device */
    status = bt_close(btd);
    if ( BT_SUCCESS != status) {
        bt_perror(btd, status, "Could not close the device");
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}


/*****************************************************************************
**
**      Function:   usage()
**
**      Purpose:    Prints command line arguments.
**
**      Args:       prog_p      Name of the program.  Usually argv[0].
**
**      Returns:    void
**
**      Calls:      fprintf()
**
*****************************************************************************/
static void    usage(
    char *prog_p
    )
{
    fprintf(stderr, 
"\n"
"    This example program uses the bt_read() Bit 3 Mirror API function to read\n"
"    from any of the Bit 3 logical devices. This program is similar to the\n"
"    dumpmem program, but uses a different method to transfer data.\n"
"\n");

     fprintf(stderr,
"    USAGE: %s [-] [-t logical] [-u unit] [-a address] [-l length]\n", prog_p);

     fprintf(stderr,
"\t -            Display this command summary.\n"
"\t -a <addr>    Address to start data transfer at. Default = 0x%08x\n"
"\t -l <length>  Number of bytes to read. Default is %d.\n"
"\t -t <logdev>  Logical device to access. (%s, %s, etc.)\n"
"\t -u <unit>    Unit number to open.  Default is unit %d.\n",
    DEF_ADDR,
    DEF_LENGTH,
    bt_dev2str(BT_DEV_DEFAULT),
    bt_dev2str((BT_DEV_DEFAULT == BT_DEV_MEM) ? BT_DEV_IO : BT_DEV_MEM),
    DEF_UNIT);


     fprintf(stderr,
"\n"
"\t(All numeric values use C radix notation)\n"
"\n"
"    EXAMPLE: Read the 1st 256 bytes of data from %s starting at\n"
"    address 0x00001000:\n", bt_dev2str(BT_DEV_DEFAULT));
    fprintf(stderr, 
"\n"
"    %s -a 0x00001000\n\n", prog_p);

}  /* end usage() */    
