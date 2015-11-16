/******************************************************************************
**
**      Filename:       bit3_perror.c
**
**      Purpose:        Routine to check the status of the Bit 3 device driver
**                      and print an error message to 'stderr' if there were
**                      any status errors.
**
**      Functions:      ioctl(), printf(), fprintf()
**
**      $Revision:   1.10  $
**
******************************************************************************/
/*****************************************************************************
**
**        Copyright (c) 1990,1991 by Bit 3 Computer Corporation.
**                     All Rights Reserved.
**              License governs use and distribution.
**
*****************************************************************************/

#ifndef LINT
#ifdef __STDC__

  static const char revcntrl[] = "@(#)"__FILE__"  $Revision:   1.10  $ "__DATE__;

#else   /* __STDC__ */

  static char revcntrl[] = "@(#) $Workfile:   bit3_perror.c  $  $Revision:   1.10  $";

#endif  /* __STDC__ */
#endif  /* LINT */
  
#include        <stdio.h>
#include	<unistd.h>
#include        <errno.h>
#include        <sys/btio.h>
  
/******************************************************************************
**
**      Function:       bit3_perror()
**
**      Purpose:        Calls the BIOC_STATUS ioctl() command and checks
**                      the local status register for errors.  Prints a
**                      message to stderr if any errors are detected.
**
**      Args:           chan            File channel device was opened on.
**
**      Returns:        0               No errors.
**                      1               If status errors.
**
*****************************************************************************/
  
int     bit3_perror(chan)
int     chan;
{
    bt_status_t         data;
  
    if (ioctl(chan, BIOC_STATUS, &data)) {
        printf("BIOC_STATUS returned errno = %d.\n", errno);
        return(1);
    }
  
    if (data &= BT_STATUS_MASK) {
        fprintf(stderr,"Status error 0x%2.2x:\n", data>>BT_INTR_ERR_SHFT);
        if (data & BT_INTR_POWER) {
            fprintf(stderr,"\tRemote chassis off or cable disconnected.\n");
        } else {
            if (data & BT_INTR_PARITY)
                fprintf(stderr,"\tInterface parity error.\n");
            if (data & BT_INTR_REMBUS)
                fprintf(stderr,"\tRemote bus error.\n");
            if (data & BT_INTR_TIMEOUT)
                fprintf(stderr,"\tInterface timeout.\n");
        }
        return (1);
    }
    return (0);
}                               /* end of bit3_perror() */
  
