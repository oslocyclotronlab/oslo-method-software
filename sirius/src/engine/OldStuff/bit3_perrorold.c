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
  
int     bit3_perror(int chan)
{

    bt_status_t         data;
	
    char		err1[1024]="*** ERROR *** ENGINE: BIOC_STATUS returned errno = ";
    char		err2[1024]="*** ERROR *** ENGINE: Status error";
    char		err3[1024]="*** ERROR *** ENGINE: Remote chassis OFF or cable disconnected";
    char		err4[1024]="*** ERROR *** ENGINE: Interface parity error";
    char		err5[1024]="*** ERROR *** ENGINE: Remote bus error";
    char		err6[1024]="*** ERROR *** ENGINE: Interface timeout";


  
    if (ioctl(chan, BIOC_STATUS, &data)) {
        printf("%s%d.\n",err1,errno);
        return(1);
    }
  
    if (data &= BT_STATUS_MASK) {

        printf("%s 0x2.2x:\n",err1,data>>BT_INTR_ERR_SHFT);
        if (data & BT_INTR_POWER) {
            printf("%s\n",err3);
        } else {
            if (data & BT_INTR_PARITY)
                printf("%s\n",err4);
            if (data & BT_INTR_REMBUS)
                printf("%s\n",err5);
            if (data & BT_INTR_TIMEOUT)
                printf("%s\n",err6);
        }
        return (1);
    }
    return (0);
}                               /* end of bit3_perror() */
  
