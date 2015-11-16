/****************************************************************************
**
**      Filename:    bt_devname.c
**
**      Purpose:     All example programs that open the adaptor call the
**                   bt_devname() subroutine.  The program generates the
**                   requested device file name to be opened and figures the 
**                   correct address to use for the appropriate device type.
**                   
**                   Acceptable arguments:  -  the unit number (0,1,2,etc.),
**                                          -  the address requested,
**                                          -  the logical device to be opened,
**                                          -  and the default device name.
**
**      Functions:   bt_devname(),  bt_axsatoi(),  bt_axsname()
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

  static char revcntrl[] = "@(#) $Workfile:   bt_devname.c  $  $Revision:   1.15  $";

#endif  /* __STDC__ */
#endif  /* LINT */

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <limits.h>

#include <sys/types.h>
#include <sys/file.h>
  
#include <sys/btio.h>

/*****************************************************************************
**
**      Function:       bt_devname()
**
**      Purpose:        Used by all programs to generate a device name
**                      based on unit number, access type, address, and
**                      default device name.
**
**      Args:
**          unit        Base unit number to open.
**          addr_p      Pointer to address value needed to provide access. 
**          axstype     Requested base access type BT_AXSDP, BT_AXSRR, etc.
**          devname_p   Device name to open. (ie. /dev/bti /dev/bte)
**
**      Modifies:
**          unit            
**          axstype     Base access type
**          addr_p      Address needed to provide access
**
**      Returns:
**           NULL       If failure, pointer to device name if success.
**
**      Calls:          sprintf()
**
******************************************************************************/
  
static char devstrng[FILENAME_MAX];
  
/*
**   bt device driver name
*/
  
#ifdef PROTO_DEF

  char *bt_devname(int unit, u_long *addr_p, int axstype, char *devname_p)

#else   /* PROTO_DEF */

  char *bt_devname(unit, addr_p, axstype, devname_p)
  int unit;
  u_long *addr_p;
  int axstype;
  char *devname_p;

#endif  /* PROTO_DEF */

{
    /*
    **   Figure the address needed to provide the desired access.
    */
  
    if (addr_p != NULL) {
        if (*addr_p > (u_long) INT_MAX) {
            if ((axstype == BT_AXSRR) || (axstype == BT_AXSRE)) {
                axstype = BT_AXSRE;
                *addr_p -= ((u_long) INT_MAX) +1;
            } 
        } else {
            if (axstype == BT_AXSRE) {
                axstype = BT_AXSRR;
            } 
        }
    }
  
    /*
    **   Generate the unit name based on parameters issued.
    */
  
    unit += (axstype << BT_AXS_SHFT);
  
    sprintf(devstrng, "%s%d", devname_p, unit);
    return(devstrng);
}

/*****************************************************************************
**
**      Function:       bt_axsname()
**
**      Purpose:
**
**      Args:
**          unit        Base unit number to open.
**          addr_p      Pointer to addr_pess value. 
**          axstype_p   Pointer to requested base access type
**                         BT_AXSDP, BT_AXSRR, etc.
**          devname_p   Device name to open. "/dev/bti" "/dev/bte"
**
**      Modifies:
**          unit        May be modified
**          axstype_p   May be modified to reflect adjusted access type
**          addr_p      Address needed to provide access
**
**      Returns:
**           NULL       If failure, pointer to device name if success.
**
**      Calls:              sprintf()
**
******************************************************************************/
  
static char dvstrng[FILENAME_MAX];
  
#ifdef PROTO_DEF

  char *bt_axsname(int unit, u_long *addr_p, int *axstype_p, char *devname_p)

#else   /* PROTO_DEF */

  char *bt_axsname(unit, addr_p, axstype_p, devname_p)
  int unit;
  u_long *addr_p;
  int *axstype_p;
  char *devname_p;

#endif  /* PROTO_DEF */

{
    /*
    **   Figure the address needed to provide the desired access.
    */
  
    if (addr_p != NULL) {
        if (*addr_p > (u_long) INT_MAX) {
            if ((*axstype_p == BT_AXSRR) || (*axstype_p == BT_AXSRE)) {
                *axstype_p = BT_AXSRE;
                *addr_p -= ((u_long) INT_MAX) +1;
            } 
        } else {
            if (*axstype_p == BT_AXSRE) {
                *axstype_p = BT_AXSRR;
            }
        }
    }
  
    /*
    **   Build the unit name based on parameters issued.
    */
  
    unit += (*axstype_p << BT_AXS_SHFT);
  
    sprintf(dvstrng, "%s%d", devname_p, unit);
    return(dvstrng);
}
  
/****************************************************************************
**
**      Function:       bt_axsatoi()
**
**      Purpose:        To return the access type number based on an
**                      ascii string input.
**
**      Args:  
**          s1_p        String pointer of access type to evaluate
**
**      Modifies:
**          axstype     May be modified to reflect adjusted access type
**
**      Returns:        BT_MAX_AXSTYPS if invalid otherwise correct access
**                      type number.
**
**      Calls:          strcasecmp()
**
****************************************************************************/
  
#ifdef  PROTO_DEF

  enum BT_AXSTYPS bt_axsatoi( char *s1_p )

#else   /* PROTO_DEF */

  enum BT_AXSTYPS bt_axsatoi( s1_p )
  char *s1_p;

#endif  /* PROTO_DEF */

{
    enum BT_AXSTYPS axs_type;
  
    if (        (strcasecmp(s1_p, "BT_AXSIO")==0) ||
                (strcasecmp(s1_p, "AXSIO")==0)    ||
                (strcasecmp(s1_p, "IO")==0) ) {
        axs_type = BT_AXSIO;
    } else if ( (strcasecmp(s1_p, "BT_AXSDP")==0) ||
                (strcasecmp(s1_p, "AXSDP")==0)    ||
                (strcasecmp(s1_p, "DP")==0) ) {
        axs_type = BT_AXSDP;
    } else if ( (strcasecmp(s1_p, "BT_AXSRI")==0) ||
                (strcasecmp(s1_p, "AXSRI")==0)    ||
                (strcasecmp(s1_p, "A16")==0)      ||
                (strcasecmp(s1_p, "RI")==0) ) {
        axs_type = BT_AXSRI;
    } else if ( (strcasecmp(s1_p, "BT_AXSRR")==0) ||
                (strcasecmp(s1_p, "AXSRR")==0)    ||
                (strcasecmp(s1_p, "A32")==0)      ||
                (strcasecmp(s1_p, "RR")==0) ) {
        axs_type = BT_AXSRR;
    } else if ( (strcasecmp(s1_p, "BT_AXSRE")==0) ||
                (strcasecmp(s1_p, "AXSRE")==0)    ||
                (strcasecmp(s1_p, "RE")==0) ) {
        axs_type = BT_AXSRE;
    } else if ( (strcasecmp(s1_p, "BT_AXS24")==0) ||
                (strcasecmp(s1_p, "AXS24")==0)    ||
                (strcasecmp(s1_p, "A24")==0)      ||
                (strcasecmp(s1_p, "24")==0) ) {
        axs_type = BT_AXS24;
    } else if ( (strcasecmp(s1_p, "BT_AXSLM")==0) ||
                (strcasecmp(s1_p, "AXSLM")==0)    ||
                (strcasecmp(s1_p, "LM")==0) ) {
        axs_type = BT_AXSLM;
    } else {
        axs_type = BT_MAX_AXSTYPS;
    }
  
    return ( axs_type );
}
