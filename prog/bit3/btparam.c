/****************************************************************************
**
**      Filename:   btparam.c
**
**      Purpose:    The btparam example and utility program uses
**                  ioctl() to get or set any of a number of device
**                  parameters using BIOC_PARAM or BIOC_DEV_ATTRIB.
**                  btparam parses either the command line or
**                  standard input, reading zero or more commands, each
**                  command generating a call to ioctl().
**
**      Functions:      mnemonictol(), ltomnemonic(), useage(),
**                      print_syntax(), read_command(), parse_cla(),
**                      read_all_commands(), get_next_token(),
**                      get_next_line(), ioctl_error(), main()
**
**
**      $Revision:   1.9  $
**
*****************************************************************************/
/*****************************************************************************
**
**        Copyright (c) 1997 by SBS Technologies, Inc.
**        Copyright (c) 1995 by Bit 3 Computer Corporation.
**                     All Rights Reserved.
**              License governs use and distribution.
**
*****************************************************************************/

#ifndef LINT
#ifdef __STDC__
static const char revcntrl[] = "@(#)"__FILE__"  $Revision:   1.9  $";
#else   /* __STDC__ */
static char revcntrl[] = "@(#) $Workfile:   btparam.c  $  $Revision:   1.9  $";
#endif  /* __STDC__ */
#endif  /* __LINT__ */

#include    <stdio.h>
#include    <stdlib.h>
#include    <unistd.h>
#include    <fcntl.h>
#include    <ctype.h>
#include    <errno.h>
#include    <string.h>
#include    <sys/file.h>
#include    <sys/ioctl.h>
#include    <sys/param.h>
#include    <sys/types.h>
#include    <sys/stat.h>
#include    <limits.h>

#include    <sys/btio.h>





/*****************************************************************************
**
**    Local Data Typedefs
**
*****************************************************************************/

    /* information required to open and access the device driver */
    typedef struct {
        int filedes;       /* file handle, -1 == file has not been opened
                              yet. */
        char * filename;   /* file name to open. */
    } device_driver_t;

    /* Information used by get_next_token for input */
    typedef struct {
        /* where are we getting tokens from? */
        enum { COMMAND_LINE, STANDARD_IN } source;
        union {
            struct {
                /* Command Line */
                /* Copy of the command line (minus options) and an index
                   of the next argument to return. */
                int argc;
                char ** argv_pp;
                int argidx;
            } cmd_ln;
            struct {
                /* Standard In */
                int read_eof;  /* EOF has been read */
                int read_eoln; /* end of line has been read. */
            } std_in;
        } src_data;
    } token_input_t;   

    typedef struct {
        /* Data to map a constant name to a value */
        const char * const_name;   /* The name the constant has,
                                      NULL == end of list */
        u_long const_value;        /* The value the constant maps to. */
    } mnemonic_t;

    typedef struct {
        /* Data needed to deal with a command */
        const char * name_p;         /* The name of the command */
        u_long ioctl_arg;            /* The argument values to use with
                                        IOCTL */
        mnemonic_t * mnemonic_table;       /* The table of allowable constants
                                        == NULL if no argument is read
                                           (use default in ioctl_arg) */
    } command_t;




/*****************************************************************************
**
**    Local Function Prototypes
**
*****************************************************************************/
#ifdef PROTO_REF
    int mnemonictol(mnemonic_t *, const char *, u_long *);
    int ltomnemonic(mnemonic_t *, const char **, u_long);
    void useage(void);
    void print_syntax(command_t *);
    int read_command(token_input_t *, device_driver_t *);
    void parse_cla(int, char **, token_input_t *, device_driver_t *);
    void read_all_commands(token_input_t *, device_driver_t *);
    const char * get_next_token(token_input_t *);
    int get_next_line(token_input_t *);
    void ioctl_error(void);
    int main(int, char **);
#else /* PROTO_REF */
    int mnemonictol();
    int ltomnemonic();
    void useage();
    void print_syntax();
    int read_command();
    void parse_cla();
    void read_all_commands();
    const char * get_next_token();
    int get_next_line();
    void ioctl_error();
    int main();
#endif /* PROTO_REF */

/* prototypes from bt_devname.c */
#ifdef PROTO_REF
    char * bt_devname (int, u_long *, int, char *);
    enum BT_AXSTYPS bt_axsatoi (char *);
#else /* PROTO_REF */
    char * bt_devname();
    enum BT_AXSTYPS bt_axsatoi();
#endif /* PROTO_REF */




/*****************************************************************************
**
**    Global Data Definitions
**
*****************************************************************************/

    /* Special values */
#define CMD_HELP 1
#define CMD_QUIT 2

#ifndef MAX_LONG
#ifdef MAXLONG
#define MAX_LONG MAXLONG
#else /* MAXLONG */
#ifdef LONGMAX
#define MAX_LONG LONGMAX
#else /* LONGMAX */
#ifdef LONG_MAX
#define MAX_LONG LONG_MAX
#else /* LONG_MAX */
#define MAX_LONG ((~((unsigned long int) 0)) >> 1)
#endif /* LONG_MAX */
#endif /* LONGMAX */
#endif /* MAXLONG */
#endif /* MAX_LONG */

    mnemonic_t numeric_only[] = {
       {"max_long",        MAX_LONG},
       {"maxlong",         MAX_LONG},
       {NULL,              0}};

    mnemonic_t trace_lvl[] = {
       {"bt_trc_warn",     BT_TRC_WARN},
       {"bt_trc_error",    BT_TRC_ERROR},
       {"bt_trc_info",     BT_TRC_INFO},
       {"max_long",        MAX_LONG},
       {"maxlong",         MAX_LONG},
       {NULL,              0}};

    mnemonic_t boolean[] = {
        {"true",           TRUE},
        {"on",             TRUE},
        {"yes",            TRUE},
        {"y",              TRUE},
        {"t",              TRUE},
        {"false",          FALSE},
        {"off",            FALSE},
        {"no",             FALSE},
        {"n",              FALSE},
        {"f",              FALSE},
        {NULL,             0}};

    mnemonic_t data_size[] = {
        {"data8_siz",      DATA8_SIZ},
        {"d8",             DATA8_SIZ},
        {"d08",            DATA8_SIZ},
        {"data_8_size",    DATA8_SIZ},
        {"data16_siz",     DATA16_SIZ},
        {"d16",            DATA16_SIZ},
        {"data_16_size",   DATA16_SIZ},
        {"data32_siz",     DATA32_SIZ},
        {"d32",            DATA32_SIZ},
        {"data_32_size",   DATA32_SIZ},
        {"data_any_siz",   DATA_ANY_SIZ},
        {"data_any_size",  DATA_ANY_SIZ},
        {"d0",             DATA_ANY_SIZ},
        {"dany",           DATA_ANY_SIZ},
        {NULL,             0}};

    mnemonic_t addr_mod[] = {
        {"vme_a32_sb",     VME_A32_SB},
        {"block",          VME_A32_SB},
        {"sb",             VME_A32_SB},
        {"a32_sb",         VME_A32_SB},
        {"vme_a32_sp",     VME_A32_SP},
        {"sp",             VME_A32_SP},
        {"a32_sp",         VME_A32_SP},
        {"vme_a32_sd",     VME_A32_SD},
        {"vme_a32",        VME_A32_SD},
        {"sd",             VME_A32_SD},
        {"a32_sd",         VME_A32_SD},
        {"vme_a32_nb",     VME_A32_NB},
        {"nb",             VME_A32_NB},
        {"a32_nb",         VME_A32_NB},
        {"vme_a32_np",     VME_A32_NP},
        {"np",             VME_A32_NP},
        {"a32_np",         VME_A32_NP},
        {"vme_a32_nd",     VME_A32_ND},
        {"nd",             VME_A32_ND},
        {"a32_nd",         VME_A32_ND},
        {"vme_a24_sb",     VME_A24_SB},
        {"a24_sb",         VME_A24_SB},
        {"vme_a24_sp",     VME_A24_SP},
        {"a24_sp",         VME_A24_SP},
        {"vme_a24_sd",     VME_A24_SD},
        {"vme_a24",        VME_A24_SD},
        {"a24_sd",         VME_A24_SD},
        {"vme_a24_nb",     VME_A24_NB},
        {"a24_nb",         VME_A24_NB},
        {"vme_a24_np",     VME_A24_NP},
        {"a24_np",         VME_A24_NP},
        {"vme_a24_nd",     VME_A24_ND},
        {"a24_nd",         VME_A24_ND},
        {"vme_a16_sd",     VME_A16_SD},
        {"vme_a16",        VME_A16_SD},
        {"a16_sd",         VME_A16_SD},
        {"vme_a16_nd",     VME_A16_ND},
        {"a16_nd",         VME_A16_ND},
        {"vme_a32",        VME_A32},
        {"vme_a24",        VME_A24},
        {"vme_a16",        VME_A16},
        {"default",        0},
        {NULL,             0}};

    command_t bt_param_arg[] = {
#ifdef BT923
        /* These calls only exist on the 923 */
        {"hndshk_dr_count",  HNDSHK_DR_COUNT,  numeric_only},  
        {"hndshk_usec_time", HNDSHK_DR_COUNT,  numeric_only},  
#endif /* BT923 */
        {"trace_lvl",        TRACE_LVL,        trace_lvl},        
        {"intr_freeq_count", INTR_FREEQ_COUNT, numeric_only}, 
        {"reset_timer",      RESET_TIMER,      numeric_only},      
        {"pio_addr_mod",     PIO_ADDR_MOD,     addr_mod},     
        {"dma_addr_mod",     DMA_ADDR_MOD,     addr_mod},     
        {"rem_addr",         REM_ADDR,         numeric_only},         
        {"swap_bits",        SWAP_BITS,        numeric_only},        
        {"data_size",        DATA_SIZE,        data_size},        
        {"threshold",        THRESHOLD,        numeric_only},        
        {"dma_pause",        DMA_PAUSE,        boolean},        
        {"dma_poll_size",    DMA_POLL_SIZE,    numeric_only},
        {NULL,               0,                NULL},
        /* These commands only work with BIOC_DEV_ATTRIB */
        {"inf_unit",         INF_UNIT,         numeric_only},
        {"inf_intr_qcnt",    INF_INTR_QCNT,    numeric_only},
        {"inf_ldev_type",    INF_LDEV_TYPE,    numeric_only},
        {"inf_rdev_type",    INF_RDEV_TYPE,    numeric_only},
        {"inf_sig_tot",      INF_SIG_TOT,      numeric_only},
        {"inf_sig_err",      INF_SIG_ERR,      numeric_only},
        {"inf_sig_prg",      INF_SIG_PRG,      numeric_only},
        {"inf_sig_iack",     INF_SIG_IACK,     numeric_only},
        {"inf_bind_count",   INF_BIND_COUNT,   numeric_only},
        {"inf_bind_size",    INF_BIND_SIZE,    numeric_only},
        {NULL,               0,                NULL}};

    mnemonic_t command_list[] = {
        {"help", CMD_HELP},
        {"quit", CMD_QUIT},             
        {"exit", CMD_QUIT},             
        {"stop", CMD_QUIT},             
        {"done", CMD_QUIT},
        {NULL,   0}};


    int plen_g = 24;   /* page length -- for useage() */




/*****************************************************************************
**
**    Local Function Definitions
**
******************************************************************************/



/*****************************************************************************
**
**      Function:   mnemonictol()
**
**      Purpose:    Converts a mnemonic to a long by looking it up in a
**                  given table.
**
**      Args:       table                 Table of mnemonic name/value
**                                        pairs
**                  token                 mnemonic to look up in the
**                                        table
**                  value                 location to store the looked-
**                                        up value
**
**      Returns:    int
**                  TRUE                  if the value was found in the
**                                        table
**                  FALSE                 if the value was not found.
**
**      Calls:      
**
**      Notes:      The table end is marked by the name being a NULL
**                  pointer.
**
*****************************************************************************/
#ifdef PROTO_DEF
    int mnemonictol (mnemonic_t * table, const char * token, u_long * value)
#else /* PROTO_DEF */
    int mnemonictol (table, token, value)
        mnemonic_t * table;
        const char * token;
        u_long * value;
#endif /* PROTO_DEF */
    {
        if (table == NULL) {
            return (FALSE);
        }
        while (table->const_name != NULL) {
            if (strcasecmp(token, table->const_name) == 0) {
                *value = table->const_value;
                return (TRUE);
            }
            table+= 1;
        }
        return (FALSE);
    }




/*****************************************************************************
**
**      Function:   ltomnemonic()
**
**      Purpose:    Converts a long to a mnemonic by looking it up in a given
**                  table.
**
**      Args:       table                 Table of mnemonic name/value pairs
**                  token                 location to store the mnemonic
**                                        name (pointer to char)
**                  value                 value to look up in the table
**
**      Returns:    int
**                  TRUE                  if the value was found in the
**                                        table
**                  FALSE                 if the value was not found.
**
**      Calls:      
**
**      Notes:      The procedure will return the first mnemonic found in the
**                  table if multiple mnemonics have the same value.
**                  The table end is marked by the name being a NULL pointer.
**
*****************************************************************************/
#ifdef PROTO_DEF
    int ltomnemonic (mnemonic_t * table, const char ** name_p, u_long value)
#else /* PROTO_DEF */
    int ltomnemonic (table, name_p, value)
        mnemonic_t * table;
        const char ** name_p;
        u_long value;
#endif /* PROTO_DEF */
    {
        if (table == NULL) {
            return (FALSE);
        }
        while (table->const_name != NULL) {
            if (table->const_value == value) {
                *name_p = table->const_name;
                return (TRUE);
            }
            table += 1;
        }
        return (FALSE);
    }





/*****************************************************************************
**
**      Function:   useage()
**
**      Purpose:    Prints the (longish) useage message.
**
**      Args:       none
**
**      Returns:    void
**
**      Calls:      puts(), getchar(), fputs()
**
**      Notes:      use plen_g- a global variable.  Should be the page length
**                  or -1 for no pageing.
**
*****************************************************************************/
#ifdef PROTO_DEF
    void useage (void)
#else /* PROTO_DEF */
    void useage()
#endif /* PROTO_DEF */
    {
        int idx = 0, chr;

        static char *(message[]) = {
        "Purpose:",
        "btparam is used to set the various BIOC_PARAM parameters of the",
        "Bit3 device driver.",
        "",
        "Valid forms of btparam are:",
        "",
        "    btparam [options] <command>",
        "",
        "    btparam [options]",
        "    : <command> [value]",
        "    : <command> [value]",
        "    ...",
        "    : quit",
        "",
        "    btparam [options] < filename",
        "",
        "Valid options are:",
        "    -f <filename>      Access device driver <filename>",
        "    -u <unit_number>   Access card number <unit_number>",
        "    -t <access_type>   Access logical device <access_type>, one of:",
        "                           BT_AXSIO, BT_AXSDP, BT_AXSRI, BT_AXSRR,",
        "                           BT_AXSRE, BT_AXS24, BT_AXSLM",
        "",
        "valid commands and value mnemonics are:",
        "command:                Valid value mnemonics:",
        "intr_freeq_count,       MAX_LONG",
        "reset_timer, rem_addr,",
        "swap_bits, threshold,",
#ifdef BT923
        "dma_poll_size,",
        "hndshk_dr_count,",
        "hndshk_usec_time,",
#else /* BT923 */
        "dma_poll_size",
#endif /* BT923 */
        "",
        "addr_mod,               vme_a32_sb, vme_a32_sp, vme_a32_sd,",
        "pio_addr_mod,           vme_a32_nb, vme_a32_np, vme_a32_nd,",
        "dma_addr_mod            vme_a24_sb, vme_a24_sp, vme_a24_sd,",
        "                        vme_a24_nb, vme_a24_np, vme_a24_nd,",
        "                        vme_a16_sd, vme_a16_nd",
        "",
        "data_size               data8_siz, data16_siz, data32_siz,",
        "                        data_any_size",
        "",
        "dma_pause               true, false",
        "",
        "The following values can be read (using BIOC_DEV_ATTRIB), but cannot",
        "be set (using BIOC_PARAM).  As commands they cannot be followed by",
        "a value.",
        "",
        "    inf_unit, inf_intr_qcnt, inf_ldev_type, inf_rdev_type, ",
        "    inf_sig_tot, inf_sig_err, inf_sig_prg, inf_sig_iack, ",
        "    inf_bind_count, inf_bind_size",
        "",
        "At the shell command prompt, type \"btparam help | more\" for a",
        "paged version of this help message.",
        "",
        "For more information, please consult your Bit3 Support Software Manual.",
        "",
        "Type the command quit to exit the program.",
        "",
        NULL };

        while (message[idx] != NULL) {
            puts(message[idx++]);
            if (plen_g != -1) {
                if (idx % (plen_g - 1) == (plen_g - 2)) {
                    fputs("[Press ENTER to continue]", stdout);
                    while (((chr = getchar()) != '\n') && (chr != EOF));
                }
            }
        }
    }




/*****************************************************************************
**
**      Function:   print_syntax()
**
**      Purpose:    Prints the syntax of a given command.
**
**      Args:       command          Command to print syntax for.
**
**      Returns:    void
**
**      Calls:      puts(), printf(), putchar(), strlen() 
**
*****************************************************************************/
#ifdef PROTO_DEF
    void print_syntax (command_t * command)
#else /* PROTO_DEF */
    void print_syntax (command)
        command_t * command;
#endif /* PROTO_DEF */
    {
        int idx, llen;
        printf ("Use \"%s\" to see the value, use \"%s <value>\" to set the value.",
                command->name_p, command->name_p);
        puts("The value can be numeric or one of the following mnemonics:");
        llen = 0;
        for (idx = 0; command->mnemonic_table[idx].const_name != NULL; ++idx) {
            llen += strlen(command->mnemonic_table[idx].const_name) + 2;
            if (llen > 80) {
                putchar('\n');
                llen = strlen(command->mnemonic_table[idx].const_name) + 2;
            }
            printf("%s, ", command->mnemonic_table[idx].const_name);
        }
    }




/*****************************************************************************
**
**      Function:   read_command()
**
**      Purpose:    reads a single command from either the command line or from
**                  standard input (using get_next_token).
**
**      Args:       token_input_t * t_in_p
**                                Data for where to read tokens from (to be
**                                passed to get_next_token)
**                  device_driver_t * device_p
**                                Device driver info- file handle and file
**                                name to open (if it hasn't been opened
**                                already)
**
**      Returns:    exit status
**                      TRUE if all commands have _not_ been processed
**                          (call routine again)
**                      FALSE if all commands have been processed
**                          (do not call routine again)
**
**      Calls:      
**
**      Modifies:   call to get_next_token overwrites it's static buffer.
**
*****************************************************************************/
#ifdef PROTO_DEF
    int read_command (token_input_t * t_in_p, device_driver_t * device_p)
#else /* PROTO_DEF */
    int read_command(t_in_p, device_p)
        token_input_t * t_in_p;
        device_driver_t * device_p;
#endif /* PROTO_DEF */
    {
        const char * curr_token_p;    /* pointer to the current token */
        u_long command;
        int ioctl_cmd;                /* command for ioctl- 
                                         FALSE == use BIOC_PARAM
                                         TRUE == use BIOC_DEV_ATTRIB */
        bt_param_t ioctl_call;        /* values to be passed to the device 
                                         driver via ioctl() */
        const char * value_mnemonic;  /* The name of the value returned from
                                         ioctl(BIOC_DEV_ATTRIB) */
        int idx;                      /* Loop index */

        /* get the next token to process */
        curr_token_p = get_next_token(t_in_p);

        /* check for end of command */
        if (curr_token_p == (const char *)NULL) {
            return (TRUE);
        }

        /* Which command is it? */
        if (mnemonictol(command_list, curr_token_p, &command) == TRUE) {
            switch (command) {
                case CMD_HELP:
                    useage();
                    return (TRUE);
                case CMD_QUIT:
                    return (FALSE);
                default:
                    /* assume it's an ioctl command */
                    break;
            }
        }

        /* only ioctl commands should get this far */
        ioctl_cmd = FALSE;

        /* Find the parameter in bt_param_arg[] */
        for (idx = 0; bt_param_arg[idx].name_p != NULL; ++idx) {
            if (strcasecmp(bt_param_arg[idx].name_p, curr_token_p) == 0) {
                break; /* for loop */
            }
        }


        if (bt_param_arg[idx].name_p == NULL) {
            /* The parameter was not in bt_param_arg[] */
            ioctl_cmd = TRUE;
            /* check the list of bioc_dev_attrib-only commands */
            for (idx++; bt_param_arg[idx].name_p != NULL; ++idx) {
                if (strcasecmp(bt_param_arg[idx].name_p, curr_token_p) == 0) {
                    break; /* for loop */
                }
            }
        }
        if (bt_param_arg[idx].name_p == NULL) {
            /* print help message and return */
            useage();
            return (TRUE);
        }

        ioctl_call.param = bt_param_arg[idx].ioctl_arg;

        curr_token_p = get_next_token (t_in_p);
        if (curr_token_p == NULL) {
            if (ioctl_cmd == FALSE) {
                ioctl_cmd = TRUE;
            }
        } else {
            if (ioctl_cmd == TRUE) {
                printf ("btparam error: Command %s cannot be given a value.\n",
                    bt_param_arg[idx].name_p);
                return (TRUE);
            }
            /* We need to read an argument */
            /* Read ioctl_call.value, with the given mnemonic list */
            if (mnemonictol(bt_param_arg[idx].mnemonic_table, curr_token_p,
                            &ioctl_call.value) == FALSE) {
                if (isdigit(curr_token_p[0])) {
                    ioctl_call.value = strtol(curr_token_p, NULL, 0);
                } else {
                    print_syntax(bt_param_arg + idx);
                    return (TRUE);
                }
            }
        }
       
        /* Is the device driver open? */
        if (device_p->filedes == -1) {
            if ((device_p->filedes = open (device_p->filename, O_RDWR)) < 0) {
                perror (device_p->filename);
                exit (errno);
            }
        }

        /* use ioctl to call set the appropriate paramter */
        puts ("Executing ioctl() command.");
        if (ioctl_cmd == TRUE) {
            if (ioctl(device_p->filedes, BIOC_DEV_ATTRIB, &ioctl_call)) {
                ioctl_error();
                return (TRUE);
            }
        } else {
            if (ioctl(device_p->filedes, BIOC_PARAM, &ioctl_call)) {
                ioctl_error();
                return (TRUE);
            }
        }

        if (ioctl_cmd == TRUE) {
            if (ltomnemonic(bt_param_arg[idx].mnemonic_table, &value_mnemonic,
                            ioctl_call.value) == FALSE) {
                if ((ioctl_call.param == INF_LDEV_TYPE) ||
                    (ioctl_call.param == INF_RDEV_TYPE)) {
                    /* special case for device types- print as decimal */
                    printf ("The value of %s is %d\n", 
                            bt_param_arg[idx].name_p, ioctl_call.value);
                } else {
                    printf ("The value of %s is 0x%0lX\n", 
                            bt_param_arg[idx].name_p,
                            ioctl_call.value);
                }
            } else {
                printf ("The value of %s is %s\n", bt_param_arg[idx].name_p,
                        value_mnemonic);
            }
        }
            
        /* continue process commands. */
        return (TRUE);

    }




/*****************************************************************************
**
**      Function:   parse_cla()
**
**      Purpose:    find and parse all the command line options,
**                  Copy all the command line arguments which are not options
**                  into argc_g and argv_gpp, and
**                  open the appropriate device driver file.
**
**      Args:       argc        from main()- number of command line arguments
**                  argv        from main()- values of command line arguments
**                  t_in_p      information for get_next_token- where to read
**                              from (initlaized in this function)
**                  device_p    information for read_command()- device driver
**                              file name (initialized in this function)
**
**      Returns:    void
**
**      Calls:      useage(), bt_axsatoi(), strtol(), bt_devname()
**
**      Modifies:   argc_g and argv_gpp (initializes)
**
*****************************************************************************/
#ifdef PROTO_DEF
    void parse_cla(int argc, char ** argv, token_input_t *t_in_p,
                    device_driver_t * device_p)
#else /* PROTO_DEF */
    void parse_cla (argc, argv, t_in_p, device_p)
        int argc;
        char ** argv;
        token_input_t * t_in_p;
        device_driver_t * device_p;
#endif /* PROTO_DEF */
    {

        int idx; /* loop variable */

        /* These variables get set with different command line options,
           and help determine which dvice file to open */
        char            *file_p = BT_DEVNAME;
        int             unit = 0;
        enum BT_AXSTYPS axs_type = BT_AXSIO;

        /* As we know there is not more than one command on the command
           line (and as it is not more than two tokens long), we can
           statically allocate space for the modified argc and argv. */
        char **largv;

        /* Assume we do not have commands on the command line unless proven
           otherwise. */
        t_in_p->source = STANDARD_IN;
        t_in_p->src_data.std_in.read_eof = FALSE;
        t_in_p->src_data.std_in.read_eoln = FALSE;


        /* scroll through arguments */
        for (idx = 1; idx < argc; ++idx) {
            if (argv[idx][0] != '-') {
                /* it is not an option */
                if (t_in_p->source == STANDARD_IN) {
                    /* We need to initialize t_in_p and largv */
                    largv = (char **) calloc (2 + argc - idx, sizeof(char *));
                    t_in_p->source = COMMAND_LINE;
                    t_in_p->src_data.cmd_ln.argc = 1;
                    t_in_p->src_data.cmd_ln.argv_pp = largv;
                    t_in_p->src_data.cmd_ln.argidx = 1;
                    plen_g = -1; /* do not page */
                }
                largv[t_in_p->src_data.cmd_ln.argc] = argv[idx];
                t_in_p->src_data.cmd_ln.argc += 1;
            } else {
                /* assume it is an option */
                switch (argv[idx][1]) {
                   /* To add an option, add the appropriate case statment
                      here */
                   case 'f':
                       /* -f <file_name> option- open a given file. */
                       if (idx < (argc - 1)) {
                           file_p = argv[++idx];
                        } else {
                           useage();
                        }
                       break;
                   case 't':
                       /* -t <axs_type> option - access with a given type */
                       if (idx < (argc - 1)) {
                           axs_type = bt_axsatoi (argv[++idx]);
                       } else {
                           useage();
                       }
                       break;
                   case 'u':
                       /* -u <unit> option - access a given card */
                       if (idx < (argc - 1)) {
                           if (isdigit(argv[++idx][0])) {
                              unit = (int) strtol(argv[idx], NULL, 0);
                           } else {
                              useage();
                           }
                       } else {
                           useage();
                       }
                       break;
                   case 's':
                       /* -sl <len> option- give screen length */
                       if ((argv[idx][2] == 'l') && (plen_g != -1) && 
                           (idx < (argc - 1)) && (isdigit(argv[++idx][0]))) {
                           plen_g = (int) strtol(argv[idx], NULL, 0);
                           break;
                       }
                       useage(); /* all the else branches are the same */
                       break;
                   default:
                       useage();
                       break;
                }
            }
        }

        /* set device_name so that read_command can (if nessecary) open
           the proper device driver */
        device_p->filedes = -1; /* device driver has not been opened yet. */
        device_p->filename = bt_devname(unit, NULL, axs_type, file_p);

        /* if we are not reading in from a terminal, do not page */
        if (!isatty(0)) {
           plen_g = -1;
        }

    }






/*****************************************************************************
**
**      Function:   read_all_commands()
**
**      Purpose:    Loops calling read_command() until all commands have been
**                  processed or input is finished.  It needs to be called
**                  twice- once to process the command line, once to process
**                  the input.
**
**      Args:       t_in_p      token source, passed to read_command() and
**                              get_next_line()
**                  device_p    device driver file information, passed to
**                              read_command()
**
**      Returns:    void
**
**      Calls:      get_next_line(), read_command(), isatty(), printf()
**
*****************************************************************************/
#ifdef PROTO_DEF
    void read_all_commands (token_input_t * t_in_p, device_driver_t * device_p)
#else /* PROTO_DEF */
    void read_all_commands (t_in_p, device_p)
        token_input_t * t_in_p;
        device_driver_t * device_p;
#endif /* PROTO_DEF */
    {

        if (t_in_p->source == STANDARD_IN) {
            /* See if we're on a terminal */
            if (isatty(0)) {
                /* print the initial prompt. */
                printf (": ");
            }
        }

        while (read_command(t_in_p, device_p)) {
            /* consume past the end of command (which get_next_token() 
               will not do) */
            if (get_next_line(t_in_p) == FALSE) {
                /* there is no next line- input has been consumed. */
                return;
            }
            /* if nessecary, print prompt */
            if (t_in_p->source == STANDARD_IN) {
                if (isatty(0)) {
                    printf (": ");
                }
            }
        }
    }




/*****************************************************************************
**
**      Function:   get_next_token()
**
**      Purpose:    read the next token from the input (either the command
**                  line or standard input) into a static buffer.  If the
**                  token is read successfully, a pointer to the staic buffer
**                  is returned, otherwise (on end of input or end of command)
**                  NULL is returned.  It will not read pst the end of a
**                  command (past the end of line or a semicolon argument)-
**                  get_next_line will do that.
**
**      Args:       token_input_t       Information about where to get
**                                      input from (command line or stdin).
**
**      Returns:    char *              pointer to token
**                                      either a pointer to the static buffer
**                                      containing the token or NULL
**
**      Calls:      getchar(), isspace()
**
**
*****************************************************************************/

#define MAX_TOKEN_SIZE 132  /* size of the buffer to allocate */

#ifdef PROTO_DEF
    const char * get_next_token (token_input_t *t_in_p)
#else /* PROTO_DEF */
    const char * get_next_token(t_in_p)
        token_input_t *t_in_p;
#endif /* PROTO_DEF */
    {
        static char buffer[MAX_TOKEN_SIZE];
        int idx;
        int chr;
        if (t_in_p->source == COMMAND_LINE) {
            /* get tokens from argc & argv */

            /* check for end of input */
            if (t_in_p->src_data.cmd_ln.argidx >=
                t_in_p->src_data.cmd_ln.argc) {
                return ((const char *) NULL);
            }

            /* return approprate argument */
            t_in_p->src_data.cmd_ln.argidx++;
            return (t_in_p->src_data.cmd_ln.argv_pp[t_in_p->src_data.cmd_ln.argidx-1]);
        } else {
            /* skip whitespace */
            do {
               if ((t_in_p->src_data.std_in.read_eof == FALSE) &&
                   (t_in_p->src_data.std_in.read_eoln == FALSE)) {
                   chr = getchar();
                   if (chr == EOF) {
                       t_in_p->src_data.std_in.read_eof = TRUE;
                       return ((const char *) NULL);
                   }
               } else {
                   return ((const char *) NULL);
               }
               while (isspace(chr)) {
                   /* check for end of line (end of command) condition */
                   if (chr == '\n') {
                       /* push the end of line back into the input so it 
                          will be read again. */
                       t_in_p->src_data.std_in.read_eoln = TRUE;
                       return ((const char *) NULL);
                   }

                   chr = getchar();

                   /* check for end of input */
                   if (chr == EOF) {
                       t_in_p->src_data.std_in.read_eof = TRUE;
                       return ((const char *) NULL);
                   }
                }
                if (chr == '\\') {
                    /* check for quoted character */
                    chr = getchar();
                    if (chr == '\n') {
                        /* force quoted end of line to be space */
                        chr = ' ';
                    } else if (chr == EOF) {
                        t_in_p->src_data.std_in.read_eof = TRUE;
                        return ((const char *) NULL);
                    }
                }
            } while (isspace(chr));

            /* while the read in character is not white space... */
            for (idx = 0; !isspace(chr); ++idx) {
                /* check for EOF */
                if (chr == EOF) {
                    t_in_p->src_data.std_in.read_eof = TRUE;
                    break; /* for loop */
                }
                /* check for end of line */
                if (chr == '\n') {
                    t_in_p->src_data.std_in.read_eoln = TRUE;
                    break; /* for loop */
                }
                /* copy it into the buffer */
                if (idx < (MAX_TOKEN_SIZE - 1)) {
                    if (chr == '\\') {
                        /* we have a quoted character */
                        buffer[idx] = getchar();
                        if (buffer[idx] == EOF) {
                            t_in_p->src_data.std_in.read_eof = TRUE;
                            break; /* for loop */
                        }
                    } else {
                        buffer[idx] = chr;
                    }
                } else {
                    /* Note: when we have run ot of buffer space, characters 
                       just get dropped on the floor.  Make sure we drop the 
                       correct number on the floor. */
                    if (chr == '\\') {
                        if (getchar() == EOF) {
                            t_in_p->src_data.std_in.read_eof = TRUE;
                            break; /* for loop */
                        }
                    }
                }
                /* read in next character */
                chr = getchar();
            }

            if (chr == '\n') {
                t_in_p->src_data.std_in.read_eoln = TRUE;
            } else if (chr == EOF) {
                t_in_p->src_data.std_in.read_eof = TRUE;
            }

            if (idx > 0) {
                /* append null and return buffer */
                buffer[idx] = '\0';
                return ((const char *) buffer);
            } else {
                return ((const char *) NULL);
            }
        }
    }





/*****************************************************************************
**
**      Function:   get_next_line()
**
**      Purpose:    Reads past the end of command (end of line or semicolon)
**                  so that the next call to get_next_tone will start on
**                  the next command.  It also resets token_input.
**
**      Args:       token_input_t *     Token input sorce (for get_next_token())
**
**      Returns:    int
**                  TRUE                if end of not input reached
**                                      (parse new command)
**                  FALSE               if end of input reached
**                                      (no new command to parse)                                     
**
**      Calls:      getchar()
**
*****************************************************************************/
#ifdef PROTO_DEF
    int get_next_line (token_input_t * t_in_p)
#else /* PROTO_DEF */
    int get_next_line (t_in_p)
        token_input_t * t_in_p;
#endif /* PROTO_DEF */
    {
        int chr;

        /* check source of tokens */
        if (t_in_p->source == COMMAND_LINE) {
            /* only one command is allowed on the command line. */
            return (FALSE);

        } else {

             /* tokens are comming from stdin.  If we have hit eof, we
                have end of input */
            if (t_in_p->src_data.std_in.read_eof == TRUE) {
                return (FALSE);
            }

            /* If we have detected end of line previously, all we need to
               do is clear the end of line marker and return. */
            if (t_in_p->src_data.std_in.read_eoln == TRUE) {
                t_in_p->src_data.std_in.read_eoln = FALSE;
                return (TRUE);
            }

            /* Look for end of line (always watching out for end of file) */
            chr = getchar();
            if (chr == EOF) {
                return (FALSE);
            }
            while (chr != '\n') {
               if (chr == '\\') {
                  /* skip quoted character if it isn't eof */
                  chr = getchar();
                  if (chr == EOF) {
                     return (FALSE);
                  }
               }
               chr = getchar();
               if (chr == EOF) {
                   return (FALSE);
               }
            }
        }
        return (TRUE);
    }




/*****************************************************************************
**
**      Function:   ioctl_error()
**
**      Purpose:    handles any error from calling ioctl()
**
**      Args:       none
**
**      Returns:    void                if non-fatal error
**                  does not return     if fatal error
**
**      Calls:      perror(), fprintf(), exit()
**
*****************************************************************************/
#ifdef PROTO_DEF
    void ioctl_error(void)
#else /* PROTO_DEF */
    void ioctl_error()
#endif /* PROTO_DEF */
    {
        /* print out standard error message */
        perror ("btparam");

        /* See what really happened: */
        switch (errno) {
            case EINVAL:
            case ENOTTY:
                /* bad parameter or argument- not fatal. */
                fprintf (stderr, "Illegal parameter or argument error.\n");
                return;
            case EACCES:
                /* Access to command denied- not fatal. */
                fprintf (stderr, "Access denied.\n");
                return;
            case EIO:
                /* Adapter had hardware problem */
                fprintf (stderr, "Adapter Error.\n");
                exit (errno);
            case EBUSY:
                /* Adapter was busy- try command again. */
                fprintf (stderr, "Adapter Busy- please try command again.\n");
                exit (errno);
            default:
                /* Unknown or other error value- nothing to do but exit. */
                exit (errno);
        }
    }




/*****************************************************************************
**
**      Function:   main()
**
**      Purpose:    main body of program
**
**      Args:       argc        Number of command line argument
**                  argv        Command line argument value_ps
**
**      Returns:    int
**
**      Calls:      parse_cla(), read_all_commands(), close()      
**
*****************************************************************************/
#ifdef PROTO_DEF
    int main (int argc, char **argv)
#else /* PROTO_DEF */
    
    int main (argc, argv)
        int argc;
        char ** argv;
#endif /* PROTO_DEF */
    {
        token_input_t tokn_in;
        device_driver_t devdr;

        /* Parse command line options and open the device driver. */
        parse_cla (argc, argv, &tokn_in, &devdr);

        /* Read commands on the command line. */
        read_all_commands(&tokn_in, &devdr);

        /* close the device driver */
        if (devdr.filedes != -1) {
            close(devdr.filedes);
        }

        /* free any memory allocated for tokn_in */
        if (tokn_in.source == COMMAND_LINE) {
            free(tokn_in.src_data.cmd_ln.argv_pp);
        }

        return (0);

    }

