/* Function wprintf() is used to display messages in the text output window for SIRIUS */
/* The function works like printf()                                                    */
/* Text output widget: text_w                                                          */
/* Taken from Motif Programming Manual Vol Six A, p 476                                */

#include <Xm/Text.h>
#include <stdio.h>
#include <varargs.h> 
	


    Widget text_w;	/* Output window widget name */

void 
wprint(va_alist)
va_dcl
{
    char msgbuf[1024];
    char *fmt;
    static XmTextPosition wpr_position;
    va_list args;

    va_start (args);
    fmt = va_arg (args, char *);
#ifndef NO_VPRINTF
    (void) vsprintf (msgbuf, fmt, args);
#else /* !NO_VPRINTF */
    {
        FILE foo;
        foo._cnt = 1024;
        foo._base = foo._ptr = msgbuf; /* (unsigned char *) ?? */
        foo._flag = _IOWRT+_IOSTRG;
        (void) _doprnt (fmt, args, &foo);
        *foo._ptr = '\0'; /* plant terminating null character */
    }
#endif /* NO_VPRINTF */
    va_end (args);

    XmTextInsert (text_w, wpr_position, msgbuf);
    wpr_position = wpr_position + strlen (msgbuf);
    XtVaSetValues (text_w, XmNcursorPosition, wpr_position, NULL);
    XmTextShowPosition (text_w, wpr_position);
}


/*-------------------------------------------------------------------------------------*/
/* Function errprint() is used to display messages in the error message window         */
/* The function works like printf()                                                    */
/* Text output widget: error_w                                                         */
/* Taken from Motif Programming Manual Vol Six A, p 476                                */

#include <Xm/Text.h>
#include <stdio.h>
#include <varargs.h> 
	


    Widget error_w;	/* Output window widget name */

void 
errprint(va_alist)
va_dcl
{
    char msgbuf[1024];
    char *fmt;
    static XmTextPosition wpr_position;
    va_list args;

    va_start (args);
    fmt = va_arg (args, char *);
#ifndef NO_VPRINTF
    (void) vsprintf (msgbuf, fmt, args);
#else /* !NO_VPRINTF */
    {
        FILE foo;
        foo._cnt = 1024;
        foo._base = foo._ptr = msgbuf; /* (unsigned char *) ?? */
        foo._flag = _IOWRT+_IOSTRG;
        (void) _doprnt (fmt, args, &foo);
        *foo._ptr = '\0'; /* plant terminating null character */
    }
#endif /* NO_VPRINTF */
    va_end (args);

    XmTextInsert (error_w, wpr_position, msgbuf);
    wpr_position = wpr_position + strlen (msgbuf);
    XtVaSetValues (error_w, XmNcursorPosition, wpr_position, NULL);
    XmTextShowPosition (error_w, wpr_position);
}
