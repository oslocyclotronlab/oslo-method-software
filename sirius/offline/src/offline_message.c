/* Function wprintf() is used to display messages in the text output window for SIRIUS */
/* The function works like printf()                                                    */
/* Text output widget: text_w                                                          */
/* Taken from Motif Programming Manual Vol Six A, p 476                                */

#include <Xm/Text.h>
#include <stdio.h>
#include <stdarg.h>

/* global variable */
Widget text_w;

void wprint (const char *fmt, ...)
{
	char msgbuf[1024];
	static XmTextPosition wpr_position;
	va_list ap;
	va_start (ap, fmt);
	(void) vsprintf (msgbuf, fmt, ap);
	va_end (ap);

	XmTextInsert (text_w, wpr_position, msgbuf);
	wpr_position = wpr_position + strlen (msgbuf);
	XtVaSetValues (text_w, XmNcursorPosition, wpr_position, NULL);
	XmTextShowPosition (text_w, wpr_position);
}

#include <Xm/Text.h>
#include <stdio.h>
#include <stdarg.h>

/* global variable */
Widget error_w;

void errprint (const char *fmt, ...)
{
	char msgbuf[1024];
	static XmTextPosition wpr_position;
	va_list ap;
	va_start (ap,fmt);
	(void) vsprintf (msgbuf, fmt, ap);
	va_end (ap);

	XmTextInsert (error_w, wpr_position, msgbuf);
	wpr_position = wpr_position + strlen (msgbuf);
	XtVaSetValues (error_w, XmNcursorPosition, wpr_position, NULL);
	XmTextShowPosition (error_w, wpr_position);
}
