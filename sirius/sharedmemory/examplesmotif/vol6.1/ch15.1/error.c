/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

#include <stdio.h>
#include <Xm/Text.h>
#include <Xm/PushBG.h>
#include <Xm/RowColumn.h>
#include <varargs.h>

void wprint();
Widget text_w;

static void
x_error(dpy, err_event)
Display      *dpy;
XErrorEvent  *err_event;
{
    char                buf[BUFSIZ];

    XGetErrorText(dpy, err_event->error_code, buf, sizeof buf);

    wprint("X Error: <%s>\n", buf);
}

static void
xt_error(message)
char *message;
{
    wprint("Xt Error: %s\n", message);
}

static void
make_x_error(w, which)
Widget w;
int which;
{
    switch (which) {
	case 0 : XLookupColor(XtDisplay(text_w), NULL, "", NULL, NULL); break;
	case 2 : XtError("This is an XtError call!"); break;
	case 3 : XtWarning("This is an XtWarning call."); break;
    }
}

main(argc, argv)
int argc;
char *argv[];
{
    XtAppContext app;
    Widget       toplevel, rowcol1, rowcol2, pb;
    Arg          args[7];

    toplevel = XtVaAppInitialize(&app, "Demos",
	NULL, 0, &argc, argv, NULL, NULL);

    rowcol1 = XtVaCreateWidget("rowcol1", xmRowColumnWidgetClass, toplevel,
	NULL);
    rowcol2 = XtVaCreateWidget("rowcol2", xmRowColumnWidgetClass, rowcol1,
	XmNorientation, XmHORIZONTAL,
	NULL);
    pb = XtVaCreateManagedWidget("XLib Error",
	xmPushButtonGadgetClass, rowcol2,
	NULL);
    XtAddCallback(pb, XmNactivateCallback, make_x_error, 0);
    pb = XtVaCreateManagedWidget("Xt Error",
	xmPushButtonGadgetClass, rowcol2,
	NULL);
    XtAddCallback(pb, XmNactivateCallback, make_x_error, 2);
    pb = XtVaCreateManagedWidget("Xt Warning",
	xmPushButtonGadgetClass, rowcol2,
	NULL);
    XtAddCallback(pb, XmNactivateCallback, make_x_error, 3);

    /* Create text_w as a ScrolledText window */
    XtSetArg(args[0], XmNrows,             6);
    XtSetArg(args[1], XmNcolumns,          80);
    XtSetArg(args[2], XmNeditable,         False);
    XtSetArg(args[3], XmNeditMode,         XmMULTI_LINE_EDIT);
    XtSetArg(args[4], XmNwordWrap,         True);
    XtSetArg(args[5], XmNscrollHorizontal, False);
    XtSetArg(args[6], XmNcursorPositionVisible, False);
    text_w = XmCreateScrolledText(rowcol1, "text_w", args, 7);
    XtManageChild(text_w);

    /* catch Xt errors */
    XtAppSetErrorHandler(app, xt_error);
    XtAppSetWarningHandler(app, xt_error);
    /* and Xlib errors */
    (void) XSetErrorHandler(x_error);

    XtManageChild(rowcol1);
    XtManageChild(rowcol2);
    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}

/*VARARGS*/
void
wprint(va_alist)
va_dcl
{
    char msgbuf[BUFSIZ]; /* we're not getting huge strings */
    char *fmt;
    static XmTextPosition wpr_position; /* maintain text position */
    va_list args;

    va_start(args);
    fmt = va_arg(args, char *);
#ifndef NO_VPRINTF
    (void) vsprintf(msgbuf, fmt, args);
#else /* !NO_VPRINTF */
    {
        FILE foo;
        foo._cnt = BUFSIZ;
        foo._base = foo._ptr = msgbuf; /* (unsigned char *) ?? */
        foo._flag = _IOWRT+_IOSTRG;
        (void) _doprnt(fmt, args, &foo);
        *foo._ptr = '\0'; /* plant terminating null character */
    }
#endif /* NO_VPRINTF */
    va_end(args);

    XmTextInsert(text_w, wpr_position, msgbuf);
    wpr_position += strlen(msgbuf);
    XtVaSetValues(text_w, XmNcursorPosition, wpr_position, NULL);
    XmTextShowPosition(text_w, wpr_position);
}
