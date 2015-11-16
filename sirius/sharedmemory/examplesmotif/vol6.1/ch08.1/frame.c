/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* frame.c -- demonstrate the Frame widget.
 * Create 4 Labels or PushButtons (depending on an optional -p
 * command line argument) each with a Frame widget as its parent.
 */
#include <Xm/XmP.h>
#include <Xm/PushBG.h>
#include <Xm/LabelG.h>
#include <Xm/RowColumn.h>
#include <Xm/Frame.h>

main(argc, argv)
int argc;
char *argv[];
{
    Widget toplevel, rowcol, frame;
    WidgetClass class = xmLabelGadgetClass;
    XtAppContext app;

    /* Initialize toolkit and create TopLevel shell widget */
    toplevel = XtVaAppInitialize(&app, "Demos",
        NULL, 0, &argc, argv, NULL, NULL);

    /* "-p" specifies whether to use PushButton widgets or Labels */
    if (argv[1] && !strcmp(argv[1], "-p"))
        class = xmPushButtonGadgetClass;

    /* Make a RowColumn to contain all the Frames */
    rowcol = XtVaCreateWidget("rowcolumn",
        xmRowColumnWidgetClass, toplevel,
        XmNspacing, 5,
        NULL);

    /* Create different Frames each containing a unique shadow type */
    XtVaCreateManagedWidget("Frame Types:",
        xmLabelGadgetClass, rowcol, NULL);
    frame = XtVaCreateManagedWidget("frame1",
        xmFrameWidgetClass, rowcol,
        XmNshadowType,      XmSHADOW_IN,
        NULL);
    XtVaCreateManagedWidget("XmSHADOW_IN", class, frame, NULL);

    frame = XtVaCreateManagedWidget("frame2",
        xmFrameWidgetClass, rowcol,
        XmNshadowType,      XmSHADOW_OUT,
        NULL);
    XtVaCreateManagedWidget("XmSHADOW_OUT", class, frame, NULL);

    frame = XtVaCreateManagedWidget("frame3",
        xmFrameWidgetClass, rowcol,
        XmNshadowType,      XmSHADOW_ETCHED_IN,
        NULL);
    XtVaCreateManagedWidget("XmSHADOW_ETCHED_IN", class, frame, NULL);

    frame = XtVaCreateManagedWidget("frame4",
        xmFrameWidgetClass, rowcol,
        XmNshadowType,      XmSHADOW_ETCHED_OUT,
        NULL);
    XtVaCreateManagedWidget("XmSHADOW_ETCHED_OUT", class, frame, NULL);

    XtManageChild(rowcol);
    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}
