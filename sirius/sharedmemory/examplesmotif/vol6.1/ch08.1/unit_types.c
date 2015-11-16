/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* unit_types.c --the same as paned_win1.c except that the
 * Labels' minimum and maximum sizes are set to 1/4 inch and
 * 1/2 inch respectively.  These measurements are retained
 * regardless of the pixels-per-inch resolution of the user's
 * display.
 */
#include <Xm/Label.h>
#include <Xm/PanedW.h>
#include <Xm/Text.h>

main(argc, argv)
char *argv[];
{
    Widget        toplevel, pane;
    XtAppContext  app;

    toplevel = XtVaAppInitialize(&app, "Demos", NULL, 0,
        &argc, argv, NULL, NULL);

    pane = XtVaCreateWidget("pane",
        xmPanedWindowWidgetClass, toplevel,
        XmNunitType, Xm1000TH_INCHES,
        NULL);

    XtVaCreateManagedWidget("Hello", xmLabelWidgetClass, pane,
        XmNpaneMinimum,    250, /* quarter inch */
        XmNpaneMaximum,    500, /* half inch */
        NULL);

    XtVaCreateManagedWidget("text", xmTextWidgetClass, pane,
        XmNrows,           5,
        XmNcolumns,        80,
        XmNpaneMinimum,    250,
        XmNeditMode,       XmMULTI_LINE_EDIT,
        XmNvalue,   "This is a test of the paned window widget.",
        NULL);

    XtVaCreateManagedWidget("Goodbye", xmLabelWidgetClass, pane,
        XmNpaneMinimum,    250, /* quarter inch */
        XmNpaneMaximum,    500, /* half inch */
        NULL);

    XtManageChild(pane);

    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}
