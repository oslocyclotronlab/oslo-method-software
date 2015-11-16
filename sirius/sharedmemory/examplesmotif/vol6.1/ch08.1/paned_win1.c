/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* paned_wind1.c --there are two Label widgets that are positioned
 * above and below a Text widget.  The Labels' minimum and maximum
 * sizes are set to 25 and 45 respectively, preventing those
 * panes from growing beyond those bounds.  The Text widget has its
 * minimum size set to 35 preventing it from becoming so small that
 * its text cannot be read.
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
        NULL);

    XtVaCreateManagedWidget("Hello", xmLabelWidgetClass, pane,
        XmNpaneMinimum,    25,
        XmNpaneMaximum,    45,
        NULL);

    XtVaCreateManagedWidget("text", xmTextWidgetClass, pane,
        XmNrows,           5,
        XmNcolumns,        80,
        XmNpaneMinimum,    35,
        XmNeditMode,       XmMULTI_LINE_EDIT,
        XmNvalue,   "This is a test of the paned window widget.",
        NULL);

    XtVaCreateManagedWidget("Goodbye", xmLabelWidgetClass, pane,
        XmNpaneMinimum,    25,
        XmNpaneMaximum,    45,
        NULL);

    XtManageChild(pane);

    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}
