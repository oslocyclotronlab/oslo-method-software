/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* paned_wind2.c --there are two label widgets that are positioned
 * above and below a Text widget.  The labels' desired heights are
 * queried using XtQueryGeometry() and their corresponding maximum
 * and minimum sizes are set to the same value.  This effectively
 * prevents those panes from being resized.  The Text widget has its
 * minimum size set to 35 preventing it from becoming so small that
 * its text cannot be read.
 */
#include <Xm/Xm.h>
#include <Xm/Label.h>
#include <Xm/PanedW.h>
#include <Xm/Text.h>

main(argc, argv)
char *argv[];
{
    Widget        toplevel, pane, label;
    XtWidgetGeometry  size;
    XtAppContext  app;

    toplevel = XtVaAppInitialize(&app, "Demos", NULL, 0,
        &argc, argv, NULL, NULL);

    pane = XtVaCreateWidget("pane",
        xmPanedWindowWidgetClass, toplevel, NULL);

    label = XtVaCreateManagedWidget("Hello",
        xmLabelWidgetClass, pane, NULL);
    size.request_mode = CWHeight;
    XtQueryGeometry(label, NULL, &size);
    XtVaSetValues(label,
        XmNpaneMaximum, size.height,
        XmNpaneMinimum, size.height,
        NULL);
    printf("hello's height: %d\n", size.height);

    XtVaCreateManagedWidget("text", xmTextWidgetClass, pane,
        XmNrows,           5,
        XmNcolumns,        80,
        XmNresizeWidth,    False,
        XmNresizeHeight,   False,
        XmNpaneMinimum,    35,
        XmNeditMode,       XmMULTI_LINE_EDIT,
        XmNvalue,          "This is a test of the paned window widget.",
        NULL);

    label = XtVaCreateManagedWidget("Goodbye",
        xmLabelWidgetClass, pane, NULL);
    size.request_mode = CWHeight;
    XtQueryGeometry(label, NULL, &size);
    XtVaSetValues(label,
        XmNpaneMaximum, size.height,
        XmNpaneMinimum, size.height,
        NULL);
    printf("goodbye's height: %d\n", size.height);

    XtManageChild(pane);

    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}
