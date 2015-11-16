/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* proc_traverse.c -- demonstrate how to process keyboard traversal
 * from a PushButton's callback routine.  This simple demo contains
 * a RowColumn (a tab group) and three PushButtons.  If any of the
 * PushButtons are activated (selected), the input focus traverses
 * to the "home" item.
 */ 
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>

main(argc, argv)
int argc;
char *argv[];
{
    Widget toplevel, rowcol, pb;
    XtAppContext app;
    void do_it();

    toplevel = XtVaAppInitialize(&app, "Demos", NULL, 0, 
        &argc, argv, NULL, NULL);

    rowcol = XtVaCreateManagedWidget("rowcolumn",
        xmRowColumnWidgetClass, toplevel,
        XmNorientation, XmHORIZONTAL,
        NULL);

    (void) XtVaCreateManagedWidget("Ok",
        xmPushButtonWidgetClass, rowcol, NULL);

    pb = XtVaCreateManagedWidget("Cancel",
        xmPushButtonWidgetClass, rowcol, NULL);
    XtAddCallback(pb, XmNactivateCallback, do_it, NULL);

    pb = XtVaCreateManagedWidget("Help",
        xmPushButtonWidgetClass, rowcol, NULL);
    XtAddCallback(pb, XmNactivateCallback, do_it, NULL);

    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}

/* callback for pushbuttons */
void
do_it(w, unused, cbs)
Widget w;
XtPointer unused;
XmAnyCallbackStruct *cbs;
{
    /* do stuff here for PushButton widget */
    XtCallActionProc(w, "Arm", cbs->event, NULL, 0);
}
