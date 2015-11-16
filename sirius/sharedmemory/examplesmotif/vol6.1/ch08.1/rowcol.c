/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* rowcol.c -- demonstrate a simple RowColumn widget.  Create one
 * with 3 pushbutton gadgets.  Once created, resize the thing in
 * all sorts of contortions to get a feel for what RowColumns can
 * do with its children.
 */
#include <Xm/PushBG.h>
#include <Xm/RowColumn.h>

main(argc, argv)
int argc;
char *argv[];
{
    Widget toplevel, rowcol;
    XtAppContext app;

    toplevel = XtVaAppInitialize(&app, "Demos", NULL, 0, 
        &argc, argv, NULL, NULL);

    rowcol = XtVaCreateManagedWidget("rowcolumn",
        xmRowColumnWidgetClass, toplevel, NULL);

    (void) XtVaCreateManagedWidget("One",
        xmPushButtonGadgetClass, rowcol, NULL);

    (void) XtVaCreateManagedWidget("Two",
        xmPushButtonGadgetClass, rowcol, NULL);

    (void) XtVaCreateManagedWidget("Three",
        xmPushButtonGadgetClass, rowcol, NULL);

    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}
