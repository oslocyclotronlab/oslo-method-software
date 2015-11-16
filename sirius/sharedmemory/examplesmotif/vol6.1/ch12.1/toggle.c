/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* toggle.c -- demonstrate a simple toggle button.  */
#include <Xm/ToggleB.h>
#include <Xm/RowColumn.h>

void
toggled(widget, client_data, state)
Widget widget;
XtPointer client_data; /* unused */
XmToggleButtonCallbackStruct *state;
{
    printf("%s: %s\n", XtName(widget), state->set? "on" : "off");
}

main(argc, argv)
int argc;
char *argv[];
{
    Widget toplevel, rowcol, toggle;
    XtAppContext app;

    toplevel = XtVaAppInitialize(&app, "Demos", NULL, 0,
        &argc, argv, NULL, NULL);

    rowcol = XtVaCreateWidget("_rowcol",
        xmRowColumnWidgetClass, toplevel, NULL);

    toggle = XtVaCreateManagedWidget("toggle",
        xmToggleButtonWidgetClass, rowcol, NULL);
    XtAddCallback(toggle, XmNvalueChangedCallback, toggled, NULL);

    XtManageChild(rowcol);

    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}
