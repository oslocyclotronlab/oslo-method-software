/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* entry_cb.c -- demonstrate how the XmNentryCallback resource works
 * in RowColumn widgets.  When a callback function is set for this
 * resource, all the callbacks for the RowColumn's children are reset
 * to point to this function.  Their original functions are no longer
 * called had they been set in favor of the entry-callback function.
 */
#include <Xm/XmP.h>
#include <Xm/PushBG.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>

char *strings[] = {
    "One", "Two", "Three", "Four", "Five",
    "Six", "Seven", "Eight", "Nine", "Ten",
};

void
called(widget, client_data, child_data)
Widget widget;
XtPointer client_data; /* data specific to the RowColumn */
XmRowColumnCallbackStruct *child_data;
{
    Widget pb = child_data->widget;

    printf("%s: %d\n", XtName(pb), child_data->data);
}

static void
never_called()
{
    puts("This function is never called");
}

main(argc, argv)
int argc;
char *argv[];
{
    Widget toplevel, parent, w;
    XtAppContext app;
    int i;

    toplevel = XtVaAppInitialize(&app, "Demos",
        NULL, 0, &argc, argv, NULL, NULL);

    parent = XtVaCreateManagedWidget("rowcolumn",
        xmRowColumnWidgetClass, toplevel,
        NULL);
    XtAddCallback(parent, XmNentryCallback, called, NULL);

    /* simply loop thru the strings creating a widget for each one */
    for (i = 0; i < XtNumber(strings); i++) {
        w = XtVaCreateManagedWidget(strings[i],
            xmPushButtonGadgetClass, parent, NULL);
        /* Call XtAddCallback() to install client_data only! */
        XtAddCallback(w, XmNactivateCallback, never_called, i+1);
    }

    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}
