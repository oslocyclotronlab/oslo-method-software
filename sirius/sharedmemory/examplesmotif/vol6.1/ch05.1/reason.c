/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* reason.c -- examine the reason field of the callback structure
 * passed as the call_data of the callback function.  This field
 * indicates which action area button in the dialog was pressed.
 */
#include <Xm/RowColumn.h>
#include <Xm/MessageB.h>
#include <Xm/PushB.h>

/* main() --create a pushbutton whose callback pops up a dialog box */
main(argc, argv)
char *argv[];
{
    XtAppContext app;
    Widget toplevel, rc, pb;
    extern void pushed();

    toplevel = XtVaAppInitialize(&app, "Demos", NULL, 0,
        &argc, argv, NULL, NULL);

    rc = XtVaCreateWidget("rowcol", xmRowColumnWidgetClass, toplevel, NULL);

    pb = XtVaCreateManagedWidget("Hello", xmPushButtonWidgetClass, rc, NULL);
    XtAddCallback(pb, XmNactivateCallback, pushed, "Hello World");

    pb = XtVaCreateManagedWidget("Goodbye", xmPushButtonWidgetClass, rc, NULL);
    XtAddCallback(pb, XmNactivateCallback, pushed, "Goodbye World");

    XtManageChild(rc);
    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}

/* pushed() --the callback routine for the main app's pushbuttons.
 * Create and popup a dialog box that has callback functions for
 * the Ok, Cancel and Help buttons.
 */
void
pushed(w, message)
Widget w;
char *message; /* really: client_data, but we know what it is */
{
    static Widget dialog;
    XmString t = XmStringCreateSimple(message);

    /* See if we've already created this dialog -- if so,
     * we don't need to create it again.  Just set the message
     * and manage it (repop it up).
     */
    if (!dialog) {
        extern void callback();
        Arg args[1];

        XtSetArg(args[0], XmNautoUnmanage,  False);
        dialog = XmCreateMessageDialog(w, "notice", args, 1);
        XtAddCallback(dialog, XmNokCallback, callback, "Hi");
        XtAddCallback(dialog, XmNcancelCallback, callback, "Foo");
        XtAddCallback(dialog, XmNhelpCallback, callback, "Bar");
    }
    XtVaSetValues(dialog, XmNmessageString, t, NULL);
    XmStringFree(t);
    XtManageChild(dialog);

    XtPopup(XtParent(dialog), XtGrabNone);
}

/* callback() --One of the dialog buttons was selected.
 * Determine which one by examining the "reason" parameter.
 */
void
callback(w, client_data, cbs)
Widget w;
XtPointer client_data;
XmAnyCallbackStruct *cbs;
{
    char *button;

    switch (cbs->reason) {
        case XmCR_OK : button = "OK"; break;
        case XmCR_CANCEL : button = "Cancel"; break;
        case XmCR_HELP : button = "Help";
    }
    printf("%s was selected: %s\n", button, client_data);
    if (cbs->reason != XmCR_HELP) {
        /* the ok and cancel buttons "close" the widget */
        XtPopdown(XtParent(w));
    }
}
