/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* hello.c -- your typical Hello World program using
 * an InformationDialog.
 */
#include <Xm/RowColumn.h>
#include <Xm/MessageB.h>
#include <Xm/PushB.h>

main(argc, argv)
int argc;
char *argv[];
{
    XtAppContext app;
    Widget toplevel, rc, pb;
    extern void popup(); /* callback for the pushbuttons.  pops up dialog */

    toplevel = XtVaAppInitialize(&app, "Demos", NULL, 0,
        &argc, argv, NULL, NULL);

    rc = XtVaCreateWidget("rowcol",
        xmRowColumnWidgetClass, toplevel, NULL);
    pb = XtVaCreateManagedWidget("Hello", xmPushButtonWidgetClass, rc, NULL);
    XtAddCallback(pb, XmNactivateCallback, popup, "Hello World");

    XtManageChild(rc);
    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}

/* callback for the PushButtons.  Popup an InformationDialog displaying
 * the text passed as the client data parameter.
 */
void
popup(push_button, text, cbs)
Widget push_button;
char *text;
XmPushButtonCallbackStruct *cbs; /* unused */
{
    Widget dialog;
    XmString xm_string;
    extern void activate();
    Arg args[1];

    /* set the label for the dialog */
    xm_string = XmStringCreateSimple(text);
    XtSetArg(args[0], XmNmessageString, xm_string);

    /* Create the InformationDialog as child of push_button */
    dialog = XmCreateInformationDialog(push_button, "info", args, 1);

    /* no longer need the compound string, free it */
    XmStringFree(xm_string);

    /* add the callback routine */
    XtAddCallback(dialog, XmNokCallback, activate, NULL);

    /* manage the dialog */
    XtManageChild(dialog);
    XtPopup(XtParent(dialog), XtGrabNone);
}

/* callback routine for when the user presses the Ok button.
 * Yes, despite the fact that the Ok button was pressed, the
 * widget passed to this callback routine is the dialog!
 */
void
activate(dialog)
Widget dialog;
{
    puts("Ok was pressed.");
}
