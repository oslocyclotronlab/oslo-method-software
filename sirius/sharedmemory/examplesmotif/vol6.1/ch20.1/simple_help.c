/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* simple_help.c -- create a pushbutton that posts a dialog box
 * that entices the user to press the help button.  The callback
 * for this button displays a new dialog that gives help.
 */
#include <Xm/MessageB.h>
#include <Xm/PushB.h>

/* main() --create a pushbutton whose callback pops up a dialog box */
main(argc, argv)
char *argv[];
{
    Widget toplevel, button;
    XtAppContext app;
    XmString label;
    void pushed();

    toplevel = XtVaAppInitialize(&app, "Demos", NULL, 0,
        &argc, argv, NULL, NULL);

    label = XmStringCreateSimple("???");
    button = XtVaCreateManagedWidget("button",
        xmPushButtonWidgetClass, toplevel,
        XmNlabelString,          label,
        NULL);
    XtAddCallback(button, XmNactivateCallback,
        pushed, "You probably need help for this item.");
    XmStringFree(label);

    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}

#define HELP_TEXT "You just got help.\n    Now press 'Ok'"

/* pushed() --the callback routine for the main app's pushbutton. */
void
pushed(w, text)
Widget w;
char *text;
{
    Widget dialog;
    XmString t = XmStringCreateSimple(text);
    Arg args[2];
    extern void help_callback();

    XtSetArg(args[0], XmNautoUnmanage, False);
    XtSetArg(args[1], XmNmessageString, t);
    dialog = XmCreateMessageDialog(XtParent(w), "notice", args, 2);
    XmStringFree(t);

    XtUnmanageChild(
	XmMessageBoxGetChild(dialog, XmDIALOG_CANCEL_BUTTON));

    XtAddCallback(dialog, XmNokCallback, XtDestroyWidget, NULL);
    XtAddCallback(dialog, XmNhelpCallback, help_callback, HELP_TEXT);

    XtManageChild(dialog);
    XtPopup(XtParent(dialog), XtGrabNone);
}

/*
 * The callback routine for the Help button in the original dialog
 * box. This routine displays a HelpDialog based on the help_text
 * parameter.
 */
void
help_callback(parent, help_text, cbs)
Widget parent;
char *help_text;
XmAnyCallbackStruct *cbs;
{
    Widget dialog;
    XmString text;
    void help_done();
    Arg args[2];

    text = XmStringCreateLtoR(help_text, XmSTRING_DEFAULT_CHARSET);
    XtSetArg(args[0], XmNmessageString, text);
    XtSetArg(args[1], XmNautoUnmanage, False);
    dialog = XmCreateInformationDialog(parent, "help", args, 2);
    XmStringFree(text);

    XtUnmanageChild(  /* no need for the cancel button */
        XmMessageBoxGetChild(dialog, XmDIALOG_CANCEL_BUTTON));
    XtSetSensitive(   /* no more help is available. */
        XmMessageBoxGetChild(dialog, XmDIALOG_HELP_BUTTON), False);
    /* the Ok button will call help_done() below */
    XtAddCallback(dialog, XmNokCallback, help_done, NULL);

    /* display the help text */
    XtManageChild(dialog);
    XtPopup(XtParent(dialog), XtGrabNone);
}

/* help_done() --called when user presses "Ok" in HelpDialog.
 * Destroy the dialog shell and reset it so that help_callback()
 * will create a new one.
 */
void
help_done(dialog)
Widget dialog;
{
    XtDestroyWidget(dialog);
}
