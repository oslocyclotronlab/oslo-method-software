/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* wm_delete.c -- demonstrate how to bind the Close button in the
 * window manager's system menu to the "cancel" button in a dialog.
 */
#include <Xm/MessageB.h>
#include <Xm/PushB.h>
#include <Xm/Protocols.h>

#define YES 1
#define NO  0
int answer;

/* main() --create a pushbutton whose callback pops up a dialog box */
main(argc, argv)
char *argv[];
{
    Widget toplevel, button;
    XtAppContext app;
    void activate();

    toplevel = XtVaAppInitialize(&app, "Demos",
        NULL, 0, &argc, argv, NULL, NULL);

    button = XtCreateManagedWidget("button", xmPushButtonWidgetClass,
        toplevel, NULL, 0);
    XtAddCallback(button, XmNactivateCallback, activate, NULL);

    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}

/* Create and popup an ErrorDialog indicating that the user may have
 * done something wrong.  The dialog contains an Ok and Cancel button,
 * but he can still choose the Close button in the titlebar.
 */
void
activate(w)
Widget w;
{
    Widget dialog, shell;
    void handle_close(), response();
    XmString t = XmStringCreateSimple("Warning: Delete All Files?");
    Atom WM_DELETE_WINDOW;
    Arg args[2];

    /* Make sure the VendorShell associated with the dialog does not
     * react to the user's selection of the Close system menu item.
     */
    XtSetArg(args[0], XmNmessageString, t);
    XtSetArg(args[1], XmNdeleteResponse, XmDO_NOTHING);
    dialog = XmCreateWarningDialog(w, "notice", args, 2);
    XmStringFree(t);

    /* add callback routines for ok and cancel -- desensitize help */
    XtAddCallback(dialog, XmNokCallback, response, NULL);
    XtAddCallback(dialog, XmNcancelCallback, response, NULL);
    XtSetSensitive(XmMessageBoxGetChild(dialog, XmDIALOG_HELP_BUTTON), False);

    XtManageChild(dialog);

    /* Add a callback for the WM_DELETE_WINDOW protocol */
    shell = XtParent(dialog);
    WM_DELETE_WINDOW = XmInternAtom(XtDisplay(w), "WM_DELETE_WINDOW", False);
    XmAddWMProtocolCallback(shell, WM_DELETE_WINDOW, response, dialog);
}

/* callback for the Ok and Cancel buttons in the dialog -- may also be
 * called from the WM_DELETE_WINDOW protocol message sent by the wm.
 */
void
response(widget, client_data, cbs)
Widget widget;
XtPointer client_data;
XmAnyCallbackStruct *cbs;
{
    Widget dialog;

    if (cbs->reason == XmCR_OK) {
        answer = YES;
        puts("Yes.");
    } else {
        answer = NO;
        puts("No.");
    }
    /* test that "reason" is not the cancel button and not the ok button.
     * It's value is XmCR_PROTOCOLS (6666), but we can't check for that
     * because OSF didn't make that value public.
     */
    if (cbs->reason != XmCR_CANCEL && cbs->reason != XmCR_OK)
        /* we passed the dialog as client data for the protocol callback */
        dialog = (Widget)client_data;
    else
        dialog = widget;

    XtDestroyWidget(dialog);
}
