/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* modal.c -- demonstrate modal dialogs.  Display two pushbuttons
 * each activating a modal dialog.
 */
#include <Xm/RowColumn.h>
#include <Xm/MessageB.h>
#include <Xm/PushB.h>

/* main() --create a pushbutton whose callback pops up a dialog box */
main(argc, argv)
char *argv[];
{
    XtAppContext app;
    Widget toplevel, button, rowcolumn;
    void pushed();

    toplevel = XtVaAppInitialize(&app, "Demos",
        NULL, 0, &argc, argv, NULL, NULL);

    rowcolumn = XtCreateManagedWidget("rowcolumn",
        xmRowColumnWidgetClass, toplevel, NULL, 0);

    button = XtCreateManagedWidget("application-modal",
        xmPushButtonWidgetClass, rowcolumn, NULL, 0);
    XtAddCallback(button, XmNactivateCallback,
        pushed, XmDIALOG_FULL_APPLICATION_MODAL);
    button = XtCreateManagedWidget("system-modal",
        xmPushButtonWidgetClass, rowcolumn, NULL, 0);
    XtAddCallback(button, XmNactivateCallback, pushed,
        XmDIALOG_SYSTEM_MODAL);

    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}

/* pushed() --the callback routine for the main app's pushbutton.
 * Create either a full-application or system modal dialog box.
 */
void
pushed(w, modality)
Widget w;
unsigned char modality;
{
    static Widget dialog;
    XmString t;
    extern void dlg_callback();

    /* See if we've already created this dialog -- if so,
     * we don't need to create it again.  Just re-pop it up.
     */
    if (!dialog) {
        Arg args[2];
        XmString ok = XmStringCreateSimple("OK");
        XtSetArg(args[0], XmNautoUnmanage, False);
        XtSetArg(args[1], XmNcancelLabelString, ok);
        dialog = XmCreateInformationDialog(w, "notice", args, 2);
        XtAddCallback(dialog, XmNcancelCallback, dlg_callback, NULL);
        XtUnmanageChild(
            XmMessageBoxGetChild(dialog, XmDIALOG_OK_BUTTON));
        XtUnmanageChild(
            XmMessageBoxGetChild(dialog, XmDIALOG_HELP_BUTTON));
    }
    t = XmStringCreateSimple("You must reply to this message now!");
    XtVaSetValues(dialog,
        XmNmessageString,    t,
        XmNdialogStyle,      modality,
        NULL);
    XmStringFree(t);
    XtManageChild(dialog);
    XtPopup(XtParent(dialog), XtGrabNone);
}

void
dlg_callback(dialog, client_data, cbs)
Widget dialog;
XtPointer client_data;
XmAnyCallbackStruct *cbs;
{
    XtPopdown(XtParent(dialog));
}
