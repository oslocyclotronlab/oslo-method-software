/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/*
 * ask_user.c -- create a pushbutton that posts a dialog box
 * that asks the user a question that requires an immediate
 * response.  The function that asks the question actually
 * posts the dialog that displays the question, waits for and
 * returns the result.
 */
#include <X11/Intrinsic.h>
#include <Xm/DialogS.h>
#include <Xm/SelectioB.h>
#include <Xm/RowColumn.h>
#include <Xm/MessageB.h>
#include <Xm/PushBG.h>
#include <Xm/PushB.h>

XtAppContext app;

#define YES 1
#define NO  2

/* main() --create a pushbutton whose callback pops up a dialog box */
main(argc, argv)
char *argv[];
{
    Widget parent, button, toplevel;
    XmString label;
    void pushed();

    toplevel = XtAppInitialize(&app, "Demos",
        NULL, 0, &argc, argv, NULL, NULL);

    label = XmStringCreateSimple("/bin/rm *");
    button = XtVaCreateManagedWidget("button",
        xmPushButtonWidgetClass, toplevel,
        XmNlabelString,          label,
        NULL);
    XtAddCallback(button, XmNactivateCallback,
        pushed, "Remove Everything?");
    XmStringFree(label);

    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}

/* pushed() --the callback routine for the main app's pushbutton. */
void
pushed(w, question)
Widget w;
char *question;
{
    if (AskUser(w, question) == YES)
        puts("Yes");
    else
        puts("No");
}

/*
 * AskUser() -- a generalized routine that asks the user a question
 * and returns the response.
 */
AskUser(parent, question)
char *question;
{
    static Widget dialog;
    XmString text, yes, no;
    static int answer = 0;
    extern void response();

    if (!dialog) {
        dialog = XmCreateQuestionDialog(parent, "dialog", NULL, 0);
        yes = XmStringCreateSimple("Yes");
        no = XmStringCreateSimple("No");
        XtVaSetValues(dialog,
            XmNdialogStyle,        XmDIALOG_SYSTEM_MODAL,
            XmNokLabelString,      yes,
            XmNcancelLabelString,  no,
            NULL);
        XtSetSensitive(
            XmMessageBoxGetChild(dialog, XmDIALOG_HELP_BUTTON), False);
        XtAddCallback(dialog, XmNokCallback, response, &answer);
        XtAddCallback(dialog, XmNcancelCallback, response, &answer);
    }
    text = XmStringCreateSimple(question);
    XtVaSetValues(dialog,
        XmNmessageString,      text,
        NULL);
    XmStringFree(text);
    XtManageChild(dialog);
    XtPopup(XtParent(dialog), XtGrabNone);

    /* while the user hasn't provided an answer, simulate XtMainLoop.
     * The answer changes as soon as the user selects one of the
     * buttons and the callback routine changes its value.  Don't
     * break loop until XtPending() also returns False to assure
     * widget destruction.
     */
    while (answer == 0 || XtAppPending(app))
        XtAppProcessEvent(app, XtIMAll);
    return answer;
}

/* response() --The user made some sort of response to the
 * question posed in AskUser().  Set the answer (client_data)
 * accordingly and destroy the dialog.
 */
void
response(w, answer, reason)
Widget w;
int *answer;
XmAnyCallbackStruct *reason;
{
    switch (reason->reason) {
        case XmCR_OK:
            *answer = YES;
            break;
        case XmCR_CANCEL:
            *answer = NO;
            break;
        default:
            return;
    }
}
