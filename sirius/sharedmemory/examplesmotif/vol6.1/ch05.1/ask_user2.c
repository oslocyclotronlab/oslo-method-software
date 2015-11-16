/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* ask_user2.c -- advanced version of ask_user.c.
 * The user is presented with two pushbuttons.  The first creates
 * a file (/tmp/foo) and the second removes it.  In each case,
 * a dialog pops up asking for verification of the action.
 *
 * This program is intended to demonstrate an advanced implementation
 * of the AskUser() function.  This time, the function is passed the
 * strings to use for the Ok button and the Cancel button as well as
 * the button to use as the default value.
 */
#include <Xm/DialogS.h>
#include <Xm/SelectioB.h>
#include <Xm/RowColumn.h>
#include <Xm/MessageB.h>
#include <Xm/PushB.h>

#define YES 1
#define NO  2

/* Generalize the question/answer process by creating a data structure
 * that has the necessary labels, questions and everything needed to
 * execute a command.
 */
typedef struct {
    char *label;    /* label for pushbutton used to invoke cmd */
    char *question; /* question for dialog box to confirm cmd */
    char *yes;      /* what the "ok" button says */
    char *no;       /* what the "cancel" button says */
    int   dflt;     /* which should be the default answer */
    char *cmd;      /* actual command to execute (using system()) */
} QandA;

QandA touch_foo = {
    "Create", "Create /tmp/foo?", "Yes", "No", YES, "touch /tmp/foo"
};
QandA rm_foo = {
    "Remove", "Remove /tmp/foo?", "Do", "Don't", NO, "rm /tmp/foo"
};

XtAppContext app;

main(argc, argv)
int argc;
char *argv[];
{
    Widget toplevel, button, rowcolumn;
    XmString label;
    void pushed();

    toplevel = XtVaAppInitialize(&app, "Demos",
        NULL, 0, &argc, argv, NULL, NULL);

    rowcolumn = XtVaCreateManagedWidget("rowcolumn",
        xmRowColumnWidgetClass, toplevel, NULL);

    label = XmStringCreateSimple(touch_foo.label);
    button = XtVaCreateManagedWidget("button",
        xmPushButtonWidgetClass, rowcolumn,
        XmNlabelString,          label,
        NULL);
    XtAddCallback(button, XmNactivateCallback, pushed, &touch_foo);
    XmStringFree(label);

    label = XmStringCreateSimple(rm_foo.label);
    button = XtVaCreateManagedWidget("button",
        xmPushButtonWidgetClass, rowcolumn,
        XmNlabelString,          label,
        NULL);
    XtAddCallback(button, XmNactivateCallback, pushed, &rm_foo);
    XmStringFree(label);

    XtManageChild(rowcolumn);
    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}

/* pushed() --when a button is pressed, ask the question described
 * by the QandA parameter (client_data).  Execute the cmd if YES.
 */
void
pushed(w, quest)
Widget w;
QandA *quest;
{
    if (AskUser(w, quest->question, quest->yes, quest->no,
                quest->dflt) == YES) {
        printf("executing: %s\n", quest->cmd);
        system(quest->cmd);
    } else
        printf("not executing: %s\n", quest->cmd);
}

/*
 * AskUser() -- a generalized routine that asks the user a question
 * and returns a response.  Parameters are: the question, the labels
 * for the "Yes" and "No" buttons, and the default selection to use.
 */
AskUser(parent, question, ans1, ans2, default_ans)
char *question, *ans1, *ans2;
int default_ans;
{
    static Widget dialog; /* static to avoid multiple creation */
    XmString text, yes, no;
    static int answer = 0;
    extern void response();

    if (!dialog) {
        dialog = XmCreateQuestionDialog(parent, "dialog", NULL, 0);
        XtVaSetValues(dialog,
            XmNdialogStyle,        XmDIALOG_FULL_APPLICATION_MODAL,
            NULL);
        XtSetSensitive(
            XmMessageBoxGetChild(dialog, XmDIALOG_HELP_BUTTON),
            False);
        XtAddCallback(dialog, XmNokCallback, response, &answer);
        XtAddCallback(dialog, XmNcancelCallback, response, &answer);
    }
    text = XmStringCreateSimple(question);
    yes = XmStringCreateSimple(ans1);
    no = XmStringCreateSimple(ans2);
    XtVaSetValues(dialog,
        XmNmessageString,      text,
        XmNokLabelString,      yes,
        XmNcancelLabelString,  no,
        XmNdefaultButtonType,  default_ans == YES?
                        XmDIALOG_OK_BUTTON : XmDIALOG_CANCEL_BUTTON,
        NULL);
    XmStringFree(text);
    XmStringFree(yes);
    XmStringFree(no);
    XtManageChild(dialog);

    while (answer == 0) {
        XtAppProcessEvent(app, XtIMAll);
        XSync(XtDisplay(dialog), 0);
    }

    XtUnmanageChild(dialog);
    XSync(XtDisplay(dialog), 0);
    XmUpdateDisplay(dialog);

    return answer;
}

/* response() --The user made some sort of response to the
 * question posed in AskUser().  Set the answer (client_data)
 * accordingly.
 */
void
response(w, answer, cbs)
Widget w;
int *answer;
XmAnyCallbackStruct *cbs;
{
    if (cbs->reason == XmCR_OK)
        *answer = YES;
    else if (cbs->reason == XmCR_CANCEL)
        *answer = NO;
}
