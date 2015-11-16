/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* working.c -- represent a complicated, time-consuming task by
 * counting from 0 to 20000 and provide feedback to the user about
 * how far we are in the process.  The user may terminate the process
 * at any time by selecting the Stop button in the WorkingDialog.
 * This demonstrates how WorkingDialogs can be used to allow the
 * user to interrupt lengthy procedures.
 */
#include <Xm/MessageB.h>
#include <Xm/PushB.h>

#define MAXNUM 20000

void   done();

/* main() --create a pushbutton whose callback pops up a dialog box */
main(argc, argv)
char *argv[];
{
    XtAppContext  app;
    XtWorkProcId  work_id;
    Widget        toplevel, dialog;
    XmString      stop_txt;
    Arg           args[1];
    int           count();

    toplevel = XtVaAppInitialize(&app, "Demos",
        NULL, 0, &argc, argv, NULL, NULL);

    /* Create the dialog -- the "cancel" button says "Stop" */
    stop_txt = XmStringCreateSimple("Stop");
    XtSetArg(args[0], XmNcancelLabelString, stop_txt);
    dialog = XmCreateWorkingDialog(toplevel, "working", args, 1);
    XmStringFree(stop_txt);

    work_id = XtAppAddWorkProc(app, count, dialog);
    XtVaSetValues(dialog, XmNuserData, work_id, NULL);

    XtUnmanageChild(  /* no need for the ok button */
        XmMessageBoxGetChild(dialog, XmDIALOG_OK_BUTTON));
    XtUnmanageChild(  /* no need for the help button */
        XmMessageBoxGetChild(dialog, XmDIALOG_HELP_BUTTON));

    /* Use cancel button to stop counting. True = remove work proc */
    XtAddCallback(dialog, XmNcancelCallback, done, True);

    XtManageChild(dialog);
    XtPopup(XtParent(dialog), XtGrabNone);

    /* XtRealizeWidget(toplevel); */
    XtAppMainLoop(app);
}

/* work procedure -- counts to MAXNUM.  When we hit it, change the
 * "Stop" button to say "Done".
 */
int
count(dialog)
Widget dialog; /* client data for XtAppAddWorkProc() */
{
    static int n;
    char buf[64];
    XmString str, button;
    Arg args[2];

    /* if we printed every number, the flicker is too fast to read.
     * Therefore, just print every 1000 ticks for smoother feedback.
     */
    if (++n % 1000 != 0)
        return False;

    /* display where we are in the counter. */
    sprintf(buf, "Counter: %d", n);
    str = XmStringCreateSimple(buf);
    XtSetArg(args[0], XmNmessageString, str);

    if (n == MAXNUM) {
        button = XmStringCreateSimple("Done");
        XtSetArg(args[1], XmNcancelLabelString, button);
        XtRemoveCallback(dialog, XmNcancelCallback, done, True);
        XtAddCallback(dialog, XmNcancelCallback, done, False);

        XtManageChild(dialog);
        /* or, use:
        if (!XtIsManaged(dialog))
            done(dialog, False);
        */
    }

    XtSetValues(dialog, args, 1 + (n == MAXNUM));

    /* return either True (we're done, remove the work proc)
     * or False (continue working by calling this function).
     */
    return n == MAXNUM;
}

/* User pressed "Stop" or "Done" in WorkingDialog. */
void
done(dialog, remove_work_proc)
Widget dialog;
int remove_work_proc;
{
    if (remove_work_proc) {
        XtWorkProcId work_id;
        XtVaGetValues(dialog, XmNuserData, &work_id, NULL);
        XtRemoveWorkProc(work_id);
    }
    XtDestroyWidget(dialog);
    exit(0); /* for purposes of this demo; remove for general use */
}
