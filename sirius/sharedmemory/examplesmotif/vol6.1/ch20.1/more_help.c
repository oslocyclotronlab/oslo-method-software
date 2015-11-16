/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* more_help.c -- create a pushbutton that posts a dialog box
 * that entices the user to press the help button.  The callback
 * for this button displays a new dialog that gives help.
 */
#include <Xm/MessageB.h>
#include <Xm/PushB.h>
#include <stdio.h>

/* main() --create a pushbutton whose callback pops up a dialog box */
main(argc, argv)
char *argv[];
{
    Widget toplevel, button;
    XtAppContext app;
    XmString label;
    void help_callback();

    toplevel = XtVaAppInitialize(&app, "Demos", NULL, 0,
        &argc, argv, NULL, NULL);

    label = XmStringCreateSimple("???");
    button = XtVaCreateManagedWidget("button",
        xmPushButtonWidgetClass, toplevel,
        XmNlabelString,          label,
        NULL);
    XtAddCallback(button, XmNactivateCallback, help_callback, 0);
    XmStringFree(label);

    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}

#define MAX_HELP_STAGES 3
char *help_texts[3][5] = {
    {
        "You have reached the first stage of the Help System.",
        "If you need additional help, select the More Help button.",
        "You may exit Help at any time by pressing 'Done'.",
        NULL,
    },
    {
        "This is the second stage of the Help system.  There is",
        "more help available.  Press 'More Help' to see more.",
        "Press 'Previous' to return to the previous Help message.",
        "Or press 'Done' to exit the Help System.",
        NULL,
    },
    {
        "This is the last help message you will see on this topic.",
        "You may either press 'Previous' to return to the previous",
        "Help level, or press 'Done' to exit the Help System.",
        NULL,
    }
};

/*
 * The callback routine for the Help button in the original dialog
 * box. This routine displays a HelpDialog based on the help_text
 * parameter.  This routine also serves as the callback for the
 * Help button in *this* dialog.  Thus, it is conceptually recursive
 * (altho, not literally).  The client_data, idx_incr, indicates an
 * increment or decrement into the help_texts array decared above.
 * When idx_incr is 0, the MessageDialog called us -- check to see
 * if the dialog has already been created (in which case, don't re-
 * create it.  Otherwise, the value will be -1 or 1.  Increment the
 * "idx" (static int) so the help text changes.
 */
void
help_callback(parent, idx_incr, cbs)
Widget parent;
int idx_incr;
XmAnyCallbackStruct *cbs;
{
    static Widget dialog; /* prevent multiple help dialogs */
    XmString text;
    char buf[BUFSIZ], *p;
    static int idx;
    int i;
    void help_done();

    if (dialog && idx_incr == 0) {
        /* user pressed Help button in MesageDialog again.  We're
         * already up, so just make sure we're visible and return.
         */
        XtPopup(XtParent(dialog), XtGrabNone);
        XMapRaised(XtDisplay(dialog), XtWindow(XtParent(dialog)));
        return;
    }

    if (dialog)
        idx += idx_incr; /* more/previous help; change index */
    else {
        /* We're not up, so create new Help Dialog */
        Arg args[4];

        /* Action area button labels. */
        XmString done = XmStringCreateSimple("Done");
        XmString cancel = XmStringCreateSimple("Previous");
        XmString more = XmStringCreateSimple("More Help");

        XtSetArg(args[0], XmNautoUnmanage, False);
        XtSetArg(args[1], XmNokLabelString, done);
        XtSetArg(args[2], XmNcancelLabelString, cancel);
        XtSetArg(args[3], XmNhelpLabelString, more);
        dialog = XmCreateInformationDialog(parent, "help", args, 4);

        /* pass help_done() the address of "dialog" so it can reset */
        XtAddCallback(dialog, XmNokCallback, help_done, &dialog);
        /* if more/previous help, recall ourselves with increment */
        XtAddCallback(dialog, XmNcancelCallback, help_callback, -1);
        XtAddCallback(dialog, XmNhelpCallback, help_callback, 1);

        /* If our parent dies, we must reset "dialog" to NULL! */
        XtAddCallback(dialog, XmNdestroyCallback, help_done, &dialog);

        XmStringFree(done);   /* once dialog is created, these */
        XmStringFree(cancel); /* strings are no longer needed. */
        XmStringFree(more);

        idx = 0; /* initialize idx--needed for each new help stuff */
    }

    /* concatenate help texts into a single string with newlines */
    for (p = buf, i = 0; help_texts[idx][i]; i++) {
        p += strlen(strcpy(p, help_texts[idx][i]));
        *p++ = '\n', *p = 0;
    }

    text = XmStringCreateLtoR(buf, XmSTRING_DEFAULT_CHARSET);
    XtVaSetValues(dialog, XmNmessageString, text, NULL);
    XmStringFree(text); /* after set-values, free unneeded memory */

    /* If no previous help msg, set "Previous" to insensitive. */
    XtSetSensitive(
        XmMessageBoxGetChild(dialog,XmDIALOG_CANCEL_BUTTON), idx > 0);
    /* If no more help, set "More Help" insensitive. */
    XtSetSensitive(
        XmMessageBoxGetChild(dialog, XmDIALOG_HELP_BUTTON),
            idx < MAX_HELP_STAGES-1);

    /* display the dialog */
    XtManageChild(dialog);
    XtPopup(XtParent(dialog), XtGrabNone);
}

/* This callback is used to kill ourselves and set the dialog pointer
 * to NULL so it can't be referenced again by help_callback().
 * This function is called from the Done button in the help dialog.
 * It is also our XmNdestroyCallback, so reset our dialog_ptr to NULL.
 */
void
help_done(dialog, dialog_ptr)
Widget dialog, *dialog_ptr;
{
    if (!*dialog_ptr) /* prevent unnecessarily destroying twice */
        return;
    XtDestroyWidget(dialog); /* this might call ourselves.. */
    *dialog_ptr = NULL;
}
