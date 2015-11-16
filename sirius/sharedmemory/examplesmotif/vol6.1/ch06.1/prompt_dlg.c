/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* prompt_dlg.c -- prompt the user for a string.  Two PushButtons
 * are displayed.  When one is selected, a PromptDialog is displayed
 * allowing the user to type a string.  When done, the PushButton's
 * label changes to the string.
 */
#include <Xm/SelectioB.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>

main(argc, argv)
char *argv[];
{
    XtAppContext app;
    Widget toplevel, rc, button;
    void pushed();

    /* Initialize toolkit and create toplevel shell */
    toplevel = XtVaAppInitialize(&app, "Demos", NULL, 0,
        &argc, argv, NULL, NULL);

    /* RowColumn managed both PushButtons */
    rc = XtVaCreateWidget("rowcol", xmRowColumnWidgetClass, toplevel,
        NULL);
    /* Create two pushbuttons -- both have the same callback */
    button = XtVaCreateManagedWidget("PushMe-1",
        xmPushButtonWidgetClass, rc, NULL);
    XtAddCallback(button, XmNactivateCallback, pushed, NULL);
    button = XtVaCreateManagedWidget("PushMe-2",
        xmPushButtonWidgetClass, rc, NULL);
    XtAddCallback(button, XmNactivateCallback, pushed, NULL);

    XtManageChild(rc);
    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}

/* pushed() --the callback routine for the main app's pushbuttons.
 * Create a dialog that prompts for a new button name.
 */
void
pushed(pb)
Widget pb;
{
    static Widget dialog;
    XmString t = XmStringCreateSimple("Enter New Button Name:");
    extern void read_name();
    Arg args[2];

    /* Create the dialog -- the PushButton acts as the DialogShell's
     * parent (not the parent of the PromptDialog).
     */
    XtSetArg(args[0], XmNselectionLabelString, t);
    XtSetArg(args[1], XmNautoUnmanage, False);
    dialog = XmCreatePromptDialog(pb, "prompt", args, 2);
    XmStringFree(t); /* always destroy compound strings when done */

    /* When the user types the name, call read_name() ... */
    XtAddCallback(dialog, XmNokCallback, read_name, pb);

    /* If the user selects cancel, just destroy the dialog */
    XtAddCallback(dialog, XmNcancelCallback, XtDestroyWidget, NULL);

    /* No help is available... */
    XtSetSensitive(
        XmSelectionBoxGetChild(dialog, XmDIALOG_HELP_BUTTON), False);
    XtManageChild(dialog);

    XtPopup(XtParent(dialog), XtGrabNone);
}

/* read_name() --the text field has been filled in. */
void
read_name(w, push_button, cbs)
Widget w;
Widget push_button;  /* the "client_data" parameter to XtAddCallback */
XmSelectionBoxCallbackStruct *cbs;
{
    XtVaSetValues(push_button, XmNlabelString, cbs->value, NULL);
    /* Name's fine -- go ahead and enter it */
    XtDestroyWidget(w);
}
