/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* modify_bnt.c -- demonstrate how a default Motif dialog can be
 * modified to support additional items that extend the usability
 * of the dialog itself.  This is a modification of the prompt_dlg.c
 * program.
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
 * Create a dialog that prompts for a new button name or color.
 * A RadioBox is attached to the dialog.  Which button is selected
 * in this box is held as an int (0 or 1) in the XmNuserData resource
 * of the dialog itself.  This value is changed when selecting either
 * of the buttons in the ToggleBox and is queried in the dialog's
 * XmNokCallback function.
 */
void
pushed(pb)
Widget pb;
{
    static Widget dialog;
    XmString t = XmStringCreateSimple("Enter New Button Name:");
    extern void read_name(), toggle_callback();
    Arg args[3];

    /* Create the dialog -- the PushButton acts as the DialogShell's
     * parent (not the parent of the PromptDialog).  The "userData"
     * is used to store the value 
     */
    XtSetArg(args[0], XmNselectionLabelString, t);
    XtSetArg(args[1], XmNautoUnmanage, False);
    XtSetArg(args[2], XmNuserData, 0);
    dialog = XmCreatePromptDialog(pb, "notice_popup", args, 3);
    XmStringFree(t); /* always destroy compound strings when done */

    /* When the user types the name, call read_name() ... */
    XtAddCallback(dialog, XmNokCallback, read_name, pb);

    /* If the user selects cancel, just destroy the dialog */
    XtAddCallback(dialog, XmNcancelCallback, XtDestroyWidget, NULL);

    /* No help is available... */
    XtUnmanageChild(
        XmSelectionBoxGetChild(dialog, XmDIALOG_HELP_BUTTON));

    /* Create a toggle box -- callback routine is toggle_callback() */
    {
        XmString btn1 = XmStringCreateSimple("Change Name");
        XmString btn2 = XmStringCreateSimple("Change Color");
        Widget toggle_box = XmVaCreateSimpleRadioBox(dialog,
            "radio_box", 0 /* inital value */, toggle_callback,
            XmVaRADIOBUTTON, btn1, 0, NULL, NULL,
            XmVaRADIOBUTTON, btn2, 0, NULL, NULL,
            NULL);
        XtManageChild(toggle_box);
    }
    XtManageChild(dialog);
    XtPopup(XtParent(dialog), XtGrabNone);
}

/* callback for the items in the toggle box -- the "client data" is
 * the item number selected.  Since the function gets called whenever
 * either of the buttons changes from true to false or back again,
 * it will always be called in pairs -- ignore the "False" settings.
 * When cbs->set is true, set the dialog's label string accordingly.
 */
void
toggle_callback(toggle_box, n, cbs)
Widget toggle_box;
int n;
XmToggleButtonCallbackStruct *cbs;
{
    Widget dialog = XtParent(XtParent(toggle_box));
    XmString str;

    if (cbs->set == False)
        return; /* wait for the one that toggles "on" */
    if (n == 0)
        str = XmStringCreateSimple("Enter New Button Name:");
    else
        str = XmStringCreateSimple("Enter Text Color:");
    XtVaSetValues(dialog,
        XmNselectionLabelString, str,
        XmNuserData, n, /* reset the user data to reflect new value */
        NULL);
    XmStringFree(str);
}

/* read_name() --the text field has been filled in.  Get the userData
 * from the dialog widget and set the PushButton's name or color.
 */
void
read_name(dialog, push_button, cbs)
Widget dialog;
Widget push_button;  /* the "client_data" from XtAddCallback */
XmSelectionBoxCallbackStruct *cbs;
{
    char *text;
    int n;

    /* userData: n == 0 -> Button Label, n == 1 -> Button Color */
    XtVaGetValues(dialog, XmNuserData, &n, NULL);

    if (n == 0)
        XtVaSetValues(push_button, XmNlabelString, cbs->value, NULL);
    else {
        /* convert compound string into regular text string */
        XmStringGetLtoR(cbs->value, XmSTRING_DEFAULT_CHARSET, &text);
        XtVaSetValues(push_button,
            XtVaTypedArg, XmNforeground,
                XmRString, text, strlen(text) + 1,
            NULL);
        XtFree(text); /* must free text gotten from XmStringGetLtoR() */
    }
}
