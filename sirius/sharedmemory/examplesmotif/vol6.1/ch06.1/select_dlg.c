/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* select_dlg.c -- display two pushbuttons: days and months.
 * When the user selections one of them, post a selection
 * dialog that displays the actual days or months accordingly.
 * When the user selects or types a selection, post a dialog
 * the identifies which item was selected and whether or not
 * the item is in the list.
 *
 * This program demonstrates how to use selection boxes,
 * methods for creating generic callbacks for action area
 * selections, abstraction of data structures, and a generic
 * MessageDialogBox posting routine.
 */
#include <Xm/SelectioB.h>
#include <Xm/RowColumn.h>
#include <Xm/MessageB.h>
#include <Xm/PushB.h>

Widget PostDialog();

char *days[] = {
    "Sunday", "Monday", "Tuesday", "Wednesday",
    "Thursday", "Friday", "Saturday"
};
char *months[] = {
    "January", "February", "March", "April", "May", "June",
    "July", "August", "September", "October", "November", "December"
};
typedef struct {
    char *label;
    char **strings;
    int size;
} ListItem;

ListItem month_items = { "Months", months, XtNumber(months) };
ListItem days_items = { "Days", days, XtNumber(days) };

/* main() --create two pushbuttons whose callbacks pop up a dialog */
main(argc, argv)
char *argv[];
{
    Widget toplevel, button, rc;
    XtAppContext app;
    void pushed();

    toplevel = XtVaAppInitialize(&app, "Demos", NULL, 0,
        &argc, argv, NULL, NULL);

    rc = XtVaCreateWidget("rowcolumn", xmRowColumnWidgetClass, toplevel, NULL);

    button = XtVaCreateManagedWidget(month_items.label,
        xmPushButtonWidgetClass, rc, NULL);
    XtAddCallback(button, XmNactivateCallback, pushed, &month_items);

    button = XtVaCreateManagedWidget(days_items.label,
        xmPushButtonWidgetClass, rc, NULL);
    XtAddCallback(button, XmNactivateCallback, pushed, &days_items);

    XtManageChild(rc);
    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}

/* pushed() --the callback routine for the main app's pushbutton.
 * Create a dialog containing the list in the items parameter.
 */
void
pushed(w, items)
Widget w;
ListItem *items;
{
    Widget dialog;
    XmString t, *str;
    int i;
    extern void dialog_callback();

    str = (XmString *)XtMalloc(items->size * sizeof(XmString));
    t = XmStringCreateSimple(items->label);
    for (i = 0; i < items->size; i++)
        str[i] = XmStringCreateSimple(items->strings[i]);
    dialog = XmCreateSelectionDialog(w, "selection", NULL, 0);
    XtVaSetValues(dialog,
        XmNlistLabelString, t,
        XmNlistItems,       str,
        XmNlistItemCount,   items->size,
        XmNmustMatch,       True,
        NULL);
    XtSetSensitive(
        XmSelectionBoxGetChild(dialog, XmDIALOG_HELP_BUTTON), False);
    XtAddCallback(dialog, XmNokCallback, dialog_callback, NULL);
    XtAddCallback(dialog, XmNnoMatchCallback, dialog_callback, NULL);
    XmStringFree(t);
    while (--i >= 0)
        XmStringFree(str[i]); /* free elements of array */
    XtFree(str); /* now free array pointer */
    XtManageChild(dialog);
}

/* dialog_callback() --The Ok button was selected or the user 
 * input a name by himself.  Determine whether the result is
 * a valid name by looking at the "reason" field.
 */
void
dialog_callback(w, client_data, cbs)
Widget w;
XtPointer client_data;
XmSelectionBoxCallbackStruct *cbs;
{
    char msg[256], *prompt, *value;
    int dialog_type;

    switch (cbs->reason) {
        case XmCR_OK:
            prompt = "Selection:\n\t";
            dialog_type = XmDIALOG_MESSAGE;
            break;
        case XmCR_NO_MATCH:
            prompt = "Not a valid selection:\n";
            dialog_type = XmDIALOG_ERROR;
            break;
        default:
            prompt = "Unknown selection:\n";
            dialog_type = XmDIALOG_ERROR;
    }
    XmStringGetLtoR(cbs->value, XmSTRING_DEFAULT_CHARSET, &value);
    sprintf(msg, "%s%s", prompt, value);
    XtFree(value);
    (void) PostDialog(XtParent(XtParent(w)), dialog_type, msg);
    XmStringFree(prompt);
    if (cbs->reason != XmCR_NO_MATCH)
        XtDestroyWidget(w); /* doesn't work for pre-Motif 1.1.1 */
}

/*
 * PostDialog() -- a generalized routine that allows the programmer
 * to specify a dialog type (message, information, error, help,
 * etc..), and the message to show.
 */
Widget
PostDialog(parent, dialog_type, msg)
int dialog_type;
char *msg;
{
    Widget dialog;
    XmString text;

    dialog = XmCreateMessageDialog(parent, "dialog", NULL, 0);
    text = XmStringCreateLtoR(msg, XmSTRING_DEFAULT_CHARSET);
    XtVaSetValues(dialog,
        XmNdialogType,    dialog_type,
        XmNmessageString, text,
        NULL);
    XmStringFree(text);
    XtUnmanageChild(
        XmMessageBoxGetChild(dialog, XmDIALOG_CANCEL_BUTTON));
    XtSetSensitive(
        XmMessageBoxGetChild(dialog, XmDIALOG_HELP_BUTTON), False);
    XtAddCallback(dialog, XmNokCallback, XtDestroyWidget, NULL);
    XtManageChild(dialog);
    return dialog;
}
