/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

#include <Xm/TextF.h>
#include <Xm/SelectioB.h>
#include <Xm/PushB.h>
#include <stdio.h>

#define charset XmSTRING_DEFAULT_CHARSET

main(argc, argv)
int argc;
char *argv[];
{
    Widget        toplevel, pb;
    XtAppContext  app;
    extern void popup_dialog();

    /* Create two shells -- toplevel contains the scrolled text and
     * the FileSelectionDialog serves as the second shell.
     */
    toplevel = XtVaAppInitialize(&app, "Demos",
        NULL, 0, &argc, argv, NULL, NULL);
    pb = XtVaCreateManagedWidget("push me",
        xmPushButtonWidgetClass, toplevel, NULL);
    XtAddCallback(pb, XmNactivateCallback, popup_dialog, NULL);

    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}

void
popup_dialog(w)
Widget w;
{
    Widget dialog, text_w;
    extern void exit(), read_text();

    dialog = XmCreatePromptDialog(w, "dialog", NULL, 0);

    XtVaSetValues(dialog,
        XtVaTypedArg, XmNselectionLabelString, XmRString,
            "Type something:", 15, /* strlen() = 15 */
            NULL);

    XtAddCallback(dialog, XmNcancelCallback, exit, NULL);

    text_w = XmSelectionBoxGetChild(dialog, XmDIALOG_TEXT);
    XtAddCallback(dialog, XmNokCallback, read_text, text_w);

    XtUnmanageChild(   /* no help available; unmanage Help button */
        XmSelectionBoxGetChild(dialog, XmDIALOG_HELP_BUTTON));

    XtManageChild(dialog);
}

/* callback routine when the user selects Ok in the Dialog box.
 * This function may also be called when Return is entered in
 * the associated Text widget (it causes the Ok button to activate).
 */
void
read_text(dialog, text_w)
Widget dialog;  /* dialog box */
Widget text_w; /* passed the text_w as client_data */
{
    char *string = XmTextFieldGetString(text_w);

    printf("%s activated; text = %s\n", XtName(dialog), string);
    XtFree(string);
}
