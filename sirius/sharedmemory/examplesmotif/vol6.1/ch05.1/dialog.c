/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* hello.c -- your typical Hello World program using
 * an InformationDialog.
 */
#include <Xm/MessageB.h>
#include <Xm/PushB.h>

void
pushed(w)
Widget w;
{
    Widget dialog;
    extern void ok_pushed(), cancel_pushed(), help_pushed();
    Arg args[1];

    XtSetArg(args[0], XmNautoUnmanage, False);
    dialog = XmCreateMessageDialog(w, "notice", args, 1);
    XtAddCallback(dialog, XmNokCallback, ok_pushed, "Hi");
    XtAddCallback(dialog, XmNcancelCallback, cancel_pushed, "Foo");
    XtAddCallback(dialog, XmNhelpCallback, help_pushed, NULL);
    XtManageChild(dialog);
    XtPopup(XtParent(dialog), XtGrabNone);
}

main(argc, argv)
int argc;
char *argv[];
{
    XtAppContext app;
    Widget toplevel, pb;

    toplevel = XtVaAppInitialize(&app, "Demos", NULL, 0,
        &argc, argv, NULL, NULL);
    pb = XtVaCreateManagedWidget(NULL,
        xmPushButtonWidgetClass, toplevel, NULL);
    XtAddCallback(pb, XmNactivateCallback, pushed, NULL);

    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}

void
ok_pushed(w, client_data, reason)
Widget w;
XtPointer client_data;
XmAnyCallbackStruct *reason;
{
    printf("Ok was selected: %s\n", client_data);
    XtDestroyWidget(w);
}

void
cancel_pushed(w, client_data, reason)
Widget w;
XtPointer client_data;
XmAnyCallbackStruct *reason;
{
    printf("Cancel was selected: %s\n", client_data);
    XtDestroyWidget(w);
}

void
help_pushed(w, client_data, reason)
Widget w;
XtPointer client_data;
XmAnyCallbackStruct *reason;
{
    puts("Help was selected");
}
