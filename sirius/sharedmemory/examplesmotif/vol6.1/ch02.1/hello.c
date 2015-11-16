/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* hello.c --
 * Initialize the toolkit using an application context and a toplevel
 * shell widget, then create a pushbutton that says Hello using
 * the R4 varargs interface.
 */
#include <Xm/Xm.h>
#include <Xm/PushB.h>

main(argc, argv)
char *argv[];
{
    Widget        toplevel, button;
    XtAppContext  app;
    void i_was_pushed();
    XmString label;

    toplevel = XtVaAppInitialize(&app, "Hello", NULL, 0,
        &argc, argv, NULL, NULL);

    label = XmStringCreateSimple("Push here to say hello"); 
    button = XtVaCreateManagedWidget("pushme",
        xmPushButtonWidgetClass, toplevel,
        XmNlabelString, label,
        NULL);
    XmStringFree(label);
    XtAddCallback(button, XmNactivateCallback, i_was_pushed, NULL);

    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}

void
i_was_pushed(w, client_data, cbs)
Widget w;
XtPointer client_data;
XmPushButtonCallbackStruct *cbs;
{
    printf("Hello Yourself!\n");
}

