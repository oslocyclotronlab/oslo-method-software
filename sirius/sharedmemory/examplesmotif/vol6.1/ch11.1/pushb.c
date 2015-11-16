/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* pushb.c -- demonstrate the pushbutton widget.  Display one
 * PushButton with a single callback routine.  Print the name
 * of the widget and the number of "multiple clicks".  This
 * value is maintained by the toolkit.
 */
#include <Xm/PushB.h>

main(argc, argv)
int argc;
char *argv[];
{
    XtAppContext app;
    Widget toplevel, button;
    void my_callback();
    XmString btn_text;

    toplevel = XtVaAppInitialize(&app, "Demos",
        NULL, 0, &argc, argv, NULL, NULL);

    btn_text = XmStringCreateSimple("Push Here");
    button = XtVaCreateManagedWidget("button",
        xmPushButtonWidgetClass, toplevel,
        XmNlabelString, btn_text,
        XmNwidth,       50,
        XmNheight,      25,
        NULL);
    XmStringFree(btn_text);
    XtAddCallback(button, XmNactivateCallback, my_callback, NULL);

    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}

void
my_callback(w, client_data, cbs)
Widget w;
XtPointer client_data;
XmPushButtonCallbackStruct *cbs;
{
    printf("%s: pushed %d times\n", XtName(w), cbs->click_count);
}
