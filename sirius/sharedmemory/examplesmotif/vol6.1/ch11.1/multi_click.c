/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* multi_click.c -- demonstrate handling multiple PushButton clicks.
 * First, obtain the time interval of what constitutes a multiple
 * button click from the display and pass this as the client_data
 * for the button_click() callback function.  In the callback, single
 * button clicks set a timer to expire on that interval and call the
 * function process_clicks().  Double clicks remove the timer and
 * just call process_clicks() directly.
 */
#include <Xm/PushB.h>

XtAppContext app;

main(argc, argv)
int argc;
char *argv[];
{
    Widget toplevel, button, WidgetCreate();
    void button_click();
    XmString btn_text;
    int interval;

    toplevel = XtVaAppInitialize(&app, "Demos",
        NULL, 0, &argc, argv, NULL, NULL);

    /* get how long for a double click */
    interval = XtGetMultiClickTime(XtDisplay(toplevel));
    printf("interval = %d\n", interval);

    btn_text = XmStringCreateSimple("Push Here");
    button = XtVaCreateManagedWidget("button",
        xmPushButtonWidgetClass, toplevel,
        XmNlabelString, btn_text,
        XmNwidth,       50,
        XmNheight,      25,
        NULL);
    XmStringFree(btn_text);
    XtAddCallback(button, XmNactivateCallback, button_click, interval);

    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}

/* Process button clicks.  Single clicks set a timer, double clicks
 * remove the timer, and extended clicks are ignored.
 */
void
button_click(w, interval, cbs)
Widget w;
int interval;
XmPushButtonCallbackStruct *cbs;
{
    static XtIntervalId id;
    void process_clicks();

    if (cbs->click_count == 1)
        id = XtAppAddTimeOut(app, interval, process_clicks, False);
    else if (cbs->click_count == 2) {
        XtRemoveTimeOut(id);
        process_clicks(True);
    }
}

/* This function won't be called until we've established whether
 * or not a single or a double click has occured.
 */
void
process_clicks(double_click)
int double_click;
{
    if (double_click)
        puts("Double click");
    else
        puts("Single click");
}
