/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* set_minimum.c -- demonstrate how to set the minimum size of a
 * window to its initial size.  This method is useful if your program
 * is initially displayed at its minimum size, but it would be too
 * difficult to try to calculate ahead of time what the initial size
 * would be.
 */
#include <Xm/PushB.h>

static void getsize(), configure();

main(argc, argv)
char *argv[];
{
    Widget toplevel, button;
    XtAppContext app;

    toplevel = XtVaAppInitialize(&app, "Demos",
        NULL, 0, &argc, argv, NULL,
        XmNmaxWidth,     110,
        XmNmaxHeight,    90,
        XmNbaseWidth,    5,
        XmNbaseHeight,   5,
        XmNwidthInc,     5,
        XmNheightInc,    5,
        NULL);

    /* Add an event handler to trap the first configure event */
    XtAddEventHandler(toplevel, StructureNotifyMask, False, configure, NULL);

    /* Pushbutton's callback prints the dimensions of the shell. */
    button = XtVaCreateManagedWidget("Print Size",
        xmPushButtonWidgetClass, toplevel, NULL);
    XtAddCallback(button, XmNactivateCallback, getsize, toplevel);

    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}

static void
getsize(widget, shell)
Widget widget, shell;
{
    Dimension width, height;

    XtVaGetValues(shell, XmNwidth, &width, XmNheight, &height, NULL);
    printf("width = %d, height = %d\n", width, height);
}

static void
configure(shell, client_data, event)
Widget shell;
XtPointer client_data;
XConfigureEvent *event;
{
    if (event->type != ConfigureNotify)
        return;
    printf("width = %d, height = %d\n", event->width, event->height);
    XtVaSetValues(shell,
        XmNminWidth, event->width,
        XmNminHeight, event->height,
        NULL);
    XtRemoveEventHandler(shell, StructureNotifyMask, False, configure, NULL);
}
