/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* resize_shell.c -- demonstrate the max and min heights and widths.
 * This program should be run to really see how mwm displays the
 * size of the window as it is resized..
 */
#include <Xm/PushB.h>

main(argc, argv)
char *argv[];
{
    Widget toplevel, button;
    XtAppContext app;
    extern void getsize();

    toplevel = XtVaAppInitialize(&app, "Demos",
        NULL, 0, &argc, argv, NULL,
        XmNminWidth,     25,
        XmNminHeight,    20,
        XmNmaxWidth,     110,
        XmNmaxHeight,    90,
        XmNbaseWidth,    5,
        XmNbaseHeight,   5,
        XmNwidthInc,     5,
        XmNheightInc,    5,
        NULL);

    /* Pushbutton's callback prints the dimensions of the shell. */
    button = XtVaCreateManagedWidget("Print Size",
        xmPushButtonWidgetClass, toplevel, NULL);
    XtAddCallback(button, XmNactivateCallback, getsize, toplevel);

    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}

void
getsize(widget, shell)
Widget widget, shell;
{
    Dimension width, height;

    XtVaGetValues(shell, XmNwidth, &width, XmNheight, &height, NULL);
    printf("width = %d, height = %d\n", width, height);
}
