/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* tictactoe.c -- demonstrate how fractionBase and XmATTACH_POSITIONs
 * work in Form widgets.
 */
#include <Xm/PushBG.h>
#include <Xm/Form.h>

main(argc, argv)
int argc;
char *argv[];
{
    XtAppContext app;
    Widget toplevel, parent, w;
    int x, y;
    extern void pushed();  /* callback for each PushButton */

    toplevel = XtVaAppInitialize(&app, "Demos", NULL, 0,
        &argc, argv, NULL, NULL);

    parent = XtVaCreateManagedWidget("form", xmFormWidgetClass, toplevel,
        XmNfractionBase,    3,
        NULL);
    for (x = 0; x < 3; x++)
        for (y = 0; y < 3; y++) {
            w = XtVaCreateManagedWidget(" ",
                xmPushButtonGadgetClass, parent,
                XmNtopAttachment,    XmATTACH_POSITION,
                XmNtopPosition,      y,
                XmNleftAttachment,   XmATTACH_POSITION,
                XmNleftPosition,     x,
                XmNrightAttachment,  XmATTACH_POSITION,
                XmNrightPosition,    x+1,
                XmNbottomAttachment, XmATTACH_POSITION,
                XmNbottomPosition,   y+1,
                NULL);
            XtAddCallback(w, XmNactivateCallback, pushed, NULL);
        }

    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}

void
pushed(w, client_data, cbs)
Widget     w;           /* The PushButton that got activated */
XtPointer  client_data; /* unused -- NULL was passed to XtAddCallback() */
XmPushButtonCallbackStruct *cbs;
{
    char buf[2];
    XmString str;

    /* Shift key gets an O.  (xbutton and xkey happen to be similar) */
    if (cbs->event->xbutton.state & ShiftMask)
        buf[0] = '0';
    else
        buf[0] = 'X';
    buf[1] = 0;
    str = XmStringCreateSimple(buf);
    XtVaSetValues(w, XmNlabelString, str, NULL);
    XmStringFree(str);
}
