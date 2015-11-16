/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* simple_radio.c -- demonstrate a simple radio box by using
 * XmVaCreateSimpleRadioBox().  Create a box with 3 toggles:
 * "one", "two" and "three".  The callback routine prints
 * the most recently selected choice.
 */
#include <Xm/RowColumn.h>
#include <X11/StringDefs.h>

void
toggled(widget, which, state)
Widget widget;
int which;
XmToggleButtonCallbackStruct *state;
{
    printf("%s: %s\n", XtName(widget), state->set? "on" : "off");
}

main(argc, argv)
int argc;
char *argv[];
{
    Widget toplevel, radio_box;
    XtAppContext app;
    XmString one, two, three;

    toplevel = XtVaAppInitialize(&app, "Demos", NULL, 0,
        &argc, argv, NULL, NULL);

    one  = XmStringCreateSimple("one");
    two  = XmStringCreateSimple("two");
    three  = XmStringCreateSimple("three");
    radio_box = XmVaCreateSimpleRadioBox(toplevel, "radio_box",
        0,  /* the inital choice */
        toggled, /* the callback routine */
        XmVaRADIOBUTTON, one,  NULL, NULL, NULL,
        XmVaRADIOBUTTON, two,  NULL, NULL, NULL,
        XmVaRADIOBUTTON, three, NULL, NULL, NULL,
        NULL);
    XmStringFree(one);
    XmStringFree(two);
    XmStringFree(three);

    XtManageChild(radio_box);

    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}
