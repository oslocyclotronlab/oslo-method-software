/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* simple_radio.c -- demonstrate a simple radio box.  Create a
 * box with 3 toggles: "one", "two" and "three".  The callback
 * routine prints the most recently selected choice.  Maintain
 * a global variable that stores the most recently selected.
 */
#include <Xm/ToggleBG.h>
#include <Xm/RowColumn.h>

int toggle_item_set;

void
toggled(widget, which, state)
Widget widget;
int which;
XmToggleButtonCallbackStruct *state;
{
    printf("%s: %s\n", XtName(widget), state->set? "on" : "off");
    if (state->set)
        toggle_item_set = which;
    else
        toggle_item_set = 0;
}

main(argc, argv)
int argc;
char *argv[];
{
    Widget toplevel, radio_box, one, two, three;
    XtAppContext app;

    toplevel = XtVaAppInitialize(&app, "Demos", NULL, 0,
        &argc, argv, NULL, NULL);

    radio_box = XmCreateRadioBox(toplevel, "radio_box", NULL, 0);

    one = XtVaCreateManagedWidget("One",
        xmToggleButtonGadgetClass, radio_box, NULL);
    XtAddCallback(one, XmNvalueChangedCallback, toggled, 1);

    two = XtVaCreateManagedWidget("Two",
        xmToggleButtonGadgetClass, radio_box, NULL);
    XtAddCallback(two, XmNvalueChangedCallback, toggled, 2);

    three = XtVaCreateManagedWidget("Three",
        xmToggleButtonGadgetClass, radio_box, NULL);
    XtAddCallback(three, XmNvalueChangedCallback, toggled, 3);

    XtManageChild(radio_box);

    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}
