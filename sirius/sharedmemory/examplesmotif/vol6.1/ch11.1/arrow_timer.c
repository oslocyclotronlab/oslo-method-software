/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* arrow_timer.c -- demonstrate continuous callbacks using
 * ArrowButton widgets.  Display up and down ArrowButtons and
 * attach arm and disarm callbacks to them to start and stop timer
 * that is called repeatedly while the button is down.  A label
 * that has a value changes either positively or negatively
 * by single increments while the button is depressed.
 */
#include <Xm/ArrowBG.h>
#include <Xm/Form.h>
#include <Xm/RowColumn.h>
#include <Xm/LabelG.h>

XtAppContext app;
Widget label;
XtIntervalId arrow_timer_id;
typedef struct value_range {
    int value, min, max;
} ValueRange;

main(argc, argv)
int argc;
char *argv[];
{
    Widget w, toplevel, rowcol;
    XmString btn_text;
    void start_stop();
    ValueRange range;

    toplevel = XtVaAppInitialize(&app, "Demos",
        NULL, 0, &argc, argv, NULL, NULL);

    rowcol = XtVaCreateWidget("rowcol",
        xmRowColumnWidgetClass, toplevel,
        XmNorientation, XmHORIZONTAL,
        NULL);

    w = XtVaCreateManagedWidget("arrow_up",
        xmArrowButtonGadgetClass, rowcol,
        XmNarrowDirection,   XmARROW_UP,
        NULL);
    XtAddCallback(w, XmNarmCallback, start_stop, 1);
    XtAddCallback(w, XmNdisarmCallback, start_stop, 1);

    w = XtVaCreateManagedWidget("arrow_dn",
        xmArrowButtonGadgetClass, rowcol,
        XmNarrowDirection,   XmARROW_DOWN,
        NULL);
    XtAddCallback(w, XmNarmCallback, start_stop, -1);
    XtAddCallback(w, XmNdisarmCallback, start_stop, -1);

    range.value = 0;
    range.min = -50;
    range.max = 50;
    label = XtVaCreateManagedWidget("label",
        xmLabelGadgetClass, rowcol,
        XtVaTypedArg, XmNlabelString, XmRString, "0   ", 3,
        XmNuserData, &range,
        NULL);

    XtManageChild(rowcol);
    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}

/* start_stop is used to start or stop the incremental changes to
 * the label's value.  When the button goes down, the reason is
 * XmCR_ARM and the timer starts.  XmCR_DISARM disables the timer.
 * Keypresses are special cases since they do not generate disarm
 * callbacks (fixed in a later version of the toolkit).  For backwards
 * compatibility, change_value() must be called with a 0 id to
 * prevent the timer from starting at all.
 */
void
start_stop(w, incr, cbs)
Widget w;
int incr;
XmArrowButtonCallbackStruct *cbs;
{
    void change_value();

    if (cbs->reason == XmCR_ARM)
        /* pass either 0 or 1 as "id" to change_value */
        change_value(incr, cbs->event->type == ButtonPress);
    else if (cbs->reason == XmCR_DISARM)
        XtRemoveTimeOut(arrow_timer_id);
}

/* change_value is called each time the timer expires.  This function
 * is also used to initiate the timer.  The "id" represents that timer
 * ID returned from the last call to XtAppAddTimeOut().  If id <= 1,
 * the function was called from start_stop(), not a timeout.  If it's
 * 0, don't start the timer, just change the value.  If the value has
 * reached its maximum or minimum, don't restart timer, just return.
 * If id == 1, this is the first timeout so make it be longer to allow
 * the user to release the button and avoid getting into the "speedy"
 * part of the timeouts.
 */
void
change_value(incr, id)
int incr;
XtIntervalId id;
{
    ValueRange *range;
    char buf[8];

    XtVaGetValues(label, XmNuserData, &range, NULL);
    if (range->value + incr > range->max ||
        range->value + incr < range->min)
        return;
    range->value += incr;
    sprintf(buf, "%d", range->value);
    XtVaSetValues(label,
        XtVaTypedArg, XmNlabelString, XmRString, buf, strlen(buf),
        XmNuserData, range,
        NULL);
    if (id > 0)
        arrow_timer_id =
            XtAppAddTimeOut(app, id==1? 100: 500, change_value, incr);
}
