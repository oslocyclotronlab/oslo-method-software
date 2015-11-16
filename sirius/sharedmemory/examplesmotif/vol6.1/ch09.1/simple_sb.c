/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* simple_sb.c -- demonstrate the Scrollbar resource values from
 * a ScrolledText object.  This is used as an introductory examination
 * of the resources used by Scrollbars.
 */
#include <Xm/RowColumn.h>
#include <Xm/PushBG.h>
#include <Xm/Text.h>

/* print the "interesting" resource values of a scrollbar */
void
get_sb(pb, scrollbar)
Widget pb, scrollbar;
{
    int increment=0, maximum=0, minimum=0, page_incr=0, slider_size=0, value=0;

    XtVaGetValues(scrollbar,
        XmNincrement,     &increment,
        XmNmaximum,       &maximum,
        XmNminimum,       &minimum,
        XmNpageIncrement, &page_incr,
        XmNsliderSize,    &slider_size,
        XmNvalue,         &value,
        NULL);
    printf("increment=%d, max=%d, min=%d, page=%d, slider=%d, value=%d\n",
        increment, maximum, minimum, page_incr, slider_size, value);
}

main(argc, argv)
int argc;
char *argv[];
{
    Widget        toplevel, rowcol, text_w, pb, sb;
    XtAppContext  app;
    Arg           args[5];

    toplevel = XtVaAppInitialize(&app, "Demos",
        NULL, 0, &argc, argv, NULL, NULL);

    /* RowColumn contains ScrolledText and PushButton */
    rowcol = XtVaCreateWidget("rowcol",
        xmRowColumnWidgetClass, toplevel, NULL);

    XtSetArg(args[0], XmNrows,      10);
    XtSetArg(args[1], XmNcolumns,   80);
    XtSetArg(args[2], XmNeditMode,  XmMULTI_LINE_EDIT);
    XtSetArg(args[3], XmNscrollHorizontal,  False);
    XtSetArg(args[4], XmNwordWrap,  True);
    text_w = XmCreateScrolledText(rowcol, "text_w", args, 5);
    XtManageChild(text_w);

    /* get the scrollbar from ScrolledWindow associated with Text widget */
    XtVaGetValues(XtParent(text_w), XmNverticalScrollBar, &sb, NULL);

    /* provide a bushbutton to obtain the scrollbar's resource values */
    pb = XtVaCreateManagedWidget("Scrollbar Values",
        xmPushButtonGadgetClass, rowcol, NULL);
    XtAddCallback(pb, XmNactivateCallback, get_sb, sb);

    XtManageChild(rowcol);

    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}
