/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* simple_popup.c -- demonstrate how to use a simple popup menu.
 * Create a main window that contains a DrawingArea widget, which
 * displays a popup menu when the user presses the menu button
 * (typically button 3).
 */
#include <Xm/RowColumn.h>
#include <Xm/MainW.h>
#include <Xm/DrawingA.h>

main(argc, argv)
int argc;
char *argv[];
{
    XmString line, square, circle, quit, quit_acc;
    Widget toplevel, main_w, drawing_a, popup_menu;
    void popup_cb(), input();
    XtAppContext app;

    toplevel = XtVaAppInitialize(&app, "Demos", NULL, 0,
        &argc, argv, NULL, NULL);

    /* Create a MainWindow widget that contains a DrawingArea in
     * its work window. (This happens by default.)
     */
    main_w = XtVaCreateManagedWidget("main_w",
        xmMainWindowWidgetClass, toplevel,
        XmNscrollingPolicy,  XmAUTOMATIC,
        NULL);
    /* Create a DrawingArea -- no actual drawing will be done. */
    drawing_a = XtVaCreateManagedWidget("drawing_a",
        xmDrawingAreaWidgetClass, main_w,
        XmNwidth, 500,
        XmNheight, 500,
        NULL);

    line = XmStringCreateSimple("Line");
    square = XmStringCreateSimple("Square");
    circle = XmStringCreateSimple("Circle");
    quit = XmStringCreateSimple("Quit");
    quit_acc = XmStringCreateSimple("Ctrl-C");
    popup_menu = XmVaCreateSimplePopupMenu(drawing_a, "popup", popup_cb,
        XmVaPUSHBUTTON, line, NULL, NULL, NULL,
        XmVaPUSHBUTTON, square, NULL, NULL, NULL,
        XmVaPUSHBUTTON, circle, NULL, NULL, NULL,
        XmVaSEPARATOR,
        XmVaPUSHBUTTON, quit, NULL, "Ctrl<Key>c", quit_acc,
        NULL);
    XmStringFree(line);
    XmStringFree(square);
    XmStringFree(circle);
    XmStringFree(quit);

    /* after popup menu is created, add callback for all input events */
    XtAddCallback(drawing_a, XmNinputCallback, input, popup_menu);

    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}

/* called in responses to events in the DrawingArea; button-3 pops up menu. */
void
input(widget, popup, cbs)
Widget widget;
Widget popup;   /* popup menu associated with drawing area */
XmDrawingAreaCallbackStruct *cbs;
{
    if (cbs->event->xany.type != ButtonPress ||
        cbs->event->xbutton.button != 3)
        return;

    /* Position the menu where the event occurred */
    XmMenuPosition(popup, (XButtonPressedEvent *)(cbs->event));
    XtManageChild(popup);
}

/* invoked when the user selects an item in the popup menu */
void
popup_cb(menu_item, item_no, cbs)
Widget menu_item;
int item_no;
XmAnyCallbackStruct *cbs;
{
    if (item_no == 3) /* Quit was selected -- exit */
        exit(0);
    puts(XtName(menu_item)); /* Otherwise, just print the selection */
}
