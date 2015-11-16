/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* drawing.c -- extremely simple drawing program that introduces
 * the DrawingArea widget.  This widget provides a window for
 * drawing and some callbacks for getting input and other misc
 * events.  It's also a manager, so it can have children.
 * There is no geometry management, tho.
 */
#include <Xm/DrawingA.h>
#include <Xm/PushBG.h>
#include <Xm/RowColumn.h>

main(argc, argv)
int argc;
char *argv[];
{
    Widget toplevel, drawing_a, pb;
    XtAppContext app;
    XGCValues gcv;
    GC gc;
    void drawing_area_callback();

    toplevel = XtVaAppInitialize(&app, "Demos", NULL, 0, 
        &argc, argv, NULL,
        XmNwidth,  400,
        XmNheight, 300,
        NULL);

    /* Create a DrawingArea widget. */
    drawing_a = XtVaCreateWidget("drawing_a",
        xmDrawingAreaWidgetClass, toplevel,
        NULL);
    /* add callback for all mouse and keyboard input events */
    XtAddCallback(drawing_a, XmNinputCallback, drawing_area_callback, NULL);

    /* Since we're going to be drawing, we will be using Xlib routines
     * and therefore need a graphics context.  Create a GC and attach
     * to the DrawingArea's XmNuserData to avoid having to make global
     * variable. (Avoiding globals is a good design principle to follow.)
     */
    gcv.foreground = BlackPixelOfScreen(XtScreen(drawing_a));
    gc = XCreateGC(XtDisplay(drawing_a),
        RootWindowOfScreen(XtScreen(drawing_a)), GCForeground, &gcv);
    XtVaSetValues(drawing_a, XmNuserData, gc, NULL);

    /* add a pushbutton the user can use to clear the canvas */
    pb = XtVaCreateManagedWidget("Clear",
        xmPushButtonGadgetClass, drawing_a,
        NULL);
    /* if activated, call same callback as XmNinputCallback. */
    XtAddCallback(pb, XmNactivateCallback, drawing_area_callback, NULL);

    XtManageChild(drawing_a);
    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}

/* Callback routine for DrawingArea's input callbacks and the
 * PushButton's activate callback.  Determine which it is by
 * testing the cbs->reason field.
 */
void
drawing_area_callback(widget, data, cbs)
Widget widget;
XtPointer data;
XmDrawingAreaCallbackStruct *cbs;
{
    static Position x, y;
    XEvent *event = cbs->event;

    if (cbs->reason == XmCR_INPUT) {
        /* activated by DrawingArea input event -- draw lines.
         * Button Down events anchor the initial point and Button
         * Up draws from the anchor point to the button-up point.
         */
        if (event->xany.type == ButtonPress) {
            /* anchor initial point (i.e., save its value) */
            x = event->xbutton.x;
            y = event->xbutton.y;
        } else if (event->xany.type == ButtonRelease) {
            /* draw full line; get GC and use in XDrawLine() */
            GC gc;
            XtVaGetValues(widget, XmNuserData, &gc, NULL);
            XDrawLine(event->xany.display, cbs->window, gc, x, y,
                event->xbutton.x, event->xbutton.y);
            x = event->xbutton.x;
            y = event->xbutton.y;
        }
    }

    if (cbs->reason == XmCR_ACTIVATE)
        /* activated by pushbutton -- clear parent's window */
        XClearWindow(event->xany.display, XtWindow(XtParent(widget)));
}
