/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* drawing.c -- extremely simple drawing program that introduces
 * the DrawingArea widget.  This widget provides a window for
 * drawing and some callbacks for getting input and other misc
 * events (resize and expose).  It's also a manager, so it can
 * have children.  There is no geometry management, tho.
 */
#include <Xm/DrawingA.h>
#include <Xm/RowColumn.h>

Display *dpy;

main(argc, argv)
int argc;
char *argv[];
{
    Widget toplevel, drawing_a;
    XtAppContext app;
    XGCValues gcv;
    GC gc;
    void drawing_area_callback();

    /* Initialize toolkit and create 500x500 top level shell */
    toplevel = XtVaAppInitialize(&app, "Demos", NULL, 0, 
        &argc, argv, NULL, NULL);

    /* avoiding having to use a macro everywhere else... */
    dpy = XtDisplay(toplevel);

    /* Create a DrawingArea widget. */
    drawing_a = XtVaCreateWidget("drawing_a",
        xmDrawingAreaWidgetClass, toplevel,
	XmNunitType,   Xm1000TH_INCHES,
	XmNheight,     2000, /* 2 inches? */
	XmNwidth,      5000, /* 5 inches? */
        NULL);

    /* add callback to trap input events */
    XtAddCallback(drawing_a, XmNinputCallback,
        drawing_area_callback, NULL);
    /* add callback to trap resizing */
    XtAddCallback(drawing_a, XmNresizeCallback,
        drawing_area_callback, NULL);

    /* Create a GC for drawing (in callback).  Attach to
     * DrawingArea's XmNuserData to avoid having to make global.
     */
    gcv.foreground = BlackPixelOfScreen(XtScreen(drawing_a));
    gcv.background = WhitePixelOfScreen(XtScreen(drawing_a));
    gc = XCreateGC(dpy, RootWindowOfScreen(XtScreen(drawing_a)),
        GCForeground|GCBackground, &gcv);
    XtVaSetValues(drawing_a, XmNuserData, gc, NULL);

    XtManageChild(drawing_a);
    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}

/* Callback routine for DrawingArea's input and resize callbacks.
 * This is also used as the PushButton's callback.  Determine which
 * activated us by testing the cbs->reason field.
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
        /* activated by DrawingArea input event */
        if (event->xany.type == ButtonPress) {
            /* anchor initial point */
            x = event->xbutton.x;
            y = event->xbutton.y;
        } else if (event->xany.type == ButtonRelease) {
            /* draw full line; get GC and use in XDrawLine() */
            GC gc;
            XtVaGetValues(widget, XmNuserData, &gc, NULL);
            XDrawLine(dpy, cbs->window, gc, x, y,
                event->xbutton.x, event->xbutton.y);
            x = event->xbutton.x;
            y = event->xbutton.y;
        }
    }

    if (cbs->reason == XmCR_RESIZE && cbs->window)
        XClearWindow(dpy, cbs->window);

    if (cbs->reason == XmCR_ACTIVATE)
        /* activated by pushbutton -- clear parent's window */
        XClearWindow(dpy, XtWindow(XtParent(widget)));
}
