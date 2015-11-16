/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* free_hand.c -- simple drawing program that does freehand
 * drawing.  We use translations to do all the event handling
 * for us rather than using the drawing area's XmNinputCallback.
 */
#include <Xm/MainW.h>
#include <Xm/DrawingA.h>
#include <Xm/PushBG.h>
#include <Xm/RowColumn.h>

GC gc;
Pixmap pixmap;
/* dimensions of drawing area (pixmap) */
Dimension width, height;

main(argc, argv)
int argc;
char *argv[];
{
    Widget toplevel, main_w, drawing_a, pb;
    XtAppContext app;
    XGCValues gcv;
    void draw(), redraw(), clear_it();
    XtActionsRec actions;
    String translations = /* for the DrawingArea widget */
        /* ManagerGadget* functions are necessary for DrawingArea widgets
         * that steal away button events from the normal translation tables.
         */
        "<Btn1Down>:   draw(down) ManagerGadgetArm()  \n\
         <Btn1Up>:     draw(up)   ManagerGadgetActivate()  \n\
         <Btn1Motion>: draw(motion) ManagerGadgetButtonMotion()";

    toplevel = XtVaAppInitialize(&app, "Demos", NULL, 0, 
        &argc, argv, NULL, NULL);

    /* Create a MainWindow to contain the drawing area */
    main_w = XtVaCreateManagedWidget("main_w",
        xmMainWindowWidgetClass, toplevel,
        XmNscrollingPolicy, XmAUTOMATIC,
        NULL);

    /* Add the "draw" action/function used by the translation table */
    actions.string = "draw";
    actions.proc = draw;
    XtAppAddActions(app, &actions, 1);

    /* Create a DrawingArea widget.  Make it 5 inches wide by 6 inches tall.
     * Don't let it resize so the Clear Button doesn't force a resize.
     */
    drawing_a = XtVaCreateManagedWidget("drawing_a",
        xmDrawingAreaWidgetClass, main_w,
        XmNtranslations, XtParseTranslationTable(translations),
        XmNunitType,     Xm1000TH_INCHES,
        XmNwidth,        5000, /* 5 inches */
        XmNheight,       6000, /* 6 inches */
        XmNresizePolicy, XmNONE,  /* remain this a fixed size */
        NULL);
    /* When scrolled, the drawing area will get expose events */
    XtAddCallback(drawing_a, XmNexposeCallback, redraw, NULL);

    /* convert drawing area back to pixels to get its width and height */
    XtVaSetValues(drawing_a, XmNunitType, XmPIXELS, NULL);
    XtVaGetValues(drawing_a, XmNwidth, &width, XmNheight, &height, NULL);
    /* create a pixmap the same size as the drawing area. */
    pixmap = XCreatePixmap(XtDisplay(drawing_a),
        RootWindowOfScreen(XtScreen(drawing_a)), width, height,
        DefaultDepthOfScreen(XtScreen(drawing_a)));

    /* Create a GC for drawing (callback).  Used a lot -- make global */
    gcv.foreground = WhitePixelOfScreen(XtScreen(drawing_a));
    gc = XCreateGC(XtDisplay(drawing_a),
        RootWindowOfScreen(XtScreen(drawing_a)), GCForeground, &gcv);
    /* clear pixmap with white */
    XFillRectangle(XtDisplay(drawing_a), pixmap, gc, 0, 0, width, height);
    /* drawing is now drawn into with "black"; change the gc */
    XSetForeground(XtDisplay(drawing_a), gc,
        BlackPixelOfScreen(XtScreen(drawing_a)));

    pb = XtVaCreateManagedWidget("Clear",
        xmPushButtonGadgetClass, drawing_a, NULL);
    /* Pushing the clear button calls clear_it() */
    XtAddCallback(pb, XmNactivateCallback, clear_it, drawing_a);

    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}

/* Action procedure to respond to any of the events from the
 * translation table declared in main().  This function is called
 * in response to Button1 Down, Up and Motion events.  Basically,
 * we're just doing a freehand draw -- not lines or anything.
 */
void
draw(widget, event, args, num_args)
Widget widget;
XButtonEvent *event;
String *args;
int *num_args;
{
    static Position x, y;

    if (*num_args != 1)
        XtError("Wrong number of args!");

    if (strcmp(args[0], "down")) {
        /* if it's not "down", it must either be "up" or "motion"
         * draw full line from anchor point to new point.
         */
        XDrawLine(event->display, event->window, gc, x, y, event->x, event->y);
        XDrawLine(event->display, pixmap, gc, x, y, event->x, event->y);
    }

    /* freehand is really a bunch of line segements; save this point */
    x = event->x;
    y = event->y;
}

/* Clear the window by clearing the pixmap and calling XCopyArea() */
void
clear_it(pb, drawing_a, cbs)
Widget pb, drawing_a;
XmPushButtonCallbackStruct *cbs;
{
    /* clear pixmap with white */
    XSetForeground(XtDisplay(drawing_a), gc,
        WhitePixelOfScreen(XtScreen(drawing_a)));
    XFillRectangle(XtDisplay(drawing_a), pixmap, gc, 0, 0, width, height);
    /* drawing is now done using black; change the gc */
    XSetForeground(XtDisplay(drawing_a), gc,
        BlackPixelOfScreen(XtScreen(drawing_a)));
    XCopyArea(cbs->event->xbutton.display, pixmap, XtWindow(drawing_a), gc,
        0, 0, width, height, 0, 0);
}

/* redraw is called whenever all or portions of the drawing area is
 * exposed.  This includes newly exposed portions of the widget resulting
 * from the user's interaction with the scrollbars.
 */
void
redraw(drawing_a, client_data, cbs)
Widget    drawing_a;
XtPointer client_data;
XmDrawingAreaCallbackStruct *cbs;
{
    XCopyArea(cbs->event->xexpose.display, pixmap, cbs->window, gc,
        0, 0, width, height, 0, 0);
}
