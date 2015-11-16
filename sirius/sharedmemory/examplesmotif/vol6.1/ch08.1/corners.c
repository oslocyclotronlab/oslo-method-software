/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* corners.c -- demonstrate widget layout management for a
 * BulletinBoard widget.  There are four widgets each labeled
 * top-left, top-right, bottom-left and bottom-right.  Their
 * positions in the bulletin board correspond to their names.
 * Only when the widget is resized does the geometry management
 * kick in and position the children in their correct locations.
 */
#include <Xm/BulletinB.h>
#include <Xm/PushBG.h>

char *corners[] = {
    "Top-Left", "Top-Right", "Bottom-Left", "Bottom-Right",
};

static void resize();

main(argc, argv)
int argc;
char *argv[];
{
    Widget toplevel, bboard;
    XtAppContext app;
    XtActionsRec rec;
    int i;

    /* Initialize toolkit and create toplevel shell */
    toplevel = XtVaAppInitialize(&app, "Demos", NULL, 0,
        &argc, argv, NULL, NULL);

    /* Create your standard BulletinBoard widget */
    bboard = XtVaCreateManagedWidget("bboard",
        xmBulletinBoardWidgetClass, toplevel, NULL);

    /* Set up a translation table that captures "Resize" events
     * (also called ConfigureNotify or Configure events).  If the
     * event is generated, call the function resize().
     */
    rec.string = "resize";
    rec.proc = resize;
    XtAppAddActions(app, &rec, 1);
    XtOverrideTranslations(bboard,
        XtParseTranslationTable("<Configure>: resize()"));

    /* Create children of the dialog -- a PushButton in each corner. */
    for (i = 0; i < XtNumber(corners); i++)
        XtVaCreateManagedWidget(corners[i],
            xmPushButtonGadgetClass, bboard, NULL);

    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}

/* resize(), the routine that is automatically called by Xt upon the
 * delivery of a Configure event.  This happens whenever the widget
 * gets resized.
 */
static void
resize(w, event, args, num_args)
CompositeWidget w;   /* The widget (BulletinBoard) that got resized */
XConfigureEvent *event;  /* The event struct associated with the event */
String args[]; /* unused */
int *num_args; /* unused */
{
    WidgetList children;
    int width = event->width;
    int height = event->height;
    Dimension w_width, w_height; 
    short margin_w, margin_h;

    /* get handle to BulletinBoard's children and marginal spacing */
    XtVaGetValues(w,
        XmNchildren, &children,
        XmNmarginWidth, &margin_w,
        XmNmarginHeight, &margin_h,
        NULL);

    /* place the top left widget */
    XtVaSetValues(children[0],
        XmNx, margin_w,
        XmNy, margin_h,
        NULL);
    /* top right */
    XtVaGetValues(children[1], XmNwidth, &w_width, NULL);
    XtVaSetValues(children[1],
        XmNx, width - margin_w - w_width,
        XmNy, margin_h,
        NULL);
    /* bottom left */
    XtVaGetValues(children[2], XmNheight, &w_height, NULL);
    XtVaSetValues(children[2],
        XmNx, margin_w,
        XmNy, height - margin_h - w_height,
        NULL);
    /* bottom right */
    XtVaGetValues(children[3],
        XmNheight, &w_height,
        XmNwidth, &w_width,
        NULL);
    XtVaSetValues(children[3],
        XmNx, width - margin_w - w_width,
        XmNy, height - margin_h - w_height,
        NULL);
}
