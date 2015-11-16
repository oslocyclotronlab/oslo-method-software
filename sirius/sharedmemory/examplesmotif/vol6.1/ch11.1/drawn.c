/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* drawn.c -- demonstrate the DrawnButton widget by drawing a
 * common X logo into its window.  This is hardly much different
 * from a a PushButton widget, but the DrawnButton isn't much
 * different, except for a couple more callback routines...
 */
#include <Xm/DrawnB.h>
#include <Xm/RowColumn.h>

main(argc, argv)
int argc;
char *argv[];
{
    XtAppContext app;
    Widget toplevel, rowcol, button;
    Pixmap pixmap;
    Pixel fg, bg;
    void my_callback();

    toplevel = XtVaAppInitialize(&app, "Demos",
        NULL, 0, &argc, argv, NULL, NULL);

    rowcol = XtVaCreateManagedWidget("_rowcol",
        xmRowColumnWidgetClass, toplevel, NULL);

    XtVaGetValues(rowcol,
        XmNforeground, &fg,
        XmNbackground, &bg,
        NULL);
    pixmap = XmGetPixmap(XtScreen(rowcol), "xlogo64", fg, bg);
    button = XtVaCreateManagedWidget("button",
        xmDrawnButtonWidgetClass, rowcol,
        XmNlabelType,    XmPIXMAP,
        XmNlabelPixmap,  pixmap,
        NULL);
    XtAddCallback(button, XmNactivateCallback, my_callback, NULL);
    XtAddCallback(button, XmNexposeCallback, my_callback, NULL);
    XtAddCallback(button, XmNresizeCallback, my_callback, NULL);

    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}

void
my_callback(w, client_data, cbs)
Widget w;
XtPointer client_data;
XmDrawnButtonCallbackStruct *cbs;
{
    if (cbs->reason == XmCR_ACTIVATE)
        printf("%s: pushed %d times\n", XtName(w), cbs->click_count);
    else if (cbs->reason == XmCR_EXPOSE)
        puts("Expose");
    else /* XmCR_RESIZE */
        puts("Resize");
}
