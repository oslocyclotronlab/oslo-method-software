/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* show_pix.c -- a minimal MainWindow.  Use Label as the workWindow
 * to display a bitmap specified on the command line.
 */
#include <Xm/MainW.h>
#include <Xm/Label.h>

main(argc, argv)
int argc;
char *argv[];
{
    Widget toplevel, main_w, label;
    XtAppContext app;
    Pixmap pixmap;

    toplevel = XtVaAppInitialize(&app, "Demos",
        NULL, 0, &argc, argv, NULL, NULL);

    if (!argv[1]) {
        printf("usage: %s bitmap-file\n", *argv);
        exit(1);
    }

    main_w = XtVaCreateManagedWidget("main_window",
        xmMainWindowWidgetClass,   toplevel,
        XmNscrollBarDisplayPolicy, XmAS_NEEDED,
        XmNscrollingPolicy,        XmAUTOMATIC,
        NULL);

    /* Load bitmap given in argv[1] */
    pixmap = XmGetPixmap(XtScreen(toplevel), argv[1],
        BlackPixelOfScreen(XtScreen(toplevel)),
        WhitePixelOfScreen(XtScreen(toplevel)));

    if (pixmap == XmUNSPECIFIED_PIXMAP) {
        printf("can't create pixmap from %s\n", argv[1]);
        exit(1);
    }

    /* Now create label using pixmap */
    label = XtVaCreateManagedWidget("label", xmLabelWidgetClass, main_w,
        XmNlabelType,   XmPIXMAP,
        XmNlabelPixmap, pixmap,
        NULL);

    /* set the label as the "work area" of the main window */
    XtVaSetValues(main_w,
        XmNworkWindow, label,
        NULL);
    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}
