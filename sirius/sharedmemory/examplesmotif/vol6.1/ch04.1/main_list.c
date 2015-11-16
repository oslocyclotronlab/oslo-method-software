/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* main_list.c -- use the ScrolledList window as the feature
 * component of a MainWindow widget.
 */
#include <Xm/MainW.h>
#include <Xm/List.h>

main(argc, argv)
char *argv[];
{
    Widget toplevel, main_w, list_w;
    XtAppContext app;
    Pixmap pixmap;

    toplevel = XtVaAppInitialize(&app, "Demos",
        NULL, 0, &argc, argv, NULL, NULL);

    main_w = XtVaCreateManagedWidget("main_window",
        xmMainWindowWidgetClass,   toplevel,
        NULL);

    list_w = XmCreateScrolledList(main_w, "main_list", NULL, 0);
    XtVaSetValues(list_w,
        XtVaTypedArg, XmNitems, XmRString,
            "Red, Green, Blue, Orange, Maroon, Grey, Black, White", 53,
        XmNitemCount, 8,
        XmNvisibleItemCount, 5,
        NULL);
    XtManageChild(list_w);

    /* set the list_w as the "work area" of the main window */
    XtVaSetValues(main_w, XmNworkWindow, XtParent(list_w), NULL);
    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}
