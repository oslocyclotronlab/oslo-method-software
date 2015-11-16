/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* charset.c --
 * Create a compound string using a charset which defaults to
 * the "9x15" font.  Three pushbuttons are created: pb1, pb2
 * and pb3.  The user can specify resources so that each of the
 * widgets have different fonts associated with the "charset"
 * specified in the compound string.
 */
#include <Xm/RowColumn.h>
#include <Xm/PushBG.h>

String fallbacks[] = { "*fontList:9x15=charset", NULL };

main(argc, argv)
int argc;
char *argv[];
{
    Widget        toplevel, rowcol;
    XtAppContext  app;
    XmString      text;
    Display      *dpy;

    toplevel = XtVaAppInitialize(&app, argv[0], NULL, 0,
        &argc, argv, fallbacks, NULL);

    text = XmStringCreateSimple("Testing, testing...");

    rowcol = XtVaCreateWidget("rowcol",
        xmRowColumnWidgetClass, toplevel,
        NULL);

    XtVaCreateManagedWidget("pb1", xmPushButtonGadgetClass, rowcol,
        XmNlabelString, text,
        NULL);

    XtVaCreateManagedWidget("pb2", xmPushButtonGadgetClass, rowcol,
        XmNlabelString, text,
        NULL);

    XtVaCreateManagedWidget("pb3", xmPushButtonGadgetClass, rowcol,
        XmNlabelString, text,
        NULL);

    XmStringFree(text);
    XtManageChild(rowcol);
    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}
