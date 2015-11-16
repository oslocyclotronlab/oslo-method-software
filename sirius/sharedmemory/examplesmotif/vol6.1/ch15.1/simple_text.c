/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* simple_text.c -- Create a minimally configured Text widget */
#include <Xm/Text.h>

main(argc, argv)
int argc;
char *argv[];
{
    Widget        toplevel;
    XtAppContext  app;

    toplevel = XtVaAppInitialize(&app, "Demos",
        NULL, 0, &argc, argv, NULL, NULL);

    XtVaCreateManagedWidget("text", xmTextWidgetClass, toplevel,
        XmNvalue,     "Now is the time...",
        NULL);

    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}
