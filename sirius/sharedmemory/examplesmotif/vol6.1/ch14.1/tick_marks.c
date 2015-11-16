/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* tick_marks.c -- demonstrate a scale widget with tick marks. */

#include <Xm/Scale.h>
#include <Xm/LabelG.h>

#define MAX_VAL 20 /* arbitrary value */

main(argc, argv)
char *argv[];
{
    Widget        toplevel, scale;
    XtAppContext  app;
    int           i;

    toplevel = XtVaAppInitialize(&app, "Demos", NULL, 0,
        &argc, argv, NULL, NULL);

    scale = XtVaCreateManagedWidget("Days",
        xmScaleWidgetClass, toplevel,
        XtVaTypedArg,     XmNtitleString, XmRString, "Process load", 4,
        XmNmaximum,       MAX_VAL * 100,
        XmNdecimalPoints, 2,
        XmNshowValue,     True,
        NULL);
 
    for (i = 0; i < MAX_VAL; i++)
        XtVaCreateManagedWidget("-", xmLabelGadgetClass, scale, NULL);

    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}
