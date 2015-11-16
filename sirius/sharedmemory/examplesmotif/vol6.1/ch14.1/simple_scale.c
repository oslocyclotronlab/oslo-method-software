/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* scale.c -- demonstrate a few scale widgets. */

#include <Xm/Scale.h>
#include <Xm/RowColumn.h>

main(argc, argv)
char *argv[];
{
    Widget        toplevel, rowcol, scale;
    XtAppContext  app;
    void          new_value(); /* callback for Scale widgets */

    toplevel = XtVaAppInitialize(&app, "Demos", NULL, 0,
        &argc, argv, NULL, NULL);

    rowcol = XtVaCreateWidget("rowcol", xmRowColumnWidgetClass, toplevel,
        XmNorientation, XmHORIZONTAL,
        NULL);

    scale = XtVaCreateManagedWidget("Days",
        xmScaleWidgetClass, rowcol,
        XtVaTypedArg, XmNtitleString, XmRString, "Days", 4,
        XmNmaximum,   7,
        XmNminimum,   1,
        XmNvalue,     1,
        XmNshowValue, True,
        NULL);
    XtAddCallback(scale, XmNvalueChangedCallback, new_value, NULL);

    scale = XtVaCreateManagedWidget("Weeks",
        xmScaleWidgetClass, rowcol,
        XtVaTypedArg, XmNtitleString, XmRString, "Weeks", 5,
        XmNmaximum,   52,
        XmNminimum,   1,
        XmNvalue,     1,
        XmNshowValue, True,
        NULL);
    XtAddCallback(scale, XmNvalueChangedCallback, new_value, NULL);

    scale = XtVaCreateManagedWidget("Months",
        xmScaleWidgetClass, rowcol,
        XtVaTypedArg, XmNtitleString, XmRString, "Months", 6,
        XmNmaximum,   12,
        XmNminimum,   1,
        XmNvalue,     1,
        XmNshowValue, True,
        NULL);
    XtAddCallback(scale, XmNvalueChangedCallback, new_value, NULL);

    scale = XtVaCreateManagedWidget("Years",
        xmScaleWidgetClass, rowcol,
        XtVaTypedArg, XmNtitleString, XmRString, "Years", 5,
        XmNmaximum,   20,
        XmNminimum,   1,
        XmNvalue,     1,
        XmNshowValue, True,
        NULL);
    XtAddCallback(scale, XmNvalueChangedCallback, new_value, NULL);

    XtManageChild(rowcol);

    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}

void
new_value(scale_w, client_data, cbs)
Widget scale_w;
XtPointer client_data;
XmScaleCallbackStruct *cbs;
{
    printf("%s: %d\n", XtName(scale_w), cbs->value);
}
