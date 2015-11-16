/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* attach1.c -- demonstrate how attachments work in Form widgets. */

#include <Xm/PushBG.h>
#include <Xm/Form.h>

main(argc, argv)
int argc;
char *argv[];
{
    Widget toplevel, parent, one, two, three;
    XtAppContext app;

    toplevel = XtVaAppInitialize(&app, "Demos", NULL, 0,
        &argc, argv, NULL, NULL);

    parent = XtVaCreateManagedWidget("form",
        xmFormWidgetClass, toplevel, NULL);
    one = XtVaCreateManagedWidget("one",
        xmPushButtonGadgetClass, parent,
        XmNtopAttachment,   XmATTACH_FORM,
        XmNleftAttachment,  XmATTACH_FORM,
        NULL);
    two = XtVaCreateManagedWidget("two",
        xmPushButtonGadgetClass, parent,
        XmNleftAttachment,  XmATTACH_WIDGET,
        XmNleftWidget,      one,
        /* attach top of widget to same y coordinate as top of "one" */
        XmNtopAttachment,   XmATTACH_OPPOSITE_WIDGET,
        XmNtopWidget,       one,
        NULL);
    three = XtVaCreateManagedWidget("three",
        xmPushButtonGadgetClass, parent,
        XmNtopAttachment,   XmATTACH_WIDGET,
        XmNtopWidget,       one,
        /* attach left of widget to same x coordinate as left side of "one" */
        XmNleftAttachment,  XmATTACH_OPPOSITE_WIDGET,
        XmNleftWidget,      one,
        NULL);

    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}
