/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* allcaps.c -- demonstrate the XmNmodifyVerifyCallback for
 * Text widgets by using one to convert all typed input to
 * capital letters.
 */
#include <Xm/Text.h>
#include <Xm/LabelG.h>
#include <Xm/RowColumn.h>
#include <ctype.h>

void allcaps();

main(argc, argv)
int argc;
char *argv[];
{
    Widget        toplevel, text_w, rowcol;
    XtAppContext  app;

    toplevel = XtVaAppInitialize(&app, "Demos",
        NULL, 0, &argc, argv, NULL, NULL);

    rowcol = XtVaCreateWidget("rowcol",
        xmRowColumnWidgetClass, toplevel,
        XmNorientation, XmHORIZONTAL,
        NULL);

    XtVaCreateManagedWidget("Enter Text:",
        xmLabelGadgetClass, rowcol, NULL);
    text_w = XtVaCreateManagedWidget("text_w",
        xmTextWidgetClass, rowcol, NULL);

    XtAddCallback(text_w, XmNmodifyVerifyCallback, allcaps, NULL);

    XtManageChild(rowcol);
    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}

void
allcaps(text_w, unused, cbs)
Widget      text_w;
XtPointer   unused;
XmTextVerifyCallbackStruct *cbs;
{
    int len;

    if (cbs->text->ptr == NULL)  /* backspace */
        return;

    /* convert all input to upper-case if necessary */
    for (len = 0; len < cbs->text->length; len++)
        if (islower(cbs->text->ptr[len]))
            cbs->text->ptr[len] = toupper(cbs->text->ptr[len]);
}
