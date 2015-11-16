/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* prompt_phone.c -- a complex problem for XmNmodifyVerifyCallback.
 * prompt for a phone number by filtering digits only from input.
 * Don't allow paste operations and handle backspacing.
 */
#include <Xm/Text.h>
#include <Xm/LabelG.h>
#include <Xm/RowColumn.h>
#include <ctype.h>

void check_phone();

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

    XtVaCreateManagedWidget("Phone Number:",
        xmLabelGadgetClass, rowcol, NULL);
    text_w = XtVaCreateManagedWidget("text_w",
        xmTextWidgetClass, rowcol, NULL);

    XtAddCallback(text_w, XmNmodifyVerifyCallback, check_phone, NULL);
    XtAddCallback(text_w, XmNvalueChangedCallback, check_phone, NULL);

    XtManageChild(rowcol);
    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}

void
check_phone(text_w, unused, cbs)
Widget     text_w;
XtPointer  unused;
XmTextVerifyCallbackStruct *cbs;
{
    char c;
    int len = XmTextGetLastPosition(text_w);

    if (cbs->reason == XmCR_VALUE_CHANGED) {
        XmTextSetInsertionPosition(text_w, len);
        return;
    }

    /* no backspacing or typing in the middle of string */
    if (cbs->currInsert < len) {
        cbs->doit = False;
        return;
    }

    if (cbs->text->ptr == NULL) { /* backspace */
        if (cbs->startPos == 3 || cbs->startPos == 7)
            cbs->startPos--; /* delete the hyphen too */
        return;
    }

    if (cbs->text->length > 1) { /* don't allow clipboard copies */
        cbs->doit = False;
        return;
    }

    /* don't allow non-digits or let the input exceed 12 chars */
    if (!isdigit(c = cbs->text->ptr[0]) || len >= 12)
        cbs->doit = False;
    else if (len == 2 || len == 6) {
        cbs->text->ptr = XtRealloc(cbs->text->ptr, 2);
        cbs->text->length = 2;
        cbs->text->ptr[0] = c;
        cbs->text->ptr[1] = '-';
    }
}
