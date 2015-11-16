/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* passwd.c -- prompt for a passwd.  Meaning, all input looks like
 * a series of *'s.  Store the actual data typed by the user in
 * an internal variable.  Don't allow paste operations.  Handle
 * backspacing by deleting all text from insertion point to the
 * end of text.
 */
#include <Xm/Text.h>
#include <Xm/LabelG.h>
#include <Xm/RowColumn.h>
#include <ctype.h>

void check_passwd();
char *passwd; /* store user-typed passwd here. */

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

    XtVaCreateManagedWidget("Password:",
        xmLabelGadgetClass, rowcol, NULL);
    text_w = XtVaCreateManagedWidget("text_w",
        xmTextWidgetClass, rowcol, NULL);

    XtAddCallback(text_w, XmNmodifyVerifyCallback, check_passwd, NULL);
    XtAddCallback(text_w, XmNactivateCallback, check_passwd, NULL);

    XtManageChild(rowcol);
    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}

void
check_passwd(text_w, unused, cbs)
Widget        text_w;
XtPointer     unused;
XmTextVerifyCallbackStruct *cbs;
{
    char *new;
    int len;

    if (cbs->reason == XmCR_ACTIVATE) {
        printf("Password: %s\n", passwd);
        return;
    }

    if (cbs->text->ptr == NULL) { /* backspace */
        cbs->endPos = strlen(passwd); /* delete from here to end */
        passwd[cbs->startPos] = 0; /* backspace--terminate */
        return;
    }

    if (cbs->text->length > 1) {
        cbs->doit = False; /* don't allow "paste" operations */
        return; /* make the user *type* the password! */
    }

    new = XtMalloc(cbs->endPos + 2); /* new char + NULL terminator */
    if (passwd) {
        strcpy(new, passwd);
        XtFree(passwd);
    } else
        new[0] = NULL;
    passwd = new;
    strncat(passwd, cbs->text->ptr, cbs->text->length);
    passwd[cbs->endPos + cbs->text->length] = 0;

    for (len = 0; len < cbs->text->length; len++)
        cbs->text->ptr[len] = '*';
}
