/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* modify_text.c -- demonstrate the XmNmodifyVerifyCallback for
 * Text widgets.  Display several different type-in fields prompting
 * for name, phone, state, zip and a password.  Each widget has a
 * separate modify-callback associated with it to check that the
 * right type of input is being given.  zipcode requires digits
 * only, but the phone number also inserts -'s at the right place.
 * The state converts everything to upper case and the password only
 * output *'s.
 */
#include <Xm/TextF.h>
#include <Xm/Text.h>
#include <Xm/LabelG.h>
#include <Xm/Form.h>
#include <Xm/RowColumn.h>
#include <ctype.h>

void check_phone(), allcaps(), check_zip(),
    check_passwd(), value_changed();
char *passwd;
char *labels[] = { "Name:", "Phone:", "State:", "ZIP:", "Password:" };

typedef void (*VoidProc)();
VoidProc modify_funcs[] = {
    NULL, check_phone, allcaps, check_zip, check_passwd
};

main(argc, argv)
int argc;
char *argv[];
{
    Widget        toplevel, text_w, form, rowcol;
    XtAppContext  app;
    int           i;
    void          print_result();

    toplevel = XtVaAppInitialize(&app, "Demos",
        NULL, 0, &argc, argv, NULL, NULL);

    rowcol = XtVaCreateWidget("rowcol",
        xmRowColumnWidgetClass, toplevel, NULL);

    for (i = 0; i < XtNumber(labels); i++) {
        form = XtVaCreateWidget("form", xmFormWidgetClass, rowcol,
            XmNfractionBase,  10,
            NULL);
        XtVaCreateManagedWidget(labels[i],
            xmLabelGadgetClass, form,
            XmNtopAttachment,    XmATTACH_FORM,
            XmNbottomAttachment, XmATTACH_FORM,
            XmNleftAttachment,   XmATTACH_FORM,
            XmNrightAttachment,  XmATTACH_POSITION,
            XmNrightPosition,    3,
            XmNalignment,        XmALIGNMENT_END,
            NULL);
        text_w = XtVaCreateManagedWidget("text_w",
            xmTextWidgetClass, form,
            XmNtraversalOn,      True,
            XmNrightAttachment,  XmATTACH_FORM,
            XmNleftAttachment,   XmATTACH_POSITION,
            XmNleftPosition,     4,
            NULL);

        if (modify_funcs[i]) {
            /* When user types, monitor input */
            XtAddCallback(text_w, XmNactivateCallback,
                modify_funcs[i], labels[i]);
            XtAddCallback(text_w, XmNmodifyVerifyCallback,
                modify_funcs[i], labels[i]);
            XtAddCallback(text_w, XmNvalueChangedCallback,
                value_changed, NULL);
        } else
            XtAddCallback(text_w, XmNactivateCallback,
                XmProcessTraversal, XmTRAVERSE_NEXT_TAB_GROUP);
        XtManageChild(form);
    }
    XtManageChild(rowcol);

    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}

void
value_changed(text_w)
Widget text_w;
{
    XmTextSetInsertionPosition(text_w, XmTextGetLastPosition(text_w));
}

void
check_phone(text_w, label, cbs)
Widget text_w;
char *label;
XmTextVerifyCallbackStruct *cbs;
{
    char c;
    int len = XmTextGetLastPosition(text_w);

    if (cbs->reason == XmCR_ACTIVATE) {
        if (len == 12) {
            char *string = XmTextGetString(text_w);
            printf("%s %s\n", label, string);
            XtFree(string);
            XmProcessTraversal(text_w, XmTRAVERSE_NEXT_TAB_GROUP);
        }
        return;
    }

    /* no backspacing, typing or stuff in the middle of string */
    if (cbs->currInsert < len) {
        cbs->doit = False;
        return;
    }

    if (cbs->text->ptr == NULL) { /* backspace */
        if (cbs->startPos == 3 || cbs->startPos == 7)
            cbs->startPos--;
        return;
    }

    if (cbs->text->length > 1) { /* don't allow clipboard pastes */
        cbs->doit = False;
        return;
    }
    if (!isdigit(c = cbs->text->ptr[0]) || len >= 12)
        cbs->doit = False;
    else if (len == 2 || len == 6) {
        cbs->text->ptr = XtRealloc(cbs->text->ptr, 2);
        cbs->text->length = 2;
        cbs->text->ptr[0] = c;
        cbs->text->ptr[1] = '-';
    }
}

void
allcaps(text_w, label, cbs)
Widget text_w;
char *label;
XmTextVerifyCallbackStruct *cbs;
{
    int len;

    if (cbs->reason == XmCR_ACTIVATE) {
        XmProcessTraversal(text_w, XmTRAVERSE_NEXT_TAB_GROUP);
        return;
    }

    if (cbs->startPos < cbs->currInsert) /* backspace */
        return;

    for (len = 0; len < cbs->text->length; len++)
        if (islower(cbs->text->ptr[len]))
            cbs->text->ptr[len] = toupper(cbs->text->ptr[len]);
}

void
check_zip(text_w, label, cbs)
Widget text_w;
char *label;
XmTextVerifyCallbackStruct *cbs;
{
    int len = XmTextGetLastPosition(text_w);

    if (cbs->reason == XmCR_ACTIVATE) {
        if (len == 5) {
            char *string = XmTextGetString(text_w);
            printf("%s %s\n", label, string);
            XtFree(string);
            XmProcessTraversal(text_w, XmTRAVERSE_NEXT_TAB_GROUP);
        }
        return;
    }

    if (cbs->startPos < cbs->currInsert) /* backspace */
        return;

    if (len == 5) {
        cbs->doit = False;
        return;
    }
    /* check that the new additions won't put us over 5 */
    if (len + cbs->text->length > 5) {
        cbs->text->ptr[5 - len] = 0;
        cbs->text->length = strlen(cbs->text->ptr);
    }
    for (len = 0; len < cbs->text->length; len++) {
        /* make sure all additions are digits. */
        if (!isdigit(cbs->text->ptr[len])) {
            /* not a digit-- move all chars down one and
             * decrement cbs->text->length.
             */
            int i;
            for (i = len; (i+1) < cbs->text->length; i++)
                cbs->text->ptr[i] = cbs->text->ptr[i+1];
            cbs->text->length--;
            len--;
        }
    }
    if (cbs->text->length == 0)
        cbs->doit = False;
}

void
check_passwd(text_w, label, cbs)
Widget text_w;
char *label;
XmTextVerifyCallbackStruct *cbs;
{
    char *new;
    int len;

    if (cbs->reason == XmCR_ACTIVATE) {
        printf("%s %s\n", label, passwd);
        XmProcessTraversal(text_w, XmTRAVERSE_NEXT_TAB_GROUP);
        return;
    }
    if (cbs->text->ptr == NULL) { /* backspace */
        cbs->endPos = strlen(passwd); /* delete from here to end */
        passwd[cbs->startPos] = 0;
        return;
    }
    len = XmTextGetLastPosition(text_w); /* total length of text */
    if (len > cbs->endPos) {
        cbs->doit = False; /* can only append new text. not insert */
        return;
    }
    new = XtMalloc(len + cbs->text->length + 1);
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
