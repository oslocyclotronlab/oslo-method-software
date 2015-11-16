/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* select_text.c -- demonstrate how to position a cursor at a
 * particular location.  The position is determined by a search_pat-
 * match search.
 */
#include <Xm/Text.h>
#include <Xm/LabelG.h>
#include <Xm/RowColumn.h>
#include <X11/Xos.h>   /* for the index() function */

Widget text_w, search_w, text_output;

main(argc, argv)
int argc;
char *argv[];
{
    Widget        toplevel, rowcol_v, rowcol_h;
    XtAppContext  app;
    int           i;
    void          search_text();
    Arg           args[5];

    toplevel = XtVaAppInitialize(&app, "Demos",
        NULL, 0, &argc, argv, NULL, NULL);

    rowcol_v = XtVaCreateWidget("rowcol_v",
        xmRowColumnWidgetClass, toplevel, NULL);

    rowcol_h = XtVaCreateWidget("rowcol_h",
        xmRowColumnWidgetClass, rowcol_v,
        XmNorientation,  XmHORIZONTAL,
        NULL);
    XtVaCreateManagedWidget("Search Pattern:",
        xmLabelGadgetClass, rowcol_h, NULL);
    search_w = XtVaCreateManagedWidget("search_text",
        xmTextWidgetClass, rowcol_h, NULL);
    XtManageChild(rowcol_h);

    text_output = XtVaCreateManagedWidget("text_out",
        xmTextWidgetClass, rowcol_v,
        XmNeditable,              False,
        XmNcursorPositionVisible, False,
        XmNshadowThickness,       0,
        XmNsensitive,             False,
        NULL);

    XtSetArg(args[0], XmNrows,      10);
    XtSetArg(args[1], XmNcolumns,   80);
    XtSetArg(args[2], XmNeditMode,  XmMULTI_LINE_EDIT);
    XtSetArg(args[3], XmNscrollHorizontal,  False);
    XtSetArg(args[4], XmNwordWrap,  True);
    text_w = XmCreateScrolledText(rowcol_v, "text_w", args, 5);
    XtManageChild(text_w);

    XtAddCallback(search_w, XmNactivateCallback, search_text, NULL);

    XtManageChild(rowcol_v);

    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}

void
search_text()
{
    char *search_pat, *p, *string, buf[32];
    XmTextPosition pos;
    int len;
    Boolean found = False;

    string = XmTextGetString(text_w);
    if (!*string) {
        XmTextSetString(text_output, "No text to search.");
        XtFree(string);
        return;
    }

    search_pat = XmTextGetString(search_w);
    if (!*search_pat) {
        XmTextSetString(text_output, "Specify a search pattern.");
        XtFree(string);
        XtFree(search_pat);
        return;
    }
    len = strlen(search_pat);
    /* start searching at current cursor position + 1 */
    pos = XmTextGetCursorPosition(text_w);
    for (p = &string[pos+1]; p = index(p, *search_pat); p++)
        if (!strncmp(p, search_pat, len)) {
            found = True;
            break;
        }
    if (!found) { /* didn't find pattern? */
        /* search from beginning till we've passed "pos" */
        for (p = string; p = index(p, *search_pat); p++)
            if (p - string > pos || !strncmp(p, search_pat, len)) {
                found = True;
                break;
            }
    }
    if (!found)
        XmTextSetString(text_output, "Pattern not found.");
    else {
        pos = (XmTextPosition)(p - string);
        sprintf(buf, "Pattern found at position %ld.", pos);
        XmTextSetString(text_output, buf);
        XmTextSetInsertionPosition(text_w, pos);
	XmTextSetHighlight(text_w, pos, pos + len, XmHIGHLIGHT_SELECTED);
    }
}
