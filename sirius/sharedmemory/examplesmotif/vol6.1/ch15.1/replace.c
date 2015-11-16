/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* replace.c -- demonstrate how to search and replace text. */
#include <Xm/Text.h>
#include <Xm/LabelG.h>
#include <Xm/PushBG.h>
#include <Xm/RowColumn.h>
#include <X11/Xos.h>   /* for the index() function */

Widget text_w, search_w, replace_w, text_output;

main(argc, argv)
int argc;
char *argv[];
{
    Widget        toplevel, rowcol_v, rowcol_h, pb;
    XtAppContext  app;
    int           i;
    void          search_and_replace();
    Arg           args[5];

    toplevel = XtVaAppInitialize(&app, "Demos",
        NULL, 0, &argc, argv, NULL, NULL);

    /* create a standard vertical RowColumn... */
    rowcol_v = XtVaCreateWidget("rowcol_v",
        xmRowColumnWidgetClass, toplevel, NULL);

    /* create horizontal RowColumn inside the vertical one... */
    rowcol_h = XtVaCreateWidget("rowcol_h",
        xmRowColumnWidgetClass, rowcol_v,
        XmNorientation,  XmHORIZONTAL,
        NULL);
    /* Create two Text widgets with Labels... */
    XtVaCreateManagedWidget("Search Pattern:",
        xmLabelGadgetClass, rowcol_h, NULL);
    search_w = XtVaCreateManagedWidget("search_text",
        xmTextWidgetClass, rowcol_h, NULL);
    XtVaCreateManagedWidget("Replace Pattern:",
        xmLabelGadgetClass, rowcol_h, NULL);
    replace_w = XtVaCreateManagedWidget("replace_text",
        xmTextWidgetClass, rowcol_h, NULL);
    /* ...and a pushbutton to activate it all */
    pb = XtVaCreateManagedWidget("Ok",
        xmPushButtonGadgetClass, rowcol_h, NULL);
    XtAddCallback(pb, XmNactivateCallback, search_and_replace, NULL);

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

    XtManageChild(rowcol_v);

    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}

void
search_and_replace()
{
    char *search_pat, *p, *string, *new_pat, buf[32];
    XmTextPosition pos;
    int search_len, pattern_len;
    int nfound = 0;

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

    new_pat = XmTextGetString(replace_w);
    search_len = strlen(search_pat);
    pattern_len = strlen(new_pat);
    /* start at beginning and search entire Text widget */
    for (p = string; p = index(p, *search_pat); p++)
        if (!strncmp(p, search_pat, search_len)) {
            nfound++;
            /* get the position where pattern was found */
            pos = (XmTextPosition)(p-string);
            /* replace the text from our position + strlen(new_pat) */
            XmTextReplace(text_w, pos, pos + search_len, new_pat);
            /* "string" has changed -- we must get the new version */
            XtVaGetValues(text_w, XmNvalue, &string, NULL);
            /* continue search for next pattern -after- replacement */
            p = &string[pos + pattern_len];
        }
    if (!nfound)
        strcpy(buf, "Pattern not found.");
    else
        sprintf(buf, "Made %d replacements.", nfound);
    XmTextSetString(text_output, buf);
    XtFree(string);
    XtFree(search_pat);
    XtFree(new_pat);
}
