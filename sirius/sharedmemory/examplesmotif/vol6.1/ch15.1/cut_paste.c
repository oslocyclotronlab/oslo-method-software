/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* cut_paste.c -- demonstrate the XmText* functions that handle
 * clipboard operations.  These functions are convenience routines
 * that relieve the programmer of the need to use clipboard functions.
 * The functionality of these routines already exists in the Text
 * widget, yet it is common to place such features in the interface
 * via the MenuBar's "Edit" pulldown menu.
 */
#include <Xm/Text.h>
#include <Xm/LabelG.h>
#include <Xm/PushBG.h>
#include <Xm/RowColumn.h>
#include <Xm/MainW.h>

Widget text_w, text_output;

main(argc, argv)
int argc;
char *argv[];
{
    Widget        toplevel, main_w, menubar, rowcol_v, rowcol_h, pb;
    XtAppContext  app;
    int           i;
    void          cut_paste();
    XmString      label, cut, clear, copy, paste;
    Arg           args[5];

    toplevel = XtVaAppInitialize(&app, "Demos",
        NULL, 0, &argc, argv, NULL, NULL);

    main_w = XtVaCreateWidget("main_w",
        xmMainWindowWidgetClass, toplevel, NULL);

    /* Create a simple MenuBar that contains a single menu */
    label = XmStringCreateSimple("Edit");
    menubar = XmVaCreateSimpleMenuBar(main_w, "main_w",
        XmVaCASCADEBUTTON, label, 'E',
        NULL);
    XmStringFree(label);

    cut = XmStringCreateSimple("Cut");      /* create a simple    */
    copy = XmStringCreateSimple("Copy");    /* pulldown menu that */
    clear = XmStringCreateSimple("Clear");  /* has these menu     */
    paste = XmStringCreateSimple("Paste");  /* items in it.       */
    XmVaCreateSimplePulldownMenu(menubar, "edit_menu", 0, cut_paste,
        XmVaPUSHBUTTON, cut, 'C', NULL, NULL,
        XmVaPUSHBUTTON, copy, 'o', NULL, NULL,
        XmVaPUSHBUTTON, paste, 'P', NULL, NULL,
        XmVaSEPARATOR,
        XmVaPUSHBUTTON, clear, 'l', NULL, NULL,
        NULL);
    XmStringFree(cut);
    XmStringFree(clear);
    XmStringFree(copy);
    XmStringFree(paste);

    XtManageChild(menubar);

    /* create a standard vertical RowColumn... */
    rowcol_v = XtVaCreateWidget("rowcol_v",
        xmRowColumnWidgetClass, main_w, NULL);

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
    XtManageChild(main_w);

    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}

/* the callback routine for the items in the edit menu */
void
cut_paste(widget, num)
Widget widget;  /* the menu item (pushbutton) that was selected */
int num;        /* the menu item number */
{
    Boolean result = True;

    switch (num) {
        case 0 : result = XmTextCut(text_w, CurrentTime); break;
        case 1 : result = XmTextCopy(text_w, CurrentTime); break;
        case 2 : result = XmTextPaste(text_w);
        case 3 : XmTextClearSelection(text_w, CurrentTime); break;
    }
    if (result == False)
        XmTextSetString(text_output, "There is no selection.");
    else
        XmTextSetString(text_output, NULL);
}
