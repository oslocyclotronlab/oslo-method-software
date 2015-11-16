/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* The following program is incomplete.  It displays a List widget that
 * contains all the fonts available on the server, a CheckBox that sets
 * a text style (normal, bold, italic), and a ToggleBox that sets text
 * color.  However, there are no callback routines associated with any
 * of the widgets, so functionally, the program does nothing.  Test your
 * understanding of the Motif toolkit by doing the following exercises:
 *
 * In order of difficulty/complexity:
 *
 *  1. Provide a default value for the visible number of items in the
 *     List widget (other than 1).
 *  2. Add a TextField widget in between the Style and the Colors
 *     selections and render a string using the selected font and color.
 *     (Hint: use XmNforeground to set the color of the Text widget when
 *      a new color is selected.)
 *  3. Constrain the list of fonts to those in the "Style" selection.
 *     (Hint: determine which of the toggles are selected using either a
 *      callback routine on the toggles themselves, or using the RowColumn's
 *      XmNentryCallback and brush up on XListFonts().)
 *  4. Implement 1 using a DrawingArea.
 *     (Hint: you'll have to create a GC and set it's foreground color.)
 *  5. Implement all of the above without adding additional global variables.
 */
#include <Xm/LabelG.h>
#include <Xm/RowColumn.h>
#include <Xm/ToggleBG.h>
#include <Xm/Form.h>
#include <Xm/List.h>
#include <Xm/Frame.h>

String colors[] = {
    "red", "green", "blue", "orange", "violet", "black", "white"
};

String text_styles[] = {
    "Normal", "Bold", "Italic",
};

main(argc, argv)
int argc;
char *argv[];
{
    Widget toplevel, radio_box, rowcol, form, frame, w;
    XtAppContext app;
    extern char **XListFonts();
    char **list;
    int i, j;

    toplevel = XtVaAppInitialize(&app, "Demos",
        NULL, 0, &argc, argv, NULL, NULL);

    form = XtVaCreateWidget("form",
        xmFormWidgetClass, toplevel,
        XmNhorizontalSpacing, 5,
        XmNverticalSpacing,   5,
        NULL);

    w = XtVaCreateManagedWidget("Text Styles:",
        xmLabelGadgetClass, form,
        XmNleftAttachment,  XmATTACH_FORM,
        XmNtopAttachment,   XmATTACH_FORM,
        NULL);
    frame = XtVaCreateManagedWidget("frame1",
        xmFrameWidgetClass, form,
        XmNtopAttachment,   XmATTACH_WIDGET,
        XmNtopWidget,       w,
        XmNleftAttachment,  XmATTACH_FORM,
        NULL);
    rowcol = XtVaCreateWidget("rowcol1",
        xmRowColumnWidgetClass, frame, NULL);
    for (i = 0; i < XtNumber(text_styles); i++)
        XtVaCreateManagedWidget(text_styles[i],
            xmToggleButtonGadgetClass, rowcol, NULL);
    XtManageChild(rowcol);

    rowcol = XtVaCreateWidget("rowcol2",
        xmRowColumnWidgetClass, form,
        XmNtopAttachment,   XmATTACH_FORM,
        XmNrightAttachment, XmATTACH_FORM,
        NULL);
    XtVaCreateManagedWidget("Text Color:",
        xmLabelGadgetClass, rowcol, NULL);
    frame = XtVaCreateManagedWidget("frame2",
        xmFrameWidgetClass, rowcol, NULL);
    radio_box = XmCreateRadioBox(frame, "radio_box", NULL, 0);
    XtVaSetValues(radio_box,
        XmNnumColumns,  3,
        XmNpacking,     XmPACK_COLUMN,
        NULL);
    for (i = 0; i < XtNumber(colors); i++)
        XtVaCreateManagedWidget(colors[i],
            xmToggleButtonGadgetClass, radio_box,
            XmNset, i == 0,
            NULL);
    XtManageChild(radio_box);
    XtManageChild(rowcol);

    w = XmCreateScrolledList(form, "scrolled_list", NULL, 0);
    XtVaSetValues(XtParent(w),
        XmNleftOffset,       5,
        XmNtopOffset,        5,
        XmNtopAttachment,    XmATTACH_WIDGET,
        XmNtopWidget,        rowcol,
        XmNleftAttachment,   XmATTACH_FORM,
        XmNrightAttachment,  XmATTACH_FORM,
        XmNbottomAttachment, XmATTACH_FORM,
        NULL);
    list = XListFonts(XtDisplay(w), "*", 32767, &j);
    for (i = 0; i < j; i++) {
        XmString str = XmStringCreateSimple(list[i]);
        XmListAddItem(w, str, i);
        XmStringFree(str);
    }
    XtManageChild(w);
    XFreeFontNames(list);

    XtManageChild(form);
    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}
