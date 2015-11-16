/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* text_box.c -- demonstrate simple use of XmNactivateCallback
 * for Text widgets.  Create a rowcolumn that has rows of Form
 * widgets, each containing a Label and a Text widget.  When
 * the user presses Return, print the value of the text widget
 * and move the focus to the next text widget.
 */
#include <Xm/TextF.h>
#include <Xm/LabelG.h>
#include <Xm/Form.h>
#include <Xm/RowColumn.h>

char *labels[] = { "Name:", "Address:", "City:", "State:", "Zip:" };

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
            xmTextFieldWidgetClass, form,
            XmNtraversalOn,      True,
            XmNrightAttachment,  XmATTACH_FORM,
            XmNleftAttachment,   XmATTACH_POSITION,
            XmNleftPosition,     4,
            NULL);

        /* When user hits return, print the label+value of text_w */
        XtAddCallback(text_w, XmNactivateCallback,
            print_result, labels[i]);

        /* Also advance focus to next Text widget, which is in the
         * next Tab Group because each Text widget is in a Form by
         * itself.  If there were all in the same manager, we'd just
         * use XmTRAVERSE_NEXT instead.
         */
        XtAddCallback(text_w, XmNactivateCallback,
            XmProcessTraversal, XmTRAVERSE_NEXT_TAB_GROUP);
        XtManageChild(form);
    }
    XtManageChild(rowcol);

    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}

/* callback for when the user his return in the Text widget */
void
print_result(text_w, label)
Widget text_w;
char  *label;
{
    char *value = XmTextFieldGetString(text_w);

    printf("%s %s\n", label, value);
    XtFree(value);
}
