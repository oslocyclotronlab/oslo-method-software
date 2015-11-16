/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* spreadsheet.c -- This demo shows how the most basic use of
 * the RowColumn widget.  It displays a table of widgets in a
 * row-column format similar to a spreadsheet.  This is accomplished
 * by setting the number ROWS and COLS and setting the appropriate
 * resources correctly.
 */
#include <Xm/LabelG.h>
#include <Xm/PushBG.h>
#include <Xm/RowColumn.h>

#define ROWS  8
#define COLS 10

main(argc, argv)
int argc;
char *argv[];
{
    Widget toplevel, parent;
    XtAppContext app;
    char buf[16];
    int i, j;

    toplevel = XtVaAppInitialize(&app, "Demos", NULL, 0,
        &argc, argv, NULL, NULL);

    parent = XtVaCreateManagedWidget("rowcolumn",
        xmRowColumnWidgetClass, toplevel,
        XmNpacking,     XmPACK_COLUMN,
        XmNnumColumns,  COLS,
        XmNorientation, XmVERTICAL,
        NULL);

    /* simply loop thru the strings creating a widget for each one */
    for (i = 0; i < COLS; i++)
        for (j = 0; j < ROWS; j++) {
            sprintf(buf, "%d-%d", i+1, j+1);
            if (i == 0 || j == 0)
                XtVaCreateManagedWidget(buf,
                    xmLabelGadgetClass, parent, NULL);
            else
                XtVaCreateManagedWidget("",
                    xmPushButtonGadgetClass, parent, NULL);
        }

    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}
