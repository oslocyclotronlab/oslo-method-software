/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* file_browser.c -- use a ScrolledText object to view the
 * contents of arbitrary files chosen by the user from a
 * FileSelectionDialog or from a single-line text widget.
 */
#include <X11/Xos.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/FileSB.h>
#include <Xm/MainW.h>
#include <Xm/RowColumn.h>
#include <Xm/LabelG.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>

main(argc, argv)
int argc;
char *argv[];
{
    Widget        top, main_w, menubar, menu, rc, text_w, file_w;
    XtAppContext  app;
    XmString      file, new, quit;
    extern void   read_file(), file_cb();
    Arg           args[4];

    /* initialize toolkit and create toplevel shell */
    top = XtVaAppInitialize(&app, "Demos",
        NULL, 0, &argc, argv, NULL, NULL);

    /* MainWindow for the application -- contains menubar
     * and ScrolledText/Prompt/TextField as WorkWindow.
     */
    main_w = XtVaCreateManagedWidget("main_w",
        xmMainWindowWidgetClass, top,
        /* XmNscrollingPolicy,   XmVARIABLE, */
        NULL);

    /* Create a simple MenuBar that contains one menu */
    file = XmStringCreateSimple("File");
    menubar = XmVaCreateSimpleMenuBar(main_w, "menubar",
        XmVaCASCADEBUTTON, file, 'F',
        NULL);
    XmStringFree(file);

    /* Menu is "File" -- callback is file_cb() */
    new = XmStringCreateSimple("New ...");
    quit = XmStringCreateSimple("Quit");
    menu = XmVaCreateSimplePulldownMenu(menubar, "file_menu", 0, file_cb,
        XmVaPUSHBUTTON, new, 'N', NULL, NULL,
        XmVaSEPARATOR,
        XmVaPUSHBUTTON, quit, 'Q', NULL, NULL,
        NULL);
    XmStringFree(new);
    XmStringFree(quit);

    /* Menubar is done -- manage it */
    XtManageChild(menubar);

    rc = XtVaCreateWidget("work_area", xmRowColumnWidgetClass, main_w, NULL);
    XtVaCreateManagedWidget("Filename:", xmLabelGadgetClass, rc,
        XmNalignment, XmALIGNMENT_BEGINNING,
        NULL);
    file_w = XtVaCreateManagedWidget("text_field",
        xmTextFieldWidgetClass, rc, NULL);

    /* Create ScrolledText -- this is work area for the MainWindow */
    XtSetArg(args[0], XmNrows,      12);
    XtSetArg(args[1], XmNcolumns,   70);
    XtSetArg(args[2], XmNeditable,  False);
    XtSetArg(args[3], XmNeditMode,  XmMULTI_LINE_EDIT);
    text_w = XmCreateScrolledText(rc, "text_w", args, 4);
    XtManageChild(text_w);

    /* store text_w as user data in "File" menu for file_cb() callback */
    XtVaSetValues(menu, XmNuserData, text_w, NULL);
    /* add callback for TextField widget passing "text_w" as client data */
    XtAddCallback(file_w, XmNactivateCallback, read_file, text_w);

    XtManageChild(rc);

    /* Store the filename text widget to ScrolledText object */
    XtVaSetValues(text_w, XmNuserData, file_w, NULL);

    XmMainWindowSetAreas(main_w, menubar, NULL, NULL, NULL, rc);
    XtRealizeWidget(top);
    XtAppMainLoop(app);
}

/* The "File" menu item was selected; popup a FileSelectionDialog. */
void
file_cb(menu_item, item_no)
Widget menu_item;
int item_no; /* the index into the menu that menu_item is */
{
    static Widget dialog;
    Widget text_w;
    extern void read_file();

    if (item_no == 1)
        exit(0);  /* user chose Quit */

    if (!dialog) {
        Widget menu = XtParent(menu_item);
        dialog = XmCreateFileSelectionDialog(menu, "file_sb", NULL, 0);

        /* Get the text widget handle stored as "user data" in File menu */
        XtVaGetValues(menu, XmNuserData, &text_w, NULL);
        XtAddCallback(dialog, XmNokCallback, read_file, text_w);
        XtAddCallback(dialog, XmNcancelCallback, XtUnmanageChild, NULL);
    }
    XtManageChild(dialog);

    XtPopup(XtParent(dialog), XtGrabNone);
    /* If dialog is already popped up, XtPopup does nothing.
     * Call XMapRaised() anyway to make sure it's visible.
     */
    XMapRaised(XtDisplay(dialog), XtWindow(XtParent(dialog)));
}

/* callback routine when the user selects Ok in the FileSelection
 * Dialog or presses Return in the single-line text widget.
 * The specified file must be a regular file and readable.
 * If so, it's contents are displayed in the text_w provided as the
 * client_data to this function.
 */
void
read_file(widget, text_w, cbs)
Widget widget;  /* file selection box or text field widget */
Widget text_w; /* passed the text_w as client_data */
XmFileSelectionBoxCallbackStruct *cbs;
{
    char *filename, *text;
    struct stat statb;
    FILE *fp;
    Widget file_w;

    if (XtIsSubclass(widget, xmTextFieldWidgetClass)) {
        filename = XmTextFieldGetString(widget);
        file_w = widget; /* this *is* the file_w */
    } else {
        /* file was selected from FileSelectionDialog */
        XmStringGetLtoR(cbs->value, XmSTRING_DEFAULT_CHARSET, &filename);
        /* the user data stored the file_w widget in the text_w */
        XtVaGetValues(text_w, XmNuserData, &file_w, NULL);
    }

    if (!filename || !*filename) { /* nothing typed? */
        if (filename)
            XtFree(filename);
        return;
    }

    /* make sure the file is a regular text file and open it */
    if (stat(filename, &statb) == -1 ||
            (statb.st_mode & S_IFMT) != S_IFREG ||
            !(fp = fopen(filename, "r"))) {
        if ((statb.st_mode & S_IFMT) == S_IFREG)
            perror(filename); /* send to stderr why we can't read it */
        else
            fprintf(stderr, "%s: not a regular file\n", filename);
        XtFree(filename);
        return;
    }

    /* put the contents of the file in the Text widget by allocating
     * enough space for the entire file, reading the file into the
     * allocated space, and using XmTextFieldSetString() to show the file.
     */
    if (!(text = XtMalloc((unsigned)(statb.st_size+1)))) {
        fprintf(stderr, "Can't alloc enough space for %s", filename);
        XtFree(filename);
        fclose(fp);
        return;
    }

    if (!fread(text, sizeof(char), statb.st_size+1, fp))
        fprintf(stderr, "Warning: may not have read entire file!\n");

    text[statb.st_size] = 0; /* be sure to NULL-terminate */

    /* insert file contents in Text widget */
    XmTextSetString(text_w, text);

    /* make sure text field is up to date */
    if (file_w != widget) {
        /* only necessary if activated from FileSelectionDialog */
        XmTextFieldSetString(file_w, filename);
        XmTextFieldSetCursorPosition(file_w, strlen(filename));
    }

    /* free all allocated space and we're outta here. */
    XtFree(text);
    XtFree(filename);
    fclose(fp);
}
