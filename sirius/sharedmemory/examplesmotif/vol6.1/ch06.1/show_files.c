/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* file.c -- introduce FileSelectionDialog; print the file
 * selected by the user.
 */
#include <Xm/FileSB.h>

#define charset XmSTRING_DEFAULT_CHARSET

main(argc, argv)
int argc;
char *argv[];
{
    Widget        toplevel, text_w, dialog;
    XtAppContext  app;
    extern void   exit(), echo_file();

    toplevel = XtVaAppInitialize(&app, "Demos",
        NULL, 0, &argc, argv, NULL, NULL);

    /* Create a simple FileSelectionDialog -- no frills */
    dialog = XmCreateFileSelectionDialog(toplevel, "filesb", NULL, 0);
    XtAddCallback(dialog, XmNcancelCallback, exit, NULL);
    XtAddCallback(dialog, XmNokCallback, echo_file, NULL);
    XtManageChild(dialog);

    XtAppMainLoop(app);
}

/* callback routine when the user selects Ok in the FileSelection
 * Dialog.  Just print the file name selected.
 */
void
echo_file(fs, client_data, cbs)
Widget fs;  /* file selection box */
XtPointer client_data;
XmFileSelectionBoxCallbackStruct *cbs;
{
    char *filename;

    if (!XmStringGetLtoR(cbs->value, charset, &filename))
        return; /* must have been an internal error */

    if (!*filename) { /* nothing typed? */
        puts("No file selected.");
        XtFree(filename); /* even "" is an allocated byte */
        return;
    }

    printf("Filename given: \"%s\"\n", filename);
    XtFree(filename);
}
