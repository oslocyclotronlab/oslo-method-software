/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* file_sel.c --a single pushbutton, when selected, creates a
 * file selection dialog that displays a list of all the writable
 * files in the directory described by the XmNmask of the dialog.
 * This program demonstrates how to use the XmNfileSearchProc for
 * file selection dialog widgets.
 */
#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/FileSB.h>
#include <Xm/DialogS.h>
#include <Xm/PushBG.h>
#include <Xm/PushB.h>
#include <sys/stat.h> /* may also have to include X11/Xos.h */

void do_search(), new_file_cb();
XmStringCharSet charset = XmSTRING_DEFAULT_CHARSET;

/* routined to determine if a file is accessible, a directory,
 * or writable.  Return -1 on all errors or if the file is not
 * writable.  Return 0 if it's a directory or 1 if it's a plain
 * writable file.
 */
int
is_writable(file)
char *file;
{
    struct stat s_buf;

    /* if file can't be accessed (via stat()) return. */
    if (stat(file, &s_buf) == -1)
        return -1;
    else if ((s_buf.st_mode & S_IFMT) == S_IFDIR)
        return 0; /* a directory */
    else if (!(s_buf.st_mode & S_IFREG) || access(file, W_OK) == -1)
        /* not a normal file or it is not writable */
        return -1;
    /* legitimate file */
    return 1;
}

/* main() -- create a FileSelectionDialog */
main(argc, argv)
int argc;
char *argv[];
{
    Widget toplevel, dialog;
    XtAppContext app;
    extern void exit();
    Arg args[1];

    toplevel = XtVaAppInitialize(&app, "Demos",
        NULL, 0, &argc, argv, NULL, NULL);

    XtSetArg(args[0], XmNfileSearchProc, do_search);
    dialog = XmCreateFileSelectionDialog(toplevel, "Files", args, 1);
    XtSetSensitive(
        XmFileSelectionBoxGetChild(dialog,
            XmDIALOG_HELP_BUTTON), False);
    /* if user presses Ok button, call new_file_cb() */
    XtAddCallback(dialog, XmNokCallback, new_file_cb, NULL);
    /* if user presses Cancel button, exit program */
    XtAddCallback(dialog, XmNcancelCallback, exit, NULL);

    XtManageChild(dialog);

    /* XtRealizeWidget(toplevel); */
    XtAppMainLoop(app);
}

/* a new file was selected -- check to see if it's readable and not
 * a directory.  If it's not readable, report an error.  If it's a
 * directory, scan it just as tho the user had typed it in the mask
 * Text field and selected "Search".
 */
void
new_file_cb(w, client_data, cbs)
Widget w;
XmFileSelectionBoxCallbackStruct *cbs;
{
    char *file;

    /* get the string typed in the text field in char * format */
    if (!XmStringGetLtoR(cbs->value, charset, &file))
        return;
    if (*file != '/') {
        /* if it's not a directory, determine the full pathname
         * of the selection by concatenating it to the "dir" part
         */
        char *dir, *newfile;
        if (XmStringGetLtoR(cbs->dir, charset, &dir)) {
            newfile = XtMalloc(strlen(dir) + 1 + strlen(file) + 1);
            sprintf(newfile, "%s/%s", dir, file);
            XtFree(file);
            XtFree(dir);
            file = newfile;
        }
    }
    switch (is_writable(file)) {
        case 1 :
            puts(file); /* or do anything you want */
            break;
        case 0 : {
            /* a directory was selected, scan it */
            XmString str = XmStringCreateSimple(file);
            XmFileSelectionDoSearch(w, str);
            XmStringFree(str);
            break;
        }
        case -1 :
            /* a system error on this file */
            perror(file);
    }
    XtFree(file);
}

/* do_search() -- scan a directory and report only those files that
 * are writable.  Here, we let the shell expand the (possible)
 * wildcards and return a directory listing by using popen().
 * A *real* application should -not- do this; it should use the
 * system's directory routines: opendir(), readdir() and closedir().
 */
void
do_search(fs, cbs)
Widget fs; /* file selection box widget */
XmFileSelectionBoxCallbackStruct *cbs;
{
    char          *mask, buf[BUFSIZ], *p;
    XmString       names[256]; /* maximum of 256 files in dir */
    int            i = 0;
    FILE          *pp, *popen();

    if (!XmStringGetLtoR(cbs->mask, charset, &mask))
        return; /* can't do anything */

    sprintf(buf, "/bin/ls %s", mask);
    XtFree(mask);
    /* let the shell read the directory and expand the filenames */
    if (!(pp = popen(buf, "r")))
        return;
    /* read output from popen() -- this will be the list of files */
    while (fgets(buf, sizeof buf, pp)) {
        if (p = index(buf, '\n'))
            *p = 0;
        /* only list files that are writable and not directories */
        if (is_writable(buf) == 1 &&
            (names[i] = XmStringCreateSimple(buf)))
            i++;
    }
    pclose(pp);
    if (i) {
        XtVaSetValues(fs,
            XmNfileListItems,      names,
            XmNfileListItemCount,  i,
            XmNdirSpec,            names[0],
            XmNlistUpdated,        True,
            NULL);
        while (i > 0)
            XmStringFree(names[--i]);
    } else
        XtVaSetValues(fs,
            XmNfileListItems,      NULL,
            XmNfileListItemCount,  0,
            XmNlistUpdated,        True,
            NULL);
}
