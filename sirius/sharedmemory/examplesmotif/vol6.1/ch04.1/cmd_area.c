/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* cmd_area.c -- use a ScrolledText object to view the
 * putput of commands input by the user in a Command window.
 */
#include <Xm/Text.h>
#include <Xm/MainW.h>
#include <Xm/Command.h>
#include <stdio.h>         /* For popen() */

/* main() -- initialize toolkit, create a main window, menubar,
 * a Command Area and a ScrolledText to view the output of commands.
 */
main(argc, argv)
int argc;
char *argv[];
{
    Widget        top, main_w, menubar, menu, command_w, text_w;
    XtAppContext  app;
    XmString      file, quit;
    extern void   exec_cmd(), exit();
    Arg           args[4];

    /* initialize toolkit and create toplevel shell */
    top = XtVaAppInitialize(&app, "Demos",
        NULL, 0, &argc, argv, NULL, NULL);

    (void) close(0); /* don't let commands read from stdin */

    /* MainWindow for the application -- contains menubar, ScrolledText
     * and CommandArea (which prompts for filename).
     */
    main_w = XtVaCreateManagedWidget("main_w",
        xmMainWindowWidgetClass, top,
        NULL);

    /* Create a simple MenuBar that contains one menu */
    file = XmStringCreateSimple("File");
    menubar = XmVaCreateSimpleMenuBar(main_w, "menubar",
        XmVaCASCADEBUTTON, file, 'F',
        NULL);
    XmStringFree(file);

    /* "File" menu has only one item (Quit), so make callback exit() */
    quit = XmStringCreateSimple("Quit");
    menu = XmVaCreateSimplePulldownMenu(menubar, "file_menu", 0, exit,
        XmVaPUSHBUTTON, quit, 'Q', NULL, NULL,
        NULL);
    XmStringFree(quit);

    /* Menubar is done -- manage it */
    XtManageChild(menubar);

    /* Create ScrolledText -- this is work area for the MainWindow */
    XtSetArg(args[0], XmNrows,      24);
    XtSetArg(args[1], XmNcolumns,   80);
    XtSetArg(args[2], XmNeditable,  False);
    XtSetArg(args[3], XmNeditMode,  XmMULTI_LINE_EDIT);
    text_w = XmCreateScrolledText(main_w, "text_w", args, 4);
    XtManageChild(text_w);

    /* store text_w as user data in "File" menu for file_cb() callback */
    XtVaSetValues(menu, XmNuserData, text_w, NULL);

    /* Create the command area -- this must be a Command class widget */
    file = XmStringCreateSimple("Command:");
    command_w = XtVaCreateWidget("command_w", xmCommandWidgetClass, main_w,
        XmNpromptString, file,
        NULL);
    XmStringFree(file);
    XtAddCallback(command_w, XmNcommandEnteredCallback, exec_cmd, text_w);
    XtManageChild(command_w);

    XmMainWindowSetAreas(main_w, menubar, command_w,
        NULL, NULL, XtParent(text_w));
    XtRealizeWidget(top);
    XtAppMainLoop(app);
}

/* execute the command and redirect output to the ScrolledText window */
void
exec_cmd(cmd_widget, text_w, cbs)
Widget cmd_widget;  /* the command widget itself, not its Text widget */
Widget text_w; /* passed the text_w as client_data */
XmCommandCallbackStruct *cbs;
{
    char *cmd, buf[BUFSIZ];
    XmTextPosition pos;
    FILE *pp, *popen();

    XmStringGetLtoR(cbs->value, XmSTRING_DEFAULT_CHARSET, &cmd);

    if (!cmd || !*cmd) { /* nothing typed? */
        if (cmd)
            XtFree(cmd);
        return;
    }

    /* make sure the file is a regular text file and open it */
    if (!(pp = popen(cmd, "r")))
        perror(cmd);
    XtFree(cmd);
    if (!pp)
        return;

    /* put the output of the command in the Text widget by reading
     * until EOF (meaning that the command has terminated).
     */
    for (pos = 0; fgets(buf, sizeof buf, pp); pos += strlen(buf))
        XmTextReplace(text_w, pos, pos, buf);

    pclose(pp);
}
