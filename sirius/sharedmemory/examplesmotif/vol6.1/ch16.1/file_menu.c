/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

#include <Xm/RowColumn.h>
#include <Xm/MainW.h>
#include <Xm/DrawingA.h>
#include <Xm/CascadeBG.h>
#include <Xm/CascadeB.h>
#include <Xm/SeparatoG.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>

main(argc, argv)
int argc;
char *argv[];
{
    Widget toplevel, drawing_a, MainWindow, MenuBar, FilePullDown;
    XmString    label_str;
    XtAppContext app;

    toplevel = XtVaAppInitialize(&app, "Demos", NULL, 0,
        &argc, argv, NULL, NULL);

    MainWindow = XtVaCreateManagedWidget("main_w",
        xmMainWindowWidgetClass, toplevel,
        XmNscrollingPolicy,  XmAUTOMATIC,
        NULL);

    MenuBar = XmCreateMenuBar(MainWindow, "MenuBar", NULL, 0); 

    /* create the "File" Menu */
    FilePullDown = XmCreatePulldownMenu(MenuBar, "FilePullDown", NULL, 0);

    /* create the "File" button (attach Menu via XmNsubMenuId) */
    label_str = XmStringCreateSimple("File");
    XtVaCreateManagedWidget("File", xmCascadeButtonWidgetClass, MenuBar,
	XmNlabelString,  label_str,
	XmNmnemonic,    'F',
	XmNsubMenuId,    FilePullDown,
	NULL);
    XmStringFree(label_str); /* don't need it any longer */

    /* Now add the menu items */
    XtVaCreateManagedWidget("Open",
	xmPushButtonGadgetClass, FilePullDown, NULL);

    XtVaCreateManagedWidget("Close",
	xmPushButtonGadgetClass, FilePullDown, NULL);

    XtVaCreateManagedWidget("separator",
	xmSeparatorGadgetClass, FilePullDown, NULL);

    XtVaCreateManagedWidget("Quit",
	xmPushButtonGadgetClass, FilePullDown, NULL);

    XtManageChild(MenuBar);

    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}
