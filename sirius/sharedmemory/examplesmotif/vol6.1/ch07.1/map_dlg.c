/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* map_dlg.c -- Use the XmNmapCallback to automatically position
 * a dialog on the screen.  Each time the dialog is displayed, it
 * is mapped down and to the right by 200 pixels in each direction.
 */
#include <Xm/MessageB.h>
#include <Xm/PushB.h>

/* main() --create a pushbutton whose callback pops up a dialog box */
main(argc, argv)
char *argv[];
{
    Widget toplevel, button;
    XtAppContext app;
    void pushed();

    toplevel = XtVaAppInitialize(&app, "Demos",
        NULL, 0, &argc, argv, NULL, NULL);

    button = XtCreateManagedWidget("button", xmPushButtonWidgetClass,
        toplevel, NULL, 0);
    XtAddCallback(button, XmNactivateCallback, pushed, "Hello World");

    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}

/* callback function for XmNmapCallback.  Position dialog in 200 pixel
 * "steps".  When the edge of the screen is hit, start over.
 */
static void
map_dialog(dialog, client_data, cbs)
Widget dialog;
XtPointer client_data;
XmAnyCallbackStruct *cbs;
{
    static Position x, y;
    Dimension w, h;

    XtVaGetValues(dialog, XmNwidth, &w, XmNheight, &h, NULL);
    if ((x + w) >= WidthOfScreen(XtScreen(dialog)))
        x = 0;
    if ((y + h) >= HeightOfScreen(XtScreen(dialog)))
        y = 0;
    XtVaSetValues(dialog, XmNx, x, XmNy, y, NULL);
    x += 200, y += 200;
}

/* pushed() --the callback routine for the main app's pushbutton.
 * Create and popup a dialog box that has callback functions for
 * the Ok, Cancel and Help buttons.
 */
void
pushed(w, message)
Widget w;
char *message; /* The client_data parameter passed by XtAddCallback */
{
    Widget dialog;
    Arg arg[3];
    XmString t = XmStringCreateSimple(message);
    extern void response();

    XtSetArg(arg[0], XmNautoUnmanage, False);
    XtSetArg(arg[1], XmNmessageString, t);
    XtSetArg(arg[2], XmNdefaultPosition, False);
    dialog = XmCreateMessageDialog(w, "notice", arg, 3);
    XmStringFree(t);

    XtAddCallback(dialog, XmNmapCallback, map_dialog, NULL);

    XtManageChild(dialog);
    XtPopup(XtParent(dialog), XtGrabNone);
}
