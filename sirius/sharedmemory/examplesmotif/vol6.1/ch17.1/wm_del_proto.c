/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* wm_del_proto.c -- demonstrate how client messages are caught and
 * processed by adding an event handler for them.  Main window
 * just pops up a simple dialog, which has a client event handler.
 * Use the Close button in the window manager's system menu to
 * exercise it.  Watch the print statements during execution.
 */
#include <Xm/MessageB.h>
#include <Xm/PushB.h>
#include <Xm/Protocols.h>

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

/* pushed() --the callback routine for the main app's pushbutton.
 * Create any old dialog box that has a callback function for
 * client messages.
 */
void
pushed(w, message)
Widget w;
char *message; /* really: client_data, but we know what it is */
{
    Widget dialog, shell;
    void do_client_msg();
    Atom protocol;
    XmString t = XmStringCreateSimple(message);
    Arg args[1];

    XtSetArg(args[0], XmNmessageString, t);
    dialog = XmCreateMessageDialog(w, "notice", args, 1);
    XmStringFree(t);

    XtManageChild(dialog);

    shell = XtParent(dialog);
    protocol = XmInternAtom(XtDisplay(w), "WM_DELETE_WINDOW", False);
    XmAddWMProtocols(shell, &protocol, 1);
    XtAddEventHandler(shell, NoEventMask, True, do_client_msg, NULL);
}

/* function to handle client messages only.  Just catch WM_DELETE_WINDOW
 * messages and report them; ignore other messages.
 */
void
do_client_msg(widget, client_data, msg)
Widget widget;
XtPointer client_data;
XClientMessageEvent *msg;
{
    char  *str;
    int   message = msg->data.l[0];
    Atom  WM_DELETE_WINDOW;

    if (msg->type != ClientMessage)
        return;

    WM_DELETE_WINDOW = XmInternAtom(msg->display, "WM_DELETE_WINDOW", False);

    /* Get the atom name associated with the client message */
    str = XGetAtomName(msg->display, msg->message_type);
    printf("msg type = %s (%d)\n", str, msg->message_type);
    XFree(str);

    /* Get the atom name of the message itself... */
    str = XGetAtomName(msg->display, message);
    printf("message = %s (%d)\n", str, message);
    XFree(str);

    if (message == WM_DELETE_WINDOW)
        puts("closing window");
}
