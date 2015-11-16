/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* wm_protocol.c -- demonstrate how to add your own protocol to a
 * shell.  The nature of the protocol isn't important; however, it
 * must be registered with the _MOTIF_WM_MESSAGES property on the
 * shell.  We also add a menu item to the window manager frame's
 * window menu to allow the user to activate the protocol, if desired.
 */
#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <stdio.h>

main(argc, argv)
char *argv[];
{
    Widget toplevel;
    XtAppContext app;
    Atom motif_msgs, my_protocol;
    void my_proto_callback();
    char buf[64];

    /* initialize toolkit normally; argv has its Xt-specific args stripped */
    toplevel = XtVaAppInitialize(&app, "Demos", NULL, 0, &argc, argv, NULL,
        XmNwidth, 100,
        XmNheight, 100,
        NULL);

    /* get the motif_msgs and my_protocol atoms... if it doesn't exist,
     * it'll get created.
     */
    my_protocol = XmInternAtom(XtDisplay(toplevel), "_MY_PROTOCOL", False);
    motif_msgs = XmInternAtom(XtDisplay(toplevel), "_MOTIF_WM_MESSAGES", False);

    /* Add my_protocol to the _MOTIF_WM_MESSAGES VendorShell-defined
     * property on the shell.  Add a callback for this protocol.
     */
    XmAddProtocols(toplevel, motif_msgs, &my_protocol, 1);
    XmAddProtocolCallback(toplevel,
        motif_msgs, my_protocol, my_proto_callback, NULL);

    /* allow the user to activate the protocol through the window manager's
     * window menu on the shell.
     */
    sprintf(buf, "MyProtocol _P Ctrl<Key>P f.send_msg %d", my_protocol);
    XtVaSetValues(toplevel, XmNmwmMenu, buf, NULL);

    /* create widgets... */

    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}

/* called if _MY_PROTOCOL was activated, a client message was sent... */
void
my_proto_callback(widget, client_data, cbs)
Widget widget;              /* protocol widget -- unused */
XtPointer client_data;      /* NULL was passed */
XmAnyCallbackStruct *cbs;   /* unused */
{
    puts("my protocol got activated!");
}
