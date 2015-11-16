/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* copy_retrieve.c -- simple copy and retrieve program.  Two
 * pushbuttons: the first places text in the clipboard, the other
 * receives text from the clipboard.  This just demonstrates the
 * API involved.
 */
#include <Xm/CutPaste.h>
#include <Xm/RowColumn.h>
#include <Xm/PushBG.h>

static void to_clipbd(), from_clipbd();

main(argc, argv)
int argc;
char *argv[];
{
    Widget toplevel, rowcol, button;
    XtAppContext app;

    /* Initialize toolkit, application context and toplevel shell */
    toplevel = XtVaAppInitialize(&app, "Demos", NULL, 0,
        &argc, argv, NULL, NULL);

    /* manage two buttons in a RowColumn widget */
    rowcol = XtVaCreateWidget("rowcol", xmRowColumnWidgetClass, toplevel, NULL);

    /* button1 copies to the clipboard */
    button = XtVaCreateManagedWidget("button1",
        xmPushButtonGadgetClass, rowcol,
        XtVaTypedArg, XmNlabelString, XmRString,
            "Copy To Clipboard", 18, /* strlen() + 1 */
        NULL);
    XtAddCallback(button, XmNactivateCallback, to_clipbd, "text");

    /* button2 retrieves text stored in the clipboard */
    button = XtVaCreateManagedWidget("button2",
        xmPushButtonGadgetClass, rowcol,
        XtVaTypedArg, XmNlabelString, XmRString,
            "Retrieve From Clipboard", 24, /* strlen() + 1 */
        NULL);
    XtAddCallback(button, XmNactivateCallback, from_clipbd, NULL);

    /* manage RowColumn, realize toplevel shell and start main loop */
    XtManageChild(rowcol);
    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}

/* copy data to clipboard. */
static void
to_clipbd(widget, data)
Widget widget;
char *data;
{
    unsigned long item_id = 0;  /* clipboard item id */
    int           status;
    XmString      clip_label;
    char          buf[32];
    static int    cnt;
    Display      *dpy = XtDisplayOfObject(widget);
    Window        window = XtWindowOfObject(widget);

    sprintf(buf, "%s-%d", data, ++cnt); /* make each copy unique */

    clip_label = XmStringCreateSimple("to_clipbd");

    /* start a copy.  retry till unlocked */
    do
        status = XmClipboardStartCopy(dpy, window,
            clip_label, CurrentTime, NULL, NULL, &item_id);
    while (status == ClipboardLocked);

    XmStringFree(clip_label);

    /* copy the data (buf) -- pass "cnt" as private id for kicks */
    do
        status = XmClipboardCopy(dpy, window, item_id, "STRING",
            buf, (long)strlen(buf)+1, cnt, NULL);
    while (status == ClipboardLocked);

    /* end the copy */
    do
        status = XmClipboardEndCopy(dpy, window, item_id);
    while (status == ClipboardLocked);

    printf("copied \"%s\" to clipboard.\n", buf);
}

static void
from_clipbd(widget)
Widget widget;
{
    int         status, private_id;
    char        buf[32];
    Display    *dpy = XtDisplayOfObject(widget);
    Window      window = XtWindowOfObject(widget);

    do
        status = XmClipboardRetrieve(dpy, window,
            "STRING", buf, sizeof buf, NULL, &private_id);
    while (status == ClipboardLocked);

    if (status == ClipboardSuccess)
        printf("retrieved \"%s\" (private id = %d).\n",
            buf, private_id);
}
