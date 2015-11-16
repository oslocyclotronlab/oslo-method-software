/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* copy_by_name.c -- demonstrate clipboard bopies "by-name".
 * Copying by name requires that the copy *to* clipboard
 * functions use the same window as the copy *from* clipboard
 * functions.  This is a restriction placed on the API by the
 * toolkit, not by the ICCC.
 */

#include <Xm/Xm.h>
#include <Xm/CutPaste.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>

static void to_clipbd(), from_clipbd();
Boolean by_name;
Widget toplevel;

main(argc, argv)
int argc;
char *argv[];
{
    Widget rowcol, button;
    XtAppContext app;

    /* Initialize toolkit, application context and toplevel shell */
    toplevel = XtVaAppInitialize(&app, "Demos", NULL, 0,
        &argc, argv, NULL, NULL);

    by_name = argc > 1 && !strcmp(argv[1], "-byname");

    /* manage two buttons in a RowColumn widget */
    rowcol = XtVaCreateWidget("rowcol",
        xmRowColumnWidgetClass, toplevel,
        NULL);

    /* button1 copies to the clipboard */
    button = XtVaCreateManagedWidget("button1",
        xmPushButtonWidgetClass, rowcol,
        XtVaTypedArg, XmNlabelString, XmRString,
            "Copy To Clipboard", sizeof (char *),
        NULL);
    XtAddCallback(button, XmNactivateCallback, to_clipbd, "text");

    /* button2 retrieves text stored in the clipboard */
    button = XtVaCreateManagedWidget("button2",
        xmPushButtonWidgetClass, rowcol,
        XtVaTypedArg, XmNlabelString, XmRString,
            "Retrieve From Clipboard", sizeof (char *),
        NULL);
    XtAddCallback(button, XmNactivateCallback, from_clipbd, NULL);

    /* manage RowColumn, realize toplevel shell and start main loop */
    XtManageChild(rowcol);
    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}

static void
copy_by_name(widget, data_id, private_id, reason)
Widget widget;
int *data_id, *private_id, *reason;
{
    Display      *dpy = XtDisplay(toplevel);
    Window        window = XtWindow(toplevel);
    static int    cnt;
    int           status;
    char          buf[32];

    printf("copy_by_name called\n\treason: %s, private_id: %d, data_id: %d\n",
        *reason == XmCR_CLIPBOARD_DATA_REQUEST? "request" : "delete",
        *private_id, *data_id);
    
    if (*reason == XmCR_CLIPBOARD_DATA_REQUEST) {
        sprintf(buf, "stuff-%d", ++cnt); /* make each copy unique */

         do status = XmClipboardCopyByName(dpy, window, *data_id, buf,
            strlen(buf)+1, *private_id = cnt);
        while (status != ClipboardSuccess);
        puts("by-name: data copied");
    }
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
    Display      *dpy = XtDisplay(toplevel);
    Window        window = XtWindow(toplevel);

    clip_label = XmStringCreateSimple("to_clipbd");

    printf("Starting copy %s\n", by_name? "by-name" : "");
    /* start a copy.  retry till unlocked */
    do
        status = XmClipboardStartCopy(dpy, window,
            clip_label, CurrentTime, widget,
            by_name? copy_by_name : NULL, &item_id);
    while (status == ClipboardLocked);

    puts(by_name? "installing copy_by_name()" : "sending \"testing\"");
    /* copy by name by passing NULL as the "data", copy_by_name() as
     * the callback and "widget" as the widget.
     */
    do
        status = XmClipboardCopy(dpy, window, item_id, "STRING",
            by_name? NULL : "testing", 8L, 0, NULL);
    while (status == ClipboardLocked);

    puts("end copy");
    /* end the copy */
    do
        status = XmClipboardEndCopy(dpy, window, item_id);
    while (status == ClipboardLocked);
    puts("done.");

    XmClipboardUnlock(dpy, window, True);
}

static void
from_clipbd(widget)
Widget widget;
{
    int           status;
    unsigned      total_bytes;
    unsigned long received;
    char         *data = NULL, buf[32];
    Display      *dpy = XtDisplay(toplevel);
    Window        window = XtWindow(toplevel);

    puts("start retrieve");
    do
        status = XmClipboardStartRetrieve(dpy, window, CurrentTime);
    while (status == ClipboardLocked);

    puts("retrieving");
    /* initialize data to contain at least one byte. */
    data = XtMalloc(1);
    total_bytes = 1;
    do  {
        buf[0] = 0;
        /* retrieve data from clipboard -- if locked, try again */
        if ((status = XmClipboardRetrieve(dpy, window, "STRING",
                buf, sizeof buf, &received, NULL)) == ClipboardLocked)
            continue;
        if (status == ClipboardNoData) {
            puts("No data");
            break;
        }
        /* reallocate data to contain enough space for everything */
        if (!(data = XtRealloc(data, total_bytes + received))) {
            XtError("Can't allocate space for data");
            break; /* XtError may or may not return */
        }
        /* copy buf into data.  strncpy() does not NULL terminate */
        strncpy(&data[total_bytes-1], buf, received);
        total_bytes += received;
    } while (status == ClipboardLocked || status == ClipboardTruncate);
    data[total_bytes-1] = 0; /* NULL terminate */

    if (status == ClipboardSuccess)
        printf("retrieved \"%s\" from clipboard (%d bytes).\n",
            data, total_bytes);

    puts("end retrieve");
    do
        status = XmClipboardEndRetrieve(dpy, window);
    while (status == ClipboardLocked);
    puts("done.");

    XmClipboardUnlock(dpy, window, True);
}
