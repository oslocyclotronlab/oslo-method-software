/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* undo.c -- demonstrate undoing a clipboard copy */

#include <Xm/Xm.h>
#include <Xm/CutPaste.h>
#include <Xm/RowColumn.h>
#include <Xm/PushBG.h>

void
cut_to_clipboard(widget, data)
Widget widget;
char *data;
{
    unsigned long item_id = 0;	/* clipboard item id */
    int data_id = 0;	/* clipboard data id */
    int status = 0;	/* clipboard status  */
    XmString clip_label;
    char buf[32];
    static int cnt;
    Display *dpy = XtDisplayOfObject(widget);
    Window window = XtWindowOfObject(widget);

    sprintf(buf, "%s %d", data, ++cnt);
    printf("Putting \"%s\" on clipboard\n", buf);

    clip_label = XmStringCreateSimple("cut_to_clipboard");
    /*
     * start copy to clipboard, and continue till a sucessful start copy
     * is made
     */
    status = 0;
    while (status != ClipboardSuccess)
	status = XmClipboardStartCopy(dpy, window,
	    clip_label, CurrentTime, NULL, NULL, &item_id);

    /*
     * move the data to the clipboard, and continue till a sucessful copy
     * is made
     */
    status = 0;
    while (status != ClipboardSuccess)
	status = XmClipboardCopy(dpy, window,
	    item_id, "STRING", buf, (long) strlen(buf) + 1, 0, &data_id);

    /*
     * end the copy to the clipboard and continue till a sucessful end
     * copy is made
     */
    status = 0;
    while (status != ClipboardSuccess)
	status = XmClipboardEndCopy(dpy, window,
	    item_id);
}

void
undo(widget)
Widget widget;
{
    XmClipboardUndoCopy(XtDisplayOfObject(widget), XtWindowOfObject(widget));
}

void
retrieve_from_clipboard(widget)
Widget widget;
{
    int status = ClipboardLocked;
    char buf[32];
    Display *dpy = XtDisplayOfObject(widget);
    Window window = XtWindowOfObject(widget);

    XmClipboardStartRetrieve(dpy, window, CurrentTime);
    while (status == ClipboardLocked) {
	status = XmClipboardRetrieve(dpy, window,
	    "STRING", buf, sizeof buf, NULL, NULL);
	printf("status = %s\n",
	    (status == ClipboardSuccess)? "success" :
	    (status == ClipboardLocked)? "locked" :
	    (status == ClipboardNoData)? "no data" :
	    (status == ClipboardTruncate)? "data truncated" :
	    (status == ClipboardFail)? "Failed" : "Bad Format");
	if (status == ClipboardSuccess)
	    puts(buf);
    }
    XmClipboardEndRetrieve(dpy, window);
}

main(argc, argv)
char *argv[];
{
    Widget toplevel, rowcol, button;
    XtAppContext app;

    toplevel = XtVaAppInitialize(&app, "Demos", NULL, 0,
	&argc, argv, NULL, NULL);

    rowcol = XtVaCreateManagedWidget("rowcol",
	xmRowColumnWidgetClass, toplevel,
	NULL);
    button = XtVaCreateManagedWidget("button1",
	xmPushButtonGadgetClass, rowcol,
	XtVaTypedArg, XmNlabelString, XmRString,
	    "Cut To Clipboard", sizeof (char *),
	NULL);
    XtAddCallback(button, XmNactivateCallback, cut_to_clipboard, "data");

    button = XtVaCreateManagedWidget("button2",
	xmPushButtonGadgetClass, rowcol,
	XtVaTypedArg, XmNlabelString, XmRString,
	   "Undo Cut", sizeof (char *),
	NULL);
    XtAddCallback(button, XmNactivateCallback, undo, NULL);

    button = XtVaCreateManagedWidget("retrieve",
	xmPushButtonGadgetClass, rowcol,
	XtVaTypedArg, XmNlabelString, XmRString,
	    "Retrieve From Clipboard", sizeof (char *),
	NULL);
    XtAddCallback(button, XmNactivateCallback, retrieve_from_clipboard, NULL);

    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}
