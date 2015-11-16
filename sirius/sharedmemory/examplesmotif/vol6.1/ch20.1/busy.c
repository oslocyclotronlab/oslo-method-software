/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* busy.c -- demonstrate how to use a WorkingDialog and to process
 * only "important" events.  e.g., those that may interrupt the
 * task or to repaint widgets for exposure.  Set up a simple shell
 * and a widget that, when pressed, immediately goes into its own
 * loop.  First, "lock" the shell so that a timeout cursor is set on
 * the shell and pop up a WorkingDialog.  Then enter loop ... sleep
 * for one second ten times, checking between each interval to see
 * if the user clicked the Stop button or if any widgets need to be
 * refreshed.  Ignore all other events.
 *
 * main() and get_busy() are stubs that would be replaced by a real
 * application; all other functions can be used "as is."
 */
#include <Xm/MessageB.h>
#include <Xm/PushB.h>
#include <X11/cursorfont.h>

Widget shell;
void TimeoutCursors();
Boolean CheckForInterrupt();

main(argc, argv)
int argc;
char *argv[];
{
    XtAppContext app;
    Widget button;
    XmString label;
    void get_busy();

    shell = XtVaAppInitialize(&app, "Demos",
        NULL, 0, &argc, argv, NULL, NULL);

    label = XmStringCreateSimple(
        "Boy, is *this* going to take a long time.");
    button = XtVaCreateManagedWidget("button",
        xmPushButtonWidgetClass, shell,
        XmNlabelString,          label,
        NULL);
    XmStringFree(label);
    XtAddCallback(button, XmNactivateCallback, get_busy, argv[1]);

    XtRealizeWidget(shell);
    XtAppMainLoop(app);
}

void
get_busy(widget)
Widget widget;
{
    int n;

    TimeoutCursors(True, True);
    for (n = 0; n < 10; n++) {
        sleep(1);
        if (CheckForInterrupt()) {
            puts("Interrupt!");
            break;
        }
    }
    if (n == 10)
        puts("done.");
    TimeoutCursors(False, NULL);
}

/* The interesting part of the program -- extract and use at will */
static Boolean stopped;  /* True when user wants to stop processing */
static Widget dialog;    /* WorkingDialog displayed when timed out */

/* timeout_cursors() turns on the "watch" cursor over the application
 * to provide feedback for the user that he's going to be waiting
 * a while before he can interact with the appliation again.
 */
void
TimeoutCursors(on, interruptable)
int on, interruptable;
{
    static int locked;
    static Cursor cursor;
    extern Widget shell;
    XSetWindowAttributes attrs;
    Display *dpy = XtDisplay(shell);
    XEvent event;
    Arg args[1];
    XmString str;
    extern void stop();

    /* "locked" keeps track if we've already called the function.
     * This allows recursion and is necessary for most situations.
     */
    on? locked++ : locked--;
    if (locked > 1 || locked == 1 && on == 0)
        return; /* already locked and we're not unlocking */

    stopped = False; /* doesn't matter at this point; initialize */
    if (!cursor) /* make sure the timeout cursor is initialized */
        cursor = XCreateFontCursor(dpy, XC_watch);

    /* if "on" is true, then turn on watch cursor, otherwise, return
     * the shell's cursor to normal.
     */
    attrs.cursor = on? cursor : None;

    /* change the main application shell's cursor to be the timeout
     * cursor (or to reset it to normal).  If other shells exist in
     * this application, they will have to be listed here in order
     * for them to have timeout cursors too.
     */
    XChangeWindowAttributes(dpy, XtWindow(shell), CWCursor, &attrs);

    XFlush(dpy);

    if (on) {
        /* we're timing out, put up a WorkingDialog.  If the process
         * is interruptable, allow a "Stop" button.  Otherwise, remove
         * all actions so the user can't stop the processing.
         */
        str = XmStringCreateSimple("Busy.  Please Wait.");
        XtSetArg(args[0], XmNmessageString, str);
        dialog = XmCreateWorkingDialog(shell, "Busy", args, 1);
        XmStringFree(str);
        XtUnmanageChild(
            XmMessageBoxGetChild(dialog, XmDIALOG_OK_BUTTON));
        if (interruptable) {
            str = XmStringCreateSimple("Stop");
            XtVaSetValues(dialog, XmNcancelLabelString, str, NULL);
            XmStringFree(str);
            XtAddCallback(dialog, XmNcancelCallback, stop, NULL);
        } else
            XtUnmanageChild(
                XmMessageBoxGetChild(dialog, XmDIALOG_CANCEL_BUTTON));
        XtUnmanageChild(
            XmMessageBoxGetChild(dialog, XmDIALOG_HELP_BUTTON));
        XtManageChild(dialog);
    } else {
        /* get rid of all button and keyboard events that occured
         * during the time out.  The user shouldn't have done anything
         * during this time, so flush for button and keypress events.
         * KeyRelease events are not discarded because accelerators
         * require the corresponding release event before normal input
         * can continue.
         */
        while (XCheckMaskEvent(dpy,
                ButtonPressMask | ButtonReleaseMask | ButtonMotionMask
                | PointerMotionMask | KeyPressMask, &event)) {
            /* do nothing */;
        }
        XtDestroyWidget(dialog);
    }
}

/* User Pressed the "Stop" button in dialog. */
void
stop(dialog)
Widget dialog;
{
    stopped = True;
}

Boolean
CheckForInterrupt()
{
    extern Widget shell;
    Display *dpy = XtDisplay(shell);
    Window win = XtWindow(dialog);
    XEvent event;

    /* Make sure all our requests get to the server */
    XFlush(dpy);

    /* Let motif process all pending exposure events for us. */
    XmUpdateDisplay(shell);

    /* Check the event loop for events in the dialog ("Stop"?) */
    while (XCheckMaskEvent(dpy,
            ButtonPressMask | ButtonReleaseMask | ButtonMotionMask |
            PointerMotionMask | KeyPressMask | KeyReleaseMask,
            &event)) {
        /* got an "interesting" event. */
        if (event.xany.window == win)
            XtDispatchEvent(&event); /* it's in our dialog.. */
        else /* uninteresting event--throw it away and sound bell */
            XBell(dpy, 50);
    }
    return stopped;
}
