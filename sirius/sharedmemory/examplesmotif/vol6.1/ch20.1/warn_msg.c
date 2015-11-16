/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* warn_msg.c -- display a very urgent warning message.
 * Really catch the user's attention by flashing an urgent-
 * looking pixmap every 250 milliseconds.
 * The program demonstrates how to set the XmNsymbolPixmap
 * resource, how to destroy the pixmap and how to use timers
 * (XtAppAddTimeOut()).
 */
#include <Xm/MessageB.h>
#include <Xm/PushB.h>

/* main() --create a pushbutton whose callback pops up a dialog box */
main(argc, argv)
char *argv[];
{
    XtAppContext app;
    Widget toplevel, button;
    XmString label;
    void warning();

    toplevel = XtVaAppInitialize(&app, "Demos",
        NULL, 0, &argc, argv, NULL, NULL);

    label = XmStringCreateSimple(
        "Don't Even Think About Pressing This Button");
    button = XtVaCreateManagedWidget("button",
        xmPushButtonWidgetClass, toplevel,
        XmNlabelString,          label,
        NULL);
    XmStringFree(label);

    /* set up callback to popup warning with random message */
    XtAddCallback(button, XmNactivateCallback, warning,
        "Alert!\nThe computer room is ON FIRE!\n\
         (All your e-mail will be lost.)");

    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}

#include "bang0.symbol"
#include "bang1.symbol"

/* define the data structure we need to implement flashing effect */
typedef struct {
    XtIntervalId   id;
    int            which;
    Pixmap         pix1, pix2;
    Widget         dialog;
    XtAppContext   app;
} TimeOutClientData;

/* The callback routine for the push button.  Create a message
 * dialog and set the message string.  Allocate an instance of
 * the TimeOutClientData structure and set a timer to alternate
 * between the two pixmaps.  The data is passed to the timeout
 * routine and the callback for when the user presses "OK".
 */
void
warning(parent, help_text)
Widget parent;
char *help_text;
{
    Widget        dialog;
    XtAppContext  app = XtWidgetToApplicationContext(parent);
    XmString      text;
    extern void   done(), destroy_it(), blink();
    Display      *dpy = XtDisplay(parent);
    int           screen = DefaultScreen(dpy);
    Pixel         fg, bg;
    Arg           args[2];
    TimeOutClientData *data = XtNew(TimeOutClientData);

    /* Create the dialog -- the "cancel" button says OK */
    text = XmStringCreateSimple("OK");
    XtSetArg(args[0], XmNcancelLabelString, text);
    XtSetArg(args[1], XmNdeleteResponse, XmDESTROY);
    dialog = XmCreateMessageDialog(parent, "danger", args, 2);
    XmStringFree(text);

    XtUnmanageChild(  /* no need for the ok button */
        XmMessageBoxGetChild(dialog, XmDIALOG_OK_BUTTON));
    XtUnmanageChild(  /* no need for the help button */
        XmMessageBoxGetChild(dialog, XmDIALOG_HELP_BUTTON));
    /* The cancel button is the only button left... */
    XtAddCallback(dialog, XmNcancelCallback, done, NULL);

    XtAddCallback(dialog, XmNdestroyCallback, destroy_it, data);

    /* now that dialog has been created, it's colors are initialized */
    XtVaGetValues(dialog,
        XmNforeground, &fg,
        XmNbackground, &bg,
        NULL);

    /* Create pixmaps that are going to be used as symbolPixmaps.
     * Use the foreground and background colors of the dialog.
     */
    data->pix1 = XCreatePixmapFromBitmapData(dpy, XtWindow(parent),
        bang0_bits, bang0_width, bang0_height,
        fg, bg, DefaultDepth(dpy, screen));
    data->pix2 = XCreatePixmapFromBitmapData(dpy, XtWindow(parent),
        bang1_bits, bang1_width, bang1_height,
        fg, bg, DefaultDepth(dpy, screen));
    /* complete the timeout client data */
    data->dialog = dialog;
    data->app = app;

    /* Add the timeout for blinking effect */
    data->id = XtAppAddTimeOut(app, 1000L, blink, data);

    /* display the help text and the appropriate pixmap */
    text = XmStringCreateLtoR(help_text, XmSTRING_DEFAULT_CHARSET);
    XtVaSetValues(dialog,
        XmNmessageString,      text,
        XmNsymbolPixmap,       data->pix2,
        NULL);
    XmStringFree(text);

    XtManageChild(dialog);
    XtPopup(XtParent(dialog), XtGrabNone);
}

/* blink() --visual blinking effect for dialog's symbol.  Displays
 * flashing "!" symbol. Also, restart timer and save timer id.
 */
void
blink(data)
TimeOutClientData *data;
{
    data->id = XtAppAddTimeOut(data->app, 250L, blink, data);
    XtVaSetValues(data->dialog,
        XmNsymbolPixmap,  (data->which = !data->which)?
            data->pix1 : data->pix2,
        NULL);
}

/* done() --called when user presses "Ok" in HelpDialog or
 * if the user picked the Close button in system menu.
 * Remove the timeout id stored in data, free pixmaps and
 * make sure the widget is destroyed (which is only when
 * the user presses the "Ok" button.
 */
void
done(dialog)
Widget dialog; /* happens to be the same as data->dialog */
{
    XtDestroyWidget(dialog);
}

void
destroy_it(dialog, data)
Widget dialog; /* happens to be the same as data->dialog */
TimeOutClientData *data;
{
    Pixmap symbol;

    XtRemoveTimeOut(data->id);
    XFreePixmap(XtDisplay(data->dialog), data->pix1);
    XFreePixmap(XtDisplay(data->dialog), data->pix2);
    XtFree(data);
}
