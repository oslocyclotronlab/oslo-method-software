/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* popups.c -- demonstrate the use of popup menus in arbitrary widgets.
 * Display two PushButtons.  The second one has a popup menu
 * attached to it, but it must be activated with the *third*
 * mouse button--you cannot use the second button because of
 * a bug with the Motif toolkit (but that's ok, the Style Guide
 * says to use the third button anyway).  You don't want to use
 * the first mouse button because that activates the PushButton.
 * If you want the first button to display a menu, use an
 * OptionMenu.
 */
#include <Xm/LabelG.h>
#include <Xm/PushBG.h>
#include <Xm/PushB.h>
#include <Xm/SeparatoG.h>
#include <Xm/RowColumn.h>
#include <Xm/MenuShell.h>
#include <Xm/MessageB.h>
#include <Xm/FileSB.h>
#include <Xm/CascadeBG.h>

Widget toplevel;
extern void exit();
void open_dialog_box();

/* dummy callback for pushbutton activation */
void
put_string(w, str)
Widget w;
String str;
{
    puts(str);
}

typedef struct _menu_item {
    char        *label;
    WidgetClass *class;
    char         mnemonic;
    char        *accelerator;
    char        *accel_text;
    void       (*callback)();
    caddr_t      callback_data;
    struct _menu_item *subitems;
} MenuItem;

MenuItem file_items[] = {
    { "File Items", &xmLabelGadgetClass, NULL, NULL, NULL, NULL, NULL, NULL },
    { "_sep1", &xmSeparatorGadgetClass, NULL, NULL, NULL, NULL, NULL, NULL },
    { "New", &xmPushButtonGadgetClass, 'N', "Ctrl<Key>N", "Ctrl+N",
        put_string, "New", NULL },
    { "Open ...", &xmPushButtonGadgetClass, 'O', NULL, NULL,
        open_dialog_box, (caddr_t)XmCreateFileSelectionDialog, NULL },
    { "Save", &xmPushButtonGadgetClass, 'S', NULL, NULL,
        put_string, "Save", NULL },
    { "Save As ...", &xmPushButtonGadgetClass, 'A', NULL, NULL,
        open_dialog_box, (caddr_t)XmCreateFileSelectionDialog, NULL },
    { "Print ...", &xmPushButtonGadgetClass, 'P', NULL, NULL,
       open_dialog_box, (caddr_t)XmCreateMessageDialog, NULL },
    { "Exit", &xmPushButtonGadgetClass, 'E', "Ctrl<Key>C", "Ctrl+C",
       exit, NULL, NULL },
    NULL,
};

main(argc, argv)
int argc;
char *argv[];
{
    Widget BuildPopupMenu(), button, rowcol, popup;
    XtAppContext app;
    extern void PostIt();

    toplevel = XtVaAppInitialize(&app, "Demos", NULL, 0,
        &argc, argv, NULL, NULL);

    /* Build a RowColumn to contain two PushButtons */
    rowcol = XtVaCreateManagedWidget("rowcol",
        xmRowColumnWidgetClass, toplevel,
        NULL);

    /* The first PushButton is a -gadget-, so we cannot popup a menu
     * from here!
     */
    button = XtVaCreateManagedWidget("PushMe-1",
        xmPushButtonGadgetClass, rowcol, NULL);
    XtAddCallback(button, XmNactivateCallback, put_string, "button1");

    /* This PushButton is a widget, so it has its own window, so
     * we can pop up a menu from here by adding an event handler
     * specifically for the 3rd mouse button (motif compliance).
     */
    button = XtVaCreateManagedWidget("PushMe-2",
        xmPushButtonWidgetClass, rowcol,
        NULL);
    /* it can still have its callback! */
    XtAddCallback(button, XmNactivateCallback, put_string, "button2");

    /* build the menu... */
    popup = BuildPopupMenu(button, "Stuff", file_items, XmMENU_POPUP);
    /* Add the event handler (PostIt()) and pass the newly created menu
     * as the client_data.  This is done to avoid using unnecessary globals.
     */
    XtAddEventHandler(button, ButtonPressMask, False, PostIt, popup);

    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}

/* Event handler for the 3rd mouse button on the PushButton widget's window */
void
PostIt(pb, popup, event)
Widget pb; /* the pushbutton -- unused here */
Widget popup;  /* the client_data passed to XtAddEventHandler() */
XButtonPressedEvent *event; /* We know it's a button press because of how
                               we registered the event handler. */
{
    if (event->button != 3)
        return;
    /* position the menu at the location of the button press!  If we wanted
     * to position it elsewhere, we could change the x,y fields of the
     * event structure.
     */
    XmMenuPosition(popup, event);
    XtManageChild(popup);
}


/* callback for some of the menu items declared in the MenuItem struct.
 * The client data is a function that creates some sort of Motif dialog.
 * Since the function returns this dialog, associate it with the menu
 * item via XmNuserData so we (a) don't have a to keep a global and
 * (b) don't have to repeatedly create one.
 */
void
open_dialog_box(w, func, cbs)
Widget w;           /* the menu item that was invoked */
Widget (*func)(); /* function that creates and returns a dialog widget */
XmAnyCallbackStruct *cbs; /* unused */
{
    /* initialize in case get values doesn't return anything sensible. */
    Widget dialog = NULL;

    /* first see if this menu item's dialog has been created yet */
    XtVaGetValues(w, XmNuserData, &dialog, NULL);
    if (!dialog) {
        /* menu item hasn't been chosen yet -- create the dialog.
         * Use the toplevel as the parent because we don't want the
         * parent of a dialog to be a menu item!
         *
         * Give the dialog's title the menu item lable (arbitrary, but
         * convenient).
         *
         * Since this is a dummy function, the dialogs does nothing.
         * Attach XtUnmanageChild() as the callback for Ok and Cancel.
         *
         * Obviously, this dialog doesn't have anything in it, but this
         * is a simple example.  You could modify this open_dialog to pass
         * all necessary data in the form of a new data structure as the
         * client data.  This structure should contain the dialog creation
         * function and other stuff as needed.
         */
        dialog = (*func)(toplevel, "dialog", NULL, 0);
        XtVaSetValues(XtParent(dialog), XmNtitle, XtName(w), NULL);
        XtAddCallback(dialog, XmNokCallback, XtUnmanageChild, w);
        XtAddCallback(dialog, XmNcancelCallback, XtUnmanageChild, w);

        /* store the newly created dialog in the XmNuserData for the menu
         * item for easy retrieval next time. (see get-values above.)
         */
        XtVaSetValues(w, XmNuserData, dialog, NULL);
    }
    XtManageChild(dialog);

    /* call XtPopup() for good form... */
    XtPopup(XtParent(dialog), XtGrabNone);
    /* If the dialog was already open, XtPopup does nothing.  In
     * this case, at least make sure the window is raised to the top
     * of the window tree (or as high as it can get).
     */
    XRaiseWindow(XtDisplay(dialog), XtWindow(XtParent(dialog)));
}

Widget
BuildPopupMenu(parent, menu_title, items, menu_type)
Widget parent;
char *menu_title;
MenuItem *items;
int menu_type; /* XmMENU_POPUP, XmMENU_PULLDOWN */
{
    Widget shell, Popup, widget;
    int i;
    Arg args[2];

    XtSetArg(args[0], XmNwidth, 1);
    XtSetArg(args[1], XmNheight, 1);
    shell = XmCreateMenuShell(parent, "_popup", args, 2);
    XtSetArg(args[0], XmNrowColumnType,  menu_type);
    Popup = XmCreateRowColumn(shell, menu_title, args, 1);

    /* Now add the menu items */
    for (i = 0; items[i].label; i++) {
        if (items[i].subitems) {
            Widget new =
                BuildPopupMenu(Popup, items[i].label, items[i].subitems,
                    XmMENU_PULLDOWN);
            widget = XtVaCreateManagedWidget(items[i].label,
                xmCascadeButtonGadgetClass, Popup,
                XmNsubMenuId,   new,
                NULL);
        } else
            widget = XtVaCreateManagedWidget(items[i].label,
                *items[i].class, Popup, NULL);
        if (items[i].mnemonic)
            XtVaSetValues(widget, XmNmnemonic, items[i].mnemonic, NULL);
        if (items[i].accelerator) {
            XmString str = XmStringCreateSimple(items[i].accel_text);
            XtVaSetValues(widget,
                XmNaccelerator, items[i].accelerator,
                XmNacceleratorText, str,
                NULL);
            XmStringFree(str);
        }
        if (items[i].callback)
            XtAddCallback(widget, XmNactivateCallback,
                items[i].callback, items[i].callback_data);
    }
    return Popup;
}
