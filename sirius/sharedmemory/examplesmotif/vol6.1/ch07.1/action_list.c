/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

#include <Xm/DialogS.h>
#include <Xm/PushBG.h>
#include <Xm/PushB.h>
#include <Xm/LabelG.h>
#include <Xm/PanedW.h>
#include <Xm/Form.h>
#include <Xm/List.h>

typedef struct {
    char *label;
    void (*callback)();
    caddr_t data;
} ActionAreaItem;

static void
    activate_cb(), ok_pushed(), close_dialog(),
    do_dialog(), cancel_pushed(), help();

void SendExpose();
Widget CreateActionArea();

/* specify fallback resources -- stuff we don't want to hard-code */
String fallbacks[] = {
    /* Careful here!  |  Be sure to use a * or specify List's parent.
     * Scrolled List  |  parents are ScrolledWindows; they are unseen
     * because Motif  V  creates the ScrolledWindow silently. */
    "*dialog.pane.form*color_list.visibleItemCount: 5",
    "*dialog.pane.form*color_list.items: \
	DarkSlateBlue, coral, DimGrey, ForestGreen, LimeGreen, \
	Gold, Yellow, Maroon, Magenta, Khaki, Violet, White, \
	Black, Brown, Red, Orange, Wheat, Pink",
    "*dialog.pane.form*color_list.itemCount: 18",
    NULL
};

main(argc, argv)
int argc;
char *argv[];
{
    Widget toplevel, button;
    XtAppContext app;

    toplevel = XtVaAppInitialize(&app, "Demo",
	NULL, 0, &argc, argv, fallbacks, NULL);

    button = XtVaCreateManagedWidget("Push Me",
	xmPushButtonWidgetClass, toplevel, NULL);
    XtAddCallback(button, XmNactivateCallback, do_dialog, NULL);

    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}

/* callback routine for "Push Me" button.  Actually, this represents
 * a function that could be invoked by any arbitrary callback.  Here,
 * we demonstrate how one can build a standard customized dialog box.
 * The control area is created here and the action area is created in
 * a separate, generic routine: CreateActionArea().
 */
static void
do_dialog(w, file)
Widget w; /* will act as dialog's parent */
char *file;
{
    Widget dialog, pane, form, label, list_w, action_a;
    Arg args[10];
    static ActionAreaItem action_items[] = {
	{ "Ok",     ok_pushed,     NULL         },
	{ "Cancel", cancel_pushed, NULL         },
	{ "Close",  close_dialog,  NULL         },
	{ "Help",   help,          "Help Button" },
    };

    /* The DialogShell is the Shell for this dialog.  Set it up so
     * that the "Close" button in the window manager's system menu
     * destroys the shell (it only unmaps it by default).
     */
    dialog = XtVaCreatePopupShell("dialog",
	xmDialogShellWidgetClass, XtParent(w),
	XmNtitle,  "Dialog Shell",
	XmNdeleteResponse, XmDESTROY,  /* system menu "Close" action */
	XmNx,  400,
	XmNy,  400,
	NULL);

    /* now that the dialog is created, set the Close button's
     * client data, so close_dialog() will know what to destroy.
     */
    action_items[2].data = (caddr_t)dialog;

    /* Create the paned window as a child of the dialog.  This will
     * contain the control area (a Form widget) and the action area
     * (created by CreateActionArea() using the action_items above).
     */
    pane = XtVaCreateWidget("pane", xmPanedWindowWidgetClass, dialog,
	XmNsashWidth,  1, /* try to "hide" resizing sash.  PanedWindow */
	XmNsashHeight, 1, /* won't let us set to 0, so 1 will have to do */
	NULL);

    /* create the control area (Form) which contains a
     * Label gadget and a List widget.
     */
    form = XtVaCreateWidget("form", xmFormWidgetClass, pane, NULL);
    label = XtVaCreateManagedWidget("label", xmLabelGadgetClass, form,
	XmNleftAttachment, XmATTACH_FORM,
	XmNtopAttachment,  XmATTACH_FORM,
	XtVaTypedArg, XmNlabelString, XmRString, "Select Color:", 12,
	NULL);

    /* Created a ScrolledList */
    XtSetArg(args[0], XmNscrollingPolicy, XmAUTOMATIC);
    XtSetArg(args[1], XmNselectionPolicy, XmBROWSE_SELECT);
    XtSetArg(args[2], XmNlistSizePolicy, XmRESIZE_IF_POSSIBLE);
    XtSetArg(args[3], XmNleftAttachment, XmATTACH_FORM);
    XtSetArg(args[4], XmNrightAttachment, XmATTACH_FORM);
    XtSetArg(args[5], XmNtopAttachment, XmATTACH_WIDGET);
    XtSetArg(args[6], XmNtopWidget, label);
    XtSetArg(args[7], XmNbottomAttachment, XmATTACH_FORM);
    list_w = XmCreateScrolledList(form, "color_list", args, 8);
    /* Motif convenience routines don't manage the widgets they create */
    XtManageChild(list_w);

    /* Form is full -- now manage */
    XtManageChild(form);

    /* Set the client data "Ok" and "Cancel" button's callbacks. */
    action_items[0].data = (caddr_t)list_w;
    action_items[1].data = (caddr_t)list_w;

    /* Create the action area -- we don't need the widget it returns. */
    action_a = CreateActionArea(pane, action_items, XtNumber(action_items));
    /* callback for double-click on List widget.  Use action_a as data */
    XtAddCallback(list_w, XmNdefaultActionCallback, activate_cb, action_a);

    XtManageChild(pane);
    XtPopup(dialog, XtGrabNone);
}

/*--------------*/
/* The next three functions are the callback routines for the buttons
 * in the action area for the dialog created above.  Again, they are
 * simple examples, yet they demonstrate the fundamental design approach.
 */
static void
close_dialog(w, shell)
Widget w, shell;
{
    XtDestroyWidget(shell);
}

/* The "ok" button was pushed, or the List was double-clicked */
static void
ok_pushed(w, list_w, cbs)
Widget w, list_w;         /* the list widget is the client data */
XmAnyCallbackStruct *cbs;
{
    char *color;
    XmString *selected_color;

    if (DefaultDepthOfScreen(XtScreen(w)) < 2)
	return;

    /* we know there can only be one color selected */
    XtVaGetValues(list_w, XmNselectedItems, &selected_color, NULL);
    if (!selected_color)
	return;

    /* convert from compound string to C string */
    XmStringGetLtoR(selected_color[0], XmSTRING_DEFAULT_CHARSET, &color);
    /* free the compound string and the array pointer */
    XmStringFree(selected_color[0]);
    XtFree(selected_color);
    if (!color)
	return;

    /* set the list widget's color */
    XtVaSetValues(list_w,
	XtVaTypedArg, XmNforeground, XmRString, color, strlen(color)+1,
	NULL);
    /* free the color now that we're done with it. */
    XtFree(color);
    SendExpose(list_w);
}

static void
cancel_pushed(w, list_w, cbs)
Widget w, list_w;         /* the list widget is the client data */
XmAnyCallbackStruct *cbs;
{
    /* cancel the whole operation; reset to black.  Of course, this
     * is making gross assumptions on our part, but you get the idea
     * of what we're trying to do.
     */
    XtVaSetValues(list_w,
	XmNforeground, BlackPixelOfScreen(XtScreen(list_w)),
	NULL);
    SendExpose(list_w);
}

void
SendExpose(w)
Widget w;
{
    XExposeEvent event;

    event.type = Expose;
    event.send_event = True;
    event.display = XtDisplay(w);
    event.window = XtWindow(w);
    event.count = 1;
    XSendEvent(event.display, event.window, False, ExposureMask, &event);
}

static void
help(w, string)
Widget w;
String string;
{
    puts(string);
}
/*--------------*/

/* When Return pressed in TextField widget, respond by getting the
 * designated "default button" in the action area and activate it
 * as if the user had selected it.
 */
void
activate_cb(list_w, client_data, cbs)
Widget list_w;              /* user double clicked in this widget */
XtPointer client_data;        /* actino_area passed as client data */
XmAnyCallbackStruct *cbs;   /* borrow the "event" field from this */
{
    Widget dflt, action_area = (Widget)client_data;

    XtVaGetValues(action_area, XmNdefaultButton, &dflt, NULL);
    if (dflt) /* sanity check -- this better work */
	/* make the default button think it got pushed.  This causes
	 * "ok_pushed" to be called, but XtCallActionProc() causes
	 * the button appear to activated as if the user selected it.
	 */
	XtCallActionProc(dflt, "ArmAndActivate", cbs->event, NULL, 0);
}

#define TIGHTNESS 20

Widget
CreateActionArea(parent, actions, num_actions)
Widget parent;
ActionAreaItem *actions;
int num_actions;
{
    Widget action_area, widget;
    int i;

    action_area = XtVaCreateWidget("action_area", xmFormWidgetClass, parent,
	XmNfractionBase, TIGHTNESS*num_actions - 1,
	XmNleftOffset,   10,
	XmNrightOffset,  10,
	NULL);

    for (i = 0; i < num_actions; i++) {
	widget = XtVaCreateManagedWidget(actions[i].label,
	    xmPushButtonWidgetClass, action_area,
	    XmNleftAttachment,	     i? XmATTACH_POSITION : XmATTACH_FORM,
	    XmNleftPosition,         TIGHTNESS*i,
	    XmNtopAttachment,        XmATTACH_FORM,
	    XmNbottomAttachment,     XmATTACH_FORM,
	    XmNrightAttachment,
		    i != num_actions-1? XmATTACH_POSITION : XmATTACH_FORM,
	    XmNrightPosition,        TIGHTNESS*i + (TIGHTNESS-1),
	    XmNshowAsDefault,        i == 0,
	    XmNdefaultButtonShadowThickness, 1,
	    NULL);
	if (actions[i].callback)
	    XtAddCallback(widget, XmNactivateCallback,
		actions[i].callback, actions[i].data);
	if (i == 0) {
	    /* Set the action_area's default button to the first widget
	     * created (or, make the index a parameter to the function
	     * or have it be part of the data structure). Also, set the
	     * pane window constraint for max and min heights so this
	     * pane in the widget is not resizable.
	     */
	    Dimension height, h;
	    XtVaGetValues(action_area, XmNmarginHeight, &h, NULL);
	    XtVaGetValues(widget, XmNheight, &height, NULL);
	    height += 2 * h;
	    XtVaSetValues(action_area,
		XmNdefaultButton, widget,
		XmNpaneMaximum,   height,
		XmNpaneMinimum,   height,
		NULL);
	}
    }

    XtManageChild(action_area);

    return action_area;
}
