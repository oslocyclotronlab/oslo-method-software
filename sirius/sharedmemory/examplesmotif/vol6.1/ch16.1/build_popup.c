/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* build_popup.c -- Introduce the new function, BuildMenu(), which
 * can be used to build popup, pulldown -and- pullright menus.
 * Menus are defined by declaring an array of MenuItem structures.
 */
#include <Xm/RowColumn.h>
#include <Xm/MainW.h>
#include <Xm/DrawingA.h>
#include <Xm/CascadeBG.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>

typedef struct _menu_item {
    char        *label;         /* the label for the item */
    WidgetClass *class;         /* pushbutton, label, separator... */
    char         mnemonic;      /* mnemonic; NULL if none */
    char        *accelerator;   /* accelerator; NULL if none */
    char        *accel_text;    /* to be converted to compound string */
    void       (*callback)();   /* routine to call; NULL if none */
    XtPointer    callback_data; /* client_data for callback() */
    struct _menu_item *subitems; /* pullright menu items, if not NULL */
} MenuItem;

/* Build popup and pulldown menus, depending on the menu_type (which
 * may be either XmMENU_PULLDOWN or XmMENU_POPUP).  Pulldowns return
 * the CascadeButton that pops up the menu.  Popups return the menu.
 * Pulldown menus are built from cascade buttons, so this function
 * also includes pullright menus.  Create the menu, the cascade button
 * that owns the menu, and then the submenu items.
 */
Widget
BuildMenu(parent, menu_type, menu_title, menu_mnemonic, items)
Widget parent;
int menu_type;
char *menu_title, menu_mnemonic;
MenuItem *items;
{
    Widget menu, cascade, widget;
    int i;
    XmString str;

    if (menu_type == XmMENU_PULLDOWN)
	menu = XmCreatePulldownMenu(parent, "_pulldown", NULL, 0);
    else
	menu = XmCreatePopupMenu(parent, "_popup", NULL, 0);

    if (menu_type == XmMENU_PULLDOWN) {
	str = XmStringCreateSimple(menu_title);
	cascade = XtVaCreateManagedWidget(menu_title,
	    xmCascadeButtonGadgetClass, parent,
	    XmNsubMenuId,   menu,
	    XmNlabelString, str,
	    XmNmnemonic,    menu_mnemonic,
	    NULL);
	XmStringFree(str);
    }

    /* Now add the menu items */
    for (i = 0; items[i].label != NULL; i++) {
        /* If subitems exist, create the pull-right menu by calling this
         * function recursively.  Since the function returns a cascade
         * button, the widget returned is used..
         */
        if (items[i].subitems)
            widget = BuildMenu(menu, XmMENU_PULLDOWN,
                items[i].label, items[i].mnemonic, items[i].subitems);
        else
            widget = XtVaCreateManagedWidget(items[i].label,
                *items[i].class, menu,
                NULL);
        /* Whether the item is a real item or a cascade button with a
         * menu, it can still have a mnemonic.
         */
        if (items[i].mnemonic)
            XtVaSetValues(widget, XmNmnemonic, items[i].mnemonic, NULL);
        /* any item can have an accelerator, except cascade menus. But,
         * we don't worry about that; we know better in our declarations.
         */
        if (items[i].accelerator) {
            str = XmStringCreateSimple(items[i].accel_text);
            XtVaSetValues(widget,
                XmNaccelerator, items[i].accelerator,
                XmNacceleratorText, str,
                NULL);
            XmStringFree(str);
        }
        /* Anyone can have a callback -- however, this is an
         * activate-callback.  This may not be appropriate for all items.
         */
        if (items[i].callback)
            XtAddCallback(widget, XmNactivateCallback,
                items[i].callback, items[i].callback_data);
    }
    return menu_type == XmMENU_POPUP? menu : cascade;
}

/* callback functions for menu items declared later... */
void
set_weight(widget, weight)
Widget widget;
int weight;
{
    printf("Setting line weight to %d\n", weight);
}

void
set_color(widget, color)
Widget widget;
char *color;
{
    printf("Setting color to %s\n", color);
}

void
set_dot_dash(widget, dot_or_dash)
Widget widget;
int dot_or_dash;
{
    printf("Setting line style to %s\n", dot_or_dash? "dash" : "dot");
}

MenuItem weight_menu[] = {
    { " 1 ", &xmPushButtonGadgetClass, '1', NULL, NULL,
        set_weight, (XtPointer)1, (MenuItem *)NULL },
    { " 2 ", &xmPushButtonGadgetClass, '2', NULL, NULL,
        set_weight, (XtPointer)2, (MenuItem *)NULL },
    { " 3 ", &xmPushButtonGadgetClass, '3', NULL, NULL,
        set_weight, (XtPointer)3, (MenuItem *)NULL },
    { " 4 ", &xmPushButtonGadgetClass, '4', NULL, NULL,
        set_weight, (XtPointer)4, (MenuItem *)NULL },
    NULL,
};

MenuItem color_menu[] = {
    { "Cyan", &xmPushButtonGadgetClass, 'C', "Meta<Key>C", "Meta+C",
        set_color, "cyan", (MenuItem *)NULL },
    { "Yellow", &xmPushButtonGadgetClass, 'Y', "Meta<Key>Y", "Meta+Y",
        set_color, "yellow", (MenuItem *)NULL },
    { "Magenta", &xmPushButtonGadgetClass, 'M', "Meta<Key>M", "Meta+M",
        set_color, "magenta", (MenuItem *)NULL },
    { "Black", &xmPushButtonGadgetClass, 'B', "Meta<Key>B", "Meta+B",
        set_color, "black", (MenuItem *)NULL },
    NULL,
};

MenuItem style_menu[] = {
    { "Dash", &xmPushButtonGadgetClass, 'D', NULL, NULL,
        set_dot_dash, (XtPointer)0, (MenuItem *)NULL },
    { "Dot",  &xmPushButtonGadgetClass, 'o', NULL, NULL,
        set_dot_dash, (XtPointer)1, (MenuItem *)NULL },
    NULL,
};

MenuItem drawing_menus[] = {
    { "Line Weight", &xmCascadeButtonGadgetClass, 'W', NULL, NULL,
        0, 0, weight_menu },
    { "Line Color", &xmCascadeButtonGadgetClass, 'C', NULL, NULL,
        0, 0, color_menu },
    { "Line Style", &xmCascadeButtonGadgetClass, 'S', NULL, NULL,
        0, 0, style_menu },
    NULL,
};

main(argc, argv)
int argc;
char *argv[];
{
    Widget toplevel, main_w, drawing_a, menu;
    void input();
    XtAppContext app;

    toplevel = XtVaAppInitialize(&app, "Demos", NULL, 0,
        &argc, argv, NULL, NULL);

    /* Create a MainWindow widget that contains a DrawingArea in
     * its work window. (This happens by default.)
     */
    main_w = XtVaCreateManagedWidget("main_w",
        xmMainWindowWidgetClass, toplevel,
        XmNscrollingPolicy,  XmAUTOMATIC,
        NULL);

    /* Create a DrawingArea -- no actual drawing will be done. */
    drawing_a = XtVaCreateManagedWidget("drawing_a",
        xmDrawingAreaWidgetClass, main_w,
        XmNwidth, 500,
        XmNheight, 500,
        NULL);

    menu = BuildMenu(drawing_a, XmMENU_POPUP, "Lines", 'L', drawing_menus);

    XtAddCallback(drawing_a, XmNinputCallback, input, menu);

    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}

void
input(widget, menu, cbs)
Widget widget;
Widget menu;
XmDrawingAreaCallbackStruct *cbs;
{
    if (cbs->event->xany.type == ButtonPress &&
	cbs->event->xbutton.button == 3) {
	XmMenuPosition(menu, (XButtonPressedEvent *)(cbs->event));
	XtManageChild(menu);
    }
}
