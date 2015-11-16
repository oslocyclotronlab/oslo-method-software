/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* build_option.c -- The final version of BuildMenu() is used to
 * build popup, option, pulldown -and- pullright menus.  Menus are
 * defined by declaring an array of MenuItem structures as usual.
 */
#include <Xm/MainW.h>
#include <Xm/PanedW.h>
#include <Xm/RowColumn.h>
#include <Xm/DrawingA.h>
#include <Xm/CascadeBG.h>
#include <Xm/ToggleB.h>
#include <Xm/ToggleBG.h>
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

/* Build popup, option and pulldown menus, depending on the menu_type.
 * It may be XmMENU_PULLDOWN, XmMENU_OPTION or  XmMENU_POPUP.  Pulldowns
 * return the CascadeButton that pops up the menu.  Popups return the menu.
 * Option menus are created, but the RowColumn that acts as the option
 * "area" is returned unmanaged. (The user must manage it.)
 * Pulldown menus are built from cascade buttons, so this function
 * also builds pullright menus.  The function also adds the right
 * callback for PushButton or ToggleButton menu items.
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

    if (menu_type == XmMENU_PULLDOWN || menu_type == XmMENU_OPTION)
        menu = XmCreatePulldownMenu(parent, "_pulldown", NULL, 0);
    else if (menu_type == XmMENU_POPUP)
        menu = XmCreatePopupMenu(parent, "_popup", NULL, 0);
    else {
        XtWarning("Invalid menu type passed to BuildMenu()");
        return NULL;
    }

    /* Pulldown menus require a cascade button to be made */
    if (menu_type == XmMENU_PULLDOWN) {
        str = XmStringCreateSimple(menu_title);
        cascade = XtVaCreateManagedWidget(menu_title,
            xmCascadeButtonGadgetClass, parent,
            XmNsubMenuId,   menu,
            XmNlabelString, str,
            XmNmnemonic,    menu_mnemonic,
            NULL);
        XmStringFree(str);
    } else if (menu_type == XmMENU_OPTION) {
        /* Option menus are a special case, but not hard to handle */
        Arg args[2];
        str = XmStringCreateSimple(menu_title);
        XtSetArg(args[0], XmNsubMenuId, menu);
        XtSetArg(args[1], XmNlabelString, str);
        /* This really isn't a cascade, but this is the widget handle
         * we're going to return at the end of the function.
         */
        cascade = XmCreateOptionMenu(parent, menu_title, args, 2);
        XmStringFree(str);
    }

    /* Now add the menu items */
    for (i = 0; items[i].label != NULL; i++) {
        /* If subitems exist, create the pull-right menu by calling this
         * function recursively.  Since the function returns a cascade
         * button, the widget returned is used..
         */
        if (items[i].subitems)
            if (menu_type == XmMENU_OPTION) {
                XtWarning("You can't have submenus from option menu items.");
                continue;
            } else
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

        if (items[i].callback)
            XtAddCallback(widget,
                (items[i].class == &xmToggleButtonWidgetClass ||
                 items[i].class == &xmToggleButtonGadgetClass)?
                    XmNvalueChangedCallback : /* ToggleButton class */
                    XmNactivateCallback,      /* PushButton class */
                items[i].callback, items[i].callback_data);
    }

    /* for popup menus, just return the menu; pulldown menus, return
     * the cascade button; option menus, return the thing returned
     * from XmCreateOptionMenu().  This isn't a menu, or a cascade button!
     */
    return menu_type == XmMENU_POPUP? menu : cascade;
}

/* option menu items... quite simple */
MenuItem drawing_shapes[] = {
    { "Lines", &xmPushButtonGadgetClass, 'L', NULL, NULL, 0, 0, NULL },
    { "Circles", &xmPushButtonGadgetClass, 'C', NULL, NULL, 0, 0, NULL },
    { "Squares", &xmPushButtonGadgetClass, 'S', NULL, NULL, 0, 0, NULL },
    NULL,
};

main(argc, argv)
int argc;
char *argv[];
{
    Widget toplevel, main_w, pane, sw, drawing_a, menu, option_menu;
    void input();
    XtAppContext app;
    XtWidgetGeometry geom;

    toplevel = XtVaAppInitialize(&app, "Demos", NULL, 0,
        &argc, argv, NULL, NULL);

    /* Create a MainWindow widget that contains a DrawingArea in
     * its work window. (This happens by default.)
     */
    main_w = XtVaCreateManagedWidget("main_w",
        xmMainWindowWidgetClass, toplevel, NULL);

    /* Use a PanedWindow widget as the work area of the main window */
    pane = XtVaCreateWidget("pane", xmPanedWindowWidgetClass, main_w, NULL);

    /* create the option menu --  don't froget to manage it. */
    option_menu =
        BuildMenu(pane, XmMENU_OPTION, "Shapes", 'S', drawing_shapes);
    XtManageChild(option_menu);

    /* Set the OptionMenu (the RowColumn part) so that it can't be resized! */
    geom.request_mode = CWHeight;
    XtQueryGeometry(option_menu, NULL, &geom);
    XtVaSetValues(option_menu,
        XmNpaneMinimum, geom.height,
        XmNpaneMaximum, geom.height,
        NULL);

    /* The scrolled window (which contains the drawing area) is a child
     * of the PanedWindow; it's sibling, the option menu, cannot be resized,
     * so if the user resizes the toplevel shell, *this* window will resize.
     */
    sw = XtVaCreateManagedWidget("sw", xmScrolledWindowWidgetClass, pane,
        XmNscrollingPolicy,  XmAUTOMATIC,
        NULL);
    /* Create a DrawingArea -- no actual drawing will be done. */
    drawing_a = XtVaCreateManagedWidget("drawing_a",
        xmDrawingAreaWidgetClass, sw,
        XmNwidth, 500,
        XmNheight, 500,
        NULL);

    XtManageChild(pane);
    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}
