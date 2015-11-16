/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* list_callback.c -- introduce callback functions for List widgets.
 */
#include <Xm/List.h>

char *months[] = {
    "January", "February", "March", "April", "May", "June", "July",
    "August", "September", "October", "November", "December"
};

XmStringCharSet  charset = XmSTRING_DEFAULT_CHARSET;

static void
my_proc(list_w, client_data, cbs)
Widget list_w;
XtPointer client_data;
XmListCallbackStruct *cbs; /* CallBack Structure */
{
    char *choice;
    int   i;

    printf("reason: %d\n", cbs->reason);
    if (cbs->reason == XmCR_EXTENDED_SELECT) {
        if (cbs->selection_type == XmINITIAL)
            printf("Initial Selection: ");
        else if (cbs->selection_type == XmMODIFICATION)
            printf("Modification of selection: ");
        else /* selection type = XmADDITION */
            printf("Additional selections: ");
        printf("%d items selected:\n", cbs->selected_item_count);
        for (i = 0; i < cbs->selected_item_count; i++) {
            XmStringGetLtoR(cbs->selected_items[i], charset, &choice);
            printf("%d (%s)\n", cbs->selected_item_positions[i], choice);
            XtFree(choice);
        }
    } else {
        XmStringGetLtoR(cbs->item, charset, &choice);
        printf("%d (%s)\n", cbs->item_position, choice);
        XtFree(choice);
    }
}

main(argc, argv)
char *argv[];
{
    Widget           toplevel, list_w;
    XtAppContext     app;
    int              i, n = XtNumber(months);
    XmStringTable    str_list;
    Arg              args[4];

    toplevel = XtVaAppInitialize(&app, "Demos", NULL, 0,
        &argc, argv, NULL, NULL);

    str_list = (XmStringTable)XtMalloc(n * sizeof (XmString *));

    for (i = 0; i < n; i++)
        str_list[i] = XmStringCreateLtoR(months[i], charset);

    list_w = XmCreateScrolledList(toplevel, "months", NULL, 0);
    XtVaSetValues(list_w,
        XmNitems,            str_list,
        XmNitemCount,        n,
        XmNvisibleItemCount, 5,
        XmNselectionPolicy,  XmEXTENDED_SELECT,
        NULL);
    XtAddCallback(list_w, XmNextendedSelectionCallback, my_proc, NULL);
    XtAddCallback(list_w, XmNdefaultActionCallback, my_proc, NULL);

    XtManageChild(list_w);

    for (i = 0; i < n; i++)
        XmStringFree(str_list[i]);
    XtFree(str_list);

    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}
