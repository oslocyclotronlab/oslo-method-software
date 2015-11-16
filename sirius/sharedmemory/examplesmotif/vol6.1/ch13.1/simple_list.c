/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* simple_list.c -- introduce the List widget.  Lists present
 * a number of comound strings as choices.  Therefore, strings
 * must be converted before set in lists.  Also, the number of
 * visible items must be set or the List defaults to 1 item.
 */
#include <Xm/List.h>

char *months[] = {
    "January", "February", "March", "April", "May", "June", "July",
    "August", "September", "October", "November", "December"
};

main(argc, argv)
char *argv[];
{
    Widget           toplevel;
    XtAppContext     app;
    int              i, n = XtNumber(months);
    XmStringTable    str_list;

    toplevel = XtVaAppInitialize(&app, "Demos", NULL, 0,
        &argc, argv, NULL, NULL);

    str_list = (XmStringTable)XtMalloc(n * sizeof (XmString *));

    for (i = 0; i < n; i++)
        str_list[i] = XmStringCreateSimple(months[i]);

    XtVaCreateManagedWidget("Hello",
        xmListWidgetClass,     toplevel,
        XmNvisibleItemCount,   n,
        XmNitemCount,          n,
        XmNitems,              str_list,
        NULL);

    for (i = 0; i < n; i++)
        XmStringFree(str_list[i]);
    XtFree(str_list);

    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}
