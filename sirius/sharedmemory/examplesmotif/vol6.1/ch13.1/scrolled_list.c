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
    Widget           toplevel, list_w;
    XtAppContext     app;
    int              i, n = XtNumber(months);
    XmStringCharSet  charset = XmSTRING_DEFAULT_CHARSET;
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
        NULL);

    XtManageChild(list_w);

    for (i = 0; i < n; i++)
        XmStringFree(str_list[i]);
    XtFree(str_list);

    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}
