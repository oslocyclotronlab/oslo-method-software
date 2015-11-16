/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* alpha_list.c -- insert items into a list in alphabetical order.  */

#include <Xm/List.h>
#include <Xm/RowColumn.h>
#include <Xm/TextF.h>

XmStringCharSet charset = XmSTRING_DEFAULT_CHARSET;

main(argc, argv)
char *argv[];
{
    Widget        toplevel, rowcol, list_w, text_w;
    XtAppContext  app;
    Arg           args[1];
    void          add_item();

    toplevel = XtVaAppInitialize(&app, "Demos", NULL, 0,
        &argc, argv, NULL, NULL);

    rowcol = XtVaCreateWidget("rowcol",
        xmRowColumnWidgetClass, toplevel, NULL);

    XtSetArg(args[0], XmNvisibleItemCount, 5);
    list_w = XmCreateScrolledList(rowcol, "scrolled_list", args, 1);
    XtManageChild(list_w);

    text_w = XtVaCreateManagedWidget("text",
        xmTextFieldWidgetClass, rowcol,
        XmNcolumns,     25,
        NULL);
    XtAddCallback(text_w, XmNactivateCallback, add_item, list_w);

    XtManageChild(rowcol);
    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}

/* Add item to the list in alphabetical order.  Perform binary
 * search to find the correct location for the new item position.
 * This is the callback routine for the TextField widget.
 */
void
add_item(text_w, list_w)
Widget text_w, list_w; /* list_w is the callback data */
{
    char *text, *newtext = XmTextFieldGetString(text_w);
    XmString str, *strlist;
    int u_bound, l_bound = 0;

    /* newtext is the text typed in the TextField widget */
    if (!newtext || !*newtext) {
        /* non-null strings must be entered */
        XtFree(newtext); /* XtFree() checks for NULL */
        return;
    }
    /* get the current entries (and number of entries) from the List */
    XtVaGetValues(list_w,
        XmNitemCount, &u_bound,
        XmNitems,     &strlist,
        NULL);
    u_bound--;
    /* perform binary search */
    while (u_bound >= l_bound) {
        int i = l_bound + (u_bound - l_bound)/2;
        /* convert the compound string into a regular C string */
        if (!XmStringGetLtoR(strlist[i], charset, &text))
            break;
        if (strcmp(text, newtext) > 0)
            u_bound = i-1; /* newtext comes before item */
        else
            l_bound = i+1; /* newtext comes after item */
        XtFree(text); /* XmStringGetLtoR() allocates memory ... yuk */
    }
    str = XmStringCreateSimple(newtext); /* convert text to compound string */
    XtFree(newtext);
    /* positions indexes start at 1, so increment accordingly */
    XmListAddItemUnselected(list_w, str, l_bound+1);
    XmStringFree(str);
    XmTextFieldSetString(text_w, "");
}
