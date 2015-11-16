/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* search_list.c -- search for items in a List and select them */

#include <stdio.h>
#include <Xm/List.h>
#include <Xm/LabelG.h>
#include <Xm/Label.h>
#include <Xm/RowColumn.h>
#include <Xm/PanedW.h>
#include <Xm/TextF.h>

XmStringCharSet charset = XmSTRING_DEFAULT_CHARSET;

main(argc, argv)
char *argv[];
{
    Widget        toplevel, rowcol, list_w, text_w;
    XtAppContext  app;
    Arg           args[2];
    XmString      label;
    void          add_item(), search_item();

    toplevel = XtVaAppInitialize(&app, "Demos", NULL, 0,
        &argc, argv, NULL, NULL);

    rowcol = XtVaCreateWidget("rowcol",
        xmPanedWindowWidgetClass, toplevel, NULL);

    label = XmStringCreateSimple("List:");
    XtVaCreateManagedWidget("list_lable", xmLabelWidgetClass, rowcol,
        XmNlabelString,  label,
        NULL);
    XmStringFree(label);
    XtSetArg(args[0], XmNvisibleItemCount, 10);
    XtSetArg(args[1], XmNselectionPolicy, XmEXTENDED_SELECT);
    list_w = XmCreateScrolledList(rowcol, "scrolled_list", args, 2);
    XtManageChild(list_w);

    label = XmStringCreateSimple("Add:");
    XtVaCreateManagedWidget("add_label", xmLabelWidgetClass, rowcol,
        XmNlabelString,  label,
        NULL);
    XmStringFree(label);
    text_w = XtVaCreateManagedWidget("add_text",
        xmTextFieldWidgetClass, rowcol,
        XmNcolumns,     25,
        NULL);
    XtAddCallback(text_w, XmNactivateCallback, add_item, list_w);

    label = XmStringCreateSimple("Search:");
    XtVaCreateManagedWidget("search_label", xmLabelWidgetClass, rowcol,
        XmNlabelString,  label,
        NULL);
    XmStringFree(label);
    text_w = XtVaCreateManagedWidget("search_text",
        xmTextFieldWidgetClass, rowcol,
        XmNcolumns,     25,
        NULL);
    XtAddCallback(text_w, XmNactivateCallback, search_item, list_w);

    XtManageChild(rowcol);
    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}

/* Add item to the list in alphabetical order.  Perform binary
 * search to find the correct location for the new item position.
 * This is the callback routine for the Add: TextField widget.
 */
void
add_item(text_w, list_w)
Widget text_w, list_w; /* list_w is the callback data */
{
    char *text, *newtext = XmTextFieldGetString(text_w);
    XmString str, *strlist;
    int u_bound, l_bound = 0;

    if (!newtext || !*newtext) {
        /* non-null strings must be entered */
        XtFree(newtext);
        return;
    }
    XtVaGetValues(list_w,
        XmNitemCount, &u_bound,
        XmNitems,     &strlist,
        NULL);
    u_bound--;
    /* perform binary search */
    while (u_bound >= l_bound) {
        int i = l_bound + (u_bound - l_bound)/2;
        if (!XmStringGetLtoR(strlist[i], "", &text))
            break;
        if (strcmp(text, newtext) > 0)
            u_bound = i-1; /* newtext comes before item */
        else
            l_bound = i+1; /* newtext comes after item */
        XtFree(text);
    }
    str = XmStringCreateSimple(newtext);
    XtFree(newtext);
    /* positions indexes start at 1, so increment accordingly */
    XmListAddItemUnselected(list_w, str, l_bound+1);
    XmStringFree(str);
    XmTextFieldSetString(text_w, "");
}

/* find the item in the list that matches the specified pattern */
void
search_item(text_w, list_w)
Widget text_w, list_w; /* list_w is the callback data */
{
    char *exp, *text, *newtext = XmTextFieldGetString(text_w);
    XmString *strlist, *selectlist = NULL;
    int cnt, j = 0;
    extern char *re_comp();

    if (!newtext || !*newtext) {
        /* non-null strings must be entered */
        XtFree(newtext);
        return;
    }

    /* compile expression into pattern matching library */
#ifdef SYSV
    if (!(exp = regcmp(newtext, NULL))) {
        printf("error with regcmp(%s)\n", newtext);
        return;
    }
#else /* BSD */
    if (exp = re_comp(newtext)) {
        printf("error with re_comp(%s): %s\n", newtext, exp);
        return;
    }
#endif /* SYSV */

    /* get all the items in the list ... we're going to search each one */
    XtVaGetValues(list_w,
        XmNitemCount, &cnt,
        XmNitems,     &strlist,
        NULL);
    while (cnt--) {
        /* convert item to C string */
        if (!XmStringGetLtoR(strlist[cnt], charset, &text))
            break;
        /* do pattern match against search string */
#ifdef SYSV
        /* returns NULL if match failed */
        if (regex(exp, text, NULL)) {
            /* if selection matches, realloc list to contain item */
            selectlist = (XmString *)XtRealloc(selectlist, j+1);
            selectlist[j++] = XmStringCopy(strlist[cnt]);
        }
#else /* BSD */
        /* -1 on error, 0 if no-match, 1 if match */
        if (re_exec(text) > 0) {
            /* if selection matches, realloc list to contain item */
            selectlist = (XmString *)XtRealloc(selectlist, j+1);
            selectlist[j++] = XmStringCopy(strlist[cnt]);
        }
#endif /* SYSV */
        XtFree(text);
    }
#ifdef SYSV
    free(exp);  /* this must be freed for regcmp() */
#endif /* SYSV */
    XtFree(newtext);
    /* set the actual selected items to be those that matched */
    XtVaSetValues(list_w,
        XmNselectedItems,     selectlist,
        XmNselectedItemCount, j,
        NULL);
    while (j--)
        XmStringFree(selectlist[j]);
    XmTextFieldSetString(text_w, "");
}
