/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* fontlist.c -- demonstrate how to create, add to, and destroy
 * font lists.  The fonts and text displayed are hardcoded in
 * this program and cannot be overriden by user resources.
 */
#include <Xm/Label.h>

main(argc, argv)
char *argv[];
{
    Widget        toplevel;
    XtAppContext  app;
    XmString      s1, s2, s3, text, tmp;
    XmFontList    fontlist;
    XFontStruct   *font1, *font2, *font3;
    String        string1 = "This is a string",
                  string2 = "that contains three",
                  string3 = "separate fonts.";

    toplevel = XtVaAppInitialize(&app, "Demos", NULL, 0,
        &argc, argv, NULL, NULL);

    font1 = XLoadQueryFont(XtDisplay(toplevel), "-*-courier-*-r-*--12-*");
    font2 = XLoadQueryFont(XtDisplay(toplevel), "-*-courier-bold-o-*--14-*");
    font3 = XLoadQueryFont(XtDisplay(toplevel), "-*-courier-medium-r-*--18-*");
    fontlist = XmFontListCreate(font1, "charset1");
    fontlist = XmFontListAdd(fontlist, font2, "charset2");
    fontlist = XmFontListAdd(fontlist, font3, "charset3");

    s1 = XmStringCreate(string1, "charset1");
    s2 = XmStringCreate(string2, "charset2");
    s3 = XmStringCreate(string3, "charset3");

    /* concatenate the 3 strings on top of each other, but we can only
     * do two at a time.  So do s1 and s2 onto tmp and then do s3.
     */
    tmp = XmStringConcat(s1, s2);
    text = XmStringConcat(tmp, s3);

    XtVaCreateManagedWidget("widget_name", xmLabelWidgetClass, toplevel,
        XmNlabelString,     text,
        XmNfontList,        fontlist,
        NULL);

    XmStringFree(s1);
    XmStringFree(s2);
    XmStringFree(s3);
    XmStringFree(tmp);
    XmStringFree(text);
    XmFontListFree(fontlist);

    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}
