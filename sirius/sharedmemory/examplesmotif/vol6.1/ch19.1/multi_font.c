/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* multi_font.c --
 * Create three compound strings using 12, 14 and 18 point fonts
 * The user can specify resources so that each of the strings
 * use different fonts by setting resources similar to that shown
 * by the fallback resources.
 */
#include <Xm/Label.h>

String fallbacks[] = {
    "multi_font*fontList: \
        -*-courier-*-r-*--12-*=courier-12, \
        -*-courier-bold-o-*--14-*=courier-bold-14, \
        -*-courier-medium-r-*--18-*=courier-18",
    NULL
};

main(argc, argv)
int argc;
char *argv[];
{
    Widget        toplevel;
    XtAppContext  app;
    XmString      s1, s2, s3, text, tmp;
    String        string1 = "This is a string ",
                  string2 = "that contains three ",
                  string3 = "separate fonts.";

    toplevel = XtVaAppInitialize(&app, argv[0], NULL, 0,
        &argc, argv, fallbacks, NULL);

    s1 = XmStringCreate(string1, "courier-12");
    s2 = XmStringCreate(string2, "courier-bold-14");
    s3 = XmStringCreate(string3, "courier-18");

    /* concatenate the 3 strings on top of each other, but we can only
     * do two at a time.  So do s1 and s2 onto tmp and then do s3.
     */
    tmp = XmStringConcat(s1, s2);
    text = XmStringConcat(tmp, s3);

    XtVaCreateManagedWidget("widget_name",
        xmLabelWidgetClass, toplevel,
        XmNlabelString,     text,
        NULL);

    XmStringFree(s1);
    XmStringFree(s2);
    XmStringFree(s3);
    XmStringFree(tmp);
    XmStringFree(text);

    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}
