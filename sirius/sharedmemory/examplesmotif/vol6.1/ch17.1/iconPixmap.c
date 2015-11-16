/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

#include <Xm/Xm.h>

main(argc, argv)
int argc;
char *argv[];
{
    Widget toplevel;
    XtAppContext app;
    Screen *screen;
    Pixmap pixmap;

    toplevel = XtVaAppInitialize(&app, "Demos",
        NULL, 0, &argc, argv, NULL,
        XmNwidth, 100, /* size is irrelevant -- toplevel is iconified */
        XmNheight, 100, /* it just can't be 0, or Xt complains */
        NULL);

    screen = XtScreen(toplevel);
    pixmap = XmGetPixmap(screen, "mailfull",
        BlackPixelOfScreen(screen), WhitePixelOfScreen(screen));

    XtVaSetValues(toplevel,
        XmNiconPixmap, pixmap,
        XmNiconic,     True,
        NULL);
    
    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}
