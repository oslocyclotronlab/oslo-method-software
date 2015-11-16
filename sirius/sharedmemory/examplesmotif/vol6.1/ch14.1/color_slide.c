/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* color_slide.c -- Use scale widgets to display the different
 * colors of a colormap.
 */
#include <Xm/LabelG.h>
#include <Xm/Scale.h>
#include <Xm/RowColumn.h>
#include <Xm/DrawingA.h>

Widget colorwindow; /* the window the siplays a solid color */
XColor color;       /* the color in the colorwindow */

main(argc, argv)
char *argv[];
{
    Widget        toplevel, rowcol, scale;
    XtAppContext  app;
    Pixel         background;
    void          new_value();
    XtVarArgsList arglist;

    toplevel = XtVaAppInitialize(&app, "Demos", NULL, 0,
        &argc, argv, NULL, NULL);

    if (DefaultDepthOfScreen(XtScreen(toplevel)) < 2) {
        puts("You must be using a color screen.");
        exit(1);
    }

    color.flags = DoRed|DoGreen|DoBlue;
    /* initialize first color */
    XAllocColor(XtDisplay(toplevel),
        DefaultColormapOfScreen(XtScreen(toplevel)), &color);

    rowcol = XtVaCreateManagedWidget("rowcol",
        xmRowColumnWidgetClass, toplevel, NULL);

    colorwindow = XtVaCreateManagedWidget("colorwindow",
        widgetClass,   rowcol,
        XmNheight,     100,
        XmNbackground, color.pixel,
        NULL);

    /* use rowcol again to create another RowColumn under the 1st */
    rowcol = XtVaCreateWidget("rowcol", xmRowColumnWidgetClass, rowcol,
        XmNorientation, XmHORIZONTAL,
        NULL);

    arglist = XtVaCreateArgsList(NULL,
        XmNshowValue, True,
        XmNmaximum, 255,
        XmNscaleMultiple, 5,
        NULL);

    scale = XtVaCreateManagedWidget("Red",
        xmScaleWidgetClass, rowcol,
        XtVaNestedList, arglist,
        XtVaTypedArg, XmNtitleString, XmRString, "Red", 4,
        XtVaTypedArg, XmNforeground, XmRString, "Red", 4,
        NULL);
    XtAddCallback(scale, XmNdragCallback, new_value, DoRed);
    XtAddCallback(scale, XmNvalueChangedCallback, new_value, DoRed);

    scale = XtVaCreateManagedWidget("Green",
        xmScaleWidgetClass, rowcol,
        XtVaNestedList, arglist,
        XtVaTypedArg, XmNtitleString, XmRString, "Green", 6,
        XtVaTypedArg, XmNforeground, XmRString, "Green", 6,
        NULL);
    XtAddCallback(scale, XmNdragCallback, new_value, DoGreen);
    XtAddCallback(scale, XmNvalueChangedCallback, new_value, DoGreen);

    scale = XtVaCreateManagedWidget("Blue",
        xmScaleWidgetClass, rowcol,
        XtVaNestedList, arglist,
        XtVaTypedArg, XmNtitleString, XmRString, "Blue", 5,
        XtVaTypedArg, XmNforeground, XmRString, "Blue", 5,
        NULL);
    XtAddCallback(scale, XmNdragCallback, new_value, DoBlue);
    XtAddCallback(scale, XmNvalueChangedCallback, new_value, DoBlue);

    XtFree(arglist);

    XtManageChild(rowcol);

    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}

void
new_value(scale_w, rgb, cbs)
Widget scale_w;
int rgb;
XmScaleCallbackStruct *cbs;
{
    Colormap cmap = DefaultColormapOfScreen(XtScreen(scale_w));

    switch (rgb) {
        case DoRed :
            color.red = (cbs->value << 8);
            break;
        case DoGreen :
            color.green = (cbs->value << 8);
            break;
        case DoBlue :
            color.blue = (cbs->value << 8);
    }

    /* reuse the same color again and again */
    XFreeColors(XtDisplay(scale_w), cmap, &color.pixel, 1, 0);
    if (!XAllocColor(XtDisplay(scale_w), cmap, &color))
        puts("Couldn't XallocColor!"), exit(1);
    XtVaSetValues(colorwindow, XmNbackground, color.pixel, NULL);
}
