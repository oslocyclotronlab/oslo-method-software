/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* xshowbitmap.c -
 *	Displays a set of bitmaps specified on the command line, from a
 * pipe, or typed into stdin.  Bitmaps must be specified as file names.
 * All the bitmaps are drawn into a pixmap, which is used as the image for
 * a Label widget, which is used as the work window for a ScrolledWindow.
 * The bitmaps are displayed in an equal number of rows and columns if
 * possible.  You may override this by specify *either* the number of
 * rows (-rows N) or the number of columns (-cols) to use.
 * (You can't specify both.)
 *
 * Usage: xshowbitmap
 *   -s sorts the bitmaps in order of size (largest first)
 *   -v verbose mode for when input is redirected to stdin.
 *   -w width of viewport window
 *   -h height of viewport window
 *   -fg foreground_color
 *   -bg background_color
 *   -label   (default)
 *   -nolabel (don't label the bitmap with its filename)
 *   -grid N  line width for grid between bitmaps (defaults to 1)
 *   -rows N (cannot be used with -cols)
 *   -cols N (cannot be used with -rows)
 *   -fn font  (bitmap filename is printed with bitmap in "font")
 *   -bw max-width  (bitmaps larger than this width are excluded; default 64)
 *   -bh max-height  (bitmaps larger than this height are excluded; default 64)
 *   - indicates to read from stdin.  Piping doesn't require the '-'
 *     argument.  With no arguments, xshowfonts reads from stdin anyway.
 *
 * Example:
 *  xshowfont /usr/include/X11/bitmaps/*
 *
 * compile: (triple click and paste next line)
     cc -O -s xshowbitmap.c -lXaw -lXt -lXmu -lX11 -o xshowbitmap
 */

#include <stdio.h>
#include <X11/Xos.h>
#include <Xm/Label.h>
#include <Xm/ScrolledW.h>

struct _bitmap {
    char *name;
    int len; /* strlen(name) */
    unsigned int width, height;
    Pixmap bitmap;
};
typedef struct _bitmap Bitmap;

struct _resrcs {
    int sort;
    int verbose;
    int label_bitmap;
    int max_width, max_height;
    unsigned int grid;
    Pixel fg, bg;
    XFontStruct *font;
    int view_width, view_height;
    int rows, cols;
} Resrcs;

static XtResource resources[] = {
    { "sort", "Sort", XmRBoolean, sizeof (int),
	XtOffsetOf(struct _resrcs,sort), XmRImmediate, False },
    { "verbose", "Verbose", XmRBoolean, sizeof (int),
	XtOffsetOf(struct _resrcs,verbose), XmRImmediate, False },
    { "labelBitmap", "LabelBitmap", XmRBoolean, sizeof (int),
	XtOffsetOf(struct _resrcs,label_bitmap), XmRImmediate, (char *)True },
    { "grid", "Grid", XmRInt, sizeof (int),
	XtOffsetOf(struct _resrcs,grid), XmRImmediate, (char *)1 },
    { "bitmapWidth", "BitmapWidth", XmRInt, sizeof (int),
	XtOffsetOf(struct _resrcs,max_width), XmRImmediate, (char *)64 },
    { "bitmapHeight", "BitmapHeight", XmRInt, sizeof (int),
	XtOffsetOf(struct _resrcs,max_height), XmRImmediate, (char *)64 },
    { XmNfont, XmCFont, XmRFontStruct, sizeof (XFontStruct *),
	XtOffsetOf(struct _resrcs,font), XmRString, XtDefaultFont },
    { XmNforeground, XmCForeground, XmRPixel, sizeof (Pixel),
	XtOffsetOf(struct _resrcs,fg), XmRString, XtDefaultForeground },
    { XmNbackground, XmCBackground, XmRPixel, sizeof (Pixel),
	XtOffsetOf(struct _resrcs,bg), XmRString, XtDefaultBackground },
    { "view-width", "View-width", XmRInt, sizeof (int),
	XtOffsetOf(struct _resrcs,view_width), XmRImmediate, (char *)500 },
    { "view-height", "View-height", XmRInt, sizeof (int),
	XtOffsetOf(struct _resrcs,view_height), XmRImmediate, (char *)300 },
    { "rows", "Rows", XmRInt, sizeof (int),
	XtOffsetOf(struct _resrcs,rows), XmRImmediate, (char *)0 },
    { "cols", "Cols", XmRInt, sizeof (int),
	XtOffsetOf(struct _resrcs,cols), XmRImmediate, (char *)0 },
};

static XrmOptionDescRec options[] = {
    { "-sort", "sort", XrmoptionNoArg, "True" },
    { "-v", "verbose", XrmoptionNoArg, "True" },
    { "-fn", "font", XrmoptionSepArg, NULL },
    { "-fg", "foreground", XrmoptionSepArg, NULL },
    { "-bg", "background", XrmoptionSepArg, NULL },
    { "-w", "view-width", XrmoptionSepArg, NULL },
    { "-h", "view-height", XrmoptionSepArg, NULL },
    { "-rows", "rows", XrmoptionSepArg, NULL },
    { "-cols", "cols", XrmoptionSepArg, NULL },
    { "-bw", "bitmapWidth", XrmoptionSepArg, NULL },
    { "-bh", "bitmapHeight", XrmoptionSepArg, NULL },
    { "-bitmap_width", "bitmapWidth", XrmoptionSepArg, NULL },
    { "-bitmap_height", "bitmapHeight", XrmoptionSepArg, NULL },
    { "-label", "labelBitmap", XrmoptionNoArg, "True" },
    { "-nolabel", "labelBitmap", XrmoptionNoArg, "False" },
    { "-grid", "grid", XrmoptionSepArg, NULL },
};

/* used by qsort to sort bitmaps into alphabetical order (by name) */
size_cmp(b1, b2)
Bitmap *b1, *b2;
{
    int n = (int)(b1->width * b1->height) - (int)(b2->width * b2->height);
    if (n)
	return n;
    return strcmp(b1->name, b2->name);
}

/* get the integer square root of n -- used to put the bitmaps in an
 * equal number of rows and colums.
 */
int_sqrt(n)
register int n;
{
    register int i, s = 0, t;
    for (i = 15; i >= 0; i--) {
	t = (s | (1 << i));
	if (t * t <= n)
	    s = t;
    }
    return s;
}

main(argc, argv)
int argc;
char *argv[];
{
    extern char *strcpy();
    XtAppContext app;
    Widget toplevel, scrolled_w;
    Pixmap pixmap; /* used the as image for Label widget */
    Bitmap *list = (Bitmap *)NULL;
    char buf[128], *p;
    XFontStruct *font;
    GC gc;
    Display *dpy;
    int istty = isatty(0), redirect = !istty, i = 0, total = 0;
    unsigned int cell_width = 0, cell_height = 0, bitmap_error;
    int j, k;

    toplevel = XtVaAppInitialize(&app, "XShowbitmap",
        options, XtNumber(options), &argc, argv, NULL, NULL);
    dpy = XtDisplay(toplevel);

    XtGetApplicationResources(toplevel, &Resrcs,
	resources, XtNumber(resources), NULL, 0);

    if (Resrcs.rows && Resrcs.cols)
	XtError("You can't specify both rows *and* columns.");

    font = Resrcs.font;

    if (!argv[1] || !strcmp(argv[1], "-")) {
	printf("Loading bitmap names from input. ");
	if (istty) {
	    puts("End with EOF or .");
	    redirect++;
	} else
	    puts("Use -v to view bitmap names being loaded.");
    } else if (!istty && strcmp(argv[1], "-"))
	printf("%s: either use pipes or specify bitmap names -- not both.\n",
	    argv[0]), exit(1);
    while (*++argv || redirect) {
	if (!redirect)
	    if (!strcmp(*argv, "-"))
		redirect++;
	    else
		(void) strcpy(buf, *argv);
	if (redirect) {
	    if (istty)
		printf("Bitmap file: "), fflush(stdout);
	    if (!fgets(buf, sizeof buf, stdin) || !strcmp(buf, ".\n"))
		break;
	    buf[strlen(buf)-1] = 0;
	}
	if (!buf[0])
	    continue;
	if (istty || Resrcs.verbose)
	    printf("Loading \"%s\"...", buf), fflush(stdout);
	if (i == total) {
	    total += 10;
	    if (!(list = (Bitmap *)XtRealloc(list, total * sizeof (Bitmap))))
		XtError("Not enough memory for bitmap data");
	}
	if ((bitmap_error = XReadBitmapFile(dpy, DefaultRootWindow(dpy), buf,
		&list[i].width, &list[i].height, &list[i].bitmap,
		&j, &k)) == BitmapSuccess) {
	    if (p = rindex(buf, '/'))
		p++;
	    else
		p = buf;
	    if (Resrcs.max_height && list[i].height > Resrcs.max_height ||
		Resrcs.max_width && list[i].width > Resrcs.max_width) {
		printf("%s: bitmap too big\n", p);
		XFreePixmap(dpy, list[i].bitmap);
		continue;
	    }
	    list[i].len = strlen(p);
	    list[i].name = strcpy(XtMalloc(list[i].len + 1), p);
	    if (istty || Resrcs.verbose)
		printf("size: %dx%d\n", list[i].width, list[i].height);
	    i++;
	} else {
	    printf("couldn't load bitmap: ");
	    if (!istty && !Resrcs.verbose)
		printf("\"%s\": ", buf);
	    switch (bitmap_error) {
		case BitmapOpenFailed : puts("open failed."); break;
		case BitmapFileInvalid : puts("bad file format."); break;
		case BitmapNoMemory : puts("no enough memory."); break;
	    }
	}
    }
    if ((total = i) == 0) {
	puts("No bitmaps?!");
	exit(1);
    }
    printf("Total bitmaps loaded: %d\n", total);
    if (Resrcs.sort) {
	printf("Sorting bitmaps..."), fflush(stdout);
	qsort(list, total, sizeof (Bitmap), size_cmp);
	putchar('\n');
    }
    /* calculate size for pixmap by getting the dimensions of each bitmap. */
    printf("Calculating sizes for pixmap..."), fflush(stdout);
    for (i = 0; i < total; i++) {
	if (list[i].width > cell_width)
	    cell_width = list[i].width;
	if (list[i].height > cell_height)
	    cell_height = list[i].height;
	j = XTextWidth(font, list[i].name, list[i].len);
	if (j > cell_width)
	    cell_width = j;
    }
    cell_height += 6 + font->ascent + font->descent;
    cell_width += 6;
    if (!Resrcs.rows && !Resrcs.cols) {
	Resrcs.cols = int_sqrt(total);
	Resrcs.rows = (total + Resrcs.cols-1)/Resrcs.cols;
    } else if (Resrcs.rows)
	/* user specified rows -- figure out columns */
	Resrcs.cols = (total + Resrcs.rows-1)/Resrcs.rows;
    else
	/* user specified cols -- figure out rows */
	Resrcs.rows = (total + Resrcs.cols-1)/Resrcs.cols;

    printf("Creating pixmap area of size %dx%d (%d rows, %d cols)\n",
	Resrcs.cols * cell_width, Resrcs.rows * cell_height,
	Resrcs.rows, Resrcs.cols);

    if (!(pixmap = XCreatePixmap(dpy, DefaultRootWindow(dpy),
	Resrcs.cols * cell_width + Resrcs.grid,
	Resrcs.rows * cell_height + Resrcs.grid,
	DefaultDepth(dpy, DefaultScreen(dpy)))))
	XtError("Can't Create pixmap");

    if (!(gc = XCreateGC(dpy, pixmap, NULL, 0)))
	XtError("Can't create gc");
    XSetForeground(dpy, gc, Resrcs.bg); /* init GC's foreground to bg */
    XFillRectangle(dpy, pixmap, gc, 0, 0,
	Resrcs.cols * cell_width, Resrcs.rows * cell_height);
    XSetForeground(dpy, gc, Resrcs.fg);
    XSetBackground(dpy, gc, Resrcs.bg);
    XSetFont(dpy, gc, font->fid);
    if (Resrcs.grid) {
	if (Resrcs.grid != 1)
	    XSetLineAttributes(dpy, gc, Resrcs.grid, 0, 0, 0);
	for (j = 0; j <= Resrcs.rows * cell_height; j += cell_height)
	    XDrawLine(dpy, pixmap, gc, 0, j, Resrcs.cols * cell_width, j);
	for (j = 0; j <= Resrcs.cols * cell_width; j += cell_width)
	    XDrawLine(dpy, pixmap, gc, j, 0, j, Resrcs.rows*cell_height);
    }
    for (i = 0; i < total; i++) {
	int x = cell_width * (i % Resrcs.cols);
	int y = cell_height * (i / Resrcs.cols);
	if (Resrcs.label_bitmap)
	    XDrawString(dpy, pixmap, gc, x+5, y+font->ascent,
		list[i].name, list[i].len);
	if (DefaultDepth(dpy, DefaultScreen(dpy)) > 1)
	    XCopyPlane(dpy, list[i].bitmap, pixmap, gc,
		0, 0, list[i].width, list[i].height,
		x+5, y + font->ascent + font->descent, 1L);
	else
	    XCopyArea(dpy, list[i].bitmap, pixmap, gc,
		0, 0, list[i].width, list[i].height,
		x+5, y + font->ascent + font->descent);
	XFreePixmap(dpy, list[i].bitmap);
	XtFree(list[i].name);
    }

    /* Create Scrolled Window to contain the pixmap */
    scrolled_w = XtVaCreateManagedWidget("scrolled_w",
	xmScrolledWindowWidgetClass, toplevel,
	XmNscrollingPolicy, XmAUTOMATIC,
	XmNwidth,	    Resrcs.view_width,
	XmNheight,	    Resrcs.view_height,
	NULL);

    XtVaCreateManagedWidget(NULL, xmLabelWidgetClass, scrolled_w,
	XmNlabelType,	XmPIXMAP,
	XmNlabelPixmap,	pixmap,
	NULL);

    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}
