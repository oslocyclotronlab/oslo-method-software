/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* xmemo.c -- a memo calendar.
 * This program demonstrates a lot of very very subtle quirks about
 * X programming and some extreme sneakiness for Motif programming.
 * What separates the simple programs from the sophisticated ones is the
 * the way you have to get around these little intrcasies to get certain
 * things done.
 *
 * Basically, xmemo creates a calendar on the left and a list of months on
 * the right.  Selecting a month changes the calendar.  Selecting a day
 * causes that date to become activated and a popup window is displayed that
 * contains a text widget.  This is presumably used to keep memos for that
 * day.  You can pop up and down the window by continuing to select the
 * date on that month.
 *
 * The way this is implemented, however, is not that simple.  Each date in
 * each month is a separate PushButton widget whose XmNshadowThickness is
 * initialized to 0.  This gives the appearance that the month contains a
 * single flat area with days in it.  When a date is selected, the shadow
 * thickness for that date's PushButton widget is reset to 2 (the default),
 * giving visual feedback that there is a text-memo associated with it.
 *
 * This is just the beginning... along the way, we'll see that there are
 * a number of other snags that we'll have to get around.  These are all
 * documented in the code.
 */
#include <stdio.h>
#include <X11/Xos.h>
#include <Xm/List.h>
#include <Xm/Frame.h>
#include <Xm/LabelG.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/Form.h>
#include <Xm/Text.h>

int year;
void XmStringFreeTable(), date_dialog(), set_month();
Widget list_w, month_label;

typedef struct _month {
    char *name;
    Widget form, dates[6][7];
} Month;

char *day_names[] = {
    "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"
};
Month months[] = { /* only initialize "known" data */
    { "January" }, { "February" }, { "March" }, { "April" },
    { "May" }, { "June" }, { "July" }, { "August" }, { "September" },
    { "October" }, { "November" }, { "December" }
};

String fallback_resources[] = {
    "*XmPushButton.fontList: -*-courier-bold-r-*--18-*",
    "*XmLabelGadget.fontList: -*-courier-bold-r-*--18-*",
    "*XmList.fontList: -*-courier-medium-r-*--18-*",
    NULL
};

XtAppContext app;

main(argc, argv)
int argc;
char *argv[];
{
    Widget toplevel, frame, rowcol, rowcol2;
    int month;

    toplevel = XtVaAppInitialize(&app, "Xmemo", NULL, 0,
        &argc, argv, fallback_resources, NULL);

    /* The form is the general layout manager for the application.
     * It will contain two widgets (the calendary and the list of months).
     * These widgets are laid out horizontally.
     */
    rowcol = XtVaCreateWidget("rowcol",
        xmRowColumnWidgetClass, toplevel,
        XmNorientation, XmHORIZONTAL,
        NULL);

    /* Place a frame around the calendar... */
    frame = XtVaCreateManagedWidget("frame1",
        xmFrameWidgetClass, rowcol, NULL);
    /* the calendar is placed inside of a RowColumn widget */
    rowcol2 = XtVaCreateManagedWidget("rowcol2",
        xmRowColumnWidgetClass, frame, NULL);
    /* the month's label (name of the month) changes dynamically as each
     * month is selected.
     */
    month_label = XtVaCreateManagedWidget("month_label",
        xmLabelGadgetClass, rowcol2, NULL);
    XtVaCreateManagedWidget(" Su Mo Tu  We Th  Fr Sa",
        xmLabelGadgetClass, rowcol2, NULL);

    /* Create a ScrolledText that contains the months.  You probably won't
     * see the scrollbar unless the list is resized so that not all of
     * the month names are visible.
     */
    {
        XmString strs[XtNumber(months)];
        for (month = 0; month < XtNumber(months); month++)
            strs[month] = XmStringCreateSimple(months[month].name);
        list_w = XmCreateScrolledList(rowcol, "list", NULL, 0);
        XtVaSetValues(list_w,
            XmNitems,      strs,
            XmNitemCount,  XtNumber(months),
            NULL);
        for (month = 0; month < XtNumber(months); month++)
            XmStringFree(strs[month]);
        XtAddCallback(list_w, XmNbrowseSelectionCallback, set_month, NULL);
        XtManageChild(list_w);
    }

    /* Determine the year we're dealing with and establish today's month */
    if (argc > 1)
        year = atoi(argv[1]);
    else {
        long time(), t = time(0);
        struct tm *today = localtime(&t);
        year = 1900 + today->tm_year;
        month = today->tm_mon+1;
    }
    XmListSelectPos(list_w, month, True);

    XtManageChild(rowcol);

    /* we're ready to rock-n-roll.  Manage toplevel widget and fly... */
    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}

/* Callback routine for when a month is selected.
 * Each month is a separate, self-contained widget that contains the
 * dates as PushButton widgets.  New months do not overwrite old ones,
 * so the old month must be "unmanaged" before the new month is managed.
 * If the month has not yet been created, then figure out the dates and
 * which days of the week they fall on using clever math computations...
 */
void
set_month(w, client_data, list_cbs)
Widget w;
XtPointer client_data; /* Unused */
XmListCallbackStruct *list_cbs;
{
    char text[BUFSIZ];
    register char *p;
    int i, j, m, tot, day;
    static int month = -1;

    if (list_cbs->item_position == month+1)
        return; /* same month, don't bother redrawing */

    if (month >= 0 && months[month].form)
        XtUnmanageChild(months[month].form); /* unmanage last month */
    month = list_cbs->item_position - 1; /* set new month */
    sprintf(text, "%s  %d", months[month].name, year);
    XtVaSetValues(month_label,
        /* too lazy to convert string to compound string.
         * let type converter do it.
         */
        XtVaTypedArg, XmNlabelString, XmRString, text, strlen(text)+1,
        NULL);
    if (months[month].form) {
        /* it's already been created -- just manage and return */
        XtManageChild(months[month].form);
        return;
    }

    /* Create the month Form widget and "dates" pushbutton widgets */
    months[month].form = XtVaCreateWidget("month_form",
        xmRowColumnWidgetClass, XtParent(month_label),
        XmNorientation,    XmHORIZONTAL,
        XmNnumColumns,     6,
        XmNpacking,        XmPACK_COLUMN,
        NULL);

    /* calculate the dates of the month using "science". */
    /* day_number() takes day-of-month (1-31), returns day-of-week (0-6) */
    m = day_number(year, month + 1, 1);
    tot = days_in_month(year, month + 1);

    /* This gets funky -- we are creating a whole bunch of PushButtons,
     * but not all of them have "dates" associated with them.  The buttons
     * that -have- dates get the number sprintf'ed into it.  All others
     * get two blanks.
     */
    for (day = i = 0; i < 6; i++) {
        for (j = 0; j < 7; j++, m += (j > m && --tot > 0)) {
            char *name;
            if (j != m || tot < 1)
                name = "  ";
            else {
                /* sprintf() returns int on some machines/char * on others */
                (void) sprintf(text, "%2d", ++day);
                name = text;
            }
            months[month].dates[i][j] =
                XtVaCreateManagedWidget(name,
                    xmPushButtonWidgetClass, months[month].form,
                    /* this is where we will hold the dialog later. */
                    XmNuserData,      NULL,
                    XmNsensitive,     (j % 7 == m && tot > 0),
                    XmNshadowThickness, 0,
                    NULL);
            XtAddCallback(months[month].dates[i][j],
                XmNactivateCallback, date_dialog, day);
        }
        m = 0;
    }
    XtManageChild(months[month].form);

    /* Now, this is *extremely* sneaky.  First, some background: the RowColumn
     * widget creates equally sized boxes (cells) for each child it manages.
     * If one child is bigger than the rest, all children get that big.  The
     * problem with creating all the PushButtons with a 0 shadow thickness is
     * that as soon as one PushButton is selected and its thickness is set to
     * 2 (date_dialog() below), then the entire RowColumn will resize itself.
     * To compensate for the problem, we need to set the shadow thickness of
     * at least -one- of the buttons to 2, so that the entire RowColumn will
     * be initialized to the right size.  But, this will cause the button to
     * have a visible border (which makes it appear "preselected").
     * So, we have to make it appear invisible -- but if it's invisible then
     * it cannot be selectable.  It just so happens that the last 5 days in
     * the month will never have selectable dates, so we can use any one of
     * those.  (Let's choose the last.)  The tricky part is how to make it
     * "invisible."  This can be done by "unmapping" the widget --we can't
     * simply unmanage it or the parent won't consider its size, which defeats
     * the whole purpose.  We can't just create the widget and unmap it
     * because it has no window yet (because it must be realized first).
     * And it that can't happen till its parent has been, and it's parent
     * has been, etc... We don't want to realize and manage this entire
     * application just to make this one widget realized so we can unmap it.
     * So we set XmNmappedWhenManaged to False along with the shadow thickness
     * being set to 2.  Now the form will be created with the appropriate size.
     */
    XtVaSetValues(months[month].dates[5][6],
        XmNshadowThickness, 2,
        XmNmappedWhenManaged, False,
        NULL);
}

/* callback for a date/PushButton widget.  When a date is selected, this
 * function is called.  Create a dialog (toplevel shell) that contains a
 * multiline text widget to save memos about this date.
 */
void
date_dialog(w, date)
Widget w;
int date;
{
    Widget dialog = 0;
    XWindowAttributes xwa;

    /* the dialog is stored in the PushButton's XmNuserData */
    XtVaGetValues(w, XmNuserData, &dialog, NULL);
    if (!dialog) {
        /* it doesn't exist yet, create it. */
        char buf[32];
        Arg args[3];
        int n_pos, *list;

        /* get the month that was selected -- we just need it for its name */
        if (!XmListGetSelectedPos(list_w, &list, &n_pos))
            return;
        (void) sprintf(buf, "%s %d %d", months[list[0]-1].name, date, year);
        XtFree(list);
        dialog = XtVaCreatePopupShell(NULL,
            topLevelShellWidgetClass, XtParent(w),
            XmNtitle,            buf,
            XmNallowShellResize, True,
            XmNdeleteResponse,   XmUNMAP,
            NULL);
        XtSetArg(args[0], XmNrows,     10);
        XtSetArg(args[1], XmNcolumns,  40);
        XtSetArg(args[2], XmNeditMode, XmMULTI_LINE_EDIT);
        XtManageChild(XmCreateScrolledText(dialog, "text", args, 3));
        /* set the shadow thickness to 2 so user knows there is a memo
         * attached to this date.
         */
        XtVaSetValues(w,
            XmNuserData, dialog,
            XmNshadowThickness, 2,
            NULL);
    }
    /* See if the dialog is realized and is visible.  If so, pop it down */
    if (XtIsRealized(dialog) &&
            XGetWindowAttributes(XtDisplay(dialog), XtWindow(dialog), &xwa) &&
                xwa.map_state == IsViewable)
        XtPopdown(dialog);
    else
        XtPopup(dialog, XtGrabNone);
}

/* the rest of the file is junk to support finding the current date. */

static int mtbl[] = { 0,31,59,90,120,151,181,212,243,273,304,334,365 };

int
days_in_month(Year, month)
int Year, month;
{
    int days;

    days = mtbl[month] - mtbl[month - 1];
    if (month == 2 && Year%4 == 0 && (Year%100 != 0 || Year%400 == 0))
        days++;
    return days;
}

int
day_number(Year, month, Day)
int Year, month, Day;
{
    /* Lots of foolishness with casts for Xenix-286 16-bit ints */

    long days_ctr;      /* 16-bit ints overflowed Sept 12, 1989 */

    Year -= 1900;
    days_ctr = ((long)Year * 365L) + ((Year + 3) / 4);
    days_ctr += mtbl[month-1] + Day + 6;
    if (month > 2 && (Year % 4 == 0))
        days_ctr++;
    return (int)(days_ctr % 7L);
}
