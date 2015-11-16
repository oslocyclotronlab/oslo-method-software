/* Written by Dan Heller.  Copyright 1991, O'Reilly && Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* text_form.c -- demonstrate how attachments work in Form widgets.
 * by creating a text-entry form type application.
 */

#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/LabelG.h>
#include <Xm/Text.h>
#include <Xm/Form.h>

char *prompts[] = {
    "Name:", "Phone:", "Address:",
    "City:", "State:", "Zip:",
};

main(argc, argv)
int argc;
char *argv[];
{
    Widget toplevel, mainform, subform, label, text;
    XtAppContext app;
    char buf[32];
    int i;

    toplevel = XtVaAppInitialize(&app, "Demos", NULL, 0,
        &argc, argv, NULL, NULL);

    mainform = XtVaCreateWidget("mainform",
        xmFormWidgetClass, toplevel,
        NULL);

    for (i = 0; i < XtNumber(prompts); i++) {
        subform = XtVaCreateWidget("subform",
            xmFormWidgetClass,   mainform,
            /* first one should be attached for form */
            XmNtopAttachment,    i? XmATTACH_WIDGET : XmATTACH_FORM,
            /* others are attached to the previous subform */
            XmNtopWidget,        subform,
            XmNleftAttachment,   XmATTACH_FORM,
            XmNrightAttachment,  XmATTACH_FORM,
            NULL);
        label = XtVaCreateManagedWidget(prompts[i],
            xmLabelGadgetClass,  subform,
            XmNtopAttachment,    XmATTACH_FORM,
            XmNbottomAttachment, XmATTACH_FORM,
            XmNleftAttachment,   XmATTACH_FORM,
            XmNalignment,        XmALIGNMENT_BEGINNING,
            NULL);
        sprintf(buf, "text_%d", i);
        text = XtVaCreateManagedWidget(buf,
            xmTextWidgetClass,   subform,
            XmNtopAttachment,    XmATTACH_FORM,
            XmNbottomAttachment, XmATTACH_FORM,
            XmNrightAttachment,  XmATTACH_FORM,
            XmNleftAttachment,   XmATTACH_WIDGET,
            XmNleftWidget,       label,
            NULL);
        XtManageChild(subform);
    }
    /* Now that all the forms are added, manage the main form */
    XtManageChild(mainform);

    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}
