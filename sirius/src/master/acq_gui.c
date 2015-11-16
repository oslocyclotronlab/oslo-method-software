#include	<sys/types.h>
#include	<sys/ipc.h>
#include	<sys/stat.h>
#include	<unistd.h>
#include	<stdlib.h>

#include	<Xm/RowColumn.h>
#include	<Xm/CascadeB.h>
#include	<Xm/SeparatoG.h>
#include	<Xm/PushBG.h>
#include	<Xm/PushB.h>
#include	<Xm/MainW.h>
#include	<Xm/Text.h>
#include	<Xm/LabelG.h>
#include	<Xm/CascadeBG.h>
#include	<Xm/ToggleB.h>
#include	<Xm/ToggleBG.h>
#include	<Xm/SelectioB.h>
#include	<Xm/FileSB.h>
#include	<Xm/DrawnB.h>
#include	<Xm/MessageB.h>

#define		ATTACH 1
#define		DETTAC 2

/* Define global variables */    
int			exades;			/* File descriptor for Exabyte */
int			exano;			/* Exabyte drive number */
int			semid;			/* Semaphore set ID */
int			*messp;			/* Pointer to shared memory message_box */
int			Is_Started = 0;	/* Status variable, 0 = NOT Started, 1 = Started */
int			Media_On   = 0;	/* Status variable, 0 = NO Output, 1 = Output On */

Widget 		toplevel, rowcol, newcol, MainWindow;
Widget 		text_w, error_w;	/* Message output and error output windows */
Widget		quitm, testm;
Widget 		runm, run_start, run_stop, run_status;
Widget 		specm, spec_clear, spec_dump, spec_list, spec_mama;
Widget		filem, file_quit, file_opt, opt_tele;
Widget		outputm, output_device, output_unload, DevicePullRight, PositionPullRight;
Widget		output_position, position_current, position_bot, position_eod, position_file;
Widget		device_none, device_exb1, device_exb2, device_disc;
Widget		gainm, gain_init, gain_file, gain_show;
Widget		telewinm, telewin_init, telewin_file, telewin_show;
Widget		sortm, sort_load, show_load;
Widget 		helpm, help_about, help_install, help_online;
Widget		startb, stopb, updateb;
Pixmap		pixmap;

main(argc, argv)
int argc;
char *argv[];
{    

    Widget		MenuBar; 
    Widget		FilePullDown, RunPullDown, SpecPullDown, OutputPullDown, GainPullDown, TeleWinPullDown, SortPullDown, HelpPullDown;

    XmString    	file, run, spec, output, gain, telewin, sort, help;
    XmString		quit, quit_accel_text, text;
    XtAppContext 	app;
    void		quit_pushed();
    void		teleno_pushed();
    void		start_pushed();
    void		stop_pushed();
    void		status_pushed();		
    void		clear_pushed();		
    void		dump_pushed();		
    void		list_pushed();		
    void		mama_pushed();
    void		none_device_pushed();		
    void		exb1_device_pushed();		
    void		exb2_device_pushed();
    void		disc_device_pushed();
    void		current_position_pushed();		
    void		bot_position_pushed();		
    void		eod_position_pushed();
    void		file_position_pushed();	
    void		unload_pushed();
    void		gaininit_pushed();
    void		gainfile_pushed();
    void		gainshow_pushed();
	void		telewininit_pushed();
    void		telewinfile_pushed();
    void		telewinshow_pushed();
    void		load_pushed();		
    void		show_pushed();		
    void		h_about_pushed();
	void		h_install_pushed();
    void		h_online_pushed();

    Arg			args[7];
    int			n = 0;
    int			status;				     
    char		*cc_quit = "Ctrl<Key>Q";

    /* ------------------------------------------------------------------ */
    /* ------------------------------------------------------------------ */
    /* Initialize the data acquistion system                              */
    if ( init_sirius() == -1) {
       printf("\n **** ERROR **** Initialization failed - exiting \n");
       exit(0);
    }
	 
    /* ------------------------------------------------------------------ */
    /* ------------------------------------------------------------------ */
    /* Build the GUI, start with Motif top level routines		  */
    XtSetLanguageProc (NULL, NULL, NULL);
    toplevel = XtVaAppInitialize (&app, "Sirius", NULL, 0, &argc, argv, NULL, 
					XmNminWidth,	500,
					XmNminHeight,	550,
					XmNmaxWidth,	520,
					XmNmaxHeight,	570,				  
					NULL);


    MainWindow = XtVaCreateManagedWidget("main_w",
       					xmMainWindowWidgetClass,toplevel,
        				XmNscrollingPolicy,  	XmAUTOMATIC,
        				NULL);


    /* Define a RowColumn manager widget as child of MainWindow */
    rowcol = XtVaCreateManagedWidget 	("rowcolumn", 
					xmRowColumnWidgetClass, MainWindow,
					XmNnumColumns, 		3,
                                        XmNorientation, 	XmVERTICAL,
					XmNisAligned,		True,
                                      	NULL);


  /* Define another RowColumn manager widget as child of rowcol */
    newcol = XtVaCreateManagedWidget 	("newcolumn", 
					xmRowColumnWidgetClass, rowcol,
                                      	NULL);


    /* ------------------------------------------------------------------ */
    /* ------------------------------------------------------------------ */
    /* Create the menubar                                                 */
    MenuBar = XmCreateMenuBar 		(MainWindow, 
					"MenuBar", 
					NULL, 0); 



    /* ------------------------------------------------------------------ */
    /* create the "File" pulldown menu                                    */
    FilePullDown = XmCreatePulldownMenu (MenuBar, 
					"FilePullDown", 
					NULL, 0);
    file = XmStringCreateLocalized 	("File");
    filem = XtVaCreateManagedWidget 	("File", 
             				xmCascadeButtonWidgetClass, MenuBar,
             				XmNlabelString,  	file,
             				XmNmnemonic,    	'F',
             				XmNsubMenuId,    	FilePullDown,
             				NULL);
    XmStringFree 			( file ); 




    /* Add the menu item OPTIONS */
    DevicePullRight = XmCreatePulldownMenu( FilePullDown, "DevicePullRight", NULL, 0);

    file_opt = XtVaCreateManagedWidget ("Options",
                                      xmCascadeButtonGadgetClass, FilePullDown,
                                      XmNsubMenuId, DevicePullRight, 
                                      NULL);

    opt_tele = XtVaCreateManagedWidget ("No of Telescopes",
                                      xmPushButtonGadgetClass,
                                      DevicePullRight, 
                                      NULL);


    XtAddCallback (opt_tele, XmNactivateCallback, teleno_pushed, NULL);  


    XtVaCreateManagedWidget ("separator", xmSeparatorGadgetClass, FilePullDown, NULL);



    /* Add the menu item QUIT */
    quit_accel_text = XmStringCreateLocalized("Ctrl+Q");
    file_quit = XtVaCreateManagedWidget ("Quit",
                                      	xmPushButtonGadgetClass, 
                                      	FilePullDown, 
             			      	XmNmnemonic,    	'Q',
				      	XmNacceleratorText, 	quit_accel_text,
				     	XmNaccelerator,	    	cc_quit,
                                      NULL);
    XtAddCallback (file_quit, XmNactivateCallback, quit_pushed, NULL);  


    /* ------------------------------------------------------------------ */
    /* create the "Run" pulldown menu */
    RunPullDown = XmCreatePulldownMenu (MenuBar,"RunPullDown", NULL, 0);
    run = XmStringCreateLocalized ("Run");
    runm = XtVaCreateManagedWidget ("Run", 
             xmCascadeButtonWidgetClass, MenuBar,
             XmNlabelString,  run,
             XmNmnemonic,    'R',
             XmNsubMenuId,    RunPullDown,
             NULL);
    XmStringFree ( run ); 

    /* Add the menu item START */
    run_start = XtVaCreateManagedWidget ("Start",
                                      xmPushButtonGadgetClass, 
                                      RunPullDown, 
                                      NULL);
    XtAddCallback (run_start, XmNactivateCallback, start_pushed, NULL);

    /* Add the menu item STOP */
    run_stop = XtVaCreateManagedWidget ("Stop",
                                      xmPushButtonGadgetClass, 
                                      RunPullDown, 
                                      NULL);
    XtAddCallback (run_stop, XmNactivateCallback, stop_pushed, NULL); 

    XtVaCreateManagedWidget ("separator",
        			      xmSeparatorGadgetClass, RunPullDown, NULL);

    /* Add the menu item STATUS */
    run_status = XtVaCreateManagedWidget ("Status",
                                      xmPushButtonGadgetClass, 
                                      RunPullDown, 
                                      NULL);
    XtAddCallback (run_status, XmNactivateCallback, status_pushed, NULL); 


    /* ------------------------------------------------------------------ */
    /* create the "Spectra" pulldown menu */
    SpecPullDown = XmCreatePulldownMenu (MenuBar, "SpecPullDown", NULL, 0);
    XtVaSetValues(SpecPullDown, XmNtearOffModel, XmTEAR_OFF_ENABLED, NULL);
    spec = XmStringCreateLocalized ("Spectra");
    specm = XtVaCreateManagedWidget ("Spectra", 
             xmCascadeButtonWidgetClass, MenuBar,
             XmNlabelString,  spec,
             XmNmnemonic,    'S',
             XmNsubMenuId,    SpecPullDown,
             NULL);
    XmStringFree ( spec ); 

    /* Add the menu item CLEAR SPECTRA */
    spec_clear = XtVaCreateManagedWidget ("Clear",
                                      xmPushButtonGadgetClass, 
                                      SpecPullDown, 
                                      NULL);
    XtAddCallback (spec_clear, XmNactivateCallback, clear_pushed, NULL);

    /* Add the menu item DUMP SPECTRA */
    spec_dump = XtVaCreateManagedWidget ("Dump",
                                      xmPushButtonGadgetClass, 
                                      SpecPullDown, 
                                      NULL);
    XtAddCallback (spec_dump, XmNactivateCallback, dump_pushed, NULL);

    /* Add the menu item LIST SPECTRA */
    spec_list = XtVaCreateManagedWidget ("List",
                                      xmPushButtonGadgetClass, 
                                      SpecPullDown, 
                                      NULL);
    XtAddCallback (spec_list, XmNactivateCallback, list_pushed, NULL);

   XtVaCreateManagedWidget ("separator",
        			      xmSeparatorGadgetClass, SpecPullDown, NULL);

    /* Add the menu item START MAMA */
    spec_mama = XtVaCreateManagedWidget ("MAMA",
                                      xmPushButtonGadgetClass, 
                                      SpecPullDown, 
                                      NULL);
    XtAddCallback (spec_mama, XmNactivateCallback, mama_pushed, NULL);


    /* ------------------------------------------------------------------ */
    /* create the "Output" pulldown menu */
    OutputPullDown = XmCreatePulldownMenu (MenuBar, "OutputPullDown", NULL, 0);
    output = XmStringCreateLocalized ("Output");
    outputm = XtVaCreateManagedWidget ("Output", 
             xmCascadeButtonWidgetClass, MenuBar,
             XmNlabelString,  output,
             XmNmnemonic,    'O',
             XmNsubMenuId,    OutputPullDown,
             NULL);
    XmStringFree ( output ); 

    /* Add the menu DEVICE */
    DevicePullRight = XmCreatePulldownMenu( OutputPullDown, "DevicePullRight", NULL, 0);

    output_device = XtVaCreateManagedWidget ("Device",
                                      xmCascadeButtonGadgetClass, OutputPullDown,
                                      XmNsubMenuId, DevicePullRight, 
                                      NULL);

    device_none = XtVaCreateManagedWidget ("None",
                                      xmPushButtonGadgetClass,
                                      DevicePullRight, 
                                      NULL);
    device_exb1 = XtVaCreateManagedWidget ("Exabyte 1",
                                      xmPushButtonGadgetClass,
                                      DevicePullRight, 
                                      NULL);
    device_exb2 = XtVaCreateManagedWidget ("Exabyte 2",
                                      xmPushButtonGadgetClass,
                                      DevicePullRight, 
                                      NULL);
    device_disc = XtVaCreateManagedWidget ("Disk",
                                      xmPushButtonGadgetClass,
                                      DevicePullRight, 
                                      NULL);
    XtAddCallback (device_none, XmNactivateCallback, none_device_pushed, NULL);
    XtAddCallback (device_exb1, XmNactivateCallback, exb1_device_pushed, NULL);
    XtAddCallback (device_exb2, XmNactivateCallback, exb2_device_pushed, NULL);
    XtAddCallback (device_disc, XmNactivateCallback, disc_device_pushed, NULL);


    /* Add the menu POSTITION */
    PositionPullRight = XmCreatePulldownMenu( OutputPullDown, "PositionPullRight", NULL, 0);

    output_position = XtVaCreateManagedWidget ("Position at",
                                      xmCascadeButtonGadgetClass, OutputPullDown,
                                      XmNsubMenuId, PositionPullRight, 
                                      NULL);

    position_current = XtVaCreateManagedWidget ("Current position",
                                      xmPushButtonGadgetClass,
                                      PositionPullRight, 
                                      NULL);
    position_bot = XtVaCreateManagedWidget ("Beginning of Data",
                                      xmPushButtonGadgetClass,
                                      PositionPullRight, 
                                      NULL);
    position_eod = XtVaCreateManagedWidget ("End of Data",
                                      xmPushButtonGadgetClass,
                                      PositionPullRight, 
                                      NULL);
    position_file= XtVaCreateManagedWidget ("File no...",
                                      xmPushButtonGadgetClass,
                                      PositionPullRight, 
                                      NULL);

 
    XtAddCallback (position_current, XmNactivateCallback, current_position_pushed, NULL);
    XtAddCallback (position_bot, XmNactivateCallback, bot_position_pushed, NULL);
    XtAddCallback (position_eod, XmNactivateCallback, eod_position_pushed, NULL);
    XtAddCallback (position_file, XmNactivateCallback, file_position_pushed, NULL); 


    XtVaCreateManagedWidget ("separator",
        			      xmSeparatorGadgetClass, OutputPullDown, NULL);

    /* Add the menu item UNLOAD */
    output_unload = XtVaCreateManagedWidget ("Unload/Close",
                                      xmPushButtonGadgetClass, 
                                      OutputPullDown, 
                                      NULL);
    XtAddCallback (output_unload, XmNactivateCallback, unload_pushed, NULL); 


    /* ------------------------------------------------------------------ */
    /* create the "Gain" pulldown menu */
    GainPullDown = XmCreatePulldownMenu (MenuBar, "GainPullDown", NULL, 0);
    gain  = XmStringCreateLocalized ("Gain");
    gainm = XtVaCreateManagedWidget ("Gain", 
             xmCascadeButtonWidgetClass, MenuBar,
             XmNlabelString,  gain,
             XmNmnemonic,    'G',
             XmNsubMenuId,    GainPullDown,
             NULL);
    XmStringFree ( gain ); 

    /* Add the menu item INITIALIZE gain factors */
    gain_init = XtVaCreateManagedWidget ("Initialize",
                                      xmPushButtonGadgetClass, 
                                      GainPullDown, 
                                      NULL);
    XtAddCallback (gain_init, XmNactivateCallback, gaininit_pushed, NULL);


    /* Add the menu item From File ... sorting application */
    gain_file = XtVaCreateManagedWidget ("From File ...",
                                      xmPushButtonGadgetClass, 
                                      GainPullDown, 
                                      NULL);
    XtAddCallback (gain_file, XmNactivateCallback, gainfile_pushed, NULL);


    /* Add the menu item Show values sorting application */
    XtVaCreateManagedWidget ("separator", xmSeparatorGadgetClass, GainPullDown, NULL);
    gain_show = XtVaCreateManagedWidget ("Show values",
                                      xmPushButtonGadgetClass, 
                                      GainPullDown, 
                                      NULL);
    XtAddCallback (gain_show, XmNactivateCallback, gainshow_pushed, NULL);
	
	
	
	
		
	
	
	
	
	/* Create the "TeleWin" pulldown menu */
    TeleWinPullDown = XmCreatePulldownMenu (MenuBar, "TeleWinPullDown", NULL, 0);
    telewin  = XmStringCreateLocalized ("TeleWin");
    telewinm = XtVaCreateManagedWidget ("TeleWin", 
             xmCascadeButtonWidgetClass, MenuBar,
             XmNlabelString,  telewin,
             XmNmnemonic,    'T',
             XmNsubMenuId,    TeleWinPullDown,
             NULL);
    XmStringFree ( telewin ); 

    /* Add the menu item INITIALIZE low and high markers */
    telewin_init = XtVaCreateManagedWidget ("Initialize",
                                      xmPushButtonGadgetClass, 
                                      TeleWinPullDown, 
                                      NULL);
    XtAddCallback (telewin_init, XmNactivateCallback, telewininit_pushed, NULL);


    /* Add the menu item From File ... sorting application */
    telewin_file = XtVaCreateManagedWidget ("From File ...",
                                      xmPushButtonGadgetClass, 
                                      TeleWinPullDown, 
                                      NULL);
    XtAddCallback (telewin_file, XmNactivateCallback, telewinfile_pushed, NULL);


    /* Add the menu item Show values sorting application */
    XtVaCreateManagedWidget ("separator", xmSeparatorGadgetClass, TeleWinPullDown, NULL);
    telewin_show = XtVaCreateManagedWidget ("Show values",
                                      xmPushButtonGadgetClass, 
                                      TeleWinPullDown, 
                                      NULL);
    XtAddCallback (telewin_show, XmNactivateCallback, telewinshow_pushed, NULL);
	

	
	
	
	
	

    /* ------------------------------------------------------------------ */
    /* create the "Sorting" pulldown menu */
    SortPullDown = XmCreatePulldownMenu (MenuBar, "SortPullDown", NULL, 0);
    sort = XmStringCreateLocalized ("SortFunc");
    sortm = XtVaCreateManagedWidget ("SortFunc", 
             xmCascadeButtonWidgetClass, MenuBar,
             XmNlabelString,  sort,
             XmNmnemonic,    'o',
             XmNsubMenuId,    SortPullDown,
             NULL);
    XmStringFree ( sort ); 

    /* Add the menu item LOAD sorting application */
    sort_load = XtVaCreateManagedWidget ("Load ...",
                                      xmPushButtonGadgetClass, 
                                      SortPullDown, 
                                      NULL);
    XtAddCallback (sort_load, XmNactivateCallback, load_pushed, NULL);


    /* Add the menu item SHOW sorting application */
    show_load = XtVaCreateManagedWidget ("Show",
                                      xmPushButtonGadgetClass, 
                                      SortPullDown, 
                                      NULL);
    XtAddCallback (show_load, XmNactivateCallback, show_pushed, NULL);


    /* ------------------------------------------------------------------ */
    /* create the "Help" pulldown menu */
    HelpPullDown = XmCreatePulldownMenu (MenuBar, "HelpPullDown", NULL, 0);

    /* create the "Help" button ) */
    help = XmStringCreateLocalized ("Help");
    helpm = XtVaCreateManagedWidget ("Help", 
             xmCascadeButtonWidgetClass, MenuBar,
             XmNlabelString,  help,
             XmNmnemonic,    'H',
             XmNsubMenuId,    HelpPullDown,
             NULL);
    XmStringFree ( help ); 

    /* Postition HELP to right */
    XtVaSetValues (MenuBar, XmNmenuHelpWidget, helpm, NULL);

    /* Add menu "About" */
    help_about = XtVaCreateManagedWidget 	("About Sirius",
              					xmPushButtonGadgetClass, 
						HelpPullDown, 
						NULL);
    XtAddCallback (help_about, XmNactivateCallback, h_about_pushed, NULL);


    /* Add menu "Install" */
    help_install = XtVaCreateManagedWidget 	("Install",
        					xmPushButtonGadgetClass, 
						HelpPullDown, 
						NULL);
    XtAddCallback (help_install, XmNactivateCallback, h_install_pushed, NULL);

    XtVaCreateManagedWidget ("separator",
        xmSeparatorGadgetClass, HelpPullDown, NULL);

    /* Add menu "Sirius" */
    help_online = XtVaCreateManagedWidget 		("Sirius",
        					xmPushButtonGadgetClass, 
						HelpPullDown, 
						NULL);
    XtAddCallback (help_online, XmNactivateCallback, h_online_pushed, NULL);

    XtManageChild (MenuBar);


    XtManageChild (MenuBar);




    /* ------------------------------------------------------------------ */    
    /* Add a row of pushbuttons below the menubar */

    XtVaSetValues (	newcol, 
			XmNpacking, 	XmPACK_COLUMN,
			XmNnumColumns, 	1,
			XmNorientation, XmHORIZONTAL,
			NULL);

    text = XmStringCreateLocalized ("Start");
    startb = XtVaCreateManagedWidget ("startb", 
					xmPushButtonWidgetClass, newcol, 
					XmNlabelString, text, 
					NULL);
    XmStringFree ( text );
    XtVaSetValues (	startb, 
			XmNalignment,	XmALIGNMENT_CENTER,
			NULL);
    XtAddCallback (startb, XmNactivateCallback, start_pushed, NULL);

    text = XmStringCreateLocalized ("Stop");
    stopb = XtVaCreateManagedWidget ("stopb", 
					xmPushButtonWidgetClass, newcol, 
					XmNlabelString, text, 
					NULL);
    XtVaSetValues (	stopb, 
			XmNalignment,	XmALIGNMENT_CENTER,
			NULL);

    XtAddCallback (stopb, XmNactivateCallback, stop_pushed, NULL);

    text = XmStringCreateLocalized ("Status");
    updateb = XtVaCreateManagedWidget ("updateb", 
					xmPushButtonWidgetClass, newcol, 
					XmNlabelString, text, 
					NULL);
    XtVaSetValues (	updateb, 
			XmNalignment,	XmALIGNMENT_CENTER,
			NULL);
    XtAddCallback (updateb, XmNactivateCallback, status_pushed, NULL);



    XtManageChild (newcol);
   



    /* ------------------------------------------------------------------ */

    /* Create a text widget for program output */
    XtVaCreateManagedWidget ( "Messages:", 		xmLabelGadgetClass, rowcol, NULL);
    XtSetArg ( args[n], XmNrows,			16); n++;
    XtSetArg ( args[n], XmNcolumns,			50); n++;
    XtSetArg ( args[n], XmNeditable,			FALSE); n++;
    XtSetArg ( args[n], XmNeditMode,			XmMULTI_LINE_EDIT); n++;
    XtSetArg ( args[n], XmNscrollHorizontal, 		FALSE); n++;
    XtSetArg ( args[n], XmNwordWrap, 			TRUE); n++;

    text_w = XmCreateScrolledText ( rowcol, "text_w", args, n );
    XtVaSetValues(text_w, XtVaTypedArg,XmNforeground, XmRString,"MidnightBlue",13,NULL);
    XtManageChild (text_w); 

    /* Create a text widget for error output */
    n=0;
    XtVaCreateManagedWidget ( "Errors:", 		xmLabelGadgetClass, rowcol, NULL);
    XtSetArg ( args[n], XmNrows,			5); n++;
    XtSetArg ( args[n], XmNcolumns,			50); n++;
    XtSetArg ( args[n], XmNeditable,			FALSE); n++;
    XtSetArg ( args[n], XmNeditMode,			XmMULTI_LINE_EDIT); n++;
    XtSetArg ( args[n], XmNscrollHorizontal, 		FALSE); n++;
    XtSetArg ( args[n], XmNwordWrap, 			TRUE); n++;

    error_w = XmCreateScrolledText ( rowcol, "error_w" ,args, n);
    XtVaSetValues(error_w, XtVaTypedArg,XmNforeground, XmRString,"red",4,NULL);
    XtManageChild (error_w); 

    XtManageChild (rowcol);   

    status = set_pushbutton_status( Is_Started, Media_On );


    XtRealizeWidget (toplevel);
    XtAppMainLoop (app);
}



/* ============================================================================*/
/*               This section contains the call-back routines                  */
/* ============================================================================*/


/* --------------------------------------------------------------------------- */
void quit_pushed(Widget widget, XtPointer client_data, XtPointer call_data)

{
	Widget 		ex_dialog;
	XmString	yes, no, msg;	
	void	exit_prog();

    	ex_dialog = XmCreateQuestionDialog (MainWindow, "SIRIUS: quit confirmation", NULL, 0);
	yes = XmStringCreateLocalized("Yes");
	no  = XmStringCreateLocalized("No");
	msg = XmStringCreateLocalized("Are you sure you want to quit SIRIUS?");
	XtVaSetValues(	ex_dialog,
			XmNmessageString,	msg,
			XmNokLabelString,	yes,
			XmNcancelLabelString,	no,
			NULL);
	

   	XtAddCallback (ex_dialog, XmNcancelCallback, (void *)XtDestroyWidget, NULL);
    	XtAddCallback (ex_dialog, XmNokCallback, exit_prog, widget);
 	XmStringFree (yes);
	XmStringFree (no);
	XmStringFree (msg);


   	XtSetSensitive ( XmMessageBoxGetChild (ex_dialog, XmDIALOG_HELP_BUTTON), False);
	XtUnmanageChild( XmMessageBoxGetChild (ex_dialog, XmDIALOG_HELP_BUTTON));
    	XtManageChild (ex_dialog);

   	XtPopup (XtParent (ex_dialog), XtGrabNone);

 
}
void exit_prog(Widget widget, XtPointer client_data, XtPointer call_data)
{
   
	char	err1[1024] = "*** ERROR *** Remove Semaphore set Failed";
	char	err2[1024] = "*** ERROR *** Detach databuffer Failed";

	/* Clean up IPC resources before exit */     			
	if ( semctl(semid, 0, IPC_RMID) == -1) {
           errprint("%s\n",err1);
	} 

	/* Detach shared message box memory */
	if ( shmdt( messp ) == -1) {
           errprint("%s\n",err2);
	}

	/* Detach shared histogram memory */
	shared_hist( DETTAC );	
		
	/* Close and remove lock-file */
	remove("/Applications/sirius/system/sort.lock");
	remove("/Applications/sirius/system/specdump.lock");
	remove("/Applications/sirius/system/specclear.lock");
	remove("/Applications/sirius/system/engine.lock");
	exit(0);
}


/* --------------------------------------------------------------------------- */
void teleno_pushed(Widget widget, XtPointer client_data, XtPointer call_data)

{
	Widget  dialog, toggle;
	Arg 	args[5];
	int 	n = 0;
	int	teleno_off = 11;
	int	*telenop;
	extern void selecttele_pushed();


   	XmString t = XmStringCreateLocalized ("Select Number of Particle Telescopes:");
	XmString btn1 = XmStringCreateLocalized ("8 Telescopes");
	XmString btn2  = XmStringCreateLocalized ("64 Telescopes");

	telenop = messp + teleno_off;

	/* Create the dialog, return to previous select state */
    	XtSetArg (args[n], XmNautoUnmanage, False); n++; 
    	dialog = XmCreateQuestionDialog (MainWindow, "Telescopes", args, n);
    	XmStringFree (t); /* always destroy compound strings when done */
    	XtAddCallback (dialog, XmNokCallback, (void *)XtDestroyWidget, NULL); 
  	XtUnmanageChild (XmMessageBoxGetChild (dialog, XmDIALOG_HELP_BUTTON)); 
  	XtUnmanageChild (XmMessageBoxGetChild (dialog, XmDIALOG_CANCEL_BUTTON)); 

	if (*telenop == 8) {
	   toggle = XmVaCreateSimpleRadioBox ( dialog, "toggle", 
						0, 
						selecttele_pushed, 
						XmVaRADIOBUTTON, btn1, NULL, NULL, NULL,
						XmVaRADIOBUTTON, btn2, NULL, NULL, NULL,
						NULL);
	}
	
	if (*telenop == 64) {
	   toggle = XmVaCreateSimpleRadioBox ( dialog, "toggle", 
						1, 
						selecttele_pushed, 
						XmVaRADIOBUTTON, btn1, NULL, NULL, NULL,
						XmVaRADIOBUTTON, btn2, NULL, NULL, NULL,
						NULL);
	}


	XmStringFree (btn1);
	XmStringFree (btn2);
	XtManageChild (toggle);
    	XtManageChild (dialog);

   	XtPopup (XtParent (dialog), XtGrabNone);
		
}

void selecttele_pushed(Widget widget, XtPointer client_data, XtPointer call_data)
{
	int	teleno_off = 11;
	int	*telenop;

	int	n = (int) client_data;
    	XmToggleButtonCallbackStruct *cbs =  (XmToggleButtonCallbackStruct *) call_data;

	telenop = messp + teleno_off;
	if ( cbs->set == False)
	   return;

	if (n == 0) {
	   *telenop = 8;
	} 
	if (n == 1) {
	   *telenop = 64;
	}

}


/* --------------------------------------------------------------------------- */
void start_pushed(Widget widget, XtPointer client_data, XtPointer call_data)

{
	int	status;
	char	err[1024] = "*** ERROR *** Already started !";

        if ( Is_Started == 0)
	   status = acq_start( );
	else
	   errprint("%s\n",err);	
	   
	if ( status == -1 ) 
	   Is_Started = 0;
	else
	   Is_Started = 1;
	
	status = set_pushbutton_status( Is_Started, Media_On );

}

/* --------------------------------------------------------------------------- */
void stop_pushed(Widget widget, XtPointer client_data, XtPointer call_data)

{
	int	status;
	char	err[1024] = "*** ERROR *** Not started !";

        if ( Is_Started == 1)
	   status = acq_stop( );
	else
	   errprint("%s\n",err);	
	   
	if ( status == -1 ) 
	   Is_Started = 1;
	else
	   Is_Started = 0;

	status = set_pushbutton_status( Is_Started, Media_On );

}

/*--------------------------------------------------------------------------- */
void status_pushed(Widget widget, XtPointer client_data, XtPointer call_data)

{
	int status;
	
	status = acq_status( );
}


/*--------------------------------------------------------------------------- */
void clear_pushed(Widget widget, XtPointer client_data, XtPointer call_data)

{
	int	status;
	char	err[1024] = "*** ERROR *** Stop acquisition first !";

        if ( Is_Started == 0)
	   status = acq_clear( );
	else
	   errprint("%s\n",err);	
}

/*--------------------------------------------------------------------------- */
void dump_pushed(Widget widget, XtPointer client_data, XtPointer call_data)

{
	int	status;
	status = acq_dump( );

}

/*--------------------------------------------------------------------------- */
void list_pushed(Widget widget, XtPointer client_data, XtPointer call_data)
{
    	char	buf[1024];
	char	c;
	int	i;
	int	buflength = 1024;
    	FILE	*speclist;

	speclist = fopen( "/Applications/sirius/help/spectra.list","r+" );

	for (i=0; i<buflength-1 && (c = getc(speclist)) && c != EOF; i++)
	   buf[i] = toascii(c);
        fclose( speclist );	
	buf[i] = '\0';
    	
    	wprint("%s", buf);

}

/*--------------------------------------------------------------------------- */
void mama_pushed(Widget widget, XtPointer client_data, XtPointer call_data)

{
	 system("xterm -bg moccasin -fg black -geometry 80x25+5-60 -e /Applications/prog/bin/mama &");
}

/*--------------------------------------------------------------------------- */
	int		exano;			/* Exabyte drive number */

void none_device_pushed(Widget widget, XtPointer client_data, XtPointer call_data)

{
	int	status;
	int	device = 0;

	status = acq_storage( device );
	Media_On = 0;

	exano  = 0;
	status = set_pushbutton_status( Is_Started, Media_On );

}

/*--------------------------------------------------------------------------- */
	int		exano;			/* Exabyte drive number */

void exb1_device_pushed(Widget widget, XtPointer client_data, XtPointer call_data)

{
	int	status;
	int	device = 1;

	status = acq_storage( device );
	if ( status == -1 ) 
	   Media_On = 0;
	else
	   Media_On = 1;

	exano  = 1;
	status = set_pushbutton_status( Is_Started, Media_On );

}

/*--------------------------------------------------------------------------- */
	int		exano;			/* Exabyte drive number */

void exb2_device_pushed(Widget widget, XtPointer client_data, XtPointer call_data)

{
	int	status;
	int	device = 2;

	status = acq_storage( device );
	if ( status == -1 ) 
	   Media_On = 0;
	else
	   Media_On = 1;

	exano  = 2;
	status = set_pushbutton_status( Is_Started, Media_On );

}

/*--------------------------------------------------------------------------- */
	int		exano;		      /* Exabyte drive number (3=disk)*/

void disc_device_pushed(Widget widget, XtPointer client_data, XtPointer call_data)

{
	int	status;
	int	device = 3;

	status = acq_storage( device );
	if ( status == -1 ) 
	   Media_On = 0;
	else
	   Media_On = 1;

        exano  = 3;
	status = set_pushbutton_status( Is_Started, Media_On );

}





/*--------------------------------------------------------------------------- */
void current_position_pushed(Widget widget, XtPointer client_data, XtPointer call_data)

{
	int	status;
	int 	cup  	= 1;
	int	bot  	= 0;
	int	eod    	= 0;
	int	fileno 	= 0;
	int	recno 	= 0;

	char	err1[1024]= "*** ERROR *** Could not start at current position";

	status = acq_position( cup, bot, eod, fileno, recno );
	if ( status == -1 ) {
 	   errprint("%s\n", err1);
	}


}

/*--------------------------------------------------------------------------- */
void bot_position_pushed(Widget widget, XtPointer client_data, XtPointer call_data)

{
	int	status;
	int 	cup  	= 0;
	int	bot  	= 1;
	int	eod    	= 0;
	int	fileno 	= 0;
	int	recno 	= 0;

	char	err1[1024]= "*** ERROR *** Could not position to BOT";

	status = acq_position( cup, bot, eod, fileno, recno );
	if ( status == -1 ) {
	   errprint("%s\n", err1);
	}
}

/*--------------------------------------------------------------------------- */
void eod_position_pushed(Widget widget, XtPointer client_data, XtPointer call_data)

{
	int	status;
	int 	cup  	= 0;
	int	bot  	= 0;
	int	eod    	= 1;
	int	fileno 	= 0;
	int	recno 	= 0;

	char	err1[1024]= "*** ERROR *** Could not position to EOD";

	status = acq_position( cup, bot, eod, fileno, recno );
	if ( status == -1 ) {
 	   errprint("%s\n", err1);
	}
	
}

/*--------------------------------------------------------------------------- */
void file_position_pushed(Widget widget, XtPointer client_data, XtPointer call_data)

{

    	Widget 	dialog;
    	XmString t = XmStringCreateLocalized ("Enter File Number:");
    	Arg 	args[5];
    	int 	n = 0;

    	extern void read_fileno( );


	/* Create the dialog */
    	XtSetArg (args[n], XmNselectionLabelString, t); n++;
    	XtSetArg (args[n], XmNautoUnmanage, False); n++;

    	dialog = XmCreatePromptDialog (MainWindow, "Filenumber", args, n);
    	XmStringFree (t); /* always destroy compound strings when done */
    	XtAddCallback (dialog, XmNokCallback, read_fileno, widget);
   	XtAddCallback (dialog, XmNcancelCallback, (void *)XtDestroyWidget, NULL);
    	XtSetSensitive (XmSelectionBoxGetChild (dialog, XmDIALOG_HELP_BUTTON), False);
    	XtManageChild (dialog);

   	XtPopup (XtParent (dialog), XtGrabNone);

}

void read_fileno( Widget widget, XtPointer client_data, XtPointer call_data )
{
	char 	*cfile;
	char	err1[1024]= "*** ERROR *** Could not position to file ";
	int	status;
	int 	cup  	= 0;
	int	bot  	= 0;
	int	eod    	= 0;
	int	fileno 	= 0;
	int	recno 	= 0;

    	Widget push_button = (Widget) client_data;  
    	XmSelectionBoxCallbackStruct *cbs =  (XmSelectionBoxCallbackStruct *) call_data;
	
    	if (!XmStringGetLtoR (cbs->value, XmFONTLIST_DEFAULT_TAG, &cfile))
        	return; /* must have been an internal error */

    	XtDestroyWidget(widget);


	fileno = atoi( cfile );
	status = acq_position( cup, bot, eod, fileno, recno );
	if ( status == -1 ) {
 	   errprint("%s%d\n", err1, fileno);
	}

}



/*--------------------------------------------------------------------------- */
void unload_pushed(Widget widget, XtPointer client_data, XtPointer call_data)
{
	int	status;
	char	err[1024] = "*** ERROR *** Stop acquisition first !";

        if ( Is_Started == 0) {
	   status   = acq_unload();
           Media_On = 0;
	} else {
	   errprint("%s\n",err);
	}
	Is_Started = 0;	   
	status = set_pushbutton_status( Is_Started, Media_On );

}


/*--------------------------------------------------------------------------- */
void gaininit_pushed(Widget widget, XtPointer client_data, XtPointer call_data)

{
   void	filecopy(FILE *,FILE *);

	FILE	*ifp;
	FILE	*ofp;
	const char infile[]  = "/Applications/sirius/data/gainshift.init";
	const char outfile[] = "/Applications/sirius/data/gainshift.tmp";

	char	err5[1024] = "*** ERROR *** Could not open source file";
	char	err6[1024] = "*** ERROR *** Could not open destination file";

	extern void   get_file();

	/* Open files ... */
	if (( ifp = fopen(infile,"r")) == NULL) {
	   errprint("%s\n",err5);
	   return;
	}
	if (( ofp = fopen(outfile,"w"))  == NULL) {
	   errprint("%s\n",err6);
	   return;
	}

	/* Copy content of input file to output */
	filecopy (ifp, ofp);
	fclose(ifp);
	fclose(ofp);

}

/*--------------------------------------------------------------------------- */
void gainfile_pushed(Widget widget, XtPointer client_data, XtPointer call_data)

{
	int	status;
	char	err[1024] = "*** ERROR *** Stop acquisition first !";

    	Widget 	dialog;
    	XmString t = XmStringCreateLocalized ("File Name:");
    	Arg 	args[5];
    	int 	n = 0;

	extern void   get_file();

   
	/* Create the dialog */
    	XtSetArg (args[n], XmNselectionLabelString, t); n++;
    	XtSetArg (args[n], XmNautoUnmanage, False); n++;

    	dialog = XmCreatePromptDialog (MainWindow, "Filenumber", args, n);
    	XmStringFree (t); /* always destroy compound strings when done */
    	XtAddCallback (dialog, XmNokCallback, get_file, widget);
   	XtAddCallback (dialog, XmNcancelCallback, (void *)XtDestroyWidget, NULL);
    	XtSetSensitive (XmSelectionBoxGetChild (dialog, XmDIALOG_HELP_BUTTON), False);
    	XtManageChild (dialog);
        XtPopup (XtParent (dialog), XtGrabNone);

}

void get_file( Widget widget, XtPointer client_data, XtPointer call_data)
{
	FILE	*ifp;
	FILE	*ofp;
	char 	*filename;
	int	filestat;
	const char outfile[] = "/Applications/sirius/data/gainshift.tmp";
	char 	string[128];
	char	err1[1024] = "*** ERROR *** No file selected";
	char	err2[1024] = "*** ERROR *** File not found";
	char	err3[1024] = "*** ERROR *** File is a directory";
	char	err4[1024] = "*** ERROR *** File is not readable";
	char	err5[1024] = "*** ERROR *** Could not open source file";
	char	err6[1024] = "*** ERROR *** Could not open destination file";

	void	filecopy(FILE *,FILE *);

    	Widget 	push_button = (Widget) client_data;  

   	XmSelectionBoxCallbackStruct *cbs =  (XmSelectionBoxCallbackStruct *) call_data;
	
    	if (!XmStringGetLtoR (cbs->value, XmFONTLIST_DEFAULT_TAG, &filename))
        	return; /* must have been an internal error */


    	if (!*filename) { /* nothing typed? */
	   errprint("%s\n",err1);
           XtFree( filename); /* even "" is an allocated byte */
           return;
    	}

	/* Check file for exsistence, not directory and read access */
	filestat = is_writable(filename);
	if ( filestat == -1 ) {
	   errprint("%s\n",err2);
	   return;
	}
	else if ( filestat == -2 ) {
	   errprint("%s\n",err3);
	   return;
	}
	else if ( filestat == -3 ) {
	   errprint("%s\n",err4);
	   return;
	}
	   
	/* Open files ... */
	if (( ifp = fopen(filename,"r")) == NULL) {
	   errprint("%s\n",err5);
	   return;
	}
	if (( ofp = fopen(outfile,"w"))  == NULL) {
	   errprint("%s\n",err6);
	   return;
	}


	/* Copy content of input file to output */
	filecopy (ifp, ofp);
	fclose(ifp);
	fclose(ofp);

    	XtFree (filename);
    	XtDestroyWidget(widget);
}


/*--------------------------------------------------------------------------- */
void gainshow_pushed(Widget widget, XtPointer client_data, XtPointer call_data)

{
    	char	buf[1024];
	char	c;
	int	i;
	int	buflength = 1024;
    	FILE	*routine;

	routine = fopen( "/Applications/sirius/data/gainshift.tmp","r" );

	for (i=0; i<buflength-1 && (c = getc(routine)) && c != EOF; i++)
	   buf[i] = toascii(c);
	buf[i] = '\0';

        fclose( routine );	
    	
    	wprint("%s\n\n", buf);
}







/*--------------------------------------------------------------------------- */
void telewininit_pushed(Widget widget, XtPointer client_data, XtPointer call_data)

{
   void	filecopy(FILE *,FILE *);

	FILE	*ifp;
	FILE	*ofp;
	const char infile[]  = "/Applications/sirius/data/telewin.init";
	const char outfile[] = "/Applications/sirius/data/telewin.tmp";

	char	err5[1024] = "*** ERROR *** Could not open source file";
	char	err6[1024] = "*** ERROR *** Could not open destination file";

	extern void   get_filetelewin();

	/* Open files ... */
	if (( ifp = fopen(infile,"r")) == NULL) {
	   errprint("%s\n",err5);
	   return;
	}
	if (( ofp = fopen(outfile,"w"))  == NULL) {
	   errprint("%s\n",err6);
	   return;
	}

	/* Copy content of input file to output */
	filecopy (ifp, ofp);
	fclose(ifp);
	fclose(ofp);

}


/*--------------------------------------------------------------------------- */
void telewinfile_pushed(Widget widget, XtPointer client_data, XtPointer call_data)
{
	int	status;
	char	err[1024] = "*** ERROR *** Stop acquisition first !";

	Widget 	dialog;
	XmString t = XmStringCreateLocalized ("File Name:");
	Arg 	args[5];
	int 	n = 0;

	extern void   get_filetelewin();

	/* Create the dialog */
	XtSetArg (args[n], XmNselectionLabelString, t); n++;
	XtSetArg (args[n], XmNautoUnmanage, False); n++;

	dialog = XmCreatePromptDialog (MainWindow, "Filenumber", args, n);
	XmStringFree (t); /* always destroy compound strings when done */
	XtAddCallback (dialog, XmNokCallback, get_filetelewin, widget);
   	XtAddCallback (dialog, XmNcancelCallback, (void *)XtDestroyWidget, NULL);
	XtSetSensitive (XmSelectionBoxGetChild (dialog, XmDIALOG_HELP_BUTTON), False);
	XtManageChild (dialog);
	XtPopup (XtParent (dialog), XtGrabNone);
}

void get_filetelewin( Widget widget, XtPointer client_data, XtPointer call_data)
{
	FILE	*ifp;
	FILE	*ofp;
	char 	*filename;
	int		filestat;
	const char outfile[] = "/Applications/sirius/data/telewin.tmp";
	char 	string[128];
	char	err1[1024] = "*** ERROR *** No file selected";
	char	err2[1024] = "*** ERROR *** File not found";
	char	err3[1024] = "*** ERROR *** File is a directory";
	char	err4[1024] = "*** ERROR *** File is not readable";
	char	err5[1024] = "*** ERROR *** Could not open source file";
	char	err6[1024] = "*** ERROR *** Could not open destination file";

   void	filecopy(FILE *,FILE *);

	Widget 	push_button = (Widget) client_data;  
   	XmSelectionBoxCallbackStruct *cbs =  (XmSelectionBoxCallbackStruct *) call_data;
	
    	if (!XmStringGetLtoR (cbs->value, XmFONTLIST_DEFAULT_TAG, &filename))
        	return; /* must have been an internal error */


    	if (!*filename) { /* nothing typed? */
	   errprint("%s\n",err1);
           XtFree( filename); /* even "" is an allocated byte */
           return;
    	}

	/* Check file for exsistence, not directory and read access */
	filestat = is_writable(filename);
	if ( filestat == -1 ) {
	   errprint("%s\n",err2);
	   return;
	}
	else if ( filestat == -2 ) {
	   errprint("%s\n",err3);
	   return;
	}
	else if ( filestat == -3 ) {
	   errprint("%s\n",err4);
	   return;
	}
	   
	/* Open files ... */
	if (( ifp = fopen(filename,"r")) == NULL) {
	   errprint("%s\n",err5);
	   return;
	}
	if (( ofp = fopen(outfile,"w"))  == NULL) {
	   errprint("%s\n",err6);
	   return;
	}


	/* Copy content of input file to output */
	filecopy (ifp, ofp);
	fclose(ifp);
	fclose(ofp);

    	XtFree (filename);
    	XtDestroyWidget(widget);
}


/*--------------------------------------------------------------------------- */
void telewinshow_pushed(Widget widget, XtPointer client_data, XtPointer call_data)

{
	char	buf[1024];
	char	c;
	int	i;
	int	buflength = 1024;
	FILE	*routine;

	routine = fopen( "/Applications/sirius/data/telewin.tmp","r" );

	for (i=0; i<buflength-1 && (c = getc(routine)) && c != EOF; i++)
	   buf[i] = toascii(c);
	buf[i] = '\0';

        fclose( routine );	
    	
    	wprint("%s\n\n", buf);
}









/*--------------------------------------------------------------------------- */
void load_pushed(Widget widget, XtPointer client_data, XtPointer call_data)
{
    	Widget 	dialog;
    	XmString t = XmStringCreateLocalized ("Sorting Function File Name:");
    	Arg 	args[5];
    	int 	n = 0;

	extern void   echo_file();


	/* Create the dialog */
    	XtSetArg (args[n], XmNselectionLabelString, t); n++;
    	XtSetArg (args[n], XmNautoUnmanage, False); n++;

    	dialog = XmCreatePromptDialog (MainWindow, "Filenumber", args, n);
    	XmStringFree (t); /* always destroy compound strings when done */
    	XtAddCallback (dialog, XmNokCallback, echo_file, widget);
   	XtAddCallback (dialog, XmNcancelCallback, (void *)XtDestroyWidget, NULL);
    	XtSetSensitive (XmSelectionBoxGetChild (dialog, XmDIALOG_HELP_BUTTON), False);
    	XtManageChild (dialog);

   	XtPopup (XtParent (dialog), XtGrabNone);
	

}
void echo_file( Widget widget, XtPointer client_data, XtPointer call_data)
{
	FILE	*ifp;
	FILE	*ofp;
    	char 	*filename;
	int	filestat;
	const char outfile[] = "/Applications/sirius/src/sort/user_routine.c";
	char 	string[128];
	char	err1[1024] = "*** ERROR *** No file selected";
	char	err2[1024] = "*** ERROR *** File not found: /Applications/sirius/src/sort/user_routine.c";
	char	err3[1024] = "*** ERROR *** File is a directory: /Applications/sirius/src/sort/user_routine.c";
	char	err4[1024] = "*** ERROR *** File is not readable: /Applications/sirius/src/sort/user_routine.c";

	void	filecopy(FILE *,FILE *);

    	Widget 	push_button = (Widget) client_data;  

   	XmSelectionBoxCallbackStruct *cbs =  (XmSelectionBoxCallbackStruct *) call_data;
	
    	if (!XmStringGetLtoR (cbs->value, XmFONTLIST_DEFAULT_TAG, &filename))
        	return; /* must have been an internal error */


    	if (!*filename) { /* nothing typed? */
	   errprint("%s\n",err1);
           XtFree( filename); /* even "" is an allocated byte */
           return;
    	}

	/* Check file for exsistence, not directory and read access */
	filestat = is_writable(filename);
	if ( filestat == -1 ) {
	   errprint("%s\n",err2);
	   return;
	}
	else if ( filestat == -2 ) {
	   errprint("%s\n",err3);
	   return;
	}
	else if ( filestat == -3 ) {
	   errprint("%s\n",err4);
	   return;
	}
	   
	/* Open files ... */
	if (( ifp = fopen(filename,"r")) == NULL) {
	   errprint("%s\n",err2);
	   return;
	}
	if (( ofp = fopen(outfile,"w"))  == NULL) {
	   errprint("%s\n",err3);
	   return;
	}


	/* Copy content of input file to output */
	filecopy (ifp, ofp);
	fclose(ifp);
	fclose(ofp);

    	XtFree (filename);
    	XtDestroyWidget(widget);

		system("xterm -bg khaki -fg black -geometry 100x25-50+0 -e /Applications/sirius/bin/loadsort &");

}

/*--------------------------------------------------------------------------- */
void show_pushed(Widget widget, XtPointer client_data, XtPointer call_data)
{
	char	buf[1024];
	char	c;
	int	i;
	int	buflength = 1024;
	FILE	*routine;

	routine = fopen( "/Applications/sirius/src/sort/user_routine.c","r+" );

	for (i=0; i<buflength-1 && (c = getc(routine)) && c != EOF && c != '?'; i++)
	   buf[i] = toascii(c);
	buf[i] = '\0';

	fclose( routine );	
    	
	wprint("%s\n\n", buf);

}


/*--------------------------------------------------------------------------- */
void h_about_pushed(Widget widget, XtPointer client_data, XtPointer call_data)
{


	Widget dialog;
	XmString text;
	Arg args[5];
    	Pixel		fg, bg;
    
	XtVaGetValues (	MainWindow,
        		XmNforeground, &fg,
        		XmNbackground, &bg,
        		NULL);

        pixmap = XmGetPixmap (XtScreen (newcol), "/Applications/sirius/bitmap/logo", fg, bg);


	text = XmStringCreateLtoR ("      Welcome to SIRIUS 1.0\n\n     Oslo Cyclotron Laboratory\n         Data Acquisiton System\n\n          tore.ramsoy@nrpa.no\n   magne.guttormsen@fys.uio.no\n            Oslo March 2008\n",XmFONTLIST_DEFAULT_TAG);


	XtSetArg ( args[0], XmNmessageString, text); 
	dialog = XmCreateInformationDialog (MainWindow,"about",args,1);
	XmStringFree ( text );
	XtUnmanageChild ( XmMessageBoxGetChild( dialog, XmDIALOG_CANCEL_BUTTON) );
	XtUnmanageChild ( XmMessageBoxGetChild( dialog, XmDIALOG_HELP_BUTTON) );
	/*XtUnmanageChild ( XmMessageBoxGetChild( dialog, XmDIALOG_SYMBOL_LABEL) ); */
	XtVaSetValues(dialog, XmNsymbolPixmap,pixmap,NULL); 

	XtManageChild( dialog );
	XtPopup ( XtParent(dialog), XtGrabNone);
 

}


/*--------------------------------------------------------------------------- */

void h_install_pushed(Widget widget, XtPointer client_data, XtPointer call_data)
{
   system("nedit /Applications/sirius/help/README &");  /* for Linux*/
/*   system("open /Applications/sirius/help/README &");  for Mac */
}


/*--------------------------------------------------------------------------- */

void h_online_pushed(Widget widget, XtPointer client_data, XtPointer call_data)
{
   system("firefox http://ocl.uio.no/sirius/ &");
/*   system("open http://ocl.uio.no/sirius/ &");  for Mac */
}




/* ============================================================================*/
/*                  This section contains utility routines                     */
/* ============================================================================*/

/* Update status for all menu options and pushbuttons according to task state */
int set_pushbutton_status( int Is_Started, int Media_On )
{

	/* Running ... */
 	if ( Is_Started == 1 ) {
  	   XtSetSensitive( file_quit, 		False );
  	   XtSetSensitive( run_start, 		False );
  	   XtSetSensitive( startb,			False );
  	   XtSetSensitive( run_stop,  		True  );
  	   XtSetSensitive( stopb,			True  );
  	   XtSetSensitive( spec_clear,  	False );
  	   XtSetSensitive( output_device,  	False );
  	   XtSetSensitive( output_position, False );
  	   XtSetSensitive( output_unload,  	False );
 	   XtSetSensitive( gain_init,  		False );
  	   XtSetSensitive( gain_file,  		False );
  	   XtSetSensitive( sort_load,  		False );
	      

	}

	/* Stopped ... */
 	if ( Is_Started == 0 ) {
  	   XtSetSensitive( file_quit, 		True  );
  	   XtSetSensitive( run_start, 		True  );
  	   XtSetSensitive( startb,			True  );
  	   XtSetSensitive( run_stop,  		False );
  	   XtSetSensitive( stopb,			False );
  	   XtSetSensitive( spec_clear,  	True  );
  	   XtSetSensitive( output_device,  	True  );
  	   XtSetSensitive( output_position, False );
  	   XtSetSensitive( output_unload,  	False );
 	   XtSetSensitive( gain_init,  		True  );
  	   XtSetSensitive( gain_file,  		True  );
  	   XtSetSensitive( sort_load,  		True );
	   XtSetSensitive( device_exb1,		True  );
	   XtSetSensitive( device_exb2,		True  );
	   XtSetSensitive( device_disc,  	True  );

	   if ( Media_On == 1) {
  	      XtSetSensitive( output_unload,True  );
  	      XtSetSensitive( output_position, True  );
	      XtSetSensitive( device_exb1,	False );
	      XtSetSensitive( device_exb2,	False );
	      XtSetSensitive( device_disc,	False );

	   }
	      
	}


	return 0;
}

/* Copy a file from ifp to ofp */
void filecopy( FILE *ifp, FILE *ofp)
{
	int	c;
	while ((c = getc(ifp)) != EOF)
	   putc(c, ofp);
}


/* Check if a file is accessible */
int is_writable(char *file)
{
    struct stat s_buf;

    /* if file can't be accessed (via stat()) return */
    if (stat (file, &s_buf) == -1)
        return -1;
    else if ((s_buf.st_mode & S_IFMT) == S_IFDIR)
        return -2; /* a directory */
    else if (!(s_buf.st_mode & S_IFREG) || access (file, R_OK) == -1)
        /* not a normal file or it is not readable */
        return -3;
    /* legitimate file */
    return 0;
}

