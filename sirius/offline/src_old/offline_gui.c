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
int			exades;					/* File descriptor for Exabyte */
int			exano;					/* Exabyte drive number */
int			*messp;					/* Pointer to shared memory message_box */
int			Is_Started = 0;			/* Status variable, 0 = NOT Started, 1 = Started */
int			Media_On = 0;			/* Status variable, 0 = NO Input, 1 = Input On */
int			Input_drive_no  = 0;	/* Drive number for data input exabyte */

Widget 		toplevel, rowcol, newcol, MainWindow;
Widget 		text_w, error_w;	/* Message output and error output windows */
Widget 		runm, run_sort, sort_whole, sort_file, sort_recs, run_stop, run_status;
Widget 		specm, spec_clear, spec_dump, spec_list, spec_mama;
Widget		filem, file_quit, file_opt, opt_format, opt_tele;
Widget		inputm, input_device, input_unload, DevicePullRight, PositionPullRight;
Widget		input_position, position_current, position_bot, position_eod, position_file, position_record;
Widget		device_none, device_exb1, device_exb2, device_Disk;
Widget		sortm, sort_load, show_load;
Widget		gainm, gain_init, gain_file, gain_show;
Widget		telewinm, telewin_init, telewin_file, telewin_show;
Widget 		helpm, help_about, help_install, help_offline;
Widget		updateb;
Pixmap		pixmap,pixmap1;

main(argc, argv)
int argc;
char *argv[];
{    

    Widget		MenuBar; 
    Widget		FilePullDown, RunPullDown, SpecPullDown, InputPullDown, GainPullDown, TeleWinPullDown, SortPullDown, HelpPullDown;

    XmString    file, run, spec, input, gain, telewin, sort, help;
    XmString	quit, quit_accel_text, text;
    XtAppContext app;
    void 	quit_pushed();
    void	options_pushed();
    void	teleno_pushed();
    void	sortwhole_pushed();
    void	sortfile_pushed();
    void	sortrecs_pushed();
    void	stop_pushed();
    void	status_pushed();		
    void	clear_pushed();		
    void	dump_pushed();		
    void	list_pushed();		
    void	mama_pushed();
    void	none_device_pushed();		
    void	exb1_device_pushed();		
    void	exb2_device_pushed();
    void	Disk_device_pushed();	
    void	current_position_pushed();		
    void	bot_position_pushed();		
    void	eod_position_pushed();
    void	file_position_pushed();	
    void	record_position_pushed();	
    void	unload_pushed();
    void	gaininit_pushed();
    void	gainfile_pushed();
    void	gainshow_pushed();
	void	telewininit_pushed();
    void	telewinfile_pushed();
    void	telewinshow_pushed();
    void	load_pushed();		
    void	show_pushed();		
    void 	h_about_pushed();
    void 	h_install_pushed();
	void	h_offline_pushed();
    void	info_pushed();

    Pixel		fg, bg;
    Dimension	ht, st;

    Arg		args[7];
    int		n = 0;
    int		status;				     
    char	*cc_quit = "Ctrl<Key>Q";


    /* Initialize the data acquistion system */
    if ( offline_init( ) == -1) {
       printf("\n **** ERROR **** Initialization failed - exiting \n");
       exit(0);
    }

    /* Build the GUI, start with Motif top level routines  */
    XtSetLanguageProc (NULL, NULL, NULL);
    toplevel = XtVaAppInitialize (&app, "Offline", NULL, 0, &argc, argv, NULL, 
					XmNminWidth,	500,
					XmNminHeight,	550,
					XmNmaxWidth,	700,
					XmNmaxHeight,	600,				  
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

    /* Create the menubar */
    MenuBar = XmCreateMenuBar 		(MainWindow, 
					"MenuBar", 
					NULL, 0); 


    /* Create the "File" pulldown menu */
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

    opt_format = XtVaCreateManagedWidget ("Data Format",
                                      xmPushButtonGadgetClass,
                                      DevicePullRight, 
                                      NULL);

    opt_tele = XtVaCreateManagedWidget ("No of Telescopes",
                                      xmPushButtonGadgetClass,
                                      DevicePullRight, 
                                      NULL);

    XtAddCallback (opt_format, XmNactivateCallback, options_pushed, NULL);  
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

    /* Create the "Run" pulldown menu */
    RunPullDown = XmCreatePulldownMenu (MenuBar, "RunPullDown", NULL, 0);
    run = XmStringCreateLocalized ("Run");
    runm = XtVaCreateManagedWidget ("Run", 
             xmCascadeButtonWidgetClass, MenuBar,
             XmNlabelString,  run,
             XmNmnemonic,    'R',
             XmNsubMenuId,    RunPullDown,
             NULL);
    XmStringFree ( run ); 

    /* Add the menu Sort */
    DevicePullRight = XmCreatePulldownMenu( RunPullDown, "DevicePullRight", NULL, 0);

    run_sort = XtVaCreateManagedWidget ("Sort",
                                      xmCascadeButtonGadgetClass, RunPullDown,
                                      XmNsubMenuId, DevicePullRight, 
                                      NULL);

    sort_whole = XtVaCreateManagedWidget ("All Files",
                                      xmPushButtonGadgetClass,
                                      DevicePullRight, 
                                      NULL);
    sort_file = XtVaCreateManagedWidget ("Files ...",
                                      xmPushButtonGadgetClass,
                                      DevicePullRight, 
                                      NULL);
    sort_recs = XtVaCreateManagedWidget ("Records ...",
                                      xmPushButtonGadgetClass,
                                      DevicePullRight, 
                                      NULL);
 
    XtAddCallback (sort_whole, XmNactivateCallback, sortwhole_pushed, NULL);
    XtAddCallback (sort_file, XmNactivateCallback,  sortfile_pushed, NULL);
    XtAddCallback (sort_recs, XmNactivateCallback,  sortrecs_pushed, NULL);

    /* Add the menu item STOP */
    run_stop = XtVaCreateManagedWidget ("Stop",
                                      xmPushButtonGadgetClass, 
                                      RunPullDown, 
                                      NULL);
    XtAddCallback (run_stop, XmNactivateCallback, stop_pushed, NULL); 

    XtVaCreateManagedWidget ("separator",
        			      xmSeparatorGadgetClass, RunPullDown, NULL);

    /* Add the menu item INFO */
    run_status = XtVaCreateManagedWidget ("Info",
                                      xmPushButtonGadgetClass, 
                                      RunPullDown, 
                                      NULL);
    XtAddCallback (run_status, XmNactivateCallback, status_pushed, NULL); 


    /* Create the "Spectra" pulldown menu */
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


    /* Create the "Input" pulldown menu */
    InputPullDown = XmCreatePulldownMenu (MenuBar, "InputPullDown", NULL, 0);
    input = XmStringCreateLocalized ("Input");
    inputm = XtVaCreateManagedWidget ("Input", 
             xmCascadeButtonWidgetClass, MenuBar,
             XmNlabelString,  input,
             XmNmnemonic,    'I',
             XmNsubMenuId,    InputPullDown,
             NULL);
    XmStringFree ( input ); 

    /* Add the menu DEVICE */
    DevicePullRight = XmCreatePulldownMenu( InputPullDown, "DevicePullRight", NULL, 0);

    input_device = XtVaCreateManagedWidget ("Device",
                                      xmCascadeButtonGadgetClass, InputPullDown,
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
    device_Disk = XtVaCreateManagedWidget ("Disk",
                                      xmPushButtonGadgetClass,
                                      DevicePullRight, 
                                      NULL);
    XtAddCallback (device_none, XmNactivateCallback, none_device_pushed, NULL);
    XtAddCallback (device_exb1, XmNactivateCallback, exb1_device_pushed, NULL);
    XtAddCallback (device_exb2, XmNactivateCallback, exb2_device_pushed, NULL);
    XtAddCallback (device_Disk, XmNactivateCallback, Disk_device_pushed, NULL);



    /* Add the menu POSITION */
    PositionPullRight = XmCreatePulldownMenu( InputPullDown, "PositionPullRight", NULL, 0);

    input_position = XtVaCreateManagedWidget ("Position at",
                                      xmCascadeButtonGadgetClass, InputPullDown,
                                      XmNsubMenuId, PositionPullRight, 
                                      NULL);

    position_current = XtVaCreateManagedWidget ("Current Position",
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
    position_record= XtVaCreateManagedWidget ("Record no...",
                                      xmPushButtonGadgetClass,
                                      PositionPullRight, 
                                      NULL);

 
    XtAddCallback (position_current, XmNactivateCallback, current_position_pushed, NULL);
    XtAddCallback (position_bot, XmNactivateCallback, bot_position_pushed, NULL);
    XtAddCallback (position_eod, XmNactivateCallback, eod_position_pushed, NULL);
    XtAddCallback (position_file, XmNactivateCallback, file_position_pushed, NULL); 
    XtAddCallback (position_record, XmNactivateCallback, record_position_pushed, NULL); 


    XtVaCreateManagedWidget ("separator",
        			      xmSeparatorGadgetClass, InputPullDown, NULL);

    /* Add the menu item UNLOAD */
    input_unload = XtVaCreateManagedWidget ("Unload/Close",
                                      xmPushButtonGadgetClass, 
                                      InputPullDown, 
                                      NULL);
    XtAddCallback (input_unload, XmNactivateCallback, unload_pushed, NULL); 

    /* Create the "Gain" pulldown menu */
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
    gain_show = XtVaCreateManagedWidget ("Show Values",
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
	
	
	
	
	
	

      /* Create the "Sorting" pulldown menu */
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


    /* Create the "Help" pulldown menu */
    HelpPullDown = XmCreatePulldownMenu (MenuBar, "HelpPullDown", NULL, 0);

    /* Create the "Help" button */
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
    help_about = XtVaCreateManagedWidget 	("About Offline",
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

    /* Add menu "Offline" */
    help_offline = XtVaCreateManagedWidget 		("Offline",
        					xmPushButtonGadgetClass, 
						HelpPullDown, 
						NULL);
    XtAddCallback (help_offline, XmNactivateCallback, h_offline_pushed, NULL);

    XtManageChild (MenuBar);


    /* Add pushbuttons below the menubar */

    XtVaSetValues (	newcol, 
			XmNpacking, 	XmPACK_COLUMN,
			XmNnumColumns, 	1,
			XmNorientation, XmHORIZONTAL,
			NULL); 

    XtVaGetValues (	newcol,
        		XmNforeground, &fg,
        		XmNbackground, &bg,
        		NULL);

    pixmap1 = XmGetPixmap (XtScreen (newcol), "/Applications/sirius/bitmap/info", fg, bg);
	
    updateb = XtVaCreateManagedWidget ("updateb", 
					xmDrawnButtonWidgetClass, newcol, 
					NULL);

    XtVaGetValues (	updateb, 
        		XmNhighlightThickness, &ht,
        		XmNshadowThickness, &st,
        		NULL);

    XtVaSetValues( updateb, XmNwidth,  2 * ht +2 * st + 26,
                            XmNheight, 2 * ht +2 * st + 26,
                            NULL);

    XtAddCallback (updateb, XmNactivateCallback, info_pushed, NULL);
    XtAddCallback (updateb, XmNexposeCallback, info_pushed, NULL);


    XtManageChild (newcol);
   

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
    XtVaSetValues(error_w, XtVaTypedArg,XmNforeground, XmRString,"red",7,NULL);
    XtManageChild (error_w); 

    XtManageChild (rowcol);   

    status = set_pushbutton_status();


    XtRealizeWidget (toplevel);
    XtAppMainLoop (app);
}




/* ============================================================================*/
/*               This section contains the call-back routines                  */
/* ============================================================================*/

void quit_pushed(Widget widget, XtPointer client_data, XtPointer call_data)

{
	Widget 		ex_dialog;
	XmString	yes, no, msg;	
	void	exit_prog();

	ex_dialog = XmCreateQuestionDialog (MainWindow, "OFFLINE: quit confirmation", NULL, 0);
	yes = XmStringCreateLocalized("Yes");
	no  = XmStringCreateLocalized("No");
	msg = XmStringCreateLocalized("Are you sure you want to quit OFFLINE?");
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

	char	err2[1024] = "*** ERROR *** Detach Databuffer Failed";
 	      		

	/* Detach message box shared memory */
	if ( shmdt( messp ) == -1) {
           errprint("%s\n", err2);
        }
		
	close ( exades );
	
	/* Detach shared histogram memory */
	shared_hist ( DETTAC );
	
	remove("/Applications/sirius/offline/system/offline.lock");
	remove("/Applications/sirius/offline/system/specdump.lock");
	remove("/Applications/sirius/offline/system/specclear.lock");

	exit(0);
}


void options_pushed(Widget widget, XtPointer client_data, XtPointer call_data)
{
	Widget  dialog, toggle;
	Arg 	args[5];
	int 	n = 0;
	int	inputtype_off = 3;
	int	*typep;
	extern void select_pushed();


   	XmString t = XmStringCreateLocalized ("Select Input Format:");
	XmString btn1 = XmStringCreateLocalized ("SIRIUS Input format");
	XmString btn2  = XmStringCreateLocalized ("DAISY Input format");

	typep = messp + inputtype_off;

	/* Create the dialog, return to previous select state */
	XtSetArg (args[n], XmNautoUnmanage, False); n++; 
	dialog = XmCreateQuestionDialog (MainWindow, "Format", args, n);
	XmStringFree (t); /* always destroy compound strings when done */
	XtAddCallback (dialog, XmNokCallback, (void *)XtDestroyWidget, NULL); 
  	XtUnmanageChild (XmMessageBoxGetChild (dialog, XmDIALOG_HELP_BUTTON)); 
  	XtUnmanageChild (XmMessageBoxGetChild (dialog, XmDIALOG_CANCEL_BUTTON)); 

	if (*typep == 0) {
	   toggle = XmVaCreateSimpleRadioBox ( dialog, "toggle", 
						0, 
						select_pushed, 
						XmVaRADIOBUTTON, btn1, NULL, NULL, NULL,
						XmVaRADIOBUTTON, btn2, NULL, NULL, NULL,
						NULL);
	}
	
	if (*typep == 1) {
	   toggle = XmVaCreateSimpleRadioBox ( dialog, "toggle", 
						1, 
						select_pushed, 
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

void select_pushed(Widget widget, XtPointer client_data, XtPointer call_data)
{
	int		inputtype_off	= 3;
	int		*typep;
	int		teleno_off		= 11;
	int		*telenop;

	int	n = (int) client_data;
    	XmToggleButtonCallbackStruct *cbs =  (XmToggleButtonCallbackStruct *) call_data;

	typep = messp + inputtype_off;
	telenop = messp + teleno_off;

	if ( cbs->set == False)
	   return;

	if (n == 0) {		/* SIRIUS */
	   *typep = 0;
	} 
	if (n == 1) {		/* DAISY */
	   *typep = 1;
	   *telenop = 8;
	}

}

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

void sortwhole_pushed(Widget widget, XtPointer client_data, XtPointer call_data)
{
	int		files2do_off = 7;
	int		recs2do_off  = 8;
	int		*files2dop;
	int		*recs2dop;

	int		status;
	char	err[1024] = "*** ERROR *** Already started !";

	files2dop = messp + files2do_off;
	recs2dop  = messp + recs2do_off;

	/* Update Message_Box slots for files2do and recs2do */
	*files2dop = 0;
	*recs2dop  = 0;

	if ( Is_Started == 0)
	   status = offline_start( );
	else
	   errprint("%s\n",err);	
	   
	if ( status == -1 ) 
	   Is_Started = 0;
	else
	   Is_Started = 1;
	
	status = set_pushbutton_status();

}

void sortfile_pushed(Widget widget, XtPointer client_data, XtPointer call_data)
{
	Widget 	dialog;
	XmString t = XmStringCreateLocalized ("Enter Number of Files to Sort:");
	Arg 	args[5];
	int 	n = 0;

	extern void get_fileno( );


	/* Create the dialog */
	XtSetArg (args[n], XmNselectionLabelString, t); n++;
	XtSetArg (args[n], XmNautoUnmanage, False); n++;

	dialog = XmCreatePromptDialog (MainWindow, "Filenumber", args, n);
	XmStringFree (t); /* always destroy compound strings when done */
	XtAddCallback (dialog, XmNokCallback, get_fileno, widget);
   	XtAddCallback (dialog, XmNcancelCallback, (void *)XtDestroyWidget, NULL);
	XtSetSensitive (XmSelectionBoxGetChild (dialog, XmDIALOG_HELP_BUTTON), False);
	XtManageChild (dialog);
   	XtPopup (XtParent (dialog), XtGrabNone);
}

void get_fileno( Widget widget, XtPointer client_data, XtPointer call_data )
{
	int		files2do_off = 7;
	int		recs2do_off  = 8;
	int		*files2dop;
	int		*recs2dop;

	char 	*cfile;
	int		status;
	int		fileno;
	int		recno;
	char	err[1024] = "*** ERROR *** Already started !";

	Widget push_button = (Widget) client_data;  
	XmSelectionBoxCallbackStruct *cbs =  (XmSelectionBoxCallbackStruct *) call_data;
	
	if (!XmStringGetLtoR (cbs->value, XmFONTLIST_DEFAULT_TAG, &cfile))
		return; /* must have been an internal error */
	XtDestroyWidget(widget);

	fileno = atoi( cfile );

	/* Update Message_Box slots for files2do and recs2do */
	files2dop = messp + files2do_off;
	recs2dop  = messp + recs2do_off;
	*files2dop = fileno;
	*recs2dop  = 0;

	if ( Is_Started == 0)
	   status = offline_start( ); 
	else
	   errprint("%s\n",err); 	
	   
	if ( status == -1 ) 
	   Is_Started = 0;
	else
	   Is_Started = 1;
	
	status = set_pushbutton_status();

}

void sortrecs_pushed(Widget widget, XtPointer client_data, XtPointer call_data)
{

    	Widget 	dialog;
    	XmString t = XmStringCreateLocalized ("Enter Number of Records to Sort:");
    	Arg 	args[5];
    	int 	n = 0;

    	extern void get_recno( );


	/* Create the dialog */
    	XtSetArg (args[n], XmNselectionLabelString, t); n++;
    	XtSetArg (args[n], XmNautoUnmanage, False); n++;

    	dialog = XmCreatePromptDialog (MainWindow, "Filenumber", args, n);
    	XmStringFree (t); /* always destroy compound strings when done */
    	XtAddCallback (dialog, XmNokCallback, get_recno, widget);
   	XtAddCallback (dialog, XmNcancelCallback, (void *)XtDestroyWidget, NULL);
    	XtSetSensitive (XmSelectionBoxGetChild (dialog, XmDIALOG_HELP_BUTTON), False);
    	XtManageChild (dialog);

   	XtPopup (XtParent (dialog), XtGrabNone);

}
void get_recno( Widget widget, XtPointer client_data, XtPointer call_data )
{

	int		files2do_off = 7;
	int		recs2do_off  = 8;
	int		*files2dop;
	int		*recs2dop;

	char 		*cfile;
	int		status;
	int		recno;
	char		err[1024] = "*** ERROR *** Already started !";

    	Widget push_button = (Widget) client_data;  
    	XmSelectionBoxCallbackStruct *cbs =  (XmSelectionBoxCallbackStruct *) call_data;
	
    	if (!XmStringGetLtoR (cbs->value, XmFONTLIST_DEFAULT_TAG, &cfile))
        	return; /* must have been an internal error */

    	XtDestroyWidget(widget);


	recno = atoi( cfile );

	/* Update Message_Box slots for files2do and recs2do */
	files2dop = messp + files2do_off;
	recs2dop  = messp + recs2do_off;
	*files2dop = 0;
	*recs2dop  = recno;


        if ( Is_Started == 0)
	   status = offline_start( ); 
	else
	   errprint("%s\n",err); 	
	   
	if ( status == -1 ) 
	   Is_Started = 0;
	else
	   Is_Started = 1;
	
	status = set_pushbutton_status( );

}

/* --------------------------------------------------------------------------- */
void stop_pushed(Widget widget, XtPointer client_data, XtPointer call_data)

{
	int	status;
	char	err[1024] = "*** ERROR *** Not started !";

	if ( Is_Started == 1)
	   status = offline_stop( );
	else
	   errprint("%s\n",err);	
	   
	if ( status == -1 ) 
	   Is_Started = 1;
	else
	   Is_Started = 0;

	status = set_pushbutton_status( );

}

/*--------------------------------------------------------------------------- */
void status_pushed(Widget widget, XtPointer client_data, XtPointer call_data)

{
	int status;
	
	status = offline_status( );
	status = set_pushbutton_status( );

}




/*--------------------------------------------------------------------------- */
void info_pushed(Widget w, XtPointer client_data, XtPointer call_data)

{

	
	int status;
    	XmDrawnButtonCallbackStruct *cbs = 
        (XmDrawnButtonCallbackStruct *) call_data;
    	if (cbs->reason == XmCR_ACTIVATE) {
	   status = offline_status( );
	   status = set_pushbutton_status( );
    	}

    	else if (cbs->reason == XmCR_EXPOSE) {
           Dimension ht, st;

           XtVaGetValues (	w, 
            			XmNhighlightThickness, &ht,
            			XmNshadowThickness, &st,
            			NULL);

        XtVaSetValues 	(	w,
            			XmNwidth, 2 * ht + 2 * st + 26,
            			XmNheight, 2 * ht + 2 * st + 26,
            			NULL);

        XCopyArea 	(	XtDisplay (w), pixmap1, XtWindow (w), 
            			XDefaultGCOfScreen (XtScreen (w)), -8, -2, 26, 26, 
            			ht + st, ht + st);
    	}
}



/*--------------------------------------------------------------------------- */
void clear_pushed(Widget widget, XtPointer client_data, XtPointer call_data)

{
	int	status;
	char	err[1024] = "*** ERROR *** Stop acquisition first !";

        if ( Is_Started == 0)
	   status = offline_clear( );
	else
	   errprint("%s\n",err);	
}

/*--------------------------------------------------------------------------- */
void dump_pushed(Widget widget, XtPointer client_data, XtPointer call_data)

{
	int	status;
	status = offline_dump( );

}

/*--------------------------------------------------------------------------- */
void list_pushed(Widget widget, XtPointer client_data, XtPointer call_data)
{
	char	buf[1024];
	char	c;
	int	i;
	int	buflength = 1024;
	FILE	*speclist;

	speclist = fopen( "/Applications/sirius/offline/help/spectra.list","r+" );

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

	status = offline_storage( device );
	Media_On = 0;
	exano = 0;
	Input_drive_no = 0;
	status = set_pushbutton_status( );

}

/*--------------------------------------------------------------------------- */
	int		exano;			/* Exabyte drive number */

void exb1_device_pushed(Widget widget, XtPointer client_data, XtPointer call_data)

{
	int	status;
	int	device = 1;

	status = offline_storage( device );
	if ( status == -1 ) 
	   Media_On = 0;
	else
	   Media_On = 1;
	   exano = 1;
	   Input_drive_no = 1;

	status = set_pushbutton_status( );

}

/*--------------------------------------------------------------------------- */
	int		exano;			/* Exabyte drive number */

void exb2_device_pushed(Widget widget, XtPointer client_data, XtPointer call_data)

{
	int	status;
	int	device = 2;

	status = offline_storage( device );
	if ( status == -1 ) 
	   Media_On = 0;
	else
	   Media_On = 1;
	   exano = 2;
	   Input_drive_no = 2;

	status = set_pushbutton_status( );

}

/*--------------------------------------------------------------------------- */
	int		exano;		      /* Exabyte drive number (3=Disk)*/

void Disk_device_pushed(Widget widget, XtPointer client_data, XtPointer call_data)

{
	int	status;
	int	device = 3;

	status = offline_storage( device );
	if ( status == -1 ) 
	   Media_On = 0;
	else
	   Media_On = 1;

	status = set_pushbutton_status( );

}


/*--------------------------------------------------------------------------- */
void current_position_pushed(Widget widget, XtPointer client_data, XtPointer call_data)

{
	int	status;
	int	cup  	= 1;
	int	bot  	= 0;
	int	eod    	= 0;
	int	fileno 	= 0;
	int	recno 	= 0;

	char	err1[1024]= "*** ERROR *** Could not start at current position";

	status = offline_position( cup, bot, eod, fileno, recno );
	if ( status == -1 ) {
 	   errprint("%s\n", err1);
	}


}

/*--------------------------------------------------------------------------- */
void bot_position_pushed(Widget widget, XtPointer client_data, XtPointer call_data)

{
	int	status;
	int cup  	= 0;
	int	bot  	= 1;
	int	eod    	= 0;
	int	fileno 	= 0;
	int	recno 	= 0;

	char	err1[1024]= "*** ERROR *** Could not position to BOT";

	status = offline_position( cup, bot, eod, fileno, recno );
	if ( status == -1 ) {
	   errprint("%s\n", err1);
	}
}

/*--------------------------------------------------------------------------- */
void eod_position_pushed(Widget widget, XtPointer client_data, XtPointer call_data)

{
	int	status;
	int	cup  	= 0;
	int	bot  	= 0;
	int	eod    	= 1;
	int	fileno 	= 0;
	int	recno 	= 0;

	char	err1[1024]= "*** ERROR *** Could not position to EOD";

	status = offline_position( cup, bot, eod, fileno, recno );
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
	int cup  	= 0;
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
	status = offline_position( cup, bot, eod, fileno, recno );
/*	if ( status == -1 ) {
 	   errprint("%s%d\n", err1, fileno);
	}*/

}

/*--------------------------------------------------------------------------- */
void record_position_pushed(Widget widget, XtPointer client_data, XtPointer call_data)

{

    	Widget 	dialog;
    	XmString t = XmStringCreateLocalized ("Enter Record Number:");
    	Arg 	args[5];
    	int 	n = 0;

    	extern void read_recordno( );


	/* Create the dialog */
    	XtSetArg (args[n], XmNselectionLabelString, t); n++;
    	XtSetArg (args[n], XmNautoUnmanage, False); n++;

    	dialog = XmCreatePromptDialog (MainWindow, "Recordnumber", args, n);
    	XmStringFree (t); /* always destroy compound strings when done */
    	XtAddCallback (dialog, XmNokCallback, read_recordno, widget);
   	XtAddCallback (dialog, XmNcancelCallback, (void *)XtDestroyWidget, NULL);
    	XtSetSensitive (XmSelectionBoxGetChild (dialog, XmDIALOG_HELP_BUTTON), False);
    	XtManageChild (dialog);

   	XtPopup (XtParent (dialog), XtGrabNone);

}

void read_recordno( Widget widget, XtPointer client_data, XtPointer call_data )
{
	char 	*cfile;
	char	err1[1024]= "*** ERROR *** Could not position to record ";
	int	status;
	int	cup  	= 0;
	int	bot  	= 0;
	int	eod    	= 0;
	int	fileno 	= -1;
	int	recno 	= 0;

    	Widget push_button = (Widget) client_data;  
    	XmSelectionBoxCallbackStruct *cbs =  (XmSelectionBoxCallbackStruct *) call_data;
	
    	if (!XmStringGetLtoR (cbs->value, XmFONTLIST_DEFAULT_TAG, &cfile))
        	return; /* must have been an internal error */

    	XtDestroyWidget(widget);

	recno = atoi( cfile );
	status = offline_position( cup, bot, eod, fileno, recno );
	if ( status == -1 ) {
 	   errprint("%s%d\n", err1, recno);
	}
}


/*--------------------------------------------------------------------------- */
void unload_pushed(Widget widget, XtPointer client_data, XtPointer call_data)
{
	int	status;
	char	err[1024] = "*** ERROR *** Stop acquisition first !";

        if ( Is_Started == 0)
	   status = offline_unload();
	else
	   errprint("%s\n",err);
	
	Media_On = 0;	   
	status = set_pushbutton_status( );

}


/*--------------------------------------------------------------------------- */
void gaininit_pushed(Widget widget, XtPointer client_data, XtPointer call_data)

{
   void	filecopy(FILE *,FILE *);

	FILE	*ifp;
	FILE	*ofp;
	const char infile[]  = "/Applications/sirius/offline/data/gainshift.init";
	const char outfile[] = "/Applications/sirius/offline/data/gainshift.tmp";

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
	int		filestat;
	const char outfile[] = "/Applications/sirius/offline/data/gainshift.tmp";
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

	routine = fopen( "/Applications/sirius/offline/data/gainshift.tmp","r" );

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
	const char infile[]  = "/Applications/sirius/offline/data/telewin.init";
	const char outfile[] = "/Applications/sirius/offline/data/telewin.tmp";

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
	const char outfile[] = "/Applications/sirius/offline/data/telewin.tmp";
	char 	string[128];
	char	err1[1024] = "*** ERROR *** No file selected";
	char	err2[1024] = "*** ERROR *** File not found: /Applications/sirius/offline/data/telewin.tmp";
	char	err3[1024] = "*** ERROR *** File is a directory: /Applications/sirius/offline/data/telewin.tmp";
	char	err4[1024] = "*** ERROR *** File is not readable: /Applications/sirius/offline/data/telewin.tmp";
	char	err5[1024] = "*** ERROR *** Could not open source file: /Applications/sirius/offline/data/telewin.tmp";
	char	err6[1024] = "*** ERROR *** Could not open destination file: /Applications/sirius/offline/data/telewin.tmp";

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

	routine = fopen( "/Applications/sirius/offline/data/telewin.tmp","r" );

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
	const char outfile[] = "/Applications/sirius/offline/src/user_routine.c";
	char 	string[128];
	char	err1[1024] = "*** ERROR *** No file selected";
	char	err2[1024] = "*** ERROR *** File not found: /Applications/sirius/offline/src/user_routine.c";
	char	err3[1024] = "*** ERROR *** File is a directory: /Applications/sirius/offline/src/user_routine.c";
	char	err4[1024] = "*** ERROR *** File is not readable: /Applications/sirius/offline/src/user_routine.c";

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

        system("xterm -bg khaki -fg black -geometry 100x25-50+0 -e /Applications/sirius/bin/loadoffsort &");

}

/*--------------------------------------------------------------------------- */
void show_pushed(Widget widget, XtPointer client_data, XtPointer call_data)
{
    	char	buf[1024];
	char	c;
	int	i;
	int	buflength = 1024;
    	FILE	*routine;

	routine = fopen( "/Applications/sirius/offline/src/user_routine.c","r+" );

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


	text = XmStringCreateLtoR ("     Welcome to OFFLINE  1.0\n\n    Oslo Cyclotron Laboratory\n         Data Sorting System\n\n          tore.ramsoy@nrpa.no\n   magne.guttormsen@fys.uio.no\n            Oslo February 2008\n",XmFONTLIST_DEFAULT_TAG);


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
   system("nedit /Applications/sirius/offline/help/README &");  /* for Linux*/
/*   system("open /Applications/sirius/offline/help/README &");  for Mac */
}


/*--------------------------------------------------------------------------- */

void h_offline_pushed(Widget widget, XtPointer client_data, XtPointer call_data)
{
   system("firefox http://ocl.uio.no/offline/ &");
/*   system("open http://ocl.uio.no/offline/ &");  for Mac */
}



/* ============================================================================*/
/*                  This section contains utility routines                     */
/* ============================================================================*/


/* Update status for all menu options and pushbuttons according to task state */

int set_pushbutton_status( )
{

	int	Media_On = 0;
	int	*offlp;
	int	offl_off = 4;
	int	*typep;
	int	inputtype_off = 3;
	typep = messp + inputtype_off;
	offlp = messp + offl_off;

	if ( exades > 0 && *typep != 3 ) {    /* Exabyte is chosen */
  	   XtSetSensitive( opt_format, 		True );
	   Media_On = 1;
	} 
	if ( exades > 0 && *typep == 3 ) {    /* Disk is chosen */
  	   XtSetSensitive( opt_format, 		False );
	   Media_On = 1;
	} 

	if ( *offlp == 1 ) {
	   Is_Started = 1;
	} 
	if ( *offlp == 0 ) {
	   Is_Started = 0;
	} 


	/* Running ... */
 	if ( Is_Started == 1 ) {
  	   XtSetSensitive( file_quit, 		False );
  	   XtSetSensitive( run_sort, 		False );
  	   XtSetSensitive( run_stop,  		True  );
  	   XtSetSensitive( spec_clear,  	False );
  	   XtSetSensitive( input_device,  	False );
  	   XtSetSensitive( input_position,  	False );
  	   XtSetSensitive( input_unload,  	False );
  	   XtSetSensitive( gain_init,  		False );
  	   XtSetSensitive( gain_file,  		False );
  	   XtSetSensitive( sort_load,  		False );
	}

	/* Stopped ... */
 	if ( Is_Started == 0 ) {
  	   XtSetSensitive( file_quit, 		True  );
  	   XtSetSensitive( run_sort, 		False );
  	   XtSetSensitive( run_stop,  		False );
  	   XtSetSensitive( spec_clear,  	True  );
  	   XtSetSensitive( input_device,  	True  );
  	   XtSetSensitive( input_position,  False );
  	   XtSetSensitive( input_unload,  	False );
  	   XtSetSensitive( gain_init,  		True  );
  	   XtSetSensitive( gain_file,  		True  );
  	   XtSetSensitive( sort_load,  		True  );
	   XtSetSensitive( device_exb1,		True  );
	   XtSetSensitive( device_exb2,		True  );
	   XtSetSensitive( device_Disk,  	True  );

	   if ( Media_On == 1) {
  	      XtSetSensitive( input_unload,	True  );
  	      XtSetSensitive( input_position,True  );
  	      XtSetSensitive( run_sort, 	True  );
	      XtSetSensitive( device_exb1,	False );
	      XtSetSensitive( device_exb2,	False );
	      XtSetSensitive( device_Disk,	False );

	   }      

	}
	return 0;
}

/*--------------------------------------------------------------------------- */
/* Copy a file from ifp to ofp */
void filecopy( FILE *ifp, FILE *ofp)
{
	int	c;
	while ((c = getc(ifp)) != EOF)
	   putc(c, ofp);
}

/*--------------------------------------------------------------------------- */
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
