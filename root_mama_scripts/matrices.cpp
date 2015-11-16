//////////////////////////////////////////////////////
//	Script to plot MAMA files in ROOT				//
//	1st version: only matrices						//
//	Date: 4 Feb 2010								//
//	written by: ACL									//
//////////////////////////////////////////////////////
{
	
	// starting root stuff
	gROOT->SetStyle("Plain");
	gStyle->SetOptTitle(0);
	gStyle->SetOptStat(0);
	gStyle->SetFillColor(1);
	
	m = (TH2F*)gROOT->FindObject("matrix1");
	if (m) m->Delete();
    m2 = (TH2F*)gROOT->FindObject("matrix2");
    if (m2) m2->Delete();
    m3 = (TH2F*)gROOT->FindObject("matrix3");
    if (m3) m3->Delete();


	// declarations
	ifstream  ifile1("mat1"), ifile2("mat2"), ifile3("mat3");
	string line;
	string cal_dummy;
	string dim_dummy;
	char pdf_filename[512];
	int dim;
	int dim_start;
	int dim_stop;
	int dim_size;
	int position;
	int file_length;
	line.resize(200);	// need long enough line to read MAMA headers
	double x_cal[3] = {0.,1.,0.};	// calibration coeffs. on x axis: a0, a1, a2
	double y_cal[3] = {0.,1.,0.};	// calibration coeffs. on y axis: a0, a1, a2
	int dx, dy;	// dimension on x and y axis
	int ix, iy;
	double value;
	double x,y;
	double number_of_counts = 0.;
	double new_y1, new_y2; 
	int sign_ycal;

	
	
	
	// read MAMA header (fixed format). The 10 first lines are info text 
	if(!getline(ifile1,line) || line.substr(0,10) != "!FILE=Disk"){	// check correct format
		printf("\n This file is not a MAMA file!!!\n ");
		exit(2);
	}	
	getline(ifile1,line);	// skip !KIND=Spectrum
	getline(ifile1,line);	// skip !LABORATORY=Oslo Cyclotron Laboratory (OCL)
	getline(ifile1,line);	// skip !EXPERIMENT=mama
	getline(ifile1,line);	// skip !COMMENT=Sorted simulated data
	getline(ifile1,line);	// skip !TIME=DATE:    19/11/09 11:47:26
	getline(ifile1,line);	// get line with calibration
	cout << "\n Reading calibration coeffs.:" << endl;
	// calibration on x axis
	cal_dummy = line.substr(20,13);	// position 20, length 13 characters
	if(!(istringstream(cal_dummy) >> x_cal[0])) cout << "Could not convert string to number." << endl;
	else cout << " Calibration coeff. a0 on x axis is: " << x_cal[0] << " keV." << endl;
	cal_dummy = line.substr(34,13);	
	if(!(istringstream(cal_dummy) >> x_cal[1])) cout << "Could not convert string to number." << endl;
	else cout << " Calibration coeff. a1 on x axis is: " << x_cal[1] << " keV/ch." << endl;
	cal_dummy = line.substr(48,13);	
	if(!(istringstream(cal_dummy) >> x_cal[2])) cout << "Could not convert string to number." << endl;
	else cout << " Calibration coeff. a2 on x axis is: " << x_cal[2] << " (keV/ch)^2." << endl;
	// calibration on y axis
	cal_dummy = line.substr(62,13);	
	if(!(istringstream(cal_dummy) >> y_cal[0])) cout << "Could not convert string to number." << endl;
	else cout << " Calibration coeff. a0 on y axis is: " << y_cal[0] << " keV." << endl;
	cal_dummy = line.substr(76,13);	
	if(!(istringstream(cal_dummy) >> y_cal[1])) cout << "Could not convert string to number." << endl;
	else cout << " Calibration coeff. a1 on y axis is: " << y_cal[1] << " keV/ch." << endl;
	cal_dummy = line.substr(90,13);	
	if(!(istringstream(cal_dummy) >> y_cal[2])) cout << "Could not convert string to number." << endl;
	else cout << " Calibration coeff. a2 on y axis is: " << y_cal[2] << " (keV/ch)^2.\n" << endl;
	getline(ifile1,line);	// skip !PRECISION=16
	getline(ifile1,line);	// get dimension
	// dimension of matrix
	dim_start = line.find_first_of("=") + 1;
	dim_dummy = line.substr(dim_start,1);
	if(!(istringstream(dim_dummy) >> dim)) cout << "Could not convert string to number." << endl;
	else cout << " Dimension of matrix is: " << dim << endl;	
	getline(ifile1,line);	// get channels
	// dimension on x axis
	dim_start = line.find_first_of(":") + 1;
	dim_stop = line.find_last_of(",");
	dim_size = dim_stop - dim_start;
	dim_dummy = line.substr(dim_start,dim_size);
	if(!(istringstream(dim_dummy) >> dx)) cout << "Could not convert string to number." << endl;
	else cout << " Dimension on x axis is: " << dx+1 << " ch." << endl;	
	dx = dx+1;
	// dimension on y axis
	dim_start = line.find_last_of(":");
	dim_stop = line.find_last_of(")");
	dim_size = dim_stop - dim_start;
	dim_dummy = line.substr(dim_start+1,dim_size-1);
	if(!(istringstream(dim_dummy) >> dy)) cout << "Could not convert string to number." << endl;
	else cout << " Dimension on y axis is: " << dy+1 << " ch." << endl;	
	dy = dy+1;

    x_cal[0] /= 1000.;
    x_cal[1] /= 1000.;
    y_cal[0] /= 1000.;
    y_cal[1] /= 1000.;
	
	
	// Make histogram
	TH2D *matrix1 = new TH2D("matrix1"," ",dx,x_cal[0],dx*x_cal[1]+x_cal[0],dy,y_cal[0],dy*y_cal[1]+y_cal[0]);
	matrix1->SetOption("colz");
	gStyle->SetPalette(1);
	
	if(sign_ycal < 0.){	// if negative calibration coeff. on y axis
		for(iy=dy;iy>0;iy--){
			for(ix=0;ix<dx;ix++){
				ifile1 >> value;
				number_of_counts += value;
				matrix1->SetBinContent(ix,iy,value);
			}
		}
	}
	else{	// if positive calibration coeff. on y axis
		for(iy=0;iy<dy;iy++){
			for(ix=0;ix<dx;ix++){
				ifile1 >> value;
				number_of_counts += value;
				matrix1->SetBinContent(ix,iy,value);
			}
		}
	}
	
	cout << " Matrix 1 is now filled." << endl;
	cout << " Total number of counts in the matrix: " << number_of_counts << endl;
	// close file
	ifile1.close();
    number_of_counts=0.;

    if(!getline(ifile2,line) || line.substr(0,10) != "!FILE=Disk"){	// check correct format
		printf("\n This file is not a MAMA file!!!\n ");
		exit(2);
	}
	getline(ifile2,line);	// skip !KIND=Spectrum
	getline(ifile2,line);	// skip !LABORATORY=Oslo Cyclotron Laboratory (OCL)
	getline(ifile2,line);	// skip !EXPERIMENT=mama
	getline(ifile2,line);	// skip !COMMENT=Sorted simulated data
	getline(ifile2,line);	// skip !TIME=DATE:    19/11/09 11:47:26
	getline(ifile2,line);	// get line with calibration
	cout << "\n Reading calibration coeffs.:" << endl;
	// calibration on x axis
	cal_dummy = line.substr(20,13);	// position 20, length 13 characters
	if(!(istringstream(cal_dummy) >> x_cal[0])) cout << "Could not convert string to number." << endl;
	else cout << " Calibration coeff. a0 on x axis is: " << x_cal[0] << " keV." << endl;
	cal_dummy = line.substr(34,13);
	if(!(istringstream(cal_dummy) >> x_cal[1])) cout << "Could not convert string to number." << endl;
	else cout << " Calibration coeff. a1 on x axis is: " << x_cal[1] << " keV/ch." << endl;
	cal_dummy = line.substr(48,13);
	if(!(istringstream(cal_dummy) >> x_cal[2])) cout << "Could not convert string to number." << endl;
	else cout << " Calibration coeff. a2 on x axis is: " << x_cal[2] << " (keV/ch)^2." << endl;
	// calibration on y axis
	cal_dummy = line.substr(62,13);
	if(!(istringstream(cal_dummy) >> y_cal[0])) cout << "Could not convert string to number." << endl;
	else cout << " Calibration coeff. a0 on y axis is: " << y_cal[0] << " keV." << endl;
	cal_dummy = line.substr(76,13);
	if(!(istringstream(cal_dummy) >> y_cal[1])) cout << "Could not convert string to number." << endl;
	else cout << " Calibration coeff. a1 on y axis is: " << y_cal[1] << " keV/ch." << endl;
	cal_dummy = line.substr(90,13);
	if(!(istringstream(cal_dummy) >> y_cal[2])) cout << "Could not convert string to number." << endl;
	else cout << " Calibration coeff. a2 on y axis is: " << y_cal[2] << " (keV/ch)^2.\n" << endl;
	getline(ifile2,line);	// skip !PRECISION=16
	getline(ifile2,line);	// get dimension
	// dimension of matrix
	dim_start = line.find_first_of("=") + 1;
	dim_dummy = line.substr(dim_start,1);
	if(!(istringstream(dim_dummy) >> dim)) cout << "Could not convert string to number." << endl;
	else cout << " Dimension of matrix is: " << dim << endl;
	getline(ifile2,line);	// get channels
	// dimension on x axis
	dim_start = line.find_first_of(":") + 1;
	dim_stop = line.find_last_of(",");
	dim_size = dim_stop - dim_start;
	dim_dummy = line.substr(dim_start,dim_size);
	if(!(istringstream(dim_dummy) >> dx)) cout << "Could not convert string to number." << endl;
	else cout << " Dimension on x axis is: " << dx+1 << " ch." << endl;
	dx = dx+1;
	// dimension on y axis
	dim_start = line.find_last_of(":");
	dim_stop = line.find_last_of(")");
	dim_size = dim_stop - dim_start;
	dim_dummy = line.substr(dim_start+1,dim_size-1);
	if(!(istringstream(dim_dummy) >> dy)) cout << "Could not convert string to number." << endl;
	else cout << " Dimension on y axis is: " << dy+1 << " ch." << endl;
	dy = dy+1;
    
        x_cal[0] /= 1000.;
        x_cal[1] /= 1000.;
        y_cal[0] /= 1000.;
        y_cal[1] /= 1000.;
	
	
	// Make histogram
	TH2D *matrix2 = new TH2D("matrix2"," ",dx,x_cal[0],dx*x_cal[1]+x_cal[0],dy,y_cal[0],dy*y_cal[1]+y_cal[0]);
	matrix2->SetOption("colz");
	gStyle->SetPalette(1);
	
	if(sign_ycal < 0.){	// if negative calibration coeff. on y axis
		for(iy=dy;iy>0;iy--){
			for(ix=0;ix<dx;ix++){
				ifile2 >> value;
				number_of_counts += value;
				matrix2->SetBinContent(ix,iy,value);
			}
		}
	}
	else{	// if positive calibration coeff. on y axis
		for(iy=0;iy<dy;iy++){
			for(ix=0;ix<dx;ix++){
				ifile2 >> value;
				number_of_counts += value;
				matrix2->SetBinContent(ix,iy,value);
			}
		}
	}
	
	cout << " Matrix 2 is now filled." << endl;
	cout << " Total number of counts in the matrix: " << number_of_counts << endl;
	// close file
	ifile2.close();
    number_of_counts=0.;

    if(!getline(ifile3,line) || line.substr(0,10) != "!FILE=Disk"){	// check correct format
		printf("\n This file is not a MAMA file!!!\n ");
		exit(2);
	}
	getline(ifile3,line);	// skip !KIND=Spectrum
	getline(ifile3,line);	// skip !LABORATORY=Oslo Cyclotron Laboratory (OCL)
	getline(ifile3,line);	// skip !EXPERIMENT=mama
	getline(ifile3,line);	// skip !COMMENT=Sorted simulated data
	getline(ifile3,line);	// skip !TIME=DATE:    19/11/09 11:47:26
	getline(ifile3,line);	// get line with calibration
	cout << "\n Reading calibration coeffs.:" << endl;
	// calibration on x axis
	cal_dummy = line.substr(20,13);	// position 20, length 13 characters
	if(!(istringstream(cal_dummy) >> x_cal[0])) cout << "Could not convert string to number." << endl;
	else cout << " Calibration coeff. a0 on x axis is: " << x_cal[0] << " keV." << endl;
	cal_dummy = line.substr(34,13);
	if(!(istringstream(cal_dummy) >> x_cal[1])) cout << "Could not convert string to number." << endl;
	else cout << " Calibration coeff. a1 on x axis is: " << x_cal[1] << " keV/ch." << endl;
	cal_dummy = line.substr(48,13);
	if(!(istringstream(cal_dummy) >> x_cal[2])) cout << "Could not convert string to number." << endl;
	else cout << " Calibration coeff. a2 on x axis is: " << x_cal[2] << " (keV/ch)^2." << endl;
	// calibration on y axis
	cal_dummy = line.substr(62,13);
	if(!(istringstream(cal_dummy) >> y_cal[0])) cout << "Could not convert string to number." << endl;
	else cout << " Calibration coeff. a0 on y axis is: " << y_cal[0] << " keV." << endl;
	cal_dummy = line.substr(76,13);
	if(!(istringstream(cal_dummy) >> y_cal[1])) cout << "Could not convert string to number." << endl;
	else cout << " Calibration coeff. a1 on y axis is: " << y_cal[1] << " keV/ch." << endl;
	cal_dummy = line.substr(90,13);
	if(!(istringstream(cal_dummy) >> y_cal[2])) cout << "Could not convert string to number." << endl;
	else cout << " Calibration coeff. a2 on y axis is: " << y_cal[2] << " (keV/ch)^2.\n" << endl;
	getline(ifile3,line);	// skip !PRECISION=16
	getline(ifile3,line);	// get dimension
	// dimension of matrix
	dim_start = line.find_first_of("=") + 1;
	dim_dummy = line.substr(dim_start,1);
	if(!(istringstream(dim_dummy) >> dim)) cout << "Could not convert string to number." << endl;
	else cout << " Dimension of matrix is: " << dim << endl;
	getline(ifile3,line);	// get channels
	// dimension on x axis
	dim_start = line.find_first_of(":") + 1;
	dim_stop = line.find_last_of(",");
	dim_size = dim_stop - dim_start;
	dim_dummy = line.substr(dim_start,dim_size);
	if(!(istringstream(dim_dummy) >> dx)) cout << "Could not convert string to number." << endl;
	else cout << " Dimension on x axis is: " << dx+1 << " ch." << endl;
	dx = dx+1;
	// dimension on y axis
	dim_start = line.find_last_of(":");
	dim_stop = line.find_last_of(")");
	dim_size = dim_stop - dim_start;
	dim_dummy = line.substr(dim_start+1,dim_size-1);
	if(!(istringstream(dim_dummy) >> dy)) cout << "Could not convert string to number." << endl;
	else cout << " Dimension on y axis is: " << dy+1 << " ch." << endl;
	dy = dy+1;
    
	
        x_cal[0] /= 1000.;
        x_cal[1] /= 1000.;
        y_cal[0] /= 1000.;
        y_cal[1] /= 1000.;
        
	// Make histogram
	TH2D *matrix3 = new TH2D("matrix3"," ",dx,x_cal[0],dx*x_cal[1]+x_cal[0],dy,y_cal[0],dy*y_cal[1]+y_cal[0]);
	matrix3->SetOption("colz");
	gStyle->SetPalette(1);
	
	if(sign_ycal < 0.){	// if negative calibration coeff. on y axis
		for(iy=dy;iy>0;iy--){
			for(ix=0;ix<dx;ix++){
				ifile3 >> value;
				number_of_counts += value;
				matrix3->SetBinContent(ix,iy,value);
			}
		}
	}
	else{	// if positive calibration coeff. on y axis
		for(iy=0;iy<dy;iy++){
			for(ix=0;ix<dx;ix++){
				ifile3 >> value;
				number_of_counts += value;
				matrix3->SetBinContent(ix,iy,value);
			}
		}
	}
	
	cout << " Matrix 3 is now filled." << endl;
	cout << " Total number of counts in the matrix: " << number_of_counts << endl;
	// close file
	ifile3.close();

	// Create TCanvas
	TCanvas *c1 = new TCanvas("c1","MAMA matrix",500,700);
    c1->Divide(1,2,0,0);

    c1.cd(1);
    
	c1_1->SetLogz();
	c1_1->SetLeftMargin(0.12);
    c1_1->SetBottomMargin(0.02);
    c1_1->SetRightMargin(0.06);
    c1_1->SetTopMargin(0.01);
	//matrix1->GetXaxis()->SetTitle("E_{#gamma} (keV)");
    matrix1->GetYaxis()->SetTitleFont(42);
    matrix1->GetYaxis()->SetLabelFont(42);
	matrix1->GetYaxis()->SetTitleOffset(0.9);
	matrix1->GetYaxis()->SetTitle("E (MeV)");
    matrix1->GetYaxis()->CenterTitle();
    matrix1->GetYaxis()->SetTitleSize(0.065);
    matrix1->GetYaxis()->SetLabelSize(0.065);

    matrix1->GetXaxis()->SetLabelOffset(10);
	// Set user range in the matrix if you want to
//	matrix1->GetXaxis()->SetRangeUser(x_cal[0],9000);	
//	matrix1->GetYaxis()->SetRangeUser(x_cal[0],9000);
    matrix1->GetZaxis()->SetRangeUser(2,2000);

	matrix1->Draw("COLZ");
        
    TLatex t;
    t.SetTextFont(42);
    t.SetTextSize(0.06);
    t.DrawLatex(2.3,1.2,"(a)");
    t.DrawLatex(2.3,0.7,"^{152}Sm(p, d#gamma )^{151}Sm");
	
    gPad->Update();
	TPaletteAxis *palette = (TPaletteAxis*)matrix1->GetListOfFunctions()->FindObject("palette");
	if(!palette) cout << "nono." << endl;
    palette->SetX1NDC(0.92);
    palette->SetX2NDC(0.94);
    palette->SetLabelSize(0.055);
    palette->SetLabelOffset(-0.01);
    palette->SetLabelFont(42);
	palette->Draw();
	
//	pl = new TPaveLabel(0.40,0.89,0.6,0.96,"^{45}Ti, protons","NDC");
//	pl->SetFillColor(0);
//	pl->SetBorderSize(5);
//	pl->Draw();
	
	c1->Update();
	
    c1.cd(2);
	c1_2->SetLogz();
    c1_2->SetLeftMargin(0.12);
    c1_2->SetRightMargin(0.06);
    c1_2->SetTopMargin(0.01);
    c1_2->SetBottomMargin(0.13);
    
    matrix2->GetYaxis()->SetTitleFont(42);
    matrix2->GetYaxis()->SetLabelFont(42);
    matrix2->GetYaxis()->SetTitleOffset(0.9);
    matrix2->GetYaxis()->SetTitle("E (MeV)");
    matrix2->GetYaxis()->CenterTitle();
    matrix2->GetYaxis()->SetTitleSize(0.060);
    matrix2->GetYaxis()->SetLabelSize(0.060);

    matrix2->GetXaxis()->SetTitleFont(42);
    matrix2->GetXaxis()->SetLabelFont(42);
	matrix2->GetXaxis()->SetTitle("E_{#gamma} (MeV)");
	matrix2->GetXaxis()->SetTitleOffset(0.9);
    matrix2->GetXaxis()->CenterTitle();
	//matrix2->GetYaxis()->SetTitle("E (keV)");
	// Set user range in the matrix if you want to
    //	matrix2->GetXaxis()->SetRangeUser(x_cal[0],9000);
    //	matrix2->GetYaxis()->SetRangeUser(x_cal[0],9000);
        matrix2->GetZaxis()->SetRangeUser(2.,2000);
        matrix2->GetXaxis()->SetTitleSize(0.063);
        matrix2->GetXaxis()->SetLabelSize(0.053);
        matrix2->GetXaxis()->SetLabelSize(0.055);

	matrix2->Draw("COLZ");
    
    TLatex t;
    t.SetTextFont(42);
    t.SetTextSize(0.053);
    t.DrawLatex(2.3,1.2,"(b)");
    t.DrawLatex(2.3,0.7,"^{154}Sm(p, d#gamma)^{153}Sm");
    
    gPad->Update();
	TPaletteAxis *palette = (TPaletteAxis*)matrix2->GetListOfFunctions()->FindObject("palette");
	if(!palette) cout << "nono." << endl;
	palette->SetX1NDC(0.92);
	palette->SetX2NDC(0.94);
	palette->SetLabelSize(0.055);
    palette->SetLabelOffset(-0.01);

    palette->SetLabelFont(42);

	palette->Draw();
/*
    c1.cd(3);
	c1_3->SetLogz();
        c1_3->SetRightMargin(0.13);
        c1_3->SetTopMargin(0.05);
        c1_3->SetBottomMargin(0.15);
	//matrix3->GetXaxis()->SetTitle("E_{#gamma} (keV)");
	matrix3->GetXaxis()->SetTitleOffset(1.1);
	//matrix3->GetYaxis()->SetTitle("E (keV)");
	matrix3->GetYaxis()->SetTitleOffset(1.9);
	// Set user range in the matrix if you want to
    //	matrix3->GetXaxis()->SetRangeUser(x_cal[0],9000);
    //	matrix3->GetYaxis()->SetRangeUser(x_cal[0],9000);
        matrix3->GetZaxis()->SetRangeUser(1,900);
        matrix3->GetXaxis()->SetLabelSize(0.053);
    
	matrix3->Draw("COLZ");
        t.DrawLatex(3.5,1.5,"c) primary");

	    gPad->Update();

	TPaletteAxis *palette = (TPaletteAxis*)matrix3->GetListOfFunctions()->FindObject("palette");
	if(!palette) cout << "nono." << endl;
	palette->SetX1NDC(0.85);
	palette->SetX2NDC(0.9);
	palette->SetLabelSize(0.06);
	palette->Draw();
  */
        c1->Print("matrices.pdf");
        c1->Print("matrices.png");
        c1->Print("matrices.eps");
	
	
		
}	// END of script
