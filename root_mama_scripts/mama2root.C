/*
 Usage: > root
    root [1] .L mama2root.C
 Create a pointer to the histogram in root from a mama file "foo.m"
    root [2] auto *matrix1 = mama2root("foo.m") 
 Now you can use it as usual in root, eg. draw it or save it to file. 
    root [3] matrix1->Draw()
    root [4] matrix1->SaveAs("matrix1.root")
 */

#include <TH2.h>
#include <TFile.h>
#include <string>
#include <istream>

TH2* mama2root(const char* filename)
{
	ifstream ifile(filename);

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
	if(!getline(ifile,line) || line.substr(0,10) != "!FILE=Disk"){	// check correct format
		printf("\n This file is not a MAMA file!!!\n ");
		exit(2);
	}	
	getline(ifile,line);	// skip !KIND=Spectrum
	getline(ifile,line);	// skip !LABORATORY=Oslo Cyclotron Laboratory (OCL)
	getline(ifile,line);	// skip !EXPERIMENT=mama
	getline(ifile,line);	// skip !COMMENT=Sorted simulated data
	getline(ifile,line);	// skip !TIME=DATE:    19/11/09 11:47:26
	getline(ifile,line);	// get line with calibration
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
	getline(ifile,line);	// skip !PRECISION=16
	getline(ifile,line);	// get dimension
	// dimension of matrix
	dim_start = line.find_first_of("=") + 1;
	dim_dummy = line.substr(dim_start,1);
	if(!(istringstream(dim_dummy) >> dim)) cout << "Could not convert string to number." << endl;
	else cout << " Dimension of matrix is: " << dim << endl;	
	getline(ifile,line);	// get channels
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

	TH2D *matrix1 = new TH2D("matrix1"," ",dx,x_cal[0],dx*x_cal[1]+x_cal[0],dy,y_cal[0],dy*y_cal[1]+y_cal[0]);
	matrix1->SetOption("colz");
	gStyle->SetPalette(1);
	
	if(sign_ycal < 0.){	// if negative calibration coeff. on y axis
		for(iy=dy;iy>0;iy--){
			for(ix=0;ix<dx;ix++){
				ifile >> value;
				number_of_counts += value;
				matrix1->SetBinContent(ix,iy,value);
			}
		}
	}
	else{	// if positive calibration coeff. on y axis
		for(iy=0;iy<dy;iy++){
			for(ix=0;ix<dx;ix++){
				ifile >> value;
				number_of_counts += value;
				matrix1->SetBinContent(ix,iy,value);
			}
		}
	}
	
	std::cout << "Matrix is now filled." << std::endl;
	std::cout << "Total number of counts in the matrix: " << number_of_counts << std::endl;
	// close file
	ifile.close();
	return matrix1;
}
