// Script to pull out simulated GEANT4 spectra for SuN (from Artemis Spyrou, Aug 2018)
// and write them to MAMA format for each Q-value, multiplicity and initial excitation energy
// Cecilie, 13 September 2018
#include "TFile.h"
#include "TTree.h"
#include "TTreeReader.h"
#include "TTreeReaderValue.h"
#include <fstream>
#include <iostream>
#include <time.h>
#include "TSpectrum.h"

void read_response_bOslo(){
	
	// starting root stuff
	gROOT->SetStyle("Plain");
	gStyle->SetOptTitle(0);
	gStyle->SetOptStat(0);
	gStyle->SetFillColor(1);

    // Various declarations
    char rootfilename[128];
    char outputfilename[128] = "SuN_Exresp_sumpeak_parameters.txt";
    // File pointer to output file with all the Gaussian fit parameters
    FILE *outfile = fopen(outputfilename, "w");
    fprintf(outfile,"Q M E const mean FWHM counts\n");
    
    int counter = 0;

    // Loop over all Q-values, multiplicities and sum energies
    // Max sum energy is always the same as the Q-value
    for(int qval=5; qval<13;qval++){
        for(int mult=1; mult<6; mult++){
            for(int sum_e=1; sum_e<=qval; sum_e++){
            // string of input file name
            sprintf(rootfilename,"Q_%2.2dMeV/M%d_%dMeV.root",qval,mult,sum_e);
            printf(" File name is %s \n",rootfilename);
            // Open Root file with SuN simulated spectra
            TFile *file = TFile::Open(rootfilename);
            file->cd();
        
            // The tree is always named "t"
            TTreeReader myReader("t",file);
            int totalSize = 0;        // Sum of data size (in bytes) of all events in the branch
        
            const int nbins = 1400;   // number of bins in the histogram
            const int min_e = 0;      // minimum Ex energy (keV)
            const int max_e = 14000;  // maximum Ex energy (keV)
            double xmin     = 0;
            double xmax     = (double)nbins;
            double source[nbins];
        
            // The branch called "eneAll" is the sum of all eight segments
            TTreeReaderValue<double> eventSize(myReader, "eneAll");
            TH1D *SuN_spectrum = new TH1D("SuN_spectrum"," ",nbins,min_e,max_e);
            TH1D *SuN_spectrum_sumpeak = new TH1D("SuN_spectrum_sumpeak"," ",nbins,min_e,max_e);
            TH1D *SuN_spectrum_nosumpeak = new TH1D("SuN_spectrum_nosumpeak"," ",nbins,min_e,max_e);
        
            while (myReader.Next()){
              // Get the data from the current TTree entry by getting
              // the value from the connected reader (eventSize):
              totalSize += *eventSize;
              SuN_spectrum->Fill(*eventSize); 
              SuN_spectrum_sumpeak->Fill(*eventSize);
              SuN_spectrum_nosumpeak->Fill(*eventSize);
            } 
    
            // Fill vector with SuN spectrum values; this is for applying 
            // better options in the background estimate later
            for (int i=0; i<nbins; i++){
              source[i]=SuN_spectrum->GetBinContent(i + 1);
            } 
    
            int sizeInMB = totalSize/1024/1024;
            printf(" Total size of all events: %d MB\n", sizeInMB);
                
            // Now we need to identify the sum peak 
            int npeaks = 1; // Find only one peak
            TSpectrum *spectrum = new TSpectrum(3*npeaks);// The TSpectrum class finds candidate peak
            int nfound = spectrum->Search(SuN_spectrum,2," ",0.6);
            printf(" Found %d candidate peak to fit\n",nfound);
    
            TSpectrum *hb = new TSpectrum(); 
            hb->Background(source,nbins,15,TSpectrum::kBackDecreasingWindow,
                       TSpectrum::kBackOrder2,kTRUE,
                       TSpectrum::kBackSmoothing3,kFALSE);
    
            TH1D *background_spec = new TH1D("background_spec","",nbins,min_e,max_e);
            for (int i=0; i<nbins; i++){
              background_spec->SetBinContent(i+1,source[i]);
            }
    
            // Make new spectrum which is the SuN spectrum minus the background
            SuN_spectrum_sumpeak->Add(background_spec,-1);
            SuN_spectrum_sumpeak->Draw("same");
    
            // get centroid of the sum peak
            double *xpeaks = spectrum->GetPositionX(); 
            double xp = xpeaks[0];
            // Find channel of the sum peak centroid
            int bin = SuN_spectrum->GetXaxis()->FindBin(xp);
            cout << " Centroid channel of peak is " << bin << endl;
            double binwidth = SuN_spectrum->GetXaxis()->GetBinWidth(1);
            cout << " Bin width is " << binwidth << " keV/ch " << endl;
            // Define fit range for the sum peak
            double lowrange = xp - 500.;
            double highrange = xp + 500.;
            int lowrangebin = (int)lowrange/binwidth;
            int highrangebin = (int) highrange/binwidth;
            cout << " Range for fit of the sum peak: E = " << lowrange << "-" << highrange << " keV " << endl;
            cout << " Channels " << lowrangebin << "-" << highrangebin << endl;
    
            TF1 *peakfit = new TF1("peakfit","gaus",lowrange,highrange);
            // Always set the mean to the known centroid value
            const double centroid_value = (double)sum_e*1000.; // MeV->keV
            peakfit->SetParameter(0,5.00021e+03);
            peakfit->FixParameter(1,centroid_value); 
            peakfit->SetParameter(2,50.);
            peakfit->SetParLimits(2,1.,3000.); // sigma must be positive and >0
            cout << " Sum peak centroid is " << centroid_value << " keV " << endl;
            SuN_spectrum_sumpeak->Fit("peakfit","RBM+"); 
    
            // Subtract the fitted Gaussian peak from the total spectrum
            for(int i=lowrangebin; i<=highrangebin; i++){
              double energy = binwidth*(double)i;
              double value = (SuN_spectrum_nosumpeak->GetBinContent(i)) - (peakfit->Eval(energy));
              SuN_spectrum_nosumpeak->SetBinContent(i,value);
            }
    
            // Get fit parameters
            double par[3];
            peakfit->GetParameters(par);
            double constant = par[0];
            double mean = par[1];
            double sigma = par[2];
    
            // Use fit parameters to estimate the range of integration for the sum peak
            // After consulting with Magne, we decided that using +/- 3sigma is okay.
            int integrate_low = (int) (mean - 3.*sigma)/binwidth;
            int integrate_high = (int) (mean + 3.*sigma)/binwidth;
        
            cout << " Integration of the sum peak between " << integrate_low << " ch to " << integrate_high << " ch: " << endl;
            cout << SuN_spectrum->Integral(integrate_low,integrate_high) << " counts. " << endl;
        
            cout << SuN_spectrum_sumpeak->Integral(integrate_low,integrate_high) << " counts. " << endl;
            cout << background_spec->Integral(integrate_low,integrate_high) << " counts. " << endl;
        
            double counts_Gauss_fit = peakfit->Integral(lowrange,highrange)/binwidth;
            cout << counts_Gauss_fit << " counts from the Gauss fit." << endl;
            cout << endl;
    
            // Now print the spectrum name and the Gaussian parameters to the .txt file
            fprintf(outfile,"%d %d %d %3.3E %3.3E %3.3E %3.3E\n",qval,mult,sum_e,constant,mean,2.35*sigma,counts_Gauss_fit);
    
    
            // Print background spectrum without sum peak to MAMA format
            char tmp[128];
            const char comment[128] = "background";
            time_t now = time(0);
            strftime(tmp, sizeof(tmp), "%Y-%m-%d %T", localtime(&now)); // not the 23-Mar-07 16:02:34 format
        
            TAxis *xax = SuN_spectrum_nosumpeak->GetXaxis();
            const int nx = xax->GetNbins();
        
            // Explanation to the filename:
            // b = background, 05->12 = Q-value (MeV), 01-05 = ave. multiplicity; 01-12 = initial Ex (MeV)
            char filename[128];
            sprintf(filename,"b%2.2d_%2.2d_%2.2d",qval,mult,sum_e);; 
            std::ofstream mamafile(filename);
            mamafile << "!FILE=Disk \n"
            "!KIND=Matrix \n"
            "!LABORATORY=Oslo Cyclotron Laboratory (OCL) \n"
            "!EXPERIMENT=SuN simulations \n"
            "!COMMENT=" << comment << "\n"
            "!TIME=" << tmp << "\n"
            "!CALIBRATION EkeV=6";
            const double cal[3] = { (xax->GetBinLowEdge(1) + 0.5*xax->GetBinWidth(1)), xax->GetBinWidth(1), 0};
            for(int i=0; i<3; ++i) {
                snprintf(tmp, sizeof(tmp), ",%13.6E", cal[i]);
                mamafile << tmp;
            }
            mamafile << "\n!PRECISION=16\n";
            snprintf(tmp, sizeof(tmp), "!DIMENSION=1,0:%4d\n", nx-1);
            mamafile << tmp;
            snprintf(tmp, sizeof(tmp), "!CHANNEL=(0:%4d)\n", nx-1);
            mamafile << tmp;
            
            for(int ix=1; ix<=nx; ++ix){
                mamafile << SuN_spectrum_nosumpeak->GetBinContent(ix) << ' ';
                mamafile << "\n";
            }
            mamafile << "!IDEND=" << endl << endl;
            mamafile.close();
    
            // Print out total spectra in MAMA format
            // Explanation to the filename:
            // t = total, 05->12 = Q-value (MeV), 01-05 = ave. multiplicity; 01-12 = initial Ex (MeV)
            char filename_tot[128];
            strftime(tmp, sizeof(tmp), "%Y-%m-%d %T", localtime(&now)); // not the 23-Mar-07 16:02:34 format
            const char comment_tot[128] = "total spectrum";
            sprintf(filename_tot,"t%2.2d_%2.2d_%2.2d",qval,mult,sum_e);; 
            std::ofstream mamafile_tot(filename_tot);
            mamafile_tot << "!FILE=Disk \n"
            "!KIND=Matrix \n"
            "!LABORATORY=Oslo Cyclotron Laboratory (OCL) \n"
            "!EXPERIMENT=SuN simulations \n"
            "!COMMENT=" << comment_tot << "\n"
            "!TIME=" << tmp << "\n"
            "!CALIBRATION EkeV=6";
            for(int i=0; i<3; ++i) {
                snprintf(tmp, sizeof(tmp), ",%13.6E", cal[i]);
                mamafile_tot << tmp;
            }
            mamafile_tot << "\n!PRECISION=16\n";
            snprintf(tmp, sizeof(tmp), "!DIMENSION=1,0:%4d\n", nx-1);
            mamafile_tot << tmp;
            snprintf(tmp, sizeof(tmp), "!CHANNEL=(0:%4d)\n", nx-1);
            mamafile_tot << tmp;
            
            for(int ix=1; ix<=nx; ++ix){
                mamafile_tot << SuN_spectrum->GetBinContent(ix) << ' ';
                mamafile_tot << "\n";
            }
            mamafile_tot << "!IDEND=" << endl << endl;
            mamafile_tot.close();
    
            counter += 1;
            file->Close();
            } // end sum energy loop
        } // end multiplicity loop
    } // end Q-value loop

    fclose(outfile);
    cout << " Number of files in the loop = " << counter << endl;
	
		
}	// END of script
            
            
            
            
            
