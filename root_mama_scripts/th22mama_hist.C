/*
 Usage: .x th22mama_hist.C+ ( matrix, "output-filename.m") or .x th22mama_hist.C+ ( matrix, "output-filename.m", "comment")
*/

#include <TH2.h>
#include <TH1.h>

#include <algorithm>
#include <fstream>
#include <iostream>
#include <time.h>

void th22mama_hist(TH1* m, const char* filename, const char* comment="none")
{
    char tmp[128];
    time_t now = time(0);
    strftime(tmp, sizeof(tmp), "%Y-%m-%d %T", localtime(&now)); // not the 23-Mar-07 16:02:34 format
    
    TAxis *xax = m->GetXaxis();
    
    const int nx = std::min(xax->GetNbins(), 8192);
    std::cout << "histogram is " << nx  << "; comment='" << comment << "'" << std::endl;
    
    std::ofstream mama(filename);
    mama << "!FILE=Disk \n"
    "!KIND=Matrix \n"
    "!LABORATORY=Oslo Cyclotron Laboratory (OCL) \n"
    "!EXPERIMENT=siri2root \n"
    "!COMMENT=" << comment << "\n"
    "!TIME=" << tmp << "\n"
    "!CALIBRATION EkeV=6";
    const double cal[3] = { xax->GetBinLowEdge(1), xax->GetBinWidth(1), 0};
    for(int i=0; i<3; ++i) {
        snprintf(tmp, sizeof(tmp), ",%13.6E", cal[i]);
        mama << tmp;
    }
    mama << "\n!PRECISION=16\n";
    snprintf(tmp, sizeof(tmp), "!DIMENSION=1,0:%4d\n", nx-1);
    mama << tmp;
    snprintf(tmp, sizeof(tmp), "!CHANNEL=(0:%4d)\n", nx-1);
    mama << tmp;
    
    for(int ix=1; ix<=nx; ++ix){
        mama << m->GetBinContent(ix) << ' ';
        mama << "\n";
    }
    mama << "!IDEND=" << endl << endl;
    
    mama.close();
}


