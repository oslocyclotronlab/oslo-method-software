/*
 Usage: .x th22mama.C+ ( matrix, "output-filename.m") or .x th22mama.C+ ( matrix, "output-filename.m", "comment")
 If you get a .root file, e.g. :
    root -l Oslo_all_seg2.root
 Call the TBrowser:
    root [4] TBrowser oslo;
 Click on the folder until the matrix is displayed and the name is displayed, e.g. h3
 Then type:
 root [5] .x th22mama.C+ (h3,"seg2")
 and the mama matrix is called seg2
 */
#include <TH2.h>

#include <algorithm>
#include <fstream>
#include <iostream>
#include <time.h>

void th22mama(TH2* m, const char* filename, const char* comment="none")
{
    char tmp[128];
    time_t now = time(0);
    strftime(tmp, sizeof(tmp), "%Y-%m-%d %T", localtime(&now)); // not the 23-Mar-07 16:02:34 format

    TAxis *xax = m->GetXaxis(), *yax = m->GetYaxis();

    const int nx = std::min(xax->GetNbins(), 4096);
    const int ny = yax->GetNbins();
    std::cout << "matrix is " << nx << 'x' << ny << "; comment='" << comment << "'" << std::endl;

    std::ofstream mama(filename);
    mama << "!FILE=Disk \n"
        "!KIND=Matrix \n"
        "!LABORATORY=Oslo Cyclotron Laboratory (OCL) \n"
        "!EXPERIMENT=siri2root \n"
        "!COMMENT=" << comment << "\n"
        "!TIME=" << tmp << "\n"
        "!CALIBRATION EkeV=6";
    const double cal[6] = { xax->GetBinLowEdge(1), xax->GetBinWidth(1), 0,
                           yax->GetBinLowEdge(1), yax->GetBinWidth(1), 0 };
    for(int i=0; i<6; ++i) {
        snprintf(tmp, sizeof(tmp), ",%13.6E", cal[i]);
        mama << tmp;
    }
    mama << "\n!PRECISION=16\n";
    snprintf(tmp, sizeof(tmp), "!DIMENSION=2,0:%4d,0:%4d\n", nx-1, ny-1);
    mama << tmp;
    snprintf(tmp, sizeof(tmp), "!CHANNEL=(0:%4d,0:%4d)\n", nx-1, ny-1);
    mama << tmp;

    for(int iy=1; iy<=ny; ++iy) {
        for(int ix=1; ix<=nx; ++ix)
            mama << m->GetBinContent(ix, iy) << ' ';
        mama << "\n";
    }
    mama << "!IDEND=" << endl << endl;

    mama.close();
}
