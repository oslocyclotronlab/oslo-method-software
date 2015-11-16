
// wish list Alexander:
// - write to output file
// - switch for beam shield

#include "rkinz.h"
#include <TApplication.h>
#include <TCanvas.h>
#include <TDatime.h>
#include <TF1.h>
#include <TG3DLine.h>
#include <TGButton.h>
#include <TGButtonGroup.h>
#include <TGClient.h>
#include <TGComboBox.h>
#include <TGLabel.h>
#include <TH1.h>
#include <TH2.h>
#include <TGNumberEntry.h>
#include <TRootEmbeddedCanvas.h>
#include <TGStatusBar.h>
#include <TGTextEntry.h>
#include <TStyle.h>
#include <TSystem.h>
#include <TText.h>
#include <TGraphErrors.h>
#include <TLatex.h>

#include <cstdlib>
#include <cmath>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

#include <stdlib.h>
#include <stdio.h>
#include <sys/time.h>
#include <unistd.h>

#include "kinzlib.h"
#include "ame2003_masses.h"

//#define NDEBUG
#include "debug.h"

// ########################################################################

struct Beam_t {
    int A, Z;
    const char* name;
    double Emin, Emax;
};
static const int NBEAMS = 4;
static const Beam_t beams[NBEAMS] = {
    { 1, 1, "1H",  5, 30 },
    { 2, 1, "2H",  5, 15 },
    { 3, 2, "3He", 5, 40 },
    { 4, 2, "4He", 5, 40 },
};

// ########################################################################

typedef std::vector<float> level_list_t;

level_list_t get_level_list(int A, int Z)
{
    level_list_t levels;
    levels.push_back(0);

    const char* g4_photondata = std::getenv("G4LEVELGAMMADATA");
    if( g4_photondata ) {
        // build filename
        std::ostringstream o;
        o << g4_photondata << "/z" << Z << ".a" << A;
        // open file, no problem if it does not exist
        std::ifstream g4_photon(o.str().c_str());
        // read line by line
        std::string line;
        while( getline(g4_photon, line) ) {
            // read one level, convert from keV to MeV
            float level;
            std::istringstream iline(line);
            iline >> level;
            level /= 1000;

            // if different from previous level, add to the list
            if( levels.empty() || std::fabs(level-levels.back())>=0.005 )
                levels.push_back(level);
        }
    }
    if( levels.size() == 1 ) {
        // file not found or so
        const double* data = 0;
        if( Z == 5 && A>=9 && A<=11 ) {
            const double Ex_b9[]  = { 0, 1.5, 2.345, 2.751, 2.78, -1 }; /* MeV */
            const double Ex_b10[] = { 0, 0.718, 1.740, 2.154, 3.587, 4.774, 5.110,
                                      5.163, 5.182, 5.919, 6.024, 6.129, 6.560, 6.875, -1 }; /* MeV */
            const double Ex_b11[] = { 0, 2.124, 4.444, 5.020, 6.742, 6.791, 7.285, 7.997,
                                      8.560, 8.920, 9.185, 9.274, 9.820, -1 }; /* MeV */
            const double *Ex_b[] = { Ex_b9, Ex_b10, Ex_b11 };
            data = Ex_b[A-9];
        } else if( Z == 6 && A>=10 && A<=13 ) {
            const double Ex_c10[] = { 0, 3.353, 5.220, 5.380, 6.580, -1 }; /* MeV */
            const double Ex_c11[] = { 0, 2, 4.318, 4.804, 6.339, 6.478, 6.904, 7.499,
                                      8.104, 8.420, 8.655, 8.699, 9.2, 9.65, 9.78, 9.97, -1 }; /* MeV */
            const double Ex_c12[] = { 0, 4.43891, 7.654, 9.641, -1 }; /* MeV */
            const double Ex_c13[] = { 0, 3.089, 3.684, 3.854, 6.864, 7.49, 7.686, 8.2, 9.5,
                                      9.897, -1 }; /* MeV */
            const double *Ex_c[] = { Ex_c10, Ex_c11, Ex_c12, Ex_c13 };
            data = Ex_c[A-10];
        } else if( Z == 7 && A>=12 && A<=15 ) {
            const double Ex_n12[] = { 0, 0.96, 1.191, 1.8, 2.439, 3.132, 3.558, 4.140, 5.348,
                                      5.6, 6.4, 7.4, 7.684, 8.446, 9.035, 9.42, 9.8, -1 }; /* MeV */
            const double Ex_n13[] = { 0, 2.365, 3.502, 3.547, 6.364, 6.886, 7.155, 7.376,
                                      7.9, 8.92, 9, 9.476, -1 }; /* MeV */
            const double Ex_n14[] = { 0, 2.312, 3.948, 4.915, 5.105, 5.691, 5.834, 6.203,
                                      6.446, 7.029, 7.967, 8.062, 8.49, 8.618, 8.776, 8.907, 8.964, -1 }; /* MeV */
            const double Ex_n15[] = { 0, 5.270155, 5.298822, 6.32378, 7.15505, 7.30083, 7.5671, 8.31262, 8.57140,
                                      9.04971, 9.15190, 9.15490, 9.2221, 9.760, -1 }; /* MeV */
            const double *Ex_n[] = { Ex_n12, Ex_n12, Ex_n13, Ex_n14, Ex_n15 };
            data = Ex_n[A-12];
        }
        if( data ) {
            data++; // skip g.s., it is already in the list
            while( *data > 0 )
                levels.push_back( *data++ );
        }
    }
    return levels;
}

// ########################################################################
// ########################################################################

class MyGraphErrors : public TGraphErrors {
public:
    MyGraphErrors(int n, const double* x, const double* y, const double* dx, const double* dy, const double *E_x);
    MyGraphErrors(int n);
    virtual ~MyGraphErrors();
    virtual char* GetObjectInfo(Int_t px, Int_t py) const;
    void SetEx(int i, double Ex_)
        { Ex[i] = Ex_; }
private:
    double* Ex;
};

// ########################################################################

MyGraphErrors::MyGraphErrors(int n, const double* x, const double* y,
                             const double* dx, const double* dy, const double *E_x)
    : TGraphErrors(n, x, y, dx, dy)
{
    Ex = new double[n];
    for(int i=0; i<n; ++i)
        Ex[i] = E_x[i];
}

// ########################################################################

MyGraphErrors::MyGraphErrors(int n)
    : TGraphErrors(n)
{
    Ex = new double[n];
    for(int i=0; i<n; ++i)
        Ex[i] = 0;
}

// ########################################################################

MyGraphErrors::~MyGraphErrors()
{
    delete Ex;
}

// ########################################################################

char* MyGraphErrors::GetObjectInfo(Int_t px, Int_t py) const
{
    //std::cout << "MyGraphErrors::GetObjectInfo("<<px<<","<<py<<")" << std::endl;
    if( !gPad || GetN()==0 )
        return TGraphErrors::GetObjectInfo(px, py);

    const Float_t x = gPad->PadtoX(gPad->AbsPixeltoX(px));
    const Float_t y = gPad->PadtoY(gPad->AbsPixeltoY(py));

    int min_idx=0;
    const double* gx = GetX();
    for(int i=1; i<GetN(); ++i) {
        if( fabs(x-gx[i]) < fabs(x-gx[min_idx]) )
            min_idx = i;
    }

    static char info[128];
    snprintf(info, sizeof(info), "x=%g y=%g Ex=%g MeV", x, y, Ex[min_idx]);
    return info;
}

// ########################################################################
// ########################################################################

MyMainFrame::MyMainFrame()
   : TGMainFrame(gClient->GetRoot(), 400, 500)
{
    SetCleanup(kDeepCleanup);
    
    // ============================================================
    // -------------------- button bar
    TGHorizontalFrame *bbar = new TGHorizontalFrame(this);//, 100, 10);

    // -------------------- combo box for beam selection
    TGLabel *label = new TGLabel(bbar, "Beam:");
    bbar->AddFrame(label, new TGLayoutHints(kLHintsLeft|kLHintsCenterY));

    cmb_beam = new TGComboBox(bbar);
    cmb_beam->Connect("Selected(Int_t)","MyMainFrame",this,"OnComboBeamSelect(Int_t)");
    cmb_beam->SetMinWidth(50);
    cmb_beam->SetWidth(50);
    bbar->AddFrame(cmb_beam, new TGLayoutHints(kLHintsLeft|kLHintsExpandY,2,2,2,2));

    TGVertical3DLine *separator = new TGVertical3DLine(bbar);
    bbar->AddFrame(separator, new TGLayoutHints(kLHintsLeft|kLHintsExpandY));

    // -------------------- beam energy
    label = new TGLabel(bbar, "Energy:");
    bbar->AddFrame(label, new TGLayoutHints(kLHintsLeft|kLHintsCenterY));

    n_energy = new TGNumberEntry(bbar, 0, 6, -1, TGNumberFormat::kNESRealOne);
    n_energy->GetNumberEntry()->SetToolTipText("Beam energy in MeV.");
    bbar->AddFrame(n_energy, new TGLayoutHints(kLHintsLeft|kLHintsCenterY,2,2,2,2));

    label = new TGLabel(bbar, "Angle:");
    bbar->AddFrame(label, new TGLayoutHints(kLHintsLeft|kLHintsCenterY));

    r_forward  = new TGRadioButton(bbar, new TGHotString("&47°"));
    r_forward->Connect("Clicked()","MyMainFrame",this,"OnDetectorsForward()");
    r_forward->SetToolTipText("Calculation for forward angles (normal configuration).");
    bbar->AddFrame(r_forward, new TGLayoutHints(kLHintsLeft|kLHintsCenterY,2,2,2,2));

    r_backward  = new TGRadioButton(bbar, new TGHotString("&133°"));
    r_backward->Connect("Clicked()","MyMainFrame",this,"OnDetectorsBackward()");
    r_backward->SetToolTipText("Calculation for backward angles (232Th configuration).");
    bbar->AddFrame(r_backward, new TGLayoutHints(kLHintsLeft|kLHintsCenterY,2,2,2,2));

    label = new TGLabel(bbar, "Det.#");
    bbar->AddFrame(label, new TGLayoutHints(kLHintsLeft|kLHintsCenterY));

    n_det = new TGNumberEntry(bbar, 0, 4, -1, TGNumberFormat::kNESInteger);
    n_det->SetLimits( TGNumberFormat::kNELLimitMinMax, 0, 7 );
    n_det->GetNumberEntry()->SetToolTipText("Detector number 1..8.");
    bbar->AddFrame(n_det, new TGLayoutHints(kLHintsLeft|kLHintsCenterY,2,2,2,2));

    AddFrame(bbar,new TGLayoutHints(kLHintsLeft|kLHintsTop,2,2,2,2));

    // ============================================================
    // -------------------- target bar
    TGHorizontalFrame *pbar = new TGHorizontalFrame(this);

    label = new TGLabel(pbar, "Target:");
    pbar->AddFrame(label, new TGLayoutHints(kLHintsLeft|kLHintsCenterY));

    r_c12  = new TGRadioButton(pbar, new TGHotString("&12C"));
    r_c12->Connect("Clicked()","MyMainFrame",this,"OnTarget12C()");
    r_c12->SetToolTipText("Click this to make a calculation for 12C.");
    pbar->AddFrame(r_c12, new TGLayoutHints(kLHintsLeft|kLHintsCenterY,2,2,2,2));

    label = new TGLabel(pbar, "mg/cm2=");
    pbar->AddFrame(label, new TGLayoutHints(kLHintsLeft|kLHintsCenterY));
    n_targetT_C12 = new TGNumberEntry(pbar, 0, 5, -1, TGNumberFormat::kNESRealThree);
    n_targetT_C12->SetLimits( TGNumberFormat::kNELLimitMinMax, 0.5, 3 );
    pbar->AddFrame(n_targetT_C12, new TGLayoutHints(kLHintsLeft|kLHintsCenterY,2,2,2,2));

    // -------------------- separator
    separator = new TGVertical3DLine(pbar);
    pbar->AddFrame(separator, new TGLayoutHints(kLHintsLeft|kLHintsExpandY));

    r_anytarget  = new TGRadioButton(pbar, new TGHotString("&Other"));
    r_anytarget->Connect("Clicked()","MyMainFrame",this,"OnTargetAny()");
    pbar->AddFrame(r_anytarget, new TGLayoutHints(kLHintsLeft|kLHintsCenterY,2,2,2,2));

    label = new TGLabel(pbar, "A=");
    pbar->AddFrame(label, new TGLayoutHints(kLHintsLeft|kLHintsCenterY));
    n_targetA = new TGNumberEntry(pbar, 0, 3, -1, TGNumberFormat::kNESInteger);
    n_targetA->SetLimits( TGNumberFormat::kNELLimitMinMax, 6, 290 );
    pbar->AddFrame(n_targetA, new TGLayoutHints(kLHintsLeft|kLHintsCenterY,2,2,2,2));

    label = new TGLabel(pbar, "Z=");
    pbar->AddFrame(label, new TGLayoutHints(kLHintsLeft|kLHintsCenterY));
    n_targetZ = new TGNumberEntry(pbar, 0, 3, -1, TGNumberFormat::kNESInteger);
    n_targetZ->SetLimits( TGNumberFormat::kNELLimitMinMax, 3, 118 );
    n_targetZ->Connect("ValueSet(Long_t)", "MyMainFrame", this, "OnTargetZ(Long_t)");
    n_targetZ->GetNumberEntry()->Connect("TextChanged(const char*)", "MyMainFrame", this, "OnTargetZText(const char*)");
    pbar->AddFrame(n_targetZ, new TGLayoutHints(kLHintsLeft|kLHintsCenterY,2,2,2,2));

    t_targetName = new TGTextEntry(pbar, new TGTextBuffer(3), 1);
    t_targetName->SetState(false);
    pbar->AddFrame(t_targetName, new TGLayoutHints(kLHintsLeft|kLHintsCenterY));

    label = new TGLabel(pbar, "mg/cm2=");
    pbar->AddFrame(label, new TGLayoutHints(kLHintsLeft|kLHintsCenterY));
    n_targetT = new TGNumberEntry(pbar, 0, 5, -1, TGNumberFormat::kNESRealThree);
    n_targetT->SetLimits( TGNumberFormat::kNELLimitMinMax, 0.01, 10 );
    pbar->AddFrame(n_targetT, new TGLayoutHints(kLHintsLeft|kLHintsCenterY,2,2,2,2));

    TGTextButton *b_calc = new TGTextButton(pbar, new TGHotString("&Calculate!"));
    b_calc->Connect("Clicked()", "MyMainFrame",this,"OnButtonCalc()");
    pbar->AddFrame(b_calc, new TGLayoutHints(kLHintsLeft|kLHintsCenterY,2,2,2,2));

    AddFrame(pbar,new TGLayoutHints(kLHintsLeft|kLHintsTop,2,2,2,2));

    // ============================================================
    // -------------------- embedded canvas
    ecanvas = new TRootEmbeddedCanvas ("Ecanvas", this, 600, 400);
    ecanvas->GetCanvas()->Connect("ProcessedEvent(Int_t,Int_t,Int_t,TObject*)",
                                  "MyMainFrame", this, "OnCanvasEvent(Int_t,Int_t,Int_t,TObject*)");
    AddFrame(ecanvas, new TGLayoutHints(kLHintsExpandX | kLHintsExpandY, 2,2,2,2));

    // ============================================================
    // -------------------- status bar
    int parts[] = { 20, 10, 10, 60 };
    statusbar = new TGStatusBar(this);
    statusbar->SetParts(parts, 4);
    AddFrame(statusbar, new TGLayoutHints(kLHintsBottom|kLHintsLeft|kLHintsExpandX, 2,2,1,1));

    // ============================================================
    // set startup values
    for( int i=0; i<NBEAMS; i++)
        cmb_beam->AddEntry(beams[i].name, i);

    LoadDefaults();
    OnTargetZ(0); // show correct target name (Zr, Th, C, Sc, ...)
    OnTarget12C(); // start with 12C target


    // ============================================================
    // set window name and show the main frame
    SetWindowName("ROOT kinz");
    MapSubwindows();
    Resize(GetDefaultSize());
    MapWindow();

    cmb_beam->Select(cmb_beam->GetSelected());
}

// ########################################################################

MyMainFrame::~MyMainFrame()
{
}

// ########################################################################

template<typename T>
inline void apply_limits(T& value, const T& mini, const T& maxi)
{
    if( value<mini )
        value = mini;
    if( value>maxi )
        value = maxi;
}

// ########################################################################

void MyMainFrame::LoadDefaults()
{   
    std::ifstream dfl(".rkinz_default");
    int beam=2, angle=133, det=3, ta=232, tz=92;
    double ebeam=24, thick=0.968, thick_c12=1;
    dfl >> beam >> ebeam >> angle >> det
        >> ta >> tz >> thick >> thick_c12;

    apply_limits( beam, 0, NBEAMS );
    apply_limits( ebeam, beams[beam].Emin, beams[beam].Emax );
    apply_limits( det, 0, 7 );
    apply_limits( ta, 6, 290 );
    apply_limits( tz, 3, 118 );
    apply_limits( thick,     0.1, 15.0 );
    apply_limits( thick_c12, 0.1, 15.0 );

    cmb_beam->Select(beam);
    n_energy->SetNumber( ebeam );
    if( angle > 90 ) OnDetectorsBackward(); else OnDetectorsForward();
    n_det->SetNumber( det );
    n_targetA->SetNumber( ta );
    n_targetZ->SetNumber( tz );
    n_targetT->SetNumber( thick );
    n_targetT_C12->SetNumber( thick_c12 );
}

// ########################################################################

void MyMainFrame::SaveDefaults()
{
    std::ofstream dfl(".rkinz_default");
    dfl << cmb_beam->GetSelected() << std::endl
        << n_energy->GetNumber() << std::endl
        << ((r_forward->GetState()==kButtonDown) ? 47 : 133) << std::endl
        << n_det->GetNumber() << std::endl
        << n_targetA->GetNumber() << std::endl
        << n_targetZ->GetNumber() << std::endl
        << n_targetT->GetNumber() << std::endl
        << n_targetT_C12->GetNumber() << std::endl;
    dfl.close();
}

// ########################################################################

/**
 * Called if the window is closed.
 *
 * We terminate the application.
 */
void MyMainFrame::CloseWindow()
{
    SaveDefaults();
    gApplication->Terminate(0);
}

// ########################################################################

void MyMainFrame::OnDetectorsForward()
{
    r_forward ->SetState(kButtonDown);
    r_backward->SetState(kButtonUp);
}

// ########################################################################

void MyMainFrame::OnDetectorsBackward()
{
    r_backward->SetState(kButtonDown);
    r_forward ->SetState(kButtonUp);
}

// ########################################################################

void MyMainFrame::OnTarget12C()
{
    r_c12      ->SetState(kButtonDown);
    r_anytarget->SetState(kButtonUp);
    
    n_targetA->SetState(false);
    n_targetZ->SetState(false);
    n_targetT->SetState(false);
    n_targetT_C12->SetState(true);
}

// ########################################################################

void MyMainFrame::OnTargetAny()
{
    r_c12      ->SetState(kButtonUp);
    r_anytarget->SetState(kButtonDown);
    
    n_targetA->SetState(true);
    n_targetZ->SetState(true);
    n_targetT->SetState(true);
    n_targetT_C12->SetState(false);
}

// ########################################################################

void MyMainFrame::OnComboBeamSelect(Int_t id)
{
    const Beam_t* b = &beams[id];
    n_energy->SetLimits( TGNumberFormat::kNELLimitMinMax, b->Emin, b->Emax );
}

// ########################################################################

void MyMainFrame::OnTargetZ(Long_t)
{
    const int Z = n_targetZ->GetIntNumber();
    t_targetName->SetText(get_element_name(Z));
}

// ########################################################################

void MyMainFrame::OnTargetZText(const char*)
{
    OnTargetZ(0);
}

// ########################################################################

/* Graph for beam hitting the Si detectors directly
*/
TGraphErrors* makeDirectGraph(int pA, int pZ, double Ebeam/*MeV*/, bool shield)
{
    const int aZ=13, aA=27, sZ=14, sA=28;
    const double aT_um=10.5, t_thin_um=130, t_thick_um=1550; /* thicknesses for 90° incident particle */
    const double aM = ame2003_get_mass_amu(aA, aZ);
    const double sM = ame2003_get_mass_amu(sA, sZ);
    const double pM = ame2003_get_mass_amu(pA, pZ);
    const double asT_um = 3000; /* beam shield thickness */
    const double tf = std::sqrt(2); /* factor for thickness due to bad incident angle */

    std::cout << "# direct " << pA << get_element_name(pZ) << " beam @ "
              << Ebeam << " MeV on Si detectors";
    if( shield )
        std::cout << " with " << asT_um << " Âµm";
    else
        std::cout << " without";
    std::cout << " beamshield\n# first column (Ex) is beam energy\n";

    const unsigned int MAXP = 500;
    double Eb[MAXP], Ethin[MAXP], Ethick[MAXP], dE[MAXP];

    unsigned int p=0;
    for(double E=Ebeam; p<MAXP && E>0; E -= 1, p++) {
        const int nsteps = 50;
        const double Ea_shield /* keV*/ = shield
            ? ziegler1985(pZ, pM, aZ, aM, E*1000, asT_um,      nsteps)
            : E * 1000;
        if( Ea_shield<=0 )
            break;
        const double Ea_abs = ziegler1985(pZ, pM, aZ, aM, Ea_shield, aT_um*tf, nsteps) /* keV*/;
        if( Ea_abs<=0 )
            break;
        const double Ea_thin = ziegler1985(pZ, pM, sZ, sM, Ea_abs, t_thin_um*tf, nsteps) /* keV*/;
        if( Ea_thin<=0 )
            break;
        const double Ea_thick = ziegler1985(pZ, pM, sZ, sM, Ea_thin, t_thick_um*tf, nsteps) /* keV*/;

        Eb     [p] = E;
        Ethin  [p] = (Ea_abs  - Ea_thin) /1000 /* MeV*/;
        Ethick [p] = (Ea_thin - Ea_thick)/1000 /* MeV*/;
        dE     [p] = 0;

        // print everything in keV
        std::cout << Eb[p]*1000 << '\t' << Ea_thick  << '\t' << Ethick[p]*1000 << '\t'
                  << Ethin[p]*1000 << "t0\t" << Ea_abs << '\n';
    }
    // print two blank lines at the end
    std::cout << '\n' << std::endl;

    if( p==0 )
        return 0;

    TGraphErrors* tge = new TGraphErrors(p, Ethick, Ethin, dE, dE);
    tge->SetLineWidth(2);
    return tge;
}

// ########################################################################

class ReactionCalculator {
public:
    //! Beam particle A.
    int pA;
    //! Beam particle Z.
    int pZ;
    //! Beam energy in MeV.
    double Ebeam;
    //! Target A.
    int tA;
    //! Target Z.
    int tZ;
    //! Target thickness in mg/cm2
    double tT;
    //! Outgoing particle A (A=1 .. 4)
    int oA;
    //! Outgoing particle Z (Z=1 .. 2)
    int oZ;
    //! detection angle in deg.
    float theta;
    //! incident angle in deg.
    float thetai;

    ReactionCalculator()
        : pA(1), pZ(1), Ebeam(17), tA(90), tZ(40), tT(2), oA(1), oZ(1), theta(46), thetai(-1) { }

    struct Energies {
        //! Excitation energy in MeV
        double Ex;
        //! Energy deposit in front detector in MeV.
        double Ethin;
        //! Energy deposit in front detector in MeV.
        double Ethick;
        //! Uncertainty in energy deposit in front detector in MeV.
        double dEthin;
        //! Uncertainty in energy deposit in front detector in MeV.
        double dEthick;
        //! Energy after back detector in MeV.
        double Ea_thick;
        //! Energy after absorber in MeV.
        double Ea_abs;
        //! Energy straggling in target in keV.
        double dE;

        Energies()
            : Ex(0), Ethin(0), Ethick(0), dEthin(0), dEthick(0), Ea_thick(0), Ea_abs(0), dE(0) { }
    };
    typedef std::vector<Energies> results_t;

    results_t Calculate();
};

ReactionCalculator::results_t ReactionCalculator::Calculate()
{
    const int aZ=13, aA=27, sZ=14, sA=28;
    const double thickf = 1/std::cos(thetai*M_PI/180); /* effective thickness factor (as materials are at 47 deg) */
    const double aT_um=10.5*thickf, t_thin_um=130*thickf, t_thick_um=1550*thickf;
    const double oM = ame2003_get_mass_amu(oA, oZ);
    const double aM = ame2003_get_mass_amu(aA, aZ);
    const double sM = ame2003_get_mass_amu(sA, sZ);
    const int rA = tA + pA - oA, rZ = tZ + pZ - oZ;

    const unsigned int MAXP = 500;

    const level_list_t levels = get_level_list(rA, rZ);
    const double Estep = 0.5, Ebase = Estep*ceil((levels.back()+0.001)/Estep);

    results_t results;
    for( unsigned int p=0; p<MAXP; ++p) {
        Energies e;

        // Excitation energy in MeV
        e.Ex = p<levels.size()
            ? levels[p]
            : Ebase + Estep*(p-levels.size());

        // calculate reaction and straggling in target
        double Efinal=0; /*MeV*/
        zstraggling(tA, tZ, pA, pZ, oA, oZ, Ebeam, tT, e.Ex, theta, Efinal, e.dE);

        double Ea_thin=0, mi_Ea_abs=0, mi_Ea_thin=0, mi_Ea_thick=0, ma_Ea_abs=0, ma_Ea_thin=0, ma_Ea_thick=0;
        const int nsteps = 50;

        e.Ea_abs = ziegler1985(oZ, oM, aZ, aM, Efinal*1000, aT_um, nsteps) /* keV*/;
        if( e.Ea_abs>0 ) {
            Ea_thin = ziegler1985(oZ, oM, sZ, sM, e.Ea_abs, t_thin_um,  nsteps) /* keV*/;
            if( Ea_thin>0 )
                e.Ea_thick = ziegler1985(oZ, oM, sZ, sM, Ea_thin, t_thick_um, nsteps) /* keV*/;
        }

        mi_Ea_abs   = ziegler1985(oZ, oM, aZ, aM, Efinal*1000-e.dE, aT_um,    nsteps) /* keV*/;
        if( mi_Ea_abs>0 ) {
            mi_Ea_thin  = ziegler1985(oZ, oM, sZ, sM, mi_Ea_abs,      t_thin_um,  nsteps) /* keV*/;
            if( mi_Ea_thin>0 )
                mi_Ea_thick = ziegler1985(oZ, oM, sZ, sM, mi_Ea_thin,     t_thick_um, nsteps) /* keV*/;
        }

        ma_Ea_abs   = ziegler1985(oZ, oM, aZ, aM, Efinal*1000+e.dE, aT_um,    nsteps) /* keV*/;
        if( ma_Ea_abs>0 ) {
            ma_Ea_thin  = ziegler1985(oZ, oM, sZ, sM, ma_Ea_abs,      t_thin_um,  nsteps) /* keV*/;
            if( ma_Ea_thin>0 )
                ma_Ea_thick = ziegler1985(oZ, oM, sZ, sM, ma_Ea_thin,     t_thick_um, nsteps) /* keV*/;
        }

        if( e.Ea_abs<=0 || Ea_thin <=0 ) {
            if( 0 )
                std::cout << "# stop with Ex=" << e.Ex << " Etarget=" << Efinal
                          << "\n#    Ea_abs   = " << e.Ea_abs 
                          << "\n#    Ea_thin  = " << Ea_thin 
                          << "\n#    Ea_thick = " << e.Ea_thick
                          << "\n# mi_Ea_abs   = " << mi_Ea_abs 
                          << "\n# mi_Ea_thin  = " << mi_Ea_thin 
                          << "\n# mi_Ea_thick = " << mi_Ea_thick
                          << "\n# ma_Ea_abs   = " << ma_Ea_abs 
                          << "\n# ma_Ea_thin  = " << ma_Ea_thin 
                          << "\n# ma_Ea_thick = " << ma_Ea_thick 
                          << std::endl;
            break;
        }

        e.Ethin    = (e.Ea_abs  - Ea_thin) /1000 /* MeV*/;
        e.Ethick   = (Ea_thin - e.Ea_thick)/1000 /* MeV*/;
        e.dEthin   = fabs((mi_Ea_abs  - mi_Ea_thin)  - (ma_Ea_abs  - ma_Ea_thin)) /2000 /* MeV*/;
        e.dEthick  = fabs((mi_Ea_thin - mi_Ea_thick) - (ma_Ea_thin - ma_Ea_thick))/2000 /* MeV*/;

        results.push_back(e);
    }
    return results;
}

// ########################################################################

TGraphErrors* makeGraph(ReactionCalculator& rc)
{
    ReactionCalculator::results_t calc = rc.Calculate();
    if( calc.empty() )
        return 0;

    const int rA = rc.tA + rc.pA - rc.oA, rZ = rc.tZ + rc.pZ - rc.oZ;

    std::cout << "# " << rc.tA << get_element_name(rc.tZ) << '('
              << rc.pA << get_element_name(rc.pZ) << ','
              << rc.oA << get_element_name(rc.oZ) << ')'
              << rA    << get_element_name(rZ)    << " @ "
              << rc.Ebeam << " MeV, theta=" << rc.theta << " deg,"
              << " target thickness = " << rc.tT << " mg/cm2\n";

    MyGraphErrors* g = new MyGraphErrors( calc.size() );
    g->SetLineWidth(2);
    for( unsigned int p=0; p<calc.size(); ++p ) {
        const ReactionCalculator::Energies& e = calc[p];
        g->SetPoint( p, e.Ethick, e.Ethin );
        g->SetPointError( p, e.dEthick, e.dEthin );
        g->SetEx( p, e.Ex );

        // print everything in keV
        std::cout << e.Ex   *1000  << '\t'
                  << e.Ea_thick    << '\t'
                  << e.Ethick*1000 << '\t'
                  << e.Ethin *1000 << '\t'
                  << e.dE          << '\t'
                  << e.Ea_abs      << '\n';
    }

    if( calc.size()>5 ) {
        TGraph g( calc.size() );
        for( unsigned int p=0; p<calc.size(); ++p ) {
            const ReactionCalculator::Energies& e = calc[p];
            g.SetPoint( p, e.Ea_abs, e.Ex*1000 );
        }
        TF1 fit( "fit_ex", "pol2(0)" );
        fit.SetParameters(rc.Ebeam*1000, -1, 1e-5);
        g.Fit( &fit, "QN" );

        std::cout << "# ex_from_ede = "
                  << fit.GetParameter(0) << ' '
                  << fit.GetParameter(1) << ' '
                  << fit.GetParameter(2) << std::endl;
    }

    // print two blank lines at the end
    std::cout << '\n' << std::endl;

    return g;
}

// ########################################################################

void MyMainFrame::OnButtonCalc()
{
    TCanvas *ec = ecanvas->GetCanvas();
    ec->Clear();
    ec->Update();

    std::cout << "# columns (separated by tab character \\t):\n"
              << "# Ex\tEa_b\tEb\tEf\tdE\tEa_Al\n"
              << "# 1:Ex    = excitation energy in keV\n"
              << "# 2:Ea_b  = energy after back detector (shoot-through) in keV\n"
              << "# 3:Eb    = energy in back detector in keV\n"
              << "# 4:Ef    = energy in front detector in keV\n"
              << "# 5:dE    = energy uncertainty in target from 'straggling' in keV\n"
              << "# 6:Ea_Al = energy after Al absorber in keV\n#" << std::endl;


    ReactionCalculator rc;
    const bool forward = (r_forward->GetState()==kButtonDown);
    const int det = n_det->GetIntNumber();  /* (strip number) */
    rc.thetai = 2*det-7;
    rc.theta  = 47 + rc.thetai;
    if( !forward )
        rc.theta = 180 - rc.theta;

    const bool is_c12 = r_c12->GetState()==kButtonDown;
    if( is_c12 ) {
        rc.tZ =  6;
        rc.tA = 12;
        rc.tT = n_targetT_C12->GetNumber(); /* mg/cm2 */
    } else {
        rc.tZ = n_targetZ->GetIntNumber();
        rc.tA = n_targetA->GetIntNumber();
        rc.tT = n_targetT->GetNumber(); /* mg/cm2 */
    }

    const Beam_t* b = &beams[cmb_beam->GetSelected()];
    rc.pA    = b->A;
    rc.pZ    = b->Z;
    rc.Ebeam = n_energy->GetNumber(); /* MeV */
    
    const int NPART = 5;
    int oA[NPART] = { 1, 2, 3, 3, 4 };
    int oZ[NPART] = { 1, 1, 1, 2, 2 };

    const int NGRAPH = NPART+1;
    TGraphErrors* tge[NGRAPH] = {0,0,0,0,0,0};
    int color[NGRAPH] = { kBlack, kRed, kGreen, kBlue, kMagenta, 30/*pale green*/ };
    const char* title[NGRAPH] = {"p","d","t","^{3}He","#alpha", "direct"};
    
    double maxx = 1, maxy=0.5;
    for(int i=0; i<NGRAPH; ++i) {
        if( i<NPART ) {
            rc.oA = oA[i];
            rc.oZ = oZ[i];
            tge[i] = makeGraph(rc);
        } else if( i == NPART ) {
            tge[i] = makeDirectGraph(b->A, b->Z, rc.Ebeam, false);
        }
        if( !tge[i] )
            continue;
        
        tge[i]->SetTitle(title[i]);
        tge[i]->SetLineColor(color[i]);
        tge[i]->SetMarkerColor(color[i]);
        
        const int N = tge[i]->GetN();
        for(int j=0; j<N; ++j) {
            double my, mx;
            tge[i]->GetPoint(j, mx, my);
            if(mx>maxx)
                maxx=mx;
            if(my>maxy)
                maxy=my;
        }
    }
    
    TH1* h = new TH1I("h","",5,0,maxx*1.1);
    h->SetAxisRange(0, maxx*1.1, "X");
    h->SetAxisRange(0, maxy*1.1, "Y");
    h->GetXaxis()->SetTitle("E_{back} [MeV]");
    h->GetXaxis()->SetTitleSize(0.03);
    h->GetXaxis()->SetTitleOffset(1.3);
    h->GetYaxis()->SetTitle("E_{front} [MeV]");
    h->GetYaxis()->SetTitleSize(0.03);
    h->GetYaxis()->SetTitleOffset(1.2);
    h->DrawCopy();
    delete h;
    TLatex* l = new TLatex();
    l->SetTextSize(0.05);
    for(int i = 0; i<NGRAPH; ++i) {
        if( tge[i] ) {
            tge[i]->Draw("Lsame");
            l->SetTextColor(tge[i]->GetLineColor());
            l->DrawLatex(maxx*(0.5+0.08*i), maxy*1.02, tge[i]->GetTitle());
        }
    }
    delete l;
    ec->Update();
}

// ########################################################################

/**
 * Called for all events on the canvas, we update the status bar.
 */
void MyMainFrame::OnCanvasEvent(Int_t event, Int_t px, Int_t py, TObject* selected)
{
    if( !selected )
        return;

    TVirtualPad* savepad = gPad;
    gPad = ecanvas->GetCanvas()->GetSelectedPad();

    statusbar->SetText(selected->GetTitle(),0);
    statusbar->SetText(selected->GetName(), 1);
    char tmp[256];
    if (event == kKeyPress)
        snprintf(tmp, sizeof(tmp), "%c", (char) px);
    else
        snprintf(tmp, sizeof(tmp), "%d,%d", px, py);
    statusbar->SetText(tmp, 2);
    statusbar->SetText(selected->GetObjectInfo(px,py),3);
    gPad = savepad;
}

// ########################################################################
// ########################################################################
// ########################################################################

int main(int argc, char* argv[])
{
    TApplication theApp("ROOTupdate", &argc, argv);

    const char* g4_photondata = std::getenv("G4LEVELGAMMADATA");
    if( !g4_photondata ) {
        std::cerr << "#\n"
            "# Geant4 G4LEVELGAMMADATA not defined. To install, download the file\n"
            "#    http://geant4.cern.ch/support/source/PhotonEvaporation.2.0.tar.gz\n"
            "# Then unpack it and set the G4LEVELGAMMADATA variable to point to the\n"
            "# directory 'PhotonEvaporation2.0' (e.g., in ~/.bashrc). This will allow\n"
            "# rkinz to retrieve the known excited states for most nuclei.\n"
            "#" << std::endl;
    }

    gStyle->SetPadGridX(kTRUE);
    gStyle->SetPadGridY(kTRUE);
    gStyle->SetPalette(1);
    gStyle->SetCanvasColor(kWhite);
    gStyle->SetOptStat(0);
    gStyle->SetStatFormat("10.8g");
    gStyle->SetNdivisions(520,"y");

    // ------------------------------

    new MyMainFrame();
    theApp.Run(true);

    return 0;
}
