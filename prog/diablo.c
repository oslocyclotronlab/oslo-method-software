#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>    
#include <math.h>    
#include <fcntl.h>						/* Header for open/read/write */ 
#include <errno.h> 
#include <sys/types.h>          
#include <sys/ioctl.h>          

#define MIN(a,b)((a)<(b) ? (a) : (b))

FILE *fp;
char  line[128],cdum[128];
int   sp;
float lowsp;
float ex, eg;
float mat[4096][2048];

// We need to define 4 lines:
// Along the first  diagonal (1) we have lower (L1) and upper (U1) lines
// Along the second diagonal (2) we have lower (L2) and upper (U2) lines
// We need (x1,y1) and (x2,y2) to define each line
// As example the higher (x,y) coordinates for line L2 is callled x2L2 and y2L2
// The lines are defined by energies and later transformed to channels
// Default example from 144Nd with gs 0+ (0 keV) and first excited 2+ (696.5 keV)
// The gamma-markers taken at Ex = 3791 and 6651 keV
float x1L1  =3560., y1L1=3791., x2L1=6374., y2L1=6651.;
float x1U1  =4022., y1U1=3791., x2U1=6983., y2U1=6651.;
float x1L2  =2867., y1L2=3791., x2L2=5618., y2L2=6651.;
float x1U2  =3308., y1U2=3791., x2U2=6227., y2U2=6651.;

float exL   = 3000.,   exH = 7000.;   //Region of initial excitation energies
float Exf_1 = 0.,    Exf_2 = 696.513; //The finial excitation energies given for the two diagonals
float exe1  = 2500., sige1 = 2.8, exe2 = 7817., sige2 = 6.08; //sigma1 and sigma2 at exc. E1 and E2
float rn_1, rn_2;                     //Relative number of levels in the two diagonals
float sig_2_e1, sig_2_e2;             //sigma**2 at E1 and E2
int   oddeven = 1;                    //Half-integer or integer spins
int   linlog  = 1;                    //linear = 0, logarithmical = 1 interpolation between gsF pairs
float jlow;                           //Is 0.5 or 0
float b1, b2;                         //Parameters giving sigma**2(Ex) = b1 + b2*Ex;
float sig(float ex, float b1, float b2);
float a0fgx, a1fgx, a2fgx, a0fgy, a1fgy, a2fgy;
int   dimfgx, dimfgy, ix, iy;
float gSFave_1pair, gSFave_2pair, ematch_previous, factor_previous, ematch_next, factor_next;
int   ex2ch(float ex);
int   eg2ch(float eg);
float ch2eg(int ch);
float ch2ex(int ch);

float ex2egL1(float ex);
float ex2egU1(float ex);
float ex2egL2(float ex);
float ex2egU2(float ex);
void  compressing();
void  sewing();
void  two2one();
void  linfit( int Low, int High);
float a, b;

int   chL, chH;
float rL1,rU1,rL2,rU2;

int   i;
float sum;
int   da01_1 = 0, da01_2 = 0;          //Choosing discrete levels
float spin_1[50] = {0,2,3,5,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3}; //Spins of levels in diagonal D1
float spin_2[50] = {0,2,3,5,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3}; //Spins of levels in diagonal D2
float n_1 = 14.6278, n_2 = 38.8045;    // Average number of levels in continuum diagonal D1 and D2
float xxj_1 = 4.5, xxj_2 = 5.6;        // Average spin of levels in contiuum diagonal D1 and D2, taken for 238Np at Ex = 1100 and 1500 keV
float xj_1, xj_2;
int   n1, n2;                          // Number of spins to loop over (only = 1) in quasi-continuum
float xnl1, xnl2;                      // Average number of levels in D1 and D2
int   nl1=1, nl2=1;                    // Number of discrete levels in D1 and D2
float ccc;
float area_1[2048], area_2[2048], r_area_1[2048], r_area_2[2048];
float cenx_1[2048], cenx_2[2048], cen_1[2048], cen_2[2048], p_1[2048], p_2[2048], gsf_1[2048], gsf_2[2048], dgsf_1[2048], dgsf_2[2048];
float gsf_ave[2048], dgsf_ave[2048], egam_ave[2048], delta[2048], delta_ave[2048], dsys[2048];
float ematch[2048], factor[2048];
int   L1,L2,L3,L4;
int   ch_1[2048], ch_2[2048];
int   compress;
float a0c,a1c;
float sigma;
float prob( float sigma, float jspin);
float gsum(float sigma, float jlow);
int   makerootplot();

static void fgets_ignore(char *s, int size, FILE *stream)
{
    // suppress braindead and presumtuous glibc 'security' warning
    if( !fgets(s, size, stream) )
        return;
}

int main()
{
    printf("\n");
    printf("  ______________________________________________________________ \r\n");
    printf(" |                                                              |\r\n");
    printf(" |                       D I A B L O  1.5.2                     |\r\n");
    printf(" |                                                              |\r\n");
    printf(" |    Program to calculate gamma-ray strength function (gSF)    |\r\n");
    printf(" |   You need a first-generation matrix (fg) in your directory  |\r\n");
    printf(" |                                                              |\r\n");
    printf(" |   The program uses 2 diagonals in the fg matrix to estimate  |\r\n");
    printf(" |  the shape of the gSF. It is assumed that the fg matrix has  |\r\n");
    printf(" |  relative small dispersion like 40 keV x 40 keV per channel  |\r\n");
    printf(" |  in order to accurately integrate counts in the diagonals.   |\r\n");
    printf(" |  You will later be asked to use a dispersion around 120 keV. |\r\n");
    printf(" |       See M. Wiediking et al., PRL 108, 162503 (2012).       |\r\n");
    printf(" |                                                              |\r\n");
    printf(" |  Input files: fg (mama matrix)     Output files:   input.dia |\r\n");
    printf(" |               (input.dia)                        results.dia |\r\n");
    printf(" |                                              diablo_plot.cpp |\r\n");
    printf(" | E-mail  : magne.guttormsen@fys.uio.no                        |\r\n");
    printf(" | Created : 04 Feb 2020                                        |\r\n");
    printf(" | Modified: 14 Apr 2020  Average gSF and systematic errors     |\r\n");
    printf(" | Modified: 17 Apr 2020  Compress with accurate integrals      |\r\n");
    printf(" | Modified: 26 May 2020  Root plot created: diablo_plot.cpp    |\r\n");
    printf(" | Modified: 19 Jun 2020  Lin or Log intepolation of gSF pairs  |\r\n");
    printf(" |______________________________________________________________|\r\n");
    printf("                                                                 \r\n");
    
    /* ***************************************** */
    /* Reading default values from previous runs */
    /* ***************************************** */
    fp = fopen("input.dia", "r");
    if(fp == NULL){
        printf("\nCould not open file input.dia, default values are used \n");
    }
    else {
        fgets_ignore(line,sizeof(line),fp);
        sscanf(line, " %f %f %f %f \n",&x1L1, &y1L1, &x2L1, &y2L1);
        fgets_ignore(line,sizeof(line),fp);
        sscanf(line, " %f %f %f %f \n",&x1U1, &y1U1, &x2U1, &y2U1);
        fgets_ignore(line,sizeof(line),fp);
        sscanf(line, " %f %f %f %f \n",&x1L2, &y1L2, &x2L2, &y2L2);
        fgets_ignore(line,sizeof(line),fp);
        sscanf(line, " %f %f %f %f \n",&x1U2, &y1U2, &x2U2, &y2U2);
        fgets_ignore(line,sizeof(line),fp);
        sscanf(line, " %f %f %f %f \n",&exe1, &sige1, &exe2, &sige2);
        fgets_ignore(line,sizeof(line),fp);
        sscanf(line, " %f %f %d \n",&xxj_1, &xxj_2, &oddeven);
        fgets_ignore(line,sizeof(line),fp);
        sscanf(line, " %f %f \n",&Exf_1, &Exf_2);
        fgets_ignore(line,sizeof(line),fp);
        sscanf(line, " %f %f \n",&exL, &exH);
        fgets_ignore(line,sizeof(line),fp);
        sscanf(line, " %f %f \n",&n_1, &n_2);
        fgets_ignore(line,sizeof(line),fp);
        sscanf(line, " %d %d \n",&da01_1, &da01_2);
        fgets_ignore(line,sizeof(line),fp);
        sscanf(line, " %d %d \n",&nl1, &nl2);
        fgets_ignore(line,sizeof(line),fp);
        sscanf(line, " %d    \n",&linlog);
        for(i=0;i<50;i++){
            fgets_ignore(line,sizeof(line),fp);
            sscanf(line, " %f %f \n",&spin_1[i], &spin_2[i]);
        }
        fclose(fp);
    }
    
    /* *********************************** */
    /* Reading first-generation matrix: fg */
    /* *********************************** */
    printf("Reading first-generation matrix: fg\n");
    fp = fopen("fg", "r");
    if(fp == NULL){
        printf("No fg file found in your directory\n");
        exit(0);
    } else {
        fgets_ignore(line,sizeof(line),fp);
        fgets_ignore(line,sizeof(line),fp);
        fgets_ignore(line,sizeof(line),fp);
        fgets_ignore(line,sizeof(line),fp);
        fgets_ignore(line,sizeof(line),fp);
        fgets_ignore(line,sizeof(line),fp);
        fgets_ignore(line,sizeof(line),fp);
        sscanf(line,"%13s %7s %f %1s %f %1s %f %1s %f %1s %f %1s %f",cdum, cdum, &a0fgx, cdum, &a1fgx, cdum, &a2fgx, cdum, &a0fgy, cdum, &a1fgy, cdum, &a2fgy);
        fgets_ignore(line,sizeof(line),fp);
        fgets_ignore(line,sizeof(line),fp);
        sscanf(line,"%15s %d %3s %d",cdum, &dimfgx, cdum, &dimfgy);
        fgets_ignore(line,sizeof(line),fp);
    }
    for(iy=0;iy<=dimfgy;iy++){
        for(ix=0;ix<=dimfgx;ix++){
            fscanf(fp,"%e",&mat[ix][iy]);
        }
    }
    printf("Dimension (0 : %d, 0 : %d) and calibration \n(ax0, ax1, ax2) = (%f,%f,%f) \n(ay0, ay1, ay2) = (%f,%f,%f)\n",dimfgx,dimfgy,a0fgx,a1fgx,a2fgx,a0fgy,a1fgy,a2fgy);
    fclose(fp);
    
    /* ********************************************** */
    /* Reading coordinates for 4 diagonal lines in fg */
    /* ********************************************** */
    printf("\nWe are going to define diagonals in the fg matrix. ");
    printf("\nThis matrix should have low dispertion (dE/ch)");
    printf("\ne.g. (x,y) 40 keV x 40 keV in order to determine the ");
    printf("\nintensities of the diagonals more accurate.\n");
    
    printf("\nReading coordinates for 4 diagonal lines in fg. ");
    printf("\nThese are typically the lower and upper lines");
    printf("\nfor feeding to the gs 0+ and 1st excited 2+ diagonals.");
    printf("\nThe 4 lines are called L1, U1, L2 and U2.");
    printf("\nEach line requires two coordinates: the first");
    printf("\nand the second coordinate (xch, ych).");
    printf("\nYou will now be asked for 16 numbers, be brave!\n");
    printf("\nYou may want instead to taylor the input.dia file\n");
    printf("\nYou are asked to give (x,y) for pixel 1, 2, 3,... 8 \n");
    printf("\n        U2   D2   L2            U1   D1   L1 ");
    printf("\n        .   x   .               4   x   .   ");
    printf("\n       8   x   .               .   x   2   ");
    printf("\n      .   x   6               .   x   .   ");
    printf("\n     .   x   .               .   x   .   ");
    printf("\n    .   x   .               .   x   .   ");
    printf("\n   7   x   .               .   x   .  ");
    printf("\n  .   x   .               .   x   1  ");
    printf("\n .   x   5               3   x   . \n");
    printf("\nLower line L1 for diagonal D1:\n");
    printf(" 1 Give gamma-energy (x-axis) for first coordinate (keV)       <%7.0f>:",x1L1);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &x1L1);
    printf(" 1 Give excitation-energy (y-axis) for first coordinate (keV)  <%7.0f>:",y1L1);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &y1L1);
    printf(" 2 Give gamma-energy (x-axis) for second coordinate (keV)      <%7.0f>:",x2L1);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &x2L1);
    printf(" 2 Give excitation-energy (y-axis) for second coordinate (keV) <%7.0f>:",y2L1);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &y2L1);
    rL1 = (float)(x2L1-x1L1)/(float)((y2L1-y1L1)); //slope of the L1 diagonal line
    
    printf("\nUpper line U1 for diagonal D1:\n");
    printf(" 3 Give gamma-energy (x-axis) for first coordinate (keV)       <%7.0f>:",x1U1);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &x1U1);
    printf(" 3 Give excitation-energy (y-axis) for first coordinate (keV)  <%7.0f>:",y1U1);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &y1U1);
    printf(" 4 Give gamma-energy (x-axis) for second coordinate (keV)      <%7.0f>:",x2U1);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &x2U1);
    printf(" 4 Give excitation-energy (y-axis) for second coordinate (keV) <%7.0f>:",y2U1);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &y2U1);
    rU1 = (float)(x2U1-x1U1)/((float)(y2U1-y1U1)); //slope of the U1 diagonal line

    printf("\nLower line L2 for diagonal D2:\n");
    printf(" 5 Give gamma-energy (x-axis) for first coordinate (keV)       <%7.0f>:",x1L2);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &x1L2);
    printf(" 5 Give excitation-energy (y-axis) for first coordinate (keV)  <%7.0f>:",y1L2);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &y1L2);
    printf(" 6 Give gamma-energy (x-axis) for second coordinate (keV)      <%7.0f>:",x2L2);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &x2L2);
    printf(" 6 Give excitation-energy (y-axis) for second coordinate (keV) <%7.0f>:",y2L2);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &y2L2);
    rL2 = (float)(x2L2-x1L2)/((float)(y2L2-y1L2)); //slope of the L2 diagonal line

    printf("\nUpper line U2 for diagonal D2:\n");
    printf(" 7 Give gamma-energy (x-axis) for first coordinate (keV)       <%7.0f>:",x1U2);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &x1U2);
    printf(" 7 Give excitation-energy (y-axis) for first coordinate (keV)  <%7.0f>:",y1U2);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &y1U2);
    printf(" 8 Give gamma-energy (x-axis) for second coordinate (keV)      <%7.0f>:",x2U2);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &x2U2);
    printf(" 8 Give excitation-energy (y-axis) for second coordinate (keV) <%7.0f>:",y2U2);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &y2U2);
    rU2 = (float)(x2U2-x1U2)/((float)(y2U2-y1U2)); //slope of the U2 diagonal line
    
    printf("\nTake a look if the slopes of the 4 lines are approx. similar and = 1\n");
    printf("Slopes of (L1,U1) = (%8.3f, %8.3f) and (L2,U2) = (%8.3f, %8.3f)\n",rL1,rU1,rL2,rU2);
    
    printf("\nIf the excitation-energy dispersion is not the same as the gamma-energy");
    printf("\ndispersion (expressed in keV/ch), the gSF will have empty/double filled cannels.");
    printf("\nIn order to avoid such a bizarre phenomena, we use the energy calibration of the ");
    printf("\nEx (y-axis) for the gSF(Eg): Eg = %7.1f keV +%8.3f (keV/ch)*ch.", a0fgy, a1fgy);
    printf("\nIn order to do this, you must tell the average final excition energies,");
    printf("\nfor which the two diagonals are supposed to represent.\n");
    printf("Give (average) final excitation energy (keV) of diagonal D1 <%7.1f>:",Exf_1);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &Exf_1);
    printf("Give (average) final excitation energy (keV) of diagonal D2 <%7.1f>:",Exf_2);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &Exf_2);
    
    printf("\nTake a look if the energy distances reflect the gamma FWHM\n");
    printf("\n        U2   D2   L2            U1   D1   L1 ");
    printf("\n        .   x   .               .   x   .   ");
    printf("\n      %4d x%4d              %4d x%4d  ", (int)(y2U2-x2U2-Exf_2),(int)(x2L2-y2L2+Exf_2),(int)(y2U1-x2U1-Exf_1),(int)(x2L1-y2L1+Exf_1));
    printf("\n      .   x   .               .   x   .   ");
    printf("\n     .   x   .               .   x   .   ");
    printf("\n    .   x   .               .   x   .   ");
    printf("\n   .   x   .               .   x   .  ");
    printf("\n %4d x%4d              %4d x%4d  ", (int)(y1U2-x1U2-Exf_2),(int)(x1L2-y1L2+Exf_2),(int)(y1U1-x1U1-Exf_1),(int)(x1L1-y1L1+Exf_1));
    printf("\n .   x   .               .   x   . \n");
    
    printf("\nYou should consider what part of the initial ");
    printf("\nexcitation energy region that gives reasonable gSF,");
    printf("\ne.g. from 3500 keV and up to Ex = Sn");
    printf("\nGive lower initial excitation energy (keV)  <%7.1f>:",exL);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &exL);
    printf("Give higher initial excitation energy (keV) <%7.1f>:",exH);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &exH);
    
    /* ********************************************************** */
    /* Finding spin cutoff parameter as function of excitation Ex */
    /* ********************************************************** */
    printf("\nYou need to give excitation energy and spin cutoff parameter at two points:");
    printf("\nOften Ex = 0.5 MeV and Ex = Sn would be appropriate");
    printf("\nAt Ex = 0.5 MeV you may look into ToI for the spin distribution.");
    printf("\nAt Ex = Sn you could use the RMI estimate calculated with robin.c (E&B2006).");
    printf("\nA reasonable relation is sig**2 = Theta x Temp. with Theta as function of Ex.");

    printf("\nGive excitation energy Ex1 for lower point   <%7.1f>:",exe1);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &exe1);
    printf("Give spin cutoff sig1 for lower point        <%7.3f>:",sige1);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &sige1);
    sig_2_e1 = sige1*sige1;
    printf("Give excitation energy Ex2 for upper point   <%7.1f>:",exe2);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &exe2);
    printf("Give spin cutoff sig2 for upper point        <%7.3f>:",sige2);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &sige2);
    sig_2_e2=sige2*sige2;
    b2 = (sig_2_e2-sig_2_e1)/(exe2-exe1);
    b1 = sig_2_e1 - b2*exe1;
    printf("Constants for sig**2 = A + B * Ex: A = %f and B = %f MeV**(-1)\n",b1,b2);
    
    printf("Do you have half (0) or integer (1) spin <%1d>:",oddeven);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%d", &oddeven);
    jlow = 0.;
    if (oddeven == 0) jlow = 0.5;

    ///////////////////////////////// BEGIN //////////////////////////////////////
    //////////////////  DETAILS ON LEVELS IN THE DIAGONALS  //////////////////////
    printf("\nThe observed counts in the diagonals depend of how many final levels that");
    printf("\nthey include. In addition the spins (parities) of the levels matter.");
    printf("\nA common situation is that the first diagonal (D1) includes the 0+ ground state,");
    printf("\nand the second diagonal (D2) includes e.g. a 0+, 3- and a 4+ level.");
    printf("\nOther situations are that you only know average properties of the levels of");
    printf("\none or both diagonals. You are asked to give discrete (0) or average (1) properties");
    printf("\nof the diagonals. If you choose average properties of diagonals D1 and D2,");
    printf("\nyou may give relative number of levels in D1 and D2 (not absolute counts).");
    printf("\nIf you know the nuclear temperature T at the diagonals, you may estimate relative");
    printf("\nnumbers as exp(E1/T) and exp(E2/T) for D1 and D2, respectively.");
    
    printf("\n\nDiagonal D1:\n");
    xnl1 = 0;
    printf("Give discrete (0) or average (1) properties of D1 <%1d>:",da01_1);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%d", &da01_1);
    if(da01_1==0){
        printf("Give number of discrete levels (< 50)            <%2d>:",nl1);
        fgets_ignore(line,sizeof(line),stdin);
        sscanf(line,"%d", &nl1); if(nl1>50)nl1=50;
        for (i=0; i < nl1; i++){
            printf("Give spin of level no. %2d                      <%4.1f>:",i+1, spin_1[i]);
            fgets_ignore(line,sizeof(line),stdin);
            sscanf(line,"%f", &spin_1[i]);
        }
        n1   = nl1;
        xnl1 = nl1;
    }else{
        printf("Give average number of levels <%5.3f>:",n_1);
        fgets_ignore(line,sizeof(line),stdin);
        sscanf(line,"%f", &n_1);
        printf("Give (average) spin            <%5.2f>:",xxj_1);
        fgets_ignore(line,sizeof(line),stdin);
        sscanf(line,"%f", &xxj_1);
        xj_1 = xxj_1,
        n1   = 1;
        xnl1 = n_1;
    }
    
    printf("\n\nDiagonal D2:\n");
    xnl2 = 0;
    printf("Give discrete (0) or average (1) properties of D2 <%1d>:",da01_2);
    
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%d", &da01_2);
    if(da01_2==0){
        printf("Give number of discrete levels (< 50)            <%2d>:",nl2);
        fgets_ignore(line,sizeof(line),stdin);
        sscanf(line,"%d", &nl2); if(nl2>50)nl2=50;
        for (i=0; i < nl2; i++){
            printf("Give spin of level no. %2d                      <%4.1f>:",i+1, spin_2[i]);
            fgets_ignore(line,sizeof(line),stdin);
            sscanf(line,"%f", &spin_2[i]);
        }
        n2   = nl2;
        xnl2 = nl2;
    }else{
        printf("Give average number of levels <%5.3f>:",n_2);
        fgets_ignore(line,sizeof(line),stdin);
        sscanf(line,"%f", &n_2);
        printf("Give (average) spin            <%5.2f>:",xxj_2);
        fgets_ignore(line,sizeof(line),stdin);
        sscanf(line,"%f", &xxj_2);
        xj_2 = xxj_2;
        n2   = 1;
        xnl2 = n_2;
    }
    rn_1 = xnl1/(xnl1 + xnl2);
    rn_2 = xnl2/(xnl1 + xnl2);
    printf("\nRelative numbers of levels are %5.4f (D1) and %5.4f (D2)\n",rn_1,rn_2);
    ///////////////////////////////   END   //////////////////////////////////////
    //////////////////  DETAILS ON LEVELS IN THE DIAGONALS  //////////////////////
    
    /* ************************************************ */
    /* Finding integral (area) of diagonal 1 and 2      */
    /* and gamma-energy centroid cenx and uncertainties */
    /* ************************************************ */
    chL = ex2ch(exL);
    chH = ex2ch(exH);
    for(iy=chL;iy<=chH;iy++){
        ex = ch2ex(iy);
        for(ix=0;ix<=dimfgx;ix++){
            eg = ch2eg(ix);
            if(eg>=ex2egU1(ex) && eg<=ex2egL1(ex)){
                area_1[iy]=area_1[iy]+mat[ix][iy];
                cenx_1[iy] =cenx_1[iy] +eg*mat[ix][iy];
            }
            if(eg>=ex2egU2(ex) && eg<=ex2egL2(ex)){
                area_2[iy]=area_2[iy]+mat[ix][iy];
                cenx_2[iy] =cenx_2[iy] +eg*mat[ix][iy];
            }
        }
        if(area_1[iy] > 0.){
            r_area_1[iy] = 1./sqrt(area_1[iy]);
            cenx_1[iy] = cenx_1[iy]/area_1[iy];
        }else{
            cenx_1[iy] = 0.;
        }
        if(area_2[iy] > 0.){
            r_area_2[iy] = 1./sqrt(area_2[iy]);
            cenx_2[iy] = cenx_2[iy]/area_2[iy];
        }else{
            cenx_2[iy] = 0.;
        }
    }
    
    /* *********************************************** */
    /* Finding how much of the spin distribution in    */
    /* the quasicontinuum that populates the diagonals */
    /* *********************************************** */
    for(iy=chL;iy<=chH;iy++){
        ex = ch2ex(iy);
        sigma = sig(ex,b1,b2);
        p_1[iy] = 0.;
        if (da01_1 == 0){
            sum = 0;
            for(i=0;i<nl1;i++){
                xj_1 = spin_1[i];
                if(xj_1 >=0.999){
                    p_1[iy] = prob( sigma, xj_1 - 1.) + prob( sigma, xj_1) + prob( sigma, xj_1 + 1.);
                }
                if(xj_1 >= 0.499 && xj_1 < 0.999 ){
                    p_1[iy] = prob( sigma, xj_1) + prob( sigma, xj_1 + 1.);
                }
                if(xj_1 <= 0.499){
                    p_1[iy] = prob( sigma, xj_1 + 1.);
                }
                sum = sum + p_1[iy];
            }
            p_1[iy] = sum;
        }else{
            if(xj_1 >=0.999){
                p_1[iy] = prob( sigma, xj_1 - 1.) + prob( sigma, xj_1) + prob( sigma, xj_1 + 1.);
            }
            if(xj_1 > 0.499 && xj_1 < 0.999 ){
                p_1[iy] = prob( sigma, xj_1) + prob( sigma, xj_1 + 1.);
            }
            if(xj_1 <= 0.499){
                p_1[iy] = prob( sigma, xj_1 + 1.);
            }
            p_1[iy] = p_1[iy] * n_1;
        }
        
        p_2[iy] = 0.;
        if (da01_2 == 0){
            sum = 0;
            for(i=0;i<nl2;i++){
                xj_2 = spin_2[i];
                if(xj_2 >=0.999){
                    p_2[iy] = prob( sigma, xj_2 - 1.) + prob( sigma, xj_2) + prob( sigma, xj_2 + 1.);
                }
                if(xj_2 >= 0.499 && xj_2 < 0.999 ){
                    p_2[iy] = prob( sigma, xj_2) + prob(sigma, xj_2 + 1.);
                }
                if(xj_2 <= 0.499){
                    p_2[iy] = prob(sigma, xj_2 + 1.);
                }
                sum = sum + p_2[iy];
            }
            p_2[iy] = sum;
        }else{
            if(xj_2 >=0.999){
                p_2[iy] = prob(sigma, xj_2 - 1.) + prob( sigma, xj_2) + prob( sigma, xj_2 + 1.);
            }
            if(xj_2 > 0.499 && xj_2 < 0.999 ){
                p_2[iy] = prob(sigma, xj_2) + prob( sigma, xj_2 + 1.);
            }
            if(xj_2 <= 0.499){
                p_2[iy] = prob( sigma, xj_2 + 1.);
            }
            p_2[iy] = p_2[iy] * n_2;
        }
        
        /* ************************************************** */
        /* Fasten seatbelts                                   */
        /* Dividing by Eg**3 and the fraction p of the spin   */
        /* distribution populating all levels at the diagonal */
        /* The gSF values are still not sewed together        */
        /* ************************************************** */
        cen_1[iy] = ex -Exf_1;
        cen_2[iy] = ex -Exf_2;
        if(cen_1[iy] > 0.){
            ccc = cen_1[iy];
            gsf_1[iy] = area_1[iy]/(ccc*ccc*ccc*p_1[iy]);
        }else{
            gsf_1[iy] = 0.;
        }
        if(cen_2[iy] > 0.){
            ccc = cen_2[iy];
            gsf_2[iy] = area_2[iy]/(ccc*ccc*ccc*p_2[iy]);
        }else{
            gsf_2[iy] = 0.;
        }
    }
    
    printf("\nIn the table below, the gSF is calculated from the number of counts at diagonal D1 and D2");
    printf("\nThe pairs are still not sewed together. Each gSF point (gsf_1 and gsf_2) is");
    printf("\ncalculated by gSF = Counts / (Eg**3 x p), where p is the fraction of levels with spin that");
    printf("\ncan feed the levels of the diagonals, e.g 0+ can be fed by 1+ and 1-, only.\n\n");

    printf("                Experimental    Eg = Ex - Exf                                                              Un-sewed\n");
    printf("  y      Ex    <Eg_1>  <Eg_2>   <Eg_1>  <Eg_2>   sig   Diag_1    Diag_2      p_1      p_2   p_2/p_1     gsf_1     gsf_2\n");
    printf("(ch)    (keV)   (keV)  (keV)     (keV)  (keV)         (Counts)  (Counts)                              (MeV**-3) (MeV**-3)\n");
    for(iy=chL;iy<=chH;iy++){
        ex = ch2ex(iy);
        printf(" %3d  %7.1f %7.1f %7.1f %7.1f %7.1f %6.3f %8.1f  %8.1f   %7.4f  %7.4f  %6.4f  %8.4e %8.4e\n",iy, ex, cenx_1[iy], cenx_2[iy], cen_1[iy], cen_2[iy], sig(ex,b1,b2), area_1[iy],area_2[iy], p_1[iy], p_2[iy], p_2[iy]/p_1[iy], gsf_1[iy], gsf_2[iy]);
    }

    /* ************************************ */
    /* Choosing Eg dispersion (keV/ch)      */
    /* Original areas are added according   */
    /* to a compression factor. We preserve */
    /* the accuracy of the data             */
    /* ************************************ */
    compress = 1;
    printf("\nWe use the Eg calibration of our gSF-spectra to be that of Ex.");
    printf("\nUsually we choose an Eg dispersion of about 120 keV/ch.");
    printf("\nAs default, you are asked to compress the channels to match about this value. ");
    printf("\nPresent value is:   %7.1f keV with compression %2d\n",a1fgy, compress);
    compress = (int)((120./a1fgy)+0.5);
    if (compress<1)compress = 1;
    a1c = compress*a1fgy;
    printf("Suggested value is: %7.1f keV with compression %2d\n", a1c, compress);
    printf("Give compression factor <%2d>:", compress);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%d", &compress);
    
    compressing();
       
    /* ***************************************************** */
    /* Now starting the sewing procedure of 2 and 2 pairs    */
    /* Multiply the next gSF pairs to match the previos gSF  */
    /* centroid. This is done at the midlle of the lowest    */
    /* and highest Eg, called ematch                         */
    /* ***************************************************** */
    printf("\nNow doing the sewing procedure of 2 and 2 pairs at Ex(i) and Ex(i+1).");
    printf("\nMultiply the next (i+1) gSF-pairs with a factor to match the ");
    printf("\nprevios (i) gSF centroid. This is done at the middle of    ");
    printf("\nthe lowest and highest Eg, called ematch. We also    ");
    printf("\nestimate the statistical uncertainty d_gsf from    ");
    printf("\nthe uncertainty in the number of counts at the diagonals.   ");
    printf("\nSince we cannot sew if one diagonal is zero, these pairs are zeroed.\n");
    
    printf("\nFor the sewing process, choose linear or logarithmical interpolation of gSF pairs.");
    printf("\nLinear (0) or logarithmical (1) interpolation <%1d>:",linlog);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%d", &linlog);
    
    sewing();
    
    printf("\n   Ex     <Eg_1>  <Eg_2>  ch_1  ch_2     gsf_1      gsf_2   d_gsf_1    d_gsf_2    ematch    factor\n");
    printf("  (keV)    (keV)   (keV)              (MeV**-3)  (MeV**-3)  (MeV**-3)  (MeV**-3)  (keV)\n");
    for(iy=chL;iy<=chH;iy++){
        ex = ch2ex(iy);
        printf(" %7.1f %7.1f %7.1f %5d %5d %8.4e %8.4e %8.4e %8.4e %7.1f %10.3e\n", ex, cen_1[iy], cen_2[iy], ch_1[iy], ch_2[iy], gsf_1[iy], gsf_2[iy], dgsf_1[iy], dgsf_2[iy],ematch[iy],factor[iy]);
    }
    
    /* ***************************************************** */
    /* Making one gSF and estimating statistical (dgsf_ave)  */
    /* and systematical (dsys) uncertainties                 */
    /* ***************************************************** */
    two2one();
    printf("\nFINAL RESULT, written in file results.dia \n");
    printf("  x     Eg       gsf       d_gsf      d_sys         gsf_1       gsf_2        dgsf_1       dgsf_2\n");
    printf("(ch)   (keV)  (MeV**-3)  (MeV**-3)  (MeV**-3)     (MeV**-3)   (MeV**-3)    (MeV**-3)   (MeV**-3)\n");
    fp = fopen("results.dia", "w");
       if(fp == NULL){
           printf("Could not open file results.dia \n");
           exit(0);
       }
    for(iy=L1;iy<=L4;iy++){
        dsys[iy] = delta_ave[iy] * gsf_ave[iy];
        fprintf(fp," %3d %7.1f %10.4e %10.4e %10.4e   %10.4e   %10.4e   %10.4e   %10.4e\n", iy, egam_ave[iy], gsf_ave[iy], dgsf_ave[iy], dsys[iy], gsf_1[iy+(chL-L2)], gsf_2[iy+(chL-L1)], dgsf_1[iy+(chL-L2)], dgsf_2[iy+(chL-L1)]);
        printf(    " %3d %7.1f %10.4e %10.4e %10.4e   %10.4e   %10.4e   %10.4e   %10.4e\n", iy, egam_ave[iy], gsf_ave[iy], dgsf_ave[iy], dsys[iy], gsf_1[iy+(chL-L2)], gsf_2[iy+(chL-L1)], dgsf_1[iy+(chL-L2)], dgsf_2[iy+(chL-L1)]);
    }
    fclose(fp);
	 

    
    
    /* **************************************************** */
    /* Storing default values for the next run in input.dia */
    /* **************************************************** */
    fp = fopen("input.dia", "w");
    if(fp == NULL){
        printf("Could not open file input.dia \n");
        exit(0);
    } else {
        fprintf(fp, " %f %f %f %f \n",x1L1, y1L1, x2L1, y2L1);
        fprintf(fp, " %f %f %f %f \n",x1U1, y1U1, x2U1, y2U1);
        fprintf(fp, " %f %f %f %f \n",x1L2, y1L2, x2L2, y2L2);
        fprintf(fp, " %f %f %f %f \n",x1U2, y1U2, x2U2, y2U2);
        fprintf(fp, " %f %f %f %f \n",exe1, sige1, exe2, sige2);
        fprintf(fp, " %f %f %d \n",xxj_1, xxj_2, oddeven);
        fprintf(fp, " %f %f  \n",Exf_1, Exf_2);
        fprintf(fp, " %f %f  \n",exL, exH);
        fprintf(fp, " %f %f  \n",n_1, n_2);
        fprintf(fp, " %d %d  \n",da01_1, da01_2);
        fprintf(fp, " %d %d  \n",nl1, nl2);
        fprintf(fp, " %d     \n",linlog);
        for(i=0;i<50;i++){
            fprintf(fp, " %f %f \n",spin_1[i], spin_2[i]);
        }
    }
    fclose(fp);
    
    makerootplot();
    printf("\nYou may plot your results by: root -l diablo_plot.cpp \n\n");

    return(0);
}

float prob( float sigma, float jspin)
{
    float xx, xj=0.;
    xx = (jspin+0.5)*(jspin+0.5)/(2.*sigma*sigma);
    if( xx <= 20 && jspin >= 0){
        xj = ((2.*jspin+1.)/(2.*sigma*sigma))*exp(-xx);
    }
    if(gsum(sigma,jlow) > 0.)xj = xj/gsum(sigma,jlow);
    return xj;
}

float gsum( float sigma, float jlow)
{
    float sp;
    float xx, xj = 0.;
    for( sp = jlow; sp < jlow + 20.; sp++){
        xx = (sp+0.5)*(sp+0.5)/(2.*sigma*sigma);
        if( xx <= 40 && sp >= 0){
            xj = xj + ((2.*sp+1.)/(2.*sigma*sigma))*exp(-xx);
        }else{
        }
    }
    return xj;
}

float sig(float ex, float b1, float b2){
    float sig2;
    sig2 = b1 + b2*ex;
    if(sig2 < 0.5) sig2 = 0.5;
    return sqrt(sig2);
}

int ex2ch(float ex){
    int ch;
    ch = (int)(((ex-a0fgy)/a1fgy)+0.5);
    return ch;
}
int eg2ch(float eg){
    int ch;
    ch = (int)(((eg-a0fgx)/a1fgx)+0.5);
    return ch;
}
float ch2eg(int ch){
    return a0fgx + (float)ch*a1fgx;
}
float ch2ex(int ch){
    return a0fgy + (float)ch*a1fgy;
}

//The + or - a1fgx/2 is to assure that the chosen energy is within the
// upper and lower diagonal boarders enclosing the diagonal
float ex2egL1(float ex){
    return x1L1 + ((x2L1-x1L1)/(y2L1-y1L1))*(ex-y1L1) + (a1fgx/2.) ;
}
float ex2egL2(float ex){
    return x1L2 + ((x2L2-x1L2)/(y2L2-y1L2))*(ex-y1L2) + (a1fgx/2.);
}
float ex2egU1(float ex){
    return x1U1 + ((x2U1-x1U1)/(y2U1-y1U1))*(ex-y1U1) - (a1fgx/2.);
}
float ex2egU2(float ex){
    return x1U2 + ((x2U2-x1U2)/(y2U2-y1U2))*(ex-y1U2) - (a1fgx/2.);
}

void compressing(){
    int i, ch;
    float temp1[2048],temp2[2048],temp3[2048],temp4[2048],temp5[2048],temp6[2048],temp7[2048],temp8[2048],temp9[2048],temp10[2048];
    float  sumgsf1,sumgsf2,sumcen1,sumcen2,sumr1,sumr2,sumarea1,sumarea2,sump1,sump2,numb;
    for(i=0; i< 2048;i++ ){
        temp1[i]  = 0.;
        temp2[i]  = 0.;
        temp3[i]  = 0.;
        temp4[i]  = 0.;
        temp5[i]  = 0.;
        temp6[i]  = 0.;
        temp7[i]  = 0.;
        temp8[i]  = 0.;
        temp9[i]  = 0.;
        temp10[i] = 0.;
    }
    ex = (ch2ex(chH)+ch2ex(chH-(compress-1)))/2.;
    ch = (int)((chH/compress)+0.5);
    a1c = compress*a1fgy;
    a0c= ex - a1c*ch;
    printf("New gSF(Eg) calibration is: Eg = %7.1f keV +%8.3f (keV/ch)*ch\n", a0c, a1c);

    for (iy=chH; iy>=chL;iy=iy-compress){  //starting at high Ex, where the shape method works best
        sump1=0.;sump2=0.;sumarea1=0.;sumarea2=0.;sumgsf1=0.;sumgsf2=0.;sumcen1=0.;sumcen2=0.;sumr1=0.;sumr2=0.;numb=0;
        for(i=0;(i<compress)&&(i<=iy-chL);i++){
            sumgsf1 = sumgsf1 + gsf_1[iy-i];
            sumgsf2 = sumgsf2 + gsf_2[iy-i];
            sumcen1 = sumcen1 + cen_1[iy-i];
            sumcen2 = sumcen2 + cen_2[iy-i];
            sumr1   = sumr1   + 1./(r_area_1[iy-i]*r_area_1[iy-i]);
            sumr2   = sumr2   + 1./(r_area_2[iy-i]*r_area_2[iy-i]);
            sumarea1= sumarea1+ area_1[iy-i];
            sumarea2= sumarea2+ area_2[iy-i];
            sump1   = sump1   + p_1[iy-i];
            sump2   = sump2   + p_2[iy-i];
            if (gsf_1[iy-i]+gsf_2[iy-i] > 0.)numb = numb + 1.0;
        }
        temp1[ch] = sumgsf1/numb;
        temp2[ch] = sumgsf2/numb;
        temp3[ch] = sumcen1/numb;
        temp4[ch] = sumcen2/numb;
        temp5[ch] = 1./sqrt(sumr1);
        temp6[ch] = 1./sqrt(sumr2);
        temp7[ch] = sumarea1/numb;
        temp8[ch] = sumarea2/numb;
        temp9[ch] = sump1/numb;
        temp10[ch]= sump2/numb;
        ch = ch - 1;
    }
    for(i=0; i< 2048;i++ ){
        gsf_1[i]   = temp1[i];
        gsf_2[i]   = temp2[i];
        dgsf_1[i]   = temp1[i]*temp5[i];
        dgsf_2[i]   = temp2[i]*temp6[i];
        
        cen_1[i]   = temp3[i];
        cen_2[i]   = temp4[i];
        r_area_1[i]= temp5[i];
        r_area_2[i]= temp6[i];
        area_1[i]  = temp7[i];
        area_2[i]  = temp8[i];
        p_1[i]     = temp9[i];
        p_2[i]     = temp10[i];
    }
    chL = ch+1; chH = (int)((chH/compress)+0.5);
    a0fgy = a0c; a1fgy = a1c;
}

void sewing(){
    double x_1pair, x_2pair;
    for(iy=chL;iy<=chH;iy++){
        if(cen_1[iy] > 0.){
            ccc = cen_1[iy];
            gsf_1[iy] = area_1[iy]/(ccc*ccc*ccc*p_1[iy]);
        }else{
            gsf_1[iy] = 0.;
        }
        if(cen_2[iy] > 0.){
            ccc = cen_2[iy];
            gsf_2[iy] = area_2[iy]/(ccc*ccc*ccc*p_2[iy]);
        }else{
            gsf_2[iy] = 0.;
        }
    }
    ematch_previous=9999.9;
    factor_previous=9999.;
    for(iy=chL;iy<=chH;iy++){
        ematch_next = 0.;
        factor_next = 0.;
        ex = ch2ex(iy);
        ch_1[iy]    = ex2ch(cen_1[iy]);     // First pair
        ch_2[iy]    = ex2ch(cen_2[iy]);
        ch_1[iy+1]  = ex2ch(cen_1[iy+1]);   // Next pair
        ch_2[iy+1]  = ex2ch(cen_2[iy+1]);
        if (gsf_1[iy]>0. && gsf_2[iy]>0. && gsf_1[iy+1]>0. && gsf_2[iy+1]>0.){  // Only sew if all four integrals have counts > 0
            ematch_next = (cen_2[iy] + cen_1[iy+1])/2.;
            if(linlog == 0){
                gSFave_1pair = gsf_1[iy]  +((gsf_2[iy]  -gsf_1[iy])  /(cen_2[iy]  -cen_1[iy]))  *(ematch_next-cen_1[iy]);
                gSFave_2pair = gsf_1[iy+1]+((gsf_2[iy+1]-gsf_1[iy+1])/(cen_2[iy+1]-cen_1[iy+1]))*(ematch_next-cen_1[iy+1]);
                if (gSFave_2pair>0. && gSFave_1pair > 0.)factor_next = gSFave_1pair/gSFave_2pair;
                gsf_1[iy+1]  = gsf_1[iy+1]*factor_next;     //finished normalized for the next loop
                gsf_2[iy+1]  = gsf_2[iy+1]*factor_next;     //finished normalized for the next loop
            }
            else{
                x_1pair = log((double)gsf_1[iy])  +((log((double)gsf_2[iy])  -log((double)gsf_1[iy]))  /(cen_2[iy]  -cen_1[iy]))  *(ematch_next-cen_1[iy]);
                x_2pair = log((double)gsf_1[iy+1])+((log((double)gsf_2[iy+1])-log((double)gsf_1[iy+1]))/(cen_2[iy+1]-cen_1[iy+1]))*(ematch_next-cen_1[iy+1]);
                
                
                gSFave_1pair = exp(x_1pair); //from log to normal
                gSFave_2pair = exp(x_2pair); //from log to normal
//                printf("%e  %e  %e  %e\n",x_1pair,x_2pair,gSFave_1pair, gSFave_2pair);

                if (gSFave_2pair>0. && gSFave_1pair > 0.)factor_next = gSFave_1pair/gSFave_2pair;
                gsf_1[iy+1]  = gsf_1[iy+1]*factor_next;     //finished normalized for the next loop
                gsf_2[iy+1]  = gsf_2[iy+1]*factor_next;     //finished normalized for the next loop
            }
        }
        else{
            gsf_1[iy+1]=0.,gsf_2[iy+1]=0.;              //for the next loop
        }
        dgsf_1[iy] = r_area_1[iy]*gsf_1[iy];
        dgsf_2[iy] = r_area_2[iy]*gsf_2[iy];
        ematch[iy] = ematch_previous;
        factor[iy] = factor_previous;
        ematch_previous = ematch_next;
        factor_previous = factor_next;
    }
}

void two2one(){ //Making one gSF and estimating statistical (dgsf_ave) and systematical (delta_ave) uncertainties
    int l;
    float dA_A;
    L1 = ex2ch(cen_2[chL]);
    L2 = ex2ch(cen_1[chL]);
    L3 = ex2ch(cen_2[chH]);
    L4 = ex2ch(cen_1[chH]);
    for(l=L1;l<L2;l++){
        gsf_ave[l]  = gsf_2[l+(chL-L1)];
        dgsf_ave[l] = r_area_2[l+(chL-L1)]*gsf_ave[l];
        egam_ave[l] = cen_2[l+(chL-L1)];
    }
    for(l=L2;l<=L3;l++){
        gsf_ave[l]  = (gsf_1[l+(chL-L2)] + gsf_2[l+(chL-L1)])/2.;
        dA_A = 1./sqrt( (1./r_area_1[l+(chL-L2)])*(1./r_area_1[l+(chL-L2)]) + (1./r_area_2[l+(chL-L1)])*(1./r_area_2[l+(chL-L1)]) );
        dgsf_ave[l] = dA_A * gsf_ave[l];
        delta[l]    = fabsf(gsf_ave[l] - gsf_1[l+(chL-L2)])/gsf_ave[l];
        egam_ave[l] = (cen_1[l+(chL-L2)] + cen_2[l+(chL-L1)])/2.;
    }
    for(l=L3+1;l<=L4;l++){
        gsf_ave[l]  = gsf_1[l+(chL-L2)];
        dgsf_ave[l] = r_area_1[l+(chL-L2)]*gsf_ave[l];
        egam_ave[l] = cen_1[l+(chL-L2)];
    }
    linfit(L2, L2+(L3-L2)/3);
    for(l=L1;l<L2;l++){
        delta[l]  = a + b * egam_ave[l];
        if(delta[l]<0.05)delta[l]=0.05;
        if(delta[l]>1.00)delta[l]=1.00;
    }
    linfit(L3-(L3-L2)/3, L3);
    for(l=L3+1;l<=L4;l++){
        delta[l]  = a + b * egam_ave[l];
        if(delta[l]<0.05)delta[l]=0.05;
        if(delta[l]>1.00)delta[l]=1.00;
    }
    delta_ave[L1]   = delta[L1];
    delta_ave[L1+1] = delta[L1+1];
    delta_ave[L4]   = delta[L4];
    delta_ave[L4-1] = delta[L4-1];

    for(l=L1+2;l<=L4-2;l++){        //smoothing relative errors by 5 and 5 channels
        delta_ave[l] = (delta[l-2] + delta[l-1] + delta[l] + delta[l+1] + delta[l+2])/5.;
    }
}

void linfit(int Low, int High){ //Chi**2 fit with y = a + b*x
    int l;
    float A=0, B=0, C=0, D=0, E=0, F=0, sig2 = 0.01;   // Do not care about sigma and chi**2
    for (l=Low;l<=High;l++){
        A = A +            egam_ave[l]/sig2;
        B = B +                     1./sig2;
        C = C +               delta[l]/sig2;
        D = D +egam_ave[l]*egam_ave[l]/sig2;
        E = E +   egam_ave[l]*delta[l]/sig2;
        F = F +      delta[l]*delta[l]/sig2; //not used
    }
    a   = (D*C-E*A)/(D*B-A*A);
    b   = (E*B-C*A)/(D*B-A*A);
}

int makerootplot(){
    fp = fopen("diablo_plot.cpp", "w+");
    if(fp == NULL){
        printf("Could not open file diablo_plot.cpp \n");
        exit(0);
    }
    else {
        int middle  = (L1 + L4)/2;
        float xnorm = (5.e-08)/gsf_ave[middle];  //Scaling gSF in the middle of the data set to 5.e-08 MeV**(-3)
        float EgMax = 0.5 + (egam_ave[L4])/1000.; // In MeV for plotting gSF
        fprintf(fp,"{\n");
        fprintf(fp,"    gROOT->Reset();\n");
        fprintf(fp,"    gROOT->SetStyle(\"Plain\");\n");
        fprintf(fp,"    gStyle->SetOptTitle(0);\n");
        fprintf(fp,"    gStyle->SetOptStat(0);\n");
        fprintf(fp,"    gStyle->SetFillColor(0);\n");
        fprintf(fp,"    gStyle->SetPadBorderMode(0);\n");
        fprintf(fp,"    m = (TH1F*)gROOT->FindObject(\"h\");\n");
        fprintf(fp,"    if (m) m->Delete();\n");
        fprintf(fp,"    TCanvas *c1 = new TCanvas(\"c1\",\"Gamma-ray strength function\",800,600);\n");
        fprintf(fp,"    float EgMax = %e;\n",EgMax);
        fprintf(fp,"    TH2F *h     = new TH2F(\"h\",\" \",10,0.0, EgMax,10,5.0e-9,5.0e-07);\n");
        fprintf(fp,"    ifstream diafile(\"results.dia\");\n");
        fprintf(fp,"    float eg[500], energyerr[500];\n");
        fprintf(fp,"    float gsf_ave[500],dgsf_ave[500],dsys_ave[500];\n");
        fprintf(fp,"    float gsf1[500],gsf2[500],dgsf1[500],dgsf2[500];\n");
        fprintf(fp,"    float sysL[500], sysH[500];\n");
        fprintf(fp,"    float Eg, gsf, dgsf, dsys, gsf_1,gsf_2,dgsf_1,dgsf_2;\n");
        fprintf(fp,"    int   idum;\n");
        fprintf(fp,"    i = 0;\n");
        fprintf(fp,"    float xnorm = %e;\n",xnorm);
        fprintf(fp,"    while(diafile){\n");
        fprintf(fp,"        diafile >>idum >> Eg >>gsf>>dgsf>>dsys>>gsf_1>>gsf_2>>dgsf_1>>dgsf_2;\n");
        fprintf(fp,"        eg[i]        = Eg/1000.;\n");
        fprintf(fp,"        gsf_ave[i]   = gsf*xnorm;\n");
        fprintf(fp,"        dgsf_ave[i]  = dgsf*xnorm;\n");
        fprintf(fp,"        sysL[i]      = (gsf-dsys)*xnorm;\n");
        fprintf(fp,"        sysH[i]      = (gsf+dsys)*xnorm;\n");
        fprintf(fp,"        gsf1[i]      = gsf_1*xnorm;\n");
        fprintf(fp,"        gsf2[i]      = gsf_2*xnorm;\n");
        fprintf(fp,"        dgsf1[i]     = dgsf_1*xnorm;\n");
        fprintf(fp,"        dgsf2[i]     = dgsf_2*xnorm;\n");
        fprintf(fp,"        energyerr[i] = 0.;\n");
        fprintf(fp,"        i++;\n");
        fprintf(fp,"    }\n");
        fprintf(fp,"    TGraphErrors *gsfgr = new TGraphErrors(i,eg,gsf_ave,energyerr,dgsf_ave);\n");
        fprintf(fp,"    TGraph       *sysLgr = new TGraphErrors(i,eg,sysL);\n");
        fprintf(fp,"    TGraph       *sysHgr = new TGraphErrors(i,eg,sysH);\n");
        fprintf(fp,"    TGraphErrors *gsf1gr = new TGraphErrors(i,eg,gsf1,energyerr,dgsf1);\n");
        fprintf(fp,"    TGraphErrors *gsf2gr = new TGraphErrors(i,eg,gsf2,energyerr,dgsf2);\n");
        fprintf(fp,"    c1->SetLogy();\n");
        fprintf(fp,"    c1->SetLeftMargin(0.10);\n");
        fprintf(fp,"    c1->SetRightMargin(0.02);\n");
        fprintf(fp,"    c1->SetTopMargin(0.07);\n");
        fprintf(fp,"    c1->SetBottomMargin(0.10);\n");
        fprintf(fp,"    h->GetXaxis()->CenterTitle();\n");
        fprintf(fp,"    h->GetXaxis()->SetTitleOffset(1.1);\n");
        fprintf(fp,"    h->GetXaxis()->SetTitle(\"#gamma-ray energy E_{#gamma} (MeV)\");\n");
        fprintf(fp,"    h->GetXaxis()->SetTitleFont(42);\n");
        fprintf(fp,"    h->GetXaxis()->SetLabelFont(42);\n");
        fprintf(fp,"    h->GetYaxis()->CenterTitle();\n");
        fprintf(fp,"    h->GetYaxis()->SetTitleOffset(1.2);\n");
        fprintf(fp,"    h->GetYaxis()->SetTitle(\"#gamma-ray strength function (MeV^{-3})\");\n");
        fprintf(fp,"    h->GetYaxis()->SetTitleFont(42);\n");
        fprintf(fp,"    h->GetYaxis()->SetLabelFont(42);\n");
        fprintf(fp,"    h->Draw();\n");
        fprintf(fp,"    gsfgr->SetMarkerStyle(20);\n");
        fprintf(fp,"    gsfgr->SetMarkerSize(0.6);\n");
        fprintf(fp,"    gsfgr->SetMarkerColor(kBlack);\n");
        fprintf(fp,"    gsfgr->Draw(\"P\");\n");
        fprintf(fp,"    gsf1gr->SetMarkerStyle(22);\n");
        fprintf(fp,"    gsf1gr->SetMarkerSize(0.6);\n");
        fprintf(fp,"    gsf1gr->SetMarkerColor(kRed);\n");
        fprintf(fp,"    gsf1gr->Draw(\"P\");\n");
        fprintf(fp,"    gsf2gr->SetMarkerStyle(22);\n");
        fprintf(fp,"    gsf2gr->SetMarkerSize(0.6);\n");
        fprintf(fp,"    gsf2gr->SetMarkerColor(kBlue);\n");
        fprintf(fp,"    gsf2gr->Draw(\"P\");\n");
        fprintf(fp,"    sysLgr->SetMarkerStyle(20);\n");
        fprintf(fp,"    sysLgr->SetMarkerSize(0.6);\n");
        fprintf(fp,"    sysLgr->SetMarkerColor(kBlack);\n");
        fprintf(fp,"    sysLgr->Draw(\"L\");\n");
        fprintf(fp,"    sysHgr->SetMarkerStyle(20);\n");
        fprintf(fp,"    sysHgr->SetMarkerSize(0.6);\n");
        fprintf(fp,"    sysHgr->SetMarkerColor(kBlack);\n");
        fprintf(fp,"    sysHgr->Draw(\"L\");\n");
        fprintf(fp,"    TLegend *leg = new TLegend(0.15,0.75,0.55,0.90);\n");
        fprintf(fp,"    leg->SetBorderSize(0);\n");
        fprintf(fp,"    leg->SetFillColor(0);\n");
        fprintf(fp,"    leg->SetTextFont(42);\n");
        fprintf(fp,"    leg->AddEntry(gsfgr,\"Average gSF\",\"P\");\n");
        fprintf(fp,"    leg->AddEntry(gsf1gr,\"Gammas feeding D1\",\"P\");\n");
        fprintf(fp,"    leg->AddEntry(gsf2gr,\"Gammas feeding D2\",\"P\");\n");
        fprintf(fp,"    leg->AddEntry(sysHgr,\"Systematical errors\",\"L\");\n");
        fprintf(fp,"    leg->Draw();\n");
        fprintf(fp,"    TLatex t;\n");
        fprintf(fp,"    t.SetTextSize(0.05);\n");
        fprintf(fp,"    t.SetTextFont(42);\n");
        fprintf(fp,"    t.DrawLatex( 0.6*EgMax ,3.5e-07,\"^{xxx}Yy\");\n");
        fprintf(fp,"    c1->Update();\n");
        fprintf(fp,"    c1->Print(\"diablo_plot.pdf\");\n");
        fprintf(fp,"}\n");
    }
    fclose(fp);
    return 0;
}

