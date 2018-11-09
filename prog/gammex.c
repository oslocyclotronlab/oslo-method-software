#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>    
#include <math.h>    
#include <fcntl.h>						/* Header for open/read/write */ 
#include <errno.h> 
#include <sys/types.h>          
#include <sys/ioctl.h>          
#define MAXDIM    8192					/* Max dimension of Ex or Eg spectra */
#define MAXCONF   100                   /* The number of initial levels with specific quantuum numbers */
#define BN0       9.154 			    /* Default Bn of 96Mo */
#define PI0       3.14159265358979      /* The pi number */

#define MIN(a,b)((a)<(b) ? (a) : (b))

FILE *fp;
char line[128],cdum[128];
int i, j, k, sp, Nch, conf, dimx, dimy, jH, jL;
float rho[MAXDIM], spincut[MAXDIM], transext[MAXDIM],gammex[MAXDIM],gammex_tot[MAXDIM],gammex_1[MAXDIM],gammex_2[MAXDIM];
float Ispin[MAXCONF],Iparity[MAXCONF],Iweight[MAXCONF],Iweight2D[MAXCONF][MAXDIM];
float spin, lowsp, D;
double sum, sumconf, c, x;
float xa, xb, ya, yb, y;
float Emax;
int parity, ch_max;
float mat[4096][2048];
int x1L1=60, y1L1=60, x2L1=200, y2L1=200;
int x1U1=60, y1U1=70, x2U1=200, y2U1=220;
int x1L2=60, y1L2=90, x2L2=200, y2L2=240;
int x1U2=60, y1U2=100,x2U2=200, y2U2=280;
float  e1L=-0.2, e1U=0.05, e2L=0.05, e2U=0.2, e3L=0.2, e3U=0.4, e4L=0.4, e4U=0.8;
int    c1L,      c1U,      c2L,      c2U,     c3L,     c3U,     c4L,     c4U;
float I1=0., P1=+1., I2=2., P2=+1, I3=4., P3=+1., I4=6., P4=+1;
float zero0, zero1=1., zero2=1., zero3=1., zero4=1.;
float rL1,rU1,rL2,rU2,rL3,rU3,rL4,rU4;
float area_t[2048],area_1[2048],area_2[2048];
float area_c_t[2048],area_c_1[2048],area_c_2[2048];
float e_u,e_c;
float int_t,sint_t,int_1,sint_1,int_2,sint_2,int_3,sint_3,int_4,sint_4;

/* Defining defaults values, taken from 96Mo */
float Bn = BN0, a0 = 60., a1 = 120.;
int   eo = 0, Iconf = 1, Nconf, Nsp;
double partialsum(int ji, int jf, float Ii, int Pi);
double g(int jexc, float jspin, int jparity);
double gsum(int jexc);
void  fastenseatbelts(int jH, int jL);
float a0fgx, a1fgx, a2fgx, a0fgy, a1fgy, a2fgy;
int   dimfgx, dimfgy,ix,iy;
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
    printf(" |                        G A M M E X  1.3                      |\r\n");
    printf(" |                                                              |\r\n");
    printf(" |              Program to calculate <Gamma_g(Ex)>              |\r\n");
    printf(" |   (run counting and normalization prior to this program)     |\r\n");
    printf(" |                                                              |\r\n");
    printf(" |  Input files: transext.nrm     Output files: input.gex       |\r\n");
    printf(" |               rhotmopaw.cnt                  gammex.gex      |\r\n");
    printf(" |               spincut.cnt                    intensities.gex |\r\n");
    printf(" |               rhosp.rsg                                      |\r\n");
    printf(" |               fg                                             |\r\n");
    printf(" |               input.nrm                                      |\r\n");
    printf(" |               (input.gex)                                    |\r\n");
    printf(" | E-mail  : magne.guttormsen@fys.uio.no                        |\r\n");
    printf(" | Created : 14 Nov 2006                                        |\r\n");
    printf(" | Modified: 12 Dec 2006                                        |\r\n");
    printf(" | Modified: 08 Jun 2017   Four low states implemented          |\r\n");
    printf(" |______________________________________________________________|\r\n");
    printf("                                                                 \r\n");
    
    /* ********************* */
    /* Initializing defaults */
    /* ********************* */
    Nconf = 2;
    for (i = 0; i < MAXCONF+1; i++){
        Ispin[i]   = 0;
        Iparity[i] = 0;
        Iweight[i] = 0;
    }
    Ispin[0]   = 1.5;
    Iparity[0] =  1.;
    Iweight[0] = 0.5;
    Ispin[1]   = 2.5;
    Iparity[1] = -1.;
    Iweight[1] = 0.5;

    /* ****************************************************** */
    /* Reading default values from previous normalization run */
    /* ****************************************************** */
    fp = fopen("input.nrm", "r");
    if(fp == NULL){
        printf("\nCould not open file input.nrm, default values are used \n");
    }
    else {
        float fdum;
        int idum;
        fgets_ignore(line,sizeof(line),fp);
        sscanf(line, " %f %f %f %d %d \n", &Bn, &fdum, &fdum, &idum, &idum);
        fclose(fp);
    }
    
    /* ***************************************** */
    /* Reading default values from previous runs */
    /* ***************************************** */
    fp = fopen("input.gex", "r");
    if(fp == NULL){
        printf("\nCould not open file input.gex, default values are used \n");
    }
    else {
        fgets_ignore(line,sizeof(line),fp);
        sscanf(line, " %f %d %d %d \n", &Bn, &eo, &Iconf, &Nconf);
        fgets_ignore(line,sizeof(line),fp);
        sscanf(line, " %d %d %d %d \n",&x1L1, &y1L1, &x2L1, &y2L1);
        fgets_ignore(line,sizeof(line),fp);
        sscanf(line, " %d %d %d %d \n",&x1U1, &y1U1, &x2U1, &y2U1);
        fgets_ignore(line,sizeof(line),fp);
        sscanf(line, " %d %d %d %d \n",&x1L2, &y1L2, &x2L2, &y2L2);
        fgets_ignore(line,sizeof(line),fp);
        sscanf(line, " %d %d %d %d \n",&x1U2, &y1U2, &x2U2, &y2U2);
        fgets_ignore(line,sizeof(line),fp);
        sscanf(line, " %f %f %f %f %f %f %f %f \n",&e1L,  &e1U, &e2L,&e2U, &e3L, &e3U, &e4L, &e4U);
        fgets_ignore(line,sizeof(line),fp);
        sscanf(line, " %f %f %f %f %f %f %f %f \n",&I1,   &P1,  &I2, &P2,  &I3,  &P3,  &I4,  &P4);
        fgets_ignore(line,sizeof(line),fp);
        sscanf(line, " %f %f %f %f \n",&zero1,  &zero2, &zero3,  &zero4);
        for (i = 0; i < Nconf; i++){
            fgets_ignore(line,sizeof(line),fp);
            sscanf(line," %f   %f  %f\n", &Ispin[i], &Iparity[i], &Iweight[i]);
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
//           printf(" j = %d i = %d Ex = %f Eg = %f counts = %e\n",iy,ix,a0fgy+a1fgy*(float)iy,a0fgx+a1fgx*(float)ix,mat[ix][iy]);
        }
    }
    printf("Dimension (0 : %d, 0 : %d) and calibration \n(ax0, ax1, ax2) = (%f,%f,%f) \n(ay0, ay1, ay2) = (%f,%f,%f)\n",dimfgx,dimfgy,a0fgx,a1fgx,a2fgx,a0fgy,a1fgy,a2fgy);
    fclose(fp);
    
    /* ********************************************** */
    /* Reading coordinates for 4 diagonal lines in fg */
    /* ********************************************** */
    
    printf("\nWe are going to define diagonals in the fg matrix, ");
    printf("\nwhich is assumed to have lower dispertion (dE/ch)");
    printf("\nthan the fg.rsg matrix. Thus, the intensity determination is");
    printf("\nassumed to be more accurate. Next step is then to add");
    printf("\ntogether the intensities so that it corresponds to the fg.rsg calibration\n");
    
    printf("\nReading coordinates for 4 diagonal lines in fg. ");
    printf("\nThese are typically the lower and upper lines");
    printf("\nfor feeding to the gs 0+ and 1st excited 2+ diagonals.");
    printf("\nThe 4 lines are called L1, U1, L2 and U2.");
    printf("\nEach line requires two coordinates: the first");
    printf("\nand the second coordinate(xch,ych).");
    printf("\nYou will now be asked for 16 numbers, be brave!\n");
    printf("\nYou may want instead to taylor the input.gex file\n");
    
    printf("\nLower line L1 for diagonal 1:\n");
    printf("Give x-ch for first coordinate  <%4d>:",x1L1);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%d", &x1L1);
    printf("Give y-ch for first coordinate  <%4d>:",y1L1);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%d", &y1L1);
    printf("Give x-ch for second coordinate <%4d>:",x2L1);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%d", &x2L1);
    printf("Give y-ch for second coordinate <%4d>:",y2L1);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%d", &y2L1);
    rL1 = (float)(x2L1-x1L1)/(float)((y2L1-y1L1)); //slope of the diagonal line
    
    printf("\nUpper line U1 for diagonal 1:\n");
    printf("Give x-ch for first coordinate  <%4d>:",x1U1);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%d", &x1U1);
    printf("Give y-ch for first coordinate  <%4d>:",y1U1);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%d", &y1U1);
    printf("Give x-ch for second coordinate <%4d>:",x2U1);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%d", &x2U1);
    printf("Give y-ch for second coordinate <%4d>:",y2U1);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%d", &y2U1);
    rU1 = (float)(x2U1-x1U1)/((float)(y2U1-y1U1)); //slope of the diagonal line

    printf("\nLower line L2 for diagonal 2:\n");
    printf("Give x-ch for first coordinate  <%4d>:",x1L2);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%d", &x1L2);
    printf("Give y-ch for first coordinate  <%4d>:",y1L2);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%d", &y1L2);
    printf("Give x-ch for second coordinate <%4d>:",x2L2);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%d", &x2L2);
    printf("Give y-ch for second coordinate <%4d>:",y2L2);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%d", &y2L2);
    rL2 = (float)(x2L2-x1L2)/((float)(y2L2-y1L2)); //slope of the diagonal line

    printf("\nUpper line U2 for diagonal 2:\n");
    printf("Give x-ch for first coordinate  <%4d>:",x1U2);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%d", &x1U2);
    printf("Give y-ch for first coordinate  <%4d>:",y1U2);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%d", &y1U2);
    printf("Give x-ch for second coordinate <%4d>:",x2U2);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%d", &x2U2);
    printf("Give y-ch for second coordinate <%4d>:",y2U2);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%d", &y2U2);
    rU2 = (float)(x2U2-x1U2)/((float)(y2U2-y1U2)); //slope of the diagonal line
    
    printf("\nTake a look if the slopes of the 4 lines are approx. similar\n");
    printf("Slopes of (L1,U1) = (%8.3f, %8.3f) and (L2,U2) = (%8.3f, %8.3f)\n",rL1,rU1,rL2,rU2);
    
    
    /* ******************************************************** */
    /* Finding integral (area) of total fg and diagonal 1 and 2 */
    /* ******************************************************** */
    for(iy=0;iy<=dimfgy;iy++){
        for(ix=0;ix<=dimfgx;ix++){
            if((ix<=x1L1+rL1*(iy-y1L1)) && (ix>=                 0))area_t[iy]=area_t[iy]+mat[ix][iy];
            if((ix<=x1L1+rL1*(iy-y1L1)) && (ix>=x1U1+rU1*(iy-y1U1)))area_1[iy]=area_1[iy]+mat[ix][iy];
            if((ix<=x1L2+rL2*(iy-y1L2)) && (ix>=x1U2+rU2*(iy-y1U2)))area_2[iy]=area_2[iy]+mat[ix][iy];
        }
    }
    for(iy=0;iy<=dimfgy;iy++){
        printf("Ex= %7.0f A_tot= %8.1f  A_1= %8.1f  A_2= %8.1f \n",a0fgy + a1fgy*(float)iy,area_t[iy],area_1[iy],area_2[iy]);
    }
    
    /* ************************************************** */
    /* Reading calibration and dimensions from: rhosp.rsg */
    /* ************************************************** */
    printf("\nReading calibration and dimensions from: rhosp.rsg\n");
    fp = fopen("rhosp.rsg", "r");
    if(fp == NULL){
        printf("No rhosp.rsg file found in your directory\n");
        exit(0);
    } else {
        fgets_ignore(line,sizeof(line),fp);
        fgets_ignore(line,sizeof(line),fp);
        fgets_ignore(line,sizeof(line),fp);
        fgets_ignore(line,sizeof(line),fp);
        fgets_ignore(line,sizeof(line),fp);
        fgets_ignore(line,sizeof(line),fp);
        fgets_ignore(line,sizeof(line),fp);
        sscanf(line,"%13s %7s %f %s %f",cdum, cdum, &a0, cdum, &a1);
        fgets_ignore(line,sizeof(line),fp);
        fgets_ignore(line,sizeof(line),fp);
        sscanf(line,"%s %d %s %d",cdum, &dimx, cdum, &dimy);
        fgets_ignore(line,sizeof(line),fp);
        fclose(fp);
    }
    printf("Dimension (0 : %d, 0 : %d) and calibration (a0, a1) = (%f,%f)\n",dimx,dimy,a0,a1);
    Emax = a0fgy + a1fgy*(float)(dimfgy-1);
    ch_max = (int)(((Emax-a0)/a1)+0.5);
    
    /* **************************************************** */
    /* Compressing area array to fit calibration of fg.rsg */
    /* **************************************************** */
    printf("\nCompressing area array to fit calibration of fg.rsg \n");
    printf("Warning, works only for positive a1 (keV/ch)! \n");
   // printf(" No  Ex(keV)            A_tot             A_1               A_2 \n");
    for(i = 0 ; i < ch_max; i++){
        e_c  = a0+a1*(float)i;
        for(iy=0;iy<=dimfgy;iy++){
            e_u = a0fgy + a1fgy*(float)iy;
            if((e_u>=e_c-(a1/2.))&&(e_u < e_c+(a1/2.))){
                area_c_t[i]= area_c_t[i]+area_t[iy];
                area_c_1[i]= area_c_1[i]+area_1[iy];
                area_c_2[i]= area_c_2[i]+area_2[iy];
            }
        }
//        printf("%3d  %8.2f  %14.3e  %14.3e  %14.3e \n",i,a0+a1*(float)i,area_c_t[i],area_c_1[i],area_c_2[i]);
    }

    /* ****************************************************** */
    /* Writing areas and relative areas on file               */
    /* The factor of 2 is due to total spectra = twice the fg */
    /* ****************************************************** */
    fp = fopen("intensities.gex", "w");
    if(fp == NULL){
        printf("Could not open file intensities.gex \n");
        exit(0);
    } else {
    printf(" ch   Ex(keV)     Area_tot    dArea_tot      Area_1      dArea_1       Area_2      dArea_2 \n");
    for(i = 0 ; i < ch_max; i++){
        e_c  = a0+a1*(float)i;
        int_t  = 1.;
        sint_t = 0;
        int_1  = 0;
        sint_1 = 0;
        int_2  = 0;
        sint_2 = 0;
        if(area_c_t[i] > 0.)sint_t = int_t*sqrt(2.*area_c_t[i])/area_c_t[i];
        if(area_c_t[i] > 0.)int_1  = area_c_1[i]/area_c_t[i];
        if(area_c_t[i] > 0. && area_c_1[i] > 0.)sint_1 = int_1*sqrt( (2./area_c_1[i]) + (2./area_c_t[i]));
        if(area_c_t[i] > 0.)int_2  = area_c_2[i]/area_c_t[i];
        if(area_c_t[i] > 0. && area_c_2[i] > 0.)sint_2 = int_2*sqrt( (2./area_c_2[i]) + (2./area_c_t[i]));

        printf(    "%3d  %8.2f  %11.4e  %11.4e  %11.4e  %11.4e  %11.4e  %11.4e \n",i,e_c,int_t,sint_t, int_1,sint_1,int_2,sint_2);
        fprintf(fp,"%3d  %8.2f  %11.4e  %11.4e  %11.4e  %11.4e  %11.4e  %11.4e \n",i,e_c,int_t,sint_t, int_1,sint_1,int_2,sint_2);
        }
    }
    fclose(fp);

    /* ***************************************************************** */
    /* Reading data of experimental nuclear level density: rhotmopaw.cnt */
    /* ***************************************************************** */
    printf("\nReading data of experimental nuclear level density: rhotmopaw.cnt\n");
    fp = fopen("rhotmopaw.cnt", "r");
    if(fp == NULL){
        printf("No rhotmopaw.cnt file found in your directory\n");
        exit(0);
    }
    i = 0 ;
    while( i < ch_max){
        if(fgets(line,sizeof(line),fp) != NULL){sscanf(line,"%f", &rho[i]);}
        i++;
    }
    fclose(fp);
    
    /* ****************************************************************** */
    /* Reading data of extrapolated transmision coefficient: transext.nrm */
    /* ****************************************************************** */
    printf("Reading data of experimental transmision coefficient: transext.nrm\n");
    fp = fopen("transext.nrm", "r");
    if(fp == NULL){
        printf("No transext.nrm file found in your directory\n");
        exit(0);
    }
    i = 0 ;
    while(i < ch_max){
        if(fgets(line,sizeof(line),fp) != NULL){sscanf(line,"%f", &transext[i]);}
        i++;
    }
    fclose(fp);
    
    /* ******************************************** */
    /* Reading spin cut-off parameters: spincut.cnt */
    /* ******************************************** */
    printf("Reading spin cut-off parameters: spincut.cnt\n");
    fp = fopen("spincut.cnt", "r");
    if(fp == NULL){
        printf("No spincut.cnt file found in your directory\n");
        exit(0);
    }
    i = 0 ;
    while(i < ch_max){
        if(fgets(line,sizeof(line),fp) != NULL){sscanf(line,"%f", &spincut[i]);}
        spincut[i] = 2. * spincut[i] * spincut[i];
        i++;
    }
    fclose(fp);
    
    /* ************************ */
    /* Printing input functions */
    /* ************************ */
    printf("\n No  Ex or Eg(keV)  Rho(1/MeV)      Transext  2*Spincut**2\n");
    for(i = 0 ; i < ch_max; i++){
        printf("%3d  %8.2f  %14.3e  %14.3e %6.2f \n",i,a0+a1*(float)i,rho[i],transext[i],spincut[i]);
    }
    
    /* ****************************************** */
    /* Asking for input values and initial levels */
    /* ****************************************** */
    printf("Neutron or proton binding energy (Bn or Bp) (MeV)  <%6.3f>:",Bn);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &Bn);
    Nch = 1 + (int)(((Bn*1000.-a0)/a1)+0.5);    /* Number of channels up to Bn or Bp, used for calculating D or <Gg> at Bn/Bp */
    
    printf("Decay within integer (0) or half-integer (1) spin       <%1d>:",eo);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%d", &eo);
    lowsp = 0;                                  /* Even-mass nucleus with integer spins */
    if(eo == 1) lowsp = 0.5;                    /* Odd-mass nucleus with half integer spins */
    printf("\nYou may decay with an initial distribution of all ");
    printf("\nspins/parities weighted with the spin distribution");
    printf("\ndetermined by the spin cutoff parameter, or you may");
    printf("\nspecify certain spin/parities with given weighting");
    printf("\n(in this case the spin/parity distribution will stay ");
    printf("\nfixed for all Ex).\n");
    printf("\nDecay with all initial spins/parities (0) or specify initial levels (1) <%1d>:",Iconf);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%d", &Iconf);
    
    /* ********************** */
    /* Full spin distribution */
    /* ********************** */
    if(Iconf == 0){
        printf("\nSUMMARY for excitation energy: %f keV\n", a0 + a1*(Nch-1));
        for (i = 0; i < ch_max; i++){
            Nsp = 0.;
            for (spin = lowsp; spin < lowsp + 20.; spin++){  // Applied for spin up to 20 hbar
                for (k = 0; k < 2; k++){      // Parity (0,1) = (+,-)
                    parity = 1 - 2*k;
                    Iweight2D[Nsp][i] = g(i,spin,parity);
                    Nsp = Nsp + 1;
                }
            }
            sum = 0.;
            for (sp = 0; sp < Nsp; sp++){
                sum = sum + Iweight2D[sp][i];
            }
            for (sp = 0; sp < Nsp; sp++){
                Iweight2D[sp][i] = Iweight2D[sp][i]/sum;
                if(i == Nch-1){
                    spin   = (int)(sp/2) + lowsp;
                    parity = -1;
                    if((int)(sp/2)*2 == sp)parity = +1;
                    printf("Configuration %2d has spin %4.1f, parity %2d and weight %6.4f\n",sp+1,spin,parity,Iweight2D[sp][i]);
                }
            }
        }
    }
    
    /* ************************* */
    /* Partial spin distribution */
    /* ************************* */
    if(Iconf == 1){
        printf("Number of initial configurations (e.g for 1+ and 1- levels, answer 2)   <%1d>:",Nconf);
        fgets_ignore(line,sizeof(line),stdin);
        sscanf(line,"%d", &Nconf);
        printf("\nYou will now be asked to write the spins/paritis and weights. ");
        printf("\nThe sum of weights are automatically normalized to unity.");
        printf("\nIn the case of 3/2+ and 5/2- states with weights 0.7 and 0.3, you may answer:");
        printf("\n1.5 ");
        printf("\n2.5 ");
        printf("\n  1 ");
        printf("\n -1 ");
        printf("\n0.5 ");
        printf("\n0.5\n");
        for (i = 0; i < Nconf; i++){
            printf("Configuration %2d has spin    <%4.1f>:",i+1,Ispin[i]);
            fgets_ignore(line,sizeof(line),stdin);
            sscanf(line,"%f", &Ispin[i]);
        }
        for (i = 0; i < Nconf; i++){
            printf("Configuration %2d has parity    <%2.0f>:",i+1,Iparity[i]);
            fgets_ignore(line,sizeof(line),stdin);
            sscanf(line,"%f", &Iparity[i]);
        }
        for (i = 0; i < Nconf; i++){
            printf("Configuration %2d has weight <%5.3f>:",i+1,Iweight[i]);
            fgets_ignore(line,sizeof(line),stdin);
            sscanf(line,"%f", &Iweight[i]);
        }
        sum = 0.;
        for (i = 0; i < Nconf; i++){
            sum = sum + Iweight[i];
        }
        printf("\nSUMMARY\n");
        for (i = 0; i < Nconf; i++){
            Iweight[i] = Iweight[i]/sum;
            printf("Configuration %2d has spin %4.1f, parity %2.0f and weight %6.4f\n",i+1,Ispin[i],Iparity[i],Iweight[i]);
        }
    }
    
    printf("\nYou may define decay to e.g. the ground state and");
    printf("\nthe first excited state by giving low and upper ");
    printf("\nexcitation energies for these states");
    
    printf("\nFirst state:\n");
    printf("Give spin of state                <%4.1f>:",I1);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &I1);
    printf("Give parity of state              <%4.1f>:",P1);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &P1);
    printf("Give lower excitation energy <%5.2f> MeV:",e1L);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &e1L);
    printf("Give upper excitation energy <%5.2f> MeV:",e1U);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &e1U);
    c1L = (int)(((1000.*e1L-a0)/a1)+0.5);
    c1U = (int)(((1000.*e1U-a0)/a1)+0.5);
    for(i= c1L; i <= c1U; i++){
        printf("\n ch = %4d Ex = %5.2f MeV and rho = %5.3e",i,(a0+a1*i)/1000.,rho[i]);
    }
    printf("\n");

    
    if(I1==0.0){
        printf("\nOMG, you have spin 0!");
        printf("\nThis state has probably only 1/3 of the level density of a full state.");
    }
    printf("\nWould you like to automatically adjust this state to 1 in the ");
    printf("\nexcitation region defined? Type 0 if no,");
    printf("\ntype 1. for one whole state, or 2. for two states etc.");
    printf("\nGive number of states (normaly, you should choose 1.0)  <%4.2f>:",zero1);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &zero1);

    if(zero1>0.){
        zero0 = 0.;
        for (i = c1L; i <= c1U; i++){
            zero0 = zero0 + (a1/1000.)*rho[i];
        }
        for (i = c1L; i <= c1U; i++){
            rho[i] = rho[i]*zero1/zero0;
        }
        printf("\nBefore number of states were %4.2f, now %4.2f",zero0,zero1);
    }
    
        
    printf("\n\nSecond state:\n");
    printf("Give spin of state                <%4.1f>:",I2);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &I2);
    printf("Give parity of state              <%4.1f>:",P2);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &P2);
    printf("Give lower excitation energy <%5.2f> MeV:",e2L);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &e2L);
    printf("Give upper excitation energy <%5.2f> MeV:",e2U);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &e2U);
    c2L = (int)(((1000.*e2L-a0)/a1)+0.5);
    c2U = (int)(((1000.*e2U-a0)/a1)+0.5);
    for(i= c2L; i <= c2U; i++){
        printf("\n ch = %4d Ex = %5.2f MeV and rho = %5.3e",i,(a0+a1*i)/1000.,rho[i]);
    }
    printf("\n");
    
    if(I2==0.0){
        printf("\nOMG, you have spin 0!");
        printf("\nThis state has probably only 1/3 of the level density of a full state.");
    }
        printf("\nWould you like to automatically adjust this state to 1 in the ");
        printf("\nexcitation region defined? Type 0 if no,");
        printf("\ntype 1. for one whole state, or 2. for two states etc.");
        printf("\nGive number of states (normaly, you should choose 1.0)  <%4.2f>:",zero2);
        fgets_ignore(line,sizeof(line),stdin);
        sscanf(line,"%f", &zero2);
    
    if(zero2>0.){
        zero0 = 0.;
        for (i = c2L; i <= c2U; i++){
            zero0 = zero0 + (a1/1000.)*rho[i];
        }
        for (i = c2L; i <= c2U; i++){
            rho[i] = rho[i]*zero2/zero0;
        }
        printf("Before number of states were %4.2f, now %4.2f\n",zero0,zero2);
    }
    
    printf("\nThird state:\n");
    printf("Give spin of state                <%4.1f>:",I3);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &I3);
    printf("Give parity of state              <%4.1f>:",P3);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &P3);
    printf("Give lower excitation energy <%5.2f> MeV:",e3L);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &e3L);
    printf("Give upper excitation energy <%5.2f> MeV:",e3U);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &e3U);
    c3L = (int)(((1000.*e3L-a0)/a1)+0.5);
    c3U = (int)(((1000.*e3U-a0)/a1)+0.5);
    for(i= c3L; i <= c3U; i++){
        printf("\n ch = %4d Ex = %5.2f MeV and rho = %5.3e",i,(a0+a1*i)/1000.,rho[i]);
    }
    printf("\n");
    
    if(I3==0.0){
        printf("\nOMG, you have spin 0!");
        printf("\nThis state has probably only 1/3 of the level density of a full state.");
    }
    printf("\nWould you like to automatically adjust this state to 1 in the ");
    printf("\nexcitation region defined? Type 0 if no,");
    printf("\ntype 1. for one whole state, or 2. for two states etc.");
    printf("\nGive number of states (normaly, you should choose 1.0)  <%4.2f>:",zero3);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &zero3);
    
    if(zero3>0.){
        zero0 = 0.;
        for (i = c3L; i <= c3U; i++){
            zero0 = zero0 + (a1/1000.)*rho[i];
        }
        for (i = c3L; i <= c3U; i++){
            rho[i] = rho[i]*zero3/zero0;
        }
        printf("\nBefore number of states were %4.2f, now %4.2f",zero0,zero3);
    }
    
    printf("\n\nFourth state:\n");
    printf("Give spin of state                <%4.1f>:",I4);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &I4);
    printf("Give parity of state              <%4.1f>:",P4);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &P4);
    printf("Give lower excitation energy <%5.2f> MeV:",e4L);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &e4L);
    printf("Give upper excitation energy <%5.2f> MeV:",e4U);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &e4U);
    c4L = (int)(((1000.*e4L-a0)/a1)+0.5);
    c4U = (int)(((1000.*e4U-a0)/a1)+0.5);
    for(i= c4L; i <= c4U; i++){
        printf("\n ch = %4d Ex = %5.2f MeV and rho = %5.3e",i,(a0+a1*i)/1000.,rho[i]);
    }
    printf("\n");
    
    if(I4==0.0){
        printf("\nOMG, you have spin 0!");
        printf("\nThis state has probably only 1/3 of the level density of a full state.");
    }
    printf("\nWould you like to automatically adjust this state to 1 in the ");
    printf("\nexcitation region defined? Type 0 if no,");
    printf("\ntype 1. for one whole state, or 2. for two states etc.");
    printf("\nGive number of states (normaly, you should choose 1.0)  <%4.2f>:",zero4);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &zero4);
    
    if(zero4>0.){
        zero0 = 0.;
        for (i = c4L; i <= c4U; i++){
            zero0 = zero0 + (a1/1000.)*rho[i];
        }
        for (i = c4L; i <= c4U; i++){
            rho[i] = rho[i]*zero4/zero0;
        }
        printf("Before number of states were %4.2f, now %4.2f\n",zero0,zero4);
    }

    
	 
    /* **************************************************** */
    /* Storing default values for the next run in input.gex */
    /* **************************************************** */
    fp = fopen("input.gex", "w");
    if(fp == NULL){
        printf("Could not open file input.gex \n");
        exit(0);
    } else {
        fprintf(fp, " %f %d %d %d\n", Bn, eo, Iconf, Nconf);
        fprintf(fp, " %d %d %d %d \n",x1L1, y1L1, x2L1, y2L1);
        fprintf(fp, " %d %d %d %d \n",x1U1, y1U1, x2U1, y2U1);
        fprintf(fp, " %d %d %d %d \n",x1L2, y1L2, x2L2, y2L2);
        fprintf(fp, " %d %d %d %d \n",x1U2, y1U2, x2U2, y2U2);
        fprintf(fp, " %f %f %f %f %f %f %f %f\n",e1L, e1U, e2L, e2U, e3L, e3U, e4L, e4U);
        fprintf(fp, " %f %f %f %f %f %f %f %f\n",I1,  P1,  I2,  P2,  I3,  P3,  I4,  P4);
        fprintf(fp, " %f %f %f %f \n",zero1, zero2, zero3, zero4);

        for (i = 0; i < Nconf; i++){
            fprintf(fp,"%f   %f  %f\n",Ispin[i],Iparity[i],Iweight[i]);
        }
    }
    fclose(fp);

    /* **************** */
    /* Fasten seatbelts */
    /* **************** */
    jH = ch_max-1;
    jL = 0;            //
    fastenseatbelts( jH,  jL);
    for (i = ch_max-1; i > jL; i--){
        gammex_tot[i] = gammex[i];
    }
    jH = c1U;
    jL = c1L;
    fastenseatbelts( jH,  jL);
    for (i = ch_max-1; i > jL; i--){
        gammex_1[i] = gammex[i];
    }
    jH = c2U;
    jL = c2L;
    fastenseatbelts( jH,  jL);
    for (i = ch_max-1; i > jL; i--){
        gammex_2[i] = gammex[i];
    }

    /* ************************************** */
    /* Gamma-width <G_Eg(Ex)> written to disk */
    /* ************************************** */
    fp = fopen("gammex.gex", "w");
    if(fp == NULL){
        printf("Could not open file gammex.gex \n");
        exit(0);
    }
    else {
        for (i = 0; i < ch_max; i++){
            fprintf(fp, " %8.2f  %10.3e %10.3e %10.3e \n", a0+a1*(float)i, gammex_tot[i]*1.e+9, gammex_1[i]*1.e+9, gammex_2[i]*1.e+9);
            printf(" %3d  Ex(keV) = %8.2f   Gamma-width (meV) = %10.3e %10.3e %10.3e \n",i, a0+a1*(float)i, gammex_tot[i]*1.e+9, gammex_1[i]*1.e+9, gammex_2[i]*1.e+9);
        }
    }
    fclose(fp);	
    printf("File gammex.gex (0:%d) written to disk, (a0,a1)=(%f,%f)\n",ch_max-1,a0,a1);
    
    /* ***************** */
    /* Calculate D at Bn */
    /* ***************** */
    if(Iconf == 1){
        sum = 0.;
        for (conf = 0; conf < Nconf; conf++){
            spin = Ispin[conf];
            parity = Iparity[conf];
            sum = sum + rho[Nch-1]*g(Nch-1,spin,parity);
        }
        xa = a0+a1*(float)(Nch-1);
        ya = sum;
        
        sum = 0.;
        for (conf = 0; conf < Nconf; conf++){
            spin = Ispin[conf];
            parity = Iparity[conf];
            sum = sum + rho[Nch-2]*g(Nch-2,spin,parity);
        }
        xb = a0+a1*(float)(Nch-2);
        yb = sum;
    
        x = Bn*1000.;
        y = ya + ((yb-ya)/(xb-xa))*(x-xa);
        D = 0;
        if(y > 0.)D = 1000./y; //in keV
        printf("\nAverage level spacing is D  = %12.6f keV at Ex = %8.1f keV",D, x);
    }
    
    /* ******************** */
    /* Calculate <Gg> at Bn */
    /* ******************** */
    xa = a0+a1*(float)(Nch-1);
    xb = a0+a1*(float)(Nch-2);
    ya = gammex_tot[Nch-1]*1.e+9;
    yb = gammex_tot[Nch-2]*1.e+9;
    x = Bn*1000.;
    y = ya + ((yb-ya)/(xb-xa))*(x-xa);
    printf("\nAverage gamma width is <Gg> = %12.3f meV at Ex = %8.1f keV\n",y, x);
    printf(  "Average lifetime is   <Tau> = %12.3f fs  at Ex = %8.1f keV\n",658.2/y, x);

    return(0);
}

void fastenseatbelts(int jH, int jL){
    float x = 0.,ww = 0.;
    int IPconf;
    for (i = ch_max-1; i > 0; i--){
        gammex[i] = 0.;
    }

    /* ********************** */
    /* Full spin distribution */
    /* ********************** */
    if(Iconf == 0){
        for (i = ch_max-1; i > jL; i--){  // i is initial and j (jH-jL) is final excitation energy, ch_max is highest Ex
            IPconf = 0;
            sum = 0.;
            for (spin = lowsp; spin < lowsp + 20.; spin++){  // Applied for spin up to 20 hbar
                for (k = 0; k < 2; k++){      // Parity (0,1) = (+,-)
                    parity = 1 - 2*k;
                    if( rho[i]*g(i,spin,parity) > 0.){                        // Averaging made by weighting with g(j,spin,parity)
                        c = (a1/1000.) / (4.*PI0*rho[i]*g(i,spin,parity));
                    }else{
                        c = 0.;
                    }
                    ww  = Iweight2D[IPconf][i];
                    for (j = MIN(jH,i); j >= jL; j--){   //Looping over final states (j) from initial state (i)
                        x = ww * c * partialsum(i,j,spin,parity);
                        sum = sum + x;
                    }
                    IPconf = IPconf + 1;
                }
            }
            gammex[i] = sum;
        }
    }

    /* ************************* */
    /* Partial spin distribution */
    /* ************************* */
    if(Iconf == 1){
        for (i = ch_max; i > jL; i--){  // i is initial and j (jH-jL) is final excitation energy, ch_max is highest Ex
            sum = 0.;
            for (conf = 0; conf < Nconf; conf++){
                spin = Ispin[conf];
                parity = Iparity[conf];
                if( rho[i]*g(i,spin,parity) > 0.){       // Averageing made by weighting with g(j,spin,parity)
                    c = (a1/1000.) / (4.*PI0*rho[i]*g(i,spin,parity));
                }else{
                    c = 0.;
                }
                ww = Iweight[conf];
                for (j = MIN(jH,i); j >= jL; j--){   //Looping over final states (j) from initial state (i)
                    x = ww * c * partialsum(i,j,spin,parity);
                    sum = sum + x;
                }
            }
            gammex[i] = sum;
        }
    }
}
    
double partialsum(int ji, int jf, float Ii, int Pi)
{
    double xi = 0., xus, xuo, xms, xmo, xds, xdo;
    double TM1, TE1;
    int ps, po, jg;
    
    if(Pi == 1){
        ps = +1;			                /* The initial parity is positive */
        po = -1;
    }else{
        ps = -1;							/* The initial parity is negative */
        po = +1;
    }
    jg = (int)(ji-jf-(a0/a1)+0.5);          /* The array index of gamma energy*/
    TM1 = transext[jg];				        /* For future expension */
    TE1 = transext[jg];
                                            /* Following formulas require parity symmetry */
    xus = TM1 * rho[jf] * g(jf,Ii+1,ps);   	/* One spin up, same parity */
    xuo = TE1 * rho[jf] * g(jf,Ii+1,po);   	/* One spin up, opposite parity */
    xms = TM1 * rho[jf] * g(jf,Ii+0,ps);   	/* Same spin, same parity */
    xmo = TE1 * rho[jf] * g(jf,Ii+0,po);   	/* Same spin, opposite parity */
    xds = TM1 * rho[jf] * g(jf,Ii-1,ps);   	/* One spin down, same parity */
    xdo = TE1 * rho[jf] * g(jf,Ii-1,po);   	/* One spin down, opposite parity */
    if(Ii == 0){
        xms = 0.; // 0+ -> 0+ forbidden
        xmo = 0.; // 0+ -> 0- forbidden
    }
    xi = xus + xuo + xms + xmo + xds + xdo;
//    if(ji == 50 && jf == 40) printf(" %3d %3d %3d %5.1f %6.1f %6.5f %5.1e %5.1e %5.1e %5.1e %5.1e %5.1e %5.1e\n",ji,jf,jg,Ii,rho[jf],g(jf,Ii+0,po),xi,xus,xuo,xms,xmo,xds,xdo);
    return xi;
}

double g(int jexc, float jspin, int jparity)
{
    double wparity, xx, xj = 0;
    if(jparity == +1) wparity = 0.5;
    if(jparity == -1) wparity = 0.5;
    if(jspin < 0){
        return xj;
    }
    //Hard-wiring direct decay to e.g. 0+, 2+, 4+ and 6+ states
    if(jexc >= c1L && jexc <= c1U){
        if(jspin == I1 && jparity == P1){
            xj = 1.;
            return xj;
        }else{
        xj = 0.;
        return xj;
        }
    }
    if(jexc >= c2L && jexc <= c2U){
        if(jspin == I2 && jparity == P2){
            xj = 1.;
            return xj;
        }else{
        xj = 0.;
        return xj;
        }
    }
    if(jexc >= c3L && jexc <= c3U){
        if(jspin == I3 && jparity == P3){
            xj = 1.;
            return xj;
        }else{
            xj = 0.;
            return xj;
        }
    }
    if(jexc >= c4L && jexc <= c4U){
        if(jspin == I4 && jparity == P4){
            xj = 1.;
            return xj;
        }else{
            xj = 0.;
            return xj;
        }
    }
    
    xx = (jspin+0.5)*(jspin+0.5)/spincut[jexc];
    if( xx <= 50 && jspin >= 0){
        xj = wparity*((2.*jspin+1.)/spincut[jexc])*exp(-xx);
    }
    if(gsum(jexc) > 0.)xj = xj/gsum(jexc);
    return xj;
}

double gsum(int jexc)
{
    float sp;
    double xx, xj = 0.;
    for( sp = lowsp; sp < lowsp + 40.; sp++){
        if(spincut[jexc]>0)xx = (sp+0.5)*(sp+0.5)/spincut[jexc];
        if( xx <= 30 && sp >= 0){
            xj = xj + ((2.*sp+1.)/spincut[jexc])*exp(-xx);
        }else{
 //           printf(" warning, xx = %e and spinn =%e ",xx,sp);
        }
    }
    return xj;
}
