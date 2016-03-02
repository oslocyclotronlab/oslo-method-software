#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>    
#include <math.h>    
#include <fcntl.h>					/* Header for open/read/write */ 
#include <errno.h>
#include <time.h>					/* Header for system time */
#include <sys/types.h>          
#include <sys/ioctl.h>
#define MAXLEV        10000			/* Max number of known levels */
#define MAXDIM        8192 			/* Max dimension of Ex or Eg spectra */
#define MAXENERGY     100.0			/* Max energy Ex or Eg (MeV)  */

char  line[1024];
char  cdum[128];
float rho[MAXDIM], drho[MAXDIM], levenergy[MAXLEV],rholev[MAXDIM];
float sig[MAXDIM], dsig[MAXDIM], nsig[MAXDIM], ndsig[MAXDIM], nsigL[MAXDIM], nsigH[MAXDIM];
int   dimRhox, dimRhoy, dimSigx, dimSigy, diml, dim, H, sigdim, dimmax;
float a0, a1, emin, emax, ex, eg;
float Anorm=1., alpha=0.;                 /* Normalization constants */
float eps = 1.e-20, x;
float eps_0 = 0.000;
float c1, c2, e1, e2;
int   Lm, Hm;
FILE  *fp;
int   i,j,l;
/* Defining defaults values, taken from 162Dy */
float Amass = 164., Delta = 1.847, Bn = 8.197, rho0 = 3630000., drho0 = 363000.;
int   L1 = 10, L2 = 15, H1 = 49, H2 = 56;
int   TL1 = 10, TL2 = 15, TH1 = 49, TH2 = 56, Tfirst = 10, Tlast = 56;
int   isig = 1, itemp = 2, imodel = 1, ansL=0, ansH=0;
int   i0, i1, i2, FGmax;
float a, T, E1, E0, C1 = -1.296, eta, rhox, sig2, spcu; /* C1 not used anymore */
float aRobin = 16.824, E1Robin = 0.505;
float TRobin = 0.5, E0Robin = 0.0;
float abestL = -1000.,bbestL = -1000.,abestH = -1000.,bbestH = -1000.;

int   searchAalpha();
int   makeroot1();
int   makeroot2();
int   makeroot3();
void  rhofg(float Amass, float ex, float a, float T, float E1, float E0, int isig, int itemp, int imodel);
float corrL();
float corrH();
int   extendL();
int   extendH();

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
    printf(" |                    C O U N T I N G   1.7.4                   |\r\n");
    printf(" |                                                              |\r\n");
    printf(" |Program to normalize experimental nuclear level density (NLD) |\r\n");
    printf(" |  to NLD from known low energy levels and NLD extracted from  |\r\n");
    printf(" |     resonances spacing data at Bn. Thus, the A and alpha     |\r\n");
    printf(" |     parameters are determined for NLD. The slope of the      |\r\n");
    printf(" |   transmision coefficient T(Eg) is normalized according to   |\r\n");
    printf(" |                    the alpha parameter.                      |\r\n");
    printf(" |                                                              |\r\n");
    printf(" |  Input files:  counting.dat    Output files: rhopaw.cnt      |\r\n");
    printf(" |                rhosp.rsg                     rhotmopaw.cnt   |\r\n");
    printf(" |                sigsp.rsg                     sigpaw.cnt      |\r\n");
    printf(" |                rhopaw.rsg                    input.cnt       |\r\n");
    printf(" |                sigpaw.rsg                    spincut.cnt     |\r\n");
    printf(" |               (input.cnt)                    fermigas.cnt    |\r\n");
    printf(" |                                              efit.f          |\r\n");
    printf(" |                                              counting.cpp    |\r\n");
    printf(" |                                              spincut.cpp     |\r\n");
    printf(" |                                              sigext.cpp      |\r\n");
    printf(" |                                                              |\r\n");
    printf(" |  Fermi gas or constant temperatur parameteres are calculated |\r\n");
    printf(" |      from Egidy and Bucurescu: PRC 80, 054310 (2009)         |\r\n");
    printf(" | E-mail  : magne.guttormsen@fys.uio.no                        |\r\n");
    printf(" | Created : 11 Aug 2006                                        |\r\n");
    printf(" | Modified: 17 Jun 2009                                        |\r\n");
    printf(" | Modified: 16 Nov 2014 Ex < 0 MeV, CT output, CT with sig_FG  |\r\n");
    printf(" | Modified: 10 Feb 2015 dimRhox, dimSigx                       |\r\n");
    printf(" | Modified: 01 Jun 2015 allow modifying ExL and ExH of sigext  |\r\n");
    printf(" | Modified: 28 Aug 2015 ? replaced by \" for root scripts      |\r\n");
    printf(" | and deleting kumac output files                              |\r\n");
    printf(" | Modified: 15 Feb 2016 Cosmetics                              |\r\n");
    printf(" |______________________________________________________________|\r\n");
    printf("                                                                 \r\n");
	
	/*	**************************	*/
	/*	MODIFICATIONS OF CECILIE	*/
	/*	08 JAN 2010					*/
	/*	1) dim (n-1) in TGraph		*/
	/*	2) eps == 0 for rhopaw and	*/
	/*	   sigpaw					*/
	/*	**************************	*/
	
	
    /* *********************** */
    /* Writing The Oslo Method */
    /* *********************** */
    fp = fopen("README_Oslo.cnt", "w+");
    if(fp == NULL){
        printf("\nCould not open file README_Oslo.cnt \n");
    }
    else {
        fprintf(fp, " The Oslo-method\n \n");
        fprintf(fp, " 1. Sort ALFNA(Eg,Ex) from experiment \n");
        fprintf(fp, " 2. Use rm in mama and unfold ALFNA -> unfolded \n");
        fprintf(fp, " 3. Use fg in mama and extract first-generation matrix fg \n");
        fprintf(fp, " 4. Run rhosigchi on fg \n");
        fprintf(fp, " 5. Run counting \n");
        fprintf(fp, " 6. Run run counting.cpp, spincut.cpp and sigext.cpp in ROOT\n");
        fprintf(fp, " 7. Run normalization \n");
        fprintf(fp, " 8. Run strength.cpp in ROOT");
        fclose(fp);
    }

    /* ************************************************************* */
    /* Reading calibration and dimensions from: rhosp.rsg            */
    /* ************************************************************* */
    printf("Reading calibration and dimensions from: rhosp.rsg\n");
    fp = fopen("rhosp.rsg", "r");
    if(fp == NULL){
        printf("No rhosp.rsg file found in your directory\n");
        exit(0);
    }
    else {
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
        sscanf(line,"%s %d %s %d",cdum, &dimRhox, cdum, &dimRhoy);
        fgets_ignore(line,sizeof(line),fp);
        fclose(fp);
    }
    printf("rhosp.rsg has dimension (0 : %d, 0 : %d) and calibration (a0, a1) = (%f, %f)\n",dimRhox,dimRhoy,a0,a1);
    
    /* ************************************************************* */
    /* Reading calibration and dimensions from: sigsp.rsg            */
    /* ************************************************************* */
    printf("Reading calibration and dimensions from: sigsp.rsg\n");
    fp = fopen("sigsp.rsg", "r");
    if(fp == NULL){
        printf("No sigsp.rsg file found in your directory\n");
        exit(0);
    }
    else {
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
        sscanf(line,"%s %d %s %d",cdum, &dimSigx, cdum, &dimSigy);
        fgets_ignore(line,sizeof(line),fp);
        fclose(fp);
    }
    printf("sigsp.rsg has dimension (0 : %d, 0 : %d) and calibration (a0, a1) = (%f, %f)\n",dimSigx,dimSigy,a0,a1);
	
    /* ************************************************************************* */
    /* Reading data and errors of experimental nuclear level density: rhopaw.rsg */
    /* ************************************************************************* */
    printf("\nReading data and errors of experimental nuclear level density: rhopaw.rsg\n");
    fp = fopen("rhopaw.rsg", "r");
    if(fp == NULL){
        printf("No rhopaw.rsg file found in your directory\n");
        exit(0);
    }
    l = 0 ;
    while( l <= dimRhox){
        if(fgets(line,sizeof(line),fp) != NULL){sscanf(line,"%f", &rho[l]);}
        l++; 
    }
    l = 0 ;
    while(l <= dimRhox){
        if(fgets(line,sizeof(line),fp) != NULL){sscanf(line,"%f", &drho[l]);}
        l++; 
    }
    fclose(fp);
    printf(" No   Ex(keV)      Rho(1/MeV)     dRho(1/MeV)\n");
    for(i = 0 ; i <= dimRhox; i++){
        if(rho[i] < eps)rho[i] = eps_0;
        if(rho[i] < eps)drho[i] = eps_0;
        if(drho[i] < eps)drho[i] = eps_0;
        printf("%3d  %8.2f  %14.3e  %14.3e \n",i,a0+a1*(float)i,rho[i],drho[i]);
    }
	
    /* *************************************************************************** */
    /* Reading data and errors of experimental transmision coefficient: sigpaw.rsg */
    /* *************************************************************************** */
    printf("\nReading data and errors of experimental transmision coefficient: sigpaw.rsg\n");
    fp = fopen("sigpaw.rsg", "r");
    if(fp == NULL){
        printf("No sigpaw.rsg file found in your directory\n");
        exit(0);
    }
    l = 0 ;
    while( l <= dimSigx){
        if(fgets(line,sizeof(line),fp) != NULL){sscanf(line,"%f", &sig[l]);}
        l++; 
    }
    l = 0 ;
    while( l <= dimSigx){
        if(fgets(line,sizeof(line),fp) != NULL){sscanf(line,"%f", &dsig[l]);}
        l++; 
    }
    fclose(fp);
    printf(" No   Eg(keV)         Trans           dTrans\n");
    for(i = 0 ; i <= dimSigx; i++){
        if(sig[i] < eps)sig[i] = eps_0;
        if(sig[i] < eps)dsig[i] = eps_0;
        if(dsig[i] < eps)dsig[i] = eps_0;
        printf("%3d  %8.2f  %14.3e  %14.3e \n",i,a0+a1*(float)i,sig[i],dsig[i]);
    }
	
    /* *************************************** */
    /* Finding common upper limits for spectra */
    /* *************************************** */
    H = 0;
    dim = 0;
    dimmax=dimRhox;
    if(dimmax < dimSigx)dimmax=dimSigx;
    l = (int)fabs(a0/a1);
    for (i = l; i <= dimmax; i++){
        if(rho[i]==0 && drho[i]==0 && H == 0) H = i;	
        if(rho[i]==0 && drho[i]==0 && sig[i]==0 && dsig[i]==0 && dim == 0)dim = i;	
    }
    if(H == 0) H = dimmax;
    if(dim == 0) dim = dimmax;
	
    /* ********************************************************* */
    /* Reading excitation energies of known levels: counting.dat */
    /* ********************************************************* */
    printf("\nReading excitation energies of known levels: counting.dat\n");
    fp = fopen("counting.dat", "r");
    if(fp == NULL){
        printf("No counting.dat file found in your directory\n");
        exit(0);
    }
    i = 0;
    l = 0 ;
    while( i <= MAXLEV){
        if(fgets(line,sizeof(line),fp) != NULL){
            sscanf(line,"%f", &levenergy[l]);
            l++;
        }
	i++;
    }
    diml = l-1;
    fclose(fp);
    printf("Binning %d known levels:\n",diml);
    printf(" No   Ex(keV)   NoLev   RhoLeV(1/MeV)\n");
    for(i = 0 ; i <= dim; i++){
        emin=a0+a1*((float)i - 0.5);
        emax=a0+a1*((float)i + 0.5);
        for(j = 0 ; j < diml; j++){
            if(levenergy[j]>=emin && levenergy[j]<emax){
                rholev[i]++;
            }
        }
        l = rholev[i];
        rholev[i]=rholev[i]/(fabs(a1/1000.));
        printf("%3d  %8.2f  %6d  %14.3e\n",i,a0+a1*(float)i,l,rholev[i]);
    }
	
    /* ***************************************************************************** */
    /* Trying to estimate reasonable H1, H2 limits for fitting around Bn or Bp       */
    /* Taking the 1/6 part of the upper energy data points, having reasonable errors */
    /* ***************************************************************************** */
    for(i = dim; i > 5; i--){
        if (rho[i] > 10. && drho[i] > 10 && rho[i] > 1.2*drho[i]){
            H2 = i;
            H1 = H2 - (int)(((float)H2/6.)+0.5);
            break;
        }
    }
	
    /* ***************************************************************************** */
    /* Trying to estimate reasonable L1, L2 limits for fitting around Ex=0.5-1.5 MeV */
    /* Taking data points within 1 MeV, and test that there are no 0 points          */
    /* ***************************************************************************** */
    i1 = (int)(((500. - a0)/a1)+ 0.5);
    i2 = (int)((1000./a1)+0.5);
    if(i1 < 0) i1 = 1;
    if(i2 < 2) i2 = 2;
    i0 = i1;
    for(i = i1; i <= i1 + i2; i++){ /*Searching for zeros*/
        if(rholev[i] <= 0) i0 = i;
    }
    L1 = i0+1;
    L2 = L1 + i2;
    if(L2 > H1 -2) L2 = H1 - 2;
	
    /* ***************************************************************************** */
    /* Trying to estimate reasonable H1, H2 limits for fitting high T(Eg)            */
    /* Taking the 1/6 part of the upper energy data points, having reasonable errors */
    /* ***************************************************************************** */
    for(i = dim; i > dim/2; i--){
        if (sig[i] > 0. && dsig[i] > 0. && sig[i] > 1.2*dsig[i]){
            TH2 = i;
            TH1 = TH2 - (int)(((float)TH2/6.)+0.5);
            break;
        }
    }
    for(i = dim; i > dim/2; i--){
        if (sig[i] > 0. && dsig[i] > 0.){
            Tlast = i;
            break;
        }
    }
    /* ***************************************************************************** */
    /* Trying to estimate reasonable L1, L2 limits for fitting low T(Eg)             */
    /* Taking data points within 1 MeV, and test that there are no 0 points          */
    /* ***************************************************************************** */
	
    for(i = 0; i < dim/2; i++){
        if (sig[i] > 0. && dsig[i] > 0. && sig[i] > 1.2*dsig[i]){
            i1 = i;
            break;
        }
    }
    for(i = 0; i < dim/2; i++){
        if (sig[i] > 0. && dsig[i] > 0.){
            Tfirst = i;
            break;
        }
    }
	
    if(i1 > dim/2) i1 = dim/2;
    i2 = i1 + (int)((1000./a1)+0.5);
    if(i1 < 0) i1 = 1;
    if(i2 < 2) i2 = 2;
    i0 = i1;
    for(i = i1; i <= i1 + i2; i++){ /*Searching for zeros*/
        if(sig[i] <= 0) i0 = i;
    }
    TL1 = i0+1;
    TL2 = TL1 + (i2-i1);
    if(TL2 > TH1 -2) TL2 = TH1 - 2;
	
    /* ***************************************** */
    /* Reading default values from previous runs */
    /* ***************************************** */
    fp = fopen("input.cnt", "r");
    if(fp == NULL){
        printf("\nCould not open file input.cnt, default values are used \n");
    }
    else {
        fgets_ignore(line,sizeof(line),fp);
        sscanf(line, " %f %f %f %f %f \n", &Amass, &Delta, &Bn, &rho0, &drho0);
        fgets_ignore(line,sizeof(line),fp);
        sscanf(line, " %d %d %d %d \n", &L1, &L2, &H1, &H2);
        fgets_ignore(line,sizeof(line),fp);
        sscanf(line, " %d %d %d %d \n", &TL1, &TL2, &TH1, &TH2);
        fgets_ignore(line,sizeof(line),fp);
        sscanf(line, " %d %f %f \n", &isig, &aRobin, &E1Robin);
        fgets_ignore(line,sizeof(line),fp);
        sscanf(line, " %d %f %f \n", &itemp, &TRobin, &E0Robin);
        fgets_ignore(line,sizeof(line),fp);
        sscanf(line, " %d  \n", &imodel);
        fgets_ignore(line,sizeof(line),fp);
        sscanf(line, " %d %f %f \n", &ansL, &abestL, &bbestL);
        fgets_ignore(line,sizeof(line),fp);
        sscanf(line, " %d %f %f \n", &ansH, &abestH, &bbestH);
        fclose(fp);
    }
    // For the first time running
    if(abestL==-1000.)ansL = 0;
    if(abestH==-1000.)ansH = 0;
    
    /* *********************** */
    /* Asking for input values */
    /* *********************** */
    
    printf("Mass number A             <%3d>:",(int)Amass);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &Amass);
	
    printf("Neutron or proton binding energy (Bn or Bp) (MeV)  <%6.3f>:",Bn);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &Bn);
    if(Bn > 100){
        printf("\nToo big number");
        printf("\nCome back another day\n\n");
        return(0);
    }
    printf("\n");
    printf("\nChoose constant temperature CT (1) or Fermi gas FG (2) formula <%1d>:",imodel);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%d", &imodel);
    if(imodel < 1 || imodel > 2) exit(0);
    
    printf("\n");
    printf("\nYou should take care to use formulas from the same reference.");
    printf("\nExample: If you use rigid moment of inertia from (E&B2006),");
    printf("\nyou should also use temperature T from (E&B2006), as well.");
    printf("\nWe recommend to use (E&B2006) (or maybe the old (G&C)) for nuclei heavier than A > 150,");
    printf("\nand (E&B2009) for lighter nuclei with A < 150.");
    printf("\n");
    printf("\nIf you have chosen the CT formula, we recommend to use FG spin cut-off formula since");
    printf("\nthe CT spin cut-off formula is a constant, which is rather unphysical. In the following,");
    printf("\noption (1) might be the most appropriate for the CT-model (instead of (3)).");
    printf("\n");
    printf("\nYou may choose between 4 spin cut-off formulas:");
    printf("\n1 The rigid moment of inertia formula (RMI) (E&B2006)");
    printf("\n2 The Gilbert and Cameron formula (G&C) Can. J. Phys 43(1965) 1446");
    printf("\n3 The constant temperature (CT) formula (E&B2009) and NPA 481 (1988) 189");
    printf("\n4 The Fermi gas formula with appropriate cut-off parameter (E&B2009)");
           
    printf("\nType 1 for RMI: sig**2=0.0146*(A**(5/3))*T for FG+CT (E&B2006)");
    printf("\nType 2 for G&C: sig**2=0.0888*(A**(2/3))*a*T for FG+CT");
    printf("\nType 3 for E&B: sig**2=(0.98*(A**(0.29)))**2 for CT");
    printf("\nType 4 for E&B: sig**2=0.391*A**0.675*(E-0.5*Pa_prime)**0.312 for FG+CT");
    printf("\n");
    printf("\n\nChoose RMI(FG+CT) (1), G&C(FG+CT) (2), E&B(CT) (3) or E&B(FG+CT) (4) <%1d>:",isig);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%d", &isig);
    if(isig < 1 || isig > 4) exit(0);
    
    if(isig == 1 || isig == 2){
        itemp = 1;
        if(isig==1)itemp=2;
        printf("\nYou may choose between 2 temperature formulas:");
        printf("\n1 The common fermigas formula (CFG)");
        printf("\n2 The somewhat more advanced fermigas formula (AFG)");
        printf("\nType 1 for CFG: T = SQRT(U/a)(G&C)");
        printf("\nType 2 for AFG: T = (1+SQRT(1+4*a*U))/(2*a) (E&B2006)");
        printf("\nChoose CFG(1) or AFG (2) <%1d>:",itemp);
        fgets_ignore(line,sizeof(line),stdin);
        sscanf(line,"%d", &itemp);
        if(itemp < 1 || itemp > 2) exit(0);
    }
	
    printf("\nBe sure to use the correct Rho(Bn or Bp) according\n");
    printf("to type 1, 2, 3 or 4. Run the d2rho program to find Rho\n");
    printf("or use the systematic value found by running Robin\n");
    printf("Level density at Bn or Bp (1/MeV)                <%8.0f>:",rho0);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &rho0);
    printf("Uncertainty of level density at Bn or Bp (1/MeV) <%8.0f>:",drho0);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &drho0);
	
    /* *********************************************************************** */
    /* Asking or calculating Fermi-gas parameters according to case (1) or (2) */
    /* *********************************************************************** */
     if(isig == 1||isig == 2||isig == 4||imodel == 2){
        printf("\nYou need to run the program Robin to get the Fermi-gas parameters a and E1:");
        printf("\nLevel density parameters a (1/MeV) <%7.3f>:",aRobin);
        fgets_ignore(line,sizeof(line),stdin);
        sscanf(line,"%f", &aRobin);
        printf("Fermi-gas shift parameter E1 (MeV) <%7.3f>:",E1Robin);
        fgets_ignore(line,sizeof(line),stdin);
        sscanf(line,"%f", &E1Robin);
        a  = aRobin;
        E1 = E1Robin;
    }
    if(imodel == 1){
        printf("\nYou need to run the program Robin to get the constant temperature parameters T:");
        printf("\nTemperature parameter T (MeV) <%7.3f>:",TRobin);
        fgets_ignore(line,sizeof(line),stdin);
        sscanf(line,"%f", &TRobin);
        E0Robin = Bn - TRobin*(log(rho0)+log(TRobin));
        printf("\nThe level density goes through Rho(Bn), thus determining the");
        printf("\nconst. temp. shift parameter to be E0 = %7.3f MeV\n",E0Robin);
        T  = TRobin;
        E0 = E0Robin;
    }
    
	
    /* *********************************************/
    /* Asking for fit limits L1, L2, H1, H2 for Rho*/
    /* *********************************************/
    i1 = 0;
    i2 = L2 + (int)((1000./a1)+0.5);
    printf("\n No   Ex(keV)   RhoLeV(1/MeV)\n");
    for(i = i1 ; i <= i2; i++){
        if(i == L1){
            printf("-----------------------------\n");
        }
        printf("%3d  %8.2f  %14.3e\n",i,a0+a1*(float)i,rholev[i]);
        if(i == L2){  
            printf("-----------------------------\n");
        }
    }
    printf("Lower fit limit L1 for known levels  <%3d>:",L1);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%d", &L1);
    printf("Higher fit limit L2 for known levels <%3d>:",L2);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%d", &L2);
	
    i1 = H1 - (int)((1000./a1)+0.5);
    i2 = H2 + (int)((1000./a1)+0.5);
    if(i1 < L2)   i1 = L2;
    if(i2 > dim) i2 = dim;
    printf("\n No   Ex(keV)      Rho(1/MeV)     dRho(1/MeV)\n");
    for(i = i1 ;i <= i2; i++){
        if(i == H1){
            printf("---------------------------------------------\n");
        }
        printf("%3d  %8.2f  %14.3e  %14.3e \n",i,a0+a1*(float)i,rho[i],drho[i]);
        if(i == H2){  
            printf("---------------------------------------------\n");
        }
    }
	
    printf("Lower fit limit H1 for Rho around Bn or Bp  <%3d>:",H1);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%d", &H1);
    printf("Higher fit limit H2 for Rho around Bn or Bp <%3d>:",H2);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%d", &H2);
	
    /* ********************************************/
    /* Asking for fit limits L1, L2, H1, H2 for T */
    /* ********************************************/
    i1 = 0;
    i2 = TL2 + (int)((1000./a1)+0.5);
    printf("\n No   Eg(keV)         Trans           dTrans\n");
    for(i = i1 ; i <= i2; i++){
        if(i == TL1){
            printf("---------------------------------------------\n");
        }
        printf("%3d  %8.2f  %14.3e  %14.3e \n",i,a0+a1*(float)i,sig[i],dsig[i]);
        if(i == TL2){  
            printf("---------------------------------------------\n");
        }
    }
    printf("Lower fit limit L1 for low energy region of T(Eg)  <%3d>:",TL1);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%d", &TL1);
    printf("Higher fit limit L2 for low energy region of T(Eg) <%3d>:",TL2);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%d", &TL2);
	
    i1 = TH1 - (int)((1000./a1)+0.5);
    i2 = TH2 + (int)((1000./a1)+0.5);
    if(i1 < TL2)   i1 = TL2;
    if(i2 > dim) i2 = dim;
    printf("\n No   Eg(keV)         Trans           dTrans\n");
    for(i = i1 ;i <= i2; i++){
        if(i == TH1){
            printf("---------------------------------------------\n");
        }
        printf("%3d  %8.2f  %14.3e  %14.3e \n",i,a0+a1*(float)i,sig[i],dsig[i]);
        if(i == TH2){  
            printf("---------------------------------------------\n");
        }
    }
	
    printf("Lower fit limit H1 for high energy region of T(Eg)  <%3d>:",TH1);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%d", &TH1);
    printf("Higher fit limit H2 for high energy region of T(Eg) <%3d>:",TH2);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%d", &TH2);
	
    /* ************************************************************* */
    /* All inputs are now in place                                   */
    /* Now calculating the Fermi gas formula for the case (1) or (2) */
    /* And determine the eta-parameter to match Rho at Bn or Bp      */
    /* ************************************************************* */
    
    rhofg(Amass, Bn, a, T, E1, E0, isig, itemp, imodel);
    eta = rho0/rhox;
    if(isig==3){
        printf("\nTemperature parameter T (MeV) = %7.3f",TRobin);
        printf("\nCT shift parameter is E0(MeV) = %7.3f",E0Robin);
        printf("\nCT level density has been multiplied with eta = %6.3f\n", eta);
    }
    else{
        printf("\nFermi-gas level density has been multiplied with eta = %6.3f\n", eta);
    }
    printf("in order to match Rho(Bn or Bp) = %8.1f (1/MeV)\n", rho0);
    rhofg(Amass, Bn, a, T, E1, E0, isig, itemp, imodel);
	spcu = sqrt(sig2);
    printf("\nSpin cut-off parameter used at Bn or Bp = %6.3f\n", spcu);


    /* **************************** */
    /* Searching the normalization  */
    /* parameters Anorm and alpha   */
    /* **************************** */
    searchAalpha();
	
    /* ******************************************************* */
    /* Normalization parameters Anorm and alpha are determined */
    /* ******************************************************* */
	
    /* ************************************************************* */
    /* Writing Fermi-gas level density to high excitation energy     */
    /* ************************************************************* */
    FGmax = (int)(100./(a1/1000.));
    fp = fopen("fermigas.cnt", "w+");
    if(fp == NULL){
        printf("Could not open file fermigas.cnt \n");
        exit(0);
    }
    else {
        for (i = 0; i <= FGmax; i++){
            ex = (a0+a1*(float)i)/1000.;
            rhofg(Amass, ex, a, T, E1, E0, isig, itemp, imodel);
            fprintf(fp, " %14.7e \n", eta*rhox);
        }
    }
    fclose(fp);
    printf("\nFile fermigas.cnt (0:%d) written to disk, (a0,a1)=(%8.2f,%8.3f)\n",i2,a0,a1);

    /* ************************************************************* */
    /* Writing level density to high excitation energy. Above H2     */
    /* the Fermi gas expression is used.                             */
    /* ************************************************************* */
    fp = fopen("rhotmopaw.cnt", "w+");
    if(fp == NULL){
        printf("Could not open file rhotmopaw.cnt \n");
        exit(0);
    }
    else {
        for (i = 0; i <= FGmax; i++){
            ex = (a0+a1*(float)i)/1000.;
            if(i <= H2) fprintf(fp, " %14.7e \n", Anorm*exp(alpha*ex)*rho[i]);
            rhofg(Amass, ex, a, T, E1, E0, isig, itemp, imodel);
            if(i >  H2) fprintf(fp, " %14.7e \n", eta*rhox);
        }
    }
    fclose(fp);
    printf("File rhotmopaw.cnt (0:%d) written to disk, (a0,a1)=(%8.2f,%8.3f)\n",FGmax,a0,a1);
	
    /* ****************************************** */
    /* Normalized rhopaw.cnt are written to disk  */
    /* ****************************************** */
    fp = fopen("rhopaw.cnt", "w+");
    if(fp == NULL){
        printf("Could not open file rhopaw.cnt \n");
        exit(0);
    }
    else {
        for (i = 0; i <= dim; i++){
            ex = (a0+a1*(float)i)/1000.;
			x = Anorm*exp(alpha*ex)*rho[i];
			if(x < eps) x = eps_0;

            fprintf(fp, " %14.7e \n", x);
        }
        for (i = 0; i <= dim; i++){
            ex = (a0+a1*(float)i)/1000.;
			x = Anorm*exp(alpha*ex)*drho[i];
			if(x < eps) x = eps_0;
            fprintf(fp, " %14.7e \n", x);
        }
    }
    fclose(fp);
    printf("File rhopaw.cnt (0:%d) written to disk, (a0,a1)=(%8.2f,%8.3f)\n",2*dim+1,a0,a1);
	
    /* ************************************************ */
    /* Normalized slope sigpaw.cnt are written to disk  */
    /* ************************************************ */
    fp = fopen("sigpaw.cnt", "w+");
    if(fp == NULL){
        printf("Could not open file sigpaw.cnt \n");
        exit(0);
    }
    else {
        for (i = 0; i <= dim; i++){
            eg = (a0+a1*(float)i)/1000.;
            nsig[i] = Anorm*exp(alpha*eg)*sig[i];
			if(nsig[i] < eps) nsig[i] = eps_0;
            fprintf(fp, " %14.7e \n", nsig[i]);
        }
        for (i = 0; i <= dim; i++){
            eg = (a0+a1*(float)i)/1000.;
            ndsig[i] = Anorm*exp(alpha*eg)*dsig[i];
			if(ndsig[i] < eps) ndsig[i] = eps_0;
            fprintf(fp, " %14.7e \n", ndsig[i]);
        }
    }
    fclose(fp);
    printf("File sigpaw.cnt (0:%d) written to disk, (a0,a1)=(%8.2f,%8.3f)\n",2*dim+1,a0,a1);
	
    /* ****************************************** */
    /* Extending sigpaw.cnt to Eg = 0 and Eg = Bn */
    /* Called sigextpaw.cnt                       */
    /* ****************************************** */
    sigdim = (((Bn + 0.5 - (a0/1000.))/(a1/1000.))+0.5);
    extendL();
    extendH();
    fp = fopen("sigextpaw.cnt", "w+");
    if(fp == NULL){
        printf("Could not open file sigextpaw.cnt \n");
        exit(0);
    }
    else {
        for (i = 0; i <= TL1; i++){
            fprintf(fp, " %14.7e \n", nsigL[i]);
        }
        for (i = TL1+1; i < TH2 ; i++){
            fprintf(fp, " %14.7e \n", nsig[i]);
        }
        for (i = TH2; i <= sigdim; i++){
            fprintf(fp, " %14.7e \n", nsigH[i]);
        }
    }
    fclose(fp);
    printf("\nFile sigpawext.cnt (0:%d) written to disk, (a0,a1)=(%8.2f,%8.3f)\n",sigdim,a0,a1);
	
    fp = fopen("extendLH.cnt", "w+");
    if(fp == NULL){
        printf("Could not open file extendLH.cnt \n");
        exit(0);
    }
    else {
        for (i = 0; i <= sigdim; i++){
            fprintf(fp, " %14.7e  %14.7e \n", nsigL[i],nsigH[i] );
        }
    }
    fclose(fp);
    printf("File extendLH.cnt (0:%d) (0:%d) written to disk, (a0,a1)=(%8.2f,%8.3f)\n",sigdim,sigdim,a0,a1);
	

    /* ********************************************* */
    /* Density from known levels are written to disk */
    /* ********************************************* */
    fp = fopen("rholev.cnt", "w+");
    if(fp == NULL){
        printf("Could not open file rholev.cnt \n");
        exit(0);
    }
    else {
        for (i = 0; i <= dim; i++){
            eg = (a0+a1*(float)i)/1000.;
			if(rholev[i] < eps) rholev[i] = eps_0;
            fprintf(fp, " %14.7e \n", rholev[i]);
        }
    }
    fclose(fp);	
    printf("File rholev.cnt (0:%d) written to disk, (a0,a1)=(%8.2f,%8.3f)\n",dim,a0,a1);

    /* ****************************************** */
    /* Spin cutoff parameters are written to disk */
    /* ****************************************** */
    fp = fopen("spincut.cnt", "w+");
    if(fp == NULL){
        printf("Could not open file spincut.cnt \n");
        exit(0);
    }
    else {
        for (i = 0; i <= 3*dim; i++){
            ex = (a0+a1*(float)i)/1000.;
            rhofg(Amass, ex, a, T, E1, E0, isig, itemp, imodel);
            fprintf(fp, " %14.7e \n", sqrt(sig2));
        }
    }
    fclose(fp);	
    printf("File spincut.cnt (0:%d) written to disk, (a0,a1)=(%8.2f,%8.3f)\n",3*dim,a0,a1);
    
    /* **************************************************** */
    /* Storing default values for the next run in input.cnt */
    /* **************************************************** */
    fp = fopen("input.cnt", "w");
    if(fp == NULL){
        printf("Could not open file input.cnt \n");
        exit(0);
    }
    else {
        fprintf(fp, " %f %f %f %f %f \n", Amass, Delta, Bn, rho0, drho0);
        fprintf(fp, " %d %d %d %d \n", L1, L2, H1, H2);
        fprintf(fp, " %d %d %d %d \n", TL1, TL2, TH1, TH2);
        fprintf(fp, " %d %f %f \n", isig, aRobin, E1Robin);
        fprintf(fp, " %d %f %f \n", itemp, TRobin, E0Robin);
        fprintf(fp, " %d \n", imodel);
        fprintf(fp, " %d %f %f \n", ansL, abestL, bbestL);
        fprintf(fp, " %d %f %f \n", ansH, abestH, bbestH);
    }
    fclose(fp);
    
    makeroot1();
    makeroot2();
    makeroot3();
    return(0);
}

void rhofg(float Amass, float ex, float a, float T, float E1, float E0, int isig, int itemp, int imodel){
    float exx, uCT=0.01, uFG=0.01, vv;
    rhox = 0.1;
    exx = ex;
    if(exx <= 0.01)exx = 0.01;
    uCT = exx - E0;
    uFG = exx - E1;
    if(uCT < 0.01) uCT = 0.01;
    if(uFG < 0.01) uFG = 0.01;
    
    if(isig == 1){
        if(itemp == 1) sig2 = 0.0146*pow(Amass,(5./3.))*sqrt(uFG/a);
        if(itemp == 2) sig2 = 0.0146*pow(Amass,(5./3.))*(1. + sqrt(1. + 4.*a*uFG))/(2.*a);
    }
    if(isig == 2){
        if(itemp == 1) sig2 = 0.0888*pow(Amass,(2./3.))*a*sqrt(uFG/a);
        if(itemp == 2) sig2 = 0.0888*pow(Amass,(2./3.))*a*(1. + sqrt(1. + 4.*a*uFG))/(2.*a);
    }
    if(isig == 3) {                                         /* Constant temperature formula */
        sig2 = 0.98*pow(Amass,0.29)*0.98*pow(Amass,0.29);   /* Does not depend on Ex */
    }
    if(isig == 4){
        vv   = ex - (E1+0.381);     /* vv is (Ex-0.5Pa_prime)**0.381, with 0.5Pa_prime=E1+0.381 */
        if(vv < 0.010)vv = 0.010;
        sig2 = 0.391 *pow(Amass,(0.675))*pow(vv,0.312);
    }

    if(sig2 < 1.) sig2 = 1.;
    if(imodel == 1)rhox = (1./T)*exp(uCT/T);
    if(imodel == 2){
        if(uFG < (25./16.)/a) uFG = (25./16.)/a;
        rhox = exp(2.*sqrt(a*uFG))/(12.*sqrt(2.*sig2)*pow(a,(1./4.))*pow(uFG,(5./4.)));
    }
    return ;
}


int makeroot1(){
    float Emin, Emax, half, exx, ex0, lower, eL1, eL2, eH1, eH2, cL1, cL2, cH1, cH2, Rhomin, Rhomax;
    int Hmin, Hmax, dH;
    Emax = Bn + 0.5;
    Hmax = (((Bn*1000. + 500. - a0)/a1)+0.5);
    Emax = (a0 + a1 * Hmax)/1000.;
    Emin = a0/1000.;
    if(Emin > 0.)Emin = 0.;
    Hmin = H1 - (H2 - H1);
    if(Hmin <  Hmax/2.)Hmin =Hmax/2.;
    if(Hmin > H1) Hmin = H1;
    dH   = Hmax - Hmin +1;
    Rhomin = 1000000.;
    for (i = 0; i <= dim; i++){
        ex = (a0+a1*(float)i)/1000.;		
        if (Anorm*exp(alpha*ex)*rho[i] < Rhomin && Anorm*exp(alpha*ex)*rho[i] > 0.0001 ) Rhomin = 0.9*Anorm*exp(alpha*ex)*rho[i];
    }
    if (Rhomin < 0.0001) Rhomin = 0.0001;
    exx = log10(10.*rho0) + log10(Rhomin);
    ex0 = log10(10.*rho0);
    half = pow(10.,exx/2.);
    lower = Rhomin*0.3;
    eL1 = (a0 + a1 * L1)/1000.;
    eL2 = (a0 + a1 * L2)/1000.;
    eH1 = (a0 + a1 * H1)/1000.;
    eH2 = (a0 + a1 * H2)/1000.;
    cL1 = Anorm*exp(alpha*eL1)*rho[L1]*pow(10.,ex0/10.);
    cL2 = Anorm*exp(alpha*eL2)*rho[L2]*pow(10.,ex0/10.);
    cH1 = Anorm*exp(alpha*eH1)*rho[H1]*pow(10.,ex0/10.);
    cH2 = Anorm*exp(alpha*eH2)*rho[H2]*pow(10.,ex0/10.);
    Rhomax = 2.*cH2*pow(10.,exx/10.);
    if(Rhomax < 10.*rho0) Rhomax = 10.*rho0;
    fp = fopen("counting.cpp", "w+");
    if(fp == NULL){
        printf("Could not open file counting.cpp \n");
        exit(0);
    }
    else {
        fprintf(fp,"{\n");
        fprintf(fp,"   gROOT->Reset();\n");
        fprintf(fp,"   gROOT->SetStyle(\"Plain\");\n");
        fprintf(fp,"   gStyle->SetOptTitle(0);\n");
        fprintf(fp,"   gStyle.SetOptStat(0);\n");
        fprintf(fp,"   gStyle.SetFillColor(0);\n");
        fprintf(fp,"   gStyle.SetPadBorderMode(0);\n");
        fprintf(fp,"   m = (TH1F*)gROOT->FindObject(\"h\");\n");
        fprintf(fp,"   if (m) m->Delete();\n");
        fprintf(fp,"   TCanvas *c1 = new TCanvas(\"c1\",\"Normalization of level density\",600,600);\n");
        fprintf(fp,"   TH2F *h = new TH2F(\"h\",\" \",10,%f,%f,50,%f,%f);\n",Emin,Emax,Rhomin,Rhomax);
        fprintf(fp,"   ifstream rholev(\"rholev.cnt\"), rhopaw(\"rhopaw.cnt\"), fermi(\"fermigas.cnt\");\n");
        fprintf(fp,"   float levels[%d],rho[%d],rhoerr[%d],energy[%d],energyerr[%d],fermigas[%d];\n",dim+2,dim+2,dim+2,FGmax+2,FGmax+2,FGmax+2);
        fprintf(fp,"   float Bn[1]={%f};\n",Bn);
        fprintf(fp,"   float Bnerr[1]={0.001};\n");
        fprintf(fp,"   float rho_Bn[1]={%f};\n",rho0);
        fprintf(fp,"   float rho_Bnerr[1]={%f};\n",drho0);
        fprintf(fp,"   int i = 0;\n");
        fprintf(fp,"   float a0 = %8.4f;\n",a0/1000.);
        fprintf(fp,"   float a1 = %8.4f;\n",a1/1000.);
        fprintf(fp,"   float x,y,z;\n");
        fprintf(fp,"   while(fermi){\n");
        fprintf(fp,"   	fermi >> x;\n");
        fprintf(fp,"   	fermigas[i]=x;\n");
        fprintf(fp,"   	energy[i]=a0+(a1*i);\n");
        fprintf(fp,"   	energyerr[i]=0.0;\n");
        fprintf(fp,"      i++;\n");
        fprintf(fp,"   }\n");
        fprintf(fp,"   i=0;\n");
        fprintf(fp,"   while(rhopaw){\n");
        fprintf(fp,"   	rhopaw >> y;\n");
        fprintf(fp,"   	if(i<%d){\n",dim+1);
        fprintf(fp,"   		rho[i]=y;\n");
        fprintf(fp,"   	}\n");
        fprintf(fp,"   	else{rhoerr[i-%d]=y;}\n",dim+1);
        fprintf(fp,"   	i++;\n");
        fprintf(fp,"   }\n");
        fprintf(fp,"  	i=0;\n");
        fprintf(fp,"	while(rholev){\n");
        fprintf(fp,"		rholev >> z;\n");
        fprintf(fp,"		levels[i]=z;\n");
        fprintf(fp,"		i++;\n");
        fprintf(fp,"  }\n");
        fprintf(fp,"   TGraphErrors *rhoexp = new TGraphErrors(%d,energy,rho,energyerr,rhoerr);\n",dim+1);
        fprintf(fp,"   TGraphErrors *rhoBn = new TGraphErrors(1,Bn,rho_Bn,Bnerr,rho_Bnerr);\n");
        fprintf(fp,"   TGraph *fermicalc = new TGraph(%d,energy,fermigas);\n",FGmax+1);
        fprintf(fp,"   TGraph *level = new TGraph(%d,energy,levels);\n",dim+1);
        fprintf(fp,"   c1.SetLogy();\n");
        fprintf(fp,"   c1.SetLeftMargin(0.14);\n");
        fprintf(fp,"   h.GetXaxis().CenterTitle();\n");
        fprintf(fp,"   h.GetXaxis().SetTitle(\"Excitation energy E (MeV)\");\n");
        fprintf(fp,"   h.GetYaxis().CenterTitle();\n");
        fprintf(fp,"   h.GetYaxis().SetTitleOffset(1.4);\n");
        fprintf(fp,"   h.GetYaxis().SetTitle(\"Level density #rho (E) (MeV^{-1})\");\n");
        fprintf(fp,"   h.Draw();\n");
        fprintf(fp,"   rhoexp->SetMarkerStyle(21);");
        fprintf(fp,"   rhoexp->SetMarkerSize(0.8);\n");
        fprintf(fp,"   rhoexp->Draw(\"P\");\n");
        fprintf(fp,"   fermicalc.SetLineStyle(2);\n");
        fprintf(fp,"   fermicalc.DrawGraph(%d,&fermicalc->GetX()[%d],&fermicalc->GetY()[%d],\"L\");\n",dH,Hmin,Hmin);
        fprintf(fp,"   level.SetLineStyle(1);\n");
        fprintf(fp,"   level->Draw(\"L\");\n");
        fprintf(fp,"   rhoBn->SetMarkerStyle(25);\n");
        fprintf(fp,"   rhoBn->SetMarkerSize(0.8);\n");
        fprintf(fp,"   rhoBn->Draw(\"P\");\n");
        fprintf(fp,"   TLegend *leg = new TLegend(0.15,0.70,0.6,0.85);\n");
        fprintf(fp,"   leg.SetBorderSize(0);\n");
        fprintf(fp,"   leg.SetFillColor(0);\n");
        fprintf(fp,"   leg->AddEntry(rhoexp,\" Oslo data \",\"P\");\n");
        fprintf(fp,"   leg->AddEntry(level,\" Known levels \",\"L\");\n");
        fprintf(fp,"   leg->AddEntry(fermicalc,\" CT or FG model \",\"L\");	\n");
        fprintf(fp,"   leg->AddEntry(rhoBn,\" #rho from neutron res. data \",\"P\");\n");
        fprintf(fp,"   leg->Draw();\n");
        fprintf(fp,"   TLatex t;\n");
        fprintf(fp,"   t.SetTextSize(0.05);\n");
        fprintf(fp,"   t.DrawLatex(%9.3f,%9.3e,\"^{xx}Yy\");\n",0.8*Emax,0.5*Rhomax);
        fprintf(fp,"   TArrow *arrow1 = new TArrow(%f,%f,%f,%f,0.02,\">\");\n",eL1,cL1*pow(10.,ex0/10.),eL1,cL1);
        fprintf(fp,"   arrow1->Draw();\n");
        fprintf(fp,"   TArrow *arrow2 = new TArrow(%f,%f,%f,%f,0.02,\">\");\n",eL2,cL2*pow(10.,ex0/10.),eL2,cL2);
        fprintf(fp,"   arrow2->Draw();\n");
        fprintf(fp,"   TArrow *arrow3 = new TArrow(%f,%f,%f,%f,0.02,\">\");\n",eH1,cH1*pow(10.,ex0/10.),eH1,cH1);
        fprintf(fp,"   arrow3->Draw();\n");
        fprintf(fp,"   TArrow *arrow4 = new TArrow(%f,%f,%f,%f,0.02,\">\");\n",eH2,cH2*pow(10.,ex0/10.),eH2,cH2);
        fprintf(fp,"   arrow4->Draw();\n");
        fprintf(fp,"   c1->Update();\n");
		fprintf(fp,"   c1->Print(\"counting.pdf\");\n");
        fprintf(fp,"   c1->Print(\"counting.eps\");\n");
        fprintf(fp,"   c1->Print(\"counting.ps\");\n");
        fprintf(fp,"}\n");
    }
    fclose(fp);
    printf("File counting.cpp written to disk. Run root to plot normalized NLD.\n");
    return 0;
}

int makeroot2(){
    int dimcut;
    float emin,emax;
    float cmax;
    dimcut = 3*dim;
    emax = (a0 + a1*(float)dimcut)/1000.;
    emin = a0/1000.;
    if(emin > 0.)emin = 0.;
    rhofg(Amass, ex, a, T, E1, E0, isig, itemp, imodel);
    cmax = sqrt(sig2);
    fp = fopen("spincut.cpp", "w+");
    if(fp == NULL){
        printf("Could not open file spincut.cpp \n");
        exit(0);
    }
    else {
        fprintf(fp,"{\n");
        fprintf(fp,"	gROOT->Reset();\n");
        fprintf(fp,"	gROOT->SetStyle(\"Plain\");\n");
        fprintf(fp,"   gStyle->SetOptTitle(0);\n");	
        fprintf(fp,"   gStyle.SetOptStat(0);\n");
        fprintf(fp,"   gStyle.SetFillColor(0);\n");
        fprintf(fp,"   gStyle.SetPadBorderMode(0);\n");
        fprintf(fp,"   m = (TH1F*)gROOT->FindObject(\"h\");\n"); 
        fprintf(fp,"   if (m) m->Delete();\n"); 
        fprintf(fp,"   TCanvas *c1 = new TCanvas(\"c1\",\"Spincut parameter\",600,600);\n");	
        fprintf(fp,"   TH2F *h = new TH2F(\"h\",\" \",10,%f,%f,50,0.001,%7.1f);\n",emin,emax,cmax+1.);
        fprintf(fp,"   ifstream spincutfile(\"spincut.cnt\");\n");
        fprintf(fp,"   float energy[%d],spincut[%d];\n",dimcut+2,dimcut+2);
        fprintf(fp,"   int i = 0;\n");
        fprintf(fp,"   float a0 = %8.4f;\n",a0/1000.); 
        fprintf(fp,"   float a1 = %8.4f;\n",a1/1000.);
        fprintf(fp,"   float x,y,z;\n");
        fprintf(fp,"   while(spincutfile){\n");
        fprintf(fp,"   	spincutfile >> x;\n");
        fprintf(fp,"	   spincut[i]=x;\n");
        fprintf(fp,"	   energy[i]=a0+(a1*i);\n");
        fprintf(fp,"	   i++;\n");
        fprintf(fp,"   }\n");
        fprintf(fp,"   TGraph *spincutgraph = new TGraph(%d,energy,spincut);\n",dimcut+1);
        fprintf(fp,"   c1.SetLeftMargin(0.14);\n");
        fprintf(fp,"   h.GetXaxis().CenterTitle();\n");
        fprintf(fp,"   h.GetXaxis().SetTitle(\"Excitation energy E (MeV)\");\n");
        fprintf(fp,"   h.GetYaxis().CenterTitle();\n");
        fprintf(fp,"   h.GetYaxis().SetTitleOffset(1.4);\n");
        fprintf(fp,"   h.GetYaxis().SetTitle(\"Spin cutoff #sigma\");\n");
        fprintf(fp,"   h.Draw();\n");
        fprintf(fp,"   spincutgraph.Draw(\"L\");\n");
        fprintf(fp,"   c1->Update();\n");
		fprintf(fp,"   c1->Print(\"spincut.pdf\");\n");
        fprintf(fp,"   c1->Print(\"spincut.eps\");\n");
        fprintf(fp,"   c1->Print(\"spincut.ps\");\n");
        fprintf(fp,"}\n");
    }
    fclose(fp);
    printf("File spincut.cpp written to disk. Run root to plot the spincut.\n");
    return 0;
}

int makeroot3(){
    float Emin, Emax, half, exx, ex0, lower, eL1, eL2, eH1, eH2, cL1, cL2, cH1, cH2, T0, Tmin, Tmax;
    int Hmin, Hmax, dH;
    Emax = Bn + 0.5;
    Hmax = (((Bn + 1.5 - (a0/1000.))/(a1/1000.))+0.5); // 1500 above the Bn
    Emax = (a0/1000.) + (a1/1000.) * Hmax;
    Emin = a0/1000.;
    if(Emin > 0.)Emin = 0.;
    Hmin = TH1 - (TH2 - TH1);
    if(Hmin <  Hmax/2.)Hmin =Hmax/2.;
    if(Hmin > TH1) Hmin = TH1;
    dH   = Hmax - Hmin +1;
    Tmin = 1000000.;
    for (i = 0; i <= dim; i++){
        eg = (a0+a1*(float)i)/1000.;
        if (Anorm*exp(alpha*eg)*sig[i] < Tmin && Anorm*exp(alpha*eg)*sig[i] > 0.0001 ) Tmin = 0.1*Anorm*exp(alpha*eg)*sig[i];
    }
    if (Tmin < 0.0001) Tmin = 0.0001;
    T0=0.0001;
    for (i = 0; i <= dim; i++){
        eg = (a0+a1*(float)i)/1000.;
        if (Anorm*exp(alpha*eg)*sig[i] > T0 && Anorm*exp(alpha*eg)*sig[i] > 0.0001 ) T0 = 10.*Anorm*exp(alpha*eg)*sig[i];
    }
    exx = log10(10.*T0) + log10(Tmin);
    ex0 = log10(10.*T0);
    half = pow(10.,exx/2.);
    lower = Tmin*0.3;
    eL1 = (a0 + a1 * TL1)/1000.;
    eL2 = (a0 + a1 * TL2)/1000.;
    eH1 = (a0 + a1 * TH1)/1000.;
    eH2 = (a0 + a1 * TH2)/1000.;
    cL1 = Anorm*exp(alpha*eL1)*sig[TL1]*pow(10.,ex0/10.);
    cL2 = Anorm*exp(alpha*eL2)*sig[TL2]*pow(10.,ex0/10.);
    cH1 = Anorm*exp(alpha*eH1)*sig[TH1]*pow(10.,ex0/10.);
    cH2 = Anorm*exp(alpha*eH2)*sig[TH2]*pow(10.,ex0/10.);
    Tmax = 2.*cH2*pow(10.,exx/10.);
    if(Tmax < 10.*T0) Tmax = 10.*T0;

    fp = fopen("sigext.cpp", "w+");
    if(fp == NULL){
        printf("Could not open file sigext.cpp \n");
        exit(0);
    }
    else {
	fprintf(fp,"{\n");
	fprintf(fp,"   gROOT->Reset();\n");
	fprintf(fp,"   gROOT->SetStyle(\"Plain\");\n");
	fprintf(fp,"   gStyle->SetOptTitle(0);\n");
	fprintf(fp,"   gStyle.SetOptStat(0);\n");
	fprintf(fp,"   gStyle.SetFillColor(0);\n");
	fprintf(fp,"   gStyle.SetPadBorderMode(0);\n");
	fprintf(fp,"   m = (TH1F*)gROOT->FindObject(\"h\");\n"); 
	fprintf(fp,"   if (m) m->Delete();\n");
	fprintf(fp,"   TCanvas *c1 = new TCanvas(\"c1\",\"Normalization of gamma-transmission coefficient\",600,600);\n");	
	fprintf(fp,"   TH2F *h = new TH2F(\"h\",\" \",10,%f,%8.3f,50,%9.3e,%9.3e);\n",Emin,Emax,Tmin,Tmax);
	fprintf(fp,"   ifstream sigfile(\"sigpaw.cnt\");\n");
	fprintf(fp,"   float sig[%d],sigerr[%d];\n",dim+2,dim+2);
	fprintf(fp,"   float energy[%d],energyerr[%d];\n",Hmax+2,Hmax+2);
    fprintf(fp,"   float extL[%d],extH[%d];\n",Hmax+2,Hmax+2);
	fprintf(fp,"   int i;\n");
	fprintf(fp,"   float a0 =%8.4f;\n",a0/1000.);
	fprintf(fp,"   float a1 =%8.4f;\n",a1/1000.);
	fprintf(fp,"   for(i = 0; i < %d; i++){\n",Hmax+2);
	fprintf(fp,"   	energy[i] = a0 + (a1*i);\n");
	fprintf(fp,"   	energyerr[i] = 0.0;\n");
    fprintf(fp,"   	extL[i] = 0.0;\n");
    fprintf(fp,"   	extH[i] = 0.0;\n");
	fprintf(fp,"   }\n");
	fprintf(fp,"   float x, y;\n");
	fprintf(fp,"   i = 0;\n");
	fprintf(fp,"   while(sigfile){\n");
	fprintf(fp,"   	sigfile >> x;\n");
	fprintf(fp,"   	if(i<%d){\n",dim+1);
	fprintf(fp,"   		sig[i]=x;\n");
	fprintf(fp,"   	}\n");
	fprintf(fp,"   	else{sigerr[i-%d]=x;}\n",dim+1);
	fprintf(fp,"   	i++;\n");
	fprintf(fp,"   }\n");
	fprintf(fp,"   ifstream extendfile(\"extendLH.cnt\");\n");
	fprintf(fp,"   i = 0;\n");
	fprintf(fp,"   while(extendfile){\n");
	fprintf(fp,"   	extendfile >> x >> y ;\n");
	fprintf(fp,"   	extL[i]=x;\n");
	fprintf(fp,"   	extH[i]=y;\n");
	fprintf(fp,"   	i++;\n");
	fprintf(fp,"   }\n"); 
	fprintf(fp,"   TGraph *extLgraph = new TGraph(%d,energy,extL);\n",Hmax+1);
	fprintf(fp,"   TGraph *extHgraph = new TGraph(%d,energy,extH);\n",Hmax+1);
    fprintf(fp,"   TGraphErrors *sigexp = new TGraphErrors(%d,energy,sig,energyerr,sigerr);\n",dim+1);
	fprintf(fp,"   c1.SetLogy();\n");
	fprintf(fp,"   c1.SetLeftMargin(0.14);\n");
	fprintf(fp,"   h.GetXaxis().CenterTitle();\n");
	fprintf(fp,"   h.GetXaxis().SetTitle(\"#gamma-ray energy E_{#gamma} (MeV)\");\n");
	fprintf(fp,"   h.GetYaxis().CenterTitle();\n");
	fprintf(fp,"   h.GetYaxis().SetTitleOffset(1.4);\n");
	fprintf(fp,"   h.GetYaxis().SetTitle(\"Transmission coeff. (arb. units)\");\n");
	fprintf(fp,"   h.Draw();\n");
	fprintf(fp,"   sigexp->SetMarkerStyle(21);\n");
	fprintf(fp,"   sigexp->SetMarkerSize(0.8);\n");
	fprintf(fp,"   sigexp->Draw(\"P\");\n");
	fprintf(fp,"   extLgraph.SetLineStyle(1);\n");
	fprintf(fp,"   extLgraph.DrawGraph(%d,&extLgraph->GetX()[0],&extLgraph->GetY()[0],\"L\");\n", TL2+1);
	fprintf(fp,"   extHgraph.SetLineStyle(1);\n");
	fprintf(fp,"   extHgraph.DrawGraph(%d,&extHgraph->GetX()[%d],&extHgraph->GetY()[%d],\"L\");\n",Hmax-TH1+1,TH1,TH1);
	fprintf(fp,"   TArrow *arrow1 = new TArrow(%9.3e,%9.3e,%9.3e,%9.3e,0.02,\">\");\n",eL1,cL1*pow(10.,ex0/10.),eL1,cL1); 
	fprintf(fp,"   arrow1->Draw();\n");
	fprintf(fp,"   TArrow *arrow2 = new TArrow(%9.3e,%9.3e,%9.3e,%9.3e,0.02,\">\");\n",eL2,cL2*pow(10.,ex0/10.),eL2,cL2);
	fprintf(fp,"   arrow2->Draw();\n");
	fprintf(fp,"   TArrow *arrow3 = new TArrow(%9.3e,%9.3e,%9.3e,%9.3e,0.02,\">\");\n",eH1,cH1*pow(10.,ex0/10.),eH1,cH1); 
	fprintf(fp,"   arrow3->Draw();\n");
	fprintf(fp,"   TArrow *arrow4 = new TArrow(%9.3e,%9.3e,%9.3e,%9.3e,0.02,\">\");\n",eH2,cH2*pow(10.,ex0/10.),eH2,cH2);
	fprintf(fp,"   arrow4->Draw();\n");
	fprintf(fp,"   c1->Update();\n");
	fprintf(fp,"   c1->Print(\"sigext.pdf\");\n");
	fprintf(fp,"   c1->Print(\"sigext.eps\");\n");
	fprintf(fp,"   c1->Print(\"sigext.ps\");\n");
	fprintf(fp,"}\n");
    }
    fclose(fp);
    printf("File sigext.cpp written to disk. Run root to plot sigextpaw.cnt.\n");
    return 0;
}

int searchAalpha(){
    float rhoL;
    /* ************************************************ */
    /* Take three channels in the middle of each region */
    /* in order to make a first estimate of A and alpha */
    /* ************************************************ */
    Lm = (L1 + L2)/2;
    Hm = (H1 + H2)/2;
    c1 = (rho[Lm-1]+rho[Lm]+rho[Lm+1])/3.;
    c2 = (rho[Hm-1]+rho[Hm]+rho[Hm+1])/3.;
    rhoL = (rholev[Lm-1]+rholev[Lm]+rholev[Lm+1])/3.;
    if(rhoL < 0.01) rhoL = 0.01;
    e1 = (a0+a1*(float)Lm)/1000.;
    e2 = (a0+a1*(float)Hm)/1000.;
    /*	printf("Lm=%d,Hm=%d,c1=%f,c2=%f,e1=%f,e2=%f\n",Lm,Hm,c1,c2,e1,e2);*/
    rhofg(Amass, e2, a, T, E1, E0, isig, itemp, imodel);
    alpha = (log(rhoL)+log(c2)-log(eta*rhox)-log(c1))/(e1-e2);;
    Anorm = exp(-alpha*e1)*(rhoL)/c1;
    printf("\nFirst estimate of normalization parameters: A = %7.4f and alpha = %6.4f\n",Anorm,alpha);
	
    /* ******************************************* */
    /* Determine c1 and c2 by weighting all points */
    /* in each region with uncertainties and slope */
    /* ******************************************* */
    c1 = c1/corrL();
    c2 = c2/corrH();
    /*	printf("Lm=%d,Hm=%d,c1=%f,c2=%f,e1=%f,e2=%f\n",Lm,Hm,c1,c2,e1,e2);*/
    rhofg(Amass, e2, a, T, E1, E0, isig, itemp, imodel);
    alpha = (log(rhoL)+log(c2)-log(eta*rhox)-log(c1))/(e1-e2);
    Anorm = exp(-alpha*e1)*(rhoL)/c1;
    printf("Final estimate of normalization parameters: A = %7.4f and alpha = %6.4f\n",Anorm,alpha);
    return 0;
}

float corrL(){
    float corr, corrbest=0., sum, sumbest, sum0=0., cc, free, dc2, rhoL;
    corr = 0.25;
    sumbest = 1.0e+21;
    free = (float)L2 - (float)L1;
    if (free <= 0)free = 1.; 
    for(j = 0; j <= 3750; j++){
        corr = corr + 0.001;
        sum  = 0.0;
        for(i = L1; i <= L2; i++){
            ex = (a0+a1*(float)i)/1000.;
            rhoL = rholev[i];
            if(rhoL < 0.01) rhoL = 0.01;
            cc = corr*Anorm*exp(alpha*ex)*rho[i];
            dc2= (corr*Anorm*exp(alpha*ex)*drho[i])*(corr*Anorm*exp(alpha*ex)*drho[i]);
            dc2 =sqrt(dc2*dc2 + 1.*1.);
            if(dc2 > 0) sum = sum + ((cc-rhoL)*(cc-rhoL)/dc2);
        }
        sum = sum/free;
        if(j == 499){
            sum0 = sum;
        }
        if(sum <= sumbest){
            sumbest = sum;
            corrbest= corr;
        }
    }
    printf("Improved Chi2 for lower part:  %5.2f -> %5.2f with count correction = %5.3f \n",sum0,sumbest,corrbest);
    return corrbest;
}

float corrH(){
    float corr, corrbest=0., sum, sumbest, sum0=0., cc, free, dc2;
    corr = 0.25;
    sumbest = 1.0e+21;
    free = (float)H2 - (float)H1;
    if (free <= 0)free = 1.; 
    for(j = 0; j <= 3750; j++){
        corr = corr + 0.001;
        sum  = 0.0;
        for(i = H1; i <= H2; i++){
            ex = (a0+a1*(float)i)/1000.;
            cc = corr*Anorm*exp(alpha*ex)*rho[i];
            dc2= (corr*Anorm*exp(alpha*ex)*drho[i])*(corr*Anorm*exp(alpha*ex)*drho[i]);
            dc2 =sqrt(dc2*dc2 + 1.*1.);
            rhofg(Amass, ex, a, T, E1, E0, isig, itemp, imodel);
            if(dc2 > 0) sum=sum+(cc-eta*rhox)*(cc-eta*rhox)/dc2;
        } 
        sum = sum/free;
        if(j == 499){
            sum0 = sum;
        }
        if(sum <= sumbest){
            sumbest = sum;
            corrbest= corr;
        }
    }
    printf("Improved Chi2 for higher part: %5.2f -> %5.2f with count correction = %5.3f \n",sum0,sumbest,corrbest);
    return corrbest;
}

int extendL()
{
    int i, j, k, steps = 1000;
    float x, y, yi, dyi, ai, bi, x1, x2, y1, y2;
    float chi, aa, bb, al, bl, ah, bh, astep, bstep, chibest, abest=0., bbest=0.;
    x1 = (a0+a1*(float)TL1)/1000.;
    x2 = (a0+a1*(float)TL2)/1000.;
    y1 = log(nsig[TL1]);
    y2 = log(nsig[TL2]);
    if(TL2-TL1 > 3){
        x1 = (a0+a1*(float)TL1 + 0.5)/1000.;
        x2 = (a0+a1*(float)TL2 - 0.5)/1000.;
        y1 = (log(nsig[TL1]) + log(nsig[TL1+1]))/2.;
        y2 = (log(nsig[TL2]) + log(nsig[TL2-1]))/2.;
    }
    ai = (y2-y1)/(x2-x1);
    bi = y1 - ai*x1;
    al = ai/3.;
    ah = ai*3.;
    astep = (ah-al)/(float)steps;
    bh = bi + 2.*ai*(x2-x1);
    bl = bi - 2.*ai*(x2-x1);
    bstep = (bh-bl)/(float)steps;
//    printf(" Lower fit to sig: a = %f<%f<%f  b = %f<%f<%f\n",al,ai,ah,bl,bi,bh);
	
    chibest = 999999999.;
    bb = bl;
    for(i = 0; i < steps; i++){
        bb = bb + bstep;
        aa = al;
        for(j = 0; j < steps; j++){
            aa = aa + astep;
            chi = 0;
            for(k = TL1; k <= TL2 && nsig[k] > 0 && ndsig[k] > 0; k++){
                x   = (a0+a1*(float)k)/1000.;
                y   = aa * x + bb;
                yi  = log(nsig[k]);
                dyi = ndsig[k]/nsig[k];
                chi = chi + (y-yi)*(y-yi)/(dyi*dyi);
            }
            chi = chi/(float)(TL2-TL1);
            if(chi < chibest && chi > 0){
                chibest = chi;
                abest   = aa;
                bbest   = bb;
                /*				printf(" chi = %f a = %f b = %f \n", chi, aa, bb);  */
            }
        }
    }
    printf("\n Transmission function sigext = exp(a*Eg + b) is fitted to the \n");
    printf(" lower data points with result: Chi2 = %f a = %f b = %f \n", chibest, abest, bbest);
    printf(" Change a and b parameters? (0 = no, 1 = yes) <%d>:",ansL);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%d", &ansL);
    
    if(ansL == 1){
        if(abestL == -1000.)abestL = abest;
        if(bbestL == -1000.)bbestL = bbest;
        printf("Choose another a-value <%f>:",abestL);
        fgets_ignore(line,sizeof(line),stdin);
        sscanf(line,"%f", &abestL);
        printf("Choose another b-value <%f>:",bbestL);
        fgets_ignore(line,sizeof(line),stdin);
        sscanf(line,"%f", &bbestL);
    }else{
        abestL  = abest;
        bbestL  = bbest;
    }
    for (i = 0; i <= sigdim; i++){
        x = (a0+a1*(float)i)/1000.;
        nsigL[i] = exp(abestL * x + bbestL);
        /*		printf("i = %d  x = %f  nsigL = %14.7e \n", i, x, nsigL[i]);*/
    }
    return 0;
}

int extendH()
{
    int i, j, k, steps = 1000;
    float x, y, yi, dyi, ai, bi, x1, x2, y1, y2;
    float chi, aa, bb, al, bl, ah, bh, astep, bstep, chibest, abest=0., bbest=0.;
    x1 = (a0+a1*(float)TH1)/1000.;
    x2 = (a0+a1*(float)TH2)/1000.;
    y1 = log(nsig[TH1]);
    y2 = log(nsig[TH2]);
    if(TH2-TH1 > 3){
        x1 = (a0+a1*(float)TH1 + 0.5)/1000.;
        x2 = (a0+a1*(float)TH2 - 0.5)/1000.;
        y1 = (log(nsig[TH1]) + log(nsig[TH1+1]))/2.;
        y2 = (log(nsig[TH2]) + log(nsig[TH2-1]))/2.;
    }
    ai = (y2-y1)/(x2-x1);
    bi = y1 - ai*x1;
    al = ai/3.;
    ah = ai*3.;
    astep = (ah-al)/(float)steps;
    bh = bi + 2.*ai*(x2-x1);
    bl = bi - 2.*ai*(x2-x1);
    bstep = (bh-bl)/(float)steps;
//    printf(" Upper fit to sig: a = %f<%f<%f  b = %f<%f<%f\n",al,ai,ah,bl,bi,bh);
	
    chibest = 999999999.;
    bb = bl; 
    for(i = 0; i < steps; i++){
        bb = bb + bstep;
        aa = al;
        for(j = 0; j < steps; j++){
            aa = aa + astep;
            chi = 0;
            for(k = TH1; k <= TH2 && nsig[k] > 0 && ndsig[k] > 0; k++){
                x   = (a0+a1*(float)k)/1000.;
                y   = aa * x + bb;
                yi  = log(nsig[k]);
                dyi = ndsig[k]/nsig[k];
                chi = chi + (y-yi)*(y-yi)/(dyi*dyi);
            }
            chi = chi/(float)(TH2-TH1);
            if(chi < chibest && chi > 0){
                chibest = chi;
                abest   = aa;
                bbest   = bb;
                /*				printf(" chi = %f a = %f b = %f \n", chi, aa, bb);*/
            }
        }
    }
    
    printf("\n Transmission function sigext = exp(a*Eg + b) is fitted to the \n");
    printf(" upper data points with result: Chi2 = %f a = %f b = %f \n", chibest, abest, bbest);
    printf(" Change a and b parameters? (0 = no, 1 = yes) <%d>:",ansH);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%d", &ansH);
    
    if(ansH == 1){
        if(abestH == -1000.)abestH = abest;
        if(bbestH == -1000.)bbestH = bbest;
        printf("Choose another a-value <%f>:",abestH);
        fgets_ignore(line,sizeof(line),stdin);
        sscanf(line,"%f", &abestH);
        printf("Choose another b-value <%f>:",bbestH);
        fgets_ignore(line,sizeof(line),stdin);
        sscanf(line,"%f", &bbestH);
    }else{
        abestH  = abest;
        bbestH  = bbest;
    }
    for (i = 0; i <= sigdim; i++){
        x = (a0+a1*(float)i)/1000.;
        nsigH[i] = exp(abestH * x + bbestH);
        /*		printf("i = %d  x = %f  nsigH = %14.7e \n", i, x, nsigH[i]);*/
    }
    return 0;
}
