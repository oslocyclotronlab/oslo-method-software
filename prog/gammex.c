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
#define BN0       9.154 			    /* Default Bn of 96Mo */
#define PI0       3.14159265358979      /* The pi number */

FILE *fp;
char line[128],cdum[128];
int i, j, Nch , dimx, dimy;
float rho[MAXDIM], spincut[MAXDIM], transext[MAXDIM],gammex[MAXDIM] ;
float spin, lowsp, sum, c, x;
int parity;

/* Defining defaults values, taken from 96Mo */
float Bn = BN0, a0 = 60., a1 = 120.;
int   eo = 0;
float partialsum(int ji, int jf, float Ii, int Pi);
float g(int jexc, float jspin, int jparity);
float gsum(int jexc);

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
    printf(" |                        G A M M E X  1.0                      |\r\n");
    printf(" |                                                              |\r\n");
    printf(" |                Program to calculate <Gamma(Ex)>              |\r\n");
    printf(" |           (run normalization prior to this program)          |\r\n");
    printf(" |                                                              |\r\n");
    printf(" |  Input files: transext.nrm    Output files:  input.gex       |\r\n");
    printf(" |               rhotmopaw.cnt                  gammex.gex      |\r\n");
    printf(" |               spincut.cnt                                    |\r\n");
    printf(" |               input.nrm                                      |\r\n");
    printf(" |               (input.gex)                                    |\r\n");
    printf(" | E-mail  : magne.guttormsen@fys.uio.no                        |\r\n");
    printf(" | Created : 14 Nov 2006                                        |\r\n");
    printf(" | Modified: 12 Dec 2006                                        |\r\n");
    printf(" |______________________________________________________________|\r\n");
    printf("                                                                 \r\n");
	
    /* ************************************************** */
    /* Reading calibration and dimensions from: rhosp.rsg */
    /* ************************************************** */
    printf("Reading calibration and dimensions from: rhosp.rsg\n");
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
	sscanf(line,"%s %s %f %s %f",cdum, cdum, &a0, cdum, &a1);
	fgets_ignore(line,sizeof(line),fp);	
	fgets_ignore(line,sizeof(line),fp);
	sscanf(line,"%s %d %s %d",cdum, &dimx, cdum, &dimy);
	fgets_ignore(line,sizeof(line),fp);
	fclose(fp);
    }
    printf("Dimension (0 : %d, 0 : %d) and calibration (a0, a1) = (%f,%f)\n",dimx,dimy,a0,a1);

    /* ***************************************** */
    /* Reading default values from previous runs */
    /* ***************************************** */
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
        sscanf(line, " %f %d \n", &Bn, &eo);
        fclose(fp);
    }
	
    /* *********************** */
    /* Asking for input values */
    /* *********************** */
    printf("Decay within even (0) or odd (1) mass nucleus           <%1d>:",eo);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%d", &eo);
    lowsp = 0;                                  /* Even-mass nucleus with integer spins */
    if(eo == 1) lowsp = 0.5;                    /* Odd-mass nucleus with half integer spins */
    printf("Neutron or proton binding energy (Bn or Bp) (MeV)  <%6.3f>:",Bn);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &Bn);
    Nch = 1 + (int)(((Bn*1000.-a0)/a1)+0.5);    /* Number of channels up to Bn or Bp */
	
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
    while( i < Nch){
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
    while(i < Nch){
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
    while(i < Nch){
        if(fgets(line,sizeof(line),fp) != NULL){sscanf(line,"%f", &spincut[i]);}
        spincut[i] = 2. * spincut[i] * spincut[i];
        i++; 
    }
    fclose(fp);
	
    /* ************************ */
    /* Printing input functions */
    /* ************************ */
    printf("\n No  Ex or Eg(keV)  Rho(1/MeV)      Transext  2*Spincut**2\n");
    for(i = 0 ; i < Nch; i++){
        printf("%3d  %8.2f  %14.3e  %14.3e %6.2f \n",i,a0+a1*(float)i,rho[i],transext[i],spincut[i]);
    }
	 
    /* **************************************************** */
    /* Storing default values for the next run in input.gex */
    /* **************************************************** */
    fp = fopen("input.gex", "w");
    if(fp == NULL){
        printf("Could not open file input.gex \n");
        exit(0);
    } else {
        fprintf(fp, " %f %d\n", Bn, eo);
    }
    fclose(fp);
	
    /* **************** */
    /* Fasten seatbelts */
    /* **************** */
    for (j = 0; j < Nch; j++){
        sum = 0.;
        for (spin = lowsp; spin < lowsp + 20.; spin++){                     /* Applied for spin up to 20 hbar */
            for (parity = 0; parity < 2; parity++){                          /* Parity (0,1) = (+,-) */
                /* Averageing made by weighting with g(j,spin,parity) */
                /* This removes the g in the denominator of 1/2*pi*rho*g  */
                if( rho[j] > 0.){ 
                    c = (a1/1000.) / (2.*PI0*rho[j]);
                }
                else{
                    c = 0.;
                }	
                for (i = 0; i <= j; i++){
                    sum = sum + c * partialsum(j,j-i,spin,parity);
                }
            }
        }
        gammex[j] = sum;
    }
	
    /* *************************************************************** */
    /* Normalized gamma-transmission coefficient T(Eg) written to disk */
    /* *************************************************************** */
    fp = fopen("gammex.gex", "w");
    if(fp == NULL){
        printf("Could not open file gammex.gex \n");
        exit(0);
    }
    else {
        for (i = 0; i < Nch; i++){
            fprintf(fp, " %10.3e \n", gammex[i]*1.e+9);
            printf(" %3d  Ex(keV) = %8.2f   Gamma-width (meV) = %10.3e \n",i, a0+a1*(float)i, gammex[i]*1.e+9);
        }
    }
    fclose(fp);	
    printf("File gammex.gex (0:%d) written to disk, (a0,a1)=(%f,%f)\n",Nch-1,a0,a1);
    return(0);
}

float partialsum(int ji, int jf, float Ii, int Pi)
{
    float xi = 0., xus, xuo, xms, xmo, xds, xdo;
    float TM1, TE1;
    int ps, po;
    ps = Pi;												/* Default is negative initial parity */
    po = 0;
    if(ps == 0) po = 1;								/* The initial parity is positive */
	
    TM1 = 0.8*transext[ji-jf];				      /* For future expension */
    TE1 = 0.2*transext[ji-jf];
    /* Following formulas require parity symmetry */
    xus = TM1 * rho[jf] * g(jf,Ii+1,ps);   	/* One spin up, same parity */
    xuo = TE1 * rho[jf] * g(jf,Ii+1,po);   	/* One spin up, opposite parity */
    xms = TM1 * rho[jf] * g(jf,Ii+0,ps);   	/* Same spin, same parity */
    xmo = TE1 * rho[jf] * g(jf,Ii+0,po);   	/* Same spin, opposite parity */
    xds = TM1 * rho[jf] * g(jf,Ii-1,ps);   	/* One spin down, same parity */
    xdo = TE1 * rho[jf] * g(jf,Ii-1,po);   	/* One spin down, opposite parity */
    if(Ii == 0) xms = 0.;							/* 0+ -> 0+ forbidden */
    xi = xus + xuo + xms + xmo + xds + xdo;  
    return xi;
}

float g(int jexc, float jspin, int jparity)
{
    float wparity, xx, xj = 0;
    if(jparity == 0) wparity = 0.5;
    if(jparity == 1) wparity = 0.5;
    xx = (jspin+0.5)*(jspin+0.5)/spincut[jexc];
    if( xx <= 12 && jspin >= 0){
        xj = wparity*((2.*jspin+1.)/spincut[jexc])*exp(-xx);
    }
    return xj/gsum(jexc);
}

float gsum(int jexc)
{
    float xx, sp, xj = 0.;
    for( sp = lowsp; sp < lowsp + 20.; sp++){
        xx = (sp+0.5)*(sp+0.5)/spincut[jexc];
        if( xx <= 12 && sp >= 0){
            xj = xj + ((2.*sp+1.)/spincut[jexc])*exp(-xx);
        }
    }
    return xj;
}
