#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>    
#include <math.h>    
#include <fcntl.h>						/* Header for open/read/write */ 
#include <errno.h> 
#include <sys/types.h>
#include <sys/ioctl.h>          
#define MAXDIM    8192					/* Max dimension of Ex spectra */
#define BN0       9.154					/* Default Bn of 96Mo */
#define PI        3.14159265358979		/* The pi number */

FILE	*fp;
char	line[1024],cdum[128];
float   dum;
int		iType = 1, dimx, dimy, dim = MAXDIM-1, NchBn = 0, L1, L2, ip=+1, iip=+1;
int		i, j, ii, jj, k;
int     l, l_min, l_max;

double	rho[MAXDIM], spincut[MAXDIM], parity[MAXDIM];
double  rhopaw[MAXDIM], rhox[MAXDIM], drhox[MAXDIM];
double  n[MAXDIM]={0.}, r[MAXDIM]={0.};

float	E_min = 4., E_max = 8., E_max0 = BN0, E_i = 8., E_f = 0., E_ix, E_g, Eg_max;
double	eps = 1.e-20, x;
double  rho_ex(double),ex,eg,xI,xxI,sum;
double rho_func(double , double , int );

static void fgets_ignore(char *s, int size, FILE *stream)
{
    // suppress braindead and presumtuous glibc 'security' warning
    if( !fgets(s, size, stream) )
        return;
}

/* Defining defaults values, taken from 96Mo */
float	Bn = BN0, BnOld, a0 = 60., a1 = 120.;
int main()
{
	printf("\n");
	printf("  ______________________________________________________________ \r\n");
	printf(" |                                                              |\r\n");
	printf(" |                        P J O L T E R  1.0                    |\r\n");
    printf(" |                                                              |\r\n");
    printf(" |      Program to estimate the Porter-Thomas fluctuations      |\r\n");
    printf(" |          of the gamma-ray strength function based on         |\r\n");
    printf(" |                      total level density                     |\r\n");
    printf(" |                                                              |\r\n");
    printf(" |       (The pjolter drink was very popular for the older      |\r\n");
    printf(" |   generation in Norway: usually whisky or brandy with soda.  |\r\n");
    printf(" |    Pjolter is an akronym for Porter-Thomas, which becomes    |\r\n");
    printf(" |         completely clear to you after a few pjolters.)       |\r\n");
  	printf(" |                                                              |\r\n");
	printf(" |  Input files: rhotmopaw.cnt    Output files: input.pjo       |\r\n");
    printf(" |               rhosp.rsg                      fluct.pjo       |\r\n");
    printf(" |               rhopaw.cnt                                     |\r\n");
	printf(" |               spincut.cnt                                    |\r\n");
    printf(" |               parity.mama                                    |\r\n");
    printf(" |               input.cnt                                      |\r\n");
	printf(" |               (input.pjo)                                    |\r\n");
	printf(" | E-mail  : magne.guttormsen@fys.uio.no                        |\r\n");
	printf(" | Created : 09 Jun 2016                                        |\r\n");
    printf(" | Modified: 09 Jun 2016 replace ? and deleting kumac files     |\r\n");
	printf(" |______________________________________________________________|\r\n");
	printf("                                                                 \r\n");
    printf("The program reads automatically files created by the counting program,\n");
    printf("however, a file called parity.mama has to be hand-made in some way.\n");
    printf("This file should give the probability of having parity = pluss.\n");
    printf("In the quasi-continuum you may use a value 0.5 (both parities equal probable).\n");
    printf("The energy calibration (a0, a1) should be as e.g. given by rhosp.rsg.\n");
    printf("\r\n");
    
    /* ************************************************** */
    /* Reading calibration and dimensions from: rhosp.rsg */
    /* ************************************************** */
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
		sscanf(line,"%s %d %s %d",cdum, &dimx, cdum, &dimy);
		fgets_ignore(line,sizeof(line),fp);
		fclose(fp);
	}
  	printf("Dimension (0 : %d, 0 : %d) and calibration (a0, a1) = (%8.2f,%8.3f)\n",dimx,dimy,a0,a1);
    
    /* ****************************************************************** */
    /* Reading data of experimental extended level density: rhotmopaw.cnt */
    /* ****************************************************************** */
    printf("\nReading data of experimental extended level density: rhotmopaw.cnt\n");
    fp = fopen("rhotmopaw.cnt", "r");
    if(fp == NULL){
        printf("No rhotmopaw.cnt file found in your directory\n");
        exit(0);
    }
    i = 0 ;
    while( i < dim){
        if(fgets(line,sizeof(line),fp) != NULL){sscanf(line,"%lf", &rho[i]);}
        i++;
    }
    fclose(fp);
    
    /* ***************************************************************** */
    /* Reading data and errors of experimental level density: rhopaw.cnt */
    /* ***************************************************************** */
    printf("Reading data and errors of experimental level density: rhopaw.cnt\n");
    fp = fopen("rhopaw.cnt", "r");
    if(fp == NULL){
        printf("No rhopaw.cnt file found in your directory\n");
        exit(0);
    }
    i = 0 ;
    while(i < MAXDIM && fgets(line,sizeof(line),fp) != NULL){   //*10, to be sure long enough search for data
        sscanf(line,"%lf", &rhopaw[i]);
        i++;
    }
    fclose(fp);
    dim = i/2;
    
    /* ************************************* */
    /* Making value and error vectors of rho */
    /* ************************************* */
    for(i = 0; i < dim; i++){
        rhox[i] = rhopaw[i];
        drhox[i]= rhopaw[i+dim];
    }
    
    /* **************************************************** */
    /* Finding L1 and L2 limits for region with data points */
    /* **************************************************** */
    L1 = 0.;
    L2 = dim;
    for(i = 0; i < dim; i++){
        if (rhopaw[i] > 10.*eps && L1 == 0) L1=i;
        if (rhopaw[i] < 10.*eps && i > dim/2 && L2 == dim ) L2=i-1;
    }
    
    /* ***************************************** */
    /* Reading default Bn from the counting run  */
    /* ***************************************** */
    fp = fopen("input.cnt", "r");
    if(fp == NULL){
        printf("\nCould not open file input.cnt, default Bn value is used \n");
    }
    else {
        fgets_ignore(line,sizeof(line),fp);
        sscanf(line, " %f %f %f %f %f \n", &dum, &dum, &BnOld, &dum, &dum);
        if(Bn == BN0)Bn = BnOld;
        fclose(fp);
    }
    
    /* ***************************************** */
	/* Reading default values from previous runs */
	/* ***************************************** */
	fp = fopen("input.pjo", "r");
	if(fp == NULL){
		printf("\nCould not open file input.pjo, default values are used \n");
	}
	else {
		fgets_ignore(line,sizeof(line),fp);
		sscanf(line, " %d \n", &iType);
        fgets_ignore(line,sizeof(line),fp);
        sscanf(line, " %f %f \n", &Bn, &E_max0);
        fgets_ignore(line,sizeof(line),fp);
        sscanf(line, " %f %f \n", &E_min, &E_max);
        fgets_ignore(line,sizeof(line),fp);
        sscanf(line, " %f \n", &E_i);
        fgets_ignore(line,sizeof(line),fp);
        sscanf(line, " %f \n", &E_f);
    }
		fclose(fp);
	
	/* *********************** */
	/* Asking for input values */
	/* *********************** */
	printf("Neutron or proton binding energy (Bn or Bp) (MeV)         <%6.3f>:",Bn);
	fgets_ignore(line,sizeof(line),stdin);
	sscanf(line,"%f", &Bn);
    
    /* **************************************** */
    /* Deciding how high in excitation energy   */
    /* **************************************** */
    printf("\nYou have to decide how high you go in excitation energy for Rho.\n");
    printf("If you want energies above your experimental data points,\n");
    printf("the CT or FG extention from counting is used.\n");
    printf("The experimental data go to max = %8.3f MeV\n",(a0+a1* (float)(L2))/1000.);
    printf("The Bn value is                 = %8.3f MeV\n", Bn);
    printf("Choose your maximum excitation energy <%6.3f>:",E_max0);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &E_max0);
    dim = 1 + (int)((((E_max0*1000.) - a0)/a1) +0.5);
    
    printf("\nCalculate fluctuations using:\n");
    printf("Region of initial excitation energies (standard Oslo method) (1)\n");
    printf("One fixed initial excitation energy (2)\n");
    printf("One fixed final excitation energy (3)\n");

    printf("Type your choice (1,2,3) <%d>:",iType);
	fgets_ignore(line,sizeof(line),stdin);
	sscanf(line,"%d", &iType);
    printf("\n");
    
		
    if(iType==1){
        printf("Lower excitation energy (MeV) <%7.3f>:",E_min);
        fgets_ignore(line,sizeof(line),stdin);
        sscanf(line,"%f", &E_min);
        E_min = (a0 + a1*(float)(int)(((E_min*1000. - a0)/a1)+0.5))/1000.;
        printf("Lower excitation energy is %7.3f MeV\n",E_min);
        
        printf("Upper excitation energy (MeV) <%7.3f>:",E_max);
        fgets_ignore(line,sizeof(line),stdin);
        sscanf(line,"%f", &E_max);
        E_max = (a0 + a1*(float)(int)(((E_max*1000. - a0)/a1)+0.5))/1000.;
        printf("Initial excitation energy is %7.3f MeV\n",E_max);
    }
    if(iType==2){
        printf("Initial excitation energy (MeV) <%7.3f>:",E_i);
        fgets_ignore(line,sizeof(line),stdin);
        sscanf(line,"%f", &E_i);
        E_i = (a0 + a1*(float)(int)(((E_i*1000. - a0)/a1)+0.5))/1000.;
        printf("Initial excitation energy is %7.3f MeV\n",E_i);
    }
    if(iType==3){
        printf("Final excitation energy (MeV) <%7.3f>:",E_f);
        fgets_ignore(line,sizeof(line),stdin);
        sscanf(line,"%f", &E_f);
        E_f = (a0 + a1*(float)(int)(((E_f*1000. - a0)/a1)+0.5))/1000.;
        printf("Final excitation energyis %7.3f MeV\n",E_f);
    }
    
	/* ****************************************** */
	/* Reading spincutoff function: spincut.cnt */
	/* ****************************************** */
	printf("Reading spincutoff function: spincut.cnt\n");
	fp = fopen("spincut.cnt", "r");
	if(fp == NULL){
		printf("No spincut.cnt file found in your directory\n");
		exit(0);
	}
	i = 0 ;
	while(i < dim){
      if(fgets(line,sizeof(line),fp) != NULL){sscanf(line,"%lf", &spincut[i]);}
		spincut[i]=2.*spincut[i]*spincut[i];
	   i++; 
	}
	fclose(fp);
	
    /* ****************************************** */
    /* Reading parity function: parity.mama */
    /* ****************************************** */
    printf("Reading parity function: parity.mama\n");
    fp = fopen("parity.mama", "r");
    if(fp == NULL){
        printf("No parity.mama file found in your directory\n");
        exit(0);
    }
    i = 0 ;
    while(i < dim){
        if(fgets(line,sizeof(line),fp) != NULL){sscanf(line,"%lf", &parity[i]);}
        i++;
    }
    fclose(fp);
    
    /* ************************ */
	/* Printing input functions */
	/* ************************ */
	printf(" \n No  Ex(keV) Rho_exp(1/MeV) Rho_ext(1/MeV)  2*Spincut**2  Parity   \n");
	for(i = 0 ; i < 1 + (int)((((Bn*1000.) - a0)/a1) +0.5); i++){
		printf("%3d  %6.1f  %10.3e       %10.3e      %8.2lf %8.2lf \n", i,a0+a1*(float)i,rhox[i],rho[i],spincut[i],parity[i]);
	}
    
	/* *************************************** */
	/* Calculating rho*rho and fluctuations    */
    /* FASTEN SEAT-BELTS                       */
	/* *************************************** */
    if(iType == 1){
        printf("\n\nInitial excitation energies in between %7.3f and %7.3f MeV\n",E_min,E_max);
        l_min = (int)(((E_min*1000. - a0)/a1)+0.5);
        l_max = (int)(((E_max*1000. - a0)/a1)+0.5);
        for(l = l_min; l < l_max+1; l++){
            E_ix = a0 + a1*(float)l;
            ex  = E_ix;
            for(k = 1+ (int)(((0.- a0)/a1)+0.5); k < 1+(int)(((E_ix - a0)/a1)+0.5); k++ ){
                eg = (double)(a0 + a1*(float)k);
                sum = 0.;
                for(i = 0; i < 20; i++){
                    xI = (double)i;
                    for(j = -1; j < 2; j=j+2){
                        ip = j;
                        for(ii = i-1; ii < i+2; ii++){
                            xxI = (double)ii;
                            for(jj = -1; jj < 2; jj = jj +2){
                                iip = jj;
                                if(xxI > 0.)n[k] = n[k] + rho_func(ex, xI, ip)*rho_func(ex-eg, xxI, iip);
                            }
                        }
                    }
                }
            }
        }
        
        Eg_max = E_max*1000.;
        for(k = 1+ (int)(((0.- a0)/a1)+0.5); k < 1+(int)(((Eg_max - a0)/a1)+0.5); k++ ){
            n[k] = (a1/1000.)*(a1/1000.)*n[k];
            if (n[k] > 0.) r[k] = sqrt(2/n[k]);
            printf(" k = %4d Eg = %7.1f   n(k) = %8.4lf r(k) = %8.4lf\n", k,a0 + a1*(float)k, n[k], r[k] );
        }
    }
    
    if(iType == 2){
        Eg_max = E_i*1000.;
        ex     = E_i*1000.;
        printf("\n\nInitial excitation energy is %7.3f MeV\n",E_i);
        for(k = 1 + (int)(((0.- a0)/a1)+0.5); k < 1+(int)(((Eg_max - a0)/a1)+0.5); k++ ){
            eg = (double)(a0 + a1*(float)k);
            sum = 0.;
            for(i = 0; i < 20; i++){
                xI = (double)i;
                for(j = -1; j < 2; j=j+2){
                    ip = j;
                    for(ii = i-1; ii < i+2; ii++){
                        xxI = (double)ii;
                        for(jj = -1; jj < 2; jj = jj +2){
                            iip = jj;
                            if(xxI > 0.)sum = sum + rho_func(ex, xI, ip)*rho_func(ex-eg, xxI, iip);
                        }
                    }
                }
            }
            n[k] = (a1/1000.)*(a1/1000.)*sum;
            if (n[k] > 0.) r[k] = sqrt(2/n[k]);
            printf(" k = %4d Eg = %7.1f   n(k) = %8.4lf r(k) = %8.4lf\n", k,a0 + a1*(float)k, n[k], r[k] );
        }
    }
    
    if(iType == 3){
        Eg_max = (Bn - E_f)*1000.;
        ex     = E_f*1000.;
        printf("\n\nFinal excitation energy is %7.3f MeV\n",E_f);
        for(k = 1+ (int)(((0.- a0)/a1)+0.5); k < (int)(((Eg_max - a0)/a1)+0.5); k++ ){
            eg = (double)(a0 + a1*(float)k);
            sum = 0.;
            for(i = 0; i < 20; i++){
                xI = (double)i;
                for(j = -1; j < 2; j=j+2){
                    ip = j;
                    for(ii = i-1; ii < i+2; ii++){
                        xxI = (double)ii;
                        for(jj = -1; jj < 2; jj = jj +2){
                            iip = jj;
                            if(xxI > 0.)sum = sum + rho_func(ex, xI, ip)*rho_func(ex+eg, xxI, iip);
                        }
                    }
                }
            }
            n[k] = (a1/1000.)*(a1/1000.)*sum;
            if (n[k] > 0.) r[k] = sqrt(2/n[k]);
            printf(" k = %4d Eg = %7.1f   n(k) = %8.4lf r(k) = %8.4lf\n", k,a0 + a1*(float)k, n[k], r[k] );
        }
    }
    

 	/* **************************************************** */
	/* Storing default values for the next run in input.pjo */
	/* **************************************************** */
	fp = fopen("input.pjo", "w");
	if(fp == NULL){
		printf("Could not open file input.pjo \n");
		exit(0);
	}
	else
    {
        fprintf(fp, " %d \n",    iType);
        fprintf(fp, " %f %f\n",  Bn, E_max0);
        fprintf(fp, " %f %f\n",  E_min, E_max);
        fprintf(fp, " %f \n",    E_i);
        fprintf(fp, " %f \n",    E_f);
    }
    fclose(fp);

	/* ************************************** */
	/* Fluctuations fluct(Eg) written to disk */
	/* ************************************** */
	fp = fopen("fluct.pjo", "w");

	if(fp == NULL){
		printf("Could not open file fluct.pjo \n");
		exit(0);
	}
	else
    {
        dim = (int)(((Eg_max - a0)/a1)+0.5);
        for (i = 0; i < 1 + dim; i++){
            fprintf(fp, "%9.1f  %14.7e \n", a0 + a1*(float)i, r[i]);
		}
	}
	fclose(fp);
	printf("File fluct.pjo (0:%d) written to disk, (a0,a1)=(%8.2f,%8.3f)\n",dim,a0,a1);
    return(0);
}

double rho_func(double ex, double xI, int ip)
{
    double rhotot, Prho, Pspin, Pparity;
    int iex = (int)(((ex - a0)/a1)+0.5);
    if(iex < 0)printf("Warning: negative argument in vector\n");
    if(xI < 0.)printf("Warning: negative spin\n");
    
    Prho = rho[iex];
    Pspin = (2.*xI+1)*exp(-(xI+0.5)*(xI+0.5)/spincut[iex])/spincut[iex];
    if (ip == +1)Pparity = parity[iex];
    if (ip == -1)Pparity = 1. - parity[iex];
    return rhotot = Prho*Pspin*Pparity;
}
