#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include <fcntl.h>                                              /* Header for open/read/write */ 
#include <errno.h>
#include <time.h>                                               /* Header for system time */
#include <sys/types.h> 
#include <sys/ioctl.h> 
#define MAXLEV          200                                     /* Max number of s.p. levels */
#define MAXNUPAIR         3                                     /* Max number of pairs broken */
#define MAXDIM         1000                                     /* Max channels of output spectrum, e.g. 10 MeV with 1keV/ch */
#define MAXORBITS        50                                     /* Max number of p or n orbits included in calculation */
#define MAXENERGY      20.0                                     /* Max energy Ex (MeV) */
#define MAXSPIN			 30

int     TotSum = 0;
int     TotBhd = 0;
int     np=42, nn=56;											/* Number of total protons and neutrons */
int     ip=0,  in=0;											/* Fermi level for protons and neutrons */
int     op=0,  on=0;											/* Odd-even system */
int     maxorb = MAXORBITS;										/* Maximum of orbits */
int     no_pairs=0;												/* Number of pairs broken and quasiparticles */
int     low_spin=0;												/* Lowest spin*/
int     high_spin=MAXSPIN;
int     r0, rmax=MAXSPIN;
float   epmin, epmax, enmin, enmax;                             /* Lower and upper q.p. energies taken into account */
int     ipmin, ipmax, inmin, inmax;								/* Lower and upper level taken into account */
int     npl=0, nnl=0;											/* Proton and neutron levels taken into account */
float   spp[MAXLEV], spn[MAXLEV];                               /* Nilsson s.p. energies (MeV) */
float   qpp[MAXLEV][MAXNUPAIR+1], qpn[MAXLEV][MAXNUPAIR+1];     /* Nilsson q.p. energies (MeV) */
int     pv[MAXLEV], nv[MAXLEV];									/* Vector with present number of particles per level */
int     pip[MAXLEV], pin[MAXLEV];								/* Storing parity of s.p. */
float   Omp[MAXLEV], Omn[MAXLEV];
float   Ks[MAXNUPAIR*2 + 2];
float   Sum_K[1024];

int     pi_gs, pigsp, pigsn;
int     pp, pn;													/* Parity for multi pr. and ne. configurations */
float   lamp, lamn;                                             /* Fermi levels lambda */
float   gsp, gsn, gs;                                           /* Absolute ground state energy */
float   Emax = MAXENERGY;                                       /* Max excitation energy */
float   max_Eqp = MAXENERGY;                                    /* Max energy above and below Fermi level */
float   max_Eqp_p, max_Eqp_n;
int     max_qp = 10;                                            /* Max number of quasiparticles */
int     max_qp_calc = 0;                                        /* Max number of quasiparticles calculated */
float   pEpair  = 2.0;                                          /* The 2*Delta proton pairing energy (MeV) */
float   nEpair  = 2.0;                                          /* The 2*Delta pairing neutron energy (MeV) */
float   pair_fac= 1.0;                                          /* Delta reduced by pair_fac for each broken pair */             
float   Ags     = 0.05;											/* Rotational parameter at ground state (MeV) */
float   Arigid  = 0.02;                                         /* Rigid rotational parameter at around E = Bn (MeV) */
float   Erigid  = 8.0;											/* At Ex = Erigid, the moment of inertia becomes rigid */
float   Evib_2  = 1.2;                                          /* Vibrationl phonon energy 2+ in evev-even nucleus */
float   Evib_3  = 1.9;                                          /* Vibrationl phonon energy 3- in evev-even nucleus */
float   disp    = 0.240;                                        /* Dispertion (ch/MeV) for output spectrum Spec(Ex) */
float   fwhm    = 0.100;                                        /* Omega_p x Omega_n interaction in MeV (50 keV) */
float   hbar_omega_p, hbar_omega_n;
float   I_gs, Omgsp, Omgsn;
float   sc_p = 1.0, sc_n = 1.0;
float   spin_fac;
float   e = 0., ep = 0., en = 0.;                               /* Total Ex is assigned e */
int		p1=0,p2=0,p3=0,p4=0,p5=0,p6=0,p7=0;						/* q.p. p-orbital i1, i2,...  */
int		n1=0,n2=0,n3=0,n4=0,n5=0,n6=0,n7=0;                     /* q.p. n-orbital i1, i2,...  */
int     iseed = 11;												/* Seed for random number generator*/

int     pi[MAXDIM];												/* Spectrum with eV bins and flags parity +/- */
int     orbitno;												/* Orbital number */

float   RhoLevel[MAXDIM]={0.}, RhoState[MAXDIM]={0.};
float   Npair[MAXDIM]={0.};
float   Npair_p[MAXDIM]={0.};
float   Npair_n[MAXDIM]={0.};
float   Parity[MAXDIM]={0.};
float   Orbitp[MAXDIM][2*MAXDIM];
float   Orbitn[MAXDIM][2*MAXDIM];
float	qp_p_energy[MAXDIM]={0.}, qp_n_energy[MAXDIM]={0.};
int		ch;
int     po[MAXNUPAIR*2+1]={0.};										/* Array with protons orbitals active at Ex */
int     no[MAXNUPAIR*2+1]={0.};										/* Array with neutron orbitals active at Ex */
int		sp_no_p[MAXLEV]={0.}, sp_no_n[MAXLEV]={0.};
float   SpinDis[MAXDIM][MAXSPIN+1];
int     iSpec;                                                  /* iSpec is dimension of Spec(Ex) spectrum */
int     Include = 0;


// BEGIN: Making 96Mo spins around Bn = 9.154 MeV, with dE = 0.0 - 0.010 MeV
float   States_Bn[MAXSPIN*2 + 2];
float   Bn      = 6.152;
float   ee, dee = 0.010;
// END: 96Mo


void    odd_n(float ep, float gs, int no_p);
void    even_n(float ep, float gs, int no_p);
void    find_po(int no_p);
void    Sum_Omegas(int kmax);   
int		no_p, no_n, no_qp;

float   sumv2(float e0,float sp0[],int nl0, float Epair);
float   lambda(int n0,float sp0[],int nl0, float Epair);
float   v2(float e0,float sp0[],int i0, float Epair);
int		ranint(float fwhm);
void    inc_vib_rot();
void    inc_qp_rot();
void    inc_gs_rot();
int     I_spin(float R, float K);
int		Rot(float R, float Eintr);
double  start, secs;
int		ihours, iminu, isec;

FILE    *fp, *fparam;

static void fgets_ignore(char *s, int size, FILE *stream)
{
    // suppress braindead and presumtuous glibc 'security' warning
    if( !fgets(s, size, stream) )
        return;
}

int main () 
{
	char    line[128];
    char    cdum[128];
    int     nosc[MAXLEV], iA, iZ,idum, jdum;
    float   eps2, eps4, kappa, mu, Amass;
    int     i, j, k;
    float   dum = 0., temp;

    printf("\n");
    printf("  ________________________________________ \r\n");
    printf(" |                                        |\r\n");
    printf(" |             C O M B I  1.6             |\r\n");
    printf(" |                                        |\r\n");
    printf(" |       Program to calculate NLD by      |\r\n");
    printf(" |       combining all ways to place      |\r\n");
    printf(" |        n neutrons and p protons        |\r\n");
    printf(" |   in the BCS quasi-particle schemes:   |\r\n");
    printf(" |          spn.dat and spp.dat           |\r\n");
    printf(" |                                        |\r\n");
    printf(" |      Rotations and vibrations are      |\r\n");
    printf(" |          added schematically           |\r\n");
    printf(" |                                        |\r\n");
    printf(" | E-mail  : magne.guttormsen@fys.uio.no  |\r\n");
    printf(" | Created : 26 Jul 2008                  |\r\n");
    printf(" | Modified: 08 Mar 2013                  |\r\n");
    printf(" |________________________________________|\r\n");
    printf("                                           \r\n");

    printf("Reading single proton energy levels:  spp.dat\n");
    fp = fopen("spp.dat", "r");
    if(fp == NULL){
        printf("No spp.dat file in your directory\n");
        exit(0);
    }
    fgets_ignore(line,sizeof(line),fp);
    sscanf(line,"%s %s %s %s %s %s %d %s %d ", cdum, cdum, cdum, cdum, cdum, cdum, &iA, cdum, &iZ);
    fgets_ignore(line,sizeof(line),fp);
    sscanf(line,"%s %f %s %f %s %f %s %f", cdum, &eps2, cdum, &eps4, cdum, &kappa, cdum, &mu);
    fgets_ignore(line,sizeof(line),fp);
    fgets_ignore(line,sizeof(line),fp);
    idum = 1;
    while( fgets(line,sizeof(line),fp) != NULL && npl < MAXLEV && idum > 0 ){
        sscanf(line,"%2d %3d %s %s %s %f", &nosc[npl], &jdum, cdum, cdum, cdum, &spp[npl]);
        idum =  nosc[npl];
        if(npl == 0) idum = 1;
        Omp[npl] = ((float)jdum)/2.;
        npl++; 
    }
    npl = npl - 1;
    for(i = 0; i < npl; i++){
        pip[i] = pow(-1,nosc[i]);
    }
    fclose(fp);
    printf("Proton Nilsson scheme:\n");
    printf("(A,Z)       = (%d,%d) \n",iA,iZ); 
    printf("(eps2,eps4) = (%f,%f) \n",eps2,eps4);
    printf("(kappa,mu)  = (%f,%f) \n",kappa,mu);    
        
    printf("\nReading single neutron energy levels: spn.dat\n");
    fp = fopen("spn.dat", "r");
    if(fp == NULL){
        printf("No spn.dat file in your directory\n");
        exit(0);
    }
    fgets_ignore(line,sizeof(line),fp);
    sscanf(line,"%s %s %s %s %s %s %d %s %d", cdum, cdum, cdum, cdum, cdum, cdum, &iA, cdum, &iZ);
    fgets_ignore(line,sizeof(line),fp);
    sscanf(line,"%s %f %s %f %s %f %s %f", cdum, &eps2, cdum, &eps4, cdum, &kappa, cdum, &mu);
    fgets_ignore(line,sizeof(line),fp);
    fgets_ignore(line,sizeof(line),fp);
    idum = 1;
    while( fgets(line,sizeof(line),fp) != NULL && nnl < MAXLEV && idum > 0 ){
        sscanf(line,"%2d %3d %s %s %s %f", &nosc[nnl], &jdum, cdum, cdum, cdum, &spn[nnl]);
        idum =  nosc[nnl];
        if(nnl == 0) idum = 1;          
        Omn[nnl] = ((float)jdum)/2.;
        nnl++; 
    } 
    nnl = nnl - 1;
    for(i = 0; i < nnl; i++){
        pin[i] = pow(-1,nosc[i]);
    }
    fclose(fp);
        
    printf("Neutron Nilsson scheme:\n");
    printf("(A,Z)       = (%d,%d) \n",iA,iZ); 
    printf("(eps2,eps4) = (%f,%f) \n",eps2,eps4);
    printf("(kappa,mu)  = (%f,%f) \n",kappa,mu);    
        
    for(j = 0; j < MAXNUPAIR*2+1; j++){
        no[j] = MAXORBITS;
    }
    for(j = 0; j < MAXNUPAIR*2+1; j++){
        po[j] = MAXORBITS;
    }

    fp = fopen("combiin.dat", "r");
    if(fp == NULL){
        printf("Could not open file combiin.dat, default values are used \n");
    }else{
        fgets_ignore(line,sizeof(line),fp);
        sscanf(line, " %d %d %f %f %f %f %f %f %f %f %f %f %d %f %d %f %d %d \n",  &np, &nn, &pEpair, &nEpair, &pair_fac, &Ags, &Evib_2, &Evib_3, &dum, &disp, &sc_p, &sc_n, &max_qp, &Emax, &Include, &Erigid, &low_spin, &high_spin);
        fclose(fp);
    }

   
    printf("\nNumber of protons:                     <%3d>",np);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%d", &np);
    printf(  "Number of neutrons:                    <%3d>", nn);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%d", &nn);
    printf("Scaling proton harm. osc. quant.:   <%6.3f>", sc_p);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &sc_p);
    for( i = 0; i < npl; i++){
        spp[i] = spp[i] * sc_p;
    }
    printf("Scaling neutron harm. osc. quant.:  <%6.3f>", sc_n);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &sc_n);
    for( i = 0; i < nnl; i++){
        spn[i] = spn[i] * sc_n;
    }
    printf("Proton pairing 2*Delta (MeV):       <%6.3f>", pEpair);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &pEpair);
    printf("Neutron pairing 2*Delta (MeV):      <%6.3f>", nEpair);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &nEpair);
    printf("Pairing reduction factor:           <%6.3f>", pair_fac);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &pair_fac);
    lamp = lambda(np, spp, npl, pEpair);
    printf("Proton Fermi level (MeV):           <%6.3f>", lamp);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &lamp);
    lamn = lambda(nn, spn, nnl, nEpair);
    printf("Neutron Fermi level (MeV):          <%6.3f>", lamn);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &lamn);
    printf("Rotational parameter at gs (MeV):   <%6.4f>", Ags);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &Ags);
    printf("Nucleus assumed rigid at Ex (MeV):  <%6.3f>", Erigid);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &Erigid);
    Amass  = (float)iA;
    Arigid = 5.*(197.3)*(197.3);
    Arigid = Arigid/(4.*Amass*939.*(1.2*pow(Amass, 0.3333))*(1.2*pow(Amass, 0.3333)));
    Arigid = Arigid/(1.+0.31*eps2);
    printf("Rigid rotational parameter (MeV):   <%6.4f>", Arigid);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &Arigid);
    printf("Include spin distribution (0=no,1=yes):  <%1d>", Include);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%d", &Include);
	
	if(Include==1){
		printf("Lowest spin:                            <%2d>", low_spin);
	    fgets_ignore(line,sizeof(line),stdin);
		sscanf(line,"%d", &low_spin);
		if(low_spin < 0)low_spin = 0;
	
		printf("Highest spin:                           <%2d>", high_spin);
		fgets_ignore(line,sizeof(line),stdin);
		sscanf(line,"%d", &high_spin);
		if(high_spin > MAXSPIN)high_spin = MAXSPIN;
   }else{
		low_spin = 0;
		high_spin = MAXSPIN;
   }
	
    printf("Energy of vibrational 2+(0+) state in e-e nucleus (MeV): <%6.3f>", Evib_2);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &Evib_2);
    printf("Energy of vibrational 3-(1-) state in e-e nucleus (MeV): <%6.3f>", Evib_3);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &Evib_3);
    printf("Maximum excitation energy (max = 20 MeV):                <%6.3f>", Emax);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &Emax);
    if(max_Eqp > Emax) max_Eqp = Emax;
    printf("Maximum q.p. energy (MeV):                               <%6.3f>", max_Eqp);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &max_Eqp);
    printf("Maximum of quasi-particles (max=10):                         <%2d>", max_qp);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%d", &max_qp);
    if(max_qp > 10) max_qp = 10;
    printf("Dispersion for final rho (MeV/ch):                       <%6.3f>", disp);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &disp);
        
    for( i = 0; i < npl; i++){
        pv[i] = 0;
        if (np >= 2*(i+1)) pv[i] = 2;       
        if (np == 2*i +1 ) pv[i] = 1;
        if (ip == 0 && (pv[i] != 2)) ip = i;
    }
    printf("\n"); 
    for( i = 0; i < nnl; i++){
        nv[i] = 0;
        if (nn >= 2*(i+1)) nv[i] = 2;       
        if (nn == 2*i +1 ) nv[i] = 1;
        if (in == 0 && (nv[i] != 2)) in = i;
    }
        
    iSpec = (int)(((float)Emax/disp) + 0.5);
    if(iSpec > MAXDIM-1){
        Emax  = (MAXDIM-1)*disp;
        iSpec = MAXDIM - 1;
        printf("Warning, Emax/disp exceeds 1000, Emax reduced to: %6.3f MeV\n",Emax);
    }
    rmax = (int)(((-1. + sqrt(1. + 4.*(Emax/Arigid)))/2.) + 0.5);
    if(rmax > high_spin){
        rmax = high_spin;
        printf("Warning, maximum rotational spin set to :%d\n",rmax);
    }
    iSpec = (int)(((float)Emax/disp) + 0.5);
    if(iSpec > MAXDIM-1){
        Emax  = (MAXDIM-1)*disp;
        iSpec = MAXDIM - 1;
        printf("Warning, Emax/disp exceeds 1000, Emax reduced to: %6.3f MeV\n",Emax);
    }
        
    /*      Finding Nilsson levels above and below the Fermi level */
    /*      that have to be included in order to have Ex < max_Eqp */
    op = 0;
    if(2*(np/2) != np) op = 1;
    on = 0;
    if(2*(nn/2) != nn) on = 1;
    if(op == 0 && on == 0){
        max_Eqp_p = Emax - pEpair;
        max_Eqp_n = Emax - nEpair;
    }   
    if(op == 1 && on == 0){
        max_Eqp_p = Emax;
        max_Eqp_n = Emax - nEpair;
    }   
    if(op == 0 && on == 1){
        max_Eqp_p = Emax - pEpair;
        max_Eqp_n = Emax;
    } 
    if(op == 1 && on == 1){
        max_Eqp_p = Emax;
        max_Eqp_n = Emax;
    } 
        
    if(max_Eqp_p > max_Eqp)
        max_Eqp_p = max_Eqp;
    if(max_Eqp_n > max_Eqp)
        max_Eqp_n = max_Eqp;

    epmin = lamp - max_Eqp_p;
    epmax = lamp + max_Eqp_p;
    enmin = lamn - max_Eqp_n;
    enmax = lamn + max_Eqp_n;
        
    ipmin = 0;
    ipmax = npl - 1;
    inmin = 0;
    inmax = nnl - 1;
    for(i = 0; i < npl; i++){
        if (ipmin == 0 && (spp[i] >= epmin))
            ipmin = i-1;
        if (ipmax == npl - 1 && (spp[i] >= epmax))
            ipmax = i;
    }
    for(i = 0; i < nnl; i++){
        if (inmin == 0       && (spn[i] >= enmin))
            inmin = i-1;
        if (inmax == nnl - 1 && (spn[i] >= enmax))
            inmax = i;
    }
    if (ipmin <= 0)
        ipmin = 0;
    if (inmin <= 0)
        inmin = 0;
                
    printf("Give lower and higher orbitals to be included\n");
    printf("Lower proton orbital:                   <%2d>", ipmin);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%d", &ipmin);
    printf("Higher proton orbital:                  <%2d>", ipmax);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%d", &ipmax);
    printf("Lower neutron orbital:                  <%2d>", inmin);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%d", &inmin);
    printf("Higher neutron orbital:                 <%2d>", inmax);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%d", &inmax);
    printf("Give seed number for random generator:  <%2d>", iseed);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%d", &iseed);
    srand48(iseed);
        
    printf("Proton  Fermi energy %8.4f MeV gives %8.4f protons\n", lamp, sumv2(lamp, spp, npl, pEpair));
    printf("Neutron Fermi energy %8.4f MeV gives %8.4f neutrons\n",lamn, sumv2(lamn, spn, nnl, nEpair));
        
    fparam = fopen("parameters.info", "w");
    if(fparam == NULL){
        printf("Could not open parameters.info \n");
        exit(0);
    }
    fprintf(fparam,"Number of protons:                       %3d\n",np);
    fprintf(fparam,"Number of neutrons:                      %3d\n", nn);
    fprintf(fparam,"Scaling proton harm. osc. quantum:    %6.3f\n", sc_p);
    fprintf(fparam,"Scaling neutron harm. osc. quantum:   %6.3f\n", sc_n);
    fprintf(fparam,"Proton pairing 2*Delta (MeV):         %6.3f\n", pEpair);
    fprintf(fparam,"Neutron pairing 2*Delta (MeV):        %6.3f\n", nEpair);
    fprintf(fparam,"Pairing reduction factor:             %6.3f\n", pair_fac);
    fprintf(fparam,"Proton Fermi level (MeV):             %6.3f\n", lamp);
    fprintf(fparam,"Neutron Fermi level (MeV):            %6.3f\n", lamn);
    fprintf(fparam,"Rotational parameter at gs (MeV):     %6.4f\n", Ags);
    fprintf(fparam,"Rigid rot. parameter at %6.3f (MeV): %6.4f\n", Erigid, Arigid);
    fprintf(fparam,"Spin distribution included (0=no,1=yes):   %1d\n", Include);
	fprintf(fparam,"Lowest spin:                             %3d\n", low_spin);
	fprintf(fparam,"Max spin:                                %3d\n", high_spin);
	fprintf(fparam,"Vibrational 2+ phonon quantum (MeV):  %6.3f\n", Evib_2);
    fprintf(fparam,"Vibrational 3- phonon quantum (MeV):  %6.3f\n", Evib_3);
    fprintf(fparam,"Maximum excitation energy (MeV):      %6.3f\n", Emax);
    fprintf(fparam,"Maximum q.p. energy (MeV):            %6.3f\n", max_Eqp);
    fprintf(fparam,"Maximum number of quasi-partices:         %2d\n", max_qp);
    fprintf(fparam,"Dispersion for final rho (MeV/ch):    %6.3f\n", disp);
    fprintf(fparam,"Lower proton orbital:                     %2d\n", ipmin);
    fprintf(fparam,"Higher proton orbital:                    %2d\n", ipmax);
    fprintf(fparam,"Lower neutron orbital:                    %2d\n", inmin);
    fprintf(fparam,"Higher neutron orbital:                   %2d\n", inmax);
    fprintf(fparam,"Seed number for random generator:         %2d\n", iseed);
    fprintf(fparam,"Proton  Fermi energy %8.4f MeV gives %8.4f protons\n", lamp, sumv2(lamp, spp, npl, pEpair));
    fprintf(fparam,"Neutron Fermi energy %8.4f MeV gives %8.4f neutrons\n",lamn, sumv2(lamn, spn, nnl, nEpair));

    printf("\n");
    npl = ipmax - ipmin + 1;
    for(i = 0; i < npl; i++){
        spp[i]  = spp[i+ipmin];
        pip[i]  = pip[i+ipmin];
        Omp[i]  = Omp[i+ipmin];
    }
    gsp = 10000.;
    for (j = 0; j < MAXNUPAIR + 1; j++){    /* Making q.p. energies for 0, 1, 2, 3,... pairs broken */
        for(i = 0; i < npl; i++){
            qpp[i][j]  = sqrt((spp[i]-lamp)*(spp[i]-lamp) + ((pow(pair_fac,j)*pEpair/2.0))*((pow(pair_fac,j)*pEpair/2.0)));
            if(qpp[i][j] < gsp && j==0){
                gsp = qpp[i][j];                        /* Lowest Ex for protons (g.s.) */
                pigsp = pip[i];                         /* Parity for lowest proton     */
                Omgsp = Omp[i];                         /* Omega for lowest proton      */
            }
            if(j==0) printf(        "%2d Proton  s.p. %4.1f %2d %6.3f MeV, q.p. %6.3f MeV, v2 = %6.3f\n",
                                    i, Omp[i], pip[i], spp[i], qpp[i][j], v2(lamp, spp, i, pEpair));
            if(j==0) fprintf(fparam,"%2d Proton  s.p. %4.1f %2d %6.3f MeV, q.p. %6.3f MeV, v2 = %6.3f\n",
                             i, Omp[i], pip[i], spp[i], qpp[i][j], v2(lamp, spp, i, pEpair));
			qp_p_energy[i] = qpp[i][0];
			if (v2(lamp, spp, i, pEpair) > 1.) {
				qp_p_energy[i] = - qp_p_energy[i];
			}
        }
    }
	
	for (i = 0; i < npl; i++){
		sp_no_p[i] = i;
	}
    for (j = 0; j < npl; j++){
        dum = qpp[j][0];
        for (i = j+1; i < npl; i++){
            if( qpp[i][0] < dum){
                for (k = 0; k < MAXNUPAIR + 1; k++){
                    temp            = qpp[j][k];
                    qpp[j][k]       = qpp[i][k];
                    qpp[i][k]       = temp;
                }
                temp    = pip[j];
                pip[j]  = pip[i];
                pip[i]  = temp;
				
                temp    = Omp[j];
                Omp[j]  = Omp[i];
                Omp[i]  = temp;
				
				temp	    = sp_no_p[j];
                sp_no_p[j]  = sp_no_p[i];
                sp_no_p[i]  = temp;
				
                dum		= qpp[j][0];
            }
        }
    }
    printf("Proton orbitals sorted by energies and 0, 1, 2, 3,... pairs:\n");
    for (i = 0; i < npl; i++){
        printf(        "%3d(%3d) %4.1f %2d %9.3f %9.3f %9.3f %9.3f \n", i, sp_no_p[i], Omp[i], pip[i], qpp[i][0], qpp[i][1], qpp[i][2], qpp[i][3]);
        fprintf(fparam,"%3d(%3d) %4.1f %2d %9.3f %9.3f %9.3f %9.3f \n", i, sp_no_p[i], Omp[i], pip[i], qpp[i][0], qpp[i][1], qpp[i][2], qpp[i][3]);
    }

    printf("\n");
    nnl = inmax - inmin + 1;
    for(i = 0; i < nnl; i++){
        spn[i]  = spn[i+inmin];
        pin[i]  = pin[i+inmin];
        Omn[i]  = Omn[i+inmin];
    }
    gsn = 10000.;
    for (j = 0; j < MAXNUPAIR + 1; j++){    /* Making q.p. energies for 0, 1, 2, 3,... pairs broken */
        for(i = 0; i < nnl; i++){
            qpn[i][j]  = sqrt((spn[i]-lamn)*(spn[i]-lamn) + ((pow(pair_fac,j)*nEpair/2.0)*(pow(pair_fac,j)*nEpair/2.0)));
            if(qpn[i][j] < gsn && j==0){
                gsn = qpn[i][j];                        /* Lowest Ex for neutrons (g.s.) */
                pigsn = pin[i];                         /* Parity for lowest neutron     */
                Omgsn = Omn[i];                         /* Omega for lowest neutron      */
            }
            if(j==0) printf(        "%2d Neutron s.p. %4.1f %2d %6.3f MeV, q.p. %6.3f MeV, v2 = %6.3f\n",
									i, Omn[i], pin[i], spn[i], qpn[i][j], v2(lamn, spn, i, nEpair));
            if(j==0) fprintf(fparam,"%2d Neutron s.p. %4.1f %2d %6.3f MeV, q.p. %6.3f MeV, v2 = %6.3f\n",
									i, Omn[i], pin[i], spn[i], qpn[i][j], v2(lamn, spn, i, nEpair));
			qp_n_energy[i] = qpn[i][0];
			if (v2(lamn, spn, i, nEpair) > 1.) {
				qp_n_energy[i] = - qp_n_energy[i];
			}
        }
    }
	
	for (i = 0; i < nnl; i++){
		sp_no_n[i] = i;
	}
    for (j = 0; j < nnl; j++){
        dum = qpn[j][0];
        for (i = j+1; i < nnl; i++){
            if( qpn[i][0] < dum){
                for (k = 0; k < MAXNUPAIR + 1; k++){
                    temp            = qpn[j][k];
                    qpn[j][k]       = qpn[i][k];
                    qpn[i][k]       = temp;
                }
                temp    = pin[j];
                pin[j]  = pin[i];
                pin[i]  = temp;
				
                temp    = Omn[j];
                Omn[j]  = Omn[i];
                Omn[i]  = temp;
				
				temp	    = sp_no_n[j];
                sp_no_n[j]  = sp_no_n[i];
                sp_no_n[i]  = temp;
				
				dum     = qpn[j][0];
            }
        }
    }
    printf("Neutron orbitals sorted by energies and 0, 1, 2, 3,... pairs:\n");
    for (i = 0; i < nnl; i++){
        printf(        "%3d(%3d) %4.1f %2d %9.3f %9.3f %9.3f %9.3f \n", i, sp_no_n[i], Omn[i], pin[i], qpn[i][0], qpn[i][1], qpn[i][2], qpn[i][3]);
        fprintf(fparam,"%3d(%3d) %4.1f %2d %9.3f %9.3f %9.3f %9.3f \n", i, sp_no_n[i], Omn[i], pin[i], qpn[i][0], qpn[i][1], qpn[i][2], qpn[i][3]);
    }
        
    /* Finding basics for ground state: energy, parity and spin */
    if((op == 0 && on == 0)) gs = 0;
    if((op == 1 && on == 0)) gs = gsp;
    if((op == 0 && on == 1)) gs = gsn;
    if((op == 1 && on == 1)) gs = gsp + gsn;        
        
    if((op == 0 && on == 0)) pi_gs = 1;
    if((op == 1 && on == 0)) pi_gs = pigsp;
    if((op == 0 && on == 1)) pi_gs = pigsn;
    if((op == 1 && on == 1)) pi_gs = pigsp * pigsn;
        
    if((op == 0 && on == 0)) I_gs = 0;
    if((op == 1 && on == 0)) I_gs = Omgsp;
    if((op == 0 && on == 1)) I_gs = Omgsn;
    if((op == 1 && on == 1)) I_gs = Omgsp + Omgsn;
        
    spin_fac = 1.;					/* odd-odd or even-even nucleus, integer spins */
    if((op == 1 && on == 0) ||(op == 0 && on == 1) )
        spin_fac = 2.;				/* odd mass nucleus, half-integer spins */
    r0 = 1;							/* not ee nucleus, then spin steps of 1 */
    if(op == 0 && on == 0)
        r0 = 2;						/* ee nucleus, then spin steps of 2 */
        
    printf("Type proton (%d), neutron (%d) and spin-factor =    %3.0f\n", op, on, spin_fac);
    printf("Ground-state energy = %6.3f, spin = %4.1f, parity =%2d\n", gs, I_gs, pi_gs);
    printf("Steps in spin =                                   %3d\n", r0);
    printf("Length of spectrum =                              %3d\n", iSpec);
    printf("Maximum of orbitals =                             %3d\n", maxorb);
    printf("Lowest spins =                                    %3d\n", low_spin);
	printf("Highest spins =                                   %3d\n", high_spin);

    fprintf(fparam,"Type proton (%d), neutron (%d) and spin-factor =    %3.0f\n", op, on, spin_fac);
    fprintf(fparam,"Ground-state energy = %6.4f, spin = %4.1f, parity =%2d\n", gs, I_gs, pi_gs);
    fprintf(fparam,"Steps in spin =                                   %3d\n", r0);
    fprintf(fparam,"Length of spectrum =                              %3d\n", iSpec);
    fprintf(fparam,"Maximum of orbitals =                             %3d\n", maxorb);
	fprintf(fparam,"Lowest spins =                                    %3d\n", low_spin);
	fprintf(fparam,"Highest spins =                                   %3d\n", high_spin);
	
	
    /* ******************** */
    /*                      */
    /*  Fasten seat belts   */
    /*                      */
    /* ******************** */
    start = (double)clock();
        
    /* ********************************************************     */
    /* Adding quadrupole and octupole vibrational band-heads        */
    /* and rotations on each band-head                              */
    /* ********************************************************     */
    inc_vib_rot();

    /* ********************************************************     */
    /* Looping through all proton and neutron orbitals in the       */
    /* sequence of increasing qp-energies. This is the CPU          */
    /* intensive part of the calculation                            */
    /* ********************************************************     */
        
    /* *************** */
    /* odd-odd nucleus */
    /* *************** */
    if(op == 1 && on == 1){                         
        for (p1 = 0; p1 < npl && ep < Emax; p1++){
            printf("Now proton orbital%3d and all its combinations ...\n",p1);
            fflush(stdout);
            ep = qpp[p1][0];
            pp = pip[p1];
            no_p = 1;
            odd_n(ep,gs,no_p);
            for (p2 = p1 + 1; p2 < npl && ep < Emax; p2++){
                for (p3 = p2 + 1; p3 < npl && ep < Emax; p3++){
                    ep = qpp[p1][1] + qpp[p2][1] + qpp[p3][1];
                    pp = pip[p1]    * pip[p2]    * pip[p3];
                    no_p = 3;
                    odd_n(ep,gs,no_p);
                    for (p4 = p3 + 1; p4 < npl && ep < Emax; p4++){
                        for (p5 = p4 + 1; p5 < npl && ep < Emax; p5++){
                            ep = qpp[p1][2] + qpp[p2][2] + qpp[p3][2] + qpp[p4][2] + qpp[p5][2];
                            pp = pip[p1]    * pip[p2]    * pip[p3]    * pip[p4]    * pip[p5];
                            no_p = 5;
                            odd_n(ep,gs,no_p);
                            for (p6 = p5 + 1; p6 < npl && ep < Emax; p6++){
                                for (p7 = p6 + 1; p7 < npl && ep < Emax; p7++){
                                    ep = qpp[p1][3] + qpp[p2][3] + qpp[p3][3] + qpp[p4][3] + qpp[p5][3] + qpp[p6][3] + qpp[p7][3];
                                    pp = pip[p1]    * pip[p2]    * pip[p3]    * pip[p4]    * pip[p5]    * pip[p6]    * pip[p7];
                                    no_p = 7;
                                    odd_n(ep,gs,no_p);
                                }
                                ep = 0.;
                            }
                            ep = 0.;
                        }
                        ep = 0.;
                    }
                    ep = 0.;
                }
                ep = 0.;
            }
            ep = 0.;        
        }
        ep = 0.;
    }
        
    /* **************** */
    /* odd-even nucleus */
    /* **************** */
    if(op == 1 && on == 0){
        for (p1 = 0; p1 < npl && ep < Emax; p1++){
            printf("Now proton orbital%3d and all its combinations ...\n",p1);
            fflush(stdout);
            ep = qpp[p1][0];
            pp = pip[p1];
            no_p = 1;
            even_n(ep,gs,no_p);
            for (p2 = p1 + 1; p2 < npl && ep < Emax; p2++){
                for (p3 = p2 + 1; p3 < npl && ep < Emax; p3++){
                    ep = qpp[p1][1] + qpp[p2][1] + qpp[p3][1];
                    pp = pip[p1]    * pip[p2]    * pip[p3];
                    no_p = 3;
                    even_n(ep,gs,no_p);
                    for (p4 = p3 + 1; p4 < npl && ep < Emax; p4++){
                        for (p5 = p4 + 1; p5 < npl && ep < Emax; p5++){
                            ep = qpp[p1][2] + qpp[p2][2] + qpp[p3][2] + qpp[p4][2] + qpp[p5][2];
                            pp = pip[p1]    * pip[p2]    * pip[p3]    * pip[p4]    * pip[p5];
                            no_p = 5;
                            even_n(ep,gs,no_p);
                            for (p6 = p5 + 1; p6 < npl && ep < Emax; p6++){
                                for (p7 = p6 + 1; p7 < npl && ep < Emax; p7++){
                                    ep = qpp[p1][3] + qpp[p2][3] + qpp[p3][3] + qpp[p4][3] + qpp[p5][3] + qpp[p6][3] + qpp[p7][3];
                                    pp = pip[p1]    * pip[p2]    * pip[p3]    * pip[p4]    * pip[p5]    * pip[p6]    * pip[p7];
                                    no_p = 7;
                                    even_n(ep,gs,no_p);
                                }
                                ep = 0.;
                            }
                            ep = 0.;
                        }
                        ep = 0.;
                    }
                    ep = 0.;
                }
                ep = 0.;
            }
            ep = 0.;        
        }
        ep = 0.;
    }

    /* **************** */
    /* even-odd nucleus */
    /* **************** */
    if(op == 0 && on == 1){
        ep   = 0;
        pp   = 1;
        no_p = 0;
        odd_n(ep,gs,no_p);
        for (p1 = 0; p1 < npl && ep < Emax; p1++){
            printf("Now proton orbital%3d and all its combinations ...\n",p1);
            fflush(stdout);
            for (p2 = p1 + 1; p2 < npl && ep < Emax; p2++){
                ep = qpp[p1][1] + qpp[p2][1];
                pp = pip[p1]    * pip[p2];
                no_p = 2;
                odd_n(ep,gs,no_p);
                for (p3 = p2 + 1; p3 < npl && ep < Emax; p3++){
                    for (p4 = p3 + 1; p4 < npl && ep < Emax; p4++){
                        ep = qpp[p1][2] + qpp[p2][2] + qpp[p3][2] + qpp[p4][2];
                        pp = pip[p1]    * pip[p2]    * pip[p3]    * pip[p4];
                        no_p = 4;
                        odd_n(ep,gs,no_p);
                        for (p5 = p4 + 1; p5 < npl && ep < Emax; p5++){
                            for (p6 = p5 + 1; p6 < npl && ep < Emax; p6++){
                                ep = qpp[p1][3] + qpp[p2][3] + qpp[p3][3] + qpp[p4][3] + qpp[p5][3] + qpp[p6][3];
                                pp = pip[p1]    * pip[p2]    * pip[p3]    * pip[p4]    * pip[p5]    * pip[p6];
                                no_p = 6;
                                odd_n(ep,gs,no_p);
                            }
                            ep = 0.;
                        }
                        ep = 0.;
                    }
                    ep = 0.;
                }
                ep = 0.;
            }
            ep = 0.;
        }
        ep = 0.;
    }

    /* ***************** */
    /* even-even nucleus */
    /* ***************** */
    if(op == 0 && on == 0){
        ep   = 0; /* defining the gs */
        en   = 0;
        e    = gs;
        no_p = 0;
        no_n = 0;
        pp   = 1;
        inc_gs_rot();

        printf("Now including no proton orbitals...\n");
        even_n(ep,gs,no_p);

        for (p1 = 0; p1 < npl && ep < Emax; p1++){
            printf("Now proton orbital%3d and all its combinations ...\n",p1);
            fflush(stdout);
            for (p2 = p1 + 1; p2 < npl && ep < Emax; p2++){
                ep = qpp[p1][1] + qpp[p2][1];
                pp = pip[p1]    * pip[p2];
                no_p = 2;
                even_n(ep,gs,no_p);
                for (p3 = p2 + 1; p3 < npl && ep < Emax; p3++){
                    for (p4 = p3 + 1; p4 < npl && ep < Emax; p4++){
                        ep = qpp[p1][2] + qpp[p2][2] + qpp[p3][2] + qpp[p4][2];
                        pp = pip[p1]    * pip[p2]    * pip[p3]    * pip[p4];
                        no_p = 4;
                        even_n(ep,gs,no_p);
                        for (p5 = p4 + 1; p5 < npl && ep < Emax; p5++){
                            for (p6 = p5 + 1; p6 < npl && ep < Emax; p6++){
                                ep = qpp[p1][3] + qpp[p2][3] + qpp[p3][3] + qpp[p4][3] + qpp[p5][3] + qpp[p6][3];
                                pp = pip[p1]    * pip[p2]    * pip[p3]    * pip[p4]    * pip[p5]    * pip[p6];
                                no_p = 6;
                                even_n(ep,gs,no_p);
                            }
                            ep = 0.;
                        }
                        ep = 0.;
                    }
                    ep = 0.;
                }
                ep = 0.;
            }
            ep = 0.;
        }
        ep = 0.;
    }
    /* ************************ */
    /*                          */
    /*   Take off seat belts    */
    /*                          */
    /* ************************ */

    for( i = 0; i < iSpec; i++){                                                            
        if(RhoLevel[i] > 0) Npair[i]        = Npair[i]/RhoLevel[i];             /* Making average pairs broken */
        if(RhoLevel[i] > 0) Npair_p[i]		= Npair_p[i]/RhoLevel[i];           /* Making average pairs broken */
        if(RhoLevel[i] > 0) Npair_n[i]		= Npair_n[i]/RhoLevel[i];           /* Making average pairs broken */
        if(RhoLevel[i] > 0) Parity[i]       = Parity[i]/RhoLevel[i];            /* Making average parity */
        for( j = 0; j <= high_spin; j++){
            if(RhoLevel[i] > 0) SpinDis[i][j] = SpinDis[i][j]/RhoLevel[i];
        }
        for( j = 0; j < MAXORBITS; j++){
            if(RhoLevel[i] > 0) Orbitp[i][j] = Orbitp[i][j];//RhoLevel[i];		/* Making average protons */
            if(RhoLevel[i] > 0) Orbitn[i][j] = Orbitn[i][j];//RhoLevel[i];		/* Making average neutrons */
        }
        TotSum   = TotSum + (int)RhoLevel[i];                               /* Total number of levels */
        RhoLevel[i]  = RhoLevel[i] / disp;                                      /* Normalize to number of levels per MeV */
		RhoState[i]  = RhoState[i] / disp;                                      /* Normalize to number of levels per MeV */
    }
                                
    fp = fopen("combiin.dat", "w");
    if(fp == NULL){
        printf("Could not open file combiin.dat \n");
        exit(0);
    } else {
        fprintf(fp," %d %d %f %f %f %f %f %f %f %f %f %f %d %f %d %f %d %d\n",
                np, nn, pEpair, nEpair, pair_fac, Ags, Evib_2, Evib_3, dum, disp, sc_p, sc_n, max_qp, Emax, Include, Erigid, low_spin, high_spin);
    }
    fclose(fp);

    fp = fopen("RhoLevel.paw", "w");
    if(fp == NULL){
        printf("Could not open RhoLevel.paw \n");
        exit(0);
    }
	for( i = 0; i < iSpec; i++){
        fprintf(fp," %f\n", RhoLevel[i]);
    }
    fclose(fp);
	
	fp = fopen("RhoState.paw", "w");
    if(fp == NULL){
        printf("Could not open RhoState.paw \n");
        exit(0);
    }
	for( i = 0; i < iSpec; i++){
        fprintf(fp," %f\n", RhoState[i]);
    }
    fclose(fp);

        
    fp = fopen("pairs.paw", "w");
    if(fp == NULL){
        printf("Could not open pairs.paw \n");
        exit(0);
    }
    for( i = 0; i < iSpec; i++){
        fprintf(fp," %f\n", Npair[i]);
    }
    fclose(fp);
        
    fp = fopen("pairs_p.paw", "w");
    if(fp == NULL){
        printf("Could not open pairs_p.paw \n");
        exit(0);
    }
    for( i = 0; i < iSpec; i++){
        fprintf(fp," %f\n", Npair_p[i]);
    }
    fclose(fp);
        
    fp = fopen("pairs_n.paw", "w");
    if(fp == NULL){
        printf("Could not open pairs_n.paw \n");
        exit(0);
    }
    for( i = 0; i < iSpec; i++){
        fprintf(fp," %f\n", Npair_n[i]);
    }
    fclose(fp);
        
    fp = fopen("parity.paw", "w");
    if(fp == NULL){
        printf("Could not open parity.paw \n");
        exit(0);
    }
    for( i = 0; i < iSpec; i++){
        fprintf(fp," %f\n", Parity[i]);
    }
    fclose(fp);
        
    fp = fopen("spindis.paw", "w");
    if(fp == NULL){
        printf("Could not open spindis.paw \n");
        exit(0);
    }
    for( i = 0; i < iSpec; i++){
        for( j = 0; j <= MAXSPIN; j++){
            fprintf(fp," %f\n", SpinDis[i][j]);
        }
    }
    fclose(fp);
	
	fp = fopen("qp_p_energy.paw", "w");
    if(fp == NULL){
        printf("Could not open qp_p_energy.paw \n");
        exit(0);
    }
    for( i = 0; i < npl; i++){
        fprintf(fp," %f\n", qp_p_energy[i]);
    }
    fclose(fp);
	
	fp = fopen("qp_n_energy.paw", "w");
    if(fp == NULL){
        printf("Could not open qp_n_energy.paw \n");
        exit(0);
    }
    for( i = 0; i < nnl; i++){
        fprintf(fp," %f\n", qp_n_energy[i]);
    }
    fclose(fp);

    fp = fopen("orbitp.paw", "w");
    if(fp == NULL){
        printf("Could not open orbitp.paw \n");
        exit(0);
    }
    for( i = 0; i < iSpec; i++){
        for( j = 0; j < 2*iSpec; j++){
            fprintf(fp," %f\n", Orbitp[i][j]);
        }
    }
    fclose(fp);
        
    fp = fopen("orbitn.paw", "w");
    if(fp == NULL){
        printf("Could not open orbitn.paw \n");
        exit(0);
    }
    for( i = 0; i < iSpec; i++){
        for( j = 0; j < 2*iSpec; j++){
            fprintf(fp," %f\n", Orbitn[i][j]);
        }
    }
    fclose(fp);
    
    
// BEGIN: Making 96Mo spins around Bn = 9.154 MeV, with dE = 0.0 - 0.100 MeV
    fp = fopen("states_bn.paw", "w");
    if(fp == NULL){
        printf("Could not open states_bn.paw \n");
        exit(0);
    }
    fprintf(fp," Level density estimated from Ex = %8.5f -> %8.5f MeV\n", Bn, Bn+dee);
    fprintf(fp," Spin/Parity Levels Levels/MeV\n");
    for( i = 0; i < MAXSPIN; i++){
        fprintf(fp,"    %2d-  %8.0f  %8.1f\n", i, States_Bn[2*i], States_Bn[2*i]*(1./dee));
        fprintf(fp,"    %2d+  %8.0f  %8.1f\n", i, States_Bn[2*i+1], States_Bn[2*i+1]*(1./dee));
    }
    fclose(fp);
    
// END: Making 96Mo



    secs=((double)clock()-start)/(double)CLOCKS_PER_SEC;
    ihours  = secs/3600;
    iminu   = (secs -ihours*3600)/60;
    isec    = (secs -ihours*3600 - iminu*60);
	i = MAXSPIN+1;
        
    printf("\n\nTotal number of levels =%12d \n",TotSum);
    printf("Number of band heads   =%12d \n",TotBhd);
    printf("Spectra length =                %4d \n",iSpec);
	printf("Lowest spins =                   %3d\n",low_spin);
	printf("Highest spins =                  %3d\n",high_spin);
    printf("Maximum number of qp requested = %3d \n",max_qp);
    printf("Maximum number of qp found =     %3d \n",max_qp_calc);
    printf("You may consider changing max qp in next run\n");
	printf("Number of proton and neutron orbitals are:          %d, %d \n", npl, nnl); 
	printf("Matrix spindis.paw has dimensions:                  %d x %d \n", i, iSpec);
    printf("Matrices orbitp.paw and orbitn.paw have dimensions: %d x %d \n", 2*iSpec, iSpec);
	printf("and with calibrations ay0=0, ay1=%f, ax0=%f, ax1=%f \n", disp, -Emax, disp);
    printf("Time elapsed: %dhours %dmin %dsec\n",ihours, iminu, isec);
        
    fprintf(fparam,"\n\nTotal number of levels =%12d \n",TotSum);
    fprintf(fparam,"Number of band heads   =%12d \n",TotBhd);
    fprintf(fparam,"Spectra length =                %4d \n",iSpec);
	fprintf(fparam,"Lowest spins =                   %3d\n",low_spin);
	fprintf(fparam,"Highest spins =                  %3d\n",high_spin);
	fprintf(fparam,"Maximum number of qp requested = %3d \n",max_qp);
    fprintf(fparam,"Maximum number of qp found =     %3d \n",max_qp_calc);
    fprintf(fparam,"You may consider changing max qp in next run\n");
	fprintf(fparam,"Number of proton and neutron orbitals are:         %d, %d \n", npl, nnl); 
	fprintf(fparam,"Matrix spindis.paw has dimensions:                  %d x %d \n", i, iSpec);
    fprintf(fparam,"Matrices orbitp.paw and orbitn.paw have dimensions: %d x %d \n", 2*iSpec, iSpec);
	fprintf(fparam,"and with calibrations ay0=0, ay1=%f, ax0=%f, ax1=%f \n", disp, -Emax, disp);
    fprintf(fparam,"Time elapsed: %dhours %dmin %dsec\n",ihours, iminu, isec);
    fclose(fparam);

    return 0;
}

float lambda(int n0,float sp0[],int nl0,float Epair)
{
    int i0, l0=0, pv0, it0;
    float e1, e2, n1, n2, ratio, enew;
    for( i0 = 0; i0 < nl0; i0++){
        pv0 = 0;
        if (n0 >= 2*(i0+1)) pv0 = 2;       
        if (n0 == 2*i0 +1 ) pv0 = 1;
        if (l0 == 0 && (pv0 != 2)) l0 = i0; /*Rough estimate of lambda*/
    }
    e2 = sp0[l0];
    e1 = e2 + 1.;
    ratio = 1.;
    for( it0 = 0; it0 < 100; it0++){
        while(ratio > 0.0001){
            n1 = sumv2(e1,sp0,nl0,Epair);
            n2 = sumv2(e2,sp0,nl0,Epair);
            enew = e1 +((e2-e1)/(n2-n1))*(n0-n1);
            e1 = e2;
            e2 = enew;
            ratio =fabs(n0-n2)/n0;
        }
    }
    return e2;
}

float sumv2(float e0,float sp0[],int nl0,float Epair)
{
    int i0;
    float s = 0.;
    for( i0 = 0; i0 < nl0; i0++){
        s = s + v2(e0,sp0,i0,Epair);
    }
    return s;
}

float v2(float e0,float sp0[],int i0,float Epair)
{
    float eq = 0., v = 0.;
    eq = sqrt((sp0[i0]-e0)*(sp0[i0]-e0)+(Epair*Epair/4.0));
    v = (1.-(sp0[i0]-e0)/eq);
    return v;
}

int ranint(float fwhm)
{
    /* Inverting the monoton increasing function r=F(z) -> z=Finv(r) */
    /* This means to find the z-value giving the value rand */
    /* The function F is the cummulative Gauss function F=1/2(1+erf(z/sqrt(2)))*/
    float r,xl,xh,yl,yh,x,y;
    int ix;
    r  = drand48();
    xl =-3.0;
    xh = 3.0;
    x  = 0.0;
    yl = 0.0;
    yh = 1.0;
    y  = 0.5;
    while (fabs(y-r) > 0.001){
        x = xl + (xh-xl)*(r-yl)/(yh-yl);
        y = 0.5*(1.+erf(x/1.414213562));
        if(y > r){
            yl=y;
            xl=x;
        }
        else{
            yh=y;
            xh=x;
        }
    }
    ix=(int)(x*fwhm + 0.5);
    return ix;
}

void odd_n(float ep, float gs, int no_p)
{
    for (n1 = 0; n1 < nnl && ep + en - gs < Emax && no_p + 1 <= max_qp; n1++){
        en = qpn[n1][0];        
        pn = pin[n1];
        e  = ep + en - gs;
        find_po(no_p);
        no_n = 1;
        no[0] = n1;
        inc_qp_rot();
        for (n2 = n1 + 1; n2 < nnl && ep + en - gs < Emax && no_p + 3 <= max_qp; n2++){
            for (n3 = n2 + 1; n3 < nnl && ep + en - gs < Emax; n3++){
                en = qpn[n1][1] + qpn[n2][1] + qpn[n3][1];
                pn = pin[n1]    * pin[n2]    * pin[n3];
                e = ep + en - gs;
                find_po(no_p);
                no_n = 3;
                no[0] = n1;
                no[1] = n2;
                no[2] = n3;
                inc_qp_rot();
                for (n4 = n3 + 1; n4 < nnl && ep + en - gs < Emax && no_p + 5 <= max_qp; n4++){
                    for (n5 = n4 + 1; n5 < nnl && ep + en - gs < Emax; n5++){
                        en = qpn[n1][2] + qpn[n2][2] + qpn[n3][2] + qpn[n4][2] + qpn[n5][2];
                        pn = pin[n1]    * pin[n2]    * pin[n3]    * pin[n4]    * pin[n5];
                        e = ep + en - gs;
                        find_po(no_p);
                        no_n = 5;
                        no[0] = n1;
                        no[1] = n2;
                        no[2] = n3;
                        no[3] = n4;
                        no[4] = n5;
                        inc_qp_rot();
                        for (n6 = n5 + 1; n6 < nnl && ep + en - gs < Emax && no_p + 7 <= max_qp; n6++){
                            for (n7 = n6 + 1; n7 < nnl && ep + en - gs < Emax; n7++){
                                en = qpn[n1][3] + qpn[n2][3] + qpn[n3][3] + qpn[n4][3] + qpn[n5][3] + qpn[n6][3] + qpn[n7][3];
                                pn = pin[n1]    * pin[n2]    * pin[n3]    * pin[n4]    * pin[n5]    * pin[n6]    * pin[n7];
                                e = ep + en - gs;
                                find_po(no_p);
                                no_n = 7;
                                no[0] = n1;
                                no[1] = n2;
                                no[2] = n3;
                                no[3] = n4;
                                no[4] = n5;
                                no[5] = n6;
                                no[6] = n7;
                                inc_qp_rot();
                            }
                            en = 0.;
                        }
                        en = 0.;                                        
                    }
                    en = 0.;                                
                }
                en = 0.;                        
            }
            en = 0.;            
        }
        en = 0.;        
    }
    en = 0.;
}
        
void even_n(float ep, float gs, int no_p)
{
    en = 0; 
    pn = 1;
    e = ep + en - gs;
    find_po(no_p);
    no_n = 0;
    inc_qp_rot();
    for (n1 = 0; n1 < nnl && ep + en - gs < Emax && no_p + 2 <= max_qp; n1++){
        for (n2 = n1 + 1; n2 < nnl && ep + en - gs < Emax; n2++){
            en = qpn[n1][1] + qpn[n2][1];   
            pn = pin[n1]    * pin[n2];
            e = ep + en - gs;
            find_po(no_p);
            no_n = 2;
            no[0] = n1;
            no[1] = n2;
            inc_qp_rot();
            for (n3 = n2 + 1; n3 < nnl && ep + en - gs < Emax && no_p + 4 <= max_qp; n3++){
                for (n4 = n3 + 1; n4 < nnl && ep + en - gs < Emax; n4++){
                    en = qpn[n1][2] + qpn[n2][2] + qpn[n3][2] + qpn[n4][2];
                    pn = pin[n1]    * pin[n2]    * pin[n3]    * pin[n4];
                    e = ep + en - gs;
                    find_po(no_p);
                    no_n = 4;
                    no[0] = n1;
                    no[1] = n2;
                    no[2] = n3;
                    no[3] = n4;
                    inc_qp_rot();
                    for (n5 = n4 + 1; n5 < nnl && ep + en - gs < Emax && no_p + 6 <= max_qp; n5++){
                        for (n6 = n5 + 1; n6 < nnl && ep + en - gs < Emax; n6++){
                            en = qpn[n1][3] + qpn[n2][3] + qpn[n3][3] + qpn[n4][3] + qpn[n5][3] + qpn[n6][3];
                            pn = pin[n1]    * pin[n2]    * pin[n3]    * pin[n4]    * pin[n5]    * pin[n6];
                            e = ep + en - gs;
                            find_po(no_p);
                            no_n = 6;
                            no[0] = n1;
                            no[1] = n2;
                            no[2] = n3;
                            no[3] = n4;
                            no[4] = n5;
                            no[5] = n6;
                            inc_qp_rot();
                        }
                        en = 0.;
                    }
                    en = 0.;
                }
                en = 0.;
            }
            en = 0.;
        }
        en = 0.;
    }
    en = 0.;
}

void find_po(int no_p)
{
    if(no_p == 0){
        return;
    }
    if(no_p == 1){
        po[0] = p1;
        return;
    }
    if(no_p == 2){
        po[0] = p1;
        po[1] = p2;
        return;
    }
    if(no_p == 3){
        po[0] = p1;
        po[1] = p2;
        po[2] = p3;
        return;
    }
    if(no_p == 4){
        po[0] = p1;
        po[1] = p2;
        po[2] = p3;
        po[3] = p4;
        return;
    }
    if(no_p == 5){
        po[0] = p1;
        po[1] = p2;
        po[2] = p3;
        po[3] = p4;
        po[4] = p5;
        return;
    }
    if(no_p == 6){
        po[0] = p1;
        po[1] = p2;
        po[2] = p3;
        po[3] = p4;
        po[4] = p5;
        po[5] = p6;
        return;
    }
    if(no_p == 7){
        po[0] = p1;
        po[1] = p2;
        po[2] = p3;
        po[3] = p4;
        po[4] = p5;
        po[5] = p6;
        po[6] = p7;
        return;
    }
    return;
}

void inc_vib_rot()
{
    int		ivib, irot, r, pi_vib, I;
    int		ie;
    float   R, K;   

    /* ******************************************************** */
    /* Quadrupole and octupole vibrations of 1 and 2 phonons    */
    /* The many phonon states are split assuming 100 keV        */
    /* interaction, 50 keV up or down                           */
    /* Rotations are then build on each vibrational band head   */
    /* ******************************************************** */
        
    /* Assumes quadrupole if positive parity: (2+) and (0+,2+,4+) */
    pi_vib = 1;                     
    /* One one-phonon state */
    ie = (int)((1. * (Evib_2/disp)) + 0.5);
    for(r = 0; r <= rmax; r = r + r0){
        R = (float)r;
        irot = Rot(R, Evib_2);
        if(I_gs == 0) {
            ivib = ie;
            K = fabs(2. + I_gs);
            I = I_spin(R, K);
            if(ivib+irot < iSpec && I >=low_spin && I <= high_spin ){
                SpinDis[ivib+irot][I]			= SpinDis[ivib+irot][I] + 1.;
                RhoLevel[ivib+irot]             = RhoLevel[ivib+irot]	+ 1.; /* Rotations built on vib-heads */
				RhoState[ivib+irot]             = RhoState[ivib+irot]	+ 2.*I+1.;
                Parity[ivib+irot]               = Parity[ivib+irot]     + pi_vib*pi_gs; 
            }
        } else {
            ivib = ie;
            K = fabs(2. + I_gs);
            I = I_spin(R, K);
            if(ivib+irot < iSpec && I >=low_spin && I <= high_spin ){
                SpinDis[ivib+irot][I]			= SpinDis[ivib+irot][I] + 1.;
                RhoLevel[ivib+irot]             = RhoLevel[ivib+irot]	+ 1.; /* Rotations built on vib-heads */
				RhoState[ivib+irot]             = RhoState[ivib+irot]	+ 2.*I+1.;
                Parity[ivib+irot]               = Parity[ivib+irot]     + pi_vib*pi_gs; 
            }

            ivib = ie + abs(ranint(fwhm/disp)); /* Assumes 100 keV interaction, 50 keV up, only */
            K = fabs(2. - I_gs);
            I = I_spin(R, K);
            if(ivib+irot < iSpec  && I >=low_spin && I <= high_spin ){
                SpinDis[ivib+irot][I]			= SpinDis[ivib+irot][I] + 1.;
                RhoLevel[ivib+irot]				= RhoLevel[ivib+irot]	+ 1.; /* Rotations built on vib-heads */
				RhoState[ivib+irot]             = RhoState[ivib+irot]	+ 2.*I+1.;
                Parity[ivib+irot]				= Parity[ivib+irot]     + pi_vib*pi_gs; 
            }
        }
    }
    /* Three two-phonon states */
    ie = (int)((2. * (Evib_2/disp)) + 0.5);
    for(r = 0; r <= rmax; r = r + r0){
        R = (float)r;
        irot = Rot(R, Evib_2);
        ivib = ie + ranint(fwhm/disp);
        K = fabs(0. + I_gs);
        I = I_spin(R, K);
        if(ivib+irot < iSpec && I >=low_spin && I <= high_spin ){
            SpinDis[ivib+irot][I]				= SpinDis[ivib+irot][I] + 1.;
            RhoLevel[ivib+irot]					= RhoLevel[ivib+irot]	+ 1.;
			RhoState[ivib+irot]					= RhoState[ivib+irot]	+ 2.*I+1.;
            Parity[ivib+irot]					= Parity[ivib+irot]     + pi_vib*pi_vib*pi_gs;
        }
    }
    for(r = 0; r <= rmax; r = r + r0){
        R = (float)r;
        irot = Rot(R, Evib_2);
        if(I_gs == 0.){
            ivib = ie + ranint(fwhm/disp);
            K = fabs(2. + I_gs);
            I = I_spin(R, K);
            if(ivib+irot < iSpec && I >=low_spin && I <= high_spin ){
                SpinDis[ivib+irot][I]           = SpinDis[ivib+irot][I] + 1.;
                RhoLevel[ivib+irot]				= RhoLevel[ivib+irot]	+ 1.;
				RhoState[ivib+irot]             = RhoState[ivib+irot]	+ 2.*I+1.;
                Parity[ivib+irot]				= Parity[ivib+irot]     + pi_vib*pi_vib*pi_gs;
            }
        }else{
            ivib = ie + ranint(fwhm/disp);  
            K = fabs(2. + I_gs);
            I = I_spin(R, K);
            if(ivib+irot < iSpec && I >=low_spin && I <= high_spin ){
                SpinDis[ivib+irot][I]           = SpinDis[ivib+irot][I] + 1.;
                RhoLevel[ivib+irot]				= RhoLevel[ivib+irot]	+ 1.;
				RhoState[ivib+irot]             = RhoState[ivib+irot]	+ 2.*I+1.;
                Parity[ivib+irot]				= Parity[ivib+irot]     + pi_vib*pi_vib*pi_gs;
            }
            ivib = ie + ranint(fwhm/disp);
            K = fabs(2. - I_gs);
            I = I_spin(R, K);
            if(ivib+irot < iSpec && I >=low_spin && I <= high_spin ){
                SpinDis[ivib+irot][I]           = SpinDis[ivib+irot][I] + 1.;
                RhoLevel[ivib+irot]				= RhoLevel[ivib+irot]	+ 1.;
				RhoState[ivib+irot]             = RhoState[ivib+irot]	+ 2.*I+1.;
                Parity[ivib+irot]				= Parity[ivib+irot]     + pi_vib*pi_vib*pi_gs;
            }
        }
    }
    for(r = 0; r <= rmax; r = r + r0){
        R = (float)r;
        irot = Rot(R, Evib_2);
        if(I_gs == 0.){
            ivib = ie + ranint(fwhm/disp);
            K = fabs(4. + I_gs);                                                            
            I = I_spin(R, K);
            if(ivib+irot < iSpec && I >=low_spin && I <= high_spin ){
                SpinDis[ivib+irot][I]           = SpinDis[ivib+irot][I] + 1.;
                RhoLevel[ivib+irot]				= RhoLevel[ivib+irot]	+ 1.;
				RhoState[ivib+irot]             = RhoState[ivib+irot]	+ 2.*I+1.;
                Parity[ivib+irot]				= Parity[ivib+irot]     + pi_vib*pi_vib*pi_gs;
            }
        }else{
            ivib = ie + ranint(fwhm/disp);
            K = fabs(4. + I_gs);
            I = I_spin(R, K);
            if(ivib+irot < iSpec && I >=low_spin && I <= high_spin ){
                SpinDis[ivib+irot][I]           = SpinDis[ivib+irot][I] + 1.;
                RhoLevel[ivib+irot]				= RhoLevel[ivib+irot]	+ 1.;
				RhoState[ivib+irot]             = RhoState[ivib+irot]	+ 2.*I+1.;
                Parity[ivib+irot]				= Parity[ivib+irot]     + pi_vib*pi_vib*pi_gs;
            }
            ivib = ie + ranint(fwhm/disp);
            K = fabs(4. - I_gs);
            I = I_spin(R, K);
            if(ivib+irot < iSpec && I >=low_spin && I <= high_spin ){
                SpinDis[ivib+irot][I]           = SpinDis[ivib+irot][I] + 1.;
                RhoLevel[ivib+irot]				= RhoLevel[ivib+irot]	+ 1.;
				RhoState[ivib+irot]             = RhoState[ivib+irot]	+ 2.*I+1.;
                Parity[ivib+irot]				= Parity[ivib+irot]     + pi_vib*pi_vib*pi_gs;
            }
        }
    }
        
    /* Assumes octupole with negative parity: (3-) and (0+,2+,4+,6+) */
    pi_vib = -1;                    
    /* One one-phonon state */
    ie = (int)((1. * (Evib_3/disp)) + 0.5);
    for(r = 0; r <= rmax; r = r + r0){
        R = (float)r;
        irot = Rot(R, Evib_3);
        if(I_gs == 0){
            ivib = ie;
            K = fabs(3. + I_gs);
            I = I_spin(R, K);
            if(ivib+irot < iSpec && I >=low_spin && I <= high_spin ){
                SpinDis[ivib+irot][I]           = SpinDis[ivib+irot][I] + 1.;
                RhoLevel[ivib+irot]				= RhoLevel[ivib+irot]	+ 1.;
				RhoState[ivib+irot]             = RhoState[ivib+irot]	+ 2.*I+1.;
                Parity[ivib+irot]				= Parity[ivib+irot]     + pi_vib*pi_gs;
            }       
        }else{
            ivib = ie;
            K = fabs(3. + I_gs);
            I = I_spin(R, K);
            if(ivib+irot < iSpec && I >=low_spin && I <= high_spin ){
                SpinDis[ivib+irot][I]           = SpinDis[ivib+irot][I] + 1.;
                RhoLevel[ivib+irot]				= RhoLevel[ivib+irot]	+ 1.;
				RhoState[ivib+irot]             = RhoState[ivib+irot]	+ 2.*I+1.;
                Parity[ivib+irot]				= Parity[ivib+irot]     + pi_vib*pi_vib*pi_gs;
            }
            ivib = ie + abs(ranint(fwhm/disp)); /* Assumes 100 keV interaction, 50 keV up, only */
            K = fabs(3. - I_gs);
            I = I_spin(R, K);
            if(ivib+irot < iSpec && I >=low_spin && I <= high_spin ){
                SpinDis[ivib+irot][I]           = SpinDis[ivib+irot][I] + 1.;
                RhoLevel[ivib+irot]				= RhoLevel[ivib+irot]	+ 1.;
				RhoState[ivib+irot]             = RhoState[ivib+irot]	+ 2.*I+1.;
                Parity[ivib+irot]				= Parity[ivib+irot]     + pi_vib*pi_vib*pi_gs;
            }
        }
    }
    /* Four two-phonon states */
    ie = (int)((2. * (Evib_3/disp)) + 0.5);
    for(r = 0; r <= rmax; r = r + r0){
        R = (float)r;
        irot = Rot(R, Evib_3);
        ivib = ie + ranint(fwhm/disp);
        K = fabs(0. + I_gs);
        I = I_spin(R, K);
        if(ivib+irot < iSpec && I >=low_spin && I <= high_spin ){
            SpinDis[ivib+irot][I]				= SpinDis[ivib+irot][I]	+ 1.;
            RhoLevel[ivib+irot]					= RhoLevel[ivib+irot]	+ 1.;
			RhoState[ivib+irot]                 = RhoState[ivib+irot]	+ 2.*I+1.;
            Parity[ivib+irot]					= Parity[ivib+irot]		+ pi_vib*pi_vib*pi_gs;
        }
    }
    for(r = 0; r <= rmax; r = r + r0){
        R = (float)r;
        irot = Rot(R, Evib_3);
        if(I_gs == 0.){
            ivib = ie + ranint(fwhm/disp);
            K = fabs(2. + I_gs);
            I = I_spin(R, K);
            if(ivib+irot < iSpec && I >=low_spin && I <= high_spin ){
                SpinDis[ivib+irot][I]           = SpinDis[ivib+irot][I] + 1.;
                RhoLevel[ivib+irot]				= RhoLevel[ivib+irot]	+ 1.;
				RhoState[ivib+irot]             = RhoState[ivib+irot]	+ 2.*I+1.;
                Parity[ivib+irot]				= Parity[ivib+irot]     + pi_vib*pi_vib*pi_gs;
            }
        }else{
            ivib = ie + ranint(fwhm/disp);  
            K = fabs(2. + I_gs);
            I = I_spin(R, K);
            if(ivib+irot < iSpec && I >=low_spin && I <= high_spin ){
                SpinDis[ivib+irot][I]           = SpinDis[ivib+irot][I] + 1.;
                RhoLevel[ivib+irot]				= RhoLevel[ivib+irot]	+ 1.;
				RhoState[ivib+irot]             = RhoState[ivib+irot]	+ 2.*I+1.;
                Parity[ivib+irot]				= Parity[ivib+irot]     + pi_vib*pi_vib*pi_gs;
            }
            ivib = ie + ranint(fwhm/disp);
            K = fabs(2. - I_gs);
            I = I_spin(R, K);
            if(ivib+irot < iSpec && I >=low_spin && I <= high_spin ){
                SpinDis[ivib+irot][I]           = SpinDis[ivib+irot][I] + 1.;
                RhoLevel[ivib+irot]				= RhoLevel[ivib+irot]	+ 1.;
				RhoState[ivib+irot]             = RhoState[ivib+irot]	+ 2.*I+1.;
                Parity[ivib+irot]				= Parity[ivib+irot]     + pi_vib*pi_vib*pi_gs;
            }
        }
    }
    for(r = 0; r <= rmax; r = r + r0){
        R = (float)r;
        irot = Rot(R, Evib_3);
        if(I_gs == 0.){
            ivib = ie + ranint(fwhm/disp);
            K = fabs(4. + I_gs);                                                            
            I = I_spin(R, K);
            if(ivib+irot < iSpec && I >=low_spin && I <= high_spin ){
                SpinDis[ivib+irot][I]           = SpinDis[ivib+irot][I] + 1.;
                RhoLevel[ivib+irot]				= RhoLevel[ivib+irot]	+ 1.;
				RhoState[ivib+irot]             = RhoState[ivib+irot]	+ 2.*I+1.;
                Parity[ivib+irot]				= Parity[ivib+irot]     + pi_vib*pi_vib*pi_gs;
            }
        }else{
            ivib = ie + ranint(fwhm/disp);
            K = fabs(4. + I_gs);
            I = I_spin(R, K);
            if(ivib+irot < iSpec && I >=low_spin && I <= high_spin ){
                SpinDis[ivib+irot][I]           = SpinDis[ivib+irot][I] + 1.;
                RhoLevel[ivib+irot]				= RhoLevel[ivib+irot]	+ 1.;
				RhoState[ivib+irot]             = RhoState[ivib+irot]	+ 2.*I+1.;
                Parity[ivib+irot]				= Parity[ivib+irot]     + pi_vib*pi_vib*pi_gs;
            }
            ivib = ie + ranint(fwhm/disp);
            K = fabs(4. - I_gs);
            I = I_spin(R, K);
            if(ivib+irot < iSpec && I >=low_spin && I <= high_spin ){
                SpinDis[ivib+irot][I]           = SpinDis[ivib+irot][I] + 1.;
                RhoLevel[ivib+irot]				= RhoLevel[ivib+irot]	+ 1.;
				RhoState[ivib+irot]             = RhoState[ivib+irot]	+ 2.*I+1.;
                Parity[ivib+irot]				= Parity[ivib+irot]     + pi_vib*pi_vib*pi_gs;
            }
        }
    }
    for(r = 0; r <= rmax; r = r + r0){
        R = (float)r;
        irot = Rot(R, Evib_3);
        if(I_gs == 0.){
            ivib = ie + ranint(fwhm/disp);
            K = fabs(6. + I_gs);                                                            
            I = I_spin(R, K);
            if(ivib+irot < iSpec && I >=low_spin && I <= high_spin ){
                SpinDis[ivib+irot][I]           = SpinDis[ivib+irot][I] + 1.;
                RhoLevel[ivib+irot]				= RhoLevel[ivib+irot]	+ 1.;
				RhoState[ivib+irot]             = RhoState[ivib+irot]	+ 2.*I+1.;
                Parity[ivib+irot]				= Parity[ivib+irot]		+ pi_vib*pi_vib*pi_gs;
            }
        }else{
            ivib = ie + ranint(fwhm/disp);
            K = fabs(6. + I_gs);
            I = I_spin(R, K);
            if(ivib+irot < iSpec && I >=low_spin && I <= high_spin ){
                SpinDis[ivib+irot][I]           = SpinDis[ivib+irot][I] + 1.;
                RhoLevel[ivib+irot]				= RhoLevel[ivib+irot]	+ 1.;
				RhoState[ivib+irot]             = RhoState[ivib+irot]	+ 2.*I+1.;
                Parity[ivib+irot]				= Parity[ivib+irot]     + pi_vib*pi_vib*pi_gs;
            }
            ivib = ie + ranint(fwhm/disp);
            K = fabs(6. - I_gs);
            I = I_spin(R, K);
            if(ivib+irot < iSpec && I >=low_spin && I <= high_spin ){
                SpinDis[ivib+irot][I]           = SpinDis[ivib+irot][I] + 1.;
                RhoLevel[ivib+irot]				= RhoLevel[ivib+irot]	+ 1.;
				RhoState[ivib+irot]             = RhoState[ivib+irot]	+ 2.*I+1.;
                Parity[ivib+irot]				= Parity[ivib+irot]     + pi_vib*pi_vib*pi_gs;
            }
        }       
    }
    return;
}

void inc_qp_rot(){
    int             iqp, irot, j, k, kmax, r, I=0;
    int             ie;
    float			R=0., K=0.;
    int             Ipos, Ppos;
    /* ************************************************************************ */
    /* From a given many-qp configuration, making all combinations of Omegas    */
    /* with splitting of 100 keV interaction, 50 keV up or down                 */
    /* Rotations are then build on each many-qp band band head                  */
    /* ************************************************************************ */
    ie              = (int)((e/disp) + 0.5);
    iqp             = ie;
    no_pairs		= (no_p/2) + (no_n/2);
    no_qp			= no_p + no_n;
    kmax			= pow(2,(no_qp-1));
    if(Include == 1) Sum_Omegas(kmax);
    for(k = 0; k < kmax; k++){
        if(Include == 1) K = Sum_K[k];  
        for(r = 0; r <= rmax; r = r + r0){
            R = (float)r;
            irot = Rot(R, e);
			I=0;
            if(Include == 1) I = I_spin(R, K);
            if(iqp+irot < iSpec && I >=low_spin && I <= high_spin){
                
// BEGIN: Making 96Mo spins around Bn = 9.154 MeV, with dE = 0.0 - 0.050 MeV
                ee = e + ranint(fwhm/disp);
                if(ee > Bn && ee < Bn + dee){
                    Ipos = 2*I;
                    if(pp*nn < 0)Ppos = 0;
                    if(pp*nn > 0)Ppos = 1;
                    States_Bn[Ipos+Ppos] = States_Bn[Ipos+Ppos] + 1.;
                }
// END: Making 96Mo
                
                if (no_qp > max_qp_calc) max_qp_calc = no_qp;
                if(r == 0 && k == 0){
                    TotBhd += 1;
					if(TotBhd == (TotBhd/1000)*1000){
						printf(    "Bhd(0)=%8d Ex=%7.3f Type=%2d%2d Parity= %2d Conf=%3d K(0)=%4.1f\n", TotBhd, e, no_p, no_n, pp*pn, kmax, K);
					}
                    fprintf(fparam,"Bhd(0)=%8d Ex=%7.3f Type=%2d%2d Parity= %2d Conf=%3d K(0)=%4.1f\n", TotBhd, e, no_p, no_n, pp*pn, kmax, K);
                }
                RhoLevel[iqp+irot]		= RhoLevel[iqp+irot]		+ 1.;
				RhoState[iqp+irot]      = RhoState[iqp+irot]  + 2.*I+ 1.;
                SpinDis[iqp+irot][I]	= SpinDis [iqp+irot][I]		+ 1.;
                Parity [iqp+irot]		= Parity  [iqp+irot]		+ pp*pn;
                Npair  [iqp+irot]		= Npair   [iqp+irot]		+ no_pairs;
                Npair_p[iqp+irot]		= Npair_p [iqp+irot]		+ no_p/2;
                Npair_n[iqp+irot]		= Npair_n [iqp+irot]		+ no_n/2;
				if(r == 0){
					for(j = 0; j < no_p; j++){
						orbitno = po[j];            /* qp energy-sorted orbital number */
						orbitno = sp_no_p[orbitno]; /* orgininal sp orbital number */
//						Orbitp[iqp+irot][orbitno] = Orbitp[iqp+irot][orbitno] + 1.;
						ch = (int)(((qp_p_energy[orbitno] + Emax)/disp) + 0.5); 
						Orbitp[iqp+irot][ch] = Orbitp[iqp+irot][ch] + 1.;

					}
					for(j = 0; j < no_n; j++){
						orbitno = no[j];            /* qp energy-sorted orbital number */
						orbitno = sp_no_n[orbitno]; /* orgininal sp orbital number */
//						Orbitn[iqp+irot][orbitno] = Orbitn[iqp+irot][orbitno] + 1.;
						ch = (int)(((qp_n_energy[orbitno] + Emax)/disp) + 0.5); 
						Orbitn[iqp+irot][ch] = Orbitn[iqp+irot][ch] + 1.;
					}
				}
            }
        }
        iqp = ie + ranint(fwhm/disp); /* Assumes 100 keV interaction, 50 keV up or down */
    }
}

int I_spin(float R, float K)
{
    int Itot=0;
    float Xspin;
    /* Calculating I from I(I+1) = R(R+1) + KK */
    Xspin = (-1. + sqrt(1. + 4.*(R*(R+1.) + K*K) ))/2.;
    if(spin_fac == 1.) Itot = (int)(Xspin   + 0.5);
    if(spin_fac == 2.) Itot = (int)Xspin    + 0.5;
    return Itot;
}

void Sum_Omegas(int kmax)
{
    int i, j, k;
    float old_K[1024], new_K[1024];
        
    /* **************************************************** */
    /* Making array of all kmax possible K = Sum_of_Omegas  */
    /* **************************************************** */
    for (k = 0; k < no_p; k++){
        Ks[k] = Omp[po[k]];
    }
    for (k = no_p; k < no_qp; k++){
        Ks[k] = Omn[no[k-no_p]];
    }
        
    old_K[0] = Ks[0];
    for (j = 1; j < no_qp; j++){
        for (i = 0; i < kmax/2; i++){
            new_K[2*i]		= (Ks[j] + old_K[i]);
            new_K[2*i+1]    = (Ks[j] - old_K[i]);
        }
        for (k = 0; k < kmax; k++){
            old_K[k] = new_K[k];
        }
    }
    for (k = 0; k < kmax; k++){
        Sum_K[k] = fabs(old_K[k]);
    }
}

void inc_gs_rot()
{
    int irot, r, I;
    float R, K;
    /* ******************************************** */
    /* Rotations built on the 0+ gs in e-e- nucleus */
    /* ******************************************** */      
    K = 0.;
    for(r = 0; r <= rmax; r = r + r0){
        R = (float)r;
        irot = Rot(R, 0.);
        I = r;
        if(irot < iSpec && I >=low_spin && I <= high_spin ){
            SpinDis[irot][I]        = SpinDis[irot][I]	+ 1.;
            RhoLevel[irot]			= RhoLevel[irot]	+ 1.;
			RhoState[irot]          = RhoState[irot]+2.*I+1.;
            Parity[irot]            = Parity[irot]		+ 1;
            Npair[irot]				= Npair[irot]		+ 0;
            Npair_p[irot]           = Npair_p[irot]		+ 0;
            Npair_n[irot]           = Npair_n[irot]		+ 0;
        }
    }
}

int Rot(float R, float Eintr)
{
    float Arot0, Arot, Erot, Etot;
    if(R == 0.)
        return 0;
    if(Eintr > Erigid)
        return (int)(((Arigid*(R*(R+1.)))/disp) + 0.5);
    Arot0   = 0.5*(Ags + Arigid);				      /* first guess */
    Arot    = Arigid;                                                       
    while(fabs((Arot-Arot0)/Arot0) > 0.10){           /* finished when 10 percent is reached  */
        Erot    = Arot0*R*(R+1);
        Etot    = Erot + Eintr;
		
//		printf(" R=%6.1f Arot=%6.4f, Arot0=%6.4f, Eintr=%6.3f, Erot=%6.3f, Etot=%7.3f\n",R, Arot,Arot0,Eintr,Erot,Etot);
		
        if(Etot > Erigid){
            Arot = Arigid;
        } else {
            Arot = Ags + Etot*(Arigid - Ags)/Erigid; 
        }
		Arot0	= 0.1*Arot + 0.9*Arot0;  /* Approching slowly the right Arot */
    }
    return (int)(((Arot*(R*(R+1.)))/disp) + 0.5);
}
