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
#define MAXDIM     10000000                                     /* Max dimension of Ex spectrum (eV), i.e. 10 MeV*/
#define MAXLEN        10000                                     /* Max number of output spectrum, e.g. 10 MeV with 1keV/ch */
#define MAXORBITS        50                                     /* Max number of p or n orbits included in calculation */
#define MAXENERGY      10.0                                     /* Max energy Ex (MeV) to iterate on */
unsigned int    i10 = 10;                                       /* Number of inner loops for 2, 4, 6,... q.p. */
unsigned int    i100 = 300;
unsigned int    i1000 = 9000;
int             j=0;
int             outerloop = 2000, jtenth=0;                     /* Number of outer loops, and tenth of that */
unsigned int    np=42, nn=56;                                   /* Number of total protons and neutrons */
int             ip=0, in=0;                                     /* Fermi level for protons and neutrons */
int             op=0, on=0;                                     /* odd-even system */
int             r0;                                             /* Gives spin steps of rotational bands */
int             maxorb = MAXORBITS;                             /* Maximum of orbits */
int             npairs=0, nqpar=0;                              /* Number of pairs broken and quasiparticles */
int             k=0, kmax, iv=0, r, rot=0, rmax=8;
int             iplast=0, inlast=0;                             /* Same as above, but for reduced q.p. scheme */
double  epmin, epmax, enmin, enmax;                             /* Lower and upper q.p. energies taken into account */
unsigned int    ipmin, ipmax, inmin, inmax;                     /* Lower and upper level taken into account */
int             iline=0;                                        /* Controls that not more than 7000 lines are printed on terminal */
unsigned long int npl=0, nnl=0;                                 /* Proton and neutron levels taken into account */
double  dnpl, dnnl;                                             /* Same as above, but double */
float   spp[MAXLEV], spn[MAXLEV];                               /* Nilsson s.p. energies (MeV) */
float   qpp[MAXLEV][MAXNUPAIR+1], qpn[MAXLEV][MAXNUPAIR+1];     /* Nilsson q.p. energies (MeV) */
double  dqpp[MAXLEV][MAXNUPAIR+1], dqpn[MAXLEV][MAXNUPAIR+1];   /* Same as above, but double */
int             pv[MAXLEV], nv[MAXLEV];                         /* Vector with present number of particles per level */
int             pip[MAXLEV], pin[MAXLEV];                       /* Storing parity of s.p. */
int             pp, pn;                                         /* Parity for multi pr. and ne. configurations */
float   lamp, lamn;                                             /* Fermi levels lambda */
double  gsp, gsn, gs;                                           /* Absolute ground state energy */
float   Emax   = MAXENERGY;                                     /* Max energy above and below Fermi level */
float   Emaxdum= MAXENERGY;                                     /* Not used any more */
float   pEpair = 2.0;                                           /* The 2*Delta proton pairing energy (MeV) */
float   nEpair = 2.0;                                           /* The 2*Delta pairing neutron energy (MeV) */
float   facpair= 1.0;                                           /* Delta reduced by facpair for each broken pair */
float   Arot   = 0.050;                                         /* Rotational parameter (MeV) */
float   Evib   = 1.0;                                           /* Vibrationl phonon energy taken from 0+ in 46Ti */
float   disp   = 0.240;                                         /* Dispertion (ch/MeV) for output spectrum Spec(Ex) */
float   fwhm   = 50000.;                                        /* Omega_p x Omega_n interaction in eV (50 keV) */
float   hbar_omega_p, hbar_omega_n;
float   sc_p = 1.0;
float   sc_n = 1.0;
int             irand, pi_vib = 1;
double e, ep, en;                                               /* Total Ex is assigned e */
long int ie;
int ip1=0,ip2=0,ip3=0,ip4=0,ip5=0,ip6=0,ip7=0;                  /* Random q.p. p-orbital i1, i2,...  */
int in1=0,in2=0,in3=0,in4=0,in5=0,in6=0,in7=0;                  /* Random q.p. n-orbital i1, i2,...  */
int iseed=11;                                                   /* Seed */

long int hitsum=0, totsum=0, trysum=0;
int             Ex[MAXDIM];                                     /* Spectrum with eV bins and very long (MAXDIM) */
int             pr[MAXDIM];                                     /* Spectrum with eV bins and flags protons */
int             ne[MAXDIM];                                     /* Spectrum with eV bins and flags neutrons */
int             pi[MAXDIM];                                     /* Spectrum with eV bins and flags parity +/- */
int             orbitno;                                        /* Orbital number */

float   Spec[MAXLEN];
float   Npair[MAXLEN];
float   Npair_p[MAXLEN];
float   Npair_n[MAXLEN];
float   Parity[MAXLEN];
float   Orbitp[MAXLEN][MAXORBITS];
float   Orbitn[MAXLEN][MAXORBITS];
float   po[MAXDIM][MAXNUPAIR*2+1]; /* Matrix with protons orbitals active at Ex */
float   no[MAXDIM][MAXNUPAIR*2+1]; /* Matrix with neutron orbitals active at Ex */

unsigned int iSpec;                /* iSpec is dimension of Spec(Ex) spectrum */
clock_t start,secs;
void scatterp1();
void scattern1();
void scatterp2();
void scattern2();
void scatterp3();
void scattern3();
void scatterp4();
void scattern4();
void scatterp5();
void scattern5();
void scatterp6();
void scattern6();
void scatterp7();
void scattern7();
float sumv2(float e0,float sp0[],int nl0, float Epair);
float lambda(int n0,float sp0[],int nl0, float Epair);
float v2(float e0,float sp0[],int i0, float Epair);
int ranint(float fwhm);
void timeleft();
float secs0,secstot;
int ihours, iminu, isec;

static void fgets_ignore(char *s, int size, FILE *stream)
{
    // suppress braindead and presumtuous glibc 'security' warning
    if( !fgets(s, size, stream) )
        return;
}

int main()
{
    char line[128];
    char cdum[128];
    int nosc[MAXLEV],iA,iZ,idum;
    float eps2,eps4,kappa,mu;
    FILE *fp, *fparam;
    unsigned long int i;
    int j;

    printf("\n");
    printf("  ________________________________________ \r\n");
    printf(" |                                        |\r\n");
    printf(" |            M I C R O  4.0.1            |\r\n");
    printf(" |                                        |\r\n");
    printf(" |       Program to calculate NLD by      |\r\n");
    printf(" |       combining all ways to place      |\r\n");
    printf(" |        n neutrons and p protons        |\r\n");
    printf(" |   in the BCS quasi-particle schemes:   |\r\n");
    printf(" |          spn.dat and spp.dat           |\r\n");
    printf(" |                                        |\r\n");
    printf(" | E-mail  : magne.guttormsen@fys.uio.no  |\r\n");
    printf(" | Created :  7 Oct 2005                  |\r\n");
    printf(" | Modified: 12 Jul 2007                  |\r\n");
    printf(" | Modified: 30 Jun 2008                  |\r\n");
    printf(" |________________________________________|\r\n");
    printf("                                           \r\n");

    printf("Reading single proton energy levels:  spp.dat\n");
    fp = fopen("spp.dat", "r");
    if(fp == NULL){
        printf("No spp.dat file in your directory\n");
        exit(0);
    }
    fgets_ignore(line,sizeof(line),fp);
    sscanf(line,"%s %s %s %s %s %s %d %s %d", cdum, cdum, cdum, cdum, cdum, cdum, &iA, cdum, &iZ);
    fgets_ignore(line,sizeof(line),fp);
    sscanf(line,"%s %f %s %f %s %f %s %f", cdum, &eps2, cdum, &eps4, cdum, &kappa, cdum, &mu);
    fgets_ignore(line,sizeof(line),fp);
    fgets_ignore(line,sizeof(line),fp);
    idum = 1;
    while( fgets(line,sizeof(line),fp) != NULL && npl < MAXLEV && idum > 0 ){
        sscanf(line,"%d %s %s %s %f", &nosc[npl], cdum, cdum, cdum, &spp[npl]);
        idum =  nosc[npl];
        if(npl == 0) idum = 1;
        npl++;
    }
    npl = npl - 1;
    for(i = 0; i < npl; i++){
        pip[i] = pow(-1,nosc[i]);
    }
    fgets_ignore(line,sizeof(line),fp);
    fgets_ignore(line,sizeof(line),fp);
    fgets_ignore(line,sizeof(line),fp);
    fgets_ignore(line,sizeof(line),fp);
    fgets_ignore(line,sizeof(line),fp);
    fgets_ignore(line,sizeof(line),fp);
    fgets_ignore(line,sizeof(line),fp);
    fgets_ignore(line,sizeof(line),fp);
    sscanf(line,"%s %s %s %s %f %s", cdum, cdum, cdum, cdum, &hbar_omega_p, cdum);
    fgets_ignore(line,sizeof(line),fp);
    fclose(fp);

    printf("Proton Nilsson scheme:\n");
    printf("(A,Z)       = (%d,%d) \n",iA,iZ);
    printf("(eps2,eps4) = (%f,%f) \n",eps2,eps4);
    printf("(kappa,mu)  = (%f,%f) \n",kappa,mu);
    //      printf("HbarOmega   = %f keV\n",hbar_omega_p);

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
        sscanf(line,"%d %s %s %s %f", &nosc[nnl], cdum, cdum, cdum, &spn[nnl]);
        idum =  nosc[nnl];
        if(nnl == 0) idum = 1;
        nnl++;
    }
    nnl = nnl - 1;
    for(i = 0; i < nnl; i++){
        pin[i] = pow(-1,nosc[i]);
    }
    fgets_ignore(line,sizeof(line),fp);
    fgets_ignore(line,sizeof(line),fp);
    fgets_ignore(line,sizeof(line),fp);
    fgets_ignore(line,sizeof(line),fp);
    fgets_ignore(line,sizeof(line),fp);
    fgets_ignore(line,sizeof(line),fp);
    fgets_ignore(line,sizeof(line),fp);
    fgets_ignore(line,sizeof(line),fp);
    sscanf(line,"%s %s %s %s  %f %s ", cdum, cdum, cdum, cdum, &hbar_omega_n, cdum);
    fgets_ignore(line,sizeof(line),fp);
    fclose(fp);

    printf("Neutron Nilsson scheme:\n");
    printf("(A,Z)       = (%d,%d) \n",iA,iZ);
    printf("(eps2,eps4) = (%f,%f) \n",eps2,eps4);
    printf("(kappa,mu)  = (%f,%f) \n",kappa,mu);
    //      printf("HbarOmega   = %f keV\n",hbar_omega_n);

    for(i = 0; i < MAXDIM; i++){
        for(j = 0; j < MAXNUPAIR*2+1; j++){
            no[i][j] = MAXORBITS;
        }
    }

    for(i = 0; i < MAXDIM; i++){
        for(j = 0; j < MAXNUPAIR*2+1; j++){
            po[i][j] = MAXORBITS;
        }
    }

    fp = fopen("microin.dat", "r");
    if(fp == NULL){
        printf("Could not open file microin.dat, default values are used \n");
    }
    else {
        fgets_ignore(line,sizeof(line),fp);
        sscanf(line, " %d %d %f %f %f %f %f %f %f %f %f %d\n", &np, &nn, &pEpair, &nEpair, &facpair, &Arot, &Evib, &Emaxdum, &disp, &sc_p, &sc_n, &pi_vib);
        fclose(fp);
    }

    printf("\nNumber of protons:                      <%d>",np);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%d", &np);

    printf("Number of neutrons:                     <%d>", nn);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%d", &nn);

    printf("Scaling proton harm. osc. quant.:   <%6.3f>", sc_p);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &sc_p);
    //      printf("(Scaled HbarOmega = %f keV)\n", hbar_omega_p * sc_p);
    for( i = 0; i < npl; i++){
        spp[i] = spp[i] * sc_p;
    }

    printf("Scaling neutron harm. osc. quant.:  <%6.3f>", sc_n);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &sc_n);
    //      printf("(Scaled HbarOmega = %f keV)\n", hbar_omega_n * sc_n);
    for( i = 0; i < nnl; i++){
        spn[i] = spn[i] * sc_n;
    }

    printf("Proton pairing 2*Delta (MeV):       <%6.3f>", pEpair);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &pEpair);

    printf("Neutron pairing 2*Delta (MeV):      <%6.3f>", nEpair);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &nEpair);

    printf("Pairing reduction factor:           <%6.3f>", facpair);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &facpair);

    lamp = lambda(np, spp, npl, pEpair);
    printf("Proton Fermi level (MeV):           <%6.3f>", lamp);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &lamp);

    lamn = lambda(nn, spn, nnl, nEpair);
    printf("Neutron Fermi level (MeV):          <%6.3f>", lamn);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &lamn);

    printf("Rotational parameter (MeV):         <%6.3f>", Arot);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &Arot);

    printf("Vibrational phonon quantum (MeV):   <%6.3f>", Evib);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &Evib);

    printf("Parity of vibrational state:           <%3d>", pi_vib);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%d", &pi_vib);

    if(2*(np/2) == np) Emax = Emax - (pEpair/2.);
    if(2*(nn/2) == nn) Emax = Emax - (nEpair/2.);
    printf("Maximum q.p. energy (MeV):          <%6.3f>", Emax);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &Emax);

    printf("Dispersion for final rho (MeV/ch):  <%6.3f>", disp);
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

    /*
       Finding Nilsson levels above and below the Fermi level
       that have to be included in order to have Ex < Emax
    */

    epmin = spp[ip] - Emax;
    epmax = spp[ip] + Emax;
    enmin = spn[in] - Emax;
    enmax = spn[in] + Emax;
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
        if (inmin == 0 && (spn[i] >= enmin)) inmin = i-1;
        if (inmax == nnl - 1 && (spn[i] >= enmax)) inmax = i;
    }
    if (ipmin <= 0) ipmin = 0;
    if (inmin <= 0) inmin = 0;

    printf("Give lower and higher orbitals to be included\n");
    printf("Lower proton orbital:   <%2d>", ipmin);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%d", &ipmin);
    printf("Higher proton orbital:  <%2d>", ipmax);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%d", &ipmax);
    printf("Lower neutron orbital:  <%2d>", inmin);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%d", &inmin);
    printf("Higher neutron orbital: <%2d>", inmax);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%d", &inmax);
    printf("Give number of outer loop:            <%4d>", outerloop);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%d", &outerloop);
    jtenth = outerloop/10;
    printf("Give seed number for random generator:  <%2d>", iseed);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%d", &iseed);

    printf("Proton  Fermi energy %8.4f MeV gives %8.4f protons\n", lamp, sumv2(lamp, spp, npl, pEpair));
    printf("Neutron Fermi energy %8.4f MeV gives %8.4f neutrons\n",lamn, sumv2(lamn, spn, nnl, nEpair));

    fparam = fopen("parameters.info", "w");
    if(fparam == NULL){
        printf("Could not open parameters.info \n");
        exit(0);
    }
    fprintf(fparam,"Number of protons:            %3d\n",np);
    fprintf(fparam,"Number of neutrons:           %3d\n", nn);
    fprintf(fparam,"Proton pairing 2*Delta (MeV):       %6.3f\n", pEpair);
    fprintf(fparam,"Neutron pairing 2*Delta (MeV):      %6.3f\n", nEpair);
    fprintf(fparam,"Pairing reduction factor:           %6.3f\n", facpair);
    fprintf(fparam,"Proton Fermi level (MeV):           %6.3f\n", lamp);
    fprintf(fparam,"Neutron Fermi level (MeV):          %6.3f\n", lamn);
    fprintf(fparam,"Rotational parameter (MeV):         %6.3f\n", Arot);
    fprintf(fparam,"Vibrational phonon quantum (MeV):   %6.3f\n", Evib);
    fprintf(fparam,"Maximum q.p. energy (MeV):          %6.3f\n", Emax);
    fprintf(fparam,"Dispersion for final rho (MeV/ch):  %6.3f\n", disp);
    fprintf(fparam,"Lower proton orbital:   %2d\n", ipmin);
    fprintf(fparam,"Higher proton orbital:  %2d\n", ipmax);
    fprintf(fparam,"Lower neutron orbital:  %2d\n", inmin);
    fprintf(fparam,"Higher neutron orbital: %2d\n", inmax);
    fprintf(fparam,"Number of outer loop:            <%4d>\n", outerloop);
    fprintf(fparam,"Seed number for random generator:  %2d\n", iseed);
    fprintf(fparam,"Proton  Fermi energy %8.4f MeV gives %8.4f protons\n", lamp, sumv2(lamp, spp, npl, pEpair));
    fprintf(fparam,"Neutron Fermi energy %8.4f MeV gives %8.4f neutrons\n",lamn, sumv2(lamn, spn, nnl, nEpair));

    printf("\n");
    npl = ipmax - ipmin + 1;
    for(i = 0; i < npl; i++){
        spp[i]  = spp[i+ipmin];
        pip[i]  = pip[i+ipmin];
    }
    gsp = 10000.;
    for (j = 0; j < MAXNUPAIR + 1; j++){ /* Making q.p. energies for 0, 1, 2, 3,... pairs broken */
        for(i = 0; i < npl; i++){
            qpp[i][j]  = sqrt((spp[i]-lamp)*(spp[i]-lamp) + ((pow(facpair,j)*pEpair/2.0))*((pow(facpair,j)*pEpair/2.0)));
            dqpp[i][j] = (double)qpp[i][j];
            if(dqpp[i][j] < gsp && j==0){
                gsp = dqpp[i][j];          /* Lowest (g.s.) Ex for protons */
            }
            if(j==0) printf("%2lu Proton  s.p. %6.3f MeV, q.p. %6.3f MeV, v2 = %6.3f, pi = %2d \n",i, spp[i], qpp[i][j], v2(lamp, spp, i, pEpair), pip[i]);
            if(j==0) fprintf(fparam,"%2lu Proton  s.p. %6.3f MeV, q.p. %6.3f MeV, v2 = %6.3f, pi = %2d \n",i, spp[i], qpp[i][j], v2(lamp, spp, i, pEpair), pip[i]);
        }
    }
    printf("\n");
    nnl = inmax - inmin + 1;
    for(i = 0; i < nnl; i++){
        spn[i]  = spn[i+inmin];
        pin[i]  = pin[i+inmin];
    }
    gsn = 10000.;
    for (j = 0; j < MAXNUPAIR + 1; j++){ /* Making q.p. energies for 0, 1, 2, 3,... pairs broken */
        for(i = 0; i < nnl; i++){
            qpn[i][j]  = sqrt((spn[i]-lamn)*(spn[i]-lamn) + ((pow(facpair,j)*nEpair/2.0)*(pow(facpair,j)*nEpair/2.0)));
            dqpn[i][j] = (double)qpn[i][j];
            if(dqpn[i][j] < gsn && j==0){
                gsn = dqpn[i][j];          /* Lowest Ex for neutrons (g.s.) */
            }
            if(j==0) printf("%2lu Neutron s.p. %6.3f MeV, q.p. %6.3f MeV, v2 = %6.3f, pi = %2d \n",i, spn[i], qpn[i][j], v2(lamn, spn, i, nEpair), pin[i]);
            if(j==0) fprintf(fparam,"%2lu Neutron s.p. %6.3f MeV, q.p. %6.3f MeV, v2 = %6.3f, pi = %2d \n",i, spn[i], qpn[i][j], v2(lamn, spn, i, nEpair), pin[i]);
        }
    }
    iplast = ip-ipmin-1;
    inlast = in-inmin-1;

    if(2*(np/2) == np){
        gsp = 0;
        op  = 0;
    }
    else
        {
            gsp = gsp;
            op  = 1;
        }
    if(2*(nn/2) == nn){
        gsn = 0;
        on  = 0;
    }
    else
        {
            gsn = gsn;
            on  = 1;
        }

    gs = gsp + gsn;
    printf("\nProton type %d, neutron type %d. Absolute value of g.s. = %4.4f\n", op, on, gs);
    fprintf(fparam,"Proton type %d, neutron type %d. Absolute value of g.s. = %4.4f\n", op, on, gs);

    r0 = 1;     /* not ee nucleus, then spinsteps of 1 */
    if(op == 0 && on == 0)
        r0 = 2; /* ee nucleus, then spinsteps of 2 */
    iSpec = (int)((float)MAXDIM /(1.0E+6*disp) + 0.5);
    if(iSpec > MAXLEN) iSpec = MAXLEN;

    fprintf(fparam,"Steps in spin       %3d\n", r0);
    fprintf(fparam,"Length of spectrum  %3d\n", iSpec);
    fprintf(fparam,"Maximum of orbitals %3d\n", maxorb);

    dnpl = (double)npl;
    dnnl = (double)nnl;

    srand48(iseed);
    secs0=(float)clock();

    if(op == 1 && on == 1){                         /* odd odd nucleus */
        for( j = 0; j < outerloop; j++){
            for( i = 0; i < i10; i++){
                trysum = trysum +1;
                ip1 = (int) (drand48() * dnpl);
                scatterp1(ip1);
                in1 = (int) (drand48() * dnnl);
                scattern1(in1);
                e = ep + en - gs;
                ie = (long int)(e*1.0E+06 + 0.5);
                if (ie < MAXDIM){
                    Ex[ie] = Ex[ie] + 1.;
                    pr[ie] = 1;
                    ne[ie] = 1;
                    pi[ie] = pp*pn;
                    po[ie][0] = ip1;
                    no[ie][0] = in1;
                }
            }
            for( i = 0; i < i10; i++){
                trysum = trysum +1;
                ip1 = (int) (drand48() * dnpl);
                ip2 = (int) (drand48() * dnpl);
                ip3 = (int) (drand48() * dnpl);
                scatterp3(ip1,ip2,ip3);
                in1 = (int) (drand48() * dnnl);
                scattern1(in1);
                e = ep + en - gs;
                ie = (long int)(e*1.0E+06 + 0.5);
                if (ie < MAXDIM){
                    Ex[ie] = Ex[ie] + 1.;
                    pr[ie] = 3;
                    ne[ie] = 1;
                    pi[ie] = pp*pn;
                    po[ie][0] = ip1;
                    po[ie][1] = ip2;
                    po[ie][2] = ip3;
                    no[ie][0] = in1;
                }
            }
            for( i = 0; i < i10; i++){
                trysum = trysum +1;
                ip1 = (int) (drand48() * dnpl);
                scatterp1(ip1);
                in1 = (int) (drand48() * dnnl);
                in2 = (int) (drand48() * dnnl);
                in3 = (int) (drand48() * dnnl);
                scattern3(in1,in2,in3);
                e = ep + en - gs;
                ie = (long int)(e*1.0E+06 + 0.5);
                if (ie < MAXDIM){
                    Ex[ie] = Ex[ie] + 1.;
                    pr[ie] = 1;
                    ne[ie] = 3;
                    pi[ie] = pp*pn;
                    po[ie][0] = ip1;
                    no[ie][0] = in1;
                    no[ie][1] = in2;
                    no[ie][2] = in3;
                }
            }
            for( i = 0; i < i100; i++){
                trysum = trysum +1;
                ip1 = (int) (drand48() * dnpl);
                ip2 = (int) (drand48() * dnpl);
                ip3 = (int) (drand48() * dnpl);
                ip4 = (int) (drand48() * dnpl);
                ip5 = (int) (drand48() * dnpl);
                scatterp5(ip1,ip2,ip3,ip4,ip5);
                in1 = (int) (drand48() * dnnl);
                scattern1(in1);
                e = ep + en - gs;
                ie = (long int)(e*1.0E+06 + 0.5);
                if (ie < MAXDIM){
                    Ex[ie] = Ex[ie] + 1.;
                    pr[ie] = 5;
                    ne[ie] = 1;
                    pi[ie] = pp*pn;
                    po[ie][0] = ip1;
                    po[ie][1] = ip2;
                    po[ie][2] = ip3;
                    po[ie][3] = ip4;
                    po[ie][4] = ip5;
                    no[ie][0] = in1;
                }
            }
            for( i = 0; i < i100; i++){
                trysum = trysum +1;
                ip1 = (int) (drand48() * dnpl);
                scatterp1(ip1);
                in1 = (int) (drand48() * dnnl);
                in2 = (int) (drand48() * dnnl);
                in3 = (int) (drand48() * dnnl);
                in4 = (int) (drand48() * dnnl);
                in5 = (int) (drand48() * dnnl);
                scattern5(in1,in2,in3,in4,in5);
                e = ep + en - gs;
                ie = (long int)(e*1.0E+06 + 0.5);
                if (ie < MAXDIM){
                    Ex[ie] = Ex[ie] + 1.;
                    pr[ie] = 1;
                    ne[ie] = 5;
                    pi[ie] = pp*pn;
                    po[ie][0] = ip1;
                    no[ie][0] = in1;
                    no[ie][1] = in2;
                    no[ie][2] = in3;
                    no[ie][3] = in4;
                    no[ie][4] = in5;
                }
            }
            for( i = 0; i < i100; i++){
                trysum = trysum +1;
                ip1 = (int) (drand48() * dnpl);
                ip2 = (int) (drand48() * dnpl);
                ip3 = (int) (drand48() * dnpl);
                scatterp3(ip1,ip2,ip3);
                in1 = (int) (drand48() * dnnl);
                in2 = (int) (drand48() * dnnl);
                in3 = (int) (drand48() * dnnl);
                scattern3(in1,in2,in3);
                e = ep + en - gs;
                ie = (long int)(e*1.0E+06 + 0.5);
                if (ie < MAXDIM){
                    Ex[ie] = Ex[ie] + 1.;
                    pr[ie] = 3;
                    ne[ie] = 3;
                    pi[ie] = pp*pn;
                    po[ie][0] = ip1;
                    po[ie][1] = ip2;
                    po[ie][2] = ip3;
                    no[ie][0] = in1;
                    no[ie][1] = in2;
                    no[ie][2] = in3;
                }
            }
            for( i = 0; i < i1000; i++){
                trysum = trysum +1;
                ip1 = (int) (drand48() * dnpl);
                scatterp1(ip1);
                in1 = (int) (drand48() * dnnl);
                in2 = (int) (drand48() * dnnl);
                in3 = (int) (drand48() * dnnl);
                in4 = (int) (drand48() * dnnl);
                in5 = (int) (drand48() * dnnl);
                in6 = (int) (drand48() * dnnl);
                in7 = (int) (drand48() * dnnl);
                scattern7(in1,in2,in3,in4,in5,in6,in7);
                e = ep + en - gs;
                ie = (long int)(e*1.0E+06 + 0.5);
                if (ie < MAXDIM){
                    Ex[ie] = Ex[ie] + 1.;
                    pr[ie] = 1;
                    ne[ie] = 7;
                    pi[ie] = pp*pn;
                    po[ie][0] = ip1;
                    no[ie][0] = in1;
                    no[ie][1] = in2;
                    no[ie][2] = in3;
                    no[ie][3] = in4;
                    no[ie][4] = in5;
                    no[ie][5] = in6;
                    no[ie][6] = in7;
                }
            }
            for( i = 0; i < i1000; i++){
                trysum = trysum +1;
                ip1 = (int) (drand48() * dnpl);
                ip2 = (int) (drand48() * dnpl);
                ip3 = (int) (drand48() * dnpl);
                ip4 = (int) (drand48() * dnpl);
                ip5 = (int) (drand48() * dnpl);
                ip6 = (int) (drand48() * dnpl);
                ip7 = (int) (drand48() * dnpl);
                scatterp7(ip1,ip2,ip3,ip4,ip5,ip6,ip7);
                in1 = (int) (drand48() * dnnl);
                scattern1(in1);
                e = ep + en - gs;
                ie = (long int)(e*1.0E+06 + 0.5);
                if (ie < MAXDIM){
                    Ex[ie] = Ex[ie] + 1.;
                    pr[ie] = 7;
                    ne[ie] = 1;
                    pi[ie] = pp*pn;
                    po[ie][0] = ip1;
                    po[ie][1] = ip2;
                    po[ie][2] = ip3;
                    po[ie][3] = ip4;
                    po[ie][4] = ip5;
                    po[ie][5] = ip6;
                    po[ie][6] = ip7;
                    no[ie][0] = in1;
                }
            }
            for( i = 0; i < i1000; i++){
                trysum = trysum +1;
                ip1 = (int) (drand48() * dnpl);
                ip2 = (int) (drand48() * dnpl);
                ip3 = (int) (drand48() * dnpl);
                scatterp3(ip1,ip2,ip3);
                in1 = (int) (drand48() * dnnl);
                in2 = (int) (drand48() * dnnl);
                in3 = (int) (drand48() * dnnl);
                in4 = (int) (drand48() * dnnl);
                in5 = (int) (drand48() * dnnl);
                scattern5(in1,in2,in3,in4,in5);
                e = ep + en - gs;
                ie = (long int)(e*1.0E+06 + 0.5);
                if (ie < MAXDIM){
                    Ex[ie] = Ex[ie] + 1.;
                    pr[ie] = 3;
                    ne[ie] = 5;
                    pi[ie] = pp*pn;
                    po[ie][0] = ip1;
                    po[ie][1] = ip2;
                    po[ie][2] = ip3;
                    no[ie][0] = in1;
                    no[ie][1] = in2;
                    no[ie][2] = in3;
                    no[ie][3] = in4;
                    no[ie][4] = in5;
                }
            }
            for( i = 0; i < i1000; i++){
                trysum = trysum +1;
                ip1 = (int) (drand48() * dnpl);
                ip2 = (int) (drand48() * dnpl);
                ip3 = (int) (drand48() * dnpl);
                ip4 = (int) (drand48() * dnpl);
                ip5 = (int) (drand48() * dnpl);
                scatterp5(ip1,ip2,ip3,ip4,ip5);
                in1 = (int) (drand48() * dnnl);
                in2 = (int) (drand48() * dnnl);
                in3 = (int) (drand48() * dnnl);
                scattern3(in1,in2,in3);
                e = ep + en - gs;
                ie = (long int)(e*1.0E+06 + 0.5);
                if (ie < MAXDIM){
                    Ex[ie] = Ex[ie] + 1.;
                    pr[ie] = 5;
                    ne[ie] = 3;
                    pi[ie] = pp*pn;
                    po[ie][0] = ip1;
                    po[ie][1] = ip2;
                    po[ie][2] = ip3;
                    po[ie][3] = ip4;
                    po[ie][4] = ip5;
                    no[ie][0] = in1;
                    no[ie][1] = in2;
                    no[ie][2] = in3;
                }
            }
            if(j == 200 && j < jtenth){
                secstot = ((float)clock()-secs0)/(float)CLOCKS_PER_SEC;
                secstot = ((float)outerloop/(float)j)*secstot;
                timeleft();
                printf("Rough estimate of total CPU time: %dhours %dmin %dsec\n",ihours, iminu, isec);
            }
            if(j == jtenth){
                secstot = ((float)clock()-secs0)/(float)CLOCKS_PER_SEC;
                secstot = ((float)outerloop/(float)jtenth)*secstot;
            }
            if((j/jtenth)*jtenth==j && j > 0){
                timeleft();
                printf("\nLoop %d finished (remaining time: %dhours %dmin %dsec)\n",j,ihours, iminu, isec);
            }
        }
    }

    if(op == 1 && on == 0){                 /* odd even nucleus */
        for( j = 0; j < outerloop; j++){
            for( i = 0; i < i10; i++){
                trysum = trysum +1;
                ip1 = (int) (drand48() * dnpl);
                scatterp1(ip1);
                e = ep - gs;
                ie = (long int)(e*1.0E+06 + 0.5);
                if (ie < MAXDIM){
                    Ex[ie] = Ex[ie] + 1.;
                    pr[ie] = 1;
                    ne[ie] = 0;
                    pi[ie] = pp;
                    po[ie][0] = ip1;
                }
            }
            for( i = 0; i < i10; i++){
                trysum = trysum +1;
                ip1 = (int) (drand48() * dnpl);
                ip2 = (int) (drand48() * dnpl);
                ip3 = (int) (drand48() * dnpl);
                scatterp3(ip1,ip2,ip3);
                e = ep - gs;
                ie = (long int)(e*1.0E+06 + 0.5);
                if (ie < MAXDIM){
                    Ex[ie] = Ex[ie] + 1.;
                    pr[ie] = 3;
                    ne[ie] = 0;
                    pi[ie] = pp;
                    po[ie][0] = ip1;
                    po[ie][1] = ip2;
                    po[ie][2] = ip3;
                }
            }
            for( i = 0; i < i10; i++){
                trysum = trysum +1;
                ip1 = (int) (drand48() * dnpl);
                scatterp1(ip1);
                in1 = (int) (drand48() * dnnl);
                in2 = (int) (drand48() * dnnl);
                scattern2(in1,in2);
                e = ep + en - gs;
                ie = (long int)(e*1.0E+06 + 0.5);
                if (ie < MAXDIM){
                    Ex[ie] = Ex[ie] + 1.;
                    pr[ie] = 1;
                    ne[ie] = 2;
                    pi[ie] = pp*pn;

                    po[ie][0] = ip1;
                    no[ie][0] = in1;
                    no[ie][1] = in2;
                }
            }
            for( i = 0; i < i100; i++){
                trysum = trysum +1;
                ip1 = (int) (drand48() * dnpl);
                ip2 = (int) (drand48() * dnpl);
                ip3 = (int) (drand48() * dnpl);
                scatterp3(ip1,ip2,ip3);
                in1 = (int) (drand48() * dnnl);
                in2 = (int) (drand48() * dnnl);
                scattern2(in1,in2);
                e = ep + en - gs;
                ie = (long int)(e*1.0E+06 + 0.5);
                if (ie < MAXDIM){
                    Ex[ie] = Ex[ie] + 1.;
                    pr[ie] = 3;
                    ne[ie] = 2;
                    pi[ie] = pp*pn;

                    po[ie][0] = ip1;
                    po[ie][1] = ip2;
                    po[ie][2] = ip3;
                    no[ie][0] = in1;
                    no[ie][1] = in2;
                }
            }
            for( i = 0; i < i100; i++){
                trysum = trysum +1;
                ip1 = (int) (drand48() * dnpl);
                scatterp1(ip1);
                in1 = (int) (drand48() * dnnl);
                in2 = (int) (drand48() * dnnl);
                in3 = (int) (drand48() * dnnl);
                in4 = (int) (drand48() * dnnl);
                scattern4(in1,in2,in3,in4);
                e = ep + en - gs;
                ie = (long int)(e*1.0E+06 + 0.5);
                if (ie < MAXDIM){
                    Ex[ie] = Ex[ie] + 1.;
                    pr[ie] = 1;
                    ne[ie] = 4;
                    pi[ie] = pp*pn;
                    po[ie][0] = ip1;
                    no[ie][0] = in1;
                    no[ie][1] = in2;
                    no[ie][2] = in3;
                    no[ie][3] = in4;
                }
            }
            for( i = 0; i < i100; i++){
                trysum = trysum +1;
                ip1 = (int) (drand48() * dnpl);
                ip2 = (int) (drand48() * dnpl);
                ip3 = (int) (drand48() * dnpl);
                ip4 = (int) (drand48() * dnpl);
                ip5 = (int) (drand48() * dnpl);
                scatterp5(ip1,ip2,ip3,ip4,ip5);
                e = ep - gs;
                ie = (long int)(e*1.0E+06 + 0.5);
                if (ie < MAXDIM){
                    Ex[ie] = Ex[ie] + 1.;
                    pr[ie] = 5;
                    ne[ie] = 0;
                    pi[ie] = pp;
                    po[ie][0] = ip1;
                    po[ie][1] = ip2;
                    po[ie][2] = ip3;
                    po[ie][3] = ip4;
                    po[ie][4] = ip5;
                }
            }
            for( i = 0; i < i1000; i++){
                trysum = trysum +1;
                ip1 = (int) (drand48() * dnpl);
                scatterp1(ip1);
                in1 = (int) (drand48() * dnnl);
                in2 = (int) (drand48() * dnnl);
                in3 = (int) (drand48() * dnnl);
                in4 = (int) (drand48() * dnnl);
                in5 = (int) (drand48() * dnnl);
                in6 = (int) (drand48() * dnnl);
                scattern6(in1,in2,in3,in4,in5,in6);
                e = ep + en - gs;
                ie = (long int)(e*1.0E+06 + 0.5);
                if (ie < MAXDIM){
                    Ex[ie] = Ex[ie] + 1.;
                    pr[ie] = 1;
                    ne[ie] = 6;
                    pi[ie] = pp*pn;
                    po[ie][0] = ip1;
                    no[ie][0] = in1;
                    no[ie][1] = in2;
                    no[ie][2] = in3;
                    no[ie][3] = in4;
                    no[ie][4] = in5;
                    no[ie][5] = in6;
                }
            }
            for( i = 0; i < i1000; i++){
                trysum = trysum +1;
                ip1 = (int) (drand48() * dnpl);
                ip2 = (int) (drand48() * dnpl);
                ip3 = (int) (drand48() * dnpl);
                scatterp3(ip1,ip2,ip3);
                in1 = (int) (drand48() * dnnl);
                in2 = (int) (drand48() * dnnl);
                in3 = (int) (drand48() * dnnl);
                in4 = (int) (drand48() * dnnl);
                scattern4(in1,in2,in3,in4);
                e = ep + en - gs;
                ie = (long int)(e*1.0E+06 + 0.5);
                if (ie < MAXDIM){
                    Ex[ie] = Ex[ie] + 1.;
                    pr[ie] = 3;
                    ne[ie] = 4;
                    pi[ie] = pp*pn;
                    po[ie][0] = ip1;
                    po[ie][1] = ip2;
                    po[ie][2] = ip3;
                    no[ie][0] = in1;
                    no[ie][1] = in2;
                    no[ie][2] = in3;
                    no[ie][3] = in4;
                }
            }
            for( i = 0; i < i1000; i++){
                trysum = trysum +1;
                ip1 = (int) (drand48() * dnpl);
                ip2 = (int) (drand48() * dnpl);
                ip3 = (int) (drand48() * dnpl);
                ip4 = (int) (drand48() * dnpl);
                ip5 = (int) (drand48() * dnpl);
                scatterp5(ip1,ip2,ip3,ip4,ip5);
                in1 = (int) (drand48() * dnnl);
                in2 = (int) (drand48() * dnnl);
                scattern2(in1,in2);
                e = ep + en - gs;
                ie = (long int)(e*1.0E+06 + 0.5);
                if (ie < MAXDIM){
                    Ex[ie] = Ex[ie] + 1.;
                    pr[ie] = 5;
                    ne[ie] = 2;
                    pi[ie] = pp*pn;
                    po[ie][0] = ip1;
                    po[ie][1] = ip2;
                    po[ie][2] = ip3;
                    po[ie][3] = ip4;
                    po[ie][4] = ip5;
                    no[ie][0] = in1;
                    no[ie][1] = in2;
                }
            }
            for( i = 0; i < i1000; i++){
                trysum = trysum +1;
                ip1 = (int) (drand48() * dnpl);
                ip2 = (int) (drand48() * dnpl);
                ip3 = (int) (drand48() * dnpl);
                ip4 = (int) (drand48() * dnpl);
                ip5 = (int) (drand48() * dnpl);
                ip6 = (int) (drand48() * dnpl);
                ip7 = (int) (drand48() * dnpl);
                scatterp7(ip1,ip2,ip3,ip4,ip5,ip6,ip7);
                e = ep - gs;
                ie = (long int)(e*1.0E+06 + 0.5);
                if (ie < MAXDIM){
                    Ex[ie] = Ex[ie] + 1.;
                    pr[ie] = 7;
                    ne[ie] = 0;
                    pi[ie] = pp;
                    po[ie][0] = ip1;
                    po[ie][1] = ip2;
                    po[ie][2] = ip3;
                    po[ie][3] = ip4;
                    po[ie][4] = ip5;
                    po[ie][5] = ip6;
                    po[ie][6] = ip7;
                }
            }
            if(j == 200 && j < jtenth){
                secstot = ((float)clock()-secs0)/(float)CLOCKS_PER_SEC;
                secstot = ((float)outerloop/(float)j)*secstot;
                timeleft();
                printf("Rough estimate of total CPU time: %dhours %dmin %dsecs\n",ihours, iminu, isec);
            }
            if(j == jtenth){
                secstot = ((float)clock()-secs0)/(float)CLOCKS_PER_SEC;
                secstot = ((float)outerloop/(float)jtenth)*secstot;
            }
            if((j/jtenth)*jtenth==j && j > 0){
                timeleft();
                printf("Loop %d finished (remaining time: %dhours %dmin %dsecs)\n",j,ihours, iminu, isec);
            }
        }
    }

    if(op == 0 && on == 1){                 /* even odd nucleus */
        for( j = 0; j < outerloop; j++){
            for( i = 0; i < i10; i++){
                trysum = trysum +1;
                in1 = (int) (drand48() * dnnl);
                scattern1(in1);
                e = en - gs;
                ie = (long int)(e*1.0E+06 + 0.5);
                if (ie < MAXDIM){
                    Ex[ie] = Ex[ie] + 1.;
                    pr[ie] = 0;
                    ne[ie] = 1;
                    pi[ie] = pn;
                    no[ie][0] = in1;
                }
            }
            for( i = 0; i < i10; i++){
                trysum = trysum +1;
                in1 = (int) (drand48() * dnnl);
                in2 = (int) (drand48() * dnnl);
                in3 = (int) (drand48() * dnnl);
                scattern3(in1,in2,in3);
                e = en - gs;
                ie = (long int)(e*1.0E+06 + 0.5);
                if (ie < MAXDIM){
                    Ex[ie] = Ex[ie] + 1.;
                    pr[ie] = 0;
                    ne[ie] = 3;
                    pi[ie] = pn;
                    no[ie][0] = in1;
                    no[ie][1] = in2;
                    no[ie][2] = in3;
                }
            }
            for( i = 0; i < i10; i++){
                trysum = trysum +1;
                ip1 = (int) (drand48() * dnpl);
                ip2 = (int) (drand48() * dnpl);
                scatterp2(ip1,ip2);
                in1 = (int) (drand48() * dnnl);
                scattern1(in1);
                e = ep + en - gs;
                ie = (long int)(e*1.0E+06 + 0.5);
                if (ie < MAXDIM){
                    Ex[ie] = Ex[ie] + 1.;
                    pr[ie] = 2;
                    ne[ie] = 1;
                    pi[ie] = pp*pn;
                    po[ie][0] = ip1;
                    po[ie][1] = ip2;
                    no[ie][0] = in1;
                }
            }
            for( i = 0; i < i100; i++){
                trysum = trysum +1;
                ip1 = (int) (drand48() * dnpl);
                ip2 = (int) (drand48() * dnpl);
                scatterp2(ip1,ip2);
                in1 = (int) (drand48() * dnnl);
                in2 = (int) (drand48() * dnnl);
                in3 = (int) (drand48() * dnnl);
                scattern3(in1,in2,in3);
                e = ep + en - gs;
                ie = (long int)(e*1.0E+06 + 0.5);
                if (ie < MAXDIM){
                    Ex[ie] = Ex[ie] + 1.;
                    pr[ie] = 2;
                    ne[ie] = 3;
                    pi[ie] = pp*pn;
                    po[ie][0] = ip1;
                    po[ie][1] = ip2;
                    no[ie][0] = in1;
                    no[ie][1] = in2;
                    no[ie][2] = in3;
                }
            }
            for( i = 0; i < i100; i++){
                trysum = trysum +1;
                ip1 = (int) (drand48() * dnpl);
                ip2 = (int) (drand48() * dnpl);
                ip3 = (int) (drand48() * dnpl);
                ip4 = (int) (drand48() * dnpl);
                scatterp4(ip1,ip2,ip3,ip4);
                in1 = (int) (drand48() * dnnl);
                scattern1(in1);
                e = ep + en - gs;
                ie = (long int)(e*1.0E+06 + 0.5);
                if (ie < MAXDIM){
                    Ex[ie] = Ex[ie] + 1.;
                    pr[ie] = 4;
                    ne[ie] = 1;
                    pi[ie] = pp*pn;
                    po[ie][0] = ip1;
                    po[ie][1] = ip2;
                    po[ie][2] = ip3;
                    po[ie][3] = ip4;
                    no[ie][0] = in1;
                }
            }
            for( i = 0; i < i100; i++){
                trysum = trysum +1;
                in1 = (int) (drand48() * dnnl);
                in2 = (int) (drand48() * dnnl);
                in3 = (int) (drand48() * dnnl);
                in4 = (int) (drand48() * dnnl);
                in5 = (int) (drand48() * dnnl);
                scattern5(in1,in2,in3,in4,in5);
                e = en - gs;
                ie = (long int)(e*1.0E+06 + 0.5);
                if (ie < MAXDIM){
                    Ex[ie] = Ex[ie] + 1.;
                    pr[ie] = 0;
                    ne[ie] = 5;
                    pi[ie] = pn;
                    no[ie][0] = in1;
                    no[ie][1] = in2;
                    no[ie][2] = in3;
                    no[ie][3] = in4;
                    no[ie][4] = in5;
                }
            }
            for( i = 0; i < i1000; i++){
                trysum = trysum +1;
                ip1 = (int) (drand48() * dnpl);
                ip2 = (int) (drand48() * dnpl);
                ip3 = (int) (drand48() * dnpl);
                ip4 = (int) (drand48() * dnpl);
                ip5 = (int) (drand48() * dnpl);
                ip6 = (int) (drand48() * dnpl);
                scatterp6(ip1,ip2,ip3,ip4,ip5,ip6);
                in1 = (int) (drand48() * dnnl);
                scattern1(in1);
                e = ep + en - gs;
                ie = (long int)(e*1.0E+06 + 0.5);
                if (ie < MAXDIM){
                    Ex[ie] = Ex[ie] + 1.;
                    pr[ie] = 6;
                    ne[ie] = 1;
                    pi[ie] = pp*pn;
                    po[ie][0] = ip1;
                    po[ie][1] = ip2;
                    po[ie][2] = ip3;
                    po[ie][3] = ip4;
                    po[ie][4] = ip5;
                    po[ie][5] = ip6;
                    no[ie][0] = in1;
                }
            }
            for( i = 0; i < i1000; i++){
                trysum = trysum +1;
                ip1 = (int) (drand48() * dnpl);
                ip2 = (int) (drand48() * dnpl);
                ip3 = (int) (drand48() * dnpl);
                ip4 = (int) (drand48() * dnpl);
                scatterp4(ip1,ip2,ip3,ip4);
                in1 = (int) (drand48() * dnnl);
                in2 = (int) (drand48() * dnnl);
                in3 = (int) (drand48() * dnnl);
                scattern3(in1,in2,in3);
                e = ep + en - gs;
                ie = (long int)(e*1.0E+06 + 0.5);
                if (ie < MAXDIM){
                    Ex[ie] = Ex[ie] + 1.;
                    pr[ie] = 4;
                    ne[ie] = 3;
                    pi[ie] = pp*pn;
                    po[ie][0] = ip1;
                    po[ie][1] = ip2;
                    po[ie][2] = ip3;
                    po[ie][3] = ip4;
                    no[ie][0] = in1;
                    no[ie][1] = in2;
                    no[ie][2] = in3;
                }
            }
            for( i = 0; i < i1000; i++){
                trysum = trysum +1;
                ip1 = (int) (drand48() * dnpl);
                ip2 = (int) (drand48() * dnpl);
                scatterp2(ip1,ip2);
                in1 = (int) (drand48() * dnnl);
                in2 = (int) (drand48() * dnnl);
                in3 = (int) (drand48() * dnnl);
                in4 = (int) (drand48() * dnnl);
                in5 = (int) (drand48() * dnnl);
                scattern5(in1,in2,in3,in4,in5);
                e = ep + en - gs;
                ie = (long int)(e*1.0E+06 + 0.5);
                if (ie < MAXDIM){
                    Ex[ie] = Ex[ie] + 1.;
                    pr[ie] = 2;
                    ne[ie] = 5;
                    pi[ie] = pp*pn;
                    po[ie][0] = ip1;
                    po[ie][1] = ip2;
                    no[ie][0] = in1;
                    no[ie][1] = in2;
                    no[ie][2] = in3;
                    no[ie][3] = in4;
                    no[ie][4] = in5;
                }
            }
            for( i = 0; i < i1000; i++){
                trysum = trysum +1;
                in1 = (int) (drand48() * dnnl);
                in2 = (int) (drand48() * dnnl);
                in3 = (int) (drand48() * dnnl);
                in4 = (int) (drand48() * dnnl);
                in5 = (int) (drand48() * dnnl);
                in6 = (int) (drand48() * dnnl);
                in7 = (int) (drand48() * dnnl);
                scattern7(in1,in2,in3,in4,in5,in6,in7);
                e = en - gs;
                ie = (long int)(e*1.0E+06 + 0.5);
                if (ie < MAXDIM){
                    Ex[ie] = Ex[ie] + 1.;
                    pr[ie] = 0;
                    ne[ie] = 7;
                    pi[ie] = pn;
                    no[ie][0] = in1;
                    no[ie][1] = in2;
                    no[ie][2] = in3;
                    no[ie][3] = in4;
                    no[ie][4] = in5;
                    no[ie][5] = in6;
                    no[ie][6] = in7;
                }
            }
            if(j == 200 && j < jtenth){
                secstot = ((float)clock()-secs0)/(float)CLOCKS_PER_SEC;
                secstot = ((float)outerloop/(float)j)*secstot;
                timeleft();
                printf("Rough estimate of total CPU time: %dhours %dmin %dsecs\n",ihours, iminu, isec);
            }
            if(j == jtenth){
                secstot = ((float)clock()-secs0)/(float)CLOCKS_PER_SEC;
                secstot = ((float)outerloop/(float)jtenth)*secstot;
            }
            if((j/jtenth)*jtenth==j && j > 0){
                timeleft();
                printf("Loop %d finished (remaining time: %dhours %dmin %dsecs)\n",j,ihours, iminu, isec);
            }
        }
    }

    if(op == 0 && on == 0){                 /* even even nucleus */
        Ex[0] = 1.;                         /* ground state defined */
        pi[0] = 1;                          /* ground state has parity + */
        pr[0] = 1;
        ne[0] = 1;
        for( j = 0; j < outerloop; j++){
            for( i = 0; i < i10; i++){
                trysum = trysum +1;
                ip1 = (int) (drand48() * dnpl);
                ip2 = (int) (drand48() * dnpl);
                scatterp2(ip1,ip2);
                e = ep - gs;
                ie = (long int)(e*1.0E+06 + 0.5);
                if (ie < MAXDIM){
                    Ex[ie] = Ex[ie] + 1.;
                    pr[ie] = 2;
                    ne[ie] = 0;
                    pi[ie] = pp;
                    po[ie][0] = ip1;
                    po[ie][1] = ip2;
                }
            }
            for( i = 0; i < i10; i++){
                trysum = trysum +1;
                in1 = (int) (drand48() * dnnl);
                in2 = (int) (drand48() * dnnl);
                scattern2(in1,in2);
                e = en - gs;
                ie = (long int)(e*1.0E+06 + 0.5);
                if (ie < MAXDIM){
                    Ex[ie] = Ex[ie] + 1.;
                    pr[ie] = 0;
                    ne[ie] = 2;
                    pi[ie] = pn;
                    no[ie][0] = in1;
                    no[ie][1] = in2;
                }
            }
            for( i = 0; i < i100; i++){
                trysum = trysum +1;
                ip1 = (int) (drand48() * dnpl);
                ip2 = (int) (drand48() * dnpl);
                scatterp2(ip1,ip2);
                in1 = (int) (drand48() * dnnl);
                in2 = (int) (drand48() * dnnl);
                scattern2(in1,in2);
                e = ep + en - gs;
                ie = (long int)(e*1.0E+06 + 0.5);
                if (ie < MAXDIM){
                    Ex[ie] = Ex[ie] + 1.;
                    pr[ie] = 2;
                    ne[ie] = 2;
                    pi[ie] = pp*pn;
                    po[ie][0] = ip1;
                    po[ie][1] = ip2;
                    no[ie][0] = in1;
                    no[ie][1] = in2;
                }
            }
            for( i = 0; i < i100; i++){
                trysum = trysum +1;
                in1 = (int) (drand48() * dnnl);
                in2 = (int) (drand48() * dnnl);
                in3 = (int) (drand48() * dnnl);
                in4 = (int) (drand48() * dnnl);
                scattern4(in1,in2,in3,in4);
                e = en - gs;
                ie = (long int)(e*1.0E+06 + 0.5);
                if (ie < MAXDIM){
                    Ex[ie] = Ex[ie] + 1.;
                    pr[ie] = 0;
                    ne[ie] = 4;
                    pi[ie] = pn;
                    no[ie][0] = in1;
                    no[ie][1] = in2;
                    no[ie][2] = in3;
                    no[ie][3] = in4;
                }
            }
            for( i = 0; i < i100; i++){
                trysum = trysum +1;
                ip1 = (int) (drand48() * dnpl);
                ip2 = (int) (drand48() * dnpl);
                ip3 = (int) (drand48() * dnpl);
                ip4 = (int) (drand48() * dnpl);
                scatterp4(ip1,ip2,ip3,ip4);
                e = ep - gs;
                ie = (long int)(e*1.0E+06 + 0.5);
                if (ie < MAXDIM){
                    Ex[ie] = Ex[ie] + 1.;
                    pr[ie] = 4;
                    ne[ie] = 0;
                    pi[ie] = pp;
                    po[ie][0] = ip1;
                    po[ie][1] = ip2;
                    po[ie][2] = ip3;
                    po[ie][3] = ip4;
                }
            }
            for( i = 0; i < i1000; i++){
                trysum = trysum +1;
                in1 = (int) (drand48() * dnnl);
                in2 = (int) (drand48() * dnnl);
                in3 = (int) (drand48() * dnnl);
                in4 = (int) (drand48() * dnnl);
                in5 = (int) (drand48() * dnnl);
                in6 = (int) (drand48() * dnnl);
                scattern6(in1,in2,in3,in4,in5,in6);
                e = en - gs;
                ie = (long int)(e*1.0E+06 + 0.5);
                if (ie < MAXDIM){
                    Ex[ie] = Ex[ie] + 1.;
                    pr[ie] = 0;
                    ne[ie] = 6;
                    no[ie][0] = in1;
                    no[ie][1] = in2;
                    no[ie][2] = in3;
                    no[ie][3] = in4;
                    no[ie][4] = in5;
                    no[ie][5] = in6;
                }
            }
            for( i = 0; i < i1000; i++){
                trysum = trysum +1;
                ip1 = (int) (drand48() * dnpl);
                ip2 = (int) (drand48() * dnpl);
                ip3 = (int) (drand48() * dnpl);
                ip4 = (int) (drand48() * dnpl);
                ip5 = (int) (drand48() * dnpl);
                ip6 = (int) (drand48() * dnpl);
                scatterp6(ip1,ip2,ip3,ip4,ip5,ip6);
                e = ep - gs;
                ie = (long int)(e*1.0E+06 + 0.5);
                if (ie < MAXDIM){
                    Ex[ie] = Ex[ie] + 1.;
                    pr[ie] = 6;
                    ne[ie] = 0;
                    pi[ie] = pp;
                    po[ie][0] = ip1;
                    po[ie][1] = ip2;
                    po[ie][2] = ip3;
                    po[ie][3] = ip4;
                    po[ie][4] = ip5;
                    po[ie][5] = ip6;
                }
            }
            for( i = 0; i < i1000; i++){
                trysum = trysum +1;
                ip1 = (int) (drand48() * dnpl);
                ip2 = (int) (drand48() * dnpl);
                scatterp2(ip1,ip2);
                in1 = (int) (drand48() * dnnl);
                in2 = (int) (drand48() * dnnl);
                in3 = (int) (drand48() * dnnl);
                in4 = (int) (drand48() * dnnl);
                scattern4(in1,in2,in3,in4);
                e = ep + en - gs;
                ie = (long int)(e*1.0E+06 + 0.5);
                if (ie < MAXDIM){
                    Ex[ie] = Ex[ie] + 1.;
                    pr[ie] = 2;
                    ne[ie] = 4;
                    pi[ie] = pp*pn;
                    po[ie][0] = ip1;
                    po[ie][1] = ip2;
                    no[ie][0] = in1;
                    no[ie][1] = in2;
                    no[ie][2] = in3;
                    no[ie][3] = in4;
                }
            }
            for( i = 0; i < i1000; i++){
                trysum = trysum +1;
                ip1 = (int) (drand48() * dnpl);
                ip2 = (int) (drand48() * dnpl);
                ip3 = (int) (drand48() * dnpl);
                ip4 = (int) (drand48() * dnpl);
                scatterp4(ip1,ip2,ip3,ip4);
                in1 = (int) (drand48() * dnnl);
                in2 = (int) (drand48() * dnnl);
                scattern2(in1,in2);
                e = ep + en - gs;
                ie = (long int)(e*1.0E+06 + 0.5);
                if (ie < MAXDIM){
                    Ex[ie] = Ex[ie] + 1.;
                    pr[ie] = 4;
                    ne[ie] = 2;
                    pi[ie] = pp*pn;
                    po[ie][0] = ip1;
                    po[ie][1] = ip2;
                    po[ie][2] = ip3;
                    po[ie][3] = ip4;
                    no[ie][0] = in1;
                    no[ie][1] = in2;
                }
            }

            if(j == 200 && j < jtenth){
                secstot = ((float)clock()-secs0)/(float)CLOCKS_PER_SEC;
                secstot = ((float)outerloop/(float)j)*secstot;
                timeleft();
                printf("Rough estimate of total CPU time: %dhours %dmin %dsecs\n",ihours, iminu, isec);
            }
            if(j == jtenth){
                secstot = ((float)clock()-secs0)/(float)CLOCKS_PER_SEC;
                secstot = ((float)outerloop/(float)jtenth)*secstot;
            }
            if((j/jtenth)*jtenth==j && j > 0){
                timeleft();
                printf("Loop %d finished (remaining time: %dhours %dmin %dsecs)\n",j,ihours, iminu, isec);
            }
        }
    }

    printf("Loop %d finished\n",outerloop);
    printf("\n List of band heads found per eV:");
    for( i = 0; i < MAXLEN; i++){
        Spec[i]=0;
    }

    /* The time-consuming computation is ended, and we fix up what is left  */
    /* Here we add on other contributions to the level density:             */
    /* - for manyquasiparticle states, the Omegas may align +/- for p and n */
    /* - vibrational states added by hand in the next statements below      */
    /* - rotational states 2+, 4+, 6+ and 8+ are built on banheads          */

    /* First take the vibrations build on intrinsic states                  */
    /* The 9999xx numbers is just to identify these states in output        */
    /* Assumes 100 keV interaction, 50 keV up or down                       */

    /* One one-phonon state */
    i = (int)(1. * Evib * 1.0E+6);
    Ex[i]   = 999910;
    pi[i] = pi_vib;

    /* Three two-phonon state */
    i = (int)(2. * Evib * 1.0E+6);

    irand = ranint(fwhm);
    Ex[i + irand] = 999921;
    pi[i + irand] = pi_vib*pi_vib;

    irand = ranint(fwhm);
    Ex[i + irand] = 999922;
    pi[i + irand] = pi_vib*pi_vib;

    irand = ranint(fwhm);
    Ex[i + irand] = 999923;
    pi[i + irand] = pi_vib*pi_vib;

    /* Nine three-phonon states */
    i = (int)(3. * Evib * 1.0E+6);

    irand = ranint(fwhm);
    Ex[i + irand] = 999931;
    pi[i + irand] = pi_vib*pi_vib*pi_vib;

    irand = ranint(fwhm);
    Ex[i + irand] = 999932;
    pi[i + irand] = pi_vib*pi_vib*pi_vib;

    irand = ranint(fwhm);
    Ex[i + irand] = 999933;
    pi[i + irand] = pi_vib*pi_vib*pi_vib;
    irand = ranint(fwhm);

    Ex[i + irand] = 999934;
    pi[i + irand] = pi_vib*pi_vib*pi_vib;

    irand = ranint(fwhm);
    Ex[i + irand] = 999935;
    pi[i + irand] = pi_vib*pi_vib*pi_vib;

    irand = ranint(fwhm);
    Ex[i + irand] = 999936;
    pi[i + irand] = pi_vib*pi_vib*pi_vib;

    irand = ranint(fwhm);
    Ex[i + irand] = 999937;
    pi[i + irand] = pi_vib*pi_vib*pi_vib;

    irand = ranint(fwhm);
    Ex[i + irand] = 999938;
    pi[i + irand] = pi_vib*pi_vib*pi_vib;

    irand = ranint(fwhm);
    Ex[i + irand] = 999939;
    pi[i + irand] = pi_vib*pi_vib*pi_vib;

    fprintf(fparam,"\n Detailed output of levels:\n");
    for(i = 0; i < MAXDIM; i++){
        if(Ex[i] > 0){
            hitsum = hitsum + Ex[i];
            iv = (int)((((float)i)/(1.0E+6)/disp)+0.5);
            npairs = (pr[i]/2) + (ne[i]/2);
            nqpar  = pr[i] + ne[i];
            iline  = iline + 1;
            if(iline < 7000){
                printf(" \n Ex(eV)= %8lu   Ex(%5.3f MeV)= %4d Type= %2d %2d Parity= %2d Hits= %8d",  i,disp,iv,pr[i],ne[i],pi[i],Ex[i]);
            }
            fprintf(fparam,"Ex(eV)= %8lu   Ex(%5.3f MeV)= %4d Type= %2d %2d Parity= %2d Hits= %8d\n",i,disp,iv,pr[i],ne[i],pi[i],Ex[i]);
            if(nqpar > 1){
                kmax = pow(2,(nqpar-1));
                //                              if(pr[i] > 0 && ne[i] > 0) kmax = 2;
                for(k = 0; k < kmax; k++){
                    iv = i + ranint(fwhm);                                                          /* Assumes 100 keV interaction, 50 keV up or down */
                    iv = (int)((((float)iv)/(1.0E+6)/disp) + 0.5);
                    if(iv < 0) iv = 0;
                    for(r = 0; r <= rmax; r = r + r0){
                        rot = (int)(((Arot*((float)r*((float)r+1.)))/disp) + 0.5);
                        Spec[ iv+rot]  = Spec[ iv+rot]  + 1.;                   /* Rotations built on bandheads */
                        Npair[iv+rot]  = Npair[iv+rot]  + npairs;               /* Counting number of pairs broken */
                        Npair_p[iv+rot]  = Npair_p[iv+rot]  + pr[i]/2;          /* Counting number of proton pairs broken */
                        Npair_n[iv+rot]  = Npair_n[iv+rot]  + ne[i]/2;          /* Counting number of neutron pairs broken */
                        Parity[iv+rot] = Parity[iv+rot] + pi[i];                /* Counting sum of parities within bin */
                        for(j = 0; j < pr[i]; j++){
                            orbitno = po[i][j];
                            Orbitp[iv+rot][orbitno] = Orbitp[iv+rot][orbitno] + (1./((rmax/2.)+1.));
                        }
                        for(j = 0; j < ne[i]; j++){
                            orbitno = no[i][j];
                            Orbitn[iv+rot][orbitno] = Orbitn[iv+rot][orbitno] + (1./((rmax/2.)+1.));
                        }
                    }
                }
            }
            else{
                for(r = 0; r <=  rmax; r = r + r0){
                    rot = (int)(((Arot*((float)r*((float)r+1.)))/disp) + 0.5);
                    Spec[ iv+rot]  = Spec[ iv+rot]  + 1.;                           /* Rotations built on bandheads */
                    Npair[iv+rot]  = Npair[iv+rot]  + npairs;                       /* Counting number of pairs broken */
                    Npair_p[iv+rot]  = Npair_p[iv+rot]  + pr[i]/2;                  /* Counting number of proton pairs broken */
                    Npair_n[iv+rot]  = Npair_n[iv+rot]  + ne[i]/2;                  /* Counting number of neutron pairs broken */
                    Parity[iv+rot] = Parity[iv+rot] + pi[i];                        /* Counting sum of parities within bin */
                    for(j = 0; j < pr[i]; j++){
                        orbitno = po[i][j];
                        Orbitp[iv+rot][orbitno] = Orbitp[iv+rot][orbitno] + (1./((rmax/2.)+1.));
                    }
                    for(j = 0; j < ne[i]; j++){
                        orbitno = no[i][j];
                        Orbitn[iv+rot][orbitno] = Orbitn[iv+rot][orbitno] + (1./((rmax/2.)+1.));
                    }
                }
            }
        }
    }

    for( i = 0; i < iSpec; i++){
        if(Spec[i] > 0) Npair[i]        = Npair[i]/Spec[i];         /* Making average pairs broken */
        if(Spec[i] > 0) Npair_p[i]  = Npair_p[i]/Spec[i];           /* Making average pairs broken */
        if(Spec[i] > 0) Npair_n[i]  = Npair_n[i]/Spec[i];           /* Making average pairs broken */
        if(Spec[i] > 0) Parity[i]       = Parity[i]/Spec[i];        /* Making average parity */
        for( j = 0; j < MAXORBITS; j++){
            if(Spec[i] > 0) Orbitp[i][j] = Orbitp[i][j]/Spec[i];    /* Making average protons */
            if(Spec[i] > 0) Orbitn[i][j] = Orbitn[i][j]/Spec[i];    /* Making average neutrons */
        }
        totsum   = totsum + (int)Spec[i];                           /* Total number of levels */
        Spec[i]  = Spec[i] / disp;                                  /* Normalize to number of levels per MeV */
    }

    fp = fopen("microin.dat", "w");
    if(fp == NULL){
        printf("Could not open file microin.dat \n");
        exit(0);
    }
    else {
        fprintf(fp," %d %d %f %f %f %f %f %f %f %f %f %d\n", np, nn, pEpair, nEpair, facpair, Arot, Evib, Emaxdum, disp, sc_p, sc_n, pi_vib);
    }
    fclose(fp);

    fp = fopen("spec.paw", "w");
    if(fp == NULL){
        printf("Could not open spec.paw \n");
        exit(0);
    }
    for( i = 0; i < iSpec; i++){
        fprintf(fp," %f\n", Spec[i]);
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

    fp = fopen("orbitp.paw", "w");
    if(fp == NULL){
        printf("Could not open orbitp.paw \n");
        exit(0);
    }
    for( i = 0; i < iSpec; i++){
        for( j = 0; j < maxorb; j++){
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
        for( j = 0; j < maxorb; j++){
            fprintf(fp," %f\n", Orbitn[i][j]);
        }
    }
    fclose(fp);
    fclose(fparam);

    printf("\n\n Total number of levels =              %12ld",totsum);
    printf("\n Number of outer loops  =              %12d",outerloop);
    printf("\n Number of total tries  =              %12ld",trysum);
    printf("\n Number of valid hits   =              %12ld",hitsum);
    printf("\n Probability of valid hits =           %12.4f\n",((double)hitsum)/((double)trysum));
    return 0;
}

void scatterp1(int i1) {
    ep = dqpp[i1][0];
    pp = pip[i1];
    return;
}
void scatterp2(int i1,int i2) {
    if( i1==i2){
        ep=1000.;
        return;
    }
    ep = dqpp[i1][1] + dqpp[i2][1];
    pp = pip[i1]*pip[i2];
    return;
}
void scatterp3(int i1,int i2,int i3) {
    if( i1==i2||
        i1==i3||

        i2==i3){
        ep=1000.;
        return;
    }
    ep = dqpp[i1][1] + dqpp[i2][1] + dqpp[i3][1];
    pp = pip[i1]*pip[i2]*pip[i3];
    return;
}
void scatterp4(int i1,int i2,int i3,int i4) {
    if( i1==i2||
        i1==i3||
        i1==i4||

        i2==i3||
        i2==i4||

        i3==i4){
        ep=1000.;
        return;
    }
    ep = dqpp[i1][2] + dqpp[i2][2] + dqpp[i3][2] + dqpp[i4][2];
    pp = pip[i1]*pip[i2]*pip[i3]*pip[i4];
    return;
}
void scatterp5(int i1,int i2,int i3,int i4,int i5) {
    if( i1==i2||
        i1==i3||
        i1==i4||
        i1==i5||

        i2==i3||
        i2==i4||
        i2==i5||

        i3==i4||
        i3==i5||

        i4==i5){
        ep=1000.;
        return;
    }
    ep = dqpp[i1][2] + dqpp[i2][2] + dqpp[i3][2] + dqpp[i4][2] + dqpp[i5][2];
    pp = pip[i1]*pip[i2]*pip[i3]*pip[i4]*pip[i5];
    return;
}
void scatterp6(int i1,int i2,int i3,int i4,int i5,int i6) {
    if( i1==i2||
        i1==i3||
        i1==i4||
        i1==i5||
        i1==i6||

        i2==i3||
        i2==i4||
        i2==i5||
        i2==i6||

        i3==i4||
        i3==i5||
        i3==i6||

        i4==i5||
        i4==i6||

        i5==i6){
        ep=1000.;
        return;
    }
    ep = dqpp[i1][3] + dqpp[i2][3] + dqpp[i3][3] + dqpp[i4][3] + dqpp[i5][3] + dqpp[i6][3];
    pp = pip[i1]*pip[i2]*pip[i3]*pip[i4]*pip[i5]*pip[i6];
    return;
}
void scatterp7(int i1,int i2,int i3,int i4,int i5,int i6,int i7) {
    if( i1==i2||
        i1==i3||
        i1==i4||
        i1==i5||
        i1==i6||
        i1==i7||

        i2==i3||
        i2==i4||
        i2==i5||
        i2==i6||
        i2==i7||

        i3==i4||
        i3==i5||
        i3==i6||
        i3==i7||

        i4==i5||
        i4==i6||
        i4==i7||

        i5==i6||
        i5==i7||

        i6==i7){
        ep=1000.;
        return;
    }
    ep = dqpp[i1][3] + dqpp[i2][3] + dqpp[i3][3] + dqpp[i4][3] + dqpp[i5][3] + dqpp[i6][3] + dqpp[i7][3];
    pp = pip[i1]*pip[i2]*pip[i3]*pip[i4]*pip[i5]*pip[i6]*pip[i7];
    return;
}

void scattern1(int i1) {
    en = dqpn[i1][0];
    pn = pin[i1];
    return;
}
void scattern2(int i1,int i2) {
    if( i1==i2){

        en=1000.;
        return;
    }
    en = dqpn[i1][1] + dqpn[i2][1];
    pn = pin[i1]*pin[i2];
    return;
}
void scattern3(int i1,int i2,int i3) {
    if( i1==i2||
        i1==i3||

        i2==i3){

        en=1000.;
        return;
    }
    en = dqpn[i1][1] + dqpn[i2][1] + dqpn[i3][1];
    pn = pin[i1]*pin[i2]*pin[i3];
    return;
}
void scattern4(int i1,int i2,int i3,int i4) {
    if( i1==i2||
        i1==i3||
        i1==i4||

        i2==i3||
        i2==i4||

        i3==i4){

        en=1000.;
        return;
    }
    en = dqpn[i1][2] + dqpn[i2][2] + dqpn[i3][2] + dqpn[i4][2];
    pn = pin[i1]*pin[i2]*pin[i3]*pin[i4];
    return;
}
void scattern5(int i1,int i2,int i3,int i4,int i5) {
    if( i1==i2||
        i1==i3||
        i1==i4||
        i1==i5||

        i2==i3||
        i2==i4||
        i2==i5||

        i3==i4||
        i3==i5||

        i4==i5){

        en=1000.;
        return;
    }
    en = dqpn[i1][2] + dqpn[i2][2] + dqpn[i3][2] + dqpn[i4][2] + dqpn[i5][2];
    pn = pin[i1]*pin[i2]*pin[i3]*pin[i4]*pin[i5];
    return;
}
void scattern6(int i1,int i2,int i3,int i4,int i5,int i6)
{
    if( i1==i2||
        i1==i3||
        i1==i4||
        i1==i5||
        i1==i6||

        i2==i3||
        i2==i4||
        i2==i5||
        i2==i6||

        i3==i4||
        i3==i5||
        i3==i6||

        i4==i5||
        i4==i6||

        i5==i6){

        en=1000.;
        return;
    }
    en = dqpn[i1][3] + dqpn[i2][3] + dqpn[i3][3] + dqpn[i4][3] + dqpn[i5][3] + dqpn[i6][3];
    pn = pin[i1]*pin[i2]*pin[i3]*pin[i4]*pin[i5]*pin[i6];
    return;
}
void scattern7(int i1,int i2,int i3,int i4,int i5,int i6,int i7)
{
    if( i1==i2||
        i1==i3||
        i1==i4||
        i1==i5||
        i1==i6||
        i1==i7||

        i2==i3||
        i2==i4||
        i2==i5||
        i2==i6||
        i2==i7||

        i3==i4||
        i3==i5||
        i3==i6||
        i3==i7||

        i4==i5||
        i4==i6||
        i4==i7||

        i5==i6||
        i5==i7||

        i6==i7){
        en=1000.;
        return;
    }
    en = dqpn[i1][3] + dqpn[i2][3] + dqpn[i3][3] + dqpn[i4][3] + dqpn[i5][3] + dqpn[i6][3] + dqpn[i7][3];
    pn = pin[i1]*pin[i2]*pin[i3]*pin[i4]*pin[i5]*pin[i6]*pin[i7];
    return;
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
    r = drand48();
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

void timeleft()
{
    /* Estimation of remaining cpu time */
    int secrest;
    secs=(clock()-start)/CLOCKS_PER_SEC;
    secrest = secstot - secs;
    ihours  = secrest/3600;
    iminu   = (secrest -ihours*3600)/60;
    isec    = (secrest -ihours*3600 - iminu*60);
    return;
}
