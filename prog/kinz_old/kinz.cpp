#include <ctype.h>
#include <math.h>
#include <stdio.h>

#include "ask_par.h"
#include "ame2003_masses.h"

float Rho(int Z);
float Rk4(float Ek, float c, float Mr, float I, float h, long nstep, char list);
float Bethe(float Ek, float c, float Mr, float I);
float Relkin(float Mi, float Mf, float Mr, float Ti, float Q, float thetar);
float P2rel(float M, float T);

void Menu();
void Absorbator();
void Relkinematic();
void Straggling();
void Density();
void Gram();
void Meter();
void Ispin();
void Ziegler();

/************************************************************************/

int main(int argc, char* argv[])
{
    ask_par_init( argc, argv );

    char cmd;
    do {  
        const char *input = ask_par_input
            ("\n\n\n"
             " Menu: Help Bethe Reaction Straggling Density\n"
             "       Meter Gram Ispin Ziegler1985            Exit\n"
             " Press first letter : ");
        cmd = tolower(input[0]);
        switch (cmd) {
        case 'h': Menu(); break;
        case 'b': Absorbator(); break;
        case 'r': Relkinematic(); break;
        case 's': Straggling(); break;
        case 'd': Density(); break;
        case 'm': Meter(); break;
        case 'g': Gram(); break;
        case 'i': Ispin(); break;
        case 'z': Ziegler(); break;
        case 'e': break;
        default : printf(" Invalid choice. Try again\n");
        }
    } while (cmd != 'e');
    
    return 0;
}

/* function definitions....*/

void Menu()
{
    printf("\n");
    printf(" H  This command                     \n");
    printf(" B  Energyloss due to Bethes formula \n");
    printf(" R  Relativistic collision           \n");
    printf(" S  Straggling through target        \n");
    printf(" D  Density of element Z             \n");
    printf(" M  mg/cm**2 to micrometers          \n");
    printf(" G  micrometers to mg/cm**2          \n");
    printf(" I  Find Imax for direct reaction    \n");
    printf(" E  Exit program                     \n");
    printf("\r\n");
}


float Rho(int Z)
{
    float dtab[96]= {
        0.0899,0.1787,0.53  ,1.85  ,2.34  ,2.62  ,1.251 ,1.439 ,
        1.696 ,0.901 ,0.97  ,1.74  ,2.70  ,2.33  ,1.82  ,2.07  ,
        3.17  ,1.784 ,0.86  ,1.55  ,3.0   ,4.50  ,5.8   ,7.19  ,
        7.43  ,7.86  ,8.90  ,8.90  ,8.97  ,7.14  ,5.91  ,5.32  ,
        5.72  ,4.80  ,3.12  ,3.74  ,1.53  ,2.6   ,4.5   ,6.49  ,
        6.49  ,10.2  ,11.5  ,12.2  ,12.4  ,12.0  ,10.5  ,8.65  ,
        7.31  ,7.30  ,6.68  ,6.24  ,4.92  ,5.89  ,1.87  ,3.5   ,
        6.7   ,6.78  ,6.77  ,7.01  ,6.475 ,7.54  ,5.26  ,7.89  ,
        8.27  ,8.54  ,8.80  ,9.05  ,9.33  ,6.98  ,9.84  ,13.1  ,
        16.6  ,19.3  ,21.0  ,22.4  ,22.5  ,21.4  ,19.3  ,13.53 ,
        11.85 ,11.4  ,9.8   ,9.4   ,0.0   ,9.91  ,0.0   ,5.    ,
        10.07 ,11.7  ,15.4  ,18.90 ,20.4  ,19.8  ,13.6  ,13.511
    };
    
    if(Z > 96|| Z < 1) {
        fprintf(stderr, " Sorry, densities only available for 0 < Z < 97");
        return 0;
    }
    if(dtab[Z-1]<0.00001)
        fprintf(stderr, " Sorry, no density available for Z= %3d", Z);
    return dtab[Z-1];
}

void Absorbator()
{
    int Z=14, A=28, Zi=2, Ai=3;
    long nstep=1000;
    float Ti=45., hmax=19.0;
    char list='n';
    const char *input;
    float Ek, h, I, c, Mir;
   
    ask_par_int(" Give A for the absorbating medium (%3d): ", &A);
    ask_par_Z  (" Give Z for the absorbating medium (%3d/%-2s): ", &Z);
    ask_par_int(" Give A for the projectile (%3d): ", &Ai);
    ask_par_Z  (" Give Z for the projectile (%3d/%-2s): ", &Zi);

    ask_par_float(" Give the thickness (%4.1f um): ", &hmax);
    ask_par_long_int(" Give # integration step (%ld): ", &nstep);

    input = ask_par_input(" Do you want energies for each integration step (y/n) (%c): ",list);
    sscanf(input,"%c",&list);
    list=tolower(list);

    if( Rho(Z)*ame2003_get_mass_amu(Ai, Zi) < 0.0001 )
        return;

    h   = hmax/(float)nstep*1.e-4;
    I   = 9.1*Z*(1+1.9*pow(Z,(-2./3.)))*1.e-6;
    c   = 0.30707*Rho(Z)*Z/(float)A*Zi*Zi;
    Mir = ame2003_get_mass_amu(Ai, Zi)*931.5016;

    ask_par_float(" Give energy of the projectile (-1 = stop) (%3.1f): ", &Ti);

    while (Ti!=-1) {
        Ek = Rk4(Ti, c, Mir, I, h, nstep, list);
        printf("\n The final energy is %7.3f MeV with energy loss of %7.3f MeV\n",
               Ek, Ti-Ek);
        ask_par_float("\n Give energy of the projectile (-1 = stop) (%3.1f): ", &Ti);
    }
}


float Rk4(float Ek, float c, float Mr, float I, float h, long nstep, char list)
{
    long j;
    float f1, f2, f3, f4, pos;
    if(list=='y')
        printf("\n Absorbation-depth (micrometer)  Kinetic energy (MeV)");

    for(j=1; j<=nstep; j++) {
        f1 = h*Bethe(Ek,c,Mr,I);
        f2 = h*Bethe(Ek+0.5*f1,c,Mr,I);
        f3 = h*Bethe(Ek+0.5*f2,c,Mr,I);
        f4 = h*Bethe(Ek+f3,c,Mr,I);
        Ek = Ek + (f1+f2+f2+f3+f3+f4)/6.0;
        if(Ek <= 0)                {
            Ek = 0;
            pos = j*h*1.e+4;
            printf("\n        %7.3f	                      %7.3f         ",pos,Ek);
            j = nstep;
            list = 'n';
        }
        if(list=='y') {
            pos = j*h*1.e+4;
            printf("\n        %7.3f	                      %7.3f         ",pos,Ek);
        }
    }
    if(list=='y')
        printf("\n");
    return Ek;
}

float Bethe(float Ek, float c, float Mr, float I)
{
    const float tt = Ek/Mr;
    const float b2 = tt*(tt+2)/((tt+1.)*(tt+1.));
    const float Ar = 1.022*b2/(I*(1.-b2));
    float Bth=0;
    if(Ar > 1.)
        Bth = -(c/b2)*(log(Ar)-b2);
    return Bth;
}

void Relkinematic()
{
    int Zi=2, Ai=3, Zf=2, Af=4, Zr=66, Ar=172;
    float Ti=45., theta=45.;
   
    ask_par_int(" Give A for the projectile    (%3d): ", &Ai);
    ask_par_Z  (" Give Z for the projectile    (%3d/%-2s): ", &Zi);
    ask_par_int(" Give A for the light-product (%3d): ", &Af);
    ask_par_Z  (" Give Z for the light-product (%3d/%-2s): ", &Zf);
    ask_par_int(" Give A for the rest-nucleus  (%3d): ", &Ar);
    ask_par_Z  (" Give Z for the rest-nucleus  (%3d/%-2s): ", &Zr);
    ask_par_float(" Give the energy of the projectile (%3.1f MeV): ", &Ti);

    const float Q = ame2003_get_Q_keV(Ai, Zi, Af+Ar-Ai, Zf+Zr-Zi, Af, Zf)/1e3;
    printf(" AME2003 Q-value is %3.1f MeV.\n", Q);

    ask_par_float(" Give angle in degree (-1 = stop) (%3.1f): ", &theta);

    const double Mi = ame2003_get_mass_amu(Ai, Zi);
    const double Mf = ame2003_get_mass_amu(Af, Zf);
    const double Mr = ame2003_get_mass_amu(Ar, Zr);

    if( Mr<=0 || Mf<=0 || Mi<=0 )
        return;

    while(theta != -1) {
        const float thetar = theta*M_PI/180.;
        const float Ek = Relkin(Mi, Mf, Mr, Ti, Q, thetar);
        printf("\n Kinetic energy of the light-product %6.3f MeV\n",Ek);

        ask_par_float("\n Give angle in degree (-1 = stop) (%3.1f): ", &theta);
    }
}


float Relkin(float Mi, float Mf, float Mr, float Ti, float Q, float thetar)
{
    /* start values*/
    double t2f = (Ti+Q)*0.6;
    double delta = -(Ti+Q)*0.2;

    while( fabs(delta) > 1.e-10 ) {
        //printf("t2f=%3.1lf delta=%lg\n", t2f, delta);

        /* new values for the iteration: */
        const double t1f =  t2f;
        t2f += delta;
        
        const double t1r  = Q + Ti - t1f;
        const double t2r  = Q + Ti - t2f;
        const double p21r = P2rel(Mr,t1r);
        const double p22r = P2rel(Mr,t2r);
        const double p2i  = P2rel(Mi,Ti);
        const double p21f = P2rel(Mf,t1f);
        const double p22f = P2rel(Mf,t2f);
        
        const double f1 = p21r - p2i - p21f + 2*cos(thetar)*sqrt(p2i*p21f);
        const double f2 = p22r - p2i - p22f + 2*cos(thetar)*sqrt(p2i*p22f);

        //printf("f1=%lg f2=%lg\n", f1, f2);

        if( fabs(f1-f2)<1e-10 )
            break;

        /* the secant formula: */
        delta *= - 1/(1-f1/f2);
    }
    return t2f;
}


float P2rel(float M, float T)
{
    float x;
    x = 2.*931.5016*M*T + T*T;
    return(x);
}



void Straggling()
{
    int Z=70, A=173, Zi=2, Ai=3, Zf=2, Af=4;
    long nstep =1000;
    float Ti=45., d=2., Ex=0., theta=45.;
   
    ask_par_int(" Give A for the absorbating medium (%3d): ", &A);
    ask_par_Z  (" Give Z for the absorbating medium (%3d/%-2s): ", &Z);
    ask_par_int(" Give A for the projectile (%3d): ", &Ai);
    ask_par_Z  (" Give Z for the projectile (%3d/%-2s): ", &Zi);
    ask_par_int(" Give A for the light-product (%3d): ", &Af);
    ask_par_Z  (" Give Z for the light-product (%3d/%-2s): ", &Zf);

    const double Q = ame2003_get_Q_keV(Ai, Zi, A, Z, Af, Zf)/1e3;
    printf(" AME2003 Q-value is %6.4f MeV.\n", Q);

    ask_par_float(" Give energy of the projectile (%3.1f): ", &Ti);
    ask_par_float(" Give the thickness of target (mg/cm**2) (%3.1f): ", &d);
    ask_par_float(" Give exitation-energy for the nucleus (%3.1f): ", &Ex);

    const int Zr = Z + Zi - Zf;
    const int Ar = A + Ai - Af;
    const float Mi = ame2003_get_mass_amu(Ai, Zi);
    const float Mf = ame2003_get_mass_amu(Af, Zf);
    const float Mr = ame2003_get_mass_amu(Ar, Zr);
    if(Rho(Z) <=0 || Mi<=0 || Mf<=0 )
        return;

    const float I = 9.1*Z*(1.+1.9*pow(Z,(-2./3.)))*1e-6;
    const float c  = 0.30707*Rho(Z)*Z/(float)A*Zi*Zi;
    const float Mir = Mi*931.5016;
    const float Mfr = Mf*931.5016;

    ask_par_float(" Give angle in degree (-1 = stop) (%3.1f): ", &theta);
    while(theta!=-1) {
        const float thetar = theta*M_PI/180;
        const float d1  = d;
        const float d2  = d/cos(thetar);
        const float E1s = 18.*Zi*sqrt(d1);
        const float E2s = 18.*Zf*sqrt(d2);
        const float h1  = d1/Rho(Z)*1.e-5;
        const float h2  = d2/Rho(Z)*1.e-5;
        const float dh1 = h1/(float)nstep*100.;
        const float dh2 = h2/(float)nstep*100.;

        float Ek1 = Rk4(Ti,c,Mir,I,dh1,nstep,'n');
        printf("Ek1 after target=%.5g\n", Ek1);
        Ek1 = Relkin(Mi,Mf,Mr,Ek1,Q,thetar) - Ex;
        printf("Ek1 after relkin=%.5g\n", Ek1);

        float Ek2 = Relkin(Mi,Mf,Mr,Ti,Q,thetar) - Ex;
        printf("Ek2 after relkin=%.5g\n", Ek2);
        Ek2 = Rk4(Ek2,c,Mfr,I,dh2,nstep,'n');
        printf("Ek2 after target=%.5g\n", Ek2);

        const float Ekf = sqrt(.5*(Ek1*Ek1+Ek2*Ek2));
        const float dE  = sqrt(((Ek1-Ek2)*1000.)*((Ek1-Ek2)*1000.) + 0.5*(E1s*E1s+E2s*E2s));

        printf("de: dE_1=%f dE_2=%f\n", Ek1-Ek2, 0.5*(E1s*E1s+E2s*E2s));
        printf("\n The final energy is %6.3f MeV  with FWHM %5.1f keV\n",Ekf,dE);

        ask_par_float("\n Give angle in degree (-1 = stop) (%3.1f): ", &theta);
    }
}

void Density()
{
    int Z = 13;
    ask_par_Z(" Give Z (%3d/%-2s): ", &Z);

    if( Rho(Z) <= 0 )
        return;
    printf(" Density is:  %5.2f g/cm^3", Rho(Z));
}

void Gram()
{
    float tmicro = 19.;
    int Z = 13;

    ask_par_float(" Give thickness (%3.1f um): ", &tmicro);
    ask_par_Z    (" Give Z (%3d/%-2s): ", &Z);

    if(Rho(Z) <= 0)
        return;
    printf("\n Thickness is %5.2f mg/cm^2\n",tmicro*Rho(Z)/10.);
}

void Meter()
{
    float tmg = 2.2;
    int Z = 13;
   
    ask_par_float(" Give thickness (%3.1f mg/cm^2): ", &tmg);
    ask_par_Z    (" Give Z (%3d/%-2s): ", &Z);

    if(Rho(Z) <=0 )
        return;
    printf("\n Thickness is %5.2f um\n",tmg*10./Rho(Z));
}


void Ispin()
{
    int Ai=3, Zi=2, Af=4, Zf=2, At=163, Zt=66;
    float Ti=45., Tf = 20., Theta=45.;
   
    ask_par_int(" Give A for the projectile (%3d): ", &Ai);
    ask_par_Z  (" Give Z for the projectile (%3d/%-2s): ", &Zi);
    ask_par_int(" Give A for the light-product (%3d): ", &Af);
    ask_par_Z  (" Give Z for the light-product (%3d/%-2s): ", &Zf);
    ask_par_int(" Give A for the target-nucleus (%3d): ", &At);
    ask_par_Z  (" Give Z for the target-nucleus (%3d/%-2s): ", &Zt);

    const float Q = ame2003_get_Q_keV(Ai, Zi, At, Zt, Af, Zf)/1e3;
    printf(" AME2003 Q-value is %3.1f MeV.\n", Q);

    const int Ar=At+Ai-Af;

    ask_par_float(" Give the energy of the projectile (%3.1f MeV): ", &Ti);
    ask_par_float(" Give the energy of the ejectile   (%3.1f MeV): ", &Tf);

    const double Ex=Q+Ti-Tf;

    ask_par_float(" Give scattering angle in degrees  (%3.1f): ", &Theta);
   
    /* Transfering values to MeV and fm. Atomic masses are used (incorrect!) */
    const double Thetar = Theta*M_PI/180;
    const double Mi = ame2003_get_mass_amu(Ai, Zi)*931.502;
    const double Mf = ame2003_get_mass_amu(Af, Zf)*931.502;
   
    /* Finding the corresponding wavenumbers k in units of 1/fm */
    const double k1=(1./197.329)*sqrt(2.*Mi*Ti);  
    const double k2=(1./197.329)*sqrt(2.*Mf*Tf);
 
    /* Finding average radius of the nucleus */
    const double R1=1.25*(pow(At,1/3.)+pow(Ai,1/3.));
    const double R2=1.25*(pow(Ar,1/3.)+pow(Af,1/3.));

    /* Calculating the maximum spin transfer */
    const double DSpin=0.5*(R1+R2)*sqrt(k1*k1+k2*k2 -2*k1*k2*cos(Thetar));
    const double CSpin=R1*k1 + R2*k2;
       
    printf("\n Maximum spintransfer for direct reaction at Ex= %6.3f is %6.3f\n",Ex,DSpin);
    printf(   "Maximum spintransfer for comp.  reaction at Ex= %6.3f is %6.3f\n",Ex,CSpin);   
}

/**************************************************************************/
/**************************************************************************/
/**            next part written by A. Bürger, Nov. 2008                 **/
/**************************************************************************/
/**************************************************************************/

#include <algorithm>

using namespace std;

/** these function are almost verbatim from Ziegler et al, The
    Stopping and Range of Ions in Solids, Pergamon, 1985 **/

static void pstop(int /*z1*/, double /*m1*/, int z2, double /*m2*/, double e,
                  const double* pcoef, double& se)
{
    const double pe0 = 25;
    const double pe = max(pe0, e);
    const double sl = pcoef[1]*pow(pe,pcoef[2]) + pcoef[3]*pow(pe, pcoef[4]);
    const double sh = pcoef[5]/pow(pe, pcoef[6])*log((pcoef[7]/pe)+pcoef[8]*pe);
    se = sl*sh/(sl+sh);
    if( e <= pe0 ) {
        double velpwr = 0.45;
        if( z2 <= 6 )
            velpwr = 0.25;
        se *= pow(e/pe0, velpwr);
    }
}

static void hestop(int z1, double m1, int z2, double m2, double e, const double* pcoef, double& se)
{
    const double E0 = 1;
    const double E  = max(E0, e);
    const double lE = log(E);
    const double c[6] = { 0.2865, 0.1266, -0.001429, 0.02402, -0.01135, 0.001475 };
    double g2He = 0, plE=1;
    for( int i=0; i<6; ++i, plE *= lE )
        g2He += c[i]*plE;
    g2He = 1-exp(-min(30.0, g2He));

    const double tmp1 = 7.6 - max(0.0, g2He);
    const double tmp2 = 1+(0.007+0.00005*z2)*exp(-tmp1*tmp1);
    g2He *= tmp2*tmp2;

    double sp;
    pstop( z1, m1, z2, m2, E, pcoef, sp );

    se = sp*g2He*z1*z1;
    if( e <= E0 )
        se *= sqrt(e/E0);
}

static void histop(int z1, double m1, int z2, double m2, double e, double /*ee*/,
                   double vfermi, double lfctr, const double* pcoef, double& se)
{
    const double yrmin = 0.13;
    double vrmin = 1.0;
    const double v = sqrt(e/25)/vfermi;
    double vr;
    if( v < 1 ) {
        vr = (3*vfermi/4.0)*(1+(2*v*v/3.0)-pow(v, 4)/15.0);
    } else {
        vr = v*vfermi*(1+1/(5.0*v*v));
    }
    const double z13 = pow(z1, 0.3333), z23 = pow(z1, 0.6667);
    const double yr  = max(max(yrmin, vr/z23), vrmin/z23);
    const double a   = -0.803*pow(yr, 0.3) + 1.3167*pow(yr, 0.6) + 0.38157*yr + 0.008983*yr*yr;
    const double q   = min(1.0,  max(0.0,  1-exp(-min(a, 50.0))));
    const double b   = min(0.43, max(0.32, 0.12+0.025*z1))/z13;
    const double l0  = (0.8-q*min(1.2, 0.6+z1/30.0))/z13;
    double l1;
    if( q < 0.2 ) {
        l1 = 0;
    } else if( q < max(0.0, 0.9-0.025*z1) ) {
        l1 = b*(q-0.2)/fabs(max(0.0, 0.9-0.025*z1)-0.2000001);
    } else if( q < max(0.0, 1-0.025*min(16, z1)) ) {
        l1 = b;
    } else {
        l1 = b*(1-q)/(0.025*min(16, z1));
    }
    const double l    = max(l1, l0*lfctr);
    const double aa   = 7.6-max(0.0, log(e));
    const double zeta = (q + (1/(2*vfermi*vfermi))*(1-q)*log(1+pow(4*l*vfermi/1.919, 2)))
        * (1+1.0/(z1*z1)*(0.18+0.0015*z2)*exp(-aa*aa));
    if( yr <= max(yrmin, vrmin/z23) ) {
        vrmin = max(vrmin, yrmin*z23);
        const double vmin = 0.5*(vrmin+sqrt(max(0.0, vrmin*vrmin-0.8*vfermi*vfermi)));
        const double eee = 25*vmin*vmin;
        double sp = 0;
        pstop( z1, m1, z2, m2, eee, pcoef, sp );
        double power = 0.5;
        if( (z2==6) || ((z2==14 || z2==32) && (z1 <= 19)) )
            power = 0.375;
        se = sp*pow(zeta*z1, 2)*pow(e/eee, power);
    } else {
        double sp = 0;
        pstop(z1, m1, z2, m2, e, pcoef, sp);
        se = sp*pow(zeta*z1, 2);
    }
}

static void scoef(int z1, double& mm1, double& m1, double& m2, double& rho, double& atrho,
                  double& vfermi, double& lfctr, const double*& pcoef)
{
#include "ziegler1985_tables.cppi"

    const double* pp = p1[z1-1];
    mm1    = pp[1]; // amu
    m1     = pp[2]; // amu
    m2     = pp[3]; // amu
    rho    = pp[4];
    atrho  = pp[5]*1e22;
    vfermi = pp[6];
    lfctr  = pp[7];

    pcoef  = p2[z1-1];
}

static bool stop(int z1, double m1/*amu*/, int z2, double m2/*amu*/,
                 double ee/*keV*/, double& se/*eV/ang*/, double& sn/*eV/ang*/)
{
    se = sn = 0;

    // check if z is in known range
    if( z1<1 || z1>92 || z2<1 || z2>92 || m1<=0 || m2<=0 )
        return false;
    if( ee < 1e-10 )
        return true;

    // dummy variables
    double y;
    const double *x;
    // retrieve lfctr for projectile
    double lfctr;
    scoef( z1, y, y, y, y, y, y, lfctr, x);

    // convert to MeV/amu and check that this is below 100 MeV/amu
    const double e = ee/m1;
    if( e > 1.1e5 )
        return false;

    // retrieve values for target
    const double *pcoef=0;
    double rho, atrho, vfermi, mm2;
    scoef(z2, y, y, mm2, rho, atrho, vfermi, y, pcoef);
    m2 = mm2; // XXX

    if( z1 == 1 ) {
        pstop(z1, m1, z2, m2, e, pcoef, se);
    } else if( z1 == 2 ) {
        hestop(z1, m1, z2, m2, e, pcoef, se);
    } else {
        histop(z1, m1, z2, m2, e, ee, vfermi, lfctr, pcoef, se);
    }
    const double rm = (m1+m2)*(pow(z1,0.23)+pow(z2,0.23));
    const double epsil = 32.53*m2*ee/(z1*z2*rm);
    if( epsil < 30 ) {
        const double a = 0.01321*pow(epsil, 0.21226) + 0.19593*sqrt(epsil);
        sn = 0.5*log(1+1.1383*epsil)/(epsil+a);
    } else {
        sn = 0.5*log(epsil)/epsil;
    }
    sn *= z1*z2*m1*8.462/rm;

    // convert so ev/angstrom
    se *= atrho*1e-23;
    sn *= atrho*1e-23;
    return true;
}

/**************************************************************************/
/**************************************************************************/

/** some convenience functions **/

static double /*keV/ang*/ loss(int z1, double m1/*amu*/, int z2,
                               double m2/*amu*/, double e/*keV*/)
{
    double se, sn;
    stop(z1, m1, z2, m2, e, se, sn);
    return -1e-3*(se + sn); /* convert to de/dx in keV/angstrom */
}

static double ziegler1985(int z1, double m1/*amu*/, int z2, double m2/*amu*/,
                          double e/*keV*/, float d/*um*/, int nstep)
{
    const double sl = d*1e4 / nstep; // step length in angstrom
    for(int j=0; e>0 && j<nstep; ++j) {
        const double de1 = sl*loss( z1, m1, z2, m2, e       );
        const double de2 = sl*loss( z1, m1, z2, m2, e+de1/2 );
        const double de3 = sl*loss( z1, m1, z2, m2, e+de2/2 );
        const double de4 = sl*loss( z1, m1, z2, m2, e+de3   );
        const double de  = (de1 + 2*(de2+de3) + de4)/6;
        //printf("%4d %8.3lg %8.3lg\n", j, de, e);
        e += de;
        if( e<0 )
            e = 0;
    }
    return e;
}

/**************************************************************************/
/**************************************************************************/

static void prog_mass()
{
    // get mass of most abundant isotope from ziegler's table
    int z1 = 14;
    ask_par_Z(" Give Z (%3d/%-2s)", &z1);

    double m1, y;
    const double* x;
    scoef( z1, y, m1, y, y, y, y, y, x);
    printf(" The mass of the most abundant isotope is %8.5f amu\n", m1);
}

static void prog_dedx()
{
    int a1=4, z1=2, a2=28, z2=14, e_per_amu=1;
    double e=20000;
    
    ask_par_int(" Give A of projectile (%3d): ", &a1);
    ask_par_Z  (" Give Z of projectile (%3d/%-2s): ", &z1);
    const double m1 = ame2003_get_mass_amu(a1, z1);
    printf(" Projectile mass is %.5g amu\n", m1 );

    ask_par_int(" Give A of absorber   (%3d): ", &a2);
    ask_par_Z  (" Give Z of absorber   (%3d/%-2s): ", &z2);
    const double m2 = ame2003_get_mass_amu(a2, z2);
    printf(" Absorber mass is %.5g amu\n", m2 );

    ask_par_double(" Give Ekin of projectile (%5.3f keV): ", &e);
    ask_par_int   (" Is Ekin given per amu?  (%d): ", &e_per_amu);

    if( e_per_amu ) {
        e *= m1;
        printf("Ekin is %.3g keV\n", e );
    }

    double sn=0, se=0;
    stop( z1, m1, z2, m2, e, se, sn);
    printf("Stopping power is el= %.3lg nuc= %.3lg eV/angstrom\n", se, sn );
}

static void prog_loss()
{
    // integrated energy loss
    int a1=4, z1=2, a2=28, z2=14, nstep=1000;
    double ee=20, d=150;

    ask_par_int(" Give A of projectile (%3d): ", &a1);
    ask_par_Z  (" Give Z of projectile (%3d/%-2s): ", &z1);
    const double m1 = ame2003_get_mass_amu(a1, z1);
    printf("Mass is %.5g amu\n", m1 );

    ask_par_int(" Give A of absorber   (%3d): ", &a2);
    ask_par_Z  (" Give Z of absorber   (%3d/%-2s): ", &z2);
    const double m2 = ame2003_get_mass_amu(a2, z2);
    printf("Absorber mass is %.5g amu\n", m2 );

    ask_par_double(" Give Ekin of projectile    (%5.3f MeV): ", &ee);
    ask_par_double(" Give thickness of absorber (%5.3f um): ", &d);
    ask_par_int   (" Give n. of steps           (%3d): ", &nstep);

    const double e = ziegler1985(z1, m1, z2, m2, ee*1000, d, nstep)/1000;

    printf("\nThe final energy is %7.3lf MeV with energy loss of %lf MeV\n",
           e, ee-e );
}

static void prog_zstraggling()
{
    int A=46, Z=22, Ai=1, Zi=1, Zf=2, Af=4;
    long nstep = 1000;
    double Ti=32., d=3., Ex=0., theta=45.;
   
    ask_par_int(" Give A for the absorbating medium (%3d): ", &A);
    ask_par_Z  (" Give Z for the absorbating medium (%3d/%-2s): ", &Z);
    ask_par_int(" Give A for the projectile         (%3d): ", &Ai);
    ask_par_Z  (" Give Z for the projectile         (%3d/%-2s): ", &Zi);
    ask_par_int(" Give A for the light-product      (%3d): ", &Af);
    ask_par_Z  (" Give Z for the light-product      (%3d/%-2s): ", &Zf);

    const double Q = ame2003_get_Q_keV(Ai, Zi, A, Z, Af, Zf)/1e3;
    printf(" AME2003 Q-value is %6.4f MeV.\n", Q);

    ask_par_double(" Give energy of the projectile         (%3.1f MeV): ", &Ti);
    ask_par_double(" Give the thickness of target          (%3.1f mg/cm^2): ", &d);
    ask_par_double(" Give exitation-energy for the nucleus (%3.1f MeV): ", &Ex);

    const int Zr = Z + Zi - Zf;
    const int Ar = A + Ai - Af;
    const double M  = ame2003_get_mass_amu(A,  Z );
    const double Mi = ame2003_get_mass_amu(Ai, Zi);
    const double Mf = ame2003_get_mass_amu(Af, Zf);
    const double Mr = ame2003_get_mass_amu(Ar, Zr);

    if( Rho(Z)<=0 || Mi<=0 || Mf<=0 )
        return;

    ask_par_double(" Give angle in degree (-1 = stop) (%3.1f): ", &theta);
    while(theta!=-1) {
        const double thetar   = theta*M_PI/180, cost=cos(thetar);
        const double thick_um = d/Rho(Z)*10;//*1.e-5;
        printf("thick_um=%f Z=%d d=%lf rho=%lf\n", thick_um, Z, d, Rho(Z));

        // calculate for reaction at the end of the target
        double Ek1 = ziegler1985(Zi, Mi, Z, M, Ti*1000, thick_um, nstep)/1000;
        printf("Ek1 after target=%.5g\n", Ek1);
        Ek1 = Relkin(Mi,Mf,Mr,Ek1,Q,thetar) - Ex;
        printf("Ek1 after relkin=%.5g\n", Ek1);
        
        // calculate for reaction at the beginning of the target
        double Ek2 = Relkin(Mi,Mf,Mr,Ti,Q,thetar) - Ex;
        printf("Ek2 after relkin=%.5g\n", Ek2);
        Ek2 = ziegler1985(Zf, Mf, Z, M, Ek2*1000, thick_um/cost, nstep)/1000;
        printf("Ek2 after target=%.5g\n", Ek2);

        // calculate mean and straggling
        const double Ekf = sqrt(.5*(Ek1*Ek1+Ek2*Ek2));
        const double E1s = 18.*Zi*sqrt(d);
        const double E2s = 18.*Zf*sqrt(d/cost);
        const double dE  = sqrt(((Ek1-Ek2)*1000.)*((Ek1-Ek2)*1000.) + 0.5*(E1s*E1s+E2s*E2s));

        printf("de: dE_1=%f dE_2=%f\n", Ek1-Ek2, 0.5*(E1s*E1s+E2s*E2s));
        printf("The final energy is %6.3f MeV with FWHM %5.1f keV\n",Ekf,dE);

        ask_par_double("\n Give angle in degree (-1 = stop) (%3.1f): ", &theta);
    }
}

void Ziegler()
{
    bool go_on = true;
    while(go_on) {
        const char* input = ask_par_input("\n\n Select Ziegler1985 sub-program: \n"
                                          "    dEdX Loss Mass Straggling Exit: ");
        switch(tolower(input[0])) {
        case 'd': prog_dedx(); break;
        case 'l': prog_loss(); break;
        case 'm': prog_mass(); break;
        case 's': prog_zstraggling(); break;
        default: go_on = false; break;
        }
    }
}

/* for emacs */
/*** Local Variables: ***/
/*** indent-tabs-mode: nil ***/
/*** c-basic-offset: 4 ***/
/*** End: ***/
