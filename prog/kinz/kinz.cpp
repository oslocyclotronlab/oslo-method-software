#include <ctype.h>
#include <math.h>
#include <stdio.h>

#include "ask_par.h"
#include "ame2003_masses.h"
#include "kinzlib.h"

float Rk4(float Ek, float c, float Mr, float I, float h, long nstep, char list);
float Bethe(float Ek, float c, float Mr, float I);

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
    printf(" AME2003 Q-value is %8.5g MeV.\n", Q);

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
    double Ti=32., d=3., Ex=0., theta=45.;
   
    ask_par_int(" Give A for the absorbating medium (%3d): ", &A);
    ask_par_Z  (" Give Z for the absorbating medium (%3d/%-2s): ", &Z);
    ask_par_int(" Give A for the projectile         (%3d): ", &Ai);
    ask_par_Z  (" Give Z for the projectile         (%3d/%-2s): ", &Zi);
    ask_par_int(" Give A for the light-product      (%3d): ", &Af);
    ask_par_Z  (" Give Z for the light-product      (%3d/%-2s): ", &Zf);

    const double Q = ame2003_get_Q_keV(Ai, Zi, A, Z, Af, Zf)/1e3;
    printf(" AME2003 Q-value is %8.5g MeV.\n", Q);

    ask_par_double(" Give energy of the projectile         (%3.1f MeV): ", &Ti);
    ask_par_double(" Give the thickness of target          (%3.1f mg/cm^2): ", &d);
    ask_par_double(" Give exitation-energy for the nucleus (%3.1f MeV): ", &Ex);

    ask_par_double(" Give angle in degree (-1 = stop) (%3.1f): ", &theta);
    while(theta!=-1) {
        double Ekf=0, dE=-1;
        zstraggling(A, Z, Ai, Zi, Af, Zf, Ti, d, Ex, theta, Ekf, dE);
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
