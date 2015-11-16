
#include "kinzlib.h"
#include "ame2003_masses.h"

#include <cmath>
#include <algorithm>
#include <cstdio>
#include <iostream>

using namespace std;

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


float P2rel(float M, float T)
{
    float x;
    x = 2.*931.5016*M*T + T*T;
    return(x);
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

void scoef(int z1, double& mm1, double& m1, double& m2, double& rho, double& atrho,
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

bool stop(int z1, double m1/*amu*/, int z2, double m2/*amu*/,
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

double /*keV/ang*/ loss(int z1, double m1/*amu*/, int z2,
                               double m2/*amu*/, double e/*keV*/)
{
    double se, sn;
    stop(z1, m1, z2, m2, e, se, sn);
    return -1e-3*(se + sn); /* convert to de/dx in keV/angstrom */
}

double ziegler1985(int z1, double m1/*amu*/, int z2, double m2/*amu*/,
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

bool zstraggling(int A, int Z, int Ai, int Zi, int Af, int Zf,
                 double Ti /* MeV */, double d, double Ex /* MeV */,
                 double theta /* deg */, double &Ekf_r /* MeV */, double& dE_r /* keV */)
{
    long nstep = 1000;

    const double Q = ame2003_get_Q_keV(Ai, Zi, A, Z, Af, Zf)/1e3; /* MeV */

    const int Zr = Z + Zi - Zf;
    const int Ar = A + Ai - Af;
    const double M  = ame2003_get_mass_amu(A,  Z ); /* amu */
    const double Mi = ame2003_get_mass_amu(Ai, Zi); /* amu */
    const double Mf = ame2003_get_mass_amu(Af, Zf); /* amu */
    const double Mr = ame2003_get_mass_amu(Ar, Zr); /* amu */

    if( Rho(Z)<=0 || Mi<=0 || Mf<=0 )
        return false;

    const double thetar   = theta*M_PI/180, cost=fabs(cos(thetar));
    const double thick_um = d/Rho(Z)*10;

    // calculate for reaction at the end of the target
    double Ek1 = ziegler1985(Zi, Mi, Z, M, Ti*1000, thick_um, nstep)/1000; /* MeV */
    Ek1 = Relkin(Mi,Mf,Mr,Ek1,Q,thetar) - Ex;
    if( theta > 90 )
        Ek1 = ziegler1985(Zf, Mf, Z, M, Ek1*1000, thick_um/cost, nstep)/1000;
        
    // calculate for reaction at the beginning of the target
    double Ek2 = Relkin(Mi,Mf,Mr,Ti,Q,thetar) - Ex; /* MeV */
    if( theta <= 90 )
        Ek2 = ziegler1985(Zf, Mf, Z, M, Ek2*1000, thick_um/cost, nstep)/1000;

    // calculate mean and straggling
    const double Ekf = sqrt(.5*(Ek1*Ek1+Ek2*Ek2)); /* MeV */
    const double E1s = 18.*Zi*sqrt(d);
    const double E2s = 18.*Zf*sqrt(d/cost);
    const double dE  = sqrt(((Ek1-Ek2)*1000.)*((Ek1-Ek2)*1000.) + 0.5*(E1s*E1s+E2s*E2s)); /* keV */

    Ekf_r = Ekf;
    dE_r  = dE;
    return true;
}
