#include <algorithm>
#include <iostream>
#include <cmath>
#include <cstdarg>
#include <cstdlib>
#include <cstdio>

//#define DBGV(x) cout << #x "='" << x << "'\n"
#define DBGV(x) 

std::string fmt(const char *fmt, ...) __attribute__ ((format (printf, 1, 2)));
std::string fmt(const char *fmt, ...)
{
    char msgbuf[8192];
    va_list ap;
    va_start (ap, fmt);
    (void) std::vsnprintf(msgbuf, sizeof(msgbuf), fmt, ap);
    va_end (ap);

    return msgbuf;
}

using namespace std;

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

static double rpstop(int z2, double e, const double *pcoef)
{
    const double pe0 = 25;
    const double pe = max(pe0, e);
    const double sl = pcoef[1]*pow(pe, pcoef[2]) + pcoef[3]*pow(pe, pcoef[4]);
    const double sh = pcoef[5]/pow(pe, pcoef[6])*log(pcoef[7]/pe+pcoef[8]*pe);
    double sp = sl*sh/(sl+sh);
    if( e <= pe0 ) {
        const double velpwr = ( z2<=6 ) ? 0.25 : 0.45;
        sp *= pow(e/pe0, velpwr);
    }
    return sp;
}


static double sezieg(int z1, int z2, double m1, double /*m2*/, double e, double /*rho*/)
{
    double ee = e/1000/m1, atrho, vfermi, y;
    const double *py, *pcoef;
    scoef(z2, y, y, y, y, atrho, vfermi, y, pcoef);
    double evang = 0.1*atrho/1e22, lfctr, se;
    scoef(z1, y, y, y, y, y, y, lfctr, py);

    e = ee;
    if( z1 == 1 ) {
        // proton case
        se = rpstop(z2, e, pcoef);
    } else if( z1 == 2 ) {
        // helium case
        double he0 = 1;
        double he = max(he0, e);
        double b = log(he), b2=b*b, b4=b2*b2;
        double a = 0.2865 + 0.1266*b - 0.001429*b2 + 0.02402*b*b2 - 0.01135*b4+0.001475*b*b4;
        double heh = 1-exp(-min(30.,a));
        double sp = rpstop(z2, he, pcoef);
        se = sp*heh*4;
        if( e <= he0 )
            se *= sqrt(e/he0);
    } else {
        // heavy ion case
        double yrmin = 0.13;
        double vrmin = 1.0;
        double v = sqrt(e/25)/vfermi, vr;
        if( v >= 1 ) {
            vr = v*vfermi*(1+1/(5*v*v));
        } else {
            vr = (3*vfermi/4)*(1+(2*v*v/3)-v*v*v*v/15);
        }
        const double z1p6 = pow(z1, 0.6667), z1p3 = pow(z1, 0.3333);
        const double yr = max(max(yrmin, vr/z1p6), vrmin/z1p6);
        double a = -0.803*pow(yr, 0.3)+1.3167*pow(yr, 0.6)+0.38157*yr+0.008983*yr*yr;
        double q = min(1., max(0., 1-exp(-min(a,50.))));
        double b = min(0.43, max(0.32, 0.12+0.025*z1))/z1p3;
        double l0 = (0.8-q*min(1.2, 0.6+z1/30))/z1p3, l1;
        if( q < 0.2 ) {
            l1 = 0;
        } else if( q < max(0., 0.9-0.025*z1) ) {
            //q1 = 0.2;
            l1 = b*(q-0.2)/fabs(max(0., 0.9-0.025*z1)-0.2000001);
        } else if( q < max(0., 1-0.025*min(16, z1)) ) {
            l1 = b;
        } else {
            l1 = b*(1-q)/(0.025*min(16, z1));
        }
        double l = max(l1, l0*lfctr);
        double zeta = q+(1/(2*vfermi*vfermi))*(1-q)*log(1+pow(4*l*vfermi/1.919, 2));
        a = -pow(7.6-max(0., log(e)), 2);
        zeta *= 1+(1/(z1*z1))*(0.18+0.0015*z2)*exp(a);
        double sp;
        if( yr<=max(yrmin, vrmin/z1p6) ) {
            sp = rpstop(z2, e, pcoef);
            se = sp*pow(zeta*z1, 2);
        } else {
            vrmin = max(vrmin, yrmin*z1p6);
            double vmin = 0.5*(vrmin+sqrt(max(0., vrmin*vrmin-0.8*vfermi*vfermi)));
            double eee = 25*vmin*vmin;
            sp = rpstop(z2, eee, pcoef);
            double power = 0.5;
            if(z2 == 6 || ((z2 == 14 || z2 == 32) && z2 <= 19 ))
                power = 0.35;
            se = sp*pow(zeta*z1, 2)*pow(e/eee, 0.5);
            if( z2 == 6 )
                se *= pow(e/eee, 0.75*0.5)/pow(e/eee, 0.5);
        }
    }
    se *= evang;
    return se;
}

static void pral(double energy, int z1, int z2, double m1, double m2, double rho, double& xm, double& sigmax)
{
    const int irast = 100;
    //const double emax = energy / 1000;
    const double arho = rho * 0.6023/m2;
    const double mu = m2/m1;
    const double gamma = 4*mu/pow(1+mu, 2);
    const double a = 0.4685/(pow(z1, 0.23)+pow(z2,0.23));
    const double epsfak = a*mu/((1+mu)*z1*z2*14.4);
    const double snfak = 3.14159*a*a*gamma*arho/epsfak;
    //double xfak = 1/(arho*3.14159*a*a*gamma);

    //cout << "emax=" << emax << " z1=" << z1 << " m1=" << m1 << " z2=" << z2 << " m2=" << m2
    //     << " rho=" << rho << endl;

    double e = 10;
    double eps = e*epsfak;

    double sn = eps + 0.01321*pow(eps, 0.21226) + 0.19594*sqrt(eps);
    sn = 0.5*log(1+1.1383*eps)/sn;
    sn *= snfak;

    double qn = 1/(4 + 0.197/pow(eps, 1.6991) + 6.684/pow(eps, 1.0494));
    qn *= gamma*snfak/epsfak;

    double se = sezieg(z1, z2, m1, m2, e, rho);
    double st = se+sn;

    const double nu = mu/(1.2*(1+se/sn));
    const double beta = 1/(1-qn/(gamma*e*sn));
    xm = 2*e/(mu*sn*(1+1/nu));
    double xe = e/(0.4*se);
    double xt = e/(0.4*st);
    double xi = (1+nu)*xm*xm;
    double sigz2 = xi/(2+beta/nu), sigx2, sigmaz;

    DBGV(nu);
    DBGV(mu);
    DBGV(beta);
    DBGV(gamma);

//    cout <<
//        "   energy   epsilon      x-el   x-total    x-proj    sigmax    sigmaz        se        sn        qn\n"
//        "       eV                   Å         Å         Å         Å         Å      eV/Å      eV/Å     eV²/Å\n";
#ifdef EV_ANGSTROM
    cout <<
        "#       energy         x-proj    sigmax \n"
        "#           eV              Å         Å \n";
#else
    cout <<
        "#       energy         x-proj    sigmax \n"
        "#          MeV             um        um \n";
#endif

    double econ = pow(10, 0.01/irast);
    do {
        sigx2 = xi - sigz2-xm*xm;
        sigmax = sqrt(sigx2);
        sigmaz = sqrt(sigz2);

//        const char* f = "%9.2g ";
//        cout << fmt(f, e) << fmt(f, eps) << fmt(f, xe) << fmt(f, xt)
//             << fmt(f, xm) << fmt(f, sigmax) << fmt(f, sigmaz)
//             << fmt(f, se) << fmt(f, sn) << fmt(f, qn) << endl;
#ifdef EV_ANGSTROM
        cout << fmt("%14.5g ", e) << fmt("%14.5g ", xm) << fmt("%9.2g ", sigmax) << endl;
#else
        cout << fmt("%14.8g ", e/1e6) << fmt("%14.8g ", xm/1e4) << fmt("%9.5g ", sigmax/1e4) << endl;
#endif
        for(int i3=0; i3<irast; ++i3) {
            double e0 = e;
            e *= econ;
            double de = e - e0;
            eps = e*epsfak;

            // wie oben
            sn = eps + 0.01321*pow(eps, 0.21226) + 0.19594*sqrt(eps);
            sn = 0.5*log(1+1.1383*eps)/sn;
            sn *= snfak;

            qn = 1/(4 + 0.197/pow(eps, 1.6991) + 6.684/pow(eps, 1.0494));
            qn *= gamma*snfak/epsfak;
        
            se = sezieg(z1, z2, m1, m2, e, rho);
            st = sn+se;

            double az1 = 2*mu*e*sn;
            double az2 = 2*mu*qn;
            double az8 = 4*az2;
            double az3 = (1-mu)*(1-mu)*qn;
            double an1 = 4*e*st;
            double deltum = de/e;
            double qq = 1-az8/an1;
            xe += de/se;
            xt += de/st;
            xm += (4*e*e-(az1+mu*qn)*xm)*deltum/(an1-az2);
            xi += 2*xm*de/st;
            double hilf1 = an1-az8;
            double hilf2 = 2*az1-az3;
            double hilf3 = e*az8*xm/st;
            if(qq<0.001) {
                sigz2 = 0.5*xi-xm*e*e/(az1-az3/2);
            } else {
                sigz2 += (hilf2*(xi-2*sigz2)-hilf3)*deltum/hilf1;
                sigx2 = xi - sigz2 - xm*xm;
            }
        }
    } while( e<energy );
        
    sigmax = sqrt(sigx2);
    sigmaz = sqrt(sigz2);

    //double sninv = 1/snfak;
    
    //cout << "irast=" << irast << " 
}

static void prog_pral(int z1, double m1, int z2)
{
    double energy = 45e6, m2, rho, xm, sigmax, y;
    const double *yp;
    scoef(z2, y, y, m2, rho, y, y, y, yp);
    pral(energy, z1, z2, m1, m2, rho, xm, sigmax);
}

int main(int argc, char* argv[])
{
    if( argc != 4 ) {
        cerr << argv[0] << " z1 m1 z2" << endl
             << "  z1,m1 => projectile" << endl
             << "  z2    => target (will lookup mass)" << endl;
        exit(-1);
    }

    int z1 = atoi(argv[1]), z2 = atoi(argv[3]);
    double m1 = atof(argv[2]);
    if( m1 == 0 ) {
        double y;
        const double* py;
        scoef(z1, y, m1, y, y, y, y, y, py);
    }

    prog_pral(z1, m1, z2);
    return 0;
}

/*** Local Variables: ***/
/*** compile-command:"g++ -O2 -g -Wall -W -o /tmp/range.exe range.cpp" ***/
/*** End: ***/
