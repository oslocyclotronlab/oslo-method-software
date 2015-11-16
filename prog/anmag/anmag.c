#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include <fcntl.h>                                      // Header for open/read/write
#include <errno.h>
#include <sys/types.h>
#include <sys/ioctl.h> 
#define MAXAMP          133.                            // Max amps for the analyzing magnet
#define MAXTESLA       0.923                            // Max Tesla for the analyzing magnet
float c2 = 931.50; //units MeV/u
float m_e   = 0.511;
float m_p   = 1.007825;                                 // All masses in a.m.u
float m_d   = 2.014102;
float m_3he = 3.016029;
float m_4he = 4.002603;
float c     = 2.997925*1.e+8;                           // Speed of light in m/s
float e     = 1.60210*1.e-19;                           // elementary charge in Coulomb
int type    = 1;
float r     = 1.00;
float a0    = -6.0, a1 = 0.999*MAXAMP/MAXTESLA;           // I = a0 + a1*B with units A and A/T
float MeV   = 1.602e-13;                                // 1MeV   = 1.602e-13 Joule
float T     = 20.;
float mc2, q, p, B, I;
float p1,B1,I1,p2,B2,I2,p3,B3,I3,p4,B4,I4;
float   p_func(float, float );
float   B_func(float, float, float );

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
    int     i;
    m_p   = m_p*c2   - 1.*m_e;                               // nuclear masses in MeV
    m_d   = m_d*c2   - 1.*m_e;
    m_3he = m_3he*c2 - 2.*m_e;
    m_4he = m_4he*c2 - 2.*m_e;

    printf("\n");
    printf("  _________________________________________ \r\n");
    printf(" |                                         |\r\n");
    printf(" |              A N M A G  1.0             |\r\n");
    printf(" |                                         |\r\n");
    printf(" |         Program to calculate the        |\r\n");
    printf(" |       I(A) and B(T) for a certain       |\r\n");
    printf(" |       particle type (p,d,3He,4He)       |\r\n");
    printf(" |  with a certain kinetic energy T(MeV)   |\r\n");
    printf(" |                                         |\r\n");
    printf(" |   Relativistic expressions are used:    |\r\n");
    printf(" |            T = E - mc^2 and             |\r\n");
    printf(" |         E^2= (pc)^2 + (mc^2)^2          |\r\n");
    printf(" |                                         |\r\n");
    printf(" | E-mail  : magne.guttormsen@fys.uio.no   |\r\n");
    printf(" | Created : 25 Feb 2014                   |\r\n");
    printf(" | Modified: 08 Mar 2014                   |\r\n");
    printf(" |_________________________________________|\r\n");
    printf("                                           \r\n");

    fp = fopen("anmag_in.txt", "r");
    if(fp == NULL){
        printf("Could not open file anmag_in.txt, default values are used \n");
    }else{
        fgets_ignore(line,sizeof(line),fp);
        sscanf(line, " %d %f \n",  &type,&T);
    }
    fclose(fp);

    printf("\nParticle type (p,d,3He,4He) = (1,2,3,4):  <%1d>",type);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%d", &type);
    if(type==1){
        q   = 1.*e;
        mc2 = m_p;
    }
    if(type==2){
        q   = 1.*e;
        mc2 = m_d;
    }
    if(type==3){
        q   = 2.*e;
        mc2 = m_3he;
    }
    if(type==4){
        q   = 2.*e;
        mc2 = m_4he;
    }
        
    printf("\nParticle kinetic energy:             <%7.3f>", T);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &T);
    
    printf("\nMass %7.3e,  charge %7.3e, type particle %d, and beam energy %6.3f",mc2,q,type,T);

    p = p_func( T,  mc2);
    B = B_func( p,  q,  r);
    I = a0 + a1 * B;
	
    printf("\nMomentum p %7.3e,  field B %7.3e, Coil currents (A) %f\n",p,B,I);
    
    
    fp = fopen("anmag_in.txt", "w");
    if(fp == NULL){
        printf("Could not open file anmag_in.txt \n");
    }else{
       fprintf(fp,"%d %f \n", type,T);
    }
    fclose(fp);
    
    fp = fopen("anmag_out.txt", "w");
    for(i=1;i<=500;i++){
        T=((float)i)/10.;
        p1=p_func(T,m_p);
        B1=B_func(p1,1.*e,r);
        I1 = a0 + a1 * B1;
        p2=p_func(T,m_d);
        B2=B_func(p2,1.*e,r);
        I2 = a0 + a1 * B2;
        p3=p_func(T,m_3he);
        B3=B_func(p3,2.*e,r);
        I3 = a0 + a1 * B3;
        p4=p_func(T,m_4he);
        B4=B_func(p4,2.*e,r);
        I4 = a0 + a1 * B4;
        fprintf(fp," %7.1f   %7.3f %7.2f    %7.3f %7.2f    %7.3f %7.2f    %7.3f %7.2f \n",T,B1,I1,B2,I2,B3,I3,B4,I4);
    }
    fclose(fp);
    return 0;
}

float p_func(float T, float mc2)
{
    float p;
    p = (1./c)*sqrt((T+mc2)*(T+mc2)-mc2*mc2) * MeV;
    return p;
}

float B_func(float p, float q, float r)
{
    float B;
    B = p/(q*r);
    return B;
}