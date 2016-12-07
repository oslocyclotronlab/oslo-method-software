#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include <fcntl.h>                                         /* Header for open/read/write */ 
#include <errno.h>
#include <sys/types.h> 
#include <sys/ioctl.h> 

int      iN, iZ = 90, iA = 233, ans1=1, ans2=1;				   /* proton and mass number */
float     N, Z, A, I_rigid, w_D, w_Q, Ksi, w, I_IV, B_M1, S_u, S_d, g_IS, g_IV;
float delta = 0.24;											/* deformation */
float   r_0 = 1.15;											/* nucleon radius */
float   m_N = 939.;											/* nucleon mass */
float   g_p = 1.00;											/* proton gyro fac. */
float   g_n = 0.00;											/* neutron gyro fac. */
float    w0 = 2.2;                                          /* assumed w(M1) */
float gamm = 15.0, gam, E1, E2, E3, B1, B2, B3, w1, w2, w3;
float    pi = 3.14159;
float hbarc = 197.3;                                        /* MeV fm */
float   E2 = 0.049369;

FILE    *fp;

static void fgets_ignore(char *s, int size, FILE *stream)
{
    // suppress braindead and presumtuous glibc 'security' warning
    if( !fgets(s, size, stream) )
        return;
}

int main ()
{
	char    line[128];
    int     read_input;

    printf("\n");
    printf("  _____________________________________________________ \r\n");
    printf(" |                                                     |\r\n");
    printf(" |                   SCISSORS 1.4.1                    |\r\n");
    printf(" |                                                     |\r\n");
    printf(" |                 Program to calculate                |\r\n");
    printf(" |            scissors mode energy centroid            |\r\n");
    printf(" |              and summed B(M1) strength              |\r\n");
    printf(" |        J.Enders et al., PRC71, 014306(2005)         |\r\n");
    printf(" |  K. Heyde, et al., Rev. Mod. Phys., 82, 2365 (2010) |\r\n");
    printf(" |    E. Lipparini et al., Phys. Rep. 175, 103 (1989)  |\r\n");
    printf(" |                                                     |\r\n");
    printf(" | E-mail  : magne.guttormsen@fys.uio.no               |\r\n");
    printf(" | Created : 16 Feb 2012                               |\r\n");
    printf(" | Modified: 05 Nov 2013                               |\r\n");
    printf(" | Modified: 16 Nov 2016                               |\r\n");
    printf(" |_____________________________________________________|\r\n");
    printf("                                                        \r\n");

    read_input = 0;
    fp = fopen("input.sci", "r");
    if(fp == NULL){
        printf("Could not open file input.sci, default values are used \n");
    }else{
        fgets_ignore(line,sizeof(line),fp);
        sscanf(line, " %d %d %f %f %f %f %f %f %f %f %d %d %f\n", &iZ,&iA,&delta,&r_0,&m_N,&g_p,&g_n,&I_IV,&w0,&gamm, &ans1,&ans2,&E2);
        fclose(fp);
        read_input = 1;
    }

    printf("\nProton number Z:                     <%3d>",iZ);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%d", &iZ);
    printf(  "Mass number A:                       <%3d>", iA);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%d", &iA);
    printf("Deformation parameter Delta:      <%6.3f>", delta);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &delta);
    printf("Nucleon radius r_0 (fm):          <%6.3f>", r_0);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &r_0);
    printf("Nucleon mass m_N:                 <%6.1f>", m_N);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &m_N);
    iN = iA - iZ;
    A  = (float)iA;
    N  = (float)iN;
    Z  = (float)iZ;
    w_D  = (31.2*pow(A,-(1./3.)) + 20.6*pow(A,-(1./6.)))*(1.-0.61*delta);
    w_Q  =  64.7*pow(A,-(1./3.))*(1.-0.3*delta);
   
    
    printf(" \n");
    printf("Energi w_D (MeV)               = %9.6f\n",w_D);
    printf("Energi w_Q (MeV)               = %9.6f\n",w_Q);
    
    printf(" \n");

    printf("Energi w_D (MeV):              <%9.6f>", w_D);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &w_D);

    printf("Energi w_Q (MeV):              <%9.6f>", w_Q);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &w_Q);
    
    Ksi  = (w_Q*w_Q)/((w_Q*w_Q)+2.0*(w_D*w_D));
    printf("Factor Ksi:                    <%9.6f>", Ksi);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &Ksi);

    
////////////////////////////////////////////////////////////////
    
    printf("Use g_p-g_n=2Z/A (1) or type gp and gn (2): <%1d>", ans1);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%d", &ans1);
    if(ans1==1){
        g_p = 2.*Z/A;
        g_n = 0.;
    }else{
        printf("Proton gyromagnetic orb. fac.:    <%6.3f>", g_p);
        fgets_ignore(line,sizeof(line),stdin);
        sscanf(line,"%f", &g_p);
        printf("Neutron gyromagnetic orb. fac.:   <%6.3f>", g_n);
        fgets_ignore(line,sizeof(line),stdin);
        sscanf(line,"%f", &g_n);
    }
    
    printf(" \n");
    printf("g_p                            = %9.6f\n",g_p);
    printf("g_n                            = %9.6f\n",g_n);
    
//////////////////////////////////////////////
    I_rigid = (2./5.)*m_N*r_0*r_0*pow(A,(5./3.))/(hbarc*hbarc);        /* (fm*fm)*hbarc*hbarc */
    I_rigid = I_rigid*(1.+0.31*delta);
    printf("Use rigid (1) or gsb moment of inertia (2): <%1d>", ans2);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%d", &ans2);
    if(ans2==1){
        printf("The rigid moment of inertia (RMI):%8.3f\n", I_rigid);
        I_IV = I_rigid;
        printf("Moment of inertia I_IV):        <%8.3f>", I_IV);
        fgets_ignore(line,sizeof(line),stdin);
        sscanf(line,"%f", &I_IV);
        
    }else{
        printf("Excitation energy of first rotational 2+ state (MeV): <%8.6f>", E2);
        fgets_ignore(line,sizeof(line),stdin);
        sscanf(line,"%f", &E2);
        I_IV = 3./E2;
        printf("Moment of inertia I_IV):        <%8.3f>", I_IV);
        fgets_ignore(line,sizeof(line),stdin);
        sscanf(line,"%f", &I_IV);
    }
/////////////////////
    
//    I_rigid = 0.8*I_rigid;
//    I_IV    = 0.8*I_IV;
    
//////////////////////////////////////////////////////////////////////////////    

////////////////////////////////////////////////////////////////////////////////
    
    S_u  = (3./( 8.*pi))*I_rigid*delta*delta*w_D*w_D*(g_p-g_n)*(g_p-g_n);
    S_d  = (3./(16.*pi))*I_IV                       *(g_p-g_n)*(g_p-g_n);
    
    w    = sqrt(S_u/S_d);
    B_M1 = sqrt(S_u*S_d);
    
    printf(" \n");
    printf("__________________________________________\n");
    printf("Without separation of high-lying M1 strength\n");
    printf("Strength S_+                   = %9.5f\n",S_u);
    printf("Strength S_-                   = %9.5f\n",S_d);
    printf("Energy centroid (MeV)          = %9.5f\n",w);
    printf("B(M1) strength (u_N**2)        = %9.5f\n",B_M1);
    printf("__________________________________________\n");

    
////////////////////////////////////////////////////////////////////////////////    


    g_IS = 0.5*(g_p + g_n);
    g_IV = 0.5*(g_p - g_n);
    S_u  = (3./(2.*pi))*I_rigid*delta*delta*w_D*w_D*g_IS*g_IS*Ksi;
    w    = sqrt(S_u/S_d);
    B_M1 = sqrt(S_u*S_d);               /* Alternatively B_M1 = S_d*w */
    
    printf("__________________________________________\n");
    printf("With separation of high-lying M1 strength\n");
    printf("Strength S_+                   = %9.5f\n",S_u);
    printf("Strength S_-                   = %9.5f\n",S_d);
    printf("Energy centroid (MeV)          = %9.5f\n",w);
    printf("B(M1) strength (u_N**2)        = %9.5f\n",B_M1);
    printf("__________________________________________\n");


////////////////////////////////////////////////////////////////////////////////
//    printf(" \n \n");
//    printf("Systematics for centroid w(sys): %9.3f\n", 66.*delta*pow(A,-(1./3.)));
//    printf("Energy centroid w(exp) (MeV):     <%6.3f>", w0);
//    fgets_ignore(line,sizeof(line),stdin);
//    sscanf(line,"%f", &w0);
    
//    B_M1 = 0.0042*w0*pow(A,(5./3.))*delta*delta*(g_p-g_n)*(g_p-g_n);
//    printf("\nResults Lo Iudice and Richter, Phys. Lett. B 304, 193.\n");
//    printf("B(M1) strength (u_N**2)       = %f\n",B_M1);
    
//    B_M1 = S_d * w0;
//    printf("Results with S_- * w(exp) \n");
//    printf("B(M1) strength (u_N**2)         =%9.3f\n",B_M1);
//    printf(" \n \n");

/////////////Splitting of scissors mode //////////////////////////  
//    printf(" \n \n");
//    printf("Gamma deformation:        <%6.3f>", gamm);
//    fgets_ignore(line,sizeof(line),stdin);
//    sscanf(line,"%f", &gamm);
//    gam = gamm*(2.*pi/360.);
//    w1 = (cos(gam)+(1./sqrt(3.))*sin(gam))*w0;
//    w2 = (cos(gam)-(1./sqrt(3.))*sin(gam))*w0;
//    w3 =        2.*(1./sqrt(3.))*sin(gam) *w0;
//    B1 = 0.5*(cos(gam)+(1./sqrt(3.))*sin(gam))*B_M1;
//    B2 = 0.5*(cos(gam)-(1./sqrt(3.))*sin(gam))*B_M1;
//    B3 = B_M1-(B1 + B2);
//    printf("E1(MeV) = %6.3f B1(u_N**2) = %6.3f\n",w1,B1);
//    printf("E2(MeV) = %6.3f B2(u_N**2) = %6.3f\n",w2,B2);
//    printf("E3(MeV) = %6.3f B3(u_N**2) = %6.3f\n",w3,B3);
//    printf("For two higher peaks:E12(MeV) = %6.3f B12(u_N**2) = %6.3f\n",(w1*B1+w2*B2)/(B1+B2),B1+B2);


////////////////////////////////////////////////////////////////////////////////
    fp = fopen("input.sci", "w");
    if(fp == NULL){
        printf("Could not open input.sci \n");
    }else{
        fprintf(fp, "%d %d %e %e %e %e %e %e %e %e %d %d, %e",iZ,iA,delta,r_0,m_N,g_p,g_n,I_IV,w0,gamm,ans1,ans2,E2);
    }
    fclose(fp);
    return 0;
}
