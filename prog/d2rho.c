#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>    
#include <math.h>    
#include <fcntl.h>						/* Header for open/read/write */ 
#include <errno.h> 
#include <sys/types.h>          
#include <sys/ioctl.h>          
 
FILE *fp;
char line[128];
int ell = 0, J0;
float alpha, dalpha, y1a, y2a, y3a, y4a, y1b, y2b, y3b, y4b, z1, z2, u1, u2, u3, rho, drho;

/* Defining default values */
float  It  = 2.5, D  = 2.67,   dD = 0.13,   sig = 5.55 ,   dsig = -1;
float  It0 = 2.5, D0 = 2.67,  dD0 = 0.13,  sig0 = 5.55 ,  dsig0 = -1;
float  It1 = 2.5, D1 = 2.67,  dD1 = 0.13,  sig1 = 5.55 ,  dsig1 = -1;

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
    printf(" |                          D2RHO 1.1.3                         |\r\n");
    printf(" |                                                              |\r\n");
    printf(" |   Program to calculate level density from level spacing D    |\r\n");
    printf(" |  at Bn, Bp or Ex. The resonance capture levels are formed by |\r\n");
    printf(" |           neutron or proton s (l=0) or p-waves (l=1)         |\r\n");
    printf(" |            (based on neutron.f by Andreas Schiller)          |\r\n");
    printf(" |                                                              |\r\n");
    printf(" |  Input files: (input.d2r)          Output files: input.d2r   |\r\n");
    printf(" |                                                              |\r\n");
    printf(" | E-mail  : magne.guttormsen@fys.uio.no                        |\r\n");
    printf(" | Created : 15 Dec 2006                                        |\r\n");
    printf(" | Modified: 17 Mar 2007                                        |\r\n");
    printf(" | Modified: 05 Nov 2012                                        |\r\n");
    printf(" |______________________________________________________________|\r\n");
    printf("                                                                 \r\n");

    printf(" The spin cut-off parameter sigma can be found in two ways (A) or (B).\r\n");
    printf(" For the uncertainty, use about 10 percent of sigma\r\n");
    printf(" \r\n");
    printf(" (A) The easiest and recommended way:\r\n");
    printf(" Run the ROBIN program and find sigma (use option 4)\r\n");
    printf(" \r\n");
    printf(" (B) Very old, but values are a sort of standards:\r\n");
    printf(" Take Tables I or II (pages 1448-1450) from the article:\r\n");
    printf(" A. Gilbert, A.G.W. Cameron, Can. Jour. Phys. 43(1965)1446\r\n");
    printf(" Note: Take the row on the left side with target nucleus A-1\r\n");
    printf(" in the neutron capture. This is one less mass number\r\n");
    printf(" than the nucleus (A) that you investigate. If the nucleus\r\n");
    printf(" is not listed there, find another reference...\r\n");
    printf("\n");

    /* ***************************************** */
    /* Reading default values from previous runs */
    /* ***************************************** */
    fp = fopen("input.d2r", "r");
    if(fp == NULL){
        printf("\nCould not open file input.d2r, default values are used \n");
    } else {
        fgets_ignore(line,sizeof(line),fp);
        sscanf(line, " %d \n", &ell);
        fgets_ignore(line,sizeof(line),fp);
        sscanf(line, " %f %f %f %f %f \n", &It0,  &D0,  &dD0,  &sig0,  &dsig0);
        fgets_ignore(line,sizeof(line),fp);
        sscanf(line, " %f %f %f %f %f \n", &It1,  &D1,  &dD1,  &sig1,  &dsig1);
        fclose(fp);
    }

    /* *********************** */
    /* Asking for input values */
    /* *********************** */
    printf("s- (l=0) or p- (l=1) wave neutron/proton capture                    <%1d>:",ell);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%d", &ell);
    if(ell == 0){
        It  = It0;
        D   = D0;
        dD  = dD0;
        sig = sig0;
        dsig= dsig0;
    }
    else{
        It  = It1;
        D   = D1;
        dD  = dD1;
        sig = sig1;
        dsig= dsig1;
    }
	
    printf("Target spin in (n,g) or (p,g) reaction (for the A-1 nucleus)     <%4.1f>:",It);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &It);

    printf("Neutron or proton resonance spacing parameter D (eV)        <%9.2f>:",D);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &D);

    if(dD/D < 0.01) dD = (0.13/2.67)*D;					/* Unreasonable low error */
    printf("Standard deviation for resonance spacing parameter dD (eV)  <%9.2f>:",dD);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &dD);

    printf("Spin cut-off parameter sigma                                <%9.2f>:",sig);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &sig);
	
    if(dsig < 0) dsig = 0.1 * sig;
    printf("Standard deviation of spin cut-off parameter sigma          <%9.2f>:",dsig);
    fgets_ignore(line,sizeof(line),stdin);
    sscanf(line,"%f", &dsig);
    printf("\n");

    alpha = 2. * sig * sig;
    dalpha= 4. * sig * dsig;

    if(ell == 0) {
        if(It == 0){
            y1a = (It+1.)*exp(-(It+1.)*(It+1.)/alpha);
            y1b = (It+1.)*(It+1.)*y1a;
            z1  = y1b;
            z2  = y1a;
            J0  = (int)((It+0.5)*2);
            printf("Spin populated in nucleus A: %2d/2. Parity = parity of gs of nucleus A-1\n",J0);
        }
        else{
            y1a = (It+0.)*exp(-(It+0.)*(It+0.)/alpha);
            y2a = (It+1.)*exp(-(It+1.)*(It+1.)/alpha);
            y1b = (It+0.)*(It+0.)*y1a;
            y2b = (It+1.)*(It+1.)*y2a;
            z1  = y1b+y2b;
            z2  = y1a+y2a;
            if(It == (int)It){
                J0  = (int)((It-0.5)*2);
                printf("Spin populated in nucleus A: %2d/2, %2d/2. Parity = parity of gs of nucleus A-1\n",J0,J0+2);
            }
            else{
                J0  = (int)(It-0.5);
                printf("Spin populated in nucleus A: %2d, %2d. Parity = parity of gs of nucleus A-1\n",J0,J0+1);
            }
        }
    }
    else if(ell == 1) {
        if(It == 0){
            y1a = (It+1.)*exp(-(It+1.)*(It+1.)/alpha);
            y2a = (It+2.)*exp(-(It+2.)*(It+2.)/alpha);
            y1b = (It+1.)*(It+1.)*y1a;
            y2b = (It+2.)*(It+2.)*y2a;
            z1  = y1b+y2b;
            z2  = y1a+y2a;			
            J0  = (int)((It+0.5)*2);
            printf("Spin populated in nucleus A: %2d/2, %2d/2. Parity = opposite parity of gs of nucleus A-1\n",J0,J0+2);
        }
        else if(It == 0.5){
            y1a = (It+0.)*exp(-(It+0.)*(It+0.)/alpha);
            y2a = (It+1.)*exp(-(It+1.)*(It+1.)/alpha);
            y3a = (It+2.)*exp(-(It+2.)*(It+2.)/alpha);
            y1b = (It+0.)*(It+0.)*y1a;
            y2b = (It+1.)*(It+1.)*y2a;
            y3b = (It+2.)*(It+2.)*y3a;
            z1  = y1b+y2b+y3b;
            z2  = y1a+y2a+y3a;
            J0  = (int)(It-0.5);
            printf("Spin/parity states populated in nucleus A: %2d, %2d, %2d. Parity = opposite parity of gs of nucleus A-1\n",J0,J0+1,J0+2);
        }
        else if(It == 1.0){
            y1a = (It+0.)*exp(-(It+0.)*(It+0.)/alpha);
            y2a = (It+1.)*exp(-(It+1.)*(It+1.)/alpha);
            y3a = (It+2.)*exp(-(It+2.)*(It+2.)/alpha);
            y1b = (It+0.)*(It+0.)*y1a;
            y2b = (It+1.)*(It+1.)*y2a;
            y3b = (It+2.)*(It+2.)*y3a;
            z1  = y1b+y2b+y3b;
            z2  = y1a+y2a+y3a;
            J0  = (int)((It-0.5)*2);
            printf("Spin/parity states populated in nucleus A: %2d/2, %2d/2, %2d/2. Parity = opposite parity of gs of nucleus A-1\n",J0,J0+2,J0+4);
        }
        else{
            y1a = (It-1.)*exp(-(It-1.)*(It-1.)/alpha);
            y2a = (It+0.)*exp(-(It+0.)*(It+0.)/alpha);
            y3a = (It+1.)*exp(-(It+1.)*(It+1.)/alpha);
            y4a = (It+2.)*exp(-(It+2.)*(It+2.)/alpha);
            y1b = (It-1.)*(It-1.)*y1a;
            y2b = (It+0.)*(It+0.)*y2a;
            y3b = (It+1.)*(It+1.)*y3a;
            y4b = (It+2.)*(It+2.)*y4a;
            z1  = y1b+y2b+y3b+y4b;
            z2  = y1a+y2a+y3a+y4a;
            if(It == (int)It){
                J0  = (int)((It-1.5)*2);
                printf("Spin/parity states populated in nucleus A: %2d/2, %2d/2, %2d/2, %2d/2. Parity = opposite parity of gs of nucleus A-1\n",J0,J0+2,J0+4,J0+6);
            }
            else{
                J0  = (int)(It-1.5);
                printf("Spin/parity states populated in nucleus A: %2d, %2d, %2d, %2d. Parity = opposite parity of gs of nucleus A-1\n",J0,J0+1,J0+2,J0+3);
            }
        }
    } else {
        printf(" Only l=0 or l=1 waves are taken into account\n");
        exit(0);
    }
	
    u1    = dD / D;
    u2    = dalpha / alpha;
    u3    = 1. - z1 / (alpha * z2);
    rho   = 1.E6 * alpha / (D * z2);
    drho  = rho * sqrt(u1 * u1 + u2 * u2 * u3 * u3);
	
    printf("Level density is rho = %9.3E +/- %9.3E 1/MeV\n", rho, drho);
    printf("(This level density is the total density defined as\n");
    printf("twice the level density obtained for one parity, where we\n");
    printf("asssume equal number of positive and negative parity states.\n");
    printf("If you know D's for l=0 and l=1, the total level density is\n");
    printf("rho(total) = 0.5*rho(l=0) + 0.5*rho(l=1))\n");

    /* **************************************************** */
    /* Storing default values for the next run in input.d2r */
    /* **************************************************** */
    fp = fopen("input.d2r", "w");
    if(fp == NULL){
        printf("Could not open file input.d2r \n");
        exit(0);
    } else {
        if(ell == 0){
            It0  = It;
            D0   = D;
            dD0  = dD;
            sig0 = sig;
            dsig0= dsig;
	} else{
            It1  = It;
            D1   = D;
            dD1  = dD;
            sig1 = sig;
            dsig1= dsig;
	}
	fprintf(fp, " %d \n", ell);
	fprintf(fp, " %f %f %f %f %f \n", It0,  D0,  dD0,  sig0,  dsig0);
	fprintf(fp, " %f %f %f %f %f \n", It1,  D1,  dD1,  sig1,  dsig1);
    }
    fclose(fp);
    return(0);
}
