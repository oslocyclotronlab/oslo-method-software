#include <ctype.h>
#include <math.h>
#include <stdio.h>

static void fgets_ignore(char *s, int size, FILE *stream)
{
    // suppress braindead and presumtuous glibc 'security' warning
    if( !fgets(s, size, stream) )
        return;
}

int main()
{
    float Imax=12.0,Arot=12.3,K=0.0,I1=8.0,E1=921.0,E0,I,Ex;
    char ans='y',inbuf[130];
   
    printf("To what spin will the band be extended        (%4.1f): ",Imax);
    fgets_ignore(inbuf,sizeof(inbuf),stdin);
    sscanf(inbuf,"%f",&Imax);
   
    while (ans=='y') {
  
        printf("Arot (keV) for the rotational band            (%4.1f): ",Arot);
        fgets_ignore(inbuf,sizeof(inbuf),stdin);
        sscanf(inbuf,"%f",&Arot);

        printf("K-projection (hbar) for the rotational band   (%4.1f): ",K);
        fgets_ignore(inbuf,sizeof(inbuf),stdin);
        sscanf(inbuf,"%f",&K);

        printf("Ex (keV) for the upper last known level     (%6.1f): ",E1);
        fgets_ignore(inbuf,sizeof(inbuf),stdin);
        sscanf(inbuf,"%f",&E1);

        printf("Spin for the upper last known level           (%4.1f): ",I1);
        fgets_ignore(inbuf,sizeof(inbuf),stdin);
        sscanf(inbuf,"%f",&I1);
   
        E0 = E1 - (Arot*(I1*(I1+1) - K*K));
        printf("\n\nBandhead found at %6.1f keV ",E0);
      
        for(I=I1; I<=Imax; I=I+1.) {
            Ex = E0 + Arot*(I*(I+1.)-K*K);
            printf("\nSpin = %4.1f	Ex =  %6.1f    ",I,Ex);
        }
 
        printf("New calculation (y/n) (%c): ",ans);
        fgets_ignore(inbuf,sizeof(inbuf),stdin);
        sscanf(inbuf,"%c",&ans);
        ans=tolower(ans);
        printf("\n");
    }

    return 0;
}
