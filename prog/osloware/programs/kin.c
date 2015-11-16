#include <math.h>
#include <stdio.h>
void Menu(void);
float Rho(int Z);
float Mass(int A);
void Absorbator(void);
float Rk4(float Ek, float c, float Mr, float I, float h, long nstep, char list);
float Bethe(float Ek, float c, float Mr, float I);
float Relkin(float Mi, float Mf, float Mr, float Ti, float Q, float thetar);
void Relkinematic(void);
float P2rel(float M, float T);
void Straggling(void);
void Density(void);
void Gram(void);
void Meter(void);
void Ispin(void);

int main()
{
   char cmd;
    do
   {  
      printf("\n\n\n");
      printf(" Menu: Help Bethe Reaction Straggling Density Meter Gram Ispin Exit\n");
      printf(" Press first letter : ");
      cmd = getchar();
      cmd = tolower(cmd);
      switch (cmd)
      {
	  case 'h': Menu(); break;
	  case 'b': Absorbator(); break;
	  case 'r': Relkinematic(); break;
	  case 's': Straggling(); break;
          case 'd': Density(); break;
	  case 'm': Meter(); break;
	  case 'g': Gram(); break;
	  case 'i': Ispin(); break;
	  case 'e': break;
	  default : printf(" Invalid choice. Try again\n");
      }
   }while (cmd != 'e');

   return 0;
}

/* function definitions....*/

void Menu()
{
   printf("\n");
   printf(" H  This command                     \r\n");
   printf(" B  Energyloss due to Bethes formula \r\n");
   printf(" R  Relativistic collision           \r\n");
   printf(" S  Straggling throught target       \r\n");
   printf(" D  Density of element Z             \r\n");
   printf(" M  mg/cm**2 to micrometers          \r\n");
   printf(" G  micrometers to mg/cm**2          \r\n");
   printf(" I  Find Imax for direct reaction    \r\n");
   printf(" E  Exit program                     \r\n");
   printf("\r\n");
}


float Rho(int Z)
{
   float dtab[96]=
   {
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

    if(Z > 96|| Z < 1)
   {
    printf(" Sorry, densities only available for 0 < Z < 97");
    return(0.);
   }
   if(dtab[Z-1]<0.00001)
   {
      printf(" Sorry, no density available for Z= %3d");
   }
   return(dtab[Z-1]);
}


float Mass(int A)
{ 
   float mtab[214] =
      {
       1.0083,2.0141,3.0160,4.0026,5.0124,6.0170,7.0165,8.0175,
       9.0152,10.014,11.014,12.011,13.009,14.005,15.005,16.001,
       17.003,18.002,19.001,20.000,20.996,21.992,22.993,23.992,
       24.988,25.985,26.984,27.984,28.980,29.976,30.976,31.977,
       32.973,33.972,34.971,35.968,36.969,37.969,38.967,39.965,
       40.963,41.961,42.959,43.956,44.956,45.954,46.953,47.951,
       48.952,49.946,50.945,51.943,52.941,53.940,54.939,55.937,
       56.936,57.935,58.934,59.932,60.931,61.928,62.928,63.929,
       64.929,65.927,66.927,67.925,68.926,69.925,70.926,71.924,
       72.923,73.922,74.922,75.921,76.921,77.919,78.919,79.916,
       80.916,81.915,82.916,83.912,84.912,85.910,86.913,87.908,
       88.906,89.906,90.906,91.905,92.906,93.905,94.907,95.907,
       96.909,97.906,98.907,99.906,100.91,101.90,102.91,103.91,
       104.91,105.91,106.91,107.90,108.90,109.90,110.91,111.90,
       112.90,113.90,114.90,115.90,116.91,117.90,118.90,119.90,
       120.90,121.90,122.90,123.91,124.91,125.90,126.91,127.90,
       128.91,129.91,130.91,131.91,132.91,133.91,134.91,135.91,
       136.91,137.91,138.91,139.91,140.91,141.91,142.91,143.91,
       144.91,145.91,146.92,147.92,148.92,149.92,150.92,151.92,
       152.92,153.92,154.92,155.92,156.92,157.92,158.93,159.93,
       160.93,161.93,162.93,163.93,164.93,165.93,166.93,167.93,
       168.93,169.94,170.94,171.94,172.94,173.94,174.94,175.94,
       176.94,177.94,178.95,179.95,180.95,181.95,182.95,183.95,
       184.95,185.95,186.96,187.96,188.96,189.96,190.96,191.96,
       192.96,193.96,194.97,195.97,196.97,197.97,198.97,199.97,
       200.97,201.91,202.97,203.97,204.98,205.98,206.98,207.98,
       208.98,209.99,210.99,211.99,212.99,214.00
   };
   if(A > 214 || A < 1)
   {
      printf(" Sorry, masses only available for 0 < A < 215");
      return(0.0);
   }
   if(mtab[A-1]==0)
   {
      printf(" Sorry, no mass available for A= %3d");
   }
   return(mtab[A-1]);
}

void Absorbator()
{
   int Z=14, A=28, Zi=2, Ai=3;
   long nstep=1000;
   float Ti=45., hmax=19.0;
   char list='n', inbuf[130];
   float Ek, h, I, c, Mir;
   
   gets(inbuf);
   printf(" Give Z for the absorbating medium (%3d): ",Z);
   gets(inbuf);
   sscanf(inbuf,"%d",&Z);

   printf(" Give A for the absorbating medium (%3d): ",A);
   gets(inbuf);
   sscanf(inbuf,"%d",&A);

   printf(" Give Z for the projectile (%3d): ",Zi);
   gets(inbuf);
   sscanf(inbuf,"%d",&Zi);

   printf(" Give A for the projectile (%3d): ",Ai);
   gets(inbuf);
   sscanf(inbuf,"%d",&Ai);

   printf(" Give the thickness (micrometer) (%4.1f): ",hmax);
   gets(inbuf);
   sscanf(inbuf,"%f",&hmax);

   printf(" Give # integration step (%ld): ",nstep);
   gets(inbuf);
   sscanf(inbuf,"%ld",&nstep);

   printf(" Do you want energies for each integration step (y/n) (%c): ",list);
   gets(inbuf);
   sscanf(inbuf,"%c",&list);
   list=tolower(list);

   if(Rho(Z)*Mass(Ai)<0.0001) return;
   h   = hmax/(float)nstep*1.e-4;
   I   = 9.1*Z*(1+1.9*pow(Z,(-2./3.)))*1.e-6;
   c   = 0.30707*Rho(Z)*Z/(float)A*Zi*Zi;
   Mir = (Mass(Ai)-Zi*0.000549)*931.5016;

   printf(" Give energy of the projectile (-1 = stop) (%3.1f): ",Ti);
   gets(inbuf);
   sscanf(inbuf,"%f",&Ti);

   while (Ti!=-1)
   {
   Ek = Ti;
   Ek = Rk4(Ek, c, Mir, I, h, nstep, list);
   printf(" The final energy is %7.3f MeV with energy loss of %7.3f MeV",
   Ek, Ti-Ek);
   printf("\n\n Give energy of the projectile (-1 = stop) (%3.1f): ",Ti);
   gets(inbuf);
   sscanf(inbuf,"%f",&Ti);
   }
}


float Rk4(float Ek, float c, float Mr, float I, float h, long nstep, char list)
{
   long j;
   float f1, f2, f3, f4, pos;
   if(list=='y')
   {
      printf("\n Absorbation-depth (micrometer)  Kinetic energy (MeV)");

   }

   for(j=1; j<=nstep; j++)
   {
      f1 = h*Bethe(Ek,c,Mr,I);
      f2 = h*Bethe(Ek+0.5*f1,c,Mr,I);
      f3 = h*Bethe(Ek+0.5*f2,c,Mr,I);
      f4 = h*Bethe(Ek+f3,c,Mr,I);
      Ek = Ek + (f1+f2+f2+f3+f3+f4)/6.0;
      if(Ek <= 0)
      {
	 Ek = 0;
	 pos = j*h*1.e+4;
	 printf("\n        %7.3f	                      %7.3f         ",pos,Ek);
	 j = nstep;
	 list = 'n';
      }
      if(list=='y')
      {
	 pos = j*h*1.e+4;
	 printf("\n        %7.3f	                      %7.3f         ",pos,Ek);
      }
   }
if(list=='y') printf("\n");
return(Ek);
}


float Bethe(float Ek, float c, float Mr, float I)
{
   float Bth, tt, b2, Ar;
   tt = Ek/Mr;
   b2 = tt*(tt+2)/((tt+1.)*(tt+1.));
   Ar = 1.022*b2/(I*(1.-b2));
   if(Ar > 1.) Bth = -(c/b2)*(log(Ar)-b2);
return(Bth);
}



void Relkinematic()
{
   int Ai=3, Af=4, Ar=172;
   float Ti=45., Q=15., theta=45., thetar;
   float Mi, Mf, Mr, Ek;
   char inbuf[130];
   
   gets(inbuf);
   printf(" Give A for the projectile (%3d): ",Ai);
   gets(inbuf);
   sscanf(inbuf,"%d",&Ai);

   printf(" Give A for the light-product (%3d): ",Af);
   gets(inbuf);
   sscanf(inbuf,"%d",&Af);

   printf(" Give A for the rest-nucleus (%3d): ",Ar);
   gets(inbuf);
   sscanf(inbuf,"%d",&Ar);

   printf(" Give the energy of the projectile (%3.1f): ",Ti);
   gets(inbuf);
   sscanf(inbuf,"%f",&Ti);

   printf(" Give Q-value for the reaction (%3.1f): ",Q);
   gets(inbuf);
   sscanf(inbuf,"%f",&Q);

   printf(" Give angle in degree (-1 = stop) (%3.1f): ",theta);
   gets(inbuf);
   sscanf(inbuf,"%f",&theta);

   while(theta != -1)
   {
      thetar = 2.*3.1415927*theta/360.;
      Mi = Mass(Ai);
      Mf = Mass(Af);
      Mr = Mass(Ar);
      if(Mr*Mf*Mi<0.0001) return;
      Ek = Relkin(Mi, Mf, Mr, Ti, Q, thetar);
      printf(" Kinetic energy of the light-product %6.3f",Ek);
      printf("\n\n Give angle in degree (-1 = stop) (%3.1f): ",theta);
      gets(inbuf);
      sscanf(inbuf,"%f",&theta);
   }
}


float Relkin(float Mi, float Mf, float Mr, float Ti, float Q, float thetar)
{
   float t1f,t2f,t3f,t1r,t2r,f1,f2;
   float p21r,p22r,p21f,p22f,p2i;

/* start values*/

   t1f = (Ti+Q)*0.6;
   t2f = (Ti+Q)*0.8;
   while (fabs(t2f-t1f)>1.e-10)
   {
      t1r  = Q + Ti - t1f;
      t2r  = Q + Ti - t2f;
      p21r = P2rel(Mr,t1r);
      p22r = P2rel(Mr,t2r);
      p2i  = P2rel(Mi,Ti);
      p21f = P2rel(Mf,t1f);
      p22f = P2rel(Mf,t2f);


      f1 = p21r - p2i - p21f + 2*cos(thetar)*sqrt(p2i*p21f);
      f2 = p22r - p2i - p22f + 2*cos(thetar)*sqrt(p2i*p22f);

/*    the secant formula:*/

      t3f = t2f - f2*(t2f-t1f)/(f2-f1);

/*    new values for the iteration:*/
      t1f = t2f;
      t2f = t3f;
   }
return(t2f);
}


float P2rel(float M, float T)
{
   float x;
   x = 2.*931.5016*M*T + T*T;
return(x);
}



void Straggling()
{
   int Z=70, A=173, Zi=2, Ai=3, Zf=2, Af=4, Zr, Ar;
   long nstep =1000;
   float Ti=45., d=2., Q=15., Ex=0., theta=45., thetar;
   float I, c, Mi, Mf, Mr, Mir, Mfr;
   float d1, d2, E1s, E2s, h1, h2, dh1, dh2;
   float Ek1, Ek2, Ekf, dE;
   char inbuf[130];
   
   gets(inbuf);
   printf(" Give Z for the absorbating medium (%3d): ",Z);
   gets(inbuf);
   sscanf(inbuf,"%d",&Z);

   printf(" Give A for the absorbating medium (%3d): ",A);
   gets(inbuf);
   sscanf(inbuf,"%d",&A);

   printf(" Give Z for the projectile (%3d): ",Zi);
   gets(inbuf);
   sscanf(inbuf,"%d",&Zi);

   printf(" Give A for the projectile (%3d): ",Ai);
   gets(inbuf);
   sscanf(inbuf,"%d",&Ai);

   printf(" Give Z for the light-product (%3d): ",Zf);
   gets(inbuf);
   sscanf(inbuf,"%d",&Zf);

   printf(" Give A for the light-product (%3d): ",Af);
   gets(inbuf);
   sscanf(inbuf,"%d",&Af);

   printf(" Give energy of the projectile (%3.1f): ",Ti);
   gets(inbuf);
   sscanf(inbuf,"%f",&Ti);

   printf(" Give the thickness of target (mg/cm**2) (%3.1f): ",d);
   gets(inbuf);
   sscanf(inbuf,"%f",&d);

   printf(" Give Q-value for the reaction (%3.1f): ",Q);
   gets(inbuf);
   sscanf(inbuf,"%f",&Q);

   printf(" Give exitation-energy for the nucleus (%3.1f): ",Ex);
   gets(inbuf);
   sscanf(inbuf,"%f",&Ex);
   if(Rho(Z)*Mass(Ai)*Mass(Af)<0.0001) return;
   I = 9.1*Z*(1.+1.9*pow(Z,(-2./3.)))*1e-6;
   c  = 0.30707*Rho(Z)*Z/(float)A*Zi*Zi;
   Zr = Z + Zi - Zf;
   Ar = A + Ai - Af;
   Mi = Mass(Ai) - Zi*0.000549;
   Mf = Mass(Af) - Zf*0.000549;
   Mr = Mass(Ar) - Zr*0.000549;
   Mir = Mi*931.5016;
   Mfr = Mf*931.5016;

   printf(" Give angle in degree (-1 = stop) (%3.1f): ",theta);
   gets(inbuf);
   sscanf(inbuf,"%f",&theta);
   while(theta!=-1)
   {
      thetar = 2*3.14159*theta/360.;
      d1  = d;
      d2  = d/cos(thetar);
      E1s = 18.*Zi*sqrt(d1);
      E2s = 18.*Zf*sqrt(d2);
      h1  = d1/Rho(Z)*1.e-5;
      h2  = d2/Rho(Z)*1.e-5;
      dh1 = h1/(float)nstep*100.;
      dh2 = h2/(float)nstep*100.;

      Ek1 = Ti;
      Ek2 = Ti;
      Ek1 = Rk4(Ek1,c,Mir,I,dh1,nstep,'n');
      Ek1 = Relkin(Mi,Mf,Mr,Ek1,Q,thetar);
      Ek2 = Relkin(Mi,Mf,Mr,Ek2,Q,thetar);
      Ek2 = Rk4(Ek2-Ex,c,Mfr,I,dh2,nstep,'n');
      Ekf = sqrt(.5*((Ek1-Ex)*(Ek1-Ex)+Ek2*Ek2));
      dE  = sqrt(((Ek1-Ex-Ek2)*1000.)*((Ek1-Ex-Ek2)*1000.) + 0.5*(E1s*E1s+E2s*E2s));
      printf(" The final energy is %6.3f MeV  with FWHM %5.1f keV",Ekf,dE);
      printf(" \n\n Give angle in degree (-1 = stop) (%3.1f): ",theta);
      gets(inbuf);
      sscanf(inbuf,"%f",&theta);
   }
}

void Density()
{
   char inbuf[130];
   int Z = 13;
   
   gets(inbuf);
   printf(" Give atomic mass number Z (%d): ",Z);
   gets(inbuf);
   sscanf(inbuf,"%d",&Z);

   if(Rho(Z)<0.00001)return;
   printf(" Density is:  %5.2f g/cm**3",Rho(Z));
   return;
}

void Gram()
{
   char inbuf[130];
   float tmicro = 19.;
   int Z = 13;

   gets(inbuf);
   printf(" Give thickness in micrometer (%3.1f): ",tmicro);
   gets(inbuf);
   sscanf(inbuf,"%f",&tmicro);

   printf(" Give atomic mass number Z (%d): ",Z);
   gets(inbuf);
   sscanf(inbuf,"%d",&Z);

   if(Rho(Z)<0.00001)return;
   printf(" Thickness in mg/cm**2 is:  %5.2f",tmicro*Rho(Z)/10.);
   return;
}

 void Meter()
{
   char inbuf[130];
   float tmg = 2.2;
   int Z = 13;
   
   gets(inbuf);
   printf(" Give thickness in mg/cm**2 (%3.1f): ",tmg);
   gets(inbuf);
   sscanf(inbuf,"%f",&tmg);

   printf(" Give atomic mass number Z (%d): ",Z);
   gets(inbuf);
   sscanf(inbuf,"%d",&Z);

   if(Rho(Z)<0.00001)return;
   printf(" Thickness in micrometer is:  %5.2f",tmg*10./Rho(Z));
   return;
}


void Ispin()
{
   int Ai=3, Af=4, At=163, Ar=162;
   float Ti=45., Tf = 20., Q=15., Ex, Theta=45., Thetar;
   float Mi, Mf, Mt, Mr, k1, k2, R1, R2, DSpin, CSpin;
   char inbuf[130];
   
   
   
   gets(inbuf);
   
   printf(" Give Q-value for the reaction (%3.1f): ",Q);
   gets(inbuf);
   sscanf(inbuf,"%d",&Q);
      
   printf(" Give A for the projectile (%3d): ",Ai);
   gets(inbuf);
   sscanf(inbuf,"%d",&Ai);

   printf(" Give A for the light-product (%3d): ",Af);
   gets(inbuf);
   sscanf(inbuf,"%d",&Af);

   printf(" Give A for the target-nucleus (%3d): ",At);
   gets(inbuf);
   sscanf(inbuf,"%d",&At);
   Ar=At+Ai-Af;

   printf(" Give the energy of the projectile (%3.1f): ",Ti);
   gets(inbuf);
   sscanf(inbuf,"%f",&Ti);

   printf(" Give the energy of the ejectile   (%3.1f): ",Tf);
   gets(inbuf);
   sscanf(inbuf,"%f",&Tf);
   Ex=Q+Ti-Tf;

   printf(" Give scattering angle in degrees  (%3.1f): ",Theta);
   gets(inbuf);
   sscanf(inbuf,"%f",&Theta);
   
/* Transfering values to MeV and fm. Atomic masses are used (incorrect!) */
   Thetar = 2.*3.1415927*Theta/360.;
   Mi = Mass(Ai)*931.502;
   Mf = Mass(Af)*931.502;
   Mr = Mass(Ar)*931.502;
   Mt = Mass(At)*931.502;
   
/* Finding the corresponding wavenumbers k in units of 1/fm */
   k1=(1./197.329)*sqrt(2.*Mi*Ti);  
   k2=(1./197.329)*sqrt(2.*Mf*Tf);
 
/* Finding average radius of the nucleus */
   R1=1.25*(pow(At,0.333333)+pow(Ai,0.333333));
   R2=1.25*(pow(Ar,0.333333)+pow(Af,0.333333));

/* Calculating the maximum spin transfer */
   DSpin=0.5*(R1+R2)*sqrt(k1*k1+k2*k2 -2.*k1*k2*cos(Thetar));
   CSpin=R1*k1 + R2*k2;
       
   printf(  " Maximum spintransfer for direct reaction at Ex= %6.3f is %6.3f",Ex,DSpin);
   printf("\n Maximum spintransfer for comp.  reaction at Ex= %6.3f is %6.3f",Ex,CSpin);   
 
}



