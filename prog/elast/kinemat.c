/* ******************************************************************** */
/*                                                                      */
/*                           K I N E M A T . C                          */
/*                                                                      */
/*               Containing relatevistic kinematics for ELAST.          */
/*                                                                      */
/* ******************************************************************** */


/* Thanks to Dr. Josef Homolka, who gave me an existing c-code and his
   formulars to use in this part of ELAST */

/* INCLUDE */

#include "constants.c"
#include <math.h>

/* ***** Prototypes ***** */

double
  asin(),
  atan(),
  exp(),
  fabs(),
  log(),
  log10(),
  pow(),
  sin(),
  sqrt();

/* ***** define Macros ***** */

  /* All in units MeV where c = 1 */
  /* T = Kinetic energy */
  /* B = BETA = v/c */


#define T2P(t,m) (sqrt( (t) * (2. * (m) + (t)))) 
#define T2B(t,m) (sqrt((t) * (2. * (m) + (t)))/((m) + (t)))
#define P2T(p,m) (p * p /(m + sqrt(m * m + p * p)))
#define U2MeV(m) 931.4943 * (m)                           /* Units to MeV */

  /* some constants */

#define EPS 1.0E-15  /* a constant */
#ifndef TRUE
# define TRUE 1
# define FALSE 0
#endif

/* define "the buffer" */

typedef struct  { double T1;    double T2; 
		  double TCMin; double TCMout;   /* kin. energy */
		  double T1CM;  double T2CM;
		  double T3CM;  double T4CM;     /* kin. energy com */
		  double P1;    double P2;       /* momentum */
		  double PCMout; double PCMin;
		  double M1;    double M2;
		  double M3;    double M4;       /* masses */
		  double A3;    double A3CM;
		  double A4;    double A4CM;
		  double Gamma;
		  double Beta;
		  double Beta1CM; double Beta2CM;
		  double Beta3CM; double Beta4CM; /* v/c */
		  double ECM;   double Q; } kine; 

 /* Energy in com, Q */
			
 kine k;

/* ***** Functions ***** */


/* cal_kin defines most fields the global structure k, which
   (partly redundantent) define the bibary inelastic encounter

                    M1(M2,M3)M4

   given by the values in k. Note, that the Q-value is the difference
   of the masses, which are invariant mases, and that all units are
   MeV, Mev/c, MeV/(c*c) and so on.

   before calling cal_kin, it is necessary to set the four masses in
   k and k.T1 as well es K.T2 

   FALSE is returned for an energetic impossible reaction, else TRUE */


int cal_kin()

{
  /* var */

  double x,
         Gamma1;

   int i;
        
  /* code */
  
  k.P1 = T2P(k.T1, k.M1);
  k.P2 = T2P(k.T2, k.M2);

  k.Beta = (k.P1 + k.P2)/(k.M1 + k.M2 + k.T1 + k.T2);
  x = 2. * (k.M2*k.T1 + k.M1*k.T2 + k.T1*k.T2 - k.P1*k.P2)/((k.M1+k.M2) * 
                                                (k.M1+k.M2));

  k.TCMin  = (k.M1 + k.M2) * x / (1. + sqrt(1. + x));

  k.ECM    = k.M1 + k.M2 + k.TCMin;

  Gamma1 = (k.T1 + k.T2 - k.TCMin) / k.ECM;
  k.Gamma  = Gamma1 + 1.;

  k.Q      = k.M1 + k.M2 - k.M3 - k.M4;

  if ((k.Q < 1E-10) && (k.Q > -1E-10))          /* little dirty trick to */
    k.Q = -1E-10;                               /* avoid numeric problems */


  if ((k.TCMout = k.TCMin + k.Q) < 0)           /* energetic impossible */
    return FALSE;

  k.T1CM = 0.5 * k.TCMin  * (k.TCMin  + 2. * k.M2) / k.ECM;
  k.T2CM = 0.5 * k.TCMin  * (k.TCMin  + 2. * k.M1) / k.ECM;
  k.T3CM = 0.5 * k.TCMout * (k.TCMout + 2. * k.M4) / k.ECM;
  k.T4CM = 0.5 * k.TCMout * (k.TCMout + 2. * k.M3) / k.ECM;

  k.Beta1CM = T2B(k.T1CM, k.M1);
  k.Beta2CM = T2B(k.T2CM, k.M2);
  k.Beta3CM = T2B(k.T3CM, k.M3);
  k.Beta4CM = T2B(k.T4CM, k.M4);

  k.PCMin  = T2P(k.T2CM, k.M2);
  k.PCMout = T2P(k.T3CM, k.M3);

  return TRUE;
  
}
 
  /* ECoM ****

     This function calculates the center of mass energy
     for a two particle collision. The target particle
     is assumed have no energy in the lab system. Energy
     is given in MeV (or what ever), mass in nuc. mass units.
     (or what ever).

    */

double ECoM(double M1,   /* Mass of 1st particle (target) */
	    double M2,   /* Mass of 2nd particle (projektile) */
	    double M3,   /* Mass of 3rd particle (Ejektile) */
	    double M4,   /* Mass of 4rd particle (Residual) */
	    double T1,   /* Energy of Target (mostly 0...) */
	    double T2)   /* Energy of projektile in the Lab. */ 
{

  /* var */


  /* code */

  if ((T1 < 0) || (T2 < 0 ))
    return -1.0;

  k.M1 = U2MeV(M1);
  k.M2 = U2MeV(M2);
  k.M3 = U2MeV(M3);
  k.M4 = U2MeV(M4);
  k.T1 = T1;
  k.T2 = T2;

  if (! cal_kin())    /* impossible reaction */
    return 0.0;

  if ((k.T1CM + k.T2CM + k.Q) > 0)
    return (k.T1CM + k.T2CM + k.Q);
  else
    return 0;

}

  /* V_C ****

     This function v / C0, the spead of light. Mass is given
     in unit, energy is given in MeV

    */


double V_C(double M0,   /* constant mass of particle */
	   double E)    /* Energy of projektile */ 

{

  /* var */

  double c2 = C0 * C0;    /* C0 sqare */

  /* code */

  E  = E * Q_EL * 1.0E6;   /* energy in joule */
  M0 = M0 * UNIT;          /* mass in kg */

  return (sqrt (1 - (1/( (E*E/(M0*M0*c2*c2) + (2*E/(M0*c2)) +1) ))));

}
   /* AngLab2CM
 
     The Forward/Only solution for the CM-Angle. 
     ThLab is in degree, NOT the output, which is in rad! 

     This function calculates the CM-Angle to a given Lab Angle. Call
     this fuction AFTER you called cal_kin! This is not a stand-alone
     calculation! Uses k.Beta, ...

    */


double AngLab2CM(double ThLab)
	     
{

  /* var */

  double A     = 0.0, /* tan(thLab/2)= (-1 +-sqrt(1 + B^2 - A^2))/(B-A) */
         B     = 0.0,
         sqDet = 0.0;

  /* code */

  ThLab = ThLab * PI / 180;
  B     = tan(ThLab)*k.Gamma;
  A     = B * k.Beta/k.Beta3CM;
  sqDet = sqrt(1 + B*B - A*A);

  if ((k.Beta < k.Beta3CM) && ( ThLab > (PI/2)))
    return fabs( 2*atan ( (-1-sqDet) / (B-A+1.0E-33)));
  else    
    return fabs( 2*atan ( (-1+sqDet) / (B-A+1.0E-33)));
 
}

   /* AngLab2CM2
 
     The backward solution for the CM-Angle. NOTE that this function
     produces nonsens, if there is no backward solution! 
     ThLab is in degree, NOT the output, which is in rad! 

     This function calculates the CM-Angle to a given Lab Angle. Call
     this fuction AFTER you called cal_kin! This is not a stand-alone
     calculation! Uses k.Beta, ...

    */


double AngLab2CM2(double ThLab)
	     
{

  /* var */

  double A     = 0.0, /* tan(thLab/2)= (-1 +-sqrt(1 + B^2 - A^2))/(B-A) */
         B     = 0.0,
         sqDet = 0.0;

  /* code */

  ThLab = ThLab * PI / 180;
  B     = tan(ThLab)*k.Gamma;
  A     = B * k.Beta/k.Beta3CM;
  sqDet = sqrt(1 + B*B - A*A);

  return fabs( 2*atan ( (-1-sqDet) / (B-A+1.0E-33)));

}

  /* CMAngle  ****

     For particle 3:

     Calculate (both) CM-Angles for a given LAB angle, pass the results
     (in degree) back in ACMF (forward) and ACMB (backward). If there
     is no backward solution, make that ACMB 0.

     return the actual number of valid angles (0, 1 (fwd only), 2)

    */


int CMAngle(double MT,      /* Masses of the projektil ... */
            double MP,      /* ... the target (1st one normaly) ... */
	    double MProd,   /* ... the product (of intrest)... */
	    double MRes,    /* ... and the last one */
            double T1,      /* Energy of target (usualy 0) */
            double T2,      /* Energy of projektile */
	    double Angle,   /* The Lab angle of particle 3 */
	    double *ACMF,   /* return here the fwd. CM angle */
	    double *ACMB)   /* return here the bwd. CM angle */
{

  /* var */

  /* code */

  k.M1 = U2MeV(MT);
  k.M2 = U2MeV(MP);
  k.M3 = U2MeV(MProd);
  k.M4 = U2MeV(MRes);
  k.T1 = T1;
  k.T2 = T2;

  if (! cal_kin())    /* impossible reaction */
    {
      *ACMF = 0.0;
      *ACMB = 0.0;
      return 0;
    };
      
  if (k.Beta > k.Beta3CM)   /* two solutions */
    {
      if (                 (360 / (2*PI)) * atan ( 1 /
        (k.Gamma * sqrt ((k.Beta * k.Beta) / (k.Beta3CM * k.Beta3CM) -1)))
			           >
               	                Angle )    /* Angle < MaxAngle */
	{ 
	  *ACMF = AngLab2CM(Angle) / PI * 180;
	  *ACMB = AngLab2CM2(Angle) / PI * 180;
	  return 2;
	}
      else
	{
	  *ACMF = 0.0;
	  *ACMB = 0.0;
	  return 0;
	};
    }
  else
    {
      *ACMF = AngLab2CM(Angle) / PI * 180;
      *ACMB = 0.0;      
      return 1;
    };

}

  /* MaxAngle  ****

     This function calculates the maximum angle in Lab for the
     product particle in an inelastic binary encounter.

    */


double MaxAngle(double MT,      /* Masses of the projektil ... */
		double MP,      /* ... the target (1st one normaly) ... */
		double MProd,   /* ... the product (of intrest)... */
		double MRes,    /* ... and the last one */
		double T1,      /* Energy of target (usualy 0) */
		double T2)      /* Energy of projektile */ 
{

  /* var */

  /* code */

  k.M1 = U2MeV(MT);
  k.M2 = U2MeV(MP);
  k.M3 = U2MeV(MProd);
  k.M4 = U2MeV(MRes);
  k.T1 = T1;
  k.T2 = T2;

  if (! cal_kin())    /* impossible reaction */
    return 0.0;
  
  if (k.Beta > k.Beta3CM)
    return (360 / (2*PI)) * atan ( 1 /
        (k.Gamma * sqrt ((k.Beta * k.Beta) / (k.Beta3CM * k.Beta3CM) -1)));
  else
    return 180;

}

  /* MaxEnergy  ****

     This function calculates the maximum energy in Lab for the
     product particle in an inelastic binary encounter.

    */


double MaxEnergy(double MT,      /* Masses of the projektil ... */
		 double MP,      /* ... the target (1st one normaly) ... */
		 double MProd,   /* ... the product (of intrest)... */
		 double MRes,    /* ... and the last one */
		 double T1,      /* Energy of target (usualy 0) */
		 double T2,      /* Energy of projektile */ 
		 double Angle)   /* Angle in the Lab */
{

  /* var */

  /* code */

  k.M1 = U2MeV(MT);
  k.M2 = U2MeV(MP);
  k.M3 = U2MeV(MProd);
  k.M4 = U2MeV(MRes);
  k.T1 = T1;
  k.T2 = T2;

  if (! cal_kin())    /* impossible reaction */
    return 0.0;
  
  if (MaxAngle(MT, MP, MProd, MRes, T1, T2) < Angle)
    return 0.0;

  return (k.Gamma-1)*k.M3 + k.Gamma*k.T3CM + 
            k.Gamma * k.Beta * T2P(k.T3CM,k.M3) * cos(AngLab2CM(Angle));
  
}
  /* MinEnergy  ****

     This function calculates the minimum energy in Lab for the
     product particle in an inelastic binary encounter.

    */


double MinEnergy(double MT,      /* Masses of the projektil ... */
		 double MP,      /* ... the target (1st one normaly) ... */
		 double MProd,   /* ... the product (of intrest)... */
		 double MRes,    /* ... and the last one */
		 double T1,      /* Energy of target (usualy 0) */
		 double T2,      /* Energy of projektile */ 
		 double Angle)   /* Angle in the Lab */
{

  /* var */

  /* code */

  k.M1 = U2MeV(MT);
  k.M2 = U2MeV(MP);
  k.M3 = U2MeV(MProd);
  k.M4 = U2MeV(MRes);
  k.T1 = T1;
  k.T2 = T2;

  if (! cal_kin())    /* impossible reaction */
    return 0.0;
  
  if (MaxAngle(MT, MP, MProd, MRes, T1, T2) < Angle)
    return 0.0;

  if (k.Beta3CM > k.Beta) /* there is no backward solution! */
    return 0.0;

  return (k.Gamma-1)*k.M3 + k.Gamma*k.T3CM + 
            k.Gamma * k.Beta * T2P(k.T3CM,k.M3) * cos(AngLab2CM2(Angle));
}



  /* MidEnergy  ****

     This function calculates a middle energy of the Reaktion
     Product in Lab for an inelastic binary encounter. The
     Abgle Parameter to elast is interpreted as an COM-Angle (!!),
     allowing to find solutions matching certaine CM-Angles.

    */


double MidEnergy(double MT,      /* Masses of the projektil ... */
                 double MP,      /* ... the target (1st one normaly) ... */
                 double MProd,   /* ... the product (of intrest)... */
                 double MRes,    /* ... and the last one */
	         double T1,      /* Target energy */
		 double T2,      /* Energy of projektile */ 
		 double Angle)   /* Angle in lab for Product */ 
{

  /* var */


  /* code */

  k.M1 = U2MeV(MT);
  k.M2 = U2MeV(MP);
  k.M3 = U2MeV(MProd);
  k.M4 = U2MeV(MRes);
  k.T1 = T1;
  k.T2 = T2;

  if (! cal_kin())    /* impossible reaction */
    return 0.0;

  return (k.Gamma-1)*k.M3 + k.Gamma*k.T3CM + 
            k.Gamma * k.Beta * T2P(k.T3CM,k.M3) * cos(Angle/180.0*PI);

}
  /* JakoFWD  ****

     This function calculates the ration dOmega(lab)/dOmega(CM) of 
     the Reaktion Product using the forward/only solution

     I'm not sure, if this is relativistically correct!!
     
    */


double JakoFWD   (double MT,      /* Masses of the projektil ... */
                 double MP,      /* ... the target (1st one normaly) ... */
                 double MProd,   /* ... the product (of intrest)... */
                 double MRes,    /* ... and the last one */
	         double T1,      /* Target energy */
		 double T2,      /* Energy of projektile */ 
		 double Angle)   /* Angle in lab for Product */ 
{

  /* var */

  double AngleCM;

  /* code */

  k.M1 = U2MeV(MT);
  k.M2 = U2MeV(MP);
  k.M3 = U2MeV(MProd);
  k.M4 = U2MeV(MRes);
  k.T1 = T1;
  k.T2 = T2;

  if (! cal_kin())    /* impossible reaction */
    return 0.0;

  if (MaxAngle(MT, MP, MProd, MRes, T1, T2) < Angle)
    return 0.0;

  if (Angle < 0.001)  /* too lazy to program the limit of that function! */
    Angle = 0.001;
  AngleCM = AngLab2CM(Angle);   /* input of this function is in degree! */
  Angle = Angle * PI / 180;

  return (sin(Angle)*sin(Angle)) / (sin(AngleCM)*sin(AngleCM)) *
		 cos(AngleCM-Angle);

}
  /* JakoBWD  ****

     This function calculates the ration dOmega(lab)/dOmega(CM) of 
     the Reaktion Product using the forward/only solution

     I'm not sure, if this is relativistically correct!!
     
    */


double JakoBWD  (double MT,      /* Masses of the projektil ... */
                 double MP,      /* ... the target (1st one normaly) ... */
                 double MProd,   /* ... the product (of intrest)... */
                 double MRes,    /* ... and the last one */
	         double T1,      /* Target energy */
		 double T2,      /* Energy of projektile */ 
		 double Angle)   /* Angle in lab for Product */ 
{

  /* var */

  double AngleCM;

  /* code */

  k.M1 = U2MeV(MT);
  k.M2 = U2MeV(MP);
  k.M3 = U2MeV(MProd);
  k.M4 = U2MeV(MRes);
  k.T1 = T1;
  k.T2 = T2;

  if (! cal_kin())    /* impossible reaction */
    return 0.0;

  if (k.Beta3CM > k.Beta) /* there is no backward solution! */
    return 0.0;

  if (MaxAngle(MT, MP, MProd, MRes, T1, T2) < Angle)
    return 0.0;

  if (Angle < 0.001)  /* too lazy to program the limit of that function! */
    Angle = 0.001;
  AngleCM = AngLab2CM2(Angle);   /* input of this function is in degree! */
  Angle = Angle * PI / 180;

  return -(sin(Angle)*sin(Angle)) / (sin(AngleCM)*sin(AngleCM)) *
		 cos(AngleCM-Angle);

}

/*   LabAngle4B ****

     This function returns the angle in degree for the 4th pathicle of
     a reaction, fitting the higher or only energy solution.

     */

double LabAngle4B(double MT,      /* Masses of the projektil ... */
		  double MP,      /* ... the target (1st one normaly) ... */
		  double MProd,   /* ... the product (of intrest)... */
		  double MRes,    /* ... and the last one */
		  double T1,      /* Energy of target (usualy 0) */
		  double T2,      /* Energy of projektile */ 
		  double Angle)   /* Angle in the Lab */
{

  /* var */

  double AngleCM4 = 0,
         Gamma4CM = 0,
         ALab     = 0;

  /* code */

  k.M1 = U2MeV(MT);
  k.M2 = U2MeV(MP);
  k.M3 = U2MeV(MProd);
  k.M4 = U2MeV(MRes);
  k.T1 = T1;
  k.T2 = T2;

  if (! cal_kin())    /* impossible reaction */
    return 0.0;
  
  if (MaxAngle(MT, MP, MProd, MRes, T1, T2) < Angle)
    return 0.0;


  AngleCM4 = PI - AngLab2CM(Angle);  /* allways take the bw. or only
                                        solution to the 3rd particle
                                        (fwd. or only!) */


  Gamma4CM = 1/sqrt(1 - k.Beta4CM*k.Beta4CM);

  ALab = -180/PI*atan( sin(AngleCM4)/
		      (k.Gamma * cos(AngleCM4) + k.Beta/k.Beta4CM));

  if (ALab > 0)
    ALab += -180;


  if (fabs(ALab) > 179.9999 ||
      fabs(ALab) <   0.0001)
    {
      if ((k.Beta < k.Beta4CM && fabs(ALab) > 179.0)
	  || (k.Beta < k.Beta4CM) && (Angle < 0.0001))
	return -180.0;
      else
	return 0.0;
    }
  else
    return ALab;

}

/*   LabAngle4F ****

     This function returns the angle in degree for the 4th pathicle of
     a reaction, fitting the lower energy solution, IF there are two 
     solutions, otherwise 0.

     */

double LabAngle4F(double MT,      /* Masses of the projektil ... */
		  double MP,      /* ... the target (1st one normaly) ... */
		  double MProd,   /* ... the product (of intrest)... */
		  double MRes,    /* ... and the last one */
		  double T1,      /* Energy of target (usualy 0) */
		  double T2,      /* Energy of projektile */ 
		  double Angle)   /* Angle in the Lab */
{

  /* var */

  double AngleCM4 = 0,
         Gamma4CM = 0,
         ALab     = 0;

  /* code */

  k.M1 = U2MeV(MT);
  k.M2 = U2MeV(MP);
  k.M3 = U2MeV(MProd);
  k.M4 = U2MeV(MRes);
  k.T1 = T1;
  k.T2 = T2;

  if (! cal_kin())    /* impossible reaction */
    return 0.0;
  
  if (MaxAngle(MT, MP, MProd, MRes, T1, T2) < Angle)
    return 0.0;

  if (k.Beta3CM > k.Beta) /* there is no backward solution for 3rd! */
    return 0;             /* ergo no fwd. for 4th! */
  else                    /* take the backward solution of 3rd */
    AngleCM4 = PI-AngLab2CM2(Angle);

  Gamma4CM = 1/sqrt(1 - k.Beta4CM*k.Beta4CM);

  ALab = -180/PI*atan( sin(AngleCM4)/
		      (k.Gamma * cos(AngleCM4) + k.Beta/k.Beta4CM));
  if (ALab > 0)
    ALab += -180;

  if (fabs(ALab) > 179.9999 ||
      fabs(ALab) <   0.0001)
    {
      if ((k.Beta < k.Beta4CM) && (fabs(ALab) > 179.0))
	return -180.0;
      else
	return 0.0;
    }
  else
    return ALab;

}
































