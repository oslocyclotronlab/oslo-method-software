/* ******************************************************************** */
/*                                                                      */
/*                           S A S T R A G . C                          */
/*                                                                      */
/*       Containing small angle straggling functions for ELAST.         */
/*                                                                      */
/* ******************************************************************** */

/* DEFINE */

#ifndef mNT       
#  define mNT 10  /* Max. mumber of "ingredients" of target */
#endif

/* INCLUDE */

#include "constants.c"
#include <math.h>
#include "a_tables.h"

/* ***** Prototypes ***** */

double
  asin(),
  atan(),
  exp(),
  log(),
  log10(),
  pow(),
  sin(),
  sqrt();

/* ***** Functions ***** */

  /* lgamma(z)

     This function returns the nat. lograrithm of the gamma-function,
     The algorithm  was "stolen" from the gnuplot sources, and
     is published elsewhere. */

double lgamma(double z)
{

  /* Var & const */

  static double a[] =
  {
         .99999999999980993227684700473478296744476168282198,
      676.52036812188509856700919044401903816411251975244084,
    -1259.13921672240287047156078755282840836424300664868028,
      771.32342877765307884865282588943070775227268469602500,
     -176.61502916214059906584551353999392943274507608117860,
       12.50734327868690481445893685327104972970563021816420,
        -.13857109526572011689554706984971501358032683492780,
         .00000998436957801957085956266828104544089848531228,
         .00000015056327351493115583383579667028994545044040,
  };

    double sum,
           ZPos,
           LG,
           tmp;
    int i;

    /* code */

    if (z <= 0.0)      /* for reflection formula to make the following */
      ZPos = 1.0 - z;     /* loop calculate lgamma_pos_z(1-z) */
    else
      ZPos = z;

    sum = a[0];        /* lgamma_pos_z(ZPos) */
    for (i = 1, tmp = ZPos; i < 9; i++)
      {
	sum += a[i] / tmp;
	tmp++;
      }

    LG = log(sum) +  0.9189385332046727 - ZPos - 
         6.5 + (ZPos - 0.5) * log(ZPos + 6.5);

    if (z <= 0.0)
      {
	/* Use reflection formula; LG was calculated for (1-z) */

	tmp = sin(z * 3.14159265358979323846);

	if (fabs(tmp) < 1.0E-08) /* MachEpsilon */
	  tmp = 0.0;
	else if (tmp < 0.0)
	  tmp = -tmp;

	LG =  1.14472988584940016 - LG - log(tmp);
      }

    return LG;
}


  /* AStrag ****

     This function estimates one sigma of an assumed gaussian 
     angular distribution of the projektilenuclei after passing
     through the target. Units of input as in control.c defined,
     output in degree. 

     This formula was first published by 
     L.MEYER, PHYS.STAT.SOL. 44,253(1971) 

     Obviously, this form of the expression is a fit to a table in
     this paper (g1(Tau)) plus some substitution for a seccond term?! */


double AStrag(int    ZP,   /* see elast.c for meaning of variables */
	      double MP,   /* Names are choosen to be indentical */
	      int    ZT[mNT],
	      double MT[mNT],
	      double CT[mNT],
	      int    NTarEl,
	      double TT,
	      double E )

  /* This function is taken from eneloss, BUT corrected concerning the
     variable A. This variable wasn't summed in eneloss, (evenso
     there is a loop). */

{

  /* var */

  int i = 0;

  double A      = 0.,        /* temp. vars related to angles in stange units */
         A_     = 0.,        /* dito */
         Zz     = 0.,        /* Some simple function of ZT and ZP */
         Tau    = 0.,        /* a kind of effective thickness */
         Exp    = 0.,        /* temp. var */
         Power  = 2.0 / 3.0;

  /* code */

  for( i=0 ; i < NTarEl; i++)
    {
      Zz  = sqrt( pow( ZT[i],Power) + pow(ZP, Power));
      Tau = ( 41470.0 * CT[i] * TT )/( Zz * Zz * MT[i]);
      Exp = pow( log( 1.03+Tau ), -0.115) -0.115;
      A_ = ZT[i] * Zz * pow ( Tau,  Exp) * 0.0022 * ZP /E ; 
      A +=   A_*A_ ;
    };
  return sqrt(A);

}

  /* EStrag **** 

     This function calculates full-with-half-maximum of the
     energy distribution of the projektile nuclei after passing
     through the target. It returns, however, ONE SIGMA of an 
     assumed gaussian distribution (good assumption for reasonably
     small (not only tiny) Eloss/E)

     The formular was published by H. Schmidt-Boecking in Lecture
     Notes in Physics 83 (1978) */

double EStrag(int    ZP,
	      double MP,
	      int    ZT[mNT],
	      double CT[mNT],
	      int    NTarEl,
	      double dEdX1,      /* dE/dX in first layer of target */
	      double dEdX2,      /* dE/dX in last layer of target */
	      double ELoss_ )    /* energy lost in target */

{
  /* var */

  double Power = 1.0/3.0, /* 0.33333... */
         Z     = 0.0,     /* something like an effective Z of collision */
         StSqr = 0.0;     /* Square of energy widening so far */

  int    TEl   = 0;       /* Counter for target elements */
       
  /* code */

  if ((dEdX1 < 1E-10) || (dEdX2 < 1E-10)) /* does it move cross the target? */ 
    return 0;

  for ( TEl=0 ; TEl < NTarEl; TEl++)
    {
      Z = pow(ZT[TEl],Power) + pow(ZP,Power);
      StSqr = StSqr + pow (0.014 * sqrt(ZP * ZT[TEl] /Z) *
	                        pow( ELoss_ , 0.53) * (dEdX2 / dEdX1),
			   2 ) * CT[TEl];
    } ;

  return sqrt(StSqr)   / 1.386294361;
  /*     ^FWHM of peak   ^1/(2*ln(2)) */
}

  /* Qeff **** 

     This function calculates the average charge starte of 
     an ion passing through matter with energy E, mass M
     and nuclear charge Z.

     The formular was published by H. -D. Betz,
     rev. Mod. Phys. 44, 465 (1972) */

double Qeff(int    Z, 
	    double M,  
	    double E )

{
  /* var */
         
  /* code */

  return Z * pow (1.0 +  pow( (double) Z, 0.75) * 0.105
                       * pow( (M/E), 0.83 )
                                            , (-0.6));

}
  /* PopCSn0() **** 

     This function calculates the population of a given
     charge starte CSn of an ion passing with energy E,
     mass M and nuclear charge Z through a (not too) thin carbon foil.
     This simple code was substituted by a more advanced, full
     Robenet-Bobinett code (see below) and is left here only as a
     reference.

     Source: Cheng Lie's SPEAZEASY code

     */

double PopCSn0(int    Z, 
	      double M,  
	      double E,
              int CSn )

{
  /* var */
     
  int    Q     = 0;    /* the atomic charge running from 0 to Z */
  double Sum   = 0,    /* Sum variable for normalisation */ 
         Q0    = 0,    /* the centroid of the CS-distribution */
         Width = 0,    /* the width-parameter of the distribution */
         WP    = 0;    /* a width-related parameter */

  /* code */

  if (Z < CSn)         /* nonsense! charge state > Z! */
    return -1.0;

  Q0 = Z /
           pow( 1 + pow( 0.260 * pow(Z,0.45) / sqrt(E/M),
			 1.66666667),
		0.6);
 
  Width = 0.5 * sqrt( Q0 * ( 1 - pow(Q0/Z, 1.66666667) ));
  WP    = 0.39894228 / Width;
  for (Q=0; Q <= Z; Q++)
    {
      Sum += WP * exp( -0.5 * (Q0-Q)*(Q0-Q) / (Width*Width) );
    };

  return WP * exp( -0.5 * (Q0-CSn)*(Q0-CSn) / (Width*Width) )/Sum;

}
  /* PopCSn() **** 

     This function calculates the population of a given
     charge starte CSn of an ion passing with energy E,
     mass M and nuclear charge Z through a not-too THICK foil,
     i.e. the charge state distribution shall not change
     after equilibrium is reached. It is possible, however,
     that equilibrium is NOT reached, if the foil is sufficiently
     thin.

     An initial charge state can be given. If it is negative, an
     average charge state is assumed.

     The return value of the function is the most abudant charge
     state.

     Source: Original code written by Eliott Canter, an Target-
             corrected Robinet-Bobinett algorithm.

     */

int  PopCSn(int    ZTarg,  /* Target nucl. charge (no compounds, sorry) */
            double TT,     /* taget thickness in mg/cm**2 */
            int    ZP,     /* The projectile nucl. charge */
            double MP,     /* The projectile mass */
            double E,      /* the particles energy */
            double QI,     /* the initial average charge state. */ 
            double CS[]    /* An array with (at least) Z+1 elements */ )

{
  /* var */
     
  int    Q     = 0,     /* the atomic charge running from 0 to Z */
         ZT    = 0,     /* Nucl. charge -6 (carbon) for ZTarg-corr */
         NE    = 0;     /* something like number of electrons? */
  double Sum   = 0,     /* Sum variable for normalisation */ 
         Q0    = 0,     /* the centroid of the CS-distribution */
         Width = 0,     /* the width-parameter of the distribution */
         WP    = 0,     /* a width-related parameter */
         V     = 0,     /* particle velocity */
         E_A   = 0,     /* The projectile energy in MeV/u */
         QInf  = 0,     /* av. chage state after equilibrating in 12C */
         ZRat  = 0,     /* for Z-target correction factor for centroid */
         VP    = 3.6E8, /* don't ask me.... */
         A     = 3.8585,
         T     = 0,
         TMax  = 0, 
         NU    = 0,
         S     = 0,
         C     = 0,
         IBar  = 0,
         ALam  = 0,
         Arg   = 0,
         Core  = 0,
         N     = 0,
         XSect = 0,
         B     = 0.447; /* .... ask Eliott! */

  /* code */

  TT  = TT * 1000;      /* this function calculates INTERNLY in mueg/cm**2 */
  E_A = E/MP;
  V   = 13.892E8 * sqrt(E_A);  
                        /* non-relativistic, but that should not matter,
                           since high energies are naked anyways! */
  /* Z Target correction from Baron & Delaunay PRA 12(1975)40 */
  
  ZT   = ZTarg-6;
  ZRat = 1.0 - (5.21E-3 * ZT) + (9.56E-5 * ZT*ZT) - (5.96E-7 * ZT*ZT*ZT);

  S    = (0.426  - 0.0571*V / (VP * pow(ZP,0.45) )) * pow(ZP,0.4);

  IBar = ZP * ( 1 - 1/exp( A*sqrt(E_A) / pow(ZP,B))) * ZRat;
  if (ZP > 10)
    IBar += 0.2;

  QInf = IBar;
  if (QI < 0.0)         /* set initial charge state to 12C equilibrium */
    QI = QInf;

  NE = (int) (0.5 + ZP - IBar);
  if (NE < 1)
    NE = 1;

  if (NE > 18)           /* some shell correction, why only to Z = 36 ?? */
    N = 3 + NE/36;
  else if (NE > 10)
    N = 2 + NE/18;
  else if (NE > 2)
    N = 1 + NE/10;
  else
    N = 1;

  Sum = 0;
  for (Q = 1; Q <= ZP; Q++)
    Sum += Q;

  Core  = ZP - 2 * Sum * Sum;
  XSect = N*N * 0.529/Core;
  XSect = PI * XSect * XSect;
  ALam = 50 * TT / (10/XSect);

  IBar =IBar*( 1- (IBar-QI) * exp( -ALam)/IBar);

  Sum = 0;  /* the sum over the calculated F's */

  if (V <= 3.6E8 * pow(ZP, 0.45))
    for (Q=0; Q <= ZP; Q++)
      {
	Arg = (Q-IBar)*(Q-IBar)/(2*S*S);
	if (Arg > 40)
	  Arg = 40;
	Sum += CS[Q] = 1/(sqrt(2*PI*S) * exp(Arg));
      }
  else /* high energy treatment */
    {
      C  = 2 * (ZP+2-IBar)/S ;
      NU = C * C / 2 ;
      C /= S;

      TMax = 0;         /* determine TMax */
      for (Q=0; Q <= ZP; Q++)
	if  ( (T=C*(ZP-Q+2)) > TMax)
	  TMax = T;

      for (Q=0; Q <= ZP; Q++)
	{
	  T=C*(ZP-Q+2);
	  if (((NU/2 <= 57.5) && (NU/2 >= 1E-30)) && 
	      (0.5 * NU * abs(log(TMax)) <= 50) &&
	      (TMax <= 85))    
	    CS[Q] = pow(T,NU/2-1) * exp(-T/2) 
	                   / (pow(2,NU/2) * exp(lgamma(NU/2)));
	  
	  else /* large ion treatment */
	    CS[Q] = exp((NU/2-1) * log(T) - T/2 - 
			(NU/2)*log(2) - lgamma(NU/2));
	    
	  CS[Q] *= C;
	  Sum += CS[Q];
	}
    } /* endif high-e/low-e */

  ZT = 0;     /* recycle ZT as most abundant CS */
  A  = 0; /* ... and A as a buffer */
  for ( Q=0; Q <= ZP; Q++)    /* normalize and find most abundant CS */
    {
      if (CS[Q] > A)
	{
	  A = CS[Q];
	  ZT = Q;
	}
      CS[Q]=CS[Q]/Sum;
    };

  return ZT;

}


  /* dEdX **** 

     This function calculates the stopping power dEdX of an
     ion in matter. The calculation uses the J.F.ZIEGLER tables
     and distinguishes between several target types. 

     Unit of output: MeV / (mg cm**2). 

     The author likes to state, that this function was taken form
     ENELOSS with only minor changes in structure and names of
     variables. I didn't understand all that uncommented
     spagehetti code, and hope, there is no major bug in it..

     */

double dEdX(int    ZP,   /* see control.c for meaning of variables */
	    double MP,   /* Names are choosen to be indentical */
	    int    ZT[],
	    double MT[],
	    double CT[],
	    int    NTarEl,
	    int    IsGas,
	    double E )       

{
  /* var */
  
  /* uses  a_tables.h */

  double dEdXN = 0.0 ,     /* Nuclear stopping power */
         dEdXE = 0.0 ,     /* electronic stopping power */
         dEEl  = 0.0 ,     /* stopping power of this element */
         M0    = 0.0,      /* Mass in kg */
         E_    = 0.0 ,     /* Kind of energy */
         EJou  = 0.0 ,     /* E in Joule */
         c2    = C0 * C0 , /* C0 square */
         EPS   = 0.0 ,     /* A kind of Energy combined with charge */
         M     = 0.0 ,     /* Mass of projektile plus the act. tar. element */
         Z     = 0.0 ,     /* A kind of effective Z */
         VSQ   = 0.0 ,     /* approx speed**2 in units of MeV and UNIT at lower v */
         VdivC = 0.0 ,     /* speed in units of c */
         Veff  = 0.0 ,     /* a effective velocity weighted with Z */
         FAC   = 0.0 ,     /* a kind of 1/mass_target */
         B     = 0.0 ,     /* another temporary .... */
         HExp  = 0.0 ,     /* .... variable */
         G     = 0.0 ,     /* Zeff due to remaining electrons of projektile */
         Euler = 2.718281828,  /* the weight of an average elefant in tonns */
         Power = 2.0/3.0;  /* another constant */

  int    TEl     = 0;        /* conter of target elements in main loop */

  /* code */

  EJou  = E * Q_EL * 1.0E6;   /* energy in joule */
  M0 = MP * UNIT;             /* mass in kg */

  VSQ = 1 - ( 1 / ( (EJou*EJou/(M0*M0*c2*c2) + (2*EJou/(M0*c2)) +1) ));
  VdivC = sqrt(VSQ);          /* V in units of C0, CHNAGED from old code */
  VSQ = E / MP;               /* should this be made relativistic, too ??? */

  Veff  = 0.073 * pow (ZP, Power);

  for ( TEl=0 ;  TEl < NTarEl; TEl++ ) 
    {

      FAC = 0.60225 / MT[TEl];

      /* Nuclear stopping, see e.g.: J.F. Ziegler, Handbook of Stopping
         Cross-Sections for energetic ions in all elements (Vol5, Pg. 19) */

      M   = MP + MT[TEl];
      Z   = sqrt( pow ( ZP, Power) + pow ( ZT[TEl], Power) );
      EPS = E * 32520.0 * ( MT[TEl] / M ) /  ( ZP * ZT[TEl] * Z );

      if ( EPS < 0.01 )
	dEEl = 1.593 * sqrt(EPS) ;
      else if ( EPS > 10.0 )
	dEEl = 0.5 * log( 0.47 * EPS) / EPS;
      else
	dEEl = 1.7 * sqrt(EPS) * log( EPS + Euler) / 
	                 ( 1.0 + 6.8 * EPS + 3.4 * pow(EPS , 1.5) );

      dEEl = dEEl * 8.462 * ( ZP * ZT[TEl] / Z ) * ( MP / M ) * FAC;
      dEdXN = dEdXN + (dEEl * CT[TEl]);

      /* Electronic stopping, according to J.F. Ziegler */

      switch (ZP)      /* Handle first three elements diffrent */
	{
	case 1: /* hydrogen */

	  E_ = 1000.0 * VSQ;

	  if (E_ > 1000)
	    dEEl = FAC * H_SGM[ ZT[TEl] ][5] /  (VdivC * VdivC) *
	            log(H_SGM[ ZT[TEl] ][6] *  (VdivC * VdivC) /
			                  (1-(VdivC * VdivC))) 
		    + SC[ ZT[TEl] ] / E_ ;

	  else if (E_ < 10)
	    dEEl = FAC * H_SGM[ ZT[TEl] ][0] * sqrt(E_);
	  else
	      dEEl = FAC /
	                ( 1.0 / (H_SGM[ ZT[TEl] ][1] * pow( E_, 0.45))
			  + E_ / H_SGM[ ZT[TEl] ][2] /
			         log(1.0 + H_SGM[ ZT[TEl] ][3] / E_ + 
			                   H_SGM[ ZT[TEl] ][4] * E_ )  ) ;
	  break;

	case 2: /* helium */

	  E_ = 4.0 * VSQ;
	  
	  if ( E_ < 10 )   /* low energy */
	    if (IsGas)     /* gas target */
	      dEEl = FAC / 
		        ( 1.0 / (pow((1000.0 * E_),He_GM_LE[ ZT[TEl] ][1]) *
				 He_GM_LE[ ZT[TEl] ][0])                
			  + E_ / He_GM_LE[ ZT[TEl] ][2] /
			         log(1.0 + He_GM_LE[ ZT[TEl] ][3] / E_ +
				           He_GM_LE[ ZT[TEl] ][4] * E_ ) );
	    else           /* solid target */
	      dEEl = FAC / 
		        ( 1.0 / (pow((1000.0 * E_),He_SM_LE[ ZT[TEl] ][1]) *
				 He_SM_LE[ ZT[TEl] ][0])                
			  + E_ / He_SM_LE[ ZT[TEl] ][2] /
			         log(1.0 + He_SM_LE[ ZT[TEl] ][3] / E_ +
				           He_SM_LE[ ZT[TEl] ][4] * E_ ) );
	  else             /* high energy */
	    dEEl = FAC * exp (He_GSM_HE[ ZT[TEl] ][0] +
			        (He_GSM_HE[ ZT[TEl] ][1] +
				  (He_GSM_HE[ ZT[TEl] ][2] +
				    He_GSM_HE[ ZT[TEl] ][3] *
				    log (1/E_)
				   ) *  log (1/E_)
				 ) *  log (1/E_) );  
	  break;

	case 3: /* lithium */
	  
	  /* copy from hydrogene, .... */

	  E_ = 1000.0 * VSQ;

	  if (E_ > 1000)
	    dEEl = FAC * H_SGM[ ZT[TEl] ][5] /  (VdivC * VdivC) *
	            log(H_SGM[ ZT[TEl] ][6] *  (VdivC * VdivC) /
			                  (1-(VdivC * VdivC))) 
		    + SC[ ZT[TEl] ] / E_ ;
	  else if (E_ < 10)
	    dEEl = FAC * H_SGM[ ZT[TEl] ][0] * sqrt(E_);
	  else
	      dEEl = FAC /
	                ( 1.0 / (H_SGM[ ZT[TEl] ][1] * pow( E_, 0.45))
			  + E_ / H_SGM[ ZT[TEl] ][2] /
			         log(1.0 + H_SGM[ ZT[TEl] ][3] / E_ + 
			                   H_SGM[ ZT[TEl] ][4] * E_ )  ) ;

	  /* .... BUT: correction of Z */

	  HExp = 0.7138 + 0.002797 * E_ + 1.348E-6 * E_* E_;  /* Shielding */
	  if (HExp < 60)
	    G = 3.0 - 3.0 / exp(HExp);
	  else
	    G = 3;  /* unshielded since fully ionisized */

	  dEEl = dEEl * G * G;  /* dE = Z**2 times dE(Hydrogene) */
	  
	  break;

	default:

	  /* copy from hydrogene, .... */

	  E_ = 1000.0 * VSQ;

	  if (E_ > 1000)
	    dEEl = FAC * H_SGM[ ZT[TEl] ][5] /  (VdivC * VdivC) *
	            log(H_SGM[ ZT[TEl] ][6] *  (VdivC * VdivC) /
			                  (1-(VdivC * VdivC))) 
		    + SC[ ZT[TEl] ] / E_ ;
	  else if (E_ < 10)
	    dEEl = FAC * H_SGM[ ZT[TEl] ][0] * sqrt(E_);
	  else
	      dEEl = FAC /
	                ( 1.0 / (H_SGM[ ZT[TEl] ][1] * pow( E_, 0.45))
			  + E_ / H_SGM[ ZT[TEl] ][2] /
			         log(1.0 + H_SGM[ ZT[TEl] ][3] / E_ + 
			                   H_SGM[ ZT[TEl] ][4] * E_ )  ) ;

	  /* .... BUT: Zieglers extrapolation, correction to Z ! */

	  B = ( 0.1772 * sqrt(E_)) / pow(ZP,Power);
	  G = ZP * (1.0 - (1.034 - 0.1777 / exp(0.08114 * ZP)) /
		    exp(B + 0.0378 * sin(1.5708 * B)));
	  if (G < 1)
	    G = 1;   /* i don't think, this is good, but it makes no dif. */

	  dEEl = G * G * dEEl;  /* dE = Z**2 times dE(Hydrogene) */

	  break;

	} /* switch */

      dEdXE = dEdXE + (dEEl * CT[TEl]);

    }; /* for TEl */
  
  if ((dEdXN + dEdXE) < 1E-30)
    return 1E-30;
  else
    return (dEdXN + dEdXE); /* return total stopping power */

}


  /* Range **** 

     This function calculates the stopping range of 
     an ion passing through matter with initial energy E, mass M
     and nuclear charge Z.

     Integration to 1E-8

     */

double Range(int    ZP,   /* see control.c for meaning of variables */
	     double MP,   /* Names are choosen to be indentical */
	     int    ZT[],
	     double MT[],
	     double CT[],
	     int    NTarEl,
	     int    IsGas,
	     double E )
{
  /* define */

  /* var */
        
  double E1     = 0.0,    /* Variable for the Weddle-formular */
         E2     = 0.0,    /* dito */
         H      = 0.0,    /* dito */
         Rng    = 1E-30;  /* Value approxing the result */

  int     i      = 0,      /* counter */
          N      = 0;      /* Var. for the Weddle-formular */
  
  /* code */
  
  E1 = 1.0E-8;            /* 1E-8 Accuracy */
  
  if (E < E1)             /* Energy to small */
    return 1.0E-30;
  
  N = 8 -1 + (0.5 + (log(E) * 0.4342944819));  /* plus 0.5 to round */ 
                                    /* The "8"  is the accuracy - exponent! */
  
  E2 = E / pow( 10.0 , (double) N );
  
  N++;
  for ( i=1 ; i <= N ; i++)
    {
      H = (E2 - E1) / 6;
      Rng = Rng + H * 
	( 0.3 / dEdX(ZP, MP, ZT, MT, CT, NTarEl, IsGas, E1) +
 	  1.5 / dEdX(ZP, MP, ZT, MT, CT, NTarEl, IsGas, (E1 + H)) +
	  0.3 / dEdX(ZP, MP, ZT, MT, CT, NTarEl, IsGas, (E1 + 2*H)) +
	  1.8 / dEdX(ZP, MP, ZT, MT, CT, NTarEl, IsGas, (E1 + 3*H )) +
	  0.3 / dEdX(ZP, MP, ZT, MT, CT, NTarEl, IsGas, (E2 - 2*H)) +
	  1.5 / dEdX(ZP, MP, ZT, MT, CT, NTarEl, IsGas, (E2 - H)) +
	  0.3 / dEdX(ZP, MP, ZT, MT, CT, NTarEl, IsGas, E2) );
      
      E1 = E2;
      E2 = E2 * 10;
    }
  
  return Rng;
  
}

/* RangeToX **** 
   
   This function calculates the range of 
   an ion passing through matter with initial energy E1, mass M
   and nuclear charge Z, leaving it with E2. This is the answer to
   The question: How much material was inbetween... 
   
   Integration to 1E-8
   
   */

double RangeToX(int    ZP,   /* see control.c for meaning of variables */
	        double MP,   /* Names are choosen to be indentical */
	        int    ZT[],
	        double MT[],
	        double CT[],
	        int    NTarEl,
	        int    IsGas,
	        double E1, 
	        double E2)
     
{
  /* define */

  /* var */
        
  double dEdX_   = 0.0,    /* buffer */
         E       = 0.0,
         dX      = 0.0,    /* exactely this */
         Rng     = 0.0;  /* Value approxing the result */

  /* code */

  if (E1 <= E2)             /* Energy difference negative or zero */
    return 0.0;
 
  E = E1;

  dEdX_ = dEdX(ZP, MP, ZT, MT, CT, NTarEl, IsGas, E1);
  dX = 0.1/dEdX_;
  dEdX_ = dEdX(ZP, MP, ZT, MT, CT, NTarEl, IsGas, E1 - 0.5*(dEdX_ * dX));

  while( (E-E2) >= 0)
    {
      if ((E -= dEdX_ * dX) < 0) 
	return -1.0;                   /* should not happen! p. stopped! */
      
      dEdX_ = dEdX(ZP, MP, ZT, MT, CT, NTarEl, IsGas, E - 0.5*(dEdX_ * dX));
      Rng += dX;
    };

  Rng = Rng - dX*(1 - (dX*dEdX_-(E2-E))/(dX*dEdX_)); /* last fraction */
  return Rng;

}


  /* ELoss **** 

     This function calculates the energy loss  of 
     an ion passing through matter with initial energy E, mass M
     and nuclear charge Z. This is the version form ENELOSS,
     that integrates in a brute-force way, which is actually good 
     for thin targets, bad very slow for really thick targets. In
     this case, one should use the code ELossCL, which has a constant
     performance for all non-stopping targets and is a bit more accurate.

     */

double ELoss(int    ZP,   /* see control.c for meaning of variables */
	     double MP,   /* Names are choosen to be indentical */
	     int    ZT[],
	     double MT[],
	     double CT[],
	     int    NTarEl,
	     int    IsGas,
	     double TT,
	     double E )
{
  /* var */

  double EIn   = 0.0,    /* Energy before traget */
         dEdX_ = 0.0,    /* actual value of dE/dX */
         dX    = 0.0,    /* exactly, what the name implies! */
         X     = 0.0;    /* Position */

  /* code */

  EIn = E;
  dEdX_ = dEdX(ZP, MP, ZT, MT, CT, NTarEl, IsGas, EIn);
  dX = 0.1/dEdX_;

  if((Range(ZP, MP, ZT, MT, CT, NTarEl, IsGas, E) - TT) > 0)
    {
      while((TT-X-dX) >= 0)
	{
	 if ((E = E - (dEdX_ * dX)) < 0)   /* possible a very low energies */
	   return EIn;
	 X = X + dX;
	 dEdX_ = dEdX(ZP, MP, ZT, MT, CT, NTarEl, IsGas, E);
       };
      E = E - (TT-X)*dEdX_;    /* Minimal loss or last "half" step */
      return (EIn-E);
    }
  else
    return EIn;

}

  /* ELossCL **** 

     This function calculates the energy loss  of 
     an ion passing through matter with initial energy E, mass M
     and nuclear charge Z. This is Cheng-Lie's version, that works
     in a binary-search alogorithm way, using the fast Range
     function.

     */

double ELossCL(int    ZP,   /* see control.c for meaning of variables */
	      double MP,    /* Names are choosen to be indentical */
	      int    ZT[],
	      double MT[],
	      double CT[],
	      int    NTarEl,
	      int    IsGas,
	      double TT,
	      double E )
{
  /* var */

  double EIn    = 0.0,    /* Energy before traget */
         ELow   = 0.0,    /* Lower limit in approximation */
         EUp    = 0.0,    /* Upperlimit in approximation */
         TTStep = 0.0,    /* When to interrupt the interpolation, mg/cm**2 */
         R      = 0.0,    /* range at a given point in calc.*/
         DR     = 0.0,    /* Delta-Range */
         FFF1   = 0.681;  /* no big differences between 0.5 and 0.7, the
                             numerical parameter, that controls the
                             attempted step in the interpolation */

  /* code */

  if (TT < 0)
    return 0;

  if (E <= 0)
    return E;

  if ((DR = Range(ZP, MP, ZT, MT, CT, NTarEl, IsGas, E) - TT) <= 0)
    return E;

  EIn     = EUp = E;    /* initial parameters */
  E       = E-E*FFF1;
  TTStep  = TT / 100;  /* accuracy */ 

  R = Range(ZP, MP, ZT, MT, CT, NTarEl, IsGas, E);
  while (fabs(R-DR) >= TTStep)
    {
      R = Range(ZP, MP, ZT, MT, CT, NTarEl, IsGas, E);
      if (R > DR)
	{
	  EUp = E;
	  E = EUp - (EUp - ELow) * FFF1;
	}
      else if (R < DR)
	{
	  ELow = E;
	  E = ELow + (EUp-ELow) * FFF1;
	}
      else   /* ... a rare case... */
	return (EIn-E);
    };
  
  return (EIn -E);
  
}

  /* ELossPeak **** 

     This function calculates the maximum stopping power somewhere in the
     target with thickness TT. Usefull to simmulate a Bragg-Curve
     detector. 

     */

double ELossPeak(int    ZP,   /* see control.c for meaning of variables */
	         double MP,   /* Names are choosen to be indentical */
	         int    ZT[],
	         double MT[],
	         double CT[],
	         int    NTarEl,
	         int    IsGas,
	         double TT,
	         double E )
{
  /* var */

  double EIn   = 0.0,    /* Energy before traget */
         dEdX_ = 0.0,    /* actual value of dE/dX */
         dX    = 0.0,    /* exactly, what the name implies! */
         X     = 0.0,    /* Position */
         dEMax = 0.0;    /* Max. dE/dX */

  /* code */

  EIn = E;
  dEdX_ = dEdX(ZP, MP, ZT, MT, CT, NTarEl, IsGas, EIn);
  dX = 0.1/dEdX_;
  dEMax = dEdX_;


  while((TT-X-dX) >= 0)
    {
      if ((E = E - (dEdX_ * dX)) < 0)   /* possible a very low energies */
	return dEMax;
      X = X + dX;
      dEdX_ = dEdX(ZP, MP, ZT, MT, CT, NTarEl, IsGas, E);
      if (dEdX_ > dEMax)
	dEMax = dEdX_;
    };
  return (dEMax);

}






































