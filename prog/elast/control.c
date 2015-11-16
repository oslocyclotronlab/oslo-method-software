/* ********************************************************************* */
/*                                                                       */
/*                           C O N T R O L . C                           */
/*                                                                       */
/*                    Energy Loss And Straggeling Tool                   */
/*                                                                       */
/* ********************************************************************* */

/* INCLUDE */

  /* ANSI-C standrad library */

#include <stdio.h>
#include <stdlib.h>      /* getenv */
#include <string.h>

  /* Modules of ELAST */

/* include "a_tables.h" */

#include "isotope.c"
#include "sastrag.c"
#include "kinemat.c"

/* DEFINE */

#define TRUE  1
#define FALSE 0

#ifndef EXIT_FAILURE
# define EXIT_FAILURE -1
# define EXIT_SUCCESS 0
#endif

#define mCM 20             /* Max. number of commands */
#define CM_NO_COM -1       /* Tokens .... */
#define CM_ENERGY  1
#define CM_ASTRAG  2
#define CM_ESTRAG  3
#define CM_ELOSS   4
#define CM_RANGE   5
#define CM_DEDX    6
#define CM_CHARGE  7
#define CM_EOUT    8
#define CM_DEDXMAX 9
#define CM_V_C     10
#define CM_CSN     11      /* .. for the commands refering to straggeling.. */

#define CM_ENE_MID  100
#define CM_ENE_HI   1001
#define CM_ENE_LO   1002
#define CM_ASTRAG_P 102     /* .. and Tokens ...*/
#define CM_ESTRAG_P 103
#define CM_ELOSS_P  104
#define CM_CHARGE_P 107
#define CM_CSN3     108
#define CM_EOUT_P   109
#define CM_ECM      120     /* ... for commands for reaction products */ 
#define CM_ANGL_MAX 121
#define CM_ANGLE    122
#define CM_ANGLE4F  1231
#define CM_ANGLE4B  1232
#define CM_CMANG3F  1233
#define CM_CMANG3B  1234
#define CM_CMANG4F  1235
#define CM_CMANG4B  1236
#define CM_E4F      1241
#define CM_E4B      1242
#define CM_CSN4     1245
#define CM_JAKO_F   1251
#define CM_JAKO_B   1252

#define CM_XS_RU_L  200
#define CM_XS_RU_CM 201
#define CM_XS_RU_LJ 202

#define CM_DB_MASS  300
#define CM_DB_Z     301
#define CM_DB_Q     302     /* not jet impl. */
#define CM_DB_SYM   303

#ifndef mNT       
#  define mNT 10  /* Max. mumber of "ingredients" of target */
#endif

/* ***** Function Prototypes ***** */

  /* ScanArgs

     ScanArgs analyses the arguments passed to ELAST. If there is
     a syntactic error, it returns -1, else 0.
     All values are entered in the corresponding variables of main,
     so they can be accessed in the functions called. Nevertheless,
     variables are passed as parameters if nothing else is stated
     in the fuinction declaration. */ 

int ScanArgs(int ArgC, char *ArgS[], /* UNIX-Parameters without first */
	     double *E1,             /* Pointers to the Var's, related */
	     double *E2,             /*   to the Arguments */
	     double *DeltaE,
	     double *Q,
	     double *MP,
	     double MT[],
	     double *MProd,
	     double *MRes,
	     double CT[],
	     double *TT,
	     double *Angle1,
	     double *Angle2,
	     double *DeltaA,
	     int ZT[],
	     int *ZP,
	     int *ZProd,
	     int *ZRes,
	     int *NTarEl,
	     int *IsGas,
	     int *ESteps,
             int *CSn,
             int *SwitchQ,
	     int *Switchq,
	     int *Switchv,
	     int *Switchx,
	     int *SwitchX,
	     int Com[],
	     char *Char_S,
	     char *Char_s );

  /* DispArgs

     Displayes all arguments, that were passed to ELAST and how they
     were intternaly interpreted. */

void DispArgs(int ArgC, char *ArgS[],     /* UNIX-Parameters without first */
	      double E1,                  /* Var's, related */
	      double E2,                  /* to the Arguments */
	      double DeltaE,
	      double Q,
	      double MP,
	      double MT[],
	      double MProd,
	      double MRes,
	      double CT[],
	      double TT,
	      double Angle1,
	      double Angle2,
	      double DeltaA,
	      int ZT[],
	      int ZP,
	      int ZProd,
	      int ZRes,
	      int NTarEl,
	      int IsGas,
	      int Esteps,
	      int SwitchQ,
	      int Switchq,
	      int Switchv,
	      int Switchx,
	      int SwitchX,
	      int Com[],
      	      char Char_S,
	      char Char_s );

  /* ExecuteCom

     Executes the commandstring, that is passed to ELAST. */

void ExecuteComArgs(double E1,              
		    double E2,   
		    double DeltaE,
		    double Q,
		    double MP,
		    double MT[],
		    double MProd,
		    double MRes,
		    double CT[],
		    double TT,
		    double Angle1,
		    double Angle2,
		    double DeltaA,
		    int ZT[],
		    int ZP,
		    int ZProd,
		    int ZRes,
		    int NTarEl,
		    int IsGas,
		    int ESteps,
                    int CSn,
		    int SwitchQ,
		    int Switchq,
		    int Com[],
		    char Char_S,
		    char Char_s );

  /* ErrPrint

     Prints an errormessage to the the device given by ErrFile  */

void ErrPrint(char ErrText[]);


/* ***** main ***** */

int main(int  ArgC,       /* Number of arguments + 1 (Name of exected file) */
	 char *ArgS[] )   /* All the arguments (+ Name of ex. file in [0]   */

{

  /* *** var of main *** */

  int Err = 0;  /* Last Errorcode */

  double
    E1      = .0 ,    /* Energy of projektile low energy border) */
    E2      = .0 ,    /* Energy of projektile high energy border */
    DeltaE  = .0 ,    /* A energy step  of E1 to E2, may be negative! */
    Q       = .0 ,    /* Q-value of reaction, if a reaction is given. */
    MP      = .0 ,    /* Mass of projektile */
    MT[mNT] = { .0 }, /* Mass of all target "ingeredients" */
    MProd   = .0,     /* Mass of reaction product, if present */
    MRes    = .0,     /* Mass of the seccond ejektil */
    CT[mNT] = { .0 }, /* Concentration of target "ingredients" in percent */
    TT      = 0.0,    /* Total thickness of target in mg/cm**2 */
    Angle1  = 0.0,    /* 1st lab. Angle to calculate kinematics with. */
    Angle2  = 0.0,    /* Last Lab Angle to calculate kinematics for */
    DeltaA  = 1.0;    /* Step to go from Angle1 to Angle2 */

  int
    ZT[mNT] = { 0 }, /* charge of the different target components */
    ZP      = 0,     /* Nuclear charge of projektile */
    ZProd   = 0,     /* Nuclear charge of reaction product, if present */
    ZRes    = 0,     /* Charge of the seccond ejektil */ 
    NTarEl  = 1,     /* Number of elements in compound target */
    IsGas   = FALSE, /* Flag, set from the command line Switch -g */
    CSn     = -1,    /* Charge State of the ion, if -1, use default Z */
    SwitchQ = FALSE, /* Switch -Q from command line */
    Switchq = FALSE, /* Switch -q from command line */
    Switchv = FALSE, /* Be-verbous-switch (redisplay enterd values...) */
    Switchx = FALSE, /* an excitation energy of the beam particle was given */
    SwitchX = FALSE, /* an excitation energy of the product was given */
    ESteps  = 1,     /* Argument numbers of steps */

    Com[mCM+1] = {0};     /* Array of tokenized commands in order, 0 = end */
                          /* last one (the +1) for the 0 in case of full CL */
  char 
    Char_S      = '\n',
    Char_s      = ' ';

  /* *** code  of main *** */

  Err=ScanArgs(ArgC-1, &ArgS[1],   /* Pass all but first */
	       &E1, &E2, &DeltaE, &Q,
	       &MP, MT, &MProd, &MRes, CT, &TT, &Angle1, &Angle2, &DeltaA,
	       ZT, &ZP, &ZProd, &ZRes,
	       &NTarEl,
	       &IsGas, &ESteps,
               &CSn,
               &SwitchQ, &Switchq, &Switchv, &Switchx, &SwitchX,
	       Com,
	       &Char_S, &Char_s);

  /* Error in parameter? -> Output some general help and stop ELAST */

  if (Err <= 0 )  
    { fprintf (stderr, "
USAGE:     elast [-switches] commands {targets | DC} tar_thickness
                 projectile [reakt_product Q_value] E1 [E2 N_E-steps]

UNITS:    Energy: MeV       Angles: degree           Thickness: mg/cm**2,
          Charge: e           Mass: unit         cross-section: mb 

SWITCHES : -q --> quiet            -Q --> very quiet, numbers only
           -g --> gasous target,   -v --> verbous 
           -x,-X <Ex, Particle 2,3)   --> excited product (-X) or beam (-x)
           -a Ang1 [Ang2 AngStep]     --> Angle (Lab or CM) [loop over]
           -s <OutFiledSepChar> -S <OutLineSepChar>   --> seperator chars 
           -c <charge state>          --> Charge state used (instead of Z)  
COMMANDS:  {  A,E,a,e,l,r,p,c,s,v,o,P                echo / energy loss comm's
              D{m,z,s,q}                             data commands
              R{E+|E-|E0|J+|J-|t+|t-|a|e|l|c|s|C|T}, reac. comm's, 3rd particle
(see          R4{A+|A-|E+|E-|t+|t-|s},               reac. comm's, 4th particle
 README.TXT)  X{r}                                   class. Rutherford XS & Co

EX.: elast -q Eloae \"1(C)2(H)\" 1. 17F 60 70 10
      elast -a 0 22 2 A.Rt+.RJ+.RE+.R4A-.R4E-.RJ-.RE- \"1(1H)\" .0 17F 4HE d 60
");

      if (Switchv)
	DispArgs(ArgC-1, &ArgS[1],   /* Pass all but first */
		 E1, E2, DeltaE, Q,
		 MP, MT, MProd, MRes, CT, TT, Angle1, Angle2, DeltaA,
		 ZT, ZP, ZProd, ZRes,
		 NTarEl,
		 IsGas, ESteps,
		 SwitchQ, Switchq, Switchv, Switchx, SwitchX,
		 Com,
		 Char_S, Char_s);     

      return (EXIT_FAILURE); };       /* errorcode for bad parameter */
   

  /* Output general information about input parameters and their 
     interpretation by ELAST input scanner, if userer asked for it */

  if (Switchv)
    DispArgs(ArgC-1, &ArgS[1],   /* Pass all but first */
	     E1, E2, DeltaE, Q,
	     MP, MT, MProd, MRes, CT, TT, Angle1, Angle2, DeltaA,
	     ZT, ZP, ZProd, ZRes,
	     NTarEl,
	     IsGas, ESteps,
	     SwitchQ, Switchq, Switchv,Switchx,SwitchX,
	     Com,
	     Char_S, Char_s);

  ExecuteComArgs(E1, E2, DeltaE, Q, MP, MT, MProd, MRes,
		 CT, TT, Angle1, Angle2, DeltaA, ZT, ZP, ZProd, ZRes, NTarEl,
		 IsGas, ESteps, CSn, SwitchQ, Switchq, Com,
		 Char_S, Char_s );

  return (EXIT_SUCCESS);

}

/* ***** Functions ***** */

/* Top find a short description of the functions (if there is any), see
   the prototype declaration on top of this file. Find definitions here,
   if the functions are only for use in another function and therefor
   *not* prototyped on top of the file. These functions appear directly
   in front of the function, that makes use of them. */


  /* ExecuteCom ****

     Executes the commandstring, that is passed to ELAST. */

void ExecuteComArgs(double E1,              
		    double E2,   
		    double DeltaE,
		    double Q,
		    double MP,
		    double MT[mNT],
		    double MProd,
		    double MRes,
		    double CT[mNT],
		    double TT,
		    double Angle1,
		    double Angle2,
		    double DeltaA,
		    int ZT[mNT],
		    int ZP,
		    int ZProd,
		    int ZRes,
		    int NTarEl,
		    int IsGas,
		    int ESteps,
                    int CSn,
		    int SwitchQ,
		    int Switchq,
		    int Com[],
		    char Char_S,
		    char Char_s )

{
  /* var */

  int    Pos  = 0,         /* Position in command line */  
         i    = 0;         /* Temp counter */

  double E      = 0.0,       /* Energy as the running variable */
         Angle  = 0.0,
         CS[MAXEL],          /* Buffer for the charge state distributuion */
         ELoss_ = 0.0,       /* temp. buffer for ELoss() in other comm's */
         EMid_  = 0.0,       /* temo. buffer for MidEnergy in other comm's */
         R      = 0.0,       /* temp. result of las calculation */    
         TT0    = 0.0,       /* the target thickness */
         Dummy;              /* MIV  (most important variable) */
  
  /* code */

  Angle = Angle1;
  TT0 = TT;

  if ((! Switchq) && (! SwitchQ) )   /* Introduce yourself */
    {
      printf("\n                   **** ELAST ****\n\n");
      printf("This programm is free software an may be freely distributed\n");
      printf("for no-commercial purposes .\n\n (C) 1996-98 by Boris Harss, ANL\n\n");
      if (MProd > 0)
	if (ZT[0] > 0 && MT[0] > 0)
	  printf("Reaction: %i%s(%i%s,%i%s)%i%s    Q = %f\n\n",
		 (int) (MT[0] + 0.5), N_El[ ZT[0] ],
		 (int) (MP + 0.5), N_El[ZP],
		 (int) (MProd + 0.5), N_El[ZProd],
		 (int) (MRes + 0.5), N_El[ZRes],
		 Q);
	else
	  printf("Decay: %i%s --> %i%s + %i%s    Q = %f\n\n",
		 (int) (MP + 0.5), N_El[ZP],
		 (int) (MProd + 0.5), N_El[ZProd],
		 (int) (MRes + 0.5), N_El[ZRes],
		 Q);
	 
    };
  while ((Com[Pos] != CM_NO_COM) && (! SwitchQ))    /* Headline for Table  */
    {
      switch ( Com[Pos] )    
	{
	case CM_ENERGY :           /* it's the Energy output command */
	  printf("Energy    ");
	  break;
	case CM_ANGLE :            /* it's the Angle output command */
	  printf("Angle     ");
	  break;
	case CM_ASTRAG :           /* it's the Angular straggling command */ 
	  printf("An.-Strag ");
	  break;
	case CM_ESTRAG :           /* it's the Energy straggling command */ 
	  printf("En.-Strag ");
	  break;
	case CM_ELOSS :            /* it's the energy Loss command */
	  printf("En.-loss  ");
	  break;
	case CM_RANGE :            /* it's the stopping Range command */ 
	  printf("Stop Rng. ");
	  break;
	case CM_DEDX :             /* it's the power deposit (dE/dx) command */
	  printf("dE/dx     ");
	  break;
	case CM_DEDXMAX :          /* it's the max. dE/dx command */
	  printf("dE/dx MAX ");
	  break;
	case CM_CHARGE :           /* it's the av. charge state command */
	  printf("C.-State  ");
	  break;
	case CM_CHARGE_P :         /* it's the prod. av. charge st. command */
	  printf("Prod C.S. ");
	  break;
        case CM_CSN :              /* it's the population of CSn of the beam */
          if (CSn < 0)
	    printf("  CS2:%i   ",ZP);
          else
	    {
	      if (ZP >= CSn)
		printf("  CS2:%i   ",CSn);
	      else
		{
		  ErrPrint("Chrg. state given by -c-switch greater than Z(2)");
		  return;
		}
	    }
	  break;
        case CM_CSN3 :             /* it's the population of CSn of product */
          if (CSn < 0)
	    printf("  CS3:%i   ",ZProd);
          else
	    {
	      if (ZProd >= CSn)
		printf("  CS3:%i   ",CSn);
	      else
		{
		  ErrPrint("Chrg. state given by -c-switch greater than Z(3)");
		  return;
		}
	    }
	  break;
        case CM_CSN4 :             /* it's the population of CSn of resedue */
          if (CSn < 0)
	    printf("  CS4:%i   ",ZRes);
          else
	    {
	      if (ZRes >= CSn)
		printf("  CS4:%i   ",CSn);
	      else
		{
		  ErrPrint("Chrg. state given by -c-switch greater than Z(3)");
		  return;
		}
	    }
	  break;
	case CM_ECM :              /* it's center of mass energy - command */
	  printf("E(c.o.m.) ");
	  break;
	case CM_V_C :              /* it's the c/v - command */
	  printf(" v / c0   ");
	  break;
	case CM_EOUT :             /* it's the energy-after-target command */
	  printf("E_ProjOut ");
	  break;	
	case CM_ANGL_MAX :         /* it's the max. reaktion angle command */
	  printf("max.angle ");
	  break;
	case  CM_EOUT_P :          /* it's the middle product out energy */
	  printf("Ene.3_out ");
	  break;
	case  CM_ENE_HI :          /* it's the max. product energy (kine) */
	  printf("Energy3_+ ");
	  break;
	case  CM_ENE_LO :          /* it's the min. product energy (kine)*/
	  printf("Energy3_- ");
	  break;
	case  CM_ENE_MID :          /* it's the middle product energy (kine) */
	  printf("Energy3_0 ");
	  break;
	case CM_ELOSS_P  :         /* it's the middle product energy loss */
	  printf("E_LossPro ");
	  break;
	case CM_ANGLE4F  :         /* it's the 4th particle forward angle  */
	  printf("Angle4_+  ");
	  break;
	case CM_ANGLE4B  :         /* it's the 4th particle backward angle */
	  printf("Angle4_-  ");
	  break;
	case CM_CMANG3F  :         /* it's the 3rd particle fwd. CM angle */
	  printf("CMAng3_+  ");
	  break;
	case CM_CMANG3B  :         /* it's the 3th particle bwd. CM angle */
	  printf("CMAng3_-  ");
	  break;
	case CM_CMANG4F  :         /* it's the 4th particle fwd. CM angle */
	  printf("CMAng4_+  ");
	  break;
	case CM_CMANG4B  :         /* it's the 4th particle bwd. CM  angle */
	  printf("CMAng4_-  ");
	  break;
	case CM_JAKO_F :           /* it's the Jakobinan forward command */
	  printf("Jakobian+ ");
	  break;
	case CM_JAKO_B :           /* it's the Jakobinan backward command */
	  printf("Jakobian- ");
	  break;
	case CM_E4F  :             /* it's the 4th particle forward energy */
	  printf("Energy4_+ ");
	  break;
	case CM_E4B  :             /* it's the 4th particle backward energy */
	  printf("Energy4_- ");
	  break;
 	case CM_XS_RU_CM  :        /* it's the Rutherford-XS-in CM command! */
	  printf("XS_Rut_CM ");
	  break;
	case CM_DB_MASS  :         /* it's the database- mass-of projectile */
	  printf("MASS      ");
	  break;
	case CM_DB_Z  :            /* it's the database- Z of projectile */
	  printf("Nucl_Ch_Z ");
	  break;
	case CM_DB_Q  :            /* it's the database- Q of reaction */
	  printf("Q-Value   ");
	  break;
	case CM_DB_SYM  :          /* it's the Atomic- Symbol of projectile */
	  printf("AtomicSym ");
	  break;


       default :       	  
	  printf("NotImplm! ");
	  break;
	
	};

      Pos++;           /* next command */

    };

  if (! SwitchQ)
    printf("\n");  /* end headline... */

  for ( Angle = Angle1;
	DeltaA/fabs(DeltaA) * Angle <=     /* make it work up or down! */ 
	  DeltaA/fabs(DeltaA) * Angle2 ;
	Angle += DeltaA)
   for ( i=0, E=E1, TT= TT0/( 1.0E-33 + fabs(cos( Angle*PI/180 )) ) ;
	 i < ESteps ;
	 i++)
    {
      
      Pos=0;
      
      while (Com[Pos] != CM_NO_COM)                        /* Print Table  */
	{
	  switch ( Com[Pos] )    
	    {
	    case CM_ENERGY :         /* it's the Energy output command */
	      printf("%9g%c",E,Char_s);
	      break;
	    case CM_ANGLE :         /* it's the Angle output command */
	      printf("%9g%c",Angle,Char_s);
	      break;
	    case CM_ASTRAG :         /* it's the Angular straggling command */ 
	      R =  AStrag(ZP, MP, ZT, MT, CT, NTarEl, TT, E);
	      printf("%9g%c",R,Char_s);
	      break;
	    case CM_DB_MASS :         /* it's the database-mass-command */
	      R = IsoMass(ZP, (int)MP);
	      printf("%9g%c",R,Char_s);
	      break;
	    case CM_DB_Z :            /* it's the database-Z-command */
	      printf("%9g%c",(double)ZP,Char_s);
	      break;
	    case CM_DB_SYM :          /* it's the Atomic Symbol-command */
	      printf(" %s      %c",N_El[ZP],Char_s);
	      break;
	    case CM_DB_Q :            /* it's the database-Q-value-command */
	      printf("%9g%c",U2MeV(MP+MT[0]-MProd-MRes),Char_s);
	      break;
	    case CM_ESTRAG :          /* it's the Energy straggling command */ 
	      ELoss_ = ELoss(ZP, MP, ZT, MT, CT, NTarEl, IsGas, TT, E);
	      if ((E-ELoss_) > 0.01)
		R=EStrag(ZP, MP, ZT, CT, NTarEl,
			 dEdX(ZP, MP, ZT, MT, CT, NTarEl, IsGas, E ),
			 dEdX(ZP, MP, ZT, MT, CT, NTarEl, IsGas, (E-ELoss_)),
			 ELoss_ );
	      else
		R=0;
	      printf("%9g%c",R,Char_s);
	      break;
	    case CM_ELOSS :          /* it's the energy Loss command */
	      R=ELoss(ZP, MP, ZT, MT, CT, NTarEl, IsGas,TT, E);
	      printf("%9g%c",R,Char_s);
	      break;
	    case CM_RANGE :          /* it's the stopping Range command */ 
	      R=Range(ZP, MP, ZT, MT, CT, NTarEl, IsGas, E);
	      printf("%9g%c",R,Char_s);
	      break;
	    case CM_DEDX :           /* it's the power (dE/dx) command */
	      R = dEdX(ZP, MP, ZT, MT, CT, NTarEl, IsGas, E );
      	      printf("%9g%c",R,Char_s);
	      break;
	    case CM_DEDXMAX :        /* it's the max. dE/dX  command */
	      R=ELossPeak(ZP, MP, ZT, MT, CT, NTarEl, IsGas,TT, E);
	      printf("%9g%c",R,Char_s);
	      break;
	    case CM_CHARGE :         /* it's the charge state command */
	      ELoss_ = ELoss(ZP, MP, ZT, MT, CT, NTarEl, IsGas, TT, E);
	      R=Qeff(ZP, MP, E-ELoss_);
      	      printf("%9g%c",R,Char_s);
	      break; 
	    case CM_CSN :           /* it's the pop-charge state (proj) */
	      PopCSn(ZT[0],TT,ZP, MP, E,-1, CS);
	      if( CSn < 0)
		printf("%9g%c",CS[ZP],Char_s);
	      else
		printf("%9g%c",CS[CSn],Char_s);
	      break; 
	    case CM_CSN3 :          /* it's the pop-charge state (proj) */
	      R=MaxEnergy(MT[0], MP, MProd, MRes, 0, E, Angle);
	      if (R > 0)
		{
		  PopCSn(ZT[0],TT,ZProd, MProd, R,-1, CS);
		  if( CSn < 0)
		    printf("%9g%c",CS[ZProd],Char_s);
		  else
		    printf("%9g%c",CS[CSn],Char_s);
		}
	      else
		printf("%9g%c",0.0,Char_s);
	      break; 
	    case CM_CSN4 :          /* it's the pop-charge state (resed.) */
	      R=MaxEnergy(MT[0], MP, MProd, MRes, 0, E, Angle);
	      if (R > 0)
		{
		  PopCSn(ZT[0],TT,ZRes, MRes, E-R+Q,-1, CS);
		  if( CSn < 0)
		    printf("%9g%c",CS[ZRes],Char_s);
		  else
		    printf("%9g%c",CS[CSn],Char_s);
		}
	      else
		printf("%9g%c",0.0,Char_s);
	      break; 
	    case CM_CHARGE_P :       /* it's the charge state command */
	      EMid_  = MidEnergy(MT[0], MP, MProd, MRes, 0, E, Angle);
	      ELoss_ = ELoss(ZProd, MProd, ZT, MT, CT, NTarEl,
			     IsGas, TT, EMid_);
	      R=Qeff(ZP, MP, EMid_ - ELoss_);
      	      printf("%9g%c",R,Char_s);
	      break; 
	    case CM_ECM :            /* it's the energy in c.o.m. command */
	      R=ECoM(MT[0], MP , MProd, MRes, 0, E);
      	      printf("%9g%c",R,Char_s);
	      break; 
	    case CM_V_C :            /* it's the v/c -  command */
	      R=V_C(MP, E);
      	      printf("%9g%c",R,Char_s);
	      break; 
	    case CM_EOUT :           /* it's the energy-after-target command */
	      R=E-ELoss(ZP, MP, ZT, MT, CT, NTarEl, IsGas, TT, E);
	      if (R < 0.0)           /* should not happen, but happened.. */
		R = 0;
      	      printf("%9g%c",R,Char_s);
	      break; 
	    case CM_ANGL_MAX :       /* it's the max. reaction angle-command */
	      R=MaxAngle(MT[0], MP, MProd, MRes, 0, E);
      	      printf("%9g%c",R,Char_s);
	      break;
	    case CM_ANGLE4F :       /* it's the 4th particle forward
                                       angle-command */
	      R=LabAngle4F(MT[0], MP, MProd, MRes, 0, E, Angle);
      	      printf("%9g%c",R,Char_s);
	      break;
	    case CM_ANGLE4B :       /* it's the 4th particle backward
                                       angle-command */
	      R=LabAngle4B(MT[0], MP, MProd, MRes, 0, E, Angle);
      	      printf("%9g%c",R,Char_s);
	      break;
	    case CM_CMANG3F :       /* it's the 3rd particle forward
                                       CM angle-command */
	      if (CMAngle(MT[0], MP, MProd, MRes, 0, E, Angle,&R,&Dummy) == 0)
		printf("%9g%c",0.0,Char_s);
	      else
		printf("%9g%c",R,Char_s);		
	      break;
	    case CM_CMANG3B :       /* it's the 3rd particle backward
                                       CM angle-command */
	      if (CMAngle(MT[0], MP, MProd, MRes, 0, E, Angle,&Dummy,&R) < 2)
		printf("%9g%c",0.0,Char_s);
	      else
		printf("%9g%c",R,Char_s);		
	      break;
	    case CM_CMANG4F :       /* it's the 4th particle forward
                                       CM angle-command */
	      if (CMAngle(MT[0], MP, MProd, MRes, 0, E, Angle,&Dummy,&R) < 2)
		printf("%9g%c",0.0,Char_s);
	      else
		printf("%9g%c",-0.00001*(int)((180.0-R)*100000),Char_s); 
	      break;
	    case CM_CMANG4B :       /* it's the 4th particle backward
                                       CM angle-command */
	      if (CMAngle(MT[0], MP, MProd, MRes, 0, E, Angle,&R,&Dummy) == 0)
		printf("%9g%c",0.0,Char_s);
	      else
		printf("%9g%c", -0.00001*(int)((180.0-R)*100000) ,Char_s); 
	      break;
	    case CM_JAKO_F :       /* it's the Jakobinan forward command */
	      R=JakoFWD(MT[0], MP, MProd, MRes, 0, E, Angle);
      	      printf("%9g%c",R,Char_s);
	      break;
	    case CM_JAKO_B :       /* it's the Jakobinan forward command */
	      R=JakoBWD(MT[0], MP, MProd, MRes, 0, E, Angle);
      	      printf("%9g%c",R,Char_s);
	      break;
	    case CM_ENE_HI :       /* it's the max. reaction energy-command */
	      R=MaxEnergy(MT[0], MP, MProd, MRes, 0, E, Angle);
      	      printf("%9g%c",R,Char_s);
	      break;
	    case CM_E4B :          /* it's the max. reaction 4Energy-command */
	      R=MaxEnergy(MT[0], MP, MProd, MRes, 0, E, Angle);
	      if (R > 0 )
		printf("%9g%c",E+Q-R,Char_s);
	      else
		printf("%9g%c",0.0,Char_s);
	      break;
	    case CM_E4F :          /* it's the max. reaction 4Energy-command */
	      R=MinEnergy(MT[0], MP, MProd, MRes, 0, E, Angle);
	      if (R > 0 )
		printf("%9g%c",E+Q-R,Char_s);
	      else
		printf("%9g%c",0.0,Char_s);
	      break;
	    case CM_ENE_LO :       /* it's the min. reaction energy-command */
	      R=MinEnergy(MT[0], MP, MProd, MRes, 0, E, Angle);
      	      printf("%9g%c",R,Char_s);
	      break;
	    case CM_ENE_MID :      /* it's the Mid. reaction energy-command */
	      R=MidEnergy(MT[0], MP, MProd, MRes, 0, E, Angle);
      	      printf("%9g%c",R,Char_s);
	      break;
	    case CM_EOUT_P :         /* it's the middle product energy */ 
	      EMid_ = MidEnergy(MT[0], MP, MProd, MRes, 0, E, Angle);
	      ELoss_ = ELoss(ZProd, MProd, ZT, MT, CT, NTarEl,
			     IsGas, TT, EMid_);
	      R = EMid_ - ELoss_;
      	      printf("%9g%c",R,Char_s);
	      break; 
	    case  CM_ELOSS_P :        /* it's the energy loss product energy */
              EMid_ = MidEnergy(MT[0], MP, MProd, MRes, 0, E, Angle);
	      if (EMid_ >  0)                 /* is there a reaction ? */
		R = E - EMid_ + ELoss (ZProd,MProd, ZT, MT, CT, NTarEl,
				       IsGas, TT, EMid_ );
	      else
		R = 0;                        /* no -> zero! */
      	      printf("%9g%c",R,Char_s);
	      break; 
	    case CM_XS_RU_CM  :     /* it's the Rutherford-XS-in CM command! */
	      if (Angle == 0.0)
		printf("%9g%c",-1.0,Char_s);
	      else
		{ EMid_ = E*MT[0]/(MT[0]+MP); /* CM Energy class. */
		  R = 1.296 * (ZP*ZP*ZT[0]*ZT[0]/(EMid_*EMid_)) 
		            / pow(sin(Angle * PI / 180.0 / 2.0),4);
		  printf("%9g%c",R,Char_s);
		};
	      break;
				 
	    default:
	      printf(" NotImpl. ");
	    };
	  
	  Pos++;           /* next command */
	  
	};  /* while */
      
      printf("%c",Char_S);
      E = E + DeltaE;

    };   /* for i, the energy loop and Angle loop arround it */
  
  return;
  
}
   /*  GetZfromStr (dependent function of ScanTarget, ...) ****

       GetZfromStr converts the first one or two letters of a string
       to a nuclear charge number, if they are a ISO symbol. The value
       is entered in the integer, to that the parameter Z points.
       The number of chars fot the symbol is returned (1 or 2).

       If conversion is not possible, an error message is displayed and
       the value -1 is returned.

       */

int GetZfromStr(int  *Z,
		char *Symbol)
     
{

  /* var */

  int ZCnt    = 1,       /* conter for the Z-number of Symbol */
      NumChar = 1;       /* number of chars in symbol */

  /* code */

  if ((Symbol[1] >= 'A') && (Symbol[1] <= 'Z'))
    NumChar = 2;

  while ( ZCnt < N_El_MAX )
    {
      switch (NumChar)
	{
	case 1:
	  if ((Symbol[0] == N_El[ZCnt][0]) && 
	                  ((N_El[ZCnt][1] == 0) || (N_El[ZCnt][1] == ' ' )) )
	    { *Z = ZCnt;
	      return 1; }
	  break;
	case 2:
	  if ((Symbol[0] == N_El[ZCnt][0]) && (Symbol[1] == N_El[ZCnt][1]) )
	    { *Z = ZCnt;
	      return 2; }
	  break;
	}

      ZCnt ++;

    } /* while i points to a valid symbol */

  return -1;      /* no matching symbol was found! */

}

   /* ScanTarget (dependent function of ScanArgs) ****

       ScanTarget evaluates the Argument TARGET, which may consist of
       several compound elements. It returns the number of compounds
       as function value or -1 for an syntax error. It also displays
       an appropriate error-message, if possible.
       The values for target masses are entered in MT and the thickness
       of each compound is calculated and entered in CT.

       The function sets the flag *MixMass to TRUE, if the returned
       mass of the first t.- element is taken from the tabele of
       natural isotope mixing masses.

       WARNING: Works only with A-Z and 0-9 ordered charsets like
                ASCII and ANSI */

int ScanTarget(char TArg[],
	       char Thick[],
	       double MT[],
	       double CT[],
	       double *TT,
	       int ZT[],
	       int *MixMass1)

{
  /* var */

  int    NTarEl   = 0,           /* Computed in this function and returned */
         Pos      = 0,           /* Position pointer in TArg[] */
         done     = FALSE,       /* Flag */
         i        = 0,           /* Temp */
         SymLet   = 0;           /* number of letters od the act. Nuc. Symb. */

  double Norm     = 0;           /* Normation constant */
  /* code */

  if (! sscanf(Thick, "%lf", TT) )      /* Get value of total thickness */
    { ErrPrint("ERROR:Unable to covert thickness argument!");
      return -1; };

  while (TArg[i] != 0)                  /* make letters capital */
    { if ((TArg[i] >= 'a' ) && (TArg[i] <= 'z' ))
	TArg[i] = TArg[i] + 'A' - 'a';
      i++ ; };

  while ( (! done) && ( NTarEl <= mNT ) ) /* Scan String for compounds */
    {
      while ( TArg[Pos] == ' ' )          /* skip leading Blanks */
	  Pos++;

      if ((TArg[Pos]   == 'D') &&      /* DECAY ??? */
	  (TArg[Pos+1] == 'C') && 
	  (NTarEl == 0) )
	{
	  NTarEl = 1;
	  *MixMass1 = FALSE;
	  CT[NTarEl] = 0;
	  MT[NTarEl] = 0;
	  ZT[NTarEl] = 0;
	  
	  return NTarEl;
	  
	};

      if (TArg[Pos] == 0)                 /* String end */
	  if (NTarEl > 0)
	    done = TRUE;
	  else
	    { ErrPrint("ERROR: Target parameter seems to be empty!");
	      return -1; }
      else
	{      
	  if ( (TArg[Pos] < '0') || (TArg[Pos] > '9'))       /* CS ordered! */
	    {                                      /* is 1st. chr a number? */
	      ErrPrint("ERROR: target definition: leading number expected");
	      return -1; };
	  
	  while ((TArg[Pos] >= '0') && (TArg[Pos] <= '9'))     /* Get int CT */
	    { CT[NTarEl] = CT[NTarEl] * 10  + (double) (TArg[Pos]  - '0') ;
	      Pos ++; };
	  
	  while ( TArg[Pos] == ' ' )           /* skip Blanks */
	    Pos++;
	  
	  if ( TArg[Pos] != '(')               /* There MUST be a bracket */
	    { ErrPrint("ERROR: target definition: '(' expected");
	      return -1; };
	  
	  Pos ++;                             /* Skip bracket */
	  
	  while ( TArg[Pos] == ' ' )           /* skip Blanks */
	    Pos++;
	  
	  if ( ( TArg[Pos] >= 'A' ) && ( TArg[Pos] <= 'Z' ))  /* only el. */
	    
	    if ((SymLet = GetZfromStr(&ZT[NTarEl], &TArg[Pos])) == -1)
	      return -1;                        
	    else                /* Value is already entered by GetZfromStr! */
	      { MT[NTarEl] = M_El[ ZT[NTarEl] ];     /* get mass from table */
		if (NTarEl == 0)
		  *MixMass1 = TRUE;                  /* Flag for mixed mass */
		Pos = Pos + SymLet;

		while ( TArg[Pos] == ' ' )           /* skip Blanks */
		  Pos++;

		if ( TArg[Pos] != ')')              /* There MUST be a comma */
		  { ErrPrint("ERROR: target definition: ')' expected");
		    return -1; };
		
		Pos++;                              /* skip last bracket ')' */
	      }    

	  else if  ( ( TArg[Pos] >= '0' ) && ( TArg[Pos] <= '9' ) )
	    {
	      /* Convention with two figures or symbol with mass is expected */
	      /* read now first number, assuming it is the charge  */
	      
	      while ((TArg[Pos] >= '0') && (TArg[Pos] <= '9'))   /* int ZT[] */
		{ ZT[NTarEl] = ZT[NTarEl]*10 + (int) (TArg[Pos] - '0');
		  Pos ++; };
	      
	      while ( TArg[Pos] == ' ' )           /* skip Blanks */
		Pos++;    
	      
	      if ( (TArg[Pos] >= 'A') &&  (TArg[Pos] <= 'Z')) /* Symbol */
		{
		  MT[NTarEl] = (double) ZT[NTarEl]; /* 1st n. was M, not Z ! */
		  if ((SymLet = GetZfromStr(&ZT[NTarEl], &TArg[Pos])) == -1)
		    return -1;                        
		  else   
		    Pos = Pos + SymLet; 
		}

	      else if( TArg[Pos] != ',')          /* There MUST be a comma */
		{ ErrPrint("ERROR: target definition: ',' expected");
		  return -1; }
	      else
		{
		  Pos++;                               /* Skip comma */
		  
		  while ( TArg[Pos] == ' ' )           /* skip Blanks */
		    Pos++;  

		  if ( (TArg[Pos] < '0') || (TArg[Pos] > '9'))   /* Number? */
		    { ErrPrint("ERROR: target defintion: No number after ','");
		      return -1; };
	      
		  while ((TArg[Pos] >= '0') && (TArg[Pos] <= '9')) /* GetNum */
		    { MT[NTarEl] = MT[NTarEl]*10 + (double) (TArg[Pos] - '0');
		      Pos ++; };
		}

	      while ( TArg[Pos] == ' ' )           /* skip Blanks */
		Pos++;  
	      
	      if ( TArg[Pos] != ')')               /* There MUST be a ')' */
		{ ErrPrint("ERROR: target definition: ')' expected");
		  return -1; };

	      Pos++;                               /* skip last bracket ')' */

	      if ((ZT[NTarEl] == 0) || ZT[NTarEl] > MAXEL)      /* ZT  OK? */
		{ ErrPrint("ERROR: Z of target zero or out of range!");
		  return -1; };

	      if ( MT[NTarEl] <  (ZT[NTarEl]-0.1) )    /* MT too small? */
		{ ErrPrint("ERROR: M of target can't be much smaller than Z!");
		  return -1; };

	    }
	  else                                     /* Syntax error */
	    { ErrPrint("ERROR: target definition: No valid target after '('");
	      return -1; };
	  
	  NTarEl ++;                               /* ..one element element */

	};  /* If no zero found */

    };   /* while, soolping back to scan nexe element */

  if (! done)
    { ErrPrint("ERROR: Too many components in Target (see dokumentation");
      return -1; };
  
  /* set  CT */

  for (i=0; i < NTarEl; i++)                                  /* Find Norm */
      Norm = Norm + ( CT[i] * MT[i] );
  
  for (i=0; i < NTarEl; i++)          /* Set CT to *normated* concentration */
      CT[i] = CT[i] * MT[i] /  Norm ;
  
  return NTarEl;

}
   /* ScanPro (dependent function of ScanArgs)

       ScanTarget evaluates the Argument PROJEKTIL, or of PRODUCT,
       which may consists of a singel Particledefinition. It returns
       -1 for an syntax error. ScanPro also displays an appropriate
       error-message, if possible. The values for particle mass is
       entered in MP, Z in ZP.

       WARNING: Works only with A-Z and 0-9 ordered charsets like
                ASCII and ANSI */

int ScanPro (char ArgS[],
	     double *MP,
	     int *ZP )

{
  /* var */

  int i     = 0,                      /* counter */
      Pos   = 0,                      /* act. position in argument */
      SymLep = 0;                     /* lenght of projektile symbol */

  /* code */

  while ( ArgS[Pos] == ' ' )             /* skip leading blanks */
    Pos++;

  if ( ArgS[Pos] == '(')                 /* skip a bracket if present */
    Pos++;

  while ( ArgS[Pos] == ' ' )             /* skip blanks */
    Pos++;  

  if ((ArgS[Pos] == 'n') && (ArgS[Pos+1] == ')' ))  /* special for neutron */
      { *MP = M_El[0];
	*ZP = 0;
	return 1; };

  while (ArgS[i] != 0)                  /* make element symbols capitals */
    { if ((ArgS[i] >= 'a' ) && (ArgS[i] <= 'z' ))
	ArgS[i] = ArgS[i] + 'A' - 'a';
      i++ ; };

  if ( ( ArgS[Pos] >= 'A' ) && ( ArgS[Pos] <= 'Z' ) )
    { ErrPrint("ERROR: No natural element mass mix in projectile or product!");
      return -1; }

  else if  ( ( ArgS[Pos] >= '0' ) && ( ArgS[Pos] <= '9' ) )
    {
      /* Convention with two figures is expected */
      /* read now first on: */
      
      while ((ArgS[Pos] >= '0') && (ArgS[Pos] <= '9'))   /* 1st Int: ZP */
	{ *ZP = *ZP * 10 + (int) (ArgS[Pos] - '0');
	  Pos ++; };
      
      while ( ArgS[Pos] == ' ' )                        /* skip Blanks */
	Pos++;    

      if ( (ArgS[Pos] >= 'A') &&  (ArgS[Pos] <= 'Z')) /* Symbol */
	{
	  *MP = (double) *ZP;              /* 1st number was M, not Z ! */
	  if ((SymLep = GetZfromStr(ZP, &ArgS[Pos])) == -1)
	    return -1;                        
	  else   
	    Pos = Pos + SymLep; 
	}
      else if( ArgS[Pos] != ',')          /* There MUST be a comma */
	{ ErrPrint("ERROR: proj. or prod. def.: ',' or el. sym. expected");
	  return -1; }
      else
	{
	  Pos++;                               /* Skip comma */
	  
	  while ( ArgS[Pos] == ' ' )           /* skip Blanks */
	    Pos++;
	  
	  if ( (ArgS[Pos] < '0') || (ArgS[Pos] > '9'))      /* Number? */
	    { ErrPrint("ERROR: proj. or prod. def.: Mass number expected");
	      return -1; };
	  
	  while ((ArgS[Pos] >= '0') && (ArgS[Pos] <= '9'))   /* 2nd Int */
	    { *MP = *MP * 10 + (double) (ArgS[Pos] - '0');
	      Pos ++; };
	}


      while ( ArgS[Pos] == ' ' )           /* skip Blanks */
	Pos++;  
      
      if ( ArgS[Pos] == ')')               /* skip Bracket ')' */
	Pos++;    
      
      while ( ArgS[Pos] == ' ' )           /* skip Blanks */
	Pos++;
      
      if ( ArgS[Pos] != 0)
	ErrPrint("WARNING: ELAST truncated proj. or prod. def.! Continuing.");

      if (*ZP > MAXEL)                           /* too large? */
	{ ErrPrint("ERROR: Z of Projektil or Product too large!");
	  return -1; };
      
      if ( *MP <  (*ZP-0.1) )                          /* too small? */
	{ ErrPrint("ERROR: A of proj. or prod. can't be smaller than Z!");
	  return -1; };

    }
  else                                     /* Syntax error */
    { ErrPrint("ERROR: proj. or prod. def. : No valid particle after '('");
      return -1; }; 

  return 1;                                /* Exit status OK */
}



int ScanArgs(int ArgC, char *ArgS[],     /* UNIX-Parameters without first */
	     double *E1,                 /* Pointers to the Var's, related */
	     double *E2,                 /*   to the Arguments */
	     double *DeltaE,
	     double *Q,
	     double *MP,
	     double MT[],
	     double *MProd,
	     double *MRes,
	     double CT[],
	     double *TT,
	     double *Angle1,
	     double *Angle2,
	     double *DeltaA,
	     int ZT[],
	     int *ZP,
	     int *ZProd,
	     int *ZRes,
	     int *NTarEl,
	     int *IsGas,
	     int *ESteps,
             int *CSn,
	     int *SwitchQ,
	     int *Switchq,
	     int *Switchv,
             int *Switchx,
	     int *SwitchX,
	     int Com[],
	     char *Char_S,
	     char *Char_s )

{

  /* var */

  int i        = 0,          /* Temp */
      ii       = 0,          /* temp counter for scanned command */
      ArgNo    = 0,          /* Number of actual argument */
      ArgLen   = 0,          /* Lenghts of the actual argument */
      SwNo     = 1,          /* Number of switch in Argument, starting at 1 */
      ArgNeed  = 5,          /* Number of arguments needed */
      MixMass1 = FALSE,      /* Flag for 1st. taget element: Iso-Mix in Mass */
      GetProd  = FALSE,      /* Flag, if any reaction is needed => get Prod. */
      done     = FALSE;      /* Flag */

  double EX    = 0,          /* Exitation energy of beam particle [MeV] */  
         EX3   = 0;          /* Exitation energy of product particle [MeV] */ 

  /* code */

  /* Are there enough arguments to meet syntax? */

  if (ArgC < ArgNeed) 
    { ErrPrint("ERROR: Too few parameters");
      return -1; };

  /* Get switches */

  while (! done)            /* Scan all leading arguments that start with - */
    {
      if (ArgS[ArgNo][0] == '-')   
	{
	  ArgNeed++;        /* Switches were not taken in account so far */
	  if (ArgC < ArgNeed) 
	    { ErrPrint("ERROR: Too few parameters");
	      return FALSE; };

	  SwNo = 1;
	  ArgLen = strlen( ArgS[ArgNo]);
	  if (ArgLen == 1)
	    { ErrPrint("ERROR: '-' alone is an invalid argument.");
	      return -1; };
	  
	  while (ArgLen-1 >= SwNo)    /* Scanning the actual argument  */
	    {
	      switch ( ArgS[ArgNo][SwNo] )
		{
		case 'q':
		  *Switchq = TRUE;
		  *SwitchQ = FALSE;
		  *Switchv = FALSE;
	       	  break;
		  
		case 'Q':
		  *SwitchQ = TRUE;
		  *Switchq = TRUE;
		  *Switchv = FALSE;
		  break;
		  
		case 'v':
		  *Switchv = TRUE;
		  *Switchq = FALSE;
		  *SwitchQ = FALSE;
		  break;
		  
		case 'g':
		  *IsGas = TRUE;
		  break;  

		case 'c':
		  if (ArgLen-1 > SwNo)              /* syntax error? */
		    { ErrPrint("ERROR: c- switch not last in parameter");
		      return -1; };

		  if (ArgNeed > ArgC)                /* missing parameter */
		    {  ErrPrint("ERROR: Too few parameter");
		      return -1; };

		  if (! sscanf(ArgS[ArgNo+1], "%i", CSn) )
		    { ErrPrint("ERROR: Unable to charge state argument!");
		      return -1; }

		  ArgNo++;                           /* Correct "eaten" Arg. */
		  ArgNeed++;
		  ArgLen = 0;                        /* TRICK: End dbl. loop */
		  break;

		case 'x':
		  *Switchx = TRUE;                  /* let it be known... */
		  if (ArgLen-1 > SwNo)              /* syntax error? */
		    { ErrPrint("ERROR: x- switch not last in parameter");
		      return -1; };

		  if (ArgNeed > ArgC)                /* missing parameter */
		    {  ErrPrint("ERROR: Too few parameter");
		      return -1; };

		  if (! sscanf(ArgS[ArgNo+1], "%lf", &EX) )
		    { ErrPrint("ERROR: Unable to covert (2) ex. energy arg.!");
		      return -1; }

		  ArgNo++;                           /* Correct "eaten" Arg. */
		  ArgNeed++;
		  ArgLen = 0;                        /* TRICK: End dbl. loop */
		  break;

		case 'X':
		  *SwitchX = TRUE;                  /* let it be known... */
		  if (ArgLen-1 > SwNo)              /* syntax error? */
		    { ErrPrint("ERROR: X- switch not last in parameter");
		      return -1; };

		  if (ArgNeed > ArgC)                /* missing parameter */
		    {  ErrPrint("ERROR: Too few parameter");
		      return -1; };

		  if (! sscanf(ArgS[ArgNo+1], "%lf", &EX3) )
		    { ErrPrint("ERROR: Unable to covert (3)-ex. energy arg.!");
		      return -1; }

		  ArgNo++;                           /* Correct "eaten" Arg. */
		  ArgNeed++;
		  ArgLen = 0;                        /* TRICK: End dbl. loop */
		  break;

		case 's':
		  if (ArgLen-1 > SwNo)              /* syntax error? */
		    { ErrPrint("ERROR: s- switch not last in parameter");
		      return -1; };

		  if (ArgNeed > ArgC)                /* missing parameter */
		    {  ErrPrint("ERROR: Too few parameter");
		      return -1; };

		  if (strlen(ArgS[ArgNo + 1]) != 1)  /* Is next Arg. valid? */
		    { ErrPrint("ERROR: after -s one Character expected");
		      return -1; };

		  *Char_s = ArgS[ArgNo +1][0];		  
		  ArgNo++;                           /* Correct "eaten" Arg. */
		  ArgNeed++;
		  ArgLen = 0;                        /* TRICK: End dbl. loop */
		  break;

		case 'S':
		  if (ArgLen-1 > SwNo)              /* syntax error? */
		    { ErrPrint("ERROR: S- switch not last in parameter");
		      return -1; };

		  if (ArgNeed > ArgC)                /* missing parameter */
		    { ErrPrint("ERROR: Too few parameter");
		      return -1; };

		  if (strlen(ArgS[ArgNo + 1]) != 1)  /* Is next Arg. valid? */
		    { ErrPrint("ERROR: after -S one Character expected");
		      return -1; };

		  *Char_S = ArgS[ArgNo +1][0];		  
		  ArgNo++;                           /* Correct "eaten" Arg. */
		  ArgNeed++;
		  ArgLen = 0;                        /* TRICK: End dbl.loop */
		  break;

		case 'a':
		 
		  if (ArgLen-1 > SwNo)              /* syntax error? */
		    { ErrPrint("ERROR: a- switch not last in Argument");
		      return -1; };

		  if (ArgNeed > ArgC)                /* missing parameter */
		    { ErrPrint("ERROR: Too few parameter");
		      return -1; };

		  if (! sscanf(ArgS[ArgNo+1], "%lf", Angle1) )
		    { ErrPrint("ERROR: Unable to covert angle argument!");
		      return -1; }

		  ArgNo++;                           /* Correct "eaten" Arg. */
		  ArgNeed++;
		  
		  if (! sscanf(ArgS[ArgNo+1], "%lf", Angle2) ) /* one angle? */
		    { *Angle2 = *Angle1;
		      *DeltaA = 1; }
		  else
		    { ArgNeed=ArgNeed+2;
		      ArgNo=ArgNo+2;
		      if (! sscanf(ArgS[ArgNo], "%lf", DeltaA) )
			{ ErrPrint("ERROR: Trouble cov. angle step Arg.!");
			  return -1; };
		    };

		  ArgLen = 0;                        /* !!TRICK to end loop */
		  break;

		default:
		  ErrPrint("ERROR: Unimplemented Switch");
		  return -1;
		  break;

		} /* Switch */

	      SwNo++;

	    }; /* while */
	  
	  ArgNo ++;
	}
      else
	done = TRUE;

    }; /* while */
  
  done = FALSE;
  
  /* check for number of arguments after all switches done to aviod rt.Error */

  if (ArgNeed > ArgC)
    { ErrPrint("ERROR: Too few parameter");
      return -1; };

  /* check if commands are valid and copy to Com[] */

  ArgLen = strlen( ArgS[ArgNo]);

  for ( i=0 ; ( i <= ArgLen-1 ) ; i++ )
    {
      if (ii >= mCM)
	{ ErrPrint("ERROR: Too many commands in Com-String");
	  return -1; }

      switch ( ArgS[ArgNo][i] )         /* checking and tokenizing */
	{
	case 'A' :                /* it's the Angle output command */
	  Com[ii] = CM_ANGLE;
	  break;
	case 'E' :                /* it's the Energy output command */
	  Com[ii] = CM_ENERGY;
	  break;
	case 'a' :                /* it's the Angular straggling command */ 
	  Com[ii] = CM_ASTRAG;
	  break;
	case 'e' :                /* it's the Energy straggling command */ 
	  Com[ii] = CM_ESTRAG;
	  break;
	case 'l' :                /* it's the energy Loss command */
	  Com[ii] = CM_ELOSS;
	  break;
	case 'r' :                /* it's the stopping Range command */ 
	  Com[ii] = CM_RANGE;
	  break;
	case 'p' :                /* it's the power deposit (dE/dx) command */ 
	  Com[ii] = CM_DEDX;
	  break;
	case 'P' :                /* it's the max. dE/dx command */ 
	  Com[ii] = CM_DEDXMAX;
	  break;
	case 'c' :                /* it's the charge state command */ 
	  Com[ii] = CM_CHARGE;
	  break;
	case 'v' :                /* it's the v/c command */ 
	  Com[ii] = CM_V_C;
	  break;
	case 'o' :                /* it's energy-after-target command */ 
	  Com[ii] = CM_EOUT;
	  break;
	case 's' :                /* it's the beam charge state population */
          Com[ii] = CM_CSN;
          break;
	case 'X' :                /* if's a cross-section related command */
	  i++;
	  switch ( ArgS[ArgNo][i] )    /* checking and tokenizing X-Command */
	    {
	    case 'r' :                 /* CM-rutherford cross-section  */
	      Com[ii] =CM_XS_RU_CM;
	      break;

	    default:
	      ErrPrint("Undefined X-command.");
	      return -1;
	      break;
	    };
	  break;

	case 'D' :                /* if's a Database  command */
	  i++;
	  switch ( ArgS[ArgNo][i] )    /* checking and tokenizing X-Command */
	    {
	    case 'm' :                 /* CM-Mass of projectile  */
	      Com[ii] =CM_DB_MASS;
	      break;

	    case 'z' :                 /* CM-Z of projectile */
	      Com[ii] =CM_DB_Z;
	      break;

	    case 'q' :                 /* CM-q-value of reaction */
	      Com[ii] =CM_DB_Q;
	      break;

	    case 's' :                 /* CM-Symbol of projectile */
	      Com[ii] =CM_DB_SYM;
	      break;


	    default:
	      ErrPrint("Undefined D-command.");
	      return -1;
	      break;
	    };
	  break;
  
	case 'R' :                     /* it's a reaktion  produkt command ! */
	  
	  i++;                         /* need one more char */
	  GetProd = TRUE;              /* a product is needed in Com.Line */
	  
	  switch ( ArgS[ArgNo][i] )    /* checking and tokenizing R-Command */
	    {
	    case '4' :                 /* A R4 command can have 4 chars! */
	      i++;                     /* .. the third char */
	      switch (ArgS[ArgNo][i])
		{
	        case 's' :             /* the R4s command: ch. state pop. */
		  Com[ii] = CM_CSN4;
		  break;

		case 'A':
		  i++;                 /* .. and the 4th char */
		  switch (ArgS[ArgNo][i])
		    {
		    case '+':
		      Com[ii] = CM_ANGLE4F;
		      break;
		    case '-':
		      Com[ii] = CM_ANGLE4B;
		      break;
		      
		    default:
		      ErrPrint("Undefined R4A-command. use R4A+ or R4A-");
		      return -1;
		      break;
		    };
		  
		  break; /* end case 'A' */
		  
		  
		case 'E':
		  i++;                 /* .. and the 4th char */
		  switch (ArgS[ArgNo][i])
		    {
		    case '+':
		      Com[ii] = CM_E4F;
		      break;
		    case '-':
		      Com[ii] = CM_E4B;
		      break;
		      
		    default:
		      ErrPrint("Undefined R4E-command: Use R4E+ or R4E-");
		      return -1;
		      break;
		    };
		  
		  break; /* end case 'E' */
		  

		case 't': /* 4th cm-angle */
		  i++;                 /* .. and the 4th char */
		  switch (ArgS[ArgNo][i])
		    {
		    case '+':
		      Com[ii] = CM_CMANG4F;
		      break;
		    case '-':
		      Com[ii] = CM_CMANG4B;
		      break;
		      
		    default:
		      ErrPrint("Undefined R4C-command: Use R4t+ or R4t-");
		      return -1;
		      break;
		    };
		  
		  break; /* end case 't' */
		  
		default:
		  ErrPrint("Undefined R4-command");
		  return -1;
		  break;
		    
		};
	      
	      break;  /* end case R4-command */
  
	    case 'E' :             /* Its one of the kinematic E-comms.. */
	      i++;                 /* .. so it has a third char */
	      switch (ArgS[ArgNo][i])
		{
		case '+':
		  Com[ii] = CM_ENE_HI;
		  break;
		case '-':
		  Com[ii] = CM_ENE_LO;
		  break;
		case '0':
		  Com[ii] = CM_ENE_MID;
		  break;

		default:
		  ErrPrint("Undef. RE-command: Last letter must be +,- or 0");
		  return -1;
		  break;
		};
	      
	      break; /* RE-command */

	    case 't' :             /* Its one of the CM-angle-comms.. */
	      i++;                 /* .. so it has a third char */
	      switch (ArgS[ArgNo][i])
		{
		case '+':
		  Com[ii] = CM_CMANG3F;
		  break;
		case '-':
		  Com[ii] = CM_CMANG3B;
		  break;

		default:
		  ErrPrint("Undef. RC-command: Last letter must be + or -");
		  return -1;
		  break;
		};
	      
	      break; /* RC-command */


	    case 'J' :             /* Its one of the Jakobian commands */
	      i++;                 /* .. so it has a third char */
	      switch (ArgS[ArgNo][i])
		{
		case '+':
		  Com[ii] = CM_JAKO_F;
		  break;
		case '-':
		  Com[ii] = CM_JAKO_B;
		  break;

		default:
		  ErrPrint("Undef. RJ-command: Last letter must be + or -");
		  return -1;
		  break;
		};
	      
	      break; /* RJ-command */

	      
	    case 's' :             /* it's the product cs-population */
              Com[ii] = CM_CSN3;
              break;
	    case 'a' :             /* it's the Angular straggling command */ 
	      Com[ii] = CM_ASTRAG_P;
	      break;
	    case 'e' :             /* it's the Energy straggling command */ 
	      Com[ii] = CM_ESTRAG_P;
	      break;
	    case 'l' :               /* it's the energy Loss command */
	      Com[ii] = CM_ELOSS_P;
	      break;
	    case 'c' :               /* it's the charge state command */ 
	      Com[ii] = CM_CHARGE_P;
	      break;
	    case 'o' :               /* it's the average energy after r. */ 
	      Com[ii] = CM_EOUT_P;
	      break;
	    case 'T' :               /* it's the max. react. angle command */ 
	      Com[ii] = CM_ANGL_MAX;
	      break;
	    case 'C' :               /* it's the center of mass energy */
	      Com[ii] = CM_ECM;
	      break;
	      
	      /* NO blanks allowed between R and command char! */
	      
	    default:                 /* no command! => error! */
	      ErrPrint("Undefined rekation prod. command in command string!");
	      return -1;
	      break;
	    };

	  break;
	  
	case '.' : 
	  ii--;                      /* correct for wrong inc down the line */ 
	  break;                     /* dot to be ignored */
	  
	default:                     /* no command! => error! */
	  ErrPrint("Undefined command in command string!");
	  return -1;
	  break;
	};
      ii ++;	                     /* This was an Arggument...*/

    };

  Com[ii] = CM_NO_COM;                    /* Mark end of commands */
  
  ArgNo ++;                               /* Next argument! */

  *NTarEl = ScanTarget( ArgS[ArgNo], ArgS[ArgNo +1] ,
                        MT, CT, TT, ZT, &MixMass1);

  if (*NTarEl < 1)                        /* Error in target or thickness */
    return -1;

  ArgNo += 2;                             /* Set argument to next parameter */

  /* Look for the projektile .... */

  if ( -1 == ScanPro( ArgS[ArgNo], MP, ZP ) )   /* get projektil */
    return -1;

  if (*ZP == 0)
    { ErrPrint("ERROR: Sorry, can't calculate neutron scattering.");
      return -1; };

  ArgNo++;                                /* Set argument to next parameter */

  /* GetProduct and Q-Value */

  if (GetProd)                            /* a 2X command needs a product! */
    { 
      ArgNeed += 2;                       /* still enough pars.left? */

      if (ArgNeed > ArgC)
	{ ErrPrint("ERROR: Too few parameter for reakion produkt mode");
	  return -1; };

      if (MixMass1)
	ErrPrint ("WARNING: 1st target set to isotopic mass. (check with -v)");

      if ( -1 == ScanPro( ArgS[ArgNo], MProd, ZProd ) ) /* get Product */
	return -1;

       /* Use the exact masses for kinematic */

      *MP = IsoMass(*ZP, (int) (*MP + 0.5)) + EX * 0.001073543;
      MT[0] = IsoMass(ZT[0], (int) (MT[0] + 0.5)); /* Take 1st target def. */
      *MProd = IsoMass(*ZProd, (int) (*MProd + 0.5)) + EX3 *  0.001073543;
      *ZRes = *ZP + ZT[0] - *ZProd ;
      
      if (*ZRes < 0)                               /* charge OK? */
	{ ErrPrint("ERROR: This resction is impossible (4th charge negative)");
	  return -1; };     

      if (*ZRes > MAXEL)                           /* too large? */
	{ ErrPrint("ERROR: Z of 4th reaction partner is too large!");
	  return -1; };

      *MRes = IsoMass(*ZRes, (int) (*MP + MT[0] - *MProd + 0.5)); 
      
      if (*MRes < 0 )
	{ ErrPrint("ERROR: This reaction is impossible (4th mass negative)");
	  return -1; };

      ArgNo ++;

      if ((ArgS[ArgNo][0] == 'X') || (ArgS[ArgNo][0] == 'x') ||
	  (ArgS[ArgNo][0] == 'D') || (ArgS[ArgNo][0] == 'd')) /* Q from Data */
	  {
	    *Q = IsoQ(*ZP, ZT[0], *ZProd,   /* dirty trick to get A's ... */
		      (int) (*MP+0.5), (int) (MT[0]+0.5), (int) (*MProd+0.5));

	    if (*Q == UNKNOWN)
	      { ErrPrint("ERROR: Sorry, can't calculate Q-Value. No Data");
		return -1; };

	    *Q = *Q + EX - EX3;
	  }
      else if (! sscanf(ArgS[ArgNo], "%lf", Q) )
	  { ErrPrint("ERROR:Unable to covert energy Q-value argument!");
	    return -1; }

      else /* Q from User! Assign different Q-Value to heavier Product*/
	{ if (*MProd > *MRes)
	    *MProd = MT[0] + *MP - *MRes  - *Q * 0.001073543;  /* Q to unit */
	  else
	    *MRes  = MT[0] + *MP - *MProd - *Q * 0.001073543;
	};

      ArgNo ++;

    }

  /* Get energy values */

  if (! sscanf(ArgS[ArgNo], "%lf", E1) )  /* E1 is already a pointer */
    { ErrPrint("ERROR:Unable to covert energy (E1) argument!");
      return -1; };

   if (*E1 <= 0.0)
     *E1 = 0.0000001;

  if ( ArgNo == ( ArgC-1 ) )              /* Ready! (only E1 def.) */
    return 1;

  if ( ArgNo != (ArgC-3))                 /* two more or failure! */
    { ErrPrint("ERROR: Number of arguments either one to small or to large");
      return -1; };

  ArgNo++;

  if (! sscanf(ArgS[ArgNo], "%lf", E2) )     /* E2 is already a pointer */
    { ErrPrint("ERROR:Unable to covert energy (E2) argument!");
      return -1; };

  if ((*E2 <= 0.) || ( ((*E1-*E2)*(*E1-*E2)) < 0.0001) )
    { ErrPrint("ERROR:Energy E2 must be positive and other than E1");
      return -1;} ;

  ArgNo++;

  if (! sscanf(ArgS[ArgNo], "%i", ESteps) )  /* ESteps is already a pointer */
    { ErrPrint("ERROR:Unable to covert energy (ESteps) argument!");
      return -1; };

  if (*ESteps < 1)
    { ErrPrint("ERROR: Number of energy steps < 1: Nonsense!");
      return -1; };
  
  *DeltaE = (*E2 - *E1) / (float) (*ESteps-1) ;
         /* allows intentionally negative results! */
    

  

  return 1;                              /* Ready! */
  
}


void DispArgs(int ArgC, char *ArgS[],     /* UNIX-Parameters without first */
	      double E1,                  /* Var's, related */
	      double E2,                  /* to the Arguments */
	      double DeltaE,
	      double Q,
	      double MP,
	      double MT[],
	      double MProd,
	      double MRes,
	      double CT[],
	      double TT,
	      double Angle1,
	      double Angle2,
	      double DeltaA,
	      int ZT[],
	      int ZP,
	      int ZProd,
	      int ZRes,
	      int NTarEl,
	      int IsGas,
	      int ESteps,
	      int SwitchQ,
	      int Switchq,
	      int Switchv,
	      int Switchx,
	      int SwitchX,
	      int Com[],
	      char Char_S,
	      char Char_s )
{

  /* var */
   
  int i   =0;
  
  /* code */

  printf("\n\n        ***** Input and Internal Interpretation *****\n\n");

  printf("Arguments passed to ELAST:\n");
  for ( i=0 ; i < ArgC ; i++ )
    {
      printf("'%s' " , ArgS[i]);
    };

  printf("\n\nSwitchsetting is: (0:Off, 1:On)\n");
  printf("Q:%i, q:%i, v:%i g:%i x:%i\n\n" ,SwitchQ ,Switchq, Switchv, IsGas, Switchx); 

  printf("The seperator charcacters for the output are\n");
  printf("Seperation IN the lines: ASC %i\n", ((int) Char_s) );
  printf("Seperation OF the lines: ASC %i\n\n", ((int) Char_S) );

  if (IsGas)
    printf("Target type: gas\n\n");
  else
    printf("Target type: solid\n\n");

  printf("Angle,Ejektile in the Lab.: %f deg to %f deg, Step %f deg\n\n",
	  Angle1,Angle2,DeltaA);

  printf("Energy: E1:%f [MeV], E2:%f [MeV], ESteps:%i, DeltaE:%f [MeV]\n",
	 E1, E2, ESteps, DeltaE);
  printf("  ...taken in the system of the target, e.g.: the lab!\n\n");

  printf("Nuclear charge and mass of projektile        : %i [e] %f [unit]\n",
	 ZP, MP);
  printf("Nuclear charge and mass of react. product    : %i [e] %f [unit]\n",
	 ZProd, MProd);
  printf("Nuclear charge and mass of 2nd react. product: %i [e] %f [unit]\n",
	 ZRes, MRes);
  printf("Total target thickness: %f [mg/cm**2]\n",TT);
  printf("Z, Mass and rel. concentration of target:\n");

  for (i=0 ; i < NTarEl ; i++ )
    printf("Component %i: %i [e], %f [unit], %f\n", 
	    (i+1), ZT[i], MT[i], CT[i] );
}

void ErrPrint(char ErrText[])

{
  fprintf( stderr, "ELAST: %s\n", ErrText);
}














