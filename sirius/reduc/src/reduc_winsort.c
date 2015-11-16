#include	<stdio.h>
#include	<stdlib.h>
#include	<ctype.h>
#include	<string.h>    
#include	<math.h>
#include	<gsdims.h>
#include	<specdims.h>

/* ******************************************************/
/* Selecting particle types from thickness				*/
/* Good event defined as either:						*/
/* particle thickness between h_low and h_high (p,d,t)	*/ 
/* or thickness between alfa_low and alfa_high (4He)	*/ 
/* ******************************************************/


/* 64 telescopes
box  2	= energy	VME dE  32 ch
box  3	= energy	VME dE  32 ch
box  4	= energy	VME E   32 ch */

/*	============================================================*/
/*  DO NOT CHANGE the values presorted for this event:			*/
/*	int		dp[64],	 p[64],  na[32],  tna[32],  nim[32],	cadc[32],	ctdc[32],	pu[32], 	sc[32]; */
/*	int		dpi[64], pi[64], nai[32], tnai[32], nimi[32],	cadci[32],	ctdci[32],	pui[32], 	sci[32];*/
/*	int		dpnu,	 pnu,    nanu,    tnanu,    nimnu,		cadcnu,		ctdcnu,		punu, 		scnu;	*/
/*	These values is given in the include file "gsdims.h"  		*/								
/*	============================================================*/

int reduc_winsort( int *pesp, int *pdesp, int *pedesp)
{
	int		dei, ei, i_max, de_max;
	int		i, j;
	int		de, e, ede, thick;
	float	x, r1, r2, fe, fde;
	int		good;
	
/* Assumes a bad event */
   good = -1;

/* Random floats between -0.5 and +0.5 */
	r1 = drand48() - 0.5;
	r2 = drand48() - 0.5;

/* ****************************************	*/
/*	Only accepting events with one and only */
/*	one E-detector							*/
/* ****************************************	*/
    if(pnu < 0 || pnu > 1)							goto L9999;
	if(pnu == 1 & abs(pi[0]-pi[1]) != 1)			goto L9999;
	ei	= pi[0]/2;
	e	= p[ei];
	
/* ****************************************	*/
/*	Check that one dE matches the E detector*/
/*	Picking the dE with highest energy		*/
/*	Checking that pads of dE and E match	*/
/* ****************************************	*/
	if(dpnu < 0 || dpnu > 2)						goto L9999;
	de_max	=  0;
	i_max	= -1;
	for (i = 0; i <= dpnu; i++){
		if(dp[i] > de_max){
			de_max	= dp[i];
			i_max	= i;
		}
	}
	if(i_max == -1)									goto L9999;
	if(dpi[i_max]/8 != ei)							goto L9999;
	dei = dpi[i_max];       /* The dE detector with highest energy */
	de  = dp[dei];

/* ****************************************	*/
/*	A good dE-E telescope event is found	*/
/*	Energies: de  and e						*/
/*	Pads:     dei and ei					*/
/*	We will now align energies				*/
/* ****************************************	*/
	e  = (int)((e  + r1) * gaine[ei]   + shifte[ei]   + 0.5);
	de = (int)((de + r2) * gainde[dei] + shiftde[dei] + 0.5);
	if (e  < 1 || e  > 2047)						goto L9999;
	if (de < 1 || de > 2047)						goto L9999;
	fe	=  e * gaine[ei]   + shifte[ei];
	fde	= de * gainde[dei] + shiftde[dei];
	ede = (int)(fe + fde + r1 + 0.5);
	if (ede < 1 || ede > 2047)						goto L9999;
	thick = range[ede] - range[e];
	if (thick >=         h_low && thick <=         h_high) good = 0;				/* Window on p,d and t */
	if (thick >= alfa_low[dei] && thick <= alfa_high[dei]) good = 0;				/* Window on alpha */
	L9999:
	return good;
}