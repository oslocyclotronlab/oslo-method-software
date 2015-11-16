/*Offline sorting routine for SIRIUS
Written by: Magne Guttormsen
Date      :  1. Febr. 2008
Exp name  : only new and old NaI
Comment   : 64 telescopes, 28 NaI, 16 Nim ADCs
box  0	= energy	VME NaI 32 ch
box  1	= time		VME NaI 32 ch
box  2	= energy	VME dE  32 ch
box  3	= energy	VME dE  32 ch
box  4	= energy	VME E   32 ch
box  5	= miscl		Nim     16 ch
box  6-9	= energy	Camac ADCs, 8 ch
box  10-13	= time	Camac TDCs, 8 ch
box  14	= pileup	Camac 32 ch				
box  15 = Scaler 1151N 32 ch, 16 LSB stored in ch  0-15, 16 mSB stored in ch  16-31?*/
/*	============================================================*/
/*	The lines above should be included in all sorting routines	*/
/*	in order to identify the routine in use.					*/
/*	The header is terminated by the	?-symbol					*/
/*	============================================================*/

/*	============================================================*/
/*  DO NOT CHANGE the values presorted for this event:			*/
/*	int		dp[64],	 p[64],  na[32],  tna[32],  nim[32],	cadc[32],	ctdc[32],	pu[32], 	sc[32]; */
/*	int		dpi[64], pi[64], nai[32], tnai[32], nimi[32],	cadci[32],	ctdci[32],	pui[32], 	sci[32];*/
/*	int		dpnu,	 pnu,    nanu,    tnanu,    nimnu,		cadcnu,		ctdcnu,		punu, 		scnu;	*/
/*	These values is given in the include file "gsdims.h"  		*/								
/*	============================================================*/

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>    
#include <math.h>
#include <time.h>
#include <gsdims.h>

int		box_chn(int box, int chn);

int user_routine(	int *psingles, int *pesp, int *pdesp, int *pedesp, int *pthicksp, 
					int *pgesp, int *ptgesp, int *pnasp, int *ptnasp, int *pansp, int *pagsp, int *pmtsp)
{  
	int		i;
	float	x, r1;

/* Random floats between -0.5 and +0.5 */
	r1 = drand48() - 0.5;

/* **************************************** */
/* Making old CAMAC NaI energy and time spectra		*/
/* **************************************** */
	for (i = 0; i <= cadcnu; i++){
		x		= (cadc[i] + r1)/2.;
		cadc[i]	= (int) (x);
		if (cadc[i] < 0 || cadc[i] > 2047) cadc[i] = 0;
		(*(pmtsp + cadc[i] + cadci[i] * 2048))++;
	}
	for (i = 0; i <= ctdcnu; i++){
		x		= (ctdc[i] + r1)/16.;
		ctdc[i]	= (int) (x);
		if (ctdc[i] < 0 || ctdc[i] > 511) ctdc[i] = 0;
		(*(pmtsp + ctdc[i] + ctdci[i] * 512 + 32 * 2048))++;
	}
	 				
/* **************************************** */
/* Making new VME NaI energy and time spectra		*/
/* **************************************** */
	for (i = 0; i <= nanu; i++){
		x		= (na[i] + r1)/2.;
		na[i]	= (int) (x * gainna[nai[i]] + shiftna[nai[i]] + 0.5);
		if (na[i] < 0 || na[i] > 2047) na[i] = 0;
		(*(pnasp + na[i] + nai[i] * 2048))++;
	}
	for (i = 0; i <= tnanu; i++){
		x		= (tna[i] + r1)/16.;
		tna[i]	= (int) (x + shifttna[tnai[i]] + 0.5);
		if (tna[i] < 0 || tna[i] > 511) tna[i] = 0;
		(*(ptnasp + tna[i] + tnai[i] * 512))++;
	}
	
/* **************************************** */
/* Making Ge energy and time spectra		*/
/* bit 4 = ADC0(e), bit 5 = ADC1(t),		*/
/* bit 6 = ADC2(e), bit 7 = ADC3(t), etc	*/
/* **************************************** */
	for (i = 0; i <= nimnu; i++){
		if(nimi[i] > 3){
			if((nimi[i]/2) * 2 == nimi[i]){
				x		= (nim[i] + r1);
				nim[i]	= (int) (x * gainge[(nimi[i]/2) * 2] + shiftge[(nimi[i]/2) * 2] + 0.5);
				if (nim[i] < 0 || nim[i] > 4095) nim[i] = 0;
				(*(pgesp + nim[i] + ((nimi[i]/2) * 2) * 4096))++;
			}else{
				x		= (nim[i] + r1)/8;
				nim[i]	= (int) (x + shiftge[((nimi[i]/2) * 2)+1] + 0.5);
				if (nim[i] < 0 || nim[i] > 511) nim[i] = 0;
				(*(ptgesp + nim[i] + ((nimi[i]/2) * 2 + 1) * 512))++;
			}
		}
	}
	return 0;
}
