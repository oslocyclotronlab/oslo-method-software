/*Sorting routine for SIRIUS
 Written by: Magne Guttormsen
 Date      : 30. Jan. 2009
 Exp name  : CAEN TDC's and ADC's
 Comment   : 64 telescopes, 28 NaI
 box 0x00 = Pattern          ch 0-3, NIM ADCs ch 4-15
 box 0x01 = Wall-clock time  ch 16-17 
 box 0x02 = VME scaler 1151N ch 0-31 (16 LSB stored in ch 0-15, 16 MSB in ch 16-31)
 box 0x10 = Time VME NaI     ch 0-31
 box 0x20 and 0x24 = Energy VME NaI   ch 0-31
 box 0x21 = Energy VME E     ch 0-31
 box 0x22 = Energy VME dE1   ch 0-31
 box 0x23 = Energy VME dE2   ch 0-31?*/
/*	============================================================*/
/*	The lines above should be included in all sorting routines	*/
/*	in order to identify the routine in use.					*/
/*	The header is terminated by the	?-symbol					*/
/*	============================================================*/

/*	============================================================*/
/*  DO NOT CHANGE the values presorted for this event:			*/
/*	int		dp[64],	 p[64],  na[32],  tna[32],  nim[32],	cadc[32],	ctdc[32],	pu[32], 	sc[32]; */
/*	int		dpi[64], pi[64], nai[32], tnai[32], nimi[32],	cadci[32],	ctdci[32],	pui[32], 	sci[32];*/
/*	int		dpnu,	 pnu,    nanu,    tnanu,    nimnu,		cadcnu,		ctdcnu,		punu,scnu;	*/
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
float	dT_start(int ch);
float	dT_stop(int ch);
int user_routine(	int *psingles, int *pesp, int *pdesp, int *pedesp, int *pthicksp, 
				 int *pgesp, int *ptgesp, int *pnasp, int *ptnasp, int *pansp, int *pagsp, int *pmtsp)
{  
	int		i, j;
	float	x, r1, r2;
	int		gam[32], time[32], gami[32], gamnu;
	int		thick;
	int		de = -1, e = -1, ede = -1,  k = -1;
	int		dei= -1, ei = -1, eOK =-1, deOK = -1, edeOK = -1, naOK = -1;
	
	/* ************************************ */
	/* Random floats between -0.5 and +0.5	*/
	/* ************************************ */
	r1 = drand48() - 0.5;
	r2 = drand48() - 0.5;
	
	/* ****************************** */
	/* Making dE and E energy spectra */
	/* ****************************** */
	if (dpnu > 1 || pnu > 1) return 0;
	
	for (i = 0; i <= dpnu; i++){
		x		= (dp[i] + r1)/8.;
		dp[i]	= (int) (x * gainde[dpi[i]] + shiftde[dpi[i]] + 0.5);
		if (dp[i] > 1 && dp[i] < 512 && dpi[i] > -1 && dpi[i] < 64){ 
			de  = dp[i];
			dei = dpi[i];
			deOK = 1;
			(*(pdesp + de + dei * 2048))++;
		}
	}
	for (i = 0; i <= pnu; i++){
		x		= (p[i] + r2)/2.;
		p[i]	= (int) (x * gaine[pi[i]*4] + 0 + 0.5);
		if (p[i] > 1 && p[i] < 2048 && pi[i]/2 > -1 && pi[i]/2 < 8){ 
			e  = p[i];
			ei = pi[i]/2;
			eOK = 1;
			(*(pesp + e + ei * 2048))++;
		}
	}
	
	/* ************************* */
	/* Making EDESP matrix       */
	/* Making ALFNA banan matrix */
	/* ************************* */
	if(eOK == 1 && deOK == 1){
		e = e + shifte[dei];
		ede = de + e;
		if (ede > 0 && ede < 2048){
			edeOK = 1;
			(*(pansp  +   e +   de * 2048))++;
			(*(pedesp + ede +  dei * 2048))++;
		}
	}
	
	/* ******************************************************** */
	/* Making sure it is a good telescope event, if not, return */
	/* ******************************************************** */
	if(eOK !=1 || deOK != 1 || edeOK != 1 || dei/8 != ei ){
		return 0;
	}
	
	/* ************************ */
	/* Making thickness spectra */
	/* ************************ */
	thick = range[ede] - range[e];
	if (thick > 0 && thick < 2048) (*(pthicksp + thick + dei * 2048))++;

	/* ********************************* */
	/* Putting gate on thickness spectra */
	/* ********************************* */
	if(thick >= 102 && thick <= 113){ //3He
		(*(pmtsp + ede + 0*2048))++;
	}
	if(thick >= 125 && thick <= 138){ //4He
		(*(pmtsp + ede + 1*2048))++;
	}
	
	/* **************************************** */
	/* Making NaI energy and time spectra		*/
	/* Check that NaI energy and time exist		*/
	/* **************************************** */	
	for (i = 0; i <= tnanu; i++){
		for (j = 0; j <= nanu; j++){
			if(nai[j]  == tnai[i]){
				x		= (na[j] + r1)/2.;
				na[j]	= (int) (x * gainna[nai[j]] + shiftna[nai[j]] + 0.5);
				x		= tna[i]/8.;
				tna[i]	= (int) (x + shifttna[tnai[i]] + 0.5);
				tna[i]  = tna[i] - (int)(dT_stop(na[j]) + dT_start(e));
				if (na[j] > 0 && na[j] < 2048 && tna[i] > 0 && tna[i] < 512) { 
					naOK = 1;
					k++;
					gam[k]  = na[j];
					time[k] = tna[i];
					gami[k] = tnai[i];
					(*(pnasp  +  gam[k] + gami[k] * 2048))++;
					(*(ptnasp + time[k] + gami[k] *  512))++;
				}
			}
		}
	}
	gamnu = k;
//	printf("nanu= %d tnanu= %d gamnu= %d \n",nanu, tnanu, gamnu);
			   
	for (i = 0; i <= gamnu; i++){
		if(thick >= 102 && thick <= 113 ){ //3He
			(*(pmtsp + time[i] + 2*2048))++;
			if(time[i] >= 142 && time[i] <= 157) (*(pagsp + (gam[i]/2) + (ede/4) * 2048))++;
			if(time[i] >= 171 && time[i] <= 186) (*(pagsp + (gam[i]/2) + (ede/4) * 2048))--;
		}
		if(thick >= 125 && thick <= 138 ){ //4He
			(*(pmtsp + time[i] + 3*2048))++;
			if(time[i] >= 142 && time[i] <= 157) (*(pagsp + (gam[i]/2) + 1024 + (ede/4) * 2048))++;
			if(time[i] >= 223 && time[i] <= 238) (*(pagsp + (gam[i]/2) + 1024 + (ede/4) * 2048))--;
		}
	}
			
	return 0;
}

float dT_stop(int ch)
{
	float a = 50., b = -0.017;
	return (float)a*exp(b*(float)ch);
}

float dT_start(int ch)
{
	float a = -400., b = -0.015;
	return (float)a*exp(b*(float)ch);
}

