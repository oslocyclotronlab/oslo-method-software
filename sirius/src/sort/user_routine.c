/*Sorting routine for SIRIUS
Written by: Magne Guttormsen
Date      :  1. Febr. 2009
Exp name  : new CAEN tdc and adc
Comment   : 64 telescopes, 28 NaI, 16 Nim ADCs
box 0x00 = Pattern 			ch 0-3, NIM ADCs ch 4-15
box 0x01 = Wall-clock time  ch 16-17 
box 0x02 = VME scaler 1151N ch 0-31 (16 LSB stored in ch 0-15, 16 MSB in ch 16-31)
box 0x10 = Time VME NaI		ch 0-31
box 0x20 = Energy VME NaI 	ch 0-31
box 0x21 = Energy VME E   	ch 0-31
box 0x22 = Energy VME dE1  	ch 0-31
box 0x23 = Energy VME dE2   ch 0-31

box  6-9	= energy	Camac ADCs, 8 ch
box  10-13	= time	Camac TDCs, 8 ch
box  14	= pileup	Camac 32 ch?*/
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
	int		i, i1, i2;
	float	x, r1;

/* ************************************ */
/* Random floats between -0.5 and +0.5	*/
/* ************************************ */
	r1 = drand48() - 0.5;
	 				
/* **************************************** */
/* Making NaI energy and time spectra		*/
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

	 				
/* ****************************** */
/* Making dE and E energy spectra */
/* ****************************** */
	for (i = 0; i <= dpnu; i++){
		x		= (dp[i] + r1)/2.;
		dp[i]	= (int) (x * gainde[dpi[i]] + shiftde[dpi[i]] + 0.5);
		if (dp[i] < 0 || dp[i] > 2047) dp[i] = 0;
		(*(pdesp + dp[i] + dpi[i] * 2048))++;
	}
	for (i = 0; i <= pnu; i++){
		x		= (p[i] + r1)/2.;
		p[i]	= (int) (x * gaine[pi[i]] + shifte[pi[i]] + 0.5);
		if (p[i] < 0 || p[i] > 2047) p[i] = 0;
		(*(pesp + p[i] + pi[i] * 2048))++;
	}








































	
/* ************************************************************	*/
/* Making 16 scaler read out, stored in raw 9 of SINGLES matrix	*/
/* Scaler value stored in chs 0-15, and increments in chs 16-31	*/
/* The scaler value (32 bits) is composed of two 16-bit words	*/
/* 16 LSB are in scaler-chs 0-15, 16 MSB are in chs 16-31		*/
/* ************************************************************	*/
	for (i = 0; i <= scnu; i = i + 2){
		i1 = (unsigned int) sc[i];
		i2 = (unsigned int) sc[i+1];
		i2 = i1 + ((i2 & 0x0000ffff) << 16);
		*(psingles + sci[i] + 16 + 9 * 4096) = i2 - (unsigned int) *(psingles + sci[i] + 9 * 4096);
		*(psingles + sci[i] +      9 * 4096) = i2;
	}

/* ************************************************************	*/
/* Reading wall-clock time and store in raw 9 of SINGLES matrix	*/
/* Time value stored in ch 32, and incremented time in ch 33	*/
/* The time value (32 bits) is composed of two 16-bit words		*/
/* 16 LSB are in ch 16, 16 MSB are in ch 17						*/
/* ************************************************************	*/
	for (i = 0; i <= nimnu; i++){
		if(nimi[i] == 16){
			i1 = (unsigned int) nim[i];
			i2 = (unsigned int) nim[i+1];
			i2 = i1 + ((i2 & 0x0000ffff) << 16);
			*(psingles + 33 + 9 * 4096) = i2 - (unsigned int) *(psingles + 32 + 9 * 4096);
			*(psingles + 32 + 9 * 4096) = i2;
		}
	}
	
/* **************************************** */
/* Making Ge energy and time spectra		*/
/* bit 4 = ADC0(e), bit 5 = ADC1(t),		*/
/* bit 6 = ADC2(e), bit 7 = ADC3(t), etc	*/
/* **************************************** */
/*
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
*/
	return 0;
}

