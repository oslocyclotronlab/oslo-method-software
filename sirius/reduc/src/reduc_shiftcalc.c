/* -----------------------------------------*/
/* For each E-counter, exctract the shift	*/    
/* adjustment factors, adfac(0-63)			*/
/* Written by: Magne Guttormsen             */
/* Date      : January 2008 without Ges     */
/* Exp name  : General for CACTUS           */
/* Comment   : 64 telescopes, 28NaI         */
/* -----------------------------------------*/
/* ==================================================	*/
/* The Event matrix										*/
/* Define spectra										*/
/* Number of particle telescopes 8/64					*/
/* The goal peak values, read from file					*/
/* Finding the shift for E-detector						*/	
/* ==================================================	*/

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>    
#include <math.h>
#include <gsdims.h>

int shift_calc(float goalp[64], float shift[64], int telescopeno, int *pesp, int *pdesp, int *pedesp, float centroid[64])
{
	float sumcount;
	int i, i1, i2, maxch, low;
	float weigthcount;
	int chan, high, temp, istep;
	float count;
	int teleno;
	float sumweigthcount;

	for (teleno = 0; teleno < telescopeno; ++teleno) {
		low = 0.8*goalp[teleno];						/* Find peak search limits */
		high =1.2* goalp[teleno];
		if (low  < 10) low = 10;
		if (high > 2047) high = 2048;
		maxch = 0;										/* Find highest peak within limits */
		for (i = low; i < high; ++i) {
			temp = *(pedesp + i + teleno * 2048);
			if (temp > maxch) {
				maxch = temp;
				chan = i;
			}
		}

/* Find low/high marker for ede-centroid calculations */
		low = chan - 50;
		high = chan + 50;
		if (low  < 10) low = 10;
		if (high > 2047) high = 2048;
	
		for (i = low; i <= chan; ++i) {
			if (*(pedesp + i + teleno * 2048) < maxch / 2) {
				i1 = i;
			}
		}
		for (i = chan; i <= high; ++i) {
			if (*(pedesp + i + teleno * 2048) > maxch / 2) {
				i2 = i;
			}
		}
		istep = (int) ((float) (i2 - i1) * 1.5 + .5);
		if (istep < 1) istep = 1;
		if (istep > 50) istep = 50;
	
/* Calculate centroid of peak */
		low  = chan - istep;
		high = chan + istep;
		if (low  < 10) low = 10;
		if (high > 2047) high = 2048;
	
		sumcount = 0.0;
		sumweigthcount = 0.0;
		for (i = low; i <= high; ++i) {
			count = (float) (*(pedesp + i + teleno * 2048));
			weigthcount = count * (float)i;
			sumcount += count;
			sumweigthcount += weigthcount;
		}
		if (sumcount > 0.0) {
			centroid[teleno] = sumweigthcount / sumcount;
		} else {
			centroid[teleno] = 0.0;
		}
	
/*	  if(teleno == 4) printf("low = %5d high = %5d, centroid = %f \n",low, high, centroid[teleno]); */
	
/* Calculate shift adjustment based on goal peak value */
		if (centroid[teleno] > 0.0) {
			shift[teleno] = goalp[teleno] - centroid[teleno];
		} else {
			shift[teleno] = 0.0;
		}
	}
	return 0;
}

