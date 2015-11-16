#include	<stdio.h>
#include	<stdlib.h>
#include	<ctype.h>
#include	<string.h>    
#include	<math.h>
#include	<gsdims.h>

#define boe(x)	((x&0xf0000000)>>28)
#define ndw(x)	(x&0x000000ff)
#define box(x)	((x&0x3f800000)>>23)
#define chn(x)	((x&0x007f0000)>>16)
#define dta(x)	(x&0x0000ffff)
#define EOB     0xe0000000

/* ======================================================== */
/* Purpose :Sort data into a matrix which is used by		*/
/*			the sorting application routine.				*/
/*			The event matrix is processed by the subr.		*/
/*			reduc_winsort which returns whether the event	*/
/*			is to be stored or not.							*/
/* ======================================================== */

int reduc_winsort(int *pesp, int *pdesp, int *pedesp);
int grec, evlen;

int reduc_winformat(int *bufp, int *outp, int *messp, int *pesp, int *pdesp, int *pedesp, float final_edeshift[64], int *pwritestat, int *perrstat,  int newoutlength)
{
	float r;
	float evfrac;
	int ie, i, iE_counter;
	int header_id, header_mask;
	int tpu, nextpointer, temp, xrow, telescopeno;
	int header, xindex, yindex, currentpointer, pattern;
	int good, writestatus;
	int gevents, events, evlen;
	int rawevent[1024];
	int bufferlength; 
	int tel;
	int writestat, errstat;
	int outlength;
	
	int	l_word;
	int	words, nword, nboe, nbox;
	
	writestat = *pwritestat;
	errstat   = *perrstat;
	outlength = newoutlength;
	
	r	= drand48() - 0.5;										/* Random between -0.5 and +0.5	*/
	
	bufferlength	= 32768;
	errstat			= 0;										/*	Initialisering */
	words			= 0;
	
	errstat			= 0;										/*	Initialisering */
	currentpointer  = 0;
	nextpointer		= 0;
	gevents         = 0;
	events          = 0;
	writestat       = 0;
	good			=-1;

	telescopeno		= *(messp + 11);							/* Number of telescopes used */
 
	while (words < bufferlength) {								/* Go through buffer */
		dpnu		= -1;											/* Reset numbers */
		pnu			= -1;             
		nanu		= -1;
		tnanu		= -1;
		nimnu		= -1;
		cadcnu		= -1;
		ctdcnu		= -1;
		punu		= -1;
		scnu		= -1;
        iE_counter	= -1;
		
/*	**************************************************************	*/
/*	Testing header word of event									*/
/*	No more data in buffer if header = 0, return from function		*/
/*	If this should occur too early (partly filled buffer), it may	*/
/*	be a serious problem. Mark such a buffer as bad buffer.			*/
/*	**************************************************************	*/
		currentpointer = nextpointer;
		l_word	= *(bufp + nextpointer);
		nboe	= boe(l_word);
		if(nboe == 0xc){										/* Begin of event */
			nword = ndw(l_word);								/* Number of words (ADCs) with data */
		}else{
			if(l_word != EOB ) errstat = -1;                    /* End of buffer */
			goto L999;											/* Drop current buffer */
		}

/*	Save pointer to next event */
		evlen = nword + 1;
		if( evlen > 1024){
			printf("Warning, eventlength > 1024 \n");
			errstat = -1;
			goto L999;
		}
		nextpointer = evlen + currentpointer;
		
/*	Save the raw event */
		for( i = 0; i < evlen; i++){
			rawevent[i] = *(bufp + currentpointer + i);
		}

/* **************************************** */
/*	Looping through all words (ADCs) and	*/
/*	storing in respective vectors			*/
/* **************************************** */
		for (i = 1; i <= nword; i++){
			l_word	= *(bufp + i + words);
			nbox	= box(l_word);					/* The box that has fired */
			
			if(nbox == 0x00){						/* Pattern ch 0 - 3, NIM ADCs ch 4 - 15*/
				nimnu++;                        
				nimi[nimnu]		=	chn(l_word);
				nim[nimnu]		=	dta(l_word);
			}
			if(nbox == 0x01){						
				nimnu++;                        	/* Wall-clock time  ch 16 - 17*/
				nimi[nimnu]		=	chn(l_word);
				nim[nimnu]		=	dta(l_word);
			}
			if(nbox == 0x02){						/* VME scaler 1151N ch 0-31*/
				scnu++;
				sci[scnu]		=	chn(l_word);
				sc[scnu]		=	dta(l_word);
			}
			if(nbox == 0x10){						/* Time of NaI ch 0-31*/
				tnanu++;
				tnai[tnanu]		=	chn(l_word);
				tna[tnanu]		=	dta(l_word);
			}		
			if(nbox == 0x20){						/* Energy of NaI ch 0-31*/
				nanu++;
				nai[nanu]		=	chn(l_word);
				na[nanu]		=	dta(l_word);
			}
			if(nbox == 0x21){						/* Energy E ch 0-32		*/
				pnu++;								/* Only 8 E counters in */
				pi[pnu]			=	chn(l_word);	/* ch 0, 2,..., 16		*/
				p[pnu]			=	dta(l_word);
			}
			if(nbox == 0x22){						/* Energy dE1 ch 0-31 */
				dpnu++;
				dpi[dpnu]		=	chn(l_word);
				dp[dpnu]		=	dta(l_word);
			}
			if(nbox == 0x23){						/* Energy dE2 ch 32-61 */
				dpnu++;
				dpi[dpnu]		=	chn(l_word) + 32;
				dp[dpnu]		=	dta(l_word);
			}


			
			
			
			
			
#if 0
			if(nbox == 6){						/* Camac ADC1 ch 0-7*/
				cadcnu++;
				cadci[cadcnu]	=	chn(l_word);
				cadc[cadcnu]	=	dta(l_word);
			}
			if(nbox == 7){						/* Camac ADC2 ch 0-7*/
				cadcnu++;
				cadci[cadcnu]	=	chn(l_word) + 8;
				cadc[cadcnu]	=	dta(l_word);
			}
			if(nbox == 8){						/* Camac ADC3 ch 0-7*/
				cadcnu++;
				cadci[cadcnu]	=	chn(l_word) + 16;
				cadc[cadcnu]	=	dta(l_word);
			}
			if(nbox == 9){						/* Camac ADC4 ch 0-7*/
				cadcnu++;
				cadci[cadcnu]	=	chn(l_word) + 24;
				cadc[cadcnu]	=	dta(l_word);
			}
			if(nbox == 10){						/* Camac TDC1 ch 0-7*/
				ctdcnu++;
				ctdci[ctdcnu]	=	chn(l_word);
				ctdc[ctdcnu]	=	dta(l_word) ;
			}
			if(nbox == 11){						/* Camac TDC2 ch 0-7*/
				ctdcnu++;
				ctdci[ctdcnu]	=	chn(l_word) + 8;
				ctdc[ctdcnu]	=	dta(l_word);
			}
			if(nbox == 12){						/* Camac TDC3 ch 0-7*/
				ctdcnu++;
				ctdci[ctdcnu]	=	chn(l_word) + 16;
				ctdc[ctdcnu]	=	dta(l_word);
			}
			if(nbox == 13){						/* Camac TDC4 ch 0-7*/
				ctdcnu++;
				ctdci[ctdcnu]	=	chn(l_word) + 24;
				ctdc[ctdcnu]	=	dta(l_word);
			}
			if(nbox == 14){						/* Camac Pile-up ch 0-1*/
				punu++;
				pui[punu]		=	chn(l_word);
				pu[punu]		=	dta(l_word);
			}
#endif
		}
		words = words + evlen;
		
/*	======================================================	*/
/*  Now, the event is formatted and ready to be sorted		*/
/*	======================================================	*/
		good = reduc_winsort( pesp, pdesp, pedesp);					/* Gate on particles, returns 0 of OK */
		++events;
		if (good == 0 ){											/* Good event, save it */
			++gevents;												/* No of good events */
			evfrac = (gevents * 100) / events;
			*(messp + 14) = (int) evfrac;							/* Percent events accepted */
			if (outlength > bufferlength - evlen) { 
				writestatus = write_exa(outp, messp);				/* Write databuffer to target device */
				if (writestatus == -1){								/* Error in write, terminate session */
					writestat = -1;
					printf("Error in write, terminate session = -1\n");
					goto L999;
				} 
				++grec;												/* Increment "good" buffers */
				*(messp + 15) = grec;								/* Number of accepted buffers */
				outlength = 0;										/* Reset local output buffer pointer */
				for (i = 0; i < bufferlength; ++i){					/* Zeroing local output buffer */
					*(outp + i) = 0;
				}
			}
		}
		
/* ********************************************************************	*/
/* Shift the E detector with the calculated shift. The event has one	*/
/* and only one particle event word for E, located in iE_counter			*/
/* ******************************************************************** */
		p[pnu] = (int)((float)p[pnu] + final_edeshift[pi[pnu]] + r + 0.5);
		if(p[pnu] < 0 | p[pnu] > 2047) p[pnu] = 0;
		
/* ********************************************* */
/* Replace the event word for E with new energy	 */
/* First we zero the energy, then add the energy */
/* ********************************************* */
        rawevent[iE_counter] = rawevent[iE_counter] & 0xffff0000;
        rawevent[iE_counter] = rawevent[iE_counter] + p[pnu];

/* **************************************************** */
/* Store E-counter gain-stabilized event in good Buffer */
/* **************************************************** */
		for (i = 0; i < evlen; ++i){					
            *(outp + outlength + i) = rawevent[i];
		}
		outlength = outlength + evlen;
	}													/* End of good event block */
														/* End-Of-Event Loop, fetch next event */
	L999:
	return outlength;
}
