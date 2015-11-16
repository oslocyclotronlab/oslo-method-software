#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>    
#include <math.h>
#include <gsdims.h>

#define boe(x)	((x&0xf0000000)>>28)
#define ndw(x)	(x&0x000000ff)
#define box(x)	((x&0x3f800000)>>23)
#define chn(x)	((x&0x007f0000)>>16)
#define dta(x)	(x&0x0000ffff)
#define EOB     0x80000000

/* ============================================================	*/
/*  Purpose : Sort event by event into vectors to be			*/
/*            used in the user_subroutine						*/
/*																*/
/*  INPUT  via parameter : *bufp : integer						*/
/*                         message_box : integer				*/
/*  OUTPUT via parameter : errstat : integer					*/
/*                          = -1  <=> buffer error				*/
/*  =========================================================== */
	int user_routine( int *psingles, int *pesp, int *pdesp,int *pedesp, int *pthicksp, 
		int *pgesp, int *ptgesp, int *pnasp, int *ptnasp, int *pansp, int *pagsp, int *pmtsp);

	int format_event(int *bufp, int *messp, int *psingles, int *pesp, int *pdesp,int *pedesp, int *pthicksp, 
		int *pgesp, int *ptgesp, int *pnasp, int *ptnasp, int *pansp, int *pagsp, int *pmtsp)
{
	int		i, bufferlength, errstat;
	int		l_word;
	int		words, nword, nboe, nbox;
	
	bufferlength	= 32768;
	errstat			= 0;										/*	Initialisering */
	words			= 0;

	while (words < bufferlength) {								/* Go through buffer */
		dpnu	= -1;											/* Reset numbers */
		pnu		= -1;             
		nanu	= -1;
		tnanu	= -1;
		nimnu	= -1;
		cadcnu	= -1;
		ctdcnu	= -1;
		punu	= -1;
		scnu	= -1;
		
/*	**************************************************************	*/
/*	Testing header word of event									*/
/*	No more data in buffer if header = 0, return from function		*/
/*	If this should occur too early (partly filled buffer), it may	*/
/*	be a serious problem. Mark such a buffer as bad buffer.			*/
/*	**************************************************************	*/
		l_word	= *(bufp + words);
		nboe	= boe(l_word);
		if(nboe == 0xc){										/* Begin of event */
			nword = ndw(l_word);								/* Number of words (ADCs) with data */
		}else{
			if(l_word != EOB ) errstat = -1;					/* End of buffer */
			goto L999;											/* Drop current buffer */
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
			if(nbox == 0x20 || nbox==0x24){			/* Energy of NaI ch 0-31*/
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
		user_routine(psingles, pesp, pdesp, pedesp, pthicksp, pgesp, ptgesp, pnasp, ptnasp, pansp, pagsp, pmtsp);
		words = words + nword + 1;
	}
	L999:
	return errstat;
}
