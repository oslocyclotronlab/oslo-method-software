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
#define SAFE    256
#define EOB     0xe0000000

/* ============================================================	*/
/*  Purpose : Sort event by event into vectors to be			*/
/*            used in the user_subroutine						*/
/*																*/
/*  INPUT  via parameter : *bufp : integer						*/
/*                         message_box : integer				*/
/*  OUTPUT via parameter : errstat : integer					*/
/*                          = -1  <=> buffer error				*/
/*  =========================================================== */

int shift_sort( int *pesp, int *pdesp, int *pedesp);

int shift_format(int *bufp, int *pesp, int *pdesp, int *pedesp)
{
	int		i, bufferlength, errstat, sortfrac;
	int		l_word;
	int		words, nword, nboe, nbox;
	bufferlength	= 32768;
	errstat			= 0;										/*	Initialisering */
	words			= 0;

	while (words < bufferlength) {								/* Go through buffer */
		dpnu	= -1;											/* Reset numbers */
		pnu		= -1;             

/*	**************************************************************	*/
/*	Testing header word of event									*/
/*	No more data in buffer if header = 0, return from function		*/
/*	If this should occur too early (partly filled buffer), it may	*/
/*	be a serious problem. Mark such a buffer as bad buffer.			*/
/*	**************************************************************	*/
		l_word	= *(bufp + words);
		nboe	= boe(l_word);
		if(nboe == 0xc){
			nword = ndw(l_word);								/* Number of words (ADCs) with data */
		}else{
		    if(l_word != EOB ) errstat = -1;
			goto L999;											/* Drop current buffer */
		}

/* **************************************** */
/*	Looping through all words (ADCs) and	*/
/*	storing in respective vectors			*/
/* **************************************** */
		for (i = 1; i <= nword; i++){
			l_word	= *(bufp + i + words);
			nbox	= box(l_word);				/* The box that has fired */
			if(nbox == 2){						/* Energy dE ch 0-31 */
				dpnu++;
				dpi[dpnu]		=	chn(l_word);
				dp[dpnu]		=	dta(l_word);
			}
			if(nbox == 3){						/* Energy dE ch 32-61 */
				dpnu++;
				dpi[dpnu]		=	chn(l_word) + 32;
				dp[dpnu]		=	dta(l_word);
			}
			if(nbox == 4){						/* Energy E ch 0-32		*/
				pnu++;							/* Only 8 E counters in */
				pi[pnu]			=	chn(l_word);/* ch 0, 2,..., 16		*/
				p[pnu]			=	dta(l_word);
			}
		}
		shift_sort( pesp, pdesp, pedesp);
	}
	L999:
	return errstat;
}
