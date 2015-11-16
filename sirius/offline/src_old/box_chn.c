#include <stdio.h>
#include <stdlib.h>
#include <gsdims.h>

int box_chn(int box, int chn)
/* Gets the data word from (box, chn). Some	boxes	*/
/* are connected by increasing channel numbers		*/
/* as e.g. box 6-9: box 9 has channels 24-31		*/

{
	int i;
		if(box == 0){						
			for (i = 0; i <= nanu; i++){
				if(nai[i] == chn){
					return na[i];
				}
			}
		}
		if(box == 1){						
			for (i = 0; i <= tnanu; i++){
				if(tnai[i] == chn){
					return tna[i];
				}
			}
		}
		if(box > 1 && box < 4){						
			for (i = 0; i <= dpnu; i++){
				if(dpi[i] == chn){			
					return dp[i];
				}
			}
		}
		if(box == 4){						
			for (i = 0; i <= pnu; i++){
				if(pi[i] == chn){			/* Only 8 E counters in ch 0, 2,..., 16	*/
					return p[i];
				}
			}
		}
		if(box == 5){						
			for (i = 0; i <= nimnu; i++){	/* Pattern ch 0 - 3, NIM ADCs ch 4 - 15	*/
				if(nimi[i] == chn){			/* Wall-clock time (32 bits) in ch 16 (LSB) and ch 17(MSB)	*/
					return nim[i];
				}
			}
		}
		if(box > 5 && box < 10){						
			for (i = 0; i <= cadcnu; i++){
				if(cadci[i] == chn){
					return cadc[i];
				}
			}
		}
		if(box > 9 && box < 14){						
			for (i = 0; i <= ctdcnu; i++){
				if(ctdci[i] == chn){
					return ctdc[i];
				}
			}
		}
		if(box == 14){						
			for (i = 0; i <= punu; i++){
				if(pui[i] == chn){
					return pu[i];
				}
			}
		}
		if(box == 15){						
			for (i = 0; i <= scnu; i++){	/* The scaler values (32 bits) are composed of two 16-bit words	*/
				if(sci[i] == chn){			/* 16 LSB are in scaler-chs 0-15, 16 MSB are in chs 16-31		*/
					return sc[i];
				}
			}
		}
		return 0;
}
