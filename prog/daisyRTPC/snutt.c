#include <stdio.h>
#include <math.h>
#include <strings.h>

#define AM24 0x39
#define AM32 0x09

#define BOOL(x)   (!(!(x)))
#define bit(a,x)  BOOL((a) & (1L << (x)))

#define SIRINXBUF 0xF0F0400d  /*CSR, next buffer to be read by VME*/
#define SIRINUBUF 0xF0F0400c  /*CSR, number of buffers in ring buffer (1 - 32)*/
#define SIRIEVRDY 0xF0F04009  /*CSR, event has been read by ROCO*/
#define SIRIEVEAD 0xF0F04005  /*CSR, address of SIRI event*/
#define SIRIBUFAD 0xF0F04004  /*CSR, address of SIRI buffer*/
#define SIRICOST  0xF0F04000  /*CSR, main control and status register*/
#define SIRIDSR   0xF0F00000  /*DSR, start of event words*/


unsigned char Tpara, Spara;
unsigned char *pT1NUMREG,*pNEXTREG;
int SIRIpat1, SIRIpat2, SIRIpat3, SIRIpat4;
int i, j, imax, k, chipadi, buf32[100];
int evno[128], chipad[128], energy[128];
int dE[128], E[128]; /*maximum 128 telescopes, present only 64 used*/
int p1;              /*the number of words written on tape (buf32(p1))*/
int one = 1L;        /*used for making bit-pattern by shift (<<) command*/
unsigned char *pSIRICOST, *pSIRIBUFAD, *pSIRIEVEAD, *pSIRIEVRDY, *pSIRINUBUF, *pSIRINXBUF;
unsigned short *pSIRIDSR;
void SIRIopen();
void SIRIconfig();
void SIRIclose();


void main(){
   SIRIopen();
   SIRIconfig();

   /*++++++++++++++++++++++++++++++++++++++++++++++++++*/
   /*     Infinite main readout loop starts here       */
   /*++++++++++++++++++++++++++++++++++++++++++++++++++*/
   for(;;){
      nextevent:
      *pNEXTREG   = Tpara; /*Enable TPUs for next event*/
      *pSIRIEVRDY = 0x01;  /*Enable SIRI for next event*/
      notdetected:
      Tpara = *pT1NUMREG;
      Spara = *pSIRICOST;
      if (bit(Tpara, 7) == 1) goto detected;
      if (bit(Spara, 7) == 1) goto detected;
      goto notdetected;
      detected:
     
      if ( bit(Spara, 7) == 1){
      	 pSIRIDSR[0] = *pSIRIDSR;
         if(bit(pSIRIDSR[0], 7) == 1) goto nextevent;
         evno[0]   = pSIRIDSR[0] & 0xff00;
         chipad[0] = pSIRIDSR[0] & 0x007f;
         energy[0] = pSIRIDSR[1];
         for(i = 1; i < 128; i++){
            j = 2 * i;
            evno[i]     = pSIRIDSR[j] & 0xff00;
            if(evno[i] != evno[0])      goto eventfinnished;
            if(bit(pSIRIDSR[j],7) == 1) goto eventfinnished;
            chipad[i]   = pSIRIDSR[j] & 0x007f;
            energy[i]   = pSIRIDSR[j + 1];
         }
         eventfinnished:
         imax = i;

         /*Making bit patterns. Assumes that The E-detectors are*/
         /*always 32 channels above dE detectors                        */
         SIRIpat1=0;
         SIRIpat2=0;
         SIRIpat3=0;
         SIRIpat4=0;

         for(i = 0; i < imax; i++){
            for(j = 0; j < 32; j++){
               chipadi = chipad[i];
               if(chipadi < 32){
                  if(j == chipadi){
                     dE[j] = energy[i];
                     SIRIpat1 = SIRIpat1 | (one << j);
                     goto identified;
                  } 
               }
               if(chipadi < 64){
                  if(j == chipadi - 32){
                     E[j] = energy[i];
                     SIRIpat1 = SIRIpat1 | (one << j);
                     goto identified;
                  } 
               }
               if(chipadi < 96){
                  if(j == chipadi - 64){
                     dE[j + 32] = energy[i];
                     SIRIpat2 = SIRIpat2 | (one << j);
                     goto identified;
                  } 
               }
               if(chipadi < 128){
                  if(j == chipadi - 96){
                     E[j + 32] = energy[i];
                     SIRIpat2 = SIRIpat2 | (one << j);
                     goto identified;
                  } 
               }
            }
            identified:
            continue;
         }
         buf32[p1] = 0x800E;       /*defining TPU5 (SIRI-detector)*/
         p1        = p1 + 1;
         buf32[p1] = SIRIpat1;     /*pattern 1 for 32 first dE-E events*/
         p1        = p1 + 1;
         buf32[p1] = SIRIpat2;     /*pattern 2 for 32 next telescopes*/
         p1        = p1 + 1;
         buf32[p1] = SIRIpat3;     /*pattern 3 and 4 for future use*/
         p1        = p1 + 1;       /*written on tape, but not used */
         buf32[p1] = SIRIpat4;
         p1        = p1 + 1;

         for(i = 0; i < 32; i++){
            if( bit(SIRIpat1, i) == 1){ /*bit i is set, dE and E is written*/
               buf32[p1] = dE[i];
               p1        = p1 + 1;
               buf32[p1] = E[i];
               p1        = p1 + 1;
               dE[i]     = 0;
               E[i]      = 0;
            }
         }
         for(i = 32; i < 64; i++){
            if( bit(SIRIpat2, (i - 32)) == 1){
               buf32[p1] = dE[i];
               p1        = p1 + 1;
               buf32[p1] = E[i];
               p1        = p1 + 1;
               dE[i]     = 0;
               E[i]      = 0;
            }
         } 
/* to be used for future expension with 128 dE-E telescopes            
         for(i = 64; i < 96; i++){
           if( bit(SIRIpat3, (i - 64)) == 1){
               buf32[p1] = dE[i];
               p1        = p1 + 1;
               buf32[p1] = E[i];
               p1        = p1 + 1;
               dE[i]     = 0;
               E[i]      = 0;
            }
         }
         for(i = 96; i < 128; i++){
           if( bit(SIRIpat4, (i - 96)) == 1){
               buf32[p1] = dE[i];
               p1        = p1 + 1;
               buf32[p1] = E[i];
               p1        = p1 + 1;
               dE[i]     = 0;
               E[i]      = 0;
            }
         }
*/         
         
      }  
      *pSIRIEVRDY=0x01; /*telling that eventbuilder is finnished*/
   }
   SIRIclose();
}

void SIRIopen(){
   /************************************************/
   /* Set up of address pointers for CSR and DSR   */
   /* Some of the pointers are necessarily not in  */
   /* use (active) for this acquisition program    */
   /************************************************/
   unsigned short dts;
   unsigned char  dtc;

   /* Pointers to CSR register (8 kb x 16 bits)       */
   /* Only a few locations of CSR are used            */
   /* 6 words, each 8 bits long, starts at 0xf0f0'4000*/

   pSIRICOST = (unsigned char *)vme_map(SIRICOST,sizeof(unsigned char),AM24);
   if (pSIRICOST == ( unsigned char *)0){
      fprintf(stderr,"unable to map VME address for SIRICOST register\n");
      exit(0);
   }
   dtc = *pSIRICOST;
   printf("AD=0x%08x, AM=0x%02x: data read: %8x\n",SIRICOST,AM24,dtc);

   pSIRIBUFAD = (unsigned char *)vme_map(SIRIBUFAD,sizeof(unsigned char),AM24);
   if (pSIRIBUFAD == ( unsigned char *)0){
      fprintf(stderr,"unable to map VME address for SIRIBUFAD register\n");
      exit(0);
   }
   dtc = *pSIRIBUFAD;
   printf("AD=0x%08x, AM=0x%02x: data read: %8x\n",SIRIBUFAD,AM24,dtc);

   pSIRIEVEAD = (unsigned char *)vme_map(SIRIEVEAD,sizeof(unsigned char),AM24);
   if (pSIRIEVEAD == ( unsigned char *)0){
      fprintf(stderr,"unable to map VME address for SIRIEVEAD register\n");
      exit(0);
   }
   dtc = *pSIRIEVEAD;
   printf("AD=0x%08x, AM=0x%02x: data read: %8x\n",SIRIEVEAD,AM24,dtc);

   pSIRIEVRDY = (unsigned char *)vme_map(SIRIEVRDY,sizeof(unsigned char),AM24);
   if (pSIRIEVRDY == ( unsigned char *)0){
      fprintf(stderr,"unable to map VME address for SIRIEVRDY register\n");
      exit(0);
   }
   dtc = *pSIRIEVRDY;
   printf("AD=0x%08x, AM=0x%02x: data read: %8x\n",SIRIEVRDY,AM24,dtc);

   pSIRINUBUF = (unsigned char *)vme_map(SIRINUBUF,sizeof(unsigned char),AM24);
   if (pSIRINUBUF == ( unsigned char *)0){
      fprintf(stderr,"unable to map VME address for SIRINUBUF register\n");
      exit(0);
   }
   dtc = *pSIRINUBUF;
   printf("AD=0x%08x, AM=0x%02x: data read: %8x\n",SIRINUBUF,AM24,dtc);

   pSIRINXBUF = (unsigned char *)vme_map(SIRINXBUF,sizeof(unsigned char),AM24);
   if (pSIRINXBUF == ( unsigned char *)0){
      fprintf(stderr,"unable to map VME address for SIRINXBUF register\n");
      exit(0);
   }
   dtc = *pSIRINXBUF;
   printf("AD=0x%08x, AM=0x%02x: data read: %8x\n",SIRINXBUF,AM24,dtc);


   /* Pointers to DSR register (8 kb x 16 bits)               */
   /* Only first buffer of interrest (no ring buffers assumed)*/
   /* 256 words, each 16 bit long, starts at 0xf0f0'0000      */
   pSIRIDSR = (unsigned short *)vme_map(SIRIDSR,256*sizeof(unsigned short),AM24);
   if (pSIRIDSR == (unsigned short *)0){
      fprintf(stderr,"unable to map VME address for SIRIs DSR\n");
      exit(0);
   }
   for(i = 0; i < 256; i++){
      pSIRIDSR[i] = *pSIRIDSR;
      printf("AD=0x%08x, AM=0x%02x: data read:%8x\n",SIRIDSR+i,AM24,pSIRIDSR[i]);
   }
   return;
}

void SIRIconfig(){
   /***************************************************/
   /* The present SIRI configuration is set up for one*/
   /* ring buffer - and without interrupt handling    */
   /***************************************************/
   *pSIRINXBUF = 0x00;  /*Next buffer to be read by VME*/
   *pSIRINUBUF = 0x01;  /*Number of buffers in ring buffer (1 - 32)*/
   *pSIRIEVRDY = 0x00;  /*Event has been read by ROCO*/
   *pSIRIEVEAD = 0x00;  /*Address of SIRI event*/
   *pSIRIBUFAD = 0x00;  /*Address of SIRI buffer*/
   *pSIRICOST  = 0x81;  /*Main control and status register COST*/
   for(i = 0; i < 128; i++){
      dE[i] = 0;
      E[i]  = 0;
   }
   return;
}

void SIRIclose (){
   vme_rel(pSIRICOST, sizeof(unsigned char));
   vme_rel(pSIRIBUFAD,sizeof(unsigned char));
   vme_rel(pSIRIEVEAD,sizeof(unsigned char));
   vme_rel(pSIRIEVRDY,sizeof(unsigned char));
   vme_rel(pSIRINUBUF,sizeof(unsigned char));
   vme_rel(pSIRINXBUF,sizeof(unsigned char));
   vme_rel(pSIRIDSR,256*sizeof(unsigned short));
   return;
}
