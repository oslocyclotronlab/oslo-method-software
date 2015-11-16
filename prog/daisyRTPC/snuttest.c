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
int p1,kk;              /*the number of words written on tape (buf32(p1))*/
int one = 1L;        /*used for making bit-pattern by shift (<<) command*/
unsigned char *pSIRICOST, *pSIRIBUFAD, *pSIRIEVEAD, *pSIRIEVRDY, *pSIRINUBUF, *pSIRINXBUF;
unsigned short pSIRIDSR[20];
void SIRIconfig();


void main(){
   SIRIconfig();

   /*++++++++++++++++++++++++++++++++++++++++++++++++++*/
   /*     Infinite main readout loop starts here       */
   /*++++++++++++++++++++++++++++++++++++++++++++++++++*/
   for(kk=0;kk < 1;kk++){
   	  p1=0;
      nextevent:
      *pNEXTREG   = 0x01; /*Enable TPUs for next event*/
      *pSIRIEVRDY = 0x01;  /*Enable SIRI for next event*/
      notdetected:
      Tpara = *pT1NUMREG;
      Spara = 0x80;
      if (bit(Tpara, 7) == 1) goto detected;
      if (bit(Spara, 7) == 1) goto detected;
      goto notdetected;
      detected:
      if ( bit(Spara, 7) == 1){

      	 pSIRIDSR[0] = 0x943b;
      	 pSIRIDSR[1] = 0xecd;
      	 pSIRIDSR[2] = 0x945f;
      	 pSIRIDSR[3] = 0x248;
      	 pSIRIDSR[4] = 0x940b;
      	 pSIRIDSR[5] = 0xd03;
      	 pSIRIDSR[6] = 0x941a;
      	 pSIRIDSR[7] = 0x333;
      	 pSIRIDSR[8] = 0x933c;
      	 pSIRIDSR[9] = 0x933;

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
         	chipadi = chipad[i];
             printf("i :%d  chipad: %x \n",i,chipadi);

            for(j = 0; j < 32; j++){
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
      } 
      printf("exabyte TPU5 written:%x \n",buf32[0]);
      printf("exabyte patt1 written:%x \n",buf32[1]);
      printf("exabyte patt2 written:%x \n",buf32[2]);
      printf("exabyte patt3 written:%x \n",buf32[3]);
      printf("exabyte patt4 written:%x \n",buf32[4]);

      for(i = 4; i < 14; i++){
          printf("exabyte energy no :%d  written:%x \n",i-4,buf32[i]);
      }             
      *pSIRIEVRDY=0x01; /*telling that eventbuilder is finnished*/
   }
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

