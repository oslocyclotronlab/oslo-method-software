#include	<stdio.h>
#include	<strings.h>

#define AM32 (0x09)
#define AM24 (0x39)

#define SIRINUBUF 0xf0f04007  /*CSR, number of buffers in ring buffer (1 - 32)*/
#define SIRINXBUF 0xf0f04006  /*CSR, next buffer to be read by VME*/
#define SIRIIRQVC 0xf0f04005  /*CSR, interrupt vector register*/
#define SIRIEVRDY 0xf0f04004  /*CSR, event has been read by ROCO*/
#define SIRIBUFAD 0xf0f04003  /*CSR, address of SIRI buffer*/
#define SIRIEVEAD 0xf0f04002  /*CSR, address of SIRI event*/
#define SIRICOST  0xf0f04001  /*CSR, main control and status register*/
#define SIRICLR   0xf0f04000  /*CSR, clear command register*/
#define SIRIDSR   0xf0f00000  /*DSR, start of event words*/

unsigned char *pSIRICLR, *pSIRICOST, *pSIRIEVEAD, *pSIRIBUFAD, *pSIRIEVRDY, *pSIRIIRQVC, *pSIRINXBUF, *pSIRINUBUF;
unsigned short *pSIRIDSR, SIRIadc[64];

void main(void)
{
   unsigned short dts;
   unsigned char dtc;
   int i, j;
   printf(" ____________________________________________________________ \r\n");
   printf("|                                                            |\r\n");
   printf("|                       Peeksiri 1.1                         |\r\n");
   printf("|                                                            |\r\n");
   printf("|  Testing 100000 times various VME-addresses used by siri,  |\r\n");
   printf("|     based on the vme_map function described and used       |\r\n");
   printf("|      in the C-programs vpeek and vpoke found in the        |\r\n");
   printf("|        directory /usr/ces/examples/vlib of bobcat          |\r\n");
   printf("|                                                            |\r\n");
   printf("| E-mail  : magne.guttormsen@fys.uio.no                      |\r\n");
   printf("| Created : 11-09-1998                                       |\r\n");
   printf("| Modified: 16-09-1998, 20-09-1998                           |\r\n");
   printf("|____________________________________________________________|\r\n");
   printf("                                                              \r\n");

   printf("SIRI Control and Status Register:\n");
   pSIRICLR = (unsigned char *)vme_map(SIRICLR,sizeof(unsigned char),AM32);
   if (pSIRICLR == (unsigned char *)0){
      fprintf(stderr,"unable to map VME address for SIRICLR register\n");
      exit(0);
   }
   dtc = *pSIRICLR;
   printf("SIRICLR    AD=0x%08x, AM=0x%02x: data read: %8x\n",SIRICLR,AM32,dtc);

   pSIRICOST = (unsigned char *)vme_map(SIRICOST,sizeof(unsigned char),AM32);
   if (pSIRICOST == (unsigned char *)0){
      fprintf(stderr,"unable to map VME address for SIRICOST register\n");
      exit(0);
   }
   dtc = *pSIRICOST;
   printf("SIRICOST   AD=0x%08x, AM=0x%02x: data read: %8x\n",SIRICOST,AM32,dtc);

   pSIRIEVEAD = (unsigned char *)vme_map(SIRIEVEAD,sizeof(unsigned char),AM32);
   if (pSIRIEVEAD == (unsigned char *)0){
      fprintf(stderr,"unable to map VME address for SIRIEVEAD register\n");
      exit(0);
   }
   dtc = *pSIRIEVEAD;
   printf("SIRIEVEAD  AD=0x%08x, AM=0x%02x: data read: %8x\n",SIRIEVEAD,AM32,dtc);

   pSIRIBUFAD = (unsigned char *)vme_map(SIRIBUFAD,sizeof(unsigned char),AM32);
   if (pSIRIBUFAD == (unsigned char *)0){
      fprintf(stderr,"unable to map VME address for SIRIBUFAD register\n");
      exit(0);
   }
   dtc = *pSIRIBUFAD;
   printf("SIRIBUFAD  AD=0x%08x, AM=0x%02x: data read: %8x\n",SIRIBUFAD,AM32,dtc);

   pSIRIEVRDY = (unsigned char *)vme_map(SIRIEVRDY,sizeof(unsigned char),AM32);
   if (pSIRIEVRDY == (unsigned char *)0){
      fprintf(stderr,"unable to map VME address for SIRIEVRDY register\n");
      exit(0);
   }
   dtc = *pSIRIEVRDY;
   printf("SIRIEVRDY  AD=0x%08x, AM=0x%02x: data read: %8x\n",SIRIEVRDY,AM32,dtc);

   pSIRIIRQVC = (unsigned char *)vme_map(SIRIIRQVC,sizeof(unsigned char),AM32);
   if (pSIRIIRQVC == (unsigned char *)0){
      fprintf(stderr,"unable to map VME address for SIRIIRQVC register\n");
      exit(0);
   }
   dtc = * pSIRIIRQVC;
   printf("SIRIIRQVC  AD=0x%08x, AM=0x%02x: data read: %8x\n", SIRIIRQVC,AM32,dtc);

   pSIRINXBUF = (unsigned char *)vme_map(SIRINXBUF,sizeof(unsigned char),AM32);
   if (pSIRINXBUF == (unsigned char *)0){
      fprintf(stderr,"unable to map VME address for SIRINXBUF register\n");
      exit(0);
   }
   dtc = *pSIRINXBUF;
   printf("SIRINXBUF  AD=0x%08x, AM=0x%02x: data read: %8x\n",SIRINXBUF,AM32,dtc);

   pSIRINUBUF = (unsigned char *)vme_map(SIRINUBUF,sizeof(unsigned char),AM32);
   if (pSIRINUBUF == (unsigned char *)0){
      fprintf(stderr,"unable to map VME address for SIRINUBUF register\n");
      exit(0);
   }
   dtc = *pSIRINUBUF;
   printf("SIRINUBUF  AD=0x%08x, AM=0x%02x: data read: %8x\n",SIRINUBUF,AM32,dtc);

               /*Pointers to DSR register (8 kb x 16 bits)               */
               /*Only first buffer of interrest (no ring buffers assumed)*/
               /*256 words, each 16 bit long, starts at 0xf0f0'0000      */

   printf("\n");
   printf("SIRI Patterns and ADCs:\n");
   pSIRIDSR = (unsigned short *)vme_map(SIRIDSR,64*sizeof(unsigned short),AM32);
   if (pSIRIDSR == (unsigned short *)0){
      fprintf(stderr,"unable to map VME address for SIRIs DSR\n");
      exit(0);
   }
   for(i = 0; i < 32; i++){
      SIRIadc[2*i] = *(pSIRIDSR+2*i); 
      printf("Pattern %2d, AD=0x%08x, AM=0x%02x: data read: %8x\n",i,SIRIDSR+2*(2*i),AM32, SIRIadc[2*i]);
SIRIadc[2*i+1] = *(pSIRIDSR+2*i+1); 
      printf("ADC  no %2d, AD=0x%08x, AM=0x%02x: data read: %8x\n",i,SIRIDSR+2*(2*i+1),AM32, SIRIadc[2*i+1]);

   }

  printf("A 100000-times loop started, look at the lamps!!!\n");
  for(i=1; i < 100000; i++){
     dtc = *pSIRICOST;
     dtc = *pSIRIBUFAD;
     dtc = *pSIRIEVEAD;
     dts = *pSIRIEVRDY;
     dts = *pSIRINUBUF;
     dtc = *pSIRINXBUF;
     for(j=0; j<64; j++){
        SIRIadc[j] = *(pSIRIDSR+j);
     }
  }
   vme_rel(pSIRICLR,  sizeof(unsigned char));
   vme_rel(pSIRICOST, sizeof(unsigned char));
   vme_rel(pSIRIEVEAD,sizeof(unsigned char));
   vme_rel(pSIRIBUFAD,sizeof(unsigned char));
   vme_rel(pSIRIEVRDY,sizeof(unsigned char));
   vme_rel(pSIRIIRQVC,sizeof(unsigned char));
   vme_rel(pSIRINXBUF,sizeof(unsigned char));
   vme_rel(pSIRINUBUF,sizeof(unsigned char));
   vme_rel(pSIRIDSR,64*sizeof(unsigned short));
}
