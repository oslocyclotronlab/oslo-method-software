#include	<stdio.h>
#include	<strings.h>

#define AM32 (0x09)
#define AM24 (0x39)

#define SIRINXBUF      0xF0F0400d    /*CSR, next buffer to be read by VME     */
#define SIRINUBUF      0xF0F0400c    /*CSR, number of buffers in ring (1 - 32)*/
#define SIRIEVRDY      0xF0F04009    /*CSR, event has been read by ROCO       */
#define SIRIEVEAD      0xF0F04005    /*CSR, address of SIRI event             */
#define SIRIBUFAD      0xF0F04004    /*CSR, address of SIRI buffer            */
#define SIRICOST       0xF0F04000    /*CSR, main control and status register  */
#define SIRIDSRaddr    0xF0F00000    /*DSR, base addr for event data words    */

unsigned char   *pSIRICOST, *pSIRIBUFAD, *pSIRIEVEAD;
unsigned char  *pSIRIEVRDY, *pSIRINUBUF, *pSIRINXBUF;
unsigned short SIRIDSR[64], *pSIRIDSR;

void main(void)
{
   unsigned short dts;
   unsigned char dtc;
   int i;
   printf(" ____________________________________________________________ \r\n");
   printf("|                                                            |\r\n");
   printf("|                       Peeksiri 1.0                         |\r\n");
   printf("|                                                            |\r\n");
   printf("|  Testing 100000 times various VME-addresses used by siri,  |\r\n");
   printf("|     based on the vme_map function described and used       |\r\n");
   printf("|      in the C-programs vpeek and vpoke found in the        |\r\n");
   printf("|        directory /usr/ces/examples/vlib of bobcat          |\r\n");
   printf("|                                                            |\r\n");
   printf("| E-mail  : magne.guttormsen@fys.uio.no                      |\r\n");
   printf("| Created : 11-09-1998                                       |\r\n");
   printf("| Modified: 16-09-1998                                       |\r\n");
   printf("|____________________________________________________________|\r\n");
   printf("                                                              \r\n");

   pSIRICOST = (unsigned char *)vme_map(SIRICOST,sizeof(unsigned char),AM24);
   if (pSIRICOST == (unsigned char *)0){
      fprintf(stderr,"unable to map VME address for SIRICOST register\n");
      exit(0);
   }
   dtc = *pSIRICOST;
   printf("AD=0x%08x, AM=0x%02x: data read: %8x\n",SIRICOST,AM24,dtc);

   pSIRIBUFAD = (unsigned char *)vme_map(SIRIBUFAD,sizeof(unsigned char),AM24);
   if (pSIRIBUFAD == (unsigned char *)0){
      fprintf(stderr,"unable to map VME address for SIRIBUFAD register\n");
      exit(0);
   }
   dtc = *pSIRIBUFAD;
   printf("AD=0x%08x, AM=0x%02x: data read: %8x\n",SIRIBUFAD,AM24,dtc);

   pSIRIEVEAD = (unsigned char *)vme_map(SIRIEVEAD,sizeof(unsigned char),AM24);
   if (pSIRIEVEAD == (unsigned char *)0){
      fprintf(stderr,"unable to map VME address for SIRIEVEAD register\n");
      exit(0);
   }
   dtc = *pSIRIEVEAD;
   printf("AD=0x%08x, AM=0x%02x: data read: %8x\n",SIRIEVEAD,AM24,dtc);

   pSIRIEVRDY = (unsigned char *)vme_map(SIRIEVRDY,sizeof(unsigned char),AM24);
   if (pSIRIEVRDY == (unsigned char *)0){
      fprintf(stderr,"unable to map VME address for SIRIEVRDY register\n");
      exit(0);
   }
   dtc = *pSIRIEVRDY;
   printf("AD=0x%08x, AM=0x%02x: data read: %8x\n",SIRIEVRDY,AM24,dtc);

   pSIRINUBUF = (unsigned char *)vme_map(SIRINUBUF,sizeof(unsigned char),AM24);
   if (pSIRINUBUF == (unsigned char *)0){
      fprintf(stderr,"unable to map VME address for SIRINUBUF register\n");
      exit(0);
   }
   dtc = *pSIRINUBUF;
   printf("AD=0x%08x, AM=0x%02x: data read: %8x\n",SIRINUBUF,AM24,dtc);

   pSIRINXBUF = (unsigned char *)vme_map(SIRINXBUF,sizeof(unsigned char),AM24);
   if (pSIRINXBUF == (unsigned char *)0){
      fprintf(stderr,"unable to map VME address for SIRINXBUF register\n");
      exit(0);
   }
   dtc = *pSIRINXBUF;
   printf("AD=0x%08x, AM=0x%02x: data read: %8x\n",SIRINXBUF,AM24,dtc);

                    /*Pointers to DSR register (8 kb x 16 bits)               */
                    /*Only first buffer of interrest (no ring buffers assumed)*/
                    /*256 words, each 16 bit long, starts at 0xf0f0'0000      */
   pSIRIDSR = (unsigned short *)vme_map(SIRIDSRaddr,64*sizeof(unsigned short),AM24);
   if (pSIRIDSR == (unsigned short *)0){
      fprintf(stderr,"unable to map VME address for SIRIs DSR\n");
      exit(0);
   }
   for(i = 0; i < 64; i++){
     /* SIRIDSR[i] = *SIRIDSR;*/
      printf("AD=0x%08x, AM=0x%02x: data read:%8x\n",SIRIDSRaddr+i,AM24, SIRIDSR[i]);
   }

  printf("A 100000-times loop started, look at the lamps!!!\n");
  for(i=1; i < 100000; i++){
     dtc = *pSIRICOST;
     dtc = *pSIRIBUFAD;
     dtc = *pSIRIEVEAD;
     dts = *pSIRIEVRDY;
     dts = *pSIRINUBUF;
     dtc = *pSIRINXBUF;
     for(i=0; i<64; i++){
        SIRIDSR[i] = *SIRIDSR;
     }
  }

   vme_rel(pSIRICOST, sizeof(unsigned char));
   vme_rel(pSIRIBUFAD,sizeof(unsigned char));
   vme_rel(pSIRIEVEAD,sizeof(unsigned char));
   vme_rel(pSIRIEVRDY,sizeof(unsigned char));
   vme_rel(pSIRINUBUF,sizeof(unsigned char));
   vme_rel(pSIRINXBUF,sizeof(unsigned char));
   vme_rel(pSIRIDSR,64*sizeof(unsigned short));
}
