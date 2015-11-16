#include	<stdio.h>
#include	<strings.h>

#define AM32 (0x09)
#define AM24 (0x39)

#define NEXTREG     0xF0FFFF09
#define T1STATUS    0xF0FFFF00  /*Registers in Master TPU #1*/
#define T1NUMREG    0xF0FFFF05
#define T1PATTERN   0xF0FFFF06
#define T2STATUS    0xF0FFFF10  /*Registers in Slave TPU  #2*/
#define T2NUMREG    0xF0FFFF15
#define T2PATTERN   0xF0FFFF16
#define T3STATUS    0xF0FFFF20  /*Registers in Slave TPU  #3*/
#define T3NUMREG    0xF0FFFF25
#define T3PATTERN   0xF0FFFF26
#define NIMCSR      0xF0EFFF9C  /*Control & Status register */
#define NIMPATTERN  0xF0EFFF9E  /*Pattern register          */
#define NIMbaseaddr 0xf0efffa0  /*baseaddress for NIM ADCs  */

unsigned short *pT1STATUS,  *pT2STATUS,  *pT3STATUS;
unsigned char  *pT1NUMREG,  *pT2NUMREG,  *pT3NUMREG;
unsigned short *pT1PATTERN, *pT2PATTERN, *pT3PATTERN;

unsigned short *pNIMCSR, *pNIMPATTERN, *pNIMadc, NIMadc[16];
unsigned char *pNEXTREG;

void main(void)
{
  unsigned short dts;
  unsigned char dtc;
  int i,j,inc=2;

  printf(" ____________________________________________________________ \r\n");
  printf("|                                                            |\r\n");
  printf("|                       Peekdaisy 1.2                        |\r\n");
  printf("|                                                            |\r\n");
  printf("|  Testing 100000 times various VME-addresses used by daisy  |\r\n");
  printf("|     based on the vme_map function described and used       |\r\n");
  printf("|      in the C-programs vpeek and vpoke found in the        |\r\n");
  printf("|        directory /usr/ces/examples/vlib of bobcat          |\r\n");
  printf("|                                                            |\r\n");
  printf("| E-mail  : magne.guttormsen@fys.uio.no                      |\r\n");
  printf("| Created : 15-02-1998                                       |\r\n");
  printf("| Modified: 11-09-1998                                       |\r\n");
  printf("|____________________________________________________________|\r\n");
  printf("                                                              \r\n");

  printf("TPU1:\n");
  pNEXTREG = (unsigned char*)vme_map(NEXTREG,sizeof(char),AM24);
  if (pNEXTREG == (unsigned char *)0){
     fprintf(stderr,"unable to map VME address\n");
     exit(0);
  }
  dtc = *pNEXTREG;
  printf("NEXTREG    AD=0x%08x, AM=0x%02x: data read: %8x\n",NEXTREG,AM24,dtc);

  pT1STATUS = (unsigned short *)vme_map(T1STATUS,sizeof(short),AM24);
  if (pT1STATUS == (unsigned short *)0){
     fprintf(stderr,"unable to map VME address\n");
     exit(0);
  }
  dts = *pT1STATUS;
  printf("T1STATUS   AD=0x%08x, AM=0x%02x: data read: %8x\n",T1STATUS,AM24,dts);

  pT1NUMREG = (unsigned char *)vme_map(T1NUMREG,sizeof(unsigned char),AM24);
  if (pT1NUMREG == ( unsigned char *)0){
     fprintf(stderr,"unable to map VME address\n");
     exit(0);
  }
  dtc = *pT1NUMREG;
  printf("T1NUMREG   AD=0x%08x, AM=0x%02x: data read: %8x\n",T1NUMREG,AM24,dtc);

  pT1PATTERN = (unsigned short *)vme_map(T1PATTERN,sizeof(short),AM24);
  if (pT1PATTERN == (unsigned short *)0){
     fprintf(stderr,"unable to map VME address\n");
     exit(0);
  }
  dts = *pT1PATTERN;
  printf("T1PATTERN  AD=0x%08x, AM=0x%02x: data read: %8x\n",T1PATTERN,AM24,dts);

  printf("\n");
  printf("TPU2:\n");
  pT2STATUS = (unsigned short *)vme_map(T2STATUS,sizeof(short),AM24);
  if (pT2STATUS == (unsigned short *)0){
     fprintf(stderr,"unable to map VME address\n");
     exit(0);
  }
  dts = *pT2STATUS;
  printf("T2STATUS   AD=0x%08x, AM=0x%02x: data read: %8x\n",T2STATUS,AM24,dts);

  pT2NUMREG = (unsigned char *)vme_map(T2NUMREG,sizeof(unsigned char),AM24);
  if (pT2NUMREG == (unsigned char *)0){
     fprintf(stderr,"unable to map VME address\n");
     exit(0);
  }
  dtc = *pT2NUMREG;
  printf("T2NUMREG   AD=0x%08x, AM=0x%02x: data read: %8x\n",T2NUMREG,AM24,dtc);

  pT2PATTERN = (unsigned short *)vme_map(T2PATTERN,sizeof(short),AM24);
  if (pT2PATTERN == (unsigned short *)0){
     fprintf(stderr,"unable to map VME address\n");
     exit(0);
  }
  dts = *pT2PATTERN;
  printf("T2PATTERN  AD=0x%08x, AM=0x%02x: data read: %8x\n",T2PATTERN,AM24,dts);

  printf("\n");
  printf("TPU3:\n");
  pT3STATUS = (unsigned short *)vme_map(T3STATUS,sizeof(short),AM24);
  if (pT3STATUS == (unsigned short *)0){
     fprintf(stderr,"unable to map VME address\n");
     exit(0);
  }
  dts = *pT3STATUS;
  printf("T3STATUS   AD=0x%08x, AM=0x%02x: data read: %8x\n",T3STATUS,AM24,dts);

  pT3NUMREG = (unsigned char *)vme_map(T3NUMREG,sizeof(unsigned char),AM24);
  if (pT3NUMREG == (unsigned char *)0){
     fprintf(stderr,"unable to map VME address\n");
     exit(0);
  }
  dtc = *pT3NUMREG;
  printf("T3NUMREG   AD=0x%08x, AM=0x%02x: data read: %8x\n",T3NUMREG,AM24,dtc);

  pT3PATTERN = (unsigned short *)vme_map(T3PATTERN,sizeof(short),AM24);
  if (pT3PATTERN == (unsigned short *)0){
     fprintf(stderr,"unable to map VME address\n");
     exit(0);
  }
  dts = *pT3PATTERN;
  printf("T3PATTERN  AD=0x%08x, AM=0x%02x: data read: %8x\n",T3PATTERN,AM24,dts);

  printf("\n");
  printf("NIM REGISTER:\n");
  pNIMCSR = (unsigned short *)vme_map(NIMCSR,sizeof(short),AM24);
  if (pNIMCSR == (unsigned short *)0){
     fprintf(stderr,"unable to map VME address\n");
     exit(0);
  }
  dts = *pNIMCSR;
  printf("NIMCSR     AD=0x%08x, AM=0x%02x: data read: %8x\n",NIMCSR,AM24,dts);

  pNIMPATTERN = (unsigned short *)vme_map(NIMPATTERN,sizeof(short),AM24);
  if (pNIMPATTERN == (unsigned short *)0){
     fprintf(stderr,"unable to map VME address\n");
     exit(0);
  }
  dts = *pNIMPATTERN;
  printf("NIMPATTERN AD=0x%08x, AM=0x%02x: data read: %8x\n",NIMPATTERN,AM24,dts);

  printf("\n");
  printf("NIM ADCs:\n");
  pNIMadc = (unsigned short *)vme_map(NIMbaseaddr,16*sizeof(short),AM24);
  if (pNIMadc == (unsigned short *)0){
     fprintf(stderr,"unable to map VME address\n");
     exit(0);
  }
  for(i=0; i<16; i++){
     NIMadc[i] = *(pNIMadc+i);
     printf("ADC no %2d, AD=0x%08x, AM=0x%02x: data read: %8x\n",i,NIMbaseaddr+i*inc,AM24,NIMadc[i]);
  }

  printf("A 100000-times loop started, look at the lamps!!!\n");
  for(i=1; i < 100000; i++){
     dtc = *pNEXTREG;
     dts = *pT1STATUS;
     dtc = *pT1NUMREG;
     dts = *pT1PATTERN;
     dts = *pT2STATUS;
     dtc = *pT2NUMREG;
     dts = *pT2PATTERN;
     dts = *pT3STATUS;
     dtc = *pT3NUMREG;
     dts = *pT3PATTERN;
     dts = *pNIMCSR;
     dts = *pNIMPATTERN;
     for(j=0; j<16; j++){
        NIMadc[j] = *(pNIMadc+j);
     }
  }

  vme_rel(pNEXTREG,sizeof(char));
  vme_rel(pT1STATUS,sizeof(short));
  vme_rel(pT1NUMREG,sizeof(char));
  vme_rel(pT1PATTERN,sizeof(short));

  vme_rel(pT2STATUS,sizeof(short));
  vme_rel(pT2NUMREG,sizeof(char));
  vme_rel(pT2PATTERN,sizeof(short));

  vme_rel(pT3STATUS,sizeof(short));
  vme_rel(pT3NUMREG,sizeof(char));
  vme_rel(pT3PATTERN,sizeof(short));

  vme_rel(pNIMCSR,sizeof(short));
  vme_rel(pNIMPATTERN,sizeof(short));
  vme_rel(pNIMadc,16*sizeof(short)); 
}
