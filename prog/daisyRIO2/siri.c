/* Simple, non-foolproof test program for SIRI ROCO Card #3 */
/* Written by Jon Wikne, Dept of Physics, Univ. of Oslo     */
/* Based on CES RTPC 8067 LynxOS VME library & examples     */
/* Version 0.05 * 1998-09-22                                */
/* Version 0.06 * 1998-09-25                                */

#include <stdio.h>
#include <math.h>
#include <strings.h>
#include <signal.h>
#include <types.h>
#include <ces/uiocmd.h>
#include <ces/vmelib.h>

#define AM24 0x39
#define AM32 0x09
#define BASE 0xF0F0

#define BOOL(x)   (!(!(x)))
#define bit(a,x)  BOOL((a) & (1L << (x)))

#define SIRINUBUF 0x4007  /*CSR, number of buffers in ring buffer (1 - 32)*/
#define SIRINXBUF 0x4006  /*CSR, next buffer to be read by VME*/
#define SIRIIRQVC 0x4005  /*CSR, interrupt vector register*/
#define SIRIEVRDY 0x4004  /*CSR, event has been read by ROCO*/
#define SIRIBUFAD 0x4003  /*CSR, address of SIRI buffer*/
#define SIRIEVEAD 0x4002  /*CSR, address of SIRI event*/
#define SIRICOST  0x4001  /*CSR, main control and status register*/
#define SIRICLR   0x4000  /*CSR, clear command register*/
#define SIRIDSR   0x0000  /*DSR, start of event words*/

unsigned char *pSIRICOST, *pSIRIBUFAD, *pSIRIEVEAD, *pSIRIEVRDY, *pSIRINUBUF, *pSIRINXBUF;
unsigned short *pSIRIDSR;

char cm[100];       /*command string*/
long base;
const long offset = 0x10000;
char loop;          /*single or loop*/
char getmeoutahere; /*break loop interrupt*/
void roco_peek(char argv1[10], char argv2[10]);
void roco_poke(char argv1[10], char argv2[10], char argv3[10]);
void roco_poke_8(char argv1[10], char argv2[10], char argv3[10]);
void roco_dump(char argv1[10], char argv2[10], char argv3[10]);
void roco_fill(char argv1[10], char argv2[10], char argv3[10], char argv4[10]);
void roco_test(void);
void compare(long addr, long testword);
void help(void);
void keyb_int(int sig_num);

void main(){
   printf("   SIRI ROCO Card #3 Test Program\n");
   printf("   Version 0.06 * 1998-09-25\n");
   printf("   Copyright Jon Wikne\n\n");
   base = BASE * offset;
   loop = 'n';

   /* Interrupt handler */
   if(signal(SIGINT, SIG_IGN) != SIG_IGN)
      signal(SIGINT, keyb_int);

   /*++++++++++++++++++++++++++++++++++++++++++++++++++*/
   /*     Infinite main command loop starts here       */
   /*++++++++++++++++++++++++++++++++++++++++++++++++++*/
   for(;;){
      getmeoutahere= 'n';
      printf("ROCO#3 > ");
      while (fgets(cm,100,stdin) != NULL) { 

        char word1[20], word2[10], word3[10], word4[10];
        char c;
        int i;

        strcpy(word1,""); strcpy(word2,""); strcpy(word3,""); strcpy(word4,"");
        sscanf(cm,"%s",word1); /* parse one word */

        /* Exit */
        if (!strcmp(word1,"exit")) exit(0);
        if (!strcmp(word1,"quit")) exit(0);
        if (!strcmp(word1,"xx")) exit(0);

        sscanf(cm,"%s %s %s %s",word1,word2,word3,word4); /* parse more words */

        /* Read */
        if (!strcmp(word1,"read")) {
          roco_peek(word2,NULL); /* first time */
          while (loop == 'y' && getmeoutahere == 'n') {
            roco_peek(word2,NULL); /* multiple */
          }
          break;
        }

        /* Write */
        if (!strcmp(word1,"write")) {
          roco_poke(word2,word3,NULL); /* first time */
          while (loop == 'y' && getmeoutahere == 'n') {
            roco_poke(word2,word3,NULL); /* multiple */
          }
          break;
        }

        /* Write, then read */
        if (!strcmp(word1,"w-r")) {
          printf("%s\t%s\t%s\n","roco_w-r",word2,word3);
          roco_poke(word2,word3,NULL); /* first time */
          roco_peek(word2,NULL); /* first time */
          while (loop == 'y' && getmeoutahere == 'n') {
            roco_poke(word2,word3,NULL); /* multiple */
            roco_peek(word2,NULL); /* multiple */
          }
          break;
        }

        /* Dump */
        if (!strcmp(word1,"dump")) {
          roco_dump(word2,word3,NULL); /* first time */
          while (loop == 'y' && getmeoutahere == 'n') {
            roco_dump(word2,word3,NULL); /* multiple */
          }
          break;
        }

        /* Fill */
        if (!strcmp(word1,"fill")) {
          roco_fill(word2,word3,word4,NULL); /* first time */
          while (loop == 'y' && getmeoutahere == 'n') {
            roco_fill(word2,word3,word4,NULL); /* multiple */
          }
          break;
        }

        /* Test memory */
        if (!strcmp(word1,"test")) {
          printf("%s\n","roco_test");
          roco_poke("4002","1fff",NULL); /* Pointers on input side out of the way */
          roco_test();
          break;
        }

        /* Write single byte */
        if (!strcmp(word1,"wbyte")) {
          roco_poke_8(word2,word3,NULL); /* first time */
          while (loop == 'y' && getmeoutahere == 'n') {
            roco_poke_8(word2,word3,NULL); /* multiple */
          }
          break;
        }

        /* Reset */
        if (!strcmp(word1,"reset")) {
          printf("%s\n","roco_reset");
          roco_poke("4000","0",NULL); /* COST & master clear */
          roco_poke("4004","0",NULL); /* interrupt vector & ready */
          roco_poke("4006","1",NULL); /* one buffer */
          break;
        }

        /* Enable */
        if (!strcmp(word1,"enable")) {
          printf("%s\n","roco_enable");
          roco_poke_8("4001","1",NULL); /* COST lower byte */
          break;
        }

        /* Change base */
        if (!strcmp(word1,"base")) {
          printf("%s\t%s\t%s\n","roco_base",word2,word3);
          if (strcmp(word2,"")){
            sscanf(word2,"%x",&base);
            base = base * offset;
          }
          else base = BASE * offset;
          printf("%s\t0x%08x\n","roco_base",base);
          break;
        }

        /* Toggle infinite loop */
        if (!strcmp(word1,"loop")) {
          if (loop == 'n') {
            loop = 'y';
            printf("%s\n","loop on"); }
          else {
            loop = 'n';
            printf("%s\n","loop off"); }
          break;
        }

        /* Help */
        if (!strcmp(word1,"help") || !strcmp(word1,"menu") || !strcmp(word1,"?")) {
          help();
          break;
        }

        /* Blank command */
        if (!strcmp(word1,"")) {
          break;
        }

        /* Bummer! */
        printf("Syntax error!\n");
        break;
      }
   }
}

/* Read a 16-bit word from module */
void roco_peek(char argv1[10], char argv2[10]){

   unsigned short *p;
   long dt,ad;
   int size = 2;

   sscanf(argv1,"%x",&ad);
   printf("%s\t0x%04x\n","roco_read",base+ad);
   if (ad % 2 != 0){
     printf("%s\n","Sorry, 16 bit operations must start at _even_ adresses");
     return;
   }
   p = (unsigned short *)vme_map(base+ad,size,AM32);
   if (p == (unsigned short *)0){
     fprintf(stderr,"unable to map VME address\n");
     exit(1);
   }
   dt = *p;
   printf("Address 0x%08x, data read: %04x\n",base+ad,dt);
   vme_rel(p,size);
}

/* Write a 16-bit word to module */
void roco_poke(char argv1[10], char argv2[10], char argv3[10]){

   unsigned short *p;
   long dt,ad;
   int size = 2;

   sscanf(argv1,"%x",&ad);
   sscanf(argv2,"%x",&dt);
   printf("%s\t0x%08x\t0x%04x\n","roco_write",base+ad,dt);
   if (ad % 2 != 0){
     printf("%s\n","Sorry, 16 bit operations must start at _even_ adresses");
     return;
   }
   p = (unsigned short *)vme_map(base+ad,size,AM32);
   if (p == (unsigned short *)0){
     fprintf(stderr,"unable to map VME address\n");
     exit(1);
   }
   *p = dt;
   printf("Address 0x%08x, data written: %04x\n",base+ad,dt);
   vme_rel(p,size);
}

/* Write a byte (8 bits)  to module */
void roco_poke_8(char argv1[10], char argv2[10], char argv3[10]){

   unsigned char *p;
   long dt,ad;
   int size = 1;

   sscanf(argv1,"%x",&ad);
   sscanf(argv2,"%x",&dt);
   printf("%s\t0x%08x\t0x%02x\n","roco_write_8",base+ad,dt);
   p = (unsigned char *)vme_map(base+ad,size,AM32);
   if (p == (unsigned char *)0){
     fprintf(stderr,"unable to map VME address\n");
     exit(1);
   }
   *p = dt;
   printf("Address 0x%08x, data written: %02x\n",base+ad,dt);
   vme_rel(p,size);
}

/* Dump a block of 16-bit words from module */
void roco_dump(char argv1[10], char argv2[10], char argv3[10]){

   unsigned short *p;
   long dt,ad,n,i,d;
   int size = 2;

   sscanf(argv1,"%x",&ad);
   sscanf(argv2,"%d",&n);
   printf("%s\t0x%08x\t%d%s\n","roco_dump",base+ad,n," words");
   if (ad % 2 != 0){
     printf("%s\n","Sorry, 16 bit operations must start at _even_ adresses");
     return;
   }
   for (i = 0; i < n; i++) {
     d = 2*i; /* 16 bits at a time */
     p = (unsigned short *)vme_map(base+ad+d,size,AM32);
     if (p == (unsigned short *)0){
       fprintf(stderr,"unable to map VME address\n");
       exit(1);
     }
     dt = *p;
     printf("Address 0x%08x, data read: %04x\n",base+ad+d,dt);
     vme_rel(p,size);
     if (getmeoutahere == 'y') break;
     /* usleep(1000); */
   }
}

/* Fill a block of 16-bit words in module with a value */
void roco_fill(char argv1[10], char argv2[10], char argv3[10], char argv4[10]){

   unsigned short *p;
   long dt,ad,n,i,d;
   int size = 2;

   sscanf(argv1,"%x",&ad);
   sscanf(argv2,"%x",&dt);
   sscanf(argv3,"%d",&n);
   printf("%s\t0x%08x\t0x%04x\t%d%s\n","roco_fill",base+ad,dt,n," words");
   if (ad % 2 != 0){
     printf("%s\n","Sorry, 16 bit operations must start at _even_ adresses");
     return;
   }
   for (i = 0; i < n; i++) {
     d = 2*i; /* 16 bits at a time */
     p = (unsigned short *)vme_map(base+ad+d,size,AM32);
     if (p == (unsigned short *)0){
       fprintf(stderr,"unable to map VME address\n");
       exit(1);
     }
     *p = dt;
     printf("Address 0x%08x, data written: %04x\n",base+ad+d,dt);
     vme_rel(p,size);
     if (getmeoutahere == 'y') break;
   }
}

/* Test dual port memory by writing / reading a pattern */
void roco_test(void){

   long dtw,ad,i,n = 0x1fff;

   for (i = 0; i < n; i++) {
     ad = 2*i; /* 16 bits at a time */
     dtw = 0x5555; /* test pattern 0101.... */
     compare(base+ad,dtw);
     dtw = 0xaaaa; /* test pattern 1010.... */
     compare(base+ad,dtw);
     if (getmeoutahere == 'y') break;
     printf("\r0x%08x",base+ad);
   }
   printf("\n");
}

/* Routine for write / read / compare to same location */
void compare(long addr, long testword){

   unsigned short *p;
   long dtr;
   int size = 2;

     p = (unsigned short *)vme_map(addr,size,AM32);
     if (p == (unsigned short *)0){
       fprintf(stderr,"unable to map VME address\n");
       exit(1);
     }
     *p = testword;
     dtr = *p;
     if (dtr != testword){
       printf("\n%s0x%08x%s\t%04x%s\t%04x%s\n","Error at ",addr," :",testword," written,",dtr," read");
       getmeoutahere = 'y';
     }
     vme_rel(p,size);
}

/* Help */
void help(void){

     printf("%s\n","roco_help");
     printf("%s\n\n","[hex] = hex number, [dec] = decimal number");
     printf("%s\t\t\t%s\n","base [hex]","Assign module physical base address [hex] (default 0xf0f0)");
     printf("%s\t\t%s\n","dump [hex] [dec]","Dump [dec] 16 bit words starting at offset [hex]");
     printf("%s\t\t\t\t%s\n","enable","Enable module for acquisition");
     printf("%s\t\t\t\t%s\n","exit","Exit program");
     printf("%s\t%s\n","fill [hex1] [hex2] [dec]","Fill [dec] 16 bit words [hex2] starting at offset [hex1]");
     printf("%s\t\t\t\t%s\n","help","Print this list");
     printf("%s\t\t\t\t%s\n","loop","Toggle single/loop read/write");
     printf("%s\t\t\t\t%s\n","quit","Exit program");
     printf("%s\t\t\t\t%s\n","reset","Write reset sequence to module");
     printf("%s\t\t\t%s\n","read [hex]","Read 16 bit word at offset [hex]");
     printf("%s\t\t\t\t%s\n","test","Write / read / compare entire dual port RAM");
     printf("%s\t\t%s\n","write [hex1] [hex2]","Write 16 bit word [hex2] at offset [hex1]");
     printf("%s\t\t%s\n\n","w-r [hex1] [hex2]","Write, then read same location");
}

/* Keyboard interrupt routine */
void keyb_int(int sig_num){

   if (sig_num == SIGINT) {
     printf(" Interrupt!!\n");
     getmeoutahere= 'y';
   }
}
