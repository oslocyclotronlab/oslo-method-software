/* Simple, non-foolproof test program for SIRI ROCO Card #3 */
/* Written by Jon Wikne, Dept of Physics, Univ. of Oslo     */
/* Based on CES RTPC 8067 LynxOS VME library & examples     */
/* Version 0.05 * 1998-09-22                                */
/* Version 0.06 * 1998-09-25                                */
/* Version 0.07 * 1999-04-15                                */

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
#define RING 0x06
#define EVLN 0x02

#define SIRI_NUMBUF "0x4007"  /* CSR, number of buffers in ring buffer (1 - 32) */
#define SIRI_NXTBUF "0x4006"  /* CSR, next buffer to be read by VME */
#define SIRI_IRQVEC "0x4005"  /* CSR, interrupt vector register */
#define SIRI_EVERDY "0x4004"  /* CSR, event has been read by ROCO */
#define SIRI_BUFPTR "0x4003"  /* CSR, address of SIRI buffer */
#define SIRI_EVEPTR "0x4002"  /* CSR, address of SIRI event */
#define SIRI_COST   "0x4001"  /* CSR, main control and status register */
#define SIRI_CLEAR  "0x4000"  /* CSR, clear command register */
#define SIRI_DSR    "0x0000"  /* DSR, start of event words */

#define BOOL(x)   (!(!(x)))
#define bit(a,x)  BOOL((a) & (1L << (x)))

char cm[100];       /* command string */
long base;
char ring[10];      /* ring buffer control string */
int ring_num;       /* ring buffer word */
char ev_len[10];    /* event length control string */
int ev_len_num;     /* event length word */
const long offset = 0x10000;
char loop;          /* single or loop */
char getmeoutahere; /* break loop interrupt */
char verbose;       /* verbose output */
long roco_peek(char argv1[10], char argv2[10]);
void roco_poke(char argv1[10], char argv2[10], char argv3[10]);
void roco_poke_8(char argv1[10], char argv2[10], char argv3[10]);
void roco_dump(char argv1[10], char argv2[10], char argv3[10]);
void roco_fill(char argv1[10], char argv2[10], char argv3[10], char argv4[10]);
void roco_test(void);
void roco_acquire(void);
void compare(long addr, long testword);
void help(void);
void keyb_int(int sig_num);

void main(){
   printf("   SIRI ROCO Card #3 Test Program\n");
   printf("   Version 0.07 * 1999-04-15\n");
   printf("   Copyright Jon Wikne\n\n");
   base = BASE * offset;
   sprintf(ring,"%04x",(RING + ((RING +1) * 0x0100))); /* init ring buffer control string */
   ring_num = RING;
   sprintf(ev_len,"%04x",(EVLN * 2));                 /* init event length control string */
   ev_len_num = EVLN;
   loop = 'n';

   /* Interrupt handler */
   if(signal(SIGINT, SIG_IGN) != SIG_IGN)
      signal(SIGINT, keyb_int);

   /*++++++++++++++++++++++++++++++++++++++++++++++++++*/
   /*     Infinite main command loop starts here       */
   /*++++++++++++++++++++++++++++++++++++++++++++++++++*/
   for(;;){
      getmeoutahere= 'n';
      verbose = 'v';
      printf("ROCO#3 > ");
      while (fgets(cm,100,stdin) != NULL) { 

        char word1[20], word2[10], word3[10], word4[10];
        char c;
        int i;

        strcpy(word1,""); strcpy(word2,""); strcpy(word3,""); strcpy(word4,"");
        sscanf(cm,"%s",word1);                            /* parse one word */

        /* Exit */
        if (!strcmp(word1,"exit")) exit(0);
        if (!strcmp(word1,"quit")) exit(0);
        if (!strcmp(word1,"xx")) exit(0);

        sscanf(cm,"%s %s %s %s",word1,word2,word3,word4); /* parse more words */

        /* Read */
        if (!strcmp(word1,"read")) {
	  if (!strcmp(word2,""))
            {printf("missing parameter\n"); break;}
          roco_peek(word2,NULL);                          /* first time */
          while (loop == 'y' && getmeoutahere == 'n') {
            roco_peek(word2,NULL);                        /* multiple */
          }
          break;
        }

        /* Write */
        if (!strcmp(word1,"write")) {
	  if (!strcmp(word2,"") || !strcmp(word3,""))
            {printf("missing parameter\n"); break;}
          roco_poke(word2,word3,NULL);                    /* first time */
          while (loop == 'y' && getmeoutahere == 'n') {
            roco_poke(word2,word3,NULL);                  /* multiple */
          }
          break;
        }

        /* Write, then read */
        if (!strcmp(word1,"w-r")) {
	  if (!strcmp(word2,"") || !strcmp(word3,""))
            {printf("missing parameter\n"); break;}
          printf("%s\t%s\t%s\n","roco_w-r",word2,word3);
          roco_poke(word2,word3,NULL);                    /* first time */
          roco_peek(word2,NULL);                          /* first time */
          while (loop == 'y' && getmeoutahere == 'n') {
            roco_poke(word2,word3,NULL);                  /* multiple */
            roco_peek(word2,NULL);                        /* multiple */
          }
          break;
        }

        /* Dump */
        if (!strcmp(word1,"dump")) {
	  if (!strcmp(word2,"") || !strcmp(word3,""))
            {printf("missing parameter\n"); break;}
          roco_dump(word2,word3,NULL);                    /* first time */
          while (loop == 'y' && getmeoutahere == 'n') {
            roco_dump(word2,word3,NULL);                  /* multiple */
          }
          break;
        }

        /* Fill */
        if (!strcmp(word1,"fill")) {
	  if (!strcmp(word2,"") || !strcmp(word3,"") || !strcmp(word4,""))
            {printf("missing parameter\n"); break;}
          roco_fill(word2,word3,word4,NULL);              /* first time */
          while (loop == 'y' && getmeoutahere == 'n') {
            roco_fill(word2,word3,word4,NULL);            /* multiple */
          }
          break;
        }

        /* Test memory */
        if (!strcmp(word1,"test")) {
          printf("%s\n","roco_test");
          roco_poke(SIRI_EVEPTR,"1fff",NULL);             /* Pointers on input side out of the way */
          roco_test();
          break;
        }

        /* Write single byte */
        if (!strcmp(word1,"wbyte")) {
	  if (!strcmp(word2,"") || !strcmp(word3,""))
            {printf("missing parameter\n"); break;}
          roco_poke_8(word2,word3,NULL);                  /* first time */
          while (loop == 'y' && getmeoutahere == 'n') {
            roco_poke_8(word2,word3,NULL);                /* multiple */
          }
          break;
        }

        /* Reset */
        if (!strcmp(word1,"reset")) {
          printf("%s\n","roco_reset");
          roco_poke(SIRI_CLEAR,"0",NULL);                 /* COST & master clear */
          roco_poke(SIRI_EVERDY,"0",NULL);                /* interrupt vector & ready */
          roco_poke(SIRI_NXTBUF,"1",NULL);                /* one buffer */
          break;
        }

        /* Start simple data-aqcuisition */
        if (!strcmp(word1,"start")) {
          printf("%s\n","roco_start");
          verbose = 's';                                  /* shut up */
          roco_poke(SIRI_CLEAR,"0",NULL);                 /* COST & master clear */
          roco_poke(SIRI_EVERDY,"0",NULL);                /* interrupt vector & ready */
          roco_poke(SIRI_NXTBUF,ring,NULL);               /* setup ring buffer */
          roco_poke_8(SIRI_COST,"1",NULL);                /* COST lower byte, enable */
          while (getmeoutahere == 'n') {
            roco_acquire();
          }
          break;
        }

        /* Enable */
        if (!strcmp(word1,"enable")) {
          printf("%s\n","roco_enable");
          roco_poke_8(SIRI_COST,"1",NULL);                /* COST lower byte */
          break;
        }

        /* Disable */
        if (!strcmp(word1,"disable")) {
          printf("%s\n","roco_disable");
          roco_poke_8(SIRI_COST,"0",NULL);                /* COST lower byte */
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

        /* Change number of buffers in ring buffer */
        if (!strcmp(word1,"ring")) {
          printf("%s\t%s\t%s\n","roco_ring",word2,word3);
          if (strcmp(word2,"")){
            sscanf(word2,"%x",&ring_num);                 /* convert to number */
            if (ring_num > 0x20) {
              printf("Error: Not enough memory\n");
              break; }
            sprintf(ring,"%04x",(ring_num + ((ring_num + 1) * 0x0100)));
          }
          else {
            /* Defaults */
            sprintf(ring,"%04x",(RING + ((RING + 1) * 0x0100)));
            ring_num = RING;
          }
          printf("%s\t0x%s\n","roco_ring",ring);
          roco_poke(SIRI_NXTBUF,ring,NULL);               /* setup ring buffer */
          break;
        }

        /* Change event length to be displayed */
        if (!strcmp(word1,"length")) {
          printf("%s\t%s\t%s\n","roco_length",word2,word3);
          if (strcmp(word2,"")){
            sscanf(word2,"%x",&ev_len_num);               /* convert to number */
            sprintf(ev_len,"%04x",(ev_len_num * 2));
          }
          else {
            /* Defaults */
            sprintf(ev_len,"%04x",(EVLN * 2));
            ev_len_num = EVLN;
          }
          printf("%s\t0x%s\n","roco_length",ev_len);
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
long roco_peek(char argv1[10], char argv2[10]){

   unsigned short *p;
   long dt,ad;
   int size = 2;

   sscanf(argv1,"%x",&ad);
   if (verbose == 'v') printf("%s\t0x%04x\n","roco_read",base+ad);
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
   if (verbose == 'v') printf("Address 0x%08x, data read: %04x\n",base+ad,dt);
   vme_rel(p,size);
   return dt;
}

/* Write a 16-bit word to module */
void roco_poke(char argv1[10], char argv2[10], char argv3[10]){

   unsigned short *p;
   long dt,ad;
   int size = 2;

   sscanf(argv1,"%x",&ad);
   sscanf(argv2,"%x",&dt);
   if (verbose == 'v') printf("%s\t0x%08x\t0x%04x\n","roco_write",base+ad,dt);
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
   if (verbose == 'v') printf("Address 0x%08x, data written: %04x\n",base+ad,dt);
   vme_rel(p,size);
}

/* Write a byte (8 bits)  to module */
void roco_poke_8(char argv1[10], char argv2[10], char argv3[10]){

   unsigned char *p;
   long dt,ad;
   int size = 1;

   sscanf(argv1,"%x",&ad);
   sscanf(argv2,"%x",&dt);
   if (verbose == 'v') printf("%s\t0x%08x\t0x%02x\n","roco_write_8",base+ad,dt);
   p = (unsigned char *)vme_map(base+ad,size,AM32);
   if (p == (unsigned char *)0){
     fprintf(stderr,"unable to map VME address\n");
     exit(1);
   }
   *p = dt;
   if (verbose == 'v') printf("Address 0x%08x, data written: %02x\n",base+ad,dt);
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

/* Simple test data-acquisition */
void roco_acquire(void){

   long ptr = 0,cost = 0,end_of_event = 0x0080;
   char pointer[10];

   while (end_of_event != 0 && getmeoutahere == 'n') {
     cost = roco_peek(SIRI_CLEAR,NULL); /* Read control register */
     end_of_event = cost & 0x0080;      /* Check for bit 7 (end of event) */
   }
   ptr = roco_peek(SIRI_EVEPTR,NULL);   /* Read pointer register */
   if (getmeoutahere == 'n')
     printf("%s\n%s%04x\t%s%04x\n","End of event detected!","COST 0x",cost,"pointer 0x",ptr);
   else {
     printf("%s\n%s%04x\t%s%04x\n","Acquisition interrupt detected!","COST 0x",cost,"pointer 0x",ptr);
     return; }
   ptr = ptr & 0xff00;                  /* Truncate internal buffer index */
   if (ptr == 0)
     ptr = (ring_num - 1) * 0x0200;     /* Buffer wrapped around */
   else
     ptr = (ptr * 2) - 0x0200;          /* Previous to be displayed */
   sprintf(pointer,"%04x",ptr);         /* Convert to string */
   roco_dump(pointer,ev_len,NULL);      /* Dump words of latest acquired event */
   roco_poke_8(SIRI_EVERDY,"0",NULL);   /* Signal event ready */
}

/* Help */
void help(void){

     printf("%s\n","roco_help");
     printf("%s\n\n","[hex] = hex number, [dec] = decimal number");
     printf("%s\t\t\t%s\n","base [hex]","Assign module physical base address [hex] (default 0xf0f0)");
     printf("%s\t\t\t\t%s\n","disable","Disable module for acquisition");
     printf("%s\t\t%s\n","dump [hex] [dec]","Dump [dec] 16 bit words starting at offset [hex]");
     printf("%s\t\t\t\t%s\n","enable","Enable module for acquisition");
     printf("%s\t\t\t\t%s\n","exit","Exit program");
     printf("%s\t%s\n","fill [hex1] [hex2] [dec]","Fill [dec] 16 bit words [hex2] starting at offset [hex1]");
     printf("%s\t\t\t\t%s\n","help","Print this list");
     printf("%s\t\t\t%s\n","length [hex]","Length of event to be displayed [hex]");
     printf("%s\t\t\t\t%s\n","loop","Toggle single/loop read/write");
     printf("%s\t\t\t\t%s\n","quit","Exit program");
     printf("%s\t\t\t\t%s\n","reset","Write reset sequence to module");
     printf("%s\t\t\t%s\n","read [hex]","Read 16 bit word at offset [hex]");
     printf("%s\t\t\t%s\n","ring [hex]","Number of elements (each 0x100 words long) in ring buffer [hex]");
     printf("%s\t\t\t\t%s\n","start","Start test data-acquisition");
     printf("%s\t\t\t\t%s\n","test","Write / read / compare entire dual port RAM");
     printf("%s\t\t%s\n","wbyte [hex1] [hex2]","Write 8 bit byte [hex2] at offset [hex1]");
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
