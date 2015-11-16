/* Eventbuilder, a VME-based data acquisition system for the CACTUS/SIRI      */
/* multi-detdetector system. Written by Magne Guttormsen for the CES RIO2     */
/* single board processor including a PowerPC 604r @ 300 MHz CPU running      */
/* LynxOS. The message box (in A24 slave memory) has base address pmes in     */
/* this program (this CPU) and the absolute VME address 0x85000 to be used    */
/* from Sun (second CPU). The transfer through SRAM requires byte swaps, which*/
/* is performed with the swap(x) macro below. The eventbuffer in slave        */
/* A24 memory has base address pmove in this program (this CPU) and the       */
/* VME address is calculated to be vad, which is to be used from Sun          */
/* (second CPU). The vad address is transferred to Sun via the message        */
/* box. The second CPU (Sun CPU) access the A24 slave memory of the           */
/* VME CES RIO2 CPU through the Bit3 Sbus <-> VME interface.                  */
/* For further details see: M. Guttormsen, Eventbuilder for the RTPC8067      */
/* Single Board Computer, Department of Physics, University of Oslo,          */
/* Institute Report, UIO/PHYS/98-08 1998                                      */

#include      <stdio.h>
#include       <time.h>
#include       <smem.h>
#include <ces/uiocmd.h>
#include <ces/vmelib.h>
#include     <signal.h>
#include      <types.h>

#define BOOL(x)   (!(!(x)))
#define bit(a,x)  BOOL((a) & (1L << (x)))
#define swap(x) ((x&0xff000000)>>24)+((x&0x00ff0000)>>8)+((x&0x0000ff00)<<8)+((x&0x000000ff)<<24)
#define swap1(x) (((x&0xff000000)>>24)+((x&0x00ff0000)>>8)+((x&0x0000ff00)<<8)+((x&0x000000ff)<<24)&0x00000001)
#define WAIT(x) {volatile long ii;for(ii=x;ii;ii--);}/*1 loop =66/300*0.1647us*/
#define WAITupdate(a,b) if(a>b){WAIT(a-b);b=a+2;}

#define usLOOP         0.0362             /*Number of microseconds/loop       */
#define MESSNUM        10                 /*Number of words in message box    */
#define MAXBUF         32767              /*Upper limit of buffer             */
#define EVENT_HEADER   0xf000             /*Event_header identification code  */
#define SAFE           200                /*Maximum eventlength with 5 TPU`s  */
#define EMPTY          0x0                /*Reset semaphore                   */
#define FULL           0x1                /*Set semaphore                     */

#define CPUADDR        0xff010000         /*Phys. addr. for SRAM -> A24 slave */
#define SLV24ADDR      0x00850000         /*Absolute VME A24 slave address    */
                                          /*with hex switch = 5 on CPU card   */
#define CLEAR          0x0
#define ENABLE         0x1
#define DISABLE        0x0
#define NUMBUF         0x1                /*Number of ring buffers (1 - 32)   */
#define START          0x1
#define STOP           0x0
#define AM24           0x39               /*Address modifier for 24 bits AM24 */
#define AM32           0x09               /*Address modifier for 32 bits AM32 */

#define NEXTREG        0xf0ffff09
#define T1STATUS       0xf0ffff00         /*Registers in Master TPU1          */
#define T1NUMREG       0xf0ffff05
#define T1PATTERN      0xf0ffff06
#define T2STATUS       0xf0ffff10         /*Registers in Slave  TPU2          */
#define T2NUMREG       0xf0ffff15
#define T2PATTERN      0xf0ffff16
#define T3STATUS       0xf0ffff20         /*Registers in Slave  TPU3          */
#define T3NUMREG       0xf0ffff25
#define T3PATTERN      0xf0ffff26
#define NIMCSR         0xf0efff9c         /*Control & Status register         */
#define NIMPATTERN     0xf0efff9e         /*Pattern register                  */
#define NIMbaseaddr    0xf0efffa0         /*Baseaddress for NIM ADCs          */

#define SIRINUBUF 0xf0f04007  /*CSR, number of buffers in ring buffer (1 - 32)*/
#define SIRINXBUF 0xf0f04006  /*CSR, next buffer to be read by VME            */
#define SIRIIRQVC 0xf0f04005  /*CSR, interrupt vector register                */
#define SIRIEVRDY 0xf0f04004  /*CSR, event has been read by ROCO              */
#define SIRIBUFAD 0xf0f04003  /*CSR, address of SIRI buffer                   */
#define SIRIEVEAD 0xf0f04002  /*CSR, address of SIRI event                    */
#define SIRICOST  0xf0f04001  /*CSR, main control and status register         */
#define SIRICLR   0xf0f04000  /*CSR, clear command register                   */
#define SIRIDSR   0xf0f00000  /*DSR, start of event words                     */

char tpu_2, tpu_3;
long i, j, f, q, SIRI, TPUS;
int curbuf, curmax, par, para, value, pileup, onerun, bufwait;
u_short patcopy1, patcopy2, patcopy3;

time_t now;
struct tm *date;
char sSTART[80], sSTOP[80], inbuf[130];
int rx, ex, tx, r1, r2, e1, e2, t0, t1, t2;

u_short *pT1STATUS, *pT2STATUS,  *pT3STATUS;
u_char  *pT1NUMREG, *pT2NUMREG,  *pT3NUMREG;
u_short *pT1PATTERN,*pT2PATTERN, *pT3PATTERN;
u_short *pNIMCSR,   *pNIMPATTERN;
u_short *pNIMadc;
u_char  *pNEXTREG;

int SIRIpat1, SIRIpat2, SIRIpat3, SIRIpat4;
int imax, k, chipadi;
int evno[32], chipad[32], energy[32];                 /*Storing 16 dE-E values*/
int   dE[64],      E[64];                             /*64 telescopes         */
int assignE[64];                                      /*Make E assigned to dE */
int one = 1L;                                         /*for bit-pattern       */

u_char  *pSIRICLR,   *pSIRICOST,  *pSIRIEVEAD, *pSIRIBUFAD;
u_char  *pSIRIEVRDY, *pSIRIIRQVC, *pSIRINXBUF, *pSIRINUBUF;
u_short *pSIRIDSR,   SIRIadc[64];

long *pBUFFER_ADDRESS, *pBUFFER_LENGTH, *pSEMA_1, *pSEMA_2, *pSEMA_3, *pSEMA_4;
long *pVMESTATUS, *pSUNSTATUS;
long header, p1;
long records, events, recordsold, eventsold;

int  mem24;
long *pmes, *pbuf, *pmove, *p;
long bufferbytes, messagebytes, slavebytes, movebytes, err;
long ad, vad;
uio_mem_t cmem_dsc;

float uPUR, uNIM, uCAMAC, uSIRI, uEVENT;       /*Extra delay until ADCs finish*/
int loopTOT, loopPUR, loopNIM, loopCAMAC, loopSIRI, loopEVENT;
int loop500ns, loop1us;
int acqmode;

#define BRANCH      1                      /*Rotary switch on Branch Driver   */
#define CRATE       1                      /*Rotary switch on Crate Controller*/
#define PUR1SLOT    7                      /*Position of PUR1                 */
#define ADC1SLOT   11                      /*Position of ADC1                 */
#define ADC2SLOT   12
#define ADC3SLOT   13
#define ADC4SLOT   14
#define TDC1SLOT   15
#define TDC2SLOT   16
#define TDC3SLOT   17
#define TDC4SLOT   18

u_short tempc, tpp0, tpp1;                 /*Temporary storage for CAMAC, PUR */

int cadc1[8];                              /*ADC1 data registers              */
int cadc2[8];                              /*ADC2 data registers              */
int cadc3[8];                              /*ADC3 data registers              */
int cadc4[8];                              /*ADC4 data registers              */
int ctdc1[8];                              /*TDC1 data registers              */
int ctdc2[8];                              /*TDC2 data registers              */
int ctdc3[8];                              /*TDC3 data registers              */
int ctdc4[8];                              /*TDC4 data registers              */
int cpur1[4];                              /*PUR1 data registers              */

int cmd;
char leaveloop;                            /*Break loop interrupt (Ctrl_C)    */

void keyb_int(int sig_num);
void Menu();
void Loop();
void Acquisitionmode();
void Dumpbuf();
void Status();
void Parameters();
void DisEnAble();
void Microsecond();
void SLAVEinitiate();
void CAMACopen();
void CAMACconfig();
void CAMACclose();
/*
void SIRIopen();
void SIRIconfig();
void SIRIclose();
*/
void TPUopen();
void TPUconfig();
void TPUclose();
void A24open();
void MESSAGEBOXconfig();
void A24close();
void BUFFERopen();
void BUFFERclear();
void BUFFERclose();
void eventmonitor();

int main() {                                      /*Interrupt handler (Ctrl_C)*/
   if(signal(SIGINT, SIG_IGN) != SIG_IGN){ 
      signal(SIGINT, keyb_int);
   }

   printf(" ____________________________________________________________ \r\n");
   printf("|                                                            |\r\n");
   printf("|                     Eventbuilder+ 4.2                      |\r\n");
   printf("|                                                            |\r\n");
   printf("|          A VME-based data acquisition system for           |\r\n");
   printf("|            the CACTUS/SIRI multidetector system            |\r\n");
   printf("|      Written for the CES RIO2 single board processor       |\r\n");
   printf("|      with a PowerPC 604r @ 300 MHz CPU running LynxOS      |\r\n");
   printf("|   NB! to activate SIRI, various comments must be removed   |\r\n");
   printf("| E-mail  : magne.guttormsen@fys.uio.no                      |\r\n");
   printf("| Created : 14-02-1998                                       |\r\n");
   printf("| Modified: 09-03-1998/ 12-06-1998/ 05-01-1999/ 12-12-2001   |\r\n");
   printf("|____________________________________________________________|\r\n");
   printf("                                                              \r\n");

   SLAVEinitiate();
   A24open();
   BUFFERopen();
   BUFFERclear();
   CAMACopen();
   CAMACconfig();
 /* SIRIopen();comment away if not SIRI*/
 /* SIRIconfig();comment away if not SIRI*/
   TPUopen();
   TPUconfig();
   MESSAGEBOXconfig();

   acqmode   =  2;    /* SIRI is not included */
   events    =  0;
   records   =  0;

   uPUR      =  8.0;
   uNIM      = 45.0;
   uCAMAC    = 35.0;
   uSIRI     =  8.0;
   uEVENT    =  0.0;
   loopPUR   = (int)((uPUR   / usLOOP) + 0.5);
   loopNIM   = (int)((uNIM   / usLOOP) + 0.5);
   loopCAMAC = (int)((uCAMAC / usLOOP) + 0.5);
   loopSIRI  = (int)((uSIRI  / usLOOP) + 0.5);
   loopEVENT = (int)((uEVENT / usLOOP) + 0.5);
   loop500ns = (int)((0.500  / usLOOP) + 0.5);
   loop1us   = (int)((1.000  / usLOOP) + 0.5);
                                                    /*Assigning E to wright dE*/
   assignE[32] = 40;   /*P11 -> P13*/ 
   assignE[33] = 41;
   assignE[34] = 42;
   assignE[35] = 43;
   assignE[36] = 44;
   assignE[37] = 45;
   assignE[38] = 46;
   assignE[39] = 47;
   assignE[40] = 48;   /*P13 -> P15*/
   assignE[41] = 49;
   assignE[42] = 50;
   assignE[43] = 51;
   assignE[44] = 52;
   assignE[45] = 53;
   assignE[46] = 54;
   assignE[47] = 55;
   assignE[48] = 32;   /*P15 -> P11*/
   assignE[49] = 33;
   assignE[50] = 34;
   assignE[51] = 35;
   assignE[52] = 36;
   assignE[53] = 37;
   assignE[54] = 38;
   assignE[55] = 39;
   assignE[56] = 56;   /*P17 -> P17*/
   assignE[57] = 57;
   assignE[58] = 58;
   assignE[59] = 59;
   assignE[60] = 60;
   assignE[61] = 61;
   assignE[62] = 62;
   assignE[63] = 63;

   Menu();

   do{
      eventsold  = events;                                 /*Making statistics*/
      recordsold = records;
      if(cmd == 'r'|cmd == 'o'){
         *pVMESTATUS = swap(STOP);                 /*Set VME status to stopped*/
         time( &now );
         date = localtime( &now );
         strftime( sSTOP, 80, "%c", date );
         printf("\nVME data acquisition stopped at %s\n", sSTOP );
         printf("Records = %ld    Events = %ld \n\n", recordsold, eventsold);
      }

      leaveloop ='n';                          /*Do not jump out of event-loop*/
      printf("eventbuilder+>");
      cmd = readchar();
      switch (cmd) {
         case 'a':Acquisitionmode();break;
         case 'r':onerun=0; Loop(); break;
         case 'o':onerun=1; Loop(); break;
         case 'd':Dumpbuf();        break;
         case 's':Status();         break;
         case 'e':DisEnAble();      break;
         case 'm':Microsecond();    break;
         case 'p':Parameters();     break;
         case 'h':Menu();           break;
         case '\n':                 break;
         case '*':                  break;
         default :printf(" Illegal command, try again\n");
      }
   }while (cmd != '*');

   TPUclose();
   /*SIRIclose();                                     comment away if not SIRI*/
   CAMACclose();
   A24close();
   BUFFERclose();
   return 0;
}                                                           /*End main program*/


void Loop(){
   events  = 0;                                    /*Reset numbers for new run*/
   records = 0;
   time( &now );
   date = localtime( &now );
   strftime( sSTART, 80, "%c", date );
   printf( "\nVME data acquisition started at %s\n", sSTART );
   printf( "To jump out of event-loop, press Ctrl_C\n");
  
   waitstate:
   usleep(1);                             /*Wait 1 microseconds               */
   *pT1STATUS    = CLEAR;
   *pVMESTATUS   = swap(START);           /*Set VME status to started         */
   *pNIMPATTERN  = CLEAR;                 /*Reset NIM ADCs for event          */
   *pSEMA_1      = swap(EMPTY);           /*Buffer 1 is empty                 */
   *pSEMA_2      = swap(EMPTY);           /*Buffer 2 is empty                 */
   *pSEMA_3      = swap(EMPTY);           /*Buffer 3 is empty                 */
   *pSEMA_4      = swap(EMPTY);           /*Buffer 4 is empty                 */

   cccc(cadc1[0]);                        /*Reset all CAMAC devices for event */

   p1            = 0;
   curmax        = MAXBUF;
   if(onerun    == 0){
      usleep(10000);                             /*Wait 10000 microseconds    */
      par = swap1(*pSUNSTATUS);
      if(par == 1){
         printf( "SIRIUS is running: Push the STOP button for SIRIUS!!!\n");
         return;
      }
      printf( "Push the START button for SIRIUS!!!\n");

      while(par != 1){
         usleep(10000);
         par = swap1(*pSUNSTATUS);
         if(leaveloop == 'y') return;            /*Jump out of event-loop     */
      }
      printf("\n Eventbuilder monitor (update every minute) \n");
      printf("-------------------------------------------   \n");
      printf("     Running    Events/s    Records/s         \n");
      t0 = time(&now);
      t1 = 0;
      r1 = 0;
      e1 = 0;
      records = 0;
      events  = 0;
   }


   /*++++++++++++++++++++++++++++++++++++++++++++++++++*/
   /*     Infinite main readout loop starts here       */
   /*++++++++++++++++++++++++++++++++++++++++++++++++++*/
   for(;;){
      nextevent:

      /*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
      /*                          Buffer handling section                    */
      /*  New from version 4.0 and higher:  Since A32 slave DRAM gives crash */
      /*  after typically 2 - 5 hours, we use A24 SRAM. However, this space  */
      /*  is only 128 kbytes. Thus, we cannot use the two-buffer technique   */
      /*  from versions 2.0 - 3.0 (originally designed by Tore Ramsoey). We  */
      /*  have to segment 1 eventbuffers (32 kwords) into 4 movebuffers,     */
      /*  each 32 kbytes. Tests show that this is almost as fast as before,  */
      /*  and no more crashes of the eventbuilder+ program of RTPC!!!        */
      /*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
      if(p1 >= curmax-SAFE){                 /*Not space enough, empty buffer*/
         if(onerun == 1)return;
         if(p1 >= curmax-(SAFE/2)){       /*Warning when close to full buffer*/
            time( &now );
            date = localtime( &now );
            strftime( sSTART, 80, "%c", date );
            printf( "Buffer %d long!, max = %d (%s)\n", p1, curmax, sSTART );
         }
         for(i = p1; i < MAXBUF + 1; i++){
            *(pbuf+i) = 0;                  /*Remove old data from upper part*/
         }

         p = pbuf;
         for(i = 0; i < 0x2000; i++){
            *(pmove+i) = *(p+i);                              /*Move buffer 1*/ 
         }
         *pSEMA_1     = swap(FULL);          /*Set buffer 1 semaphore to FULL*/
         value        = FULL;
         while(value != EMPTY){            /*Waits for buffer 1 to be fetched*/
           WAIT(loop1us);
            par       = swap1(*pSUNSTATUS); /*Checking that Sirius is running*/
            if(par == 0) goto waitstate;
            if(leaveloop == 'y') return;             /*Jump out of event-loop*/
            value        = swap1(*pSEMA_1);
         }

         p = pbuf + 0x2000;
         for(i = 0; i < 0x2000; i++){
            *(pmove+i) = *(p+i);                              /*Move buffer 2*/
         } 
         *pSEMA_2     = swap(FULL);          /*Set buffer 2 semaphore to FULL*/
         value        = FULL;
         while(value != EMPTY){            /*Waits for buffer 1 to be fetched*/
            WAIT(loop1us);
            par       = swap1(*pSUNSTATUS); /*Checking that Sirius is running*/
            if(par == 0) goto waitstate;
            if(leaveloop == 'y') return;             /*Jump out of event-loop*/
            value        = swap1(*pSEMA_2);
         }

         p = pbuf + 0x4000;
         for(i = 0; i < 0x2000; i++){
            *(pmove+i) = *(p+i);                              /*Move buffer 3*/
         } 
         *pSEMA_3     = swap(FULL);          /*Set buffer 3 semaphore to FULL*/
         value        = FULL;
         while(value != EMPTY){            /*Waits for buffer 3 to be fetched*/
            WAIT(loop1us);
            par       = swap1(*pSUNSTATUS); /*Checking that Sirius is running*/
            if(par == 0) goto waitstate;
            if(leaveloop == 'y') return;             /*Jump out of event-loop*/
            value        = swap1(*pSEMA_3);
         }

         p = pbuf + 0x6000;
         for(i = 0; i < 0x2000; i++){
            *(pmove+i) = *(p+i);                              /*Move buffer 4*/
         } 
         *pSEMA_4     = swap(FULL);          /*Set buffer 4 semaphore to FULL*/
         value        = FULL;
         while(value != EMPTY){            /*Waits for buffer 4 to be fetched*/
            WAIT(loop1us);
            par       = swap1(*pSUNSTATUS); /*Checking that Sirius is running*/
            if(par == 0) goto waitstate;
            if(leaveloop == 'y') return;             /*Jump out of event-loop*/
            value        = swap1(*pSEMA_4);
         }


         records = records + 1;
         p1      = 0;               /*Set pointer to first location of buffer*/
         if(onerun == 1)return;
         eventmonitor();
      }

      loopTOT = 0;
      *pNIMPATTERN = CLEAR;                      /*Reset NIM   for next event */
      cccc(cadc1[0]);                            /*Reset CAMAC for next event */
      *pNEXTREG    = CLEAR;                      /*Reset TPUs  for next event */
      /*  *pSIRIEVRDY  = CLEAR;                    Reset SIRI  for next event */
 
      TPUS = 0;
      SIRI = 0;
      if(leaveloop == 'y') return;               /*Jump out of event-loop     */

      if(acqmode != 4){
         while(TPUS == 0){                   /*For AcqMode = 1, 2 and 3       */
         if(leaveloop == 'y') return;        /*Jump out of event-loop         */
         TPUS   =  bit(*pT1NUMREG, 7);       /*Bit7=1 means event (eoe)       */
         }
      }else{
         while(SIRI == 0){                   /*For AcqMode = 4                */
         if(leaveloop == 'y') return;        /*Jump out of event-loop         */
         SIRI = !bit(*pSIRICOST, 7);         /*Bit7=0 means SIRI ADC finished */
         }   
      }

      WAITupdate(loopEVENT,loopTOT);         /*General wait for ADC convertion*/
       

      /* :::::::::::::::::::::::::::::::::::::::::::::::::::*/
      /*     Check for pileup                               */
      /* :::::::::::::::::::::::::::::::::::::::::::::::::::*/
      WAITupdate(loopPUR,loopTOT);                       /*Waiting for pile-up*/  
      para   = *pT1NUMREG;
      pileup = bit(para, 6);                             /*Pile-up flag       */
         
      if (pileup == 1) {           /*Skip this event and continue with next   */
         goto nextevent;           /*Continue with next event                 */
      }

      if(acqmode == 3) goto sirievent;                      /*Only SIRI events*/

      header = p1;                 /*Start of event pointer is saved in header*/
      p1     = p1 + 1;             /*Update buffer pointer                    */

 
      if (TPUS > 0){
         /*:::::::::::::::::::::::::::::::::::::::::::::::::*/
         /* Data structure and readout for TPU1             */
         /* TPU1 (Master) is dedicated to NIM ADC`s         */
         /* Bit 0 points to ADC1 and 2, bit 2 to ADC3 and 4 */
         /* and so on.                                      */
         /* Odd bit numbers are not used by event_builder   */
         /* They may be used as logical bits in sorting     */
         /*:::::::::::::::::::::::::::::::::::::::::::::::::*/
         patcopy1 = *pT1PATTERN;                 /*Take a copy of TPU1 pattern*/
         if (patcopy1 != 0){ 
            *(pbuf+p1) = swap(0x800A);
            p1         = p1 + 1;
            *(pbuf+p1) = swap(patcopy1);                     /*Store pattern 1*/
            p1         = p1 + 1;
            
            /*           *** Test Only ***                       */
            /* Loop on eventreadyflag from NIM CONTROLLER        */
            /* Not necessary with the present code efficiency    */
            /* 7411 : max 22 us at 2k, time elapsed is app. 70us */
            /* 7420 : max 6 us at 2k                             */
            /* notready:                                         */
            /* para    = *pNIMCSR;                               */
            /* counter = counter + 1;                            */
            /* if (bit(para, 15) = 0)  goto notready;            */
            /* Now the conversion for NIM ADC`s are ready        */
            /* dummy = *pNIMPATTERN;                             */
            
            if((patcopy1&0x5555)!=0){   /*Only bit 0, 2,... valid for NIM ADCs*/
               WAITupdate(loopNIM,loopTOT);             /*Waiting for NIM ADCs*/  
               for(i = 0; i < 15; i+=2){
                  if( bit(patcopy1, i) == 1){ 
                     *(pbuf+p1) = swap(*(pNIMadc+i));
                     p1         = p1 + 1;
                     *(pbuf+p1) = swap(*(pNIMadc+i+1));
                     p1         = p1 + 1;
                  }
               }
            }
         }

         f = 0;                                              /*Read CAMAC data*/

         /*:::::::::::::::::::::::::::::::::::::::::::::::::*/
         /*  Data structure and readout for TPU2            */
         /*  TPU is dedicated to CAMAC Silena ADC`s & TDC`s */
         /*:::::::::::::::::::::::::::::::::::::::::::::::::*/
         patcopy2 = *pT2PATTERN;                 /*Take a copy of TPU2 pattern*/
         if (patcopy2 != 0){
            *(pbuf+p1) = swap(0x800B);
            p1         = p1 + 1;
            *(pbuf+p1) = swap(patcopy2);                     /*Store pattern 2*/
            p1         = p1 + 1;
            WAITupdate(loopCAMAC,loopTOT);                 /*Waiting for CAMAC*/
 
            for (i = 0; i < 8; i++){
               if( bit(patcopy2, i) == 1){ 
                  cssa(f, cadc1[i], &tempc, &q);                   /*Read ADC1*/
                  if(tempc > 3839)tempc = 0;
                  *(pbuf+p1) = swap(tempc);
                  p1         = p1 + 1;
                  cssa(f, ctdc1[i], &tempc, &q);                   /*Read TDC1*/
                  if(tempc > 3839)tempc = 0;
                  *(pbuf+p1) = swap(tempc);
                  p1         = p1 + 1;
               }
            }
            for (i = 0; i < 8 ; i++){
               if( bit(patcopy2, i+8) == 1){ 
                  cssa(f, cadc2[i], &tempc, &q);                   /*Read ADC2*/
                  if(tempc > 3839)tempc = 0;
                  *(pbuf+p1) = swap(tempc);
                  p1         = p1 + 1;
                  cssa(f, ctdc2[i], &tempc, &q);                   /*Read TDC2*/
                  if(tempc > 3839)tempc = 0;
                  *(pbuf+p1) = swap(tempc);
                  p1         = p1 + 1;
               }
            }
         }
         /*:::::::::::::::::::::::::::::::::::::::::::::::::*/
         /*  Data structure and readout for TPU3            */
         /*  TPU is dedicated to CAMAC Silena ADC`s & TDC`s */
         /*:::::::::::::::::::::::::::::::::::::::::::::::::*/
         patcopy3  = *pT3PATTERN;                /*Take a copy of TPU3 pattern*/
         if (patcopy3 != 0){
            *(pbuf+p1) = swap(0x800C);
            p1         = p1 + 1;
            *(pbuf+p1) = swap(patcopy3);                      /*Store pattern3*/
            p1         = p1 + 1;
            WAITupdate(loopCAMAC,loopTOT);                 /*Waiting for CAMAC*/  
            for (i = 0; i < 8; i++){
               if( bit(patcopy3, i) == 1){ 
                  cssa(f, cadc3[i], &tempc, &q);                   /*Read ADC3*/
                  if(tempc > 3839)tempc = 0;
                  *(pbuf+p1) = swap(tempc);
                  p1         = p1 + 1;
                  cssa(f, ctdc3[i], &tempc, &q);                   /*Read TDC3*/
                  if(tempc > 3839)tempc = 0;
                  *(pbuf+p1) = swap(tempc);
                  p1         = p1 + 1;
               }
            }
            for (i = 0; i < 8; i++){
               if( bit(patcopy3, i+8) == 1){ 
                  cssa(f, cadc4[i], &tempc, &q);                   /*Read ADC4*/
                  if(tempc > 3839)tempc = 0;
                  *(pbuf+p1) = swap(tempc);
                  p1         = p1 + 1;
                  cssa(f, ctdc4[i], &tempc, &q);                   /*Read TDC4*/
                  if(tempc > 3839)tempc = 0;
                  *(pbuf+p1) = swap(tempc);
                  p1         = p1 + 1;
               }
            }
         }
      }                                                          /*End of TPUS*/
  

      /*:::::::::::::::::::::::::::::::::::::::::::::::::*/
      /*  Data structure and readout for TPU4 (virtuel)  */
      /*  TPU is dedicated to pile-up CAMAC module/JW    */
      /*:::::::::::::::::::::::::::::::::::::::::::::::::*/
      WAITupdate(loopPUR,loopTOT);                       /*Waiting for pile-up*/  
      cssa(1,cpur1[0], &tpp0, &q);                              /*Read PUR1(0)*/
      cssa(1,cpur1[1], &tpp1, &q);                              /*Read PUR1(1)*/
      tempc = tpp0 | tpp1;
      if(tempc != 0){
         *(pbuf+p1) = swap(0x800D); 
         p1         = p1 + 1;
         *(pbuf+p1) = swap(tpp0); 
         p1         = p1 + 1;
         *(pbuf+p1) = swap(tpp1); 
         p1         = p1 + 1;
      }
       

      /*:::::::::::::::::::::::::::::::::::::::::::::::::*/
      /*  Data structure and readout for TPU5 (virtuel)  */
      /*  TPU is dedicated to SIRI dE-E telescopes       */
      /*:::::::::::::::::::::::::::::::::::::::::::::::::*/
      sirievent:
      if(acqmode == 2) goto notsiri;
      if(acqmode == 1 | acqmode == 3){
         WAITupdate(loopSIRI,loopTOT);                      /*Waiting for SIRI*/  
         SIRI = !bit(*pSIRICOST, 7);                /*Bit7=0 means event (eoe)*/
      }
      if(leaveloop == 'y') return;                    /*Jump out of event-loop*/
      if (SIRI > 0){
         evno[0]   = *pSIRIDSR & 0xff00;
         chipad[0] = *pSIRIDSR & 0x007f;
         energy[0] = 0x1000 - (*(pSIRIDSR+1) & 0x0fff);
         for(i = 1; i < 32; i++){
            j = 2 * i;
            evno[i]     = *(pSIRIDSR+j) & 0xff00;
            if(evno[i] != evno[0])break;
            chipad[i]   = *(pSIRIDSR+j) & 0x007f;
            energy[i]   = 0x1000 - (*(pSIRIDSR+j+1) & 0x0fff);
         }
         imax = i;
                           /*Making bit patterns. Assumes that the E-detectors*/
                           /*are always 32 channels above the dE detectors    */
         SIRIpat1 = 0;
         SIRIpat2 = 0;
         SIRIpat3 = 0;
         SIRIpat4 = 0;
         for(i = 0; i < imax; i++){
            chipadi = chipad[i];
            for(j = 0; j < 32; j++){
               if(chipadi < 32){
                  if(j == chipadi){
                     dE[j] = energy[i];
                     SIRIpat1 = SIRIpat1 | (one << j);
                     goto identified;
                  } 
               }
               if(chipadi < 64){
                  if(j == assignE[chipadi] - 32){  /*NB, assign E trapez -> dE*/
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
/*
 
printf("SIRI-event1 no 0x%x  pattern 0x%x energy 0x%x \n", evno[0], chipad[0], energy[0]);
printf("SIRI-event2 no 0x%x  pattern 0x%x energy 0x%x \n", evno[1], chipad[1], energy[1]);
printf("Number of energies %d \n", imax);
printf("Patterns 0x%x  0x%x  0x%x  0x%x \n", SIRIpat1, SIRIpat2, SIRIpat3, SIRIpat4);
  
*/
         for(i = 0; i < 32; i++){
            evno[i]=0xffff;
         }



         *(pbuf+p1) = swap(0x800E);       /*Defining TPU5 (SIRI-detector)     */
         p1         = p1 + 1;
         *(pbuf+p1) = swap(SIRIpat1);     /*Pattern 1 for 32 first dE-E events*/
         p1         = p1 + 1;
         *(pbuf+p1) = swap(SIRIpat2);     /*Pattern 2 for 32 next telescopes  */
         p1         = p1 + 1;
         *(pbuf+p1) = swap(SIRIpat3);     /*Pattern 3 and 4 for future use    */
         p1         = p1 + 1;            
         *(pbuf+p1) = swap(SIRIpat4);     /*Written on tape, but not used     */
         p1         = p1 + 1;

         for(i = 0; i < 32; i++){
            if( bit(SIRIpat1, i) == 1){   /*Bit i is set, dE and E is written */
               *(pbuf+p1) = swap(dE[i]);
               p1         = p1 + 1;
               *(pbuf+p1) = swap(E[i]);
               p1         = p1 + 1;
               dE[i]      = 0;
               E[i]       = 0;
            }
         }
         for(i = 32; i < 64; i++){
            if( bit(SIRIpat2, (i - 32)) == 1){
               *(pbuf+p1) = swap(dE[i]);
               p1         = p1 + 1;
               *(pbuf+p1) = swap(E[i]);
               p1         = p1 + 1;
               dE[i]      = 0;
               E[i]       = 0;
            }
         } 
/* to be used for future expension with 128 dE-E telescopes            
         for(i = 64; i < 96; i++){
           if( bit(SIRIpat3, (i - 64)) == 1){
               *(pbuf+p1) = swap(dE[i]);
               p1         = p1 + 1;
               *(pbuf+p1) = swap(E[i]);
               p1         = p1 + 1;
               dE[i]      = 0;
               E[i]       = 0;
            }
         }
         for(i = 96; i < 128; i++){
           if( bit(SIRIpat4, (i - 96)) == 1){
               *(pbuf+p1) = swap(dE[i]);
               p1         = p1 + 1;
               *(pbuf+p1) = swap(E[i]);
               p1         = p1 + 1;
               dE[i]      = 0;
               E[i]       = 0;
            }
         } */

      }                                                  /*End of SIRI        */
      notsiri:

      *(pbuf+header) = swap((p1-header)+EVENT_HEADER);
      events         = events + 1;
   }                                                     /*End of infinit loop*/
   return;
}                                                        /*End function loop  */


void Menu() {
   printf("\n");
   printf("      A : Acquisition mode for SIRI        \r\n");
   printf("      R : Run infinite event-loop          \r\n");
   printf("      O : One-buffer run                   \r\n");
   printf("      D : Dump data buffer (buffer one)    \r\n");
   printf("      S : Status                           \r\n");
   printf("      E : Enable/Disable TPUs              \r\n");
   printf("      M : Microsecond delay of events      \r\n");
   printf("      P : Parameters in SIRI/TPU locations \r\n");
   printf("      H : Help, listing of this menu       \r\n");
   printf(" Ctrl_C : Jump out of infinite event-loop  \r\n");
   printf("      * : Exit                             \r\n");
   printf("\n");
   return;
}


int readchar(){
   int c1;
   int c2 = '\n';
   while((c1 = getchar()) != '\n')
	c2 = tolower(c1);
   return c2;
}


void keyb_int(int sig_num){                     /* Keyboard interrupt routine */
   if (sig_num == SIGINT) {
      printf("\n\nLeaving infinite event-loop...\n");
      printf("Type r if you want to restart VME-acquisition\n");
      leaveloop = 'y';
   }
}


void Dumpbuf(){
   int nettowords;
   float nzro = 0.;
   float nabc = 0.;
   float npur = 0.;
   float nsir = 0.;
   float nxxx = 0.;
   float nozr = 0.;
   float pzro = 100.;
   int evnr = 0;
   int i1    = 500;
   int i2    = MAXBUF - 500;

   printf("Dump of 500 first words and 500 last words in buffer"); 

   for(i = 0; i < MAXBUF + 1; i++){
      par=swap(*(pbuf+i));
      if(par==0) nzro = nzro + 1.;
      if(par==0x800a|par==0x800b|par==0x800c) nabc = nabc + 1.;
      if(par==0x800d) npur = npur + 1.;
      if(par==0x800e) nsir = nsir + 1.;
      if(par==0x800f) nxxx = nxxx + 1.;
      if((par&0xffffff00) == 0xf000)evnr = evnr + 1.;/*Masking out eventheader*/
      if(par != 0) nozr = nozr + 1.;
         
      if(par == 0x800a){if(i<i1 | i>i2)printf("\n");}
      if(par == 0x800b){if(i<i1 | i>i2)printf("\n");}
      if(par == 0x800c){if(i<i1 | i>i2)printf("\n");}
      if(par == 0x800d){if(i<i1 | i>i2)printf("\n");}
      if(par == 0x800e){if(i<i1 | i>i2)printf("\n");}
      if(par == 0x800f){if(i<i1 | i>i2)printf("\n");}           /*Up to 6 TPUs*/
      if((par&0xffffff00) == 0xf000){                           /*Eventheader */
      if(i<i1 | i>i2)printf("\n\n");
      if(i<i1 | i>i2)printf(" %x    (Event  %d    Word  %d)",par,evnr,i+1);
      }else{
   	   if(i<i1|i>i2)printf(" %x",par); 
      }
   }
   nozr = (nozr-evnr-2.*nabc-3.*npur-3.*nsir-2.*nxxx);        /*True energies */
   nzro = nzro-(float)SAFE; 
   if (nozr > 0){
      pzro = 100.*nzro/(nzro + nozr);
   }else{
      pzro = 100.;
   }
   nettowords = MAXBUF + 1 - SAFE;
   printf("\nScanning buffer and making estimates:\n"); 
   printf("Netto words ( -200 zeros at end)   %6d \n", nettowords);
   printf("Eventheader words                  %6d \n", (int)evnr);
   printf("Patterns from TPU 1, TPU 2 or TPU3 %6d \n", (int)nabc);
   printf("Patterns from TPU 4 (pile-up)      %6d \n", (int)npur);
   printf("Patterns from TPU 5 (Siri)         %6d \n", (int)nsir);
   printf("Patterns from TPU 6 (not used)     %6d \n", (int)nxxx);
   printf("Data (energy, time...) equal 0     %6d \n", (int)nzro);
   printf("Data (energy, time...) not equal 0 %6d \n", (int)nozr);
   printf("Percent data with content 0 is %6.1f\n", pzro);
   printf("\nWaiting for pile-up                %4.1f us\n", uPUR); 
   printf("Conversion time for NIM ADCs       %4.1f us\n", uNIM); 
   printf("Conversion time for CAMAC ADCs     %4.1f us\n", uCAMAC); 
   printf("Conversion time for SIRI ADCs      %4.1f us\n", uSIRI); 
   printf("General delay before reading event %4.1f us\n", uEVENT);
   printf("\n"); 
   return;
}                                                       /*End function Dumpbuf*/
      
void Acquisitionmode(){
   printf("\n");
   printf("Choose acquisition mode:                          \n");
   printf("If TPU1 fires, SIRI + everything is read out  (1) \n");
   printf("As (1), but SIRI excluded                     (2) \n");
   printf("As (1), but only SIRI included                (3) \n");
   printf("Only SIRI, self gated without TPU1 as trigger (4) \n");
   printf("\n");
   printf("Type 1, 2, 3 or 4 (defaults by CR):\n");
   acqmode = 2;
   printf("Aquisition method              (%d):", acqmode); 
   gets(inbuf);
   sscanf(inbuf,"%d",&acqmode);

   printf("Acquisition mode = ");
   if(acqmode == 1){
      printf("1: TPU1 triggers all detectors + SIRI \n");
   }
   if(acqmode == 2){
      printf("2: TPU1 triggers all detectors except SIRI \n");
   }
   if(acqmode == 3){
      printf("3: TPU1 triggers only SIRI \n");
   }
   if(acqmode == 4){
      printf("4: Only SIRI, self-triggered without TPU1 \n");
   }

   printf("\n"); 
   return;
}

void Status(){ 
   int messlow, messhigh;
   int movelow, movehigh;

   bufferbytes = 4 * (MAXBUF+1);
   messlow  = SLV24ADDR;  
   messhigh = SLV24ADDR + 4 * (MESSNUM-1);
   movelow   = vad;
   movehigh  = vad + 4 * (((MAXBUF+1)/4)-1);

   printf("Length of eventbuffer in RIO2:     0x%x bytes\n", bufferbytes);
   printf("Length of movebuffer RIO2 <-> Sun: 0x%x bytes\n", movebytes);
   printf("Length of messagebox RIO2 <-> Sun:   0x%x bytes\n", messagebytes);
   printf("Memory locations for remote access: Sun -> Bit3 -> RIO2: \n");
   printf("Message box (VME A24 slave): 0x%x - 0x%x\n", messlow, messhigh);
   printf("Movebuffer  (VME A24 slave): 0x%x - 0x%x\n", movelow, movehigh);
   par=*(pmes+0);
   printf("Messagebox 0: Address VME  0x%x \n",swap(par));
   par=*(pmes+1);
   printf("Messagebox 1: Bufferlength 0x%x words\n",swap(par));
   par=*(pmes+2);
   printf("Messagebox 2: Semaphore 1  0x%x \n",swap(par));
   par=*(pmes+3);
   printf("Messagebox 3: Semaphore 2  0x%x \n",swap(par));
   par=*(pmes+4);
   printf("Messagebox 4: Semaphore 3  0x%x \n",swap(par));
   par=*(pmes+5);
   printf("Messagebox 5: Semaphore 4  0x%x \n",swap(par));
   par=*(pmes+6);
   printf("Messagebox 6: VME status   0x%x \n",swap(par));
   par=*(pmes+7);
   printf("Messagebox 7: SUN status   0x%x \n",swap(par));
   par=*(pmes+8);
   printf("Messagebox 8: Not used     0x%x \n",swap(par));
   par=*(pmes+9);
   printf("Messagebox 9: Not used     0x%x \n",swap(par));

   printf("TPU1 is always enabled \n");
   if( tpu_2 == 'e' | tpu_2 == 'E' )
      printf("TPU2 is enabled \n");
   else
      printf("TPU2 is disabled \n");
   if( tpu_3 == 'e' | tpu_3 == 'E' )
      printf("TPU3 is enabled \n");
   else
      printf("TPU2 is disabled \n");

   printf("Waiting for pile-up                %4.1f us\n", uPUR); 
   printf("Conversion time for NIM ADCs       %4.1f us\n", uNIM); 
   printf("Conversion time for CAMAC ADCs     %4.1f us\n", uCAMAC); 
   printf("Conversion time for SIRI ADCs      %4.1f us\n", uSIRI); 
   printf("General delay before reading event %4.1f us\n", uEVENT);

   printf("Acquisition mode = ");
   if(acqmode == 1){
      printf("1: TPU1 triggers all detectors + SIRI \n");
   }
   if(acqmode == 2){
      printf("2: TPU1 triggers all detectors except SIRI \n");
   }
   if(acqmode == 3){
      printf("3: TPU1 triggers only SIRI \n");
   }
   if(acqmode == 4){
      printf("4: Only SIRI, self-triggered without TPU1 \n");
   }

   if(eventsold != 0){
      printf("VME data acquisition started at %s\n", sSTART );
      printf("VME data acquisition stopped at %s\n", sSTOP );
      printf("Records = %ld    Events = %ld \n", recordsold, eventsold);
   }
   printf("\n");
   return;
}                                                        /*End function Status*/
      

void Parameters(){ 
   u_short dts;
   u_char  dtc;
/*  printf("SIRI CSR registers:\n");
   dtc = *pSIRICOST;
   printf("SIRICOST    AD=0x%08x, AM=0x%02x: data read: %8x\n",SIRICOST,AM32,dtc);
   dtc = *pSIRIEVEAD;
   printf("SIRIEVEAD   AD=0x%08x, AM=0x%02x: data read: %8x\n",SIRIEVEAD,AM32,dtc); 
   dtc = *pSIRIBUFAD;
   printf("SIRIBUFAD   AD=0x%08x, AM=0x%02x: data read: %8x\n",SIRIBUFAD,AM32,dtc); 

   printf("\n");
   printf("SIRI DSR, patterns and ADC values:\n");
   for(i = 0; i < 32; i++){
      SIRIadc[2*i] = *(pSIRIDSR+2*i); 
   printf("Pattern %2d, AD=0x%08x, AM=0x%02x: data read: %8x\n", i, SIRIDSR+2*(2*i), AM32, SIRIadc[2*i]); 
      SIRIadc[2*i+1] = *(pSIRIDSR+2*i+1); 
   printf("ADC  no %2d, AD=0x%08x, AM=0x%02x: data read: %8x\n", i, SIRIDSR+2*(2*i+1),AM32, SIRIadc[2*i+1]);
   }
*/
   printf("\nTPU registers:\n");
   printf("TPU1:\n");
   dtc = *pNEXTREG;
   printf("NEXTREG     AD=0x%08x, AM=0x%02x: data read: %8x\n",NEXTREG,AM24,dtc);
   dts = *pT1STATUS;
   printf("T1STATUS    AD=0x%08x, AM=0x%02x: data read: %8x\n",T1STATUS,AM24,dts);
   dtc = *pT1NUMREG;
   printf("T1NUMREG    AD=0x%08x, AM=0x%02x: data read: %8x\n",T1NUMREG,AM24,dtc);
   dts = *pT1PATTERN;
   printf("T1PATTERN   AD=0x%08x, AM=0x%02x: data read: %8x\n",T1PATTERN,AM24,dts);

   printf("TPU2:\n");
   dts = *pT2STATUS;
   printf("T2STATUS    AD=0x%08x, AM=0x%02x: data read: %8x\n",T2STATUS,AM24,dts);
   dtc = *pT2NUMREG;
   printf("T2NUMREG    AD=0x%08x, AM=0x%02x: data read: %8x\n",T2NUMREG,AM24,dtc);
   dts = *pT2PATTERN;
   printf("T2PATTERN   AD=0x%08x, AM=0x%02x: data read: %8x\n",T2PATTERN,AM24,dts);

   printf("TPU3:\n");
   dts = *pT3STATUS;
   printf("T3STATUS    AD=0x%08x, AM=0x%02x: data read: %8x\n",T3STATUS,AM24,dts);
   dtc = *pT3NUMREG;
   printf("T3NUMREG    AD=0x%08x, AM=0x%02x: data read: %8x\n",T3NUMREG,AM24,dtc);
   dts = *pT3PATTERN;
   printf("T3PATTERN   AD=0x%08x, AM=0x%02x: data read: %8x\n",T3PATTERN,AM24,dts);

   printf("NIM registers:\n");
   dts = *pNIMCSR;
   printf("NIMCSR      AD=0x%08x, AM=0x%02x: data read: %8x\n",NIMCSR,AM24,dts);
   dts = *pNIMPATTERN;
   printf("NIMPATTERN  AD=0x%08x, AM=0x%02x: data read: %8x\n",NIMPATTERN, AM24,dts);

   printf("NIM ADCs:\n");
   for(i=0; i<16; i++){
       printf("ADC no %2d,  AD=0x%08x, AM=0x%02x: data read: %8x\n", i, NIMbaseaddr+2*i, AM24, *(pNIMadc+i)); 


   }
   printf("\n");
   return;
}                                                    /*End function Parameters*/

      
void DisEnAble(){
   printf("Enable(E) / Disable(D) TPU2 :");
   cmd = readchar();
   if( cmd == 'e') {
      tpu_2 = 'e';
      *pT2STATUS = ENABLE;
      printf("TPU2 enabled \n");
   }
   else if( cmd == 'd') {
      tpu_2 = 'd';
      *pT2STATUS = DISABLE;
      printf( "TPU2 disabled \n");
   }
   else
      printf("Illegal command \n");

   printf("Enable(E) / Disable(D) TPU3 :");
   cmd = readchar();
   if( cmd == 'e') {
      tpu_3 = 'e';
      *pT3STATUS = ENABLE;
      printf("TPU3 enabled \n");
   }
   else if(cmd == 'd') {
      tpu_3 = 'd';
      *pT3STATUS = DISABLE;
      printf( "TPU3 disabled \n");
   }
   else
      printf("Illegal command \n");
   return;   
}                                                     /*End function DisEnAble*/


void Microsecond(){

   printf("\n");
   printf("With this command you can change delay in front of the read-   \n");
   printf("out of various detectors. This is important for ADCs with long \n");
   printf("conversion times. (The delays for a specific detector will be  \n");
   printf("reduced according to the delays of previous ADCs)              \n");
   printf("Suggested values:                                              \n");
   printf("Pile-up rejection      :  8 us                                 \n");
   printf("NIM ADC Silena 7411    : 22 us at 2K, 44 us at 4K, 88 us at 8K \n");
   printf("NIM ADC Silena 7420    :  6 us at 2K, 12 us at 4K, 28 us at 8K \n");
   printf("CAMAC ADCs and TDCs    : 35 us\n");
   printf("SIRI telescopes        :  2 us x detectors fired = 8 us\n");
   printf("In front of event loop :  0 us\n");
   printf("\n");
   printf("Type your values (defaults by CR):\n");

   loopPUR   = (int)((uPUR   / usLOOP) + 0.5);
   loopNIM   = (int)((uNIM   / usLOOP) + 0.5);
   loopCAMAC = (int)((uCAMAC / usLOOP) + 0.5);
   loopSIRI  = (int)((uSIRI  / usLOOP) + 0.5);
   loopEVENT = (int)((uEVENT / usLOOP) + 0.5);

   uPUR   = (float)loopPUR   * usLOOP;
   uNIM   = (float)loopNIM   * usLOOP;
   uCAMAC = (float)loopCAMAC * usLOOP;
   uSIRI  = (float)loopSIRI  * usLOOP;
   uEVENT = (float)loopEVENT * usLOOP;
   
   printf("Waiting for pile-up                (%4.1f us) : ", uPUR); 
   gets(inbuf);
   sscanf(inbuf,"%f",&uPUR);
   printf("Conversion time for NIM ADCs       (%4.1f us) : ", uNIM); 
   gets(inbuf);
   sscanf(inbuf,"%f",&uNIM);
   printf("Conversion time for CAMAC ADCs     (%4.1f us) : ", uCAMAC); 
   gets(inbuf);
   sscanf(inbuf,"%f",&uCAMAC);
   printf("Conversion time for SIRI ADCs      (%4.1f us) : ", uSIRI); 
   gets(inbuf);
   sscanf(inbuf,"%f",&uSIRI);
   printf("General delay before reading event (%4.1f us) : ", uEVENT); 
   gets(inbuf);
   sscanf(inbuf,"%f",&uEVENT);

   loopPUR   = (int)((uPUR   / usLOOP) + 0.5);
   loopNIM   = (int)((uNIM   / usLOOP) + 0.5);
   loopCAMAC = (int)((uCAMAC / usLOOP) + 0.5);
   loopSIRI  = (int)((uSIRI  / usLOOP) + 0.5);
   loopEVENT = (int)((uEVENT / usLOOP) + 0.5);

   uPUR   = (float)loopPUR   * usLOOP;
   uNIM   = (float)loopNIM   * usLOOP;
   uCAMAC = (float)loopCAMAC * usLOOP;
   uSIRI  = (float)loopSIRI  * usLOOP;
   uEVENT = (float)loopEVENT * usLOOP;

   printf("\nWaiting for pile-up                %4.1f us", uPUR); 
   printf("\nConversion time for NIM ADCs       %4.1f us", uNIM); 
   printf("\nConversion time for CAMAC ADCs     %4.1f us", uCAMAC); 
   printf("\nConversion time for SIRI ADCs      %4.1f us", uSIRI); 
   printf("\nGeneral delay before reading event %4.1f us", uEVENT); 
   printf("\n(The values are multiples of %8.4f us)", usLOOP); 

   printf("\n");
   return;   
}                                                   /*End function Microsecond*/


void SLAVEinitiate(){
   #define PCI_CTL0 0xa0f50000
   #define PCI_CTL1 0xa0f50004
   #define PCI_CTL2 0xa0f50008
   #define PCI_CTL3 0xa0f5000c

   long adsl, dtsl, memsl, *psl;

   printf("Enable slave A24 memory (writting to PCI_CTL registers)...");

   adsl = PCI_CTL2;
   dtsl = 0x01400000;
   if (!(memsl=(int)smem_create("MEMsl",(char *)(adsl&~0xFFF), 0x4,SM_WRITE)))
   {
      fprintf(stderr,"Unable to allocate MEMsl window\n");
      exit(0);
   }
   psl = (long *)(memsl | (adsl&0xFFF));
   *psl = dtsl;
   /*printf("CTL2 at phys AD=%x,log AD=0x%08x:set to: %8x\n",adsl,psl,dtsl);*/
   smem_create("MEMsl",(char *)memsl,0x4,SM_DETACH);
   smem_remove("MEMsl");

   adsl = PCI_CTL3;
   dtsl = 0x5f000000;
   if (!(memsl=(int)smem_create("MEMsl",(char *)(adsl&~0xFFF), 0x4,SM_WRITE)))
   {
      fprintf(stderr,"Unable to allocate MEMsl window\n");
      exit(0);
   }
   psl = (long *)(memsl | (adsl&0xFFF));
   *psl = dtsl;
   /*printf("CTL3 at phys AD=%x,log AD=0x%08x:set to: %8x\n",adsl,psl,dtsl);*/
   smem_create("MEMsl",(char *)memsl,0x4,SM_DETACH);
   smem_remove("MEMsl");

   adsl = PCI_CTL2;
   dtsl = 0x00400000;
   if (!(memsl=(int)smem_create("MEMsl",(char *)(adsl&~0xFFF), 0x4,SM_WRITE)))
   {
      fprintf(stderr,"Unable to allocate MEMsl window\n");
      exit(0);
   }
   psl = (long *)(memsl | (adsl&0xFFF));
   *psl = dtsl;
  /*printf("CTL2 at phys AD=%x,log AD=0x%08x:set to: %8x\n",adsl,psl,dtsl);*/
   smem_create("MEMsl",(char *)memsl,0x4,SM_DETACH);
   smem_remove("MEMsl");

   adsl = PCI_CTL3;
   dtsl = 0x00800000;
   if (!(memsl=(int)smem_create("MEMsl",(char *)(adsl&~0xFFF), 0x4,SM_WRITE)))
   {
      fprintf(stderr,"Unable to allocate MEMsl window\n");
      exit(0);
   }
   psl = (long *)(memsl | (adsl&0xFFF));
   *psl = dtsl;
   /*printf("CTL3 at phys AD=%x,log AD=0x%08x: et to: %8x\n",adsl,psl,dtsl);*/
   smem_create("MEMsl",(char *)memsl,0x4,SM_DETACH);
   smem_remove("MEMsl");

   printf("DONE \n");
   return;
}


void A24open(){
   messagebytes = 0x28;
   movebytes    = 0x8000;
   slavebytes   = messagebytes + movebytes;
   printf("Allocating SRAM A24 slave memory \nfor messagebox (0x%x bytes) and buffer (0x%x bytes)...", messagebytes, movebytes);
   if (!(mem24=(int)smem_create("MEM24",(char *)(CPUADDR&~0xFFF), slavebytes,
      SM_WRITE | SM_READ))){
      fprintf(stderr,"Unable to allocate MEM window for slave A24 memory");
      exit(0);
   }
   pmes = (long *)(mem24 | (CPUADDR&0xFFF));          /*First 10 message-words*/
   pBUFFER_ADDRESS = pmes + 0;
   pBUFFER_LENGTH  = pmes + 1;
   pSEMA_1         = pmes + 2;
   pSEMA_2         = pmes + 3;
   pSEMA_3         = pmes + 4;
   pSEMA_4         = pmes + 5;
   pVMESTATUS      = pmes + 6;
   pSUNSTATUS      = pmes + 7;
   pmove = pmes + (messagebytes/4);         /*Start of eventbuffer for program*/ 
   vad   = SLV24ADDR + messagebytes; /*Start of eventbuffer for other CPUs*/
   printf("DONE \n");
   return;
}


void BUFFERopen(){
   printf("Opening eventbuffer...");
   pbuf = (long *)calloc(MAXBUF+1,sizeof(long));
   printf("DONE \n");
   return;
}


void BUFFERclear(){
   printf("Zeroing eventbuffer, movebuffer and semaphores...");
   for( i = 0; i < MAXBUF+1; i++){                         /*Clear eventbuffer*/
      *(pbuf+i) = 0;
   }  
   for( i = 0; i < ((MAXBUF+1)/4)+1; i++){
      *(pmove+i) = 0;
   }
 
   p1         = 0;
   *pSEMA_1   = swap(EMPTY);         /*Set semaphore for buffer to empty state*/
   *pSEMA_2   = swap(EMPTY);
   *pSEMA_3   = swap(EMPTY);         /*Set semaphore for buffer to empty state*/
   *pSEMA_4   = swap(EMPTY);
   curmax     = MAXBUF;             /*Initialise the roof of buffer           */
   printf("DONE \n");
   return;
}


void BUFFERclose(){
   printf("Closing eventbuffer...");
   free(pbuf);
   printf("DONE \n");
   return;
}


void MESSAGEBOXconfig(){
   printf("Writing status, moveaddress and bufferlength to messagebox...");
   *(pmes+8) = 0;                          /*Zeroing unused locations         */
   *(pmes+9) = 0;
   *pBUFFER_ADDRESS = swap(vad);           /*Start address of eventbuffer     */
   *pBUFFER_LENGTH  = swap(MAXBUF+1);      /*Store buffer length              */
   *pVMESTATUS      = swap(STOP);          /*Eventbuilder is not yet running  */
   *pSUNSTATUS      = swap(STOP);          /*Sirius has to be restarted to run*/
   printf("DONE \n");
   return;
}


void A24close(){
   *pVMESTATUS      = swap(STOP);
   printf("Closing A24 slave memory...");
   smem_create("MEM24",(char *)mem24, slavebytes,SM_DETACH);
   smem_remove("MEM24");
   printf("DONE \n");
   return;
}


void CAMACopen(){
   printf("Opening CAMAC, ");
   copen();
   printf("...DONE \n");
   return;
}      
  

void CAMACclose(){
   printf("Closing CAMAC...");
   cclose();
   printf("DONE \n");
   return;
}      


void CAMACconfig(){                      /*Subroutine to configure the setup  */
   u_short statw = 0xA00;                /*LAM disable,OVF disable,SUB disable*/
   u_short puren = 0;
   u_short dummy;   
   int astat1,astat2,astat3,astat4;
   int tstat1,tstat2,tstat3,tstat4;
   int pcont1;
   printf("Configuring CAMAC registers...");

                       /* Define the the symbolic addresses of CAMAC devices: */
   for ( i = 0; i < 8; i++)                                 /* Camac ADC1     */
      cdreg(&cadc1[i], BRANCH, CRATE, ADC1SLOT, i);         /* Data reg. 0-7  */
   cdreg(&astat1, BRANCH, CRATE, ADC1SLOT, 14);             /* Status register*/
   
   for ( i = 0; i < 8; i++)                                 /* Camac ADC2     */
      cdreg(&cadc2[i], BRANCH, CRATE, ADC2SLOT, i);         /* Data reg. 0-7  */
   cdreg(&astat2, BRANCH, CRATE, ADC2SLOT, 14);             /* Status register*/
   
   for ( i = 0; i < 8; i++)                                 /* Camac ADC3     */
      cdreg(&cadc3[i], BRANCH, CRATE, ADC3SLOT, i);         /* Data reg. 0-7  */
   cdreg(&astat3, BRANCH, CRATE, ADC3SLOT, 14);             /* Status register*/
   
   for ( i = 0; i < 8; i++)                                 /* Camac ADC4     */
      cdreg(&cadc4[i], BRANCH, CRATE, ADC4SLOT, i);         /* Data reg. 0-7  */
   cdreg(&astat4, BRANCH, CRATE, ADC4SLOT, 14);             /* Status register*/
   
   for ( i = 0; i < 8; i++)                                 /* Camac TDC1     */
      cdreg(&ctdc1[i], BRANCH, CRATE, TDC1SLOT, i);         /* Data reg. 0-7  */
   cdreg(&tstat1, BRANCH, CRATE, TDC1SLOT, 14);             /* Status register*/
   
   for ( i = 0; i < 8; i++)                                 /* Camac TDC2     */
      cdreg(&ctdc2[i], BRANCH, CRATE, TDC2SLOT, i);         /* Data reg. 0-7  */
   cdreg(&tstat2, BRANCH, CRATE, TDC2SLOT, 14);             /* Status register*/
   
   for ( i = 0; i < 8; i++)                                 /* Camac TDC3     */
      cdreg(&ctdc3[i], BRANCH, CRATE, TDC3SLOT, i);         /* Data reg. 0-7  */
   cdreg(&tstat3, BRANCH, CRATE, TDC3SLOT, 14);             /* Status register*/
   
   for ( i = 0; i < 8; i++)                                 /* Camac TDC4     */
      cdreg(&ctdc4[i], BRANCH, CRATE, TDC4SLOT, i);         /* Data reg. 0-7  */
   cdreg(&tstat4, BRANCH, CRATE, TDC4SLOT, 14);             /* Status register*/

   for ( i= 0; i< 4; i++)                                   /* Camac PUR1     */
      cdreg(&cpur1[i],BRANCH,CRATE,PUR1SLOT,i);             /* Data reg. 0-3  */
   cdreg(&pcont1,BRANCH,CRATE,PUR1SLOT,0);                  /* Controlregister*/
                                                    /*Enable CAMAC ADC/TDC    */
   cccz(cadc1[0]);                                  /*Reset crate             */
   ccci(cadc1[0], 0);                               /*Clear crate inhibit flag*/
   
   f = 20;                                          /*Write to status register*/
   cssa(f, astat1, &statw, &q);
   if(q == 0) printf("Missing Q-response from ADC1\n");
   cssa(f, astat2, &statw, &q);
   if(q == 0) printf("Missing Q-response from ADC2\n");
   cssa(f, astat3, &statw, &q);
   if(q == 0) printf("Missing Q-response from ADC3\n");
   cssa(f, astat4, &statw, &q);
   if(q == 0) printf("Missing Q-response from ADC4\n");
   cssa(f, tstat1, &statw, &q);
   if(q == 0) printf("Missing Q-response from TDC1\n");
   cssa(f, tstat2, &statw, &q);
   if(q == 0) printf("Missing Q-response from TDC2\n");
   cssa(f, tstat3, &statw, &q);
   if(q == 0) printf("Missing Q-response from TDC3\n");
   cssa(f, tstat4, &statw, &q);
   if(q == 0) printf("Missing Q-response from TDC4\n");
                                                    /*Enable PUR              */
   cssa(16, pcont1, &puren, &q);
   cssa( 0, pcont1, &dummy, &q);
   if(q == 0) printf("Missing Q-response from PUR1\n");  
   printf("DONE \n");
   return;   
}                                                   /*End function CAMACconfig*/


void SIRIopen(){
   /************************************************/
   /* Set-up of address pointers for CSR and DSR   */
   /************************************************/
   u_short dts;
   u_char  dtc;
   printf("Opening SIRI VME interface...");

    /*Pointers to CSR register (8 kb x 16 bits)       */
    /*Only a few locations of CSR are used            */
    /*8 words, each 8 bits long, starts at 0xf0f0'4000*/

   pSIRICLR = (u_char *)vme_map(SIRICLR,sizeof(u_char),AM32);
   if (pSIRICLR == (u_char *)0){
      fprintf(stderr,"Unable to map VME address for SIRICLR register\n");
      exit(0);
   }

   pSIRICOST = (u_char *)vme_map(SIRICOST,sizeof(u_char),AM32);
   if (pSIRICOST == (u_char *)0){
      fprintf(stderr,"Unable to map VME address for SIRICOST register\n");
      exit(0);
   }
   dtc = *pSIRICOST;
/*   printf("SIRICOST   AD=0x%08x, AM=0x%02x: data read: %8x\n",SIRICOST,AM32,dtc); */

   pSIRIEVEAD = (u_char *)vme_map(SIRIEVEAD,sizeof(u_char),AM32);
   if (pSIRIEVEAD == (u_char *)0){
      fprintf(stderr,"Unable to map VME address for SIRIEVEAD register\n");
      exit(0);
   }
   dtc = *pSIRIEVEAD;
/*   printf("SIRIEVEAD  AD=0x%08x, AM=0x%02x: data read: %8x\n",SIRIEVEAD,AM32, dtc); */

   pSIRIBUFAD = (u_char *)vme_map(SIRIBUFAD,sizeof(u_char),AM32);
   if (pSIRIBUFAD == (u_char *)0){
      fprintf(stderr,"Unable to map VME address for SIRIBUFAD register\n");
      exit(0);
   }
   dtc = *pSIRIBUFAD;
/*   printf("SIRIBUFAD  AD=0x%08x, AM=0x%02x: data read: %8x\n",SIRIBUFAD,AM32, dtc); */

   pSIRIEVRDY = (u_char *)vme_map(SIRIEVRDY,sizeof(u_char),AM32);
   if (pSIRIEVRDY == (u_char *)0){
      fprintf(stderr,"Unable to map VME address for SIRIEVRDY register\n");
      exit(0);
   }

   pSIRIIRQVC = (u_char *)vme_map(SIRIIRQVC,sizeof(u_char),AM32);
   if (pSIRIIRQVC == (u_char *)0){
      fprintf(stderr,"Unable to map VME address for SIRIIRQVC register\n");
      exit(0);
   }

   pSIRINXBUF = (u_char *)vme_map(SIRINXBUF,sizeof(u_char),AM32);
   if (pSIRINXBUF == (u_char *)0){
      fprintf(stderr,"Unable to map VME address for SIRINXBUF register\n");
      exit(0);
   }

   pSIRINUBUF = (u_char *)vme_map(SIRINUBUF,sizeof(u_char),AM32);
   if (pSIRINUBUF == (u_char *)0){
      fprintf(stderr,"Unable to map VME address for SIRINUBUF register\n");
      exit(0);
   }

/*Pointers to DSR register (8 kb x 16 bits)               */
/*Only first buffer of interrest (no ring buffers assumed)*/
/*256 words, each 16 bit long, starts at 0xf0f0'0000      */

/*   printf("\n");
   printf("SIRI patterns and ADCs:\n");*/
   pSIRIDSR = (u_short *)vme_map(SIRIDSR,64*sizeof(u_short),AM32);
   if (pSIRIDSR == (u_short *)0){
      fprintf(stderr,"Unable to map VME address for SIRIs DSR\n");
      exit(0);
   }
   for(i = 0; i < 32; i++){
      SIRIadc[2*i] = *(pSIRIDSR+2*i); 
/*      printf("Pattern %2d, AD=0x%08x, AM=0x%02x: data read: %8x\n", i, SIRIDSR+2*(2*i), AM32, SIRIadc[2*i]); */
      SIRIadc[2*i+1] = *(pSIRIDSR+2*i+1); 
/*      printf("ADC  no %2d, AD=0x%08x, AM=0x%02x: data read: %8x\n", i, SIRIDSR+2*(2*i+1),AM32, SIRIadc[2*i+1]); */
   }
   printf("DONE \n");
   return;
}                                                      /*End function SIRIopen*/


void SIRIconfig(){              /*One ring buffer - without interrupt handling*/
   printf("Configuring and initialisation of SIRI VME interface...");
   *pSIRICOST  = CLEAR;         /*COST register cleared                       */
   *pSIRICLR   = CLEAR;         /*CLEAR register cleared                      */
   *pSIRIIRQVC = CLEAR;         /*Interrupt vector cleared                    */
   *pSIRINXBUF = CLEAR;         /*Next buffer to be read by VME = 0           */
   *pSIRINUBUF = NUMBUF;        /*Number of ring-buffers (1 - 32) = 1         */
   *pSIRIEVRDY = 1;             /*Event not yet read by ROCO (to prevent data)*/
   *pSIRIEVEAD = CLEAR;         /*Address of SIRI event                       */
   *pSIRIBUFAD = CLEAR;         /*Address of SIRI buffer                      */
   *pSIRICOST  = ENABLE;        /*Master enable of COST                       */
   for(i = 0; i < 64; i++){
      *(pSIRIDSR+i) = 0;
      dE[i]         = 0;
      E[i]          = 0;
   }
   printf("DONE \n");
   return;
}


void SIRIclose (){
   printf("Closing SIRI...");
   *pSIRICOST = DISABLE;
   vme_rel(pSIRICLR,     sizeof(u_char));
   vme_rel(pSIRICOST,    sizeof(u_char));
   vme_rel(pSIRIEVEAD,   sizeof(u_char));
   vme_rel(pSIRIBUFAD,   sizeof(u_char));
   vme_rel(pSIRIEVRDY,   sizeof(u_char));
   vme_rel(pSIRIIRQVC,   sizeof(u_char));
   vme_rel(pSIRINXBUF,   sizeof(u_char));
   vme_rel(pSIRINUBUF,   sizeof(u_char));
   vme_rel(pSIRIDSR, 64*sizeof(u_short));
   printf("DONE \n");
   return;
}


void TPUopen(){
   u_short dts;
   u_char dtc;

   printf("Opening TPUs...");
 /*printf("\n TPU1:\n");*/
   pNEXTREG = (u_char*)vme_map(NEXTREG,sizeof(u_char),AM24);
   if (pNEXTREG == (u_char*)0){
      fprintf(stderr,"Unable to map VME address\n");
      exit(0);
   }
   dtc = *pNEXTREG;
 /*printf("AD=0x%08x, AM=0x%02x: data read: %8x\n",NEXTREG,AM24,dtc); */
  
   pT1STATUS = (u_short *)vme_map(T1STATUS,sizeof(u_short),AM24);
   if (pT1STATUS == (u_short *)0){
      fprintf(stderr,"Unable to map VME address\n");
      exit(0);
   }
   dts = *pT1STATUS;
 /*printf("AD=0x%08x, AM=0x%02x: data read: %8x\n",T1STATUS,AM24,dts); */

   pT1NUMREG = (u_char *)vme_map(T1NUMREG,sizeof(u_char),AM24);
   if (pT1NUMREG == (u_char *)0){
      fprintf(stderr,"Unable to map VME address\n");
      exit(0);
   }
   dtc = *pT1NUMREG;
 /*printf("AD=0x%08x, AM=0x%02x: data read: %8x\n",T1NUMREG,AM24,dtc); */

   pT1PATTERN = (u_short *)vme_map(T1PATTERN,sizeof(u_short),AM24);
   if (pT1PATTERN == (u_short *)0){
      fprintf(stderr,"Unable to map VME address\n");
      exit(0);
   }
   dts = *pT1PATTERN;
 /*printf("AD=0x%08x, AM=0x%02x: data read: %8x\n",T1PATTERN,AM24,dts); */

 /*printf("TPU2:\n");*/
   pT2STATUS = (u_short *)vme_map(T2STATUS,sizeof(u_short),AM24);
   if (pT2STATUS == (u_short *)0){
      fprintf(stderr,"Unable to map VME address\n");
      exit(0);
   }
   dts = *pT2STATUS;
 /*printf("AD=0x%08x, AM=0x%02x: data read: %8x\n",T2STATUS,AM24,dts); */

   pT2NUMREG = (u_char *)vme_map(T2NUMREG,sizeof(u_char),AM24);
   if (pT2NUMREG == (u_char *)0){
      fprintf(stderr,"Unable to map VME address\n");
      exit(0);
   }
   dtc = *pT2NUMREG;
 /*printf("AD=0x%08x, AM=0x%02x: data read: %8x\n",T2NUMREG,AM24,dtc); */

   pT2PATTERN = (u_short *)vme_map(T2PATTERN,sizeof(u_short),AM24);
   if (pT2PATTERN == (u_short *)0){
      fprintf(stderr,"Unable to map VME address\n");
      exit(0);
   }
   dts = *pT2PATTERN;
 /*printf("AD=0x%08x, AM=0x%02x: data read: %8x\n",T2PATTERN,AM24,dts); */

 /*printf("TPU3:\n");*/
   pT3STATUS = (u_short *)vme_map(T3STATUS,sizeof(u_short),AM24);
   if (pT3STATUS == (u_short *)0){
      fprintf(stderr,"Unable to map VME address\n");
      exit(0);
   }
   dts = *pT3STATUS;
 /*printf("AD=0x%08x, AM=0x%02x: data read: %8x\n",T3STATUS,AM24,dts); */

   pT3NUMREG = (u_char *)vme_map(T3NUMREG,sizeof(u_char),AM24);
   if (pT3NUMREG == (u_char *)0){
      fprintf(stderr,"Unable to map VME address\n");
      exit(0);
   }
   dtc = *pT3NUMREG;
 /*printf("AD=0x%08x, AM=0x%02x: data read: %8x\n",T3NUMREG,AM24,dtc); */

   pT3PATTERN = (u_short *)vme_map(T3PATTERN,sizeof(u_short),AM24);
   if (pT3PATTERN == (u_short *)0){
      fprintf(stderr,"Unable to map VME address\n");
      exit(0);
   }
   dts = *pT3PATTERN;
 /*printf("AD=0x%08x, AM=0x%02x: data read: %8x\n",T3PATTERN,AM24,dts); */

 /*printf("NIM REGISTER:\n");*/
   pNIMCSR = (u_short *)vme_map(NIMCSR,sizeof(u_short),AM24);
   if (pNIMCSR == (u_short *)0){
      fprintf(stderr,"Unable to map VME address\n");
      exit(0);
   }
   dts = *pNIMCSR;
 /*printf("AD=0x%08x, AM=0x%02x: data read: %8x\n",NIMCSR,AM24,dts); */

   pNIMPATTERN = (u_short *)vme_map(NIMPATTERN,sizeof(u_short),AM24);
   if (pNIMPATTERN == (u_short *)0){
      fprintf(stderr,"Unable to map VME address\n");
      exit(0);
   }
   dts = *pNIMPATTERN;
 /*printf("AD=0x%08x, AM=0x%02x: data read: %8x\n",NIMPATTERN,AM24,dts); */

 /*printf("NIM ADCs:\n");*/
   pNIMadc = (u_short *)vme_map(NIMbaseaddr,16*sizeof(u_short),AM24);
   if (pNIMadc == (u_short *)0){
      fprintf(stderr,"Unable to map VME address\n");
      exit(0);
   }
   for(i=0; i<16; i++){
    /*printf("ADC no %2d, AD=0x%08x, AM=0x%02x: data read: %8x\n", i, NIMbaseaddr+2*i, AM24, *(pNIMadc+i)); */
   }
   printf("DONE \n");
   return;
}                                                       /*End function TPUopen*/


void TPUconfig(){                             /*Enable NIM Controller as slave*/
    printf("Configuring and initialising TPUs...");
   *pNIMCSR     = CLEAR;                      /*Reset CSR                     */
   *pNIMPATTERN = CLEAR;                      /*Reset Pattern & ADC`s         */
   *pNIMCSR     = ENABLE;                     /*Enable as Slave               */
   *pT1STATUS   = CLEAR;                      /*Reset TPU before running      */
   *pT2STATUS   = ENABLE;                     /*Enable TPU2                   */
   *pT3STATUS   = ENABLE;                     /*Enable TPU3                   */
   tpu_2        = 'e';
   tpu_3        = 'e';
   printf("DONE \n");
   return;
}


void TPUclose(){
   printf("Closing TPUs...");

   vme_rel(pNEXTREG,sizeof(u_char));
   vme_rel(pT1STATUS,sizeof(u_short));
   vme_rel(pT1NUMREG,sizeof(u_char));
   vme_rel(pT1PATTERN,sizeof(u_short));

   vme_rel(pT2STATUS,sizeof(u_short));
   vme_rel(pT2NUMREG,sizeof(u_char));
   vme_rel(pT2PATTERN,sizeof(u_short));

   vme_rel(pT3STATUS,sizeof(u_short));
   vme_rel(pT3NUMREG,sizeof(u_char));
   vme_rel(pT3PATTERN,sizeof(u_short));

   vme_rel(pNIMCSR,sizeof(u_short));
   vme_rel(pNIMPATTERN,sizeof(u_short));
   vme_rel(pNIMadc,16*sizeof(u_short));

   printf("DONE \n");
   return;
}


void eventmonitor(){
   int th, tm;
   float rs, es;
   t2 = time(&now) - t0;
   tx = t2 - t1;
   if(tx >= 60){
      r2 = records;
      e2 = events;
      rx = r2 - r1;
      ex = e2 - e1;
      rs = (float)rx/(float)tx;
      es = (float)ex/(float)tx;
      th = t2/3600;
      tm = ((((float)t2-th*3600.)/60.) + 0.5); 
      printf("%7dh %2dm    %8.2f    %7.2f \n",th,tm,es,rs);
      r1 = r2;
      e1 = e2;
      t1 = t2;
   }
   return;
}
