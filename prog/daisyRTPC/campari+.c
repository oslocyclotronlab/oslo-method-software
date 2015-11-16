#include <stdio.h>
#include <math.h>

char *Qresp= "\n Missing Q-response ";
char *text[196];
unsigned short pardat[196];    /*Contains saved parameters for 4 CAMAC ADC`s  */
                               /* Format:       0-3   Common thresholds ADC`s */
                               /*               4-67  ULD/LLD   ADC`s         */
                               /*              68-99  Offsets   ADC`s         */
                               /*             100-163 ULD/LLD   TDC`s         */
                               /*             164-195 Offsets   TDC`s         */
int n[8];                      /* CAMAC Station addresses                     */
                               /* Symbolic CAMAC registers                    */
int comtr[4];                  /* Common threshold for ADC 1 to 4             */    
int disc1[16];                 /* Upper and lower level disc for ADC 1        */
int disc2[16];                 /* Upper and lower level disc for ADC 2        */
int disc3[16];                 /* Upper and lower level disc for ADC 3        */
int disc4[16];                 /* Upper and lower level disc for ADC 4        */
int disc5[16];                 /* Upper and lower level disc for TDC 1        */
int disc6[16];                 /* Upper and lower level disc for TDC 2        */
int disc7[16];                 /* Upper and lower level disc for TDC 3        */
int disc8[16];                 /* Upper and lower level disc for TDC 4        */
int off1[8];                   /* Offset for ADC 1                            */
int off2[8];                   /* Offset for ADC 2                            */
int off3[8];                   /* Offset for ADC 3                            */
int off4[8];                   /* Offset for ADC 4                            */
int off5[8];                   /* Offset for TDC 1                            */
int off6[8];                   /* Offset for TDC 2                            */
int off7[8];                   /* Offset for TDC 3                            */
int off8[8];                   /* Offset for TDC 4                            */
unsigned short para, value;

/*====================================================*/
/* Silena 4418/V status register :                    */
/*                                                    */
/*      15  14  13  12  11  10   9   8  7        0    */
/*    _____________________________________________   */
/*    |   |   |   |   |   |   |   |   |           |   */
/*    | 0 |CLE|CSR|CCE|OVF|EEN|SUB| 0 |    VSN    |   */
/*    |   |   |   |   |   |   |   |   |           |   */
/*    ---------------------------------------------   */
/*                                                    */
/*       VSN: Logical address, not used               */
/*       SUB: Subaddress, Enable = 0                  */
/*       EEN: ECL/CAMAC, CAMAC = 0                    */
/*       OVF: Overflow flag, Enable = 0               */
/*       CCE: Zero compression                        */
/*       CSR: Addressed/Sequential, Addressed = 0     */
/*       CLE: CAMAC LAM, Enabled = 1                  */
/*                                                    */
/*  Read: F(4)A(14),  Write:F(20)A(14)                */
/*                                                    */
/*                                                    */
/* Format of some ESONE standard CAMAC subroutines :  */
/*                                                    */
/* Declare CAMAC registers:                           */
/*    CDREG (EXT, B, C, N, A)                         */
/*      EXT: external symbolic CAMAC register, int    */
/*      B: Branch, int                                */
/*      C: Crate, int                                 */
/*      N: Station number, int                        */
/*      A: Subaddress (0-15), int                     */
/*                                                    */
/* Clear Crate:                                       */
/*    CCCC (EXT)                                      */
/*      EXT: Any defined register, int                */
/*                                                    */
/*                                                    */
/* Declarations of CAMAC parameters                   */
int statw = 0x4A00;                   /* LAM enabled  */
int c = 1;            /* Crate #, rotary switch on CC */
int b = 1;     /* Branch #, rotary switch on Branch D */
int reg1;
int reg[8];

int i,j,f, q, point;

void menu();
void set_adc_par();
void set_tdc_par();
void writesetup();
void resetcrate();
void initialise(char start);

int main() {
   int cmd;

   printf(" __________________________________________________________   \r\n");
   printf("|                                                          |  \r\n");
   printf("|                       Campari+ 1.2                       |  \r\n");
   printf("|                                                          |  \r\n");
   printf("|              CAMAC parameter set-up for                  |  \r\n");
   printf("|         the CACTUS/SIRI multidetector system             |  \r\n");
   printf("|   (Written for the CES RTPC8067 single board processor   |  \r\n");
   printf("|      with a PowerPC 603 @ 66 MHz CPU running LynxOS)     |  \r\n");
   printf("|                                                          |  \r\n");
   printf("| E-mail  : magne.guttormsen@fys.uio.no                    |  \r\n");
   printf("| Created : 24-02-1998                                     |  \r\n");
   printf("| Modified: 13-09-1998                                     |  \r\n");
   printf("|__________________________________________________________|  \r\n");
   printf("                                                                \r\n");
   printf("New CAMAC settings can either be performed by tailoring the     \r\n");
   printf("campari.dat file, or by using the menus of this program. When   \r\n");
   printf("you are finished, the new settings can be stored by the         \r\n");
   printf("Save set-up to campari.dat file command. Remember, that the old  \r\n");
   printf("campari.dat file will be overwritten. Be sure to have your own  \r\n");
   printf("backup-copy of campari.dat.                                     \r\n");
   printf("                                                                \r\n");

   copen();              /* Open for camac calls (cbdlibc.c)                  */
                         /* Initialise CAMAC station numbers for ADC`s/TDC`s  */
   n[0] = 11;
   n[1] = 12;
   n[2] = 13;
   n[3] = 14;
   n[4] = 15;
   n[5] = 16;
   n[6] = 17;
   n[7] = 18;
                         /*Writing comments to parameters in campari.dat      */
   for(j =  0; j <  4; j++) text[j] = "   ADC threshold";
   for(j =  4; j < 12; j++) text[j] = "   ADC 1, uld";
   for(j = 12; j < 20; j++) text[j] = "   ADC 1, lld";
   for(j = 20; j < 28; j++) text[j] = "   ADC 2, uld";
   for(j = 28; j < 36; j++) text[j] = "   ADC 2, lld";
   for(j = 36; j < 44; j++) text[j] = "   ADC 3, uld";
   for(j = 44; j < 52; j++) text[j] = "   ADC 3, lld";
   for(j = 52; j < 60; j++) text[j] = "   ADC 4, uld";
   for(j = 60; j < 68; j++) text[j] = "   ADC 4, lld";  
   for(j = 68; j <100; j++) text[j] = "   ADC offset";
   for(j =100; j <108; j++) text[j] = "   TDC 1, uld";
   for(j =108; j <116; j++) text[j] = "   TDC 1, lld";
   for(j =116; j <124; j++) text[j] = "   TDC 2, uld";
   for(j =124; j <132; j++) text[j] = "   TDC 2, lld";
   for(j =132; j <140; j++) text[j] = "   TDC 3, uld";
   for(j =140; j <148; j++) text[j] = "   TDC 3, lld";
   for(j =148; j <156; j++) text[j] = "   TDC 4, uld";
   for(j =156; j <164; j++) text[j] = "   TDC 4, lld";
   for(j =164; j <196; j++) text[j] = "   TDC offset";
                             
   for( i = 0; i < 4; i++)   /* Define symbolic registers for common threshold*/
      cdreg(&comtr[i], b, c, n[i], 9);                              /* ADC1-4 */ 
                             
   for( i = 0; i < 16; i++)  /* Define upper and lower discriminator registers*/
      cdreg(&disc1[i], b, c, n[0], i);             /* ADC1, 0-7 LLD, 8-15 ULD */
   for( i = 0; i < 16; i++)
      cdreg(&disc2[i], b, c, n[1], i);             /* ADC2, 0-7 LLD, 8-15 ULD */
   for( i = 0; i < 16; i++)
      cdreg(&disc3[i], b, c, n[2], i);             /* ADC3, 0-7 LLD, 8-15 ULD */
   for( i = 0; i < 16; i++)
      cdreg(&disc4[i], b, c, n[3], i);             /* ADC4, 0-7 LLD, 8-15 ULD */
   for( i = 0; i < 16; i++)
      cdreg(&disc5[i], b, c, n[4], i);             /* TDC1, 0-7 LLD, 8-15 ULD */
   for( i = 0; i < 16; i++)
      cdreg(&disc6[i], b, c, n[5], i);             /* TDC2, 0-7 LLD, 8-15 ULD */
   for( i = 0; i < 16; i++)
      cdreg(&disc7[i], b, c, n[6], i);             /* TDC3, 0-7 LLD, 8-15 ULD */
   for( i = 0; i < 16; i++)
      cdreg(&disc8[i], b, c, n[7], i);             /* TDC4, 0-7 LLD, 8-15 ULD */
                                              
   for( i = 0; i < 8; i++)                    /* Define offset value registers*/
      cdreg( &off1[i], b, c, n[0], i);                                 /* ADC1*/
   for( i = 0; i < 8; i++)
      cdreg( &off2[i], b, c, n[1], i);                                 /* ADC2*/
   for( i = 0; i < 8; i++)
      cdreg( &off3[i], b, c, n[2], i);                                 /* ADC3*/
   for( i = 0; i < 8; i++)
      cdreg( &off4[i], b, c, n[3], i);                                 /* ADC4*/
   for( i = 0; i < 8; i++)
      cdreg( &off5[i], b, c, n[4], i);                                 /* TDC1*/
   for( i = 0; i < 8; i++)
      cdreg( &off6[i], b, c, n[5], i);                                 /* TDC2*/
   for( i = 0; i < 8; i++)
      cdreg( &off7[i], b, c, n[6], i);                                 /* TDC3*/
   for( i = 0; i < 8; i++)
      cdreg( &off8[i], b, c, n[7], i);                                 /* TDC4*/
                                         /* Declare symbolic status registers */
   cdreg(&reg[0], b, c, n[0], 14);       /* Status register ADC 1             */        
   cdreg(&reg[1], b, c, n[1], 14);       /* Status register ADC 2             */
   cdreg(&reg[2], b, c, n[2], 14);       /* Status register ADC 3             */
   cdreg(&reg[3], b, c, n[3], 14);       /* Status register ADC 4             */
   cdreg(&reg[4], b, c, n[4], 14);       /* Status register TDC 1             */         
   cdreg(&reg[5], b, c, n[5], 14);       /* Status register TDC 2             */
   cdreg(&reg[6], b, c, n[6], 14);       /* Status register TDC 3             */
   cdreg(&reg[7], b, c, n[7], 14);       /* Status register TDC 4             */
   cdreg(&reg1  , b, c, n[0], 14);       /* Used for cccz and ccci            */

   initialise('Y');                      /* Initialise ADC`s at startup       */
 
   printf("                                          \r\n");
   printf("A : Set Camac ADC parameters              \r\n");
   printf("B : Set Camac TDC parameters              \r\n");
   printf("C : Save set-up to file campari.dat       \r\n");
   printf("D : Initialise Camac crate                \r\n");
   printf("H : Help, listing of this menu            \r\n");
   printf("* : EXIT                                  \r\n");
   printf("                                          \r\n");
   
   do {
      printf("campari>");
      cmd = readchar();
      switch (cmd)  {
	  case 'a' :set_adc_par(); break;
	  case 'b' :set_tdc_par(); break;
	  case 'c' :writesetup();  break;
	  case 'd' :resetcrate();  break;
	  case 'h' :menu();        break;
	  case '\n': break;
      case '*' : break;
	  default : printf(" Illegal command, try again\n");
      }
   }while (cmd != '*');
   cclose();                            /* Closing for camac calls (cbdlibc.c)*/
   return 0;
}                                       /* End main program                   */


void menu() {
   printf("\n");
   printf(" A : Set Camac ADC parameters       \r\n");
   printf(" B : Set Camac TDC parameters       \r\n");
   printf(" C : Save set up to file campari.dat\r\n");
   printf(" D : Initialise Camac crate         \r\n");
   printf(" H : Help, listing of this menu     \r\n");
   printf(" * : EXIT                           \r\n");
   printf("\n");
   return;
}
        
        
void writesetup() { 
   int cmd;
   FILE *fp;
   printf(" WARNING: This command will (overwrite) current campari.dat file\n");
   printf(" Are you sure (y/n)?");
   cmd = readchar();
   if(cmd == 'y'){
      fp = fopen("campari.dat", "w");
      printf("\n Saving set-up data to file: campari.dat \n");
      for( i = 0; i < 196; i++)
      fprintf( fp, "%6d%6d%s\n" ,i, pardat[i], text[i]);
      fclose(fp);
      return;
   }
   else
   {
      printf(" File not written \n"); 
   }
   return;
}             


void resetcrate() {                                            /* Reset Crate */
   int cmd;
   printf(" WARNING: This command will cause all CAMAC registers to be cleared,\n");
   printf(" and the parameterset described in the campari.dat file will be loaded\n");
   printf(" Are you sure (y/n)?");
   cmd = readchar();
   if(cmd == 'y'){
      printf( "Z-cycle performed\n");
      cccz(reg1);
      initialise('Y');
   }
   else
      printf( " Reset not done \n");
   return;
}


void initialise( char start){        /* Initialise SILENA 4418 ADC`s and TDC`s*/
   int dum;
   char line[128];
   FILE *fp;
   if( start == 'Y'){ 
      printf("\n Reading set-up data from file: campari.dat\n");
      fp = fopen("campari.dat", "r");
      if(fp == NULL){
         printf(" No campari.dat file in your directory\n");
         printf(" Reading default values: /usr/src/ces/examples/cbd/campari.dat\n");
         fp = fopen("/usr/src/ces/examples/cbd/campari.dat", "r");
      }
      for( i = 0; i < 196; i++){
        fgets(line,128,fp);
        sscanf(line, "%hu%hu", &dum, &pardat[i]);
      }
      fclose(fp);

      cccz(reg1);                                  /* Reset Crate             */
      ccci(reg1, 0);                               /* Clear inhibit flag      */
      f = 20;                                      /* Write to Statusregister */
      cssa(f, reg[0], &statw, &q);
      if( q == 0) printf("\n No Q-response from ADC 1 ");
      cssa(f, reg[1], &statw, &q);
      if( q == 0) printf("\n No Q-response from ADC 2 ");
      cssa(f, reg[2], &statw, &q);
      if( q == 0) printf("\n No Q-response from ADC 3 ");
      cssa(f, reg[3], &statw, &q);
      if( q == 0) printf("\n No Q-response from ADC 4 ");
      cssa(f, reg[4], &statw, &q);
      if( q == 0) printf("\n No Q-response from TDC 1 ");
      cssa(f, reg[5], &statw, &q);
      if( q == 0) printf("\n No Q-response from TDC 2 ");
      cssa(f, reg[6], &statw, &q);
      if( q == 0) printf("\n No Q-response from TDC 3 ");
      cssa(f, reg[7], &statw, &q);
      if( q == 0) printf("\n No Q-response from TDC 4 ");
   }   
   
   printf(" Initialising ADCs: Commmon tresholds, ULDs, LLDs, Offsets\n");

   f = 20;                              /* Write to common threshold register */
   for( i = 0; i < 4; i++)
      cssa(f, comtr[i], &pardat[i], &q);
   if( q == 0 ) printf("%s",Qresp);
    
   f = 17;                              /* Write to ULD, LLD registers        */
   for( i = 0; i < 8; i++)
      cssa(f, disc1[i], &pardat[i+4], &q);
   if( q == 0 ) printf("%s",Qresp);
    
   for( i = 8; i < 16; i++)
      cssa(f, disc1[i], &pardat[i+4], &q);
   if( q == 0 ) printf("%s",Qresp);
    
   for( i = 0; i < 8; i++)
      cssa(f, disc2[i], &pardat[i+20], &q);
   if( q == 0 ) printf("%s",Qresp);
    
   for( i = 8; i < 16; i++)
      cssa(f, disc2[i], &pardat[i+20], &q);
   if( q == 0 ) printf("%s",Qresp);
    
   for( i = 0; i < 8; i++)
      cssa(f, disc3[i], &pardat[i+36], &q);
   if( q == 0 ) printf("%s",Qresp);
    
   for( i = 8; i < 16; i++)
      cssa(f, disc3[i], &pardat[i+36], &q);
   if( q == 0 ) printf("%s",Qresp);
    
   for( i = 0; i < 8; i++)
      cssa(f, disc4[i], &pardat[i+52], &q);
   if( q == 0 ) printf("%s",Qresp);
    
   for( i = 8; i < 16; i++)
      cssa(f, disc4[i], &pardat[i+52], &q);
   if( q == 0 ) printf("%s",Qresp);
    
   f = 20;                              /* Write to OFFSET registers          */
   for( i = 0; i < 8; i++)
      cssa(f, off1[i], &pardat[i+68], &q);
   if( q == 0 ) printf("%s",Qresp);
    
   for( i = 0; i < 8; i++)
      cssa(f, off2[i], &pardat[i+76], &q);
   if( q == 0 ) printf("%s",Qresp);
    
   for( i = 0; i < 8; i++)
      cssa(f, off3[i], &pardat[i+84], &q);
   if( q == 0 ) printf("%s",Qresp);
    
   for( i = 0; i < 8; i++)
      cssa(f, off4[i], &pardat[i+92], &q);
   if( q == 0 ) printf("%s",Qresp);

   printf(" Initialising TDCs:                    ULDs, LLDs, Offsets \n");
   f = 17;                              /* Write to ULD, LLD registers        */
   for( i = 0; i < 8; i++)
      cssa(f, disc5[i], &pardat[i+100], &q);
   if( q == 0 ) printf("%s",Qresp);
    
   for( i = 8; i < 16; i++)
      cssa(f, disc5[i], &pardat[i+100], &q);
   if( q == 0 ) printf("%s",Qresp);
    
   for( i = 0; i < 8; i++)
      cssa(f, disc6[i], &pardat[i+116], &q);
   if( q == 0 ) printf("%s",Qresp);
    
   for( i = 8; i < 16; i++)
      cssa(f, disc6[i], &pardat[i+116], &q);
   if( q == 0 ) printf("%s",Qresp);
    
   for( i = 0; i < 8; i++)
      cssa(f, disc7[i], &pardat[i+132], &q);
   if( q == 0 ) printf("%s",Qresp);
    
   for( i = 8; i < 16; i++)
      cssa(f, disc7[i], &pardat[i+132], &q);
   if( q == 0 ) printf("%s",Qresp);
    
   for( i = 0; i < 8; i++)
      cssa(f, disc8[i], &pardat[i+148], &q);
   if( q == 0 ) printf("%s",Qresp);
    
   for( i = 8; i < 16; i++)
      cssa(f, disc8[i], &pardat[i+148], &q);
   if( q == 0 ) printf("%s",Qresp);
          
   f = 20;                              /* Write to OFFSET registers          */
   for( i = 0; i < 8; i++)
      cssa(f, off5[i], &pardat[i+164], &q);
   if( q == 0 ) printf("%s",Qresp);
       
   for( i = 0; i < 8; i++)
      cssa(f, off6[i], &pardat[i+172], &q);
   if( q == 0 ) printf("%s",Qresp);
       
   for( i = 0; i < 8; i++)
      cssa(f, off7[i], &pardat[i+180], &q);
   if( q == 0 ) printf("%s",Qresp);
       
   for( i = 0; i < 8; i++)
      cssa(f, off8[i], &pardat[i+188], &q);
   if( q == 0 ) printf("%s",Qresp);
   return;
}                                       /*End function initialise             */


void set_adc_par() {                    /* Subroutine to set ADC parameters   */
   int cmd;
   void comthresADC();
   void uldADC();
   void lldADC();
   void offADC();
   void menuADC();

   printf("\n");
   printf("  ________________________________________ \r\n");
   printf(" |                                        |\r\n");
   printf(" |          Camac ADC Parameters          |\r\n");
   printf(" |________________________________________|\r\n");
   printf("                                           \r\n");
   printf(" A : Set common threshold                  \r\n");
   printf(" B : Set Upper Level Discriminators (ULD)  \r\n");
   printf(" C : Set Lower Level Discriminators (LLD)  \r\n");
   printf(" D : Set offset values                     \r\n");
   printf(" H : Help, listing of this menu            \r\n");
   printf(" * : Exit from ADC menu                    \r\n");
   printf("                                           \r\n");
      
   do
   {  
      printf("campari/ADC>");
      cmd = readchar();
      switch (cmd)
      {
	  case 'a' :comthresADC(); break;
	  case 'b' :uldADC(); break;
	  case 'c' :lldADC(); break;
	  case 'd' :offADC(); break;
      case 'h' :menuADC();break;
      case '\n':break;
      case '*' :menu();break;
	  default  :printf(" Illegal command, try again\n");
      }
   }while (cmd != '*');
   return;
}


void menuADC() {
printf("\n");
   printf(" A : Set common threshold                  \r\n");
   printf(" B : Set Upper Level Discriminators (ULD)  \r\n");
   printf(" C : Set Lower Level Discriminators (LLD)  \r\n");
   printf(" D : Set offset values                     \r\n");
   printf(" H : Help, listing of this menu            \r\n");
   printf(" * : Exit from ADC menu                    \r\n");
   printf("\n");
   return;
}
        

void comthresADC() { 
   int cmd;
   int mvolt;
   printf("\n Threshold should be about 20 mV");
   point = 0;
   for(j = 0; j < 4; j++) {
      f = 4;                            /* Read common threshold              */
      cssa(f, comtr[j], &para, &q);
      if( q == 0 ) {
          printf("%s", Qresp);
          goto qerror;
      }
      mvolt = ((para/0.2125)+0.5);
      printf("\n Current threshold is %5d mV for ADC%d", mvolt,j);
      printf("\n Change (y/n/*)?");
      cmd = readchar();
      if( cmd == '*') {
         printf( "\n");
         return;
      }
      if( cmd == 'y') {
         label:
         printf(" Give threshold  (0 - 1200 mV)");
         mvolt=readint();
         if( (mvolt > 1200 )|| (mvolt < 0) ){
            printf(" Value (%d) out of range, try again\n",mvolt);
            goto label;
         }
         para = (mvolt*0.2125);
         pardat[point]  = para;
         f = 20;                        /* Write common threshold             */
         cssa(f, comtr[j], &para, &q);
         if( q == 0 ) printf("%s",Qresp);
      }
   qerror:                              /* Missing Q-response detected        */
   point = point+1;
   }
   printf( "\n");
   return;
}
         
        
void uldADC() {              /* Set Upper Level Discriminators (ULD) for ADCs */  
   int cmd;
   int mvolt, uld;
   printf("\n Upper threshold should be about 0 mV\n");
   printf(" Common or individual setting for ULD (c/i)?");
   cmd = readchar();
   if( cmd == 'c') {
      label:
      printf(" Give common ULD (0-1500 mV):");
      mvolt=readint();
      if( (mvolt > 1500 )|| (mvolt < 0) ) {
          printf(" Value (%d) out of range, try again\n",mvolt);
          goto label;
      }
      uld = 255- (mvolt*0.17);
      for(j =  4; j < 12; j++){
         pardat[j]  = uld;
      }
      for(j = 20; j < 28; j++){
         pardat[j]  = uld;
      }
      for(j = 36; j < 44; j++){
         pardat[j]  = uld;
      }
      for(j = 52; j < 60; j++){
         pardat[j]  = uld;
      }
      initialise('N');                  /* Put value ULD into ADC registers   */
   }
   else if( cmd == 'i') {
                                           
      point = 4;                        /* Set ULD  values for CAMAC ADC1     */
      for(j = 0; j < 8; j++){
         f = 1;                         /* Read  ULD  registers               */
         cssa(f, disc1[j], &para, &q);
         if( q == 0 ) printf("%s",Qresp);
         mvolt = ((255-para)/0.17);
         printf("\n ULD is %5d mV for ADC1(ch %d) ", mvolt,j);
         printf("\n Change (y/n/*)?");
         cmd = readchar();
         if( cmd == '*' ) {
            menuADC();
            return;
         }
         if(cmd == 'y') {
            ua1:
            printf(" Give ULD (0 - 1500 mV)");
            mvolt=readint();
            if( (mvolt > 1500) || (mvolt < 0) ){
               printf(" Value (%d) out of range, try again\n",mvolt);
               goto ua1;
            }
            para = 255-(mvolt*0.17);
            pardat[point]  = para;
            f = 17;
            cssa(f, disc1[j], &para, &q);
            if( q == 0 ) printf("%s",Qresp);
         }
         point = point+1;
      }  
                                        /* Set ULD  values for CAMAC ADC2     */
      point = 20;
      for(j = 0; j < 8; j++){
         f = 1;                         /* Read  ULD  registers               */
         cssa(f, disc2[j], &para, &q);
         if( q == 0 ) printf("%s",Qresp);
         mvolt = ((255-para)/0.17);
         printf("\n ULD is %5d mV for ADC2 (ch %d)", mvolt,j);
         printf("\n Change (y/n/*)?");
         cmd = readchar();
         if( cmd == '*' ) {
               menuADC();
               return;
         }
         if(cmd == 'y') {
            ua2:
            printf(" Give ULD (0 - 1500 mV)");
            mvolt=readint();
            if( (mvolt > 1500 )|| (mvolt < 0) ){
               printf(" Value (%d) out of range, try again\n",mvolt);
               goto ua2;
            }
            para = 255-(mvolt*0.17);
            pardat[point]  = para;
            f = 17;
            cssa(f, disc2[j], &para, &q);
            if( q == 0 ) printf("%s",Qresp);
         }
            point = point+1;
      }
                                        /* Set ULD  values for CAMAC ADC3     */
      point = 36;
      for(j = 0; j < 8; j++){
         f = 1;                         /* Read  ULD  registers               */
         cssa(f, disc3[j], &para, &q);
         if( q == 0 ) printf("%s",Qresp);
         mvolt = ((255-para)/0.17);
         printf("\n ULD is %5d mV for ADC3 (ch %d)", mvolt,j);
         printf("\n Change (y/n/*)?");
         cmd = readchar();
         if( cmd == '*' ) {
            menuADC();
            return;
         }
         if(cmd == 'y') {
            ua3:
            printf(" Give ULD (0 - 1500 mV)");
            mvolt=readint();
            if( (mvolt > 1500) || (mvolt < 0 )){
               printf(" Value (%d) out of range, try again\n",mvolt);
               goto ua3;
            }
            para = 255-(mvolt*0.17);
            pardat[point]  = para;
            f = 17;
            cssa(f, disc3[j], &para, &q);
            if( q == 0 ) printf("%s",Qresp);
         }
         point = point+1;
      }
                                        /* Set ULD  values for CAMAC ADC4     */
      point = 52;
      for(j = 0; j < 8; j++) {
         f = 1;                         /* Read  ULD  registers               */
         cssa(f, disc4[j], &para, &q); 
         if( q == 0 ) printf("%s",Qresp);
         mvolt = ((255-para)/0.17);
         printf("\n ULD is %5d mV for ADC4 (ch %d)", mvolt,j);
         printf("\n Change (y/n/*)?");
         cmd = readchar();
         if( cmd == '*' ) {
            menuADC();
            return;
         }
         if(cmd == 'y') {
            ua4:
            printf(" Give ULD (0 - 1500 mV)");
            mvolt=readint();
            if(( mvolt > 1500 )|| (mvolt < 0 )){
               printf(" Value (%d) out of range, try again\n",mvolt);
               goto ua4;
            }
            para = 255-(mvolt*0.17);
            pardat[point]  = para;
            f = 17;
            cssa(f, disc4[j], &para, &q);
            if( q == 0 ) printf("%s",Qresp);
         }
         point = point+1;
      }
   }
   else
   printf( "\n");
   return;
}                                       /* End of function uldADC             */


void lldADC() {              /* Set Lower Level Discriminators (LLD) for ADCs */
   int cmd;
   int mvolt, lld;
   printf("\n Lower threshold should be about 20 mV");
   printf("\n Common or individual setting for LLD (c/i)?");
   cmd = readchar();
   if( cmd == 'c' ) {
      label:
      printf(" Give common LLD (0-1000 mV):");
      mvolt=readint();
      if(( mvolt > 1000) || (mvolt < 0 )) {
         printf(" Value (%d) out of range, try again\n",mvolt);
         goto label;
      }
      lld =mvolt*0.255;
      for(j = 12; j < 20; j++){
         pardat[j]  = lld;
      }
      for(j = 28; j < 36; j++){
         pardat[j]  = lld;
      }
      for(j = 44; j < 52; j++){
         pardat[j]  = lld;
      }
      for(j = 60; j < 68; j++){
         pardat[j]  = lld;
      }
      initialise('N' );                 /* Put value LLD into ADC registers   */
   } 
   else if( cmd == 'i' ) {
                                           
      point =12;                        /* Set LLD  values for CAMAC ADC1     */
      for(j = 8; j < 16; j++){
         f = 1;                         /* Read  LLD  registers               */
         cssa(f, disc1[j], &para, &q);
         if( q == 0 ) printf("%s",Qresp);
         mvolt = para/0.255;
         printf("\n LLD is %5d mV for ADC1 (ch %d)", mvolt,j-8);
         printf("\n Change (y/n/*)?");
         cmd = readchar();
         if( cmd == '*' ) {
            menuADC();
            return;
         }
         if(cmd == 'y') {
            ua1:
            printf(" Give LLD (0 - 1000 mV)");
            mvolt=readint();
            if( (mvolt > 1000) || (mvolt < 0 )){
               printf(" Value (%d) out of range, try again\n",mvolt);
               goto ua1;
            }
            para = mvolt*0.255;
            pardat[point]  = para;
            f = 17;
            cssa(f, disc1[j], &para, &q);
            if( q == 0 ) printf("%s",Qresp);
         }
         point = point+1;
      }
                                        /* Set LLD  values for CAMAC ADC2     */
      point = 28;
      for(j = 8; j < 16; j++) {
         f = 1;                         /* Read  LLD  registers               */
         cssa(f, disc2[j], &para, &q);
         if( q == 0 ) printf("%s",Qresp);
         mvolt = para/0.255;
         printf("\n LLD is %5d mV for ADC2 (ch %d)", mvolt,j-8);
         printf("\n Change (y/n/*)?");
         cmd = readchar();
         if( cmd == '*' ) {
            menuADC();
            return;
         }
         if(cmd == 'y') {
            ua2:
            printf(" Give LLD (0 - 1000 mV)");
            mvolt=readint();
            if(( mvolt > 1000) || (mvolt < 0 )){
               printf(" Value (%d) out of range, try again\n",mvolt);
               goto ua2;
            }
            para = mvolt*0.255;
            pardat[point]  = para;
            f = 17;
            cssa(f, disc2[j], &para, &q);
            if( q == 0 ) printf("%s",Qresp);
         }
         point = point+1;
      }
            
                                        /* Set LLD  values for CAMAC ADC3     */
       point = 44;
       for(j = 8; j < 16; j++){
          f = 1;                        /* Read  LLD  registers               */
          cssa(f, disc3[j], &para, &q);
          if( q == 0 ) printf("%s",Qresp);
          mvolt = para/0.255;
          printf("\n LLD is %5d mV for ADC3 (ch %d)", mvolt,j-8);
          printf("\n Change (y/n/*)?");
          cmd = readchar();
          if( cmd == '*' ) {
             menuADC();
             return;
          }
          if(cmd == 'y') {
             ua3:
             printf(" Give LLD (0 - 1000 mV)");
             mvolt=readint();
             if(( mvolt > 1000) || (mvolt < 0) ){
                printf(" Value (%d) out of range, try again\n",mvolt);
                goto ua3;
             }
             para = mvolt*0.255;
             pardat[point]  = para;
             f = 17;
             cssa(f, disc3[j], &para, &q);
             if( q == 0 ) printf("%s",Qresp);
          }
          point = point+1;
       }
                                        /* Set LLD  values for CAMAC ADC4     */
      point = 60;
      for(j = 8; j < 16; j++){
         f = 1;                         /* Read  LLD  registers               */
         cssa(f, disc4[j], &para, &q);
         if( q == 0 ) printf("%s",Qresp);
         mvolt = para/0.255;
         printf("\n LLD is %5d mV for ADC4 (ch %d)", mvolt,j-8);
         printf("\n Change (y/n/*)?");
         cmd = readchar();
         if( cmd == '*' ) {
            menuADC();
            return;
         }
         if(cmd == 'y') {
            ua4:
            printf(" Give LLD (0 - 1000 mV)");
            mvolt=readint();
            if( (mvolt > 1000 )|| (mvolt < 0 )){
               printf(" Value (%d) out of range, try again\n",mvolt);
               goto ua4;
            }
            para = mvolt*0.255;
            pardat[point]  = para;
            f = 17;
            cssa(f, disc4[j], &para, &q);
            if( q == 0 ) printf("%s",Qresp);
         }
         point = point+1;
      }
   }
   else
   printf( "\n");
   return;
}                                       /* End of function lldADC             */


void offADC() {                         /* Set offsets for ADCs               */
   int cmd;
   int channels, off;
   printf("\n Offset should be about 0 channels");
   printf("\n Common or individual setting for offset (c/i)?");
   cmd = readchar();
   if( cmd == 'c' ) {
      label:
      printf(" Give common offset (-128 ch  -  +127 ch):");
      channels=readint();
      if( (channels > 127) || (channels < -128)) {
         printf(" Value (%d) out of range, try again\n",channels);
         goto label;
      }
      off = channels + 128;
      for(j = 68; j < 100; j++){
         pardat[j] = off;
      }
      initialise('N' );                 /* Put value offset for ADC registers */
   }         
   else if( cmd == 'i' ) {
                                           
      point =68;                        /* Set offset  values for CAMAC ADC1  */
      for(j = 0; j < 8; j++){
         f = 4;                         /* Read  offset  registers            */
         cssa(f, off1[j], &para, &q);
         if( q == 0 ) printf("%s",Qresp);
         channels = para -128;
         printf("\n Offset is %4d channels for ADC1 (ch %d)", channels,j);
         printf("\n Change (y/n/*)?");
         cmd = readchar();
         if( cmd == '*' ) {
            menuADC();
            return;
         }
         if(cmd == 'y') {
            ua1:
            printf(" Give offset ADC1 (-128 ch  -  +127 ch):");
            channels=readint();
            if(( channels > 127) || (channels < -128)){
               printf(" Value (%d) out of range, try again\n",channels);
               goto ua1;
            }
            para = channels +128;
            pardat[point]  = para;
            f = 20;
            cssa(f, off1[j], &para, &q);
            if( q == 0 ) printf("%s",Qresp);
         }
         point = point+1;
      }
            

      point =76;
      for(j = 0; j < 8; j++){
         f = 4;                                     /* Read  offset  registers*/
         cssa(f, off2[j], &para, &q);
         if( q == 0 ) printf("%s",Qresp);
         channels = para -128;
         printf("\n Offset is %4d channels for ADC2 (ch %d)", channels,j);
         printf("\n Change (y/n/*)?");
         cmd = readchar();
         if( cmd == '*' ) {
            menuADC();
            return;
         }
         if(cmd == 'y') {
            ua2:
            printf(" Give offset ADC2 (-128 ch  -  +127 ch):");
            channels=readint();
            if( (channels > 127) || (channels < -128)){
               printf(" Value (%d) out of range, try again\n",channels);
               goto ua2;
            }
            para = channels +128;
            pardat[point]  = para;
            f = 20;
            cssa(f, off2[j], &para, &q);
            if( q == 0 ) printf("%s",Qresp);
        }
        point = point+1;
     }

      point =84;
      for(j = 0; j < 8; j++){
         f = 4;                                     /* Read  offset  registers*/
         cssa(f, off3[j], &para, &q);
         if( q == 0 ) printf("%s",Qresp);
         channels = para -128;
         printf("\n Offset is %4d channels for ADC3 (ch %d)", channels,j);
         printf("\n Change (y/n/*)?");
         cmd = readchar();
         if( cmd == '*' ) {
            menuADC();
            return;
         }
         if(cmd == 'y') {
            ua3:
            printf(" Give offset ADC3 (-128 ch  -  +127 ch):");
            channels=readint();
            if( (channels > 127 )|| (channels < -128)){
               printf(" Value (%d) out of range, try again\n",channels);
               goto ua3;
            }
            para = channels +128;
            pardat[point]  = para;
            f = 20;
            cssa(f, off3[j], &para, &q);
            if( q == 0 ) printf("%s",Qresp);
         }
         point = point+1;
      }

      point =92;
      for(j = 0; j < 8; j++){
         f = 4;                                     /* Read  offset  registers*/
         cssa(f, off4[j], &para, &q);
         if( q == 0 ) printf("%s",Qresp);
         channels = para -128;
         printf("\n Offset is %4d channels for ADC4 (ch %d)", channels,j);
         printf("\n Change (y/n/*)?");
         cmd = readchar();
         if( cmd == '*' ) {
            menuADC();
            return;
         }
         if(cmd == 'y') {
            ua4:
            printf(" Give offset ADC4 (-128 ch  -  +127 ch):");
            channels=readint();
            if(( channels > 127)|| (channels < -128)){
               printf(" Value (%d) out of range, try again\n",channels);
               goto ua4;
            }
            para = channels +128;
            pardat[point]  = para;
            f = 20;
            cssa(f, off4[j], &para, &q);
            if( q == 0 ) printf("%s",Qresp);
         }
         point = point+1;
      }
   }
   else
   printf( "\n");
   return;
}                                       /* End of function offADC             */


void set_tdc_par() {                    /* Subroutine to set TDC parameters   */
  int cmd;

   void uldTDC();
   void lldTDC();
   void offTDC();
   void menuTDC();

   printf("\n");
   printf("  ________________________________________ \r\n");
   printf(" |                                        |\r\n");
   printf(" |          Camac TDC Parameters          |\r\n");
   printf(" |________________________________________|\r\n");
   printf("                                           \r\n");
   printf(" A : Set Upper Level Discriminators (ULD)  \r\n");
   printf(" B : Set Lower Level Discriminators (LLD)  \r\n");
   printf(" C : Set offset values                     \r\n");
   printf(" H : Help, listing of this menu            \r\n");
   printf(" * : Exit from TDC menu                    \r\n");
   printf("                                           \r\n");

   do {
      printf("campari/TDC>");
      cmd = readchar();
      switch (cmd)
      {
	  case 'a' :uldTDC(); break;
	  case 'b' :lldTDC();  break;
	  case 'c' :offTDC();  break;
      case 'h' :menuTDC();  break;
      case '\n':break;
      case '*' :menu();break;
	  default  :printf(" Illegal command, try again\n");
      }
   }while (cmd != '*');
   return;
}


void menuTDC() {
   printf("\n");
   printf(" A : Set Upper Level Discriminators (ULD)  \r\n");
   printf(" B : Set Lower Level Discriminators (LLD)  \r\n");
   printf(" C : Set offset values                     \r\n");
   printf(" H : Help, listing of this menu            \r\n");
   printf(" * : Exit from TDC menu                    \r\n");
   printf("\n");
   return;
}
       
        
void uldTDC() {              /* Set Upper Level Discriminators (ULD) for TDCs */  
   int cmd;       
   int mvolt, uld;
   printf("\n Upper threshold should be about 0 mV");
   printf("\n Common or individual setting for ULD (c/i)?");
   cmd = readchar();
   if( cmd == 'c' ) {
      label:
      printf( " Give common ULD (0-1500 mV):");
      mvolt=readint();
      if( (mvolt > 1500 )|| (mvolt < 0 )) {
         printf(" Value (%d) out of range, try again\n",mvolt);
         goto label;
      }
      uld = 255- (mvolt*0.17);
      for(j = 100; j < 108; j++){
         pardat[j]  = uld;
      }
      for(j = 116; j < 124; j++){
         pardat[j]  = uld;
      }
      for(j = 132; j < 140; j++){
         pardat[j]  = uld;
      }
      for(j = 148; j < 156; j++){
         pardat[j]  = uld;
      }
      initialise('N' );                 /* Put value ULD into TDC registers   */
   } 
   else if( cmd =='i' ) {
                                             
      point = 100;                      /* Set ULD  values for CAMAC TDC1     */
      for(j = 0; j < 8; j++){
         f = 1;                         /* Read  ULD  registers               */
         cssa(f, disc5[j], &para, &q);
         if( q == 0 ) printf("%s",Qresp);
         mvolt = ((255-para)/0.17);
         printf("\n ULD is %5d mV for TDC1 (ch %d)", mvolt,j);
         printf("\n Change (y/n/*)?");
         cmd = readchar();
         if( cmd == '*' ) {
            menuTDC();
            return;
         }
         if(cmd == 'y') {
            ua1:
            printf(" Give ULD (0 - 1500 mV)");
            mvolt=readint();
            if(( mvolt > 1500) || (mvolt < 0 )){
               printf(" Value (%d) out of range, try again\n",mvolt);
               goto ua1;
            }
            para = 255-(mvolt*0.17);
            pardat[point]  = para;
            f = 17;
            cssa(f, disc5[j], &para, &q);
            if( q == 0 ) printf("%s",Qresp);
         }
         point = point+1;
      }
                                        /* Set ULD  values for CAMAC TDC2     */
      point = 116;
      for(j = 0; j < 8; j++){
         f = 1;                         /* Read  ULD  registers               */
         cssa(f, disc6[j], &para, &q);  
         if( q == 0 ) printf("%s",Qresp);
         mvolt = ((255-para)/0.17);
         printf("\n ULD is %5d mV for TDC2 (ch %d)", mvolt,j);
         printf("\n Change (y/n/*)?");
         cmd = readchar();
         if( cmd == '*' ) {
            menuTDC();
            return;
         }
         if(cmd == 'y') {
            ua2:
            printf(" Give ULD (0 - 1500 mV)");
            mvolt=readint();
            if(( mvolt > 1500) || (mvolt < 0 )){
               printf(" Value (%d) out of range, try again\n",mvolt);
               goto ua2;
            }
            para = 255-(mvolt*0.17);
            pardat[point]  = para;
            f = 17;
            cssa(f, disc6[j], &para, &q);
            if( q == 0 ) printf("%s",Qresp);
         }
         point = point+1;
   }
                                        /* Set ULD  values for CAMAC TDC3     */
      point = 132;
      for(j = 0; j < 8; j++){
         f = 1;                         /* Read  ULD  registers               */
         cssa(f, disc7[j], &para, &q);
         if( q == 0 ) printf("%s",Qresp);
         mvolt = ((255-para)/0.17);
         printf("\n ULD is %5d mV for TDC3 (ch %d)", mvolt,j);
         printf("\n Change (y/n/*)?");
         cmd = readchar();
         if( cmd == '*' ) {
            menuTDC();
            return;
         }
         if(cmd == 'y') {
            ua3:
            printf(" Give ULD (0 - 1500 mV)");
            mvolt=readint();
            if(( mvolt > 1500) || (mvolt < 0 )){
               printf(" Value (%d) out of range, try again\n",mvolt);
               goto ua3;
            }
            para = 255-(mvolt*0.17);
            pardat[point]  = para;
            f = 17;
            cssa(f, disc7[j], &para, &q);
            if( q == 0 ) printf("%s",Qresp);
         }
         point = point+1;

      }
                                        /* Set ULD  values for CAMAC TDC4     */
      point = 148;
      for(j = 0; j < 8; j++){
         f = 1;                         /* Read  ULD  registers               */
         cssa(f, disc8[j], &para, &q);
         if( q == 0 ) printf("%s",Qresp);
         mvolt = ((255-para)/0.17);
         printf("\n ULD is %5d mV for TDC4 (ch %d)", mvolt,j);
         printf("\n Change (y/n/*)?");
         cmd = readchar();
         if( cmd == '*' ) {
            menuTDC();
            return;
         }
         if(cmd == 'y') {
            ua4:
            printf(" Give ULD (0 - 1500 mV)");
            mvolt=readint();
            if(( mvolt > 1500) || (mvolt < 0) ){
               printf(" Value (%d) out of range, try again\n",mvolt);
               goto ua4;
            }
            para = 255-(mvolt*0.17);
            pardat[point]  = para;
            f = 17;
            cssa(f, disc8[j], &para, &q);
            if( q == 0 ) printf("%s",Qresp);
         }
         point = point+1;
      }
   }
   else
   printf( "\n");
   return;
}                                       /* End of function uldTDC             */            


void lldTDC() {              /* Set lower level discriminators (LLD) for TDCs */
   int cmd;
   int mvolt, lld;
   printf("\n Lower threshold should be about 20 mV");
   printf("\n Common or individual setting for LLD (c/i)?");
   cmd = readchar();
   if( cmd == 'c' ) {
      label:
       printf(" Give common LLD (0-1000 mV):");
       mvolt=readint();
       if(( mvolt > 1000) || (mvolt < 0 )) {
          printf(" Value (%d) out of range, try again\n",mvolt);
          goto label;
       }
       lld =mvolt*0.255;
       for(j =108; j < 116; j++){
          pardat[j]  = lld;
       }
       for(j = 124; j < 132; j++){
          pardat[j]  = lld;
       }
       for(j = 140; j < 148; j++){
          pardat[j]  = lld;
       }
       for(j = 156; j < 164; j++){
          pardat[j]  = lld;
       }
       initialise('N' );                /* Put value LLD into TDC registers   */
   }
   else if( cmd =='i' ) {
                                           
      point =108;                       /* Set LLD  values for CAMAC TDC1     */
      for(j = 8; j < 16; j++){   
         f = 1;                         /* Read  LLD  registers               */
         cssa(f, disc5[j], &para, &q);
         if( q == 0 ) printf("%s",Qresp);
         mvolt = para/0.255;
         printf("\n LLD is %5d mV for TDC1 (ch %d)", mvolt,j-8);
         printf("\n Change (y/n/*)?");
         cmd = readchar();
         if( cmd == '*' ) {
            menuTDC();
            return;
         }
         if(cmd == 'y') {
            ua1:
            printf(" Give LLD (0 - 1000 mV)");
            mvolt=readint();
            if( (mvolt > 1000) || (mvolt < 0 )){
               printf(" Value (%d) out of range, try again\n",mvolt);
               goto ua1;
            }
            para = mvolt*0.255;
            pardat[point]  = para;
            f = 17;
            cssa(f, disc5[j], &para, &q);
            if( q == 0 ) printf("%s",Qresp);
         }
         point = point+1;
      }
                                        /* Set LLD  values for CAMAC TDC2     */
      point = 124;
      for(j = 8; j < 16; j++){
         f = 1;                         /* Read  LLD  registers               */
         cssa(f, disc6[j], &para, &q);
         if( q == 0 ) printf("%s",Qresp);
         mvolt = para/0.255;
         printf("\n LLD is %5d mV for TDC2 (ch %d)", mvolt,j-8);
         printf("\n Change (y/n/*)?");
         cmd = readchar();
         if( cmd == '*' ) {
            menuTDC();
            return;
         }
         if(cmd == 'y') {
            ua2:
            printf(" Give LLD (0 - 1000 mV)");
            mvolt=readint();
            if(( mvolt > 1000) || (mvolt < 0 )){
               printf(" Value (%d) out of range, try again\n",mvolt);
               goto ua2;
            }
            para = mvolt*0.255;
            pardat[point]  = para;
            f = 17;
            cssa(f, disc6[j], &para, &q);
            if( q == 0 ) printf("%s",Qresp);
         }
         point = point+1;
      }
                                        /* Set LLD  values for CAMAC TDC3     */
      point = 140;
      for(j = 8; j < 16; j++){
         f = 1;                         /* Read  LLD  registers               */
         cssa(f, disc7[j], &para, &q);
         if( q == 0 ) printf("%s",Qresp);
         mvolt = para/0.255;
         printf("\n LLD is %5d mV for TDC3 (ch %d)", mvolt,j-8);
         printf("\n Change (y/n/*)?");
         cmd = readchar();
         if( cmd == '*' ) {
            menuTDC();
            return;
         }
         if(cmd == 'y') {
            ua3:
            printf(" Give LLD (0 - 1000 mV)");
            mvolt=readint();
            if(( mvolt > 1000) || (mvolt < 0 )){
               printf(" Value (%d) out of range, try again\n",mvolt);
               goto ua3;
            }
            para = mvolt*0.255;
            pardat[point]  = para;
            f = 17;
            cssa(f, disc7[j], &para, &q);
            if( q == 0 ) printf("%s",Qresp);
         }
         point = point+1;
      }
                                        /* Set LLD  values for CAMAC TDC4     */
      point = 156;
      for(j = 8; j < 16; j++){
         f = 1;                         /* Read  LLD  registers               */
         cssa(f, disc8[j], &para, &q);
         if( q == 0 ) printf("%s",Qresp);
         mvolt = para/0.255;
         printf("\n LLD is %5d mV for TDC4 (ch %d)", mvolt,j-8);
         printf("\n Change (y/n/*)?");
         cmd = readchar();
         if( cmd == '*' ) {
            menuTDC();
            return;
         }
         if(cmd == 'y') {
            ua4:
            printf(" Give LLD (0 - 1000 mV)");
            mvolt=readint();
            if( (mvolt > 1000) || (mvolt < 0) ){
               printf(" Value (%d) out of range, try again\n",mvolt);
               goto ua4;
            }
            para = mvolt*0.255;
            pardat[point]  = para;
            f = 17;
            cssa(f, disc8[j], &para, &q);
            if( q == 0 ) printf("%s",Qresp);
         }
         point = point+1;
      }
   }       
   else
   printf( "\n");
   return;
}                                       /* End of function lldTDC             */


void offTDC() {                         /* Set offsets for TDCs               */ 
   int cmd;                                      
   int channels, off;
   printf("\n Offset should be about 0 channels");
   printf("\n Common or individual setting for offset (c/i)?");
   cmd = readchar();
   if( cmd == 'c' ) {
      label:
      printf(" Give common offset (-128 ch  -  +127 ch):");
      channels=readint();
      if( (channels > 127)|| (channels < -128)) {
         printf(" Value (%d) out of range, try again\n",channels);
         goto label;
      }
      off = channels + 128;
      for(j = 164; j < 196; j++){
         pardat[j] = off;
      }
      initialise('N');                  /* Put value offset for TDC registers */
   }         
   else if( cmd =='i' ) {
                                           
      point =164;                       /* Set offset  values for CAMAC TDC1  */
      for(j = 0; j < 8; j++){
         f = 4;                         /* Read  offset  registers            */
         cssa(f, off5[j], &para, &q);
         if( q == 0 ) printf("%s",Qresp);
         channels = para -128;
         printf("\n Offset is %4d channels for TDC1 (ch %d)", channels,j);
         printf("\n Change (y/n/*)?");
         cmd = readchar();
         if( cmd == '*' ) {
            menuTDC();
            return;
         }
         if(cmd == 'y') {
            ua1:
            printf(" Give offset TDC1 (-128 ch  -  +127 ch):");
            channels=readint();
            if( (channels > 127)|| (channels < -128)){
               printf(" Value (%d) out of range, try again\n",channels);
               goto ua1;
            }
            para = channels +128;
            pardat[point]  = para;
            f = 20;
            cssa(f, off5[j], &para, &q);
            if( q == 0 ) printf("%s",Qresp);
         }
          point = point+1;
      }

      point =172;
      for(j = 0; j < 8; j++){
         f = 4;                         /* Read  offset  registers            */
         cssa(f, off6[j], &para, &q);
         if( q == 0 ) printf("%s",Qresp);
         channels = para -128;
         printf("\n Offset is %4d channels for TDC2 (ch %d)", channels,j);
         printf("\n Change (y/n/*)?");
         cmd = readchar();
         if( cmd == '*' ) {
            menuTDC();
            return;
         }
         if(cmd == 'y') {
            ua2:
            printf(" Give offset TDC2 (-128 ch  -  +127 ch):");
            channels=readint();
            if( (channels > 127)|| (channels < -128)){
               printf(" Value (%d) out of range, try again\n",channels);
               goto ua2;
            }
            para = channels +128;
            pardat[point]  = para;
            f = 20;
            cssa(f, off6[j], &para, &q);
            if( q == 0 ) printf("%s",Qresp);
         }
         point = point+1;
      }

      point =180;
      for(j = 0; j < 8; j++){
         f = 4;                         /* Read  offset  registers            */
         cssa(f, off7[j], &para, &q);
         if( q == 0 ) printf("%s",Qresp);
         channels = para -128;
         printf("\n Offset is %4d channels for TDC3 (ch %d)", channels,j);
         printf("\n Change (y/n/*)?");
         cmd = readchar();
         if( cmd == '*' ) {
            menuTDC();
            return;
         }
         if(cmd == 'y') {
            ua3:
            printf(" Give offset TDC3 (-128 ch  -  +127 ch):");
            channels=readint();
            if(( channels > 127)|| (channels < -128)){
               printf(" Value (%d) out of range, try again\n",channels);
               goto ua3;
            }
            para = channels +128;
            pardat[point]  = para;
            f = 20;
            cssa(f, off7[j], &para, &q);
            if( q == 0 ) printf("%s",Qresp);
         }
         point = point+1;
      }

      point =188;
      for(j = 0; j < 8; j++){
         f = 4;                         /* Read  offset  registers            */
         cssa(f, off8[j], &para, &q);
         if( q == 0 ) printf("%s",Qresp);
         channels = para -128;
         printf("\n Offset is %4d channels for TDC4 (ch %d)", channels,j);
         printf("\n Change (y/n/*)?");
         cmd = readchar();
         if( cmd == '*' ) {
            menuTDC();
            return;
         }
         if(cmd == 'y') {
            ua4:
            printf(" Give offset TDC4 (-128 ch  -  +127 ch):");
            channels=readint();
            if(( channels > 127)|| (channels < -128)){
               printf(" Value (%d) out of range, try again\n",channels);
               goto ua4;
            }
            para = channels +128;
            pardat[point]  = para;
            f = 20;
            cssa(f, off8[j], &para, &q);
            if( q == 0 ) printf("%s",Qresp);
         }
         point = point+1;
      }
   }
    
   else
   printf( "\n");
   return;
}                                       /* End of function offTDC             */


int readchar() {
	int c1;
	int c2 = '\n';
	while((c1 = getchar()) != '\n')
	   c2 = tolower(c1);
	return c2;
}


int readint() {
	int integer, c1;
	scanf("%d",&integer);
/*	if(integer == '\n')
	   integer = 10000;*/
	while((c1 = getchar()) != '\n')
	   continue;
	return integer;
}
