/******************************************************************************
* file: <slottest.c>            created: 10-December-1991       L. Tremblet   *
*                                                                             *
* CAMAC slot test program                                                     *
* -----------------------                                                     *
* -- This program checks the dataway lines at a given CAMAC                   *
* -- station using the BORER display module (1801,1802)                       *
*                                                                             *
* Revision history:                                                           *
* ----------------                                                            *
*                                                                             *
* M. Gentile   30/01/92    Added User Interface (Menulib)                     *
* L. Tremblet  03/03/92    Added WANT_MENU compile time option                *
* MAWE         05/12/92    stripped down version, remove I after Z            *
* MAWE         08/09/92    removed include "camac.h" (was never used)         *
* MAWE         28/10/93    replace 'linktrap()' by 'copen()', add 'cclose()'  *
*                                                                             *
/*****************************************************************************/


#include <stdio.h>
/*
* #include "camac.h"
*/

#define ENABLE  -1
#define DISABLE  0

#define DEFAULT_BRANCH  1
#define DEFAULT_CRATE   1
#define DEFAULT_STATION 8
#define DEFAULT_MAXLOOP 1000

#define BOOL(x) (!(!(x)))
#define bit(a,x)        BOOL((a) & (1L << (x)))
#define shift(a,x)      ((x>=0) ? ((a) << (x)) : ((a) >> (x)))
#define subword(a,p,x)  ((unsigned)(((unsigned) a) << (32-p-x)) >> (32-x))

#define PRINSTEP 100

#define test1 " TEST 1/ INITIALIZE (Z). TEST IF=0 "
#define test2 " TEST 2/ SET ALL WRITE BITS = 1 AND CHECK "
#define test3 " TEST 3/ F9 , TEST IF ALL BITS CLEARED "
#define test4 " TEST 4/ BIT CROSS TALK "
#define test6 " TEST 6/ SET LAM AND TEST "
#define test7 " TEST 7/ CLEAR LAM AND TEST "
#define test8 " TEST 8/ SET I AND TEST "
#define test9 " TEST 9/ CLEAR I AND TEST "
#define test10 " TEST 10/ TEST BITS 1-24, ONE AT A TIME "

#define test_naf " TEST NAF/ "

int reg[16], idmy[2];
int xread, yread, xwrit;
int cycle, chan, unit;
int reg1, lam, dum;
int br, bw, b, c, n, q;
int i, j, m;
int once = 0;
int loop, max_loop;



int wait_key()
{
  int c;

  printf("Press <return>");
  c = getchar();
  return 1;
}

void checknaf(a2, f2)
 int a2, f2;
  {
    /* -- a2,f2 = most recent A,F */
   int a3, f3, q;
   cfsa(1, reg[a2], &yread, &q);
   if(q==0)
     {
      printf(test_naf); printf( "Q=0 for F=1 A=%4d\n", a2);
     }
    /* -- get A pattern */
   a3 = subword(yread, 0, 4);
   if(a2!=a3)
     {
      printf(test_naf); printf( "A=%4d not%4d\n", a3, a2);
     }
    /* -- get F pattern */
   f3 = subword(yread, 4, 5);
   if(f2!=f3)
     {
      printf(test_naf); printf( "F=%4d not%4d\n", f3, f2);
     }
    /* -- check N line */
   if(bit(yread,9)==0)
     {
      printf(test_naf); printf( "wrong N\n");
     }
  } /* ENDSUB checknaf */


void test_camac()
{
   printf("\n***** slot test started *****\n");

   for(i=0;i<=15;i++) {
      cdreg(&reg[i], b, c, n, i);
   }
   cdreg(&reg1, b, c, n, 0);
   idmy[0] = 1;
   cdlam(&lam, b, c, n, 0, idmy);

   cycle = 0;

   if (max_loop == 0) max_loop = 0x7fffffff;
   for (loop = 0; loop < max_loop; loop++) {

     /* -- check Z */
       cccz(reg1);
       ccci(reg1,DISABLE);
       for(i=0;i<=15;i++) {
          cfsa(0, reg[i], &xread, &q);
          checknaf(i, 0);
          if(q==0) {
 	    printf(test1); printf(" Q=0, F=0, A=%4d\n", i);
          }
          if(xread!=0) {
             for(j=0;j<=23;j++) {
                if(bit(xread,j)!=0) {
 	          printf(test1); printf("BIT%4d not 0 AFTER Z\n", j+1);
 	       }
              }
          }
       }
       xwrit = 0xFFFFFF;
       cfsa(16, reg1, &xwrit, &q);
        /*  A,F arrived correctly ? */
       checknaf(0, 16);
       if(q==0) {
          printf(test2); printf("Q=0,F=16,A=0\n");
       }
       cfsa(0, reg1, &xread, &q);
       if(xread!=xwrit) {
          for(j=0;j<=23;j++) {
             if(bit(xread,j)==0) {
 	       printf(" TEST 2, SET ALL WRITE BITS = 1 AND CHECK\n");
                if(bit(xread,j)==0) {
 	          printf(test2); printf(" bit %4d not 1\n", j+1);
                }
             }
          }
       }

       cfsa(9, reg1, &xwrit, &q);
       checknaf(0, 9);
       cfsa(0, reg1, &xread, &q);
       if(xread!=0) {
          for(j=0;j<=23;j++) {
             if(bit(xread,j)!=0) {
      	       printf(test3); printf("F9 does not clear bit%4d\n", j+1);
             }
          }
       }
       for(i=1;i<=2;i++) {
           /*  alternate pattern */
          if(i==1) {
              xwrit = 0x555555;
          }
          if(i==2) {
              xwrit = 0xAAAAAA;
          }
          cfsa(16, reg1, &xwrit, &q);
          cfsa(0, reg1, &xread, &q);
          if(xread!=xwrit) {
             for(j=0;j<=23;j++) {
                bw = bit(xwrit, j); br = bit(xread, j);
                if(bw!=br) {
 	          printf(test4); printf("bit%4d =%4d not%4d\n", j+1, bw, br);
                }
             }
          }
       }
       cfsa(9, reg1, &dum, &q);
        /*  enable and set LAM */
       cclm(lam, ENABLE);
       cfsa(25, reg1, &dum, &q);
       checknaf(0, 25);
       ctlm(lam, &q);
       checknaf(0, 8);
       if(q==0) {
          printf(test6); printf("LAM not set\n");
       }
       cclc(lam);
       checknaf(0, 10);
       ctlm(lam, &q);
       if(q!=0) {
          printf(test7); printf("LAM not cleared\n");
       }
        /* -- check inhibit */
       ccci(reg1, ENABLE);
       cfsa(1, reg1, &xread, &q);
       if(bit(xread,12)==0) {
          printf(test8); printf("I OFF not ON\n");
       }
       ccci(reg1, DISABLE);
       cfsa(1, reg1, &xread, &q);
       if(bit(xread,12)!=0) {
          printf(test8); printf("I ON not OFF\n");
       }
        /*  check R,W lines , one bit at a time */
       for(j=0;j<=23;j++) {
          xwrit = shift(1, j);
          cfsa(16, reg1, &xwrit, &q); cfsa(0, reg1, &xread, &q);
          if(xread!=xwrit) {
             for(m=0;m<=23;m++) {
                br = bit(xread, m); bw = bit(xwrit, m);
                if(br!=bw) {
 	       printf(test10); printf("FAIL for%4d bit%4d =%4d", j+1, m+1, br);
         	       }
	     }
          }
          cfsa(9, reg1, &dum, &q);
       }
       cycle++;
       if(cycle%PRINSTEP ==0 ) {
          printf("CYCLE %d\n", cycle);
       }
  }
  /*
  wait_key();
  */
}


int quit_action()
   {
    return 0;       /* Quit return */
   }


main()
{
   printf(" B C N = ");
   scanf("%d %d %d", &b, &c, &n);
   max_loop = 2000;
   copen();
   test_camac();
   cclose();
}
