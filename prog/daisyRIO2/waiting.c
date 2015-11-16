#include <stdio.h>
#include <math.h>
#include <time.h>

#define WAIT(x) {volatile long ii;for(ii=x;ii;ii--);}  /* 1 loop = 0.1647us on RTPC */
long loopTOT;

char inbuf[130];

main()
{
  long loopTOT;
  float sec, usec;
  clock_t starttime;
  

   printf("Type number of loops:\n");
   loopTOT = 1000000000;
   printf("Loops              (%d):", loopTOT); 
   gets(inbuf);
   sscanf(inbuf,"%d",&loopTOT);
  


  starttime=clock();
  
    WAIT(loopTOT);
  
  sec=(clock()-starttime)/CLOCKS_PER_SEC;
  usec=sec*1000000.;
  printf("\n time used      = %f sec",sec);
  usec = usec/loopTOT;
  printf("\n One loop takes = %f usec",usec); 
  printf("\n ");

    
  return 0;
}
