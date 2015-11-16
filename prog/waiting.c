#include <stdio.h>
#include <math.h>
#include <time.h>



#define WAIT100 \
  {volatile int ii; for(ii=100;ii;ii--);}
  
#define WAIT1000 \
  {volatile int ii; for(ii=1000;ii;ii--);}
  
#define WAIT10000 \
  {volatile int ii; for(ii=10000;ii;ii--);}
  
#define WAIT0

int main()
{
  int i;
  float usec, usec_wait;
  clock_t starttime;
  
  starttime=clock();
  for (i=1;i<=10000;i++)
  {
    WAIT100
  }
  usec=1000000.*(clock()-starttime)/CLOCKS_PER_SEC;
  usec_wait=usec/1000000.;
  printf("\n WAIT with 100 loops");
  printf("\n time used      = %f usec",usec);
  printf("\n one WAIT takes = %f usec",usec_wait); 
  printf("\n ");

  starttime=clock();
  for (i=1;i<=10000;i++)
  {
    WAIT1000
  }
  usec=1000000.*(clock()-starttime)/CLOCKS_PER_SEC;
  usec_wait=usec/10000000.;
  printf("\n WAIT with 1000 loops");
  printf("\n time used      = %f usec",usec);
  printf("\n one WAIT takes = %f usec",usec_wait); 
  printf("\n ");
  
  starttime=clock();
  for (i=1;i<=10000;i++)
  {
    WAIT10000
  }
  usec=1000000.*(clock()-starttime)/CLOCKS_PER_SEC;
  usec_wait=usec/100000000.;
  printf("\n WAIT with 10000 loops");
  printf("\n time used      = %f usec",usec);
  printf("\n one WAIT takes = %f usec",usec_wait); 
  printf("\n ");
  
  return 0;
}
