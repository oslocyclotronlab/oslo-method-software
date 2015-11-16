#include <stdio.h>
#include <math.h>
#include <time.h>



#define WAIT100 \
  {volatile long ii; for(ii=100;ii;ii--);}
  
#define WAIT1000 \
  { volatile long ii; for(ii=1000;ii;ii--);}
  
#define WAIT10000 \
  { volatile long ii; for(ii=10000;ii;ii--);}
  
#define WAIT0

main()
{
  long i;
  float sec, sec_wait;
  clock_t starttime;
  
  starttime=clock();
  for (i=1;i<=10000;i++)
  {
    WAIT100
  }
  sec=(clock()-starttime)/CLOCKS_PER_SEC;
  sec_wait=sec/1000000.;
  printf("\n WAIT with 100 loops");
  printf("\n time used      = %f sec",sec);
  printf("\n one WAIT takes = %f sec",sec_wait); 
  printf("\n ");

  starttime=clock();
  for (i=1;i<=10000;i++)
  {
    WAIT1000
  }
  sec=(clock()-starttime)/CLOCKS_PER_SEC;
  sec_wait=sec/10000000.;
  printf("\n WAIT with 1000 loops");
  printf("\n time used      = %f sec",sec);
  printf("\n one WAIT takes = %f sec",sec_wait); 
  printf("\n ");
  
  starttime=clock();
  for (i=1;i<=10000;i++)
  {
    WAIT10000
  }
  sec=(clock()-starttime)/CLOCKS_PER_SEC;
  sec_wait=sec/100000000.;
  printf("\n WAIT with 10000 loops");
  printf("\n time used      = %f sec",sec);
  printf("\n one WAIT takes = %f sec",sec_wait); 
  printf("\n ");
  
  return 0;
}
