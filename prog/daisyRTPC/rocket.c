#include <stdio.h>
#include <math.h>
#include <time.h>

/* Program which estimates (1+x)^n with the serie 1+nx/1!+n(n-1)x*x/2!+...*/
/* In this case we have 1+x=sqrt(2) and n=4, which should give exactly 4.         */


int main()
{
  long int i;
  long double sum,kernel,x,n;
  clock_t start,secs;
  start=clock();
  x=sqrt(2.)-1.;n=4.;sum=1.;kernel=1.;
  for (i=1;i<=1000000;i++)
  {
    kernel=kernel*(n+1.-(long double)i)*x/(long double)i;
    sum=sum+kernel;
  }
  secs=1000.*(clock()-start)/CLOCKS_PER_SEC;
  printf("\nResult=%17.15Lf (should be 4.0), time used = %lu msecs.\n",sum,secs);
  return 0;
}
