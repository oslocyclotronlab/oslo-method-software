
#include  <stdio.h>
#include  <time.h>

int main(){
   char sSTART[80];
   time_t now;
   struct tm *date;
   time( &now );
   date = localtime( &now );
   strftime( sSTART, 80, "%c", date );
   printf( "\nVME data acquisition started at %s\n", sSTART );
   printf( "To jump out of event-loop, press Ctrl_C\n");
   return;
}
