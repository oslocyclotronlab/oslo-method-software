#include       <time.h>

int main(){
char sSTOP[80];
time_t now;
struct tm *date;
time( &now );
         date = localtime( &now );
         strftime( sSTOP, 80, "%c", date );
         printf("\n%s\n", sSTOP );
return;
}