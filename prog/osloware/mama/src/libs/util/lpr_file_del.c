#include <string.h>
#define MAXMESS 64
#define MAXFILE 50 /* MAXMESS - 14 */

int lpr_file_del_(filename)
     char filename[MAXFILE];
{
  int stat, i;
  char message[MAXMESS];

  for (i=1; i<MAXMESS+1; i++)
    message[i] = '\0';

  strncpy(message,"lp -r ./",8);
  strcat(message,filename);
  stat = system(message);
  return ( stat );
}
