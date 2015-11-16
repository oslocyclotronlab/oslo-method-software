#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>    
#include <math.h>    
#include <fcntl.h>						/* Header for open/read/write */ 
#include <errno.h> 
#include <sys/types.h>
#include <sys/ioctl.h>          

FILE	*fp1, *fp2;
char	line[1024];
int		i, ch, counts, cnts[8192];

static void fgets_ignore(char *s, int size, FILE *stream)
{
    // suppress braindead and presumtuous glibc 'security' warning
    if( !fgets(s, size, stream) )
        return;
}

int main()
{
	printf("\n");
	printf("  ______________________________________________________________ \r\n");
	printf(" |                                                              |\r\n");
	printf(" |                            Singles                           |\r\n");
	printf(" |                                                              |\r\n");
	printf(" |  Reads energy-counts table of 8001 ch (MSU) and transfer to  |\r\n");
	printf(" |                        array of counts                       |\r\n");
	printf(" |                                                              |\r\n");
	printf(" | E-mail  : magne.guttormsen@fys.uio.no                        |\r\n");
	printf(" | Created : 27 Jun 2014                                        |\r\n");
	printf(" |______________________________________________________________|\r\n");
	printf("                                                                 \r\n");
	/* **************** */
	/* Reading spectrum */
	/* **************** */
	printf("Reading spectrum\n");
	fp1 = fopen("seg3_6000.asc", "r");
	if(fp1 == NULL){
		printf("No spectrum in your directory\n");
		exit(0);
	}
	else {
		for(i=0;i<8001;i++){
            fgets_ignore(line,sizeof(line),fp1);
            sscanf(line," %d %d", &ch, &counts);
            cnts[i] = counts;
        }
        fclose(fp1);
    }
	/* *************** */
	/* Writing spectrum*/
	/* *************** */
    fp2 = fopen("sc6000", "w");
    for(i=0;i<8001;i++){
        fprintf(fp2, " %d \n", cnts[i]);
    }
    fclose(fp2);
	return(0);
}

