#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>

int cleansymbols(FILE *fp1, FILE *fp2)
{
	int i;
	char line[1024];
	while(fgets(line,1024,fp1) != NULL){;
		for(i = 0; line[i] != '\0'; i++){
			if(line[i] == ','||line[i] == ':'||line[i] ==';'||line[i] == '!'||line[i] == '/')line[i] = ' ';
		}
		fputs(line, fp2);
	}
	return 0;
}
