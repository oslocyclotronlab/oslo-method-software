#include	<string.h>
#include	<stdio.h>
#include	<ctype.h>
#include	<fcntl.h>
#include	<stdlib.h>

int number2string(int n,char *s1) {
	char *numbers[] = {"0","1","2","3","4","5","6","7","8","9"};
	int i1=0;
	int i2=0;
	int i3=0;
	char *c1;
	char *c2;
	char *c3;
	i1=n/100;
	i2=(n-i1*100)/10;
	i3=(n-i1*100-i2*10);
	*s1=0;
	if(i1 != 0){
		c1=numbers[i1];
    	c2=numbers[i2];
    	c3=numbers[i3];
    	strcat(s1,c1);
		strcat(s1,c2);
		strcat(s1,c3);
	}
	else if(i2 != 0){
		c2=numbers[i2];
    	c3=numbers[i3];
		strcat(s1,c2);
		strcat(s1,c3);
	}
	else {
		c3=numbers[i3];
		strcat(s1,c3);
	}
	return 0;
}
