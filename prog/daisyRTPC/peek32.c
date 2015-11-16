/*
****************************************************************************
* file: peek.c
* 920824 MAWE
* 940527 MAWE adapted to Lynx
* 940819 MAWE adapted to RAID 8235
* 950314 MAWE adapted to RTPC 8067
* 951109 MAWE version without 'smem...'
* 951109 MAWE last update
*
* read long word from given address
****************************************************************************
*/
#include	<stdio.h>
#include	<strings.h>
#include	<smem.h>

#define		AD	(0xc0000000)
/*
*===========================================================================
* do it
*---------------------------------------------------------------------------
*/
main(argc,argv)
int argc;
char *argv[];
{
	long i,ad,ct,dt,*p;
	
	if ((argc==3)&&(sscanf(argv[2],"%x",&ct)==1)) {argc--;} else {ct=1;}
	if ((argc==2)&&(sscanf(argv[1],"%x",&ad)==1)) {argc--;} else {ad=AD;}
	if (argc != 1) {
		printf("use: %s [<AD>[<cnt>]]\n",argv[0]);
		printf("     read address <AD> <cnt> times, display result\n");
		exit(0);
	}
	printf("AD=%x: data read: %8x\n",ad,dt);

	p = (long *)ad;
	printf("AD=%x: data read: %8x\n",ad,dt);

	/*for(i=ct;i;i--) {dt = *p;}*/
           dt=*p;
	printf("AD=%x: data read: %8x\n",ad,dt);
}
