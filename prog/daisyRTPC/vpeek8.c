/*********************************************************
* file: vpeek8.c
* 950516 MAWE 
* 950914 MAWE new 'vlib'
* 951110 MAWE last update
* 980205 Magne Guttormsen, for unchar
* read 8 bit from given VME address (based on 'vlib')
*********************************************************/
#include   <stdio.h>
#include   <strings.h>
#define   AD   (0xf0ffff00)
#define   AM   (0x39)

main(argc,argv)
int argc;
char *argv[];
{
   unsigned char dt,*p;
   long i,ad,am,ct;

   if ((argc==4)&&(sscanf(argv[3],"%x",&ct)==1)) {argc--;} else {ct=1;}
   if ((argc==3)&&(sscanf(argv[2],"%x",&am)==1)) {argc--;} else {am=AM;}
   if ((argc==2)&&(sscanf(argv[1],"%x",&ad)==1)) {argc--;} else {ad=AD;}
   if (argc != 1) {
      printf("use: %s [<AD>[<AM>[<cnt>]]]\n",argv[0]);
      printf("     read VME address <AD> using address modifier code<AM>\n");
      printf("     <cnt> times, display result\n");
      exit(0);
   }

   p = (unsigned char *)vme_map(ad,sizeof(unsigned char),am);
   if (p == (unsigned char *)0){
      fprintf(stderr,"unable to map VME address\n");
      exit(0);
   }
   fprintf(stderr,"The pointer is : 0x%x \n",p);
   for(i=ct;i;i--) {dt = *p;}
   printf("AD=0x%08x, AM=0x%02x: data read: %8x\n",ad,am,dt);
   vme_rel(p, sizeof(unsigned char));
   return 0;
}
