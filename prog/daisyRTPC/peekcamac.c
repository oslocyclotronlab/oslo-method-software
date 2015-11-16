#include   <stdio.h>
#include   <strings.h>

main(){
   unsigned short dt,*p1,*p2;
   long i,j,ad1,ad2,am,ct;
   am  = 0x39;
   ad1 = 0x892002;
   ad2 = 0x894002;
   ct  = 200000;

   printf(" ____________________________________________________________ \r\n");
   printf("|                                                            |\r\n");
   printf("|                       Peekcamac 1.0                        |\r\n");
   printf("|                                                            |\r\n");
   printf("|    Testing 200000 times the CAMAC-addresses 0x892002 and   |\r\n");
   printf("|      0x894002, and looping 20 times. The program is        |\r\n");
   printf("|     based on the vme_map function described and used       |\r\n");
   printf("|       in the C-programs vpeek and vpoke found in the       |\r\n");
   printf("|         directory /usr/ces/examples/vlib of bobcat         |\r\n");
   printf("| E-mail  : magne.guttormsen@fys.uio.no                      |\r\n");
   printf("| Created : 15-02-1998                                       |\r\n");
   printf("| Modified: 16-09-1998                                       |\r\n");
   printf("|____________________________________________________________|\r\n");
   printf("                                                              \r\n");

   p1 = (unsigned short *)vme_map(ad1,2,am);
   if (p1 == (unsigned short *)0){
      fprintf(stderr,"unable to map VME address\n");
      exit(0);
   }
   p2 = (unsigned short *)vme_map(ad2,2,am);
   if (p2 == (unsigned short *)0){
      fprintf(stderr,"unable to map VME address\n");
      exit(0);
   }
   for(j = 0; j < 10; j++){
      for(i=ct;i;i--) {dt = *p1;}
      printf("AD=0x%08x, AM=0x%02x: data read: %8x\n",ad1,am,dt);
      for(i=ct;i;i--) {dt = *p2;}
      printf("AD=0x%08x, AM=0x%02x: data read: %8x\n",ad2,am,dt);
   }
   vme_rel(p1,2);
   vme_rel(p2,2);
   return 0;
}
