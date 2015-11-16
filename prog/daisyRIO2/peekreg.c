#include <stdio.h>
#include <strings.h>
#include <smem.h>

#define PCI_CTL0 0xa0f50000
#define PCI_CTL1 0xa0f50004
#define PCI_CTL2 0xa0f50008
#define PCI_CTL3 0xa0f5000c
#define VME_CTL0 0xa0f50040
#define VME_CTL1 0xa0f50044
#define VME_CTL2 0xa0f50048
#define VME_CTL3 0xa0f5004c

int main()
{

   long ad, dt, mem, *p;
   printf(" ____________________________________________________________ \r\n");
   printf("|                                                            |\r\n");
   printf("|                       Peekreg 1.0                          |\r\n");
   printf("|                                                            |\r\n");
   printf("|            Reading registers PCI_CTL and VME_CTL           |\r\n");
   printf("|             The register contents are described            |\r\n");
   printf("|           in the RTPC 8067LK manual, page 73 - 77          |\r\n");
   printf("|            (based on the speek function for SRAM)          |\r\n");
   printf("|                                                            |\r\n");
   printf("| E-mail  : magne.guttormsen@fys.uio.no                      |\r\n");
   printf("| Created : 04-06-1998                                       |\r\n");
   printf("| Modified: 16-09-1998                                       |\r\n");
   printf("|____________________________________________________________|\r\n");
   printf("                                                              \r\n");
   printf("The expression <should be> is not neccessary correct. Only      \r\n");
   printf("some bits are significant. Please, consult the RTPC manual.     \r\n");
   printf("Assume that A24 slave memory is not enabled:                    \r\n");
   printf("Then bit 4 of VME_CTL1 is 0, and you read:90... instead of 98...\r\n");
   printf("Use the spoke command: spoke a0f50044 98800000                  \r\n");
   printf("Run peekreg again to verify your changes                       \r\n");
   printf("                                                                \r\n");

/*******************************************************/
   ad = PCI_CTL0;
   if (!(mem=(int)smem_create("MEM",(char *)(ad&~0xFFF),0x1000,SM_READ))){
      fprintf(stderr,"unable to allocate MEM window\n");
      exit(0);
   }
   p = (long *)(mem | (ad&0xFFF));
   dt = *p;
   printf("PCI_CTL0: phy AD=%x, log AD=0x%08x, should be:01... read: %8x \n",ad,p,dt);
   smem_create("MEM",(char *)mem,0x1000,SM_DETACH);
   smem_remove("MEM");

/*******************************************************/
   ad = PCI_CTL1;
   if (!(mem=(int)smem_create("MEM",(char *)(ad&~0xFFF),0x1000,SM_READ))){
      fprintf(stderr,"unable to allocate MEM window\n");
      exit(0);
   }
   p = (long *)(mem | (ad&0xFFF));
   dt = *p;
   printf("PCI_CTL1: phy AD=%x, log AD=0x%08x, should be:f8... read: %8x \n",ad,p,dt);
   smem_create("MEM",(char *)mem,0x1000,SM_DETACH);
   smem_remove("MEM");

/*******************************************************/
   ad = PCI_CTL2;
   if (!(mem=(int)smem_create("MEM",(char *)(ad&~0xFFF),0x1000,SM_READ))){
      fprintf(stderr,"unable to allocate MEM window\n");
      exit(0);
   }
   p = (long *)(mem | (ad&0xFFF));
   dt = *p;
   printf("PCI_CTL2: phy AD=%x, log AD=0x%08x, should be:004.. read: %8x \n",ad,p,dt);
   smem_create("MEM",(char *)mem,0x1000,SM_DETACH);
   smem_remove("MEM");

/*******************************************************/
   ad = PCI_CTL3;
   if (!(mem=(int)smem_create("MEM",(char *)(ad&~0xFFF),0x1000,SM_READ))){
      fprintf(stderr,"unable to allocate MEM window\n");
      exit(0);
   }
   p = (long *)(mem | (ad&0xFFF));
   dt = *p;
   printf("PCI_CTL3: phy AD=%x, log AD=0x%08x, should be:008.. read: %8x \n",ad,p,dt);
   smem_create("MEM",(char *)mem,0x1000,SM_DETACH);
   smem_remove("MEM");

/*******************************************************/
   ad = VME_CTL0;
   if (!(mem=(int)smem_create("MEM",(char *)(ad&~0xFFF),0x1000,SM_READ))){
      fprintf(stderr,"unable to allocate MEM window\n");
      exit(0);
   }
   p = (long *)(mem | (ad&0xFFF));
   dt = *p;
   printf("VME_CTL0: phy AD=%x, log AD=0x%08x, should be:08... read: %8x \n",ad,p,dt);
   smem_create("MEM",(char *)mem,0x1000,SM_DETACH);
   smem_remove("MEM");

/*******************************************************/
   ad = VME_CTL1;
   if (!(mem=(int)smem_create("MEM",(char *)(ad&~0xFFF),0x1000,SM_READ))){
      fprintf(stderr,"unable to allocate MEM window\n");
      exit(0);
   }
   p = (long *)(mem | (ad&0xFFF));
   dt = *p;
   printf("VME_CTL1: phy AD=%x, log AD=0x%08x, should be:98... read: %8x \n",ad,p,dt);
   smem_create("MEM",(char *)mem,0x1000,SM_DETACH);
   smem_remove("MEM");

/*******************************************************/
   ad = VME_CTL2;
   if (!(mem=(int)smem_create("MEM",(char *)(ad&~0xFFF),0x1000,SM_READ))){
      fprintf(stderr,"unable to allocate MEM window\n");
      exit(0);
   }
   p = (long *)(mem | (ad&0xFFF));
   dt = *p;
   printf("VME_CTL2: phy AD=%x, log AD=0x%08x, should be:85... read: %8x \n",ad,p,dt);
   smem_create("MEM",(char *)mem,0x1000,SM_DETACH);
   smem_remove("MEM");

/*******************************************************/
   ad = VME_CTL3;
   if (!(mem=(int)smem_create("MEM",(char *)(ad&~0xFFF),0x1000,SM_READ))){
      fprintf(stderr,"unable to allocate MEM window\n");
      exit(0);
   }
   p = (long *)(mem | (ad&0xFFF));
   dt = *p;
   printf("VME_CTL3: phy AD=%x, log AD=0x%08x, should be:08... read: %8x \n",ad,p,dt);
   smem_create("MEM",(char *)mem,0x1000,SM_DETACH);
   smem_remove("MEM");

/*******************************************************/


return 0;
}


