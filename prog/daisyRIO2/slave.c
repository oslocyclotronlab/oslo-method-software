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

   printf("\n");
   printf(" ____________________________________________________________ \r\n");
   printf("|                                                            |\r\n");
   printf("|                          Slave 1.0                         |\r\n");
   printf("|                                                            |\r\n");
   printf("|          Writing to PCI_CTL2 and PCI_CTL3 registers        |\r\n");
   printf("|             The register contents are described            |\r\n");
   printf("|             in the RTPC 8067LK manual, page 75             |\r\n");
   printf("|           (Based on the spoke functions for SRAM)          |\r\n");
   printf("|                                                            |\r\n");
   printf("| E-mail  : magne.guttormsen@fys.uio.no                      |\r\n");
   printf("| Created : 04-06-1998                                       |\r\n");
   printf("| Modified:                                                  |\r\n");
   printf("|____________________________________________________________|\r\n");
   printf("                                                              \r\n");
   printf("This program should be run after hardware reset in order      \r\n");
   printf("to define address pointer and initialise SRAM. Test also      \r\n");
   printf("with peekreg, that VME Slave address is enabled correctly     \r\n");
   printf("                                                              \r\n");

/*******************************************************/

   ad = PCI_CTL2;
   dt = 0x01400000;
   if (!(mem=(int)smem_create("MEM",(char *)(ad&~0xFFF), 0x1000,SM_WRITE)))
   {
      fprintf(stderr,"unable to allocate MEM window\n");
      exit(0);
   }
   p = (long *)(mem | (ad&0xFFF));
   *p = dt;
   printf("PCI_CTL2 at phys AD=%x, log AD=0x%08x: set to: %8x\n",ad,p,dt);
   smem_create("MEM",(char *)mem,0x1000,SM_DETACH);
   smem_remove("MEM");
 /*******************************************************/

   ad = PCI_CTL3;
   dt = 0x5f000000;
   if (!(mem=(int)smem_create("MEM",(char *)(ad&~0xFFF), 0x1000,SM_WRITE)))
   {
      fprintf(stderr,"unable to allocate MEM window\n");
      exit(0);
   }
   p = (long *)(mem | (ad&0xFFF));
   *p = dt;
   printf("PCI_CTL3 at phys AD=%x, log AD=0x%08x: set to: %8x\n",ad,p,dt);
   smem_create("MEM",(char *)mem,0x1000,SM_DETACH);
   smem_remove("MEM");
 /*******************************************************/

   ad = PCI_CTL2;
   dt = 0x00400000;
   if (!(mem=(int)smem_create("MEM",(char *)(ad&~0xFFF), 0x1000,SM_WRITE)))
   {
      fprintf(stderr,"unable to allocate MEM window\n");
      exit(0);
   }
   p = (long *)(mem | (ad&0xFFF));
   *p = dt;
   printf("PCI_CTL2 at phys AD=%x, log AD=0x%08x: set to: %8x\n",ad,p,dt);
   smem_create("MEM",(char *)mem,0x1000,SM_DETACH);
   smem_remove("MEM");
 /*******************************************************/

   ad = PCI_CTL3;
   dt = 0x00800000;
   if (!(mem=(int)smem_create("MEM",(char *)(ad&~0xFFF), 0x1000,SM_WRITE)))
   {
      fprintf(stderr,"unable to allocate MEM window\n");
      exit(0);
   }
   p = (long *)(mem | (ad&0xFFF));
   *p = dt;
   printf("PCI_CTL3 at phys AD=%x, log AD=0x%08x: set to: %8x\n",ad,p,dt);
   smem_create("MEM",(char *)mem,0x1000,SM_DETACH);
   smem_remove("MEM");
/*******************************************************/

   return 0;
}

