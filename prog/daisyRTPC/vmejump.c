/* Writing 1 into the messagebox of eventbuilder  */
/* The message box location is  0xff010010, which */
/* corresponds to Message_box[4]                  */
/* eventbuilder tests this location for every     */
/* loop, and returns then to prompt.              */
/* E-mail  : magne.guttormsen@fys.uio.no          */
/* Created : 17-09-1998                           */
/* Modified:                                      */

#include <stdio.h>
#include <smem.h>
#include <ces/vmelib.h>
#define CPUADDR        0xff010010          /*Phys. addr. for SRAM -> A24 slave*/
                                           /*whith hex switch = 5 on CPU card */
#define swap(x) ((x&0xff000000)>>24)+((x&0x00ff0000)>>8)+((x&0x0000ff00)<<8)+((x&0x000000ff)<<24)

u_long  dts, *pVMEjump;
long mem24, *p, one;

int main() {
   one = 1;

   if (!(mem24=(int)smem_create("MEM24",(char *)(CPUADDR&~0xFFF),0x4,
      SM_WRITE | SM_READ))){
      fprintf(stderr,"Unable to allocate MEM window for slave A24 memory");
      exit(0);
   }

   p = (long *)(mem24 | (CPUADDR&0xFFF));          /*Location for VMEjump*/

   dts=swap(*p);
   printf("Message box(4): VMEjump status was    : 0x%x \n",dts);
   *p = swap(1);
   dts=swap(*p);
   printf("Message box(4): VMEjump status is now : 0x%x \n",dts);
   printf("VME eventbuilder is leaving infinite eventloop...\n");

   smem_create("MEM24",(char *)mem24,0x4,SM_DETACH);
   smem_remove("MEM24"); 
  
   return;
}
