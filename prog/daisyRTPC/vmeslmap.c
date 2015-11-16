/*Nytt test program*/

#include <errno.h>
#include <file.h>
#include <types.h>
#include	<ces/uiocmd.h>
#include <ces/vmelib.h>

#define		EXIT_SUCCESS	0
#define		EXIT_FAILURE	1

void main()
{
   u_long pciaddr, size, vmeaddr, status;
   struct pdparam_slave param;
   long dt,*usraddr;

      pciaddr=0x80000000; 
      param.rdpref = 0;
      param.wrprotect = 0;
      param.wrpost = 0;
      param.swap = SINGLE_AUTO_SWAP;
      param.pcispace = PCI_MEM_CES;
      vmeaddr = vme_slave_map(pciaddr, 0x40000,&param);
      if(vmeaddr == -1)
      {
         printf("VME_SLAVE_MAP: can not assigned pci address = %x \n",pciaddr);
      }
      else
      {
         printf("vmeaddr = %x \n",vmeaddr);
      }

      usraddr=(long*)0x80000000;
      dt=*usraddr;
      printf("Hello\n");
      printf("data read: %x\n", dt);   

      status = vme_slave_unmap(vmeaddr, 0x40000);
      if(status == -1)
      {
         printf("VME_SLAVE_UNMAP: can not desallocate vme address = %x \n",vmeaddr);
      }
      else
      {
         printf("VME_SLAVE_UNMAP: desallocate vme address = %x \n",vmeaddr);
      }
}
