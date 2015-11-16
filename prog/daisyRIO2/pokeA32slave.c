#include  <stdio.h> 
#include  <ces/uiocmd.h>
#include  <ces/vmelib.h>
#define PHYS_2_PCI(p) ((p)|0x80000000)

#define MAXBUF 65536  

int main(){ 
   uio_mem_t cmem_dsc;
   struct pdparam_slave sp;
   unsigned long vad;
   long i, dt, *p, sz, err;
   printf(" ____________________________________________________________ \r\n");
   printf("|                                                            |\r\n");
   printf("|                      PokeA32slave 1.2                      |\r\n");
   printf("|                                                            |\r\n");
   printf("|         Writing numbers 0, 1, 2,... 65535 into A32         |\r\n");
   printf("|       slave memory of the RTPC 8067. It is required,       |\r\n");
   printf("|         that the ram of the RTPC is static mapped.         |\r\n");
   printf("|    (Based on the speek, spoke, vmem and cmem functions)    |\r\n");
   printf("|                                                            |\r\n");
   printf("| E-mail  : magne.guttormsen@fys.uio.no                      |\r\n");
   printf("| Created : 04-09-1998                                       |\r\n");
   printf("| Modified: 26-09-1998 13-07-99                              |\r\n");
   printf("|____________________________________________________________|\r\n");
   printf("                                                              \r\n");
   printf("This program can be run to check if A32 slave memory works.   \r\n");
   printf("Type CR to write 64 kwords into memory. The RTPC may crash,   \r\n");
   printf("so - if you are not sure - type CTRL C to exit now...         \r\n");

   fflush(stdout);
   getchar(); 
   sz = 4 * MAXBUF;
   if(err=uio_open()) {
      uio_perror("uio_open",err);
      exit(0);
   }

   /*
   * prepare page descriptor options
   */
   sp.rdpref = 0;			/* read prefetch (before = 3, magne)*/
   sp.wrpost = 0;			/* write posting (before = 1, magne)*/
   sp.wrprotect = 0;		/* enable writing */
   sp.swap = SINGLE_AUTO_SWAP; /* auto-swapping */
   sp.pcispace = PCI_MEM_CES;	/* PCI memory space */
	
   printf("Allocating 0x%x (%d) bytes ...\n",sz,sz);
   if (err=uio_calloc(&cmem_dsc,sz)) {
      uio_perror("uio_calloc",err);
      exit(0);
   }else{
      printf("Kernel virtual address: 0x%08x\n",cmem_dsc.kaddr);
      printf("User   virtual address: 0x%08x\n",cmem_dsc.uaddr);
      printf("Physical address      : 0x%08x\n",cmem_dsc.paddr);
      printf("Size                  : %d (0x%x) bytes\n",cmem_dsc.size, cmem_dsc.size);
   }


   /*
   * map to VME
   */

vad = vme_slave_map(PHYS_2_PCI(cmem_dsc.paddr),sz,&sp);
   if (vad == -1) {
      fprintf(stderr,"mapping to VME failed -terminating\n");
      uio_cfree(&cmem_dsc);
      exit(0);
   }else{
      printf("Mapped 0x%x (%d) bytes at PCI 0x%08x to VME 0x%08x\n",
      sz,sz,PHYS_2_PCI(cmem_dsc.paddr),vad);
   }

   printf("<CR> to continue");
   fflush(stdout);
   getchar();

   p  = (u_long *)cmem_dsc.uaddr;
   for( i = 0; i < MAXBUF; i++){
     *(p+i) = i;
     dt     = *(p+i); 
     if((int)(i/10000.)*10000. == i){
        printf("log AD=0x%08x (%ld): data write/read: %ld\n",p+i,i,dt);
     }
   }
   printf("log AD=0x%08x (%ld): data write/read: %ld\n",p+i-1,i-1,dt);


   printf("Congratulation, it worked! You may now see if you can read    \r\n");
   printf("the same numbers from lynx (Sun SparcStation) via bit3 with:  \r\n");
   printf("/usr/local/944/v1.1/src/readram -t BT_AXSRR -a 0x08xxxxxx     \r\n");
   printf("Here, the xxxxxx's is the physical address to start from.     \r\n");
   printf("(The first data start at physical address:0x%08x)\n",cmem_dsc.paddr);
   printf("After this, you may type CR to remove the allocated buffer    \r\n");
   printf("from the RTPC A32 slave space. Good luck, and have a nice day!\r\n");

   fflush(stdout);
   getchar(); 

   /*
   * release memory, unmap VME
   */
   if (err=uio_cfree(&cmem_dsc)) {
      uio_perror("uio_cfree",err);
      exit(0);
   }
   err=vme_slave_unmap(vad,sz);
   if (err == -1) {
      fprintf(stderr,"unmapping from VME failed\n");
      exit(0);
   }

   uio_close();
   return 0;
} 
