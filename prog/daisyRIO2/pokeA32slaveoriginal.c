#include  <stdio.h> 
#include  <smem.h>
#include  <ces/uiocmd.h>

#define MAXBUF 65536  

int main(){ 
   uio_mem_t cmem_dsc;
   long i, ad, dt, mem, *p, sz, err;
   long *buf;
   printf(" ____________________________________________________________ \r\n");
   printf("|                                                            |\r\n");
   printf("|                      PokeA32slave 1.1                      |\r\n");
   printf("|                                                            |\r\n");
   printf("|         Writing numbers 0, 1, 2,... 65535 into A32         |\r\n");
   printf("|       slave memory of the RTPC 8067. It is required,       |\r\n");
   printf("|         that the ram of the RTPC is static mapped.         |\r\n");
   printf("|       (Based on the speek, spoke and cmem functions)       |\r\n");
   printf("|                                                            |\r\n");
   printf("| E-mail  : magne.guttormsen@fys.uio.no                      |\r\n");
   printf("| Created : 04-09-1998                                       |\r\n");
   printf("| Modified: 26-09-1998                                       |\r\n");
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

   ad = cmem_dsc.paddr; 
   if (!(mem=(int)smem_create("MEM",(char *)(ad&~0xFFF),sz, SM_READ|SM_WRITE))){ 
      fprintf(stderr,"Unable to allocate MEM window\n");
      exit(0);
   }

   p = (long *)(mem | (ad&0xFFF));
   for( i = 0; i < MAXBUF; i++){
     *(p+i) = i;
     dt     = *(p+i); 
     printf("log AD=0x%08x (%ld): data write/read: %ld\n",p+i,i,dt);
   }

   printf("Congratulation, it worked! You may now see if you can read    \r\n");
   printf("the same numbers from lynx (Sun SparcStation) via bit3 with:  \r\n");
   printf("/usr/local/944/v1.1/src/readram -t BT_AXSRR -a 0x08xxxxxx     \r\n");
   printf("Here, the xxxxxx's is the physical address to start from.     \r\n");
   printf("(The first data start at physical address: 0x%08x)\n",ad           );
   printf("After this, you may type CR to remove the allocated buffer    \r\n");
   printf("from the RTPC A32 slave space. Good luck, and have a nice day!\r\n");

   fflush(stdout);
   getchar(); 
   smem_create("MEM",(char *)mem,sz,SM_DETACH); 
   smem_remove("MEM");
   if (err=uio_cfree(&cmem_dsc)) {
      uio_perror("uio_cfree",err);
      exit(0);
   }
   uio_close();
   free(buf);
   return 0;
} 
