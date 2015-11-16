/*
****************************************************************************
* file: vmem.c
* 960229 MAWE
* 960229 MAWE last update
*
* demonstrate allocation of physically contiguous memory
* mapped for VME slave access
****************************************************************************
*/
#include	<stdio.h>
#include	<strings.h>

#include	<ces/uiocmd.h>
#include	<ces/vmelib.h>

#define		SZ	(0x1234)
#define		RP	3
#define		WP	1
#define		CT	1

#define PHYS_2_PCI(p) ((p)|0x80000000)
/*
*===========================================================================
* do it
*---------------------------------------------------------------------------
*/
main(argc,argv)
int argc;
char *argv[];
{
	long i,ad,sz,rp,wp,ct,err;
	unsigned long vad;
	uio_mem_t cmem_dsc;
	struct pdparam_slave sp;

	if ((argc==5)&&(sscanf(argv[4],"%x",&ct)==1)) {argc--;} else {ct=CT;}
	if ((argc==4)&&(sscanf(argv[3],"%x",&wp)==1)) {argc--;} else {wp=WP;}
	if ((argc==3)&&(sscanf(argv[2],"%x",&rp)==1)) {argc--;} else {rp=RP;}
	if ((argc==2)&&(sscanf(argv[1],"%x",&sz)==1)) {argc--;} else {sz=SZ;}
	if (argc != 1) {
		printf("use: %s
[<size>[<read-prefetch>[<write-post>[<count>]]]]\n",argv[0]);
		printf("     allocate <size> bytes of physically contguous
memory\n");
		printf("     map it to VME with <read-prefetch>
(0=OFF,1=4w,2=8w,3=16w)\n");
		printf("     and <write-post> (0=OFF, 1=ON).\n");
		printf("     then unmap it from VME and free it again,
<count> times\n");
		exit(0);
	}

	if(err=uio_open()) {
		uio_perror("uio_open",err);
		exit(0);
	}
	/*
	* prepare page descriptor options
	*/
	sp.rdpref = rp;			/* read prefetch */
	sp.wrpost = wp;			/* write posting */
	sp.wrprotect = 0;		/* enable writing */
	sp.swap = SINGLE_AUTO_SWAP;	/* auto-swapping */
	sp.pcispace = PCI_MEM_CES;	/* PCI memory space */

	for (i=1;i<=ct;i++) {
		/*
		* allocate memory
		*/
		printf("%d> allocating 0x%x (%d) bytes ...\n",i,sz,sz);
		if (err=uio_calloc(&cmem_dsc,sz)) {
			uio_perror("uio_calloc",err);
			break;
		}else{
			printf("kernel virtual address:
0x%08x\n",cmem_dsc.kaddr);
			printf("user   virtual address:
0x%08x\n",cmem_dsc.uaddr);
			printf("physical address      :
0x%08x\n",cmem_dsc.paddr);
			printf("size                  : %d (0x%x)
bytes\n",cmem_dsc.size,cmem_dsc.size);
		}
		/*
		* map to VME
		*/
		vad = vme_slave_map(PHYS_2_PCI(cmem_dsc.paddr),sz,&sp);
		if (vad == -1) {
			fprintf(stderr,"mapping to VME failed -
terminating\n");
			uio_cfree(&cmem_dsc);
			break;
		}else{
			printf("%d> mapped 0x%x (%d) bytes at PCI 0x%08x to
VME 0x%08x\n",
			i,sz,sz,PHYS_2_PCI(cmem_dsc.paddr),vad);
		}
		/*
		* wait for <CR> to release
		*/
		if (ct == 1) {
			printf("<CR> to continue");
			fflush(stdout);
			getchar();
		}
		/*
		* release memory, unmap VME
		*/
		if (err=uio_cfree(&cmem_dsc)) {
			uio_perror("uio_cfree",err);
			break;
		}
		if (err=vme_slave_unmap(vad,sz)) {
			fprintf(stderr,"unmapping from VME failed\n");
			break;
		}
	}
	uio_close();
}


