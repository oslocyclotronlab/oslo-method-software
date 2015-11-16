#include        <stdio.h>
#include        <errno.h>
#include        <sys/types.h>
#include	<sys/ioctl.h>
#include	<sys/mtio.h>
#include        <fcntl.h>

#include	<buffer_defs.h>

int write_exa( int *outp, int *messp )
{


	int			exades;	
	int			*buf1p;
	int			*buf2p;
	int			*buf3p;
	int			*buf4p;
	int			*stor2p;
        int		        stor2_off = 12;
	int			bytes_written;
	int			part_offset	= 8192;

	int	*debugp;

	struct mtop 	mt_command;
      
  
	buf1p = outp;				/* Points to part 1 of databuffer */
	buf2p = buf1p + part_offset;		/* Points to part 2 of databuffer */
	buf3p = buf1p + part_offset*2;		/* Points to part 3 of databuffer */
	buf4p = buf1p + part_offset*3;		/* Points to part 4 of databuffer */

	stor2p = messp + stor2_off;		/* Points to output exabyte file descriptor */
        debugp = messp + 6;

	exades = *stor2p;

	bytes_written = write(exades, buf1p, RECORD_LENGTH);
	bytes_written = write(exades, buf2p, RECORD_LENGTH);
	bytes_written = write(exades, buf3p, RECORD_LENGTH);
	bytes_written = write(exades, buf4p, RECORD_LENGTH);
        
	
      	if (bytes_written != RECORD_LENGTH) {
              perror("\n **** ERROR: Cannot Write to Tape, Aborting ... ***\n");
              return -1;
       	}

	return 0;
}
