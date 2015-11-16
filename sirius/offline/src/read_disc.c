#include	<stdio.h>
#include	<stdlib.h>
#include	<errno.h>
#include	<sys/types.h>
#include	<sys/ioctl.h>
#include	<fcntl.h>
#include	<buffer_defs.h>
#include	<string.h>
#include	<unistd.h>
#define swap(x) ((x&0xff000000)>>24)+((x&0x00ff0000)>>8)+((x&0x0000ff00)<<8)+((x&0x000000ff)<<24)


int read_disc( int *bufp, int *messp)
{
	int			i, word;
	int			exades;	
	int			*buf1p;
	int			*buf2p;
	int			*buf3p;
	int			*buf4p;
	int			*filedesp;
	int			*filerecp;
	int			filedisc;

	int			bytes_read;
	int			part_offset	= 8192;
	int			status		= 0;
	int			eof_reached	= 2;
	int			eom_reached	= 3;
	int			error_in_read	= 9;
	char		s1[4];
	char		in_file[128];				/* file name from disc file_0, file_1, ... */
  
	buf1p = bufp;							/* Points to part 1 of databuffer */
	buf2p = buf1p + part_offset;			/* Points to part 2 of databuffer */
	buf3p = buf1p + part_offset*2;			/* Points to part 3 of databuffer */
	buf4p = buf1p + part_offset*3;			/* Points to part 4 of databuffer */

	filedesp = messp + 2;					/* Points to exabyte file descriptor */
	filerecp = messp + 13;					/* Points to present file+rec number*/
	exades   = *filedesp;
        
/* Using same recordlength as for exabytes (SIRIUS), reading 4 times per record */
	bytes_read = read(exades, buf1p, RECORD_LENGTH);
	if ( bytes_read != RECORD_LENGTH ) {
		if ( bytes_read == 0 ) {												/* EOF or EOM */
			close(exades);
			filedisc = *filerecp/1000000;
			number2string(filedisc + 1, s1);									/* Jump to next file */
			sprintf(in_file,"file_%s",s1);
			if ((exades = open(in_file,O_RDONLY)) == -1) {				/* Next file do not exist */
				return eom_reached; 
			} else {
				*filerecp = (filedisc + 1)*1000000;
				*filedesp = exades;
			}
			return eof_reached;
		} else {
			return error_in_read;
		}
	}
	bytes_read = read(exades, buf2p, RECORD_LENGTH);
	if ( bytes_read != RECORD_LENGTH ) {
		if ( bytes_read == 0 ) {												/* EOF or EOM */
			close(exades); 
			filedisc = *filerecp/1000000;
			number2string(filedisc + 1, s1);									/* Jump to next file */
			sprintf(in_file,"file_%s",s1);
			if ((exades = open(in_file,O_RDONLY)) == -1) {				/* Next file do not exist */
				return eom_reached; 
			} else {
				*filerecp = (filedisc + 1)*1000000;
				*filedesp = exades;
			}
			return eof_reached;
		} else {
			return error_in_read;
		}
	}
	bytes_read = read(exades, buf3p, RECORD_LENGTH);
	if ( bytes_read != RECORD_LENGTH ) {
		if ( bytes_read == 0 ) {												/* EOF or EOM */
			close(exades); 
			filedisc = *filerecp/1000000;
			number2string(filedisc + 1, s1);									/* Jump to next file */
			sprintf(in_file,"file_%s",s1);
			if ((exades = open(in_file,O_RDONLY)) == -1) {				/* Next file do not exist */
				return eom_reached; 
			} else {
				*filerecp = (filedisc + 1)*1000000;
				*filedesp = exades;
			}
			return eof_reached;
		} else {
			return error_in_read;
		}
	}
	bytes_read = read(exades, buf4p, RECORD_LENGTH);
	if ( bytes_read != RECORD_LENGTH ) {
		if ( bytes_read == 0 ) {												/* EOF or EOM */
			close(exades); 
			filedisc = *filerecp/1000000;
			number2string(filedisc + 1, s1);									/* Jump to next file */
			sprintf(in_file,"file_%s",s1);
			if ((exades = open(in_file,O_RDONLY)) == -1) {				/* Next file do not exist */
				return eom_reached; 
			} else {
				*filerecp = (filedisc + 1)*1000000;
				*filedesp = exades;
			}
			return eof_reached;
		} else {
			return error_in_read;
		}
	}
	
/* **************************************/
/* Swap bytes from intel to PowerPC cpu	*/
/* Take away the comments in the next   */
/* 4 lines if you have PowerPC cpu	    */
/* **************************************/
/*	for(i = 0; i < 32768; i++){
		word = *(bufp + i);
		*(bufp + i) = swap(word);
	}
*/
	*filerecp = *filerecp + 1;
	return 0;
}
