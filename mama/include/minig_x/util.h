#ifndef UTIL_H
#define UTIL_H

int cask(char *instring, char *outstring, int *pmaxretlen);
/* returns number of chars in outstring */
int caskyn(char *instring);
/* returns 0 for no, 1 for yes */
int datetime_(char *dattim);
int ffin(char *instring, int *pnc, float *out1, float *out2, float *out3);
/* returns 1 on error */
int file_error(char *inmessage, char *infilnam);
int get_directory(char *env_name, char *dir_name, int maxchar);
int inin(char *instring, int *pnc, int *out1, int *out2, int *out3);
/*  returns 1 on error */
int inq_file(char *infilnam, int *outreclen);
/* returns 0 for file not exists, 1 for file exists */
int matread(char *fn, float *sp, char *namesp, int *numch, int idimsp,
	    int *spmode);
/* returns 1 on error */
FILE *open_new_unf(char *filnam);
FILE *open_pr_file(char *filnam);
FILE *open_readonly(char *filnam);
/* return valid file descriptor on success, NULL on failure */
int pr_and_del_file__(char *filnam);
int read_cal_file(char *infilnam, char *outtitle, int *outorder,
		  double *outpars);
/* returns 1 on error */
int read_eff_file(char *infilnam, char *outtitle, float *outpars);
/* returns 1 on error */
int read_spe_file(char *infilnam, float *outspec,
		  char *outnam, int *outnumchs, int inmaxchs);
/* returns 1 on error */
int read_tab_file(char *infilnam, int *outnclook, int *outlookmin,
		  int *outlookmax, short *outlooktab, int inmaxchs);
/* returns 1 on error */
int readsp(char *filnam, float *sp, char *namesp, int *numch, int idimsp);
/* returns 1 on error */
int rmat(FILE *fd, int ich0, int nchs, short *matseg);
int rmat4b(FILE *fd, int ich0, int nchs, int *matseg);
int wmat(FILE *fd, int ich0, int nchs, short *matseg);
int wmat4b(FILE *fd, int ich0, int nchs, int *matseg);
int setext(char *filnam, char *cext, int filnam_len);
int spkio(int mode, FILE *spkfile, int idn, int *ihed, int maxh,
	  int *idat, int *ndx, int nch);
/* mode = 0,1,2 says initialize, read, write
   mode = 6 says return id-list in idat (idat[0] = # of id's)
   spkfile = file descr. of open file
   idn  = requested id #
   ihed - array to contain header
   maxh = maximum length of ihed in half-words
   idat - array to contain data
   ndx  - array to contain indices of 1st channel to xfer
   nch  = # of channels to xfer
   returns 0 on success, error code on failure */
FILE *spkman(char *mode, char *namf, char *iacp);
/* open/create spk-files for input/output
   mode = "OPEN", "CREA"
   namf = filename
   iacp = "RO", "RW" = access mode
   returns valid file descriptor on success, NULL on failure */
int spkread(char *fn, float *sp, char *namesp, int *numch, int idimsp);
/* returns 1 on error */
int wspec(char *filnam, float *spec, int idim);
int wrresult(char *out, float value, float err, int minlen);

void swapb8(char *buf);
void swapb4(char *buf);
void swapb2(char *buf);
int get_file_rec(FILE *fd, void *data, int maxbytes, int swap_bytes);
int put_file_rec(FILE *fd, void *data, int numbytes);


#endif // UTIL_H