#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define LIN_MAX  250
#define CHUNK_SIZE  1000


/* FGets : wrapper around fgets, does some cheking of line, etc... 
           In addition, the current linenumber just read is stored
           in *linenumber. */

char *FGets( char *ln, FILE *fp, int *linenumber)
{
  char *cp, c;
  int n;
  static int lineno=0;
  
  do    /*  do/while-loop  */
    {
      /* read a line from file : */
      cp = fgets(ln,LIN_MAX,fp); 
      lineno++;
      *linenumber = lineno;

      /*  if NULL, just return NULL (ie. end of file) : */
      if ( cp == NULL ) 
	return NULL;

      /* check if the whole line was read: */
      if ( strchr(ln,'\n') == NULL && strlen(ln) == LIN_MAX -1) 
	{
	  fprintf(stderr,"FGets : maximum line length exceeded"
		  " (line %d), exiting.\n", lineno);
	  exit(1);
	}

      /* skip comments, by turning '!' into null-bytes. */
      if( (cp = strchr(ln, '!')) )
	*cp = '\0';

      /* search the line for some non-white char.....*/
      n = sscanf(ln,"%1s",&c);

      /* ... if at least one such char was found, exit from loop,
	 else, go back and try another line... */
    } while ( n < 1 );

  /* aparently all is well, return this line : */
  return ln;
}

/* file2array: reads numbers from file, puts them in an array.
   The function takes as a argument the filename, and a pointer
   to an _unallocated_ pointer to long. It allocates space for the
   pointer to long, which is filled with the numbers.
   The function returns the number of numbers read. */
   
long file2array(char *filename, long **buffer) 
{ 
  char line[LIN_MAX], *pnumber, *pcomma;
  long *pbuf, realbufs;
  FILE *fp;
  int lnr;
 
  fp = fopen(filename,"r");
  if ( !fp ) {
    fprintf(stderr,"file2array : Can't find file %s, exiting ...\n",filename);
    exit(1);
    }
 
  realbufs = CHUNK_SIZE;
  *buffer = (long *) malloc(realbufs * sizeof(long) );
  if ( !(*buffer) ) {
    fprintf(stderr,"file2array : Can't get memory, exiting ...\n");
    exit(1);
    }
 
  pbuf = *buffer;
 
  while( FGets(line,fp,&lnr) ) {  /* while there is another line ... */

    pnumber = line;  /* start on beginning of line : */
 
    /* while there is another comma on the line : */
    while( (pcomma = strchr(pnumber,',')) ) {
 
      /* convert string to long : */
      *pbuf++ = atol(pnumber);
 
      /* if overflow, make new buffer : */
      if ( pbuf == *buffer + realbufs ) { 
        *buffer = realloc( *buffer, realbufs * 2 *sizeof(long) );
        if ( !(*buffer) ) {
          fprintf(stderr,"file2array : Can't realloc, returning...\n");
          /* restore buffer, return with what we have : */
          *buffer = pbuf -  realbufs;  
          return  realbufs; 
        }
        pbuf = *buffer + realbufs;
        realbufs *= 2;
      }
 
      /* go to next comma : */
      pnumber = pcomma+1;
    }
  }
  
  return pbuf - *buffer;
}
 


long array2file(char *fname,int num,long *array)
{
  int j;
  FILE *fp;                                                              


  /* Open file */

  
  fp = fopen(fname,"w");
  if ( !fp ) {
    fprintf(stderr,"array2file : Can't open file %s, exiting ...\n",fname);
    exit(1);
    }
                                            

  /* Write data */
 
  for ( j=0;j<num;j++)
    fprintf(fp,"%10ld\n",array[j]);
 
  return 0;
}



int main(int argc, char *argv[])
{
  long *array;
  long num;
  char *infile, *outfile;

  if ( argc < 3 ) {
    fprintf(stderr,"Usage: shiva2paw <shivafile> <pawfile>\n");   
    exit(1);
  }


  infile = argv[1];
  outfile = argv[2];
 
  num = file2array( infile, &array);
  printf("%10ld\n",num);
  array2file( outfile, num,  array);

  return 0;
}



















