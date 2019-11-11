#include <stdio.h>
#include <stdlib.h>
/*
DO NOT REMOVE THE stdio.h and stdlib.h
because these libraries are needed for
linking to the minig_x archive 
(there is no linking to these include files
in the Makefile for the minig_x) 
Magne
*/

int grabchar_()

{
  int c;

/*  system("/bin/stty icanon"); */
  c = getchar();
/*  system("/bin/stty -icanon");*/
  return ( c );
}

