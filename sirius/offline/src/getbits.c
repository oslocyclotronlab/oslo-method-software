#include	<stdio.h>
#include	<stdlib.h>

unsigned getbits(unsigned x, int p, int n)
{
	return ( x >> (p + 1 - n)) & ~(~0 << n);
}
