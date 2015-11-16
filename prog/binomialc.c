/************************************************* 
   Program to calculate multiplicity and entropy 
   translated from Magne's binomial.f
**************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

static int bin(int n, int k);

int main()
{
	int n, p, h, c, s, mp;			// declaration of variables
	int i;
	double Wc, Ws, Wcs, Wtot, Scs, Stot, b;
	
	// Nucleus: 51-V. Initializing
	
	Wtot = 0;
	n = 8;		// number of states in f7/2
	p = 4;		// number of particles in f7/2
	
	
	mp = p/2.;
	for(i=mp; i >= 0; i--) {
		h = n - p;
		c = i;
		s = p - 2*c;
		b = bin(n/2,c);   // number of c pairs at n/2 places
		Wc = b;
		
		if(s > ((n-2*c)/2)) { // one or more pairs appear from sqp
			Ws = 0.;
			Wc = 0.;
		}
			else {
				b = bin(n/2-c,s);  // permutation of S sqp in doubly deg.
				Ws = (double) b*pow(2.,s);
		}
		
		Wcs  = Wc*Ws;
		Scs  = 0;
		Stot = 0;
		
		if(Wcs > 0) {
			Scs  = log(Wcs);
		}
		
		Wtot += Wcs;
		if(Wtot > 0) {
			Stot = log(Wtot);
		}
		printf("Number of available (j,m)-states: %d\n", n);
		printf("Number of particles             : %d\n", p);
		printf("Number of holes                 : %d\n", h);
		printf("Number of Cooper pairs          : %d\n", c);
		printf("Number of single quasi particles: %d\n", s);
		printf("Multiplicity of Cooper states   : %f\n", Wc);
		printf("Multiplicity of s.q.p. states   : %f\n", Ws);
		printf("Multiplicity Wc*Ws              : %f\n", Wcs);
		printf("Entropy ln(Wcs)                 : %f\n", Scs);
		printf("Total multiplicity              : %f\n", Wtot);
		printf("Total entropy ln(Wtot)          : %f\n", Stot);
		printf("\n");
	} // end of for-loop

        return 0;
} // End: function main()


/* The function bin() calculates and returns n!/k!(n-k)! */

static int bin(int n, int k)
{
	int i;
	int m, lh, ll;
	double b, x, y;
	
	m = n - k;
	if( m > k) { 
		lh = m;
	}
	if(m < k) {
		lh = k;
	}
	if(m < k) {
		ll = m;
	}
	if(m > k) {
		ll = k;
	}
		if(m == k) {
		ll = k;
		lh = k;
	}
	
	x = 1.;
	for(i = lh + 1; i <=n; i++) {
		x *= (double)i;
	}
	
	y = 1.;
	for(i = 1; i <= ll; i++) {
		y *= (double)i;
	}
	b = x/y;
	return b;
} // End: function bin()
