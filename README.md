# Oslo software
 



README-file for OsloSoftware

*****************************************************************************

Magne Guttormsen
Department of Physics 
University of Oslo
P. Box 1048, Blindern
N-0316 OSLO

E-mail: magne.guttormsen@fys.uio.no                  Oslo, September 27. 2014
                                                     MSU , February  18. 2015
                                                     Oslo, June      26. 2015
                                                     Oslo, October   23. 2015
                                                     Oslo, November  11. 2015
*****************************************************************************

OsloSoftware is copylefted free software: you can redistribute it and/or 
modify it under the terms of the GNU General Public License as published by 
the Free Software Foundation, see http://www.gnu.org.
This program is distributed in the hope that it will be useful, 
but WITHOUT ANY WARRANTY; without even the implied warranty of 
MERCHANTABILITY or FITNESS FOR A PARTICULAR PUR-POSE. 
See the GNU General Public License for more details. 
For your reports and publications, 
you should refer to the following publications:

1. The Oslo method
A. Schiller, L. Bergholt, M. Guttormsen, E. Melby, J. Rekstad,
and S. Siem, Nucl. Instrum. Methods Phys. Res. A 447 494 (2000).

2. The unfolding procedure:
M. Guttormsen, T.S. Tveter, L. Bergholt, F. Ingebretsen, and
J. Rekstad, Nucl. Instrum. Methods Phys. Res. A 374, 371 (1996).

3. The first generation gamma-ray spectra procedure:
M. Guttormsen, T. Ramsøy, and J. Rekstad, 
Nucl. Instrum. Methods Phys. Res. A 255, 518 (1987).

*****************************************************************************

In order to do the Oslo method, you need to run the following MAMA commands:

Read in the raw particle-gamma matrix with the command:

re 

Make response matrix with the command:

rm 

Unfold the particle-gamma matrix:

un 

Run the first-generation procedure on the unfolded particle-gamma matrix
to get the distribution of primary gamma rays for each excitation energy:

fg 

To remove negative counts:
after 'un' and 'fg', you should use the following commands:

Fill negative counts from neighbours:

fn 

Replace (remaining) negative counts with zeros:

rn 

Then write out the first-generation matrix:

wr 

and call it fg (or some other name you find convenient)

For the further analysis, you need programs found in the /prog directory:

rhosigchi.f 	-- Find Rho and T from a least-square fit to the matrix fg

d2rho.f     	-- Find Rho(Sn) from D0 or D1 values

rhobin.f    	-- Find Rho(Sn) from global systematics (e.g. Egidy&Bucurescu)

counting.c  	-- Normalize the level density function Rho(Ex)

normalization.c -- Normalize the gamma-ray strength function f(Egamma)

radex.f		-- Find the gamma-ray strength function from P/Rho

(There are several other programs in /prog and /mama that you do not need.
Some of them are out-dated or do not work…)

*****************************************************************************

Installation:

You need a Mac or a Unix/Linux computer with installed:
gfortran
gcc
g++

You would also benefit from downloading root from CERN

The three folders:
/mama
/prog
/sirius

have to be located at the same level. For Mac, place them
inside the /Applications folder.
(The sirius folder just keeps some include-files that mama needs,
and you should not compile anything inside the sirius folder)

*****************************************************************************

After the three folders are downloaded and unzipped, 
you write in a terminal window:

> cd mama

> make very-clean

> make all

> make install



> cd ..

> cd prog

> make very-clean

> make all

> make install


All executables are now installed in /prog/bin

You need a .bashrc or .bash_profile file telling where the programs are located. 
It could look like this:

*****************************************************************************

PATH=$PATH:/Applications/prog/bin

export LC_ALL="C"
export LANG="en_US"
export TERM=xterm-color

PS1='$USER@\H:\W>'

## For ROOT, not installed by Fink
export ROOTSYS=/Applications/root
export PATH=$ROOTSYS/bin:$PATH
export LD_LIBRARY_PATH=$ROOTSYS/lib:$LD_LIBRARY_PATH
export LDYLD_LIBRARY_PATH=$ROOTSYS/lib:$DYLD_LIBRARY_PATH

## For Fink
test -r /sw/bin/init.sh && . /sw/bin/init.sh

*****************************************************************************

Enjoy, Magne

