
# README-file for OsloSoftware

Concept DOI: [![DOI](https://zenodo.org/badge/46267755.svg)](https://zenodo.org/badge/latestdoi/46267755)

When citing, please use the DOI of the specific verion, which will be accessible via Zenodo.

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
                                                     Oslo, December  15. 2015

For questions or issues with installation, you may also contact Jørgen E. Midtbø,
j.e.midtbo@fys.uio.no.



Enjoy, Magne

(The package and README was modified in January 2016 by Fabio Zeiser and Jørgen E. Midtbø to be more generally installable than just on MacOSX.)

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

In order to do the Oslo method, you need to run the mama-commands:

	re Read in the raw particle-gamma matrix
	rm Make response matrix
	un Unfold the particle-gamma matrix
	fg Run the first-generation procedure on the unfolded particle-gamma matrix

after un and fg, you should use the commands:

	fn # Fill negative counts from neighbours 
	rn # Replace (remaining) negative counts with zeros

Then write (wr command) the first-generation matrix and call it fg

For the further analysis, you need programs found in the /prog directory:  
rhosigchi.f 	Find Rho and T from a least-square fit to the matrix fg  
d2rho.f     	Find Rho(Sn) from D0 or D1 values  
rhobin.f    	Find Rho(Sn) from global systematics (e.g. Egidy&Bucurescu)  
counting.c  	Normalize the level density function Rho(Ex)  
normalization.c Normalize the gamma-ray strength function f(Egamma)  
radex.f		Find the gamma-ray strength function from P/Rho  

(There are several other programs in /prog and /mama that you do not need.
Some of them are out-dated or do not work…)

*****************************************************************************

# Installation:

You need a Mac or a Unix/Linux computer with the following software installed:  
	gfortran  
	gcc  
	g++  
	(On linux (Ubuntu) you will also need the packages libx11-dev, libxt-dev, libreadline-dev, libncurses-dev)

You would also benefit from downloading ROOT from CERN (see below).

Then, clone this repository (or download and unpack the zip file) into the folder where you want the software to be installed. Let us say it is 

	/path-to-software/oslo-method-software/

This folder should now contain the three subfolders mama/, prog/ and sirius/, among other things.

You must then add this path to your system environment variables in the following way:

Edit your /home/user/.bash_profile file (or something equivalent, if you prefer and know what you are doing) and add the two lines

	export UIO_APPLICATIONS=/path-to-software/oslo-method-software
	PATH=$PATH:$UIO_APPLICATIONS/prog/bin

You must of course replace the installation path with your own.

Then, navigate in a terminal to your installation directory, and execute the following commands:

	cd mama
	make very-clean
	make all
	make install
	
	cd ..
	cd prog
	make very-clean
	make all
	make install

All executables are now installed in /path-to-software/oslo-method-software/prog/bin, but they should also be in your $PATH so you can run them from anywhere by typing e.g.
	
	mama

This should open the mama prompt, as well as a graphical window.

*****************************************************************************

# Ensuring ROOT support

To our knowledge, we have now ensured compatibility with ROOT 6 in all cases. Please report if you (still) experience backward compatibility issues.

To get ROOT working the environment variable ROOTSYS needs to be defined. If you have ROOT installed, you should already have this, 
since it is set automatically by the script /path-to-root/bin/thisroot.sh, which you should be calling in some startup script, e.g. in your
/home/user/.profile file.

You can test whether you have the correct setup by typing

	echo $ROOTSYS

in a terminal. If it prints the path to your ROOT directory, you are all set. If not, open your /home/user/.profile file in a text editor and add the line 

	. /path-to-root/bin/thisroot.sh # for sh shell user, e.g. bash users (if you are unsure which shell you are running, use this)

or

	source /path-to-root/bin/thisroot.csh # for csh shell users

to the end of it.

## ROOT for Mac

For MacOSX users, you might need the following to get ROOT working:

	export LC_ALL="C" 
	export LANG="en_US" 
	export TERM=xterm-color
	
	PS1='$USER@\H:\W>'

For ROOT, not installed by Fink  
	export ROOTSYS=/Applications/root
	export PATH=$ROOTSYS/bin:$PATH
	export LD_LIBRARY_PATH=$ROOTSYS/lib:$LD_LIBRARY_PATH
	export LDYLD_LIBRARY_PATH=$ROOTSYS/lib:$DYLD_LIBRARY_PATH

For Fink
	test -r /sw/bin/init.sh && . /sw/bin/init.sh


*****************************

# Fixing a font problem

For Ubuntu (and possibly other GNU/Linux) users, there might be a font problem.
This manifests itself when you open mama as a complaint about something called X_OpenFont. 

The solution is to install the apt package

	xfonts-100dpi

(i.e. do
	sudo apt-get install xfonts-100dpi
in an Ubuntu terminal)

and then run the following terminal commands: 
	
	cd /usr/share/fonts/X11/100dpi/
	sudo mkfontdir
	xset fp+ /usr/share/fonts/X11/100dpi
	
To add the path permanently, add
	FontPath /usr/share/fonts/X11/100dpi
to ~/.xinitrc



*****************************


