
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
                                                     Oslo, December  15. 2015
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

fn Fill negative counts from neighbours 
rn Replace (remaining) negative counts with zeros

Then write (wr command) the first-generation matrix and call it fg

For the further analysis, you need programs found in the /prog directory:
rhosigchi.f 	Find Rho and T from a least-square fit to the matrix fg
d2rho.f     	Find Rho(Sn) from D0 or D1 values
rhobin.f    	Find Rho(Sn) from global systematics (e.g. Egidy&Bucurescu)
counting.c  	Normalize the level density function Rho(Ex)
normalization.c Normalize the gamma-ray strength function f(Egamma)
radex.f		Find the gamma-ray strength function from P/Rho

(There are several other programs in /prog and /mama that you do not need.
Some of them are out-dated or does not work…)

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

cd mama
make very-clean
make all
make install

cd ..
cd prog
make very-clean
make all
make install

All executables are now installed in /prog/bin

You need a .bashrc or .profile file telling where the programs are located. 
It could look like this:

*****************************************************************************

export UIO_APPLICATIONS=<applications_base_path> 	# Replace with the path where you want mama to be installed.
													# For MacOSX users this can be /Applications
PATH=$PATH:$UIO_APPLICATIONS/prog/bin

*****************************************************************************

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

**************************

For MacOSX users, you might need the following to get ROOT working:

export LC_ALL="C" 
export LANG="en_US" 
export TERM=xterm-color

PS1='$USER@\H:\W>'

# For ROOT, not installed by Fink
export ROOTSYS=/Applications/root
export PATH=$ROOTSYS/bin:$PATH
export LD_LIBRARY_PATH=$ROOTSYS/lib:$LD_LIBRARY_PATH
export LDYLD_LIBRARY_PATH=$ROOTSYS/lib:$DYLD_LIBRARY_PATH

# For Fink
test -r /sw/bin/init.sh && . /sw/bin/init.sh


*****************************

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





Enjoy, Magne

(Modfied by Fabio Zeiser and Jørgen E. Midtbø to be more generally installable than just on MacOSX.)

