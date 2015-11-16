#####################################################################
# Make file to create PEDAPLOT program
# on Linux Intel Fortran V11.0 compiler
# Created by Andrew Stuchbery (Andrew.Stuchbery@anu.edu.au)
# source file: 
# library: NONE
#####################################################################
# Compiler flags - comment out the one being not used

# Linux Lahey-Fujitcu LF95
#FFLAGS=-c
#F77=lf95
#F90=lf95
#LFLAGS=--staticlink

# Linux Intel F90

# debug
#FFLAGS=-c -g -O0 -traceback
# non-debug
FFLAGS=-c -O

F77=ifort 
F90=ifort 
#LFLAGS=-static-libcxa

# UNIX Digital Fortran 90
#FFLAGS=-c
#F77=f77
#F90=f90
#LFLAGS=

##################################################################
# Usage:  make pedaplot<cr>
LIB_DIR=/home/tibor/fortran/lib
BIN_DIR=/home/tibor/bin
PEDAPLOT_EXE=pedaplot

PEDAPLOT_SRC_F90 = \
      pedaplot.f90 \
      R3PACK.for \
      DCOSUBS1.for
      
      
 
######################################

.SUFFIXES: .f90 .for .o

.f90.o:
	$(F90) $(FFLAGS) -o $(@:.f90=.o) $<


 all: pedaplot

 pedaplot:  $(PEDAPLOT_SRC_F90:.f90=.o) 
	  $(F90) $(LFLAGS) -o $(BIN_DIR)/$(PEDAPLOT_EXE) \
          $(PEDAPLOT_SRC_F90:.f90=.o) 

######################################
# Usage: make clean<cr>

clean :
	rm *.o 
