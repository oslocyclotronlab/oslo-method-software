#*****************************************************************************
#*
#*    Filename: Makefile
#*
#*    Purpose:  To compile the Model SBS 1003 for Sirius at OCL
#*    (built on makefile for the Model 1003 /usr/local/SBS/1003/src software)
#*    
#*****************************************************************************
#*****************************************************************************
#*
#*        Magne Guttormsen  April 3. 2008
#*          magne.guttormsen@fys.uio.no
#*           Oslo Cyclotron Laboratory
#*
#*****************************************************************************

#*****************************************************************************
#*    Makefile macros
#*****************************************************************************

CP    = mv
CP_BIN = $(CP) engine /Applications/sirius/bin/acq_engine

# This is required to identify what SBS Software Product is being used
EXTRA_DEFINE=-DBT1003
CC_OPTIM=-O2
ADDED_CFLAGS=-g -DDEBUG
#ADDED_CFLAGS=-DNDEBUG

# Possible location of SBS Mirror API header files
BTINCLUDE=/usr/local/SBS/1003/include
LIB_EXTRA=-L$(BTINCLUDE) -lbtp -lpthread 
EXTRA_INCLUDE=/Applications/sirius/include

CPPFLAGS =-I. -I$(BTINCLUDE) -I$(EXTRA_INCLUDE) $(EXTRA_DEFINE) -D_XOPEN_SOURCE -D_POSIX_C_SOURCE -D_REENTRANT

CFLAGS=$(CC_OPTIM) $(ADDED_CFLAGS) -Wall -Wstrict-prototypes

#*****************************************************************************
#* Targets
#*****************************************************************************

SOURCES = engine.c
EXAMPLES = $(SOURCES:%.c=%)

#*****************************************************************************
#* Dependencies
#*****************************************************************************

#exe all: $(EXAMPLES)

%: %.c
	$(CC) $(CPPFLAGS) $(CFLAGS) $< -o $@ $(LIB_EXTRA)

clean:
	$(RM) *.o $(EXAMPLES)

install:
	$(CP_BIN)

