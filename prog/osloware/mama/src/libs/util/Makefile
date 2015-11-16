# Makefile for util directory
          AR = ar rv
          CC = cc
      CFLAGS = -xO3 -I/usr/openwin/include
          CP = mv
      CP_DIR = ../..
         F77 = f77
      FFLAGS = -O3 -C -e
          RM = rm -f
      RM_CMD = $(RM) *.o core *~

ARCHIVE = util.a
SRCS =  ask.f \
        askyn.f \
        cask.f \
        caskyn.f \
        datetime.f \
        erf.f \
        ffin.f \
        ffin_flg.f \
        get_directory.f \
	grabchar.c \
	inin.f \
        inin_flg.f \
	lpr_file_del.c \
        matread.f \
        open_pr_file.f \
	open_saf.f \
        pr_and_del_file.f \
        readsp.f \
        rein.f \
        rmat.f \
	setext.f \
        wspec.f

OBJS =  ask.o \
        askyn.o \
        cask.o \
        caskyn.o \
        datetime.o \
        erf.o \
        ffin.o \
        ffin_flg.o \
        get_directory.o \
	grabchar.o \
	inin.o \
        inin_flg.o \
	lpr_file_del.o \
        matread.o \
        open_pr_file.o \
	open_saf.o \
        pr_and_del_file.o \
        readsp.o \
        rein.o \
        rmat.o \
	setext.o \
        wspec.o 

.f.o:
	$(RM) $@
	$(F77) -c $(FFLAGS) $*.f
.c.o:
	$(RM) $@
	$(CC) -c $(CFLAGS) $*.c

all:: $(ARCHIVE)
	$(RM_CMD) $(ARCHIVE)

$(ARCHIVE):  $(OBJS) 
	$(AR) $@ $(OBJS)
	$(CP) $(ARCHIVE) $(CP_DIR)

clean: 
	$(RM_CMD)

very-clean:
	$(RM_CMD) $(ARCHIVE)

