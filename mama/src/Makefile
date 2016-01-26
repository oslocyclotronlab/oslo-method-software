#    Makefile for mama
#    Magne Guttormsen & Alexander Bürger
#    December 2008

# most settings like F95, CC, ... are in a common file 'Makeconf'
BASEDIR = ..
include $(BASEDIR)/Makeconf

TARGET = mama

ARCHIVES = gf2.a minig_x.a
OBJ = gf2.o

all: $(TARGET)

gf2.o: FORCE
	$(MAKE) -C libs/gf2 gf2.o

mama: gf2.o gf2.a minig_custom.o minig_x.a 
	@echo "  [LINK] $@"
	@$(F95) $(FFLAGS) -o $@ $^ $(LIB_PATHS) $(LIBS)

# archives 
#       force check of libraries....
gf2.a: FORCE
	$(MAKE) -C libs/gf2 $@

minig_x.a: FORCE
	$(MAKE) -C libs/minig_x $@

.c.o:
	@echo "    [CC] $@"
	$(CC) $(CFLAGS) -o $@ -c $*.c

FORCE:

clean: 
	$(RM) *.o core *[~%]
	$(MAKE) -C libs/gf2     clean
	$(MAKE) -C libs/minig_x clean

very-clean:
	$(RM) *.o core *[~%] $(TARGET) $(ARCHIVES)
	$(MAKE) -C libs/gf2     very-clean
	$(MAKE) -C libs/minig_x very-clean
	for target in $(TARGET) ; \
		do rm -f $(UIO_APPLICATIONS)/prog/bin/$$target ; \
	done
install:
	for program in $(TARGET) ; \
		do $(INSTALL_BIN) $$program $(INSTALL_DIR)/bin/$$program; \
	done
