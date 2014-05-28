########################################################################
# Makefile for MPItutorial
#
########################################################################

CC         =  mpicc
CCFLAGS    =  -O3
LIBS       =  -lmpi

all:
	@echo "Usage: make hello"

hello:          hello.c
	$(CC) $(CCFLAGS) -o hello hello.c $(LIBS)

exchange:       exchange.c
	$(CC) $(CCFLAGS) -o exchange exchange.c $(LIBS)

pingpong:       pingpong.c
	$(CC) $(CCFLAGS) -o pingpong pingpong.c $(LIBS)

onetoall:          onetoall.c
	$(CC) $(CCFLAGS) -o onetoall onetoall.c $(LIBS)

pi:             pi.c
	$(CC) $(CCFLAGS) -o pi pi.c $(LIBS)

datatypes:             datatypes.c
	$(CC) $(CCFLAGS) -o datatypes datatypes.c $(LIBS)

communicators:             communicators.c
	$(CC) $(CCFLAGS) -o communicators communicators.c $(LIBS)


