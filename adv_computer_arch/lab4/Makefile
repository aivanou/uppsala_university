#
# Makefile for lab4 in Advanced Computer Architecture.
#
# Author: Andreas Sandberg <andreas.sandberg@it.uu.se>
#
# $Id: Makefile 1189 2011-11-03 13:31:05Z ansan501 $
#

ifdef NO_SSE41
SSE_ARCH_FLAGS=-msse3
MOV_INSTRUCTIONS=MOVDQU MOVDQA
else
SSE_ARCH_FLAGS=-msse4.1
MOV_INSTRUCTIONS=MOVDQU MOVDQA MOVNTDQ
endif

CC=gcc
ARCH=-m64 $(SSE_ARCH_FLAGS)
CFLAGS=-std=gnu99 -O2 $(ARCH) -g -Wall
LDFLAGS=$(ARCH)
LIBS=-lrt

all: $(MOV_INSTRUCTIONS:%=lcase_%) \
	matmul_SSE_BLOCKED matmul_SSE matmul_BLOCKED \
	matvec

matvec: matvec.o util.o
	$(CC) $(LDFLAGS) -o $@ $^ $(LIBS)

lcase_%: lcase_%.o util.o
	$(CC) $(LDFLAGS) -o $@ $^ $(LIBS)

lcase_%.o: lcase.c
	$(CC) -c -o $@ -DACCESS_TYPE=ACCESS_$* $(CFLAGS) $< $(LIBS)

matmul_%: matmul_%.o util.o
	$(CC) $(LDFLAGS) -o $@ $^ $(LIBS)

matmul_%.o: matmul.c
	$(CC) -c -o $@ -DMODE=MODE_$* $(CFLAGS) $<

matmul.c: util.h

util.c: util.h

lcase.c: util.h

clean:
	$(RM) *.o lcase_* matmul_* matvec

.PHONY: clean all
