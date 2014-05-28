#
# Makefile for lab3 in Advanced Computer Architecture.
#
# Author: Andreas Sandberg <andreas.sandberg@it.uu.se>
#
# $Id: Makefile 1187 2011-11-03 13:27:42Z ansan501 $
#
CC=gcc
CFLAGS=-std=c99 -D_XOPEN_SOURCE=600 -O3 -Wall -Werror
LDFLAGS=
LIBS=-lm -lrt
TMP_PREFIX=/var/tmp/$$USER.

all: gs_seq gs_pth

gs_pth: gs_common.o gsi_pth.o timing.o
	$(CC) $(LDFLAGS) -o $@ $^ $(LIBS) -lpthread

gs_seq: gs_common.o gsi_seq.o timing.o
	$(CC) $(LDFLAGS) -o $@ $^ $(LIBS)

test: gs_seq gs_pth
	@echo '**********************************************************************'
	@echo 'Starting sequential reference run...'
	@echo '**********************************************************************'
	./gs_seq -s 512 -o $(TMP_PREFIX)gs_seq.test.out
	@echo

	@echo '**********************************************************************'
	@echo 'Starting parallel run...'
	@echo '**********************************************************************'
	./gs_pth -s 512 -o $(TMP_PREFIX)gs_pth.test.out
	@echo

	@echo "Test results: "
	@if diff -q "$(TMP_PREFIX)gs_seq.test.out" \
		    "$(TMP_PREFIX)gs_pth.test.out" >/dev/null; then	\
		echo 'OK';						\
		rm -f $(TMP_PREFIX)*.test.out;				\
	else								\
		echo 'MISMATCH';					\
		rm -f $(TMP_PREFIX)*.test.out;				\
		exit 1;							\
	fi


clean:
	rm -f gs_seq gs_pth *.o

.PHONY: all test clean
