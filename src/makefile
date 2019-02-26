#  Makefile for System-V flavoured UNIX
#
#CC = gcc  # you may use both ANSI and pre-ANSI

#
#  Please see source and/or README  for system defition
#
#CFLAGS = -g -DSYSV -traditional -traditional-cpp -Wid-clash-8
CFLAGS = -O

all :	miniscm

miniscm : miniscm.o bignum.o
	$(CC) $(CFLAGS) -o miniscm miniscm.o bignum.o -lm

miniscm.o : miniscm.c miniscm.h
	$(CC) $(CFLAGS) -c miniscm.c

bignum.o : bignum.c bignum.h
	$(CC) $(CFLAGS) -c bignum.c

clean :
	rm -f core *.o miniscm *~

