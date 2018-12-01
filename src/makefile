#  Makefile for System-V flavoured UNIX
#
#CC = gcc  # you may use both ANSI and pre-ANSI

#
#  Please see source and/or README  for system defition
#
#CFLAGS = -g -DSYSV -traditional -traditional-cpp -Wid-clash-8
CFLAGS = -O

all :	miniscm

miniscm : miniscm.c
	$(CC) $(CFLAGS) -o miniscm miniscm.c -lm

miniscm : miniscm.h

clean :
	rm -f core *.o miniscm *~

