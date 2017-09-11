#DEBUG=-g
DEBUG=
SCHEME_H_DIR=..
CC=gcc
CFLAGS=-DUSE_DL=1 -I $(SCHEME_H_DIR)

tsx.so : tsx.c tsx.h Makefile
	$(CC) -shared -Wall -fPIC $(CFLAGS) -o tsx.so $(DEBUG) tsx.c
	strip tsx.so
	ls -l tsx.so

.PHONY : clean
clean:
	rm -f *.o
	rm -f tsx.so
	rm -f *~