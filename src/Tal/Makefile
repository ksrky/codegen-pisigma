CC = gcc
CFLAGS = -Wall -g

all: tal-test

tal-test: test.o context.o instruction.o interpreter.o
	$(CC) $(CFLAGS) -o tal-test test.o context.o instruction.o interpreter.o

test.o: test.c context.h
	$(CC) $(CFLAGS) -c test.c

context.o: context.c spec.h
	$(CC) $(CFLAGS) -c context.c

instruction.o: instruction.c spec.h context.h
	$(CC) $(CFLAGS) -c instruction.c

interpreter.o: interpreter.c spec.h context.h
	$(CC) $(CFLAGS) -c interpreter.c

clean:
	rm -f *.o tal-test
