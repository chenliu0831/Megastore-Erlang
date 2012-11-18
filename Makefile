#MAKEFILE FOR AMS 595 HW2: POINT IN POLYGON TEST

CC = gcc
CFLAGS = -Wall 
DEPS = inPolygon.h
SRC = $(wildcard *.c)

test_inPolygon: $(SRC)
	$(CC) -o $@ $^ $(CFLAGS) 

clean:
	rm *.o *~ test_inPolygon


