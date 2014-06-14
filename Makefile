CC=gcc
CFLAGS=-c -std=c99 -Wall -O3
LD=gcc
LDFLAGS=

OBJECTS=sim.o

all: sim

sim: $(OBJECTS)
	$(LD) $(LDFLAGS) $^ -o $@

sim.o: sim.c
	$(CC) $(CFLAGS) $^

clean:
	-rm *.o
	rm sim
