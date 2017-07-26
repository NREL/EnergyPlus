CC = g++

INCLUDES = -I../../../third_party

CXXFLAGS = -g -std=c++11 $(INCLUDES)

EXECUTABLES: generate_embeddable_jdd

all: $(EXECUTABLES)

.SUFFIXES: .c .o
.c.o:
	$(CC) $(CXXFLAGS) -c $*.c

generate_embeddable_jdd: generate_embeddable_jdd.o
	$(CC) $(CXXFLAGS) -o generate_embeddable_jdd generate_embeddable_jdd.o

#make clean will rid your directory of the executable,
#object files, and any core dumps you've caused
clean:
	rm core $(EXECUTABLES) *.o


