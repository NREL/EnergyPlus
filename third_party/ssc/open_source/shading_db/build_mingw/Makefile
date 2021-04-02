CXX = g++
CFLAGS =  -g -O2 -I..\ 
CXXFLAGS= $(CFLAGS)


OBJECTS  = \
	..\lib_pv_shade_loss_mpp.cpp \
	..\lib_miniz.cpp \
	..\test.cpp
	
TARGET = shade_db

shade_db:
	$(CXX) $(CXXFLAGS) -g -o $(TARGET) $(OBJECTS) 
	
clean:
	del  *.exe 

all: clean shade_db