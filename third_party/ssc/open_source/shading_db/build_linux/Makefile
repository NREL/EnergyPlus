CXX = g++
CFLAGS =  -g -Wall -I../ 
CXXFLAGS= $(CFLAGS)


OBJECTS  = \
	../lib_pv_shade_loss_mpp.cpp \
	../lib_miniz.cpp \
	../test.cpp
	
TARGET = shade_db

shade_db:
	$(CXX) $(CXXFLAGS) -g -o $(TARGET) $(OBJECTS) 
	
clean:
	rm -rf $(TARGET)

all: clean shade_db
