CXX = cl
CFLAGS =  /EHsc  /I..\ 
CXXFLAGS= $(CFLAGS)

SRC_FILES = \
	..\lib_pv_shade_loss_mpp.cpp \
	..\lib_miniz.cpp \
	..\test.cpp
	
TARGET = shade_db
    
shade_db:
	$(CXX) $(CXXFLAGS) /Fe$(TARGET) $(SRC_FILES) 
	
clean:
	del  *.exe *.obj 

all: clean shade_db