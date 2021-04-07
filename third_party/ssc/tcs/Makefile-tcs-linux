SSCDIR=../..

CC = gcc
CXX = g++
CFLAGS=-I.. -I$(SSCDIR) -fPIC -Wall -g -O3
CXXFLAGS=$(CFLAGS)



OBJECTS= \
	cavity_calcs.o \
	datatest.o \
	direct_steam_receivers.o \
	htf_props.o \
	interpolation_routines.o \
	powerblock.o \
	pump.o \
	sam_csp_util.o \
	sam_dsg_controller_type265.o \
	sam_lf_st_pt_type232.o \
	sam_mw_csp_SumCalcs.o \
	sam_mw_gen_Type260.o \
	sam_mw_lf_type261_steam.o \
	sam_mw_lf_type262_salt.o \
	sam_mw_pt_type221.o \
	sam_mw_pt_type222.o \
	sam_mw_pt_type224.o \
	sam_mw_pt_type228.o \
	sam_mw_trough_type250.o \
	sam_mw_trough_type251.o \
	sam_mw_type234.o \
	sam_pf_dish_collector_type295.o \
	sam_pf_dish_engine_type297.o \
	sam_pf_dish_parasitic_type298.o \
	sam_pf_dish_receiver_type296.o \
	sam_trough_model_type805.o \
	sam_trough_plant_type807.o \
	sam_trough_storage_type806.o \
	solarcollector.o \
	storage_hx.o \
	sumprod.o \
	trnsys_weatherreader.o \
	typelib.o \
	weatherreader.o


TARGET= typelib.dylib

$(TARGET):$(OBJECTS)
	$(CXX) -shared -o $(TARGET) $(OBJECTS) ../waterprop/waterprop64.dylib
#	$(CXX) -shared -o $(TARGET) $(OBJECTS) $(SSCDIR)/recore/recore.a ../waterprop/waterprop64.dylib

clean:
	rm -rf $(OBJECTS) $(TARGET)
