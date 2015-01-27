//Geof Sawaya, 2014, LBL & DOE

//implementation of multi-alloc array for false sharing free
//multi-threaded writing (per thread allocation)
#include <iostream>
#include <random>
#include "ThreadArray.hh"

using namespace EppPerformance;

genPerTArray::~genPerTArray(){}

bool
genPerTArray::isOptimized(){return optimized;}

void
genPerTArray::optimize(std::vector<size_t>&){}

