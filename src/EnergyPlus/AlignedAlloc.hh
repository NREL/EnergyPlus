#ifndef EppPerformance_alloc_hh_INCLUDED
#define EppPerformance_alloc_hh_INCLUDED

//allocation library for alignment on cache line boundaries
//Geof Sawaya, 2014, LBL & DOE 
// should be fully portable at this point

//#include <cstddef>
#include <memory>
#include <new>

#include <AnalyzeHardware.hh>

namespace EppPerformance{

template <typename T>
struct AlignedAlloc{
  static T* allocate (std::size_t n, size_t alignment) { 
    void **ptr;
    void *raw;
    size_t toUse = sizeof(T) * n;
    size_t offset = alignment - 1 + sizeof(void*);
    raw = ::operator new(toUse + offset);
    //	 ptr = (void**)std::align(alignment, toUse, raw, toUse);  NOT IMPLEMENTED in GCC 4.8.1 :P
    size_t temp = ((size_t)raw + offset) & ~(alignment - 1);
    ptr = (void**)temp;
    ptr[ -1 ] = raw;
    return static_cast<T*>((void*)ptr);
  }

  static void deallocate (T* ptr) { ::operator delete(((void**)ptr)[ -1 ]);}
};

}

#endif
