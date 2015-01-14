#ifndef EppPerformance_alloc_hh_INCLUDED
#define EppPerformance_alloc_hh_INCLUDED

//allocation library for alignment on cache line boundaries
//Geof Sawaya, 2014, LBL & DOE 

//#include <cstddef>
#include <memory>
#include <new>

#include <utility.hh>

//we'll have to deal with portability on this one.  
//While there's a pointer alignment for c++11, std::align,
//it isn't implemented anywhere (that I know of)
// The alignment will be set to cache line size of the 
// current arch, unless specified in the constructor

namespace EppPerformance{

// extern "C" {
// #include <malloc.h>
// }

template <typename T>
struct AlignedAlloc{
  // AlignedAlloc(int alignment) noexcept : alignment(alignment){}
  // AlignedAlloc() noexcept {
  //   alignment = Utility::getL1CacheLineSize();
  // }
  // template <class U> AlignedAlloc (const AlignedAlloc<U>&a) noexcept {
  //   alignment = a.alignment;
  // }
  static T* allocate (std::size_t n, size_t alignment) { //return static_cast<T*>(::new(n*sizeof(T))); }
    void **ptr;
    void *raw;
    size_t toUse = sizeof(T) * n;
    size_t offset = alignment - 1 + sizeof(void*);
    raw = ::operator new(toUse + offset);
    //	 ptr = (void**)std::align(alignment, toUse, raw, toUse);  NOT IMPLEMENTED in GCC 4.8.1 :P
    size_t temp = ((size_t)raw + offset) & ~(alignment - 1);
    ptr = (void**)temp;
    ptr[ - 1 ] = raw;
    // if(posix_memalign(&ptr, alignment, n * sizeof(T)) != 0){
    //   throw std::bad_alloc(;
    // }
    return static_cast<T*>((void*)ptr);
  }

  static void deallocate (T* ptr) { ::operator delete(((void**)ptr)[ -1 ]);} //::delete(p); }
};

// template <class T, class U>
// constexpr bool operator== (const AlignedAlloc<T>&, const AlignedAlloc<U>&) 
//   noexcept
// {return true;}

// template <class T, class U>
// constexpr bool operator!= (const AlignedAlloc<T>&, const AlignedAlloc<U>&) 
//   noexcept
// {return false;} 

}

#endif
