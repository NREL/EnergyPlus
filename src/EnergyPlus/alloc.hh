#ifndef EppPerformance_alloc_hh_INCLUDED
#define EppPerformance_alloc_hh_INCLUDED

//allocation library for alignment on cache line boundaries
//Geof Sawaya, 2014, LBL & DOE 

#include <cstddef>
#include <memory>
#include <new>

#include <utility.hh>

//we'll have to deal with portability on this one.  
//While there's a pointer alignment for c++11, std::align,
//it isn't implemented anywhere (that I know of)
// The alignment will be set to cache line size of the 
// current arch, unless specified in the constructor

namespace EppPerformance{

extern "C" {
#include <malloc.h>
}

template <typename T>
struct AlignedAlloc{
  typedef T value_type;
  AlignedAlloc(int alignment) noexcept : alignment(alignment){}
  AlignedAlloc() noexcept {
    alignment = Utility::getL1CacheLineSize();
  }
  template <class U> AlignedAlloc (const AlignedAlloc<U>&a) noexcept {
    alignment = a.alignment;
  }
  T* allocate (std::size_t n) { //return static_cast<T*>(::new(n*sizeof(T))); }
    void *ptr;
    if(posix_memalign(&ptr, alignment, n * sizeof(T)) != 0){
      throw new std::bad_alloc;
    }
    return static_cast<T*>(ptr);
  }

  void deallocate (T* p, std::size_t n) { free(p);} //::delete(p); }

private:
  int alignment;
};

template <class T, class U>
constexpr bool operator== (const AlignedAlloc<T>&, const AlignedAlloc<U>&) 
  noexcept
{return true;}

template <class T, class U>
constexpr bool operator!= (const AlignedAlloc<T>&, const AlignedAlloc<U>&) 
  noexcept
{return false;} 

}

#endif
