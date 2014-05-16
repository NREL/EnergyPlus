#ifndef EppPerformance_perTArray_hh_INCLUDED
#define EppPerformance_perTArray_hh_INCLUDED

//Geof Sawaya, 2014, LBL & DOE

//This class implements an array (with a subset of std::Array type ops).
//It is specially allocated in sub-arrays, each with cache line boundary
//alignment.  It is statically allocated at creation time.

#include <cstddef>
#include <iterator>
#include <vector>
#include <iostream>

#include <alloc.hh>


namespace EppPerformance{

using std::vector;

class genPerTArray{
public:
  virtual ~genPerTArray();
  genPerTArray( genPerTArray&& a): optimized(a.optimized){}
  genPerTArray(){}
  bool isOptimized();
  virtual void optimize(std::vector<size_t>&);
protected:
  bool optimized = false;
};

template <typename T>
class ptaIterator;

template<typename T>
class perTArray: public genPerTArray{
public:

  perTArray(){
    data_ = nullptr;
  }

  perTArray( perTArray&& a): 
		threads(a.threads), 
		el_size(a.el_size),
		length(a.length), 
		sizes(a.sizes),
		firstIndex(a.firstIndex),
		genPerTArray(std::move(a))
  {
    data_ = a.data_;
    a.data_ = nullptr;
  }

  perTArray &
  operator =( perTArray const & a){
    if(this != &a){
      threads = a.threads;
      el_size = a.el_size;
      sizes = a.sizes;
      firstIndex = a.firstIndex;
      optimized = a.optimized;
      doAllocation();
      std::copy(begin(), end(), const_cast<perTArray&>(a).begin());
    }
		return *this;
  }

  //NOTE threads * size must always be less or equal to the actual number
  //of elements used in the optimized version
  //(This constructs an unoptimized version, for when the optimization
  //info is unavailable at the time of construction)
  perTArray(int threads, size_t size):
		threads(threads),
		el_size(size)
	{
    AlignedAlloc<T> a(Utility::getL1CacheLineSize());
    data_ = new T*[threads];
    sizes = vector<size_t>(threads, size);
    doAllocation();
    setFirstIndex();
  }
  
  perTArray(int threads, const vector<size_t>& sizes):
		threads(threads),
    el_size(0), 
		sizes(sizes)
  {
    doAllocation();
    setFirstIndex();   
    optimized = true;
  }
  
  ~perTArray(){
    if(data_ != nullptr){
      for(int x = 0; x < threads; ++x){
	free(data_[x]);
      }
      delete[] data_;
      data_ = nullptr;
    }
  }

  //when the owner of this container has developed a load balancing strategy in 
  //the form of a 'sizes' vector, we can reshape the container to follow suit
  void
  optimize(vector<size_t>& sizes){
    auto temp = new T*[threads];
    AlignedAlloc<T> a(Utility::getL1CacheLineSize());
    size_t newLength = 0;
    for(int x = 0; x < threads; ++x){
      temp[x] = a.allocate(sizes[x]);
      newLength += sizes[x];
      // std::copy(temp[x], temp[x] + sizes[x], data_[x]);
      // free(data_[x]);
    }
    //ya, could have done this with a copy constructor
    //but would still have to deal with the different shaped data
    copyData(temp, sizes);
    this->sizes = sizes;
    length = newLength;
    for(int x = 0; x < threads; ++x) free(data_[x]);
    delete[] data_;
    data_ = temp;
    setFirstIndex();
    optimized = true;
  }

  // void dumpDataAddrs(){
  //   for(int x = 0; x < threads; ++x){
  //     for(int y = 0; y < sizes[x]; ++y){
  // 	std::cout << "\t" << &data_[x][y] << std::endl;
  //     }
  //     std::cout << std::endl;
  //   }
  //   std::cout << &data_ << std::endl;
  // }
private:
  friend class ptaIterator<T>;
  T** data_ = nullptr;
  int threads;
  size_t el_size;
  size_t length;
  vector<size_t> sizes;
  vector<size_t> firstIndex;
  typedef T value_type;

  void 
  copyData(T** newD, vector<size_t> sizes){
    int i = 0;
    for(int x = 0; x < threads; ++x){
      for(int y = 0; y < sizes[x]; ++y){ 
	if(i < length){
	  newD[x][y] = this->operator[](i++);
	}else{
	  return;
	}
      }
    }
  }

  void
  doAllocation(){
    length = 0;
    AlignedAlloc<T> a(Utility::getL1CacheLineSize());
    data_ = new T*[threads];
    for(int x = 0; x < threads; ++x){
      data_[x] = a.allocate(sizes[x]);
      length += sizes[x];
    }
  }

  struct tiPair {
    tiPair(){tid = 0; index = 0;}
    tiPair(const tiPair& tip):tid(tip.tid), index(tip.index){}
    int tid;
    int index;
    bool operator==(const tiPair& rhs){
      return tid == rhs.tid && index == rhs.index;}    
    void operator=(const tiPair& rhs){
      tid = rhs.tid;
      index = rhs.index;
    }
  };

  tiPair
  translateIndex(int index){
    tiPair retVal;
    retVal.tid = lookupTid(index);
    retVal.index = index - firstIndex[retVal.tid];
    return retVal;
  }  

  size_t 
  translateToIndex(const tiPair& p){
    size_t retVal = 0;
    for(int i =0; i < p.tid; ++i){
      retVal += sizes[i];
    }
    retVal += p.index;
		return retVal;
  }

  int
  lookupTid(int index){
    if(el_size != 0){
      return index / el_size;
    }else{
      for(int x = 1; x < firstIndex.size(); ++x){
				if(firstIndex[x] > index){
					return x - 1;
				}
      }
      return firstIndex.size() - 1;
    }
  }

  void
  setFirstIndex(){
    firstIndex.push_back(0);
    int accum = 0;
    for(auto s: sizes){
      accum += s;
      firstIndex.push_back(accum);
    }
  }
	
public:
  bool isOptimized(){ return optimized;}
  inline
  T& operator[](tiPair tip){return data_[tip.tid][tip.index];}
  inline
  ptaIterator<T> begin(){return ptaIterator<T>(*this);}
  // inline
  // ptaIterator<T> cbegin() const {return ptaIterator<T>(*this);}
  inline
  ptaIterator<T> end(){ptaIterator<T> tmp(*this); tmp.pos.index = 0; 
    tmp.pos.tid = threads; return tmp;}
  ptaIterator<T> rand(int index){ptaIterator<T> tmp(*this); 
    tmp.pos = translateIndex(index); return tmp;}
  inline
  T& operator[](int index){
    tiPair p = translateIndex(index);
    return data_[p.tid][p.index];
  }
  inline
  size_t size(){return length;}
  inline void
  clear(T val){
    for(auto& i : *this){
      i = val;
    }
  }
  inline void
  clearRange(int beg, int end, T val){
    for(auto it = rand(beg); it != rand(end); ++it){
      *it = val;
    }
  }
  // inline void
  // operator=(T value)
};

template <typename T>
class ptaIterator : public std::iterator<std::random_access_iterator_tag, perTArray<T>>
{
 private:
  friend class perTArray<T>;
  typedef perTArray<T> container;
  typedef typename perTArray<T>::tiPair tip;
  
  container& array;
  tip  pos; 

public:
  typedef T value_type;
  ptaIterator(container& a): array(a), pos(){}
  ptaIterator(container& a, tip pos): array(a), pos(pos){}
  ptaIterator(container& a, int index): array(a), pos(a.translateIndex(index)){}
  ptaIterator(const ptaIterator& ptai) : array(ptai.array), pos(ptai.pos){}
  ~ptaIterator(){}

  ptaIterator&
  operator[](const int& index){
    this.pos = array.translateIndex(index);
    return *this;
  }

  ptaIterator&
  operator++(){
    if(pos.index + 1 >= array.sizes[pos.tid]){
      pos.index = 0; ++pos.tid;
    }else{
      ++pos.index;
    }
    return *this;
  }

  ptaIterator&
  operator--(){
    if(pos.index == 0){
      --pos.tid; 
      pos.index = array.sizes[pos.tid] - 1;
    }else{
      --pos.index;
    }
    return *this;
  }

  ptrdiff_t
  operator-(const ptaIterator& rhs){
    return array.translateToIndex(pos) - rhs.array.translateToIndex(rhs.pos);
  }

  bool operator==(const ptaIterator& rhs){return pos == rhs.pos;}
  bool operator!=(const ptaIterator& rhs){return !(pos == rhs.pos);}
  value_type& operator*(){return array[pos];}
};

}

#endif
