//Geof Sawaya, 2014, LBL & DOE

//implementation of multi-alloc array for false sharing free
//multi-threaded writing (per thread allocation)
#include <iostream>
#include <random>
#include "perTArray.hh"

//namespace EppPerformance{ 
  
  //a little test for the perTArray and ptaIterator classes
using namespace EppPerformance;

genPerTArray::~genPerTArray(){}

bool
genPerTArray::isOptimized(){return optimized;}

void
genPerTArray::optimize(std::vector<size_t>&){}

// int
// main(){
//   vector<size_t> threadRegionSizes = {7,10,4,6,3,12,78,10,13,15};
//   perTArray<int> pta(10, 16);
//   std::default_random_engine rgen;
//   typedef std::uniform_int_distribution<int> rdist;
//   rdist distribution(1,23);
//   auto rand = std::bind(distribution, rgen);
  
//   // std::cout << "Dumping Data Addrs" << std::endl;
//   // pta.dumpDataAddrs();

//   std::cout << "OK, loading the collection . . ." << std::endl;
//   for(auto it = pta.begin(); it != pta.end(); ++it){
//     *it = rand();
//   }

//   // std::cout << "Dumping Data Addrs" << std::endl;
//   // pta.dumpDataAddrs();

//   std::cout << "Dumping the collection . . ." << std::endl;
//   for(auto it= pta.begin(); it != pta.end(); ++it){
//     std::cout << *it << std::endl;
//   }

//   std::cout << "OK, dumping the collection in reverse . . . " << std::endl;
//   auto it = pta.end();
//   for(--it; it != pta.begin(); --it){
//     std::cout << *it << std::endl;
//   }
//   std::cout << *(pta.begin()) << std::endl;
  
//   std::cout << "OK, let's try our random access iterator. We'll go random and verify with " <<
//     "the perTArray's index operator" << std::endl;
//   //assumes pta.size() > 1
//   rdist begrand(0, pta.size()-2); 
//   int beg = begrand(rgen);
//   rdist endrand(beg+1, pta.size()-1);
//   int end = endrand(rgen);
//   std::cout << "Here's the array with the random iterators beg: " << beg << " and end: " << 
//     end << std::endl;
//   for(auto it = pta.rand(beg); it != pta.rand(end); ++it){
//     std::cout << *it << std::endl;
//   }
//   std::cout << "And here's the check, going through indexes:" << std::endl;
//   for(int i = beg; i < end; ++i){
//     std::cout << pta[i] << std::endl;
//   }

//   std::cout << "cool, let's try the 'for (x : collection)' version (for read):" << std::endl;
//   for(auto i : pta){
//     std::cout << i << std::endl;
//   }

//   std::cout << "OK, now let's optimize the collection and see how it looks . . ." << std::endl;
//   pta.optimize(threadRegionSizes);
//   for(auto i : pta){
//     std::cout << i << std::endl;
//   }

//   std::cout << "Great.  Now let's try our new 'clear' function, resetting to '999': " << std::endl;
//   pta.clear(999);
//   for(auto i : pta){
//     std::cout << i << std::endl;
//   }
  
//   std::cout << "OK, now let's try clearRange(44, 66, 123):" << std::endl;
//   pta.clearRange(44, 66, 123);
//   std::cout << "here's 44:66:" << std::endl;
//   for(auto it = pta.rand(44); it != pta.rand(66); ++it){
//     std::cout << *it << std::endl;
//   }
//   std::cout << "...and full range: " << std::endl;
//   for(auto i: pta){
//     std::cout << i << std::endl;
//   }

//   // std::cout << "Dumping Data Addrs" << std::endl;
//   // Ptacv.dumpDataAddrs();

//   std::cout.flush();
//   return 0;
// }


