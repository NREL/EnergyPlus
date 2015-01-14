// This is my C++ implementation of speed timers -- Geof Sawaya, LBL & DOE 2014

#include <ctime>
#include <cstdlib>
#include <map>
#include <cstdio>
#include <string>
#include <iostream>
#include <fstream>
#include <algorithm>

#ifdef CLOCK_MONOTONIC_RAW
#define CLOCK CLOCK_MONOTONIC_RAW
#else
#define CLOCK CLOCK_REALTIME
#endif

namespace EppPerformance {
  using std::map;
  using std::string;
  using std::pair;

  class Timer
  {
  private:
    static const string outFile; 
    static map<string, Timer*> Timers;
    struct timespec _sum;
    struct timespec _start;
    struct timespec _stop;
    long long _hits;
    string _label;
    void zeroTimer();
    void resetTimer();
  public:
    Timer(string label);
    ~Timer();
    static void printTimerData();
    static string getTimerData();
    double getSumElapsedSeconds();
    double getLastElapsedSeconds();
    struct timespec getElapsedTime();
    long long getNumHits();
    inline void
    startTimer(){
      clock_gettime(CLOCK, &_start);
    }
 
    inline void
    stopTimer(){
      clock_gettime(CLOCK, &_stop);
      long nsecs = _stop.tv_nsec - _start.tv_nsec;
      if(nsecs < 0){
	_sum.tv_sec -= 1; //borrow
	_sum.tv_nsec += 1E9 + nsecs;
      }else{
	_sum.tv_nsec += nsecs;
      }
      _sum.tv_sec += _stop.tv_sec - _start.tv_sec;
      ++_hits;
    }
	public:
		class InlineTimer
		{
		public:
			InlineTimer(string label):t(new Timer(label)){
				t->startTimer();
			}
			~InlineTimer(){
				t->stopTimer();
			}
		private:
			Timer *t;
		};
  };

}
