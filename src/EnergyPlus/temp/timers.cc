#include "timers.hh"
#include <sstream>

namespace EppPerformance{

  Timer::Timer(string label):_label(label)
  {
    resetTimer();
    Timers.insert(pair<string, Timer*>(label, this));
  }

  Timer::~Timer()
  {
    Timers.erase(_label);
  }

  map<string, Timer*> Timer::Timers;
  
  void
  Timer::zeroTimer()
  {
    _sum.tv_sec = 0;
    _sum.tv_nsec = 0;
    _start.tv_sec = 0;
    _start.tv_nsec = 0;
    _stop.tv_sec = 0;
    _stop.tv_nsec = 0;
  }

  void
  Timer::resetTimer()
  {
    zeroTimer();
    _hits = 0;
  }

  double
  Timer::getSumElapsedSeconds()
  {
    return _sum.tv_sec + _sum.tv_nsec / 1.0E9;
  }

  double
  Timer::getLastElapsedSeconds()
  {
    struct timespec ts = getElapsedTime();
    return ts.tv_sec + ts.tv_nsec / 1.0E9;
  }

  struct timespec
  Timer::getElapsedTime()
  {
    long nsecs = _stop.tv_nsec - _start.tv_nsec;
    struct timespec retval;
    if(nsecs < 0){
      retval.tv_sec = -1;/*borrow*/
      retval.tv_nsec = 1E9 + nsecs;
    }else{
      retval.tv_sec = 0;
      retval.tv_nsec = nsecs;
    }
    retval.tv_sec += _stop.tv_sec - _start.tv_sec;
    return retval;   
  }

  long long
  Timer::getNumHits()
  {
    return _hits;
  }

  const string Timer::outFile("speedup_timers.txt");

	string
	Timer::getTimerData()
	{
		std::stringstream results;
    for (auto t: Timers){
		     results << t.second->_label << ",\t" << t.second->getSumElapsedSeconds() 
			     << " seconds" << std::endl;
		     results << "\t" << "Hits on timer: " << t.second->_hits << std::endl;
    }		
		return results.str();
	}

  void
  Timer::printTimerData()
  {
    std::ofstream thefile;
    thefile.open(outFile);
		thefile << getTimerData();
    thefile.close();
  }
}
