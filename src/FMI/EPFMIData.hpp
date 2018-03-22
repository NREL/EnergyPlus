#include <thread>
#include <mutex>
#include <condition_variable>

enum class EPStatus { WORKING, IDLE, TERMINATING };

struct FMUTimeInfo {
  FMUTimeInfo() {
    current = 0.0;
    tStart = 0.0;
    tEnd = 0.0;
    stopTimeDefined = false;
  }

  double current;
  double tStart;
  double tEnd;
  bool stopTimeDefined;
};

extern EPStatus epstatus;
extern FMUTimeInfo fmutimeinfo;
extern std::condition_variable time_cv;
extern std::mutex time_mutex;

