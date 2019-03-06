#include "FMIDataGlobals.hh"

FMUTimeInfo fmutimeinfo;
std::condition_variable time_cv;
std::mutex time_mutex;
EPStatus epstatus = EPStatus::IDLE;

