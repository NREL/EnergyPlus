#include "EPFMI.hpp"
#include "EPFMIData.hpp"
#include "../EnergyPlus/public/EnergyPlusPgm.hh"
#include "../EnergyPlus/CommandLineInterface.hh"
#include "../EnergyPlus/ZoneTempPredictorCorrector.hh"

#define UNUSED(expr) do { (void)(expr); } while (0);

unsigned int instantiate(const char *input,
                         const char *weather,
                         const char *idd,
                         const char *instanceName,
                         const char** varNames,
                         double* varPointers[],
                         size_t nVars,
                         const char *log)
{
  UNUSED(instanceName);
  UNUSED(varNames);
  UNUSED(varPointers);
  UNUSED(nVars);
  UNUSED(log);

  const int argc = 6;
  const char * argv[argc];
  argv[0] = "energyplus";
  argv[1] = "-w";
  argv[2] = weather;
  argv[3] = "-i";
  argv[4] = idd;
  argv[5] = input;

	EnergyPlus::CommandLineInterface::ProcessArgs( argc, argv );

  return 0;
}

unsigned int setupExperiment(double tStart,
                             bool stopTimeDefined,
                             double tEnd,
                             const char *log)
{
  UNUSED(log)

  timeinfo.stopTimeDefined = stopTimeDefined;
  timeinfo.tStart = tStart;
  timeinfo.tEnd = tEnd;

  std::thread t(EnergyPlusPgm, "");
  t.join();

  return 0;
}

unsigned int setTime(double time,
                     const char *log)
{
  UNUSED(log)

  {
    std::unique_lock<std::mutex> lk(time_mutex);
    timeinfo.current = time;
    epstatus = EPStatus::WORKING;
  }
  // Notify E+ to advance
  time_cv.notify_one();
  {
    // Wait for E+ to advance and go back to IDLE before returning
    std::unique_lock<std::mutex> lk( time_mutex );
    time_cv.wait( lk, [](){ return epstatus == EPStatus::IDLE; } );
  }

  return 0;
}

unsigned int getTimeDerivatives(const unsigned int valueReferences[],
                                double * const variablePointers[],
                                size_t nVars5,
                                const char *log)
{
  UNUSED(valueReferences)
  UNUSED(log)

  // Get ZoneNum
  for ( size_t i = 0; i < nVars5; ++i ) {
    int variableIndex = 0;
    UNUSED(variableIndex)
    int ZoneNum = 0;
    UNUSED(ZoneNum)
    Real64 hdot = EnergyPlus::ZoneTempPredictorCorrector::HDot(ZoneNum);
    *variablePointers[i] = hdot;
  }

  return 0;
}


