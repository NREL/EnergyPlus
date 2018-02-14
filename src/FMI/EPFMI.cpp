#include <thread>
#include "EPFMI.hpp"
#include "../EnergyPlus/public/EnergyPlusPgm.hh"
#include "../EnergyPlus/CommandLineInterface.hh"

unsigned int instantiate(const char *input,
                         const char *weather,
                         const char *idd,
                         const char *instanceName,
                         const char** varNames,
                         double* varPointers[],
                         size_t nVars,
                         const char *log)
{
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

void run() {
  EnergyPlusPgm();
}

unsigned int setupExperiment(double tStart,
                             bool stopTimeDefined,
                             double tEnd,
                             const char *log)
{
  std::thread t(run);
  t.join();

  return 0;
}

