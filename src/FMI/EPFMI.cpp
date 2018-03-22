#include "EPFMI.hpp"
#include "EPFMIData.hpp"
#include "../EnergyPlus/public/EnergyPlusPgm.hh"
#include "../EnergyPlus/CommandLineInterface.hh"
#include "../EnergyPlus/ZoneTempPredictorCorrector.hh"
#include "../EnergyPlus/DataGlobals.hh"
#include "../EnergyPlus/DataHeatBalFanSys.hh"
#include <functional>
#include <map>
#include <memory>

using namespace std::placeholders;

#define UNUSED(expr) do { (void)(expr); } while (0);

std::map<unsigned int, std::function<void(double*)> > valueGetters;
std::map<unsigned int, std::function<void(const double*)> > valueSetters;
std::thread * epthread;

void noGetter(double*) {
  // This Getter is not implemented
  std::cout << "placeholder getter" << std::endl;
}

void noSetter(const double*) {
  // This Setter is not implemented
  std::cout << "placeholder setter" << std::endl;
}

void getZoneTemperature(double* temp, int zoneNum) {
  *temp = EnergyPlus::DataHeatBalFanSys::ZT( zoneNum );
}

void setZoneTemperature(const double* temp, int zoneNum) {
  EnergyPlus::DataHeatBalFanSys::ZT( zoneNum ) = *temp;
}

void getZoneH(double* h, int zoneNum) {
  *h = EnergyPlus::ZoneTempPredictorCorrector::HDot(zoneNum);
}

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

  fmutimeinfo.stopTimeDefined = stopTimeDefined;
  fmutimeinfo.tStart = tStart;
  fmutimeinfo.tEnd = tEnd;

  {
    std::unique_lock<std::mutex> lk(time_mutex);
    fmutimeinfo.current = 0.0;
    epstatus = EPStatus::WORKING;
  }

  epthread = new std::thread(EnergyPlusPgm, "");

  {
    // Wait for E+ to go back to IDLE
    std::unique_lock<std::mutex> lk( time_mutex );
    time_cv.wait( lk, [](){ return epstatus == EPStatus::IDLE; } );
  }

  // For the purpose of demonstration, assume a one zone model with the following valueReferences
  // 10 = Temperature, T, of zone
  // 11 = Enthalpy, H of zone 

  int varsPerZone = 2;
  for ( int z = 0; z < EnergyPlus::DataGlobals::NumOfZones; ++z ) {
    unsigned int zz = varsPerZone * z;
    unsigned int valueReference;
    unsigned int znumber = z + 1;

    // 10 = Zone Temperature
    valueReference = 10;
    valueGetters[zz + valueReference] = std::bind(getZoneTemperature, _1, znumber);
    valueSetters[zz + valueReference] = std::bind(setZoneTemperature, _1, znumber);

    // 11 = Zone Enthalpy, H
    valueReference = 11;
    valueGetters[zz + valueReference] = std::bind(getZoneH, _1, znumber);
    valueSetters[zz + valueReference] = noSetter;
  }

  return 0;
}

unsigned int setTime(double time,
                     const char *log)
{
  UNUSED(log)

  {
    std::unique_lock<std::mutex> lk(time_mutex);
    fmutimeinfo.current = time;
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

unsigned int setVariables(const unsigned int valueReferences[],
                          const double variablePointers[],
                          size_t nVars1,
                          const char *log)
{
  UNUSED(log);

  for ( size_t i = 0; i < nVars1; ++i ) {
    auto valueRef = valueReferences[i];
    auto variablePointer = &variablePointers[i];
    auto setter = valueSetters[valueRef];

    setter(variablePointer);
  }

  return 0;
}

unsigned int getVariables(const unsigned int valueReferences[],
                          double variablePointers[],
                          size_t nVars2,
                          const char *log)
{
  UNUSED(log);

  for ( size_t i = 0; i < nVars2; ++i ) {
    auto valueRef = valueReferences[i];
    auto variablePointer = &variablePointers[i];
    auto getter = valueGetters[valueRef];

    getter(variablePointer);
  }

  return 0;
}

unsigned int getNextEventTime(fmiEventInfo *eventInfo,
                              const char *log)
{
  UNUSED(log);

  eventInfo->nextEventTime = EnergyPlus::DataGlobals::NextSimTime;
  eventInfo->nextEventTimeDefined = fmiTrue;

  return 0;
}

unsigned int terminate(const char *log) {
  UNUSED(log);

  {
    std::unique_lock<std::mutex> lk(time_mutex);
    epstatus = EPStatus::TERMINATING;
  }
  // Notify E+ to advance
  time_cv.notify_one();

  epthread->join();
  delete epthread;

  return 0;
}



