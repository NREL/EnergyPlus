#include "EPFMI.hpp"
#include "EPFMIData.hpp"
#include "../EnergyPlus/public/EnergyPlusPgm.hh"
#include "../EnergyPlus/CommandLineInterface.hh"
#include "../EnergyPlus/ZoneTempPredictorCorrector.hh"
#include "../EnergyPlus/DataGlobals.hh"
#include "../EnergyPlus/DataHeatBalFanSys.hh"
#include "../EnergyPlus/DataHeatBalance.hh"
#include <functional>
#include <map>
#include <memory>
#include <string>
#include <sstream>
#include <algorithm>

using namespace std::placeholders;

#define UNUSED(expr) do { (void)(expr); } while (0);

std::map<unsigned int, std::function<void(double*)> > valueGetters;
std::map<unsigned int, std::function<void(const double*)> > valueSetters;
std::thread * epthread;

const char ** g_parameterNames;
const unsigned int * g_parameterValueReferences;
size_t g_nPar;
const char ** g_inputNames;
const unsigned int * g_inputValueReferences;
size_t g_nInp;
const char ** g_outputNames;
const unsigned int * g_outputValueReferences;
size_t g_nOut;

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

// Pair of strings, first is a zone name, second is a variable name
typedef std::pair<std::string, std::string> NamePair;

struct VarInfo {
  std::string zoneName;
  int zoneNum;
  int varValueRef;
  std::string varName;
};

int zoneIndex(const std::string & zoneName) {
  std::string name(zoneName);
  std::transform(name.begin(), name.end(),name.begin(), ::toupper);
  for ( int i = 0; i < EnergyPlus::DataGlobals::NumOfZones; ++i ) {
    if ( EnergyPlus::DataHeatBalance::Zone[i].Name == name ) {
      return i + 1;
    }
  }

  return 0;
}

// Split a char * with zone name and variable name, 
// separated by a ","
VarInfo createVarInfo(const char * name, int varValueRef) {
  NamePair pair;

  std::vector<std::string> strings;
  std::istringstream f(name);
  std::string s;    
  while (std::getline(f, s, ',')) {
      strings.push_back(s);
  }

  VarInfo info;
  info.zoneName = strings[0];
  info.varName = strings[1];
  info.zoneNum = zoneIndex(info.zoneName);
  info.varValueRef = varValueRef;

  return info;
}

unsigned int instantiate(const char *input,
                         const char *weather,
                         const char *idd,
                         const char *instanceName,
                         const char ** parameterNames,
                         const unsigned int parameterValueReferences[],
                         size_t nPar,
                         const char ** inputNames,
                         const unsigned int inputValueReferences[],
                         size_t nInp,
                         const char ** outputNames,
                         const unsigned int outputValueReferences[],
                         size_t nOut,
                         const char *log)
{
  UNUSED(instanceName);
  UNUSED(log);

  g_parameterNames = parameterNames;
  g_parameterValueReferences = parameterValueReferences;
  g_nPar = nPar;
  g_inputNames = inputNames;
  g_inputValueReferences = inputValueReferences;
  g_nInp = nInp;
  g_outputNames = outputNames;
  g_outputValueReferences = outputValueReferences;
  g_nOut = nOut;

  for ( size_t i = 0; i < nPar; ++i ) {
  }

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
                             const char *log)
{
  UNUSED(log)

  fmutimeinfo.stopTimeDefined = stopTimeDefined;
  fmutimeinfo.tStart = tStart;

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

  for ( size_t i = 0; i < g_nInp; ++i ) {
    const char * inputNamePair = g_inputNames[i];
    const unsigned int inputValueRef = g_inputValueReferences[i];

    auto varInfo = createVarInfo(inputNamePair, inputValueRef);

    if ( varInfo.varName == "T" ) {
      valueSetters[varInfo.varValueRef] = std::bind(setZoneTemperature, _1, varInfo.zoneNum);
    }

  }

  for ( size_t i = 0; i < g_nOut; ++i ) {
    const char * outputNamePair = g_outputNames[i];
    const unsigned int outputValueRef = g_outputValueReferences[i];

    auto varInfo = createVarInfo(outputNamePair, outputValueRef);

    if ( varInfo.varName == "QConSen_flow" ) {
      valueGetters[varInfo.varValueRef] = std::bind(getZoneH, _1, varInfo.zoneNum);
    }

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



