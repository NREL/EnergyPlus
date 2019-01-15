#include "EPFMI.hpp"
#include "EPFMIData.hpp"
#include "../EnergyPlus/public/EnergyPlusPgm.hh"
#include "../EnergyPlus/CommandLineInterface.hh"
#include "../EnergyPlus/ZoneTempPredictorCorrector.hh"
#include "../EnergyPlus/DataGlobals.hh"
#include "../EnergyPlus/DataHeatBalFanSys.hh"
#include "../EnergyPlus/DataHeatBalance.hh"
#include "../EnergyPlus/DataEnvironment.hh"
#include <functional>
#include <map>
#include <memory>
#include <string>
#include <sstream>
#include <algorithm>

using namespace std::placeholders;

#define UNUSED(expr) do { (void)(expr); } while (0);

struct Variable {
  Variable(const char * name, int ref)
    : valueRef(ref)
  {
    std::vector<std::string> strings;
    std::istringstream f(name);
    std::string s;    
    while (std::getline(f, s, ',')) {
        strings.push_back(s);
    }

    zoneName = strings[0];
    varName = strings[1];
  };

  int zoneNum() const {
    std::string name(zoneName);
    std::transform(name.begin(), name.end(),name.begin(), ::toupper);
    for ( int i = 0; i < EnergyPlus::DataGlobals::NumOfZones; ++i ) {
      if ( EnergyPlus::DataHeatBalance::Zone[i].Name == name ) {
        return i + 1;
      }
    }
  
    return 0;
  };

  std::string zoneName;
  int valueRef;
  std::string varName;
};

std::map<unsigned int, std::function<void(double*)> > valueGetters;
std::map<unsigned int, std::function<void(const double*)> > valueSetters;
std::thread * epthread;

std::vector<Variable> parameters;
std::vector<Variable> inputs;
std::vector<Variable> outputs;

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
  EnergyPlus::DataHeatBalFanSys::MAT( zoneNum ) = *temp;
}

void getZoneH(double* h, int zoneNum) {
  *h = EnergyPlus::ZoneTempPredictorCorrector::HDot(zoneNum);
}

void getZoneVolume(double* volume, int zoneNum) {
  *volume = EnergyPlus::DataHeatBalance::Zone( zoneNum ).Volume;
}

void getZoneFloorArea(double* area, int zoneNum) {
  *area = EnergyPlus::DataHeatBalance::Zone( zoneNum ).FloorArea;
}

void getZoneCapacityMult(double* mult, int zoneNum) {
  *mult = EnergyPlus::DataHeatBalance::Zone( zoneNum ).ZoneVolCapMultpSens;
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

  for ( size_t i = 0; i < nPar; ++i ) {
    parameters.emplace_back(parameterNames[i], parameterValueReferences[i]);
  }

  for ( size_t i = 0; i < nInp; ++i ) {
    inputs.emplace_back(inputNames[i], inputValueReferences[i]);
  }

  for ( size_t i = 0; i < nOut; ++i ) {
    outputs.emplace_back(outputNames[i], outputValueReferences[i]);
  }

  const int argc = 8;
  const char * argv[argc];
  argv[0] = "energyplus";
  argv[1] = "-d";
  argv[2] = instanceName;
  argv[3] = "-w";
  argv[4] = weather;
  argv[5] = "-i";
  argv[6] = idd;
  argv[7] = input;

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

  for ( const auto & param : parameters ) {
    if ( param.varName == "V" ) {
      valueGetters[param.valueRef] = std::bind(getZoneVolume, _1, param.zoneNum());
    } else if ( param.varName == "AFlo" ) {
      valueGetters[param.valueRef] = std::bind(getZoneFloorArea, _1, param.zoneNum());
    } else if ( param.varName == "mSenFac" ) {
      valueGetters[param.valueRef] = std::bind(getZoneCapacityMult, _1, param.zoneNum());
    } else {
      std::cout << "parameter named " << param.varName << " is not valid" << std::endl;
    }
  }

  for ( const auto & input : inputs ) {
    if ( input.varName == "T" ) {
      valueSetters[input.valueRef] = std::bind(setZoneTemperature, _1, input.zoneNum());
    } else {
      std::cout << "input named " << input.varName << " is not valid" << std::endl;
    }
  }

  for ( const auto & output : outputs ) {
    if ( output.varName == "QConSen_flow" ) {
      valueGetters[output.valueRef] = std::bind(getZoneH, _1, output.zoneNum());
    } else {
      std::cout << "output named " << output.varName << " is not valid" << std::endl;
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
  std::cout << "begin getVariables" << std::endl;
  //for ( const auto & var : valueReferences ) {
  //  std::cout << var << std::endl;
  //}

  for ( size_t i = 0; i < nVars2; ++i ) {
    auto valueRef = valueReferences[i];
    auto variablePointer = &variablePointers[i];
    auto getter = valueGetters[valueRef];

    getter(variablePointer);
  }
  std::cout << "end getVariables" << std::endl;

  return 0;
}

unsigned int getNextEventTime(fmi2EventInfo *eventInfo,
                              const char *log)
{
  UNUSED(log);

  eventInfo->nextEventTime = EnergyPlus::DataGlobals::NextSimTime;
  eventInfo->nextEventTimeDefined = fmi2True;

  return 0;
}

unsigned int terminateSim(const char *log) {
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



