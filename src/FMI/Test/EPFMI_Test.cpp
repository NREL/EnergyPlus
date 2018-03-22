#include <gtest/gtest.h>
#include "../EPFMI.hpp"

TEST( EPFMI, Alpha ) {
  const char * input = "/Users/kbenne/Development/EnergyPlus/testfiles/RefBldgSmallOfficeNew2004_Chicago.idf";
  const char * weather = "/Users/kbenne/Development/EnergyPlus/weather/USA_IL_Chicago-OHare_TMY2.epw";
  const char * idd = "/Users/kbenne/Development/EnergyPlus/build/Products/Energy+.idd";

  int result = instantiate(input, // input
                           weather, // weather
                           idd, // idd
                           "Alpha", // instanceName
                           nullptr, // varNames
                           nullptr, // varPointers
                           0, // nVars
                           nullptr); // log

  double tStart = 0.0;
  bool stopTimeDefined = true;
  double tEnd = 86400;

  result = setupExperiment(tStart, stopTimeDefined, tEnd, nullptr);

  fmiEventInfo eventInfo;
  double time = tStart;

  double outputs[] = {0.0};
  const unsigned int outputRefs[] = {11};

  while ( time < tEnd ) {
    result = getNextEventTime(&eventInfo, nullptr);
    std::cout << "Current time: " << time << std::endl;
    std::cout << "Next event time: " << eventInfo.nextEventTime << std::endl;

    result = getVariables(outputRefs, outputs, 1, nullptr); 
    std::cout << "Output 11: " << outputs[0] << std::endl;

    time = eventInfo.nextEventTime;
    setTime(time, nullptr);
  }

  terminate(nullptr);
};

