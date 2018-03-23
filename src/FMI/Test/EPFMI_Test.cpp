#include <gtest/gtest.h>
#include "../EPFMI.hpp"
#include "test-config.hpp"


TEST( EPFMI, Alpha ) {

const char * inputNames[] = {"Attic,T", "Core_ZN,T", "Perimeter_ZN_1,T", "Perimeter_ZN_2,T", "Perimeter_ZN_3,T", "Perimeter_ZN_4,T"};
const unsigned int inputValueReferences[] = {0, 1, 2, 3, 4, 5,};

const char * outputNames[] = {"Attic,QConSen_flow", "Core_ZN,QConSen_flow", "Perimeter_ZN_1,QConSen_flow", "Perimeter_ZN_2,QConSen_flow", "Perimeter_ZN_3,QConSen_flow", "Perimeter_ZN_4,QConSen_flow"};
const unsigned int outputValueReferences[] = {6, 7, 8, 9, 10, 11};

  int result = instantiate(input, // input
                           weather, // weather
                           idd, // idd
                           "Alpha", // instanceName
                           nullptr, // parameterNames
                           nullptr, // parameterValueReferences[]
                           0, // nPar
                           inputNames, // inputNames
                           inputValueReferences, // inputValueReferences[]
                           6, // nInp
                           outputNames, // outputNames
                           outputValueReferences, // outputValueReferences[]
                           6, // nOut
                           nullptr); //log);

  double tStart = 0.0;
  bool stopTimeDefined = true;
  double tEnd = 86400;

  result = setupExperiment(tStart, stopTimeDefined, tEnd, nullptr);

  fmiEventInfo eventInfo;
  double time = tStart;

  double outputs[] = {0.0, 0.0};
  const unsigned int outputRefs[] = {6, 7};

  double inputs[] = {21.0, 21.0};
  const unsigned int inputRefs[] = {0, 1};

  while ( time < tEnd ) {
    result = getNextEventTime(&eventInfo, nullptr);
    std::cout << "Current time: " << time << std::endl;
    std::cout << "Next event time: " << eventInfo.nextEventTime << std::endl;

    result = setVariables(inputRefs, inputs, 2, nullptr);

    result = getVariables(outputRefs, outputs, 2, nullptr); 
    std::cout << "Output 6 - Attic QConSen_flow: " << outputs[0] << std::endl;
    std::cout << "Output 7 - Core_ZN QConSen_flow: " << outputs[1] << std::endl;

    time = eventInfo.nextEventTime;
    setTime(time, nullptr);
  }

  terminate(nullptr);
};

