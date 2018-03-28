#include <gtest/gtest.h>
#include "../EPFMI.hpp"
#include "test-config.hpp"


TEST( EPFMI, Alpha ) {

const char * inputNames[] = {"Attic,T", "Core_ZN,T", "Perimeter_ZN_1,T", "Perimeter_ZN_2,T", "Perimeter_ZN_3,T", "Perimeter_ZN_4,T"};
const unsigned int inputValueReferences[] = {0, 1, 2, 3, 4, 5,};

const char * outputNames[] = {
"Attic,QConSen_flow", "Core_ZN,QConSen_flow", "Perimeter_ZN_1,QConSen_flow", "Perimeter_ZN_2,QConSen_flow", "Perimeter_ZN_3,QConSen_flow", "Perimeter_ZN_4,QConSen_flow",
"Attic,V", "Core_ZN,V", "Perimeter_ZN_1,V", "Perimeter_ZN_2,V", "Perimeter_ZN_3,V", "Perimeter_ZN_4,V",
"Attic,AFlo", "Core_ZN,AFlo", "Perimeter_ZN_1,AFlo", "Perimeter_ZN_2,AFlo", "Perimeter_ZN_3,AFlo", "Perimeter_ZN_4,AFlo",
"Attic,mSenFac", "Core_ZN,mSenFac", "Perimeter_ZN_1,mSenFac", "Perimeter_ZN_2,mSenFac", "Perimeter_ZN_3,mSenFac", "Perimeter_ZN_4,mSenFac"
};
const unsigned int outputValueReferences[] = {
  6, 7, 8, 9, 10, 11,
  12, 13, 14, 15, 16, 17,
  18, 19, 20, 21, 22, 23,
  24, 25, 26, 27, 28, 29
};

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
                           24, // nOut
                           nullptr); //log);

  double tStart = 0.0;
  bool stopTimeDefined = true;
  double tEnd = 86400;

  result = setupExperiment(tStart, stopTimeDefined, nullptr);

  fmi2EventInfo eventInfo;
  double time = tStart;

  double outputs[] = {0.0, 0.0, 0.0, 0.0};
  const unsigned int outputRefs[] = {6, 12, 18, 24};

  double inputs[] = {21.0, 21.0};
  const unsigned int inputRefs[] = {0, 1};

  while ( time < tEnd ) {
    result = getNextEventTime(&eventInfo, nullptr);
    std::cout << "Current time: " << time << std::endl;
    std::cout << "Next event time: " << eventInfo.nextEventTime << std::endl;

    result = setVariables(inputRefs, inputs, 2, nullptr);

    result = getVariables(outputRefs, outputs, 4, nullptr); 
    std::cout << "Output 6 - Attic QConSen_flow: " << outputs[0] << std::endl;
    std::cout << "Output 12 - Attic Volume: " << outputs[1] << std::endl;
    std::cout << "Output 18 - Attic AFlo: " << outputs[2] << std::endl;
    std::cout << "Output 24 - Attic mSenFac: " << outputs[3] << std::endl;

    time = eventInfo.nextEventTime;
    setTime(time, nullptr);
  }

  terminate(nullptr);
};

