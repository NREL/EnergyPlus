#include <gtest/gtest.h>
#include "../EPFMI.hpp"
#include "test-config.h"
#include "../EnergyPlus/DataEnvironment.hh"


TEST( EPFMI, static_lib ) {

	const char * inputNames[] = {"Attic,T", "Core_ZN,T", "Perimeter_ZN_1,T", "Perimeter_ZN_2,T", "Perimeter_ZN_3,T", "Perimeter_ZN_4,T"};
	const unsigned int inputValueReferences[] = {0, 1, 2, 3, 4, 5};

	const char * outputNames[] = {
	"Attic,QConSen_flow", "Core_ZN,QConSen_flow", "Perimeter_ZN_1,QConSen_flow", "Perimeter_ZN_2,QConSen_flow", "Perimeter_ZN_3,QConSen_flow", "Perimeter_ZN_4,QConSen_flow",
	};
	const unsigned int outputValueReferences[] = {
	  6, 7, 8, 9, 10, 11
	};

	const char * parNames[] = {
	"Attic,V", "Core_ZN,V", "Perimeter_ZN_1,V", "Perimeter_ZN_2,V", "Perimeter_ZN_3,V", "Perimeter_ZN_4,V",
	"Attic,AFlo", "Core_ZN,AFlo", "Perimeter_ZN_1,AFlo", "Perimeter_ZN_2,AFlo", "Perimeter_ZN_3,AFlo", "Perimeter_ZN_4,AFlo",
	"Attic,mSenFac", "Core_ZN,mSenFac", "Perimeter_ZN_1,mSenFac", "Perimeter_ZN_2,mSenFac", "Perimeter_ZN_3,mSenFac", "Perimeter_ZN_4,mSenFac"
	};
	const unsigned int parValueReferences[] = {
	  12, 13, 14, 15, 16, 17,
	  18, 19, 20, 21, 22, 23,
	  24, 25, 26, 27, 28, 29
	};

  int result = instantiate(input, // input
                           weather, // weather
                           idd, // idd
                           "Alpha", // instanceName
                           parNames, // parameterNames
                           parValueReferences, // parameterValueReferences[]
                           18, // nPar
                           inputNames, // inputNames
                           inputValueReferences, // inputValueReferences[]
                           6, // nInp
                           outputNames, // outputNames
                           outputValueReferences, // outputValueReferences[]
                           6, // nOut
                           nullptr); //log);

  double tStart = 0.0;
  //int stopTimeDefined = 1;
  double tEnd = 86400;
  double outputs[] = {0.0};
  const unsigned int outputRefs[] = {6};
  double inputs[] = {21.0, 21.0,21.0, 21.0, 21.0, 21.0};
  const unsigned int inputRefs[] = {0, 1, 2, 3, 4, 5};
  const unsigned int paramRefs[] = {12};
  double params[1];

  fmi2EventInfo eventInfo;

  result = setupExperiment(tStart, 1, NULL);

  getVariables(paramRefs, params, 1, nullptr);
  double atticVolume = params[0]; // m^3
  //double atticVolume = 20; // m^3
  double atticTemp = 21.0;

  double time = tStart;

  while ( time < tEnd ) {
    result = getNextEventTime(&eventInfo, nullptr);

    double lastTime = time;
    time = eventInfo.nextEventTime;
    setTime(time, nullptr);
    double dt = time - lastTime;

    inputs[0] = atticTemp;
    result = setVariables(inputRefs, inputs, 6, nullptr);

    // update atticTemp
    
    result = getVariables(outputRefs, outputs, 1, nullptr); 
    double atticQFlow = outputs[0]; // J/s 
    double densityAir = 1.276; // kg/m^3
    double heatCapacity = 1000.6; // J/kgK
    double tempDot = atticQFlow / ( atticVolume * densityAir * heatCapacity );

    atticTemp = atticTemp + (dt * tempDot);

    std::cout << "Current time: " << time << std::endl;
	  std::cout << "OutDryBulbTemp: " << EnergyPlus::DataEnvironment::OutDryBulbTemp << std::endl;
    std::cout << "Attic Temp is: " << atticTemp << std::endl;
  }

  terminateSim(nullptr);
}

