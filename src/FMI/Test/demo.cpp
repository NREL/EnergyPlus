
#include "../EPFMI.hpp"

int main() {
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
  bool stopTimeDefined = false;
  double tEnd = 0.0;
  
  int result2 = setupExperiment(tStart, stopTimeDefined, tEnd, nullptr);
}

