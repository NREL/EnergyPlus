#include <cstddef>
#include <fmi2FunctionTypes.h>

//typedef struct {
//  fmi2Boolean newDiscreteStatesNeeded;
//  fmi2Boolean terminateSimulation;
//  fmi2Boolean nominalsOfContinuousStatesChanged;
//  fmi2Boolean valuesOfContinuousStatesChanged;
//  fmi2Boolean nextEventTimeDefined;
//  fmi2Real nextEventTime; // next event if nextEventTimeDefined=fmi2True
//} fmi2EventInfo;

extern "C" {

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
                         const char *log);

unsigned int setupExperiment(double tStart,
                             bool stopTimeDefined,
                             const char *log);

unsigned int setTime(double time,
                     const char *log);

unsigned int setVariables(const unsigned int valueReferences[],
                          const double variablePointers[],
                          size_t nVars1,
                          const char *log);

unsigned int getVariables(const unsigned int valueReferences[],
                          double variablePointers[],
                          size_t nVars2,
                          const char *log);

unsigned int getNextEventTime(fmi2EventInfo *eventInfo,
                              const char *log);

unsigned int terminateSim(const char *log);

}


