#include <cstddef>
#include <fmiModelTypes.h>
//#include <fmi1_types.h>
//#include <fmi1_functions.h>
//#include <jmi_types.h>

typedef struct {
  fmiBoolean newDiscreteStatesNeeded;
  fmiBoolean terminateSimulation;
  fmiBoolean nominalsOfContinuousStatesChanged;
  fmiBoolean valuesOfContinuousStatesChanged;
  fmiBoolean nextEventTimeDefined;
  fmiReal nextEventTime; // next event if nextEventTimeDefined=fmi2True
} fmiEventInfo;

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
                             double tEnd,
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

unsigned int getNextEventTime(fmiEventInfo *eventInfo,
                              const char *log);

unsigned int terminate(const char *log);

