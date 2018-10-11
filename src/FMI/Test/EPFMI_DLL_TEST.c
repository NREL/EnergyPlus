#include "test-config.h"
#include <stdlib.h>
#include <stdio.h>
#include <dlfcn.h>
#include <stdbool.h>

typedef void*           fmi2Component;               /* Pointer to FMU instance       */
typedef void*           fmi2ComponentEnvironment;    /* Pointer to FMU environment    */
typedef void*           fmi2FMUstate;                /* Pointer to internal FMU state */
typedef unsigned int    fmi2ValueReference;
typedef double          fmi2Real   ;
typedef int             fmi2Integer;
typedef int             fmi2Boolean;
typedef char            fmi2Char;
typedef const fmi2Char* fmi2String;
typedef char            fmi2Byte;

typedef struct {
	 fmi2Boolean newDiscreteStatesNeeded;
   fmi2Boolean terminateSimulation;
   fmi2Boolean nominalsOfContinuousStatesChanged;
   fmi2Boolean valuesOfContinuousStatesChanged;
   fmi2Boolean nextEventTimeDefined;
   fmi2Real    nextEventTime;
} fmi2EventInfo;

typedef unsigned int (*fInstantiate)(const char *input,
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

typedef unsigned int (*fSetupExperiment)(double tStart,
                             bool stopTimeDefined,
                             const char *log);

typedef unsigned int (*fSetTime)(double time,
                     const char *log);

typedef unsigned int (*fSetVariables)(const unsigned int valueReferences[],
                          const double variablePointers[],
                          size_t nVars1,
                          const char *log);

typedef unsigned int (*fGetVariables)(const unsigned int valueReferences[],
                          double variablePointers[],
                          size_t nVars2,
                          const char *log);

typedef unsigned int (*fGetNextEventTime)(fmi2EventInfo *eventInfo,
                              const char *log);

typedef unsigned int (*fTerminate)(const char *log);

typedef struct FMU{
	void * dllHandle;
	fInstantiate instantiate;
	fSetupExperiment setupExperiment;
	fSetTime setTime;
	fSetVariables setVariables;
	fGetVariables getVariables;
	fGetNextEventTime getNextEventTime;
	fTerminate terminate;
} FMU;

void* getAdr(FMU *fmu, const char* functionName){
	void* fp;

#ifdef _MSC_VER
	fp = GetProcAddress(fmu->dllHandle, functionName);
#else
  printf("*****%s*****", functionName);
	fp = dlsym(fmu->dllHandle, functionName);
#endif
	if (!fp) {
		printf("****** Function %s,  not "
    "found in the EnergyPlus functions library****** \n",
    functionName);
	}
	return fp;
}

int loadLib(const char* libPath, FMU *fmu) {
  char msg[500];
#ifdef _MSC_VER
	HINSTANCE h;
#else
	void *h;
#endif


#ifdef _MSC_VER
	h = LoadLibrary(libPath);
	if (!h) {
		printf("****** Unable to load the "
    "EnergyPlus functions library with path %s ****** \n",
    libPath);
		return -1;
	}
#else
	h = dlopen(libPath, RTLD_LAZY);
	if (!h) {
		printf("****** Unable to load the "
    "EnergyPlus functions library with path %s ****** \n",
    libPath);
		return -1;
	}
#endif

	fmu->dllHandle = h;
	fmu->instantiate = (fInstantiate)getAdr(fmu, "instantiate");
	if (!(fmu->instantiate)) {
		printf("Can't find function instantiate()\n");
		return -1;
	}

	fmu->setupExperiment = (fSetupExperiment)getAdr(fmu, "setupExperiment");
	if (!(fmu->setupExperiment)) {
		printf("Can't find function setupExperiment()\n");
		return -1;
	}

	fmu->setTime = (fSetTime)getAdr(fmu, "setTime");
	if (!(fmu->setTime)) {
		printf("Can't find function setTime()\n");
		return -1;
	}

	fmu->setVariables = (fSetVariables) getAdr(fmu, "setVariables");
	if (!(fmu->setVariables)) {
		printf("Can't find function setVariables()\n");
		return -1;
	}
	fmu->getVariables = (fGetVariables)getAdr(fmu, "getVariables");
	if (!(fmu->getVariables)) {
		printf("Can't find function getVariables()\n");
		return -1;
	}

	fmu->getNextEventTime = (fGetNextEventTime)getAdr(fmu, "getNextEventTime");
	if (!(fmu->getNextEventTime)) {
		printf("Can't find function getNextEventTime()\n");
		return -1;
	}

	fmu->terminate = (fTerminate)getAdr(fmu, "terminate");
	if (!(fmu->terminate)) {
		printf("Can't find function terminate()\n");
		return -1;
	}
	return 0; //success

}

int main(){
  char msg[500];
  //FMUZone* zone = (FMUZone*) object;
  FMU* fmu;
	//FMU* fmu2;
  fmu = (FMU*)malloc(sizeof(FMU));
	//fmu2 = (FMU*)malloc(sizeof(FMU));
  int retVal;

	const char * inputNames[] = {"Attic,T", "Core_ZN,T", "Perimeter_ZN_1,T", "Perimeter_ZN_2,T", "Perimeter_ZN_3,T", "Perimeter_ZN_4,T"};
	const unsigned int inputValueReferences[] = {0, 1, 2, 3, 4, 5};

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
  retVal = loadLib(fmulib, fmu);
	if (retVal  < 0) {
		 printf("There was an error loading the EnergyPlus library\n");
		}
  printf("Initializing library \n");
  int result = fmu->instantiate(input, // input
                           weather, // weather
                           idd, // idd
                           "office", // instanceName
                           NULL, // parameterNames
                           NULL, // parameterValueReferences[]
                           0, // nPar
                           inputNames, // inputNames
                           inputValueReferences, // inputValueReferences[]
                           6, // nInp
                           outputNames, // outputNames
                           outputValueReferences, // outputValueReferences[]
                           24, // nOut
                           NULL); //log);


  double tStart = 0.0;
  int stopTimeDefined = 1;
  double tEnd = 86400;
	double outputs[] = {0.0};
	const unsigned int outputRefs[] = {6};
	double inputs[] = {21.0, 21.0,21.0, 21.0, 21.0, 21.0};
	const unsigned int inputRefs[] = {0, 1, 2, 3, 4, 5};
  fmi2EventInfo eventInfo;

  printf("Ready to setup experiment the FMU library \n");
  result = fmu->setupExperiment(tStart, 1, NULL);
	printf("Done setting up experiment the FMU library \n");
  double time = tStart;

  while ( time < tEnd ) {
		fmu->setTime(time, NULL);

    result = fmu->getNextEventTime(&eventInfo, NULL);
    result = fmu->setVariables(inputRefs, inputs, 6, NULL);
    result = fmu->getVariables(outputRefs, outputs, 1, NULL);
    printf("At time %f the output of name %s is %f\n.", time, outputNames[0], outputs[0]);
    //snprintf(msg, 500, "The output of value is is %f\n.", outputs[2]);
    //printf(msg);
    time = time + 600;
		printf("This is the time %f\n", time);
  }
	printf("Done running code in the loop\n");
  return 0;
}
