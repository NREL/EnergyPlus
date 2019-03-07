#ifndef EPFMI_I
#define EPFMI_I

%module epfmi

// This tells SWIG to treat char ** as a special case
%typemap(in) char ** {
  /* Check if is a list */
  if (PyList_Check($input)) {
    int size = PyList_Size($input);
    int i = 0;
    $1 = (char **) malloc((size+1)*sizeof(char *));
    for (i = 0; i < size; i++) {
      PyObject *o = PyList_GetItem($input,i);
      if (PyString_Check(o))
	$1[i] = PyString_AsString(PyList_GetItem($input,i));
      else {
	PyErr_SetString(PyExc_TypeError,"list must contain strings");
	free($1);
	return NULL;
      }
    }
    $1[i] = 0;
  } else {
    PyErr_SetString(PyExc_TypeError,"not a list");
    return NULL;
  }
}

// This cleans up the char ** array we malloc'd before the function call
%typemap(freearg) char ** {
  free((char *) $1);
}

%{
  #include "EPFMI.hpp"
%}

#define EPFMI_API

/* Type definitions of variables passed as arguments
   Version "default" means:

   fmi2Component           : an opaque object pointer
   fmi2ComponentEnvironment: an opaque object pointer
   fmi2FMUstate            : an opaque object pointer
   fmi2ValueReference      : handle to the value of a variable
   fmi2Real                : double precision floating-point data type
   fmi2Integer             : basic signed integer data type
   fmi2Boolean             : basic signed integer data type
   fmi2Char                : character data type
   fmi2String              : a pointer to a vector of fmi2Char characters
                             ('\0' terminated, UTF8 encoded)
   fmi2Byte                : smallest addressable unit of the machine, typically one byte.
*/
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

EPFMI_API unsigned int instantiate(const char *input,
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

EPFMI_API unsigned int setupExperiment(double tStart,
                             bool stopTimeDefined,
                             const char *log);

EPFMI_API unsigned int setTime(double time,
                     const char *log);

EPFMI_API unsigned int setVariables(const unsigned int valueReferences[],
                          const double variablePointers[],
                          size_t nVars1,
                          const char *log);

EPFMI_API unsigned int getVariables(const unsigned int valueReferences[],
                          double variablePointers[],
                          size_t nVars2,
                          const char *log);

// DLM: this is the only tricky one due to fmi2EventInfo*                 
//EPFMI_API unsigned int getNextEventTime(fmi2EventInfo *eventInfo,
//                              const char *log);

EPFMI_API unsigned int terminateSim(const char *log);
                             
#endif //EPFMI_I