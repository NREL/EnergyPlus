#include "jni.h"
#include <memory>
#include <functional>

extern "C" {

#include <fmi1_functions.h>

}

typedef fmiComponent (*t_fmiInstantiateModel)(fmiString,fmiString,fmiCallbackFunctions,fmiBoolean);
typedef fmiStatus (*t_fmiSetTime)(fmiComponent, fmiReal);
typedef fmiStatus (*t_fmiInitialize)(fmiComponent c, fmiBoolean toleranceControlled, fmiReal relativeTolerance, fmiEventInfo* eventInfo);
typedef fmiStatus (*t_fmiGetContinuousStates)(fmiComponent c, fmiReal states[], size_t nx);
typedef fmiStatus (*t_fmiSetContinuousStates)(fmiComponent c, fmiReal states[], size_t nx);
typedef fmiStatus (*t_fmiGetDerivatives)(fmiComponent c, fmiReal derivatives[] , size_t nx);
typedef fmiStatus (*t_fmiCompletedIntegratorStep)(fmiComponent c, fmiBoolean* callEventUpdate);
typedef  fmiStatus (*t_fmiGetEventIndicators)(fmiComponent c, fmiReal eventIndicators[], size_t ni);
typedef fmiStatus (*t_fmiTerminate)(fmiComponent c);
typedef void (*t_fmiFreeModelInstance)(fmiComponent c);
typedef fmiStatus (*t_fmiEventUpdate)(fmiComponent c, fmiBoolean intermediateResults, fmiEventInfo* eventInfo);
typedef fmiStatus (*t_fmiGetReal)(fmiComponent c, const fmiValueReference vr[], size_t nvr, fmiReal value[]);
typedef fmiStatus (*t_fmiSetReal)(fmiComponent c, const fmiValueReference vr[], size_t nvr, const fmiReal value[]);

class EModelica {
  public:

  EModelica();

  virtual ~EModelica();

  void compileModel(const std::string & moFilePath);

  fmiValueReference scalarVariableValueReference(const std::string & variableName) const;

  fmiStatus fmiSetTime(fmiReal);
  fmiStatus fmiInitialize(fmiBoolean toleranceControlled, fmiReal relativeTolerance, fmiEventInfo* eventInfo);
  fmiStatus fmiGetContinuousStates(fmiReal states[], size_t nx);
  fmiStatus fmiSetContinuousStates(fmiReal states[], size_t nx);
  fmiStatus fmiGetDerivatives(fmiReal derivatives[] , size_t nx);
  fmiStatus fmiCompletedIntegratorStep(fmiBoolean* callEventUpdate);
  fmiStatus fmiGetEventIndicators(fmiReal eventIndicators[], size_t ni);
  fmiStatus fmiTerminate();
  void fmiFreeModelInstance();
  fmiStatus fmiEventUpdate(fmiBoolean intermediateResults, fmiEventInfo* eventInfo);
  fmiStatus fmiGetReal(const fmiValueReference vr[], size_t nvr, fmiReal value[]);
  fmiStatus fmiSetReal(const fmiValueReference vr[], size_t nvr, const fmiReal value[]);

  private:

  void compileModelica(const std::string & moFilePath);
  void compileFMU();

  JavaVM* m_vm;
  JNIEnv* m_env;

  std::function<std::remove_pointer<t_fmiInstantiateModel>::type > m_fmiInstantiateModel;
  std::function<std::remove_pointer<t_fmiSetTime>::type > m_fmiSetTime;
  std::function<std::remove_pointer<t_fmiInitialize>::type > m_fmiInitialize;
  std::function<std::remove_pointer<t_fmiGetContinuousStates>::type > m_fmiGetContinuousStates;
  std::function<std::remove_pointer<t_fmiSetContinuousStates>::type > m_fmiSetContinuousStates;
  std::function<std::remove_pointer<t_fmiGetDerivatives>::type > m_fmiGetDerivatives;
  std::function<std::remove_pointer<t_fmiCompletedIntegratorStep>::type > m_fmiCompletedIntegratorStep;
  std::function<std::remove_pointer<t_fmiGetEventIndicators>::type > m_fmiGetEventIndicators;
  std::function<std::remove_pointer<t_fmiTerminate>::type > m_fmiTerminate;
  std::function<std::remove_pointer<t_fmiFreeModelInstance>::type > m_fmiFreeModelInstance;
  std::function<std::remove_pointer<t_fmiEventUpdate>::type > m_fmiEventUpdate;
  std::function<std::remove_pointer<t_fmiGetReal>::type > m_fmiGetReal;
  std::function<std::remove_pointer<t_fmiSetReal>::type > m_fmiSetReal;

  fmiComponent m_c;                  // instance of the fmu 
};

