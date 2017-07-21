#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/IR/LLVMContext.h"
#include "clang/Driver/Driver.h"

#include "jit.h"
#include "cc.hpp"
#include "EModelica.hpp"
#include "iostream"
#include <boost/filesystem.hpp>
#include <boost/regex.hpp>
#include <functional>

#if (defined __MINGW32__) || (defined _MSC_VER)
#  define EXPORT __declspec(dllexport)
#else
#  define EXPORT __attribute__ ((visibility("default"))) \
  __attribute__ ((used))
#endif

#if (! defined __x86_64__) && ((defined __MINGW32__) || (defined _MSC_VER))
#  define SYMBOL(x) binary_boot_jar_##x
#else
#  define SYMBOL(x) _binary_boot_jar_##x
#endif

extern "C" {

#include <fmiModelTypes.h>
#include <fmi1_types.h>
#include <fmi1_functions.h>
#include <jmi_types.h>

  extern const uint8_t SYMBOL(start)[];
  extern const uint8_t SYMBOL(end)[];

  EXPORT const uint8_t*
  bootJar(size_t* size)
  {
    *size = SYMBOL(end) - SYMBOL(start);
    return SYMBOL(start);
  }

  void myLogger(fmiComponent c, fmiString instanceName, fmiStatus status, fmiString category, fmiString message, ...) {
    std::cout << "FMI Logger Message: " << message << " Category: " << category << std::endl;;
  }

} // extern "C"

extern "C" void __cxa_pure_virtual(void) { abort(); }

extern "C" {
extern char *C_GUID;
extern int fmi_runtime_options_map_length;
extern char **fmi_runtime_options_map_names;
extern int *fmi_runtime_options_map_vrefs;

}

llvm::orc::KaleidoscopeJIT * j;

int main(int argc, const char **argv, char * const *envp) {
  if( argc < 2 ) {
    return 0;
  }

  EModelica emo;
  emo.compileModel(argv[1]);

  const std::string target_path( "mo-build/sources/" );
  const boost::regex c_filter( ".*\\.c" );
  
  std::vector< std::string > files;
  std::vector< std::string > oFiles;
  
  boost::filesystem::directory_iterator end_itr;
  for( boost::filesystem::directory_iterator i( target_path ); i != end_itr; ++i )
  {
      if( !boost::filesystem::is_regular_file( i->status() ) ) continue;
      boost::smatch what;
      if( !boost::regex_match( i->path().string(), what, c_filter ) ) continue;
      files.push_back( i->path().string() );
  }

  llvm::InitializeNativeTarget();
  llvm::InitializeNativeTargetAsmPrinter();
  llvm::InitializeNativeTargetAsmParser();

  j = new llvm::orc::KaleidoscopeJIT();

  llvm::LLVMContext context;

  for( const auto & file : files ) {
    std::cout << "Compiling file: " << file << std::endl;
    std::unique_ptr<llvm::Module> m = compile(file,context);
    if( m ) {
      auto h = j->addModule(std::move(m));
    }
  }

  if( auto symbol = j->findSymbol("Vdp_fmiGetVersion") ) {
    char* (*fmiGetVersion)(void) =
      (char*(*)(void))symbol.getAddress();
    const char* result = fmiGetVersion();
    std::cout << "fmi version is: " << result << std::endl << std::flush;
  } else {
    std::cout << "symbol not found" << std::endl << std::flush;
  }

  if( auto symbol = j->findSymbol("C_GUID") ) {
    C_GUID = *((char **)symbol.getAddress());
    std::cout << "C_GUID is: " << C_GUID << std::endl << std::flush;
  } else {
    std::cout << "symbol not found" << std::endl << std::flush;
  }

  if( auto symbol = j->findSymbol("fmi_runtime_options_map_length") ) {
    fmi_runtime_options_map_length = *((int *)symbol.getAddress());
  } else {
    std::cout << "symbol not found" << std::endl << std::flush;
  }

  if( auto symbol = j->findSymbol("fmi_runtime_options_map_names_local") ) {
    fmi_runtime_options_map_names = (char **)calloc(fmi_runtime_options_map_length, sizeof(char *));
    fmi_runtime_options_map_names = (char **)symbol.getAddress();
  } else {
    std::cout << "symbol not found" << std::endl << std::flush;
  }

  if( auto symbol = j->findSymbol("fmi_runtime_options_map_vrefs_local") ) {
    fmi_runtime_options_map_vrefs = (int *)realloc(fmi_runtime_options_map_vrefs,fmi_runtime_options_map_length * sizeof(int));
    fmi_runtime_options_map_vrefs = (int *)symbol.getAddress();
  } else {
    std::cout << "symbol not found" << std::endl << std::flush;
  }

  typedef fmiComponent (*t_fmiInstantiateModel)(fmiString,fmiString,fmiCallbackFunctions,fmiBoolean);
  std::function<std::remove_pointer<t_fmiInstantiateModel>::type > fmiInstantiateModel;

  typedef fmiStatus (*t_fmiSetTime)(fmiComponent, fmiReal);
  std::function<std::remove_pointer<t_fmiSetTime>::type > fmiSetTime;

  typedef fmiStatus (*t_fmiInitialize)(fmiComponent c, fmiBoolean toleranceControlled, fmiReal relativeTolerance, fmiEventInfo* eventInfo);
  std::function<std::remove_pointer<t_fmiInitialize>::type > fmiInitialize;

  typedef fmiStatus (*t_fmiGetContinuousStates)(fmiComponent c, fmiReal states[], size_t nx);
  std::function<std::remove_pointer<t_fmiGetContinuousStates>::type > fmiGetContinuousStates;

  typedef fmiStatus (*t_fmiSetContinuousStates)(fmiComponent c, fmiReal states[], size_t nx);
  std::function<std::remove_pointer<t_fmiSetContinuousStates>::type > fmiSetContinuousStates;

  typedef fmiStatus (*t_fmiGetDerivatives)(fmiComponent c, fmiReal derivatives[] , size_t nx);
  std::function<std::remove_pointer<t_fmiGetDerivatives>::type > fmiGetDerivatives;

  typedef fmiStatus (*t_fmiCompletedIntegratorStep)(fmiComponent c, fmiBoolean* callEventUpdate);
  std::function<std::remove_pointer<t_fmiCompletedIntegratorStep>::type > fmiCompletedIntegratorStep;

  typedef  fmiStatus (*t_fmiGetEventIndicators)(fmiComponent c, fmiReal eventIndicators[], size_t ni);
  std::function<std::remove_pointer<t_fmiGetEventIndicators>::type > fmiGetEventIndicators;

  typedef fmiStatus (*t_fmiTerminate)(fmiComponent c);
  std::function<std::remove_pointer<t_fmiTerminate>::type > fmiTerminate;

  typedef void (*t_fmiFreeModelInstance)(fmiComponent c);
  std::function<std::remove_pointer<t_fmiFreeModelInstance>::type > fmiFreeModelInstance;

  typedef fmiStatus (*t_fmiEventUpdate)(fmiComponent c, fmiBoolean intermediateResults, fmiEventInfo* eventInfo);
  std::function<std::remove_pointer<t_fmiEventUpdate>::type > fmiEventUpdate;

  typedef fmiStatus (*t_fmiGetReal)(fmiComponent c, const fmiValueReference vr[], size_t nvr, fmiReal value[]);
  std::function<std::remove_pointer<t_fmiGetReal>::type > fmiGetReal;

  typedef fmiStatus (*t_fmiSetReal)(fmiComponent c, const fmiValueReference vr[], size_t nvr, const fmiReal value[]);
  std::function<std::remove_pointer<t_fmiSetReal>::type > fmiSetReal;

  if( auto symbol = j->findSymbol("Vdp_fmiInstantiateModel") ) {
    fmiInstantiateModel = (t_fmiInstantiateModel)symbol.getAddress();
  }

  if( auto symbol = j->findSymbol("Vdp_fmiSetTime") ) {
    fmiSetTime = (t_fmiSetTime)symbol.getAddress();
  }

  if( auto symbol = j->findSymbol("Vdp_fmiInitialize") ) {
    fmiInitialize = (t_fmiInitialize)symbol.getAddress();
  }

  if( auto symbol = j->findSymbol("Vdp_fmiGetContinuousStates") ) {
    fmiGetContinuousStates = (t_fmiGetContinuousStates)symbol.getAddress();
  }

  if( auto symbol = j->findSymbol("Vdp_fmiSetContinuousStates") ) {
    fmiSetContinuousStates = (t_fmiSetContinuousStates)symbol.getAddress();
  }

  if( auto symbol = j->findSymbol("Vdp_fmiGetReal") ) {
    fmiGetReal = (t_fmiGetReal)symbol.getAddress();
  }

  if( auto symbol = j->findSymbol("Vdp_fmiSetReal") ) {
    fmiSetReal = (t_fmiSetReal)symbol.getAddress();
  }

  if( auto symbol = j->findSymbol("Vdp_fmiGetDerivatives") ) {
    fmiGetDerivatives = (t_fmiGetDerivatives)symbol.getAddress();
  }

  if( auto symbol = j->findSymbol("Vdp_fmiCompletedIntegratorStep") ) {
    fmiCompletedIntegratorStep = (t_fmiCompletedIntegratorStep)symbol.getAddress();
  }

  if( auto symbol = j->findSymbol("Vdp_fmiGetEventIndicators") ) {
    fmiGetEventIndicators = (t_fmiGetEventIndicators)symbol.getAddress();
  }

  if( auto symbol = j->findSymbol("Vdp_fmiTerminate") ) {
    fmiTerminate = (t_fmiTerminate)symbol.getAddress();
  }

  if( auto symbol = j->findSymbol("Vdp_fmiFreeModelInstance") ) {
    fmiFreeModelInstance = (t_fmiFreeModelInstance)symbol.getAddress();
  }

  if( auto symbol = j->findSymbol("Vdp_fmiEventUpdate") ) {
    fmiEventUpdate = (t_fmiEventUpdate)symbol.getAddress();
  }

  fmiString instanceName("Vdp");
  fmiString uid(C_GUID);
  fmiCallbackFunctions functions;
  fmiBoolean loggingOn(true);

  functions.logger = &myLogger;
  functions.allocateMemory = &calloc;
  functions.freeMemory = &free;

  double tEnd = 2;
  double h = 0.01;

  // Simulate
  int i;
  double dt, tPre;
  fmiBoolean timeEvent, stateEvent, stepEvent;
  double time;  
  int nx;                          // number of state variables
  int nz;                          // number of state event indicators
  double *x;                       // continuous states
  double *xdot;                    // the crresponding derivatives in same order
  double *z = NULL;                // state event indicators
  double *prez = NULL;             // previous values of state event indicators
  fmiEventInfo eventInfo;          // updated by calls to initialize and eventUpdate
  //ModelDescription* md;            // handle to the parsed XML file
  const char* guid;                // global unique id of the fmu
  fmiCallbackFunctions callbacks;  // called by the model during simulation
  fmiComponent c;                  // instance of the fmu 
  fmiStatus fmiFlag;               // return code of the fmu functions
  fmiReal t0 = 0;                  // start time
  fmiBoolean toleranceControlled = fmiFalse;
  int nSteps = 0;
  int nTimeEvents = 0;
  int nStepEvents = 0;
  int nStateEvents = 0;

  fmiValueReference inputRefs[1];
  double inputs[1];
  // 46 corresponds to:
	//	<ScalarVariable name="cop" valueReference="46" variability="continuous" causality="input" alias="noAlias">
  //	as found in the FMU's xml file
  inputRefs[0] = 46;

  c = fmiInstantiateModel(instanceName,uid,functions,loggingOn);

  if( ! c ) {
    std::cout << "did not get component" << std::endl; 
    return 0;
  }

  // allocate memory 
  nx = 2;
  nz = 1;
  x    = (double *) calloc(nx, sizeof(double));
  xdot = (double *) calloc(nx, sizeof(double));
  if (nz>0) {
      z    =  (double *) calloc(nz, sizeof(double));
      prez =  (double *) calloc(nz, sizeof(double));
  }
  if ((!x || !xdot) || (nz>0 && (!z || !prez))) return 0;

  // set the start time and initialize
  time = t0;
  fmiFlag =  fmiSetTime(c, t0);
  if (fmiFlag > fmiWarning) return 0;
  fmiFlag =  fmiInitialize(c, toleranceControlled, t0, &eventInfo);
  if (fmiFlag > fmiWarning)  return 0;
  if (eventInfo.terminateSimulation) {
      std::cout << "model requested termination at init" << std::endl;
      tEnd = time;
  }

  while (time < tEnd) {
   // get current state and derivatives
   fmiFlag = fmiGetContinuousStates(c, x, nx);
   if (fmiFlag > fmiWarning) return 0; //error("could not retrieve states");
   fmiFlag = fmiGetDerivatives(c, xdot, nx);
   if (fmiFlag > fmiWarning) return 0; //error("could not retrieve derivatives");

   // advance time
   tPre = time;
   time = fmin(time+h, tEnd);
   timeEvent = eventInfo.upcomingTimeEvent && eventInfo.nextEventTime <= time;
   if (timeEvent) time = eventInfo.nextEventTime;
   dt = time - tPre; 
   fmiFlag = fmiSetTime(c, time);
   inputs[0] = time / 2.0 * 3.0; // this is just a demo, input[0] is cop, it isn't used, scaling it by time is meaningless
   fmiSetReal(c,inputRefs,1,inputs);
   // DllExport fmiStatus fmiSetReal(fmiComponent c, const fmiValueReference vr[], size_t nvr, const fmiReal value[]) {
   //if (fmiFlag > fmiWarning) //error("could not set time");

   // perform one step
   for (i=0; i<nx; i++) x[i] += dt*xdot[i]; // forward Euler method
   fmiFlag = fmiSetContinuousStates(c, x, nx);
   if (fmiFlag > fmiWarning) return 0; //error("could not set states");
   if (loggingOn) printf("Step %d to t=%.16g, x1=%.16g, x2=%.16g\n", nSteps, time,x[0],x[1]);
   if (loggingOn) {
     fmiGetReal(c,inputRefs,1,inputs);
     printf("Step %d to t=%.16g, cop=%.16g \n", nSteps, time,  inputs[0]);
   } 

   // Check for step event, e.g. dynamic state selection
   fmiFlag = fmiCompletedIntegratorStep(c, &stepEvent);
   if (fmiFlag > fmiWarning) return 0; //error("could not complete integrator step");

   // Check for state event
   for (i=0; i<nz; i++) prez[i] = z[i]; 
   fmiFlag = fmiGetEventIndicators(c, z, nz);
   if (fmiFlag > fmiWarning) return 0; //error("could not retrieve event indicators");
   stateEvent = FALSE;
   for (i=0; i<nz; i++) 
       stateEvent = stateEvent || (prez[i] * z[i] < 0);

   // handle events
   if (timeEvent || stateEvent || stepEvent) {

      if (timeEvent) {
          nTimeEvents++;
          if (loggingOn) printf("time event at t=%.16g\n", time);
      }
      if (stateEvent) {
          nStateEvents++;
          if (loggingOn) for (i=0; i<nz; i++)
              printf("state event %s z[%d] at t=%.16g\n", 
                      (prez[i]>0 && z[i]<0) ? "-\\-" : "-/-", i, time);
      }
      if (stepEvent) {
          nStepEvents++;
          if (loggingOn) printf("step event at t=%.16g\n", time);
      }

      // event iteration in one step, ignoring intermediate results
      fmiFlag = fmiEventUpdate(c, fmiFalse, &eventInfo);
      if (fmiFlag > fmiWarning) return 0; //error("could not perform event update");
      
      // terminate simulation, if requested by the model
      if (eventInfo.terminateSimulation) {
          printf("model requested termination at t=%.16g\n", time);
          break; // success
      }

      // check for change of value of states
      if (eventInfo.stateValuesChanged && loggingOn) {
          printf("state values changed at t=%.16g\n", time);
      }
      
      // check for selection of new state variables
      if (eventInfo.stateValueReferencesChanged && loggingOn) {
          printf("new state variables selected at t=%.16g\n", time);
      }

   } // if event


   nSteps++;
  }

  llvm::llvm_shutdown();

  std::cout << "shutting down" << std::endl;

  return 0;
}

