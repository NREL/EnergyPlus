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

#include <fmiModelTypes.h>
#include <fmi1_types.h>
#include <fmi1_functions.h>
#include <jmi_types.h>

int main(int argc, const char **argv, char * const *envp) {
  if( argc < 2 ) {
    return 0;
  }

  EModelica emo;
  emo.compileModel(argv[1]);

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
  fmiStatus fmiFlag;               // return code of the fmu functions
  fmiReal t0 = 0;                  // start time
  fmiBoolean toleranceControlled = fmiFalse;
  int nSteps = 0;
  int nTimeEvents = 0;
  int nStepEvents = 0;
  int nStateEvents = 0;

  fmiValueReference inputRefs[1];
  double inputs[1];
  inputRefs[0] = emo.scalarVariableValueReference("cop");

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
  fmiFlag =  emo.fmiSetTime(t0);
  if (fmiFlag > fmiWarning) return 0;
  fmiFlag =  emo.fmiInitialize(toleranceControlled, t0, &eventInfo);
  if (fmiFlag > fmiWarning)  return 0;
  if (eventInfo.terminateSimulation) {
      std::cout << "model requested termination at init" << std::endl;
      tEnd = time;
  }

  bool loggingOn = true;

  while (time < tEnd) {
   // get current state and derivatives
   fmiFlag = emo.fmiGetContinuousStates(x, nx);
   if (fmiFlag > fmiWarning) return 0; //error("could not retrieve states");
   fmiFlag = emo.fmiGetDerivatives(xdot, nx);
   if (fmiFlag > fmiWarning) return 0; //error("could not retrieve derivatives");

   // advance time
   tPre = time;
   time = fmin(time+h, tEnd);
   timeEvent = eventInfo.upcomingTimeEvent && eventInfo.nextEventTime <= time;
   if (timeEvent) time = eventInfo.nextEventTime;
   dt = time - tPre; 
   fmiFlag = emo.fmiSetTime( time);
   inputs[0] = time / 2.0 * 3.0; // this is just a demo, input[0] is cop, it isn't used, scaling it by time is meaningless
   emo.fmiSetReal(inputRefs,1,inputs);
   // DllExport fmiStatus fmiSetReal(fmiComponent c, const fmiValueReference vr[], size_t nvr, const fmiReal value[]) {
   //if (fmiFlag > fmiWarning) //error("could not set time");

   // perform one step
   for (i=0; i<nx; i++) x[i] += dt*xdot[i]; // forward Euler method
   fmiFlag = emo.fmiSetContinuousStates(x, nx);
   if (fmiFlag > fmiWarning) return 0; //error("could not set states");
   if (loggingOn) printf("Step %d to t=%.16g, x1=%.16g, x2=%.16g\n", nSteps, time,x[0],x[1]);
   if (loggingOn) {
     emo.fmiGetReal(inputRefs,1,inputs);
     printf("Step %d to t=%.16g, cop=%.16g \n", nSteps, time,  inputs[0]);
   } 

   // Check for step event, e.g. dynamic state selection
   fmiFlag = emo.fmiCompletedIntegratorStep(&stepEvent);
   if (fmiFlag > fmiWarning) return 0; //error("could not complete integrator step");

   // Check for state event
   for (i=0; i<nz; i++) prez[i] = z[i]; 
   fmiFlag = emo.fmiGetEventIndicators(z, nz);
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
      fmiFlag = emo.fmiEventUpdate(fmiFalse, &eventInfo);
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

  std::cout << "shutting down" << std::endl;

  return 0;
}

