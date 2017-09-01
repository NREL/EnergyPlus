#include "EModelica.hpp"
#include "jit.h"
#include "cc.hpp"
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
#include <boost/filesystem.hpp>
#include <boost/regex.hpp>
#include <functional>
#include <stdint.h>
#include <stdlib.h>
#include <iostream>

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

// Ugly global data, means you can only have one instance of Emodelica
// Try to fix this but it is hard. See stub.cpp
llvm::orc::KaleidoscopeJIT * j;

extern "C" {

  #include <fmi1_functions.h>
  #include "xml_parser_cosim.h"

  extern char *C_GUID;
  extern int fmi_runtime_options_map_length;
  extern char **fmi_runtime_options_map_names;
  extern int *fmi_runtime_options_map_vrefs;

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

}

EModelica::EModelica() {
  JavaVMInitArgs vmArgs;
  vmArgs.version = JNI_VERSION_1_2;
  vmArgs.nOptions = 1;
  vmArgs.ignoreUnrecognized = JNI_TRUE;

  JavaVMOption options[vmArgs.nOptions];
  vmArgs.options = options;
  options[0].optionString = const_cast<char*>("-Xbootclasspath:[bootJar]");

  void* env;
  JNI_CreateJavaVM(&m_vm, &env, &vmArgs);

  m_env = static_cast<JNIEnv*>(env);
}

EModelica::~EModelica() {
  m_vm->DestroyJavaVM();
  llvm::llvm_shutdown();
  fmiFreeModelInstance();
}

void EModelica::compileModel(const std::string & moFilePath) {
  compileModelica(moFilePath);
  compileFMU();
}

void EModelica::compileModelica(const std::string & moFilePath) {
  jclass c = m_env->FindClass("Main");
  if (not m_env->ExceptionCheck()) {
    jmethodID m = m_env->GetStaticMethodID(c, "compileModel", "(Ljava/lang/String;)V");
    if (not m_env->ExceptionCheck()) {
      jclass stringClass = m_env->FindClass("java/lang/String");
      if (not m_env->ExceptionCheck()) {
          m_env->CallStaticVoidMethod(c, m,m_env->NewStringUTF(moFilePath.c_str()));
        //}
      }
    }
  }

  int exitCode = 0;
  if (m_env->ExceptionCheck()) {
    exitCode = -1;
    m_env->ExceptionDescribe();
  }
}


void EModelica::compileFMU() {
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

  if( auto symbol = j->findSymbol("C_GUID") ) {
    C_GUID = *((char **)symbol.getAddress());
  }

  if( auto symbol = j->findSymbol("fmi_runtime_options_map_length") ) {
    fmi_runtime_options_map_length = *((int *)symbol.getAddress());
  }

  if( auto symbol = j->findSymbol("fmi_runtime_options_map_names_local") ) {
    fmi_runtime_options_map_names = (char **)calloc(fmi_runtime_options_map_length, sizeof(char *));
    fmi_runtime_options_map_names = (char **)symbol.getAddress();
  }

  if( auto symbol = j->findSymbol("fmi_runtime_options_map_vrefs_local") ) {
    fmi_runtime_options_map_vrefs = (int *)realloc(fmi_runtime_options_map_vrefs,fmi_runtime_options_map_length * sizeof(int));
    fmi_runtime_options_map_vrefs = (int *)symbol.getAddress();
  }

  if( auto symbol = j->findSymbol("Vdp_fmiInstantiateModel") ) {
    m_fmiInstantiateModel = (t_fmiInstantiateModel)symbol.getAddress();
  }

  if( auto symbol = j->findSymbol("Vdp_fmiSetTime") ) {
    m_fmiSetTime = (t_fmiSetTime)symbol.getAddress();
  }

  if( auto symbol = j->findSymbol("Vdp_fmiInitialize") ) {
    m_fmiInitialize = (t_fmiInitialize)symbol.getAddress();
  }

  if( auto symbol = j->findSymbol("Vdp_fmiGetContinuousStates") ) {
    m_fmiGetContinuousStates = (t_fmiGetContinuousStates)symbol.getAddress();
  }

  if( auto symbol = j->findSymbol("Vdp_fmiSetContinuousStates") ) {
    m_fmiSetContinuousStates = (t_fmiSetContinuousStates)symbol.getAddress();
  }

  if( auto symbol = j->findSymbol("Vdp_fmiGetReal") ) {
    m_fmiGetReal = (t_fmiGetReal)symbol.getAddress();
  }

  if( auto symbol = j->findSymbol("Vdp_fmiSetReal") ) {
    m_fmiSetReal = (t_fmiSetReal)symbol.getAddress();
  }

  if( auto symbol = j->findSymbol("Vdp_fmiGetDerivatives") ) {
    m_fmiGetDerivatives = (t_fmiGetDerivatives)symbol.getAddress();
  }

  if( auto symbol = j->findSymbol("Vdp_fmiCompletedIntegratorStep") ) {
    m_fmiCompletedIntegratorStep = (t_fmiCompletedIntegratorStep)symbol.getAddress();
  }

  if( auto symbol = j->findSymbol("Vdp_fmiGetEventIndicators") ) {
    m_fmiGetEventIndicators = (t_fmiGetEventIndicators)symbol.getAddress();
  }

  if( auto symbol = j->findSymbol("Vdp_fmiTerminate") ) {
    m_fmiTerminate = (t_fmiTerminate)symbol.getAddress();
  }

  if( auto symbol = j->findSymbol("Vdp_fmiFreeModelInstance") ) {
    m_fmiFreeModelInstance = (t_fmiFreeModelInstance)symbol.getAddress();
  }

  if( auto symbol = j->findSymbol("Vdp_fmiEventUpdate") ) {
    m_fmiEventUpdate = (t_fmiEventUpdate)symbol.getAddress();
  }

  fmiString instanceName("Vdp");
  fmiString uid(C_GUID);
  fmiCallbackFunctions functions;
  fmiBoolean loggingOn(true);

  functions.logger = &myLogger;
  functions.allocateMemory = &calloc;
  functions.freeMemory = &free;

  m_c = m_fmiInstantiateModel(instanceName,uid,functions,loggingOn);
}

fmiStatus EModelica::fmiSetTime(fmiReal time) {
  return m_fmiSetTime(m_c,time);
}

fmiStatus EModelica::fmiInitialize(fmiBoolean toleranceControlled, fmiReal relativeTolerance, fmiEventInfo* eventInfo) {
  return m_fmiInitialize(m_c,toleranceControlled, relativeTolerance, eventInfo);
}

fmiStatus EModelica::fmiGetContinuousStates(fmiReal states[], size_t nx) {
  return m_fmiGetContinuousStates(m_c, states, nx);
}

fmiStatus EModelica::fmiSetContinuousStates(fmiReal states[], size_t nx) {
  return m_fmiSetContinuousStates(m_c, states, nx);
}

fmiStatus EModelica::fmiGetDerivatives(fmiReal derivatives[] , size_t nx) {
  return m_fmiGetDerivatives(m_c, derivatives, nx);
}

fmiStatus EModelica::fmiCompletedIntegratorStep(fmiBoolean* callEventUpdate) {
  return m_fmiCompletedIntegratorStep(m_c, callEventUpdate);
}

fmiStatus EModelica::fmiGetEventIndicators(fmiReal eventIndicators[], size_t ni) {
  return m_fmiGetEventIndicators(m_c, eventIndicators, ni);
}

fmiStatus EModelica::fmiTerminate() {
  return m_fmiTerminate(m_c);
}

void EModelica::fmiFreeModelInstance() {
  return m_fmiFreeModelInstance(m_c);
}

fmiStatus EModelica::fmiEventUpdate(fmiBoolean intermediateResults, fmiEventInfo* eventInfo) {
  return m_fmiEventUpdate(m_c, intermediateResults, eventInfo);
}

fmiStatus EModelica::fmiGetReal(const fmiValueReference vr[], size_t nvr, fmiReal value[]) {
  return m_fmiGetReal(m_c, vr, nvr, value);
}

fmiStatus EModelica::fmiSetReal(const fmiValueReference vr[], size_t nvr, const fmiReal value[]) {
  return m_fmiSetReal(m_c, vr, nvr, value);
}

fmiValueReference EModelica::scalarVariableValueReference(const std::string & variableName) const {
  ModelDescription*  md = parse("mo-build/modelDescription.xml");
  ScalarVariable * var = getVariableByName(md, variableName.c_str());
  return getValueReference(var);
}

