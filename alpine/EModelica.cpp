#include "EModelica.hpp"
#include "stdint.h"
#include "stdlib.h"
#include "iostream"

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
}

std::string EModelica::compileModel(const std::string & moFilePath) {
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

  return std::string();
}

