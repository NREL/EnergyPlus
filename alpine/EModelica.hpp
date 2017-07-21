#include "jni.h"
#include <memory>

class EModelica {
  public:

  EModelica();

  virtual ~EModelica();

  std::string compileModel(const std::string & moFilePath);

  private:

  JavaVM* m_vm;
  JNIEnv* m_env;
};

