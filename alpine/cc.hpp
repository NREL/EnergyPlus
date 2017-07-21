#include "llvm/IR/Module.h"
#include "llvm/IR/LLVMContext.h"

std::unique_ptr<llvm::Module> compile(const std::string & source, llvm::LLVMContext & context);

