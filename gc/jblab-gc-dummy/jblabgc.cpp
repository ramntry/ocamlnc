#include <llvm/CodeGen/GCStrategy.h>
#include <llvm/CodeGen/GCMetadata.h>
#include <llvm/Support/Compiler.h>

namespace {
  class LLVM_LIBRARY_VISIBILITY JBLabGC : public llvm::GCStrategy {
  public:
    JBLabGC() {}
  };

  llvm::GCRegistry::Add<JBLabGC> gc("jblab-gc", "dummy gc");
}
