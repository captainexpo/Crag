#ifndef BACKEND_H
#define BACKEND_H

#include "ast.h"
#include <memory>
#include "module_resolver.h"



typedef enum {
  LLVM,
  // TODO: QBE,
} BackendType;

enum OptLevel {
  Debug,
  Release,
};

typedef struct {
  BackendType backend;
  OptLevel opt_level;
  bool do_runtime_safety;
} CompilerOptions;

class Backend {
public:
  virtual ~Backend() = default;
  virtual void generate(std::shared_ptr<Module> module) = 0;
};

#endif
