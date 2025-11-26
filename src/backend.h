#ifndef BACKEND_H
#define BACKEND_H

#include "ast.h"
#include <memory>
#include "module_resolver.h"

enum OptLevel {
  Debug,
  Release,
};

class Backend {
public:
  virtual ~Backend() = default;
  virtual void generate(std::shared_ptr<Module> module) = 0;
};

#endif
