/* Copyright (c) 2018 Big Ladder Software LLC. All rights reserved.
 * See the LICENSE file for additional terms and conditions. */

#ifndef BTWXT_ERROR_H_
#define BTWXT_ERROR_H_

#include <sstream>

// BTWXT
#include "btwxt.h"

namespace Btwxt {

void showMessage(MsgLevel messageType, std::string message);

struct expand_type {
  template <typename... T> expand_type(T &&...) {}
};

template <typename... ArgTypes> std::string stringify(ArgTypes... args) {
  std::ostringstream oss;
  expand_type{0, (oss << args, 0)...};
  return oss.str();
}

} // namespace Btwxt
#endif // BTWXT_ERROR_H_
