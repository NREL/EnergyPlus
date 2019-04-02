/* Copyright (c) 2017 Big Ladder Software LLC. All rights reserved.
* See the LICENSE file for additional terms and conditions. */

#ifndef PENUMBRA_ERROR_H_
#define PENUMBRA_ERROR_H_

// Penumbra
#include <penumbra/penumbra.h>

namespace Pumbra {

extern PenumbraCallbackFunction penumbraCallbackFunction;
extern void* messageCallbackContextPtr;

void showMessage(
  const int messageType,
  const std::string message
);

}
#endif // PENUMBRA_ERROR_H_
