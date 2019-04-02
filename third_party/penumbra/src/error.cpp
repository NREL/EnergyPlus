/* Copyright (c) 2017 Big Ladder Software LLC. All rights reserved.
* See the LICENSE file for additional terms and conditions. */

// Standard
#include <iostream>

// Penumbra
#include <error.h>

namespace Pumbra {

PenumbraCallbackFunction penumbraCallbackFunction;
void* messageCallbackContextPtr;

void showMessage(
  const int messageType,
  const std::string message
)
{
  if (penumbraCallbackFunction != NULL) {
    (*penumbraCallbackFunction)(messageType, message, messageCallbackContextPtr);
  } else {
    if (messageType == MSG_ERR) {
      std::cerr << "Error: " << message << std::endl;
      exit(EXIT_FAILURE);
    } else if (messageType == MSG_WARN) {
      std::cerr << "Warning: " << message << std::endl;
    } else /*if (messageType == MSG_INFO)*/ {
      std::cout << "Note: " << message << std::endl;
    }
  }
}

}
