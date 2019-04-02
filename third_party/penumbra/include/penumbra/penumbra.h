/* Copyright (c) 2017 Big Ladder Software LLC. All rights reserved.
* See the LICENSE file for additional terms and conditions. */

#ifndef PENUMBRA_H_
#define PENUMBRA_H_

// Standard
#include <memory>
#include <string>
#include <vector>
#include <array>

// Penumbra
#include <penumbra/surface.h>

namespace Pumbra {

const int PN_SUCCESS = 0;
const int PN_FAILURE = 1;
const int MSG_INFO = 0;
const int MSG_WARN = 1;
const int MSG_ERR = 2;

typedef void(*PenumbraCallbackFunction)(
  const int messageType,
  const std::string message,
  void* contextPtr
  );

class PenumbraPrivate;

void penumbraTerminate(); // Call once before exiting calling program to ensure safe cleanup of OpenGL memory

class Penumbra {
public:
  Penumbra(unsigned size = 512u);
  
  Penumbra(
    PenumbraCallbackFunction callbackFunction,
    unsigned size = 512u
  );

  Penumbra(
    PenumbraCallbackFunction callbackFunction,
    void* contextPtr,
    unsigned size = 512u
  );

  ~Penumbra();

public:
  unsigned addSurface(const Surface& surface);
  int setModel();
  int clearModel();
  int setSunPosition(
    const float azm, // in radians, clockwise, north = 0
    const float alt  // in radians, horizon = 0, vertical = pi/2
  );
  float calculatePSSF(unsigned surfaceIndex);
  int renderScene(unsigned surfaceIndex); // Primarily for debug purposes
  void setMessageCallback(
    PenumbraCallbackFunction callbackFunction,
    void* contextPtr
  );

private:
  std::unique_ptr<PenumbraPrivate> penumbra;

};

}

#endif /* PENUMBRA_H_ */
