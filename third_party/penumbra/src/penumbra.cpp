/* Copyright (c) 2017 Big Ladder Software LLC. All rights reserved.
* See the LICENSE file for additional terms and conditions. */

// Standard
#include <memory>
#include <iostream>

// Penumbra
#include <penumbra/penumbra.h>
#include <penumbra-private.h>
#include <error.h>

namespace Pumbra{

void penumbraTerminate() {
	glfwTerminate();
}

Penumbra::Penumbra(unsigned int size)
{
  penumbra = std::unique_ptr<PenumbraPrivate>(new PenumbraPrivate(size));
}

Penumbra::Penumbra(
  PenumbraCallbackFunction callbackFunction,
  unsigned size
)
{
  setMessageCallback(callbackFunction, NULL);
  penumbra = std::unique_ptr<PenumbraPrivate>(new PenumbraPrivate(size));
}

Penumbra::Penumbra(
  PenumbraCallbackFunction callbackFunction,
  void* contextPtr,
  unsigned size
)
{
  setMessageCallback(callbackFunction, contextPtr);
  penumbra = std::unique_ptr<PenumbraPrivate>(new PenumbraPrivate(size));
}

Penumbra::~Penumbra(){}

unsigned Penumbra::addSurface(const Surface& surface)
{
  penumbra->addSurface(surface);
  return penumbra->surfaceCounter++;
}

int Penumbra::setModel()
{
  if (penumbra->surfaces.size() > 0) {
    const int vertexSize = 3;
    unsigned nextStartingIndex = 0;
    for (auto& surface : penumbra->surfaces ) {
      TessData tess = surface.tessellate();
      penumbra->surfaceBuffers.push_back({nextStartingIndex/vertexSize,tess.numVerts/vertexSize});
      for (unsigned i = 0; i < tess.numVerts; ++i) {
        penumbra->model.push_back(tess.vertices[i]);
      }
      nextStartingIndex += tess.numVerts;
    }

    penumbra->context.setModel(penumbra->model);
  }
  else {
    showMessage(MSG_WARN,"No surfaces added to Penumbra before calling setModel().");
  }
  return PN_SUCCESS;
}

int Penumbra::clearModel(){
  penumbra->surfaces.clear();
  penumbra->surfaceCounter = 0;
  penumbra->surfaceBuffers.clear();
  penumbra->model.clear();
  penumbra->context.clearModel();
  return PN_SUCCESS;
}

int Penumbra::setSunPosition(
  const float azm, // in radians, clockwise, north = 0
  const float alt  // in radians, horizon = 0, vertical = pi/2
)
{
  penumbra->sun.setView(azm, alt);
  return PN_SUCCESS;
}

float Penumbra::calculatePSSF(unsigned surfaceIndex)
{
  if (penumbra->checkSurface(surfaceIndex)) {
    penumbra->context.setScene(
      penumbra->surfaceBuffers[surfaceIndex].first,
      penumbra->surfaceBuffers[surfaceIndex].second,
      penumbra->sun.getView()
    );

    return penumbra->context.calculatePSSF(
      penumbra->surfaceBuffers[surfaceIndex].first,
      penumbra->surfaceBuffers[surfaceIndex].second
    );
  } else {
    showMessage(MSG_ERR, "Surface index, X, does not exist. Cannot calculate PSSF."); // TODO format string
    return -1.f;
  }
}

int Penumbra::renderScene(unsigned surfaceIndex)
{
  if (penumbra->checkSurface(surfaceIndex)) {
    penumbra->context.setScene(
      penumbra->surfaceBuffers[surfaceIndex].first,
      penumbra->surfaceBuffers[surfaceIndex].second,
      penumbra->sun.getView()
    );
    penumbra->context.showRendering(
      penumbra->surfaceBuffers[surfaceIndex].first,
      penumbra->surfaceBuffers[surfaceIndex].second
    );
    return PN_SUCCESS;
  }
  else {
    showMessage(MSG_ERR, "Surface index, X, does not exist. Cannot render scene."); // TODO format string
    return PN_FAILURE;
  }
}

void Penumbra::setMessageCallback(
  PenumbraCallbackFunction callBackFunction,
  void* contextPtr
)
{
  penumbraCallbackFunction = callBackFunction;
  messageCallbackContextPtr = contextPtr;
}

PenumbraPrivate::PenumbraPrivate(unsigned size) :
  context(size),
  surfaceCounter(0)
{}

PenumbraPrivate::~PenumbraPrivate(){}

void PenumbraPrivate::addSurface(const Surface& surface)
{
  surfaces.push_back(*surface.surface);
}

bool PenumbraPrivate::checkSurface(const unsigned index)
{
  return index < surfaces.size();
}


}
