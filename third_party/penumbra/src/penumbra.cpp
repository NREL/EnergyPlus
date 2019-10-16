/* Copyright (c) 2017 Big Ladder Software LLC. All rights reserved.
 * See the LICENSE file for additional terms and conditions. */

// Standard
#include <memory>
#include <iostream>

// Penumbra
#include <penumbra/penumbra.h>
#include <penumbra-private.h>
#include <error.h>

namespace Pumbra {

void penumbraTerminate() { glfwTerminate(); }

Penumbra::Penumbra(unsigned int size) {
  penumbra = std::unique_ptr<PenumbraPrivate>(new PenumbraPrivate(size));
}

Penumbra::Penumbra(PenumbraCallbackFunction callbackFunction, unsigned size) {
  setMessageCallback(callbackFunction, NULL);
  penumbra = std::unique_ptr<PenumbraPrivate>(new PenumbraPrivate(size));
}

Penumbra::Penumbra(PenumbraCallbackFunction callbackFunction, void *contextPtr, unsigned size) {
  setMessageCallback(callbackFunction, contextPtr);
  penumbra = std::unique_ptr<PenumbraPrivate>(new PenumbraPrivate(size));
}

Penumbra::~Penumbra() {}

unsigned Penumbra::addSurface(const Surface &surface) {
  penumbra->addSurface(surface);
  return penumbra->surfaces.size() - 1u;
}

unsigned Penumbra::getNumSurfaces() {
  return penumbra->surfaces.size();
}

int Penumbra::setModel() {
  if (penumbra->surfaces.size() > 0) {

    // Tesselate each surface into triangles
    std::vector<SurfaceBuffer> surfaceBuffers;
    unsigned nextStartingIndex = 0;
    unsigned surfNum = 0;
    for (auto &surface : penumbra->surfaces) {
      TessData tess = surface.tessellate();
      surfaceBuffers.emplace_back(nextStartingIndex / TessData::vertexSize,
                                  tess.numVerts / TessData::vertexSize, surfNum);
      for (unsigned i = 0; i < tess.numVerts; ++i) {
        penumbra->model.push_back(tess.vertices[i]);
      }
      nextStartingIndex += tess.numVerts;
      ++surfNum;
    }
    penumbra->context.setModel(penumbra->model, surfaceBuffers);
  } else {
    showMessage(MSG_WARN, "No surfaces added to Penumbra before calling setModel().");
  }
  return PN_SUCCESS;
}

int Penumbra::clearModel() {
  penumbra->surfaces.clear();
  penumbra->model.clear();
  penumbra->context.clearModel();
  return PN_SUCCESS;
}

int Penumbra::setSunPosition(const float azm, // in radians, clockwise, north = 0
                             const float alt  // in radians, horizon = 0, vertical = pi/2
) {
  penumbra->sun.setView(azm, alt);
  return PN_SUCCESS;
}

float Penumbra::getSunAzimuth() {
  return penumbra->sun.getAzimuth();
}

float Penumbra::getSunAltitude() {
  return penumbra->sun.getAltitude();
}

void Penumbra::submitPSSA(unsigned surfaceIndex) {
  if (penumbra->checkSurface(surfaceIndex)) {
    penumbra->context.submitPSSA(surfaceIndex, penumbra->sun.getView());
  } else {
    showMessage(MSG_ERR,
                "Surface index, X, does not exist. Cannot calculate PSSA."); // TODO format string
  }
}

void Penumbra::submitPSSA(const std::vector<unsigned> &surfaceIndices) {
  for (auto const surfaceIndex : surfaceIndices) {
    if (!penumbra->checkSurface(surfaceIndex)) {
      showMessage(MSG_ERR, "Surface index, X, does not exist. Cannot calculate PSSA."); // TODO format string
      return;
    }
  }
  penumbra->context.submitPSSA(surfaceIndices, penumbra->sun.getView());
}

void Penumbra::submitPSSA() {
  penumbra->context.submitPSSA(penumbra->sun.getView());
}

float Penumbra::calculatePSSA(unsigned surfaceIndex) {
  if (penumbra->checkSurface(surfaceIndex)) {
    return penumbra->context.calculatePSSA(surfaceIndex);
  } else {
    showMessage(MSG_ERR,
                "Surface index, X, does not exist. Cannot calculate PSSA."); // TODO format string
    return -1.f;
  }
}

std::vector<float> Penumbra::calculatePSSA(const std::vector<unsigned> &surfaceIndices) {
  for (auto const surfaceIndex : surfaceIndices) {
    if (!penumbra->checkSurface(surfaceIndex)) {
      showMessage(MSG_ERR, "Surface index, X, does not exist. Cannot calculate PSSA."); // TODO format string
      return {};
    }
  }
  return penumbra->context.calculatePSSA(surfaceIndices);;
}

std::vector<float> Penumbra::calculatePSSA() {
  return penumbra->context.calculatePSSA();;
}

std::map<unsigned, float>
Penumbra::calculateInteriorPSSAs(const std::vector<unsigned> &transparentSurfaceIndices,
                                 const std::vector<unsigned> &interiorSurfaceIndices) {
  std::map<unsigned, float> pssas;
  if (transparentSurfaceIndices.size() > 0) {
    if (penumbra->checkSurface(transparentSurfaceIndices[0])) {
      for (auto &transSurf : transparentSurfaceIndices) {
        if (!penumbra->checkSurface(transSurf)) {
          showMessage(
              MSG_ERR,
              "Transparent surface index, X, does not exist. Cannot calculate PSSA."); // TODO format string
        }
      }
      for (auto &intSurf : interiorSurfaceIndices) {
        if (!penumbra->checkSurface(intSurf)) {
          showMessage(
              MSG_ERR,
              "Interior surface index, X, does not exist. Cannot calculate PSSA."); // TODO format
                                                                                    // string
        }
      }
      pssas = penumbra->context.calculateInteriorPSSAs(transparentSurfaceIndices, interiorSurfaceIndices, penumbra->sun.getView());
    } else {
      showMessage(
          MSG_ERR,
          "Transparent surface index, X, does not exist. Cannot calculate PSSA."); // TODO format
                                                                                   // string
    }
  } else {
    showMessage(MSG_ERR, "Cannot calculate interior PSSAs without defining at least one "
                         "transparent surface index."); // TODO format string
  }
  return pssas;
}

int Penumbra::renderScene(unsigned surfaceIndex) {
 if (penumbra->checkSurface(surfaceIndex)) {
   penumbra->context.showRendering(surfaceIndex, penumbra->sun.getView());
   return PN_SUCCESS;
 } else {
   showMessage(MSG_ERR,
               "Surface index, X, does not exist. Cannot render scene."); // TODO format string
   return PN_FAILURE;
 }
}

int Penumbra::renderInteriorScene(std::vector<unsigned> transparentSurfaceIndices,
                                  std::vector<unsigned> interiorSurfaceIndices) {
 if (transparentSurfaceIndices.size() > 0) {
   if (penumbra->checkSurface(transparentSurfaceIndices[0])) {
     for (auto &transSurf : transparentSurfaceIndices) {
       if (!penumbra->checkSurface(transSurf)) {
         showMessage(
             MSG_ERR,
             "Transparent surface index, X, does not exist. Cannot calculate PSSA."); // TODO format string
       }
     }
     for (auto &intSurf : interiorSurfaceIndices) {
       if (penumbra->checkSurface(intSurf)) {
         penumbra->context.showInteriorRendering(transparentSurfaceIndices, intSurf, penumbra->sun.getView());
       } else {
         showMessage(
             MSG_ERR,
             "Interior surface index, X, does not exist. Cannot calculate PSSA."); // TODO format
                                                                                   // string
         return PN_FAILURE;
       }
     }
     return PN_SUCCESS;
   } else {
     showMessage(
         MSG_ERR,
         "Transparent surface index, X, does not exist. Cannot calculate PSSA."); // TODO format
                                                                                  // string
     return PN_FAILURE;
   }
 } else {
   showMessage(MSG_ERR, "Cannot calculate interior PSSAs without defining at least one "
                        "transparent surface index."); // TODO format string
   return PN_FAILURE;
 }
}

void Penumbra::setMessageCallback(PenumbraCallbackFunction callBackFunction, void *contextPtr) {
  penumbraCallbackFunction = callBackFunction;
  messageCallbackContextPtr = contextPtr;
}

PenumbraPrivate::PenumbraPrivate(unsigned size) : context(size) {}

PenumbraPrivate::~PenumbraPrivate() {}

void PenumbraPrivate::addSurface(const Surface &surface) { surfaces.push_back(*surface.surface); }

bool PenumbraPrivate::checkSurface(const unsigned index) { return index < surfaces.size(); }

} // namespace Pumbra
