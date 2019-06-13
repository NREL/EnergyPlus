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
  return penumbra->surfaceCounter++;
}

int Penumbra::setModel() {
  if (penumbra->surfaces.size() > 0) {

    // Tesselate each surface into triangles
    unsigned nextStartingIndex = 0;
    unsigned surfNum = 0;
    for (auto &surface : penumbra->surfaces) {
      TessData tess = surface.tessellate();
      penumbra->surfaceBuffers.emplace_back(nextStartingIndex / TessData::vertexSize,
                                            tess.numVerts / TessData::vertexSize, surfNum);
      for (unsigned i = 0; i < tess.numVerts; ++i) {
        penumbra->model.push_back(tess.vertices[i]);
      }
      nextStartingIndex += tess.numVerts;
      ++surfNum;
    }
    penumbra->context.setModel(penumbra->model);
  } else {
    showMessage(MSG_WARN, "No surfaces added to Penumbra before calling setModel().");
  }
  return PN_SUCCESS;
}

int Penumbra::clearModel() {
  penumbra->surfaces.clear();
  penumbra->surfaceCounter = 0;
  penumbra->surfaceBuffers.clear();
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

float Penumbra::calculatePSSA(unsigned surfaceIndex) {
  if (penumbra->checkSurface(surfaceIndex)) {
    penumbra->context.setScene(penumbra->surfaceBuffers[surfaceIndex], penumbra->sun.getView());

    return penumbra->context.calculatePSSA(penumbra->surfaceBuffers[surfaceIndex]);
  } else {
    showMessage(MSG_ERR,
                "Surface index, X, does not exist. Cannot calculate PSSA."); // TODO format string
    return -1.f;
  }
}

std::map<unsigned, float>
Penumbra::calculateInteriorPSSAs(std::vector<unsigned> transparentSurfaceIndices,
                                 std::vector<unsigned> interiorSurfaceIndices) {
  std::map<unsigned, float> pssas;
  if (transparentSurfaceIndices.size() > 0) {
    if (penumbra->checkSurface(transparentSurfaceIndices[0])) {
      penumbra->context.setScene(penumbra->surfaceBuffers[transparentSurfaceIndices[0]],
                                 penumbra->sun.getView(), false);
      std::vector<SurfaceBuffer> transparentSurfaces;
      for (auto &transSurf : transparentSurfaceIndices) {
        if (penumbra->checkSurface(transSurf)) {
          transparentSurfaces.push_back(penumbra->surfaceBuffers[transSurf]);
        } else {
          showMessage(
              MSG_ERR,
              "Transparent surface index, X, does not exist. Cannot calculate PSSA."); // TODO
                                                                                       // format
                                                                                       // string
        }
      }
      std::vector<SurfaceBuffer> interiorSurfaces;
      for (auto &intSurf : interiorSurfaceIndices) {
        if (penumbra->checkSurface(intSurf)) {
          interiorSurfaces.push_back(penumbra->surfaceBuffers[intSurf]);
        } else {
          showMessage(
              MSG_ERR,
              "Interior surface index, X, does not exist. Cannot calculate PSSA."); // TODO format
                                                                                    // string
        }
      }
      pssas = penumbra->context.calculateInteriorPSSAs(transparentSurfaces, interiorSurfaces);
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
    penumbra->context.setScene(penumbra->surfaceBuffers[surfaceIndex], penumbra->sun.getView());
    penumbra->context.showRendering(penumbra->surfaceBuffers[surfaceIndex]);
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
      penumbra->context.setScene(penumbra->surfaceBuffers[transparentSurfaceIndices[0]],
                                 penumbra->sun.getView(), false);
      std::vector<SurfaceBuffer> transparentSurfaces;
      for (auto &transSurf : transparentSurfaceIndices) {
        if (penumbra->checkSurface(transSurf)) {
          transparentSurfaces.push_back(penumbra->surfaceBuffers[transSurf]);
        } else {
          showMessage(
              MSG_ERR,
              "Transparent surface index, X, does not exist. Cannot calculate PSSA."); // TODO
                                                                                       // format
                                                                                       // string
        }
      }
      for (auto &intSurf : interiorSurfaceIndices) {
        if (penumbra->checkSurface(intSurf)) {
          penumbra->context.showInteriorRendering(transparentSurfaces,
                                                  penumbra->surfaceBuffers[intSurf]);
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

PenumbraPrivate::PenumbraPrivate(unsigned size) : context(size), surfaceCounter(0) {}

PenumbraPrivate::~PenumbraPrivate() {}

void PenumbraPrivate::addSurface(const Surface &surface) { surfaces.push_back(*surface.surface); }

bool PenumbraPrivate::checkSurface(const unsigned index) { return index < surfaces.size(); }

} // namespace Pumbra
