/* Copyright (c) 2017 Big Ladder Software LLC. All rights reserved.
 * See the LICENSE file for additional terms and conditions. */

#ifndef PENUMBRA_PRIVATE_H_
#define PENUMBRA_PRIVATE_H_

// Standard
#include <memory>

// Penumbra
#include <penumbra/penumbra.h>
#include <penumbra/surface.h>
#include <surface-private.h>
#include <gl/context.h>

namespace Pumbra {

class PenumbraPrivate {

public:
  PenumbraPrivate(unsigned size = 512);
  ~PenumbraPrivate();

public:
  void addSurface(const Surface &surface);
  Context context;
  Sun sun;
  std::vector<float> model;
  std::vector<SurfacePrivate> surfaces;
  bool checkSurface(const unsigned index);
};

} // namespace Pumbra
#endif // PENUMBRA_PRIVATE_H_
