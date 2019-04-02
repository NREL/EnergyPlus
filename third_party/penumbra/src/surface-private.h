/* Copyright (c) 2017 Big Ladder Software LLC. All rights reserved.
* See the LICENSE file for additional terms and conditions. */

#ifndef SURFACE_PRIVATE_H_
#define SURFACE_PRIVATE_H_

// Vendor
#include <tesselator.h>

// Standard
#include <vector>
#include <array>

namespace Pumbra {

struct TessData {
  TessData(
    const float* array,
    unsigned numVerts
  );
  std::vector<float> vertices;
  unsigned numVerts;
};

class SurfacePrivate{
public:
  SurfacePrivate();
  SurfacePrivate(const Polygon& polygon);
  Polygon polygon;
  std::vector<Polygon> holes;
  TessData tessellate();
  std::shared_ptr<float> vertPtr;
};

}
#endif // SURFACE_PRIVATE_H_
