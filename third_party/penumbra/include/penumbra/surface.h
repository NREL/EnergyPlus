/* Copyright (c) 2017 Big Ladder Software LLC. All rights reserved.
* See the LICENSE file for additional terms and conditions. */

#ifndef SURFACE_H_
#define SURFACE_H_

// Standard
#include <vector>
#include <array>
#include <memory>

namespace Pumbra {

typedef std::vector<float> Polygon; // Implicit list of 3D coordinates

class SurfacePrivate;

class Surface {
  friend class PenumbraPrivate;
public:
  Surface();
  Surface(const Polygon& polygon);
  Surface(const Surface& srf);
  ~Surface();
  int setOuterPolygon(const Polygon& polygon); // Defined in counter-clockwise order when facing front
  int addHole(const Polygon& hole); // Defined in counter-clockwise order when facing front

private:
  std::shared_ptr<SurfacePrivate> surface;
};

}
#endif // SURFACE_H_
