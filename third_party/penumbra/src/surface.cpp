/* Copyright (c) 2017 Big Ladder Software LLC. All rights reserved.
* See the LICENSE file for additional terms and conditions. */

// Standard
#include <vector>
#include <array>

// Penumbra
#include <penumbra/surface.h>
#include <surface-private.h>
#include <error.h>

namespace Pumbra {

/*
void* stdAlloc(void* userData, unsigned size)
{
	int* allocated = ( int*)userData;
	TESS_NOTUSED(userData);
	*allocated += (int)size;
	return malloc(size);
}

void stdFree(void* userData, void* ptr)
{
	TESS_NOTUSED(userData);
	free(ptr);
}
*/

TessData::TessData(
  const float* array,
  unsigned numVerts
) :
numVerts(numVerts)
{
  vertices.insert(vertices.end(), (const float*)array, (const float*)array + numVerts);
}

Surface::Surface()
{
  surface = std::make_shared<SurfacePrivate>();
}

Surface::Surface(const Polygon& polygon)
{
  surface = std::make_shared<SurfacePrivate>(polygon);
}

Surface::Surface(const Surface& srf)
{
  surface = srf.surface;
}

Surface::~Surface(){}

int Surface::setOuterPolygon(const Polygon& polygon)
{
  surface->polygon = polygon;
	return 0;
}

int Surface::addHole(const Polygon& hole)
{
  surface->holes.push_back(hole);
	return 0;
}

SurfacePrivate::SurfacePrivate(){}

SurfacePrivate::SurfacePrivate(const Polygon& polygon) :
  polygon(polygon)
{}

TessData SurfacePrivate::tessellate()
{
  TESStesselator* tess = 0;
  tess = tessNewTess(nullptr);

  if (!tess) {
    showMessage(MSG_ERR, "Unable to create tessellator.");
  }

  int polySize = 3; // making triangles
  int vertexSize = 3; // i.e., 3D

  // Add primary polygon
  tessAddContour(tess, polySize, &polygon[0], sizeof(float) * vertexSize, (int)polygon.size() / vertexSize);

  // Add holes
  for (auto& hole : holes) {
    tessAddContour(tess, polySize, &hole[0], sizeof(float) * vertexSize, (int)hole.size() / vertexSize);
  }

  if (!tessTesselate(tess, TESS_WINDING_ODD, TESS_POLYGONS, polySize, vertexSize, nullptr)) {
    showMessage(MSG_ERR, "Unable to tessellate surface.");
  }

  // For now convert to glDrawArrays() style of vertices, sometime may change to glDrawElements (with element buffers)
  std::vector<float> vertexArray;
  const TESSreal* verts = tessGetVertices(tess);
  const int nelems = tessGetElementCount(tess);
  const TESSindex* elems = tessGetElements(tess);
  for (int i = 0; i < nelems * polySize; ++i) {
    const int vert = *(elems+i);
    for (int j = 0; j < vertexSize; ++j) {
      vertexArray.push_back(verts[vert * vertexSize + j]);
    }
  }

  TessData data(&vertexArray[0],vertexArray.size());

  tessDeleteTess(tess);

  return data;

}

}
