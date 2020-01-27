/* Copyright (c) 2017 Big Ladder Software LLC. All rights reserved.
 * See the LICENSE file for additional terms and conditions. */

#ifndef MODEL_H_
#define MODEL_H_

// Vendor
#include <glad/glad.h>
#include <GLFW/glfw3.h>

// Standard
#include <vector>

namespace Pumbra {

class SurfaceBuffer {
public:
  SurfaceBuffer(GLuint begin = 0u, GLuint count = 0u, GLint index = -1);
  GLuint begin;
  GLuint count;
  GLint index;
};

class GLModel {
public:
  GLModel() : objectsSet(false) {};
  ~GLModel();
  void setVertices(const std::vector<float> &vertices);
  void setSurfaceBuffers(const std::vector<SurfaceBuffer> &surfaceBuffers);
  void drawSurface(SurfaceBuffer surfaceBuffer);
  void drawAll();
  void drawExcept(std::vector<SurfaceBuffer> hiddenSurfaces);
  void clearModel();
  std::vector<float> vertexArray;
  std::vector<SurfaceBuffer> surfaceBuffers;
  unsigned numPoints;
  static const int vertexSize = 3; // i.e., 3D
private:
  GLuint vbo, vao;
  bool objectsSet;
};

} // namespace Pumbra

#endif // MODEL_H_
