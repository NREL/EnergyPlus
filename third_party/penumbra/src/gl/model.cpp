/* Copyright (c) 2017 Big Ladder Software LLC. All rights reserved.
 * See the LICENSE file for additional terms and conditions. */

// Standard
#include <algorithm>

// Penumbra
#include <gl/model.h>

#ifdef __APPLE__
#define glGenVertexArraysX glGenVertexArraysAPPLE
#define glBindVertexArrayX glBindVertexArrayAPPLE
#define glDeleteVertexArraysX glDeleteVertexArraysAPPLE
#else
#define glGenVertexArraysX glGenVertexArrays
#define glBindVertexArrayX glBindVertexArray
#define glDeleteVertexArraysX glDeleteVertexArrays
#endif

namespace Pumbra {

SurfaceBuffer::SurfaceBuffer(GLuint begin, GLuint count, GLint index)
    : begin(begin), count(count), index(index) {}

GLModel::~GLModel() { clearModel(); }

void GLModel::clearModel() {
  if (objectsSet)
  {
    glDeleteVertexArraysX(1, &vao);
    glDeleteBuffers(1, &vbo);
  }
  surfaceBuffers.clear();
}

void GLModel::setVertices(const std::vector<float> &vertices) {

  vertexArray = vertices;
  numPoints = vertices.size() / vertexSize;
  // Set up vertex array object
  glGenVertexArraysX(1, &vao);
  glBindVertexArrayX(vao);

  // Set up array buffer to store vertex information
  glGenBuffers(1, &vbo);
  glBindBuffer(GL_ARRAY_BUFFER, vbo);
  glBufferData(GL_ARRAY_BUFFER, sizeof(float) * vertices.size(), &vertices[0], GL_STATIC_DRAW);

  // Set drawing pointers for current vertex buffer
  glEnableVertexAttribArray(0);
  glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, sizeof(float) * 3, (void *)0);

  objectsSet = true;
}

void GLModel::setSurfaceBuffers(const std::vector<SurfaceBuffer> &surfaceBuffers) {
  this->surfaceBuffers = surfaceBuffers;
}

void GLModel::drawSurface(SurfaceBuffer surfaceBuffer) {
  glDrawArrays(GL_TRIANGLES, surfaceBuffer.begin, surfaceBuffer.count);
}

void GLModel::drawAll() { glDrawArrays(GL_TRIANGLES, 0, numPoints); }

void GLModel::drawExcept(std::vector<SurfaceBuffer> hiddenSurfaces) {

  if (hiddenSurfaces.size() == 0) { // draw all if no hidden surfaces
    drawAll();
    return;
  }

  // Sort vector
  std::sort(
      hiddenSurfaces.begin(), hiddenSurfaces.end(),
      [](const SurfaceBuffer &a, const SurfaceBuffer &b) -> bool { return a.begin > b.begin; });

  // Begin (if first hidden surface isn't first surface)
  if (hiddenSurfaces[0].begin != 0u) {
    glDrawArrays(GL_TRIANGLES, 0, hiddenSurfaces[0].begin);
  }

  GLuint nextBegin = hiddenSurfaces[0].begin + hiddenSurfaces[0].count;

  // Loop through all exceptions
  for (std::size_t i = 1; i < hiddenSurfaces.size(); ++i) {
    if (nextBegin == numPoints) {
      // This is the last surface
      return;
    }
    if (nextBegin == hiddenSurfaces[i].begin) {
      // Next surface is also hidden
      nextBegin = hiddenSurfaces[i].begin + hiddenSurfaces[i].count;
      break;
    }
    glDrawArrays(GL_TRIANGLES, nextBegin, hiddenSurfaces[i + 1].begin - 1);
  }

  if (nextBegin < numPoints) {
    glDrawArrays(GL_TRIANGLES, nextBegin, numPoints - nextBegin);
  }
}

} // namespace Pumbra
