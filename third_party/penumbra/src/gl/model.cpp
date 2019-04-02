/* Copyright (c) 2017 Big Ladder Software LLC. All rights reserved.
* See the LICENSE file for additional terms and conditions. */

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
GLModel::~GLModel() {
  clearModel();
}

void GLModel::clearModel() {
  glDeleteVertexArraysX(1, &vao);
  glDeleteBuffers(1, &vbo);
}

void GLModel::setVertices(const std::vector<float>& vertices) {

  vertexArray = vertices;
  numVerts = vertices.size();
  // Set up vertex array object
  glGenVertexArraysX(1, &vao);
  glBindVertexArrayX(vao);

  // Set up array buffer to store vertex information
  glGenBuffers(1, &vbo);
  glBindBuffer(GL_ARRAY_BUFFER, vbo);
  glBufferData(GL_ARRAY_BUFFER, sizeof(float)*vertices.size(), &vertices[0], GL_STATIC_DRAW);

  // Set drawing pointers for current vertex buffer
  glEnableVertexAttribArray(0);
  glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, sizeof(float) * 3, (void*)0);

}

void GLModel::draw(GLint first, GLsizei count) {
  glBindVertexArrayX(vao);
  glDrawArrays(GL_TRIANGLES, first, count);
}

}
