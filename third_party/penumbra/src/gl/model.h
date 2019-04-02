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

class GLModel {
public:
  ~GLModel();
  void setVertices(const std::vector<float>& vertices);
  void draw(GLint first, GLsizei count);
  void clearModel();
  std::vector<float> vertexArray;
  unsigned numVerts;
private:
  GLuint vbo, vao;
};

}

#endif // MODEL_H_
