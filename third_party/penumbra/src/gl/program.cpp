/* Copyright (c) 2017 Big Ladder Software LLC. All rights reserved.
 * See the LICENSE file for additional terms and conditions. */

// Penumbra
#include <gl/program.h>
#include <gl/shader.h>

namespace Pumbra {

GLProgram::GLProgram(const char *vertexSource, const char *fragmentSource) {
  program = glCreateProgram();
  GLShader vertex(GL_VERTEX_SHADER, vertexSource);
  glAttachShader(program, vertex.getInt());
  if (fragmentSource) {
    GLShader fragment(GL_FRAGMENT_SHADER, fragmentSource);
    glAttachShader(program, fragment.getInt());
  }
  glLinkProgram(program);
}

GLProgram::~GLProgram() {}

GLuint GLProgram::getInt() { return program; }

} // namespace Pumbra
