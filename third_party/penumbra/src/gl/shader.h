/* Copyright (c) 2017 Big Ladder Software LLC. All rights reserved.
* See the LICENSE file for additional terms and conditions. */

#ifndef SHADER_H_
#define SHADER_H_

// Vendor
#include <glad/glad.h>
#include <GLFW/glfw3.h>

namespace Pumbra {

class GLShader {
public:
  GLShader(GLenum type, const char* source);
  ~GLShader();
  GLuint getInt();
private:
  GLuint shader;
};

}

#endif // SHADER_H_
