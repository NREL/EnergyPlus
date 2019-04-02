/* Copyright (c) 2017 Big Ladder Software LLC. All rights reserved.
* See the LICENSE file for additional terms and conditions. */

#include <iostream>

// Penumbra
#include <gl/context.h>
#include <error.h>

namespace Pumbra {

const char* Context::vertexShaderSource =
R"src(
  #version 120
  uniform mat4 MVP;
  uniform vec3 vCol;
  attribute vec3 vPos;
  varying vec3 color;
  void main()
  {
    gl_Position = MVP * vec4(vPos, 1.0);
    color = vCol;
  }
)src";

const char* Context::fragmentShaderSource =
R"src(
  #version 120
  varying vec3 color;
  void main()
  {
    gl_FragColor = vec4(color, 1.0);
  }
)src";

static void glErrorCallback(int, const char* description)
{
  showMessage(MSG_INFO, description);
}

Context::Context(unsigned size) :
  size(size),
  modelSet(false)
{

  glfwSetErrorCallback(glErrorCallback);

  if (!glfwInit()) {
    showMessage(MSG_ERR, "Unable to initialize GLFW.");
  }

  glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 2);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 1);
  glfwWindowHint(GLFW_VISIBLE, GL_FALSE);
  window = glfwCreateWindow(1, 1, "Penumbra", NULL, NULL);
  glfwMakeContextCurrent(window);
  if (!window) {
    showMessage(MSG_ERR, "Unable to create OpenGL context. OpenGL 2.1 is required to perform shading calculations.");
  }

  // OpenGL extension loader
  if (!gladLoadGLLoader((GLADloadproc)glfwGetProcAddress)) {
    showMessage(MSG_ERR, "Failed to load OpenGL extensions.");
  }

  if (!glfwExtensionSupported("GL_ARB_vertex_array_object") && !glfwExtensionSupported("GL_APPLE_vertex_array_object")) {
    showMessage(MSG_ERR, "Your version of OpenGL does not support vertex array objects.");
  }
  if (!glfwExtensionSupported("GL_EXT_framebuffer_object")) {
    showMessage(MSG_ERR, "Your version of OpenGL does not support framebuffer objects.");
  }

  //std::string glVersion = (char*)glGetString(GL_VERSION);
  //showMessage(MSG_INFO, "OpenGL version = " + glVersion);

  glfwSwapInterval(1);

  glEnable(GL_DEPTH_TEST);

  // Shader program
  GLProgram program(vertexShaderSource, fragmentShaderSource);

  glBindAttribLocation(program.getInt(), 0, "vPos");
  mvpLocation = glGetUniformLocation(program.getInt(), "MVP");
  vColLocation = glGetUniformLocation(program.getInt(), "vCol");

  glGenFramebuffersEXT(1, &fbo);
  glGenRenderbuffersEXT(1, &rbo);

}

Context::~Context(){
  glDeleteFramebuffersEXT(1, &fbo);
  glDeleteRenderbuffersEXT(1, &rbo);
  glfwDestroyWindow(window);
}

void Context::clearModel(){
  model.clearModel();
  modelSet = false;
}

void Context::setModel(const std::vector<float>& vertices) {

  // set model vertices
  model.setVertices(vertices);

  float bLeft = MAX_FLOAT, bBottom = MAX_FLOAT, bFront = MAX_FLOAT;
  float bRight = -MAX_FLOAT, bTop = -MAX_FLOAT, bBack = -MAX_FLOAT;


  // calculate bounding box
  for(int i = 0; i < (int)vertices.size(); i += 3) {
    float x = vertices[i];
    float y = vertices[i+1];
    float z = vertices[i+2];
    bLeft = std::min(x, bLeft);
    bRight = std::max(x, bRight);
    bFront = std::min(y, bFront);
    bBack = std::max(y, bBack);
    bBottom = std::min(z, bBottom);
    bTop = std::max(z, bTop);
  }

  float tempBox[8][4] =
  {
    { bLeft, bFront, bBottom, 0.0 },
    { bLeft, bFront, bTop, 0.0 },
    { bLeft, bBack, bBottom, 0.0 },
    { bLeft, bBack, bTop, 0.0 },
    { bRight, bFront, bBottom, 0.0 },
    { bRight, bFront, bTop, 0.0 },
    { bRight, bBack, bBottom, 0.0 },
    { bRight, bBack, bTop, 0.0 }
  };

  for (std::size_t i = 0; i < 8; i++) {
    for (std::size_t j = 0; j < 4; j++) {
      modelBox[i][j] = tempBox[i][j];
    }
  }

  modelSet = true;

}

void Context::setScene(GLint first, GLsizei count, mat4x4 sunView) {

  if (!modelSet) {
    showMessage(MSG_ERR, "Model has not been set. Cannot set OpenGL scene.");
  }

  for (std::size_t i = 0; i < 4; i++) {
    for (std::size_t j = 0; j < 4; j++) {
      view[j][i] = sunView[j][i];
    }
  }

  // calculate clipping planes in rendered coorinates
  float left(MAX_FLOAT);
  float right(-MAX_FLOAT);
  float bottom(MAX_FLOAT);
  float top(-MAX_FLOAT);
  float near_(-MAX_FLOAT);
  float far_(MAX_FLOAT);

  const int vertexSize = 3;

  for (int i = first*vertexSize; i < first*vertexSize + count*vertexSize; i += vertexSize) {
    vec4 point = {
      model.vertexArray[i],
      model.vertexArray[i + 1],
      model.vertexArray[i + 2],
      0 };
    vec4 trans;
    mat4x4_mul_vec4(trans, view, point);
    left = std::min(trans[0], left);
    right = std::max(trans[0], right);
    bottom = std::min(trans[1], bottom);
    top = std::max(trans[1], top);
    //near_ = min(trans[2], near_);
    far_ = std::min(trans[2], far_);
  }

  // Use model box to determine near clipping plane
  for (std::size_t i = 0; i < 8; i++) {
    vec4 trans;
    mat4x4_mul_vec4(trans, view, modelBox[i]);
    near_ = std::max(trans[2], near_);
  }

  // account for camera position
  near_ -= 1.f;
  far_ -= 1.f;

  // Grow horizontal extents of view by one pixel on each side
  float deltaX = (right - left)/size;
  left -= deltaX;
  right += deltaX;

  // Grow vertical extents of view by one pixel on each side
  float deltaY = (top - bottom)/size;
  bottom -= deltaY;
  top += deltaY;

  // calculate pixel area (A[i]*cos(theta) for each pixel of the surface)
  // multiplies by the number of pixels to get projected sunlit surface fraction
  pixelArea = (right - left)*(top - bottom) / (size*size);

  // set projection matrix
  mat4x4_ortho(projection, left, right, bottom, top, -near_, -far_);
  setMVP();
}


void Context::showRendering(GLint first, GLsizei count)
{
  glfwSetWindowSize(window, size, size);
  glfwShowWindow(window);

  while (!glfwWindowShouldClose(window))
  {
    drawScene(first, count);
    glfwSwapBuffers(window);
    glfwPollEvents();
  }

  glfwSetWindowShouldClose(window, 0);
  glfwHideWindow(window);
}

void Context::drawScene(GLint first, GLsizei count)
{
  const int vertexSize = 3;
  glViewport(0, 0, size, size);
  glClearColor(0.0f, 0.0f, 0.0f, 1.0f);
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  glDepthFunc(GL_LESS);
  glUniform3f(vColLocation, 0.5f, 0.5f, 0.5f);
  model.draw(0, model.numVerts/vertexSize);
  glDepthFunc(GL_EQUAL);
  glUniform3f(vColLocation, 1.f, 1.f, 1.f);
  model.draw(first, count);
}

void Context::setMVP()
{
  mat4x4_mul(mvp, projection, view);
  glUniformMatrix4fv(mvpLocation, 1, GL_FALSE, (const GLfloat*)mvp);
}

float Context::calculatePSSF(GLint first, GLsizei count) {
  glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, fbo);
  glDrawBuffer(GL_NONE);
  glReadBuffer(GL_NONE);
  glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, rbo);
  glRenderbufferStorageEXT(GL_RENDERBUFFER_EXT, GL_DEPTH_COMPONENT24, size, size);
  glFramebufferRenderbufferEXT(GL_FRAMEBUFFER_EXT, GL_DEPTH_ATTACHMENT_EXT, GL_RENDERBUFFER_EXT, rbo);

  GLenum status = glCheckFramebufferStatusEXT(GL_FRAMEBUFFER_EXT);
  if (status != GL_FRAMEBUFFER_COMPLETE_EXT) {
    std::string reason;
    switch(status) {
      case GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT_EXT: {
        reason = "Incomplete attachment.";
      } break;
      case GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT_EXT: {
        reason = "Incomplete or missing attachment.";
      } break;
      case GL_FRAMEBUFFER_INCOMPLETE_FORMATS_EXT: {
        reason = "Incomplete formats.";
      } break;
      case GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER_EXT: {
        reason = "Incomplete draw buffer.";
      } break;
      case GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER_EXT: {
        reason = "Incomplete read buffer.";
      } break;
      case GL_FRAMEBUFFER_UNSUPPORTED_EXT: {
        reason = "Framebuffers are not supported.";
      } break;
      default: {
        reason = "Reason unknown.";
      }
    }
    showMessage(MSG_ERR, "Unable to create framebuffer. " + reason);

  }

  glGenQueries(1, &query);
  glColorMask(GL_FALSE, GL_FALSE, GL_FALSE, GL_FALSE);

  const int vertexSize = 3;
  glUniform3f(vColLocation, 0.5f, 0.5f, 0.5f); // TODO: Change shader to ignore color in this case
  glViewport(0, 0, size, size);
  glClearColor(0.0f, 0.0f, 0.0f, 1.0f);
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  glDepthFunc(GL_LESS);
  model.draw(0, model.numVerts/vertexSize);
  glDepthFunc(GL_EQUAL);
  glBeginQuery(GL_SAMPLES_PASSED, query);
  model.draw(first, count);
  glEndQuery(GL_SAMPLES_PASSED);

  // wait until the result is available
  GLint ready(0);
  while (!ready) {
    glGetQueryObjectiv(query, GL_QUERY_RESULT_AVAILABLE, &ready);
  }

  // retrieve result
  GLint pixelCount;
  glGetQueryObjectiv(query, GL_QUERY_RESULT, &pixelCount);

  // reset to default framebuffer
  glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0);
  glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, 0);
  glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE);
  glDeleteQueries(1, &query);

  return pixelCount*pixelArea;

}

}
