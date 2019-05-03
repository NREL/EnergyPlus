/* Copyright (c) 2017 Big Ladder Software LLC. All rights reserved.
* See the LICENSE file for additional terms and conditions. */

#ifndef CONTEXT_H_
#define CONTEXT_H_

// Vendor
#include <glad/glad.h>
#include <GLFW/glfw3.h>

// Standard
#include <vector>
#include <array>
#include <limits>

// Penumbra
#include <sun.h>
#include <gl/model.h>
#include <gl/shader.h>
#include <gl/program.h>

#define MAX_FLOAT std::numeric_limits<float>::max()

namespace Pumbra {

class Context {

public:
  Context(unsigned size=600);
  ~Context();
  void showRendering(GLint first, GLsizei count);
  void drawScene(GLint first, GLsizei count);
  void setModel(const std::vector<float>& vertices);
  void setScene(GLint first, GLsizei count, mat4x4 sunView);
  float calculatePSSF(GLint first, GLsizei count);
  void clearModel();

private:
  static const int vertexSize= 3;
  GLFWwindow* window;
  GLuint query, fbo, rbo;
  static const char* vertexShaderSource;
  static const char* fragmentShaderSource;
  unsigned size;
  GLModel model;
  bool modelSet;
  float pixelArea;
  float modelBox[8][4];
  mat4x4 projection, view, mvp;
  mat4x4 cameraView;
  GLint mvpLocation, vColLocation;
  bool isWireFrame = false;
  bool isCameraMode = false;
  float left, right, bottom, top, near_, far_;
  float viewScale = 1.;
  double prevPosX, prevPosY;
  float cameraRotAngleX = 0, cameraRotAngleY = 0;
  bool lbutton_down = true;

  void setMVP();
  void setCameraMVP();
  void calcCameraView();
  void toggleWireFrame();
  void toggleCameraMode();

};

}

#endif // CONTEXT_H_
