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
#include <map>

// Penumbra
#include <sun.h>
#include <gl/model.h>
#include <gl/shader.h>
#include <gl/program.h>

#define MAX_FLOAT std::numeric_limits<float>::max()

namespace Pumbra {

class Context {

public:
  Context(unsigned size = 512);
  ~Context();
  void showRendering(const SurfaceBuffer &surfaceBuffer);
  void setModel(const std::vector<float> &vertices);
  void setScene(const SurfaceBuffer &surfaceBuffer, mat4x4 sunView, bool clipFar = true);
  float calculatePSSA(const SurfaceBuffer &surfaceBuffer);
  std::map<unsigned, float>
  calculateInteriorPSSAs(const std::vector<SurfaceBuffer> &hiddenSurfaces,
                         const std::vector<SurfaceBuffer> &interiorSurfaces);
  void showInteriorRendering(const std::vector<SurfaceBuffer> &hiddenSurfaces,
                             const SurfaceBuffer &interiorSurface);
  void clearModel();

private:
  GLFWwindow *window;
  GLuint query, fbo, rbo;
  static const char *vertexShaderSource;
  static const char *fragmentShaderSource;
  unsigned size;
  GLModel model;
  bool modelSet;
  float pixelArea;
  float modelBox[8][4];
  mat4x4 projection, view, mvp;
  mat4x4 cameraView;
  GLint mvpLocation, vColLocation;
  bool isWireFrame;
  bool isCameraMode;
  float left, right, bottom, top, near_, far_;
  float viewScale;
  double prevPosX, prevPosY;
  float cameraRotAngleX, cameraRotAngleY;
  bool lbutton_down;
  bool isRenderMode;

  void drawModel();
  void drawExcept(const std::vector<SurfaceBuffer> &hiddenSurfaces);
  void setMVP();
  void setCameraMVP();
  void calcCameraView();
  void toggleWireFrame();
  void toggleCameraMode();
  void initOffScreenMode();
  void initRenderMode();
};

} // namespace Pumbra

#endif // CONTEXT_H_
