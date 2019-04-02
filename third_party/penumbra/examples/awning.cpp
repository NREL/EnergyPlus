/* Copyright (c) 2017 Big Ladder Software and Chip Barnaby. All rights reserved.
* See the LICENSE file for additional terms and conditions. */

// Standard
#include <iostream>

// Penumbra
#include <penumbra/penumbra.h>

void errorCallback(
  const int messageType,
  const std::string message,
  void* /*contextPtr*/
)
{
  if (messageType == Pumbra::MSG_INFO) {
    std::cout << "  NOTE: " << message << std::endl;
  } else if (messageType == Pumbra::MSG_WARN) {
    std::cout << "  WARNING: " << message << std::endl;
  } else if (messageType == Pumbra::MSG_ERR) {
    std::cout << "  ERROR: " << message << std::endl;
    exit(EXIT_FAILURE);
  }
}

int main(void)
{
    Pumbra::Polygon wallVerts =
    {
      0.f, 0.f, 0.f,
      1.f, 0.f, 0.f,
      1.f, 0.f, 1.f,
      0.f, 0.f, 1.f
    };

    Pumbra::Polygon windowVerts =
    {
      0.25f, 0.f, 0.25f,
      0.75f, 0.f, 0.25f,
      0.75f, 0.f, 0.5f,
      0.25f, 0.f, 0.5f
    };

    Pumbra::Polygon awningVerts =
    {
      0.25f, 0.f, 0.5f,
      0.75f, 0.f, 0.5f,
      0.75f, -0.5f, 0.5f,
      0.25f, -0.5f, 0.5f
    };

    Pumbra::Surface wall(wallVerts);
    wall.addHole(windowVerts);

    Pumbra::Surface window(windowVerts);
    Pumbra::Surface awning(awningVerts);

    Pumbra::Penumbra pumbra(errorCallback);

    unsigned wallId = pumbra.addSurface(wall);
    unsigned windowId = pumbra.addSurface(window);
    unsigned awningId = pumbra.addSurface(awning);

    pumbra.setModel();
    pumbra.setSunPosition(2.50f,0.3f);
    //pumbra.setSunPosition(3.14f, 0.0f);
    pumbra.renderScene(wallId);
    float wallPSSF = pumbra.calculatePSSF(wallId);

    std::cout << "Wall PSSF: " << wallPSSF << std::endl;

    pumbra.renderScene(windowId);
    float windowPSSF = pumbra.calculatePSSF(windowId);

    std::cout << "Window PSSF: " << windowPSSF << std::endl;

    pumbra.clearModel();

    Pumbra::Polygon finVerts =
    {
      0.75f, -0.25f, 0.5f,
      0.75f, -0.25f, 0.25f,
      0.75f, 0.0f, 0.25f,
      0.75f, 0.0f, 0.5f
    };

    Pumbra::Surface fin(finVerts);

    windowId = pumbra.addSurface(window);
    awningId = pumbra.addSurface(awning);
    /*unsigned finID = */pumbra.addSurface(fin);

    pumbra.setModel();

    pumbra.renderScene(windowId);
    windowPSSF = pumbra.calculatePSSF(windowId);

    std::cout << "Window PSSF with fin: " << windowPSSF << std::endl;

    return 0;
}
