/* Copyright (c) 2017 Big Ladder Software LLC. All rights reserved.
 * See the LICENSE file for additional terms and conditions. */

#define _USE_MATH_DEFINES
#include <cmath>

#include "gtest/gtest.h"

#include <penumbra/penumbra.h>

TEST(PenumbraTest, azimuth) {
  Pumbra::Polygon wallVerts = {0.f, 0.f, 0.f, 1.f, 0.f, 0.f, 1.f, 0.f, 1.f, 0.f, 0.f, 1.f};
  Pumbra::Surface wall(wallVerts);
  Pumbra::Penumbra pumbra;

  unsigned wallId = pumbra.addSurface(wall);
  pumbra.setModel();

  // Loop azimuth around the surface (with zero altitude).
  for (float azm = 0.0f; azm <= 2 * M_PI; azm += M_PI_4) {
    pumbra.setSunPosition(azm, 0.0f);
    float wallPSSA = pumbra.calculatePSSA(wallId);
    EXPECT_NEAR(wallPSSA, std::abs(cos(azm)), 0.0001) << "azm evaluates to " << azm;
    // pumbra.renderScene(wallId);
  }
}

TEST(PenumbraTest, interior) {
  float const r_W = 2., r_D = 1., r_H = 1.; // Overall dimensions
  float const w_W = 1., w_H = .5;           // Window dimensions

  Pumbra::Polygon wallFrontVerts = {
      -r_W / 2.f, r_D / 2.f, -r_H / 2.f, r_W / 2.f,  r_D / 2.f, -r_H / 2.f,
      r_W / 2.f,  r_D / 2.f, r_H / 2.f,  -r_W / 2.f, r_D / 2.f, r_H / 2.f,
  };

  Pumbra::Polygon wallFrontWindowVerts = {
      -w_W / 2.f, r_D / 2.f, -w_H / 2.f, w_W / 2.f,  r_D / 2.f, -w_H / 2.f,
      w_W / 2.f,  r_D / 2.f, w_H / 2.f,  -w_W / 2.f, r_D / 2.f, w_H / 2.f,
  };

  Pumbra::Polygon wallBackVerts = {
      r_W / 2.f,  -r_D / 2.f, -r_H / 2.f, -r_W / 2.f, -r_D / 2.f, -r_H / 2.f,
      -r_W / 2.f, -r_D / 2.f, r_H / 2.f,  r_W / 2.f,  -r_D / 2.f, r_H / 2.f,
  };

  /*	Pumbra::Polygon wallBackVerts =	//Can use this to check changing norm of back wall.
  {
            -r_W / 2.f, -r_D / 2.f, -r_H / 2.f,
            r_W / 2.f, -r_D / 2.f, -r_H / 2.f,
            r_W / 2.f, -r_D / 2.f, r_H / 2.f,
            -r_W / 2.f, -r_D / 2.f, r_H / 2.f,
          };
  */
  Pumbra::Polygon roofVerts = {
      -r_W / 2.f, r_D / 2.f,  r_H / 2.f, r_W / 2.f,  r_D / 2.f,  r_H / 2.f,
      r_W / 2.f,  -r_D / 2.f, r_H / 2.f, -r_W / 2.f, -r_D / 2.f, r_H / 2.f,
  };

  Pumbra::Polygon floorVerts = {
      -r_W / 2.f, r_D / 2.f,  -r_H / 2.f, r_W / 2.f,  r_D / 2.f,  -r_H / 2.f,
      r_W / 2.f,  -r_D / 2.f, -r_H / 2.f, -r_W / 2.f, -r_D / 2.f, -r_H / 2.f,
  };

  Pumbra::Polygon sideWallLeftVerts = {
      -r_W / 2.f, r_D / 2.f,  -r_H / 2.f, -r_W / 2.f, r_D / 2.f,  r_H / 2.f,
      -r_W / 2.f, -r_D / 2.f, r_H / 2.f,  -r_W / 2.f, -r_D / 2.f, -r_H / 2.f,
  };

  Pumbra::Polygon sideWallRightVerts = {
      r_W / 2.f, r_D / 2.f,  -r_H / 2.f, r_W / 2.f, r_D / 2.f,  r_H / 2.f,
      r_W / 2.f, -r_D / 2.f, r_H / 2.f,  r_W / 2.f, -r_D / 2.f, -r_H / 2.f,
  };

  Pumbra::Surface wallFront(wallFrontVerts);
  wallFront.addHole(wallFrontWindowVerts);

  Pumbra::Surface window(wallFrontWindowVerts);
  Pumbra::Surface wallBack(wallBackVerts);
  Pumbra::Surface roof(roofVerts);
  Pumbra::Surface floor(floorVerts);
  Pumbra::Surface sideWallLeft(sideWallLeftVerts);
  Pumbra::Surface sideWallRight(sideWallRightVerts);

  Pumbra::Penumbra pumbra2;

  unsigned wallFrontId = pumbra2.addSurface(wallFront);
  unsigned windowId = pumbra2.addSurface(window);
  unsigned wallBackId = pumbra2.addSurface(wallBack);
  /* unsigned roofId = */ pumbra2.addSurface(roof);
  unsigned floorId = pumbra2.addSurface(floor);
  unsigned sideWallLeftId = pumbra2.addSurface(sideWallLeft);
  unsigned sideWallRightId = pumbra2.addSurface(sideWallRight);

  pumbra2.setModel();
  pumbra2.setSunPosition(0.0f, 0.f);
  float wallPSSA2 = pumbra2.calculatePSSA(wallFrontId);
  EXPECT_NEAR(wallPSSA2, r_W * r_H - w_W * w_H, 0.0001);
  // pumbra2.renderScene(wallFrontId);

  float backWallInteriorPSSA = pumbra2.calculateInteriorPSSAs({windowId}, {wallBackId})[wallBackId];
  EXPECT_NEAR(backWallInteriorPSSA, w_W * w_H, 0.0001);
  // pumbra2.renderInteriorScene({windowId},{wallBackId});

  pumbra2.setSunPosition(3.1415f, 0.f);
  float wallPSSA3 = pumbra2.calculatePSSA(wallFrontId);
  EXPECT_NEAR(wallPSSA3, 0, 0.0001); // WallFront should be blocked by back wall.
  // pumbra2.renderScene(wallFrontId);

  pumbra2.setSunPosition(0.5f, 0.5f);
  std::map<unsigned, float> intPSSAs = pumbra2.calculateInteriorPSSAs(
      {windowId}, {wallBackId, floorId, sideWallLeftId, sideWallRightId});
  backWallInteriorPSSA = intPSSAs[wallBackId];
  float floorInteriorPSSA = intPSSAs[floorId];
  float leftWallInteriorPSSA = intPSSAs[sideWallLeftId];
  float rightWallInteriorPSSA = intPSSAs[sideWallRightId];
  EXPECT_GT(backWallInteriorPSSA, 0);
  EXPECT_GT(floorInteriorPSSA, 0);
  EXPECT_GT(leftWallInteriorPSSA, 0);
  EXPECT_EQ(rightWallInteriorPSSA, 0);
  // pumbra2.renderInteriorScene({windowId},{wallBackId, floorId, sideWallLeftId, sideWallRightId});
}

int main(int argc, char **argv) {
  ::testing::InitGoogleTest(&argc, argv);

  return RUN_ALL_TESTS();
}