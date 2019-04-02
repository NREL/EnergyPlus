/* Copyright (c) 2017 Big Ladder Software LLC. All rights reserved.
* See the LICENSE file for additional terms and conditions. */

#include "gtest/gtest.h"

#include <penumbra/penumbra.h>

TEST(PenumbraTest, PenumbraTests) {
  Pumbra::Polygon wallVerts =
    {
      0.f, 0.f, 0.f,
      1.f, 0.f, 0.f,
      1.f, 0.f, 1.f,
      0.f, 0.f, 1.f
    };
  Pumbra::Surface wall(wallVerts);
  Pumbra::Penumbra pumbra;

  unsigned wallId = pumbra.addSurface(wall);
  pumbra.setModel();
  pumbra.setSunPosition(3.14f, 0.0f);
  float wallPSSF = pumbra.calculatePSSF(wallId);

  EXPECT_NEAR(wallPSSF, 1.0, 0.0001);
}

int main(int argc, char **argv) {
  ::testing::InitGoogleTest(&argc, argv);

  return RUN_ALL_TESTS();
}