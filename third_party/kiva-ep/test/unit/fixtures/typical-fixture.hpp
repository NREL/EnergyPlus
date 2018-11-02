/* Copyright (c) 2012-2018 Big Ladder Software LLC. All rights reserved.
 * See the LICENSE file for additional terms and conditions. */

#ifndef TYPICAL_FIXTURE_HPP_
#define TYPICAL_FIXTURE_HPP_

#include "base-fixture.hpp"

using namespace Kiva;

class TypicalFixture : public BaseFixture {
protected:
  void SetUp() {

    fnd.reductionStrategy = Foundation::RS_BOUNDARY;

    Material concrete(1.95, 2400.0, 900.0);

    Layer tempLayer;
    tempLayer.thickness = 0.10;
    tempLayer.material = concrete;

    fnd.slab.interior.emissivity = 0.8;
    fnd.slab.layers.push_back(tempLayer);

    tempLayer.thickness = 0.2;
    tempLayer.material = concrete;

    fnd.wall.layers.push_back(tempLayer);

    fnd.wall.heightAboveGrade = 0.1;
    fnd.wall.depthBelowSlab = 0.2;
    fnd.wall.interior.emissivity = 0.8;
    fnd.wall.exterior.emissivity = 0.8;
    fnd.wall.interior.absorptivity = 0.8;
    fnd.wall.exterior.absorptivity = 0.8;

    fnd.foundationDepth = 0.0;
    fnd.numericalScheme = Foundation::NS_ADI;
  }

  double calcQ() {
    init();
    bcs.localWindSpeed = 0;
    bcs.outdoorTemp = 283.15;
    bcs.indoorTemp = 310.15;
    bcs.slabRadiantTemp = 310.15;
    bcs.wallRadiantTemp = 310.15;
    ground->calculate(bcs, 3600.0);
    ground->calculateSurfaceAverages();
    return ground->getSurfaceAverageValue({Surface::ST_SLAB_CORE, GroundOutput::OT_RATE});
  }
};

#endif /* TYPICAL_FIXTURE_HPP_ */
