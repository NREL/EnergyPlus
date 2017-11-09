/* Copyright (c) 2012-2017 Big Ladder Software LLC. All rights reserved.
* See the LICENSE file for additional terms and conditions. */

#ifndef BASE_FIXTURE_HPP_
#define BASE_FIXTURE_HPP_

#include <gtest/gtest.h>
#include "Ground.hpp"

using namespace Kiva;

class BaseFixture : public testing::Test {
protected:

  void init(){
    ground = std::make_shared<Ground>(fnd,outputMap);
    ground->buildDomain();
    Foundation::NumericalScheme tempNS = fnd.numericalScheme;
    fnd.numericalScheme = Foundation::NS_STEADY_STATE;
    ground->calculate(bcs);
    fnd.numericalScheme = tempNS;
  }

  double calcQ(){
    init();
    ground->calculateSurfaceAverages();
    return ground->getSurfaceAverageValue({Surface::ST_SLAB_CORE,GroundOutput::OT_RATE});
  }

  std::shared_ptr<Ground> ground;
  std::map<Surface::SurfaceType, std::vector<GroundOutput::OutputType>> outputMap;
  BoundaryConditions bcs;
  Foundation fnd;
};

#endif /* BASE_FIXTURE_HPP_ */
