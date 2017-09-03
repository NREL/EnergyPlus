/* Copyright (c) 2012-2017 Big Ladder Software LLC. All rights reserved.
* See the LICENSE file for additional terms and conditions. */

#ifndef GroundOutput_HPP
#define GroundOutput_HPP

#include "Foundation.hpp"

namespace Kiva {

class GroundOutput {
public:

  // Outputs
  enum OutputType
  {
    OT_FLUX,
    OT_TEMP,
    OT_EFF_TEMP,
    OT_RATE,
    OT_CONV
  };

  typedef std::map<Surface::SurfaceType, std::vector<OutputType>> OutputMap;

  GroundOutput(OutputMap oM) : outputMap(oM)
  {};

  GroundOutput() {};

  OutputMap outputMap;

  std::map<std::pair<Surface::SurfaceType, OutputType>,double> outputValues;

};

}

#endif // GroundOutput_HPP
