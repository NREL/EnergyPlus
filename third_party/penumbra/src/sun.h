/* Copyright (c) 2017 Big Ladder Software LLC. All rights reserved.
* See the LICENSE file for additional terms and conditions. */

#ifndef SUN_H_
#define SUN_H_

#include <cmath>
#include <linmath.h>

namespace Pumbra {

//typedef std::array<std::array<float, 4>, 4> mat4x4_std;
typedef float (*mat4x4_ptr)[4];

class Sun {
public:
  Sun(float azm, float alt);
  Sun();

// Member functions
public:
  mat4x4_ptr getView();
  void setView(float azm, float alt);
  //void setView(mat4x4 view);
  float getAzimuth();
  float getAltitude();

private:
  void setView();
  void setAzimuth(float azm);
  void setAltitude(float alt);

// Data Members
private:
  mat4x4 view;
  float azimuth, altitude;
  bool posSet;
};

}

#endif // SUN_H_
