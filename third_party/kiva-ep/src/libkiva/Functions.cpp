/* Copyright (c) 2012-2017 Big Ladder Software LLC. All rights reserved.
* See the LICENSE file for additional terms and conditions. */

#ifndef Functions_CPP
#define Functions_CPP

#include "Functions.hpp"

namespace Kiva {

static const double EPSILON = 1E-7;

bool isLessThan(double first, double second)
{
  if(first - second < -EPSILON)
    return true;
  else
    return false;
}

bool isLessOrEqual(double first, double second)
{
  if(first - second < EPSILON)
    return true;
  else
    return false;
}

bool isEqual(double first, double second)
{
  if(fabs(first - second) < EPSILON)
    return true;
  else
    return false;
}

bool isEqual(double first, double second, double epsilon)
{
  if(fabs(first - second) < epsilon)
    return true;
  else
    return false;
}

bool isGreaterThan(double first, double second)
{
  if(first - second > EPSILON)
    return true;
  else
    return false;
}

bool isGreaterOrEqual(double first, double second)
{
  if(first - second > -EPSILON)
    return true;
  else
    return false;
}

bool isOdd(int N)
{
  return (N % 2 != 0);
}

bool isEven(int N)
{
  return (N % 2 == 0);
}

void solveTDM(const std::vector<double>& a1, const std::vector<double>& a2,
              std::vector<double>& a3, std::vector<double>& b,
              std::vector<double>& x)
{
  std::size_t N = b.size();

  a3[0] /= a2[0];
  b[0] /= a2[0];

  for (std::size_t i = 1; i < N; i++)
  {
    a3[i] /= a2[i] - a1[i]*a3[i-1];
    b[i] = (b[i] - a1[i]*b[i-1]) / (a2[i] - a1[i]*a3[i-1]);
  }

  x[N-1] = b[N-1];
  for (std::size_t i = N-2; /* i >= 0 && */ i < N; i--)
  {
      x[i] = b[i] - a3[i]*x[i+1];
  }
}

}

#endif
