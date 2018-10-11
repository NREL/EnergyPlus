/* Copyright (c) 2012-2018 Big Ladder Software LLC. All rights reserved.
 * See the LICENSE file for additional terms and conditions. */

#ifndef Functions_CPP
#define Functions_CPP

#include "Functions.hpp"

namespace Kiva {

static const double EPSILON = 1E-7;

bool isLessThan(double first, double second) {
  if (first - second < -EPSILON)
    return true;
  else
    return false;
}

bool isLessOrEqual(double first, double second) {
  if (first - second < EPSILON)
    return true;
  else
    return false;
}

bool isEqual(double first, double second) {
  if (fabs(first - second) < EPSILON)
    return true;
  else
    return false;
}

bool isEqual(double first, double second, double epsilon) {
  if (fabs(first - second) < epsilon)
    return true;
  else
    return false;
}

bool isGreaterThan(double first, double second) {
  if (first - second > EPSILON)
    return true;
  else
    return false;
}

bool isGreaterOrEqual(double first, double second) {
  if (first - second > -EPSILON)
    return true;
  else
    return false;
}

bool isOdd(int N) { return (N % 2 != 0); }

bool isEven(int N) { return (N % 2 == 0); }

void solveTDM(const std::vector<double> &a1, const std::vector<double> &a2, std::vector<double> &a3,
              std::vector<double> &b, std::vector<double> &x) {
  std::size_t N = b.size();
  std::size_t i;

  double const *a1_(&a1[0]);
  double const *a2_(&a2[0]);
  double *a3_(&a3[0]);
  double *b_(&b[0]);
  double *x_(&x[0]);

  a3_[0] /= a2_[0];
  b_[0] /= a2_[0];

  for (i = 1; i < N; ++i) {
    a3_[i] /= a2_[i] - a1_[i] * a3_[i - 1];
    b_[i] = (b_[i] - a1_[i] * b_[i - 1]) / (a2_[i] - a1_[i] * a3_[i - 1]);
  }
  x_[N - 1] = b_[N - 1];
  for (i = N - 2; /* i >= 0 && */ i < N; --i) {
    x_[i] = b_[i] - a3_[i] * x_[i + 1];
  }
}

} // namespace Kiva

#endif
