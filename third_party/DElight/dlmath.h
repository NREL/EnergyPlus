#include <math.h>

#ifndef HAS_ISNAN
inline bool isnan(double v) 
{ 
  return _isnan(v); 
} 
#endif

inline double safeDivide(double a, double b)
{
  double tol = 1E-10;
  
  double denom = b;
  if (b >= 0 && b < tol){
    denom = tol;
  }else if (b < 0 && b > -tol){
    denom = -tol;
  }

  double result = a / denom;
  return result;
}