#ifndef __FMIN_
#define __FMIN_

double fminbr(double a, double b, double(*f)(double x, void *data), void *data_in, double tol);

#endif