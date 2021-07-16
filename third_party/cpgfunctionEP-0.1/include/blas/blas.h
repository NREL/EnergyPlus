//
// Created by jackcook on 6/29/21.
//

// BLAS functions that have been written in C++

#include <vector>

using namespace std;

#ifndef CPGFUNCTION_BLAS_H
#define CPGFUNCTION_BLAS_H

namespace jcc  { namespace blas {

        void axpy(int &n, double &a, vector<double> &x, vector<double> &y,
                  int &start, int &n_threads);

        void spmv(int &n, double &alpha, vector<double> &A, vector<double> &x,
                  double &beta, vector<double> &y, int &start, int &n_threads);

    }  // namespace blas
}  // namespace jcc


#endif //CPGFUNCTION_BLAS_H
