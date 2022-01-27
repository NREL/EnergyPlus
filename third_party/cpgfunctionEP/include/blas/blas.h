//
// Created by jackcook on 6/29/21.
//

// BLAS functions that have been written in C++

#ifndef CPGFUNCTION_BLAS_H
#define CPGFUNCTION_BLAS_H

#include <vector>

namespace jcc  { namespace blas {

        void axpy(int &n, double &a, std::vector<double> &x, std::vector<double> &y,
                  int &start, int &n_threads);

        void spmv(int &n, double &alpha, std::vector<double> &A, std::vector<double> &x,
                  double &beta, std::vector<double> &y, int &start, int &n_threads);

    }  // namespace blas
}  // namespace jcc


#endif //CPGFUNCTION_BLAS_H
