//
// Created by jackcook on 6/29/21.
//

#include <blas/blas.h>

using namespace std;

namespace jcc::blas {

    void axpy(int &n, double &a, vector<double> &x, vector<double> &y,
              int &start, int &n_threads){
        // y = a*x + y
        #pragma omp parallel for num_threads(n_threads)
        for (int i=0; i<n; i++) {
            y[i] = a * x[i+start] + y[i];
        }
    }  // axpy();

    void spmv(int &n, double &alpha, vector<double> &A, vector<double> &x,
              double &beta, vector<double> &y, int &start, int &n_threads) {
        // TODO: implement upper

        // LOWER
        double zero = 0.;
        double temp1;
        double temp2;
        int kk = 0;
        int k;
        for (int j=0; j<n; j++) {
            temp1 = alpha*x[j+start];
            temp2 = zero;
            y[j] = y[j] + temp1*A[kk];
            k = kk + 1;
            for (int i=j+1; i<n; i++) {
                y[i] = y[i] + temp1*A[k];
                temp2 = temp2 + A[k]*x[i+start];
                k = k + 1;
            }  // next i
            y[j] = y[j] + alpha*temp2;
            kk = kk + (n-j);
        }  // next j
    }  // spmv();

}  // namespace jcc::blas