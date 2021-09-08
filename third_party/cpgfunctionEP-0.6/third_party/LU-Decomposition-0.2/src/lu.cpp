//
// Created by jackcook on 8/17/21.
//

#include <LU-Decomposition/lu.h>
#include <algorithm>
#include <numeric>
#include <thread>

double jcc::dot(int &n, std::vector<double> &A, int &begin_x, int &incx,
                int &begin_y, int &incy, int &n_threads) {

    int k;
    double summation = 0.;
    // reduction notes: https://stackoverflow.com/a/26999512
//#pragma omp parallel for default(none) num_threads(n_threads) shared(n, A, begin_x, begin_y, incx, incy) private(k, _x, _y) reduction(+:summation)
    for (k=0; k<n; k++) {
        summation += A[incx * k + begin_x] * A[incy * k + begin_y];
    }

    return summation;
}

void jcc::CheckSingularity(std::vector<double> &A, int &n) {

    int i;
    std::vector<double>::iterator begin;
    std::vector<double>::iterator end;

    double _min;
    double _max;
    bool singular = false;
    for (i=0; i<n; i++) {
        // Get the maximum and minimum element in row i
        begin = A.begin() + (i  * n);
        end = A.begin() + (i * n + n);
        auto [min, max] = std::minmax_element(begin, end);
        _min = fabs(*min);
        _max = *max;
        singular = (_min + _max < 1.0e-6);
    } // next i
    // Make sure the matrix is not singular
    if (singular) throw std::invalid_argument("The matrix is singular. One of the"
                                         "rows contains all values less than"
                                         "1.0e-6.");
}

void jcc::CroutDecomposition(std::vector<double > &A, int &n,
                             std::vector<int> &indx, int &n_threads) {

    int i, j, i_piv;
    double summation, pivot, largest;

    std::vector<double> tmp(n, 0);

    // A * x = (L U) * x = L (U x) = b (2.3.3)
    // Perform the decomposition

    // Crout's algorithm with implicit pivoting

    // check to make sure the matrix is non-singular
    std::vector<double>::iterator begin;
    std::vector<double>::iterator end;
    std::vector<double>::iterator begin_2;
    int incx;
    int incy;
    int bgx;
    int bgy;

    // L_ii = 1 for i = 0,..., N-1 (2.3.11)
    for (j=0; j<n; j++) {
        // for i = 0, 1,...,j use (2.3.8), (2.3.9) and (2.3.11)
        // U_ij = A_ij - sum_{k=0}^{i-1} L_ik U_kj (2.3.12)
        for (i=0; i<j; i++) {
            bgx = n*i;
            bgy = j;
            incx = 1;
            incy = n;
            summation = jcc::dot(i, A, bgx, incx, bgy, incy, n_threads);
            A[n*i+j] -= summation;
        } // next i

        // for i = j+1, j+2,...,n-1 use (2.3.10)
        // L_ij = 1 / U_jj (A[i][j] - sum_{k=0}^{j-1} A[i][k] A[k][j])
        // Save the pivot term for later
        i_piv = j;
        largest = 0.;
        for (i=j; i<n; i++) {
            bgx = n*i;
            bgy = j;
            incx = 1;
            incy = n;
            summation = jcc::dot(j, A, bgx, incx, bgy, incy, n_threads);
            A[n*i+j] -= summation;
            pivot = A[n*i+j];
            // store the row in which the largest pivot occurs
            if (pivot > largest) {
                largest = pivot;
                i_piv = i;
            }
        } // next i
      
        // if the pivot term changed from the diagonal, swap rows
        if ( i_piv != j) {
            // swap row i_piv with row j
            begin = A.begin() + i_piv * n;
            end = A.begin() + i_piv * n + n;
            begin_2 = A.begin() + j * n;
            swap_ranges(begin, end, begin_2);
        } // end if
        indx[j] = i_piv;
        if (j != (n-1)) {
            // Make sure the pivot element is non-zero
            if (A[j*n + j] == 0.) A[j*n + j] = 1.0e-10;
            pivot = 1. / A[j * n + j];
#pragma omp parallel for default(none) num_threads(n_threads) shared(j, pivot, n, A) private(i)
            for (i=j+1; i<n; i++) {
                A[n*i+j] *= pivot;
            } // next i
        }
    } // next j

} // jcc::CroutDecomposition

void jcc::CroutSolve(std::vector<double> &LU, std::vector<double> &b, int &n,
                     std::vector<int> &indx, int &n_threads) {
    // L * y = b    (2.3.4)
    // U * x = y    (2.3.5)

    int i, j;
    double summation, inv;

    // Forward substitution
    // y_0 = b_0 / L_00
    std::swap(b[0], b[indx[0]]); // work with implicit pivot
    // y_i = 1 / L_ii * (b_i - sum_{j=0}^{i-1} A[i][j] * y[j])
    // i = 1,2,...,N-1
    for (i=1; i<n; i++) {
        std::swap(b[i], b[indx[i]]);
        summation = 0.;
//#pragma omp parallel for default(none) num_threads(n_threads) shared(LU, b, n, i) private(j) reduction(+:summation)
        for (j=0; j<i; j++) {
            summation += LU[n*i+j] * b[j];
        } // next j
        b[i] -= summation;
    } // next i

    // Backward substitution
    // x[n-1] = y[n-1] / (LU[n-1][n-1])
    b[n-1] = b[n-1] / LU[n*n-1];
    // x_i = 1 / LU_ii * (y_i - sum_{j=i+1}^{N-1} LU[i][j] * x[j])
    // i = N-1, N-3,...,0
    for (i=(n-2); i>=0; i--) {
        summation = 0.;
//#pragma omp parallel for default(none) num_threads(n_threads) shared(LU, b, n, i) private(j) reduction(+:summation)
        for (j=i+1; j<n; j++) {
            summation += LU[n*i+j] * b[j];
        }
        inv = 1 / LU[n*i+i];
        b[i] -= summation;
        b[i] *= inv;
    }

} // jcc::CroutSolve
