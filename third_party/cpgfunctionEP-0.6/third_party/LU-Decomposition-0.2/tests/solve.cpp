//
// Created by jackcook on 8/17/21.
//

#include <LU-Decomposition/lu.h>

#include <vector>
#include <iostream>
#include <thread>

using namespace std;

int main() {

    //may return 0 when not able to detect
    const auto processor_count = std::thread::hardware_concurrency();
    int n_threads = int(processor_count);

    int n = 3;

    // Define 2D vector A of nxn full of zeros in a 1D space
    vector<double> A (n*n, 0);
    //        {5., 9., 10.},
    //        {2., 7., 3.},
    //        {8., 2., 4.}
    A[n*0+0] = 5.;
    A[n*0+1] = 9.;
    A[n*0+2] = 10.;
    A[n*1+0] = 2.;
    A[n*1+1] = 7.;
    A[n*1+2] = 3.;
    A[n*2+0] = 8.;
    A[n*2+1] = 2.;
    A[n*2+2] = 4.;

    vector<double> b(n, 0);
    b[0] = 22.;
    b[1] = 13.;
    b[2] = 17.;

    vector<int> indx(n);

    jcc::CheckSingularity(A, n);
    jcc::CroutDecomposition(A, n, indx, n_threads);
    jcc::CroutSolve(A, b, n, indx, n_threads);


    // Run checks
    std::vector<double> LU(n*n, 0);
    LU[n*0+0] = 8.;
    LU[n*0+1] = 2.;
    LU[n*0+2] = 4.;
    LU[n*1+0] = 0.625;
    LU[n*1+1] = 7.75;
    LU[n*1+2] = 7.5;
    LU[n*2+0] = 0.25;
    LU[n*2+1] = 0.83871;
    LU[n*2+2] = -4.29032;

    double error;
    std::cout << "Check LU decomposition matrix: ";
    for (size_t i=0; i<n; i++) {
        for (size_t j=0; j<n; j++) {
            error = std::fabs(A[n*i+j] - LU[n*i+j]) / std::fabs(LU[n*i+j]);
            if (error > 1.0e-6) {
                throw std::invalid_argument("Wrong value found in LU.");
            }
        }
    }
    std::cout << "Passed" << std::endl;

    // From np.linalg.solve(), the solution is
    //      b: [1.71052632 1.28947368 0.18421053]
    std::vector<double> x(n, 0.);
    x[0] = 1.71052632;
    x[1] = 1.28947368;
    x[2] = 0.18421053;
    std::cout << "Check solution to system of equations: ";
    for (size_t i=0; i<n; i++) {
        error = std::fabs(b[i] - x[i]) / std::fabs(x[i]);
        if (error > 1.0e-6) {
            throw std::invalid_argument("Wrong value found in LU.");
        }
    }
    std::cout << "Passed" << std::endl;

    return 0;
}