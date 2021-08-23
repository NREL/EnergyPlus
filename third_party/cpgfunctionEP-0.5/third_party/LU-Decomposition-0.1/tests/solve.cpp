//
// Created by jackcook on 8/17/21.
//

#include <LU-Decomposition/lu.h>

#include <vector>
#include <iostream>

int main() {

    int n = 3;

    // Define 2D vector A of nxn full of zeros
    vector<vector<double> > A(n, vector<double> (n, 0));
    //        {5., 9., 10.},
    //        {2., 7., 3.},
    //        {8., 2., 4.}
    A[0][0] = 5.;
    A[0][1] = 9.;
    A[0][2] = 10.;
    A[1][0] = 2.;
    A[1][1] = 7.;
    A[1][2] = 3.;
    A[2][0] = 8.;
    A[2][1] = 2.;
    A[2][2] = 4.;

    vector<double> b(n, 0);
    b[0] = 22.;
    b[1] = 13.;
    b[2] = 17.;

    vector<int> indx(n);

    double d;

    jcc::decomposition(A, n, indx, d);

    std::cout << "A matrix after decomposition function" << std::endl;
    for (size_t i=0; i<n; i++) {
        for (size_t j=0; j<n; j++) std::cout << A[i][j] << " ";
        std::cout << endl;
    }

    jcc::back_substitution(A, n, indx, b);

    // From np.linalg.solve(), the solution is
    //      b: [1.71052632 1.28947368 0.18421053]
    std::cout << "Solution to system of equations" << std::endl;
    for (size_t i=0; i<n; i++)
        std::cout << b[i] << std::endl;
}