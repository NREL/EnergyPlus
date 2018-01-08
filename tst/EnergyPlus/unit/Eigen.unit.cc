// EnergyPlus, Copyright (c) 1996-2017, The Board of Trustees of the University of Illinois,
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy), Oak Ridge
// National Laboratory, managed by UT-Battelle, Alliance for Sustainable Energy, LLC, and other
// contributors. All rights reserved.
//
// NOTICE: This Software was developed under funding from the U.S. Department of Energy and the
// U.S. Government consequently retains certain rights. As such, the U.S. Government has been
// granted for itself and others acting on its behalf a paid-up, nonexclusive, irrevocable,
// worldwide license in the Software to reproduce, distribute copies to the public, prepare
// derivative works, and perform publicly and display publicly, and to permit others to do so.
//
// Redistribution and use in source and binary forms, with or without modification, are permitted
// provided that the following conditions are met:
//
// (1) Redistributions of source code must retain the above copyright notice, this list of
//     conditions and the following disclaimer.
//
// (2) Redistributions in binary form must reproduce the above copyright notice, this list of
//     conditions and the following disclaimer in the documentation and/or other materials
//     provided with the distribution.
//
// (3) Neither the name of the University of California, Lawrence Berkeley National Laboratory,
//     the University of Illinois, U.S. Dept. of Energy nor the names of its contributors may be
//     used to endorse or promote products derived from this software without specific prior
//     written permission.
//
// (4) Use of EnergyPlus(TM) Name. If Licensee (i) distributes the software in stand-alone form
//     without changes from the version obtained under this License, or (ii) Licensee makes a
//     reference solely to the software portion of its product, Licensee must refer to the
//     software as "EnergyPlus version X" software, where "X" is the version number Licensee
//     obtained under this License and may not use a different name for the software. Except as
//     specifically required in this Section (4), Licensee shall not use in a company name, a
//     product name, in advertising, publicity, or other promotional activities any name, trade
//     name, trademark, logo, or other designation of "EnergyPlus", "E+", "e+" or confusingly
//     similar designation, without the U.S. Department of Energy's prior written consent.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
// AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
// CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
// OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.

#define min(x,y) (((x) < (y)) ? (x) : (y))

#include <iostream>
// Google Test Headers
#include <gtest/gtest.h>
#define EIGEN_USE_MKL_ALL
#include <Eigen/Dense>
#include <mkl.h>
#include <fstream>

//TEST(Eigen, EigenTest) {
//    Eigen::MatrixXd u = Eigen::MatrixXd::Random(3000, 2300);
//    Eigen::MatrixXd v = Eigen::MatrixXd::Random(2300, 3200);
//    auto const initial = dsecnd();
//    Eigen::MatrixXd w = u * v;
//    auto const elapsed = dsecnd() - initial;
//    std::cerr << "Eigen w/ IntelMKL: " << elapsed << " seconds" << std::endl;
//}

//TEST(EigenIntelMkl, MklTest) {
//    const int m = 3000, k = 2300, n = 3200;
//    double alpha = 1.0, beta = 0.0;
//    double *A = (double *)mkl_malloc( m*k*sizeof( double ), 64 );
//    double *B = (double *)mkl_malloc( k*n*sizeof( double ), 64 );
//    double *C = (double *)mkl_malloc( m*n*sizeof( double ), 64 );
//
//    for (int i = 0; i < (m*k); i++) {
//        A[i] = (double)rand();
//    }
//    for (int i = 0; i < (k*n); i++) {
//        B[i] = (double)rand();
//    }
//    for (int i = 0; i < (m*n); i++) {
//        C[i] = 0.0;
//    }
//    auto const sInitial = dsecnd();
//    cblas_dgemm(CblasRowMajor, CblasNoTrans, CblasNoTrans,
//                m, n, k, alpha, A, k, B, n, beta, C, n);
//    auto const sElapsed = dsecnd() - sInitial;
//    std::cerr << "IntelMKL: " << sElapsed << " seconds" << std::endl;
//    mkl_free(A);
//    mkl_free(B);
//    mkl_free(C);
//}

//TEST(EigenIntelMkl, MklTestWithoutThreading) {
//    mkl_set_num_threads(1); // for testing sequential
//    const int m = 3000, k = 2300, n = 3200;
//    double alpha = 1.0, beta = 0.0;
//    double *A = (double *)mkl_malloc( m*k*sizeof( double ), 64 );
//    double *B = (double *)mkl_malloc( k*n*sizeof( double ), 64 );
//    double *C = (double *)mkl_malloc( m*n*sizeof( double ), 64 );
//
//    for (int i = 0; i < (m*k); i++) {
//        A[i] = (double)rand();
//    }
//    for (int i = 0; i < (k*n); i++) {
//        B[i] = (double)rand();
//    }
//    for (int i = 0; i < (m*n); i++) {
//        C[i] = 0.0;
//    }
//
//    auto const sInitial = dsecnd();
//    cblas_dgemm(CblasRowMajor, CblasNoTrans, CblasNoTrans,
//                m, n, k, alpha, A, k, B, n, beta, C, n);
//
//    auto const sElapsed = dsecnd() - sInitial;
//    std::cerr << "IntelMKL w/o threading: " << sElapsed << " seconds" << std::endl;
//    mkl_free(A);
//    mkl_free(B);
//    mkl_free(C);
//}

//TEST(Eigen, EigenTestWithoutThreading) {
//    mkl_set_num_threads(1);
//    Eigen::MatrixXd u = Eigen::MatrixXd::Random(3000, 2300);
//    Eigen::MatrixXd v = Eigen::MatrixXd::Random(2300, 3200);
//    auto const initial = dsecnd();
//    Eigen::MatrixXd w = u * v;
//    auto const elapsed = dsecnd() - initial;
//    std::cerr << "Eigen w/IntelMKL w/o threading: " << elapsed << " seconds" << std::endl;
//}
