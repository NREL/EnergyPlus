// EnergyPlus, Copyright (c) 1996-2020, The Board of Trustees of the University of Illinois,
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

#ifndef TempSolveRoot_hh_INCLUDED
#define TempSolveRoot_hh_INCLUDED

// C++ Headers
#include <functional>
#include <type_traits>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1A.hh>
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Array1S.hh>
#include <ObjexxFCL/Array2A.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus/EnergyPlus.hh>

namespace EnergyPlus {

// Forward declarations
struct EnergyPlusData;

// Forward declaration
namespace OutputProcessor {
    enum class TimeStepType;
}

namespace TempSolveRoot {

    // Data
    // This module should not contain variables in the module sense as it is
    // intended strictly to provide "interfaces" to routines used by other
    // parts of the simulation.

    // MODULE PARAMETER DEFINITIONS
    // na

    // DERIVED TYPE DEFINITIONS
    // na

    // INTERFACE DEFINITIONS

    // MODULE VARIABLE DECLARATIONS:
    // na

    // SUBROUTINE SPECIFICATIONS FOR MODULE General
    // PUBLIC  SaveCompDesWaterFlow
    // PUBLIC  ErfFunction

    // Functions

    void SolveRoot(EnergyPlusData &state, Real64 const Eps, // required absolute accuracy
                   int const MaxIte, // maximum number of allowed iterations
                   int &Flag,        // integer storing exit status
                   Real64 &XRes,     // value of x that solves f(x [,Par]) = 0
                   std::function<Real64(EnergyPlusData &state, Real64 const, std::vector<Real64> const &)> f,
                   Real64 const X_0,         // 1st bound of interval that contains the solution
                   Real64 const X_1,         // 2nd bound of interval that contains the solution
                   std::vector<Real64> const &Par // array with additional parameters used for function evaluation
    );

    void SolveRoot(EnergyPlusData &state, Real64 const Eps, // required absolute accuracy
                   int const MaxIte, // maximum number of allowed iterations
                   int &Flag,        // integer storing exit status
                   Real64 &XRes,     // value of x that solves f(x [,Par]) = 0
                   std::function<Real64(EnergyPlusData &state, Real64 const, Array1D<Real64> const &)> f,
                   Real64 const X_0,         // 1st bound of interval that contains the solution
                   Real64 const X_1,         // 2nd bound of interval that contains the solution
                   Array1D<Real64> const &Par // array with additional parameters used for function evaluation
    );


    void SolveRoot(EnergyPlusData &state, Real64 const Eps, // required absolute accuracy
                   int const MaxIte, // maximum number of allowed iterations
                   int &Flag,        // integer storing exit status
                   Real64 &XRes,     // value of x that solves f(x [,Par]) = 0
                   std::function<Real64(EnergyPlusData &state, Real64 const, Array1D<Real64> const &)> f,
                   Real64 const X_0,           // 1st bound of interval that contains the solution
                   Real64 const X_1,           // 2nd bound of interval that contains the solution
                   Array1D<Real64> const &Par, // array with additional parameters used for function evaluation
                   int const AlgorithmTypeNum, // ALgorithm selection
                   Real64 &XX_0,               // Low bound obtained with maximum number of allowed iterations
                   Real64 &XX_1                // Hign bound obtained with maximum number of allowed iterations
    );

    void SolveRoot(EnergyPlusData &state,
                   Real64 const Eps, // required absolute accuracy
                   int const MaxIte, // maximum number of allowed iterations
                   int &Flag,        // integer storing exit status
                   Real64 &XRes,     // value of x that solves f(x) = 0
                   std::function<Real64(EnergyPlusData &state, Real64 const)> f,
                   Real64 const X_0, // 1st bound of interval that contains the solution
                   Real64 const X_1  // 2nd bound of interval that contains the solution
    );

/*
    void SolveRoot(Real64 const Eps, // required absolute accuracy
                   int const MaxIte, // maximum number of allowed iterations
                   int &Flag,        // integer storing exit status
                   Real64 &XRes,     // value of x that solves f(x) = 0
                   std::function<Real64(Real64 const)> f,
                   Real64 const X_0,           // 1st bound of interval that contains the solution
                   Real64 const X_1,           // 2nd bound of interval that contains the solution
                   int const AlgorithmTypeNum, // ALgorithm selection
                   Real64 &XX_0,               // Low bound obtained with maximum number of allowed iterations
                   Real64 &XX_1                // Hign bound obtained with maximum number of allowed iterations
    );
 */
} // namespace General

} // namespace EnergyPlus

#endif
