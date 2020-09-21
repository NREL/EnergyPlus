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

// C++ Headers
#include <cassert>
#include <cmath>
#include <cstdlib>

// ObjexxFCL Headers
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/TempSolveRoot.hh>

#if defined(_WIN32) && _MSC_VER < 1900
#define snprintf _snprintf
#endif

namespace EnergyPlus {

namespace TempSolveRoot {

    // Module containing routines for general use

    // MODULE INFORMATION:
    //       AUTHOR         Fred Buhl, Linda Lawrie
    //       DATE WRITTEN   December 2001
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // contains routines (most likely numeric) that may be needed in several parts
    // of EnergyPlus

    // METHODOLOGY EMPLOYED:
    // na

    // REFERENCES: none

    // OTHER NOTES: none

    // Using/Aliasing
    using DataHVACGlobals::Bisection;
    using DataHVACGlobals::HVACSystemRootFinding;

    // Data
    // This module should not contain variables in the module sense as it is
    // intended strictly to provide "interfaces" to routines used by other
    // parts of the simulation.

    // MODULE PARAMETER DEFINITIONS
    static std::string const BlankString;

    // DERIVED TYPE DEFINITIONS
    // na

    // INTERFACE DEFINITIONS

    // MODULE VARIABLE DECLARATIONS:
    // na

    // SUBROUTINE SPECIFICATIONS FOR MODULE General
    // PUBLIC  SaveCompDesWaterFlow
    // PUBLIC  ErfFunction

    // Functions

    void SolveRoot(EnergyPlusData &state,
                   Real64 const Eps, // required absolute accuracy
                   int const MaxIte, // maximum number of allowed iterations
                   int &Flag,        // integer storing exit status
                   Real64 &XRes,     // value of x that solves f(x,Par) = 0
                   std::function<Real64(EnergyPlusData &state, Real64 const, Array1D<Real64> const &)> f,
                   Real64 const X_0,         // 1st bound of interval that contains the solution
                   Real64 const X_1,         // 2nd bound of interval that contains the solution
                   Array1D<Real64> const &Par // array with additional parameters used for function evaluation
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Michael Wetter
        //       DATE WRITTEN   March 1999
        //       MODIFIED       Fred Buhl November 2000, R. Raustad October 2006 - made subroutine RECURSIVE
        //                      L. Gu, May 2017 - allow both Bisection and RegulaFalsi
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Find the value of x between x0 and x1 such that f(x,Par)
        // is equal to zero.

        // METHODOLOGY EMPLOYED:
        // Uses the Regula Falsi (false position) method (similar to secant method)

        // REFERENCES:
        // See Press et al., Numerical Recipes in Fortran, Cambridge University Press,
        // 2nd edition, 1992. Page 347 ff.

        // USE STATEMENTS:
        // na

        // Argument array dimensioning

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // = -2: f(x0) and f(x1) have the same sign
        // = -1: no convergence
        // >  0: number of iterations performed
        // optional
        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const SMALL(1.e-10);

        // INTERFACE BLOCK SPECIFICATIONS

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 X0;       // present 1st bound
        Real64 X1;       // present 2nd bound
        Real64 XTemp;    // new estimate
        Real64 Y0;       // f at X0
        Real64 Y1;       // f at X1
        Real64 YTemp;    // f at XTemp
        Real64 DY;       // DY = Y0 - Y1
        bool Conv;       // flag, true if convergence is achieved
        bool StopMaxIte; // stop due to exceeding of maximum # of iterations
        bool Cont;       // flag, if true, continue searching
        int NIte;        // number of interations
        int AltIte;      // an accounter used for Alternation choice

        X0 = X_0;
        X1 = X_1;
        XTemp = X0;
        Conv = false;
        StopMaxIte = false;
        Cont = true;
        NIte = 0;
        AltIte = 0;

        Y0 = f(state, X0, Par);
        Y1 = f(state, X1, Par);
        // check initial values
        if (Y0 * Y1 > 0) {
            Flag = -2;
            XRes = X0;
            return;
        }

        while (Cont) {

            DY = Y0 - Y1;
            if (std::abs(DY) < SMALL) DY = SMALL;
            if (std::abs(X1 - X0) < SMALL) {
                break;
            }
            // new estimation
            switch (HVACSystemRootFinding.HVACSystemRootSolver) {
            case DataHVACGlobals::HVACSystemRootSolverAlgorithm::RegulaFalsi: {
                XTemp = (Y0 * X1 - Y1 * X0) / DY;
                break;
            }
            case DataHVACGlobals::HVACSystemRootSolverAlgorithm::Bisection: {
                XTemp = (X1 + X0) / 2.0;
                break;
            }
            case DataHVACGlobals::HVACSystemRootSolverAlgorithm::RegulaFalsiThenBisection: {
                if (NIte > HVACSystemRootFinding.NumOfIter) {
                    XTemp = (X1 + X0) / 2.0;
                } else {
                    XTemp = (Y0 * X1 - Y1 * X0) / DY;
                }
                break;
            }
            case DataHVACGlobals::HVACSystemRootSolverAlgorithm::BisectionThenRegulaFalsi: {
                if (NIte <= HVACSystemRootFinding.NumOfIter) {
                    XTemp = (X1 + X0) / 2.0;
                } else {
                    XTemp = (Y0 * X1 - Y1 * X0) / DY;
                }
                break;
            }
            case DataHVACGlobals::HVACSystemRootSolverAlgorithm::Alternation: {
                if (AltIte > HVACSystemRootFinding.NumOfIter) {
                    XTemp = (X1 + X0) / 2.0;
                    if (AltIte >= 2 * HVACSystemRootFinding.NumOfIter) AltIte = 0;
                } else {
                    XTemp = (Y0 * X1 - Y1 * X0) / DY;
                }
                break;
            }
            default: {
                XTemp = (Y0 * X1 - Y1 * X0) / DY;
            }
            }

            YTemp = f(state, XTemp, Par);

            ++NIte;
            ++AltIte;

            // check convergence
            if (std::abs(YTemp) < Eps) Conv = true;

            if (NIte > MaxIte) StopMaxIte = true;

            if ((!Conv) && (!StopMaxIte)) {
                Cont = true;
            } else {
                Cont = false;
            }

            if (Cont) {

                // reassign values (only if further iteration required)
                if (Y0 < 0.0) {
                    if (YTemp < 0.0) {
                        X0 = XTemp;
                        Y0 = YTemp;
                    } else {
                        X1 = XTemp;
                        Y1 = YTemp;
                    }
                } else {
                    if (YTemp < 0.0) {
                        X1 = XTemp;
                        Y1 = YTemp;
                    } else {
                        X0 = XTemp;
                        Y0 = YTemp;
                    }
                } // ( Y0 < 0 )

            } // (Cont)

        } // Cont

        if (Conv) {
            Flag = NIte;
        } else {
            Flag = -1;
        }
        XRes = XTemp;
    }

    void SolveRoot(EnergyPlusData &state,
                   Real64 const Eps, // required absolute accuracy
                   int const MaxIte, // maximum number of allowed iterations
                   int &Flag,        // integer storing exit status
                   Real64 &XRes,     // value of x that solves f(x,Par) = 0
                   std::function<Real64(EnergyPlusData &state, Real64 const, std::vector<Real64> const &)> f,
                   Real64 const X_0,         // 1st bound of interval that contains the solution
                   Real64 const X_1,         // 2nd bound of interval that contains the solution
                   std::vector<Real64> const &Par // array with additional parameters used for function evaluation
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Michael Wetter
        //       DATE WRITTEN   March 1999
        //       MODIFIED       Fred Buhl November 2000, R. Raustad October 2006 - made subroutine RECURSIVE
        //                      L. Gu, May 2017 - allow both Bisection and RegulaFalsi
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Find the value of x between x0 and x1 such that f(x,Par)
        // is equal to zero.

        // METHODOLOGY EMPLOYED:
        // Uses the Regula Falsi (false position) method (similar to secant method)

        // REFERENCES:
        // See Press et al., Numerical Recipes in Fortran, Cambridge University Press,
        // 2nd edition, 1992. Page 347 ff.

        // USE STATEMENTS:
        // na

        // Argument array dimensioning

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // = -2: f(x0) and f(x1) have the same sign
        // = -1: no convergence
        // >  0: number of iterations performed
        // optional
        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const SMALL(1.e-10);

        // INTERFACE BLOCK SPECIFICATIONS

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 X0;       // present 1st bound
        Real64 X1;       // present 2nd bound
        Real64 XTemp;    // new estimate
        Real64 Y0;       // f at X0
        Real64 Y1;       // f at X1
        Real64 YTemp;    // f at XTemp
        Real64 DY;       // DY = Y0 - Y1
        bool Conv;       // flag, true if convergence is achieved
        bool StopMaxIte; // stop due to exceeding of maximum # of iterations
        bool Cont;       // flag, if true, continue searching
        int NIte;        // number of interations
        int AltIte;      // an accounter used for Alternation choice

        X0 = X_0;
        X1 = X_1;
        XTemp = X0;
        Conv = false;
        StopMaxIte = false;
        Cont = true;
        NIte = 0;
        AltIte = 0;

        Y0 = f(state, X0, Par);
        Y1 = f(state, X1, Par);
        // check initial values
        if (Y0 * Y1 > 0) {
            Flag = -2;
            XRes = X0;
            return;
        }

        while (Cont) {

            DY = Y0 - Y1;
            if (std::abs(DY) < SMALL) DY = SMALL;
            if (std::abs(X1 - X0) < SMALL) {
                break;
            }
            // new estimation
            switch (HVACSystemRootFinding.HVACSystemRootSolver) {
            case DataHVACGlobals::HVACSystemRootSolverAlgorithm::RegulaFalsi: {
                XTemp = (Y0 * X1 - Y1 * X0) / DY;
                break;
            }
            case DataHVACGlobals::HVACSystemRootSolverAlgorithm::Bisection: {
                XTemp = (X1 + X0) / 2.0;
                break;
            }
            case DataHVACGlobals::HVACSystemRootSolverAlgorithm::RegulaFalsiThenBisection: {
                if (NIte > HVACSystemRootFinding.NumOfIter) {
                    XTemp = (X1 + X0) / 2.0;
                } else {
                    XTemp = (Y0 * X1 - Y1 * X0) / DY;
                }
                break;
            }
            case DataHVACGlobals::HVACSystemRootSolverAlgorithm::BisectionThenRegulaFalsi: {
                if (NIte <= HVACSystemRootFinding.NumOfIter) {
                    XTemp = (X1 + X0) / 2.0;
                } else {
                    XTemp = (Y0 * X1 - Y1 * X0) / DY;
                }
                break;
            }
            case DataHVACGlobals::HVACSystemRootSolverAlgorithm::Alternation: {
                if (AltIte > HVACSystemRootFinding.NumOfIter) {
                    XTemp = (X1 + X0) / 2.0;
                    if (AltIte >= 2 * HVACSystemRootFinding.NumOfIter) AltIte = 0;
                } else {
                    XTemp = (Y0 * X1 - Y1 * X0) / DY;
                }
                break;
            }
            default: {
                XTemp = (Y0 * X1 - Y1 * X0) / DY;
            }
            }

            YTemp = f(state, XTemp, Par);

            ++NIte;
            ++AltIte;

            // check convergence
            if (std::abs(YTemp) < Eps) Conv = true;

            if (NIte > MaxIte) StopMaxIte = true;

            if ((!Conv) && (!StopMaxIte)) {
                Cont = true;
            } else {
                Cont = false;
            }

            if (Cont) {

                // reassign values (only if further iteration required)
                if (Y0 < 0.0) {
                    if (YTemp < 0.0) {
                        X0 = XTemp;
                        Y0 = YTemp;
                    } else {
                        X1 = XTemp;
                        Y1 = YTemp;
                    }
                } else {
                    if (YTemp < 0.0) {
                        X1 = XTemp;
                        Y1 = YTemp;
                    } else {
                        X0 = XTemp;
                        Y0 = YTemp;
                    }
                } // ( Y0 < 0 )

            } // (Cont)

        } // Cont

        if (Conv) {
            Flag = NIte;
        } else {
            Flag = -1;
        }
        XRes = XTemp;
    }

    void SolveRoot(EnergyPlusData &state,
                   Real64 const Eps, // required absolute accuracy
                   int const MaxIte, // maximum number of allowed iterations
                   int &Flag,        // integer storing exit status
                   Real64 &XRes,     // value of x that solves f(x,Par) = 0
                   std::function<Real64(EnergyPlusData &state, Real64 const, Array1D<Real64> const &)> f,
                   Real64 const X_0,           // 1st bound of interval that contains the solution
                   Real64 const X_1,           // 2nd bound of interval that contains the solution
                   Array1D<Real64> const &Par,  // array with additional parameters used for function evaluation
                   int const AlgorithmTypeNum, // ALgorithm selection
                   Real64 &XX_0,               // Low bound obtained with maximum number of allowed iterations
                   Real64 &XX_1                // Hign bound obtained with maximum number of allowed iterations
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Michael Wetter
        //       DATE WRITTEN   March 1999
        //       MODIFIED       Fred Buhl November 2000, R. Raustad October 2006 - made subroutine RECURSIVE
        //                      L. Gu, May 2017 - selcte an algorithm and output both bounds
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Find the value of x between x0 and x1 such that f(x,Par)
        // is equal to zero.

        // METHODOLOGY EMPLOYED:
        // Uses the Regula Falsi (false position) method (similar to secant method)

        // REFERENCES:
        // See Press et al., Numerical Recipes in Fortran, Cambridge University Press,
        // 2nd edition, 1992. Page 347 ff.

        // USE STATEMENTS:
        // na

        // Argument array dimensioning

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // = -2: f(x0) and f(x1) have the same sign
        // = -1: no convergence
        // >  0: number of iterations performed
        // optional
        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const SMALL(1.e-10);

        // INTERFACE BLOCK SPECIFICATIONS

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 X0;       // present 1st bound
        Real64 X1;       // present 2nd bound
        Real64 XTemp;    // new estimate
        Real64 Y0;       // f at X0
        Real64 Y1;       // f at X1
        Real64 YTemp;    // f at XTemp
        Real64 DY;       // DY = Y0 - Y1
        bool Conv;       // flag, true if convergence is achieved
        bool StopMaxIte; // stop due to exceeding of maximum # of iterations
        bool Cont;       // flag, if true, continue searching
        int NIte;        // number of interations

        X0 = X_0;
        X1 = X_1;
        XTemp = X0;
        Conv = false;
        StopMaxIte = false;
        Cont = true;
        NIte = 0;

        Y0 = f(state, X0, Par);
        Y1 = f(state, X1, Par);
        // check initial values
        if (Y0 * Y1 > 0) {
            Flag = -2;
            XRes = X0;
            return;
        }

        while (Cont) {

            DY = Y0 - Y1;
            if (std::abs(DY) < SMALL) DY = SMALL;
            if (std::abs(X1 - X0) < SMALL) {
                break;
            }
            // new estimation
            if (AlgorithmTypeNum == Bisection) {
                // Bisection
                XTemp = (X1 + X0) / 2.0;
            } else {
                // Regula Falsi
                XTemp = (Y0 * X1 - Y1 * X0) / DY;
            }
            YTemp = f(state, XTemp, Par);

            ++NIte;

            // check convergence
            if (std::abs(YTemp) < Eps) Conv = true;

            if (NIte > MaxIte) StopMaxIte = true;

            if ((!Conv) && (!StopMaxIte)) {
                Cont = true;
            } else {
                Cont = false;
            }

            if (Cont) {

                // reassign values (only if further iteration required)
                if (Y0 < 0.0) {
                    if (YTemp < 0.0) {
                        X0 = XTemp;
                        Y0 = YTemp;
                    } else {
                        X1 = XTemp;
                        Y1 = YTemp;
                    }
                } else {
                    if (YTemp < 0.0) {
                        X1 = XTemp;
                        Y1 = YTemp;
                    } else {
                        X0 = XTemp;
                        Y0 = YTemp;
                    }
                } // ( Y0 < 0 )

            } // (Cont)

        } // Cont

        if (Conv) {
            Flag = NIte;
        } else {
            Flag = -1;
        }
        XRes = XTemp;
        XX_0 = X0;
        XX_1 = X1;
    }

    void SolveRoot(EnergyPlusData &state,
                   Real64 const Eps, // required absolute accuracy
                   int const MaxIte, // maximum number of allowed iterations
                   int &Flag,        // integer storing exit status
                   Real64 &XRes,     // value of x that solves f(x) = 0
                   std::function<Real64(EnergyPlusData &state, Real64 const)> f,
                   Real64 const X_0, // 1st bound of interval that contains the solution
                   Real64 const X_1  // 2nd bound of interval that contains the solution
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Michael Wetter
        //       DATE WRITTEN   March 1999
        //       MODIFIED       Fred Buhl November 2000, R. Raustad October 2006 - made subroutine RECURSIVE
        //                      L. Gu, May 2017 - allow both Bisection and RegulaFalsi
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Find the value of x between x0 and x1 such that f(x)
        // is equal to zero.

        // METHODOLOGY EMPLOYED:
        // Uses the Regula Falsi (false position) method (similar to secant method)

        // REFERENCES:
        // See Press et al., Numerical Recipes in Fortran, Cambridge University Press,
        // 2nd edition, 1992. Page 347 ff.

        // USE STATEMENTS:
        // na

        // Argument array dimensioning

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // = -2: f(x0) and f(x1) have the same sign
        // = -1: no convergence
        // >  0: number of iterations performed
        // optional
        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const SMALL(1.e-10);

        // INTERFACE BLOCK SPECIFICATIONS

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 X0;       // present 1st bound
        Real64 X1;       // present 2nd bound
        Real64 XTemp;    // new estimate
        Real64 Y0;       // f at X0
        Real64 Y1;       // f at X1
        Real64 YTemp;    // f at XTemp
        Real64 DY;       // DY = Y0 - Y1
        bool Conv;       // flag, true if convergence is achieved
        bool StopMaxIte; // stop due to exceeding of maximum # of iterations
        bool Cont;       // flag, if true, continue searching
        int NIte;        // number of interations
        int AltIte;      // used for Alternation choice

        X0 = X_0;
        X1 = X_1;
        XTemp = X0;
        Conv = false;
        StopMaxIte = false;
        Cont = true;
        NIte = 0;
        AltIte = 0;

        Y0 = f(state, X0);
        Y1 = f(state, X1);
        // check initial values
        if (Y0 * Y1 > 0) {
            Flag = -2;
            XRes = X0;
            return;
        }

        while (Cont) {

            DY = Y0 - Y1;
            if (std::abs(DY) < SMALL) DY = SMALL;
            if (std::abs(X1 - X0) < SMALL) {
                break;
            }
            // new estimation
            switch (HVACSystemRootFinding.HVACSystemRootSolver) {
            case DataHVACGlobals::HVACSystemRootSolverAlgorithm::RegulaFalsi: {
                XTemp = (Y0 * X1 - Y1 * X0) / DY;
                break;
            }
            case DataHVACGlobals::HVACSystemRootSolverAlgorithm::Bisection: {
                XTemp = (X1 + X0) / 2.0;
                break;
            }
            case DataHVACGlobals::HVACSystemRootSolverAlgorithm::RegulaFalsiThenBisection: {
                if (NIte > HVACSystemRootFinding.NumOfIter) {
                    XTemp = (X1 + X0) / 2.0;
                } else {
                    XTemp = (Y0 * X1 - Y1 * X0) / DY;
                }
                break;
            }
            case DataHVACGlobals::HVACSystemRootSolverAlgorithm::BisectionThenRegulaFalsi: {
                if (NIte <= HVACSystemRootFinding.NumOfIter) {
                    XTemp = (X1 + X0) / 2.0;
                } else {
                    XTemp = (Y0 * X1 - Y1 * X0) / DY;
                }
                break;
            }
            case DataHVACGlobals::HVACSystemRootSolverAlgorithm::Alternation: {
                if (AltIte > HVACSystemRootFinding.NumOfIter) {
                    XTemp = (X1 + X0) / 2.0;
                    if (AltIte >= 2 * HVACSystemRootFinding.NumOfIter) AltIte = 0;
                } else {
                    XTemp = (Y0 * X1 - Y1 * X0) / DY;
                }
                break;
            }
            default: {
                XTemp = (Y0 * X1 - Y1 * X0) / DY;
            }
            }

            YTemp = f(state, XTemp);

            ++NIte;
            ++AltIte;

            // check convergence
            if (std::abs(YTemp) < Eps) Conv = true;

            if (NIte > MaxIte) StopMaxIte = true;

            if ((!Conv) && (!StopMaxIte)) {
                Cont = true;
            } else {
                Cont = false;
            }

            if (Cont) {

                // reassign values (only if further iteration required)
                if (Y0 < 0.0) {
                    if (YTemp < 0.0) {
                        X0 = XTemp;
                        Y0 = YTemp;
                    } else {
                        X1 = XTemp;
                        Y1 = YTemp;
                    }
                } else {
                    if (YTemp < 0.0) {
                        X1 = XTemp;
                        Y1 = YTemp;
                    } else {
                        X0 = XTemp;
                        Y0 = YTemp;
                    }
                } // ( Y0 < 0 )

            } // (Cont)

        } // Cont

        if (Conv) {
            Flag = NIte;
        } else {
            Flag = -1;
        }
        XRes = XTemp;
    }

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
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Michael Wetter
        //       DATE WRITTEN   March 1999
        //       MODIFIED       Fred Buhl November 2000, R. Raustad October 2006 - made subroutine RECURSIVE
        //                      L. Gu, May 2017 - selcte an algorithm and output both bounds
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Find the value of x between x0 and x1 such that f(x)
        // is equal to zero.

        // METHODOLOGY EMPLOYED:
        // Uses the Regula Falsi (false position) method (similar to secant method)

        // REFERENCES:
        // See Press et al., Numerical Recipes in Fortran, Cambridge University Press,
        // 2nd edition, 1992. Page 347 ff.

        // USE STATEMENTS:
        // na

        // Argument array dimensioning

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // = -2: f(x0) and f(x1) have the same sign
        // = -1: no convergence
        // >  0: number of iterations performed
        // optional
        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const SMALL(1.e-10);

        // INTERFACE BLOCK SPECIFICATIONS

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 X0;       // present 1st bound
        Real64 X1;       // present 2nd bound
        Real64 XTemp;    // new estimate
        Real64 Y0;       // f at X0
        Real64 Y1;       // f at X1
        Real64 YTemp;    // f at XTemp
        Real64 DY;       // DY = Y0 - Y1
        bool Conv;       // flag, true if convergence is achieved
        bool StopMaxIte; // stop due to exceeding of maximum # of iterations
        bool Cont;       // flag, if true, continue searching
        int NIte;        // number of interations

        X0 = X_0;
        X1 = X_1;
        XTemp = X0;
        Conv = false;
        StopMaxIte = false;
        Cont = true;
        NIte = 0;

        Y0 = f(X0);
        Y1 = f(X1);
        // check initial values
        if (Y0 * Y1 > 0) {
            Flag = -2;
            XRes = X0;
            return;
        }

        while (Cont) {

            DY = Y0 - Y1;
            if (std::abs(DY) < SMALL) DY = SMALL;
            if (std::abs(X1 - X0) < SMALL) {
                break;
            }
            // new estimation
            if (AlgorithmTypeNum == Bisection) {
                // Bisection
                XTemp = (X1 + X0) / 2.0;
            } else {
                // Regula Falsi
                XTemp = (Y0 * X1 - Y1 * X0) / DY;
            }
            YTemp = f(XTemp);

            ++NIte;

            // check convergence
            if (std::abs(YTemp) < Eps) Conv = true;

            if (NIte > MaxIte) StopMaxIte = true;

            if ((!Conv) && (!StopMaxIte)) {
                Cont = true;
            } else {
                Cont = false;
            }

            if (Cont) {

                // reassign values (only if further iteration required)
                if (Y0 < 0.0) {
                    if (YTemp < 0.0) {
                        X0 = XTemp;
                        Y0 = YTemp;
                    } else {
                        X1 = XTemp;
                        Y1 = YTemp;
                    }
                } else {
                    if (YTemp < 0.0) {
                        X1 = XTemp;
                        Y1 = YTemp;
                    } else {
                        X0 = XTemp;
                        Y0 = YTemp;
                    }
                } // ( Y0 < 0 )

            } // (Cont)

        } // Cont

        if (Conv) {
            Flag = NIte;
        } else {
            Flag = -1;
        }
        XRes = XTemp;
        XX_0 = X0;
        XX_1 = X1;
    }
*/
} // namespace General

} // namespace EnergyPlus
