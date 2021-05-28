// EnergyPlus, Copyright (c) 1996-2021, The Board of Trustees of the University of Illinois,
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

#ifndef SOLVER_HPP
#define SOLVER_HPP

// C++ Headers
#include <unordered_map>

// ObjexxFCL Headers
#include <ObjexxFCL/Array2D.hh>

#include "AirflowNetwork/Properties.hpp"

#include <EnergyPlus/Data/BaseData.hh>

namespace EnergyPlus {

// Forward declarations
struct EnergyPlusData;

// define this variable to get new code, commenting should yield original
#define SKYLINE_MATRIX_REMOVE_ZERO_COLUMNS

namespace AirflowNetwork {

    int constexpr NrInt = 20; // Number of intervals for a large opening

    struct AirProperties
    {
        Real64 temperature{20.0};
        // Real64 pressure;      //{0.0}; // gage pressure
        Real64 humidityRatio{0.0};
        Real64 density{0.0};
        Real64 sqrtDensity{0.0};
        Real64 viscosity{AIRDYNAMICVISCOSITY(20.0)};

        explicit AirProperties(double const airDensity);
    };

    // Forward declaration
    struct AirflowElement;

    struct Solver
    {
        Solver() : PB(0.0)
        {
        }

        void allocate(EnergyPlusData &state);
        void initialize(EnergyPlusData &state);
        void setsky(EnergyPlusData &state);
        void airmov(EnergyPlusData &state);
        void solvzp(EnergyPlusData &state, int &ITER); // number of iterations
        void filjac(EnergyPlusData &state,
                    int const NNZE,  // number of nonzero entries in the "AU" array.
                    bool const LFLAG // if = 1, use laminar relationship (initialization).
        );

        void clear()
        {
            PB = 0.0;
            // LIST = 0;
            elements.clear();
            compnum.clear();
            properties.clear();
            AFECTL.deallocate();
            AFLOW2.deallocate();
            AFLOW.deallocate();
            PS.deallocate();
            PW.deallocate();
            SUMAF.deallocate();
            PZ.deallocate();
            ID.deallocate();
            IK.deallocate();
            AD.deallocate();
            AU.deallocate();

#ifdef SKYLINE_MATRIX_REMOVE_ZERO_COLUMNS
            newIK.deallocate();
            newAU.deallocate();
#endif
            SUMF.deallocate();
        }

        std::unordered_map<std::string, AirflowElement *> elements;
        std::unordered_map<std::string, int> compnum; // Stopgap until all the introspection is dealt with

        std::vector<AirProperties> properties;

        // int NetworkNumOfLinks;
        // int NetworkNumOfNodes;

        // int const NrInt; // Number of intervals for a large opening

        // Common block AFEDAT
        Array1D<Real64> AFECTL; // This gets used in calculate, encapsulation fail
        Array1D<Real64> AFLOW2;
        Array1D<Real64> AFLOW; // This gets used in calculate, encapsulation fail
        Array1D<Real64> PS;
        Array1D<Real64> PW;

        // Common block CONTRL
        Real64 PB;
        // int LIST;

        // Common block ZONL
        // Array1D<Real64> RHOZ;
        // Array1D<Real64> SQRTDZ;
        // Array1D<Real64> VISCZ;
        Array1D<Real64> SUMAF;
        // Array1D<Real64> TZ; // Temperature [C]
        // Array1D<Real64> WZ; // Humidity ratio [kg/kg]
        Array1D<Real64> PZ; // Pressure [Pa]

        // Other array variables
        Array1D_int ID;
        Array1D_int IK;
        Array1D<Real64> AD;
        Array1D<Real64> AU;

#ifdef SKYLINE_MATRIX_REMOVE_ZERO_COLUMNS
        Array1D_int newIK;     // noel
        Array1D<Real64> newAU; // noel
#endif

        // REAL(r64), ALLOCATABLE, DIMENSION(:) :: AL
        Array1D<Real64> SUMF;
    };

    // Functions

    int GenericCrack(EnergyPlusData &state,
                     Real64 &coef,               // Flow coefficient
                     Real64 const expn,          // Flow exponent
                     bool const LFLAG,           // Initialization flag.If = 1, use laminar relationship
                     Real64 const PDROP,         // Total pressure drop across a component (P1 - P2) [Pa]
                     const AirProperties &propN, // Node 1 properties
                     const AirProperties &propM, // Node 2 properties
                     std::array<Real64, 2> &F,   // Airflow through the component [kg/s]
                     std::array<Real64, 2> &DF   // Partial derivative:  DF/DP
    );

    int GenericDuct(Real64 const Length,        // Duct length
                    Real64 const Diameter,      // Duct diameter
                    bool const LFLAG,           // Initialization flag.If = 1, use laminar relationship
                    Real64 const PDROP,         // Total pressure drop across a component (P1 - P2) [Pa]
                    const AirProperties &propN, // Node 1 properties
                    const AirProperties &propM, // Node 2 properties
                    std::array<Real64, 2> &F,   // Airflow through the component [kg/s]
                    std::array<Real64, 2> &DF   // Partial derivative:  DF/DP
    );

    void FACSKY(EnergyPlusData &state,
                Array1D<Real64> &AU,   // the upper triangle of [A] before and after factoring
                Array1D<Real64> &AD,   // the main diagonal of [A] before and after factoring
                Array1D<Real64> &AL,   // the lower triangle of [A] before and after factoring
                const Array1D_int &IK, // pointer to the top of column/row "K"
                int const NEQ,         // number of equations
                int const NSYM         // symmetry:  0 = symmetric matrix, 1 = non-symmetric
    );

    void SLVSKY(EnergyPlusData &state,
                const Array1D<Real64> &AU, // the upper triangle of [A] before and after factoring
                const Array1D<Real64> &AD, // the main diagonal of [A] before and after factoring
                const Array1D<Real64> &AL, // the lower triangle of [A] before and after factoring
                Array1D<Real64> &B,        // "B" vector (input); "X" vector (output).
                const Array1D_int &IK,     // pointer to the top of column/row "K"
                int const NEQ,             // number of equations
                int const NSYM             // symmetry:  0 = symmetric matrix, 1 = non-symmetric
    );

    void FILSKY(EnergyPlusData &state,
                const Array1D<Real64> &X,    // element array (row-wise sequence)
                std::array<int, 2> const LM, // location matrix
                const Array1D_int &IK,       // pointer to the top of column/row "K"
                Array1D<Real64> &AU,         // the upper triangle of [A] before and after factoring
                Array1D<Real64> &AD,         // the main diagonal of [A] before and after factoring
                int const FLAG               // mode of operation
    );

    void DUMPVD(std::string const &S,     // Description
                const Array1D<Real64> &V, // Output values
                int const n,              // Array size
                std::ostream &UOUT        // Output file
    );

    void DUMPVR(std::string const &S,     // Description
                const Array1D<Real64> &V, // Output values
                int const n,              // Array size
                std::ostream &UOUT        // Output file
    );

    void PresProfile(EnergyPlusData &state,
                     int const il,                  // Linkage number
                     int const Pprof,               // Opening number
                     Real64 const G,                // gravitation field strength [N/kg]
                     const Array1D<Real64> &DpF,    // Stack pressures at start heights of Layers
                     const Array1D<Real64> &DpT,    // Stack pressures at start heights of Layers
                     const Array1D<Real64> &BetaF,  // Density gradients in the FROM zone (starting at linkheight) [Kg/m3/m]
                     const Array1D<Real64> &BetaT,  // Density gradients in the TO zone (starting at linkheight) [Kg/m3/m]
                     const Array1D<Real64> &RhoStF, // Density at the start heights of Layers in the FROM zone
                     const Array1D<Real64> &RhoStT, // Density at the start heights of Layers in the TO zone
                     int const From,                // Number of FROM zone
                     int const To,                  // Number of To zone
                     Real64 const ActLh,            // Actual height of opening [m]
                     Real64 const OwnHeightFactor   // Cosine of deviation angle of the opening plane from the vertical direction
    );

    void PStack(EnergyPlusData &state);

    Real64 psz(Real64 const Pz0,  // Pressure at altitude z0 [Pa]
               Real64 const Rho0, // density at altitude z0 [kg/m3]
               Real64 const beta, // density gradient [kg/m4]
               Real64 const z0,   // reference altitude [m]
               Real64 const z,    // altitude[m]
               Real64 const g     // gravity field strength [N/kg]
    );

    void LClimb(EnergyPlusData &state,
                Real64 const G,   // gravity field strength [N/kg]
                Real64 &Rho,      // Density link level (initialized with rho zone) [kg/m3]
                Real64 const Z,   // Height of the link above the zone reference [m]
                Real64 &T,        // temperature at link level [C]
                Real64 &X,        // absolute humidity at link level [kg/kg]
                Real64 &Dp,       // Stackpressure to the linklevel [Pa]
                int const zone,   // Zone number
                Real64 const PZ,  // Zone Pressure (reflevel) [Pa]
                Real64 const Pbz, // Barometric pressure at entrance level [Pa]
                Real64 &RhoDr     // Air density of dry air on the link level used
    );

    //*****************************************************************************************

} // namespace AirflowNetwork

struct AirflowNetworkSolverData : BaseGlobalStruct
{
    AirflowNetwork::Solver solver;

    // Data
    int NetworkNumOfLinks = 0;
    int NetworkNumOfNodes = 0;

    // Common block AFEDAT
    Array1D<Real64> AFECTL;
    Array1D<Real64> AFLOW2;
    Array1D<Real64> AFLOW;
    Array1D<Real64> PS;
    Array1D<Real64> PW;

    // Common block CONTRL
    Real64 PB = 0.0;
    int LIST = 0;

    // Common block ZONL
    // Array1D<Real64> RHOZ;
    // Array1D<Real64> SQRTDZ;
    // Array1D<Real64> VISCZ;
    Array1D<Real64> SUMAF;
    // Array1D<Real64> TZ; // Temperature [C]
    // Array1D<Real64> WZ; // Humidity ratio [kg/kg]
    Array1D<Real64> PZ; // Pressure [Pa]

    // Other array variables
    Array1D_int ID;
    Array1D_int IK;
    Array1D<Real64> AD;
    Array1D<Real64> AU;

#ifdef SKYLINE_MATRIX_REMOVE_ZERO_COLUMNS
    Array1D_int newIK;     // noel
    Array1D<Real64> newAU; // noel
#endif

    // REAL(r64), ALLOCATABLE, DIMENSION(:) :: AL
    Array1D<Real64> SUMF;

    // Large opening variables
    Array1D<Real64> DpProf;   // Differential pressure profile for Large Openings [Pa]
    Array1D<Real64> RhoProfF; // Density profile in FROM zone [kg/m3]
    Array1D<Real64> RhoProfT; // Density profile in TO zone [kg/m3]
    Array2D<Real64> DpL;      // Array of stack pressures in link

    void clear_state() override
    {
        NetworkNumOfLinks = 0;
        NetworkNumOfNodes = 0;
        AFECTL.clear();
        AFLOW2.clear();
        AFLOW.clear();
        PS.clear();
        PW.clear();
        PB = 0.0;
        LIST = 0;
        SUMAF.clear();
        PZ.clear();
        ID.clear();
        IK.clear();
        AD.clear();
        AU.clear();
        solver.clear();
    }
};

} // namespace EnergyPlus

#endif
