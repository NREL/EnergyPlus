// EnergyPlus, Copyright (c) 1996-2018, The Board of Trustees of the University of Illinois,
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

// EnergyPlus Headers
#include <DataCostEstimate.hh>
#include <DataPrecisionGlobals.hh>

namespace EnergyPlus {

namespace DataCostEstimate {
    // PURPOSE OF THIS MODULE:
    // This data-only module is a repository for Cost Estimation variables which are considered
    // to be "global" in nature in EnergyPlus.

    // METHODOLOGY EMPLOYED:

    // REFERENCES:

    // OTHER NOTES:

    // Using/Aliasing
    using namespace DataPrecisionGlobals;

    // Data
    // -only module should be available to other modules and routines.
    // Thus, all variables in this module must be PUBLIC.

    // MODULE PARAMETER DEFINITIONS:
    // na

    // DERIVED TYPE DEFINITIONS

    // INTERFACE BLOCK SPECIFICATIONS
    // na

    // MODULE VARIABLE DECLARATIONS:

    // CurntBldg holds results for current bldg. cost estimate
    // RefrnceBldg holds user input for comparison.

    int NumLineItems(0);        // number of cost estimate line items
    bool DoCostEstimate(false); // set to true if any cost estimating needed

    int numMonetaryUnit(0);
    int selectedMonetaryUnit(0);

    // Object Data
    Array1D<CostLineItemStruct> CostLineItem;
    CostAdjustmentStruct CurntBldg(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0); // holds total from line item cost calculations | holds user-defined
                                                                                 // constant cost model | holds user-defined fraction for design fees
                                                                                 // | holds user-defined fraction for contractor fees | holds
                                                                                 // user-defined fraction for contingencies | holds user-defined
                                                                                 // fraction for bonding costs | holds user-defined fraction for
                                                                                 // commissioning costs | holds user-defined multiplier to account for
                                                                                 // regional diffs | the Grand Total of all line items plus all other
                                                                                 // costs
    CostAdjustmentStruct RefrncBldg(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0); // holds total from line item cost calculations | holds user-defined
                                                                                  // constant cost model | holds user-defined fraction for design fees
                                                                                  // | holds user-defined fraction for contractor fees | holds
                                                                                  // user-defined fraction for contingencies | holds user-defined
                                                                                  // fraction for bonding costs | holds user-defined fraction for
                                                                                  // commissioning costs | holds user-defined multiplier to account
                                                                                  // for regional diffs | the Grand Total of all line items plus all
                                                                                  // other costs
    Array1D<monetaryUnitType> monetaryUnit;

} // namespace DataCostEstimate

} // namespace EnergyPlus
