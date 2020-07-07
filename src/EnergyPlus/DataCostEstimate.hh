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

#ifndef DataCostEstimate_hh_INCLUDED
#define DataCostEstimate_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/EnergyPlus.hh>

namespace EnergyPlus {

namespace DataCostEstimate {

    struct CostAdjustmentStruct
    {
        // Members
        Real64 LineItemTot;        // = 0.0 ! holds total from line item cost calculations
        Real64 MiscCostperSqMeter; // = 0.0 ! holds user-defined constant cost model
        Real64 DesignFeeFrac;      // = 0.0 ! holds user-defined fraction for design fees
        Real64 ContractorFeeFrac;  // = 0.0 ! holds user-defined fraction for contractor fees
        Real64 ContingencyFrac;    // = 0.0 ! holds user-defined fraction for contingencies
        Real64 BondCostFrac;       // = 0.0 ! holds user-defined fraction for bonding costs
        Real64 CommissioningFrac;  // = 0.0 ! holds user-defined fraction for commissioning costs
        Real64 RegionalModifier;   // = 1.0 ! holds user-defined multiplier to account for regional diffs
        Real64 GrandTotal;         // = 0.0 ! the Grand Total of all line items plus all other costs

        // Default Constructor
        CostAdjustmentStruct()
        {
        }

        // Member Constructor
        CostAdjustmentStruct(Real64 const LineItemTot,        // = 0.0 ! holds total from line item cost calculations
                             Real64 const MiscCostperSqMeter, // = 0.0 ! holds user-defined constant cost model
                             Real64 const DesignFeeFrac,      // = 0.0 ! holds user-defined fraction for design fees
                             Real64 const ContractorFeeFrac,  // = 0.0 ! holds user-defined fraction for contractor fees
                             Real64 const ContingencyFrac,    // = 0.0 ! holds user-defined fraction for contingencies
                             Real64 const BondCostFrac,       // = 0.0 ! holds user-defined fraction for bonding costs
                             Real64 const CommissioningFrac,  // = 0.0 ! holds user-defined fraction for commissioning costs
                             Real64 const RegionalModifier,   // = 1.0 ! holds user-defined multiplier to account for regional diffs
                             Real64 const GrandTotal          // = 0.0 ! the Grand Total of all line items plus all other costs
                             )
            : LineItemTot(LineItemTot), MiscCostperSqMeter(MiscCostperSqMeter), DesignFeeFrac(DesignFeeFrac), ContractorFeeFrac(ContractorFeeFrac),
              ContingencyFrac(ContingencyFrac), BondCostFrac(BondCostFrac), CommissioningFrac(CommissioningFrac), RegionalModifier(RegionalModifier),
              GrandTotal(GrandTotal)
        {
        }
    };

    struct monetaryUnitType
    {
        // Members
        std::string code; // ISO code for currency such as USD or EUR
        std::string txt;  // text representation of the currency
        std::string html; // representation for HTML file - contains unicode references

        // Default Constructor
        monetaryUnitType()
        {
        }
    };

    // Object Data
    extern CostAdjustmentStruct CurntBldg;  // holds total from line item cost calculations | holds user-defined constant cost model | holds
                                            // user-defined fraction for design fees | holds user-defined fraction for contractor fees | holds
                                            // user-defined fraction for contingencies | holds user-defined fraction for bonding costs | holds
                                            // user-defined fraction for commissioning costs | holds user-defined multiplier to account for regional
                                            // diffs | the Grand Total of all line items plus all other costs
    extern CostAdjustmentStruct RefrncBldg; // holds total from line item cost calculations | holds user-defined constant cost model | holds
                                            // user-defined fraction for design fees | holds user-defined fraction for contractor fees | holds
                                            // user-defined fraction for contingencies | holds user-defined fraction for bonding costs | holds
                                            // user-defined fraction for commissioning costs | holds user-defined multiplier to account for regional
                                            // diffs | the Grand Total of all line items plus all other costs
    extern Array1D<monetaryUnitType> monetaryUnit;

} // namespace DataCostEstimate

} // namespace EnergyPlus

#endif
