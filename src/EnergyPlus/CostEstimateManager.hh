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

#ifndef CostEstimateManager_hh_INCLUDED
#define CostEstimateManager_hh_INCLUDED

// EnergyPlus Headers
#include <EnergyPlus/EnergyPlus.hh>
#include <EnergyPlus/Data/BaseData.hh>

namespace EnergyPlus {

// Forward declarations
struct EnergyPlusData;

namespace CostEstimateManager {

    struct CostLineItemStruct
    {
        // Members
        std::string LineName;      // object name (needed ?)
        std::string LineType;      // Case statement driver?
        std::string ParentObjType; // parent reference to IDD object type
        std::string ParentObjName; // parent instance in IDF
        std::string ParentObjKey;  // end use key for parent object
        int ParentObjIDinList;
        Real64 PerSquareMeter;     // cost per square meter
        Real64 PerEach;            // cost per each
        Real64 PerKiloWattCap;     // cost per kW of nominal capacity
        Real64 PerKWCapPerCOP;     // cost per kW of nominal capacity per COP
        Real64 PerCubicMeter;      // cost per cubic meter
        Real64 PerCubMeterPerSec;  // cost per cubic meter per second
        Real64 PerUAinWattperDelK; // cost per (UA) in Watt/deltaK
        int LineNumber;      // number of line item in detail list
        Real64 Qty;          // quantity in calculations (can be input)
        std::string Units;   // Reported units
        Real64 ValuePer;     // Cost used in final calculation
        Real64 LineSubTotal; // line item total  Qty * ValuePer

        // Default Constructor
        CostLineItemStruct()
            : ParentObjIDinList(1), PerSquareMeter(0.0), PerEach(0.0), PerKiloWattCap(0.0), PerKWCapPerCOP(0.0), PerCubicMeter(0.0),
              PerCubMeterPerSec(0.0), PerUAinWattperDelK(0.0), LineNumber(-1), Qty(0.0), ValuePer(0.0), LineSubTotal(0.0)
        {
        }
    };

    void SimCostEstimate(EnergyPlusData &state);

    void GetCostEstimateInput(EnergyPlusData &state);

    void CheckCostEstimateInput(EnergyPlusData &state, bool &ErrorsFound); // Set to true if errors in input, fatal at end of routine

    void CalcCostEstimate(EnergyPlusData &state);

} // namespace CostEstimateManager

struct CostEstimateManagerData : BaseGlobalStruct {

    bool GetCostInput = true;
    int NumLineItems = 0 ;        // number of cost estimate line items
    bool DoCostEstimate = false;  // set to true if any cost estimating needed
    int numMonetaryUnit = 0;
    int selectedMonetaryUnit = 0;

    Array1D<CostEstimateManager::CostLineItemStruct> CostLineItem;

    void clear_state() override
    {
        GetCostInput = true;
        NumLineItems = 0;
        DoCostEstimate = false;
        numMonetaryUnit = 0;
        selectedMonetaryUnit = 0;
    }
};

} // namespace EnergyPlus

#endif
