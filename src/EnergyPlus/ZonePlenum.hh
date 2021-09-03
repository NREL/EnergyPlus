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

#ifndef ZonePlenum_hh_INCLUDED
#define ZonePlenum_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/EPVector.hh>
#include <EnergyPlus/EnergyPlus.hh>

namespace EnergyPlus {

// Forward declarations
struct EnergyPlusData;

namespace ZonePlenum {

    struct ZoneReturnPlenumConditions
    {
        // Members
        std::string ZonePlenumName;
        std::string ZoneName;
        std::string ZoneNodeName;
        Real64 ZoneTemp;
        Real64 ZoneHumRat;
        Real64 ZoneEnthalpy;
        Real64 OutletTemp;
        Real64 OutletHumRat;
        Real64 OutletEnthalpy;
        Real64 OutletPressure;
        int ZoneNodeNum;
        int ActualZoneNum;
        int OutletNode;
        Real64 OutletMassFlowRate;         // MassFlow through the ZonePlenum being Simulated [kg/Sec]
        Real64 OutletMassFlowRateMaxAvail; // [kg/Sec]
        Real64 OutletMassFlowRateMinAvail; // [kg/Sec]
        int NumInducedNodes;
        Array1D_int InducedNode;
        Array1D<Real64> InducedMassFlowRate;
        Array1D<Real64> InducedMassFlowRateMaxAvail;
        Array1D<Real64> InducedMassFlowRateMinAvail;
        Array1D<Real64> InducedTemp;
        Array1D<Real64> InducedHumRat;
        Array1D<Real64> InducedEnthalpy;
        Array1D<Real64> InducedPressure;
        Array1D<Real64> InducedCO2;
        Array1D<Real64> InducedGenContam;
        bool InitFlag;
        int NumInletNodes;
        Array1D_int InletNode;
        Array1D<Real64> InletMassFlowRate;
        Array1D<Real64> InletMassFlowRateMaxAvail;
        Array1D<Real64> InletMassFlowRateMinAvail;
        Array1D<Real64> InletTemp;
        Array1D<Real64> InletHumRat;
        Array1D<Real64> InletEnthalpy;
        Array1D<Real64> InletPressure;
        Array1D_int ADUIndex;  // index to AirDistUnit leaking to this plenum
        int NumADUs;           // number of ADU's that can leak to this plenum
        Array1D_int ZoneEqNum; // list of zone equip config indices for this plenum

        // Default Constructor
        ZoneReturnPlenumConditions()
            : ZoneTemp(0.0), ZoneHumRat(0.0), ZoneEnthalpy(0.0), OutletTemp(0.0), OutletHumRat(0.0), OutletEnthalpy(0.0), OutletPressure(0.0),
              ZoneNodeNum(0), ActualZoneNum(0), OutletNode(0), OutletMassFlowRate(0.0), OutletMassFlowRateMaxAvail(0.0),
              OutletMassFlowRateMinAvail(0.0), NumInducedNodes(0), InitFlag(false), NumInletNodes(0)
        {
        }
    };

    struct ZoneSupplyPlenumConditions
    {
        // Members
        std::string ZonePlenumName;
        std::string ZoneName;
        std::string ZoneNodeName;
        Real64 ZoneTemp;
        Real64 ZoneHumRat;
        Real64 ZoneEnthalpy;
        Real64 InletTemp;
        Real64 InletHumRat;
        Real64 InletEnthalpy;
        Real64 InletPressure;
        int ZoneNodeNum;
        int ActualZoneNum;
        int InletNode;
        Real64 InletMassFlowRate;         // MassFlow through the ZonePlenum being Simulated [kg/Sec]
        Real64 InletMassFlowRateMaxAvail; // [kg/Sec]
        Real64 InletMassFlowRateMinAvail; // [kg/Sec]
        bool InitFlag;
        int NumOutletNodes;
        Array1D_int OutletNode;
        Array1D<Real64> OutletMassFlowRate;
        Array1D<Real64> OutletMassFlowRateMaxAvail;
        Array1D<Real64> OutletMassFlowRateMinAvail;
        Array1D<Real64> OutletTemp;
        Array1D<Real64> OutletHumRat;
        Array1D<Real64> OutletEnthalpy;
        Array1D<Real64> OutletPressure;

        // Default Constructor
        ZoneSupplyPlenumConditions()
            : ZoneTemp(0.0), ZoneHumRat(0.0), ZoneEnthalpy(0.0), InletTemp(0.0), InletHumRat(0.0), InletEnthalpy(0.0), InletPressure(0.0),
              ZoneNodeNum(0), ActualZoneNum(0), InletNode(0), InletMassFlowRate(0.0), InletMassFlowRateMaxAvail(0.0), InletMassFlowRateMinAvail(0.0),
              InitFlag(false), NumOutletNodes(0)
        {
        }
    };

    // Functions

    void SimAirZonePlenum(EnergyPlusData &state,
                          std::string_view CompName,
                          int const iCompType,
                          int &CompIndex,
                          Optional_bool_const FirstHVACIteration = _, // Autodesk:OPTIONAL Used without PRESENT check
                          Optional_bool_const FirstCall = _,          // Autodesk:OPTIONAL Used without PRESENT check
                          Optional_bool PlenumInletChanged = _        // Autodesk:OPTIONAL Used without PRESENT check
    );

    void GetZonePlenumInput(EnergyPlusData &state);

    void InitAirZoneReturnPlenum(EnergyPlusData &state, int const ZonePlenumNum);

    void InitAirZoneSupplyPlenum(EnergyPlusData &state, int const ZonePlenumNum, bool const FirstHVACIteration, bool const FirstCall);

    void CalcAirZoneReturnPlenum(EnergyPlusData &state, int const ZonePlenumNum);

    void CalcAirZoneSupplyPlenum(EnergyPlusData &state, int const ZonePlenumNum, bool const FirstCall);

    void UpdateAirZoneReturnPlenum(EnergyPlusData &state, int const ZonePlenumNum);

    void UpdateAirZoneSupplyPlenum(EnergyPlusData &state, int const ZonePlenumNum, bool &PlenumInletChanged, bool const FirstCall);

    int GetReturnPlenumIndex(EnergyPlusData &state, int const &ExNodeNum);

    void GetReturnPlenumName(EnergyPlusData &state, int const &ReturnPlenumIndex, std::string &ReturnPlenumName);

    int getReturnPlenumIndexFromInletNode(EnergyPlusData &state, int const &InNodeNum);

    bool ValidateInducedNode(EnergyPlusData &state, int const InduceNodeNum, int const NumReturnNodes, Array1D<int> const &ReturnNode);

} // namespace ZonePlenum

struct ZonePlenumData : BaseGlobalStruct
{

    bool GetInputFlag; // Flag set to make sure you get input once
    bool InitAirZoneReturnPlenumEnvrnFlag;
    bool InitAirZoneReturnPlenumOneTimeFlag;

    int NumZonePlenums;       // The Number of ZonePlenums found in the Input
    int NumZoneReturnPlenums; // The Number of ZoneReturnPlenums found in the Input
    int NumZoneSupplyPlenums; // The Number of ZoneSupplyPlenums found in the Input
    Array1D_bool CheckRetEquipName;
    Array1D_bool CheckSupEquipName;

    // Object Data
    EPVector<ZonePlenum::ZoneReturnPlenumConditions> ZoneRetPlenCond;
    EPVector<ZonePlenum::ZoneSupplyPlenumConditions> ZoneSupPlenCond;
    bool MyEnvrnFlag = true;

    void clear_state() override
    {
        this->GetInputFlag = true;
        this->InitAirZoneReturnPlenumEnvrnFlag = true;
        this->InitAirZoneReturnPlenumOneTimeFlag = true;
        this->NumZonePlenums = 0;
        this->NumZoneReturnPlenums = 0;
        this->NumZoneSupplyPlenums = 0;
        this->ZoneRetPlenCond.deallocate();
        this->ZoneSupPlenCond.deallocate();
        this->MyEnvrnFlag = true;
    }

    // Default Constructor
    ZonePlenumData()
        : GetInputFlag(true), InitAirZoneReturnPlenumEnvrnFlag(true), InitAirZoneReturnPlenumOneTimeFlag(true), NumZonePlenums(0),
          NumZoneReturnPlenums(0), NumZoneSupplyPlenums(0)
    {
    }
};

} // namespace EnergyPlus

#endif
