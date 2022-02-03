// EnergyPlus, Copyright (c) 1996-2022, The Board of Trustees of the University of Illinois,
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

#ifndef ExhaustAirSystemManager_hh_INCLUDED
#define ExhaustAirSystemManager_hh_INCLUDED

// EnergyPlus Headers
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/EnergyPlus.hh>

namespace EnergyPlus {

// Forward declarations
struct EnergyPlusData;

namespace ExhaustAirSystemManager {

    // 2022-01-28: May move this definition to ExhaustAirSystemManager.hh
    struct ExhaustAir
    {
        // Members
        std::string Name;
        // int NumOfComponents;
        // int OutletNodeNum;

        // 2022-01-20: Exhaust system does not need to be specified this way, so the following lines
        // should be replaced by some new variables, which might be simpler to handle.
        // Array1D_string ComponentType; // TODO: Convert this from string to enum and remove ComponentTypeEnum below
        // Array1D<DataZoneEquipment::AirLoopHVACZone> ComponentTypeEnum;
        // Array1D_string ComponentName;
        // Array1D_int ComponentIndex;

        int AvailScheduleNum;
        std::string ZoneMixerName;
        int ZoneMixerIndex;
        int CentralFanTypeNum;
        std::string CentralFanName; // 2022-01: adding this seems to make the sim call easier
        int CentralFanIndex;

        // Default Constructor
        ExhaustAir() : AvailScheduleNum(0), ZoneMixerName(""), ZoneMixerIndex(0), CentralFanTypeNum(0), CentralFanName(""), CentralFanIndex(0)
        {
        }
    };

    // 2022-01-28: May move this definition to ExhaustAirSystemManager.hh as well
    struct ZoneExhaustControl
    {
        std::string Name;
        // int NumOfComponents; // not sure if this is necessary

        int AvailScheduleNum;

        std::string ZoneName;
        int ZoneNum;

        int InletNodeNum;
        int OutletNodeNum;

        Real64 DesignExhaustFlowRate;
        int FlowControlTypeNum; // for now define 0 = Scheduled; 1 = Follow-supply
        int ExhaustFlowFractionScheduleNum;
        int SupplyNodeOrNodelistNum;
        int MinZoneTempLimitScheduleNum;
        int MinExhFlowFracScheduleNum;
        int BalancedExhFracScheduleNum;

        // default constructor
        // Question: why the constructor skipped the first element std::string Name?
        ZoneExhaustControl()
            : AvailScheduleNum(0), ZoneName(""), ZoneNum(0), InletNodeNum(0), OutletNodeNum(0), DesignExhaustFlowRate(0.0), FlowControlTypeNum(0),
              ExhaustFlowFractionScheduleNum(0), SupplyNodeOrNodelistNum(0), MinZoneTempLimitScheduleNum(0), MinExhFlowFracScheduleNum(0),
              BalancedExhFracScheduleNum(0)
        {
        }
    };

    void SimExhaustAirSystem(EnergyPlusData &state, bool FirstHVACIteration);

    void GetExhaustAirSystemInput(EnergyPlusData &state);

    void InitExhaustAirSystem(int &ExhaustAirSystemNum); // maybe unused

    void CalcExhaustAirSystem(EnergyPlusData &state, int &ExhaustAirSystemNum, bool FirstHVACIteration);

    void ReportExhaustAirSystem(int &ExhaustAirSystemNum); // may condiser reactivate this for the exhaust system

    void GetZoneExhaustControlInput(EnergyPlusData &state);

    void SimZoneHVACExhaustControls(EnergyPlusData &state, bool FirstHVACIteration);

    void CalcZoneHVACExhaustControl(EnergyPlusData &state, int &ZoneHVACExhaustControlNum, bool FirstHVACIteration);

    void SizeExhaustSystem(EnergyPlusData &state);

    void SizeExhaustControlFlow(EnergyPlusData &state, int zoneExhCtrlNum, Array1D_int &NodeNums);
} // namespace ExhaustAirSystemManager

struct ExhaustAirSystemMgr : BaseGlobalStruct
{

    bool GetInputFlag = true;

    void clear_state() override
    {
        this->GetInputFlag = true;
    }
};

struct ExhaustControlSystemMgr : BaseGlobalStruct
{

    bool GetInputFlag = true;

    void clear_state() override
    {
        this->GetInputFlag = true;
    }
};

} // namespace EnergyPlus

#endif
