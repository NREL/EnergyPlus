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

#ifndef ZoneEquipmentManager_hh_INCLUDED
#define ZoneEquipmentManager_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/DataGlobalConstants.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/EnergyPlus.hh>

namespace EnergyPlus {

// Forward declarations
struct EnergyPlusData;

namespace ZoneEquipmentManager {

    struct SimulationOrder
    {
        // Members
        std::string EquipType;
        int EquipType_Num;
        std::string EquipName;
        int EquipPtr;
        int CoolingPriority;
        int HeatingPriority;

        // Default Constructor
        SimulationOrder() : EquipType_Num(0), EquipPtr(0), CoolingPriority(0), HeatingPriority(0)
        {
        }
    };

    // Functions

    void ManageZoneEquipment(EnergyPlusData &state,
                             bool const FirstHVACIteration,
                             bool &SimZone, // Set to false at the end of the routine
                             bool &SimAir   // Eventually set to true via SimZoneEquipment if AirLoop must be resimulated
    );

    void GetZoneEquipment(EnergyPlusData &state);

    void InitZoneEquipment(EnergyPlusData &state, bool const FirstHVACIteration); // unused 1208

    void SizeZoneEquipment(EnergyPlusData &state);

    void SetUpZoneSizingArrays(EnergyPlusData &state);

    void RezeroZoneSizingArrays(EnergyPlusData &state);

    void UpdateZoneSizing(EnergyPlusData &state, DataGlobalConstants::CallIndicator const CallIndicator);

    void SimZoneEquipment(EnergyPlusData &state, bool const FirstHVACIteration, bool &SimAir);

    void SetZoneEquipSimOrder(EnergyPlusData &state, int const ControlledZoneNum, int const ActualZoneNum);

    void InitSystemOutputRequired(EnergyPlusData &state, int const ZoneNum, bool const FirstHVACIteration, bool const ResetSimOrder = false);

    void DistributeSystemOutputRequired(EnergyPlusData &state, int const ActualZoneNum, bool const FirstHVACIteration);

    void UpdateSystemOutputRequired(EnergyPlusData &state,
                                    int const ZoneNum,
                                    Real64 const SysOutputProvided,         // sensible output provided by zone equipment (W)
                                    Real64 const LatOutputProvided,         // latent output provided by zone equipment (kg/s)
                                    Optional_int_const EquipPriorityNum = _ // index in PrioritySimOrder for this update
    );

    void CalcZoneMassBalance(EnergyPlusData &state, bool const FirstHVACIteration);

    void CalcZoneReturnFlows(EnergyPlusData &state,
                             int const ZoneNum,
                             Real64 &ExpTotalReturnMassFlow,  // Expected total return air mass flow rate
                             Real64 &FinalTotalReturnMassFlow // Final total return air mass flow rate
    );

    void CalcZoneInfiltrationFlows(EnergyPlusData &state,
                                   int const ZoneNum,                // current zone index
                                   Real64 &ZoneReturnAirMassFlowRate // zone total zone return air mass flow rate
    );

    void CalcAirFlowSimple(EnergyPlusData &state,
                           int const SysTimestepLoop = 0,                    // System time step index
                           bool const AdjustZoneMixingFlowFlag = false,      // flags to adjust zone mxing mass flow rate
                           bool const AdjustZoneInfiltrationFlowFlag = false // flags to djust zone infiltration air flow rate
    );

    void GetStandAloneERVNodes(EnergyPlusData &state, int const OutdoorNum); // Zone Air Balance Outdoor index

    void CalcZoneMixingFlowRateOfReceivingZone(EnergyPlusData &state, int const ZoneNum, Real64 &ZoneMixingAirMassFlowRate);

    void CalcZoneMixingFlowRateOfSourceZone(EnergyPlusData &state, int const ZoneNum);

    void CalcZoneLeavingConditions(EnergyPlusData &state, bool const FirstHVACIteration);

    void UpdateZoneEquipment(EnergyPlusData &state, bool &SimAir);

    void CalcDOASSupCondsForSizing(EnergyPlusData &state,
                                   Real64 OutDB,        // outside air temperature [C]
                                   Real64 OutHR,        // outside humidity ratio [kg Water / kg Dry Air]
                                   int DOASControl,     // dedicated outside air control strategy
                                   Real64 DOASLowTemp,  // DOAS low setpoint [C]
                                   Real64 DOASHighTemp, // DOAS high setpoint [C]
                                   Real64 W90H, // humidity ratio at DOAS high setpoint temperature and 90% relative humidity [kg Water / kg Dry Air]
                                   Real64 W90L, // humidity ratio at DOAS low setpoint temperature and 90% relative humidity [kg Water / kg Dry Air]
                                   Real64 &DOASSupTemp, // DOAS supply temperature [C]
                                   Real64 &DOASSupHR    // DOAS Supply Humidity ratio [kg Water / kg Dry Air]
    );

    void AutoCalcDOASControlStrategy(EnergyPlusData &state);

    void ReportZoneSizingDOASInputs(EnergyPlusData &state,
                                    std::string const &ZoneName,         // the name of the zone
                                    std::string const &DOASCtrlStrategy, // DOAS control strategy
                                    Real64 const DOASLowTemp,            // DOAS design low setpoint temperature [C]
                                    Real64 const DOASHighTemp            // DOAS design high setpoint temperature [C]
    );

} // namespace ZoneEquipmentManager

struct ZoneEquipmentManagerData : BaseGlobalStruct
{

    Array1D<Real64> AvgData; // scratch array for storing averaged data
    int NumOfTimeStepInDay;  // number of zone time steps in a day
    bool GetZoneEquipmentInputFlag;
    bool SizeZoneEquipmentOneTimeFlag;

    Array1D<ZoneEquipmentManager::SimulationOrder> PrioritySimOrder;

    bool reportDOASZoneSizingHeader;
    bool InitZoneEquipmentOneTimeFlag;
    bool InitZoneEquipmentEnvrnFlag;
    bool FirstPassZoneEquipFlag; // indicates first pass through zone equipment, used to reset selected ZoneEqSizing variables

    void clear_state() override
    {
        this->SizeZoneEquipmentOneTimeFlag = true;
        this->InitZoneEquipmentOneTimeFlag = true;
        this->InitZoneEquipmentEnvrnFlag = true;
        this->AvgData.deallocate();   // scratch array for storing averaged data
        this->NumOfTimeStepInDay = 0; // number of zone time steps in a day
        this->GetZoneEquipmentInputFlag = true;
        this->PrioritySimOrder.deallocate();
        this->FirstPassZoneEquipFlag = true;
        this->reportDOASZoneSizingHeader = true;
    }

    // Default Constructor
    ZoneEquipmentManagerData()
        : NumOfTimeStepInDay(0), GetZoneEquipmentInputFlag(true), SizeZoneEquipmentOneTimeFlag(true), reportDOASZoneSizingHeader(true),
          InitZoneEquipmentOneTimeFlag(true), InitZoneEquipmentEnvrnFlag(true), FirstPassZoneEquipFlag(true)
    {
    }
};

} // namespace EnergyPlus

#endif
