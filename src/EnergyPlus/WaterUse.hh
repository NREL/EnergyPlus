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

#ifndef WaterUse_hh_INCLUDED
#define WaterUse_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/EnergyPlus.hh>
#include <EnergyPlus/PlantComponent.hh>

namespace EnergyPlus {

// Forward declarations
struct EnergyPlusData;

namespace WaterUse {

    enum struct HeatRecoveryHXEnum
    {
        Ideal,
        CounterFlow,
        CrossFlow
    };

    enum struct HeatRecoveryConfigEnum
    {
        Plant,
        Equipment,
        PlantAndEquip
    };

    struct WaterEquipmentType
    {
        std::string Name; // Name of DHW
        std::string EndUseSubcatName;
        int Connections;          // Index for WATER USE CONNECTIONS object
        Real64 PeakVolFlowRate;   // Peak volumetric flow rate, also water consumption rate (m3/s)
        int FlowRateFracSchedule; // Pointer to schedule object
        Real64 ColdVolFlowRate;
        Real64 HotVolFlowRate;
        Real64 TotalVolFlowRate; // Volumetric flow rate, also water consumption rate (m3/s)
        Real64 ColdMassFlowRate;
        Real64 HotMassFlowRate;
        Real64 TotalMassFlowRate; // Mass flow rate (kg/s)
        Real64 DrainMassFlowRate;
        int ColdTempSchedule;   // Index for schedule object
        int HotTempSchedule;    // Index for schedule object
        int TargetTempSchedule; // Index for schedule object
        Real64 ColdTemp;        // Cold supply water temperature (C)
        Real64 HotTemp;         // Hot supply water temperature (C)
        Real64 TargetTemp;      // Target (mixed) water temperature (C)
        Real64 MixedTemp;       // Actual outlet (mixed) water temperature (C)
        Real64 DrainTemp;
        int Zone;                 // Index for zone object
        int SensibleFracSchedule; // Pointer to schedule object
        Real64 SensibleRate;
        Real64 SensibleEnergy;
        Real64 SensibleRateNoMultiplier;
        int LatentFracSchedule; // Pointer to schedule object
        Real64 LatentRate;
        Real64 LatentEnergy;
        Real64 LatentRateNoMultiplier;
        Real64 MoistureRate;
        Real64 MoistureMass;
        Real64 ColdVolume;  // Water consumption (m3)
        Real64 HotVolume;   // Water consumption (m3)
        Real64 TotalVolume; // Water consumption (m3)
        Real64 Power;       // Heating rate required to meet the mixed water temperature (W)
        Real64 Energy;      // Heating energy required to meet the mixed water temperature (J)
        bool setupMyOutputVars;

        WaterEquipmentType()
            : Connections(0), PeakVolFlowRate(0.0), FlowRateFracSchedule(0), ColdVolFlowRate(0.0), HotVolFlowRate(0.0), TotalVolFlowRate(0.0),
              ColdMassFlowRate(0.0), HotMassFlowRate(0.0), TotalMassFlowRate(0.0), DrainMassFlowRate(0.0), ColdTempSchedule(0), HotTempSchedule(0),
              TargetTempSchedule(0), ColdTemp(0.0), HotTemp(0.0), TargetTemp(0.0), MixedTemp(0.0), DrainTemp(0.0), Zone(0), SensibleFracSchedule(0),
              SensibleRate(0.0), SensibleEnergy(0.0), SensibleRateNoMultiplier(0.0), LatentFracSchedule(0), LatentRate(0.0), LatentEnergy(0.0),
              LatentRateNoMultiplier(0.0), MoistureRate(0.0), MoistureMass(0.0), ColdVolume(0.0), HotVolume(0.0), TotalVolume(0.0), Power(0.0),
              Energy(0.0), setupMyOutputVars(true)
        {
        }

        // Reset Some Values to Zeros
        void reset()
        {
            SensibleRate = 0.0;
            SensibleEnergy = 0.0;
            LatentRate = 0.0;
            LatentEnergy = 0.0;
            MixedTemp = 0.0;
            TotalMassFlowRate = 0.0;
            DrainTemp = 0.0;
        }

        void CalcEquipmentFlowRates(EnergyPlusData &state);

        void CalcEquipmentDrainTemp(EnergyPlusData &state);

        void setupOutputVars(EnergyPlusData &state);
    };

    struct WaterConnectionsType : PlantComponent
    {
        std::string Name; // Name of DHW
        bool Init;        // Flag for initialization:  TRUE means do the init
        bool InitSizing;  // Flag for initialization of plant sizing
        bool StandAlone;  // Flag for operation with no plant connections
        int InletNode;    // Hot water demand node
        int OutletNode;   // Cold water supply node
        int SupplyTankNum;
        int RecoveryTankNum;
        int TankDemandID; // array to request flow from supply tank
        int TankSupplyID; // array to send flow to recovery tank
        bool HeatRecovery;
        HeatRecoveryHXEnum HeatRecoveryHX;
        HeatRecoveryConfigEnum HeatRecoveryConfig;
        Real64 HXUA;
        Real64 Effectiveness;
        Real64 RecoveryRate;
        Real64 RecoveryEnergy;
        Real64 MainsMassFlowRate; // Mass flow rate (kg/s)
        Real64 TankMassFlowRate;  // Mass flow rate (kg/s)
        Real64 ColdMassFlowRate;  // Mass flow rate (kg/s)  cold = mains + tank
        Real64 HotMassFlowRate;   // Mass flow rate (kg/s)
        Real64 TotalMassFlowRate; // Mass flow rate (kg/s) total = cold + hot
        Real64 DrainMassFlowRate;
        Real64 RecoveryMassFlowRate;
        Real64 PeakVolFlowRate;  // Volumetric flow rate, also water consumption rate (m3/s)
        Real64 MainsVolFlowRate; // Volumetric flow rate, also water consumption rate (m3/s)
        Real64 TankVolFlowRate;  // Volumetric flow rate, also water consumption rate (m3/s)
        Real64 ColdVolFlowRate;  // Volumetric flow rate, also water consumption rate (m3/s)
        Real64 HotVolFlowRate;   // Volumetric flow rate, also water consumption rate (m3/s)
        Real64 TotalVolFlowRate; // Volumetric flow rate, also water consumption rate (m3/s)
        Real64 DrainVolFlowRate;
        Real64 PeakMassFlowRate; // Peak Mass flow rate for MassFlowRateMax
        int ColdTempSchedule;    // Index for schedule object
        int HotTempSchedule;     // Index for schedule object
        Real64 MainsTemp;        // Cold supply water temperature (C)
        Real64 TankTemp;         // Cold supply water temperature (C)
        Real64 ColdSupplyTemp;   // cold from mains, schedule, or tank, depending
        Real64 ColdTemp;         // Cold supply water temperature (C)  actual cold (could be reheated)
        Real64 HotTemp;          // Hot supply water temperature (C)
        Real64 DrainTemp;
        Real64 RecoveryTemp;
        Real64 ReturnTemp;
        Real64 WasteTemp;
        Real64 TempError;
        Real64 MainsVolume; // Water consumption (m3)
        Real64 TankVolume;  // Water consumption (m3)
        Real64 ColdVolume;  // Water consumption (m3)
        Real64 HotVolume;   // Water consumption (m3)
        Real64 TotalVolume; // Water consumption (m3)
        Real64 Power;       // Heating rate required to raise temperature from cold to hot (W)
        Real64 Energy;      // Heating energy required to raise temperature from cold to hot (J)
        int NumWaterEquipment;
        int MaxIterationsErrorIndex; // recurring error index
        Array1D_int myWaterEquipArr;
        int PlantLoopNum;
        int PlantLoopSide;
        int PlantLoopBranchNum;
        int PlantLoopCompNum;
        bool MyEnvrnFlag;
        bool setupMyOutputVars;
        bool plantScanFlag;

        WaterConnectionsType()
            : Init(true), InitSizing(true), StandAlone(false), InletNode(0), OutletNode(0), SupplyTankNum(0), RecoveryTankNum(0), TankDemandID(0),
              TankSupplyID(0), HeatRecovery(false), HeatRecoveryHX(HeatRecoveryHXEnum::Ideal), HeatRecoveryConfig(HeatRecoveryConfigEnum::Plant),
              HXUA(0.0), Effectiveness(0.0), RecoveryRate(0.0), RecoveryEnergy(0.0), MainsMassFlowRate(0.0), TankMassFlowRate(0.0),
              ColdMassFlowRate(0.0), HotMassFlowRate(0.0), TotalMassFlowRate(0.0), DrainMassFlowRate(0.0), RecoveryMassFlowRate(0.0),
              PeakVolFlowRate(0.0), MainsVolFlowRate(0.0), TankVolFlowRate(0.0), ColdVolFlowRate(0.0), HotVolFlowRate(0.0), TotalVolFlowRate(0.0),
              DrainVolFlowRate(0.0), PeakMassFlowRate(0.0), ColdTempSchedule(0), HotTempSchedule(0), MainsTemp(0.0), TankTemp(0.0),
              ColdSupplyTemp(0.0), ColdTemp(0.0), HotTemp(0.0), DrainTemp(0.0), RecoveryTemp(0.0), ReturnTemp(0.0), WasteTemp(0.0), TempError(0.0),
              MainsVolume(0.0), TankVolume(0.0), ColdVolume(0.0), HotVolume(0.0), TotalVolume(0.0), Power(0.0), Energy(0.0), NumWaterEquipment(0),
              MaxIterationsErrorIndex(0), PlantLoopNum(0), PlantLoopSide(0), PlantLoopBranchNum(0), PlantLoopCompNum(0), MyEnvrnFlag(true),
              setupMyOutputVars(true), plantScanFlag(true)
        {
        }

        static PlantComponent *factory(EnergyPlusData &state, std::string const &objectName);

        void simulate([[maybe_unused]] EnergyPlusData &state,
                      const PlantLocation &calledFromLocation,
                      bool FirstHVACIteration,
                      Real64 &CurLoad,
                      bool RunFlag) override;

        void InitConnections(EnergyPlusData &state);

        void CalcConnectionsFlowRates(EnergyPlusData &state, bool FirstHVACIteration);

        void CalcConnectionsDrainTemp(EnergyPlusData &state);

        void CalcConnectionsHeatRecovery(EnergyPlusData &state);

        void UpdateWaterConnections(EnergyPlusData &state);

        void ReportWaterUse(EnergyPlusData &state);

        void setupOutputVars([[maybe_unused]] EnergyPlusData &state);
    };

    void SimulateWaterUse(EnergyPlusData &state, bool FirstHVACIteration);

    void GetWaterUseInput(EnergyPlusData &state);

    void ReportStandAloneWaterUse(EnergyPlusData &state);

    void CalcWaterUseZoneGains(EnergyPlusData &state);

} // namespace WaterUse

struct WaterUseData : BaseGlobalStruct
{

    int numWaterEquipment;
    int numWaterConnections;
    bool getWaterUseInputFlag;
    bool MyEnvrnFlagLocal;
    Array1D_bool CheckEquipName;
    Array1D<WaterUse::WaterEquipmentType> WaterEquipment;
    EPVector<WaterUse::WaterConnectionsType> WaterConnections;

    void clear_state() override
    {
        this->numWaterEquipment = 0;
        this->numWaterConnections = 0;
        this->getWaterUseInputFlag = true;
        this->MyEnvrnFlagLocal = true;
        this->CheckEquipName.deallocate();
        this->WaterEquipment.deallocate();
        this->WaterConnections.deallocate();
    }

    // Default Constructor
    WaterUseData() : numWaterEquipment(0), numWaterConnections(0), getWaterUseInputFlag(true), MyEnvrnFlagLocal(true)
    {
    }
};

} // namespace EnergyPlus

#endif
