// EnergyPlus, Copyright (c) 1996-2024, The Board of Trustees of the University of Illinois,
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

#ifndef ENERGYPLUS_PLANTLOOPHEATPUMPEIR_HH
#define ENERGYPLUS_PLANTLOOPHEATPUMPEIR_HH

// C++ headers
#include <functional>
#include <string>
#include <vector>

// EnergyPlus headers
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/Plant/PlantLocation.hh>
#include <EnergyPlus/PlantComponent.hh>

namespace EnergyPlus {

// Forward declarations
struct EnergyPlusData;

namespace EIRPlantLoopHeatPumps {

    // control heat pump on load or set point
    enum class ControlType
    {
        Invalid = -1,
        Setpoint,
        Load,
        Num
    };

    // Method of sizing heating capacity
    enum class HeatSizingType
    {
        Invalid = -1,
        Heating,
        Cooling,
        GreaterOfCoolingOrHeating,
        Num
    };

    // Defrost strategy
    enum class DefrostControl
    {
        Invalid = -1,
        None,
        Timed,          // uses fixed time reverse cycle defrost strategy
        OnDemand,       // uses outdoor humidity defrost timing
        TimedEmpirical, // uses outdoor temperature defrost timing
        Num,
    };

    struct InOutNodePair
    {
        int inlet;
        int outlet;

        InOutNodePair() : inlet(0), outlet(0)
        {
        }
    };

    struct EIRPlantLoopHeatPump : public EnergyPlus::PlantComponent
    {

        // fixed configuration parameters
        std::string name;
        DataPlant::PlantEquipmentType EIRHPType = DataPlant::PlantEquipmentType::Invalid;
        std::string companionCoilName;
        EIRPlantLoopHeatPump *companionHeatPumpCoil = nullptr;
        Real64 sizingFactor = 1.0;
        bool waterSource = false;
        bool airSource = false;
        ControlType sysControlType = ControlType::Invalid;
        DataPlant::FlowMode flowControl = DataPlant::FlowMode::Invalid;

        // sizing data
        Real64 heatSizingRatio = 1.0;
        HeatSizingType heatSizingMethod = HeatSizingType::Invalid;

        // reference data
        Real64 referenceCapacity = 0.0;
        bool referenceCapacityWasAutoSized = false;
        Real64 referenceCOP = 0.0;
        Real64 minimumPLR = 0.0;
        Real64 partLoadRatio = 0.0;
        Real64 cyclingRatio = 0.0;
        Real64 minSourceTempLimit = -999.0;
        Real64 maxSourceTempLimit = 999.0;

        // curve references
        int capFuncTempCurveIndex = 0;
        int powerRatioFuncTempCurveIndex = 0;
        int powerRatioFuncPLRCurveIndex = 0;
        int capacityDryAirCurveIndex = 0;
        int minSupplyWaterTempCurveIndex = 0;
        int maxSupplyWaterTempCurveIndex = 0;

        // flow rate terms
        Real64 loadSideDesignVolFlowRate = 0.0;
        bool loadSideDesignVolFlowRateWasAutoSized = false;
        Real64 sourceSideDesignVolFlowRate = 0.0;
        bool sourceSideDesignVolFlowRateWasAutoSized = false;
        Real64 loadSideDesignMassFlowRate = 0.0;
        Real64 sourceSideDesignMassFlowRate = 0.0;
        Real64 loadSideMassFlowRate = 0.0;
        Real64 sourceSideMassFlowRate = 0.0;
        Real64 loadVSPumpMinLimitMassFlow = 0.0;
        Real64 sourceVSPumpMinLimitMassFlow = 0.0;
        bool loadVSBranchPump = false;
        bool loadVSLoopPump = false;
        bool sourceVSBranchPump = false;
        bool sourceVSLoopPump = false;

        // simulation variables
        Real64 loadSideHeatTransfer = 0.0;
        Real64 sourceSideHeatTransfer = 0.0;
        Real64 loadSideInletTemp = 0.0;
        Real64 loadSideOutletTemp = 0.0;
        Real64 sourceSideInletTemp = 0.0;
        Real64 sourceSideOutletTemp = 0.0;
        Real64 powerUsage = 0.0;
        Real64 loadSideEnergy = 0.0;
        Real64 sourceSideEnergy = 0.0;
        Real64 powerEnergy = 0.0;
        // Real64 sourceSideCp = 0.0; // debugging variable
        bool running = false;

        // topology variables
        PlantLocation loadSidePlantLoc;
        PlantLocation sourceSidePlantLoc;
        InOutNodePair loadSideNodes;
        InOutNodePair sourceSideNodes;
        bool heatRecoveryHeatPump = false; // HP that transfers heat between plants and should not increase plant size

        // counters and indexes
        int condMassFlowRateTriggerIndex = 0;
        int recurringConcurrentOperationWarningIndex = 0;

        // logic flags
        bool oneTimeInitFlag = true;
        bool envrnInit = true;

        // recurrent warning messages index integers
        int capModFTErrorIndex = 0;
        int eirModFTErrorIndex = 0;
        int eirModFPLRErrorIndex = 0;

        // defrost
        DefrostControl defrostStrategy = DefrostControl::Invalid;
        Real64 defrostTime = 0.0;
        int defrostFreqCurveIndex = 0;
        int defrostHeatLoadCurveIndex = 0;
        int defrostHeatEnergyCurveIndex = 0;
        int defrostLoadCurveDims = 0;
        int defrostEnergyCurveDims = 0;
        int defrostEIRFTIndex = 0;
        bool defrostAvailable = false;
        Real64 loadDueToDefrost = 0.0;
        Real64 defrostEnergyRate = 0.0;
        Real64 defrostEnergy = 0.0;
        Real64 fractionalDefrostTime = 0.0;
        Real64 maxOutdoorTemperatureDefrost = 0.0;

        // a couple worker functions to easily allow merging of cooling and heating operations
        std::function<Real64(Real64, Real64)> calcLoadOutletTemp;
        std::function<Real64(Real64, Real64)> calcQsource;
        std::function<Real64(Real64, Real64)> calcSourceOutletTemp;

        virtual ~EIRPlantLoopHeatPump() = default;

        EIRPlantLoopHeatPump() = default;

        void
        simulate(EnergyPlusData &state, const PlantLocation &calledFromLocation, bool FirstHVACIteration, Real64 &CurLoad, bool RunFlag) override;

        void onInitLoopEquip([[maybe_unused]] EnergyPlusData &state, [[maybe_unused]] const PlantLocation &calledFromLocation) override;

        void getDesignCapacities(EnergyPlusData &state,
                                 [[maybe_unused]] const PlantLocation &calledFromLocation,
                                 [[maybe_unused]] Real64 &MaxLoad,
                                 [[maybe_unused]] Real64 &MinLoad,
                                 [[maybe_unused]] Real64 &OptLoad) override;

        virtual void doPhysics(EnergyPlusData &state, Real64 currentLoad);

        void sizeLoadSide(EnergyPlusData &state);

        void sizeSrcSideWSHP(EnergyPlusData &state);

        void sizeSrcSideASHP(EnergyPlusData &state);

        Real64 getLoadSideOutletSetPointTemp(EnergyPlusData &state) const;

        void setOperatingFlowRatesASHP(EnergyPlusData &state, bool FirstHVACIteration);

        void setOperatingFlowRatesWSHP(EnergyPlusData &state, bool FirstHVACIteration);

        virtual void resetReportingVariables();

        static PlantComponent *factory(EnergyPlusData &state, DataPlant::PlantEquipmentType hp_type, const std::string &hp_name);

        static void pairUpCompanionCoils(EnergyPlusData &state);

        static void processInputForEIRPLHP(EnergyPlusData &state);

        static void checkConcurrentOperation(EnergyPlusData &state);

        static Real64 add(Real64 const a, Real64 const b)
        {
            return a + b;
        }

        static Real64 subtract(Real64 const a, Real64 const b)
        {
            return a - b;
        }

        void isPlantInletOrOutlet(EnergyPlusData &state);

        void oneTimeInit(EnergyPlusData &state) override;
    };

    struct EIRFuelFiredHeatPump : public EIRPlantLoopHeatPump
    {
        // fixed configuration parameters
        // reference data
        // curve references
        // flow rate terms
        // simulation variables
        // topology variables
        // counters and indexes
        // logic flags
        // a couple worker functions to easily allow merging of cooling and heating operations

        // enum definitions for Fuel Fired only
        enum class OATempCurveVar
        {
            Invalid = -1,
            DryBulb,
            WetBulb,
            Num
        };

        enum class WaterTempCurveVar
        {
            Invalid = -1,
            EnteringCondenser,
            LeavingCondenser,
            EnteringEvaporator,
            LeavingEvaporator,
            Num
        };

        enum class DefrostType
        {
            Invalid = -1,
            Timed,
            OnDemand,
            Num
        };

        // New additions for GAHP only
        Constant::eFuel fuelType = Constant::eFuel::Invalid; // Fuel type assignment
        std::string endUseSubcat = "";
        DataPlant::FlowMode flowMode = DataPlant::FlowMode::Invalid;
        Real64 desSupplyTemp = 60.0;
        Real64 desTempLift = 11.1;
        OATempCurveVar oaTempCurveInputVar = OATempCurveVar::DryBulb;
        WaterTempCurveVar waterTempCurveInputVar = WaterTempCurveVar::EnteringCondenser;
        // int capFuncTempCurveIndex = 0;
        // int powerRatioFuncTempCurveIndex = 0;
        //  int powerRatioFuncPLRCurveIndex = 0;
        Real64 minPLR = 0.1;
        Real64 maxPLR = 1.0;

        int defrostEIRCurveIndex = 0;
        DefrostType defrostType = DefrostType::OnDemand;
        Real64 defrostOpTimeFrac = 0.0;
        Real64 defrostResistiveHeaterCap = 0.0;
        Real64 defrostMaxOADBT = 5.0;

        int cycRatioCurveIndex = 0;
        Real64 nominalAuxElecPower = 0.0;
        int auxElecEIRFoTempCurveIndex = 0;
        int auxElecEIRFoPLRCurveIndex = 0;
        Real64 standbyElecPower = 0.0;

        // new output variables for derived class only
        Real64 loadSideVolumeFlowRate = 0.0;
        Real64 fuelRate = 0.0;   // Unit in W
        Real64 fuelEnergy = 0.0; // Unit in J
        int capModFTErrorIndex = 0;
        int eirModFTErrorIndex = 0;
        int eirModFPLRErrorIndex = 0;
        int eirDefrostFTErrorIndex = 0;
        int eirAuxElecFTErrorIndex = 0;
        int eirAuxElecFPLRErrorIndex = 0;

        // Override parent methods to be declared
        void doPhysics(EnergyPlusData &state, Real64 currentLoad);
        void sizeSrcSideASHP(EnergyPlusData &state); // 2022-05-18: may not need this one
        void resetReportingVariables();
        static PlantComponent *factory(EnergyPlusData &state, DataPlant::PlantEquipmentType hp_type, const std::string &hp_name);
        static void pairUpCompanionCoils(EnergyPlusData &state);
        static void processInputForEIRPLHP(EnergyPlusData &state);
        void oneTimeInit(EnergyPlusData &state);

        // New or specialized functions for derived struct
        virtual ~EIRFuelFiredHeatPump() = default;
        EIRFuelFiredHeatPump() = default;
    };
} // namespace EIRPlantLoopHeatPumps

struct EIRPlantLoopHeatPumpsData : BaseGlobalStruct
{
    std::vector<EIRPlantLoopHeatPumps::EIRPlantLoopHeatPump> heatPumps;
    bool getInputsPLHP = true;
    void clear_state() override
    {
        new (this) EIRPlantLoopHeatPumpsData();
    }
};

struct EIRFuelFiredHeatPumpsData : BaseGlobalStruct
{
    std::vector<EIRPlantLoopHeatPumps::EIRFuelFiredHeatPump> heatPumps;
    bool getInputsFFHP = true;
    void clear_state() override
    {
        new (this) EIRFuelFiredHeatPumpsData();
    }
};

} // namespace EnergyPlus

#endif // ENERGYPLUS_PLANTLOOPHEATPUMPEIR_HH
