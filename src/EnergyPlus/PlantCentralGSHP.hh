// EnergyPlus, Copyright (c) 1996-present, The Board of Trustees of the University of Illinois,
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy), Oak Ridge
// National Laboratory, managed by UT-Battelle, Alliance for Energy Innovation, LLC, and other
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

#ifndef PlantCentralGSHP_hh_INCLUDED
#define PlantCentralGSHP_hh_INCLUDED

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

namespace PlantCentralGSHP {

    enum class CurrentMode
    {
        Invalid = -1,
        Off,
        CoolingOnly,
        HeatingOnly,
        HeatRecovery,
        CoolingDominant,
        HeatingDominant,
        Num
    };

    enum class CondenserModeTemperature
    {
        Invalid = -1,
        EnteringCondenser,
        LeavingCondenser,
        Num
    };

    struct WrapperComponentSpecs
    {
        std::string WrapperPerformanceObjectType; // Component type
        std::string WrapperComponentName;         // Component name
        int WrapperPerformanceObjectIndex = 0;    // Component index in the input array
        int WrapperIdenticalObjectNum = 0;        // Number of identical objects
        Sched::Schedule *chSched = nullptr;       // schedule
    };

    struct ChillerHeaterResult
    {
        CurrentMode currentMode = CurrentMode::Off;
        bool isAvailable = false;
        bool isRunning = false;

        Real64 requestedCoolingLoad = 0.0;        // Cooling load presented to this module [W]
        Real64 requestedHeatingLoad = 0.0;        // Heating load presented to this module [W]
        Real64 availableEvaporatorCapacity = 0.0; // Available evaporator capacity at the operating temperatures [W]
        Real64 availableCondenserCapacity = 0.0;  // Available condenser capacity at the operating temperatures [W]
        Real64 qEvaporator = 0.0;                 // Refrigerant evaporator heat transfer [W]
        Real64 qCondenser = 0.0;                  // Refrigerant condenser heat transfer [W]

        Real64 compressorPower = 0.0;        // Compressor electric power counted once [W]
        Real64 coolingPower = 0.0;           // Compressor power assigned to cooling reporting [W]
        Real64 heatingPower = 0.0;           // Compressor power assigned to heating reporting [W]
        Real64 motorHeatToRefrigerant = 0.0; // Compressor power delivered to the refrigerant [W]
        Real64 motorHeatLoss = 0.0;          // Compressor power rejected outside the refrigerant cycle [W]
        Real64 falseLoadRate = 0.0;          // False loading above useful water-side load [W]

        Real64 partLoadRatio = 0.0;
        Real64 cyclingRatio = 0.0;
        Real64 unloadingRatio = 0.0;
        Real64 capacityTemperatureModifier = 0.0;
        Real64 eirTemperatureModifier = 0.0;
        Real64 eirPartLoadModifier = 0.0;
        Real64 capacityCurveEvaporatorTemp = 0.0;   // Evaporator temperature used by the capacity curve [C]
        Real64 capacityCurveCondenserTemp = 0.0;    // Condenser temperature used by the capacity curve [C]
        Real64 eirCurveEvaporatorTemp = 0.0;        // Evaporator temperature used by the EIR temperature curve [C]
        Real64 eirCurveCondenserTemp = 0.0;         // Condenser temperature used by the EIR temperature curve [C]
        Real64 eirPartLoadCurvePLR = 0.0;           // PLR used to evaluate the EIR part-load curve
        Real64 eirPartLoadCurveCondenserTemp = 0.0; // Condenser temperature used by a bivariate EIR part-load curve [C]
        Real64 actualCOP = 0.0;

        Real64 evaporatorInletTemp = 0.0;
        Real64 evaporatorOutletTemp = 0.0;
        Real64 evaporatorMassFlowRate = 0.0;
        Real64 condenserInletTemp = 0.0;
        Real64 condenserOutletTemp = 0.0;
        Real64 condenserMassFlowRate = 0.0;

        Real64 chilledWaterInletTemp = 0.0;
        Real64 chilledWaterOutletTemp = 0.0;
        Real64 chilledWaterMassFlowRate = 0.0;
        Real64 hotWaterInletTemp = 0.0;
        Real64 hotWaterOutletTemp = 0.0;
        Real64 hotWaterMassFlowRate = 0.0;
        Real64 sourceInletTemp = 0.0;
        Real64 sourceOutletTemp = 0.0;
        Real64 sourceMassFlowRate = 0.0;

        Real64 coolingDelivered = 0.0;   // Useful chilled-water cooling [W]
        Real64 heatingDelivered = 0.0;   // Useful hot-water heating [W]
        Real64 heatRecovered = 0.0;      // Condenser heat routed to heat recovery [W]
        Real64 sourceHeatTransfer = 0.0; // Positive rejects to source; negative extracts from source [W]
        Real64 unmetCoolingLoad = 0.0;   // Remaining wrapper cooling load after this module [W]
        Real64 unmetHeatingLoad = 0.0;   // Remaining wrapper heating load after this module [W]

        Real64 falseLoadEnergy = 0.0;
        Real64 coolingEnergy = 0.0;
        Real64 heatingEnergy = 0.0;
        Real64 evaporatorEnergy = 0.0;
        Real64 condenserEnergy = 0.0;

        void updateEnergies(Real64 secondsInTimeStep)
        {
            this->falseLoadEnergy = this->falseLoadRate * secondsInTimeStep;
            this->coolingEnergy = this->coolingPower * secondsInTimeStep;
            this->heatingEnergy = this->heatingPower * secondsInTimeStep;
            this->evaporatorEnergy = this->qEvaporator * secondsInTimeStep;
            this->condenserEnergy = this->qCondenser * secondsInTimeStep;
        }

        void updatePowerAccounting(Real64 openMotorEfficiency)
        {
            this->compressorPower = this->coolingPower + this->heatingPower;
            this->motorHeatToRefrigerant = this->compressorPower * openMotorEfficiency;
            this->motorHeatLoss = this->compressorPower - this->motorHeatToRefrigerant;
            this->isRunning = this->currentMode == CurrentMode::CoolingOnly || this->currentMode == CurrentMode::HeatingOnly ||
                              this->currentMode == CurrentMode::HeatRecovery || this->currentMode == CurrentMode::CoolingDominant ||
                              this->currentMode == CurrentMode::HeatingDominant;
        }

        Real64 moduleEnergyBalanceResidual() const
        {
            return this->qCondenser - this->qEvaporator - this->motorHeatToRefrigerant - this->falseLoadRate;
        }

        Real64 routingEnergyBalanceResidual() const
        {
            return this->heatingDelivered + this->sourceHeatTransfer - this->coolingDelivered - this->motorHeatToRefrigerant - this->falseLoadRate;
        }
    };

    struct ChillerHeaterPerformanceData
    {
        std::string Name;
        CondenserModeTemperature CondModeCooling = CondenserModeTemperature::Invalid;
        CondenserModeTemperature CondModeHeating = CondenserModeTemperature::Invalid;
        bool ConstantFlow = false;
        bool VariableFlow = false;
        int ChillerCapFTCoolingIDX = 0;
        int ChillerEIRFTCoolingIDX = 0;
        int ChillerEIRFPLRCoolingIDX = 0;
        int ChillerCapFTHeatingIDX = 0;
        int ChillerEIRFTHeatingIDX = 0;
        int ChillerEIRFPLRHeatingIDX = 0;
        Real64 RefCapCooling = 0.0;
        bool RefCapCoolingWasAutoSized = false;
        Real64 RefCOPCooling = 0.0;
        Real64 TempRefEvapOutCooling = 0.0;
        Real64 TempRefCondInCooling = 0.0;
        Real64 TempRefCondOutCooling = 0.0;
        Real64 MaxPartLoadRatCooling = 0.0;
        Real64 OptPartLoadRatCooling = 0.0;
        Real64 MinPartLoadRatCooling = 0.0;
        Real64 ClgHtgToCoolingCapRatio = 0.0;
        Real64 ClgHtgtoCogPowerRatio = 0.0;
        Real64 RefCapClgHtg = 0.0;
        Real64 RefCOPClgHtg = 0.0;
        Real64 RefPowerClgHtg = 0.0;
        Real64 TempRefEvapOutClgHtg = 0.0;
        Real64 TempRefCondInClgHtg = 0.0;
        Real64 TempRefCondOutClgHtg = 0.0;
        Real64 TempLowLimitEvapOut = 0.0;
        Real64 MaxPartLoadRatClgHtg = 0.0;
        Real64 OptPartLoadRatClgHtg = 0.0;
        Real64 MinPartLoadRatClgHtg = 0.0;
        Real64 EvapVolFlowRate = 0.0;
        bool EvapVolFlowRateWasAutoSized = false;
        Real64 CondVolFlowRate = 0.0;
        bool CondVolFlowRateWasAutoSized = false;
        Real64 DesignHotWaterVolFlowRate = 0.0;
        Real64 OpenMotorEff = 0.0;
        Real64 SizFac = 1.0;
        Real64 MaxHeatingLeavingCondTemp = 0.0;
        bool MaxHeatingLeavingCondTempWasBlank = true;
    };

    struct ChillerHeaterSizingData
    {
        Real64 RefCapCooling = 0.0;
        Real64 RefCapClgHtg = 0.0;
        Real64 RefCOPClgHtg = 0.0;
        Real64 RefPowerClgHtg = 0.0;
        Real64 EvapVolFlowRate = 0.0;
        Real64 tmpEvapVolFlowRate = 0.0;
        Real64 CondVolFlowRate = 0.0;
        Real64 tmpCondVolFlowRate = 0.0;
        Real64 CondMassFlowRateMax = 0.0;
        Real64 EvapMassFlowRateMax = 0.0;
        Real64 ChilledWaterMassFlowRateMax = 0.0;
        Real64 HotWaterMassFlowRateMax = 0.0;
        Real64 SourceEvapMassFlowRateMax = 0.0;
        Real64 SourceCondMassFlowRateMax = 0.0;
    };

    struct ModePerformanceData
    {
        CondenserModeTemperature condenserMode = CondenserModeTemperature::Invalid;
        int capacityTemperatureCurveIndex = 0;
        int eirTemperatureCurveIndex = 0;
        int eirPartLoadCurveIndex = 0;
        Real64 referenceEvaporatorCapacity = 0.0;
        Real64 referenceCOP = 0.0;
        Real64 referenceEvaporatorLeavingTemp = 0.0;
        Real64 referenceCondenserEnteringTemp = 0.0;
        Real64 referenceCondenserLeavingTemp = 0.0;
        Real64 minimumPartLoadRatio = 0.0;
        Real64 maximumPartLoadRatio = 0.0;
        Real64 optimumPartLoadRatio = 0.0;
    };

    struct ChillerHeaterModule
    {
        int performanceIndex = 0;
        ChillerHeaterPerformanceData const *performance = nullptr;
        Sched::Schedule *availabilitySchedule = nullptr;
        bool VariableFlow = false;
        ChillerHeaterSizingData sizing;
        Real64 minimumEvaporatorOutletTemp = 0.0;
        int capacityCurveErrorCount = 0;
        int capacityCurveErrorIndex = 0;
        ChillerHeaterResult Result;

        void initialize(int performanceIndex, ChillerHeaterPerformanceData const &performance, Sched::Schedule *availabilitySchedule);
        ChillerHeaterPerformanceData const &performanceData() const;
        std::string const &name() const;
        bool isAvailable() const;
        ModePerformanceData coolingModePerformance() const;
        ModePerformanceData heatingModePerformance() const;
        void mapResultToPlantConnections();
        void updateResultEnergies(Real64 secondsInTimeStep);
        void resetResult(Real64 evaporatorInletTemp, Real64 condenserInletTemp);
    };

    struct WrapperReportVars
    {
        Real64 Power = 0.0;             // Wrapper power, W
        Real64 QCHW = 0.0;              // Chilled water heat transfer rate [W]
        Real64 QHW = 0.0;               // Hot Water heat transfer rate [W]
        Real64 QGLHE = 0.0;             // Geo-field heat transfer rate [W]
        Real64 TotElecCooling = 0.0;    // Wrapper cooling electric consumption [J]
        Real64 TotElecHeating = 0.0;    // Wrapper heating electric consumption [J]
        Real64 CoolingEnergy = 0.0;     // Chilled water heat transfer energy [J]
        Real64 HeatingEnergy = 0.0;     // Hot Water heat transfer energy [J]
        Real64 GLHEEnergy = 0.0;        // Geo-field heat transfer energy [J]
        Real64 TotElecCoolingPwr = 0.0; // Wrapper cooling electric consumption rate [W]
        Real64 TotElecHeatingPwr = 0.0; // Wrapper heating electric consumption rate [W]
        Real64 CoolingRate = 0.0;       // Chilled water heat transfer rate [W]
        Real64 HeatingRate = 0.0;       // Hot Water heat transfer rate [W]
        Real64 GLHERate = 0.0;          // Geo-field heat transfer rate [W]
        Real64 CHWInletTemp = 0.0;      // Chilled water inlet temperature [C]
        Real64 HWInletTemp = 0.0;       // Hot water inlet temperature [C]
        Real64 GLHEInletTemp = 0.0;     // Geo-field inlet temperature [C]
        Real64 CHWOutletTemp = 0.0;     // Chilled water Outlet temperature [C]
        Real64 HWOutletTemp = 0.0;      // Hot water Outlet temperature [C]
        Real64 GLHEOutletTemp = 0.0;    // Geo-field Outlet temperature [C]
        Real64 CHWmdot = 0.0;           // Chilled water mass flow rate [kg/s]
        Real64 HWmdot = 0.0;            // Hot water mass flow rate [kg/s]
        Real64 GLHEmdot = 0.0;          // Geo-field mass flow rate [kg/s]
    };

    struct WrapperSpecs : PlantComponent
    {
        std::string Name;                               // User identifier
        bool VariableFlowCH = false;                    // True if all chiller heaters are variable flow control
        Sched::Schedule *ancillaryPowerSched = nullptr; // Schedule value for ancillary power control
        Sched::Schedule *chSched = nullptr;             // Schedule value for individual chiller heater control
        int CHWInletNodeNum = 0;                        // Node number on the inlet side of the plant (Chilled Water side)
        int CHWOutletNodeNum = 0;                       // Node number on the outlet side of the plant (Chilled Water side)
        int HWInletNodeNum = 0;                         // Node number on the inlet side of the plant (Hot Water side)
        int HWOutletNodeNum = 0;                        // Node number on the outlet side of the plant (Hot Water side)
        int GLHEInletNodeNum = 0;                       // Node number on the inlet side of the plant (GLHE Water side)
        int GLHEOutletNodeNum = 0;                      // Node number on the outlet side of the plant (GLHE Water side)
        int CoolSetPointTempNode = 0;                   // Node number of the cooling setpoint temperature node
        int HeatSetPointTempNode = 0;                   // Node number of the heating setpoint temperature node
        bool EvapOutletMinTempCheck = true;             // True if minimum evaporator outlet temperature warning is enabled
        int NumOfComp = 0;                              // Number of Components under the wrapper
        Real64 CHWMassFlowRate = 0.0;                   // Chilled water mass flow rate
        Real64 HWMassFlowRate = 0.0;                    // Hot water mass flow rate
        Real64 GLHEMassFlowRate = 0.0;                  // Condenser water mass flow rate
        Real64 CHWMassFlowRateMax = 0.0;                // Maximum chilled water mass flow rate
        Real64 HWMassFlowRateMax = 0.0;                 // Maximum hot water mass flow rate
        Real64 GLHEMassFlowRateMax = 0.0;               // Maximum condenser water mass flow rate
        Real64 WrapperCoolingLoad = 0.0;                // Cooling demand for the central heat pump system
        Real64 WrapperHeatingLoad = 0.0;                // Heating demand for the central heat pump system
        Real64 AncillaryPower = 0.0;                    // Wrapper Ancillary Power
        Array1D<WrapperComponentSpecs> WrapperComp;
        Array1D<ChillerHeaterModule> ChillerHeater; // Dimension to number of machines
        bool CoolSetPointErrDone = false;           // true if setpoint warning issued
        bool HeatSetPointErrDone = false;           // true if setpoint warning issued
        int ChillerHeaterNums = 0;                  // Total number of chiller heater units
        PlantLocation CWPlantLoc = {};              // Chilled water plant loop component index
        PlantLocation HWPlantLoc = {};              // Hot water plant loop component index
        PlantLocation GLHEPlantLoc = {};            // Geo-field water plant loop component index
        int CHWMassFlowIndex = 0;                   // Chilled water flow index
        int HWMassFlowIndex = 0;                    // Hot water flow index
        int GLHEMassFlowIndex = 0;                  // Condenser side flow index
        Real64 SizingFactor = 1.0;                  // Sizing factor to adjust the capacity
        Real64 CHWVolFlowRate = 0.0;                // Chilled water volume flow rate [kg/s]
        Real64 HWVolFlowRate = 0.0;                 // Hot water volume flow rate [kg/s]
        Real64 GLHEVolFlowRate = 0.0;               // Geo-field volume flow rate [kg/s]
        bool MyWrapperFlag = true;
        bool MyWrapperEnvrnFlag = true;
        bool SimulClgDominant = false;
        bool SimulHtgDominant = false;
        WrapperReportVars Report;
        bool setupOutputVarsFlag = true;
        bool mySizesReported = false;

        static PlantComponent *factory(EnergyPlusData &state, std::string const &objectName);

        void getSizingFactor(Real64 &SizFac) override;

        void getDesignCapacities(
            EnergyPlusData &state, const PlantLocation &calledFromLocation, Real64 &MaxLoad, Real64 &MinLoad, Real64 &OptLoad) override;

        void setupOutputVars(EnergyPlusData &state);

        void initialize(EnergyPlusData &state,
                        Real64 MyLoad, // Demand Load
                        int LoopNum,   // Loop Number Index
                        bool RunFlag = true);

        void initializeDesignFlowLimits(EnergyPlusData &state);

        void resolveFlowMode(EnergyPlusData &state);

        void resetOffState(EnergyPlusData &state, bool releasePlantFlows = true);

        void simulate([[maybe_unused]] EnergyPlusData &state,
                      const PlantLocation &calledFromLocation,
                      bool FirstHVACIteration,
                      Real64 &CurLoad,
                      bool RunFlag) override;

        void SizeWrapper(EnergyPlusData &state);

        void CalcWrapperModel(EnergyPlusData &state, Real64 &MyLoad, int LoopNum);

        void CalcCoolingOnlyModel(
            EnergyPlusData &state, Real64 chilledWaterMassFlowRate, Real64 sourceMassFlowRate, Real64 chilledWaterInletTemp, Real64 sourceInletTemp);

        void CalcHeatingOnlyModel(
            EnergyPlusData &state, Real64 hotWaterMassFlowRate, Real64 sourceMassFlowRate, Real64 hotWaterInletTemp, Real64 sourceInletTemp);

        ChillerHeaterResult solveCoolingOnly(EnergyPlusData &state,
                                             int chillerHeaterNum,
                                             Real64 requestedCoolingLoad,
                                             Real64 evaporatorMassFlowRateMax,
                                             Real64 condenserMassFlowRate,
                                             Real64 evaporatorInletTemp,
                                             Real64 condenserInletTemp);

        ChillerHeaterResult solveHeatingOnly(EnergyPlusData &state,
                                             int chillerHeaterNum,
                                             Real64 requestedHeatingLoad,
                                             Real64 evaporatorMassFlowRate,
                                             Real64 condenserMassFlowRateMax,
                                             Real64 evaporatorInletTemp,
                                             Real64 condenserInletTemp);

        ChillerHeaterResult solveSimultaneous(EnergyPlusData &state,
                                              int chillerHeaterNum,
                                              Real64 requestedCoolingLoad,
                                              Real64 requestedHeatingLoad,
                                              Real64 chilledWaterMassFlowRateMax,
                                              Real64 hotWaterMassFlowRateMax,
                                              Real64 sourceMassFlowRateMax,
                                              Real64 chilledWaterInletTemp,
                                              Real64 hotWaterInletTemp,
                                              Real64 sourceInletTemp);

        void CalcSimultaneousModel(EnergyPlusData &state,
                                   Real64 chilledWaterMassFlowRate,
                                   Real64 hotWaterMassFlowRate,
                                   Real64 sourceMassFlowRate,
                                   Real64 chilledWaterInletTemp,
                                   Real64 hotWaterInletTemp,
                                   Real64 sourceInletTemp);

        void updateWrapperReportingAndNodes(EnergyPlusData &state,
                                            Real64 chilledWaterMassFlowRate,
                                            Real64 hotWaterMassFlowRate,
                                            Real64 sourceMassFlowRate,
                                            Real64 chilledWaterInletTemp,
                                            Real64 hotWaterInletTemp,
                                            Real64 sourceInletTemp);

        static Real64 setChillerHeaterCondTemp(ModePerformanceData const &modePerformance, Real64 condEnteringTemp, Real64 condLeavingTemp);

        Real64 calcChillerCapFT(
            EnergyPlusData &state, ChillerHeaterModule &module, ModePerformanceData const &modePerformance, Real64 evapOutletTemp, Real64 condTemp);

        void onInitLoopEquip([[maybe_unused]] EnergyPlusData &state, [[maybe_unused]] const PlantLocation &calledFromLocation) override;

        void oneTimeInit_new(EnergyPlusData &state) override;

        void oneTimeInit(EnergyPlusData &state) override;
    };

    void GetWrapperInput(EnergyPlusData &state);

    void GetChillerHeaterInput(EnergyPlusData &state);

} // namespace PlantCentralGSHP

struct PlantCentralGSHPData : BaseGlobalStruct
{

    bool getWrapperInputFlag = true;   // When TRUE, calls subroutine to read input file.
    int numWrappers = 0;               // Number of Wrappers specified in input
    int numPerformanceDefinitions = 0; // Number of performance definitions specified in input
    int numPerformanceReferences = 0;  // Number of wrapper performance references
    EPVector<PlantCentralGSHP::WrapperSpecs> Wrapper;
    EPVector<PlantCentralGSHP::ChillerHeaterPerformanceData> performanceDefinitions;

    void init_constant_state([[maybe_unused]] EnergyPlusData &state) override
    {
    }

    void init_state([[maybe_unused]] EnergyPlusData &state) override
    {
    }

    void clear_state() override
    {
        new (this) PlantCentralGSHPData();
    }
};

} // namespace EnergyPlus

#endif
