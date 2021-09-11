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

#ifndef ChillerElectricEIR_hh_INCLUDED
#define ChillerElectricEIR_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/EnergyPlus.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/PlantComponent.hh>

namespace EnergyPlus {

// Forward declarations
struct EnergyPlusData;

namespace ChillerElectricEIR {

    struct ElectricEIRChillerSpecs : PlantComponent
    {
        // Members
        std::string Name;                       // User identifier
        int TypeNum;                            // plant loop type identifier
        DataPlant::CondenserType CondenserType; // Type of Condenser - Air Cooled, Water Cooled or Evap Cooled
        Real64 RefCap;                          // Reference capacity of chiller [W]
        bool RefCapWasAutoSized;                // reference capacity was autosized on input
        Real64 RefCOP;                          // Reference coefficient of performance [W/W]
        DataPlant::FlowMode FlowMode;           // one of 3 modes for component flow during operation
        bool ModulatedFlowSetToLoop;            // True if the setpoint is missing at the outlet node
        bool ModulatedFlowErrDone;              // true if setpoint warning issued
        bool HRSPErrDone;                       // TRUE if set point warning issued for heat recovery loop
        Real64 EvapVolFlowRate;                 // Reference water volumetric flow rate through the evaporator [m3/s]
        bool EvapVolFlowRateWasAutoSized;       // true if previous was autosize input
        Real64 EvapMassFlowRate;
        Real64 EvapMassFlowRateMax;       // Reference water mass flow rate through evaporator [kg/s]
        Real64 CondVolFlowRate;           // Reference water volumetric flow rate through the condenser [m3/s]
        bool CondVolFlowRateWasAutoSized; // true if previous was set to autosize on input
        Real64 CondMassFlowRate;          // Condenser mass flow rate [kg/s]
        Real64 CondMassFlowRateMax;       // Reference water mass flow rate through condenser [kg/s]
        Real64 CondenserFanPowerRatio;    // Reference power of condenser fan to capacity ratio, W/W
        Real64 CompPowerToCondenserFrac;  // Fraction of compressor electric power rejected by condenser [0 to 1]
        int EvapInletNodeNum;             // Node number on the inlet side of the plant (evaporator side)
        int EvapOutletNodeNum;            // Node number on the outlet side of the plant (evaporator side)
        Real64 EvapOutletTemp;            // Evaporator outlet temperature [C]
        int CondInletNodeNum;             // Node number on the inlet side of the condenser
        int CondOutletNodeNum;            // Node number on the outlet side of the condenser
        Real64 CondOutletTemp;            // Condenser outlet temperature [C]
        Real64 CondOutletHumRat;          // Condenser outlet humidity ratio [kg/kg]
        Real64 MinPartLoadRat;            // Minimum allowed operating fraction of full load
        Real64 MaxPartLoadRat;            // Maximum allowed operating fraction of full load
        Real64 OptPartLoadRat;            // Optimal operating fraction of full load
        Real64 MinUnloadRat;              // Minimum unloading ratio
        Real64 TempRefCondIn;             // The reference secondary loop fluid temperature
        // at the chiller condenser side inlet [C]
        Real64 TempRefEvapOut; // The reference primary loop fluid temperature
        // at the chiller evaporator side outlet [C]
        Real64 TempLowLimitEvapOut;                // Low temperature shut off [C]
        Real64 DesignHeatRecVolFlowRate;           // Design water volumetric flow rate through heat recovery loop [m3/s]
        bool DesignHeatRecVolFlowRateWasAutoSized; // true if previous input was autosize
        Real64 DesignHeatRecMassFlowRate;          // Design water mass flow rate through heat recovery loop [kg/s]
        Real64 SizFac;                             // sizing factor
        Real64 BasinHeaterPowerFTempDiff;          // Basin heater capacity per degree C below setpoint (W/C)
        Real64 BasinHeaterSetPointTemp;            // setpoint temperature for basin heater operation (C)
        bool HeatRecActive;                        // True when entered Heat Rec Vol Flow Rate > 0
        int HeatRecInletNodeNum;                   // Node number for the heat recovery inlet side of the condenser
        int HeatRecOutletNodeNum;                  // Node number for the heat recovery outlet side of the condenser
        Real64 HeatRecCapacityFraction;            // user input for heat recovery capacity fraction []
        Real64 HeatRecMaxCapacityLimit;            // Capacity limit for Heat recovery, one time calc [W]
        int HeatRecSetPointNodeNum;                // index for system node with the heat recover leaving setpoint
        int HeatRecInletLimitSchedNum;             // index for schedule for the inlet high limit for heat recovery operation
        int ChillerCapFTIndex;                     // Index for the total cooling capacity modifier curve
        // (function of leaving chilled water temperature and
        //  entering condenser fluid temperature)
        int ChillerEIRFTIndex; // Index for the energy input ratio modifier curve
        // (function of leaving chilled water temperature and
        //  entering condenser fluid temperature)
        int ChillerEIRFPLRIndex;      // Index for the EIR vs part-load ratio curve
        int ChillerCapFTError;        // Used for negative capacity as a function of temp warnings
        int ChillerCapFTErrorIndex;   // Used for negative capacity as a function of temp warnings
        int ChillerEIRFTError;        // Used for negative EIR as a function of temp warnings
        int ChillerEIRFTErrorIndex;   // Used for negative EIR as a function of temp warnings
        int ChillerEIRFPLRError;      // Used for negative EIR as a function of PLR warnings
        int ChillerEIRFPLRErrorIndex; // Used for negative EIR as a function of PLR warnings
        Real64 ChillerEIRFPLRMin;     // Minimum value of PLR from EIRFPLR curve
        Real64 ChillerEIRFPLRMax;     // Maximum value of PLR from EIRFPLR curve
        int DeltaTErrCount;           // Evaporator delta T equals 0 for variable flow chiller warning messages
        int DeltaTErrCountIndex;      // Index to evaporator delta T = 0 for variable flow chiller warning messages
        int CWLoopNum;                // chilled water plant loop index number
        int CWLoopSideNum;            // chilled water plant loop side index
        int CWBranchNum;              // chilled water plant loop branch index
        int CWCompNum;                // chilled water plant loop component index
        int CDLoopNum;                // condenser water plant loop index number
        int CDLoopSideNum;            // condenser water plant loop side index
        int CDBranchNum;              // condenser water plant loop branch index
        int CDCompNum;                // condenser water plant loop component index
        int HRLoopNum;                // heat recovery water plant loop index
        int HRLoopSideNum;            // heat recovery water plant loop side index
        int HRBranchNum;              // heat recovery water plant loop branch index
        int HRCompNum;                // heat recovery water plant loop component index
        int BasinHeaterSchedulePtr;   // Pointer to basin heater schedule
        int CondMassFlowIndex;
        std::string MsgBuffer1;  // - buffer to print warning messages on following time step
        std::string MsgBuffer2;  // - buffer to print warning messages on following time step
        Real64 MsgDataLast;      // value of data when warning occurred (passed to Recurring Warn)
        bool PrintMessage;       // logical to determine if message is valid
        int MsgErrorCount;       // number of occurrences of warning
        int ErrCount1;           // for recurring error messages
        bool PossibleSubcooling; // flag to indicate chiller is doing less cooling that requested
        // Operational fault parameters
        bool FaultyChillerSWTFlag;         // True if the chiller has SWT sensor fault
        int FaultyChillerSWTIndex;         // Index of the fault object corresponding to the chiller
        Real64 FaultyChillerSWTOffset;     // Chiller SWT sensor offset
        bool FaultyChillerFoulingFlag;     // True if the chiller has fouling fault
        int FaultyChillerFoulingIndex;     // Index of the fault object corresponding to the chiller
        Real64 FaultyChillerFoulingFactor; // Chiller fouling factor
        std::string EndUseSubcategory;     // identifier use for the end use subcategory
        Real64 TimeStepSysLast;
        Real64 CurrentEndTimeLast;
        bool oneTimeFlag;
        bool MyEnvrnFlag;
        Real64 EvapWaterConsump;              // Evap cooler water consumption (m3)
        Real64 EvapWaterConsumpRate;          // Evap condenser water consumption rate [m3/s]
        Real64 Power;                         // Rate of chiller electric energy use [W]
        Real64 QEvaporator;                   // Rate of heat transfer to the evaporator coil [W]
        Real64 QCondenser;                    // Rate of heat transfer to the condenser coil [W]
        Real64 QHeatRecovered;                // Rate of heat transfer to the heat recovery coil [W]
        Real64 HeatRecOutletTemp;             // Heat recovery outlet temperature [C]
        Real64 CondenserFanPower;             // Condenser Fan Power (fan cycles with compressor) [W]
        Real64 ChillerCapFT;                  // Chiller capacity fraction (evaluated as a function of temperature)
        Real64 ChillerEIRFT;                  // Chiller electric input ratio (EIR = 1 / COP) as a function of temperature
        Real64 ChillerEIRFPLR;                // Chiller EIR as a function of part-load ratio (PLR)
        Real64 ChillerPartLoadRatio;          // Chiller part-load ratio (PLR)
        Real64 ChillerCyclingRatio;           // Chiller cycling ratio
        Real64 BasinHeaterPower;              // Basin heater power (W)
        Real64 ChillerFalseLoadRate;          // Chiller false load over and above the water-side load [W]
        Real64 ChillerFalseLoad;              // reporting: Chiller false load over and above water side load [W]
        Real64 Energy;                        // reporting: Chiller electric consumption [J]
        Real64 EvapEnergy;                    // reporting: Evaporator heat transfer energy [J]
        Real64 CondEnergy;                    // reporting: Condenser heat transfer energy [J]
        Real64 CondInletTemp;                 // reporting: Condenser inlet temperature [C]
        Real64 EvapInletTemp;                 // reporting: Evaporator inlet temperature [C]
        Real64 ActualCOP;                     // reporting: Coefficient of performance
        Real64 EnergyHeatRecovery;            // reporting: Energy recovered from water-cooled condenser [J]
        Real64 HeatRecInletTemp;              // reporting: Heat reclaim inlet temperature [C]
        Real64 HeatRecMassFlow;               // reporting: Heat reclaim mass flow rate [kg/s]
        Real64 ChillerCondAvgTemp;            // reporting: average condenser temp for curves with Heat recovery [C]
        Real64 CondenserFanEnergyConsumption; // reporting: Air-cooled condenser fan energy [J]
        Real64 BasinHeaterConsumption;        // Basin heater energy consumption (J)
        bool IPLVFlag;
        DataBranchAirLoopPlant::ControlTypeEnum EquipFlowCtrl;

        // Default Constructor
        ElectricEIRChillerSpecs()
            : TypeNum(0), CondenserType(DataPlant::CondenserType::Unassigned), RefCap(0.0), RefCapWasAutoSized(false), RefCOP(0.0),
              FlowMode(DataPlant::FlowMode::Unassigned), ModulatedFlowSetToLoop(false), ModulatedFlowErrDone(false), HRSPErrDone(false),
              EvapVolFlowRate(0.0), EvapVolFlowRateWasAutoSized(false), EvapMassFlowRate(0.0), EvapMassFlowRateMax(0.0), CondVolFlowRate(0.0),
              CondVolFlowRateWasAutoSized(false), CondMassFlowRate(0.0), CondMassFlowRateMax(0.0), CondenserFanPowerRatio(0.0),
              CompPowerToCondenserFrac(0.0), EvapInletNodeNum(0), EvapOutletNodeNum(0), EvapOutletTemp(0.0), CondInletNodeNum(0),
              CondOutletNodeNum(0), CondOutletTemp(0.0), CondOutletHumRat(0.0), MinPartLoadRat(0.0), MaxPartLoadRat(0.0), OptPartLoadRat(0.0),
              MinUnloadRat(0.0), TempRefCondIn(0.0), TempRefEvapOut(0.0), TempLowLimitEvapOut(0.0), DesignHeatRecVolFlowRate(0.0),
              DesignHeatRecVolFlowRateWasAutoSized(false), DesignHeatRecMassFlowRate(0.0), SizFac(0.0), BasinHeaterPowerFTempDiff(0.0),
              BasinHeaterSetPointTemp(0.0), HeatRecActive(false), HeatRecInletNodeNum(0), HeatRecOutletNodeNum(0), HeatRecCapacityFraction(0.0),
              HeatRecMaxCapacityLimit(0.0), HeatRecSetPointNodeNum(0), HeatRecInletLimitSchedNum(0), ChillerCapFTIndex(0), ChillerEIRFTIndex(0),
              ChillerEIRFPLRIndex(0), ChillerCapFTError(0), ChillerCapFTErrorIndex(0), ChillerEIRFTError(0), ChillerEIRFTErrorIndex(0),
              ChillerEIRFPLRError(0), ChillerEIRFPLRErrorIndex(0), ChillerEIRFPLRMin(0.0), ChillerEIRFPLRMax(0.0), DeltaTErrCount(0),
              DeltaTErrCountIndex(0), CWLoopNum(0), CWLoopSideNum(0), CWBranchNum(0), CWCompNum(0), CDLoopNum(0), CDLoopSideNum(0), CDBranchNum(0),
              CDCompNum(0), HRLoopNum(0), HRLoopSideNum(0), HRBranchNum(0), HRCompNum(0), BasinHeaterSchedulePtr(0), CondMassFlowIndex(0),
              MsgDataLast(0.0), PrintMessage(false), MsgErrorCount(0), ErrCount1(0), PossibleSubcooling(false), FaultyChillerSWTFlag(false),
              FaultyChillerSWTIndex(0), FaultyChillerSWTOffset(0.0), FaultyChillerFoulingFlag(false), FaultyChillerFoulingIndex(0),
              FaultyChillerFoulingFactor(1.0), TimeStepSysLast(0.0), CurrentEndTimeLast(0.0), oneTimeFlag(true), MyEnvrnFlag(true),
              EvapWaterConsump(0.0), EvapWaterConsumpRate(0.0), Power(0.0), QEvaporator(0.0), QCondenser(0.0), QHeatRecovered(0.0),
              HeatRecOutletTemp(0.0), CondenserFanPower(0.0), ChillerCapFT(0.0), ChillerEIRFT(0.0), ChillerEIRFPLR(0.0), ChillerPartLoadRatio(0.0),
              ChillerCyclingRatio(0.0), BasinHeaterPower(0.0), ChillerFalseLoadRate(0.0), ChillerFalseLoad(0.0), Energy(0.0), EvapEnergy(0.0),
              CondEnergy(0.0), CondInletTemp(0.0), EvapInletTemp(0.0), ActualCOP(0.0), EnergyHeatRecovery(0.0), HeatRecInletTemp(0.0),
              HeatRecMassFlow(0.0), ChillerCondAvgTemp(0.0), CondenserFanEnergyConsumption(0.0), BasinHeaterConsumption(0.0), IPLVFlag(true),
              EquipFlowCtrl(DataBranchAirLoopPlant::ControlTypeEnum::Unknown)
        {
        }

        static PlantComponent *factory(EnergyPlusData &state, std::string const &objectName);

        void setupOutputVars(EnergyPlusData &state);

        void simulate([[maybe_unused]] EnergyPlusData &state,
                      const PlantLocation &calledFromLocation,
                      bool FirstHVACIteration,
                      Real64 &CurLoad,
                      bool RunFlag) override;

        void getDesignCapacities(
            EnergyPlusData &state, const PlantLocation &calledFromLocation, Real64 &MaxLoad, Real64 &MinLoad, Real64 &OptLoad) override;

        void getDesignTemperatures(Real64 &TempDesCondIn, Real64 &TempDesEvapOut) override;

        void getSizingFactor(Real64 &sizFac) override;

        void onInitLoopEquip(EnergyPlusData &state, const PlantLocation &calledFromLocation) override;

        void oneTimeInit(EnergyPlusData &state) override;

        void initEachEnvironment(EnergyPlusData &state);

        void initialize(EnergyPlusData &state, bool RunFlag, Real64 MyLoad);

        void size(EnergyPlusData &state);

        void calculate(EnergyPlusData &state, Real64 &MyLoad, bool RunFlag);

        void calcHeatRecovery(EnergyPlusData &state,
                              Real64 &QCond,        // Current condenser load [W]
                              Real64 CondMassFlow,  // Current condenser mass flow [kg/s]
                              Real64 condInletTemp, // Current condenser inlet temp [C]
                              Real64 &QHeatRec      // Amount of heat recovered [W]
        );

        void update(EnergyPlusData &state, Real64 MyLoad, bool RunFlag);
    };

    void GetElectricEIRChillerInput(EnergyPlusData &state);

} // namespace ChillerElectricEIR

struct ChillerElectricEIRData : BaseGlobalStruct
{
    int NumElectricEIRChillers = 0;
    bool getInputFlag = true;
    Array1D<ChillerElectricEIR::ElectricEIRChillerSpecs> ElectricEIRChiller;

    void clear_state() override
    {
        this->NumElectricEIRChillers = 0;
        this->getInputFlag = true;
        this->ElectricEIRChiller.deallocate();
    }
};

} // namespace EnergyPlus

#endif
