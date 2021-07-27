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

#ifndef ChillerReformulatedEIR_hh_INCLUDED
#define ChillerReformulatedEIR_hh_INCLUDED

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

namespace ChillerReformulatedEIR {

    enum class PLR
    {
        Unassigned,
        LeavingCondenserWaterTemperature, // Type 1_LeavingCondenserWaterTemperature
        Lift                              // Type 2_Lift
    };

    struct ReformulatedEIRChillerSpecs : PlantComponent
    {
        // Members
        std::string Name;                       // User identifier
        int TypeNum;                            // plant loop type identifier
        std::string CAPFTName;                  // CAPFT curve name
        std::string EIRFTName;                  // EIRFT curve name
        std::string EIRFPLRName;                // EIRPLR curve name
        DataPlant::CondenserType CondenserType; // Type of Condenser. Water Cooled is the only available option for now
        PLR PartLoadCurveType;                  // Part Load Ratio Curve Type: 1_LeavingCondenserWaterTemperature; 2_Lift
        Real64 RefCap;                          // Reference capacity of the chiller [W]
        bool RefCapWasAutoSized;                // reference capacity was autosized on input
        Real64 RefCOP;                          // Reference coefficient of performance [W/W]
        DataPlant::FlowMode FlowMode;           // one of 3 modes for component flow during operation
        bool ModulatedFlowSetToLoop;            // True if the setpoint is missing at the outlet node
        bool ModulatedFlowErrDone;              // true if setpoint warning issued
        Real64 EvapVolFlowRate;                 // Reference water volumetric flow rate through the evaporator [m3/s]
        bool EvapVolFlowRateWasAutoSized;       // true if previous was autosize input
        Real64 EvapMassFlowRateMax;             // Reference water mass flow rate through evaporator [kg/s]
        Real64 CondVolFlowRate;                 // Reference water volumetric flow rate through the condenser [m3/s]
        bool CondVolFlowRateWasAutoSized;       // true if previous was set to autosize on input
        Real64 CondMassFlowRateMax;             // Reference water mass flow rate through condenser [kg/s]
        Real64 CompPowerToCondenserFrac;        // Fraction of compressor electric power rejected by condenser [0 to 1]
        int EvapInletNodeNum;                   // Node number on the inlet side of the plant (evaporator side)
        int EvapOutletNodeNum;                  // Node number on the outlet side of the plant (evaporator side)
        int CondInletNodeNum;                   // Node number on the inlet side of the condenser
        int CondOutletNodeNum;                  // Node number on the outlet side of the condenser
        Real64 MinPartLoadRat;                  // Minimum allowed operating fraction of full load
        Real64 MaxPartLoadRat;                  // Maximum allowed operating fraction of full load
        Real64 OptPartLoadRat;                  // Optimal operating fraction of full load
        Real64 MinUnloadRat;                    // Minimum unloading ratio
        Real64 TempRefCondIn;                   // The reference secondary loop fluid temperature at the
        // chiller condenser side inlet for the reformulated chiller [C]
        Real64 TempRefCondOut; // The reference secondary loop fluid temperature at the
        // chiller condenser side outlet for the reformulated chiller [C]
        Real64 TempRefEvapOut; // The reference primary loop fluid
        // temperature at the chiller evaporator side outlet [C]
        Real64 TempLowLimitEvapOut;                // Low temperature shut off [C]
        Real64 DesignHeatRecVolFlowRate;           // Design water volumetric flow rate through heat recovery loop [m3/s]
        bool DesignHeatRecVolFlowRateWasAutoSized; // true if previous input was autosize
        Real64 DesignHeatRecMassFlowRate;          // Design water mass flow rate through heat recovery loop [kg/s]
        Real64 SizFac;                             // sizing factor
        bool HeatRecActive;                        // True when entered Heat Rec Vol Flow Rate > 0
        int HeatRecInletNodeNum;                   // Node number for the heat recovery inlet side of the condenser
        int HeatRecOutletNodeNum;                  // Node number for the heat recovery outlet side of the condenser
        Real64 HeatRecCapacityFraction;            // user input for heat recovery capacity fraction []
        Real64 HeatRecMaxCapacityLimit;            // Capacity limit for Heat recovery, one time calc [W]
        int HeatRecSetPointNodeNum;                // index for system node with the heat recover leaving setpoint
        int HeatRecInletLimitSchedNum;             // index for schedule for the inlet high limit for heat recovery operation
        int ChillerCapFTIndex;                     // Index for the total cooling capacity modifier curve
        // (function of leaving evaporator and condenser water temperatures)
        int ChillerEIRFTIndex; // Index for the energy input ratio modifier curve
        // (function of leaving evaporator and condenser water temperatures)
        int ChillerEIRFPLRIndex; // Index for the energy input ratio vs part-load ratio curve
        // (function of leaving condenser water temperature and part-load ratio)
        int ChillerCapFTError;         // Used for negative capacity as a function of temp warnings
        int ChillerCapFTErrorIndex;    // Used for negative capacity as a function of temp warnings
        int ChillerEIRFTError;         // Used for negative EIR as a function of temp warnings
        int ChillerEIRFTErrorIndex;    // Used for negative EIR as a function of temp warnings
        int ChillerEIRFPLRError;       // Used for negative EIR as a function of PLR warnings
        int ChillerEIRFPLRErrorIndex;  // Used for negative EIR as a function of PLR warnings
        Real64 ChillerCAPFTXTempMin;   // Minimum value of CAPFT curve X variable [C]
        Real64 ChillerCAPFTXTempMax;   // Maximum value of CAPFT curve X variable [C]
        Real64 ChillerCAPFTYTempMin;   // Minimum value of CAPFT curve Y variable [C]
        Real64 ChillerCAPFTYTempMax;   // Maximum value of CAPFT curve Y variable [C]
        Real64 ChillerEIRFTXTempMin;   // Minimum value of EIRFT curve X variable [C]
        Real64 ChillerEIRFTXTempMax;   // Maximum value of EIRFT curve X variable [C]
        Real64 ChillerEIRFTYTempMin;   // Minimum value of EIRFT curve Y variable [C]
        Real64 ChillerEIRFTYTempMax;   // Maximum value of EIRFT curve Y variable [C]
        Real64 ChillerEIRFPLRTempMin;  // Minimum value of EIRFPLR curve condenser outlet temperature [C]
        Real64 ChillerEIRFPLRTempMax;  // Maximum value of EIRFPLR curve condenser outlet temperature [C]
        Real64 ChillerEIRFPLRPLRMin;   // Minimum value of EIRFPLR curve part-load ratio
        Real64 ChillerEIRFPLRPLRMax;   // Maximum value of EIRFPLR curve part-load ratio
        Real64 ChillerLiftNomMin;      // Minimum value of EIRFPLR curve Normalized Chiller lift
        Real64 ChillerLiftNomMax;      // Maximum value of EIRFPLR curve Normalized Chiller lift
        Real64 ChillerTdevNomMin;      // Minimum value of EIRFPLR curve Normalized Tdev
        Real64 ChillerTdevNomMax;      // Maximum value of EIRFPLR curve Normalized Tdev
        int CAPFTXIter;                // Iteration counter for evaporator outlet temperature CAPFT warning messages
        int CAPFTXIterIndex;           // Index for evaporator outlet temperature CAPFT warning messages
        int CAPFTYIter;                // Iteration counter for condenser outlet temperature CAPFT warning messages
        int CAPFTYIterIndex;           // Index for condenser outlet temperature CAPFT warning messages
        int EIRFTXIter;                // Iteration counter for evaporator outlet temperature EIRFT warning messages
        int EIRFTXIterIndex;           // Index for evaporator outlet temperature EIRFT warning messages
        int EIRFTYIter;                // Iteration counter for condenser outlet temperature EIRFT warning messages
        int EIRFTYIterIndex;           // Index for condenser outlet temperature EIRFT warning messages
        int EIRFPLRTIter;              // Iteration counter for condenser outlet temperature EIRFPLR warning messages
        int EIRFPLRTIterIndex;         // Index for condenser outlet temperature EIRFPLR warning messages
        int EIRFPLRPLRIter;            // Iteration counter for part-load ratio EIRFPLR warning messages
        int EIRFPLRPLRIterIndex;       // Index for part-load ratio EIRFPLR warning messages
        bool FaultyChillerSWTFlag;     // True if the chiller has SWT sensor fault
        int FaultyChillerSWTIndex;     // Index of the fault object corresponding to the chiller
        Real64 FaultyChillerSWTOffset; // Chiller SWT sensor offset
        int IterLimitExceededNum;      // Iteration limit exceeded for RegulaFalsi routine
        int IterLimitErrIndex;         // Index to iteration limit warning for RegulaFalsi routine
        int IterFailed;                // Iteration limit failed for RegulaFalsi routine
        int IterFailedIndex;           // Index to iteration limit failed for RegulaFalsi routine
        int DeltaTErrCount;            // Evaporator delta T equals 0 for variable flow chiller warning messages
        int DeltaTErrCountIndex;       // Index to evaporator delta T = 0 for variable flow chiller warning messages
        int CWLoopNum;                 // chilled water plant loop index number
        int CWLoopSideNum;             // chilled water plant loop side index
        int CWBranchNum;               // chilled water plant loop branch index
        int CWCompNum;                 // chilled water plant loop component index
        int CDLoopNum;                 // condenser water plant loop index number
        int CDLoopSideNum;             // condenser water plant loop side index
        int CDBranchNum;               // condenser water plant loop branch index
        int CDCompNum;                 // condenser water plant loop component index
        int HRLoopNum;                 // heat recovery water plant loop index
        int HRLoopSideNum;             // heat recovery water plant loop side index
        int HRBranchNum;               // heat recovery water plant loop branch index
        int HRCompNum;                 // heat recovery water plant loop component index
        int CondMassFlowIndex;
        bool PossibleSubcooling; // flag to indicate chiller is doing less cooling that requested
        // Operational fault parameters
        bool FaultyChillerFoulingFlag;     // True if the chiller has fouling fault
        int FaultyChillerFoulingIndex;     // Index of the fault object corresponding to the chiller
        Real64 FaultyChillerFoulingFactor; // Chiller fouling factor
        std::string EndUseSubcategory;     // identifier use for the end use subcategory
        bool MyEnvrnFlag;
        bool MyInitFlag;
        bool MySizeFlag;
        Real64 ChillerCondAvgTemp;   // average condenser temp for curves with Heat recovery [C]
        Real64 ChillerFalseLoadRate; // Chiller false load over and above water side load [J]
        Real64 ChillerCyclingRatio;  // Chiller cycling ratio (time on/time step)
        Real64 ChillerPartLoadRatio; // Chiller PLR (Load/Capacity)
        Real64 ChillerEIRFPLR;       // Chiller EIRFPLR curve output value
        Real64 ChillerEIRFT;         // Chiller EIRFT curve output value
        Real64 ChillerCapFT;         // Chiller capacity curve output value
        Real64 HeatRecOutletTemp;
        Real64 QHeatRecovery; // Heat recovered from water-cooled condenser [W]
        Real64 QCondenser;
        Real64 QEvaporator;        // Evaporator heat transfer rate [W]
        Real64 Power;              // Chiller power [W]
        Real64 EvapOutletTemp;     // Evaporator outlet temperature [C]
        Real64 CondOutletTemp;     // Condenser outlet temperature [C]
        Real64 EvapMassFlowRate;   // Evaporator mass flow rate [kg/s]
        Real64 CondMassFlowRate;   // Condenser mass flow rate [kg/s]
        Real64 ChillerFalseLoad;   // Chiller false load over and above water side load [W]
        Real64 Energy;             // Chiller electric consumption [J]
        Real64 EvapEnergy;         // Evaporator heat transfer energy [J]
        Real64 CondEnergy;         // Condenser heat transfer energy [J]
        Real64 CondInletTemp;      // Condenser inlet temperature [C]
        Real64 EvapInletTemp;      // Evaporator inlet temperature [C]
        Real64 ActualCOP;          // Coefficient of performance
        Real64 EnergyHeatRecovery; // Energy recovered from water-cooled condenser [J]
        Real64 HeatRecInletTemp;   // Heat reclaim inlet temperature [C]
        Real64 HeatRecMassFlow;    // Heat reclaim mass flow rate [kg/s]
        DataBranchAirLoopPlant::ControlTypeEnum EquipFlowCtrl;

        // Default Constructor
        ReformulatedEIRChillerSpecs()
            : TypeNum(0), CondenserType(DataPlant::CondenserType::Unassigned), PartLoadCurveType(PLR::Unassigned), RefCap(0.0),
              RefCapWasAutoSized(false), RefCOP(0.0), FlowMode(DataPlant::FlowMode::Unassigned), ModulatedFlowSetToLoop(false),
              ModulatedFlowErrDone(false), EvapVolFlowRate(0.0), EvapVolFlowRateWasAutoSized(false), EvapMassFlowRateMax(0.0), CondVolFlowRate(0.0),
              CondVolFlowRateWasAutoSized(false), CondMassFlowRateMax(0.0), CompPowerToCondenserFrac(0.0), EvapInletNodeNum(0), EvapOutletNodeNum(0),
              CondInletNodeNum(0), CondOutletNodeNum(0), MinPartLoadRat(0.0), MaxPartLoadRat(0.0), OptPartLoadRat(0.0), MinUnloadRat(0.0),
              TempRefCondIn(0.0), TempRefCondOut(0.0), TempRefEvapOut(0.0), TempLowLimitEvapOut(0.0), DesignHeatRecVolFlowRate(0.0),
              DesignHeatRecVolFlowRateWasAutoSized(false), DesignHeatRecMassFlowRate(0.0), SizFac(0.0), HeatRecActive(false), HeatRecInletNodeNum(0),
              HeatRecOutletNodeNum(0), HeatRecCapacityFraction(0.0), HeatRecMaxCapacityLimit(0.0), HeatRecSetPointNodeNum(0),
              HeatRecInletLimitSchedNum(0), ChillerCapFTIndex(0), ChillerEIRFTIndex(0), ChillerEIRFPLRIndex(0), ChillerCapFTError(0),
              ChillerCapFTErrorIndex(0), ChillerEIRFTError(0), ChillerEIRFTErrorIndex(0), ChillerEIRFPLRError(0), ChillerEIRFPLRErrorIndex(0),
              ChillerCAPFTXTempMin(0.0), ChillerCAPFTXTempMax(0.0), ChillerCAPFTYTempMin(0.0), ChillerCAPFTYTempMax(0.0), ChillerEIRFTXTempMin(0.0),
              ChillerEIRFTXTempMax(0.0), ChillerEIRFTYTempMin(0.0), ChillerEIRFTYTempMax(0.0), ChillerEIRFPLRTempMin(0.0), ChillerEIRFPLRTempMax(0.0),
              ChillerEIRFPLRPLRMin(0.0), ChillerEIRFPLRPLRMax(0.0), ChillerLiftNomMin(0.0), ChillerLiftNomMax(10.0), ChillerTdevNomMin(0.0),
              ChillerTdevNomMax(10.0), CAPFTXIter(0), CAPFTXIterIndex(0), CAPFTYIter(0), CAPFTYIterIndex(0), EIRFTXIter(0), EIRFTXIterIndex(0),
              EIRFTYIter(0), EIRFTYIterIndex(0), EIRFPLRTIter(0), EIRFPLRTIterIndex(0), EIRFPLRPLRIter(0), EIRFPLRPLRIterIndex(0),
              FaultyChillerSWTFlag(false), FaultyChillerSWTIndex(0), FaultyChillerSWTOffset(0.0), IterLimitExceededNum(0), IterLimitErrIndex(0),
              IterFailed(0), IterFailedIndex(0), DeltaTErrCount(0), DeltaTErrCountIndex(0), CWLoopNum(0), CWLoopSideNum(0), CWBranchNum(0),
              CWCompNum(0), CDLoopNum(0), CDLoopSideNum(0), CDBranchNum(0), CDCompNum(0), HRLoopNum(0), HRLoopSideNum(0), HRBranchNum(0),
              HRCompNum(0), CondMassFlowIndex(0), PossibleSubcooling(false), FaultyChillerFoulingFlag(false), FaultyChillerFoulingIndex(0),
              FaultyChillerFoulingFactor(1.0), MyEnvrnFlag(true), MyInitFlag(true), MySizeFlag(true), ChillerCondAvgTemp(0.0),
              ChillerFalseLoadRate(0.0), ChillerCyclingRatio(0.0), ChillerPartLoadRatio(0.0), ChillerEIRFPLR(0.0), ChillerEIRFT(0.0),
              ChillerCapFT(0.0), HeatRecOutletTemp(0.0), QHeatRecovery(0.0), QCondenser(0.0), QEvaporator(0.0), Power(0.0), EvapOutletTemp(0.0),
              CondOutletTemp(0.0), EvapMassFlowRate(0.0), CondMassFlowRate(0.0), ChillerFalseLoad(0.0), Energy(0.0), EvapEnergy(0.0), CondEnergy(0.0),
              CondInletTemp(0.0), EvapInletTemp(0.0), ActualCOP(0.0), EnergyHeatRecovery(0.0), HeatRecInletTemp(0.0), HeatRecMassFlow(0.0),
              EquipFlowCtrl(DataBranchAirLoopPlant::ControlTypeEnum::Unknown)
        {
        }

        static PlantComponent *factory(EnergyPlusData &state, std::string const &objectName);

        void simulate([[maybe_unused]] EnergyPlusData &state,
                      const PlantLocation &calledFromLocation,
                      bool FirstHVACIteration,
                      Real64 &CurLoad,
                      bool RunFlag) override;

        void getDesignCapacities(
            EnergyPlusData &state, const PlantLocation &calledFromLocation, Real64 &MaxLoad, Real64 &MinLoad, Real64 &OptLoad) override;

        void getDesignTemperatures(Real64 &TempDesCondIn, Real64 &TempDesEvapOut) override;

        void getSizingFactor(Real64 &sizFac) override;

        void onInitLoopEquip([[maybe_unused]] EnergyPlusData &state, const PlantLocation &calledFromLocation) override;

        void oneTimeInit(EnergyPlusData &state) override;

        void initialize(EnergyPlusData &state, bool RunFlag, Real64 MyLoad);

        void setupOutputVars(EnergyPlusData &state);

        void size(EnergyPlusData &state);

        void control(EnergyPlusData &state, Real64 &MyLoad, bool RunFlag, bool FirstIteration);

        void calculate(EnergyPlusData &state, Real64 &MyLoad, bool RunFlag, Real64 FalsiCondOutTemp);

        void calcHeatRecovery(EnergyPlusData &state, Real64 &QCond, Real64 CondMassFlow, Real64 condInletTemp, Real64 &QHeatRec);

        void update(EnergyPlusData &state, Real64 MyLoad, bool RunFlag);

        void checkMinMaxCurveBoundaries(EnergyPlusData &state, bool FirstIteration);

        Real64 condOutTempResidual(EnergyPlusData &state, Real64 FalsiCondOutTemp, std::array<Real64, 4> const &Par);
    };

    void GetElecReformEIRChillerInput(EnergyPlusData &state);

} // namespace ChillerReformulatedEIR

struct ChillerReformulatedEIRData : BaseGlobalStruct
{
    int NumElecReformEIRChillers = 0;
    bool GetInputREIR = true;
    Array1D<ChillerReformulatedEIR::ReformulatedEIRChillerSpecs> ElecReformEIRChiller;

    void clear_state() override
    {
        this->NumElecReformEIRChillers = 0;
        this->GetInputREIR = true;
        this->ElecReformEIRChiller.deallocate();
    }
};

} // namespace EnergyPlus

#endif
