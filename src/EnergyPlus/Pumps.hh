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

#ifndef Pumps_hh_INCLUDED
#define Pumps_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/EnergyPlus.hh>

namespace EnergyPlus {

// Forward declarations
struct EnergyPlusData;

namespace Pumps {

    // Using/Aliasing

    // Data
    // MODULE PARAMETER DEFINITIONS:

    enum class PumpControlType
    {
        Unassigned,
        Continuous,   // Pump control type (pump always running)
        Intermittent, // Pump control type (pump runs only when there is a demand)
    };

    enum class ControlTypeVFD
    {
        Unassigned,
        VFDManual,    // VFD control type (Scheduled RPM)
        VFDAutomatic, // VFD control type (Variable RPM according to flow request)
    };

    enum class PumpBankControlSeq
    {
        Unassigned,
        OptimalScheme,    // Control sequencing for pump bank
        SequentialScheme, // Control sequencing for pump bank
        UserDefined,      // Control sequencing for pump bank
    };

    extern std::string const cPump_VarSpeed;
    extern std::string const cPump_ConSpeed;
    extern std::string const cPump_Cond;
    extern std::string const cPumpBank_VarSpeed;
    extern std::string const cPumpBank_ConSpeed;

    enum class PumpType : int
    {
        Unassigned = -1,
        VarSpeed = 0,
        ConSpeed = 1,
        Cond = 2,
        Bank_VarSpeed = 3,
        Bank_ConSpeed = 4
    };

    enum class PowerSizingMethod
    {
        sizePowerPerFlow,
        sizePowerPerFlowPerPressure
    };

    struct PumpVFDControlData
    {
        // Members
        std::string Name;
        std::string ManualRPMSchedName;
        int ManualRPMSchedIndex;
        std::string LowerPsetSchedName;
        int LowerPsetSchedIndex;
        std::string UpperPsetSchedName;
        int UpperPsetSchedIndex;
        std::string MinRPMSchedName;
        int MinRPMSchedIndex;
        std::string MaxRPMSchedName;
        int MaxRPMSchedIndex;
        ControlTypeVFD VFDControlType; // VFDControlType
        Real64 MaxRPM;                 // Maximum RPM range value - schedule limit
        Real64 MinRPM;                 // Minimum RPM range value - schedule limit
        Real64 PumpActualRPM;          // RPM recalculated from final flow through the loop

        // Default Constructor
        PumpVFDControlData()
            : ManualRPMSchedIndex(0), LowerPsetSchedIndex(0), UpperPsetSchedIndex(0), MinRPMSchedIndex(0), MaxRPMSchedIndex(0),
              VFDControlType(ControlTypeVFD::Unassigned), MaxRPM(0.0), MinRPM(0.0), PumpActualRPM(0.0)
        {
        }
    };

    struct PumpSpecs
    {
        // Members
        std::string Name;               // user identifier
        std::string PumpSchedule;       // Schedule to modify the design nominal capacity of the pump
        std::string PressureCurve_Name; // - placeholder for pump curve name
        PumpType pumpType;              // pump type enumerator, based on local parameter values, used to identify
        // index in the cPumpTypes string array to do error reporting
        int TypeOf_Num;                              // pump type of number in reference to the dataplant values
        int LoopNum;                                 // loop where pump is located
        int LoopSideNum;                             // LoopSide index on loop where pump is located
        int BranchNum;                               // branch index on LoopSide where pump is located
        int CompNum;                                 // component index on branch where pump is located
        PumpControlType PumpControl;                 // Integer equivalent of PumpControlType
        int PumpScheduleIndex;                       // Schedule Pointer
        int InletNodeNum;                            // Node number on the inlet side of the plant
        int OutletNodeNum;                           // Node number on the outlet side of the plant
        PumpBankControlSeq SequencingScheme;         // Optimal, Sequential, User-Defined
        int FluidIndex;                              // Index for Fluid Properties
        int NumPumpsInBank;                          // Node number on the inlet side of the plant
        int PowerErrIndex1;                          // for recurring errors
        int PowerErrIndex2;                          // for recurring errors
        Real64 MinVolFlowRateFrac;                   // minimum schedule value fraction modifier
        Real64 NomVolFlowRate;                       // design nominal capacity of Pump
        bool NomVolFlowRateWasAutoSized;             // true if previous was autosize on input
        Real64 MassFlowRateMax;                      // design nominal capacity of Pump
        bool EMSMassFlowOverrideOn;                  // if true, then EMS is calling to override flow requests.
        Real64 EMSMassFlowValue;                     // EMS value to use for mass flow rate [kg/s]
        Real64 NomSteamVolFlowRate;                  // For Steam Pump
        bool NomSteamVolFlowRateWasAutoSized;        // true if steam volume flow rate was autosize on input
        Real64 MinVolFlowRate;                       // For a Variable Flow Pump this is the minimum capacity during operation.
        bool minVolFlowRateWasAutosized;             // true if minimum flow rate was autosize on input
        Real64 MassFlowRateMin;                      // For a Variable Flow Pump this is the minimum capacity during operation.
        Real64 NomPumpHead;                          // design nominal head pressure of Pump, [Pa]
        bool EMSPressureOverrideOn;                  // if true, EMS is calling to override pump pressure
        Real64 EMSPressureOverrideValue;             // EMS value to use for pressure [Pa]
        Real64 NomPowerUse;                          // design nominal capacity of Pump
        bool NomPowerUseWasAutoSized;                // true if power was autosize on input
        PowerSizingMethod powerSizingMethod;         // which method is used for sizing nominal power use
        Real64 powerPerFlowScalingFactor;            // design electric power per unit flow rate
        Real64 powerPerFlowPerPressureScalingFactor; // design shaft power per unit flow rate per unit head
        Real64 MotorEffic;                           // efficiency of the motor
        Real64 PumpEffic;                            // efficiency of the pump
        Real64 FracMotorLossToFluid;                 // ?????
        Real64 Energy;                               // Energy consumed
        Real64 Power;                                // Power used
        Array1D<Real64> PartLoadCoef;                // Pump Curve Coefficients
        int PressureCurve_Index;                     // Pointer to a pump coefficient curve
        Real64 PumpMassFlowRateMaxRPM;               // Mass flow rate calculated from maximum rpm
        Real64 PumpMassFlowRateMinRPM;               // Mass flow rate calculated from minimum rpm
        Real64 MinPhiValue;                          // Minimum value of Phi (from CurveManager)
        Real64 MaxPhiValue;                          // Maximum value of Phi (from CurveManager)
        Real64 ImpellerDiameter;                     // Pump Impeller Diameter [m]
        Real64 RotSpeed_RPM;                         // Rotational speed used for input in revs/min
        Real64 RotSpeed;                             // Rotational speed for calculations in revs/sec
        bool PumpInitFlag;
        bool PumpOneTimeFlag;
        bool CheckEquipName;
        bool HasVFD;
        PumpVFDControlData VFD;
        bool OneTimePressureWarning;
        bool HeatLossesToZone;        // if true then pump losses added to surrounding zone
        int ZoneNum;                  // index for zone surrounding pump
        Real64 SkinLossRadFraction;   // radiative split for skin losses to zone
        bool LoopSolverOverwriteFlag; // loop solver overwrite for determining pump minimum flow rate
        std::string EndUseSubcategoryName;

        // Default Constructor
        PumpSpecs()
            : pumpType(PumpType::Unassigned), TypeOf_Num(0), LoopNum(0), LoopSideNum(0), BranchNum(0), CompNum(0),
              PumpControl(PumpControlType::Unassigned), PumpScheduleIndex(0), InletNodeNum(0), OutletNodeNum(0),
              SequencingScheme(PumpBankControlSeq::Unassigned), FluidIndex(0), NumPumpsInBank(0), PowerErrIndex1(0), PowerErrIndex2(0),
              MinVolFlowRateFrac(0.0), NomVolFlowRate(0.0), NomVolFlowRateWasAutoSized(false), MassFlowRateMax(0.0), EMSMassFlowOverrideOn(false),
              EMSMassFlowValue(0.0), NomSteamVolFlowRate(0.0), NomSteamVolFlowRateWasAutoSized(false), MinVolFlowRate(0.0),
              minVolFlowRateWasAutosized(false), MassFlowRateMin(0.0), NomPumpHead(0.0), EMSPressureOverrideOn(false), EMSPressureOverrideValue(0.0),
              NomPowerUse(0.0), NomPowerUseWasAutoSized(false), powerSizingMethod(PowerSizingMethod::sizePowerPerFlowPerPressure),
              powerPerFlowScalingFactor(348701.1),            // 22 W/gpm
              powerPerFlowPerPressureScalingFactor(1 / 0.78), // legacy impeller efficiency
              MotorEffic(0.0), PumpEffic(0.0), FracMotorLossToFluid(0.0), Energy(0.0), Power(0.0), PartLoadCoef(4, 0.0), PressureCurve_Index(0),
              PumpMassFlowRateMaxRPM(0.0), PumpMassFlowRateMinRPM(0.0), MinPhiValue(0.0), MaxPhiValue(0.0), ImpellerDiameter(0.0), RotSpeed_RPM(0.0),
              RotSpeed(0.0), PumpInitFlag(true), PumpOneTimeFlag(true), CheckEquipName(true), HasVFD(false), OneTimePressureWarning(true),
              HeatLossesToZone(false), ZoneNum(0), SkinLossRadFraction(0.0), LoopSolverOverwriteFlag(false)
        {
        }
    };

    struct ReportVars
    {
        // Members
        int NumPumpsOperating;        // Used in pump bank. reports how many pumps are ON
        Real64 PumpMassFlowRate;      // Mass flow rate of the pump
        Real64 PumpHeattoFluid;       // Heat transfer from pump to fluid (W)
        Real64 PumpHeattoFluidEnergy; // Pump Energy dissipated into fluid stream
        Real64 OutletTemp;            // Pump outlet temperature
        Real64 ShaftPower;            // Power input at the shaft
        Real64 ZoneTotalGainRate;     // total pump skin losses to zone (W)
        Real64 ZoneTotalGainEnergy;   // total pump skin losses to zone energy (J)
        Real64 ZoneConvGainRate;      // pump skin losses convecting to zone air (W)
        Real64 ZoneRadGainRate;       // pump skin losses radiating to inside of zone (W)

        // Default Constructor
        ReportVars()
            : NumPumpsOperating(0), PumpMassFlowRate(0.0), PumpHeattoFluid(0.0), PumpHeattoFluidEnergy(0.0), OutletTemp(0.0), ShaftPower(0.0),
              ZoneTotalGainRate(0.0), ZoneTotalGainEnergy(0.0), ZoneConvGainRate(0.0), ZoneRadGainRate(0.0)
        {
        }
    };

    void SimPumps(EnergyPlusData &state,
                  std::string const &PumpName, // Name of pump to be managed
                  int const LoopNum,           // Plant loop number
                  Real64 const FlowRequest,    // requested flow from adjacent demand side
                  bool &PumpRunning,           // .TRUE. if the loop pump is actually operating
                  int &PumpIndex,
                  Real64 &PumpHeat);

    void GetPumpInput(EnergyPlusData &state);

    void InitializePumps(EnergyPlusData &state, int const PumpNum);

    void SetupPumpMinMaxFlows(EnergyPlusData &state, int const LoopNum, int const PumpNum);

    void CalcPumps(EnergyPlusData &state, int const PumpNum, Real64 const FlowRequest, bool &PumpRunning);

    void SizePump(EnergyPlusData &state, int const PumpNum);

    void ReportPumps(EnergyPlusData &state, int const PumpNum);

    void PumpDataForTable(EnergyPlusData &state, int const NumPump);

    void GetRequiredMassFlowRate(EnergyPlusData &state,
                                 int const LoopNum,
                                 int const PumpNum,
                                 Real64 const InletNodeMassFlowRate,
                                 Real64 &ActualFlowRate,
                                 Real64 &PumpMinMassFlowRateVFDRange,
                                 Real64 &PumpMaxMassFlowRateVFDRange);

} // namespace Pumps

struct PumpsData : BaseGlobalStruct
{

    int NumPumps = 0;         // Num Pumps (used in pump bank)
    int NumPumpsRunning = 0;  // Num of pumps ON (used in pump bank)
    int NumPumpsFullLoad = 0; // Num pumps running at full load (used in pump bank)
    bool GetInputFlag = true;
    Real64 PumpMassFlowRate = 0.0; // mass flow rate at pump inlet node
    Real64 PumpHeattoFluid = 0.0;  // Pump Power dissipated in fluid stream
    Real64 Power = 0.0;            // Pump Electric power
    Real64 ShaftPower = 0.0;       // Power passing through pump shaft
    Array1D<Pumps::PumpSpecs> PumpEquip;
    Array1D<Pumps::ReportVars> PumpEquipReport;
    std::unordered_map<std::string, std::string> PumpUniqueNames;

    void clear_state() override
    {
        this->NumPumps = 0;
        this->NumPumpsRunning = 0;
        this->NumPumpsFullLoad = 0;
        this->GetInputFlag = true;
        this->PumpMassFlowRate = 0.0;
        this->PumpHeattoFluid = 0.0;
        this->Power = 0.0;
        this->ShaftPower = 0.0;
        this->PumpEquip.deallocate();
        this->PumpEquipReport.deallocate();
        this->PumpUniqueNames.clear();
    }
};

} // namespace EnergyPlus

#endif
