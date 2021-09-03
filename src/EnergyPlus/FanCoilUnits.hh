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

#ifndef FanCoilUnits_hh_INCLUDED
#define FanCoilUnits_hh_INCLUDED

// C++ Headers
#include <memory>
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/EnergyPlus.hh>
#include <EnergyPlus/HVACFan.hh>

namespace EnergyPlus {

// Forward declarations
struct EnergyPlusData;

namespace FanCoilUnits {

    // Using/Aliasing

    // Data
    // MODULE PARAMETER DEFINITIONS

    // coil operation
    constexpr int On(1);  // normal coil operation
    constexpr int Off(0); // signal coil shouldn't run

    // coil type units supported in this module
    constexpr int FanCoilUnit_4Pipe(1);

    enum class CCoil
    {
        Unassigned,
        Water,
        Detailed,
        HXAssist
    };

    enum class HCoil
    {
        Unassigned,
        Water,
        Electric
    };

    enum class CCM // capacity control method supported in this module
    {
        Unassigned,
        ConsFanVarFlow,
        CycFan,
        VarFanVarFlow,
        VarFanConsFlow,
        MultiSpeedFan,
        ASHRAE
    };

    struct FanCoilData
    {
        // Members
        // Input data
        int UnitType_Num;
        std::string Sched;       // availability schedule
        int SchedPtr;            // index to schedule
        std::string SchedOutAir; // outside air schedule, multipliy maximum outdoor air flow rate
        int SchedOutAirPtr;      // index to outside air schedule
        int FanType_Num;         // index to fan type
        std::string CapCtrlMeth; // type of capacity control method
        // 'ConstantFanVariableFlow' or
        // 'CyclingFan' or
        // 'VariableFanVariableFlow'
        int SpeedFanSel; // Speed fan selected
        CCM CapCtrlMeth_Num;
        Real64 PLR;               // Part Load Ratio, fraction of time step fancoil is on
        int MaxIterIndexH;        // Maximum iterations exceeded for heating
        int BadMassFlowLimIndexH; // Bad mass flow limit error index for heating
        int MaxIterIndexC;        // Maximum iterations exceeded for cooling
        int BadMassFlowLimIndexC; // Bad mass flow limit error index for cooling
        Real64 FanAirVolFlow;     // m3/s
        Real64 MaxAirVolFlow;     // m3/s
        Real64 MaxAirMassFlow;    // kg/s
        Real64 LowSpeedRatio;     // Low speed fan supply air flow ratio
        Real64 MedSpeedRatio;     // Medium speed fan supply air flow ratio
        Real64 SpeedFanRatSel;    // Speed fan ratio determined by fan speed selection at each timestep
        Real64 OutAirVolFlow;     // m3/s
        Real64 OutAirMassFlow;    // kg/s
        int AirInNode;            // inlet air node number
        int AirOutNode;           // outlet air node number
        int OutsideAirNode;       // outside air node number
        int AirReliefNode;        // relief air node number
        int MixedAirNode;         // Mixed Air Node number
        std::string OAMixName;    // name of outside air mixer
        std::string OAMixType;    // type of outside air mixer
        int OAMixIndex;
        std::string FanName;   // name of fan
        std::string FanType;   // type of fan
        int FanIndex;          // index for fan
        std::string CCoilName; // name of cooling coil
        int CCoilName_Index;   // Index for this Cooling Coil in SimWaterComp
        std::string CCoilType; // type of cooling coil:
        // 'Coil:Cooling:Water' or
        // 'Coil:Cooling:Water:DetailedGeometry' or
        // 'CoilSystem:Cooling:Water:HeatExchangerAssisted'
        CCoil CCoilType_Num;        // Numeric equivalent for type of cooling coil
        std::string CCoilPlantName; // name of cooling coil (child<=CoilSystem:Cooling:Water:HeatExchangerAssisted)
        std::string CCoilPlantType; // type of cooling coil (child<=CoilSystem:Cooling:Water:HeatExchangerAssisted)
        int CCoilPlantTypeOfNum;
        int ControlCompTypeNum;
        int CompErrIndex;
        Real64 MaxColdWaterVolFlow; // m3/s
        Real64 MinColdWaterVolFlow; // m3/s
        Real64 MinColdWaterFlow;    // kg/s
        Real64 ColdControlOffset;   // control tolerance
        std::string HCoilName;      // name of heating coil
        int HCoilName_Index;
        std::string HCoilType; // type of heating coil:
        // 'Coil:Heating:Water' or
        HCoil HCoilType_Num; // Numeric equivalent for type of cooling coil
        int HCoilPlantTypeOfNum;
        Real64 MaxHotWaterVolFlow;    // m3/s
        Real64 MinHotWaterVolFlow;    // m3/s
        Real64 MinHotWaterFlow;       // kg/s
        Real64 HotControlOffset;      // control tolerance
        Real64 DesignHeatingCapacity; // size of electric heating coil [W]
        int AvailStatus;
        std::string AvailManagerListName; // Name of an availability manager list object
        // addition for OA to Zone Units
        std::string ATMixerName; // name of air terminal mixer
        int ATMixerIndex;        // index to the air terminal mixer
        int ATMixerType;         // 1 = inlet side mixer, 2 = supply side mixer
        int ATMixerPriNode;      // primary inlet air node number for the air terminal mixer
        int ATMixerSecNode;      // secondary air inlet node number for the air terminal mixer
        int HVACSizingIndex;     // index of a HVACSizing object for a fancoil unit
        Real64 SpeedRatio;       // speed ratio when the fan is cycling between stages
        int FanOpModeSchedPtr;   // pointer to supply air fan operating mode schedule
        int FanOpMode;           // 1=cycling fan cycling coil; 2=constant fan cycling coil
        bool ASHRAETempControl;  // ASHRAE90.1 control to temperature set point when true
        Real64 QUnitOutNoHC;     // unit output with coils off [W]
        Real64 QUnitOutMaxH;     // unit output at maximum heating [W]
        Real64 QUnitOutMaxC;     // unit output at maximum cooling [W]
        int LimitErrCountH;      // count of SolveRoot limit errors
        int LimitErrCountC;      // count of SolveRoot limit errors
        int ConvgErrCountH;      // count of SolveRoot iteration limit errors
        int ConvgErrCountC;      // count of SolveRoot iteration limit errors
        // Report data
        Real64 HeatPower;          // unit heating output in watts
        Real64 HeatEnergy;         // unit heating output in J
        Real64 TotCoolPower;       // unit total cooling power output in watts
        Real64 TotCoolEnergy;      // unit total cooling energy output in joules
        Real64 SensCoolPower;      // unit sensible cooling power output in watts
        Real64 SensCoolEnergy;     // unit sensible cooling energy output in joules
        Real64 ElecPower;          // unit electric power consumption in watts
        Real64 ElecEnergy;         // unit electiric energy consumption in joules
        Real64 DesCoolingLoad;     // used for reporting in watts
        Real64 DesHeatingLoad;     // used for reporting in watts
        Real64 DesZoneCoolingLoad; // used for reporting in watts
        Real64 DesZoneHeatingLoad; // used for reporting in watts
        int DSOAPtr;               // design specification outdoor air object index
        bool FirstPass;            // detects first time through for resetting sizing data
        int fanAvailSchIndex;      // fan availability schedule index

        // SZVAV Model inputs
        std::string Name;                // name of unit
        std::string UnitType;            // type of unit
        Real64 MaxCoolCoilFluidFlow;     // kg/s
        Real64 MaxHeatCoilFluidFlow;     // kg/s
        Real64 DesignMinOutletTemp;      // ASHRAE90.1 maximum supply air temperature in Cooling mode
        Real64 DesignMaxOutletTemp;      // ASHRAE90.1 maximum supply air temperature in Heating mode
        Real64 MaxNoCoolHeatAirMassFlow; // minimum air flow rate using constant fan and ASHRAE90.1 control method
        Real64 MaxCoolAirMassFlow;       // used in ASHRAE90.1 model, same as MaxAirMassFlow
        Real64 MaxHeatAirMassFlow;       // used in ASHRAE90.1 model, same as MaxAirMassFlow
        Real64 LowSpeedCoolFanRatio;     // ratio of min air flow to max air flow
        Real64 LowSpeedHeatFanRatio;     // ratio of min air flow to max air flow
        int CoolCoilFluidInletNode;      // chilled water control node
        int CoolCoilFluidOutletNodeNum;  // chilled water coil outlet plant node
        int HeatCoilFluidInletNode;      // hot water control node
        int HeatCoilFluidOutletNodeNum;  // hot water coil outlet plant node
        int CoolCoilLoopNum;             // index for plant loop with chilled water coil
        int CoolCoilLoopSide;            // index for plant loop side for chilled water coil
        int CoolCoilBranchNum;           // index for plant branch for chilled water coil
        int CoolCoilCompNum;             // index for plant component for chilled water coil
        int HeatCoilLoopNum;             // index for plant loop with hot water coil
        int HeatCoilLoopSide;            // index for plant loop side for hot water coil
        int HeatCoilBranchNum;           // index for plant branch for hot water coil
        int HeatCoilCompNum;             // index for plant component for hot water coil
        int CoolCoilInletNodeNum;        // index of cooling coil inlet node number
        int CoolCoilOutletNodeNum;       // index of cooling coil outlet node number
        int HeatCoilInletNodeNum;        // index of heating coil inlet node number
        int HeatCoilOutletNodeNum;       // index of heating coil outlet node number
        int ControlZoneNum;              // pointer to a zone served by a fancoil unit
        int NodeNumOfControlledZone;     // node number of controlled zone
        bool ATMixerExists;              // True if there is an ATMixer
        int ATMixerOutNode;              // outlet air node number for the air terminal mixer
        Real64 FanPartLoadRatio;         // ratio of air flow to max air flow to simulation modulating fan
        Real64 HeatCoilWaterFlowRatio;   // ratio of water flow rate to max water flow rate
        Real64 ControlZoneMassFlowFrac;  // flow fraction of control zone (always 1 for zone equipment)
        int MaxIterIndex;                // recurring message index
        int RegulaFalsiFailedIndex;      // iteration loop warning

        FanCoilData() // Default Constructor
            : UnitType_Num(0), SchedPtr(0), SchedOutAirPtr(0), FanType_Num(0), SpeedFanSel(0), CapCtrlMeth_Num(CCM::Unassigned), PLR(0.0),
              MaxIterIndexH(0), BadMassFlowLimIndexH(0), MaxIterIndexC(0), BadMassFlowLimIndexC(0), FanAirVolFlow(0.0), MaxAirVolFlow(0.0),
              MaxAirMassFlow(0.0), LowSpeedRatio(0.0), MedSpeedRatio(0.0), SpeedFanRatSel(0.0), OutAirVolFlow(0.0), OutAirMassFlow(0.0), AirInNode(0),
              AirOutNode(0), OutsideAirNode(0), AirReliefNode(0), MixedAirNode(0), OAMixIndex(0), FanIndex(0), CCoilName_Index(0),
              CCoilType_Num(CCoil::Unassigned), CCoilPlantTypeOfNum(0), ControlCompTypeNum(0), CompErrIndex(0), MaxColdWaterVolFlow(0.0),
              MinColdWaterVolFlow(0.0), MinColdWaterFlow(0.0), ColdControlOffset(0.0), HCoilName_Index(0), HCoilType_Num(HCoil::Unassigned),
              MaxHotWaterVolFlow(0.0), MinHotWaterVolFlow(0.0), MinHotWaterFlow(0.0), HotControlOffset(0.0), DesignHeatingCapacity(0.0),
              AvailStatus(0), ATMixerIndex(0), ATMixerType(0), ATMixerPriNode(0), ATMixerSecNode(0), HVACSizingIndex(0), SpeedRatio(0.0),
              FanOpModeSchedPtr(0), FanOpMode(1), ASHRAETempControl(false), QUnitOutNoHC(0.0), QUnitOutMaxH(0.0), QUnitOutMaxC(0.0),
              LimitErrCountH(0), LimitErrCountC(0), ConvgErrCountH(0), ConvgErrCountC(0), HeatPower(0.0), HeatEnergy(0.0), TotCoolPower(0.0),
              TotCoolEnergy(0.0), SensCoolPower(0.0), SensCoolEnergy(0.0), ElecPower(0.0), ElecEnergy(0.0), DesCoolingLoad(0.0), DesHeatingLoad(0.0),
              DesZoneCoolingLoad(0.0), DesZoneHeatingLoad(0.0), DSOAPtr(0), FirstPass(true), fanAvailSchIndex(0), MaxCoolCoilFluidFlow(0.0),
              MaxHeatCoilFluidFlow(0.0), DesignMinOutletTemp(0.0), DesignMaxOutletTemp(0.0), MaxNoCoolHeatAirMassFlow(0.0), MaxCoolAirMassFlow(0.0),
              MaxHeatAirMassFlow(0.0), LowSpeedCoolFanRatio(0.0), LowSpeedHeatFanRatio(0.0), CoolCoilFluidInletNode(0), CoolCoilFluidOutletNodeNum(0),
              HeatCoilFluidInletNode(0), HeatCoilFluidOutletNodeNum(0), CoolCoilLoopNum(0), CoolCoilLoopSide(0), CoolCoilBranchNum(0),
              CoolCoilCompNum(0), HeatCoilLoopNum(0), HeatCoilLoopSide(0), HeatCoilBranchNum(0), HeatCoilCompNum(0), CoolCoilInletNodeNum(0),
              CoolCoilOutletNodeNum(0), HeatCoilInletNodeNum(0), HeatCoilOutletNodeNum(0), ControlZoneNum(0), NodeNumOfControlledZone(0),
              ATMixerExists(false), ATMixerOutNode(0), FanPartLoadRatio(0.0), HeatCoilWaterFlowRatio(0.0), ControlZoneMassFlowFrac(1.0),
              MaxIterIndex(0), RegulaFalsiFailedIndex(0)
        {
        }
    };

    struct FanCoilNumericFieldData
    {
        // Members
        Array1D_string FieldNames;

        // Default Constructor
        FanCoilNumericFieldData()
        {
        }
    };

    void SimFanCoilUnit(EnergyPlusData &state,
                        std::string_view CompName,     // name of the fan coil unit
                        int const ZoneNum,             // number of zone being served
                        int const ControlledZoneNum,   // index into ZoneEquipConfig array; may not be equal to ZoneNum
                        bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
                        Real64 &PowerMet,              // Sensible power supplied (W)
                        Real64 &LatOutputProvided,     // Latent add/removal supplied by window AC (kg/s), dehumid = negative
                        int &CompIndex);

    void GetFanCoilUnits(EnergyPlusData &state);

    void InitFanCoilUnits(EnergyPlusData &state,
                          int const FanCoilNum,         // number of the current fan coil unit being simulated
                          int const ZoneNum,            // number of zone being served
                          int const ControlledZoneNum); // index into ZoneEquipConfig array; may not be equal to ZoneNum

    void SizeFanCoilUnit(EnergyPlusData &state,
                         int const FanCoilNum,
                         int const ControlledZoneNum); // index into ZoneEquipConfig array; may not be equal to ZoneNum

    void Sim4PipeFanCoil(EnergyPlusData &state,
                         int &FanCoilNum,               // number of the current fan coil unit being simulated
                         int const ZoneNum,             // number of zone being served
                         int const ControlledZoneNum,   // index into ZoneEqupConfig
                         bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
                         Real64 &PowerMet,              // Sensible power supplied (W)
                         Real64 &LatOutputProvided      // Latent power supplied (kg/s), negative = dehumidification
    );

    void TightenWaterFlowLimits(EnergyPlusData &state,
                                int const FanCoilNum,          // Unit index in fan coil array
                                bool const CoolingLoad,        // true if zone requires cooling
                                bool const HeatingLoad,        // true if zone requires heating
                                int const WaterControlNode,    // water control node, either cold or hot water
                                int const ControlledZoneNum,   // controlling zone index
                                bool const FirstHVACIteration, //  TRUE if 1st HVAC simulation of system timestep
                                Real64 const QZnReq,           // zone load [W]
                                Real64 &MinWaterFlow,          // minimum water flow rate
                                Real64 &MaxWaterFlow           // maximum water flow rate
    );

    void TightenAirAndWaterFlowLimits(EnergyPlusData &state,
                                      int const FanCoilNum,          // Unit index in fan coil array
                                      bool const CoolingLoad,        // true if zone requires cooling
                                      bool const HeatingLoad,        // true if zone requires heating
                                      int const WaterControlNode,    // water control node, either cold or hot water
                                      int const ControlledZoneNum,   // controlling zone index
                                      bool const FirstHVACIteration, //  TRUE if 1st HVAC simulation of system timestep
                                      Real64 const QZnReq,           // zone load [W]
                                      Real64 &PLRMin,                // minimum part-load ratio
                                      Real64 &PLRMax                 // maximum part-load ratio
    );

    void Calc4PipeFanCoil(EnergyPlusData &state,
                          int const FanCoilNum,          // Unit index in fan coil array
                          int const ControlledZoneNum,   // ZoneEquipConfig index
                          bool const FirstHVACIteration, // flag for 1st HVAV iteration in the time step
                          Real64 &LoadMet,               // load met by unit (watts)
                          Optional<Real64> PLR = _,      // Part Load Ratio, fraction of time step fancoil is on
                          Real64 ElecHeatCoilPLR = 1.0   // electric heating coil PLR used with MultiSpeedFan capacity control
    );

    void SimMultiStage4PipeFanCoil(EnergyPlusData &state,
                                   int &FanCoilNum,               // number of the current fan coil unit being simulated
                                   int const ControlledZoneNum,   // index into ZoneEqupConfig
                                   bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
                                   Real64 &PowerMet               // Sensible power supplied (W)
    );

    void CalcMultiStage4PipeFanCoil(EnergyPlusData &state,
                                    int &FanCoilNum,               // number of the current fan coil unit being simulated
                                    int const ZoneNum,             // number of zone being served
                                    bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
                                    Real64 const QZnReq,           // current zone cooling or heating load
                                    Real64 &SpeedRatio,            // fan coil speed ratio
                                    Real64 &PartLoadRatio,         // fan coil part load ratio
                                    Real64 &PowerMet               // Sensible power supplied (W)
    );

    void ReportFanCoilUnit(EnergyPlusData &state, int const FanCoilNum); // number of the current fan coil unit being simulated

    int GetFanCoilZoneInletAirNode(EnergyPlusData &state, int const FanCoilNum);

    int GetFanCoilOutAirNode(EnergyPlusData &state, int const FanCoilNum);

    int GetFanCoilReturnAirNode(EnergyPlusData &state, int const FanCoilNum);

    int GetFanCoilMixedAirNode(EnergyPlusData &state, int const FanCoilNum);

    int GetFanCoilInletAirNode(EnergyPlusData &state, int const FanCoilNum);

    void GetFanCoilIndex(EnergyPlusData &state, std::string const &FanCoilName, int &FanCoilIndex);

    Real64 CalcFanCoilLoadResidual(EnergyPlusData &state,
                                   Real64 const PartLoadRatio, // DX cooling coil part load ratio
                                   Array1D<Real64> const &Par  // Function parameters
    );

    Real64 CalcFanCoilPLRResidual(EnergyPlusData &state,
                                  Real64 const PLR,          // part-load ratio of air and water mass flow rate
                                  Array1D<Real64> const &Par // Function parameters
    );

    Real64 CalcFanCoilHeatCoilPLRResidual(EnergyPlusData &state,
                                          Real64 const CyclingR,     // electric heating coil cycling ratio
                                          Array1D<Real64> const &Par // Function parameters
    );

    Real64 CalcFanCoilHWLoadResidual(EnergyPlusData &state,
                                     Real64 const HWFlow,       // water mass flow rate [kg/s]
                                     Array1D<Real64> const &Par // Function parameters
    );

    Real64 CalcFanCoilCWLoadResidual(EnergyPlusData &state,
                                     Real64 const CWFlow,       // water mass flow rate [kg/s]
                                     Array1D<Real64> const &Par // Function parameters
    );
    Real64 CalcFanCoilWaterFlowTempResidual(EnergyPlusData &state,
                                            Real64 const WaterFlow,    // water mass flow rate [kg/s]
                                            Array1D<Real64> const &Par // Function parameters
    );

    Real64 CalcFanCoilWaterFlowResidual(EnergyPlusData &state,
                                        Real64 const WaterFlow,    // water mass flow rate [kg/s]
                                        Array1D<Real64> const &Par // Function parameters
    );

    Real64 CalcFanCoilAirAndWaterFlowResidual(EnergyPlusData &state,
                                              Real64 const WaterFlow,    // water mass flow rate [kg/s]
                                              Array1D<Real64> const &Par // Function parameters
    );

    Real64 CalcFanCoilAirAndWaterInStepResidual(EnergyPlusData &state,
                                                Real64 const PLR,          // air and water mass flow rate ratio
                                                Array1D<Real64> const &Par // Function parameters
    );

    Real64 CalcFanCoilBothFlowResidual(EnergyPlusData &state,
                                       Real64 const PLR,          // air and water mass flow rate ratio
                                       Array1D<Real64> const &Par // Function parameters
    );

    Real64 CalcFanCoilElecHeatResidual(EnergyPlusData &state,
                                       Real64 const PLR,          // electric heating coil part load ratio
                                       Array1D<Real64> const &Par // Function parameters
    );

    Real64 CalcFanCoilElecHeatTempResidual(EnergyPlusData &state,
                                           Real64 const PLR,          // electric heating coil part load ratio
                                           Array1D<Real64> const &Par // Function parameters
    );

} // namespace FanCoilUnits

struct FanCoilUnitsData : BaseGlobalStruct
{

    std::string const cMO_FanCoil = "ZoneHVAC:FourPipeFanCoil";
    int NumFanCoils = 0;
    int Num4PipeFanCoils = 0;
    Array1D_bool MySizeFlag;
    Array1D_bool CheckEquipName;
    bool GetFanCoilInputFlag = true; // First time, input is "gotten"
    Real64 FanFlowRatio = 0.0;
    bool HeatingLoad = false;       // True when zone needs heating
    bool CoolingLoad = false;       // True when zone needs cooling
    Real64 const Small5WLoad = 5.0; // load threshold 5.0 W
    Array1D<FanCoilUnits::FanCoilData> FanCoil;
    Array1D<FanCoilUnits::FanCoilNumericFieldData> FanCoilNumericFields;
    bool InitFanCoilUnitsOneTimeFlag = true;
    bool InitFanCoilUnitsCheckInZoneEquipmentListFlag = false; // True after the Zone Equipment List has been checked for items

    // static variables extracted from functions
    bool ErrorsFound = false;        // Set to true if errors in input, fatal at end of routine
    bool errFlag = false;            // Local error flag for GetOAMixerNodeNums
    int TotalArgs = 0;               // Total number of alpha and numeric arguments (max) for a
    bool ZoneExNodeNotFound = false; // used in error checking
    bool ZoneInNodeNotFound = false; // used in error checking
    int ATMixerNum = 0;              // index of air terminal mixer in the air terminal mixer data array
    int ATMixerType = 0;             // type of air terminal mixer (1=inlet side; 2=supply side)
    int ATMixerPriNode = 0;          // node number of the air terminal mixer primary air inlet
    int ATMixerSecNode = 0;          // node number of the air terminal mixer secondary air inlet
    int ATMixerOutNode = 0;          // node number of the air terminal mixer secondary air inlet
    Array1D_bool MyEnvrnFlag;
    Array1D_bool MyPlantScanFlag;
    Array1D_bool MyZoneEqFlag; // used to set up zone equipment availability managers
    int CoilWaterInletNode = 0;
    int CoilWaterOutletNode = 0;
    int ATMixOutNode = 0; // outlet node of ATM Mixer
    int ZoneNode = 0;     // zone node

    void clear_state() override
    {
        this->NumFanCoils = 0;
        this->Num4PipeFanCoils = 0;
        this->MySizeFlag.deallocate();
        this->CheckEquipName.deallocate();
        this->GetFanCoilInputFlag = true;
        this->FanFlowRatio = 0.0;
        this->HeatingLoad = false;
        this->CoolingLoad = false;
        this->FanCoil.deallocate();
        this->FanCoilNumericFields.deallocate();
        this->InitFanCoilUnitsOneTimeFlag = true;
        this->InitFanCoilUnitsCheckInZoneEquipmentListFlag = false;
        this->ErrorsFound = false;
        this->errFlag = false;
        this->TotalArgs = 0;
        this->ZoneExNodeNotFound = false;
        this->ZoneInNodeNotFound = false;
        this->ATMixerNum = 0;
        this->ATMixerType = 0;
        this->ATMixerPriNode = 0;
        this->ATMixerSecNode = 0;
        this->ATMixerOutNode = 0;
        this->MyEnvrnFlag.clear();
        this->MyPlantScanFlag.clear();
        this->MyZoneEqFlag.clear();
        this->CoilWaterInletNode = 0;
        this->CoilWaterOutletNode = 0;
        this->ATMixOutNode = 0;
        this->ZoneNode = 0;
    }
};

} // namespace EnergyPlus

#endif
