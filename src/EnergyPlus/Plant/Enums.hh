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

#ifndef PlantOperationEnums_hh_INCLUDED
#define PlantOperationEnums_hh_INCLUDED

namespace EnergyPlus::DataPlant {

// Parameters for loop flow request priority,
//     used in logic to deal with Node%MassFlowRequest for determining overall loop flow rate
int const LoopFlowStatus_Unknown(21);             // component's status is not yet set
int const LoopFlowStatus_NeedyAndTurnsLoopOn(22); // component is a "winner" for loop flow requests
// active valve inside component that modulates flow
//  gets the loop going under most conditions
int const LoopFlowStatus_NeedyIfLoopOn(23); // component is a "winner" for loop flow requests
// but doesn't normally get the loop going to start with
//  once loop is going, may increase needs, non-zero minimums
int const LoopFlowStatus_TakesWhatGets(24); // component is a "loser" for loop flow requests,
// but if the loop is on it
// it does make flow requests (for s/m resolution)

// Parameters for scheme types
// Used in TYPE(OperationData)%OpSchemeType
// As in PlantLoop(:)%OpScheme(:)%OpSchemeType
// Also in PlantLoop()LoopSide()Branch()Comp()%CurOpSchemeType
// this may be changed later...
enum OpSchemeType
{ // Changed to enum: Better semantic fit and allows use in switch statements: Suggest this migration throughout EnergyPlus (and probably C++11
  // enum "class")
    UnknownStatusOpSchemeType = -2,
    NoControlOpSchemeType = -1,         // Scheme Type placeholder for items such as pipes
    HeatingRBOpSchemeType = 1,          // Scheme Type for Heating Load Range Based Operation
    CoolingRBOpSchemeType = 2,          // Scheme Type for Cooling  Load Range Based Operation
    WetBulbRBOpSchemeType = 3,          // Scheme Type for Wet bulb range based Operation
    DryBulbRBOpSchemeType = 4,          // Scheme Type for Dry bulb range based Operation
    DewPointRBOpSchemeType = 5,         // Scheme Type for Dewpoint range based Operation
    RelHumRBOpSchemeType = 6,           // Scheme Type for relative humidity range based Operation
    DryBulbTDBOpSchemeType = 7,         // Scheme Type for relative humidity range based Operation
    WetBulbTDBOpSchemeType = 8,         // Scheme Type for Wet bulb range based Operation
    DewPointTDBOpSchemeType = 9,        // Scheme Type for Wet bulb range based Operation
    CompSetPtBasedSchemeType = 10,      // Temp Based Control
    UncontrolledOpSchemeType = 11,      // Scheme Type for Uncontrolled Operation
    EMSOpSchemeType = 12,               // Scheme Type for EMS based operation user Define scheme
    PumpOpSchemeType = 13,              // Not really an OpScheme, just a placeholder
    DemandOpSchemeType = 14,            // Placeholder for demand side equipment such as coils
    FreeRejectionOpSchemeType = 15,     // Scheme Type for waterside economizers and the like
    WSEconOpSchemeType = 16,            // Scheme Type for waterside economizers and the like
    ThermalEnergyStorageSchemeType = 17 // Scheme Type for Simplified Thermal Energy Storage operation
};

enum class PlantEquipmentType
{
    Invalid = 0,
    Boiler_Simple = 1,
    Boiler_Steam = 2,
    Chiller_Absorption = 3,          // older BLAST absorption chiller
    Chiller_Indirect_Absorption = 4, // revised absorption chiller
    Chiller_CombTurbine = 5,
    Chiller_ConstCOP = 6,
    Chiller_DFAbsorption = 7,
    Chiller_Electric = 8, // basic BLAST Chiller
    Chiller_ElectricEIR = 9,
    Chiller_ElectricReformEIR = 10,
    Chiller_EngineDriven = 11,
    CoolingTower_SingleSpd = 12,
    CoolingTower_TwoSpd = 13,
    CoolingTower_VarSpd = 14,
    Generator_FCExhaust = 15,
    HeatPumpWtrHeaterPumped = 16,
    HPWaterEFCooling = 17,
    HPWaterEFHeating = 18,
    HPWaterPECooling = 19,
    HPWaterPEHeating = 20,
    Pipe = 21,
    PipeSteam = 22,
    PipeExterior = 23,
    PipeInterior = 24,
    PipeUnderground = 25,
    PurchChilledWater = 26,
    PurchHotWater = 27,
    TS_IceDetailed = 28,
    TS_IceSimple = 29,
    ValveTempering = 30,
    WtrHeaterMixed = 31,
    WtrHeaterStratified = 32,
    PumpVariableSpeed = 33,
    PumpConstantSpeed = 34,
    PumpCondensate = 35,
    PumpBankVariableSpeed = 36,
    PumpBankConstantSpeed = 37,
    WaterUseConnection = 38,
    CoilWaterCooling = 39,             // demand side component
    CoilWaterDetailedFlatCooling = 40, // demand side component
    CoilWaterSimpleHeating = 41,       // demand side component
    CoilSteamAirHeating = 42,          // demand side component
    SolarCollectorFlatPlate = 43,      // demand side component
    PlantLoadProfile = 44,             // demand side component
    GrndHtExchgSystem = 45,
    GrndHtExchgSurface = 46,
    GrndHtExchgPond = 47,
    Generator_MicroTurbine = 48, // newer FSEC turbine
    Generator_ICEngine = 49,
    Generator_CTurbine = 50, // older BLAST turbine
    Generator_MicroCHP = 51,
    Generator_FCStackCooler = 52,
    FluidCooler_SingleSpd = 53,
    FluidCooler_TwoSpd = 54,
    EvapFluidCooler_SingleSpd = 55,
    EvapFluidCooler_TwoSpd = 56,
    ChilledWaterTankMixed = 57,
    ChilledWaterTankStratified = 58,
    PVTSolarCollectorFlatPlate = 59,
    Baseboard_Conv_Water = 60,
    Baseboard_Rad_Conv_Steam = 61,
    Baseboard_Rad_Conv_Water = 62,
    LowTempRadiant_VarFlow = 63,
    LowTempRadiant_ConstFlow = 64,
    CooledBeamAirTerminal = 65,
    CoilWAHPHeatingEquationFit = 66,
    CoilWAHPCoolingEquationFit = 67,
    CoilWAHPHeatingParamEst = 68,
    CoilWAHPCoolingParamEst = 69,
    RefrigSystemWaterCondenser = 70,
    RefrigerationWaterCoolRack = 71,
    MultiSpeedHeatPumpRecovery = 72,
    Chiller_ExhFiredAbsorption = 73,
    PipingSystemPipeCircuit = 74,
    SolarCollectorICS = 75,
    CoilVSWAHPHeatingEquationFit = 76,
    CoilVSWAHPCoolingEquationFit = 77,
    PlantComponentUserDefined = 78,
    CoilUserDefined = 79,
    ZoneHVACAirUserDefined = 80,
    AirTerminalUserDefined = 81,
    HeatPumpVRF = 82,
    GrndHtExchgHorizTrench = 83,
    FluidToFluidPlantHtExchg = 84,
    WaterSource = 85,
    CentralGroundSourceHeatPump = 86,
    UnitarySysRecovery = 87,
    PackagedTESCoolingCoil = 88,
    CoolingTower_VarSpdMerkel = 89,
    SwimmingPool_Indoor = 90,
    GrndHtExchgSlinky = 91,
    HeatPumpWtrHeaterWrapped = 92,
    FourPipeBeamAirTerminal = 93,
    CoolingPanel_Simple = 94,
    HeatPumpEIRCooling = 95,
    HeatPumpEIRHeating = 96,
    Num = 97
};

// Parameters for component character wrt how load gets met (or not)
//  used in %HowLoadServed to facilitate load dispatch logic
int const HowMet_Unknown(50);                              // not yet set
int const HowMet_NoneDemand(51);                           // does not meet a load, demand component
int const HowMet_PassiveCap(52);                           // Passive machine, does what conditions allow but
int const HowMet_ByNominalCap(53);                         // MaxLoad, MinLoad, OptLoad should work
int const HowMet_ByNominalCapLowOutLimit(54);              // MaxLoad, MinLoad, OptLoad but with low limit temp on outlet
int const HowMet_ByNominalCapHiOutLimit(55);               // MaxLoad, MinLoad, OptLoad but with high limit temp on outlet
int const HowMet_ByNominalCapFreeCoolCntrl(56);            // HowMet_ByNominalCap with free cool shutdown
int const HowMet_ByNominalCapLowOutLimitFreeCoolCntrl(57); // HowMet_ByNominalCapLowOutLimit with free cool shutdown

enum class iLoadingScheme
{
    Unassigned,
    Optimal,              // Optimal Load Distribution Scheme
    Sequential,           // Sequential Load Distribution Scheme
    Uniform,              // Uniform Load Distribution Scheme
    UniformPLR,           // Uniform PLR Load Distribution Scheme
    SequentialUniformPLR, // Sequential Uniform PLR Load Distribution Scheme
};

enum class FlowMode
{
    Unassigned,
    Constant,
    NotModulated,
    LeavingSetpointModulated,
};

enum class CondenserType
{
    Unassigned,
    AirCooled,
    WaterCooled,
    EvapCooled,
};

// SimFlagCriteriaTypes for use in performing interconnect re-sim checks
enum class iCriteriaType
{
    MassFlowRate,
    Temperature,
    HeatTransferRate,
};

enum class iFreeCoolControlMode
{
    Unassigned,
    WetBulb, // HeatExchanger:Hydronic model control type mode, outdoor wetbulb sensor
    DryBulb, // HeatExchanger:Hydronic model control type mode, outdoor drybulb sensor
    Loop,    // HeatExchanger:Hydronic model control type mode, loop setpoint sensor
};

enum class iLoopDemandCalcScheme
{
    Unassigned,
    SingleSetPoint,       // Uses a single temp setpoint to calculate loop demand
    DualSetPointDeadBand, // Uses a dual temp setpoint with a deadband between the high
};

enum class iCommonPipeType
{
    No,
    Single,
    TwoWay,
};

enum class iFlowLock
{
    PumpQuery, // Used to ask the pumps for their min/max avail based on no constraints
    Unlocked,  // components request flow
    Locked,    // components take their inlet flow
};

enum class iPressureCall
{
    Init,
    Calc,
    Update,
};

enum class iPressSimType
{
    NoPressure,          // Nothing for that particular loop
    PumpPowerCorrection, // Only updating the pump power
    FlowCorrection,      // Update pump flow rate based on pump curve
    FlowSimulation,      // Full pressure network simulation
};

enum class iCtrlType
{
    Unassigned,
    HeatingOp, // Constant for Heating Operation
    CoolingOp, // Constant for Cooling Operation
    DualOp,    // Constant for Cooling or Heating Operation
};

// branch loop type for absorption chillerheater models
enum class BrLoopType
{
    Chiller,
    Heater,
    Condenser,
    NoMatch
};

} // namespace EnergyPlus::DataPlant

#endif
