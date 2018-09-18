// EnergyPlus, Copyright (c) 1996-2018, The Board of Trustees of the University of Illinois,
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

#ifndef DataPlant_hh_INCLUDED
#define DataPlant_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <DataGlobals.hh>
#include <DataLoopNode.hh>
#include <EnergyPlus.hh>
#include <OutputProcessor.hh>
#include <Plant/CallingOrder.hh>
#include <Plant/Enums.hh>
#include <Plant/Loop.hh>
#include <Plant/PlantAvailManager.hh>
#include <Plant/PlantLoopSolver.hh>
#include <Plant/ReportLoopData.hh>
#include <Plant/ReportVars.hh>

namespace EnergyPlus {

namespace DataPlant {

    // Using/Aliasing
    using DataLoopNode::SensedNodeFlagValue;

    int const OptimalLoading(1);              // Optimal Load Distribution Scheme
    int const SequentialLoading(2);           // Sequential Load Distribution Scheme
    int const UniformLoading(3);              // Uniform Load Distribution Scheme
    int const UniformPLRLoading(4);           // Uniform PLR Load Distribution Scheme
    int const SequentialUniformPLRLoading(5); // Sequential Uniform PLR Load Distribution Scheme

    extern int const LoadRangeBasedMin;
    extern int const LoadRangeBasedMax;

    // SimFlagCriteriaTypes for use in performing interconnect re-sim checks
    extern int const CriteriaType_MassFlowRate;
    extern int const CriteriaType_Temperature;
    extern int const CriteriaType_HeatTransferRate;

    // Criteria percentage limits for determining re-simulation of connected loop sides
    extern Real64 const CriteriaDelta_MassFlowRate;
    extern Real64 const CriteriaDelta_Temperature;
    extern Real64 const CriteriaDelta_HeatTransferRate;

    extern int const FreeCoolControlMode_WetBulb; // HeatExchanger:Hydronic model control type mode, outdoor wetbulb sensor
    extern int const FreeCoolControlMode_DryBulb; // HeatExchanger:Hydronic model control type mode, outdoor drybulb sensor
    extern int const FreeCoolControlMode_Loop;    // HeatExchanger:Hydronic model control type mode, loop setpoint sensor

    // Parameters for use in Loop Demand Calculation Schemes
    extern int const SingleSetPoint;       // Uses a single temp setpoint to calculate loop demand
    extern int const DualSetPointDeadBand; // Uses a dual temp setpoint with a deadband between the high
    //  and the low to calculate loop demand
    // Parameters for loop setpoint reference
    extern int const Air;
    extern int const Ground;

    // Parameters for common pipe
    extern int const CommonPipe_No;
    extern int const CommonPipe_Single;
    extern int const CommonPipe_TwoWay;

    // Parameters for loop side location
    extern int const DemandSupply_No;
    extern int const DemandSide;
    extern int const SupplySide;

    // Parameters for economizer
    extern int const Integrated;
    extern int const NonIntegrated;
    extern int const None;

    // Parameters for tolerance
    extern Real64 const LoopDemandTol; // minimum significant loop cooling or heating demand
    extern Real64 const DeltaTempTol;  // minimum significant loop temperature difference

    // Parameters for Component/Equipment Types  (ref: TypeOf in CompData)
    extern int const LoopType_Plant;
    extern int const LoopType_Condenser;
    extern int const LoopType_Both;

    // Parameters for FlowLock standardization
    extern int const FlowPumpQuery; // Used to ask the pumps for their min/max avail based on no constraints
    extern int const FlowUnlocked;  // components request flow
    extern int const FlowLocked;    // components take their inlet flow

    // Pressure Routine Call Enumeration
    extern int const PressureCall_Init;
    extern int const PressureCall_Calc;
    extern int const PressureCall_Update;

    // Pressure Simulation Types
    extern int const Press_NoPressure;          // Nothing for that particular loop
    extern int const Press_PumpPowerCorrection; // Only updating the pump power
    extern int const Press_FlowCorrection;      // Update pump flow rate based on pump curve
    extern int const Press_FlowSimulation;      // Full pressure network simulation
    extern Array1D_string const PressureSimType;
    // Parameters for Component/Equipment Types  (ref: TypeOf in CompData)
    extern int const NumSimPlantEquipTypes;
    extern Array1D_string const SimPlantEquipTypes;

    extern Array1D_string const ccSimPlantEquipTypes;

    extern Array1D_int const ValidLoopEquipTypes;

    extern int const TypeOf_Other;
    extern int const TypeOf_Boiler_Simple;
    extern int const TypeOf_Boiler_Steam;
    extern int const TypeOf_Chiller_Absorption;          // older BLAST absorption chiller
    extern int const TypeOf_Chiller_Indirect_Absorption; // revised absorption chiller
    extern int const TypeOf_Chiller_CombTurbine;
    extern int const TypeOf_Chiller_ConstCOP;
    extern int const TypeOf_Chiller_DFAbsorption;
    extern int const TypeOf_Chiller_Electric; // basic BLAST Chiller
    extern int const TypeOf_Chiller_ElectricEIR;
    extern int const TypeOf_Chiller_ElectricReformEIR;
    extern int const TypeOf_Chiller_EngineDriven;
    extern int const TypeOf_CoolingTower_SingleSpd;
    extern int const TypeOf_CoolingTower_TwoSpd;
    extern int const TypeOf_CoolingTower_VarSpd;
    extern int const TypeOf_Generator_FCExhaust;
    extern int const TypeOf_HeatPumpWtrHeaterPumped;
    extern int const TypeOf_HeatPumpWtrHeaterWrapped;
    extern int const TypeOf_HPWaterEFCooling;
    extern int const TypeOf_HPWaterEFHeating;
    extern int const TypeOf_HPWaterPECooling;
    extern int const TypeOf_HPWaterPEHeating;
    extern int const TypeOf_Pipe;
    extern int const TypeOf_PipeSteam;
    extern int const TypeOf_PipeExterior;
    extern int const TypeOf_PipeInterior;
    extern int const TypeOf_PipeUnderground;
    extern int const TypeOf_PurchChilledWater;
    extern int const TypeOf_PurchHotWater;
    extern int const TypeOf_TS_IceDetailed;
    extern int const TypeOf_TS_IceSimple;
    extern int const TypeOf_ValveTempering;
    extern int const TypeOf_WtrHeaterMixed;
    extern int const TypeOf_WtrHeaterStratified;
    extern int const TypeOf_PumpVariableSpeed;
    extern int const TypeOf_PumpConstantSpeed;
    extern int const TypeOf_PumpCondensate;
    extern int const TypeOf_PumpBankVariableSpeed;
    extern int const TypeOf_PumpBankConstantSpeed;
    extern int const TypeOf_WaterUseConnection;
    extern int const TypeOf_CoilWaterCooling;             // demand side component
    extern int const TypeOf_CoilWaterDetailedFlatCooling; // demand side component
    extern int const TypeOf_CoilWaterSimpleHeating;       // demand side component
    extern int const TypeOf_CoilSteamAirHeating;          // demand side component
    extern int const TypeOf_SolarCollectorFlatPlate;      // demand side component
    extern int const TypeOf_PlantLoadProfile;             // demand side component
    extern int const TypeOf_GrndHtExchgSystem;
    extern int const TypeOf_GrndHtExchgSurface;
    extern int const TypeOf_GrndHtExchgPond;
    extern int const TypeOf_Generator_MicroTurbine; // newer FSEC turbine
    extern int const TypeOf_Generator_ICEngine;
    extern int const TypeOf_Generator_CTurbine; // older BLAST turbine
    extern int const TypeOf_Generator_MicroCHP;
    extern int const TypeOf_Generator_FCStackCooler;
    extern int const TypeOf_FluidCooler_SingleSpd;
    extern int const TypeOf_FluidCooler_TwoSpd;
    extern int const TypeOf_EvapFluidCooler_SingleSpd;
    extern int const TypeOf_EvapFluidCooler_TwoSpd;
    extern int const TypeOf_ChilledWaterTankMixed;
    extern int const TypeOf_ChilledWaterTankStratified;
    extern int const TypeOf_PVTSolarCollectorFlatPlate;
    extern int const TypeOf_Baseboard_Conv_Water;
    extern int const TypeOf_Baseboard_Rad_Conv_Steam;
    extern int const TypeOf_Baseboard_Rad_Conv_Water;
    extern int const TypeOf_CoolingPanel_Simple;
    extern int const TypeOf_LowTempRadiant_VarFlow;
    extern int const TypeOf_LowTempRadiant_ConstFlow;
    extern int const TypeOf_CooledBeamAirTerminal;
    extern int const TypeOf_CoilWAHPHeatingEquationFit;
    extern int const TypeOf_CoilWAHPCoolingEquationFit;
    extern int const TypeOf_CoilWAHPHeatingParamEst;
    extern int const TypeOf_CoilWAHPCoolingParamEst;
    extern int const TypeOf_RefrigSystemWaterCondenser;
    extern int const TypeOf_RefrigerationWaterCoolRack;
    extern int const TypeOf_MultiSpeedHeatPumpRecovery;
    extern int const TypeOf_Chiller_ExhFiredAbsorption;
    extern int const TypeOf_PipingSystemPipeCircuit;
    extern int const TypeOf_SolarCollectorICS;
    extern int const TypeOf_CoilVSWAHPHeatingEquationFit;
    extern int const TypeOf_CoilVSWAHPCoolingEquationFit;
    extern int const TypeOf_PlantComponentUserDefined;
    extern int const TypeOf_CoilUserDefined;
    extern int const TypeOf_ZoneHVACAirUserDefined;
    extern int const TypeOf_AirTerminalUserDefined;
    extern int const TypeOf_HeatPumpVRF;
    extern int const TypeOf_GrndHtExchgHorizTrench;
    extern int const TypeOf_FluidToFluidPlantHtExchg;
    extern int const TypeOf_WaterSource;
    extern int const TypeOf_CentralGroundSourceHeatPump;
    extern int const TypeOf_UnitarySysRecovery;
    extern int const TypeOf_PackagedTESCoolingCoil;
    extern int const TypeOf_CoolingTower_VarSpdMerkel;
    extern int const TypeOf_SwimmingPool_Indoor;
    extern int const TypeOf_GrndHtExchgSlinky;
    extern int const TypeOf_FourPipeBeamAirTerminal;

    // Parameters for General Equipment Types
    extern int const NumGeneralEquipTypes;
    extern Array1D_string const GeneralEquipTypes;

    extern int const GenEquipTypes_Boiler;
    extern int const GenEquipTypes_Chiller;
    extern int const GenEquipTypes_CoolingTower;
    extern int const GenEquipTypes_Generator;
    extern int const GenEquipTypes_HeatExchanger;
    extern int const GenEquipTypes_HeatPump;
    extern int const GenEquipTypes_Pipe;
    extern int const GenEquipTypes_Pump;
    extern int const GenEquipTypes_Purchased;
    extern int const GenEquipTypes_ThermalStorage;
    extern int const GenEquipTypes_Valve;
    extern int const GenEquipTypes_WaterThermalTank;
    extern int const GenEquipTypes_WaterUse;
    extern int const GenEquipTypes_DemandCoil;
    extern int const GenEquipTypes_SolarCollector;
    extern int const GenEquipTypes_LoadProfile;
    extern int const GenEquipTypes_FluidCooler;
    extern int const GenEquipTypes_EvapFluidCooler;
    extern int const GenEquipTypes_GroundHeatExchanger;
    extern int const GenEquipTypes_ZoneHVACDemand;
    extern int const GenEquipTypes_Refrigeration;
    extern int const GenEquipTypes_PlantComponent;
    extern int const GenEquipTypes_CentralHeatPumpSystem;

    extern Array1D<Real64> const ConvergenceHistoryARR;
    extern Real64 const sum_ConvergenceHistoryARR;
    extern Real64 const square_sum_ConvergenceHistoryARR;
    extern Real64 const sum_square_ConvergenceHistoryARR;

    extern int NumPipes;                       // Total number of pipes
    extern int NumPlantPipes;                  // Total number of plant pipes
    extern int NumCondPipes;                   // Total number of condenser pipes
    extern int TotNumLoops;                    // number of plant and condenser loops
    extern int TotNumHalfLoops;                // number of half loops (2 * TotNumLoops)
    extern bool PlantFirstSizeCompleted;       // true if first-pass sizing is still going on and not finished
    extern bool PlantFirstSizesOkayToFinalize; // true if first-pass plant sizing is finish and can save results for simulation
    extern bool PlantFirstSizesOkayToReport;   // true if initial first pass size can be reported
    extern bool PlantFinalSizesOkayToReport;   // true if plant sizing is really all done and final results reported
    extern bool PlantReSizingCompleted;

    extern bool AnyEMSPlantOpSchemesInModel;

    extern Array1D_int EconBranchNum; // Branch num on which economizer is placed
    extern Array1D_int EconCompNum;   // Component num of economizer in the economizer branch

    extern Array1D_bool LoadChangeDownStream; // sim control flag.

    extern int PlantManageSubIterations; // tracks plant iterations to characterize solver
    extern int PlantManageHalfLoopCalls; // tracks number of half loop calls

    // Object Data
    extern Array1D<PlantLoopData> PlantLoop;
    extern Array1D<PlantAvailMgrData> PlantAvailMgr;
    extern Array1D<ReportVars> PlantReport;
    extern Array1D<ReportLoopData> VentRepPlantSupplySide;
    extern Array1D<ReportLoopData> VentRepPlantDemandSide;
    extern Array1D<ReportLoopData> VentRepCondSupplySide;
    extern Array1D<ReportLoopData> VentRepCondDemandSide;
    extern Array1D<PlantCallingOrderInfoStruct> PlantCallingOrderInfo;

    // Clears the global data in DataPlant.
    // Needed for unit tests, should not be normally called.
    void clear_state();

} // namespace DataPlant

} // namespace EnergyPlus

#endif
