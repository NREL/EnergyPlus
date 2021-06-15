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

// C++ Headers
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EnergyPlus/Autosizing/All_Simple_Sizing.hh>
#include <EnergyPlus/Autosizing/CoolingAirFlowSizing.hh>
#include <EnergyPlus/Autosizing/CoolingCapacitySizing.hh>
#include <EnergyPlus/Autosizing/CoolingWaterDesAirInletHumRatSizing.hh>
#include <EnergyPlus/Autosizing/CoolingWaterDesAirInletTempSizing.hh>
#include <EnergyPlus/Autosizing/CoolingWaterDesAirOutletHumRatSizing.hh>
#include <EnergyPlus/Autosizing/CoolingWaterDesAirOutletTempSizing.hh>
#include <EnergyPlus/Autosizing/CoolingWaterDesWaterInletTempSizing.hh>
#include <EnergyPlus/Autosizing/CoolingWaterNumofTubesPerRowSizing.hh>
#include <EnergyPlus/Autosizing/CoolingWaterflowSizing.hh>
#include <EnergyPlus/Autosizing/HeatingAirFlowSizing.hh>
#include <EnergyPlus/Autosizing/HeatingAirflowUASizing.hh>
#include <EnergyPlus/Autosizing/HeatingCapacitySizing.hh>
#include <EnergyPlus/Autosizing/HeatingWaterDesAirInletHumRatSizing.hh>
#include <EnergyPlus/Autosizing/HeatingWaterDesAirInletTempSizing.hh>
#include <EnergyPlus/Autosizing/HeatingWaterDesCoilLoadUsedForUASizing.hh>
#include <EnergyPlus/Autosizing/HeatingWaterDesCoilWaterVolFlowUsedForUASizing.hh>
#include <EnergyPlus/Autosizing/HeatingWaterflowSizing.hh>
#include <EnergyPlus/Autosizing/WaterHeatingCapacitySizing.hh>
#include <EnergyPlus/Autosizing/WaterHeatingCoilUASizing.hh>
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataBranchAirLoopPlant.hh>
#include <EnergyPlus/DataContaminantBalance.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataWater.hh>
#include <EnergyPlus/EMSManager.hh>
#include <EnergyPlus/FaultsManager.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/HVACControllers.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ReportCoilSelection.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SetPointManager.hh>
#include <EnergyPlus/SimAirServingZones.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/WaterCoils.hh>
#include <EnergyPlus/WaterManager.hh>

namespace EnergyPlus::WaterCoils {
// Module containing the WaterCoil simulation routines

// MODULE INFORMATION:
//       AUTHOR         Richard J. Liesen
//       DATE WRITTEN   April 1998
//       MODIFIED       April 2004: Rahul Chillar
//                      Feb. 2010, Brent Griffith, Plant Demand Side Update, general fluid properties
//       RE-ENGINEERED  na

// PURPOSE OF THIS MODULE:
// To encapsulate the data and algorithms required to
// manage the WaterCoil System Component

using namespace DataLoopNode;
using namespace DataHVACGlobals;
using DataPlant::TypeOf_CoilWaterCooling;
using DataPlant::TypeOf_CoilWaterDetailedFlatCooling;
using DataPlant::TypeOf_CoilWaterSimpleHeating;
using FluidProperties::GetDensityGlycol;
using FluidProperties::GetSpecificHeatGlycol;
using Psychrometrics::PsyCpAirFnW;
using Psychrometrics::PsyHFnTdbRhPb;
using Psychrometrics::PsyHFnTdbW;
using Psychrometrics::PsyRhoAirFnPbTdbW;
using Psychrometrics::PsyTdbFnHW;
using Psychrometrics::PsyTdpFnWPb;
using Psychrometrics::PsyTsatFnHPb;
using Psychrometrics::PsyWFnTdbH;
using Psychrometrics::PsyWFnTdbRhPb;
using Psychrometrics::PsyWFnTdbTwbPb;
using Psychrometrics::PsyWFnTdpPb;
using namespace ScheduleManager;

void SimulateWaterCoilComponents(EnergyPlusData &state,
                                 std::string_view CompName,
                                 bool const FirstHVACIteration,
                                 int &CompIndex,
                                 Optional<Real64> QActual,
                                 Optional_int_const FanOpMode,
                                 Optional<Real64 const> PartLoadRatio)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard Liesen
    //       DATE WRITTEN   February 1998
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine manages WaterCoil component simulation.

    // Using/Aliasing

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int CoilNum;         // The WaterCoil that you are currently loading input into
    int OpMode;          // fan operating mode
    Real64 PartLoadFrac; // part-load fraction of heating coil

    // Obtains and Allocates WaterCoil related parameters from input file
    if (state.dataWaterCoils->GetWaterCoilsInputFlag) { // First time subroutine has been entered
        GetWaterCoilInput(state);
        state.dataWaterCoils->GetWaterCoilsInputFlag = false;
    }

    // Find the correct WaterCoilNumber with the Coil Name
    if (CompIndex == 0) {
        CoilNum = UtilityRoutines::FindItemInList(CompName, state.dataWaterCoils->WaterCoil);
        if (CoilNum == 0) {
            ShowFatalError(state, "SimulateWaterCoilComponents: Coil not found=" + std::string{CompName});
        }
        CompIndex = CoilNum;
    } else {
        CoilNum = CompIndex;
        if (CoilNum > state.dataWaterCoils->NumWaterCoils || CoilNum < 1) {
            ShowFatalError(state,
                           format("SimulateWaterCoilComponents: Invalid CompIndex passed={}, Number of Water Coils={}, Coil name={}",
                                  CoilNum,
                                  state.dataWaterCoils->NumWaterCoils,
                                  CompName));
        }
        if (state.dataWaterCoils->CheckEquipName(CoilNum)) {
            if (CompName != state.dataWaterCoils->WaterCoil(CoilNum).Name) {
                ShowFatalError(state,
                               format("SimulateWaterCoilComponents: Invalid CompIndex passed={}, Coil name={}, stored Coil Name for that index={}",
                                      CoilNum,
                                      CompName,
                                      state.dataWaterCoils->WaterCoil(CoilNum).Name));
            }
            state.dataWaterCoils->CheckEquipName(CoilNum) = false;
        }
    }

    // With the correct CoilNum Initialize
    InitWaterCoil(state, CoilNum, FirstHVACIteration); // Initialize all WaterCoil related parameters

    if (present(FanOpMode)) {
        OpMode = FanOpMode;
    } else {
        OpMode = ContFanCycCoil;
    }
    if (present(PartLoadRatio)) {
        PartLoadFrac = PartLoadRatio;
    } else {
        PartLoadFrac = 1.0;
    }

    // Calculate the Correct WaterCoil Model with the current CoilNum
    if (state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType == DataPlant::TypeOf_CoilWaterDetailedFlatCooling) {
        CalcDetailFlatFinCoolingCoil(state, CoilNum, state.dataWaterCoils->SimCalc, OpMode, PartLoadFrac);
        if (present(QActual)) QActual = state.dataWaterCoils->WaterCoil(CoilNum).SenWaterCoolingCoilRate;
    } else if (state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType == DataPlant::TypeOf_CoilWaterCooling) {
        CoolingCoil(state, CoilNum, FirstHVACIteration, state.dataWaterCoils->SimCalc, OpMode, PartLoadFrac);
        if (present(QActual)) QActual = state.dataWaterCoils->WaterCoil(CoilNum).SenWaterCoolingCoilRate;
    }

    if (state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType == DataPlant::TypeOf_CoilWaterSimpleHeating) {
        CalcSimpleHeatingCoil(state, CoilNum, OpMode, PartLoadFrac, state.dataWaterCoils->SimCalc);
        if (present(QActual)) QActual = state.dataWaterCoils->WaterCoil(CoilNum).TotWaterHeatingCoilRate;
    }

    // Update the current WaterCoil to the outlet nodes
    UpdateWaterCoil(state, CoilNum);

    // Report the current WaterCoil
    ReportWaterCoil(state, CoilNum);
}

// Get Input Section of the Module
//******************************************************************************

void GetWaterCoilInput(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard Liesen
    //       DATE WRITTEN   April 1998
    //       MODIFIED       April 2004: Rahul Chillar
    //                      November 2013: Tianzhen Hong for fouling coils
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Obtains input data for coils and stores it in coil data structures

    // METHODOLOGY EMPLOYED:
    // Uses "Get" routines to read in data.

    // Using/Aliasing
    using BranchNodeConnections::TestCompSet;
    using DataSizing::AutoSize;
    using GlobalNames::VerifyUniqueCoilName;
    using NodeInputManager::GetOnlySingleNode;
    using SetPointManager::NodeHasSPMCtrlVarType;
    using WaterManager::SetupTankSupplyComponent;
    using namespace FaultsManager;

    // SUBROUTINE PARAMETER DEFINITIONS:
    static constexpr std::string_view RoutineName("GetWaterCoilInput: "); // include trailing blank space

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int CoilNum; // The WaterCoil that you are currently loading input into
    int NumSimpHeat(0);
    int NumFlatFin(0);
    int NumCooling(0);
    int SimpHeatNum;
    int FlatFinNum;
    int CoolingNum;
    int NumAlphas;
    int NumNums;
    int IOStat;
    std::string CurrentModuleObject; // for ease in getting objects
    Array1D_string AlphArray;        // Alpha input items for object
    Array1D_string cAlphaFields;     // Alpha field names
    Array1D_string cNumericFields;   // Numeric field names
    Array1D<Real64> NumArray;        // Numeric input items for object
    Array1D_bool lAlphaBlanks;       // Logical array, alpha field input BLANK = .TRUE.
    Array1D_bool lNumericBlanks;     // Logical array, numeric field input BLANK = .TRUE.
    int MaxNums(0);                  // Maximum number of numeric input fields
    int MaxAlphas(0);                // Maximum number of alpha input fields
    int TotalArgs(0);                // Total number of alpha and numeric arguments (max) for a certain object in the input file
    bool ErrorsFound(false);         // If errors detected in input

    NumSimpHeat = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Coil:Heating:Water");
    NumFlatFin = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Coil:Cooling:Water:DetailedGeometry");
    NumCooling = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Coil:Cooling:Water");
    state.dataWaterCoils->NumWaterCoils = NumSimpHeat + NumFlatFin + NumCooling;

    if (state.dataWaterCoils->NumWaterCoils > 0) {
        state.dataWaterCoils->WaterCoil.allocate(state.dataWaterCoils->NumWaterCoils);
        state.dataWaterCoils->WaterCoilNumericFields.allocate(state.dataWaterCoils->NumWaterCoils);
        state.dataWaterCoils->WaterTempCoolCoilErrs.dimension(state.dataWaterCoils->NumWaterCoils, 0);
        state.dataWaterCoils->PartWetCoolCoilErrs.dimension(state.dataWaterCoils->NumWaterCoils, 0);
        state.dataWaterCoils->CheckEquipName.dimension(state.dataWaterCoils->NumWaterCoils, true);
    }

    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, "Coil:Heating:Water", TotalArgs, NumAlphas, NumNums);
    MaxNums = max(MaxNums, NumNums);
    MaxAlphas = max(MaxAlphas, NumAlphas);
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, "Coil:Cooling:Water:DetailedGeometry", TotalArgs, NumAlphas, NumNums);
    MaxNums = max(MaxNums, NumNums);
    MaxAlphas = max(MaxAlphas, NumAlphas);
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, "Coil:Cooling:Water", TotalArgs, NumAlphas, NumNums);
    MaxNums = max(MaxNums, NumNums);
    MaxAlphas = max(MaxAlphas, NumAlphas);

    AlphArray.allocate(MaxAlphas);
    cAlphaFields.allocate(MaxAlphas);
    cNumericFields.allocate(MaxNums);
    NumArray.dimension(MaxNums, 0.0);
    lAlphaBlanks.dimension(MaxAlphas, true);
    lNumericBlanks.dimension(MaxNums, true);
    auto &cCurrentModuleObject = state.dataIPShortCut->cCurrentModuleObject;
    CurrentModuleObject = "Coil:Heating:Water";
    // Get the data for simple heating coils
    for (SimpHeatNum = 1; SimpHeatNum <= NumSimpHeat; ++SimpHeatNum) {

        CoilNum = SimpHeatNum;

        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 CurrentModuleObject,
                                                                 SimpHeatNum,
                                                                 AlphArray,
                                                                 NumAlphas,
                                                                 NumArray,
                                                                 NumNums,
                                                                 IOStat,
                                                                 lNumericBlanks,
                                                                 lAlphaBlanks,
                                                                 cAlphaFields,
                                                                 cNumericFields);

        state.dataWaterCoils->WaterCoilNumericFields(CoilNum).FieldNames.allocate(MaxNums);
        state.dataWaterCoils->WaterCoilNumericFields(CoilNum).FieldNames = "";
        state.dataWaterCoils->WaterCoilNumericFields(CoilNum).FieldNames = cNumericFields;
        UtilityRoutines::IsNameEmpty(state, AlphArray(1), cCurrentModuleObject, ErrorsFound);

        // ErrorsFound will be set to True if problem was found, left untouched otherwise
        VerifyUniqueCoilName(state, CurrentModuleObject, AlphArray(1), ErrorsFound, CurrentModuleObject + " Name");

        state.dataWaterCoils->WaterCoil(CoilNum).Name = AlphArray(1);
        state.dataWaterCoils->WaterCoil(CoilNum).Schedule = AlphArray(2);
        if (lAlphaBlanks(2)) {
            state.dataWaterCoils->WaterCoil(CoilNum).SchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
        } else {
            state.dataWaterCoils->WaterCoil(CoilNum).SchedPtr = GetScheduleIndex(state, AlphArray(2));
            if (state.dataWaterCoils->WaterCoil(CoilNum).SchedPtr == 0) {
                ShowSevereError(state,
                                CurrentModuleObject + ": invalid " + cAlphaFields(2) + " entered =" + AlphArray(2) + " for " + cAlphaFields(1) + '=' +
                                    AlphArray(1));
                ErrorsFound = true;
            }
        }

        state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilTypeA = "Heating";
        state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType = DataPlant::TypeOf_CoilWaterCooling; // 'Heating'
        state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilModelA = "SIMPLE";
        state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilModel = iCoilModel::HeatingSimple; // 'SIMPLE'
        state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType = DataPlant::TypeOf_CoilWaterSimpleHeating;

        state.dataWaterCoils->WaterCoil(CoilNum).UACoil = NumArray(1);
        state.dataWaterCoils->WaterCoil(CoilNum).UACoilVariable = state.dataWaterCoils->WaterCoil(CoilNum).UACoil;
        state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate = NumArray(2);
        state.dataWaterCoils->WaterCoil(CoilNum).WaterInletNodeNum = GetOnlySingleNode(state,
                                                                                       AlphArray(3),
                                                                                       ErrorsFound,
                                                                                       CurrentModuleObject,
                                                                                       AlphArray(1),
                                                                                       DataLoopNode::NodeFluidType::Water,
                                                                                       DataLoopNode::NodeConnectionType::Inlet,
                                                                                       2,
                                                                                       ObjectIsNotParent);
        state.dataWaterCoils->WaterCoil(CoilNum).WaterOutletNodeNum = GetOnlySingleNode(state,
                                                                                        AlphArray(4),
                                                                                        ErrorsFound,
                                                                                        CurrentModuleObject,
                                                                                        AlphArray(1),
                                                                                        DataLoopNode::NodeFluidType::Water,
                                                                                        DataLoopNode::NodeConnectionType::Outlet,
                                                                                        2,
                                                                                        ObjectIsNotParent);
        state.dataWaterCoils->WaterCoil(CoilNum).AirInletNodeNum = GetOnlySingleNode(state,
                                                                                     AlphArray(5),
                                                                                     ErrorsFound,
                                                                                     CurrentModuleObject,
                                                                                     AlphArray(1),
                                                                                     DataLoopNode::NodeFluidType::Air,
                                                                                     DataLoopNode::NodeConnectionType::Inlet,
                                                                                     1,
                                                                                     ObjectIsNotParent);
        state.dataWaterCoils->WaterCoil(CoilNum).AirOutletNodeNum = GetOnlySingleNode(state,
                                                                                      AlphArray(6),
                                                                                      ErrorsFound,
                                                                                      CurrentModuleObject,
                                                                                      AlphArray(1),
                                                                                      DataLoopNode::NodeFluidType::Air,
                                                                                      DataLoopNode::NodeConnectionType::Outlet,
                                                                                      1,
                                                                                      ObjectIsNotParent);

        {
            auto const SELECT_CASE_var(AlphArray(7));
            if (SELECT_CASE_var == "UFACTORTIMESAREAANDDESIGNWATERFLOWRATE") {
                state.dataWaterCoils->WaterCoil(CoilNum).CoilPerfInpMeth = state.dataWaterCoils->UAandFlow;

            } else if (SELECT_CASE_var == "NOMINALCAPACITY") {
                state.dataWaterCoils->WaterCoil(CoilNum).CoilPerfInpMeth = state.dataWaterCoils->NomCap;

            } else {
                // will be caught by input processor
                state.dataWaterCoils->WaterCoil(CoilNum).CoilPerfInpMeth = state.dataWaterCoils->UAandFlow;
            }
        }

        state.dataWaterCoils->WaterCoil(CoilNum).DesTotWaterCoilLoad = NumArray(3);

        if (state.dataWaterCoils->WaterCoil(CoilNum).UACoil == AutoSize &&
            state.dataWaterCoils->WaterCoil(CoilNum).CoilPerfInpMeth == state.dataWaterCoils->UAandFlow)
            state.dataWaterCoils->WaterCoil(CoilNum).RequestingAutoSize = true;
        if (state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate == AutoSize)
            state.dataWaterCoils->WaterCoil(CoilNum).RequestingAutoSize = true;
        if (state.dataWaterCoils->WaterCoil(CoilNum).DesTotWaterCoilLoad == AutoSize &&
            state.dataWaterCoils->WaterCoil(CoilNum).CoilPerfInpMeth == state.dataWaterCoils->NomCap)
            state.dataWaterCoils->WaterCoil(CoilNum).RequestingAutoSize = true;

        state.dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp = NumArray(4);
        state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirTemp = NumArray(5);
        state.dataWaterCoils->WaterCoil(CoilNum).DesOutletWaterTemp = NumArray(6);
        state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirTemp = NumArray(7);
        state.dataWaterCoils->WaterCoil(CoilNum).RatioAirSideToWaterSideConvect = NumArray(8);
        if (!lNumericBlanks(9)) {
            state.dataWaterCoils->WaterCoil(CoilNum).DesignWaterDeltaTemp = NumArray(9);
            state.dataWaterCoils->WaterCoil(CoilNum).UseDesignWaterDeltaTemp = true;
        } else {
            state.dataWaterCoils->WaterCoil(CoilNum).UseDesignWaterDeltaTemp = false;
        }
        if (state.dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp <= state.dataWaterCoils->WaterCoil(CoilNum).DesOutletWaterTemp) {
            ShowSevereError(state, "For " + CurrentModuleObject + ", " + AlphArray(1));
            ShowContinueError(state, "  the " + cNumericFields(4) + " must be greater than the " + cNumericFields(6) + '.');
            ErrorsFound = true;
        }
        if (state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirTemp >= state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirTemp) {
            ShowSevereError(state, "For " + CurrentModuleObject + ", " + AlphArray(1));
            ShowContinueError(state, "  the " + cNumericFields(5) + " must be less than the " + cNumericFields(7) + '.');
            ErrorsFound = true;
        }
        if (state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirTemp >= state.dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp) {
            ShowSevereError(state, "For " + CurrentModuleObject + ", " + AlphArray(1));
            ShowContinueError(state, "  the " + cNumericFields(5) + " must be less than the " + cNumericFields(4) + '.');
            ErrorsFound = true;
        }

        TestCompSet(state, CurrentModuleObject, AlphArray(1), AlphArray(3), AlphArray(4), "Water Nodes");
        TestCompSet(state, CurrentModuleObject, AlphArray(1), AlphArray(5), AlphArray(6), "Air Nodes");

        // Setup the Simple Heating Coil reporting variables
        // CurrentModuleObject = "Coil:Heating:Water"
        SetupOutputVariable(state,
                            "Heating Coil Heating Energy",
                            OutputProcessor::Unit::J,
                            state.dataWaterCoils->WaterCoil(CoilNum).TotWaterHeatingCoilEnergy,
                            "System",
                            "Sum",
                            state.dataWaterCoils->WaterCoil(CoilNum).Name,
                            _,
                            "ENERGYTRANSFER",
                            "HEATINGCOILS",
                            _,
                            "System");
        SetupOutputVariable(state,
                            "Heating Coil Source Side Heat Transfer Energy",
                            OutputProcessor::Unit::J,
                            state.dataWaterCoils->WaterCoil(CoilNum).TotWaterHeatingCoilEnergy,
                            "System",
                            "Sum",
                            state.dataWaterCoils->WaterCoil(CoilNum).Name,
                            _,
                            "PLANTLOOPHEATINGDEMAND",
                            "HEATINGCOILS",
                            _,
                            "System");
        SetupOutputVariable(state,
                            "Heating Coil Heating Rate",
                            OutputProcessor::Unit::W,
                            state.dataWaterCoils->WaterCoil(CoilNum).TotWaterHeatingCoilRate,
                            "System",
                            "Average",
                            state.dataWaterCoils->WaterCoil(CoilNum).Name);
        SetupOutputVariable(state,
                            "Heating Coil U Factor Times Area Value",
                            OutputProcessor::Unit::W_K,
                            state.dataWaterCoils->WaterCoil(CoilNum).UACoilVariable,
                            "System",
                            "Average",
                            state.dataWaterCoils->WaterCoil(CoilNum).Name);
    }

    CurrentModuleObject = "Coil:Cooling:Water:DetailedGeometry";
    // Get the data for detailed cooling coils.
    for (FlatFinNum = 1; FlatFinNum <= NumFlatFin; ++FlatFinNum) {

        CoilNum = NumSimpHeat + FlatFinNum;

        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 CurrentModuleObject,
                                                                 FlatFinNum,
                                                                 AlphArray,
                                                                 NumAlphas,
                                                                 NumArray,
                                                                 NumNums,
                                                                 IOStat,
                                                                 lNumericBlanks,
                                                                 lAlphaBlanks,
                                                                 cAlphaFields,
                                                                 cNumericFields);

        state.dataWaterCoils->WaterCoilNumericFields(CoilNum).FieldNames.allocate(MaxNums);
        state.dataWaterCoils->WaterCoilNumericFields(CoilNum).FieldNames = "";
        state.dataWaterCoils->WaterCoilNumericFields(CoilNum).FieldNames = cNumericFields;
        UtilityRoutines::IsNameEmpty(state, AlphArray(1), cCurrentModuleObject, ErrorsFound);

        // ErrorsFound will be set to True if problem was found, left untouched otherwise
        VerifyUniqueCoilName(state, CurrentModuleObject, AlphArray(1), ErrorsFound, CurrentModuleObject + " Name");

        state.dataWaterCoils->WaterCoil(CoilNum).Name = AlphArray(1);
        state.dataWaterCoils->WaterCoil(CoilNum).Schedule = AlphArray(2);
        if (lAlphaBlanks(2)) {
            state.dataWaterCoils->WaterCoil(CoilNum).SchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
        } else {
            state.dataWaterCoils->WaterCoil(CoilNum).SchedPtr = GetScheduleIndex(state, AlphArray(2));
            if (state.dataWaterCoils->WaterCoil(CoilNum).SchedPtr == 0) {
                ShowSevereError(state,
                                CurrentModuleObject + ": invalid " + cAlphaFields(2) + " entered =" + AlphArray(2) + " for " + cAlphaFields(1) + '=' +
                                    AlphArray(1));
                ErrorsFound = true;
            }
        }

        state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilTypeA = "Cooling";
        state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType = DataPlant::TypeOf_CoilWaterCooling; // 'Cooling'
        state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilModelA = "DETAILED FLAT FIN";
        state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilModel = iCoilModel::CoolingDetailed; // 'DETAILED FLAT FIN'
        state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType = DataPlant::TypeOf_CoilWaterDetailedFlatCooling;

        state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate = NumArray(1);
        if (state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate == AutoSize)
            state.dataWaterCoils->WaterCoil(CoilNum).RequestingAutoSize = true;
        state.dataWaterCoils->WaterCoil(CoilNum).TubeOutsideSurfArea = NumArray(2);
        if (state.dataWaterCoils->WaterCoil(CoilNum).TubeOutsideSurfArea == AutoSize)
            state.dataWaterCoils->WaterCoil(CoilNum).RequestingAutoSize = true;
        state.dataWaterCoils->WaterCoil(CoilNum).TotTubeInsideArea = NumArray(3);
        if (state.dataWaterCoils->WaterCoil(CoilNum).TotTubeInsideArea == AutoSize)
            state.dataWaterCoils->WaterCoil(CoilNum).RequestingAutoSize = true;
        state.dataWaterCoils->WaterCoil(CoilNum).FinSurfArea = NumArray(4);
        if (state.dataWaterCoils->WaterCoil(CoilNum).FinSurfArea == AutoSize) state.dataWaterCoils->WaterCoil(CoilNum).RequestingAutoSize = true;
        state.dataWaterCoils->WaterCoil(CoilNum).MinAirFlowArea = NumArray(5);
        if (state.dataWaterCoils->WaterCoil(CoilNum).MinAirFlowArea == AutoSize) state.dataWaterCoils->WaterCoil(CoilNum).RequestingAutoSize = true;
        state.dataWaterCoils->WaterCoil(CoilNum).CoilDepth = NumArray(6);
        if (state.dataWaterCoils->WaterCoil(CoilNum).CoilDepth == AutoSize) state.dataWaterCoils->WaterCoil(CoilNum).RequestingAutoSize = true;
        state.dataWaterCoils->WaterCoil(CoilNum).FinDiam = NumArray(7);
        if (state.dataWaterCoils->WaterCoil(CoilNum).FinDiam == AutoSize) state.dataWaterCoils->WaterCoil(CoilNum).RequestingAutoSize = true;
        state.dataWaterCoils->WaterCoil(CoilNum).FinThickness = NumArray(8);
        if (state.dataWaterCoils->WaterCoil(CoilNum).FinThickness <= 0.0) {
            ShowSevereError(state,
                            CurrentModuleObject + ": " + cNumericFields(8) + " must be > 0.0, for " + cAlphaFields(1) + " = " +
                                state.dataWaterCoils->WaterCoil(CoilNum).Name);
            ErrorsFound = true;
        }
        state.dataWaterCoils->WaterCoil(CoilNum).TubeInsideDiam = NumArray(9);
        state.dataWaterCoils->WaterCoil(CoilNum).TubeOutsideDiam = NumArray(10);
        state.dataWaterCoils->WaterCoil(CoilNum).TubeThermConductivity = NumArray(11);
        if (state.dataWaterCoils->WaterCoil(CoilNum).TubeThermConductivity <= 0.0) {
            ShowSevereError(state,
                            CurrentModuleObject + ": " + cNumericFields(11) + " must be > 0.0, for " + cAlphaFields(1) + " = " +
                                state.dataWaterCoils->WaterCoil(CoilNum).Name);
            ErrorsFound = true;
        }
        state.dataWaterCoils->WaterCoil(CoilNum).FinThermConductivity = NumArray(12);
        if (state.dataWaterCoils->WaterCoil(CoilNum).FinThermConductivity <= 0.0) {
            ShowSevereError(state,
                            CurrentModuleObject + ": " + cNumericFields(12) + " must be > 0.0, for " + cAlphaFields(1) + " = " +
                                state.dataWaterCoils->WaterCoil(CoilNum).Name);
            ErrorsFound = true;
        }
        state.dataWaterCoils->WaterCoil(CoilNum).FinSpacing = NumArray(13);
        state.dataWaterCoils->WaterCoil(CoilNum).TubeDepthSpacing = NumArray(14);
        state.dataWaterCoils->WaterCoil(CoilNum).NumOfTubeRows = NumArray(15);
        state.dataWaterCoils->WaterCoil(CoilNum).NumOfTubesPerRow = NumArray(16);
        if (state.dataWaterCoils->WaterCoil(CoilNum).NumOfTubesPerRow == AutoSize) state.dataWaterCoils->WaterCoil(CoilNum).RequestingAutoSize = true;
        if (!lNumericBlanks(17)) {
            state.dataWaterCoils->WaterCoil(CoilNum).DesignWaterDeltaTemp = NumArray(17);
            state.dataWaterCoils->WaterCoil(CoilNum).UseDesignWaterDeltaTemp = true;
        } else {
            state.dataWaterCoils->WaterCoil(CoilNum).UseDesignWaterDeltaTemp = false;
        }
        state.dataWaterCoils->WaterCoil(CoilNum).WaterInletNodeNum = GetOnlySingleNode(state,
                                                                                       AlphArray(3),
                                                                                       ErrorsFound,
                                                                                       CurrentModuleObject,
                                                                                       AlphArray(1),
                                                                                       DataLoopNode::NodeFluidType::Water,
                                                                                       DataLoopNode::NodeConnectionType::Inlet,
                                                                                       2,
                                                                                       ObjectIsNotParent);
        state.dataWaterCoils->WaterCoil(CoilNum).WaterOutletNodeNum = GetOnlySingleNode(state,
                                                                                        AlphArray(4),
                                                                                        ErrorsFound,
                                                                                        CurrentModuleObject,
                                                                                        AlphArray(1),
                                                                                        DataLoopNode::NodeFluidType::Water,
                                                                                        DataLoopNode::NodeConnectionType::Outlet,
                                                                                        2,
                                                                                        ObjectIsNotParent);
        state.dataWaterCoils->WaterCoil(CoilNum).AirInletNodeNum = GetOnlySingleNode(state,
                                                                                     AlphArray(5),
                                                                                     ErrorsFound,
                                                                                     CurrentModuleObject,
                                                                                     AlphArray(1),
                                                                                     DataLoopNode::NodeFluidType::Air,
                                                                                     DataLoopNode::NodeConnectionType::Inlet,
                                                                                     1,
                                                                                     ObjectIsNotParent);
        state.dataWaterCoils->WaterCoil(CoilNum).AirOutletNodeNum = GetOnlySingleNode(state,
                                                                                      AlphArray(6),
                                                                                      ErrorsFound,
                                                                                      CurrentModuleObject,
                                                                                      AlphArray(1),
                                                                                      DataLoopNode::NodeFluidType::Air,
                                                                                      DataLoopNode::NodeConnectionType::Outlet,
                                                                                      1,
                                                                                      ObjectIsNotParent);

        // A7 ; \field Name of Water Storage Tank for Condensate Collection
        state.dataWaterCoils->WaterCoil(CoilNum).CondensateCollectName = AlphArray(7);
        if (lAlphaBlanks(7)) {
            state.dataWaterCoils->WaterCoil(CoilNum).CondensateCollectMode = state.dataWaterCoils->CondensateDiscarded;
        } else {
            state.dataWaterCoils->WaterCoil(CoilNum).CondensateCollectMode = state.dataWaterCoils->CondensateToTank;
            SetupTankSupplyComponent(state,
                                     state.dataWaterCoils->WaterCoil(CoilNum).Name,
                                     CurrentModuleObject,
                                     state.dataWaterCoils->WaterCoil(CoilNum).CondensateCollectName,
                                     ErrorsFound,
                                     state.dataWaterCoils->WaterCoil(CoilNum).CondensateTankID,
                                     state.dataWaterCoils->WaterCoil(CoilNum).CondensateTankSupplyARRID);
        }

        TestCompSet(state, CurrentModuleObject, AlphArray(1), AlphArray(3), AlphArray(4), "Water Nodes");
        TestCompSet(state, CurrentModuleObject, AlphArray(1), AlphArray(5), AlphArray(6), "Air Nodes");

        // Setup Report variables for the Detailed Flat Fin Cooling Coils
        // CurrentModuleObject = "Coil:Cooling:Water:DetailedGeometry"
        SetupOutputVariable(state,
                            "Cooling Coil Total Cooling Energy",
                            OutputProcessor::Unit::J,
                            state.dataWaterCoils->WaterCoil(CoilNum).TotWaterCoolingCoilEnergy,
                            "System",
                            "Sum",
                            state.dataWaterCoils->WaterCoil(CoilNum).Name,
                            _,
                            "ENERGYTRANSFER",
                            "COOLINGCOILS",
                            _,
                            "System");
        SetupOutputVariable(state,
                            "Cooling Coil Source Side Heat Transfer Energy",
                            OutputProcessor::Unit::J,
                            state.dataWaterCoils->WaterCoil(CoilNum).TotWaterCoolingCoilEnergy,
                            "System",
                            "Sum",
                            state.dataWaterCoils->WaterCoil(CoilNum).Name,
                            _,
                            "PLANTLOOPCOOLINGDEMAND",
                            "COOLINGCOILS",
                            _,
                            "System");
        SetupOutputVariable(state,
                            "Cooling Coil Sensible Cooling Energy",
                            OutputProcessor::Unit::J,
                            state.dataWaterCoils->WaterCoil(CoilNum).SenWaterCoolingCoilEnergy,
                            "System",
                            "Sum",
                            state.dataWaterCoils->WaterCoil(CoilNum).Name);
        SetupOutputVariable(state,
                            "Cooling Coil Total Cooling Rate",
                            OutputProcessor::Unit::W,
                            state.dataWaterCoils->WaterCoil(CoilNum).TotWaterCoolingCoilRate,
                            "System",
                            "Average",
                            state.dataWaterCoils->WaterCoil(CoilNum).Name);
        SetupOutputVariable(state,
                            "Cooling Coil Sensible Cooling Rate",
                            OutputProcessor::Unit::W,
                            state.dataWaterCoils->WaterCoil(CoilNum).SenWaterCoolingCoilRate,
                            "System",
                            "Average",
                            state.dataWaterCoils->WaterCoil(CoilNum).Name);

        if (state.dataWaterCoils->WaterCoil(CoilNum).CondensateCollectMode == state.dataWaterCoils->CondensateToTank) {

            SetupOutputVariable(state,
                                "Cooling Coil Condensate Volume Flow Rate",
                                OutputProcessor::Unit::m3_s,
                                state.dataWaterCoils->WaterCoil(CoilNum).CondensateVdot,
                                "System",
                                "Average",
                                state.dataWaterCoils->WaterCoil(CoilNum).Name);
            SetupOutputVariable(state,
                                "Cooling Coil Condensate Volume",
                                OutputProcessor::Unit::m3,
                                state.dataWaterCoils->WaterCoil(CoilNum).CondensateVol,
                                "System",
                                "Sum",
                                state.dataWaterCoils->WaterCoil(CoilNum).Name,
                                _,
                                "OnSiteWater",
                                "Condensate",
                                _,
                                "System");
        }
    }

    CurrentModuleObject = "Coil:Cooling:Water";
    // Get the data for Cooling coils.
    for (CoolingNum = 1; CoolingNum <= NumCooling; ++CoolingNum) {

        CoilNum = NumSimpHeat + NumFlatFin + CoolingNum;

        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 CurrentModuleObject,
                                                                 CoolingNum,
                                                                 AlphArray,
                                                                 NumAlphas,
                                                                 NumArray,
                                                                 NumNums,
                                                                 IOStat,
                                                                 lNumericBlanks,
                                                                 lAlphaBlanks,
                                                                 cAlphaFields,
                                                                 cNumericFields);

        state.dataWaterCoils->WaterCoilNumericFields(CoilNum).FieldNames.allocate(MaxNums);
        state.dataWaterCoils->WaterCoilNumericFields(CoilNum).FieldNames = "";
        state.dataWaterCoils->WaterCoilNumericFields(CoilNum).FieldNames = cNumericFields;
        UtilityRoutines::IsNameEmpty(state, AlphArray(1), cCurrentModuleObject, ErrorsFound);

        // ErrorsFound will be set to True if problem was found, left untouched otherwise
        VerifyUniqueCoilName(state, CurrentModuleObject, AlphArray(1), ErrorsFound, CurrentModuleObject + " Name");

        state.dataWaterCoils->WaterCoil(CoilNum).Name = AlphArray(1);
        state.dataWaterCoils->WaterCoil(CoilNum).Schedule = AlphArray(2);
        if (lAlphaBlanks(2)) {
            state.dataWaterCoils->WaterCoil(CoilNum).SchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
        } else {
            state.dataWaterCoils->WaterCoil(CoilNum).SchedPtr = GetScheduleIndex(state, AlphArray(2));
            if (state.dataWaterCoils->WaterCoil(CoilNum).SchedPtr == 0) {
                ShowSevereError(state,
                                CurrentModuleObject + ": invalid " + cAlphaFields(2) + " entered =" + AlphArray(2) + " for " + cAlphaFields(1) + '=' +
                                    AlphArray(1));
                ErrorsFound = true;
            }
        }

        state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilTypeA = "Cooling";
        state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType = DataPlant::TypeOf_CoilWaterCooling; // 'Cooling'
        state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilModelA = "Cooling";
        state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilModel = iCoilModel::CoolingSimple; // 'Cooling'
        state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType = DataPlant::TypeOf_CoilWaterCooling;

        state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate = NumArray(1); // Liquid mass flow rate at Design  kg/s
        if (state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate == AutoSize)
            state.dataWaterCoils->WaterCoil(CoilNum).RequestingAutoSize = true;
        state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate = NumArray(2); // Dry air mass flow rate at Design (kg/s)
        if (state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate == AutoSize)
            state.dataWaterCoils->WaterCoil(CoilNum).RequestingAutoSize = true;
        state.dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp = NumArray(3); // Entering water temperature at Design C
        if (state.dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp == AutoSize)
            state.dataWaterCoils->WaterCoil(CoilNum).RequestingAutoSize = true;
        state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirTemp = NumArray(4); // Entering air dry bulb temperature at Design(C)
        if (state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirTemp == AutoSize) state.dataWaterCoils->WaterCoil(CoilNum).RequestingAutoSize = true;
        state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirTemp = NumArray(5); // Leaving air dry bulb temperature at Design(C)
        if (state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirTemp == AutoSize) state.dataWaterCoils->WaterCoil(CoilNum).RequestingAutoSize = true;
        state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirHumRat = NumArray(6); // Entering air humidity ratio  at Design
        if (state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirHumRat == AutoSize)
            state.dataWaterCoils->WaterCoil(CoilNum).RequestingAutoSize = true;
        state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirHumRat = NumArray(7); // Leaving air humidity ratio  at Design
        if (state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirHumRat == AutoSize)
            state.dataWaterCoils->WaterCoil(CoilNum).RequestingAutoSize = true;
        if (!lNumericBlanks(8)) {
            state.dataWaterCoils->WaterCoil(CoilNum).DesignWaterDeltaTemp = NumArray(8);
            state.dataWaterCoils->WaterCoil(CoilNum).UseDesignWaterDeltaTemp = true;
        } else {
            state.dataWaterCoils->WaterCoil(CoilNum).UseDesignWaterDeltaTemp = false;
        }

        state.dataWaterCoils->WaterCoil(CoilNum).WaterInletNodeNum = GetOnlySingleNode(state,
                                                                                       AlphArray(3),
                                                                                       ErrorsFound,
                                                                                       CurrentModuleObject,
                                                                                       AlphArray(1),
                                                                                       DataLoopNode::NodeFluidType::Water,
                                                                                       DataLoopNode::NodeConnectionType::Inlet,
                                                                                       2,
                                                                                       ObjectIsNotParent);
        state.dataWaterCoils->WaterCoil(CoilNum).WaterOutletNodeNum = GetOnlySingleNode(state,
                                                                                        AlphArray(4),
                                                                                        ErrorsFound,
                                                                                        CurrentModuleObject,
                                                                                        AlphArray(1),
                                                                                        DataLoopNode::NodeFluidType::Water,
                                                                                        DataLoopNode::NodeConnectionType::Outlet,
                                                                                        2,
                                                                                        ObjectIsNotParent);
        state.dataWaterCoils->WaterCoil(CoilNum).AirInletNodeNum = GetOnlySingleNode(state,
                                                                                     AlphArray(5),
                                                                                     ErrorsFound,
                                                                                     CurrentModuleObject,
                                                                                     AlphArray(1),
                                                                                     DataLoopNode::NodeFluidType::Air,
                                                                                     DataLoopNode::NodeConnectionType::Inlet,
                                                                                     1,
                                                                                     ObjectIsNotParent);
        state.dataWaterCoils->WaterCoil(CoilNum).AirOutletNodeNum = GetOnlySingleNode(state,
                                                                                      AlphArray(6),
                                                                                      ErrorsFound,
                                                                                      CurrentModuleObject,
                                                                                      AlphArray(1),
                                                                                      DataLoopNode::NodeFluidType::Air,
                                                                                      DataLoopNode::NodeConnectionType::Outlet,
                                                                                      1,
                                                                                      ObjectIsNotParent);

        {
            auto const SELECT_CASE_var(AlphArray(7));
            // The default is SimpleAnalysis = 2.  and DetailedAnalysis   =1
            if (SELECT_CASE_var == "SIMPLEANALYSIS") {
                state.dataWaterCoils->WaterCoil(CoilNum).CoolingCoilAnalysisMode = state.dataWaterCoils->SimpleAnalysis;

            } else if (SELECT_CASE_var == "DETAILEDANALYSIS") {
                state.dataWaterCoils->WaterCoil(CoilNum).CoolingCoilAnalysisMode = state.dataWaterCoils->DetailedAnalysis;

            } else {
                state.dataWaterCoils->WaterCoil(CoilNum).CoolingCoilAnalysisMode = state.dataWaterCoils->SimpleAnalysis;
            }
        }

        {
            auto const SELECT_CASE_var(AlphArray(8));
            // The default is CrossFlow = 2.  and CounterFlow=1
            if (SELECT_CASE_var == "CROSSFLOW") {
                state.dataWaterCoils->WaterCoil(CoilNum).HeatExchType = state.dataWaterCoils->CrossFlow;

            } else if (SELECT_CASE_var == "COUNTERFLOW") {
                state.dataWaterCoils->WaterCoil(CoilNum).HeatExchType = state.dataWaterCoils->CounterFlow;

            } else {
                state.dataWaterCoils->WaterCoil(CoilNum).HeatExchType = state.dataWaterCoils->CrossFlow;
            }
        }

        // A9; \field Name of Water Storage Tank for Condensate Collection
        state.dataWaterCoils->WaterCoil(CoilNum).CondensateCollectName = AlphArray(9);
        if (lAlphaBlanks(9)) {
            state.dataWaterCoils->WaterCoil(CoilNum).CondensateCollectMode = state.dataWaterCoils->CondensateDiscarded;
        } else {
            state.dataWaterCoils->WaterCoil(CoilNum).CondensateCollectMode = state.dataWaterCoils->CondensateToTank;
            SetupTankSupplyComponent(state,
                                     state.dataWaterCoils->WaterCoil(CoilNum).Name,
                                     CurrentModuleObject,
                                     state.dataWaterCoils->WaterCoil(CoilNum).CondensateCollectName,
                                     ErrorsFound,
                                     state.dataWaterCoils->WaterCoil(CoilNum).CondensateTankID,
                                     state.dataWaterCoils->WaterCoil(CoilNum).CondensateTankSupplyARRID);
        }

        TestCompSet(state, CurrentModuleObject, AlphArray(1), AlphArray(3), AlphArray(4), "Water Nodes");
        TestCompSet(state, CurrentModuleObject, AlphArray(1), AlphArray(5), AlphArray(6), "Air Nodes");

        // Setup Report variables for the Design input Cooling Coils
        // CurrentModuleObject = "Coil:Cooling:Water"
        SetupOutputVariable(state,
                            "Cooling Coil Total Cooling Energy",
                            OutputProcessor::Unit::J,
                            state.dataWaterCoils->WaterCoil(CoilNum).TotWaterCoolingCoilEnergy,
                            "System",
                            "Sum",
                            state.dataWaterCoils->WaterCoil(CoilNum).Name,
                            _,
                            "ENERGYTRANSFER",
                            "COOLINGCOILS",
                            _,
                            "System");
        SetupOutputVariable(state,
                            "Cooling Coil Source Side Heat Transfer Energy",
                            OutputProcessor::Unit::J,
                            state.dataWaterCoils->WaterCoil(CoilNum).TotWaterCoolingCoilEnergy,
                            "System",
                            "Sum",
                            state.dataWaterCoils->WaterCoil(CoilNum).Name,
                            _,
                            "PLANTLOOPCOOLINGDEMAND",
                            "COOLINGCOILS",
                            _,
                            "System");
        SetupOutputVariable(state,
                            "Cooling Coil Sensible Cooling Energy",
                            OutputProcessor::Unit::J,
                            state.dataWaterCoils->WaterCoil(CoilNum).SenWaterCoolingCoilEnergy,
                            "System",
                            "Sum",
                            state.dataWaterCoils->WaterCoil(CoilNum).Name);
        SetupOutputVariable(state,
                            "Cooling Coil Total Cooling Rate",
                            OutputProcessor::Unit::W,
                            state.dataWaterCoils->WaterCoil(CoilNum).TotWaterCoolingCoilRate,
                            "System",
                            "Average",
                            state.dataWaterCoils->WaterCoil(CoilNum).Name);
        SetupOutputVariable(state,
                            "Cooling Coil Sensible Cooling Rate",
                            OutputProcessor::Unit::W,
                            state.dataWaterCoils->WaterCoil(CoilNum).SenWaterCoolingCoilRate,
                            "System",
                            "Average",
                            state.dataWaterCoils->WaterCoil(CoilNum).Name);
        SetupOutputVariable(state,
                            "Cooling Coil Wetted Area Fraction",
                            OutputProcessor::Unit::None,
                            state.dataWaterCoils->WaterCoil(CoilNum).SurfAreaWetFraction,
                            "System",
                            "Average",
                            state.dataWaterCoils->WaterCoil(CoilNum).Name);

        if (state.dataWaterCoils->WaterCoil(CoilNum).CondensateCollectMode == state.dataWaterCoils->CondensateToTank) {

            SetupOutputVariable(state,
                                "Cooling Coil Condensate Volume Flow Rate",
                                OutputProcessor::Unit::m3_s,
                                state.dataWaterCoils->WaterCoil(CoilNum).CondensateVdot,
                                "System",
                                "Average",
                                state.dataWaterCoils->WaterCoil(CoilNum).Name);
            SetupOutputVariable(state,
                                "Cooling Coil Condensate Volume",
                                OutputProcessor::Unit::m3,
                                state.dataWaterCoils->WaterCoil(CoilNum).CondensateVol,
                                "System",
                                "Sum",
                                state.dataWaterCoils->WaterCoil(CoilNum).Name,
                                _,
                                "OnSiteWater",
                                "Condensate",
                                _,
                                "System");
        }
    }

    if (ErrorsFound) {
        ShowFatalError(state, std::string{RoutineName} + "Errors found in getting input.");
    }

    AlphArray.deallocate();
    cAlphaFields.deallocate();
    cNumericFields.deallocate();
    NumArray.deallocate();
    lAlphaBlanks.deallocate();
    lNumericBlanks.deallocate();
}

void InitWaterCoil(EnergyPlusData &state, int const CoilNum, bool const FirstHVACIteration)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard J. Liesen
    //       DATE WRITTEN   February 1998
    //       MODIFIED       April 2004: Rahul Chillar
    //                      November 2013: XP, Tianzhen Hong to handle fouling coils
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine is for initializations of the WaterCoil Components.

    // METHODOLOGY EMPLOYED:
    // Uses the status flags to trigger initializations.

    // REFERENCES:

    // Using/Aliasing
    using General::Iterate;

    using General::SafeDivide;
    using General::SolveRoot;
    using namespace OutputReportPredefined;
    using PlantUtilities::InitComponentNodes;
    using PlantUtilities::RegisterPlantCompDesignFlow;
    using PlantUtilities::ScanPlantLoopsForObject;
    using namespace FaultsManager;
    using HVACControllers::GetControllerNameAndIndex;
    using SimAirServingZones::CheckWaterCoilIsOnAirLoop;

    // SUBROUTINE PARAMETER DEFINITIONS:
    constexpr Real64 SmallNo(1.e-9); // SmallNo number in place of zero
    constexpr int itmax(10);
    constexpr int MaxIte(500); // Maximum number of iterations
    static constexpr std::string_view RoutineName("InitWaterCoil");

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int tempCoilNum;                   // loop variable
    Real64 DesInletAirEnth;            // Entering air enthalpy at rating (J/kg)
    Real64 DesOutletAirEnth;           // Leaving air enthalpy at rating(J/kg)
    Real64 DesAirApparatusDewPtEnth;   // Air enthalpy at apparatus dew point at rating(J/kg)
    Real64 DesSatEnthAtWaterInTemp;    // Saturated enthalpy at entering liquid temp(J/kg)
    Real64 DesHumRatAtWaterInTemp;     // Enthalpy at water inlet temp and entering air HumRat (J/kg)
    Real64 CapacitanceAir;             // Air-side capacity rate(W/C)
    Real64 DesAirTempApparatusDewPt;   // Temperature apparatus dew point at design capacity
    Real64 DesAirHumRatApparatusDewPt; // Humidity Ratio at apparatus dew point at design capacity
    Real64 DesBypassFactor;            // ByPass Factor at design condition
    Real64 SlopeTempVsHumRatio;        // Ratio temperature difference to humidity difference
    // between entering and leaving air states
    Real64 TempApparatusDewPtEstimate; // Estimate of TAdp from SlopeTempVsHumRatio
    Real64 Y1;                         // Previous values of dependent variable in ITERATE
    Real64 X1;                         // Previous values of independent variable in ITERATE
    Real64 error;                      // Deviation of dependent variable in iteration
    int iter;                          // Iteration counter
    int icvg;                          // Iteration convergence flag
    Real64 ResultX;                    // Output variable from ITERATE function.
    int Ipass;                         // loop index for App_Dewpoint_Loop
    int AirInletNode;
    int WaterInletNode;
    int WaterOutletNode;
    Real64 FinDiamVar;
    Real64 TubeToFinDiamRatio;
    Real64 CpAirStd; // specific heat of air at std conditions
    int SolFla;      // Flag of solver
    Real64 UA0;      // lower bound for UA
    Real64 UA1;      // upper bound for UA
    Real64 UA;
    Real64 DesUACoilExternalEnth; // enthalpy based UAExternal for wet coil surface {kg/s}
    Real64 LogMeanEnthDiff;       // long mean enthalpy difference {J/kg}
    Real64 LogMeanTempDiff;       // long mean temperature difference {C}
    Real64 DesOutletWaterTemp;
    Real64 DesSatEnthAtWaterOutTemp;
    Real64 DesEnthAtWaterOutTempAirInHumRat;
    Real64 DesEnthWaterOut;
    Real64 Cp;  // local fluid specific heat
    Real64 rho; // local fluid density
    bool errFlag;
    Real64 EnthCorrFrac(0.0); // enthalpy correction factor
    Real64 TempCorrFrac(0.0); // temperature correction factor

    auto &Node(state.dataLoopNodes->Node);

    if (state.dataWaterCoils->InitWaterCoilOneTimeFlag) {
        // initialize the environment and sizing flags
        state.dataWaterCoils->MyEnvrnFlag.allocate(state.dataWaterCoils->NumWaterCoils);
        state.dataWaterCoils->MySizeFlag.allocate(state.dataWaterCoils->NumWaterCoils);
        state.dataWaterCoils->CoilWarningOnceFlag.allocate(state.dataWaterCoils->NumWaterCoils);
        state.dataWaterCoils->DesCpAir.allocate(state.dataWaterCoils->NumWaterCoils);
        state.dataWaterCoils->MyUAAndFlowCalcFlag.allocate(state.dataWaterCoils->NumWaterCoils);
        state.dataWaterCoils->MyCoilDesignFlag.allocate(state.dataWaterCoils->NumWaterCoils);
        state.dataWaterCoils->MyCoilReportFlag.allocate(state.dataWaterCoils->NumWaterCoils);
        state.dataWaterCoils->DesUARangeCheck.allocate(state.dataWaterCoils->NumWaterCoils);
        state.dataWaterCoils->PlantLoopScanFlag.allocate(state.dataWaterCoils->NumWaterCoils);

        state.dataWaterCoils->DesCpAir = 0.0;
        state.dataWaterCoils->DesUARangeCheck = 0.0;
        state.dataWaterCoils->MyEnvrnFlag = true;
        state.dataWaterCoils->MySizeFlag = true;
        state.dataWaterCoils->CoilWarningOnceFlag = true;
        state.dataWaterCoils->MyUAAndFlowCalcFlag = true;
        state.dataWaterCoils->MyCoilDesignFlag = true;
        state.dataWaterCoils->MyCoilReportFlag = true;
        state.dataWaterCoils->InitWaterCoilOneTimeFlag = false;
        state.dataWaterCoils->PlantLoopScanFlag = true;

        for (tempCoilNum = 1; tempCoilNum <= state.dataWaterCoils->NumWaterCoils; ++tempCoilNum) {
            GetControllerNameAndIndex(state,
                                      state.dataWaterCoils->WaterCoil(tempCoilNum).WaterInletNodeNum,
                                      state.dataWaterCoils->WaterCoil(tempCoilNum).ControllerName,
                                      state.dataWaterCoils->WaterCoil(tempCoilNum).ControllerIndex,
                                      errFlag);
        }
    }

    if (state.dataWaterCoils->WaterCoilControllerCheckOneTimeFlag && (state.dataHVACGlobal->GetAirPathDataDone)) {
        bool ErrorsFound = false;
        bool WaterCoilOnAirLoop = true;
        for (tempCoilNum = 1; tempCoilNum <= state.dataWaterCoils->NumWaterCoils; ++tempCoilNum) {
            if (state.dataWaterCoils->WaterCoil(tempCoilNum).ControllerIndex > 0) {
                int CoilTypeNum(0);
                std::string CompType;
                std::string CompName = state.dataWaterCoils->WaterCoil(tempCoilNum).Name;
                if (state.dataWaterCoils->WaterCoil(tempCoilNum).WaterCoilType == DataPlant::TypeOf_CoilWaterCooling) {
                    CoilTypeNum = SimAirServingZones::WaterCoil_Cooling;
                    CompType = cAllCoilTypes(DataHVACGlobals::Coil_CoolingWater);
                } else if (state.dataWaterCoils->WaterCoil(tempCoilNum).WaterCoilType == DataPlant::TypeOf_CoilWaterDetailedFlatCooling) {
                    CoilTypeNum = SimAirServingZones::WaterCoil_DetailedCool;
                    CompType = cAllCoilTypes(DataHVACGlobals::Coil_CoolingWaterDetailed);
                } else if (state.dataWaterCoils->WaterCoil(tempCoilNum).WaterCoilType == DataPlant::TypeOf_CoilWaterSimpleHeating) {
                    CoilTypeNum = SimAirServingZones::WaterCoil_SimpleHeat;
                    CompType = cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater);
                }
                WaterCoilOnAirLoop = true;
                CheckWaterCoilIsOnAirLoop(state, CoilTypeNum, CompType, CompName, WaterCoilOnAirLoop);
                if (!WaterCoilOnAirLoop) {
                    ShowContinueError(state,
                                      "Controller:WaterCoil = " + state.dataWaterCoils->WaterCoil(tempCoilNum).ControllerName +
                                          ". Invalid water controller entry.");
                    ErrorsFound = true;
                }
            }
        }
        state.dataWaterCoils->WaterCoilControllerCheckOneTimeFlag = false;
        if (ErrorsFound) {
            ShowFatalError(state, "Program terminated for previous condition.");
        }
    }

    if (state.dataWaterCoils->PlantLoopScanFlag(CoilNum) && allocated(state.dataPlnt->PlantLoop)) {
        errFlag = false;
        ScanPlantLoopsForObject(state,
                                state.dataWaterCoils->WaterCoil(CoilNum).Name,
                                state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType,
                                state.dataWaterCoils->WaterCoil(CoilNum).WaterLoopNum,
                                state.dataWaterCoils->WaterCoil(CoilNum).WaterLoopSide,
                                state.dataWaterCoils->WaterCoil(CoilNum).WaterLoopBranchNum,
                                state.dataWaterCoils->WaterCoil(CoilNum).WaterLoopCompNum,
                                errFlag,
                                _,
                                _,
                                _,
                                _,
                                _);
        if (errFlag) {
            ShowFatalError(state, "InitWaterCoil: Program terminated for previous conditions.");
        }
        state.dataWaterCoils->PlantLoopScanFlag(CoilNum) = false;
    }
    if (!state.dataGlobal->SysSizingCalc && state.dataWaterCoils->MySizeFlag(CoilNum)) {
        // for each coil, do the sizing once.
        SizeWaterCoil(state, CoilNum);

        state.dataWaterCoils->MySizeFlag(CoilNum) = false;
    }

    // Do the Begin Environment initializations
    if (state.dataGlobal->BeginEnvrnFlag && state.dataWaterCoils->MyEnvrnFlag(CoilNum)) {
        rho = GetDensityGlycol(state,
                               state.dataPlnt->PlantLoop(state.dataWaterCoils->WaterCoil(CoilNum).WaterLoopNum).FluidName,
                               DataGlobalConstants::InitConvTemp,
                               state.dataPlnt->PlantLoop(state.dataWaterCoils->WaterCoil(CoilNum).WaterLoopNum).FluidIndex,
                               RoutineName);
        // Initialize all report variables to a known state at beginning of simulation
        state.dataWaterCoils->WaterCoil(CoilNum).TotWaterHeatingCoilEnergy = 0.0;
        state.dataWaterCoils->WaterCoil(CoilNum).TotWaterCoolingCoilEnergy = 0.0;
        state.dataWaterCoils->WaterCoil(CoilNum).SenWaterCoolingCoilEnergy = 0.0;
        state.dataWaterCoils->WaterCoil(CoilNum).TotWaterHeatingCoilRate = 0.0;
        state.dataWaterCoils->WaterCoil(CoilNum).TotWaterCoolingCoilRate = 0.0;
        state.dataWaterCoils->WaterCoil(CoilNum).SenWaterCoolingCoilRate = 0.0;

        // The rest of the one time initializations
        AirInletNode = state.dataWaterCoils->WaterCoil(CoilNum).AirInletNodeNum;
        WaterInletNode = state.dataWaterCoils->WaterCoil(CoilNum).WaterInletNodeNum;
        WaterOutletNode = state.dataWaterCoils->WaterCoil(CoilNum).WaterOutletNodeNum;

        state.dataWaterCoils->DesCpAir(CoilNum) = PsyCpAirFnW(0.0);
        state.dataWaterCoils->DesUARangeCheck(CoilNum) = (-1568.6 * state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirHumRat + 20.157);

        if ((state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType == DataPlant::TypeOf_CoilWaterCooling) ||
            (state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType == DataPlant::TypeOf_CoilWaterDetailedFlatCooling)) { // 'Cooling'
            Node(WaterInletNode).Temp = 5.0;

            Cp = GetSpecificHeatGlycol(state,
                                       state.dataPlnt->PlantLoop(state.dataWaterCoils->WaterCoil(CoilNum).WaterLoopNum).FluidName,
                                       Node(WaterInletNode).Temp,
                                       state.dataPlnt->PlantLoop(state.dataWaterCoils->WaterCoil(CoilNum).WaterLoopNum).FluidIndex,
                                       RoutineName);

            Node(WaterInletNode).Enthalpy = Cp * Node(WaterInletNode).Temp;
            Node(WaterInletNode).Quality = 0.0;
            Node(WaterInletNode).Press = 0.0;
            Node(WaterInletNode).HumRat = 0.0;
        }

        if (state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType == DataPlant::TypeOf_CoilWaterSimpleHeating) { // 'Heating'
            Node(WaterInletNode).Temp = 60.0;

            Cp = GetSpecificHeatGlycol(state,
                                       state.dataPlnt->PlantLoop(state.dataWaterCoils->WaterCoil(CoilNum).WaterLoopNum).FluidName,
                                       Node(WaterInletNode).Temp,
                                       state.dataPlnt->PlantLoop(state.dataWaterCoils->WaterCoil(CoilNum).WaterLoopNum).FluidIndex,
                                       RoutineName);

            Node(WaterInletNode).Enthalpy = Cp * Node(WaterInletNode).Temp;
            Node(WaterInletNode).Quality = 0.0;
            Node(WaterInletNode).Press = 0.0;
            Node(WaterInletNode).HumRat = 0.0;
            state.dataWaterCoils->MyUAAndFlowCalcFlag(CoilNum) = false;
            // fill values for variable UA
            CpAirStd = PsyCpAirFnW(0.0);
            state.dataWaterCoils->WaterCoil(CoilNum).DesAirMassFlowRate =
                state.dataEnvrn->StdRhoAir * state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate;
            state.dataWaterCoils->WaterCoil(CoilNum).LiquidSideNominalConvect =
                state.dataWaterCoils->WaterCoil(CoilNum).UACoil * (state.dataWaterCoils->WaterCoil(CoilNum).RatioAirSideToWaterSideConvect + 1) /
                state.dataWaterCoils->WaterCoil(CoilNum).RatioAirSideToWaterSideConvect;
            state.dataWaterCoils->WaterCoil(CoilNum).AirSideNominalConvect = state.dataWaterCoils->WaterCoil(CoilNum).RatioAirSideToWaterSideConvect *
                                                                             state.dataWaterCoils->WaterCoil(CoilNum).LiquidSideNominalConvect;
        } else {
            state.dataWaterCoils->MyUAAndFlowCalcFlag(CoilNum) = false;
        }

        state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterMassFlowRate = rho * state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate;

        InitComponentNodes(state,
                           0.0,
                           state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterMassFlowRate,
                           state.dataWaterCoils->WaterCoil(CoilNum).WaterInletNodeNum,
                           state.dataWaterCoils->WaterCoil(CoilNum).WaterOutletNodeNum,
                           state.dataWaterCoils->WaterCoil(CoilNum).WaterLoopNum,
                           state.dataWaterCoils->WaterCoil(CoilNum).WaterLoopSide,
                           state.dataWaterCoils->WaterCoil(CoilNum).WaterLoopBranchNum,
                           state.dataWaterCoils->WaterCoil(CoilNum).WaterLoopCompNum);

        // effective fin diameter for detailed flat fin coil
        if (state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilModel == iCoilModel::CoolingDetailed) { // 'DETAILED FLAT FIN'
            state.dataWaterCoils->WaterCoil(CoilNum).EffectiveFinDiam =
                std::sqrt(4.0 * state.dataWaterCoils->WaterCoil(CoilNum).FinDiam * state.dataWaterCoils->WaterCoil(CoilNum).CoilDepth /
                          (DataGlobalConstants::Pi * state.dataWaterCoils->WaterCoil(CoilNum).NumOfTubeRows *
                           state.dataWaterCoils->WaterCoil(CoilNum).NumOfTubesPerRow));

            //   calculate fixed geometric parameters of the coil:
            //   Total Area
            state.dataWaterCoils->WaterCoil(CoilNum).TotCoilOutsideSurfArea =
                state.dataWaterCoils->WaterCoil(CoilNum).TubeOutsideSurfArea + state.dataWaterCoils->WaterCoil(CoilNum).FinSurfArea;
            //   Effective Tube Inside Diameter - the model assumes that the coil
            //   can be simulated as a tube with an equivalent hydraulic diameter.
            state.dataWaterCoils->WaterCoil(CoilNum).CoilEffectiveInsideDiam = 4.0 * state.dataWaterCoils->WaterCoil(CoilNum).MinAirFlowArea *
                                                                               state.dataWaterCoils->WaterCoil(CoilNum).CoilDepth /
                                                                               state.dataWaterCoils->WaterCoil(CoilNum).TotCoilOutsideSurfArea;
            //   Ratio of tube outside diameter to effective fin diameter should always
            //   be less than 1
            TubeToFinDiamRatio = state.dataWaterCoils->WaterCoil(CoilNum).TubeOutsideDiam / state.dataWaterCoils->WaterCoil(CoilNum).EffectiveFinDiam;
            if (TubeToFinDiamRatio > 1.0) {
                ShowWarningError(state, format("InitWaterCoil: Detailed Flat Fin Coil, TubetoFinDiamRatio > 1.0, [{:.4R}]", TubeToFinDiamRatio));
                // reset tube depth spacing and recalc dependent parameters
                state.dataWaterCoils->WaterCoil(CoilNum).TubeDepthSpacing *= (pow_2(TubeToFinDiamRatio) + 0.1);
                state.dataWaterCoils->WaterCoil(CoilNum).CoilDepth =
                    state.dataWaterCoils->WaterCoil(CoilNum).TubeDepthSpacing * state.dataWaterCoils->WaterCoil(CoilNum).NumOfTubeRows;
                state.dataWaterCoils->WaterCoil(CoilNum).EffectiveFinDiam =
                    std::sqrt(4.0 * state.dataWaterCoils->WaterCoil(CoilNum).FinDiam * state.dataWaterCoils->WaterCoil(CoilNum).CoilDepth /
                              (DataGlobalConstants::Pi * state.dataWaterCoils->WaterCoil(CoilNum).NumOfTubeRows *
                               state.dataWaterCoils->WaterCoil(CoilNum).NumOfTubesPerRow));
                state.dataWaterCoils->WaterCoil(CoilNum).CoilEffectiveInsideDiam = 4.0 * state.dataWaterCoils->WaterCoil(CoilNum).MinAirFlowArea *
                                                                                   state.dataWaterCoils->WaterCoil(CoilNum).CoilDepth /
                                                                                   state.dataWaterCoils->WaterCoil(CoilNum).TotCoilOutsideSurfArea;
                TubeToFinDiamRatio =
                    state.dataWaterCoils->WaterCoil(CoilNum).TubeOutsideDiam / state.dataWaterCoils->WaterCoil(CoilNum).EffectiveFinDiam;
                ShowContinueError(
                    state, format("  Resetting tube depth spacing to {:.4R} meters", state.dataWaterCoils->WaterCoil(CoilNum).TubeDepthSpacing));
                ShowContinueError(state, format("  Resetting coil depth to {:.4R} meters", state.dataWaterCoils->WaterCoil(CoilNum).CoilDepth));
            }

            CalcDryFinEffCoef(state, TubeToFinDiamRatio, state.dataWaterCoils->CoefSeries);

            state.dataWaterCoils->WaterCoil(CoilNum).DryFinEfficncyCoef = state.dataWaterCoils->CoefSeries;

            FinDiamVar = 0.5 * (state.dataWaterCoils->WaterCoil(CoilNum).EffectiveFinDiam - state.dataWaterCoils->WaterCoil(CoilNum).TubeOutsideDiam);

            state.dataWaterCoils->WaterCoil(CoilNum).GeometryCoef1 =
                0.159 *
                std::pow(state.dataWaterCoils->WaterCoil(CoilNum).FinThickness / state.dataWaterCoils->WaterCoil(CoilNum).CoilEffectiveInsideDiam,
                         -0.065) *
                std::pow(state.dataWaterCoils->WaterCoil(CoilNum).FinThickness / FinDiamVar, 0.141);
            state.dataWaterCoils->WaterCoil(CoilNum).GeometryCoef2 =
                -0.323 * std::pow(state.dataWaterCoils->WaterCoil(CoilNum).FinSpacing / FinDiamVar, 0.049) *
                std::pow(state.dataWaterCoils->WaterCoil(CoilNum).EffectiveFinDiam / state.dataWaterCoils->WaterCoil(CoilNum).TubeDepthSpacing,
                         0.549) *
                std::pow(state.dataWaterCoils->WaterCoil(CoilNum).FinThickness / state.dataWaterCoils->WaterCoil(CoilNum).FinSpacing, -0.028);

            // Set some initial values for simulation
            state.dataWaterCoils->WaterCoil(CoilNum).SatEnthlCurveConstCoef = -10.57;
            state.dataWaterCoils->WaterCoil(CoilNum).SatEnthlCurveSlope = 3.3867;
            state.dataWaterCoils->WaterCoil(CoilNum).EnthVsTempCurveAppxSlope = 3.3867;
            state.dataWaterCoils->WaterCoil(CoilNum).EnthVsTempCurveConst = -10.57;
            // Set Saved Values to Zero
            state.dataWaterCoils->WaterCoil(CoilNum).SurfAreaWetSaved = 0.0;
            state.dataWaterCoils->WaterCoil(CoilNum).MeanWaterTempSaved = 0.0;
            state.dataWaterCoils->WaterCoil(CoilNum).InWaterTempSaved = 0.0;
            state.dataWaterCoils->WaterCoil(CoilNum).OutWaterTempSaved = 0.0;

        } // End the Detailed Flat Fin Coil Initialization

        // Calculation for Cooling Coil, The part between the '@@@' are design condition
        // and are calculated only once to calculate standard values for UAs and other physical parameters of
        // the cooling coil.
        // Basic Idea for UA:  Heat Transfer= UAenthalpybased*(Delta enthalpy), this is a necessity since the
        // coil may be Wet or Dry or Partially Wet-Dry, so latent effects are accounted for in this model while
        // calculating the UA. A fictitious specific heat is also defined to caculate the conventional UA.
        // On the air side, enthalpy capacity rate is the air mass flow rate,while on water side it is
        // enthalpy of saturated air at water temperature.
        //@@@ DESIGN CONDITION BEGIN HERE @@@

        // Check for zero design cooling capacity as specified by coil design inputs
        if (state.dataWaterCoils->MyCoilDesignFlag(CoilNum) &&
            (state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilModel == iCoilModel::CoolingSimple) &&
            (state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate > 0.0) &&
            (state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterMassFlowRate > 0.0)) {

            DesInletAirEnth =
                PsyHFnTdbW(state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirTemp, state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirHumRat);
            DesOutletAirEnth =
                PsyHFnTdbW(state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirTemp, state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirHumRat);
            DesSatEnthAtWaterInTemp =
                PsyHFnTdbW(state.dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp,
                           PsyWFnTdpPb(state, state.dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp, state.dataEnvrn->StdBaroPress));
            // check for dry coil
            DesHumRatAtWaterInTemp =
                PsyWFnTdbH(state, state.dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp, DesSatEnthAtWaterInTemp, RoutineName);
            if (DesHumRatAtWaterInTemp > state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirHumRat &&
                state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirTemp > state.dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp) {
                // if the design outlet air humrat is lower than the saturated air humrat at the design inlet water temp
                // and the design outlet air temperature is higher than the design inlet water temp (i.e, cooling possible),
                // move the design outlet air saturated enthalpy down (i.e., to Twaterin, Wair,out) to allow the coil to size.
                DesSatEnthAtWaterInTemp = PsyHFnTdbW(state.dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp,
                                                     state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirHumRat) -
                                          0.0001;
            }
            if (DesOutletAirEnth >= DesInletAirEnth ||
                state.dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp >= state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirTemp) {
                ShowWarningError(state,
                                 "The design cooling capacity is zero for Coil:Cooling:Water " + state.dataWaterCoils->WaterCoil(CoilNum).Name);
                ShowContinueError(state, "  The maximum water flow rate for this coil will be set to zero and the coil will do no cooling.");
                ShowContinueError(state,
                                  format("  Check the following coil design inputs for problems: Tair,in = {:.4R}",
                                         state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirTemp));
                ShowContinueError(state,
                                  format("                                                       Wair,in = {:.6R}",
                                         state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirHumRat));
                ShowContinueError(state,
                                  format("                                                       Twater,in = {:.4R}",
                                         state.dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp));
                ShowContinueError(state,
                                  format("                                                       Tair,out = {:.4R}",
                                         state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirTemp));
                ShowContinueError(state,
                                  format("                                                       Wair,out = {:.6R}",
                                         state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirHumRat));
                state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate = 0.0;
                state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterMassFlowRate = 0.0;
            }
        }

        if (state.dataWaterCoils->MyCoilDesignFlag(CoilNum) &&
            (state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilModel == iCoilModel::CoolingSimple) &&
            (state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate > 0.0) &&
            (state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterMassFlowRate > 0.0)) { // 'Cooling'

            state.dataWaterCoils->MyCoilDesignFlag(CoilNum) = false;
            state.dataWaterCoils->NoSatCurveIntersect = false;
            state.dataWaterCoils->BelowInletWaterTemp = false;
            state.dataWaterCoils->CBFTooLarge = false;
            state.dataWaterCoils->NoExitCondReset = false;
            for (Ipass = 1; Ipass <= 2; ++Ipass) {
                if (Ipass == 2) {
                    if (!state.dataWaterCoils->NoSatCurveIntersect && !state.dataWaterCoils->BelowInletWaterTemp &&
                        !state.dataWaterCoils->CBFTooLarge) {
                        goto Inlet_Conditions_Loop_exit; // coil UA calcs OK
                    } else {
                        ShowWarningError(state,
                                         "In calculating the design coil UA for Coil:Cooling:Water " + state.dataWaterCoils->WaterCoil(CoilNum).Name);
                        if (state.dataWaterCoils->NoSatCurveIntersect) {
                            ShowContinueError(state, "no apparatus dew-point can be found for the initial entering and leaving conditions;");
                        }
                        if (state.dataWaterCoils->BelowInletWaterTemp) {
                            ShowContinueError(state, "the apparatus dew-point is below the coil design inlet water temperature;");
                        }
                        if (state.dataWaterCoils->CBFTooLarge) {
                            ShowContinueError(state, "the coil bypass factor is unrealistically large;");
                        }
                        if (!state.dataWaterCoils->NoExitCondReset) {
                            ShowContinueError(state, "the coil outlet design conditions will be changed to correct the problem.");
                        }
                        ShowContinueError(
                            state,
                            format("The initial design conditions are: Tair,in = {:.4R}", state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirTemp));
                        ShowContinueError(state,
                                          format("                                   Wair,in = {:.6R}",
                                                 state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirHumRat));
                        ShowContinueError(state,
                                          format("                                   Twater,in = {:.4R}",
                                                 state.dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp));
                        ShowContinueError(state,
                                          format("                                   Tair,out = {:.4R}",
                                                 state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirTemp));
                        ShowContinueError(state,
                                          format("                                   Wair,out = {:.6R}",
                                                 state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirHumRat));
                        if (!state.dataWaterCoils->NoExitCondReset) {
                            ShowContinueError(state, format("The revised design conditions are: Tair,out = {:.4R}", state.dataWaterCoils->TOutNew));
                            ShowContinueError(state, format("                                   Wair,out = {:.6R}", state.dataWaterCoils->WOutNew));
                            state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirHumRat = state.dataWaterCoils->WOutNew;
                            state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirTemp = state.dataWaterCoils->TOutNew;
                            // update outlet air conditions used for sizing
                            std::string CompType;
                            if (state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilModel == iCoilModel::CoolingDetailed) {
                                CompType = cAllCoilTypes(Coil_CoolingWaterDetailed);
                            } else {
                                CompType = cAllCoilTypes(Coil_CoolingWater);
                            }
                            state.dataRptCoilSelection->coilSelectionReportObj->setCoilLvgAirTemp(
                                state, state.dataWaterCoils->WaterCoil(CoilNum).Name, CompType, state.dataWaterCoils->TOutNew);
                            state.dataRptCoilSelection->coilSelectionReportObj->setCoilLvgAirHumRat(
                                state, state.dataWaterCoils->WaterCoil(CoilNum).Name, CompType, state.dataWaterCoils->WOutNew);
                            // end update outlet air conditions used for sizing
                        }
                    }
                }

                // Volume flow rate being converted to mass flow rate for water
                state.dataWaterCoils->WaterCoil(CoilNum).DesAirMassFlowRate =
                    state.dataEnvrn->StdRhoAir * state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate;

                // Enthalpy of Air at Inlet design conditions
                DesInletAirEnth =
                    PsyHFnTdbW(state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirTemp, state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirHumRat);

                // Enthalpy of Air at outlet at design conditions
                DesOutletAirEnth = PsyHFnTdbW(state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirTemp,
                                              state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirHumRat);

                // already calculated above and possibly reset if dry coil
                //        ! Enthalpy of Water at Inlet design conditions
                //        DesSatEnthAtWaterInTemp =PsyHFnTdbW(WaterCoil(CoilNum)%DesInletWaterTemp, &
                //                                             PsyWFnTdpPb(state, WaterCoil(CoilNum)%DesInletWaterTemp,StdBaroPress))

                // Total Coil Load from Inlet and Outlet Air States (which include fan heat as appropriate).
                state.dataWaterCoils->WaterCoil(CoilNum).DesTotWaterCoilLoad =
                    state.dataWaterCoils->WaterCoil(CoilNum).DesAirMassFlowRate * (DesInletAirEnth - DesOutletAirEnth);

                // Enthalpy of Water at Intlet design conditions
                Cp = GetSpecificHeatGlycol(state,
                                           state.dataPlnt->PlantLoop(state.dataWaterCoils->WaterCoil(CoilNum).WaterLoopNum).FluidName,
                                           state.dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp,
                                           state.dataPlnt->PlantLoop(state.dataWaterCoils->WaterCoil(CoilNum).WaterLoopNum).FluidIndex,
                                           RoutineName);

                DesOutletWaterTemp = state.dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp +
                                     state.dataWaterCoils->WaterCoil(CoilNum).DesTotWaterCoilLoad /
                                         (state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterMassFlowRate * Cp);

                DesSatEnthAtWaterOutTemp = PsyHFnTdbW(DesOutletWaterTemp, PsyWFnTdpPb(state, DesOutletWaterTemp, state.dataEnvrn->StdBaroPress));
                DesEnthAtWaterOutTempAirInHumRat = PsyHFnTdbW(DesOutletWaterTemp, state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirHumRat);
                DesEnthWaterOut = min(DesSatEnthAtWaterOutTemp, DesEnthAtWaterOutTempAirInHumRat);

                // dry coil test
                if (state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirHumRat < state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirHumRat &&
                    DesHumRatAtWaterInTemp < state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirHumRat) { // wet coil

                    // Calculations for BYPASS FACTOR at design conditions
                    // Calculate "slope" of temperature vs. humidity ratio between entering and leaving states
                    SlopeTempVsHumRatio =
                        (state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirTemp - state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirTemp) /
                        max((state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirHumRat -
                             state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirHumRat),
                            SmallNo);

                    // Initialize iteration parameters
                    DesAirTempApparatusDewPt =
                        PsyTdpFnWPb(state, state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirHumRat, state.dataEnvrn->OutBaroPress);

                    // Iterating to calculate Apparatus Dew Point Temperature at Design Conditions
                    for (iter = 1; iter <= itmax; ++iter) {

                        // Calculate apparatus dewpoint and compare with predicted value
                        // using entering conditions and SlopeTempVsHumRatio
                        DesAirHumRatApparatusDewPt = PsyWFnTdpPb(state, DesAirTempApparatusDewPt, state.dataEnvrn->OutBaroPress);

                        // Initial Estimate for apparatus Dew Point Temperature
                        TempApparatusDewPtEstimate =
                            state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirTemp -
                            SlopeTempVsHumRatio * (state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirHumRat - DesAirHumRatApparatusDewPt);

                        // Iterating to calculate Apparatus Dew Point Temperature at Design Condition
                        error = DesAirTempApparatusDewPt - TempApparatusDewPtEstimate;
                        Iterate(ResultX, 0.01, DesAirTempApparatusDewPt, error, X1, Y1, iter, icvg);
                        DesAirTempApparatusDewPt = ResultX;

                        // If converged, exit loop
                        if (icvg == 1) {
                            goto App_DewPoint_Loop1_exit;
                        }

                        // If not converged due to low Humidity Ratio approximate value at outlet conditions
                        if (iter == itmax) {
                            state.dataWaterCoils->NoSatCurveIntersect = true;
                            DesAirTempApparatusDewPt =
                                PsyTdpFnWPb(state, state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirHumRat, state.dataEnvrn->OutBaroPress);
                            DesAirHumRatApparatusDewPt = PsyWFnTdpPb(state, DesAirTempApparatusDewPt, state.dataEnvrn->OutBaroPress);
                            goto App_DewPoint_Loop1_exit;
                        }

                        // End of Loop for Iteration
                    }
                App_DewPoint_Loop1_exit:;

                    // Air enthalpy at apparatus dew point at design conditions
                    DesAirApparatusDewPtEnth = PsyHFnTdbW(DesAirTempApparatusDewPt, DesAirHumRatApparatusDewPt);

                    // Calculate bypass factor from enthalpies calculated above.
                    DesBypassFactor = (DesOutletAirEnth - DesAirApparatusDewPtEnth) / (DesInletAirEnth - DesAirApparatusDewPtEnth);

                    // Check for bypass factor for unsuitable value. Note that bypass factor is never used in the coil calculation
                    if ((DesBypassFactor > 0.5) || (DesBypassFactor < 0.0)) {
                        state.dataWaterCoils->CBFTooLarge = true;
                        DesBypassFactor = 0.37;
                    }

                    if (DesEnthWaterOut > DesInletAirEnth) {
                        ShowWarningError(state,
                                         "In calculating the design coil UA for Coil:Cooling:Water " + state.dataWaterCoils->WaterCoil(CoilNum).Name);
                        ShowContinueError(state, "the outlet chilled water design enthalpy is greater than the inlet air design enthalpy.");
                        ShowContinueError(state,
                                          format("To correct this condition the design chilled water flow rate will be increased from {:.5R}",
                                                 state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate));
                        EnthCorrFrac = (DesEnthWaterOut - DesInletAirEnth) / (DesEnthWaterOut - DesSatEnthAtWaterInTemp);
                        state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate *= (1.0 + 2.0 * EnthCorrFrac);
                        ShowContinueError(state, format("to {:.5R} m3/s", state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate));
                        state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterMassFlowRate =
                            rho * state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate;
                        DesOutletWaterTemp = state.dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp +
                                             state.dataWaterCoils->WaterCoil(CoilNum).DesTotWaterCoilLoad /
                                                 (state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterMassFlowRate * Cp);
                        DesSatEnthAtWaterOutTemp =
                            PsyHFnTdbW(DesOutletWaterTemp, PsyWFnTdpPb(state, DesOutletWaterTemp, state.dataEnvrn->StdBaroPress));
                        DesEnthAtWaterOutTempAirInHumRat = PsyHFnTdbW(DesOutletWaterTemp, state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirHumRat);
                        DesEnthWaterOut = min(DesSatEnthAtWaterOutTemp, DesEnthAtWaterOutTempAirInHumRat);
                    }

                    // Determine air-side coefficient, UACoilExternal, assuming that the
                    // surface temperature is at the apparatus dewpoint temperature
                    if (DesAirApparatusDewPtEnth <= DesSatEnthAtWaterInTemp) state.dataWaterCoils->BelowInletWaterTemp = true;
                    if ((DesInletAirEnth - DesEnthWaterOut) > SmallNo && (DesOutletAirEnth - DesSatEnthAtWaterInTemp) > SmallNo) {
                        LogMeanEnthDiff = ((DesInletAirEnth - DesEnthWaterOut) - (DesOutletAirEnth - DesSatEnthAtWaterInTemp)) /
                                          std::log((DesInletAirEnth - DesEnthWaterOut) / (DesOutletAirEnth - DesSatEnthAtWaterInTemp));
                    } else {
                        LogMeanEnthDiff = 2000.0; // UA will be 1/2 the design coil load
                    }
                    DesUACoilExternalEnth = state.dataWaterCoils->WaterCoil(CoilNum).DesTotWaterCoilLoad / LogMeanEnthDiff;
                    state.dataWaterCoils->WaterCoil(CoilNum).UACoilExternal =
                        DesUACoilExternalEnth * PsyCpAirFnW(state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirHumRat);

                    if (Ipass == 1 && (state.dataWaterCoils->NoSatCurveIntersect || state.dataWaterCoils->CBFTooLarge ||
                                       state.dataWaterCoils->BelowInletWaterTemp)) {
                        // reset outlet conditions to 90% relative humidity at the same outlet enthalpy
                        state.dataWaterCoils->TOutNew = TdbFnHRhPb(state, DesOutletAirEnth, 0.9, state.dataEnvrn->StdBaroPress);
                        state.dataWaterCoils->WOutNew = PsyWFnTdbH(state, state.dataWaterCoils->TOutNew, DesOutletAirEnth);
                        if (state.dataWaterCoils->WOutNew >= state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirHumRat ||
                            state.dataWaterCoils->TOutNew > state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirTemp) {
                            state.dataWaterCoils->NoExitCondReset = true;
                        }
                        goto Inlet_Conditions_Loop_loop;
                    }

                    state.dataWaterCoils->WaterCoil(CoilNum).UACoilInternal = state.dataWaterCoils->WaterCoil(CoilNum).UACoilExternal * 3.30;
                    // Overall heat transfer coefficient
                    state.dataWaterCoils->WaterCoil(CoilNum).UACoilTotal = 1.0 / (1.0 / state.dataWaterCoils->WaterCoil(CoilNum).UACoilExternal +
                                                                                  1.0 / state.dataWaterCoils->WaterCoil(CoilNum).UACoilInternal);

                } else { // dry coil

                    if (DesOutletWaterTemp > state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirTemp) {
                        ShowWarningError(state,
                                         "In calculating the design coil UA for Coil:Cooling:Water " + state.dataWaterCoils->WaterCoil(CoilNum).Name);
                        ShowContinueError(state, "the outlet chilled water design temperature is greater than the inlet air design temperature.");
                        ShowContinueError(state,
                                          format("To correct this condition the design chilled water flow rate will be increased from {:.5R}",
                                                 state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate));
                        TempCorrFrac = (DesOutletWaterTemp - state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirTemp) /
                                       (DesOutletWaterTemp - state.dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp);
                        state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate *= (1.0 + 2.0 * TempCorrFrac);
                        ShowContinueError(state, format("to {:.5R} m3/s", state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate));
                        state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterMassFlowRate =
                            rho * state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate;
                        DesOutletWaterTemp = state.dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp +
                                             state.dataWaterCoils->WaterCoil(CoilNum).DesTotWaterCoilLoad /
                                                 (state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterMassFlowRate * Cp);
                    }

                    if ((state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirTemp - DesOutletWaterTemp) > SmallNo &&
                        (state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirTemp - state.dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp) >
                            SmallNo) {
                        LogMeanTempDiff = ((state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirTemp - DesOutletWaterTemp) -
                                           (state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirTemp -
                                            state.dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp)) /
                                          std::log((state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirTemp - DesOutletWaterTemp) /
                                                   (state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirTemp -
                                                    state.dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp));
                        state.dataWaterCoils->WaterCoil(CoilNum).UACoilExternal =
                            state.dataWaterCoils->WaterCoil(CoilNum).DesTotWaterCoilLoad / LogMeanTempDiff;
                    } else {
                        state.dataWaterCoils->WaterCoil(CoilNum).UACoilExternal =
                            state.dataWaterCoils->WaterCoil(CoilNum).DesTotWaterCoilLoad / 2.0; // make the UA large
                    }
                    state.dataWaterCoils->WaterCoil(CoilNum).UACoilInternal = state.dataWaterCoils->WaterCoil(CoilNum).UACoilExternal * 3.30;
                    // Overall heat transfer coefficient
                    state.dataWaterCoils->WaterCoil(CoilNum).UACoilTotal = 1.0 / (1.0 / state.dataWaterCoils->WaterCoil(CoilNum).UACoilExternal +
                                                                                  1.0 / state.dataWaterCoils->WaterCoil(CoilNum).UACoilInternal);
                    goto Inlet_Conditions_Loop_exit;
                }

            Inlet_Conditions_Loop_loop:;
            }
        Inlet_Conditions_Loop_exit:;

            // estimate the heat external transfer surface area using typical design over all U value
            state.dataWaterCoils->WaterCoil(CoilNum).TotCoilOutsideSurfArea = EstimateHEXSurfaceArea(state, CoilNum);
            // calculate internal and external "UA per external surface area"
            state.dataWaterCoils->WaterCoil(CoilNum).UACoilInternalPerUnitArea =
                state.dataWaterCoils->WaterCoil(CoilNum).UACoilInternal / state.dataWaterCoils->WaterCoil(CoilNum).TotCoilOutsideSurfArea;
            state.dataWaterCoils->WaterCoil(CoilNum).UAWetExtPerUnitArea =
                state.dataWaterCoils->WaterCoil(CoilNum).UACoilExternal / state.dataWaterCoils->WaterCoil(CoilNum).TotCoilOutsideSurfArea;
            // approximate the dry UA as 1.0 times wet UA
            state.dataWaterCoils->WaterCoil(CoilNum).UADryExtPerUnitArea = state.dataWaterCoils->WaterCoil(CoilNum).UAWetExtPerUnitArea;

            // Now use SolveRoot to "invert" the cooling coil model to obtain the UA given the specified design inlet and outlet conditions
            // Note that the UAs we have obtained so far are rough estimates that are the starting points for the the following iterative
            //   calulation of the actual UAs.
            state.dataWaterCoils->Par(1) = state.dataWaterCoils->WaterCoil(CoilNum).DesTotWaterCoilLoad;
            state.dataWaterCoils->Par(2) = double(CoilNum);
            state.dataWaterCoils->Par(3) = double(ContFanCycCoil); // fan operating mode
            state.dataWaterCoils->Par(4) = 1.0;                    // part-load ratio
            state.dataWaterCoils->WaterCoil(CoilNum).InletAirTemp = state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirTemp;
            state.dataWaterCoils->WaterCoil(CoilNum).InletAirHumRat = state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirHumRat;
            state.dataWaterCoils->WaterCoil(CoilNum).InletWaterTemp = state.dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp;
            state.dataWaterCoils->WaterCoil(CoilNum).InletWaterMassFlowRate = rho * state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate;
            state.dataWaterCoils->WaterCoil(CoilNum).InletAirMassFlowRate = state.dataWaterCoils->WaterCoil(CoilNum).DesAirMassFlowRate;
            // set the lower and upper limits on the UA
            UA0 = 0.1 * state.dataWaterCoils->WaterCoil(CoilNum).UACoilExternal;
            UA1 = 10.0 * state.dataWaterCoils->WaterCoil(CoilNum).UACoilExternal;
            // Invert the simple cooling coil model: given the design inlet conditions and the design load, find the design UA
            General::SolveRoot(state, 0.001, MaxIte, SolFla, UA, SimpleCoolingCoilUAResidual, UA0, UA1, state.dataWaterCoils->Par);
            // if the numerical inversion failed, issue error messages.
            if (SolFla == -1) {
                ShowSevereError(state, "Calculation of cooling coil design UA failed for coil " + state.dataWaterCoils->WaterCoil(CoilNum).Name);
                ShowContinueError(state, "  Iteration limit exceeded in calculating coil UA");
                state.dataWaterCoils->WaterCoil(CoilNum).UACoilExternal = UA0 * 10.0;
                state.dataWaterCoils->WaterCoil(CoilNum).UACoilInternal = state.dataWaterCoils->WaterCoil(CoilNum).UACoilExternal * 3.3;
                state.dataWaterCoils->WaterCoil(CoilNum).UACoilTotal = 1.0 / (1.0 / state.dataWaterCoils->WaterCoil(CoilNum).UACoilExternal +
                                                                              1.0 / state.dataWaterCoils->WaterCoil(CoilNum).UACoilInternal);
                state.dataWaterCoils->WaterCoil(CoilNum).TotCoilOutsideSurfArea = EstimateHEXSurfaceArea(state, CoilNum);
                state.dataWaterCoils->WaterCoil(CoilNum).UACoilInternalPerUnitArea =
                    state.dataWaterCoils->WaterCoil(CoilNum).UACoilInternal / state.dataWaterCoils->WaterCoil(CoilNum).TotCoilOutsideSurfArea;
                state.dataWaterCoils->WaterCoil(CoilNum).UAWetExtPerUnitArea =
                    state.dataWaterCoils->WaterCoil(CoilNum).UACoilExternal / state.dataWaterCoils->WaterCoil(CoilNum).TotCoilOutsideSurfArea;
                state.dataWaterCoils->WaterCoil(CoilNum).UADryExtPerUnitArea = state.dataWaterCoils->WaterCoil(CoilNum).UAWetExtPerUnitArea;
                ShowContinueError(state, format(" Coil design UA set to {:.6R} [W/C]", state.dataWaterCoils->WaterCoil(CoilNum).UACoilTotal));
            } else if (SolFla == -2) {
                ShowSevereError(state, "Calculation of cooling coil design UA failed for coil " + state.dataWaterCoils->WaterCoil(CoilNum).Name);
                ShowContinueError(state, "  Bad starting values for UA");
                state.dataWaterCoils->WaterCoil(CoilNum).UACoilExternal = UA0 * 10.0;
                state.dataWaterCoils->WaterCoil(CoilNum).UACoilInternal = state.dataWaterCoils->WaterCoil(CoilNum).UACoilExternal * 3.3;
                state.dataWaterCoils->WaterCoil(CoilNum).UACoilTotal = 1.0 / (1.0 / state.dataWaterCoils->WaterCoil(CoilNum).UACoilExternal +
                                                                              1.0 / state.dataWaterCoils->WaterCoil(CoilNum).UACoilInternal);
                state.dataWaterCoils->WaterCoil(CoilNum).TotCoilOutsideSurfArea = EstimateHEXSurfaceArea(state, CoilNum);
                state.dataWaterCoils->WaterCoil(CoilNum).UACoilInternalPerUnitArea =
                    state.dataWaterCoils->WaterCoil(CoilNum).UACoilInternal / state.dataWaterCoils->WaterCoil(CoilNum).TotCoilOutsideSurfArea;
                state.dataWaterCoils->WaterCoil(CoilNum).UAWetExtPerUnitArea =
                    state.dataWaterCoils->WaterCoil(CoilNum).UACoilExternal / state.dataWaterCoils->WaterCoil(CoilNum).TotCoilOutsideSurfArea;
                state.dataWaterCoils->WaterCoil(CoilNum).UADryExtPerUnitArea = state.dataWaterCoils->WaterCoil(CoilNum).UAWetExtPerUnitArea;
                ShowContinueError(state, format(" Coil design UA set to {:.6R} [W/C]", state.dataWaterCoils->WaterCoil(CoilNum).UACoilTotal));
            }

            // cooling coil surface area
            state.dataWaterCoils->SurfaceArea = state.dataWaterCoils->WaterCoil(CoilNum).TotCoilOutsideSurfArea;

            // cooling coil overall UA value
            state.dataWaterCoils->UATotal = state.dataWaterCoils->WaterCoil(CoilNum).UACoilTotal;

            // save the design internal and external UAs
            state.dataWaterCoils->WaterCoil(CoilNum).UACoilExternalDes = state.dataWaterCoils->WaterCoil(CoilNum).UACoilExternal;
            state.dataWaterCoils->WaterCoil(CoilNum).UACoilInternalDes = state.dataWaterCoils->WaterCoil(CoilNum).UACoilInternal;
        }

        //@@@@ DESIGN CONDITION END HERE @@@@

        // Calculate rated Total, latent, sensible capacity, SHR, effectiveness
        if (state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType == DataPlant::TypeOf_CoilWaterSimpleHeating) {
            state.dataWaterCoils->WaterCoil(CoilNum).InletAirTemp = 16.6;
            state.dataWaterCoils->WaterCoil(CoilNum).InletAirHumRat = PsyWFnTdbRhPb(state, 16.6, 0.5, state.dataEnvrn->StdBaroPress, RoutineName);
            state.dataWaterCoils->WaterCoil(CoilNum).InletWaterTemp = 82.2;
        } else {
            state.dataWaterCoils->WaterCoil(CoilNum).InletAirTemp = 26.67;
            state.dataWaterCoils->WaterCoil(CoilNum).InletAirHumRat = PsyWFnTdbTwbPb(state, 26.67, 19.44, state.dataEnvrn->StdBaroPress, RoutineName);
            state.dataWaterCoils->WaterCoil(CoilNum).InletWaterTemp = 6.67;
        }
        state.dataWaterCoils->WaterCoil(CoilNum).InletAirEnthalpy =
            PsyHFnTdbW(state.dataWaterCoils->WaterCoil(CoilNum).InletAirTemp, state.dataWaterCoils->WaterCoil(CoilNum).InletAirHumRat);
        state.dataWaterCoils->WaterCoil(CoilNum).InletWaterMassFlowRate = state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterMassFlowRate;
        state.dataWaterCoils->WaterCoil(CoilNum).InletAirMassFlowRate =
            state.dataEnvrn->StdRhoAir * state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate;
        CapacitanceAir =
            state.dataWaterCoils->WaterCoil(CoilNum).InletAirMassFlowRate * PsyCpAirFnW(state.dataWaterCoils->WaterCoil(CoilNum).InletAirHumRat);

        Cp = GetSpecificHeatGlycol(state,
                                   state.dataPlnt->PlantLoop(state.dataWaterCoils->WaterCoil(CoilNum).WaterLoopNum).FluidName,
                                   state.dataWaterCoils->WaterCoil(CoilNum).InletWaterTemp,
                                   state.dataPlnt->PlantLoop(state.dataWaterCoils->WaterCoil(CoilNum).WaterLoopNum).FluidIndex,
                                   RoutineName);

        state.dataWaterCoils->CapacitanceWater = state.dataWaterCoils->WaterCoil(CoilNum).InletWaterMassFlowRate * Cp;
        state.dataWaterCoils->CMin = min(CapacitanceAir, state.dataWaterCoils->CapacitanceWater);
        if (state.dataWaterCoils->CMin > 0.0) {
            if (state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType == DataPlant::TypeOf_CoilWaterCooling) {
                CoolingCoil(state, CoilNum, FirstHVACIteration, state.dataWaterCoils->DesignCalc, ContFanCycCoil, 1.0);
                state.dataWaterCoils->CoilEffectiveness =
                    (state.dataWaterCoils->WaterCoil(CoilNum).InletAirTemp - state.dataWaterCoils->WaterCoil(CoilNum).OutletAirTemp) /
                    (state.dataWaterCoils->WaterCoil(CoilNum).InletAirTemp - state.dataWaterCoils->WaterCoil(CoilNum).InletWaterTemp) *
                    (CapacitanceAir / state.dataWaterCoils->CMin);
                state.dataWaterCoils->RatedLatentCapacity = state.dataWaterCoils->WaterCoil(CoilNum).TotWaterCoolingCoilRate -
                                                            state.dataWaterCoils->WaterCoil(CoilNum).SenWaterCoolingCoilRate;
                state.dataWaterCoils->RatedSHR = state.dataWaterCoils->WaterCoil(CoilNum).SenWaterCoolingCoilRate /
                                                 state.dataWaterCoils->WaterCoil(CoilNum).TotWaterCoolingCoilRate;
            } else if (state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType == DataPlant::TypeOf_CoilWaterDetailedFlatCooling) {
                CalcDetailFlatFinCoolingCoil(state, CoilNum, state.dataWaterCoils->DesignCalc, ContFanCycCoil, 1.0);
                state.dataWaterCoils->CoilEffectiveness =
                    (state.dataWaterCoils->WaterCoil(CoilNum).InletAirTemp - state.dataWaterCoils->WaterCoil(CoilNum).OutletAirTemp) /
                    (state.dataWaterCoils->WaterCoil(CoilNum).InletAirTemp - state.dataWaterCoils->WaterCoil(CoilNum).InletWaterTemp) *
                    (CapacitanceAir / state.dataWaterCoils->CMin);
                state.dataWaterCoils->RatedLatentCapacity = state.dataWaterCoils->WaterCoil(CoilNum).TotWaterCoolingCoilRate -
                                                            state.dataWaterCoils->WaterCoil(CoilNum).SenWaterCoolingCoilRate;
                state.dataWaterCoils->RatedSHR = state.dataWaterCoils->WaterCoil(CoilNum).SenWaterCoolingCoilRate /
                                                 state.dataWaterCoils->WaterCoil(CoilNum).TotWaterCoolingCoilRate;
            } else if (state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType == DataPlant::TypeOf_CoilWaterSimpleHeating) {
                CalcSimpleHeatingCoil(state, CoilNum, ContFanCycCoil, 1.0, state.dataWaterCoils->DesignCalc);
                state.dataWaterCoils->CoilEffectiveness =
                    (state.dataWaterCoils->WaterCoil(CoilNum).OutletAirTemp - state.dataWaterCoils->WaterCoil(CoilNum).InletAirTemp) /
                    (state.dataWaterCoils->WaterCoil(CoilNum).InletWaterTemp - state.dataWaterCoils->WaterCoil(CoilNum).InletAirTemp) *
                    (CapacitanceAir / state.dataWaterCoils->CMin);
            }
        } else {
            state.dataWaterCoils->CoilEffectiveness = 0.0;
            state.dataWaterCoils->WaterCoil(CoilNum).TotWaterHeatingCoilRate = 0.0;
            state.dataWaterCoils->WaterCoil(CoilNum).TotWaterCoolingCoilRate = 0.0;
            state.dataWaterCoils->WaterCoil(CoilNum).SenWaterCoolingCoilRate = 0.0;
            state.dataWaterCoils->RatedLatentCapacity = 0.0;
            state.dataWaterCoils->RatedSHR = 0.0;
        }
        state.dataWaterCoils->MyEnvrnFlag(CoilNum) = false;

    } // End If for the Begin Environment initializations

    if (!state.dataGlobal->BeginEnvrnFlag) {
        state.dataWaterCoils->MyEnvrnFlag(CoilNum) = true;
    }

    if (!state.dataGlobal->DoingSizing) {
        if (state.dataWaterCoils->MyCoilReportFlag(CoilNum)) {
            // create predefined report entries
            state.dataWaterCoils->MyCoilReportFlag(CoilNum) = false;
            {
                auto const SELECT_CASE_var(state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType);
                if (SELECT_CASE_var == DataPlant::TypeOf_CoilWaterSimpleHeating) {
                    if (state.dataWaterCoils->RptCoilHeaderFlag(1)) {
                        print(state.files.eio, "{}", "! <Water Heating Coil Capacity Information>,Component Type,Name,Nominal Total Capacity {W}\n");
                        state.dataWaterCoils->RptCoilHeaderFlag(1) = false;
                    }
                    PreDefTableEntry(
                        state, state.dataOutRptPredefined->pdchHeatCoilType, state.dataWaterCoils->WaterCoil(CoilNum).Name, "Coil:Heating:Water");
                    PreDefTableEntry(state,
                                     state.dataOutRptPredefined->pdchHeatCoilDesCap,
                                     state.dataWaterCoils->WaterCoil(CoilNum).Name,
                                     state.dataWaterCoils->WaterCoil(CoilNum).DesWaterHeatingCoilRate);
                    PreDefTableEntry(state,
                                     state.dataOutRptPredefined->pdchHeatCoilNomCap,
                                     state.dataWaterCoils->WaterCoil(CoilNum).Name,
                                     state.dataWaterCoils->WaterCoil(CoilNum).TotWaterHeatingCoilRate);
                    PreDefTableEntry(state, state.dataOutRptPredefined->pdchHeatCoilNomEff, state.dataWaterCoils->WaterCoil(CoilNum).Name, "-");
                    addFootNoteSubTable(
                        state,
                        state.dataOutRptPredefined->pdstHeatCoil,
                        "Nominal values are gross at rated conditions, i.e., the supply air fan heat and electric power NOT accounted for.");
                    print(state.files.eio,
                          "{},{},{:.2R}\n",
                          "Water Heating Coil Capacity Information,Coil:Heating:Water",
                          state.dataWaterCoils->WaterCoil(CoilNum).Name,
                          state.dataWaterCoils->WaterCoil(CoilNum).TotWaterHeatingCoilRate);
                    state.dataRptCoilSelection->coilSelectionReportObj->setCoilAirFlow(state,
                                                                                       state.dataWaterCoils->WaterCoil(CoilNum).Name,
                                                                                       "Coil:Heating:Water",
                                                                                       state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate,
                                                                                       state.dataWaterCoils->WaterCoil(CoilNum).RequestingAutoSize);
                    state.dataRptCoilSelection->coilSelectionReportObj->setCoilWaterHeaterCapacityNodeNums(
                        state,
                        state.dataWaterCoils->WaterCoil(CoilNum).Name,
                        "Coil:Heating:Water",
                        state.dataWaterCoils->WaterCoil(CoilNum).DesWaterHeatingCoilRate,
                        state.dataWaterCoils->WaterCoil(CoilNum).RequestingAutoSize,
                        state.dataWaterCoils->WaterCoil(CoilNum).WaterInletNodeNum,
                        state.dataWaterCoils->WaterCoil(CoilNum).WaterOutletNodeNum,
                        state.dataWaterCoils->WaterCoil(CoilNum).WaterLoopNum); // coil report
                } else if (SELECT_CASE_var == DataPlant::TypeOf_CoilWaterDetailedFlatCooling) {
                    if (state.dataWaterCoils->RptCoilHeaderFlag(2)) {
                        print(state.files.eio,
                              "{}\n",
                              "! <Water Cooling Coil Capacity Information>,Component Type,Name,Nominal Total "
                              "Capacity {W},Nominal Sensible Capacity {W},Nominal Latent Capacity {W},Nominal "
                              "Sensible Heat Ratio");
                        state.dataWaterCoils->RptCoilHeaderFlag(2) = false;
                    }
                    state.dataWaterCoils->RatedLatentCapacity = state.dataWaterCoils->WaterCoil(CoilNum).TotWaterCoolingCoilRate -
                                                                state.dataWaterCoils->WaterCoil(CoilNum).SenWaterCoolingCoilRate;
                    state.dataWaterCoils->RatedSHR = SafeDivide(state.dataWaterCoils->WaterCoil(CoilNum).SenWaterCoolingCoilRate,
                                                                state.dataWaterCoils->WaterCoil(CoilNum).TotWaterCoolingCoilRate);
                    PreDefTableEntry(state,
                                     state.dataOutRptPredefined->pdchCoolCoilType,
                                     state.dataWaterCoils->WaterCoil(CoilNum).Name,
                                     "Coil:Cooling:Water:DetailedGeometry");
                    PreDefTableEntry(state,
                                     state.dataOutRptPredefined->pdchCoolCoilDesCap,
                                     state.dataWaterCoils->WaterCoil(CoilNum).Name,
                                     state.dataWaterCoils->WaterCoil(CoilNum).DesWaterCoolingCoilRate);
                    PreDefTableEntry(state,
                                     state.dataOutRptPredefined->pdchCoolCoilTotCap,
                                     state.dataWaterCoils->WaterCoil(CoilNum).Name,
                                     state.dataWaterCoils->WaterCoil(CoilNum).TotWaterCoolingCoilRate);
                    PreDefTableEntry(state,
                                     state.dataOutRptPredefined->pdchCoolCoilSensCap,
                                     state.dataWaterCoils->WaterCoil(CoilNum).Name,
                                     state.dataWaterCoils->WaterCoil(CoilNum).SenWaterCoolingCoilRate);
                    PreDefTableEntry(state,
                                     state.dataOutRptPredefined->pdchCoolCoilLatCap,
                                     state.dataWaterCoils->WaterCoil(CoilNum).Name,
                                     state.dataWaterCoils->RatedLatentCapacity);
                    PreDefTableEntry(state,
                                     state.dataOutRptPredefined->pdchCoolCoilSHR,
                                     state.dataWaterCoils->WaterCoil(CoilNum).Name,
                                     state.dataWaterCoils->RatedSHR);
                    PreDefTableEntry(state, state.dataOutRptPredefined->pdchCoolCoilNomEff, state.dataWaterCoils->WaterCoil(CoilNum).Name, "-");
                    addFootNoteSubTable(
                        state,
                        state.dataOutRptPredefined->pdstCoolCoil,
                        "Nominal values are gross at rated conditions, i.e., the supply air fan heat and electric power NOT accounted for.");
                    print(state.files.eio,
                          "{},{},{:.2R},{:.2R},{:.2R},{:.2R}\n",
                          "Water Cooling Coil Capacity Information,Coil:Cooling:Water:DetailedGeometry",
                          state.dataWaterCoils->WaterCoil(CoilNum).Name,
                          state.dataWaterCoils->WaterCoil(CoilNum).TotWaterCoolingCoilRate,
                          state.dataWaterCoils->WaterCoil(CoilNum).SenWaterCoolingCoilRate,
                          state.dataWaterCoils->RatedLatentCapacity,
                          state.dataWaterCoils->RatedSHR);
                    state.dataRptCoilSelection->coilSelectionReportObj->setCoilAirFlow(
                        state,
                        state.dataWaterCoils->WaterCoil(CoilNum).Name,
                        "Coil:Cooling:Water:DetailedGeometry",
                        state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate,
                        state.dataWaterCoils->WaterCoil(CoilNum).RequestingAutoSize); // Coil Report
                    state.dataRptCoilSelection->coilSelectionReportObj->setCoilWaterCoolingCapacity(
                        state,
                        state.dataWaterCoils->WaterCoil(CoilNum).Name,
                        "Coil:Cooling:Water:DetailedGeometry",
                        state.dataWaterCoils->WaterCoil(CoilNum).DesWaterCoolingCoilRate,
                        state.dataWaterCoils->WaterCoil(CoilNum).RequestingAutoSize,
                        state.dataWaterCoils->WaterCoil(CoilNum).WaterInletNodeNum,
                        state.dataWaterCoils->WaterCoil(CoilNum).WaterOutletNodeNum,
                        state.dataWaterCoils->WaterCoil(CoilNum).WaterLoopNum); // Coil Report
                } else if (SELECT_CASE_var == DataPlant::TypeOf_CoilWaterCooling) {
                    if (state.dataWaterCoils->RptCoilHeaderFlag(2)) {
                        print(state.files.eio,
                              "{}\n",
                              "! <Water Cooling Coil Capacity Information>,Component Type,Name,Nominal Total "
                              "Capacity {W},Nominal Sensible Capacity {W},Nominal Latent Capacity {W},Nominal "
                              "Sensible Heat Ratio, Nominal Coil UA Value {W/C}, Nominal Coil Surface Area {m2}");
                        state.dataWaterCoils->RptCoilHeaderFlag(2) = false;
                    }
                    state.dataWaterCoils->RatedLatentCapacity = state.dataWaterCoils->WaterCoil(CoilNum).TotWaterCoolingCoilRate -
                                                                state.dataWaterCoils->WaterCoil(CoilNum).SenWaterCoolingCoilRate;
                    state.dataWaterCoils->RatedSHR = SafeDivide(state.dataWaterCoils->WaterCoil(CoilNum).SenWaterCoolingCoilRate,
                                                                state.dataWaterCoils->WaterCoil(CoilNum).TotWaterCoolingCoilRate);
                    PreDefTableEntry(
                        state, state.dataOutRptPredefined->pdchCoolCoilType, state.dataWaterCoils->WaterCoil(CoilNum).Name, "Coil:Cooling:Water");
                    PreDefTableEntry(state,
                                     state.dataOutRptPredefined->pdchCoolCoilDesCap,
                                     state.dataWaterCoils->WaterCoil(CoilNum).Name,
                                     state.dataWaterCoils->WaterCoil(CoilNum).DesWaterCoolingCoilRate);
                    PreDefTableEntry(state,
                                     state.dataOutRptPredefined->pdchCoolCoilTotCap,
                                     state.dataWaterCoils->WaterCoil(CoilNum).Name,
                                     state.dataWaterCoils->WaterCoil(CoilNum).TotWaterCoolingCoilRate);
                    PreDefTableEntry(state,
                                     state.dataOutRptPredefined->pdchCoolCoilSensCap,
                                     state.dataWaterCoils->WaterCoil(CoilNum).Name,
                                     state.dataWaterCoils->WaterCoil(CoilNum).SenWaterCoolingCoilRate);
                    PreDefTableEntry(state,
                                     state.dataOutRptPredefined->pdchCoolCoilLatCap,
                                     state.dataWaterCoils->WaterCoil(CoilNum).Name,
                                     state.dataWaterCoils->RatedLatentCapacity);
                    PreDefTableEntry(state,
                                     state.dataOutRptPredefined->pdchCoolCoilSHR,
                                     state.dataWaterCoils->WaterCoil(CoilNum).Name,
                                     state.dataWaterCoils->RatedSHR);
                    PreDefTableEntry(state, state.dataOutRptPredefined->pdchCoolCoilNomEff, state.dataWaterCoils->WaterCoil(CoilNum).Name, "-");
                    PreDefTableEntry(state,
                                     state.dataOutRptPredefined->pdchCoolCoilUATotal,
                                     state.dataWaterCoils->WaterCoil(CoilNum).Name,
                                     state.dataWaterCoils->WaterCoil(CoilNum).UACoilTotal);
                    PreDefTableEntry(state,
                                     state.dataOutRptPredefined->pdchCoolCoilArea,
                                     state.dataWaterCoils->WaterCoil(CoilNum).Name,
                                     state.dataWaterCoils->WaterCoil(CoilNum).TotCoilOutsideSurfArea);
                    addFootNoteSubTable(
                        state,
                        state.dataOutRptPredefined->pdstCoolCoil,
                        "Nominal values are gross at rated conditions, i.e., the supply air fan heat and electric power NOT accounted for.");
                    print(state.files.eio,
                          "{},{},{:.2R},{:.2R},{:.2R},{:.2R},{:.2R},{:.2R}\n",
                          "Water Cooling Coil Capacity Information,Coil:Cooling:Water",
                          state.dataWaterCoils->WaterCoil(CoilNum).Name,
                          state.dataWaterCoils->WaterCoil(CoilNum).TotWaterCoolingCoilRate,
                          state.dataWaterCoils->WaterCoil(CoilNum).SenWaterCoolingCoilRate,
                          state.dataWaterCoils->RatedLatentCapacity,
                          state.dataWaterCoils->RatedSHR,
                          state.dataWaterCoils->UATotal,
                          state.dataWaterCoils->SurfaceArea);
                    state.dataRptCoilSelection->coilSelectionReportObj->setCoilAirFlow(
                        state,
                        state.dataWaterCoils->WaterCoil(CoilNum).Name,
                        "Coil:Cooling:Water",
                        state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate,
                        state.dataWaterCoils->WaterCoil(CoilNum).RequestingAutoSize); // Coil Report
                    state.dataRptCoilSelection->coilSelectionReportObj->setCoilWaterCoolingCapacity(
                        state,
                        state.dataWaterCoils->WaterCoil(CoilNum).Name,
                        "Coil:Cooling:Water",
                        state.dataWaterCoils->WaterCoil(CoilNum).DesWaterCoolingCoilRate,
                        state.dataWaterCoils->WaterCoil(CoilNum).RequestingAutoSize,
                        state.dataWaterCoils->WaterCoil(CoilNum).WaterInletNodeNum,
                        state.dataWaterCoils->WaterCoil(CoilNum).WaterOutletNodeNum,
                        state.dataWaterCoils->WaterCoil(CoilNum).WaterLoopNum); // Coil Report
                }
            }
            if (state.dataWaterCoils->WaterCoil(CoilNum).DesWaterCoolingCoilRate <= 0.0)
                state.dataWaterCoils->WaterCoil(CoilNum).DesWaterCoolingCoilRate = state.dataWaterCoils->WaterCoil(CoilNum).TotWaterCoolingCoilRate;
            if (state.dataWaterCoils->WaterCoil(CoilNum).DesWaterHeatingCoilRate <= 0.0)
                state.dataWaterCoils->WaterCoil(CoilNum).DesWaterHeatingCoilRate = state.dataWaterCoils->WaterCoil(CoilNum).TotWaterHeatingCoilRate;

            // call coil model with everthing set at rating point
            state.dataWaterCoils->WaterCoil(CoilNum).InletAirMassFlowRate = state.dataWaterCoils->WaterCoil(CoilNum).DesAirMassFlowRate;
            state.dataWaterCoils->WaterCoil(CoilNum).InletAirTemp = state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirTemp;
            state.dataWaterCoils->WaterCoil(CoilNum).InletAirHumRat =
                state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirHumRat; // fixed in sizing routine
            state.dataWaterCoils->WaterCoil(CoilNum).InletAirEnthalpy = Psychrometrics::PsyHFnTdbW(
                state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirTemp, state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirHumRat);
            Real64 DesInletWetBulb = Psychrometrics::PsyTwbFnTdbWPb(state,
                                                                    state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirTemp,
                                                                    state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirHumRat,
                                                                    DataEnvironment::StdPressureSeaLevel,
                                                                    "InitWaterCoils");
            state.dataWaterCoils->WaterCoil(CoilNum).InletWaterMassFlowRate = state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterMassFlowRate;
            state.dataWaterCoils->WaterCoil(CoilNum).InletWaterTemp = state.dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp;
            Real64 cp = GetSpecificHeatGlycol(state,
                                              state.dataPlnt->PlantLoop(state.dataWaterCoils->WaterCoil(CoilNum).WaterLoopNum).FluidName,
                                              state.dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp,
                                              state.dataPlnt->PlantLoop(state.dataWaterCoils->WaterCoil(CoilNum).WaterLoopNum).FluidIndex,
                                              "InitWaterCoil");
            state.dataWaterCoils->WaterCoil(CoilNum).InletWaterEnthalpy = cp * state.dataWaterCoils->WaterCoil(CoilNum).InletWaterTemp;

            state.dataWaterCoils->WaterCoil(CoilNum).UACoilVariable = state.dataWaterCoils->WaterCoil(CoilNum).UACoil;
            state.dataWaterCoils->WaterCoil(CoilNum).FaultyCoilFoulingFactor = 0.0;
            Real64 holdOutBaroPress = state.dataEnvrn->OutBaroPress;
            state.dataEnvrn->OutBaroPress = DataEnvironment::StdPressureSeaLevel; // assume rating is for sea level.
            CalcAdjustedCoilUA(state, CoilNum);

            std::string coilTypeName(" ");
            // calculate coil sim model at rating point, full load, continuous fan
            if (state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType == DataPlant::TypeOf_CoilWaterDetailedFlatCooling) {
                CalcDetailFlatFinCoolingCoil(state, CoilNum, state.dataWaterCoils->SimCalc, ContFanCycCoil, 1.0);
                coilTypeName = "Coil:Cooling:Water:DetailedGeometry";
            } else if (state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType == DataPlant::TypeOf_CoilWaterCooling) {
                CoolingCoil(state, CoilNum, FirstHVACIteration, state.dataWaterCoils->SimCalc, ContFanCycCoil, 1.0);
                coilTypeName = "Coil:Cooling:Water";
            } else if (state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType == DataPlant::TypeOf_CoilWaterSimpleHeating) {
                CalcSimpleHeatingCoil(state, CoilNum, ContFanCycCoil, 1.0, state.dataWaterCoils->SimCalc);
                coilTypeName = "Coil:Heating:Water";
            }

            // coil outlets
            Real64 RatedOutletWetBulb(0.0);
            RatedOutletWetBulb = Psychrometrics::PsyTwbFnTdbWPb(state,
                                                                state.dataWaterCoils->WaterCoil(CoilNum).OutletAirTemp,
                                                                state.dataWaterCoils->WaterCoil(CoilNum).OutletAirHumRat,
                                                                DataEnvironment::StdPressureSeaLevel,
                                                                "InitWaterCoil");

            // call set routine in coil report
            if (state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType == DataPlant::TypeOf_CoilWaterDetailedFlatCooling ||
                state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType == DataPlant::TypeOf_CoilWaterCooling) {
                state.dataRptCoilSelection->coilSelectionReportObj->setRatedCoilConditions(
                    state,
                    state.dataWaterCoils->WaterCoil(CoilNum).Name,
                    coilTypeName,
                    state.dataWaterCoils->WaterCoil(CoilNum).TotWaterCoolingCoilRate, // this is the report variable
                    state.dataWaterCoils->WaterCoil(CoilNum).SenWaterCoolingCoilRate, // this is the report variable
                    state.dataWaterCoils->WaterCoil(CoilNum).InletAirMassFlowRate,
                    state.dataWaterCoils->WaterCoil(CoilNum).InletAirTemp,
                    state.dataWaterCoils->WaterCoil(CoilNum).InletAirHumRat,
                    DesInletWetBulb,
                    state.dataWaterCoils->WaterCoil(CoilNum).OutletAirTemp,
                    state.dataWaterCoils->WaterCoil(CoilNum).OutletAirHumRat,
                    RatedOutletWetBulb,
                    -999.0,
                    -999.0,
                    -999.0,
                    -999.0); // coil effectiveness
            } else if (state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType == DataPlant::TypeOf_CoilWaterSimpleHeating) {
                state.dataRptCoilSelection->coilSelectionReportObj->setRatedCoilConditions(
                    state,
                    state.dataWaterCoils->WaterCoil(CoilNum).Name,
                    coilTypeName,
                    state.dataWaterCoils->WaterCoil(CoilNum).TotWaterHeatingCoilRate, // this is the report variable
                    state.dataWaterCoils->WaterCoil(CoilNum).TotWaterHeatingCoilRate, // this is the report variable
                    state.dataWaterCoils->WaterCoil(CoilNum).InletAirMassFlowRate,
                    state.dataWaterCoils->WaterCoil(CoilNum).InletAirTemp,
                    state.dataWaterCoils->WaterCoil(CoilNum).InletAirHumRat,
                    DesInletWetBulb,
                    state.dataWaterCoils->WaterCoil(CoilNum).OutletAirTemp,
                    state.dataWaterCoils->WaterCoil(CoilNum).OutletAirHumRat,
                    RatedOutletWetBulb,
                    -999.0,
                    -999.0,
                    -999.0,
                    -999.0); // coil effectiveness
            }
            // now replace the outdoor air conditions set above for one time rating point calc
            state.dataEnvrn->OutBaroPress = holdOutBaroPress;
        }
    }

    // Do the Begin Day initializations
    // NONE

    // Do the begin HVAC time step initializations
    // NONE

    // Do the following initializations (every time step): This should be the info from
    // the previous components outlets or the node data in this section.
    // First set the conditions for the air into the coil model
    AirInletNode = state.dataWaterCoils->WaterCoil(CoilNum).AirInletNodeNum;
    WaterInletNode = state.dataWaterCoils->WaterCoil(CoilNum).WaterInletNodeNum;
    state.dataWaterCoils->WaterCoil(CoilNum).InletAirMassFlowRate = Node(AirInletNode).MassFlowRate;
    state.dataWaterCoils->WaterCoil(CoilNum).InletAirTemp = Node(AirInletNode).Temp;
    state.dataWaterCoils->WaterCoil(CoilNum).InletAirHumRat = Node(AirInletNode).HumRat;
    state.dataWaterCoils->WaterCoil(CoilNum).InletAirEnthalpy = Node(AirInletNode).Enthalpy;

    state.dataWaterCoils->WaterCoil(CoilNum).InletWaterMassFlowRate = Node(WaterInletNode).MassFlowRate;
    state.dataWaterCoils->WaterCoil(CoilNum).InletWaterTemp = Node(WaterInletNode).Temp;
    state.dataWaterCoils->WaterCoil(CoilNum).InletWaterEnthalpy = Node(WaterInletNode).Enthalpy;

    state.dataWaterCoils->WaterCoil(CoilNum).UACoilVariable = state.dataWaterCoils->WaterCoil(CoilNum).UACoil;

    CalcAdjustedCoilUA(state, CoilNum);

    state.dataWaterCoils->WaterCoil(CoilNum).TotWaterHeatingCoilRate = 0.0;
    state.dataWaterCoils->WaterCoil(CoilNum).TotWaterCoolingCoilRate = 0.0;
    state.dataWaterCoils->WaterCoil(CoilNum).SenWaterCoolingCoilRate = 0.0;
}

// refactor coilUA adjustment into separate routine, for use with rating calc
void CalcAdjustedCoilUA(EnergyPlusData &state, int const CoilNum)
{
    // Pull these precalc routines out of big init routine
    // modify the coil UA based on model in Wetter 1999
    Real64 x_a;                  // result of Eq.70 in Wetter 1999
    Real64 x_w;                  // result of Eq.72 in Wetter 1999
    Real64 AirConvectTerm;       // result of Eq.71 in Wetter 1999
    Real64 WaterConvectTerm;     // result of Eq.73 in Wetter 1999
    Real64 WaterConvSensitivity; // "s" in Wetter 1999, temperature sensitivity in water side convection

    // Coil:Heating:Water
    if ((state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType == DataPlant::TypeOf_CoilWaterSimpleHeating) &&
        (!(state.dataWaterCoils->MyUAAndFlowCalcFlag(CoilNum)))) { // update Coil UA based on inlet mass flows and temps
        x_a = 1.0 + 4.769E-3 * (state.dataWaterCoils->WaterCoil(CoilNum).InletAirTemp - state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirTemp);
        if (state.dataWaterCoils->WaterCoil(CoilNum).DesAirMassFlowRate > 0.0) {
            AirConvectTerm =
                x_a *
                std::pow(state.dataWaterCoils->WaterCoil(CoilNum).InletAirMassFlowRate / state.dataWaterCoils->WaterCoil(CoilNum).DesAirMassFlowRate,
                         0.8) *
                state.dataWaterCoils->WaterCoil(CoilNum).AirSideNominalConvect;
        } else {
            AirConvectTerm = 0.0;
        }
        WaterConvSensitivity = 0.014 / (1.0 + 0.014 * state.dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp);
        x_w = 1.0 + WaterConvSensitivity *
                        (state.dataWaterCoils->WaterCoil(CoilNum).InletWaterTemp - state.dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp);
        if (state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterMassFlowRate > 0.0) {
            WaterConvectTerm = x_w *
                               std::pow(state.dataWaterCoils->WaterCoil(CoilNum).InletWaterMassFlowRate /
                                            state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterMassFlowRate,
                                        0.85) *
                               state.dataWaterCoils->WaterCoil(CoilNum).LiquidSideNominalConvect;
        } else {
            WaterConvectTerm = 0.0;
        }
        if ((AirConvectTerm > 0.0) && (WaterConvectTerm > 0.0)) {
            state.dataWaterCoils->WaterCoil(CoilNum).UACoilVariable = 1.0 / ((1.0 / WaterConvectTerm) + (1.0 / AirConvectTerm));
        } else {
            // use nominal UA since variable UA cannot be calculated
            state.dataWaterCoils->WaterCoil(CoilNum).UACoilVariable = state.dataWaterCoils->WaterCoil(CoilNum).UACoil;
        }

        // calculate the Faulty Coil Fouling (thermal insulance) Factor using fault information
        if (state.dataWaterCoils->WaterCoil(CoilNum).FaultyCoilFoulingFlag &&
            // The fault shouldn't apply during sizing.
            (!state.dataGlobal->WarmupFlag) && (!state.dataGlobal->DoingSizing) && (!state.dataGlobal->KickOffSimulation) &&
            // This was preexisting
            !(state.dataWaterCoils->MyUAAndFlowCalcFlag(CoilNum))) {
            // Store original value
            state.dataWaterCoils->WaterCoil(CoilNum).OriginalUACoilVariable = state.dataWaterCoils->WaterCoil(CoilNum).UACoilVariable;

            int FaultIndex = state.dataWaterCoils->WaterCoil(CoilNum).FaultyCoilFoulingIndex;
            FaultsManager::FaultPropertiesFoulingCoil &fouling = state.dataFaultsMgr->FouledCoils(FaultIndex);
            Real64 FaultFrac = fouling.FaultFraction(state);

            if (fouling.FoulingInputMethod == FaultsManager::FouledCoil::UARated) {
                // 1/UA' = Frac * (1/UAFouled) + (1-Frac) / UA
                state.dataWaterCoils->WaterCoil(CoilNum).UACoilVariable =
                    1 / (FaultFrac / (fouling.UAFouled) + (1 - FaultFrac) / state.dataWaterCoils->WaterCoil(CoilNum).UACoilVariable);
            } else {
                // R' = R + Rfoul
                // Rfoul = r_air/A_air + r_water/A_water (FoulingFactor = thermal insulance [K/W, A] = Area [m2], r=fouling factor [m2.K/W]
                Real64 FoulingFactor = FaultFrac * (fouling.Rfw / (fouling.Aratio * fouling.Aout) + fouling.Rfa / fouling.Aout);
                state.dataWaterCoils->WaterCoil(CoilNum).UACoilVariable =
                    1.0 / ((1.0 / state.dataWaterCoils->WaterCoil(CoilNum).UACoilVariable) + FoulingFactor);
            }

            // Do not allow improving coil performance
            state.dataWaterCoils->WaterCoil(CoilNum).UACoilVariable =
                min(state.dataWaterCoils->WaterCoil(CoilNum).UACoilVariable, state.dataWaterCoils->WaterCoil(CoilNum).OriginalUACoilVariable);

            // Only for reporting purposes
            state.dataWaterCoils->WaterCoil(CoilNum).FaultyCoilFoulingFactor =
                (1.0 / state.dataWaterCoils->WaterCoil(CoilNum).UACoilVariable) -
                (1.0 / state.dataWaterCoils->WaterCoil(CoilNum).OriginalUACoilVariable);
        } else {
            state.dataWaterCoils->WaterCoil(CoilNum).FaultyCoilFoulingFactor = 0;
        }
    }

    // Coil:Cooling:Water
    // update Coil UA based on inlet mass flows and temps
    if (state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType == DataPlant::TypeOf_CoilWaterCooling &&
        (!state.dataWaterCoils->MyCoilDesignFlag(CoilNum))) {
        if (state.dataWaterCoils->WaterCoil(CoilNum).DesAirMassFlowRate > 0.0) {
            x_a = 1.0 + 4.769E-3 * (state.dataWaterCoils->WaterCoil(CoilNum).InletAirTemp - state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirTemp);
            state.dataWaterCoils->WaterCoil(CoilNum).UACoilExternal =
                x_a *
                std::pow(state.dataWaterCoils->WaterCoil(CoilNum).InletAirMassFlowRate / state.dataWaterCoils->WaterCoil(CoilNum).DesAirMassFlowRate,
                         0.8) *
                state.dataWaterCoils->WaterCoil(CoilNum).UACoilExternalDes;
        } else {
            state.dataWaterCoils->WaterCoil(CoilNum).UACoilExternal = state.dataWaterCoils->WaterCoil(CoilNum).UACoilExternalDes;
        }

        if (state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterMassFlowRate > 0.0) {
            WaterConvSensitivity = 0.014 / (1.0 + 0.014 * state.dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp);
            x_w = 1.0 + WaterConvSensitivity *
                            (state.dataWaterCoils->WaterCoil(CoilNum).InletWaterTemp - state.dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp);
            state.dataWaterCoils->WaterCoil(CoilNum).UACoilInternal = x_w *
                                                                      std::pow(state.dataWaterCoils->WaterCoil(CoilNum).InletWaterMassFlowRate /
                                                                                   state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterMassFlowRate,
                                                                               0.85) *
                                                                      state.dataWaterCoils->WaterCoil(CoilNum).UACoilInternalDes;
        } else {
            state.dataWaterCoils->WaterCoil(CoilNum).UACoilInternal = state.dataWaterCoils->WaterCoil(CoilNum).UACoilInternalDes;
        }

        if (!(state.dataWaterCoils->WaterCoil(CoilNum).UACoilInternal > 0.0 && state.dataWaterCoils->WaterCoil(CoilNum).UACoilExternal > 0.0)) {
            state.dataWaterCoils->WaterCoil(CoilNum).UACoilInternal = state.dataWaterCoils->WaterCoil(CoilNum).UACoilInternalDes;
            state.dataWaterCoils->WaterCoil(CoilNum).UACoilExternal = state.dataWaterCoils->WaterCoil(CoilNum).UACoilExternalDes;
        }

        // If Fouling
        if (state.dataWaterCoils->WaterCoil(CoilNum).FaultyCoilFoulingFlag &&
            // The fault shouldn't apply during sizing.
            (!state.dataGlobal->WarmupFlag) && (!state.dataGlobal->DoingSizing) && (!state.dataGlobal->KickOffSimulation) &&
            // This was preexisting
            !(state.dataWaterCoils->MyUAAndFlowCalcFlag(CoilNum))) {
            // Store original value
            // This is really UACoilTotal technically, but I don't see the point of declaring another Real on the struct just for that
            state.dataWaterCoils->WaterCoil(CoilNum).OriginalUACoilVariable =
                1.0 / (1.0 / state.dataWaterCoils->WaterCoil(CoilNum).UACoilExternal + 1.0 / state.dataWaterCoils->WaterCoil(CoilNum).UACoilInternal);

            state.dataWaterCoils->WaterCoil(CoilNum).OriginalUACoilExternal = state.dataWaterCoils->WaterCoil(CoilNum).UACoilExternal;
            state.dataWaterCoils->WaterCoil(CoilNum).OriginalUACoilInternal = state.dataWaterCoils->WaterCoil(CoilNum).UACoilInternal;

            int FaultIndex = state.dataWaterCoils->WaterCoil(CoilNum).FaultyCoilFoulingIndex;

            FaultsManager::FaultPropertiesFoulingCoil &fouling = state.dataFaultsMgr->FouledCoils(FaultIndex);
            Real64 FaultFrac = fouling.FaultFraction(state);

            if (fouling.FoulingInputMethod == FaultsManager::FouledCoil::FoulingFactor) {
                // Adjust the External (air) UA and Internal (water) UA accordingly
                Real64 Rfoul_air = FaultFrac * (fouling.Rfa / fouling.Aout);
                Real64 Rfoul_water = FaultFrac * (fouling.Rfw / (fouling.Aratio * fouling.Aout));

                state.dataWaterCoils->WaterCoil(CoilNum).UACoilInternal =
                    1.0 / (1.0 / state.dataWaterCoils->WaterCoil(CoilNum).UACoilInternal + Rfoul_water);
                state.dataWaterCoils->WaterCoil(CoilNum).UACoilExternal =
                    1.0 / (1.0 / state.dataWaterCoils->WaterCoil(CoilNum).UACoilExternal + Rfoul_air);
                //
            } else { // iFouledCoil_UARated
                // FouledUARated is supposed to be the overall UA. So we need to split between Internal and External UAs

                // How should I split fouling between internal/external?
                // We can actually use the current ratio before fouling...
                // splitRatio = UACoilInternal/UACoilExternal
                // UACoilInternal = UACoilExternal * splitRatio

                // UACoilTotal = 1 / (1 / UACoilExternal + 1 / UACoilInternal)
                // UACoilTotal = 1 / (1 / UACoilExternal + 1 / (UACoilExernal * splitRatio))
                // UACoilTotal = UACoilExternal / (1 + 1 / splitRatio) = UACoilExternal  * splitRatio / (1 + splitRatio)
                // UACoilExternal = UACoilTotal * (1 + splitRatio) / splitRatio
                // UACoilInternal = UACoilTotal * (1 + splitRatio)

                // Adding in FaultFrac:
                // UACoilExternal = FaultFrac * [UAFouled * (1+splitRatio) / splitRatio] + (1-FaultFrac) * UACoilExternal
                // UACoilInternal = FaultFrac * [UAFouled * splitRatio] + (1-FaultFrac) * UACoilInternal

                Real64 splitRatio = state.dataWaterCoils->WaterCoil(CoilNum).UACoilInternal / state.dataWaterCoils->WaterCoil(CoilNum).UACoilExternal;

                state.dataWaterCoils->WaterCoil(CoilNum).UACoilExternal =
                    1.0 / ((FaultFrac * splitRatio) / ((1 + splitRatio) * fouling.UAFouled) +
                           (1 - FaultFrac) / state.dataWaterCoils->WaterCoil(CoilNum).UACoilExternal);

                // WaterCoil(CoilNum).UACoilInternal =   1.0 /
                //( FaultFrac / ((1 + splitRatio) * fouling.UAFouled) +
                //(1-FaultFrac) / WaterCoil(CoilNum).UACoilInternal);

                state.dataWaterCoils->WaterCoil(CoilNum).UACoilInternal = splitRatio * state.dataWaterCoils->WaterCoil(CoilNum).UACoilExternal;
            }

            // Do not allow improving coil performance
            state.dataWaterCoils->WaterCoil(CoilNum).UACoilExternal =
                min(state.dataWaterCoils->WaterCoil(CoilNum).UACoilExternal, state.dataWaterCoils->WaterCoil(CoilNum).OriginalUACoilExternal);
            state.dataWaterCoils->WaterCoil(CoilNum).UACoilInternal =
                min(state.dataWaterCoils->WaterCoil(CoilNum).UACoilInternal, state.dataWaterCoils->WaterCoil(CoilNum).OriginalUACoilInternal);

            // Only for reporting purposes
            state.dataWaterCoils->WaterCoil(CoilNum).FaultyCoilFoulingFactor =
                (1.0 / state.dataWaterCoils->WaterCoil(CoilNum).UACoilExternal) -
                (1.0 / state.dataWaterCoils->WaterCoil(CoilNum).OriginalUACoilExternal) +
                (1.0 / state.dataWaterCoils->WaterCoil(CoilNum).UACoilInternal) -
                (1.0 / state.dataWaterCoils->WaterCoil(CoilNum).OriginalUACoilInternal);
        } else {
            state.dataWaterCoils->WaterCoil(CoilNum).FaultyCoilFoulingFactor = 0;
        }

        state.dataWaterCoils->WaterCoil(CoilNum).UACoilTotal =
            1.0 / (1.0 / state.dataWaterCoils->WaterCoil(CoilNum).UACoilExternal + 1.0 / state.dataWaterCoils->WaterCoil(CoilNum).UACoilInternal);

        state.dataWaterCoils->WaterCoil(CoilNum).UACoilInternalPerUnitArea =
            state.dataWaterCoils->WaterCoil(CoilNum).UACoilInternal / state.dataWaterCoils->WaterCoil(CoilNum).TotCoilOutsideSurfArea;
        state.dataWaterCoils->WaterCoil(CoilNum).UAWetExtPerUnitArea =
            state.dataWaterCoils->WaterCoil(CoilNum).UACoilExternal / state.dataWaterCoils->WaterCoil(CoilNum).TotCoilOutsideSurfArea;
        state.dataWaterCoils->WaterCoil(CoilNum).UADryExtPerUnitArea = state.dataWaterCoils->WaterCoil(CoilNum).UAWetExtPerUnitArea;
    }
}

void SizeWaterCoil(EnergyPlusData &state, int const CoilNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   November 2001
    //       MODIFIED       August 2013 Daeho Kang, add component sizing table entries
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine is for sizing Water Coil Components for which flow rates and UAs have not been
    // specified in the input.

    // METHODOLOGY EMPLOYED:
    // Obtains flow rates from the zone or system sizing arrays and plant sizing data. UAs are
    // calculated by numerically inverting the individual coil calculation routines.

    // Using/Aliasing
    using namespace DataSizing;
    using PlantUtilities::RegisterPlantCompDesignFlow;

    // SUBROUTINE PARAMETER DEFINITIONS:
    static constexpr std::string_view InitWaterCoil("InitWaterCoil");
    static constexpr std::string_view RoutineName("SizeWaterCoil");

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 rho;
    int FieldNum = 2;                      // IDD numeric field number where input field description is found
    std::string CompType;                  // component type
    int SizingType;                        // type of sizing to perform
    std::string SizingString;              // input field sizing description (e.g., Nominal Capacity)
    bool bPRINT = true;                    // TRUE if sizing is reported to output (eio)
    Real64 TempSize;                       // autosized value
    Real64 DesCoilWaterInTempSaved;        // coil water inlet temp used for error checking UA sizing
    Real64 DesCoilInletWaterTempUsed(0.0); // coil design inlet water temp for UA sizing only
    Real64 Cp;
    bool NomCapUserInp = false; // flag for whether user has onput a nominal heating capacity

    bool ErrorsFound = false;
    bool LoopErrorsFound = false;
    int PltSizCoolNum = 0;
    int PltSizHeatNum = 0;
    Real64 DesCoilAirFlow = 0.0;
    Real64 DesCoilExitTemp = 0.0;
    Real64 CpAirStd = PsyCpAirFnW(0.0);
    std::string CompName = state.dataWaterCoils->WaterCoil(CoilNum).Name;

    auto &ZoneEqSizing(state.dataSize->ZoneEqSizing);
    auto &OASysEqSizing(state.dataSize->OASysEqSizing);

    // cooling coils
    if (((state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType == DataPlant::TypeOf_CoilWaterCooling) ||
         (state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType == DataPlant::TypeOf_CoilWaterDetailedFlatCooling)) &&
        state.dataWaterCoils->WaterCoil(CoilNum).RequestingAutoSize) {
        // find the appropriate Plant Sizing object
        PltSizCoolNum = PlantUtilities::MyPlantSizingIndex(state,
                                                           "chilled water coil",
                                                           state.dataWaterCoils->WaterCoil(CoilNum).Name,
                                                           state.dataWaterCoils->WaterCoil(CoilNum).WaterInletNodeNum,
                                                           state.dataWaterCoils->WaterCoil(CoilNum).WaterOutletNodeNum,
                                                           LoopErrorsFound);
    }

    if (((state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType == DataPlant::TypeOf_CoilWaterCooling) ||
         (state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType == DataPlant::TypeOf_CoilWaterDetailedFlatCooling))) { // 'Cooling'

        if (state.dataWaterCoils->WaterCoil(CoilNum).UseDesignWaterDeltaTemp) {
            state.dataSize->DataWaterCoilSizCoolDeltaT = state.dataWaterCoils->WaterCoil(CoilNum).DesignWaterDeltaTemp;
        } else {
            if (PltSizCoolNum > 0) {
                state.dataSize->DataWaterCoilSizCoolDeltaT = state.dataSize->PlantSizData(PltSizCoolNum).DeltaT;
            }
        }

        if (PltSizCoolNum > 0) {

            state.dataSize->DataPltSizCoolNum = PltSizCoolNum;
            state.dataSize->DataWaterLoopNum = state.dataWaterCoils->WaterCoil(CoilNum).WaterLoopNum;

            if (state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilModel == iCoilModel::CoolingDetailed) { // 'DETAILED FLAT FIN'
                CompType = cAllCoilTypes(Coil_CoolingWaterDetailed);                                      // Coil:Cooling:Water:DetailedGeometry
            } else {
                CompType = cAllCoilTypes(Coil_CoolingWater); // Coil:Cooling:Water
            }

            bPRINT = false; // do not print this sizing request since the autosized value is needed and this input may not be autosized (we should
                            // print this!)
            if (state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate == state.dataSize->DataFlowUsedForSizing) {
                TempSize = state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate; // represents parent object has hard-sized airflow
            } else {
                TempSize = AutoSize; // get the autosized air volume flow rate for use in other calculations
            }

            bool errorsFound = false;
            CoolingAirFlowSizer sizingCoolingAirFlow;
            CompName = state.dataWaterCoils->WaterCoil(CoilNum).Name;
            sizingCoolingAirFlow.initializeWithinEP(state, CompType, CompName, bPRINT, RoutineName);
            Real64 autoSizedValue = sizingCoolingAirFlow.size(state, TempSize, errorsFound);
            state.dataWaterCoils->WaterCoil(CoilNum).InletAirMassFlowRate =
                state.dataEnvrn->StdRhoAir * autoSizedValue; // inlet air mass flow rate is the autosized value

            // Check if the air volume flow rate is defined in parent HVAC equipment and set water coil design air volume flow rate accordingly
            if (state.dataSize->CurZoneEqNum > 0) {
                if (ZoneEqSizing(state.dataSize->CurZoneEqNum).DesignSizeFromParent &&
                    state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate == autoSizedValue) {
                    state.dataSize->DataAirFlowUsedForSizing = ZoneEqSizing(state.dataSize->CurZoneEqNum).AirVolFlow;
                    state.dataSize->DataFlowUsedForSizing = ZoneEqSizing(state.dataSize->CurZoneEqNum).AirVolFlow;
                    state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate = AutoSize; // represents water coil being autosized
                } else {
                    state.dataSize->DataAirFlowUsedForSizing =
                        autoSizedValue; // many autosized inputs use the design (autosized) air volume flow rate, save this value
                    state.dataSize->DataFlowUsedForSizing = autoSizedValue;
                }
            } else {
                state.dataSize->DataAirFlowUsedForSizing =
                    autoSizedValue; // many autosized inputs use the design (autosized) air volume flow rate, save this value
                state.dataSize->DataFlowUsedForSizing = autoSizedValue;
            }

            if (state.dataSize->CurSysNum > 0 && state.dataSize->CurOASysNum == 0) {
                Real64 DesCoilExitHumRat(0.0); // fix coil sizing inconsistency
                DataSizing::GetCoilDesFlowT(state, state.dataSize->CurSysNum, CpAirStd, DesCoilAirFlow, DesCoilExitTemp, DesCoilExitHumRat);
                state.dataSize->DataAirFlowUsedForSizing = DesCoilAirFlow;
                state.dataSize->DataFlowUsedForSizing = DesCoilAirFlow;
                state.dataSize->DataDesOutletAirTemp = DesCoilExitTemp;
                state.dataSize->DataDesOutletAirHumRat = DesCoilExitHumRat; // need to test for dry coil but inlet conditions not yet known
            }

            // calculate pre-sizing data needed for specific functions (e.g., CoolingWaterDesAirInletTempSizing needs HRin and air flow)
            // these will be calculated again after other parameters are known
            if (state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilModel == iCoilModel::CoolingDetailed) { // 'DETAILED FLAT FIN'
                TempSize = AutoSize;                                                                      // coil report
            } else {
                TempSize = state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirHumRat; // preserve input if entered
            }
            CoolingWaterDesAirInletHumRatSizer sizerCWDesInHumRat;
            sizerCWDesInHumRat.initializeWithinEP(state, CompType, CompName, bPRINT, RoutineName);
            state.dataSize->DataDesInletAirHumRat = sizerCWDesInHumRat.size(state, TempSize, ErrorsFound);

            TempSize = AutoSize;
            CoolingCapacitySizer sizerCoolingCapacity;
            sizerCoolingCapacity.overrideSizingString(SizingString);
            sizerCoolingCapacity.initializeWithinEP(state, CompType, CompName, bPRINT, RoutineName);
            state.dataSize->DataCapacityUsedForSizing = sizerCoolingCapacity.size(state, TempSize, ErrorsFound);
            TempSize = state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate;
            CoolingWaterflowSizer sizerCWWaterflow;
            sizerCWWaterflow.initializeWithinEP(state, CompType, CompName, bPRINT, RoutineName);
            Real64 autoSizedCWFlow = sizerCWWaterflow.size(state, TempSize, ErrorsFound);
            // Check if the water flow rate is defined in parent HVAC equipment and set water coil design water flow rate accordingly
            if (state.dataSize->CurZoneEqNum > 0) {
                if (ZoneEqSizing(state.dataSize->CurZoneEqNum).DesignSizeFromParent) {
                    state.dataSize->DataWaterFlowUsedForSizing = ZoneEqSizing(state.dataSize->CurZoneEqNum).MaxCWVolFlow;
                } else {
                    state.dataSize->DataWaterFlowUsedForSizing = autoSizedCWFlow;
                }
            } else {
                state.dataSize->DataWaterFlowUsedForSizing = autoSizedCWFlow;
            }
            // end pre-sizing data calculations

            if (state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilModel == iCoilModel::CoolingDetailed) { // 'DETAILED FLAT FIN'
                bPRINT = false;       // do not print this sizing request since this coil does not have a design inlet air temp input field (we
                                      // should print this!)
                TempSize = AutoSize;  // not an input for this model
                SizingString.clear(); // doesn't matter
            } else {
                FieldNum = 4; //  N4 , \field Design Inlet Air Temperature
                bPRINT = true;
                TempSize = state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirTemp; // preserve input if entered
                SizingString = state.dataWaterCoils->WaterCoilNumericFields(CoilNum).FieldNames(FieldNum) + " [C]";
            }

            CoolingWaterDesAirInletTempSizer sizerCWDesInletAirTemp;
            sizerCWDesInletAirTemp.initializeWithinEP(state, CompType, CompName, bPRINT, RoutineName);
            state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirTemp = sizerCWDesInletAirTemp.size(state, TempSize, ErrorsFound);
            state.dataSize->DataDesInletAirTemp = state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirTemp;

            if (state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilModel == iCoilModel::CoolingDetailed) { // 'DETAILED FLAT FIN'
                bPRINT = false;       // no field for detailed water coil, should print to eio anyway
                TempSize = AutoSize;  // coil report
                SizingString.clear(); // doesn't matter
            } else {
                FieldNum = 3; //  N3 , \field Design Inlet Water Temperature
                bPRINT = true;
                TempSize = state.dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp; // preserve input if entered
                SizingString = state.dataWaterCoils->WaterCoilNumericFields(CoilNum).FieldNames(FieldNum) + " [C]";
            }
            CoolingWaterDesWaterInletTempSizer sizerCWDesWaterInTemp;
            sizerCWDesWaterInTemp.initializeWithinEP(state, CompType, CompName, bPRINT, RoutineName);
            state.dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp = sizerCWDesWaterInTemp.size(state, TempSize, ErrorsFound);

            if (state.dataSize->CurZoneEqNum > 0) { // zone equipment use air inlet humrat to calculate design outlet air temperature
                if (state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilModel == iCoilModel::CoolingDetailed) { // 'DETAILED FLAT FIN'
                    bPRINT = false;      // no field for detailed water coil, should print to eio anyway
                    TempSize = AutoSize; // coil report
                } else {
                    bPRINT = true;
                    TempSize = state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirHumRat; // preserve input if entered
                }
                sizerCWDesInHumRat.initializeWithinEP(state, CompType, CompName, bPRINT, RoutineName);
                state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirHumRat = sizerCWDesInHumRat.size(state, TempSize, ErrorsFound);
            }

            if (state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilModel == iCoilModel::CoolingDetailed) { // 'DETAILED FLAT FIN'
                bPRINT = false;       // no field for detailed water coil, should print to eio anyway
                TempSize = AutoSize;  // coil report
                SizingString.clear(); // doesn't matter
            } else {
                FieldNum = 5; //  N5 , \field Design Outlet Air Temperature
                bPRINT = true;
                TempSize = state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirTemp; // preserve input if entered
                SizingString = state.dataWaterCoils->WaterCoilNumericFields(CoilNum).FieldNames(FieldNum) + " [C]";
            }

            state.dataSize->DataDesInletWaterTemp = state.dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp; // used for warning messages
            CoolingWaterDesAirOutletTempSizer sizerCWDesAirOutTemp;
            sizerCWDesAirOutTemp.initializeWithinEP(state, CompType, CompName, bPRINT, RoutineName);
            state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirTemp = sizerCWDesAirOutTemp.size(state, TempSize, ErrorsFound);
            state.dataSize->DataDesOutletAirTemp = state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirTemp;

            if (state.dataSize->CurSysNum > 0) { // This call can be deleted at a future time and remove the if ( CurZoneEqNum > 0 ) check above. This
                                                 // will change the order of the eio file.
                if (state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilModel == iCoilModel::CoolingDetailed) { // 'DETAILED FLAT FIN'
                    bPRINT = false;      // no field for detailed water coil, should print this to eio anyway
                    TempSize = AutoSize; // coil report
                } else {
                    bPRINT = true;
                    TempSize = state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirHumRat;
                }
                sizerCWDesInHumRat.initializeWithinEP(state, CompType, CompName, bPRINT, RoutineName);
                state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirHumRat = sizerCWDesInHumRat.size(state, TempSize, ErrorsFound);
            }

            if (state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilModel == iCoilModel::CoolingDetailed) { // 'DETAILED FLAT FIN'
                bPRINT = false;      // no field for detailed water coil, should print this to eio anyway
                TempSize = AutoSize; // coil report
            } else {
                bPRINT = true;
                TempSize = state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirHumRat; // preserve input if entered
            }
            CoolingWaterDesAirOutletHumRatSizer sizerCWDesOutHumRat;
            sizerCWDesOutHumRat.initializeWithinEP(state, CompType, CompName, bPRINT, RoutineName);
            state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirHumRat = sizerCWDesOutHumRat.size(state, TempSize, ErrorsFound);
            state.dataSize->DataDesOutletAirHumRat = state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirHumRat;

            TempSize = AutoSize;
            bPRINT = true;
            if (state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate != AutoSize) bPRINT = false;
            if (state.dataSize->CurSysNum == 0) bPRINT = false;
            SizingString = "Design Coil Load [W]"; // there is no input field for this value and this is not the rated capacity (we should
                                                   // always print this!)
            // air inlet/outlet conditions should be known. Don't include fan heat in capacity calculation.
            state.dataSize->DataDesAccountForFanHeat = false;
            CoolingCapacitySizer sizerCoolingCapacity2;
            sizerCoolingCapacity2.overrideSizingString(SizingString);
            sizerCoolingCapacity2.initializeWithinEP(state, CompType, CompName, bPRINT, RoutineName);
            state.dataWaterCoils->WaterCoil(CoilNum).DesWaterCoolingCoilRate = sizerCoolingCapacity2.size(state, TempSize, ErrorsFound);
            state.dataWaterCoils->WaterCoil(CoilNum).InletAirMassFlowRate =
                state.dataEnvrn->StdRhoAir * state.dataSize->DataFlowUsedForSizing; // inlet air mass flow rate is the autosized value
            state.dataSize->DataCapacityUsedForSizing = state.dataWaterCoils->WaterCoil(CoilNum).DesWaterCoolingCoilRate;

            // Why isn't the water volume flow rate based on the user inputs for inlet/outlet air/water temps? Water volume flow rate is
            // always based on autosized inputs.
            bPRINT = true;
            TempSize = state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate;
            sizerCWWaterflow.initializeWithinEP(state, CompType, CompName, bPRINT, RoutineName);
            state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate = sizerCWWaterflow.size(state, TempSize, ErrorsFound);
            state.dataSize->DataWaterFlowUsedForSizing = state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate;

            if (state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilModel == iCoilModel::CoolingDetailed) { // 'DETAILED FLAT FIN'
                bPRINT = false; // do not print this sizing request since this coil does not have a design air flow rate input field (we
                                // should print this!)
            } else {
                bPRINT = true;
            }
            TempSize = state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate;
            CoolingAirFlowSizer sizingCoolingAirFlow2;
            std::string stringOverride = "Design Air Flow Rate [m3/s]";
            if (state.dataGlobal->isEpJSON) stringOverride = "design_air_flow_rate [m3/s]";
            sizingCoolingAirFlow2.overrideSizingString(stringOverride);
            // sizingCoolingAirFlow2.setHVACSizingIndexData(FanCoil(FanCoilNum).HVACSizingIndex);
            sizingCoolingAirFlow2.initializeWithinEP(state, CompType, CompName, bPRINT, RoutineName);
            state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate = sizingCoolingAirFlow2.size(state, TempSize, errorsFound);
            state.dataWaterCoils->WaterCoil(CoilNum).DesAirMassFlowRate =
                state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate * state.dataEnvrn->StdRhoAir;

            if (state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate <= 0.0) {
                state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate = 0.0;
                ShowWarningError(state, "The design air flow rate is zero for Coil:Cooling:Water " + state.dataWaterCoils->WaterCoil(CoilNum).Name);
                ShowContinueError(state, "The autosize value for max air volume flow rate is zero");
            }

            if (state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilModel == iCoilModel::CoolingDetailed) {

                FieldNum = 16; //  N16, \field Number of Tubes per Row
                bPRINT = true;
                SizingString = state.dataWaterCoils->WaterCoilNumericFields(CoilNum).FieldNames(FieldNum);
                // Auto size detailed cooling coil number of tubes per row = int( 13750.0 * WaterCoil( CoilNum ).MaxWaterVolFlowRate ) + 1
                state.dataSize->DataFlowUsedForSizing = state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate;
                TempSize = float(state.dataWaterCoils->WaterCoil(CoilNum).NumOfTubesPerRow);
                CoolingWaterNumofTubesPerRowSizer sizerCWNumofTubesPerRow;
                sizerCWNumofTubesPerRow.initializeWithinEP(state, CompType, CompName, bPRINT, RoutineName);
                state.dataWaterCoils->WaterCoil(CoilNum).NumOfTubesPerRow = sizerCWNumofTubesPerRow.size(state, TempSize, ErrorsFound);

                // Auto size water coil fin diameter = 0.335 * WaterCoil( CoilNum ).InletAirMassFlowRate
                state.dataSize->DataConstantUsedForSizing = state.dataWaterCoils->WaterCoil(CoilNum).InletAirMassFlowRate;
                state.dataSize->DataFractionUsedForSizing = 0.335;
                TempSize = state.dataWaterCoils->WaterCoil(CoilNum).FinDiam;

                AutoCalculateSizer sizerFinDiameter;
                std::string stringOverride = "Fin Diameter [m]";
                if (state.dataGlobal->isEpJSON) stringOverride = "fin_diameter [m]";
                sizerFinDiameter.overrideSizingString(stringOverride);
                sizerFinDiameter.initializeWithinEP(state, CompType, CompName, bPRINT, RoutineName);
                state.dataWaterCoils->WaterCoil(CoilNum).FinDiam = sizerFinDiameter.size(state, TempSize, ErrorsFound);

                // Auto size water coil minimum airflow area = 0.44 * WaterCoil( CoilNum ).InletAirMassFlowRate
                state.dataSize->DataConstantUsedForSizing = state.dataWaterCoils->WaterCoil(CoilNum).InletAirMassFlowRate;
                state.dataSize->DataFractionUsedForSizing = 0.44;
                TempSize = state.dataWaterCoils->WaterCoil(CoilNum).MinAirFlowArea;

                AutoCalculateSizer sizerMinAirFlowArea;
                stringOverride = "Minimum Airflow Area [m2]";
                if (state.dataGlobal->isEpJSON) stringOverride = "minimum_airflow_area [m2]";
                sizerMinAirFlowArea.overrideSizingString(stringOverride);
                sizerMinAirFlowArea.initializeWithinEP(state, CompType, CompName, bPRINT, RoutineName);
                state.dataWaterCoils->WaterCoil(CoilNum).MinAirFlowArea = sizerMinAirFlowArea.size(state, TempSize, ErrorsFound);

                if (state.dataWaterCoils->WaterCoil(CoilNum).MinAirFlowArea <= 0.0) {
                    ShowSevereError(state, "Coil:Cooling:Water:DetailedGeometry: \"" + state.dataWaterCoils->WaterCoil(CoilNum).Name + "\"");
                    ShowContinueError(state,
                                      format("Coil Minimum Airflow Area must be greater than 0. Coil area = {:.6T}",
                                             state.dataWaterCoils->WaterCoil(CoilNum).MinAirFlowArea));
                    ErrorsFound = true;
                }

                // Auto size water coil finned surface area = 78.5 * WaterCoil( CoilNum ).InletAirMassFlowRate
                state.dataSize->DataConstantUsedForSizing =
                    state.dataWaterCoils->WaterCoil(CoilNum)
                        .InletAirMassFlowRate; // actual autosized air mass flow rate, not calculated from user input
                state.dataSize->DataFractionUsedForSizing = 78.5;
                TempSize = state.dataWaterCoils->WaterCoil(CoilNum).FinSurfArea;

                AutoCalculateSizer sizerFinSurfaceArea;
                stringOverride = "Fin Surface Area [m2]";
                if (state.dataGlobal->isEpJSON) stringOverride = "fin_surface_area [m2]";
                sizerFinSurfaceArea.overrideSizingString(stringOverride);
                sizerFinSurfaceArea.initializeWithinEP(state, CompType, CompName, bPRINT, RoutineName);
                state.dataWaterCoils->WaterCoil(CoilNum).FinSurfArea = sizerFinSurfaceArea.size(state, TempSize, ErrorsFound);

                // Auto size water coil total tube inside surface area = 4.4 * WaterCoil( CoilNum ).TubeInsideDiam * WaterCoil( CoilNum
                // ).NumOfTubeRows * WaterCoil( CoilNum ).NumOfTubesPerRow
                state.dataSize->DataConstantUsedForSizing = state.dataWaterCoils->WaterCoil(CoilNum).TubeInsideDiam *
                                                            state.dataWaterCoils->WaterCoil(CoilNum).NumOfTubeRows *
                                                            state.dataWaterCoils->WaterCoil(CoilNum).NumOfTubesPerRow;
                state.dataSize->DataFractionUsedForSizing = 4.4;
                TempSize = state.dataWaterCoils->WaterCoil(CoilNum).TotTubeInsideArea;

                AutoCalculateSizer sizerTubeInsideArea;
                stringOverride = "Total Tube Inside Area [m2]";
                if (state.dataGlobal->isEpJSON) stringOverride = "total_tube_inside_area [m2]";
                sizerTubeInsideArea.overrideSizingString(stringOverride);
                sizerTubeInsideArea.initializeWithinEP(state, CompType, CompName, bPRINT, RoutineName);
                state.dataWaterCoils->WaterCoil(CoilNum).TotTubeInsideArea = sizerTubeInsideArea.size(state, TempSize, ErrorsFound);

                // Auto size water coil total tube outside surface area = 4.1 * WaterCoil( CoilNum ).TubeOutsideDiam * WaterCoil( CoilNum
                // ).NumOfTubeRows * WaterCoil( CoilNum ).NumOfTubesPerRow
                state.dataSize->DataConstantUsedForSizing = state.dataWaterCoils->WaterCoil(CoilNum).TubeOutsideDiam *
                                                            state.dataWaterCoils->WaterCoil(CoilNum).NumOfTubeRows *
                                                            state.dataWaterCoils->WaterCoil(CoilNum).NumOfTubesPerRow;
                state.dataSize->DataFractionUsedForSizing = 4.1;
                TempSize = state.dataWaterCoils->WaterCoil(CoilNum).TubeOutsideSurfArea;

                AutoCalculateSizer sizerTubeOutsideArea;
                stringOverride = "Tube Outside Surface Area [m2]";
                if (state.dataGlobal->isEpJSON) stringOverride = "tube_outside_surface_area [m2]";
                sizerTubeOutsideArea.overrideSizingString(stringOverride);
                sizerTubeOutsideArea.initializeWithinEP(state, CompType, CompName, bPRINT, RoutineName);
                state.dataWaterCoils->WaterCoil(CoilNum).TubeOutsideSurfArea = sizerTubeOutsideArea.size(state, TempSize, ErrorsFound);

                if ((state.dataWaterCoils->WaterCoil(CoilNum).FinSurfArea + state.dataWaterCoils->WaterCoil(CoilNum).TubeOutsideSurfArea) <= 0.0) {
                    ShowSevereError(state, "Coil:Cooling:Water:DetailedGeometry: \"" + state.dataWaterCoils->WaterCoil(CoilNum).Name + "\"");
                    ShowContinueError(
                        state,
                        format(
                            "Coil Fin Surface Area plus Coil Tube Outside Surface Area must be greater than 0. Total surface area = {:.6T}",
                            (state.dataWaterCoils->WaterCoil(CoilNum).FinSurfArea + state.dataWaterCoils->WaterCoil(CoilNum).TubeOutsideSurfArea)));
                    ErrorsFound = true;
                }

                // Auto size water coil coil depth = WaterCoil( CoilNum ).TubeDepthSpacing * WaterCoil( CoilNum ).NumOfTubeRows
                state.dataSize->DataConstantUsedForSizing = state.dataWaterCoils->WaterCoil(CoilNum).TubeDepthSpacing;
                state.dataSize->DataFractionUsedForSizing = state.dataWaterCoils->WaterCoil(CoilNum).NumOfTubeRows;
                TempSize = state.dataWaterCoils->WaterCoil(CoilNum).CoilDepth;

                AutoCalculateSizer sizerCoilDepth;
                stringOverride = "Coil Depth [m]";
                if (state.dataGlobal->isEpJSON) stringOverride = "coil_depth [m]";
                sizerCoilDepth.overrideSizingString(stringOverride);
                sizerCoilDepth.initializeWithinEP(state, CompType, CompName, bPRINT, RoutineName);
                state.dataWaterCoils->WaterCoil(CoilNum).CoilDepth = sizerCoilDepth.size(state, TempSize, ErrorsFound);
            }
            state.dataSize->DataPltSizCoolNum = 0; // reset all globals to 0 to ensure correct sizing for other child components
            state.dataSize->DataWaterLoopNum = 0;
            state.dataSize->DataConstantUsedForSizing = 0.0;
            state.dataSize->DataFractionUsedForSizing = 0.0;
            state.dataSize->DataAirFlowUsedForSizing = 0.0;
            state.dataSize->DataFlowUsedForSizing = 0.0;
            state.dataSize->DataWaterFlowUsedForSizing = 0.0;
            state.dataSize->DataCapacityUsedForSizing = 0.0;
            state.dataSize->DataDesInletAirTemp = 0.0;
            state.dataSize->DataDesOutletAirTemp = 0.0;
            state.dataSize->DataDesOutletAirHumRat = 0.0;
            state.dataSize->DataDesInletAirHumRat = 0.0;
            state.dataSize->DataDesInletWaterTemp = 0.0;
            state.dataSize->DataWaterCoilSizCoolDeltaT = 0.0;
            state.dataSize->DataDesAccountForFanHeat = true;
        } else {
            // If there is no cooling Plant Sizing object and autosizing was requested, issue fatal error message
            if (state.dataWaterCoils->WaterCoil(CoilNum).RequestingAutoSize) {
                ShowSevereError(state, "Autosizing of water coil requires a cooling loop Sizing:Plant object");
                ShowContinueError(state, "Occurs in water coil object= " + state.dataWaterCoils->WaterCoil(CoilNum).Name);
                ErrorsFound = true;
            }
        }
        //} // end of cooling Plant Sizing existence IF - ELSE
    } // end cooling coil IF

    // if this is a heating coil
    if (state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType == DataPlant::TypeOf_CoilWaterSimpleHeating &&
        state.dataWaterCoils->WaterCoil(CoilNum).RequestingAutoSize) {
        // find the appropriate heating Plant Sizing object
        PltSizHeatNum = PlantUtilities::MyPlantSizingIndex(state,
                                                           "hot water coil",
                                                           state.dataWaterCoils->WaterCoil(CoilNum).Name,
                                                           state.dataWaterCoils->WaterCoil(CoilNum).WaterInletNodeNum,
                                                           state.dataWaterCoils->WaterCoil(CoilNum).WaterOutletNodeNum,
                                                           LoopErrorsFound);
    }

    if (state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType == DataPlant::TypeOf_CoilWaterSimpleHeating) {

        if (state.dataWaterCoils->WaterCoil(CoilNum).UseDesignWaterDeltaTemp) {
            // use water design deltaT specified in the heating water coils
            state.dataSize->DataWaterCoilSizHeatDeltaT = state.dataWaterCoils->WaterCoil(CoilNum).DesignWaterDeltaTemp;
        } else {
            if (PltSizHeatNum > 0) {
                state.dataSize->DataWaterCoilSizHeatDeltaT = state.dataSize->PlantSizData(PltSizHeatNum).DeltaT;
            }
        }

        if (PltSizHeatNum > 0) {

            state.dataSize->DataPltSizHeatNum = PltSizHeatNum;
            state.dataSize->DataWaterLoopNum = state.dataWaterCoils->WaterCoil(CoilNum).WaterLoopNum;
            rho = GetDensityGlycol(state,
                                   state.dataPlnt->PlantLoop(state.dataWaterCoils->WaterCoil(CoilNum).WaterLoopNum).FluidName,
                                   DataGlobalConstants::HWInitConvTemp,
                                   state.dataPlnt->PlantLoop(state.dataWaterCoils->WaterCoil(CoilNum).WaterLoopNum).FluidIndex,
                                   RoutineName);
            Cp = GetSpecificHeatGlycol(state,
                                       state.dataPlnt->PlantLoop(state.dataSize->DataWaterLoopNum).FluidName,
                                       DataGlobalConstants::HWInitConvTemp,
                                       state.dataPlnt->PlantLoop(state.dataSize->DataWaterLoopNum).FluidIndex,
                                       RoutineName);
            if (state.dataWaterCoils->WaterCoil(CoilNum).DesTotWaterCoilLoad > 0.0) {
                NomCapUserInp = true;
            } else if (state.dataSize->CurSysNum > 0 && state.dataSize->CurSysNum <= state.dataHVACGlobal->NumPrimaryAirSys) {
                if (state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).HeatingCapMethod == CapacityPerFloorArea) {
                    NomCapUserInp = true;
                } else if (state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).HeatingCapMethod == HeatingDesignCapacity &&
                           state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).HeatingTotalCapacity > 0.0) {
                    NomCapUserInp = true;
                }
            } else {
                NomCapUserInp = false;
            }
            bPRINT = false;                              // do not print this sizing request
            TempSize = AutoSize;                         // get the autosized air volume flow rate for use in other calculations
            SizingString.clear();                        // doesn't matter
            CompType = cAllCoilTypes(Coil_HeatingWater); // "Coil:Heating:Water"
            CompName = state.dataWaterCoils->WaterCoil(CoilNum).Name;
            if (state.dataWaterCoils->WaterCoil(CoilNum).DesiccantRegenerationCoil) {
                state.dataSize->DataDesicRegCoil = true;
                state.dataSize->DataDesicDehumNum = state.dataWaterCoils->WaterCoil(CoilNum).DesiccantDehumNum;
                HeatingCoilDesAirInletTempSizer sizerHeatingDesInletTemp;
                bool ErrorsFound = false;
                sizerHeatingDesInletTemp.initializeWithinEP(state, CompType, CompName, bPRINT, RoutineName);
                state.dataSize->DataDesInletAirTemp = sizerHeatingDesInletTemp.size(state, DataSizing::AutoSize, ErrorsFound);

                HeatingCoilDesAirOutletTempSizer sizerHeatingDesOutletTemp;
                ErrorsFound = false;
                sizerHeatingDesOutletTemp.initializeWithinEP(state, CompType, CompName, bPRINT, RoutineName);
                state.dataSize->DataDesOutletAirTemp = sizerHeatingDesOutletTemp.size(state, DataSizing::AutoSize, ErrorsFound);

                if (state.dataSize->CurOASysNum > 0) {
                    OASysEqSizing(state.dataSize->CurOASysNum).AirFlow = true;
                    OASysEqSizing(state.dataSize->CurOASysNum).AirVolFlow =
                        state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).DesOutAirVolFlow;
                }
                TempSize = AutoSize; // reset back
            }
            bool errorsFound = false;
            HeatingAirFlowSizer sizingHeatingAirFlow;
            sizingHeatingAirFlow.overrideSizingString(SizingString);
            // sizingHeatingAirFlow.setHVACSizingIndexData(FanCoil(FanCoilNum).HVACSizingIndex);
            sizingHeatingAirFlow.initializeWithinEP(state, CompType, CompName, bPRINT, RoutineName);
            TempSize = sizingHeatingAirFlow.size(state, TempSize, errorsFound);
            // reset the design air volume flow rate for air loop coils only
            if (state.dataSize->CurSysNum > 0) state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate = TempSize;
            state.dataWaterCoils->WaterCoil(CoilNum).InletAirMassFlowRate =
                state.dataEnvrn->StdRhoAir * TempSize; // inlet air mass flow rate is not the autosized value
            state.dataSize->DataAirFlowUsedForSizing = TempSize;
            state.dataSize->DataFlowUsedForSizing = TempSize; // many autosized inputs use the design (autosized) air flow rate, save this value

            bPRINT = true;
            if (state.dataWaterCoils->WaterCoil(CoilNum).CoilPerfInpMeth == state.dataWaterCoils->NomCap && NomCapUserInp) {
                TempSize = state.dataWaterCoils->WaterCoil(CoilNum).DesTotWaterCoilLoad;
                state.dataSize->DataNomCapInpMeth = true;
            } else {
                TempSize = AutoSize;
            }
            if (state.dataSize->CurSysNum > 0) {
                SizingType = HeatingCapacitySizing;
                FieldNum = 3; //  N3 , \field Rated Capacity
                SizingString = state.dataWaterCoils->WaterCoilNumericFields(CoilNum).FieldNames(FieldNum) + " [W]";
                bool errorsFound = false;
                HeatingCapacitySizer sizerHeatingCapacity;
                sizerHeatingCapacity.overrideSizingString(SizingString);
                sizerHeatingCapacity.initializeWithinEP(state, CompType, CompName, bPRINT, RoutineName);
                TempSize = sizerHeatingCapacity.size(state, TempSize, errorsFound);
                state.dataWaterCoils->WaterCoil(CoilNum).DesWaterHeatingCoilRate = TempSize;
                state.dataWaterCoils->WaterCoil(CoilNum).DesTotWaterCoilLoad = TempSize;
                state.dataSize->DataCapacityUsedForSizing = state.dataWaterCoils->WaterCoil(CoilNum).DesWaterHeatingCoilRate;
            } else {
                WaterHeatingCapacitySizer sizerWaterHeatingCapacity;
                bool ErrorsFound = false;
                sizerWaterHeatingCapacity.initializeWithinEP(state, CompType, CompName, bPRINT, RoutineName);
                state.dataWaterCoils->WaterCoil(CoilNum).DesWaterHeatingCoilRate = sizerWaterHeatingCapacity.size(state, TempSize, ErrorsFound);
                state.dataWaterCoils->WaterCoil(CoilNum).DesTotWaterCoilLoad = state.dataWaterCoils->WaterCoil(CoilNum).DesWaterHeatingCoilRate;
                state.dataSize->DataCapacityUsedForSizing = state.dataWaterCoils->WaterCoil(CoilNum).DesWaterHeatingCoilRate;
            }

            // We now have the design load if it was autosized. For the case of CoilPerfInpMeth == NomCap, calculate the air flow rate
            // specified by the NomCap inputs. This overrides all previous values
            if (state.dataWaterCoils->WaterCoil(CoilNum).CoilPerfInpMeth == state.dataWaterCoils->NomCap && NomCapUserInp) {
                state.dataWaterCoils->WaterCoil(CoilNum).InletAirMassFlowRate =
                    state.dataWaterCoils->WaterCoil(CoilNum).DesTotWaterCoilLoad /
                    (CpAirStd *
                     (state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirTemp - state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirTemp));
                state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate =
                    state.dataWaterCoils->WaterCoil(CoilNum).InletAirMassFlowRate / state.dataEnvrn->StdRhoAir;
                state.dataSize->DataAirFlowUsedForSizing = state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate;
                state.dataSize->DataFlowUsedForSizing = state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate;
            }

            TempSize = state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate;

            if (state.dataWaterCoils->WaterCoil(CoilNum).CoilPerfInpMeth == state.dataWaterCoils->NomCap && NomCapUserInp) {
                if (state.dataWaterCoils->WaterCoil(CoilNum).DesTotWaterCoilLoad > SmallLoad) {
                    state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate =
                        state.dataSize->DataCapacityUsedForSizing /
                        (Cp * rho *
                         (state.dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp - state.dataWaterCoils->WaterCoil(CoilNum).DesOutletWaterTemp));
                } else {
                    state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate = 0.0;
                }
                state.dataSize->DataConstantUsedForSizing = state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate;
                state.dataSize->DataFractionUsedForSizing = 1.0;
            }
            HeatingWaterflowSizer sizerHWWaterflow;
            sizerHWWaterflow.initializeWithinEP(state, CompType, CompName, bPRINT, RoutineName);
            Real64 sizedMaxWaterVolFlowRate = sizerHWWaterflow.size(state, TempSize, ErrorsFound);
            // Check if the water flow rate is defined in parent HVAC equipment and set water coil design water flow rate accordingly
            if (state.dataSize->CurZoneEqNum > 0) {
                if (ZoneEqSizing(state.dataSize->CurZoneEqNum).DesignSizeFromParent) {
                    state.dataSize->DataWaterFlowUsedForSizing = ZoneEqSizing(state.dataSize->CurZoneEqNum).MaxHWVolFlow;
                    state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate = ZoneEqSizing(state.dataSize->CurZoneEqNum).MaxHWVolFlow;
                } else {
                    state.dataSize->DataWaterFlowUsedForSizing = sizedMaxWaterVolFlowRate;
                    state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate = sizedMaxWaterVolFlowRate;
                }
            } else {
                state.dataSize->DataWaterFlowUsedForSizing = sizedMaxWaterVolFlowRate;
                state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate = sizedMaxWaterVolFlowRate;
            }
            state.dataSize->DataConstantUsedForSizing = 0.0; // reset these in case NomCapUserInp was true
            state.dataSize->DataFractionUsedForSizing = 0.0;
            if (state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate <= 0.0) {
                //                    MaxWaterVolFlowRateDes = 0.0;
                ShowWarningError(state, "The design coil load is zero for Coil:Heating:Water " + state.dataWaterCoils->WaterCoil(CoilNum).Name);
                ShowContinueError(state, "The autosize value for maximum water flow rate is zero");
                ShowContinueError(state, "To change this, input a value for UA, change the heating design day, or raise the");
                ShowContinueError(state, "  system heating design supply air temperature. Also check to make sure the Preheat");
                ShowContinueError(state, "  Design Temperature is not the same as the Central Heating Design Supply Air Temperature. ");
            }

            // initialize the water coil inlet conditions
            bPRINT = false; // no need to print to eio since we only need the values
            state.dataSize->DataFlowUsedForSizing = state.dataSize->DataAirFlowUsedForSizing;
            if (state.dataWaterCoils->WaterCoil(CoilNum).CoilPerfInpMeth == state.dataWaterCoils->NomCap && NomCapUserInp) {
                state.dataWaterCoils->WaterCoil(CoilNum).InletAirTemp = state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirTemp;
                state.dataWaterCoils->WaterCoil(CoilNum).InletAirHumRat =
                    PsyWFnTdbRhPb(state, state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirTemp, 0.5, state.dataEnvrn->StdBaroPress, RoutineName);
                state.dataWaterCoils->WaterCoil(CoilNum).InletAirMassFlowRate =
                    state.dataSize->DataAirFlowUsedForSizing * state.dataEnvrn->StdRhoAir;                        // don't need this
                state.dataSize->DataDesOutletAirTemp = state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirTemp; // for error messages
                state.dataSize->DataDesOutletAirHumRat =
                    PsyWFnTdbRhPb(state, state.dataSize->DataDesOutletAirTemp, 0.5, state.dataEnvrn->StdBaroPress, RoutineName); // for error messages
                state.dataWaterCoils->WaterCoil(CoilNum).InletWaterMassFlowRate = rho * state.dataSize->DataWaterFlowUsedForSizing;
                state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterMassFlowRate = rho * state.dataSize->DataWaterFlowUsedForSizing;
                state.dataWaterCoils->WaterCoil(CoilNum).InletWaterTemp = state.dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp;
            } else if (state.dataWaterCoils->WaterCoil(CoilNum).DesiccantRegenerationCoil) {
                state.dataWaterCoils->WaterCoil(CoilNum).InletAirTemp = state.dataSize->DataDesInletAirTemp;
                HeatingCoilDesAirInletHumRatSizer sizerHeatingDesInletHumRat;
                bool ErrorsFound = false;
                sizerHeatingDesInletHumRat.initializeWithinEP(state, CompType, CompName, bPRINT, RoutineName);
                state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirHumRat =
                    sizerHeatingDesInletHumRat.size(state, DataSizing::AutoSize, ErrorsFound);
                state.dataWaterCoils->WaterCoil(CoilNum).InletAirHumRat = state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirHumRat;

                state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate = state.dataSize->DataAirFlowUsedForSizing; // coil report
                state.dataWaterCoils->WaterCoil(CoilNum).InletAirMassFlowRate =
                    state.dataSize->DataAirFlowUsedForSizing * state.dataEnvrn->StdRhoAir; // this is stiil volume flow!
            } else {
                HeatingWaterDesAirInletTempSizer sizerHWDesInletTemp;
                sizerHWDesInletTemp.initializeWithinEP(state, CompType, CompName, bPRINT, RoutineName);
                state.dataWaterCoils->WaterCoil(CoilNum).InletAirTemp = sizerHWDesInletTemp.size(state, DataSizing::AutoSize, ErrorsFound);

                TempSize = AutoSize; // these data are initially 0, set to autosize to receive a result from Sizers
                HeatingWaterDesAirInletHumRatSizer sizerHWAirInletHumRat;
                sizerHWAirInletHumRat.initializeWithinEP(state, CompType, CompName, bPRINT, RoutineName);
                state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirHumRat = sizerHWAirInletHumRat.size(state, DataSizing::AutoSize, ErrorsFound);
                state.dataWaterCoils->WaterCoil(CoilNum).InletAirHumRat = state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirHumRat;

                HeatingAirflowUASizer sizerHWAirFlowUA;
                sizerHWAirFlowUA.initializeWithinEP(state, CompType, CompName, bPRINT, RoutineName);
                state.dataWaterCoils->WaterCoil(CoilNum).DesAirMassFlowRate = sizerHWAirFlowUA.size(state, DataSizing::AutoSize, ErrorsFound);
                state.dataWaterCoils->WaterCoil(CoilNum).InletAirMassFlowRate = state.dataWaterCoils->WaterCoil(CoilNum).DesAirMassFlowRate;
            }

            // zone and air loop coils use different design coil load calculations, air loop coils use air side capacity,
            // zone coils use water side capacity
            state.dataSize->DataDesInletAirTemp = state.dataWaterCoils->WaterCoil(CoilNum).InletAirTemp;                   // used in error mesages
            state.dataSize->DataDesInletAirHumRat = state.dataWaterCoils->WaterCoil(CoilNum).InletAirHumRat;               // used in error mesages
            state.dataSize->DataFlowUsedForSizing = state.dataSize->DataAirFlowUsedForSizing * state.dataEnvrn->StdRhoAir; // used in error mesages
            state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate = state.dataSize->DataWaterFlowUsedForSizing;     // why is this here?
            if (!(state.dataWaterCoils->WaterCoil(CoilNum).CoilPerfInpMeth == state.dataWaterCoils->NomCap && NomCapUserInp)) {
                // get the design coil load used to size UA
                HeatingWaterDesCoilLoadUsedForUASizer sizerHWDesCoilLoadForUA;
                sizerHWDesCoilLoadForUA.initializeWithinEP(state, CompType, CompName, bPRINT, RoutineName);
                state.dataSize->DataCapacityUsedForSizing = sizerHWDesCoilLoadForUA.size(state, DataSizing::AutoSize, ErrorsFound);
                // get the water volume flow rate used to size UA
                HeatingWaterDesCoilWaterVolFlowUsedForUASizer sizerHWWaterVolFlowUA;
                sizerHWWaterVolFlowUA.initializeWithinEP(state, CompType, CompName, bPRINT, RoutineName);
                state.dataSize->DataWaterFlowUsedForSizing = sizerHWWaterVolFlowUA.size(state, DataSizing::AutoSize, ErrorsFound);
                state.dataWaterCoils->WaterCoil(CoilNum).InletWaterTemp = state.dataSize->PlantSizData(PltSizHeatNum).ExitTemp;
                state.dataWaterCoils->WaterCoil(CoilNum).InletWaterMassFlowRate = rho * state.dataSize->DataWaterFlowUsedForSizing;
                state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterMassFlowRate = rho * state.dataSize->DataWaterFlowUsedForSizing;
                state.dataWaterCoils->WaterCoil(CoilNum).DesWaterHeatingCoilRate = state.dataSize->DataCapacityUsedForSizing;
            }
            // calculate UA
            if (state.dataSize->CurSysNum > 0)
                state.dataWaterCoils->WaterCoil(CoilNum).DesTotWaterCoilLoad = state.dataSize->DataCapacityUsedForSizing;
            FieldNum = 1;  // N1 , \field U-Factor Times Area Value
            bPRINT = true; // report to eio the UA value
            SizingString = state.dataWaterCoils->WaterCoilNumericFields(CoilNum).FieldNames(FieldNum) + " [W/K]";
            state.dataSize->DataCoilNum = CoilNum;
            state.dataSize->DataFanOpMode = ContFanCycCoil;
            if (state.dataWaterCoils->WaterCoil(CoilNum).CoilPerfInpMeth == state.dataWaterCoils->NomCap && NomCapUserInp) {
                TempSize = AutoSize;
            } else {
                TempSize = state.dataWaterCoils->WaterCoil(CoilNum).UACoil;
            }

            state.dataSize->DataFlowUsedForSizing = state.dataWaterCoils->WaterCoil(CoilNum).InletAirMassFlowRate;
            DesCoilWaterInTempSaved = state.dataWaterCoils->WaterCoil(state.dataSize->DataCoilNum).InletWaterTemp;
            if (DesCoilWaterInTempSaved < DesCoilHWInletTempMin) {
                // at low coil design water inlet temp, sizing has convergence issue hence slightly higher water inlet temperature
                // is estimated in "EstimateCoilInletWaterTemp" and used for UA autosizing only
                EstimateCoilInletWaterTemp(state,
                                           state.dataSize->DataCoilNum,
                                           state.dataSize->DataFanOpMode,
                                           1.0,
                                           state.dataSize->DataCapacityUsedForSizing,
                                           DesCoilInletWaterTempUsed);
                state.dataWaterCoils->WaterCoil(state.dataSize->DataCoilNum).InletWaterTemp = DesCoilInletWaterTempUsed;
            }
            // must set DataCapacityUsedForSizing, DataWaterFlowUsedForSizing and DataFlowUsedForSizing to size UA. Any value of 0 will result
            // in UA = 1.
            WaterHeatingCoilUASizer sizerHWCoilUA;
            sizerHWCoilUA.initializeWithinEP(state, CompType, CompName, bPRINT, RoutineName);
            state.dataWaterCoils->WaterCoil(CoilNum).UACoil = sizerHWCoilUA.size(state, TempSize, ErrorsFound);
            if (DesCoilWaterInTempSaved < DesCoilHWInletTempMin) {
                ShowWarningError(state, "Autosizing of heating coil UA for Coil:Heating:Water \"" + std::string{CompName} + "\"");
                ShowContinueError(state,
                                  format(" Plant design loop exit temperature = {:.2T} C",
                                         state.dataSize->PlantSizData(state.dataSize->DataPltSizHeatNum).ExitTemp));
                ShowContinueError(state, " Plant design loop exit temperature is low for design load and leaving air temperature anticipated.");
                ShowContinueError(state,
                                  format(" Heating coil UA-value is sized using coil water inlet temperature = {:.2T} C", DesCoilInletWaterTempUsed));
                state.dataWaterCoils->WaterCoil(state.dataSize->DataCoilNum).InletWaterTemp =
                    DesCoilWaterInTempSaved; // reset the Design Coil Inlet Water Temperature
            }
            // if coil UA did not size due to one of these variables being 0, must set UACoilVariable to avoid crash later on
            if (state.dataSize->DataCapacityUsedForSizing == 0.0 || state.dataSize->DataWaterFlowUsedForSizing == 0.0 ||
                state.dataSize->DataFlowUsedForSizing == 0.0) {
                if (state.dataWaterCoils->WaterCoil(CoilNum).UACoilVariable == AutoSize) {
                    state.dataWaterCoils->WaterCoil(CoilNum).UACoilVariable = state.dataWaterCoils->WaterCoil(CoilNum).UACoil;
                }
            }
            // WaterCoil(CoilNum).UACoilVariable = WaterCoil(CoilNum).UACoil;
            state.dataWaterCoils->WaterCoil(CoilNum).DesWaterHeatingCoilRate = state.dataSize->DataCapacityUsedForSizing;
            state.dataWaterCoils->WaterCoil(state.dataSize->DataCoilNum).InletWaterTemp =
                DesCoilWaterInTempSaved; // reset the Design Coil Inlet Water Temperature

            state.dataSize->DataWaterLoopNum = 0; // reset all globals to 0 to ensure correct sizing for other child components
            state.dataSize->DataPltSizHeatNum = 0;
            state.dataSize->DataCoilNum = 0;
            state.dataSize->DataFanOpMode = 0;
            state.dataSize->DataCapacityUsedForSizing = 0.0;
            state.dataSize->DataWaterFlowUsedForSizing = 0.0;
            state.dataSize->DataDesInletAirTemp = 0.0;
            state.dataSize->DataDesInletAirHumRat = 0.0;
            state.dataSize->DataDesOutletAirTemp = 0.0;
            state.dataSize->DataDesOutletAirHumRat = 0.0;
            state.dataSize->DataAirFlowUsedForSizing = 0.0;
            state.dataSize->DataFlowUsedForSizing = 0.0;
            state.dataSize->DataDesicDehumNum = 0;
            state.dataSize->DataDesicRegCoil = false;
            state.dataSize->DataWaterCoilSizHeatDeltaT = 0.0;
            state.dataSize->DataNomCapInpMeth = false;

        } else {
            // if there is no heating Plant Sizing object and autosizng was requested, issue an error message
            if (state.dataWaterCoils->WaterCoil(CoilNum).RequestingAutoSize) {
                ShowSevereError(state, "Autosizing of water coil requires a heating loop Sizing:Plant object");
                ShowContinueError(state, "Occurs in water coil object= " + state.dataWaterCoils->WaterCoil(CoilNum).Name);
                ErrorsFound = true;
            }
        }
        //} // end of heating Plant Sizing existence IF - ELSE
    } // end heating coil IF

    // save the design water volumetric flow rate for use by the water loop sizing algorithms
    if (state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate > 0.0) {
        RegisterPlantCompDesignFlow(
            state, state.dataWaterCoils->WaterCoil(CoilNum).WaterInletNodeNum, state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate);
    }

    if (ErrorsFound || state.dataSize->DataErrorsFound) {
        ShowFatalError(state, "Preceding water coil sizing errors cause program termination");
    }
}

// End Initialization Section of the Module
//******************************************************************************

// Begin Algorithm Section of the Module
//******************************************************************************

void CalcSimpleHeatingCoil(EnergyPlusData &state,
                           int const CoilNum,          // index to heating coil
                           int const FanOpMode,        // fan operating mode
                           Real64 const PartLoadRatio, // part-load ratio of heating coil
                           int const CalcMode          // 1 = design calc; 2 = simulation calculation
)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Rich Liesen
    //       DATE WRITTEN
    //       MODIFIED       Aug. 2007 - R. Raustad, added fan operating mode and part-load ratio to
    //                                  calculate the outlet conditions when fan and coil cycle.
    //                                  Air and water outlet temperature are full output with average
    //                                  air and water mass flow rate when fan and coil cycle.
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Simulates a simple NTU effectiveness model heating coil

    // METHODOLOGY EMPLOYED:
    // (1) outlet conditions are calculated from the effectiveness and the inlet conditions.
    // (2) Effectiveness is calculated from the NTU formula for a cross flow heat exchanger
    //     with both streams unmixed.
    // Note: UA is input by user and is fixed.

    // REFERENCES:
    // See for instance ASHRAE HVAC 2 Toolkit, page 4-4, formula (4-7)

    // Using/Aliasing

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:

    // SUBROUTINE PARAMETER DEFINITIONS:
    static constexpr std::string_view RoutineName("CalcSimpleHeatingCoil");

    // INTERFACE BLOCK SPECIFICATIONS
    // na

    // DERIVED TYPE DEFINITIONS
    // na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 WaterMassFlowRate;
    Real64 AirMassFlow; // [kg/sec]
    Real64 TempAirIn;   // [C]
    Real64 TempAirOut;  // [C]
    Real64 Win;
    Real64 TempWaterIn;
    Real64 TempWaterOut;
    Real64 UA;
    Real64 CapacitanceAir;
    Real64 CapacitanceWater;
    Real64 CapacitanceMin;
    Real64 CapacitanceMax;
    Real64 HeatingCoilLoad;
    Real64 NTU;
    Real64 ETA;
    Real64 A;
    Real64 CapRatio;
    Real64 E1;
    Real64 E2;
    Real64 Effec;
    Real64 Cp;
    int Control;

    UA = state.dataWaterCoils->WaterCoil(CoilNum).UACoilVariable;
    TempAirIn = state.dataWaterCoils->WaterCoil(CoilNum).InletAirTemp;
    Win = state.dataWaterCoils->WaterCoil(CoilNum).InletAirHumRat;
    Control = state.dataWaterCoils->WaterCoil(CoilNum).Control;
    TempWaterIn = state.dataWaterCoils->WaterCoil(CoilNum).InletWaterTemp;

    // adjust mass flow rates for cycling fan cycling coil operation
    if (FanOpMode == CycFanCycCoil) {
        if (PartLoadRatio > 0.0) {
            AirMassFlow = state.dataWaterCoils->WaterCoil(CoilNum).InletAirMassFlowRate / PartLoadRatio;
            WaterMassFlowRate = min(state.dataWaterCoils->WaterCoil(CoilNum).InletWaterMassFlowRate / PartLoadRatio,
                                    state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterMassFlowRate);
        } else {
            AirMassFlow = 0.0;
            WaterMassFlowRate = 0.0;
        }
    } else {
        AirMassFlow = state.dataWaterCoils->WaterCoil(CoilNum).InletAirMassFlowRate;
        WaterMassFlowRate = state.dataWaterCoils->WaterCoil(CoilNum).InletWaterMassFlowRate;
    }

    if (WaterMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance) { // If the coil is operating
        CapacitanceAir = PsyCpAirFnW(Win) * AirMassFlow;
        Cp = GetSpecificHeatGlycol(state,
                                   state.dataPlnt->PlantLoop(state.dataWaterCoils->WaterCoil(CoilNum).WaterLoopNum).FluidName,
                                   TempWaterIn,
                                   state.dataPlnt->PlantLoop(state.dataWaterCoils->WaterCoil(CoilNum).WaterLoopNum).FluidIndex,
                                   RoutineName);
        CapacitanceWater = Cp * WaterMassFlowRate;
        CapacitanceMin = min(CapacitanceAir, CapacitanceWater);
        CapacitanceMax = max(CapacitanceAir, CapacitanceWater);
    } else {
        CapacitanceAir = 0.0;
        CapacitanceWater = 0.0;
    }

    // If the coil is operating there should be some heating capacitance
    //  across the coil, so do the simulation. If not set outlet to inlet and no load.
    //  Also the coil has to be scheduled to be available
    if (((CapacitanceAir > 0.0) && (CapacitanceWater > 0.0)) &&
        (CalcMode == state.dataWaterCoils->DesignCalc || state.dataWaterCoils->MySizeFlag(CoilNum) ||
         state.dataWaterCoils->MyUAAndFlowCalcFlag(CoilNum) ||
         GetCurrentScheduleValue(state, state.dataWaterCoils->WaterCoil(CoilNum).SchedPtr) > 0.0)) {

        if (UA <= 0.0) {
            ShowFatalError(state, "UA is zero for COIL:Heating:Water " + state.dataWaterCoils->WaterCoil(CoilNum).Name);
        }
        NTU = UA / CapacitanceMin;
        ETA = std::pow(NTU, 0.22);
        CapRatio = CapacitanceMin / CapacitanceMax;
        A = CapRatio * NTU / ETA;

        if (A > 20.0) {
            A = ETA * 1.0 / CapRatio;
        } else {
            E1 = std::exp(-A);
            A = ETA * (1.0 - E1) / CapRatio;
        }

        if (A > 20.0) {
            Effec = 1.0;
        } else {
            E2 = std::exp(-A);
            Effec = 1.0 - E2;
        }

        TempAirOut = TempAirIn + Effec * CapacitanceMin * (TempWaterIn - TempAirIn) / CapacitanceAir;
        TempWaterOut = TempWaterIn - CapacitanceAir * (TempAirOut - TempAirIn) / CapacitanceWater;
        HeatingCoilLoad = CapacitanceWater * (TempWaterIn - TempWaterOut);
        // The HeatingCoilLoad is the change in the enthalpy of the water
        state.dataWaterCoils->WaterCoil(CoilNum).OutletWaterEnthalpy =
            state.dataWaterCoils->WaterCoil(CoilNum).InletWaterEnthalpy -
            HeatingCoilLoad / state.dataWaterCoils->WaterCoil(CoilNum).InletWaterMassFlowRate;
        state.dataWaterCoils->WaterCoil(CoilNum).OutletWaterMassFlowRate = state.dataWaterCoils->WaterCoil(CoilNum).InletWaterMassFlowRate;

    } else { // If not running Conditions do not change across coil from inlet to outlet

        TempAirOut = TempAirIn;
        TempWaterOut = TempWaterIn;
        HeatingCoilLoad = 0.0;
        state.dataWaterCoils->WaterCoil(CoilNum).OutletWaterEnthalpy = state.dataWaterCoils->WaterCoil(CoilNum).InletWaterEnthalpy;
        state.dataWaterCoils->WaterCoil(CoilNum).OutletWaterMassFlowRate = 0.0;
    }

    if (FanOpMode == CycFanCycCoil) {
        HeatingCoilLoad *= PartLoadRatio;
    }

    // Set the outlet conditions
    state.dataWaterCoils->WaterCoil(CoilNum).TotWaterHeatingCoilRate = HeatingCoilLoad;
    state.dataWaterCoils->WaterCoil(CoilNum).OutletAirTemp = TempAirOut;
    state.dataWaterCoils->WaterCoil(CoilNum).OutletWaterTemp = TempWaterOut;

    // This WaterCoil does not change the moisture or Mass Flow across the component
    state.dataWaterCoils->WaterCoil(CoilNum).OutletAirHumRat = state.dataWaterCoils->WaterCoil(CoilNum).InletAirHumRat;
    state.dataWaterCoils->WaterCoil(CoilNum).OutletAirMassFlowRate = state.dataWaterCoils->WaterCoil(CoilNum).InletAirMassFlowRate;
    // Set the outlet enthalpys for air and water
    state.dataWaterCoils->WaterCoil(CoilNum).OutletAirEnthalpy =
        PsyHFnTdbW(state.dataWaterCoils->WaterCoil(CoilNum).OutletAirTemp, state.dataWaterCoils->WaterCoil(CoilNum).OutletAirHumRat);
}

void CalcDetailFlatFinCoolingCoil(EnergyPlusData &state,
                                  int const CoilNum,
                                  int const CalcMode,
                                  int const FanOpMode,       // fan operating mode
                                  Real64 const PartLoadRatio // part-load ratio of heating coil
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR(S)      Russell Taylor / Richard Liesen
    //       DATE WRITTEN   Mar 1997
    //       MODIFIED       Feb 2010, B. Nigusse, FSEC, corrected units inconsistency for tube and fins
    //                      materials thermal conductivties. Now input values in the idf are in {W/(m.K)}
    //       RE-ENGINEERED  Sept 1998

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine simulates a chilled water cooling coil.  Provided with
    // the coil geometry and the flow (i.e. air and water) inlet conditions,
    // it will calculate the flow outlet conditions and the total and latent
    // heat extraction rates from the air.  The coil model has some limitations
    // as noted in the code.

    // METHODOLOGY EMPLOYED:
    // successive substitution, solve coil as if all wet, then
    // again if partly or entirely dry

    // REFERENCES:
    // First found in Type 12 from MODSIM, but now
    // programmed directly from Elmahdy, A.H. and Mitalas, G.P.  "A
    // Simple Model for Cooling and Dehumidifying Coils for Use in
    // Calculating Energy Requirements for Buildings"  _ASHRAE
    // Transactions_ Vol. 83, Part 2, pp. 103-117 (1977).

    // OTHER NOTES:
    // Routine was originally adapted for use in IBLAST by R.D. Taylor in l993.
    // Subsequently rewritten and improved by J.C. Vanderzee in 1994
    // Revised and further enhanced by R.D. Taylor in Jan 1996
    // Re-engineered for EnergyPlus by Richard Liesen PhD in 1998

    // Using/Aliasing

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:

    // SUBROUTINE PARAMETER DEFINITIONS:
    static Real64 const exp_47(std::exp(-0.41718));
    static Real64 const exp_35(std::exp(-0.3574));
    static constexpr std::string_view RoutineName("CalcDetailFlatFinCoolingCoil");

    constexpr Real64 AirViscosity(1.846e-5); // Dynamic Viscosity of Air in kg/(m.s)
    constexpr Real64 ConvK(1.0e-3);          // Unit conversion factor
    constexpr Real64 unity(1.0);
    constexpr Real64 zero(0.0);
    constexpr Real64 TubeFoulFactor(5.0e-2); // Inside tube fouling factor for water, in m2K/kW
    // Changed from m2K/W to m2K/kW for consistency with the
    // other parameters in "TubeFoulThermResis" calculation

    // INTERFACE BLOCK SPECIFICATIONS
    // na

    // DERIVED TYPE DEFINITIONS
    // na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int CoefPointer;
    //    INTEGER :: CoolCoilErrs = 0
    int PartWetIterations;
    int WaterTempConvgLoop;

    bool CoilPartWetConvg;
    bool WaterTempConvg;

    Real64 AirEnthAtRsdInletWaterTemp;
    Real64 AirExitEnthlAtCoilSurfTemp;
    Real64 AirExitCoilSurfTemp;
    Real64 AirReynoldsNo;
    Real64 AirEnthAtWetDryIntrfcSurfTemp;
    Real64 AirSideDrySurfFilmCoef;
    Real64 AirSideWetSurfFilmCoef;
    Real64 AirWetDryInterfcTemp;
    Real64 CoilToAirThermResistDrySurf;
    Real64 CoilToAirThermResistWetSurf;
    Real64 DryAirSpecHeat;
    Real64 DryCoilCoeff1;
    Real64 DryCoilCoeff;
    Real64 DryCoilEfficiency;
    Real64 DryFinEfficncy;
    Real64 DryCoilInThermResist;
    Real64 DrySideEffectiveWaterTemp;
    Real64 EnterAirDewPoint;
    Real64 EnterAirHumRatDiff;
    Real64 WetDryInterSurfTempErrorLast;
    Real64 WetDryInterSurfTempError;
    Real64 expon;
    Real64 FilmCoefEqnFactor;
    Real64 FilmCoefReynldsCorrelatnFact;
    Real64 FinToTotSurfAreaRatio;
    Real64 InCoilSurfTemp;
    Real64 InsdToOutsdThermResistRatio;
    Real64 InSurfTempSatAirEnthl;
    Real64 K1;
    Real64 MeanWaterTemp;
    Real64 MoistAirSpecificHeat;
    Real64 OutCoilSurfTemp;
    Real64 OutSurfTempSatAirEnthl;
    Real64 RaisedInletWaterTemp;
    Real64 RsdInletWaterTempSatAirHumRat;
    Real64 ScaledAirMassFlowRate;
    Real64 ScaledCoilAirThermResistWetSurf;
    Real64 ScaledWaterSpecHeat;
    Real64 ScaledWaterToTubeThermResist;
    Real64 SensToTotEnthDiffRatio;
    Real64 SurfAreaWet;
    Real64 TubeFoulThermResist;
    Real64 TubeWaterVel;
    Real64 UACoilAllWet;
    Real64 UACoilPartWet;
    Real64 UADryCoil;
    Real64 WaterToTubeThermResist;
    Real64 WetAreaChange;
    Real64 WetAreaLast;
    Real64 WetCoilCoeff;
    Real64 WetCoilFinEfficncy;
    Real64 WetDryInterfcAirEnthl;
    Real64 WetDryInterfcSurfTemp;
    Real64 WetDryInterfcWaterTemp;
    Real64 WetFinEfficncy;
    Real64 WetSideEffctvWaterTemp;
    Real64 y;
    Real64 TempAirIn;
    Real64 TempAirOut;
    Real64 InletAirHumRat;
    Real64 OutletAirHumRat;
    Real64 InletAirEnthalpy;
    Real64 OutletAirEnthalpy;
    Real64 WaterMassFlowRate;
    Real64 AirMassFlow;
    Real64 TempWaterIn;
    Real64 TempWaterOut;
    Real64 TotWaterCoilLoad;
    Real64 SenWaterCoilLoad;
    Real64 AirDensity;
    Real64 AirVelocity;
    Real64 denom;
    Real64 rho;
    Real64 Cp;

    // Set derived type variables to shorter local variables
    TempAirIn = state.dataWaterCoils->WaterCoil(CoilNum).InletAirTemp;
    InletAirHumRat = state.dataWaterCoils->WaterCoil(CoilNum).InletAirHumRat;
    TempWaterIn = state.dataWaterCoils->WaterCoil(CoilNum).InletWaterTemp;

    //  adjust mass flow rates for cycling fan cycling coil operation
    if (FanOpMode == CycFanCycCoil) {
        if (PartLoadRatio > 0.0) {
            AirMassFlow = state.dataWaterCoils->WaterCoil(CoilNum).InletAirMassFlowRate / PartLoadRatio;
            WaterMassFlowRate = min(state.dataWaterCoils->WaterCoil(CoilNum).InletWaterMassFlowRate / PartLoadRatio,
                                    state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterMassFlowRate);
        } else {
            AirMassFlow = 0.0;
            WaterMassFlowRate = 0.0;
        }
    } else {
        AirMassFlow = state.dataWaterCoils->WaterCoil(CoilNum).InletAirMassFlowRate;
        WaterMassFlowRate = state.dataWaterCoils->WaterCoil(CoilNum).InletWaterMassFlowRate;
    }

    if (WaterMassFlowRate < state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterMassFlowRate * WaterCoils::MinWaterMassFlowFrac) {
        WaterMassFlowRate = 0.0;
    }
    if (TempAirIn <= TempWaterIn) {
        WaterMassFlowRate = 0.0;
    }
    WetDryInterfcAirEnthl = 0.0;
    OutletAirEnthalpy = 0.0;
    InletAirEnthalpy = 0.0;

    // Warning and error messages for large flow rates for the given user input geometry
    AirDensity = PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, TempAirIn, InletAirHumRat, RoutineName);
    if (AirMassFlow > (5.0 * state.dataWaterCoils->WaterCoil(CoilNum).MinAirFlowArea / AirDensity) &&
        state.dataWaterCoils->CoilWarningOnceFlag(CoilNum)) {
        ShowWarningError(state, "Coil:Cooling:Water:DetailedGeometry in Coil =" + state.dataWaterCoils->WaterCoil(CoilNum).Name);
        ShowContinueError(state, "Air Flow Rate Velocity has greatly exceeded upper design guidelines of ~2.5 m/s");
        ShowContinueError(state, format("Air Mass Flow Rate[kg/s]={:.6T}", AirMassFlow));
        // [m/s] = [kg/s] / ([m2] * [kg/m3])
        AirVelocity = AirMassFlow / (state.dataWaterCoils->WaterCoil(CoilNum).MinAirFlowArea * AirDensity);
        ShowContinueError(state, format("Air Face Velocity[m/s]={:.6T}", AirVelocity));
        ShowContinueError(state,
                          format("Approximate Mass Flow Rate limit for Face Area[kg/s]={:.6T}",
                                 2.5 * state.dataWaterCoils->WaterCoil(CoilNum).MinAirFlowArea * AirDensity));
        ShowContinueError(state, "Coil:Cooling:Water:DetailedGeometry could be resized/autosized to handle capacity");
        state.dataWaterCoils->CoilWarningOnceFlag(CoilNum) = false;
    } else if (AirMassFlow > (44.7 * state.dataWaterCoils->WaterCoil(CoilNum).MinAirFlowArea * AirDensity)) {
        ShowSevereError(state, "Coil:Cooling:Water:DetailedGeometry in Coil =" + state.dataWaterCoils->WaterCoil(CoilNum).Name);
        ShowContinueError(state, "Air Flow Rate Velocity is > 100MPH (44.7m/s) and simulation cannot continue");
        ShowContinueError(state, format("Air Mass Flow Rate[kg/s]={:.6T}", AirMassFlow));
        AirVelocity = AirMassFlow / (state.dataWaterCoils->WaterCoil(CoilNum).MinAirFlowArea * AirDensity);
        ShowContinueError(state, format("Air Face Velocity[m/s]={:.6T}", AirVelocity));
        ShowContinueError(state,
                          format("Approximate Mass Flow Rate limit for Face Area[kg/s]={:.6T}",
                                 44.7 * state.dataWaterCoils->WaterCoil(CoilNum).MinAirFlowArea * AirDensity));
        ShowFatalError(state, "Coil:Cooling:Water:DetailedGeometry needs to be resized/autosized to handle capacity");
    }

    // If Coil is Scheduled ON then do the simulation
    if (((GetCurrentScheduleValue(state, state.dataWaterCoils->WaterCoil(CoilNum).SchedPtr) > 0.0) && (WaterMassFlowRate > 0.0) &&
         (AirMassFlow >= WaterCoils::MinAirMassFlow)) ||
        (CalcMode == state.dataWaterCoils->DesignCalc)) {
        //        transfer inputs to simulation variables and calculate
        //        known thermodynamic functions
        // All coil calcs are done in KJoules.  Convert to KJ here and then convert
        //  back to Joules at the end of the Subroutine.
        DryAirSpecHeat = PsyCpAirFnW(zero) * ConvK;
        MoistAirSpecificHeat = PsyCpAirFnW(InletAirHumRat) * ConvK;
        InletAirEnthalpy = state.dataWaterCoils->WaterCoil(CoilNum).InletAirEnthalpy * ConvK;

        EnterAirDewPoint = PsyTdpFnWPb(state, InletAirHumRat, state.dataEnvrn->OutBaroPress, RoutineName);
        //       Ratio of secondary (fin) to total (secondary plus primary) surface areas
        FinToTotSurfAreaRatio =
            state.dataWaterCoils->WaterCoil(CoilNum).FinSurfArea / state.dataWaterCoils->WaterCoil(CoilNum).TotCoilOutsideSurfArea;
        //      known water and air flow parameters:
        rho = GetDensityGlycol(state,
                               state.dataPlnt->PlantLoop(state.dataWaterCoils->WaterCoil(CoilNum).WaterLoopNum).FluidName,
                               TempWaterIn,
                               state.dataPlnt->PlantLoop(state.dataWaterCoils->WaterCoil(CoilNum).WaterLoopNum).FluidIndex,
                               RoutineName);
        //      water flow velocity - assuming number of water circuits = NumOfTubesPerRow
        TubeWaterVel = WaterMassFlowRate * 4.0 /
                       (state.dataWaterCoils->WaterCoil(CoilNum).NumOfTubesPerRow * rho * DataGlobalConstants::Pi *
                        state.dataWaterCoils->WaterCoil(CoilNum).TubeInsideDiam * state.dataWaterCoils->WaterCoil(CoilNum).TubeInsideDiam);
        //      air mass flow rate per unit area
        ScaledAirMassFlowRate = (1.0 + InletAirHumRat) * AirMassFlow / state.dataWaterCoils->WaterCoil(CoilNum).MinAirFlowArea;
        //      air flow Reynold's Number
        AirReynoldsNo = state.dataWaterCoils->WaterCoil(CoilNum).CoilEffectiveInsideDiam * ScaledAirMassFlowRate / AirViscosity;
        //       heat transfer coefficients and resistance components:
        //              inside (water)
        WaterToTubeThermResist = std::pow(state.dataWaterCoils->WaterCoil(CoilNum).TubeInsideDiam, 0.2) /
                                 (state.dataWaterCoils->WaterCoil(CoilNum).TotTubeInsideArea * 1.429 * std::pow(TubeWaterVel, 0.8));
        //              metal and fouling
        TubeFoulThermResist =
            (0.5 * (state.dataWaterCoils->WaterCoil(CoilNum).TubeOutsideDiam - state.dataWaterCoils->WaterCoil(CoilNum).TubeInsideDiam) /
                 (ConvK * state.dataWaterCoils->WaterCoil(CoilNum).TubeThermConductivity) +
             TubeFoulFactor) /
            state.dataWaterCoils->WaterCoil(CoilNum).TotTubeInsideArea;
        //              outside (wet and dry coil)
        FilmCoefEqnFactor =
            state.dataWaterCoils->WaterCoil(CoilNum).GeometryCoef1 * std::pow(AirReynoldsNo, state.dataWaterCoils->WaterCoil(CoilNum).GeometryCoef2);
        //       (1.23 is 1/Prandt(air)**(2/3))
        AirSideDrySurfFilmCoef = 1.23 * FilmCoefEqnFactor * MoistAirSpecificHeat * ScaledAirMassFlowRate;
        FilmCoefReynldsCorrelatnFact = 1.425 + AirReynoldsNo * (-0.51e-3 + AirReynoldsNo * 0.263e-6);
        //       NOTE: the equation for FilmCoefReynldsCorrelatnFact generates valid results over
        //             a limited range of Air Reynolds Numbers as indicated by
        //             deleted code below.  Reynolds Numbers outside this range
        //             may result in inaccurate results or failure of the coil
        //             simulation to obtain a solution
        //             Deleted code by J.C. Vanderzee

        AirSideWetSurfFilmCoef = FilmCoefReynldsCorrelatnFact * AirSideDrySurfFilmCoef;
        //--                     need wet fin efficiency for outside
        RaisedInletWaterTemp = TempWaterIn + 0.5;

        // By this statement the Inlet Air enthalpy will never be equal to AirEnthAtRsdInletWaterTemp
        if ((RaisedInletWaterTemp - TempAirIn) < 0.000001) {
            RaisedInletWaterTemp = TempWaterIn + 0.3;
        }
        if (TempAirIn < RaisedInletWaterTemp) {
            RaisedInletWaterTemp = TempAirIn - 0.3;
        }

        RsdInletWaterTempSatAirHumRat = PsyWFnTdbRhPb(state, RaisedInletWaterTemp, unity, state.dataEnvrn->OutBaroPress, RoutineName);
        AirEnthAtRsdInletWaterTemp = PsyHFnTdbW(RaisedInletWaterTemp, RsdInletWaterTempSatAirHumRat) * ConvK;

        SensToTotEnthDiffRatio = DryAirSpecHeat * (TempAirIn - RaisedInletWaterTemp) / (InletAirEnthalpy - AirEnthAtRsdInletWaterTemp);

        EnterAirHumRatDiff = InletAirHumRat - RsdInletWaterTempSatAirHumRat;
        DryFinEfficncy =
            0.5 * (state.dataWaterCoils->WaterCoil(CoilNum).EffectiveFinDiam - state.dataWaterCoils->WaterCoil(CoilNum).TubeOutsideDiam) *
            std::sqrt(
                2.0 * AirSideWetSurfFilmCoef /
                (ConvK * state.dataWaterCoils->WaterCoil(CoilNum).FinThermConductivity * state.dataWaterCoils->WaterCoil(CoilNum).FinThickness));
        if (EnterAirHumRatDiff < 0) {
            //       note that this condition indicates dry coil
            EnterAirHumRatDiff = -EnterAirHumRatDiff;
            SensToTotEnthDiffRatio = std::abs(SensToTotEnthDiffRatio);
        }

        if (EnterAirHumRatDiff > 1.0) {
            EnterAirHumRatDiff = 1.0;
        } else if (EnterAirHumRatDiff < 0.00001) {
            EnterAirHumRatDiff = 0.00001;
        }

        if (DryFinEfficncy > 1.0) {
            DryFinEfficncy = 1.0;
        } else if (DryFinEfficncy < 0.00001) {
            DryFinEfficncy = 0.00001;
        }

        if (TempAirIn > 48.0 / 1.8) {
            WetFinEfficncy =
                exp_47 * std::pow(SensToTotEnthDiffRatio, 0.09471) * std::pow(EnterAirHumRatDiff, 0.0108) * std::pow(DryFinEfficncy, -0.50303);
        } else {
            WetFinEfficncy =
                exp_35 * std::pow(SensToTotEnthDiffRatio, 0.16081) * std::pow(EnterAirHumRatDiff, 0.01995) * std::pow(DryFinEfficncy, -0.52951);
        }

        if (WetFinEfficncy > 1.0) WetFinEfficncy = 0.99;
        if (WetFinEfficncy < 0.0) WetFinEfficncy = 0.001;
        //       wet coil fin efficiency

        WetCoilFinEfficncy = 1.0 + FinToTotSurfAreaRatio * (WetFinEfficncy - 1.0);
        //       wet coil outside thermal resistance = [1/UA] (wet coil)
        CoilToAirThermResistWetSurf =
            MoistAirSpecificHeat / (state.dataWaterCoils->WaterCoil(CoilNum).TotCoilOutsideSurfArea * AirSideWetSurfFilmCoef * WetCoilFinEfficncy);
        //--                     and dry fin efficiency
        DryFinEfficncy =
            0.5 * (state.dataWaterCoils->WaterCoil(CoilNum).EffectiveFinDiam - state.dataWaterCoils->WaterCoil(CoilNum).TubeOutsideDiam) *
            std::sqrt(
                2.0 * AirSideDrySurfFilmCoef /
                (ConvK * state.dataWaterCoils->WaterCoil(CoilNum).FinThermConductivity * state.dataWaterCoils->WaterCoil(CoilNum).FinThickness));
        //      NOTE: The same caveats on the validity of the FilmCoefReynldsCorrelatnFact equation
        //            hold for the DryFinEfficncy equation.  Values of DryFinEfficncy outside the
        //            specified range of validity are not guaranteed to
        //            produce results
        //             Deleted code by J.C. Vanderzee
        //       dry coil fin efficiency
        DryCoilEfficiency = 0.0;
        // Tuned Replaced by below to eliminate pow calls
        //            for ( CoefPointer = 1; CoefPointer <= 5; ++CoefPointer ) {
        //                DryCoilEfficiency += WaterCoil( CoilNum ).DryFinEfficncyCoef( CoefPointer ) * std::pow(
        // DryFinEfficncy,
        // CoefPointer
        //-
        // 1
        //);             } // CoefPointer
        auto const &dry_fin_eff_coef(state.dataWaterCoils->WaterCoil(CoilNum).DryFinEfficncyCoef);
        auto DryFinEfficncy_pow(1.0);
        for (CoefPointer = 1; CoefPointer <= 5; ++CoefPointer) {
            DryCoilEfficiency += dry_fin_eff_coef(CoefPointer) * DryFinEfficncy_pow;
            DryFinEfficncy_pow *= DryFinEfficncy;
        } // CoefPointer
        DryCoilEfficiency = 1.0 + FinToTotSurfAreaRatio * (DryCoilEfficiency - 1.0);
        //       dry coil outside thermal resistance = [1/UA] (dry coil)
        CoilToAirThermResistDrySurf =
            1.0 / (state.dataWaterCoils->WaterCoil(CoilNum).TotCoilOutsideSurfArea * AirSideDrySurfFilmCoef * DryCoilEfficiency);
        //       definitions made to simplify some of the expressions used below
        Cp = GetSpecificHeatGlycol(state,
                                   state.dataPlnt->PlantLoop(state.dataWaterCoils->WaterCoil(CoilNum).WaterLoopNum).FluidName,
                                   TempWaterIn,
                                   state.dataPlnt->PlantLoop(state.dataWaterCoils->WaterCoil(CoilNum).WaterLoopNum).FluidIndex,
                                   RoutineName);
        ScaledWaterSpecHeat = WaterMassFlowRate * Cp * ConvK / AirMassFlow;
        DryCoilCoeff1 = 1.0 / (AirMassFlow * MoistAirSpecificHeat) - 1.0 / (WaterMassFlowRate * Cp * ConvK);
        //       perform initialisations for all wet solution
        WetSideEffctvWaterTemp =
            state.dataWaterCoils->WaterCoil(CoilNum).MeanWaterTempSaved + (TempWaterIn - state.dataWaterCoils->WaterCoil(CoilNum).InWaterTempSaved);
        WaterTempConvgLoop = 0;
        WaterTempConvg = false;
        //       Loop to solve coil as if all wet, converges on MeanWaterTemp eq WetSideEffctvWaterTemp
        //       if conv=.TRUE. at any time program exits loop and proceeds
        //       to part wet / part dry solution
        while (WaterTempConvgLoop < 8 && !WaterTempConvg) {
            ++WaterTempConvgLoop;
            ScaledWaterToTubeThermResist = WaterToTubeThermResist / (1.0 + 0.0146 * WetSideEffctvWaterTemp);
            ScaledCoilAirThermResistWetSurf = CoilToAirThermResistWetSurf / state.dataWaterCoils->WaterCoil(CoilNum).SatEnthlCurveSlope;
            UACoilAllWet = 1.0 / (state.dataWaterCoils->WaterCoil(CoilNum).SatEnthlCurveSlope *
                                  (TubeFoulThermResist + ScaledWaterToTubeThermResist + ScaledCoilAirThermResistWetSurf));
            //       prevents floating point error when taking exponential
            //       of a very large number
            expon =
                UACoilAllWet * (1.0 / AirMassFlow - state.dataWaterCoils->WaterCoil(CoilNum).SatEnthlCurveSlope / (WaterMassFlowRate * Cp * ConvK));
            if (expon < 20.0) { // CR7189 changed from ABS(expon) < 20
                //       negative expon can happen, but lead to tiny WetCoilCoef that aren't a problem
                WetCoilCoeff = std::exp(expon);
                // following appears similar to eq. 320 in Eng Ref but neglects K1 term
                TempWaterOut = ((1.0 - WetCoilCoeff) * (InletAirEnthalpy - state.dataWaterCoils->WaterCoil(CoilNum).SatEnthlCurveConstCoef) +
                                WetCoilCoeff * TempWaterIn * (state.dataWaterCoils->WaterCoil(CoilNum).SatEnthlCurveSlope - ScaledWaterSpecHeat)) /
                               (state.dataWaterCoils->WaterCoil(CoilNum).SatEnthlCurveSlope - WetCoilCoeff * ScaledWaterSpecHeat);
            } else {
                // following appears to be same as above with equation simplified to use only significant terms when WetCoilCoeff very large
                TempWaterOut = ((InletAirEnthalpy - state.dataWaterCoils->WaterCoil(CoilNum).SatEnthlCurveConstCoef) -
                                TempWaterIn * (state.dataWaterCoils->WaterCoil(CoilNum).SatEnthlCurveSlope - ScaledWaterSpecHeat)) /
                               ScaledWaterSpecHeat;
            }
            //      above is inverted form of WaterMassFlowRate*cpw*(TempWaterOut-TempWaterIn) = UA(LMHD)
            //      note simplification that hsat = WaterCoil(CoilNum)%SatEnthlCurveConstCoef +  &
            //                                      WaterCoil(CoilNum)%SatEnthlCurveSlope*WetSideEffctvWaterTemp
            MeanWaterTemp = 0.5 * (TempWaterIn + TempWaterOut);
            OutletAirEnthalpy = InletAirEnthalpy - (TempWaterOut - TempWaterIn) * ScaledWaterSpecHeat;

            InsdToOutsdThermResistRatio = (TubeFoulThermResist + ScaledWaterToTubeThermResist) / ScaledCoilAirThermResistWetSurf;
            InCoilSurfTemp = UACoilAllWet * ScaledCoilAirThermResistWetSurf *
                             (state.dataWaterCoils->WaterCoil(CoilNum).SatEnthlCurveSlope * TempWaterIn +
                              (OutletAirEnthalpy - state.dataWaterCoils->WaterCoil(CoilNum).SatEnthlCurveConstCoef) * InsdToOutsdThermResistRatio);
            OutCoilSurfTemp = UACoilAllWet * ScaledCoilAirThermResistWetSurf *
                              (state.dataWaterCoils->WaterCoil(CoilNum).SatEnthlCurveSlope * TempWaterOut +
                               (InletAirEnthalpy - state.dataWaterCoils->WaterCoil(CoilNum).SatEnthlCurveConstCoef) * InsdToOutsdThermResistRatio);

            if (std::abs(MeanWaterTemp - WetSideEffctvWaterTemp) > 0.01) {
                WetSideEffctvWaterTemp = MeanWaterTemp;
                InSurfTempSatAirEnthl = PsyHFnTdbRhPb(state, InCoilSurfTemp, unity, state.dataEnvrn->OutBaroPress, RoutineName) * ConvK;
                OutSurfTempSatAirEnthl = PsyHFnTdbRhPb(state, OutCoilSurfTemp, unity, state.dataEnvrn->OutBaroPress, RoutineName) * ConvK;

                state.dataWaterCoils->WaterCoil(CoilNum).SatEnthlCurveSlope =
                    (OutSurfTempSatAirEnthl - InSurfTempSatAirEnthl) / (OutCoilSurfTemp - InCoilSurfTemp);
                state.dataWaterCoils->WaterCoil(CoilNum).SatEnthlCurveConstCoef =
                    InSurfTempSatAirEnthl - state.dataWaterCoils->WaterCoil(CoilNum).SatEnthlCurveSlope * InCoilSurfTemp;
            } else {
                WaterTempConvg = true;
            }
        } // End of iteration loop to get MeanWaterTemp=WetSideEffctvWaterTemp
        //      if 8 CoolCoilErrs are reached without convergence and the
        //      predicted coil surface temperature at the outlet is less than
        //      the dew point coil is apparently all wet but a solution
        //      cannot be obtained
        if (!WaterTempConvg && !state.dataGlobal->WarmupFlag && (OutCoilSurfTemp < EnterAirDewPoint)) {
            ShowRecurringWarningErrorAtEnd(state,
                                           state.dataWaterCoils->WaterCoil(CoilNum).Name +
                                               " not converged (8 iterations) due to \"Wet Convergence\" conditions.",
                                           state.dataWaterCoils->WaterTempCoolCoilErrs(CoilNum),
                                           std::abs(MeanWaterTemp - WetSideEffctvWaterTemp),
                                           std::abs(MeanWaterTemp - WetSideEffctvWaterTemp));
            //       CoolCoilErrs = CoolCoilErrs + 1
            //       IF (CoolCoilErrs .LE. MaxCoolCoilErrs) THEN
            //          CALL ShowWarningError(state, 'tp12c0:  not converged in 8 CoolCoilErrs')
            //       END IF
        }
        state.dataWaterCoils->WaterCoil(CoilNum).MeanWaterTempSaved = MeanWaterTemp;
        //      now simulate wet dry coil - test outlet condition from all
        //      wet case to give an idea of the expected solution
        PartWetIterations = 0;
        WetDryInterSurfTempError = 0.0;
        CoilPartWetConvg = false;
        //      Surface temp at coil water outlet (air inlet) is less than
        //      the dew point - Coil must be completely wet so no need to
        //      simulate wet/dry case
        if (OutCoilSurfTemp < EnterAirDewPoint) {
            CoilPartWetConvg = true;
            state.dataWaterCoils->WaterCoil(CoilNum).SurfAreaWetFraction = 1.0;
            TotWaterCoilLoad = AirMassFlow * (InletAirEnthalpy - OutletAirEnthalpy);
            AirWetDryInterfcTemp = TempAirIn;
            WetDryInterfcAirEnthl = InletAirEnthalpy;
            //      Surface temperature at coil water inlet is greater than the
            //      dewpoint - coil cannot be all wet but may be all dry -
            //      initialise with all dry solution
        } else if (InCoilSurfTemp > EnterAirDewPoint) {
            SurfAreaWet = 0.0;
            state.dataWaterCoils->WaterCoil(CoilNum).SurfAreaWetFraction = 0.0;
            WetDryInterfcWaterTemp = TempWaterIn;
            TempWaterOut = state.dataWaterCoils->WaterCoil(CoilNum).OutWaterTempSaved +
                           (TempWaterIn - state.dataWaterCoils->WaterCoil(CoilNum).InWaterTempSaved);
            WetAreaLast = 0.05 * state.dataWaterCoils->WaterCoil(CoilNum).TotCoilOutsideSurfArea;
            //      General case - must be part-wet/part-dry - initialise
            //      accordingly with some non-zero wet area
        } else {
            if (state.dataWaterCoils->WaterCoil(CoilNum).SurfAreaWetSaved != 0.0) {
                SurfAreaWet = state.dataWaterCoils->WaterCoil(CoilNum).SurfAreaWetSaved;
            } else {
                SurfAreaWet = 0.8 * state.dataWaterCoils->WaterCoil(CoilNum).TotCoilOutsideSurfArea * (EnterAirDewPoint - InCoilSurfTemp) /
                              (OutCoilSurfTemp - InCoilSurfTemp);
            }
            WetDryInterfcWaterTemp = TempWaterIn + EnterAirDewPoint - InCoilSurfTemp;
            WetAreaLast = 0.0;
        }
        //       Loop to solve partly wet coil, converges on wet area and
        //       boundary temperature at dew point
        //       Dry coil is special case with zero wet area, converges on
        //       mean water temperature
        while (PartWetIterations < 40 && !CoilPartWetConvg) {
            ++PartWetIterations;
            //      effective water temp on dry side of coil
            DrySideEffectiveWaterTemp = 0.5 * (TempWaterOut + WetDryInterfcWaterTemp);
            //      tube inside thermal resistance
            DryCoilInThermResist = WaterToTubeThermResist / (1.0 + 0.0146 * DrySideEffectiveWaterTemp);
            //      overall UA, from water to air, of dry portion of coil

            UADryCoil = (state.dataWaterCoils->WaterCoil(CoilNum).TotCoilOutsideSurfArea - SurfAreaWet) /
                        (state.dataWaterCoils->WaterCoil(CoilNum).TotCoilOutsideSurfArea *
                         (TubeFoulThermResist + DryCoilInThermResist + CoilToAirThermResistDrySurf));

            // This is a numerical trap for a very small number in the EXP function that is approaching zero
            if (UADryCoil * DryCoilCoeff1 < -60.0) {
                DryCoilCoeff = 0.0;
            } else {
                DryCoilCoeff = std::exp(UADryCoil * DryCoilCoeff1);
            }

            K1 = WaterMassFlowRate * Cp * ConvK * (DryCoilCoeff - 1.0) /
                 (WaterMassFlowRate * Cp * ConvK * DryCoilCoeff - AirMassFlow * MoistAirSpecificHeat);
            if (SurfAreaWet != 0) {
                state.dataWaterCoils->WaterCoil(CoilNum).SurfAreaWetFraction =
                    SurfAreaWet / state.dataWaterCoils->WaterCoil(CoilNum).TotCoilOutsideSurfArea;
                //      effective water temp on wet side of coil
                WetSideEffctvWaterTemp = 0.5 * (TempWaterIn + WetDryInterfcWaterTemp);
                //      tube inside thermal resistance
                ScaledWaterToTubeThermResist = WaterToTubeThermResist / (1.0 + 0.0146 * WetSideEffctvWaterTemp);
                ScaledCoilAirThermResistWetSurf = CoilToAirThermResistWetSurf / state.dataWaterCoils->WaterCoil(CoilNum).EnthVsTempCurveAppxSlope;
                //      overall UA, from water to air, of wet portion of coil
                UACoilAllWet = 1.0 / (state.dataWaterCoils->WaterCoil(CoilNum).EnthVsTempCurveAppxSlope *
                                      (TubeFoulThermResist + ScaledWaterToTubeThermResist + ScaledCoilAirThermResistWetSurf));
                UACoilPartWet = state.dataWaterCoils->WaterCoil(CoilNum).SurfAreaWetFraction * UACoilAllWet;
                expon = UACoilPartWet *
                        (1.0 / AirMassFlow - state.dataWaterCoils->WaterCoil(CoilNum).EnthVsTempCurveAppxSlope / (WaterMassFlowRate * Cp * ConvK));
                //        prevents floating point error when taking exponential
                //        of a very large number
                if (expon < 20.0) {
                    WetCoilCoeff = std::exp(expon);
                    //          write(outputfiledebug,*) ' wcc=',wetcoilcoeff
                    denom = (state.dataWaterCoils->WaterCoil(CoilNum).EnthVsTempCurveAppxSlope - WetCoilCoeff * ScaledWaterSpecHeat -
                             (1.0 - WetCoilCoeff) * K1 * MoistAirSpecificHeat);
                    //          write(outputfiledebug,*) ' denom=',denom
                    //          WetDryInterfcWaterTemp = ((1.0 - WetCoilCoeff) * (InletAirEnthalpy - WaterCoil(CoilNum)%EnthVsTempCurveConst -
                    //          K1
                    //          *  &
                    //                                     MoistAirSpecificHeat * TempAirIn) + WetCoilCoeff * &
                    //                                     TempWaterIn * (WaterCoil(CoilNum)%EnthVsTempCurveAppxSlope -  &
                    //                                     ScaledWaterSpecHeat)) / (WaterCoil(CoilNum)%EnthVsTempCurveAppxSlope -  &
                    //                                      WetCoilCoeff * ScaledWaterSpecHeat - (1.0 - WetCoilCoeff) * K1 * &
                    //                                     MoistAirSpecificHeat)
                    WetDryInterfcWaterTemp =
                        ((1.0 - WetCoilCoeff) * (InletAirEnthalpy - state.dataWaterCoils->WaterCoil(CoilNum).EnthVsTempCurveConst -
                                                 K1 * MoistAirSpecificHeat * TempAirIn) +
                         WetCoilCoeff * TempWaterIn * (state.dataWaterCoils->WaterCoil(CoilNum).EnthVsTempCurveAppxSlope - ScaledWaterSpecHeat)) /
                        denom;
                } else {
                    //         approximation to equation for WetDryInterfcWaterTemp when WetCoilCoeff-->inf.
                    WetDryInterfcWaterTemp =
                        (TempWaterIn * (state.dataWaterCoils->WaterCoil(CoilNum).EnthVsTempCurveAppxSlope - ScaledWaterSpecHeat) -
                         (InletAirEnthalpy - state.dataWaterCoils->WaterCoil(CoilNum).EnthVsTempCurveConst - K1 * MoistAirSpecificHeat * TempAirIn)) /
                        (K1 * MoistAirSpecificHeat - ScaledWaterSpecHeat);
                }
            }
            //        air temperature at wet-dry interface
            AirWetDryInterfcTemp = TempAirIn - (TempAirIn - WetDryInterfcWaterTemp) * K1;
            //        coil surface temperature at wet-dry interface
            WetDryInterfcSurfTemp = WetDryInterfcWaterTemp + (AirWetDryInterfcTemp - WetDryInterfcWaterTemp) *
                                                                 (TubeFoulThermResist + DryCoilInThermResist) /
                                                                 (TubeFoulThermResist + DryCoilInThermResist + CoilToAirThermResistDrySurf);
            if (SurfAreaWet != 0) {
                WetDryInterfcAirEnthl = InletAirEnthalpy - MoistAirSpecificHeat * (TempAirIn - AirWetDryInterfcTemp);
                //        conservation of energy - wet portion of coil
                OutletAirEnthalpy = WetDryInterfcAirEnthl - WaterMassFlowRate * Cp * ConvK * (WetDryInterfcWaterTemp - TempWaterIn) / AirMassFlow;
                //        ratio of inside to outside thermal resistance
                InsdToOutsdThermResistRatio = (TubeFoulThermResist + ScaledWaterToTubeThermResist) / ScaledCoilAirThermResistWetSurf;
                //        coil surface temperature at water inlet (air outlet)
                InCoilSurfTemp = UACoilAllWet * ScaledCoilAirThermResistWetSurf *
                                 (state.dataWaterCoils->WaterCoil(CoilNum).EnthVsTempCurveAppxSlope * TempWaterIn +
                                  (OutletAirEnthalpy - state.dataWaterCoils->WaterCoil(CoilNum).EnthVsTempCurveConst) * InsdToOutsdThermResistRatio);
                WetDryInterSurfTempErrorLast = WetDryInterSurfTempError;
                //        in part-wet/part-dry solution EnterAirDewPoint=WetDryInterfcSurfTemp drives WetDryInterSurfTempError->0
                WetDryInterSurfTempError = EnterAirDewPoint - WetDryInterfcSurfTemp;
            } else {
                //        dry coil solution
                WetDryInterfcAirEnthl = 0.0;
                OutletAirEnthalpy = InletAirEnthalpy - MoistAirSpecificHeat * (TempAirIn - AirWetDryInterfcTemp);
            }
            //        total cooling = change in air enthalpy across coil
            TotWaterCoilLoad = AirMassFlow * (InletAirEnthalpy - OutletAirEnthalpy);
            //        conservation of energy on water stream gives water outlet
            //        temperature
            TempWaterOut = WaterMassFlowRate * Cp * ConvK; // Temp for next calc
            TempWaterOut = min(TempWaterIn + TotWaterCoilLoad / TempWaterOut, TempAirIn);
            //        update estimate of coil wet area

            if (SurfAreaWet == 0) {
                MeanWaterTemp = 0.5 * (TempWaterOut + WetDryInterfcWaterTemp);
                if (EnterAirDewPoint > WetDryInterfcSurfTemp) {
                    SurfAreaWet = 0.5 * WetAreaLast;
                } else if (std::abs(MeanWaterTemp - DrySideEffectiveWaterTemp) <= 0.00002) {
                    CoilPartWetConvg = true;
                }
            } else if (std::abs(WetDryInterSurfTempError) > 0.00002 ||
                       std::abs(SurfAreaWet - WetAreaLast) / state.dataWaterCoils->WaterCoil(CoilNum).TotCoilOutsideSurfArea > 0.00001) {
                if (WetAreaLast == 0) {
                    WetAreaLast = SurfAreaWet;
                    SurfAreaWet += 0.4 * state.dataWaterCoils->WaterCoil(CoilNum).TotCoilOutsideSurfArea * WetDryInterSurfTempError /
                                   (OutCoilSurfTemp - InCoilSurfTemp);
                } else if (WetDryInterSurfTempError != WetDryInterSurfTempErrorLast) {
                    WetAreaChange = SurfAreaWet - WetAreaLast;
                    WetAreaLast = SurfAreaWet;
                    SurfAreaWet -= 0.8 * WetDryInterSurfTempError * WetAreaChange / (WetDryInterSurfTempError - WetDryInterSurfTempErrorLast);
                }
                if (SurfAreaWet >= state.dataWaterCoils->WaterCoil(CoilNum).TotCoilOutsideSurfArea) {
                    SurfAreaWet = state.dataWaterCoils->WaterCoil(CoilNum).TotCoilOutsideSurfArea;
                    MeanWaterTemp = 0.5 * (TempWaterIn + WetDryInterfcWaterTemp);
                    if (WetAreaLast == state.dataWaterCoils->WaterCoil(CoilNum).TotCoilOutsideSurfArea &&
                        std::abs(MeanWaterTemp - WetSideEffctvWaterTemp) <= 0.00002) {
                        CoilPartWetConvg = true;
                    }
                }
                if (SurfAreaWet <= 0) {
                    SurfAreaWet = 0.0;
                    state.dataWaterCoils->WaterCoil(CoilNum).SurfAreaWetFraction = 0.0;
                    WetDryInterfcWaterTemp = TempWaterIn;
                }
                InSurfTempSatAirEnthl = PsyHFnTdbRhPb(state, InCoilSurfTemp, unity, state.dataEnvrn->OutBaroPress, RoutineName) * ConvK;
                if ((EnterAirDewPoint - InCoilSurfTemp) >= 0.0001) {
                    AirEnthAtWetDryIntrfcSurfTemp = PsyHFnTdbRhPb(state, EnterAirDewPoint, unity, state.dataEnvrn->OutBaroPress, RoutineName) * ConvK;
                    state.dataWaterCoils->WaterCoil(CoilNum).EnthVsTempCurveAppxSlope =
                        (AirEnthAtWetDryIntrfcSurfTemp - InSurfTempSatAirEnthl) / (EnterAirDewPoint - InCoilSurfTemp);
                } else {
                    AirEnthAtWetDryIntrfcSurfTemp =
                        PsyHFnTdbRhPb(state, InCoilSurfTemp + 0.0001, unity, state.dataEnvrn->OutBaroPress, RoutineName) * ConvK;
                    state.dataWaterCoils->WaterCoil(CoilNum).EnthVsTempCurveAppxSlope =
                        (AirEnthAtWetDryIntrfcSurfTemp - InSurfTempSatAirEnthl) / 0.0001;
                }
                state.dataWaterCoils->WaterCoil(CoilNum).EnthVsTempCurveConst =
                    InSurfTempSatAirEnthl - state.dataWaterCoils->WaterCoil(CoilNum).EnthVsTempCurveAppxSlope * InCoilSurfTemp;
            } else {
                CoilPartWetConvg = true;
            }
        }
        //      error checking to see if convergence has been achieved
        if (!CoilPartWetConvg && !state.dataGlobal->WarmupFlag) {
            ShowRecurringWarningErrorAtEnd(state,
                                           state.dataWaterCoils->WaterCoil(CoilNum).Name +
                                               " not converged (40 iterations) due to \"Partial Wet Convergence\" conditions.",
                                           state.dataWaterCoils->PartWetCoolCoilErrs(CoilNum));
            //      CoolCoilErrs = CoolCoilErrs + 1
            //      IF (CoolCoilErrs .LE. MaxCoolCoilErrs) THEN
            //        CALL ShowWarningError(state, 'tp12c0:  not converged in 20 CoolCoilErrs')
            //      END IF
        }
        if (state.dataWaterCoils->WaterCoil(CoilNum).SurfAreaWetFraction > 0 && state.dataWaterCoils->WaterCoil(CoilNum).SurfAreaWetFraction < 1) {
            state.dataWaterCoils->WaterCoil(CoilNum).SurfAreaWetSaved = SurfAreaWet;
        }
        //       calculate TempAirOut, OutletAirHumRat, and SensCoolRate based on equations from
        //       TYPE12 and the ASHRAE toolkit
        if (state.dataWaterCoils->WaterCoil(CoilNum).SurfAreaWetFraction == 0) {
            //       dry coil
            TempAirOut = TempAirIn - TotWaterCoilLoad / (AirMassFlow * MoistAirSpecificHeat);
            OutletAirHumRat = InletAirHumRat;
            SenWaterCoilLoad = TotWaterCoilLoad;
        } else {
            //       coil effectiveness
            expon = state.dataWaterCoils->WaterCoil(CoilNum).SurfAreaWetFraction / (CoilToAirThermResistWetSurf * AirMassFlow);
            y = 0.0;
            if (expon < 20.0) y = std::exp(-expon);
            AirExitEnthlAtCoilSurfTemp = WetDryInterfcAirEnthl - (WetDryInterfcAirEnthl - OutletAirEnthalpy) / (1.0 - y);
            AirExitCoilSurfTemp = AirExitEnthlAtCoilSurfTemp / ConvK; // TEmporary calc
            AirExitCoilSurfTemp = PsyTsatFnHPb(state, AirExitCoilSurfTemp, state.dataEnvrn->OutBaroPress);
            //       Implementation of epsilon*NTU method
            TempAirOut = AirExitCoilSurfTemp + (AirWetDryInterfcTemp - AirExitCoilSurfTemp) * y;
            OutletAirHumRat = PsyWFnTdbH(state, TempAirOut, 1000.0 * OutletAirEnthalpy, RoutineName);
            SenWaterCoilLoad = AirMassFlow * (PsyCpAirFnW(InletAirHumRat) * TempAirIn - PsyCpAirFnW(OutletAirHumRat) * TempAirOut) * ConvK;
        }

        if (FanOpMode == CycFanCycCoil) {
            TotWaterCoilLoad *= PartLoadRatio;
            SenWaterCoilLoad *= PartLoadRatio;
        }

        // Set the outlet conditions
        state.dataWaterCoils->WaterCoil(CoilNum).TotWaterCoolingCoilRate = TotWaterCoilLoad * 1000.0;
        state.dataWaterCoils->WaterCoil(CoilNum).SenWaterCoolingCoilRate = SenWaterCoilLoad * 1000.0;
        state.dataWaterCoils->WaterCoil(CoilNum).OutletAirTemp = TempAirOut;
        state.dataWaterCoils->WaterCoil(CoilNum).OutletWaterTemp = TempWaterOut;
        state.dataWaterCoils->WaterCoil(CoilNum).OutletAirEnthalpy = OutletAirEnthalpy * 1000.0;
        state.dataWaterCoils->WaterCoil(CoilNum).OutletAirHumRat = OutletAirHumRat;
        // The CoolingCoilLoad is the change in the enthalpy of the water
        state.dataWaterCoils->WaterCoil(CoilNum).OutletWaterEnthalpy =
            state.dataWaterCoils->WaterCoil(CoilNum).InletWaterEnthalpy +
            state.dataWaterCoils->WaterCoil(CoilNum).TotWaterCoolingCoilRate / state.dataWaterCoils->WaterCoil(CoilNum).InletWaterMassFlowRate;

        // This WaterCoil does not change the Mass Flow across the component
        state.dataWaterCoils->WaterCoil(CoilNum).OutletAirMassFlowRate = state.dataWaterCoils->WaterCoil(CoilNum).InletAirMassFlowRate;
        state.dataWaterCoils->WaterCoil(CoilNum).OutletWaterMassFlowRate = state.dataWaterCoils->WaterCoil(CoilNum).InletWaterMassFlowRate;
    } else {
        // If Coil is scheduled OFF then Outlet conditions are set to Inlet Conditions
        state.dataWaterCoils->WaterCoil(CoilNum).TotWaterCoolingCoilRate = 0.0;
        state.dataWaterCoils->WaterCoil(CoilNum).SenWaterCoolingCoilRate = 0.0;
        TempAirOut = TempAirIn;
        TempWaterOut = TempWaterIn;
        // set the outlet conditions to the coil derived type
        state.dataWaterCoils->WaterCoil(CoilNum).OutletAirTemp = TempAirOut;
        state.dataWaterCoils->WaterCoil(CoilNum).OutletWaterTemp = TempWaterOut;
        state.dataWaterCoils->WaterCoil(CoilNum).OutletAirEnthalpy = state.dataWaterCoils->WaterCoil(CoilNum).InletAirEnthalpy;
        state.dataWaterCoils->WaterCoil(CoilNum).OutletAirHumRat = state.dataWaterCoils->WaterCoil(CoilNum).InletAirHumRat;
        // The CoolingCoilLoad is the change in the enthalpy of the water
        state.dataWaterCoils->WaterCoil(CoilNum).OutletWaterEnthalpy = state.dataWaterCoils->WaterCoil(CoilNum).InletWaterEnthalpy;

        // This WaterCoil does not change the Mass Flow across the component
        state.dataWaterCoils->WaterCoil(CoilNum).OutletAirMassFlowRate = state.dataWaterCoils->WaterCoil(CoilNum).InletAirMassFlowRate;
        state.dataWaterCoils->WaterCoil(CoilNum).OutletWaterMassFlowRate = 0.0;
    }

    // Save some of the Values for next Time step
    state.dataWaterCoils->WaterCoil(CoilNum).InWaterTempSaved = TempWaterIn;
    state.dataWaterCoils->WaterCoil(CoilNum).OutWaterTempSaved = TempWaterOut;
}

void CoolingCoil(EnergyPlusData &state,
                 int const CoilNum,
                 bool const FirstHVACIteration,
                 int const CalcMode,
                 int const FanOpMode,       // fan operating mode
                 Real64 const PartLoadRatio // part-load ratio of heating coil
)
{

    // FUNCTION INFORMATION:
    // AUTHOR         Rahul Chillar
    // DATE WRITTEN   Mar 2004
    // MODIFIED       na
    // RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // The subroutine has the coil logic. Three types of Cooling Coils exist:
    // They are 1.CoilDry , 2.CoilWet, 3. CoilPartDryPartWet. The logic for
    // the three individual cases is in this subroutine.

    // METHODOLOGY EMPLOYED:
    // Simulates a Coil Model from Design conditions and subsequently uses
    // configuration values (example: UA)calculated from those design conditions
    // to calculate new performance of coil from operating inputs.The values are
    // calculated in the Subroutine InitWaterCoil

    // REFERENCES:
    // ASHRAE Secondary HVAC Toolkit TRNSYS.  1990.  A Transient System
    // Simulation Program: Reference Manual. Solar Energy Laboratory, Univ. Wisconsin-
    // Madison, pp. 4.6.8-1 - 4.6.8-12.
    // Threlkeld, J.L.  1970.  Thermal Environmental Engineering, 2nd Edition,
    // Englewood Cliffs: Prentice-Hall,Inc. pp. 254-270.

    // Using/Aliasing
    using General::SafeDivide;

    // Enforce explicit typing of all variables in this routine

    // Locals
    // FUNCTION ARGUMENT DEFINITIONS:

    // FUNCTION PARAMETER DEFINITIONS:
    // na

    // INTERFACE BLOCK SPECIFICATIONS
    // na

    // DERIVED TYPE DEFINITIONS
    // na

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    Real64 AirInletCoilSurfTemp; // Coil surface temperature at air entrance(C)
    Real64 AirDewPointTemp;      // Temperature dew point at operating condition
    Real64 OutletAirTemp;        // Outlet air temperature at operating condition
    Real64 OutletAirHumRat;      // Outlet air humidity ratio at operating condition
    Real64 OutletWaterTemp;      // Outlet water temperature at operating condtitons
    Real64 TotWaterCoilLoad;     // Total heat transfer rate(W)
    Real64 SenWaterCoilLoad;     // Sensible heat transfer rate
    Real64 SurfAreaWetFraction;  // Fraction of surface area wet
    Real64 AirMassFlowRate;      // Air mass flow rate for the calculation

    AirInletCoilSurfTemp = 0.0; // Coil surface temperature at air entrance(C)
    AirDewPointTemp = 0.0;      // Temperature dew point at operating condition
    OutletAirTemp = 0.0;        // Outlet air temperature at operating condition
    OutletAirHumRat = 0.0;      // Outlet air humidity ratio at operating condition
    OutletWaterTemp = 0.0;      // Outlet water temperature at operating condtitons
    TotWaterCoilLoad = 0.0;     // Total heat transfer rate(W)
    SenWaterCoilLoad = 0.0;     // Sensible heat transfer rate
    SurfAreaWetFraction = 0.0;  // Fraction of surface area wet

    if (FanOpMode == CycFanCycCoil && PartLoadRatio > 0.0) { // FB Start
        AirMassFlowRate = state.dataWaterCoils->WaterCoil(CoilNum).InletAirMassFlowRate / PartLoadRatio;
    } else {
        AirMassFlowRate = state.dataWaterCoils->WaterCoil(CoilNum).InletAirMassFlowRate;
    }

    // If Coil is Scheduled ON then do the simulation
    if (((GetCurrentScheduleValue(state, state.dataWaterCoils->WaterCoil(CoilNum).SchedPtr) > 0.0) &&
         (state.dataWaterCoils->WaterCoil(CoilNum).InletWaterMassFlowRate > 0.0) && (AirMassFlowRate >= WaterCoils::MinAirMassFlow) &&
         (state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate > 0.0) &&
         (state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterMassFlowRate > 0.0)) ||
        (CalcMode == state.dataWaterCoils->DesignCalc)) {

        // Calculate Temperature Dew Point at operating conditions.
        AirDewPointTemp = PsyTdpFnWPb(state, state.dataWaterCoils->WaterCoil(CoilNum).InletAirHumRat, state.dataEnvrn->OutBaroPress);

        {
            auto const SELECT_CASE_var(state.dataWaterCoils->WaterCoil(CoilNum).CoolingCoilAnalysisMode);
            if (SELECT_CASE_var == state.dataWaterCoils->DetailedAnalysis) {
                // Coil is completely dry if AirDewPointTemp is less than InletWaterTemp,hence Call CoilCompletelyDry
                if (AirDewPointTemp <= state.dataWaterCoils->WaterCoil(CoilNum).InletWaterTemp) {

                    // Calculate the leaving conditions and performance of dry coil
                    CoilCompletelyDry(state,
                                      CoilNum,
                                      state.dataWaterCoils->WaterCoil(CoilNum).InletWaterTemp,
                                      state.dataWaterCoils->WaterCoil(CoilNum).InletAirTemp,
                                      state.dataWaterCoils->WaterCoil(CoilNum).UACoilTotal,
                                      OutletWaterTemp,
                                      OutletAirTemp,
                                      OutletAirHumRat,
                                      TotWaterCoilLoad,
                                      FanOpMode,
                                      PartLoadRatio);

                    SenWaterCoilLoad = TotWaterCoilLoad;
                    SurfAreaWetFraction = 0.0;

                } else {
                    // Else If AirDewPointTemp is greater than InletWaterTemp then assume the
                    // external surface of coil is completely wet,hence Call CoilCompletelyWet
                    // Calculate the leaving conditions and performance of wet coil
                    CoilCompletelyWet(state,
                                      CoilNum,
                                      state.dataWaterCoils->WaterCoil(CoilNum).InletWaterTemp,
                                      state.dataWaterCoils->WaterCoil(CoilNum).InletAirTemp,
                                      state.dataWaterCoils->WaterCoil(CoilNum).InletAirHumRat,
                                      state.dataWaterCoils->WaterCoil(CoilNum).UACoilInternal,
                                      state.dataWaterCoils->WaterCoil(CoilNum).UACoilExternal,
                                      OutletWaterTemp,
                                      OutletAirTemp,
                                      OutletAirHumRat,
                                      TotWaterCoilLoad,
                                      SenWaterCoilLoad,
                                      SurfAreaWetFraction,
                                      AirInletCoilSurfTemp,
                                      FanOpMode,
                                      PartLoadRatio);

                    // If AirDewPointTemp is less than temp of coil surface at entry of air
                    if (AirDewPointTemp < AirInletCoilSurfTemp) {

                        // Then coil is partially wet and dry hence call CoilPartWetPartDry
                        // Calculate the leaving conditions and performance of dry coil
                        CoilPartWetPartDry(state,
                                           CoilNum,
                                           FirstHVACIteration,
                                           state.dataWaterCoils->WaterCoil(CoilNum).InletWaterTemp,
                                           state.dataWaterCoils->WaterCoil(CoilNum).InletAirTemp,
                                           AirDewPointTemp,
                                           OutletWaterTemp,
                                           OutletAirTemp,
                                           OutletAirHumRat,
                                           TotWaterCoilLoad,
                                           SenWaterCoilLoad,
                                           SurfAreaWetFraction,
                                           FanOpMode,
                                           PartLoadRatio);

                    } // End if for part wet part dry coil
                }     // End if for dry coil

            } else if (SELECT_CASE_var == state.dataWaterCoils->SimpleAnalysis) {
                // Coil is completely dry if AirDewPointTemp is less than InletWaterTemp,hence Call CoilCompletelyDry
                if (AirDewPointTemp <= state.dataWaterCoils->WaterCoil(CoilNum).InletWaterTemp) {

                    // Calculate the leaving conditions and performance of dry coil
                    CoilCompletelyDry(state,
                                      CoilNum,
                                      state.dataWaterCoils->WaterCoil(CoilNum).InletWaterTemp,
                                      state.dataWaterCoils->WaterCoil(CoilNum).InletAirTemp,
                                      state.dataWaterCoils->WaterCoil(CoilNum).UACoilTotal,
                                      OutletWaterTemp,
                                      OutletAirTemp,
                                      OutletAirHumRat,
                                      TotWaterCoilLoad,
                                      FanOpMode,
                                      PartLoadRatio);

                    SenWaterCoilLoad = TotWaterCoilLoad;
                    SurfAreaWetFraction = 0.0;

                } else {
                    // Else If AirDewPointTemp is greater than InletWaterTemp then assume the
                    // external surface of coil is completely wet,hence Call CoilCompletelyWet
                    // Calculate the leaving conditions and performance of wet coil
                    CoilCompletelyWet(state,
                                      CoilNum,
                                      state.dataWaterCoils->WaterCoil(CoilNum).InletWaterTemp,
                                      state.dataWaterCoils->WaterCoil(CoilNum).InletAirTemp,
                                      state.dataWaterCoils->WaterCoil(CoilNum).InletAirHumRat,
                                      state.dataWaterCoils->WaterCoil(CoilNum).UACoilInternal,
                                      state.dataWaterCoils->WaterCoil(CoilNum).UACoilExternal,
                                      OutletWaterTemp,
                                      OutletAirTemp,
                                      OutletAirHumRat,
                                      TotWaterCoilLoad,
                                      SenWaterCoilLoad,
                                      SurfAreaWetFraction,
                                      AirInletCoilSurfTemp,
                                      FanOpMode,
                                      PartLoadRatio);

                } // End if for dry coil
            }
        }

        // Report outlet variables at nodes
        state.dataWaterCoils->WaterCoil(CoilNum).OutletAirTemp = OutletAirTemp;
        state.dataWaterCoils->WaterCoil(CoilNum).OutletAirHumRat = OutletAirHumRat;
        state.dataWaterCoils->WaterCoil(CoilNum).OutletWaterTemp = OutletWaterTemp;
        // Report output results if the coil was operating

        if (FanOpMode == CycFanCycCoil) {
            TotWaterCoilLoad *= PartLoadRatio;
            SenWaterCoilLoad *= PartLoadRatio;
        }

        state.dataWaterCoils->WaterCoil(CoilNum).TotWaterCoolingCoilRate = TotWaterCoilLoad;
        state.dataWaterCoils->WaterCoil(CoilNum).SenWaterCoolingCoilRate = SenWaterCoilLoad;
        state.dataWaterCoils->WaterCoil(CoilNum).SurfAreaWetFraction = SurfAreaWetFraction;
        //       WaterCoil(CoilNum)%OutletWaterEnthalpy = WaterCoil(CoilNum)%InletWaterEnthalpy+ &
        //                                WaterCoil(CoilNum)%TotWaterCoolingCoilRate/WaterCoil(CoilNum)%InletWaterMassFlowRate
        state.dataWaterCoils->WaterCoil(CoilNum).OutletWaterEnthalpy =
            state.dataWaterCoils->WaterCoil(CoilNum).InletWaterEnthalpy + SafeDivide(state.dataWaterCoils->WaterCoil(CoilNum).TotWaterCoolingCoilRate,
                                                                                     state.dataWaterCoils->WaterCoil(CoilNum).InletWaterMassFlowRate);

    } else {
        // If both mass flow rates are zero, set outputs to inputs and return
        state.dataWaterCoils->WaterCoil(CoilNum).OutletWaterTemp = state.dataWaterCoils->WaterCoil(CoilNum).InletWaterTemp;
        state.dataWaterCoils->WaterCoil(CoilNum).OutletAirTemp = state.dataWaterCoils->WaterCoil(CoilNum).InletAirTemp;
        state.dataWaterCoils->WaterCoil(CoilNum).OutletAirHumRat = state.dataWaterCoils->WaterCoil(CoilNum).InletAirHumRat;
        state.dataWaterCoils->WaterCoil(CoilNum).OutletWaterEnthalpy = state.dataWaterCoils->WaterCoil(CoilNum).InletWaterEnthalpy;
        state.dataWaterCoils->WaterCoil(CoilNum).TotWaterCoolingCoilEnergy = 0.0;
        state.dataWaterCoils->WaterCoil(CoilNum).SenWaterCoolingCoilEnergy = 0.0;
        state.dataWaterCoils->WaterCoil(CoilNum).SurfAreaWetFraction = 0.0;

    } // End of the Flow or No flow If block
    state.dataWaterCoils->WaterCoil(CoilNum).OutletWaterMassFlowRate = state.dataWaterCoils->WaterCoil(CoilNum).InletWaterMassFlowRate;
    state.dataWaterCoils->WaterCoil(CoilNum).OutletAirMassFlowRate = state.dataWaterCoils->WaterCoil(CoilNum).InletAirMassFlowRate;
    state.dataWaterCoils->WaterCoil(CoilNum).OutletAirEnthalpy =
        PsyHFnTdbW(state.dataWaterCoils->WaterCoil(CoilNum).OutletAirTemp, state.dataWaterCoils->WaterCoil(CoilNum).OutletAirHumRat);
}

// End Algorithm Section of the Module

// Coil Completely Dry Subroutine for Cooling Coil

void CoilCompletelyDry(EnergyPlusData &state,
                       int const CoilNum,
                       Real64 const WaterTempIn,  // Entering water temperature
                       Real64 const AirTempIn,    // Entering air dry bulb temperature
                       Real64 const CoilUA,       // Overall heat transfer coefficient
                       Real64 &OutletWaterTemp,   // Leaving water temperature
                       Real64 &OutletAirTemp,     // Leaving air dry bulb temperature
                       Real64 &OutletAirHumRat,   // Leaving air humidity ratio
                       Real64 &Q,                 // Heat transfer rate
                       int const FanOpMode,       // fan operating mode
                       Real64 const PartLoadRatio // part-load ratio of heating coil
)
{

    // FUNCTION INFORMATION:
    // AUTHOR         Rahul Chillar
    // DATE WRITTEN   March 2004
    // MODIFIED       na
    // RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // Calculate the performance of a sensible air-liquid heat exchanger.  Calculated
    // results include outlet air temperature and humidity, outlet water temperature,
    // and heat transfer rate.

    // METHODOLOGY EMPLOYED:
    // Models coil using effectiveness-NTU model.

    // REFERENCES:
    // Kays, W.M. and A.L. London.  1964,Compact Heat Exchangers, 2nd Edition,
    // New York: McGraw-Hill.

    // USE STATEMENTS:
    // na

    // Enforce explicit typing of all variables in this routine

    // Locals
    // FUNCTION ARGUMENT DEFINITIONS:

    // FUNCTION PARAMETER DEFINITIONS:
    static constexpr std::string_view RoutineName("CoilCompletelyDry");

    // INTERFACE BLOCK SPECIFICATIONS
    // na

    // DERIVED TYPE DEFINITIONS
    // na

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    Real64 CapacitanceAir;   // Air-side capacity rate(W/C)
    Real64 CapacitanceWater; // Water-side capacity rate(W/C)
    Real64 AirMassFlow;
    Real64 WaterMassFlowRate;
    Real64 Cp;

    //  adjust mass flow rates for cycling fan cycling coil operation
    if (FanOpMode == CycFanCycCoil) {
        if (PartLoadRatio > 0.0) {
            AirMassFlow = state.dataWaterCoils->WaterCoil(CoilNum).InletAirMassFlowRate / PartLoadRatio;
            WaterMassFlowRate = min(state.dataWaterCoils->WaterCoil(CoilNum).InletWaterMassFlowRate / PartLoadRatio,
                                    state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterMassFlowRate);
        } else {
            AirMassFlow = 0.0;
            WaterMassFlowRate = 0.0;
        }
    } else {
        AirMassFlow = state.dataWaterCoils->WaterCoil(CoilNum).InletAirMassFlowRate;
        WaterMassFlowRate = state.dataWaterCoils->WaterCoil(CoilNum).InletWaterMassFlowRate;
    }

    // Calculate air and water capacity rates
    CapacitanceAir = AirMassFlow * PsyCpAirFnW(state.dataWaterCoils->WaterCoil(CoilNum).InletAirHumRat);
    // Water Capacity Rate
    Cp = GetSpecificHeatGlycol(state,
                               state.dataPlnt->PlantLoop(state.dataWaterCoils->WaterCoil(CoilNum).WaterLoopNum).FluidName,
                               WaterTempIn,
                               state.dataPlnt->PlantLoop(state.dataWaterCoils->WaterCoil(CoilNum).WaterLoopNum).FluidIndex,
                               RoutineName);

    CapacitanceWater = WaterMassFlowRate * Cp;

    // Determine the air and water outlet conditions
    CoilOutletStreamCondition(state, CoilNum, CapacitanceWater, WaterTempIn, CapacitanceAir, AirTempIn, CoilUA, OutletWaterTemp, OutletAirTemp);

    // Calculate the total and sensible heat transfer rate both are equal in case of Dry Coil
    Q = CapacitanceAir * (AirTempIn - OutletAirTemp);

    // Outlet humidity is equal to Inlet Humidity because its a dry coil
    OutletAirHumRat = state.dataWaterCoils->WaterCoil(CoilNum).InletAirHumRat;
}

// Coil Completely Wet Subroutine for Cooling Coil

void CoilCompletelyWet(EnergyPlusData &state,
                       int const CoilNum,            // Number of Coil
                       Real64 const WaterTempIn,     // Water temperature IN to this function (C)
                       Real64 const AirTempIn,       // Air dry bulb temperature IN to this function(C)
                       Real64 const AirHumRat,       // Air Humidity Ratio IN to this funcation (C)
                       Real64 const UAInternalTotal, // Internal overall heat transfer coefficient(W/m2 C)
                       Real64 const UAExternalTotal, // External overall heat transfer coefficient(W/m2 C)
                       Real64 &OutletWaterTemp,      // Leaving water temperature (C)
                       Real64 &OutletAirTemp,        // Leaving air dry bulb temperature(C)
                       Real64 &OutletAirHumRat,      // Leaving air humidity ratio
                       Real64 &TotWaterCoilLoad,     // Total heat transfer rate(W)
                       Real64 &SenWaterCoilLoad,     // Sensible heat transfer rate(W)
                       Real64 &SurfAreaWetFraction,  // Fraction of surface area wet
                       Real64 &AirInletCoilSurfTemp, // Surface temperature at air entrance(C)
                       int const FanOpMode,          // fan operating mode
                       Real64 const PartLoadRatio    // part-load ratio of heating coil
)
{

    // FUNCTION INFORMATION:
    // AUTHOR         Rahul Chillar
    // DATE WRITTEN   Mar 2004
    // MODIFIED       na
    // RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // Calculate the performance of a cooling coil when the external fin surface is
    // complete wet.  Results include outlet air temperature and humidity,
    // outlet water temperature, sensible and total cooling capacities, and the wet
    // fraction of the air-side surface area.

    // METHODOLOGY EMPLOYED:
    // Models coil as counterflow heat exchanger. Approximates saturated air enthalpy as
    // a linear function of temperature
    // TRNSYS.  1990.  A Transient System Simulation Program: Reference Manual.
    // Solar Energy Laboratory, Univ. Wisconsin Madison, pp. 4.6.8-1 - 4.6.8-12.
    // Threlkeld, J.L.  1970.  Thermal Environmental Engineering, 2nd Edition,
    // Englewood Cliffs: Prentice-Hall,Inc. pp. 254-270.
    // Coil Uses Enthalpy Based Heat Transfer Coefficents and converts them to
    // convential UA values. Intermediate value of fictitious Cp is defined. This follow
    // the same procedure followed in the Design Calculation of the Coil. See the node in
    // the one time calculation for design condition.

    // REFERENCES:
    // Elmahdy, A.H. and Mitalas, G.P.  1977."A Simple Model for Cooling and
    // Dehumidifying Coils for Use In Calculating Energy Requirements for Buildings,"
    // ASHRAE Transactions,Vol.83 Part 2, pp. 103-117.

    // USE STATEMENTS:

    // Enforce explicit typing of all variables in this routine

    // Locals
    // FUNCTION ARGUMENT DEFINITIONS:

    // FUNCTION PARAMETER DEFINITIONS:
    static constexpr std::string_view RoutineName("CoilCompletelyWet");

    // INTERFACE BLOCK SPECIFICATIONS
    // na

    // DERIVED TYPE DEFINITIONS
    // na

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    Real64 AirSideResist;                  // Air-side resistance to heat transfer(m2 C/W)
    Real64 WaterSideResist;                // Liquid-side resistance to heat transfer(m2 C/W)
    Real64 EnteringAirDewPt;               // Entering air dew point(C)
    Real64 UACoilTotalEnth;                // Overall enthalpy heat transfer coefficient(kg/s)
    Real64 CapacityRateAirWet;             // Air-side capacity rate(kg/s)
    Real64 CapacityRateWaterWet;           // Liquid-side capacity rate(kg/s)
    Real64 ResistRatio;                    // Ratio of resistances
    Real64 EnthAirOutlet;                  // Outlet air enthalpy
    Real64 EnthSatAirInletWaterTemp;       // Saturated enthalpy of air at entering water temperature(J/kg)
    Real64 EnthSatAirOutletWaterTemp;      // Saturated enthalpy of air at exit water temperature(J/kg)
    Real64 EnthSatAirCoilSurfaceEntryTemp; // Saturated enthalpy of air at entering surface temperature(J/kg)
    Real64 EnthSatAirCoilSurfaceExitTemp;  // Saturated enthalpy of air at exit surface temperature(J/kg)
    Real64 EnthAirInlet;                   // Enthalpy of air at inlet
    Real64 IntermediateCpSat;              // Coefficient for equation below(J/kg C)
    // EnthSat1-EnthSat2 = IntermediateCpSat*(TSat1-TSat2)
    // (all water and surface temperatures are
    // related to saturated air enthalpies for
    // wet surface heat transfer calculations)
    Real64 const SmallNo(1.e-9); // smallNo used in place of 0
    Real64 AirMassFlow;
    Real64 WaterMassFlowRate;
    Real64 Cp;

    SurfAreaWetFraction = 1.0;
    AirSideResist = 1.0 / max(UAExternalTotal, SmallNo);
    WaterSideResist = 1.0 / max(UAInternalTotal, SmallNo);

    //  adjust mass flow rates for cycling fan cycling coil operation
    if (FanOpMode == CycFanCycCoil) {
        if (PartLoadRatio > 0.0) {
            AirMassFlow = state.dataWaterCoils->WaterCoil(CoilNum).InletAirMassFlowRate / PartLoadRatio;
            WaterMassFlowRate = min(state.dataWaterCoils->WaterCoil(CoilNum).InletWaterMassFlowRate / PartLoadRatio,
                                    state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterMassFlowRate);
        } else {
            AirMassFlow = 0.0;
            WaterMassFlowRate = 0.0;
        }
    } else {
        AirMassFlow = state.dataWaterCoils->WaterCoil(CoilNum).InletAirMassFlowRate;
        WaterMassFlowRate = state.dataWaterCoils->WaterCoil(CoilNum).InletWaterMassFlowRate;
    }

    // Calculate enthalpies of entering air and water

    // Enthalpy of air at inlet to the coil
    EnthAirInlet = PsyHFnTdbW(AirTempIn, AirHumRat);

    // Saturation Enthalpy of Air at inlet water temperature
    EnthSatAirInletWaterTemp = PsyHFnTdbW(WaterTempIn, PsyWFnTdpPb(state, WaterTempIn, state.dataEnvrn->OutBaroPress));

    // Estimate IntermediateCpSat using entering air dewpoint and water temperature
    EnteringAirDewPt = PsyTdpFnWPb(state, AirHumRat, state.dataEnvrn->OutBaroPress);

    // An intermediate value of Specific heat . EnthSat1-EnthSat2 = IntermediateCpSat*(TSat1-TSat2)
    IntermediateCpSat =
        (PsyHFnTdbW(EnteringAirDewPt, PsyWFnTdpPb(state, EnteringAirDewPt, state.dataEnvrn->OutBaroPress)) - EnthSatAirInletWaterTemp) /
        (EnteringAirDewPt - WaterTempIn);

    // Determine air and water enthalpy outlet conditions by modeling
    // coil as counterflow enthalpy heat exchanger
    UACoilTotalEnth = 1.0 / (IntermediateCpSat * WaterSideResist + AirSideResist * PsyCpAirFnW(0.0));
    CapacityRateAirWet = AirMassFlow;
    Cp = GetSpecificHeatGlycol(state,
                               state.dataPlnt->PlantLoop(state.dataWaterCoils->WaterCoil(CoilNum).WaterLoopNum).FluidName,
                               WaterTempIn,
                               state.dataPlnt->PlantLoop(state.dataWaterCoils->WaterCoil(CoilNum).WaterLoopNum).FluidIndex,
                               RoutineName);
    CapacityRateWaterWet = WaterMassFlowRate * (Cp / IntermediateCpSat);
    CoilOutletStreamCondition(state,
                              CoilNum,
                              CapacityRateAirWet,
                              EnthAirInlet,
                              CapacityRateWaterWet,
                              EnthSatAirInletWaterTemp,
                              UACoilTotalEnth,
                              EnthAirOutlet,
                              EnthSatAirOutletWaterTemp);

    // Calculate entering and leaving external surface conditions from
    // air and water conditions and the ratio of resistances
    ResistRatio = (WaterSideResist) / (WaterSideResist + PsyCpAirFnW(0.0) / IntermediateCpSat * AirSideResist);
    EnthSatAirCoilSurfaceEntryTemp = EnthSatAirOutletWaterTemp + ResistRatio * (EnthAirInlet - EnthSatAirOutletWaterTemp);
    EnthSatAirCoilSurfaceExitTemp = EnthSatAirInletWaterTemp + ResistRatio * (EnthAirOutlet - EnthSatAirInletWaterTemp);

    // Calculate Coil Surface Temperature at air entry to the coil
    AirInletCoilSurfTemp = PsyTsatFnHPb(state, EnthSatAirCoilSurfaceEntryTemp, state.dataEnvrn->OutBaroPress);

    // Calculate outlet air temperature and humidity from enthalpies and surface conditions.
    TotWaterCoilLoad = AirMassFlow * (EnthAirInlet - EnthAirOutlet);
    OutletWaterTemp = WaterTempIn + TotWaterCoilLoad / max(WaterMassFlowRate, SmallNo) / Cp;

    // Calculates out put variable for  the completely wet coil
    WetCoilOutletCondition(state, CoilNum, AirTempIn, EnthAirInlet, EnthAirOutlet, UAExternalTotal, OutletAirTemp, OutletAirHumRat, SenWaterCoilLoad);
}

// Coil Part Wet Part Dry Subroutine for Cooling Coil

void CoilPartWetPartDry(EnergyPlusData &state,
                        int const CoilNum,             // Number of Coil
                        bool const FirstHVACIteration, // Saving Old values
                        Real64 const InletWaterTemp,   // Entering liquid temperature(C)
                        Real64 const InletAirTemp,     // Entering air dry bulb temperature(C)
                        Real64 const AirDewPointTemp,  // Entering air dew point(C)
                        Real64 &OutletWaterTemp,       // Leaving liquid temperature(C)
                        Real64 &OutletAirTemp,         // Leaving air dry bulb temperature(C)
                        Real64 &OutletAirHumRat,       // Leaving air humidity ratio
                        Real64 &TotWaterCoilLoad,      // Total heat transfer rate (W)
                        Real64 &SenWaterCoilLoad,      // Sensible heat transfer rate (W)
                        Real64 &SurfAreaWetFraction,   // Fraction of surface area wet
                        int const FanOpMode,           // fan operating mode
                        Real64 const PartLoadRatio     // part-load ratio of heating coil
)
{

    // FUNCTION INFORMATION:
    // AUTHOR         Rahul Chillar
    // DATE WRITTEN   March 2004
    // MODIFIED       na
    // RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // Calculate the performance of a cooling  coil when the external fin surface is
    // part wet and part dry.  Results include outlet air temperature and humidity,
    // outlet liquid temperature, sensible and total cooling capacities, and the wet
    // fraction of the air-side surface area.

    // METHODOLOGY EMPLOYED:
    // Models coil using effectiveness NTU model

    // REFERENCES:
    // Elmahdy, A.H. and Mitalas, G.P.  1977. "A Simple Model for Cooling and
    // Dehumidifying Coils for Use In Calculating Energy Requirements for Buildings,"
    // ASHRAE Transactions,Vol.83 Part 2, pp. 103-117.
    // TRNSYS.  1990.  A Transient System Simulation Program: Reference Manual.
    // Solar Energy Laboratory, Univ. Wisconsin- Madison, pp. 4.6.8-1 - 4.6.8-12.
    // Threlkeld, J.L.  1970.  Thermal Environmental Engineering, 2nd Edition,
    // Englewood Cliffs: Prentice-Hall,Inc. pp. 254-270.

    // Using/Aliasing
    using General::Iterate;

    // Enforce explicit typing of all variables in this routine

    // Locals
    // FUNCTION ARGUMENT DEFINITIONS:

    // FUNCTION PARAMETER DEFINITIONS:
    int const itmax(60);
    Real64 const smalltempdiff(1.0e-9);

    // INTERFACE BLOCK SPECIFICATIONS
    // na

    // DERIVED TYPE DEFINITIONS
    // na

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    Real64 DryCoilHeatTranfer;             // Heat transfer rate for dry coil(W)
    Real64 WetCoilTotalHeatTransfer;       // Total heat transfer rate for wet coil(W)
    Real64 WetCoilSensibleHeatTransfer;    // Sensible heat transfer rate for wet coil(W)
    Real64 SurfAreaWet;                    // Air-side area of wet coil(m2)
    Real64 SurfAreaDry;                    // Air-side area of dry coil(m2)
    Real64 DryCoilUA;                      // Overall heat transfer coefficient for dry coil(W/C)
    Real64 WetDryInterfcWaterTemp;         // Liquid temperature at wet/dry boundary(C)
    Real64 WetDryInterfcAirTemp;           // Air temperature at wet/dry boundary(C)
    Real64 WetDryInterfcSurfTemp;          // Surface temperature at wet/dry boundary(C)
    Real64 EstimateWetDryInterfcWaterTemp; // Estimated liquid temperature at wet/dry boundary(C)
    Real64 EstimateSurfAreaWetFraction;    // Initial Estimate for Fraction of Surface Wet with condensation
    Real64 WetPartUAInternal;              // UA of Wet Coil Internal
    Real64 WetPartUAExternal;              // UA of Dry Coil External
    Real64 WetDryInterfcHumRat;            // Humidity Ratio at interface of the wet dry transition
    Real64 X1T;                            // Variables used in the two iteration in this subroutine.
    Real64 NewSurfAreaWetFrac;             // Variables used in the two iteration in this subroutine.
    Real64 ResultXT;                       // Variables used in the two iteration in this subroutine.
    Real64 Y1T;                            // Variables used in the two iterations in this subroutine.
    Real64 errorT;                         // Error in interation for First If loop
    Real64 error;                          // Deviation of dependent variable in iteration
    Real64 SurfAreaFracPrevious;
    Real64 ErrorPrevious;
    Real64 SurfAreaFracLast;
    Real64 ErrorLast;
    int iter;  // Iteration counter
    int icvg;  // Iteration convergence flag
    int icvgT; // Iteration Convergence Flag for First If loop
    int itT;   // Iteration Counter for First If Loop

    // Iterates on SurfAreaWetFraction to converge on surface temperature equal to
    // entering air dewpoint at wet/dry boundary.

    // Preliminary estimates of coil performance to begin iteration
    OutletWaterTemp = InletAirTemp;
    DryCoilHeatTranfer = 0.0;
    WetCoilTotalHeatTransfer = 0.0;
    WetCoilSensibleHeatTransfer = 0.0;

    if (FirstHVACIteration) {
        // Estimate liquid temperature at boundary as entering air dew point
        WetDryInterfcWaterTemp = AirDewPointTemp;

        // Estimate fraction wet surface area based on liquid temperatures
        if (std::abs(OutletWaterTemp - InletWaterTemp) > smalltempdiff) {
            SurfAreaWetFraction = (WetDryInterfcWaterTemp - InletWaterTemp) / (OutletWaterTemp - InletWaterTemp);
        } else {
            SurfAreaWetFraction = 0.0;
        }

    } else {
        SurfAreaWetFraction = state.dataWaterCoils->WaterCoil(CoilNum).SurfAreaWetFractionSaved;
    }
    // BEGIN LOOP to converge on SurfAreaWetFraction
    // The method employed in this loop is as follows: The coil is partially wet and partially dry,
    // we calculate the temperature of the coil at the interface, (the point at which the moisture begins
    // to condense) temperature of the  water  at interface and air temp is dew point at that location.
    // This is done by Iterating between the Completely Dry and Completely Wet Coil until the outlet
    // water temperature of one coil equals the inlet water temperature of another.
    // Using this value of interface temperature we now iterate to calculate Surface Fraction Wet, Iterate
    // function perturbs the value of Surface Fraction Wet and based on this new value the entire loop is
    // repeated to get a new interface water temperature and then surface fraction wet is again calculated.
    // This process continues till the error between the Wet Dry Interface Temp and Air Dew Point becomes
    // very negligible and in 95% of the cases its is a complete convergence to give the exact surface Wet
    // fraction.
    NewSurfAreaWetFrac = SurfAreaWetFraction;
    error = 0.0;
    SurfAreaFracPrevious = SurfAreaWetFraction;
    ErrorPrevious = 0.0;
    SurfAreaFracLast = SurfAreaWetFraction;
    ErrorLast = 0.0;

    for (iter = 1; iter <= itmax; ++iter) {

        // Calculating Surface Area Wet and Surface Area Dry
        SurfAreaWet = SurfAreaWetFraction * state.dataWaterCoils->WaterCoil(CoilNum).TotCoilOutsideSurfArea;
        SurfAreaDry = state.dataWaterCoils->WaterCoil(CoilNum).TotCoilOutsideSurfArea - SurfAreaWet;

        // Calculating UA values for the Dry Part of the Coil
        DryCoilUA = SurfAreaDry / (1.0 / state.dataWaterCoils->WaterCoil(CoilNum).UACoilInternalPerUnitArea +
                                   1.0 / state.dataWaterCoils->WaterCoil(CoilNum).UADryExtPerUnitArea);

        // Calculating UA Value for the Wet part of the Coil
        WetPartUAExternal = state.dataWaterCoils->WaterCoil(CoilNum).UAWetExtPerUnitArea * SurfAreaWet;
        WetPartUAInternal = state.dataWaterCoils->WaterCoil(CoilNum).UACoilInternalPerUnitArea * SurfAreaWet;

        // Calculating Water Temperature at Wet Dry Interface of the coil
        WetDryInterfcWaterTemp = InletWaterTemp + SurfAreaWetFraction * (OutletWaterTemp - InletWaterTemp);

        // BEGIN LOOP to converge on liquid temperature at wet/dry boundary
        for (itT = 1; itT <= itmax; ++itT) {

            // Calculate dry coil performance with estimated liquid temperature at the boundary.
            CoilCompletelyDry(state,
                              CoilNum,
                              WetDryInterfcWaterTemp,
                              InletAirTemp,
                              DryCoilUA,
                              OutletWaterTemp,
                              WetDryInterfcAirTemp,
                              WetDryInterfcHumRat,
                              DryCoilHeatTranfer,
                              FanOpMode,
                              PartLoadRatio);

            // Calculate wet coil performance with calculated air temperature at the boundary.
            CoilCompletelyWet(state,
                              CoilNum,
                              InletWaterTemp,
                              WetDryInterfcAirTemp,
                              WetDryInterfcHumRat,
                              WetPartUAInternal,
                              WetPartUAExternal,
                              EstimateWetDryInterfcWaterTemp,
                              OutletAirTemp,
                              OutletAirHumRat,
                              WetCoilTotalHeatTransfer,
                              WetCoilSensibleHeatTransfer,
                              EstimateSurfAreaWetFraction,
                              WetDryInterfcSurfTemp,
                              FanOpMode,
                              PartLoadRatio);

            // Iterating to calculate the actual wet dry interface water temperature.
            errorT = EstimateWetDryInterfcWaterTemp - WetDryInterfcWaterTemp;
            Iterate(ResultXT, 0.001, WetDryInterfcWaterTemp, errorT, X1T, Y1T, itT, icvgT);
            WetDryInterfcWaterTemp = ResultXT;

            // IF convergence is achieved then exit the itT to itmax Do loop.
            if (icvgT == 1) break;

        } // End Do for Liq Boundary temp Convergence

        // Wet Dry Interface temperature not converged after maximum specified iterations.
        // Print error message, set return error flag
        if ((itT > itmax) && (!state.dataGlobal->WarmupFlag)) {
            ShowWarningError(state, "For Coil:Cooling:Water " + state.dataWaterCoils->WaterCoil(CoilNum).Name);
            ShowContinueError(state, "CoilPartWetPartDry: Maximum iterations exceeded for Liq Temp, at Interface");
        }

        // If Following condition prevails then surface is dry, calculate dry coil performance and return
        if (SurfAreaWetFraction <= 0.0 && WetDryInterfcSurfTemp >= AirDewPointTemp) {

            // Calculating Value of Dry UA for the coil
            DryCoilUA = state.dataWaterCoils->WaterCoil(CoilNum).TotCoilOutsideSurfArea /
                        (1.0 / state.dataWaterCoils->WaterCoil(CoilNum).UACoilInternalPerUnitArea +
                         1.0 / state.dataWaterCoils->WaterCoil(CoilNum).UADryExtPerUnitArea);

            // Calling the Completely Dry Coil for outputs
            CoilCompletelyDry(state,
                              CoilNum,
                              InletWaterTemp,
                              InletAirTemp,
                              DryCoilUA,
                              OutletWaterTemp,
                              OutletAirTemp,
                              OutletAirHumRat,
                              TotWaterCoilLoad,
                              FanOpMode,
                              PartLoadRatio);

            // Sensible load = Total load in a Completely Dry Coil
            SenWaterCoilLoad = TotWaterCoilLoad;

            // All coil is Dry so fraction wet is ofcourse =0
            SurfAreaWetFraction = 0.0;
            return;
        }

        // IF the coil is not Dry then iterate to calculate Fraction of surface area that is wet.
        error = WetDryInterfcSurfTemp - AirDewPointTemp;
        CoilAreaFracIter(
            NewSurfAreaWetFrac, SurfAreaWetFraction, error, SurfAreaFracPrevious, ErrorPrevious, SurfAreaFracLast, ErrorLast, iter, icvg);
        SurfAreaWetFraction = NewSurfAreaWetFrac;

        // If converged, leave iteration loop
        if (icvg == 1) break;

        // Surface temperature not converged.  Repeat calculations with new
        // estimate of fraction wet surface area.
        if (SurfAreaWetFraction > 1.0) SurfAreaWetFraction = 1.0;
        if (SurfAreaWetFraction <= 0.0) SurfAreaWetFraction = 0.0098;

    } // End do for the overall iteration

    // Calculate sum of total and sensible heat transfer from dry and wet parts.
    TotWaterCoilLoad = DryCoilHeatTranfer + WetCoilTotalHeatTransfer;
    SenWaterCoilLoad = DryCoilHeatTranfer + WetCoilSensibleHeatTransfer;

    // Save last iterations values for this current time step
    state.dataWaterCoils->WaterCoil(CoilNum).SurfAreaWetFractionSaved = SurfAreaWetFraction;
}

// Calculating coil UA for Cooling Coil

Real64 CalcCoilUAbyEffectNTU(EnergyPlusData &state,
                             int const CoilNum,
                             Real64 const CapacityStream1,     // Capacity rate of stream1.(W/C)
                             Real64 const EnergyInStreamOne,   // Inlet state of stream1.(C)
                             Real64 const CapacityStream2,     // Capacity rate of stream2.(W/C)
                             Real64 const EnergyInStreamTwo,   // Inlet state of stream2.(C)
                             Real64 const DesTotalHeatTransfer // Heat transfer rate(W)
)
{

    // FUNCTION INFORMATION:
    // AUTHOR         Rahul Chillar
    // DATE WRITTEN   March 2004
    // MODIFIED       na
    // RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // Calculate the UA of a heat exchanger using the effectiveness-NTU relationships
    // given the entering capacity rate and temperature of each flow stream, the
    // heat transfer rate under these conditions and the heat exchanger configuration.

    // METHODOLOGY EMPLOYED:
    // Models coil using effectiveness NTU model

    // REFERENCES:
    // na

    // Using/Aliasing
    using General::Iterate;

    // Enforce explicit typing of all variables in this routine

    // Return value
    Real64 CalcCoilUAbyEffectNTU; // Overall heat transfer coefficient(W/C)

    // Locals
    // FUNCTION ARGUMENT DEFINITIONS:

    // FUNCTION PARAMETER DEFINITIONS:
    Real64 const SmallNo(1.e-9);
    int const itmax(12);

    // INTERFACE BLOCK SPECIFICATIONS
    // na

    // DERIVED TYPE DEFINITIONS
    // na

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    Real64 MaxHeatTransfer;       // Maximum heat transfer from inlet conditions (W)
    Real64 EstimatedHeatTransfer; // Estimated heat transfer in iteration(W)
    Real64 CoilUA;                // Estimated heat transfer coefficient(W/C)
    Real64 error;                 // Deviation of dependent variable in iteration
    Real64 X1;                    // Previous values of independent variable in iteration
    Real64 Y1;
    Real64 ResultX;
    Real64 EnergyOutStreamOne;        // Intermediate Variable used
    Real64 EnergyOutStreamTwo;        // Intermediate variable used
    Real64 DesTotalHeatTransferCheck; // Check value to keep design total heat transfer in range
    int iter;                         // Iteration index
    int icvg;                         // Iteration convergence flag

    // Check for Q out of range (effectiveness > 1)
    MaxHeatTransfer = std::abs(min(CapacityStream1, CapacityStream2) * (EnergyInStreamOne - EnergyInStreamTwo));

    // Error Message
    if ((std::abs(DesTotalHeatTransfer) - MaxHeatTransfer) / max(MaxHeatTransfer, SmallNo) > SmallNo) {
        ShowWarningError(state, "For Coil:Cooling:Water " + state.dataWaterCoils->WaterCoil(CoilNum).Name);
        ShowContinueError(state, "CalcCoilUAbyEffectNTU:Given Q impossible for given inlet states, proceeding with MaxHeat Transfer");
        ShowContinueError(state, "Check the Sizing:System and Sizing:Zone cooling design supply air temperature and ");
        ShowContinueError(state,
                          "the Sizing:Plant design Loop exit temperature.  There must be sufficient difference between these two temperatures.");
    }

    // Design Heat Transfer cannot exceed Max heat Transfer , setting it value such that effectiveness<1.0
    if ((DesTotalHeatTransfer) > (MaxHeatTransfer)) {
        // Pegging value so that effectiveness is less than 1.
        DesTotalHeatTransferCheck = 0.9 * MaxHeatTransfer;

        // Estimate CalcCoilUAbyEffectNTU
        CoilUA = std::abs(DesTotalHeatTransferCheck / (EnergyInStreamOne - EnergyInStreamTwo));

    } else {

        // Estimate CalcCoilUAbyEffectNTU
        CoilUA = std::abs(DesTotalHeatTransfer / (EnergyInStreamOne - EnergyInStreamTwo));
    }

    // BEGIN LOOP to iteratively calculate CalcCoilUAbyEffectNTU
    for (iter = 1; iter <= itmax; ++iter) {

        // Calculate heat transfer rate for estimated CalcCoilUAbyEffectNTU
        CoilOutletStreamCondition(
            state, CoilNum, CapacityStream1, EnergyInStreamOne, CapacityStream2, EnergyInStreamTwo, CoilUA, EnergyOutStreamOne, EnergyOutStreamTwo);

        // Initial Guess for a value of heat transfer
        EstimatedHeatTransfer = CapacityStream1 * (EnergyInStreamOne - EnergyOutStreamOne);

        // Calculate new estimate for CalcCoilUAbyEffectNTU by iteration
        if (DesTotalHeatTransfer > MaxHeatTransfer) {
            error = std::abs(EstimatedHeatTransfer) - std::abs(DesTotalHeatTransferCheck);
        } else {
            error = std::abs(EstimatedHeatTransfer) - std::abs(DesTotalHeatTransfer);
        }
        Iterate(ResultX, 0.01, CoilUA, error, X1, Y1, iter, icvg);
        CoilUA = ResultX;
        // If converged, leave loop
        if (icvg == 1) break;
    }

    // If not converged after itmax iterations, return error code
    if ((iter > itmax) && (!state.dataGlobal->WarmupFlag)) {
        ShowWarningError(state, "For Coil:Cooling:Water " + state.dataWaterCoils->WaterCoil(CoilNum).Name);
        ShowContinueError(state, "CalcCoilUAbyEffectNTU: Maximum iterations exceeded:Coil UA calculation");
        CalcCoilUAbyEffectNTU = 0.0; // Autodesk:Return Line added to set return value: Using non-converged CoilUA value may be preferred but
                                     // that was not happening
    } else {

        // Assign value to CalcCoilUAbyEffectNTU
        CalcCoilUAbyEffectNTU = CoilUA;
    }

    return CalcCoilUAbyEffectNTU;
}

// Calculating coil outlet stream conditions and coil UA for Cooling Coil

void CoilOutletStreamCondition(EnergyPlusData &state,
                               int const CoilNum,
                               Real64 const CapacityStream1,   // Capacity rate of stream1(W/C)
                               Real64 const EnergyInStreamOne, // Inlet state of stream1 (C)
                               Real64 const CapacityStream2,   // Capacity rate of stream2 (W/C)
                               Real64 const EnergyInStreamTwo, // Inlet state of stream2 (C)
                               Real64 const CoilUA,            // Heat transfer rateW)
                               Real64 &EnergyOutStreamOne,     // Outlet state of stream1 (C)
                               Real64 &EnergyOutStreamTwo      // Outlet state of stream2 (C)
)
{

    // FUNCTION INFORMATION:
    // AUTHOR         Rahul Chillar
    // DATE WRITTEN   March 2004
    // MODIFIED       na
    // RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // Calculate the outlet states of a simple heat exchanger using the effectiveness-Ntu
    // method of analysis.

    // METHODOLOGY EMPLOYED:
    // na

    // REFERENCES:
    // Kays, W.M. and A.L. London.  1964.Compact Heat Exchangers, 2nd Ed.McGraw-Hill:New York.

    // USE STATEMENTS:
    // na

    // Enforce explicit typing of all variables in this routine

    // Locals
    // FUNCTION ARGUMENT DEFINITIONS:

    // FUNCTION PARAMETER DEFINITIONS:
    Real64 const LargeNo(1.e10);  // value used in place of infinity
    Real64 const SmallNo(1.e-15); // value used in place of zero

    // INTERFACE BLOCK SPECIFICATIONS
    // na

    // DERIVED TYPE DEFINITIONS
    // na

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    Real64 MinimumCapacityStream; // Minimum capacity rate of the streams(W/C)
    Real64 MaximumCapacityStream; // Maximum capacity rate of the streams(W/C)
    Real64 RatioStreamCapacity;   // Ratio of minimum to maximum capacity rate
    Real64 NTU;                   // Number of transfer units
    Real64 effectiveness(0.0);    // Heat exchanger effectiveness
    Real64 MaxHeatTransfer;       // Maximum heat transfer possible(W)
    Real64 e;                     // Intermediate variables in effectiveness equation
    Real64 eta;
    Real64 b;
    Real64 d;

    // NTU and MinimumCapacityStream/MaximumCapacityStream (RatioStreamCapacity) calculations
    MinimumCapacityStream = min(CapacityStream1, CapacityStream2);
    MaximumCapacityStream = max(CapacityStream1, CapacityStream2);

    if (std::abs(MaximumCapacityStream) <= 1.e-6) { // .EQ. 0.0d0) THEN
        RatioStreamCapacity = 1.0;
    } else {
        RatioStreamCapacity = MinimumCapacityStream / MaximumCapacityStream;
    }

    if (std::abs(MinimumCapacityStream) <= 1.e-6) { // .EQ. 0.0d0) THEN
        NTU = LargeNo;
    } else {
        NTU = CoilUA / MinimumCapacityStream;
    }

    // Calculate effectiveness for special limiting cases
    if (NTU <= 0.0) {
        effectiveness = 0.0;

    } else if (RatioStreamCapacity < SmallNo) {
        // MinimumCapacityStream/MaximumCapacityStream = 0 and effectiveness is independent of configuration
        // 20 is the Limit Chosen for Exponential Function, beyond which there is float point error.
        if (NTU > 20.0) {
            effectiveness = 1.0;
        } else {
            effectiveness = 1.0 - std::exp(-NTU);
        }
        // Calculate effectiveness depending on heat exchanger configuration
    } else if (state.dataWaterCoils->WaterCoil(CoilNum).HeatExchType == state.dataWaterCoils->CounterFlow) {

        // Counterflow Heat Exchanger Configuration
        if (std::abs(RatioStreamCapacity - 1.0) < SmallNo) {
            effectiveness = NTU / (NTU + 1.0);
        } else {
            if (NTU * (1.0 - RatioStreamCapacity) > 20.0) {
                e = 0.0;
            } else {
                e = std::exp(-NTU * (1.0 - RatioStreamCapacity));
            }
            effectiveness = (1.0 - e) / (1.0 - RatioStreamCapacity * e);
        }

    } else if (state.dataWaterCoils->WaterCoil(CoilNum).HeatExchType == state.dataWaterCoils->CrossFlow) {
        // Cross flow, both streams unmixed
        eta = std::pow(NTU, -0.22);
        if ((NTU * RatioStreamCapacity * eta) > 20.0) {
            b = 1.0 / (RatioStreamCapacity * eta);
            if (b > 20.0) {
                effectiveness = 1.0;
            } else {
                effectiveness = 1.0 - std::exp(-b);
                if (effectiveness < 0.0) effectiveness = 0.0;
            }
        } else {
            d = ((std::exp(-NTU * RatioStreamCapacity * eta) - 1.0) / (RatioStreamCapacity * eta));
            if (d < -20.0 || d > 0.0) {
                effectiveness = 1.0;
            } else {
                effectiveness = 1.0 - std::exp((std::exp(-NTU * RatioStreamCapacity * eta) - 1.0) / (RatioStreamCapacity * eta));
                if (effectiveness < 0.0) effectiveness = 0.0;
            }
        }
    }

    // Determine leaving conditions for the two streams
    MaxHeatTransfer = max(MinimumCapacityStream, SmallNo) * (EnergyInStreamOne - EnergyInStreamTwo);
    EnergyOutStreamOne = EnergyInStreamOne - effectiveness * MaxHeatTransfer / max(CapacityStream1, SmallNo);
    EnergyOutStreamTwo = EnergyInStreamTwo + effectiveness * MaxHeatTransfer / max(CapacityStream2, SmallNo);
}

// Subroutine for caculating outlet condition if coil is wet , for Cooling Coil

void WetCoilOutletCondition(EnergyPlusData &state,
                            int const CoilNum,
                            Real64 const AirTempIn,      // Entering air dry bulb temperature(C)
                            Real64 const EnthAirInlet,   // Entering air enthalpy(J/kg)
                            Real64 const EnthAirOutlet,  // Leaving air enthalpy(J/kg)
                            Real64 const UACoilExternal, // Heat transfer coefficient for external surface (W/C)
                            Real64 &OutletAirTemp,       // Leaving air dry bulb temperature(C)
                            Real64 &OutletAirHumRat,     // Leaving air humidity ratio
                            Real64 &SenWaterCoilLoad     // Sensible heat transfer rate(W)
)
{

    // FUNCTION INFORMATION:
    // AUTHOR         Rahul Chillar
    // DATE WRITTEN   Mar 2004
    // MODIFIED       na
    // RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // Calculate the leaving air temperature,the leaving air humidity ratio and the
    // sensible cooling capacity of wet cooling coil.

    // METHODOLOGY EMPLOYED:
    // Assumes condensate at uniform temperature.

    // REFERENCES:
    // Elmahdy, A.H. and Mitalas, G.P.  1977."A Simple Model for Cooling and
    // Dehumidifying Coils for Use In Calculating Energy Requirements for Buildings,"
    // ASHRAE Transactions,Vol.83 Part 2, pp. 103-117.

    // USE STATEMENTS:

    // Enforce explicit typing of all variables in this routine

    // Locals
    // FUNCTION ARGUMENT DEFINITIONS:

    // FUNCTION PARAMETER DEFINITIONS:
    Real64 const SmallNo(1.e-9); // SmallNo value used in place of zero

    // INTERFACE BLOCK SPECIFICATIONS
    // na

    // DERIVED TYPE DEFINITIONS
    // na

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    Real64 CapacitanceAir;        // Air capacity rate(W/C)
    Real64 NTU;                   // Number of heat transfer units
    Real64 effectiveness;         // Heat exchanger effectiveness
    Real64 EnthAirCondensateTemp; // Saturated air enthalpy at temperature of condensate(J/kg)
    Real64 TempCondensation;      // Temperature of condensate(C)
    Real64 TempAirDewPoint;       // Temperature air dew point

    // Determine the temperature effectiveness, assuming the temperature
    // of the condensate is constant (MinimumCapacityStream/MaximumCapacityStream = 0) and the specific heat
    // of moist air is constant
    CapacitanceAir =
        state.dataWaterCoils->WaterCoil(CoilNum).InletAirMassFlowRate * PsyCpAirFnW(state.dataWaterCoils->WaterCoil(CoilNum).InletAirHumRat);

    // Calculating NTU from UA and Capacitance.
    // del      NTU = UACoilExternal/MAX(CapacitanceAir,SmallNo)
    // del      effectiveness = 1 - EXP(-MAX(0.0d0,NTU))
    // Calculating NTU from UA and Capacitance.
    if (UACoilExternal > 0.0) {
        if (CapacitanceAir > 0.0) {
            NTU = UACoilExternal / CapacitanceAir;
        } else {
            NTU = 0.0;
        }
        effectiveness = 1.0 - std::exp(-NTU);
    } else {
        effectiveness = 0.0;
    }

    // Calculate coil surface enthalpy and temperature at the exit
    // of the wet part of the coil using the effectiveness relation
    effectiveness = max(effectiveness, SmallNo);
    EnthAirCondensateTemp = EnthAirInlet - (EnthAirInlet - EnthAirOutlet) / effectiveness;

    // Calculate condensate temperature as the saturation temperature
    // at given saturation enthalpy
    TempCondensation = PsyTsatFnHPb(state, EnthAirCondensateTemp, state.dataEnvrn->OutBaroPress);

    TempAirDewPoint = PsyTdpFnWPb(state, state.dataWaterCoils->WaterCoil(CoilNum).InletAirHumRat, state.dataEnvrn->OutBaroPress);

    if ((TempAirDewPoint - TempCondensation) > 0.1) {

        // Calculate Outlet Air Temperature using effectivness
        OutletAirTemp = AirTempIn - (AirTempIn - TempCondensation) * effectiveness;
        // Calculate Outlet air humidity ratio from PsyWFnTdbH routine
        OutletAirHumRat = PsyWFnTdbH(state, OutletAirTemp, EnthAirOutlet);

    } else {
        OutletAirHumRat = state.dataWaterCoils->WaterCoil(CoilNum).InletAirHumRat;
        OutletAirTemp = PsyTdbFnHW(EnthAirOutlet, OutletAirHumRat);
    }

    // Calculate Sensible Coil Load
    SenWaterCoilLoad = CapacitanceAir * (AirTempIn - OutletAirTemp);
}

// Beginning of Update subroutines for the WaterCoil Module
// *****************************************************************************

void UpdateWaterCoil(EnergyPlusData &state, int const CoilNum)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard Liesen
    //       DATE WRITTEN   1998
    //       MODIFIED       April 2004: Rahul Chillar
    //                      Feb 2010 B. Griffith, plant upgrades
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine updates the coil outlet nodes.

    // METHODOLOGY EMPLOYED:
    // Data is moved from the coil data structure to the coil outlet nodes.

    // REFERENCES:
    // na

    // Using/Aliasing
    using PlantUtilities::SetComponentFlowRate;

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:

    // SUBROUTINE PARAMETER DEFINITIONS:
    // na

    // INTERFACE BLOCK SPECIFICATIONS
    // na

    // DERIVED TYPE DEFINITIONS
    // na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int AirInletNode;
    int WaterInletNode;
    int AirOutletNode;
    int WaterOutletNode;

    AirInletNode = state.dataWaterCoils->WaterCoil(CoilNum).AirInletNodeNum;
    WaterInletNode = state.dataWaterCoils->WaterCoil(CoilNum).WaterInletNodeNum;
    AirOutletNode = state.dataWaterCoils->WaterCoil(CoilNum).AirOutletNodeNum;
    WaterOutletNode = state.dataWaterCoils->WaterCoil(CoilNum).WaterOutletNodeNum;

    // Set the outlet air nodes of the WaterCoil
    state.dataLoopNodes->Node(AirOutletNode).MassFlowRate = state.dataWaterCoils->WaterCoil(CoilNum).OutletAirMassFlowRate;
    state.dataLoopNodes->Node(AirOutletNode).Temp = state.dataWaterCoils->WaterCoil(CoilNum).OutletAirTemp;
    state.dataLoopNodes->Node(AirOutletNode).HumRat = state.dataWaterCoils->WaterCoil(CoilNum).OutletAirHumRat;
    state.dataLoopNodes->Node(AirOutletNode).Enthalpy = state.dataWaterCoils->WaterCoil(CoilNum).OutletAirEnthalpy;

    state.dataLoopNodes->Node(WaterOutletNode).Temp = state.dataWaterCoils->WaterCoil(CoilNum).OutletWaterTemp;
    state.dataLoopNodes->Node(WaterOutletNode).Enthalpy = state.dataWaterCoils->WaterCoil(CoilNum).OutletWaterEnthalpy;

    // Set the outlet nodes for properties that just pass through & not used
    state.dataLoopNodes->Node(AirOutletNode).Quality = state.dataLoopNodes->Node(AirInletNode).Quality;
    state.dataLoopNodes->Node(AirOutletNode).Press = state.dataLoopNodes->Node(AirInletNode).Press;
    state.dataLoopNodes->Node(AirOutletNode).MassFlowRateMin = state.dataLoopNodes->Node(AirInletNode).MassFlowRateMin;
    state.dataLoopNodes->Node(AirOutletNode).MassFlowRateMax = state.dataLoopNodes->Node(AirInletNode).MassFlowRateMax;
    state.dataLoopNodes->Node(AirOutletNode).MassFlowRateMinAvail = state.dataLoopNodes->Node(AirInletNode).MassFlowRateMinAvail;
    state.dataLoopNodes->Node(AirOutletNode).MassFlowRateMaxAvail = state.dataLoopNodes->Node(AirInletNode).MassFlowRateMaxAvail;
    if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
        state.dataLoopNodes->Node(AirOutletNode).CO2 = state.dataLoopNodes->Node(AirInletNode).CO2;
    }
    if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
        state.dataLoopNodes->Node(AirOutletNode).GenContam = state.dataLoopNodes->Node(AirInletNode).GenContam;
    }
}

//        End of Update subroutines for the WaterCoil Module
// *****************************************************************************

// Beginning of Reporting subroutines for the WaterCoil Module
// *****************************************************************************

void ReportWaterCoil(EnergyPlusData &state, int const CoilNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard Liesen
    //       DATE WRITTEN   1998
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine updates the report variable for the coils.

    // METHODOLOGY EMPLOYED:
    // NA

    // REFERENCES:
    // na

    // Using/Aliasing

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:

    // SUBROUTINE PARAMETER DEFINITIONS:
    static constexpr std::string_view RoutineName("ReportWaterCoil");

    // INTERFACE BLOCK SPECIFICATIONS
    // na

    // DERIVED TYPE DEFINITIONS
    // na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 RhoWater;
    Real64 Tavg;
    Real64 SpecHumOut;
    Real64 SpecHumIn;
    Real64 ReportingConstant;

    if (state.dataWaterCoils->WaterCoil(CoilNum).reportCoilFinalSizes) {
        if (!state.dataGlobal->WarmupFlag && !state.dataGlobal->DoingHVACSizingSimulations && !state.dataGlobal->DoingSizing) {
            std::string coilObjClassName;
            if (state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType == DataPlant::TypeOf_CoilWaterSimpleHeating) {
                coilObjClassName = "Coil:Heating:Water";
                state.dataRptCoilSelection->coilSelectionReportObj->setCoilFinalSizes(
                    state,
                    state.dataWaterCoils->WaterCoil(CoilNum).Name,
                    coilObjClassName,
                    state.dataWaterCoils->WaterCoil(CoilNum).DesWaterHeatingCoilRate,
                    state.dataWaterCoils->WaterCoil(CoilNum).DesWaterHeatingCoilRate,
                    state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate,
                    state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate);
                state.dataWaterCoils->WaterCoil(CoilNum).reportCoilFinalSizes = false;
            } else if (state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType == DataPlant::TypeOf_CoilWaterDetailedFlatCooling) {
                coilObjClassName = "Coil:Cooling:Water:DetailedGeometry";
                state.dataRptCoilSelection->coilSelectionReportObj->setCoilFinalSizes(
                    state,
                    state.dataWaterCoils->WaterCoil(CoilNum).Name,
                    coilObjClassName,
                    state.dataWaterCoils->WaterCoil(CoilNum).DesWaterCoolingCoilRate,
                    -999.0,
                    state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate,
                    state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate);
                state.dataWaterCoils->WaterCoil(CoilNum).reportCoilFinalSizes = false;
            } else if (state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType == DataPlant::TypeOf_CoilWaterCooling) {
                coilObjClassName = "Coil:Cooling:Water";
                state.dataRptCoilSelection->coilSelectionReportObj->setCoilFinalSizes(
                    state,
                    state.dataWaterCoils->WaterCoil(CoilNum).Name,
                    coilObjClassName,
                    state.dataWaterCoils->WaterCoil(CoilNum).DesWaterCoolingCoilRate,
                    -999.0,
                    state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate,
                    state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate);
                state.dataWaterCoils->WaterCoil(CoilNum).reportCoilFinalSizes = false;
            }
        }
    }
    ReportingConstant = state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
    // report the WaterCoil energy from this component
    state.dataWaterCoils->WaterCoil(CoilNum).TotWaterHeatingCoilEnergy =
        state.dataWaterCoils->WaterCoil(CoilNum).TotWaterHeatingCoilRate * ReportingConstant;
    state.dataWaterCoils->WaterCoil(CoilNum).TotWaterCoolingCoilEnergy =
        state.dataWaterCoils->WaterCoil(CoilNum).TotWaterCoolingCoilRate * ReportingConstant;
    state.dataWaterCoils->WaterCoil(CoilNum).SenWaterCoolingCoilEnergy =
        state.dataWaterCoils->WaterCoil(CoilNum).SenWaterCoolingCoilRate * ReportingConstant;

    // report the WaterCoil water collection to water storage tank (if needed)

    if (state.dataWaterCoils->WaterCoil(CoilNum).CondensateCollectMode == state.dataWaterCoils->CondensateToTank) {
        // calculate and report condensation rates  (how much water extracted from the air stream)
        // water volumetric flow of water in m3/s for water system interactions
        //  put here to catch all types of DX coils
        Tavg = (state.dataWaterCoils->WaterCoil(CoilNum).InletAirTemp - state.dataWaterCoils->WaterCoil(CoilNum).OutletAirTemp) / 2.0;

        RhoWater = GetDensityGlycol(state,
                                    state.dataPlnt->PlantLoop(state.dataWaterCoils->WaterCoil(CoilNum).WaterLoopNum).FluidName,
                                    Tavg,
                                    state.dataPlnt->PlantLoop(state.dataWaterCoils->WaterCoil(CoilNum).WaterLoopNum).FluidIndex,
                                    RoutineName);
        //   CR9155 Remove specific humidity calculations
        SpecHumIn = state.dataWaterCoils->WaterCoil(CoilNum).InletAirHumRat;
        SpecHumOut = state.dataWaterCoils->WaterCoil(CoilNum).OutletAirHumRat;
        //  mdot * del HumRat / rho water
        state.dataWaterCoils->WaterCoil(CoilNum).CondensateVdot =
            max(0.0, (state.dataWaterCoils->WaterCoil(CoilNum).InletAirMassFlowRate * (SpecHumIn - SpecHumOut) / RhoWater));
        state.dataWaterCoils->WaterCoil(CoilNum).CondensateVol = state.dataWaterCoils->WaterCoil(CoilNum).CondensateVdot * ReportingConstant;

        state.dataWaterData->WaterStorage(state.dataWaterCoils->WaterCoil(CoilNum).CondensateTankID)
            .VdotAvailSupply(state.dataWaterCoils->WaterCoil(CoilNum).CondensateTankSupplyARRID) =
            state.dataWaterCoils->WaterCoil(CoilNum).CondensateVdot;
        state.dataWaterData->WaterStorage(state.dataWaterCoils->WaterCoil(CoilNum).CondensateTankID)
            .TwaterSupply(state.dataWaterCoils->WaterCoil(CoilNum).CondensateTankSupplyARRID) =
            state.dataWaterCoils->WaterCoil(CoilNum).OutletAirTemp;
    }
}

//        End of Reporting subroutines for the WaterCoil Module
// *****************************************************************************

// Beginning of Coil Utility subroutines for the Detailed Model
// *****************************************************************************

void CalcDryFinEffCoef(EnergyPlusData &state, Real64 const OutTubeEffFinDiamRatio, Array1D<Real64> &PolynomCoef)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR   Unknown
    //       DATE WRITTEN   Unknown
    //       DATE REWRITTEN  April 1997 by Russell D. Taylor, Ph.D.
    //       MODIFIED
    //       RE-ENGINEERED

    // PURPOSE OF THIS SUBROUTINE:
    // The following subroutines are used once per cooling coil
    // simulation to obtain the coefficients of the dry fin
    // efficiency equation.  CalcDryFinEffCoef is the main calling
    // routine which manages calls to the Bessel funtion and polynomial
    // fit routines.

    // REFERENCES:
    // First found in MODSIM.
    // USE STATEMENTS:
    // na

    // Argument array dimensioning

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:

    // SUBROUTINE PARAMETER DEFINITIONS:
    // na

    // INTERFACE BLOCK SPECIFICATIONS
    // na

    // DERIVED TYPE DEFINITIONS
    // na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 FAI;
    Real64 FED;
    Real64 FEDnumerator;
    int I;
    int IE1;
    int IE2;
    int IE3;
    int IE4;
    int IE5;
    int IE6;
    Real64 R1;
    Real64 R1I1;
    Real64 R1K1;
    Real64 R2;
    Real64 R2I0;
    Real64 R2I1;
    Real64 R2K0;
    Real64 R2K1;
    Real64 RO;

    FAI = 0.02;
    for (I = 1; I <= WaterCoils::MaxOrderedPairs; ++I) {
        FAI += 0.035;
        R1 = FAI / (1.0 - OutTubeEffFinDiamRatio);
        R2 = R1 * OutTubeEffFinDiamRatio;
        RO = 2.0 * OutTubeEffFinDiamRatio / (FAI * (1.0 + OutTubeEffFinDiamRatio));
        CalcIBesselFunc(R1, 1, R1I1, IE1);
        CalcKBesselFunc(R2, 1, R2K1, IE2);
        CalcIBesselFunc(R2, 1, R2I1, IE3);
        CalcKBesselFunc(R1, 1, R1K1, IE4);
        CalcIBesselFunc(R2, 0, R2I0, IE5);
        CalcKBesselFunc(R2, 0, R2K0, IE6);
        FEDnumerator = RO * (R1I1 * R2K1 - R2I1 * R1K1);
        if (FEDnumerator != 0.0) {
            FED = FEDnumerator / (R2I0 * R1K1 + R1I1 * R2K0);
        } else {
            FED = 0.0;
        }
        //      FED = RO * (R1I1 * R2K1 - R2I1 * R1K1) / (R2I0 * R1K1 + R1I1 * R2K0)
        state.dataWaterCoils->OrderedPair(I, 1) = FAI;
        state.dataWaterCoils->OrderedPair(I, 2) = FED;
    }
    CalcPolynomCoef(state, state.dataWaterCoils->OrderedPair, PolynomCoef);
}

void CalcIBesselFunc(Real64 const BessFuncArg, int const BessFuncOrd, Real64 &IBessFunc, int &ErrorCode)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR   Unknown
    //       DATE WRITTEN   Unknown
    //       DATE REWRITTEN  April 1997 by Russell D. Taylor, Ph.D.
    //       MODIFIED
    //       RE-ENGINEERED

    // PURPOSE OF THIS SUBROUTINE:
    // To calculate the modified Bessel Function from order 0 to BessFuncOrd
    // BessFuncArg    ARGUMENT OF BESSEL FUNCTION
    // BessFuncOrd    ORDER OF BESSEL FUNCTION, GREATER THAN OR EQUAL TO ZERO
    // IBessFunc   RESULTANT VALUE OF I BESSEL FUNCTION
    // ErrorCode  RESULTANT ERROR CODE:
    //       ErrorCode = 0   NO ERROR
    //       ErrorCode = 1   BessFuncOrd .LT. 0
    //       ErrorCode = 2   BessFuncArg .LT. 0
    //       ErrorCode = 3   IBessFunc .LT. 10**(-30),     IBessFunc IS SET TO 0
    //       ErrorCode = 4   BessFuncArg .GT. BessFuncOrd & BessFuncArg .GT. 90,  IBessFunc IS SET TO 10**38

    // REFERENCES:
    // First found in MODSIM.

    // USE STATEMENTS:
    // na

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:

    // SUBROUTINE PARAMETER DEFINITIONS:
    Real64 const ErrorTol(1.0e-06);

    // INTERFACE BLOCK SPECIFICATIONS
    // na

    // DERIVED TYPE DEFINITIONS
    // na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int LoopCount;

    Real64 FI;
    Real64 FK;
    Real64 TERM;

    ErrorCode = 0;
    IBessFunc = 1.0;
    if (BessFuncArg == 0.0 && BessFuncOrd == 0) return;

    if (BessFuncOrd < 0) {
        ErrorCode = 1;
        return;
    } else if (BessFuncArg < 0.0) {
        ErrorCode = 2;
        return;
    } else if (BessFuncArg > 12.0 && BessFuncArg > BessFuncOrd) {
        if (BessFuncArg > 90.0) {
            ErrorCode = 4;
            IBessFunc = 1.0e30;
            return;
        }
        TERM = 1.0;
        IBessFunc = 1.0;
        for (LoopCount = 1; LoopCount <= 30; ++LoopCount) { // Start of 1st LoopCount Loop
            if (std::abs(TERM) <= std::abs(ErrorTol * IBessFunc)) {
                IBessFunc *= std::exp(BessFuncArg) / std::sqrt(2.0 * DataGlobalConstants::Pi * BessFuncArg);
                return;
            }
            TERM *= 0.125 / BessFuncArg * (pow_2(2 * LoopCount - 1) - 4 * BessFuncOrd * BessFuncOrd) / double(LoopCount);
            IBessFunc += TERM;
        } // End of 1st LoopCount loop
    }

    TERM = 1.0;
    if (BessFuncOrd > 0) {
        for (LoopCount = 1; LoopCount <= BessFuncOrd; ++LoopCount) { // Start of 2nd LoopCount Loop
            FI = LoopCount;
            if (std::abs(TERM) < 1.0e-30 * FI / (BessFuncArg * 2.0)) {
                ErrorCode = 3;
                IBessFunc = 0.0;
                return;
            }
            TERM *= BessFuncArg / (2.0 * FI);
        } // End of 2nd LoopCount loop
    }

    IBessFunc = TERM;
    for (LoopCount = 1; LoopCount <= 1000; ++LoopCount) { // Start of 3rd LoopCount Loop
        if (std::abs(TERM) <= std::abs(IBessFunc * ErrorTol)) return;
        FK = LoopCount * (BessFuncOrd + LoopCount);
        TERM *= pow_2(BessFuncArg) / (4.0 * FK);
        IBessFunc += TERM;
    } // End of  3rd LoopCount loop
}

void CalcKBesselFunc(Real64 const BessFuncArg, int const BessFuncOrd, Real64 &KBessFunc, int &ErrorCode)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR   Unknown
    //       DATE WRITTEN   Unknown
    //       DATE REWRITTEN  April 1997 by Russell D. Taylor, Ph.D.
    //       MODIFIED
    //       RE-ENGINEERED

    // PURPOSE OF THIS SUBROUTINE:
    // To calculate the K Bessel Function for a given argument and
    // order
    //  BessFuncArg    THE ARGUMENT OF THE K BESSEL FUNCTION DESIRED
    //  BessFuncOrd    THE ORDER OF THE K BESSEL FUNCTION DESIRED
    //  KBessFunc   THE RESULTANT K BESSEL FUNCTION
    //  ErrorCode  RESULTANT ERROR CODE:
    //        ErrorCode=0  NO ERROR
    //        ErrorCode=1  BessFuncOrd IS NEGATIVE
    //        ErrorCode=2  BessFuncArg IS ZERO OR NEGATIVE
    //        ErrorCode=3  BessFuncArg .GT. 85, KBessFunc .LT. 10**-38; KBessFunc SET TO 0.
    //        ErrorCode=4  KBessFunc .GT. 10**38; KBessFunc SET TO 10**38
    // NOTE: BessFuncOrd MUST BE GREATER THAN OR EQUAL TO ZERO
    // METHOD:
    //  COMPUTES ZERO ORDER AND FIRST ORDER BESSEL FUNCTIONS USING
    //  SERIES APPROXIMATIONS AND THEN COMPUTES BessFuncOrd TH ORDER FUNCTION
    //  USING RECURRENCE RELATION.
    //  RECURRENCE RELATION AND POLYNOMIAL APPROXIMATION TECHNIQUE
    //  AS DESCRIBED BY A.J.M. HITCHCOCK, 'POLYNOMIAL APPROXIMATIONS
    //  TO BESSEL FUNCTIONS OF ORDER ZERO AND ONE AND TO RELATED
    //  FUNCTIONS,' M.T.A.C., V.11, 1957, PP. 86-88, AND G.BessFuncOrd. WATSON,
    //  'A TREATISE ON THE THEORY OF BESSEL FUNCTIONS,' CAMBRIDGE
    //  UNIVERSITY PRESS, 1958, P.62

    // USE STATEMENTS:
    // na

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:

    // SUBROUTINE PARAMETER DEFINITIONS:
    Real64 const GJMAX(1.0e+38);

    // INTERFACE BLOCK SPECIFICATIONS
    // na

    // DERIVED TYPE DEFINITIONS
    // na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int LoopCount;
    bool StopLoop;

    Real64 FACT;
    Real64 G0;
    Real64 G1;
    Real64 GJ;
    Real64 HJ;
    Array1D<Real64> T(12);
    Real64 X2J;

    KBessFunc = 0.0;
    G0 = 0.0;
    GJ = 0.0;

    if (BessFuncOrd < 0.0) {
        ErrorCode = 1;
        return;
    } else if (BessFuncArg <= 0.0) {
        ErrorCode = 2;
        return;
    } else if (BessFuncArg > 85.0) {
        ErrorCode = 3;
        KBessFunc = 0.0;
        return;
    }

    ErrorCode = 0;

    //     Use polynomial approximation if BessFuncArg > 1.

    if (BessFuncArg > 1.0) {
        T(1) = 1.0 / BessFuncArg;
        for (LoopCount = 2; LoopCount <= 12; ++LoopCount) {
            T(LoopCount) = T(LoopCount - 1) / BessFuncArg;
        } // End of LoopCount Loop
        if (BessFuncOrd != 1) {

            //     Compute K0 using polynomial approximation

            G0 = std::exp(-BessFuncArg) *
                 (1.2533141 - 0.1566642 * T(1) + 0.08811128 * T(2) - 0.09139095 * T(3) + 0.1344596 * T(4) - 0.2299850 * T(5) + 0.3792410 * T(6) -
                  0.5247277 * T(7) + 0.5575368 * T(8) - 0.4262633 * T(9) + 0.2184518 * T(10) - 0.06680977 * T(11) + 0.009189383 * T(12)) *
                 std::sqrt(1.0 / BessFuncArg);
            if (BessFuncOrd == 0) {
                KBessFunc = G0;
                return;
            }
        }

        //     Compute K1 using polynomial approximation

        G1 = std::exp(-BessFuncArg) *
             (1.2533141 + 0.4699927 * T(1) - 0.1468583 * T(2) + 0.1280427 * T(3) - 0.1736432 * T(4) + 0.2847618 * T(5) - 0.4594342 * T(6) +
              0.6283381 * T(7) - 0.6632295 * T(8) + 0.5050239 * T(9) - 0.2581304 * T(10) + 0.07880001 * T(11) - 0.01082418 * T(12)) *
             std::sqrt(1.0 / BessFuncArg);
        if (BessFuncOrd == 1) {
            KBessFunc = G1;
            return;
        }
    } else {

        //     Use series expansion if BessFuncArg <= 1.

        if (BessFuncOrd != 1) {

            //     Compute K0 using series expansion

            G0 = -(0.5772157 + std::log(BessFuncArg / 2.0));
            X2J = 1.0;
            FACT = 1.0;
            HJ = 0.0;
            for (LoopCount = 1; LoopCount <= 6; ++LoopCount) {
                X2J *= pow_2(BessFuncArg) / 4.0;
                FACT *= pow_2(1.0 / double(LoopCount));
                HJ += 1.0 / double(LoopCount);
                G0 += X2J * FACT * (HJ - (0.5772157 + std::log(BessFuncArg / 2.0)));
            } // End of LoopCount Loop
            if (BessFuncOrd == 0.0) {
                KBessFunc = G0;
                return;
            }
        }

        //     Compute K1 using series expansion

        X2J = BessFuncArg / 2.0;
        FACT = 1.0;
        HJ = 1.0;
        G1 = 1.0 / BessFuncArg + X2J * (0.5 + (0.5772157 + std::log(BessFuncArg / 2.0)) - HJ);
        for (LoopCount = 2; LoopCount <= 8; ++LoopCount) {
            X2J *= pow_2(BessFuncArg) / 4.0;
            FACT *= pow_2(1.0 / double(LoopCount));
            HJ += 1.0 / double(LoopCount);
            G1 += X2J * FACT * (0.5 + ((0.5772157 + std::log(BessFuncArg / 2.0)) - HJ) * double(LoopCount));
        } // End of LoopCount Loop
        if (BessFuncOrd == 1) {
            KBessFunc = G1;
            return;
        }
    }

    //     From K0 and K1 compute KN using recurrence relation

    LoopCount = 2;
    StopLoop = false;
    while (LoopCount <= BessFuncOrd && !StopLoop) {
        GJ = 2.0 * (double(LoopCount) - 1.0) * G1 / BessFuncArg + G0;
        if (GJ - GJMAX > 0.0) {
            ErrorCode = 4;
            GJ = GJMAX;
            StopLoop = true;
        } else {
            G0 = G1;
            G1 = GJ;
            ++LoopCount;
        }
    } // End of LoopCount Loop
    KBessFunc = GJ;
}

void CalcPolynomCoef(EnergyPlusData &state, Array2<Real64> const &OrderedPair, Array1D<Real64> &PolynomCoef)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR   Unknown
    //       DATE WRITTEN   Unknown
    //       DATE REWRITTEN  April 1997 by Russell D. Taylor, Ph.D.
    //       MODIFIED
    //       RE-ENGINEERED

    // PURPOSE OF THIS SUBROUTINE:
    // Fits polynomial of order from 1 to MaxPolynomOrder to the
    // ordered pairs of data points X,Y

    // USE STATEMENTS:
    // na

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:

    // SUBROUTINE PARAMETER DEFINITIONS:
    // na

    // INTERFACE BLOCK SPECIFICATIONS
    // na

    // DERIVED TYPE DEFINITIONS
    // na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    bool Converged;
    Real64 B;
    int I;
    int II;
    int J;
    int PolynomOrder;
    int CurrentOrder;
    int CurrentOrdPair;
    Real64 S1;
    Real64 S2;

    auto &OrdPairSum(state.dataWaterCoils->OrdPairSum);
    auto &OrdPairSumMatrix(state.dataWaterCoils->OrdPairSumMatrix);

    OrdPairSum = 0.0;
    OrdPairSum(1, 1) = WaterCoils::MaxOrderedPairs;
    PolynomCoef = 0.0;
    for (CurrentOrdPair = 1; CurrentOrdPair <= WaterCoils::MaxOrderedPairs; ++CurrentOrdPair) {
        OrdPairSum(2, 1) += OrderedPair(CurrentOrdPair, 1);
        OrdPairSum(3, 1) += OrderedPair(CurrentOrdPair, 1) * OrderedPair(CurrentOrdPair, 1);
        OrdPairSum(1, 2) += OrderedPair(CurrentOrdPair, 2);
        OrdPairSum(2, 2) += OrderedPair(CurrentOrdPair, 1) * OrderedPair(CurrentOrdPair, 2);
    }
    PolynomOrder = 1;
    Converged = false;
    while (!Converged) {
        for (CurrentOrder = 1; CurrentOrder <= PolynomOrder + 1; ++CurrentOrder) {
            for (J = 1; J <= PolynomOrder + 1; ++J) {
                OrdPairSumMatrix(J, CurrentOrder) = OrdPairSum(J - 1 + CurrentOrder, 1);
            } // End of J loop
            OrdPairSumMatrix(PolynomOrder + 2, CurrentOrder) = OrdPairSum(CurrentOrder, 2);
        } // End of CurrentOrder loop

        for (CurrentOrder = 1; CurrentOrder <= PolynomOrder + 1; ++CurrentOrder) {
            OrdPairSumMatrix(CurrentOrder, PolynomOrder + 2) = -1.0;
            for (J = CurrentOrder + 1; J <= PolynomOrder + 2; ++J) {
                OrdPairSumMatrix(J, PolynomOrder + 2) = 0.0;
            } // End of J loop

            for (II = 2; II <= PolynomOrder + 2; ++II) {
                for (J = CurrentOrder + 1; J <= PolynomOrder + 2; ++J) {
                    OrdPairSumMatrix(J, II) -= OrdPairSumMatrix(J, 1) * OrdPairSumMatrix(CurrentOrder, II) / OrdPairSumMatrix(CurrentOrder, 1);
                } // End of J loop
            }     // End of II loop
            for (II = 1; II <= PolynomOrder + 1; ++II) {
                for (J = CurrentOrder + 1; J <= PolynomOrder + 2; ++J) {
                    OrdPairSumMatrix(J, II) = OrdPairSumMatrix(J, II + 1);
                } // End of J loop
            }     // End of II loop
        }         // End of CurrentOrder loop

        S2 = 0.0;
        for (CurrentOrdPair = 1; CurrentOrdPair <= WaterCoils::MaxOrderedPairs; ++CurrentOrdPair) {
            S1 = OrdPairSumMatrix(PolynomOrder + 2, 1);
            auto const OrderedPair1C(OrderedPair(CurrentOrdPair, 1));
            auto OrderedPair1C_pow(1.0);
            for (CurrentOrder = 1; CurrentOrder <= PolynomOrder; ++CurrentOrder) {
                OrderedPair1C_pow *= OrderedPair1C;
                S1 += OrdPairSumMatrix(PolynomOrder + 2, CurrentOrder + 1) * OrderedPair1C_pow;
            } // End of CurrentOrder loop
            S2 += (S1 - OrderedPair(CurrentOrdPair, 2)) * (S1 - OrderedPair(CurrentOrdPair, 2));
        } // End of CurrentOrdPair loop
        B = WaterCoils::MaxOrderedPairs - (PolynomOrder + 1);
        if (S2 > 0.0001) S2 = std::sqrt(S2 / B);
        for (CurrentOrder = 1; CurrentOrder <= PolynomOrder + 1; ++CurrentOrder) {
            PolynomCoef(CurrentOrder) = OrdPairSumMatrix(PolynomOrder + 2, CurrentOrder);
        } // End of CurrentOrder loop

        if ((PolynomOrder - WaterCoils::MaxPolynomOrder < 0) && (S2 - WaterCoils::PolyConvgTol > 0.0)) {
            ++PolynomOrder;
            J = 2 * PolynomOrder;
            OrdPairSum(J, 1) = OrdPairSum(J + 1, 1) = 0.0;
            auto OrdPairSum2P = OrdPairSum(PolynomOrder + 1, 2) = 0.0;
            for (I = 1; I <= WaterCoils::MaxOrderedPairs; ++I) {
                auto const OrderedPair1I(OrderedPair(I, 1));
                auto OrderedPair_pow(std::pow(OrderedPair1I, J - 1));
                OrdPairSum(J, 1) += OrderedPair_pow;
                OrderedPair_pow *= OrderedPair1I;
                OrdPairSum(J + 1, 1) += OrderedPair_pow;
                OrdPairSum2P += OrderedPair(I, 2) * std::pow(OrderedPair1I, PolynomOrder);
            }
            OrdPairSum(PolynomOrder + 1, 2) = OrdPairSum2P;
        } else {
            Converged = true;
        }
    }
}

Real64 SimpleHeatingCoilUAResidual(EnergyPlusData &state,
                                   Real64 const UA,           // UA of coil
                                   Array1D<Real64> const &Par // par(1) = design coil load [W]
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   November 2001
    //       MODIFIED
    //       RE-ENGINEERED

    // PURPOSE OF THIS FUNCTION:
    // Calculates residual function (Design Coil Load - Coil Heating Output) / Design Coil Load.
    // Coil Heating Output depends on the UA which is being varied to zero the residual.

    // METHODOLOGY EMPLOYED:
    // Puts UA into the water coil data structure, calls CalcSimpleHeatingCoil, and calculates
    // the residual as defined above.

    // Return value
    Real64 Residuum; // residual to be minimized to zero

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    int CoilIndex;
    int FanOpMode;
    Real64 PartLoadRatio;

    CoilIndex = int(Par(2));
    FanOpMode = (Par(3) == 1.0 ? CycFanCycCoil : ContFanCycCoil);
    PartLoadRatio = Par(4);
    state.dataWaterCoils->WaterCoil(CoilIndex).UACoilVariable = UA;
    CalcSimpleHeatingCoil(state, CoilIndex, FanOpMode, PartLoadRatio, state.dataWaterCoils->SimCalc);
    Residuum = (Par(1) - state.dataWaterCoils->WaterCoil(CoilIndex).TotWaterHeatingCoilRate) / Par(1);
    state.dataSize->DataDesignCoilCapacity = state.dataWaterCoils->WaterCoil(CoilIndex).TotWaterHeatingCoilRate;

    return Residuum;
}

Real64 SimpleCoolingCoilUAResidual(EnergyPlusData &state,
                                   Real64 const UA,           // UA of coil
                                   Array1D<Real64> const &Par // par(1) = design coil load [W]
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   September 2011
    //       MODIFIED
    //       RE-ENGINEERED

    // PURPOSE OF THIS FUNCTION:
    // Calculates residual function (Design Coil Load - Coil Cooling Output) / Design Coil Load.
    // Coil Cooling Output depends on the UA which is being varied to zero the residual.

    // METHODOLOGY EMPLOYED:
    // Puts UA into the water coil data structure, calls CoolingCoil, and calculates
    // the residual as defined above.

    // REFERENCES:

    // USE STATEMENTS:
    // na

    // Return value
    Real64 Residuum; // residual to be minimized to zero

    // Argument array dimensioning

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:

    // FUNCTION PARAMETER DEFINITIONS:
    // na

    // INTERFACE BLOCK SPECIFICATIONS
    // na

    // DERIVED TYPE DEFINITIONS
    // na

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    int CoilIndex;
    int FanOpMode;
    Real64 PartLoadRatio;

    CoilIndex = int(Par(2));
    FanOpMode = (Par(3) == 1.0 ? CycFanCycCoil : ContFanCycCoil);
    PartLoadRatio = Par(4);
    state.dataWaterCoils->WaterCoil(CoilIndex).UACoilExternal = UA;
    state.dataWaterCoils->WaterCoil(CoilIndex).UACoilInternal = state.dataWaterCoils->WaterCoil(CoilIndex).UACoilExternal * 3.3;
    state.dataWaterCoils->WaterCoil(CoilIndex).UACoilTotal =
        1.0 / (1.0 / state.dataWaterCoils->WaterCoil(CoilIndex).UACoilExternal + 1.0 / state.dataWaterCoils->WaterCoil(CoilIndex).UACoilInternal);
    state.dataWaterCoils->WaterCoil(CoilIndex).TotCoilOutsideSurfArea = EstimateHEXSurfaceArea(state, CoilIndex);
    state.dataWaterCoils->WaterCoil(CoilIndex).UACoilInternalPerUnitArea =
        state.dataWaterCoils->WaterCoil(CoilIndex).UACoilInternal / state.dataWaterCoils->WaterCoil(CoilIndex).TotCoilOutsideSurfArea;
    state.dataWaterCoils->WaterCoil(CoilIndex).UAWetExtPerUnitArea =
        state.dataWaterCoils->WaterCoil(CoilIndex).UACoilExternal / state.dataWaterCoils->WaterCoil(CoilIndex).TotCoilOutsideSurfArea;
    state.dataWaterCoils->WaterCoil(CoilIndex).UADryExtPerUnitArea = state.dataWaterCoils->WaterCoil(CoilIndex).UAWetExtPerUnitArea;

    CoolingCoil(state, CoilIndex, true, state.dataWaterCoils->DesignCalc, FanOpMode, PartLoadRatio);

    Residuum = (Par(1) - state.dataWaterCoils->WaterCoil(CoilIndex).TotWaterCoolingCoilRate) / Par(1);

    return Residuum;
}

// Iterate Routine for Cooling Coil

void CoilAreaFracIter(Real64 &NewSurfAreaWetFrac,       // Out Value of variable
                      Real64 const SurfAreaFracCurrent, // Driver Value
                      Real64 const ErrorCurrent,        // Objective Function
                      Real64 &SurfAreaFracPrevious,     // First Previous value of Surf Area Fraction
                      Real64 &ErrorPrevious,            // First Previous value of error
                      Real64 &SurfAreaFracLast,         // Second Previous value of Surf Area Fraction
                      Real64 &ErrorLast,                // Second Previous value of error
                      int const IterNum,                // Number of Iterations
                      int &icvg                         // Iteration convergence flag
)
{
    // FUNCTION INFORMATION:
    // AUTHOR         Rahul Chillar
    // DATE WRITTEN   June 2004
    // MODIFIED       na
    // RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // Iterately solves for the value of SurfAreaWetFraction for the Cooling Coil.

    // METHODOLOGY EMPLOYED:
    // First function generates 2 sets of guess points by perturbation and subsequently
    // by Linear Fit and using the generated points calculates coeffecients for Quadratic
    // fit to predict the next value of surface area wet fraction.

    // REFERENCES:
    // ME 423 Design of Thermal Systems Class Notes.UIUC. W.F.Stoecker

    // USE STATEMENTS:
    // na

    // Enforce explicit typing of all variables in this routine

    // Locals
    // FUNCTION ARGUMENT DEFINITIONS:

    // FUNCTION PARAMETER DEFINITIONS:
    Real64 const Tolerance(1.e-5);         // Relative error tolerance
    Real64 const PerturbSurfAreaFrac(0.1); // Perturbation applied to Surf Fraction to initialize iteration
    Real64 const SmallNum(1.e-9);          // Small Number

    // INTERFACE BLOCK SPECIFICATIONS
    // na

    // DERIVED TYPE DEFINITIONS
    // na

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    Real64 check;             // Validity Check for moving to Quad Solution
    Real64 QuadCoefThree;     // Term under radical in quadratic solution
    Real64 QuadCoefOne;       // Term under radical in quadratic solution
    Real64 QuadCoefTwo;       // Term under radical in quadratic solution
    Real64 Slope;             // Slope for linear fit
    Real64 SurfAreaFracOther; // Intermediate Value of Surf Area
    int mode;                 // Linear/ perturbation option

    // Convergence Check  by comparing previous and current value of surf area fraction
    if ((std::abs(SurfAreaFracCurrent - SurfAreaFracPrevious) < Tolerance * max(std::abs(SurfAreaFracCurrent), SmallNum) && IterNum != 1) ||
        ErrorCurrent == 0.0) {
        // Setting value for surface area fraction for coil
        NewSurfAreaWetFrac = SurfAreaFracCurrent;
        icvg = 1; // Convergance Flag
        return;
    }

    // If Icvg = 0 , it has not converged.By perturbation for getting second set of
    // data (mode=1), Getting Third set of data by performing a  linear fit(Mode=2).
    // Now using the above 3 points generated by perturbation and Linear Fit to perform
    // a quadratic fit.This will happen after second iteration only.
    icvg = 0; // Convergance flag = false
    // For First Iteration Start with perturbation, For second iteration start with linear fit
    // from the previous two values
    mode = IterNum;

Label10:;
    if (mode == 1) {

        // FirstGuess Set of Points provided by perturbation
        if (std::abs(SurfAreaFracCurrent) > SmallNum) {
            NewSurfAreaWetFrac = SurfAreaFracCurrent * (1.0 + PerturbSurfAreaFrac);
        } else {
            NewSurfAreaWetFrac = PerturbSurfAreaFrac;
        }

        // Second set of values being calculated from the first set of values (incoming & perturb)
    } else if (mode == 2) {

        // Calculating Slope for interpolating to the New Point (Simple Linear Extrapolation)
        Slope = (ErrorPrevious - ErrorCurrent) / (SurfAreaFracPrevious - SurfAreaFracCurrent);
        // Error Check for value or Slope
        if (Slope == 0.0) {
            mode = 1; // Go back to Perturbation
            goto Label10;
        }
        // Guessing New Value for Surface Area Fraction
        NewSurfAreaWetFrac = SurfAreaFracCurrent - ErrorCurrent / Slope;
    } else {

        // Check for Quadratic Fit possible here ,Previous value of surf area fraction
        // equals current value then Try linear fit for another point.
        if (SurfAreaFracCurrent == SurfAreaFracPrevious) {
            // Assign Value of previous point to Last Variable for storing
            // Go back and calculate new value for Previous.
            SurfAreaFracPrevious = SurfAreaFracLast;
            ErrorPrevious = ErrorLast;
            mode = 2;
            goto Label10;
        } else if (SurfAreaFracCurrent == SurfAreaFracLast) {
            // Calculate another value using Linear Fit.
            mode = 2;
            goto Label10;
        }

        // Now We have enough previous points to calculate coefficients and
        // perform a quadratic fit for new guess value of surface area fraction

        // Calculating First Coefficients for Quadratic Curve Fit
        QuadCoefThree = ((ErrorLast - ErrorCurrent) / (SurfAreaFracLast - SurfAreaFracCurrent) -
                         (ErrorPrevious - ErrorCurrent) / (SurfAreaFracPrevious - SurfAreaFracCurrent)) /
                        (SurfAreaFracLast - SurfAreaFracPrevious);
        // Calculating Second Coefficients for Quadratic Curve Fit
        QuadCoefTwo = (ErrorPrevious - ErrorCurrent) / (SurfAreaFracPrevious - SurfAreaFracCurrent) -
                      (SurfAreaFracPrevious + SurfAreaFracCurrent) * QuadCoefThree;

        // Calculating Third Coefficients for Quadratic Curve Fit
        QuadCoefOne = ErrorCurrent - (QuadCoefTwo + QuadCoefThree * SurfAreaFracCurrent) * SurfAreaFracCurrent;

        // Check for validity of coefficients , if not REAL(r64) ,Then fit is linear
        if (std::abs(QuadCoefThree) < 1.E-10) {
            mode = 2; // going to Linear mode, due to colinear points.
            goto Label10;
        }

        // If value of Quadratic coefficients not suitable enought due to round off errors
        // to predict new point go to linear fit and acertain new values for the coefficients.
        if (std::abs((QuadCoefOne + (QuadCoefTwo + QuadCoefThree * SurfAreaFracPrevious) * SurfAreaFracPrevious - ErrorPrevious) / ErrorPrevious) >
            1.E-4) {
            mode = 2; // go to linear mode
            goto Label10;
        }

        // Validity Check for Imaginary roots, In this case go back to linear fit.
        check = pow_2(QuadCoefTwo) - 4.0 * QuadCoefOne * QuadCoefThree;
        // Imaginary Root Exist
        if (check < 0) {
            mode = 2;
            goto Label10;
        } else if (check > 0) {
            // real unequal roots exist, Determine the roots nearest to most recent guess
            NewSurfAreaWetFrac = (-QuadCoefTwo + std::sqrt(check)) / QuadCoefThree / 2.0;
            SurfAreaFracOther = -NewSurfAreaWetFrac - QuadCoefTwo / QuadCoefThree;
            // Assigning value to Surface Area Fraction with recent
            if (std::abs(NewSurfAreaWetFrac - SurfAreaFracCurrent) > std::abs(SurfAreaFracOther - SurfAreaFracCurrent))
                NewSurfAreaWetFrac = SurfAreaFracOther;
        } else {
            // The roots are real, one solution exists.
            NewSurfAreaWetFrac = -QuadCoefTwo / QuadCoefThree / 2;
        }
    }

    if (mode < 3) {
        // No valid previous points to eliminate, since it just has 2 points.
        // Loading previous values into last
        SurfAreaFracLast = SurfAreaFracPrevious;
        ErrorLast = ErrorPrevious;
        // Loading Current Values into previous
        SurfAreaFracPrevious = SurfAreaFracCurrent;
        ErrorPrevious = ErrorCurrent;
    } else {

        // Elimination the most distance previous point from the answer based on sign and
        // magnitute of the error. Keeping Current Point
        if (ErrorPrevious * ErrorCurrent > 0 && ErrorLast * ErrorCurrent > 0) {
            // If sign are same , simply eliminate the one with biggest error value.
            if (std::abs(ErrorLast) > std::abs(ErrorPrevious)) {
                // Eliminating Last Value
                SurfAreaFracLast = SurfAreaFracPrevious;
                ErrorLast = ErrorPrevious;
            }
        } else {
            // If signs are different eliminate previous error with same sign as current error
            if (ErrorLast * ErrorCurrent > 0) {
                // Previous Loaded to Last
                SurfAreaFracLast = SurfAreaFracPrevious;
                ErrorLast = ErrorPrevious;
            }
        }
        // Current Loaded into previous.
        SurfAreaFracPrevious = SurfAreaFracCurrent;
        ErrorPrevious = ErrorCurrent;
    }
}

void CheckWaterCoilSchedule(EnergyPlusData &state,
                            [[maybe_unused]] std::string const &CompType, // unused1208
                            std::string_view CompName,
                            Real64 &Value,
                            int &CompIndex)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   October 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // <description>

    // Using/Aliasing

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int CoilNum;

    // Obtains and Allocates WaterCoil related parameters from input file
    if (state.dataWaterCoils->GetWaterCoilsInputFlag) { // First time subroutine has been entered
        GetWaterCoilInput(state);
        state.dataWaterCoils->GetWaterCoilsInputFlag = false;
    }

    // Find the correct Coil number
    if (CompIndex == 0) {
        CoilNum = UtilityRoutines::FindItemInList(CompName, state.dataWaterCoils->WaterCoil);
        if (CoilNum == 0) {
            ShowFatalError(state, "CheckWaterCoilSchedule: Coil not found=" + std::string{CompName});
        }
        CompIndex = CoilNum;
        Value = GetCurrentScheduleValue(state, state.dataWaterCoils->WaterCoil(CoilNum).SchedPtr); // not scheduled?
    } else {
        CoilNum = CompIndex;
        if (CoilNum > state.dataWaterCoils->NumWaterCoils || CoilNum < 1) {
            ShowFatalError(state,
                           format("CheckWaterCoilSchedule: Invalid CompIndex passed={}, Number of Heating Coils={}, Coil name={}",
                                  CoilNum,
                                  state.dataWaterCoils->NumWaterCoils,
                                  CompName));
        }
        if (CompName != state.dataWaterCoils->WaterCoil(CoilNum).Name) {
            ShowFatalError(state,
                           format("CheckWaterCoilSchedule: Invalid CompIndex passed={}, Coil name={}, stored Coil Name for that index={}",
                                  CoilNum,
                                  CompName,
                                  state.dataWaterCoils->WaterCoil(CoilNum).Name));
        }
        Value = GetCurrentScheduleValue(state, state.dataWaterCoils->WaterCoil(CoilNum).SchedPtr); // not scheduled?
    }
}

Real64 GetCoilMaxWaterFlowRate(EnergyPlusData &state,
                               std::string const &CoilType, // must match coil types in this module
                               std::string const &CoilName, // must match coil names for the coil type
                               bool &ErrorsFound            // set to true if problem
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   November 2006
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This function looks up the max water flow rate for the given coil and returns it.  If
    // incorrect coil type or name is given, ErrorsFound is returned as true and capacity is returned
    // as negative.

    // Return value
    Real64 MaxWaterFlowRate; // returned max water flow rate of matched coil

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    int WhichCoil;

    // Obtains and Allocates WaterCoil related parameters from input file
    if (state.dataWaterCoils->GetWaterCoilsInputFlag) { // First time subroutine has been entered
        GetWaterCoilInput(state);
        state.dataWaterCoils->GetWaterCoilsInputFlag = false;
    }

    WhichCoil = 0;
    if (UtilityRoutines::SameString(CoilType, "Coil:Heating:Water") || UtilityRoutines::SameString(CoilType, "Coil:Cooling:Water:DetailedGeometry") ||
        UtilityRoutines::SameString(CoilType, "Coil:Cooling:Water")) {
        WhichCoil = UtilityRoutines::FindItem(CoilName, state.dataWaterCoils->WaterCoil);
        if (WhichCoil != 0) {
            // coil does not specify MaxWaterFlowRate
            MaxWaterFlowRate = state.dataWaterCoils->WaterCoil(WhichCoil).MaxWaterVolFlowRate;
        }
    } else {
        WhichCoil = 0;
    }

    if (WhichCoil == 0) {
        ShowSevereError(state, "GetCoilMaxWaterFlowRate: Could not find Coil, Type=\"" + CoilType + "\" Name=\"" + CoilName + "\"");
        ShowContinueError(state, "... Max Water Flow rate returned as -1000.");
        ErrorsFound = true;
        MaxWaterFlowRate = -1000.0;
    }

    return MaxWaterFlowRate;
}

int GetCoilInletNode(EnergyPlusData &state,
                     std::string const &CoilType, // must match coil types in this module
                     std::string const &CoilName, // must match coil names for the coil type
                     bool &ErrorsFound            // set to true if problem
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         R. Raustad
    //       DATE WRITTEN   March 2007
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This function looks up the given coil and returns the inlet node number.  If
    // incorrect coil type or name is given, ErrorsFound is returned as true and node number is returned
    // as zero.

    // Return value
    int NodeNumber; // returned node number of matched coil

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    int WhichCoil;

    // Obtains and Allocates DXCoils
    if (state.dataWaterCoils->GetWaterCoilsInputFlag) {
        GetWaterCoilInput(state);
        state.dataWaterCoils->GetWaterCoilsInputFlag = false;
    }

    NodeNumber = 0;
    WhichCoil = 0;
    if (UtilityRoutines::SameString(CoilType, "Coil:Heating:Water") || UtilityRoutines::SameString(CoilType, "Coil:Cooling:Water:DetailedGeometry") ||
        UtilityRoutines::SameString(CoilType, "Coil:Cooling:Water")) {
        WhichCoil = UtilityRoutines::FindItem(CoilName, state.dataWaterCoils->WaterCoil);
        if (WhichCoil != 0) {
            NodeNumber = state.dataWaterCoils->WaterCoil(WhichCoil).AirInletNodeNum;
        }
    } else {
        WhichCoil = 0;
    }

    if (WhichCoil == 0) {
        ShowSevereError(state, "GetCoilInletNode: Could not find Coil, Type=\"" + CoilType + "\" Name=\"" + CoilName + "\"");
        ErrorsFound = true;
        NodeNumber = 0;
    }

    return NodeNumber;
}

int GetCoilOutletNode(EnergyPlusData &state,
                      std::string const &CoilType, // must match coil types in this module
                      std::string const &CoilName, // must match coil names for the coil type
                      bool &ErrorsFound            // set to true if problem
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         R. Raustad
    //       DATE WRITTEN   March 2007
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This function looks up the given coil and returns the inlet node number.  If
    // incorrect coil type or name is given, ErrorsFound is returned as true and node number is returned
    // as zero.

    // Return value
    int NodeNumber; // returned node number of matched coil

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    int WhichCoil;

    // Obtains and Allocates DXCoils
    if (state.dataWaterCoils->GetWaterCoilsInputFlag) {
        GetWaterCoilInput(state);
        state.dataWaterCoils->GetWaterCoilsInputFlag = false;
    }

    WhichCoil = 0;
    NodeNumber = 0;
    if (UtilityRoutines::SameString(CoilType, "Coil:Heating:Water") || UtilityRoutines::SameString(CoilType, "Coil:Cooling:Water:DetailedGeometry") ||
        UtilityRoutines::SameString(CoilType, "Coil:Cooling:Water")) {
        WhichCoil = UtilityRoutines::FindItem(CoilName, state.dataWaterCoils->WaterCoil);
        if (WhichCoil != 0) {
            NodeNumber = state.dataWaterCoils->WaterCoil(WhichCoil).AirOutletNodeNum;
        }
    } else {
        WhichCoil = 0;
    }

    if (WhichCoil == 0) {
        ShowSevereError(state,
                        "GetCoilOutletNode: Could not find Coil, Type=\"" + CoilType + "\" Name=\"" + CoilName +
                            "\" when accessing coil outlet node number.");
        ErrorsFound = true;
        NodeNumber = 0;
    }

    return NodeNumber;
}

int GetCoilWaterInletNode(EnergyPlusData &state,
                          std::string const &CoilType, // must match coil types in this module
                          std::string const &CoilName, // must match coil names for the coil type
                          bool &ErrorsFound            // set to true if problem
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         R. Raustad
    //       DATE WRITTEN   July 2007
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This function looks up the given coil and returns the inlet water control node number.  If
    // incorrect coil type or name is given, ErrorsFound is returned as true and node number is returned
    // as zero.

    // Return value
    int NodeNumber; // returned node number of matched coil

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    int WhichCoil;

    // Obtains and Allocates DXCoils
    if (state.dataWaterCoils->GetWaterCoilsInputFlag) {
        GetWaterCoilInput(state);
        state.dataWaterCoils->GetWaterCoilsInputFlag = false;
    }

    NodeNumber = 0;
    WhichCoil = 0;
    if (UtilityRoutines::SameString(CoilType, "Coil:Heating:Water") || UtilityRoutines::SameString(CoilType, "Coil:Cooling:Water:DetailedGeometry") ||
        UtilityRoutines::SameString(CoilType, "Coil:Cooling:Water")) {
        WhichCoil = UtilityRoutines::FindItem(CoilName, state.dataWaterCoils->WaterCoil);
        if (WhichCoil != 0) {
            NodeNumber = state.dataWaterCoils->WaterCoil(WhichCoil).WaterInletNodeNum;
        }
    } else {
        WhichCoil = 0;
    }

    if (WhichCoil == 0) {
        ShowSevereError(state, "GetCoilWaterInletNode: Could not find Coil, Type=\"" + CoilType + "\" Name=\"" + CoilName + "\"");
        ErrorsFound = true;
        NodeNumber = 0;
    }

    return NodeNumber;
}

int GetCoilWaterOutletNode(EnergyPlusData &state,
                           std::string const &CoilType, // must match coil types in this module
                           std::string const &CoilName, // must match coil names for the coil type
                           bool &ErrorsFound            // set to true if problem
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         R. Raustad
    //       DATE WRITTEN   July 2007
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This function looks up the given coil and returns the outlet water node number.  If
    // incorrect coil type or name is given, ErrorsFound is returned as true and node number is returned
    // as zero.

    // Return value
    int NodeNumber; // returned node number of matched coil

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    int WhichCoil;

    // Obtains and Allocates DXCoils
    if (state.dataWaterCoils->GetWaterCoilsInputFlag) {
        GetWaterCoilInput(state);
        state.dataWaterCoils->GetWaterCoilsInputFlag = false;
    }

    NodeNumber = 0;
    WhichCoil = 0;
    if (UtilityRoutines::SameString(CoilType, "Coil:Heating:Water") || UtilityRoutines::SameString(CoilType, "Coil:Cooling:Water:DetailedGeometry") ||
        UtilityRoutines::SameString(CoilType, "Coil:Cooling:Water")) {
        WhichCoil = UtilityRoutines::FindItem(CoilName, state.dataWaterCoils->WaterCoil);
        if (WhichCoil != 0) {
            NodeNumber = state.dataWaterCoils->WaterCoil(WhichCoil).WaterOutletNodeNum;
        }
    } else {
        WhichCoil = 0;
    }

    if (WhichCoil == 0) {
        ShowSevereError(state, "GetCoilWaterOutletNode: Could not find Coil, Type=\"" + CoilType + "\" Name=\"" + CoilName + "\"");
        ErrorsFound = true;
        NodeNumber = 0;
    }

    return NodeNumber;
}

void SetCoilDesFlow(EnergyPlusData &state,
                    std::string const &CoilType, // must match coil types in this module
                    std::string const &CoilName, // must match coil names for the coil type
                    Real64 const CoilDesFlow,    // coil volumetric air flow rate [m3/s]
                    bool &ErrorsFound            // set to true if problem
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   May 2009
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This routine is designed to set the design air volume flow rate in the
    // water coil data structure. Some of the coil types do not have this datum as
    // an input parameter and it is needed for calculating capacity for output reporting.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int WhichCoil; // index to coil

    if (state.dataWaterCoils->GetWaterCoilsInputFlag) { // First time subroutine has been entered
        GetWaterCoilInput(state);
        state.dataWaterCoils->GetWaterCoilsInputFlag = false;
    }

    if (UtilityRoutines::SameString(CoilType, "Coil:Heating:Water") || UtilityRoutines::SameString(CoilType, "Coil:Cooling:Water:DetailedGeometry") ||
        UtilityRoutines::SameString(CoilType, "Coil:Cooling:Water")) {
        WhichCoil = UtilityRoutines::FindItem(CoilName, state.dataWaterCoils->WaterCoil);
        if (WhichCoil != 0) {
            if (state.dataWaterCoils->WaterCoil(WhichCoil).DesAirVolFlowRate <= 0.0) {
                state.dataWaterCoils->WaterCoil(WhichCoil).DesAirVolFlowRate = CoilDesFlow;
            } else {
                // WaterCoil(WhichCoil).DesAirVolFlowRate = CoilDesFlow;
            }
        } else {
            ShowSevereError(state, "GetCoilMaxWaterFlowRate: Could not find Coil, Type=\"" + CoilType + "\" Name=\"" + CoilName + "\"");
            ErrorsFound = true;
        }
    }
}

Real64 GetWaterCoilDesAirFlow(EnergyPlusData &state,
                              std::string const &CoilType, // must match coil types in this module
                              std::string const &CoilName, // must match coil names for the coil type
                              bool &ErrorsFound            // set to true if problem
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   May 2009
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This routine is designed to set the design air volume flow rate in the
    // water coil data structure. Some of the coil types do not have this datum as
    // an input parameter and it is needed for calculating capacity for output reporting.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int WhichCoil; // index to coil
    Real64 CoilDesAirFlow;

    CoilDesAirFlow = 0.0;

    if (state.dataWaterCoils->GetWaterCoilsInputFlag) { // First time subroutine has been entered
        GetWaterCoilInput(state);
        state.dataWaterCoils->GetWaterCoilsInputFlag = false;
    }

    if (UtilityRoutines::SameString(CoilType, "Coil:Cooling:Water")) {
        WhichCoil = UtilityRoutines::FindItem(CoilName, state.dataWaterCoils->WaterCoil);
        if (WhichCoil != 0) {
            CoilDesAirFlow = state.dataWaterCoils->WaterCoil(WhichCoil).DesAirVolFlowRate;
        } else {
            ShowSevereError(state, "GetWaterCoilDesAirFlowRate: Could not find Coil, Type=\"" + CoilType + "\" Name=\"" + CoilName + "\"");
            ErrorsFound = true;
        }
    } else {
        ShowSevereError(state, "GetWaterCoilDesAirFlowRate: Funciton not valid for Coil, Type=\"" + CoilType + "\" Name=\"" + CoilName + "\"");
        ErrorsFound = true;
    }

    return CoilDesAirFlow;
}

void CheckActuatorNode(EnergyPlusData &state,
                       int const ActuatorNodeNum, // input actuator node number
                       int &iNodeType,            // Cooling or Heating or 0
                       bool &NodeNotFound         // true if matching water inlet node not found
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   January 2009
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This subroutine checks that the input actuator node number is matched by
    // the water inlet node number of some water coil

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    int WhichCoil;
    int CoilNum;

    // Obtains and Allocates DXCoils
    if (state.dataWaterCoils->GetWaterCoilsInputFlag) {
        GetWaterCoilInput(state);
        state.dataWaterCoils->GetWaterCoilsInputFlag = false;
    }

    WhichCoil = 0;
    iNodeType = 0;
    NodeNotFound = true;
    for (CoilNum = 1; CoilNum <= state.dataWaterCoils->NumWaterCoils; ++CoilNum) {
        if (state.dataWaterCoils->WaterCoil(CoilNum).WaterInletNodeNum == ActuatorNodeNum) {
            WhichCoil = CoilNum;
            iNodeType = state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType;
            NodeNotFound = false;
        }
    }
}

void CheckForSensorAndSetPointNode(EnergyPlusData &state,
                                   int const SensorNodeNum,                     // controller sensor node number
                                   HVACControllers::iCtrl const &ControlledVar, // controlled variable type
                                   bool &NodeNotFound                           // true if matching air outlet node not found
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Bereket Nigusse
    //       DATE WRITTEN   March 2013
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine checks that the sensor node number matches the air outlet node number
    // of some water coils

    // Using/Aliasing
    using EMSManager::CheckIfNodeSetPointManagedByEMS;
    using SetPointManager::iCtrlVarType;
    using SetPointManager::NodeHasSPMCtrlVarType;

    // SUBROUTINE PARAMETER DEFINITIONS:
    static constexpr std::string_view RoutineName("CheckForSensorAndSetpointNode: ");

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int WhichCoil;             // water coil index
    int CoilNum;               // counter
    std::string WaterCoilType; // water coil type
    bool EMSSetPointErrorFlag; // flag true is EMS is used to set node setpoints

    // Obtains and Allocates DXCoils
    if (state.dataWaterCoils->GetWaterCoilsInputFlag) {
        GetWaterCoilInput(state);
        state.dataWaterCoils->GetWaterCoilsInputFlag = false;
    }

    WhichCoil = 0;
    NodeNotFound = true;

    for (CoilNum = 1; CoilNum <= state.dataWaterCoils->NumWaterCoils; ++CoilNum) {
        if (SensorNodeNum != state.dataWaterCoils->WaterCoil(CoilNum).AirOutletNodeNum) continue;
        NodeNotFound = false;
        WhichCoil = CoilNum;
        break;
    }
    // now if the sensor node is on the water coil air outlet node then check that
    // a setpoint is also specified on the water coil outlet node
    if (!NodeNotFound) {
        if (WhichCoil > 0) {
            if (state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType == DataPlant::TypeOf_CoilWaterDetailedFlatCooling) {
                WaterCoilType = "Coil:Cooling:Water:DetailedGeometry";
            } else if (state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType == DataPlant::TypeOf_CoilWaterCooling) {
                WaterCoilType = "Coil:Cooling:Water";
            } else if (state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType == DataPlant::TypeOf_CoilWaterSimpleHeating) {
                WaterCoilType = "Coil:Heating:Water";
            }
            EMSSetPointErrorFlag = false;
            {
                auto const SELECT_CASE_var(ControlledVar);
                if (SELECT_CASE_var == HVACControllers::iCtrl::Temperature) {
                    CheckIfNodeSetPointManagedByEMS(state, SensorNodeNum, EMSManager::SPControlType::iTemperatureSetPoint, EMSSetPointErrorFlag);
                    state.dataLoopNodes->NodeSetpointCheck(SensorNodeNum).needsSetpointChecking = false;
                    if (EMSSetPointErrorFlag) {
                        if (!NodeHasSPMCtrlVarType(state, SensorNodeNum, iCtrlVarType::Temp)) {
                            ShowWarningError(state, std::string{RoutineName} + WaterCoilType + "=\"" + state.dataWaterCoils->WaterCoil(WhichCoil).Name + "\". ");
                            ShowContinueError(state, " ..Temperature setpoint not found on coil air outlet node.");
                            ShowContinueError(state,
                                              " ..The setpoint may have been placed on a node downstream of the coil or on an airloop outlet node.");
                            ShowContinueError(state, " ..Specify the setpoint and the sensor on the coil air outlet node when possible.");
                        }
                    }
                } else if (SELECT_CASE_var == HVACControllers::iCtrl::HumidityRatio) {
                    CheckIfNodeSetPointManagedByEMS(state, SensorNodeNum, EMSManager::SPControlType::iHumidityRatioMaxSetPoint, EMSSetPointErrorFlag);
                    state.dataLoopNodes->NodeSetpointCheck(SensorNodeNum).needsSetpointChecking = false;
                    if (EMSSetPointErrorFlag) {
                        if (!NodeHasSPMCtrlVarType(state, SensorNodeNum, iCtrlVarType::MaxHumRat)) {
                            ShowWarningError(state, std::string{RoutineName} + WaterCoilType + "=\"" + state.dataWaterCoils->WaterCoil(WhichCoil).Name + "\". ");
                            ShowContinueError(state, " ..Humidity ratio setpoint not found on coil air outlet node.");
                            ShowContinueError(state,
                                              " ..The setpoint may have been placed on a node downstream of the coil or on an airloop outlet node.");
                            ShowContinueError(state, " ..Specify the setpoint and the sensor on the coil air outlet node when possible.");
                        }
                    }
                } else if (SELECT_CASE_var == HVACControllers::iCtrl::TemperatureAndHumidityRatio) {
                    CheckIfNodeSetPointManagedByEMS(state, SensorNodeNum, EMSManager::SPControlType::iTemperatureSetPoint, EMSSetPointErrorFlag);
                    state.dataLoopNodes->NodeSetpointCheck(SensorNodeNum).needsSetpointChecking = false;
                    if (EMSSetPointErrorFlag) {
                        if (!NodeHasSPMCtrlVarType(state, SensorNodeNum, iCtrlVarType::Temp)) {
                            ShowWarningError(state, std::string{RoutineName} + WaterCoilType + "=\"" + state.dataWaterCoils->WaterCoil(WhichCoil).Name + "\". ");
                            ShowContinueError(state, " ..Temperature setpoint not found on coil air outlet node.");
                            ShowContinueError(state,
                                              " ..The setpoint may have been placed on a node downstream of the coil or on an airloop outlet node.");
                            ShowContinueError(state, " ..Specify the setpoint and the sensor on the coil air outlet node when possible.");
                        }
                    }
                    EMSSetPointErrorFlag = false;
                    CheckIfNodeSetPointManagedByEMS(state, SensorNodeNum, EMSManager::SPControlType::iHumidityRatioMaxSetPoint, EMSSetPointErrorFlag);
                    state.dataLoopNodes->NodeSetpointCheck(SensorNodeNum).needsSetpointChecking = false;
                    if (EMSSetPointErrorFlag) {
                        if (!NodeHasSPMCtrlVarType(state, SensorNodeNum, iCtrlVarType::MaxHumRat)) {
                            ShowWarningError(state, std::string{RoutineName} + WaterCoilType + "=\"" + state.dataWaterCoils->WaterCoil(WhichCoil).Name + "\". ");
                            ShowContinueError(state, " ..Humidity ratio setpoint not found on coil air outlet node.");
                            ShowContinueError(state,
                                              " ..The setpoint may have been placed on a node downstream of the coil or on an airloop outlet node.");
                            ShowContinueError(state, " ..Specify the setpoint and the sensor on the coil air outlet node when possible.");
                        }
                    }
                }
            }
        }
    }
}

Real64 TdbFnHRhPb(EnergyPlusData &state,
                  Real64 const H,  // specific enthalpy {J/kg}
                  Real64 const RH, // relative humidity value (0.0-1.0)
                  Real64 const PB  // barometric pressure {Pascals}
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   April 1, 2009
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // Given the specific enthalpy, relative humidity, and the
    // barometric pressure, the function returns the dry bulb temperature.

    // METHODOLOGY EMPLOYED:
    // Inverts PsyHFnTdbRhPb

    // REFERENCES:
    // none

    // Using/Aliasing

    using General::SolveRoot;

    // Return value
    Real64 T; // result=> humidity ratio

    // Locals
    // FUNCTION ARGUMENT DEFINITIONS:

    // FUNCTION PARAMETER DEFINITIONS:
    int const MaxIte(500); // Maximum number of iterations
    Real64 const Acc(1.0); // Accuracy of result

    // INTERFACE BLOCK SPECIFICATIONS
    // na

    // DERIVED TYPE DEFINITIONS
    // na

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    int SolFla;             // Flag of solver
    Real64 T0;              // lower bound for Tprov [C]
    Real64 T1;              // upper bound for Tprov [C]
    Real64 Tprov(0.0);      // provisional value of drybulb temperature [C]
    Array1D<Real64> Par(3); // Par(1) = desired enthaply H [J/kg]
                            // Par(2) = desired relative humidity (0.0 - 1.0)
                            // Par(3) = barometric pressure [N/m2 (Pascals)]

    T0 = 1.0;
    T1 = 50.0;
    Par(1) = H;
    Par(2) = RH;
    Par(3) = PB;
    General::SolveRoot(state, Acc, MaxIte, SolFla, Tprov, EnthalpyResidual, T0, T1, Par);
    // if the numerical inversion failed, issue error messages.
    if (SolFla == -1) {
        ShowSevereError(state, "Calculation of drybulb temperature failed in TdbFnHRhPb(H,RH,PB)");
        ShowContinueError(state, "   Iteration limit exceeded");
        ShowContinueError(state, format("   H=[{:.6R}], RH=[{:.4R}], PB=[{:.5R}].", H, RH, PB));
    } else if (SolFla == -2) {
        ShowSevereError(state, "Calculation of drybulb temperature failed in TdbFnHRhPb(H,RH,PB)");
        ShowContinueError(state, "  Bad starting values for Tdb");
        ShowContinueError(state, format("   H=[{:.6R}], RH=[{:.4R}], PB=[{:.5R}].", H, RH, PB));
    }
    if (SolFla < 0) {
        T = 0.0;
    } else {
        T = Tprov;
    }

    return T;
}

Real64 EnthalpyResidual(EnergyPlusData &state,
                        Real64 const Tprov,        // test value of Tdb [C]
                        Array1D<Real64> const &Par // Par(1) = desired enthaply H [J/kg]
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   April 2009
    //       MODIFIED
    //       RE-ENGINEERED

    // PURPOSE OF THIS FUNCTION:
    // Calculates residual function Hdesired - H(Tdb,Rh,Pb)

    // METHODOLOGY EMPLOYED:
    // Calls PsyHFnTdbRhPb

    // REFERENCES:

    // Using/Aliasing
    using Psychrometrics::PsyHFnTdbRhPb;

    // Return value
    Real64 Residuum; // residual to be minimized to zero

    // Argument array dimensioning

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:
    // Par(2) = desired relative humidity (0.0 - 1.0)
    // Par(3) = barometric pressure [N/m2 (Pascals)]

    // FUNCTION PARAMETER DEFINITIONS:
    // na

    // INTERFACE BLOCK SPECIFICATIONS
    // na

    // DERIVED TYPE DEFINITIONS
    // na

    // FUNCTION LOCAL VARIABLE DECLARATIONS:

    Residuum = Par(1) - PsyHFnTdbRhPb(state, Tprov, Par(2), Par(3));

    return Residuum;
}

Real64 EstimateHEXSurfaceArea(EnergyPlusData &state, int const CoilNum) // coil number, [-]
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Bereket A Nigusse, FSEC
    //       DATE WRITTEN   July 2010
    //       MODIFIED
    //       RE-ENGINEERED

    // PURPOSE OF THIS FUNCTION:
    // Splits the UA value of a simple coil:cooling:water heat exchanger model into
    // "A" and U" values.

    // METHODOLOGY EMPLOYED:
    // A typical design U overall heat transfer coefficient is used to split the "UA" into "A"
    // and "U" values. Currently a constant U value calculated for a typical cooling coil is
    // used. The assumptions used to calculate a typical U value are:
    //     (1) tube side water velocity of 2.0 [m/s]
    //     (2) inside to outside total surface area ratio (Ai/Ao) =  0.07 [-]
    //     (3) fins overall efficiency = 0.92 based on aluminum fin, 12 fins per inch, and
    //         fins area to total outside surafce area ratio of about 90%.
    //     (4) air side convection coefficient of 140.0 [W/m2C].  Assumes sensible convection
    //         of 58.0 [W/m2C] and 82.0 [W/m2C] sensible convection equivalent of the mass
    //         transfer coefficient converted using the approximate relation:
    //         hequivalent = hmasstransfer/CpAir.

    // REFERENCES:

    // USE STATEMENTS:

    // Return value

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:

    // FUNCTION PARAMETER DEFINITIONS:
    constexpr Real64 OverallFinEfficiency(0.92); // Assumes aluminum fins, 12 fins per inch, fins
    // area of about 90% of external surface area Ao.

    constexpr Real64 AreaRatio(0.07); // Heat exchanger Inside to Outside surface area ratio
    // design values range from (Ai/Ao) = 0.06 to 0.08

    // Constant value air side heat transfer coefficient is assumed. This coefficient has sensible
    // (58.d0 [W/m2C]) and latent (82.d0 [W/m2C]) heat transfer coefficient components.
    constexpr Real64 hAirTubeOutside(58.0 + 82.0); // Air side heat transfer coefficient [W/m2C]

    // Tube side water convection heat transfer coefficient of the cooling coil is calculated for
    // inside tube diameter of 0.0122m (~0.5 inch nominal diameter) and water velocity 2.0 m/s:
    static Real64 const hWaterTubeInside(1429.0 * std::pow(2.0, 0.8) * std::pow(0.0122, -0.2)); // water (tube) side heat transfer coefficient [W/m2C]

    // Estimate the overall heat transfer coefficient, UOverallHeatTransferCoef in [W/(m2C)].
    // Neglecting tube wall and fouling resistance, the overall U value can be estimated as:
    // 1/UOverallHeatTransferCoef = 1/(hi*AreaRatio) + 1/(ho*OverallFinEfficiency)
    static Real64 const UOverallHeatTransferCoef_inv(
        1.0 / (hWaterTubeInside * AreaRatio) +
        1.0 / (hAirTubeOutside * OverallFinEfficiency)); // Inverse of overall heat transfer coefficient for coil [W/m2C]

    // INTERFACE BLOCK SPECIFICATIONS
    // na

    // DERIVED TYPE DEFINITIONS
    // na

    // FUNCTION LOCAL VARIABLE DECLARATIONS:

    state.dataWaterCoils->WaterCoil(CoilNum).UACoilTotal =
        1.0 / (1.0 / state.dataWaterCoils->WaterCoil(CoilNum).UACoilExternal + 1.0 / state.dataWaterCoils->WaterCoil(CoilNum).UACoilInternal);

    // the heat exchanger surface area is calculated as follows:
    return state.dataWaterCoils->WaterCoil(CoilNum).UACoilTotal * UOverallHeatTransferCoef_inv; // Heat exchanger surface area [m2]
}

int GetWaterCoilIndex(EnergyPlusData &state,
                      std::string const &CoilType, // must match coil types in this module
                      std::string const &CoilName, // must match coil names for the coil type
                      bool &ErrorsFound            // set to true if problem
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         B. Nigusse, FSEC
    //       DATE WRITTEN   Feb 2012
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This function looks up the index for the given coil and returns it.  If incorrect coil
    // type or name is given, ErrorsFound is returned as true and node number is returned
    // as zero.

    // Return value
    int IndexNum; // returned coil index if matched coil

    // Obtains and allocates WaterCoil related parameters from input file
    if (state.dataWaterCoils->GetWaterCoilsInputFlag) {
        GetWaterCoilInput(state);
        state.dataWaterCoils->GetWaterCoilsInputFlag = false;
    }

    IndexNum = 0;
    if (CoilType == "COIL:HEATING:WATER") {
        IndexNum = UtilityRoutines::FindItemInList(CoilName, state.dataWaterCoils->WaterCoil);
    } else if (CoilType == "COIL:COOLING:WATER") {
        IndexNum = UtilityRoutines::FindItemInList(CoilName, state.dataWaterCoils->WaterCoil);
    } else if (CoilType == "COIL:COOLING:WATER:DETAILEDGEOMETRY") {
        IndexNum = UtilityRoutines::FindItemInList(CoilName, state.dataWaterCoils->WaterCoil);
    } else {
        IndexNum = 0;
    }

    if (IndexNum == 0) {
        ShowSevereError(state, "GetWaterCoilIndex: Could not find CoilType=\"" + CoilType + "\" with Name=\"" + CoilName + "\"");
        ErrorsFound = true;
    }

    return IndexNum;
}

Real64 GetWaterCoilCapacity(EnergyPlusData &state,
                            std::string const &CoilType, // must match coil types in this module
                            std::string const &CoilName, // must match coil names for the coil type
                            bool &ErrorsFound            // set to true if problem
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         R. Raustad, FSEC
    //       DATE WRITTEN   Sep 2013
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This function looks up the capacity for the given coil and returns it.  If incorrect coil
    // type or name is given, ErrorsFound is returned as true and capacity is returned
    // as zero.

    // Return value
    Real64 Capacity; // returned coil capacity if matched coil

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    int IndexNum; // index to water coil

    // Obtains and allocates WaterCoil related parameters from input file
    if (state.dataWaterCoils->GetWaterCoilsInputFlag) {
        GetWaterCoilInput(state);
        state.dataWaterCoils->GetWaterCoilsInputFlag = false;
    }

    Capacity = -1.0;

    if (CoilType == "COIL:HEATING:WATER") {
        IndexNum = UtilityRoutines::FindItemInList(CoilName, state.dataWaterCoils->WaterCoil);
        Capacity = state.dataWaterCoils->WaterCoil(IndexNum).DesWaterHeatingCoilRate;
    } else if (CoilType == "COIL:COOLING:WATER") {
        IndexNum = UtilityRoutines::FindItemInList(CoilName, state.dataWaterCoils->WaterCoil);
        Capacity = state.dataWaterCoils->WaterCoil(IndexNum).DesWaterCoolingCoilRate;
    } else if (CoilType == "COIL:COOLING:WATER:DETAILEDGEOMETRY") {
        IndexNum = UtilityRoutines::FindItemInList(CoilName, state.dataWaterCoils->WaterCoil);
        Capacity = state.dataWaterCoils->WaterCoil(IndexNum).DesWaterCoolingCoilRate;
    } else {
        IndexNum = 0;
    }

    if (IndexNum == 0) {
        ShowSevereError(state, "GetWaterCoilCapacity: Could not find CoilType=\"" + CoilType + "\" with Name=\"" + CoilName + "\"");
        ErrorsFound = true;
    }

    return Capacity;
}

void UpdateWaterToAirCoilPlantConnection(EnergyPlusData &state,
                                         int const CoilTypeNum,
                                         std::string const &CoilName,
                                         [[maybe_unused]] int const EquipFlowCtrl, // Flow control mode for the equipment
                                         int const LoopNum,                        // Plant loop index for where called from
                                         int const LoopSide,                       // Plant loop side index for where called from
                                         int &CompIndex,                           // Chiller number pointer
                                         [[maybe_unused]] bool const FirstHVACIteration,
                                         bool &InitLoopEquip // If not zero, calculate the max load for operating conditions
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         B. Griffith
    //       DATE WRITTEN   February 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // update sim routine called from plant

    // Using/Aliasing
    using DataPlant::ccSimPlantEquipTypes;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

    int CoilNum;
    bool DidAnythingChange(false); // set to true if conditions changed
    int InletNodeNum;
    int OutletNodeNum;

    // Find the correct water coil
    if (CompIndex == 0) {
        CoilNum = UtilityRoutines::FindItemInList(CoilName, state.dataWaterCoils->WaterCoil);
        if (CoilNum == 0) {
            ShowFatalError(state, "UpdateWaterToAirCoilPlantConnection: Specified Coil not one of Valid water coils=" + CoilName);
        }
        CompIndex = CoilNum;
    } else {
        CoilNum = CompIndex;
        if (CoilNum > state.dataWaterCoils->NumWaterCoils || CoilNum < 1) {
            ShowFatalError(state,
                           format("UpdateWaterToAirCoilPlantConnection:  Invalid CompIndex passed={}, Number of Coils={}, Entered Coil name={}",
                                  CoilNum,
                                  state.dataWaterCoils->NumWaterCoils,
                                  CoilName));
        }
        if (state.dataGlobal->KickOffSimulation) {
            if (CoilName != state.dataWaterCoils->WaterCoil(CoilNum).Name) {
                ShowFatalError(
                    state,
                    format("UpdateWaterToAirCoilPlantConnection: Invalid CompIndex passed={}, Coil name={}, stored Coil Name for that index={}",
                           CoilNum,
                           CoilName,
                           state.dataWaterCoils->WaterCoil(CoilNum).Name));
            }
            if (CoilTypeNum != state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType) {
                ShowFatalError(
                    state,
                    format("UpdateWaterToAirCoilPlantConnection: Invalid CompIndex passed={}, Coil name={}, stored Coil Name for that index={}",
                           CoilNum,
                           CoilName,
                           ccSimPlantEquipTypes(CoilTypeNum)));
            }
        }
    }

    if (InitLoopEquip) {
        return;
    }

    DidAnythingChange = false;

    InletNodeNum = state.dataWaterCoils->WaterCoil(CoilNum).WaterInletNodeNum;
    OutletNodeNum = state.dataWaterCoils->WaterCoil(CoilNum).WaterOutletNodeNum;

    if (state.dataLoopNodes->Node(InletNodeNum).Temp != state.dataWaterCoils->WaterCoil(CoilNum).InletWaterTemp) DidAnythingChange = true;

    if (state.dataLoopNodes->Node(OutletNodeNum).Temp != state.dataWaterCoils->WaterCoil(CoilNum).OutletWaterTemp) DidAnythingChange = true;

    if (state.dataLoopNodes->Node(InletNodeNum).MassFlowRate != state.dataWaterCoils->WaterCoil(CoilNum).OutletWaterMassFlowRate) {
        DidAnythingChange = true;
        state.dataLoopNodes->Node(OutletNodeNum).MassFlowRate =
            state.dataLoopNodes->Node(InletNodeNum).MassFlowRate; // make sure flows are consistent
    }

    if (state.dataLoopNodes->Node(OutletNodeNum).MassFlowRate != state.dataWaterCoils->WaterCoil(CoilNum).OutletWaterMassFlowRate)
        DidAnythingChange = true;

    if (DidAnythingChange) {
        // set sim flag for this loop
        state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSide).SimLoopSideNeeded = true;
        // set sim flags for air side users of coils

        state.dataHVACGlobal->SimAirLoopsFlag = true;
        state.dataHVACGlobal->SimZoneEquipmentFlag = true;
    } else { // nothing changed so turn off sim flag
        state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSide).SimLoopSideNeeded = false;
    }
}

int GetWaterCoilAvailScheduleIndex(EnergyPlusData &state,
                                   std::string const &CoilType, // must match coil types in this module
                                   std::string const &CoilName, // must match coil names for the coil type
                                   bool &ErrorsFound            // set to true if problem
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Chandan Sharma, FSEC
    //       DATE WRITTEN   February 2013
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This function looks up the given coil and returns the availability schedule index.  If
    // incorrect coil type or name is given, ErrorsFound is returned as true and index is returned
    // as zero.

    // Return value
    int AvailSchIndex; // returned availability schedule of matched coil

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    int WhichCoil;

    // Obtains and Allocates HeatingCoil related parameters from input file
    // Obtains and Allocates DXCoils
    if (state.dataWaterCoils->GetWaterCoilsInputFlag) {
        GetWaterCoilInput(state);
        state.dataWaterCoils->GetWaterCoilsInputFlag = false;
    }

    WhichCoil = 0;
    AvailSchIndex = 0;

    if (UtilityRoutines::SameString(CoilType, "Coil:Heating:Water") || UtilityRoutines::SameString(CoilType, "Coil:Cooling:Water") ||
        UtilityRoutines::SameString(CoilType, "Coil:Cooling:Water:DetailedGeometry")) {
        WhichCoil = UtilityRoutines::FindItem(CoilName, state.dataWaterCoils->WaterCoil);
        if (WhichCoil != 0) {
            AvailSchIndex = state.dataWaterCoils->WaterCoil(WhichCoil).SchedPtr;
        }
    } else {
        WhichCoil = 0;
    }

    if (WhichCoil == 0) {
        ShowSevereError(state, "GetCoilAvailScheduleIndex: Could not find Coil, Type=\"" + CoilType + "\" Name=\"" + CoilName + "\"");
        ErrorsFound = true;
        AvailSchIndex = 0;
    }

    return AvailSchIndex;
}

void SetWaterCoilData(EnergyPlusData &state,
                      int const CoilNum,                       // Number of hot water heating Coil
                      bool &ErrorsFound,                       // Set to true if certain errors found
                      Optional_bool DesiccantRegenerationCoil, // Flag that this coil is used as regeneration air heating coil
                      Optional_int DesiccantDehumIndex         // Index for the desiccant dehum system where this caoil is used
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Bereket Nigusse
    //       DATE WRITTEN   February 2016
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This function sets data to water Heating Coil using the coil index and arguments passed

    // Using/Aliasing

    if (state.dataWaterCoils->GetWaterCoilsInputFlag) {
        GetWaterCoilInput(state);
        state.dataWaterCoils->GetWaterCoilsInputFlag = false;
    }

    if (CoilNum <= 0 || CoilNum > state.dataWaterCoils->NumWaterCoils) {
        ShowSevereError(state,
                        format("SetHeatingCoilData: called with heating coil Number out of range={} should be >0 and <{}",
                               CoilNum,
                               state.dataWaterCoils->NumWaterCoils));
        ErrorsFound = true;
        return;
    }

    if (present(DesiccantRegenerationCoil)) {
        state.dataWaterCoils->WaterCoil(CoilNum).DesiccantRegenerationCoil = DesiccantRegenerationCoil;
    }

    if (present(DesiccantDehumIndex)) {
        state.dataWaterCoils->WaterCoil(CoilNum).DesiccantDehumNum = DesiccantDehumIndex;
    }
}

void EstimateCoilInletWaterTemp(EnergyPlusData &state,
                                int const CoilNum,                // index to heating coil
                                int const FanOpMode,              // fan operating mode
                                Real64 const PartLoadRatio,       // part-load ratio of heating coil
                                Real64 const UAMax,               // maximum UA-Value = design heating capacity
                                Real64 &DesCoilInletWaterTempUsed // estimated coil design inlet water temperature
)
{
    // SUBROUTINE INFORMATION:

    // PURPOSE OF THIS SUBROUTINE:
    // returns estimated coil inlet water temperature given UA value for assumed
    // maximum effectiveness value for heating coil

    // METHODOLOGY EMPLOYED:
    // applies energy balance around the water coil and estimates coil water inlet temperature
    // assuming coil effectiveness of 0.8

    // REFERENCES:
    // na

    // Using/Aliasing

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:

    // SUBROUTINE PARAMETER DEFINITIONS:
    static constexpr std::string_view RoutineName("EstimateCoilInletWaterTemp");
    constexpr Real64 EffectivenessMaxAssumed(0.80);

    // INTERFACE BLOCK SPECIFICATIONS
    // na

    // DERIVED TYPE DEFINITIONS
    // na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 WaterMassFlowRate;
    Real64 AirMassFlow;
    Real64 TempAirIn;
    Real64 TempAirOut; // [C]
    Real64 Win;
    Real64 TempWaterIn;
    Real64 UA;
    Real64 CapacitanceAir;
    Real64 CapacitanceWater;
    Real64 CapacitanceMin;
    Real64 CapacitanceMax;
    Real64 NTU;
    Real64 ETA;
    Real64 A;
    Real64 CapRatio;
    Real64 E1;
    Real64 E2;
    Real64 Effec;
    Real64 Cp;

    UA = UAMax;
    DesCoilInletWaterTempUsed = DesCoilHWInletTempMin;
    TempAirIn = state.dataWaterCoils->WaterCoil(CoilNum).InletAirTemp;
    Win = state.dataWaterCoils->WaterCoil(CoilNum).InletAirHumRat;
    TempWaterIn = state.dataWaterCoils->WaterCoil(CoilNum).InletWaterTemp;
    // adjust mass flow rates for cycling fan cycling coil operation
    if (FanOpMode == CycFanCycCoil) {
        if (PartLoadRatio > 0.0) {
            AirMassFlow = state.dataWaterCoils->WaterCoil(CoilNum).InletAirMassFlowRate / PartLoadRatio;
            WaterMassFlowRate = min(state.dataWaterCoils->WaterCoil(CoilNum).InletWaterMassFlowRate / PartLoadRatio,
                                    state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterMassFlowRate);
        } else {
            AirMassFlow = 0.0;
            WaterMassFlowRate = 0.0;
            return;
        }
    } else {
        AirMassFlow = state.dataWaterCoils->WaterCoil(CoilNum).InletAirMassFlowRate;
        WaterMassFlowRate = state.dataWaterCoils->WaterCoil(CoilNum).InletWaterMassFlowRate;
    }
    if (WaterMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance) { // if the coil is operating
        CapacitanceAir = PsyCpAirFnW(Win) * AirMassFlow;
        Cp = GetSpecificHeatGlycol(state,
                                   state.dataPlnt->PlantLoop(state.dataWaterCoils->WaterCoil(CoilNum).WaterLoopNum).FluidName,
                                   TempWaterIn,
                                   state.dataPlnt->PlantLoop(state.dataWaterCoils->WaterCoil(CoilNum).WaterLoopNum).FluidIndex,
                                   RoutineName);
        CapacitanceWater = Cp * WaterMassFlowRate;
        CapacitanceMin = min(CapacitanceAir, CapacitanceWater);
        CapacitanceMax = max(CapacitanceAir, CapacitanceWater);
    } else {
        CapacitanceAir = 0.0;
        CapacitanceWater = 0.0;
        return;
    }
    // calculate DesCoilInletWaterTempUsed
    if (((CapacitanceAir > 0.0) && (CapacitanceWater > 0.0))) {

        if (UA <= 0.0) {
            ShowWarningError(state, "UA is zero for COIL:Heating:Water " + state.dataWaterCoils->WaterCoil(CoilNum).Name);
            return;
        }
        NTU = UA / CapacitanceMin;
        ETA = std::pow(NTU, 0.22);
        CapRatio = CapacitanceMin / CapacitanceMax;
        A = CapRatio * NTU / ETA;

        if (A > 20.0) {
            A = ETA * 1.0 / CapRatio;
        } else {
            E1 = std::exp(-A);
            A = ETA * (1.0 - E1) / CapRatio;
        }

        if (A > 20.0) {
            Effec = 1.0;
        } else {
            E2 = std::exp(-A);
            Effec = 1.0 - E2;
        }
        TempAirOut = TempAirIn + Effec * CapacitanceMin * (TempWaterIn - TempAirIn) / CapacitanceAir;
        // this formulation assumes coil effectiveness of 0.80 to increase the estimated coil water inlet temperatures
        DesCoilInletWaterTempUsed = CapacitanceAir * (TempAirOut - TempAirIn) / (CapacitanceMin * EffectivenessMaxAssumed) + TempAirIn;
        // water coil should not be sized at coil water inlet temperature lower than 46.0C (for convergence problem in Regulafalsi)
        DesCoilInletWaterTempUsed = max(DesCoilInletWaterTempUsed, DesCoilHWInletTempMin);
    }
}

// End of Coil Utility subroutines
// *****************************************************************************

} // namespace EnergyPlus::WaterCoils
