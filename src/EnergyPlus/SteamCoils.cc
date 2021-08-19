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
#include <EnergyPlus/Autosizing/HeatingAirFlowSizing.hh>
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataContaminantBalance.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/FaultsManager.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/HVACFan.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ReportCoilSelection.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SteamCoils.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

namespace SteamCoils {

    // Module containing the SteamCoil simulation routines

    // MODULE INFORMATION:
    //   AUTHOR         Rahul Chillar
    //   DATE WRITTEN   Jan 2005
    //   MODIFIED       na
    //   RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // To encapsulate the data and algorithms required to
    // manage the SteamCoil System Component.

    using namespace DataLoopNode;
    using namespace DataHVACGlobals;
    using namespace Psychrometrics;
    using namespace FluidProperties;
    using DataPlant::TypeOf_CoilSteamAirHeating;
    using PlantUtilities::MyPlantSizingIndex;
    using PlantUtilities::ScanPlantLoopsForObject;
    using namespace ScheduleManager;

    static constexpr std::string_view fluidNameSteam("STEAM");

    void SimulateSteamCoilComponents(EnergyPlusData &state,
                                     std::string_view CompName,
                                     bool const FirstHVACIteration,
                                     int &CompIndex,
                                     Optional<Real64 const> QCoilReq, // coil load to be met
                                     Optional<Real64> QCoilActual,    // coil load actually delivered returned to calling component
                                     Optional_int_const FanOpMode,
                                     Optional<Real64 const> PartLoadRatio)
    {

        // SUBROUTINE INFORMATION:
        //   AUTHOR         Rahul Chillar
        //   DATE WRITTEN   Jan 2005
        //   MODIFIED       na
        //   RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine manages SteamCoil component simulation.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 QCoilActualTemp; // coil load actually delivered returned to calling component
        int CoilNum;            // The SteamCoil that you are currently loading input into
        int OpMode;             // fan operating mode
        Real64 PartLoadFrac;    // part-load fraction of heating coil
        Real64 QCoilReqLocal;   // local required heating load optional

        // Obtains and Allocates SteamCoil related parameters from input file
        if (state.dataSteamCoils->GetSteamCoilsInputFlag) { // First time subroutine has been entered
            GetSteamCoilInput(state);
            state.dataSteamCoils->GetSteamCoilsInputFlag = false;
        }

        // Find the correct SteamCoilNumber with the Coil Name
        if (CompIndex == 0) {
            CoilNum = UtilityRoutines::FindItemInList(CompName, state.dataSteamCoils->SteamCoil);
            if (CoilNum == 0) {
                ShowFatalError(state, "SimulateSteamCoilComponents: Coil not found=" + std::string{CompName});
            }
            CompIndex = CoilNum;
        } else {
            CoilNum = CompIndex;
            if (CoilNum > state.dataSteamCoils->NumSteamCoils || CoilNum < 1) {
                ShowFatalError(state,
                               format("SimulateSteamCoilComponents: Invalid CompIndex passed={}, Number of Steam Coils={}, Coil name={}",
                                      CoilNum,
                                      state.dataSteamCoils->NumSteamCoils,
                                      CompName));
            }
            if (state.dataSteamCoils->CheckEquipName(CoilNum)) {
                if (CompName != state.dataSteamCoils->SteamCoil(CoilNum).Name) {
                    ShowFatalError(
                        state,
                        format("SimulateSteamCoilComponents: Invalid CompIndex passed={}, Coil name={}, stored Coil Name for that index={}",
                               CoilNum,
                               CompName,
                               state.dataSteamCoils->SteamCoil(CoilNum).Name));
                }
                state.dataSteamCoils->CheckEquipName(CoilNum) = false;
            }
        }

        // With the correct CoilNum Initialize
        InitSteamCoil(state, CoilNum, FirstHVACIteration); // Initialize all SteamCoil related parameters

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
        if (present(QCoilReq)) {
            QCoilReqLocal = QCoilReq;
        } else {
            QCoilReqLocal = 0.0;
        }

        if (state.dataSteamCoils->SteamCoil(CoilNum).SteamCoilType_Num == state.dataSteamCoils->SteamCoil_AirHeating) {
            CalcSteamAirCoil(
                state, CoilNum, QCoilReqLocal, QCoilActualTemp, OpMode, PartLoadFrac); // Autodesk:OPTIONAL QCoilReq used without PRESENT check
            if (present(QCoilActual)) QCoilActual = QCoilActualTemp;
        }

        // Update the current SteamCoil to the outlet nodes
        UpdateSteamCoil(state, CoilNum);

        // Report the current SteamCoil
        ReportSteamCoil(state, CoilNum);
    }

    // Get Input Section of the Module

    void GetSteamCoilInput(EnergyPlusData &state)
    {
        // SUBROUTINE INFORMATION:
        //   AUTHOR         Rahul Chillar
        //   DATE WRITTEN   Jan 2005
        //   MODIFIED       na
        //   RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Obtains input data for coils and stores it in coil data structures

        // METHODOLOGY EMPLOYED:
        // Uses "Get" routines to read in data.

        // Using/Aliasing
        using BranchNodeConnections::TestCompSet;
        using FluidProperties::FindRefrigerant;
        using GlobalNames::VerifyUniqueCoilName;
        using NodeInputManager::GetOnlySingleNode;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static constexpr std::string_view RoutineName("GetSteamCoilInput: "); // include trailing blank space

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int CoilNum; // The SteamCoil that you are currently loading input into
        int NumStmHeat;
        int StmHeatNum;
        int NumAlphas;
        int NumNums;
        int IOStat;
        bool ErrorsFound(false);         // If errors detected in input
        std::string CurrentModuleObject; // for ease in getting objects
        Array1D_string AlphArray;        // Alpha input items for object
        Array1D_string cAlphaFields;     // Alpha field names
        Array1D_string cNumericFields;   // Numeric field names
        Array1D<Real64> NumArray;        // Numeric input items for object
        Array1D_bool lAlphaBlanks;       // Logical array, alpha field input BLANK = .TRUE.
        Array1D_bool lNumericBlanks;     // Logical array, numeric field input BLANK = .TRUE.
        int TotalArgs(0);                // Total number of alpha and numeric arguments (max) for a
                                         //  certain object in the input file

        CurrentModuleObject = "Coil:Heating:Steam";
        NumStmHeat = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
        state.dataSteamCoils->NumSteamCoils = NumStmHeat;
        if (state.dataSteamCoils->NumSteamCoils > 0) {
            state.dataSteamCoils->SteamCoil.allocate(state.dataSteamCoils->NumSteamCoils);
            state.dataSteamCoils->CheckEquipName.dimension(state.dataSteamCoils->NumSteamCoils, true);
        }

        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, CurrentModuleObject, TotalArgs, NumAlphas, NumNums);
        AlphArray.allocate(NumAlphas);
        cAlphaFields.allocate(NumAlphas);
        cNumericFields.allocate(NumNums);
        NumArray.dimension(NumNums, 0.0);
        lAlphaBlanks.dimension(NumAlphas, true);
        lNumericBlanks.dimension(NumNums, true);

        // Get the data for steam heating coils
        for (StmHeatNum = 1; StmHeatNum <= NumStmHeat; ++StmHeatNum) {

            CoilNum = StmHeatNum;

            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     StmHeatNum,
                                                                     AlphArray,
                                                                     NumAlphas,
                                                                     NumArray,
                                                                     NumNums,
                                                                     IOStat,
                                                                     lNumericBlanks,
                                                                     lAlphaBlanks,
                                                                     cAlphaFields,
                                                                     cNumericFields);
            UtilityRoutines::IsNameEmpty(state, AlphArray(1), CurrentModuleObject, ErrorsFound);

            // ErrorsFound will be set to True if problem was found, left untouched otherwise
            VerifyUniqueCoilName(state, CurrentModuleObject, AlphArray(1), ErrorsFound, CurrentModuleObject + " Name");

            state.dataSteamCoils->SteamCoil(CoilNum).Name = AlphArray(1);
            state.dataSteamCoils->SteamCoil(CoilNum).Schedule = AlphArray(2);
            if (lAlphaBlanks(2)) {
                state.dataSteamCoils->SteamCoil(CoilNum).SchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
            } else {
                state.dataSteamCoils->SteamCoil(CoilNum).SchedPtr = GetScheduleIndex(state, AlphArray(2));
                if (state.dataSteamCoils->SteamCoil(CoilNum).SchedPtr == 0) {
                    ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + AlphArray(1) + "\", invalid data.");
                    ShowContinueError(state, cAlphaFields(2) + " not found=" + AlphArray(2));
                    ErrorsFound = true;
                }
            }

            state.dataSteamCoils->SteamCoil(CoilNum).SteamCoilTypeA = "Heating";
            state.dataSteamCoils->SteamCoil(CoilNum).SteamCoilType_Num = state.dataSteamCoils->SteamCoil_AirHeating;
            state.dataSteamCoils->SteamCoil(CoilNum).Coil_PlantTypeNum = TypeOf_CoilSteamAirHeating;
            state.dataSteamCoils->SteamCoil(CoilNum).MaxSteamVolFlowRate = NumArray(1);
            state.dataSteamCoils->SteamCoil(CoilNum).DegOfSubcooling = NumArray(2);
            state.dataSteamCoils->SteamCoil(CoilNum).LoopSubcoolReturn = NumArray(3);

            state.dataSteamCoils->SteamCoil(CoilNum).SteamInletNodeNum = GetOnlySingleNode(state,
                                                                                           AlphArray(3),
                                                                                           ErrorsFound,
                                                                                           CurrentModuleObject,
                                                                                           AlphArray(1),
                                                                                           DataLoopNode::NodeFluidType::Steam,
                                                                                           DataLoopNode::NodeConnectionType::Inlet,
                                                                                           NodeInputManager::compFluidStream::Secondary,
                                                                                           ObjectIsNotParent);
            state.dataSteamCoils->SteamCoil(CoilNum).SteamOutletNodeNum = GetOnlySingleNode(state,
                                                                                            AlphArray(4),
                                                                                            ErrorsFound,
                                                                                            CurrentModuleObject,
                                                                                            AlphArray(1),
                                                                                            DataLoopNode::NodeFluidType::Steam,
                                                                                            DataLoopNode::NodeConnectionType::Outlet,
                                                                                            NodeInputManager::compFluidStream::Secondary,
                                                                                            ObjectIsNotParent);
            state.dataSteamCoils->SteamCoil(CoilNum).AirInletNodeNum = GetOnlySingleNode(state,
                                                                                         AlphArray(5),
                                                                                         ErrorsFound,
                                                                                         CurrentModuleObject,
                                                                                         AlphArray(1),
                                                                                         DataLoopNode::NodeFluidType::Air,
                                                                                         DataLoopNode::NodeConnectionType::Inlet,
                                                                                         NodeInputManager::compFluidStream::Primary,
                                                                                         ObjectIsNotParent);
            state.dataSteamCoils->SteamCoil(CoilNum).AirOutletNodeNum = GetOnlySingleNode(state,
                                                                                          AlphArray(6),
                                                                                          ErrorsFound,
                                                                                          CurrentModuleObject,
                                                                                          AlphArray(1),
                                                                                          DataLoopNode::NodeFluidType::Air,
                                                                                          DataLoopNode::NodeConnectionType::Outlet,
                                                                                          NodeInputManager::compFluidStream::Primary,
                                                                                          ObjectIsNotParent);

            {
                auto const SELECT_CASE_var(UtilityRoutines::MakeUPPERCase(AlphArray(7)));
                // TEMPERATURE SETPOINT CONTROL or ZONE LOAD CONTROLLED Coils
                if (SELECT_CASE_var == "TEMPERATURESETPOINTCONTROL") {
                    state.dataSteamCoils->SteamCoil(CoilNum).TypeOfCoil = state.dataSteamCoils->TemperatureSetPointControl;
                    state.dataSteamCoils->SteamCoil(CoilNum).TempSetPointNodeNum = GetOnlySingleNode(state,
                                                                                                     AlphArray(8),
                                                                                                     ErrorsFound,
                                                                                                     CurrentModuleObject,
                                                                                                     AlphArray(1),
                                                                                                     DataLoopNode::NodeFluidType::Air,
                                                                                                     DataLoopNode::NodeConnectionType::Sensor,
                                                                                                     NodeInputManager::compFluidStream::Primary,
                                                                                                     ObjectIsNotParent);
                    if (state.dataSteamCoils->SteamCoil(CoilNum).TempSetPointNodeNum == 0) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + cAlphaFields(8) + " not found for " + CurrentModuleObject + " = " + AlphArray(1));
                        ShowContinueError(state, "..required for Temperature Setpoint Controlled Coils.");
                        ErrorsFound = true;
                    }

                } else if (SELECT_CASE_var == "ZONELOADCONTROL") {
                    state.dataSteamCoils->SteamCoil(CoilNum).TypeOfCoil = state.dataSteamCoils->ZoneLoadControl;

                    if (!lAlphaBlanks(8)) {
                        ShowWarningError(state, std::string{RoutineName} + "ZoneLoad Controlled Coil, so " + cAlphaFields(8) + " not needed");
                        ShowContinueError(state, "for " + CurrentModuleObject + " = " + AlphArray(1));
                        state.dataSteamCoils->SteamCoil(CoilNum).TempSetPointNodeNum = 0;
                    }

                } else {
                    ShowSevereError(state,
                                    std::string{RoutineName} + "Invalid " + cAlphaFields(7) + " [" + AlphArray(7) + "] specified for " +
                                        CurrentModuleObject + " = " + AlphArray(1));
                    ErrorsFound = true;
                }
            }

            TestCompSet(state, CurrentModuleObject, AlphArray(1), AlphArray(3), AlphArray(4), "Steam Nodes");
            TestCompSet(state, CurrentModuleObject, AlphArray(1), AlphArray(5), AlphArray(6), "Air Nodes");

            if (state.dataSteamCoils->SteamIndex == 0 && CoilNum == 1) {
                state.dataSteamCoils->SteamIndex = FindRefrigerant(state, "Steam");
                if (state.dataSteamCoils->SteamIndex == 0) {
                    ShowSevereError(state, std::string{RoutineName} + "Steam Properties for " + AlphArray(1) + " not found.");
                    ShowContinueError(state, "Steam Fluid Properties should have been included in the input file.");
                    ErrorsFound = true;
                }
            }

            state.dataSteamCoils->SteamCoil(CoilNum).FluidIndex = state.dataSteamCoils->SteamIndex;
        }

        for (CoilNum = 1; CoilNum <= NumStmHeat; ++CoilNum) {

            // Setup the Simple Heating Coil reporting variables
            // CurrentModuleObject = "Coil:Heating:Steam"
            SetupOutputVariable(state,
                                "Heating Coil Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataSteamCoils->SteamCoil(CoilNum).TotSteamHeatingCoilEnergy,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                state.dataSteamCoils->SteamCoil(CoilNum).Name,
                                _,
                                "ENERGYTRANSFER",
                                "HEATINGCOILS",
                                _,
                                "System");
            SetupOutputVariable(state,
                                "Heating Coil Heating Rate",
                                OutputProcessor::Unit::W,
                                state.dataSteamCoils->SteamCoil(CoilNum).TotSteamHeatingCoilRate,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataSteamCoils->SteamCoil(CoilNum).Name);
            SetupOutputVariable(state,
                                "Heating Coil Steam Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                state.dataSteamCoils->SteamCoil(CoilNum).OutletSteamMassFlowRate,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataSteamCoils->SteamCoil(CoilNum).Name);
            SetupOutputVariable(state,
                                "Heating Coil Steam Inlet Temperature",
                                OutputProcessor::Unit::C,
                                state.dataSteamCoils->SteamCoil(CoilNum).InletSteamTemp,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataSteamCoils->SteamCoil(CoilNum).Name);
            SetupOutputVariable(state,
                                "Heating Coil Steam Outlet Temperature",
                                OutputProcessor::Unit::C,
                                state.dataSteamCoils->SteamCoil(CoilNum).OutletSteamTemp,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataSteamCoils->SteamCoil(CoilNum).Name);
            SetupOutputVariable(state,
                                "Heating Coil Steam Trap Loss Rate",
                                OutputProcessor::Unit::W,
                                state.dataSteamCoils->SteamCoil(CoilNum).LoopLoss,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataSteamCoils->SteamCoil(CoilNum).Name);
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

    // End of Get Input subroutines for the HB Module

    // Beginning Initialization Section of the Module

    void InitSteamCoil(EnergyPlusData &state, int const CoilNum, bool const FirstHVACIteration)
    {
        // SUBROUTINE INFORMATION:
        //   AUTHOR         Rahul Chillar
        //   DATE WRITTEN   Jan 2005
        //   MODIFIED       na
        //   RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for initializations of the SteamCoil Components.

        // METHODOLOGY EMPLOYED:
        // Uses the status flags to trigger initializations.

        // REFERENCES:
        // na

        // Using/Aliasing
        using FluidProperties::GetSatDensityRefrig;
        using FluidProperties::GetSatEnthalpyRefrig;
        using PlantUtilities::InitComponentNodes;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        static constexpr std::string_view RoutineName("InitSteamCoil");

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int AirInletNode;
        int SteamInletNode;
        int ControlNode;
        int AirOutletNode;
        Real64 SteamDensity;
        Real64 StartEnthSteam;
        auto &MyEnvrnFlag = state.dataSteamCoils->MyEnvrnFlag;
        auto &MyPlantScanFlag = state.dataSteamCoils->MyPlantScanFlag;
        bool errFlag;

        if (state.dataSteamCoils->MyOneTimeFlag) {
            // initialize the environment and sizing flags
            MyEnvrnFlag.allocate(state.dataSteamCoils->NumSteamCoils);
            state.dataSteamCoils->MySizeFlag.allocate(state.dataSteamCoils->NumSteamCoils);
            state.dataSteamCoils->CoilWarningOnceFlag.allocate(state.dataSteamCoils->NumSteamCoils);
            MyPlantScanFlag.allocate(state.dataSteamCoils->NumSteamCoils);
            MyEnvrnFlag = true;
            state.dataSteamCoils->MySizeFlag = true;
            state.dataSteamCoils->CoilWarningOnceFlag = true;
            MyPlantScanFlag = true;
            state.dataSteamCoils->MyOneTimeFlag = false;
        }

        if (MyPlantScanFlag(CoilNum) && allocated(state.dataPlnt->PlantLoop)) {
            errFlag = false;
            ScanPlantLoopsForObject(state,
                                    state.dataSteamCoils->SteamCoil(CoilNum).Name,
                                    state.dataSteamCoils->SteamCoil(CoilNum).Coil_PlantTypeNum,
                                    state.dataSteamCoils->SteamCoil(CoilNum).LoopNum,
                                    state.dataSteamCoils->SteamCoil(CoilNum).LoopSide,
                                    state.dataSteamCoils->SteamCoil(CoilNum).BranchNum,
                                    state.dataSteamCoils->SteamCoil(CoilNum).CompNum,
                                    errFlag,
                                    _,
                                    _,
                                    _,
                                    _,
                                    _);
            if (errFlag) {
                ShowFatalError(state, "InitSteamCoil: Program terminated for previous conditions.");
            }
            MyPlantScanFlag(CoilNum) = false;
        }

        if (!state.dataGlobal->SysSizingCalc && state.dataSteamCoils->MySizeFlag(CoilNum)) {
            // for each coil, do the sizing once.
            SizeSteamCoil(state, CoilNum);
            state.dataSteamCoils->MySizeFlag(CoilNum) = false;
        }

        // Do the Begin Environment initializations
        if (state.dataGlobal->BeginEnvrnFlag && MyEnvrnFlag(CoilNum)) {
            // Initialize all report variables to a known state at beginning of simulation
            state.dataSteamCoils->SteamCoil(CoilNum).TotSteamHeatingCoilEnergy = 0.0;
            state.dataSteamCoils->SteamCoil(CoilNum).TotSteamCoolingCoilEnergy = 0.0;
            state.dataSteamCoils->SteamCoil(CoilNum).SenSteamCoolingCoilEnergy = 0.0;
            state.dataSteamCoils->SteamCoil(CoilNum).TotSteamHeatingCoilRate = 0.0;
            state.dataSteamCoils->SteamCoil(CoilNum).TotSteamCoolingCoilRate = 0.0;
            state.dataSteamCoils->SteamCoil(CoilNum).SenSteamCoolingCoilRate = 0.0;
            // Initialize other module level variables
            state.dataSteamCoils->SteamCoil(CoilNum).InletAirMassFlowRate = 0.0;
            state.dataSteamCoils->SteamCoil(CoilNum).OutletAirMassFlowRate = 0.0;
            state.dataSteamCoils->SteamCoil(CoilNum).InletAirTemp = 0.0;
            state.dataSteamCoils->SteamCoil(CoilNum).OutletAirTemp = 0.0;
            state.dataSteamCoils->SteamCoil(CoilNum).InletAirHumRat = 0.0;
            state.dataSteamCoils->SteamCoil(CoilNum).OutletAirHumRat = 0.0;
            state.dataSteamCoils->SteamCoil(CoilNum).InletAirEnthalpy = 0.0;
            state.dataSteamCoils->SteamCoil(CoilNum).OutletAirEnthalpy = 0.0;
            state.dataSteamCoils->SteamCoil(CoilNum).TotSteamCoilLoad = 0.0;
            state.dataSteamCoils->SteamCoil(CoilNum).SenSteamCoilLoad = 0.0;
            state.dataSteamCoils->SteamCoil(CoilNum).LoopLoss = 0.0;
            state.dataSteamCoils->SteamCoil(CoilNum).LeavingRelHum = 0.0;
            state.dataSteamCoils->SteamCoil(CoilNum).DesiredOutletTemp = 0.0;
            state.dataSteamCoils->SteamCoil(CoilNum).DesiredOutletHumRat = 0.0;
            state.dataSteamCoils->SteamCoil(CoilNum).InletSteamTemp = 0.0;
            state.dataSteamCoils->SteamCoil(CoilNum).OutletSteamTemp = 0.0;
            state.dataSteamCoils->SteamCoil(CoilNum).InletSteamMassFlowRate = 0.0;
            state.dataSteamCoils->SteamCoil(CoilNum).OutletSteamMassFlowRate = 0.0;
            state.dataSteamCoils->SteamCoil(CoilNum).InletSteamEnthalpy = 0.0;
            state.dataSteamCoils->SteamCoil(CoilNum).OutletWaterEnthalpy = 0.0;
            state.dataSteamCoils->SteamCoil(CoilNum).InletSteamPress = 0.0;
            state.dataSteamCoils->SteamCoil(CoilNum).InletSteamQuality = 0.0;
            state.dataSteamCoils->SteamCoil(CoilNum).OutletSteamQuality = 0.0;

            // More Environment initializations
            AirInletNode = state.dataSteamCoils->SteamCoil(CoilNum).AirInletNodeNum;
            SteamInletNode = state.dataSteamCoils->SteamCoil(CoilNum).SteamInletNodeNum;
            ControlNode = state.dataSteamCoils->SteamCoil(CoilNum).TempSetPointNodeNum;
            AirOutletNode = state.dataSteamCoils->SteamCoil(CoilNum).AirOutletNodeNum;

            state.dataLoopNodes->Node(SteamInletNode).Temp = 100.0;
            state.dataLoopNodes->Node(SteamInletNode).Press = 101325.0;
            SteamDensity = GetSatDensityRefrig(state,
                                               fluidNameSteam,
                                               state.dataLoopNodes->Node(SteamInletNode).Temp,
                                               1.0,
                                               state.dataLoopNodes->Node(SteamInletNode).FluidIndex,
                                               RoutineName);
            StartEnthSteam = GetSatEnthalpyRefrig(state,
                                                  fluidNameSteam,
                                                  state.dataLoopNodes->Node(SteamInletNode).Temp,
                                                  1.0,
                                                  state.dataLoopNodes->Node(SteamInletNode).FluidIndex,
                                                  RoutineName);
            state.dataLoopNodes->Node(SteamInletNode).Enthalpy = StartEnthSteam;
            state.dataLoopNodes->Node(SteamInletNode).Quality = 1.0;
            state.dataLoopNodes->Node(SteamInletNode).HumRat = 0.0;
            state.dataSteamCoils->SteamCoil(CoilNum).MaxSteamMassFlowRate =
                SteamDensity * state.dataSteamCoils->SteamCoil(CoilNum).MaxSteamVolFlowRate;
            //     Node(SteamInletNode)%MassFlowRate         = SteamCoil(CoilNum)%MaxSteamMassFlowRate
            //     Node(SteamInletNode)%MassFlowRateMinAvail = 0.0
            //     Node(SteamInletNode)%MassFlowRateMaxAvail = SteamCoil(CoilNum)%MaxSteamMassFlowRate
            InitComponentNodes(state,
                               0.0,
                               state.dataSteamCoils->SteamCoil(CoilNum).MaxSteamMassFlowRate,
                               state.dataSteamCoils->SteamCoil(CoilNum).SteamInletNodeNum,
                               state.dataSteamCoils->SteamCoil(CoilNum).SteamOutletNodeNum,
                               state.dataSteamCoils->SteamCoil(CoilNum).LoopNum,
                               state.dataSteamCoils->SteamCoil(CoilNum).LoopSide,
                               state.dataSteamCoils->SteamCoil(CoilNum).BranchNum,
                               state.dataSteamCoils->SteamCoil(CoilNum).CompNum);
            MyEnvrnFlag(CoilNum) = false;
        } // End If for the Begin Environment initializations

        if (!state.dataGlobal->BeginEnvrnFlag) {
            MyEnvrnFlag(CoilNum) = true;
        }

        // Do the Begin Day initializations
        // NONE

        // Do the begin HVAC time step initializations
        // NONE

        // Do the following initializations (every time step): This should be the info from
        // the previous components outlets or the node data in this section.

        AirInletNode = state.dataSteamCoils->SteamCoil(CoilNum).AirInletNodeNum;
        SteamInletNode = state.dataSteamCoils->SteamCoil(CoilNum).SteamInletNodeNum;
        ControlNode = state.dataSteamCoils->SteamCoil(CoilNum).TempSetPointNodeNum;
        AirOutletNode = state.dataSteamCoils->SteamCoil(CoilNum).AirOutletNodeNum;

        // First set the conditions for the air into the coil model

        // If a temperature setpoint controlled coil must set the desired outlet temp everytime
        if (ControlNode == 0) {
            state.dataSteamCoils->SteamCoil(CoilNum).DesiredOutletTemp = 0.0;
        } else if (ControlNode == AirOutletNode) {
            state.dataSteamCoils->SteamCoil(CoilNum).DesiredOutletTemp = state.dataLoopNodes->Node(ControlNode).TempSetPoint;
        } else {
            state.dataSteamCoils->SteamCoil(CoilNum).DesiredOutletTemp =
                state.dataLoopNodes->Node(ControlNode).TempSetPoint -
                (state.dataLoopNodes->Node(ControlNode).Temp - state.dataLoopNodes->Node(AirOutletNode).Temp);
        }

        state.dataSteamCoils->SteamCoil(CoilNum).InletAirMassFlowRate = state.dataLoopNodes->Node(AirInletNode).MassFlowRate;
        state.dataSteamCoils->SteamCoil(CoilNum).InletAirTemp = state.dataLoopNodes->Node(AirInletNode).Temp;
        state.dataSteamCoils->SteamCoil(CoilNum).InletAirHumRat = state.dataLoopNodes->Node(AirInletNode).HumRat;
        state.dataSteamCoils->SteamCoil(CoilNum).InletAirEnthalpy = state.dataLoopNodes->Node(AirInletNode).Enthalpy;
        if (FirstHVACIteration) {
            state.dataSteamCoils->SteamCoil(CoilNum).InletSteamMassFlowRate = state.dataSteamCoils->SteamCoil(CoilNum).MaxSteamMassFlowRate;
        } else {
            state.dataSteamCoils->SteamCoil(CoilNum).InletSteamMassFlowRate = state.dataLoopNodes->Node(SteamInletNode).MassFlowRate;
        }
        state.dataSteamCoils->SteamCoil(CoilNum).InletSteamTemp = state.dataLoopNodes->Node(SteamInletNode).Temp;
        state.dataSteamCoils->SteamCoil(CoilNum).InletSteamEnthalpy = state.dataLoopNodes->Node(SteamInletNode).Enthalpy;
        state.dataSteamCoils->SteamCoil(CoilNum).InletSteamPress = state.dataLoopNodes->Node(SteamInletNode).Press;
        state.dataSteamCoils->SteamCoil(CoilNum).InletSteamQuality = state.dataLoopNodes->Node(SteamInletNode).Quality;
        state.dataSteamCoils->SteamCoil(CoilNum).TotSteamHeatingCoilRate = 0.0;
        state.dataSteamCoils->SteamCoil(CoilNum).TotSteamCoolingCoilRate = 0.0;
        state.dataSteamCoils->SteamCoil(CoilNum).SenSteamCoolingCoilRate = 0.0;
        //   Node(SteamInletNode)%MassFlowRateMaxAvail = MIN(Node(SteamInletNode)%MassFlowRateMaxAvail,&
        //                                                   SteamCoil(CoilNum)%MaxSteamMassFlowRate)
    }

    void SizeSteamCoil(EnergyPlusData &state, int const CoilNum)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rahul Chillar
        //       DATE WRITTEN   Jan 2005
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for sizing Steam Coil Components for which flow rates have not been
        // specified in the input.

        // METHODOLOGY EMPLOYED:
        // Obtains flow rates from the zone or system sizing arrays and plant sizing data.

        // Using/Aliasing
        using namespace DataSizing;
        using FluidProperties::GetSatDensityRefrig;
        using FluidProperties::GetSatEnthalpyRefrig;
        using PlantUtilities::RegisterPlantCompDesignFlow;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static constexpr std::string_view RoutineName("SizeSteamCoil");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int PltSizNum;      // do loop index for plant sizing
        int PltSizSteamNum; // index of plant sizing object for 1st steam loop
        bool ErrorsFound;   // If errors detected in input
        Real64 CoilInTemp;
        Real64 CoilOutTemp;
        Real64 CoilOutHumRat;
        Real64 CoilInHumRat;
        Real64 DesCoilLoad;
        Real64 DesMassFlow;
        Real64 DesVolFlow;
        Real64 MinFlowFrac;
        Real64 OutAirFrac;
        Real64 TempSteamIn(100.0);
        Real64 EnthSteamInDry;
        Real64 EnthSteamOutWet;
        Real64 LatentHeatSteam;
        Real64 SteamDensity;
        Real64 RhoAirStd; // density of air at standard conditions
        Real64 CpAirStd;  // specific heat of air at std conditions
        Real64 CpWater;   // specific heat of water (condensed steam)

        std::string CompName;     // component name
        std::string CompType;     // component type
        std::string SizingString; // input field sizing description (e.g., Nominal Capacity)
        bool bPRINT = false;      // TRUE if sizing is reported to output (eio)
        Real64 TempSize;          // autosized value

        ErrorsFound = false;
        PltSizSteamNum = 0;
        PltSizNum = 0;
        CoilInTemp = 0.0;
        CoilInHumRat = 0.0;
        CoilOutTemp = 0.0;
        DesCoilLoad = 0.0;
        MinFlowFrac = 0.0;
        DesMassFlow = 0.0;
        DesVolFlow = 0.0;
        CpWater = 0.0;
        RhoAirStd = PsyRhoAirFnPbTdbW(state, state.dataEnvrn->StdBaroPress, 20.0, 0.0);
        CpAirStd = PsyCpAirFnW(0.0);
        bool coilWasAutosized(false); // coil report

        auto &OASysEqSizing(state.dataSize->OASysEqSizing);
        auto &TermUnitSizing(state.dataSize->TermUnitSizing);

        // If this is a steam coil
        // Find the appropriate steam Plant Sizing object
        if (state.dataSteamCoils->SteamCoil(CoilNum).MaxSteamVolFlowRate == AutoSize) {
            coilWasAutosized = true; // coil report
            PltSizSteamNum = MyPlantSizingIndex(state,
                                                "steam heating coil",
                                                state.dataSteamCoils->SteamCoil(CoilNum).Name,
                                                state.dataSteamCoils->SteamCoil(CoilNum).SteamInletNodeNum,
                                                state.dataSteamCoils->SteamCoil(CoilNum).SteamOutletNodeNum,
                                                ErrorsFound);
        }

        if (PltSizSteamNum > 0) {
            // If this is a central air system heating coil
            if (state.dataSize->CurSysNum > 0) {
                // If the coil water volume flow rate needs autosizing, then do it
                if (state.dataSteamCoils->SteamCoil(CoilNum).MaxSteamVolFlowRate == AutoSize) {
                    CheckSysSizing(state, "Coil:Heating:Steam", state.dataSteamCoils->SteamCoil(CoilNum).Name);

                    if (state.dataSteamCoils->SteamCoil(CoilNum).DesiccantRegenerationCoil) {

                        state.dataSize->DataDesicRegCoil = true;
                        state.dataSize->DataDesicDehumNum = state.dataSteamCoils->SteamCoil(CoilNum).DesiccantDehumNum;
                        CompType = state.dataSteamCoils->SteamCoil(CoilNum).SteamCoilType;
                        CompName = state.dataSteamCoils->SteamCoil(CoilNum).Name;
                        bPRINT = false;
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

                    // Set the duct flow rate
                    {
                        auto const SELECT_CASE_var(state.dataSize->CurDuctType);
                        if (SELECT_CASE_var == Main) {
                            DesVolFlow = state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).SysAirMinFlowRat *
                                         state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).DesMainVolFlow;
                        } else if (SELECT_CASE_var == Cooling) {
                            DesVolFlow = state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).SysAirMinFlowRat *
                                         state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).DesCoolVolFlow;
                        } else if (SELECT_CASE_var == Heating) {
                            DesVolFlow = state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).DesHeatVolFlow;
                        } else if (SELECT_CASE_var == Other) {
                            DesVolFlow = state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).DesMainVolFlow;
                        } else {
                            DesVolFlow = state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).DesMainVolFlow;
                        }
                    }
                    if (state.dataSize->DataDesicRegCoil) {
                        bPRINT = false;
                        TempSize = AutoSize;
                        bool errorsFound = false;
                        HeatingAirFlowSizer sizingHeatingAirFlow;
                        sizingHeatingAirFlow.overrideSizingString(SizingString);
                        // sizingHeatingAirFlow.setHVACSizingIndexData(FanCoil(FanCoilNum).HVACSizingIndex);
                        sizingHeatingAirFlow.initializeWithinEP(state, CompType, CompName, bPRINT, RoutineName);
                        DesVolFlow = sizingHeatingAirFlow.size(state, TempSize, errorsFound);
                    }
                    DesMassFlow = RhoAirStd * DesVolFlow;
                    // get the outside air fraction
                    if (state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).HeatOAOption == MinOA) {
                        if (DesVolFlow > 0.0) {
                            OutAirFrac = state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).DesOutAirVolFlow / DesVolFlow;
                        } else {
                            OutAirFrac = 1.0;
                        }
                        OutAirFrac = min(1.0, max(0.0, OutAirFrac));
                    } else {
                        OutAirFrac = 1.0;
                    }

                    if (state.dataSize->DataDesicRegCoil) {
                        DesCoilLoad = CpAirStd * DesMassFlow * (state.dataSize->DataDesOutletAirTemp - state.dataSize->DataDesInletAirTemp);
                    } else {
                        // mixed air temp
                        CoilInTemp = OutAirFrac * state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).HeatOutTemp +
                                     (1.0 - OutAirFrac) * state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).HeatRetTemp;
                        // coil load
                        DesCoilLoad = CpAirStd * DesMassFlow * (state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).HeatSupTemp - CoilInTemp);
                    }
                    // AUTOSTEAMCOIL
                    if (DesCoilLoad >= SmallLoad) {
                        // TempSteamIn=SteamCoil(CoilNum)%InletSteamTemp
                        // TempSteamIn=PlantSizData(PltSizSteamNum)%ExitTemp
                        TempSteamIn = 100.0; // Should be from the PlantSizing object (ExitTemp) instead of hardwired to 100?
                        // RefrigIndex is set during GetInput for this module
                        EnthSteamInDry = GetSatEnthalpyRefrig(
                            state, fluidNameSteam, TempSteamIn, 1.0, state.dataSteamCoils->SteamCoil(CoilNum).FluidIndex, RoutineName);
                        EnthSteamOutWet = GetSatEnthalpyRefrig(
                            state, fluidNameSteam, TempSteamIn, 0.0, state.dataSteamCoils->SteamCoil(CoilNum).FluidIndex, RoutineName);
                        LatentHeatSteam = EnthSteamInDry - EnthSteamOutWet;
                        SteamDensity = GetSatDensityRefrig(
                            state, fluidNameSteam, TempSteamIn, 1.0, state.dataSteamCoils->SteamCoil(CoilNum).FluidIndex, RoutineName);
                        // SteamCoil(CoilNum)%MaxSteamVolFlowRate = DesCoilLoad/(SteamDensity * LatentHeatSteam)
                        //            CpWater  =  GetSpecificHeatGlycol('WATER',  &
                        //                                              TempSteamIn, &
                        //                                              PlantLoop(SteamCoil(CoilNum)%LoopNum)%FluidIndex, &
                        //                                             'SizeSteamCoil')
                        CpWater = GetSatSpecificHeatRefrig(
                            state, fluidNameSteam, TempSteamIn, 0.0, state.dataSteamCoils->SteamCoil(CoilNum).FluidIndex, RoutineName);

                        state.dataSteamCoils->SteamCoil(CoilNum).MaxSteamVolFlowRate =
                            DesCoilLoad / (SteamDensity * (LatentHeatSteam + state.dataSteamCoils->SteamCoil(CoilNum).DegOfSubcooling * CpWater));
                        //             PlantSizData(PltSizSteamNum)%DeltaT*CPHW(PlantSizData(PltSizSteamNum)%ExitTemp)))
                    } else {
                        state.dataSteamCoils->SteamCoil(CoilNum).MaxSteamVolFlowRate = 0.0;
                        ShowWarningError(state,
                                         "The design coil load is zero for COIL:Heating:Steam " + state.dataSteamCoils->SteamCoil(CoilNum).Name);
                    }
                    BaseSizer::reportSizerOutput(state,
                                                 "Coil:Heating:Steam",
                                                 state.dataSteamCoils->SteamCoil(CoilNum).Name,
                                                 "Maximum Steam Flow Rate [m3/s]",
                                                 state.dataSteamCoils->SteamCoil(CoilNum).MaxSteamVolFlowRate);
                }
                state.dataSize->DataDesicRegCoil = false; // reset all globals to 0 to ensure correct sizing for other child components
                // Coil report, set fan info for airloopnum
                switch (state.dataAirSystemsData->PrimaryAirSystems(state.dataSize->CurSysNum).supFanModelTypeEnum) {
                case DataAirSystems::structArrayLegacyFanModels: {
                    int SupFanNum = state.dataAirSystemsData->PrimaryAirSystems(state.dataSize->CurSysNum).SupFanNum;
                    if (SupFanNum > 0) {
                        state.dataRptCoilSelection->coilSelectionReportObj->setCoilSupplyFanInfo(
                            state,
                            state.dataSteamCoils->SteamCoil(CoilNum).Name,
                            "Coil:Heating:Steam",
                            state.dataFans->Fan(state.dataAirSystemsData->PrimaryAirSystems(state.dataSize->CurSysNum).SupFanNum).FanName,
                            DataAirSystems::structArrayLegacyFanModels,
                            state.dataAirSystemsData->PrimaryAirSystems(state.dataSize->CurSysNum).SupFanNum);
                    }

                    break;
                }
                case DataAirSystems::objectVectorOOFanSystemModel: {
                    if (state.dataAirSystemsData->PrimaryAirSystems(state.dataSize->CurSysNum).supFanVecIndex >= 0) {
                        state.dataRptCoilSelection->coilSelectionReportObj->setCoilSupplyFanInfo(
                            state,
                            state.dataSteamCoils->SteamCoil(CoilNum).Name,
                            "Coil:Heating:Steam",
                            state.dataHVACFan->fanObjs[state.dataAirSystemsData->PrimaryAirSystems(state.dataSize->CurSysNum).supFanVecIndex]->name,
                            DataAirSystems::objectVectorOOFanSystemModel,
                            state.dataAirSystemsData->PrimaryAirSystems(state.dataSize->CurSysNum).supFanVecIndex);
                    }
                    break;
                }
                case DataAirSystems::fanModelTypeNotYetSet: {
                    // do nothing
                    break;
                }
                }

                // if this is a zone coil
            } else if (state.dataSize->CurZoneEqNum > 0) {
                CheckZoneSizing(state, "Coil:Heating:Steam", state.dataSteamCoils->SteamCoil(CoilNum).Name);
                // autosize the coil steam volume flow rate if needed
                if (state.dataSteamCoils->SteamCoil(CoilNum).MaxSteamVolFlowRate == AutoSize) {
                    // if coil is part of a terminal unit just use the terminal unit value
                    if (state.dataSize->TermUnitSingDuct || state.dataSize->TermUnitPIU || state.dataSize->TermUnitIU) {
                        if (state.dataSize->CurTermUnitSizingNum > 0) {
                            state.dataSteamCoils->SteamCoil(CoilNum).MaxSteamVolFlowRate =
                                TermUnitSizing(state.dataSize->CurTermUnitSizingNum).MaxSTVolFlow;
                        } else {
                            state.dataSteamCoils->SteamCoil(CoilNum).MaxSteamVolFlowRate = 0.0;
                        }
                        // if coil is part of a zonal unit, calc coil load to get hot Steam flow rate
                        DesCoilLoad = TermUnitSizing(state.dataSize->CurTermUnitSizingNum).DesHeatingLoad; // coil report
                        DesVolFlow = TermUnitSizing(state.dataSize->CurTermUnitSizingNum).AirVolFlow *
                                     TermUnitSizing(state.dataSize->CurTermUnitSizingNum).ReheatAirFlowMult; // coil report
                    } else {
                        CoilInTemp = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).DesHeatCoilInTemp;
                        CoilOutTemp = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).HeatDesTemp;
                        CoilOutHumRat = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).HeatDesHumRat;
                        DesMassFlow = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).DesHeatMassFlow;
                        DesVolFlow = DesMassFlow / RhoAirStd;
                        DesCoilLoad = PsyCpAirFnW(CoilOutHumRat) * DesMassFlow * (CoilOutTemp - CoilInTemp);
                        if (DesCoilLoad >= SmallLoad) {
                            TempSteamIn = 100.0;
                            // RefrigIndex is set during GetInput for this module
                            EnthSteamInDry = GetSatEnthalpyRefrig(
                                state, fluidNameSteam, TempSteamIn, 1.0, state.dataSteamCoils->SteamCoil(CoilNum).FluidIndex, RoutineName);
                            EnthSteamOutWet = GetSatEnthalpyRefrig(
                                state, fluidNameSteam, TempSteamIn, 0.0, state.dataSteamCoils->SteamCoil(CoilNum).FluidIndex, RoutineName);
                            LatentHeatSteam = EnthSteamInDry - EnthSteamOutWet;
                            SteamDensity = GetSatDensityRefrig(
                                state, fluidNameSteam, TempSteamIn, 1.0, state.dataSteamCoils->SteamCoil(CoilNum).FluidIndex, RoutineName);
                            // SteamCoil(CoilNum)%MaxSteamVolFlowRate = DesCoilLoad/(SteamDensity * LatentHeatSteam)
                            //           CpWater  =  GetSpecificHeatGlycol('WATER',  &
                            //                                             TempSteamIn, &
                            //                                             PlantLoop(SteamCoil(CoilNum)%LoopNum)%FluidIndex, &
                            //                                            'SizeSteamCoil')
                            CpWater = GetSatSpecificHeatRefrig(
                                state, fluidNameSteam, TempSteamIn, 0.0, state.dataSteamCoils->SteamCoil(CoilNum).FluidIndex, RoutineName);

                            state.dataSteamCoils->SteamCoil(CoilNum).MaxSteamVolFlowRate =
                                DesCoilLoad / (SteamDensity * (LatentHeatSteam + state.dataSteamCoils->SteamCoil(CoilNum).DegOfSubcooling * CpWater));
                            //             PlantSizData(PltSizSteamNum)%DeltaT*CPHW(PlantSizData(PltSizSteamNum)%ExitTemp)))
                        } else {
                            state.dataSteamCoils->SteamCoil(CoilNum).MaxSteamVolFlowRate = 0.0;
                        }
                    }
                    // issue warning if hw coil has zero flow
                    if (state.dataSteamCoils->SteamCoil(CoilNum).MaxSteamVolFlowRate == 0.0) {
                        ShowWarningError(state,
                                         "The design coil load is zero for COIL:Heating:Steam " + state.dataSteamCoils->SteamCoil(CoilNum).Name);
                        ShowContinueError(state, "The autosize value for max Steam flow rate is zero");
                    }
                    BaseSizer::reportSizerOutput(state,
                                                 "Coil:Heating:Steam",
                                                 state.dataSteamCoils->SteamCoil(CoilNum).Name,
                                                 "Maximum Steam Flow Rate [m3/s]",
                                                 state.dataSteamCoils->SteamCoil(CoilNum).MaxSteamVolFlowRate);
                }
            } // end zone coil ELSE - IF

        } else {
            // if there is no heating Plant Sizing object and autosizng was requested, issue an error message
            if (state.dataSteamCoils->SteamCoil(CoilNum).MaxSteamVolFlowRate == AutoSize) {
                ShowSevereError(state, "Autosizing of Steam coil requires a heating loop Sizing:Plant object");
                ShowContinueError(state, "Occurs in Steam coil object= " + state.dataSteamCoils->SteamCoil(CoilNum).Name);
                ErrorsFound = true;
            }
        } // end of heating Plant Sizing existence IF - ELSE

        // save the design Steam volumetric flow rate for use by the Steam loop sizing algorithms
        RegisterPlantCompDesignFlow(
            state, state.dataSteamCoils->SteamCoil(CoilNum).SteamInletNodeNum, state.dataSteamCoils->SteamCoil(CoilNum).MaxSteamVolFlowRate);

        state.dataRptCoilSelection->coilSelectionReportObj->setCoilHeatingCapacity(state,
                                                                                   state.dataSteamCoils->SteamCoil(CoilNum).Name,
                                                                                   "Coil:Heating:Steam",
                                                                                   DesCoilLoad,
                                                                                   coilWasAutosized,
                                                                                   state.dataSize->CurSysNum,
                                                                                   state.dataSize->CurZoneEqNum,
                                                                                   state.dataSize->CurOASysNum,
                                                                                   0.0,
                                                                                   1.0,
                                                                                   -999.0,
                                                                                   -999.0);
        state.dataRptCoilSelection->coilSelectionReportObj->setCoilWaterFlowNodeNums(state,
                                                                                     state.dataSteamCoils->SteamCoil(CoilNum).Name,
                                                                                     "Coil:Heating:Steam",
                                                                                     state.dataSteamCoils->SteamCoil(CoilNum).MaxSteamVolFlowRate,
                                                                                     coilWasAutosized,
                                                                                     state.dataSteamCoils->SteamCoil(CoilNum).SteamInletNodeNum,
                                                                                     state.dataSteamCoils->SteamCoil(CoilNum).SteamOutletNodeNum,
                                                                                     state.dataSteamCoils->SteamCoil(CoilNum).LoopNum);
        state.dataRptCoilSelection->coilSelectionReportObj->setCoilWaterHeaterCapacityNodeNums(
            state,
            state.dataSteamCoils->SteamCoil(CoilNum).Name,
            "Coil:Heating:Steam",
            DesCoilLoad,
            coilWasAutosized,
            state.dataSteamCoils->SteamCoil(CoilNum).SteamInletNodeNum,
            state.dataSteamCoils->SteamCoil(CoilNum).SteamOutletNodeNum,
            state.dataSteamCoils->SteamCoil(CoilNum).LoopNum);
        state.dataRptCoilSelection->coilSelectionReportObj->setCoilEntWaterTemp(
            state, state.dataSteamCoils->SteamCoil(CoilNum).Name, "Coil:Heating:Steam", TempSteamIn); // coil  report
        state.dataRptCoilSelection->coilSelectionReportObj->setCoilLvgWaterTemp(
            state,
            state.dataSteamCoils->SteamCoil(CoilNum).Name,
            "Coil:Heating:Steam",
            TempSteamIn - state.dataSteamCoils->SteamCoil(CoilNum).DegOfSubcooling); // coil report
        state.dataRptCoilSelection->coilSelectionReportObj->setCoilWaterDeltaT(
            state,
            state.dataSteamCoils->SteamCoil(CoilNum).Name,
            "Coil:Heating:Steam",
            state.dataSteamCoils->SteamCoil(CoilNum).DegOfSubcooling); // coil report
        state.dataSteamCoils->SteamCoil(CoilNum).DesCoilCapacity = DesCoilLoad;
        state.dataSteamCoils->SteamCoil(CoilNum).DesAirVolFlow = DesVolFlow;
        if (ErrorsFound) {
            ShowFatalError(state, "Preceding Steam coil sizing errors cause program termination");
        }

        // There is no standard rating for heating coils at this point, so fill with dummy flag values
        state.dataRptCoilSelection->coilSelectionReportObj->setRatedCoilConditions(state,
                                                                                   state.dataSteamCoils->SteamCoil(CoilNum).Name,
                                                                                   "Coil:Heating:Steam",
                                                                                   -999.0,
                                                                                   -999.0,
                                                                                   -999.0,
                                                                                   -999.0,
                                                                                   -999.0,
                                                                                   -999.0,
                                                                                   -999.0,
                                                                                   -999.0,
                                                                                   -999.0,
                                                                                   -999.0,
                                                                                   -999.0,
                                                                                   -999.0,
                                                                                   -999.0);
    }

    // End Initialization Section of the Module

    // Begin Algorithm Section of the Module

    void CalcSteamAirCoil(EnergyPlusData &state,
                          int const CoilNum,
                          Real64 const QCoilRequested, // requested coil load
                          Real64 &QCoilActual,         // coil load actually delivered
                          int const FanOpMode,         // fan operating mode
                          Real64 const PartLoadRatio   // part-load ratio of heating coil
    )
    {
        // SUBROUTINE INFORMATION:
        //   AUTHOR         Rahul Chillar
        //   DATE WRITTEN   Jan 2005
        //   MODIFIED       Sep. 2012, B. Griffith, add calls to SetComponentFlowRate for plant interactions
        //                  Jul. 2016, R. Zhang, Applied the coil supply air temperature sensor offset fault model
        //   RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Simple Steam to air heat exchanger which,
        // serves as an interface for distributing heat from boiler to zones.

        // METHODOLOGY EMPLOYED:
        // Steam coils are different, All of steam condenses in heat exchanger
        // Steam traps allow only water to leave the coil,the degree of subcooling
        // desired is input by the user, which is used to calculate water outlet temp.
        // Heat exchange is = Latent Heat + Sensible heat,coil effectivness is 1.0

        using DataHVACGlobals::TempControlTol;
        using PlantUtilities::SetComponentFlowRate;

        static constexpr std::string_view RoutineName("CalcSteamAirCoil");
        static constexpr std::string_view RoutineNameSizeSteamCoil("SizeSteamCoil");

        Real64 SteamMassFlowRate(0.0);
        Real64 AirMassFlow(0.0); // [kg/sec]
        Real64 TempAirIn(0.0);   // [C]
        Real64 TempAirOut(0.0);  // [C]
        Real64 Win(0.0);
        Real64 TempSteamIn(0.0);
        Real64 TempWaterOut(0.0);
        Real64 CapacitanceAir(0.0);
        Real64 HeatingCoilLoad(0.0);
        Real64 CoilPress(0.0);
        Real64 EnthSteamInDry(0.0);
        Real64 EnthSteamOutWet(0.0);
        Real64 LatentHeatSteam(0.0);
        Real64 SubcoolDeltaTemp(0.0);
        Real64 TempSetPoint(0.0);
        Real64 QCoilReq(0.0);
        Real64 QCoilCap(0.0);
        Real64 QSteamCoilMaxHT(0.0);
        Real64 TempWaterAtmPress(0.0);
        Real64 TempLoopOutToPump(0.0);
        Real64 EnergyLossToEnvironment(0.0);
        Real64 EnthCoilOutlet(0.0);
        Real64 EnthPumpInlet(0.0);
        Real64 EnthAtAtmPress(0.0);
        Real64 CpWater(0.0);

        QCoilReq = QCoilRequested;
        TempAirIn = state.dataSteamCoils->SteamCoil(CoilNum).InletAirTemp;
        Win = state.dataSteamCoils->SteamCoil(CoilNum).InletAirHumRat;
        TempSteamIn = state.dataSteamCoils->SteamCoil(CoilNum).InletSteamTemp;
        CoilPress = state.dataSteamCoils->SteamCoil(CoilNum).InletSteamPress;
        SubcoolDeltaTemp = state.dataSteamCoils->SteamCoil(CoilNum).DegOfSubcooling;
        TempSetPoint = state.dataSteamCoils->SteamCoil(CoilNum).DesiredOutletTemp;

        // If there is a fault of coil SAT Sensor
        if (state.dataSteamCoils->SteamCoil(CoilNum).FaultyCoilSATFlag && (!state.dataGlobal->WarmupFlag) && (!state.dataGlobal->DoingSizing) &&
            (!state.dataGlobal->KickOffSimulation)) {
            // calculate the sensor offset using fault information
            int FaultIndex = state.dataSteamCoils->SteamCoil(CoilNum).FaultyCoilSATIndex;
            state.dataSteamCoils->SteamCoil(CoilNum).FaultyCoilSATOffset =
                state.dataFaultsMgr->FaultsCoilSATSensor(FaultIndex).CalFaultOffsetAct(state);
            // update the TempSetPoint
            TempSetPoint -= state.dataSteamCoils->SteamCoil(CoilNum).FaultyCoilSATOffset;
        }

        //  adjust mass flow rates for cycling fan cycling coil operation
        if (FanOpMode == CycFanCycCoil) {
            if (PartLoadRatio > 0.0) {
                AirMassFlow = state.dataSteamCoils->SteamCoil(CoilNum).InletAirMassFlowRate / PartLoadRatio;
                SteamMassFlowRate = min(state.dataSteamCoils->SteamCoil(CoilNum).InletSteamMassFlowRate / PartLoadRatio,
                                        state.dataSteamCoils->SteamCoil(CoilNum).MaxSteamMassFlowRate);
                QCoilReq /= PartLoadRatio;
            } else {
                AirMassFlow = 0.0;
                SteamMassFlowRate = 0.0;
            }
        } else {
            AirMassFlow = state.dataSteamCoils->SteamCoil(CoilNum).InletAirMassFlowRate;
            SteamMassFlowRate = state.dataSteamCoils->SteamCoil(CoilNum).InletSteamMassFlowRate;
        }

        if (AirMassFlow > 0.0) { // If the coil is operating
            CapacitanceAir = PsyCpAirFnW(Win) * AirMassFlow;
        } else {
            CapacitanceAir = 0.0;
        }

        // If the coil is operating there should be some heating capacitance
        //  across the coil, so do the simulation. If not set outlet to inlet and no load.
        //  Also the coil has to be scheduled to be available
        //  Control output to meet load QCoilReq. Load Controlled Coil.
        {
            auto const SELECT_CASE_var(state.dataSteamCoils->SteamCoil(CoilNum).TypeOfCoil);

            if (SELECT_CASE_var == state.dataSteamCoils->ZoneLoadControl) {
                if ((CapacitanceAir > 0.0) && ((state.dataSteamCoils->SteamCoil(CoilNum).InletSteamMassFlowRate) > 0.0) &&
                    (GetCurrentScheduleValue(state, state.dataSteamCoils->SteamCoil(CoilNum).SchedPtr) > 0.0 ||
                     state.dataSteamCoils->MySizeFlag(CoilNum)) &&
                    (QCoilReq > 0.0)) {

                    // Steam heat exchangers would not have effectivness, since all of the steam is
                    // converted to water and only then the steam trap allows it to leave the heat
                    // exchanger, subsequently heat exchange is latent heat + subcooling.
                    EnthSteamInDry = GetSatEnthalpyRefrig(
                        state, fluidNameSteam, TempSteamIn, 1.0, state.dataSteamCoils->SteamCoil(CoilNum).FluidIndex, RoutineName);
                    EnthSteamOutWet = GetSatEnthalpyRefrig(
                        state, fluidNameSteam, TempSteamIn, 0.0, state.dataSteamCoils->SteamCoil(CoilNum).FluidIndex, RoutineName);

                    LatentHeatSteam = EnthSteamInDry - EnthSteamOutWet;

                    //          CpWater = GetSpecificHeatGlycol('WATER',  &
                    //                                           TempSteamIn, &
                    //                                           PlantLoop(SteamCoil(CoilNum)%LoopNum)%FluidIndex, &
                    //                                           'CalcSteamAirCoil')

                    CpWater = GetSatSpecificHeatRefrig(
                        state, fluidNameSteam, TempSteamIn, 0.0, state.dataSteamCoils->SteamCoil(CoilNum).FluidIndex, RoutineNameSizeSteamCoil);

                    // Max Heat Transfer
                    QSteamCoilMaxHT = state.dataSteamCoils->SteamCoil(CoilNum).MaxSteamMassFlowRate * (LatentHeatSteam + SubcoolDeltaTemp * CpWater);
                    state.dataSteamCoils->SteamCoil(CoilNum).OperatingCapacity = QSteamCoilMaxHT;

                    // Determine the Max coil capacity and check for the same.
                    if (QCoilReq > QSteamCoilMaxHT) {
                        QCoilCap = QSteamCoilMaxHT;
                    } else {
                        QCoilCap = QCoilReq;
                    }

                    // Steam Mass Flow Rate Required
                    SteamMassFlowRate = QCoilCap / (LatentHeatSteam + SubcoolDeltaTemp * CpWater);

                    SetComponentFlowRate(state,
                                         SteamMassFlowRate,
                                         state.dataSteamCoils->SteamCoil(CoilNum).SteamInletNodeNum,
                                         state.dataSteamCoils->SteamCoil(CoilNum).SteamOutletNodeNum,
                                         state.dataSteamCoils->SteamCoil(CoilNum).LoopNum,
                                         state.dataSteamCoils->SteamCoil(CoilNum).LoopSide,
                                         state.dataSteamCoils->SteamCoil(CoilNum).BranchNum,
                                         state.dataSteamCoils->SteamCoil(CoilNum).CompNum);

                    // recalculate if mass flow rate changed in previous call.
                    QCoilCap = SteamMassFlowRate * (LatentHeatSteam + SubcoolDeltaTemp * CpWater);

                    // In practice Sensible & Superheated heat transfer is negligible compared to latent part.
                    // This is required for outlet water temperature, otherwise it will be saturation temperature.
                    // Steam Trap drains off all the Water formed.
                    // Here Degree of Subcooling is used to calculate hot water return temperature.

                    // Calculating Water outlet temperature
                    TempWaterOut = TempSteamIn - SubcoolDeltaTemp;

                    // Total Heat Transfer to air
                    HeatingCoilLoad = QCoilCap;

                    // Temperature of air at outlet
                    TempAirOut = TempAirIn + QCoilCap / (AirMassFlow * PsyCpAirFnW(Win));

                    state.dataSteamCoils->SteamCoil(CoilNum).OutletSteamMassFlowRate = SteamMassFlowRate;
                    state.dataSteamCoils->SteamCoil(CoilNum).InletSteamMassFlowRate = SteamMassFlowRate;

                    //************************* Loop Losses *****************************
                    // Loop pressure return considerations included in steam coil since the pipes are
                    // perfect and do not account for losses.
                    // Return water is condensate at atmoshperic pressure
                    // Process is considered constant enthalpy expansion
                    // No quality function in EnergyPlus hence no option left apart from
                    // considering saturated state.
                    //              StdBaroPress=101325

                    TempWaterAtmPress = GetSatTemperatureRefrig(
                        state, fluidNameSteam, state.dataEnvrn->StdBaroPress, state.dataSteamCoils->SteamCoil(CoilNum).FluidIndex, RoutineName);

                    // Point 4 at atm - loop delta subcool during return journery back to pump
                    TempLoopOutToPump = TempWaterAtmPress - state.dataSteamCoils->SteamCoil(CoilNum).LoopSubcoolReturn;

                    // Actual Steam Coil Outlet Enthalpy
                    EnthCoilOutlet = GetSatEnthalpyRefrig(
                                         state, fluidNameSteam, TempSteamIn, 0.0, state.dataSteamCoils->SteamCoil(CoilNum).FluidIndex, RoutineName) -
                                     CpWater * SubcoolDeltaTemp;

                    // Enthalpy at Point 4
                    EnthAtAtmPress = GetSatEnthalpyRefrig(
                        state, fluidNameSteam, TempWaterAtmPress, 0.0, state.dataSteamCoils->SteamCoil(CoilNum).FluidIndex, RoutineName);

                    // Reported value of coil outlet enthalpy at the node to match the node outlet temperature
                    CpWater = GetSatSpecificHeatRefrig(
                        state, fluidNameSteam, TempLoopOutToPump, 0.0, state.dataSteamCoils->SteamCoil(CoilNum).FluidIndex, RoutineNameSizeSteamCoil);

                    EnthPumpInlet = EnthAtAtmPress - CpWater * state.dataSteamCoils->SteamCoil(CoilNum).LoopSubcoolReturn;

                    state.dataSteamCoils->SteamCoil(CoilNum).OutletWaterEnthalpy = EnthPumpInlet;

                    // Point 3-Point 5,
                    EnergyLossToEnvironment = SteamMassFlowRate * (EnthCoilOutlet - EnthPumpInlet);

                    // Loss to enviornment due to pressure drop
                    state.dataSteamCoils->SteamCoil(CoilNum).LoopLoss = EnergyLossToEnvironment;
                    //************************* Loop Losses *****************************
                } else { // Coil is not running.

                    TempAirOut = TempAirIn;
                    TempWaterOut = TempSteamIn;
                    HeatingCoilLoad = 0.0;
                    state.dataSteamCoils->SteamCoil(CoilNum).OutletWaterEnthalpy = state.dataSteamCoils->SteamCoil(CoilNum).InletSteamEnthalpy;
                    state.dataSteamCoils->SteamCoil(CoilNum).OutletSteamMassFlowRate = 0.0;
                    state.dataSteamCoils->SteamCoil(CoilNum).OutletSteamQuality = 0.0;
                    state.dataSteamCoils->SteamCoil(CoilNum).LoopLoss = 0.0;
                    TempLoopOutToPump = TempWaterOut;
                }

            } else if (SELECT_CASE_var == state.dataSteamCoils->TemperatureSetPointControl) {
                // Control coil output to meet a Setpoint Temperature.
                if ((CapacitanceAir > 0.0) && ((state.dataSteamCoils->SteamCoil(CoilNum).InletSteamMassFlowRate) > 0.0) &&
                    (GetCurrentScheduleValue(state, state.dataSteamCoils->SteamCoil(CoilNum).SchedPtr) > 0.0 ||
                     state.dataSteamCoils->MySizeFlag(CoilNum)) &&
                    (std::abs(TempSetPoint - TempAirIn) > TempControlTol)) {

                    // Steam heat exchangers would not have effectivness, since all of the steam is
                    // converted to water and only then the steam trap allows it to leave the heat
                    // exchanger, subsequently heat exchange is latent heat + subcooling.
                    EnthSteamInDry = GetSatEnthalpyRefrig(
                        state, fluidNameSteam, TempSteamIn, 1.0, state.dataSteamCoils->SteamCoil(CoilNum).FluidIndex, RoutineName);
                    EnthSteamOutWet = GetSatEnthalpyRefrig(
                        state, fluidNameSteam, TempSteamIn, 0.0, state.dataSteamCoils->SteamCoil(CoilNum).FluidIndex, RoutineName);
                    LatentHeatSteam = EnthSteamInDry - EnthSteamOutWet;

                    //          CpWater = GetSpecificHeatGlycol('WATER',  &
                    //                                           TempSteamIn, &
                    //                                           PlantLoop(SteamCoil(CoilNum)%LoopNum)%FluidIndex, &
                    //                                           'CalcSteamAirCoil')
                    CpWater = GetSatSpecificHeatRefrig(
                        state, fluidNameSteam, TempSteamIn, 0.0, state.dataSteamCoils->SteamCoil(CoilNum).FluidIndex, RoutineNameSizeSteamCoil);

                    // Max Heat Transfer
                    QSteamCoilMaxHT = state.dataSteamCoils->SteamCoil(CoilNum).MaxSteamMassFlowRate * (LatentHeatSteam + SubcoolDeltaTemp * CpWater);

                    // Coil Load in case of temperature setpoint
                    QCoilCap = CapacitanceAir * (TempSetPoint - TempAirIn);

                    // Check to see if setpoint above enetering temperature. If not, set
                    // output to zero.
                    if (QCoilCap <= 0.0) {
                        QCoilCap = 0.0;
                        TempAirOut = TempAirIn;

                        // Steam Mass Flow Rate Required
                        SteamMassFlowRate = 0.0;
                        SetComponentFlowRate(state,
                                             SteamMassFlowRate,
                                             state.dataSteamCoils->SteamCoil(CoilNum).SteamInletNodeNum,
                                             state.dataSteamCoils->SteamCoil(CoilNum).SteamOutletNodeNum,
                                             state.dataSteamCoils->SteamCoil(CoilNum).LoopNum,
                                             state.dataSteamCoils->SteamCoil(CoilNum).LoopSide,
                                             state.dataSteamCoils->SteamCoil(CoilNum).BranchNum,
                                             state.dataSteamCoils->SteamCoil(CoilNum).CompNum);
                        // Inlet equal to outlet when not required to run.
                        TempWaterOut = TempSteamIn;

                        // Total Heat Transfer to air
                        HeatingCoilLoad = QCoilCap;

                        // The HeatingCoilLoad is the change in the enthalpy of the water
                        state.dataSteamCoils->SteamCoil(CoilNum).OutletWaterEnthalpy = state.dataSteamCoils->SteamCoil(CoilNum).InletSteamEnthalpy;

                        // Outlet flow rate set to inlet
                        state.dataSteamCoils->SteamCoil(CoilNum).OutletSteamMassFlowRate = SteamMassFlowRate;
                        state.dataSteamCoils->SteamCoil(CoilNum).InletSteamMassFlowRate = SteamMassFlowRate;

                    } else if (QCoilCap > QSteamCoilMaxHT) {
                        // Setting to Maximum Coil Capacity
                        QCoilCap = QSteamCoilMaxHT;

                        // Temperature of air at outlet
                        TempAirOut = TempAirIn + QCoilCap / (AirMassFlow * PsyCpAirFnW(Win));

                        // In practice Sensible & Superheated heat transfer is negligible compared to latent part.
                        // This is required for outlet water temperature, otherwise it will be saturation temperature.
                        // Steam Trap drains off all the Water formed.
                        // Here Degree of Subcooling is used to calculate hot water return temperature.

                        // Calculating Water outlet temperature
                        TempWaterOut = TempSteamIn - SubcoolDeltaTemp;

                        // Steam Mass Flow Rate Required
                        SteamMassFlowRate = QCoilCap / (LatentHeatSteam + SubcoolDeltaTemp * CpWater);
                        SetComponentFlowRate(state,
                                             SteamMassFlowRate,
                                             state.dataSteamCoils->SteamCoil(CoilNum).SteamInletNodeNum,
                                             state.dataSteamCoils->SteamCoil(CoilNum).SteamOutletNodeNum,
                                             state.dataSteamCoils->SteamCoil(CoilNum).LoopNum,
                                             state.dataSteamCoils->SteamCoil(CoilNum).LoopSide,
                                             state.dataSteamCoils->SteamCoil(CoilNum).BranchNum,
                                             state.dataSteamCoils->SteamCoil(CoilNum).CompNum);

                        // recalculate in case previous call changed mass flow rate
                        QCoilCap = SteamMassFlowRate * (LatentHeatSteam + SubcoolDeltaTemp * CpWater);
                        TempAirOut = TempAirIn + QCoilCap / (AirMassFlow * PsyCpAirFnW(Win));

                        // Total Heat Transfer to air
                        HeatingCoilLoad = QCoilCap;

                        // The HeatingCoilLoad is the change in the enthalpy of the water
                        state.dataSteamCoils->SteamCoil(CoilNum).OutletWaterEnthalpy =
                            state.dataSteamCoils->SteamCoil(CoilNum).InletSteamEnthalpy - HeatingCoilLoad / SteamMassFlowRate;
                        state.dataSteamCoils->SteamCoil(CoilNum).OutletSteamMassFlowRate = SteamMassFlowRate;
                        state.dataSteamCoils->SteamCoil(CoilNum).InletSteamMassFlowRate = SteamMassFlowRate;

                    } else {
                        // Temp air out is temperature Setpoint
                        TempAirOut = TempSetPoint;

                        // In practice Sensible & Superheated heat transfer is negligible compared to latent part.
                        // This is required for outlet water temperature, otherwise it will be saturation temperature.
                        // Steam Trap drains off all the Water formed.
                        // Here Degree of Subcooling is used to calculate hot water return temperature.

                        // Calculating Water outlet temperature
                        TempWaterOut = TempSteamIn - SubcoolDeltaTemp;

                        // Steam Mass Flow Rate Required
                        SteamMassFlowRate = QCoilCap / (LatentHeatSteam + SubcoolDeltaTemp * CpWater);
                        SetComponentFlowRate(state,
                                             SteamMassFlowRate,
                                             state.dataSteamCoils->SteamCoil(CoilNum).SteamInletNodeNum,
                                             state.dataSteamCoils->SteamCoil(CoilNum).SteamOutletNodeNum,
                                             state.dataSteamCoils->SteamCoil(CoilNum).LoopNum,
                                             state.dataSteamCoils->SteamCoil(CoilNum).LoopSide,
                                             state.dataSteamCoils->SteamCoil(CoilNum).BranchNum,
                                             state.dataSteamCoils->SteamCoil(CoilNum).CompNum);

                        // recalculate in case previous call changed mass flow rate
                        QCoilCap = SteamMassFlowRate * (LatentHeatSteam + SubcoolDeltaTemp * CpWater);
                        TempAirOut = TempAirIn + QCoilCap / (AirMassFlow * PsyCpAirFnW(Win));

                        // Total Heat Transfer to air
                        HeatingCoilLoad = QCoilCap;

                        state.dataSteamCoils->SteamCoil(CoilNum).OutletSteamMassFlowRate = SteamMassFlowRate;
                        state.dataSteamCoils->SteamCoil(CoilNum).InletSteamMassFlowRate = SteamMassFlowRate;

                        //************************* Loop Losses *****************************
                        // Loop pressure return considerations included in steam coil since the pipes are
                        // perfect and do not account for losses.

                        // Return water is condensate at atmoshperic pressure
                        // Process is considered constant enthalpy expansion
                        // No quality function in EnergyPlus hence no option left apart from
                        // considering saturated state.
                        //              StdBaroPress=101325

                        TempWaterAtmPress = GetSatTemperatureRefrig(
                            state, fluidNameSteam, state.dataEnvrn->StdBaroPress, state.dataSteamCoils->SteamCoil(CoilNum).FluidIndex, RoutineName);

                        // Point 4 at atm - loop delta subcool during return journery back to pump
                        TempLoopOutToPump = TempWaterAtmPress - state.dataSteamCoils->SteamCoil(CoilNum).LoopSubcoolReturn;

                        // Actual Steam Coil Outlet Enthalpy
                        EnthCoilOutlet =
                            GetSatEnthalpyRefrig(
                                state, fluidNameSteam, TempSteamIn, 0.0, state.dataSteamCoils->SteamCoil(CoilNum).FluidIndex, RoutineName) -
                            CpWater * SubcoolDeltaTemp;

                        // Enthalpy at Point 4
                        EnthAtAtmPress = GetSatEnthalpyRefrig(
                            state, fluidNameSteam, TempWaterAtmPress, 0.0, state.dataSteamCoils->SteamCoil(CoilNum).FluidIndex, RoutineName);

                        CpWater = GetSatSpecificHeatRefrig(state,
                                                           fluidNameSteam,
                                                           TempLoopOutToPump,
                                                           0.0,
                                                           state.dataSteamCoils->SteamCoil(CoilNum).FluidIndex,
                                                           RoutineNameSizeSteamCoil);

                        // Reported value of coil outlet enthalpy at the node to match the node outlet temperature
                        EnthPumpInlet = EnthAtAtmPress - CpWater * state.dataSteamCoils->SteamCoil(CoilNum).LoopSubcoolReturn;

                        state.dataSteamCoils->SteamCoil(CoilNum).OutletWaterEnthalpy = EnthPumpInlet;

                        // Point 3-Point 5,
                        EnergyLossToEnvironment = SteamMassFlowRate * (EnthCoilOutlet - EnthPumpInlet);

                        // Loss to enviornment due to pressure drop
                        state.dataSteamCoils->SteamCoil(CoilNum).LoopLoss = EnergyLossToEnvironment;
                        //************************* Loop Losses *****************************
                    }

                } else { // If not running Conditions do not change across coil from inlet to outlet
                    SteamMassFlowRate = 0.0;
                    SetComponentFlowRate(state,
                                         SteamMassFlowRate,
                                         state.dataSteamCoils->SteamCoil(CoilNum).SteamInletNodeNum,
                                         state.dataSteamCoils->SteamCoil(CoilNum).SteamOutletNodeNum,
                                         state.dataSteamCoils->SteamCoil(CoilNum).LoopNum,
                                         state.dataSteamCoils->SteamCoil(CoilNum).LoopSide,
                                         state.dataSteamCoils->SteamCoil(CoilNum).BranchNum,
                                         state.dataSteamCoils->SteamCoil(CoilNum).CompNum);
                    TempAirOut = TempAirIn;
                    TempWaterOut = TempSteamIn;
                    HeatingCoilLoad = 0.0;
                    state.dataSteamCoils->SteamCoil(CoilNum).OutletWaterEnthalpy = state.dataSteamCoils->SteamCoil(CoilNum).InletSteamEnthalpy;
                    state.dataSteamCoils->SteamCoil(CoilNum).OutletSteamMassFlowRate = 0.0;
                    state.dataSteamCoils->SteamCoil(CoilNum).OutletSteamQuality = 0.0;
                    state.dataSteamCoils->SteamCoil(CoilNum).LoopLoss = 0.0;
                    TempLoopOutToPump = TempWaterOut;
                }
            }
        }

        if (FanOpMode == CycFanCycCoil) {
            HeatingCoilLoad *= PartLoadRatio;
        }

        // Set the outlet conditions
        state.dataSteamCoils->SteamCoil(CoilNum).TotSteamHeatingCoilRate = HeatingCoilLoad;
        state.dataSteamCoils->SteamCoil(CoilNum).OutletAirTemp = TempAirOut;
        state.dataSteamCoils->SteamCoil(CoilNum).OutletSteamTemp = TempLoopOutToPump;
        state.dataSteamCoils->SteamCoil(CoilNum).OutletSteamQuality = 0.0;
        QCoilActual = HeatingCoilLoad;

        // This SteamCoil does not change the moisture or Mass Flow across the component
        state.dataSteamCoils->SteamCoil(CoilNum).OutletAirHumRat = state.dataSteamCoils->SteamCoil(CoilNum).InletAirHumRat;
        state.dataSteamCoils->SteamCoil(CoilNum).OutletAirMassFlowRate = state.dataSteamCoils->SteamCoil(CoilNum).InletAirMassFlowRate;
        // Set the outlet enthalpys for air and water
        state.dataSteamCoils->SteamCoil(CoilNum).OutletAirEnthalpy =
            PsyHFnTdbW(state.dataSteamCoils->SteamCoil(CoilNum).OutletAirTemp, state.dataSteamCoils->SteamCoil(CoilNum).OutletAirHumRat);
    }

    // Beginning of Update subroutines for the SteamCoil Module

    void UpdateSteamCoil(EnergyPlusData &state, int const CoilNum)
    {
        // SUBROUTINE INFORMATION:
        //   AUTHOR         Rahul Chillar
        //   DATE WRITTEN   Jan 2005
        //   MODIFIED       na
        //   RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine updates the coil outlet nodes.

        // METHODOLOGY EMPLOYED:
        // Data is moved from the coil data structure to the coil outlet nodes.

        using PlantUtilities::SafeCopyPlantNode;

        int AirInletNode;
        int SteamInletNode;
        int AirOutletNode;
        int SteamOutletNode;

        AirInletNode = state.dataSteamCoils->SteamCoil(CoilNum).AirInletNodeNum;
        SteamInletNode = state.dataSteamCoils->SteamCoil(CoilNum).SteamInletNodeNum;
        AirOutletNode = state.dataSteamCoils->SteamCoil(CoilNum).AirOutletNodeNum;
        SteamOutletNode = state.dataSteamCoils->SteamCoil(CoilNum).SteamOutletNodeNum;

        // Set the outlet air nodes of the SteamCoil
        state.dataLoopNodes->Node(AirOutletNode).MassFlowRate = state.dataSteamCoils->SteamCoil(CoilNum).OutletAirMassFlowRate;
        state.dataLoopNodes->Node(AirOutletNode).Temp = state.dataSteamCoils->SteamCoil(CoilNum).OutletAirTemp;
        state.dataLoopNodes->Node(AirOutletNode).HumRat = state.dataSteamCoils->SteamCoil(CoilNum).OutletAirHumRat;
        state.dataLoopNodes->Node(AirOutletNode).Enthalpy = state.dataSteamCoils->SteamCoil(CoilNum).OutletAirEnthalpy;

        SafeCopyPlantNode(state, SteamInletNode, SteamOutletNode);

        // Set the outlet Steam nodes for the Coil
        //   Node(SteamOutletNode)%MassFlowRate = SteamCoil(CoilNum)%OutletSteamMassFlowRate
        state.dataLoopNodes->Node(SteamOutletNode).Temp = state.dataSteamCoils->SteamCoil(CoilNum).OutletSteamTemp;
        state.dataLoopNodes->Node(SteamOutletNode).Enthalpy = state.dataSteamCoils->SteamCoil(CoilNum).OutletWaterEnthalpy;
        state.dataLoopNodes->Node(SteamOutletNode).Quality = state.dataSteamCoils->SteamCoil(CoilNum).OutletSteamQuality;
        // Node(SteamInletNode)%MassFlowRate  = SteamCoil(CoilNum)%OutletSteamMassFlowRate

        // Set the outlet nodes for properties that just pass through & not used
        state.dataLoopNodes->Node(AirOutletNode).Quality = state.dataLoopNodes->Node(AirInletNode).Quality;
        state.dataLoopNodes->Node(AirOutletNode).Press = state.dataLoopNodes->Node(AirInletNode).Press;
        state.dataLoopNodes->Node(AirOutletNode).MassFlowRateMin = state.dataLoopNodes->Node(AirInletNode).MassFlowRateMin;
        state.dataLoopNodes->Node(AirOutletNode).MassFlowRateMax = state.dataLoopNodes->Node(AirInletNode).MassFlowRateMax;
        state.dataLoopNodes->Node(AirOutletNode).MassFlowRateMinAvail = state.dataLoopNodes->Node(AirInletNode).MassFlowRateMinAvail;
        state.dataLoopNodes->Node(AirOutletNode).MassFlowRateMaxAvail = state.dataLoopNodes->Node(AirInletNode).MassFlowRateMaxAvail;

        // Set the outlet nodes for properties that just pass through & not used

        // Node(SteamOutletNode)%Press              = Node(SteamInletNode)%Press
        //   Node(SteamOutletNode)%Press               = StdBaroPress  ! Water out at atm pressure
        //   Node(SteamOutletNode)%HumRat              = Node(SteamInletNode)%HumRat
        //   Node(SteamOutletNode)%MassFlowRateMin     = Node(SteamInletNode)%MassFlowRateMin
        //   Node(SteamOutletNode)%MassFlowRateMax     = Node(SteamInletNode)%MassFlowRateMax
        //   Node(SteamOutletNode)%MassFlowRateMinAvail= Node(SteamInletNode)%MassFlowRateMinAvail
        //   Node(SteamOutletNode)%MassFlowRateMaxAvail= Node(SteamInletNode)%MassFlowRateMaxAvail

        //   IF (SteamCoil(CoilNum)%InletSteamMassFlowRate.EQ.0.0) THEN
        //     Node(SteamInletNode)%MassFlowRate         = 0.0
        //     Node(SteamInletNode)%MassFlowRateMinAvail = 0.0
        //     Node(SteamOutletNode)%MassFlowRateMinAvail= 0.0
        //   END IF

        if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
            state.dataLoopNodes->Node(AirOutletNode).CO2 = state.dataLoopNodes->Node(AirInletNode).CO2;
        }
        if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
            state.dataLoopNodes->Node(AirOutletNode).GenContam = state.dataLoopNodes->Node(AirInletNode).GenContam;
        }
    }

    // End of Update subroutines for the SteamCoil Module

    // Beginning of Reporting subroutines for the SteamCoil Module

    void ReportSteamCoil(EnergyPlusData &state, int const CoilNum)
    {
        // SUBROUTINE INFORMATION:
        //   AUTHOR         Rahul Chillar
        //   DATE WRITTEN   Jan 2005
        //   MODIFIED       na
        //   RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine updates the report variable for the coils.

        // Report the SteamCoil energy from this component
        state.dataSteamCoils->SteamCoil(CoilNum).TotSteamHeatingCoilEnergy =
            state.dataSteamCoils->SteamCoil(CoilNum).TotSteamHeatingCoilRate * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
    }

    // End of Reporting subroutines for the SteamCoil Module

    // Utility subroutines for the SteamCoil Module

    int GetSteamCoilIndex(EnergyPlusData &state,
                          std::string const &CoilType, // must match coil types in this module
                          std::string const &CoilName, // must match coil names for the coil type
                          bool &ErrorsFound            // set to true if problem
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         R. Raustad
        //       DATE WRITTEN   August 2007
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the index for the given coil and returns it.  If
        // incorrect coil type or name is given, ErrorsFound is returned as true and node number is returned
        // as zero.

        // Return value
        int IndexNum; // returned air inlet node number of matched coil

        // Obtains and Allocates SteamCoil related parameters from input file
        if (state.dataSteamCoils->GetSteamCoilsInputFlag) { // First time subroutine has been entered
            GetSteamCoilInput(state);
            state.dataSteamCoils->GetSteamCoilsInputFlag = false;
        }

        if (CoilType == "COIL:HEATING:STEAM") {
            IndexNum = UtilityRoutines::FindItemInList(CoilName, state.dataSteamCoils->SteamCoil);
        } else {
            IndexNum = 0;
        }

        if (IndexNum == 0) {
            ShowSevereError(state, "GetSteamCoilIndex: Could not find CoilType=\"" + CoilType + "\" with Name=\"" + CoilName + "\"");
            ErrorsFound = true;
        }

        return IndexNum;
    }

    void CheckSteamCoilSchedule(
        EnergyPlusData &state, [[maybe_unused]] std::string const &CompType, std::string_view CompName, Real64 &Value, int &CompIndex)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   March 2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Gets the correct schedule value for this coil

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int CoilNum;

        // Obtains and Allocates SteamCoil related parameters from input file
        if (state.dataSteamCoils->GetSteamCoilsInputFlag) { // First time subroutine has been entered
            GetSteamCoilInput(state);
            state.dataSteamCoils->GetSteamCoilsInputFlag = false;
        }

        // Find the correct Coil number
        if (CompIndex == 0) {
            CoilNum = UtilityRoutines::FindItemInList(CompName, state.dataSteamCoils->SteamCoil);
            if (CoilNum == 0) {
                ShowFatalError(state, "CheckSteamCoilSchedule: Coil not found=" + std::string{CompName});
            }
            CompIndex = CoilNum;
            Value = GetCurrentScheduleValue(state, state.dataSteamCoils->SteamCoil(CoilNum).SchedPtr); // not scheduled?
        } else {
            CoilNum = CompIndex;
            if (CoilNum > state.dataSteamCoils->NumSteamCoils || CoilNum < 1) {
                ShowFatalError(state,
                               format("SimulateSteamCoilComponents: Invalid CompIndex passed={}, Number of Steam Coils={}, Coil name={}",
                                      CoilNum,
                                      state.dataSteamCoils->NumSteamCoils,
                                      CompName));
            }
            if (CompName != state.dataSteamCoils->SteamCoil(CoilNum).Name) {
                ShowFatalError(state,
                               format("SimulateSteamCoilComponents: Invalid CompIndex passed={}, Coil name={}, stored Coil Name for that index={}",
                                      CoilNum,
                                      CompName,
                                      state.dataSteamCoils->SteamCoil(CoilNum).Name));
            }
            Value = GetCurrentScheduleValue(state, state.dataSteamCoils->SteamCoil(CoilNum).SchedPtr); // not scheduled?
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
        auto &ErrCount = state.dataSteamCoils->ErrCount;

        // Obtains and Allocates SteamCoil related parameters from input file
        if (state.dataSteamCoils->GetSteamCoilsInputFlag) { // First time subroutine has been entered
            GetSteamCoilInput(state);
            state.dataSteamCoils->GetSteamCoilsInputFlag = false;
        }

        if (UtilityRoutines::SameString(CoilType, "Coil:Heating:Steam")) {
            WhichCoil = UtilityRoutines::FindItem(CoilName, state.dataSteamCoils->SteamCoil);
            if (WhichCoil != 0) {
                // coil does not specify MaxWaterFlowRate
                MaxWaterFlowRate = 0.0;
                ShowRecurringWarningErrorAtEnd(state, "Requested Max Water Flow Rate from COIL:Heating:Steam N/A", ErrCount);
            }
        } else {
            WhichCoil = 0;
        }

        if (WhichCoil == 0) {
            ShowSevereError(state, "GetCoilMaxWaterFlowRate: Could not find CoilType=\"" + CoilType + "\" with Name=\"" + CoilName + "\"");
            ErrorsFound = true;
            MaxWaterFlowRate = -1000.0;
        }

        return MaxWaterFlowRate;
    }

    Real64 GetCoilMaxSteamFlowRate(EnergyPlusData &state,
                                   int const CoilIndex, // must match coil types in this module
                                   bool &ErrorsFound    // set to true if problem
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         R. Raustad
        //       DATE WRITTEN   August 2007
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the max steam flow rate for the given coil and returns it.  If
        // incorrect coil type or name is given, ErrorsFound is returned as true and flow rate is returned
        // as zero.

        // Return value
        Real64 MaxSteamFlowRate; // returned max steam flow rate of matched coil

        // Obtains and Allocates SteamCoil related parameters from input file
        if (state.dataSteamCoils->GetSteamCoilsInputFlag) { // First time subroutine has been entered
            GetSteamCoilInput(state);
            state.dataSteamCoils->GetSteamCoilsInputFlag = false;
        }

        if (CoilIndex == 0) {
            ShowSevereError(state, "GetCoilMaxSteamFlowRate: Could not find CoilType = \"Coil:Heating:Steam\"");
            ErrorsFound = true;
            MaxSteamFlowRate = 0.0;
        } else {
            MaxSteamFlowRate = state.dataSteamCoils->SteamCoil(CoilIndex).MaxSteamVolFlowRate;
        }

        return MaxSteamFlowRate;
    }

    int GetCoilAirInletNode(EnergyPlusData &state,
                            int const CoilIndex,         // must match coil types in this module
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
        // This function looks up the air inlet node number for the given coil and returns it.  If
        // incorrect coil type or name is given, ErrorsFound is returned as true and node number is returned
        // as zero.

        // Return value
        int NodeNumber; // returned air inlet node number of matched coil

        // Obtains and Allocates SteamCoil related parameters from input file
        if (state.dataSteamCoils->GetSteamCoilsInputFlag) { // First time subroutine has been entered
            GetSteamCoilInput(state);
            state.dataSteamCoils->GetSteamCoilsInputFlag = false;
        }

        if (CoilIndex == 0) {
            ShowSevereError(state, "GetCoilAirInletNode: Could not find CoilType = \"Coil:Heating:Steam\" with Name = " + CoilName);
            ErrorsFound = true;
            NodeNumber = 0;
        } else {
            NodeNumber = state.dataSteamCoils->SteamCoil(CoilIndex).AirInletNodeNum;
        }

        return NodeNumber;
    }

    int GetCoilAirOutletNode(EnergyPlusData &state,
                             int const CoilIndex,         // must match coil types in this module
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
        // This function looks up the air outlet node number for the given coil and returns it.  If
        // incorrect coil type or name is given, ErrorsFound is returned as true and node number is returned
        // as zero.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Return value
        int NodeNumber; // returned air inlet node number of matched coil

        // Obtains and Allocates SteamCoil related parameters from input file
        if (state.dataSteamCoils->GetSteamCoilsInputFlag) { // First time subroutine has been entered
            GetSteamCoilInput(state);
            state.dataSteamCoils->GetSteamCoilsInputFlag = false;
        }

        if (CoilIndex == 0) {
            ShowSevereError(state, "GetCoilAirOutletNode: Could not find CoilType = \"Coil:Heating:Steam\" with Name = " + CoilName);
            ErrorsFound = true;
            NodeNumber = 0;
        } else {
            NodeNumber = state.dataSteamCoils->SteamCoil(CoilIndex).AirOutletNodeNum;
        }

        return NodeNumber;
    }

    int GetCoilAirOutletNode(EnergyPlusData &state,
                             std::string const &CoilType,       // must match coil types in this module
                             std::string const &CoilName,       // must match coil names for the coil type
                             [[maybe_unused]] bool &ErrorsFound // set to true if problem
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         R. Raustad
        //       DATE WRITTEN   July 2007
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the air outlet node number for the given coil and returns it.  If
        // incorrect coil type or name is given, ErrorsFound is returned as true and node number is returned
        // as zero.

        // Return value
        int NodeNumber; // returned air inlet node number of matched coil

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int IndexNum; // returned air inlet node number of matched coil

        // Obtains and Allocates SteamCoil related parameters from input file
        if (state.dataSteamCoils->GetSteamCoilsInputFlag) { // First time subroutine has been entered
            GetSteamCoilInput(state);
            state.dataSteamCoils->GetSteamCoilsInputFlag = false;
        }

        if (UtilityRoutines::SameString(CoilType, "Coil:Heating:Steam")) {
            IndexNum = UtilityRoutines::FindItem(CoilName, state.dataSteamCoils->SteamCoil);
        } else {
            IndexNum = 0;
        }

        if (IndexNum == 0) {
            NodeNumber = 0;
        } else {
            NodeNumber = state.dataSteamCoils->SteamCoil(IndexNum).AirOutletNodeNum;
        }

        return NodeNumber;
    }

    int GetCoilSteamInletNode(EnergyPlusData &state,
                              int const CoilIndex,         // must match coil types in this module
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
        // This function looks up the steam inlet node number for the given coil and returns it.  If
        // incorrect coil type or name is given, ErrorsFound is returned as true and node number is returned
        // as zero.

        // Return value
        int NodeNumber; // returned air inlet node number of matched coil

        // Obtains and Allocates SteamCoil related parameters from input file
        if (state.dataSteamCoils->GetSteamCoilsInputFlag) { // First time subroutine has been entered
            GetSteamCoilInput(state);
            state.dataSteamCoils->GetSteamCoilsInputFlag = false;
        }

        if (CoilIndex == 0) {
            ShowSevereError(state, "GetCoilSteamInletNode: Could not find CoilType = \"Coil:Heating:Steam\" with Name = " + CoilName);
            ErrorsFound = true;
            NodeNumber = 0;
        } else {
            NodeNumber = state.dataSteamCoils->SteamCoil(CoilIndex).SteamInletNodeNum;
        }

        return NodeNumber;
    }

    int GetCoilSteamInletNode(EnergyPlusData &state,
                              std::string const &CoilType, // must match coil types in this module
                              std::string const &CoilName, // must match coil names for the coil type
                              bool &ErrorsFound            // set to true if problem
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         L. Lawrie (based on R. Raustad)
        //       DATE WRITTEN   June 2008
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the steam inlet node number for the given coil and returns it.  If
        // incorrect coil type or name is given, ErrorsFound is returned as true and node number is returned
        // as zero.

        // Return value
        int NodeNumber; // returned air inlet node number of matched coil

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int IndexNum; // returned air inlet node number of matched coil

        // Obtains and Allocates SteamCoil related parameters from input file
        if (state.dataSteamCoils->GetSteamCoilsInputFlag) { // First time subroutine has been entered
            GetSteamCoilInput(state);
            state.dataSteamCoils->GetSteamCoilsInputFlag = false;
        }

        if (UtilityRoutines::SameString(CoilType, "Coil:Heating:Steam")) {
            IndexNum = UtilityRoutines::FindItem(CoilName, state.dataSteamCoils->SteamCoil);
        } else {
            IndexNum = 0;
        }

        if (IndexNum == 0) {
            ShowSevereError(state, "GetCoilSteamInletNode: Could not find CoilType = \"Coil:Heating:Steam\" with Name = " + CoilName);
            ErrorsFound = true;
            NodeNumber = 0;
        } else {
            NodeNumber = state.dataSteamCoils->SteamCoil(IndexNum).SteamInletNodeNum;
        }

        return NodeNumber;
    }

    int GetCoilSteamOutletNode(EnergyPlusData &state,
                               int const CoilIndex,         // must match coil types in this module
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
        // This function looks up the steam inlet node number for the given coil and returns it.  If
        // incorrect coil type or name is given, ErrorsFound is returned as true and node number is returned
        // as zero.

        // Return value
        int NodeNumber; // returned air inlet node number of matched coil

        // Obtains and Allocates SteamCoil related parameters from input file
        if (state.dataSteamCoils->GetSteamCoilsInputFlag) { // First time subroutine has been entered
            GetSteamCoilInput(state);
            state.dataSteamCoils->GetSteamCoilsInputFlag = false;
        }

        if (CoilIndex == 0) {
            ShowSevereError(state, "GetCoilSteamInletNode: Could not find CoilType = \"Coil:Heating:Steam\" with Name = " + CoilName);
            ErrorsFound = true;
            NodeNumber = 0;
        } else {
            NodeNumber = state.dataSteamCoils->SteamCoil(CoilIndex).SteamOutletNodeNum;
        }

        return NodeNumber;
    }

    int GetCoilSteamOutletNode(EnergyPlusData &state,
                               std::string const &CoilType, // must match coil types in this module
                               std::string const &CoilName, // must match coil names for the coil type
                               bool &ErrorsFound            // set to true if problem
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         L. Lawrie (based on R. Raustad)
        //       DATE WRITTEN   June 2008
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the steam inlet node number for the given coil and returns it.  If
        // incorrect coil type or name is given, ErrorsFound is returned as true and node number is returned
        // as zero.

        // Return value
        int NodeNumber; // returned air inlet node number of matched coil

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int IndexNum; // returned air inlet node number of matched coil

        // Obtains and Allocates SteamCoil related parameters from input file
        if (state.dataSteamCoils->GetSteamCoilsInputFlag) { // First time subroutine has been entered
            GetSteamCoilInput(state);
            state.dataSteamCoils->GetSteamCoilsInputFlag = false;
        }

        if (UtilityRoutines::SameString(CoilType, "Coil:Heating:Steam")) {
            IndexNum = UtilityRoutines::FindItem(CoilName, state.dataSteamCoils->SteamCoil);
        } else {
            IndexNum = 0;
        }

        if (IndexNum == 0) {
            ShowSevereError(state, "GetCoilSteamInletNode: Could not find CoilType = \"Coil:Heating:Steam\" with Name = " + CoilName);
            ErrorsFound = true;
            NodeNumber = 0;
        } else {
            NodeNumber = state.dataSteamCoils->SteamCoil(IndexNum).SteamOutletNodeNum;
        }

        return NodeNumber;
    }

    Real64 GetCoilCapacity(EnergyPlusData &state,
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
        // This function looks up the steam coils operating capacity and returns it.  If
        // incorrect coil type or name is given, ErrorsFound is returned as true and node number is returned
        // as zero.

        // Return value
        Real64 Capacity; // returned operating capacity of matched coil (W)

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int WhichCoil;

        // Obtains and Allocates SteamCoil related parameters from input file
        if (state.dataSteamCoils->GetSteamCoilsInputFlag) { // First time subroutine has been entered
            GetSteamCoilInput(state);
            state.dataSteamCoils->GetSteamCoilsInputFlag = false;
        }

        if (UtilityRoutines::SameString(CoilType, "Coil:Heating:Steam")) {
            WhichCoil = UtilityRoutines::FindItem(CoilName, state.dataSteamCoils->SteamCoil);
            if (WhichCoil != 0) {
                // coil does not specify MaxWaterFlowRate
                Capacity = state.dataSteamCoils->SteamCoil(WhichCoil).OperatingCapacity;
            }
        } else {
            WhichCoil = 0;
        }

        if (WhichCoil == 0) {
            ShowSevereError(state, "GetCoilSteamInletNode: Could not find CoilType=\"" + CoilType + "\" with Name=\"" + CoilName + "\"");
            ErrorsFound = true;
            Capacity = 0.0;
        }

        return Capacity;
    }

    int GetTypeOfCoil(EnergyPlusData &state,
                      int const CoilIndex,         // must match coil types in this module
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
        // This function looks up the steam coils operating capacity and returns it.  If
        // incorrect coil type or name is given, ErrorsFound is returned as true and node number is returned
        // as zero.

        // Return value
        int TypeOfCoil; // returned coil type of matched coil (W)

        // Locals
        // FUNCTION ARGUMENT DEFINITIONS:
        // 1 = TemperatureSetPointControl
        // 3 = ZoneLoadControl

        // Obtains and Allocates SteamCoil related parameters from input file
        if (state.dataSteamCoils->GetSteamCoilsInputFlag) { // First time subroutine has been entered
            GetSteamCoilInput(state);
            state.dataSteamCoils->GetSteamCoilsInputFlag = false;
        }

        if (CoilIndex == 0) {
            ShowSevereError(state, "GetCoilSteamInletNode: Could not find CoilType = \"Coil:Heating:Steam\" with Name = " + CoilName);
            ErrorsFound = true;
            TypeOfCoil = 0;
        } else {
            TypeOfCoil = state.dataSteamCoils->SteamCoil(CoilIndex).TypeOfCoil;
        }

        return TypeOfCoil;
    }

    int GetSteamCoilControlNodeNum(EnergyPlusData &state,
                                   std::string const &CoilType, // must match coil types in this module
                                   std::string const &CoilName, // must match coil names for the coil type
                                   bool &ErrorFlag              // set to true if problem
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         B. Nigusse, FSEC
        //       DATE WRITTEN   January 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the steam coils and returns the steam control node number.  If
        // incorrect coil type or name is given, ErrorsFound is returned as true and node number is returned
        // as zero.

        // Return value
        int NodeNumber; // returned node number of matched coil

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int WhichCoil;

        // Obtains and Allocates SteamCoil related parameters from input file
        if (state.dataSteamCoils->GetSteamCoilsInputFlag) { // First time subroutine has been entered
            GetSteamCoilInput(state);
            state.dataSteamCoils->GetSteamCoilsInputFlag = false;
        }

        WhichCoil = 0;
        NodeNumber = 0;
        if (UtilityRoutines::SameString(CoilType, "Coil:Heating:Steam")) {
            WhichCoil = UtilityRoutines::FindItem(CoilName, state.dataSteamCoils->SteamCoil);
            if (WhichCoil != 0) {
                NodeNumber = state.dataSteamCoils->SteamCoil(WhichCoil).TempSetPointNodeNum;
            }
        } else {
            WhichCoil = 0;
        }

        if (WhichCoil == 0) {
            ShowSevereError(state, "GetSteamCoilControlNodeNum: Could not find Coil, Type=\"" + CoilType + "\" Name=\"" + CoilName + "\"");
            ErrorFlag = true;
            NodeNumber = 0;
        }

        return NodeNumber;
    }

    int GetSteamCoilAvailScheduleIndex(EnergyPlusData &state,
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
        if (state.dataSteamCoils->GetSteamCoilsInputFlag) { // First time subroutine has been entered
            GetSteamCoilInput(state);
            state.dataSteamCoils->GetSteamCoilsInputFlag = false;
        }

        WhichCoil = 0;
        AvailSchIndex = 0;

        if (UtilityRoutines::SameString(CoilType, "Coil:Heating:Steam")) {
            WhichCoil = UtilityRoutines::FindItem(CoilName, state.dataSteamCoils->SteamCoil);
            if (WhichCoil != 0) {
                AvailSchIndex = state.dataSteamCoils->SteamCoil(WhichCoil).SchedPtr;
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

    void SetSteamCoilData(EnergyPlusData &state,
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

        if (state.dataSteamCoils->GetSteamCoilsInputFlag) {
            GetSteamCoilInput(state);
            state.dataSteamCoils->GetSteamCoilsInputFlag = false;
        }

        if (CoilNum <= 0 || CoilNum > state.dataSteamCoils->NumSteamCoils) {
            ShowSevereError(state,
                            format("SetHeatingCoilData: called with heating coil Number out of range={} should be >0 and <{}",
                                   CoilNum,
                                   state.dataSteamCoils->NumSteamCoils));
            ErrorsFound = true;
            return;
        }

        if (present(DesiccantRegenerationCoil)) {
            state.dataSteamCoils->SteamCoil(CoilNum).DesiccantRegenerationCoil = DesiccantRegenerationCoil;
        }

        if (present(DesiccantDehumIndex)) {
            state.dataSteamCoils->SteamCoil(CoilNum).DesiccantDehumNum = DesiccantDehumIndex;
        }
    }
    // End of Utility subroutines for the SteamCoil Module

} // namespace SteamCoils

} // namespace EnergyPlus
