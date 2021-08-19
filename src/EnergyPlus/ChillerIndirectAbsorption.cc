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
#include <cassert>
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EnergyPlus/Autosizing/Base.hh>
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/ChillerIndirectAbsorption.hh>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataBranchAirLoopPlant.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/EMSManager.hh>
#include <EnergyPlus/FaultsManager.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/Plant/PlantLocation.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus::ChillerIndirectAbsorption {

// MODULE INFORMATION:
//       AUTHOR         R. Raustad (FSEC)
//       DATE WRITTEN   May 2008
//       MODIFIED       na
//       RE-ENGINEERED  na

// PURPOSE OF THIS MODULE:
// This module simulates the performance of the revised BLAST
// absorbers. New curve objects are included.

// METHODOLOGY EMPLOYED:
// Once the PlantLoopManager determines that the revised BLAST absorber
// is available to meet a loop cooling demand, it calls SimIndirectAbsorber
// which in turn calls the appropriate Indirect Absorption Chiller model.
// All Absorption Chiller models are based on a polynomial fit of Absorber
// performance data.

// REFERENCES:
// 1. BLAST Users Manual

// OTHER NOTES:
// Manufacturers performance data can be used to generate the coefficients for the model.

static constexpr std::string_view calcChillerAbsorptionIndirect("CALC Chiller:Absorption:Indirect ");
auto constexpr waterIndex(1);
static constexpr std::string_view fluidNameSteam = "STEAM";
static constexpr std::string_view fluidNameWater = "WATER";

PlantComponent *IndirectAbsorberSpecs::factory(EnergyPlusData &state, std::string const &objectName)
{
    // Process the input data
    if (state.dataChillerIndirectAbsorption->GetInput) {
        GetIndirectAbsorberInput(state);
        state.dataChillerIndirectAbsorption->GetInput = false;
    }
    // Now look for this particular object
    for (auto &thisAbs : state.dataChillerIndirectAbsorption->IndirectAbsorber) {
        if (thisAbs.Name == objectName) {
            return &thisAbs;
        }
    }
    // If we didn't find it, fatal
    ShowFatalError(state, "LocalIndirectAbsorptionChillerFactory: Error getting inputs for object named: " + objectName); // LCOV_EXCL_LINE
    // Shut up the compiler
    return nullptr; // LCOV_EXCL_LINE
}

void IndirectAbsorberSpecs::simulate(
    EnergyPlusData &state, const PlantLocation &calledFromLocation, bool FirstHVACIteration, Real64 &CurLoad, bool RunFlag)
{
    if (calledFromLocation.loopNum == this->CWLoopNum) {

        this->initialize(state, RunFlag, CurLoad);
        this->calculate(state, CurLoad, RunFlag);
        this->updateRecords(state, CurLoad, RunFlag);

    } else if (calledFromLocation.loopNum == this->CDLoopNum) {
        // Called from non-dominant condenser water connection loop side
        PlantUtilities::UpdateChillerComponentCondenserSide(state,
                                                            calledFromLocation.loopNum,
                                                            calledFromLocation.loopSideNum,
                                                            DataPlant::TypeOf_Chiller_Indirect_Absorption,
                                                            this->CondInletNodeNum,
                                                            this->CondOutletNodeNum,
                                                            this->Report.QCond,
                                                            this->Report.CondInletTemp,
                                                            this->Report.CondOutletTemp,
                                                            this->Report.Condmdot,
                                                            FirstHVACIteration);

    } else if (calledFromLocation.loopNum == this->GenLoopNum) {
        // Called from non-dominant generator hot water or steam connection loop side
        PlantUtilities::UpdateAbsorberChillerComponentGeneratorSide(state,
                                                                    calledFromLocation.loopNum,
                                                                    calledFromLocation.loopSideNum,
                                                                    DataPlant::TypeOf_Chiller_Indirect_Absorption,
                                                                    this->GeneratorInletNodeNum,
                                                                    this->GeneratorOutletNodeNum,
                                                                    this->GenHeatSourceType,
                                                                    this->Report.QGenerator,
                                                                    this->Report.SteamMdot,
                                                                    FirstHVACIteration);

    } else {
        ShowFatalError(state,
                       format("SimIndirectAbsorber: Invalid LoopNum passed={}, Unit name={}, stored chilled water loop={}, stored condenser "
                              "water loop={}, stored generator loop={}",
                              calledFromLocation.loopNum,
                              this->Name,
                              this->CWLoopNum,
                              this->CDLoopNum,
                              this->GenLoopNum));
    }
}

void IndirectAbsorberSpecs::getDesignCapacities(
    [[maybe_unused]] EnergyPlusData &state, const PlantLocation &calledFromLocation, Real64 &MaxLoad, Real64 &MinLoad, Real64 &OptLoad)
{
    if (calledFromLocation.loopNum == this->CWLoopNum) {
        MinLoad = this->NomCap * this->MinPartLoadRat;
        MaxLoad = this->NomCap * this->MaxPartLoadRat;
        OptLoad = this->NomCap * this->OptPartLoadRat;
    } else {
        MinLoad = 0.0;
        MaxLoad = 0.0;
        OptLoad = 0.0;
    }
}

void IndirectAbsorberSpecs::getSizingFactor(Real64 &sizFac)
{
    sizFac = this->SizFac;
}

void IndirectAbsorberSpecs::onInitLoopEquip(EnergyPlusData &state, const PlantLocation &calledFromLocation)
{
    bool runFlag = true;
    Real64 myLoad = 0.0;

    this->initialize(state, runFlag, myLoad);

    if (calledFromLocation.loopNum == this->CWLoopNum) {
        this->sizeChiller(state); // only size when called from chilled water loop
    }
}

void GetIndirectAbsorberInput(EnergyPlusData &state)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR:          R. Raustad (FSEC)
    //       DATE WRITTEN:    May 2008

    // PURPOSE OF THIS SUBROUTINE:
    // This routine will get the input
    // required by the Indirect Absorption chiller models as shown below:

    // METHODOLOGY EMPLOYED:
    // EnergyPlus input processor

    static constexpr std::string_view RoutineName("GetIndirectAbsorberInput: "); // include trailing blank space

    int AbsorberNum = 0; // Absorber counter
    int NumAlphas = 0;   // Number of elements in the alpha array
    int NumNums = 0;     // Number of elements in the numeric array
    int IOStat = 0;      // IO Status when calling get input subroutine
    bool ErrorsFound(false);

    state.dataIPShortCut->cCurrentModuleObject = "Chiller:Absorption:Indirect";
    state.dataChillerIndirectAbsorption->NumIndirectAbsorbers =
        state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, state.dataIPShortCut->cCurrentModuleObject);

    if (state.dataChillerIndirectAbsorption->NumIndirectAbsorbers <= 0) {
        ShowSevereError(state, "No " + state.dataIPShortCut->cCurrentModuleObject + " equipment specified in input file");
        // See if load distribution manager has already gotten the input
        ErrorsFound = true;
    }

    if (allocated(state.dataChillerIndirectAbsorption->IndirectAbsorber)) return;

    state.dataChillerIndirectAbsorption->IndirectAbsorber.allocate(state.dataChillerIndirectAbsorption->NumIndirectAbsorbers);

    // LOAD ARRAYS WITH BLAST CURVE FIT Absorber DATA
    for (AbsorberNum = 1; AbsorberNum <= state.dataChillerIndirectAbsorption->NumIndirectAbsorbers; ++AbsorberNum) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 state.dataIPShortCut->cCurrentModuleObject,
                                                                 AbsorberNum,
                                                                 state.dataIPShortCut->cAlphaArgs,
                                                                 NumAlphas,
                                                                 state.dataIPShortCut->rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);
        UtilityRoutines::IsNameEmpty(state, state.dataIPShortCut->cAlphaArgs(1), state.dataIPShortCut->cCurrentModuleObject, ErrorsFound);

        // ErrorsFound will be set to True if problem was found, left untouched otherwise
        GlobalNames::VerifyUniqueChillerName(state,
                                             state.dataIPShortCut->cCurrentModuleObject,
                                             state.dataIPShortCut->cAlphaArgs(1),
                                             ErrorsFound,
                                             state.dataIPShortCut->cCurrentModuleObject + " Name");

        auto &thisChiller = state.dataChillerIndirectAbsorption->IndirectAbsorber(AbsorberNum);
        thisChiller.Name = state.dataIPShortCut->cAlphaArgs(1);
        thisChiller.NomCap = state.dataIPShortCut->rNumericArgs(1);
        if (thisChiller.NomCap == DataSizing::AutoSize) {
            thisChiller.NomCapWasAutoSized = true;
        }
        thisChiller.NomPumpPower = state.dataIPShortCut->rNumericArgs(2);
        if (thisChiller.NomPumpPower == DataSizing::AutoSize) {
            thisChiller.NomPumpPowerWasAutoSized = true;
        }
        if (state.dataIPShortCut->rNumericArgs(1) == 0.0) {
            ShowSevereError(state, format("Invalid {}={:.2R}", state.dataIPShortCut->cNumericFieldNames(1), state.dataIPShortCut->rNumericArgs(1)));
            ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
            ErrorsFound = true;
        }
        // Assign Node Numbers to specified nodes
        thisChiller.EvapInletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                                           state.dataIPShortCut->cAlphaArgs(2),
                                                                           ErrorsFound,
                                                                           state.dataIPShortCut->cCurrentModuleObject,
                                                                           state.dataIPShortCut->cAlphaArgs(1),
                                                                           DataLoopNode::NodeFluidType::Water,
                                                                           DataLoopNode::NodeConnectionType::Inlet,
                                                                           NodeInputManager::compFluidStream::Primary,
                                                                           DataLoopNode::ObjectIsNotParent);
        thisChiller.EvapOutletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                                            state.dataIPShortCut->cAlphaArgs(3),
                                                                            ErrorsFound,
                                                                            state.dataIPShortCut->cCurrentModuleObject,
                                                                            state.dataIPShortCut->cAlphaArgs(1),
                                                                            DataLoopNode::NodeFluidType::Water,
                                                                            DataLoopNode::NodeConnectionType::Outlet,
                                                                            NodeInputManager::compFluidStream::Primary,
                                                                            DataLoopNode::ObjectIsNotParent);
        BranchNodeConnections::TestCompSet(state,
                                           state.dataIPShortCut->cCurrentModuleObject,
                                           state.dataIPShortCut->cAlphaArgs(1),
                                           state.dataIPShortCut->cAlphaArgs(2),
                                           state.dataIPShortCut->cAlphaArgs(3),
                                           "Chilled Water Nodes");

        thisChiller.CondInletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                                           state.dataIPShortCut->cAlphaArgs(4),
                                                                           ErrorsFound,
                                                                           state.dataIPShortCut->cCurrentModuleObject,
                                                                           state.dataIPShortCut->cAlphaArgs(1),
                                                                           DataLoopNode::NodeFluidType::Water,
                                                                           DataLoopNode::NodeConnectionType::Inlet,
                                                                           NodeInputManager::compFluidStream::Secondary,
                                                                           DataLoopNode::ObjectIsNotParent);
        thisChiller.CondOutletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                                            state.dataIPShortCut->cAlphaArgs(5),
                                                                            ErrorsFound,
                                                                            state.dataIPShortCut->cCurrentModuleObject,
                                                                            state.dataIPShortCut->cAlphaArgs(1),
                                                                            DataLoopNode::NodeFluidType::Water,
                                                                            DataLoopNode::NodeConnectionType::Outlet,
                                                                            NodeInputManager::compFluidStream::Secondary,
                                                                            DataLoopNode::ObjectIsNotParent);
        BranchNodeConnections::TestCompSet(state,
                                           state.dataIPShortCut->cCurrentModuleObject,
                                           state.dataIPShortCut->cAlphaArgs(1),
                                           state.dataIPShortCut->cAlphaArgs(4),
                                           state.dataIPShortCut->cAlphaArgs(5),
                                           "Condenser (not tested) Nodes");

        thisChiller.GeneratorInputCurvePtr = CurveManager::GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(7));
        if (thisChiller.GeneratorInputCurvePtr > 0) {
            // Verify Curve Object, only legal types are Quadratic or Cubic
            ErrorsFound |= CurveManager::CheckCurveDims(state,
                                                        thisChiller.GeneratorInputCurvePtr,         // Curve index
                                                        {1},                                        // Valid dimensions
                                                        RoutineName,                                // Routine name
                                                        state.dataIPShortCut->cCurrentModuleObject, // Object Type
                                                        thisChiller.Name,                           // Object Name
                                                        state.dataIPShortCut->cAlphaFieldNames(7)); // Field Name
        }

        thisChiller.PumpPowerCurvePtr = CurveManager::GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(8));
        if (thisChiller.PumpPowerCurvePtr > 0) {
            // Verify Curve Object, only legal types are Quadratic or Cubic
            ErrorsFound |= CurveManager::CheckCurveDims(state,
                                                        thisChiller.PumpPowerCurvePtr,              // Curve index
                                                        {1},                                        // Valid dimensions
                                                        RoutineName,                                // Routine name
                                                        state.dataIPShortCut->cCurrentModuleObject, // Object Type
                                                        thisChiller.Name,                           // Object Name
                                                        state.dataIPShortCut->cAlphaFieldNames(8)); // Field Name
        }

        if (NumAlphas > 15) {
            if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(16), "HotWater") ||
                UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(16), "HotWater")) {
                thisChiller.GenHeatSourceType = DataLoopNode::NodeFluidType::Water;
                //       Default to Steam if left blank
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(16), fluidNameSteam) ||
                       state.dataIPShortCut->cAlphaArgs(16).empty()) {
                thisChiller.GenHeatSourceType = DataLoopNode::NodeFluidType::Steam;
            } else {
                ShowWarningError(state, state.dataIPShortCut->cCurrentModuleObject + ", Name=" + state.dataIPShortCut->cAlphaArgs(1));
                ShowContinueError(state, "...Generator heat source type must be Steam or Hot Water.");
                ShowContinueError(state, "...Entered generator heat source type = " + state.dataIPShortCut->cAlphaArgs(16));
                ErrorsFound = true;
            }
        } else {
            //     Default to Steam if not entered as input
            thisChiller.GenHeatSourceType = DataLoopNode::NodeFluidType::Steam;
        }

        if ((!state.dataIPShortCut->cAlphaArgs(9).empty()) && (!state.dataIPShortCut->cAlphaArgs(10).empty())) {
            thisChiller.GenInputOutputNodesUsed = true;
            if (thisChiller.GenHeatSourceType == DataLoopNode::NodeFluidType::Water) {
                thisChiller.GeneratorInletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                                                        state.dataIPShortCut->cAlphaArgs(9),
                                                                                        ErrorsFound,
                                                                                        state.dataIPShortCut->cCurrentModuleObject,
                                                                                        state.dataIPShortCut->cAlphaArgs(1),
                                                                                        DataLoopNode::NodeFluidType::Water,
                                                                                        DataLoopNode::NodeConnectionType::Inlet,
                                                                                        NodeInputManager::compFluidStream::Tertiary,
                                                                                        DataLoopNode::ObjectIsNotParent);
                thisChiller.GeneratorOutletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                                                         state.dataIPShortCut->cAlphaArgs(10),
                                                                                         ErrorsFound,
                                                                                         state.dataIPShortCut->cCurrentModuleObject,
                                                                                         state.dataIPShortCut->cAlphaArgs(1),
                                                                                         DataLoopNode::NodeFluidType::Water,
                                                                                         DataLoopNode::NodeConnectionType::Outlet,
                                                                                         NodeInputManager::compFluidStream::Tertiary,
                                                                                         DataLoopNode::ObjectIsNotParent);
                BranchNodeConnections::TestCompSet(state,
                                                   state.dataIPShortCut->cCurrentModuleObject,
                                                   state.dataIPShortCut->cAlphaArgs(1),
                                                   state.dataIPShortCut->cAlphaArgs(9),
                                                   state.dataIPShortCut->cAlphaArgs(10),
                                                   "Hot Water Nodes");
            } else {
                thisChiller.SteamFluidIndex = FluidProperties::FindRefrigerant(state, fluidNameSteam);
                thisChiller.GeneratorInletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                                                        state.dataIPShortCut->cAlphaArgs(9),
                                                                                        ErrorsFound,
                                                                                        state.dataIPShortCut->cCurrentModuleObject,
                                                                                        state.dataIPShortCut->cAlphaArgs(1),
                                                                                        DataLoopNode::NodeFluidType::Steam,
                                                                                        DataLoopNode::NodeConnectionType::Inlet,
                                                                                        NodeInputManager::compFluidStream::Tertiary,
                                                                                        DataLoopNode::ObjectIsNotParent);
                thisChiller.GeneratorOutletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                                                         state.dataIPShortCut->cAlphaArgs(10),
                                                                                         ErrorsFound,
                                                                                         state.dataIPShortCut->cCurrentModuleObject,
                                                                                         state.dataIPShortCut->cAlphaArgs(1),
                                                                                         DataLoopNode::NodeFluidType::Steam,
                                                                                         DataLoopNode::NodeConnectionType::Outlet,
                                                                                         NodeInputManager::compFluidStream::Tertiary,
                                                                                         DataLoopNode::ObjectIsNotParent);
                BranchNodeConnections::TestCompSet(state,
                                                   state.dataIPShortCut->cCurrentModuleObject,
                                                   state.dataIPShortCut->cAlphaArgs(1),
                                                   state.dataIPShortCut->cAlphaArgs(9),
                                                   state.dataIPShortCut->cAlphaArgs(10),
                                                   "Steam Nodes");
            }
        } else if (state.dataIPShortCut->cAlphaArgs(9).empty() != state.dataIPShortCut->cAlphaArgs(10).empty()) {
            ShowWarningError(state, state.dataIPShortCut->cCurrentModuleObject + ", Name=" + state.dataIPShortCut->cAlphaArgs(1));
            ShowContinueError(state, "...Generator fluid nodes must both be entered (or both left blank).");
            ShowContinueError(state, "...Generator fluid inlet node  = " + state.dataIPShortCut->cAlphaArgs(9));
            ShowContinueError(state, "...Generator fluid outlet node = " + state.dataIPShortCut->cAlphaArgs(10));
            ErrorsFound = true;
        } else {
            //     Generator fluid type must be steam if generator inlet/outlet nodes are not used
            if (thisChiller.GenHeatSourceType == DataLoopNode::NodeFluidType::Water) {
                ShowWarningError(state, state.dataIPShortCut->cCurrentModuleObject + ", Name=" + state.dataIPShortCut->cAlphaArgs(1));
                ShowContinueError(state, "...Generator fluid type must be Steam if generator inlet/outlet nodes are blank.");
                ShowContinueError(state, "...Generator fluid type is set to Steam and the simulation continues.");
                thisChiller.GenHeatSourceType = DataLoopNode::NodeFluidType::Steam;
            }
        }

        {
            auto const SELECT_CASE_var(state.dataIPShortCut->cAlphaArgs(6));
            if (SELECT_CASE_var == "CONSTANTFLOW") {
                thisChiller.FlowMode = DataPlant::FlowMode::Constant;
            } else if (SELECT_CASE_var == "LEAVINGSETPOINTMODULATED") {
                thisChiller.FlowMode = DataPlant::FlowMode::LeavingSetpointModulated;
            } else if (SELECT_CASE_var == "NOTMODULATED") {
                thisChiller.FlowMode = DataPlant::FlowMode::NotModulated;
            } else {
                ShowSevereError(state,
                                std::string{RoutineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) +
                                    "\",");
                ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(6) + '=' + state.dataIPShortCut->cAlphaArgs(6));
                ShowContinueError(state, "Available choices are ConstantFlow, NotModulated, or LeavingSetpointModulated");
                ShowContinueError(state, "Flow mode NotModulated is assumed and the simulation continues.");
                thisChiller.FlowMode = DataPlant::FlowMode::NotModulated;
            }
        }

        thisChiller.CapFCondenserTempPtr = CurveManager::GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(11));
        if (thisChiller.CapFCondenserTempPtr > 0) {
            // Verify Curve Object, only legal types are Quadratic or Cubic
            ErrorsFound |= CurveManager::CheckCurveDims(state,
                                                        thisChiller.CapFCondenserTempPtr,            // Curve index
                                                        {1},                                         // Valid dimensions
                                                        RoutineName,                                 // Routine name
                                                        state.dataIPShortCut->cCurrentModuleObject,  // Object Type
                                                        thisChiller.Name,                            // Object Name
                                                        state.dataIPShortCut->cAlphaFieldNames(11)); // Field Name
        }

        thisChiller.CapFEvaporatorTempPtr = CurveManager::GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(12));
        if (thisChiller.CapFEvaporatorTempPtr > 0) {
            // Verify Curve Object, only legal types are Quadratic or Cubic
            ErrorsFound |= CurveManager::CheckCurveDims(state,
                                                        thisChiller.CapFEvaporatorTempPtr,           // Curve index
                                                        {1},                                         // Valid dimensions
                                                        RoutineName,                                 // Routine name
                                                        state.dataIPShortCut->cCurrentModuleObject,  // Object Type
                                                        thisChiller.Name,                            // Object Name
                                                        state.dataIPShortCut->cAlphaFieldNames(12)); // Field Name
        }

        thisChiller.CapFGeneratorTempPtr = CurveManager::GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(13));
        if (thisChiller.CapFGeneratorTempPtr > 0) {
            // Verify Curve Object, only legal types are Quadratic or Cubic
            ErrorsFound |= CurveManager::CheckCurveDims(state,
                                                        thisChiller.CapFGeneratorTempPtr,            // Curve index
                                                        {1},                                         // Valid dimensions
                                                        RoutineName,                                 // Routine name
                                                        state.dataIPShortCut->cCurrentModuleObject,  // Object Type
                                                        thisChiller.Name,                            // Object Name
                                                        state.dataIPShortCut->cAlphaFieldNames(13)); // Field Name
        }

        thisChiller.HeatInputFCondTempPtr = CurveManager::GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(14));
        if (thisChiller.HeatInputFCondTempPtr > 0) {
            // Verify Curve Object, only legal types are Quadratic or Cubic
            ErrorsFound |= CurveManager::CheckCurveDims(state,
                                                        thisChiller.HeatInputFCondTempPtr,           // Curve index
                                                        {1},                                         // Valid dimensions
                                                        RoutineName,                                 // Routine name
                                                        state.dataIPShortCut->cCurrentModuleObject,  // Object Type
                                                        thisChiller.Name,                            // Object Name
                                                        state.dataIPShortCut->cAlphaFieldNames(14)); // Field Name
        }

        thisChiller.HeatInputFEvapTempPtr = CurveManager::GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(15));
        if (thisChiller.HeatInputFEvapTempPtr > 0) {
            // Verify Curve Object, only legal types are Quadratic or Cubic
            ErrorsFound |= CurveManager::CheckCurveDims(state,
                                                        thisChiller.HeatInputFEvapTempPtr,           // Curve index
                                                        {1},                                         // Valid dimensions
                                                        RoutineName,                                 // Routine name
                                                        state.dataIPShortCut->cCurrentModuleObject,  // Object Type
                                                        thisChiller.Name,                            // Object Name
                                                        state.dataIPShortCut->cAlphaFieldNames(15)); // Field Name
        }

        // Get remaining data
        thisChiller.MinPartLoadRat = state.dataIPShortCut->rNumericArgs(3);
        thisChiller.MaxPartLoadRat = state.dataIPShortCut->rNumericArgs(4);
        thisChiller.OptPartLoadRat = state.dataIPShortCut->rNumericArgs(5);
        thisChiller.TempDesCondIn = state.dataIPShortCut->rNumericArgs(6);
        thisChiller.MinCondInletTemp = state.dataIPShortCut->rNumericArgs(7);
        thisChiller.TempLowLimitEvapOut = state.dataIPShortCut->rNumericArgs(8);
        thisChiller.EvapVolFlowRate = state.dataIPShortCut->rNumericArgs(9);
        if (thisChiller.EvapVolFlowRate == DataSizing::AutoSize) {
            thisChiller.EvapVolFlowRateWasAutoSized = true;
        }
        thisChiller.CondVolFlowRate = state.dataIPShortCut->rNumericArgs(10);
        if (thisChiller.CondVolFlowRate == DataSizing::AutoSize) {
            thisChiller.CondVolFlowRateWasAutoSized = true;
        }
        if (NumNums > 10) {
            thisChiller.GeneratorVolFlowRate = state.dataIPShortCut->rNumericArgs(11);
            if (thisChiller.GeneratorVolFlowRate == DataSizing::AutoSize) {
                thisChiller.GeneratorVolFlowRateWasAutoSized = true;
            }
        }

        if (thisChiller.GeneratorVolFlowRate == 0.0 && thisChiller.GenHeatSourceType == DataLoopNode::NodeFluidType::Water) {
            ShowWarningError(state, state.dataIPShortCut->cCurrentModuleObject + ", Name=" + state.dataIPShortCut->cAlphaArgs(1));
            ShowContinueError(state, "...Generator water flow rate must be greater than 0 when absorber generator fluid type is hot water.");
            ErrorsFound = true;
        }

        if (NumNums > 11) {
            thisChiller.MinGeneratorInletTemp = state.dataIPShortCut->rNumericArgs(12);
        } else {
            thisChiller.MinGeneratorInletTemp = 0.0;
        }

        if (NumNums > 12) {
            thisChiller.GeneratorSubcool = state.dataIPShortCut->rNumericArgs(13);
        } else {
            thisChiller.GeneratorSubcool = 0.0;
        }

        if (NumNums > 13) {
            thisChiller.LoopSubcool = state.dataIPShortCut->rNumericArgs(14);
        } else {
            thisChiller.LoopSubcool = 0.0;
        }

        if (NumNums > 14) {
            thisChiller.SizFac = state.dataIPShortCut->rNumericArgs(15);
        } else {
            thisChiller.SizFac = 1.0;
        }
    }

    if (ErrorsFound) {
        ShowFatalError(state, "Errors found in getting Chiller:Absorption:Indirect");
    }
}

void IndirectAbsorberSpecs::setupOutputVars(EnergyPlusData &state)
{
    SetupOutputVariable(state,
                        "Chiller Electricity Rate",
                        OutputProcessor::Unit::W,
                        this->Report.PumpingPower,
                        OutputProcessor::SOVTimeStepType::System,
                        OutputProcessor::SOVStoreType::Average,
                        this->Name);

    SetupOutputVariable(state,
                        "Chiller Electricity Energy",
                        OutputProcessor::Unit::J,
                        this->Report.PumpingEnergy,
                        OutputProcessor::SOVTimeStepType::System,
                        OutputProcessor::SOVStoreType::Summed,
                        this->Name,
                        _,
                        "ELECTRICITY",
                        "Cooling",
                        _,
                        "Plant");

    SetupOutputVariable(state,
                        "Chiller Evaporator Cooling Rate",
                        OutputProcessor::Unit::W,
                        this->Report.QEvap,
                        OutputProcessor::SOVTimeStepType::System,
                        OutputProcessor::SOVStoreType::Average,
                        this->Name);

    SetupOutputVariable(state,
                        "Chiller Evaporator Cooling Energy",
                        OutputProcessor::Unit::J,
                        this->Report.EvapEnergy,
                        OutputProcessor::SOVTimeStepType::System,
                        OutputProcessor::SOVStoreType::Summed,
                        this->Name,
                        _,
                        "ENERGYTRANSFER",
                        "CHILLERS",
                        _,
                        "Plant");

    SetupOutputVariable(state,
                        "Chiller Evaporator Inlet Temperature",
                        OutputProcessor::Unit::C,
                        this->Report.EvapInletTemp,
                        OutputProcessor::SOVTimeStepType::System,
                        OutputProcessor::SOVStoreType::Average,
                        this->Name);

    SetupOutputVariable(state,
                        "Chiller Evaporator Outlet Temperature",
                        OutputProcessor::Unit::C,
                        this->Report.EvapOutletTemp,
                        OutputProcessor::SOVTimeStepType::System,
                        OutputProcessor::SOVStoreType::Average,
                        this->Name);

    SetupOutputVariable(state,
                        "Chiller Evaporator Mass Flow Rate",
                        OutputProcessor::Unit::kg_s,
                        this->Report.Evapmdot,
                        OutputProcessor::SOVTimeStepType::System,
                        OutputProcessor::SOVStoreType::Average,
                        this->Name);

    SetupOutputVariable(state,
                        "Chiller Condenser Heat Transfer Rate",
                        OutputProcessor::Unit::W,
                        this->Report.QCond,
                        OutputProcessor::SOVTimeStepType::System,
                        OutputProcessor::SOVStoreType::Average,
                        this->Name);

    SetupOutputVariable(state,
                        "Chiller Condenser Heat Transfer Energy",
                        OutputProcessor::Unit::J,
                        this->Report.CondEnergy,
                        OutputProcessor::SOVTimeStepType::System,
                        OutputProcessor::SOVStoreType::Summed,
                        this->Name,
                        _,
                        "ENERGYTRANSFER",
                        "HEATREJECTION",
                        _,
                        "Plant");

    SetupOutputVariable(state,
                        "Chiller Condenser Inlet Temperature",
                        OutputProcessor::Unit::C,
                        this->Report.CondInletTemp,
                        OutputProcessor::SOVTimeStepType::System,
                        OutputProcessor::SOVStoreType::Average,
                        this->Name);

    SetupOutputVariable(state,
                        "Chiller Condenser Outlet Temperature",
                        OutputProcessor::Unit::C,
                        this->Report.CondOutletTemp,
                        OutputProcessor::SOVTimeStepType::System,
                        OutputProcessor::SOVStoreType::Average,
                        this->Name);

    SetupOutputVariable(state,
                        "Chiller Condenser Mass Flow Rate",
                        OutputProcessor::Unit::kg_s,
                        this->Report.Condmdot,
                        OutputProcessor::SOVTimeStepType::System,
                        OutputProcessor::SOVStoreType::Average,
                        this->Name);

    if (this->GenHeatSourceType == DataLoopNode::NodeFluidType::Water) {
        SetupOutputVariable(state,
                            "Chiller Hot Water Consumption Rate",
                            OutputProcessor::Unit::W,
                            this->Report.QGenerator,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);

        SetupOutputVariable(state,
                            "Chiller Source Hot Water Energy",
                            OutputProcessor::Unit::J,
                            this->Report.GeneratorEnergy,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            this->Name,
                            _,
                            "EnergyTransfer",
                            "Cooling",
                            _,
                            "Plant");
    } else {
        if (this->GenInputOutputNodesUsed) {
            SetupOutputVariable(state,
                                "Chiller Source Steam Rate",
                                OutputProcessor::Unit::W,
                                this->Report.QGenerator,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                this->Name);

            SetupOutputVariable(state,
                                "Chiller Source Steam Energy",
                                OutputProcessor::Unit::J,
                                this->Report.GeneratorEnergy,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                this->Name,
                                _,
                                "PLANTLOOPHEATINGDEMAND",
                                "CHILLERS",
                                _,
                                "Plant");
        } else {
            SetupOutputVariable(state,
                                "Chiller Source Steam Rate",
                                OutputProcessor::Unit::W,
                                this->Report.QGenerator,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                this->Name);

            SetupOutputVariable(state,
                                "Chiller Source Steam Energy",
                                OutputProcessor::Unit::J,
                                this->Report.GeneratorEnergy,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                this->Name,
                                _,
                                fluidNameSteam,
                                "Cooling",
                                _,
                                "Plant");
        }
    }

    SetupOutputVariable(state,
                        "Chiller COP",
                        OutputProcessor::Unit::W_W,
                        this->Report.ActualCOP,
                        OutputProcessor::SOVTimeStepType::System,
                        OutputProcessor::SOVStoreType::Average,
                        this->Name);

    SetupOutputVariable(state,
                        "Chiller Part Load Ratio",
                        OutputProcessor::Unit::None,
                        this->Report.ChillerPartLoadRatio,
                        OutputProcessor::SOVTimeStepType::System,
                        OutputProcessor::SOVStoreType::Average,
                        this->Name);

    SetupOutputVariable(state,
                        "Chiller Cycling Ratio",
                        OutputProcessor::Unit::None,
                        this->Report.ChillerCyclingFrac,
                        OutputProcessor::SOVTimeStepType::System,
                        OutputProcessor::SOVStoreType::Average,
                        this->Name);

    SetupOutputVariable(state,
                        "Chiller Steam Heat Loss Rate",
                        OutputProcessor::Unit::W,
                        this->Report.LoopLoss,
                        OutputProcessor::SOVTimeStepType::System,
                        OutputProcessor::SOVStoreType::Average,
                        this->Name);

    if (state.dataGlobal->AnyEnergyManagementSystemInModel) {
        SetupEMSInternalVariable(state, "Chiller Nominal Capacity", this->Name, "[W]", this->NomCap);
    }
}

void IndirectAbsorberSpecs::oneTimeInit(EnergyPlusData &state)
{
    // Locate the chillers on the plant loops for later usage
    bool errFlag = false;
    PlantUtilities::ScanPlantLoopsForObject(state,
                                            this->Name,
                                            DataPlant::TypeOf_Chiller_Indirect_Absorption,
                                            this->CWLoopNum,
                                            this->CWLoopSideNum,
                                            this->CWBranchNum,
                                            this->CWCompNum,
                                            errFlag,
                                            this->TempLowLimitEvapOut,
                                            _,
                                            _,
                                            this->EvapInletNodeNum,
                                            _);

    PlantUtilities::ScanPlantLoopsForObject(state,
                                            this->Name,
                                            DataPlant::TypeOf_Chiller_Indirect_Absorption,
                                            this->CDLoopNum,
                                            this->CDLoopSideNum,
                                            this->CDBranchNum,
                                            this->CDCompNum,
                                            errFlag,
                                            _,
                                            _,
                                            _,
                                            this->CondInletNodeNum,
                                            _);
    PlantUtilities::InterConnectTwoPlantLoopSides(
        state, this->CWLoopNum, this->CWLoopSideNum, this->CDLoopNum, this->CDLoopSideNum, DataPlant::TypeOf_Chiller_Indirect_Absorption, true);

    if (this->GeneratorInletNodeNum > 0) {
        PlantUtilities::ScanPlantLoopsForObject(state,
                                                this->Name,
                                                DataPlant::TypeOf_Chiller_Indirect_Absorption,
                                                this->GenLoopNum,
                                                this->GenLoopSideNum,
                                                this->GenBranchNum,
                                                this->GenCompNum,
                                                errFlag,
                                                _,
                                                _,
                                                _,
                                                this->GeneratorInletNodeNum,
                                                _);
        PlantUtilities::InterConnectTwoPlantLoopSides(
            state, this->CWLoopNum, this->CWLoopSideNum, this->GenLoopNum, this->GenCompNum, DataPlant::TypeOf_Chiller_Indirect_Absorption, true);
    }

    if ((this->CondInletNodeNum > 0) && (this->GeneratorInletNodeNum > 0)) {
        PlantUtilities::InterConnectTwoPlantLoopSides(
            state, this->CDLoopNum, this->CDLoopSideNum, this->GenLoopNum, this->GenCompNum, DataPlant::TypeOf_Chiller_Indirect_Absorption, false);
    }
    if (errFlag) {
        ShowFatalError(state, "InitIndirectAbsorpChiller: Program terminated due to previous condition(s).");
    }

    if (this->FlowMode == DataPlant::FlowMode::Constant) {
        // reset flow priority
        state.dataPlnt->PlantLoop(this->CWLoopNum).LoopSide(this->CWLoopSideNum).Branch(this->CWBranchNum).Comp(this->CWCompNum).FlowPriority =
            DataPlant::LoopFlowStatus_NeedyIfLoopOn;
    }

    if (this->FlowMode == DataPlant::FlowMode::LeavingSetpointModulated) {
        // reset flow priority
        state.dataPlnt->PlantLoop(this->CWLoopNum).LoopSide(this->CWLoopSideNum).Branch(this->CWBranchNum).Comp(this->CWCompNum).FlowPriority =
            DataPlant::LoopFlowStatus_NeedyIfLoopOn;

        if ((state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempSetPoint == DataLoopNode::SensedNodeFlagValue) &&
            (state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempSetPointHi == DataLoopNode::SensedNodeFlagValue)) {
            if (!state.dataGlobal->AnyEnergyManagementSystemInModel) {
                if (!this->ModulatedFlowErrDone) {
                    ShowWarningError(state, "Missing temperature setpoint for LeavingSetpointModulated mode chiller named " + this->Name);
                    ShowContinueError(
                        state, "  A temperature setpoint is needed at the outlet node of a chiller in variable flow mode, use a SetpointManager");
                    ShowContinueError(state, "  The overall loop setpoint will be assumed for chiller. The simulation continues ... ");
                    this->ModulatedFlowErrDone = true;
                }
            } else {
                // need call to EMS to check node
                bool FatalError = false; // but not really fatal yet, but should be.
                EMSManager::CheckIfNodeSetPointManagedByEMS(
                    state, this->EvapOutletNodeNum, EMSManager::SPControlType::iTemperatureSetPoint, FatalError);
                state.dataLoopNodes->NodeSetpointCheck(this->EvapOutletNodeNum).needsSetpointChecking = false;
                if (FatalError) {
                    if (!this->ModulatedFlowErrDone) {
                        ShowWarningError(state, "Missing temperature setpoint for LeavingSetpointModulated mode chiller named " + this->Name);
                        ShowContinueError(state,
                                          "  A temperature setpoint is needed at the outlet node of a chiller evaporator in variable flow mode");
                        ShowContinueError(state, "  use a Setpoint Manager to establish a setpoint at the chiller evaporator outlet node ");
                        ShowContinueError(state, "  or use an EMS actuator to establish a setpoint at the outlet node ");
                        ShowContinueError(state, "  The overall loop setpoint will be assumed for chiller. The simulation continues ... ");
                        this->ModulatedFlowErrDone = true;
                    }
                }
            }

            this->ModulatedFlowSetToLoop = true;
            state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempSetPoint =
                state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPoint;
            state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempSetPointHi =
                state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPointHi;
        }
    }
}

void IndirectAbsorberSpecs::initialize(EnergyPlusData &state, bool RunFlag, Real64 MyLoad)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard Raustad
    //       DATE WRITTEN   September 2009
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine is for initializations of the Indirect Absorption Chiller components

    // METHODOLOGY EMPLOYED:
    // Uses the status flags to trigger initializations.

    static constexpr std::string_view RoutineName("InitIndirectAbsorpChiller");

    // Init more variables
    if (this->MyOneTimeFlag) {
        this->oneTimeInit(state);
        this->setupOutputVars(state);
        this->MyOneTimeFlag = false;
    }

    this->EquipFlowCtrl =
        state.dataPlnt->PlantLoop(this->CWLoopNum).LoopSide(this->CWLoopSideNum).Branch(this->CWBranchNum).Comp(this->CWCompNum).FlowCtrl;

    // Initialize Supply Side Variables
    if (this->MyEnvrnFlag && state.dataGlobal->BeginEnvrnFlag && (state.dataPlnt->PlantFirstSizesOkayToFinalize)) {

        Real64 rho = FluidProperties::GetDensityGlycol(state,
                                                       state.dataPlnt->PlantLoop(this->CWLoopNum).FluidName,
                                                       DataGlobalConstants::CWInitConvTemp,
                                                       state.dataPlnt->PlantLoop(this->CWLoopNum).FluidIndex,
                                                       RoutineName);

        this->EvapMassFlowRateMax = this->EvapVolFlowRate * rho;

        PlantUtilities::InitComponentNodes(state,
                                           0.0,
                                           this->EvapMassFlowRateMax,
                                           this->EvapInletNodeNum,
                                           this->EvapOutletNodeNum,
                                           this->CWLoopNum,
                                           this->CWLoopSideNum,
                                           this->CWBranchNum,
                                           this->CWCompNum);

        rho = FluidProperties::GetDensityGlycol(state,
                                                state.dataPlnt->PlantLoop(this->CDLoopNum).FluidName,
                                                DataGlobalConstants::CWInitConvTemp,
                                                state.dataPlnt->PlantLoop(this->CDLoopNum).FluidIndex,
                                                RoutineName);

        this->CondMassFlowRateMax = rho * this->CondVolFlowRate;

        PlantUtilities::InitComponentNodes(state,
                                           0.0,
                                           this->CondMassFlowRateMax,
                                           this->CondInletNodeNum,
                                           this->CondOutletNodeNum,
                                           this->CDLoopNum,
                                           this->CDLoopSideNum,
                                           this->CDBranchNum,
                                           this->CDCompNum);

        state.dataLoopNodes->Node(this->CondInletNodeNum).Temp = this->TempDesCondIn;

        if (this->GeneratorInletNodeNum > 0) {

            if (this->GenHeatSourceType == DataLoopNode::NodeFluidType::Water) {

                rho = FluidProperties::GetDensityGlycol(state,
                                                        state.dataPlnt->PlantLoop(this->GenLoopNum).FluidName,
                                                        DataGlobalConstants::HWInitConvTemp,
                                                        state.dataPlnt->PlantLoop(this->GenLoopNum).FluidIndex,
                                                        RoutineName);
                this->GenMassFlowRateMax = rho * this->GeneratorVolFlowRate;

            } else {
                Real64 SteamDensity = FluidProperties::GetSatDensityRefrig(state,
                                                                           fluidNameSteam,
                                                                           state.dataLoopNodes->Node(this->GeneratorInletNodeNum).Temp,
                                                                           1.0,
                                                                           this->SteamFluidIndex,
                                                                           std::string{calcChillerAbsorptionIndirect} + this->Name);
                this->GenMassFlowRateMax = SteamDensity * this->GeneratorVolFlowRate;
            }

            PlantUtilities::InitComponentNodes(state,
                                               0.0,
                                               this->GenMassFlowRateMax,
                                               this->GeneratorInletNodeNum,
                                               this->GeneratorOutletNodeNum,
                                               this->GenLoopNum,
                                               this->GenLoopSideNum,
                                               this->GenBranchNum,
                                               this->GenCompNum);
        }
        this->MyEnvrnFlag = false;
    }
    if (!state.dataGlobal->BeginEnvrnFlag) {
        this->MyEnvrnFlag = true;
    }

    if ((this->FlowMode == DataPlant::FlowMode::LeavingSetpointModulated) && this->ModulatedFlowSetToLoop) {
        // fix for clumsy old input that worked because loop setpoint was spread.
        //  could be removed with transition, testing , model change, period of being obsolete.
        state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempSetPoint =
            state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPoint;
        state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempSetPointHi =
            state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPointHi;
    }

    Real64 mdotEvap = 0.0; // local fluid mass flow rate thru evaporator
    Real64 mdotCond = 0.0; // local fluid mass flow rate thru condenser
    Real64 mdotGen = 0.0;  // local fluid mass flow rate thru generator

    if ((MyLoad < 0.0) && RunFlag) {
        mdotEvap = this->EvapMassFlowRateMax;
        mdotCond = this->CondMassFlowRateMax;
        mdotGen = this->GenMassFlowRateMax;
    }

    PlantUtilities::SetComponentFlowRate(
        state, mdotEvap, this->EvapInletNodeNum, this->EvapOutletNodeNum, this->CWLoopNum, this->CWLoopSideNum, this->CWBranchNum, this->CWCompNum);

    PlantUtilities::SetComponentFlowRate(
        state, mdotCond, this->CondInletNodeNum, this->CondOutletNodeNum, this->CDLoopNum, this->CDLoopSideNum, this->CDBranchNum, this->CDCompNum);

    if (this->GeneratorInletNodeNum > 0) {

        PlantUtilities::SetComponentFlowRate(state,
                                             mdotGen,
                                             this->GeneratorInletNodeNum,
                                             this->GeneratorOutletNodeNum,
                                             this->GenLoopNum,
                                             this->GenLoopSideNum,
                                             this->GenBranchNum,
                                             this->GenCompNum);
    }
}

void IndirectAbsorberSpecs::sizeChiller(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         R. Raustad (FSEC)
    //       DATE WRITTEN   May 2008
    //       MODIFIED       November 2013 Daeho Kang, add component sizing table entries
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine is for sizing Indirect Absorption Chiller Components for which capacities and flow rates
    // have not been specified in the input.

    // METHODOLOGY EMPLOYED:
    // Obtains evaporator flow rate from the plant sizing array. Calculates nominal capacity from
    // the evaporator flow rate and the chilled water loop design delta T. The condenser flow rate
    // is calculated from the nominal capacity, the COP, and the condenser loop design delta T.

    static constexpr std::string_view RoutineName("SizeIndirectAbsorpChiller");
    static constexpr std::string_view SizeChillerAbsorptionIndirect("SIZE Chiller:Absorption:Indirect");

    bool LoopErrorsFound = false;

    Real64 PltSizCondNum = 0;
    Real64 PltSizHeatingNum = 0;
    Real64 PltSizSteamNum = 0;
    bool ErrorsFound = false;
    // init local temporary version in case of partial/mixed autosizing

    // local nominal capacity cooling power
    Real64 tmpNomCap = this->NomCap;

    // local evaporator design volume flow rate
    Real64 tmpEvapVolFlowRate = this->EvapVolFlowRate;

    // local condenser design volume flow rate
    Real64 tmpCondVolFlowRate = this->CondVolFlowRate;

    // local generator design volume flow rate
    Real64 tmpGeneratorVolFlowRate = this->GeneratorVolFlowRate;

    Real64 SteamInputRatNom = 1.0; // nominal energy input ratio (steam or hot water)
    if (this->GeneratorInputCurvePtr > 0) {
        SteamInputRatNom = CurveManager::CurveValue(state, this->GeneratorInputCurvePtr, 1.0);
    }

    // find the appropriate Plant Sizing object
    int PltSizNum = state.dataPlnt->PlantLoop(this->CWLoopNum).PlantSizNum;

    // IF (IndirectAbsorber(ChillNum)%CondVolFlowRate == AutoSize) THEN
    if (PltSizNum > 0) {
        PltSizCondNum = PlantUtilities::MyPlantSizingIndex(
            state, "Chiller:Absorption:Indirect", this->Name, this->CondInletNodeNum, this->CondOutletNodeNum, LoopErrorsFound);
    }

    if (this->GenHeatSourceType == DataLoopNode::NodeFluidType::Steam) {
        if (this->GeneratorInletNodeNum > 0 && this->GeneratorOutletNodeNum > 0) {
            PltSizSteamNum = PlantUtilities::MyPlantSizingIndex(
                state, "Chiller:Absorption:Indirect", this->Name, this->GeneratorInletNodeNum, this->GeneratorOutletNodeNum, LoopErrorsFound);
        } else {
            for (int PltSizIndex = 1; PltSizIndex <= state.dataSize->NumPltSizInput; ++PltSizIndex) {
                if (state.dataSize->PlantSizData(PltSizIndex).LoopType == DataSizing::SteamLoop) {
                    PltSizSteamNum = PltSizIndex;
                }
            }
        }
    } else {
        if (this->GeneratorInletNodeNum > 0 && this->GeneratorOutletNodeNum > 0) {
            PltSizHeatingNum = PlantUtilities::MyPlantSizingIndex(
                state, "Chiller:Absorption:Indirect", this->Name, this->GeneratorInletNodeNum, this->GeneratorOutletNodeNum, LoopErrorsFound);
        } else {
            for (int PltSizIndex = 1; PltSizIndex <= state.dataSize->NumPltSizInput; ++PltSizIndex) {
                if (state.dataSize->PlantSizData(PltSizIndex).LoopType == DataSizing::HeatingLoop) {
                    PltSizHeatingNum = PltSizIndex;
                }
            }
        }
    }

    if (PltSizNum > 0) {
        if (state.dataSize->PlantSizData(PltSizNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {

            Real64 Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                               state.dataPlnt->PlantLoop(this->CWLoopNum).FluidName,
                                                               DataGlobalConstants::CWInitConvTemp,
                                                               state.dataPlnt->PlantLoop(this->CWLoopNum).FluidIndex,
                                                               RoutineName);

            Real64 rho = FluidProperties::GetDensityGlycol(state,
                                                           state.dataPlnt->PlantLoop(this->CWLoopNum).FluidName,
                                                           DataGlobalConstants::CWInitConvTemp,
                                                           state.dataPlnt->PlantLoop(this->CWLoopNum).FluidIndex,
                                                           RoutineName);
            tmpNomCap =
                Cp * rho * state.dataSize->PlantSizData(PltSizNum).DeltaT * state.dataSize->PlantSizData(PltSizNum).DesVolFlowRate * this->SizFac;
            if (!this->NomCapWasAutoSized) tmpNomCap = this->NomCap;
        } else {
            if (this->NomCapWasAutoSized) tmpNomCap = 0.0;
        }
        if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
            if (this->NomCapWasAutoSized) {
                this->NomCap = tmpNomCap;
                if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(state, "Chiller:Absorption:Indirect", this->Name, "Design Size Nominal Capacity [W]", tmpNomCap);
                }
                if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(
                        state, "Chiller:Absorption:Indirect", this->Name, "Initial Design Size Nominal Capacity [W]", tmpNomCap);
                }
            } else {
                if (this->NomCap > 0.0 && tmpNomCap > 0.0) {
                    Real64 NomCapUser = this->NomCap;
                    if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(state,
                                                     "Chiller:Absorption:Indirect",
                                                     this->Name,
                                                     "Design Size Nominal Capacity [W]",
                                                     tmpNomCap,
                                                     "User-Specified Nominal Capacity [W]",
                                                     NomCapUser);
                        if (state.dataGlobal->DisplayExtraWarnings) {
                            if ((std::abs(tmpNomCap - NomCapUser) / NomCapUser) > state.dataSize->AutoVsHardSizingThreshold) {
                                ShowMessage(state, "SizeChillerAbsorptionIndirect: Potential issue with equipment sizing for " + this->Name);
                                ShowContinueError(state, format("User-Specified Nominal Capacity of {:.2R} [W]", NomCapUser));
                                ShowContinueError(state, format("differs from Design Size Nominal Capacity of {:.2R} [W]", tmpNomCap));
                                ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                                ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                            }
                        }
                    }
                    tmpNomCap = NomCapUser;
                }
            }
        }
    } else {
        if (this->NomCapWasAutoSized) {
            if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                ShowSevereError(state, "Autosizing of Absorption Chiller nominal capacity requires a loop Sizing:Plant object");
                ShowContinueError(state, "Occurs in Chiller:Absorption:Indirect object=" + this->Name);
                ErrorsFound = true;
            }
        } else {
            if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                if (this->NomCap > 0.0) {
                    BaseSizer::reportSizerOutput(
                        state, "Chiller:Absorption:Indirect", this->Name, "User-Specified Nominal Capacity [W]", this->NomCap);
                }
            }
        }
    }

    // local nominal pump power
    Real64 tmpNomPumpPower = 0.0045 * tmpNomCap;
    if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
        // the DOE-2 EIR for single stage absorption chiller
        if (this->NomPumpPowerWasAutoSized) {
            this->NomPumpPower = tmpNomPumpPower; // 0.0045d0 * IndirectAbsorber(ChillNum)%NomCap
            if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                BaseSizer::reportSizerOutput(
                    state, "Chiller:Absorption:Indirect", this->Name, "Design Size Nominal Pumping Power [W]", tmpNomPumpPower);
            }
            if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                BaseSizer::reportSizerOutput(
                    state, "Chiller:Absorption:Indirect", this->Name, "Initial Design Size Nominal Pumping Power [W]", tmpNomPumpPower);
            }
        } else {
            if (this->NomPumpPower > 0.0 && tmpNomPumpPower > 0.0) {
                Real64 NomPumpPowerUser = this->NomPumpPower;
                if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(state,
                                                 "Chiller:Absorption:Indirect",
                                                 this->Name,
                                                 "Design Size Nominal Pumping Power [W]",
                                                 tmpNomPumpPower,
                                                 "User-Specified Nominal Pumping Power [W]",
                                                 NomPumpPowerUser);
                    if (state.dataGlobal->DisplayExtraWarnings) {
                        if ((std::abs(tmpNomPumpPower - NomPumpPowerUser) / NomPumpPowerUser) > state.dataSize->AutoVsHardSizingThreshold) {
                            ShowMessage(state, "SizeChillerAbsorptionIndirect: Potential issue with equipment sizing for " + this->Name);
                            ShowContinueError(state, format("User-Specified Nominal Pumping Power of {:.2R} [W]", NomPumpPowerUser));
                            ShowContinueError(state, format("differs from Design Size Nominal Pumping Power of {:.2R} [W]", tmpNomPumpPower));
                            ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                            ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                        }
                    }
                }
                tmpNomPumpPower = NomPumpPowerUser;
            }
        }
    }

    if (PltSizNum > 0) {
        if (state.dataSize->PlantSizData(PltSizNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
            tmpEvapVolFlowRate = state.dataSize->PlantSizData(PltSizNum).DesVolFlowRate * this->SizFac;
            if (!this->EvapVolFlowRateWasAutoSized) tmpEvapVolFlowRate = this->EvapVolFlowRate;
        } else {
            if (this->EvapVolFlowRateWasAutoSized) tmpEvapVolFlowRate = 0.0;
        }
        if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
            if (this->EvapVolFlowRateWasAutoSized) {
                this->EvapVolFlowRate = tmpEvapVolFlowRate;
                if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(
                        state, "Chiller:Absorption:Indirect", this->Name, "Design Size Design Chilled Water Flow Rate [m3/s]", tmpEvapVolFlowRate);
                }
                if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(state,
                                                 "Chiller:Absorption:Indirect",
                                                 this->Name,
                                                 "Initial Design Size Design Chilled Water Flow Rate [m3/s]",
                                                 tmpEvapVolFlowRate);
                }
            } else {
                if (this->EvapVolFlowRate > 0.0 && tmpEvapVolFlowRate > 0.0) {
                    Real64 EvapVolFlowRateUser = this->EvapVolFlowRate;
                    if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(state,
                                                     "Chiller:Absorption:Indirect",
                                                     this->Name,
                                                     "Design Size Design Chilled Water Flow Rate [m3/s]",
                                                     tmpEvapVolFlowRate,
                                                     "User-Specified Design Chilled Water Flow Rate [m3/s]",
                                                     EvapVolFlowRateUser);
                        if (state.dataGlobal->DisplayExtraWarnings) {
                            if ((std::abs(tmpEvapVolFlowRate - EvapVolFlowRateUser) / EvapVolFlowRateUser) >
                                state.dataSize->AutoVsHardSizingThreshold) {
                                ShowMessage(state, "SizeChillerElectricIndirect: Potential issue with equipment sizing for " + this->Name);
                                ShowContinueError(state,
                                                  format("User-Specified Design Chilled Water Flow Rate of {:.5R} [m3/s]", EvapVolFlowRateUser));
                                ShowContinueError(
                                    state, format("differs from Design Size Design Chilled Water Flow Rate of {:.5R} [m3/s]", tmpEvapVolFlowRate));
                                ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                                ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                            }
                        }
                    }
                    tmpEvapVolFlowRate = EvapVolFlowRateUser;
                }
            }
        }
    } else {
        if (this->EvapVolFlowRateWasAutoSized) {
            if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                ShowSevereError(state, "Autosizing of Absorption Chiller evap flow rate requires a loop Sizing:Plant object");
                ShowContinueError(state, "Occurs in Chiller:Absorption:Indirect object=" + this->Name);
                ErrorsFound = true;
            }
        } else {
            if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                if (this->EvapVolFlowRate > 0.0) {
                    BaseSizer::reportSizerOutput(state,
                                                 "Chiller:Absorption:Indirect",
                                                 this->Name,
                                                 "User-Specified Design Chilled Water Flow Rate [m3/s]",
                                                 this->EvapVolFlowRate);
                }
            }
        }
    }

    if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
        PlantUtilities::RegisterPlantCompDesignFlow(state, this->EvapInletNodeNum, this->EvapVolFlowRate);
    } else {
        PlantUtilities::RegisterPlantCompDesignFlow(state, this->EvapInletNodeNum, tmpEvapVolFlowRate);
    }

    if (PltSizCondNum > 0 && PltSizNum > 0) {
        if (this->EvapVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow && tmpNomCap > 0.0) {
            //       QCondenser = QEvaporator + QGenerator + PumpingPower

            Real64 Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                               state.dataPlnt->PlantLoop(this->CDLoopNum).FluidName,
                                                               DataGlobalConstants::CWInitConvTemp,
                                                               state.dataPlnt->PlantLoop(this->CDLoopNum).FluidIndex,
                                                               RoutineName);

            Real64 rho = FluidProperties::GetDensityGlycol(state,
                                                           state.dataPlnt->PlantLoop(this->CDLoopNum).FluidName,
                                                           DataGlobalConstants::CWInitConvTemp,
                                                           state.dataPlnt->PlantLoop(this->CDLoopNum).FluidIndex,
                                                           RoutineName);
            tmpCondVolFlowRate =
                tmpNomCap * (1.0 + SteamInputRatNom + tmpNomPumpPower / tmpNomCap) / (state.dataSize->PlantSizData(PltSizCondNum).DeltaT * Cp * rho);
            if (!this->CondVolFlowRateWasAutoSized) tmpCondVolFlowRate = this->CondVolFlowRate;
        } else {
            if (this->CondVolFlowRateWasAutoSized) tmpCondVolFlowRate = 0.0;
        }
        if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
            if (this->CondVolFlowRateWasAutoSized) {
                this->CondVolFlowRate = tmpCondVolFlowRate;
                if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(
                        state, "Chiller:Absorption:Indirect", this->Name, "Design Size Design Condenser Water Flow Rate [m3/s]", tmpCondVolFlowRate);
                }
                if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(state,
                                                 "Chiller:Absorption:Indirect",
                                                 this->Name,
                                                 "Initial Design Size Design Condenser Water Flow Rate [m3/s]",
                                                 tmpCondVolFlowRate);
                }
            } else {
                if (this->CondVolFlowRate > 0.0 && tmpCondVolFlowRate > 0.0) {
                    Real64 CondVolFlowRateUser = this->CondVolFlowRate;
                    if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(state,
                                                     "Chiller:Absorption:Indirect",
                                                     this->Name,
                                                     "Design Size Design Condenser Water Flow Rate [m3/s]",
                                                     tmpCondVolFlowRate,
                                                     "User-Specified Design Condenser Water Flow Rate [m3/s]",
                                                     CondVolFlowRateUser);
                        if (state.dataGlobal->DisplayExtraWarnings) {
                            if ((std::abs(tmpCondVolFlowRate - CondVolFlowRateUser) / CondVolFlowRateUser) >
                                state.dataSize->AutoVsHardSizingThreshold) {
                                ShowMessage(state, "SizeChillerAbsorptionIndirect: Potential issue with equipment sizing for " + this->Name);
                                ShowContinueError(state,
                                                  format("User-Specified Design Condenser Water Flow Rate of {:.5R} [m3/s]", CondVolFlowRateUser));
                                ShowContinueError(
                                    state, format("differs from Design Size Design Condenser Water Flow Rate of {:.5R} [m3/s]", tmpCondVolFlowRate));
                                ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                                ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                            }
                        }
                    }
                    tmpCondVolFlowRate = CondVolFlowRateUser;
                }
            }
        }
    } else {
        if (this->CondVolFlowRateWasAutoSized) {
            if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                ShowSevereError(state, "Autosizing of Absorption Chiller condenser flow rate requires a condenser");
                ShowContinueError(state, "loop Sizing:Plant object");
                ShowContinueError(state, "Occurs in Chiller:Absorption:Indirect object=" + this->Name);
                ErrorsFound = true;
            }
        } else {
            if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                if (this->CondVolFlowRate > 0.0) {
                    BaseSizer::reportSizerOutput(state,
                                                 "Chiller:Absorption:Indirect",
                                                 this->Name,
                                                 "User-Specified Design Condenser Water Flow Rate [m3/s]",
                                                 this->CondVolFlowRate);
                }
            }
        }
    }

    // save the design condenser water volumetric flow rate for use by the condenser water loop sizing algorithms
    if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
        PlantUtilities::RegisterPlantCompDesignFlow(state, this->CondInletNodeNum, this->CondVolFlowRate);
    } else {
        PlantUtilities::RegisterPlantCompDesignFlow(state, this->CondInletNodeNum, tmpCondVolFlowRate);
    }

    if ((PltSizSteamNum > 0 && this->GenHeatSourceType == DataLoopNode::NodeFluidType::Steam) ||
        (PltSizHeatingNum > 0 && this->GenHeatSourceType == DataLoopNode::NodeFluidType::Water)) {
        if (this->EvapVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow && tmpNomCap > 0.0) {
            if (this->GenHeatSourceType == DataLoopNode::NodeFluidType::Water) {
                Real64 CpWater = FluidProperties::GetSpecificHeatGlycol(state,
                                                                        state.dataPlnt->PlantLoop(this->GenLoopNum).FluidName,
                                                                        state.dataSize->PlantSizData(PltSizHeatingNum).ExitTemp,
                                                                        state.dataPlnt->PlantLoop(this->GenLoopNum).FluidIndex,
                                                                        RoutineName);
                Real64 SteamDeltaT = max(0.5, state.dataSize->PlantSizData(PltSizHeatingNum).DeltaT);

                Real64 RhoWater = FluidProperties::GetDensityGlycol(state,
                                                                    state.dataPlnt->PlantLoop(this->GenLoopNum).FluidName,
                                                                    (state.dataSize->PlantSizData(PltSizHeatingNum).ExitTemp - SteamDeltaT),
                                                                    state.dataPlnt->PlantLoop(this->GenLoopNum).FluidIndex,
                                                                    RoutineName);
                tmpGeneratorVolFlowRate = (tmpNomCap * SteamInputRatNom) / (CpWater * SteamDeltaT * RhoWater);
                if (!this->GeneratorVolFlowRateWasAutoSized) tmpGeneratorVolFlowRate = this->GeneratorVolFlowRate;
                if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                    if (this->GeneratorVolFlowRateWasAutoSized) {
                        this->GeneratorVolFlowRate = tmpGeneratorVolFlowRate;
                        if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                            BaseSizer::reportSizerOutput(state,
                                                         "Chiller:Absorption:Indirect",
                                                         this->Name,
                                                         "Design Size Design Generator Fluid Flow Rate [m3/s]",
                                                         tmpGeneratorVolFlowRate);
                        }
                        if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                            BaseSizer::reportSizerOutput(state,
                                                         "Chiller:Absorption:Indirect",
                                                         this->Name,
                                                         "Initial Design Size Design Generator Fluid Flow Rate [m3/s]",
                                                         tmpGeneratorVolFlowRate);
                        }
                    } else {
                        if (this->GeneratorVolFlowRate > 0.0 && tmpGeneratorVolFlowRate > 0.0) {
                            Real64 GeneratorVolFlowRateUser = this->GeneratorVolFlowRate;
                            if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                                BaseSizer::reportSizerOutput(state,
                                                             "Chiller:Absorption:Indirect",
                                                             this->Name,
                                                             "Design Size Design Generator Fluid Flow Rate [m3/s]",
                                                             tmpGeneratorVolFlowRate,
                                                             "User-Specified Design Generator Fluid Flow Rate [m3/s]",
                                                             GeneratorVolFlowRateUser);
                                if (state.dataGlobal->DisplayExtraWarnings) {
                                    if ((std::abs(tmpGeneratorVolFlowRate - GeneratorVolFlowRateUser) / GeneratorVolFlowRateUser) >
                                        state.dataSize->AutoVsHardSizingThreshold) {
                                        ShowMessage(state, "SizeChillerAbsorptionIndirect: Potential issue with equipment sizing for " + this->Name);
                                        ShowContinueError(
                                            state,
                                            format("User-Specified Design Generator Fluid Flow Rate of {:.5R} [m3/s]", GeneratorVolFlowRateUser));
                                        ShowContinueError(state,
                                                          format("differs from Design Size Design Generator Fluid Flow Rate of {:.5R} [m3/s]",
                                                                 tmpGeneratorVolFlowRate));
                                        ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                                        ShowContinueError(state,
                                                          "Verify that the value entered is intended and is consistent with other components.");
                                    }
                                }
                            }
                            tmpGeneratorVolFlowRate = GeneratorVolFlowRateUser;
                        }
                    }
                }
            } else {
                Real64 SteamDensity = FluidProperties::GetSatDensityRefrig(state,
                                                                           fluidNameSteam,
                                                                           state.dataSize->PlantSizData(PltSizSteamNum).ExitTemp,
                                                                           1.0,
                                                                           this->SteamFluidIndex,
                                                                           std::string{SizeChillerAbsorptionIndirect} + this->Name);
                Real64 SteamDeltaT = state.dataSize->PlantSizData(PltSizSteamNum).DeltaT;
                Real64 GeneratorOutletTemp = state.dataSize->PlantSizData(PltSizSteamNum).ExitTemp - SteamDeltaT;

                // dry enthalpy of steam (quality = 1)
                Real64 EnthSteamOutDry = FluidProperties::GetSatEnthalpyRefrig(state,
                                                                               fluidNameSteam,
                                                                               state.dataSize->PlantSizData(PltSizSteamNum).ExitTemp,
                                                                               1.0,
                                                                               this->SteamFluidIndex,
                                                                               std::string{SizeChillerAbsorptionIndirect} + this->Name);

                // wet enthalpy of steam (quality = 0)
                Real64 EnthSteamOutWet = FluidProperties::GetSatEnthalpyRefrig(state,
                                                                               fluidNameSteam,
                                                                               state.dataSize->PlantSizData(PltSizSteamNum).ExitTemp,
                                                                               0.0,
                                                                               this->SteamFluidIndex,
                                                                               std::string{SizeChillerAbsorptionIndirect} + this->Name);
                Real64 CpWater =
                    FluidProperties::GetSpecificHeatGlycol(state, fluidNameWater, GeneratorOutletTemp, const_cast<int &>(waterIndex), RoutineName);
                Real64 HfgSteam = EnthSteamOutDry - EnthSteamOutWet;
                //         calculate the mass flow rate through the generator
                Real64 SteamMassFlowRate = (tmpNomCap * SteamInputRatNom) / ((HfgSteam) + (SteamDeltaT * CpWater));
                //         calculate the steam volumetric flow rate
                tmpGeneratorVolFlowRate = SteamMassFlowRate / SteamDensity;
                if (!this->GeneratorVolFlowRateWasAutoSized) tmpGeneratorVolFlowRate = this->GeneratorVolFlowRate;
                if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                    if (this->GeneratorVolFlowRateWasAutoSized) {
                        this->GeneratorVolFlowRate = tmpGeneratorVolFlowRate;
                        if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                            BaseSizer::reportSizerOutput(state,
                                                         "Chiller:Absorption:Indirect",
                                                         this->Name,
                                                         "Design Size Design Generator Fluid Flow Rate [m3/s]",
                                                         tmpGeneratorVolFlowRate);
                        }
                        if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                            BaseSizer::reportSizerOutput(state,
                                                         "Chiller:Absorption:Indirect",
                                                         this->Name,
                                                         "Initial Design Size Design Generator Fluid Flow Rate [m3/s]",
                                                         tmpGeneratorVolFlowRate);
                        }
                    } else {
                        if (this->GeneratorVolFlowRate > 0.0 && tmpGeneratorVolFlowRate > 0.0) {
                            Real64 GeneratorVolFlowRateUser = this->GeneratorVolFlowRate;
                            if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                                BaseSizer::reportSizerOutput(state,
                                                             "Chiller:Absorption:Indirect",
                                                             this->Name,
                                                             "Design Size Design Generator Fluid Flow Rate [m3/s]",
                                                             tmpGeneratorVolFlowRate,
                                                             "User-Specified Design Generator Fluid Flow Rate [m3/s]",
                                                             GeneratorVolFlowRateUser);
                                if (state.dataGlobal->DisplayExtraWarnings) {
                                    if ((std::abs(tmpGeneratorVolFlowRate - GeneratorVolFlowRateUser) / GeneratorVolFlowRateUser) >
                                        state.dataSize->AutoVsHardSizingThreshold) {
                                        ShowMessage(state, "SizeChillerAbsorptionIndirect: Potential issue with equipment sizing for " + this->Name);
                                        ShowContinueError(
                                            state,
                                            format("User-Specified Design Generator Fluid Flow Rate of {:.5R} [m3/s]", GeneratorVolFlowRateUser));
                                        ShowContinueError(state,
                                                          format("differs from Design Size Design Generator Fluid Flow Rate of {:.5R} [m3/s]",
                                                                 tmpGeneratorVolFlowRate));
                                        ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                                        ShowContinueError(state,
                                                          "Verify that the value entered is intended and is consistent with other components.");
                                    }
                                }
                            }
                            tmpGeneratorVolFlowRate = GeneratorVolFlowRateUser;
                        }
                    }
                }
            }
        } else {
            if (this->GeneratorVolFlowRateWasAutoSized) {
                if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                    this->GeneratorVolFlowRate = 0.0;
                } else {
                    tmpGeneratorVolFlowRate = 0.0;
                }
            }
        }
    } else {
        if (this->GeneratorVolFlowRateWasAutoSized) {
            if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                ShowSevereError(state, "Autosizing of Absorption Chiller generator flow rate requires a loop Sizing:Plant object.");
                ShowContinueError(state, " For steam loops, use a steam Sizing:Plant object.");
                ShowContinueError(state, " For hot water loops, use a heating Sizing:Plant object.");
                ShowContinueError(state, "Occurs in Chiller:Absorption:Indirect object=" + this->Name);
                ErrorsFound = true;
            }
        } else {
            if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                if (this->GeneratorVolFlowRate > 0.0) {
                    BaseSizer::reportSizerOutput(state,
                                                 "Chiller:Absorption:Indirect",
                                                 this->Name,
                                                 "User-Specified Design Generator Fluid Flow Rate [m3/s]",
                                                 this->GeneratorVolFlowRate);
                }
            }
        }
    }

    // save the design steam or hot water volumetric flow rate for use by the steam or hot water loop sizing algorithms
    if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
        PlantUtilities::RegisterPlantCompDesignFlow(state, this->GeneratorInletNodeNum, this->GeneratorVolFlowRate);
    } else {
        PlantUtilities::RegisterPlantCompDesignFlow(state, this->GeneratorInletNodeNum, tmpGeneratorVolFlowRate);
    }

    if (this->GeneratorDeltaTempWasAutoSized) {
        if (PltSizHeatingNum > 0 && this->GenHeatSourceType == DataLoopNode::NodeFluidType::Water) {
            this->GeneratorDeltaTemp = max(0.5, state.dataSize->PlantSizData(PltSizHeatingNum).DeltaT);
        } else if (this->GenHeatSourceType == DataLoopNode::NodeFluidType::Water) {
            Real64 rho = FluidProperties::GetDensityGlycol(state,
                                                           state.dataPlnt->PlantLoop(this->GenLoopNum).FluidName,
                                                           DataGlobalConstants::HWInitConvTemp,
                                                           state.dataPlnt->PlantLoop(this->GenLoopNum).FluidIndex,
                                                           RoutineName);
            Real64 CpWater = FluidProperties::GetSpecificHeatGlycol(state,
                                                                    state.dataPlnt->PlantLoop(this->GenLoopNum).FluidName,
                                                                    state.dataSize->PlantSizData(PltSizHeatingNum).ExitTemp,
                                                                    state.dataPlnt->PlantLoop(this->GenLoopNum).FluidIndex,
                                                                    RoutineName);
            if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                this->GeneratorDeltaTemp = (SteamInputRatNom * this->NomCap) / (CpWater * rho * this->GeneratorVolFlowRate);
            }
        }
    }

    if (ErrorsFound) {
        ShowFatalError(state, "Preceding sizing errors cause program termination");
    }

    if (state.dataPlnt->PlantFinalSizesOkayToReport) {
        // create predefined report
        std::string equipName = this->Name;
        OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchMechType, equipName, "Chiller:Absorption:Indirect");
        OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchMechNomEff, equipName, "n/a");
        OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchMechNomCap, equipName, this->NomCap);
    }
}

void IndirectAbsorberSpecs::calculate(EnergyPlusData &state, Real64 const MyLoad, bool const RunFlag)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         R. Raustad (FSEC)
    //       DATE WRITTEN   May 2008
    //       MODIFIED       Jun. 2016, Rongpeng Zhang, Applied the chiller supply water temperature sensor fault model
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // simulate a vapor compression Absorber using a revised BLAST model

    // METHODOLOGY EMPLOYED:
    // curve fit of performance data:

    // REFERENCES:
    // 1.  BLAST User Manual
    // 2.  Absorber User Manual

    static constexpr std::string_view RoutineName("CalcIndirectAbsorberModel");
    static constexpr std::string_view LoopLossesChillerAbsorptionIndirect("Loop Losses: Chiller:Absorption:Indirect");
    static constexpr std::string_view LoopLossesChillerAbsorptionIndirectSpace("Loop Losses: Chiller:Absorption:Indirect ");

    Real64 TempEvapOutSetPoint(0.0); // C - evaporator outlet temperature setpoint
    Real64 EvapDeltaTemp(0.0);       // C - evaporator temperature difference, water side

    // set module level inlet and outlet nodes
    this->EvapMassFlowRate = 0.0;
    this->CondMassFlowRate = 0.0;
    this->GenMassFlowRate = 0.0;
    this->QCondenser = 0.0;
    this->QEvaporator = 0.0;
    this->QGenerator = 0.0;
    this->PumpingEnergy = 0.0;
    this->CondenserEnergy = 0.0;
    this->EvaporatorEnergy = 0.0;
    this->GeneratorEnergy = 0.0;
    this->PumpingPower = 0.0;
    this->ChillerONOFFCyclingFrac = 0.0;
    this->EnergyLossToEnvironment = 0.0;

    //  If no loop demand or Absorber OFF, return
    if (MyLoad >= 0.0 || !RunFlag) {
        if (this->EquipFlowCtrl == DataBranchAirLoopPlant::ControlTypeEnum::SeriesActive)
            this->EvapMassFlowRate = state.dataLoopNodes->Node(this->EvapInletNodeNum).MassFlowRate;
        return;
    }

    // Warn if entering condenser water temperature is below minimum
    if (state.dataLoopNodes->Node(this->CondInletNodeNum).Temp < this->MinCondInletTemp) {
        if (!state.dataGlobal->WarmupFlag) {
            if (this->MinCondInletTempCtr < 1) {
                ++this->MinCondInletTempCtr;
                ShowWarningError(state, "Chiller:Absorption:Indirect \"" + this->Name + "\"");
                ShowContinueError(state,
                                  format("...Entering condenser water temperature below specified minimum ({:.3R} C).", this->MinCondInletTemp));
                ShowContinueError(
                    state, format("...Entering condenser water temperature = {:.3R} C.", state.dataLoopNodes->Node(this->CondInletNodeNum).Temp));
                ShowContinueErrorTimeStamp(state, "...simulation continues.");
            } else {
                ShowRecurringWarningErrorAtEnd(state,
                                               "Entering condenser water temperature below specified minimum error continues.",
                                               this->MinCondInletTempIndex,
                                               state.dataLoopNodes->Node(this->CondInletNodeNum).Temp,
                                               state.dataLoopNodes->Node(this->CondInletNodeNum).Temp);
            }
        }
    }

    // Warn if entering generator fluid temperature is below minimum
    if (this->GeneratorInletNodeNum > 0) {
        if (state.dataLoopNodes->Node(this->GeneratorInletNodeNum).Temp < this->MinGeneratorInletTemp) {
            if (!state.dataGlobal->WarmupFlag) {
                if (this->MinGenInletTempCtr < 1) {
                    ++this->MinGenInletTempCtr;
                    ShowWarningError(state, "Chiller:Absorption:Indirect \"" + this->Name + "\"");
                    ShowContinueError(
                        state, format("...Entering generator fluid temperature below specified minimum ({:.3R} C).", this->MinGeneratorInletTemp));
                    ShowContinueError(
                        state,
                        format("...Entering generator fluid temperature = {:.3R} C.", state.dataLoopNodes->Node(this->GeneratorInletNodeNum).Temp));
                    ShowContinueErrorTimeStamp(state, "...simulation continues.");
                } else {
                    ShowRecurringWarningErrorAtEnd(state,
                                                   "Entering generator fluid temperature below specified minimum error continues.",
                                                   this->MinGenInletTempIndex,
                                                   state.dataLoopNodes->Node(this->GeneratorInletNodeNum).Temp,
                                                   state.dataLoopNodes->Node(this->GeneratorInletNodeNum).Temp);
                }
            }
        }
    }

    // Set module level Absorber inlet and temperature variables
    // C - evaporator inlet temperature, water side
    Real64 EvapInletTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp;

    // C - condenser inlet temperature, water side
    Real64 CondInletTemp = state.dataLoopNodes->Node(this->CondInletNodeNum).Temp;

    // Set the condenser mass flow rates
    this->CondMassFlowRate = state.dataLoopNodes->Node(this->CondInletNodeNum).MassFlowRate;

    // Absorber nominal capacity
    Real64 AbsorberNomCap = this->NomCap;

    // C - (BLAST ADJTC(1)The design secondary loop fluid
    Real64 TempCondIn = state.dataLoopNodes->Node(this->CondInletNodeNum).Temp;

    // C - evaporator outlet temperature, water side
    Real64 TempEvapOut = state.dataLoopNodes->Node(this->EvapOutletNodeNum).Temp;

    // C - Evaporator low temp. limit cut off
    Real64 TempLowLimitEout = this->TempLowLimitEvapOut;

    Real64 CpFluid = FluidProperties::GetSpecificHeatGlycol(state,
                                                            state.dataPlnt->PlantLoop(this->CWLoopNum).FluidName,
                                                            EvapInletTemp,
                                                            state.dataPlnt->PlantLoop(this->CWLoopNum).FluidIndex,
                                                            RoutineName);

    // If there is a fault of Chiller SWT Sensor
    if (this->FaultyChillerSWTFlag && (!state.dataGlobal->WarmupFlag) && (!state.dataGlobal->DoingSizing) && (!state.dataGlobal->KickOffSimulation)) {
        int FaultIndex = this->FaultyChillerSWTIndex;
        Real64 EvapOutletTemp_ff = TempEvapOut;

        // calculate the sensor offset using fault information
        this->FaultyChillerSWTOffset = state.dataFaultsMgr->FaultsChillerSWTSensor(FaultIndex).CalFaultOffsetAct(state);
        // update the TempEvapOut
        TempEvapOut = max(this->TempLowLimitEvapOut,
                          min(state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp, EvapOutletTemp_ff - this->FaultyChillerSWTOffset));
        this->FaultyChillerSWTOffset = EvapOutletTemp_ff - TempEvapOut;
    }

    Real64 CapacityfAbsorberTemp = 1.0; // performance curve output
    if (this->CapFCondenserTempPtr > 0) {
        CapacityfAbsorberTemp = CurveManager::CurveValue(state, this->CapFCondenserTempPtr, TempCondIn);
    }

    Real64 CapacityfEvaporatorTemp = 1.0; // performance curve output
    if (this->CapFEvaporatorTempPtr > 0) {
        CapacityfEvaporatorTemp = CurveManager::CurveValue(state, this->CapFEvaporatorTempPtr, TempEvapOut);
    }

    Real64 CapacityfGeneratorTemp = 1.0; // performance curve output
    if (this->CapFGeneratorTempPtr > 0) {
        if (this->GeneratorInletNodeNum > 0) {
            if (this->GenHeatSourceType == DataLoopNode::NodeFluidType::Water) {
                CapacityfGeneratorTemp =
                    CurveManager::CurveValue(state, this->CapFGeneratorTempPtr, state.dataLoopNodes->Node(this->GeneratorInletNodeNum).Temp);
            } else {
                CapacityfGeneratorTemp = 1.0;
            }
        } else {
            CapacityfGeneratorTemp = 1.0;
        }
    }

    AbsorberNomCap *= CapacityfAbsorberTemp * CapacityfEvaporatorTemp * CapacityfGeneratorTemp;

    // If FlowLock is True, the new resolved mdot is used to update Power, QEvap, Qcond, and
    // condenser side outlet temperature.
    if (state.dataPlnt->PlantLoop(this->CWLoopNum).LoopSide(this->CWLoopSideNum).FlowLock == DataPlant::iFlowLock::Unlocked) {
        this->PossibleSubcooling = false;
        this->QEvaporator = std::abs(MyLoad);

        // Either set the flow to the Constant value or caluclate the flow for the variable volume
        if ((this->FlowMode == DataPlant::FlowMode::Constant) || (this->FlowMode == DataPlant::FlowMode::NotModulated)) {
            this->EvapMassFlowRate = state.dataLoopNodes->Node(this->EvapInletNodeNum).MassFlowRate;

            if (this->EvapMassFlowRate != 0.0) {
                EvapDeltaTemp = this->QEvaporator / this->EvapMassFlowRate / CpFluid;
            } else {
                EvapDeltaTemp = 0.0;
            }
            this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - EvapDeltaTemp;

        } else if (this->FlowMode == DataPlant::FlowMode::LeavingSetpointModulated) {
            // Calculate the Delta Temp from the inlet temp to the chiller outlet setpoint
            {
                auto const SELECT_CASE_var(state.dataPlnt->PlantLoop(this->CWLoopNum).LoopDemandCalcScheme);
                if (SELECT_CASE_var == DataPlant::iLoopDemandCalcScheme::SingleSetPoint) {
                    EvapDeltaTemp =
                        state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempSetPoint;
                } else if (SELECT_CASE_var == DataPlant::iLoopDemandCalcScheme::DualSetPointDeadBand) {
                    EvapDeltaTemp =
                        state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempSetPointHi;
                } else {
                    assert(false);
                }
            }

            if (EvapDeltaTemp != 0) {
                this->EvapMassFlowRate = std::abs(this->QEvaporator / CpFluid / EvapDeltaTemp);
                if ((this->EvapMassFlowRate - this->EvapMassFlowRateMax) > DataBranchAirLoopPlant::MassFlowTolerance) this->PossibleSubcooling = true;
                // Check to see if the Maximum is exceeded, if so set to maximum
                this->EvapMassFlowRate = min(this->EvapMassFlowRateMax, this->EvapMassFlowRate);
                PlantUtilities::SetComponentFlowRate(state,
                                                     this->EvapMassFlowRate,
                                                     this->EvapInletNodeNum,
                                                     this->EvapOutletNodeNum,
                                                     this->CWLoopNum,
                                                     this->CWLoopSideNum,
                                                     this->CWBranchNum,
                                                     this->CWCompNum);
                {
                    auto const SELECT_CASE_var(state.dataPlnt->PlantLoop(this->CWLoopNum).LoopDemandCalcScheme);
                    if (SELECT_CASE_var == DataPlant::iLoopDemandCalcScheme::SingleSetPoint) {
                        this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempSetPoint;
                    } else if (SELECT_CASE_var == DataPlant::iLoopDemandCalcScheme::DualSetPointDeadBand) {
                        this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempSetPointHi;
                    }
                }
            } else {
                this->EvapMassFlowRate = 0.0;
                this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp;

                ShowRecurringWarningErrorAtEnd(state,
                                               "CalcIndirectAbsorberModel: Name=\"" + this->Name +
                                                   "\" Evaporative Condenser Delta Temperature = 0 in mass flow calculation.",
                                               this->ErrCount2);
            }
        } // End of Constant Variable Flow If Block

        // If there is a fault of Chiller SWT Sensor
        if (this->FaultyChillerSWTFlag && (!state.dataGlobal->WarmupFlag) && (!state.dataGlobal->DoingSizing) &&
            (!state.dataGlobal->KickOffSimulation) && (this->EvapMassFlowRate > 0)) {
            // calculate directly affected variables at faulty case: EvapOutletTemp, EvapMassFlowRate, QEvaporator
            int FaultIndex = this->FaultyChillerSWTIndex;
            bool VarFlowFlag = (this->FlowMode == DataPlant::FlowMode::LeavingSetpointModulated);
            state.dataFaultsMgr->FaultsChillerSWTSensor(FaultIndex)
                .CalFaultChillerSWT(VarFlowFlag,
                                    this->FaultyChillerSWTOffset,
                                    CpFluid,
                                    state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp,
                                    this->EvapOutletTemp,
                                    this->EvapMassFlowRate,
                                    this->QEvaporator);
            // update corresponding variables at faulty case
            // PartLoadRat = ( AvailChillerCap > 0.0 ) ? ( QEvaporator / AvailChillerCap ) : 0.0;
            // PartLoadRat = max( 0.0, min( PartLoadRat, MaxPartLoadRat ));
            // ChillerPartLoadRatio = PartLoadRat;
        }

    } else { // If FlowLock is True

        this->EvapMassFlowRate = state.dataLoopNodes->Node(this->EvapInletNodeNum).MassFlowRate;
        if (this->PossibleSubcooling) {
            this->QEvaporator = std::abs(MyLoad);
            EvapDeltaTemp = this->QEvaporator / this->EvapMassFlowRate / CpFluid;
            this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - EvapDeltaTemp;
        } else {
            {
                auto const SELECT_CASE_var(state.dataPlnt->PlantLoop(this->CWLoopNum).LoopDemandCalcScheme);
                if (SELECT_CASE_var == DataPlant::iLoopDemandCalcScheme::SingleSetPoint) {
                    if ((this->FlowMode == DataPlant::FlowMode::LeavingSetpointModulated) ||
                        (state.dataPlnt->PlantLoop(this->CWLoopNum)
                             .LoopSide(this->CWLoopSideNum)
                             .Branch(this->CWBranchNum)
                             .Comp(this->CWCompNum)
                             .CurOpSchemeType == DataPlant::CompSetPtBasedSchemeType) ||
                        (state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempSetPoint != DataLoopNode::SensedNodeFlagValue)) {
                        TempEvapOutSetPoint = state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempSetPoint;
                    } else {
                        TempEvapOutSetPoint = state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPoint;
                    }
                } else if (SELECT_CASE_var == DataPlant::iLoopDemandCalcScheme::DualSetPointDeadBand) {
                    if ((this->FlowMode == DataPlant::FlowMode::LeavingSetpointModulated) ||
                        (state.dataPlnt->PlantLoop(this->CWLoopNum)
                             .LoopSide(this->CWLoopSideNum)
                             .Branch(this->CWBranchNum)
                             .Comp(this->CWCompNum)
                             .CurOpSchemeType == DataPlant::CompSetPtBasedSchemeType) ||
                        (state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempSetPointHi != DataLoopNode::SensedNodeFlagValue)) {
                        TempEvapOutSetPoint = state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempSetPointHi;
                    } else {
                        TempEvapOutSetPoint =
                            state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPointHi;
                    }
                } else {
                    assert(false);
                }
            }
            EvapDeltaTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - TempEvapOutSetPoint;
            this->QEvaporator = std::abs(this->EvapMassFlowRate * CpFluid * EvapDeltaTemp);
            this->EvapOutletTemp = TempEvapOutSetPoint;
        }
        // Check that the Evap outlet temp honors both plant loop temp low limit and also the chiller low limit
        if (this->EvapOutletTemp < TempLowLimitEout) {
            if ((state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - TempLowLimitEout) > DataPlant::DeltaTempTol) {
                this->EvapOutletTemp = TempLowLimitEout;
                EvapDeltaTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - this->EvapOutletTemp;
                this->QEvaporator = this->EvapMassFlowRate * CpFluid * EvapDeltaTemp;
            } else {
                this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp;
                EvapDeltaTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - this->EvapOutletTemp;
                this->QEvaporator = this->EvapMassFlowRate * CpFluid * EvapDeltaTemp;
            }
        }
        if (this->EvapOutletTemp < state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempMin) {
            if ((state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempMin) >
                DataPlant::DeltaTempTol) {
                this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempMin;
                EvapDeltaTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - this->EvapOutletTemp;
                this->QEvaporator = this->EvapMassFlowRate * CpFluid * EvapDeltaTemp;
            } else {
                this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp;
                EvapDeltaTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - this->EvapOutletTemp;
                this->QEvaporator = this->EvapMassFlowRate * CpFluid * EvapDeltaTemp;
            }
        }

        // Checks QEvaporator on the basis of the machine limits.
        if (this->QEvaporator > std::abs(MyLoad)) {
            if (this->EvapMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance) {
                this->QEvaporator = std::abs(MyLoad);
                EvapDeltaTemp = this->QEvaporator / this->EvapMassFlowRate / CpFluid;
                this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - EvapDeltaTemp;
            } else {
                this->QEvaporator = 0.0;
                this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp;
            }
        }

        // If there is a fault of Chiller SWT Sensor
        if (this->FaultyChillerSWTFlag && (!state.dataGlobal->WarmupFlag) && (!state.dataGlobal->DoingSizing) &&
            (!state.dataGlobal->KickOffSimulation) && (this->EvapMassFlowRate > 0)) {
            // calculate directly affected variables at faulty case: EvapOutletTemp, EvapMassFlowRate, QEvaporator
            int FaultIndex = this->FaultyChillerSWTIndex;
            bool VarFlowFlag = false;
            state.dataFaultsMgr->FaultsChillerSWTSensor(FaultIndex)
                .CalFaultChillerSWT(VarFlowFlag,
                                    this->FaultyChillerSWTOffset,
                                    CpFluid,
                                    state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp,
                                    this->EvapOutletTemp,
                                    this->EvapMassFlowRate,
                                    this->QEvaporator);
            // update corresponding variables at faulty case
        }

    } // This is the end of the FlowLock Block

    // Operating part load ratio
    Real64 OperPartLoadRat = this->QEvaporator / AbsorberNomCap;

    // part load ratio for efficiency calc
    Real64 PartLoadRat = max(this->MinPartLoadRat, OperPartLoadRat);
    this->Report.ChillerPartLoadRatio = OperPartLoadRat;

    Real64 FRAC = 1.0; // fraction of time step chiller cycles
    if (OperPartLoadRat < PartLoadRat) {
        FRAC = min(1.0, OperPartLoadRat / this->MinPartLoadRat);
    }

    this->ChillerONOFFCyclingFrac = FRAC;

    Real64 HeatInputfCondTemp = 1.0; // performance curve output
    if (this->GeneratorInletNodeNum > 0) {
        if (this->HeatInputFCondTempPtr > 0) {
            HeatInputfCondTemp =
                CurveManager::CurveValue(state, this->HeatInputFCondTempPtr, state.dataLoopNodes->Node(this->GeneratorInletNodeNum).Temp);
        } else {
            HeatInputfCondTemp = 1.0;
        }
    }

    Real64 HeatInputfEvapTemp = 1.0; // performance curve output
    if (this->HeatInputFEvapTempPtr > 0) {
        HeatInputfEvapTemp = CurveManager::CurveValue(state, this->HeatInputFEvapTempPtr, state.dataLoopNodes->Node(this->EvapOutletNodeNum).Temp);
    }

    // Calculate steam input ratio. Include impact of generator and evaporator temperatures
    Real64 HeatInputRat = HeatInputfCondTemp * HeatInputfEvapTemp; // generator heat input ratio
    if (this->GeneratorInputCurvePtr > 0) {
        HeatInputRat = CurveManager::CurveValue(state, this->GeneratorInputCurvePtr, PartLoadRat) * HeatInputfCondTemp * HeatInputfEvapTemp;
    }

    // Calculate electric input ratio
    Real64 ElectricInputRat = 1.0; // energy input ratio
    if (this->PumpPowerCurvePtr > 0) {
        ElectricInputRat = CurveManager::CurveValue(state, this->PumpPowerCurvePtr, PartLoadRat);
    }

    this->QGenerator = HeatInputRat * AbsorberNomCap * FRAC;
    this->PumpingPower = ElectricInputRat * this->NomPumpPower * FRAC;

    if (this->EvapMassFlowRate == 0.0) {
        this->QGenerator = 0.0;
        this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp;
        this->PumpingPower = 0.0;
    }

    this->QCondenser = this->QEvaporator + this->QGenerator + this->PumpingPower;

    CpFluid = FluidProperties::GetSpecificHeatGlycol(state,
                                                     state.dataPlnt->PlantLoop(this->CDLoopNum).FluidName,
                                                     CondInletTemp,
                                                     state.dataPlnt->PlantLoop(this->CDLoopNum).FluidIndex,
                                                     RoutineName);

    if (this->CondMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance) {
        this->CondOutletTemp = this->QCondenser / this->CondMassFlowRate / CpFluid + CondInletTemp;
    } else {
        this->CondOutletTemp = CondInletTemp;
        this->CondMassFlowRate = 0.0;
        this->QCondenser = 0.0;
        return;
        // V7 plant upgrade, no longer fatal here anymore... set some things and return
    }

    if (this->GeneratorInletNodeNum > 0) {
        //   Hot water plant is used for the generator
        if (this->GenHeatSourceType == DataLoopNode::NodeFluidType::Water) {

            CpFluid = FluidProperties::GetSpecificHeatGlycol(state,
                                                             state.dataPlnt->PlantLoop(this->GenLoopNum).FluidName,
                                                             state.dataLoopNodes->Node(this->GeneratorInletNodeNum).Temp,
                                                             state.dataPlnt->PlantLoop(this->GenLoopNum).FluidIndex,
                                                             RoutineName);
            if ((this->FlowMode == DataPlant::FlowMode::Constant) || (this->FlowMode == DataPlant::FlowMode::NotModulated)) {
                this->GenMassFlowRate = this->GenMassFlowRateMax;
            } else {
                this->GenMassFlowRate = this->QGenerator / CpFluid / this->GeneratorDeltaTemp;
            }

            PlantUtilities::SetComponentFlowRate(state,
                                                 this->GenMassFlowRate,
                                                 this->GeneratorInletNodeNum,
                                                 this->GeneratorOutletNodeNum,
                                                 this->GenLoopNum,
                                                 this->GenLoopSideNum,
                                                 this->GenBranchNum,
                                                 this->GenCompNum);

            if (this->GenMassFlowRate <= 0.0) {
                this->GenOutletTemp = state.dataLoopNodes->Node(this->GeneratorInletNodeNum).Temp;
                this->SteamOutletEnthalpy = state.dataLoopNodes->Node(this->GeneratorInletNodeNum).Enthalpy;
            } else {
                this->GenOutletTemp =
                    state.dataLoopNodes->Node(this->GeneratorInletNodeNum).Temp - this->QGenerator / (CpFluid * this->GenMassFlowRate);
                this->SteamOutletEnthalpy =
                    state.dataLoopNodes->Node(this->GeneratorInletNodeNum).Enthalpy - this->QGenerator / this->GenMassFlowRate;
            }

        } else { // using a steam plant for the generator

            // enthalpy of dry steam at generator inlet
            Real64 EnthSteamOutDry = FluidProperties::GetSatEnthalpyRefrig(state,
                                                                           fluidNameSteam,
                                                                           state.dataLoopNodes->Node(this->GeneratorInletNodeNum).Temp,
                                                                           1.0,
                                                                           this->SteamFluidIndex,
                                                                           std::string{calcChillerAbsorptionIndirect} + this->Name);

            // enthalpy of wet steam at generator inlet
            Real64 EnthSteamOutWet = FluidProperties::GetSatEnthalpyRefrig(state,
                                                                           fluidNameSteam,
                                                                           state.dataLoopNodes->Node(this->GeneratorInletNodeNum).Temp,
                                                                           0.0,
                                                                           this->SteamFluidIndex,
                                                                           std::string{calcChillerAbsorptionIndirect} + this->Name);

            // temperature difference of fluid through generator
            Real64 SteamDeltaT = this->GeneratorSubcool;

            // generator outlet temperature
            Real64 SteamOutletTemp = state.dataLoopNodes->Node(this->GeneratorInletNodeNum).Temp - SteamDeltaT;

            // heat of vaporization of steam
            Real64 HfgSteam = EnthSteamOutDry - EnthSteamOutWet;
            CpFluid = FluidProperties::GetSpecificHeatGlycol(
                state, fluidNameWater, SteamOutletTemp, const_cast<int &>(waterIndex), std::string{calcChillerAbsorptionIndirect} + this->Name);
            this->GenMassFlowRate = this->QGenerator / (HfgSteam + CpFluid * SteamDeltaT);
            PlantUtilities::SetComponentFlowRate(state,
                                                 this->GenMassFlowRate,
                                                 this->GeneratorInletNodeNum,
                                                 this->GeneratorOutletNodeNum,
                                                 this->GenLoopNum,
                                                 this->GenLoopSideNum,
                                                 this->GenBranchNum,
                                                 this->GenCompNum);

            if (this->GenMassFlowRate <= 0.0) {
                this->GenOutletTemp = state.dataLoopNodes->Node(this->GeneratorInletNodeNum).Temp;
                this->SteamOutletEnthalpy = state.dataLoopNodes->Node(this->GeneratorInletNodeNum).Enthalpy;
            } else {
                this->GenOutletTemp = state.dataLoopNodes->Node(this->GeneratorInletNodeNum).Temp - SteamDeltaT;
                this->SteamOutletEnthalpy = FluidProperties::GetSatEnthalpyRefrig(state,
                                                                                  fluidNameSteam,
                                                                                  state.dataLoopNodes->Node(this->GeneratorInletNodeNum).Temp,
                                                                                  0.0,
                                                                                  this->SteamFluidIndex,
                                                                                  std::string{LoopLossesChillerAbsorptionIndirect} + this->Name);
                CpFluid = FluidProperties::GetSpecificHeatGlycol(state,
                                                                 fluidNameWater,
                                                                 state.dataLoopNodes->Node(this->GeneratorInletNodeNum).Temp,
                                                                 const_cast<int &>(waterIndex),
                                                                 std::string{calcChillerAbsorptionIndirect} + this->Name);

                this->SteamOutletEnthalpy -= CpFluid * SteamDeltaT;

                //************************* Loop Losses *****************************

                // temperature of condensed steam leaving generator (after condensate trap)
                Real64 TempWaterAtmPress = FluidProperties::GetSatTemperatureRefrig(state,
                                                                                    fluidNameSteam,
                                                                                    state.dataEnvrn->OutBaroPress,
                                                                                    this->SteamFluidIndex,
                                                                                    std::string{LoopLossesChillerAbsorptionIndirect} + this->Name);

                // enthalpy  of condensed steam leaving generator (after condensate trap)
                Real64 EnthAtAtmPress = FluidProperties::GetSatEnthalpyRefrig(state,
                                                                              fluidNameSteam,
                                                                              TempWaterAtmPress,
                                                                              0.0,
                                                                              this->SteamFluidIndex,
                                                                              std::string{LoopLossesChillerAbsorptionIndirectSpace} + this->Name);

                // Point 4 at atm - loop delta subcool during return journey back to pump

                // temperature of condensed steam entering pump (includes loop losses)
                Real64 TempLoopOutToPump = TempWaterAtmPress - this->LoopSubcool;

                // Reported value of coil outlet enthalpy at the node to match the node outlet temperature

                // enthalpy of condensed steam entering pump (includes loop losses)
                Real64 EnthPumpInlet = EnthAtAtmPress - CpFluid * this->LoopSubcool;

                // Point 3-Point 5,
                this->EnergyLossToEnvironment = this->GenMassFlowRate * (this->SteamOutletEnthalpy - EnthPumpInlet);

                //************************* Loop Losses *****************************

                this->GenOutletTemp = TempLoopOutToPump;
                this->SteamOutletEnthalpy = EnthPumpInlet;

            } // IF(GenMassFlowRate .LE. 0.0d0)THEN

        } // IF(IndirectAbsorber(ChillNum)%GenHeatSourceType == DataLoopNode::NodeFluidType::Water)THEN

    } // IF(GeneratorInletNode .GT. 0)THEN

    // convert power to energy
    this->GeneratorEnergy = this->QGenerator * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
    this->EvaporatorEnergy = this->QEvaporator * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
    this->CondenserEnergy = this->QCondenser * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
    this->PumpingEnergy = this->PumpingPower * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;

    //                              ------
    //                            /        \.
    //                          /           |
    //                       6/-------------1 - Boiler Outlet Temp/Enthalpy/Pressure
    //                    /  /             /.
    //                 /    /             / . \_
    //               /    /              /  .  _pressure drop (PD) across steam pressure regulator
    // P           /     /              /   . /
    // r         5      /              /    .
    // e        /    3-2'-------------2------ - Generator Inlet Temp/Enthalpy/Pressure
    // s       /     |/              /
    // s      /      |  PD across   /      2-2' latent heat of vaporization (neglecting amount of superheat due to PD)
    // u     /      /| condensate  /       1-3  delta H in generator
    // r    /      / |   trap     /        2'-3 subcooling of hot water in generator
    // e   4------/--3'          /         3-3' pressure drop at generator hot-water condensate trap
    //           /              /          3-4  loop subcooling back to loop pump
    //          /              /           4-5  pressure/temp/enthalpy increase due to loop condensate pump
    //         /              /            5-6  heat addition in boiler to return condensate
    //        /              /             6-1  heat of vaporization in boiler of return condensate to steam
    //____________________________________
    //         Enthalpy (H)
}

void IndirectAbsorberSpecs::updateRecords(EnergyPlusData &state, Real64 MyLoad, bool RunFlag)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR:          R. Raustad (FSEC)
    //       DATE WRITTEN:    May 2008

    // PURPOSE OF THIS SUBROUTINE:
    // reporting

    int CondOutletNode = this->CondOutletNodeNum;

    if (MyLoad >= 0 || !RunFlag) {
        // set node temperature
        PlantUtilities::SafeCopyPlantNode(state, this->EvapInletNodeNum, this->EvapOutletNodeNum);
        PlantUtilities::SafeCopyPlantNode(state, this->CondInletNodeNum, CondOutletNode);

        this->Report.PumpingPower = 0.0;
        this->Report.QEvap = 0.0;
        this->Report.QCond = 0.0;
        this->Report.QGenerator = 0.0;
        this->Report.PumpingEnergy = 0.0;
        this->Report.EvapEnergy = 0.0;
        this->Report.CondEnergy = 0.0;
        this->Report.GeneratorEnergy = 0.0;
        this->Report.EvapInletTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp;
        this->Report.CondInletTemp = state.dataLoopNodes->Node(this->CondInletNodeNum).Temp;
        this->Report.CondOutletTemp = state.dataLoopNodes->Node(CondOutletNode).Temp;
        this->Report.EvapOutletTemp = state.dataLoopNodes->Node(this->EvapOutletNodeNum).Temp;
        this->Report.Evapmdot = 0.0;
        this->Report.Condmdot = 0.0;
        this->Report.Genmdot = 0.0;
        this->Report.ActualCOP = 0.0;
        this->Report.ChillerPartLoadRatio = 0.0;
        this->Report.LoopLoss = 0.0;
        this->Report.ChillerCyclingFrac = 0.0;

        if (this->GeneratorInletNodeNum > 0) {
            PlantUtilities::SafeCopyPlantNode(state, this->GeneratorInletNodeNum, this->GeneratorOutletNodeNum);
        }

    } else {
        // set node temperatures
        PlantUtilities::SafeCopyPlantNode(state, this->EvapInletNodeNum, this->EvapOutletNodeNum);
        PlantUtilities::SafeCopyPlantNode(state, this->CondInletNodeNum, CondOutletNode);
        state.dataLoopNodes->Node(this->EvapOutletNodeNum).Temp = this->EvapOutletTemp;
        state.dataLoopNodes->Node(CondOutletNode).Temp = this->CondOutletTemp;

        this->Report.PumpingPower = this->PumpingPower;
        this->Report.QEvap = this->QEvaporator;
        this->Report.QCond = this->QCondenser;
        this->Report.QGenerator = this->QGenerator;
        this->Report.PumpingEnergy = this->PumpingEnergy;
        this->Report.EvapEnergy = this->EvaporatorEnergy;
        this->Report.CondEnergy = this->CondenserEnergy;
        this->Report.GeneratorEnergy = this->GeneratorEnergy;
        this->Report.EvapInletTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp;
        this->Report.CondInletTemp = state.dataLoopNodes->Node(this->CondInletNodeNum).Temp;
        this->Report.CondOutletTemp = state.dataLoopNodes->Node(CondOutletNode).Temp;
        this->Report.EvapOutletTemp = state.dataLoopNodes->Node(this->EvapOutletNodeNum).Temp;
        this->Report.Evapmdot = this->EvapMassFlowRate;
        this->Report.Condmdot = this->CondMassFlowRate;
        this->Report.Genmdot = this->GenMassFlowRate;
        this->Report.LoopLoss = this->EnergyLossToEnvironment;
        this->Report.ChillerCyclingFrac = this->ChillerONOFFCyclingFrac;

        if (this->QGenerator != 0.0) {
            this->Report.ActualCOP = this->QEvaporator / this->QGenerator;
        } else {
            this->Report.ActualCOP = 0.0;
        }

        if (this->GeneratorInletNodeNum > 0) {
            PlantUtilities::SafeCopyPlantNode(state, this->GeneratorInletNodeNum, this->GeneratorOutletNodeNum);
            state.dataLoopNodes->Node(this->GeneratorOutletNodeNum).Temp = this->GenOutletTemp;
        }
    }
}

} // namespace EnergyPlus::ChillerIndirectAbsorption
