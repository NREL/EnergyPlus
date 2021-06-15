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
#include <EnergyPlus/Autosizing/Base.hh>
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataBranchAirLoopPlant.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/FluidCoolers.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutAirNodeManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus::FluidCoolers {

// Module containing the routines dealing with the objects FluidCooler:SingleSpeed and
// FluidCooler:TwoSpeed

// MODULE INFORMATION:
//       AUTHOR         Chandan Sharma
//       DATE WRITTEN   August 2008
//       MODIFIED       April 2010, Chandan Sharma, FSEC
//       RE-ENGINEERED  na

// PURPOSE OF THIS MODULE:
// Model the performance of fluid coolers

// REFERENCES:
// Based on cooling tower by Shirey, Raustad: Dec 2000; Shirey, Sept 2002

// MODULE PARAMETER DEFINITIONS:
std::string const cFluidCooler_SingleSpeed("FluidCooler:SingleSpeed");
std::string const cFluidCooler_TwoSpeed("FluidCooler:TwoSpeed");

PlantComponent *FluidCoolerspecs::factory(EnergyPlusData &state, int objectType, std::string const &objectName)
{
    if (state.dataFluidCoolers->GetFluidCoolerInputFlag) {
        GetFluidCoolerInput(state);
        state.dataFluidCoolers->GetFluidCoolerInputFlag = false;
    }
    // Now look for this particular fluid cooler in the list
    for (auto &fc : state.dataFluidCoolers->SimpleFluidCooler) {
        if (fc.FluidCoolerType_Num == objectType && fc.Name == objectName) {
            return &fc;
        }
    }
    // If we didn't find it, fatal
    ShowFatalError(state, "FluidCooler::factory: Error getting inputs for cooler named: " + objectName);
    // Shut up the compiler
    return nullptr;
}

void FluidCoolerspecs::simulate(EnergyPlusData &state,
                                [[maybe_unused]] const PlantLocation &calledFromLocation,
                                [[maybe_unused]] bool const FirstHVACIteration,
                                [[maybe_unused]] Real64 &CurLoad,
                                bool const RunFlag)
{
    this->initialize(state);
    if (this->FluidCoolerType_Num == DataPlant::TypeOf_FluidCooler_SingleSpd) {
        this->calcSingleSpeed(state);
    } else {
        this->calcTwoSpeed(state);
    }
    this->update(state);
    this->report(state, RunFlag);
}

void FluidCoolerspecs::onInitLoopEquip(EnergyPlusData &state, [[maybe_unused]] const PlantLocation &calledFromLocation)
{
    this->initialize(state);
    this->size(state);
}

void FluidCoolerspecs::getDesignCapacities([[maybe_unused]] EnergyPlusData &state,
                                           [[maybe_unused]] const PlantLocation &calledFromLocation,
                                           Real64 &MaxLoad,
                                           Real64 &MinLoad,
                                           Real64 &OptLoad)
{
    MaxLoad = this->FluidCoolerNominalCapacity;
    OptLoad = this->FluidCoolerNominalCapacity;
    MinLoad = 0.0;
}

void GetFluidCoolerInput(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR:          Chandan Sharma
    //       DATE WRITTEN:    August 2008
    //       MODIFIED         Chandan Sharma, FSEC, April 2010
    //       RE-ENGINEERED    na

    // PURPOSE OF THIS SUBROUTINE:
    // Obtains input data for fluid coolers and stores it in SimpleFluidCooler data structure.

    // METHODOLOGY EMPLOYED:
    // Uses "Get" routines to read in the data.

    // REFERENCES:
    // Based on GetTowerInput subroutine from Don Shirey, Jan 2001 and Sept/Oct 2002;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int NumAlphas = 0;            // Number of elements in the alpha array
    int NumNums = 0;              // Number of elements in the numeric array
    int IOStat = 0;               // IO Status when calling get input subroutine
    bool ErrorsFound(false);      // Logical flag set .TRUE. if errors found while getting input data
    Array1D<Real64> NumArray(16); // Numeric input data array
    Array1D_string AlphArray(5);  // Character string input data array

    // Get number of all Fluid Coolers specified in the input data file (idf)
    int const NumSingleSpeedFluidCoolers = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "FluidCooler:SingleSpeed");
    int const NumTwoSpeedFluidCoolers = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "FluidCooler:TwoSpeed");
    state.dataFluidCoolers->NumSimpleFluidCoolers = NumSingleSpeedFluidCoolers + NumTwoSpeedFluidCoolers;

    if (state.dataFluidCoolers->NumSimpleFluidCoolers <= 0)
        ShowFatalError(state,
                       "No fluid cooler objects found in input, however, a branch object has specified a fluid cooler. Search the input for "
                       "fluid cooler to determine the cause for this error.");

    // See if load distribution manager has already gotten the input
    if (allocated(state.dataFluidCoolers->SimpleFluidCooler)) return;
    state.dataFluidCoolers->GetFluidCoolerInputFlag = false;

    // Allocate data structures to hold fluid cooler input data, report data and fluid cooler inlet conditions
    state.dataFluidCoolers->SimpleFluidCooler.allocate(state.dataFluidCoolers->NumSimpleFluidCoolers);
    state.dataFluidCoolers->UniqueSimpleFluidCoolerNames.reserve(state.dataFluidCoolers->NumSimpleFluidCoolers);

    int FluidCoolerNum = 0;

    // Load data structures with fluid cooler input data
    auto &cCurrentModuleObject = state.dataIPShortCut->cCurrentModuleObject;
    cCurrentModuleObject = cFluidCooler_SingleSpeed;
    for (int SingleSpeedFluidCoolerNumber = 1; SingleSpeedFluidCoolerNumber <= NumSingleSpeedFluidCoolers; ++SingleSpeedFluidCoolerNumber) {
        FluidCoolerNum = SingleSpeedFluidCoolerNumber;
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 SingleSpeedFluidCoolerNumber,
                                                                 AlphArray,
                                                                 NumAlphas,
                                                                 NumArray,
                                                                 NumNums,
                                                                 IOStat,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);
        GlobalNames::VerifyUniqueInterObjectName(state,
                                                 state.dataFluidCoolers->UniqueSimpleFluidCoolerNames,
                                                 AlphArray(1),
                                                 cCurrentModuleObject,
                                                 state.dataIPShortCut->cAlphaFieldNames(1),
                                                 ErrorsFound);

        state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).Name = AlphArray(1);
        state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).FluidCoolerType = cCurrentModuleObject;
        state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).FluidCoolerType_Num = DataPlant::TypeOf_FluidCooler_SingleSpd;
        state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).indexInArray = FluidCoolerNum;
        state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).FluidCoolerMassFlowRateMultiplier = 2.5;
        state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).WaterInletNodeNum =
            NodeInputManager::GetOnlySingleNode(state,
                                                AlphArray(2),
                                                ErrorsFound,
                                                cCurrentModuleObject,
                                                AlphArray(1),
                                                DataLoopNode::NodeFluidType::Water,
                                                DataLoopNode::NodeConnectionType::Inlet,
                                                1,
                                                DataLoopNode::ObjectIsNotParent);
        state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).WaterOutletNodeNum =
            NodeInputManager::GetOnlySingleNode(state,
                                                AlphArray(3),
                                                ErrorsFound,
                                                cCurrentModuleObject,
                                                AlphArray(1),
                                                DataLoopNode::NodeFluidType::Water,
                                                DataLoopNode::NodeConnectionType::Outlet,
                                                1,
                                                DataLoopNode::ObjectIsNotParent);
        BranchNodeConnections::TestCompSet(state, cCurrentModuleObject, AlphArray(1), AlphArray(2), AlphArray(3), "Chilled Water Nodes");
        state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).HighSpeedFluidCoolerUA = NumArray(1);
        if (state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).HighSpeedFluidCoolerUA == DataSizing::AutoSize) {
            state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).HighSpeedFluidCoolerUAWasAutoSized = true;
        }
        state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).FluidCoolerNominalCapacity = NumArray(2);
        state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).DesignEnteringWaterTemp = NumArray(3);
        state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).DesignEnteringAirTemp = NumArray(4);
        state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).DesignEnteringAirWetBulbTemp = NumArray(5);
        state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).DesignWaterFlowRate = NumArray(6);
        if (state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).DesignWaterFlowRate == DataSizing::AutoSize) {
            state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).DesignWaterFlowRateWasAutoSized = true;
        }
        state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).HighSpeedAirFlowRate = NumArray(7);
        if (state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).HighSpeedAirFlowRate == DataSizing::AutoSize) {
            state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).HighSpeedAirFlowRateWasAutoSized = true;
        }
        state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).HighSpeedFanPower = NumArray(8);
        if (state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).HighSpeedFanPower == DataSizing::AutoSize) {
            state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).HighSpeedFanPowerWasAutoSized = true;
        }

        //   outdoor air inlet node
        if (AlphArray(5).empty()) {
            state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).OutdoorAirInletNodeNum = 0;
        } else {
            state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).OutdoorAirInletNodeNum =
                NodeInputManager::GetOnlySingleNode(state,
                                                    AlphArray(5),
                                                    ErrorsFound,
                                                    cCurrentModuleObject,
                                                    state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).Name,
                                                    DataLoopNode::NodeFluidType::Air,
                                                    DataLoopNode::NodeConnectionType::OutsideAirReference,
                                                    1,
                                                    DataLoopNode::ObjectIsNotParent);
            if (!OutAirNodeManager::CheckOutAirNodeNumber(state, state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).OutdoorAirInletNodeNum)) {
                ShowSevereError(state,
                                cCurrentModuleObject + "= \"" + state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).Name + "\" " +
                                    state.dataIPShortCut->cAlphaFieldNames(5) + "= \"" + AlphArray(5) + "\" not valid.");
                ShowContinueError(state, "...does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node.");
                ErrorsFound = true;
            }
        }

        ErrorsFound |=
            state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum)
                .validateSingleSpeedInputs(
                    state, cCurrentModuleObject, AlphArray, state.dataIPShortCut->cNumericFieldNames, state.dataIPShortCut->cAlphaFieldNames);

    } // End Single-Speed fluid cooler Loop

    cCurrentModuleObject = cFluidCooler_TwoSpeed;
    for (int TwoSpeedFluidCoolerNumber = 1; TwoSpeedFluidCoolerNumber <= NumTwoSpeedFluidCoolers; ++TwoSpeedFluidCoolerNumber) {
        FluidCoolerNum = NumSingleSpeedFluidCoolers + TwoSpeedFluidCoolerNumber;
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 TwoSpeedFluidCoolerNumber,
                                                                 AlphArray,
                                                                 NumAlphas,
                                                                 NumArray,
                                                                 NumNums,
                                                                 IOStat,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);
        GlobalNames::VerifyUniqueInterObjectName(state,
                                                 state.dataFluidCoolers->UniqueSimpleFluidCoolerNames,
                                                 AlphArray(1),
                                                 cCurrentModuleObject,
                                                 state.dataIPShortCut->cAlphaFieldNames(1),
                                                 ErrorsFound);

        state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).Name = AlphArray(1);
        state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).FluidCoolerType = cCurrentModuleObject;
        state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).FluidCoolerType_Num = DataPlant::TypeOf_FluidCooler_TwoSpd;
        state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).indexInArray = FluidCoolerNum;
        state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).FluidCoolerMassFlowRateMultiplier = 2.5;
        state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).WaterInletNodeNum =
            NodeInputManager::GetOnlySingleNode(state,
                                                AlphArray(2),
                                                ErrorsFound,
                                                cCurrentModuleObject,
                                                AlphArray(1),
                                                DataLoopNode::NodeFluidType::Water,
                                                DataLoopNode::NodeConnectionType::Inlet,
                                                1,
                                                DataLoopNode::ObjectIsNotParent);
        state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).WaterOutletNodeNum =
            NodeInputManager::GetOnlySingleNode(state,
                                                AlphArray(3),
                                                ErrorsFound,
                                                cCurrentModuleObject,
                                                AlphArray(1),
                                                DataLoopNode::NodeFluidType::Water,
                                                DataLoopNode::NodeConnectionType::Outlet,
                                                1,
                                                DataLoopNode::ObjectIsNotParent);
        BranchNodeConnections::TestCompSet(state, cCurrentModuleObject, AlphArray(1), AlphArray(2), AlphArray(3), "Chilled Water Nodes");

        state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).HighSpeedFluidCoolerUA = NumArray(1);
        if (state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).HighSpeedFluidCoolerUA == DataSizing::AutoSize) {
            state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).HighSpeedFluidCoolerUAWasAutoSized = true;
        }
        state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).LowSpeedFluidCoolerUA = NumArray(2);
        if (state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).LowSpeedFluidCoolerUA == DataSizing::AutoSize) {
            state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).LowSpeedFluidCoolerUAWasAutoSized = true;
        }
        state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).LowSpeedFluidCoolerUASizingFactor = NumArray(3);
        state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).FluidCoolerNominalCapacity = NumArray(4);
        state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).FluidCoolerLowSpeedNomCap = NumArray(5);
        if (state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).FluidCoolerLowSpeedNomCap == DataSizing::AutoSize) {
            state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).FluidCoolerLowSpeedNomCapWasAutoSized = true;
        }
        state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).FluidCoolerLowSpeedNomCapSizingFactor = NumArray(6);
        state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).DesignEnteringWaterTemp = NumArray(7);
        state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).DesignEnteringAirTemp = NumArray(8);
        state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).DesignEnteringAirWetBulbTemp = NumArray(9);
        state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).DesignWaterFlowRate = NumArray(10);
        if (state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).DesignWaterFlowRate == DataSizing::AutoSize) {
            state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).DesignWaterFlowRateWasAutoSized = true;
        }
        state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).HighSpeedAirFlowRate = NumArray(11);
        if (state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).HighSpeedAirFlowRate == DataSizing::AutoSize) {
            state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).HighSpeedAirFlowRateWasAutoSized = true;
        }
        state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).HighSpeedFanPower = NumArray(12);
        if (state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).HighSpeedFanPower == DataSizing::AutoSize) {
            state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).HighSpeedFanPowerWasAutoSized = true;
        }
        state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).LowSpeedAirFlowRate = NumArray(13);
        if (state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).LowSpeedAirFlowRate == DataSizing::AutoSize) {
            state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).LowSpeedAirFlowRateWasAutoSized = true;
        }
        state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).LowSpeedAirFlowRateSizingFactor = NumArray(14);
        state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).LowSpeedFanPower = NumArray(15);
        if (state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).LowSpeedFanPower == DataSizing::AutoSize) {
            state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).LowSpeedFanPowerWasAutoSized = true;
        }
        state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).LowSpeedFanPowerSizingFactor = NumArray(16);

        //   outdoor air inlet node
        if (AlphArray(5).empty()) {
            state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).OutdoorAirInletNodeNum = 0;
        } else {
            state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).OutdoorAirInletNodeNum =
                NodeInputManager::GetOnlySingleNode(state,
                                                    AlphArray(5),
                                                    ErrorsFound,
                                                    cCurrentModuleObject,
                                                    state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).Name,
                                                    DataLoopNode::NodeFluidType::Air,
                                                    DataLoopNode::NodeConnectionType::OutsideAirReference,
                                                    1,
                                                    DataLoopNode::ObjectIsNotParent);
            if (!OutAirNodeManager::CheckOutAirNodeNumber(state, state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).OutdoorAirInletNodeNum)) {
                ShowSevereError(state,
                                cCurrentModuleObject + "= \"" + state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).Name + "\" " +
                                    state.dataIPShortCut->cAlphaFieldNames(5) + "= \"" + AlphArray(5) + "\" not valid.");
                ShowContinueError(state, "...does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node.");
                ErrorsFound = true;
            }
        }

        ErrorsFound |=
            state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum)
                .validateTwoSpeedInputs(
                    state, cCurrentModuleObject, AlphArray, state.dataIPShortCut->cNumericFieldNames, state.dataIPShortCut->cAlphaFieldNames);
    }

    if (ErrorsFound) {
        ShowFatalError(state, "Errors found in getting fluid cooler input.");
    }
}

void FluidCoolerspecs::setupOutputVars(EnergyPlusData &state)
{

    SetupOutputVariable(state, "Cooling Tower Inlet Temperature", OutputProcessor::Unit::C, this->InletWaterTemp, "System", "Average", this->Name);
    SetupOutputVariable(state, "Cooling Tower Outlet Temperature", OutputProcessor::Unit::C, this->OutletWaterTemp, "System", "Average", this->Name);
    SetupOutputVariable(state, "Cooling Tower Mass Flow Rate", OutputProcessor::Unit::kg_s, this->WaterMassFlowRate, "System", "Average", this->Name);
    SetupOutputVariable(state, "Cooling Tower Heat Transfer Rate", OutputProcessor::Unit::W, this->Qactual, "System", "Average", this->Name);
    SetupOutputVariable(state, "Cooling Tower Fan Electricity Rate", OutputProcessor::Unit::W, this->FanPower, "System", "Average", this->Name);
    SetupOutputVariable(state,
                        "Cooling Tower Fan Electricity Energy",
                        OutputProcessor::Unit::J,
                        this->FanEnergy,
                        "System",
                        "Sum",
                        this->Name,
                        _,
                        "Electricity",
                        "HeatRejection",
                        _,
                        "Plant");
}

bool FluidCoolerspecs::validateSingleSpeedInputs(EnergyPlusData &state,
                                                 std::string const &cCurrentModuleObject,
                                                 Array1D<std::string> const &AlphArray,
                                                 Array1D<std::string> const &cNumericFieldNames,
                                                 Array1D<std::string> const &cAlphaFieldNames)
{
    // FUNCTION INFORMATION:
    //       AUTHOR:          Chandan Sharma
    //       DATE WRITTEN:    August 2008
    //       MODIFIED         Chandan Sharma, FSEC, April 2010
    //       RE-ENGINEERED    Jason Glazer, GARD Analytics, February 2015, refactor into a separate function

    // PURPOSE OF THIS FUNCTION:
    // Separate the testing of inputs related to design so that it could be called from the unit tests

    // REFERENCES:
    // Based on GetTowerInput subroutine from Don Shirey, Jan 2001 and Sept/Oct 2002;

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    bool ErrorsFound = false;

    //   Design entering water temperature, design entering air temperature and design entering air
    //   wetbulb temperature must be specified for the both the performance input methods
    if (this->DesignEnteringWaterTemp <= 0.0) {
        ShowSevereError(state,
                        cCurrentModuleObject + " = \"" + AlphArray(1) + "\", invalid data for \"" + cNumericFieldNames(3) +
                            "\", entered value <= 0.0, but must be > 0 ");
        ErrorsFound = true;
    }
    if (this->DesignEnteringAirTemp <= 0.0) {
        ShowSevereError(state,
                        cCurrentModuleObject + " = \"" + AlphArray(1) + "\", invalid data for \"" + cNumericFieldNames(4) +
                            "\", entered value <= 0.0, but must be > 0 ");
        ErrorsFound = true;
    }
    if (this->DesignEnteringAirWetBulbTemp <= 0.0) {
        ShowSevereError(state,
                        cCurrentModuleObject + " = \"" + AlphArray(1) + "\", invalid data for \"" + cNumericFieldNames(5) +
                            "\", entered value <= 0.0, but must be > 0 ");
        ErrorsFound = true;
    }
    if (this->DesignEnteringWaterTemp <= this->DesignEnteringAirTemp) {
        ShowSevereError(state,
                        cCurrentModuleObject + "= \"" + AlphArray(1) + "\"," + cNumericFieldNames(3) + " must be greater than " +
                            cNumericFieldNames(4) + '.');
        ErrorsFound = true;
    }
    if (this->DesignEnteringAirTemp <= this->DesignEnteringAirWetBulbTemp) {
        ShowSevereError(state,
                        cCurrentModuleObject + "= \"" + AlphArray(1) + "\"," + cNumericFieldNames(4) + " must be greater than " +
                            cNumericFieldNames(5) + '.');
        ErrorsFound = true;
    }
    if (this->HighSpeedAirFlowRate <= 0.0 && this->HighSpeedAirFlowRate != DataSizing::AutoSize) {
        ShowSevereError(state,
                        cCurrentModuleObject + " = \"" + AlphArray(1) + "\", invalid data for \"" + cNumericFieldNames(7) +
                            "\", entered value <= 0.0, but must be > 0 for " + cAlphaFieldNames(4) + " = \"" + AlphArray(4) + "\".");
        ErrorsFound = true;
    }
    if (this->DesignWaterFlowRate <= 0.0 && !this->DesignWaterFlowRateWasAutoSized) {
        ShowSevereError(state,
                        cCurrentModuleObject + " = \"" + AlphArray(1) + "\", invalid data for \"" + cNumericFieldNames(6) +
                            "\", entered value <= 0.0, but must be > 0 for " + cAlphaFieldNames(4) + " = \"" + AlphArray(4) + "\".");
        ErrorsFound = true;
    }
    if (this->HighSpeedFanPower <= 0.0 && this->HighSpeedFanPower != DataSizing::AutoSize) {
        ShowSevereError(state,
                        cCurrentModuleObject + " = \"" + AlphArray(1) + "\", invalid data for \"" + cNumericFieldNames(8) +
                            "\", entered value <= 0.0, but must be > 0 for " + cAlphaFieldNames(4) + " = \"" + AlphArray(4) + "\".");
        ErrorsFound = true;
    }

    //   Check various inputs for both the performance input methods
    if (UtilityRoutines::SameString(AlphArray(4), "UFactorTimesAreaAndDesignWaterFlowRate")) {
        this->PerformanceInputMethod_Num = PerfInputMethod::U_FACTOR;
        if (this->HighSpeedFluidCoolerUA <= 0.0 && this->HighSpeedFluidCoolerUA != DataSizing::AutoSize) {
            ShowSevereError(state,
                            cCurrentModuleObject + " = \"" + AlphArray(1) + "\", invalid data for \"" + cNumericFieldNames(1) +
                                "\", entered value <= 0.0, but must be > 0 for " + cAlphaFieldNames(4) + " = \"" + AlphArray(4) + "\".");
            ErrorsFound = true;
        }
    } else if (UtilityRoutines::SameString(AlphArray(4), "NominalCapacity")) {
        this->PerformanceInputMethod_Num = PerfInputMethod::NOMINAL_CAPACITY;
        if (this->FluidCoolerNominalCapacity <= 0.0) {
            ShowSevereError(state,
                            cCurrentModuleObject + " = \"" + AlphArray(1) + "\", invalid data for \"" + cNumericFieldNames(2) +
                                "\", entered value <= 0.0, but must be > 0 for " + cAlphaFieldNames(4) + " = \"" + AlphArray(4) + "\".");
            ErrorsFound = true;
        }
        if (this->HighSpeedFluidCoolerUA != 0.0) {
            if (this->HighSpeedFluidCoolerUA > 0.0) {
                ShowWarningError(state,
                                 cCurrentModuleObject + "= \"" + this->Name +
                                     "\". Nominal fluid cooler capacity and design fluid cooler UA have been specified.");
            } else {
                ShowWarningError(state,
                                 cCurrentModuleObject + "= \"" + this->Name +
                                     "\". Nominal fluid cooler capacity has been specified and design fluid cooler UA is being autosized.");
            }
            ShowContinueError(state,
                              "Design fluid cooler UA field must be left blank when nominal fluid cooler capacity performance input method is used.");
            ShowContinueError(state, "Design fluid cooler UA value will be reset to zero and the simulation continuous.");
            this->HighSpeedFluidCoolerUA = 0.0;
        }
    } else { // Fluid cooler performance input method is not specified as a valid "choice"
        ShowSevereError(state, cCurrentModuleObject + "= \"" + AlphArray(1) + "\", invalid " + cAlphaFieldNames(4) + " = \"" + AlphArray(4) + "\".");
        ShowContinueError(state, R"(... must be "UFactorTimesAreaAndDesignWaterFlowRate" or "NominalCapacity".)");
        ErrorsFound = true;
    }
    return ErrorsFound;
}

bool FluidCoolerspecs::validateTwoSpeedInputs(EnergyPlusData &state,
                                              std::string const &cCurrentModuleObject,
                                              Array1D<std::string> const &AlphArray,
                                              Array1D<std::string> const &cNumericFieldNames,
                                              Array1D<std::string> const &cAlphaFieldNames)
{
    // FUNCTION INFORMATION:
    //       AUTHOR:          Chandan Sharma
    //       DATE WRITTEN:    August 2008
    //       MODIFIED         Chandan Sharma, FSEC, April 2010
    //       RE-ENGINEERED    Jason Glazer, GARD Analytics, February 2015, refactor into a separate function

    // PURPOSE OF THIS FUNCTION:
    // Separate the testing of inputs related to design so that it could be called from the unit tests

    // REFERENCES:
    // Based on GetTowerInput subroutine from Don Shirey, Jan 2001 and Sept/Oct 2002;

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    bool ErrorsFound = false;

    //   Design entering water temperature, design entering air temperature and design entering air
    //   wetbulb temperature must be specified for the both the performance input methods
    if (this->DesignEnteringWaterTemp <= 0.0) {
        ShowSevereError(state,
                        cCurrentModuleObject + " = \"" + AlphArray(1) + "\", invalid data for \"" + cNumericFieldNames(7) +
                            "\", entered value <= 0.0, but must be > 0 ");
        ErrorsFound = true;
    }
    if (this->DesignEnteringAirTemp <= 0.0) {
        ShowSevereError(state,
                        cCurrentModuleObject + " = \"" + AlphArray(1) + "\", invalid data for \"" + cNumericFieldNames(8) +
                            "\", entered value <= 0.0, but must be > 0 ");
        ErrorsFound = true;
    }
    if (this->DesignEnteringAirWetBulbTemp <= 0.0) {
        ShowSevereError(state,
                        cCurrentModuleObject + " = \"" + AlphArray(1) + "\", invalid data for \"" + cNumericFieldNames(9) +
                            "\", entered value <= 0.0, but must be > 0 ");
        ErrorsFound = true;
    }
    if (this->DesignEnteringWaterTemp <= this->DesignEnteringAirTemp) {
        ShowSevereError(state,
                        cCurrentModuleObject + " = \"" + AlphArray(1) + "\", " + cNumericFieldNames(7) + " must be greater than " +
                            cNumericFieldNames(8) + '.');
        ErrorsFound = true;
    }
    if (this->DesignEnteringAirTemp <= this->DesignEnteringAirWetBulbTemp) {
        ShowSevereError(state,
                        cCurrentModuleObject + " = \"" + AlphArray(1) + "\", " + cNumericFieldNames(8) + " must be greater than " +
                            cNumericFieldNames(9) + '.');
        ErrorsFound = true;
    }

    //   Check various inputs for both the performance input methods
    if (this->DesignWaterFlowRate <= 0.0 && !this->DesignWaterFlowRateWasAutoSized) {
        ShowSevereError(state,
                        cCurrentModuleObject + "= \"" + AlphArray(1) + "\", invalid data for \"" + cNumericFieldNames(10) +
                            "\", entered value <= 0.0, but must be > 0 for " + cAlphaFieldNames(4) + "= \"" + AlphArray(4) + "\".");
        ErrorsFound = true;
    }
    if (this->HighSpeedAirFlowRate <= 0.0 && !this->HighSpeedAirFlowRateWasAutoSized) {
        ShowSevereError(state,
                        cCurrentModuleObject + "= \"" + AlphArray(1) + "\", invalid data for \"" + cNumericFieldNames(11) +
                            "\", entered value <= 0.0, but must be > 0 for " + cAlphaFieldNames(4) + "= \"" + AlphArray(4) + "\".");
        ErrorsFound = true;
    }
    if (this->LowSpeedAirFlowRate <= 0.0 && !this->LowSpeedAirFlowRateWasAutoSized) {
        ShowSevereError(state,
                        cCurrentModuleObject + "= \"" + AlphArray(1) + "\", invalid data for \"" + cNumericFieldNames(13) +
                            "\", entered value <= 0.0, but must be > 0 for " + cAlphaFieldNames(4) + "= \"" + AlphArray(4) + "\".");
        ErrorsFound = true;
    }
    //   High speed air flow rate must be greater than low speed air flow rate.
    //   Can't tell yet if autosized, check later in InitFluidCooler.
    if (this->HighSpeedAirFlowRate <= this->LowSpeedAirFlowRate && !this->HighSpeedAirFlowRateWasAutoSized) {
        ShowSevereError(state,
                        cCurrentModuleObject + "= \"" + this->Name +
                            "\". Fluid cooler air flow rate at low fan speed must be less than the air flow rate at high fan speed.");
        ErrorsFound = true;
    }
    if (this->HighSpeedFanPower <= 0.0 && !this->HighSpeedFanPowerWasAutoSized) {
        ShowSevereError(state,
                        cCurrentModuleObject + " = \"" + AlphArray(1) + "\", invalid data for \"" + cNumericFieldNames(12) +
                            "\", entered value <= 0.0, but must be > 0 for " + cAlphaFieldNames(4) + " = \"" + AlphArray(4) + "\".");
        ErrorsFound = true;
    }
    if (this->LowSpeedFanPower <= 0.0 && !this->LowSpeedFanPowerWasAutoSized) {
        ShowSevereError(state,
                        cCurrentModuleObject + " = \"" + AlphArray(1) + "\", invalid data for \"" + cNumericFieldNames(15) +
                            "\", entered value <= 0.0, but must be > 0 for " + cAlphaFieldNames(4) + " = \"" + AlphArray(4) + "\".");
        ErrorsFound = true;
    }
    if (this->HighSpeedFanPower <= this->LowSpeedFanPower && !this->HighSpeedFanPowerWasAutoSized) {
        ShowSevereError(state,
                        cCurrentModuleObject + "= \"" + this->Name + "\". Fluid cooler low speed fan power must be less than high speed fan power.");
        ErrorsFound = true;
    }

    if (UtilityRoutines::SameString(AlphArray(4), "UFactorTimesAreaAndDesignWaterFlowRate")) {
        this->PerformanceInputMethod_Num = PerfInputMethod::U_FACTOR;
        if (this->HighSpeedFluidCoolerUA <= 0.0 && !this->HighSpeedFluidCoolerUAWasAutoSized) {
            ShowSevereError(state,
                            cCurrentModuleObject + " = \"" + AlphArray(1) + "\", invalid data for \"" + cNumericFieldNames(1) +
                                "\", entered value <= 0.0, but must be > 0 for " + cAlphaFieldNames(4) + " = \"" + AlphArray(4) + "\".");
            ErrorsFound = true;
        }
        if (this->LowSpeedFluidCoolerUA <= 0.0 && !this->LowSpeedFluidCoolerUAWasAutoSized) {
            ShowSevereError(state,
                            cCurrentModuleObject + " = \"" + AlphArray(1) + "\", invalid data for \"" + cNumericFieldNames(2) +
                                "\", entered value <= 0.0, but must be > 0 for " + cAlphaFieldNames(4) + " = \"" + AlphArray(4) + "\".");
            ErrorsFound = true;
        }
        if (this->HighSpeedFluidCoolerUA <= this->LowSpeedFluidCoolerUA && !this->HighSpeedFluidCoolerUAWasAutoSized) {
            ShowSevereError(state,
                            cCurrentModuleObject + "= \"" + this->Name +
                                "\". Fluid cooler UA at low fan speed must be less than the fluid cooler UA at high fan speed.");
            ErrorsFound = true;
        }
    } else if (UtilityRoutines::SameString(AlphArray(4), "NominalCapacity")) {
        this->PerformanceInputMethod_Num = PerfInputMethod::NOMINAL_CAPACITY;
        if (this->FluidCoolerNominalCapacity <= 0.0) {
            ShowSevereError(state,
                            cCurrentModuleObject + " = \"" + AlphArray(1) + "\", invalid data for \"" + cNumericFieldNames(4) +
                                "\", entered value <= 0.0, but must be > 0 for " + cAlphaFieldNames(4) + "= \"" + AlphArray(4) + "\".");
            ErrorsFound = true;
        }
        if (this->FluidCoolerLowSpeedNomCap <= 0.0 && !this->FluidCoolerLowSpeedNomCapWasAutoSized) {
            ShowSevereError(state,
                            cCurrentModuleObject + " = \"" + AlphArray(1) + "\", invalid data for \"" + cNumericFieldNames(5) +
                                "\", entered value <= 0.0, but must be > 0 for " + cAlphaFieldNames(4) + "= \"" + AlphArray(4) + "\".");
            ErrorsFound = true;
        }
        if (this->HighSpeedFluidCoolerUA != 0.0) {
            if (this->HighSpeedFluidCoolerUA > 0.0) {
                ShowSevereError(state,
                                cCurrentModuleObject + "= \"" + this->Name +
                                    "\". Nominal capacity input method and fluid cooler UA at high fan speed have been specified.");
            } else {
                ShowSevereError(state,
                                cCurrentModuleObject + "= \"" + this->Name +
                                    "\". Nominal capacity input method has been specified and fluid cooler UA at high fan speed is being autosized.");
            }
            ShowContinueError(
                state, "Fluid cooler UA at high fan speed must be left blank when nominal fluid cooler capacity performance input method is used.");
            ErrorsFound = true;
        }
        if (this->LowSpeedFluidCoolerUA != 0.0) {
            if (this->LowSpeedFluidCoolerUA > 0.0) {
                ShowSevereError(state,
                                cCurrentModuleObject + "= \"" + this->Name +
                                    "\". Nominal capacity input method and fluid cooler UA at low fan speed have been specified.");
            } else {
                ShowSevereError(state,
                                cCurrentModuleObject + "= \"" + this->Name +
                                    "\". Nominal capacity input method has been specified and fluid cooler UA at low fan speed is being autosized.");
            }
            ShowContinueError(
                state, "Fluid cooler UA at low fan speed must be left blank when nominal fluid cooler capacity performance input method is used.");
            ErrorsFound = true;
        }
        if (this->FluidCoolerLowSpeedNomCap >= this->FluidCoolerNominalCapacity) {
            ShowSevereError(state,
                            cCurrentModuleObject + " = \"" + this->Name +
                                "\". Low-speed nominal capacity must be less than the high-speed nominal capacity.");
            ErrorsFound = true;
        }
    } else { // Fluid cooler performance input method is not specified as a valid "choice"
        ShowSevereError(state, cCurrentModuleObject + "= \"" + AlphArray(1) + "\", invalid " + cAlphaFieldNames(4) + "= \"" + AlphArray(4) + "\".");
        ShowContinueError(state, R"(... must be "UFactorTimesAreaAndDesignWaterFlowRate" or "NominalCapacity".)");
        ErrorsFound = true;
    }
    return ErrorsFound;
}

void FluidCoolerspecs::oneTimeInit(EnergyPlusData &state)
{
    bool ErrorsFound = false;
    // Locate the tower on the plant loops for later usage
    PlantUtilities::ScanPlantLoopsForObject(
        state, this->Name, this->FluidCoolerType_Num, this->LoopNum, this->LoopSideNum, this->BranchNum, this->CompNum, ErrorsFound, _, _, _, _, _);

    if (ErrorsFound) {
        ShowFatalError(state, "InitFluidCooler: Program terminated due to previous condition(s).");
    }
}

void FluidCoolerspecs::initEachEnvironment(EnergyPlusData &state)
{
    static constexpr std::string_view RoutineName("FluidCoolerspecs::initEachEnvironment");
    Real64 const rho = FluidProperties::GetDensityGlycol(state,
                                                         state.dataPlnt->PlantLoop(this->LoopNum).FluidName,
                                                         DataGlobalConstants::InitConvTemp,
                                                         state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex,
                                                         RoutineName);
    this->DesWaterMassFlowRate = this->DesignWaterFlowRate * rho;
    PlantUtilities::InitComponentNodes(state,
                                       0.0,
                                       this->DesWaterMassFlowRate,
                                       this->WaterInletNodeNum,
                                       this->WaterOutletNodeNum,
                                       this->LoopNum,
                                       this->LoopSideNum,
                                       this->BranchNum,
                                       this->CompNum);
}

void FluidCoolerspecs::initialize(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Chandan Sharma
    //       DATE WRITTEN   August 2008
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine is for initializations of the fluid cooler components and for
    // final checking of fluid cooler inputs (post autosizing)

    // METHODOLOGY EMPLOYED:
    // Uses the status flags to trigger initializations.

    // REFERENCES:
    // Based on InitTower subroutine by Don Shirey Sept/Oct 2002, F Buhl Oct 2002

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

    if (this->oneTimeInitFlag) {
        this->setupOutputVars(state);
        this->oneTimeInit(state);
        this->oneTimeInitFlag = false;
    }

    // Begin environment initializations
    if (this->beginEnvrnInit && state.dataGlobal->BeginEnvrnFlag && (state.dataPlnt->PlantFirstSizesOkayToFinalize)) {
        this->initEachEnvironment(state);
        this->beginEnvrnInit = false;
    }

    if (!state.dataGlobal->BeginEnvrnFlag) {
        this->beginEnvrnInit = true;
    }

    // Each time initializations
    this->WaterTemp = state.dataLoopNodes->Node(this->WaterInletNodeNum).Temp;

    if (this->OutdoorAirInletNodeNum != 0) {
        this->AirTemp = state.dataLoopNodes->Node(this->OutdoorAirInletNodeNum).Temp;
        this->AirHumRat = state.dataLoopNodes->Node(this->OutdoorAirInletNodeNum).HumRat;
        this->AirPress = state.dataLoopNodes->Node(this->OutdoorAirInletNodeNum).Press;
        this->AirWetBulb = state.dataLoopNodes->Node(this->OutdoorAirInletNodeNum).OutAirWetBulb;
    } else {
        this->AirTemp = state.dataEnvrn->OutDryBulbTemp;
        this->AirHumRat = state.dataEnvrn->OutHumRat;
        this->AirPress = state.dataEnvrn->OutBaroPress;
        this->AirWetBulb = state.dataEnvrn->OutWetBulbTemp;
    }

    this->WaterMassFlowRate = PlantUtilities::RegulateCondenserCompFlowReqOp(state,
                                                                             this->LoopNum,
                                                                             this->LoopSideNum,
                                                                             this->BranchNum,
                                                                             this->CompNum,
                                                                             this->DesWaterMassFlowRate * this->FluidCoolerMassFlowRateMultiplier);

    PlantUtilities::SetComponentFlowRate(state,
                                         this->WaterMassFlowRate,
                                         this->WaterInletNodeNum,
                                         this->WaterOutletNodeNum,
                                         this->LoopNum,
                                         this->LoopSideNum,
                                         this->BranchNum,
                                         this->CompNum);
}

void FluidCoolerspecs::size(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Chandan Sharma
    //       DATE WRITTEN   August 2008
    //       MODIFIED       April 2010, Chandan Sharma, FSEC
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine is for sizing fluid cooler Components for which capacities and flow rates
    // have not been specified in the input. This subroutine also calculates fluid cooler UA if the user
    // has specified fluid cooler performance via the "Nominal Capacity" method.

    // METHODOLOGY EMPLOYED:
    // Obtains condenser flow rate from the plant sizing array. If fluid cooler performance is specified
    // via the "Nominal Capacity" method, the water flow rate is directly proportional to capacity.

    // REFERENCES:
    // Based on SizeTower by Don Shirey, Sept/Oct 2002; Richard Raustad, Feb 2005

    // SUBROUTINE PARAMETER DEFINITIONS:
    constexpr int MaxIte(500);    // Maximum number of iterations
    constexpr Real64 Acc(0.0001); // Accuracy of result
    static constexpr std::string_view CalledFrom("SizeFluidCooler");

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int SolFla;                     // Flag of solver
    Real64 DesFluidCoolerLoad(0.0); // Design fluid cooler load [W]
    Real64 UA0;                     // Lower bound for UA [W/C]
    Real64 UA1;                     // Upper bound for UA [W/C]
    Real64 UA;                      // Calculated UA value
    Real64 OutWaterTempAtUA0;       // Water outlet temperature at UA0
    Real64 OutWaterTempAtUA1;       // Water outlet temperature at UA1
    std::array<Real64, 5> Par;      // Parameter array need for RegulaFalsi routine
    std::string equipName;
    Real64 Cp;                            // local specific heat for fluid
    Real64 rho;                           // local density for fluid
    Real64 tmpHighSpeedFanPower;          // local temporary for high speed fan power
    Real64 tmpHighSpeedEvapFluidCoolerUA; // local temporary for high speed cooler UA
    bool ErrorsFound;

    Real64 tmpDesignWaterFlowRate = this->DesignWaterFlowRate;
    Real64 tmpHighSpeedAirFlowRate = this->HighSpeedAirFlowRate;
    // Find the appropriate Plant Sizing object
    int PltSizCondNum = state.dataPlnt->PlantLoop(this->LoopNum).PlantSizNum;

    if (this->DesignWaterFlowRateWasAutoSized) {
        if (PltSizCondNum > 0) {
            if (state.dataSize->PlantSizData(PltSizCondNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                tmpDesignWaterFlowRate = state.dataSize->PlantSizData(PltSizCondNum).DesVolFlowRate;
                if (state.dataPlnt->PlantFirstSizesOkayToFinalize) this->DesignWaterFlowRate = tmpDesignWaterFlowRate;
            } else {
                tmpDesignWaterFlowRate = 0.0;
                if (state.dataPlnt->PlantFirstSizesOkayToFinalize) this->DesignWaterFlowRate = tmpDesignWaterFlowRate;
            }
            if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(
                        state, this->FluidCoolerType, this->Name, "Design Water Flow Rate [m3/s]", this->DesignWaterFlowRate);
                }
                if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(
                        state, this->FluidCoolerType, this->Name, "Initial Design Water Flow Rate [m3/s]", this->DesignWaterFlowRate);
                }
            }
        } else {
            if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                ShowSevereError(state, "Autosizing error for fluid cooler object = " + this->Name);
                ShowFatalError(state, "Autosizing of fluid cooler condenser flow rate requires a loop Sizing:Plant object.");
            }
        }
        // This conditional statement is to trap when the user specified Condenser/Fluid Cooler water design setpoint
        // temperature is less than design inlet air dry bulb temperature
        if (state.dataSize->PlantSizData(PltSizCondNum).ExitTemp <= this->DesignEnteringAirTemp && state.dataPlnt->PlantFirstSizesOkayToFinalize) {
            ShowSevereError(state, "Error when autosizing the UA value for fluid cooler = " + this->Name + '.');
            ShowContinueError(state,
                              format("Design Loop Exit Temperature ({:.2R} C) must be greater than design entering air dry-bulb temperature "
                                     "({:.2R} C) when autosizing the fluid cooler UA.",
                                     state.dataSize->PlantSizData(PltSizCondNum).ExitTemp,
                                     this->DesignEnteringAirTemp));
            ShowContinueError(state,
                              "It is recommended that the Design Loop Exit Temperature = design inlet air dry-bulb temp plus the Fluid Cooler "
                              "design approach temperature (e.g., 4 C).");
            ShowContinueError(state,
                              "If using HVACTemplate:Plant:ChilledWaterLoop, then check that input field Condenser Water Design Setpoint must be "
                              "> design inlet air dry-bulb temp if autosizing the Fluid Cooler.");
            ShowFatalError(state, "Review and revise design input values as appropriate.");
        }
    }

    PlantUtilities::RegisterPlantCompDesignFlow(state, this->WaterInletNodeNum, tmpDesignWaterFlowRate);

    if (this->PerformanceInputMethod_Num == PerfInputMethod::U_FACTOR && this->HighSpeedFluidCoolerUAWasAutoSized) {
        if (PltSizCondNum > 0) {
            rho = FluidProperties::GetDensityGlycol(state,
                                                    state.dataPlnt->PlantLoop(this->LoopNum).FluidName,
                                                    DataGlobalConstants::InitConvTemp,
                                                    state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex,
                                                    CalledFrom);
            Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                        state.dataPlnt->PlantLoop(this->LoopNum).FluidName,
                                                        state.dataSize->PlantSizData(PltSizCondNum).ExitTemp,
                                                        state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex,
                                                        CalledFrom);
            DesFluidCoolerLoad = rho * Cp * tmpDesignWaterFlowRate * state.dataSize->PlantSizData(PltSizCondNum).DeltaT;
            if (state.dataPlnt->PlantFirstSizesOkayToFinalize) this->FluidCoolerNominalCapacity = DesFluidCoolerLoad;
        } else {
            if (state.dataPlnt->PlantFirstSizesOkayToFinalize) this->FluidCoolerNominalCapacity = 0.0;
        }
    }

    if (this->HighSpeedFanPowerWasAutoSized) {
        // We assume the nominal fan power is 0.0105 times the design load
        if (this->PerformanceInputMethod_Num == PerfInputMethod::NOMINAL_CAPACITY) {
            tmpHighSpeedFanPower = 0.0105 * this->FluidCoolerNominalCapacity;
            if (state.dataPlnt->PlantFirstSizesOkayToFinalize) this->HighSpeedFanPower = tmpHighSpeedFanPower;
        } else {
            if (DesFluidCoolerLoad > 0.0) {
                tmpHighSpeedFanPower = 0.0105 * DesFluidCoolerLoad;
                if (state.dataPlnt->PlantFirstSizesOkayToFinalize) this->HighSpeedFanPower = tmpHighSpeedFanPower;
            } else if (PltSizCondNum > 0) {
                if (state.dataSize->PlantSizData(PltSizCondNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                    // This conditional statement is to trap when the user specified Condenser/Fluid Cooler water design setpoint
                    // temperature is less than design inlet air dry bulb temperature
                    if (state.dataSize->PlantSizData(PltSizCondNum).ExitTemp <= this->DesignEnteringAirTemp &&
                        state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                        ShowSevereError(state, "Error when autosizing the UA value for fluid cooler = " + this->Name + '.');
                        ShowContinueError(state,
                                          format("Design Loop Exit Temperature ({:.2R} C) must be greater than design entering air dry-bulb "
                                                 "temperature ({:.2R} C) when autosizing the fluid cooler UA.",
                                                 state.dataSize->PlantSizData(PltSizCondNum).ExitTemp,
                                                 this->DesignEnteringAirTemp));
                        ShowContinueError(state,
                                          "It is recommended that the Design Loop Exit Temperature = design inlet air dry-bulb temp plus the "
                                          "Fluid Cooler design approach temperature (e.g., 4 C).");
                        ShowContinueError(state,
                                          "If using HVACTemplate:Plant:ChilledWaterLoop, then check that input field Condenser Water Design "
                                          "Setpoint must be > design inlet air dry-bulb temp if autosizing the Fluid Cooler.");
                        ShowFatalError(state, "Review and revise design input values as appropriate.");
                    }
                    rho = FluidProperties::GetDensityGlycol(state,
                                                            state.dataPlnt->PlantLoop(this->LoopNum).FluidName,
                                                            DataGlobalConstants::InitConvTemp,
                                                            state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex,
                                                            CalledFrom);
                    Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                                state.dataPlnt->PlantLoop(this->LoopNum).FluidName,
                                                                state.dataSize->PlantSizData(PltSizCondNum).ExitTemp,
                                                                state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex,
                                                                CalledFrom);
                    DesFluidCoolerLoad = rho * Cp * tmpDesignWaterFlowRate * state.dataSize->PlantSizData(PltSizCondNum).DeltaT;
                    tmpHighSpeedFanPower = 0.0105 * DesFluidCoolerLoad;
                    if (state.dataPlnt->PlantFirstSizesOkayToFinalize) this->HighSpeedFanPower = tmpHighSpeedFanPower;
                } else {
                    tmpHighSpeedFanPower = 0.0;
                    if (state.dataPlnt->PlantFirstSizesOkayToFinalize) this->HighSpeedFanPower = tmpHighSpeedFanPower;
                }
            } else {
                if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                    ShowSevereError(state, "Autosizing of fluid cooler fan power requires a loop Sizing:Plant object.");
                    ShowFatalError(state, " Occurs in fluid cooler object = " + this->Name);
                }
            }
        }
        if (this->FluidCoolerType_Num == DataPlant::TypeOf_FluidCooler_SingleSpd) {
            if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(
                        state, this->FluidCoolerType, this->Name, "Fan Power at Design Air Flow Rate [W]", this->HighSpeedFanPower);
                }
                if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(
                        state, this->FluidCoolerType, this->Name, "Initial Fan Power at Design Air Flow Rate [W]", this->HighSpeedFanPower);
                }
            }
        } else if (this->FluidCoolerType_Num == DataPlant::TypeOf_FluidCooler_TwoSpd) {
            if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(
                        state, this->FluidCoolerType, this->Name, "Fan Power at High Fan Speed [W]", this->HighSpeedFanPower);
                }
                if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(
                        state, this->FluidCoolerType, this->Name, "Initial Fan Power at High Fan Speed [W]", this->HighSpeedFanPower);
                }
            }
        }
    }

    if (this->HighSpeedAirFlowRateWasAutoSized) {
        if (this->PerformanceInputMethod_Num == PerfInputMethod::NOMINAL_CAPACITY) {
            tmpHighSpeedAirFlowRate = this->FluidCoolerNominalCapacity / (this->DesignEnteringWaterTemp - this->DesignEnteringAirTemp) * 4.0;
            if (state.dataPlnt->PlantFirstSizesOkayToFinalize) this->HighSpeedAirFlowRate = tmpHighSpeedAirFlowRate;
        } else {
            if (DesFluidCoolerLoad > 0.0) {
                tmpHighSpeedAirFlowRate = DesFluidCoolerLoad / (this->DesignEnteringWaterTemp - this->DesignEnteringAirTemp) * 4.0;
                if (state.dataPlnt->PlantFirstSizesOkayToFinalize) this->HighSpeedAirFlowRate = tmpHighSpeedAirFlowRate;
            } else if (PltSizCondNum > 0) {
                if (state.dataSize->PlantSizData(PltSizCondNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                    // This conditional statement is to trap when the user specified Condenser/Fluid Cooler water design setpoint
                    // temperature is less than design inlet air dry bulb temperature
                    if (state.dataSize->PlantSizData(PltSizCondNum).ExitTemp <= this->DesignEnteringAirTemp &&
                        state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                        ShowSevereError(state, "Error when autosizing the UA value for fluid cooler = " + this->Name + '.');
                        ShowContinueError(state,
                                          format("Design Loop Exit Temperature ({:.2R} C) must be greater than design entering air dry-bulb "
                                                 "temperature ({:.2R} C) when autosizing the fluid cooler UA.",
                                                 state.dataSize->PlantSizData(PltSizCondNum).ExitTemp,
                                                 this->DesignEnteringAirTemp));
                        ShowContinueError(state,
                                          "It is recommended that the Design Loop Exit Temperature = design inlet air dry-bulb temp plus the "
                                          "Fluid Cooler design approach temperature (e.g., 4 C).");
                        ShowContinueError(state,
                                          "If using HVACTemplate:Plant:ChilledWaterLoop, then check that input field Condenser Water Design "
                                          "Setpoint must be > design inlet air dry-bulb temp if autosizing the Fluid Cooler.");
                        ShowFatalError(state, "Review and revise design input values as appropriate.");
                    }
                    rho = FluidProperties::GetDensityGlycol(state,
                                                            state.dataPlnt->PlantLoop(this->LoopNum).FluidName,
                                                            DataGlobalConstants::InitConvTemp,
                                                            state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex,
                                                            CalledFrom);
                    Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                                state.dataPlnt->PlantLoop(this->LoopNum).FluidName,
                                                                state.dataSize->PlantSizData(PltSizCondNum).ExitTemp,
                                                                state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex,
                                                                CalledFrom);
                    DesFluidCoolerLoad = rho * Cp * tmpDesignWaterFlowRate * state.dataSize->PlantSizData(PltSizCondNum).DeltaT;
                    tmpHighSpeedAirFlowRate = DesFluidCoolerLoad / (this->DesignEnteringWaterTemp - this->DesignEnteringAirTemp) * 4.0;
                    if (state.dataPlnt->PlantFirstSizesOkayToFinalize) this->HighSpeedAirFlowRate = tmpHighSpeedAirFlowRate;
                } else {
                    tmpHighSpeedAirFlowRate = 0.0;
                    if (state.dataPlnt->PlantFirstSizesOkayToFinalize) this->HighSpeedAirFlowRate = tmpHighSpeedAirFlowRate;
                }
            } else {
                if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                    ShowSevereError(state, "Autosizing of fluid cooler air flow rate requires a loop Sizing:Plant object");
                    ShowFatalError(state, " Occurs in fluid cooler object = " + this->Name);
                }
            }
        }
        if (this->FluidCoolerType_Num == DataPlant::TypeOf_FluidCooler_SingleSpd) {
            if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(state, this->FluidCoolerType, this->Name, "Design Air Flow Rate [m3/s]", this->HighSpeedAirFlowRate);
                }
                if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(
                        state, this->FluidCoolerType, this->Name, "Initial Design Air Flow Rate [m3/s]", this->HighSpeedAirFlowRate);
                }
            }
        } else if (this->FluidCoolerType == "FluidCooler:TwoSpeed") {
            if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(
                        state, this->FluidCoolerType, this->Name, "Air Flow Rate at High Fan Speed [m3/s]", this->HighSpeedAirFlowRate);
                }
                if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(
                        state, this->FluidCoolerType, this->Name, "Initial Air Flow Rate at High Fan Speed [m3/s]", this->HighSpeedAirFlowRate);
                }
            }
        }
    }

    if (this->HighSpeedFluidCoolerUAWasAutoSized && state.dataPlnt->PlantFirstSizesOkayToFinalize) {
        if (PltSizCondNum > 0) {
            if (state.dataSize->PlantSizData(PltSizCondNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                // This conditional statement is to trap when the user specified Condenser/Fluid Cooler water design setpoint
                // temperature is less than design inlet air dry bulb temperature
                if (state.dataSize->PlantSizData(PltSizCondNum).ExitTemp <= this->DesignEnteringAirTemp &&
                    state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                    ShowSevereError(state, "Error when autosizing the UA value for fluid cooler = " + this->Name + '.');
                    ShowContinueError(state,
                                      format("Design Loop Exit Temperature ({:.2R} C) must be greater than design entering air dry-bulb "
                                             "temperature ({:.2R} C) when autosizing the fluid cooler UA.",
                                             state.dataSize->PlantSizData(PltSizCondNum).ExitTemp,
                                             this->DesignEnteringAirTemp));
                    ShowContinueError(state,
                                      "It is recommended that the Design Loop Exit Temperature = design inlet air dry-bulb temp plus the Fluid "
                                      "Cooler design approach temperature (e.g., 4 C).");
                    ShowContinueError(state,
                                      "If using HVACTemplate:Plant:ChilledWaterLoop, then check that input field Condenser Water Design Setpoint "
                                      "must be > design inlet air dry-bulb temp if autosizing the Fluid Cooler.");
                    ShowFatalError(state, "Review and revise design input values as appropriate.");
                }
                rho = FluidProperties::GetDensityGlycol(state,
                                                        state.dataPlnt->PlantLoop(this->LoopNum).FluidName,
                                                        DataGlobalConstants::InitConvTemp,
                                                        state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex,
                                                        CalledFrom);
                Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                            state.dataPlnt->PlantLoop(this->LoopNum).FluidName,
                                                            state.dataSize->PlantSizData(PltSizCondNum).ExitTemp,
                                                            state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex,
                                                            CalledFrom);
                DesFluidCoolerLoad = rho * Cp * tmpDesignWaterFlowRate * state.dataSize->PlantSizData(PltSizCondNum).DeltaT;
                Par[0] = DesFluidCoolerLoad;
                Par[1] = double(this->indexInArray);
                Par[2] = rho * tmpDesignWaterFlowRate; // design water mass flow rate
                Par[3] = tmpHighSpeedAirFlowRate;      // design air volume flow rate
                Par[4] = Cp;
                UA0 = 0.0001 * DesFluidCoolerLoad; // Assume deltaT = 10000K (limit)
                UA1 = DesFluidCoolerLoad;          // Assume deltaT = 1K
                this->WaterTemp = state.dataSize->PlantSizData(PltSizCondNum).ExitTemp + state.dataSize->PlantSizData(PltSizCondNum).DeltaT;
                this->AirTemp = this->DesignEnteringAirTemp;
                this->AirWetBulb = this->DesignEnteringAirWetBulbTemp;
                this->AirPress = state.dataEnvrn->StdBaroPress;
                this->AirHumRat = Psychrometrics::PsyWFnTdbTwbPb(state, this->AirTemp, this->AirWetBulb, this->AirPress, CalledFrom);
                General::SolveRoot(state, Acc, MaxIte, SolFla, UA, SimpleFluidCoolerUAResidual, UA0, UA1, Par);
                if (SolFla == -1) {
                    ShowWarningError(state, "Iteration limit exceeded in calculating fluid cooler UA.");
                    ShowContinueError(state, "Autosizing of fluid cooler UA failed for fluid cooler = " + this->Name);
                    ShowContinueError(state, format("The final UA value ={:.2R} W/K, and the simulation continues...", UA));
                } else if (SolFla == -2) {
                    CalcFluidCoolerOutlet(state, int(Par[1]), Par[2], Par[3], UA0, OutWaterTempAtUA0);
                    CalcFluidCoolerOutlet(state, int(Par[1]), Par[2], Par[3], UA1, OutWaterTempAtUA1);
                    ShowSevereError(state, std::string{CalledFrom} + ": The combination of design input values did not allow the calculation of a ");
                    ShowContinueError(state, "reasonable UA value. Review and revise design input values as appropriate. Specifying hard");
                    ShowContinueError(state, R"(sizes for some "autosizable" fields while autosizing other "autosizable" fields may be )");
                    ShowContinueError(state, "contributing to this problem.");
                    ShowContinueError(state, "This model iterates on UA to find the heat transfer required to provide the design outlet ");
                    ShowContinueError(state, "water temperature. Initially, the outlet water temperatures at high and low UA values are ");
                    ShowContinueError(state, "calculated. The Design Exit Water Temperature should be between the outlet water ");
                    ShowContinueError(state, "temperatures calculated at high and low UA values. If the Design Exit Water Temperature is ");
                    ShowContinueError(state, "out of this range, the solution will not converge and UA will not be calculated. ");
                    ShowContinueError(state, "The possible solutions could be to manually input adjusted water and/or air flow rates based ");
                    ShowContinueError(state, "on the autosized values shown below or to adjust design fluid cooler air inlet dry-bulb temperature.");
                    ShowContinueError(state, "Plant:Sizing object inputs also influence these results (e.g. DeltaT and ExitTemp).");
                    ShowContinueError(state, "Inputs to the fluid cooler object:");
                    ShowContinueError(state, format("Design Fluid Cooler Load [W]                       = {:.2R}", Par[0]));
                    ShowContinueError(state, format("Design Fluid Cooler Water Volume Flow Rate [m3/s]  = {:.6R}", this->DesignWaterFlowRate));
                    ShowContinueError(state, format("Design Fluid Cooler Air Volume Flow Rate [m3/s]    = {:.2R}", Par[3]));
                    ShowContinueError(state, format("Design Fluid Cooler Air Inlet Dry-bulb Temp [C]    = {:.2R}", this->AirTemp));
                    ShowContinueError(state, "Inputs to the plant sizing object:");
                    ShowContinueError(
                        state,
                        format("Design Exit Water Temp [C]                         = {:.2R}", state.dataSize->PlantSizData(PltSizCondNum).ExitTemp));
                    ShowContinueError(
                        state,
                        format("Loop Design Temperature Difference [C]             = {:.2R}", state.dataSize->PlantSizData(PltSizCondNum).DeltaT));
                    ShowContinueError(state, format("Design Fluid Cooler Water Inlet Temp [C]           = {:.2R}", this->WaterTemp));
                    ShowContinueError(state, format("Calculated water outlet temp at low UA [C] (UA = {:.2R} W/K) = {:.2R}", UA0, OutWaterTempAtUA0));
                    ShowContinueError(state, format("Calculated water outlet temp at high UA [C](UA = {:.2R} W/K) = {:.2R}", UA1, OutWaterTempAtUA1));
                    ShowFatalError(state, "Autosizing of Fluid Cooler UA failed for fluid cooler = " + this->Name);
                }
                tmpHighSpeedEvapFluidCoolerUA = UA;
                if (state.dataPlnt->PlantFirstSizesOkayToFinalize) this->HighSpeedFluidCoolerUA = tmpHighSpeedEvapFluidCoolerUA;
                this->FluidCoolerNominalCapacity = DesFluidCoolerLoad;
            } else {
                tmpHighSpeedEvapFluidCoolerUA = 0.0;
                if (state.dataPlnt->PlantFirstSizesOkayToFinalize) this->HighSpeedFluidCoolerUA = tmpHighSpeedEvapFluidCoolerUA;
            }
            if (this->FluidCoolerType_Num == DataPlant::TypeOf_FluidCooler_SingleSpd) {
                if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                    if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(state,
                                                     this->FluidCoolerType,
                                                     this->Name,
                                                     "U-factor Times Area Value at Design Air Flow Rate [W/K]",
                                                     this->HighSpeedFluidCoolerUA);
                    }
                    if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(state,
                                                     this->FluidCoolerType,
                                                     this->Name,
                                                     "Initial U-factor Times Area Value at Design Air Flow Rate [W/K]",
                                                     this->HighSpeedFluidCoolerUA);
                    }
                }
            } else if (this->FluidCoolerType_Num == DataPlant::TypeOf_FluidCooler_TwoSpd) {
                if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                    if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(state,
                                                     this->FluidCoolerType,
                                                     this->Name,
                                                     "U-factor Times Area Value at High Fan Speed [W/K]",
                                                     this->HighSpeedFluidCoolerUA);
                    }
                    if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(state,
                                                     this->FluidCoolerType,
                                                     this->Name,
                                                     "Initial U-factor Times Area Value at High Fan Speed [W/K]",
                                                     this->HighSpeedFluidCoolerUA);
                    }
                }
            }
        } else {
            if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                ShowSevereError(state, "Autosizing error for fluid cooler object = " + this->Name);
                ShowFatalError(state, "Autosizing of fluid cooler UA requires a loop Sizing:Plant object.");
            }
        }
    }

    if (this->PerformanceInputMethod_Num == PerfInputMethod::NOMINAL_CAPACITY) {
        if (this->DesignWaterFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
            rho = FluidProperties::GetDensityGlycol(state,
                                                    state.dataPlnt->PlantLoop(this->LoopNum).FluidName,
                                                    DataGlobalConstants::InitConvTemp,
                                                    state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex,
                                                    CalledFrom);
            Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                        state.dataPlnt->PlantLoop(this->LoopNum).FluidName,
                                                        this->DesignEnteringWaterTemp,
                                                        state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex,
                                                        CalledFrom);
            DesFluidCoolerLoad = this->FluidCoolerNominalCapacity;
            Par[0] = DesFluidCoolerLoad;
            Par[1] = double(this->indexInArray);
            Par[2] = rho * tmpDesignWaterFlowRate; // design water mass flow rate
            Par[3] = tmpHighSpeedAirFlowRate;      // design air volume flow rate
            Par[4] = Cp;
            UA0 = 0.0001 * DesFluidCoolerLoad;                     // Assume deltaT = 10000K (limit)
            UA1 = DesFluidCoolerLoad;                              // Assume deltaT = 1K
            this->WaterTemp = this->DesignEnteringWaterTemp;       // design inlet water temperature
            this->AirTemp = this->DesignEnteringAirTemp;           // design inlet air dry-bulb temp
            this->AirWetBulb = this->DesignEnteringAirWetBulbTemp; // design inlet air wet-bulb temp
            this->AirPress = state.dataEnvrn->StdBaroPress;
            this->AirHumRat = Psychrometrics::PsyWFnTdbTwbPb(state, this->AirTemp, this->AirWetBulb, this->AirPress);
            General::SolveRoot(state, Acc, MaxIte, SolFla, UA, SimpleFluidCoolerUAResidual, UA0, UA1, Par);
            if (SolFla == -1) {
                ShowWarningError(state, "Iteration limit exceeded in calculating fluid cooler UA.");
                if (PltSizCondNum > 0) {
                    ShowContinueError(state, "Autosizing of fluid cooler UA failed for fluid cooler = " + this->Name);
                }
                ShowContinueError(state, format("The final UA value ={:.2R} W/K, and the simulation continues...", UA));
            } else if (SolFla == -2) {
                CalcFluidCoolerOutlet(state, int(Par[1]), Par[2], Par[3], UA0, OutWaterTempAtUA0);
                CalcFluidCoolerOutlet(state, int(Par[1]), Par[2], Par[3], UA1, OutWaterTempAtUA1);
                ShowSevereError(state, std::string{CalledFrom} + ": The combination of design input values did not allow the calculation of a ");
                ShowContinueError(state, "reasonable UA value. Review and revise design input values as appropriate. Specifying hard");
                ShowContinueError(state, R"(sizes for some "autosizable" fields while autosizing other "autosizable" fields may be )");
                ShowContinueError(state, "contributing to this problem.");
                ShowContinueError(state, "This model iterates on UA to find the heat transfer required to provide the design outlet ");
                ShowContinueError(state, "water temperature. Initially, the outlet water temperatures at high and low UA values are ");
                ShowContinueError(state, "calculated. The Design Exit Water Temperature should be between the outlet water ");
                ShowContinueError(state, "temperatures calculated at high and low UA values. If the Design Exit Water Temperature is ");
                ShowContinueError(state, "out of this range, the solution will not converge and UA will not be calculated. ");
                ShowContinueError(state, "The possible solutions could be to manually input adjusted water and/or air flow rates based ");
                ShowContinueError(state, "on the autosized values shown below or to adjust design fluid cooler air inlet dry-bulb temperature.");
                ShowContinueError(state, "Plant:Sizing object inputs also influence these results (e.g. DeltaT and ExitTemp).");
                ShowContinueError(state, "Inputs to the fluid cooler object:");
                ShowContinueError(state, format("Design Fluid Cooler Load [W]                       = {:.2R}", Par[0]));
                ShowContinueError(state, format("Design Fluid Cooler Water Volume Flow Rate [m3/s]  = {:.6R}", this->DesignWaterFlowRate));
                ShowContinueError(state, format("Design Fluid Cooler Air Volume Flow Rate [m3/s]    = {:.2R}", Par[3]));
                ShowContinueError(state, format("Design Fluid Cooler Air Inlet Dry-bulb Temp [C]    = {:.2R}", this->AirTemp));
                if (PltSizCondNum > 0) {
                    ShowContinueError(state, "Inputs to the plant sizing object:");
                    ShowContinueError(
                        state,
                        format("Design Exit Water Temp [C]                         = {:.2R}", state.dataSize->PlantSizData(PltSizCondNum).ExitTemp));
                    ShowContinueError(
                        state,
                        format("Loop Design Temperature Difference [C]             = {:.2R}", state.dataSize->PlantSizData(PltSizCondNum).DeltaT));
                };
                ShowContinueError(state, format("Design Fluid Cooler Water Inlet Temp [C]           = {:.2R}", this->WaterTemp));
                ShowContinueError(state, format("Calculated water outlet temp at low UA [C] (UA = {:.2R} W/K) = {:.2R}", UA0, OutWaterTempAtUA0));
                ShowContinueError(state, format("Calculated water outlet temp at high UA [C] (UA = {:.2R} W/K) = {:.2R}", UA1, OutWaterTempAtUA1));
                if (PltSizCondNum > 0) {
                    ShowFatalError(state, "Autosizing of Fluid Cooler UA failed for fluid cooler = " + this->Name);
                }
            }
            if (state.dataPlnt->PlantFirstSizesOkayToFinalize) this->HighSpeedFluidCoolerUA = UA;
        } else {
            if (state.dataPlnt->PlantFirstSizesOkayToFinalize) this->HighSpeedFluidCoolerUA = 0.0;
        }
        if (this->FluidCoolerType_Num == DataPlant::TypeOf_FluidCooler_SingleSpd) {
            if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(state,
                                                 this->FluidCoolerType,
                                                 this->Name,
                                                 "Fluid cooler UA value at design air flow rate based on nominal capacity input [W/K]",
                                                 this->HighSpeedFluidCoolerUA);
                }
                if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(state,
                                                 this->FluidCoolerType,
                                                 this->Name,
                                                 "Initial Fluid cooler UA value at design air flow rate based on nominal capacity input [W/K]",
                                                 this->HighSpeedFluidCoolerUA);
                }
            }
        } else if (this->FluidCoolerType_Num == DataPlant::TypeOf_FluidCooler_TwoSpd) {
            if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(state,
                                                 this->FluidCoolerType,
                                                 this->Name,
                                                 "Fluid cooler UA value at high fan speed based on nominal capacity input [W/K]",
                                                 this->HighSpeedFluidCoolerUA);
                }
                if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(state,
                                                 this->FluidCoolerType,
                                                 this->Name,
                                                 "Initial Fluid cooler UA value at high fan speed based on nominal capacity input [W/K]",
                                                 this->HighSpeedFluidCoolerUA);
                }
            }
        }
    }

    if (this->LowSpeedAirFlowRateWasAutoSized && state.dataPlnt->PlantFirstSizesOkayToFinalize) {
        this->LowSpeedAirFlowRate = this->LowSpeedAirFlowRateSizingFactor * this->HighSpeedAirFlowRate;
        if (state.dataPlnt->PlantFinalSizesOkayToReport) {
            BaseSizer::reportSizerOutput(
                state, this->FluidCoolerType, this->Name, "Air Flow Rate at Low Fan Speed [m3/s]", this->LowSpeedAirFlowRate);
        }
        if (state.dataPlnt->PlantFirstSizesOkayToReport) {
            BaseSizer::reportSizerOutput(
                state, this->FluidCoolerType, this->Name, "Initial Air Flow Rate at Low Fan Speed [m3/s]", this->LowSpeedAirFlowRate);
        }
    }

    if (this->LowSpeedFanPowerWasAutoSized && state.dataPlnt->PlantFirstSizesOkayToFinalize) {
        this->LowSpeedFanPower = this->LowSpeedFanPowerSizingFactor * this->HighSpeedFanPower;
        if (state.dataPlnt->PlantFinalSizesOkayToReport) {
            BaseSizer::reportSizerOutput(state, this->FluidCoolerType, this->Name, "Fan Power at Low Fan Speed [W]", this->LowSpeedFanPower);
        }
        if (state.dataPlnt->PlantFirstSizesOkayToReport) {
            BaseSizer::reportSizerOutput(state, this->FluidCoolerType, this->Name, "Initial Fan Power at Low Fan Speed [W]", this->LowSpeedFanPower);
        }
    }

    if (this->LowSpeedFluidCoolerUAWasAutoSized && state.dataPlnt->PlantFirstSizesOkayToFinalize) {
        this->LowSpeedFluidCoolerUA = this->LowSpeedFluidCoolerUASizingFactor * this->HighSpeedFluidCoolerUA;
        if (state.dataPlnt->PlantFinalSizesOkayToReport) {
            BaseSizer::reportSizerOutput(
                state, this->FluidCoolerType, this->Name, "U-factor Times Area Value at Low Fan Speed [W/K]", this->LowSpeedFluidCoolerUA);
        }
        if (state.dataPlnt->PlantFirstSizesOkayToReport) {
            BaseSizer::reportSizerOutput(
                state, this->FluidCoolerType, this->Name, "Initial U-factor Times Area Value at Low Fan Speed [W/K]", this->LowSpeedFluidCoolerUA);
        }
    }

    if (this->PerformanceInputMethod_Num == PerfInputMethod::NOMINAL_CAPACITY && this->FluidCoolerType_Num == DataPlant::TypeOf_FluidCooler_TwoSpd) {
        if (this->FluidCoolerLowSpeedNomCapWasAutoSized && state.dataPlnt->PlantFirstSizesOkayToFinalize) {
            this->FluidCoolerLowSpeedNomCap = this->FluidCoolerLowSpeedNomCapSizingFactor * this->FluidCoolerNominalCapacity;
            if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                BaseSizer::reportSizerOutput(
                    state, this->FluidCoolerType, this->Name, "Low Fan Speed Nominal Capacity [W]", this->FluidCoolerLowSpeedNomCap);
            }
            if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                BaseSizer::reportSizerOutput(
                    state, this->FluidCoolerType, this->Name, "Initial Low Fan Speed Nominal Capacity [W]", this->FluidCoolerLowSpeedNomCap);
            }
        }

        if (this->DesignWaterFlowRate >= DataHVACGlobals::SmallWaterVolFlow && this->FluidCoolerLowSpeedNomCap > 0.0) {
            rho = FluidProperties::GetDensityGlycol(state,
                                                    state.dataPlnt->PlantLoop(this->LoopNum).FluidName,
                                                    DataGlobalConstants::InitConvTemp,
                                                    state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex,
                                                    CalledFrom);
            Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                        state.dataPlnt->PlantLoop(this->LoopNum).FluidName,
                                                        this->DesignEnteringWaterTemp,
                                                        state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex,
                                                        CalledFrom);
            DesFluidCoolerLoad = this->FluidCoolerLowSpeedNomCap;
            Par[0] = DesFluidCoolerLoad;
            Par[1] = double(this->indexInArray);
            Par[2] = rho * tmpDesignWaterFlowRate; // design water mass flow rate
            Par[3] = this->LowSpeedAirFlowRate;    // Air volume flow rate at low fan speed
            Par[4] = Cp;
            UA0 = 0.0001 * DesFluidCoolerLoad;                     // Assume deltaT = 10000K (limit)
            UA1 = DesFluidCoolerLoad;                              // Assume deltaT = 1K
            this->WaterTemp = this->DesignEnteringWaterTemp;       // design inlet water temperature
            this->AirTemp = this->DesignEnteringAirTemp;           // design inlet air dry-bulb temp
            this->AirWetBulb = this->DesignEnteringAirWetBulbTemp; // design inlet air wet-bulb temp
            this->AirPress = state.dataEnvrn->StdBaroPress;
            this->AirHumRat = Psychrometrics::PsyWFnTdbTwbPb(state, this->AirTemp, this->AirWetBulb, this->AirPress, CalledFrom);
            General::SolveRoot(state, Acc, MaxIte, SolFla, UA, SimpleFluidCoolerUAResidual, UA0, UA1, Par);
            if (SolFla == -1) {
                ShowWarningError(state, "Iteration limit exceeded in calculating fluid cooler UA.");
                ShowContinueError(state, "Autosizing of fluid cooler UA failed for fluid cooler = " + this->Name);
                ShowContinueError(state, format("The final UA value at low fan speed ={:.2R} W/C, and the simulation continues...", UA));
            } else if (SolFla == -2) {
                CalcFluidCoolerOutlet(state, int(Par[1]), Par[2], Par[3], UA0, OutWaterTempAtUA0);
                CalcFluidCoolerOutlet(state, int(Par[1]), Par[2], Par[3], UA1, OutWaterTempAtUA1);
                ShowSevereError(state, std::string{CalledFrom} + ": The combination of design input values did not allow the calculation of a ");
                ShowContinueError(state, "reasonable low-speed UA value. Review and revise design input values as appropriate. ");
                ShowContinueError(state, R"(Specifying hard sizes for some "autosizable" fields while autosizing other "autosizable" )");
                ShowContinueError(state, "fields may be contributing to this problem.");
                ShowContinueError(state, "This model iterates on UA to find the heat transfer required to provide the design outlet ");
                ShowContinueError(state, "water temperature. Initially, the outlet water temperatures at high and low UA values are ");
                ShowContinueError(state, "calculated. The Design Exit Water Temperature should be between the outlet water ");
                ShowContinueError(state, "temperatures calculated at high and low UA values. If the Design Exit Water Temperature is ");
                ShowContinueError(state, "out of this range, the solution will not converge and UA will not be calculated. ");
                ShowContinueError(state, "The possible solutions could be to manually input adjusted water and/or air flow rates based ");
                ShowContinueError(state, "on the autosized values shown below or to adjust design fluid cooler air inlet dry-bulb temperature.");
                ShowContinueError(state, "Plant:Sizing object inputs also influence these results (e.g. DeltaT and ExitTemp).");
                ShowContinueError(state, "Inputs to the fluid cooler object:");
                ShowContinueError(state, format("Design Fluid Cooler Load [W]                         = {:.2R}", Par[0]));
                ShowContinueError(state, format("Design Fluid Cooler Water Volume Flow Rate [m3/s]    = {:.6R}", this->DesignWaterFlowRate));
                ShowContinueError(state, format("Design Fluid Cooler Air Volume Flow Rate [m3/s]      = {:.2R}", Par[3]));
                ShowContinueError(state, format("Design Fluid Cooler Air Inlet Dry-bulb Temp [C]      = {:.2R}", this->AirTemp));
                ShowContinueError(state, "Inputs to the plant sizing object:");
                ShowContinueError(
                    state,
                    format("Design Exit Water Temp [C]                           = {:.2R}", state.dataSize->PlantSizData(PltSizCondNum).ExitTemp));
                ShowContinueError(
                    state,
                    format("Loop Design Temperature Difference [C]               = {:.2R}", state.dataSize->PlantSizData(PltSizCondNum).DeltaT));
                ShowContinueError(state, format("Design Fluid Cooler Water Inlet Temp [C]             = {:.2R}", this->WaterTemp));
                ShowContinueError(state, format("Calculated water outlet temp at low UA [C](UA = {:.2R} W/C) = {:.2R}", UA0, OutWaterTempAtUA0));
                ShowContinueError(state, format("Calculated water outlet temp at high UA [C](UA = {:.2R} W/C) = {:.2R}", UA1, OutWaterTempAtUA1));
                ShowFatalError(state, "Autosizing of Fluid Cooler UA failed for fluid cooler = " + this->Name);
            }
            if (state.dataPlnt->PlantFirstSizesOkayToFinalize) this->LowSpeedFluidCoolerUA = UA;
        } else {
            if (state.dataPlnt->PlantFirstSizesOkayToFinalize) this->LowSpeedFluidCoolerUA = 0.0;
        }
        if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
            if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                BaseSizer::reportSizerOutput(
                    state, this->FluidCoolerType, this->Name, "U-factor Times Area Value at Low Fan Speed [W/C]", this->LowSpeedFluidCoolerUA);
            }
            if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                BaseSizer::reportSizerOutput(state,
                                             this->FluidCoolerType,
                                             this->Name,
                                             "Initial U-factor Times Area Value at Low Fan Speed [W/C]",
                                             this->LowSpeedFluidCoolerUA);
            }
        }
    }

    ErrorsFound = false;

    if (state.dataPlnt->PlantFinalSizesOkayToReport) {
        // create predefined report
        equipName = this->Name;
        OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchMechType, equipName, this->FluidCoolerType);
        OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchMechNomCap, equipName, this->FluidCoolerNominalCapacity);
    }

    if (this->FluidCoolerType_Num == DataPlant::TypeOf_FluidCooler_TwoSpd && state.dataPlnt->PlantFirstSizesOkayToFinalize) {
        if (this->DesignWaterFlowRate > 0.0) {
            if (this->HighSpeedAirFlowRate <= this->LowSpeedAirFlowRate) {
                ShowSevereError(state,
                                "FluidCooler:TwoSpeed  \"" + this->Name + "\". Low speed air flow rate must be less than high speed air flow rate.");
                ErrorsFound = true;
            }
            if (this->HighSpeedFluidCoolerUA <= this->LowSpeedFluidCoolerUA) {
                ShowSevereError(state,
                                "FluidCooler:TwoSpeed  \"" + this->Name +
                                    "\". Fluid cooler UA at low fan speed must be less than the fluid cooler UA at high fan speed.");
                ErrorsFound = true;
            }
        }
    }

    if (ErrorsFound) {
        ShowFatalError(state, "SizeFluidCooler: Program terminated due to previous condition(s).");
    }
}

void FluidCoolerspecs::calcSingleSpeed(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Chandan Sharma
    //       DATE WRITTEN   August 2008
    //       MODIFIED       Dec. 2008. BG. added RunFlag logic per original methodology
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // To simulate the operation of a single-speed fan fluid cooler.

    // METHODOLOGY EMPLOYED:
    // The fluid cooler is modeled using effectiveness-NTU relationships for
    // cross flow heat exchangers (both stream unmixed)based on cooling tower model.
    // The subroutine calculates the period of time required to meet a
    // leaving water temperature setpoint. It assumes that part-load
    // operation represents a linear interpolation of two steady-state regimes.
    // Cyclic losses are neglected. The period of time required to meet the
    // leaving water temperature setpoint is used to determine the required
    // fan power and energy.
    // A RunFlag is passed by the upper level manager to indicate the ON/OFF status,
    // or schedule, of the fluid cooler. If the fluid cooler is OFF, outlet water
    // temperature and flow rate are passed through the model from inlet node to
    // outlet node without intervention. Reports are also updated with fan power
    // and energy being zero.
    // When the RunFlag indicates an ON condition for thefluid cooler, the
    // mass flow rate and water temperature are read from the inlet node of the
    // fluid cooler (water-side). The outdoor air dry-bulb temperature is used
    // as the entering condition to thefluid cooler (air-side).Thefluid cooler
    // fan is turned on and design parameters are used to calculate the leaving
    // water temperature.If the calculated leaving water temperature is below the setpoint,
    // a fan run-time fraction is calculated and used to determine fan power. The leaving
    // water temperature setpoint is placed on the outlet node. If the calculated
    // leaving water temperature is at or above the setpoint, the calculated
    // leaving water temperature is placed on the outlet node and the fan runs at
    // full power. Water mass flow rate is passed from inlet node to outlet node
    // with no intervention.

    // REFERENCES:
    // ASHRAE HVAC1KIT: A Toolkit for Primary HVAC System Energy Calculation. 1999.
    // Based on SingleSpeedTower subroutine by Dan Fisher ,Sept 1998.

    // SUBROUTINE PARAMETER DEFINITIONS:
    static constexpr std::string_view RoutineName("SingleSpeedFluidCooler");

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 TempSetPoint = 0.0;

    // set inlet and outlet nodes
    auto &waterInletNode = this->WaterInletNodeNum;
    this->Qactual = 0.0;
    this->FanPower = 0.0;
    this->OutletWaterTemp = state.dataLoopNodes->Node(waterInletNode).Temp;
    {
        auto const SELECT_CASE_var(state.dataPlnt->PlantLoop(this->LoopNum).LoopDemandCalcScheme);
        if (SELECT_CASE_var == DataPlant::iLoopDemandCalcScheme::SingleSetPoint) {
            TempSetPoint = state.dataPlnt->PlantLoop(this->LoopNum).LoopSide(this->LoopSideNum).TempSetPoint;
        } else if (SELECT_CASE_var == DataPlant::iLoopDemandCalcScheme::DualSetPointDeadBand) {
            TempSetPoint = state.dataPlnt->PlantLoop(this->LoopNum).LoopSide(this->LoopSideNum).TempSetPointHi;
        }
    }

    //   MassFlowTol is a parameter to indicate a no flow condition
    if (this->WaterMassFlowRate <= DataBranchAirLoopPlant::MassFlowTolerance) return;

    if (this->OutletWaterTemp < TempSetPoint) { // already there don't need to run the cooler
        return;
    }

    //   Initialize local variables
    Real64 OutletWaterTempOFF = state.dataLoopNodes->Node(waterInletNode).Temp;
    this->OutletWaterTemp = OutletWaterTempOFF;

    Real64 UAdesign = this->HighSpeedFluidCoolerUA;
    Real64 AirFlowRate = this->HighSpeedAirFlowRate;
    Real64 FanPowerOn = this->HighSpeedFanPower;

    CalcFluidCoolerOutlet(state, this->indexInArray, this->WaterMassFlowRate, AirFlowRate, UAdesign, this->OutletWaterTemp);

    if (this->OutletWaterTemp <= TempSetPoint) {
        //   Setpoint was met with pump ON and fan ON, calculate run-time fraction or just wasn't needed at all
        Real64 FanModeFrac = 0.0;
        if (this->OutletWaterTemp != OutletWaterTempOFF) { // don't divide by zero
            FanModeFrac = (TempSetPoint - OutletWaterTempOFF) / (this->OutletWaterTemp - OutletWaterTempOFF);
        }
        this->FanPower = max(FanModeFrac * FanPowerOn, 0.0); // BG change
        this->OutletWaterTemp = TempSetPoint;
    } else {
        //    Setpoint was not met, fluid cooler ran at full capacity
        this->FanPower = FanPowerOn;
    }
    Real64 CpWater = FluidProperties::GetSpecificHeatGlycol(state,
                                                            state.dataPlnt->PlantLoop(this->LoopNum).FluidName,
                                                            state.dataLoopNodes->Node(waterInletNode).Temp,
                                                            state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex,
                                                            RoutineName);
    this->Qactual = this->WaterMassFlowRate * CpWater * (state.dataLoopNodes->Node(waterInletNode).Temp - this->OutletWaterTemp);
}

void FluidCoolerspecs::calcTwoSpeed(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Chandan Sharma
    //       DATE WRITTEN   August 2008
    //       MODIFIED       Dec. 2008. BG. added RunFlag logic per original methodology
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // To simulate the operation of a fluid cooler with a two-speed fan.

    // METHODOLOGY EMPLOYED:
    // The fluid cooler is modeled using effectiveness-NTU relationships for
    // cross flow heat exchangers (both stream unmixed)based on cooling tower model.
    // The subroutine calculates the period of time required to meet a
    // leaving water temperature setpoint. It assumes that part-load
    // operation represents a linear interpolation of two steady-state regimes
    // (high-speed fan operation and low-speed fan operation).
    // Cyclic losses are neglected. The period of time required to meet the
    // leaving water temperature setpoint is used to determine the required
    // fan power and energy.
    // A RunFlag is passed by the upper level manager to indicate the ON/OFF status,
    // or schedule, of the fluid cooler. If the fluid cooler is OFF, outlet water
    // temperature and flow rate are passed through the model from inlet node to
    // outlet node without intervention.Reports are also updated with fan power
    // and fan energy being zero.
    // When the RunFlag indicates an ON condition for the fluid cooler, the
    // mass flow rate and water temperature are read from the inlet node of the
    // fluid cooler (water-side). The outdoor air dry-bulb temperature is used
    // as the entering condition to the fluid cooler (air-side). Input deck
    // parameters are read for the low fan speed and a leaving water temperature
    // is calculated.
    // If the calculated leaving water temperature is below the setpoint,
    // a fan run-time fraction (FanModeFrac) is calculated and used to determine fan power.
    // The leaving water temperature setpoint is placed on the outlet node.
    // If the calculated leaving water temperature is at or above
    // the setpoint, the fluid cooler fan is turned on 'high speed' and the routine is
    // repeated. If the calculated leaving water temperature is below the setpoint,
    // a fan run-time fraction is calculated for the second stage fan and fan power
    // is calculated as FanModeFrac*HighSpeedFanPower+(1-FanModeFrac)*LowSpeedFanPower.
    // If the calculated leaving water temperature is above the leaving water temp.
    // setpoint, the calculated leaving water temperature is placed on the outlet
    // node and the fan runs at full power (High Speed Fan Power). Water mass flow
    // rate is passed from inlet node to outlet node with no intervention.

    // REFERENCES:
    // ASHRAE HVAC1KIT: A Toolkit for Primary HVAC System Energy Calculation. 1999.
    // Based on TwoSpeedTower by Dan Fisher ,Sept. 1998.

    // SUBROUTINE PARAMETER DEFINITIONS:
    static constexpr std::string_view RoutineName("TwoSpeedFluidCooler");

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 TempSetPoint = 0.0;

    auto &waterInletNode = this->WaterInletNodeNum;
    this->Qactual = 0.0;
    this->FanPower = 0.0;
    this->OutletWaterTemp = state.dataLoopNodes->Node(waterInletNode).Temp;
    {
        auto const SELECT_CASE_var(state.dataPlnt->PlantLoop(this->LoopNum).LoopDemandCalcScheme);
        if (SELECT_CASE_var == DataPlant::iLoopDemandCalcScheme::SingleSetPoint) {
            TempSetPoint = state.dataPlnt->PlantLoop(this->LoopNum).LoopSide(this->LoopSideNum).TempSetPoint;
        } else if (SELECT_CASE_var == DataPlant::iLoopDemandCalcScheme::DualSetPointDeadBand) {
            TempSetPoint = state.dataPlnt->PlantLoop(this->LoopNum).LoopSide(this->LoopSideNum).TempSetPointHi;
        }
    }

    // MassFlowTol is a parameter to indicate a no flow condition
    if (this->WaterMassFlowRate <= DataBranchAirLoopPlant::MassFlowTolerance ||
        state.dataPlnt->PlantLoop(this->LoopNum).LoopSide(this->LoopSideNum).FlowLock == DataPlant::iFlowLock::Unlocked)
        return;

    // set local variable for fluid cooler
    this->WaterMassFlowRate = state.dataLoopNodes->Node(waterInletNode).MassFlowRate;
    Real64 OutletWaterTempOFF = state.dataLoopNodes->Node(waterInletNode).Temp;
    Real64 OutletWaterTemp1stStage = OutletWaterTempOFF;
    Real64 OutletWaterTemp2ndStage = OutletWaterTempOFF;
    Real64 FanModeFrac = 0.0;

    if (OutletWaterTempOFF < TempSetPoint) { // already there don't need to run the cooler
        return;
    }

    Real64 UAdesign = this->LowSpeedFluidCoolerUA;
    Real64 AirFlowRate = this->LowSpeedAirFlowRate;
    Real64 FanPowerLow = this->LowSpeedFanPower;

    CalcFluidCoolerOutlet(state, this->indexInArray, this->WaterMassFlowRate, AirFlowRate, UAdesign, OutletWaterTemp1stStage);

    if (OutletWaterTemp1stStage <= TempSetPoint) {
        // Setpoint was met with pump ON and fan ON 1st stage, calculate fan mode fraction
        if (OutletWaterTemp1stStage != OutletWaterTempOFF) { // don't divide by zero
            FanModeFrac = (TempSetPoint - OutletWaterTempOFF) / (OutletWaterTemp1stStage - OutletWaterTempOFF);
        }
        this->FanPower = FanModeFrac * FanPowerLow;
        this->OutletWaterTemp = TempSetPoint;
        this->Qactual *= FanModeFrac;
    } else {
        // Setpoint was not met, turn on fluid cooler 2nd stage fan
        UAdesign = this->HighSpeedFluidCoolerUA;
        AirFlowRate = this->HighSpeedAirFlowRate;
        Real64 FanPowerHigh = this->HighSpeedFanPower;

        CalcFluidCoolerOutlet(state, this->indexInArray, this->WaterMassFlowRate, AirFlowRate, UAdesign, OutletWaterTemp2ndStage);

        if ((OutletWaterTemp2ndStage <= TempSetPoint) && UAdesign > 0.0) {
            // Setpoint was met with pump ON and fan ON 2nd stage, calculate fan mode fraction
            FanModeFrac = (TempSetPoint - OutletWaterTemp1stStage) / (OutletWaterTemp2ndStage - OutletWaterTemp1stStage);
            this->FanPower = max((FanModeFrac * FanPowerHigh) + (1.0 - FanModeFrac) * FanPowerLow, 0.0);
            this->OutletWaterTemp = TempSetPoint;
        } else {
            // Setpoint was not met, fluid cooler ran at full capacity
            this->OutletWaterTemp = OutletWaterTemp2ndStage;
            this->FanPower = FanPowerHigh;
        }
    }
    Real64 CpWater = FluidProperties::GetSpecificHeatGlycol(state,
                                                            state.dataPlnt->PlantLoop(this->LoopNum).FluidName,
                                                            state.dataLoopNodes->Node(waterInletNode).Temp,
                                                            state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex,
                                                            RoutineName);
    this->Qactual = this->WaterMassFlowRate * CpWater * (state.dataLoopNodes->Node(waterInletNode).Temp - this->OutletWaterTemp);
}

void CalcFluidCoolerOutlet(
    EnergyPlusData &state, int FluidCoolerNum, Real64 _WaterMassFlowRate, Real64 AirFlowRate, Real64 UAdesign, Real64 &_OutletWaterTemp)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Chandan Sharma
    //       DATE WRITTEN   August 2008
    //       MODIFIED       April 2010, Chandan Sharma, FSEC
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // See purpose for Single Speed or Two Speed Fluid Cooler model

    // METHODOLOGY EMPLOYED:
    // See methodology for Single Speed or Two Speed Fluid Cooler model

    // Locals
    Real64 _Qactual; // Actual heat transfer rate between fluid cooler water and air [W]

    // SUBROUTINE PARAMETER DEFINITIONS:
    static constexpr std::string_view RoutineName("CalcFluidCoolerOutlet");

    if (UAdesign == 0.0) return;

    // set local fluid cooler inlet and outlet temperature variables
    Real64 _InletWaterTemp = state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).WaterTemp;
    _OutletWaterTemp = _InletWaterTemp;
    Real64 InletAirTemp = state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).AirTemp;

    // set water and air properties
    Real64 AirDensity = Psychrometrics::PsyRhoAirFnPbTdbW(state,
                                                          state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).AirPress,
                                                          InletAirTemp,
                                                          state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).AirHumRat);
    Real64 AirMassFlowRate = AirFlowRate * AirDensity;
    Real64 CpAir = Psychrometrics::PsyCpAirFnW(state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).AirHumRat);
    Real64 CpWater = FluidProperties::GetSpecificHeatGlycol(
        state,
        state.dataPlnt->PlantLoop(state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).LoopNum).FluidName,
        _InletWaterTemp,
        state.dataPlnt->PlantLoop(state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerNum).LoopNum).FluidIndex,
        RoutineName);

    // Calculate mass flow rates
    Real64 MdotCpWater = _WaterMassFlowRate * CpWater;
    Real64 AirCapacity = AirMassFlowRate * CpAir;

    // calculate the minimum to maximum capacity ratios of airside and waterside
    Real64 CapacityRatioMin = min(AirCapacity, MdotCpWater);
    Real64 CapacityRatioMax = max(AirCapacity, MdotCpWater);
    Real64 CapacityRatio = CapacityRatioMin / CapacityRatioMax;

    // Calculate number of transfer units (NTU)
    Real64 NumTransferUnits = UAdesign / CapacityRatioMin;
    Real64 ETA = std::pow(NumTransferUnits, 0.22);
    Real64 A = CapacityRatio * NumTransferUnits / ETA;
    Real64 effectiveness = 1.0 - std::exp((std::exp(-A) - 1.0) / (CapacityRatio / ETA));

    // calculate water to air heat transfer
    _Qactual = effectiveness * CapacityRatioMin * (_InletWaterTemp - InletAirTemp);

    if (_Qactual >= 0.0) {
        _OutletWaterTemp = _InletWaterTemp - _Qactual / MdotCpWater;
    } else {
        _OutletWaterTemp = _InletWaterTemp;
    }
}

Real64 SimpleFluidCoolerUAResidual(EnergyPlusData &state,
                                   Real64 const UA,                 // UA of fluid cooler
                                   std::array<Real64, 5> const &Par // par(1) = design fluid cooler load [W]
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Chandan Sharma
    //       DATE WRITTEN   August 2008
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // Calculates residual function (Design fluid cooler load - fluid cooler Output) / Design fluid cooler load.
    // Fluid cooler output depends on the UA which is being varied to zero the residual.

    // METHODOLOGY EMPLOYED:
    // Puts UA into the fluid cooler data structure, calls CalcFluidCoolerOutlet, and calculates
    // the residual as defined above.

    // REFERENCES:
    // Based on SimpleTowerUAResidual by Fred Buhl, May 2002

    // par(2) = Fluid cooler number
    // par(3) = design water mass flow rate [kg/s]
    // par(4) = design air volume flow rate [m3/s]
    // par(5) = water specific heat [J/(kg*C)]

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    Real64 OutWaterTemp = 0.0; // outlet water temperature [C]

    int FluidCoolerIndex = int(Par[1]);
    CalcFluidCoolerOutlet(state, FluidCoolerIndex, Par[2], Par[3], UA, OutWaterTemp);
    Real64 const Output = Par[4] * Par[2] * (state.dataFluidCoolers->SimpleFluidCooler(FluidCoolerIndex).WaterTemp - OutWaterTemp);
    return (Par[0] - Output) / Par[0];
}

void FluidCoolerspecs::update(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR:          Chandan Sharma
    //       DATE WRITTEN:    August 2008
    //       MODIFIED         na
    //       RE-ENGINEERED    na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine is for passing results to the outlet water node.

    // SUBROUTINE PARAMETER DEFINITIONS:

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 LoopMinTemp;

    auto &waterOutletNode = this->WaterOutletNodeNum;
    state.dataLoopNodes->Node(waterOutletNode).Temp = this->OutletWaterTemp;

    if (state.dataPlnt->PlantLoop(this->LoopNum).LoopSide(this->LoopSideNum).FlowLock == DataPlant::iFlowLock::Unlocked ||
        state.dataGlobal->WarmupFlag)
        return;

    // Check flow rate through fluid cooler and compare to design flow rate, show warning if greater than Design * Mulitplier
    if (state.dataLoopNodes->Node(waterOutletNode).MassFlowRate > this->DesWaterMassFlowRate * this->FluidCoolerMassFlowRateMultiplier) {
        ++this->HighMassFlowErrorCount;
        if (this->HighMassFlowErrorCount < 2) {
            ShowWarningError(state, this->FluidCoolerType + " \"" + this->Name + "\"");
            ShowContinueError(state, " Condenser Loop Mass Flow Rate is much greater than the fluid coolers design mass flow rate.");
            ShowContinueError(state, format(" Condenser Loop Mass Flow Rate = {:.6T}", state.dataLoopNodes->Node(waterOutletNode).MassFlowRate));
            ShowContinueError(state, format(" Fluid Cooler Design Mass Flow Rate   = {:.6T}", this->DesWaterMassFlowRate));
            ShowContinueErrorTimeStamp(state, "");
        } else {
            ShowRecurringWarningErrorAtEnd(
                state,
                this->FluidCoolerType + " \"" + this->Name +
                    "\"  Condenser Loop Mass Flow Rate is much greater than the fluid coolers design mass flow rate error continues...",
                this->HighMassFlowErrorIndex,
                state.dataLoopNodes->Node(waterOutletNode).MassFlowRate,
                state.dataLoopNodes->Node(waterOutletNode).MassFlowRate);
        }
    }

    // Check if OutletWaterTemp is below the minimum condenser loop temp and warn user
    LoopMinTemp = state.dataPlnt->PlantLoop(this->LoopNum).MinTemp;
    if (this->OutletWaterTemp < LoopMinTemp && this->WaterMassFlowRate > 0.0) {
        ++this->OutletWaterTempErrorCount;

        if (this->OutletWaterTempErrorCount < 2) {
            ShowWarningError(state, format("{} \"{}\"", this->FluidCoolerType, this->Name));
            ShowContinueError(state,
                              format(" Fluid cooler water outlet temperature ({.2F} C) is below the specified minimum condenser loop temp of {.2F} C",
                                     this->OutletWaterTemp,
                                     LoopMinTemp));
            ShowContinueErrorTimeStamp(state, "");
        } else {
            ShowRecurringWarningErrorAtEnd(
                state,
                this->FluidCoolerType + " \"" + this->Name +
                    "\" Fluid cooler water outlet temperature is below the specified minimum condenser loop temp error continues...",
                this->OutletWaterTempErrorIndex,
                this->OutletWaterTemp,
                this->OutletWaterTemp);
        }
    }

    // Check if water mass flow rate is small (e.g. no flow) and warn user
    if (this->WaterMassFlowRate > 0.0 && this->WaterMassFlowRate <= DataBranchAirLoopPlant::MassFlowTolerance) {
        ++this->SmallWaterMassFlowErrorCount;
        if (this->SmallWaterMassFlowErrorCount < 2) {
            ShowWarningError(state, this->FluidCoolerType + " \"" + this->Name + "\"");
            ShowContinueError(state, " Fluid cooler water mass flow rate near zero.");
            ShowContinueErrorTimeStamp(state, "");
            ShowContinueError(state, format("Actual Mass flow = {:.2T}", this->WaterMassFlowRate));
        } else {
            ShowRecurringWarningErrorAtEnd(state,
                                           this->FluidCoolerType + " \"" + this->Name +
                                               "\" Fluid cooler water mass flow rate near zero error continues...",
                                           this->SmallWaterMassFlowErrorIndex,
                                           this->WaterMassFlowRate,
                                           this->WaterMassFlowRate);
        }
    }
}

void FluidCoolerspecs::report(EnergyPlusData &state, bool const RunFlag)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR:          Chandan Sharma
    //       DATE WRITTEN:    August 2008
    //       MODIFIED         na
    //       RE-ENGINEERED    na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine updates the report variables for the fluid cooler.

    Real64 ReportingConstant = state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
    auto &waterInletNode = this->WaterInletNodeNum;
    if (!RunFlag) {
        this->InletWaterTemp = state.dataLoopNodes->Node(waterInletNode).Temp;
        this->OutletWaterTemp = state.dataLoopNodes->Node(waterInletNode).Temp;
        this->Qactual = 0.0;
        this->FanPower = 0.0;
        this->FanEnergy = 0.0;
    } else {
        this->InletWaterTemp = state.dataLoopNodes->Node(waterInletNode).Temp;
        this->FanEnergy = this->FanPower * ReportingConstant;
    }
}

} // namespace EnergyPlus::FluidCoolers
