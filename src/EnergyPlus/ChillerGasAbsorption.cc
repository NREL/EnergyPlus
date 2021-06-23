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
#include <EnergyPlus/ChillerGasAbsorption.hh>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataBranchAirLoopPlant.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/EMSManager.hh>
#include <EnergyPlus/FluidProperties.hh>
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

namespace EnergyPlus::ChillerGasAbsorption {

// MODULE INFORMATION:
//    AUTHOR         Jason Glazer of GARD Analytics, Inc.
//                   for Gas Research Institute
//    DATE WRITTEN   March 2001
//    MODIFIED       Brent Griffith, Nov 2010 plant upgrades, generalize fluid properties
//    RE-ENGINEERED  na
// PURPOSE OF THIS MODULE:
//    This module simulates the performance of the direct fired
//    absorption chiller.
// METHODOLOGY EMPLOYED:
//    Once the PlantLoopManager determines that the absorber chiller
//    is available to meet a loop cooling demand, it calls SimGasAbsorption
//    which in turn calls the appropriate Absorption Chiller model.
// REFERENCES:
//    DOE-2.1e Supplement
//    PG&E CoolTools GasMod
// OTHER NOTES:
//    The curves on this model follow the DOE-2 approach of using
//    electric and heat input ratios.  In addition, the temperature
//    correction curve has two independent variables for the
//    chilled water temperature and either the entering or leaving
//    condenser water temperature.
//    The code was originally adopted from the ChillerAbsorption
//    routine but has been extensively modified.
//    Development of this module was funded by the Gas Research Institute.
//    (Please see copyright and disclaimer information at end of module)

PlantComponent *GasAbsorberSpecs::factory(EnergyPlusData &state, std::string const &objectName)
{
    // Process the input data if it hasn't been done already
    if (state.dataChillerGasAbsorption->getGasAbsorberInputs) {
        GetGasAbsorberInput(state);
        state.dataChillerGasAbsorption->getGasAbsorberInputs = false;
    }
    // Now look for this particular pipe in the list
    for (auto &comp : state.dataChillerGasAbsorption->GasAbsorber) {
        if (comp.Name == objectName) {
            return &comp;
        }
    }
    // If we didn't find it, fatal
    ShowFatalError(state, "LocalGasAbsorberFactory: Error getting inputs for comp named: " + objectName); // LCOV_EXCL_LINE
    // Shut up the compiler
    return nullptr; // LCOV_EXCL_LINE
}

void GasAbsorberSpecs::simulate(
    EnergyPlusData &state, const PlantLocation &calledFromLocation, bool FirstHVACIteration, Real64 &CurLoad, bool RunFlag)
{
    DataPlant::BrLoopType brIdentity(DataPlant::BrLoopType::NoMatch);

    int branchTotalComp = state.dataPlnt->PlantLoop(calledFromLocation.loopNum)
                              .LoopSide(calledFromLocation.loopSideNum)
                              .Branch(calledFromLocation.branchNum)
                              .TotalComponents;

    for (int iComp = 1; iComp <= branchTotalComp; iComp++) {
        // kind of a hacky way to find the location of this, but it's what plantloopequip was doing
        int compInletNodeNum = state.dataPlnt->PlantLoop(calledFromLocation.loopNum)
                                   .LoopSide(calledFromLocation.loopSideNum)
                                   .Branch(calledFromLocation.branchNum)
                                   .Comp(iComp)
                                   .NodeNumIn;
        // Match inlet node name of calling branch to determine if this call is for heating or cooling
        if (compInletNodeNum == this->ChillReturnNodeNum) { // Operate as chiller
            brIdentity = DataPlant::BrLoopType::Chiller;
            break;
        } else if (compInletNodeNum == this->HeatReturnNodeNum) { // Operate as heater
            brIdentity = DataPlant::BrLoopType::Heater;
            break;
        } else if (compInletNodeNum == this->CondReturnNodeNum) { // called from condenser loop
            brIdentity = DataPlant::BrLoopType::Condenser;
            break;
        } else {
            brIdentity = DataPlant::BrLoopType::NoMatch;
        }
    }

    if (brIdentity == DataPlant::BrLoopType::Chiller) {
        // Calculate Node Values
        // Calculate Equipment and Update Variables
        this->InCoolingMode = RunFlag != 0;
        this->initialize(state);
        this->calculateChiller(state, CurLoad);
        this->updateCoolRecords(state, CurLoad, RunFlag);
    } else if (brIdentity == DataPlant::BrLoopType::Heater) {
        // Calculate Node Values
        // Calculate Equipment and Update Variables
        this->InHeatingMode = RunFlag != 0;
        this->initialize(state);
        this->calculateHeater(state, CurLoad, RunFlag);
        this->updateHeatRecords(state, CurLoad, RunFlag);
    } else if (brIdentity == DataPlant::BrLoopType::Condenser) {
        if (this->CDLoopNum > 0) {
            PlantUtilities::UpdateChillerComponentCondenserSide(state,
                                                                this->CDLoopNum,
                                                                this->CDLoopSideNum,
                                                                DataPlant::TypeOf_Chiller_DFAbsorption,
                                                                this->CondReturnNodeNum,
                                                                this->CondSupplyNodeNum,
                                                                this->TowerLoad,
                                                                this->CondReturnTemp,
                                                                this->CondSupplyTemp,
                                                                this->CondWaterFlowRate,
                                                                FirstHVACIteration);
        }
    } else {
        // Error, nodes do not match
        ShowSevereError(state, "Invalid call to Gas Absorber Chiller " + this->Name);
        ShowContinueError(state, "Node connections in branch are not consistent with object nodes.");
        ShowFatalError(state, "Preceding conditions cause termination.");
    }
}

void GasAbsorberSpecs::getDesignCapacities(
    EnergyPlusData &state, const PlantLocation &calledFromLocation, Real64 &MaxLoad, Real64 &MinLoad, Real64 &OptLoad)
{
    bool matchfound(false);

    int branchTotalComp = state.dataPlnt->PlantLoop(calledFromLocation.loopNum)
                              .LoopSide(calledFromLocation.loopSideNum)
                              .Branch(calledFromLocation.branchNum)
                              .TotalComponents;

    for (int iComp = 1; iComp <= branchTotalComp; iComp++) {
        // kind of a hacky way to find the location of this, but it's what plantloopequip was doing
        int compInletNodeNum = state.dataPlnt->PlantLoop(calledFromLocation.loopNum)
                                   .LoopSide(calledFromLocation.loopSideNum)
                                   .Branch(calledFromLocation.branchNum)
                                   .Comp(iComp)
                                   .NodeNumIn;

        if (compInletNodeNum == this->ChillReturnNodeNum) { // Operate as chiller
            MinLoad = this->NomCoolingCap * this->MinPartLoadRat;
            MaxLoad = this->NomCoolingCap * this->MaxPartLoadRat;
            OptLoad = this->NomCoolingCap * this->OptPartLoadRat;
            matchfound = true;
            break;
        } else if (compInletNodeNum == this->HeatReturnNodeNum) { // Operate as heater
            Real64 Sim_HeatCap = this->NomCoolingCap * this->NomHeatCoolRatio;
            MinLoad = Sim_HeatCap * this->MinPartLoadRat;
            MaxLoad = Sim_HeatCap * this->MaxPartLoadRat;
            OptLoad = Sim_HeatCap * this->OptPartLoadRat;
            matchfound = true;
            break;
        } else if (compInletNodeNum == this->CondReturnNodeNum) { // called from condenser loop
            MinLoad = 0.0;
            MaxLoad = 0.0;
            OptLoad = 0.0;
            matchfound = true;
            break;
        } else {
            matchfound = false;
        }
    }

    if (!matchfound) {
        // Error, nodes do not match
        ShowSevereError(state, "SimGasAbsorber: Invalid call to Gas Absorbtion Chiller-Heater " + this->Name);
        ShowContinueError(state, "Node connections in branch are not consistent with object nodes.");
        ShowFatalError(state, "Preceding conditions cause termination.");
    } // Operate as Chiller or Heater
}

void GasAbsorberSpecs::getSizingFactor(Real64 &_SizFac)
{
    _SizFac = this->SizFac;
}

void GasAbsorberSpecs::onInitLoopEquip(EnergyPlusData &state, const PlantLocation &calledFromLocation)
{
    this->initialize(state);

    // kind of a hacky way to find the location of this, but it's what plantloopequip was doing
    int BranchInletNodeNum =
        state.dataPlnt->PlantLoop(calledFromLocation.loopNum).LoopSide(calledFromLocation.loopSideNum).Branch(calledFromLocation.branchNum).NodeNumIn;

    if (BranchInletNodeNum == this->ChillReturnNodeNum) {       // Operate as chiller
        this->size(state);                                      // only call from chilled water loop
    } else if (BranchInletNodeNum == this->HeatReturnNodeNum) { // Operate as heater
        // don't do anything here
    } else if (BranchInletNodeNum == this->CondReturnNodeNum) { // called from condenser loop
                                                                // don't do anything here
    } else {                                                    // Error, nodes do not match
        ShowSevereError(state, "SimGasAbsorber: Invalid call to Gas Absorbtion Chiller-Heater " + this->Name);
        ShowContinueError(state, "Node connections in branch are not consistent with object nodes.");
        ShowFatalError(state, "Preceding conditions cause termination.");
    } // Operate as Chiller or Heater
}

void GasAbsorberSpecs::getDesignTemperatures(Real64 &TempCondInDesign, Real64 &TempEvapOutDesign)
{
    TempEvapOutDesign = this->TempDesCHWSupply;
    TempCondInDesign = this->TempDesCondReturn;
}

void GetGasAbsorberInput(EnergyPlusData &state)
{
    //       AUTHOR:          Jason Glazer
    //       DATE WRITTEN:    March 2001
    // This routine will get the input
    // required by the Direct Fired Absorption chiller model in the object ChillerHeater:Absorption:DirectFired

    using BranchNodeConnections::TestCompSet;
    using CurveManager::GetCurveCheck;
    using DataSizing::AutoSize;
    using GlobalNames::VerifyUniqueChillerName;
    using NodeInputManager::GetOnlySingleNode;
    using OutAirNodeManager::CheckAndAddAirNodeNumber;

    int AbsorberNum; // Absorber counter
    int NumAlphas;   // Number of elements in the alpha array
    int NumNums;     // Number of elements in the numeric array
    int IOStat;      // IO Status when calling get input subroutine
    std::string ChillerName;
    bool Okay;
    bool Get_ErrorsFound(false);
    int NumGasAbsorbers(0); // number of Absorption Chillers specified in input
    auto &cCurrentModuleObject = state.dataIPShortCut->cCurrentModuleObject;

    state.dataIPShortCut->cCurrentModuleObject = "ChillerHeater:Absorption:DirectFired";
    NumGasAbsorbers = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

    if (NumGasAbsorbers <= 0) {
        ShowSevereError(state, "No " + cCurrentModuleObject + " equipment found in input file");
        Get_ErrorsFound = true;
    }

    if (allocated(state.dataChillerGasAbsorption->GasAbsorber)) return;

    // ALLOCATE ARRAYS
    state.dataChillerGasAbsorption->GasAbsorber.allocate(NumGasAbsorbers);

    // LOAD ARRAYS

    for (AbsorberNum = 1; AbsorberNum <= NumGasAbsorbers; ++AbsorberNum) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 AbsorberNum,
                                                                 state.dataIPShortCut->cAlphaArgs,
                                                                 NumAlphas,
                                                                 state.dataIPShortCut->rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 _,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);
        UtilityRoutines::IsNameEmpty(state, state.dataIPShortCut->cAlphaArgs(1), cCurrentModuleObject, Get_ErrorsFound);

        // Get_ErrorsFound will be set to True if problem was found, left untouched otherwise
        VerifyUniqueChillerName(state, cCurrentModuleObject, state.dataIPShortCut->cAlphaArgs(1), Get_ErrorsFound, cCurrentModuleObject + " Name");

        auto &thisChiller = state.dataChillerGasAbsorption->GasAbsorber(AbsorberNum);
        thisChiller.Name = state.dataIPShortCut->cAlphaArgs(1);
        ChillerName = cCurrentModuleObject + " Named " + thisChiller.Name;

        // Assign capacities
        thisChiller.NomCoolingCap = state.dataIPShortCut->rNumericArgs(1);
        if (thisChiller.NomCoolingCap == AutoSize) {
            thisChiller.NomCoolingCapWasAutoSized = true;
        }
        thisChiller.NomHeatCoolRatio = state.dataIPShortCut->rNumericArgs(2);
        // Assign efficiencies
        thisChiller.FuelCoolRatio = state.dataIPShortCut->rNumericArgs(3);
        thisChiller.FuelHeatRatio = state.dataIPShortCut->rNumericArgs(4);
        thisChiller.ElecCoolRatio = state.dataIPShortCut->rNumericArgs(5);
        thisChiller.ElecHeatRatio = state.dataIPShortCut->rNumericArgs(6);

        // Assign Node Numbers to specified nodes
        thisChiller.ChillReturnNodeNum = GetOnlySingleNode(state,
                                                           state.dataIPShortCut->cAlphaArgs(2),
                                                           Get_ErrorsFound,
                                                           cCurrentModuleObject,
                                                           state.dataIPShortCut->cAlphaArgs(1),
                                                           DataLoopNode::NodeFluidType::Water,
                                                           DataLoopNode::NodeConnectionType::Inlet,
                                                           NodeInputManager::compFluidStream::Primary,
                                                           DataLoopNode::ObjectIsNotParent);
        thisChiller.ChillSupplyNodeNum = GetOnlySingleNode(state,
                                                           state.dataIPShortCut->cAlphaArgs(3),
                                                           Get_ErrorsFound,
                                                           cCurrentModuleObject,
                                                           state.dataIPShortCut->cAlphaArgs(1),
                                                           DataLoopNode::NodeFluidType::Water,
                                                           DataLoopNode::NodeConnectionType::Outlet,
                                                           NodeInputManager::compFluidStream::Primary,
                                                           DataLoopNode::ObjectIsNotParent);
        TestCompSet(state,
                    cCurrentModuleObject,
                    state.dataIPShortCut->cAlphaArgs(1),
                    state.dataIPShortCut->cAlphaArgs(2),
                    state.dataIPShortCut->cAlphaArgs(3),
                    "Chilled Water Nodes");
        // Condenser node processing depends on condenser type, see below
        thisChiller.HeatReturnNodeNum = GetOnlySingleNode(state,
                                                          state.dataIPShortCut->cAlphaArgs(6),
                                                          Get_ErrorsFound,
                                                          cCurrentModuleObject,
                                                          state.dataIPShortCut->cAlphaArgs(1),
                                                          DataLoopNode::NodeFluidType::Water,
                                                          DataLoopNode::NodeConnectionType::Inlet,
                                                          NodeInputManager::compFluidStream::Tertiary,
                                                          DataLoopNode::ObjectIsNotParent);
        thisChiller.HeatSupplyNodeNum = GetOnlySingleNode(state,
                                                          state.dataIPShortCut->cAlphaArgs(7),
                                                          Get_ErrorsFound,
                                                          cCurrentModuleObject,
                                                          state.dataIPShortCut->cAlphaArgs(1),
                                                          DataLoopNode::NodeFluidType::Water,
                                                          DataLoopNode::NodeConnectionType::Outlet,
                                                          NodeInputManager::compFluidStream::Tertiary,
                                                          DataLoopNode::ObjectIsNotParent);
        TestCompSet(state,
                    cCurrentModuleObject,
                    state.dataIPShortCut->cAlphaArgs(1),
                    state.dataIPShortCut->cAlphaArgs(6),
                    state.dataIPShortCut->cAlphaArgs(7),
                    "Hot Water Nodes");
        if (Get_ErrorsFound) {
            ShowFatalError(state, "Errors found in processing node input for " + cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
            Get_ErrorsFound = false;
        }

        // Assign Part Load Ratios
        thisChiller.MinPartLoadRat = state.dataIPShortCut->rNumericArgs(7);
        thisChiller.MaxPartLoadRat = state.dataIPShortCut->rNumericArgs(8);
        thisChiller.OptPartLoadRat = state.dataIPShortCut->rNumericArgs(9);
        // Assign Design Conditions
        thisChiller.TempDesCondReturn = state.dataIPShortCut->rNumericArgs(10);
        thisChiller.TempDesCHWSupply = state.dataIPShortCut->rNumericArgs(11);
        thisChiller.EvapVolFlowRate = state.dataIPShortCut->rNumericArgs(12);
        if (thisChiller.EvapVolFlowRate == AutoSize) {
            thisChiller.EvapVolFlowRateWasAutoSized = true;
        }
        if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(16), "AirCooled")) {
            thisChiller.CondVolFlowRate = 0.0011; // Condenser flow rate not used for this cond type
        } else {
            thisChiller.CondVolFlowRate = state.dataIPShortCut->rNumericArgs(13);
            if (thisChiller.CondVolFlowRate == AutoSize) {
                thisChiller.CondVolFlowRateWasAutoSized = true;
            }
        }
        thisChiller.HeatVolFlowRate = state.dataIPShortCut->rNumericArgs(14);
        if (thisChiller.HeatVolFlowRate == AutoSize) {
            thisChiller.HeatVolFlowRateWasAutoSized = true;
        }
        // Assign Curve Numbers
        thisChiller.CoolCapFTCurve = GetCurveCheck(state, state.dataIPShortCut->cAlphaArgs(8), Get_ErrorsFound, ChillerName);
        thisChiller.FuelCoolFTCurve = GetCurveCheck(state, state.dataIPShortCut->cAlphaArgs(9), Get_ErrorsFound, ChillerName);
        thisChiller.FuelCoolFPLRCurve = GetCurveCheck(state, state.dataIPShortCut->cAlphaArgs(10), Get_ErrorsFound, ChillerName);
        thisChiller.ElecCoolFTCurve = GetCurveCheck(state, state.dataIPShortCut->cAlphaArgs(11), Get_ErrorsFound, ChillerName);
        thisChiller.ElecCoolFPLRCurve = GetCurveCheck(state, state.dataIPShortCut->cAlphaArgs(12), Get_ErrorsFound, ChillerName);
        thisChiller.HeatCapFCoolCurve = GetCurveCheck(state, state.dataIPShortCut->cAlphaArgs(13), Get_ErrorsFound, ChillerName);
        thisChiller.FuelHeatFHPLRCurve = GetCurveCheck(state, state.dataIPShortCut->cAlphaArgs(14), Get_ErrorsFound, ChillerName);
        if (Get_ErrorsFound) {
            ShowFatalError(state, "Errors found in processing curve input for " + cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
            Get_ErrorsFound = false;
        }
        if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(15), "LeavingCondenser")) {
            thisChiller.isEnterCondensTemp = false;
        } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(15), "EnteringCondenser")) {
            thisChiller.isEnterCondensTemp = true;
        } else {
            thisChiller.isEnterCondensTemp = true;
            ShowWarningError(state, cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\", invalid value");
            ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(15) + "=\"" + state.dataIPShortCut->cAlphaArgs(15) + "\"");
            ShowContinueError(state, "resetting to EnteringCondenser, simulation continues");
        }
        // Assign Other Parameters
        if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(16), "AirCooled")) {
            thisChiller.isWaterCooled = false;
        } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(16), "WaterCooled")) {
            thisChiller.isWaterCooled = true;
        } else {
            thisChiller.isWaterCooled = true;
            ShowWarningError(state, cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\", invalid value");
            ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(16) + '=' + state.dataIPShortCut->cAlphaArgs(16));
            ShowContinueError(state, "resetting to WaterCooled, simulation continues");
        }
        if (!thisChiller.isEnterCondensTemp && !thisChiller.isWaterCooled) {
            thisChiller.isEnterCondensTemp = true;
            ShowWarningError(state, cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\", invalid value");
            ShowContinueError(state, "Invalid to have both LeavingCondenser and AirCooled.");
            ShowContinueError(state, "resetting to EnteringCondenser, simulation continues");
        }
        if (thisChiller.isWaterCooled) {
            if (state.dataIPShortCut->lAlphaFieldBlanks(5)) {
                ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\", invalid value");
                ShowContinueError(state, "For WaterCooled chiller the condenser outlet node is required.");
                Get_ErrorsFound = true;
            }
            thisChiller.CondReturnNodeNum = GetOnlySingleNode(state,
                                                              state.dataIPShortCut->cAlphaArgs(4),
                                                              Get_ErrorsFound,
                                                              cCurrentModuleObject,
                                                              state.dataIPShortCut->cAlphaArgs(1),
                                                              DataLoopNode::NodeFluidType::Water,
                                                              DataLoopNode::NodeConnectionType::Inlet,
                                                              NodeInputManager::compFluidStream::Secondary,
                                                              DataLoopNode::ObjectIsNotParent);
            thisChiller.CondSupplyNodeNum = GetOnlySingleNode(state,
                                                              state.dataIPShortCut->cAlphaArgs(5),
                                                              Get_ErrorsFound,
                                                              cCurrentModuleObject,
                                                              state.dataIPShortCut->cAlphaArgs(1),
                                                              DataLoopNode::NodeFluidType::Water,
                                                              DataLoopNode::NodeConnectionType::Outlet,
                                                              NodeInputManager::compFluidStream::Secondary,
                                                              DataLoopNode::ObjectIsNotParent);
            TestCompSet(state,
                        cCurrentModuleObject,
                        state.dataIPShortCut->cAlphaArgs(1),
                        state.dataIPShortCut->cAlphaArgs(4),
                        state.dataIPShortCut->cAlphaArgs(5),
                        "Condenser Water Nodes");
        } else {
            thisChiller.CondReturnNodeNum = GetOnlySingleNode(state,
                                                              state.dataIPShortCut->cAlphaArgs(4),
                                                              Get_ErrorsFound,
                                                              cCurrentModuleObject,
                                                              state.dataIPShortCut->cAlphaArgs(1),
                                                              DataLoopNode::NodeFluidType::Air,
                                                              DataLoopNode::NodeConnectionType::OutsideAirReference,
                                                              NodeInputManager::compFluidStream::Secondary,
                                                              DataLoopNode::ObjectIsNotParent);
            // Condenser outlet node not used for air or evap cooled condenser so ingore cAlphaArgs( 5 )
            // Connection not required for air or evap cooled condenser so no call to TestCompSet here
            CheckAndAddAirNodeNumber(state, thisChiller.CondReturnNodeNum, Okay);
            if (!Okay) {
                ShowWarningError(state, cCurrentModuleObject + ", Adding OutdoorAir:Node=" + state.dataIPShortCut->cAlphaArgs(4));
            }
        }
        thisChiller.CHWLowLimitTemp = state.dataIPShortCut->rNumericArgs(15);
        thisChiller.FuelHeatingValue = state.dataIPShortCut->rNumericArgs(16);
        thisChiller.SizFac = state.dataIPShortCut->rNumericArgs(17);

        // Validate fuel type input
        bool FuelTypeError(false);
        UtilityRoutines::ValidateFuelType(state, state.dataIPShortCut->cAlphaArgs(17), thisChiller.FuelType, FuelTypeError);
        if (FuelTypeError) {
            ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\", invalid value");
            ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(17) + '=' + state.dataIPShortCut->cAlphaArgs(17));
            ShowContinueError(
                state, "Valid choices are Electricity, NaturalGas, Propane, Diesel, Gasoline, FuelOilNo1, FuelOilNo2,OtherFuel1 or OtherFuel2");
            Get_ErrorsFound = true;
            FuelTypeError = false;
        }
    }

    if (Get_ErrorsFound) {
        ShowFatalError(state, "Errors found in processing input for " + cCurrentModuleObject);
    }
}

void GasAbsorberSpecs::setupOutputVariables(EnergyPlusData &state)
{
    std::string &ChillerName = this->Name;

    SetupOutputVariable(
        state, "Chiller Heater Evaporator Cooling Rate", OutputProcessor::Unit::W, this->CoolingLoad, "System", "Average", ChillerName);
    SetupOutputVariable(state,
                        "Chiller Heater Evaporator Cooling Energy",
                        OutputProcessor::Unit::J,
                        this->CoolingEnergy,
                        "System",
                        "Sum",
                        ChillerName,
                        _,
                        "ENERGYTRANSFER",
                        "CHILLERS",
                        _,
                        "Plant");

    SetupOutputVariable(state, "Chiller Heater Heating Rate", OutputProcessor::Unit::W, this->HeatingLoad, "System", "Average", ChillerName);
    SetupOutputVariable(state,
                        "Chiller Heater Heating Energy",
                        OutputProcessor::Unit::J,
                        this->HeatingEnergy,
                        "System",
                        "Sum",
                        ChillerName,
                        _,
                        "ENERGYTRANSFER",
                        "BOILERS",
                        _,
                        "Plant");

    SetupOutputVariable(
        state, "Chiller Heater Condenser Heat Transfer Rate", OutputProcessor::Unit::W, this->TowerLoad, "System", "Average", ChillerName);
    SetupOutputVariable(state,
                        "Chiller Heater Condenser Heat Transfer Energy",
                        OutputProcessor::Unit::J,
                        this->TowerEnergy,
                        "System",
                        "Sum",
                        ChillerName,
                        _,
                        "ENERGYTRANSFER",
                        "HEATREJECTION",
                        _,
                        "Plant");

    SetupOutputVariable(
        state, "Chiller Heater " + this->FuelType + " Rate", OutputProcessor::Unit::W, this->FuelUseRate, "System", "Average", ChillerName);
    // Do not include this on meters, this would duplicate the cool fuel and heat fuel
    SetupOutputVariable(
        state, "Chiller Heater " + this->FuelType + " Energy", OutputProcessor::Unit::J, this->FuelEnergy, "System", "Sum", ChillerName);

    SetupOutputVariable(state,
                        "Chiller Heater Cooling " + this->FuelType + " Rate",
                        OutputProcessor::Unit::W,
                        this->CoolFuelUseRate,
                        "System",
                        "Average",
                        ChillerName);
    SetupOutputVariable(state,
                        "Chiller Heater Cooling " + this->FuelType + " Energy",
                        OutputProcessor::Unit::J,
                        this->CoolFuelEnergy,
                        "System",
                        "Sum",
                        ChillerName,
                        _,
                        this->FuelType,
                        "Cooling",
                        _,
                        "Plant");

    SetupOutputVariable(state, "Chiller Heater Cooling COP", OutputProcessor::Unit::W_W, this->FuelCOP, "System", "Average", ChillerName);

    SetupOutputVariable(state,
                        "Chiller Heater Heating " + this->FuelType + " Rate",
                        OutputProcessor::Unit::W,
                        this->HeatFuelUseRate,
                        "System",
                        "Average",
                        ChillerName);
    SetupOutputVariable(state,
                        "Chiller Heater Heating " + this->FuelType + " Energy",
                        OutputProcessor::Unit::J,
                        this->HeatFuelEnergy,
                        "System",
                        "Sum",
                        ChillerName,
                        _,
                        this->FuelType,
                        "Heating",
                        _,
                        "Plant");

    SetupOutputVariable(state, "Chiller Heater Electricity Rate", OutputProcessor::Unit::W, this->ElectricPower, "System", "Average", ChillerName);
    // Do not include this on meters, this would duplicate the cool electric and heat electric
    SetupOutputVariable(state, "Chiller Heater Electricity Energy", OutputProcessor::Unit::J, this->ElectricEnergy, "System", "Sum", ChillerName);

    SetupOutputVariable(
        state, "Chiller Heater Cooling Electricity Rate", OutputProcessor::Unit::W, this->CoolElectricPower, "System", "Average", ChillerName);
    SetupOutputVariable(state,
                        "Chiller Heater Cooling Electricity Energy",
                        OutputProcessor::Unit::J,
                        this->CoolElectricEnergy,
                        "System",
                        "Sum",
                        ChillerName,
                        _,
                        "Electricity",
                        "Cooling",
                        _,
                        "Plant");

    SetupOutputVariable(
        state, "Chiller Heater Heating Electricity Rate", OutputProcessor::Unit::W, this->HeatElectricPower, "System", "Average", ChillerName);
    SetupOutputVariable(state,
                        "Chiller Heater Heating Electricity Energy",
                        OutputProcessor::Unit::J,
                        this->HeatElectricEnergy,
                        "System",
                        "Sum",
                        ChillerName,
                        _,
                        "Electricity",
                        "Heating",
                        _,
                        "Plant");

    SetupOutputVariable(
        state, "Chiller Heater Evaporator Inlet Temperature", OutputProcessor::Unit::C, this->ChillReturnTemp, "System", "Average", ChillerName);
    SetupOutputVariable(
        state, "Chiller Heater Evaporator Outlet Temperature", OutputProcessor::Unit::C, this->ChillSupplyTemp, "System", "Average", ChillerName);
    SetupOutputVariable(
        state, "Chiller Heater Evaporator Mass Flow Rate", OutputProcessor::Unit::kg_s, this->ChillWaterFlowRate, "System", "Average", ChillerName);

    if (this->isWaterCooled) {
        SetupOutputVariable(
            state, "Chiller Heater Condenser Inlet Temperature", OutputProcessor::Unit::C, this->CondReturnTemp, "System", "Average", ChillerName);
        SetupOutputVariable(
            state, "Chiller Heater Condenser Outlet Temperature", OutputProcessor::Unit::C, this->CondSupplyTemp, "System", "Average", ChillerName);
        SetupOutputVariable(
            state, "Chiller Heater Condenser Mass Flow Rate", OutputProcessor::Unit::kg_s, this->CondWaterFlowRate, "System", "Average", ChillerName);
    } else {
        SetupOutputVariable(
            state, "Chiller Heater Condenser Inlet Temperature", OutputProcessor::Unit::C, this->CondReturnTemp, "System", "Average", ChillerName);
    }

    SetupOutputVariable(
        state, "Chiller Heater Heating Inlet Temperature", OutputProcessor::Unit::C, this->HotWaterReturnTemp, "System", "Average", ChillerName);
    SetupOutputVariable(
        state, "Chiller Heater Heating Outlet Temperature", OutputProcessor::Unit::C, this->HotWaterSupplyTemp, "System", "Average", ChillerName);
    SetupOutputVariable(
        state, "Chiller Heater Heating Mass Flow Rate", OutputProcessor::Unit::kg_s, this->HotWaterFlowRate, "System", "Average", ChillerName);

    SetupOutputVariable(
        state, "Chiller Heater Cooling Part Load Ratio", OutputProcessor::Unit::None, this->CoolPartLoadRatio, "System", "Average", ChillerName);
    SetupOutputVariable(
        state, "Chiller Heater Maximum Cooling Rate", OutputProcessor::Unit::W, this->CoolingCapacity, "System", "Average", ChillerName);
    SetupOutputVariable(
        state, "Chiller Heater Heating Part Load Ratio", OutputProcessor::Unit::None, this->HeatPartLoadRatio, "System", "Average", ChillerName);
    SetupOutputVariable(
        state, "Chiller Heater Maximum Heating Rate", OutputProcessor::Unit::W, this->HeatingCapacity, "System", "Average", ChillerName);

    SetupOutputVariable(
        state, "Chiller Heater Runtime Fraction", OutputProcessor::Unit::None, this->FractionOfPeriodRunning, "System", "Average", ChillerName);
}

void GasAbsorberSpecs::oneTimeInit(EnergyPlusData &state)
{
    // Locate the chillers on the plant loops for later usage
    bool errFlag = false;
    PlantUtilities::ScanPlantLoopsForObject(state,
                                            this->Name,
                                            DataPlant::TypeOf_Chiller_DFAbsorption,
                                            this->CWLoopNum,
                                            this->CWLoopSideNum,
                                            this->CWBranchNum,
                                            this->CWCompNum,
                                            errFlag,
                                            this->CHWLowLimitTemp,
                                            _,
                                            _,
                                            this->ChillReturnNodeNum,
                                            _);
    if (errFlag) {
        ShowFatalError(state, "InitGasAbsorber: Program terminated due to previous condition(s).");
    }

    PlantUtilities::ScanPlantLoopsForObject(state,
                                            this->Name,
                                            DataPlant::TypeOf_Chiller_DFAbsorption,
                                            this->HWLoopNum,
                                            this->HWLoopSideNum,
                                            this->HWBranchNum,
                                            this->HWCompNum,
                                            errFlag,
                                            _,
                                            _,
                                            _,
                                            this->HeatReturnNodeNum,
                                            _);
    if (errFlag) {
        ShowFatalError(state, "InitGasAbsorber: Program terminated due to previous condition(s).");
    }

    if (this->isWaterCooled) {
        PlantUtilities::ScanPlantLoopsForObject(state,
                                                this->Name,
                                                DataPlant::TypeOf_Chiller_DFAbsorption,
                                                this->CDLoopNum,
                                                this->CDLoopSideNum,
                                                this->CDBranchNum,
                                                this->CDCompNum,
                                                errFlag,
                                                _,
                                                _,
                                                _,
                                                this->CondReturnNodeNum,
                                                _);
        if (errFlag) {
            ShowFatalError(state, "InitGasAbsorber: Program terminated due to previous condition(s).");
        }
        PlantUtilities::InterConnectTwoPlantLoopSides(
            state, this->CWLoopNum, this->CWLoopSideNum, this->CDLoopNum, this->CDLoopSideNum, DataPlant::TypeOf_Chiller_DFAbsorption, true);
        PlantUtilities::InterConnectTwoPlantLoopSides(
            state, this->HWLoopNum, this->HWLoopSideNum, this->CDLoopNum, this->CDLoopSideNum, DataPlant::TypeOf_Chiller_DFAbsorption, true);
    }

    PlantUtilities::InterConnectTwoPlantLoopSides(
        state, this->CWLoopNum, this->CWLoopSideNum, this->HWLoopNum, this->HWLoopSideNum, DataPlant::TypeOf_Chiller_DFAbsorption, true);

    // check if outlet node of chilled water side has a setpoint.
    if ((state.dataLoopNodes->Node(this->ChillSupplyNodeNum).TempSetPoint == DataLoopNode::SensedNodeFlagValue) &&
        (state.dataLoopNodes->Node(this->ChillSupplyNodeNum).TempSetPointHi == DataLoopNode::SensedNodeFlagValue)) {
        if (!state.dataGlobal->AnyEnergyManagementSystemInModel) {
            if (!this->ChillSetPointErrDone) {
                ShowWarningError(state, "Missing temperature setpoint on cool side for chiller heater named " + this->Name);
                ShowContinueError(state, "  A temperature setpoint is needed at the outlet node of this chiller, use a SetpointManager");
                ShowContinueError(state, "  The overall loop setpoint will be assumed for chiller. The simulation continues ... ");
                this->ChillSetPointErrDone = true;
            }
        } else {
            // need call to EMS to check node
            errFlag = false; // but not really fatal yet, but should be.
            EMSManager::CheckIfNodeSetPointManagedByEMS(state, this->ChillSupplyNodeNum, EMSManager::SPControlType::iTemperatureSetPoint, errFlag);
            state.dataLoopNodes->NodeSetpointCheck(this->ChillSupplyNodeNum).needsSetpointChecking = false;
            if (errFlag) {
                if (!this->ChillSetPointErrDone) {
                    ShowWarningError(state, "Missing temperature setpoint on cool side for chiller heater named " + this->Name);
                    ShowContinueError(state, "  A temperature setpoint is needed at the outlet node of this chiller evaporator ");
                    ShowContinueError(state, "  use a Setpoint Manager to establish a setpoint at the chiller evaporator outlet node ");
                    ShowContinueError(state, "  or use an EMS actuator to establish a setpoint at the outlet node ");
                    ShowContinueError(state, "  The overall loop setpoint will be assumed for chiller. The simulation continues ... ");
                    this->ChillSetPointErrDone = true;
                }
            }
        }
        this->ChillSetPointSetToLoop = true;
        state.dataLoopNodes->Node(this->ChillSupplyNodeNum).TempSetPoint =
            state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPoint;
        state.dataLoopNodes->Node(this->ChillSupplyNodeNum).TempSetPointHi =
            state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPointHi;
    }
    // check if outlet node of hot water side has a setpoint.
    if ((state.dataLoopNodes->Node(this->HeatSupplyNodeNum).TempSetPoint == DataLoopNode::SensedNodeFlagValue) &&
        (state.dataLoopNodes->Node(this->HeatSupplyNodeNum).TempSetPointLo == DataLoopNode::SensedNodeFlagValue)) {
        if (!state.dataGlobal->AnyEnergyManagementSystemInModel) {
            if (!this->HeatSetPointErrDone) {
                ShowWarningError(state, "Missing temperature setpoint on heat side for chiller heater named " + this->Name);
                ShowContinueError(state, "  A temperature setpoint is needed at the outlet node of this chiller, use a SetpointManager");
                ShowContinueError(state, "  The overall loop setpoint will be assumed for chiller. The simulation continues ... ");
                this->HeatSetPointErrDone = true;
            }
        } else {
            // need call to EMS to check node
            errFlag = false; // but not really fatal yet, but should be.
            EMSManager::CheckIfNodeSetPointManagedByEMS(state, this->HeatSupplyNodeNum, EMSManager::SPControlType::iTemperatureSetPoint, errFlag);
            state.dataLoopNodes->NodeSetpointCheck(this->HeatSupplyNodeNum).needsSetpointChecking = false;
            if (errFlag) {
                if (!this->HeatSetPointErrDone) {
                    ShowWarningError(state, "Missing temperature setpoint on heat side for chiller heater named " + this->Name);
                    ShowContinueError(state, "  A temperature setpoint is needed at the outlet node of this chiller heater ");
                    ShowContinueError(state, "  use a Setpoint Manager to establish a setpoint at the heater side outlet node ");
                    ShowContinueError(state, "  or use an EMS actuator to establish a setpoint at the outlet node ");
                    ShowContinueError(state, "  The overall loop setpoint will be assumed for heater side. The simulation continues ... ");
                    this->HeatSetPointErrDone = true;
                }
            }
        }
        this->HeatSetPointSetToLoop = true;
        state.dataLoopNodes->Node(this->HeatSupplyNodeNum).TempSetPoint =
            state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(this->HWLoopNum).TempSetPointNodeNum).TempSetPoint;
        state.dataLoopNodes->Node(this->HeatSupplyNodeNum).TempSetPointLo =
            state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(this->HWLoopNum).TempSetPointNodeNum).TempSetPointLo;
    }
}

void GasAbsorberSpecs::initialize(EnergyPlusData &state)
{
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   June 2003

    // This subroutine is for initializations of direct fired absorption chiller
    // components.

    // Uses the status flags to trigger initializations.

    std::string const RoutineName("InitGasAbsorber");

    Real64 rho = 0.0;  // local fluid density
    Real64 mdot = 0.0; // lcoal fluid mass flow rate

    if (this->oneTimeFlag) {
        this->setupOutputVariables(state);
        this->oneTimeFlag = false;
    }

    // Init more variables
    if (this->plantScanFlag) {
        this->oneTimeInit(state);
        this->plantScanFlag = false;
    }

    int CondInletNode = this->CondReturnNodeNum;
    int CondOutletNode = this->CondSupplyNodeNum;
    int HeatInletNode = this->HeatReturnNodeNum;
    int HeatOutletNode = this->HeatSupplyNodeNum;

    if (this->envrnFlag && state.dataGlobal->BeginEnvrnFlag && (state.dataPlnt->PlantFirstSizesOkayToFinalize)) {

        if (this->isWaterCooled) {
            // init max available condenser water flow rate
            if (this->CDLoopNum > 0) {
                rho = FluidProperties::GetDensityGlycol(state,
                                                        state.dataPlnt->PlantLoop(this->CDLoopNum).FluidName,
                                                        DataGlobalConstants::CWInitConvTemp,
                                                        state.dataPlnt->PlantLoop(this->CDLoopNum).FluidIndex,
                                                        RoutineName);
            } else {
                rho = Psychrometrics::RhoH2O(DataGlobalConstants::InitConvTemp);
            }

            this->DesCondMassFlowRate = rho * this->CondVolFlowRate;
            PlantUtilities::InitComponentNodes(state,
                                               0.0,
                                               this->DesCondMassFlowRate,
                                               CondInletNode,
                                               CondOutletNode,
                                               this->CDLoopNum,
                                               this->CDLoopSideNum,
                                               this->CDBranchNum,
                                               this->CDCompNum);
        }

        if (this->HWLoopNum > 0) {
            rho = FluidProperties::GetDensityGlycol(state,
                                                    state.dataPlnt->PlantLoop(this->HWLoopNum).FluidName,
                                                    DataGlobalConstants::HWInitConvTemp,
                                                    state.dataPlnt->PlantLoop(this->HWLoopNum).FluidIndex,
                                                    RoutineName);
        } else {
            rho = Psychrometrics::RhoH2O(DataGlobalConstants::InitConvTemp);
        }
        this->DesHeatMassFlowRate = rho * this->HeatVolFlowRate;
        // init available hot water flow rate
        PlantUtilities::InitComponentNodes(state,
                                           0.0,
                                           this->DesHeatMassFlowRate,
                                           HeatInletNode,
                                           HeatOutletNode,
                                           this->HWLoopNum,
                                           this->HWLoopSideNum,
                                           this->HWBranchNum,
                                           this->HWCompNum);

        if (this->CWLoopNum > 0) {
            rho = FluidProperties::GetDensityGlycol(state,
                                                    state.dataPlnt->PlantLoop(this->CWLoopNum).FluidName,
                                                    DataGlobalConstants::CWInitConvTemp,
                                                    state.dataPlnt->PlantLoop(this->CWLoopNum).FluidIndex,
                                                    RoutineName);
        } else {
            rho = Psychrometrics::RhoH2O(DataGlobalConstants::InitConvTemp);
        }
        this->DesEvapMassFlowRate = rho * this->EvapVolFlowRate;
        // init available hot water flow rate
        PlantUtilities::InitComponentNodes(state,
                                           0.0,
                                           this->DesEvapMassFlowRate,
                                           this->ChillReturnNodeNum,
                                           this->ChillSupplyNodeNum,
                                           this->CWLoopNum,
                                           this->CWLoopSideNum,
                                           this->CWBranchNum,
                                           this->CWCompNum);

        this->envrnFlag = false;
    }

    if (!state.dataGlobal->BeginEnvrnFlag) {
        this->envrnFlag = true;
    }

    // this component model works off setpoints on the leaving node
    // fill from plant if needed
    if (this->ChillSetPointSetToLoop) {
        state.dataLoopNodes->Node(this->ChillSupplyNodeNum).TempSetPoint =
            state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPoint;
        state.dataLoopNodes->Node(this->ChillSupplyNodeNum).TempSetPointHi =
            state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPointHi;
    }

    if (this->HeatSetPointSetToLoop) {
        state.dataLoopNodes->Node(this->HeatSupplyNodeNum).TempSetPoint =
            state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(this->HWLoopNum).TempSetPointNodeNum).TempSetPoint;
        state.dataLoopNodes->Node(this->HeatSupplyNodeNum).TempSetPointLo =
            state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(this->HWLoopNum).TempSetPointNodeNum).TempSetPointLo;
    }

    if ((this->isWaterCooled) && ((this->InHeatingMode) || (this->InCoolingMode)) && (!this->plantScanFlag)) {
        mdot = this->DesCondMassFlowRate;

        PlantUtilities::SetComponentFlowRate(
            state, mdot, this->CondReturnNodeNum, this->CondSupplyNodeNum, this->CDLoopNum, this->CDLoopSideNum, this->CDBranchNum, this->CDCompNum);

    } else {
        mdot = 0.0;
        if (this->CDLoopNum > 0 && this->isWaterCooled) {
            PlantUtilities::SetComponentFlowRate(state,
                                                 mdot,
                                                 this->CondReturnNodeNum,
                                                 this->CondSupplyNodeNum,
                                                 this->CDLoopNum,
                                                 this->CDLoopSideNum,
                                                 this->CDBranchNum,
                                                 this->CDCompNum);
        }
    }
}

void GasAbsorberSpecs::size(EnergyPlusData &state)
{
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   June 2003
    //       MODIFIED       November 2013 Daeho Kang, add component sizing table entries

    // This subroutine is for sizing direct fired gas absorption chiller components for which
    // capacities and flow rates have not been specified in the input.

    // METHODOLOGY EMPLOYED:
    // Obtains evaporator flow rate from the plant sizing array. Calculates nominal capacity from
    // the evaporator flow rate and the chilled water loop design delta T. The condenser flow rate
    // is calculated from the nominal capacity, the COP, and the condenser loop design delta T.

    std::string const RoutineName("SizeGasAbsorber");

    std::string equipName;
    Real64 Cp;                     // local fluid specific heat
    Real64 rho;                    // local fluid density
    Real64 tmpNomCap;              // local nominal capacity cooling power
    Real64 tmpEvapVolFlowRate;     // local evaporator design volume flow rate
    Real64 tmpCondVolFlowRate;     // local condenser design volume flow rate
    Real64 tmpHeatRecVolFlowRate;  // local heat recovery design volume flow rate
    Real64 NomCapUser;             // Hardsized nominal capacity for reporting
    Real64 EvapVolFlowRateUser;    // Hardsized evaporator volume flow rate for reporting
    Real64 CondVolFlowRateUser;    // Hardsized condenser flow rate for reporting
    Real64 HeatRecVolFlowRateUser; // Hardsized generator flow rate for reporting

    bool ErrorsFound = false;
    tmpNomCap = this->NomCoolingCap;
    tmpEvapVolFlowRate = this->EvapVolFlowRate;
    tmpCondVolFlowRate = this->CondVolFlowRate;
    tmpHeatRecVolFlowRate = this->HeatVolFlowRate;
    // Commenting this could cause diffs - NomCapUser = 0.0;
    // Commenting this could cause diffs - EvapVolFlowRateUser = 0.0;
    // Commenting this could cause diffs - CondVolFlowRateUser = 0.0;
    // Commenting this could cause diffs - HeatRecVolFlowRateUser = 0.0;

    int PltSizCondNum = 0; // Plant Sizing index for condenser loop
    if (this->isWaterCooled) PltSizCondNum = state.dataPlnt->PlantLoop(this->CDLoopNum).PlantSizNum;
    int PltSizHeatNum = state.dataPlnt->PlantLoop(this->HWLoopNum).PlantSizNum;
    int PltSizCoolNum = state.dataPlnt->PlantLoop(this->CWLoopNum).PlantSizNum;

    if (PltSizCoolNum > 0) {
        if (state.dataSize->PlantSizData(PltSizCoolNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
            Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                        state.dataPlnt->PlantLoop(this->CWLoopNum).FluidName,
                                                        DataGlobalConstants::CWInitConvTemp,
                                                        state.dataPlnt->PlantLoop(this->CWLoopNum).FluidIndex,
                                                        RoutineName);
            rho = FluidProperties::GetDensityGlycol(state,
                                                    state.dataPlnt->PlantLoop(this->CWLoopNum).FluidName,
                                                    DataGlobalConstants::CWInitConvTemp,
                                                    state.dataPlnt->PlantLoop(this->CWLoopNum).FluidIndex,
                                                    RoutineName);
            tmpNomCap = Cp * rho * state.dataSize->PlantSizData(PltSizCoolNum).DeltaT * state.dataSize->PlantSizData(PltSizCoolNum).DesVolFlowRate *
                        this->SizFac;
            if (!this->NomCoolingCapWasAutoSized) tmpNomCap = this->NomCoolingCap;
        } else {
            if (this->NomCoolingCapWasAutoSized) tmpNomCap = 0.0;
        }
        if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
            if (this->NomCoolingCapWasAutoSized) {
                this->NomCoolingCap = tmpNomCap;
                if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(
                        state, "ChillerHeater:Absorption:DirectFired", this->Name, "Design Size Nominal Cooling Capacity [W]", tmpNomCap);
                }
                if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(
                        state, "ChillerHeater:Absorption:DirectFired", this->Name, "Initial Design Size Nominal Cooling Capacity [W]", tmpNomCap);
                }
            } else {
                if (this->NomCoolingCap > 0.0 && tmpNomCap > 0.0) {
                    NomCapUser = this->NomCoolingCap;
                    if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(state,
                                                     "ChillerHeater:Absorption:DirectFired",
                                                     this->Name,
                                                     "Design Size Nominal Cooling Capacity [W]",
                                                     tmpNomCap,
                                                     "User-Specified Nominal Cooling Capacity [W]",
                                                     NomCapUser);
                        if (state.dataGlobal->DisplayExtraWarnings) {
                            if ((std::abs(tmpNomCap - NomCapUser) / NomCapUser) > state.dataSize->AutoVsHardSizingThreshold) {
                                ShowMessage(state, "SizeChillerHeaterAbsorptionDirectFired: Potential issue with equipment sizing for " + this->Name);
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
        if (this->NomCoolingCapWasAutoSized) {
            if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                ShowSevereError(state, "SizeGasAbsorber: ChillerHeater:Absorption:DirectFired=\"" + this->Name + "\", autosize error.");
                ShowContinueError(state, "Autosizing of Direct Fired Absorption Chiller nominal cooling capacity requires");
                ShowContinueError(state, "a cooling loop Sizing:Plant object.");
                ErrorsFound = true;
            }
        } else {
            if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                if (this->NomCoolingCap > 0.0) {
                    BaseSizer::reportSizerOutput(
                        state, "ChillerHeater:Absorption:DirectFired", this->Name, "User-Specified Nominal Capacity [W]", this->NomCoolingCap);
                }
            }
        }
    }

    if (PltSizCoolNum > 0) {
        if (state.dataSize->PlantSizData(PltSizCoolNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
            tmpEvapVolFlowRate = state.dataSize->PlantSizData(PltSizCoolNum).DesVolFlowRate * this->SizFac;
            if (!this->EvapVolFlowRateWasAutoSized) tmpEvapVolFlowRate = this->EvapVolFlowRate;
        } else {
            if (this->EvapVolFlowRateWasAutoSized) tmpEvapVolFlowRate = 0.0;
        }
        if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
            if (this->EvapVolFlowRateWasAutoSized) {
                this->EvapVolFlowRate = tmpEvapVolFlowRate;
                if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(state,
                                                 "ChillerHeater:Absorption:DirectFired",
                                                 this->Name,
                                                 "Design Size Design Chilled Water Flow Rate [m3/s]",
                                                 tmpEvapVolFlowRate);
                }
                if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(state,
                                                 "ChillerHeater:Absorption:DirectFired",
                                                 this->Name,
                                                 "Initial Design Size Design Chilled Water Flow Rate [m3/s]",
                                                 tmpEvapVolFlowRate);
                }
            } else {
                if (this->EvapVolFlowRate > 0.0 && tmpEvapVolFlowRate > 0.0) {
                    EvapVolFlowRateUser = this->EvapVolFlowRate;
                    if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(state,
                                                     "ChillerHeater:Absorption:DirectFired",
                                                     this->Name,
                                                     "Design Size Design Chilled Water Flow Rate [m3/s]",
                                                     tmpEvapVolFlowRate,
                                                     "User-Specified Design Chilled Water Flow Rate [m3/s]",
                                                     EvapVolFlowRateUser);
                        if (state.dataGlobal->DisplayExtraWarnings) {
                            if ((std::abs(tmpEvapVolFlowRate - EvapVolFlowRateUser) / EvapVolFlowRateUser) >
                                state.dataSize->AutoVsHardSizingThreshold) {
                                ShowMessage(state, "SizeChillerAbsorptionDirectFired: Potential issue with equipment sizing for " + this->Name);
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
                ShowSevereError(state, "SizeGasAbsorber: ChillerHeater:Absorption:DirectFired=\"" + this->Name + "\", autosize error.");
                ShowContinueError(state, "Autosizing of Direct Fired Absorption Chiller evap flow rate requires");
                ShowContinueError(state, "a cooling loop Sizing:Plant object.");
                ErrorsFound = true;
            }
        } else {
            if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                if (this->EvapVolFlowRate > 0.0) {
                    BaseSizer::reportSizerOutput(state,
                                                 "ChillerHeater:Absorption:DirectFired",
                                                 this->Name,
                                                 "User-Specified Design Chilled Water Flow Rate [m3/s]",
                                                 this->EvapVolFlowRate);
                }
            }
        }
    }

    PlantUtilities::RegisterPlantCompDesignFlow(state, this->ChillReturnNodeNum, tmpEvapVolFlowRate);

    if (PltSizHeatNum > 0) {
        if (state.dataSize->PlantSizData(PltSizHeatNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
            tmpHeatRecVolFlowRate = state.dataSize->PlantSizData(PltSizHeatNum).DesVolFlowRate * this->SizFac;
            if (!this->HeatVolFlowRateWasAutoSized) tmpHeatRecVolFlowRate = this->HeatVolFlowRate;

        } else {
            if (this->HeatVolFlowRateWasAutoSized) tmpHeatRecVolFlowRate = 0.0;
        }
        if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
            if (this->HeatVolFlowRateWasAutoSized) {
                this->HeatVolFlowRate = tmpHeatRecVolFlowRate;
                if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(state,
                                                 "ChillerHeater:Absorption:DirectFired",
                                                 this->Name,
                                                 "Design Size Design Hot Water Flow Rate [m3/s]",
                                                 tmpHeatRecVolFlowRate);
                }
                if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(state,
                                                 "ChillerHeater:Absorption:DirectFired",
                                                 this->Name,
                                                 "Initial Design Size Design Hot Water Flow Rate [m3/s]",
                                                 tmpHeatRecVolFlowRate);
                }
            } else {
                if (this->HeatVolFlowRate > 0.0 && tmpHeatRecVolFlowRate > 0.0) {
                    HeatRecVolFlowRateUser = this->HeatVolFlowRate;
                    if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(state,
                                                     "ChillerHeater:Absorption:DirectFired",
                                                     this->Name,
                                                     "Design Size Design Hot Water Flow Rate [m3/s]",
                                                     tmpHeatRecVolFlowRate,
                                                     "User-Specified Design Hot Water Flow Rate [m3/s]",
                                                     HeatRecVolFlowRateUser);
                        if (state.dataGlobal->DisplayExtraWarnings) {
                            if ((std::abs(tmpHeatRecVolFlowRate - HeatRecVolFlowRateUser) / HeatRecVolFlowRateUser) >
                                state.dataSize->AutoVsHardSizingThreshold) {
                                ShowMessage(state, "SizeChillerHeaterAbsorptionDirectFired: Potential issue with equipment sizing for " + this->Name);
                                ShowContinueError(state,
                                                  format("User-Specified Design Hot Water Flow Rate of {:.5R} [m3/s]", HeatRecVolFlowRateUser));
                                ShowContinueError(
                                    state, format("differs from Design Size Design Hot Water Flow Rate of {:.5R} [m3/s]", tmpHeatRecVolFlowRate));
                                ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                                ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                            }
                        }
                    }
                    tmpHeatRecVolFlowRate = HeatRecVolFlowRateUser;
                }
            }
        }
    } else {
        if (this->HeatVolFlowRateWasAutoSized) {
            if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                ShowSevereError(state, "SizeGasAbsorber: ChillerHeater:Absorption:DirectFired=\"" + this->Name + "\", autosize error.");
                ShowContinueError(state, "Autosizing of Direct Fired Absorption Chiller hot water flow rate requires");
                ShowContinueError(state, "a heating loop Sizing:Plant object.");
                ErrorsFound = true;
            }
        } else {
            if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                if (this->HeatVolFlowRate > 0.0) {
                    BaseSizer::reportSizerOutput(state,
                                                 "ChillerHeater:Absorption:DirectFired",
                                                 this->Name,
                                                 "User-Specified Design Hot Water Flow Rate [m3/s]",
                                                 this->HeatVolFlowRate);
                }
            }
        }
    }

    PlantUtilities::RegisterPlantCompDesignFlow(state, this->HeatReturnNodeNum, tmpHeatRecVolFlowRate);

    if (PltSizCondNum > 0 && PltSizCoolNum > 0) {
        if (state.dataSize->PlantSizData(PltSizCoolNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow && tmpNomCap > 0.0) {

            Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                        state.dataPlnt->PlantLoop(this->CDLoopNum).FluidName,
                                                        this->TempDesCondReturn,
                                                        state.dataPlnt->PlantLoop(this->CDLoopNum).FluidIndex,
                                                        RoutineName);
            rho = FluidProperties::GetDensityGlycol(state,
                                                    state.dataPlnt->PlantLoop(this->CDLoopNum).FluidName,
                                                    this->TempDesCondReturn,
                                                    state.dataPlnt->PlantLoop(this->CDLoopNum).FluidIndex,
                                                    RoutineName);
            tmpCondVolFlowRate = tmpNomCap * (1.0 + this->FuelCoolRatio) / (state.dataSize->PlantSizData(PltSizCondNum).DeltaT * Cp * rho);
            if (!this->CondVolFlowRateWasAutoSized) tmpCondVolFlowRate = this->CondVolFlowRate;
            // IF (PlantFirstSizesOkayToFinalize) GasAbsorber(ChillNum)%CondVolFlowRate = tmpCondVolFlowRate
        } else {
            if (this->CondVolFlowRateWasAutoSized) tmpCondVolFlowRate = 0.0;
            // IF (PlantFirstSizesOkayToFinalize) GasAbsorber(ChillNum)%CondVolFlowRate = tmpCondVolFlowRate
        }
        if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
            if (this->CondVolFlowRateWasAutoSized) {
                this->CondVolFlowRate = tmpCondVolFlowRate;
                if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(state,
                                                 "ChillerHeater:Absorption:DirectFired",
                                                 this->Name,
                                                 "Design Size Design Condenser Water Flow Rate [m3/s]",
                                                 tmpCondVolFlowRate);
                }
                if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(state,
                                                 "ChillerHeater:Absorption:DirectFired",
                                                 this->Name,
                                                 "Initial Design Size Design Condenser Water Flow Rate [m3/s]",
                                                 tmpCondVolFlowRate);
                }
            } else {
                if (this->CondVolFlowRate > 0.0 && tmpCondVolFlowRate > 0.0) {
                    CondVolFlowRateUser = this->CondVolFlowRate;
                    if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(state,
                                                     "ChillerHeater:Absorption:DirectFired",
                                                     this->Name,
                                                     "Design Size Design Condenser Water Flow Rate [m3/s]",
                                                     tmpCondVolFlowRate,
                                                     "User-Specified Design Condenser Water Flow Rate [m3/s]",
                                                     CondVolFlowRateUser);
                        if (state.dataGlobal->DisplayExtraWarnings) {
                            if ((std::abs(tmpCondVolFlowRate - CondVolFlowRateUser) / CondVolFlowRateUser) >
                                state.dataSize->AutoVsHardSizingThreshold) {
                                ShowMessage(state, "SizeChillerAbsorptionDirectFired: Potential issue with equipment sizing for " + this->Name);
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
                ShowSevereError(state, "SizeGasAbsorber: ChillerHeater:Absorption:DirectFired=\"" + this->Name + "\", autosize error.");
                ShowContinueError(state, "Autosizing of Direct Fired Absorption Chiller condenser flow rate requires a condenser");
                ShowContinueError(state, "loop Sizing:Plant object.");
                ErrorsFound = true;
            }
        } else {
            if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                if (this->CondVolFlowRate > 0.0) {
                    BaseSizer::reportSizerOutput(state,
                                                 "ChillerHeater:Absorption:DirectFired",
                                                 this->Name,
                                                 "User-Specified Design Condenser Water Flow Rate [m3/s]",
                                                 this->CondVolFlowRate);
                }
            }
        }
    }

    // save the design condenser water volumetric flow rate for use by the condenser water loop sizing algorithms
    if (this->isWaterCooled) PlantUtilities::RegisterPlantCompDesignFlow(state, this->CondReturnNodeNum, tmpCondVolFlowRate);

    if (ErrorsFound) {
        ShowFatalError(state, "Preceding sizing errors cause program termination");
    }

    if (state.dataPlnt->PlantFinalSizesOkayToReport) {
        // create predefined report
        equipName = this->Name;
        OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchMechType, equipName, "ChillerHeater:Absorption:DirectFired");
        OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchMechNomEff, equipName, this->FuelCoolRatio);
        OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchMechNomCap, equipName, this->NomCoolingCap);
    }
}

void GasAbsorberSpecs::calculateChiller(EnergyPlusData &state, Real64 &MyLoad)
{
    //       AUTHOR         Jason Glazer
    //       DATE WRITTEN   March 2001

    // Simulate a direct fired (gas consuming) absorption chiller using
    // curves and inputs similar to DOE-2.1e

    // METHODOLOGY EMPLOYED:
    // Curve fit of performance data

    // REFERENCES:
    // 1.  DOE-2.1e Supplement and source code
    // 2.  CoolTools GasMod work

    // FlowLock = 0  if mass flow rates may be changed by loop components
    // FlowLock = 1  if mass flow rates may not be changed by loop components

    std::string const RoutineName("CalcGasAbsorberChillerModel");

    // Local copies of GasAbsorberSpecs Type
    // all variables that are local copies of data structure
    // variables are prefaced with an "l" for local.
    Real64 lNomCoolingCap;    // W - design nominal capacity of Absorber
    Real64 lFuelCoolRatio;    // ratio of fuel input to cooling output
    Real64 lFuelHeatRatio;    // ratio of fuel input to heating output
    Real64 lElecCoolRatio;    // ratio of electricity input to cooling output
    int lChillReturnNodeNum;  // Node number on the inlet side of the plant
    int lChillSupplyNodeNum;  // Node number on the outlet side of the plant
    int lCondReturnNodeNum;   // Node number on the inlet side of the condenser
    Real64 lMinPartLoadRat;   // min allowed operating frac full load
    Real64 lMaxPartLoadRat;   // max allowed operating frac full load
    int lCoolCapFTCurve;      // cooling capacity as a function of temperature curve
    int lFuelCoolFTCurve;     // Fuel-Input-to cooling output Ratio Function of Temperature Curve
    int lFuelCoolFPLRCurve;   // Fuel-Input-to cooling output Ratio Function of Part Load Ratio Curve
    int lElecCoolFTCurve;     // Electric-Input-to cooling output Ratio Function of Temperature Curve
    int lElecCoolFPLRCurve;   // Electric-Input-to cooling output Ratio Function of Part Load Ratio Curve
    bool lIsEnterCondensTemp; // if using entering conderser water temperature is TRUE, exiting is FALSE
    bool lIsWaterCooled;      // if water cooled it is TRUE
    Real64 lCHWLowLimitTemp;  // Chilled Water Lower Limit Temperature
    // Local copies of GasAbsorberReportVars Type
    Real64 lCoolingLoad(0.0);              // cooling load on the chiller (previously called QEvap)
    Real64 lTowerLoad(0.0);                // load on the cooling tower/condenser (previously called QCond)
    Real64 lCoolFuelUseRate(0.0);          // instantaneous use of gas for period for cooling
    Real64 lHeatFuelUseRate(0.0);          // instantaneous use of gas for period for heating
    Real64 lCoolElectricPower(0.0);        // parasitic electric power used  for cooling
    Real64 lHeatElectricPower(0.0);        // parasitic electric power used  for heating
    Real64 lChillReturnTemp(0.0);          // reporting: evaporator inlet temperature (was EvapInletTemp)
    Real64 lChillSupplyTemp(0.0);          // reporting: evaporator outlet temperature (was EvapOutletTemp)
    Real64 lChillWaterMassFlowRate(0.0);   // reporting: evaporator mass flow rate (was Evapmdot)
    Real64 lCondReturnTemp(0.0);           // reporting: condenser inlet temperature (was CondInletTemp)
    Real64 lCondSupplyTemp(0.0);           // reporting: condenser outlet temperature (was CondOutletTemp)
    Real64 lCondWaterMassFlowRate(0.0);    // reporting: condenser mass flow rate (was Condmdot)
    Real64 lCoolPartLoadRatio(0.0);        // operating part load ratio (load/capacity for cooling)
    Real64 lHeatPartLoadRatio(0.0);        // operating part load ratio (load/capacity for heating)
    Real64 lAvailableCoolingCapacity(0.0); // current capacity after temperature adjustment
    Real64 lFractionOfPeriodRunning(0.0);
    Real64 PartLoadRat(0.0);           // actual operating part load ratio of unit (ranges from minplr to 1)
    Real64 lChillWaterMassflowratemax; // Maximum flow rate through the evaporator

    // other local variables
    Real64 ChillDeltaTemp; // chilled water temperature difference
    Real64 ChillSupplySetPointTemp(0.0);

    Real64 calcCondTemp; // the condenser temperature used for curve calculation
    // either return or supply depending on user input
    Real64 revisedEstimateAvailCap; // final estimate of available capacity if using leaving
    // condenser water temperature
    Real64 errorAvailCap; // error fraction on final estimate of AvailableCoolingCapacity
    int LoopNum;
    int LoopSideNum;
    Real64 Cp_CW; // local fluid specific heat for chilled water
    Real64 Cp_CD; // local fluid specific heat for condenser water

    // set node values to data structure values for nodes

    lChillReturnNodeNum = this->ChillReturnNodeNum;
    lChillSupplyNodeNum = this->ChillSupplyNodeNum;
    lCondReturnNodeNum = this->CondReturnNodeNum;

    // set local copies of data from rest of input structure

    lNomCoolingCap = this->NomCoolingCap;
    lFuelCoolRatio = this->FuelCoolRatio;
    lFuelHeatRatio = this->FuelHeatRatio;
    lElecCoolRatio = this->ElecCoolRatio;
    lMinPartLoadRat = this->MinPartLoadRat;
    lMaxPartLoadRat = this->MaxPartLoadRat;
    lCoolCapFTCurve = this->CoolCapFTCurve;
    lFuelCoolFTCurve = this->FuelCoolFTCurve;
    lFuelCoolFPLRCurve = this->FuelCoolFPLRCurve;
    lElecCoolFTCurve = this->ElecCoolFTCurve;
    lElecCoolFPLRCurve = this->ElecCoolFPLRCurve;
    lIsEnterCondensTemp = this->isEnterCondensTemp;
    lIsWaterCooled = this->isWaterCooled;
    lCHWLowLimitTemp = this->CHWLowLimitTemp;

    lHeatElectricPower = this->HeatElectricPower;
    lHeatFuelUseRate = this->HeatFuelUseRate;
    lHeatPartLoadRatio = this->HeatPartLoadRatio;

    // initialize entering conditions
    lChillReturnTemp = state.dataLoopNodes->Node(lChillReturnNodeNum).Temp;
    lChillWaterMassFlowRate = state.dataLoopNodes->Node(lChillReturnNodeNum).MassFlowRate;
    lCondReturnTemp = state.dataLoopNodes->Node(lCondReturnNodeNum).Temp;
    // Commenting this could be cause of diffs - lCondWaterMassFlowRate = Node(lCondReturnNodeNum).MassFlowRate;
    {
        auto const SELECT_CASE_var(state.dataPlnt->PlantLoop(this->CWLoopNum).LoopDemandCalcScheme);
        if (SELECT_CASE_var == DataPlant::iLoopDemandCalcScheme::SingleSetPoint) {
            ChillSupplySetPointTemp = state.dataLoopNodes->Node(lChillSupplyNodeNum).TempSetPoint;
        } else if (SELECT_CASE_var == DataPlant::iLoopDemandCalcScheme::DualSetPointDeadBand) {
            ChillSupplySetPointTemp = state.dataLoopNodes->Node(lChillSupplyNodeNum).TempSetPointHi;
        } else {
            assert(false);
        }
    }
    ChillDeltaTemp = std::abs(lChillReturnTemp - ChillSupplySetPointTemp);

    Cp_CW = FluidProperties::GetSpecificHeatGlycol(state,
                                                   state.dataPlnt->PlantLoop(this->CWLoopNum).FluidName,
                                                   lChillReturnTemp,
                                                   state.dataPlnt->PlantLoop(this->CWLoopNum).FluidIndex,
                                                   RoutineName);
    Cp_CD = 0; // putting this here as a dummy initialization to hush the compiler warning, in real runs this value should never be used
    if (this->CDLoopNum > 0) {
        Cp_CD = FluidProperties::GetSpecificHeatGlycol(state,
                                                       state.dataPlnt->PlantLoop(this->CDLoopNum).FluidName,
                                                       lChillReturnTemp,
                                                       state.dataPlnt->PlantLoop(this->CDLoopNum).FluidIndex,
                                                       RoutineName);
    }

    // If no loop demand or Absorber OFF, return
    // will need to modify when absorber can act as a boiler
    if (MyLoad >= 0 || !((this->InHeatingMode) || (this->InCoolingMode))) {
        // set node temperatures
        lChillSupplyTemp = lChillReturnTemp;
        lCondSupplyTemp = lCondReturnTemp;
        lCondWaterMassFlowRate = 0.0;
        if (lIsWaterCooled) {
            PlantUtilities::SetComponentFlowRate(state,
                                                 lCondWaterMassFlowRate,
                                                 this->CondReturnNodeNum,
                                                 this->CondSupplyNodeNum,
                                                 this->CDLoopNum,
                                                 this->CDLoopSideNum,
                                                 this->CDBranchNum,
                                                 this->CDCompNum);
        }
        // Commenting this could cause diffs - ChillDeltaTemp = 0.0;
        lFractionOfPeriodRunning = min(1.0, max(lHeatPartLoadRatio, lCoolPartLoadRatio) / lMinPartLoadRat);
    } else {

        // if water cooled use the input node otherwise just use outside air temperature
        if (lIsWaterCooled) {
            // most manufacturers rate have tables of entering condenser water temperature
            // but a few use leaving condenser water temperature so we have a flag
            // when leaving is used it uses the previous iterations value of the value
            lCondReturnTemp = state.dataLoopNodes->Node(lCondReturnNodeNum).Temp;
            if (lIsEnterCondensTemp) {
                calcCondTemp = lCondReturnTemp;
            } else {
                if (this->oldCondSupplyTemp == 0) {
                    this->oldCondSupplyTemp = lCondReturnTemp + 8.0; // if not previously estimated assume 8C greater than return
                }
                calcCondTemp = this->oldCondSupplyTemp;
            }
            // Set mass flow rates
            lCondWaterMassFlowRate = this->DesCondMassFlowRate;
            PlantUtilities::SetComponentFlowRate(state,
                                                 lCondWaterMassFlowRate,
                                                 this->CondReturnNodeNum,
                                                 this->CondSupplyNodeNum,
                                                 this->CDLoopNum,
                                                 this->CDLoopSideNum,
                                                 this->CDBranchNum,
                                                 this->CDCompNum);
        } else {
            // air cooled
            state.dataLoopNodes->Node(lCondReturnNodeNum).Temp = state.dataLoopNodes->Node(lCondReturnNodeNum).OutAirDryBulb;
            calcCondTemp = state.dataLoopNodes->Node(lCondReturnNodeNum).OutAirDryBulb;
            lCondReturnTemp = state.dataLoopNodes->Node(lCondReturnNodeNum).Temp;
            lCondWaterMassFlowRate = 0.0;
            if (this->CDLoopNum > 0) {
                PlantUtilities::SetComponentFlowRate(state,
                                                     lCondWaterMassFlowRate,
                                                     this->CondReturnNodeNum,
                                                     this->CondSupplyNodeNum,
                                                     this->CDLoopNum,
                                                     this->CDLoopSideNum,
                                                     this->CDBranchNum,
                                                     this->CDCompNum);
            }
        }

        // Determine available cooling capacity using the setpoint temperature
        lAvailableCoolingCapacity = lNomCoolingCap * CurveManager::CurveValue(state, lCoolCapFTCurve, ChillSupplySetPointTemp, calcCondTemp);

        // Calculate current load for cooling
        MyLoad = sign(max(std::abs(MyLoad), lAvailableCoolingCapacity * lMinPartLoadRat), MyLoad);
        MyLoad = sign(min(std::abs(MyLoad), lAvailableCoolingCapacity * lMaxPartLoadRat), MyLoad);

        // Determine the following variables depending on if the flow has been set in
        // the nodes (flowlock=1 to 2) or if the amount of load is still be determined (flowlock=0)
        //    chilled water flow,
        //    cooling load taken by the chiller, and
        //    supply temperature
        lChillWaterMassflowratemax = this->DesEvapMassFlowRate;

        LoopNum = this->CWLoopNum;
        LoopSideNum = this->CWLoopSideNum;
        {
            auto const SELECT_CASE_var(state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).FlowLock);
            if (SELECT_CASE_var == DataPlant::iFlowLock::Unlocked) { // mass flow rates may be changed by loop components
                this->PossibleSubcooling = false;
                lCoolingLoad = std::abs(MyLoad);
                if (ChillDeltaTemp != 0.0) {
                    lChillWaterMassFlowRate = std::abs(lCoolingLoad / (Cp_CW * ChillDeltaTemp));
                    if (lChillWaterMassFlowRate - lChillWaterMassflowratemax > DataBranchAirLoopPlant::MassFlowTolerance)
                        this->PossibleSubcooling = true;

                    PlantUtilities::SetComponentFlowRate(state,
                                                         lChillWaterMassFlowRate,
                                                         this->ChillReturnNodeNum,
                                                         this->ChillSupplyNodeNum,
                                                         this->CWLoopNum,
                                                         this->CWLoopSideNum,
                                                         this->CWBranchNum,
                                                         this->CWCompNum);
                    // Commenting this could cause diffs - lChillSupplyTemp = ChillSupplySetPointTemp;
                } else {
                    lChillWaterMassFlowRate = 0.0;
                    ShowRecurringWarningErrorAtEnd(state,
                                                   "GasAbsorberChillerModel:Cooling\"" + this->Name + "\", DeltaTemp = 0 in mass flow calculation",
                                                   this->DeltaTempCoolErrCount);
                }
                lChillSupplyTemp = ChillSupplySetPointTemp;
            } else if (SELECT_CASE_var == DataPlant::iFlowLock::Locked) { // mass flow rates may not be changed by loop components
                lChillWaterMassFlowRate = state.dataLoopNodes->Node(lChillReturnNodeNum).MassFlowRate;
                if (this->PossibleSubcooling) {
                    lCoolingLoad = std::abs(MyLoad);

                    ChillDeltaTemp = lCoolingLoad / lChillWaterMassFlowRate / Cp_CW;
                    lChillSupplyTemp = state.dataLoopNodes->Node(lChillReturnNodeNum).Temp - ChillDeltaTemp;
                } else {
                    ChillDeltaTemp = state.dataLoopNodes->Node(lChillReturnNodeNum).Temp - ChillSupplySetPointTemp;
                    lCoolingLoad = std::abs(lChillWaterMassFlowRate * Cp_CW * ChillDeltaTemp);
                    lChillSupplyTemp = ChillSupplySetPointTemp;
                }
                // Check that the Chiller Supply outlet temp honors both plant loop temp low limit and also the chiller low limit
                if (lChillSupplyTemp < lCHWLowLimitTemp) {
                    if ((state.dataLoopNodes->Node(lChillReturnNodeNum).Temp - lCHWLowLimitTemp) > DataPlant::DeltaTempTol) {
                        lChillSupplyTemp = lCHWLowLimitTemp;
                        ChillDeltaTemp = state.dataLoopNodes->Node(lChillReturnNodeNum).Temp - lChillSupplyTemp;
                        lCoolingLoad = lChillWaterMassFlowRate * Cp_CW * ChillDeltaTemp;
                    } else {
                        lChillSupplyTemp = state.dataLoopNodes->Node(lChillReturnNodeNum).Temp;
                        ChillDeltaTemp = state.dataLoopNodes->Node(lChillReturnNodeNum).Temp - lChillSupplyTemp;
                        lCoolingLoad = lChillWaterMassFlowRate * Cp_CW * ChillDeltaTemp;
                    }
                }
                if (lChillSupplyTemp < state.dataLoopNodes->Node(lChillSupplyNodeNum).TempMin) {
                    if ((state.dataLoopNodes->Node(lChillReturnNodeNum).Temp - state.dataLoopNodes->Node(lChillSupplyNodeNum).TempMin) >
                        DataPlant::DeltaTempTol) {
                        lChillSupplyTemp = state.dataLoopNodes->Node(lChillSupplyNodeNum).TempMin;
                        ChillDeltaTemp = state.dataLoopNodes->Node(lChillReturnNodeNum).Temp - lChillSupplyTemp;
                        lCoolingLoad = lChillWaterMassFlowRate * Cp_CW * ChillDeltaTemp;
                    } else {
                        lChillSupplyTemp = state.dataLoopNodes->Node(lChillReturnNodeNum).Temp;
                        ChillDeltaTemp = state.dataLoopNodes->Node(lChillReturnNodeNum).Temp - lChillSupplyTemp;
                        lCoolingLoad = lChillWaterMassFlowRate * Cp_CW * ChillDeltaTemp;
                    }
                }

                // Checks Coolingload on the basis of the machine limits.
                if (lCoolingLoad > std::abs(MyLoad)) {
                    if (lChillWaterMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance) {
                        lCoolingLoad = std::abs(MyLoad);
                        ChillDeltaTemp = lCoolingLoad / lChillWaterMassFlowRate / Cp_CW;
                        lChillSupplyTemp = state.dataLoopNodes->Node(lChillReturnNodeNum).Temp - ChillDeltaTemp;
                    } else {
                        lChillSupplyTemp = state.dataLoopNodes->Node(lChillReturnNodeNum).Temp;
                        ChillDeltaTemp = state.dataLoopNodes->Node(lChillReturnNodeNum).Temp - lChillSupplyTemp;
                        lCoolingLoad = lChillWaterMassFlowRate * Cp_CW * ChillDeltaTemp;
                    }
                }
            }
        }

        // Calculate operating part load ratio for cooling
        PartLoadRat = min(std::abs(MyLoad) / lAvailableCoolingCapacity, lMaxPartLoadRat);
        PartLoadRat = max(lMinPartLoadRat, PartLoadRat);

        if (lAvailableCoolingCapacity > 0.0) {
            if (std::abs(MyLoad) / lAvailableCoolingCapacity < lMinPartLoadRat) {
                lCoolPartLoadRatio = MyLoad / lAvailableCoolingCapacity;
            } else {
                lCoolPartLoadRatio = PartLoadRat;
            }
        } else { // Else if AvailableCoolingCapacity < 0.0
            lCoolPartLoadRatio = 0.0;
        }

        // calculate the fraction of the time period that the chiller would be running
        // use maximum from heating and cooling sides
        if (lCoolPartLoadRatio < lMinPartLoadRat || lHeatPartLoadRatio < lMinPartLoadRat) {
            lFractionOfPeriodRunning = min(1.0, max(lHeatPartLoadRatio, lCoolPartLoadRatio) / lMinPartLoadRat);
        } else {
            lFractionOfPeriodRunning = 1.0;
        }

        // Calculate fuel consumption for cooling
        // fuel used for cooling availCap * HIR * HIR-FT * HIR-FPLR
        lCoolFuelUseRate = lAvailableCoolingCapacity * lFuelCoolRatio *
                           CurveManager::CurveValue(state, lFuelCoolFTCurve, lChillSupplyTemp, calcCondTemp) *
                           CurveManager::CurveValue(state, lFuelCoolFPLRCurve, lCoolPartLoadRatio) * lFractionOfPeriodRunning;

        // Calculate electric parasitics used
        // based on nominal capacity, not available capacity,
        // electric used for cooling nomCap * %OP * EIR * EIR-FT * EIR-FPLR
        lCoolElectricPower = lNomCoolingCap * lElecCoolRatio * lFractionOfPeriodRunning *
                             CurveManager::CurveValue(state, lElecCoolFTCurve, lChillSupplyTemp, calcCondTemp) *
                             CurveManager::CurveValue(state, lElecCoolFPLRCurve, lCoolPartLoadRatio);

        // determine conderser load which is cooling load plus the
        // fuel used for cooling times the burner efficiency plus
        // the electricity used
        lTowerLoad = lCoolingLoad + lCoolFuelUseRate / lFuelHeatRatio + lCoolElectricPower;

        // for water cooled condenser make sure enough flow rate
        // for air cooled condenser just set supply to return temperature
        if (lIsWaterCooled) {
            if (lCondWaterMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance) {
                lCondSupplyTemp = lCondReturnTemp + lTowerLoad / (lCondWaterMassFlowRate * Cp_CD);
            } else {
                ShowSevereError(state, "CalcGasAbsorberChillerModel: Condenser flow = 0, for Gas Absorber Chiller=" + this->Name);
                ShowContinueErrorTimeStamp(state, "");
                // ShowFatalError(state, "Program Terminates due to previous error condition.");
            }
        } else {
            lCondSupplyTemp = lCondReturnTemp; // if air cooled condenser just set supply and return to same temperature
        }

        // save the condenser water supply temperature for next iteration if that is used in lookup
        // and if capacity is large enough error than report problem
        this->oldCondSupplyTemp = lCondSupplyTemp;
        if (!lIsEnterCondensTemp) {
            // calculate the fraction of the estimated error between the capacity based on the previous
            // iteration's value of condenser supply temperature and the actual calculated condenser supply
            // temperature.  If this becomes too common then may need to iterate a solution instead of
            // relying on previous iteration method.
            revisedEstimateAvailCap = lNomCoolingCap * CurveManager::CurveValue(state, lCoolCapFTCurve, ChillSupplySetPointTemp, lCondSupplyTemp);
            if (revisedEstimateAvailCap > 0.0) {
                errorAvailCap = std::abs((revisedEstimateAvailCap - lAvailableCoolingCapacity) / revisedEstimateAvailCap);
                if (errorAvailCap > 0.05) { // if more than 5% error in estimate
                    ShowRecurringWarningErrorAtEnd(state,
                                                   "GasAbsorberChillerModel:\"" + this->Name + "\", poor Condenser Supply Estimate",
                                                   this->CondErrCount,
                                                   errorAvailCap,
                                                   errorAvailCap);
                }
            }
        }
    } // IF(MyLoad>=0 .OR. .NOT. RunFlag)
    // Write into the Report Variables except for nodes
    this->CoolingLoad = lCoolingLoad;
    this->TowerLoad = lTowerLoad;
    this->CoolFuelUseRate = lCoolFuelUseRate;
    this->CoolElectricPower = lCoolElectricPower;
    this->CondReturnTemp = lCondReturnTemp;
    this->ChillReturnTemp = lChillReturnTemp;
    this->CondSupplyTemp = lCondSupplyTemp;
    this->ChillSupplyTemp = lChillSupplyTemp;
    this->ChillWaterFlowRate = lChillWaterMassFlowRate;
    this->CondWaterFlowRate = lCondWaterMassFlowRate;
    this->CoolPartLoadRatio = lCoolPartLoadRatio;
    this->CoolingCapacity = lAvailableCoolingCapacity;
    this->FractionOfPeriodRunning = lFractionOfPeriodRunning;

    // write the combined heating and cooling fuel used and electric used
    this->FuelUseRate = lCoolFuelUseRate + lHeatFuelUseRate;
    this->ElectricPower = lCoolElectricPower + lHeatElectricPower;
}

void GasAbsorberSpecs::calculateHeater(EnergyPlusData &state, Real64 &MyLoad, bool const RunFlag)
{
    //       AUTHOR         Jason Glazer and Michael J. Witte
    //       DATE WRITTEN   March 2001
    // Simulate a direct fired (gas consuming) absorption chiller using
    // curves and inputs similar to DOE-2.1e

    // METHODOLOGY EMPLOYED:
    // Curve fit of performance data

    // REFERENCES:
    // 1.  DOE-2.1e Supplement and source code
    // 2.  CoolTools GasMod work

    // FlowLock = 0  if mass flow rates may be changed by loop components
    // FlowLock = 1  if mass flow rates may not be changed by loop components
    // FlowLock = 2  if overloaded and mass flow rates has changed to a small amount and Tout drops
    //                 below Setpoint

    // SUBROUTINE PARAMETER DEFINITIONS:
    std::string const RoutineName("CalcGasAbsorberHeaterModel");

    // Local copies of GasAbsorberSpecs Type
    // all variables that are local copies of data structure
    // variables are prefaced with an "l" for local.
    Real64 lNomCoolingCap;    // W - design nominal capacity of Absorber
    Real64 lNomHeatCoolRatio; // ratio of heating to cooling capacity
    Real64 lFuelHeatRatio;    // ratio of fuel input to heating output
    Real64 lElecHeatRatio;    // ratio of electricity input to heating output
    int lHeatReturnNodeNum;   // absorber steam inlet node number, water side
    int lHeatSupplyNodeNum;   // absorber steam outlet node number, water side
    Real64 lMinPartLoadRat;   // min allowed operating frac full load
    Real64 lMaxPartLoadRat;   // max allowed operating frac full load
    int lHeatCapFCoolCurve;   // Heating Capacity Function of Cooling Capacity Curve
    int lFuelHeatFHPLRCurve;  // Fuel Input to heat output ratio during heating only function
    // Local copies of GasAbsorberReportVars Type
    Real64 lHeatingLoad(0.0);              // heating load on the chiller
    Real64 lCoolFuelUseRate(0.0);          // instantaneous use of gas for period for cooling
    Real64 lHeatFuelUseRate(0.0);          // instantaneous use of gas for period for heating
    Real64 lCoolElectricPower(0.0);        // parasitic electric power used  for cooling
    Real64 lHeatElectricPower(0.0);        // parasitic electric power used  for heating
    Real64 lHotWaterReturnTemp(0.0);       // reporting: hot water return (inlet) temperature
    Real64 lHotWaterSupplyTemp(0.0);       // reporting: hot water supply (outlet) temperature
    Real64 lHotWaterMassFlowRate(0.0);     // reporting: hot water mass flow rate
    Real64 lCoolPartLoadRatio(0.0);        // operating part load ratio (load/capacity for cooling)
    Real64 lHeatPartLoadRatio(0.0);        // operating part load ratio (load/capacity for heating)
    Real64 lAvailableHeatingCapacity(0.0); // current heating capacity
    Real64 lFractionOfPeriodRunning(0.0);
    // other local variables
    Real64 HeatDeltaTemp(0.0); // hot water temperature difference
    Real64 HeatSupplySetPointTemp(0.0);
    int LoopNum;
    int LoopSideNum;
    Real64 Cp_HW; // local fluid specific heat for hot water

    // set node values to data structure values for nodes

    lHeatReturnNodeNum = this->HeatReturnNodeNum;
    lHeatSupplyNodeNum = this->HeatSupplyNodeNum;

    // set local copies of data from rest of input structure

    lNomCoolingCap = this->NomCoolingCap;
    lNomHeatCoolRatio = this->NomHeatCoolRatio;
    lFuelHeatRatio = this->FuelHeatRatio;
    lElecHeatRatio = this->ElecHeatRatio;
    lMinPartLoadRat = this->MinPartLoadRat;
    lMaxPartLoadRat = this->MaxPartLoadRat;
    lHeatCapFCoolCurve = this->HeatCapFCoolCurve;
    lFuelHeatFHPLRCurve = this->FuelHeatFHPLRCurve;
    LoopNum = this->HWLoopNum;
    LoopSideNum = this->HWLoopSideNum;

    Cp_HW = FluidProperties::GetSpecificHeatGlycol(
        state, state.dataPlnt->PlantLoop(LoopNum).FluidName, lHotWaterReturnTemp, state.dataPlnt->PlantLoop(LoopNum).FluidIndex, RoutineName);

    lCoolElectricPower = this->CoolElectricPower;
    lCoolFuelUseRate = this->CoolFuelUseRate;
    lCoolPartLoadRatio = this->CoolPartLoadRatio;

    // initialize entering conditions
    lHotWaterReturnTemp = state.dataLoopNodes->Node(lHeatReturnNodeNum).Temp;
    lHotWaterMassFlowRate = state.dataLoopNodes->Node(lHeatReturnNodeNum).MassFlowRate;
    {
        auto const SELECT_CASE_var(state.dataPlnt->PlantLoop(LoopNum).LoopDemandCalcScheme);
        if (SELECT_CASE_var == DataPlant::iLoopDemandCalcScheme::SingleSetPoint) {
            HeatSupplySetPointTemp = state.dataLoopNodes->Node(lHeatSupplyNodeNum).TempSetPoint;
        } else if (SELECT_CASE_var == DataPlant::iLoopDemandCalcScheme::DualSetPointDeadBand) {
            HeatSupplySetPointTemp = state.dataLoopNodes->Node(lHeatSupplyNodeNum).TempSetPointLo;
        } else {
            assert(false);
        }
    }
    HeatDeltaTemp = std::abs(lHotWaterReturnTemp - HeatSupplySetPointTemp);

    // If no loop demand or Absorber OFF, return
    // will need to modify when absorber can act as a boiler
    if (MyLoad <= 0 || !RunFlag) {
        // set node temperatures
        lHotWaterSupplyTemp = lHotWaterReturnTemp;
        // Commenting this could cause diffs - HeatDeltaTemp = 0.0;
        lFractionOfPeriodRunning = min(1.0, max(lHeatPartLoadRatio, lCoolPartLoadRatio) / lMinPartLoadRat);
    } else {

        // Determine available heating capacity using the current cooling load
        lAvailableHeatingCapacity = this->NomHeatCoolRatio * this->NomCoolingCap *
                                    CurveManager::CurveValue(state, lHeatCapFCoolCurve, (this->CoolingLoad / this->NomCoolingCap));

        // Calculate current load for heating
        MyLoad = sign(max(std::abs(MyLoad), this->HeatingCapacity * lMinPartLoadRat), MyLoad);
        MyLoad = sign(min(std::abs(MyLoad), this->HeatingCapacity * lMaxPartLoadRat), MyLoad);

        // Determine the following variables depending on if the flow has been set in
        // the nodes (flowlock=1 to 2) or if the amount of load is still be determined (flowlock=0)
        //    chilled water flow,
        //    cooling load taken by the chiller, and
        //    supply temperature
        {
            auto const SELECT_CASE_var(state.dataPlnt->PlantLoop(LoopNum).LoopSide(LoopSideNum).FlowLock);
            if (SELECT_CASE_var == DataPlant::iFlowLock::Unlocked) { // mass flow rates may be changed by loop components
                lHeatingLoad = std::abs(MyLoad);
                if (HeatDeltaTemp != 0) {
                    lHotWaterMassFlowRate = std::abs(lHeatingLoad / (Cp_HW * HeatDeltaTemp));

                    PlantUtilities::SetComponentFlowRate(state,
                                                         lHotWaterMassFlowRate,
                                                         this->HeatReturnNodeNum,
                                                         this->HeatSupplyNodeNum,
                                                         this->HWLoopNum,
                                                         this->HWLoopSideNum,
                                                         this->HWBranchNum,
                                                         this->HWCompNum);

                } else {
                    lHotWaterMassFlowRate = 0.0;
                    ShowRecurringWarningErrorAtEnd(state,
                                                   "GasAbsorberChillerModel:Heating\"" + this->Name + "\", DeltaTemp = 0 in mass flow calculation",
                                                   this->DeltaTempHeatErrCount);
                }
                lHotWaterSupplyTemp = HeatSupplySetPointTemp;
            } else if (SELECT_CASE_var == DataPlant::iFlowLock::Locked) { // mass flow rates may not be changed by loop components
                lHotWaterSupplyTemp = HeatSupplySetPointTemp;
                lHeatingLoad = std::abs(lHotWaterMassFlowRate * Cp_HW * HeatDeltaTemp);
            }
        }

        // Calculate operating part load ratio for cooling
        if (lAvailableHeatingCapacity <= 0.0) {
            lAvailableHeatingCapacity = 0.0;
            lHeatPartLoadRatio = 0.0;
        } else {
            lHeatPartLoadRatio = lHeatingLoad / lAvailableHeatingCapacity;
        }

        // Calculate fuel consumption for cooling
        // fuel used for cooling availCap * HIR * HIR-FT * HIR-FPLR

        lHeatFuelUseRate = lAvailableHeatingCapacity * lFuelHeatRatio * CurveManager::CurveValue(state, lFuelHeatFHPLRCurve, lHeatPartLoadRatio);

        // calculate the fraction of the time period that the chiller would be running
        // use maximum from heating and cooling sides
        lFractionOfPeriodRunning = min(1.0, max(lHeatPartLoadRatio, lCoolPartLoadRatio) / lMinPartLoadRat);

        // Calculate electric parasitics used
        // for heating based on nominal capacity not available capacity
        lHeatElectricPower = lNomCoolingCap * lNomHeatCoolRatio * lElecHeatRatio * lFractionOfPeriodRunning;
        // Coodinate electric parasitics for heating and cooling to avoid double counting
        // Total electric is the max of heating electric or cooling electric
        // If heating electric is greater, leave cooling electric and subtract if off of heating elec
        // If cooling electric is greater, set heating electric to zero
        if (lHeatElectricPower <= lCoolElectricPower) {
            lHeatElectricPower = 0.0;
        } else {
            lHeatElectricPower -= lCoolElectricPower;
        }

    } // IF(MyLoad==0 .OR. .NOT. RunFlag)
    // Write into the Report Variables except for nodes
    this->HeatingLoad = lHeatingLoad;
    this->HeatFuelUseRate = lHeatFuelUseRate;
    this->HeatElectricPower = lHeatElectricPower;
    this->HotWaterReturnTemp = lHotWaterReturnTemp;
    this->HotWaterSupplyTemp = lHotWaterSupplyTemp;
    this->HotWaterFlowRate = lHotWaterMassFlowRate;
    this->HeatPartLoadRatio = lHeatPartLoadRatio;
    this->HeatingCapacity = lAvailableHeatingCapacity;
    this->FractionOfPeriodRunning = lFractionOfPeriodRunning;

    // write the combined heating and cooling fuel used and electric used
    this->FuelUseRate = lCoolFuelUseRate + lHeatFuelUseRate;
    this->ElectricPower = lCoolElectricPower + lHeatElectricPower;
}

void GasAbsorberSpecs::updateCoolRecords(EnergyPlusData &state,
                                         Real64 const MyLoad, // current load
                                         bool const RunFlag   // TRUE if Absorber operating
)
{
    //       AUTHOR         Jason Glazer
    //       DATE WRITTEN   March 2001

    int lChillReturnNodeNum = this->ChillReturnNodeNum;
    int lChillSupplyNodeNum = this->ChillSupplyNodeNum;
    int lCondReturnNodeNum = this->CondReturnNodeNum;
    int lCondSupplyNodeNum = this->CondSupplyNodeNum;

    if (MyLoad == 0 || !RunFlag) {
        state.dataLoopNodes->Node(lChillSupplyNodeNum).Temp = state.dataLoopNodes->Node(lChillReturnNodeNum).Temp;
        if (this->isWaterCooled) {
            state.dataLoopNodes->Node(lCondSupplyNodeNum).Temp = state.dataLoopNodes->Node(lCondReturnNodeNum).Temp;
        }
    } else {
        state.dataLoopNodes->Node(lChillSupplyNodeNum).Temp = this->ChillSupplyTemp;
        if (this->isWaterCooled) {
            state.dataLoopNodes->Node(lCondSupplyNodeNum).Temp = this->CondSupplyTemp;
        }
    }

    // convert power to energy and instantaneous use to use over the time step
    this->CoolingEnergy = this->CoolingLoad * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
    this->TowerEnergy = this->TowerLoad * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
    this->FuelEnergy = this->FuelUseRate * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
    this->CoolFuelEnergy = this->CoolFuelUseRate * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
    this->ElectricEnergy = this->ElectricPower * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
    this->CoolElectricEnergy = this->CoolElectricPower * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
    if (this->CoolFuelUseRate != 0.0) {
        this->FuelCOP = this->CoolingLoad / this->CoolFuelUseRate;
    } else {
        this->FuelCOP = 0.0;
    }
}

void GasAbsorberSpecs::updateHeatRecords(EnergyPlusData &state,
                                         Real64 const MyLoad, // current load
                                         bool const RunFlag   // TRUE if Absorber operating
)
{
    //       AUTHOR         Jason Glazer
    //       DATE WRITTEN   March 2001

    int lHeatReturnNodeNum = this->HeatReturnNodeNum;
    int lHeatSupplyNodeNum = this->HeatSupplyNodeNum;

    if (MyLoad == 0 || !RunFlag) {
        state.dataLoopNodes->Node(lHeatSupplyNodeNum).Temp = state.dataLoopNodes->Node(lHeatReturnNodeNum).Temp;
    } else {
        state.dataLoopNodes->Node(lHeatSupplyNodeNum).Temp = this->HotWaterSupplyTemp;
    }

    // convert power to energy and instantaneous use to use over the time step
    this->HeatingEnergy = this->HeatingLoad * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
    this->FuelEnergy = this->FuelUseRate * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
    this->HeatFuelEnergy = this->HeatFuelUseRate * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
    this->ElectricEnergy = this->ElectricPower * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
    this->HeatElectricEnergy = this->HeatElectricPower * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
}

} // namespace EnergyPlus::ChillerGasAbsorption
