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
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <EnergyPlus/Autosizing/Base.hh>
#include <EnergyPlus/BranchNodeConnections.hh>
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
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutAirNodeManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/Plant/PlantLocation.hh>
#include <EnergyPlus/PlantChillers.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

namespace PlantChillers {

    // MODULE INFORMATION:
    //       AUTHOR         Dan Fisher / Brandon Anderson
    //       DATE WRITTEN   September 2000
    //       MODIFIED       Richard Liesen Nov-Dec 2001; Jan 2002
    //                      Chandan Sharma, FSEC, February 2010, Added basin heater
    //       RE-ENGINEERED  Edwin: Merged Four Chiller Modules Into One

    // PURPOSE OF THIS MODULE:
    // This module simulates the performance of the Electric vapor
    // compression Chillers, Gas Turbine Chillers, Engine Drivent chillers, and
    // Constant COP chillers

    // METHODOLOGY EMPLOYED:
    // Called by plantloopequipment, model accepts inputs, and calculates a
    // thermal response using new plant routines such as SetComponentFlowRate

    // REFERENCES:
    // 1. BLAST Users Manual

    // Parameters for use in Chillers
    Real64 constexpr KJtoJ(1000.0); // convert Kjoules to joules

    void BaseChillerSpecs::getDesignCapacities(
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

    void BaseChillerSpecs::getSizingFactor(Real64 &sizFac)
    {
        sizFac = this->SizFac;
    }

    void BaseChillerSpecs::onInitLoopEquip(EnergyPlusData &state, const PlantLocation &calledFromLocation)
    {
        this->initialize(state, false, 0.0);
        if (calledFromLocation.loopNum == this->CWLoopNum) {
            this->size(state);
        }
    }

    void BaseChillerSpecs::getDesignTemperatures(Real64 &tempDesCondIn, Real64 &tempDesEvapOut)
    {
        tempDesEvapOut = this->TempDesEvapOut;
        tempDesCondIn = this->TempDesCondIn;
    }

    ElectricChillerSpecs *ElectricChillerSpecs::factory(EnergyPlusData &state, std::string const &chillerName)
    {
        if (state.dataPlantChillers->GetElectricInput) {
            ElectricChillerSpecs::getInput(state);
            state.dataPlantChillers->GetElectricInput = false;
        }
        for (auto &thisChiller : state.dataPlantChillers->ElectricChiller) {
            if (UtilityRoutines::MakeUPPERCase(thisChiller.Name) == chillerName) {
                return &thisChiller;
            }
        }
        ShowFatalError(state, "Could not locate electric chiller with name: " + chillerName);
        return nullptr;
    }

    void ElectricChillerSpecs::getInput(EnergyPlusData &state)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Dan Fisher / Brandon Anderson
        //       DATE WRITTEN:    September 2000

        // PURPOSE OF THIS SUBROUTINE:
        // This routine will get the input
        // required by the Electric Chiller model.

        static constexpr std::string_view RoutineName("GetElectricChillerInput: "); // include trailing blank space

        int NumAlphas; // Number of elements in the alpha array
        int NumNums;   // Number of elements in the numeric array
        int IOStat;    // IO Status when calling get input subroutine
        bool ErrorsFound(false);

        state.dataIPShortCut->cCurrentModuleObject = "Chiller:Electric";
        state.dataPlantChillers->NumElectricChillers =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, state.dataIPShortCut->cCurrentModuleObject);

        if (state.dataPlantChillers->NumElectricChillers <= 0) {
            ShowSevereError(state, "No " + state.dataIPShortCut->cCurrentModuleObject + " Equipment specified in input file");
            ErrorsFound = true;
        }

        // See if load distribution manager has already gotten the input
        if (allocated(state.dataPlantChillers->ElectricChiller)) return;

        // ALLOCATE ARRAYS
        state.dataPlantChillers->ElectricChiller.allocate(state.dataPlantChillers->NumElectricChillers);

        // LOAD ARRAYS WITH Electric CURVE FIT CHILLER DATA
        for (int ChillerNum = 1; ChillerNum <= state.dataPlantChillers->NumElectricChillers; ++ChillerNum) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     state.dataIPShortCut->cCurrentModuleObject,
                                                                     ChillerNum,
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

            auto &thisChiller = state.dataPlantChillers->ElectricChiller(ChillerNum);
            thisChiller.Name = state.dataIPShortCut->cAlphaArgs(1);
            thisChiller.plantTypeOfNum = DataPlant::TypeOf_Chiller_Electric;

            if (state.dataIPShortCut->cAlphaArgs(2) == "AIRCOOLED") {
                thisChiller.CondenserType = DataPlant::CondenserType::AirCooled;
            } else if (state.dataIPShortCut->cAlphaArgs(2) == "WATERCOOLED") {
                thisChiller.CondenserType = DataPlant::CondenserType::WaterCooled;
            } else if (state.dataIPShortCut->cAlphaArgs(2) == "EVAPORATIVELYCOOLED") {
                thisChiller.CondenserType = DataPlant::CondenserType::EvapCooled;
            } else {
                ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(2) + '=' + state.dataIPShortCut->cAlphaArgs(2));
                ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
                ErrorsFound = true;
            }

            thisChiller.NomCap = state.dataIPShortCut->rNumericArgs(1);
            if (thisChiller.NomCap == DataSizing::AutoSize) {
                thisChiller.NomCapWasAutoSized = true;
            }
            if (state.dataIPShortCut->rNumericArgs(1) == 0.0) {
                ShowSevereError(state,
                                format("Invalid {}={:.2R}", state.dataIPShortCut->cNumericFieldNames(1), state.dataIPShortCut->rNumericArgs(1)));
                ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
                ErrorsFound = true;
            }
            thisChiller.COP = state.dataIPShortCut->rNumericArgs(2);
            if (state.dataIPShortCut->rNumericArgs(2) == 0.0) {
                ShowSevereError(state,
                                format("Invalid {}={:.3R}", state.dataIPShortCut->cNumericFieldNames(2), state.dataIPShortCut->rNumericArgs(2)));
                ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
                ErrorsFound = true;
            }
            thisChiller.EvapInletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                                               state.dataIPShortCut->cAlphaArgs(3),
                                                                               ErrorsFound,
                                                                               state.dataIPShortCut->cCurrentModuleObject,
                                                                               state.dataIPShortCut->cAlphaArgs(1),
                                                                               DataLoopNode::NodeFluidType::Water,
                                                                               DataLoopNode::NodeConnectionType::Inlet,
                                                                               NodeInputManager::compFluidStream::Primary,
                                                                               DataLoopNode::ObjectIsNotParent);
            thisChiller.EvapOutletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                                                state.dataIPShortCut->cAlphaArgs(4),
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
                                               state.dataIPShortCut->cAlphaArgs(3),
                                               state.dataIPShortCut->cAlphaArgs(4),
                                               "Chilled Water Nodes");

            if (thisChiller.CondenserType == DataPlant::CondenserType::AirCooled ||
                thisChiller.CondenserType == DataPlant::CondenserType::EvapCooled) {
                // Connection not required for air or evap cooled condenser
                // If the condenser inlet is blank for air cooled and evap cooled condensers then supply a generic name
                //  since it is not used elsewhere for connection
                // for transition purposes, add this node if not there.
                if (state.dataIPShortCut->lAlphaFieldBlanks(5)) {
                    if (len(state.dataIPShortCut->cAlphaArgs(1)) <
                        DataGlobalConstants::MaxNameLength - 21) { // protect against long name leading to > 100 chars
                        state.dataIPShortCut->cAlphaArgs(5) = state.dataIPShortCut->cAlphaArgs(1) + " CONDENSER INLET NODE";
                    } else {
                        state.dataIPShortCut->cAlphaArgs(5) = state.dataIPShortCut->cAlphaArgs(1).substr(0, 79) + " CONDENSER INLET NODE";
                    }
                }
                if (state.dataIPShortCut->lAlphaFieldBlanks(6)) {
                    if (len(state.dataIPShortCut->cAlphaArgs(1)) <
                        DataGlobalConstants::MaxNameLength - 22) { // protect against long name leading to > 100 chars
                        state.dataIPShortCut->cAlphaArgs(6) = state.dataIPShortCut->cAlphaArgs(1) + " CONDENSER OUTLET NODE";
                    } else {
                        state.dataIPShortCut->cAlphaArgs(6) = state.dataIPShortCut->cAlphaArgs(1).substr(0, 78) + " CONDENSER OUTLET NODE";
                    }
                }

                thisChiller.CondInletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                                                   state.dataIPShortCut->cAlphaArgs(5),
                                                                                   ErrorsFound,
                                                                                   state.dataIPShortCut->cCurrentModuleObject,
                                                                                   state.dataIPShortCut->cAlphaArgs(1),
                                                                                   DataLoopNode::NodeFluidType::Air,
                                                                                   DataLoopNode::NodeConnectionType::OutsideAirReference,
                                                                                   NodeInputManager::compFluidStream::Secondary,
                                                                                   DataLoopNode::ObjectIsNotParent);
                bool Okay;
                OutAirNodeManager::CheckAndAddAirNodeNumber(state, thisChiller.CondInletNodeNum, Okay);
                if (!Okay) {
                    ShowWarningError(state,
                                     state.dataIPShortCut->cCurrentModuleObject + ", Adding OutdoorAir:Node=" + state.dataIPShortCut->cAlphaArgs(5));
                }

                thisChiller.CondOutletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                                                    state.dataIPShortCut->cAlphaArgs(6),
                                                                                    ErrorsFound,
                                                                                    state.dataIPShortCut->cCurrentModuleObject,
                                                                                    state.dataIPShortCut->cAlphaArgs(1),
                                                                                    DataLoopNode::NodeFluidType::Air,
                                                                                    DataLoopNode::NodeConnectionType::Outlet,
                                                                                    NodeInputManager::compFluidStream::Secondary,
                                                                                    DataLoopNode::ObjectIsNotParent);
            } else if (thisChiller.CondenserType == DataPlant::CondenserType::WaterCooled) {
                thisChiller.CondInletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                                                   state.dataIPShortCut->cAlphaArgs(5),
                                                                                   ErrorsFound,
                                                                                   state.dataIPShortCut->cCurrentModuleObject,
                                                                                   state.dataIPShortCut->cAlphaArgs(1),
                                                                                   DataLoopNode::NodeFluidType::Water,
                                                                                   DataLoopNode::NodeConnectionType::Inlet,
                                                                                   NodeInputManager::compFluidStream::Secondary,
                                                                                   DataLoopNode::ObjectIsNotParent);
                thisChiller.CondOutletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                                                    state.dataIPShortCut->cAlphaArgs(6),
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
                                                   state.dataIPShortCut->cAlphaArgs(5),
                                                   state.dataIPShortCut->cAlphaArgs(6),
                                                   "Condenser Water Nodes");
                // Condenser Inlet node name is necessary for Water Cooled
                if (state.dataIPShortCut->lAlphaFieldBlanks(5)) {
                    ShowSevereError(state, "Invalid, " + state.dataIPShortCut->cAlphaFieldNames(5) + "is blank ");
                    ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
                    ErrorsFound = true;
                } else if (state.dataIPShortCut->lAlphaFieldBlanks(6)) {
                    ShowSevereError(state, "Invalid, " + state.dataIPShortCut->cAlphaFieldNames(6) + "is blank ");
                    ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
                    ErrorsFound = true;
                }
            } else {
                thisChiller.CondInletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                                                   state.dataIPShortCut->cAlphaArgs(5),
                                                                                   ErrorsFound,
                                                                                   state.dataIPShortCut->cCurrentModuleObject,
                                                                                   state.dataIPShortCut->cAlphaArgs(1),
                                                                                   DataLoopNode::NodeFluidType::blank,
                                                                                   DataLoopNode::NodeConnectionType::Inlet,
                                                                                   NodeInputManager::compFluidStream::Secondary,
                                                                                   DataLoopNode::ObjectIsNotParent);
                thisChiller.CondOutletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                                                    state.dataIPShortCut->cAlphaArgs(6),
                                                                                    ErrorsFound,
                                                                                    state.dataIPShortCut->cCurrentModuleObject,
                                                                                    state.dataIPShortCut->cAlphaArgs(1),
                                                                                    DataLoopNode::NodeFluidType::blank,
                                                                                    DataLoopNode::NodeConnectionType::Outlet,
                                                                                    NodeInputManager::compFluidStream::Secondary,
                                                                                    DataLoopNode::ObjectIsNotParent);
                BranchNodeConnections::TestCompSet(state,
                                                   state.dataIPShortCut->cCurrentModuleObject,
                                                   state.dataIPShortCut->cAlphaArgs(1),
                                                   state.dataIPShortCut->cAlphaArgs(5),
                                                   state.dataIPShortCut->cAlphaArgs(6),
                                                   "Condenser (unknown?) Nodes");
                // Condenser Inlet node name is necessary
                if (state.dataIPShortCut->lAlphaFieldBlanks(5)) {
                    ShowSevereError(state, "Invalid, " + state.dataIPShortCut->cAlphaFieldNames(5) + "is blank ");
                    ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
                    ErrorsFound = true;
                } else if (state.dataIPShortCut->lAlphaFieldBlanks(6)) {
                    ShowSevereError(state, "Invalid, " + state.dataIPShortCut->cAlphaFieldNames(6) + "is blank ");
                    ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
                    ErrorsFound = true;
                }
            }

            thisChiller.MinPartLoadRat = state.dataIPShortCut->rNumericArgs(3);
            thisChiller.MaxPartLoadRat = state.dataIPShortCut->rNumericArgs(4);
            thisChiller.OptPartLoadRat = state.dataIPShortCut->rNumericArgs(5);
            thisChiller.TempDesCondIn = state.dataIPShortCut->rNumericArgs(6);
            thisChiller.TempRiseCoef = state.dataIPShortCut->rNumericArgs(7);
            thisChiller.TempDesEvapOut = state.dataIPShortCut->rNumericArgs(8);
            thisChiller.EvapVolFlowRate = state.dataIPShortCut->rNumericArgs(9);
            if (thisChiller.EvapVolFlowRate == DataSizing::AutoSize) {
                thisChiller.EvapVolFlowRateWasAutoSized = true;
            }
            thisChiller.CondVolFlowRate = state.dataIPShortCut->rNumericArgs(10);
            if (thisChiller.CondVolFlowRate == DataSizing::AutoSize) {
                if (thisChiller.CondenserType == DataPlant::CondenserType::WaterCooled) {
                    thisChiller.CondVolFlowRateWasAutoSized = true;
                }
            }
            thisChiller.CapRatCoef(1) = state.dataIPShortCut->rNumericArgs(11);
            thisChiller.CapRatCoef(2) = state.dataIPShortCut->rNumericArgs(12);
            thisChiller.CapRatCoef(3) = state.dataIPShortCut->rNumericArgs(13);
            if ((state.dataIPShortCut->rNumericArgs(11) + state.dataIPShortCut->rNumericArgs(12) + state.dataIPShortCut->rNumericArgs(13)) == 0.0) {
                ShowSevereError(state,
                                state.dataIPShortCut->cCurrentModuleObject +
                                    ": Sum of Capacity Ratio Coef = 0.0, chiller=" + state.dataIPShortCut->cAlphaArgs(1));
                ErrorsFound = true;
            }
            thisChiller.PowerRatCoef(1) = state.dataIPShortCut->rNumericArgs(14);
            thisChiller.PowerRatCoef(2) = state.dataIPShortCut->rNumericArgs(15);
            thisChiller.PowerRatCoef(3) = state.dataIPShortCut->rNumericArgs(16);
            thisChiller.FullLoadCoef(1) = state.dataIPShortCut->rNumericArgs(17);
            thisChiller.FullLoadCoef(2) = state.dataIPShortCut->rNumericArgs(18);
            thisChiller.FullLoadCoef(3) = state.dataIPShortCut->rNumericArgs(19);
            thisChiller.TempLowLimitEvapOut = state.dataIPShortCut->rNumericArgs(20);
            thisChiller.SizFac = state.dataIPShortCut->rNumericArgs(22);
            if (thisChiller.SizFac <= 0.0) thisChiller.SizFac = 1.0;

            {
                auto const SELECT_CASE_var(state.dataIPShortCut->cAlphaArgs(7));
                if (SELECT_CASE_var == "CONSTANTFLOW") {
                    thisChiller.FlowMode = DataPlant::FlowMode::Constant;
                } else if (SELECT_CASE_var == "LEAVINGSETPOINTMODULATED") {
                    thisChiller.FlowMode = DataPlant::FlowMode::LeavingSetpointModulated;
                } else if (SELECT_CASE_var == "NOTMODULATED") {
                    thisChiller.FlowMode = DataPlant::FlowMode::NotModulated;
                } else {
                    ShowSevereError(state,
                                    std::string{RoutineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" +
                                        state.dataIPShortCut->cAlphaArgs(1) + "\",");
                    ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(7) + '=' + state.dataIPShortCut->cAlphaArgs(7));
                    ShowContinueError(state, "Available choices are ConstantFlow, NotModulated, or LeavingSetpointModulated");
                    ShowContinueError(state, "Flow mode NotModulated is assumed and the simulation continues.");
                    thisChiller.FlowMode = DataPlant::FlowMode::NotModulated;
                }
            }

            // These are the Heat Recovery Inputs
            thisChiller.DesignHeatRecVolFlowRate = state.dataIPShortCut->rNumericArgs(21);
            if (thisChiller.DesignHeatRecVolFlowRate == DataSizing::AutoSize) {
                thisChiller.DesignHeatRecVolFlowRateWasAutoSized = true;
            }

            if ((thisChiller.DesignHeatRecVolFlowRate > 0.0) || (thisChiller.DesignHeatRecVolFlowRate == DataSizing::AutoSize)) {
                thisChiller.HeatRecActive = true;
                thisChiller.HeatRecInletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                                                      state.dataIPShortCut->cAlphaArgs(8),
                                                                                      ErrorsFound,
                                                                                      state.dataIPShortCut->cCurrentModuleObject,
                                                                                      state.dataIPShortCut->cAlphaArgs(1),
                                                                                      DataLoopNode::NodeFluidType::Water,
                                                                                      DataLoopNode::NodeConnectionType::Inlet,
                                                                                      NodeInputManager::compFluidStream::Tertiary,
                                                                                      DataLoopNode::ObjectIsNotParent);
                if (thisChiller.HeatRecInletNodeNum == 0) {
                    ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(8) + '=' + state.dataIPShortCut->cAlphaArgs(8));
                    ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
                    ErrorsFound = true;
                }
                thisChiller.HeatRecOutletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                                                       state.dataIPShortCut->cAlphaArgs(9),
                                                                                       ErrorsFound,
                                                                                       state.dataIPShortCut->cCurrentModuleObject,
                                                                                       state.dataIPShortCut->cAlphaArgs(1),
                                                                                       DataLoopNode::NodeFluidType::Water,
                                                                                       DataLoopNode::NodeConnectionType::Outlet,
                                                                                       NodeInputManager::compFluidStream::Tertiary,
                                                                                       DataLoopNode::ObjectIsNotParent);
                if (thisChiller.HeatRecOutletNodeNum == 0) {
                    ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(9) + '=' + state.dataIPShortCut->cAlphaArgs(9));
                    ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
                    ErrorsFound = true;
                }

                BranchNodeConnections::TestCompSet(state,
                                                   state.dataIPShortCut->cCurrentModuleObject,
                                                   state.dataIPShortCut->cAlphaArgs(1),
                                                   state.dataIPShortCut->cAlphaArgs(8),
                                                   state.dataIPShortCut->cAlphaArgs(9),
                                                   "Heat Recovery Nodes");
                if (thisChiller.DesignHeatRecVolFlowRate > 0.0) {
                    PlantUtilities::RegisterPlantCompDesignFlow(state, thisChiller.HeatRecInletNodeNum, thisChiller.DesignHeatRecVolFlowRate);
                }
                // Condenser flow rate must be specified for heat reclaim
                if (thisChiller.CondenserType == DataPlant::CondenserType::AirCooled ||
                    thisChiller.CondenserType == DataPlant::CondenserType::EvapCooled) {
                    if (thisChiller.CondVolFlowRate <= 0.0) {
                        ShowSevereError(
                            state, format("Invalid {}={:.6R}", state.dataIPShortCut->cNumericFieldNames(10), state.dataIPShortCut->rNumericArgs(10)));
                        ShowSevereError(state, "Condenser fluid flow rate must be specified for Heat Reclaim applications.");
                        ShowContinueError(state,
                                          "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
                        ErrorsFound = true;
                    }
                }

                if (NumNums > 24) {
                    if (!state.dataIPShortCut->lNumericFieldBlanks(25)) {
                        thisChiller.HeatRecCapacityFraction = state.dataIPShortCut->rNumericArgs(25);
                    } else {
                        thisChiller.HeatRecCapacityFraction = 1.0;
                    }
                } else {
                    thisChiller.HeatRecCapacityFraction = 1.0;
                }

                if (NumAlphas > 10) {
                    if (!state.dataIPShortCut->lAlphaFieldBlanks(11)) {
                        thisChiller.HeatRecInletLimitSchedNum = ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(11));
                        if (thisChiller.HeatRecInletLimitSchedNum == 0) {
                            ShowSevereError(state,
                                            std::string{RoutineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" +
                                                state.dataIPShortCut->cAlphaArgs(1) + "\"");
                            ShowContinueError(state,
                                              "Invalid " + state.dataIPShortCut->cAlphaFieldNames(11) + '=' + state.dataIPShortCut->cAlphaArgs(11));
                            ErrorsFound = true;
                        }
                    } else {
                        thisChiller.HeatRecInletLimitSchedNum = 0;
                    }
                } else {
                    thisChiller.HeatRecInletLimitSchedNum = 0;
                }

                if (NumAlphas > 11) {
                    if (!state.dataIPShortCut->lAlphaFieldBlanks(12)) {
                        thisChiller.HeatRecSetPointNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                                                                 state.dataIPShortCut->cAlphaArgs(12),
                                                                                                 ErrorsFound,
                                                                                                 state.dataIPShortCut->cCurrentModuleObject,
                                                                                                 state.dataIPShortCut->cAlphaArgs(1),
                                                                                                 DataLoopNode::NodeFluidType::Water,
                                                                                                 DataLoopNode::NodeConnectionType::Sensor,
                                                                                                 NodeInputManager::compFluidStream::Primary,
                                                                                                 DataLoopNode::ObjectIsNotParent);
                    } else {
                        thisChiller.HeatRecSetPointNodeNum = 0;
                    }
                } else {
                    thisChiller.HeatRecSetPointNodeNum = 0;
                }

            } else {
                thisChiller.HeatRecActive = false;
                thisChiller.DesignHeatRecMassFlowRate = 0.0;
                thisChiller.HeatRecInletNodeNum = 0;
                thisChiller.HeatRecOutletNodeNum = 0;
                // if heat recovery is not used, don't care about condenser flow rate for air/evap-cooled equip.
                if (thisChiller.CondenserType == DataPlant::CondenserType::AirCooled ||
                    thisChiller.CondenserType == DataPlant::CondenserType::EvapCooled) {
                    thisChiller.CondVolFlowRate = 0.0011; // set to avoid errors in calc routine
                }
                if ((!state.dataIPShortCut->lAlphaFieldBlanks(8)) || (!state.dataIPShortCut->lAlphaFieldBlanks(9))) {
                    ShowWarningError(state,
                                     "Since Design Heat Flow Rate = 0.0, Heat Recovery inactive for " + state.dataIPShortCut->cCurrentModuleObject +
                                         '=' + state.dataIPShortCut->cAlphaArgs(1));
                    ShowContinueError(state, "However, Node names were specified for Heat Recovery inlet or outlet nodes");
                }
            }
            //   Basin heater power as a function of temperature must be greater than or equal to 0
            thisChiller.BasinHeaterPowerFTempDiff = state.dataIPShortCut->rNumericArgs(23);
            if (state.dataIPShortCut->rNumericArgs(23) < 0.0) {
                ShowSevereError(state,
                                state.dataIPShortCut->cCurrentModuleObject + ", \"" + thisChiller.Name +
                                    "\" TRIM(state.dataIPShortCut->cNumericFieldNames(23)) must be >= 0");
                ErrorsFound = true;
            }

            thisChiller.BasinHeaterSetPointTemp = state.dataIPShortCut->rNumericArgs(24);

            if (thisChiller.BasinHeaterPowerFTempDiff > 0.0) {
                if (NumNums < 24) {
                    thisChiller.BasinHeaterSetPointTemp = 2.0;
                }
                if (thisChiller.BasinHeaterSetPointTemp < 2.0) {
                    ShowWarningError(state,
                                     state.dataIPShortCut->cCurrentModuleObject + ":\"" + thisChiller.Name + "\", " +
                                         state.dataIPShortCut->cNumericFieldNames(24) + " is less than 2 deg C. Freezing could occur.");
                }
            }

            if (!state.dataIPShortCut->lAlphaFieldBlanks(10)) {
                thisChiller.BasinHeaterSchedulePtr = ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(10));
                if (thisChiller.BasinHeaterSchedulePtr == 0) {
                    ShowWarningError(state,
                                     state.dataIPShortCut->cCurrentModuleObject + ", \"" + thisChiller.Name +
                                         "\" TRIM(state.dataIPShortCut->cAlphaFieldNames(10)) \"" + state.dataIPShortCut->cAlphaArgs(10) +
                                         "\" was not found. Basin heater operation will not be modeled and the simulation continues");
                }
            }
            if (NumAlphas > 12) {
                thisChiller.EndUseSubcategory = state.dataIPShortCut->cAlphaArgs(13);
            } else {
                thisChiller.EndUseSubcategory = "General";
            }
        }

        if (ErrorsFound) {
            ShowFatalError(state, "Errors found in processing input for " + state.dataIPShortCut->cCurrentModuleObject);
        }
    }

    void ElectricChillerSpecs::setupOutputVariables(EnergyPlusData &state)
    {
        SetupOutputVariable(state,
                            "Chiller Electricity Rate",
                            OutputProcessor::Unit::W,
                            this->Power,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);
        SetupOutputVariable(state,
                            "Chiller Electricity Energy",
                            OutputProcessor::Unit::J,
                            this->Energy,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            this->Name,
                            _,
                            "ELECTRICITY",
                            "Cooling",
                            this->EndUseSubcategory,
                            "Plant");

        SetupOutputVariable(state,
                            "Chiller Evaporator Cooling Rate",
                            OutputProcessor::Unit::W,
                            this->QEvaporator,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);
        SetupOutputVariable(state,
                            "Chiller Evaporator Cooling Energy",
                            OutputProcessor::Unit::J,
                            this->EvaporatorEnergy,
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
                            this->EvapInletTemp,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);
        SetupOutputVariable(state,
                            "Chiller Evaporator Outlet Temperature",
                            OutputProcessor::Unit::C,
                            this->EvapOutletTemp,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);
        SetupOutputVariable(state,
                            "Chiller Evaporator Mass Flow Rate",
                            OutputProcessor::Unit::kg_s,
                            this->EvapMassFlowRate,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);
        SetupOutputVariable(state,
                            "Chiller Condenser Heat Transfer Rate",
                            OutputProcessor::Unit::W,
                            this->QCondenser,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);
        SetupOutputVariable(state,
                            "Chiller Condenser Heat Transfer Energy",
                            OutputProcessor::Unit::J,
                            this->CondenserEnergy,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            this->Name,
                            _,
                            "ENERGYTRANSFER",
                            "HEATREJECTION",
                            _,
                            "Plant");
        SetupOutputVariable(state,
                            "Chiller COP",
                            OutputProcessor::Unit::W_W,
                            this->ActualCOP,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);
        SetupOutputVariable(state,
                            "Chiller Condenser Inlet Temperature",
                            OutputProcessor::Unit::C,
                            this->CondInletTemp,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);

        // Condenser mass flow and outlet temp are valid for water cooled
        if (this->CondenserType == DataPlant::CondenserType::WaterCooled) {
            SetupOutputVariable(state,
                                "Chiller Condenser Outlet Temperature",
                                OutputProcessor::Unit::C,
                                this->CondOutletTemp,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                this->Name);
            SetupOutputVariable(state,
                                "Chiller Condenser Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                this->CondMassFlowRate,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                this->Name);
        } else if (this->CondenserType == DataPlant::CondenserType::AirCooled) {
        } else if (this->CondenserType == DataPlant::CondenserType::EvapCooled) {
            if (this->BasinHeaterPowerFTempDiff > 0.0) {
                SetupOutputVariable(state,
                                    "Chiller Basin Heater Electricity Rate",
                                    OutputProcessor::Unit::W,
                                    this->BasinHeaterPower,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    this->Name);
                SetupOutputVariable(state,
                                    "Chiller Basin Heater Electricity Energy",
                                    OutputProcessor::Unit::J,
                                    this->BasinHeaterConsumption,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    this->Name,
                                    _,
                                    "Electricity",
                                    "CHILLERS",
                                    _,
                                    "Plant");
            }
        }

        // If heat recovery is active then setup report variables
        if (this->HeatRecActive) {
            SetupOutputVariable(state,
                                "Chiller Total Recovered Heat Rate",
                                OutputProcessor::Unit::W,
                                this->QHeatRecovery,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                this->Name);
            SetupOutputVariable(state,
                                "Chiller Total Recovered Heat Energy",
                                OutputProcessor::Unit::J,
                                this->EnergyHeatRecovery,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                this->Name,
                                _,
                                "ENERGYTRANSFER",
                                "HEATRECOVERY",
                                _,
                                "Plant");
            SetupOutputVariable(state,
                                "Chiller Heat Recovery Inlet Temperature",
                                OutputProcessor::Unit::C,
                                this->HeatRecInletTemp,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                this->Name);
            SetupOutputVariable(state,
                                "Chiller Heat Recovery Outlet Temperature",
                                OutputProcessor::Unit::C,
                                this->HeatRecOutletTemp,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                this->Name);
            SetupOutputVariable(state,
                                "Chiller Heat Recovery Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                this->HeatRecMdot,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                this->Name);
            SetupOutputVariable(state,
                                "Chiller Effective Heat Rejection Temperature",
                                OutputProcessor::Unit::C,
                                this->ChillerCondAvgTemp,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                this->Name);
        }
        if (state.dataGlobal->AnyEnergyManagementSystemInModel) {
            SetupEMSInternalVariable(state, "Chiller Nominal Capacity", this->Name, "[W]", this->NomCap);
        }
    }

    void ElectricChillerSpecs::simulate(
        EnergyPlusData &state, const PlantLocation &calledFromLocation, bool FirstHVACIteration, Real64 &CurLoad, bool RunFlag)
    {
        if (calledFromLocation.loopNum == this->CWLoopNum) { // chilled water loop
            this->initialize(state, RunFlag, CurLoad);
            auto &sim_component(
                state.dataPlnt->PlantLoop(this->CWLoopNum).LoopSide(this->CWLoopSideNum).Branch(this->CWBranchNum).Comp(this->CWCompNum));
            this->calculate(state, CurLoad, RunFlag, sim_component.FlowCtrl);
            this->update(state, CurLoad, RunFlag);
        } else if (calledFromLocation.loopNum == this->CDLoopNum) { // condenser loop
            PlantUtilities::UpdateChillerComponentCondenserSide(state,
                                                                this->CDLoopNum,
                                                                this->CDLoopSideNum,
                                                                this->plantTypeOfNum,
                                                                this->CondInletNodeNum,
                                                                this->CondOutletNodeNum,
                                                                this->QCondenser,
                                                                this->CondInletTemp,
                                                                this->CondOutletTemp,
                                                                this->CondMassFlowRate,
                                                                FirstHVACIteration);
        } else if (calledFromLocation.loopNum == this->HRLoopNum) { // heat recovery loop
            PlantUtilities::UpdateComponentHeatRecoverySide(state,
                                                            this->HRLoopNum,
                                                            this->HRLoopSideNum,
                                                            this->plantTypeOfNum,
                                                            this->HeatRecInletNodeNum,
                                                            this->HeatRecOutletNodeNum,
                                                            this->QHeatRecovery,
                                                            this->HeatRecInletTemp,
                                                            this->HeatRecOutletTemp,
                                                            this->HeatRecMdot,
                                                            FirstHVACIteration);
        }
    }

    void ElectricChillerSpecs::initialize(EnergyPlusData &state, bool const RunFlag, Real64 const MyLoad)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   April 2002
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for initializations of the Electric Chiller components

        // METHODOLOGY EMPLOYED:
        // Uses the status flags to trigger initializations.

        // SUBROUTINE PARAMETER DEFINITIONS:
        static constexpr std::string_view const RoutineName("InitElectricChiller");

        this->oneTimeInit(state);

        if (this->MyEnvrnFlag && state.dataGlobal->BeginEnvrnFlag && (state.dataPlnt->PlantFirstSizesOkayToFinalize)) {

            Real64 rho = FluidProperties::GetDensityGlycol(state,
                                                           state.dataPlnt->PlantLoop(this->CWLoopNum).FluidName,
                                                           DataGlobalConstants::CWInitConvTemp,
                                                           state.dataPlnt->PlantLoop(this->CWLoopNum).FluidIndex,
                                                           RoutineName);

            this->EvapMassFlowRateMax = rho * this->EvapVolFlowRate;
            PlantUtilities::InitComponentNodes(state,
                                               0.0,
                                               this->EvapMassFlowRateMax,
                                               this->EvapInletNodeNum,
                                               this->EvapOutletNodeNum,
                                               this->CWLoopNum,
                                               this->CWLoopSideNum,
                                               this->CWBranchNum,
                                               this->CWCompNum);

            // init maximum available condenser flow rate
            if (this->CondenserType == DataPlant::CondenserType::WaterCooled) {

                state.dataLoopNodes->Node(this->CondInletNodeNum).Temp = this->TempDesCondIn; // old behavior, still want?

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
            } else { // air or evap-air

                rho = Psychrometrics::PsyRhoAirFnPbTdbW(state, state.dataEnvrn->StdBaroPress, this->TempDesCondIn, 0.0, RoutineName);
                this->CondMassFlowRateMax = rho * this->CondVolFlowRate;

                state.dataLoopNodes->Node(this->CondInletNodeNum).MassFlowRate = this->CondMassFlowRateMax;
                state.dataLoopNodes->Node(this->CondOutletNodeNum).MassFlowRate = state.dataLoopNodes->Node(this->CondInletNodeNum).MassFlowRate;
                state.dataLoopNodes->Node(this->CondInletNodeNum).MassFlowRateMaxAvail =
                    state.dataLoopNodes->Node(this->CondInletNodeNum).MassFlowRate;
                state.dataLoopNodes->Node(this->CondInletNodeNum).MassFlowRateMax = state.dataLoopNodes->Node(this->CondInletNodeNum).MassFlowRate;
                state.dataLoopNodes->Node(this->CondOutletNodeNum).MassFlowRateMax = state.dataLoopNodes->Node(this->CondInletNodeNum).MassFlowRate;
                state.dataLoopNodes->Node(this->CondInletNodeNum).MassFlowRateMinAvail = 0.0;
                state.dataLoopNodes->Node(this->CondInletNodeNum).MassFlowRateMin = 0.0;
                state.dataLoopNodes->Node(this->CondOutletNodeNum).MassFlowRateMinAvail = 0.0;
                state.dataLoopNodes->Node(this->CondOutletNodeNum).MassFlowRateMin = 0.0;
            }

            if (this->HeatRecActive) {
                rho = FluidProperties::GetDensityGlycol(state,
                                                        state.dataPlnt->PlantLoop(this->HRLoopNum).FluidName,
                                                        DataGlobalConstants::HWInitConvTemp,
                                                        state.dataPlnt->PlantLoop(this->HRLoopNum).FluidIndex,
                                                        RoutineName);
                this->DesignHeatRecMassFlowRate = rho * this->DesignHeatRecVolFlowRate;

                PlantUtilities::InitComponentNodes(state,
                                                   0.0,
                                                   this->DesignHeatRecMassFlowRate,
                                                   this->HeatRecInletNodeNum,
                                                   this->HeatRecOutletNodeNum,
                                                   this->HRLoopNum,
                                                   this->HRLoopSideNum,
                                                   this->HRBranchNum,
                                                   this->HRCompNum);
                this->HeatRecMaxCapacityLimit = this->HeatRecCapacityFraction * (this->NomCap + this->NomCap / this->COP);

                if (this->HeatRecSetPointNodeNum > 0) {
                    Real64 THeatRecSetPoint(0.0);
                    {
                        auto const SELECT_CASE_var(state.dataPlnt->PlantLoop(this->HRLoopNum).LoopDemandCalcScheme);
                        if (SELECT_CASE_var == DataPlant::iLoopDemandCalcScheme::SingleSetPoint) {
                            THeatRecSetPoint = state.dataLoopNodes->Node(this->HeatRecSetPointNodeNum).TempSetPoint;
                        } else if (SELECT_CASE_var == DataPlant::iLoopDemandCalcScheme::DualSetPointDeadBand) {
                            THeatRecSetPoint = state.dataLoopNodes->Node(this->HeatRecSetPointNodeNum).TempSetPointHi;
                        }
                    }
                    if (THeatRecSetPoint == DataLoopNode::SensedNodeFlagValue) {
                        if (!state.dataGlobal->AnyEnergyManagementSystemInModel) {
                            if (!this->HRSPErrDone) {
                                ShowWarningError(state, "Missing heat recovery temperature setpoint for chiller named " + this->Name);
                                ShowContinueError(state,
                                                  "  A temperature setpoint is needed at the heat recovery leaving temperature setpoint node "
                                                  "specified, use a SetpointManager");
                                ShowContinueError(state,
                                                  "  The overall loop setpoint will be assumed for heat recovery. The simulation continues ...");
                                this->HeatRecSetPointNodeNum = state.dataPlnt->PlantLoop(this->HRLoopNum).TempSetPointNodeNum;
                                this->HRSPErrDone = true;
                            }
                        } else {
                            // need call to EMS to check node
                            bool FatalError = false; // but not really fatal yet, but should be.
                            EMSManager::CheckIfNodeSetPointManagedByEMS(
                                state, this->EvapOutletNodeNum, EMSManager::SPControlType::iTemperatureSetPoint, FatalError);
                            state.dataLoopNodes->NodeSetpointCheck(this->EvapOutletNodeNum).needsSetpointChecking = false;
                            if (FatalError) {
                                if (!this->HRSPErrDone) {
                                    ShowWarningError(state, "Missing heat recovery temperature setpoint for chiller named " + this->Name);
                                    ShowContinueError(state,
                                                      "  A temperature setpoint is needed at the heat recovery leaving temperature setpoint node "
                                                      "specified, use a SetpointManager to establish a setpoint");
                                    ShowContinueError(state, "  or use an EMS actuator to establish a setpoint at this node ");
                                    ShowContinueError(state,
                                                      "  The overall loop setpoint will be assumed for heat recovery. The simulation continues ...");
                                    this->HeatRecSetPointNodeNum = state.dataPlnt->PlantLoop(this->HRLoopNum).TempSetPointNodeNum;
                                    this->HRSPErrDone = true;
                                }
                            }
                        } // IF (.NOT. AnyEnergyManagementSystemInModel) THEN
                    }     // IF(THeatRecSetpoint == DataLoopNode::SensedNodeFlagValue)THEN
                }         // IF(ElectricChiller(ChillNum)%HeatRecSetpointNodeNum > 0)THEN
            }             // IF (ElectricChiller(ChillNum)%HeatRecActive) THEN

            this->MyEnvrnFlag = false;
        }
        if (!state.dataGlobal->BeginEnvrnFlag) {
            this->MyEnvrnFlag = true;
        }

        if ((this->FlowMode == DataPlant::FlowMode::LeavingSetpointModulated) && (this->ModulatedFlowSetToLoop)) {
            // fix for clumsy old input that worked because loop setpoint was spread.
            //  could be removed with transition, testing , model change, period of being obsolete.
            {
                auto const SELECT_CASE_var(state.dataPlnt->PlantLoop(this->CWLoopNum).LoopDemandCalcScheme);
                if (SELECT_CASE_var == DataPlant::iLoopDemandCalcScheme::SingleSetPoint) {
                    state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempSetPoint =
                        state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPoint;
                } else if (SELECT_CASE_var == DataPlant::iLoopDemandCalcScheme::DualSetPointDeadBand) {
                    state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempSetPointHi =
                        state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPointHi;
                }
            }
        }

        Real64 mdot = 0.0;
        Real64 mdotCond = 0.0;
        if ((MyLoad < 0.0) && RunFlag) {
            // request full then take what can get
            mdot = this->EvapMassFlowRateMax;
            mdotCond = this->CondMassFlowRateMax;
        }
        PlantUtilities::SetComponentFlowRate(
            state, mdot, this->EvapInletNodeNum, this->EvapOutletNodeNum, this->CWLoopNum, this->CWLoopSideNum, this->CWBranchNum, this->CWCompNum);
        if (this->CondenserType == DataPlant::CondenserType::WaterCooled) {
            PlantUtilities::SetComponentFlowRate(state,
                                                 mdotCond,
                                                 this->CondInletNodeNum,
                                                 this->CondOutletNodeNum,
                                                 this->CDLoopNum,
                                                 this->CDLoopSideNum,
                                                 this->CDBranchNum,
                                                 this->CDCompNum);
        }

        // Initialize heat recovery flow rates at node
        if (this->HeatRecActive) {

            Real64 thisMdot = 0.0;
            if (RunFlag) {
                thisMdot = this->DesignHeatRecMassFlowRate;
            }

            PlantUtilities::SetComponentFlowRate(state,
                                                 thisMdot,
                                                 this->HeatRecInletNodeNum,
                                                 this->HeatRecOutletNodeNum,
                                                 this->HRLoopNum,
                                                 this->HRLoopSideNum,
                                                 this->HRBranchNum,
                                                 this->HRCompNum);
        }

        if (this->CondenserType == DataPlant::CondenserType::EvapCooled) {
            this->BasinHeaterPower = 0.0;
        }
    }

    void ElectricChillerSpecs::size(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   April 2002
        //       MODIFIED       November 2013 Daeho Kang, add component sizing table entries
        //       RE-ENGINEERED  B. Griffith, April 2011, allow repeated sizing calls, finish when ready to do so

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for sizing Electric Chiller Components for which capacities and flow rates
        // have not been specified in the input.

        // METHODOLOGY EMPLOYED:
        // Obtains evaporator flow rate from the plant sizing array. Calculates nominal capacity from
        // the evaporator flow rate and the chilled water loop design delta T. The condenser flow rate
        // is calculated from the nominal capacity, the COP, and the condenser loop design delta T.

        // SUBROUTINE PARAMETER DEFINITIONS:
        static constexpr std::string_view RoutineName("SizeElectricChiller");

        int PltSizCondNum(0);    // Plant Sizing index for condenser loop
        bool ErrorsFound(false); // If errors detected in input

        if (this->CondenserType == DataPlant::CondenserType::WaterCooled) {
            PltSizCondNum = state.dataPlnt->PlantLoop(this->CDLoopNum).PlantSizNum;
        }

        int PltSizNum = state.dataPlnt->PlantLoop(this->CWLoopNum).PlantSizNum;

        Real64 tmpNomCap = this->NomCap;
        if (PltSizNum > 0) {
            if (state.dataSize->PlantSizData(PltSizNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                Real64 rho = FluidProperties::GetDensityGlycol(state,
                                                               state.dataPlnt->PlantLoop(this->CWLoopNum).FluidName,
                                                               DataGlobalConstants::CWInitConvTemp,
                                                               state.dataPlnt->PlantLoop(this->CWLoopNum).FluidIndex,
                                                               RoutineName);
                Real64 Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                                   state.dataPlnt->PlantLoop(this->CWLoopNum).FluidName,
                                                                   DataGlobalConstants::CWInitConvTemp,
                                                                   state.dataPlnt->PlantLoop(this->CWLoopNum).FluidIndex,
                                                                   RoutineName);
                tmpNomCap =
                    Cp * rho * state.dataSize->PlantSizData(PltSizNum).DeltaT * state.dataSize->PlantSizData(PltSizNum).DesVolFlowRate * this->SizFac;
            } else {
                if (this->NomCapWasAutoSized) tmpNomCap = 0.0;
            }
            if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                if (this->NomCapWasAutoSized) {
                    this->NomCap = tmpNomCap;
                    if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(state, "Chiller:Electric", this->Name, "Design Size Nominal Capacity [W]", tmpNomCap);
                    }
                    if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(state, "Chiller:Electric", this->Name, "Initial Design Size Nominal Capacity [W]", tmpNomCap);
                    }
                } else {
                    if (this->NomCap > 0.0 && tmpNomCap > 0.0) {
                        if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                            BaseSizer::reportSizerOutput(state,
                                                         "Chiller:Electric",
                                                         this->Name,
                                                         "Design Size Nominal Capacity [W]",
                                                         tmpNomCap,
                                                         "User-Specified Nominal Capacity [W]",
                                                         this->NomCap);
                            if (state.dataGlobal->DisplayExtraWarnings) {
                                if ((std::abs(tmpNomCap - this->NomCap) / this->NomCap) > state.dataSize->AutoVsHardSizingThreshold) {
                                    ShowMessage(state, "SizeChillerElectric: Potential issue with equipment sizing for " + this->Name);
                                    ShowContinueError(state, format("User-Specified Nominal Capacity of {:.2R} [W]", this->NomCap));
                                    ShowContinueError(state, format("differs from Design Size Nominal Capacity of {:.2R} [W]", tmpNomCap));
                                    ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                        }
                        tmpNomCap = this->NomCap;
                    }
                }
            }
        } else {
            if (this->NomCapWasAutoSized && state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                ShowSevereError(state, "Autosizing of Electric Chiller nominal capacity requires a loop Sizing:Plant object");
                ShowContinueError(state, "Occurs in Electric Chiller object=" + this->Name);
                ErrorsFound = true;
            }
            if (!this->NomCapWasAutoSized && state.dataPlnt->PlantFinalSizesOkayToReport && (this->NomCap > 0.0)) {
                BaseSizer::reportSizerOutput(state, "Chiller:Electric", this->Name, "User-Specified Nominal Capacity [W]", this->NomCap);
            }
        }

        Real64 tmpEvapVolFlowRate = this->EvapVolFlowRate;
        if (PltSizNum > 0) {
            if (state.dataSize->PlantSizData(PltSizNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                tmpEvapVolFlowRate = state.dataSize->PlantSizData(PltSizNum).DesVolFlowRate * this->SizFac;
            } else {
                if (this->EvapVolFlowRateWasAutoSized) tmpEvapVolFlowRate = 0.0;
            }
            if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                if (this->EvapVolFlowRateWasAutoSized) {
                    this->EvapVolFlowRate = tmpEvapVolFlowRate;
                    if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(
                            state, "Chiller:Electric", this->Name, "Design Size Design Chilled Water Flow Rate [m3/s]", tmpEvapVolFlowRate);
                    }
                    if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(
                            state, "Chiller:Electric", this->Name, "Initial Design Size Design Chilled Water Flow Rate [m3/s]", tmpEvapVolFlowRate);
                    }
                } else {
                    if (this->EvapVolFlowRate > 0.0 && tmpEvapVolFlowRate > 0.0) {
                        if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                            BaseSizer::reportSizerOutput(state,
                                                         "Chiller:Electric",
                                                         this->Name,
                                                         "Design Size Design Chilled Water Flow Rate [m3/s]",
                                                         tmpEvapVolFlowRate,
                                                         "User-Specified Design Chilled Water Flow Rate [m3/s]",
                                                         this->EvapVolFlowRate);
                            if (state.dataGlobal->DisplayExtraWarnings) {
                                if ((std::abs(tmpEvapVolFlowRate - this->EvapVolFlowRate) / this->EvapVolFlowRate) >
                                    state.dataSize->AutoVsHardSizingThreshold) {
                                    ShowMessage(state, "SizeChillerElectric: Potential issue with equipment sizing for " + this->Name);
                                    ShowContinueError(
                                        state, format("User-Specified Design Chilled Water Flow Rate of {:.5R} [m3/s]", this->EvapVolFlowRate));
                                    ShowContinueError(
                                        state,
                                        format("differs from Design Size Design Chilled Water Flow Rate of {:.5R} [m3/s]", tmpEvapVolFlowRate));
                                    ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                        }
                        tmpEvapVolFlowRate = this->EvapVolFlowRate;
                    }
                }
            }
        } else {
            if (this->EvapVolFlowRateWasAutoSized && state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                ShowSevereError(state, "Autosizing of Electric Chiller evap flow rate requires a loop Sizing:Plant object");
                ShowContinueError(state, "Occurs in Electric Chiller object=" + this->Name);
                ErrorsFound = true;
            }
            if (!this->EvapVolFlowRateWasAutoSized && state.dataPlnt->PlantFinalSizesOkayToReport && (this->EvapVolFlowRate > 0.0)) {
                BaseSizer::reportSizerOutput(
                    state, "Chiller:Electric", this->Name, "User-Specified Design Chilled Water Flow Rate [m3/s]", this->EvapVolFlowRate);
            }
        }

        PlantUtilities::RegisterPlantCompDesignFlow(state, this->EvapInletNodeNum, tmpEvapVolFlowRate);

        Real64 tmpCondVolFlowRate = this->CondVolFlowRate;
        if (PltSizCondNum > 0 && PltSizNum > 0) {
            if (state.dataSize->PlantSizData(PltSizNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow && tmpNomCap > 0.0) {
                Real64 rho = FluidProperties::GetDensityGlycol(state,
                                                               state.dataPlnt->PlantLoop(this->CDLoopNum).FluidName,
                                                               this->TempDesCondIn,
                                                               state.dataPlnt->PlantLoop(this->CDLoopNum).FluidIndex,
                                                               RoutineName);
                Real64 Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                                   state.dataPlnt->PlantLoop(this->CDLoopNum).FluidName,
                                                                   this->TempDesCondIn,
                                                                   state.dataPlnt->PlantLoop(this->CDLoopNum).FluidIndex,
                                                                   RoutineName);
                tmpCondVolFlowRate = tmpNomCap * (1.0 + 1.0 / this->COP) / (state.dataSize->PlantSizData(PltSizCondNum).DeltaT * Cp * rho);
            } else {
                if (this->CondVolFlowRateWasAutoSized) tmpCondVolFlowRate = 0.0;
            }
            if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                if (this->CondVolFlowRateWasAutoSized) {
                    this->CondVolFlowRate = tmpCondVolFlowRate;
                    if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(
                            state, "Chiller:Electric", this->Name, "Design Size Design Condenser Water Flow Rate [m3/s]", tmpCondVolFlowRate);
                    }
                    if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(
                            state, "Chiller:Electric", this->Name, "Initial Design Size Design Condenser Water Flow Rate [m3/s]", tmpCondVolFlowRate);
                    }
                } else {
                    if (this->CondVolFlowRate > 0.0 && tmpCondVolFlowRate > 0.0) {
                        if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                            BaseSizer::reportSizerOutput(state,
                                                         "Chiller:Electric",
                                                         this->Name,
                                                         "Design Size Design Condenser Water Flow Rate [m3/s]",
                                                         tmpCondVolFlowRate,
                                                         "User-Specified Design Condenser Water Flow Rate [m3/s]",
                                                         this->CondVolFlowRate);
                            if (state.dataGlobal->DisplayExtraWarnings) {
                                if ((std::abs(tmpCondVolFlowRate - this->CondVolFlowRate) / this->CondVolFlowRate) >
                                    state.dataSize->AutoVsHardSizingThreshold) {
                                    ShowMessage(state, "SizeChillerElectric: Potential issue with equipment sizing for " + this->Name);
                                    ShowContinueError(
                                        state, format("User-Specified Design Condenser Water Flow Rate of {:.5R} [m3/s]", this->CondVolFlowRate));
                                    ShowContinueError(
                                        state,
                                        format("differs from Design Size Design Condenser Water Flow Rate of {:.5R} [m3/s]", tmpCondVolFlowRate));
                                    ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                        }
                        tmpCondVolFlowRate = this->CondVolFlowRate;
                    }
                }
            }
        } else {
            if (this->CondVolFlowRateWasAutoSized && state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                ShowSevereError(state, "Autosizing of Electric Chiller condenser flow rate requires a condenser");
                ShowContinueError(state, "loop Sizing:Plant object");
                ShowContinueError(state, "Occurs in Electric Chiller object=" + this->Name);
                ErrorsFound = true;
            }
            if (!this->CondVolFlowRateWasAutoSized && state.dataPlnt->PlantFinalSizesOkayToReport && (this->CondVolFlowRate > 0.0)) {
                BaseSizer::reportSizerOutput(
                    state, "Chiller:Electric", this->Name, "User-Specified Design Condenser Water Flow Rate [m3/s]", this->CondVolFlowRate);
            }
        }

        // save the design condenser water volumetric flow rate for use by the condenser water loop sizing algorithms
        if (this->CondenserType == DataPlant::CondenserType::WaterCooled) {
            PlantUtilities::RegisterPlantCompDesignFlow(state, this->CondInletNodeNum, tmpCondVolFlowRate);
        }
        if (ErrorsFound) {
            ShowFatalError(state, "Preceding sizing errors cause program termination");
        }

        if (this->HeatRecActive) {
            Real64 tmpHeatRecVolFlowRate = this->CondVolFlowRate * this->HeatRecCapacityFraction;
            if (!this->DesignHeatRecVolFlowRateWasAutoSized) tmpHeatRecVolFlowRate = this->DesignHeatRecVolFlowRate;
            if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                if (this->DesignHeatRecVolFlowRateWasAutoSized) {
                    this->DesignHeatRecVolFlowRate = tmpHeatRecVolFlowRate;
                    if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(
                            state, "Chiller:Electric", this->Name, "Design Size Design Heat Recovery Fluid Flow Rate [m3/s]", tmpHeatRecVolFlowRate);
                    }
                    if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(state,
                                                     "Chiller:Electric",
                                                     this->Name,
                                                     "Initial Design Size Design Heat Recovery Fluid Flow Rate [m3/s]",
                                                     tmpHeatRecVolFlowRate);
                    }
                } else {
                    if (this->DesignHeatRecVolFlowRate > 0.0 && tmpHeatRecVolFlowRate > 0.0) {
                        if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                            BaseSizer::reportSizerOutput(state,
                                                         "Chiller:Electric",
                                                         this->Name,
                                                         "Design Size Design Heat Recovery Fluid Flow Rate [m3/s]",
                                                         tmpHeatRecVolFlowRate,
                                                         "User-Specified Design Heat Recovery Fluid Flow Rate [m3/s]",
                                                         this->DesignHeatRecVolFlowRate);
                            if (state.dataGlobal->DisplayExtraWarnings) {
                                if ((std::abs(tmpHeatRecVolFlowRate - this->DesignHeatRecVolFlowRate) / this->DesignHeatRecVolFlowRate) >
                                    state.dataSize->AutoVsHardSizingThreshold) {
                                    ShowMessage(state, "SizeChillerElectric: Potential issue with equipment sizing for " + this->Name);
                                    ShowContinueError(state,
                                                      format("User-Specified Design Heat Recovery Fluid Flow Rate of {:.5R} [m3/s]",
                                                             this->DesignHeatRecVolFlowRate));
                                    ShowContinueError(state,
                                                      format("differs from Design Size Design Heat Recovery Fluid Flow Rate of {:.5R} [m3/s]",
                                                             tmpHeatRecVolFlowRate));
                                    ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                        }
                        tmpHeatRecVolFlowRate = this->DesignHeatRecVolFlowRate;
                    }
                }
            }
            // save the reference heat recovery fluid volumetric flow rate
            PlantUtilities::RegisterPlantCompDesignFlow(state, this->HeatRecInletNodeNum, tmpHeatRecVolFlowRate);
        }

        if (state.dataPlnt->PlantFinalSizesOkayToReport) {
            OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchMechType, this->Name, "Chiller:Electric");
            OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchMechNomEff, this->Name, this->COP);
            OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchMechNomCap, this->Name, this->NomCap);
        }
    }

    void ElectricChillerSpecs::calculate(EnergyPlusData &state,
                                         Real64 &MyLoad,
                                         bool const RunFlag,
                                         DataBranchAirLoopPlant::ControlTypeEnum const EquipFlowCtrl)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Dan Fisher / Brandon Anderson
        //       DATE WRITTEN   Sept. 2000
        //       MODIFIED       Feb. 2010, Chandan Sharma, FSEC. Added basin heater
        //                      Jun. 2016, Rongpeng Zhang, LBNL. Applied the chiller supply water temperature sensor fault model
        //                      Nov. 2016, Rongpeng Zhang, LBNL. Added Fouling Chiller fault
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // simulate a vapor compression chiller using the Electric model

        // METHODOLOGY EMPLOYED:
        // curve fit of performance data:

        // REFERENCES:
        // 1. BLAST Users Manual
        // 2. CHILLER User Manual

        static constexpr std::string_view RoutineName("CalcElectricChillerModel");
        this->EvapMassFlowRate = 0.0;
        this->CondMassFlowRate = 0.0;
        this->Power = 0.0;
        this->Energy = 0.0;
        this->QCondenser = 0.0;
        this->QEvaporator = 0.0;
        this->CondenserEnergy = 0.0;
        this->EvaporatorEnergy = 0.0;
        this->QHeatRecovered = 0.0;
        this->ActualCOP = 0.0;

        //   calculate end time of current time step
        Real64 CurrentEndTime = state.dataGlobal->CurrentTime + state.dataHVACGlobal->SysTimeElapsed;

        //   Print warning messages only when valid and only for the first occurrence. Let summary provide statistics.
        //   Wait for next time step to print warnings. If simulation iterates, print out
        //   the warning for the last iteration only. Must wait for next time step to accomplish this.
        //   If a warning occurs and the simulation down shifts, the warning is not valid.
        if (CurrentEndTime > this->CurrentEndTimeLast && state.dataHVACGlobal->TimeStepSys >= this->TimeStepSysLast) {
            if (this->PrintMessage) {
                ++this->MsgErrorCount;
                //       Show single warning and pass additional info to ShowRecurringWarningErrorAtEnd
                if (this->MsgErrorCount < 2) {
                    ShowWarningError(state, this->MsgBuffer1 + '.');
                    ShowContinueError(state, this->MsgBuffer2);
                } else {
                    ShowRecurringWarningErrorAtEnd(
                        state, this->MsgBuffer1 + " error continues.", this->ErrCount1, this->MsgDataLast, this->MsgDataLast, _, "[C]", "[C]");
                }
            }
        }

        //   save last system time step and last end time of current time step (used to determine if warning is valid)
        this->TimeStepSysLast = state.dataHVACGlobal->TimeStepSys;
        this->CurrentEndTimeLast = CurrentEndTime;

        // If no loop demand or chiller OFF, return
        // If Chiller load is 0 or chiller is not running then leave the subroutine.
        if (MyLoad >= 0.0 || !RunFlag) {
            // call for zero flow before leaving
            if (EquipFlowCtrl == DataBranchAirLoopPlant::ControlTypeEnum::SeriesActive ||
                state.dataPlnt->PlantLoop(this->CWLoopNum).LoopSide(this->CWLoopSideNum).FlowLock == DataPlant::iFlowLock::Locked) {
                this->EvapMassFlowRate = state.dataLoopNodes->Node(this->EvapInletNodeNum).MassFlowRate;
            } else {
                this->EvapMassFlowRate = 0.0;
                PlantUtilities::SetComponentFlowRate(state,
                                                     this->EvapMassFlowRate,
                                                     this->EvapInletNodeNum,
                                                     this->EvapOutletNodeNum,
                                                     this->CWLoopNum,
                                                     this->CWLoopSideNum,
                                                     this->CWBranchNum,
                                                     this->CWCompNum);
            }
            if (this->CondenserType == DataPlant::CondenserType::WaterCooled) {
                if (state.dataPlnt->PlantLoop(this->CDLoopNum)
                        .LoopSide(this->CDLoopSideNum)
                        .Branch(this->CDBranchNum)
                        .Comp(this->CDCompNum)
                        .FlowCtrl == DataBranchAirLoopPlant::ControlTypeEnum::SeriesActive) {
                    this->CondMassFlowRate = state.dataLoopNodes->Node(this->CondInletNodeNum).MassFlowRate;
                } else {
                    this->CondMassFlowRate = 0.0;
                    PlantUtilities::SetComponentFlowRate(state,
                                                         this->CondMassFlowRate,
                                                         this->CondInletNodeNum,
                                                         this->CondOutletNodeNum,
                                                         this->CDLoopNum,
                                                         this->CDLoopSideNum,
                                                         this->CDBranchNum,
                                                         this->CDCompNum);
                }
            }

            if (this->CondenserType == DataPlant::CondenserType::EvapCooled) {
                CalcBasinHeaterPower(
                    state, this->BasinHeaterPowerFTempDiff, this->BasinHeaterSchedulePtr, this->BasinHeaterSetPointTemp, this->BasinHeaterPower);
            }
            this->PrintMessage = false;
            return;
        }

        // If not air or evap cooled then set to the condenser node that is attached to a cooling tower

        // Set mass flow rates
        if (this->CondenserType == DataPlant::CondenserType::WaterCooled) {
            this->CondMassFlowRate = this->CondMassFlowRateMax;
            PlantUtilities::SetComponentFlowRate(state,
                                                 this->CondMassFlowRate,
                                                 this->CondInletNodeNum,
                                                 this->CondOutletNodeNum,
                                                 this->CDLoopNum,
                                                 this->CDLoopSideNum,
                                                 this->CDBranchNum,
                                                 this->CDCompNum);
            PlantUtilities::PullCompInterconnectTrigger(state,
                                                        this->CWLoopNum,
                                                        this->CWLoopSideNum,
                                                        this->CWBranchNum,
                                                        this->CWCompNum,
                                                        this->CondMassFlowIndex,
                                                        this->CDLoopNum,
                                                        this->CDLoopSideNum,
                                                        DataPlant::iCriteriaType::MassFlowRate,
                                                        this->CondMassFlowRate);
            if (this->CondMassFlowRate < DataBranchAirLoopPlant::MassFlowTolerance) return;
        }

        //  LOAD LOCAL VARIABLES FROM DATA STRUCTURE (for code readability)
        Real64 PartLoadRat = this->MinPartLoadRat;
        Real64 ChillerNomCap = this->NomCap;
        Real64 TempEvapOut = state.dataLoopNodes->Node(this->EvapOutletNodeNum).Temp;

        // If there is a fault of chiller fouling
        if (this->FaultyChillerFoulingFlag && (!state.dataGlobal->WarmupFlag) && (!state.dataGlobal->DoingSizing) &&
            (!state.dataGlobal->KickOffSimulation)) {
            int FaultIndex = this->FaultyChillerFoulingIndex;
            Real64 NomCap_ff = ChillerNomCap;
            Real64 RatedCOP_ff = this->COP;

            // calculate the Faulty Chiller Fouling Factor using fault information
            this->FaultyChillerFoulingFactor = state.dataFaultsMgr->FaultsChillerFouling(FaultIndex).CalFoulingFactor(state);

            // update the Chiller nominal capacity and COP at faulty cases
            ChillerNomCap = NomCap_ff * this->FaultyChillerFoulingFactor;
            this->COP = RatedCOP_ff * this->FaultyChillerFoulingFactor;
        }

        // initialize outlet air humidity ratio of air or evap cooled chillers
        this->CondOutletHumRat = state.dataLoopNodes->Node(this->CondInletNodeNum).HumRat;

        if (this->CondenserType == DataPlant::CondenserType::AirCooled) { // Condenser inlet temp = outdoor temp
            state.dataLoopNodes->Node(this->CondInletNodeNum).Temp = state.dataLoopNodes->Node(this->CondInletNodeNum).OutAirDryBulb;
            //  Warn user if entering condenser temperature falls below 0C
            if (state.dataLoopNodes->Node(this->CondInletNodeNum).Temp < 0.0 && !state.dataGlobal->WarmupFlag) {
                this->PrintMessage = true;
                this->MsgBuffer1 =
                    format("CalcElectricChillerModel - Chiller:Electric \"{}\" - Air Cooled Condenser Inlet Temperature below 0C", this->Name);
                this->MsgBuffer2 = format("... Outdoor Dry-bulb Condition = {:6.2F} C. Occurrence info = {}, {} {}",
                                          state.dataLoopNodes->Node(this->CondInletNodeNum).Temp,
                                          state.dataEnvrn->EnvironmentName,
                                          state.dataEnvrn->CurMnDy,
                                          General::CreateSysTimeIntervalString(state));
                this->MsgDataLast = state.dataLoopNodes->Node(this->CondInletNodeNum).Temp;
            } else {
                this->PrintMessage = false;
            }
        } else if (this->CondenserType == DataPlant::CondenserType::EvapCooled) { // Condenser inlet temp = (outdoor wet bulb)
            state.dataLoopNodes->Node(this->CondInletNodeNum).Temp = state.dataLoopNodes->Node(this->CondInletNodeNum).OutAirWetBulb;
            //  line above assumes evaporation pushes condenser inlet air humidity ratio to saturation
            this->CondOutletHumRat = Psychrometrics::PsyWFnTdbTwbPb(state,
                                                                    state.dataLoopNodes->Node(this->CondInletNodeNum).Temp,
                                                                    state.dataLoopNodes->Node(this->CondInletNodeNum).Temp,
                                                                    state.dataLoopNodes->Node(this->CondInletNodeNum).Press);
            //  Warn user if evap condenser wet bulb temperature falls below 10C
            if (state.dataLoopNodes->Node(this->CondInletNodeNum).Temp < 10.0 && !state.dataGlobal->WarmupFlag) {
                this->PrintMessage = true;
                this->MsgBuffer1 =
                    format("CalcElectricChillerModel - Chiller:Electric \"{}\" - Evap Cooled Condenser Inlet Temperature below 10C", this->Name);
                this->MsgBuffer2 = format("... Outdoor Wet-bulb Condition = {:6.2F} C. Occurrence info = {}, {} {}",
                                          state.dataLoopNodes->Node(this->CondInletNodeNum).Temp,
                                          state.dataEnvrn->EnvironmentName,
                                          state.dataEnvrn->CurMnDy,
                                          General::CreateSysTimeIntervalString(state));
                this->MsgDataLast = state.dataLoopNodes->Node(this->CondInletNodeNum).Temp;
            } else {
                this->PrintMessage = false;
            }
        } // End of the Air Cooled/Evap Cooled Logic block

        Real64 condInletTemp = state.dataLoopNodes->Node(this->CondInletNodeNum).Temp;

        // correct inlet temperature if using heat recovery
        if (this->HeatRecActive) {
            if ((this->QHeatRecovery + this->QCondenser) > 0.0) {
                this->AvgCondSinkTemp = (this->QHeatRecovery * this->HeatRecInletTemp + this->QCondenser * this->CondInletTemp) /
                                        (this->QHeatRecovery + this->QCondenser);
            } else {
                this->AvgCondSinkTemp = condInletTemp;
            }
        } else {
            this->AvgCondSinkTemp = condInletTemp;
        }

        // If there is a fault of Chiller SWT Sensor
        if (this->FaultyChillerSWTFlag && (!state.dataGlobal->WarmupFlag) && (!state.dataGlobal->DoingSizing) &&
            (!state.dataGlobal->KickOffSimulation)) {
            int FaultIndex = this->FaultyChillerSWTIndex;
            Real64 EvapOutletTemp_ff = TempEvapOut;

            // calculate the sensor offset using fault information
            this->FaultyChillerSWTOffset = state.dataFaultsMgr->FaultsChillerSWTSensor(FaultIndex).CalFaultOffsetAct(state);
            // update the TempEvapOut
            TempEvapOut = max(this->TempLowLimitEvapOut,
                              min(state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp, EvapOutletTemp_ff - this->FaultyChillerSWTOffset));
            this->FaultyChillerSWTOffset = EvapOutletTemp_ff - TempEvapOut;
        }

        // Calculate chiller performance from this set of performance equations.
        Real64 DeltaTemp = (this->AvgCondSinkTemp - this->TempDesCondIn) / this->TempRiseCoef - (TempEvapOut - this->TempDesEvapOut);

        // model should have bounds on DeltaTemp and check them (also needs engineering ref content)
        Real64 AvailNomCapRat = this->CapRatCoef(1) + this->CapRatCoef(2) * DeltaTemp + this->CapRatCoef(3) * pow_2(DeltaTemp);

        Real64 AvailChillerCap = ChillerNomCap * AvailNomCapRat;

        Real64 FullLoadPowerRat = this->PowerRatCoef(1) + this->PowerRatCoef(2) * AvailNomCapRat + this->PowerRatCoef(3) * pow_2(AvailNomCapRat);

        // Calculate the PLR. When there is Min PLR and the load is less than Min PLR then the Frac Full load Power
        // is calculated at Min PLR, while all other calculations are based on the actual PLR. So in that case once
        // FracFullLoadPower is calculated the PLR should be recalculated
        if (AvailChillerCap > 0.0) {
            PartLoadRat = max(this->MinPartLoadRat, min(std::abs(MyLoad) / AvailChillerCap, this->MaxPartLoadRat));
        }

        Real64 FracFullLoadPower = this->FullLoadCoef(1) + this->FullLoadCoef(2) * PartLoadRat + this->FullLoadCoef(3) * pow_2(PartLoadRat);

        // If the PLR is less than Min PLR calculate the actual PLR for calculations. The power will then adjust for
        // the cycling.

        Real64 OperPartLoadRat; // Actual Operating PLR
        if (AvailChillerCap > 0.0) {
            if (std::abs(MyLoad) / AvailChillerCap < this->MinPartLoadRat) {
                OperPartLoadRat = std::abs(MyLoad) / AvailChillerCap;
            } else {
                OperPartLoadRat = PartLoadRat;
            }
        } else {
            OperPartLoadRat = 0.0;
        }

        Real64 Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                           state.dataPlnt->PlantLoop(this->CWLoopNum).FluidName,
                                                           state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp,
                                                           state.dataPlnt->PlantLoop(this->CWLoopNum).FluidIndex,
                                                           RoutineName);

        // If FlowLock is True, the new resolved mdot is used to update Power, QEvap, Qcond, and
        // condenser side outlet temperature.
        if (state.dataPlnt->PlantLoop(this->CWLoopNum).LoopSide(this->CWLoopSideNum).FlowLock == DataPlant::iFlowLock::Unlocked) {
            this->PossibleSubcooling = !(state.dataPlnt->PlantLoop(this->CWLoopNum)
                                             .LoopSide(this->CWLoopSideNum)
                                             .Branch(this->CWLoopSideNum)
                                             .Comp(this->CWCompNum)
                                             .CurOpSchemeType == DataPlant::CompSetPtBasedSchemeType);
            this->QEvaporator = AvailChillerCap * OperPartLoadRat;
            Real64 FRAC;
            if (OperPartLoadRat < this->MinPartLoadRat) {
                FRAC = min(1.0, (OperPartLoadRat / this->MinPartLoadRat));
            } else {
                FRAC = 1.0;
            }
            this->Power = FracFullLoadPower * FullLoadPowerRat * AvailChillerCap / this->COP * FRAC;

            // Either set the flow to the Constant value or calculate the flow for the variable volume
            if ((this->FlowMode == DataPlant::FlowMode::Constant) || (this->FlowMode == DataPlant::FlowMode::NotModulated)) {

                // Start by assuming max (design) flow
                this->EvapMassFlowRate = EvapMassFlowRateMax;
                // Use SetComponentFlowRate to decide actual flow
                PlantUtilities::SetComponentFlowRate(state,
                                                     this->EvapMassFlowRate,
                                                     this->EvapInletNodeNum,
                                                     this->EvapOutletNodeNum,
                                                     this->CWLoopNum,
                                                     this->CWLoopSideNum,
                                                     this->CWBranchNum,
                                                     this->CWCompNum);
                // Evaluate delta temp based on actual flow rate
                Real64 EvapDeltaTemp(0.0);
                if (this->EvapMassFlowRate != 0.0) {
                    EvapDeltaTemp = this->QEvaporator / this->EvapMassFlowRate / Cp;
                } else {
                    EvapDeltaTemp = 0.0;
                }
                // Evaluate outlet temp based on delta
                this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - EvapDeltaTemp;

            } else if (this->FlowMode == DataPlant::FlowMode::LeavingSetpointModulated) {

                // Calculate the Delta Temp from the inlet temp to the chiller outlet setpoint
                Real64 EvapDeltaTemp(0.0);
                {
                    auto const SELECT_CASE_var(state.dataPlnt->PlantLoop(this->CWLoopNum).LoopDemandCalcScheme);
                    if (SELECT_CASE_var == DataPlant::iLoopDemandCalcScheme::SingleSetPoint) {
                        EvapDeltaTemp =
                            state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempSetPoint;
                    } else if (SELECT_CASE_var == DataPlant::iLoopDemandCalcScheme::DualSetPointDeadBand) {
                        EvapDeltaTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp -
                                        state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempSetPointHi;
                    }
                }

                if (EvapDeltaTemp != 0.0) {

                    // Calculate desired flow to request based on load
                    this->EvapMassFlowRate = std::abs(this->QEvaporator / Cp / EvapDeltaTemp);
                    // Check to see if the Maximum is exceeded, if so set to maximum
                    if ((this->EvapMassFlowRate - EvapMassFlowRateMax) > DataBranchAirLoopPlant::MassFlowTolerance) this->PossibleSubcooling = true;
                    this->EvapMassFlowRate = min(EvapMassFlowRateMax, this->EvapMassFlowRate);
                    // Use SetComponentFlowRate to decide actual flow
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

                    // Try to request zero flow
                    this->EvapMassFlowRate = 0.0;
                    // Use SetComponentFlowRate to decide actual flow
                    PlantUtilities::SetComponentFlowRate(state,
                                                         this->EvapMassFlowRate,
                                                         this->EvapInletNodeNum,
                                                         this->EvapOutletNodeNum,
                                                         this->CWLoopNum,
                                                         this->CWLoopSideNum,
                                                         this->CWBranchNum,
                                                         this->CWCompNum);
                    // No deltaT since component is not running
                    this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp;
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
                                        Cp,
                                        state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp,
                                        this->EvapOutletTemp,
                                        this->EvapMassFlowRate,
                                        this->QEvaporator);
            }

        } else { // If FlowLock is True

            this->EvapMassFlowRate = state.dataLoopNodes->Node(this->EvapInletNodeNum).MassFlowRate;
            PlantUtilities::SetComponentFlowRate(state,
                                                 this->EvapMassFlowRate,
                                                 this->EvapInletNodeNum,
                                                 this->EvapOutletNodeNum,
                                                 this->CWLoopNum,
                                                 this->CWLoopSideNum,
                                                 this->CWBranchNum,
                                                 this->CWCompNum);

            //       Some other component set the flow to 0. No reason to continue with calculations.
            if (this->EvapMassFlowRate == 0.0) {
                MyLoad = 0.0;
                if (this->CondenserType == DataPlant::CondenserType::EvapCooled) {
                    CalcBasinHeaterPower(
                        state, this->BasinHeaterPowerFTempDiff, this->BasinHeaterSchedulePtr, this->BasinHeaterSetPointTemp, this->BasinHeaterPower);
                }
                this->PrintMessage = false;
                return;
            }
            // Flow resolver might have given less flow or control scheme have provided more load, which may
            // result in subcooling.
            if (this->PossibleSubcooling) {
                this->QEvaporator = std::abs(MyLoad);
                Real64 EvapDeltaTemp = this->QEvaporator / this->EvapMassFlowRate / Cp;
                this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - EvapDeltaTemp;
            } else { // No subcooling in this case.No recalculation required.Still need to check chiller low temp limit

                Real64 TempEvapOutSetPoint(0.0); // C - evaporator outlet temperature setpoint
                {
                    auto const SELECT_CASE_var(state.dataPlnt->PlantLoop(this->CWLoopNum).LoopDemandCalcScheme);
                    if (SELECT_CASE_var == DataPlant::iLoopDemandCalcScheme::SingleSetPoint) {
                        if ((this->FlowMode == DataPlant::FlowMode::LeavingSetpointModulated) ||
                            (state.dataPlnt->PlantLoop(this->CWLoopNum)
                                 .LoopSide(this->CWLoopSideNum)
                                 .Branch(this->CWLoopSideNum)
                                 .Comp(this->CWCompNum)
                                 .CurOpSchemeType == DataPlant::CompSetPtBasedSchemeType) ||
                            (state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempSetPoint != DataLoopNode::SensedNodeFlagValue)) {
                            TempEvapOutSetPoint = state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempSetPoint;
                        } else {
                            TempEvapOutSetPoint =
                                state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPoint;
                        }
                    } else if (SELECT_CASE_var == DataPlant::iLoopDemandCalcScheme::DualSetPointDeadBand) {
                        if ((this->FlowMode == DataPlant::FlowMode::LeavingSetpointModulated) ||
                            (state.dataPlnt->PlantLoop(this->CWLoopNum)
                                 .LoopSide(this->CWLoopSideNum)
                                 .Branch(this->CWLoopSideNum)
                                 .Comp(this->CWCompNum)
                                 .CurOpSchemeType == DataPlant::CompSetPtBasedSchemeType) ||
                            (state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempSetPointHi != DataLoopNode::SensedNodeFlagValue)) {
                            TempEvapOutSetPoint = state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempSetPointHi;
                        } else {
                            TempEvapOutSetPoint =
                                state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPointHi;
                        }
                    }
                }
                Real64 EvapDeltaTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - TempEvapOutSetPoint;
                this->QEvaporator = std::abs(this->EvapMassFlowRate * Cp * EvapDeltaTemp);
                this->EvapOutletTemp = TempEvapOutSetPoint;
            }
            // Check that the Evap outlet temp honors both plant loop temp low limit and also the chiller low limit
            if (this->EvapOutletTemp < this->TempLowLimitEvapOut) {
                if ((state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - this->TempLowLimitEvapOut) > DataPlant::DeltaTempTol) {
                    this->EvapOutletTemp = this->TempLowLimitEvapOut;
                    Real64 EvapDeltaTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - this->EvapOutletTemp;
                    this->QEvaporator = this->EvapMassFlowRate * Cp * EvapDeltaTemp;
                } else {
                    this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp;
                    Real64 EvapDeltaTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - this->EvapOutletTemp;
                    this->QEvaporator = this->EvapMassFlowRate * Cp * EvapDeltaTemp;
                }
            }
            if (this->EvapOutletTemp < state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempMin) {
                if ((state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempMin) >
                    DataPlant::DeltaTempTol) {
                    this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempMin;
                    Real64 EvapDeltaTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - this->EvapOutletTemp;
                    this->QEvaporator = this->EvapMassFlowRate * Cp * EvapDeltaTemp;
                } else {
                    this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp;
                    Real64 EvapDeltaTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - this->EvapOutletTemp;
                    this->QEvaporator = this->EvapMassFlowRate * Cp * EvapDeltaTemp;
                }
            }

            // If load exceeds the distributed load set to the distributed load
            if (this->QEvaporator > std::abs(MyLoad)) {
                if (this->EvapMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance) {
                    this->QEvaporator = std::abs(MyLoad);
                    Real64 EvapDeltaTemp = this->QEvaporator / this->EvapMassFlowRate / Cp;
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
                                        Cp,
                                        state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp,
                                        this->EvapOutletTemp,
                                        this->EvapMassFlowRate,
                                        this->QEvaporator);
            }

            // Checks QEvaporator on the basis of the machine limits.
            if (this->QEvaporator > (AvailChillerCap * this->MaxPartLoadRat)) {
                if (this->EvapMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance) {
                    this->QEvaporator = AvailChillerCap * OperPartLoadRat;
                    Real64 EvapDeltaTemp = this->QEvaporator / this->EvapMassFlowRate / Cp;
                    this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - EvapDeltaTemp;
                } else {
                    this->QEvaporator = 0.0;
                    this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp;
                }
            }

            Real64 FRAC;
            if (OperPartLoadRat < this->MinPartLoadRat) {
                FRAC = min(1.0, (OperPartLoadRat / this->MinPartLoadRat));
            } else {
                FRAC = 1.0;
            }

            // Chiller is false loading below PLR = minimum unloading ratio, find PLR used for energy calculation
            this->Power = FracFullLoadPower * FullLoadPowerRat * AvailChillerCap / this->COP * FRAC;

            if (this->EvapMassFlowRate == 0.0) {
                this->QEvaporator = 0.0;
                this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp;
                this->Power = 0.0;
                this->PrintMessage = false;
            }
            if (this->QEvaporator == 0.0 && this->CondenserType == DataPlant::CondenserType::EvapCooled) {
                CalcBasinHeaterPower(
                    state, this->BasinHeaterPowerFTempDiff, this->BasinHeaterSchedulePtr, this->BasinHeaterSetPointTemp, this->BasinHeaterPower);
            }
        } // This is the end of the FlowLock Block

        // QCondenser is calculated the same for each type, but the power consumption should be different
        //  depending on the performance coefficients used for the chiller model.
        this->QCondenser = this->Power + this->QEvaporator;

        if (this->CondenserType == DataPlant::CondenserType::WaterCooled) {
            if (this->CondMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance) {
                // If Heat Recovery specified for this vapor compression chiller, then Qcondenser will be adjusted by this subroutine
                if (this->HeatRecActive) this->calcHeatRecovery(state, this->QCondenser, this->CondMassFlowRate, condInletTemp, this->QHeatRecovered);
                Real64 CpCond = FluidProperties::GetSpecificHeatGlycol(state,
                                                                       state.dataPlnt->PlantLoop(this->CDLoopNum).FluidName,
                                                                       condInletTemp,
                                                                       state.dataPlnt->PlantLoop(this->CDLoopNum).FluidIndex,
                                                                       RoutineName);
                this->CondOutletTemp = this->QCondenser / this->CondMassFlowRate / CpCond + condInletTemp;
            } else {
                ShowSevereError(state, "CalcElectricChillerModel: Condenser flow = 0, for ElectricChiller=" + this->Name);
                ShowContinueErrorTimeStamp(state, "");
            }
        } else { // Air Cooled or Evap Cooled

            if (this->QCondenser > 0.0) {
                this->CondMassFlowRate = this->CondMassFlowRateMax * OperPartLoadRat;
            } else {
                this->CondMassFlowRate = 0.0;
            }

            // If Heat Recovery specified for this vapor compression chiller, then Qcondenser will be adjusted by this subroutine
            if (this->HeatRecActive) this->calcHeatRecovery(state, this->QCondenser, this->CondMassFlowRate, condInletTemp, this->QHeatRecovered);
            if (this->CondMassFlowRate > 0.0) {
                Real64 CpCond = Psychrometrics::PsyCpAirFnW(state.dataLoopNodes->Node(this->CondInletNodeNum).HumRat);
                this->CondOutletTemp = condInletTemp + this->QCondenser / this->CondMassFlowRate / CpCond;
            } else {
                this->CondOutletTemp = condInletTemp;
            }
        }

        // Calculate Energy
        this->CondenserEnergy = this->QCondenser * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
        this->Energy = this->Power * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
        this->EvaporatorEnergy = this->QEvaporator * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;

        // check for problems (deal with observed negative energy results)
        if (this->Energy < 0.0) { // there is a serious problem

            if (this->CondenserType == DataPlant::CondenserType::WaterCooled) {
                // first check for run away condenser loop temps (only reason yet to be observed for this?)
                if (condInletTemp > 70.0) {
                    ShowSevereError(state,
                                    "CalcElectricChillerModel: Condenser loop inlet temperatures over 70.0 C for ElectricChiller=" + this->Name);
                    ShowContinueErrorTimeStamp(state, "");
                    ShowContinueError(state, format("Condenser loop water temperatures are too high at{:.2R}", condInletTemp));
                    ShowContinueError(state, "Check input for condenser plant loop, especially cooling tower");
                    ShowContinueError(state, format("Evaporator inlet temperature: {:.2R}", state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp));

                    ShowFatalError(state, "Program Terminates due to previous error condition");
                }
            }
            if (!state.dataGlobal->WarmupFlag) {
                if (AvailNomCapRat < 0.0) { // apparently the real reason energy goes negative
                    ShowSevereError(state, "CalcElectricChillerModel: Capacity ratio below zero for ElectricChiller=" + this->Name);
                    ShowContinueErrorTimeStamp(state, "");
                    ShowContinueError(state, "Check input for Capacity Ratio Curve");
                    ShowContinueError(state, format("Condenser inlet temperature: {:.2R}", condInletTemp));
                    ShowContinueError(state, format("Evaporator inlet temperature: {:.2R}", state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp));
                    ShowFatalError(state, "Program Terminates due to previous error condition");
                }
            }
            // If makes it here, set limits, chiller can't have negative energy/power
            // proceeding silently for now but may want to throw error here
            this->Power = 0.0;
            this->Energy = 0.0;
        }
    }

    void ElectricChillerSpecs::calcHeatRecovery(EnergyPlusData &state,
                                                Real64 &QCond,              // current condenser load
                                                Real64 const CondMassFlow,  // current condenser Mass Flow
                                                Real64 const condInletTemp, // current condenser Inlet Temp
                                                Real64 &QHeatRec            // amount of heat recovered
    )
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Richard Liesen
        //       DATE WRITTEN:    January 2004

        // PURPOSE OF THIS SUBROUTINE:
        // Calculate the heat recovered from the chiller condenser

        static constexpr std::string_view RoutineName("ChillerHeatRecovery");

        // setup initial state
        PlantUtilities::SafeCopyPlantNode(state, this->HeatRecInletNodeNum, this->HeatRecOutletNodeNum);
        this->QHeatRecovery = 0.0;
        this->EnergyHeatRecovery = 0.0;

        // Begin routine
        this->HeatRecInletTemp = state.dataLoopNodes->Node(this->HeatRecInletNodeNum).Temp;
        Real64 HeatRecMassFlowRate = state.dataLoopNodes->Node(this->HeatRecInletNodeNum).MassFlowRate;

        Real64 CpHeatRec = FluidProperties::GetSpecificHeatGlycol(state,
                                                                  state.dataPlnt->PlantLoop(this->HRLoopNum).FluidName,
                                                                  this->HeatRecInletTemp,
                                                                  state.dataPlnt->PlantLoop(this->HRLoopNum).FluidIndex,
                                                                  RoutineName);

        Real64 CpCond;
        if (this->CondenserType == DataPlant::CondenserType::WaterCooled) {
            CpCond = FluidProperties::GetSpecificHeatGlycol(state,
                                                            state.dataPlnt->PlantLoop(this->CDLoopNum).FluidName,
                                                            condInletTemp,
                                                            state.dataPlnt->PlantLoop(this->CDLoopNum).FluidIndex,
                                                            RoutineName);
        } else {
            CpCond = Psychrometrics::PsyCpAirFnW(state.dataLoopNodes->Node(this->CondInletNodeNum).HumRat);
        }

        // Before we modify the QCondenser, the total or original value is transferred to QTot
        Real64 QTotal = QCond;

        if (this->HeatRecSetPointNodeNum == 0) { // use original algorithm that blends temps
            Real64 TAvgIn = (HeatRecMassFlowRate * CpHeatRec * this->HeatRecInletTemp + CondMassFlow * CpCond * condInletTemp) /
                            (HeatRecMassFlowRate * CpHeatRec + CondMassFlow * CpCond);

            Real64 TAvgOut = QTotal / (HeatRecMassFlowRate * CpHeatRec + CondMassFlow * CpCond) + TAvgIn;

            QHeatRec = HeatRecMassFlowRate * CpHeatRec * (TAvgOut - this->HeatRecInletTemp);
            QHeatRec = max(QHeatRec, 0.0); // ensure non negative
            // check if heat flow too large for physical size of bundle
            QHeatRec = min(QHeatRec, this->HeatRecMaxCapacityLimit);
        } else { // use new algorithm to meet setpoint
            Real64 THeatRecSetPoint(0.0);
            {
                auto const SELECT_CASE_var(state.dataPlnt->PlantLoop(this->HRLoopNum).LoopDemandCalcScheme);

                if (SELECT_CASE_var == DataPlant::iLoopDemandCalcScheme::SingleSetPoint) {
                    THeatRecSetPoint = state.dataLoopNodes->Node(this->HeatRecSetPointNodeNum).TempSetPoint;
                } else if (SELECT_CASE_var == DataPlant::iLoopDemandCalcScheme::DualSetPointDeadBand) {
                    THeatRecSetPoint = state.dataLoopNodes->Node(this->HeatRecSetPointNodeNum).TempSetPointHi;
                }
            }

            Real64 QHeatRecToSetPoint = HeatRecMassFlowRate * CpHeatRec * (THeatRecSetPoint - this->HeatRecInletTemp);
            QHeatRecToSetPoint = max(QHeatRecToSetPoint, 0.0);
            QHeatRec = min(QTotal, QHeatRecToSetPoint);
            // check if heat flow too large for physical size of bundle
            QHeatRec = min(QHeatRec, this->HeatRecMaxCapacityLimit);
        }
        // check if limit on inlet is present and exceeded.
        if (this->HeatRecInletLimitSchedNum > 0) {
            Real64 HeatRecHighInletLimit = ScheduleManager::GetCurrentScheduleValue(state, this->HeatRecInletLimitSchedNum);
            if (this->HeatRecInletTemp > HeatRecHighInletLimit) { // shut down heat recovery
                QHeatRec = 0.0;
            }
        }

        QCond = QTotal - QHeatRec;

        // Calculate a new Heat Recovery Coil Outlet Temp
        if (HeatRecMassFlowRate > 0.0) {
            this->HeatRecOutletTemp = QHeatRec / (HeatRecMassFlowRate * CpHeatRec) + this->HeatRecInletTemp;
        } else {
            this->HeatRecOutletTemp = this->HeatRecInletTemp;
        }

        this->QHeatRecovery = this->QHeatRecovered;
        this->EnergyHeatRecovery = this->QHeatRecovered * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
        state.dataLoopNodes->Node(this->HeatRecOutletNodeNum).Temp = this->HeatRecOutletTemp;
        this->HeatRecMdot = state.dataLoopNodes->Node(this->HeatRecInletNodeNum).MassFlowRate;
        this->ChillerCondAvgTemp = this->AvgCondSinkTemp;
    }

    void ElectricChillerSpecs::update(EnergyPlusData &state, Real64 const MyLoad, bool const RunFlag)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Dan Fisher / Brandon Anderson
        //       DATE WRITTEN:    September 2000

        Real64 const ReportingConstant = state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;

        if (MyLoad >= 0.0 || !RunFlag) { // Chiller not running so pass inlet states to outlet states
            // set node temperatures
            state.dataLoopNodes->Node(this->EvapOutletNodeNum).Temp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp;
            state.dataLoopNodes->Node(this->CondOutletNodeNum).Temp = state.dataLoopNodes->Node(this->CondInletNodeNum).Temp;
            if (this->CondenserType != DataPlant::CondenserType::WaterCooled) {
                state.dataLoopNodes->Node(this->CondOutletNodeNum).HumRat = state.dataLoopNodes->Node(this->CondInletNodeNum).HumRat;
                state.dataLoopNodes->Node(this->CondOutletNodeNum).Enthalpy = state.dataLoopNodes->Node(this->CondInletNodeNum).Enthalpy;
            }

            this->EvapInletTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp;
            this->CondInletTemp = state.dataLoopNodes->Node(this->CondInletNodeNum).Temp;
            this->CondOutletTemp = state.dataLoopNodes->Node(this->CondOutletNodeNum).Temp;
            this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapOutletNodeNum).Temp;

            if (this->CondenserType == DataPlant::CondenserType::EvapCooled) {
                this->BasinHeaterPower = this->BasinHeaterPower;
                this->BasinHeaterConsumption = this->BasinHeaterPower * ReportingConstant;
            }

            if (this->HeatRecActive) {

                PlantUtilities::SafeCopyPlantNode(state, this->HeatRecInletNodeNum, this->HeatRecOutletNodeNum);
                this->HeatRecInletTemp = state.dataLoopNodes->Node(this->HeatRecInletNodeNum).Temp;
                this->HeatRecOutletTemp = state.dataLoopNodes->Node(this->HeatRecOutletNodeNum).Temp;
                this->HeatRecMdot = state.dataLoopNodes->Node(this->HeatRecInletNodeNum).MassFlowRate;

                this->ChillerCondAvgTemp = this->AvgCondSinkTemp;
            }

        } else { // Chiller is running, so pass calculated values
            // set node temperatures
            state.dataLoopNodes->Node(this->EvapOutletNodeNum).Temp = this->EvapOutletTemp;
            state.dataLoopNodes->Node(this->CondOutletNodeNum).Temp = this->CondOutletTemp;
            if (this->CondenserType != DataPlant::CondenserType::WaterCooled) {
                state.dataLoopNodes->Node(this->CondOutletNodeNum).HumRat = this->CondOutletHumRat;
                state.dataLoopNodes->Node(this->CondOutletNodeNum).Enthalpy =
                    Psychrometrics::PsyHFnTdbW(this->CondOutletTemp, this->CondOutletHumRat);
            }
            // set node flow rates;  for these load based models
            // assume that the sufficient evaporator flow rate available
            this->EvapInletTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp;
            this->CondInletTemp = state.dataLoopNodes->Node(this->CondInletNodeNum).Temp;
            this->CondOutletTemp = state.dataLoopNodes->Node(this->CondOutletNodeNum).Temp;
            this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapOutletNodeNum).Temp;
            if (this->CondenserType == DataPlant::CondenserType::EvapCooled) {
                this->BasinHeaterPower = this->BasinHeaterPower;
                this->BasinHeaterConsumption = this->BasinHeaterPower * ReportingConstant;
            }
            if (this->Power != 0.0) {
                this->ActualCOP = this->QEvaporator / this->Power;
            } else {
                this->ActualCOP = 0.0;
            }
        }
    }
    void ElectricChillerSpecs::oneTimeInit(EnergyPlusData &state)
    {

        if (this->MyFlag) {
            // Locate the chillers on the plant loops for later usage
            bool errFlag = false;
            this->setupOutputVariables(state);
            PlantUtilities::ScanPlantLoopsForObject(state,
                                                    this->Name,
                                                    this->plantTypeOfNum,
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
            if (this->CondenserType != DataPlant::CondenserType::AirCooled && this->CondenserType != DataPlant::CondenserType::EvapCooled) {
                PlantUtilities::ScanPlantLoopsForObject(state,
                                                        this->Name,
                                                        this->plantTypeOfNum,
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
                    state, this->CWLoopNum, this->CWLoopSideNum, this->CDLoopNum, this->CDLoopSideNum, this->plantTypeOfNum, true);
            }
            if (this->HeatRecActive) {
                PlantUtilities::ScanPlantLoopsForObject(state,
                                                        this->Name,
                                                        this->plantTypeOfNum,
                                                        this->HRLoopNum,
                                                        this->HRLoopSideNum,
                                                        this->HRBranchNum,
                                                        this->HRCompNum,
                                                        errFlag,
                                                        _,
                                                        _,
                                                        _,
                                                        this->HeatRecInletNodeNum,
                                                        _);
                PlantUtilities::InterConnectTwoPlantLoopSides(
                    state, this->CWLoopNum, this->CWLoopSideNum, this->HRLoopNum, this->HRLoopSideNum, this->plantTypeOfNum, true);
            }

            if (this->CondenserType != DataPlant::CondenserType::AirCooled && this->CondenserType != DataPlant::CondenserType::EvapCooled &&
                this->HeatRecActive) {
                PlantUtilities::InterConnectTwoPlantLoopSides(
                    state, this->CDLoopNum, this->CDLoopSideNum, this->HRLoopNum, this->HRLoopSideNum, this->plantTypeOfNum, false);
            }

            if (errFlag) {
                ShowFatalError(state, "InitElectricChiller: Program terminated due to previous condition(s).");
            }

            if (this->FlowMode == DataPlant::FlowMode::Constant) {
                // reset flow priority
                state.dataPlnt->PlantLoop(this->CWLoopNum)
                    .LoopSide(this->CWLoopSideNum)
                    .Branch(this->CWBranchNum)
                    .Comp(this->CWCompNum)
                    .FlowPriority = DataPlant::LoopFlowStatus_NeedyIfLoopOn;
            }

            if (this->FlowMode == DataPlant::FlowMode::LeavingSetpointModulated) {
                // reset flow priority
                state.dataPlnt->PlantLoop(this->CWLoopNum)
                    .LoopSide(this->CWLoopSideNum)
                    .Branch(this->CWBranchNum)
                    .Comp(this->CWCompNum)
                    .FlowPriority = DataPlant::LoopFlowStatus_NeedyIfLoopOn;

                // check if setpoint on outlet node
                if ((state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempSetPoint == DataLoopNode::SensedNodeFlagValue) &&
                    (state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempSetPointHi == DataLoopNode::SensedNodeFlagValue)) {
                    if (!state.dataGlobal->AnyEnergyManagementSystemInModel) {
                        if (!this->ModulatedFlowErrDone) {
                            ShowWarningError(state, "Missing temperature setpoint for LeavingSetpointModulated mode chiller named " + this->Name);
                            ShowContinueError(
                                state,
                                "  A temperature setpoint is needed at the outlet node of a chiller in variable flow mode, use a SetpointManager");
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
                                ShowContinueError(
                                    state, "  A temperature setpoint is needed at the outlet node of a chiller evaporator in variable flow mode");
                                ShowContinueError(state, "  use a Setpoint Manager to establish a setpoint at the chiller evaporator outlet node ");
                                ShowContinueError(state, "  or use an EMS actuator to establish a setpoint at the outlet node ");
                                ShowContinueError(state, "  The overall loop setpoint will be assumed for chiller. The simulation continues ... ");
                                this->ModulatedFlowErrDone = true;
                            }
                        }
                    }
                    this->ModulatedFlowSetToLoop = true;
                    {
                        auto const SELECT_CASE_var(state.dataPlnt->PlantLoop(this->CWLoopNum).LoopDemandCalcScheme);
                        if (SELECT_CASE_var == DataPlant::iLoopDemandCalcScheme::SingleSetPoint) {
                            state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempSetPoint =
                                state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPoint;
                        } else if (SELECT_CASE_var == DataPlant::iLoopDemandCalcScheme::DualSetPointDeadBand) {
                            state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempSetPointHi =
                                state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPointHi;
                        }
                    }
                }
            }
            this->MyFlag = false;
        }
    }

    EngineDrivenChillerSpecs *EngineDrivenChillerSpecs::factory(EnergyPlusData &state, std::string const &chillerName)
    {
        if (state.dataPlantChillers->GetEngineDrivenInput) {
            EngineDrivenChillerSpecs::getInput(state);
            state.dataPlantChillers->GetEngineDrivenInput = false;
        }
        for (auto &thisChiller : state.dataPlantChillers->EngineDrivenChiller) {
            if (UtilityRoutines::MakeUPPERCase(thisChiller.Name) == chillerName) {
                return &thisChiller;
            }
        }
        ShowFatalError(state, "Could not locate engine driven chiller with name: " + chillerName);
        return nullptr;
    }

    void EngineDrivenChillerSpecs::simulate(
        EnergyPlusData &state, const PlantLocation &calledFromLocation, bool FirstHVACIteration, Real64 &CurLoad, bool RunFlag)
    {
        if (calledFromLocation.loopNum == this->CWLoopNum) { // chilled water loop
            this->initialize(state, RunFlag, CurLoad);
            auto &sim_component(
                state.dataPlnt->PlantLoop(this->CWLoopNum).LoopSide(this->CWLoopSideNum).Branch(this->CWBranchNum).Comp(this->CWCompNum));
            this->calculate(state, CurLoad, RunFlag, sim_component.FlowCtrl);
            this->update(state, CurLoad, RunFlag);
        } else if (calledFromLocation.loopNum == this->CDLoopNum) { // condenser loop
            PlantUtilities::UpdateChillerComponentCondenserSide(state,
                                                                this->CDLoopNum,
                                                                this->CDLoopSideNum,
                                                                this->plantTypeOfNum,
                                                                this->CondInletNodeNum,
                                                                this->CondOutletNodeNum,
                                                                this->QCondenser,
                                                                this->CondInletTemp,
                                                                this->CondOutletTemp,
                                                                this->CondMassFlowRate,
                                                                FirstHVACIteration);
        } else if (calledFromLocation.loopNum == this->HRLoopNum) { // heat recovery loop
            PlantUtilities::UpdateComponentHeatRecoverySide(state,
                                                            this->HRLoopNum,
                                                            this->HRLoopSideNum,
                                                            this->plantTypeOfNum,
                                                            this->HeatRecInletNodeNum,
                                                            this->HeatRecOutletNodeNum,
                                                            this->QTotalHeatRecovered,
                                                            this->HeatRecInletTemp,
                                                            this->HeatRecOutletTemp,
                                                            this->HeatRecMdot,
                                                            FirstHVACIteration);
        }
    }

    void EngineDrivenChillerSpecs::getInput(EnergyPlusData &state)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Dan Fisher / Brandon Anderson
        //       DATE WRITTEN:    September 2000
        // PURPOSE OF THIS SUBROUTINE:
        // This routine will get the input
        // required by the EngineDriven Chiller model.

        // Locals
        // PARAMETERS
        static constexpr std::string_view RoutineName("GetEngineDrivenChillerInput: "); // include trailing blank space

        int NumAlphas; // Number of elements in the alpha array
        int NumNums;   // Number of elements in the numeric array
        int IOStat;    // IO Status when calling get input subroutine
        bool ErrorsFound(false);

        state.dataIPShortCut->cCurrentModuleObject = "Chiller:EngineDriven";
        state.dataPlantChillers->NumEngineDrivenChillers =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, state.dataIPShortCut->cCurrentModuleObject);

        if (state.dataPlantChillers->NumEngineDrivenChillers <= 0) {
            ShowSevereError(state, "No " + state.dataIPShortCut->cCurrentModuleObject + " equipment specified in input file");
            ErrorsFound = true;
        }
        // See if load distribution manager has already gotten the input
        if (allocated(state.dataPlantChillers->EngineDrivenChiller)) return;

        // ALLOCATE ARRAYS
        state.dataPlantChillers->EngineDrivenChiller.allocate(state.dataPlantChillers->NumEngineDrivenChillers);

        // LOAD ARRAYS WITH EngineDriven CURVE FIT CHILLER DATA
        for (int ChillerNum = 1; ChillerNum <= state.dataPlantChillers->NumEngineDrivenChillers; ++ChillerNum) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     state.dataIPShortCut->cCurrentModuleObject,
                                                                     ChillerNum,
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

            auto &thisChiller = state.dataPlantChillers->EngineDrivenChiller(ChillerNum);
            thisChiller.Name = state.dataIPShortCut->cAlphaArgs(1);
            thisChiller.plantTypeOfNum = DataPlant::TypeOf_Chiller_EngineDriven;

            thisChiller.NomCap = state.dataIPShortCut->rNumericArgs(1);
            if (thisChiller.NomCap == DataSizing::AutoSize) {
                thisChiller.NomCapWasAutoSized = true;
            }
            if (state.dataIPShortCut->rNumericArgs(1) == 0.0) {
                ShowSevereError(state,
                                format("Invalid {}={:.2R}", state.dataIPShortCut->cNumericFieldNames(1), state.dataIPShortCut->rNumericArgs(1)));
                ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
                ErrorsFound = true;
            }

            thisChiller.COP = state.dataIPShortCut->rNumericArgs(2);
            if (state.dataIPShortCut->rNumericArgs(2) == 0.0) {
                ShowSevereError(state,
                                format("Invalid {}={:.2R}", state.dataIPShortCut->cNumericFieldNames(2), state.dataIPShortCut->rNumericArgs(2)));
                ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
                ErrorsFound = true;
            }

            if (state.dataIPShortCut->cAlphaArgs(2) == "AIRCOOLED") {
                thisChiller.CondenserType = DataPlant::CondenserType::AirCooled;
            } else if (state.dataIPShortCut->cAlphaArgs(2) == "WATERCOOLED") {
                thisChiller.CondenserType = DataPlant::CondenserType::WaterCooled;
            } else if (state.dataIPShortCut->cAlphaArgs(2) == "EVAPORATIVELYCOOLED") {
                thisChiller.CondenserType = DataPlant::CondenserType::EvapCooled;
            } else {
                ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(2) + '=' + state.dataIPShortCut->cAlphaArgs(2));
                ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
                ErrorsFound = true;
            }

            thisChiller.EvapInletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                                               state.dataIPShortCut->cAlphaArgs(3),
                                                                               ErrorsFound,
                                                                               state.dataIPShortCut->cCurrentModuleObject,
                                                                               state.dataIPShortCut->cAlphaArgs(1),
                                                                               DataLoopNode::NodeFluidType::Water,
                                                                               DataLoopNode::NodeConnectionType::Inlet,
                                                                               NodeInputManager::compFluidStream::Primary,
                                                                               DataLoopNode::ObjectIsNotParent);
            thisChiller.EvapOutletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                                                state.dataIPShortCut->cAlphaArgs(4),
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
                                               state.dataIPShortCut->cAlphaArgs(3),
                                               state.dataIPShortCut->cAlphaArgs(4),
                                               "Chilled Water Nodes");

            if (thisChiller.CondenserType == DataPlant::CondenserType::AirCooled ||
                thisChiller.CondenserType == DataPlant::CondenserType::EvapCooled) {
                // Connection not required for air or evap cooled condenser
                // If the condenser inlet is blank for air cooled and evap cooled condensers then supply a generic name
                //  since it is not used elsewhere for connection
                if (state.dataIPShortCut->lAlphaFieldBlanks(5)) {
                    if (len(state.dataIPShortCut->cAlphaArgs(1)) <
                        DataGlobalConstants::MaxNameLength - 21) { // protect against long name leading to > 100 chars
                        state.dataIPShortCut->cAlphaArgs(5) = state.dataIPShortCut->cAlphaArgs(1) + " CONDENSER INLET NODE";
                    } else {
                        state.dataIPShortCut->cAlphaArgs(5) = state.dataIPShortCut->cAlphaArgs(1).substr(0, 79) + " CONDENSER INLET NODE";
                    }
                }
                if (state.dataIPShortCut->lAlphaFieldBlanks(6)) {
                    if (len(state.dataIPShortCut->cAlphaArgs(1)) <
                        DataGlobalConstants::MaxNameLength - 22) { // protect against long name leading to > 100 chars
                        state.dataIPShortCut->cAlphaArgs(6) = state.dataIPShortCut->cAlphaArgs(1) + " CONDENSER OUTLET NODE";
                    } else {
                        state.dataIPShortCut->cAlphaArgs(6) = state.dataIPShortCut->cAlphaArgs(1).substr(0, 78) + " CONDENSER OUTLET NODE";
                    }
                }

                thisChiller.CondInletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                                                   state.dataIPShortCut->cAlphaArgs(5),
                                                                                   ErrorsFound,
                                                                                   state.dataIPShortCut->cCurrentModuleObject,
                                                                                   state.dataIPShortCut->cAlphaArgs(1),
                                                                                   DataLoopNode::NodeFluidType::Air,
                                                                                   DataLoopNode::NodeConnectionType::OutsideAirReference,
                                                                                   NodeInputManager::compFluidStream::Secondary,
                                                                                   DataLoopNode::ObjectIsNotParent);
                bool Okay;
                OutAirNodeManager::CheckAndAddAirNodeNumber(state, thisChiller.CondInletNodeNum, Okay);
                if (!Okay) {
                    ShowWarningError(state,
                                     state.dataIPShortCut->cCurrentModuleObject +
                                         ", Adding OutdoorAir:DataLoopNode::Node=" + state.dataIPShortCut->cAlphaArgs(5));
                }

                thisChiller.CondOutletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                                                    state.dataIPShortCut->cAlphaArgs(6),
                                                                                    ErrorsFound,
                                                                                    state.dataIPShortCut->cCurrentModuleObject,
                                                                                    state.dataIPShortCut->cAlphaArgs(1),
                                                                                    DataLoopNode::NodeFluidType::Air,
                                                                                    DataLoopNode::NodeConnectionType::Outlet,
                                                                                    NodeInputManager::compFluidStream::Secondary,
                                                                                    DataLoopNode::ObjectIsNotParent);
            } else if (thisChiller.CondenserType == DataPlant::CondenserType::WaterCooled) {
                thisChiller.CondInletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                                                   state.dataIPShortCut->cAlphaArgs(5),
                                                                                   ErrorsFound,
                                                                                   state.dataIPShortCut->cCurrentModuleObject,
                                                                                   state.dataIPShortCut->cAlphaArgs(1),
                                                                                   DataLoopNode::NodeFluidType::Water,
                                                                                   DataLoopNode::NodeConnectionType::Inlet,
                                                                                   NodeInputManager::compFluidStream::Secondary,
                                                                                   DataLoopNode::ObjectIsNotParent);
                thisChiller.CondOutletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                                                    state.dataIPShortCut->cAlphaArgs(6),
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
                                                   state.dataIPShortCut->cAlphaArgs(5),
                                                   state.dataIPShortCut->cAlphaArgs(6),
                                                   "Condenser Water Nodes");
                // Condenser Inlet node name is necessary for Water Cooled
                if (state.dataIPShortCut->lAlphaFieldBlanks(5)) {
                    ShowSevereError(state, "Invalid, " + state.dataIPShortCut->cAlphaFieldNames(5) + " is blank ");
                    ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
                    ErrorsFound = true;
                } else if (state.dataIPShortCut->lAlphaFieldBlanks(6)) {
                    ShowSevereError(state, "Invalid, " + state.dataIPShortCut->cAlphaFieldNames(6) + " is blank ");
                    ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
                    ErrorsFound = true;
                }
            } else {
                thisChiller.CondInletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                                                   state.dataIPShortCut->cAlphaArgs(5),
                                                                                   ErrorsFound,
                                                                                   state.dataIPShortCut->cCurrentModuleObject,
                                                                                   state.dataIPShortCut->cAlphaArgs(1),
                                                                                   DataLoopNode::NodeFluidType::blank,
                                                                                   DataLoopNode::NodeConnectionType::Inlet,
                                                                                   NodeInputManager::compFluidStream::Secondary,
                                                                                   DataLoopNode::ObjectIsNotParent);
                thisChiller.CondOutletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                                                    state.dataIPShortCut->cAlphaArgs(6),
                                                                                    ErrorsFound,
                                                                                    state.dataIPShortCut->cCurrentModuleObject,
                                                                                    state.dataIPShortCut->cAlphaArgs(1),
                                                                                    DataLoopNode::NodeFluidType::blank,
                                                                                    DataLoopNode::NodeConnectionType::Outlet,
                                                                                    NodeInputManager::compFluidStream::Secondary,
                                                                                    DataLoopNode::ObjectIsNotParent);
                BranchNodeConnections::TestCompSet(state,
                                                   state.dataIPShortCut->cCurrentModuleObject,
                                                   state.dataIPShortCut->cAlphaArgs(1),
                                                   state.dataIPShortCut->cAlphaArgs(5),
                                                   state.dataIPShortCut->cAlphaArgs(6),
                                                   "Condenser (unknown?) Nodes");
                // Condenser Inlet node name is necessary for Water Cooled
                if (state.dataIPShortCut->lAlphaFieldBlanks(5)) {
                    ShowSevereError(state, "Invalid, " + state.dataIPShortCut->cAlphaFieldNames(5) + " is blank ");
                    ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
                    ErrorsFound = true;
                } else if (state.dataIPShortCut->lAlphaFieldBlanks(6)) {
                    ShowSevereError(state, "Invalid, " + state.dataIPShortCut->cAlphaFieldNames(6) + " is blank ");
                    ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
                    ErrorsFound = true;
                }
            }

            thisChiller.MinPartLoadRat = state.dataIPShortCut->rNumericArgs(3);
            thisChiller.MaxPartLoadRat = state.dataIPShortCut->rNumericArgs(4);
            thisChiller.OptPartLoadRat = state.dataIPShortCut->rNumericArgs(5);
            thisChiller.TempDesCondIn = state.dataIPShortCut->rNumericArgs(6);
            thisChiller.TempRiseCoef = state.dataIPShortCut->rNumericArgs(7);
            thisChiller.TempDesEvapOut = state.dataIPShortCut->rNumericArgs(8);
            thisChiller.EvapVolFlowRate = state.dataIPShortCut->rNumericArgs(9);
            if (thisChiller.EvapVolFlowRate == DataSizing::AutoSize) {
                thisChiller.EvapVolFlowRateWasAutoSized = true;
            }
            thisChiller.CondVolFlowRate = state.dataIPShortCut->rNumericArgs(10);
            if (thisChiller.CondVolFlowRate == DataSizing::AutoSize) {
                if (thisChiller.CondenserType == DataPlant::CondenserType::WaterCooled) {
                    thisChiller.CondVolFlowRateWasAutoSized = true;
                }
            }
            thisChiller.CapRatCoef(1) = state.dataIPShortCut->rNumericArgs(11);
            thisChiller.CapRatCoef(2) = state.dataIPShortCut->rNumericArgs(12);
            thisChiller.CapRatCoef(3) = state.dataIPShortCut->rNumericArgs(13);
            if ((state.dataIPShortCut->rNumericArgs(11) + state.dataIPShortCut->rNumericArgs(12) + state.dataIPShortCut->rNumericArgs(13)) == 0.0) {
                ShowSevereError(state,
                                state.dataIPShortCut->cCurrentModuleObject +
                                    ": Sum of Capacity Ratio Coef = 0.0, chiller=" + state.dataIPShortCut->cAlphaArgs(1));
                ErrorsFound = true;
            }
            thisChiller.PowerRatCoef(1) = state.dataIPShortCut->rNumericArgs(14);
            thisChiller.PowerRatCoef(2) = state.dataIPShortCut->rNumericArgs(15);
            thisChiller.PowerRatCoef(3) = state.dataIPShortCut->rNumericArgs(16);
            thisChiller.FullLoadCoef(1) = state.dataIPShortCut->rNumericArgs(17);
            thisChiller.FullLoadCoef(2) = state.dataIPShortCut->rNumericArgs(18);
            thisChiller.FullLoadCoef(3) = state.dataIPShortCut->rNumericArgs(19);
            thisChiller.TempLowLimitEvapOut = state.dataIPShortCut->rNumericArgs(20);

            // Load Special EngineDriven Chiller Curve Fit Inputs
            thisChiller.ClngLoadtoFuelCurve = CurveManager::GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(7)); // convert curve name to number
            if (thisChiller.ClngLoadtoFuelCurve == 0) {
                ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(7) + '=' + state.dataIPShortCut->cAlphaArgs(7));
                ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
                ErrorsFound = true;
            }

            thisChiller.RecJacHeattoFuelCurve =
                CurveManager::GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(8)); // convert curve name to number
            if (thisChiller.RecJacHeattoFuelCurve == 0) {
                ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(8) + '=' + state.dataIPShortCut->cAlphaArgs(8));
                ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
                ErrorsFound = true;
            }

            thisChiller.RecLubeHeattoFuelCurve =
                CurveManager::GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(9)); // convert curve name to number
            if (thisChiller.RecLubeHeattoFuelCurve == 0) {
                ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(9) + '=' + state.dataIPShortCut->cAlphaArgs(9));
                ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
                ErrorsFound = true;
            }

            thisChiller.TotExhausttoFuelCurve =
                CurveManager::GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(10)); // convert curve name to number
            if (thisChiller.TotExhausttoFuelCurve == 0) {
                ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(10) + '=' + state.dataIPShortCut->cAlphaArgs(10));
                ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
                ErrorsFound = true;
            }

            thisChiller.ExhaustTempCurve = CurveManager::GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(11)); // convert curve name to number
            if (thisChiller.ExhaustTempCurve == 0) {
                ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(11) + '=' + state.dataIPShortCut->cAlphaArgs(11));
                ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
                ErrorsFound = true;
            }

            thisChiller.UACoef(1) = state.dataIPShortCut->rNumericArgs(21);
            thisChiller.UACoef(2) = state.dataIPShortCut->rNumericArgs(22);

            thisChiller.MaxExhaustperPowerOutput = state.dataIPShortCut->rNumericArgs(23);
            thisChiller.DesignMinExitGasTemp = state.dataIPShortCut->rNumericArgs(24);

            // Validate fuel type input
            bool FuelTypeError(false);
            UtilityRoutines::ValidateFuelType(state, state.dataIPShortCut->cAlphaArgs(12), thisChiller.FuelType, FuelTypeError);
            if (FuelTypeError) {
                ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(12) + '=' + state.dataIPShortCut->cAlphaArgs(12));
                ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
                ShowContinueError(
                    state, "Valid choices are Electricity, NaturalGas, Propane, Diesel, Gasoline, FuelOilNo1, FuelOilNo2,OtherFuel1 or OtherFuel2");
                ErrorsFound = true;
                FuelTypeError = false;
            }

            thisChiller.FuelHeatingValue = state.dataIPShortCut->rNumericArgs(25);

            // add support of autosize to this.

            thisChiller.DesignHeatRecVolFlowRate = state.dataIPShortCut->rNumericArgs(26);
            if (thisChiller.DesignHeatRecVolFlowRate > 0.0 || thisChiller.DesignHeatRecVolFlowRate == DataSizing::AutoSize) {
                thisChiller.HeatRecActive = true;
                thisChiller.HeatRecInletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                                                      state.dataIPShortCut->cAlphaArgs(13),
                                                                                      ErrorsFound,
                                                                                      state.dataIPShortCut->cCurrentModuleObject,
                                                                                      state.dataIPShortCut->cAlphaArgs(1),
                                                                                      DataLoopNode::NodeFluidType::Water,
                                                                                      DataLoopNode::NodeConnectionType::Inlet,
                                                                                      NodeInputManager::compFluidStream::Tertiary,
                                                                                      DataLoopNode::ObjectIsNotParent);
                if (thisChiller.HeatRecInletNodeNum == 0) {
                    ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(13) + '=' + state.dataIPShortCut->cAlphaArgs(13));
                    ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
                    ErrorsFound = true;
                }
                thisChiller.HeatRecOutletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                                                       state.dataIPShortCut->cAlphaArgs(14),
                                                                                       ErrorsFound,
                                                                                       state.dataIPShortCut->cCurrentModuleObject,
                                                                                       state.dataIPShortCut->cAlphaArgs(1),
                                                                                       DataLoopNode::NodeFluidType::Water,
                                                                                       DataLoopNode::NodeConnectionType::Outlet,
                                                                                       NodeInputManager::compFluidStream::Tertiary,
                                                                                       DataLoopNode::ObjectIsNotParent);
                if (thisChiller.HeatRecOutletNodeNum == 0) {
                    ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(14) + '=' + state.dataIPShortCut->cAlphaArgs(14));
                    ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
                    ErrorsFound = true;
                }
                BranchNodeConnections::TestCompSet(state,
                                                   state.dataIPShortCut->cCurrentModuleObject,
                                                   state.dataIPShortCut->cAlphaArgs(1),
                                                   state.dataIPShortCut->cAlphaArgs(13),
                                                   state.dataIPShortCut->cAlphaArgs(14),
                                                   "Heat Recovery Nodes");
                if (thisChiller.DesignHeatRecVolFlowRate == DataSizing::AutoSize) {
                    thisChiller.DesignHeatRecVolFlowRateWasAutoSized = true;
                } else {
                    PlantUtilities::RegisterPlantCompDesignFlow(state, thisChiller.HeatRecInletNodeNum, thisChiller.DesignHeatRecVolFlowRate);
                }

                // Condenser flow rate must be specified for heat reclaim
                if (thisChiller.CondenserType == DataPlant::CondenserType::AirCooled ||
                    thisChiller.CondenserType == DataPlant::CondenserType::EvapCooled) {
                    if (thisChiller.CondVolFlowRate <= 0.0) {
                        ShowSevereError(
                            state, format("Invalid {}={:.6R}", state.dataIPShortCut->cNumericFieldNames(10), state.dataIPShortCut->rNumericArgs(10)));
                        ShowSevereError(state, "Condenser fluid flow rate must be specified for Heat Reclaim applications.");
                        ShowContinueError(state,
                                          "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
                        ErrorsFound = true;
                    }
                }

            } else {

                thisChiller.HeatRecActive = false;
                thisChiller.DesignHeatRecMassFlowRate = 0.0;
                thisChiller.HeatRecInletNodeNum = 0;
                thisChiller.HeatRecOutletNodeNum = 0;
                // if heat recovery is not used, don't care about condenser flow rate for air/evap-cooled equip.
                if (thisChiller.CondenserType == DataPlant::CondenserType::AirCooled ||
                    thisChiller.CondenserType == DataPlant::CondenserType::EvapCooled) {
                    thisChiller.CondVolFlowRate = 0.0011; // set to avoid errors in calc routine
                }
                if ((!state.dataIPShortCut->lAlphaFieldBlanks(13)) || (!state.dataIPShortCut->lAlphaFieldBlanks(14))) {
                    ShowWarningError(state,
                                     "Since Design Heat Flow Rate = 0.0, Heat Recovery inactive for " + state.dataIPShortCut->cCurrentModuleObject +
                                         '=' + state.dataIPShortCut->cAlphaArgs(1));
                    ShowContinueError(state, "However, Node names were specified for Heat Recovery inlet or outlet nodes");
                }
            }

            {
                auto const SELECT_CASE_var(state.dataIPShortCut->cAlphaArgs(15));
                if (SELECT_CASE_var == "CONSTANTFLOW") {
                    thisChiller.FlowMode = DataPlant::FlowMode::Constant;
                } else if (SELECT_CASE_var == "LEAVINGSETPOINTMODULATED") {
                    thisChiller.FlowMode = DataPlant::FlowMode::LeavingSetpointModulated;
                } else if (SELECT_CASE_var == "NOTMODULATED") {
                    thisChiller.FlowMode = DataPlant::FlowMode::NotModulated;
                } else {
                    ShowSevereError(state,
                                    std::string{RoutineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" +
                                        state.dataIPShortCut->cAlphaArgs(1) + "\",");
                    ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(15) + '=' + state.dataIPShortCut->cAlphaArgs(15));
                    ShowContinueError(state, "Available choices are ConstantFlow, NotModulated, or LeavingSetpointModulated");
                    ShowContinueError(state, "Flow mode NotModulated is assumed and the simulation continues.");
                    thisChiller.FlowMode = DataPlant::FlowMode::NotModulated;
                }
            }

            thisChiller.HeatRecMaxTemp = state.dataIPShortCut->rNumericArgs(27);
            thisChiller.SizFac = state.dataIPShortCut->rNumericArgs(28);
            if (thisChiller.SizFac <= 0.0) thisChiller.SizFac = 1.0;

            //   Basin heater power as a function of temperature must be greater than or equal to 0
            thisChiller.BasinHeaterPowerFTempDiff = state.dataIPShortCut->rNumericArgs(29);
            if (state.dataIPShortCut->rNumericArgs(29) < 0.0) {
                ShowSevereError(state,
                                state.dataIPShortCut->cCurrentModuleObject + ", \"" + thisChiller.Name +
                                    "\" TRIM(state.dataIPShortCut->cNumericFieldNames(29)) must be >= 0");
                ErrorsFound = true;
            }

            thisChiller.BasinHeaterSetPointTemp = state.dataIPShortCut->rNumericArgs(30);

            if (thisChiller.BasinHeaterPowerFTempDiff > 0.0) {
                if (NumNums < 30) {
                    thisChiller.BasinHeaterSetPointTemp = 2.0;
                }
                if (thisChiller.BasinHeaterSetPointTemp < 2.0) {
                    ShowWarningError(state,
                                     state.dataIPShortCut->cCurrentModuleObject + ":\"" + thisChiller.Name + "\", " +
                                         state.dataIPShortCut->cNumericFieldNames(30) + " is less than 2 deg C. Freezing could occur.");
                }
            }

            if (!state.dataIPShortCut->lAlphaFieldBlanks(16)) {
                thisChiller.BasinHeaterSchedulePtr = ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(16));
                if (thisChiller.BasinHeaterSchedulePtr == 0) {
                    ShowWarningError(state,
                                     state.dataIPShortCut->cCurrentModuleObject + ", \"" + thisChiller.Name +
                                         "\" TRIM(state.dataIPShortCut->cAlphaFieldNames(16)) \"" + state.dataIPShortCut->cAlphaArgs(16) +
                                         "\" was not found. Basin heater operation will not be modeled and the simulation continues");
                }
            }

            if (NumNums > 30) {
                if (!state.dataIPShortCut->lNumericFieldBlanks(31)) {
                    thisChiller.HeatRecCapacityFraction = state.dataIPShortCut->rNumericArgs(31);
                } else {
                    thisChiller.HeatRecCapacityFraction = 1.0;
                }
            } else {
                thisChiller.HeatRecCapacityFraction = 1.0;
            }
        }

        if (ErrorsFound) {
            ShowFatalError(state, "Errors found in processing input for " + state.dataIPShortCut->cCurrentModuleObject);
        }
    }

    void EngineDrivenChillerSpecs::setupOutputVariables(EnergyPlusData &state)
    {
        SetupOutputVariable(state,
                            "Chiller Drive Shaft Power",
                            OutputProcessor::Unit::W,
                            this->Power,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);
        SetupOutputVariable(state,
                            "Chiller Drive Shaft Energy",
                            OutputProcessor::Unit::J,
                            this->Energy,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            this->Name);
        SetupOutputVariable(state,
                            "Chiller Evaporator Cooling Rate",
                            OutputProcessor::Unit::W,
                            this->QEvaporator,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);
        SetupOutputVariable(state,
                            "Chiller Evaporator Cooling Energy",
                            OutputProcessor::Unit::J,
                            this->EvaporatorEnergy,
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
                            this->EvapInletTemp,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);
        SetupOutputVariable(state,
                            "Chiller Evaporator Outlet Temperature",
                            OutputProcessor::Unit::C,
                            this->EvapOutletTemp,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);
        SetupOutputVariable(state,
                            "Chiller Evaporator Mass Flow Rate",
                            OutputProcessor::Unit::kg_s,
                            this->EvapMassFlowRate,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);
        SetupOutputVariable(state,
                            "Chiller Condenser Heat Transfer Rate",
                            OutputProcessor::Unit::W,
                            this->QCondenser,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);
        SetupOutputVariable(state,
                            "Chiller Condenser Heat Transfer Energy",
                            OutputProcessor::Unit::J,
                            this->CondenserEnergy,
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
                            this->CondInletTemp,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);

        // Condenser mass flow and outlet temp are valid for Water Cooled
        if (this->CondenserType == DataPlant::CondenserType::WaterCooled) {
            SetupOutputVariable(state,
                                "Chiller Condenser Outlet Temperature",
                                OutputProcessor::Unit::C,
                                this->CondOutletTemp,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                this->Name);
            SetupOutputVariable(state,
                                "Chiller Condenser Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                this->CondMassFlowRate,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                this->Name);
        } else if (this->CondenserType == DataPlant::CondenserType::AirCooled) {
        } else if (this->CondenserType == DataPlant::CondenserType::EvapCooled) {
            if (this->BasinHeaterPowerFTempDiff > 0.0) {
                SetupOutputVariable(state,
                                    "Chiller Basin Heater Electricity Rate",
                                    OutputProcessor::Unit::W,
                                    this->BasinHeaterPower,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    this->Name);
                SetupOutputVariable(state,
                                    "Chiller Basin Heater Electricity Energy",
                                    OutputProcessor::Unit::J,
                                    this->BasinHeaterConsumption,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    this->Name,
                                    _,
                                    "Electricity",
                                    "CHILLERS",
                                    _,
                                    "Plant");
            }
        }

        SetupOutputVariable(state,
                            "Chiller " + this->FuelType + " Rate",
                            OutputProcessor::Unit::W,
                            this->FuelEnergyUseRate,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);
        SetupOutputVariable(state,
                            "Chiller " + this->FuelType + " Energy",
                            OutputProcessor::Unit::J,
                            this->FuelEnergy,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            this->Name,
                            _,
                            this->FuelType,
                            "Cooling",
                            _,
                            "Plant");

        SetupOutputVariable(state,
                            "Chiller COP",
                            OutputProcessor::Unit::W_W,
                            this->FuelCOP,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);
        SetupOutputVariable(state,
                            "Chiller " + this->FuelType + " Mass Flow Rate",
                            OutputProcessor::Unit::kg_s,
                            this->FuelMdot,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);
        SetupOutputVariable(state,
                            "Chiller Exhaust Temperature",
                            OutputProcessor::Unit::C,
                            this->ExhaustStackTemp,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);
        SetupOutputVariable(state,
                            "Chiller Heat Recovery Mass Flow Rate",
                            OutputProcessor::Unit::kg_s,
                            this->HeatRecMdot,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);

        if (this->HeatRecActive) {
            // need to only report if heat recovery active
            SetupOutputVariable(state,
                                "Chiller Jacket Recovered Heat Rate",
                                OutputProcessor::Unit::W,
                                this->QJacketRecovered,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                this->Name);
            SetupOutputVariable(state,
                                "Chiller Jacket Recovered Heat Energy",
                                OutputProcessor::Unit::J,
                                this->JacketEnergyRec,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                this->Name,
                                _,
                                "ENERGYTRANSFER",
                                "HEATRECOVERY",
                                _,
                                "Plant");

            SetupOutputVariable(state,
                                "Chiller Lube Recovered Heat Rate",
                                OutputProcessor::Unit::W,
                                this->QLubeOilRecovered,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                this->Name);
            SetupOutputVariable(state,
                                "Chiller Lube Recovered Heat Energy",
                                OutputProcessor::Unit::J,
                                this->LubeOilEnergyRec,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                this->Name,
                                _,
                                "ENERGYTRANSFER",
                                "HEATRECOVERY",
                                _,
                                "Plant");

            SetupOutputVariable(state,
                                "Chiller Exhaust Recovered Heat Rate",
                                OutputProcessor::Unit::W,
                                this->QExhaustRecovered,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                this->Name);
            SetupOutputVariable(state,
                                "Chiller Exhaust Recovered Heat Energy",
                                OutputProcessor::Unit::J,
                                this->ExhaustEnergyRec,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                this->Name,
                                _,
                                "ENERGYTRANSFER",
                                "HEATRECOVERY",
                                _,
                                "Plant");

            SetupOutputVariable(state,
                                "Chiller Total Recovered Heat Rate",
                                OutputProcessor::Unit::W,
                                this->QTotalHeatRecovered,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                this->Name);
            SetupOutputVariable(state,
                                "Chiller Total Recovered Heat Energy",
                                OutputProcessor::Unit::J,
                                this->TotalHeatEnergyRec,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                this->Name);
            SetupOutputVariable(state,
                                "Chiller Heat Recovery Inlet Temperature",
                                OutputProcessor::Unit::C,
                                this->HeatRecInletTemp,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                this->Name);
            SetupOutputVariable(state,
                                "Chiller Heat Recovery Outlet Temperature",
                                OutputProcessor::Unit::C,
                                this->HeatRecOutletTemp,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                this->Name);
        }
        if (state.dataGlobal->AnyEnergyManagementSystemInModel) {
            SetupEMSInternalVariable(state, "Chiller Nominal Capacity", this->Name, "[W]", this->NomCap);
        }
    }

    void EngineDrivenChillerSpecs::initialize(EnergyPlusData &state, bool const RunFlag, Real64 const MyLoad)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   June 2002
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for initializations of the Engine Driven Chiller components

        // METHODOLOGY EMPLOYED:
        // Uses the status flags to trigger initializations.

        // SUBROUTINE PARAMETER DEFINITIONS:
        static constexpr std::string_view RoutineName("InitEngineDrivenChiller");

        this->oneTimeInit(state);

        // Initialize critical Demand Side Variables
        if (this->MyEnvrnFlag && state.dataGlobal->BeginEnvrnFlag && (state.dataPlnt->PlantFirstSizesOkayToFinalize)) {

            Real64 rho = FluidProperties::GetDensityGlycol(state,
                                                           state.dataPlnt->PlantLoop(this->CWLoopNum).FluidName,
                                                           DataGlobalConstants::CWInitConvTemp,
                                                           state.dataPlnt->PlantLoop(this->CWLoopNum).FluidIndex,
                                                           RoutineName);

            this->EvapMassFlowRateMax = rho * this->EvapVolFlowRate;
            PlantUtilities::InitComponentNodes(state,
                                               0.0,
                                               this->EvapMassFlowRateMax,
                                               this->EvapInletNodeNum,
                                               this->EvapOutletNodeNum,
                                               this->CWLoopNum,
                                               this->CWLoopSideNum,
                                               this->CWBranchNum,
                                               this->CWCompNum);

            // init maximum available condenser flow rate

            if (this->CondenserType == DataPlant::CondenserType::WaterCooled) {

                state.dataLoopNodes->Node(this->CondInletNodeNum).Temp = this->TempDesCondIn;

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
            } else { // air or evap-air
                state.dataLoopNodes->Node(this->CondInletNodeNum).MassFlowRate =
                    this->CondVolFlowRate *
                    Psychrometrics::PsyRhoAirFnPbTdbW(state, state.dataEnvrn->StdBaroPress, this->TempDesCondIn, 0.0, RoutineName);

                state.dataLoopNodes->Node(this->CondOutletNodeNum).MassFlowRate = state.dataLoopNodes->Node(this->CondInletNodeNum).MassFlowRate;
                state.dataLoopNodes->Node(this->CondInletNodeNum).MassFlowRateMaxAvail =
                    state.dataLoopNodes->Node(this->CondInletNodeNum).MassFlowRate;
                state.dataLoopNodes->Node(this->CondInletNodeNum).MassFlowRateMax = state.dataLoopNodes->Node(this->CondInletNodeNum).MassFlowRate;
                state.dataLoopNodes->Node(this->CondOutletNodeNum).MassFlowRateMax = state.dataLoopNodes->Node(this->CondInletNodeNum).MassFlowRate;
                state.dataLoopNodes->Node(this->CondInletNodeNum).MassFlowRateMinAvail = 0.0;
                state.dataLoopNodes->Node(this->CondInletNodeNum).MassFlowRateMin = 0.0;
                state.dataLoopNodes->Node(this->CondOutletNodeNum).MassFlowRateMinAvail = 0.0;
                state.dataLoopNodes->Node(this->CondOutletNodeNum).MassFlowRateMin = 0.0;
            }

            if (this->HeatRecActive) {
                rho = FluidProperties::GetDensityGlycol(state,
                                                        state.dataPlnt->PlantLoop(this->HRLoopNum).FluidName,
                                                        DataGlobalConstants::HWInitConvTemp,
                                                        state.dataPlnt->PlantLoop(this->HRLoopNum).FluidIndex,
                                                        RoutineName);
                this->DesignHeatRecMassFlowRate = rho * this->DesignHeatRecVolFlowRate;

                PlantUtilities::InitComponentNodes(state,
                                                   0.0,
                                                   this->DesignHeatRecMassFlowRate,
                                                   this->HeatRecInletNodeNum,
                                                   this->HeatRecOutletNodeNum,
                                                   this->HRLoopNum,
                                                   this->HRLoopSideNum,
                                                   this->HRBranchNum,
                                                   this->HRCompNum);
            }

            this->MyEnvrnFlag = false;
        }

        if (!state.dataGlobal->BeginEnvrnFlag) {
            this->MyEnvrnFlag = true;
        }

        if ((this->FlowMode == DataPlant::FlowMode::LeavingSetpointModulated) && (this->ModulatedFlowSetToLoop)) {
            // fix for clumsy old input that worked because loop setpoint was spread.
            //  could be removed with transition, testing , model change, period of being obsolete.
            state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempSetPoint =
                state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPoint;
            state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempSetPointHi =
                state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPointHi;
        }

        Real64 mdot;
        Real64 mdotCond;
        if ((std::abs(MyLoad) > 0.0) && RunFlag) {
            mdot = this->EvapMassFlowRateMax;
            mdotCond = this->CondMassFlowRateMax;
        } else {
            mdot = 0.0;
            mdotCond = 0.0;
        }

        PlantUtilities::SetComponentFlowRate(
            state, mdot, this->EvapInletNodeNum, this->EvapOutletNodeNum, this->CWLoopNum, this->CWLoopSideNum, this->CWBranchNum, this->CWCompNum);
        if (this->CondenserType == DataPlant::CondenserType::WaterCooled) {
            PlantUtilities::SetComponentFlowRate(state,
                                                 mdotCond,
                                                 this->CondInletNodeNum,
                                                 this->CondOutletNodeNum,
                                                 this->CDLoopNum,
                                                 this->CDLoopSideNum,
                                                 this->CDBranchNum,
                                                 this->CDCompNum);
        }

        // Initialize heat recovery flow rates at node
        if (this->HeatRecActive) {

            if (RunFlag) {
                mdot = this->DesignHeatRecMassFlowRate;
            } else {
                mdot = 0.0;
            }

            PlantUtilities::SetComponentFlowRate(state,
                                                 mdot,
                                                 this->HeatRecInletNodeNum,
                                                 this->HeatRecOutletNodeNum,
                                                 this->HRLoopNum,
                                                 this->HRLoopSideNum,
                                                 this->HRBranchNum,
                                                 this->HRCompNum);
        }
        if (this->CondenserType == DataPlant::CondenserType::EvapCooled) {
            this->BasinHeaterPower = 0.0;
        }
    }

    void EngineDrivenChillerSpecs::size(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   June 2002
        //       MODIFIED       November 2013 Daeho Kang, add component sizing table entries
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for sizing Engine Driven Chiller Components for which capacities and flow rates
        // have not been specified in the input.

        // METHODOLOGY EMPLOYED:
        // Obtains evaporator flow rate from the plant sizing array. Calculates nominal capacity from
        // the evaporator flow rate and the chilled water loop design delta T. The condenser flow rate
        // is calculated from the nominal capacity, the COP, and the condenser loop design delta T.

        // SUBROUTINE PARAMETER DEFINITIONS:
        static constexpr std::string_view RoutineName("SizeEngineDrivenChiller");

        int PltSizCondNum = 0;
        bool ErrorsFound = false;
        Real64 tmpNomCap = this->NomCap;
        Real64 tmpEvapVolFlowRate = this->EvapVolFlowRate;
        Real64 tmpCondVolFlowRate = this->CondVolFlowRate;

        if (this->CondenserType == DataPlant::CondenserType::WaterCooled) {
            PltSizCondNum = state.dataPlnt->PlantLoop(this->CDLoopNum).PlantSizNum;
        }

        int PltSizNum = state.dataPlnt->PlantLoop(this->CWLoopNum).PlantSizNum;

        if (PltSizNum > 0) {
            if (state.dataSize->PlantSizData(PltSizNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                Real64 rho = FluidProperties::GetDensityGlycol(state,
                                                               state.dataPlnt->PlantLoop(this->CWLoopNum).FluidName,
                                                               DataGlobalConstants::CWInitConvTemp,
                                                               state.dataPlnt->PlantLoop(this->CWLoopNum).FluidIndex,
                                                               RoutineName);
                Real64 Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                                   state.dataPlnt->PlantLoop(this->CWLoopNum).FluidName,
                                                                   DataGlobalConstants::CWInitConvTemp,
                                                                   state.dataPlnt->PlantLoop(this->CWLoopNum).FluidIndex,
                                                                   RoutineName);
                tmpNomCap =
                    Cp * rho * state.dataSize->PlantSizData(PltSizNum).DeltaT * state.dataSize->PlantSizData(PltSizNum).DesVolFlowRate * this->SizFac;
            } else {
                if (this->NomCapWasAutoSized) tmpNomCap = 0.0;
            }
            if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                if (this->NomCapWasAutoSized) {
                    this->NomCap = tmpNomCap;
                    if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(state, "Chiller:EngineDriven", this->Name, "Design Size Nominal Capacity [W]", tmpNomCap);
                    }
                    if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(
                            state, "Chiller:EngineDriven", this->Name, "Initial Design Size Nominal Capacity [W]", tmpNomCap);
                    }
                } else {
                    if (this->NomCap > 0.0 && tmpNomCap > 0.0) {
                        if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                            BaseSizer::reportSizerOutput(state,
                                                         "Chiller:EngineDriven",
                                                         this->Name,
                                                         "Design Size Nominal Capacity [W]",
                                                         tmpNomCap,
                                                         "User-Specified Nominal Capacity [W]",
                                                         this->NomCap);
                            if (state.dataGlobal->DisplayExtraWarnings) {
                                if ((std::abs(tmpNomCap - this->NomCap) / this->NomCap) > state.dataSize->AutoVsHardSizingThreshold) {
                                    ShowMessage(state, "SizeChillerEngineDriven: Potential issue with equipment sizing for " + this->Name);
                                    ShowContinueError(state, format("User-Specified Nominal Capacity of {:.2R} [W]", this->NomCap));
                                    ShowContinueError(state, format("differs from Design Size Nominal Capacity of {:.2R} [W]", tmpNomCap));
                                    ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                        }
                        tmpNomCap = this->NomCap;
                    }
                }
            }
        } else {
            if (this->NomCapWasAutoSized && state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                ShowSevereError(state, "Autosizing of Engine Driven Chiller nominal capacity requires a loop Sizing:Plant object");
                ShowContinueError(state, "Occurs in Engine Driven Chiller object=" + this->Name);
                ErrorsFound = true;
            }
            if (!this->NomCapWasAutoSized && state.dataPlnt->PlantFinalSizesOkayToReport && (this->NomCap > 0.0)) {
                BaseSizer::reportSizerOutput(state, "Chiller:EngineDriven", this->Name, "User-Specified Nominal Capacity [W]", this->NomCap);
            }
        }

        if (PltSizNum > 0) {
            if (state.dataSize->PlantSizData(PltSizNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                tmpEvapVolFlowRate = state.dataSize->PlantSizData(PltSizNum).DesVolFlowRate * this->SizFac;
            } else {
                if (this->EvapVolFlowRateWasAutoSized) tmpEvapVolFlowRate = 0.0;
            }
            if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                if (this->EvapVolFlowRateWasAutoSized) {
                    this->EvapVolFlowRate = tmpEvapVolFlowRate;
                    if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(
                            state, "Chiller:EngineDriven", this->Name, "Design Chilled Water Flow Rate [m3/s]", tmpEvapVolFlowRate);
                    }
                    if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(
                            state, "Chiller:EngineDriven", this->Name, "Initial Design Chilled Water Flow Rate [m3/s]", tmpEvapVolFlowRate);
                    }
                } else {
                    if (this->EvapVolFlowRate > 0.0 && tmpEvapVolFlowRate > 0.0) {
                        if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                            BaseSizer::reportSizerOutput(state,
                                                         "Chiller:EngineDriven",
                                                         this->Name,
                                                         "Design Size Design Chilled Water Flow Rate [m3/s]",
                                                         tmpEvapVolFlowRate,
                                                         "User-Specified Design Chilled Water Flow Rate [m3/s]",
                                                         this->EvapVolFlowRate);
                            if (state.dataGlobal->DisplayExtraWarnings) {
                                if ((std::abs(tmpEvapVolFlowRate - this->EvapVolFlowRate) / this->EvapVolFlowRate) >
                                    state.dataSize->AutoVsHardSizingThreshold) {
                                    ShowMessage(state, "SizeChillerEngineDriven: Potential issue with equipment sizing for " + this->Name);
                                    ShowContinueError(
                                        state, format("User-Specified Design Chilled Water Flow Rate of {:.5R} [m3/s]", this->EvapVolFlowRate));
                                    ShowContinueError(
                                        state,
                                        format("differs from Design Size Design Chilled Water Flow Rate of {:.5R} [m3/s]", tmpEvapVolFlowRate));
                                    ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                        }
                        tmpEvapVolFlowRate = this->EvapVolFlowRate;
                    }
                }
            }
        } else {
            if (this->EvapVolFlowRateWasAutoSized && state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                ShowSevereError(state, "Autosizing of Engine Driven Chiller evap flow rate requires a loop Sizing:Plant object");
                ShowContinueError(state, "Occurs in Engine Driven Chiller object=" + this->Name);
                ErrorsFound = true;
            }
            if (!this->EvapVolFlowRateWasAutoSized && state.dataPlnt->PlantFinalSizesOkayToReport && (this->EvapVolFlowRate > 0.0)) {
                BaseSizer::reportSizerOutput(
                    state, "Chiller:EngineDriven", this->Name, "User-Specified Design Chilled Water Flow Rate [m3/s]", this->EvapVolFlowRate);
            }
        }

        PlantUtilities::RegisterPlantCompDesignFlow(state, this->EvapInletNodeNum, tmpEvapVolFlowRate);

        if (PltSizCondNum > 0 && PltSizNum > 0) {
            if (state.dataSize->PlantSizData(PltSizNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow && tmpNomCap > 0.0) {
                Real64 rho = FluidProperties::GetDensityGlycol(state,
                                                               state.dataPlnt->PlantLoop(this->CDLoopNum).FluidName,
                                                               this->TempDesCondIn,
                                                               state.dataPlnt->PlantLoop(this->CDLoopNum).FluidIndex,
                                                               RoutineName);

                Real64 Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                                   state.dataPlnt->PlantLoop(this->CDLoopNum).FluidName,
                                                                   this->TempDesCondIn,
                                                                   state.dataPlnt->PlantLoop(this->CDLoopNum).FluidIndex,
                                                                   RoutineName);
                tmpCondVolFlowRate = tmpNomCap * (1.0 + 1.0 / this->COP) / (state.dataSize->PlantSizData(PltSizCondNum).DeltaT * Cp * rho);
            } else {
                if (this->CondVolFlowRateWasAutoSized) tmpCondVolFlowRate = 0.0;
            }
            if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                if (this->CondVolFlowRateWasAutoSized) {
                    this->CondVolFlowRate = tmpCondVolFlowRate;
                    if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(
                            state, "Chiller:EngineDriven", this->Name, "Design Size Design Condenser Water Flow Rate [m3/s]", tmpCondVolFlowRate);
                    }
                    if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(state,
                                                     "Chiller:EngineDriven",
                                                     this->Name,
                                                     "Initial Design Size Design Condenser Water Flow Rate [m3/s]",
                                                     tmpCondVolFlowRate);
                    }
                } else {
                    if (this->CondVolFlowRate > 0.0 && tmpCondVolFlowRate > 0.0) {
                        if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                            BaseSizer::reportSizerOutput(state,
                                                         "Chiller:EngineDriven",
                                                         this->Name,
                                                         "Design Size Design Condenser Water Flow Rate [m3/s]",
                                                         tmpCondVolFlowRate,
                                                         "User-Specified Design Condenser Water Flow Rate [m3/s]",
                                                         this->CondVolFlowRate);
                            if (state.dataGlobal->DisplayExtraWarnings) {
                                if ((std::abs(tmpCondVolFlowRate - this->CondVolFlowRate) / this->CondVolFlowRate) >
                                    state.dataSize->AutoVsHardSizingThreshold) {
                                    ShowMessage(state, "SizeChillerEngineDriven: Potential issue with equipment sizing for " + this->Name);
                                    ShowContinueError(
                                        state, format("User-Specified Design Condenser Water Flow Rate of {:.5R} [m3/s]", this->CondVolFlowRate));
                                    ShowContinueError(
                                        state,
                                        format("differs from Design Size Design Condenser Water Flow Rate of {:.5R} [m3/s]", tmpCondVolFlowRate));
                                    ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                        }
                        tmpCondVolFlowRate = this->CondVolFlowRate;
                    }
                }
            }
        } else {
            if (this->CondVolFlowRateWasAutoSized && state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                ShowSevereError(state, "Autosizing of EngineDriven Chiller condenser flow rate requires a condenser");
                ShowContinueError(state, "loop Sizing:Plant object");
                ShowContinueError(state, "Occurs in EngineDriven Chiller object=" + this->Name);
                ErrorsFound = true;
            }
            if (!this->CondVolFlowRateWasAutoSized && state.dataPlnt->PlantFinalSizesOkayToReport && (this->CondVolFlowRate > 0.0)) {
                BaseSizer::reportSizerOutput(
                    state, "Chiller:EngineDriven", this->Name, "User-Specified Design Condenser Water Flow Rate [m3/s]", this->CondVolFlowRate);
            }
        }

        // save the design condenser water volumetric flow rate for use by the condenser water loop sizing algorithms
        if (this->CondenserType == DataPlant::CondenserType::WaterCooled) {
            PlantUtilities::RegisterPlantCompDesignFlow(state, this->CondInletNodeNum, tmpCondVolFlowRate);
        }

        // autosize support for heat recovery flow rate.
        if (this->HeatRecActive) {
            Real64 tmpHeatRecVolFlowRate = tmpCondVolFlowRate * this->HeatRecCapacityFraction;
            if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                if (this->DesignHeatRecVolFlowRateWasAutoSized) {
                    this->DesignHeatRecVolFlowRate = tmpHeatRecVolFlowRate;
                    if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(state,
                                                     "Chiller:EngineDriven",
                                                     this->Name,
                                                     "Design Size Design Heat Recovery Fluid Flow Rate [m3/s]",
                                                     tmpHeatRecVolFlowRate);
                    }
                    if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(state,
                                                     "Chiller:EngineDriven",
                                                     this->Name,
                                                     "Initial Design Size Design Heat Recovery Fluid Flow Rate [m3/s]",
                                                     tmpHeatRecVolFlowRate);
                    }
                } else {
                    if (this->DesignHeatRecVolFlowRate > 0.0 && tmpHeatRecVolFlowRate > 0.0) {
                        Real64 DesignHeatRecVolFlowRateUser = this->DesignHeatRecVolFlowRate;
                        if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                            if (state.dataGlobal->DoPlantSizing) {
                                BaseSizer::reportSizerOutput(state,
                                                             "Chiller:EngineDriven",
                                                             this->Name,
                                                             "Design Size Design Heat Recovery Fluid Flow Rate [m3/s]",
                                                             tmpHeatRecVolFlowRate,
                                                             "User-Specified Design Heat Recovery Fluid Flow Rate [m3/s]",
                                                             DesignHeatRecVolFlowRateUser);
                            } else {
                                BaseSizer::reportSizerOutput(state,
                                                             "Chiller:EngineDriven",
                                                             this->Name,
                                                             "User-Specified Design Heat Recovery Fluid Flow Rate [m3/s]",
                                                             DesignHeatRecVolFlowRateUser);
                            }
                            if (state.dataGlobal->DisplayExtraWarnings) {
                                if ((std::abs(tmpHeatRecVolFlowRate - DesignHeatRecVolFlowRateUser) / DesignHeatRecVolFlowRateUser) >
                                    state.dataSize->AutoVsHardSizingThreshold) {
                                    ShowMessage(state, "SizeEngineDrivenChiller: Potential issue with equipment sizing for " + this->Name);
                                    ShowContinueError(
                                        state,
                                        format("User-Specified Design Heat Recovery Fluid Flow Rate of {:.5R} [m3/s]", DesignHeatRecVolFlowRateUser));
                                    ShowContinueError(state,
                                                      format("differs from Design Size Design Heat Recovery Fluid Flow Rate of {:.5R} [m3/s]",
                                                             tmpHeatRecVolFlowRate));
                                    ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                        }
                        tmpHeatRecVolFlowRate = DesignHeatRecVolFlowRateUser;
                    }
                }
            }
            if (!this->DesignHeatRecVolFlowRateWasAutoSized) tmpHeatRecVolFlowRate = this->DesignHeatRecVolFlowRate;
            // save the reference heat recovery fluid volumetric flow rate
            PlantUtilities::RegisterPlantCompDesignFlow(state, this->HeatRecInletNodeNum, tmpHeatRecVolFlowRate);
        }

        if (state.dataPlnt->PlantFinalSizesOkayToReport) {
            OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchMechType, this->Name, "Chiller:EngineDriven");
            OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchMechNomEff, this->Name, this->COP);
            OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchMechNomCap, this->Name, this->NomCap);
        }

        if (ErrorsFound) {
            ShowFatalError(state, "Preceding sizing errors cause program termination");
        }
    }

    void EngineDrivenChillerSpecs::calculate(EnergyPlusData &state,
                                             Real64 &MyLoad,
                                             bool const RunFlag,
                                             DataBranchAirLoopPlant::ControlTypeEnum const EquipFlowCtrl)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Dan Fisher / Brandon Anderson
        //       DATE WRITTEN   Sept. 2000
        //       MODIFIED       Feb. 2010, Chandan Sharma, FSEC. Added basin heater
        //                      Jun. 2016, Rongpeng Zhang, LBNL. Applied the chiller supply water temperature sensor fault model
        //                      Nov. 2016, Rongpeng Zhang, LBNL. Added Fouling Chiller fault
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // simulate a vapor compression chiller using the EngineDriven model

        // METHODOLOGY EMPLOYED:
        // curve fit of performance data:

        // REFERENCES:
        // 1. BLAST Users Manual
        // 2. CHILLER User Manual

        Real64 const ExhaustCP(1.047);    // Exhaust Gas Specific Heat (J/kg-K)
        Real64 const ReferenceTemp(25.0); // Reference temperature by which lower heating
        // value is reported.  This should be subtracted
        // off of when calculated exhaust energies.
        static constexpr std::string_view RoutineName("CalcEngineDrivenChillerModel");

        // set module level inlet and outlet nodes
        this->EvapMassFlowRate = 0.0;
        this->CondMassFlowRate = 0.0;
        this->Power = 0.0;
        this->QCondenser = 0.0;
        this->QEvaporator = 0.0;
        this->Energy = 0.0;
        this->CondenserEnergy = 0.0;
        this->EvaporatorEnergy = 0.0;
        this->HeatRecMdotActual = 0.0;
        this->QTotalHeatRecovered = 0.0;
        this->QJacketRecovered = 0.0;
        this->QLubeOilRecovered = 0.0;
        this->QExhaustRecovered = 0.0;
        this->FuelEnergyUseRate = 0.0;
        this->TotalHeatEnergyRec = 0.0;
        this->JacketEnergyRec = 0.0;
        this->LubeOilEnergyRec = 0.0;
        this->ExhaustEnergyRec = 0.0;
        this->FuelEnergy = 0.0;
        this->FuelMdot = 0.0;
        this->ExhaustStackTemp = 0.0;

        if (this->HeatRecActive) {
            this->HeatRecInletTemp = state.dataLoopNodes->Node(this->HeatRecInletNodeNum).Temp;
            this->HeatRecOutletTemp = state.dataLoopNodes->Node(this->HeatRecInletNodeNum).Temp;
        }

        //   calculate end time of current time step
        Real64 CurrentEndTime = state.dataGlobal->CurrentTime + state.dataHVACGlobal->SysTimeElapsed;

        //   Print warning messages only when valid and only for the first ocurrance. Let summary provide statistics.
        //   Wait for next time step to print warnings. If simulation iterates, print out
        //   the warning for the last iteration only. Must wait for next time step to accomplish this.
        //   If a warning occurs and the simulation down shifts, the warning is not valid.
        if (CurrentEndTime > this->CurrentEndTimeLast && state.dataHVACGlobal->TimeStepSys >= this->TimeStepSysLast) {
            if (this->PrintMessage) {
                ++this->MsgErrorCount;
                //     Show single warning and pass additional info to ShowRecurringWarningErrorAtEnd
                if (this->MsgErrorCount < 2) {
                    ShowWarningError(state, this->MsgBuffer1 + '.');
                    ShowContinueError(state, this->MsgBuffer2);
                } else {
                    ShowRecurringWarningErrorAtEnd(
                        state, this->MsgBuffer1 + " error continues.", this->ErrCount1, this->MsgDataLast, this->MsgDataLast, _, "[C]", "[C]");
                }
            }
        }

        // save last system time step and last end time of current time step (used to determine if warning is valid)
        this->TimeStepSysLast = state.dataHVACGlobal->TimeStepSys;
        this->CurrentEndTimeLast = CurrentEndTime;

        // If Chiller load is 0 or chiller is not running then leave the subroutine.
        if (MyLoad >= 0.0 || !RunFlag) {
            if (EquipFlowCtrl == DataBranchAirLoopPlant::ControlTypeEnum::SeriesActive ||
                state.dataPlnt->PlantLoop(this->CWLoopNum).LoopSide(this->CWLoopSideNum).FlowLock == DataPlant::iFlowLock::Locked) {
                this->EvapMassFlowRate = state.dataLoopNodes->Node(this->EvapInletNodeNum).MassFlowRate;
            } else {
                this->EvapMassFlowRate = 0.0;

                PlantUtilities::SetComponentFlowRate(state,
                                                     this->EvapMassFlowRate,
                                                     this->EvapInletNodeNum,
                                                     this->EvapOutletNodeNum,
                                                     this->CWLoopNum,
                                                     this->CWLoopSideNum,
                                                     this->CWBranchNum,
                                                     this->CWCompNum);
            }

            if (this->CondenserType == DataPlant::CondenserType::WaterCooled) {
                if (state.dataPlnt->PlantLoop(this->CDLoopNum)
                        .LoopSide(this->CDLoopSideNum)
                        .Branch(this->CDBranchNum)
                        .Comp(this->CDCompNum)
                        .FlowCtrl == DataBranchAirLoopPlant::ControlTypeEnum::SeriesActive) {
                    this->CondMassFlowRate = state.dataLoopNodes->Node(this->CondInletNodeNum).MassFlowRate;
                } else {
                    this->CondMassFlowRate = 0.0;
                    PlantUtilities::SetComponentFlowRate(state,
                                                         this->CondMassFlowRate,
                                                         this->CondInletNodeNum,
                                                         this->CondOutletNodeNum,
                                                         this->CDLoopNum,
                                                         this->CDLoopSideNum,
                                                         this->CDBranchNum,
                                                         this->CDCompNum);
                }
            }

            if (this->CondenserType == DataPlant::CondenserType::EvapCooled) {
                CalcBasinHeaterPower(
                    state, this->BasinHeaterPowerFTempDiff, this->BasinHeaterSchedulePtr, this->BasinHeaterSetPointTemp, this->BasinHeaterPower);
            }
            this->PrintMessage = false;
            return;
        }

        if (this->CondenserType == DataPlant::CondenserType::AirCooled) { // Condenser inlet temp = outdoor temp
            state.dataLoopNodes->Node(this->CondInletNodeNum).Temp = state.dataLoopNodes->Node(this->CondInletNodeNum).OutAirDryBulb;
            //  Warn user if entering condenser temperature falls below 0C
            if (state.dataLoopNodes->Node(this->CondInletNodeNum).Temp < 0.0 && !state.dataGlobal->WarmupFlag) {
                this->PrintMessage = true;
                this->MsgBuffer1 = format(
                    "CalcEngineDrivenChillerModel - Chiller:EngineDriven \"{}\" - Air Cooled Condenser Inlet Temperature below 0C", this->Name);
                this->MsgBuffer2 = format("... Outdoor Dry-bulb Condition = {:6.2F} C. Occurrence info = {}, {} {}",
                                          state.dataLoopNodes->Node(this->CondInletNodeNum).Temp,
                                          state.dataEnvrn->EnvironmentName,
                                          state.dataEnvrn->CurMnDy,
                                          General::CreateSysTimeIntervalString(state));
                this->MsgDataLast = state.dataLoopNodes->Node(this->CondInletNodeNum).Temp;
            } else {
                this->PrintMessage = false;
            }
        } else if (this->CondenserType == DataPlant::CondenserType::EvapCooled) { // Condenser inlet temp = (outdoor wet bulb)
            state.dataLoopNodes->Node(this->CondInletNodeNum).Temp = state.dataLoopNodes->Node(this->CondInletNodeNum).OutAirWetBulb;
            //  Warn user if evap condenser wet bulb temperature falls below 10C
            if (state.dataLoopNodes->Node(this->CondInletNodeNum).Temp < 10.0 && !state.dataGlobal->WarmupFlag) {
                this->PrintMessage = true;
                this->MsgBuffer1 = format(
                    "CalcEngineDrivenChillerModel - Chiller:EngineDriven \"{}\" - Evap Cooled Condenser Inlet Temperature below 10C", this->Name);
                this->MsgBuffer2 = format("... Outdoor Wet-bulb Condition = {:6.2F} C. Occurrence info = {}, {} {}",
                                          state.dataLoopNodes->Node(this->CondInletNodeNum).Temp,
                                          state.dataEnvrn->EnvironmentName,
                                          state.dataEnvrn->CurMnDy,
                                          General::CreateSysTimeIntervalString(state));
                this->MsgDataLast = state.dataLoopNodes->Node(this->CondInletNodeNum).Temp;
            } else {
                this->PrintMessage = false;
            }
        } // End of the Air Cooled/Evap Cooled Logic block

        // If not air or evap cooled then set to the condenser node that is attached to a cooling tower
        this->CondInletTemp = state.dataLoopNodes->Node(this->CondInletNodeNum).Temp;

        // Set mass flow rates
        if (this->CondenserType == DataPlant::CondenserType::WaterCooled) {
            this->CondMassFlowRate = this->CondMassFlowRateMax;
            PlantUtilities::SetComponentFlowRate(state,
                                                 this->CondMassFlowRate,
                                                 this->CondInletNodeNum,
                                                 this->CondOutletNodeNum,
                                                 this->CDLoopNum,
                                                 this->CDLoopSideNum,
                                                 this->CDBranchNum,
                                                 this->CDCompNum);
            PlantUtilities::PullCompInterconnectTrigger(state,
                                                        this->CWLoopNum,
                                                        this->CWLoopSideNum,
                                                        this->CWBranchNum,
                                                        this->CWCompNum,
                                                        this->CondMassFlowIndex,
                                                        this->CDLoopNum,
                                                        this->CDLoopSideNum,
                                                        DataPlant::iCriteriaType::MassFlowRate,
                                                        this->CondMassFlowRate);
            if (this->CondMassFlowRate < DataBranchAirLoopPlant::MassFlowTolerance) return;
        }

        //  LOAD LOCAL VARIABLES FROM DATA STRUCTURE (for code readability)
        Real64 ChillerNomCap = this->NomCap;
        Real64 COPLocal = this->COP;
        Real64 TempCondIn = state.dataLoopNodes->Node(this->CondInletNodeNum).Temp;
        Real64 TempEvapOut = state.dataLoopNodes->Node(this->EvapOutletNodeNum).Temp;

        // If there is a fault of chiller fouling
        if (this->FaultyChillerFoulingFlag && (!state.dataGlobal->WarmupFlag) && (!state.dataGlobal->DoingSizing) &&
            (!state.dataGlobal->KickOffSimulation)) {
            int FaultIndex = this->FaultyChillerFoulingIndex;
            Real64 NomCap_ff = ChillerNomCap;
            Real64 COP_ff = COPLocal;

            // calculate the Faulty Chiller Fouling Factor using fault information
            this->FaultyChillerFoulingFactor = state.dataFaultsMgr->FaultsChillerFouling(FaultIndex).CalFoulingFactor(state);

            // update the Chiller nominal capacity and COP at faulty cases
            ChillerNomCap = NomCap_ff * this->FaultyChillerFoulingFactor;
            COPLocal = COP_ff * this->FaultyChillerFoulingFactor;
        }

        // If there is a fault of Chiller SWT Sensor
        if (this->FaultyChillerSWTFlag && (!state.dataGlobal->WarmupFlag) && (!state.dataGlobal->DoingSizing) &&
            (!state.dataGlobal->KickOffSimulation)) {
            int FaultIndex = this->FaultyChillerSWTIndex;
            Real64 EvapOutletTemp_ff = TempEvapOut;

            // calculate the sensor offset using fault information
            this->FaultyChillerSWTOffset = state.dataFaultsMgr->FaultsChillerSWTSensor(FaultIndex).CalFaultOffsetAct(state);
            // update the TempEvapOut
            TempEvapOut = max(this->TempLowLimitEvapOut,
                              min(state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp, EvapOutletTemp_ff - this->FaultyChillerSWTOffset));
            this->FaultyChillerSWTOffset = EvapOutletTemp_ff - TempEvapOut;
        }

        // Calculate chiller performance from this set of performance equations.
        Real64 DeltaTemp = (TempCondIn - this->TempDesCondIn) / this->TempRiseCoef - (TempEvapOut - this->TempDesEvapOut);

        // available nominal capacity ratio
        Real64 AvailNomCapRat = this->CapRatCoef(1) + this->CapRatCoef(2) * DeltaTemp + this->CapRatCoef(3) * pow_2(DeltaTemp);

        Real64 AvailChillerCap = ChillerNomCap * AvailNomCapRat;

        // full load power ratio
        Real64 FullLoadPowerRat = this->PowerRatCoef(1) + this->PowerRatCoef(2) * AvailNomCapRat + this->PowerRatCoef(3) * pow_2(AvailNomCapRat);

        Real64 PartLoadRat(0.0); // part load ratio for efficiency
        if (AvailChillerCap > 0.0) {
            PartLoadRat = max(this->MinPartLoadRat, min(std::abs(MyLoad) / AvailChillerCap, this->MaxPartLoadRat));
        }

        Real64 FracFullLoadPower = this->FullLoadCoef(1) + this->FullLoadCoef(2) * PartLoadRat + this->FullLoadCoef(3) * pow_2(PartLoadRat);

        Real64 OperPartLoadRat; // Actual operating PLR
        if (AvailChillerCap > 0.0) {
            if (std::abs(MyLoad) / AvailChillerCap < this->MinPartLoadRat) {
                OperPartLoadRat = std::abs(MyLoad) / AvailChillerCap;
            } else {
                OperPartLoadRat = PartLoadRat;
            }
        } else {
            OperPartLoadRat = 0.0;
        }

        Real64 Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                           state.dataPlnt->PlantLoop(this->CWLoopNum).FluidName,
                                                           state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp,
                                                           state.dataPlnt->PlantLoop(this->CWLoopNum).FluidIndex,
                                                           RoutineName);

        // If FlowLock is True, the new resolved mdot is used to update Power, QEvap, Qcond, and
        // condenser side outlet temperature.
        if (state.dataPlnt->PlantLoop(this->CWLoopNum).LoopSide(this->CWLoopSideNum).FlowLock == DataPlant::iFlowLock::Unlocked) {
            this->PossibleSubcooling = false;
            this->QEvaporator = AvailChillerCap * OperPartLoadRat;
            Real64 FRAC;
            if (OperPartLoadRat < this->MinPartLoadRat) {
                FRAC = min(1.0, (OperPartLoadRat / this->MinPartLoadRat));
            } else {
                FRAC = 1.0;
            }
            this->Power = FracFullLoadPower * FullLoadPowerRat * AvailChillerCap / COPLocal * FRAC;

            // Either set the flow to the Constant value or caluclate the flow for the variable volume
            if ((this->FlowMode == DataPlant::FlowMode::Constant) || (this->FlowMode == DataPlant::FlowMode::NotModulated)) {
                // Start by assuming max (design) flow
                this->EvapMassFlowRate = this->EvapMassFlowRateMax;
                // Use SetComponentFlowRate to decide actual flow
                PlantUtilities::SetComponentFlowRate(state,
                                                     this->EvapMassFlowRate,
                                                     this->EvapInletNodeNum,
                                                     this->EvapOutletNodeNum,
                                                     this->CWLoopNum,
                                                     this->CWLoopSideNum,
                                                     this->CWBranchNum,
                                                     this->CWCompNum);
                // Evaluate delta temp based on actual flow rate
                Real64 EvapDeltaTemp(0.0);
                if (this->EvapMassFlowRate != 0.0) {
                    EvapDeltaTemp = this->QEvaporator / this->EvapMassFlowRate / Cp;
                } else {
                    EvapDeltaTemp = 0.0;
                }
                // Evaluate outlet temp based on delta
                this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - EvapDeltaTemp;

            } else if (this->FlowMode == DataPlant::FlowMode::LeavingSetpointModulated) {

                // Calculate the Delta Temp from the inlet temp to the chiller outlet setpoint
                Real64 EvapDeltaTemp(0.0);
                {
                    auto const SELECT_CASE_var(state.dataPlnt->PlantLoop(this->CWLoopNum).LoopDemandCalcScheme);
                    if (SELECT_CASE_var == DataPlant::iLoopDemandCalcScheme::SingleSetPoint) {
                        EvapDeltaTemp =
                            state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempSetPoint;
                    } else if (SELECT_CASE_var == DataPlant::iLoopDemandCalcScheme::DualSetPointDeadBand) {
                        EvapDeltaTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp -
                                        state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempSetPointHi;
                    }
                }

                if (EvapDeltaTemp != 0.0) {
                    this->EvapMassFlowRate = std::abs(this->QEvaporator / Cp / EvapDeltaTemp);
                    if ((this->EvapMassFlowRate - this->EvapMassFlowRateMax) > DataBranchAirLoopPlant::MassFlowTolerance)
                        this->PossibleSubcooling = true;
                    // Check to see if the Maximum is exceeded, if so set to maximum
                    this->EvapMassFlowRate = min(this->EvapMassFlowRateMax, this->EvapMassFlowRate);
                    // Use SetComponentFlowRate to decide actual flow
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
                    // Try to request zero flow
                    this->EvapMassFlowRate = 0.0;
                    // Use SetComponentFlowRate to decide actual flow
                    PlantUtilities::SetComponentFlowRate(state,
                                                         this->EvapMassFlowRate,
                                                         this->EvapInletNodeNum,
                                                         this->EvapOutletNodeNum,
                                                         this->CWLoopNum,
                                                         this->CWLoopSideNum,
                                                         this->CWBranchNum,
                                                         this->CWCompNum);
                    // No deltaT since component is not running
                    this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp;
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
                                        Cp,
                                        state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp,
                                        this->EvapOutletTemp,
                                        this->EvapMassFlowRate,
                                        this->QEvaporator);
                // update corresponding variables at faulty case
                PartLoadRat = (AvailChillerCap > 0.0) ? (this->QEvaporator / AvailChillerCap) : 0.0;
                PartLoadRat = max(0.0, min(PartLoadRat, this->MaxPartLoadRat));
            }

        } else { // If FlowLock is True

            this->EvapMassFlowRate = state.dataLoopNodes->Node(this->EvapInletNodeNum).MassFlowRate;
            PlantUtilities::SetComponentFlowRate(state,
                                                 this->EvapMassFlowRate,
                                                 this->EvapInletNodeNum,
                                                 this->EvapOutletNodeNum,
                                                 this->CWLoopNum,
                                                 this->CWLoopSideNum,
                                                 this->CWBranchNum,
                                                 this->CWCompNum);
            // Some other component set the flow to 0. No reason to continue with calculations.
            if (this->EvapMassFlowRate == 0.0) {
                MyLoad = 0.0;
                if (this->CondenserType == DataPlant::CondenserType::EvapCooled) {
                    CalcBasinHeaterPower(
                        state, this->BasinHeaterPowerFTempDiff, this->BasinHeaterSchedulePtr, this->BasinHeaterSetPointTemp, this->BasinHeaterPower);
                }
                this->PrintMessage = false;
                return;
            }

            if (this->PossibleSubcooling) {
                this->QEvaporator = std::abs(MyLoad);
                Real64 EvapDeltaTemp = this->QEvaporator / this->EvapMassFlowRate / Cp;
                this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - EvapDeltaTemp;
                if (this->EvapOutletTemp < state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempMin) {
                    this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempMin;
                    EvapDeltaTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - this->EvapOutletTemp;
                    this->QEvaporator = std::abs(this->EvapMassFlowRate * Cp * EvapDeltaTemp);
                }
            } else { // No subcooling in this case.No recalculation required.Still need to check chiller low temp limit

                Real64 TempEvapOutSetPoint(0.0); // C - evaporator outlet temperature setpoint

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
                            TempEvapOutSetPoint =
                                state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPoint;
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
                    }
                }

                Real64 EvapDeltaTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - TempEvapOutSetPoint;
                this->QEvaporator = std::abs(this->EvapMassFlowRate * Cp * EvapDeltaTemp);
                this->EvapOutletTemp = TempEvapOutSetPoint;
            }
            // Check that the Evap outlet temp honors both plant loop temp low limit and also the chiller low limit
            if (this->EvapOutletTemp < this->TempLowLimitEvapOut) {
                if ((state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - this->TempLowLimitEvapOut) > DataPlant::DeltaTempTol) {
                    this->EvapOutletTemp = this->TempLowLimitEvapOut;
                    Real64 EvapDeltaTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - this->EvapOutletTemp;
                    this->QEvaporator = this->EvapMassFlowRate * Cp * EvapDeltaTemp;
                } else {
                    this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp;
                    Real64 EvapDeltaTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - this->EvapOutletTemp;
                    this->QEvaporator = this->EvapMassFlowRate * Cp * EvapDeltaTemp;
                }
            }
            if (this->EvapOutletTemp < state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempMin) {
                if ((state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempMin) >
                    DataPlant::DeltaTempTol) {
                    this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempMin;
                    Real64 EvapDeltaTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - this->EvapOutletTemp;
                    this->QEvaporator = this->EvapMassFlowRate * Cp * EvapDeltaTemp;
                } else {
                    this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp;
                    Real64 EvapDeltaTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - this->EvapOutletTemp;
                    this->QEvaporator = this->EvapMassFlowRate * Cp * EvapDeltaTemp;
                }
            }
            // If load exceeds the distributed load set to the distributed load
            if (this->QEvaporator > std::abs(MyLoad)) {
                if (this->EvapMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance) {
                    this->QEvaporator = std::abs(MyLoad);
                    Real64 EvapDeltaTemp = this->QEvaporator / this->EvapMassFlowRate / Cp;
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
                                        Cp,
                                        state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp,
                                        this->EvapOutletTemp,
                                        this->EvapMassFlowRate,
                                        this->QEvaporator);
            }

            // Checks QEvaporator on the basis of the machine limits.
            if (this->QEvaporator > (AvailChillerCap * this->MaxPartLoadRat)) {
                if (this->EvapMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance) {
                    this->QEvaporator = AvailChillerCap * OperPartLoadRat;
                    Real64 EvapDeltaTemp = this->QEvaporator / this->EvapMassFlowRate / Cp;
                    this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - EvapDeltaTemp;
                } else {
                    this->QEvaporator = 0.0;
                    this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp;
                }
            }

            Real64 FRAC;
            if (OperPartLoadRat < this->MinPartLoadRat) {
                FRAC = min(1.0, (OperPartLoadRat / this->MinPartLoadRat));
            } else {
                FRAC = 1.0;
            }

            // Chiller is false loading below PLR = minimum unloading ratio, find PLR used for energy calculation
            this->Power = FracFullLoadPower * FullLoadPowerRat * AvailChillerCap / COPLocal * FRAC;

            if (this->EvapMassFlowRate == 0.0) {
                this->QEvaporator = 0.0;
                this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp;
                this->Power = 0.0;
                this->PrintMessage = false;
            }
            if (this->QEvaporator == 0.0 && this->CondenserType == DataPlant::CondenserType::EvapCooled) {
                CalcBasinHeaterPower(
                    state, this->BasinHeaterPowerFTempDiff, this->BasinHeaterSchedulePtr, this->BasinHeaterSetPointTemp, this->BasinHeaterPower);
            }
        } // This is the end of the FlowLock Block

        // Now determine Cooling
        // QCondenser is calculated the same for each type, but the power consumption should be different
        //  depending on the performance coefficients used for the chiller model.
        this->QCondenser = this->Power + this->QEvaporator;

        if (this->CondenserType == DataPlant::CondenserType::WaterCooled) {

            if (this->CondMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance) {
                Real64 CpCond = FluidProperties::GetSpecificHeatGlycol(state,
                                                                       state.dataPlnt->PlantLoop(this->CDLoopNum).FluidName,
                                                                       this->CondInletTemp,
                                                                       state.dataPlnt->PlantLoop(this->CDLoopNum).FluidIndex,
                                                                       RoutineName);
                this->CondOutletTemp = this->QCondenser / this->CondMassFlowRate / CpCond + this->CondInletTemp;
            } else {
                ShowSevereError(state, "CalcEngineDrivenChillerModel: Condenser flow = 0, for EngineDrivenChiller=" + this->Name);
                ShowContinueErrorTimeStamp(state, "");
            }

        } else { // Air Cooled or Evap Cooled

            // don't care about outlet temp for Air-Cooled or Evap Cooled
            this->CondOutletTemp = this->CondInletTemp;
        }

        // EngineDriven Portion of the Engine Driven Chiller:

        // DETERMINE FUEL CONSUMED AND AVAILABLE WASTE HEAT

        // Use Curve fit to determine Fuel Energy Input.  For electric power generated in Watts, the fuel
        // energy input is calculated in J/s.  The PLBasedFuelInputCurve selects ratio of fuel flow (J/s)/cooling load (J/s).
        Real64 EngineDrivenFuelEnergy;
        if (PartLoadRat == 0) {
            EngineDrivenFuelEnergy = 0.0;
        } else {
            PartLoadRat = max(this->MinPartLoadRat, PartLoadRat);
            // (RELDC) Ratio of Shaft Power to Fuel Energy Input
            Real64 ClngLoadFuelRat = CurveManager::CurveValue(state, this->ClngLoadtoFuelCurve, PartLoadRat);
            EngineDrivenFuelEnergy = this->QEvaporator / ClngLoadFuelRat;
        }
        // Use Curve fit to determine energy recovered in the water jacket.  This curve calculates the water jacket energy recovered (J/s) by
        // multiplying the total fuel input (J/s) by the fraction of that power that could be recovered in the water jacket at that
        // particular part load.

        // (RJACDC) Ratio of Recoverable Jacket Heat to Fuel Energy Input
        Real64 RecJacHeattoFuelRat = CurveManager::CurveValue(state, this->RecJacHeattoFuelCurve, PartLoadRat);
        this->QJacketRecovered = EngineDrivenFuelEnergy * RecJacHeattoFuelRat;

        // Use Curve fit to determine Heat Recovered Lubricant Energy.  This curve calculates the lube energy recovered (J/s) by
        // multiplying the total fuel input (J/s) by the fraction of that power that could be recovered in the lube oil at that
        // particular part load.
        // (RLUBDC) Ratio of Recoverable Lube Oil Heat to Fuel Energy Input
        Real64 RecLubeHeattoFuelRat = CurveManager::CurveValue(state, this->RecLubeHeattoFuelCurve, PartLoadRat);
        this->QLubeOilRecovered = EngineDrivenFuelEnergy * RecLubeHeattoFuelRat;

        // Use Curve fit to determine Heat Recovered from the exhaust.  This curve calculates the  energy recovered (J/s) by
        // multiplying the total fuel input (J/s) by the fraction of that power that could be recovered in the exhaust at that
        // particular part load.
        Real64 TotExhausttoFuelRat = CurveManager::CurveValue(state, this->TotExhausttoFuelCurve, PartLoadRat);
        Real64 TotalExhaustEnergy = EngineDrivenFuelEnergy * TotExhausttoFuelRat;

        // Use Curve fit to determine Exhaust Temperature in C.  The temperature is simply a curve fit
        // of the exhaust temperature in C to the part load ratio.
        if (PartLoadRat != 0) {
            Real64 exhaustTemp = CurveManager::CurveValue(state, this->ExhaustTempCurve, PartLoadRat);
            Real64 ExhaustGasFlow = TotalExhaustEnergy / (ExhaustCP * (exhaustTemp - ReferenceTemp));

            // Use Curve fit to determine stack temp after heat recovery
            Real64 UALocal = this->UACoef(1) * std::pow(ChillerNomCap, this->UACoef(2));
            Real64 designMinExitGasTemp = this->DesignMinExitGasTemp;

            this->ExhaustStackTemp =
                designMinExitGasTemp + (exhaustTemp - designMinExitGasTemp) /
                                           std::exp(UALocal / (max(ExhaustGasFlow, this->MaxExhaustperPowerOutput * ChillerNomCap) * ExhaustCP));

            this->QExhaustRecovered = max(ExhaustGasFlow * ExhaustCP * (exhaustTemp - this->ExhaustStackTemp), 0.0);
        } else {
            this->QExhaustRecovered = 0.0;
        }

        this->QTotalHeatRecovered = this->QExhaustRecovered + this->QLubeOilRecovered + this->QJacketRecovered;

        // Update Heat Recovery temperatures
        if (this->HeatRecActive) {
            Real64 HeatRecRatio;
            this->calcHeatRecovery(state, this->QTotalHeatRecovered, HeatRecRatio);
            this->QExhaustRecovered *= HeatRecRatio;
            this->QLubeOilRecovered *= HeatRecRatio;
            this->QJacketRecovered *= HeatRecRatio;
        }

        // Calculate Energy
        this->CondenserEnergy = this->QCondenser * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
        this->Energy = this->Power * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
        this->EvaporatorEnergy = this->QEvaporator * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
        this->FuelEnergyUseRate = EngineDrivenFuelEnergy;
        this->FuelEnergy = this->FuelEnergyUseRate * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
        this->JacketEnergyRec = this->QJacketRecovered * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
        this->LubeOilEnergyRec = this->QLubeOilRecovered * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
        this->ExhaustEnergyRec = this->QExhaustRecovered * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
        this->QTotalHeatRecovered = this->QExhaustRecovered + this->QLubeOilRecovered + this->QJacketRecovered;
        this->TotalHeatEnergyRec = this->ExhaustEnergyRec + this->LubeOilEnergyRec + this->JacketEnergyRec;
        this->FuelEnergyUseRate = std::abs(this->FuelEnergyUseRate);
        this->FuelEnergy = std::abs(this->FuelEnergy);
        this->FuelMdot = std::abs(this->FuelEnergyUseRate) / (this->FuelHeatingValue * KJtoJ);

        // check for problems BG 9/12/06 (deal with observed negative energy results)
        if (this->Energy < 0.0) { // there is a serious problem
            if (this->CondenserType == DataPlant::CondenserType::WaterCooled) {
                // first check for run away condenser loop temps (only reason yet to be observed for this?)
                if (this->CondInletTemp > 70.0) {
                    ShowSevereError(state,
                                    "CalcEngineDrivenChillerModel: Condenser loop inlet temperatures > 70.0 C for EngineDrivenChiller=" + this->Name);
                    ShowContinueErrorTimeStamp(state, "");
                    ShowContinueError(state, format("Condenser loop water temperatures are too high at{:.2R}", this->CondInletTemp));
                    ShowContinueError(state, "Check input for condenser plant loop, especially cooling tower");
                    ShowContinueError(state, format("Evaporator inlet temperature: {:.2R}", state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp));

                    ShowFatalError(state, "Program Terminates due to previous error condition");
                }
            }
            if (!state.dataGlobal->WarmupFlag) {
                if (AvailNomCapRat < 0.0) { // apparently the real reason energy goes negative
                    ShowSevereError(state, "CalcEngineDrivenChillerModel: Capacity ratio below zero for EngineDrivenChiller=" + this->Name);
                    ShowContinueErrorTimeStamp(state, "");
                    ShowContinueError(state, "Check input for Capacity Ratio Curve");
                    ShowContinueError(state, format("Condenser inlet temperature: {:.2R}", this->CondInletTemp));
                    ShowContinueError(state, format("Evaporator inlet temperature: {:.2R}", state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp));
                    ShowFatalError(state, "Program Terminates due to previous error condition");
                }
            }
            // If makes it here, set limits, chiller can't have negative energy/power
            // proceeding silently for now but may want to throw error here
            this->Power = 0.0;
            this->Energy = 0.0;
        }
    }

    void EngineDrivenChillerSpecs::calcHeatRecovery(EnergyPlusData &state, Real64 const EnergyRecovered, Real64 &HeatRecRatio)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Brandon Anderson
        //       DATE WRITTEN:    November 2000

        // PURPOSE OF THIS SUBROUTINE:
        // To perform heat recovery calculations and node updates

        // METHODOLOGY EMPLOYED: This routine is required for the heat recovery loop.
        // It works in conjunction with the Heat Recovery Manager, and the PlantWaterHeater.
        // The chiller sets the flow on the loop first by the input design flowrate and then
        // performs a check to verify that

        // SUBROUTINE PARAMETER DEFINITIONS:
        static constexpr std::string_view RoutineName("ChillerHeatRecovery");

        // Need to set the HeatRecRatio to 1.0 if it is not modified
        HeatRecRatio = 1.0;

        // This mdot is input specified mdot "Desired Flowrate", already set in init routine
        this->HeatRecMdotActual = state.dataLoopNodes->Node(this->HeatRecInletNodeNum).MassFlowRate;

        this->HeatRecInletTemp = state.dataLoopNodes->Node(this->HeatRecInletNodeNum).Temp;
        Real64 cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                           state.dataPlnt->PlantLoop(this->HRLoopNum).FluidName,
                                                           this->HeatRecInletTemp,
                                                           state.dataPlnt->PlantLoop(this->HRLoopNum).FluidIndex,
                                                           RoutineName);

        // Don't divide by zero - Note This also results in no heat recovery when
        //  design Mdot for Heat Recovery - Specified on Chiller Input - is zero
        //  In order to see what minimum heat recovery flow rate is for the design temperature
        //  The design heat recovery flow rate can be set very small, but greater than zero.
        if ((this->HeatRecMdotActual > 0) && (cp > 0)) {
            this->HeatRecOutletTemp = (EnergyRecovered) / (this->HeatRecMdotActual * cp) + this->HeatRecInletTemp;
        } else {
            this->HeatRecOutletTemp = this->HeatRecInletTemp;
        }

        // Now verify that the design flowrate was large enough to prevent phase change
        if (this->HeatRecOutletTemp > this->HeatRecMaxTemp) {
            Real64 MinHeatRecMdot(0.0);
            if (this->HeatRecMaxTemp != this->HeatRecInletTemp) {
                MinHeatRecMdot = (EnergyRecovered) / (cp * (this->HeatRecMaxTemp - this->HeatRecInletTemp));
                if (MinHeatRecMdot < 0.0) MinHeatRecMdot = 0.0;
            }

            // Recalculate Outlet Temperature, with adjusted flowrate
            if ((MinHeatRecMdot > 0.0) && (cp > 0.0)) {
                this->HeatRecOutletTemp = (EnergyRecovered) / (MinHeatRecMdot * cp) + this->HeatRecInletTemp;
                HeatRecRatio = this->HeatRecMdotActual / MinHeatRecMdot;
            } else {
                this->HeatRecOutletTemp = this->HeatRecInletTemp;
                HeatRecRatio = 0.0;
            }
        }
    }

    void EngineDrivenChillerSpecs::update(EnergyPlusData &state, Real64 const MyLoad, bool const RunFlag)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Dan Fisher / Brandon Anderson
        //       DATE WRITTEN:    September 2000

        Real64 const ReportingConstant = state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;

        if (MyLoad >= 0.0 || !RunFlag) { // Chiller not running
            // set node temperatures
            state.dataLoopNodes->Node(this->EvapOutletNodeNum).Temp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp;
            state.dataLoopNodes->Node(this->CondOutletNodeNum).Temp = state.dataLoopNodes->Node(this->CondInletNodeNum).Temp;

            this->Power = 0.0;
            this->QEvaporator = 0.0;
            this->QCondenser = 0.0;
            this->Energy = 0.0;
            this->EvaporatorEnergy = 0.0;
            this->CondenserEnergy = 0.0;
            this->EvapInletTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp;
            this->CondInletTemp = state.dataLoopNodes->Node(this->CondInletNodeNum).Temp;
            this->CondOutletTemp = state.dataLoopNodes->Node(this->CondOutletNodeNum).Temp;
            this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapOutletNodeNum).Temp;
            this->FuelCOP = 0.0;
            if (this->CondenserType == DataPlant::CondenserType::EvapCooled) {
                this->BasinHeaterPower = this->BasinHeaterPower;
                this->BasinHeaterConsumption = this->BasinHeaterPower * ReportingConstant;
            }
        } else { // Chiller is running
            // set node temperatures
            state.dataLoopNodes->Node(this->EvapOutletNodeNum).Temp = this->EvapOutletTemp;
            state.dataLoopNodes->Node(this->CondOutletNodeNum).Temp = this->CondOutletTemp;

            this->EvapInletTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp;
            this->CondInletTemp = state.dataLoopNodes->Node(this->CondInletNodeNum).Temp;
            this->CondOutletTemp = state.dataLoopNodes->Node(this->CondOutletNodeNum).Temp;
            this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapOutletNodeNum).Temp;
            if (this->FuelEnergyUseRate != 0.0) {
                this->FuelCOP = this->QEvaporator / this->FuelEnergyUseRate;
            } else {
                this->FuelCOP = 0.0;
            }
            if (this->CondenserType == DataPlant::CondenserType::EvapCooled) {
                this->BasinHeaterPower = this->BasinHeaterPower;
                this->BasinHeaterConsumption = this->BasinHeaterPower * ReportingConstant;
            }
        }

        // Update Heat Recovery Stuff whether running or not, variables should be set correctly
        this->HeatRecMdot = this->HeatRecMdotActual;

        // Update the Heat Recovery outlet
        if (this->HeatRecActive) {
            PlantUtilities::SafeCopyPlantNode(state, this->HeatRecInletNodeNum, this->HeatRecOutletNodeNum);
            state.dataLoopNodes->Node(this->HeatRecOutletNodeNum).Temp = this->HeatRecOutletTemp;
        }
    }
    void EngineDrivenChillerSpecs::oneTimeInit(EnergyPlusData &state)
    {

        if (this->MyFlag) {
            // Locate the chillers on the plant loops for later usage
            bool errFlag = false;
            this->setupOutputVariables(state);
            PlantUtilities::ScanPlantLoopsForObject(state,
                                                    this->Name,
                                                    this->plantTypeOfNum,
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
            if (this->CondenserType != DataPlant::CondenserType::AirCooled && this->CondenserType != DataPlant::CondenserType::EvapCooled) {
                PlantUtilities::ScanPlantLoopsForObject(state,
                                                        this->Name,
                                                        this->plantTypeOfNum,
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
                    state, this->CWLoopNum, this->CWLoopSideNum, this->CDLoopNum, this->CDLoopSideNum, this->plantTypeOfNum, true);
            }
            if (this->HeatRecActive) {
                PlantUtilities::ScanPlantLoopsForObject(state,
                                                        this->Name,
                                                        this->plantTypeOfNum,
                                                        this->HRLoopNum,
                                                        this->HRLoopSideNum,
                                                        this->HRBranchNum,
                                                        this->HRCompNum,
                                                        errFlag,
                                                        _,
                                                        _,
                                                        _,
                                                        this->HeatRecInletNodeNum,
                                                        _);
                PlantUtilities::InterConnectTwoPlantLoopSides(
                    state, this->CWLoopNum, this->CWLoopSideNum, this->HRLoopNum, this->HRLoopSideNum, this->plantTypeOfNum, true);
            }

            if (this->CondenserType != DataPlant::CondenserType::AirCooled && this->CondenserType != DataPlant::CondenserType::EvapCooled &&
                this->HeatRecActive) {
                PlantUtilities::InterConnectTwoPlantLoopSides(
                    state, this->CDLoopNum, this->CDLoopSideNum, this->HRLoopNum, this->HRLoopSideNum, this->plantTypeOfNum, false);
            }
            if (errFlag) {
                ShowFatalError(state, "InitEngineDrivenChiller: Program terminated due to previous condition(s).");
            }

            if (this->FlowMode == DataPlant::FlowMode::Constant) {
                // reset flow priority
                state.dataPlnt->PlantLoop(this->CWLoopNum)
                    .LoopSide(this->CWLoopSideNum)
                    .Branch(this->CWBranchNum)
                    .Comp(this->CWCompNum)
                    .FlowPriority = DataPlant::LoopFlowStatus_NeedyIfLoopOn;
            }

            if (this->FlowMode == DataPlant::FlowMode::LeavingSetpointModulated) {
                // reset flow priority
                state.dataPlnt->PlantLoop(this->CWLoopNum)
                    .LoopSide(this->CWLoopSideNum)
                    .Branch(this->CWBranchNum)
                    .Comp(this->CWCompNum)
                    .FlowPriority = DataPlant::LoopFlowStatus_NeedyIfLoopOn;
                // check if setpoint on outlet node
                if ((state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempSetPoint == DataLoopNode::SensedNodeFlagValue) &&
                    (state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempSetPointHi == DataLoopNode::SensedNodeFlagValue)) {
                    if (!state.dataGlobal->AnyEnergyManagementSystemInModel) {
                        if (!this->ModulatedFlowErrDone) {
                            ShowWarningError(state, "Missing temperature setpoint for LeavingSetpointModulated mode chiller named " + this->Name);
                            ShowContinueError(
                                state,
                                "  A temperature setpoint is needed at the outlet node of a chiller in variable flow mode, use a SetpointManager");
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
                                ShowContinueError(
                                    state, "  A temperature setpoint is needed at the outlet node of a chiller evaporator in variable flow mode");
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

            this->MyFlag = false;
        }
    }

    GTChillerSpecs *GTChillerSpecs::factory(EnergyPlusData &state, std::string const &chillerName)
    {
        if (state.dataPlantChillers->GetGasTurbineInput) {
            GTChillerSpecs::getInput(state);
            state.dataPlantChillers->GetGasTurbineInput = false;
        }
        for (auto &thisChiller : state.dataPlantChillers->GTChiller) {
            if (UtilityRoutines::MakeUPPERCase(thisChiller.Name) == chillerName) {
                return &thisChiller;
            }
        }
        ShowFatalError(state, "Could not locate gas turbine chiller with name: " + chillerName);
        return nullptr;
    }

    void
    GTChillerSpecs::simulate(EnergyPlusData &state, const PlantLocation &calledFromLocation, bool FirstHVACIteration, Real64 &CurLoad, bool RunFlag)
    {
        if (calledFromLocation.loopNum == this->CWLoopNum) { // chilled water loop
            this->initialize(state, RunFlag, CurLoad);
            auto &sim_component(
                state.dataPlnt->PlantLoop(this->CWLoopNum).LoopSide(this->CWLoopSideNum).Branch(this->CWBranchNum).Comp(this->CWCompNum));
            this->calculate(state, CurLoad, RunFlag, sim_component.FlowCtrl);
            this->update(state, CurLoad, RunFlag);
        } else if (calledFromLocation.loopNum == this->CDLoopNum) { // condenser loop
            PlantUtilities::UpdateChillerComponentCondenserSide(state,
                                                                this->CDLoopNum,
                                                                this->CDLoopSideNum,
                                                                this->plantTypeOfNum,
                                                                this->CondInletNodeNum,
                                                                this->CondOutletNodeNum,
                                                                this->QCondenser,
                                                                this->CondInletTemp,
                                                                this->CondOutletTemp,
                                                                this->CondMassFlowRate,
                                                                FirstHVACIteration);
        } else if (calledFromLocation.loopNum == this->HRLoopNum) { // heat recovery loop
            PlantUtilities::UpdateComponentHeatRecoverySide(state,
                                                            this->HRLoopNum,
                                                            this->HRLoopSideNum,
                                                            this->plantTypeOfNum,
                                                            this->HeatRecInletNodeNum,
                                                            this->HeatRecOutletNodeNum,
                                                            this->HeatRecLubeRate,
                                                            this->HeatRecInletTemp,
                                                            this->HeatRecOutletTemp,
                                                            this->HeatRecMdot,
                                                            FirstHVACIteration);
        }
    }

    void GTChillerSpecs::getInput(EnergyPlusData &state)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Dan Fisher / Brandon Anderson
        //       DATE WRITTEN:    September 2000

        // PURPOSE OF THIS SUBROUTINE:
        // This routine will get the input
        // required by the GT Chiller model.

        // METHODOLOGY EMPLOYED:
        // EnergyPlus input processor

        static constexpr std::string_view RoutineName("GetGTChillerInput: "); // include trailing blank space

        int NumAlphas; // Number of elements in the alpha array
        int NumNums;   // Number of elements in the numeric array
        int IOStat;    // IO Status when calling get input subroutine
        bool ErrorsFound(false);

        state.dataIPShortCut->cCurrentModuleObject = "Chiller:CombustionTurbine";
        state.dataPlantChillers->NumGTChillers =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, state.dataIPShortCut->cCurrentModuleObject);

        if (state.dataPlantChillers->NumGTChillers <= 0) {
            ShowSevereError(state, "No " + state.dataIPShortCut->cCurrentModuleObject + " equipment specified in input file");
            ErrorsFound = true;
        }
        // See if load distribution manager has already gotten the input
        if (allocated(state.dataPlantChillers->GTChiller)) return;

        // ALLOCATE ARRAYS
        state.dataPlantChillers->GTChiller.allocate(state.dataPlantChillers->NumGTChillers);

        for (int ChillerNum = 1; ChillerNum <= state.dataPlantChillers->NumGTChillers; ++ChillerNum) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     state.dataIPShortCut->cCurrentModuleObject,
                                                                     ChillerNum,
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

            auto &thisChiller = state.dataPlantChillers->GTChiller(ChillerNum);
            thisChiller.Name = state.dataIPShortCut->cAlphaArgs(1);
            thisChiller.plantTypeOfNum = DataPlant::TypeOf_Chiller_CombTurbine;

            thisChiller.NomCap = state.dataIPShortCut->rNumericArgs(1);

            if (thisChiller.NomCap == DataSizing::AutoSize) {
                thisChiller.NomCapWasAutoSized = true;
            }
            if (state.dataIPShortCut->rNumericArgs(1) == 0.0) {
                ShowSevereError(state,
                                format("Invalid {}={:.2R}", state.dataIPShortCut->cNumericFieldNames(1), state.dataIPShortCut->rNumericArgs(1)));
                ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
                ErrorsFound = true;
            }

            thisChiller.COP = state.dataIPShortCut->rNumericArgs(2);
            if (state.dataIPShortCut->rNumericArgs(2) == 0.0) {
                ShowSevereError(state,
                                format("Invalid {}={:.2R}", state.dataIPShortCut->cNumericFieldNames(2), state.dataIPShortCut->rNumericArgs(2)));
                ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
                ErrorsFound = true;
            }

            if (state.dataIPShortCut->cAlphaArgs(2) == "AIRCOOLED") {
                thisChiller.CondenserType = DataPlant::CondenserType::AirCooled;
            } else if (state.dataIPShortCut->cAlphaArgs(2) == "WATERCOOLED") {
                thisChiller.CondenserType = DataPlant::CondenserType::WaterCooled;
            } else if (state.dataIPShortCut->cAlphaArgs(2) == "EVAPORATIVELYCOOLED") {
                thisChiller.CondenserType = DataPlant::CondenserType::EvapCooled;
            } else {
                ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(2) + '=' + state.dataIPShortCut->cAlphaArgs(2));
                ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
                ErrorsFound = true;
            }

            thisChiller.EvapInletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                                               state.dataIPShortCut->cAlphaArgs(3),
                                                                               ErrorsFound,
                                                                               state.dataIPShortCut->cCurrentModuleObject,
                                                                               state.dataIPShortCut->cAlphaArgs(1),
                                                                               DataLoopNode::NodeFluidType::Water,
                                                                               DataLoopNode::NodeConnectionType::Inlet,
                                                                               NodeInputManager::compFluidStream::Primary,
                                                                               DataLoopNode::ObjectIsNotParent);
            thisChiller.EvapOutletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                                                state.dataIPShortCut->cAlphaArgs(4),
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
                                               state.dataIPShortCut->cAlphaArgs(3),
                                               state.dataIPShortCut->cAlphaArgs(4),
                                               "Chilled Water Nodes");

            if (thisChiller.CondenserType == DataPlant::CondenserType::AirCooled ||
                thisChiller.CondenserType == DataPlant::CondenserType::EvapCooled) {
                // Connection not required for air or evap cooled condenser
                // If the condenser inlet is blank for air cooled and evap cooled condensers then supply a generic name
                // since it is not used elsewhere for connection
                if (state.dataIPShortCut->lAlphaFieldBlanks(5)) {
                    if (len(state.dataIPShortCut->cAlphaArgs(1)) <
                        DataGlobalConstants::MaxNameLength - 21) { // protect against long name leading to > 100 chars
                        state.dataIPShortCut->cAlphaArgs(5) = state.dataIPShortCut->cAlphaArgs(1) + " CONDENSER INLET NODE";
                    } else {
                        state.dataIPShortCut->cAlphaArgs(5) = state.dataIPShortCut->cAlphaArgs(1).substr(0, 79) + " CONDENSER INLET NODE";
                    }
                }
                if (state.dataIPShortCut->lAlphaFieldBlanks(6)) {
                    if (len(state.dataIPShortCut->cAlphaArgs(1)) <
                        DataGlobalConstants::MaxNameLength - 22) { // protect against long name leading to > 100 chars
                        state.dataIPShortCut->cAlphaArgs(6) = state.dataIPShortCut->cAlphaArgs(1) + " CONDENSER OUTLET NODE";
                    } else {
                        state.dataIPShortCut->cAlphaArgs(6) = state.dataIPShortCut->cAlphaArgs(1).substr(0, 78) + " CONDENSER OUTLET NODE";
                    }
                }

                thisChiller.CondInletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                                                   state.dataIPShortCut->cAlphaArgs(5),
                                                                                   ErrorsFound,
                                                                                   state.dataIPShortCut->cCurrentModuleObject,
                                                                                   state.dataIPShortCut->cAlphaArgs(1),
                                                                                   DataLoopNode::NodeFluidType::Air,
                                                                                   DataLoopNode::NodeConnectionType::OutsideAirReference,
                                                                                   NodeInputManager::compFluidStream::Secondary,
                                                                                   DataLoopNode::ObjectIsNotParent);
                bool Okay;
                OutAirNodeManager::CheckAndAddAirNodeNumber(state, thisChiller.CondInletNodeNum, Okay);
                if (!Okay) {
                    ShowWarningError(state,
                                     state.dataIPShortCut->cCurrentModuleObject + ", Adding OutdoorAir:Node=" + state.dataIPShortCut->cAlphaArgs(5));
                }

                thisChiller.CondOutletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                                                    state.dataIPShortCut->cAlphaArgs(6),
                                                                                    ErrorsFound,
                                                                                    state.dataIPShortCut->cCurrentModuleObject,
                                                                                    state.dataIPShortCut->cAlphaArgs(1),
                                                                                    DataLoopNode::NodeFluidType::Air,
                                                                                    DataLoopNode::NodeConnectionType::Outlet,
                                                                                    NodeInputManager::compFluidStream::Secondary,
                                                                                    DataLoopNode::ObjectIsNotParent);
            } else { // WaterCooled CondenserType
                thisChiller.CondInletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                                                   state.dataIPShortCut->cAlphaArgs(5),
                                                                                   ErrorsFound,
                                                                                   state.dataIPShortCut->cCurrentModuleObject,
                                                                                   state.dataIPShortCut->cAlphaArgs(1),
                                                                                   DataLoopNode::NodeFluidType::blank,
                                                                                   DataLoopNode::NodeConnectionType::Inlet,
                                                                                   NodeInputManager::compFluidStream::Secondary,
                                                                                   DataLoopNode::ObjectIsNotParent);
                thisChiller.CondOutletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                                                    state.dataIPShortCut->cAlphaArgs(6),
                                                                                    ErrorsFound,
                                                                                    state.dataIPShortCut->cCurrentModuleObject,
                                                                                    state.dataIPShortCut->cAlphaArgs(1),
                                                                                    DataLoopNode::NodeFluidType::blank,
                                                                                    DataLoopNode::NodeConnectionType::Outlet,
                                                                                    NodeInputManager::compFluidStream::Secondary,
                                                                                    DataLoopNode::ObjectIsNotParent);
                BranchNodeConnections::TestCompSet(state,
                                                   state.dataIPShortCut->cCurrentModuleObject,
                                                   state.dataIPShortCut->cAlphaArgs(1),
                                                   state.dataIPShortCut->cAlphaArgs(5),
                                                   state.dataIPShortCut->cAlphaArgs(6),
                                                   "Condenser (unknown?) Nodes");
                // Condenser Inlet node name is necessary for Water Cooled
                if (state.dataIPShortCut->lAlphaFieldBlanks(5)) {
                    ShowSevereError(state, "Invalid, " + state.dataIPShortCut->cAlphaFieldNames(5) + " is blank ");
                    ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
                    ErrorsFound = true;
                } else if (state.dataIPShortCut->lAlphaFieldBlanks(6)) {
                    ShowSevereError(state, "Invalid, " + state.dataIPShortCut->cAlphaFieldNames(6) + " is blank ");
                    ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
                    ErrorsFound = true;
                }
            }

            thisChiller.MinPartLoadRat = state.dataIPShortCut->rNumericArgs(3);
            thisChiller.MaxPartLoadRat = state.dataIPShortCut->rNumericArgs(4);
            thisChiller.OptPartLoadRat = state.dataIPShortCut->rNumericArgs(5);
            thisChiller.TempDesCondIn = state.dataIPShortCut->rNumericArgs(6);
            thisChiller.TempRiseCoef = state.dataIPShortCut->rNumericArgs(7);
            thisChiller.TempDesEvapOut = state.dataIPShortCut->rNumericArgs(8);
            thisChiller.EvapVolFlowRate = state.dataIPShortCut->rNumericArgs(9);
            if (thisChiller.EvapVolFlowRate == DataSizing::AutoSize) {
                thisChiller.EvapVolFlowRateWasAutoSized = true;
            }

            thisChiller.CondVolFlowRate = state.dataIPShortCut->rNumericArgs(10);
            if (thisChiller.CondVolFlowRate == DataSizing::AutoSize) {
                if (thisChiller.CondenserType == DataPlant::CondenserType::WaterCooled) {
                    thisChiller.CondVolFlowRateWasAutoSized = true;
                }
            }
            thisChiller.CapRatCoef(1) = state.dataIPShortCut->rNumericArgs(11);
            thisChiller.CapRatCoef(2) = state.dataIPShortCut->rNumericArgs(12);
            thisChiller.CapRatCoef(3) = state.dataIPShortCut->rNumericArgs(13);
            if ((state.dataIPShortCut->rNumericArgs(11) + state.dataIPShortCut->rNumericArgs(12) + state.dataIPShortCut->rNumericArgs(13)) == 0.0) {
                ShowSevereError(state,
                                state.dataIPShortCut->cCurrentModuleObject +
                                    ": Sum of Capacity Ratio Coef = 0.0, chiller=" + state.dataIPShortCut->cAlphaArgs(1));
                ErrorsFound = true;
            }
            thisChiller.PowerRatCoef(1) = state.dataIPShortCut->rNumericArgs(14);
            thisChiller.PowerRatCoef(2) = state.dataIPShortCut->rNumericArgs(15);
            thisChiller.PowerRatCoef(3) = state.dataIPShortCut->rNumericArgs(16);
            thisChiller.FullLoadCoef(1) = state.dataIPShortCut->rNumericArgs(17);
            thisChiller.FullLoadCoef(2) = state.dataIPShortCut->rNumericArgs(18);
            thisChiller.FullLoadCoef(3) = state.dataIPShortCut->rNumericArgs(19);
            thisChiller.TempLowLimitEvapOut = state.dataIPShortCut->rNumericArgs(20);

            // Load Special GT Chiller Input

            thisChiller.PLBasedFuelInputCoef(1) = state.dataIPShortCut->rNumericArgs(21);
            thisChiller.PLBasedFuelInputCoef(2) = state.dataIPShortCut->rNumericArgs(22);
            thisChiller.PLBasedFuelInputCoef(3) = state.dataIPShortCut->rNumericArgs(23);

            thisChiller.TempBasedFuelInputCoef(1) = state.dataIPShortCut->rNumericArgs(24);
            thisChiller.TempBasedFuelInputCoef(2) = state.dataIPShortCut->rNumericArgs(25);
            thisChiller.TempBasedFuelInputCoef(3) = state.dataIPShortCut->rNumericArgs(26);

            thisChiller.ExhaustFlowCoef(1) = state.dataIPShortCut->rNumericArgs(27);
            thisChiller.ExhaustFlowCoef(2) = state.dataIPShortCut->rNumericArgs(28);
            thisChiller.ExhaustFlowCoef(3) = state.dataIPShortCut->rNumericArgs(29);

            thisChiller.PLBasedExhaustTempCoef(1) = state.dataIPShortCut->rNumericArgs(30);
            thisChiller.PLBasedExhaustTempCoef(2) = state.dataIPShortCut->rNumericArgs(31);
            thisChiller.PLBasedExhaustTempCoef(3) = state.dataIPShortCut->rNumericArgs(32);

            thisChiller.TempBasedExhaustTempCoef(1) = state.dataIPShortCut->rNumericArgs(33);
            thisChiller.TempBasedExhaustTempCoef(2) = state.dataIPShortCut->rNumericArgs(34);
            thisChiller.TempBasedExhaustTempCoef(3) = state.dataIPShortCut->rNumericArgs(35);

            thisChiller.HeatRecLubeEnergyCoef(1) = state.dataIPShortCut->rNumericArgs(36);
            thisChiller.HeatRecLubeEnergyCoef(2) = state.dataIPShortCut->rNumericArgs(37);
            thisChiller.HeatRecLubeEnergyCoef(3) = state.dataIPShortCut->rNumericArgs(38);

            thisChiller.UAtoCapCoef(1) = state.dataIPShortCut->rNumericArgs(39);
            thisChiller.UAtoCapCoef(2) = state.dataIPShortCut->rNumericArgs(40);

            thisChiller.GTEngineCapacity = state.dataIPShortCut->rNumericArgs(41);
            if (thisChiller.GTEngineCapacity == DataSizing::AutoSize) {
                thisChiller.GTEngineCapacityWasAutoSized = true;
            }
            thisChiller.MaxExhaustperGTPower = state.dataIPShortCut->rNumericArgs(42);
            thisChiller.DesignSteamSatTemp = state.dataIPShortCut->rNumericArgs(43);
            thisChiller.FuelHeatingValue = state.dataIPShortCut->rNumericArgs(44);

            // Get the Heat Recovery information
            // handle autosize
            thisChiller.DesignHeatRecVolFlowRate = state.dataIPShortCut->rNumericArgs(45);
            if (thisChiller.DesignHeatRecVolFlowRate > 0.0 || thisChiller.DesignHeatRecVolFlowRate == DataSizing::AutoSize) {
                thisChiller.HeatRecActive = true;
                thisChiller.HeatRecInletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                                                      state.dataIPShortCut->cAlphaArgs(7),
                                                                                      ErrorsFound,
                                                                                      state.dataIPShortCut->cCurrentModuleObject,
                                                                                      state.dataIPShortCut->cAlphaArgs(1),
                                                                                      DataLoopNode::NodeFluidType::Water,
                                                                                      DataLoopNode::NodeConnectionType::Inlet,
                                                                                      NodeInputManager::compFluidStream::Tertiary,
                                                                                      DataLoopNode::ObjectIsNotParent);
                if (thisChiller.HeatRecInletNodeNum == 0) {
                    ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(7) + '=' + state.dataIPShortCut->cAlphaArgs(7));
                    ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
                    ErrorsFound = true;
                }
                thisChiller.HeatRecOutletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                                                       state.dataIPShortCut->cAlphaArgs(8),
                                                                                       ErrorsFound,
                                                                                       state.dataIPShortCut->cCurrentModuleObject,
                                                                                       state.dataIPShortCut->cAlphaArgs(1),
                                                                                       DataLoopNode::NodeFluidType::Water,
                                                                                       DataLoopNode::NodeConnectionType::Outlet,
                                                                                       NodeInputManager::compFluidStream::Tertiary,
                                                                                       DataLoopNode::ObjectIsNotParent);
                if (thisChiller.HeatRecOutletNodeNum == 0) {
                    ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(8) + '=' + state.dataIPShortCut->cAlphaArgs(8));
                    ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
                    ErrorsFound = true;
                }
                BranchNodeConnections::TestCompSet(state,
                                                   state.dataIPShortCut->cCurrentModuleObject,
                                                   state.dataIPShortCut->cAlphaArgs(1),
                                                   state.dataIPShortCut->cAlphaArgs(7),
                                                   state.dataIPShortCut->cAlphaArgs(8),
                                                   "Heat Recovery Nodes");

                if (thisChiller.DesignHeatRecVolFlowRate == DataSizing::AutoSize) {
                    thisChiller.DesignHeatRecVolFlowRateWasAutoSized = true;
                } else {
                    PlantUtilities::RegisterPlantCompDesignFlow(state, thisChiller.HeatRecInletNodeNum, thisChiller.DesignHeatRecVolFlowRate);
                }

                // Condenser flow rate must be specified for heat reclaim, but Why couldn't this be okay??
                if (thisChiller.CondenserType == DataPlant::CondenserType::AirCooled ||
                    thisChiller.CondenserType == DataPlant::CondenserType::EvapCooled) {
                    if (thisChiller.CondVolFlowRate <= 0.0) {
                        ShowSevereError(
                            state, format("Invalid {}={:.6R}", state.dataIPShortCut->cNumericFieldNames(10), state.dataIPShortCut->rNumericArgs(10)));
                        ShowSevereError(state, "Condenser fluid flow rate must be specified for Heat Reclaim applications.");
                        ShowContinueError(state,
                                          "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
                        ErrorsFound = true;
                    }
                }

            } else {
                thisChiller.HeatRecActive = false;
                thisChiller.DesignHeatRecMassFlowRate = 0.0;
                thisChiller.HeatRecInletNodeNum = 0;
                thisChiller.HeatRecOutletNodeNum = 0;
                if ((!state.dataIPShortCut->lAlphaFieldBlanks(7)) || (!state.dataIPShortCut->lAlphaFieldBlanks(8))) {
                    ShowWarningError(state,
                                     "Since Design Heat Flow Rate = 0.0, Heat Recovery inactive for " + state.dataIPShortCut->cCurrentModuleObject +
                                         '=' + state.dataIPShortCut->cAlphaArgs(1));
                    ShowContinueError(state, "However, Node names were specified for heat recovery inlet or outlet nodes");
                }
                if (thisChiller.CondenserType == DataPlant::CondenserType::AirCooled ||
                    thisChiller.CondenserType == DataPlant::CondenserType::EvapCooled) {
                    thisChiller.CondVolFlowRate = 0.0011; // set to avoid errors in calc routine
                }
            }

            {
                auto const SELECT_CASE_var(state.dataIPShortCut->cAlphaArgs(9));
                if (SELECT_CASE_var == "CONSTANTFLOW") {
                    thisChiller.FlowMode = DataPlant::FlowMode::Constant;
                } else if (SELECT_CASE_var == "LEAVINGSETPOINTMODULATED") {
                    thisChiller.FlowMode = DataPlant::FlowMode::LeavingSetpointModulated;
                } else if (SELECT_CASE_var == "NOTMODULATED") {
                    thisChiller.FlowMode = DataPlant::FlowMode::NotModulated;
                } else {
                    ShowSevereError(state,
                                    std::string{RoutineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" +
                                        state.dataIPShortCut->cAlphaArgs(1) + "\",");
                    ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(9) + '=' + state.dataIPShortCut->cAlphaArgs(9));
                    ShowContinueError(state, "Available choices are ConstantFlow, NotModulated, or LeavingSetpointModulated");
                    ShowContinueError(state, "Flow mode NotModulated is assumed and the simulation continues.");
                    thisChiller.FlowMode = DataPlant::FlowMode::NotModulated;
                }
            }

            // Fuel Type Case Statement
            bool FuelTypeError(false);
            UtilityRoutines::ValidateFuelType(state, state.dataIPShortCut->cAlphaArgs(10), thisChiller.FuelType, FuelTypeError);
            if (FuelTypeError) {
                ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(10) + '=' + state.dataIPShortCut->cAlphaArgs(10));
                ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
                ShowContinueError(
                    state, "Valid choices are Electricity, NaturalGas, Propane, Diesel, Gasoline, FuelOilNo1, FuelOilNo2,OtherFuel1 or OtherFuel2");
                ErrorsFound = true;
                FuelTypeError = false;
            }

            thisChiller.HeatRecMaxTemp = state.dataIPShortCut->rNumericArgs(46);
            thisChiller.SizFac = state.dataIPShortCut->rNumericArgs(47);
            if (thisChiller.SizFac <= 0.0) thisChiller.SizFac = 1.0;

            //   Basin heater power as a function of temperature must be greater than or equal to 0
            thisChiller.BasinHeaterPowerFTempDiff = state.dataIPShortCut->rNumericArgs(48);
            if (state.dataIPShortCut->rNumericArgs(48) < 0.0) {
                ShowSevereError(state,
                                state.dataIPShortCut->cCurrentModuleObject + "=\"" + thisChiller.Name + "\"" +
                                    state.dataIPShortCut->cNumericFieldNames(48) + " must be >= 0");
                ErrorsFound = true;
            }

            thisChiller.BasinHeaterSetPointTemp = state.dataIPShortCut->rNumericArgs(49);

            if (thisChiller.BasinHeaterPowerFTempDiff > 0.0) {
                if (NumNums < 49) {
                    thisChiller.BasinHeaterSetPointTemp = 2.0;
                }
                if (thisChiller.BasinHeaterSetPointTemp < 2.0) {
                    ShowWarningError(state,
                                     state.dataIPShortCut->cCurrentModuleObject + ":\"" + thisChiller.Name + "\", " +
                                         state.dataIPShortCut->cNumericFieldNames(49) + " is less than 2 deg C. Freezing could occur.");
                }
            }

            if (!state.dataIPShortCut->lAlphaFieldBlanks(11)) {
                thisChiller.BasinHeaterSchedulePtr = ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(11));
                if (thisChiller.BasinHeaterSchedulePtr == 0) {
                    ShowWarningError(state,
                                     state.dataIPShortCut->cCurrentModuleObject + ", \"" + thisChiller.Name +
                                         "\" TRIM(state.dataIPShortCut->cAlphaFieldNames(11)) \"" + state.dataIPShortCut->cAlphaArgs(11) +
                                         "\" was not found. Basin heater operation will not be modeled and the simulation continues");
                }
            }

            if (NumNums > 49) {
                if (!state.dataIPShortCut->lNumericFieldBlanks(50)) {
                    thisChiller.HeatRecCapacityFraction = state.dataIPShortCut->rNumericArgs(50);
                } else {
                    thisChiller.HeatRecCapacityFraction = 1.0;
                }
            } else {
                thisChiller.HeatRecCapacityFraction = 1.0;
            }

            if (NumNums > 50) {
                if (!state.dataIPShortCut->lNumericFieldBlanks(51)) {
                    thisChiller.engineCapacityScalar = state.dataIPShortCut->rNumericArgs(51);
                } else {
                    thisChiller.engineCapacityScalar = 0.35;
                }
            } else {
                thisChiller.engineCapacityScalar = 0.35;
            }
        }

        if (ErrorsFound) {
            ShowFatalError(state, "Errors found in processing input for " + state.dataIPShortCut->cCurrentModuleObject);
        }
    }

    void GTChillerSpecs::setupOutputVariables(EnergyPlusData &state)
    {
        SetupOutputVariable(state,
                            "Chiller Drive Shaft Power",
                            OutputProcessor::Unit::W,
                            this->Power,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);
        SetupOutputVariable(state,
                            "Chiller Drive Shaft Energy",
                            OutputProcessor::Unit::J,
                            this->Energy,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            this->Name);
        SetupOutputVariable(state,
                            "Chiller Evaporator Cooling Rate",
                            OutputProcessor::Unit::W,
                            this->QEvaporator,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);
        SetupOutputVariable(state,
                            "Chiller Evaporator Cooling Energy",
                            OutputProcessor::Unit::J,
                            this->EvaporatorEnergy,
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
                            this->EvapInletTemp,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);
        SetupOutputVariable(state,
                            "Chiller Evaporator Outlet Temperature",
                            OutputProcessor::Unit::C,
                            this->EvapOutletTemp,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);
        SetupOutputVariable(state,
                            "Chiller Evaporator Mass Flow Rate",
                            OutputProcessor::Unit::kg_s,
                            this->EvapMassFlowRate,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);
        SetupOutputVariable(state,
                            "Chiller Condenser Heat Transfer Rate",
                            OutputProcessor::Unit::W,
                            this->QCondenser,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);
        SetupOutputVariable(state,
                            "Chiller Condenser Heat Transfer Energy",
                            OutputProcessor::Unit::J,
                            this->CondenserEnergy,
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
                            this->CondInletTemp,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);

        // Condenser mass flow and outlet temp are valid for water cooled
        if (this->CondenserType == DataPlant::CondenserType::WaterCooled) {
            SetupOutputVariable(state,
                                "Chiller Condenser Outlet Temperature",
                                OutputProcessor::Unit::C,
                                this->CondOutletTemp,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                this->Name);
            SetupOutputVariable(state,
                                "Chiller Condenser Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                this->CondMassFlowRate,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                this->Name);
        } else if (this->CondenserType == DataPlant::CondenserType::AirCooled) {
        } else if (this->CondenserType == DataPlant::CondenserType::EvapCooled) {
            if (this->BasinHeaterPowerFTempDiff > 0.0) {
                SetupOutputVariable(state,
                                    "Chiller Basin Heater Electricity Rate",
                                    OutputProcessor::Unit::W,
                                    this->BasinHeaterPower,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    this->Name);
                SetupOutputVariable(state,
                                    "Chiller Basin Heater Electricity Energy",
                                    OutputProcessor::Unit::J,
                                    this->BasinHeaterConsumption,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    this->Name,
                                    _,
                                    "Electricity",
                                    "CHILLERS",
                                    _,
                                    "Plant");
            }
        }

        SetupOutputVariable(state,
                            "Chiller Lube Recovered Heat Rate",
                            OutputProcessor::Unit::W,
                            this->HeatRecLubeRate,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);
        SetupOutputVariable(state,
                            "Chiller Lube Recovered Heat Energy",
                            OutputProcessor::Unit::J,
                            this->HeatRecLubeEnergy,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            this->Name,
                            _,
                            "ENERGYTRANSFER",
                            "HeatRecovery",
                            _,
                            "Plant");

        SetupOutputVariable(state,
                            "Chiller " + this->FuelType + " Rate",
                            OutputProcessor::Unit::W,
                            this->FuelEnergyUsedRate,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);

        SetupOutputVariable(state,
                            "Chiller " + this->FuelType + " Energy",
                            OutputProcessor::Unit::J,
                            this->FuelEnergyUsed,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            this->Name,
                            _,
                            this->FuelType,
                            "Cooling",
                            _,
                            "Plant");

        SetupOutputVariable(state,
                            "Chiller " + this->FuelType + " Mass Flow Rate",
                            OutputProcessor::Unit::kg_s,
                            this->FuelMassUsedRate,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);
        SetupOutputVariable(state,
                            "Chiller " + this->FuelType + " Mass",
                            OutputProcessor::Unit::kg,
                            this->FuelMassUsed,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            this->Name);
        SetupOutputVariable(state,
                            "Chiller Exhaust Temperature",
                            OutputProcessor::Unit::C,
                            this->ExhaustStackTemp,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);
        SetupOutputVariable(state,
                            "Chiller Heat Recovery Inlet Temperature",
                            OutputProcessor::Unit::C,
                            this->HeatRecInletTemp,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);
        SetupOutputVariable(state,
                            "Chiller Heat Recovery Outlet Temperature",
                            OutputProcessor::Unit::C,
                            this->HeatRecOutletTemp,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);
        SetupOutputVariable(state,
                            "Chiller Heat Recovery Mass Flow Rate",
                            OutputProcessor::Unit::kg_s,
                            this->HeatRecMdot,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);
        SetupOutputVariable(state,
                            "Chiller COP",
                            OutputProcessor::Unit::W_W,
                            this->FuelCOP,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);

        if (state.dataGlobal->AnyEnergyManagementSystemInModel) {
            SetupEMSInternalVariable(state, "Chiller Nominal Capacity", this->Name, "[W]", this->NomCap);
        }
    }

    void GTChillerSpecs::initialize(EnergyPlusData &state, bool const RunFlag, Real64 const MyLoad)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   November 2003
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for initializations of the Gas Turbine Chiller components

        // METHODOLOGY EMPLOYED:
        // Uses the status flags to trigger initializations.

        // SUBROUTINE PARAMETER DEFINITIONS:
        static constexpr std::string_view RoutineName("InitGTChiller");

        this->oneTimeInit(state);

        if (this->MyEnvrnFlag && state.dataGlobal->BeginEnvrnFlag && (state.dataPlnt->PlantFirstSizesOkayToFinalize)) {

            Real64 rho = FluidProperties::GetDensityGlycol(state,
                                                           state.dataPlnt->PlantLoop(this->CWLoopNum).FluidName,
                                                           DataGlobalConstants::CWInitConvTemp,
                                                           state.dataPlnt->PlantLoop(this->CWLoopNum).FluidIndex,
                                                           RoutineName);

            this->EvapMassFlowRateMax = rho * this->EvapVolFlowRate;
            PlantUtilities::InitComponentNodes(state,
                                               0.0,
                                               this->EvapMassFlowRateMax,
                                               this->EvapInletNodeNum,
                                               this->EvapOutletNodeNum,
                                               this->CWLoopNum,
                                               this->CWLoopSideNum,
                                               this->CWBranchNum,
                                               this->CWCompNum);

            // init maximum available condenser flow rate
            if (this->CondenserType == DataPlant::CondenserType::WaterCooled) {

                state.dataLoopNodes->Node(this->CondInletNodeNum).Temp = this->TempDesCondIn;

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
            } else { // air or evap-air
                state.dataLoopNodes->Node(this->CondInletNodeNum).MassFlowRate =
                    this->CondVolFlowRate *
                    Psychrometrics::PsyRhoAirFnPbTdbW(state, state.dataEnvrn->StdBaroPress, this->TempDesCondIn, 0.0, RoutineName);

                state.dataLoopNodes->Node(this->CondOutletNodeNum).MassFlowRate = state.dataLoopNodes->Node(this->CondInletNodeNum).MassFlowRate;
                state.dataLoopNodes->Node(this->CondInletNodeNum).MassFlowRateMaxAvail =
                    state.dataLoopNodes->Node(this->CondInletNodeNum).MassFlowRate;
                state.dataLoopNodes->Node(this->CondInletNodeNum).MassFlowRateMax = state.dataLoopNodes->Node(this->CondInletNodeNum).MassFlowRate;
                state.dataLoopNodes->Node(this->CondOutletNodeNum).MassFlowRateMax = state.dataLoopNodes->Node(this->CondInletNodeNum).MassFlowRate;
                state.dataLoopNodes->Node(this->CondInletNodeNum).MassFlowRateMinAvail = 0.0;
                state.dataLoopNodes->Node(this->CondInletNodeNum).MassFlowRateMin = 0.0;
                state.dataLoopNodes->Node(this->CondOutletNodeNum).MassFlowRateMinAvail = 0.0;
                state.dataLoopNodes->Node(this->CondOutletNodeNum).MassFlowRateMin = 0.0;
            }

            if (this->HeatRecActive) {
                rho = FluidProperties::GetDensityGlycol(state,
                                                        state.dataPlnt->PlantLoop(this->HRLoopNum).FluidName,
                                                        DataGlobalConstants::HWInitConvTemp,
                                                        state.dataPlnt->PlantLoop(this->HRLoopNum).FluidIndex,
                                                        RoutineName);
                this->DesignHeatRecMassFlowRate = rho * this->DesignHeatRecVolFlowRate;

                PlantUtilities::InitComponentNodes(state,
                                                   0.0,
                                                   this->DesignHeatRecMassFlowRate,
                                                   this->HeatRecInletNodeNum,
                                                   this->HeatRecOutletNodeNum,
                                                   this->HRLoopNum,
                                                   this->HRLoopSideNum,
                                                   this->HRBranchNum,
                                                   this->HRCompNum);
            }

            this->MyEnvrnFlag = false;
        }

        if (!state.dataGlobal->BeginEnvrnFlag) {
            this->MyEnvrnFlag = true;
        }

        if ((this->FlowMode == DataPlant::FlowMode::LeavingSetpointModulated) && (this->ModulatedFlowSetToLoop)) {
            // fix for clumsy old input that worked because loop setpoint was spread.
            //  could be removed with transition, testing , model change, period of being obsolete.
            state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempSetPoint =
                state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPoint;
            state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempSetPointHi =
                state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPointHi;
        }

        Real64 mdot;
        Real64 mdotCond;
        if ((std::abs(MyLoad) > 0.0) && RunFlag) {
            mdot = this->EvapMassFlowRateMax;
            mdotCond = this->CondMassFlowRateMax;
        } else {
            mdot = 0.0;
            mdotCond = 0.0;
        }

        PlantUtilities::SetComponentFlowRate(
            state, mdot, this->EvapInletNodeNum, this->EvapOutletNodeNum, this->CWLoopNum, this->CWLoopSideNum, this->CWBranchNum, this->CWCompNum);
        if (this->CondenserType == DataPlant::CondenserType::WaterCooled) {
            PlantUtilities::SetComponentFlowRate(state,
                                                 mdotCond,
                                                 this->CondInletNodeNum,
                                                 this->CondOutletNodeNum,
                                                 this->CDLoopNum,
                                                 this->CDLoopSideNum,
                                                 this->CDBranchNum,
                                                 this->CDCompNum);
        }

        // Initialize heat recovery flow rates at node
        if (this->HeatRecActive) {

            if (RunFlag) {
                mdot = this->DesignHeatRecMassFlowRate;
            } else {
                mdot = 0.0;
            }

            PlantUtilities::SetComponentFlowRate(state,
                                                 mdot,
                                                 this->HeatRecInletNodeNum,
                                                 this->HeatRecOutletNodeNum,
                                                 this->HRLoopNum,
                                                 this->HRLoopSideNum,
                                                 this->HRBranchNum,
                                                 this->HRCompNum);
        }
        if (this->CondenserType == DataPlant::CondenserType::EvapCooled) {
            this->BasinHeaterPower = 0.0;
        }
    }

    void GTChillerSpecs::size(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   June 2002
        //       MODIFIED       November 2013 Daeho Kang, add component sizing table entries
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for sizing Gas Turbine Chiller Components for which capacities and flow rates
        // have not been specified in the input.

        // METHODOLOGY EMPLOYED:
        // Obtains evaporator flow rate from the plant sizing array. Calculates nominal capacity from
        // the evaporator flow rate and the chilled water loop design delta T. The condenser flow rate
        // is calculated from the nominal capacity, the COP, and the condenser loop design delta T.

        // SUBROUTINE PARAMETER DEFINITIONS:
        static constexpr std::string_view RoutineName("SizeGTChiller");

        bool ErrorsFound = false;
        Real64 tmpNomCap = this->NomCap;
        Real64 tmpEvapVolFlowRate = this->EvapVolFlowRate;
        Real64 tmpCondVolFlowRate = this->CondVolFlowRate;

        int PltSizCondNum(0); // Plant Sizing index for condenser loop
        if (this->CondenserType == DataPlant::CondenserType::WaterCooled) {
            PltSizCondNum = state.dataPlnt->PlantLoop(this->CDLoopNum).PlantSizNum;
        }

        int PltSizNum = state.dataPlnt->PlantLoop(this->CWLoopNum).PlantSizNum;

        if (PltSizNum > 0) {
            if (state.dataSize->PlantSizData(PltSizNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                Real64 rho = FluidProperties::GetDensityGlycol(state,
                                                               state.dataPlnt->PlantLoop(this->CWLoopNum).FluidName,
                                                               DataGlobalConstants::CWInitConvTemp,
                                                               state.dataPlnt->PlantLoop(this->CWLoopNum).FluidIndex,
                                                               RoutineName);
                Real64 Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                                   state.dataPlnt->PlantLoop(this->CWLoopNum).FluidName,
                                                                   DataGlobalConstants::CWInitConvTemp,
                                                                   state.dataPlnt->PlantLoop(this->CWLoopNum).FluidIndex,
                                                                   RoutineName);
                tmpNomCap =
                    Cp * rho * state.dataSize->PlantSizData(PltSizNum).DeltaT * state.dataSize->PlantSizData(PltSizNum).DesVolFlowRate * this->SizFac;
            } else {
                if (this->NomCapWasAutoSized) tmpNomCap = 0.0;
            }
            if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                if (this->NomCapWasAutoSized) {
                    this->NomCap = tmpNomCap;
                    if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(state, "Chiller:CombustionTurbine", this->Name, "Design Size Nominal Capacity [W]", tmpNomCap);
                    }
                    if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(
                            state, "Chiller:CombustionTurbine", this->Name, "Initial Design Size Nominal Capacity [W]", tmpNomCap);
                    }
                } else {
                    if (this->NomCap > 0.0 && tmpNomCap > 0.0) {
                        if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                            BaseSizer::reportSizerOutput(state,
                                                         "Chiller:CombustionTurbine",
                                                         this->Name,
                                                         "Design Size Nominal Capacity [W]",
                                                         tmpNomCap,
                                                         "User-Specified Nominal Capacity [W]",
                                                         this->NomCap);
                            if (state.dataGlobal->DisplayExtraWarnings) {
                                if ((std::abs(tmpNomCap - this->NomCap) / this->NomCap) > state.dataSize->AutoVsHardSizingThreshold) {
                                    ShowMessage(state, "SizeGTChiller: Potential issue with equipment sizing for " + this->Name);
                                    ShowContinueError(state, format("User-Specified Nominal Capacity of {:.2R} [W]", this->NomCap));
                                    ShowContinueError(state, format("differs from Design Size Nominal Capacity of {:.2R} [W]", tmpNomCap));
                                    ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                        }
                        tmpNomCap = this->NomCap;
                    }
                }
            }
        } else {
            if (this->NomCapWasAutoSized && state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                ShowSevereError(state, "Autosizing of Gas Turbine Chiller nominal capacity requires a loop Sizing:Plant object");
                ShowContinueError(state, "Occurs in Gas Turbine Chiller object=" + this->Name);
                ErrorsFound = true;
            }
            if (!this->NomCapWasAutoSized && state.dataPlnt->PlantFinalSizesOkayToReport && (this->NomCap > 0.0)) {
                BaseSizer::reportSizerOutput(
                    state, "Chiller:CombustionTurbine", this->Name, "User-Specified Design Size Nominal Capacity [W]", this->NomCap);
            }
        }

        if (PltSizNum > 0) {
            if (state.dataSize->PlantSizData(PltSizNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                tmpEvapVolFlowRate = state.dataSize->PlantSizData(PltSizNum).DesVolFlowRate * this->SizFac;
            } else {
                if (this->EvapVolFlowRateWasAutoSized) tmpEvapVolFlowRate = 0.0;
            }
            if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                if (this->EvapVolFlowRateWasAutoSized) {
                    this->EvapVolFlowRate = tmpEvapVolFlowRate;
                    if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(
                            state, "Chiller:CombustionTurbine", this->Name, "Design size Design Chilled Water Flow Rate [m3/s]", tmpEvapVolFlowRate);
                    }
                    if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(state,
                                                     "Chiller:CombustionTurbine",
                                                     this->Name,
                                                     "Initial Design size Design Chilled Water Flow Rate [m3/s]",
                                                     tmpEvapVolFlowRate);
                    }
                } else {
                    if (this->EvapVolFlowRate > 0.0 && tmpEvapVolFlowRate > 0.0) {
                        if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                            BaseSizer::reportSizerOutput(state,
                                                         "Chiller:CombustionTurbine",
                                                         this->Name,
                                                         "Design size Design Chilled Water Flow Rate [m3/s]",
                                                         tmpEvapVolFlowRate,
                                                         "User-Specified Design Chilled Water Flow Rate [m3/s]",
                                                         this->EvapVolFlowRate);
                            if (state.dataGlobal->DisplayExtraWarnings) {
                                if ((std::abs(tmpEvapVolFlowRate - this->EvapVolFlowRate) / this->EvapVolFlowRate) >
                                    state.dataSize->AutoVsHardSizingThreshold) {
                                    ShowMessage(state, "SizeGTChiller: Potential issue with equipment sizing for " + this->Name);
                                    ShowContinueError(
                                        state, format("User-Specified Design Chilled Water Flow Rate of {:.5R} [m3/s]", this->EvapVolFlowRate));
                                    ShowContinueError(
                                        state,
                                        format("differs from Design Size Design Chilled Water Flow Rate of {:.5R} [m3/s]", tmpEvapVolFlowRate));
                                    ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                        }
                        tmpEvapVolFlowRate = this->EvapVolFlowRate;
                    }
                }
            }
        } else {
            if (this->EvapVolFlowRateWasAutoSized && state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                ShowSevereError(state, "Autosizing of Gas Turbine Chiller evap flow rate requires a loop Sizing:Plant object");
                ShowContinueError(state, "Occurs in Gas Turbine Chiller object=" + this->Name);
                ErrorsFound = true;
            }
            if (!this->EvapVolFlowRateWasAutoSized && state.dataPlnt->PlantFinalSizesOkayToReport && (this->EvapVolFlowRate > 0.0)) {
                BaseSizer::reportSizerOutput(
                    state, "Chiller:CombustionTurbine", this->Name, "User-Specified Design Chilled Water Flow Rate [m3/s]", this->EvapVolFlowRate);
            }
        }

        PlantUtilities::RegisterPlantCompDesignFlow(state, this->EvapInletNodeNum, tmpEvapVolFlowRate);

        if (PltSizCondNum > 0 && PltSizNum > 0) {
            if (state.dataSize->PlantSizData(PltSizNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow && tmpNomCap > 0.0) {
                Real64 rho = FluidProperties::GetDensityGlycol(state,
                                                               state.dataPlnt->PlantLoop(this->CDLoopNum).FluidName,
                                                               this->TempDesCondIn,
                                                               state.dataPlnt->PlantLoop(this->CDLoopNum).FluidIndex,
                                                               RoutineName);
                Real64 Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                                   state.dataPlnt->PlantLoop(this->CDLoopNum).FluidName,
                                                                   this->TempDesCondIn,
                                                                   state.dataPlnt->PlantLoop(this->CDLoopNum).FluidIndex,
                                                                   RoutineName);
                tmpCondVolFlowRate = tmpNomCap * (1.0 + 1.0 / this->COP) / (state.dataSize->PlantSizData(PltSizCondNum).DeltaT * Cp * rho);
            } else {
                if (this->CondVolFlowRateWasAutoSized) tmpCondVolFlowRate = 0.0;
            }
            if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                if (this->CondVolFlowRateWasAutoSized) {
                    this->CondVolFlowRate = tmpCondVolFlowRate;
                    if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(state,
                                                     "Chiller:CombustionTurbine",
                                                     this->Name,
                                                     "Design Size Design Condenser Water Flow Rate [m3/s]",
                                                     tmpCondVolFlowRate);
                    }
                    if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(state,
                                                     "Chiller:CombustionTurbine",
                                                     this->Name,
                                                     "Initial Design Size Design Condenser Water Flow Rate [m3/s]",
                                                     tmpCondVolFlowRate);
                    }
                } else {
                    if (this->CondVolFlowRate > 0.0 && tmpCondVolFlowRate > 0.0) {
                        if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                            BaseSizer::reportSizerOutput(state,
                                                         "Chiller:CombustionTurbine",
                                                         this->Name,
                                                         "Design Size Design Condenser Water Flow Rate [m3/s]",
                                                         tmpCondVolFlowRate,
                                                         "User-Specified Design Condenser Water Flow Rate [m3/s]",
                                                         this->CondVolFlowRate);
                            if (state.dataGlobal->DisplayExtraWarnings) {
                                if ((std::abs(tmpCondVolFlowRate - this->CondVolFlowRate) / this->CondVolFlowRate) >
                                    state.dataSize->AutoVsHardSizingThreshold) {
                                    ShowMessage(state, "SizeGTChiller: Potential issue with equipment sizing for " + this->Name);
                                    ShowContinueError(
                                        state, format("User-Specified Design Condenser Water Flow Rate of {:.5R} [m3/s]", this->CondVolFlowRate));
                                    ShowContinueError(
                                        state,
                                        format("differs from Design Size Design Condenser Water Flow Rate of {:.5R} [m3/s]", tmpCondVolFlowRate));
                                    ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                        }
                        tmpCondVolFlowRate = this->CondVolFlowRate;
                    }
                }
            }
        } else {
            if (this->CondVolFlowRateWasAutoSized && state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                ShowSevereError(state, "Autosizing of Gas Turbine Chiller condenser flow rate requires a condenser");
                ShowContinueError(state, "loop Sizing:Plant object");
                ShowContinueError(state, "Occurs in Gas Turbine Chiller object=" + this->Name);
                ErrorsFound = true;
            }
            if (!this->CondVolFlowRateWasAutoSized && state.dataPlnt->PlantFinalSizesOkayToReport && (this->CondVolFlowRate > 0.0)) {
                BaseSizer::reportSizerOutput(
                    state, "Chiller:CombustionTurbine", this->Name, "User-Specified Design Condenser Water Flow Rate [m3/s]", this->CondVolFlowRate);
            }
        }
        // save the design condenser water volumetric flow rate for use by the condenser water loop sizing algorithms
        if (this->CondenserType == DataPlant::CondenserType::WaterCooled)
            PlantUtilities::RegisterPlantCompDesignFlow(state, this->CondInletNodeNum, tmpCondVolFlowRate);

        Real64 GTEngineCapacityDes = this->NomCap / (this->engineCapacityScalar * this->COP);
        if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
            if (this->GTEngineCapacityWasAutoSized) {
                this->GTEngineCapacity = GTEngineCapacityDes;
                if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(
                        state, "Chiller:CombustionTurbine", this->Name, "Design Size Gas Turbine Engine Capacity [W]", GTEngineCapacityDes);
                }
                if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(
                        state, "Chiller:CombustionTurbine", this->Name, "Initial Design Size Gas Turbine Engine Capacity [W]", GTEngineCapacityDes);
                }
            } else {
                if (this->GTEngineCapacity > 0.0 && GTEngineCapacityDes > 0.0) {
                    if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(state,
                                                     "Chiller:CombustionTurbine",
                                                     this->Name,
                                                     "Design Size Gas Turbine Engine Capacity [W]",
                                                     GTEngineCapacityDes,
                                                     "User-Specified Gas Turbine Engine Capacity [W]",
                                                     this->GTEngineCapacity);
                    }
                    if (state.dataGlobal->DisplayExtraWarnings) {
                        if ((std::abs(GTEngineCapacityDes - this->GTEngineCapacity) / this->GTEngineCapacity) >
                            state.dataSize->AutoVsHardSizingThreshold) {
                            ShowMessage(state, "SizeGTChiller: Potential issue with equipment sizing for " + this->Name);
                            ShowContinueError(state, format("User-Specified Gas Turbine Engine Capacity of {:.2R} [W]", this->GTEngineCapacity));
                            ShowContinueError(state,
                                              format("differs from Design Size Gas Turbine Engine Capacity of {:.2R} [W]", GTEngineCapacityDes));
                            ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                            ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                        }
                    }
                }
            }
        }

        // autosize support for heat recovery flow rate.
        if (this->HeatRecActive) {
            Real64 tmpHeatRecVolFlowRate = this->CondVolFlowRate * this->HeatRecCapacityFraction;
            if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                if (this->DesignHeatRecVolFlowRateWasAutoSized) {
                    this->DesignHeatRecVolFlowRate = tmpHeatRecVolFlowRate;
                    if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(state,
                                                     "Chiller:CombustionTurbine",
                                                     this->Name,
                                                     "Design Size Design Heat Recovery Fluid Flow Rate [m3/s]",
                                                     tmpHeatRecVolFlowRate);
                    }
                    if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(state,
                                                     "Chiller:CombustionTurbine",
                                                     this->Name,
                                                     "Initial Design Size Design Heat Recovery Fluid Flow Rate [m3/s]",
                                                     tmpHeatRecVolFlowRate);
                    }
                } else {
                    if (this->DesignHeatRecVolFlowRate > 0.0 && tmpHeatRecVolFlowRate > 0.0) {
                        Real64 DesignHeatRecVolFlowRateUser = this->DesignHeatRecVolFlowRate;
                        if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                            if (state.dataGlobal->DoPlantSizing) {
                                BaseSizer::reportSizerOutput(state,
                                                             "Chiller:CombustionTurbine",
                                                             this->Name,
                                                             "Design Size Design Heat Recovery Fluid Flow Rate [m3/s]",
                                                             tmpHeatRecVolFlowRate,
                                                             "User-Specified Design Heat Recovery Fluid Flow Rate [m3/s]",
                                                             DesignHeatRecVolFlowRateUser);
                            } else {
                                BaseSizer::reportSizerOutput(state,
                                                             "Chiller:CombustionTurbine",
                                                             this->Name,
                                                             "User-Specified Design Heat Recovery Fluid Flow Rate [m3/s]",
                                                             DesignHeatRecVolFlowRateUser);
                            }
                            if (state.dataGlobal->DisplayExtraWarnings) {
                                if ((std::abs(tmpHeatRecVolFlowRate - DesignHeatRecVolFlowRateUser) / DesignHeatRecVolFlowRateUser) >
                                    state.dataSize->AutoVsHardSizingThreshold) {
                                    ShowMessage(state, "SizeGasTurbineChiller: Potential issue with equipment sizing for " + this->Name);
                                    ShowContinueError(
                                        state,
                                        format("User-Specified Design Heat Recovery Fluid Flow Rate of {:.5R} [m3/s]", DesignHeatRecVolFlowRateUser));
                                    ShowContinueError(state,
                                                      format("differs from Design Size Design Heat Recovery Fluid Flow Rate of {:.5R} [m3/s]",
                                                             tmpHeatRecVolFlowRate));
                                    ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                        }
                        tmpHeatRecVolFlowRate = DesignHeatRecVolFlowRateUser;
                    }
                }
            }
            if (!this->DesignHeatRecVolFlowRateWasAutoSized) tmpHeatRecVolFlowRate = this->DesignHeatRecVolFlowRate;
            // save the reference heat recovery fluid volumetric flow rate
            PlantUtilities::RegisterPlantCompDesignFlow(state, this->HeatRecInletNodeNum, tmpHeatRecVolFlowRate);
        }

        if (state.dataPlnt->PlantFinalSizesOkayToReport) {
            OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchMechType, this->Name, "Chiller:CombustionTurbine");
            OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchMechNomEff, this->Name, this->COP);
            OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchMechNomCap, this->Name, this->NomCap);
        }

        if (ErrorsFound) {
            ShowFatalError(state, "Preceding sizing errors cause program termination");
        }
    }

    void
    GTChillerSpecs::calculate(EnergyPlusData &state, Real64 &MyLoad, bool const RunFlag, DataBranchAirLoopPlant::ControlTypeEnum const EquipFlowCtrl)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Dan Fisher / Brandon Anderson
        //       DATE WRITTEN   Sept. 2000
        //       MODIFIED       Feb. 2010, Chandan Sharma, FSEC. Added basin heater
        //                      Jun. 2016, Rongpeng Zhang, LBNL. Applied the chiller supply water temperature sensor fault model
        //                      Nov. 2016, Rongpeng Zhang, LBNL. Added Fouling Chiller fault
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // simulate a vapor compression chiller using the GT model

        // METHODOLOGY EMPLOYED:
        // curve fit of performance data:

        // REFERENCES:
        // 1. BLAST Users Manual
        // 2. CHILLER User Manual

        Real64 const ExhaustCP(1.047); // Exhaust Gas Specific Heat
        static constexpr std::string_view RoutineName("CalcGTChillerModel");
        static constexpr std::string_view RoutineNameHeatRecovery("ChillerHeatRecovery");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 TempEvapOutSetPoint(0.0); // C - evaporator outlet temperature setpoint
        Real64 EvapDeltaTemp(0.0);       // C - evaporator temperature difference, water side
        Real64 PartLoadRat(0.0);         // part load ratio for efficiency calculations
        Real64 fuelEnergyIn(0.0);        // (EFUEL) Amount of Fuel Energy Required to run gas turbine
        Real64 exhaustFlow(0.0);         // (FEX) Exhaust Gas Flow Rate cubic meters per second
        Real64 exhaustTemp(0.0);         // (TEX) Exhaust Gas Temperature in C
        Real64 HeatRecOutTemp(0.0);      // Heat Recovery Fluid Outlet Temperature
        Real64 heatRecMdot(0.0);         // Heat Recovery Fluid Mass FlowRate
        Real64 MinHeatRecMdot(0.0);      // Mass Flow rate that keeps from exceeding max temp

        // set module level inlet and outlet nodes
        this->EvapMassFlowRate = 0.0;
        this->CondMassFlowRate = 0.0;
        this->Power = 0.0;
        this->QCondenser = 0.0;
        this->QEvaporator = 0.0;
        this->Energy = 0.0;
        this->CondenserEnergy = 0.0;
        this->EvaporatorEnergy = 0.0;
        this->HeatRecLubeRate = 0.0;
        this->ExhaustStackTemp = 0.0;

        // calculate end time of current time step
        Real64 CurrentEndTime = state.dataGlobal->CurrentTime + state.dataHVACGlobal->SysTimeElapsed;

        // Print warning messages only when valid and only for the first occurrence. Let summary provide statistics.
        // Wait for next time step to print warnings. If simulation iterates, print out
        // the warning for the last iteration only. Must wait for next time step to accomplish this.
        // If a warning occurs and the simulation down shifts, the warning is not valid.
        if (CurrentEndTime > this->CurrentEndTimeLast && state.dataHVACGlobal->TimeStepSys >= this->TimeStepSysLast) {
            if (this->PrintMessage) {
                ++this->MsgErrorCount;
                // Show single warning and pass additional info to ShowRecurringWarningErrorAtEnd
                if (this->MsgErrorCount < 2) {
                    ShowWarningError(state, this->MsgBuffer1 + '.');
                    ShowContinueError(state, this->MsgBuffer2);
                } else {
                    ShowRecurringWarningErrorAtEnd(
                        state, this->MsgBuffer1 + " error continues.", this->ErrCount1, this->MsgDataLast, this->MsgDataLast, _, "[C]", "[C]");
                }
            }
        }

        // save last system time step and last end time of current time step (used to determine if warning is valid)
        this->TimeStepSysLast = state.dataHVACGlobal->TimeStepSys;
        this->CurrentEndTimeLast = CurrentEndTime;

        // If Chiller load is 0 or chiller is not running then leave the subroutine.Before leaving
        // if the component control is SERIESACTIVE we set the component flow to inlet flow so that
        // flow resolver will not shut down the branch
        if (MyLoad >= 0.0 || !RunFlag) {
            if (EquipFlowCtrl == DataBranchAirLoopPlant::ControlTypeEnum::SeriesActive ||
                state.dataPlnt->PlantLoop(this->CWLoopNum).LoopSide(this->CWLoopSideNum).FlowLock == DataPlant::iFlowLock::Locked) {
                this->EvapMassFlowRate = state.dataLoopNodes->Node(this->EvapInletNodeNum).MassFlowRate;
            } else {
                this->EvapMassFlowRate = 0.0;

                PlantUtilities::SetComponentFlowRate(state,
                                                     this->EvapMassFlowRate,
                                                     this->EvapInletNodeNum,
                                                     this->EvapOutletNodeNum,
                                                     this->CWLoopNum,
                                                     this->CWLoopSideNum,
                                                     this->CWBranchNum,
                                                     this->CWCompNum);
            }
            if (this->CondenserType == DataPlant::CondenserType::WaterCooled) {
                if (state.dataPlnt->PlantLoop(this->CDLoopNum)
                        .LoopSide(this->CDLoopSideNum)
                        .Branch(this->CDBranchNum)
                        .Comp(this->CDCompNum)
                        .FlowCtrl == DataBranchAirLoopPlant::ControlTypeEnum::SeriesActive) {
                    this->CondMassFlowRate = state.dataLoopNodes->Node(this->CondInletNodeNum).MassFlowRate;
                } else {
                    this->CondMassFlowRate = 0.0;
                    PlantUtilities::SetComponentFlowRate(state,
                                                         this->CondMassFlowRate,
                                                         this->CondInletNodeNum,
                                                         this->CondOutletNodeNum,
                                                         this->CDLoopNum,
                                                         this->CDLoopSideNum,
                                                         this->CDBranchNum,
                                                         this->CDCompNum);
                }
            }

            if (this->CondenserType == DataPlant::CondenserType::EvapCooled) {
                CalcBasinHeaterPower(
                    state, this->BasinHeaterPowerFTempDiff, this->BasinHeaterSchedulePtr, this->BasinHeaterSetPointTemp, this->BasinHeaterPower);
            }
            this->PrintMessage = false;
            return;
        }

        if (this->CondenserType == DataPlant::CondenserType::AirCooled) { // Condenser inlet temp = outdoor temp
            state.dataLoopNodes->Node(this->CondInletNodeNum).Temp = state.dataLoopNodes->Node(this->CondInletNodeNum).OutAirDryBulb;
            //  Warn user if entering condenser temperature falls below 0C
            if (state.dataLoopNodes->Node(this->CondInletNodeNum).Temp < 0.0 && !state.dataGlobal->WarmupFlag) {
                this->PrintMessage = true;
                this->MsgBuffer1 =
                    "CalcGasTurbineChillerModel - Chiller:CombustionTurbine \"" + this->Name + "\" - Air Cooled Condenser Inlet Temperature below 0C";
                this->MsgBuffer2 = format("... Outdoor Dry-bulb Condition = {:6.2F} C. Occurrence info = {}, {} {}",
                                          state.dataLoopNodes->Node(this->CondInletNodeNum).Temp,
                                          state.dataEnvrn->EnvironmentName,
                                          state.dataEnvrn->CurMnDy,
                                          General::CreateSysTimeIntervalString(state));
                this->MsgDataLast = state.dataLoopNodes->Node(this->CondInletNodeNum).Temp;
            } else {
                this->PrintMessage = false;
            }
        } else if (this->CondenserType == DataPlant::CondenserType::EvapCooled) { // Condenser inlet temp = (outdoor wet bulb)
            state.dataLoopNodes->Node(this->CondInletNodeNum).Temp = state.dataLoopNodes->Node(this->CondInletNodeNum).OutAirWetBulb;
            //  Warn user if evap condenser wet bulb temperature falls below 10C
            if (state.dataLoopNodes->Node(this->CondInletNodeNum).Temp < 10.0 && !state.dataGlobal->WarmupFlag) {
                this->PrintMessage = true;
                this->MsgBuffer1 = "CalcGasTurbineChillerModel - Chiller:CombustionTurbine \"" + this->Name +
                                   "\" - Evap Cooled Condenser Inlet Temperature below 10C";
                this->MsgBuffer2 = format("... Outdoor Wet-bulb Condition = {:6.2F} C. Occurrence info = {}, {} {}",
                                          state.dataLoopNodes->Node(this->CondInletNodeNum).Temp,
                                          state.dataEnvrn->EnvironmentName,
                                          state.dataEnvrn->CurMnDy,
                                          General::CreateSysTimeIntervalString(state));
                this->MsgDataLast = state.dataLoopNodes->Node(this->CondInletNodeNum).Temp;
            } else {
                this->PrintMessage = false;
            }
        } // End of the Air Cooled/Evap Cooled Logic block

        // If not air or evap cooled then set to the condenser node that is attached to a cooling tower
        Real64 condInletTemp = state.dataLoopNodes->Node(this->CondInletNodeNum).Temp;

        // Set mass flow rates
        if (this->CondenserType == DataPlant::CondenserType::WaterCooled) {
            this->CondMassFlowRate = this->CondMassFlowRateMax;
            PlantUtilities::SetComponentFlowRate(state,
                                                 this->CondMassFlowRate,
                                                 this->CondInletNodeNum,
                                                 this->CondOutletNodeNum,
                                                 this->CDLoopNum,
                                                 this->CDLoopSideNum,
                                                 this->CDBranchNum,
                                                 this->CDCompNum);
            PlantUtilities::PullCompInterconnectTrigger(state,
                                                        this->CWLoopNum,
                                                        this->CWLoopSideNum,
                                                        this->CWBranchNum,
                                                        this->CWCompNum,
                                                        this->CondMassFlowIndex,
                                                        this->CDLoopNum,
                                                        this->CDLoopSideNum,
                                                        DataPlant::iCriteriaType::MassFlowRate,
                                                        this->CondMassFlowRate);

            if (this->CondMassFlowRate < DataBranchAirLoopPlant::MassFlowTolerance) return;
        }

        //  LOAD LOCAL VARIABLES FROM DATA STRUCTURE (for code readability)
        Real64 ChillerNomCap = this->NomCap;
        Real64 COP = this->COP;
        Real64 TempCondIn = state.dataLoopNodes->Node(this->CondInletNodeNum).Temp;
        Real64 TempEvapOut = state.dataLoopNodes->Node(this->EvapOutletNodeNum).Temp;

        // If there is a fault of chiller fouling
        if (this->FaultyChillerFoulingFlag && (!state.dataGlobal->WarmupFlag) && (!state.dataGlobal->DoingSizing) &&
            (!state.dataGlobal->KickOffSimulation)) {
            int FaultIndex = this->FaultyChillerFoulingIndex;
            Real64 NomCap_ff = ChillerNomCap;
            Real64 COP_ff = COP;

            // calculate the Faulty Chiller Fouling Factor using fault information
            this->FaultyChillerFoulingFactor = state.dataFaultsMgr->FaultsChillerFouling(FaultIndex).CalFoulingFactor(state);

            // update the Chiller nominal capacity and COP at faulty cases
            ChillerNomCap = NomCap_ff * this->FaultyChillerFoulingFactor;
            COP = COP_ff * this->FaultyChillerFoulingFactor;
        }

        // If there is a fault of Chiller SWT Sensor
        if (this->FaultyChillerSWTFlag && (!state.dataGlobal->WarmupFlag) && (!state.dataGlobal->DoingSizing) &&
            (!state.dataGlobal->KickOffSimulation)) {
            int FaultIndex = this->FaultyChillerSWTIndex;
            Real64 EvapOutletTemp_ff = TempEvapOut;

            // calculate the sensor offset using fault information
            this->FaultyChillerSWTOffset = state.dataFaultsMgr->FaultsChillerSWTSensor(FaultIndex).CalFaultOffsetAct(state);
            // update the TempEvapOut
            TempEvapOut = max(this->TempLowLimitEvapOut,
                              min(state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp, EvapOutletTemp_ff - this->FaultyChillerSWTOffset));
            this->FaultyChillerSWTOffset = EvapOutletTemp_ff - TempEvapOut;
        }

        // Calculate chiller performance from this set of performance equations.
        Real64 DeltaTemp = (TempCondIn - this->TempDesCondIn) / this->TempRiseCoef - (TempEvapOut - this->TempDesEvapOut);
        Real64 AvailNomCapRat = this->CapRatCoef(1) + this->CapRatCoef(2) * DeltaTemp + this->CapRatCoef(3) * pow_2(DeltaTemp);
        Real64 AvailChillerCap = ChillerNomCap * AvailNomCapRat;
        Real64 FullLoadPowerRat = this->PowerRatCoef(1) + this->PowerRatCoef(2) * AvailNomCapRat + this->PowerRatCoef(3) * pow_2(AvailNomCapRat);

        if (AvailChillerCap > 0.0) {
            PartLoadRat = max(this->MinPartLoadRat, min(std::abs(MyLoad) / AvailChillerCap, this->MaxPartLoadRat));
        }

        Real64 FracFullLoadPower = this->FullLoadCoef(1) + this->FullLoadCoef(2) * PartLoadRat + this->FullLoadCoef(3) * pow_2(PartLoadRat);
        Real64 OperPartLoadRat; // Actual Operating PLR
        if (AvailChillerCap > 0.0) {
            if (std::abs(MyLoad) / AvailChillerCap < this->MinPartLoadRat) {
                OperPartLoadRat = std::abs(MyLoad) / AvailChillerCap;
            } else {
                OperPartLoadRat = PartLoadRat;
            }
        } else {
            OperPartLoadRat = 0.0;
        }

        Real64 Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                           state.dataPlnt->PlantLoop(this->CWLoopNum).FluidName,
                                                           state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp,
                                                           state.dataPlnt->PlantLoop(this->CWLoopNum).FluidIndex,
                                                           RoutineName);
        // If FlowLock is True, the new resolved mdot is used to update Power, QEvap, Qcond, and
        // condenser side outlet temperature.
        if (state.dataPlnt->PlantLoop(this->CWLoopNum).LoopSide(this->CWLoopSideNum).FlowLock == DataPlant::iFlowLock::Unlocked) {
            this->PossibleSubcooling = false;
            this->QEvaporator = AvailChillerCap * OperPartLoadRat;
            Real64 FRAC;
            if (OperPartLoadRat < this->MinPartLoadRat) {
                FRAC = min(1.0, (OperPartLoadRat / this->MinPartLoadRat));
            } else {
                FRAC = 1.0;
            }
            this->Power = FracFullLoadPower * FullLoadPowerRat * AvailChillerCap / COP * FRAC;

            // Either set the flow to the Constant value or caluclate the flow for the variable volume
            if ((this->FlowMode == DataPlant::FlowMode::Constant) || (this->FlowMode == DataPlant::FlowMode::NotModulated)) {
                // Start by assuming max (design) flow
                this->EvapMassFlowRate = this->EvapMassFlowRateMax;
                // Use SetComponentFlowRate to decide actual flow
                PlantUtilities::SetComponentFlowRate(state,
                                                     this->EvapMassFlowRate,
                                                     this->EvapInletNodeNum,
                                                     this->EvapOutletNodeNum,
                                                     this->CWLoopNum,
                                                     this->CWLoopSideNum,
                                                     this->CWBranchNum,
                                                     this->CWCompNum);
                // Evaluate delta temp based on actual flow rate
                if (this->EvapMassFlowRate != 0.0) {
                    EvapDeltaTemp = this->QEvaporator / this->EvapMassFlowRate / Cp;
                } else {
                    EvapDeltaTemp = 0.0;
                }
                // Evaluate outlet temp based on delta
                this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - EvapDeltaTemp;
            } else if (this->FlowMode == DataPlant::FlowMode::LeavingSetpointModulated) {
                // Calculate the Delta Temp from the inlet temp to the chiller outlet setpoint
                {
                    auto const SELECT_CASE_var(state.dataPlnt->PlantLoop(this->CWLoopNum).LoopDemandCalcScheme);
                    if (SELECT_CASE_var == DataPlant::iLoopDemandCalcScheme::SingleSetPoint) {
                        EvapDeltaTemp =
                            state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempSetPoint;
                    } else if (SELECT_CASE_var == DataPlant::iLoopDemandCalcScheme::DualSetPointDeadBand) {
                        EvapDeltaTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp -
                                        state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempSetPointHi;
                    }
                }
                if (EvapDeltaTemp != 0.0) {
                    // Calculate desired flow to request based on load
                    this->EvapMassFlowRate = std::abs(this->QEvaporator / Cp / EvapDeltaTemp);
                    if ((this->EvapMassFlowRate - this->EvapMassFlowRateMax) > DataBranchAirLoopPlant::MassFlowTolerance)
                        this->PossibleSubcooling = true;
                    // Check to see if the Maximum is exceeded, if so set to maximum
                    this->EvapMassFlowRate = min(this->EvapMassFlowRateMax, this->EvapMassFlowRate);
                    // Use SetComponentFlowRate to decide actual flow
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
                    // Try to request zero flow
                    this->EvapMassFlowRate = 0.0;
                    // Use SetComponentFlowRate to decide actual flow
                    PlantUtilities::SetComponentFlowRate(state,
                                                         this->EvapMassFlowRate,
                                                         this->EvapInletNodeNum,
                                                         this->EvapOutletNodeNum,
                                                         this->CWLoopNum,
                                                         this->CWLoopSideNum,
                                                         this->CWBranchNum,
                                                         this->CWCompNum);
                    // No deltaT since component is not running
                    this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp;
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
                                        Cp,
                                        state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp,
                                        this->EvapOutletTemp,
                                        this->EvapMassFlowRate,
                                        this->QEvaporator);
            }

        } else { // If FlowLock is True

            this->EvapMassFlowRate = state.dataLoopNodes->Node(this->EvapInletNodeNum).MassFlowRate;
            PlantUtilities::SetComponentFlowRate(state,
                                                 this->EvapMassFlowRate,
                                                 this->EvapInletNodeNum,
                                                 this->EvapOutletNodeNum,
                                                 this->CWLoopNum,
                                                 this->CWLoopSideNum,
                                                 this->CWBranchNum,
                                                 this->CWCompNum);
            //       Some other component set the flow to 0. No reason to continue with calculations.
            if (this->EvapMassFlowRate == 0.0) {
                MyLoad = 0.0;
                if (this->CondenserType == DataPlant::CondenserType::EvapCooled) {
                    CalcBasinHeaterPower(
                        state, this->BasinHeaterPowerFTempDiff, this->BasinHeaterSchedulePtr, this->BasinHeaterSetPointTemp, this->BasinHeaterPower);
                }
                this->PrintMessage = false;
                return;
            }

            if (this->PossibleSubcooling) {
                this->QEvaporator = std::abs(MyLoad);
                EvapDeltaTemp = this->QEvaporator / this->EvapMassFlowRate / Cp;
                this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - EvapDeltaTemp;
            } else { // No subcooling in this case.No recalculation required.Still need to check chiller low temp limit
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
                            TempEvapOutSetPoint =
                                state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPoint;
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
                    }
                }
                EvapDeltaTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - TempEvapOutSetPoint;
                this->QEvaporator = std::abs(this->EvapMassFlowRate * Cp * EvapDeltaTemp);
                this->EvapOutletTemp = TempEvapOutSetPoint;
            }
            // Check that the Evap outlet temp honors both plant loop temp low limit and also the chiller low limit
            if (this->EvapOutletTemp < this->TempLowLimitEvapOut) {
                if ((state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - this->TempLowLimitEvapOut) > DataPlant::DeltaTempTol) {
                    this->EvapOutletTemp = this->TempLowLimitEvapOut;
                    EvapDeltaTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - this->EvapOutletTemp;
                    this->QEvaporator = this->EvapMassFlowRate * Cp * EvapDeltaTemp;
                } else {
                    this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp;
                    EvapDeltaTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - this->EvapOutletTemp;
                    this->QEvaporator = this->EvapMassFlowRate * Cp * EvapDeltaTemp;
                }
            }
            if (this->EvapOutletTemp < state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempMin) {
                if ((state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempMin) >
                    DataPlant::DeltaTempTol) {
                    this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempMin;
                    EvapDeltaTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - this->EvapOutletTemp;
                    this->QEvaporator = this->EvapMassFlowRate * Cp * EvapDeltaTemp;
                } else {
                    this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp;
                    EvapDeltaTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - this->EvapOutletTemp;
                    this->QEvaporator = this->EvapMassFlowRate * Cp * EvapDeltaTemp;
                }
            }
            // If load exceeds the distributed load set to the distributed load
            if (this->QEvaporator > std::abs(MyLoad)) {
                if (this->EvapMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance) {
                    this->QEvaporator = std::abs(MyLoad);
                    EvapDeltaTemp = this->QEvaporator / this->EvapMassFlowRate / Cp;
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
                                        Cp,
                                        state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp,
                                        this->EvapOutletTemp,
                                        this->EvapMassFlowRate,
                                        this->QEvaporator);
            }

            // Checks QEvaporator on the basis of the machine limits.
            if (this->QEvaporator > (AvailChillerCap * this->MaxPartLoadRat)) {
                if (this->EvapMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance) {
                    this->QEvaporator = AvailChillerCap * PartLoadRat;
                    EvapDeltaTemp = this->QEvaporator / this->EvapMassFlowRate / Cp;
                    this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - EvapDeltaTemp;
                } else {
                    this->QEvaporator = 0.0;
                    this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp;
                }
            }

            Real64 FRAC;
            if (OperPartLoadRat < this->MinPartLoadRat) {
                FRAC = min(1.0, (OperPartLoadRat / this->MinPartLoadRat));
            } else {
                FRAC = 1.0;
            }

            // Chiller is false loading below PLR = minimum unloading ratio, find PLR used for energy calculation
            this->Power = FracFullLoadPower * FullLoadPowerRat * AvailChillerCap / COP * FRAC;

            if (this->EvapMassFlowRate == 0.0) {
                this->QEvaporator = 0.0;
                this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp;
                this->Power = 0.0;
                this->PrintMessage = false;
            }
            if (this->QEvaporator == 0.0 && this->CondenserType == DataPlant::CondenserType::EvapCooled) {
                CalcBasinHeaterPower(
                    state, this->BasinHeaterPowerFTempDiff, this->BasinHeaterSchedulePtr, this->BasinHeaterSetPointTemp, this->BasinHeaterPower);
            }

        } // This is the end of the FlowLock Block

        // Now determine Cooling
        // QCondenser is calculated the same for each type, but the power consumption should be different
        //  depending on the performance coefficients used for the chiller model.
        this->QCondenser = this->Power + this->QEvaporator;

        if (this->CondenserType == DataPlant::CondenserType::WaterCooled) {

            if (this->CondMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance) {
                Real64 CpCond = FluidProperties::GetSpecificHeatGlycol(state,
                                                                       state.dataPlnt->PlantLoop(this->CDLoopNum).FluidName,
                                                                       condInletTemp,
                                                                       state.dataPlnt->PlantLoop(this->CDLoopNum).FluidIndex,
                                                                       RoutineName);
                this->CondOutletTemp = this->QCondenser / this->CondMassFlowRate / CpCond + condInletTemp;
            } else {
                ShowSevereError(state, "CalcGasTurbineChillerModel: Condenser flow = 0, for GasTurbineChiller=" + this->Name);
                ShowContinueErrorTimeStamp(state, "");
            }

        } else { // Air Cooled or Evap Cooled

            // don't care about outlet temp for Air-Cooled or Evap Cooled and there is no CondMassFlowRate and would divide by zero
            this->CondOutletTemp = condInletTemp;
        }

        // Gas Turbine Driven Portion of the Chiller:

        Real64 RPLoad;
        if (AvailChillerCap > 0) {
            RPLoad = this->Power / AvailChillerCap;
        } else {
            RPLoad = 0.0;
        }

        if (this->Power > 0) {
            Real64 PLoad = ChillerNomCap * RPLoad;
            Real64 RL = max(PLoad / ChillerNomCap, this->MinPartLoadRat);
            Real64 RL2 = pow_2(RL);

            // ??? Not sure about this Ambient Actual Temp - also do we need to have design ambient as input?

            Real64 AmbientDeltaT; // (ATAIR) Difference between ambient actual and ambient design temperatures
            if (this->CondenserType == DataPlant::CondenserType::WaterCooled) {
                AmbientDeltaT = state.dataEnvrn->OutDryBulbTemp - 25.0;
            } else { // air or evap cooled
                AmbientDeltaT = state.dataLoopNodes->Node(this->CondInletNodeNum).OutAirDryBulb - 25.0;
            }

            fuelEnergyIn = PLoad * (this->PLBasedFuelInputCoef(1) + this->PLBasedFuelInputCoef(2) * RL + this->PLBasedFuelInputCoef(3) * RL2) *
                           (this->TempBasedFuelInputCoef(1) + this->TempBasedFuelInputCoef(2) * AmbientDeltaT +
                            this->TempBasedFuelInputCoef(3) * pow_2(AmbientDeltaT));

            exhaustFlow = this->GTEngineCapacity *
                          (this->ExhaustFlowCoef(1) + this->ExhaustFlowCoef(2) * AmbientDeltaT + this->ExhaustFlowCoef(3) * pow_2(AmbientDeltaT));

            exhaustTemp = (this->PLBasedExhaustTempCoef(1) + this->PLBasedExhaustTempCoef(2) * RL + this->PLBasedExhaustTempCoef(3) * RL2) *
                              (this->TempBasedExhaustTempCoef(1) + this->TempBasedExhaustTempCoef(2) * AmbientDeltaT +
                               this->TempBasedExhaustTempCoef(3) * pow_2(AmbientDeltaT)) -
                          273;

            if (PLoad != 0.0) {
                Real64 UAtoCapRatLocal = this->UAtoCapCoef(1) * std::pow(this->GTEngineCapacity, this->UAtoCapCoef(2));
                this->ExhaustStackTemp =
                    this->DesignSteamSatTemp +
                    (exhaustTemp - this->DesignSteamSatTemp) /
                        std::exp(UAtoCapRatLocal / (max(exhaustFlow, this->MaxExhaustperGTPower * this->GTEngineCapacity) * ExhaustCP));
            }

            if (this->HeatRecActive) {
                this->HeatRecLubeRate =
                    PLoad * (this->HeatRecLubeEnergyCoef(1) + this->HeatRecLubeEnergyCoef(2) * RL + this->HeatRecLubeEnergyCoef(3) * RL2);

            } else {
                this->HeatRecLubeRate = 0.0;
            }

            // Heat Recovery Loop -  lube recovered heat
            //   If lube is not present, then the energy should be 0 at this point
            // Thigh = Energy / (Mdot*Cp) + Tlow

            // Need to set the HeatRecRatio to 1.0 if it is not modified
            Real64 HeatRecRatio = 1.0;

            if (this->HeatRecActive) {
                // This mdot is input specified mdot "Desired Flowrate", already set at node in init routine
                heatRecMdot = state.dataLoopNodes->Node(this->HeatRecInletNodeNum).MassFlowRate;
                this->HeatRecInletTemp = state.dataLoopNodes->Node(this->HeatRecInletNodeNum).Temp;
                Real64 HeatRecCp = FluidProperties::GetSpecificHeatGlycol(state,
                                                                          state.dataPlnt->PlantLoop(this->HRLoopNum).FluidName,
                                                                          this->HeatRecInletTemp,
                                                                          state.dataPlnt->PlantLoop(this->HRLoopNum).FluidIndex,
                                                                          RoutineNameHeatRecovery);

                // Don't divide by zero
                if ((heatRecMdot > 0.0) && (HeatRecCp > 0.0)) {
                    HeatRecOutTemp = (this->HeatRecLubeRate) / (heatRecMdot * HeatRecCp) + this->HeatRecInletTemp;
                } else {
                    HeatRecOutTemp = this->HeatRecInletTemp;
                }

                // Now verify that the design flowrate was large enough to prevent phase change
                if (HeatRecOutTemp > this->HeatRecMaxTemp) {
                    if (this->HeatRecMaxTemp != this->HeatRecInletTemp) {
                        MinHeatRecMdot = (this->HeatRecLubeRate) / (HeatRecCp * (this->HeatRecMaxTemp - this->HeatRecInletTemp));
                        if (MinHeatRecMdot < 0.0) MinHeatRecMdot = 0.0;
                    }

                    // Recalculate Outlet Temperature, with adjusted flowrate
                    if ((MinHeatRecMdot > 0.0) && (HeatRecCp > 0.0)) {
                        HeatRecOutTemp = (this->HeatRecLubeRate) / (MinHeatRecMdot * HeatRecCp) + this->HeatRecInletTemp;
                        HeatRecRatio = heatRecMdot / MinHeatRecMdot;
                    } else {
                        HeatRecOutTemp = this->HeatRecInletTemp;
                        HeatRecRatio = 0.0;
                    }
                }

                this->HeatRecLubeRate *= HeatRecRatio;
            } else {
                this->HeatRecInletTemp = 0.0;
                heatRecMdot = 0.0;
                HeatRecOutTemp = 0.0;
            }
        }

        this->HeatRecOutletTemp = HeatRecOutTemp;
        this->HeatRecMdot = heatRecMdot;
        this->HeatRecLubeEnergy = this->HeatRecLubeRate * (state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour);
        this->FuelEnergyIn = std::abs(fuelEnergyIn);
        this->FuelMassUsedRate = std::abs(fuelEnergyIn) / (this->FuelHeatingValue * KJtoJ);

        // Calculate Energy
        this->CondenserEnergy = this->QCondenser * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
        this->Energy = this->Power * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
        this->EvaporatorEnergy = this->QEvaporator * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;

        // check for problems BG 9/12/06 (deal with observed negative energy results)
        if (this->Energy < 0.0) { // there is a serious problem

            if (this->CondenserType == DataPlant::CondenserType::WaterCooled) {
                // first check for run away condenser loop temps (only reason yet to be observed for this?)
                if (condInletTemp > 70.0) {
                    ShowSevereError(state, "CalcGTChillerModel: Condenser loop inlet temperatures over 70.0 C for GTChiller=" + this->Name);
                    ShowContinueErrorTimeStamp(state, "");
                    ShowContinueError(state, format("Condenser loop water temperatures are too high at{:.2R}", condInletTemp));
                    ShowContinueError(state, "Check input for condenser plant loop, especially cooling tower");
                    ShowContinueError(state, format("Evaporator inlet temperature: {:.2R}", state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp));

                    ShowFatalError(state, "Program Terminates due to previous error condition");
                }
            }
            if (!state.dataGlobal->WarmupFlag) {
                if (AvailNomCapRat < 0.0) { // apparently the real reason energy goes negative
                    ShowSevereError(state, "CalcGTChillerModel: Capacity ratio below zero for GTChiller=" + this->Name);
                    ShowContinueErrorTimeStamp(state, "");
                    ShowContinueError(state, "Check input for Capacity Ratio Curve");
                    ShowContinueError(state, format("Condenser inlet temperature: {:.2R}", condInletTemp));
                    ShowContinueError(state, format("Evaporator inlet temperature: {:.2R}", state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp));
                    ShowFatalError(state, "Program Terminates due to previous error condition");
                }
            }
            // If makes it here, set limits, chiller can't have negative energy/power
            // proceeding silently for now but may want to throw error here
            this->Power = 0.0;
            this->Energy = 0.0;
        }
    }

    void GTChillerSpecs::update(EnergyPlusData &state, Real64 const MyLoad, bool const RunFlag)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Dan Fisher / Brandon Anderson
        //       DATE WRITTEN:    September 2000

        Real64 const ReportingConstant = state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;

        if (MyLoad >= 0.0 || !RunFlag) { // Chiller not running so pass inlet states to outlet states
            // set node temperatures
            state.dataLoopNodes->Node(this->EvapOutletNodeNum).Temp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp;
            state.dataLoopNodes->Node(this->CondOutletNodeNum).Temp = state.dataLoopNodes->Node(this->CondInletNodeNum).Temp;

            if (this->HeatRecActive) {
                PlantUtilities::SafeCopyPlantNode(state, this->HeatRecOutletNodeNum, this->HeatRecInletNodeNum);
                this->HeatRecInletTemp = state.dataLoopNodes->Node(this->HeatRecInletNodeNum).Temp;
                this->HeatRecOutletTemp = state.dataLoopNodes->Node(this->HeatRecOutletNodeNum).Temp;
            }

            this->Power = 0.0;
            this->QEvaporator = 0.0;
            this->QCondenser = 0.0;
            this->Energy = 0.0;
            this->EvaporatorEnergy = 0.0;
            this->CondenserEnergy = 0.0;
            this->EvapInletTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp;
            this->CondInletTemp = state.dataLoopNodes->Node(this->CondInletNodeNum).Temp;
            this->CondOutletTemp = state.dataLoopNodes->Node(this->CondOutletNodeNum).Temp;
            this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapOutletNodeNum).Temp;
            this->FuelEnergyUsedRate = 0.0;
            this->FuelMassUsedRate = 0.0;
            this->FuelEnergyUsed = 0.0;
            this->FuelMassUsed = 0.0;

            this->HeatRecLubeEnergy = 0.0;
            this->HeatRecLubeRate = 0.0;
            this->ExhaustStackTemp = 0.0;
            this->FuelCOP = 0.0;
            if (this->CondenserType == DataPlant::CondenserType::EvapCooled) {
                this->BasinHeaterPower = this->BasinHeaterPower;
                this->BasinHeaterConsumption = this->BasinHeaterPower * ReportingConstant;
            }

        } else { // Chiller is running so report calculated values
            // set node temperatures
            state.dataLoopNodes->Node(this->EvapOutletNodeNum).Temp = this->EvapOutletTemp;
            state.dataLoopNodes->Node(this->CondOutletNodeNum).Temp = this->CondOutletTemp;

            if (this->HeatRecActive) {
                PlantUtilities::SafeCopyPlantNode(state, this->HeatRecOutletNodeNum, this->HeatRecInletNodeNum);
                state.dataLoopNodes->Node(this->HeatRecOutletNodeNum).Temp = this->HeatRecOutletTemp;
            }

            this->EvapInletTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp;
            this->CondInletTemp = state.dataLoopNodes->Node(this->CondInletNodeNum).Temp;
            this->CondOutletTemp = state.dataLoopNodes->Node(this->CondOutletNodeNum).Temp;
            this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapOutletNodeNum).Temp;

            this->FuelEnergyUsedRate = this->FuelEnergyIn;
            this->FuelEnergyUsed = this->FuelEnergyUsedRate * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
            this->FuelMassUsed = this->FuelMassUsedRate * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
            if (this->FuelEnergyUsedRate != 0.0) {
                this->FuelCOP = this->QEvaporator / this->FuelEnergyUsedRate;
            } else {
                this->FuelCOP = 0.0;
            }
            if (this->CondenserType == DataPlant::CondenserType::EvapCooled) {
                this->BasinHeaterPower = this->BasinHeaterPower;
                this->BasinHeaterConsumption = this->BasinHeaterPower * ReportingConstant;
            }
        }
    }
    void GTChillerSpecs::oneTimeInit(EnergyPlusData &state)
    {
        // Init more variables
        if (this->MyFlag) {
            // Locate the chillers on the plant loops for later usage
            bool errFlag = false;
            this->setupOutputVariables(state);
            PlantUtilities::ScanPlantLoopsForObject(state,
                                                    this->Name,
                                                    this->plantTypeOfNum,
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
            if (this->CondenserType != DataPlant::CondenserType::AirCooled && this->CondenserType != DataPlant::CondenserType::EvapCooled) {
                PlantUtilities::ScanPlantLoopsForObject(state,
                                                        this->Name,
                                                        this->plantTypeOfNum,
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
                    state, this->CWLoopNum, this->CWLoopSideNum, this->CDLoopNum, this->CDLoopSideNum, this->plantTypeOfNum, true);
            }
            if (this->HeatRecActive) {
                PlantUtilities::ScanPlantLoopsForObject(state,
                                                        this->Name,
                                                        this->plantTypeOfNum,
                                                        this->HRLoopNum,
                                                        this->HRLoopSideNum,
                                                        this->HRBranchNum,
                                                        this->HRCompNum,
                                                        errFlag,
                                                        _,
                                                        _,
                                                        _,
                                                        this->HeatRecInletNodeNum,
                                                        _);
                PlantUtilities::InterConnectTwoPlantLoopSides(
                    state, this->CWLoopNum, this->CWLoopSideNum, this->HRLoopNum, this->HRLoopSideNum, this->plantTypeOfNum, true);
            }

            if (this->CondenserType != DataPlant::CondenserType::AirCooled && this->CondenserType != DataPlant::CondenserType::EvapCooled &&
                this->HeatRecActive) {
                PlantUtilities::InterConnectTwoPlantLoopSides(
                    state, this->CDLoopNum, this->CDLoopSideNum, this->HRLoopNum, this->HRLoopSideNum, this->plantTypeOfNum, false);
            }
            if (errFlag) {
                ShowFatalError(state, "InitGTChiller: Program terminated due to previous condition(s).");
            }

            if (this->FlowMode == DataPlant::FlowMode::Constant) {
                // reset flow priority
                state.dataPlnt->PlantLoop(this->CWLoopNum)
                    .LoopSide(this->CWLoopSideNum)
                    .Branch(this->CWBranchNum)
                    .Comp(this->CWCompNum)
                    .FlowPriority = DataPlant::LoopFlowStatus_NeedyIfLoopOn;
            }

            if (this->FlowMode == DataPlant::FlowMode::LeavingSetpointModulated) {
                // reset flow priority
                state.dataPlnt->PlantLoop(this->CWLoopNum)
                    .LoopSide(this->CWLoopSideNum)
                    .Branch(this->CWBranchNum)
                    .Comp(this->CWCompNum)
                    .FlowPriority = DataPlant::LoopFlowStatus_NeedyIfLoopOn;

                // check if setpoint on outlet node
                if ((state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempSetPoint == DataLoopNode::SensedNodeFlagValue) &&
                    (state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempSetPointHi == DataLoopNode::SensedNodeFlagValue)) {
                    if (!state.dataGlobal->AnyEnergyManagementSystemInModel) {
                        if (!this->ModulatedFlowErrDone) {
                            ShowWarningError(state, "Missing temperature setpoint for LeavingSetpointModulated mode chiller named " + this->Name);
                            ShowContinueError(
                                state,
                                "  A temperature setpoint is needed at the outlet node of a chiller in variable flow mode, use a SetpointManager");
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
                                ShowContinueError(
                                    state, "  A temperature setpoint is needed at the outlet node of a chiller evaporator in variable flow mode");
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
            this->MyFlag = false;
        }
    }

    ConstCOPChillerSpecs *ConstCOPChillerSpecs::factory(EnergyPlusData &state, std::string const &chillerName)
    {
        // GET INPUT
        if (state.dataPlantChillers->GetConstCOPInput) {
            ConstCOPChillerSpecs::getInput(state);
            state.dataPlantChillers->GetConstCOPInput = false;
        }
        for (auto &thisChiller : state.dataPlantChillers->ConstCOPChiller) {
            if (UtilityRoutines::MakeUPPERCase(thisChiller.Name) == chillerName) {
                return &thisChiller;
            }
        }
        ShowFatalError(state, "Could not locate constant COP chiller with name: " + chillerName);
        return nullptr;
    }

    void ConstCOPChillerSpecs::simulate(
        EnergyPlusData &state, const PlantLocation &calledFromLocation, bool FirstHVACIteration, Real64 &CurLoad, bool RunFlag)
    {
        if (calledFromLocation.loopNum == this->CWLoopNum) {
            this->initialize(state, RunFlag, CurLoad);
            auto &sim_component(
                state.dataPlnt->PlantLoop(this->CWLoopNum).LoopSide(this->CWLoopSideNum).Branch(this->CWBranchNum).Comp(this->CWCompNum));
            this->calculate(state, CurLoad, RunFlag, sim_component.FlowCtrl);
            this->update(state, CurLoad, RunFlag);
        } else if (calledFromLocation.loopNum == this->CDLoopNum) {
            PlantUtilities::UpdateChillerComponentCondenserSide(state,
                                                                this->CDLoopNum,
                                                                this->CDLoopSideNum,
                                                                this->plantTypeOfNum,
                                                                this->CondInletNodeNum,
                                                                this->CondOutletNodeNum,
                                                                this->QCondenser,
                                                                this->CondInletTemp,
                                                                this->CondOutletTemp,
                                                                this->CondMassFlowRate,
                                                                FirstHVACIteration);
        }
    }

    void ConstCOPChillerSpecs::getInput(EnergyPlusData &state)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Dan Fisher
        //       DATE WRITTEN:    April 1998

        // PURPOSE OF THIS SUBROUTINE:!This routine will get the input
        // required by the PrimaryPlantLoopManager.  As such
        // it will interact with the Input Scanner to retrieve
        // information from the input file, count the number of
        // heating and cooling loops and begin to fill the
        // arrays associated with the type PlantLoopProps.

        // SUBROUTINE PARAMETER DEFINITIONS:
        static constexpr std::string_view RoutineName("GetConstCOPChillerInput: "); // include trailing blank space

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int NumAlphas; // Number of elements in the alpha array
        int NumNums;   // Number of elements in the numeric array
        int IOStat;    // IO Status when calling get input subroutine
        bool ErrorsFound(false);

        // GET NUMBER OF ALL EQUIPMENT TYPES
        state.dataIPShortCut->cCurrentModuleObject = "Chiller:ConstantCOP";
        state.dataPlantChillers->NumConstCOPChillers =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, state.dataIPShortCut->cCurrentModuleObject);

        if (state.dataPlantChillers->NumConstCOPChillers <= 0) {
            ShowSevereError(state, "No " + state.dataIPShortCut->cCurrentModuleObject + " equipment specified in input file");
            ErrorsFound = true;
        }

        // See if load distribution manager has already gotten the input
        if (allocated(state.dataPlantChillers->ConstCOPChiller)) return;

        state.dataPlantChillers->ConstCOPChiller.allocate(state.dataPlantChillers->NumConstCOPChillers);

        // LOAD ARRAYS WITH BLAST ConstCOP CHILLER DATA
        for (int ChillerNum = 1; ChillerNum <= state.dataPlantChillers->NumConstCOPChillers; ++ChillerNum) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     state.dataIPShortCut->cCurrentModuleObject,
                                                                     ChillerNum,
                                                                     state.dataIPShortCut->cAlphaArgs,
                                                                     NumAlphas,
                                                                     state.dataIPShortCut->rNumericArgs,
                                                                     NumNums,
                                                                     IOStat,
                                                                     _,
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

            auto &thisChiller = state.dataPlantChillers->ConstCOPChiller(ChillerNum);
            thisChiller.Name = state.dataIPShortCut->cAlphaArgs(1);
            thisChiller.plantTypeOfNum = DataPlant::TypeOf_Chiller_ConstCOP;
            thisChiller.NomCap = state.dataIPShortCut->rNumericArgs(1);
            if (thisChiller.NomCap == DataSizing::AutoSize) {
                thisChiller.NomCapWasAutoSized = true;
            }
            if (state.dataIPShortCut->rNumericArgs(1) == 0.0) {
                ShowSevereError(state,
                                format("Invalid {}={:.2R}", state.dataIPShortCut->cNumericFieldNames(1), state.dataIPShortCut->rNumericArgs(1)));
                ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
                ErrorsFound = true;
            }
            thisChiller.COP = state.dataIPShortCut->rNumericArgs(2);
            if (state.dataIPShortCut->rNumericArgs(2) == 0.0) {
                ShowSevereError(state,
                                format("Invalid {}={:.2R}", state.dataIPShortCut->cNumericFieldNames(2), state.dataIPShortCut->rNumericArgs(2)));
                ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
                ErrorsFound = true;
            }

            // Set the Condenser Type from input
            if (state.dataIPShortCut->cAlphaArgs(6) == "AIRCOOLED") {
                thisChiller.CondenserType = DataPlant::CondenserType::AirCooled;
            } else if (state.dataIPShortCut->cAlphaArgs(6) == "EVAPORATIVELYCOOLED") {
                thisChiller.CondenserType = DataPlant::CondenserType::EvapCooled;
            } else if (state.dataIPShortCut->cAlphaArgs(6) == "WATERCOOLED") {
                thisChiller.CondenserType = DataPlant::CondenserType::WaterCooled;
            } else {
                ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(6) + '=' + state.dataIPShortCut->cAlphaArgs(6));
                ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
                ErrorsFound = true;
            }

            thisChiller.EvapVolFlowRate = state.dataIPShortCut->rNumericArgs(3);
            if (thisChiller.EvapVolFlowRate == DataSizing::AutoSize) {
                thisChiller.EvapVolFlowRateWasAutoSized = true;
            }
            if (thisChiller.CondenserType == DataPlant::CondenserType::AirCooled ||
                thisChiller.CondenserType == DataPlant::CondenserType::EvapCooled) { // Condenser flow rate not used for these cond types
                thisChiller.CondVolFlowRate = 0.0011;
            } else {
                thisChiller.CondVolFlowRate = state.dataIPShortCut->rNumericArgs(4);
                if (thisChiller.CondVolFlowRate == DataSizing::AutoSize) {
                    if (thisChiller.CondenserType == DataPlant::CondenserType::WaterCooled) {
                        thisChiller.CondVolFlowRateWasAutoSized = true;
                    }
                }
            }
            thisChiller.SizFac = state.dataIPShortCut->rNumericArgs(5);

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

            if (thisChiller.CondenserType == DataPlant::CondenserType::AirCooled ||
                thisChiller.CondenserType == DataPlant::CondenserType::EvapCooled) {
                // Connection not required for air or evap cooled condenser
                // If the condenser inlet is blank for air cooled and evap cooled condensers then supply a generic name
                //  since it is not used elsewhere for connection
                if (state.dataIPShortCut->lAlphaFieldBlanks(4)) {
                    if (len(state.dataIPShortCut->cAlphaArgs(1)) <
                        DataGlobalConstants::MaxNameLength - 21) { // protect against long name leading to > 100 chars
                        state.dataIPShortCut->cAlphaArgs(4) = state.dataIPShortCut->cAlphaArgs(1) + " CONDENSER INLET NODE";
                    } else {
                        state.dataIPShortCut->cAlphaArgs(4) = state.dataIPShortCut->cAlphaArgs(1).substr(0, 79) + " CONDENSER INLET NODE";
                    }
                }
                if (state.dataIPShortCut->lAlphaFieldBlanks(5)) {
                    if (len(state.dataIPShortCut->cAlphaArgs(1)) <
                        DataGlobalConstants::MaxNameLength - 22) { // protect against long name leading to > 100 chars
                        state.dataIPShortCut->cAlphaArgs(5) = state.dataIPShortCut->cAlphaArgs(1) + " CONDENSER OUTLET NODE";
                    } else {
                        state.dataIPShortCut->cAlphaArgs(5) = state.dataIPShortCut->cAlphaArgs(1).substr(0, 78) + " CONDENSER OUTLET NODE";
                    }
                }

                thisChiller.CondInletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                                                   state.dataIPShortCut->cAlphaArgs(4),
                                                                                   ErrorsFound,
                                                                                   state.dataIPShortCut->cCurrentModuleObject,
                                                                                   state.dataIPShortCut->cAlphaArgs(1),
                                                                                   DataLoopNode::NodeFluidType::Air,
                                                                                   DataLoopNode::NodeConnectionType::OutsideAirReference,
                                                                                   NodeInputManager::compFluidStream::Secondary,
                                                                                   DataLoopNode::ObjectIsNotParent);
                bool Okay;
                OutAirNodeManager::CheckAndAddAirNodeNumber(state, thisChiller.CondInletNodeNum, Okay);
                if (!Okay) {
                    ShowWarningError(state,
                                     state.dataIPShortCut->cCurrentModuleObject +
                                         ", Adding OutdoorAir:DataLoopNode::Node=" + state.dataIPShortCut->cAlphaArgs(4));
                }

                thisChiller.CondOutletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                                                    state.dataIPShortCut->cAlphaArgs(5),
                                                                                    ErrorsFound,
                                                                                    state.dataIPShortCut->cCurrentModuleObject,
                                                                                    state.dataIPShortCut->cAlphaArgs(1),
                                                                                    DataLoopNode::NodeFluidType::Air,
                                                                                    DataLoopNode::NodeConnectionType::Outlet,
                                                                                    NodeInputManager::compFluidStream::Secondary,
                                                                                    DataLoopNode::ObjectIsNotParent);
            } else if (thisChiller.CondenserType == DataPlant::CondenserType::WaterCooled) {
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
                                                                                    NodeInputManager ::compFluidStream::Secondary,
                                                                                    DataLoopNode::ObjectIsNotParent);
                BranchNodeConnections::TestCompSet(state,
                                                   state.dataIPShortCut->cCurrentModuleObject,
                                                   state.dataIPShortCut->cAlphaArgs(1),
                                                   state.dataIPShortCut->cAlphaArgs(4),
                                                   state.dataIPShortCut->cAlphaArgs(5),
                                                   "Condenser Water Nodes");
                // Condenser Inlet node name is necessary for Water Cooled
                if (state.dataIPShortCut->lAlphaFieldBlanks(4)) {
                    ShowSevereError(state, "Invalid, " + state.dataIPShortCut->cAlphaFieldNames(4) + "is blank ");
                    ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
                    ErrorsFound = true;
                } else if (state.dataIPShortCut->lAlphaFieldBlanks(5)) {
                    ShowSevereError(state, "Invalid, " + state.dataIPShortCut->cAlphaFieldNames(5) + "is blank ");
                    ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
                    ErrorsFound = true;
                }
            } else {
                thisChiller.CondInletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                                                   state.dataIPShortCut->cAlphaArgs(4),
                                                                                   ErrorsFound,
                                                                                   state.dataIPShortCut->cCurrentModuleObject,
                                                                                   state.dataIPShortCut->cAlphaArgs(1),
                                                                                   DataLoopNode::NodeFluidType::blank,
                                                                                   DataLoopNode::NodeConnectionType::Inlet,
                                                                                   NodeInputManager::compFluidStream::Secondary,
                                                                                   DataLoopNode::ObjectIsNotParent);
                thisChiller.CondOutletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                                                    state.dataIPShortCut->cAlphaArgs(5),
                                                                                    ErrorsFound,
                                                                                    state.dataIPShortCut->cCurrentModuleObject,
                                                                                    state.dataIPShortCut->cAlphaArgs(1),
                                                                                    DataLoopNode::NodeFluidType::blank,
                                                                                    DataLoopNode::NodeConnectionType::Outlet,
                                                                                    NodeInputManager::compFluidStream::Secondary,
                                                                                    DataLoopNode::ObjectIsNotParent);
                BranchNodeConnections::TestCompSet(state,
                                                   state.dataIPShortCut->cCurrentModuleObject,
                                                   state.dataIPShortCut->cAlphaArgs(1),
                                                   state.dataIPShortCut->cAlphaArgs(4),
                                                   state.dataIPShortCut->cAlphaArgs(5),
                                                   "Condenser (unknown?) Nodes");
                // Condenser Inlet node name is necessary for Water Cooled
                if (state.dataIPShortCut->lAlphaFieldBlanks(4)) {
                    ShowSevereError(state, "Invalid, " + state.dataIPShortCut->cAlphaFieldNames(4) + "is blank ");
                    ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
                    ErrorsFound = true;
                } else if (state.dataIPShortCut->lAlphaFieldBlanks(5)) {
                    ShowSevereError(state, "Invalid, " + state.dataIPShortCut->cAlphaFieldNames(5) + "is blank ");
                    ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
                    ErrorsFound = true;
                }
            }

            {
                auto const SELECT_CASE_var(state.dataIPShortCut->cAlphaArgs(7));
                if (SELECT_CASE_var == "CONSTANTFLOW") {
                    thisChiller.FlowMode = DataPlant::FlowMode::Constant;
                } else if (SELECT_CASE_var == "LEAVINGSETPOINTMODULATED") {
                    thisChiller.FlowMode = DataPlant::FlowMode::LeavingSetpointModulated;
                } else if (SELECT_CASE_var == "NOTMODULATED") {
                    thisChiller.FlowMode = DataPlant::FlowMode::NotModulated;
                } else {
                    ShowSevereError(state,
                                    std::string{RoutineName} + state.dataIPShortCut->cCurrentModuleObject + "=\"" +
                                        state.dataIPShortCut->cAlphaArgs(1) + "\",");
                    ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(7) + '=' + state.dataIPShortCut->cAlphaArgs(7));
                    ShowContinueError(state, "Available choices are ConstantFlow, NotModulated, or LeavingSetpointModulated");
                    ShowContinueError(state, "Flow mode NotModulated is assumed and the simulation continues.");
                    thisChiller.FlowMode = DataPlant::FlowMode::NotModulated;
                }
            }

            //   Basin heater power as a function of temperature must be greater than or equal to 0
            thisChiller.BasinHeaterPowerFTempDiff = state.dataIPShortCut->rNumericArgs(6);
            if (state.dataIPShortCut->rNumericArgs(6) < 0.0) {
                ShowSevereError(state,
                                state.dataIPShortCut->cCurrentModuleObject + ", \"" + thisChiller.Name +
                                    "\" TRIM(state.dataIPShortCut->cNumericFieldNames(6)) must be >= 0");
                ErrorsFound = true;
            }

            thisChiller.BasinHeaterSetPointTemp = state.dataIPShortCut->rNumericArgs(7);

            if (thisChiller.BasinHeaterPowerFTempDiff > 0.0) {
                if (NumNums < 7) {
                    thisChiller.BasinHeaterSetPointTemp = 2.0;
                }
                if (thisChiller.BasinHeaterSetPointTemp < 2.0) {
                    ShowWarningError(state,
                                     state.dataIPShortCut->cCurrentModuleObject + ":\"" + thisChiller.Name + "\", " +
                                         state.dataIPShortCut->cNumericFieldNames(7) + " is less than 2 deg C. Freezing could occur.");
                }
            }

            if (!state.dataIPShortCut->lAlphaFieldBlanks(8)) {
                thisChiller.BasinHeaterSchedulePtr = ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(8));
                if (thisChiller.BasinHeaterSchedulePtr == 0) {
                    ShowWarningError(state,
                                     state.dataIPShortCut->cCurrentModuleObject + ", \"" + thisChiller.Name +
                                         "\" TRIM(state.dataIPShortCut->cAlphaFieldNames(8)) \"" + state.dataIPShortCut->cAlphaArgs(8) +
                                         "\" was not found. Basin heater operation will not be modeled and the simulation continues");
                }
            }
        }

        if (ErrorsFound) {
            ShowFatalError(state, "Errors found in processing input for " + state.dataIPShortCut->cCurrentModuleObject);
        }
    }

    void ConstCOPChillerSpecs::setupOutputVariables(EnergyPlusData &state)
    {
        SetupOutputVariable(state,
                            "Chiller Electricity Rate",
                            OutputProcessor::Unit::W,
                            this->Power,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);
        SetupOutputVariable(state,
                            "Chiller Electricity Energy",
                            OutputProcessor::Unit::J,
                            this->Energy,
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
                            this->QEvaporator,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);
        SetupOutputVariable(state,
                            "Chiller Evaporator Cooling Energy",
                            OutputProcessor::Unit::J,
                            this->EvaporatorEnergy,
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
                            this->EvapInletTemp,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);
        SetupOutputVariable(state,
                            "Chiller Evaporator Outlet Temperature",
                            OutputProcessor::Unit::C,
                            this->EvapOutletTemp,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);
        SetupOutputVariable(state,
                            "Chiller Evaporator Mass Flow Rate",
                            OutputProcessor::Unit::kg_s,
                            this->EvapMassFlowRate,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);
        SetupOutputVariable(state,
                            "Chiller COP",
                            OutputProcessor::Unit::W_W,
                            this->ActualCOP,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);
        SetupOutputVariable(state,
                            "Chiller Condenser Heat Transfer Rate",
                            OutputProcessor::Unit::W,
                            this->QCondenser,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);
        SetupOutputVariable(state,
                            "Chiller Condenser Heat Transfer Energy",
                            OutputProcessor::Unit::J,
                            this->CondenserEnergy,
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
                            this->CondInletTemp,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);

        // Condenser mass flow and outlet temp are valid for water cooled
        if (this->CondenserType == DataPlant::CondenserType::WaterCooled) {
            SetupOutputVariable(state,
                                "Chiller Condenser Outlet Temperature",
                                OutputProcessor::Unit::C,
                                this->CondOutletTemp,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                this->Name);
            SetupOutputVariable(state,
                                "Chiller Condenser Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                this->CondMassFlowRate,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                this->Name);
        } else if (this->CondenserType == DataPlant::CondenserType::AirCooled) {
        } else if (this->CondenserType == DataPlant::CondenserType::EvapCooled) {
            if (this->BasinHeaterPowerFTempDiff > 0.0) {
                SetupOutputVariable(state,
                                    "Chiller Basin Heater Electricity Rate",
                                    OutputProcessor::Unit::W,
                                    this->BasinHeaterPower,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    this->Name);
                SetupOutputVariable(state,
                                    "Chiller Basin Heater Electricity Energy",
                                    OutputProcessor::Unit::J,
                                    this->BasinHeaterConsumption,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Summed,
                                    this->Name,
                                    _,
                                    "Electricity",
                                    "CHILLERS",
                                    _,
                                    "Plant");
            }
        }
        if (state.dataGlobal->AnyEnergyManagementSystemInModel) {
            SetupEMSInternalVariable(state, "Chiller Nominal Capacity", this->Name, "[W]", this->NomCap);
        }
    }

    void ConstCOPChillerSpecs::initialize(EnergyPlusData &state, bool const RunFlag, Real64 const MyLoad)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Chandan Sharma
        //       DATE WRITTEN   September 2010
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for initializations of the Electric Chiller components

        // METHODOLOGY EMPLOYED:
        // Uses the status flags to trigger initializations.

        // REFERENCES:
        // Based on InitElectricChiller from Fred Buhl

        // SUBROUTINE PARAMETER DEFINITIONS:
        static constexpr std::string_view RoutineName("InitConstCOPChiller");
        constexpr Real64 TempDesCondIn(25.0); // Design condenser inlet temp. C

        this->oneTimeInit(state);

        // Initialize critical Demand Side Variables at the beginning of each environment
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

            // init maximum available condenser flow rate
            if (this->CondenserType == DataPlant::CondenserType::WaterCooled) {

                state.dataLoopNodes->Node(this->CondInletNodeNum).Temp = TempDesCondIn;

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
            } else { // air or evap-air
                state.dataLoopNodes->Node(this->CondInletNodeNum).MassFlowRate =
                    this->CondVolFlowRate * Psychrometrics::PsyRhoAirFnPbTdbW(state, state.dataEnvrn->StdBaroPress, TempDesCondIn, 0.0, RoutineName);

                state.dataLoopNodes->Node(this->CondOutletNodeNum).MassFlowRate = state.dataLoopNodes->Node(this->CondInletNodeNum).MassFlowRate;
                state.dataLoopNodes->Node(this->CondInletNodeNum).MassFlowRateMaxAvail =
                    state.dataLoopNodes->Node(this->CondInletNodeNum).MassFlowRate;
                state.dataLoopNodes->Node(this->CondInletNodeNum).MassFlowRateMax = state.dataLoopNodes->Node(this->CondInletNodeNum).MassFlowRate;
                state.dataLoopNodes->Node(this->CondOutletNodeNum).MassFlowRateMax = state.dataLoopNodes->Node(this->CondInletNodeNum).MassFlowRate;
                state.dataLoopNodes->Node(this->CondInletNodeNum).MassFlowRateMinAvail = 0.0;
                state.dataLoopNodes->Node(this->CondInletNodeNum).MassFlowRateMin = 0.0;
                state.dataLoopNodes->Node(this->CondOutletNodeNum).MassFlowRateMinAvail = 0.0;
                state.dataLoopNodes->Node(this->CondOutletNodeNum).MassFlowRateMin = 0.0;
            }
            this->MyEnvrnFlag = false;
        }

        if (!state.dataGlobal->BeginEnvrnFlag) {
            this->MyEnvrnFlag = true;
        }
        if ((this->FlowMode == DataPlant::FlowMode::LeavingSetpointModulated) && (this->ModulatedFlowSetToLoop)) {
            // fix for clumsy old input that worked because loop setpoint was spread.
            //  could be removed with transition, testing , model change, period of being obsolete.
            state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempSetPoint =
                state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPoint;
            state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempSetPointHi =
                state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPointHi;
        }

        Real64 mdot;
        Real64 mdotCond;

        if ((MyLoad < 0.0) && RunFlag) {
            mdot = this->EvapMassFlowRateMax;
            mdotCond = this->CondMassFlowRateMax;
        } else {
            mdot = 0.0;
            mdotCond = 0.0;
        }

        PlantUtilities::SetComponentFlowRate(
            state, mdot, this->EvapInletNodeNum, this->EvapOutletNodeNum, this->CWLoopNum, this->CWLoopSideNum, this->CWBranchNum, this->CWCompNum);
        if (this->CondenserType == DataPlant::CondenserType::WaterCooled) {
            PlantUtilities::SetComponentFlowRate(state,
                                                 mdotCond,
                                                 this->CondInletNodeNum,
                                                 this->CondOutletNodeNum,
                                                 this->CDLoopNum,
                                                 this->CDLoopSideNum,
                                                 this->CDBranchNum,
                                                 this->CDCompNum);
        }

        if (this->CondenserType == DataPlant::CondenserType::EvapCooled) {
            this->BasinHeaterPower = 0.0;
        }
    }

    void ConstCOPChillerSpecs::size(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   March 2008
        //       MODIFIED       November 2013 Daeho Kang, add component sizing table entries
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for sizing Constant COP Chiller Components for which capacities and flow rates
        // have not been specified in the input.

        // METHODOLOGY EMPLOYED:
        // Obtains evaporator flow rate from the plant sizing array. Calculates nominal capacity from
        // the evaporator flow rate and the chilled water loop design delta T. The condenser flow rate
        // is calculated from the nominal capacity, the COP, and the condenser loop design delta T.

        // SUBROUTINE PARAMETER DEFINITIONS:
        static constexpr std::string_view RoutineName("SizeConstCOPChiller");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 tmpNomCap;           // local nominal capacity cooling power
        Real64 tmpEvapVolFlowRate;  // local evaporator design volume flow rate
        Real64 tmpCondVolFlowRate;  // local condenser design volume flow rate
        Real64 EvapVolFlowRateUser; // Hardsized evaporator flow for reporting
        Real64 NomCapUser;          // Hardsized reference capacity for reporting

        int PltSizCondNum = 0;
        bool ErrorsFound = false;
        tmpNomCap = this->NomCap;
        tmpEvapVolFlowRate = this->EvapVolFlowRate;
        tmpCondVolFlowRate = this->CondVolFlowRate;

        if (this->CondenserType == DataPlant::CondenserType::WaterCooled) {
            PltSizCondNum = state.dataPlnt->PlantLoop(this->CDLoopNum).PlantSizNum;
        }

        int PltSizNum = state.dataPlnt->PlantLoop(this->CWLoopNum).PlantSizNum;

        if (PltSizNum > 0) {
            if (state.dataSize->PlantSizData(PltSizNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                Real64 rho = FluidProperties::GetDensityGlycol(state,
                                                               state.dataPlnt->PlantLoop(this->CWLoopNum).FluidName,
                                                               DataGlobalConstants::CWInitConvTemp,
                                                               state.dataPlnt->PlantLoop(this->CWLoopNum).FluidIndex,
                                                               RoutineName);
                Real64 Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                                   state.dataPlnt->PlantLoop(this->CWLoopNum).FluidName,
                                                                   DataGlobalConstants::CWInitConvTemp,
                                                                   state.dataPlnt->PlantLoop(this->CWLoopNum).FluidIndex,
                                                                   RoutineName);
                tmpNomCap =
                    Cp * rho * state.dataSize->PlantSizData(PltSizNum).DeltaT * state.dataSize->PlantSizData(PltSizNum).DesVolFlowRate * this->SizFac;
            } else {
                if (this->NomCapWasAutoSized) tmpNomCap = 0.0;
            }
            if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                if (this->NomCapWasAutoSized) {
                    this->NomCap = tmpNomCap;
                    if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(state, "Chiller:ConstantCOP", this->Name, "Design Size Nominal Capacity [W]", tmpNomCap);
                    }
                    if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(state, "Chiller:ConstantCOP", this->Name, "Initial Design Size Nominal Capacity [W]", tmpNomCap);
                    }
                } else { // Hard-size with sizing data
                    if (this->NomCap > 0.0 && tmpNomCap > 0.0) {
                        NomCapUser = this->NomCap;
                        if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                            BaseSizer::reportSizerOutput(state,
                                                         "Chiller:ConstantCOP",
                                                         this->Name,
                                                         "Design Size Nominal Capacity [W]",
                                                         tmpNomCap,
                                                         "User-Specified Nominal Capacity [W]",
                                                         NomCapUser);
                            if (state.dataGlobal->DisplayExtraWarnings) {
                                if ((std::abs(tmpNomCap - NomCapUser) / NomCapUser) > state.dataSize->AutoVsHardSizingThreshold) {
                                    ShowMessage(state, "SizeChillerConstantCOP: Potential issue with equipment sizing for " + this->Name);
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
            if (this->NomCapWasAutoSized && state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                ShowSevereError(state, "Autosizing of Constant COP Chiller nominal capacity requires a loop Sizing:Plant object");
                ShowContinueError(state, "Occurs in Chiller:ConstantCOP object=" + this->Name);
                ErrorsFound = true;
            }
            if (!this->NomCapWasAutoSized && state.dataPlnt->PlantFinalSizesOkayToReport && (this->NomCap > 0.0)) {
                BaseSizer::reportSizerOutput(state, "Chiller:ConstantCOP", this->Name, "User-Specified Nominal Capacity [W]", this->NomCap);
            }
        }

        if (PltSizNum > 0) {
            if (state.dataSize->PlantSizData(PltSizNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                tmpEvapVolFlowRate = state.dataSize->PlantSizData(PltSizNum).DesVolFlowRate * this->SizFac;
            } else {
                if (this->EvapVolFlowRateWasAutoSized) tmpEvapVolFlowRate = 0.0;
            }
            if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                if (this->EvapVolFlowRateWasAutoSized) {
                    this->EvapVolFlowRate = tmpEvapVolFlowRate;
                    if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(
                            state, "Chiller:ConstantCOP", this->Name, "Design Size Design Chilled Water Flow Rate [m3/s]", tmpEvapVolFlowRate);
                    }
                    if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(state,
                                                     "Chiller:ConstantCOP",
                                                     this->Name,
                                                     "Initial Design Size Design Chilled Water Flow Rate [m3/s]",
                                                     tmpEvapVolFlowRate);
                    }
                } else {
                    if (this->EvapVolFlowRate > 0.0 && tmpEvapVolFlowRate > 0.0) {
                        EvapVolFlowRateUser = this->EvapVolFlowRate;
                        if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                            BaseSizer::reportSizerOutput(state,
                                                         "Chiller:ConstantCOP",
                                                         this->Name,
                                                         "Design Size Design Chilled Water Flow Rate [m3/s]",
                                                         tmpEvapVolFlowRate,
                                                         "User-Specified Design Chilled Water Flow Rate [m3/s]",
                                                         EvapVolFlowRateUser);
                            if (state.dataGlobal->DisplayExtraWarnings) {
                                if ((std::abs(tmpEvapVolFlowRate - EvapVolFlowRateUser) / EvapVolFlowRateUser) >
                                    state.dataSize->AutoVsHardSizingThreshold) {
                                    ShowMessage(state, "SizeChillerConstantCOP: Potential issue with equipment sizing for " + this->Name);
                                    ShowContinueError(state,
                                                      format("User-Specified Design Chilled Water Flow Rate of {:.5R} [m3/s]", EvapVolFlowRateUser));
                                    ShowContinueError(
                                        state,
                                        format("differs from Design Size Design Chilled Water Flow Rate of {:.5R} [m3/s]", tmpEvapVolFlowRate));
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
            if (this->EvapVolFlowRateWasAutoSized && state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                ShowSevereError(state, "Autosizing of Constant COP Chiller evap flow rate requires a loop Sizing:Plant object");
                ShowContinueError(state, "Occurs in Chiller:ConstantCOP object=" + this->Name);
                ErrorsFound = true;
            }
            if (!this->EvapVolFlowRateWasAutoSized && state.dataPlnt->PlantFinalSizesOkayToReport && (this->EvapVolFlowRate > 0.0)) {
                BaseSizer::reportSizerOutput(
                    state, "Chiller:ConstantCOP", this->Name, "User-Specified Design Chilled Water Flow Rate [m3/s]", this->EvapVolFlowRate);
            }
        }

        PlantUtilities::RegisterPlantCompDesignFlow(state, this->EvapInletNodeNum, tmpEvapVolFlowRate);

        if (this->CondenserType == DataPlant::CondenserType::WaterCooled) {
            if (PltSizCondNum > 0 && PltSizNum > 0) {
                if (state.dataSize->PlantSizData(PltSizNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow && tmpNomCap > 0.0) {
                    Real64 rho = FluidProperties::GetDensityGlycol(state,
                                                                   state.dataPlnt->PlantLoop(this->CDLoopNum).FluidName,
                                                                   29.44,
                                                                   state.dataPlnt->PlantLoop(this->CDLoopNum).FluidIndex,
                                                                   RoutineName);
                    Real64 Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                                       state.dataPlnt->PlantLoop(this->CDLoopNum).FluidName,
                                                                       29.44,
                                                                       state.dataPlnt->PlantLoop(this->CDLoopNum).FluidIndex,
                                                                       RoutineName);
                    tmpCondVolFlowRate = tmpNomCap * (1.0 + 1.0 / this->COP) / (state.dataSize->PlantSizData(PltSizCondNum).DeltaT * Cp * rho);
                } else {
                    if (this->CondVolFlowRateWasAutoSized) tmpCondVolFlowRate = 0.0;
                }
                if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                    if (this->CondVolFlowRateWasAutoSized) {
                        this->CondVolFlowRate = tmpCondVolFlowRate;
                        if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                            BaseSizer::reportSizerOutput(
                                state, "Chiller:ConstantCOP", this->Name, "Design Size Design Condenser Water Flow Rate [m3/s]", tmpCondVolFlowRate);
                        }
                        if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                            BaseSizer::reportSizerOutput(state,
                                                         "Chiller:ConstantCOP",
                                                         this->Name,
                                                         "Initial Design Size Design Condenser Water Flow Rate [m3/s]",
                                                         tmpCondVolFlowRate);
                        }
                    } else {
                        if (this->CondVolFlowRate > 0.0 && tmpCondVolFlowRate > 0.0) {
                            Real64 CondVolFlowRateUser = this->CondVolFlowRate;
                            if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                                BaseSizer::reportSizerOutput(state,
                                                             "Chiller:ConstantCOP",
                                                             this->Name,
                                                             "Design Size Design Condenser Water Flow Rate [m3/s]",
                                                             tmpCondVolFlowRate,
                                                             "User-Specified Design Condenser Water Flow Rate [m3/s]",
                                                             CondVolFlowRateUser);
                                if (state.dataGlobal->DisplayExtraWarnings) {
                                    if ((std::abs(tmpCondVolFlowRate - CondVolFlowRateUser) / CondVolFlowRateUser) >
                                        state.dataSize->AutoVsHardSizingThreshold) {
                                        ShowMessage(state, "SizeChillerConstantCOP: Potential issue with equipment sizing for " + this->Name);
                                        ShowContinueError(
                                            state, format("User-Specified Design Condenser Water Flow Rate of {:.5R} [m3/s]", CondVolFlowRateUser));
                                        ShowContinueError(
                                            state,
                                            format("differs from Design Size Design Condenser Water Flow Rate of {:.5R} [m3/s]", tmpCondVolFlowRate));
                                        ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                                        ShowContinueError(state,
                                                          "Verify that the value entered is intended and is consistent with other components.");
                                    }
                                }
                            }
                            tmpCondVolFlowRate = CondVolFlowRateUser;
                        }
                    }
                }
            } else {
                if (this->CondVolFlowRateWasAutoSized && state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                    ShowSevereError(state, "Autosizing of Constant COP Chiller condenser flow rate requires a condenser");
                    ShowContinueError(state, "loop Sizing:Plant object");
                    ShowContinueError(state, "Occurs in Chiller:ConstantCOP object=" + this->Name);
                    ErrorsFound = true;
                }
                if (!this->CondVolFlowRateWasAutoSized && state.dataPlnt->PlantFinalSizesOkayToReport && (this->CondVolFlowRate > 0.0)) {
                    BaseSizer::reportSizerOutput(
                        state, "Chiller:ConstantCOP", this->Name, "User-Specified Design Condenser Water Flow Rate [m3/s]", this->CondVolFlowRate);
                }
            }
        }

        // save the design condenser water volumetric flow rate for use by the condenser water loop sizing algorithms
        if (this->CondenserType == DataPlant::CondenserType::WaterCooled)
            PlantUtilities::RegisterPlantCompDesignFlow(state, this->CondInletNodeNum, tmpCondVolFlowRate);

        if (ErrorsFound) {
            ShowFatalError(state, "Preceding sizing errors cause program termination");
        }

        // create predefined report
        if (state.dataPlnt->PlantFinalSizesOkayToReport) {
            OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchMechType, this->Name, "Chiller:ConstantCOP");
            OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchMechNomEff, this->Name, this->COP);
            OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchMechNomCap, this->Name, this->NomCap);
        }
    }

    void ConstCOPChillerSpecs::calculate(EnergyPlusData &state,
                                         Real64 &MyLoad,
                                         bool const RunFlag,
                                         DataBranchAirLoopPlant::ControlTypeEnum const EquipFlowCtrl)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Dan Fisher
        //       DATE WRITTEN   Sept. 1998
        //       MODIFIED       Nov.-Dec. 2001, Jan. 2002, Richard Liesen
        //                      Feb. 2010, Chandan Sharma, FSEC. Added basin heater
        //                      Jun. 2016, Rongpeng Zhang, LBNL. Applied the chiller supply water temperature sensor fault model
        //                      Nov. 2016, Rongpeng Zhang, LBNL. Added Fouling Chiller fault

        static constexpr std::string_view RoutineName("CalcConstCOPChillerModel");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 EvapDeltaTemp;
        Real64 TempEvapOutSetPoint(0.0); // C - evaporator outlet temperature setpoint
        Real64 CurrentEndTime;           // end time of time step for current simulation time step
        Real64 COP;                      // coefficient of performance
        Real64 Cp;                       // local for fluid specif heat, for evaporator
        Real64 CpCond;                   // local for fluid specif heat, for condenser
        Real64 ChillerNomCap;            // chiller nominal capacity

        ChillerNomCap = this->NomCap;
        COP = this->COP;

        // If there is a fault of chiller fouling
        if (this->FaultyChillerFoulingFlag && (!state.dataGlobal->WarmupFlag) && (!state.dataGlobal->DoingSizing) &&
            (!state.dataGlobal->KickOffSimulation)) {
            int FaultIndex = this->FaultyChillerFoulingIndex;
            Real64 NomCap_ff = ChillerNomCap;
            Real64 COP_ff = COP;

            // calculate the Faulty Chiller Fouling Factor using fault information
            this->FaultyChillerFoulingFactor = state.dataFaultsMgr->FaultsChillerFouling(FaultIndex).CalFoulingFactor(state);

            // update the Chiller nominal capacity and COP at faulty cases
            ChillerNomCap = NomCap_ff * this->FaultyChillerFoulingFactor;
            COP = COP_ff * this->FaultyChillerFoulingFactor;
        }

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
                    TempEvapOutSetPoint = state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPointHi;
                }
            }
        }

        // If there is a fault of Chiller SWT Sensor
        if (this->FaultyChillerSWTFlag && (!state.dataGlobal->WarmupFlag) && (!state.dataGlobal->DoingSizing) &&
            (!state.dataGlobal->KickOffSimulation)) {
            int FaultIndex = this->FaultyChillerSWTIndex;
            Real64 EvapOutletTemp_ff = TempEvapOutSetPoint;

            // calculate the sensor offset using fault information
            this->FaultyChillerSWTOffset = state.dataFaultsMgr->FaultsChillerSWTSensor(FaultIndex).CalFaultOffsetAct(state);
            // update the TempEvapOutSetPoint
            TempEvapOutSetPoint = min(state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp, EvapOutletTemp_ff - this->FaultyChillerSWTOffset);
            this->FaultyChillerSWTOffset = EvapOutletTemp_ff - TempEvapOutSetPoint;
        }

        EvapDeltaTemp = std::abs(state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - TempEvapOutSetPoint);

        // If no component demand, or chiller OFF, or Chiller type set to 'Passive' by free
        // cooling heat exchanger, then set condenser side flow and heat transfer rates set to zero
        if (MyLoad >= 0.0 || !RunFlag) {

            // If Chiller load is 0 or greater or chiller is not running then leave the subroutine.Before leaving
            // if the component control is SERIESACTIVE we set the component flow to inlet flow so that
            // flow resolver will not shut down the branch
            if (EquipFlowCtrl == DataBranchAirLoopPlant::ControlTypeEnum::SeriesActive ||
                state.dataPlnt->PlantLoop(this->CWLoopNum).LoopSide(this->CWLoopSideNum).FlowLock == DataPlant::iFlowLock::Locked) {
                this->EvapMassFlowRate = state.dataLoopNodes->Node(this->EvapInletNodeNum).MassFlowRate;
            } else {
                this->EvapMassFlowRate = 0.0;
                PlantUtilities::SetComponentFlowRate(state,
                                                     this->EvapMassFlowRate,
                                                     this->EvapInletNodeNum,
                                                     this->EvapOutletNodeNum,
                                                     this->CWLoopNum,
                                                     this->CWLoopSideNum,
                                                     this->CWBranchNum,
                                                     this->CWCompNum);
            }
            if (this->CondenserType == DataPlant::CondenserType::WaterCooled) {
                if (state.dataPlnt->PlantLoop(this->CDLoopNum)
                        .LoopSide(this->CDLoopSideNum)
                        .Branch(this->CDBranchNum)
                        .Comp(this->CDCompNum)
                        .FlowCtrl == DataBranchAirLoopPlant::ControlTypeEnum::SeriesActive) {
                    this->CondMassFlowRate = state.dataLoopNodes->Node(this->CondInletNodeNum).MassFlowRate;
                } else {
                    this->CondMassFlowRate = 0.0;
                    PlantUtilities::SetComponentFlowRate(state,
                                                         this->CondMassFlowRate,
                                                         this->CondInletNodeNum,
                                                         this->CondOutletNodeNum,
                                                         this->CDLoopNum,
                                                         this->CDLoopSideNum,
                                                         this->CDBranchNum,
                                                         this->CDCompNum);
                }
            }

            this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp;
            this->CondOutletTemp = state.dataLoopNodes->Node(this->CondInletNodeNum).Temp;

            this->Power = 0.0;
            this->QEvaporator = 0.0;
            this->QCondenser = 0.0;
            this->Energy = 0.0;
            this->EvaporatorEnergy = 0.0;
            this->CondenserEnergy = 0.0;

            if (this->CondenserType == DataPlant::CondenserType::EvapCooled) {
                CalcBasinHeaterPower(
                    state, this->BasinHeaterPowerFTempDiff, this->BasinHeaterSchedulePtr, this->BasinHeaterSetPointTemp, this->BasinHeaterPower);
            }
            this->PrintMessage = false;
            return;
        }

        //   calculate end time of current time step
        CurrentEndTime = state.dataGlobal->CurrentTime + state.dataHVACGlobal->SysTimeElapsed;

        //   Print warning messages only when valid and only for the first ocurrance. Let summary provide statistics.
        //   Wait for next time step to print warnings. If simulation iterates, print out
        //   the warning for the last iteration only. Must wait for next time step to accomplish this.
        //   If a warning occurs and the simulation down shifts, the warning is not valid.
        if (CurrentEndTime > this->CurrentEndTimeLast && state.dataHVACGlobal->TimeStepSys >= this->TimeStepSysLast) {
            if (this->PrintMessage) {
                ++this->MsgErrorCount;
                //       Show single warning and pass additional info to ShowRecurringWarningErrorAtEnd
                if (this->MsgErrorCount < 2) {
                    ShowWarningError(state, this->MsgBuffer1 + '.');
                    ShowContinueError(state, this->MsgBuffer2);
                } else {
                    ShowRecurringWarningErrorAtEnd(
                        state, this->MsgBuffer1 + " error continues.", this->ErrCount1, this->MsgDataLast, this->MsgDataLast, _, "[C]", "[C]");
                }
            }
        }

        //   save last system time step and last end time of current time step (used to determine if warning is valid)
        this->TimeStepSysLast = state.dataHVACGlobal->TimeStepSys;
        this->CurrentEndTimeLast = CurrentEndTime;

        // otherwise the chiller is running...

        if (this->CondenserType == DataPlant::CondenserType::AirCooled) { // Condenser inlet temp = outdoor temp
            state.dataLoopNodes->Node(this->CondInletNodeNum).Temp = state.dataLoopNodes->Node(this->CondInletNodeNum).OutAirDryBulb;
            //  Warn user if entering condenser temperature falls below 0C
            if (state.dataLoopNodes->Node(this->CondInletNodeNum).Temp < 0.0 && !state.dataGlobal->WarmupFlag) {
                this->PrintMessage = true;
                this->MsgBuffer1 =
                    "CalcConstCOPChillerModel - Chiller:ConstantCOP \"" + this->Name + "\" - Air Cooled Condenser Inlet Temperature below 0C";
                this->MsgBuffer2 = format("... Outdoor Dry-bulb Condition = {:6.2F} C. Occurrence info = {}, {} {}",
                                          state.dataLoopNodes->Node(this->CondInletNodeNum).Temp,
                                          state.dataEnvrn->EnvironmentName,
                                          state.dataEnvrn->CurMnDy,
                                          General::CreateSysTimeIntervalString(state));
                this->MsgDataLast = state.dataLoopNodes->Node(this->CondInletNodeNum).Temp;
            } else {
                this->PrintMessage = false;
            }
        } else if (this->CondenserType == DataPlant::CondenserType::EvapCooled) { // Condenser inlet temp = (outdoor wet bulb)
            state.dataLoopNodes->Node(this->CondInletNodeNum).Temp = state.dataLoopNodes->Node(this->CondInletNodeNum).OutAirWetBulb;
            //  Warn user if evap condenser wet bulb temperature falls below 10C
            if (state.dataLoopNodes->Node(this->CondInletNodeNum).Temp < 10.0 && !state.dataGlobal->WarmupFlag) {
                this->PrintMessage = true;
                this->MsgBuffer1 =
                    "CalcConstCOPChillerModel - Chiller:ConstantCOP \"" + this->Name + "\" - Evap Cooled Condenser Inlet Temperature below 10C";
                this->MsgBuffer2 = format("... Outdoor Wet-bulb Condition = {:6.2F} C. Occurrence info = {}, {} {}",
                                          state.dataLoopNodes->Node(this->CondInletNodeNum).Temp,
                                          state.dataEnvrn->EnvironmentName,
                                          state.dataEnvrn->CurMnDy,
                                          General::CreateSysTimeIntervalString(state));
                this->MsgDataLast = state.dataLoopNodes->Node(this->CondInletNodeNum).Temp;
            } else {
                this->PrintMessage = false;
            }
        } // End of the Air Cooled/Evap Cooled Logic block

        // Set condenser flow rate
        if (this->CondenserType == DataPlant::CondenserType::WaterCooled) {
            this->CondMassFlowRate = this->CondMassFlowRateMax;
            PlantUtilities::SetComponentFlowRate(state,
                                                 this->CondMassFlowRate,
                                                 this->CondInletNodeNum,
                                                 this->CondOutletNodeNum,
                                                 this->CDLoopNum,
                                                 this->CDLoopSideNum,
                                                 this->CDBranchNum,
                                                 this->CDCompNum);
            PlantUtilities::PullCompInterconnectTrigger(state,
                                                        this->CWLoopNum,
                                                        this->CWLoopSideNum,
                                                        this->CWBranchNum,
                                                        this->CWCompNum,
                                                        this->CondMassFlowIndex,
                                                        this->CDLoopNum,
                                                        this->CDLoopSideNum,
                                                        DataPlant::iCriteriaType::MassFlowRate,
                                                        this->CondMassFlowRate);

            if (this->CondMassFlowRate < DataBranchAirLoopPlant::MassFlowTolerance) return;
        }

        // If FlowLock is True, the new resolved mdot is used to update Power, QEvap, Qcond, and
        // condenser side outlet temperature.

        Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                    state.dataPlnt->PlantLoop(this->CWLoopNum).FluidName,
                                                    state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp,
                                                    state.dataPlnt->PlantLoop(this->CWLoopNum).FluidIndex,
                                                    RoutineName);

        if (state.dataPlnt->PlantLoop(this->CWLoopNum).LoopSide(this->CWLoopSideNum).FlowLock == DataPlant::iFlowLock::Unlocked) {
            this->PossibleSubcooling = false;
            this->QEvaporator = std::abs(MyLoad);
            this->Power = std::abs(MyLoad) / COP;

            // Either set the flow to the Constant value or caluclate the flow for the variable volume
            if ((this->FlowMode == DataPlant::FlowMode::Constant) || (this->FlowMode == DataPlant::FlowMode::NotModulated)) {

                // Start by assuming max (design) flow
                this->EvapMassFlowRate = this->EvapMassFlowRateMax;
                // Use SetComponentFlowRate to decide actual flow
                PlantUtilities::SetComponentFlowRate(state,
                                                     this->EvapMassFlowRate,
                                                     this->EvapInletNodeNum,
                                                     this->EvapOutletNodeNum,
                                                     this->CWLoopNum,
                                                     this->CWLoopSideNum,
                                                     this->CWBranchNum,
                                                     this->CWCompNum);
                // Evaluate delta temp based on actual flow rate
                if (this->EvapMassFlowRate != 0.0) {
                    EvapDeltaTemp = this->QEvaporator / this->EvapMassFlowRate / Cp;
                } else {
                    EvapDeltaTemp = 0.0;
                }
                // Evaluate outlet temp based on delta
                this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - EvapDeltaTemp;

            } else if (this->FlowMode == DataPlant::FlowMode::LeavingSetpointModulated) {

                // Calculate the Delta Temp from the inlet temp to the chiller outlet setpoint
                {
                    auto const SELECT_CASE_var(state.dataPlnt->PlantLoop(this->CWLoopNum).LoopDemandCalcScheme);
                    if (SELECT_CASE_var == DataPlant::iLoopDemandCalcScheme::SingleSetPoint) {
                        EvapDeltaTemp = std::abs(state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp -
                                                 state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempSetPoint);
                    } else if (SELECT_CASE_var == DataPlant::iLoopDemandCalcScheme::DualSetPointDeadBand) {
                        EvapDeltaTemp = std::abs(state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp -
                                                 state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempSetPointHi);
                    }
                }

                if (EvapDeltaTemp > DataPlant::DeltaTempTol) {
                    this->EvapMassFlowRate = std::abs(this->QEvaporator / Cp / EvapDeltaTemp);
                    if ((this->EvapMassFlowRate - this->EvapMassFlowRateMax) > DataBranchAirLoopPlant::MassFlowTolerance)
                        this->PossibleSubcooling = true;
                    // Check to see if the Maximum is exceeded, if so set to maximum
                    this->EvapMassFlowRate = min(this->EvapMassFlowRateMax, this->EvapMassFlowRate);
                    // Use SetComponentFlowRate to decide actual flow
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
                    // Try to request zero flow
                    this->EvapMassFlowRate = 0.0;
                    // Use SetComponentFlowRate to decide actual flow
                    PlantUtilities::SetComponentFlowRate(state,
                                                         this->EvapMassFlowRate,
                                                         this->EvapInletNodeNum,
                                                         this->EvapOutletNodeNum,
                                                         this->CWLoopNum,
                                                         this->CWLoopSideNum,
                                                         this->CWBranchNum,
                                                         this->CWCompNum);
                    // No deltaT since component is not running
                    this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp;
                }
            } // End of Constant or Variable Flow If Block for FlowLock = 0 (or making a flow request)

            // If there is a fault of Chiller SWT Sensor
            if (this->FaultyChillerSWTFlag && (!state.dataGlobal->WarmupFlag) && (!state.dataGlobal->DoingSizing) &&
                (!state.dataGlobal->KickOffSimulation) && (this->EvapMassFlowRate > 0)) {
                // calculate directly affected variables at faulty case: EvapOutletTemp, EvapMassFlowRate, QEvaporator
                int FaultIndex = this->FaultyChillerSWTIndex;
                bool VarFlowFlag = (this->FlowMode == DataPlant::FlowMode::LeavingSetpointModulated);
                state.dataFaultsMgr->FaultsChillerSWTSensor(FaultIndex)
                    .CalFaultChillerSWT(VarFlowFlag,
                                        this->FaultyChillerSWTOffset,
                                        Cp,
                                        state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp,
                                        this->EvapOutletTemp,
                                        this->EvapMassFlowRate,
                                        this->QEvaporator);
            }

        } else { // If FlowLock is True

            this->EvapMassFlowRate = state.dataLoopNodes->Node(this->EvapInletNodeNum).MassFlowRate;
            PlantUtilities::SetComponentFlowRate(state,
                                                 this->EvapMassFlowRate,
                                                 this->EvapInletNodeNum,
                                                 this->EvapOutletNodeNum,
                                                 this->CWLoopNum,
                                                 this->CWLoopSideNum,
                                                 this->CWBranchNum,
                                                 this->CWCompNum);
            //   Some other component set the flow to 0. No reason to continue with calculations.
            if (this->EvapMassFlowRate == 0.0) {
                MyLoad = 0.0;
                if (this->CondenserType == DataPlant::CondenserType::EvapCooled) {
                    CalcBasinHeaterPower(
                        state, this->BasinHeaterPowerFTempDiff, this->BasinHeaterSchedulePtr, this->BasinHeaterSetPointTemp, this->BasinHeaterPower);
                }
                this->PrintMessage = false;
                return;
            }

            // Recalculate the Delts Temp
            if (this->PossibleSubcooling) {
                this->QEvaporator = std::abs(MyLoad);
                EvapDeltaTemp = this->QEvaporator / this->EvapMassFlowRate / Cp;
                this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - EvapDeltaTemp;
                if (this->EvapOutletTemp < state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempMin) {
                    this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempMin;
                    EvapDeltaTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - this->EvapOutletTemp;
                    this->QEvaporator = this->EvapMassFlowRate * Cp * EvapDeltaTemp;
                }
            } else {
                EvapDeltaTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - TempEvapOutSetPoint;
                // Calculate the evaporator heat transfer at the specified flow which could have changed
                //  in the Flow Resolution step.
                this->QEvaporator = std::abs(this->EvapMassFlowRate * Cp * EvapDeltaTemp);
                this->EvapOutletTemp = TempEvapOutSetPoint;
            }
            // Check that the Evap outlet temp honors both plant loop temp low limit and also the chiller low limit
            if (this->EvapOutletTemp < state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempMin) {
                if ((state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempMin) >
                    DataPlant::DeltaTempTol) {
                    this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempMin;
                    EvapDeltaTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - this->EvapOutletTemp;
                    this->QEvaporator = this->EvapMassFlowRate * Cp * EvapDeltaTemp;
                } else {
                    this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp;
                    EvapDeltaTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - this->EvapOutletTemp;
                    this->QEvaporator = this->EvapMassFlowRate * Cp * EvapDeltaTemp;
                }
            }
            // If load exceeds the distributed load set to the distributed load
            if (this->QEvaporator > std::abs(MyLoad)) {
                if (this->EvapMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance) {
                    this->QEvaporator = std::abs(MyLoad);
                    EvapDeltaTemp = this->QEvaporator / this->EvapMassFlowRate / Cp;
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
                                        Cp,
                                        state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp,
                                        this->EvapOutletTemp,
                                        this->EvapMassFlowRate,
                                        this->QEvaporator);
            }

            // Checks QEvaporator on the basis of the machine limits.
            if (this->QEvaporator > ChillerNomCap) {
                if (this->EvapMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance) {
                    this->QEvaporator = ChillerNomCap;
                    EvapDeltaTemp = this->QEvaporator / this->EvapMassFlowRate / Cp;
                    this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp - EvapDeltaTemp;
                } else {
                    this->QEvaporator = 0.0;
                    this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp;
                }
            }
            // Calculate the Power consumption of the Const COP chiller which is a simplified calculation
            this->Power = this->QEvaporator / COP;
            if (this->EvapMassFlowRate == 0.0) {
                this->QEvaporator = 0.0;
                this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp;
                this->Power = 0.0;
                this->PrintMessage = false;
            }
            if (this->QEvaporator == 0.0 && this->CondenserType == DataPlant::CondenserType::EvapCooled) {
                CalcBasinHeaterPower(
                    state, this->BasinHeaterPowerFTempDiff, this->BasinHeaterSchedulePtr, this->BasinHeaterSetPointTemp, this->BasinHeaterPower);
            }

        } // This is the end of the FlowLock Block

        // QCondenser is calculated the same for each type, but the power consumption should be different
        //  depending on the performance coefficients used for the chiller model.
        this->QCondenser = this->Power + this->QEvaporator;

        // If not air or evap cooled then set to the condenser node that is attached to a cooling tower
        Real64 const CondInletTemp = state.dataLoopNodes->Node(this->CondInletNodeNum).Temp;

        if (this->CondenserType == DataPlant::CondenserType::WaterCooled) {
            CpCond = FluidProperties::GetSpecificHeatGlycol(state,
                                                            state.dataPlnt->PlantLoop(this->CDLoopNum).FluidName,
                                                            CondInletTemp,
                                                            state.dataPlnt->PlantLoop(this->CDLoopNum).FluidIndex,
                                                            RoutineName);
            if (this->CondMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance) {
                this->CondOutletTemp = this->QCondenser / this->CondMassFlowRate / CpCond + CondInletTemp;
            } else {
                ShowSevereError(state, "CalcConstCOPChillerModel: Condenser flow = 0, for CONST COP Chiller=" + this->Name);
                ShowContinueErrorTimeStamp(state, "");
            }
        } else { // Air Cooled or Evap Cooled
            //  Set condenser outlet temp to condenser inlet temp for Air Cooled or Evap Cooled
            //  since there is no CondMassFlowRate and would divide by zero
            this->CondOutletTemp = CondInletTemp;
        }

        // Calculate Energy
        this->CondenserEnergy = this->QCondenser * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
        this->Energy = this->Power * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
        this->EvaporatorEnergy = this->QEvaporator * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;

        // check for problems BG 9/12/06 (deal with observed negative energy results)
        if (this->Energy < 0.0) { // there is a serious problem

            if (this->CondenserType == DataPlant::CondenserType::WaterCooled) {
                // first check for run away condenser loop temps (only reason yet to be observed for this?)
                if (CondInletTemp > 70.0) {
                    ShowSevereError(state,
                                    "CalcConstCOPChillerModel: Condenser loop inlet temperatures over 70.0 C for ConstCOPChiller=" + this->Name);
                    ShowContinueErrorTimeStamp(state, "");
                    ShowContinueError(state, format("Condenser loop water temperatures are too high at{:.2R}", CondInletTemp));
                    ShowContinueError(state, "Check input for condenser plant loop, especially cooling tower");
                    ShowContinueError(state, format("Evaporator inlet temperature: {:.2R}", state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp));

                    ShowFatalError(state, "Program Terminates due to previous error condition");
                }
            }
            // If makes it here, set limits, chiller can't have negative energy/power
            // proceeding silently for now but may want to throw error here
            this->Power = 0.0;
            this->Energy = 0.0;
        }
    }

    void ConstCOPChillerSpecs::update(EnergyPlusData &state, Real64 const MyLoad, bool const RunFlag)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Dan Fisher
        //       DATE WRITTEN:    October 1998

        Real64 ReportingConstant = state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;

        if (MyLoad >= 0.0 || !RunFlag) { // Chiller not running so pass inlet states to outlet states
            this->Power = 0.0;
            this->QEvaporator = 0.0;
            this->QCondenser = 0.0;
            this->Energy = 0.0;
            this->EvaporatorEnergy = 0.0;
            this->CondenserEnergy = 0.0;
            this->CondInletTemp = state.dataLoopNodes->Node(this->CondInletNodeNum).Temp;
            this->EvapInletTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp;
            this->CondOutletTemp = state.dataLoopNodes->Node(this->CondInletNodeNum).Temp;
            this->EvapOutletTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp;
            this->ActualCOP = 0.0;
            if (this->CondenserType == DataPlant::CondenserType::EvapCooled) {
                this->BasinHeaterPower = this->BasinHeaterPower;
                this->BasinHeaterConsumption = this->BasinHeaterPower * ReportingConstant;
            }

            // set outlet node temperatures
            state.dataLoopNodes->Node(this->EvapOutletNodeNum).Temp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp;
            state.dataLoopNodes->Node(this->CondOutletNodeNum).Temp = state.dataLoopNodes->Node(this->CondInletNodeNum).Temp;

        } else {
            this->CondInletTemp = state.dataLoopNodes->Node(this->CondInletNodeNum).Temp;
            this->EvapInletTemp = state.dataLoopNodes->Node(this->EvapInletNodeNum).Temp;
            if (this->Power != 0.0) {
                this->ActualCOP = this->QEvaporator / this->Power;
            } else {
                this->ActualCOP = 0.0;
            }
            if (this->CondenserType == DataPlant::CondenserType::EvapCooled) {
                this->BasinHeaterPower = this->BasinHeaterPower;
                this->BasinHeaterConsumption = this->BasinHeaterPower * ReportingConstant;
            }

            // set outlet node temperatures
            state.dataLoopNodes->Node(this->EvapOutletNodeNum).Temp = this->EvapOutletTemp;
            state.dataLoopNodes->Node(this->CondOutletNodeNum).Temp = this->CondOutletTemp;
        }
    }
    void ConstCOPChillerSpecs::oneTimeInit(EnergyPlusData &state)
    {
        // Init more variables
        if (this->MyFlag) {
            // Locate the chillers on the plant loops for later usage
            bool errFlag = false;
            this->setupOutputVariables(state);
            PlantUtilities::ScanPlantLoopsForObject(state,
                                                    this->Name,
                                                    this->plantTypeOfNum,
                                                    this->CWLoopNum,
                                                    this->CWLoopSideNum,
                                                    this->CWBranchNum,
                                                    this->CWCompNum,
                                                    errFlag,
                                                    _,
                                                    _,
                                                    _,
                                                    this->EvapInletNodeNum,
                                                    _);
            if (this->CondenserType != DataPlant::CondenserType::AirCooled && this->CondenserType != DataPlant::CondenserType::EvapCooled) {
                PlantUtilities::ScanPlantLoopsForObject(state,
                                                        this->Name,
                                                        this->plantTypeOfNum,
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
                    state, this->CWLoopNum, this->CWLoopSideNum, this->CDLoopNum, this->CDLoopSideNum, this->plantTypeOfNum, true);
            }

            if (errFlag) {
                ShowFatalError(state, "CalcConstCOPChillerModel: Program terminated due to previous condition(s).");
            }
            if (this->FlowMode == DataPlant::FlowMode::Constant) {
                // reset flow priority
                state.dataPlnt->PlantLoop(this->CWLoopNum)
                    .LoopSide(this->CWLoopSideNum)
                    .Branch(this->CWBranchNum)
                    .Comp(this->CWCompNum)
                    .FlowPriority = DataPlant::LoopFlowStatus_NeedyIfLoopOn;
            }

            if (this->FlowMode == DataPlant::FlowMode::LeavingSetpointModulated) {
                // reset flow priority
                state.dataPlnt->PlantLoop(this->CWLoopNum)
                    .LoopSide(this->CWLoopSideNum)
                    .Branch(this->CWBranchNum)
                    .Comp(this->CWCompNum)
                    .FlowPriority = DataPlant::LoopFlowStatus_NeedyIfLoopOn;

                // check if setpoint on outlet node
                if ((state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempSetPoint == DataLoopNode::SensedNodeFlagValue) &&
                    (state.dataLoopNodes->Node(this->EvapOutletNodeNum).TempSetPointHi == DataLoopNode::SensedNodeFlagValue)) {
                    if (!state.dataGlobal->AnyEnergyManagementSystemInModel) {
                        if (!this->ModulatedFlowErrDone) {
                            ShowWarningError(state, "Missing temperature setpoint for LeavingSetpointModulated mode chiller named " + this->Name);
                            ShowContinueError(
                                state,
                                "  A temperature setpoint is needed at the outlet node of a chiller in variable flow mode, use a SetpointManager");
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
                                ShowContinueError(
                                    state, "  A temperature setpoint is needed at the outlet node of a chiller evaporator in variable flow mode");
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
            this->MyFlag = false;
        }
    }

} // namespace PlantChillers

} // namespace EnergyPlus
