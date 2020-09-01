// EnergyPlus, Copyright (c) 1996-2020, The Board of Trustees of the University of Illinois,
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
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/DataBranchAirLoopPlant.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/EMSManager.hh>
#include <EnergyPlus/FaultsManager.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutAirNodeManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/Plant/PlantLocation.hh>
#include <EnergyPlus/PlantChillers.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ReportSizingManager.hh>
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

    void BaseChillerSpecs::getDesignCapacities(const PlantLocation &calledFromLocation, Real64 &MaxLoad, Real64 &MinLoad, Real64 &OptLoad)
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
        this->initialize(state.dataBranchInputManager, false, 0.0);
        if (calledFromLocation.loopNum == this->CWLoopNum) {
            this->size();
        }
    }

    void BaseChillerSpecs::getDesignTemperatures(Real64 &tempDesCondIn, Real64 &tempDesEvapOut)
    {
        tempDesEvapOut = this->TempDesEvapOut;
        tempDesCondIn = this->TempDesCondIn;
    }

    ElectricChillerSpecs *ElectricChillerSpecs::factory(PlantChillersData &chillers, std::string const &chillerName)
    {
        if (chillers.GetElectricInput) {
            ElectricChillerSpecs::getInput(chillers);
            chillers.GetElectricInput = false;
        }
        for (auto &thisChiller : chillers.ElectricChiller) {
            if (UtilityRoutines::MakeUPPERCase(thisChiller.Name) == chillerName) {
                return &thisChiller;
            }
        }
        ShowFatalError("Could not locate electric chiller with name: " + chillerName);
        return nullptr;
    }

    void ElectricChillerSpecs::getInput(PlantChillersData &chillers)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Dan Fisher / Brandon Anderson
        //       DATE WRITTEN:    September 2000

        // PURPOSE OF THIS SUBROUTINE:
        // This routine will get the input
        // required by the Electric Chiller model.

        static std::string const RoutineName("GetElectricChillerInput: "); // include trailing blank space

        int NumAlphas;  // Number of elements in the alpha array
        int NumNums;    // Number of elements in the numeric array
        int IOStat;     // IO Status when calling get input subroutine
        bool ErrorsFound(false);

        // FLOW
        DataIPShortCuts::cCurrentModuleObject = "Chiller:Electric";
        chillers.NumElectricChillers = inputProcessor->getNumObjectsFound(DataIPShortCuts::cCurrentModuleObject);

        if (chillers.NumElectricChillers <= 0) {
            ShowSevereError("No " + DataIPShortCuts::cCurrentModuleObject + " Equipment specified in input file");
            ErrorsFound = true;
        }

        // See if load distribution manager has already gotten the input
        if (allocated(chillers.ElectricChiller)) return;

        // ALLOCATE ARRAYS
        chillers.ElectricChiller.allocate(chillers.NumElectricChillers);

        // LOAD ARRAYS WITH Electric CURVE FIT CHILLER DATA
        for (int ChillerNum = 1; ChillerNum <= chillers.NumElectricChillers; ++ChillerNum) {
            inputProcessor->getObjectItem(DataIPShortCuts::cCurrentModuleObject,
                                          ChillerNum,
                                          DataIPShortCuts::cAlphaArgs,
                                          NumAlphas,
                                          DataIPShortCuts::rNumericArgs,
                                          NumNums,
                                          IOStat,
                                          DataIPShortCuts::lNumericFieldBlanks,
                                          DataIPShortCuts::lAlphaFieldBlanks,
                                          DataIPShortCuts::cAlphaFieldNames,
                                          DataIPShortCuts::cNumericFieldNames);
            UtilityRoutines::IsNameEmpty(DataIPShortCuts::cAlphaArgs(1), DataIPShortCuts::cCurrentModuleObject, ErrorsFound);

            // ErrorsFound will be set to True if problem was found, left untouched otherwise
            GlobalNames::VerifyUniqueChillerName(DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), ErrorsFound, DataIPShortCuts::cCurrentModuleObject + " Name");

            auto &thisChiller = chillers.ElectricChiller(ChillerNum);
            thisChiller.Name = DataIPShortCuts::cAlphaArgs(1);
            thisChiller.plantTypeOfNum = DataPlant::TypeOf_Chiller_Electric;

            if (DataIPShortCuts::cAlphaArgs(2) == "AIRCOOLED") {
                thisChiller.CondenserType = DataPlant::CondenserType::AIRCOOLED;
            } else if (DataIPShortCuts::cAlphaArgs(2) == "WATERCOOLED") {
                thisChiller.CondenserType = DataPlant::CondenserType::WATERCOOLED;
            } else if (DataIPShortCuts::cAlphaArgs(2) == "EVAPORATIVELYCOOLED") {
                thisChiller.CondenserType = DataPlant::CondenserType::EVAPCOOLED;
            } else {
                ShowSevereError("Invalid " + DataIPShortCuts::cAlphaFieldNames(2) + '=' + DataIPShortCuts::cAlphaArgs(2));
                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                ErrorsFound = true;
            }

            thisChiller.NomCap = DataIPShortCuts::rNumericArgs(1);
            if (thisChiller.NomCap == DataSizing::AutoSize) {
                thisChiller.NomCapWasAutoSized = true;
            }
            if (DataIPShortCuts::rNumericArgs(1) == 0.0) {
                ShowSevereError("Invalid " + DataIPShortCuts::cNumericFieldNames(1) + '=' + General::RoundSigDigits(DataIPShortCuts::rNumericArgs(1), 2));
                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                ErrorsFound = true;
            }
            thisChiller.COP = DataIPShortCuts::rNumericArgs(2);
            if (DataIPShortCuts::rNumericArgs(2) == 0.0) {
                ShowSevereError("Invalid " + DataIPShortCuts::cNumericFieldNames(2) + '=' + General::RoundSigDigits(DataIPShortCuts::rNumericArgs(2), 3));
                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                ErrorsFound = true;
            }
            thisChiller.EvapInletNodeNum = NodeInputManager::GetOnlySingleNode(
                DataIPShortCuts::cAlphaArgs(3), ErrorsFound, DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Inlet, 1, DataLoopNode::ObjectIsNotParent);
            thisChiller.EvapOutletNodeNum = NodeInputManager::GetOnlySingleNode(
                DataIPShortCuts::cAlphaArgs(4), ErrorsFound, DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Outlet, 1, DataLoopNode::ObjectIsNotParent);
            BranchNodeConnections::TestCompSet(DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataIPShortCuts::cAlphaArgs(3), DataIPShortCuts::cAlphaArgs(4), "Chilled Water Nodes");

            if (thisChiller.CondenserType == DataPlant::CondenserType::AIRCOOLED || thisChiller.CondenserType == DataPlant::CondenserType::EVAPCOOLED) {
                // Connection not required for air or evap cooled condenser
                // If the condenser inlet is blank for air cooled and evap cooled condensers then supply a generic name
                //  since it is not used elsewhere for connection
                // for transition purposes, add this node if not there.
                if (DataIPShortCuts::lAlphaFieldBlanks(5)) {
                    if (len(DataIPShortCuts::cAlphaArgs(1)) < DataGlobals::MaxNameLength - 21) { // protect against long name leading to > 100 chars
                        DataIPShortCuts::cAlphaArgs(5) = DataIPShortCuts::cAlphaArgs(1) + " CONDENSER INLET NODE";
                    } else {
                        DataIPShortCuts::cAlphaArgs(5) = DataIPShortCuts::cAlphaArgs(1).substr(0, 79) + " CONDENSER INLET NODE";
                    }
                }
                if (DataIPShortCuts::lAlphaFieldBlanks(6)) {
                    if (len(DataIPShortCuts::cAlphaArgs(1)) < DataGlobals::MaxNameLength - 22) { // protect against long name leading to > 100 chars
                        DataIPShortCuts::cAlphaArgs(6) = DataIPShortCuts::cAlphaArgs(1) + " CONDENSER OUTLET NODE";
                    } else {
                        DataIPShortCuts::cAlphaArgs(6) = DataIPShortCuts::cAlphaArgs(1).substr(0, 78) + " CONDENSER OUTLET NODE";
                    }
                }

                thisChiller.CondInletNodeNum = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(5),
                                                                                 ErrorsFound,
                                                                                 DataIPShortCuts::cCurrentModuleObject,
                                                                                 DataIPShortCuts::cAlphaArgs(1),
                                                                                 DataLoopNode::NodeType_Air,
                                                                                 DataLoopNode::NodeConnectionType_OutsideAirReference,
                                                                                 2,
                                                                                 DataLoopNode::ObjectIsNotParent);
                bool Okay;
                OutAirNodeManager::CheckAndAddAirNodeNumber(thisChiller.CondInletNodeNum, Okay);
                if (!Okay) {
                    ShowWarningError(DataIPShortCuts::cCurrentModuleObject + ", Adding OutdoorAir:Node=" + DataIPShortCuts::cAlphaArgs(5));
                }

                thisChiller.CondOutletNodeNum = NodeInputManager::GetOnlySingleNode(
                    DataIPShortCuts::cAlphaArgs(6), ErrorsFound, DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataLoopNode::NodeType_Air, DataLoopNode::NodeConnectionType_Outlet, 2, DataLoopNode::ObjectIsNotParent);
            } else if (thisChiller.CondenserType == DataPlant::CondenserType::WATERCOOLED) {
                thisChiller.CondInletNodeNum = NodeInputManager::GetOnlySingleNode(
                    DataIPShortCuts::cAlphaArgs(5), ErrorsFound, DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Inlet, 2, DataLoopNode::ObjectIsNotParent);
                thisChiller.CondOutletNodeNum = NodeInputManager::GetOnlySingleNode(
                    DataIPShortCuts::cAlphaArgs(6), ErrorsFound, DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Outlet, 2, DataLoopNode::ObjectIsNotParent);
                BranchNodeConnections::TestCompSet(DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataIPShortCuts::cAlphaArgs(5), DataIPShortCuts::cAlphaArgs(6), "Condenser Water Nodes");
                // Condenser Inlet node name is necessary for Water Cooled
                if (DataIPShortCuts::lAlphaFieldBlanks(5)) {
                    ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(5) + "is blank ");
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                    ErrorsFound = true;
                } else if (DataIPShortCuts::lAlphaFieldBlanks(6)) {
                    ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(6) + "is blank ");
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                    ErrorsFound = true;
                }
            } else {
                thisChiller.CondInletNodeNum = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(5),
                                                                                 ErrorsFound,
                                                                                 DataIPShortCuts::cCurrentModuleObject,
                                                                                 DataIPShortCuts::cAlphaArgs(1),
                                                                                 DataLoopNode::NodeType_Unknown,
                                                                                 DataLoopNode::NodeConnectionType_Inlet,
                                                                                 2,
                                                                                 DataLoopNode::ObjectIsNotParent);
                thisChiller.CondOutletNodeNum = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(6),
                                                                                  ErrorsFound,
                                                                                  DataIPShortCuts::cCurrentModuleObject,
                                                                                  DataIPShortCuts::cAlphaArgs(1),
                                                                                  DataLoopNode::NodeType_Unknown,
                                                                                  DataLoopNode::NodeConnectionType_Outlet,
                                                                                  2,
                                                                                  DataLoopNode::ObjectIsNotParent);
                BranchNodeConnections::TestCompSet(DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataIPShortCuts::cAlphaArgs(5), DataIPShortCuts::cAlphaArgs(6), "Condenser (unknown?) Nodes");
                // Condenser Inlet node name is necessary
                if (DataIPShortCuts::lAlphaFieldBlanks(5)) {
                    ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(5) + "is blank ");
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                    ErrorsFound = true;
                } else if (DataIPShortCuts::lAlphaFieldBlanks(6)) {
                    ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(6) + "is blank ");
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                    ErrorsFound = true;
                }
            }

            thisChiller.MinPartLoadRat = DataIPShortCuts::rNumericArgs(3);
            thisChiller.MaxPartLoadRat = DataIPShortCuts::rNumericArgs(4);
            thisChiller.OptPartLoadRat = DataIPShortCuts::rNumericArgs(5);
            thisChiller.TempDesCondIn = DataIPShortCuts::rNumericArgs(6);
            thisChiller.TempRiseCoef = DataIPShortCuts::rNumericArgs(7);
            thisChiller.TempDesEvapOut = DataIPShortCuts::rNumericArgs(8);
            thisChiller.EvapVolFlowRate = DataIPShortCuts::rNumericArgs(9);
            if (thisChiller.EvapVolFlowRate == DataSizing::AutoSize) {
                thisChiller.EvapVolFlowRateWasAutoSized = true;
            }
            thisChiller.CondVolFlowRate = DataIPShortCuts::rNumericArgs(10);
            if (thisChiller.CondVolFlowRate == DataSizing::AutoSize) {
                if (thisChiller.CondenserType == DataPlant::CondenserType::WATERCOOLED) {
                    thisChiller.CondVolFlowRateWasAutoSized = true;
                }
            }
            thisChiller.CapRatCoef(1) = DataIPShortCuts::rNumericArgs(11);
            thisChiller.CapRatCoef(2) = DataIPShortCuts::rNumericArgs(12);
            thisChiller.CapRatCoef(3) = DataIPShortCuts::rNumericArgs(13);
            if ((DataIPShortCuts::rNumericArgs(11) + DataIPShortCuts::rNumericArgs(12) + DataIPShortCuts::rNumericArgs(13)) == 0.0) {
                ShowSevereError(DataIPShortCuts::cCurrentModuleObject + ": Sum of Capacity Ratio Coef = 0.0, chiller=" + DataIPShortCuts::cAlphaArgs(1));
                ErrorsFound = true;
            }
            thisChiller.PowerRatCoef(1) = DataIPShortCuts::rNumericArgs(14);
            thisChiller.PowerRatCoef(2) = DataIPShortCuts::rNumericArgs(15);
            thisChiller.PowerRatCoef(3) = DataIPShortCuts::rNumericArgs(16);
            thisChiller.FullLoadCoef(1) = DataIPShortCuts::rNumericArgs(17);
            thisChiller.FullLoadCoef(2) = DataIPShortCuts::rNumericArgs(18);
            thisChiller.FullLoadCoef(3) = DataIPShortCuts::rNumericArgs(19);
            thisChiller.TempLowLimitEvapOut = DataIPShortCuts::rNumericArgs(20);
            thisChiller.SizFac = DataIPShortCuts::rNumericArgs(22);
            if (thisChiller.SizFac <= 0.0) thisChiller.SizFac = 1.0;

            {
                auto const SELECT_CASE_var(DataIPShortCuts::cAlphaArgs(7));
                if (SELECT_CASE_var == "CONSTANTFLOW") {
                    thisChiller.FlowMode = DataPlant::FlowMode::CONSTANT;
                } else if (SELECT_CASE_var == "LEAVINGSETPOINTMODULATED") {
                    thisChiller.FlowMode = DataPlant::FlowMode::LEAVINGSETPOINTMODULATED;
                } else if (SELECT_CASE_var == "NOTMODULATED") {
                    thisChiller.FlowMode = DataPlant::FlowMode::NOTMODULATED;
                } else {
                    ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\",");
                    ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(7) + '=' + DataIPShortCuts::cAlphaArgs(7));
                    ShowContinueError("Available choices are ConstantFlow, NotModulated, or LeavingSetpointModulated");
                    ShowContinueError("Flow mode NotModulated is assumed and the simulation continues.");
                    thisChiller.FlowMode = DataPlant::FlowMode::NOTMODULATED;
                }
            }

            // These are the Heat Recovery Inputs
            thisChiller.DesignHeatRecVolFlowRate = DataIPShortCuts::rNumericArgs(21);
            if (thisChiller.DesignHeatRecVolFlowRate == DataSizing::AutoSize) {
                thisChiller.DesignHeatRecVolFlowRateWasAutoSized = true;
            }

            if ((thisChiller.DesignHeatRecVolFlowRate > 0.0) || (thisChiller.DesignHeatRecVolFlowRate == DataSizing::AutoSize)) {
                thisChiller.HeatRecActive = true;
                thisChiller.HeatRecInletNodeNum = NodeInputManager::GetOnlySingleNode(
                    DataIPShortCuts::cAlphaArgs(8), ErrorsFound, DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Inlet, 3, DataLoopNode::ObjectIsNotParent);
                if (thisChiller.HeatRecInletNodeNum == 0) {
                    ShowSevereError("Invalid " + DataIPShortCuts::cAlphaFieldNames(8) + '=' + DataIPShortCuts::cAlphaArgs(8));
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                    ErrorsFound = true;
                }
                thisChiller.HeatRecOutletNodeNum = NodeInputManager::GetOnlySingleNode(
                    DataIPShortCuts::cAlphaArgs(9), ErrorsFound, DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Outlet, 3, DataLoopNode::ObjectIsNotParent);
                if (thisChiller.HeatRecOutletNodeNum == 0) {
                    ShowSevereError("Invalid " + DataIPShortCuts::cAlphaFieldNames(9) + '=' + DataIPShortCuts::cAlphaArgs(9));
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                    ErrorsFound = true;
                }

                BranchNodeConnections::TestCompSet(DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataIPShortCuts::cAlphaArgs(8), DataIPShortCuts::cAlphaArgs(9), "Heat Recovery Nodes");
                if (thisChiller.DesignHeatRecVolFlowRate > 0.0) {
                    PlantUtilities::RegisterPlantCompDesignFlow(thisChiller.HeatRecInletNodeNum,
                                                thisChiller.DesignHeatRecVolFlowRate);
                }
                // Condenser flow rate must be specified for heat reclaim
                if (thisChiller.CondenserType == DataPlant::CondenserType::AIRCOOLED || thisChiller.CondenserType == DataPlant::CondenserType::EVAPCOOLED) {
                    if (thisChiller.CondVolFlowRate <= 0.0) {
                        ShowSevereError("Invalid " + DataIPShortCuts::cNumericFieldNames(10) + '=' + General::RoundSigDigits(DataIPShortCuts::rNumericArgs(10), 6));
                        ShowSevereError("Condenser fluid flow rate must be specified for Heat Reclaim applications.");
                        ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                        ErrorsFound = true;
                    }
                }

                if (NumNums > 24) {
                    if (!DataIPShortCuts::lNumericFieldBlanks(25)) {
                        thisChiller.HeatRecCapacityFraction = DataIPShortCuts::rNumericArgs(25);
                    } else {
                        thisChiller.HeatRecCapacityFraction = 1.0;
                    }
                } else {
                    thisChiller.HeatRecCapacityFraction = 1.0;
                }

                if (NumAlphas > 10) {
                    if (!DataIPShortCuts::lAlphaFieldBlanks(11)) {
                        thisChiller.HeatRecInletLimitSchedNum = ScheduleManager::GetScheduleIndex(DataIPShortCuts::cAlphaArgs(11));
                        if (thisChiller.HeatRecInletLimitSchedNum == 0) {
                            ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\"");
                            ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(11) + '=' + DataIPShortCuts::cAlphaArgs(11));
                            ErrorsFound = true;
                        }
                    } else {
                        thisChiller.HeatRecInletLimitSchedNum = 0;
                    }
                } else {
                    thisChiller.HeatRecInletLimitSchedNum = 0;
                }

                if (NumAlphas > 11) {
                    if (!DataIPShortCuts::lAlphaFieldBlanks(12)) {
                        thisChiller.HeatRecSetPointNodeNum = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(12),
                                                                                               ErrorsFound,
                                                                                               DataIPShortCuts::cCurrentModuleObject,
                                                                                               DataIPShortCuts::cAlphaArgs(1),
                                                                                               DataLoopNode::NodeType_Water,
                                                                                               DataLoopNode::NodeConnectionType_Sensor,
                                                                                               1,
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
                if (thisChiller.CondenserType == DataPlant::CondenserType::AIRCOOLED || thisChiller.CondenserType == DataPlant::CondenserType::EVAPCOOLED) {
                    thisChiller.CondVolFlowRate = 0.0011; // set to avoid errors in calc routine
                }
                if ((!DataIPShortCuts::lAlphaFieldBlanks(8)) || (!DataIPShortCuts::lAlphaFieldBlanks(9))) {
                    ShowWarningError("Since Design Heat Flow Rate = 0.0, Heat Recovery inactive for " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                    ShowContinueError("However, Node names were specified for Heat Recovery inlet or outlet nodes");
                }
            }
            //   Basin heater power as a function of temperature must be greater than or equal to 0
            thisChiller.BasinHeaterPowerFTempDiff = DataIPShortCuts::rNumericArgs(23);
            if (DataIPShortCuts::rNumericArgs(23) < 0.0) {
                ShowSevereError(DataIPShortCuts::cCurrentModuleObject + ", \"" + thisChiller.Name + "\" TRIM(DataIPShortCuts::cNumericFieldNames(23)) must be >= 0");
                ErrorsFound = true;
            }

            thisChiller.BasinHeaterSetPointTemp = DataIPShortCuts::rNumericArgs(24);

            if (thisChiller.BasinHeaterPowerFTempDiff > 0.0) {
                if (NumNums < 24) {
                    thisChiller.BasinHeaterSetPointTemp = 2.0;
                }
                if (thisChiller.BasinHeaterSetPointTemp < 2.0) {
                    ShowWarningError(DataIPShortCuts::cCurrentModuleObject + ":\"" + thisChiller.Name + "\", " + DataIPShortCuts::cNumericFieldNames(24) +
                                     " is less than 2 deg C. Freezing could occur.");
                }
            }

            if (!DataIPShortCuts::lAlphaFieldBlanks(10)) {
                thisChiller.BasinHeaterSchedulePtr = ScheduleManager::GetScheduleIndex(DataIPShortCuts::cAlphaArgs(10));
                if (thisChiller.BasinHeaterSchedulePtr == 0) {
                    ShowWarningError(DataIPShortCuts::cCurrentModuleObject + ", \"" + thisChiller.Name + "\" TRIM(DataIPShortCuts::cAlphaFieldNames(10)) \"" +
                                     DataIPShortCuts::cAlphaArgs(10) + "\" was not found. Basin heater operation will not be modeled and the simulation continues");
                }
            }
            if (NumAlphas > 12) {
                thisChiller.EndUseSubcategory = DataIPShortCuts::cAlphaArgs(13);
            } else {
                thisChiller.EndUseSubcategory = "General";
            }
        }

        if (ErrorsFound) {
            ShowFatalError("Errors found in processing input for " + DataIPShortCuts::cCurrentModuleObject);
        }
    }

    void ElectricChillerSpecs::setupOutputVariables()
    {
        SetupOutputVariable("Chiller Electricity Rate", OutputProcessor::Unit::W, this->Power, "System", "Average", this->Name);
        SetupOutputVariable("Chiller Electricity Energy",
                            OutputProcessor::Unit::J,
                            this->Energy,
                            "System",
                            "Sum",
                            this->Name,
                            _,
                            "ELECTRICITY",
                            "Cooling",
                            this->EndUseSubcategory,
                            "Plant");

        SetupOutputVariable("Chiller Evaporator Cooling Rate", OutputProcessor::Unit::W, this->QEvaporator, "System", "Average", this->Name);
        SetupOutputVariable("Chiller Evaporator Cooling Energy",
                            OutputProcessor::Unit::J,
                            this->EvaporatorEnergy,
                            "System",
                            "Sum",
                            this->Name,
                            _,
                            "ENERGYTRANSFER",
                            "CHILLERS",
                            _,
                            "Plant");
        SetupOutputVariable("Chiller Evaporator Inlet Temperature", OutputProcessor::Unit::C, this->EvapInletTemp, "System", "Average", this->Name);
        SetupOutputVariable("Chiller Evaporator Outlet Temperature", OutputProcessor::Unit::C, this->EvapOutletTemp, "System", "Average", this->Name);
        SetupOutputVariable("Chiller Evaporator Mass Flow Rate", OutputProcessor::Unit::kg_s, this->EvapMassFlowRate, "System", "Average", this->Name);
        SetupOutputVariable("Chiller Condenser Heat Transfer Rate", OutputProcessor::Unit::W, this->QCondenser, "System", "Average", this->Name);
        SetupOutputVariable("Chiller Condenser Heat Transfer Energy",
                            OutputProcessor::Unit::J,
                            this->CondenserEnergy,
                            "System",
                            "Sum",
                            this->Name,
                            _,
                            "ENERGYTRANSFER",
                            "HEATREJECTION",
                            _,
                            "Plant");
        SetupOutputVariable("Chiller COP", OutputProcessor::Unit::W_W, this->ActualCOP, "System", "Average", this->Name);
        SetupOutputVariable("Chiller Condenser Inlet Temperature", OutputProcessor::Unit::C, this->CondInletTemp, "System", "Average", this->Name);

        // Condenser mass flow and outlet temp are valid for water cooled
        if (this->CondenserType == DataPlant::CondenserType::WATERCOOLED) {
            SetupOutputVariable("Chiller Condenser Outlet Temperature", OutputProcessor::Unit::C, this->CondOutletTemp, "System", "Average", this->Name);
            SetupOutputVariable("Chiller Condenser Mass Flow Rate", OutputProcessor::Unit::kg_s, this->CondMassFlowRate, "System", "Average", this->Name);
        } else if (this->CondenserType == DataPlant::CondenserType::AIRCOOLED) {
        } else if (this->CondenserType == DataPlant::CondenserType::EVAPCOOLED) {
            if (this->BasinHeaterPowerFTempDiff > 0.0) {
                SetupOutputVariable("Chiller Basin Heater Electricity Rate", OutputProcessor::Unit::W, this->BasinHeaterPower, "System", "Average", this->Name);
                SetupOutputVariable("Chiller Basin Heater Electricity Energy",
                                    OutputProcessor::Unit::J,
                                    this->BasinHeaterConsumption,
                                    "System",
                                    "Sum",
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
            SetupOutputVariable("Chiller Total Recovered Heat Rate", OutputProcessor::Unit::W, this->QHeatRecovery, "System", "Average", this->Name);
            SetupOutputVariable("Chiller Total Recovered Heat Energy",
                                OutputProcessor::Unit::J,
                                this->EnergyHeatRecovery,
                                "System",
                                "Sum",
                                this->Name,
                                _,
                                "ENERGYTRANSFER",
                                "HEATRECOVERY",
                                _,
                                "Plant");
            SetupOutputVariable("Chiller Heat Recovery Inlet Temperature", OutputProcessor::Unit::C, this->HeatRecInletTemp, "System", "Average", this->Name);
            SetupOutputVariable("Chiller Heat Recovery Outlet Temperature", OutputProcessor::Unit::C, this->HeatRecOutletTemp, "System", "Average", this->Name);
            SetupOutputVariable("Chiller Heat Recovery Mass Flow Rate", OutputProcessor::Unit::kg_s, this->HeatRecMdot, "System", "Average", this->Name);
            SetupOutputVariable("Chiller Effective Heat Rejection Temperature", OutputProcessor::Unit::C, this->ChillerCondAvgTemp, "System", "Average", this->Name);
        }
        if (DataGlobals::AnyEnergyManagementSystemInModel) {
            SetupEMSInternalVariable("Chiller Nominal Capacity", this->Name, "[W]", this->NomCap);
        }
    }

    void ElectricChillerSpecs::simulate(EnergyPlusData &state, const PlantLocation &calledFromLocation, bool FirstHVACIteration, Real64 &CurLoad, bool RunFlag)
    {
        if (calledFromLocation.loopNum == this->CWLoopNum) { // chilled water loop
            this->initialize(state.dataBranchInputManager, RunFlag, CurLoad);
            auto &sim_component(DataPlant::PlantLoop(this->CWLoopNum).LoopSide(this->CWLoopSideNum).Branch(this->CWBranchNum).Comp(this->CWCompNum));
            this->calculate(CurLoad, RunFlag, sim_component.FlowCtrl);
            this->update(CurLoad, RunFlag);
        } else if (calledFromLocation.loopNum == this->CDLoopNum) { // condenser loop
            PlantUtilities::UpdateChillerComponentCondenserSide(this->CDLoopNum,
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
            PlantUtilities::UpdateComponentHeatRecoverySide(this->HRLoopNum,
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

    void ElectricChillerSpecs::initialize(BranchInputManagerData &dataBranchInputManager, bool const RunFlag, Real64 const MyLoad)
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
        static std::string const RoutineName("InitElectricChiller");

        if (this->MyFlag) {
            // Locate the chillers on the plant loops for later usage
            bool errFlag = false;
            this->setupOutputVariables();
            PlantUtilities::ScanPlantLoopsForObject(dataBranchInputManager,
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
            if (this->CondenserType != DataPlant::CondenserType::AIRCOOLED && this->CondenserType != DataPlant::CondenserType::EVAPCOOLED) {
                PlantUtilities::ScanPlantLoopsForObject(dataBranchInputManager,
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
                    this->CWLoopNum, this->CWLoopSideNum, this->CDLoopNum, this->CDLoopSideNum, this->plantTypeOfNum, true);
            }
            if (this->HeatRecActive) {
                PlantUtilities::ScanPlantLoopsForObject(dataBranchInputManager,
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
                    this->CWLoopNum, this->CWLoopSideNum, this->HRLoopNum, this->HRLoopSideNum, this->plantTypeOfNum, true);
            }

            if (this->CondenserType != DataPlant::CondenserType::AIRCOOLED && this->CondenserType != DataPlant::CondenserType::EVAPCOOLED && this->HeatRecActive) {
                PlantUtilities::InterConnectTwoPlantLoopSides(
                    this->CDLoopNum, this->CDLoopSideNum, this->HRLoopNum, this->HRLoopSideNum, this->plantTypeOfNum, false);
            }

            if (errFlag) {
                ShowFatalError("InitElectricChiller: Program terminated due to previous condition(s).");
            }

            if (this->FlowMode == DataPlant::FlowMode::CONSTANT) {
                // reset flow priority
                DataPlant::PlantLoop(this->CWLoopNum).LoopSide(this->CWLoopSideNum).Branch(this->CWBranchNum).Comp(this->CWCompNum).FlowPriority =
                    DataPlant::LoopFlowStatus_NeedyIfLoopOn;
            }

            if (this->FlowMode == DataPlant::FlowMode::LEAVINGSETPOINTMODULATED) {
                // reset flow priority
                DataPlant::PlantLoop(this->CWLoopNum).LoopSide(this->CWLoopSideNum).Branch(this->CWBranchNum).Comp(this->CWCompNum).FlowPriority =
                    DataPlant::LoopFlowStatus_NeedyIfLoopOn;

                // check if setpoint on outlet node
                if ((DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPoint == DataLoopNode::SensedNodeFlagValue) &&
                    (DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPointHi == DataLoopNode::SensedNodeFlagValue)) {
                    if (!DataGlobals::AnyEnergyManagementSystemInModel) {
                        if (!this->ModulatedFlowErrDone) {
                            ShowWarningError("Missing temperature setpoint for LeavingSetpointModulated mode chiller named " + this->Name);
                            ShowContinueError(
                                "  A temperature setpoint is needed at the outlet node of a chiller in variable flow mode, use a SetpointManager");
                            ShowContinueError("  The overall loop setpoint will be assumed for chiller. The simulation continues ... ");
                            this->ModulatedFlowErrDone = true;
                        }
                    } else {
                        // need call to EMS to check node
                        bool FatalError = false; // but not really fatal yet, but should be.
                        EMSManager::CheckIfNodeSetPointManagedByEMS(this->EvapOutletNodeNum, EMSManager::iTemperatureSetPoint, FatalError);
                        if (FatalError) {
                            if (!this->ModulatedFlowErrDone) {
                                ShowWarningError("Missing temperature setpoint for LeavingSetpointModulated mode chiller named " + this->Name);
                                ShowContinueError(
                                    "  A temperature setpoint is needed at the outlet node of a chiller evaporator in variable flow mode");
                                ShowContinueError("  use a Setpoint Manager to establish a setpoint at the chiller evaporator outlet node ");
                                ShowContinueError("  or use an EMS actuator to establish a setpoint at the outlet node ");
                                ShowContinueError("  The overall loop setpoint will be assumed for chiller. The simulation continues ... ");
                                this->ModulatedFlowErrDone = true;
                            }
                        }
                    }
                    this->ModulatedFlowSetToLoop = true;
                    {
                        auto const SELECT_CASE_var(DataPlant::PlantLoop(this->CWLoopNum).LoopDemandCalcScheme);
                        if (SELECT_CASE_var == DataPlant::SingleSetPoint) {
                            DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPoint = DataLoopNode::Node(DataPlant::PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPoint;
                        } else if (SELECT_CASE_var == DataPlant::DualSetPointDeadBand) {
                            DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPointHi =
                                DataLoopNode::Node(DataPlant::PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPointHi;
                        }
                    }
                }
            }
            this->MyFlag = false;
        }

        if (this->MyEnvrnFlag && DataGlobals::BeginEnvrnFlag && (DataPlant::PlantFirstSizesOkayToFinalize)) {

            Real64 rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->CWLoopNum).FluidName,
                                                           DataGlobals::CWInitConvTemp,
                                                           DataPlant::PlantLoop(this->CWLoopNum).FluidIndex,
                                                           RoutineName);

            this->EvapMassFlowRateMax = rho * this->EvapVolFlowRate;
            PlantUtilities::InitComponentNodes(0.0,
                                               this->EvapMassFlowRateMax,
                                               this->EvapInletNodeNum,
                                               this->EvapOutletNodeNum,
                                               this->CWLoopNum,
                                               this->CWLoopSideNum,
                                               this->CWBranchNum,
                                               this->CWCompNum);

            // init maximum available condenser flow rate
            if (this->CondenserType == DataPlant::CondenserType::WATERCOOLED) {

                DataLoopNode::Node(this->CondInletNodeNum).Temp = this->TempDesCondIn; // DSU? old behavior, still want?

                rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->CDLoopNum).FluidName,
                                                        DataGlobals::CWInitConvTemp,
                                                        DataPlant::PlantLoop(this->CDLoopNum).FluidIndex,
                                                        RoutineName);

                this->CondMassFlowRateMax = rho * this->CondVolFlowRate;

                PlantUtilities::InitComponentNodes(0.0,
                                                   this->CondMassFlowRateMax,
                                                   this->CondInletNodeNum,
                                                   this->CondOutletNodeNum,
                                                   this->CDLoopNum,
                                                   this->CDLoopSideNum,
                                                   this->CDBranchNum,
                                                   this->CDCompNum);
            } else { // air or evap-air

                rho = Psychrometrics::PsyRhoAirFnPbTdbW(DataEnvironment::StdBaroPress, this->TempDesCondIn, 0.0, RoutineName);
                this->CondMassFlowRateMax = rho * this->CondVolFlowRate;

                DataLoopNode::Node(this->CondInletNodeNum).MassFlowRate = this->CondMassFlowRateMax;
                DataLoopNode::Node(this->CondOutletNodeNum).MassFlowRate = DataLoopNode::Node(this->CondInletNodeNum).MassFlowRate;
                DataLoopNode::Node(this->CondInletNodeNum).MassFlowRateMaxAvail = DataLoopNode::Node(this->CondInletNodeNum).MassFlowRate;
                DataLoopNode::Node(this->CondInletNodeNum).MassFlowRateMax = DataLoopNode::Node(this->CondInletNodeNum).MassFlowRate;
                DataLoopNode::Node(this->CondOutletNodeNum).MassFlowRateMax = DataLoopNode::Node(this->CondInletNodeNum).MassFlowRate;
                DataLoopNode::Node(this->CondInletNodeNum).MassFlowRateMinAvail = 0.0;
                DataLoopNode::Node(this->CondInletNodeNum).MassFlowRateMin = 0.0;
                DataLoopNode::Node(this->CondOutletNodeNum).MassFlowRateMinAvail = 0.0;
                DataLoopNode::Node(this->CondOutletNodeNum).MassFlowRateMin = 0.0;
            }

            if (this->HeatRecActive) {
                rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->HRLoopNum).FluidName,
                                                        DataGlobals::HWInitConvTemp,
                                                        DataPlant::PlantLoop(this->HRLoopNum).FluidIndex,
                                                        RoutineName);
                this->DesignHeatRecMassFlowRate = rho * this->DesignHeatRecVolFlowRate;

                PlantUtilities::InitComponentNodes(0.0,
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
                        auto const SELECT_CASE_var(DataPlant::PlantLoop(this->HRLoopNum).LoopDemandCalcScheme);
                        if (SELECT_CASE_var == DataPlant::SingleSetPoint) {
                            THeatRecSetPoint = DataLoopNode::Node(this->HeatRecSetPointNodeNum).TempSetPoint;
                        } else if (SELECT_CASE_var == DataPlant::DualSetPointDeadBand) {
                            THeatRecSetPoint = DataLoopNode::Node(this->HeatRecSetPointNodeNum).TempSetPointHi;
                        }
                    }
                    if (THeatRecSetPoint == DataLoopNode::SensedNodeFlagValue) {
                        if (!DataGlobals::AnyEnergyManagementSystemInModel) {
                            if (!this->HRSPErrDone) {
                                ShowWarningError("Missing heat recovery temperature setpoint for chiller named " + this->Name);
                                ShowContinueError("  A temperature setpoint is needed at the heat recovery leaving temperature setpoint node "
                                                  "specified, use a SetpointManager");
                                ShowContinueError("  The overall loop setpoint will be assumed for heat recovery. The simulation continues ...");
                                this->HeatRecSetPointNodeNum = DataPlant::PlantLoop(this->HRLoopNum).TempSetPointNodeNum;
                                this->HRSPErrDone = true;
                            }
                        } else {
                            // need call to EMS to check node
                            bool FatalError = false; // but not really fatal yet, but should be.
                            EMSManager::CheckIfNodeSetPointManagedByEMS(this->EvapOutletNodeNum, EMSManager::iTemperatureSetPoint, FatalError);
                            if (FatalError) {
                                if (!this->HRSPErrDone) {
                                    ShowWarningError("Missing heat recovery temperature setpoint for chiller named " + this->Name);
                                    ShowContinueError("  A temperature setpoint is needed at the heat recovery leaving temperature setpoint node "
                                                      "specified, use a SetpointManager to establish a setpoint");
                                    ShowContinueError("  or use an EMS actuator to establish a setpoint at this node ");
                                    ShowContinueError("  The overall loop setpoint will be assumed for heat recovery. The simulation continues ...");
                                    this->HeatRecSetPointNodeNum = DataPlant::PlantLoop(this->HRLoopNum).TempSetPointNodeNum;
                                    this->HRSPErrDone = true;
                                }
                            }
                        } // IF (.NOT. AnyEnergyManagementSystemInModel) THEN
                    }     // IF(THeatRecSetpoint == DataLoopNode::SensedNodeFlagValue)THEN
                }         // IF(ElectricChiller(ChillNum)%HeatRecSetpointNodeNum > 0)THEN
            }             // IF (ElectricChiller(ChillNum)%HeatRecActive) THEN

            this->MyEnvrnFlag = false;
        }
        if (!DataGlobals::BeginEnvrnFlag) {
            this->MyEnvrnFlag = true;
        }

        if ((this->FlowMode == DataPlant::FlowMode::LEAVINGSETPOINTMODULATED) && (this->ModulatedFlowSetToLoop)) {
            // fix for clumsy old input that worked because loop setpoint was spread.
            //  could be removed with transition, testing , model change, period of being obsolete.
            {
                auto const SELECT_CASE_var(DataPlant::PlantLoop(this->CWLoopNum).LoopDemandCalcScheme);
                if (SELECT_CASE_var == DataPlant::SingleSetPoint) {
                    DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPoint = DataLoopNode::Node(DataPlant::PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPoint;
                } else if (SELECT_CASE_var == DataPlant::DualSetPointDeadBand) {
                    DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPointHi = DataLoopNode::Node(DataPlant::PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPointHi;
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
            mdot, this->EvapInletNodeNum, this->EvapOutletNodeNum, this->CWLoopNum, this->CWLoopSideNum, this->CWBranchNum, this->CWCompNum);
        if (this->CondenserType == DataPlant::CondenserType::WATERCOOLED) {
            PlantUtilities::SetComponentFlowRate(
                mdotCond, this->CondInletNodeNum, this->CondOutletNodeNum, this->CDLoopNum, this->CDLoopSideNum, this->CDBranchNum, this->CDCompNum);
        }

        // Initialize heat recovery flow rates at node
        if (this->HeatRecActive) {

            Real64 thisMdot = 0.0;
            if (RunFlag) {
                thisMdot = this->DesignHeatRecMassFlowRate;
            }

            PlantUtilities::SetComponentFlowRate(thisMdot,
                                                 this->HeatRecInletNodeNum,
                                                 this->HeatRecOutletNodeNum,
                                                 this->HRLoopNum,
                                                 this->HRLoopSideNum,
                                                 this->HRBranchNum,
                                                 this->HRCompNum);
        }

        if (this->CondenserType == DataPlant::CondenserType::EVAPCOOLED) {
            this->BasinHeaterPower = 0.0;
        }
    }

    void ElectricChillerSpecs::size()
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
        static std::string const RoutineName("SizeElectricChiller");

        int PltSizCondNum(0);    // Plant Sizing index for condenser loop
        bool ErrorsFound(false); // If errors detected in input

        if (this->CondenserType == DataPlant::CondenserType::WATERCOOLED) {
            PltSizCondNum = DataPlant::PlantLoop(this->CDLoopNum).PlantSizNum;
        }

        int PltSizNum = DataPlant::PlantLoop(this->CWLoopNum).PlantSizNum;

        Real64 tmpNomCap = this->NomCap;
        if (PltSizNum > 0) {
            if (DataSizing::PlantSizData(PltSizNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                Real64 rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->CWLoopNum).FluidName,
                                                        DataGlobals::CWInitConvTemp,
                                                        DataPlant::PlantLoop(this->CWLoopNum).FluidIndex,
                                                        RoutineName);
                Real64 Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(this->CWLoopNum).FluidName,
                                                            DataGlobals::CWInitConvTemp,
                                                            DataPlant::PlantLoop(this->CWLoopNum).FluidIndex,
                                                            RoutineName);
                tmpNomCap = Cp * rho * DataSizing::PlantSizData(PltSizNum).DeltaT * DataSizing::PlantSizData(PltSizNum).DesVolFlowRate * this->SizFac;
            } else {
                if (this->NomCapWasAutoSized) tmpNomCap = 0.0;
            }
            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                if (this->NomCapWasAutoSized) {
                    this->NomCap = tmpNomCap;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput("Chiller:Electric", this->Name, "Design Size Nominal Capacity [W]", tmpNomCap);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(
                            "Chiller:Electric", this->Name, "Initial Design Size Nominal Capacity [W]", tmpNomCap);
                    }
                } else {
                    if (this->NomCap > 0.0 && tmpNomCap > 0.0) {
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput("Chiller:Electric",
                                                                    this->Name,
                                                                    "Design Size Nominal Capacity [W]",
                                                                    tmpNomCap,
                                                                    "User-Specified Nominal Capacity [W]",
                                                                    this->NomCap);
                            if (DataGlobals::DisplayExtraWarnings) {
                                if ((std::abs(tmpNomCap - this->NomCap) / this->NomCap) > DataSizing::AutoVsHardSizingThreshold) {
                                    ShowMessage("SizeChillerElectric: Potential issue with equipment sizing for " + this->Name);
                                    ShowContinueError("User-Specified Nominal Capacity of " + General::RoundSigDigits(this->NomCap, 2) + " [W]");
                                    ShowContinueError("differs from Design Size Nominal Capacity of " + General::RoundSigDigits(tmpNomCap, 2) + " [W]");
                                    ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                        }
                        tmpNomCap = this->NomCap;
                    }
                }
            }
        } else {
            if (this->NomCapWasAutoSized && DataPlant::PlantFirstSizesOkayToFinalize) {
                ShowSevereError("Autosizing of Electric Chiller nominal capacity requires a loop Sizing:Plant object");
                ShowContinueError("Occurs in Electric Chiller object=" + this->Name);
                ErrorsFound = true;
            }
            if (!this->NomCapWasAutoSized && DataPlant::PlantFinalSizesOkayToReport && (this->NomCap > 0.0)) {
                ReportSizingManager::ReportSizingOutput("Chiller:Electric", this->Name, "User-Specified Nominal Capacity [W]", this->NomCap);
            }
        }

        Real64 tmpEvapVolFlowRate = this->EvapVolFlowRate;
        if (PltSizNum > 0) {
            if (DataSizing::PlantSizData(PltSizNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                tmpEvapVolFlowRate = DataSizing::PlantSizData(PltSizNum).DesVolFlowRate * this->SizFac;
            } else {
                if (this->EvapVolFlowRateWasAutoSized) tmpEvapVolFlowRate = 0.0;
            }
            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                if (this->EvapVolFlowRateWasAutoSized) {
                    this->EvapVolFlowRate = tmpEvapVolFlowRate;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(
                            "Chiller:Electric", this->Name, "Design Size Design Chilled Water Flow Rate [m3/s]", tmpEvapVolFlowRate);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(
                            "Chiller:Electric", this->Name, "Initial Design Size Design Chilled Water Flow Rate [m3/s]", tmpEvapVolFlowRate);
                    }
                } else {
                    if (this->EvapVolFlowRate > 0.0 && tmpEvapVolFlowRate > 0.0) {
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput("Chiller:Electric",
                                                                    this->Name,
                                                                    "Design Size Design Chilled Water Flow Rate [m3/s]",
                                                                    tmpEvapVolFlowRate,
                                                                    "User-Specified Design Chilled Water Flow Rate [m3/s]",
                                                                    this->EvapVolFlowRate);
                            if (DataGlobals::DisplayExtraWarnings) {
                                if ((std::abs(tmpEvapVolFlowRate - this->EvapVolFlowRate) / this->EvapVolFlowRate) >
                                    DataSizing::AutoVsHardSizingThreshold) {
                                    ShowMessage("SizeChillerElectric: Potential issue with equipment sizing for " + this->Name);
                                    ShowContinueError("User-Specified Design Chilled Water Flow Rate of " + General::RoundSigDigits(this->EvapVolFlowRate, 5) +
                                                      " [m3/s]");
                                    ShowContinueError("differs from Design Size Design Chilled Water Flow Rate of " +
                                                      General::RoundSigDigits(tmpEvapVolFlowRate, 5) + " [m3/s]");
                                    ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                        }
                        tmpEvapVolFlowRate = this->EvapVolFlowRate;
                    }
                }
            }
        } else {
            if (this->EvapVolFlowRateWasAutoSized && DataPlant::PlantFirstSizesOkayToFinalize) {
                ShowSevereError("Autosizing of Electric Chiller evap flow rate requires a loop Sizing:Plant object");
                ShowContinueError("Occurs in Electric Chiller object=" + this->Name);
                ErrorsFound = true;
            }
            if (!this->EvapVolFlowRateWasAutoSized && DataPlant::PlantFinalSizesOkayToReport && (this->EvapVolFlowRate > 0.0)) {
                ReportSizingManager::ReportSizingOutput(
                    "Chiller:Electric", this->Name, "User-Specified Design Chilled Water Flow Rate [m3/s]", this->EvapVolFlowRate);
            }
        }

        PlantUtilities::RegisterPlantCompDesignFlow(this->EvapInletNodeNum, tmpEvapVolFlowRate);

        Real64 tmpCondVolFlowRate = this->CondVolFlowRate;
        if (PltSizCondNum > 0 && PltSizNum > 0) {
            if (DataSizing::PlantSizData(PltSizNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow && tmpNomCap > 0.0) {
                Real64 rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->CDLoopNum).FluidName,
                                                        this->TempDesCondIn,
                                                        DataPlant::PlantLoop(this->CDLoopNum).FluidIndex,
                                                        RoutineName);
                Real64 Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(this->CDLoopNum).FluidName,
                                                            this->TempDesCondIn,
                                                            DataPlant::PlantLoop(this->CDLoopNum).FluidIndex,
                                                            RoutineName);
                tmpCondVolFlowRate = tmpNomCap * (1.0 + 1.0 / this->COP) / (DataSizing::PlantSizData(PltSizCondNum).DeltaT * Cp * rho);
            } else {
                if (this->CondVolFlowRateWasAutoSized) tmpCondVolFlowRate = 0.0;
            }
            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                if (this->CondVolFlowRateWasAutoSized) {
                    this->CondVolFlowRate = tmpCondVolFlowRate;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(
                            "Chiller:Electric", this->Name, "Design Size Design Condenser Water Flow Rate [m3/s]", tmpCondVolFlowRate);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(
                            "Chiller:Electric", this->Name, "Initial Design Size Design Condenser Water Flow Rate [m3/s]", tmpCondVolFlowRate);
                    }
                } else {
                    if (this->CondVolFlowRate > 0.0 && tmpCondVolFlowRate > 0.0) {
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput("Chiller:Electric",
                                                                    this->Name,
                                                                    "Design Size Design Condenser Water Flow Rate [m3/s]",
                                                                    tmpCondVolFlowRate,
                                                                    "User-Specified Design Condenser Water Flow Rate [m3/s]",
                                                                    this->CondVolFlowRate);
                            if (DataGlobals::DisplayExtraWarnings) {
                                if ((std::abs(tmpCondVolFlowRate - this->CondVolFlowRate) / this->CondVolFlowRate) >
                                    DataSizing::AutoVsHardSizingThreshold) {
                                    ShowMessage("SizeChillerElectric: Potential issue with equipment sizing for " + this->Name);
                                    ShowContinueError("User-Specified Design Condenser Water Flow Rate of " + General::RoundSigDigits(this->CondVolFlowRate, 5) +
                                                      " [m3/s]");
                                    ShowContinueError("differs from Design Size Design Condenser Water Flow Rate of " +
                                                      General::RoundSigDigits(tmpCondVolFlowRate, 5) + " [m3/s]");
                                    ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                        }
                        tmpCondVolFlowRate = this->CondVolFlowRate;
                    }
                }
            }
        } else {
            if (this->CondVolFlowRateWasAutoSized && DataPlant::PlantFirstSizesOkayToFinalize) {
                ShowSevereError("Autosizing of Electric Chiller condenser flow rate requires a condenser");
                ShowContinueError("loop Sizing:Plant object");
                ShowContinueError("Occurs in Electric Chiller object=" + this->Name);
                ErrorsFound = true;
            }
            if (!this->CondVolFlowRateWasAutoSized && DataPlant::PlantFinalSizesOkayToReport && (this->CondVolFlowRate > 0.0)) {
                ReportSizingManager::ReportSizingOutput(
                    "Chiller:Electric", this->Name, "User-Specified Design Condenser Water Flow Rate [m3/s]", this->CondVolFlowRate);
            }
        }

        // save the design condenser water volumetric flow rate for use by the condenser water loop sizing algorithms
        if (this->CondenserType == DataPlant::CondenserType::WATERCOOLED) {
            PlantUtilities::RegisterPlantCompDesignFlow(this->CondInletNodeNum, tmpCondVolFlowRate);
        }
        if (ErrorsFound) {
            ShowFatalError("Preceding sizing errors cause program termination");
        }

        if (this->HeatRecActive) {
            Real64 tmpHeatRecVolFlowRate = this->CondVolFlowRate * this->HeatRecCapacityFraction;
            if (!this->DesignHeatRecVolFlowRateWasAutoSized) tmpHeatRecVolFlowRate = this->DesignHeatRecVolFlowRate;
            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                if (this->DesignHeatRecVolFlowRateWasAutoSized) {
                    this->DesignHeatRecVolFlowRate = tmpHeatRecVolFlowRate;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(
                            "Chiller:Electric", this->Name, "Design Size Design Heat Recovery Fluid Flow Rate [m3/s]", tmpHeatRecVolFlowRate);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(
                            "Chiller:Electric", this->Name, "Initial Design Size Design Heat Recovery Fluid Flow Rate [m3/s]", tmpHeatRecVolFlowRate);
                    }
                } else {
                    if (this->DesignHeatRecVolFlowRate > 0.0 && tmpHeatRecVolFlowRate > 0.0) {
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput("Chiller:Electric",
                                                                    this->Name,
                                                                    "Design Size Design Heat Recovery Fluid Flow Rate [m3/s]",
                                                                    tmpHeatRecVolFlowRate,
                                                                    "User-Specified Design Heat Recovery Fluid Flow Rate [m3/s]",
                                                                    this->DesignHeatRecVolFlowRate);
                            if (DataGlobals::DisplayExtraWarnings) {
                                if ((std::abs(tmpHeatRecVolFlowRate - this->DesignHeatRecVolFlowRate) / this->DesignHeatRecVolFlowRate) >
                                    DataSizing::AutoVsHardSizingThreshold) {
                                    ShowMessage("SizeChillerElectric: Potential issue with equipment sizing for " + this->Name);
                                    ShowContinueError("User-Specified Design Heat Recovery Fluid Flow Rate of " +
                                                      General::RoundSigDigits(this->DesignHeatRecVolFlowRate, 5) + " [m3/s]");
                                    ShowContinueError("differs from Design Size Design Heat Recovery Fluid Flow Rate of " +
                                                      General::RoundSigDigits(tmpHeatRecVolFlowRate, 5) + " [m3/s]");
                                    ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                        }
                        tmpHeatRecVolFlowRate = this->DesignHeatRecVolFlowRate;
                    }
                }
            }
            // save the reference heat recovery fluid volumetric flow rate
            PlantUtilities::RegisterPlantCompDesignFlow(this->HeatRecInletNodeNum, tmpHeatRecVolFlowRate);
        }

        if (DataPlant::PlantFinalSizesOkayToReport) {
            OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchMechType, this->Name, "Chiller:Electric");
            OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchMechNomEff, this->Name, this->COP);
            OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchMechNomCap, this->Name, this->NomCap);
        }
    }

    void ElectricChillerSpecs::calculate(Real64 &MyLoad, bool const RunFlag, int const EquipFlowCtrl)
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

        static std::string const RoutineName("CalcElectricChillerModel");
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
        Real64 CurrentEndTime = DataGlobals::CurrentTime + DataHVACGlobals::SysTimeElapsed;

        //   Print warning messages only when valid and only for the first occurrence. Let summary provide statistics.
        //   Wait for next time step to print warnings. If simulation iterates, print out
        //   the warning for the last iteration only. Must wait for next time step to accomplish this.
        //   If a warning occurs and the simulation down shifts, the warning is not valid.
        if (CurrentEndTime > this->CurrentEndTimeLast && DataHVACGlobals::TimeStepSys >= this->TimeStepSysLast) {
            if (this->PrintMessage) {
                ++this->MsgErrorCount;
                //       Show single warning and pass additional info to ShowRecurringWarningErrorAtEnd
                if (this->MsgErrorCount < 2) {
                    ShowWarningError(this->MsgBuffer1 + '.');
                    ShowContinueError(this->MsgBuffer2);
                } else {
                    ShowRecurringWarningErrorAtEnd(
                        this->MsgBuffer1 + " error continues.", this->ErrCount1, this->MsgDataLast, this->MsgDataLast, _, "[C]", "[C]");
                }
            }
        }

        //   save last system time step and last end time of current time step (used to determine if warning is valid)
        this->TimeStepSysLast = DataHVACGlobals::TimeStepSys;
        this->CurrentEndTimeLast = CurrentEndTime;

        // If no loop demand or chiller OFF, return
        // If Chiller load is 0 or chiller is not running then leave the subroutine.
        if (MyLoad >= 0.0 || !RunFlag) {
            // call for zero flow before leaving
            if (EquipFlowCtrl == DataBranchAirLoopPlant::ControlType_SeriesActive ||
                DataPlant::PlantLoop(this->CWLoopNum).LoopSide(this->CWLoopSideNum).FlowLock == 1) {
                this->EvapMassFlowRate = DataLoopNode::Node(this->EvapInletNodeNum).MassFlowRate;
            } else {
                this->EvapMassFlowRate = 0.0;
                PlantUtilities::SetComponentFlowRate(
                    this->EvapMassFlowRate, this->EvapInletNodeNum, this->EvapOutletNodeNum, this->CWLoopNum, this->CWLoopSideNum, this->CWBranchNum, this->CWCompNum);
            }
            if (this->CondenserType == DataPlant::CondenserType::WATERCOOLED) {
                if (DataPlant::PlantLoop(this->CDLoopNum).LoopSide(this->CDLoopSideNum).Branch(this->CDBranchNum).Comp(this->CDCompNum).FlowCtrl ==
                    DataBranchAirLoopPlant::ControlType_SeriesActive) {
                    this->CondMassFlowRate = DataLoopNode::Node(this->CondInletNodeNum).MassFlowRate;
                } else {
                    this->CondMassFlowRate = 0.0;
                    PlantUtilities::SetComponentFlowRate(this->CondMassFlowRate,
                                                         this->CondInletNodeNum,
                                                         this->CondOutletNodeNum,
                                                         this->CDLoopNum,
                                                         this->CDLoopSideNum,
                                                         this->CDBranchNum,
                                                         this->CDCompNum);
                }
            }

            if (this->CondenserType == DataPlant::CondenserType::EVAPCOOLED) {
                CalcBasinHeaterPower(
                    this->BasinHeaterPowerFTempDiff, this->BasinHeaterSchedulePtr, this->BasinHeaterSetPointTemp, this->BasinHeaterPower);
            }
            this->PrintMessage = false;
            return;
        }

        // If not air or evap cooled then set to the condenser node that is attached to a cooling tower

        // Set mass flow rates
        if (this->CondenserType == DataPlant::CondenserType::WATERCOOLED) {
            this->CondMassFlowRate = this->CondMassFlowRateMax;
            PlantUtilities::SetComponentFlowRate(
                this->CondMassFlowRate, this->CondInletNodeNum, this->CondOutletNodeNum, this->CDLoopNum, this->CDLoopSideNum, this->CDBranchNum, this->CDCompNum);
            PlantUtilities::PullCompInterconnectTrigger(this->CWLoopNum,
                                                        this->CWLoopSideNum,
                                                        this->CWBranchNum,
                                                        this->CWCompNum,
                                                        this->CondMassFlowIndex,
                                                        this->CDLoopNum,
                                                        this->CDLoopSideNum,
                                                        DataPlant::CriteriaType_MassFlowRate,
                                                        this->CondMassFlowRate);
            if (this->CondMassFlowRate < DataBranchAirLoopPlant::MassFlowTolerance) return;
        }

        //  LOAD LOCAL VARIABLES FROM DATA STRUCTURE (for code readability)
        Real64 PartLoadRat = this->MinPartLoadRat;
        Real64 ChillerNomCap = this->NomCap;
        Real64 TempEvapOut = DataLoopNode::Node(this->EvapOutletNodeNum).Temp;

        // If there is a fault of chiller fouling
        if (this->FaultyChillerFoulingFlag && (!DataGlobals::WarmupFlag) && (!DataGlobals::DoingSizing) && (!DataGlobals::KickOffSimulation)) {
            int FaultIndex = this->FaultyChillerFoulingIndex;
            Real64 NomCap_ff = ChillerNomCap;
            Real64 RatedCOP_ff = this->COP;

            // calculate the Faulty Chiller Fouling Factor using fault information
            this->FaultyChillerFoulingFactor = FaultsManager::FaultsChillerFouling(FaultIndex).CalFoulingFactor();

            // update the Chiller nominal capacity and COP at faulty cases
            ChillerNomCap = NomCap_ff * this->FaultyChillerFoulingFactor;
            this->COP = RatedCOP_ff * this->FaultyChillerFoulingFactor;
        }

        // initialize outlet air humidity ratio of air or evap cooled chillers
        this->CondOutletHumRat = DataLoopNode::Node(this->CondInletNodeNum).HumRat;

        if (this->CondenserType == DataPlant::CondenserType::AIRCOOLED) { // Condenser inlet temp = outdoor temp
            DataLoopNode::Node(this->CondInletNodeNum).Temp = DataLoopNode::Node(this->CondInletNodeNum).OutAirDryBulb;
            //  Warn user if entering condenser temperature falls below 0C
            if (DataLoopNode::Node(this->CondInletNodeNum).Temp < 0.0 && !DataGlobals::WarmupFlag) {
                this->PrintMessage = true;
                this->MsgBuffer1 =
                    format("CalcElectricChillerModel - Chiller:Electric \"{}\" - Air Cooled Condenser Inlet Temperature below 0C", this->Name);
                this->MsgBuffer2 = format("... Outdoor Dry-bulb Condition = {:6.2F} C. Occurrence info = {}, {} {}",
                                          DataLoopNode::Node(this->CondInletNodeNum).Temp,
                                          DataEnvironment::EnvironmentName,
                                          DataEnvironment::CurMnDy,
                                          General::CreateSysTimeIntervalString());
                this->MsgDataLast = DataLoopNode::Node(this->CondInletNodeNum).Temp;
            } else {
                this->PrintMessage = false;
            }
        } else if (this->CondenserType == DataPlant::CondenserType::EVAPCOOLED) { // Condenser inlet temp = (outdoor wet bulb)
            DataLoopNode::Node(this->CondInletNodeNum).Temp = DataLoopNode::Node(this->CondInletNodeNum).OutAirWetBulb;
            //  line above assumes evaporation pushes condenser inlet air humidity ratio to saturation
            this->CondOutletHumRat = Psychrometrics::PsyWFnTdbTwbPb(DataLoopNode::Node(this->CondInletNodeNum).Temp, DataLoopNode::Node(this->CondInletNodeNum).Temp, DataLoopNode::Node(this->CondInletNodeNum).Press);
            //  Warn user if evap condenser wet bulb temperature falls below 10C
            if (DataLoopNode::Node(this->CondInletNodeNum).Temp < 10.0 && !DataGlobals::WarmupFlag) {
                this->PrintMessage = true;
                this->MsgBuffer1 =
                    format("CalcElectricChillerModel - Chiller:Electric \"{}\" - Evap Cooled Condenser Inlet Temperature below 10C", this->Name);
                this->MsgBuffer2 = format("... Outdoor Wet-bulb Condition = {:6.2F} C. Occurrence info = {}, {} {}",
                                          DataLoopNode::Node(this->CondInletNodeNum).Temp,
                                          DataEnvironment::EnvironmentName,
                                          DataEnvironment::CurMnDy,
                                          General::CreateSysTimeIntervalString());
                this->MsgDataLast = DataLoopNode::Node(this->CondInletNodeNum).Temp;
            } else {
                this->PrintMessage = false;
            }
        } // End of the Air Cooled/Evap Cooled Logic block

        Real64 condInletTemp = DataLoopNode::Node(this->CondInletNodeNum).Temp;

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
        if (this->FaultyChillerSWTFlag && (!DataGlobals::WarmupFlag) && (!DataGlobals::DoingSizing) && (!DataGlobals::KickOffSimulation)) {
            int FaultIndex = this->FaultyChillerSWTIndex;
            Real64 EvapOutletTemp_ff = TempEvapOut;

            // calculate the sensor offset using fault information
            this->FaultyChillerSWTOffset = FaultsManager::FaultsChillerSWTSensor(FaultIndex).CalFaultOffsetAct();
            // update the TempEvapOut
            TempEvapOut = max(this->TempLowLimitEvapOut, min(DataLoopNode::Node(this->EvapInletNodeNum).Temp, EvapOutletTemp_ff - this->FaultyChillerSWTOffset));
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

        Real64 OperPartLoadRat;          // Actual Operating PLR
        if (AvailChillerCap > 0.0) {
            if (std::abs(MyLoad) / AvailChillerCap < this->MinPartLoadRat) {
                OperPartLoadRat = std::abs(MyLoad) / AvailChillerCap;
            } else {
                OperPartLoadRat = PartLoadRat;
            }
        } else {
            OperPartLoadRat = 0.0;
        }

        Real64 Cp = FluidProperties::GetSpecificHeatGlycol(
            DataPlant::PlantLoop(this->CWLoopNum).FluidName, DataLoopNode::Node(this->EvapInletNodeNum).Temp, DataPlant::PlantLoop(this->CWLoopNum).FluidIndex, RoutineName);

        // If FlowLock is True, the new resolved mdot is used to update Power, QEvap, Qcond, and
        // condenser side outlet temperature.
        if (DataPlant::PlantLoop(this->CWLoopNum).LoopSide(this->CWLoopSideNum).FlowLock == 0) {
            this->PossibleSubcooling = !(DataPlant::PlantLoop(this->CWLoopNum).LoopSide(this->CWLoopSideNum).Branch(this->CWLoopSideNum).Comp(this->CWCompNum).CurOpSchemeType ==
                                         DataPlant::CompSetPtBasedSchemeType);
            this->QEvaporator = AvailChillerCap * OperPartLoadRat;
            Real64 FRAC;
            if (OperPartLoadRat < this->MinPartLoadRat) {
                FRAC = min(1.0, (OperPartLoadRat / this->MinPartLoadRat));
            } else {
                FRAC = 1.0;
            }
            this->Power = FracFullLoadPower * FullLoadPowerRat * AvailChillerCap / this->COP * FRAC;

            // Either set the flow to the Constant value or calculate the flow for the variable volume
            if ((this->FlowMode == DataPlant::FlowMode::CONSTANT) || (this->FlowMode == DataPlant::FlowMode::NOTMODULATED)) {

                // Start by assuming max (design) flow
                this->EvapMassFlowRate = EvapMassFlowRateMax;
                // Use SetComponentFlowRate to decide actual flow
                PlantUtilities::SetComponentFlowRate(
                    this->EvapMassFlowRate, this->EvapInletNodeNum, this->EvapOutletNodeNum, this->CWLoopNum, this->CWLoopSideNum, this->CWBranchNum, this->CWCompNum);
                // Evaluate delta temp based on actual flow rate
                Real64 EvapDeltaTemp(0.0);
                if (this->EvapMassFlowRate != 0.0) {
                    EvapDeltaTemp = this->QEvaporator / this->EvapMassFlowRate / Cp;
                } else {
                    EvapDeltaTemp = 0.0;
                }
                // Evaluate outlet temp based on delta
                this->EvapOutletTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp - EvapDeltaTemp;

            } else if (this->FlowMode == DataPlant::FlowMode::LEAVINGSETPOINTMODULATED) {

                // Calculate the Delta Temp from the inlet temp to the chiller outlet setpoint
                Real64 EvapDeltaTemp(0.0);
                {
                    auto const SELECT_CASE_var(DataPlant::PlantLoop(this->CWLoopNum).LoopDemandCalcScheme);
                    if (SELECT_CASE_var == DataPlant::SingleSetPoint) {
                        EvapDeltaTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp - DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPoint;
                    } else if (SELECT_CASE_var == DataPlant::DualSetPointDeadBand) {
                        EvapDeltaTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp - DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPointHi;
                    }
                }

                if (EvapDeltaTemp != 0.0) {

                    // Calculate desired flow to request based on load
                    this->EvapMassFlowRate = std::abs(this->QEvaporator / Cp / EvapDeltaTemp);
                    // Check to see if the Maximum is exceeded, if so set to maximum
                    if ((this->EvapMassFlowRate - EvapMassFlowRateMax) > DataBranchAirLoopPlant::MassFlowTolerance) this->PossibleSubcooling = true;
                    this->EvapMassFlowRate = min(EvapMassFlowRateMax, this->EvapMassFlowRate);
                    // Use SetComponentFlowRate to decide actual flow
                    PlantUtilities::SetComponentFlowRate(this->EvapMassFlowRate,
                                                         this->EvapInletNodeNum,
                                                         this->EvapOutletNodeNum,
                                                         this->CWLoopNum,
                                                         this->CWLoopSideNum,
                                                         this->CWBranchNum,
                                                         this->CWCompNum);
                    {
                        auto const SELECT_CASE_var(DataPlant::PlantLoop(this->CWLoopNum).LoopDemandCalcScheme);
                        if (SELECT_CASE_var == DataPlant::SingleSetPoint) {
                            this->EvapOutletTemp = DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPoint;
                        } else if (SELECT_CASE_var == DataPlant::DualSetPointDeadBand) {
                            this->EvapOutletTemp = DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPointHi;
                        }
                    }

                } else {

                    // Try to request zero flow
                    this->EvapMassFlowRate = 0.0;
                    // Use SetComponentFlowRate to decide actual flow
                    PlantUtilities::SetComponentFlowRate(this->EvapMassFlowRate,
                                                         this->EvapInletNodeNum,
                                                         this->EvapOutletNodeNum,
                                                         this->CWLoopNum,
                                                         this->CWLoopSideNum,
                                                         this->CWBranchNum,
                                                         this->CWCompNum);
                    // No deltaT since component is not running
                    this->EvapOutletTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp;
                }

            } // End of Constant Variable Flow If Block

            // If there is a fault of Chiller SWT Sensor
            if (this->FaultyChillerSWTFlag && (!DataGlobals::WarmupFlag) && (!DataGlobals::DoingSizing) && (!DataGlobals::KickOffSimulation) &&
                (this->EvapMassFlowRate > 0)) {
                // calculate directly affected variables at faulty case: EvapOutletTemp, EvapMassFlowRate, QEvaporator
                int FaultIndex = this->FaultyChillerSWTIndex;
                bool VarFlowFlag = (this->FlowMode == DataPlant::FlowMode::LEAVINGSETPOINTMODULATED);
                FaultsManager::FaultsChillerSWTSensor(FaultIndex)
                    .CalFaultChillerSWT(VarFlowFlag,
                                        this->FaultyChillerSWTOffset,
                                        Cp,
                                        DataLoopNode::Node(this->EvapInletNodeNum).Temp,
                                        this->EvapOutletTemp,
                                        this->EvapMassFlowRate,
                                        this->QEvaporator);
            }

        } else { // If FlowLock is True

            this->EvapMassFlowRate = DataLoopNode::Node(this->EvapInletNodeNum).MassFlowRate;
            PlantUtilities::SetComponentFlowRate(
                this->EvapMassFlowRate, this->EvapInletNodeNum, this->EvapOutletNodeNum, this->CWLoopNum, this->CWLoopSideNum, this->CWBranchNum, this->CWCompNum);

            //       Some other component set the flow to 0. No reason to continue with calculations.
            if (this->EvapMassFlowRate == 0.0) {
                MyLoad = 0.0;
                if (this->CondenserType == DataPlant::CondenserType::EVAPCOOLED) {
                    CalcBasinHeaterPower(
                        this->BasinHeaterPowerFTempDiff, this->BasinHeaterSchedulePtr, this->BasinHeaterSetPointTemp, this->BasinHeaterPower);
                }
                this->PrintMessage = false;
                return;
            }
            // Flow resolver might have given less flow or control scheme have provided more load, which may
            // result in subcooling.
            if (this->PossibleSubcooling) {
                this->QEvaporator = std::abs(MyLoad);
                Real64 EvapDeltaTemp = this->QEvaporator / this->EvapMassFlowRate / Cp;
                this->EvapOutletTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp - EvapDeltaTemp;
            } else { // No subcooling in this case.No recalculation required.Still need to check chiller low temp limit

                Real64 TempEvapOutSetPoint(0.0); // C - evaporator outlet temperature setpoint
                {
                    auto const SELECT_CASE_var(DataPlant::PlantLoop(this->CWLoopNum).LoopDemandCalcScheme);
                    if (SELECT_CASE_var == DataPlant::SingleSetPoint) {
                        if ((this->FlowMode == DataPlant::FlowMode::LEAVINGSETPOINTMODULATED) ||
                            (DataPlant::PlantLoop(this->CWLoopNum).LoopSide(this->CWLoopSideNum).Branch(this->CWLoopSideNum).Comp(this->CWCompNum).CurOpSchemeType ==
                             DataPlant::CompSetPtBasedSchemeType) ||
                            (DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPoint != DataLoopNode::SensedNodeFlagValue)) {
                            TempEvapOutSetPoint = DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPoint;
                        } else {
                            TempEvapOutSetPoint = DataLoopNode::Node(DataPlant::PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPoint;
                        }
                    } else if (SELECT_CASE_var == DataPlant::DualSetPointDeadBand) {
                        if ((this->FlowMode == DataPlant::FlowMode::LEAVINGSETPOINTMODULATED) ||
                            (DataPlant::PlantLoop(this->CWLoopNum).LoopSide(this->CWLoopSideNum).Branch(this->CWLoopSideNum).Comp(this->CWCompNum).CurOpSchemeType ==
                             DataPlant::CompSetPtBasedSchemeType) ||
                            (DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPointHi != DataLoopNode::SensedNodeFlagValue)) {
                            TempEvapOutSetPoint = DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPointHi;
                        } else {
                            TempEvapOutSetPoint = DataLoopNode::Node(DataPlant::PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPointHi;
                        }
                    }
                }
                Real64 EvapDeltaTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp - TempEvapOutSetPoint;
                this->QEvaporator = std::abs(this->EvapMassFlowRate * Cp * EvapDeltaTemp);
                this->EvapOutletTemp = TempEvapOutSetPoint;
            }
            // Check that the Evap outlet temp honors both plant loop temp low limit and also the chiller low limit
            if (this->EvapOutletTemp < this->TempLowLimitEvapOut) {
                if ((DataLoopNode::Node(this->EvapInletNodeNum).Temp - this->TempLowLimitEvapOut) > DataPlant::DeltaTempTol) {
                    this->EvapOutletTemp = this->TempLowLimitEvapOut;
                    Real64 EvapDeltaTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp - this->EvapOutletTemp;
                    this->QEvaporator = this->EvapMassFlowRate * Cp * EvapDeltaTemp;
                } else {
                    this->EvapOutletTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp;
                    Real64 EvapDeltaTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp - this->EvapOutletTemp;
                    this->QEvaporator = this->EvapMassFlowRate * Cp * EvapDeltaTemp;
                }
            }
            if (this->EvapOutletTemp < DataLoopNode::Node(this->EvapOutletNodeNum).TempMin) {
                if ((DataLoopNode::Node(this->EvapInletNodeNum).Temp - DataLoopNode::Node(this->EvapOutletNodeNum).TempMin) > DataPlant::DeltaTempTol) {
                    this->EvapOutletTemp = DataLoopNode::Node(this->EvapOutletNodeNum).TempMin;
                    Real64 EvapDeltaTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp - this->EvapOutletTemp;
                    this->QEvaporator = this->EvapMassFlowRate * Cp * EvapDeltaTemp;
                } else {
                    this->EvapOutletTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp;
                    Real64 EvapDeltaTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp - this->EvapOutletTemp;
                    this->QEvaporator = this->EvapMassFlowRate * Cp * EvapDeltaTemp;
                }
            }

            // If load exceeds the distributed load set to the distributed load
            if (this->QEvaporator > std::abs(MyLoad)) {
                if (this->EvapMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance) {
                    this->QEvaporator = std::abs(MyLoad);
                    Real64 EvapDeltaTemp = this->QEvaporator / this->EvapMassFlowRate / Cp;
                    this->EvapOutletTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp - EvapDeltaTemp;
                } else {
                    this->QEvaporator = 0.0;
                    this->EvapOutletTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp;
                }
            }

            // If there is a fault of Chiller SWT Sensor
            if (this->FaultyChillerSWTFlag && (!DataGlobals::WarmupFlag) && (!DataGlobals::DoingSizing) && (!DataGlobals::KickOffSimulation) &&
                (this->EvapMassFlowRate > 0)) {
                // calculate directly affected variables at faulty case: EvapOutletTemp, EvapMassFlowRate, QEvaporator
                int FaultIndex = this->FaultyChillerSWTIndex;
                bool VarFlowFlag = false;
                FaultsManager::FaultsChillerSWTSensor(FaultIndex)
                    .CalFaultChillerSWT(VarFlowFlag,
                                        this->FaultyChillerSWTOffset,
                                        Cp,
                                        DataLoopNode::Node(this->EvapInletNodeNum).Temp,
                                        this->EvapOutletTemp,
                                        this->EvapMassFlowRate,
                                        this->QEvaporator);
            }

            // Checks QEvaporator on the basis of the machine limits.
            if (this->QEvaporator > (AvailChillerCap * this->MaxPartLoadRat)) {
                if (this->EvapMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance) {
                    this->QEvaporator = AvailChillerCap * OperPartLoadRat;
                    Real64 EvapDeltaTemp = this->QEvaporator / this->EvapMassFlowRate / Cp;
                    this->EvapOutletTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp - EvapDeltaTemp;
                } else {
                    this->QEvaporator = 0.0;
                    this->EvapOutletTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp;
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
                this->EvapOutletTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp;
                this->Power = 0.0;
                this->PrintMessage = false;
            }
            if (this->QEvaporator == 0.0 && this->CondenserType == DataPlant::CondenserType::EVAPCOOLED) {
                CalcBasinHeaterPower(
                    this->BasinHeaterPowerFTempDiff, this->BasinHeaterSchedulePtr, this->BasinHeaterSetPointTemp, this->BasinHeaterPower);
            }
        } // This is the end of the FlowLock Block

        // QCondenser is calculated the same for each type, but the power consumption should be different
        //  depending on the performance coefficients used for the chiller model.
        this->QCondenser = this->Power + this->QEvaporator;

        if (this->CondenserType == DataPlant::CondenserType::WATERCOOLED) {
            if (this->CondMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance) {
                // If Heat Recovery specified for this vapor compression chiller, then Qcondenser will be adjusted by this subroutine
                if (this->HeatRecActive) this->calcHeatRecovery(this->QCondenser, this->CondMassFlowRate, condInletTemp, this->QHeatRecovered);
                Real64 CpCond = FluidProperties::GetSpecificHeatGlycol(
                    DataPlant::PlantLoop(this->CDLoopNum).FluidName, condInletTemp, DataPlant::PlantLoop(this->CDLoopNum).FluidIndex, RoutineName);
                this->CondOutletTemp = this->QCondenser / this->CondMassFlowRate / CpCond + condInletTemp;
            } else {
                ShowSevereError("CalcElectricChillerModel: Condenser flow = 0, for ElectricChiller=" + this->Name);
                ShowContinueErrorTimeStamp("");
            }
        } else { // Air Cooled or Evap Cooled

            if (this->QCondenser > 0.0) {
                this->CondMassFlowRate = this->CondMassFlowRateMax * OperPartLoadRat;
            } else {
                this->CondMassFlowRate = 0.0;
            }

            // If Heat Recovery specified for this vapor compression chiller, then Qcondenser will be adjusted by this subroutine
            if (this->HeatRecActive) this->calcHeatRecovery(this->QCondenser, this->CondMassFlowRate, condInletTemp, this->QHeatRecovered);
            if (this->CondMassFlowRate > 0.0) {
                Real64 CpCond = Psychrometrics::PsyCpAirFnW(DataLoopNode::Node(this->CondInletNodeNum).HumRat);
                this->CondOutletTemp = condInletTemp + this->QCondenser / this->CondMassFlowRate / CpCond;
            } else {
                this->CondOutletTemp = condInletTemp;
            }
        }

        // Calculate Energy
        this->CondenserEnergy = this->QCondenser * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
        this->Energy = this->Power * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
        this->EvaporatorEnergy = this->QEvaporator * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

        // check for problems (deal with observed negative energy results)
        if (this->Energy < 0.0) { // there is a serious problem

            if (this->CondenserType == DataPlant::CondenserType::WATERCOOLED) {
                // first check for run away condenser loop temps (only reason yet to be observed for this?)
                if (condInletTemp > 70.0) {
                    ShowSevereError("CalcElectricChillerModel: Condenser loop inlet temperatures over 70.0 C for ElectricChiller=" + this->Name);
                    ShowContinueErrorTimeStamp("");
                    ShowContinueError("Condenser loop water temperatures are too high at" + General::RoundSigDigits(condInletTemp, 2));
                    ShowContinueError("Check input for condenser plant loop, especially cooling tower");
                    ShowContinueError("Evaporator inlet temperature: " + General::RoundSigDigits(DataLoopNode::Node(this->EvapInletNodeNum).Temp, 2));

                    ShowFatalError("Program Terminates due to previous error condition");
                }
            }
            if (!DataGlobals::WarmupFlag) {
                if (AvailNomCapRat < 0.0) { // apparently the real reason energy goes negative
                    ShowSevereError("CalcElectricChillerModel: Capacity ratio below zero for ElectricChiller=" + this->Name);
                    ShowContinueErrorTimeStamp("");
                    ShowContinueError("Check input for Capacity Ratio Curve");
                    ShowContinueError("Condenser inlet temperature: " + General::RoundSigDigits(condInletTemp, 2));
                    ShowContinueError("Evaporator inlet temperature: " + General::RoundSigDigits(DataLoopNode::Node(this->EvapInletNodeNum).Temp, 2));
                    ShowFatalError("Program Terminates due to previous error condition");
                }
            }
            // If makes it here, set limits, chiller can't have negative energy/power
            // proceeding silently for now but may want to throw error here
            this->Power = 0.0;
            this->Energy = 0.0;
        }
    }

    void ElectricChillerSpecs::calcHeatRecovery(Real64 &QCond,               // current condenser load
                                                Real64 const CondMassFlow,   // current condenser Mass Flow
                                                Real64 const condInletTemp, // current condenser Inlet Temp
                                                Real64 &QHeatRec             // amount of heat recovered
    )
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Richard Liesen
        //       DATE WRITTEN:    January 2004

        // PURPOSE OF THIS SUBROUTINE:
        // Calculate the heat recovered from the chiller condenser

        static std::string const RoutineName("ChillerHeatRecovery");

        // setup initial state
        PlantUtilities::SafeCopyPlantNode(this->HeatRecInletNodeNum, this->HeatRecOutletNodeNum);
        this->QHeatRecovery = 0.0;
        this->EnergyHeatRecovery = 0.0;

        // Begin routine
        this->HeatRecInletTemp = DataLoopNode::Node(this->HeatRecInletNodeNum).Temp;
        Real64 HeatRecMassFlowRate = DataLoopNode::Node(this->HeatRecInletNodeNum).MassFlowRate;

        Real64 CpHeatRec = FluidProperties::GetSpecificHeatGlycol(
            DataPlant::PlantLoop(this->HRLoopNum).FluidName, this->HeatRecInletTemp, DataPlant::PlantLoop(this->HRLoopNum).FluidIndex, RoutineName);

        Real64 CpCond;
        if (this->CondenserType == DataPlant::CondenserType::WATERCOOLED) {
            CpCond = FluidProperties::GetSpecificHeatGlycol(
                DataPlant::PlantLoop(this->CDLoopNum).FluidName, condInletTemp, DataPlant::PlantLoop(this->CDLoopNum).FluidIndex, RoutineName);
        } else {
            CpCond = Psychrometrics::PsyCpAirFnW(DataLoopNode::Node(this->CondInletNodeNum).HumRat);
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
                auto const SELECT_CASE_var(DataPlant::PlantLoop(this->HRLoopNum).LoopDemandCalcScheme);

                if (SELECT_CASE_var == DataPlant::SingleSetPoint) {
                    THeatRecSetPoint = DataLoopNode::Node(this->HeatRecSetPointNodeNum).TempSetPoint;
                } else if (SELECT_CASE_var == DataPlant::DualSetPointDeadBand) {
                    THeatRecSetPoint = DataLoopNode::Node(this->HeatRecSetPointNodeNum).TempSetPointHi;
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
            Real64 HeatRecHighInletLimit = ScheduleManager::GetCurrentScheduleValue(this->HeatRecInletLimitSchedNum);
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
        this->EnergyHeatRecovery = this->QHeatRecovered * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
        DataLoopNode::Node(this->HeatRecOutletNodeNum).Temp = this->HeatRecOutletTemp;
        this->HeatRecMdot = DataLoopNode::Node(this->HeatRecInletNodeNum).MassFlowRate;
        this->ChillerCondAvgTemp = this->AvgCondSinkTemp;

    }

    void ElectricChillerSpecs::update(Real64 const MyLoad, bool const RunFlag)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Dan Fisher / Brandon Anderson
        //       DATE WRITTEN:    September 2000

        Real64 const ReportingConstant = DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

        if (MyLoad >= 0.0 || !RunFlag) { // Chiller not running so pass inlet states to outlet states
            // set node temperatures
            DataLoopNode::Node(this->EvapOutletNodeNum).Temp = DataLoopNode::Node(this->EvapInletNodeNum).Temp;
            DataLoopNode::Node(this->CondOutletNodeNum).Temp = DataLoopNode::Node(this->CondInletNodeNum).Temp;
            if (this->CondenserType != DataPlant::CondenserType::WATERCOOLED) {
                DataLoopNode::Node(this->CondOutletNodeNum).HumRat = DataLoopNode::Node(this->CondInletNodeNum).HumRat;
                DataLoopNode::Node(this->CondOutletNodeNum).Enthalpy = DataLoopNode::Node(this->CondInletNodeNum).Enthalpy;
            }

            this->EvapInletTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp;
            this->CondInletTemp = DataLoopNode::Node(this->CondInletNodeNum).Temp;
            this->CondOutletTemp = DataLoopNode::Node(this->CondOutletNodeNum).Temp;
            this->EvapOutletTemp = DataLoopNode::Node(this->EvapOutletNodeNum).Temp;

            if (this->CondenserType == DataPlant::CondenserType::EVAPCOOLED) {
                this->BasinHeaterPower = this->BasinHeaterPower;
                this->BasinHeaterConsumption = this->BasinHeaterPower * ReportingConstant;
            }

            if (this->HeatRecActive) {

                PlantUtilities::SafeCopyPlantNode(this->HeatRecInletNodeNum, this->HeatRecOutletNodeNum);
                this->HeatRecInletTemp = DataLoopNode::Node(this->HeatRecInletNodeNum).Temp;
                this->HeatRecOutletTemp = DataLoopNode::Node(this->HeatRecOutletNodeNum).Temp;
                this->HeatRecMdot = DataLoopNode::Node(this->HeatRecInletNodeNum).MassFlowRate;

                this->ChillerCondAvgTemp = this->AvgCondSinkTemp;
            }

        } else { // Chiller is running, so pass calculated values
            // set node temperatures
            DataLoopNode::Node(this->EvapOutletNodeNum).Temp = this->EvapOutletTemp;
            DataLoopNode::Node(this->CondOutletNodeNum).Temp = this->CondOutletTemp;
            if (this->CondenserType != DataPlant::CondenserType::WATERCOOLED) {
                DataLoopNode::Node(this->CondOutletNodeNum).HumRat = this->CondOutletHumRat;
                DataLoopNode::Node(this->CondOutletNodeNum).Enthalpy = Psychrometrics::PsyHFnTdbW(this->CondOutletTemp, this->CondOutletHumRat);
            }
            // set node flow rates;  for these load based models
            // assume that the sufficient evaporator flow rate available
            this->EvapInletTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp;
            this->CondInletTemp = DataLoopNode::Node(this->CondInletNodeNum).Temp;
            this->CondOutletTemp = DataLoopNode::Node(this->CondOutletNodeNum).Temp;
            this->EvapOutletTemp = DataLoopNode::Node(this->EvapOutletNodeNum).Temp;
            if (this->CondenserType == DataPlant::CondenserType::EVAPCOOLED) {
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

    EngineDrivenChillerSpecs *EngineDrivenChillerSpecs::factory(PlantChillersData &chillers, std::string const &chillerName)
    {
        if (chillers.GetEngineDrivenInput) {
            EngineDrivenChillerSpecs::getInput(chillers);
            chillers.GetEngineDrivenInput = false;
        }
        for (auto &thisChiller : chillers.EngineDrivenChiller) {
            if (UtilityRoutines::MakeUPPERCase(thisChiller.Name) == chillerName) {
                return &thisChiller;
            }
        }
        ShowFatalError("Could not locate engine driven chiller with name: " + chillerName);
        return nullptr;
    }

    void EngineDrivenChillerSpecs::simulate(EnergyPlusData &state, const PlantLocation &calledFromLocation, bool FirstHVACIteration, Real64 &CurLoad, bool RunFlag)
    {
        if (calledFromLocation.loopNum == this->CWLoopNum) { // chilled water loop
            this->initialize(state.dataBranchInputManager, RunFlag, CurLoad);
            auto &sim_component(DataPlant::PlantLoop(this->CWLoopNum).LoopSide(this->CWLoopSideNum).Branch(this->CWBranchNum).Comp(this->CWCompNum));
            this->calculate(CurLoad, RunFlag, sim_component.FlowCtrl);
            this->update(CurLoad, RunFlag);
        } else if (calledFromLocation.loopNum == this->CDLoopNum) { // condenser loop
            PlantUtilities::UpdateChillerComponentCondenserSide(this->CDLoopNum,
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
            PlantUtilities::UpdateComponentHeatRecoverySide(this->HRLoopNum,
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

    void EngineDrivenChillerSpecs::getInput(PlantChillersData &chillers)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Dan Fisher / Brandon Anderson
        //       DATE WRITTEN:    September 2000
        // PURPOSE OF THIS SUBROUTINE:
        // This routine will get the input
        // required by the EngineDriven Chiller model.

        // Locals
        // PARAMETERS
        static std::string const RoutineName("GetEngineDrivenChillerInput: "); // include trailing blank space

        int NumAlphas;  // Number of elements in the alpha array
        int NumNums;    // Number of elements in the numeric array
        int IOStat;     // IO Status when calling get input subroutine
        bool ErrorsFound(false);

        // FLOW
        DataIPShortCuts::cCurrentModuleObject = "Chiller:EngineDriven";
        chillers.NumEngineDrivenChillers = inputProcessor->getNumObjectsFound(DataIPShortCuts::cCurrentModuleObject);

        if (chillers.NumEngineDrivenChillers <= 0) {
            ShowSevereError("No " + DataIPShortCuts::cCurrentModuleObject + " equipment specified in input file");
            ErrorsFound = true;
        }
        // See if load distribution manager has already gotten the input
        if (allocated(chillers.EngineDrivenChiller)) return;

        // ALLOCATE ARRAYS
        chillers.EngineDrivenChiller.allocate(chillers.NumEngineDrivenChillers);

        // LOAD ARRAYS WITH EngineDriven CURVE FIT CHILLER DATA
        for (int ChillerNum = 1; ChillerNum <= chillers.NumEngineDrivenChillers; ++ChillerNum) {
            inputProcessor->getObjectItem(DataIPShortCuts::cCurrentModuleObject,
                                          ChillerNum,
                                          DataIPShortCuts::cAlphaArgs,
                                          NumAlphas,
                                          DataIPShortCuts::rNumericArgs,
                                          NumNums,
                                          IOStat,
                                          DataIPShortCuts::lNumericFieldBlanks,
                                          DataIPShortCuts::lAlphaFieldBlanks,
                                          DataIPShortCuts::cAlphaFieldNames,
                                          DataIPShortCuts::cNumericFieldNames);
            UtilityRoutines::IsNameEmpty(DataIPShortCuts::cAlphaArgs(1), DataIPShortCuts::cCurrentModuleObject, ErrorsFound);

            // ErrorsFound will be set to True if problem was found, left untouched otherwise
            GlobalNames::VerifyUniqueChillerName(DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), ErrorsFound, DataIPShortCuts::cCurrentModuleObject + " Name");

            auto &thisChiller = chillers.EngineDrivenChiller(ChillerNum);
            thisChiller.Name = DataIPShortCuts::cAlphaArgs(1);
            thisChiller.plantTypeOfNum = DataPlant::TypeOf_Chiller_EngineDriven;

            thisChiller.NomCap = DataIPShortCuts::rNumericArgs(1);
            if (thisChiller.NomCap == DataSizing::AutoSize) {
                thisChiller.NomCapWasAutoSized = true;
            }
            if (DataIPShortCuts::rNumericArgs(1) == 0.0) {
                ShowSevereError("Invalid " + DataIPShortCuts::cNumericFieldNames(1) + '=' + General::RoundSigDigits(DataIPShortCuts::rNumericArgs(1), 2));
                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                ErrorsFound = true;
            }

            thisChiller.COP = DataIPShortCuts::rNumericArgs(2);
            if (DataIPShortCuts::rNumericArgs(2) == 0.0) {
                ShowSevereError("Invalid " + DataIPShortCuts::cNumericFieldNames(2) + '=' + General::RoundSigDigits(DataIPShortCuts::rNumericArgs(2), 2));
                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                ErrorsFound = true;
            }

            if (DataIPShortCuts::cAlphaArgs(2) == "AIRCOOLED") {
                thisChiller.CondenserType = DataPlant::CondenserType::AIRCOOLED;
            } else if (DataIPShortCuts::cAlphaArgs(2) == "WATERCOOLED") {
                thisChiller.CondenserType = DataPlant::CondenserType::WATERCOOLED;
            } else if (DataIPShortCuts::cAlphaArgs(2) == "EVAPORATIVELYCOOLED") {
                thisChiller.CondenserType = DataPlant::CondenserType::EVAPCOOLED;
            } else {
                ShowSevereError("Invalid " + DataIPShortCuts::cAlphaFieldNames(2) + '=' + DataIPShortCuts::cAlphaArgs(2));
                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                ErrorsFound = true;
            }

            thisChiller.EvapInletNodeNum = NodeInputManager::GetOnlySingleNode(
                DataIPShortCuts::cAlphaArgs(3), ErrorsFound, DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Inlet, 1, DataLoopNode::ObjectIsNotParent);
            thisChiller.EvapOutletNodeNum = NodeInputManager::GetOnlySingleNode(
                DataIPShortCuts::cAlphaArgs(4), ErrorsFound, DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Outlet, 1, DataLoopNode::ObjectIsNotParent);
            BranchNodeConnections::TestCompSet(DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataIPShortCuts::cAlphaArgs(3), DataIPShortCuts::cAlphaArgs(4), "Chilled Water Nodes");

            if (thisChiller.CondenserType == DataPlant::CondenserType::AIRCOOLED || thisChiller.CondenserType == DataPlant::CondenserType::EVAPCOOLED) {
                // Connection not required for air or evap cooled condenser
                // If the condenser inlet is blank for air cooled and evap cooled condensers then supply a generic name
                //  since it is not used elsewhere for connection
                if (DataIPShortCuts::lAlphaFieldBlanks(5)) {
                    if (len(DataIPShortCuts::cAlphaArgs(1)) < DataGlobals::MaxNameLength - 21) { // protect against long name leading to > 100 chars
                        DataIPShortCuts::cAlphaArgs(5) = DataIPShortCuts::cAlphaArgs(1) + " CONDENSER INLET NODE";
                    } else {
                        DataIPShortCuts::cAlphaArgs(5) = DataIPShortCuts::cAlphaArgs(1).substr(0, 79) + " CONDENSER INLET NODE";
                    }
                }
                if (DataIPShortCuts::lAlphaFieldBlanks(6)) {
                    if (len(DataIPShortCuts::cAlphaArgs(1)) < DataGlobals::MaxNameLength - 22) { // protect against long name leading to > 100 chars
                        DataIPShortCuts::cAlphaArgs(6) = DataIPShortCuts::cAlphaArgs(1) + " CONDENSER OUTLET NODE";
                    } else {
                        DataIPShortCuts::cAlphaArgs(6) = DataIPShortCuts::cAlphaArgs(1).substr(0, 78) + " CONDENSER OUTLET NODE";
                    }
                }

                thisChiller.CondInletNodeNum = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(5),
                                                                                     ErrorsFound,
                                                                                     DataIPShortCuts::cCurrentModuleObject,
                                                                                     DataIPShortCuts::cAlphaArgs(1),
                                                                                     DataLoopNode::NodeType_Air,
                                                                                     DataLoopNode::NodeConnectionType_OutsideAirReference,
                                                                                     2,
                                                                                     DataLoopNode::ObjectIsNotParent);
                bool Okay;
                OutAirNodeManager::CheckAndAddAirNodeNumber(thisChiller.CondInletNodeNum, Okay);
                if (!Okay) {
                    ShowWarningError(DataIPShortCuts::cCurrentModuleObject + ", Adding OutdoorAir:DataLoopNode::Node=" + DataIPShortCuts::cAlphaArgs(5));
                }

                thisChiller.CondOutletNodeNum = NodeInputManager::GetOnlySingleNode(
                    DataIPShortCuts::cAlphaArgs(6), ErrorsFound, DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataLoopNode::NodeType_Air, DataLoopNode::NodeConnectionType_Outlet, 2, DataLoopNode::ObjectIsNotParent);
                // CALL TestCompSet(TRIM(DataIPShortCuts::cCurrentModuleObject),DataIPShortCuts::cAlphaArgs(1),DataIPShortCuts::cAlphaArgs(5),DataIPShortCuts::cAlphaArgs(6),'Condenser (Air) Nodes')
            } else if (thisChiller.CondenserType == DataPlant::CondenserType::WATERCOOLED) {
                thisChiller.CondInletNodeNum = NodeInputManager::GetOnlySingleNode(
                    DataIPShortCuts::cAlphaArgs(5), ErrorsFound, DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Inlet, 2, DataLoopNode::ObjectIsNotParent);
                thisChiller.CondOutletNodeNum = NodeInputManager::GetOnlySingleNode(
                    DataIPShortCuts::cAlphaArgs(6), ErrorsFound, DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Outlet, 2, DataLoopNode::ObjectIsNotParent);
                BranchNodeConnections::TestCompSet(DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataIPShortCuts::cAlphaArgs(5), DataIPShortCuts::cAlphaArgs(6), "Condenser Water Nodes");
                // Condenser Inlet node name is necessary for Water Cooled
                if (DataIPShortCuts::lAlphaFieldBlanks(5)) {
                    ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(5) + " is blank ");
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                    ErrorsFound = true;
                } else if (DataIPShortCuts::lAlphaFieldBlanks(6)) {
                    ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(6) + " is blank ");
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                    ErrorsFound = true;
                }
            } else {
                thisChiller.CondInletNodeNum = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(5),
                                                                                     ErrorsFound,
                                                                                     DataIPShortCuts::cCurrentModuleObject,
                                                                                     DataIPShortCuts::cAlphaArgs(1),
                                                                                     DataLoopNode::NodeType_Unknown,
                                                                                     DataLoopNode::NodeConnectionType_Inlet,
                                                                                     2,
                                                                                     DataLoopNode::ObjectIsNotParent);
                thisChiller.CondOutletNodeNum = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(6),
                                                                                      ErrorsFound,
                                                                                      DataIPShortCuts::cCurrentModuleObject,
                                                                                      DataIPShortCuts::cAlphaArgs(1),
                                                                                      DataLoopNode::NodeType_Unknown,
                                                                                      DataLoopNode::NodeConnectionType_Outlet,
                                                                                      2,
                                                                                      DataLoopNode::ObjectIsNotParent);
                BranchNodeConnections::TestCompSet(DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataIPShortCuts::cAlphaArgs(5), DataIPShortCuts::cAlphaArgs(6), "Condenser (unknown?) Nodes");
                // Condenser Inlet node name is necessary for Water Cooled
                if (DataIPShortCuts::lAlphaFieldBlanks(5)) {
                    ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(5) + " is blank ");
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                    ErrorsFound = true;
                } else if (DataIPShortCuts::lAlphaFieldBlanks(6)) {
                    ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(6) + " is blank ");
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                    ErrorsFound = true;
                }
            }

            thisChiller.MinPartLoadRat = DataIPShortCuts::rNumericArgs(3);
            thisChiller.MaxPartLoadRat = DataIPShortCuts::rNumericArgs(4);
            thisChiller.OptPartLoadRat = DataIPShortCuts::rNumericArgs(5);
            thisChiller.TempDesCondIn = DataIPShortCuts::rNumericArgs(6);
            thisChiller.TempRiseCoef = DataIPShortCuts::rNumericArgs(7);
            thisChiller.TempDesEvapOut = DataIPShortCuts::rNumericArgs(8);
            thisChiller.EvapVolFlowRate = DataIPShortCuts::rNumericArgs(9);
            if (thisChiller.EvapVolFlowRate == DataSizing::AutoSize) {
                thisChiller.EvapVolFlowRateWasAutoSized = true;
            }
            thisChiller.CondVolFlowRate = DataIPShortCuts::rNumericArgs(10);
            if (thisChiller.CondVolFlowRate == DataSizing::AutoSize) {
                if (thisChiller.CondenserType == DataPlant::CondenserType::WATERCOOLED) {
                    thisChiller.CondVolFlowRateWasAutoSized = true;
                }
            }
            thisChiller.CapRatCoef(1) = DataIPShortCuts::rNumericArgs(11);
            thisChiller.CapRatCoef(2) = DataIPShortCuts::rNumericArgs(12);
            thisChiller.CapRatCoef(3) = DataIPShortCuts::rNumericArgs(13);
            if ((DataIPShortCuts::rNumericArgs(11) + DataIPShortCuts::rNumericArgs(12) + DataIPShortCuts::rNumericArgs(13)) == 0.0) {
                ShowSevereError(DataIPShortCuts::cCurrentModuleObject + ": Sum of Capacity Ratio Coef = 0.0, chiller=" + DataIPShortCuts::cAlphaArgs(1));
                ErrorsFound = true;
            }
            thisChiller.PowerRatCoef(1) = DataIPShortCuts::rNumericArgs(14);
            thisChiller.PowerRatCoef(2) = DataIPShortCuts::rNumericArgs(15);
            thisChiller.PowerRatCoef(3) = DataIPShortCuts::rNumericArgs(16);
            thisChiller.FullLoadCoef(1) = DataIPShortCuts::rNumericArgs(17);
            thisChiller.FullLoadCoef(2) = DataIPShortCuts::rNumericArgs(18);
            thisChiller.FullLoadCoef(3) = DataIPShortCuts::rNumericArgs(19);
            thisChiller.TempLowLimitEvapOut = DataIPShortCuts::rNumericArgs(20);

            // Load Special EngineDriven Chiller Curve Fit Inputs
            thisChiller.ClngLoadtoFuelCurve = CurveManager::GetCurveIndex(DataIPShortCuts::cAlphaArgs(7)); // convert curve name to number
            if (thisChiller.ClngLoadtoFuelCurve == 0) {
                ShowSevereError("Invalid " + DataIPShortCuts::cAlphaFieldNames(7) + '=' + DataIPShortCuts::cAlphaArgs(7));
                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                ErrorsFound = true;
            }

            thisChiller.RecJacHeattoFuelCurve = CurveManager::GetCurveIndex(DataIPShortCuts::cAlphaArgs(8)); // convert curve name to number
            if (thisChiller.RecJacHeattoFuelCurve == 0) {
                ShowSevereError("Invalid " + DataIPShortCuts::cAlphaFieldNames(8) + '=' + DataIPShortCuts::cAlphaArgs(8));
                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                ErrorsFound = true;
            }

            thisChiller.RecLubeHeattoFuelCurve = CurveManager::GetCurveIndex(DataIPShortCuts::cAlphaArgs(9)); // convert curve name to number
            if (thisChiller.RecLubeHeattoFuelCurve == 0) {
                ShowSevereError("Invalid " + DataIPShortCuts::cAlphaFieldNames(9) + '=' + DataIPShortCuts::cAlphaArgs(9));
                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                ErrorsFound = true;
            }

            thisChiller.TotExhausttoFuelCurve = CurveManager::GetCurveIndex(DataIPShortCuts::cAlphaArgs(10)); // convert curve name to number
            if (thisChiller.TotExhausttoFuelCurve == 0) {
                ShowSevereError("Invalid " + DataIPShortCuts::cAlphaFieldNames(10) + '=' + DataIPShortCuts::cAlphaArgs(10));
                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                ErrorsFound = true;
            }

            thisChiller.ExhaustTempCurve = CurveManager::GetCurveIndex(DataIPShortCuts::cAlphaArgs(11)); // convert curve name to number
            if (thisChiller.ExhaustTempCurve == 0) {
                ShowSevereError("Invalid " + DataIPShortCuts::cAlphaFieldNames(11) + '=' + DataIPShortCuts::cAlphaArgs(11));
                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                ErrorsFound = true;
            }

            thisChiller.UACoef(1) = DataIPShortCuts::rNumericArgs(21);
            thisChiller.UACoef(2) = DataIPShortCuts::rNumericArgs(22);

            thisChiller.MaxExhaustperPowerOutput = DataIPShortCuts::rNumericArgs(23);
            thisChiller.DesignMinExitGasTemp = DataIPShortCuts::rNumericArgs(24);

            // Validate fuel type input
            bool FuelTypeError(false);
            UtilityRoutines::ValidateFuelType(DataIPShortCuts::cAlphaArgs(12), thisChiller.FuelType, FuelTypeError);
            if (FuelTypeError) {
                ShowSevereError("Invalid " + DataIPShortCuts::cAlphaFieldNames(12) + '=' + DataIPShortCuts::cAlphaArgs(12));
                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                ShowContinueError(
                    "Valid choices are Electricity, NaturalGas, Propane, Diesel, Gasoline, FuelOilNo1, FuelOilNo2,OtherFuel1 or OtherFuel2");
                ErrorsFound = true;
                FuelTypeError = false;
            }

            thisChiller.FuelHeatingValue = DataIPShortCuts::rNumericArgs(25);

            // add support of autosize to this.

            thisChiller.DesignHeatRecVolFlowRate = DataIPShortCuts::rNumericArgs(26);
            if (thisChiller.DesignHeatRecVolFlowRate > 0.0 ||
                thisChiller.DesignHeatRecVolFlowRate == DataSizing::AutoSize) {
                thisChiller.HeatRecActive = true;
                thisChiller.HeatRecInletNodeNum = NodeInputManager::GetOnlySingleNode(
                    DataIPShortCuts::cAlphaArgs(13), ErrorsFound, DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Inlet, 3, DataLoopNode::ObjectIsNotParent);
                if (thisChiller.HeatRecInletNodeNum == 0) {
                    ShowSevereError("Invalid " + DataIPShortCuts::cAlphaFieldNames(13) + '=' + DataIPShortCuts::cAlphaArgs(13));
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                    ErrorsFound = true;
                }
                thisChiller.HeatRecOutletNodeNum = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(14),
                                                                                         ErrorsFound,
                                                                                         DataIPShortCuts::cCurrentModuleObject,
                                                                                         DataIPShortCuts::cAlphaArgs(1),
                                                                                         DataLoopNode::NodeType_Water,
                                                                                         DataLoopNode::NodeConnectionType_Outlet,
                                                                                         3,
                                                                                         DataLoopNode::ObjectIsNotParent);
                if (thisChiller.HeatRecOutletNodeNum == 0) {
                    ShowSevereError("Invalid " + DataIPShortCuts::cAlphaFieldNames(14) + '=' + DataIPShortCuts::cAlphaArgs(14));
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                    ErrorsFound = true;
                }
                BranchNodeConnections::TestCompSet(DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataIPShortCuts::cAlphaArgs(13), DataIPShortCuts::cAlphaArgs(14), "Heat Recovery Nodes");
                if (thisChiller.DesignHeatRecVolFlowRate == DataSizing::AutoSize) {
                    thisChiller.DesignHeatRecVolFlowRateWasAutoSized = true;
                } else {
                    PlantUtilities::RegisterPlantCompDesignFlow(thisChiller.HeatRecInletNodeNum,
                                                thisChiller.DesignHeatRecVolFlowRate);
                }

                // Condenser flow rate must be specified for heat reclaim
                if (thisChiller.CondenserType == DataPlant::CondenserType::AIRCOOLED || thisChiller.CondenserType == DataPlant::CondenserType::EVAPCOOLED) {
                    if (thisChiller.CondVolFlowRate <= 0.0) {
                        ShowSevereError("Invalid " + DataIPShortCuts::cNumericFieldNames(10) + '=' + General::RoundSigDigits(DataIPShortCuts::rNumericArgs(10), 6));
                        ShowSevereError("Condenser fluid flow rate must be specified for Heat Reclaim applications.");
                        ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                        ErrorsFound = true;
                    }
                }

            } else {

                thisChiller.HeatRecActive = false;
                thisChiller.DesignHeatRecMassFlowRate = 0.0;
                thisChiller.HeatRecInletNodeNum = 0;
                thisChiller.HeatRecOutletNodeNum = 0;
                // if heat recovery is not used, don't care about condenser flow rate for air/evap-cooled equip.
                if (thisChiller.CondenserType == DataPlant::CondenserType::AIRCOOLED || thisChiller.CondenserType == DataPlant::CondenserType::EVAPCOOLED) {
                    thisChiller.CondVolFlowRate = 0.0011; // set to avoid errors in calc routine
                }
                if ((!DataIPShortCuts::lAlphaFieldBlanks(13)) || (!DataIPShortCuts::lAlphaFieldBlanks(14))) {
                    ShowWarningError("Since Design Heat Flow Rate = 0.0, Heat Recovery inactive for " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                    ShowContinueError("However, Node names were specified for Heat Recovery inlet or outlet nodes");
                }
            }

            {
                auto const SELECT_CASE_var(DataIPShortCuts::cAlphaArgs(15));
                if (SELECT_CASE_var == "CONSTANTFLOW") {
                    thisChiller.FlowMode = DataPlant::FlowMode::CONSTANT;
                } else if (SELECT_CASE_var == "LEAVINGSETPOINTMODULATED") {
                    thisChiller.FlowMode = DataPlant::FlowMode::LEAVINGSETPOINTMODULATED;
                } else if (SELECT_CASE_var == "NOTMODULATED") {
                    thisChiller.FlowMode = DataPlant::FlowMode::NOTMODULATED;
                } else {
                    ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\",");
                    ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(15) + '=' + DataIPShortCuts::cAlphaArgs(15));
                    ShowContinueError("Available choices are ConstantFlow, NotModulated, or LeavingSetpointModulated");
                    ShowContinueError("Flow mode NotModulated is assumed and the simulation continues.");
                    thisChiller.FlowMode = DataPlant::FlowMode::NOTMODULATED;
                }
            }

            thisChiller.HeatRecMaxTemp = DataIPShortCuts::rNumericArgs(27);
            thisChiller.SizFac = DataIPShortCuts::rNumericArgs(28);
            if (thisChiller.SizFac <= 0.0) thisChiller.SizFac = 1.0;

            //   Basin heater power as a function of temperature must be greater than or equal to 0
            thisChiller.BasinHeaterPowerFTempDiff = DataIPShortCuts::rNumericArgs(29);
            if (DataIPShortCuts::rNumericArgs(29) < 0.0) {
                ShowSevereError(DataIPShortCuts::cCurrentModuleObject + ", \"" + thisChiller.Name +
                                "\" TRIM(DataIPShortCuts::cNumericFieldNames(29)) must be >= 0");
                ErrorsFound = true;
            }

            thisChiller.BasinHeaterSetPointTemp = DataIPShortCuts::rNumericArgs(30);

            if (thisChiller.BasinHeaterPowerFTempDiff > 0.0) {
                if (NumNums < 30) {
                    thisChiller.BasinHeaterSetPointTemp = 2.0;
                }
                if (thisChiller.BasinHeaterSetPointTemp < 2.0) {
                    ShowWarningError(DataIPShortCuts::cCurrentModuleObject + ":\"" + thisChiller.Name + "\", " + DataIPShortCuts::cNumericFieldNames(30) +
                                     " is less than 2 deg C. Freezing could occur.");
                }
            }

            if (!DataIPShortCuts::lAlphaFieldBlanks(16)) {
                thisChiller.BasinHeaterSchedulePtr = ScheduleManager::GetScheduleIndex(DataIPShortCuts::cAlphaArgs(16));
                if (thisChiller.BasinHeaterSchedulePtr == 0) {
                    ShowWarningError(DataIPShortCuts::cCurrentModuleObject + ", \"" + thisChiller.Name + "\" TRIM(DataIPShortCuts::cAlphaFieldNames(16)) \"" +
                                     DataIPShortCuts::cAlphaArgs(16) + "\" was not found. Basin heater operation will not be modeled and the simulation continues");
                }
            }

            if (NumNums > 30) {
                if (!DataIPShortCuts::lNumericFieldBlanks(31)) {
                    thisChiller.HeatRecCapacityFraction = DataIPShortCuts::rNumericArgs(31);
                } else {
                    thisChiller.HeatRecCapacityFraction = 1.0;
                }
            } else {
                thisChiller.HeatRecCapacityFraction = 1.0;
            }
        }

        if (ErrorsFound) {
            ShowFatalError("Errors found in processing input for " + DataIPShortCuts::cCurrentModuleObject);
        }
    }

    void EngineDrivenChillerSpecs::setupOutputVariables()
    {
        SetupOutputVariable("Chiller Drive Shaft Power", OutputProcessor::Unit::W, this->Power, "System", "Average", this->Name);
        SetupOutputVariable("Chiller Drive Shaft Energy", OutputProcessor::Unit::J, this->Energy, "System", "Sum", this->Name);
        SetupOutputVariable("Chiller Evaporator Cooling Rate", OutputProcessor::Unit::W, this->QEvaporator, "System", "Average", this->Name);
        SetupOutputVariable("Chiller Evaporator Cooling Energy",
                            OutputProcessor::Unit::J,
                            this->EvaporatorEnergy,
                            "System",
                            "Sum",
                            this->Name,
                            _,
                            "ENERGYTRANSFER",
                            "CHILLERS",
                            _,
                            "Plant");
        SetupOutputVariable("Chiller Evaporator Inlet Temperature", OutputProcessor::Unit::C, this->EvapInletTemp, "System", "Average", this->Name);
        SetupOutputVariable("Chiller Evaporator Outlet Temperature", OutputProcessor::Unit::C, this->EvapOutletTemp, "System", "Average", this->Name);
        SetupOutputVariable("Chiller Evaporator Mass Flow Rate", OutputProcessor::Unit::kg_s, this->EvapMassFlowRate, "System", "Average", this->Name);
        SetupOutputVariable("Chiller Condenser Heat Transfer Rate", OutputProcessor::Unit::W, this->QCondenser, "System", "Average", this->Name);
        SetupOutputVariable("Chiller Condenser Heat Transfer Energy",
                            OutputProcessor::Unit::J,
                            this->CondenserEnergy,
                            "System",
                            "Sum",
                            this->Name,
                            _,
                            "ENERGYTRANSFER",
                            "HEATREJECTION",
                            _,
                            "Plant");

        SetupOutputVariable("Chiller Condenser Inlet Temperature", OutputProcessor::Unit::C, this->CondInletTemp, "System", "Average", this->Name);

        // Condenser mass flow and outlet temp are valid for Water Cooled
        if (this->CondenserType == DataPlant::CondenserType::WATERCOOLED) {
            SetupOutputVariable("Chiller Condenser Outlet Temperature", OutputProcessor::Unit::C, this->CondOutletTemp, "System", "Average", this->Name);
            SetupOutputVariable("Chiller Condenser Mass Flow Rate", OutputProcessor::Unit::kg_s, this->CondMassFlowRate, "System", "Average", this->Name);
        } else if (this->CondenserType == DataPlant::CondenserType::AIRCOOLED) {
        } else if (this->CondenserType == DataPlant::CondenserType::EVAPCOOLED) {
            if (this->BasinHeaterPowerFTempDiff > 0.0) {
                SetupOutputVariable("Chiller Basin Heater Electricity Rate", OutputProcessor::Unit::W, this->BasinHeaterPower, "System", "Average", this->Name);
                SetupOutputVariable("Chiller Basin Heater Electricity Energy",
                                    OutputProcessor::Unit::J,
                                    this->BasinHeaterConsumption,
                                    "System",
                                    "Sum",
                                    this->Name,
                                    _,
                                    "Electricity",
                                    "CHILLERS",
                                    _,
                                    "Plant");
            }
        }

        SetupOutputVariable("Chiller " + this->FuelType + " Rate", OutputProcessor::Unit::W, this->FuelEnergyUseRate, "System", "Average", this->Name);
        SetupOutputVariable("Chiller " + this->FuelType + " Energy",
                            OutputProcessor::Unit::J,
                            this->FuelEnergy,
                            "System",
                            "Sum",
                            this->Name,
                            _,
                            this->FuelType,
                            "Cooling",
                            _,
                            "Plant");

        SetupOutputVariable("Chiller COP", OutputProcessor::Unit::W_W, this->FuelCOP, "System", "Average", this->Name);
        SetupOutputVariable("Chiller " + this->FuelType + " Mass Flow Rate", OutputProcessor::Unit::kg_s, this->FuelMdot, "System", "Average", this->Name);
        SetupOutputVariable("Chiller Exhaust Temperature", OutputProcessor::Unit::C, this->ExhaustStackTemp, "System", "Average", this->Name);
        SetupOutputVariable("Chiller Heat Recovery Mass Flow Rate", OutputProcessor::Unit::kg_s, this->HeatRecMdot, "System", "Average", this->Name);

        if (this->HeatRecActive) {
            // need to only report if heat recovery active
            SetupOutputVariable("Chiller Jacket Recovered Heat Rate", OutputProcessor::Unit::W, this->QJacketRecovered, "System", "Average", this->Name);
            SetupOutputVariable("Chiller Jacket Recovered Heat Energy",
                                OutputProcessor::Unit::J,
                                this->JacketEnergyRec,
                                "System",
                                "Sum",
                                this->Name,
                                _,
                                "ENERGYTRANSFER",
                                "HEATRECOVERY",
                                _,
                                "Plant");

            SetupOutputVariable("Chiller Lube Recovered Heat Rate", OutputProcessor::Unit::W, this->QLubeOilRecovered, "System", "Average", this->Name);
            SetupOutputVariable("Chiller Lube Recovered Heat Energy",
                                OutputProcessor::Unit::J,
                                this->LubeOilEnergyRec,
                                "System",
                                "Sum",
                                this->Name,
                                _,
                                "ENERGYTRANSFER",
                                "HEATRECOVERY",
                                _,
                                "Plant");

            SetupOutputVariable("Chiller Exhaust Recovered Heat Rate", OutputProcessor::Unit::W, this->QExhaustRecovered, "System", "Average", this->Name);
            SetupOutputVariable("Chiller Exhaust Recovered Heat Energy",
                                OutputProcessor::Unit::J,
                                this->ExhaustEnergyRec,
                                "System",
                                "Sum",
                                this->Name,
                                _,
                                "ENERGYTRANSFER",
                                "HEATRECOVERY",
                                _,
                                "Plant");

            SetupOutputVariable("Chiller Total Recovered Heat Rate", OutputProcessor::Unit::W, this->QTotalHeatRecovered, "System", "Average", this->Name);
            SetupOutputVariable("Chiller Total Recovered Heat Energy", OutputProcessor::Unit::J, this->TotalHeatEnergyRec, "System", "Sum", this->Name);
            SetupOutputVariable("Chiller Heat Recovery Inlet Temperature", OutputProcessor::Unit::C, this->HeatRecInletTemp, "System", "Average", this->Name);
            SetupOutputVariable("Chiller Heat Recovery Outlet Temperature", OutputProcessor::Unit::C, this->HeatRecOutletTemp, "System", "Average", this->Name);
        }
        if (DataGlobals::AnyEnergyManagementSystemInModel) {
            SetupEMSInternalVariable("Chiller Nominal Capacity", this->Name, "[W]", this->NomCap);
        }
    }

    void EngineDrivenChillerSpecs::initialize(BranchInputManagerData &dataBranchInputManager, bool const RunFlag, Real64 const MyLoad)
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
        static std::string const RoutineName("InitEngineDrivenChiller");

        if (this->MyFlag) {
            // Locate the chillers on the plant loops for later usage
            bool errFlag = false;
            this->setupOutputVariables();
            PlantUtilities::ScanPlantLoopsForObject(dataBranchInputManager,
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
            if (this->CondenserType != DataPlant::CondenserType::AIRCOOLED && this->CondenserType != DataPlant::CondenserType::EVAPCOOLED) {
                PlantUtilities::ScanPlantLoopsForObject(dataBranchInputManager,
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
                    this->CWLoopNum, this->CWLoopSideNum, this->CDLoopNum, this->CDLoopSideNum, this->plantTypeOfNum, true);
            }
            if (this->HeatRecActive) {
                PlantUtilities::ScanPlantLoopsForObject(dataBranchInputManager,
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
                    this->CWLoopNum, this->CWLoopSideNum, this->HRLoopNum, this->HRLoopSideNum, this->plantTypeOfNum, true);
            }

            if (this->CondenserType != DataPlant::CondenserType::AIRCOOLED && this->CondenserType != DataPlant::CondenserType::EVAPCOOLED && this->HeatRecActive) {
                PlantUtilities::InterConnectTwoPlantLoopSides(
                    this->CDLoopNum, this->CDLoopSideNum, this->HRLoopNum, this->HRLoopSideNum, this->plantTypeOfNum, false);
            }
            if (errFlag) {
                ShowFatalError("InitEngineDrivenChiller: Program terminated due to previous condition(s).");
            }

            if (this->FlowMode == DataPlant::FlowMode::CONSTANT) {
                // reset flow priority
                DataPlant::PlantLoop(this->CWLoopNum).LoopSide(this->CWLoopSideNum).Branch(this->CWBranchNum).Comp(this->CWCompNum).FlowPriority =
                    DataPlant::LoopFlowStatus_NeedyIfLoopOn;
            }

            if (this->FlowMode == DataPlant::FlowMode::LEAVINGSETPOINTMODULATED) {
                // reset flow priority
                DataPlant::PlantLoop(this->CWLoopNum).LoopSide(this->CWLoopSideNum).Branch(this->CWBranchNum).Comp(this->CWCompNum).FlowPriority =
                    DataPlant::LoopFlowStatus_NeedyIfLoopOn;
                // check if setpoint on outlet node
                if ((DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPoint == DataLoopNode::SensedNodeFlagValue) &&
                    (DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPointHi == DataLoopNode::SensedNodeFlagValue)) {
                    if (!DataGlobals::AnyEnergyManagementSystemInModel) {
                        if (!this->ModulatedFlowErrDone) {
                            ShowWarningError("Missing temperature setpoint for LeavingSetpointModulated mode chiller named " + this->Name);
                            ShowContinueError(
                                "  A temperature setpoint is needed at the outlet node of a chiller in variable flow mode, use a SetpointManager");
                            ShowContinueError("  The overall loop setpoint will be assumed for chiller. The simulation continues ... ");
                            this->ModulatedFlowErrDone = true;
                        }
                    } else {
                        // need call to EMS to check node
                        bool FatalError = false; // but not really fatal yet, but should be.
                        EMSManager::CheckIfNodeSetPointManagedByEMS(this->EvapOutletNodeNum, EMSManager::iTemperatureSetPoint, FatalError);
                        if (FatalError) {
                            if (!this->ModulatedFlowErrDone) {
                                ShowWarningError("Missing temperature setpoint for LeavingSetpointModulated mode chiller named " + this->Name);
                                ShowContinueError(
                                    "  A temperature setpoint is needed at the outlet node of a chiller evaporator in variable flow mode");
                                ShowContinueError("  use a Setpoint Manager to establish a setpoint at the chiller evaporator outlet node ");
                                ShowContinueError("  or use an EMS actuator to establish a setpoint at the outlet node ");
                                ShowContinueError("  The overall loop setpoint will be assumed for chiller. The simulation continues ... ");
                                this->ModulatedFlowErrDone = true;
                            }
                        }
                    }
                    this->ModulatedFlowSetToLoop = true;
                    DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPoint = DataLoopNode::Node(DataPlant::PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPoint;
                    DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPointHi = DataLoopNode::Node(DataPlant::PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPointHi;
                }
            }

            this->MyFlag = false;
        }

        // Initialize critical Demand Side Variables
        if (this->MyEnvrnFlag && DataGlobals::BeginEnvrnFlag && (DataPlant::PlantFirstSizesOkayToFinalize)) {

            Real64 rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->CWLoopNum).FluidName,
                                                    DataGlobals::CWInitConvTemp,
                                                    DataPlant::PlantLoop(this->CWLoopNum).FluidIndex,
                                                    RoutineName);

            this->EvapMassFlowRateMax = rho * this->EvapVolFlowRate;
            PlantUtilities::InitComponentNodes(0.0,
                                               this->EvapMassFlowRateMax,
                                               this->EvapInletNodeNum,
                                               this->EvapOutletNodeNum,
                                               this->CWLoopNum,
                                               this->CWLoopSideNum,
                                               this->CWBranchNum,
                                               this->CWCompNum);

            // init maximum available condenser flow rate

            if (this->CondenserType == DataPlant::CondenserType::WATERCOOLED) {

                DataLoopNode::Node(this->CondInletNodeNum).Temp = this->TempDesCondIn;

                rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->CDLoopNum).FluidName,
                                                        DataGlobals::CWInitConvTemp,
                                                        DataPlant::PlantLoop(this->CDLoopNum).FluidIndex,
                                                        RoutineName);

                this->CondMassFlowRateMax = rho * this->CondVolFlowRate;

                PlantUtilities::InitComponentNodes(0.0,
                                                   this->CondMassFlowRateMax,
                                                   this->CondInletNodeNum,
                                                   this->CondOutletNodeNum,
                                                   this->CDLoopNum,
                                                   this->CDLoopSideNum,
                                                   this->CDBranchNum,
                                                   this->CDCompNum);
            } else { // air or evap-air
                DataLoopNode::Node(this->CondInletNodeNum).MassFlowRate =
                    this->CondVolFlowRate * Psychrometrics::PsyRhoAirFnPbTdbW(DataEnvironment::StdBaroPress, this->TempDesCondIn, 0.0, RoutineName);

                DataLoopNode::Node(this->CondOutletNodeNum).MassFlowRate = DataLoopNode::Node(this->CondInletNodeNum).MassFlowRate;
                DataLoopNode::Node(this->CondInletNodeNum).MassFlowRateMaxAvail = DataLoopNode::Node(this->CondInletNodeNum).MassFlowRate;
                DataLoopNode::Node(this->CondInletNodeNum).MassFlowRateMax = DataLoopNode::Node(this->CondInletNodeNum).MassFlowRate;
                DataLoopNode::Node(this->CondOutletNodeNum).MassFlowRateMax = DataLoopNode::Node(this->CondInletNodeNum).MassFlowRate;
                DataLoopNode::Node(this->CondInletNodeNum).MassFlowRateMinAvail = 0.0;
                DataLoopNode::Node(this->CondInletNodeNum).MassFlowRateMin = 0.0;
                DataLoopNode::Node(this->CondOutletNodeNum).MassFlowRateMinAvail = 0.0;
                DataLoopNode::Node(this->CondOutletNodeNum).MassFlowRateMin = 0.0;
            }

            if (this->HeatRecActive) {
                rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->HRLoopNum).FluidName,
                                                        DataGlobals::HWInitConvTemp,
                                                        DataPlant::PlantLoop(this->HRLoopNum).FluidIndex,
                                                        RoutineName);
                this->DesignHeatRecMassFlowRate = rho * this->DesignHeatRecVolFlowRate;

                PlantUtilities::InitComponentNodes(0.0,
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

        if (!DataGlobals::BeginEnvrnFlag) {
            this->MyEnvrnFlag = true;
        }

        if ((this->FlowMode == DataPlant::FlowMode::LEAVINGSETPOINTMODULATED) && (this->ModulatedFlowSetToLoop)) {
            // fix for clumsy old input that worked because loop setpoint was spread.
            //  could be removed with transition, testing , model change, period of being obsolete.
            DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPoint = DataLoopNode::Node(DataPlant::PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPoint;
            DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPointHi = DataLoopNode::Node(DataPlant::PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPointHi;
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
            mdot, this->EvapInletNodeNum, this->EvapOutletNodeNum, this->CWLoopNum, this->CWLoopSideNum, this->CWBranchNum, this->CWCompNum);
        if (this->CondenserType == DataPlant::CondenserType::WATERCOOLED) {
            PlantUtilities::SetComponentFlowRate(
                mdotCond, this->CondInletNodeNum, this->CondOutletNodeNum, this->CDLoopNum, this->CDLoopSideNum, this->CDBranchNum, this->CDCompNum);
        }

        // Initialize heat recovery flow rates at node
        if (this->HeatRecActive) {

            if (RunFlag) {
                mdot = this->DesignHeatRecMassFlowRate;
            } else {
                mdot = 0.0;
            }

            PlantUtilities::SetComponentFlowRate(mdot, this->HeatRecInletNodeNum, this->HeatRecOutletNodeNum, this->HRLoopNum, this->HRLoopSideNum, this->HRBranchNum, this->HRCompNum);
        }
        if (this->CondenserType == DataPlant::CondenserType::EVAPCOOLED) {
            this->BasinHeaterPower = 0.0;
        }
    }

    void EngineDrivenChillerSpecs::size()
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
        static std::string const RoutineName("SizeEngineDrivenChiller");

        int PltSizCondNum = 0;
        bool ErrorsFound = false;
        Real64 tmpNomCap = this->NomCap;
        Real64 tmpEvapVolFlowRate = this->EvapVolFlowRate;
        Real64 tmpCondVolFlowRate = this->CondVolFlowRate;

        if (this->CondenserType == DataPlant::CondenserType::WATERCOOLED) {
            PltSizCondNum = DataPlant::PlantLoop(this->CDLoopNum).PlantSizNum;
        }

        int PltSizNum = DataPlant::PlantLoop(this->CWLoopNum).PlantSizNum;

        if (PltSizNum > 0) {
            if (DataSizing::PlantSizData(PltSizNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                Real64 rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->CWLoopNum).FluidName,
                                                        DataGlobals::CWInitConvTemp,
                                                        DataPlant::PlantLoop(this->CWLoopNum).FluidIndex,
                                                        RoutineName);
                Real64 Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(this->CWLoopNum).FluidName,
                                                            DataGlobals::CWInitConvTemp,
                                                            DataPlant::PlantLoop(this->CWLoopNum).FluidIndex,
                                                            RoutineName);
                tmpNomCap = Cp * rho * DataSizing::PlantSizData(PltSizNum).DeltaT * DataSizing::PlantSizData(PltSizNum).DesVolFlowRate * this->SizFac;
            } else {
                if (this->NomCapWasAutoSized) tmpNomCap = 0.0;
            }
            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                if (this->NomCapWasAutoSized) {
                    this->NomCap = tmpNomCap;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput("Chiller:EngineDriven", this->Name, "Design Size Nominal Capacity [W]", tmpNomCap);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(
                            "Chiller:EngineDriven", this->Name, "Initial Design Size Nominal Capacity [W]", tmpNomCap);
                    }
                } else {
                    if (this->NomCap > 0.0 && tmpNomCap > 0.0) {
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput("Chiller:EngineDriven",
                                                                    this->Name,
                                                                    "Design Size Nominal Capacity [W]",
                                                                    tmpNomCap,
                                                                    "User-Specified Nominal Capacity [W]",
                                                                    this->NomCap);
                            if (DataGlobals::DisplayExtraWarnings) {
                                if ((std::abs(tmpNomCap - this->NomCap) / this->NomCap) > DataSizing::AutoVsHardSizingThreshold) {
                                    ShowMessage("SizeChillerEngineDriven: Potential issue with equipment sizing for " + this->Name);
                                    ShowContinueError("User-Specified Nominal Capacity of " + General::RoundSigDigits(this->NomCap, 2) + " [W]");
                                    ShowContinueError("differs from Design Size Nominal Capacity of " + General::RoundSigDigits(tmpNomCap, 2) + " [W]");
                                    ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                        }
                        tmpNomCap = this->NomCap;
                    }
                }
            }
        } else {
            if (this->NomCapWasAutoSized && DataPlant::PlantFirstSizesOkayToFinalize) {
                ShowSevereError("Autosizing of Engine Driven Chiller nominal capacity requires a loop Sizing:Plant object");
                ShowContinueError("Occurs in Engine Driven Chiller object=" + this->Name);
                ErrorsFound = true;
            }
            if (!this->NomCapWasAutoSized && DataPlant::PlantFinalSizesOkayToReport && (this->NomCap > 0.0)) {
                ReportSizingManager::ReportSizingOutput("Chiller:EngineDriven", this->Name, "User-Specified Nominal Capacity [W]", this->NomCap);
            }
        }

        if (PltSizNum > 0) {
            if (DataSizing::PlantSizData(PltSizNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                tmpEvapVolFlowRate = DataSizing::PlantSizData(PltSizNum).DesVolFlowRate * this->SizFac;
            } else {
                if (this->EvapVolFlowRateWasAutoSized) tmpEvapVolFlowRate = 0.0;
            }
            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                if (this->EvapVolFlowRateWasAutoSized) {
                    this->EvapVolFlowRate = tmpEvapVolFlowRate;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(
                            "Chiller:EngineDriven", this->Name, "Design Chilled Water Flow Rate [m3/s]", tmpEvapVolFlowRate);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(
                            "Chiller:EngineDriven", this->Name, "Initial Design Chilled Water Flow Rate [m3/s]", tmpEvapVolFlowRate);
                    }
                } else {
                    if (this->EvapVolFlowRate > 0.0 && tmpEvapVolFlowRate > 0.0) {
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput("Chiller:EngineDriven",
                                                                    this->Name,
                                                                    "Design Size Design Chilled Water Flow Rate [m3/s]",
                                                                    tmpEvapVolFlowRate,
                                                                    "User-Specified Design Chilled Water Flow Rate [m3/s]",
                                                                    this->EvapVolFlowRate);
                            if (DataGlobals::DisplayExtraWarnings) {
                                if ((std::abs(tmpEvapVolFlowRate - this->EvapVolFlowRate) / this->EvapVolFlowRate) >
                                    DataSizing::AutoVsHardSizingThreshold) {
                                    ShowMessage("SizeChillerEngineDriven: Potential issue with equipment sizing for " + this->Name);
                                    ShowContinueError("User-Specified Design Chilled Water Flow Rate of " + General::RoundSigDigits(this->EvapVolFlowRate, 5) +
                                                      " [m3/s]");
                                    ShowContinueError("differs from Design Size Design Chilled Water Flow Rate of " +
                                                      General::RoundSigDigits(tmpEvapVolFlowRate, 5) + " [m3/s]");
                                    ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                        }
                        tmpEvapVolFlowRate = this->EvapVolFlowRate;
                    }
                }
            }
        } else {
            if (this->EvapVolFlowRateWasAutoSized && DataPlant::PlantFirstSizesOkayToFinalize) {
                ShowSevereError("Autosizing of Engine Driven Chiller evap flow rate requires a loop Sizing:Plant object");
                ShowContinueError("Occurs in Engine Driven Chiller object=" + this->Name);
                ErrorsFound = true;
            }
            if (!this->EvapVolFlowRateWasAutoSized && DataPlant::PlantFinalSizesOkayToReport && (this->EvapVolFlowRate > 0.0)) {
                ReportSizingManager::ReportSizingOutput(
                    "Chiller:EngineDriven", this->Name, "User-Specified Design Chilled Water Flow Rate [m3/s]", this->EvapVolFlowRate);
            }
        }

        PlantUtilities::RegisterPlantCompDesignFlow(this->EvapInletNodeNum, tmpEvapVolFlowRate);

        if (PltSizCondNum > 0 && PltSizNum > 0) {
            if (DataSizing::PlantSizData(PltSizNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow && tmpNomCap > 0.0) {
                Real64 rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->CDLoopNum).FluidName,
                                                        this->TempDesCondIn,
                                                        DataPlant::PlantLoop(this->CDLoopNum).FluidIndex,
                                                        RoutineName);

                Real64 Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(this->CDLoopNum).FluidName,
                                                            this->TempDesCondIn,
                                                            DataPlant::PlantLoop(this->CDLoopNum).FluidIndex,
                                                            RoutineName);
                tmpCondVolFlowRate = tmpNomCap * (1.0 + 1.0 / this->COP) / (DataSizing::PlantSizData(PltSizCondNum).DeltaT * Cp * rho);
            } else {
                if (this->CondVolFlowRateWasAutoSized) tmpCondVolFlowRate = 0.0;
            }
            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                if (this->CondVolFlowRateWasAutoSized) {
                    this->CondVolFlowRate = tmpCondVolFlowRate;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(
                            "Chiller:EngineDriven", this->Name, "Design Size Design Condenser Water Flow Rate [m3/s]", tmpCondVolFlowRate);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(
                            "Chiller:EngineDriven", this->Name, "Initial Design Size Design Condenser Water Flow Rate [m3/s]", tmpCondVolFlowRate);
                    }
                } else {
                    if (this->CondVolFlowRate > 0.0 && tmpCondVolFlowRate > 0.0) {
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput("Chiller:EngineDriven",
                                                                    this->Name,
                                                                    "Design Size Design Condenser Water Flow Rate [m3/s]",
                                                                    tmpCondVolFlowRate,
                                                                    "User-Specified Design Condenser Water Flow Rate [m3/s]",
                                                                    this->CondVolFlowRate);
                            if (DataGlobals::DisplayExtraWarnings) {
                                if ((std::abs(tmpCondVolFlowRate - this->CondVolFlowRate) / this->CondVolFlowRate) >
                                    DataSizing::AutoVsHardSizingThreshold) {
                                    ShowMessage("SizeChillerEngineDriven: Potential issue with equipment sizing for " + this->Name);
                                    ShowContinueError("User-Specified Design Condenser Water Flow Rate of " + General::RoundSigDigits(this->CondVolFlowRate, 5) +
                                                      " [m3/s]");
                                    ShowContinueError("differs from Design Size Design Condenser Water Flow Rate of " +
                                                      General::RoundSigDigits(tmpCondVolFlowRate, 5) + " [m3/s]");
                                    ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                        }
                        tmpCondVolFlowRate = this->CondVolFlowRate;
                    }
                }
            }
        } else {
            if (this->CondVolFlowRateWasAutoSized && DataPlant::PlantFirstSizesOkayToFinalize) {
                ShowSevereError("Autosizing of EngineDriven Chiller condenser flow rate requires a condenser");
                ShowContinueError("loop Sizing:Plant object");
                ShowContinueError("Occurs in EngineDriven Chiller object=" + this->Name);
                ErrorsFound = true;
            }
            if (!this->CondVolFlowRateWasAutoSized && DataPlant::PlantFinalSizesOkayToReport && (this->CondVolFlowRate > 0.0)) {
                ReportSizingManager::ReportSizingOutput(
                    "Chiller:EngineDriven", this->Name, "User-Specified Design Condenser Water Flow Rate [m3/s]", this->CondVolFlowRate);
            }
        }

        // save the design condenser water volumetric flow rate for use by the condenser water loop sizing algorithms
        if (this->CondenserType == DataPlant::CondenserType::WATERCOOLED) {
            PlantUtilities::RegisterPlantCompDesignFlow(this->CondInletNodeNum, tmpCondVolFlowRate);
        }

        // autosize support for heat recovery flow rate.
        if (this->HeatRecActive) {
            Real64 tmpHeatRecVolFlowRate = tmpCondVolFlowRate * this->HeatRecCapacityFraction;
            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                if (this->DesignHeatRecVolFlowRateWasAutoSized) {
                    this->DesignHeatRecVolFlowRate = tmpHeatRecVolFlowRate;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(
                            "Chiller:EngineDriven", this->Name, "Design Size Design Heat Recovery Fluid Flow Rate [m3/s]", tmpHeatRecVolFlowRate);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput("Chiller:EngineDriven",
                                                                this->Name,
                                                                "Initial Design Size Design Heat Recovery Fluid Flow Rate [m3/s]",
                                                                tmpHeatRecVolFlowRate);
                    }
                } else {
                    if (this->DesignHeatRecVolFlowRate > 0.0 && tmpHeatRecVolFlowRate > 0.0) {
                        Real64 DesignHeatRecVolFlowRateUser = this->DesignHeatRecVolFlowRate;
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            if (DataGlobals::DoPlantSizing) {
                                ReportSizingManager::ReportSizingOutput("Chiller:EngineDriven",
                                                                        this->Name,
                                                                        "Design Size Design Heat Recovery Fluid Flow Rate [m3/s]",
                                                                        tmpHeatRecVolFlowRate,
                                                                        "User-Specified Design Heat Recovery Fluid Flow Rate [m3/s]",
                                                                        DesignHeatRecVolFlowRateUser);
                            } else {
                                ReportSizingManager::ReportSizingOutput("Chiller:EngineDriven",
                                                                        this->Name,
                                                                        "User-Specified Design Heat Recovery Fluid Flow Rate [m3/s]",
                                                                        DesignHeatRecVolFlowRateUser);
                            }
                            if (DataGlobals::DisplayExtraWarnings) {
                                if ((std::abs(tmpHeatRecVolFlowRate - DesignHeatRecVolFlowRateUser) / DesignHeatRecVolFlowRateUser) >
                                    DataSizing::AutoVsHardSizingThreshold) {
                                    ShowMessage("SizeEngineDrivenChiller: Potential issue with equipment sizing for " + this->Name);
                                    ShowContinueError("User-Specified Design Heat Recovery Fluid Flow Rate of " +
                                                      General::RoundSigDigits(DesignHeatRecVolFlowRateUser, 5) + " [m3/s]");
                                    ShowContinueError("differs from Design Size Design Heat Recovery Fluid Flow Rate of " +
                                                      General::RoundSigDigits(tmpHeatRecVolFlowRate, 5) + " [m3/s]");
                                    ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                        }
                        tmpHeatRecVolFlowRate = DesignHeatRecVolFlowRateUser;
                    }
                }
            }
            if (!this->DesignHeatRecVolFlowRateWasAutoSized) tmpHeatRecVolFlowRate = this->DesignHeatRecVolFlowRate;
            // save the reference heat recovery fluid volumetric flow rate
            PlantUtilities::RegisterPlantCompDesignFlow(this->HeatRecInletNodeNum, tmpHeatRecVolFlowRate);
        }

        if (DataPlant::PlantFinalSizesOkayToReport) {
            OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchMechType, this->Name, "Chiller:EngineDriven");
            OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchMechNomEff, this->Name, this->COP);
            OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchMechNomCap, this->Name, this->NomCap);
        }

        if (ErrorsFound) {
            ShowFatalError("Preceding sizing errors cause program termination");
        }
    }

    void EngineDrivenChillerSpecs::calculate(Real64 &MyLoad, bool const RunFlag, int const EquipFlowCtrl)
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
        static std::string const RoutineName("CalcEngineDrivenChillerModel");

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
            this->HeatRecInletTemp = DataLoopNode::Node(this->HeatRecInletNodeNum).Temp;
            this->HeatRecOutletTemp = DataLoopNode::Node(this->HeatRecInletNodeNum).Temp;
        }

        //   calculate end time of current time step
        Real64 CurrentEndTime = DataGlobals::CurrentTime + DataHVACGlobals::SysTimeElapsed;

        //   Print warning messages only when valid and only for the first ocurrance. Let summary provide statistics.
        //   Wait for next time step to print warnings. If simulation iterates, print out
        //   the warning for the last iteration only. Must wait for next time step to accomplish this.
        //   If a warning occurs and the simulation down shifts, the warning is not valid.
        if (CurrentEndTime > this->CurrentEndTimeLast && DataHVACGlobals::TimeStepSys >= this->TimeStepSysLast) {
            if (this->PrintMessage) {
                ++this->MsgErrorCount;
                //     Show single warning and pass additional info to ShowRecurringWarningErrorAtEnd
                if (this->MsgErrorCount < 2) {
                    ShowWarningError(this->MsgBuffer1 + '.');
                    ShowContinueError(this->MsgBuffer2);
                } else {
                    ShowRecurringWarningErrorAtEnd(
                        this->MsgBuffer1 + " error continues.", this->ErrCount1, this->MsgDataLast, this->MsgDataLast, _, "[C]", "[C]");
                }
            }
        }

        // save last system time step and last end time of current time step (used to determine if warning is valid)
        this->TimeStepSysLast = DataHVACGlobals::TimeStepSys;
        this->CurrentEndTimeLast = CurrentEndTime;

        // If Chiller load is 0 or chiller is not running then leave the subroutine.
        if (MyLoad >= 0.0 || !RunFlag) {
            if (EquipFlowCtrl == DataBranchAirLoopPlant::ControlType_SeriesActive ||
                DataPlant::PlantLoop(this->CWLoopNum).LoopSide(this->CWLoopSideNum).FlowLock == 1) {
                this->EvapMassFlowRate = DataLoopNode::Node(this->EvapInletNodeNum).MassFlowRate;
            } else {
                this->EvapMassFlowRate = 0.0;

                PlantUtilities::SetComponentFlowRate(
                    this->EvapMassFlowRate, this->EvapInletNodeNum, this->EvapOutletNodeNum, this->CWLoopNum, this->CWLoopSideNum, this->CWBranchNum, this->CWCompNum);
            }

            if (this->CondenserType == DataPlant::CondenserType::WATERCOOLED) {
                if (DataPlant::PlantLoop(this->CDLoopNum).LoopSide(this->CDLoopSideNum).Branch(this->CDBranchNum).Comp(this->CDCompNum).FlowCtrl ==
                    DataBranchAirLoopPlant::ControlType_SeriesActive) {
                    this->CondMassFlowRate = DataLoopNode::Node(this->CondInletNodeNum).MassFlowRate;
                } else {
                    this->CondMassFlowRate = 0.0;
                    PlantUtilities::SetComponentFlowRate(this->CondMassFlowRate,
                                                         this->CondInletNodeNum,
                                                         this->CondOutletNodeNum,
                                                         this->CDLoopNum,
                                                         this->CDLoopSideNum,
                                                         this->CDBranchNum,
                                                         this->CDCompNum);
                }
            }

            if (this->CondenserType == DataPlant::CondenserType::EVAPCOOLED) {
                CalcBasinHeaterPower(
                    this->BasinHeaterPowerFTempDiff, this->BasinHeaterSchedulePtr, this->BasinHeaterSetPointTemp, this->BasinHeaterPower);
            }
            this->PrintMessage = false;
            return;
        }

        if (this->CondenserType == DataPlant::CondenserType::AIRCOOLED) { // Condenser inlet temp = outdoor temp
            DataLoopNode::Node(this->CondInletNodeNum).Temp = DataLoopNode::Node(this->CondInletNodeNum).OutAirDryBulb;
            //  Warn user if entering condenser temperature falls below 0C
            if (DataLoopNode::Node(this->CondInletNodeNum).Temp < 0.0 && !DataGlobals::WarmupFlag) {
                this->PrintMessage = true;
                this->MsgBuffer1 =
                    format("CalcEngineDrivenChillerModel - Chiller:EngineDriven \"{}\" - Air Cooled Condenser Inlet Temperature below 0C", this->Name);
                this->MsgBuffer2 = format("... Outdoor Dry-bulb Condition = {:6.2F} C. Occurrence info = {}, {} {}",
                                          DataLoopNode::Node(this->CondInletNodeNum).Temp,
                                          DataEnvironment::EnvironmentName,
                                          DataEnvironment::CurMnDy,
                                          General::CreateSysTimeIntervalString());
                this->MsgDataLast = DataLoopNode::Node(this->CondInletNodeNum).Temp;
            } else {
                this->PrintMessage = false;
            }
        } else if (this->CondenserType == DataPlant::CondenserType::EVAPCOOLED) { // Condenser inlet temp = (outdoor wet bulb)
            DataLoopNode::Node(this->CondInletNodeNum).Temp = DataLoopNode::Node(this->CondInletNodeNum).OutAirWetBulb;
            //  Warn user if evap condenser wet bulb temperature falls below 10C
            if (DataLoopNode::Node(this->CondInletNodeNum).Temp < 10.0 && !DataGlobals::WarmupFlag) {
                this->PrintMessage = true;
                this->MsgBuffer1 =
                    format("CalcEngineDrivenChillerModel - Chiller:EngineDriven \"{}\" - Evap Cooled Condenser Inlet Temperature below 10C", this->Name);
                this->MsgBuffer2 = format("... Outdoor Wet-bulb Condition = {:6.2F} C. Occurrence info = {}, {} {}",
                                          DataLoopNode::Node(this->CondInletNodeNum).Temp,
                                          DataEnvironment::EnvironmentName,
                                          DataEnvironment::CurMnDy,
                                          General::CreateSysTimeIntervalString());
                this->MsgDataLast = DataLoopNode::Node(this->CondInletNodeNum).Temp;
            } else {
                this->PrintMessage = false;
            }
        } // End of the Air Cooled/Evap Cooled Logic block

        // If not air or evap cooled then set to the condenser node that is attached to a cooling tower
        this->CondInletTemp = DataLoopNode::Node(this->CondInletNodeNum).Temp;

        // Set mass flow rates
        if (this->CondenserType == DataPlant::CondenserType::WATERCOOLED) {
            this->CondMassFlowRate = this->CondMassFlowRateMax;
            PlantUtilities::SetComponentFlowRate(
                this->CondMassFlowRate, this->CondInletNodeNum, this->CondOutletNodeNum, this->CDLoopNum, this->CDLoopSideNum, this->CDBranchNum, this->CDCompNum);
            PlantUtilities::PullCompInterconnectTrigger(this->CWLoopNum,
                                                        this->CWLoopSideNum,
                                                        this->CWBranchNum,
                                                        this->CWCompNum,
                                                        this->CondMassFlowIndex,
                                                        this->CDLoopNum,
                                                        this->CDLoopSideNum,
                                                        DataPlant::CriteriaType_MassFlowRate,
                                                        this->CondMassFlowRate);
            if (this->CondMassFlowRate < DataBranchAirLoopPlant::MassFlowTolerance) return;
        }

        //  LOAD LOCAL VARIABLES FROM DATA STRUCTURE (for code readability)
        Real64 ChillerNomCap = this->NomCap;
        Real64 COPLocal = this->COP;
        Real64 TempCondIn = DataLoopNode::Node(this->CondInletNodeNum).Temp;
        Real64 TempEvapOut = DataLoopNode::Node(this->EvapOutletNodeNum).Temp;

        // If there is a fault of chiller fouling
        if (this->FaultyChillerFoulingFlag && (!DataGlobals::WarmupFlag) && (!DataGlobals::DoingSizing) && (!DataGlobals::KickOffSimulation)) {
            int FaultIndex = this->FaultyChillerFoulingIndex;
            Real64 NomCap_ff = ChillerNomCap;
            Real64 COP_ff = COPLocal;

            // calculate the Faulty Chiller Fouling Factor using fault information
            this->FaultyChillerFoulingFactor = FaultsManager::FaultsChillerFouling(FaultIndex).CalFoulingFactor();

            // update the Chiller nominal capacity and COP at faulty cases
            ChillerNomCap = NomCap_ff * this->FaultyChillerFoulingFactor;
            COPLocal = COP_ff * this->FaultyChillerFoulingFactor;
        }

        // If there is a fault of Chiller SWT Sensor (zrp_Jun2016)
        if (this->FaultyChillerSWTFlag && (!DataGlobals::WarmupFlag) && (!DataGlobals::DoingSizing) && (!DataGlobals::KickOffSimulation)) {
            int FaultIndex = this->FaultyChillerSWTIndex;
            Real64 EvapOutletTemp_ff = TempEvapOut;

            // calculate the sensor offset using fault information
            this->FaultyChillerSWTOffset = FaultsManager::FaultsChillerSWTSensor(FaultIndex).CalFaultOffsetAct();
            // update the TempEvapOut
            TempEvapOut = max(this->TempLowLimitEvapOut, min(DataLoopNode::Node(this->EvapInletNodeNum).Temp, EvapOutletTemp_ff - this->FaultyChillerSWTOffset));
            this->FaultyChillerSWTOffset = EvapOutletTemp_ff - TempEvapOut;
        }

        // Calculate chiller performance from this set of performance equations.
        Real64 DeltaTemp = (TempCondIn - this->TempDesCondIn) / this->TempRiseCoef - (TempEvapOut - this->TempDesEvapOut);

        // available nominal capacity ratio
        Real64 AvailNomCapRat = this->CapRatCoef(1) + this->CapRatCoef(2) * DeltaTemp + this->CapRatCoef(3) * pow_2(DeltaTemp);

        Real64 AvailChillerCap = ChillerNomCap * AvailNomCapRat;

        // full load power ratio
        Real64 FullLoadPowerRat = this->PowerRatCoef(1) + this->PowerRatCoef(2) * AvailNomCapRat + this->PowerRatCoef(3) * pow_2(AvailNomCapRat);

        Real64 PartLoadRat(0.0);         // part load ratio for efficiency
        if (AvailChillerCap > 0.0) {
            PartLoadRat = max(this->MinPartLoadRat, min(std::abs(MyLoad) / AvailChillerCap, this->MaxPartLoadRat));
        }

        Real64 FracFullLoadPower = this->FullLoadCoef(1) + this->FullLoadCoef(2) * PartLoadRat + this->FullLoadCoef(3) * pow_2(PartLoadRat);

        Real64 OperPartLoadRat;          // Actual operating PLR
        if (AvailChillerCap > 0.0) {
            if (std::abs(MyLoad) / AvailChillerCap < this->MinPartLoadRat) {
                OperPartLoadRat = std::abs(MyLoad) / AvailChillerCap;
            } else {
                OperPartLoadRat = PartLoadRat;
            }
        } else {
            OperPartLoadRat = 0.0;
        }

        Real64 Cp = FluidProperties::GetSpecificHeatGlycol(
            DataPlant::PlantLoop(this->CWLoopNum).FluidName, DataLoopNode::Node(this->EvapInletNodeNum).Temp, DataPlant::PlantLoop(this->CWLoopNum).FluidIndex, RoutineName);

        // If FlowLock is True, the new resolved mdot is used to update Power, QEvap, Qcond, and
        // condenser side outlet temperature.
        if (DataPlant::PlantLoop(this->CWLoopNum).LoopSide(this->CWLoopSideNum).FlowLock == 0) {
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
            if ((this->FlowMode == DataPlant::FlowMode::CONSTANT) || (this->FlowMode == DataPlant::FlowMode::NOTMODULATED)) {
                // Start by assuming max (design) flow
                this->EvapMassFlowRate = this->EvapMassFlowRateMax;
                // Use SetComponentFlowRate to decide actual flow
                PlantUtilities::SetComponentFlowRate(
                    this->EvapMassFlowRate, this->EvapInletNodeNum, this->EvapOutletNodeNum, this->CWLoopNum, this->CWLoopSideNum, this->CWBranchNum, this->CWCompNum);
                // Evaluate delta temp based on actual flow rate
                Real64 EvapDeltaTemp(0.0);
                if (this->EvapMassFlowRate != 0.0) {
                    EvapDeltaTemp = this->QEvaporator / this->EvapMassFlowRate / Cp;
                } else {
                    EvapDeltaTemp = 0.0;
                }
                // Evaluate outlet temp based on delta
                this->EvapOutletTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp - EvapDeltaTemp;

            } else if (this->FlowMode == DataPlant::FlowMode::LEAVINGSETPOINTMODULATED) {

                // Calculate the Delta Temp from the inlet temp to the chiller outlet setpoint
                Real64 EvapDeltaTemp(0.0);
                {
                    auto const SELECT_CASE_var(DataPlant::PlantLoop(this->CWLoopNum).LoopDemandCalcScheme);
                    if (SELECT_CASE_var == DataPlant::SingleSetPoint) {
                        EvapDeltaTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp - DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPoint;
                    } else if (SELECT_CASE_var == DataPlant::DualSetPointDeadBand) {
                        EvapDeltaTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp - DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPointHi;
                    }
                }

                if (EvapDeltaTemp != 0.0) {
                    this->EvapMassFlowRate = std::abs(this->QEvaporator / Cp / EvapDeltaTemp);
                    if ((this->EvapMassFlowRate - this->EvapMassFlowRateMax) > DataBranchAirLoopPlant::MassFlowTolerance) this->PossibleSubcooling = true;
                    // Check to see if the Maximum is exceeded, if so set to maximum
                    this->EvapMassFlowRate = min(this->EvapMassFlowRateMax, this->EvapMassFlowRate);
                    // Use SetComponentFlowRate to decide actual flow
                    PlantUtilities::SetComponentFlowRate(this->EvapMassFlowRate,
                                                         this->EvapInletNodeNum,
                                                         this->EvapOutletNodeNum,
                                                         this->CWLoopNum,
                                                         this->CWLoopSideNum,
                                                         this->CWBranchNum,
                                                         this->CWCompNum);
                    {
                        auto const SELECT_CASE_var(DataPlant::PlantLoop(this->CWLoopNum).LoopDemandCalcScheme);
                        if (SELECT_CASE_var == DataPlant::SingleSetPoint) {
                            this->EvapOutletTemp = DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPoint;
                        } else if (SELECT_CASE_var == DataPlant::DualSetPointDeadBand) {
                            this->EvapOutletTemp = DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPointHi;
                        }
                    }
                } else {
                    // Try to request zero flow
                    this->EvapMassFlowRate = 0.0;
                    // Use SetComponentFlowRate to decide actual flow
                    PlantUtilities::SetComponentFlowRate(this->EvapMassFlowRate,
                                                         this->EvapInletNodeNum,
                                                         this->EvapOutletNodeNum,
                                                         this->CWLoopNum,
                                                         this->CWLoopSideNum,
                                                         this->CWBranchNum,
                                                         this->CWCompNum);
                    // No deltaT since component is not running
                    this->EvapOutletTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp;
                }
            } // End of Constant Variable Flow If Block

            // If there is a fault of Chiller SWT Sensor (zrp_Jun2016)
            if (this->FaultyChillerSWTFlag && (!DataGlobals::WarmupFlag) && (!DataGlobals::DoingSizing) && (!DataGlobals::KickOffSimulation) &&
                (this->EvapMassFlowRate > 0)) {
                // calculate directly affected variables at faulty case: EvapOutletTemp, EvapMassFlowRate, QEvaporator
                int FaultIndex = this->FaultyChillerSWTIndex;
                bool VarFlowFlag = (this->FlowMode == DataPlant::FlowMode::LEAVINGSETPOINTMODULATED);
                FaultsManager::FaultsChillerSWTSensor(FaultIndex)
                    .CalFaultChillerSWT(VarFlowFlag,
                                        this->FaultyChillerSWTOffset,
                                        Cp,
                                        DataLoopNode::Node(this->EvapInletNodeNum).Temp,
                                        this->EvapOutletTemp,
                                        this->EvapMassFlowRate,
                                        this->QEvaporator);
                // update corresponding variables at faulty case
                PartLoadRat = (AvailChillerCap > 0.0) ? (this->QEvaporator / AvailChillerCap) : 0.0;
                PartLoadRat = max(0.0, min(PartLoadRat, this->MaxPartLoadRat));
            }

        } else { // If FlowLock is True

            this->EvapMassFlowRate = DataLoopNode::Node(this->EvapInletNodeNum).MassFlowRate;
            PlantUtilities::SetComponentFlowRate(
                this->EvapMassFlowRate, this->EvapInletNodeNum, this->EvapOutletNodeNum, this->CWLoopNum, this->CWLoopSideNum, this->CWBranchNum, this->CWCompNum);
            // Some other component set the flow to 0. No reason to continue with calculations.
            if (this->EvapMassFlowRate == 0.0) {
                MyLoad = 0.0;
                if (this->CondenserType == DataPlant::CondenserType::EVAPCOOLED) {
                    CalcBasinHeaterPower(
                        this->BasinHeaterPowerFTempDiff, this->BasinHeaterSchedulePtr, this->BasinHeaterSetPointTemp, this->BasinHeaterPower);
                }
                this->PrintMessage = false;
                return;
            }

            if (this->PossibleSubcooling) {
                this->QEvaporator = std::abs(MyLoad);
                Real64 EvapDeltaTemp = this->QEvaporator / this->EvapMassFlowRate / Cp;
                this->EvapOutletTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp - EvapDeltaTemp;
                if (this->EvapOutletTemp < DataLoopNode::Node(this->EvapOutletNodeNum).TempMin) {
                    this->EvapOutletTemp = DataLoopNode::Node(this->EvapOutletNodeNum).TempMin;
                    EvapDeltaTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp - this->EvapOutletTemp;
                    this->QEvaporator = std::abs(this->EvapMassFlowRate * Cp * EvapDeltaTemp);
                }
            } else { // No subcooling in this case.No recalculation required.Still need to check chiller low temp limit

                Real64 TempEvapOutSetPoint(0.0); // C - evaporator outlet temperature setpoint

                {
                    auto const SELECT_CASE_var(DataPlant::PlantLoop(this->CWLoopNum).LoopDemandCalcScheme);
                    if (SELECT_CASE_var == DataPlant::SingleSetPoint) {
                        if ((this->FlowMode == DataPlant::FlowMode::LEAVINGSETPOINTMODULATED) ||
                            (DataPlant::PlantLoop(this->CWLoopNum).LoopSide(this->CWLoopSideNum).Branch(this->CWBranchNum).Comp(this->CWCompNum).CurOpSchemeType ==
                             DataPlant::CompSetPtBasedSchemeType) ||
                            (DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPoint != DataLoopNode::SensedNodeFlagValue)) {
                            TempEvapOutSetPoint = DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPoint;
                        } else {
                            TempEvapOutSetPoint = DataLoopNode::Node(DataPlant::PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPoint;
                        }
                    } else if (SELECT_CASE_var == DataPlant::DualSetPointDeadBand) {
                        if ((this->FlowMode == DataPlant::FlowMode::LEAVINGSETPOINTMODULATED) ||
                            (DataPlant::PlantLoop(this->CWLoopNum).LoopSide(this->CWLoopSideNum).Branch(this->CWBranchNum).Comp(this->CWCompNum).CurOpSchemeType ==
                             DataPlant::CompSetPtBasedSchemeType) ||
                            (DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPointHi != DataLoopNode::SensedNodeFlagValue)) {
                            TempEvapOutSetPoint = DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPointHi;
                        } else {
                            TempEvapOutSetPoint = DataLoopNode::Node(DataPlant::PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPointHi;
                        }
                    }
                }

                Real64 EvapDeltaTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp - TempEvapOutSetPoint;
                this->QEvaporator = std::abs(this->EvapMassFlowRate * Cp * EvapDeltaTemp);
                this->EvapOutletTemp = TempEvapOutSetPoint;
            }
            // Check that the Evap outlet temp honors both plant loop temp low limit and also the chiller low limit
            if (this->EvapOutletTemp < this->TempLowLimitEvapOut) {
                if ((DataLoopNode::Node(this->EvapInletNodeNum).Temp - this->TempLowLimitEvapOut) > DataPlant::DeltaTempTol) {
                    this->EvapOutletTemp = this->TempLowLimitEvapOut;
                    Real64 EvapDeltaTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp - this->EvapOutletTemp;
                    this->QEvaporator = this->EvapMassFlowRate * Cp * EvapDeltaTemp;
                } else {
                    this->EvapOutletTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp;
                    Real64 EvapDeltaTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp - this->EvapOutletTemp;
                    this->QEvaporator = this->EvapMassFlowRate * Cp * EvapDeltaTemp;
                }
            }
            if (this->EvapOutletTemp < DataLoopNode::Node(this->EvapOutletNodeNum).TempMin) {
                if ((DataLoopNode::Node(this->EvapInletNodeNum).Temp - DataLoopNode::Node(this->EvapOutletNodeNum).TempMin) > DataPlant::DeltaTempTol) {
                    this->EvapOutletTemp = DataLoopNode::Node(this->EvapOutletNodeNum).TempMin;
                    Real64 EvapDeltaTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp - this->EvapOutletTemp;
                    this->QEvaporator = this->EvapMassFlowRate * Cp * EvapDeltaTemp;
                } else {
                    this->EvapOutletTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp;
                    Real64 EvapDeltaTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp - this->EvapOutletTemp;
                    this->QEvaporator = this->EvapMassFlowRate * Cp * EvapDeltaTemp;
                }
            }
            // If load exceeds the distributed load set to the distributed load
            if (this->QEvaporator > std::abs(MyLoad)) {
                if (this->EvapMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance) {
                    this->QEvaporator = std::abs(MyLoad);
                    Real64 EvapDeltaTemp = this->QEvaporator / this->EvapMassFlowRate / Cp;
                    this->EvapOutletTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp - EvapDeltaTemp;
                } else {
                    this->QEvaporator = 0.0;
                    this->EvapOutletTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp;
                }
            }

            // If there is a fault of Chiller SWT Sensor (zrp_Jun2016)
            if (this->FaultyChillerSWTFlag && (!DataGlobals::WarmupFlag) && (!DataGlobals::DoingSizing) && (!DataGlobals::KickOffSimulation) &&
                (this->EvapMassFlowRate > 0)) {
                // calculate directly affected variables at faulty case: EvapOutletTemp, EvapMassFlowRate, QEvaporator
                int FaultIndex = this->FaultyChillerSWTIndex;
                bool VarFlowFlag = false;
                FaultsManager::FaultsChillerSWTSensor(FaultIndex)
                    .CalFaultChillerSWT(VarFlowFlag,
                                        this->FaultyChillerSWTOffset,
                                        Cp,
                                        DataLoopNode::Node(this->EvapInletNodeNum).Temp,
                                        this->EvapOutletTemp,
                                        this->EvapMassFlowRate,
                                        this->QEvaporator);
            }

            // Checks QEvaporator on the basis of the machine limits.
            if (this->QEvaporator > (AvailChillerCap * this->MaxPartLoadRat)) {
                if (this->EvapMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance) {
                    this->QEvaporator = AvailChillerCap * OperPartLoadRat;
                    Real64 EvapDeltaTemp = this->QEvaporator / this->EvapMassFlowRate / Cp;
                    this->EvapOutletTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp - EvapDeltaTemp;
                } else {
                    this->QEvaporator = 0.0;
                    this->EvapOutletTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp;
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
                this->EvapOutletTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp;
                this->Power = 0.0;
                this->PrintMessage = false;
            }
            if (this->QEvaporator == 0.0 && this->CondenserType == DataPlant::CondenserType::EVAPCOOLED) {
                CalcBasinHeaterPower(
                    this->BasinHeaterPowerFTempDiff, this->BasinHeaterSchedulePtr, this->BasinHeaterSetPointTemp, this->BasinHeaterPower);
            }
        } // This is the end of the FlowLock Block

        // Now determine Cooling
        // QCondenser is calculated the same for each type, but the power consumption should be different
        //  depending on the performance coefficients used for the chiller model.
        this->QCondenser = this->Power + this->QEvaporator;

        if (this->CondenserType == DataPlant::CondenserType::WATERCOOLED) {

            if (this->CondMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance) {
                Real64 CpCond = FluidProperties::GetSpecificHeatGlycol(
                    DataPlant::PlantLoop(this->CDLoopNum).FluidName, this->CondInletTemp, DataPlant::PlantLoop(this->CDLoopNum).FluidIndex, RoutineName);
                this->CondOutletTemp = this->QCondenser / this->CondMassFlowRate / CpCond + this->CondInletTemp;
            } else {
                ShowSevereError("CalcEngineDrivenChillerModel: Condenser flow = 0, for EngineDrivenChiller=" + this->Name);
                ShowContinueErrorTimeStamp("");
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
            Real64 ClngLoadFuelRat = CurveManager::CurveValue(this->ClngLoadtoFuelCurve, PartLoadRat);
            EngineDrivenFuelEnergy = this->QEvaporator / ClngLoadFuelRat;
        }
        // Use Curve fit to determine energy recovered in the water jacket.  This curve calculates the water jacket energy recovered (J/s) by
        // multiplying the total fuel input (J/s) by the fraction of that power that could be recovered in the water jacket at that
        // particular part load.

        // (RJACDC) Ratio of Recoverable Jacket Heat to Fuel Energy Input
        Real64 RecJacHeattoFuelRat = CurveManager::CurveValue(this->RecJacHeattoFuelCurve, PartLoadRat);
        this->QJacketRecovered = EngineDrivenFuelEnergy * RecJacHeattoFuelRat;

        // Use Curve fit to determine Heat Recovered Lubricant Energy.  This curve calculates the lube energy recovered (J/s) by
        // multiplying the total fuel input (J/s) by the fraction of that power that could be recovered in the lube oil at that
        // particular part load.
        // (RLUBDC) Ratio of Recoverable Lube Oil Heat to Fuel Energy Input
        Real64 RecLubeHeattoFuelRat = CurveManager::CurveValue(this->RecLubeHeattoFuelCurve, PartLoadRat);
        this->QLubeOilRecovered = EngineDrivenFuelEnergy * RecLubeHeattoFuelRat;

        // Use Curve fit to determine Heat Recovered from the exhaust.  This curve calculates the  energy recovered (J/s) by
        // multiplying the total fuel input (J/s) by the fraction of that power that could be recovered in the exhaust at that
        // particular part load.
        Real64 TotExhausttoFuelRat = CurveManager::CurveValue(this->TotExhausttoFuelCurve, PartLoadRat);
        Real64 TotalExhaustEnergy = EngineDrivenFuelEnergy * TotExhausttoFuelRat;

        // Use Curve fit to determine Exhaust Temperature in C.  The temperature is simply a curve fit
        // of the exhaust temperature in C to the part load ratio.
        if (PartLoadRat != 0) {
            Real64 exhaustTemp = CurveManager::CurveValue(this->ExhaustTempCurve, PartLoadRat);
            Real64 ExhaustGasFlow = TotalExhaustEnergy / (ExhaustCP * (exhaustTemp - ReferenceTemp));

            // Use Curve fit to determine stack temp after heat recovery
            Real64 UALocal = this->UACoef(1) * std::pow(ChillerNomCap, this->UACoef(2));
            Real64 designMinExitGasTemp = this->DesignMinExitGasTemp;

            this->ExhaustStackTemp =
                designMinExitGasTemp +
                (exhaustTemp - designMinExitGasTemp) / std::exp(UALocal / (max(ExhaustGasFlow, this->MaxExhaustperPowerOutput * ChillerNomCap) * ExhaustCP));

            this->QExhaustRecovered = max(ExhaustGasFlow * ExhaustCP * (exhaustTemp - this->ExhaustStackTemp), 0.0);
        } else {
            this->QExhaustRecovered = 0.0;
        }

        this->QTotalHeatRecovered = this->QExhaustRecovered + this->QLubeOilRecovered + this->QJacketRecovered;

        // Update Heat Recovery temperatures
        if (this->HeatRecActive) {
            Real64 HeatRecRatio;
            this->calcHeatRecovery(this->QTotalHeatRecovered, HeatRecRatio);
            this->QExhaustRecovered *= HeatRecRatio;
            this->QLubeOilRecovered *= HeatRecRatio;
            this->QJacketRecovered *= HeatRecRatio;
        }

        // Calculate Energy
        this->CondenserEnergy = this->QCondenser * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
        this->Energy = this->Power * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
        this->EvaporatorEnergy = this->QEvaporator * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
        this->FuelEnergyUseRate = EngineDrivenFuelEnergy;
        this->FuelEnergy = this->FuelEnergyUseRate * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
        this->JacketEnergyRec = this->QJacketRecovered * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
        this->LubeOilEnergyRec = this->QLubeOilRecovered * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
        this->ExhaustEnergyRec = this->QExhaustRecovered * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
        this->QTotalHeatRecovered = this->QExhaustRecovered + this->QLubeOilRecovered + this->QJacketRecovered;
        this->TotalHeatEnergyRec = this->ExhaustEnergyRec + this->LubeOilEnergyRec + this->JacketEnergyRec;
        this->FuelEnergyUseRate = std::abs(this->FuelEnergyUseRate);
        this->FuelEnergy = std::abs(this->FuelEnergy);
        this->FuelMdot = std::abs(this->FuelEnergyUseRate) / (this->FuelHeatingValue * KJtoJ);

        // check for problems BG 9/12/06 (deal with observed negative energy results)
        if (this->Energy < 0.0) { // there is a serious problem
            if (this->CondenserType == DataPlant::CondenserType::WATERCOOLED) {
                // first check for run away condenser loop temps (only reason yet to be observed for this?)
                if (this->CondInletTemp > 70.0) {
                    ShowSevereError("CalcEngineDrivenChillerModel: Condenser loop inlet temperatures > 70.0 C for EngineDrivenChiller=" + this->Name);
                    ShowContinueErrorTimeStamp("");
                    ShowContinueError("Condenser loop water temperatures are too high at" + General::RoundSigDigits(this->CondInletTemp, 2));
                    ShowContinueError("Check input for condenser plant loop, especially cooling tower");
                    ShowContinueError("Evaporator inlet temperature: " + General::RoundSigDigits(DataLoopNode::Node(this->EvapInletNodeNum).Temp, 2));

                    ShowFatalError("Program Terminates due to previous error condition");
                }
            }
            if (!DataGlobals::WarmupFlag) {
                if (AvailNomCapRat < 0.0) { // apparently the real reason energy goes negative
                    ShowSevereError("CalcEngineDrivenChillerModel: Capacity ratio below zero for EngineDrivenChiller=" + this->Name);
                    ShowContinueErrorTimeStamp("");
                    ShowContinueError("Check input for Capacity Ratio Curve");
                    ShowContinueError("Condenser inlet temperature: " + General::RoundSigDigits(this->CondInletTemp, 2));
                    ShowContinueError("Evaporator inlet temperature: " + General::RoundSigDigits(DataLoopNode::Node(this->EvapInletNodeNum).Temp, 2));
                    ShowFatalError("Program Terminates due to previous error condition");
                }
            }
            // If makes it here, set limits, chiller can't have negative energy/power
            // proceeding silently for now but may want to throw error here
            this->Power = 0.0;
            this->Energy = 0.0;
        }
    }

    void EngineDrivenChillerSpecs::calcHeatRecovery(Real64 const EnergyRecovered, Real64 &HeatRecRatio)
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
        static std::string const RoutineName("ChillerHeatRecovery");

        // Need to set the HeatRecRatio to 1.0 if it is not modified
        HeatRecRatio = 1.0;

        // This mdot is input specified mdot "Desired Flowrate", already set in init routine
        this->HeatRecMdotActual = DataLoopNode::Node(this->HeatRecInletNodeNum).MassFlowRate;

        this->HeatRecInletTemp = DataLoopNode::Node(this->HeatRecInletNodeNum).Temp;
        Real64 cp = FluidProperties::GetSpecificHeatGlycol(
            DataPlant::PlantLoop(this->HRLoopNum).FluidName, this->HeatRecInletTemp, DataPlant::PlantLoop(this->HRLoopNum).FluidIndex, RoutineName);

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

    void EngineDrivenChillerSpecs::update(Real64 const MyLoad, bool const RunFlag)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Dan Fisher / Brandon Anderson
        //       DATE WRITTEN:    September 2000

        Real64 const ReportingConstant = DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

        if (MyLoad >= 0.0 || !RunFlag) { // Chiller not running
            // set node temperatures
            DataLoopNode::Node(this->EvapOutletNodeNum).Temp = DataLoopNode::Node(this->EvapInletNodeNum).Temp;
            DataLoopNode::Node(this->CondOutletNodeNum).Temp = DataLoopNode::Node(this->CondInletNodeNum).Temp;

            this->Power = 0.0;
            this->QEvaporator = 0.0;
            this->QCondenser = 0.0;
            this->Energy = 0.0;
            this->EvaporatorEnergy = 0.0;
            this->CondenserEnergy = 0.0;
            this->EvapInletTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp;
            this->CondInletTemp = DataLoopNode::Node(this->CondInletNodeNum).Temp;
            this->CondOutletTemp = DataLoopNode::Node(this->CondOutletNodeNum).Temp;
            this->EvapOutletTemp = DataLoopNode::Node(this->EvapOutletNodeNum).Temp;
            this->FuelCOP = 0.0;
            if (this->CondenserType == DataPlant::CondenserType::EVAPCOOLED) {
                this->BasinHeaterPower = this->BasinHeaterPower;
                this->BasinHeaterConsumption = this->BasinHeaterPower * ReportingConstant;
            }
        } else { // Chiller is running
            // set node temperatures
            DataLoopNode::Node(this->EvapOutletNodeNum).Temp = this->EvapOutletTemp;
            DataLoopNode::Node(this->CondOutletNodeNum).Temp = this->CondOutletTemp;

            this->EvapInletTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp;
            this->CondInletTemp = DataLoopNode::Node(this->CondInletNodeNum).Temp;
            this->CondOutletTemp = DataLoopNode::Node(this->CondOutletNodeNum).Temp;
            this->EvapOutletTemp = DataLoopNode::Node(this->EvapOutletNodeNum).Temp;
            if (this->FuelEnergyUseRate != 0.0) {
                this->FuelCOP = this->QEvaporator / this->FuelEnergyUseRate;
            } else {
                this->FuelCOP = 0.0;
            }
            if (this->CondenserType == DataPlant::CondenserType::EVAPCOOLED) {
                this->BasinHeaterPower = this->BasinHeaterPower;
                this->BasinHeaterConsumption = this->BasinHeaterPower * ReportingConstant;
            }
        }

        // Update Heat Recovery Stuff whether running or not, variables should be set correctly
        this->HeatRecMdot = this->HeatRecMdotActual;

        // Update the Heat Recovery outlet
        if (this->HeatRecActive) {
            PlantUtilities::SafeCopyPlantNode(this->HeatRecInletNodeNum, this->HeatRecOutletNodeNum);
            DataLoopNode::Node(this->HeatRecOutletNodeNum).Temp = this->HeatRecOutletTemp;
        }
    }

    GTChillerSpecs *GTChillerSpecs::factory(PlantChillersData &chillers, std::string const &chillerName)
    {
        if (chillers.GetGasTurbineInput) {
            GTChillerSpecs::getInput(chillers);
            chillers.GetGasTurbineInput = false;
        }
        for (auto &thisChiller : chillers.GTChiller) {
            if (UtilityRoutines::MakeUPPERCase(thisChiller.Name) == chillerName) {
                return &thisChiller;
            }
        }
        ShowFatalError("Could not locate gas turbine chiller with name: " + chillerName);
        return nullptr;
    }

    void GTChillerSpecs::simulate(EnergyPlusData &state, const PlantLocation &calledFromLocation, bool FirstHVACIteration, Real64 &CurLoad, bool RunFlag)
    {
        if (calledFromLocation.loopNum == this->CWLoopNum) { // chilled water loop
            this->initialize(state.dataBranchInputManager, RunFlag, CurLoad);
            auto &sim_component(DataPlant::PlantLoop(this->CWLoopNum).LoopSide(this->CWLoopSideNum).Branch(this->CWBranchNum).Comp(this->CWCompNum));
            this->calculate(CurLoad, RunFlag, sim_component.FlowCtrl);
            this->update(CurLoad, RunFlag);
        } else if (calledFromLocation.loopNum == this->CDLoopNum) { // condenser loop
            PlantUtilities::UpdateChillerComponentCondenserSide(this->CDLoopNum,
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
            PlantUtilities::UpdateComponentHeatRecoverySide(this->HRLoopNum,
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

    void GTChillerSpecs::getInput(PlantChillersData &chillers)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Dan Fisher / Brandon Anderson
        //       DATE WRITTEN:    September 2000

        // PURPOSE OF THIS SUBROUTINE:
        // This routine will get the input
        // required by the GT Chiller model.

        // METHODOLOGY EMPLOYED:
        // EnergyPlus input processor

        static std::string const RoutineName("GetGTChillerInput: "); // include trailing blank space

        int NumAlphas;  // Number of elements in the alpha array
        int NumNums;    // Number of elements in the numeric array
        int IOStat;     // IO Status when calling get input subroutine
        bool ErrorsFound(false);

        // FLOW
        DataIPShortCuts::cCurrentModuleObject = "Chiller:CombustionTurbine";
        chillers.NumGTChillers = inputProcessor->getNumObjectsFound(DataIPShortCuts::cCurrentModuleObject);

        if (chillers.NumGTChillers <= 0) {
            ShowSevereError("No " + DataIPShortCuts::cCurrentModuleObject + " equipment specified in input file");
            ErrorsFound = true;
        }
        // See if load distribution manager has already gotten the input
        if (allocated(chillers.GTChiller)) return;

        // ALLOCATE ARRAYS
        chillers.GTChiller.allocate(chillers.NumGTChillers);

        for (int ChillerNum = 1; ChillerNum <= chillers.NumGTChillers; ++ChillerNum) {
            inputProcessor->getObjectItem(DataIPShortCuts::cCurrentModuleObject,
                                          ChillerNum,
                                          DataIPShortCuts::cAlphaArgs,
                                          NumAlphas,
                                          DataIPShortCuts::rNumericArgs,
                                          NumNums,
                                          IOStat,
                                          DataIPShortCuts::lNumericFieldBlanks,
                                          DataIPShortCuts::lAlphaFieldBlanks,
                                          DataIPShortCuts::cAlphaFieldNames,
                                          DataIPShortCuts::cNumericFieldNames);
            UtilityRoutines::IsNameEmpty(DataIPShortCuts::cAlphaArgs(1), DataIPShortCuts::cCurrentModuleObject, ErrorsFound);

            // ErrorsFound will be set to True if problem was found, left untouched otherwise
            GlobalNames::VerifyUniqueChillerName(DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), ErrorsFound, DataIPShortCuts::cCurrentModuleObject + " Name");

            auto &thisChiller = chillers.GTChiller(ChillerNum);
            thisChiller.Name = DataIPShortCuts::cAlphaArgs(1);
            thisChiller.plantTypeOfNum = DataPlant::TypeOf_Chiller_CombTurbine;

            thisChiller.NomCap = DataIPShortCuts::rNumericArgs(1);

            if (thisChiller.NomCap == DataSizing::AutoSize) {
                thisChiller.NomCapWasAutoSized = true;
            }
            if (DataIPShortCuts::rNumericArgs(1) == 0.0) {
                ShowSevereError("Invalid " + DataIPShortCuts::cNumericFieldNames(1) + '=' + General::RoundSigDigits(DataIPShortCuts::rNumericArgs(1), 2));
                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                ErrorsFound = true;
            }

            thisChiller.COP = DataIPShortCuts::rNumericArgs(2);
            if (DataIPShortCuts::rNumericArgs(2) == 0.0) {
                ShowSevereError("Invalid " + DataIPShortCuts::cNumericFieldNames(2) + '=' + General::RoundSigDigits(DataIPShortCuts::rNumericArgs(2), 2));
                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                ErrorsFound = true;
            }

            if (DataIPShortCuts::cAlphaArgs(2) == "AIRCOOLED") {
                thisChiller.CondenserType = DataPlant::CondenserType::AIRCOOLED;
            } else if (DataIPShortCuts::cAlphaArgs(2) == "WATERCOOLED") {
                thisChiller.CondenserType = DataPlant::CondenserType::WATERCOOLED;
            } else if (DataIPShortCuts::cAlphaArgs(2) == "EVAPORATIVELYCOOLED") {
                thisChiller.CondenserType = DataPlant::CondenserType::EVAPCOOLED;
            } else {
                ShowSevereError("Invalid " + DataIPShortCuts::cAlphaFieldNames(2) + '=' + DataIPShortCuts::cAlphaArgs(2));
                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                ErrorsFound = true;
            }

            thisChiller.EvapInletNodeNum = NodeInputManager::GetOnlySingleNode(
                DataIPShortCuts::cAlphaArgs(3), ErrorsFound, DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Inlet, 1, DataLoopNode::ObjectIsNotParent);
            thisChiller.EvapOutletNodeNum = NodeInputManager::GetOnlySingleNode(
                DataIPShortCuts::cAlphaArgs(4), ErrorsFound, DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Outlet, 1, DataLoopNode::ObjectIsNotParent);
            BranchNodeConnections::TestCompSet(DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataIPShortCuts::cAlphaArgs(3), DataIPShortCuts::cAlphaArgs(4), "Chilled Water Nodes");

            if (thisChiller.CondenserType == DataPlant::CondenserType::AIRCOOLED || thisChiller.CondenserType == DataPlant::CondenserType::EVAPCOOLED) {
                // Connection not required for air or evap cooled condenser
                // If the condenser inlet is blank for air cooled and evap cooled condensers then supply a generic name
                // since it is not used elsewhere for connection
                if (DataIPShortCuts::lAlphaFieldBlanks(5)) {
                    if (len(DataIPShortCuts::cAlphaArgs(1)) < DataGlobals::MaxNameLength - 21) { // protect against long name leading to > 100 chars
                        DataIPShortCuts::cAlphaArgs(5) = DataIPShortCuts::cAlphaArgs(1) + " CONDENSER INLET NODE";
                    } else {
                        DataIPShortCuts::cAlphaArgs(5) = DataIPShortCuts::cAlphaArgs(1).substr(0, 79) + " CONDENSER INLET NODE";
                    }
                }
                if (DataIPShortCuts::lAlphaFieldBlanks(6)) {
                    if (len(DataIPShortCuts::cAlphaArgs(1)) < DataGlobals::MaxNameLength - 22) { // protect against long name leading to > 100 chars
                        DataIPShortCuts::cAlphaArgs(6) = DataIPShortCuts::cAlphaArgs(1) + " CONDENSER OUTLET NODE";
                    } else {
                        DataIPShortCuts::cAlphaArgs(6) = DataIPShortCuts::cAlphaArgs(1).substr(0, 78) + " CONDENSER OUTLET NODE";
                    }
                }

                thisChiller.CondInletNodeNum = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(5),
                                                                           ErrorsFound,
                                                                           DataIPShortCuts::cCurrentModuleObject,
                                                                           DataIPShortCuts::cAlphaArgs(1),
                                                                           DataLoopNode::NodeType_Air,
                                                                           DataLoopNode::NodeConnectionType_OutsideAirReference,
                                                                           2,
                                                                           DataLoopNode::ObjectIsNotParent);
                bool Okay;
                OutAirNodeManager::CheckAndAddAirNodeNumber(thisChiller.CondInletNodeNum, Okay);
                if (!Okay) {
                    ShowWarningError(DataIPShortCuts::cCurrentModuleObject + ", Adding OutdoorAir:Node=" + DataIPShortCuts::cAlphaArgs(5));
                }

                thisChiller.CondOutletNodeNum = NodeInputManager::GetOnlySingleNode(
                    DataIPShortCuts::cAlphaArgs(6), ErrorsFound, DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataLoopNode::NodeType_Air, DataLoopNode::NodeConnectionType_Outlet, 2, DataLoopNode::ObjectIsNotParent);
            } else { // WaterCooled CondenserType
                thisChiller.CondInletNodeNum = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(5),
                                                                           ErrorsFound,
                                                                           DataIPShortCuts::cCurrentModuleObject,
                                                                           DataIPShortCuts::cAlphaArgs(1),
                                                                           DataLoopNode::NodeType_Unknown,
                                                                           DataLoopNode::NodeConnectionType_Inlet,
                                                                           2,
                                                                           DataLoopNode::ObjectIsNotParent);
                thisChiller.CondOutletNodeNum = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(6),
                                                                            ErrorsFound,
                                                                            DataIPShortCuts::cCurrentModuleObject,
                                                                            DataIPShortCuts::cAlphaArgs(1),
                                                                            DataLoopNode::NodeType_Unknown,
                                                                            DataLoopNode::NodeConnectionType_Outlet,
                                                                            2,
                                                                            DataLoopNode::ObjectIsNotParent);
                BranchNodeConnections::TestCompSet(DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataIPShortCuts::cAlphaArgs(5), DataIPShortCuts::cAlphaArgs(6), "Condenser (unknown?) Nodes");
                // Condenser Inlet node name is necessary for Water Cooled
                if (DataIPShortCuts::lAlphaFieldBlanks(5)) {
                    ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(5) + " is blank ");
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                    ErrorsFound = true;
                } else if (DataIPShortCuts::lAlphaFieldBlanks(6)) {
                    ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(6) + " is blank ");
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                    ErrorsFound = true;
                }
            }

            thisChiller.MinPartLoadRat = DataIPShortCuts::rNumericArgs(3);
            thisChiller.MaxPartLoadRat = DataIPShortCuts::rNumericArgs(4);
            thisChiller.OptPartLoadRat = DataIPShortCuts::rNumericArgs(5);
            thisChiller.TempDesCondIn = DataIPShortCuts::rNumericArgs(6);
            thisChiller.TempRiseCoef = DataIPShortCuts::rNumericArgs(7);
            thisChiller.TempDesEvapOut = DataIPShortCuts::rNumericArgs(8);
            thisChiller.EvapVolFlowRate = DataIPShortCuts::rNumericArgs(9);
            if (thisChiller.EvapVolFlowRate == DataSizing::AutoSize) {
                thisChiller.EvapVolFlowRateWasAutoSized = true;
            }

            thisChiller.CondVolFlowRate = DataIPShortCuts::rNumericArgs(10);
            if (thisChiller.CondVolFlowRate == DataSizing::AutoSize) {
                if (thisChiller.CondenserType == DataPlant::CondenserType::WATERCOOLED) {
                    thisChiller.CondVolFlowRateWasAutoSized = true;
                }
            }
            thisChiller.CapRatCoef(1) = DataIPShortCuts::rNumericArgs(11);
            thisChiller.CapRatCoef(2) = DataIPShortCuts::rNumericArgs(12);
            thisChiller.CapRatCoef(3) = DataIPShortCuts::rNumericArgs(13);
            if ((DataIPShortCuts::rNumericArgs(11) + DataIPShortCuts::rNumericArgs(12) + DataIPShortCuts::rNumericArgs(13)) == 0.0) {
                ShowSevereError(DataIPShortCuts::cCurrentModuleObject + ": Sum of Capacity Ratio Coef = 0.0, chiller=" + DataIPShortCuts::cAlphaArgs(1));
                ErrorsFound = true;
            }
            thisChiller.PowerRatCoef(1) = DataIPShortCuts::rNumericArgs(14);
            thisChiller.PowerRatCoef(2) = DataIPShortCuts::rNumericArgs(15);
            thisChiller.PowerRatCoef(3) = DataIPShortCuts::rNumericArgs(16);
            thisChiller.FullLoadCoef(1) = DataIPShortCuts::rNumericArgs(17);
            thisChiller.FullLoadCoef(2) = DataIPShortCuts::rNumericArgs(18);
            thisChiller.FullLoadCoef(3) = DataIPShortCuts::rNumericArgs(19);
            thisChiller.TempLowLimitEvapOut = DataIPShortCuts::rNumericArgs(20);

            // Load Special GT Chiller Input

            thisChiller.PLBasedFuelInputCoef(1) = DataIPShortCuts::rNumericArgs(21);
            thisChiller.PLBasedFuelInputCoef(2) = DataIPShortCuts::rNumericArgs(22);
            thisChiller.PLBasedFuelInputCoef(3) = DataIPShortCuts::rNumericArgs(23);

            thisChiller.TempBasedFuelInputCoef(1) = DataIPShortCuts::rNumericArgs(24);
            thisChiller.TempBasedFuelInputCoef(2) = DataIPShortCuts::rNumericArgs(25);
            thisChiller.TempBasedFuelInputCoef(3) = DataIPShortCuts::rNumericArgs(26);

            thisChiller.ExhaustFlowCoef(1) = DataIPShortCuts::rNumericArgs(27);
            thisChiller.ExhaustFlowCoef(2) = DataIPShortCuts::rNumericArgs(28);
            thisChiller.ExhaustFlowCoef(3) = DataIPShortCuts::rNumericArgs(29);

            thisChiller.PLBasedExhaustTempCoef(1) = DataIPShortCuts::rNumericArgs(30);
            thisChiller.PLBasedExhaustTempCoef(2) = DataIPShortCuts::rNumericArgs(31);
            thisChiller.PLBasedExhaustTempCoef(3) = DataIPShortCuts::rNumericArgs(32);

            thisChiller.TempBasedExhaustTempCoef(1) = DataIPShortCuts::rNumericArgs(33);
            thisChiller.TempBasedExhaustTempCoef(2) = DataIPShortCuts::rNumericArgs(34);
            thisChiller.TempBasedExhaustTempCoef(3) = DataIPShortCuts::rNumericArgs(35);

            thisChiller.HeatRecLubeEnergyCoef(1) = DataIPShortCuts::rNumericArgs(36);
            thisChiller.HeatRecLubeEnergyCoef(2) = DataIPShortCuts::rNumericArgs(37);
            thisChiller.HeatRecLubeEnergyCoef(3) = DataIPShortCuts::rNumericArgs(38);

            thisChiller.UAtoCapCoef(1) = DataIPShortCuts::rNumericArgs(39);
            thisChiller.UAtoCapCoef(2) = DataIPShortCuts::rNumericArgs(40);

            thisChiller.GTEngineCapacity = DataIPShortCuts::rNumericArgs(41);
            if (thisChiller.GTEngineCapacity == DataSizing::AutoSize) {
                thisChiller.GTEngineCapacityWasAutoSized = true;
            }
            thisChiller.MaxExhaustperGTPower = DataIPShortCuts::rNumericArgs(42);
            thisChiller.DesignSteamSatTemp = DataIPShortCuts::rNumericArgs(43);
            thisChiller.FuelHeatingValue = DataIPShortCuts::rNumericArgs(44);

            // Get the Heat Recovery information
            // handle autosize
            thisChiller.DesignHeatRecVolFlowRate = DataIPShortCuts::rNumericArgs(45);
            if (thisChiller.DesignHeatRecVolFlowRate > 0.0 || thisChiller.DesignHeatRecVolFlowRate == DataSizing::AutoSize) {
                thisChiller.HeatRecActive = true;
                thisChiller.HeatRecInletNodeNum = NodeInputManager::GetOnlySingleNode(
                    DataIPShortCuts::cAlphaArgs(7), ErrorsFound, DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Inlet, 3, DataLoopNode::ObjectIsNotParent);
                if (thisChiller.HeatRecInletNodeNum == 0) {
                    ShowSevereError("Invalid " + DataIPShortCuts::cAlphaFieldNames(7) + '=' + DataIPShortCuts::cAlphaArgs(7));
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                    ErrorsFound = true;
                }
                thisChiller.HeatRecOutletNodeNum = NodeInputManager::GetOnlySingleNode(
                    DataIPShortCuts::cAlphaArgs(8), ErrorsFound, DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Outlet, 3, DataLoopNode::ObjectIsNotParent);
                if (thisChiller.HeatRecOutletNodeNum == 0) {
                    ShowSevereError("Invalid " + DataIPShortCuts::cAlphaFieldNames(8) + '=' + DataIPShortCuts::cAlphaArgs(8));
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                    ErrorsFound = true;
                }
                BranchNodeConnections::TestCompSet(DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataIPShortCuts::cAlphaArgs(7), DataIPShortCuts::cAlphaArgs(8), "Heat Recovery Nodes");

                if (thisChiller.DesignHeatRecVolFlowRate == DataSizing::AutoSize) {
                    thisChiller.DesignHeatRecVolFlowRateWasAutoSized = true;
                } else {
                    PlantUtilities::RegisterPlantCompDesignFlow(thisChiller.HeatRecInletNodeNum, thisChiller.DesignHeatRecVolFlowRate);
                }

                // Condenser flow rate must be specified for heat reclaim, but Why couldn't this be okay??
                if (thisChiller.CondenserType == DataPlant::CondenserType::AIRCOOLED || thisChiller.CondenserType == DataPlant::CondenserType::EVAPCOOLED) {
                    if (thisChiller.CondVolFlowRate <= 0.0) {
                        ShowSevereError("Invalid " + DataIPShortCuts::cNumericFieldNames(10) + '=' + General::RoundSigDigits(DataIPShortCuts::rNumericArgs(10), 6));
                        ShowSevereError("Condenser fluid flow rate must be specified for Heat Reclaim applications.");
                        ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                        ErrorsFound = true;
                    }
                }

            } else {
                thisChiller.HeatRecActive = false;
                thisChiller.DesignHeatRecMassFlowRate = 0.0;
                thisChiller.HeatRecInletNodeNum = 0;
                thisChiller.HeatRecOutletNodeNum = 0;
                if ((!DataIPShortCuts::lAlphaFieldBlanks(7)) || (!DataIPShortCuts::lAlphaFieldBlanks(8))) {
                    ShowWarningError("Since Design Heat Flow Rate = 0.0, Heat Recovery inactive for " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                    ShowContinueError("However, Node names were specified for heat recovery inlet or outlet nodes");
                }
                if (thisChiller.CondenserType == DataPlant::CondenserType::AIRCOOLED || thisChiller.CondenserType == DataPlant::CondenserType::EVAPCOOLED) {
                    thisChiller.CondVolFlowRate = 0.0011; // set to avoid errors in calc routine
                }
            }

            {
                auto const SELECT_CASE_var(DataIPShortCuts::cAlphaArgs(9));
                if (SELECT_CASE_var == "CONSTANTFLOW") {
                    thisChiller.FlowMode = DataPlant::FlowMode::CONSTANT;
                } else if (SELECT_CASE_var == "LEAVINGSETPOINTMODULATED") {
                    thisChiller.FlowMode = DataPlant::FlowMode::LEAVINGSETPOINTMODULATED;
                } else if (SELECT_CASE_var == "NOTMODULATED") {
                    thisChiller.FlowMode = DataPlant::FlowMode::NOTMODULATED;
                } else {
                    ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\",");
                    ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(9) + '=' + DataIPShortCuts::cAlphaArgs(9));
                    ShowContinueError("Available choices are ConstantFlow, NotModulated, or LeavingSetpointModulated");
                    ShowContinueError("Flow mode NotModulated is assumed and the simulation continues.");
                    thisChiller.FlowMode = DataPlant::FlowMode::NOTMODULATED;
                }
            }

            // Fuel Type Case Statement
            bool FuelTypeError(false);
            UtilityRoutines::ValidateFuelType(DataIPShortCuts::cAlphaArgs(10), thisChiller.FuelType, FuelTypeError);
            if (FuelTypeError) {
                ShowSevereError("Invalid " + DataIPShortCuts::cAlphaFieldNames(10) + '=' + DataIPShortCuts::cAlphaArgs(10));
                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                ShowContinueError(
                    "Valid choices are Electricity, NaturalGas, Propane, Diesel, Gasoline, FuelOilNo1, FuelOilNo2,OtherFuel1 or OtherFuel2");
                ErrorsFound = true;
                FuelTypeError = false;
            }

            thisChiller.HeatRecMaxTemp = DataIPShortCuts::rNumericArgs(46);
            thisChiller.SizFac = DataIPShortCuts::rNumericArgs(47);
            if (thisChiller.SizFac <= 0.0) thisChiller.SizFac = 1.0;

            //   Basin heater power as a function of temperature must be greater than or equal to 0
            thisChiller.BasinHeaterPowerFTempDiff = DataIPShortCuts::rNumericArgs(48);
            if (DataIPShortCuts::rNumericArgs(48) < 0.0) {
                ShowSevereError(DataIPShortCuts::cCurrentModuleObject + "=\"" + thisChiller.Name + "\"" + DataIPShortCuts::cNumericFieldNames(48) + " must be >= 0");
                ErrorsFound = true;
            }

            thisChiller.BasinHeaterSetPointTemp = DataIPShortCuts::rNumericArgs(49);

            if (thisChiller.BasinHeaterPowerFTempDiff > 0.0) {
                if (NumNums < 49) {
                    thisChiller.BasinHeaterSetPointTemp = 2.0;
                }
                if (thisChiller.BasinHeaterSetPointTemp < 2.0) {
                    ShowWarningError(DataIPShortCuts::cCurrentModuleObject + ":\"" + thisChiller.Name + "\", " + DataIPShortCuts::cNumericFieldNames(49) +
                                     " is less than 2 deg C. Freezing could occur.");
                }
            }

            if (!DataIPShortCuts::lAlphaFieldBlanks(11)) {
                thisChiller.BasinHeaterSchedulePtr = ScheduleManager::GetScheduleIndex(DataIPShortCuts::cAlphaArgs(11));
                if (thisChiller.BasinHeaterSchedulePtr == 0) {
                    ShowWarningError(DataIPShortCuts::cCurrentModuleObject + ", \"" + thisChiller.Name + "\" TRIM(DataIPShortCuts::cAlphaFieldNames(11)) \"" +
                                     DataIPShortCuts::cAlphaArgs(11) + "\" was not found. Basin heater operation will not be modeled and the simulation continues");
                }
            }

            if (NumNums > 49) {
                if (!DataIPShortCuts::lNumericFieldBlanks(50)) {
                    thisChiller.HeatRecCapacityFraction = DataIPShortCuts::rNumericArgs(50);
                } else {
                    thisChiller.HeatRecCapacityFraction = 1.0;
                }
            } else {
                thisChiller.HeatRecCapacityFraction = 1.0;
            }

            if (NumNums > 50) {
                if (!DataIPShortCuts::lNumericFieldBlanks(51)) {
                    thisChiller.engineCapacityScalar = DataIPShortCuts::rNumericArgs(51);
                } else {
                    thisChiller.engineCapacityScalar = 0.35;
                }
            } else {
                thisChiller.engineCapacityScalar = 0.35;
            }
        }

        if (ErrorsFound) {
            ShowFatalError("Errors found in processing input for " + DataIPShortCuts::cCurrentModuleObject);
        }
    }

    void GTChillerSpecs::setupOutputVariables()
    {
        SetupOutputVariable("Chiller Drive Shaft Power", OutputProcessor::Unit::W, this->Power, "System", "Average", this->Name);
        SetupOutputVariable("Chiller Drive Shaft Energy", OutputProcessor::Unit::J, this->Energy, "System", "Sum", this->Name);
        SetupOutputVariable("Chiller Evaporator Cooling Rate", OutputProcessor::Unit::W, this->QEvaporator, "System", "Average", this->Name);
        SetupOutputVariable("Chiller Evaporator Cooling Energy",
                            OutputProcessor::Unit::J,
                            this->EvaporatorEnergy,
                            "System",
                            "Sum",
                            this->Name,
                            _,
                            "ENERGYTRANSFER",
                            "CHILLERS",
                            _,
                            "Plant");
        SetupOutputVariable("Chiller Evaporator Inlet Temperature", OutputProcessor::Unit::C, this->EvapInletTemp, "System", "Average", this->Name);
        SetupOutputVariable("Chiller Evaporator Outlet Temperature", OutputProcessor::Unit::C, this->EvapOutletTemp, "System", "Average", this->Name);
        SetupOutputVariable("Chiller Evaporator Mass Flow Rate", OutputProcessor::Unit::kg_s, this->EvapMassFlowRate, "System", "Average", this->Name);
        SetupOutputVariable("Chiller Condenser Heat Transfer Rate", OutputProcessor::Unit::W, this->QCondenser, "System", "Average", this->Name);
        SetupOutputVariable("Chiller Condenser Heat Transfer Energy",
                            OutputProcessor::Unit::J,
                            this->CondenserEnergy,
                            "System",
                            "Sum",
                            this->Name,
                            _,
                            "ENERGYTRANSFER",
                            "HEATREJECTION",
                            _,
                            "Plant");

        SetupOutputVariable("Chiller Condenser Inlet Temperature", OutputProcessor::Unit::C, this->CondInletTemp, "System", "Average", this->Name);

        // Condenser mass flow and outlet temp are valid for water cooled
        if (this->CondenserType == DataPlant::CondenserType::WATERCOOLED) {
            SetupOutputVariable("Chiller Condenser Outlet Temperature", OutputProcessor::Unit::C, this->CondOutletTemp, "System", "Average", this->Name);
            SetupOutputVariable("Chiller Condenser Mass Flow Rate", OutputProcessor::Unit::kg_s, this->CondMassFlowRate, "System", "Average", this->Name);
        } else if (this->CondenserType == DataPlant::CondenserType::AIRCOOLED) {
        } else if (this->CondenserType == DataPlant::CondenserType::EVAPCOOLED) {
            if (this->BasinHeaterPowerFTempDiff > 0.0) {
                SetupOutputVariable("Chiller Basin Heater Electricity Rate", OutputProcessor::Unit::W, this->BasinHeaterPower, "System", "Average", this->Name);
                SetupOutputVariable("Chiller Basin Heater Electricity Energy",
                                    OutputProcessor::Unit::J,
                                    this->BasinHeaterConsumption,
                                    "System",
                                    "Sum",
                                    this->Name,
                                    _,
                                    "Electricity",
                                    "CHILLERS",
                                    _,
                                    "Plant");
            }
        }

        SetupOutputVariable("Chiller Lube Recovered Heat Rate", OutputProcessor::Unit::W, this->HeatRecLubeRate, "System", "Average", this->Name);
        SetupOutputVariable("Chiller Lube Recovered Heat Energy",
                            OutputProcessor::Unit::J,
                            this->HeatRecLubeEnergy,
                            "System",
                            "Sum",
                            this->Name,
                            _,
                            "ENERGYTRANSFER",
                            "HeatRecovery",
                            _,
                            "Plant");

        SetupOutputVariable("Chiller " + this->FuelType + " Rate", OutputProcessor::Unit::W, this->FuelEnergyUsedRate, "System", "Average", this->Name);

        SetupOutputVariable("Chiller " + this->FuelType + " Energy",
                            OutputProcessor::Unit::J,
                            this->FuelEnergyUsed,
                            "System",
                            "Sum",
                            this->Name,
                            _,
                            this->FuelType,
                            "Cooling",
                            _,
                            "Plant");

        SetupOutputVariable("Chiller " + this->FuelType + " Mass Flow Rate", OutputProcessor::Unit::kg_s, this->FuelMassUsedRate, "System", "Average", this->Name);
        SetupOutputVariable("Chiller " + this->FuelType + " Mass", OutputProcessor::Unit::kg, this->FuelMassUsed, "System", "Sum", this->Name);
        SetupOutputVariable("Chiller Exhaust Temperature", OutputProcessor::Unit::C, this->ExhaustStackTemp, "System", "Average", this->Name);
        SetupOutputVariable("Chiller Heat Recovery Inlet Temperature", OutputProcessor::Unit::C, this->HeatRecInletTemp, "System", "Average", this->Name);
        SetupOutputVariable("Chiller Heat Recovery Outlet Temperature", OutputProcessor::Unit::C, this->HeatRecOutletTemp, "System", "Average", this->Name);
        SetupOutputVariable("Chiller Heat Recovery Mass Flow Rate", OutputProcessor::Unit::kg_s, this->HeatRecMdot, "System", "Average", this->Name);
        SetupOutputVariable("Chiller COP", OutputProcessor::Unit::W_W, this->FuelCOP, "System", "Average", this->Name);

        if (DataGlobals::AnyEnergyManagementSystemInModel) {
            SetupEMSInternalVariable("Chiller Nominal Capacity", this->Name, "[W]", this->NomCap);
        }
    }

    void GTChillerSpecs::initialize(BranchInputManagerData &dataBranchInputManager, bool const RunFlag, Real64 const MyLoad)
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
        static std::string const RoutineName("InitGTChiller");

        // Init more variables
        if (this->MyFlag) {
            // Locate the chillers on the plant loops for later usage
            bool errFlag = false;
            this->setupOutputVariables();
            PlantUtilities::ScanPlantLoopsForObject(dataBranchInputManager,
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
            if (this->CondenserType != DataPlant::CondenserType::AIRCOOLED && this->CondenserType != DataPlant::CondenserType::EVAPCOOLED) {
                PlantUtilities::ScanPlantLoopsForObject(dataBranchInputManager,
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
                    this->CWLoopNum, this->CWLoopSideNum, this->CDLoopNum, this->CDLoopSideNum, this->plantTypeOfNum, true);
            }
            if (this->HeatRecActive) {
                PlantUtilities::ScanPlantLoopsForObject(dataBranchInputManager,
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
                    this->CWLoopNum, this->CWLoopSideNum, this->HRLoopNum, this->HRLoopSideNum, this->plantTypeOfNum, true);
            }

            if (this->CondenserType != DataPlant::CondenserType::AIRCOOLED && this->CondenserType != DataPlant::CondenserType::EVAPCOOLED && this->HeatRecActive) {
                PlantUtilities::InterConnectTwoPlantLoopSides(
                    this->CDLoopNum, this->CDLoopSideNum, this->HRLoopNum, this->HRLoopSideNum, this->plantTypeOfNum, false);
            }
            if (errFlag) {
                ShowFatalError("InitGTChiller: Program terminated due to previous condition(s).");
            }

            if (this->FlowMode == DataPlant::FlowMode::CONSTANT) {
                // reset flow priority
                DataPlant::PlantLoop(this->CWLoopNum).LoopSide(this->CWLoopSideNum).Branch(this->CWBranchNum).Comp(this->CWCompNum).FlowPriority =
                    DataPlant::LoopFlowStatus_NeedyIfLoopOn;
            }

            if (this->FlowMode == DataPlant::FlowMode::LEAVINGSETPOINTMODULATED) {
                // reset flow priority
                DataPlant::PlantLoop(this->CWLoopNum).LoopSide(this->CWLoopSideNum).Branch(this->CWBranchNum).Comp(this->CWCompNum).FlowPriority =
                    DataPlant::LoopFlowStatus_NeedyIfLoopOn;

                // check if setpoint on outlet node
                if ((DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPoint == DataLoopNode::SensedNodeFlagValue) &&
                    (DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPointHi == DataLoopNode::SensedNodeFlagValue)) {
                    if (!DataGlobals::AnyEnergyManagementSystemInModel) {
                        if (!this->ModulatedFlowErrDone) {
                            ShowWarningError("Missing temperature setpoint for LeavingSetpointModulated mode chiller named " + this->Name);
                            ShowContinueError(
                                "  A temperature setpoint is needed at the outlet node of a chiller in variable flow mode, use a SetpointManager");
                            ShowContinueError("  The overall loop setpoint will be assumed for chiller. The simulation continues ... ");
                            this->ModulatedFlowErrDone = true;
                        }
                    } else {
                        // need call to EMS to check node
                        bool FatalError = false; // but not really fatal yet, but should be.
                        EMSManager::CheckIfNodeSetPointManagedByEMS(this->EvapOutletNodeNum, EMSManager::iTemperatureSetPoint, FatalError);
                        if (FatalError) {
                            if (!this->ModulatedFlowErrDone) {
                                ShowWarningError("Missing temperature setpoint for LeavingSetpointModulated mode chiller named " + this->Name);
                                ShowContinueError(
                                    "  A temperature setpoint is needed at the outlet node of a chiller evaporator in variable flow mode");
                                ShowContinueError("  use a Setpoint Manager to establish a setpoint at the chiller evaporator outlet node ");
                                ShowContinueError("  or use an EMS actuator to establish a setpoint at the outlet node ");
                                ShowContinueError("  The overall loop setpoint will be assumed for chiller. The simulation continues ... ");
                                this->ModulatedFlowErrDone = true;
                            }
                        }
                    }
                    this->ModulatedFlowSetToLoop = true;
                    DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPoint = DataLoopNode::Node(DataPlant::PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPoint;
                    DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPointHi = DataLoopNode::Node(DataPlant::PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPointHi;
                }
            }
            this->MyFlag = false;
        }

        if (this->MyEnvrnFlag && DataGlobals::BeginEnvrnFlag && (DataPlant::PlantFirstSizesOkayToFinalize)) {

            Real64 rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->CWLoopNum).FluidName,
                                                    DataGlobals::CWInitConvTemp,
                                                    DataPlant::PlantLoop(this->CWLoopNum).FluidIndex,
                                                    RoutineName);

            this->EvapMassFlowRateMax = rho * this->EvapVolFlowRate;
            PlantUtilities::InitComponentNodes(0.0,
                                               this->EvapMassFlowRateMax,
                                               this->EvapInletNodeNum,
                                               this->EvapOutletNodeNum,
                                               this->CWLoopNum,
                                               this->CWLoopSideNum,
                                               this->CWBranchNum,
                                               this->CWCompNum);

            // init maximum available condenser flow rate
            if (this->CondenserType == DataPlant::CondenserType::WATERCOOLED) {

                DataLoopNode::Node(this->CondInletNodeNum).Temp = this->TempDesCondIn;

                rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->CDLoopNum).FluidName,
                                                        DataGlobals::CWInitConvTemp,
                                                        DataPlant::PlantLoop(this->CDLoopNum).FluidIndex,
                                                        RoutineName);

                this->CondMassFlowRateMax = rho * this->CondVolFlowRate;

                PlantUtilities::InitComponentNodes(0.0,
                                                   this->CondMassFlowRateMax,
                                                   this->CondInletNodeNum,
                                                   this->CondOutletNodeNum,
                                                   this->CDLoopNum,
                                                   this->CDLoopSideNum,
                                                   this->CDBranchNum,
                                                   this->CDCompNum);
            } else { // air or evap-air
                DataLoopNode::Node(this->CondInletNodeNum).MassFlowRate =
                    this->CondVolFlowRate * Psychrometrics::PsyRhoAirFnPbTdbW(DataEnvironment::StdBaroPress, this->TempDesCondIn, 0.0, RoutineName);

                DataLoopNode::Node(this->CondOutletNodeNum).MassFlowRate = DataLoopNode::Node(this->CondInletNodeNum).MassFlowRate;
                DataLoopNode::Node(this->CondInletNodeNum).MassFlowRateMaxAvail = DataLoopNode::Node(this->CondInletNodeNum).MassFlowRate;
                DataLoopNode::Node(this->CondInletNodeNum).MassFlowRateMax = DataLoopNode::Node(this->CondInletNodeNum).MassFlowRate;
                DataLoopNode::Node(this->CondOutletNodeNum).MassFlowRateMax = DataLoopNode::Node(this->CondInletNodeNum).MassFlowRate;
                DataLoopNode::Node(this->CondInletNodeNum).MassFlowRateMinAvail = 0.0;
                DataLoopNode::Node(this->CondInletNodeNum).MassFlowRateMin = 0.0;
                DataLoopNode::Node(this->CondOutletNodeNum).MassFlowRateMinAvail = 0.0;
                DataLoopNode::Node(this->CondOutletNodeNum).MassFlowRateMin = 0.0;
            }

            if (this->HeatRecActive) {
                rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->HRLoopNum).FluidName,
                                                        DataGlobals::HWInitConvTemp,
                                                        DataPlant::PlantLoop(this->HRLoopNum).FluidIndex,
                                                        RoutineName);
                this->DesignHeatRecMassFlowRate = rho * this->DesignHeatRecVolFlowRate;

                PlantUtilities::InitComponentNodes(0.0,
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

        if (!DataGlobals::BeginEnvrnFlag) {
            this->MyEnvrnFlag = true;
        }

        if ((this->FlowMode == DataPlant::FlowMode::LEAVINGSETPOINTMODULATED) && (this->ModulatedFlowSetToLoop)) {
            // fix for clumsy old input that worked because loop setpoint was spread.
            //  could be removed with transition, testing , model change, period of being obsolete.
            DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPoint = DataLoopNode::Node(DataPlant::PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPoint;
            DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPointHi = DataLoopNode::Node(DataPlant::PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPointHi;
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
            mdot, this->EvapInletNodeNum, this->EvapOutletNodeNum, this->CWLoopNum, this->CWLoopSideNum, this->CWBranchNum, this->CWCompNum);
        if (this->CondenserType == DataPlant::CondenserType::WATERCOOLED) {
            PlantUtilities::SetComponentFlowRate(
                mdotCond, this->CondInletNodeNum, this->CondOutletNodeNum, this->CDLoopNum, this->CDLoopSideNum, this->CDBranchNum, this->CDCompNum);
        }

        // Initialize heat recovery flow rates at node
        if (this->HeatRecActive) {

            if (RunFlag) {
                mdot = this->DesignHeatRecMassFlowRate;
            } else {
                mdot = 0.0;
            }

            PlantUtilities::SetComponentFlowRate(mdot, this->HeatRecInletNodeNum, this->HeatRecOutletNodeNum, this->HRLoopNum, this->HRLoopSideNum, this->HRBranchNum, this->HRCompNum);
        }
        if (this->CondenserType == DataPlant::CondenserType::EVAPCOOLED) {
            this->BasinHeaterPower = 0.0;
        }
    }

    void GTChillerSpecs::size()
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
        static std::string const RoutineName("SizeGTChiller");

        bool ErrorsFound = false;
        Real64 tmpNomCap = this->NomCap;
        Real64 tmpEvapVolFlowRate = this->EvapVolFlowRate;
        Real64 tmpCondVolFlowRate = this->CondVolFlowRate;

        int PltSizCondNum(0); // Plant Sizing index for condenser loop
        if (this->CondenserType == DataPlant::CondenserType::WATERCOOLED) {
            PltSizCondNum = DataPlant::PlantLoop(this->CDLoopNum).PlantSizNum;
        }

        int PltSizNum = DataPlant::PlantLoop(this->CWLoopNum).PlantSizNum;

        if (PltSizNum > 0) {
            if (DataSizing::PlantSizData(PltSizNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                Real64 rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->CWLoopNum).FluidName,
                                                        DataGlobals::CWInitConvTemp,
                                                        DataPlant::PlantLoop(this->CWLoopNum).FluidIndex,
                                                        RoutineName);
                Real64 Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(this->CWLoopNum).FluidName,
                                                            DataGlobals::CWInitConvTemp,
                                                            DataPlant::PlantLoop(this->CWLoopNum).FluidIndex,
                                                            RoutineName);
                tmpNomCap = Cp * rho * DataSizing::PlantSizData(PltSizNum).DeltaT * DataSizing::PlantSizData(PltSizNum).DesVolFlowRate * this->SizFac;
            } else {
                if (this->NomCapWasAutoSized) tmpNomCap = 0.0;
            }
            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                if (this->NomCapWasAutoSized) {
                    this->NomCap = tmpNomCap;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(
                            "Chiller:CombustionTurbine", this->Name, "Design Size Nominal Capacity [W]", tmpNomCap);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(
                            "Chiller:CombustionTurbine", this->Name, "Initial Design Size Nominal Capacity [W]", tmpNomCap);
                    }
                } else {
                    if (this->NomCap > 0.0 && tmpNomCap > 0.0) {
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput("Chiller:CombustionTurbine",
                                                                    this->Name,
                                                                    "Design Size Nominal Capacity [W]",
                                                                    tmpNomCap,
                                                                    "User-Specified Nominal Capacity [W]",
                                                                    this->NomCap);
                            if (DataGlobals::DisplayExtraWarnings) {
                                if ((std::abs(tmpNomCap - this->NomCap) / this->NomCap) > DataSizing::AutoVsHardSizingThreshold) {
                                    ShowMessage("SizeGTChiller: Potential issue with equipment sizing for " + this->Name);
                                    ShowContinueError("User-Specified Nominal Capacity of " + General::RoundSigDigits(this->NomCap, 2) + " [W]");
                                    ShowContinueError("differs from Design Size Nominal Capacity of " + General::RoundSigDigits(tmpNomCap, 2) + " [W]");
                                    ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                        }
                        tmpNomCap = this->NomCap;
                    }
                }
            }
        } else {
            if (this->NomCapWasAutoSized && DataPlant::PlantFirstSizesOkayToFinalize) {
                ShowSevereError("Autosizing of Gas Turbine Chiller nominal capacity requires a loop Sizing:Plant object");
                ShowContinueError("Occurs in Gas Turbine Chiller object=" + this->Name);
                ErrorsFound = true;
            }
            if (!this->NomCapWasAutoSized && DataPlant::PlantFinalSizesOkayToReport && (this->NomCap > 0.0)) {
                ReportSizingManager::ReportSizingOutput(
                    "Chiller:CombustionTurbine", this->Name, "User-Specified Design Size Nominal Capacity [W]", this->NomCap);
            }
        }

        if (PltSizNum > 0) {
            if (DataSizing::PlantSizData(PltSizNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                tmpEvapVolFlowRate = DataSizing::PlantSizData(PltSizNum).DesVolFlowRate * this->SizFac;
            } else {
                if (this->EvapVolFlowRateWasAutoSized) tmpEvapVolFlowRate = 0.0;
            }
            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                if (this->EvapVolFlowRateWasAutoSized) {
                    this->EvapVolFlowRate = tmpEvapVolFlowRate;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(
                            "Chiller:CombustionTurbine", this->Name, "Design size Design Chilled Water Flow Rate [m3/s]", tmpEvapVolFlowRate);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(
                            "Chiller:CombustionTurbine", this->Name, "Initial Design size Design Chilled Water Flow Rate [m3/s]", tmpEvapVolFlowRate);
                    }
                } else {
                    if (this->EvapVolFlowRate > 0.0 && tmpEvapVolFlowRate > 0.0) {
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput("Chiller:CombustionTurbine",
                                                                    this->Name,
                                                                    "Design size Design Chilled Water Flow Rate [m3/s]",
                                                                    tmpEvapVolFlowRate,
                                                                    "User-Specified Design Chilled Water Flow Rate [m3/s]",
                                                                    this->EvapVolFlowRate);
                            if (DataGlobals::DisplayExtraWarnings) {
                                if ((std::abs(tmpEvapVolFlowRate - this->EvapVolFlowRate) / this->EvapVolFlowRate) >
                                    DataSizing::AutoVsHardSizingThreshold) {
                                    ShowMessage("SizeGTChiller: Potential issue with equipment sizing for " + this->Name);
                                    ShowContinueError("User-Specified Design Chilled Water Flow Rate of " + General::RoundSigDigits(this->EvapVolFlowRate, 5) +
                                                      " [m3/s]");
                                    ShowContinueError("differs from Design Size Design Chilled Water Flow Rate of " +
                                                      General::RoundSigDigits(tmpEvapVolFlowRate, 5) + " [m3/s]");
                                    ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                        }
                        tmpEvapVolFlowRate = this->EvapVolFlowRate;
                    }
                }
            }
        } else {
            if (this->EvapVolFlowRateWasAutoSized && DataPlant::PlantFirstSizesOkayToFinalize) {
                ShowSevereError("Autosizing of Gas Turbine Chiller evap flow rate requires a loop Sizing:Plant object");
                ShowContinueError("Occurs in Gas Turbine Chiller object=" + this->Name);
                ErrorsFound = true;
            }
            if (!this->EvapVolFlowRateWasAutoSized && DataPlant::PlantFinalSizesOkayToReport && (this->EvapVolFlowRate > 0.0)) {
                ReportSizingManager::ReportSizingOutput(
                    "Chiller:CombustionTurbine", this->Name, "User-Specified Design Chilled Water Flow Rate [m3/s]", this->EvapVolFlowRate);
            }
        }

        PlantUtilities::RegisterPlantCompDesignFlow(this->EvapInletNodeNum, tmpEvapVolFlowRate);

        if (PltSizCondNum > 0 && PltSizNum > 0) {
            if (DataSizing::PlantSizData(PltSizNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow && tmpNomCap > 0.0) {
                Real64 rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->CDLoopNum).FluidName,
                                                        this->TempDesCondIn,
                                                        DataPlant::PlantLoop(this->CDLoopNum).FluidIndex,
                                                        RoutineName);
                Real64 Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(this->CDLoopNum).FluidName,
                                                            this->TempDesCondIn,
                                                            DataPlant::PlantLoop(this->CDLoopNum).FluidIndex,
                                                            RoutineName);
                tmpCondVolFlowRate = tmpNomCap * (1.0 + 1.0 / this->COP) / (DataSizing::PlantSizData(PltSizCondNum).DeltaT * Cp * rho);
            } else {
                if (this->CondVolFlowRateWasAutoSized) tmpCondVolFlowRate = 0.0;
            }
            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                if (this->CondVolFlowRateWasAutoSized) {
                    this->CondVolFlowRate = tmpCondVolFlowRate;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(
                            "Chiller:CombustionTurbine", this->Name, "Design Size Design Condenser Water Flow Rate [m3/s]", tmpCondVolFlowRate);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput("Chiller:CombustionTurbine",
                                                                this->Name,
                                                                "Initial Design Size Design Condenser Water Flow Rate [m3/s]",
                                                                tmpCondVolFlowRate);
                    }
                } else {
                    if (this->CondVolFlowRate > 0.0 && tmpCondVolFlowRate > 0.0) {
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput("Chiller:CombustionTurbine",
                                                                    this->Name,
                                                                    "Design Size Design Condenser Water Flow Rate [m3/s]",
                                                                    tmpCondVolFlowRate,
                                                                    "User-Specified Design Condenser Water Flow Rate [m3/s]",
                                                                    this->CondVolFlowRate);
                            if (DataGlobals::DisplayExtraWarnings) {
                                if ((std::abs(tmpCondVolFlowRate - this->CondVolFlowRate) / this->CondVolFlowRate) >
                                    DataSizing::AutoVsHardSizingThreshold) {
                                    ShowMessage("SizeGTChiller: Potential issue with equipment sizing for " + this->Name);
                                    ShowContinueError("User-Specified Design Condenser Water Flow Rate of " + General::RoundSigDigits(this->CondVolFlowRate, 5) +
                                                      " [m3/s]");
                                    ShowContinueError("differs from Design Size Design Condenser Water Flow Rate of " +
                                                      General::RoundSigDigits(tmpCondVolFlowRate, 5) + " [m3/s]");
                                    ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                        }
                        tmpCondVolFlowRate = this->CondVolFlowRate;
                    }
                }
            }
        } else {
            if (this->CondVolFlowRateWasAutoSized && DataPlant::PlantFirstSizesOkayToFinalize) {
                ShowSevereError("Autosizing of Gas Turbine Chiller condenser flow rate requires a condenser");
                ShowContinueError("loop Sizing:Plant object");
                ShowContinueError("Occurs in Gas Turbine Chiller object=" + this->Name);
                ErrorsFound = true;
            }
            if (!this->CondVolFlowRateWasAutoSized && DataPlant::PlantFinalSizesOkayToReport && (this->CondVolFlowRate > 0.0)) {
                ReportSizingManager::ReportSizingOutput(
                    "Chiller:CombustionTurbine", this->Name, "User-Specified Design Condenser Water Flow Rate [m3/s]", this->CondVolFlowRate);
            }
        }
        // save the design condenser water volumetric flow rate for use by the condenser water loop sizing algorithms
        if (this->CondenserType == DataPlant::CondenserType::WATERCOOLED) PlantUtilities::RegisterPlantCompDesignFlow(this->CondInletNodeNum, tmpCondVolFlowRate);

        Real64 GTEngineCapacityDes = this->NomCap / (this->engineCapacityScalar * this->COP);
        if (DataPlant::PlantFirstSizesOkayToFinalize) {
            if (this->GTEngineCapacityWasAutoSized) {
                this->GTEngineCapacity = GTEngineCapacityDes;
                if (DataPlant::PlantFinalSizesOkayToReport) {
                    ReportSizingManager::ReportSizingOutput(
                        "Chiller:CombustionTurbine", this->Name, "Design Size Gas Turbine Engine Capacity [W]", GTEngineCapacityDes);
                }
                if (DataPlant::PlantFirstSizesOkayToReport) {
                    ReportSizingManager::ReportSizingOutput(
                        "Chiller:CombustionTurbine", this->Name, "Initial Design Size Gas Turbine Engine Capacity [W]", GTEngineCapacityDes);
                }
            } else {
                if (this->GTEngineCapacity > 0.0 && GTEngineCapacityDes > 0.0) {
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput("Chiller:CombustionTurbine",
                                                                this->Name,
                                                                "Design Size Gas Turbine Engine Capacity [W]",
                                                                GTEngineCapacityDes,
                                                                "User-Specified Gas Turbine Engine Capacity [W]",
                                                                this->GTEngineCapacity);
                    }
                    if (DataGlobals::DisplayExtraWarnings) {
                        if ((std::abs(GTEngineCapacityDes - this->GTEngineCapacity) / this->GTEngineCapacity) > DataSizing::AutoVsHardSizingThreshold) {
                            ShowMessage("SizeGTChiller: Potential issue with equipment sizing for " + this->Name);
                            ShowContinueError("User-Specified Gas Turbine Engine Capacity of " + General::RoundSigDigits(this->GTEngineCapacity, 2) + " [W]");
                            ShowContinueError("differs from Design Size Gas Turbine Engine Capacity of " + General::RoundSigDigits(GTEngineCapacityDes, 2) +
                                              " [W]");
                            ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                            ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                        }
                    }
                }
            }
        }

        // autosize support for heat recovery flow rate.
        if (this->HeatRecActive) {
            Real64 tmpHeatRecVolFlowRate = this->CondVolFlowRate * this->HeatRecCapacityFraction;
            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                if (this->DesignHeatRecVolFlowRateWasAutoSized) {
                    this->DesignHeatRecVolFlowRate = tmpHeatRecVolFlowRate;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput("Chiller:CombustionTurbine",
                                                                this->Name,
                                                                "Design Size Design Heat Recovery Fluid Flow Rate [m3/s]",
                                                                tmpHeatRecVolFlowRate);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput("Chiller:CombustionTurbine",
                                                                this->Name,
                                                                "Initial Design Size Design Heat Recovery Fluid Flow Rate [m3/s]",
                                                                tmpHeatRecVolFlowRate);
                    }
                } else {
                    if (this->DesignHeatRecVolFlowRate > 0.0 && tmpHeatRecVolFlowRate > 0.0) {
                        Real64 DesignHeatRecVolFlowRateUser = this->DesignHeatRecVolFlowRate;
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            if (DataGlobals::DoPlantSizing) {
                                ReportSizingManager::ReportSizingOutput("Chiller:CombustionTurbine",
                                                                        this->Name,
                                                                        "Design Size Design Heat Recovery Fluid Flow Rate [m3/s]",
                                                                        tmpHeatRecVolFlowRate,
                                                                        "User-Specified Design Heat Recovery Fluid Flow Rate [m3/s]",
                                                                        DesignHeatRecVolFlowRateUser);
                            } else {
                                ReportSizingManager::ReportSizingOutput("Chiller:CombustionTurbine",
                                                                        this->Name,
                                                                        "User-Specified Design Heat Recovery Fluid Flow Rate [m3/s]",
                                                                        DesignHeatRecVolFlowRateUser);
                            }
                            if (DataGlobals::DisplayExtraWarnings) {
                                if ((std::abs(tmpHeatRecVolFlowRate - DesignHeatRecVolFlowRateUser) / DesignHeatRecVolFlowRateUser) >
                                    DataSizing::AutoVsHardSizingThreshold) {
                                    ShowMessage("SizeGasTurbineChiller: Potential issue with equipment sizing for " + this->Name);
                                    ShowContinueError("User-Specified Design Heat Recovery Fluid Flow Rate of " +
                                                      General::RoundSigDigits(DesignHeatRecVolFlowRateUser, 5) + " [m3/s]");
                                    ShowContinueError("differs from Design Size Design Heat Recovery Fluid Flow Rate of " +
                                                      General::RoundSigDigits(tmpHeatRecVolFlowRate, 5) + " [m3/s]");
                                    ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                        }
                        tmpHeatRecVolFlowRate = DesignHeatRecVolFlowRateUser;
                    }
                }
            }
            if (!this->DesignHeatRecVolFlowRateWasAutoSized) tmpHeatRecVolFlowRate = this->DesignHeatRecVolFlowRate;
            // save the reference heat recovery fluid volumetric flow rate
            PlantUtilities::RegisterPlantCompDesignFlow(this->HeatRecInletNodeNum, tmpHeatRecVolFlowRate);
        }

        if (DataPlant::PlantFinalSizesOkayToReport) {
            OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchMechType, this->Name, "Chiller:CombustionTurbine");
            OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchMechNomEff, this->Name, this->COP);
            OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchMechNomCap, this->Name, this->NomCap);
        }

        if (ErrorsFound) {
            ShowFatalError("Preceding sizing errors cause program termination");
        }
    }

    void GTChillerSpecs::calculate(Real64 &MyLoad, bool const RunFlag, int const EquipFlowCtrl)
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
        static std::string const RoutineName("CalcGTChillerModel");
        static std::string const RoutineNameHeatRecovery("ChillerHeatRecovery");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 TempEvapOutSetPoint(0.0); // C - evaporator outlet temperature setpoint
        Real64 EvapDeltaTemp(0.0);       // C - evaporator temperature difference, water side
        Real64 PartLoadRat(0.0);         // part load ratio for efficiency calculations
        Real64 fuelEnergyIn(0.0);  // (EFUEL) Amount of Fuel Energy Required to run gas turbine
        Real64 exhaustFlow(0.0);   // (FEX) Exhaust Gas Flow Rate cubic meters per second
        Real64 exhaustTemp(0.0);   // (TEX) Exhaust Gas Temperature in C
        Real64 HeatRecOutTemp(0.0); // Heat Recovery Fluid Outlet Temperature
        Real64 heatRecMdot(0.0);   // Heat Recovery Fluid Mass FlowRate
        Real64 MinHeatRecMdot(0.0); // Mass Flow rate that keeps from exceeding max temp

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
        Real64 CurrentEndTime = DataGlobals::CurrentTime + DataHVACGlobals::SysTimeElapsed;

        // Print warning messages only when valid and only for the first ocurrance. Let summary provide statistics.
        // Wait for next time step to print warnings. If simulation iterates, print out
        // the warning for the last iteration only. Must wait for next time step to accomplish this.
        // If a warning occurs and the simulation down shifts, the warning is not valid.
        if (CurrentEndTime > this->CurrentEndTimeLast && DataHVACGlobals::TimeStepSys >= this->TimeStepSysLast) {
            if (this->PrintMessage) {
                ++this->MsgErrorCount;
                // Show single warning and pass additional info to ShowRecurringWarningErrorAtEnd
                if (this->MsgErrorCount < 2) {
                    ShowWarningError(this->MsgBuffer1 + '.');
                    ShowContinueError(this->MsgBuffer2);
                } else {
                    ShowRecurringWarningErrorAtEnd(
                        this->MsgBuffer1 + " error continues.", this->ErrCount1, this->MsgDataLast, this->MsgDataLast, _, "[C]", "[C]");
                }
            }
        }

        // save last system time step and last end time of current time step (used to determine if warning is valid)
        this->TimeStepSysLast = DataHVACGlobals::TimeStepSys;
        this->CurrentEndTimeLast = CurrentEndTime;

        // If Chiller load is 0 or chiller is not running then leave the subroutine.Before leaving
        // if the component control is SERIESACTIVE we set the component flow to inlet flow so that
        // flow resolver will not shut down the branch
        if (MyLoad >= 0.0 || !RunFlag) {
            if (EquipFlowCtrl == DataBranchAirLoopPlant::ControlType_SeriesActive ||
                DataPlant::PlantLoop(this->CWLoopNum).LoopSide(this->CWLoopSideNum).FlowLock == 1) {
                this->EvapMassFlowRate = DataLoopNode::Node(this->EvapInletNodeNum).MassFlowRate;
            } else {
                this->EvapMassFlowRate = 0.0;

                PlantUtilities::SetComponentFlowRate(
                    this->EvapMassFlowRate, this->EvapInletNodeNum, this->EvapOutletNodeNum, this->CWLoopNum, this->CWLoopSideNum, this->CWBranchNum, this->CWCompNum);
            }
            if (this->CondenserType == DataPlant::CondenserType::WATERCOOLED) {
                if (DataPlant::PlantLoop(this->CDLoopNum).LoopSide(this->CDLoopSideNum).Branch(this->CDBranchNum).Comp(this->CDCompNum).FlowCtrl ==
                    DataBranchAirLoopPlant::ControlType_SeriesActive) {
                    this->CondMassFlowRate = DataLoopNode::Node(this->CondInletNodeNum).MassFlowRate;
                } else {
                    this->CondMassFlowRate = 0.0;
                    PlantUtilities::SetComponentFlowRate(this->CondMassFlowRate,
                                                         this->CondInletNodeNum,
                                                         this->CondOutletNodeNum,
                                                         this->CDLoopNum,
                                                         this->CDLoopSideNum,
                                                         this->CDBranchNum,
                                                         this->CDCompNum);
                }
            }

            if (this->CondenserType == DataPlant::CondenserType::EVAPCOOLED) {
                CalcBasinHeaterPower(
                    this->BasinHeaterPowerFTempDiff, this->BasinHeaterSchedulePtr, this->BasinHeaterSetPointTemp, this->BasinHeaterPower);
            }
            this->PrintMessage = false;
            return;
        }

        if (this->CondenserType == DataPlant::CondenserType::AIRCOOLED) { // Condenser inlet temp = outdoor temp
            DataLoopNode::Node(this->CondInletNodeNum).Temp = DataLoopNode::Node(this->CondInletNodeNum).OutAirDryBulb;
            //  Warn user if entering condenser temperature falls below 0C
            if (DataLoopNode::Node(this->CondInletNodeNum).Temp < 0.0 && !DataGlobals::WarmupFlag) {
                this->PrintMessage = true;
                this->MsgBuffer1 =
                    "CalcGasTurbineChillerModel - Chiller:CombustionTurbine \"" + this->Name + "\" - Air Cooled Condenser Inlet Temperature below 0C";
                this->MsgBuffer2 = format("... Outdoor Dry-bulb Condition = {:6.2F} C. Occurrence info = {}, {} {}",
                                          DataLoopNode::Node(this->CondInletNodeNum).Temp,
                                          DataEnvironment::EnvironmentName,
                                          DataEnvironment::CurMnDy,
                                          General::CreateSysTimeIntervalString());
                this->MsgDataLast = DataLoopNode::Node(this->CondInletNodeNum).Temp;
            } else {
                this->PrintMessage = false;
            }
        } else if (this->CondenserType == DataPlant::CondenserType::EVAPCOOLED) { // Condenser inlet temp = (outdoor wet bulb)
            DataLoopNode::Node(this->CondInletNodeNum).Temp = DataLoopNode::Node(this->CondInletNodeNum).OutAirWetBulb;
            //  Warn user if evap condenser wet bulb temperature falls below 10C
            if (DataLoopNode::Node(this->CondInletNodeNum).Temp < 10.0 && !DataGlobals::WarmupFlag) {
                this->PrintMessage = true;
                this->MsgBuffer1 = "CalcGasTurbineChillerModel - Chiller:CombustionTurbine \"" + this->Name +
                                   "\" - Evap Cooled Condenser Inlet Temperature below 10C";
                this->MsgBuffer2 = format("... Outdoor Wet-bulb Condition = {:6.2F} C. Occurrence info = {}, {} {}",
                                          DataLoopNode::Node(this->CondInletNodeNum).Temp,
                                          DataEnvironment::EnvironmentName,
                                          DataEnvironment::CurMnDy,
                                          General::CreateSysTimeIntervalString());
                this->MsgDataLast = DataLoopNode::Node(this->CondInletNodeNum).Temp;
            } else {
                this->PrintMessage = false;
            }
        } // End of the Air Cooled/Evap Cooled Logic block

        // If not air or evap cooled then set to the condenser node that is attached to a cooling tower
        Real64 condInletTemp = DataLoopNode::Node(this->CondInletNodeNum).Temp;

        // Set mass flow rates
        if (this->CondenserType == DataPlant::CondenserType::WATERCOOLED) {
            this->CondMassFlowRate = this->CondMassFlowRateMax;
            PlantUtilities::SetComponentFlowRate(
                this->CondMassFlowRate, this->CondInletNodeNum, this->CondOutletNodeNum, this->CDLoopNum, this->CDLoopSideNum, this->CDBranchNum, this->CDCompNum);
            PlantUtilities::PullCompInterconnectTrigger(this->CWLoopNum,
                                                        this->CWLoopSideNum,
                                                        this->CWBranchNum,
                                                        this->CWCompNum,
                                                        this->CondMassFlowIndex,
                                                        this->CDLoopNum,
                                                        this->CDLoopSideNum,
                                                        DataPlant::CriteriaType_MassFlowRate,
                                                        this->CondMassFlowRate);

            if (this->CondMassFlowRate < DataBranchAirLoopPlant::MassFlowTolerance) return;
        }

        //  LOAD LOCAL VARIABLES FROM DATA STRUCTURE (for code readability)
        Real64 ChillerNomCap = this->NomCap;
        Real64 COP = this->COP;
        Real64 TempCondIn = DataLoopNode::Node(this->CondInletNodeNum).Temp;
        Real64 TempEvapOut = DataLoopNode::Node(this->EvapOutletNodeNum).Temp;

        // If there is a fault of chiller fouling
        if (this->FaultyChillerFoulingFlag && (!DataGlobals::WarmupFlag) && (!DataGlobals::DoingSizing) && (!DataGlobals::KickOffSimulation)) {
            int FaultIndex = this->FaultyChillerFoulingIndex;
            Real64 NomCap_ff = ChillerNomCap;
            Real64 COP_ff = COP;

            // calculate the Faulty Chiller Fouling Factor using fault information
            this->FaultyChillerFoulingFactor = FaultsManager::FaultsChillerFouling(FaultIndex).CalFoulingFactor();

            // update the Chiller nominal capacity and COP at faulty cases
            ChillerNomCap = NomCap_ff * this->FaultyChillerFoulingFactor;
            COP = COP_ff * this->FaultyChillerFoulingFactor;
        }

        // If there is a fault of Chiller SWT Sensor (zrp_Jun2016)
        if (this->FaultyChillerSWTFlag && (!DataGlobals::WarmupFlag) && (!DataGlobals::DoingSizing) && (!DataGlobals::KickOffSimulation)) {
            int FaultIndex = this->FaultyChillerSWTIndex;
            Real64 EvapOutletTemp_ff = TempEvapOut;

            // calculate the sensor offset using fault information
            this->FaultyChillerSWTOffset = FaultsManager::FaultsChillerSWTSensor(FaultIndex).CalFaultOffsetAct();
            // update the TempEvapOut
            TempEvapOut = max(this->TempLowLimitEvapOut, min(DataLoopNode::Node(this->EvapInletNodeNum).Temp, EvapOutletTemp_ff - this->FaultyChillerSWTOffset));
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
        Real64 OperPartLoadRat;          // Actual Operating PLR
        if (AvailChillerCap > 0.0) {
            if (std::abs(MyLoad) / AvailChillerCap < this->MinPartLoadRat) {
                OperPartLoadRat = std::abs(MyLoad) / AvailChillerCap;
            } else {
                OperPartLoadRat = PartLoadRat;
            }
        } else {
            OperPartLoadRat = 0.0;
        }

        Real64 Cp = FluidProperties::GetSpecificHeatGlycol(
            DataPlant::PlantLoop(this->CWLoopNum).FluidName, DataLoopNode::Node(this->EvapInletNodeNum).Temp, DataPlant::PlantLoop(this->CWLoopNum).FluidIndex, RoutineName);
        // If FlowLock is True, the new resolved mdot is used to update Power, QEvap, Qcond, and
        // condenser side outlet temperature.
        if (DataPlant::PlantLoop(this->CWLoopNum).LoopSide(this->CWLoopSideNum).FlowLock == 0) {
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
            if ((this->FlowMode == DataPlant::FlowMode::CONSTANT) || (this->FlowMode == DataPlant::FlowMode::NOTMODULATED)) {
                // Start by assuming max (design) flow
                this->EvapMassFlowRate = this->EvapMassFlowRateMax;
                // Use SetComponentFlowRate to decide actual flow
                PlantUtilities::SetComponentFlowRate(
                    this->EvapMassFlowRate, this->EvapInletNodeNum, this->EvapOutletNodeNum, this->CWLoopNum, this->CWLoopSideNum, this->CWBranchNum, this->CWCompNum);
                // Evaluate delta temp based on actual flow rate
                if (this->EvapMassFlowRate != 0.0) {
                    EvapDeltaTemp = this->QEvaporator / this->EvapMassFlowRate / Cp;
                } else {
                    EvapDeltaTemp = 0.0;
                }
                // Evaluate outlet temp based on delta
                this->EvapOutletTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp - EvapDeltaTemp;
            } else if (this->FlowMode == DataPlant::FlowMode::LEAVINGSETPOINTMODULATED) {
                // Calculate the Delta Temp from the inlet temp to the chiller outlet setpoint
                {
                    auto const SELECT_CASE_var(DataPlant::PlantLoop(this->CWLoopNum).LoopDemandCalcScheme);
                    if (SELECT_CASE_var == DataPlant::SingleSetPoint) {
                        EvapDeltaTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp - DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPoint;
                    } else if (SELECT_CASE_var == DataPlant::DualSetPointDeadBand) {
                        EvapDeltaTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp - DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPointHi;
                    }
                }
                if (EvapDeltaTemp != 0.0) {
                    // Calculate desired flow to request based on load
                    this->EvapMassFlowRate = std::abs(this->QEvaporator / Cp / EvapDeltaTemp);
                    if ((this->EvapMassFlowRate - this->EvapMassFlowRateMax) > DataBranchAirLoopPlant::MassFlowTolerance) this->PossibleSubcooling = true;
                    // Check to see if the Maximum is exceeded, if so set to maximum
                    this->EvapMassFlowRate = min(this->EvapMassFlowRateMax, this->EvapMassFlowRate);
                    // Use SetComponentFlowRate to decide actual flow
                    PlantUtilities::SetComponentFlowRate(this->EvapMassFlowRate,
                                                         this->EvapInletNodeNum,
                                                         this->EvapOutletNodeNum,
                                                         this->CWLoopNum,
                                                         this->CWLoopSideNum,
                                                         this->CWBranchNum,
                                                         this->CWCompNum);
                    {
                        auto const SELECT_CASE_var(DataPlant::PlantLoop(this->CWLoopNum).LoopDemandCalcScheme);
                        if (SELECT_CASE_var == DataPlant::SingleSetPoint) {
                            this->EvapOutletTemp = DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPoint;
                        } else if (SELECT_CASE_var == DataPlant::DualSetPointDeadBand) {
                            this->EvapOutletTemp = DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPointHi;
                        }
                    }
                } else {
                    // Try to request zero flow
                    this->EvapMassFlowRate = 0.0;
                    // Use SetComponentFlowRate to decide actual flow
                    PlantUtilities::SetComponentFlowRate(this->EvapMassFlowRate,
                                                         this->EvapInletNodeNum,
                                                         this->EvapOutletNodeNum,
                                                         this->CWLoopNum,
                                                         this->CWLoopSideNum,
                                                         this->CWBranchNum,
                                                         this->CWCompNum);
                    // No deltaT since component is not running
                    this->EvapOutletTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp;
                }
            } // End of Constant Variable Flow If Block

            // If there is a fault of Chiller SWT Sensor (zrp_Jun2016)
            if (this->FaultyChillerSWTFlag && (!DataGlobals::WarmupFlag) && (!DataGlobals::DoingSizing) && (!DataGlobals::KickOffSimulation) &&
                (this->EvapMassFlowRate > 0)) {
                // calculate directly affected variables at faulty case: EvapOutletTemp, EvapMassFlowRate, QEvaporator
                int FaultIndex = this->FaultyChillerSWTIndex;
                bool VarFlowFlag = (this->FlowMode == DataPlant::FlowMode::LEAVINGSETPOINTMODULATED);
                FaultsManager::FaultsChillerSWTSensor(FaultIndex)
                    .CalFaultChillerSWT(VarFlowFlag,
                                        this->FaultyChillerSWTOffset,
                                        Cp,
                                        DataLoopNode::Node(this->EvapInletNodeNum).Temp,
                                        this->EvapOutletTemp,
                                        this->EvapMassFlowRate,
                                        this->QEvaporator);
            }

        } else { // If FlowLock is True

            this->EvapMassFlowRate = DataLoopNode::Node(this->EvapInletNodeNum).MassFlowRate;
            PlantUtilities::SetComponentFlowRate(
                this->EvapMassFlowRate, this->EvapInletNodeNum, this->EvapOutletNodeNum, this->CWLoopNum, this->CWLoopSideNum, this->CWBranchNum, this->CWCompNum);
            //       Some other component set the flow to 0. No reason to continue with calculations.
            if (this->EvapMassFlowRate == 0.0) {
                MyLoad = 0.0;
                if (this->CondenserType == DataPlant::CondenserType::EVAPCOOLED) {
                    CalcBasinHeaterPower(
                        this->BasinHeaterPowerFTempDiff, this->BasinHeaterSchedulePtr, this->BasinHeaterSetPointTemp, this->BasinHeaterPower);
                }
                this->PrintMessage = false;
                return;
            }

            if (this->PossibleSubcooling) {
                this->QEvaporator = std::abs(MyLoad);
                EvapDeltaTemp = this->QEvaporator / this->EvapMassFlowRate / Cp;
                this->EvapOutletTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp - EvapDeltaTemp;
            } else { // No subcooling in this case.No recalculation required.Still need to check chiller low temp limit
                {
                    auto const SELECT_CASE_var(DataPlant::PlantLoop(this->CWLoopNum).LoopDemandCalcScheme);
                    if (SELECT_CASE_var == DataPlant::SingleSetPoint) {
                        if ((this->FlowMode == DataPlant::FlowMode::LEAVINGSETPOINTMODULATED) ||
                            (DataPlant::PlantLoop(this->CWLoopNum).LoopSide(this->CWLoopSideNum).Branch(this->CWBranchNum).Comp(this->CWCompNum).CurOpSchemeType ==
                             DataPlant::CompSetPtBasedSchemeType) ||
                            (DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPoint != DataLoopNode::SensedNodeFlagValue)) {
                            TempEvapOutSetPoint = DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPoint;
                        } else {
                            TempEvapOutSetPoint = DataLoopNode::Node(DataPlant::PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPoint;
                        }
                    } else if (SELECT_CASE_var == DataPlant::DualSetPointDeadBand) {
                        if ((this->FlowMode == DataPlant::FlowMode::LEAVINGSETPOINTMODULATED) ||
                            (DataPlant::PlantLoop(this->CWLoopNum).LoopSide(this->CWLoopSideNum).Branch(this->CWBranchNum).Comp(this->CWCompNum).CurOpSchemeType ==
                             DataPlant::CompSetPtBasedSchemeType) ||
                            (DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPointHi != DataLoopNode::SensedNodeFlagValue)) {
                            TempEvapOutSetPoint = DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPointHi;
                        } else {
                            TempEvapOutSetPoint = DataLoopNode::Node(DataPlant::PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPointHi;
                        }
                    }
                }
                EvapDeltaTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp - TempEvapOutSetPoint;
                this->QEvaporator = std::abs(this->EvapMassFlowRate * Cp * EvapDeltaTemp);
                this->EvapOutletTemp = TempEvapOutSetPoint;
            }
            // Check that the Evap outlet temp honors both plant loop temp low limit and also the chiller low limit
            if (this->EvapOutletTemp < this->TempLowLimitEvapOut) {
                if ((DataLoopNode::Node(this->EvapInletNodeNum).Temp - this->TempLowLimitEvapOut) > DataPlant::DeltaTempTol) {
                    this->EvapOutletTemp = this->TempLowLimitEvapOut;
                    EvapDeltaTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp - this->EvapOutletTemp;
                    this->QEvaporator = this->EvapMassFlowRate * Cp * EvapDeltaTemp;
                } else {
                    this->EvapOutletTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp;
                    EvapDeltaTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp - this->EvapOutletTemp;
                    this->QEvaporator = this->EvapMassFlowRate * Cp * EvapDeltaTemp;
                }
            }
            if (this->EvapOutletTemp < DataLoopNode::Node(this->EvapOutletNodeNum).TempMin) {
                if ((DataLoopNode::Node(this->EvapInletNodeNum).Temp - DataLoopNode::Node(this->EvapOutletNodeNum).TempMin) > DataPlant::DeltaTempTol) {
                    this->EvapOutletTemp = DataLoopNode::Node(this->EvapOutletNodeNum).TempMin;
                    EvapDeltaTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp - this->EvapOutletTemp;
                    this->QEvaporator = this->EvapMassFlowRate * Cp * EvapDeltaTemp;
                } else {
                    this->EvapOutletTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp;
                    EvapDeltaTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp - this->EvapOutletTemp;
                    this->QEvaporator = this->EvapMassFlowRate * Cp * EvapDeltaTemp;
                }
            }
            // If load exceeds the distributed load set to the distributed load
            if (this->QEvaporator > std::abs(MyLoad)) {
                if (this->EvapMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance) {
                    this->QEvaporator = std::abs(MyLoad);
                    EvapDeltaTemp = this->QEvaporator / this->EvapMassFlowRate / Cp;
                    this->EvapOutletTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp - EvapDeltaTemp;
                } else {
                    this->QEvaporator = 0.0;
                    this->EvapOutletTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp;
                }
            }

            // If there is a fault of Chiller SWT Sensor (zrp_Jun2016)
            if (this->FaultyChillerSWTFlag && (!DataGlobals::WarmupFlag) && (!DataGlobals::DoingSizing) && (!DataGlobals::KickOffSimulation) &&
                (this->EvapMassFlowRate > 0)) {
                // calculate directly affected variables at faulty case: EvapOutletTemp, EvapMassFlowRate, QEvaporator
                int FaultIndex = this->FaultyChillerSWTIndex;
                bool VarFlowFlag = false;
                FaultsManager::FaultsChillerSWTSensor(FaultIndex)
                    .CalFaultChillerSWT(VarFlowFlag,
                                        this->FaultyChillerSWTOffset,
                                        Cp,
                                        DataLoopNode::Node(this->EvapInletNodeNum).Temp,
                                        this->EvapOutletTemp,
                                        this->EvapMassFlowRate,
                                        this->QEvaporator);
            }

            // Checks QEvaporator on the basis of the machine limits.
            if (this->QEvaporator > (AvailChillerCap * this->MaxPartLoadRat)) {
                if (this->EvapMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance) {
                    this->QEvaporator = AvailChillerCap * PartLoadRat;
                    EvapDeltaTemp = this->QEvaporator / this->EvapMassFlowRate / Cp;
                    this->EvapOutletTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp - EvapDeltaTemp;
                } else {
                    this->QEvaporator = 0.0;
                    this->EvapOutletTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp;
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
                this->EvapOutletTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp;
                this->Power = 0.0;
                this->PrintMessage = false;
            }
            if (this->QEvaporator == 0.0 && this->CondenserType == DataPlant::CondenserType::EVAPCOOLED) {
                CalcBasinHeaterPower(
                    this->BasinHeaterPowerFTempDiff, this->BasinHeaterSchedulePtr, this->BasinHeaterSetPointTemp, this->BasinHeaterPower);
            }

        } // This is the end of the FlowLock Block

        // Now determine Cooling
        // QCondenser is calculated the same for each type, but the power consumption should be different
        //  depending on the performance coefficients used for the chiller model.
        this->QCondenser = this->Power + this->QEvaporator;

        if (this->CondenserType == DataPlant::CondenserType::WATERCOOLED) {

            if (this->CondMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance) {
                Real64 CpCond = FluidProperties::GetSpecificHeatGlycol(
                    DataPlant::PlantLoop(this->CDLoopNum).FluidName, condInletTemp, DataPlant::PlantLoop(this->CDLoopNum).FluidIndex, RoutineName);
                this->CondOutletTemp = this->QCondenser / this->CondMassFlowRate / CpCond + condInletTemp;
            } else {
                ShowSevereError("CalcGasTurbineChillerModel: Condenser flow = 0, for GasTurbineChiller=" + this->Name);
                ShowContinueErrorTimeStamp("");
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

            Real64 AmbientDeltaT;       // (ATAIR) Difference between ambient actual and ambient design temperatures
            if (this->CondenserType == DataPlant::CondenserType::WATERCOOLED) {
                AmbientDeltaT = DataEnvironment::OutDryBulbTemp - 25.0;
            } else { // air or evap cooled
                AmbientDeltaT = DataLoopNode::Node(this->CondInletNodeNum).OutAirDryBulb - 25.0;
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
                this->ExhaustStackTemp = this->DesignSteamSatTemp + (exhaustTemp - this->DesignSteamSatTemp) /
                                              std::exp(UAtoCapRatLocal / (max(exhaustFlow, this->MaxExhaustperGTPower * this->GTEngineCapacity) * ExhaustCP));
            }

            if (this->HeatRecActive) {
                this->HeatRecLubeRate = PLoad * (this->HeatRecLubeEnergyCoef(1) + this->HeatRecLubeEnergyCoef(2) * RL + this->HeatRecLubeEnergyCoef(3) * RL2);

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
                heatRecMdot = DataLoopNode::Node(this->HeatRecInletNodeNum).MassFlowRate;
                this->HeatRecInletTemp = DataLoopNode::Node(this->HeatRecInletNodeNum).Temp;
                Real64 HeatRecCp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(this->HRLoopNum).FluidName,
                                                                   this->HeatRecInletTemp,
                                                                   DataPlant::PlantLoop(this->HRLoopNum).FluidIndex,
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
        this->HeatRecLubeEnergy = this->HeatRecLubeRate * (DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour);
        this->FuelEnergyIn = std::abs(fuelEnergyIn);
        this->FuelMassUsedRate = std::abs(fuelEnergyIn) / (this->FuelHeatingValue * KJtoJ);

        // Calculate Energy
        this->CondenserEnergy = this->QCondenser * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
        this->Energy = this->Power * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
        this->EvaporatorEnergy = this->QEvaporator * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

        // check for problems BG 9/12/06 (deal with observed negative energy results)
        if (this->Energy < 0.0) { // there is a serious problem

            if (this->CondenserType == DataPlant::CondenserType::WATERCOOLED) {
                // first check for run away condenser loop temps (only reason yet to be observed for this?)
                if (condInletTemp > 70.0) {
                    ShowSevereError("CalcGTChillerModel: Condenser loop inlet temperatures over 70.0 C for GTChiller=" + this->Name);
                    ShowContinueErrorTimeStamp("");
                    ShowContinueError("Condenser loop water temperatures are too high at" + General::RoundSigDigits(condInletTemp, 2));
                    ShowContinueError("Check input for condenser plant loop, especially cooling tower");
                    ShowContinueError("Evaporator inlet temperature: " + General::RoundSigDigits(DataLoopNode::Node(this->EvapInletNodeNum).Temp, 2));

                    ShowFatalError("Program Terminates due to previous error condition");
                }
            }
            if (!DataGlobals::WarmupFlag) {
                if (AvailNomCapRat < 0.0) { // apparently the real reason energy goes negative
                    ShowSevereError("CalcGTChillerModel: Capacity ratio below zero for GTChiller=" + this->Name);
                    ShowContinueErrorTimeStamp("");
                    ShowContinueError("Check input for Capacity Ratio Curve");
                    ShowContinueError("Condenser inlet temperature: " + General::RoundSigDigits(condInletTemp, 2));
                    ShowContinueError("Evaporator inlet temperature: " + General::RoundSigDigits(DataLoopNode::Node(this->EvapInletNodeNum).Temp, 2));
                    ShowFatalError("Program Terminates due to previous error condition");
                }
            }
            // If makes it here, set limits, chiller can't have negative energy/power
            // proceeding silently for now but may want to throw error here
            this->Power = 0.0;
            this->Energy = 0.0;
        }
    }

    void GTChillerSpecs::update(Real64 const MyLoad, bool const RunFlag)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Dan Fisher / Brandon Anderson
        //       DATE WRITTEN:    September 2000

        Real64 const ReportingConstant = DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

        if (MyLoad >= 0.0 || !RunFlag) { // Chiller not running so pass inlet states to outlet states
            // set node temperatures
            DataLoopNode::Node(this->EvapOutletNodeNum).Temp = DataLoopNode::Node(this->EvapInletNodeNum).Temp;
            DataLoopNode::Node(this->CondOutletNodeNum).Temp = DataLoopNode::Node(this->CondInletNodeNum).Temp;

            if (this->HeatRecActive) {
                PlantUtilities::SafeCopyPlantNode(this->HeatRecOutletNodeNum, this->HeatRecInletNodeNum);
                this->HeatRecInletTemp = DataLoopNode::Node(this->HeatRecInletNodeNum).Temp;
                this->HeatRecOutletTemp = DataLoopNode::Node(this->HeatRecOutletNodeNum).Temp;
            }

            this->Power = 0.0;
            this->QEvaporator = 0.0;
            this->QCondenser = 0.0;
            this->Energy = 0.0;
            this->EvaporatorEnergy = 0.0;
            this->CondenserEnergy = 0.0;
            this->EvapInletTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp;
            this->CondInletTemp = DataLoopNode::Node(this->CondInletNodeNum).Temp;
            this->CondOutletTemp = DataLoopNode::Node(this->CondOutletNodeNum).Temp;
            this->EvapOutletTemp = DataLoopNode::Node(this->EvapOutletNodeNum).Temp;
            this->FuelEnergyUsedRate = 0.0;
            this->FuelMassUsedRate = 0.0;
            this->FuelEnergyUsed = 0.0;
            this->FuelMassUsed = 0.0;

            this->HeatRecLubeEnergy = 0.0;
            this->HeatRecLubeRate = 0.0;
            this->ExhaustStackTemp = 0.0;
            this->FuelCOP = 0.0;
            if (this->CondenserType == DataPlant::CondenserType::EVAPCOOLED) {
                this->BasinHeaterPower = this->BasinHeaterPower;
                this->BasinHeaterConsumption = this->BasinHeaterPower * ReportingConstant;
            }

        } else { // Chiller is running so report calculated values
            // set node temperatures
            DataLoopNode::Node(this->EvapOutletNodeNum).Temp = this->EvapOutletTemp;
            DataLoopNode::Node(this->CondOutletNodeNum).Temp = this->CondOutletTemp;

            if (this->HeatRecActive) {
                PlantUtilities::SafeCopyPlantNode(this->HeatRecOutletNodeNum, this->HeatRecInletNodeNum);
                DataLoopNode::Node(this->HeatRecOutletNodeNum).Temp = this->HeatRecOutletTemp;
            }

            this->EvapInletTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp;
            this->CondInletTemp = DataLoopNode::Node(this->CondInletNodeNum).Temp;
            this->CondOutletTemp = DataLoopNode::Node(this->CondOutletNodeNum).Temp;
            this->EvapOutletTemp = DataLoopNode::Node(this->EvapOutletNodeNum).Temp;

            this->FuelEnergyUsedRate = this->FuelEnergyIn;
            this->FuelEnergyUsed = this->FuelEnergyUsedRate * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
            this->FuelMassUsed = this->FuelMassUsedRate * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
            if (this->FuelEnergyUsedRate != 0.0) {
                this->FuelCOP = this->QEvaporator / this->FuelEnergyUsedRate;
            } else {
                this->FuelCOP = 0.0;
            }
            if (this->CondenserType == DataPlant::CondenserType::EVAPCOOLED) {
                this->BasinHeaterPower = this->BasinHeaterPower;
                this->BasinHeaterConsumption = this->BasinHeaterPower * ReportingConstant;
            }
        }
    }

    ConstCOPChillerSpecs *ConstCOPChillerSpecs::factory(PlantChillersData &chillers, std::string const &chillerName)
    {
        // GET INPUT
        if (chillers.GetConstCOPInput) {
            ConstCOPChillerSpecs::getInput(chillers);
            chillers.GetConstCOPInput = false;
        }
        for (auto &thisChiller : chillers.ConstCOPChiller) {
            if (UtilityRoutines::MakeUPPERCase(thisChiller.Name) == chillerName) {
                return &thisChiller;
            }
        }
        ShowFatalError("Could not locate constant COP chiller with name: " + chillerName);
        return nullptr;
    }

    void ConstCOPChillerSpecs::simulate(EnergyPlusData &state, const PlantLocation &calledFromLocation, bool FirstHVACIteration, Real64 &CurLoad, bool RunFlag)
    {
        if (calledFromLocation.loopNum == this->CWLoopNum) {
            this->initialize(state.dataBranchInputManager, RunFlag, CurLoad);
            auto &sim_component(DataPlant::PlantLoop(this->CWLoopNum).LoopSide(this->CWLoopSideNum).Branch(this->CWBranchNum).Comp(this->CWCompNum));
            this->calculate(CurLoad, RunFlag, sim_component.FlowCtrl);
            this->update(CurLoad, RunFlag);
        } else if (calledFromLocation.loopNum == this->CDLoopNum) {
            PlantUtilities::UpdateChillerComponentCondenserSide(this->CDLoopNum,
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

    void ConstCOPChillerSpecs::getInput(PlantChillersData &chillers)
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
        static std::string const RoutineName("GetConstCOPChillerInput: "); // include trailing blank space

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int NumAlphas; // Number of elements in the alpha array
        int NumNums;   // Number of elements in the numeric array
        int IOStat;    // IO Status when calling get input subroutine
        bool ErrorsFound(false);

        // GET NUMBER OF ALL EQUIPMENT TYPES
        DataIPShortCuts::cCurrentModuleObject = "Chiller:ConstantCOP";
        chillers.NumConstCOPChillers = inputProcessor->getNumObjectsFound(DataIPShortCuts::cCurrentModuleObject);

        if (chillers.NumConstCOPChillers <= 0) {
            ShowSevereError("No " + DataIPShortCuts::cCurrentModuleObject + " equipment specified in input file");
            ErrorsFound = true;
        }

        // See if load distribution manager has already gotten the input
        if (allocated(chillers.ConstCOPChiller)) return;

        chillers.ConstCOPChiller.allocate(chillers.NumConstCOPChillers);

        // LOAD ARRAYS WITH BLAST ConstCOP CHILLER DATA
        for (int ChillerNum = 1; ChillerNum <= chillers.NumConstCOPChillers; ++ChillerNum) {
            inputProcessor->getObjectItem(DataIPShortCuts::cCurrentModuleObject,
                                          ChillerNum,
                                          DataIPShortCuts::cAlphaArgs,
                                          NumAlphas,
                                          DataIPShortCuts::rNumericArgs,
                                          NumNums,
                                          IOStat,
                                          _,
                                          DataIPShortCuts::lAlphaFieldBlanks,
                                          DataIPShortCuts::cAlphaFieldNames,
                                          DataIPShortCuts::cNumericFieldNames);
            UtilityRoutines::IsNameEmpty(DataIPShortCuts::cAlphaArgs(1), DataIPShortCuts::cCurrentModuleObject, ErrorsFound);

            // ErrorsFound will be set to True if problem was found, left untouched otherwise
            GlobalNames::VerifyUniqueChillerName(DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), ErrorsFound, DataIPShortCuts::cCurrentModuleObject + " Name");

            auto &thisChiller = chillers.ConstCOPChiller(ChillerNum);
            thisChiller.Name = DataIPShortCuts::cAlphaArgs(1);
            thisChiller.plantTypeOfNum = DataPlant::TypeOf_Chiller_ConstCOP;
            thisChiller.NomCap = DataIPShortCuts::rNumericArgs(1);
            if (thisChiller.NomCap == DataSizing::AutoSize) {
                thisChiller.NomCapWasAutoSized = true;
            }
            if (DataIPShortCuts::rNumericArgs(1) == 0.0) {
                ShowSevereError("Invalid " + DataIPShortCuts::cNumericFieldNames(1) + '=' + General::RoundSigDigits(DataIPShortCuts::rNumericArgs(1), 2));
                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                ErrorsFound = true;
            }
            thisChiller.COP = DataIPShortCuts::rNumericArgs(2);
            if (DataIPShortCuts::rNumericArgs(2) == 0.0) {
                ShowSevereError("Invalid " + DataIPShortCuts::cNumericFieldNames(2) + '=' + General::RoundSigDigits(DataIPShortCuts::rNumericArgs(2), 2));
                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                ErrorsFound = true;
            }

            // Set the Condenser Type from input
            if (DataIPShortCuts::cAlphaArgs(6) == "AIRCOOLED") {
                thisChiller.CondenserType = DataPlant::CondenserType::AIRCOOLED;
            } else if (DataIPShortCuts::cAlphaArgs(6) == "EVAPORATIVELYCOOLED") {
                thisChiller.CondenserType = DataPlant::CondenserType::EVAPCOOLED;
            } else if (DataIPShortCuts::cAlphaArgs(6) == "WATERCOOLED") {
                thisChiller.CondenserType = DataPlant::CondenserType::WATERCOOLED;
            } else {
                ShowSevereError("Invalid " + DataIPShortCuts::cAlphaFieldNames(6) + '=' + DataIPShortCuts::cAlphaArgs(6));
                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                ErrorsFound = true;
            }

            thisChiller.EvapVolFlowRate = DataIPShortCuts::rNumericArgs(3);
            if (thisChiller.EvapVolFlowRate == DataSizing::AutoSize) {
                thisChiller.EvapVolFlowRateWasAutoSized = true;
            }
            if (thisChiller.CondenserType == DataPlant::CondenserType::AIRCOOLED ||
                thisChiller.CondenserType == DataPlant::CondenserType::EVAPCOOLED) { // Condenser flow rate not used for these cond types
                thisChiller.CondVolFlowRate = 0.0011;
            } else {
                thisChiller.CondVolFlowRate = DataIPShortCuts::rNumericArgs(4);
                if (thisChiller.CondVolFlowRate == DataSizing::AutoSize) {
                    if (thisChiller.CondenserType == DataPlant::CondenserType::WATERCOOLED) {
                        thisChiller.CondVolFlowRateWasAutoSized = true;
                    }
                }
            }
            thisChiller.SizFac = DataIPShortCuts::rNumericArgs(5);

            thisChiller.EvapInletNodeNum = NodeInputManager::GetOnlySingleNode(
                DataIPShortCuts::cAlphaArgs(2), ErrorsFound, DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Inlet, 1, DataLoopNode::ObjectIsNotParent);
            thisChiller.EvapOutletNodeNum = NodeInputManager::GetOnlySingleNode(
                DataIPShortCuts::cAlphaArgs(3), ErrorsFound, DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Outlet, 1, DataLoopNode::ObjectIsNotParent);
            BranchNodeConnections::TestCompSet(DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataIPShortCuts::cAlphaArgs(2), DataIPShortCuts::cAlphaArgs(3), "Chilled Water Nodes");

            if (thisChiller.CondenserType == DataPlant::CondenserType::AIRCOOLED || thisChiller.CondenserType == DataPlant::CondenserType::EVAPCOOLED) {
                // Connection not required for air or evap cooled condenser
                // If the condenser inlet is blank for air cooled and evap cooled condensers then supply a generic name
                //  since it is not used elsewhere for connection
                if (DataIPShortCuts::lAlphaFieldBlanks(4)) {
                    if (len(DataIPShortCuts::cAlphaArgs(1)) < DataGlobals::MaxNameLength - 21) { // protect against long name leading to > 100 chars
                        DataIPShortCuts::cAlphaArgs(4) = DataIPShortCuts::cAlphaArgs(1) + " CONDENSER INLET NODE";
                    } else {
                        DataIPShortCuts::cAlphaArgs(4) = DataIPShortCuts::cAlphaArgs(1).substr(0, 79) + " CONDENSER INLET NODE";
                    }
                }
                if (DataIPShortCuts::lAlphaFieldBlanks(5)) {
                    if (len(DataIPShortCuts::cAlphaArgs(1)) < DataGlobals::MaxNameLength - 22) { // protect against long name leading to > 100 chars
                        DataIPShortCuts::cAlphaArgs(5) = DataIPShortCuts::cAlphaArgs(1) + " CONDENSER OUTLET NODE";
                    } else {
                        DataIPShortCuts::cAlphaArgs(5) = DataIPShortCuts::cAlphaArgs(1).substr(0, 78) + " CONDENSER OUTLET NODE";
                    }
                }

                thisChiller.CondInletNodeNum = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(4),
                                                                                 ErrorsFound,
                                                                                 DataIPShortCuts::cCurrentModuleObject,
                                                                                 DataIPShortCuts::cAlphaArgs(1),
                                                                                 DataLoopNode::NodeType_Air,
                                                                                 DataLoopNode::NodeConnectionType_OutsideAirReference,
                                                                                 2,
                                                                                 DataLoopNode::ObjectIsNotParent);
                bool Okay;
                OutAirNodeManager::CheckAndAddAirNodeNumber(thisChiller.CondInletNodeNum, Okay);
                if (!Okay) {
                    ShowWarningError(DataIPShortCuts::cCurrentModuleObject + ", Adding OutdoorAir:DataLoopNode::Node=" + DataIPShortCuts::cAlphaArgs(4));
                }

                thisChiller.CondOutletNodeNum = NodeInputManager::GetOnlySingleNode(
                    DataIPShortCuts::cAlphaArgs(5), ErrorsFound, DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataLoopNode::NodeType_Air, DataLoopNode::NodeConnectionType_Outlet, 2, DataLoopNode::ObjectIsNotParent);
            } else if (thisChiller.CondenserType == DataPlant::CondenserType::WATERCOOLED) {
                thisChiller.CondInletNodeNum = NodeInputManager::GetOnlySingleNode(
                    DataIPShortCuts::cAlphaArgs(4), ErrorsFound, DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Inlet, 2, DataLoopNode::ObjectIsNotParent);
                thisChiller.CondOutletNodeNum = NodeInputManager::GetOnlySingleNode(
                    DataIPShortCuts::cAlphaArgs(5), ErrorsFound, DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Outlet, 2, DataLoopNode::ObjectIsNotParent);
                BranchNodeConnections::TestCompSet(DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataIPShortCuts::cAlphaArgs(4), DataIPShortCuts::cAlphaArgs(5), "Condenser Water Nodes");
                // Condenser Inlet node name is necessary for Water Cooled
                if (DataIPShortCuts::lAlphaFieldBlanks(4)) {
                    ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(4) + "is blank ");
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                    ErrorsFound = true;
                } else if (DataIPShortCuts::lAlphaFieldBlanks(5)) {
                    ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(5) + "is blank ");
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                    ErrorsFound = true;
                }
            } else {
                thisChiller.CondInletNodeNum = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(4),
                                                                                 ErrorsFound,
                                                                                 DataIPShortCuts::cCurrentModuleObject,
                                                                                 DataIPShortCuts::cAlphaArgs(1),
                                                                                 DataLoopNode::NodeType_Unknown,
                                                                                 DataLoopNode::NodeConnectionType_Inlet,
                                                                                 2,
                                                                                 DataLoopNode::ObjectIsNotParent);
                thisChiller.CondOutletNodeNum = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(5),
                                                                                  ErrorsFound,
                                                                                  DataIPShortCuts::cCurrentModuleObject,
                                                                                  DataIPShortCuts::cAlphaArgs(1),
                                                                                  DataLoopNode::NodeType_Unknown,
                                                                                  DataLoopNode::NodeConnectionType_Outlet,
                                                                                  2,
                                                                                  DataLoopNode::ObjectIsNotParent);
                BranchNodeConnections::TestCompSet(DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataIPShortCuts::cAlphaArgs(4), DataIPShortCuts::cAlphaArgs(5), "Condenser (unknown?) Nodes");
                // Condenser Inlet node name is necessary for Water Cooled
                if (DataIPShortCuts::lAlphaFieldBlanks(4)) {
                    ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(4) + "is blank ");
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                    ErrorsFound = true;
                } else if (DataIPShortCuts::lAlphaFieldBlanks(5)) {
                    ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(5) + "is blank ");
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                    ErrorsFound = true;
                }
            }

            {
                auto const SELECT_CASE_var(DataIPShortCuts::cAlphaArgs(7));
                if (SELECT_CASE_var == "CONSTANTFLOW") {
                    thisChiller.FlowMode = DataPlant::FlowMode::CONSTANT;
                } else if (SELECT_CASE_var == "LEAVINGSETPOINTMODULATED") {
                    thisChiller.FlowMode = DataPlant::FlowMode::LEAVINGSETPOINTMODULATED;
                } else if (SELECT_CASE_var == "NOTMODULATED") {
                    thisChiller.FlowMode = DataPlant::FlowMode::NOTMODULATED;
                } else {
                    ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\",");
                    ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(7) + '=' + DataIPShortCuts::cAlphaArgs(7));
                    ShowContinueError("Available choices are ConstantFlow, NotModulated, or LeavingSetpointModulated");
                    ShowContinueError("Flow mode NotModulated is assumed and the simulation continues.");
                    thisChiller.FlowMode = DataPlant::FlowMode::NOTMODULATED;
                }
            }

            //   Basin heater power as a function of temperature must be greater than or equal to 0
            thisChiller.BasinHeaterPowerFTempDiff = DataIPShortCuts::rNumericArgs(6);
            if (DataIPShortCuts::rNumericArgs(6) < 0.0) {
                ShowSevereError(DataIPShortCuts::cCurrentModuleObject + ", \"" + thisChiller.Name + "\" TRIM(DataIPShortCuts::cNumericFieldNames(6)) must be >= 0");
                ErrorsFound = true;
            }

            thisChiller.BasinHeaterSetPointTemp = DataIPShortCuts::rNumericArgs(7);

            if (thisChiller.BasinHeaterPowerFTempDiff > 0.0) {
                if (NumNums < 7) {
                    thisChiller.BasinHeaterSetPointTemp = 2.0;
                }
                if (thisChiller.BasinHeaterSetPointTemp < 2.0) {
                    ShowWarningError(DataIPShortCuts::cCurrentModuleObject + ":\"" + thisChiller.Name + "\", " + DataIPShortCuts::cNumericFieldNames(7) +
                                     " is less than 2 deg C. Freezing could occur.");
                }
            }

            if (!DataIPShortCuts::lAlphaFieldBlanks(8)) {
                thisChiller.BasinHeaterSchedulePtr = ScheduleManager::GetScheduleIndex(DataIPShortCuts::cAlphaArgs(8));
                if (thisChiller.BasinHeaterSchedulePtr == 0) {
                    ShowWarningError(DataIPShortCuts::cCurrentModuleObject + ", \"" + thisChiller.Name + "\" TRIM(DataIPShortCuts::cAlphaFieldNames(8)) \"" +
                                     DataIPShortCuts::cAlphaArgs(8) + "\" was not found. Basin heater operation will not be modeled and the simulation continues");
                }
            }
        }

        if (ErrorsFound) {
            ShowFatalError("Errors found in processing input for " + DataIPShortCuts::cCurrentModuleObject);
        }
    }

    void ConstCOPChillerSpecs::setupOutputVariables()
    {
        SetupOutputVariable("Chiller Electricity Rate", OutputProcessor::Unit::W, this->Power, "System", "Average", this->Name);
        SetupOutputVariable("Chiller Electricity Energy", OutputProcessor::Unit::J, this->Energy, "System", "Sum", this->Name, _, "ELECTRICITY", "Cooling", _, "Plant");
        SetupOutputVariable("Chiller Evaporator Cooling Rate", OutputProcessor::Unit::W, this->QEvaporator, "System", "Average", this->Name);
        SetupOutputVariable("Chiller Evaporator Cooling Energy",
                            OutputProcessor::Unit::J,
                            this->EvaporatorEnergy,
                            "System",
                            "Sum",
                            this->Name,
                            _,
                            "ENERGYTRANSFER",
                            "CHILLERS",
                            _,
                            "Plant");
        SetupOutputVariable("Chiller Evaporator Inlet Temperature", OutputProcessor::Unit::C, this->EvapInletTemp, "System", "Average", this->Name);
        SetupOutputVariable("Chiller Evaporator Outlet Temperature", OutputProcessor::Unit::C, this->EvapOutletTemp, "System", "Average", this->Name);
        SetupOutputVariable("Chiller Evaporator Mass Flow Rate", OutputProcessor::Unit::kg_s, this->EvapMassFlowRate, "System", "Average", this->Name);
        SetupOutputVariable("Chiller COP", OutputProcessor::Unit::W_W, this->ActualCOP, "System", "Average", this->Name);
        SetupOutputVariable("Chiller Condenser Heat Transfer Rate", OutputProcessor::Unit::W, this->QCondenser, "System", "Average", this->Name);
        SetupOutputVariable("Chiller Condenser Heat Transfer Energy",
                            OutputProcessor::Unit::J,
                            this->CondenserEnergy,
                            "System",
                            "Sum",
                            this->Name,
                            _,
                            "ENERGYTRANSFER",
                            "HEATREJECTION",
                            _,
                            "Plant");

        SetupOutputVariable("Chiller Condenser Inlet Temperature", OutputProcessor::Unit::C, this->CondInletTemp, "System", "Average", this->Name);

        // Condenser mass flow and outlet temp are valid for water cooled
        if (this->CondenserType == DataPlant::CondenserType::WATERCOOLED) {
            SetupOutputVariable("Chiller Condenser Outlet Temperature", OutputProcessor::Unit::C, this->CondOutletTemp, "System", "Average", this->Name);
            SetupOutputVariable("Chiller Condenser Mass Flow Rate", OutputProcessor::Unit::kg_s, this->CondMassFlowRate, "System", "Average", this->Name);
        } else if (this->CondenserType == DataPlant::CondenserType::AIRCOOLED) {
        } else if (this->CondenserType == DataPlant::CondenserType::EVAPCOOLED) {
            if (this->BasinHeaterPowerFTempDiff > 0.0) {
                SetupOutputVariable("Chiller Basin Heater Electricity Rate", OutputProcessor::Unit::W, this->BasinHeaterPower, "System", "Average", this->Name);
                SetupOutputVariable("Chiller Basin Heater Electricity Energy",
                                    OutputProcessor::Unit::J,
                                    this->BasinHeaterConsumption,
                                    "System",
                                    "Sum",
                                    this->Name,
                                    _,
                                    "Electricity",
                                    "CHILLERS",
                                    _,
                                    "Plant");
            }
        }
        if (DataGlobals::AnyEnergyManagementSystemInModel) {
            SetupEMSInternalVariable("Chiller Nominal Capacity", this->Name, "[W]", this->NomCap);
        }
    }

    void ConstCOPChillerSpecs::initialize(BranchInputManagerData &dataBranchInputManager, bool const RunFlag, Real64 const MyLoad)
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
        static std::string const RoutineName("InitConstCOPChiller");
        Real64 const TempDesCondIn(25.0); // Design condenser inlet temp. C

        // Init more variables
        if (this->MyFlag) {
            // Locate the chillers on the plant loops for later usage
            bool errFlag = false;
            this->setupOutputVariables();
            PlantUtilities::ScanPlantLoopsForObject(dataBranchInputManager,
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
            if (this->CondenserType != DataPlant::CondenserType::AIRCOOLED && this->CondenserType != DataPlant::CondenserType::EVAPCOOLED) {
                PlantUtilities::ScanPlantLoopsForObject(dataBranchInputManager,
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
                    this->CWLoopNum, this->CWLoopSideNum, this->CDLoopNum, this->CDLoopSideNum, this->plantTypeOfNum, true);
            }

            if (errFlag) {
                ShowFatalError("CalcConstCOPChillerModel: Program terminated due to previous condition(s).");
            }
            if (this->FlowMode == DataPlant::FlowMode::CONSTANT) {
                // reset flow priority
                DataPlant::PlantLoop(this->CWLoopNum).LoopSide(this->CWLoopSideNum).Branch(this->CWBranchNum).Comp(this->CWCompNum).FlowPriority =
                    DataPlant::LoopFlowStatus_NeedyIfLoopOn;
            }

            if (this->FlowMode == DataPlant::FlowMode::LEAVINGSETPOINTMODULATED) {
                // reset flow priority
                DataPlant::PlantLoop(this->CWLoopNum).LoopSide(this->CWLoopSideNum).Branch(this->CWBranchNum).Comp(this->CWCompNum).FlowPriority =
                    DataPlant::LoopFlowStatus_NeedyIfLoopOn;

                // check if setpoint on outlet node
                if ((DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPoint == DataLoopNode::SensedNodeFlagValue) &&
                    (DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPointHi == DataLoopNode::SensedNodeFlagValue)) {
                    if (!DataGlobals::AnyEnergyManagementSystemInModel) {
                        if (!this->ModulatedFlowErrDone) {
                            ShowWarningError("Missing temperature setpoint for LeavingSetpointModulated mode chiller named " + this->Name);
                            ShowContinueError(
                                "  A temperature setpoint is needed at the outlet node of a chiller in variable flow mode, use a SetpointManager");
                            ShowContinueError("  The overall loop setpoint will be assumed for chiller. The simulation continues ... ");
                            this->ModulatedFlowErrDone = true;
                        }
                    } else {
                        // need call to EMS to check node
                        bool FatalError = false; // but not really fatal yet, but should be.
                        EMSManager::CheckIfNodeSetPointManagedByEMS(this->EvapOutletNodeNum, EMSManager::iTemperatureSetPoint, FatalError);
                        if (FatalError) {
                            if (!this->ModulatedFlowErrDone) {
                                ShowWarningError("Missing temperature setpoint for LeavingSetpointModulated mode chiller named " + this->Name);
                                ShowContinueError(
                                    "  A temperature setpoint is needed at the outlet node of a chiller evaporator in variable flow mode");
                                ShowContinueError("  use a Setpoint Manager to establish a setpoint at the chiller evaporator outlet node ");
                                ShowContinueError("  or use an EMS actuator to establish a setpoint at the outlet node ");
                                ShowContinueError("  The overall loop setpoint will be assumed for chiller. The simulation continues ... ");
                                this->ModulatedFlowErrDone = true;
                            }
                        }
                    }
                    this->ModulatedFlowSetToLoop = true;
                    DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPoint = DataLoopNode::Node(DataPlant::PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPoint;
                    DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPointHi = DataLoopNode::Node(DataPlant::PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPointHi;
                }
            }
            this->MyFlag = false;
        }

        // Initialize critical Demand Side Variables at the beginning of each environment
        if (this->MyEnvrnFlag && DataGlobals::BeginEnvrnFlag && (DataPlant::PlantFirstSizesOkayToFinalize)) {

            Real64 rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->CWLoopNum).FluidName,
                                                    DataGlobals::CWInitConvTemp,
                                                    DataPlant::PlantLoop(this->CWLoopNum).FluidIndex,
                                                    RoutineName);
            this->EvapMassFlowRateMax = this->EvapVolFlowRate * rho;
            PlantUtilities::InitComponentNodes(0.0,
                                               this->EvapMassFlowRateMax,
                                               this->EvapInletNodeNum,
                                               this->EvapOutletNodeNum,
                                               this->CWLoopNum,
                                               this->CWLoopSideNum,
                                               this->CWBranchNum,
                                               this->CWCompNum);

            // init maximum available condenser flow rate
            if (this->CondenserType == DataPlant::CondenserType::WATERCOOLED) {

                DataLoopNode::Node(this->CondInletNodeNum).Temp = TempDesCondIn;

                rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->CDLoopNum).FluidName,
                                                        DataGlobals::CWInitConvTemp,
                                                        DataPlant::PlantLoop(this->CDLoopNum).FluidIndex,
                                                        RoutineName);

                this->CondMassFlowRateMax = rho * this->CondVolFlowRate;

                PlantUtilities::InitComponentNodes(0.0,
                                                   this->CondMassFlowRateMax,
                                                   this->CondInletNodeNum,
                                                   this->CondOutletNodeNum,
                                                   this->CDLoopNum,
                                                   this->CDLoopSideNum,
                                                   this->CDBranchNum,
                                                   this->CDCompNum);
            } else { // air or evap-air
                DataLoopNode::Node(this->CondInletNodeNum).MassFlowRate =
                    this->CondVolFlowRate * Psychrometrics::PsyRhoAirFnPbTdbW(DataEnvironment::StdBaroPress, TempDesCondIn, 0.0, RoutineName);

                DataLoopNode::Node(this->CondOutletNodeNum).MassFlowRate = DataLoopNode::Node(this->CondInletNodeNum).MassFlowRate;
                DataLoopNode::Node(this->CondInletNodeNum).MassFlowRateMaxAvail = DataLoopNode::Node(this->CondInletNodeNum).MassFlowRate;
                DataLoopNode::Node(this->CondInletNodeNum).MassFlowRateMax = DataLoopNode::Node(this->CondInletNodeNum).MassFlowRate;
                DataLoopNode::Node(this->CondOutletNodeNum).MassFlowRateMax = DataLoopNode::Node(this->CondInletNodeNum).MassFlowRate;
                DataLoopNode::Node(this->CondInletNodeNum).MassFlowRateMinAvail = 0.0;
                DataLoopNode::Node(this->CondInletNodeNum).MassFlowRateMin = 0.0;
                DataLoopNode::Node(this->CondOutletNodeNum).MassFlowRateMinAvail = 0.0;
                DataLoopNode::Node(this->CondOutletNodeNum).MassFlowRateMin = 0.0;
            }
            this->MyEnvrnFlag = false;
        }

        if (!DataGlobals::BeginEnvrnFlag) {
            this->MyEnvrnFlag = true;
        }
        if ((this->FlowMode == DataPlant::FlowMode::LEAVINGSETPOINTMODULATED) && (this->ModulatedFlowSetToLoop)) {
            // fix for clumsy old input that worked because loop setpoint was spread.
            //  could be removed with transition, testing , model change, period of being obsolete.
            DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPoint = DataLoopNode::Node(DataPlant::PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPoint;
            DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPointHi = DataLoopNode::Node(DataPlant::PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPointHi;
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
            mdot, this->EvapInletNodeNum, this->EvapOutletNodeNum, this->CWLoopNum, this->CWLoopSideNum, this->CWBranchNum, this->CWCompNum);
        if (this->CondenserType == DataPlant::CondenserType::WATERCOOLED) {
            PlantUtilities::SetComponentFlowRate(
                mdotCond, this->CondInletNodeNum, this->CondOutletNodeNum, this->CDLoopNum, this->CDLoopSideNum, this->CDBranchNum, this->CDCompNum);
        }

        if (this->CondenserType == DataPlant::CondenserType::EVAPCOOLED) {
            this->BasinHeaterPower = 0.0;
        }
    }

    void ConstCOPChillerSpecs::size()
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
        static std::string const RoutineName("SizeConstCOPChiller");

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

        if (this->CondenserType == DataPlant::CondenserType::WATERCOOLED) {
            PltSizCondNum = DataPlant::PlantLoop(this->CDLoopNum).PlantSizNum;
        }

        int PltSizNum = DataPlant::PlantLoop(this->CWLoopNum).PlantSizNum;

        if (PltSizNum > 0) {
            if (DataSizing::PlantSizData(PltSizNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                Real64 rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->CWLoopNum).FluidName,
                                                        DataGlobals::CWInitConvTemp,
                                                        DataPlant::PlantLoop(this->CWLoopNum).FluidIndex,
                                                        RoutineName);
                Real64 Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(this->CWLoopNum).FluidName,
                                                            DataGlobals::CWInitConvTemp,
                                                            DataPlant::PlantLoop(this->CWLoopNum).FluidIndex,
                                                            RoutineName);
                tmpNomCap = Cp * rho * DataSizing::PlantSizData(PltSizNum).DeltaT * DataSizing::PlantSizData(PltSizNum).DesVolFlowRate * this->SizFac;
            } else {
                if (this->NomCapWasAutoSized) tmpNomCap = 0.0;
            }
            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                if (this->NomCapWasAutoSized) {
                    this->NomCap = tmpNomCap;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput("Chiller:ConstantCOP", this->Name, "Design Size Nominal Capacity [W]", tmpNomCap);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(
                            "Chiller:ConstantCOP", this->Name, "Initial Design Size Nominal Capacity [W]", tmpNomCap);
                    }
                } else { // Hard-size with sizing data
                    if (this->NomCap > 0.0 && tmpNomCap > 0.0) {
                        NomCapUser = this->NomCap;
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput("Chiller:ConstantCOP",
                                                                    this->Name,
                                                                    "Design Size Nominal Capacity [W]",
                                                                    tmpNomCap,
                                                                    "User-Specified Nominal Capacity [W]",
                                                                    NomCapUser);
                            if (DataGlobals::DisplayExtraWarnings) {
                                if ((std::abs(tmpNomCap - NomCapUser) / NomCapUser) > DataSizing::AutoVsHardSizingThreshold) {
                                    ShowMessage("SizeChillerConstantCOP: Potential issue with equipment sizing for " + this->Name);
                                    ShowContinueError("User-Specified Nominal Capacity of " + General::RoundSigDigits(NomCapUser, 2) + " [W]");
                                    ShowContinueError("differs from Design Size Nominal Capacity of " + General::RoundSigDigits(tmpNomCap, 2) + " [W]");
                                    ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                        }
                        tmpNomCap = NomCapUser;
                    }
                }
            }
        } else {
            if (this->NomCapWasAutoSized && DataPlant::PlantFirstSizesOkayToFinalize) {
                ShowSevereError("Autosizing of Constant COP Chiller nominal capacity requires a loop Sizing:Plant object");
                ShowContinueError("Occurs in Chiller:ConstantCOP object=" + this->Name);
                ErrorsFound = true;
            }
            if (!this->NomCapWasAutoSized && DataPlant::PlantFinalSizesOkayToReport && (this->NomCap > 0.0)) {
                ReportSizingManager::ReportSizingOutput("Chiller:ConstantCOP", this->Name, "User-Specified Nominal Capacity [W]", this->NomCap);
            }
        }

        if (PltSizNum > 0) {
            if (DataSizing::PlantSizData(PltSizNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                tmpEvapVolFlowRate = DataSizing::PlantSizData(PltSizNum).DesVolFlowRate * this->SizFac;
            } else {
                if (this->EvapVolFlowRateWasAutoSized) tmpEvapVolFlowRate = 0.0;
            }
            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                if (this->EvapVolFlowRateWasAutoSized) {
                    this->EvapVolFlowRate = tmpEvapVolFlowRate;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(
                            "Chiller:ConstantCOP", this->Name, "Design Size Design Chilled Water Flow Rate [m3/s]", tmpEvapVolFlowRate);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(
                            "Chiller:ConstantCOP", this->Name, "Initial Design Size Design Chilled Water Flow Rate [m3/s]", tmpEvapVolFlowRate);
                    }
                } else {
                    if (this->EvapVolFlowRate > 0.0 && tmpEvapVolFlowRate > 0.0) {
                        EvapVolFlowRateUser = this->EvapVolFlowRate;
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput("Chiller:ConstantCOP",
                                                                    this->Name,
                                                                    "Design Size Design Chilled Water Flow Rate [m3/s]",
                                                                    tmpEvapVolFlowRate,
                                                                    "User-Specified Design Chilled Water Flow Rate [m3/s]",
                                                                    EvapVolFlowRateUser);
                            if (DataGlobals::DisplayExtraWarnings) {
                                if ((std::abs(tmpEvapVolFlowRate - EvapVolFlowRateUser) / EvapVolFlowRateUser) >
                                    DataSizing::AutoVsHardSizingThreshold) {
                                    ShowMessage("SizeChillerConstantCOP: Potential issue with equipment sizing for " + this->Name);
                                    ShowContinueError("User-Specified Design Chilled Water Flow Rate of " + General::RoundSigDigits(EvapVolFlowRateUser, 5) +
                                                      " [m3/s]");
                                    ShowContinueError("differs from Design Size Design Chilled Water Flow Rate of " +
                                                      General::RoundSigDigits(tmpEvapVolFlowRate, 5) + " [m3/s]");
                                    ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                        }
                        tmpEvapVolFlowRate = EvapVolFlowRateUser;
                    }
                }
            }
        } else {
            if (this->EvapVolFlowRateWasAutoSized && DataPlant::PlantFirstSizesOkayToFinalize) {
                ShowSevereError("Autosizing of Constant COP Chiller evap flow rate requires a loop Sizing:Plant object");
                ShowContinueError("Occurs in Chiller:ConstantCOP object=" + this->Name);
                ErrorsFound = true;
            }
            if (!this->EvapVolFlowRateWasAutoSized && DataPlant::PlantFinalSizesOkayToReport && (this->EvapVolFlowRate > 0.0)) {
                ReportSizingManager::ReportSizingOutput(
                    "Chiller:ConstantCOP", this->Name, "User-Specified Design Chilled Water Flow Rate [m3/s]", this->EvapVolFlowRate);
            }
        }

        PlantUtilities::RegisterPlantCompDesignFlow(this->EvapInletNodeNum, tmpEvapVolFlowRate);

        if (this->CondenserType == DataPlant::CondenserType::WATERCOOLED) {
            if (PltSizCondNum > 0 && PltSizNum > 0) {
                if (DataSizing::PlantSizData(PltSizNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow && tmpNomCap > 0.0) {
                    Real64 rho = FluidProperties::GetDensityGlycol(
                        DataPlant::PlantLoop(this->CDLoopNum).FluidName, 29.44, DataPlant::PlantLoop(this->CDLoopNum).FluidIndex, RoutineName);
                    Real64 Cp = FluidProperties::GetSpecificHeatGlycol(
                        DataPlant::PlantLoop(this->CDLoopNum).FluidName, 29.44, DataPlant::PlantLoop(this->CDLoopNum).FluidIndex, RoutineName);
                    tmpCondVolFlowRate = tmpNomCap * (1.0 + 1.0 / this->COP) / (DataSizing::PlantSizData(PltSizCondNum).DeltaT * Cp * rho);
                } else {
                    if (this->CondVolFlowRateWasAutoSized) tmpCondVolFlowRate = 0.0;
                }
                if (DataPlant::PlantFirstSizesOkayToFinalize) {
                    if (this->CondVolFlowRateWasAutoSized) {
                        this->CondVolFlowRate = tmpCondVolFlowRate;
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput(
                                "Chiller:ConstantCOP", this->Name, "Design Size Design Condenser Water Flow Rate [m3/s]", tmpCondVolFlowRate);
                        }
                        if (DataPlant::PlantFirstSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput(
                                "Chiller:ConstantCOP", this->Name, "Initial Design Size Design Condenser Water Flow Rate [m3/s]", tmpCondVolFlowRate);
                        }
                    } else {
                        if (this->CondVolFlowRate > 0.0 && tmpCondVolFlowRate > 0.0) {
                            Real64 CondVolFlowRateUser = this->CondVolFlowRate;
                            if (DataPlant::PlantFinalSizesOkayToReport) {
                                ReportSizingManager::ReportSizingOutput("Chiller:ConstantCOP",
                                                                        this->Name,
                                                                        "Design Size Design Condenser Water Flow Rate [m3/s]",
                                                                        tmpCondVolFlowRate,
                                                                        "User-Specified Design Condenser Water Flow Rate [m3/s]",
                                                                        CondVolFlowRateUser);
                                if (DataGlobals::DisplayExtraWarnings) {
                                    if ((std::abs(tmpCondVolFlowRate - CondVolFlowRateUser) / CondVolFlowRateUser) >
                                        DataSizing::AutoVsHardSizingThreshold) {
                                        ShowMessage("SizeChillerConstantCOP: Potential issue with equipment sizing for " + this->Name);
                                        ShowContinueError("User-Specified Design Condenser Water Flow Rate of " +
                                                          General::RoundSigDigits(CondVolFlowRateUser, 5) + " [m3/s]");
                                        ShowContinueError("differs from Design Size Design Condenser Water Flow Rate of " +
                                                          General::RoundSigDigits(tmpCondVolFlowRate, 5) + " [m3/s]");
                                        ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                                        ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                                    }
                                }
                            }
                            tmpCondVolFlowRate = CondVolFlowRateUser;
                        }
                    }
                }
            } else {
                if (this->CondVolFlowRateWasAutoSized && DataPlant::PlantFirstSizesOkayToFinalize) {
                    ShowSevereError("Autosizing of Constant COP Chiller condenser flow rate requires a condenser");
                    ShowContinueError("loop Sizing:Plant object");
                    ShowContinueError("Occurs in Chiller:ConstantCOP object=" + this->Name);
                    ErrorsFound = true;
                }
                if (!this->CondVolFlowRateWasAutoSized && DataPlant::PlantFinalSizesOkayToReport && (this->CondVolFlowRate > 0.0)) {
                    ReportSizingManager::ReportSizingOutput(
                        "Chiller:ConstantCOP", this->Name, "User-Specified Design Condenser Water Flow Rate [m3/s]", this->CondVolFlowRate);
                }
            }
        }

        // save the design condenser water volumetric flow rate for use by the condenser water loop sizing algorithms
        if (this->CondenserType == DataPlant::CondenserType::WATERCOOLED) PlantUtilities::RegisterPlantCompDesignFlow(this->CondInletNodeNum, tmpCondVolFlowRate);

        if (ErrorsFound) {
            ShowFatalError("Preceding sizing errors cause program termination");
        }

        // create predefined report
        if (DataPlant::PlantFinalSizesOkayToReport) {
            OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchMechType, this->Name, "Chiller:ConstantCOP");
            OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchMechNomEff, this->Name, this->COP);
            OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchMechNomCap, this->Name, this->NomCap);
        }
    }

    void ConstCOPChillerSpecs::calculate(Real64 &MyLoad, bool const RunFlag, int const EquipFlowCtrl)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Dan Fisher
        //       DATE WRITTEN   Sept. 1998
        //       MODIFIED       Nov.-Dec. 2001, Jan. 2002, Richard Liesen
        //                      Feb. 2010, Chandan Sharma, FSEC. Added basin heater
        //                      Jun. 2016, Rongpeng Zhang, LBNL. Applied the chiller supply water temperature sensor fault model
        //                      Nov. 2016, Rongpeng Zhang, LBNL. Added Fouling Chiller fault

        static std::string const RoutineName("CalcConstCOPChillerModel");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 EvapDeltaTemp;
        Real64 TempEvapOutSetPoint(0.0); // C - evaporator outlet temperature setpoint
        Real64 CurrentEndTime;  // end time of time step for current simulation time step
        Real64 COP;             // coefficient of performance
        Real64 Cp;              // local for fluid specif heat, for evaporator
        Real64 CpCond;          // local for fluid specif heat, for condenser
        Real64 ChillerNomCap;   // chiller nominal capacity

        ChillerNomCap = this->NomCap;
        COP = this->COP;

        // If there is a fault of chiller fouling
        if (this->FaultyChillerFoulingFlag && (!DataGlobals::WarmupFlag) && (!DataGlobals::DoingSizing) && (!DataGlobals::KickOffSimulation)) {
            int FaultIndex = this->FaultyChillerFoulingIndex;
            Real64 NomCap_ff = ChillerNomCap;
            Real64 COP_ff = COP;

            // calculate the Faulty Chiller Fouling Factor using fault information
            this->FaultyChillerFoulingFactor = FaultsManager::FaultsChillerFouling(FaultIndex).CalFoulingFactor();

            // update the Chiller nominal capacity and COP at faulty cases
            ChillerNomCap = NomCap_ff * this->FaultyChillerFoulingFactor;
            COP = COP_ff * this->FaultyChillerFoulingFactor;
        }

        {
            auto const SELECT_CASE_var(DataPlant::PlantLoop(this->CWLoopNum).LoopDemandCalcScheme);
            if (SELECT_CASE_var == DataPlant::SingleSetPoint) {
                if ((this->FlowMode == DataPlant::FlowMode::LEAVINGSETPOINTMODULATED) ||
                    (DataPlant::PlantLoop(this->CWLoopNum).LoopSide(this->CWLoopSideNum).Branch(this->CWBranchNum).Comp(this->CWCompNum).CurOpSchemeType ==
                     DataPlant::CompSetPtBasedSchemeType) ||
                    (DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPoint != DataLoopNode::SensedNodeFlagValue)) {
                    TempEvapOutSetPoint = DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPoint;
                } else {
                    TempEvapOutSetPoint = DataLoopNode::Node(DataPlant::PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPoint;
                }
            } else if (SELECT_CASE_var == DataPlant::DualSetPointDeadBand) {
                if ((this->FlowMode == DataPlant::FlowMode::LEAVINGSETPOINTMODULATED) ||
                    (DataPlant::PlantLoop(this->CWLoopNum).LoopSide(this->CWLoopSideNum).Branch(this->CWBranchNum).Comp(this->CWCompNum).CurOpSchemeType ==
                     DataPlant::CompSetPtBasedSchemeType) ||
                    (DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPointHi != DataLoopNode::SensedNodeFlagValue)) {
                    TempEvapOutSetPoint = DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPointHi;
                } else {
                    TempEvapOutSetPoint = DataLoopNode::Node(DataPlant::PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPointHi;
                }
            }
        }

        // If there is a fault of Chiller SWT Sensor
        if (this->FaultyChillerSWTFlag && (!DataGlobals::WarmupFlag) && (!DataGlobals::DoingSizing) && (!DataGlobals::KickOffSimulation)) {
            int FaultIndex = this->FaultyChillerSWTIndex;
            Real64 EvapOutletTemp_ff = TempEvapOutSetPoint;

            // calculate the sensor offset using fault information
            this->FaultyChillerSWTOffset = FaultsManager::FaultsChillerSWTSensor(FaultIndex).CalFaultOffsetAct();
            // update the TempEvapOutSetPoint
            TempEvapOutSetPoint = min(DataLoopNode::Node(this->EvapInletNodeNum).Temp, EvapOutletTemp_ff - this->FaultyChillerSWTOffset);
            this->FaultyChillerSWTOffset = EvapOutletTemp_ff - TempEvapOutSetPoint;
        }

        EvapDeltaTemp = std::abs(DataLoopNode::Node(this->EvapInletNodeNum).Temp - TempEvapOutSetPoint);

        // If no component demand, or chiller OFF, or Chiller type set to 'Passive' by free
        // cooling heat exchanger, then set condenser side flow and heat transfer rates set to zero
        if (MyLoad >= 0.0 || !RunFlag) {

            // If Chiller load is 0 or greater or chiller is not running then leave the subroutine.Before leaving
            // if the component control is SERIESACTIVE we set the component flow to inlet flow so that
            // flow resolver will not shut down the branch
            if (EquipFlowCtrl == DataBranchAirLoopPlant::ControlType_SeriesActive ||
                DataPlant::PlantLoop(this->CWLoopNum).LoopSide(this->CWLoopSideNum).FlowLock == 1) {
                this->EvapMassFlowRate = DataLoopNode::Node(this->EvapInletNodeNum).MassFlowRate;
            } else {
                this->EvapMassFlowRate = 0.0;
                PlantUtilities::SetComponentFlowRate(
                    this->EvapMassFlowRate, this->EvapInletNodeNum, this->EvapOutletNodeNum, this->CWLoopNum, this->CWLoopSideNum, this->CWBranchNum, this->CWCompNum);
            }
            if (this->CondenserType == DataPlant::CondenserType::WATERCOOLED) {
                if (DataPlant::PlantLoop(this->CDLoopNum).LoopSide(this->CDLoopSideNum).Branch(this->CDBranchNum).Comp(this->CDCompNum).FlowCtrl ==
                    DataBranchAirLoopPlant::ControlType_SeriesActive) {
                    this->CondMassFlowRate = DataLoopNode::Node(this->CondInletNodeNum).MassFlowRate;
                } else {
                    this->CondMassFlowRate = 0.0;
                    PlantUtilities::SetComponentFlowRate(this->CondMassFlowRate,
                                                         this->CondInletNodeNum,
                                                         this->CondOutletNodeNum,
                                                         this->CDLoopNum,
                                                         this->CDLoopSideNum,
                                                         this->CDBranchNum,
                                                         this->CDCompNum);
                }
            }

            this->EvapOutletTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp;
            this->CondOutletTemp = DataLoopNode::Node(this->CondInletNodeNum).Temp;

            this->Power = 0.0;
            this->QEvaporator = 0.0;
            this->QCondenser = 0.0;
            this->Energy = 0.0;
            this->EvaporatorEnergy = 0.0;
            this->CondenserEnergy = 0.0;

            if (this->CondenserType == DataPlant::CondenserType::EVAPCOOLED) {
                CalcBasinHeaterPower(
                    this->BasinHeaterPowerFTempDiff, this->BasinHeaterSchedulePtr, this->BasinHeaterSetPointTemp, this->BasinHeaterPower);
            }
            this->PrintMessage = false;
            return;
        }

        //   calculate end time of current time step
        CurrentEndTime = DataGlobals::CurrentTime + DataHVACGlobals::SysTimeElapsed;

        //   Print warning messages only when valid and only for the first ocurrance. Let summary provide statistics.
        //   Wait for next time step to print warnings. If simulation iterates, print out
        //   the warning for the last iteration only. Must wait for next time step to accomplish this.
        //   If a warning occurs and the simulation down shifts, the warning is not valid.
        if (CurrentEndTime > this->CurrentEndTimeLast && DataHVACGlobals::TimeStepSys >= this->TimeStepSysLast) {
            if (this->PrintMessage) {
                ++this->MsgErrorCount;
                //       Show single warning and pass additional info to ShowRecurringWarningErrorAtEnd
                if (this->MsgErrorCount < 2) {
                    ShowWarningError(this->MsgBuffer1 + '.');
                    ShowContinueError(this->MsgBuffer2);
                } else {
                    ShowRecurringWarningErrorAtEnd(
                        this->MsgBuffer1 + " error continues.", this->ErrCount1, this->MsgDataLast, this->MsgDataLast, _, "[C]", "[C]");
                }
            }
        }

        //   save last system time step and last end time of current time step (used to determine if warning is valid)
        this->TimeStepSysLast = DataHVACGlobals::TimeStepSys;
        this->CurrentEndTimeLast = CurrentEndTime;

        // otherwise the chiller is running...

        if (this->CondenserType == DataPlant::CondenserType::AIRCOOLED) { // Condenser inlet temp = outdoor temp
            DataLoopNode::Node(this->CondInletNodeNum).Temp = DataLoopNode::Node(this->CondInletNodeNum).OutAirDryBulb;
            //  Warn user if entering condenser temperature falls below 0C
            if (DataLoopNode::Node(this->CondInletNodeNum).Temp < 0.0 && !DataGlobals::WarmupFlag) {
                this->PrintMessage = true;
                this->MsgBuffer1 =
                    "CalcConstCOPChillerModel - Chiller:ConstantCOP \"" + this->Name + "\" - Air Cooled Condenser Inlet Temperature below 0C";
                this->MsgBuffer2 = format("... Outdoor Dry-bulb Condition = {:6.2F} C. Occurrence info = {}, {} {}",
                                          DataLoopNode::Node(this->CondInletNodeNum).Temp,
                                          DataEnvironment::EnvironmentName,
                                          DataEnvironment::CurMnDy,
                                          General::CreateSysTimeIntervalString());
                this->MsgDataLast = DataLoopNode::Node(this->CondInletNodeNum).Temp;
            } else {
                this->PrintMessage = false;
            }
        } else if (this->CondenserType == DataPlant::CondenserType::EVAPCOOLED) { // Condenser inlet temp = (outdoor wet bulb)
            DataLoopNode::Node(this->CondInletNodeNum).Temp = DataLoopNode::Node(this->CondInletNodeNum).OutAirWetBulb;
            //  Warn user if evap condenser wet bulb temperature falls below 10C
            if (DataLoopNode::Node(this->CondInletNodeNum).Temp < 10.0 && !DataGlobals::WarmupFlag) {
                this->PrintMessage = true;
                this->MsgBuffer1 =
                    "CalcConstCOPChillerModel - Chiller:ConstantCOP \"" + this->Name + "\" - Evap Cooled Condenser Inlet Temperature below 10C";
                this->MsgBuffer2 = format("... Outdoor Wet-bulb Condition = {:6.2F} C. Occurrence info = {}, {} {}",
                                          DataLoopNode::Node(this->CondInletNodeNum).Temp,
                                          DataEnvironment::EnvironmentName,
                                          DataEnvironment::CurMnDy,
                                          General::CreateSysTimeIntervalString());
                this->MsgDataLast = DataLoopNode::Node(this->CondInletNodeNum).Temp;
            } else {
                this->PrintMessage = false;
            }
        } // End of the Air Cooled/Evap Cooled Logic block

        // Set condenser flow rate
        if (this->CondenserType == DataPlant::CondenserType::WATERCOOLED) {
            this->CondMassFlowRate = this->CondMassFlowRateMax;
            PlantUtilities::SetComponentFlowRate(
                this->CondMassFlowRate, this->CondInletNodeNum, this->CondOutletNodeNum, this->CDLoopNum, this->CDLoopSideNum, this->CDBranchNum, this->CDCompNum);
            PlantUtilities::PullCompInterconnectTrigger(this->CWLoopNum,
                                                        this->CWLoopSideNum,
                                                        this->CWBranchNum,
                                                        this->CWCompNum,
                                                        this->CondMassFlowIndex,
                                                        this->CDLoopNum,
                                                        this->CDLoopSideNum,
                                                        DataPlant::CriteriaType_MassFlowRate,
                                                        this->CondMassFlowRate);

            if (this->CondMassFlowRate < DataBranchAirLoopPlant::MassFlowTolerance) return;
        }

        // If FlowLock is True, the new resolved mdot is used to update Power, QEvap, Qcond, and
        // condenser side outlet temperature.

        Cp = FluidProperties::GetSpecificHeatGlycol(
            DataPlant::PlantLoop(this->CWLoopNum).FluidName, DataLoopNode::Node(this->EvapInletNodeNum).Temp, DataPlant::PlantLoop(this->CWLoopNum).FluidIndex, RoutineName);

        if (DataPlant::PlantLoop(this->CWLoopNum).LoopSide(this->CWLoopSideNum).FlowLock == 0) {
            this->PossibleSubcooling = false;
            this->QEvaporator = std::abs(MyLoad);
            this->Power = std::abs(MyLoad) / COP;

            // Either set the flow to the Constant value or caluclate the flow for the variable volume
            if ((this->FlowMode == DataPlant::FlowMode::CONSTANT) || (this->FlowMode == DataPlant::FlowMode::NOTMODULATED)) {

                // Start by assuming max (design) flow
                this->EvapMassFlowRate = this->EvapMassFlowRateMax;
                // Use SetComponentFlowRate to decide actual flow
                PlantUtilities::SetComponentFlowRate(
                    this->EvapMassFlowRate, this->EvapInletNodeNum, this->EvapOutletNodeNum, this->CWLoopNum, this->CWLoopSideNum, this->CWBranchNum, this->CWCompNum);
                // Evaluate delta temp based on actual flow rate
                if (this->EvapMassFlowRate != 0.0) {
                    EvapDeltaTemp = this->QEvaporator / this->EvapMassFlowRate / Cp;
                } else {
                    EvapDeltaTemp = 0.0;
                }
                // Evaluate outlet temp based on delta
                this->EvapOutletTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp - EvapDeltaTemp;

            } else if (this->FlowMode == DataPlant::FlowMode::LEAVINGSETPOINTMODULATED) {

                // Calculate the Delta Temp from the inlet temp to the chiller outlet setpoint
                {
                    auto const SELECT_CASE_var(DataPlant::PlantLoop(this->CWLoopNum).LoopDemandCalcScheme);
                    if (SELECT_CASE_var == DataPlant::SingleSetPoint) {
                        EvapDeltaTemp = std::abs(DataLoopNode::Node(this->EvapInletNodeNum).Temp - DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPoint);
                    } else if (SELECT_CASE_var == DataPlant::DualSetPointDeadBand) {
                        EvapDeltaTemp = std::abs(DataLoopNode::Node(this->EvapInletNodeNum).Temp - DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPointHi);
                    }
                }

                if (EvapDeltaTemp > DataPlant::DeltaTempTol) {
                    this->EvapMassFlowRate = std::abs(this->QEvaporator / Cp / EvapDeltaTemp);
                    if ((this->EvapMassFlowRate - this->EvapMassFlowRateMax) > DataBranchAirLoopPlant::MassFlowTolerance)
                        this->PossibleSubcooling = true;
                    // Check to see if the Maximum is exceeded, if so set to maximum
                    this->EvapMassFlowRate = min(this->EvapMassFlowRateMax, this->EvapMassFlowRate);
                    // Use SetComponentFlowRate to decide actual flow
                    PlantUtilities::SetComponentFlowRate(this->EvapMassFlowRate,
                                                         this->EvapInletNodeNum,
                                                         this->EvapOutletNodeNum,
                                                         this->CWLoopNum,
                                                         this->CWLoopSideNum,
                                                         this->CWBranchNum,
                                                         this->CWCompNum);
                    {
                        auto const SELECT_CASE_var(DataPlant::PlantLoop(this->CWLoopNum).LoopDemandCalcScheme);
                        if (SELECT_CASE_var == DataPlant::SingleSetPoint) {
                            this->EvapOutletTemp = DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPoint;
                        } else if (SELECT_CASE_var == DataPlant::DualSetPointDeadBand) {
                            this->EvapOutletTemp = DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPointHi;
                        }
                    }
                } else {
                    // Try to request zero flow
                    this->EvapMassFlowRate = 0.0;
                    // Use SetComponentFlowRate to decide actual flow
                    PlantUtilities::SetComponentFlowRate(this->EvapMassFlowRate,
                                                         this->EvapInletNodeNum,
                                                         this->EvapOutletNodeNum,
                                                         this->CWLoopNum,
                                                         this->CWLoopSideNum,
                                                         this->CWBranchNum,
                                                         this->CWCompNum);
                    // No deltaT since component is not running
                    this->EvapOutletTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp;
                }
            } // End of Constant or Variable Flow If Block for FlowLock = 0 (or making a flow request)

            // If there is a fault of Chiller SWT Sensor
            if (this->FaultyChillerSWTFlag && (!DataGlobals::WarmupFlag) && (!DataGlobals::DoingSizing) && (!DataGlobals::KickOffSimulation) &&
                (this->EvapMassFlowRate > 0)) {
                // calculate directly affected variables at faulty case: EvapOutletTemp, EvapMassFlowRate, QEvaporator
                int FaultIndex = this->FaultyChillerSWTIndex;
                bool VarFlowFlag = (this->FlowMode == DataPlant::FlowMode::LEAVINGSETPOINTMODULATED);
                FaultsManager::FaultsChillerSWTSensor(FaultIndex)
                    .CalFaultChillerSWT(VarFlowFlag,
                                        this->FaultyChillerSWTOffset,
                                        Cp,
                                        DataLoopNode::Node(this->EvapInletNodeNum).Temp,
                                        this->EvapOutletTemp,
                                        this->EvapMassFlowRate,
                                        this->QEvaporator);
            }

        } else { // If FlowLock is True

            this->EvapMassFlowRate = DataLoopNode::Node(this->EvapInletNodeNum).MassFlowRate;
            PlantUtilities::SetComponentFlowRate(
                this->EvapMassFlowRate, this->EvapInletNodeNum, this->EvapOutletNodeNum, this->CWLoopNum, this->CWLoopSideNum, this->CWBranchNum, this->CWCompNum);
            //   Some other component set the flow to 0. No reason to continue with calculations.
            if (this->EvapMassFlowRate == 0.0) {
                MyLoad = 0.0;
                if (this->CondenserType == DataPlant::CondenserType::EVAPCOOLED) {
                    CalcBasinHeaterPower(
                        this->BasinHeaterPowerFTempDiff, this->BasinHeaterSchedulePtr, this->BasinHeaterSetPointTemp, this->BasinHeaterPower);
                }
                this->PrintMessage = false;
                return;
            }

            // Recalculate the Delts Temp
            if (this->PossibleSubcooling) {
                this->QEvaporator = std::abs(MyLoad);
                EvapDeltaTemp = this->QEvaporator / this->EvapMassFlowRate / Cp;
                this->EvapOutletTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp - EvapDeltaTemp;
                if (this->EvapOutletTemp < DataLoopNode::Node(this->EvapOutletNodeNum).TempMin) {
                    this->EvapOutletTemp = DataLoopNode::Node(this->EvapOutletNodeNum).TempMin;
                    EvapDeltaTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp - this->EvapOutletTemp;
                    this->QEvaporator = this->EvapMassFlowRate * Cp * EvapDeltaTemp;
                }
            } else {
                EvapDeltaTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp - TempEvapOutSetPoint;
                // Calculate the evaporator heat transfer at the specified flow which could have changed
                //  in the Flow Resolution step.
                this->QEvaporator = std::abs(this->EvapMassFlowRate * Cp * EvapDeltaTemp);
                this->EvapOutletTemp = TempEvapOutSetPoint;
            }
            // Check that the Evap outlet temp honors both plant loop temp low limit and also the chiller low limit
            if (this->EvapOutletTemp < DataLoopNode::Node(this->EvapOutletNodeNum).TempMin) {
                if ((DataLoopNode::Node(this->EvapInletNodeNum).Temp - DataLoopNode::Node(this->EvapOutletNodeNum).TempMin) > DataPlant::DeltaTempTol) {
                    this->EvapOutletTemp = DataLoopNode::Node(this->EvapOutletNodeNum).TempMin;
                    EvapDeltaTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp - this->EvapOutletTemp;
                    this->QEvaporator = this->EvapMassFlowRate * Cp * EvapDeltaTemp;
                } else {
                    this->EvapOutletTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp;
                    EvapDeltaTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp - this->EvapOutletTemp;
                    this->QEvaporator = this->EvapMassFlowRate * Cp * EvapDeltaTemp;
                }
            }
            // If load exceeds the distributed load set to the distributed load
            if (this->QEvaporator > std::abs(MyLoad)) {
                if (this->EvapMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance) {
                    this->QEvaporator = std::abs(MyLoad);
                    EvapDeltaTemp = this->QEvaporator / this->EvapMassFlowRate / Cp;
                    this->EvapOutletTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp - EvapDeltaTemp;
                } else {
                    this->QEvaporator = 0.0;
                    this->EvapOutletTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp;
                }
            }

            // If there is a fault of Chiller SWT Sensor
            if (this->FaultyChillerSWTFlag && (!DataGlobals::WarmupFlag) && (!DataGlobals::DoingSizing) && (!DataGlobals::KickOffSimulation) &&
                (this->EvapMassFlowRate > 0)) {
                // calculate directly affected variables at faulty case: EvapOutletTemp, EvapMassFlowRate, QEvaporator
                int FaultIndex = this->FaultyChillerSWTIndex;
                bool VarFlowFlag = false;
                FaultsManager::FaultsChillerSWTSensor(FaultIndex)
                    .CalFaultChillerSWT(VarFlowFlag,
                                        this->FaultyChillerSWTOffset,
                                        Cp,
                                        DataLoopNode::Node(this->EvapInletNodeNum).Temp,
                                        this->EvapOutletTemp,
                                        this->EvapMassFlowRate,
                                        this->QEvaporator);
            }

            // Checks QEvaporator on the basis of the machine limits.
            if (this->QEvaporator > ChillerNomCap) {
                if (this->EvapMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance) {
                    this->QEvaporator = ChillerNomCap;
                    EvapDeltaTemp = this->QEvaporator / this->EvapMassFlowRate / Cp;
                    this->EvapOutletTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp - EvapDeltaTemp;
                } else {
                    this->QEvaporator = 0.0;
                    this->EvapOutletTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp;
                }
            }
            // Calculate the Power consumption of the Const COP chiller which is a simplified calculation
            this->Power = this->QEvaporator / COP;
            if (this->EvapMassFlowRate == 0.0) {
                this->QEvaporator = 0.0;
                this->EvapOutletTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp;
                this->Power = 0.0;
                this->PrintMessage = false;
            }
            if (this->QEvaporator == 0.0 && this->CondenserType == DataPlant::CondenserType::EVAPCOOLED) {
                CalcBasinHeaterPower(
                    this->BasinHeaterPowerFTempDiff, this->BasinHeaterSchedulePtr, this->BasinHeaterSetPointTemp, this->BasinHeaterPower);
            }

        } // This is the end of the FlowLock Block

        // QCondenser is calculated the same for each type, but the power consumption should be different
        //  depending on the performance coefficients used for the chiller model.
        this->QCondenser = this->Power + this->QEvaporator;

        // If not air or evap cooled then set to the condenser node that is attached to a cooling tower
        Real64 const CondInletTemp = DataLoopNode::Node(this->CondInletNodeNum).Temp;

        if (this->CondenserType == DataPlant::CondenserType::WATERCOOLED) {
            CpCond = FluidProperties::GetSpecificHeatGlycol(
                DataPlant::PlantLoop(this->CDLoopNum).FluidName, CondInletTemp, DataPlant::PlantLoop(this->CDLoopNum).FluidIndex, RoutineName);
            if (this->CondMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance) {
                this->CondOutletTemp = this->QCondenser / this->CondMassFlowRate / CpCond + CondInletTemp;
            } else {
                ShowSevereError("CalcConstCOPChillerModel: Condenser flow = 0, for CONST COP Chiller=" + this->Name);
                ShowContinueErrorTimeStamp("");
            }
        } else { // Air Cooled or Evap Cooled
            //  Set condenser outlet temp to condenser inlet temp for Air Cooled or Evap Cooled
            //  since there is no CondMassFlowRate and would divide by zero
            this->CondOutletTemp = CondInletTemp;
        }

        // Calculate Energy
        this->CondenserEnergy = this->QCondenser * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
        this->Energy = this->Power * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
        this->EvaporatorEnergy = this->QEvaporator * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

        // check for problems BG 9/12/06 (deal with observed negative energy results)
        if (this->Energy < 0.0) { // there is a serious problem

            if (this->CondenserType == DataPlant::CondenserType::WATERCOOLED) {
                // first check for run away condenser loop temps (only reason yet to be observed for this?)
                if (CondInletTemp > 70.0) {
                    ShowSevereError("CalcConstCOPChillerModel: Condenser loop inlet temperatures over 70.0 C for ConstCOPChiller=" + this->Name);
                    ShowContinueErrorTimeStamp("");
                    ShowContinueError("Condenser loop water temperatures are too high at" + General::RoundSigDigits(CondInletTemp, 2));
                    ShowContinueError("Check input for condenser plant loop, especially cooling tower");
                    ShowContinueError("Evaporator inlet temperature: " + General::RoundSigDigits(DataLoopNode::Node(this->EvapInletNodeNum).Temp, 2));

                    ShowFatalError("Program Terminates due to previous error condition");
                }
            }
            // If makes it here, set limits, chiller can't have negative energy/power
            // proceeding silently for now but may want to throw error here
            this->Power = 0.0;
            this->Energy = 0.0;
        }
    }

    void ConstCOPChillerSpecs::update(Real64 const MyLoad, bool const RunFlag)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Dan Fisher
        //       DATE WRITTEN:    October 1998

        Real64 ReportingConstant = DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

        if (MyLoad >= 0.0 || !RunFlag) { // Chiller not running so pass inlet states to outlet states
            this->Power = 0.0;
            this->QEvaporator = 0.0;
            this->QCondenser = 0.0;
            this->Energy = 0.0;
            this->EvaporatorEnergy = 0.0;
            this->CondenserEnergy = 0.0;
            this->CondInletTemp = DataLoopNode::Node(this->CondInletNodeNum).Temp;
            this->EvapInletTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp;
            this->CondOutletTemp = DataLoopNode::Node(this->CondInletNodeNum).Temp;
            this->EvapOutletTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp;
            this->ActualCOP = 0.0;
            if (this->CondenserType == DataPlant::CondenserType::EVAPCOOLED) {
                this->BasinHeaterPower = this->BasinHeaterPower;
                this->BasinHeaterConsumption = this->BasinHeaterPower * ReportingConstant;
            }

            // set outlet node temperatures
            DataLoopNode::Node(this->EvapOutletNodeNum).Temp = DataLoopNode::Node(this->EvapInletNodeNum).Temp;
            DataLoopNode::Node(this->CondOutletNodeNum).Temp = DataLoopNode::Node(this->CondInletNodeNum).Temp;

        } else {
            this->CondInletTemp = DataLoopNode::Node(this->CondInletNodeNum).Temp;
            this->EvapInletTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp;
            if (this->Power != 0.0) {
                this->ActualCOP = this->QEvaporator / this->Power;
            } else {
                this->ActualCOP = 0.0;
            }
            if (this->CondenserType == DataPlant::CondenserType::EVAPCOOLED) {
                this->BasinHeaterPower = this->BasinHeaterPower;
                this->BasinHeaterConsumption = this->BasinHeaterPower * ReportingConstant;
            }

            // set outlet node temperatures
            DataLoopNode::Node(this->EvapOutletNodeNum).Temp = this->EvapOutletTemp;
            DataLoopNode::Node(this->CondOutletNodeNum).Temp = this->CondOutletTemp;
        }
    }

} // namespace PlantChillers

} // namespace EnergyPlus
