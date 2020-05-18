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
#include <cassert>
#include <cmath>
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/ChillerElectricEIR.hh>
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
#include <EnergyPlus/OutputFiles.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/Plant/PlantLocation.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ReportSizingManager.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/StandardRatings.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

namespace ChillerElectricEIR {

    // NOTES:
    // The Electric EIR and Reformulated EIR chiller models are similar.
    // They only differ in the independent variable used to evaluate the performance curves.

    // MODULE INFORMATION:
    //       AUTHOR         Richard Raustad
    //       DATE WRITTEN   June 2004
    //       MODIFIED       Chandan Sharma, FSEC, February 2010, Added basin heater
    //                      Brent Griffith, NREL, Sept 2010, revised for plant changes
    //                      generalized fluid properties
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    //  This module simulates the performance of the electric vapor
    //  compression chiller used in DOE-2.

    // METHODOLOGY EMPLOYED:
    //  Once the PlantLoopManager determines that the Electric EIR chiller
    //  is available to meet a loop cooling demand, it calls SimElectricEIRChiller
    //  which in turn calls the electric EIR model. The EIR chiller model is based on
    //  polynomial fits of chiller performance data.

    // REFERENCES:
    // 1. DOE-2 Engineers Manual, Version 2.1A, November 1982, LBL-11353

    PlantComponent *ElectricEIRChillerSpecs::factory(ChillerElectricEIRData &chillers, std::string const &objectName)
    {
        // Process the input data if it hasn't been done already
        if (chillers.getInputFlag) {
            GetElectricEIRChillerInput(chillers);
            chillers.getInputFlag = false;
        }
        // Now look for this particular object in the list
        for (auto &obj : chillers.ElectricEIRChiller) {
            if (obj.Name == objectName) {
                return &obj;
            }
        }
        // If we didn't find it, fatal
        ShowFatalError("LocalElectEIRChillerFactory: Error getting inputs for object named: " + objectName); // LCOV_EXCL_LINE
        // Shut up the compiler
        return nullptr; // LCOV_EXCL_LINE
    }

    void ElectricEIRChillerSpecs::simulate(EnergyPlusData &EP_UNUSED(state), const PlantLocation &calledFromLocation, bool FirstHVACIteration, Real64 &CurLoad, bool RunFlag)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad
        //       DATE WRITTEN   June 2004
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        //  This is the electric EIR chiller model driver. It gets the input for the
        //  model, initializes simulation variables, calls the appropriate model and sets
        //  up reporting variables.

        if (calledFromLocation.loopNum == this->CWLoopNum) {
            this->initialize(RunFlag, CurLoad);
            this->calculate(CurLoad, RunFlag);
            this->update(CurLoad, RunFlag);

        } else if (calledFromLocation.loopNum == this->CDLoopNum) {
            PlantUtilities::UpdateChillerComponentCondenserSide(calledFromLocation.loopNum,
                                                                this->CDLoopSideNum,
                                                                DataPlant::TypeOf_Chiller_ElectricEIR,
                                                                this->CondInletNodeNum,
                                                                this->CondOutletNodeNum,
                                                                this->QCondenser,
                                                                this->CondInletTemp,
                                                                this->CondOutletTemp,
                                                                this->CondMassFlowRate,
                                                                FirstHVACIteration);

        } else if (calledFromLocation.loopNum == this->HRLoopNum) {
            PlantUtilities::UpdateComponentHeatRecoverySide(this->HRLoopNum,
                                                            this->HRLoopSideNum,
                                                            DataPlant::TypeOf_Chiller_ElectricEIR,
                                                            this->HeatRecInletNodeNum,
                                                            this->HeatRecOutletNodeNum,
                                                            this->QHeatRecovered,
                                                            this->HeatRecInletTemp,
                                                            this->HeatRecOutletTemp,
                                                            this->HeatRecMassFlow,
                                                            FirstHVACIteration);
        }
    }

    void ElectricEIRChillerSpecs::getDesignCapacities(const PlantLocation &calledFromLocation, Real64 &MaxLoad, Real64 &MinLoad, Real64 &OptLoad)
    {
        if (calledFromLocation.loopNum == this->CWLoopNum) {
            MinLoad = this->RefCap * this->MinPartLoadRat;
            MaxLoad = this->RefCap * this->MaxPartLoadRat;
            OptLoad = this->RefCap * this->OptPartLoadRat;
        } else {
            MinLoad = 0.0;
            MaxLoad = 0.0;
            OptLoad = 0.0;
        }
    }

    void ElectricEIRChillerSpecs::getDesignTemperatures(Real64 &TempDesCondIn, Real64 &TempDesEvapOut)
    {
        TempDesCondIn = this->TempRefCondIn;
        TempDesEvapOut = this->TempRefEvapOut;
    }

    void ElectricEIRChillerSpecs::getSizingFactor(Real64 &sizFac)
    {
        sizFac = this->SizFac;
    }

    void ElectricEIRChillerSpecs::onInitLoopEquip(EnergyPlusData &state, const PlantLocation &calledFromLocation)
    {
        bool runFlag = true;
        Real64 myLoad = 0.0;

        this->initialize(runFlag, myLoad);

        if (calledFromLocation.loopNum == this->CWLoopNum) {
            this->size(state);
        }
    }

    void GetElectricEIRChillerInput(ChillerElectricEIRData &chillers)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Richard Raustad, FSEC
        //       DATE WRITTEN:    June 2004

        // PURPOSE OF THIS SUBROUTINE:
        //  This routine will get the input required by the Electric EIR Chiller model.

        static std::string const RoutineName("GetElectricEIRChillerInput: "); // include trailing blank space

        bool ErrorsFound(false); // True when input errors are found

        DataIPShortCuts::cCurrentModuleObject = "Chiller:Electric:EIR";
        chillers.NumElectricEIRChillers = inputProcessor->getNumObjectsFound(DataIPShortCuts::cCurrentModuleObject);

        if (chillers.NumElectricEIRChillers <= 0) {
            ShowSevereError("No " + DataIPShortCuts::cCurrentModuleObject + " equipment specified in input file");
            ErrorsFound = true;
        }

        // ALLOCATE ARRAYS
        chillers.ElectricEIRChiller.allocate(chillers.NumElectricEIRChillers);

        // Load arrays with electric EIR chiller data
        for (int EIRChillerNum = 1; EIRChillerNum <= chillers.NumElectricEIRChillers; ++EIRChillerNum) {
            int NumAlphas; // Number of elements in the alpha array
            int NumNums;   // Number of elements in the numeric array
            int IOStat;    // IO Status when calling get input subroutine
            inputProcessor->getObjectItem(DataIPShortCuts::cCurrentModuleObject,
                                          EIRChillerNum,
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
            GlobalNames::VerifyUniqueChillerName(
                DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), ErrorsFound, DataIPShortCuts::cCurrentModuleObject + " Name");

            auto &thisChiller = chillers.ElectricEIRChiller(EIRChillerNum);
            thisChiller.Name = DataIPShortCuts::cAlphaArgs(1);

            //   Performance curves
            thisChiller.ChillerCapFTIndex = CurveManager::GetCurveIndex(DataIPShortCuts::cAlphaArgs(2));
            if (thisChiller.ChillerCapFTIndex == 0) {
                ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + " \"" + DataIPShortCuts::cAlphaArgs(1) + "\"");
                ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(2) + '=' + DataIPShortCuts::cAlphaArgs(2));
                ErrorsFound = true;
            }

            thisChiller.ChillerEIRFTIndex = CurveManager::GetCurveIndex(DataIPShortCuts::cAlphaArgs(3));
            if (thisChiller.ChillerEIRFTIndex == 0) {
                ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\"");
                ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(3) + '=' + DataIPShortCuts::cAlphaArgs(3));
                ErrorsFound = true;
            }

            thisChiller.ChillerEIRFPLRIndex = CurveManager::GetCurveIndex(DataIPShortCuts::cAlphaArgs(4));
            if (thisChiller.ChillerEIRFPLRIndex == 0) {
                ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\"");
                ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(4) + '=' + DataIPShortCuts::cAlphaArgs(4));
                ErrorsFound = true;
            }

            thisChiller.EvapInletNodeNum = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(5),
                                                                                                     ErrorsFound,
                                                                                                     DataIPShortCuts::cCurrentModuleObject,
                                                                                                     DataIPShortCuts::cAlphaArgs(1),
                                                                                                     DataLoopNode::NodeType_Water,
                                                                                                     DataLoopNode::NodeConnectionType_Inlet,
                                                                                                     1,
                                                                                                     DataLoopNode::ObjectIsNotParent);
            thisChiller.EvapOutletNodeNum = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(6),
                                                                                                      ErrorsFound,
                                                                                                      DataIPShortCuts::cCurrentModuleObject,
                                                                                                      DataIPShortCuts::cAlphaArgs(1),
                                                                                                      DataLoopNode::NodeType_Water,
                                                                                                      DataLoopNode::NodeConnectionType_Outlet,
                                                                                                      1,
                                                                                                      DataLoopNode::ObjectIsNotParent);
            BranchNodeConnections::TestCompSet(DataIPShortCuts::cCurrentModuleObject,
                                               DataIPShortCuts::cAlphaArgs(1),
                                               DataIPShortCuts::cAlphaArgs(5),
                                               DataIPShortCuts::cAlphaArgs(6),
                                               "Chilled Water Nodes");

            if (UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(9), "WaterCooled")) {
                thisChiller.CondenserType = DataPlant::CondenserType::WATERCOOLED;
            } else if (UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(9), "AirCooled")) {
                thisChiller.CondenserType = DataPlant::CondenserType::AIRCOOLED;
            } else if (UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(9), "EvaporativelyCooled")) {
                thisChiller.CondenserType = DataPlant::CondenserType::EVAPCOOLED;
            } else {
                ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + ": " + DataIPShortCuts::cAlphaArgs(1));
                ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(9) + '=' + DataIPShortCuts::cAlphaArgs(9));
                ShowContinueError("Valid entries are AirCooled, WaterCooled, or EvaporativelyCooled");
                ErrorsFound = true;
            }

            if (thisChiller.CondenserType == DataPlant::CondenserType::AIRCOOLED || thisChiller.CondenserType == DataPlant::CondenserType::EVAPCOOLED) {
                // Connection not required for air or evap cooled condenser
                // If the condenser inlet is blank for air cooled and evap cooled condensers then supply a generic name
                // since it is not used elsewhere for connection
                if (DataIPShortCuts::lAlphaFieldBlanks(7)) {
                    if (len(DataIPShortCuts::cAlphaArgs(1)) < DataGlobals::MaxNameLength - 25) { // protect against long name leading to > 100 chars
                        DataIPShortCuts::cAlphaArgs(7) = DataIPShortCuts::cAlphaArgs(1) + " INLET NODE FOR CONDENSER";
                    } else {
                        DataIPShortCuts::cAlphaArgs(7) = DataIPShortCuts::cAlphaArgs(1).substr(0, 75) + " INLET NODE FOR CONDENSER";
                    }
                }
                if (DataIPShortCuts::lAlphaFieldBlanks(8)) {
                    if (len(DataIPShortCuts::cAlphaArgs(1)) < DataGlobals::MaxNameLength - 26) { // protect against long name leading to > 100 chars
                        DataIPShortCuts::cAlphaArgs(8) = DataIPShortCuts::cAlphaArgs(1) + " OUTLET NODE FOR CONDENSER";
                    } else {
                        DataIPShortCuts::cAlphaArgs(8) = DataIPShortCuts::cAlphaArgs(1).substr(0, 74) + " OUTLET NODE FOR CONDENSER";
                    }
                }

                thisChiller.CondInletNodeNum =
                    NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(7),
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
                    ShowWarningError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\"");
                    ShowContinueError("Adding OutdoorAir:Node=" + DataIPShortCuts::cAlphaArgs(7));
                }

                thisChiller.CondOutletNodeNum = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(8),
                                                                                                          ErrorsFound,
                                                                                                          DataIPShortCuts::cCurrentModuleObject,
                                                                                                          DataIPShortCuts::cAlphaArgs(1),
                                                                                                          DataLoopNode::NodeType_Air,
                                                                                                          DataLoopNode::NodeConnectionType_Outlet,
                                                                                                          2,
                                                                                                          DataLoopNode::ObjectIsNotParent);

            } else if (thisChiller.CondenserType == DataPlant::CondenserType::WATERCOOLED) {
                // Condenser inlet node name is necessary for water-cooled condenser
                if (DataIPShortCuts::lAlphaFieldBlanks(7) || DataIPShortCuts::lAlphaFieldBlanks(8)) {
                    ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\"");
                    ShowContinueError("Condenser Inlet or Outlet Node Name is blank.");
                    ErrorsFound = true;
                }

                thisChiller.CondInletNodeNum = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(7),
                                                                                                         ErrorsFound,
                                                                                                         DataIPShortCuts::cCurrentModuleObject,
                                                                                                         DataIPShortCuts::cAlphaArgs(1),
                                                                                                         DataLoopNode::NodeType_Water,
                                                                                                         DataLoopNode::NodeConnectionType_Inlet,
                                                                                                         2,
                                                                                                         DataLoopNode::ObjectIsNotParent);

                thisChiller.CondOutletNodeNum = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(8),
                                                                                                          ErrorsFound,
                                                                                                          DataIPShortCuts::cCurrentModuleObject,
                                                                                                          DataIPShortCuts::cAlphaArgs(1),
                                                                                                          DataLoopNode::NodeType_Water,
                                                                                                          DataLoopNode::NodeConnectionType_Outlet,
                                                                                                          2,
                                                                                                          DataLoopNode::ObjectIsNotParent);

                BranchNodeConnections::TestCompSet(DataIPShortCuts::cCurrentModuleObject,
                                                   DataIPShortCuts::cAlphaArgs(1),
                                                   DataIPShortCuts::cAlphaArgs(7),
                                                   DataIPShortCuts::cAlphaArgs(8),
                                                   "Condenser Water Nodes");

            } else {
                // Condenser inlet node name is necessary (never should reach this part of code)
                if (DataIPShortCuts::lAlphaFieldBlanks(7) || DataIPShortCuts::lAlphaFieldBlanks(8)) {
                    ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\"");
                    ShowContinueError("Condenser Inlet or Outlet Node Name is blank.");
                    ErrorsFound = true;
                }
                thisChiller.CondInletNodeNum = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(7),
                                                                                                         ErrorsFound,
                                                                                                         DataIPShortCuts::cCurrentModuleObject,
                                                                                                         DataIPShortCuts::cAlphaArgs(1),
                                                                                                         DataLoopNode::NodeType_Unknown,
                                                                                                         DataLoopNode::NodeConnectionType_Inlet,
                                                                                                         2,
                                                                                                         DataLoopNode::ObjectIsNotParent);

                thisChiller.CondOutletNodeNum = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(8),
                                                                                                          ErrorsFound,
                                                                                                          DataIPShortCuts::cCurrentModuleObject,
                                                                                                          DataIPShortCuts::cAlphaArgs(1),
                                                                                                          DataLoopNode::NodeType_Unknown,
                                                                                                          DataLoopNode::NodeConnectionType_Outlet,
                                                                                                          2,
                                                                                                          DataLoopNode::ObjectIsNotParent);

                BranchNodeConnections::TestCompSet(DataIPShortCuts::cCurrentModuleObject,
                                                   DataIPShortCuts::cAlphaArgs(1),
                                                   DataIPShortCuts::cAlphaArgs(7),
                                                   DataIPShortCuts::cAlphaArgs(8),
                                                   "Condenser (unknown?) Nodes");
            }

            {
                auto const SELECT_CASE_var(DataIPShortCuts::cAlphaArgs(10));
                if (SELECT_CASE_var == "CONSTANTFLOW") {
                    thisChiller.FlowMode = DataPlant::FlowMode::CONSTANT;
                } else if (SELECT_CASE_var == "LEAVINGSETPOINTMODULATED") {
                    thisChiller.FlowMode = DataPlant::FlowMode::LEAVINGSETPOINTMODULATED;
                } else if (SELECT_CASE_var == "NOTMODULATED") {
                    thisChiller.FlowMode = DataPlant::FlowMode::NOTMODULATED;
                } else {
                    ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\",");
                    ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(10) + '=' + DataIPShortCuts::cAlphaArgs(10));
                    ShowContinueError("Available choices are ConstantFlow, NotModulated, or LeavingSetpointModulated");
                    ShowContinueError("Flow mode NotModulated is assumed and the simulation continues.");
                    thisChiller.FlowMode = DataPlant::FlowMode::NOTMODULATED;
                }
            }

            //   Chiller rated performance data
            thisChiller.RefCap = DataIPShortCuts::rNumericArgs(1);
            if (thisChiller.RefCap == DataSizing::AutoSize) {
                thisChiller.RefCapWasAutoSized = true;
            }
            if (DataIPShortCuts::rNumericArgs(1) == 0.0) {
                ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\"");
                ShowContinueError("Invalid " + DataIPShortCuts::cNumericFieldNames(1) + '=' +
                                  General::RoundSigDigits(DataIPShortCuts::rNumericArgs(1), 2));
                ErrorsFound = true;
            }
            thisChiller.RefCOP = DataIPShortCuts::rNumericArgs(2);
            if (DataIPShortCuts::rNumericArgs(2) == 0.0) {
                ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\"");
                ShowContinueError("Invalid " + DataIPShortCuts::cNumericFieldNames(2) + '=' +
                                  General::RoundSigDigits(DataIPShortCuts::rNumericArgs(2), 2));
                ErrorsFound = true;
            }
            thisChiller.TempRefEvapOut = DataIPShortCuts::rNumericArgs(3);
            thisChiller.TempRefCondIn = DataIPShortCuts::rNumericArgs(4);
            thisChiller.EvapVolFlowRate = DataIPShortCuts::rNumericArgs(5);
            if (thisChiller.EvapVolFlowRate == DataSizing::AutoSize) {
                thisChiller.EvapVolFlowRateWasAutoSized = true;
            }
            thisChiller.CondVolFlowRate = DataIPShortCuts::rNumericArgs(6);
            if (thisChiller.CondVolFlowRate == DataSizing::AutoSize) {
                thisChiller.CondVolFlowRateWasAutoSized = true;
            }

            thisChiller.MinPartLoadRat = DataIPShortCuts::rNumericArgs(7);
            thisChiller.MaxPartLoadRat = DataIPShortCuts::rNumericArgs(8);
            thisChiller.OptPartLoadRat = DataIPShortCuts::rNumericArgs(9);
            thisChiller.MinUnloadRat = DataIPShortCuts::rNumericArgs(10);
            thisChiller.SizFac = DataIPShortCuts::rNumericArgs(15);
            if (thisChiller.SizFac <= 0.0) thisChiller.SizFac = 1.0;

            if (thisChiller.MinPartLoadRat > thisChiller.MaxPartLoadRat) {
                ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\"");
                ShowContinueError(DataIPShortCuts::cNumericFieldNames(7) + " [" + General::RoundSigDigits(DataIPShortCuts::rNumericArgs(7), 3) +
                                  "] > " + DataIPShortCuts::cNumericFieldNames(8) + " [" +
                                  General::RoundSigDigits(DataIPShortCuts::rNumericArgs(8), 3) + ']');
                ShowContinueError("Minimum part load ratio must be less than or equal to the maximum part load ratio ");
                ErrorsFound = true;
            }

            if (thisChiller.MinUnloadRat < thisChiller.MinPartLoadRat ||
                thisChiller.MinUnloadRat > thisChiller.MaxPartLoadRat) {
                ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\"");
                ShowContinueError(DataIPShortCuts::cNumericFieldNames(10) + " = " + General::RoundSigDigits(DataIPShortCuts::rNumericArgs(10), 3));
                ShowContinueError(DataIPShortCuts::cNumericFieldNames(10) + " must be greater than or equal to the " +
                                  DataIPShortCuts::cNumericFieldNames(7));
                ShowContinueError(DataIPShortCuts::cNumericFieldNames(10) + " must be less than or equal to the " +
                                  DataIPShortCuts::cNumericFieldNames(8));
                ErrorsFound = true;
            }

            if (thisChiller.OptPartLoadRat < thisChiller.MinPartLoadRat ||
                thisChiller.OptPartLoadRat > thisChiller.MaxPartLoadRat) {
                ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\"");
                ShowContinueError(DataIPShortCuts::cNumericFieldNames(9) + " = " + General::RoundSigDigits(DataIPShortCuts::rNumericArgs(9), 3));
                ShowContinueError(DataIPShortCuts::cNumericFieldNames(9) + " must be greater than or equal to the " +
                                  DataIPShortCuts::cNumericFieldNames(7));
                ShowContinueError(DataIPShortCuts::cNumericFieldNames(9) + " must be less than or equal to the " +
                                  DataIPShortCuts::cNumericFieldNames(8));
                ErrorsFound = true;
            }

            thisChiller.CondenserFanPowerRatio = DataIPShortCuts::rNumericArgs(11);
            thisChiller.CompPowerToCondenserFrac = DataIPShortCuts::rNumericArgs(12);

            if (thisChiller.CompPowerToCondenserFrac < 0.0 ||
                thisChiller.CompPowerToCondenserFrac > 1.0) {
                ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\"");
                ShowContinueError(DataIPShortCuts::cNumericFieldNames(12) + " = " + General::RoundSigDigits(DataIPShortCuts::rNumericArgs(12), 3));
                ShowContinueError(DataIPShortCuts::cNumericFieldNames(12) + " must be greater than or equal to zero");
                ShowContinueError(DataIPShortCuts::cNumericFieldNames(12) + " must be less than or equal to one");
                ErrorsFound = true;
            }

            thisChiller.TempLowLimitEvapOut = DataIPShortCuts::rNumericArgs(13);

            // These are the heat recovery inputs
            thisChiller.DesignHeatRecVolFlowRate = DataIPShortCuts::rNumericArgs(14);
            if (thisChiller.DesignHeatRecVolFlowRate == DataSizing::AutoSize) {
                thisChiller.DesignHeatRecVolFlowRateWasAutoSized = true;
            }
            if ((thisChiller.DesignHeatRecVolFlowRate > 0.0) ||
                (thisChiller.DesignHeatRecVolFlowRate == DataSizing::AutoSize)) {
                thisChiller.HeatRecActive = true;
                thisChiller.HeatRecInletNodeNum = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(11),
                                                                                                            ErrorsFound,
                                                                                                            DataIPShortCuts::cCurrentModuleObject,
                                                                                                            DataIPShortCuts::cAlphaArgs(1),
                                                                                                            DataLoopNode::NodeType_Water,
                                                                                                            DataLoopNode::NodeConnectionType_Inlet,
                                                                                                            3,
                                                                                                            DataLoopNode::ObjectIsNotParent);
                if (thisChiller.HeatRecInletNodeNum == 0) {
                    ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\"");
                    ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(11) + '=' + DataIPShortCuts::cAlphaArgs(11));
                    ErrorsFound = true;
                }
                thisChiller.HeatRecOutletNodeNum = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(12),
                                                                                                             ErrorsFound,
                                                                                                             DataIPShortCuts::cCurrentModuleObject,
                                                                                                             DataIPShortCuts::cAlphaArgs(1),
                                                                                                             DataLoopNode::NodeType_Water,
                                                                                                             DataLoopNode::NodeConnectionType_Outlet,
                                                                                                             3,
                                                                                                             DataLoopNode::ObjectIsNotParent);
                if (thisChiller.HeatRecOutletNodeNum == 0) {
                    ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\"");
                    ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(12) + '=' + DataIPShortCuts::cAlphaArgs(12));
                    ErrorsFound = true;
                }
                if (thisChiller.CondenserType != DataPlant::CondenserType::WATERCOOLED) {
                    ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\"");
                    ShowContinueError("Heat Recovery requires a Water Cooled Condenser.");
                    ErrorsFound = true;
                }

                BranchNodeConnections::TestCompSet(DataIPShortCuts::cCurrentModuleObject,
                                                   DataIPShortCuts::cAlphaArgs(1),
                                                   DataIPShortCuts::cAlphaArgs(11),
                                                   DataIPShortCuts::cAlphaArgs(12),
                                                   "Heat Recovery Nodes");
                // store heat recovery volume flow for plant sizing
                if (thisChiller.DesignHeatRecVolFlowRate > 0.0) {
                    PlantUtilities::RegisterPlantCompDesignFlow(thisChiller.HeatRecInletNodeNum,
                                                                thisChiller.DesignHeatRecVolFlowRate); // CR 6953
                }
                if (NumNums > 17) {
                    if (!DataIPShortCuts::lNumericFieldBlanks(18)) {
                        thisChiller.HeatRecCapacityFraction = DataIPShortCuts::rNumericArgs(18);
                    } else {
                        thisChiller.HeatRecCapacityFraction = 1.0;
                    }
                } else {
                    thisChiller.HeatRecCapacityFraction = 1.0;
                }

                if (NumAlphas > 13) {
                    if (!DataIPShortCuts::lAlphaFieldBlanks(14)) {
                        thisChiller.HeatRecInletLimitSchedNum =
                            ScheduleManager::GetScheduleIndex(DataIPShortCuts::cAlphaArgs(14));
                        if (thisChiller.HeatRecInletLimitSchedNum == 0) {
                            ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\"");
                            ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(14) + '=' + DataIPShortCuts::cAlphaArgs(14));
                            ErrorsFound = true;
                        }
                    } else {
                        thisChiller.HeatRecInletLimitSchedNum = 0;
                    }
                } else {
                    thisChiller.HeatRecInletLimitSchedNum = 0;
                }

                if (NumAlphas > 14) {
                    if (!DataIPShortCuts::lAlphaFieldBlanks(15)) {
                        thisChiller.HeatRecSetPointNodeNum =
                            NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(15),
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
                if (!DataIPShortCuts::lAlphaFieldBlanks(11) || !DataIPShortCuts::lAlphaFieldBlanks(12)) {
                    //  IF (DataIPShortCuts::cAlphaArgs(11) /= ' ' .or. DataIPShortCuts::cAlphaArgs(12) /= ' ') THEN
                    ShowWarningError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\"");
                    ShowContinueError("Since Reference Heat Reclaim Volume Flow Rate = 0.0, heat recovery is inactive.");
                    ShowContinueError("However, node names were specified for heat recovery inlet or outlet nodes.");
                }
            }

            //   Check the CAP-FT, EIR-FT, and PLR curves and warn user if different from 1.0 by more than +-10%
            if (thisChiller.ChillerCapFTIndex > 0) {
                Real64 CurveVal = CurveManager::CurveValue(thisChiller.ChillerCapFTIndex,
                                                           thisChiller.TempRefEvapOut,
                                                           thisChiller.TempRefCondIn);
                if (CurveVal > 1.10 || CurveVal < 0.90) {
                    ShowWarningError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\"");
                    ShowContinueError(
                        "Capacity ratio as a function of temperature curve output is not equal to 1.0 (+ or - 10%) at reference conditions.");
                    ShowContinueError("Curve output at reference conditions = " + General::TrimSigDigits(CurveVal, 3));
                }
            }

            if (thisChiller.ChillerEIRFTIndex > 0) {
                Real64 CurveVal = CurveManager::CurveValue(thisChiller.ChillerEIRFTIndex,
                                                           thisChiller.TempRefEvapOut,
                                                           thisChiller.TempRefCondIn);
                if (CurveVal > 1.10 || CurveVal < 0.90) {
                    ShowWarningError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\"");
                    ShowContinueError(
                        "Energy input ratio as a function of temperature curve output is not equal to 1.0 (+ or - 10%) at reference conditions.");
                    ShowContinueError("Curve output at reference conditions = " + General::TrimSigDigits(CurveVal, 3));
                }
            }

            if (thisChiller.ChillerEIRFPLRIndex > 0) {
                Real64 CurveVal = CurveManager::CurveValue(thisChiller.ChillerEIRFPLRIndex, 1.0);

                if (CurveVal > 1.10 || CurveVal < 0.90) {
                    ShowWarningError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\"");
                    ShowContinueError(
                        "Energy input ratio as a function of part-load ratio curve output is not equal to 1.0 (+ or - 10%) at reference conditions.");
                    ShowContinueError("Curve output at reference conditions = " + General::TrimSigDigits(CurveVal, 3));
                }
            }

            if (thisChiller.ChillerEIRFPLRIndex > 0) {
                bool FoundNegValue = false;
                Array1D<Real64> CurveValArray(11); // Used to evaluate PLFFPLR curve objects
                for (int CurveCheck = 0; CurveCheck <= 10; ++CurveCheck) {
                    Real64 CurveValTmp = CurveManager::CurveValue(thisChiller.ChillerEIRFPLRIndex, double(CurveCheck / 10.0));
                    if (CurveValTmp < 0.0) FoundNegValue = true;
                    CurveValArray(CurveCheck + 1) = int(CurveValTmp * 100.0) / 100.0;
                }
                std::string StringVar; // Used for EIRFPLR warning messages
                if (FoundNegValue) {
                    ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\"");
                    ShowContinueError("Energy input ratio as a function of part-load ratio curve shows negative values.");
                    ShowContinueError("EIR as a function of PLR curve output at various part-load ratios shown below:");
                    ShowContinueError("PLR          =    0.00   0.10   0.20   0.30   0.40   0.50   0.60   0.70   0.80   0.90   1.00");
                    StringVar = format("Curve Output = {:7.2F}", fmt::join(CurveValArray, ","));
                    puts(StringVar.c_str());
                    ShowContinueError(StringVar);
                    ErrorsFound = true;
                }
            }
            //   Basin heater power as a function of temperature must be greater than or equal to 0
            thisChiller.BasinHeaterPowerFTempDiff = DataIPShortCuts::rNumericArgs(16);
            if (DataIPShortCuts::rNumericArgs(16) < 0.0) {
                ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\"");
                ShowContinueError(DataIPShortCuts::cNumericFieldNames(16) + " must be >= 0");
                ErrorsFound = true;
            }

            thisChiller.BasinHeaterSetPointTemp = DataIPShortCuts::rNumericArgs(17);

            if (thisChiller.BasinHeaterPowerFTempDiff > 0.0) {
                if (NumNums < 17) {
                    thisChiller.BasinHeaterSetPointTemp = 2.0;
                }
                if (thisChiller.BasinHeaterSetPointTemp < 2.0) {
                    ShowWarningError(RoutineName + DataIPShortCuts::cCurrentModuleObject + " \"" + DataIPShortCuts::cAlphaArgs(1) + "\"");
                    ShowContinueError(DataIPShortCuts::cNumericFieldNames(17) + " is less than 2 deg C. Freezing could occur.");
                }
            }

            if (!DataIPShortCuts::lAlphaFieldBlanks(13)) {
                thisChiller.BasinHeaterSchedulePtr = ScheduleManager::GetScheduleIndex(DataIPShortCuts::cAlphaArgs(13));
                if (thisChiller.BasinHeaterSchedulePtr == 0) {
                    ShowWarningError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\"");
                    ShowWarningError(DataIPShortCuts::cAlphaFieldNames(13) + " \"" + DataIPShortCuts::cAlphaArgs(13) +
                                     "\" was not found. Basin heater operation will not be modeled and the simulation continues");
                }
            }

            if (NumAlphas > 15) {
                thisChiller.EndUseSubcategory = DataIPShortCuts::cAlphaArgs(16);
            } else {
                thisChiller.EndUseSubcategory = "General";
            }
        }

        if (ErrorsFound) {
            ShowFatalError("Errors found in processing input for " + DataIPShortCuts::cCurrentModuleObject);
        }
    }

    void ElectricEIRChillerSpecs::setupOutputVars()
    {
        SetupOutputVariable("Chiller Part Load Ratio", OutputProcessor::Unit::None, this->ChillerPartLoadRatio, "System", "Average", this->Name);

        SetupOutputVariable("Chiller Cycling Ratio", OutputProcessor::Unit::None, this->ChillerCyclingRatio, "System", "Average", this->Name);

        SetupOutputVariable("Chiller Electric Power", OutputProcessor::Unit::W, this->Power, "System", "Average", this->Name);

        SetupOutputVariable("Chiller Electric Energy",
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
                            this->EvapEnergy,
                            "System",
                            "Sum",
                            this->Name,
                            _,
                            "ENERGYTRANSFER",
                            "CHILLERS",
                            _,
                            "Plant");

        SetupOutputVariable(
            "Chiller False Load Heat Transfer Rate", OutputProcessor::Unit::W, this->ChillerFalseLoadRate, "System", "Average", this->Name);

        SetupOutputVariable("Chiller False Load Heat Transfer Energy", OutputProcessor::Unit::J, this->ChillerFalseLoad, "System", "Sum", this->Name);

        SetupOutputVariable("Chiller Evaporator Inlet Temperature", OutputProcessor::Unit::C, this->EvapInletTemp, "System", "Average", this->Name);

        SetupOutputVariable("Chiller Evaporator Outlet Temperature", OutputProcessor::Unit::C, this->EvapOutletTemp, "System", "Average", this->Name);

        SetupOutputVariable(
            "Chiller Evaporator Mass Flow Rate", OutputProcessor::Unit::kg_s, this->EvapMassFlowRate, "System", "Average", this->Name);

        SetupOutputVariable("Chiller Condenser Heat Transfer Rate", OutputProcessor::Unit::W, this->QCondenser, "System", "Average", this->Name);

        SetupOutputVariable("Chiller Condenser Heat Transfer Energy",
                            OutputProcessor::Unit::J,
                            this->CondEnergy,
                            "System",
                            "Sum",
                            this->Name,
                            _,
                            "ENERGYTRANSFER",
                            "HEATREJECTION",
                            _,
                            "Plant");

        SetupOutputVariable("Chiller COP", OutputProcessor::Unit::W_W, this->ActualCOP, "System", "Average", this->Name);

        SetupOutputVariable(
            "Chiller Capacity Temperature Modifier Multiplier", OutputProcessor::Unit::None, this->ChillerCapFT, "System", "Average", this->Name);

        SetupOutputVariable(
            "Chiller EIR Temperature Modifier Multiplier", OutputProcessor::Unit::None, this->ChillerEIRFT, "System", "Average", this->Name);

        SetupOutputVariable(
            "Chiller EIR Part Load Modifier Multiplier", OutputProcessor::Unit::None, this->ChillerEIRFPLR, "System", "Average", this->Name);

        // Condenser mass flow and outlet temp are valid for water cooled
        if (this->CondenserType == DataPlant::CondenserType::WATERCOOLED) {
            SetupOutputVariable(
                "Chiller Condenser Inlet Temperature", OutputProcessor::Unit::C, this->CondInletTemp, "System", "Average", this->Name);

            SetupOutputVariable(
                "Chiller Condenser Outlet Temperature", OutputProcessor::Unit::C, this->CondOutletTemp, "System", "Average", this->Name);

            SetupOutputVariable(
                "Chiller Condenser Mass Flow Rate", OutputProcessor::Unit::kg_s, this->CondMassFlowRate, "System", "Average", this->Name);

            // If heat recovery is active then setup report variables
            if (this->HeatRecActive) {
                SetupOutputVariable(
                    "Chiller Total Recovered Heat Rate", OutputProcessor::Unit::W, this->QHeatRecovered, "System", "Average", this->Name);

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

                SetupOutputVariable(
                    "Chiller Heat Recovery Inlet Temperature", OutputProcessor::Unit::C, this->HeatRecInletTemp, "System", "Average", this->Name);

                SetupOutputVariable(
                    "Chiller Heat Recovery Outlet Temperature", OutputProcessor::Unit::C, this->HeatRecOutletTemp, "System", "Average", this->Name);

                SetupOutputVariable(
                    "Chiller Heat Recovery Mass Flow Rate", OutputProcessor::Unit::kg_s, this->HeatRecMassFlow, "System", "Average", this->Name);

                SetupOutputVariable("Chiller Effective Heat Rejection Temperature",
                                    OutputProcessor::Unit::C,
                                    this->ChillerCondAvgTemp,
                                    "System",
                                    "Average",
                                    this->Name);
            }

        } else {
            SetupOutputVariable(
                "Chiller Condenser Inlet Temperature", OutputProcessor::Unit::C, this->CondInletTemp, "System", "Average", this->Name);

            if (this->CondenserFanPowerRatio > 0) {
                SetupOutputVariable(
                    "Chiller Condenser Fan Electric Power", OutputProcessor::Unit::W, this->CondenserFanPower, "System", "Average", this->Name);

                SetupOutputVariable("Chiller Condenser Fan Electric Energy",
                                    OutputProcessor::Unit::J,
                                    this->CondenserFanEnergyConsumption,
                                    "System",
                                    "Sum",
                                    this->Name,
                                    _,
                                    "ELECTRICITY",
                                    "Cooling",
                                    _,
                                    "Plant");
            }
            if (this->CondenserType == DataPlant::CondenserType::EVAPCOOLED) {
                SetupOutputVariable("Chiller Evaporative Condenser Water Volume",
                                    OutputProcessor::Unit::m3,
                                    this->EvapWaterConsump,
                                    "System",
                                    "Sum",
                                    this->Name,
                                    _,
                                    "Water",
                                    "Cooling",
                                    _,
                                    "System");

                SetupOutputVariable("Chiller Evaporative Condenser Mains Supply Water Volume",
                                    OutputProcessor::Unit::m3,
                                    this->EvapWaterConsump,
                                    "System",
                                    "Sum",
                                    this->Name,
                                    _,
                                    "MainsWater",
                                    "Cooling",
                                    _,
                                    "System");

                if (this->BasinHeaterPowerFTempDiff > 0.0) {
                    SetupOutputVariable(
                        "Chiller Basin Heater Electric Power", OutputProcessor::Unit::W, this->BasinHeaterPower, "System", "Average", this->Name);

                    SetupOutputVariable("Chiller Basin Heater Electric Energy",
                                        OutputProcessor::Unit::J,
                                        this->BasinHeaterConsumption,
                                        "System",
                                        "Sum",
                                        this->Name,
                                        _,
                                        "Electric",
                                        "CHILLERS",
                                        _,
                                        "Plant");
                }
            }
        }
        if (DataGlobals::AnyEnergyManagementSystemInModel) {
            SetupEMSInternalVariable("Chiller Nominal Capacity", this->Name, "[W]", this->RefCap);
        }
    }

    void ElectricEIRChillerSpecs::initialize(bool const RunFlag, Real64 const MyLoad)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   June 2004
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        //  This subroutine is for initializations of the Electric EIR Chiller variables

        // METHODOLOGY EMPLOYED:
        //  Uses the status flags to trigger initializations.

        static std::string const RoutineName("InitElectricEIRChiller");

        // Init more variables
        if (this->oneTimeFlag) {

            this->setupOutputVars();

            // Locate the chillers on the plant loops for later usage
            bool errFlag = false;
            PlantUtilities::ScanPlantLoopsForObject(this->Name,
                                                    DataPlant::TypeOf_Chiller_ElectricEIR,
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
                PlantUtilities::ScanPlantLoopsForObject(this->Name,
                                                        DataPlant::TypeOf_Chiller_ElectricEIR,
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
                    this->CWLoopNum, this->CWLoopSideNum, this->CDLoopNum, this->CDLoopSideNum, DataPlant::TypeOf_Chiller_ElectricEIR, true);
            }
            if (this->HeatRecActive) {
                PlantUtilities::ScanPlantLoopsForObject(this->Name,
                                                        DataPlant::TypeOf_Chiller_ElectricEIR,
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
                    this->CWLoopNum, this->CWLoopSideNum, this->HRLoopNum, this->HRLoopSideNum, DataPlant::TypeOf_Chiller_ElectricEIR, true);
            }

            if (this->CondenserType != DataPlant::CondenserType::AIRCOOLED && this->CondenserType != DataPlant::CondenserType::EVAPCOOLED && this->HeatRecActive) {
                PlantUtilities::InterConnectTwoPlantLoopSides(
                    this->CDLoopNum, this->CDLoopSideNum, this->HRLoopNum, this->HRLoopSideNum, DataPlant::TypeOf_Chiller_ElectricEIR, false);
            }

            if (errFlag) {
                ShowFatalError("InitElectricEIRChiller: Program terminated due to previous condition(s).");
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
                        bool fatalError = false; // but not really fatal yet, but should be.
                        EMSManager::CheckIfNodeSetPointManagedByEMS(this->EvapOutletNodeNum, EMSManager::iTemperatureSetPoint, fatalError);
                        if (fatalError) {
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
                    DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPoint =
                        DataLoopNode::Node(DataPlant::PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPoint;
                    DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPointHi =
                        DataLoopNode::Node(DataPlant::PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPointHi;
                }
            }
            this->oneTimeFlag = false;
        }

        this->EquipFlowCtrl = DataPlant::PlantLoop(this->CWLoopNum).LoopSide(this->CWLoopSideNum).Branch(this->CWBranchNum).Comp(this->CWCompNum).FlowCtrl;

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

            if (this->CondenserType == DataPlant::CondenserType::WATERCOOLED) {

                rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->CDLoopNum).FluidName,
                                                        this->TempRefCondIn,
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
                DataLoopNode::Node(this->CondInletNodeNum).Temp = this->TempRefCondIn;
            } else { // air or evap air condenser
                // Initialize maximum available condenser flow rate
                rho = Psychrometrics::PsyRhoAirFnPbTdbW(DataEnvironment::StdBaroPress, this->TempRefCondIn, 0.0, RoutineName);
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
                DataLoopNode::Node(this->CondInletNodeNum).Temp = this->TempRefCondIn;
            }

            if (this->HeatRecActive) {
                rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->HRLoopNum).FluidName,
                                                        DataGlobals::CWInitConvTemp,
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
                // overall capacity limit
                this->HeatRecMaxCapacityLimit = this->HeatRecCapacityFraction * (this->RefCap + this->RefCap / this->RefCOP);

                if (this->HeatRecSetPointNodeNum > 0) {
                    Real64 THeatRecSetPoint(0.0); // tests set point node for proper set point value
                    {
                        auto const SELECT_CASE_var(DataPlant::PlantLoop(this->HRLoopNum).LoopDemandCalcScheme);
                        if (SELECT_CASE_var == DataPlant::SingleSetPoint) {
                            THeatRecSetPoint = DataLoopNode::Node(this->HeatRecSetPointNodeNum).TempSetPoint;
                        } else if (SELECT_CASE_var == DataPlant::DualSetPointDeadBand) {
                            THeatRecSetPoint = DataLoopNode::Node(this->HeatRecSetPointNodeNum).TempSetPointHi;
                        } else {
                            assert(false);
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
                            bool fatalError = false; // but not really fatal yet, but should be.
                            EMSManager::CheckIfNodeSetPointManagedByEMS(this->EvapOutletNodeNum, EMSManager::iTemperatureSetPoint, fatalError);
                            if (fatalError) {
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
                    }     // IF(THeatRecSetPoint == SensedNodeFlagValue)THEN
                }         // IF(ElectricEIRChiller(EIRChillNum)%HeatRecSetPointNodeNum > 0)THEN
            }             // IF (ElectricEIRChiller(EIRChillNum)%HeatRecActive) THEN

            this->MyEnvrnFlag = false;
        }
        if (!DataGlobals::BeginEnvrnFlag) {
            this->MyEnvrnFlag = true;
        }

        if ((this->FlowMode == DataPlant::FlowMode::LEAVINGSETPOINTMODULATED) && this->ModulatedFlowSetToLoop) {
            // fix for clumsy old input that worked because loop setpoint was spread.
            //  could be removed with transition, testing , model change, period of being obsolete.
            DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPoint =
                DataLoopNode::Node(DataPlant::PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPoint;
            DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPointHi =
                DataLoopNode::Node(DataPlant::PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPointHi;
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
            int LoopNum = this->HRLoopNum;
            int LoopSideNum = this->HRLoopSideNum;
            int BranchIndex = this->HRBranchNum;
            int CompIndex = this->HRCompNum;
            if (RunFlag) {
                mdot = this->DesignHeatRecMassFlowRate;
            } else {
                mdot = 0.0;
            }

            PlantUtilities::SetComponentFlowRate(
                mdot, this->HeatRecInletNodeNum, this->HeatRecOutletNodeNum, LoopNum, LoopSideNum, BranchIndex, CompIndex);
        }

        if (this->CondenserType == DataPlant::CondenserType::EVAPCOOLED) {
            this->BasinHeaterPower = 0.0;
        }
    }

    void ElectricEIRChillerSpecs::size(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   June 2004
        //       MODIFIED       October 2013 Daeho Kang, add component sizing table entries
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        //  This subroutine is for sizing Electric EIR Chiller Components for which capacities and flow rates
        //  have not been specified in the input.

        // METHODOLOGY EMPLOYED:
        //  Obtains evaporator flow rate from the plant sizing array. Calculates reference capacity from
        //  the evaporator flow rate and the chilled water loop design delta T. The condenser flow rate
        //  is calculated from the reference capacity, the COP, and the condenser loop design delta T.

        static std::string const RoutineName("SizeElectricEIRChiller");

        int PltSizCondNum = 0;
        bool ErrorsFound = false;
        Real64 tmpNomCap = this->RefCap;
        Real64 tmpEvapVolFlowRate = this->EvapVolFlowRate;
        Real64 tmpCondVolFlowRate = this->CondVolFlowRate;

        if (this->CondenserType == DataPlant::CondenserType::WATERCOOLED) {
            PltSizCondNum = DataPlant::PlantLoop(this->CDLoopNum).PlantSizNum;
        }

        // find the appropriate Plant Sizing object
        int PltSizNum = DataPlant::PlantLoop(this->CWLoopNum).PlantSizNum;

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
                            "Chiller:Electric:EIR", this->Name, "Design Size Reference Chilled Water Flow Rate [m3/s]", tmpEvapVolFlowRate);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(
                            "Chiller:Electric:EIR", this->Name, "Initial Design Size Reference Chilled Water Flow Rate [m3/s]", tmpEvapVolFlowRate);
                    }
                } else { // Hard-size with sizing data
                    if (this->EvapVolFlowRate > 0.0 && tmpEvapVolFlowRate > 0.0) {
                        Real64 EvapVolFlowRateUser = this->EvapVolFlowRate;
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput("Chiller:Electric:EIR",
                                                                    this->Name,
                                                                    "Design Size Reference Chilled Water Flow Rate [m3/s]",
                                                                    tmpEvapVolFlowRate,
                                                                    "User-Specified Reference Chilled Water Flow Rate [m3/s]",
                                                                    EvapVolFlowRateUser);
                            if (DataGlobals::DisplayExtraWarnings) {
                                if ((std::abs(tmpEvapVolFlowRate - EvapVolFlowRateUser) / EvapVolFlowRateUser) >
                                    DataSizing::AutoVsHardSizingThreshold) {
                                    ShowMessage("SizeChillerElectricEIR: Potential issue with equipment sizing for " + this->Name);
                                    ShowContinueError("User-Specified Reference Chilled Water Flow Rate of " +
                                                      General::RoundSigDigits(EvapVolFlowRateUser, 5) + " [m3/s]");
                                    ShowContinueError("differs from Design Size Reference Chilled Water Flow Rate of " +
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
                ShowSevereError("Autosizing of Electric Chiller evap flow rate requires a loop Sizing:Plant object");
                ShowContinueError("Occurs in Electric Chiller object=" + this->Name);
                ErrorsFound = true;
            }
            if (!this->EvapVolFlowRateWasAutoSized && DataPlant::PlantFinalSizesOkayToReport && (this->EvapVolFlowRate > 0.0)) {
                ReportSizingManager::ReportSizingOutput(
                    "Chiller:Electric:EIR", this->Name, "User-Specified Reference Chilled Water Flow Rate [m3/s]", this->EvapVolFlowRate);
            }
        }

        PlantUtilities::RegisterPlantCompDesignFlow(this->EvapInletNodeNum, tmpEvapVolFlowRate);

        if (PltSizNum > 0) {
            if (DataSizing::PlantSizData(PltSizNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                Real64 Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(this->CWLoopNum).FluidName,
                                                                   DataGlobals::CWInitConvTemp,
                                                                   DataPlant::PlantLoop(this->CWLoopNum).FluidIndex,
                                                                   RoutineName);

                Real64 rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->CWLoopNum).FluidName,
                                                               DataGlobals::CWInitConvTemp,
                                                               DataPlant::PlantLoop(this->CWLoopNum).FluidIndex,
                                                               RoutineName);
                tmpNomCap = Cp * rho * DataSizing::PlantSizData(PltSizNum).DeltaT * tmpEvapVolFlowRate;
            } else {
                tmpNomCap = 0.0;
            }
            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                if (this->RefCapWasAutoSized) {
                    this->RefCap = tmpNomCap;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput("Chiller:Electric:EIR", this->Name, "Design Size Reference Capacity [W]", tmpNomCap);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(
                            "Chiller:Electric:EIR", this->Name, "Initial Design Size Reference Capacity [W]", tmpNomCap);
                    }
                } else { // Hard-sized with sizing data
                    if (this->RefCap > 0.0 && tmpNomCap > 0.0) {
                        Real64 RefCapUser = this->RefCap;
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput("Chiller:Electric:EIR",
                                                                    this->Name,
                                                                    "Design Size Reference Capacity [W]",
                                                                    tmpNomCap,
                                                                    "User-Specified Reference Capacity [W]",
                                                                    RefCapUser);
                            if (DataGlobals::DisplayExtraWarnings) {
                                if ((std::abs(tmpNomCap - RefCapUser) / RefCapUser) > DataSizing::AutoVsHardSizingThreshold) {
                                    ShowMessage("SizeChillerElectricEIR: Potential issue with equipment sizing for " + this->Name);
                                    ShowContinueError("User-Specified Reference Capacity of " + General::RoundSigDigits(RefCapUser, 2) + " [W]");
                                    ShowContinueError("differs from Design Size Reference Capacity of " + General::RoundSigDigits(tmpNomCap, 2) +
                                                      " [W]");
                                    ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                        }
                        tmpNomCap = RefCapUser;
                    }
                }
            }
        } else {
            if (this->RefCapWasAutoSized && DataPlant::PlantFirstSizesOkayToFinalize) {
                ShowSevereError("Autosizing of Electric Chiller reference capacity requires a loop Sizing:Plant object");
                ShowContinueError("Occurs in Electric Chiller object=" + this->Name);
                ErrorsFound = true;
            }
            if (!this->RefCapWasAutoSized && DataPlant::PlantFinalSizesOkayToReport && (this->RefCap > 0.0)) { // Hard-sized with no sizing data
                ReportSizingManager::ReportSizingOutput("Chiller:Electric:EIR", this->Name, "User-Specified Reference Capacity [W]", this->RefCap);
            }
        }

        if (PltSizCondNum > 0 && PltSizNum > 0) {
            if (DataSizing::PlantSizData(PltSizNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow && tmpNomCap > 0.0) {

                Real64 rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->CDLoopNum).FluidName,
                                                               DataGlobals::CWInitConvTemp,
                                                               DataPlant::PlantLoop(this->CDLoopNum).FluidIndex,
                                                               RoutineName);
                Real64 Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(this->CDLoopNum).FluidName,
                                                                   this->TempRefCondIn,
                                                                   DataPlant::PlantLoop(this->CDLoopNum).FluidIndex,
                                                                   RoutineName);
                tmpCondVolFlowRate = tmpNomCap * (1.0 + (1.0 / this->RefCOP) * this->CompPowerToCondenserFrac) /
                                     (DataSizing::PlantSizData(PltSizCondNum).DeltaT * Cp * rho);

            } else {
                if (this->CondVolFlowRateWasAutoSized) tmpCondVolFlowRate = 0.0;
            }
            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                if (this->CondVolFlowRateWasAutoSized) {
                    this->CondVolFlowRate = tmpCondVolFlowRate;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(
                            "Chiller:Electric:EIR", this->Name, "Design Size Reference Condenser Fluid Flow Rate [m3/s]", tmpCondVolFlowRate);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(
                            "Chiller:Electric:EIR", this->Name, "Initial Design Size Reference Condenser Fluid Flow Rate [m3/s]", tmpCondVolFlowRate);
                    }
                } else {
                    if (this->CondVolFlowRate > 0.0 && tmpCondVolFlowRate > 0.0) {
                        Real64 CondVolFlowRateUser = this->CondVolFlowRate;
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput("Chiller:Electric:EIR",
                                                                    this->Name,
                                                                    "Design Size Reference Condenser Fluid Flow Rate [m3/s]",
                                                                    tmpCondVolFlowRate,
                                                                    "User-Specified Reference Condenser Fluid Flow Rate [m3/s]",
                                                                    CondVolFlowRateUser);
                            if (DataGlobals::DisplayExtraWarnings) {
                                if ((std::abs(tmpCondVolFlowRate - CondVolFlowRateUser) / CondVolFlowRateUser) >
                                    DataSizing::AutoVsHardSizingThreshold) {
                                    ShowMessage("SizeChillerElectricEIR: Potential issue with equipment sizing for " + this->Name);
                                    ShowContinueError("User-Specified Reference Condenser Fluid Flow Rate of " +
                                                      General::RoundSigDigits(CondVolFlowRateUser, 5) + " [m3/s]");
                                    ShowContinueError("differs from Design Size Reference Condenser Fluid Flow Rate of " +
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
            if (this->CondenserType == DataPlant::CondenserType::WATERCOOLED) {

                if (this->CondVolFlowRateWasAutoSized && DataPlant::PlantFirstSizesOkayToFinalize) {
                    ShowSevereError("Autosizing of Electric EIR Chiller condenser fluid flow rate requires a condenser");
                    ShowContinueError("loop Sizing:Plant object");
                    ShowContinueError("Occurs in Electric EIR Chiller object=" + this->Name);
                    ErrorsFound = true;
                }
                if (!this->CondVolFlowRateWasAutoSized && DataPlant::PlantFinalSizesOkayToReport && (this->CondVolFlowRate > 0.0)) {
                    ReportSizingManager::ReportSizingOutput(
                        "Chiller:Electric:EIR", this->Name, "User-Specified Reference Condenser Fluid Flow Rate [m3/s]", this->CondVolFlowRate);
                }

            } else {

                // Auto size condenser air flow to Total Capacity * 0.000114 m3/s/w (850 cfm/ton)
                if (DataPlant::PlantFinalSizesOkayToReport) {
                    int SizingMethod = DataHVACGlobals::AutoCalculateSizing;
                    std::string CompType = DataPlant::ccSimPlantEquipTypes(DataPlant::TypeOf_Chiller_ElectricEIR);
                    std::string SizingString = "Reference Condenser Fluid Flow Rate  [m3/s]";
                    DataSizing::DataConstantUsedForSizing = this->RefCap;
                    DataSizing::DataFractionUsedForSizing = 0.000114;
                    Real64 TempSize = this->CondVolFlowRate;
                    bool bPRINT = true; // TRUE if sizing is reported to output (eio)
                    ReportSizingManager::RequestSizing(state, CompType, this->Name, SizingMethod, SizingString, TempSize, bPRINT, RoutineName);
                    this->CondVolFlowRate = TempSize;
                    DataSizing::DataConstantUsedForSizing = 0.0;
                    DataSizing::DataFractionUsedForSizing = 0.0;
                }
            }
        }

        // save the reference condenser water volumetric flow rate for use by the condenser water loop sizing algorithms
        PlantUtilities::RegisterPlantCompDesignFlow(this->CondInletNodeNum, tmpCondVolFlowRate);

        // now do heat recovery flow rate sizing if active
        if (this->HeatRecActive) {
            Real64 tempHeatRecVolFlowRate = tmpCondVolFlowRate * this->HeatRecCapacityFraction;
            if (this->DesignHeatRecVolFlowRateWasAutoSized) {

                if (DataPlant::PlantFirstSizesOkayToFinalize) {
                    this->DesignHeatRecVolFlowRate = tempHeatRecVolFlowRate;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(
                            "Chiller:Electric:EIR", this->Name, "Design Size Heat Recovery Water Flow Rate [m3/s]", tempHeatRecVolFlowRate);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(
                            "Chiller:Electric:EIR", this->Name, "Intial Design Size Heat Recovery Water Flow Rate [m3/s]", tempHeatRecVolFlowRate);
                    }
                }
            } else {
                if (this->DesignHeatRecVolFlowRate > 0.0 && tempHeatRecVolFlowRate > 0.0) {
                    Real64 nomHeatRecVolFlowRateUser = this->DesignHeatRecVolFlowRate;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        if (DataGlobals::DoPlantSizing) {
                            ReportSizingManager::ReportSizingOutput("Chiller:Electric:EIR",
                                                                    this->Name,
                                                                    "Design Size Heat Recovery Water Flow Rate [m3/s]",
                                                                    tempHeatRecVolFlowRate,
                                                                    "User-Specified Heat Recovery Water Flow Rate [m3/s]",
                                                                    nomHeatRecVolFlowRateUser);
                        } else {
                            ReportSizingManager::ReportSizingOutput(
                                "Chiller:Electric:EIR", this->Name, "User-Specified Heat Recovery Water Flow Rate [m3/s]", nomHeatRecVolFlowRateUser);
                        }

                        if (DataGlobals::DisplayExtraWarnings) {
                            if ((std::abs(tempHeatRecVolFlowRate - nomHeatRecVolFlowRateUser) / nomHeatRecVolFlowRateUser) >
                                DataSizing::AutoVsHardSizingThreshold) {
                                ShowMessage("SizeChillerElectricEIR: Potential issue with equipment sizing for " + this->Name);
                                ShowContinueError("User-Specified Heat Recovery Water Flow Rate of " +
                                                  General::RoundSigDigits(nomHeatRecVolFlowRateUser, 5) + " [m3/s]");
                                ShowContinueError("differs from Design Size Heat Recovery Water Flow Rate of " +
                                                  General::RoundSigDigits(tempHeatRecVolFlowRate, 5) + " [m3/s]");
                                ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                                ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                            }
                        }
                    }
                    tempHeatRecVolFlowRate = nomHeatRecVolFlowRateUser;
                }
            }
            if (!this->DesignHeatRecVolFlowRateWasAutoSized) tempHeatRecVolFlowRate = this->DesignHeatRecVolFlowRate;
            PlantUtilities::RegisterPlantCompDesignFlow(this->HeatRecInletNodeNum, tempHeatRecVolFlowRate);
        } // Heat recovery active

        if (DataPlant::PlantFinalSizesOkayToReport) {
            if (this->IPLVFlag) {
                Real64 IPLV;
                StandardRatings::CalcChillerIPLV(OutputFiles::getSingleton(),
                                                 this->Name,
                                                 DataPlant::TypeOf_Chiller_ElectricEIR,
                                                 this->RefCap,
                                                 this->RefCOP,
                                                 this->CondenserType,
                                                 this->ChillerCapFTIndex,
                                                 this->ChillerEIRFTIndex,
                                                 this->ChillerEIRFPLRIndex,
                                                 this->MinUnloadRat,
                                                 IPLV,
                                                 Optional<const Real64>(),
                                                 ObjexxFCL::Optional_int_const(),
                                                 Optional<const Real64>());
                this->IPLVFlag = false;
            }
            // create predefined report
            OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchMechType, this->Name, "Chiller:Electric:EIR");
            OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchMechNomEff, this->Name, this->RefCOP);
            OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchMechNomCap, this->Name, this->RefCap);
        }

        if (ErrorsFound) {
            ShowFatalError("Preceding sizing errors cause program termination");
        }
    }

    void ElectricEIRChillerSpecs::calculate(Real64 &MyLoad, bool const RunFlag)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   July 2004
        //       MODIFIED       Feb. 2010, Chandan Sharma, FSEC, Added basin heater
        //                      Jun. 2016, Rongpeng Zhang, Applied the chiller supply water temperature sensor fault model
        //                      Nov. 2016, Rongpeng Zhang, LBNL. Added Fouling Chiller fault
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        //  Simulate a vapor compression chiller using the DOE-2 model

        // METHODOLOGY EMPLOYED:
        //  Use empirical curve fits to model performance at off-reference conditions

        // REFERENCES:
        // 1. DOE-2 Engineers Manual, Version 2.1A, November 1982, LBL-11353


        static std::string const RoutineName("CalcElectricEIRChillerModel");

        Real64 EvapOutletTempSetPoint(0.0); // Evaporator outlet temperature setpoint [C]
        Real64 EvapDeltaTemp(0.0);          // Evaporator temperature difference [C]
        Real64 TempLoad(0.0);               // Actual load to be met by chiller. This value is compared to MyLoad
        // and reset when necessary since this chiller can cycle, the load passed
        // should be the actual load. Instead the minimum PLR * RefCap is
        // passed in. [W]
        Real64 CurrentEndTime;         // end time of time step for current simulation time step
        static std::string OutputChar; // character string for warning messages

        // Set module level inlet and outlet nodes and initialize other local variables
        this->CondMassFlowRate = 0.0;
        Real64 FRAC = 1.0; // Chiller cycling ratio

        // Set performance curve outputs to 0.0 when chiller is off
        this->ChillerCapFT = 0.0;
        this->ChillerEIRFT = 0.0;
        this->ChillerEIRFPLR = 0.0;

        // calculate end time of current time step
        CurrentEndTime = DataGlobals::CurrentTime + DataHVACGlobals::SysTimeElapsed;

        // Print warning messages only when valid and only for the first occurrence. Let summary provide statistics.
        // Wait for next time step to print warnings. If simulation iterates, print out
        // the warning for the last iteration only. Must wait for next time step to accomplish this.
        // If a warning occurs and the simulation down shifts, the warning is not valid.
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

        // If no loop demand or chiller OFF, return
        // If Chiller load is 0 or chiller is not running then leave the subroutine.Before leaving
        // if the component control is SERIESACTIVE we set the component flow to inlet flow so that
        // flow resolver will not shut down the branch
        if (MyLoad >= 0 || !RunFlag) {
            if (this->EquipFlowCtrl == DataBranchAirLoopPlant::ControlType_SeriesActive ||
                DataPlant::PlantLoop(this->CWLoopNum).LoopSide(this->CWLoopSideNum).FlowLock == 1) {
                this->EvapMassFlowRate = DataLoopNode::Node(this->EvapInletNodeNum).MassFlowRate;
            }
            if (this->CondenserType == DataPlant::CondenserType::WATERCOOLED) {
                if (DataPlant::PlantLoop(this->CDLoopNum).LoopSide(this->CDLoopSideNum).Branch(this->CDBranchNum).Comp(this->CDCompNum).FlowCtrl ==
                    DataBranchAirLoopPlant::ControlType_SeriesActive) {
                    this->CondMassFlowRate = DataLoopNode::Node(this->CondInletNodeNum).MassFlowRate;
                }
            }
            if (this->CondenserType == DataPlant::CondenserType::EVAPCOOLED) {
                CalcBasinHeaterPower(
                    this->BasinHeaterPowerFTempDiff, this->BasinHeaterSchedulePtr, this->BasinHeaterSetPointTemp, this->BasinHeaterPower);
            }
            this->PrintMessage = false;
            return;
        }

        // initialize outlet air humidity ratio of air or evap cooled chillers
        this->CondOutletHumRat = DataLoopNode::Node(this->CondInletNodeNum).HumRat;

        if (this->CondenserType == DataPlant::CondenserType::AIRCOOLED) { // Condenser inlet temp = outdoor temp
            DataLoopNode::Node(this->CondInletNodeNum).Temp = DataLoopNode::Node(this->CondInletNodeNum).OutAirDryBulb;

            // Warn user if entering condenser dry-bulb temperature falls below 0 C
            if (DataLoopNode::Node(this->CondInletNodeNum).Temp < 0.0 && std::abs(MyLoad) > 0 && RunFlag && !DataGlobals::WarmupFlag) {
                this->PrintMessage = true;

                this->MsgBuffer1 =
                    "ElectricEIRChillerModel - CHILLER:ELECTRIC:EIR \"" + this->Name + "\" - Air Cooled Condenser Inlet Temperature below 0C";
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
            this->CondOutletHumRat = Psychrometrics::PsyWFnTdbTwbPb(DataLoopNode::Node(this->CondInletNodeNum).Temp,
                                                                    DataLoopNode::Node(this->CondInletNodeNum).Temp,
                                                                    DataLoopNode::Node(this->CondInletNodeNum).Press);

            // Warn user if evap condenser wet-bulb temperature falls below 10 C
            if (DataLoopNode::Node(this->CondInletNodeNum).Temp < 10.0 && std::abs(MyLoad) > 0 && RunFlag && !DataGlobals::WarmupFlag) {
                this->PrintMessage = true;
                this->MsgBuffer1 =
                    "ElectricEIRChillerModel - CHILLER:ELECTRIC:EIR \"" + this->Name + "\" - Air Cooled Condenser Inlet Temperature below 10C";
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

        // LOAD LOCAL VARIABLES FROM DATA STRUCTURE (for code readability)
        Real64 ChillerRefCap = this->RefCap;
        Real64 ReferenceCOP = this->RefCOP;
        this->EvapOutletTemp = DataLoopNode::Node(this->EvapOutletNodeNum).Temp;
        Real64 TempLowLimitEout = this->TempLowLimitEvapOut;

        // If there is a fault of chiller fouling (zrp_Nov2016)
        if (this->FaultyChillerFoulingFlag && (!DataGlobals::WarmupFlag) && (!DataGlobals::DoingSizing) && (!DataGlobals::KickOffSimulation)) {
            int FaultIndex = this->FaultyChillerFoulingIndex;
            Real64 NomCap_ff = ChillerRefCap;
            Real64 ReferenceCOP_ff = ReferenceCOP;

            // calculate the Faulty Chiller Fouling Factor using fault information
            this->FaultyChillerFoulingFactor = FaultsManager::FaultsChillerFouling(FaultIndex).CalFoulingFactor();

            // update the Chiller nominal capacity and COP at faulty cases
            ChillerRefCap = NomCap_ff * this->FaultyChillerFoulingFactor;
            ReferenceCOP = ReferenceCOP_ff * this->FaultyChillerFoulingFactor;
        }

        // Set mass flow rates
        if (this->CondenserType == DataPlant::CondenserType::WATERCOOLED) {
            this->CondMassFlowRate = this->CondMassFlowRateMax;
            PlantUtilities::SetComponentFlowRate(this->CondMassFlowRate,
                                                 this->CondInletNodeNum,
                                                 this->CondOutletNodeNum,
                                                 this->CDLoopNum,
                                                 this->CDLoopSideNum,
                                                 this->CDBranchNum,
                                                 this->CDCompNum);
            PlantUtilities::PullCompInterconnectTrigger(this->CWLoopNum,
                                                        this->CWLoopSideNum,
                                                        this->CWBranchNum,
                                                        this->CWCompNum,
                                                        this->CondMassFlowIndex,
                                                        this->CDLoopNum,
                                                        this->CDLoopSideNum,
                                                        DataPlant::CriteriaType_MassFlowRate,
                                                        this->CondMassFlowRate);

            if (this->CondMassFlowRate < DataBranchAirLoopPlant::MassFlowTolerance) {
                if (this->EvapMassFlowRate < DataBranchAirLoopPlant::MassFlowTolerance) {
                    // Use PlantUtilities::SetComponentFlowRate to decide actual flow
                    PlantUtilities::SetComponentFlowRate(this->EvapMassFlowRate,
                                                         this->EvapInletNodeNum,
                                                         this->EvapOutletNodeNum,
                                                         this->CWLoopNum,
                                                         this->CWLoopSideNum,
                                                         this->CWBranchNum,
                                                         this->CWCompNum);
                }
                return;
            }
        }

        {
            auto const SELECT_CASE_var(DataPlant::PlantLoop(this->CWLoopNum).LoopDemandCalcScheme);
            if (SELECT_CASE_var == DataPlant::SingleSetPoint) {
                if ((this->FlowMode == DataPlant::FlowMode::LEAVINGSETPOINTMODULATED) ||
                    (DataPlant::PlantLoop(this->CWLoopNum)
                         .LoopSide(this->CWLoopSideNum)
                         .Branch(this->CWBranchNum)
                         .Comp(this->CWCompNum)
                         .CurOpSchemeType == DataPlant::CompSetPtBasedSchemeType) ||
                    (DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPoint != DataLoopNode::SensedNodeFlagValue)) {
                    // there will be a valid setpoint on outlet
                    EvapOutletTempSetPoint = DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPoint;
                } else { // use plant loop overall setpoint
                    EvapOutletTempSetPoint = DataLoopNode::Node(DataPlant::PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPoint;
                }
            } else if (SELECT_CASE_var == DataPlant::DualSetPointDeadBand) {
                if ((this->FlowMode == DataPlant::FlowMode::LEAVINGSETPOINTMODULATED) ||
                    (DataPlant::PlantLoop(this->CWLoopNum)
                         .LoopSide(this->CWLoopSideNum)
                         .Branch(this->CWBranchNum)
                         .Comp(this->CWCompNum)
                         .CurOpSchemeType == DataPlant::CompSetPtBasedSchemeType) ||
                    (DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPointHi != DataLoopNode::SensedNodeFlagValue)) {
                    // there will be a valid setpoint on outlet
                    EvapOutletTempSetPoint = DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPointHi;
                } else { // use plant loop overall setpoint
                    EvapOutletTempSetPoint = DataLoopNode::Node(DataPlant::PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPointHi;
                }
            } else {
                assert(false);
            }
        }

        // If there is a fault of Chiller SWT Sensor (zrp_Jun2016)
        if (this->FaultyChillerSWTFlag && (!DataGlobals::WarmupFlag) && (!DataGlobals::DoingSizing) && (!DataGlobals::KickOffSimulation)) {
            int FaultIndex = this->FaultyChillerSWTIndex;
            Real64 EvapOutletTempSetPoint_ff = EvapOutletTempSetPoint;

            // calculate the sensor offset using fault information
            this->FaultyChillerSWTOffset = FaultsManager::FaultsChillerSWTSensor(FaultIndex).CalFaultOffsetAct();
            // update the EvapOutletTempSetPoint
            EvapOutletTempSetPoint =
                max(this->TempLowLimitEvapOut,
                    min(DataLoopNode::Node(this->EvapInletNodeNum).Temp, EvapOutletTempSetPoint_ff - this->FaultyChillerSWTOffset));
            this->FaultyChillerSWTOffset = EvapOutletTempSetPoint_ff - EvapOutletTempSetPoint;
        }

        // correct temperature if using heat recovery
        // use report values for latest valid calculation, lagged somewhat
        Real64 AvgCondSinkTemp;
        if (this->HeatRecActive) {
            if ((this->QHeatRecovered + this->QCondenser) > 0.0) { // protect div by zero
                AvgCondSinkTemp = (this->QHeatRecovered * this->HeatRecInletTemp + this->QCondenser * this->CondInletTemp) /
                                  (this->QHeatRecovered + this->QCondenser);
            } else {
                AvgCondSinkTemp = condInletTemp;
            }
        } else {
            AvgCondSinkTemp = condInletTemp;
        }

        // Get capacity curve info with respect to CW setpoint and entering condenser water temps
        this->ChillerCapFT = CurveManager::CurveValue(this->ChillerCapFTIndex, EvapOutletTempSetPoint, AvgCondSinkTemp);

        if (this->ChillerCapFT < 0) {
            if (this->ChillerCapFTError < 1 && DataPlant::PlantLoop(this->CWLoopNum).LoopSide(this->CWLoopSideNum).FlowLock != 0 &&
                !DataGlobals::WarmupFlag) {
                ++this->ChillerCapFTError;
                ShowWarningError("CHILLER:ELECTRIC:EIR \"" + this->Name + "\":");
                ShowContinueError(" Chiller Capacity as a Function of Temperature curve output is negative (" +
                                  General::RoundSigDigits(this->ChillerCapFT, 3) + ").");
                ShowContinueError(" Negative value occurs using an Evaporator Outlet Temp of " + General::RoundSigDigits(EvapOutletTempSetPoint, 1) +
                                  " and a Condenser Inlet Temp of " + General::RoundSigDigits(condInletTemp, 1) + '.');
                ShowContinueErrorTimeStamp(" Resetting curve output to zero and continuing simulation.");
            } else if (DataPlant::PlantLoop(this->CWLoopNum).LoopSide(this->CWLoopSideNum).FlowLock != 0 && !DataGlobals::WarmupFlag) {
                ++this->ChillerCapFTError;
                ShowRecurringWarningErrorAtEnd("CHILLER:ELECTRIC:EIR \"" + this->Name +
                                                   "\": Chiller Capacity as a Function of Temperature curve output is negative warning continues...",
                                               this->ChillerCapFTErrorIndex,
                                               this->ChillerCapFT,
                                               this->ChillerCapFT);
            }
            this->ChillerCapFT = 0.0;
        }

        // Available chiller capacity as a function of temperature
        Real64 AvailChillerCap = ChillerRefCap * this->ChillerCapFT;

        // Only perform this check for temperature setpoint control
        if (DataPlant::PlantLoop(this->CWLoopNum).LoopSide(this->CWLoopSideNum).Branch(this->CWBranchNum).Comp(this->CWCompNum).CurOpSchemeType ==
            DataPlant::CompSetPtBasedSchemeType) {
            // Calculate water side load

            Real64 Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(this->CWLoopNum).FluidName,
                                                               DataLoopNode::Node(this->EvapInletNodeNum).Temp,
                                                               DataPlant::PlantLoop(this->CWLoopNum).FluidIndex,
                                                               RoutineName);
            this->EvapMassFlowRate = DataLoopNode::Node(this->EvapInletNodeNum).MassFlowRate;
            {
                auto const SELECT_CASE_var(DataPlant::PlantLoop(this->CWLoopNum).LoopDemandCalcScheme);
                if (SELECT_CASE_var == DataPlant::SingleSetPoint) {
                    TempLoad = this->EvapMassFlowRate * Cp *
                               (DataLoopNode::Node(this->EvapInletNodeNum).Temp - DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPoint);
                } else if (SELECT_CASE_var == DataPlant::DualSetPointDeadBand) {
                    TempLoad = this->EvapMassFlowRate * Cp *
                               (DataLoopNode::Node(this->EvapInletNodeNum).Temp - DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPointHi);
                } else {
                    assert(false);
                }
            }
            TempLoad = max(0.0, TempLoad);

            // MyLoad is capped at minimum PLR * RefCap, adjust load to actual water side load because this chiller can cycle
            if (std::abs(MyLoad) > TempLoad) {
                MyLoad = sign(TempLoad, MyLoad);
            }
        }

        // Part load ratio based on load and available chiller capacity, cap at max part load ratio
        Real64 PartLoadRat; // Operating part load ratio
        if (AvailChillerCap > 0) {
            PartLoadRat = max(0.0, min(std::abs(MyLoad) / AvailChillerCap, this->MaxPartLoadRat));
        } else {
            PartLoadRat = 0.0;
        }

        Real64 Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(this->CWLoopNum).FluidName,
                                                           DataLoopNode::Node(this->EvapInletNodeNum).Temp,
                                                           DataPlant::PlantLoop(this->CWLoopNum).FluidIndex,
                                                           RoutineName);

        if (DataPlant::PlantLoop(this->CWLoopNum).LoopSide(this->CWLoopSideNum).Branch(this->CWBranchNum).Comp(this->CWCompNum).CurOpSchemeType ==
            DataPlant::CompSetPtBasedSchemeType) {
            this->PossibleSubcooling = false;
        } else {
            this->PossibleSubcooling = true;
        }
        // Set evaporator heat transfer rate
        this->QEvaporator = AvailChillerCap * PartLoadRat;

        // Either set the flow to the Constant value or calculate the flow for the variable volume
        if ((this->FlowMode == DataPlant::FlowMode::CONSTANT) || (this->FlowMode == DataPlant::FlowMode::NOTMODULATED)) {
            // Set the evaporator mass flow rate to design
            // Start by assuming max (design) flow
            this->EvapMassFlowRate = this->EvapMassFlowRateMax;
            // Use PlantUtilities::SetComponentFlowRate to decide actual flow
            PlantUtilities::SetComponentFlowRate(this->EvapMassFlowRate,
                                                 this->EvapInletNodeNum,
                                                 this->EvapOutletNodeNum,
                                                 this->CWLoopNum,
                                                 this->CWLoopSideNum,
                                                 this->CWBranchNum,
                                                 this->CWCompNum);
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
                } else {
                    assert(false);
                }
            }

            if (EvapDeltaTemp != 0) {
                // Calculate desired flow to request based on load
                this->EvapMassFlowRate = std::abs(this->QEvaporator / Cp / EvapDeltaTemp);
                if ((this->EvapMassFlowRate - this->EvapMassFlowRateMax) > DataBranchAirLoopPlant::MassFlowTolerance) this->PossibleSubcooling = true;
                // Check to see if the Maximum is exceeded, if so set to maximum
                this->EvapMassFlowRate = min(this->EvapMassFlowRateMax, this->EvapMassFlowRate);
                // Use PlantUtilities::SetComponentFlowRate to decide actual flow
                PlantUtilities::SetComponentFlowRate(this->EvapMassFlowRate,
                                                     this->EvapInletNodeNum,
                                                     this->EvapOutletNodeNum,
                                                     this->CWLoopNum,
                                                     this->CWLoopSideNum,
                                                     this->CWBranchNum,
                                                     this->CWCompNum);
                // Should we recalculate this with the corrected setpoint?
                {
                    auto const SELECT_CASE_var(DataPlant::PlantLoop(this->CWLoopNum).LoopDemandCalcScheme);
                    if (SELECT_CASE_var == DataPlant::SingleSetPoint) {
                        this->EvapOutletTemp = DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPoint;
                    } else if (SELECT_CASE_var == DataPlant::DualSetPointDeadBand) {
                        this->EvapOutletTemp = DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPointHi;
                    }
                }
                this->QEvaporator = max(0.0, (this->EvapMassFlowRate * Cp * EvapDeltaTemp));
            } else {
                // Try to request zero flow
                this->EvapMassFlowRate = 0.0;
                // Use PlantUtilities::SetComponentFlowRate to decide actual flow
                PlantUtilities::SetComponentFlowRate(this->EvapMassFlowRate,
                                                     this->EvapInletNodeNum,
                                                     this->EvapOutletNodeNum,
                                                     this->CWLoopNum,
                                                     this->CWLoopSideNum,
                                                     this->CWBranchNum,
                                                     this->CWCompNum);
                // No deltaT since component is not running
                this->EvapOutletTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp;
                this->QEvaporator = 0.0;
                PartLoadRat = 0.0;
                this->ChillerPartLoadRatio = PartLoadRat;

                // DSU? so what if the delta T is zero?  On FlowLock==0, the inlet temp could = setpoint, right?
                if (this->DeltaTErrCount < 1 && !DataGlobals::WarmupFlag) {
                    ++this->DeltaTErrCount;
                    ShowWarningError("Evaporator DeltaTemp = 0 in mass flow calculation (Tevapin = Tsetpoint).");
                    ShowContinueErrorTimeStamp("");
                } else if (!DataGlobals::WarmupFlag) {
                    ++this->ChillerCapFTError;
                    ShowRecurringWarningErrorAtEnd("CHILLER:ELECTRIC:EIR \"" + this->Name +
                                                       "\": Evaporator DeltaTemp = 0 in mass flow calculation warning continues...",
                                                   this->DeltaTErrCountIndex,
                                                   EvapDeltaTemp,
                                                   EvapDeltaTemp);
                }
            }
        } // End of Constant Variable Flow If Block

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
        } else {
            EvapDeltaTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp - EvapOutletTempSetPoint;
            this->QEvaporator = max(0.0, (this->EvapMassFlowRate * Cp * EvapDeltaTemp));
            this->EvapOutletTemp = EvapOutletTempSetPoint;
        }

        // Check that the Evap outlet temp honors both plant loop temp low limit and also the chiller low limit
        if (this->EvapOutletTemp < TempLowLimitEout) {
            if ((DataLoopNode::Node(this->EvapInletNodeNum).Temp - TempLowLimitEout) > DataPlant::DeltaTempTol) {
                this->EvapOutletTemp = TempLowLimitEout;
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
            this->ChillerPartLoadRatio = PartLoadRat;
        }

        // Checks QEvaporator on the basis of the machine limits.
        if (this->QEvaporator > (AvailChillerCap * this->MaxPartLoadRat)) {
            if (this->EvapMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance) {
                this->QEvaporator = AvailChillerCap * this->MaxPartLoadRat;
                EvapDeltaTemp = this->QEvaporator / this->EvapMassFlowRate / Cp;
                // evaporator outlet temperature is allowed to float upwards (recalculate AvailChillerCap? iterate?)
                this->EvapOutletTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp - EvapDeltaTemp;
            } else {
                this->QEvaporator = 0.0;
                this->EvapOutletTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp;
            }
        }

        if (AvailChillerCap > 0.0) {
            PartLoadRat = max(0.0, min((this->QEvaporator / AvailChillerCap), this->MaxPartLoadRat));
        } else {
            PartLoadRat = 0.0;
        }

        // Chiller cycles below minimum part load ratio, FRAC = amount of time chiller is ON during this time step
        if (PartLoadRat < this->MinPartLoadRat) FRAC = min(1.0, (PartLoadRat / this->MinPartLoadRat));

        // set the module level variable used for reporting FRAC
        this->ChillerCyclingRatio = FRAC;

        // Chiller is false loading below PLR = minimum unloading ratio, find PLR used for energy calculation
        if (AvailChillerCap > 0.0) {
            PartLoadRat = max(PartLoadRat, this->MinUnloadRat);
        } else {
            PartLoadRat = 0.0;
        }

        // set the module level variable used for reporting PLR
        this->ChillerPartLoadRatio = PartLoadRat;

        // calculate the load due to false loading on chiller over and above water side load
        this->ChillerFalseLoadRate = (AvailChillerCap * PartLoadRat * FRAC) - this->QEvaporator;
        if (this->ChillerFalseLoadRate < DataHVACGlobals::SmallLoad) {
            this->ChillerFalseLoadRate = 0.0;
        }
        if (this->QEvaporator == 0.0 && this->CondenserType == DataPlant::CondenserType::EVAPCOOLED) {
            CalcBasinHeaterPower(
                this->BasinHeaterPowerFTempDiff, this->BasinHeaterSchedulePtr, this->BasinHeaterSetPointTemp, this->BasinHeaterPower);
        }

        this->ChillerEIRFT = CurveManager::CurveValue(this->ChillerEIRFTIndex, this->EvapOutletTemp, AvgCondSinkTemp);
        if (this->ChillerEIRFT < 0.0) {
            if (this->ChillerEIRFTError < 1 && DataPlant::PlantLoop(this->CWLoopNum).LoopSide(this->CWLoopSideNum).FlowLock != 0 &&
                !DataGlobals::WarmupFlag) {
                ++this->ChillerEIRFTError;
                ShowWarningError("CHILLER:ELECTRIC:EIR \"" + this->Name + "\":");
                ShowContinueError(" Chiller EIR as a Function of Temperature curve output is negative (" +
                                  General::RoundSigDigits(this->ChillerEIRFT, 3) + ").");
                ShowContinueError(" Negative value occurs using an Evaporator Outlet Temp of " + General::RoundSigDigits(this->EvapOutletTemp, 1) +
                                  " and a Condenser Inlet Temp of " + General::RoundSigDigits(condInletTemp, 1) + '.');
                ShowContinueErrorTimeStamp(" Resetting curve output to zero and continuing simulation.");
            } else if (DataPlant::PlantLoop(this->CWLoopNum).LoopSide(this->CWLoopSideNum).FlowLock != 0 && !DataGlobals::WarmupFlag) {
                ++this->ChillerEIRFTError;
                ShowRecurringWarningErrorAtEnd("CHILLER:ELECTRIC:EIR \"" + this->Name +
                                                   "\": Chiller EIR as a Function of Temperature curve output is negative warning continues...",
                                               this->ChillerEIRFTErrorIndex,
                                               this->ChillerEIRFT,
                                               this->ChillerEIRFT);
            }
            this->ChillerEIRFT = 0.0;
        }

        this->ChillerEIRFPLR = CurveManager::CurveValue(this->ChillerEIRFPLRIndex, PartLoadRat);
        if (this->ChillerEIRFPLR < 0.0) {
            if (this->ChillerEIRFPLRError < 1 && DataPlant::PlantLoop(this->CWLoopNum).LoopSide(this->CWLoopSideNum).FlowLock != 0 &&
                !DataGlobals::WarmupFlag) {
                ++this->ChillerEIRFPLRError;
                ShowWarningError("CHILLER:ELECTRIC:EIR \"" + this->Name + "\":");
                ShowContinueError(" Chiller EIR as a function of PLR curve output is negative (" + General::RoundSigDigits(this->ChillerEIRFPLR, 3) +
                                  ").");
                ShowContinueError(" Negative value occurs using a part-load ratio of " + General::RoundSigDigits(PartLoadRat, 3) + '.');
                ShowContinueErrorTimeStamp(" Resetting curve output to zero and continuing simulation.");
            } else if (DataPlant::PlantLoop(this->CWLoopNum).LoopSide(this->CWLoopSideNum).FlowLock != 0 && !DataGlobals::WarmupFlag) {
                ++this->ChillerEIRFPLRError;
                ShowRecurringWarningErrorAtEnd("CHILLER:ELECTRIC:EIR \"" + this->Name +
                                                   "\": Chiller EIR as a function of PLR curve output is negative warning continues...",
                                               this->ChillerEIRFPLRErrorIndex,
                                               this->ChillerEIRFPLR,
                                               this->ChillerEIRFPLR);
            }
            this->ChillerEIRFPLR = 0.0;
        }

        this->Power = (AvailChillerCap / ReferenceCOP) * this->ChillerEIRFPLR * this->ChillerEIRFT * FRAC;

        this->QCondenser = this->Power * this->CompPowerToCondenserFrac + this->QEvaporator + this->ChillerFalseLoadRate;

        if (this->CondenserType == DataPlant::CondenserType::WATERCOOLED) {
            if (this->CondMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance) {
                // If Heat Recovery specified for this vapor compression chiller, then Qcondenser will be adjusted by this subroutine
                if (this->HeatRecActive) this->calcHeatRecovery(this->QCondenser, this->CondMassFlowRate, condInletTemp, this->QHeatRecovered);
                Cp = FluidProperties::GetSpecificHeatGlycol(
                    DataPlant::PlantLoop(this->CDLoopNum).FluidName, condInletTemp, DataPlant::PlantLoop(this->CDLoopNum).FluidIndex, RoutineName);

                this->CondOutletTemp = this->QCondenser / this->CondMassFlowRate / Cp + condInletTemp;
            } else {
                ShowSevereError("CalcElectricEIRChillerModel: Condenser flow = 0, for ElectricEIRChiller=" + this->Name);
                ShowContinueErrorTimeStamp("");
                // DSU? maybe this could be handled earlier, check if this component has a load and an evap flow rate
                // then if cond flow is zero, just make a request to the condenser,
                // then just say it couldn't run until condenser loop wakes up.
                // CALL ShowFatalError('Program Terminates due to previous error condition.')
            }
        } else { // Air Cooled or Evap Cooled

            if (this->QCondenser > 0.0) {
                this->CondMassFlowRate = this->CondMassFlowRateMax * PartLoadRat;
            } else {
                this->CondMassFlowRate = 0.0;
            }

            // If Heat Recovery specified for this vapor compression chiller, then Qcondenser will be adjusted by this subroutine
            if (this->HeatRecActive) this->calcHeatRecovery(this->QCondenser, this->CondMassFlowRate, condInletTemp, this->QHeatRecovered);

            if (CondMassFlowRate > 0.0) {
                Cp = Psychrometrics::PsyCpAirFnW(DataLoopNode::Node(this->CondInletNodeNum).HumRat);
                CondOutletTemp = CondInletTemp + QCondenser / CondMassFlowRate / Cp;
            } else {
                this->CondOutletTemp = condInletTemp;
            }

            if (this->CondenserType == DataPlant::CondenserType::EVAPCOOLED) {
                Real64 const RhoWater = Psychrometrics::RhoH2O(DataGlobals::InitConvTemp);
                // CondMassFlowRate is already multiplied by PLR, convert to water use rate
                this->EvapWaterConsumpRate =
                    ((this->CondOutletHumRat - DataLoopNode::Node(this->CondInletNodeNum).HumRat) * this->CondMassFlowRate) / RhoWater;
            }
        }

        // Calculate condenser fan power
        if (this->ChillerCapFT > 0.0) {
            this->CondenserFanPower = ChillerRefCap * this->CondenserFanPowerRatio * FRAC;
        } else {
            this->CondenserFanPower = 0.0;
        }
    }

    void ElectricEIRChillerSpecs::calcHeatRecovery(Real64 &QCond,              // Current condenser load [W]
                                                   Real64 const CondMassFlow,  // Current condenser mass flow [kg/s]
                                                   Real64 const condInletTemp, // Current condenser inlet temp [C]
                                                   Real64 &QHeatRec            // Amount of heat recovered [W]
    )
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Richard Liesen
        //       DATE WRITTEN:    January 2004
        //       MODIFIED:        Richard Raustad, FSEC (occurrences of EIR only, calcs are identical to electric chiller)

        // PURPOSE OF THIS SUBROUTINE:
        //  Calculate the heat recovered from the chiller condenser

        static std::string const RoutineName("EIRChillerHeatRecovery");

        // Inlet node to the heat recovery heat exchanger
        Real64 heatRecInletTemp = DataLoopNode::Node(this->HeatRecInletNodeNum).Temp;
        Real64 HeatRecMassFlowRate = DataLoopNode::Node(this->HeatRecInletNodeNum).MassFlowRate;

        Real64 CpHeatRec = FluidProperties::GetSpecificHeatGlycol(
            DataPlant::PlantLoop(this->HRLoopNum).FluidName, heatRecInletTemp, DataPlant::PlantLoop(this->HRLoopNum).FluidIndex, RoutineName);
        Real64 CpCond = FluidProperties::GetSpecificHeatGlycol(
            DataPlant::PlantLoop(this->CDLoopNum).FluidName, condInletTemp, DataPlant::PlantLoop(this->CDLoopNum).FluidIndex, RoutineName);

        // Before we modify the QCondenser, the total or original value is transferred to QTot
        Real64 QTotal = QCond;

        if (this->HeatRecSetPointNodeNum == 0) { // use original algorithm that blends temps
            Real64 TAvgIn = (HeatRecMassFlowRate * CpHeatRec * heatRecInletTemp + CondMassFlow * CpCond * condInletTemp) /
                            (HeatRecMassFlowRate * CpHeatRec + CondMassFlow * CpCond);

            Real64 TAvgOut = QTotal / (HeatRecMassFlowRate * CpHeatRec + CondMassFlow * CpCond) + TAvgIn;

            QHeatRec = HeatRecMassFlowRate * CpHeatRec * (TAvgOut - heatRecInletTemp);
            QHeatRec = max(QHeatRec, 0.0); // ensure non negative
            // check if heat flow too large for physical size of bundle
            QHeatRec = min(QHeatRec, this->HeatRecMaxCapacityLimit);
        } else {                          // use new algorithm to meet setpoint
            Real64 THeatRecSetPoint(0.0); // local value for heat recovery leaving setpoint [C]
            {
                auto const SELECT_CASE_var(DataPlant::PlantLoop(this->HRLoopNum).LoopDemandCalcScheme);

                if (SELECT_CASE_var == DataPlant::SingleSetPoint) {
                    THeatRecSetPoint = DataLoopNode::Node(this->HeatRecSetPointNodeNum).TempSetPoint;
                } else if (SELECT_CASE_var == DataPlant::DualSetPointDeadBand) {
                    THeatRecSetPoint = DataLoopNode::Node(this->HeatRecSetPointNodeNum).TempSetPointHi;
                } else {
                    assert(false);
                }
            }

            // load to heat recovery setpoint
            Real64 QHeatRecToSetPoint = HeatRecMassFlowRate * CpHeatRec * (THeatRecSetPoint - heatRecInletTemp);
            QHeatRecToSetPoint = max(QHeatRecToSetPoint, 0.0);
            QHeatRec = min(QTotal, QHeatRecToSetPoint);
            // check if heat flow too large for physical size of bundle
            QHeatRec = min(QHeatRec, this->HeatRecMaxCapacityLimit);
        }

        // check if limit on inlet is present and exceeded.
        if (this->HeatRecInletLimitSchedNum > 0) {
            Real64 HeatRecHighInletLimit = ScheduleManager::GetCurrentScheduleValue(this->HeatRecInletLimitSchedNum);
            if (heatRecInletTemp > HeatRecHighInletLimit) { // shut down heat recovery
                QHeatRec = 0.0;
            }
        }

        QCond = QTotal - QHeatRec;

        // Calculate a new Heat Recovery Coil Outlet Temp
        if (HeatRecMassFlowRate > 0.0) {
            this->HeatRecOutletTemp = QHeatRec / (HeatRecMassFlowRate * CpHeatRec) + heatRecInletTemp;
        } else {
            this->HeatRecOutletTemp = heatRecInletTemp;
        }
    }

    void ElectricEIRChillerSpecs::update(Real64 const MyLoad, bool const RunFlag)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Richard Raustad, FSEC
        //       DATE WRITTEN:    June 2004

        // PURPOSE OF THIS SUBROUTINE:
        //  Reporting

        // Number of seconds per HVAC system time step, to convert from W (J/s) to J
        Real64 ReportingConstant = DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

        if (MyLoad >= 0 || !RunFlag) { // Chiller not running so pass inlet states to outlet states
            // Set node conditions
            DataLoopNode::Node(this->EvapOutletNodeNum).Temp = DataLoopNode::Node(this->EvapInletNodeNum).Temp;
            DataLoopNode::Node(this->CondOutletNodeNum).Temp = DataLoopNode::Node(this->CondInletNodeNum).Temp;
            if (this->CondenserType != DataPlant::CondenserType::WATERCOOLED) {
                DataLoopNode::Node(this->CondOutletNodeNum).HumRat = DataLoopNode::Node(this->CondInletNodeNum).HumRat;
                DataLoopNode::Node(this->CondOutletNodeNum).Enthalpy = DataLoopNode::Node(this->CondInletNodeNum).Enthalpy;
                DataLoopNode::Node(this->CondInletNodeNum).MassFlowRate = 0.0;
                DataLoopNode::Node(this->CondOutletNodeNum).MassFlowRate = 0.0;
            }

            this->ChillerPartLoadRatio = 0.0;
            this->ChillerCyclingRatio = 0.0;
            this->ChillerFalseLoadRate = 0.0;
            this->ChillerFalseLoad = 0.0;
            this->Power = 0.0;
            this->QEvaporator = 0.0;
            this->QCondenser = 0.0;
            this->Energy = 0.0;
            this->EvapEnergy = 0.0;
            this->CondEnergy = 0.0;
            this->EvapInletTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp;
            this->CondInletTemp = DataLoopNode::Node(this->CondInletNodeNum).Temp;
            this->CondOutletTemp = DataLoopNode::Node(this->CondOutletNodeNum).Temp;
            this->EvapOutletTemp = DataLoopNode::Node(this->EvapOutletNodeNum).Temp;
            this->ActualCOP = 0.0;
            this->CondenserFanPower = 0.0;
            this->CondenserFanEnergyConsumption = 0.0;
            if (this->CondenserType == DataPlant::CondenserType::EVAPCOOLED) {
                this->BasinHeaterConsumption = this->BasinHeaterPower * ReportingConstant;
                this->EvapWaterConsump = 0.0;
            }

            if (this->HeatRecActive) {

                PlantUtilities::SafeCopyPlantNode(this->HeatRecInletNodeNum, this->HeatRecOutletNodeNum);

                this->QHeatRecovered = 0.0;
                this->EnergyHeatRecovery = 0.0;
                this->HeatRecInletTemp = DataLoopNode::Node(this->HeatRecInletNodeNum).Temp;
                this->HeatRecOutletTemp = DataLoopNode::Node(this->HeatRecOutletNodeNum).Temp;
                this->HeatRecMassFlow = DataLoopNode::Node(this->HeatRecInletNodeNum).MassFlowRate;
            }

        } else { // Chiller is running, so pass calculated values
            // Set node temperatures
            if (this->CondMassFlowRate < DataBranchAirLoopPlant::MassFlowTolerance &&
                this->EvapMassFlowRate < DataBranchAirLoopPlant::MassFlowTolerance) {
                DataLoopNode::Node(this->EvapOutletNodeNum).Temp = DataLoopNode::Node(this->EvapInletNodeNum).Temp;
                DataLoopNode::Node(this->CondOutletNodeNum).Temp = DataLoopNode::Node(this->CondInletNodeNum).Temp;
                if (this->CondenserType != DataPlant::CondenserType::WATERCOOLED) {
                    DataLoopNode::Node(this->CondOutletNodeNum).HumRat = DataLoopNode::Node(this->CondInletNodeNum).HumRat;
                    DataLoopNode::Node(this->CondOutletNodeNum).Enthalpy = DataLoopNode::Node(this->CondInletNodeNum).Enthalpy;
                    DataLoopNode::Node(this->CondInletNodeNum).MassFlowRate = 0.0;
                    DataLoopNode::Node(this->CondOutletNodeNum).MassFlowRate = 0.0;
                }
            } else {
                DataLoopNode::Node(this->EvapOutletNodeNum).Temp = this->EvapOutletTemp;
                DataLoopNode::Node(this->CondOutletNodeNum).Temp = this->CondOutletTemp;
                if (this->CondenserType != DataPlant::CondenserType::WATERCOOLED) {
                    DataLoopNode::Node(this->CondOutletNodeNum).HumRat = this->CondOutletHumRat;
                    DataLoopNode::Node(this->CondOutletNodeNum).Enthalpy = Psychrometrics::PsyHFnTdbW(this->CondOutletTemp, this->CondOutletHumRat);
                    DataLoopNode::Node(this->CondInletNodeNum).MassFlowRate = this->CondMassFlowRate;
                    DataLoopNode::Node(this->CondOutletNodeNum).MassFlowRate = this->CondMassFlowRate;
                }
            }

            // Set node flow rates;  for these load based models
            // assume that sufficient evaporator flow rate is available
            this->ChillerFalseLoad = this->ChillerFalseLoadRate * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
            this->Energy = this->Power * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
            this->EvapEnergy = this->QEvaporator * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
            this->CondEnergy = this->QCondenser * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
            this->EvapInletTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp;
            this->CondInletTemp = DataLoopNode::Node(this->CondInletNodeNum).Temp;
            this->CondOutletTemp = DataLoopNode::Node(this->CondOutletNodeNum).Temp;
            this->EvapOutletTemp = DataLoopNode::Node(this->EvapOutletNodeNum).Temp;
            this->CondenserFanEnergyConsumption = this->CondenserFanPower * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
            if (this->Power != 0.0) {
                this->ActualCOP = (this->QEvaporator + this->ChillerFalseLoadRate) / this->Power;
            } else {
                this->ActualCOP = 0.0;
            }
            if (this->CondenserType == DataPlant::CondenserType::EVAPCOOLED) {
                this->BasinHeaterConsumption = this->BasinHeaterPower * ReportingConstant;
                this->EvapWaterConsump = this->EvapWaterConsumpRate * ReportingConstant;
            }

            if (this->HeatRecActive) {

                PlantUtilities::SafeCopyPlantNode(this->HeatRecInletNodeNum, this->HeatRecOutletNodeNum);
                this->EnergyHeatRecovery = this->QHeatRecovered * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
                DataLoopNode::Node(this->HeatRecOutletNodeNum).Temp = this->HeatRecOutletTemp;
                this->HeatRecInletTemp = DataLoopNode::Node(this->HeatRecInletNodeNum).Temp;
                this->HeatRecMassFlow = DataLoopNode::Node(this->HeatRecInletNodeNum).MassFlowRate;
            }
        }
    }

} // namespace ChillerElectricEIR

} // namespace EnergyPlus
