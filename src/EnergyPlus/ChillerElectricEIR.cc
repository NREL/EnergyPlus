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
#include <ObjexxFCL/gio.hh>
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
#include <EnergyPlus/DataPlant.hh>
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
    
    // Chiller type parameters
    int const AirCooled(1);
    int const WaterCooled(2);
    int const EvapCooled(3);

    // chiller flow modes
    int const FlowModeNotSet(200);
    int const ConstantFlow(201);
    int const NotModulated(202);
    int const LeavingSetPointModulated(203);

    static std::string const BlankString;

    // MODULE VARIABLE DECLARATIONS:
    int NumElectricEIRChillers(0);    // Number of electric EIR chillers specified in input

    Array1D_bool CheckEquipName;

    bool getInputFlag(true); // When TRUE, calls subroutine to read input file.

    // Object Data
    Array1D<ElectricEIRChillerSpecs> ElectricEIRChiller; // Dimension to number of machines

    // Functions
    void clear_state()
    {
        NumElectricEIRChillers = 0; // Number of electric EIR chillers specified in input
        CheckEquipName.deallocate();
        getInputFlag = true;
        ElectricEIRChiller.deallocate();
    }

    void SimElectricEIRChiller(std::string const &EP_UNUSED(EIRChillerType), // Type of chiller
                               std::string const &EIRChillerName,            // User specified name of chiller
                               int const EquipFlowCtrl,                      // Flow control mode for the equipment
                               int &CompIndex,                               // Chiller number pointer
                               int const LoopNum,                            // plant loop index pointer
                               bool const RunFlag,                           // Simulate chiller when TRUE
                               bool const FirstIteration,                    // Initialize variables when TRUE
                               bool &InitLoopEquip,                          // If not zero, calculate the max load for operating conditions
                               Real64 &MyLoad,                               // Loop demand component will meet
                               Real64 &MaxCap,                               // Maximum operating capacity of chiller [W]
                               Real64 &MinCap,                               // Minimum operating capacity of chiller [W]
                               Real64 &OptCap,                               // Optimal operating capacity of chiller [W]
                               bool const GetSizingFactor,                   // TRUE when just the sizing factor is requested
                               Real64 &SizingFactor,                         // sizing factor
                               Real64 &TempCondInDesign,
                               Real64 &TempEvapOutDesign)
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

        int EIRChillNum; // Chiller number pointer

        if (getInputFlag) {
            GetElectricEIRChillerInput();
            getInputFlag = false;
        }

        // Find the correct Chiller
        if (CompIndex == 0) {
            EIRChillNum = UtilityRoutines::FindItemInList(EIRChillerName, ElectricEIRChiller);
            if (EIRChillNum == 0) {
                ShowFatalError("SimElectricEIRChiller: Specified Chiller not one of Valid EIR Electric Chillers=" + EIRChillerName);
            }
            CompIndex = EIRChillNum;
        } else {
            EIRChillNum = CompIndex;
            if (EIRChillNum > NumElectricEIRChillers || EIRChillNum < 1) {
                ShowFatalError("SimElectricEIRChiller:  Invalid CompIndex passed=" + General::TrimSigDigits(EIRChillNum) +
                               ", Number of Units=" + General::TrimSigDigits(NumElectricEIRChillers) + ", Entered Unit name=" + EIRChillerName);
            }
            if (CheckEquipName(EIRChillNum)) {
                if (EIRChillerName != ElectricEIRChiller(EIRChillNum).Name) {
                    ShowFatalError("SimElectricEIRChiller: Invalid CompIndex passed=" + General::TrimSigDigits(EIRChillNum) + ", Unit name=" + EIRChillerName +
                                   ", stored Unit Name for that index=" + ElectricEIRChiller(EIRChillNum).Name);
                }
                CheckEquipName(EIRChillNum) = false;
            }
        }

        if (InitLoopEquip) {
            TempEvapOutDesign = ElectricEIRChiller(EIRChillNum).TempRefEvapOut;
            TempCondInDesign = ElectricEIRChiller(EIRChillNum).TempRefCondIn;

            InitElectricEIRChiller(EIRChillNum, RunFlag, MyLoad);

            if (LoopNum == ElectricEIRChiller(EIRChillNum).CWLoopNum) {
                SizeElectricEIRChiller(EIRChillNum);
                MinCap = ElectricEIRChiller(EIRChillNum).RefCap * ElectricEIRChiller(EIRChillNum).MinPartLoadRat;
                MaxCap = ElectricEIRChiller(EIRChillNum).RefCap * ElectricEIRChiller(EIRChillNum).MaxPartLoadRat;
                OptCap = ElectricEIRChiller(EIRChillNum).RefCap * ElectricEIRChiller(EIRChillNum).OptPartLoadRat;
            } else {
                MinCap = 0.0;
                MaxCap = 0.0;
                OptCap = 0.0;
            }
            if (GetSizingFactor) {
                SizingFactor = ElectricEIRChiller(EIRChillNum).SizFac;
            }
            return;
        }

        if (LoopNum == ElectricEIRChiller(EIRChillNum).CWLoopNum) {
            InitElectricEIRChiller(EIRChillNum, RunFlag, MyLoad);
            CalcElectricEIRChillerModel(EIRChillNum, MyLoad, RunFlag, FirstIteration, EquipFlowCtrl);
            UpdateElectricEIRChillerRecords(MyLoad, RunFlag, EIRChillNum);

        } else if (LoopNum == ElectricEIRChiller(EIRChillNum).CDLoopNum) {
            PlantUtilities::UpdateChillerComponentCondenserSide(LoopNum,
                    ElectricEIRChiller(EIRChillNum).CDLoopSideNum,
                                                DataPlant::TypeOf_Chiller_ElectricEIR,
                                                ElectricEIRChiller(EIRChillNum).CondInletNodeNum,
                                                ElectricEIRChiller(EIRChillNum).CondOutletNodeNum,
                                                ElectricEIRChiller(EIRChillNum).QCondenser,
                                                ElectricEIRChiller(EIRChillNum).CondInletTemp,
                                                ElectricEIRChiller(EIRChillNum).CondOutletTemp,
                                                ElectricEIRChiller(EIRChillNum).CondMassFlowRate,
                                                FirstIteration);

        } else if (LoopNum == ElectricEIRChiller(EIRChillNum).HRLoopNum) {
            PlantUtilities::UpdateComponentHeatRecoverySide(ElectricEIRChiller(EIRChillNum).HRLoopNum,
                                            ElectricEIRChiller(EIRChillNum).HRLoopSideNum,
                                            DataPlant::TypeOf_Chiller_ElectricEIR,
                                            ElectricEIRChiller(EIRChillNum).HeatRecInletNodeNum,
                                            ElectricEIRChiller(EIRChillNum).HeatRecOutletNodeNum,
                                            ElectricEIRChiller(EIRChillNum).QHeatRecovered,
                                            ElectricEIRChiller(EIRChillNum).HeatRecInletTemp,
                                            ElectricEIRChiller(EIRChillNum).HeatRecOutletTemp,
                                            ElectricEIRChiller(EIRChillNum).HeatRecMassFlow,
                                            FirstIteration);
        }
    }

    void GetElectricEIRChillerInput()
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Richard Raustad, FSEC
        //       DATE WRITTEN:    June 2004

        // PURPOSE OF THIS SUBROUTINE:
        //  This routine will get the input required by the Electric EIR Chiller model.
        
        static std::string const RoutineName("GetElectricEIRChillerInput: "); // include trailing blank space

        bool ErrorsFound(false);    // True when input errors are found

        // Formats
        static ObjexxFCL::gio::Fmt Format_530("('Curve Output = ',11(F7.2))");

        DataIPShortCuts::cCurrentModuleObject = "Chiller:Electric:EIR";
        NumElectricEIRChillers = inputProcessor->getNumObjectsFound(DataIPShortCuts::cCurrentModuleObject);

        if (NumElectricEIRChillers <= 0) {
            ShowSevereError("No " + DataIPShortCuts::cCurrentModuleObject + " equipment specified in input file");
            ErrorsFound = true;
        }

        // ALLOCATE ARRAYS
        ElectricEIRChiller.allocate(NumElectricEIRChillers);
        CheckEquipName.dimension(NumElectricEIRChillers, true);

        // Load arrays with electric EIR chiller data
        for (int EIRChillerNum = 1; EIRChillerNum <= NumElectricEIRChillers; ++EIRChillerNum) {
            int NumAlphas;                     // Number of elements in the alpha array
            int NumNums;                       // Number of elements in the numeric array
            int IOStat;                        // IO Status when calling get input subroutine
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
            GlobalNames::VerifyUniqueChillerName(DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), ErrorsFound, DataIPShortCuts::cCurrentModuleObject + " Name");

            ElectricEIRChiller(EIRChillerNum).Name = DataIPShortCuts::cAlphaArgs(1);

            //   Performance curves
            ElectricEIRChiller(EIRChillerNum).ChillerCapFTIndex = CurveManager::GetCurveIndex(DataIPShortCuts::cAlphaArgs(2));
            if (ElectricEIRChiller(EIRChillerNum).ChillerCapFTIndex == 0) {
                ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + " \"" + DataIPShortCuts::cAlphaArgs(1) + "\"");
                ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(2) + '=' + DataIPShortCuts::cAlphaArgs(2));
                ErrorsFound = true;
            }

            ElectricEIRChiller(EIRChillerNum).ChillerEIRFTIndex = CurveManager::GetCurveIndex(DataIPShortCuts::cAlphaArgs(3));
            if (ElectricEIRChiller(EIRChillerNum).ChillerEIRFTIndex == 0) {
                ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\"");
                ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(3) + '=' + DataIPShortCuts::cAlphaArgs(3));
                ErrorsFound = true;
            }

            ElectricEIRChiller(EIRChillerNum).ChillerEIRFPLRIndex = CurveManager::GetCurveIndex(DataIPShortCuts::cAlphaArgs(4));
            if (ElectricEIRChiller(EIRChillerNum).ChillerEIRFPLRIndex == 0) {
                ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\"");
                ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(4) + '=' + DataIPShortCuts::cAlphaArgs(4));
                ErrorsFound = true;
            }

            ElectricEIRChiller(EIRChillerNum).EvapInletNodeNum = NodeInputManager::GetOnlySingleNode(
                DataIPShortCuts::cAlphaArgs(5), ErrorsFound, DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Inlet, 1, DataLoopNode::ObjectIsNotParent);
            ElectricEIRChiller(EIRChillerNum).EvapOutletNodeNum = NodeInputManager::GetOnlySingleNode(
                DataIPShortCuts::cAlphaArgs(6), ErrorsFound, DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Outlet, 1, DataLoopNode::ObjectIsNotParent);
            BranchNodeConnections::TestCompSet(DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataIPShortCuts::cAlphaArgs(5), DataIPShortCuts::cAlphaArgs(6), "Chilled Water Nodes");

            if (UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(9), "WaterCooled")) {
                ElectricEIRChiller(EIRChillerNum).CondenserType = WaterCooled;
            } else if (UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(9), "AirCooled")) {
                ElectricEIRChiller(EIRChillerNum).CondenserType = AirCooled;
            } else if (UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(9), "EvaporativelyCooled")) {
                ElectricEIRChiller(EIRChillerNum).CondenserType = EvapCooled;
            } else {
                ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + ": " + DataIPShortCuts::cAlphaArgs(1));
                ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(9) + '=' + DataIPShortCuts::cAlphaArgs(9));
                ShowContinueError("Valid entries are AirCooled, WaterCooled, or EvaporativelyCooled");
                ErrorsFound = true;
            }

            if (ElectricEIRChiller(EIRChillerNum).CondenserType == AirCooled || ElectricEIRChiller(EIRChillerNum).CondenserType == EvapCooled) {
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

                ElectricEIRChiller(EIRChillerNum).CondInletNodeNum = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(7),
                                                                                       ErrorsFound,
                                                                                       DataIPShortCuts::cCurrentModuleObject,
                                                                                       DataIPShortCuts::cAlphaArgs(1),
                                                                                       DataLoopNode::NodeType_Air,
                                                                                       DataLoopNode::NodeConnectionType_OutsideAirReference,
                                                                                       2,
                                                                                       DataLoopNode::ObjectIsNotParent);
                bool Okay;
                OutAirNodeManager::CheckAndAddAirNodeNumber(ElectricEIRChiller(EIRChillerNum).CondInletNodeNum, Okay);
                if (!Okay) {
                    ShowWarningError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\"");
                    ShowContinueError("Adding OutdoorAir:Node=" + DataIPShortCuts::cAlphaArgs(7));
                }

                ElectricEIRChiller(EIRChillerNum).CondOutletNodeNum = NodeInputManager::GetOnlySingleNode(
                    DataIPShortCuts::cAlphaArgs(8), ErrorsFound, DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataLoopNode::NodeType_Air, DataLoopNode::NodeConnectionType_Outlet, 2, DataLoopNode::ObjectIsNotParent);

            } else if (ElectricEIRChiller(EIRChillerNum).CondenserType == WaterCooled) {
                // Condenser inlet node name is necessary for water-cooled condenser
                if (DataIPShortCuts::lAlphaFieldBlanks(7) || DataIPShortCuts::lAlphaFieldBlanks(8)) {
                    ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\"");
                    ShowContinueError("Condenser Inlet or Outlet Node Name is blank.");
                    ErrorsFound = true;
                }

                ElectricEIRChiller(EIRChillerNum).CondInletNodeNum = NodeInputManager::GetOnlySingleNode(
                    DataIPShortCuts::cAlphaArgs(7), ErrorsFound, DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Inlet, 2, DataLoopNode::ObjectIsNotParent);

                ElectricEIRChiller(EIRChillerNum).CondOutletNodeNum = NodeInputManager::GetOnlySingleNode(
                    DataIPShortCuts::cAlphaArgs(8), ErrorsFound, DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Outlet, 2, DataLoopNode::ObjectIsNotParent);

                BranchNodeConnections::TestCompSet(DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataIPShortCuts::cAlphaArgs(7), DataIPShortCuts::cAlphaArgs(8), "Condenser Water Nodes");

            } else {
                // Condenser inlet node name is necessary (never should reach this part of code)
                if (DataIPShortCuts::lAlphaFieldBlanks(7) || DataIPShortCuts::lAlphaFieldBlanks(8)) {
                    ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\"");
                    ShowContinueError("Condenser Inlet or Outlet Node Name is blank.");
                    ErrorsFound = true;
                }
                ElectricEIRChiller(EIRChillerNum).CondInletNodeNum = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(7),
                                                                                       ErrorsFound,
                                                                                       DataIPShortCuts::cCurrentModuleObject,
                                                                                       DataIPShortCuts::cAlphaArgs(1),
                                                                                       DataLoopNode::NodeType_Unknown,
                                                                                       DataLoopNode::NodeConnectionType_Inlet,
                                                                                       2,
                                                                                       DataLoopNode::ObjectIsNotParent);

                ElectricEIRChiller(EIRChillerNum).CondOutletNodeNum = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(8),
                                                                                        ErrorsFound,
                                                                                        DataIPShortCuts::cCurrentModuleObject,
                                                                                        DataIPShortCuts::cAlphaArgs(1),
                                                                                        DataLoopNode::NodeType_Unknown,
                                                                                        DataLoopNode::NodeConnectionType_Outlet,
                                                                                        2,
                                                                                        DataLoopNode::ObjectIsNotParent);

                BranchNodeConnections::TestCompSet(DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataIPShortCuts::cAlphaArgs(7), DataIPShortCuts::cAlphaArgs(8), "Condenser (unknown?) Nodes");
            }

            {
                auto const SELECT_CASE_var(DataIPShortCuts::cAlphaArgs(10));
                if (SELECT_CASE_var == "CONSTANTFLOW") {
                    ElectricEIRChiller(EIRChillerNum).FlowMode = ConstantFlow;
                } else if (SELECT_CASE_var == "LEAVINGSETPOINTMODULATED") {
                    ElectricEIRChiller(EIRChillerNum).FlowMode = LeavingSetPointModulated;
                } else if (SELECT_CASE_var == "NOTMODULATED") {
                    ElectricEIRChiller(EIRChillerNum).FlowMode = NotModulated;
                } else {
                    ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\",");
                    ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(10) + '=' + DataIPShortCuts::cAlphaArgs(10));
                    ShowContinueError("Available choices are ConstantFlow, NotModulated, or LeavingSetpointModulated");
                    ShowContinueError("Flow mode NotModulated is assumed and the simulation continues.");
                    ElectricEIRChiller(EIRChillerNum).FlowMode = NotModulated;
                }
            }

            //   Chiller rated performance data
            ElectricEIRChiller(EIRChillerNum).RefCap = DataIPShortCuts::rNumericArgs(1);
            if (ElectricEIRChiller(EIRChillerNum).RefCap == DataSizing::AutoSize) {
                ElectricEIRChiller(EIRChillerNum).RefCapWasAutoSized = true;
            }
            if (DataIPShortCuts::rNumericArgs(1) == 0.0) {
                ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\"");
                ShowContinueError("Invalid " + DataIPShortCuts::cNumericFieldNames(1) + '=' + General::RoundSigDigits(DataIPShortCuts::rNumericArgs(1), 2));
                ErrorsFound = true;
            }
            ElectricEIRChiller(EIRChillerNum).RefCOP = DataIPShortCuts::rNumericArgs(2);
            if (DataIPShortCuts::rNumericArgs(2) == 0.0) {
                ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\"");
                ShowContinueError("Invalid " + DataIPShortCuts::cNumericFieldNames(2) + '=' + General::RoundSigDigits(DataIPShortCuts::rNumericArgs(2), 2));
                ErrorsFound = true;
            }
            ElectricEIRChiller(EIRChillerNum).TempRefEvapOut = DataIPShortCuts::rNumericArgs(3);
            ElectricEIRChiller(EIRChillerNum).TempRefCondIn = DataIPShortCuts::rNumericArgs(4);
            ElectricEIRChiller(EIRChillerNum).EvapVolFlowRate = DataIPShortCuts::rNumericArgs(5);
            if (ElectricEIRChiller(EIRChillerNum).EvapVolFlowRate == DataSizing::AutoSize) {
                ElectricEIRChiller(EIRChillerNum).EvapVolFlowRateWasAutoSized = true;
            }
            ElectricEIRChiller(EIRChillerNum).CondVolFlowRate = DataIPShortCuts::rNumericArgs(6);
            if (ElectricEIRChiller(EIRChillerNum).CondVolFlowRate == DataSizing::AutoSize) {
                ElectricEIRChiller(EIRChillerNum).CondVolFlowRateWasAutoSized = true;
            }

            ElectricEIRChiller(EIRChillerNum).MinPartLoadRat = DataIPShortCuts::rNumericArgs(7);
            ElectricEIRChiller(EIRChillerNum).MaxPartLoadRat = DataIPShortCuts::rNumericArgs(8);
            ElectricEIRChiller(EIRChillerNum).OptPartLoadRat = DataIPShortCuts::rNumericArgs(9);
            ElectricEIRChiller(EIRChillerNum).MinUnloadRat = DataIPShortCuts::rNumericArgs(10);
            ElectricEIRChiller(EIRChillerNum).SizFac = DataIPShortCuts::rNumericArgs(15);
            if (ElectricEIRChiller(EIRChillerNum).SizFac <= 0.0) ElectricEIRChiller(EIRChillerNum).SizFac = 1.0;

            if (ElectricEIRChiller(EIRChillerNum).MinPartLoadRat > ElectricEIRChiller(EIRChillerNum).MaxPartLoadRat) {
                ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\"");
                ShowContinueError(DataIPShortCuts::cNumericFieldNames(7) + " [" + General::RoundSigDigits(DataIPShortCuts::rNumericArgs(7), 3) + "] > " + DataIPShortCuts::cNumericFieldNames(8) + " [" +
                                  General::RoundSigDigits(DataIPShortCuts::rNumericArgs(8), 3) + ']');
                ShowContinueError("Minimum part load ratio must be less than or equal to the maximum part load ratio ");
                ErrorsFound = true;
            }

            if (ElectricEIRChiller(EIRChillerNum).MinUnloadRat < ElectricEIRChiller(EIRChillerNum).MinPartLoadRat ||
                ElectricEIRChiller(EIRChillerNum).MinUnloadRat > ElectricEIRChiller(EIRChillerNum).MaxPartLoadRat) {
                ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\"");
                ShowContinueError(DataIPShortCuts::cNumericFieldNames(10) + " = " + General::RoundSigDigits(DataIPShortCuts::rNumericArgs(10), 3));
                ShowContinueError(DataIPShortCuts::cNumericFieldNames(10) + " must be greater than or equal to the " + DataIPShortCuts::cNumericFieldNames(7));
                ShowContinueError(DataIPShortCuts::cNumericFieldNames(10) + " must be less than or equal to the " + DataIPShortCuts::cNumericFieldNames(8));
                ErrorsFound = true;
            }

            if (ElectricEIRChiller(EIRChillerNum).OptPartLoadRat < ElectricEIRChiller(EIRChillerNum).MinPartLoadRat ||
                ElectricEIRChiller(EIRChillerNum).OptPartLoadRat > ElectricEIRChiller(EIRChillerNum).MaxPartLoadRat) {
                ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\"");
                ShowContinueError(DataIPShortCuts::cNumericFieldNames(9) + " = " + General::RoundSigDigits(DataIPShortCuts::rNumericArgs(9), 3));
                ShowContinueError(DataIPShortCuts::cNumericFieldNames(9) + " must be greater than or equal to the " + DataIPShortCuts::cNumericFieldNames(7));
                ShowContinueError(DataIPShortCuts::cNumericFieldNames(9) + " must be less than or equal to the " + DataIPShortCuts::cNumericFieldNames(8));
                ErrorsFound = true;
            }

            ElectricEIRChiller(EIRChillerNum).CondenserFanPowerRatio = DataIPShortCuts::rNumericArgs(11);
            ElectricEIRChiller(EIRChillerNum).CompPowerToCondenserFrac = DataIPShortCuts::rNumericArgs(12);

            if (ElectricEIRChiller(EIRChillerNum).CompPowerToCondenserFrac < 0.0 ||
                ElectricEIRChiller(EIRChillerNum).CompPowerToCondenserFrac > 1.0) {
                ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\"");
                ShowContinueError(DataIPShortCuts::cNumericFieldNames(12) + " = " + General::RoundSigDigits(DataIPShortCuts::rNumericArgs(12), 3));
                ShowContinueError(DataIPShortCuts::cNumericFieldNames(12) + " must be greater than or equal to zero");
                ShowContinueError(DataIPShortCuts::cNumericFieldNames(12) + " must be less than or equal to one");
                ErrorsFound = true;
            }

            ElectricEIRChiller(EIRChillerNum).TempLowLimitEvapOut = DataIPShortCuts::rNumericArgs(13);

            // These are the heat recovery inputs
            ElectricEIRChiller(EIRChillerNum).DesignHeatRecVolFlowRate = DataIPShortCuts::rNumericArgs(14);
            if (ElectricEIRChiller(EIRChillerNum).DesignHeatRecVolFlowRate == DataSizing::AutoSize) {
                ElectricEIRChiller(EIRChillerNum).DesignHeatRecVolFlowRateWasAutoSized = true;
            }
            if ((ElectricEIRChiller(EIRChillerNum).DesignHeatRecVolFlowRate > 0.0) ||
                (ElectricEIRChiller(EIRChillerNum).DesignHeatRecVolFlowRate == DataSizing::AutoSize)) {
                ElectricEIRChiller(EIRChillerNum).HeatRecActive = true;
                ElectricEIRChiller(EIRChillerNum).HeatRecInletNodeNum = NodeInputManager::GetOnlySingleNode(
                    DataIPShortCuts::cAlphaArgs(11), ErrorsFound, DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Inlet, 3, DataLoopNode::ObjectIsNotParent);
                if (ElectricEIRChiller(EIRChillerNum).HeatRecInletNodeNum == 0) {
                    ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\"");
                    ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(11) + '=' + DataIPShortCuts::cAlphaArgs(11));
                    ErrorsFound = true;
                }
                ElectricEIRChiller(EIRChillerNum).HeatRecOutletNodeNum = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(12),
                                                                                           ErrorsFound,
                                                                                           DataIPShortCuts::cCurrentModuleObject,
                                                                                           DataIPShortCuts::cAlphaArgs(1),
                                                                                           DataLoopNode::NodeType_Water,
                                                                                           DataLoopNode::NodeConnectionType_Outlet,
                                                                                           3,
                                                                                           DataLoopNode::ObjectIsNotParent);
                if (ElectricEIRChiller(EIRChillerNum).HeatRecOutletNodeNum == 0) {
                    ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\"");
                    ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(12) + '=' + DataIPShortCuts::cAlphaArgs(12));
                    ErrorsFound = true;
                }
                if (ElectricEIRChiller(EIRChillerNum).CondenserType != WaterCooled) {
                    ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\"");
                    ShowContinueError("Heat Recovery requires a Water Cooled Condenser.");
                    ErrorsFound = true;
                }

                BranchNodeConnections::TestCompSet(DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataIPShortCuts::cAlphaArgs(11), DataIPShortCuts::cAlphaArgs(12), "Heat Recovery Nodes");
                // store heat recovery volume flow for plant sizing
                if (ElectricEIRChiller(EIRChillerNum).DesignHeatRecVolFlowRate > 0.0) {
                    PlantUtilities::RegisterPlantCompDesignFlow(ElectricEIRChiller(EIRChillerNum).HeatRecInletNodeNum,
                                                ElectricEIRChiller(EIRChillerNum).DesignHeatRecVolFlowRate); // CR 6953
                }
                if (NumNums > 17) {
                    if (!DataIPShortCuts::lNumericFieldBlanks(18)) {
                        ElectricEIRChiller(EIRChillerNum).HeatRecCapacityFraction = DataIPShortCuts::rNumericArgs(18);
                    } else {
                        ElectricEIRChiller(EIRChillerNum).HeatRecCapacityFraction = 1.0;
                    }
                } else {
                    ElectricEIRChiller(EIRChillerNum).HeatRecCapacityFraction = 1.0;
                }

                if (NumAlphas > 13) {
                    if (!DataIPShortCuts::lAlphaFieldBlanks(14)) {
                        ElectricEIRChiller(EIRChillerNum).HeatRecInletLimitSchedNum = ScheduleManager::GetScheduleIndex(DataIPShortCuts::cAlphaArgs(14));
                        if (ElectricEIRChiller(EIRChillerNum).HeatRecInletLimitSchedNum == 0) {
                            ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\"");
                            ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(14) + '=' + DataIPShortCuts::cAlphaArgs(14));
                            ErrorsFound = true;
                        }
                    } else {
                        ElectricEIRChiller(EIRChillerNum).HeatRecInletLimitSchedNum = 0;
                    }
                } else {
                    ElectricEIRChiller(EIRChillerNum).HeatRecInletLimitSchedNum = 0;
                }

                if (NumAlphas > 14) {
                    if (!DataIPShortCuts::lAlphaFieldBlanks(15)) {
                        ElectricEIRChiller(EIRChillerNum).HeatRecSetPointNodeNum = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(15),
                                                                                                     ErrorsFound,
                                                                                                     DataIPShortCuts::cCurrentModuleObject,
                                                                                                     DataIPShortCuts::cAlphaArgs(1),
                                                                                                     DataLoopNode::NodeType_Water,
                                                                                                     DataLoopNode::NodeConnectionType_Sensor,
                                                                                                     1,
                                                                                                     DataLoopNode::ObjectIsNotParent);
                    } else {
                        ElectricEIRChiller(EIRChillerNum).HeatRecSetPointNodeNum = 0;
                    }
                } else {
                    ElectricEIRChiller(EIRChillerNum).HeatRecSetPointNodeNum = 0;
                }

            } else {
                ElectricEIRChiller(EIRChillerNum).HeatRecActive = false;
                ElectricEIRChiller(EIRChillerNum).DesignHeatRecMassFlowRate = 0.0;
                ElectricEIRChiller(EIRChillerNum).HeatRecInletNodeNum = 0;
                ElectricEIRChiller(EIRChillerNum).HeatRecOutletNodeNum = 0;
                if (!DataIPShortCuts::lAlphaFieldBlanks(11) || !DataIPShortCuts::lAlphaFieldBlanks(12)) {
                    //  IF (DataIPShortCuts::cAlphaArgs(11) /= ' ' .or. DataIPShortCuts::cAlphaArgs(12) /= ' ') THEN
                    ShowWarningError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\"");
                    ShowContinueError("Since Reference Heat Reclaim Volume Flow Rate = 0.0, heat recovery is inactive.");
                    ShowContinueError("However, node names were specified for heat recovery inlet or outlet nodes.");
                }
            }

            //   Check the CAP-FT, EIR-FT, and PLR curves and warn user if different from 1.0 by more than +-10%
            if (ElectricEIRChiller(EIRChillerNum).ChillerCapFTIndex > 0) {
                Real64 CurveVal = CurveManager::CurveValue(ElectricEIRChiller(EIRChillerNum).ChillerCapFTIndex,
                                      ElectricEIRChiller(EIRChillerNum).TempRefEvapOut,
                                      ElectricEIRChiller(EIRChillerNum).TempRefCondIn);
                if (CurveVal > 1.10 || CurveVal < 0.90) {
                    ShowWarningError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\"");
                    ShowContinueError(
                        "Capacity ratio as a function of temperature curve output is not equal to 1.0 (+ or - 10%) at reference conditions.");
                    ShowContinueError("Curve output at reference conditions = " + General::TrimSigDigits(CurveVal, 3));
                }
            }

            if (ElectricEIRChiller(EIRChillerNum).ChillerEIRFTIndex > 0) {
                Real64 CurveVal = CurveManager::CurveValue(ElectricEIRChiller(EIRChillerNum).ChillerEIRFTIndex,
                                      ElectricEIRChiller(EIRChillerNum).TempRefEvapOut,
                                      ElectricEIRChiller(EIRChillerNum).TempRefCondIn);
                if (CurveVal > 1.10 || CurveVal < 0.90) {
                    ShowWarningError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\"");
                    ShowContinueError(
                        "Energy input ratio as a function of temperature curve output is not equal to 1.0 (+ or - 10%) at reference conditions.");
                    ShowContinueError("Curve output at reference conditions = " + General::TrimSigDigits(CurveVal, 3));
                }
            }

            if (ElectricEIRChiller(EIRChillerNum).ChillerEIRFPLRIndex > 0) {
                Real64 CurveVal = CurveManager::CurveValue(ElectricEIRChiller(EIRChillerNum).ChillerEIRFPLRIndex, 1.0);

                if (CurveVal > 1.10 || CurveVal < 0.90) {
                    ShowWarningError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\"");
                    ShowContinueError(
                        "Energy input ratio as a function of part-load ratio curve output is not equal to 1.0 (+ or - 10%) at reference conditions.");
                    ShowContinueError("Curve output at reference conditions = " + General::TrimSigDigits(CurveVal, 3));
                }
            }

            if (ElectricEIRChiller(EIRChillerNum).ChillerEIRFPLRIndex > 0) {
                bool FoundNegValue = false;
                Array1D<Real64> CurveValArray(11); // Used to evaluate PLFFPLR curve objects
                for (int CurveCheck = 0; CurveCheck <= 10; ++CurveCheck) {
                    Real64 CurveValTmp = CurveManager::CurveValue(ElectricEIRChiller(EIRChillerNum).ChillerEIRFPLRIndex, double(CurveCheck / 10.0));
                    if (CurveValTmp < 0.0) FoundNegValue = true;
                    CurveValArray(CurveCheck + 1) = int(CurveValTmp * 100.0) / 100.0;
                }
                std::string StringVar;             // Used for EIRFPLR warning messages
                if (FoundNegValue) {
                    ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\"");
                    ShowContinueError("Energy input ratio as a function of part-load ratio curve shows negative values.");
                    ShowContinueError("EIR as a function of PLR curve output at various part-load ratios shown below:");
                    ShowContinueError("PLR          =    0.00   0.10   0.20   0.30   0.40   0.50   0.60   0.70   0.80   0.90   1.00");
                    ObjexxFCL::gio::write(StringVar, "'Curve Output = '");
                    static ObjexxFCL::gio::Fmt fmtF72("((F7.2),$)");
                    for (int CurveValPtr = 1; CurveValPtr <= 11; ++CurveValPtr) {
                        ObjexxFCL::gio::write(StringVar, fmtF72) << CurveValArray(CurveValPtr);
                    }
                    ObjexxFCL::gio::write(StringVar);
                    ShowContinueError(StringVar);
                    ErrorsFound = true;
                }
            }
            //   Basin heater power as a function of temperature must be greater than or equal to 0
            ElectricEIRChiller(EIRChillerNum).BasinHeaterPowerFTempDiff = DataIPShortCuts::rNumericArgs(16);
            if (DataIPShortCuts::rNumericArgs(16) < 0.0) {
                ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\"");
                ShowContinueError(DataIPShortCuts::cNumericFieldNames(16) + " must be >= 0");
                ErrorsFound = true;
            }

            ElectricEIRChiller(EIRChillerNum).BasinHeaterSetPointTemp = DataIPShortCuts::rNumericArgs(17);

            if (ElectricEIRChiller(EIRChillerNum).BasinHeaterPowerFTempDiff > 0.0) {
                if (NumNums < 17) {
                    ElectricEIRChiller(EIRChillerNum).BasinHeaterSetPointTemp = 2.0;
                }
                if (ElectricEIRChiller(EIRChillerNum).BasinHeaterSetPointTemp < 2.0) {
                    ShowWarningError(RoutineName + DataIPShortCuts::cCurrentModuleObject + " \"" + DataIPShortCuts::cAlphaArgs(1) + "\"");
                    ShowContinueError(DataIPShortCuts::cNumericFieldNames(17) + " is less than 2 deg C. Freezing could occur.");
                }
            }

            if (!DataIPShortCuts::lAlphaFieldBlanks(13)) {
                ElectricEIRChiller(EIRChillerNum).BasinHeaterSchedulePtr = ScheduleManager::GetScheduleIndex(DataIPShortCuts::cAlphaArgs(13));
                if (ElectricEIRChiller(EIRChillerNum).BasinHeaterSchedulePtr == 0) {
                    ShowWarningError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\"");
                    ShowWarningError(DataIPShortCuts::cAlphaFieldNames(13) + " \"" + DataIPShortCuts::cAlphaArgs(13) +
                                     "\" was not found. Basin heater operation will not be modeled and the simulation continues");
                }
            }

            if (NumAlphas > 15) {
                ElectricEIRChiller(EIRChillerNum).EndUseSubcategory = DataIPShortCuts::cAlphaArgs(16);
            } else {
                ElectricEIRChiller(EIRChillerNum).EndUseSubcategory = "General";
            }
        }

        if (ErrorsFound) {
            ShowFatalError("Errors found in processing input for " + DataIPShortCuts::cCurrentModuleObject);
        }

        for (int EIRChillerNum = 1; EIRChillerNum <= NumElectricEIRChillers; ++EIRChillerNum) {
            SetupOutputVariable("Chiller Part Load Ratio",
                                OutputProcessor::Unit::None,
                                ElectricEIRChiller(EIRChillerNum).ChillerPartLoadRatio,
                                "System",
                                "Average",
                                ElectricEIRChiller(EIRChillerNum).Name);
            SetupOutputVariable("Chiller Cycling Ratio",
                                OutputProcessor::Unit::None,
                                ElectricEIRChiller(EIRChillerNum).ChillerCyclingRatio,
                                "System",
                                "Average",
                                ElectricEIRChiller(EIRChillerNum).Name);
            SetupOutputVariable("Chiller Electric Power",
                                OutputProcessor::Unit::W,
                                ElectricEIRChiller(EIRChillerNum).Power,
                                "System",
                                "Average",
                                ElectricEIRChiller(EIRChillerNum).Name);
            SetupOutputVariable("Chiller Electric Energy",
                                OutputProcessor::Unit::J,
                                ElectricEIRChiller(EIRChillerNum).Energy,
                                "System",
                                "Sum",
                                ElectricEIRChiller(EIRChillerNum).Name,
                                _,
                                "ELECTRICITY",
                                "Cooling",
                                ElectricEIRChiller(EIRChillerNum).EndUseSubcategory,
                                "Plant");

            SetupOutputVariable("Chiller Evaporator Cooling Rate",
                                OutputProcessor::Unit::W,
                                ElectricEIRChiller(EIRChillerNum).QEvaporator,
                                "System",
                                "Average",
                                ElectricEIRChiller(EIRChillerNum).Name);
            SetupOutputVariable("Chiller Evaporator Cooling Energy",
                                OutputProcessor::Unit::J,
                                ElectricEIRChiller(EIRChillerNum).EvapEnergy,
                                "System",
                                "Sum",
                                ElectricEIRChiller(EIRChillerNum).Name,
                                _,
                                "ENERGYTRANSFER",
                                "CHILLERS",
                                _,
                                "Plant");
            SetupOutputVariable("Chiller False Load Heat Transfer Rate",
                                OutputProcessor::Unit::W,
                                ElectricEIRChiller(EIRChillerNum).ChillerFalseLoadRate,
                                "System",
                                "Average",
                                ElectricEIRChiller(EIRChillerNum).Name);
            SetupOutputVariable("Chiller False Load Heat Transfer Energy",
                                OutputProcessor::Unit::J,
                                ElectricEIRChiller(EIRChillerNum).ChillerFalseLoad,
                                "System",
                                "Sum",
                                ElectricEIRChiller(EIRChillerNum).Name);
            SetupOutputVariable("Chiller Evaporator Inlet Temperature",
                                OutputProcessor::Unit::C,
                                ElectricEIRChiller(EIRChillerNum).EvapInletTemp,
                                "System",
                                "Average",
                                ElectricEIRChiller(EIRChillerNum).Name);
            SetupOutputVariable("Chiller Evaporator Outlet Temperature",
                                OutputProcessor::Unit::C,
                                ElectricEIRChiller(EIRChillerNum).EvapOutletTemp,
                                "System",
                                "Average",
                                ElectricEIRChiller(EIRChillerNum).Name);
            SetupOutputVariable("Chiller Evaporator Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                ElectricEIRChiller(EIRChillerNum).EvapMassFlowRate,
                                "System",
                                "Average",
                                ElectricEIRChiller(EIRChillerNum).Name);

            SetupOutputVariable("Chiller Condenser Heat Transfer Rate",
                                OutputProcessor::Unit::W,
                                ElectricEIRChiller(EIRChillerNum).QCondenser,
                                "System",
                                "Average",
                                ElectricEIRChiller(EIRChillerNum).Name);
            SetupOutputVariable("Chiller Condenser Heat Transfer Energy",
                                OutputProcessor::Unit::J,
                                ElectricEIRChiller(EIRChillerNum).CondEnergy,
                                "System",
                                "Sum",
                                ElectricEIRChiller(EIRChillerNum).Name,
                                _,
                                "ENERGYTRANSFER",
                                "HEATREJECTION",
                                _,
                                "Plant");
            SetupOutputVariable("Chiller COP",
                                OutputProcessor::Unit::W_W,
                                ElectricEIRChiller(EIRChillerNum).ActualCOP,
                                "System",
                                "Average",
                                ElectricEIRChiller(EIRChillerNum).Name);

            SetupOutputVariable("Chiller Capacity Temperature Modifier Multiplier",
                                OutputProcessor::Unit::None,
                                ElectricEIRChiller(EIRChillerNum).ChillerCapFT,
                                "System",
                                "Average",
                                ElectricEIRChiller(EIRChillerNum).Name);
            SetupOutputVariable("Chiller EIR Temperature Modifier Multiplier",
                                OutputProcessor::Unit::None,
                                ElectricEIRChiller(EIRChillerNum).ChillerEIRFT,
                                "System",
                                "Average",
                                ElectricEIRChiller(EIRChillerNum).Name);
            SetupOutputVariable("Chiller EIR Part Load Modifier Multiplier",
                                OutputProcessor::Unit::None,
                                ElectricEIRChiller(EIRChillerNum).ChillerEIRFPLR,
                                "System",
                                "Average",
                                ElectricEIRChiller(EIRChillerNum).Name);

            // Condenser mass flow and outlet temp are valid for water cooled
            if (ElectricEIRChiller(EIRChillerNum).CondenserType == WaterCooled) {
                SetupOutputVariable("Chiller Condenser Inlet Temperature",
                                    OutputProcessor::Unit::C,
                                    ElectricEIRChiller(EIRChillerNum).CondInletTemp,
                                    "System",
                                    "Average",
                                    ElectricEIRChiller(EIRChillerNum).Name);
                SetupOutputVariable("Chiller Condenser Outlet Temperature",
                                    OutputProcessor::Unit::C,
                                    ElectricEIRChiller(EIRChillerNum).CondOutletTemp,
                                    "System",
                                    "Average",
                                    ElectricEIRChiller(EIRChillerNum).Name);
                SetupOutputVariable("Chiller Condenser Mass Flow Rate",
                                    OutputProcessor::Unit::kg_s,
                                    ElectricEIRChiller(EIRChillerNum).CondMassFlowRate,
                                    "System",
                                    "Average",
                                    ElectricEIRChiller(EIRChillerNum).Name);

                // If heat recovery is active then setup report variables
                if (ElectricEIRChiller(EIRChillerNum).HeatRecActive) {
                    SetupOutputVariable("Chiller Total Recovered Heat Rate",
                                        OutputProcessor::Unit::W,
                                        ElectricEIRChiller(EIRChillerNum).QHeatRecovered,
                                        "System",
                                        "Average",
                                        ElectricEIRChiller(EIRChillerNum).Name);
                    SetupOutputVariable("Chiller Total Recovered Heat Energy",
                                        OutputProcessor::Unit::J,
                                        ElectricEIRChiller(EIRChillerNum).EnergyHeatRecovery,
                                        "System",
                                        "Sum",
                                        ElectricEIRChiller(EIRChillerNum).Name,
                                        _,
                                        "ENERGYTRANSFER",
                                        "HEATRECOVERY",
                                        _,
                                        "Plant");
                    SetupOutputVariable("Chiller Heat Recovery Inlet Temperature",
                                        OutputProcessor::Unit::C,
                                        ElectricEIRChiller(EIRChillerNum).HeatRecInletTemp,
                                        "System",
                                        "Average",
                                        ElectricEIRChiller(EIRChillerNum).Name);
                    SetupOutputVariable("Chiller Heat Recovery Outlet Temperature",
                                        OutputProcessor::Unit::C,
                                        ElectricEIRChiller(EIRChillerNum).HeatRecOutletTemp,
                                        "System",
                                        "Average",
                                        ElectricEIRChiller(EIRChillerNum).Name);
                    SetupOutputVariable("Chiller Heat Recovery Mass Flow Rate",
                                        OutputProcessor::Unit::kg_s,
                                        ElectricEIRChiller(EIRChillerNum).HeatRecMassFlow,
                                        "System",
                                        "Average",
                                        ElectricEIRChiller(EIRChillerNum).Name);
                    SetupOutputVariable("Chiller Effective Heat Rejection Temperature",
                                        OutputProcessor::Unit::C,
                                        ElectricEIRChiller(EIRChillerNum).ChillerCondAvgTemp,
                                        "System",
                                        "Average",
                                        ElectricEIRChiller(EIRChillerNum).Name);
                }

            } else {
                SetupOutputVariable("Chiller Condenser Inlet Temperature",
                                    OutputProcessor::Unit::C,
                                    ElectricEIRChiller(EIRChillerNum).CondInletTemp,
                                    "System",
                                    "Average",
                                    ElectricEIRChiller(EIRChillerNum).Name);
                if (ElectricEIRChiller(EIRChillerNum).CondenserFanPowerRatio > 0) {
                    SetupOutputVariable("Chiller Condenser Fan Electric Power",
                                        OutputProcessor::Unit::W,
                                        ElectricEIRChiller(EIRChillerNum).CondenserFanPower,
                                        "System",
                                        "Average",
                                        ElectricEIRChiller(EIRChillerNum).Name);
                    SetupOutputVariable("Chiller Condenser Fan Electric Energy",
                                        OutputProcessor::Unit::J,
                                        ElectricEIRChiller(EIRChillerNum).CondenserFanEnergyConsumption,
                                        "System",
                                        "Sum",
                                        ElectricEIRChiller(EIRChillerNum).Name,
                                        _,
                                        "ELECTRICITY",
                                        "Cooling",
                                        _,
                                        "Plant");
                }
                if (ElectricEIRChiller(EIRChillerNum).CondenserType == EvapCooled) {
                    SetupOutputVariable("Chiller Evaporative Condenser Water Volume",
                                        OutputProcessor::Unit::m3,
                                        ElectricEIRChiller(EIRChillerNum).EvapWaterConsump,
                                        "System",
                                        "Sum",
                                        ElectricEIRChiller(EIRChillerNum).Name,
                                        _,
                                        "Water",
                                        "Cooling",
                                        _,
                                        "System");
                    SetupOutputVariable("Chiller Evaporative Condenser Mains Supply Water Volume",
                                        OutputProcessor::Unit::m3,
                                        ElectricEIRChiller(EIRChillerNum).EvapWaterConsump,
                                        "System",
                                        "Sum",
                                        ElectricEIRChiller(EIRChillerNum).Name,
                                        _,
                                        "MainsWater",
                                        "Cooling",
                                        _,
                                        "System");
                    if (ElectricEIRChiller(EIRChillerNum).BasinHeaterPowerFTempDiff > 0.0) {
                        SetupOutputVariable("Chiller Basin Heater Electric Power",
                                            OutputProcessor::Unit::W,
                                            ElectricEIRChiller(EIRChillerNum).BasinHeaterPower,
                                            "System",
                                            "Average",
                                            ElectricEIRChiller(EIRChillerNum).Name);
                        SetupOutputVariable("Chiller Basin Heater Electric Energy",
                                            OutputProcessor::Unit::J,
                                            ElectricEIRChiller(EIRChillerNum).BasinHeaterConsumption,
                                            "System",
                                            "Sum",
                                            ElectricEIRChiller(EIRChillerNum).Name,
                                            _,
                                            "Electric",
                                            "CHILLERS",
                                            _,
                                            "Plant");
                    }
                }
            }
            if (DataGlobals::AnyEnergyManagementSystemInModel) {
                SetupEMSInternalVariable(
                    "Chiller Nominal Capacity", ElectricEIRChiller(EIRChillerNum).Name, "[W]", ElectricEIRChiller(EIRChillerNum).RefCap);
            }
        }
    }

    void InitElectricEIRChiller(int const EIRChillNum, // Number of the current electric EIR chiller being simulated
                                bool const RunFlag,    // TRUE when chiller operating
                                Real64 const MyLoad    // current load put on chiller
    )
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
        if (ElectricEIRChiller(EIRChillNum).MyFlag) {
            // Locate the chillers on the plant loops for later usage
            bool errFlag = false;
            PlantUtilities::ScanPlantLoopsForObject(ElectricEIRChiller(EIRChillNum).Name,
                                    DataPlant::TypeOf_Chiller_ElectricEIR,
                                    ElectricEIRChiller(EIRChillNum).CWLoopNum,
                                    ElectricEIRChiller(EIRChillNum).CWLoopSideNum,
                                    ElectricEIRChiller(EIRChillNum).CWBranchNum,
                                    ElectricEIRChiller(EIRChillNum).CWCompNum,
                                    errFlag,
                                    ElectricEIRChiller(EIRChillNum).TempLowLimitEvapOut,
                                    _,
                                    _,
                                    ElectricEIRChiller(EIRChillNum).EvapInletNodeNum,
                                    _);
            if (ElectricEIRChiller(EIRChillNum).CondenserType != AirCooled && ElectricEIRChiller(EIRChillNum).CondenserType != EvapCooled) {
                PlantUtilities::ScanPlantLoopsForObject(ElectricEIRChiller(EIRChillNum).Name,
                                        DataPlant::TypeOf_Chiller_ElectricEIR,
                                        ElectricEIRChiller(EIRChillNum).CDLoopNum,
                                        ElectricEIRChiller(EIRChillNum).CDLoopSideNum,
                                        ElectricEIRChiller(EIRChillNum).CDBranchNum,
                                        ElectricEIRChiller(EIRChillNum).CDCompNum,
                                        errFlag,
                                        _,
                                        _,
                                        _,
                                        ElectricEIRChiller(EIRChillNum).CondInletNodeNum,
                                        _);
                PlantUtilities::InterConnectTwoPlantLoopSides(ElectricEIRChiller(EIRChillNum).CWLoopNum,
                                              ElectricEIRChiller(EIRChillNum).CWLoopSideNum,
                                              ElectricEIRChiller(EIRChillNum).CDLoopNum,
                                              ElectricEIRChiller(EIRChillNum).CDLoopSideNum,
                                              DataPlant::TypeOf_Chiller_ElectricEIR,
                                              true);
            }
            if (ElectricEIRChiller(EIRChillNum).HeatRecActive) {
                PlantUtilities::ScanPlantLoopsForObject(ElectricEIRChiller(EIRChillNum).Name,
                                        DataPlant::TypeOf_Chiller_ElectricEIR,
                                        ElectricEIRChiller(EIRChillNum).HRLoopNum,
                                        ElectricEIRChiller(EIRChillNum).HRLoopSideNum,
                                        ElectricEIRChiller(EIRChillNum).HRBranchNum,
                                        ElectricEIRChiller(EIRChillNum).HRCompNum,
                                        errFlag,
                                        _,
                                        _,
                                        _,
                                        ElectricEIRChiller(EIRChillNum).HeatRecInletNodeNum,
                                        _);
                PlantUtilities::InterConnectTwoPlantLoopSides(ElectricEIRChiller(EIRChillNum).CWLoopNum,
                                              ElectricEIRChiller(EIRChillNum).CWLoopSideNum,
                                              ElectricEIRChiller(EIRChillNum).HRLoopNum,
                                              ElectricEIRChiller(EIRChillNum).HRLoopSideNum,
                                              DataPlant::TypeOf_Chiller_ElectricEIR,
                                              true);
            }

            if (ElectricEIRChiller(EIRChillNum).CondenserType != AirCooled && ElectricEIRChiller(EIRChillNum).CondenserType != EvapCooled &&
                ElectricEIRChiller(EIRChillNum).HeatRecActive) {
                PlantUtilities::InterConnectTwoPlantLoopSides(ElectricEIRChiller(EIRChillNum).CDLoopNum,
                                              ElectricEIRChiller(EIRChillNum).CDLoopSideNum,
                                              ElectricEIRChiller(EIRChillNum).HRLoopNum,
                                              ElectricEIRChiller(EIRChillNum).HRLoopSideNum,
                                              DataPlant::TypeOf_Chiller_ElectricEIR,
                                              false);
            }

            if (errFlag) {
                ShowFatalError("InitElectricEIRChiller: Program terminated due to previous condition(s).");
            }

            if (ElectricEIRChiller(EIRChillNum).FlowMode == ConstantFlow) {
                // reset flow priority
                DataPlant::PlantLoop(ElectricEIRChiller(EIRChillNum).CWLoopNum)
                    .LoopSide(ElectricEIRChiller(EIRChillNum).CWLoopSideNum)
                    .Branch(ElectricEIRChiller(EIRChillNum).CWBranchNum)
                    .Comp(ElectricEIRChiller(EIRChillNum).CWCompNum)
                    .FlowPriority = DataPlant::LoopFlowStatus_NeedyIfLoopOn;
            }

            if (ElectricEIRChiller(EIRChillNum).FlowMode == LeavingSetPointModulated) {
                // reset flow priority
                DataPlant::PlantLoop(ElectricEIRChiller(EIRChillNum).CWLoopNum)
                    .LoopSide(ElectricEIRChiller(EIRChillNum).CWLoopSideNum)
                    .Branch(ElectricEIRChiller(EIRChillNum).CWBranchNum)
                    .Comp(ElectricEIRChiller(EIRChillNum).CWCompNum)
                    .FlowPriority = DataPlant::LoopFlowStatus_NeedyIfLoopOn;
                // check if setpoint on outlet node
                if ((DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).EvapOutletNodeNum).TempSetPoint == DataLoopNode::SensedNodeFlagValue) &&
                    (DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).EvapOutletNodeNum).TempSetPointHi == DataLoopNode::SensedNodeFlagValue)) {
                    if (!DataGlobals::AnyEnergyManagementSystemInModel) {
                        if (!ElectricEIRChiller(EIRChillNum).ModulatedFlowErrDone) {
                            ShowWarningError("Missing temperature setpoint for LeavingSetpointModulated mode chiller named " +
                                             ElectricEIRChiller(EIRChillNum).Name);
                            ShowContinueError(
                                "  A temperature setpoint is needed at the outlet node of a chiller in variable flow mode, use a SetpointManager");
                            ShowContinueError("  The overall loop setpoint will be assumed for chiller. The simulation continues ... ");
                            ElectricEIRChiller(EIRChillNum).ModulatedFlowErrDone = true;
                        }
                    } else {
                        // need call to EMS to check node
                        bool fatalError = false; // but not really fatal yet, but should be.
                        EMSManager::CheckIfNodeSetPointManagedByEMS(ElectricEIRChiller(EIRChillNum).EvapOutletNodeNum, EMSManager::iTemperatureSetPoint, fatalError);
                        if (fatalError) {
                            if (!ElectricEIRChiller(EIRChillNum).ModulatedFlowErrDone) {
                                ShowWarningError("Missing temperature setpoint for LeavingSetpointModulated mode chiller named " +
                                                 ElectricEIRChiller(EIRChillNum).Name);
                                ShowContinueError(
                                    "  A temperature setpoint is needed at the outlet node of a chiller evaporator in variable flow mode");
                                ShowContinueError("  use a Setpoint Manager to establish a setpoint at the chiller evaporator outlet node ");
                                ShowContinueError("  or use an EMS actuator to establish a setpoint at the outlet node ");
                                ShowContinueError("  The overall loop setpoint will be assumed for chiller. The simulation continues ... ");
                                ElectricEIRChiller(EIRChillNum).ModulatedFlowErrDone = true;
                            }
                        }
                    }
                    ElectricEIRChiller(EIRChillNum).ModulatedFlowSetToLoop = true;
                    DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).EvapOutletNodeNum).TempSetPoint =
                        DataLoopNode::Node(DataPlant::PlantLoop(ElectricEIRChiller(EIRChillNum).CWLoopNum).TempSetPointNodeNum).TempSetPoint;
                    DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).EvapOutletNodeNum).TempSetPointHi =
                        DataLoopNode::Node(DataPlant::PlantLoop(ElectricEIRChiller(EIRChillNum).CWLoopNum).TempSetPointNodeNum).TempSetPointHi;
                }
            }
            ElectricEIRChiller(EIRChillNum).MyFlag = false;
        }

        if (ElectricEIRChiller(EIRChillNum).MyEnvrnFlag && DataGlobals::BeginEnvrnFlag && (DataPlant::PlantFirstSizesOkayToFinalize)) {

            Real64 rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(ElectricEIRChiller(EIRChillNum).CWLoopNum).FluidName,
                                   DataGlobals::CWInitConvTemp,
                                   DataPlant::PlantLoop(ElectricEIRChiller(EIRChillNum).CWLoopNum).FluidIndex,
                                   RoutineName);

            ElectricEIRChiller(EIRChillNum).EvapMassFlowRateMax = ElectricEIRChiller(EIRChillNum).EvapVolFlowRate * rho;

            PlantUtilities::InitComponentNodes(0.0,
                               ElectricEIRChiller(EIRChillNum).EvapMassFlowRateMax,
                               ElectricEIRChiller(EIRChillNum).EvapInletNodeNum,
                               ElectricEIRChiller(EIRChillNum).EvapOutletNodeNum,
                               ElectricEIRChiller(EIRChillNum).CWLoopNum,
                               ElectricEIRChiller(EIRChillNum).CWLoopSideNum,
                               ElectricEIRChiller(EIRChillNum).CWBranchNum,
                               ElectricEIRChiller(EIRChillNum).CWCompNum);

            if (ElectricEIRChiller(EIRChillNum).CondenserType == WaterCooled) {

                rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(ElectricEIRChiller(EIRChillNum).CDLoopNum).FluidName,
                                       ElectricEIRChiller(EIRChillNum).TempRefCondIn,
                                       DataPlant::PlantLoop(ElectricEIRChiller(EIRChillNum).CDLoopNum).FluidIndex,
                                       RoutineName);
                ElectricEIRChiller(EIRChillNum).CondMassFlowRateMax = rho * ElectricEIRChiller(EIRChillNum).CondVolFlowRate;
                PlantUtilities::InitComponentNodes(0.0,
                                   ElectricEIRChiller(EIRChillNum).CondMassFlowRateMax,
                                   ElectricEIRChiller(EIRChillNum).CondInletNodeNum,
                                   ElectricEIRChiller(EIRChillNum).CondOutletNodeNum,
                                   ElectricEIRChiller(EIRChillNum).CDLoopNum,
                                   ElectricEIRChiller(EIRChillNum).CDLoopSideNum,
                                   ElectricEIRChiller(EIRChillNum).CDBranchNum,
                                   ElectricEIRChiller(EIRChillNum).CDCompNum);
                DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).CondInletNodeNum).Temp = ElectricEIRChiller(EIRChillNum).TempRefCondIn;
            } else { // air or evap air condenser
                // Initialize maximum available condenser flow rate
                rho =
                    Psychrometrics::PsyRhoAirFnPbTdbW(DataEnvironment::StdBaroPress, ElectricEIRChiller(EIRChillNum).TempRefCondIn, 0.0, RoutineName);
                ElectricEIRChiller(EIRChillNum).CondMassFlowRateMax = rho * ElectricEIRChiller(EIRChillNum).CondVolFlowRate;

                DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).CondInletNodeNum).MassFlowRate = ElectricEIRChiller(EIRChillNum).CondMassFlowRateMax;
                DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).CondOutletNodeNum).MassFlowRate = DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).CondInletNodeNum).MassFlowRate;
                DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).CondInletNodeNum).MassFlowRateMaxAvail = DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).CondInletNodeNum).MassFlowRate;
                DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).CondInletNodeNum).MassFlowRateMax = DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).CondInletNodeNum).MassFlowRate;
                DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).CondOutletNodeNum).MassFlowRateMax = DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).CondInletNodeNum).MassFlowRate;
                DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).CondInletNodeNum).MassFlowRateMinAvail = 0.0;
                DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).CondInletNodeNum).MassFlowRateMin = 0.0;
                DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).CondOutletNodeNum).MassFlowRateMinAvail = 0.0;
                DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).CondOutletNodeNum).MassFlowRateMin = 0.0;
                DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).CondInletNodeNum).Temp = ElectricEIRChiller(EIRChillNum).TempRefCondIn;
            }

            if (ElectricEIRChiller(EIRChillNum).HeatRecActive) {
                rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(ElectricEIRChiller(EIRChillNum).HRLoopNum).FluidName,
                                       DataGlobals::CWInitConvTemp,
                                       DataPlant::PlantLoop(ElectricEIRChiller(EIRChillNum).HRLoopNum).FluidIndex,
                                       RoutineName);
                ElectricEIRChiller(EIRChillNum).DesignHeatRecMassFlowRate = rho * ElectricEIRChiller(EIRChillNum).DesignHeatRecVolFlowRate;

                PlantUtilities::InitComponentNodes(0.0,
                                   ElectricEIRChiller(EIRChillNum).DesignHeatRecMassFlowRate,
                                   ElectricEIRChiller(EIRChillNum).HeatRecInletNodeNum,
                                   ElectricEIRChiller(EIRChillNum).HeatRecOutletNodeNum,
                                   ElectricEIRChiller(EIRChillNum).HRLoopNum,
                                   ElectricEIRChiller(EIRChillNum).HRLoopSideNum,
                                   ElectricEIRChiller(EIRChillNum).HRBranchNum,
                                   ElectricEIRChiller(EIRChillNum).HRCompNum);
                // overall capacity limit
                ElectricEIRChiller(EIRChillNum).HeatRecMaxCapacityLimit =
                    ElectricEIRChiller(EIRChillNum).HeatRecCapacityFraction *
                    (ElectricEIRChiller(EIRChillNum).RefCap + ElectricEIRChiller(EIRChillNum).RefCap / ElectricEIRChiller(EIRChillNum).RefCOP);

                if (ElectricEIRChiller(EIRChillNum).HeatRecSetPointNodeNum > 0) {
                    Real64 THeatRecSetPoint(0.0);    // tests set point node for proper set point value
                    {
                        auto const SELECT_CASE_var(DataPlant::PlantLoop(ElectricEIRChiller(EIRChillNum).HRLoopNum).LoopDemandCalcScheme);
                        if (SELECT_CASE_var == DataPlant::SingleSetPoint) {
                            THeatRecSetPoint = DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).HeatRecSetPointNodeNum).TempSetPoint;
                        } else if (SELECT_CASE_var == DataPlant::DualSetPointDeadBand) {
                            THeatRecSetPoint = DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).HeatRecSetPointNodeNum).TempSetPointHi;
                        } else {
                            assert(false);
                        }
                    }
                    if (THeatRecSetPoint == DataLoopNode::SensedNodeFlagValue) {
                        if (!DataGlobals::AnyEnergyManagementSystemInModel) {
                            if (!ElectricEIRChiller(EIRChillNum).HRSPErrDone) {
                                ShowWarningError("Missing heat recovery temperature setpoint for chiller named " +
                                                 ElectricEIRChiller(EIRChillNum).Name);
                                ShowContinueError("  A temperature setpoint is needed at the heat recovery leaving temperature setpoint node "
                                                  "specified, use a SetpointManager");
                                ShowContinueError("  The overall loop setpoint will be assumed for heat recovery. The simulation continues ...");
                                ElectricEIRChiller(EIRChillNum).HeatRecSetPointNodeNum =
                                    DataPlant::PlantLoop(ElectricEIRChiller(EIRChillNum).HRLoopNum).TempSetPointNodeNum;
                                ElectricEIRChiller(EIRChillNum).HRSPErrDone = true;
                            }
                        } else {
                            // need call to EMS to check node
                            bool fatalError = false; // but not really fatal yet, but should be.
                            EMSManager::CheckIfNodeSetPointManagedByEMS(ElectricEIRChiller(EIRChillNum).EvapOutletNodeNum, EMSManager::iTemperatureSetPoint, fatalError);
                            if (fatalError) {
                                if (!ElectricEIRChiller(EIRChillNum).HRSPErrDone) {
                                    ShowWarningError("Missing heat recovery temperature setpoint for chiller named " +
                                                     ElectricEIRChiller(EIRChillNum).Name);
                                    ShowContinueError("  A temperature setpoint is needed at the heat recovery leaving temperature setpoint node "
                                                      "specified, use a SetpointManager to establish a setpoint");
                                    ShowContinueError("  or use an EMS actuator to establish a setpoint at this node ");
                                    ShowContinueError("  The overall loop setpoint will be assumed for heat recovery. The simulation continues ...");
                                    ElectricEIRChiller(EIRChillNum).HeatRecSetPointNodeNum =
                                        DataPlant::PlantLoop(ElectricEIRChiller(EIRChillNum).HRLoopNum).TempSetPointNodeNum;
                                    ElectricEIRChiller(EIRChillNum).HRSPErrDone = true;
                                }
                            }
                        } // IF (.NOT. AnyEnergyManagementSystemInModel) THEN
                    }     // IF(THeatRecSetPoint == SensedNodeFlagValue)THEN
                }         // IF(ElectricEIRChiller(EIRChillNum)%HeatRecSetPointNodeNum > 0)THEN
            }             // IF (ElectricEIRChiller(EIRChillNum)%HeatRecActive) THEN

            ElectricEIRChiller(EIRChillNum).MyEnvrnFlag = false;
        }
        if (!DataGlobals::BeginEnvrnFlag) {
            ElectricEIRChiller(EIRChillNum).MyEnvrnFlag = true;
        }

        if ((ElectricEIRChiller(EIRChillNum).FlowMode == LeavingSetPointModulated) && ElectricEIRChiller(EIRChillNum).ModulatedFlowSetToLoop) {
            // fix for clumsy old input that worked because loop setpoint was spread.
            //  could be removed with transition, testing , model change, period of being obsolete.
            DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).EvapOutletNodeNum).TempSetPoint =
                DataLoopNode::Node(DataPlant::PlantLoop(ElectricEIRChiller(EIRChillNum).CWLoopNum).TempSetPointNodeNum).TempSetPoint;
            DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).EvapOutletNodeNum).TempSetPointHi =
                DataLoopNode::Node(DataPlant::PlantLoop(ElectricEIRChiller(EIRChillNum).CWLoopNum).TempSetPointNodeNum).TempSetPointHi;
        }

        Real64 mdot;
        Real64 mdotCond;
        if ((std::abs(MyLoad) > 0.0) && RunFlag) {
            mdot = ElectricEIRChiller(EIRChillNum).EvapMassFlowRateMax;
            mdotCond = ElectricEIRChiller(EIRChillNum).CondMassFlowRateMax;
        } else {
            mdot = 0.0;
            mdotCond = 0.0;
        }

        PlantUtilities::SetComponentFlowRate(mdot,
                             ElectricEIRChiller(EIRChillNum).EvapInletNodeNum,
                             ElectricEIRChiller(EIRChillNum).EvapOutletNodeNum,
                             ElectricEIRChiller(EIRChillNum).CWLoopNum,
                             ElectricEIRChiller(EIRChillNum).CWLoopSideNum,
                             ElectricEIRChiller(EIRChillNum).CWBranchNum,
                             ElectricEIRChiller(EIRChillNum).CWCompNum);

        if (ElectricEIRChiller(EIRChillNum).CondenserType == WaterCooled) {
            PlantUtilities::SetComponentFlowRate(mdotCond,
                                 ElectricEIRChiller(EIRChillNum).CondInletNodeNum,
                                 ElectricEIRChiller(EIRChillNum).CondOutletNodeNum,
                                 ElectricEIRChiller(EIRChillNum).CDLoopNum,
                                 ElectricEIRChiller(EIRChillNum).CDLoopSideNum,
                                 ElectricEIRChiller(EIRChillNum).CDBranchNum,
                                 ElectricEIRChiller(EIRChillNum).CDCompNum);
        }
        // Initialize heat recovery flow rates at node
        if (ElectricEIRChiller(EIRChillNum).HeatRecActive) {
            int LoopNum = ElectricEIRChiller(EIRChillNum).HRLoopNum;
            int LoopSideNum = ElectricEIRChiller(EIRChillNum).HRLoopSideNum;
            int BranchIndex = ElectricEIRChiller(EIRChillNum).HRBranchNum;
            int CompIndex = ElectricEIRChiller(EIRChillNum).HRCompNum;
            if (RunFlag) {
                mdot = ElectricEIRChiller(EIRChillNum).DesignHeatRecMassFlowRate;
            } else {
                mdot = 0.0;
            }

            PlantUtilities::SetComponentFlowRate(mdot, ElectricEIRChiller(EIRChillNum).HeatRecInletNodeNum, ElectricEIRChiller(EIRChillNum).HeatRecOutletNodeNum, LoopNum, LoopSideNum, BranchIndex, CompIndex);
        }

        if (ElectricEIRChiller(EIRChillNum).CondenserType == EvapCooled) {
            ElectricEIRChiller(EIRChillNum).BasinHeaterPower = 0.0;
        }
    }

    void SizeElectricEIRChiller(int const EIRChillNum)
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
        Real64 tmpNomCap = ElectricEIRChiller(EIRChillNum).RefCap;
        Real64 tmpEvapVolFlowRate = ElectricEIRChiller(EIRChillNum).EvapVolFlowRate;
        Real64 tmpCondVolFlowRate = ElectricEIRChiller(EIRChillNum).CondVolFlowRate;

        if (ElectricEIRChiller(EIRChillNum).CondenserType == WaterCooled) {
            PltSizCondNum = DataPlant::PlantLoop(ElectricEIRChiller(EIRChillNum).CDLoopNum).PlantSizNum;
        }

        // find the appropriate Plant Sizing object
        int PltSizNum = DataPlant::PlantLoop(ElectricEIRChiller(EIRChillNum).CWLoopNum).PlantSizNum;

        if (PltSizNum > 0) {
            if (DataSizing::PlantSizData(PltSizNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                tmpEvapVolFlowRate = DataSizing::PlantSizData(PltSizNum).DesVolFlowRate * ElectricEIRChiller(EIRChillNum).SizFac;
            } else {
                if (ElectricEIRChiller(EIRChillNum).EvapVolFlowRateWasAutoSized) tmpEvapVolFlowRate = 0.0;
            }
            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                if (ElectricEIRChiller(EIRChillNum).EvapVolFlowRateWasAutoSized) {
                    ElectricEIRChiller(EIRChillNum).EvapVolFlowRate = tmpEvapVolFlowRate;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput("Chiller:Electric:EIR",
                                           ElectricEIRChiller(EIRChillNum).Name,
                                           "Design Size Reference Chilled Water Flow Rate [m3/s]",
                                           tmpEvapVolFlowRate);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput("Chiller:Electric:EIR",
                                           ElectricEIRChiller(EIRChillNum).Name,
                                           "Initial Design Size Reference Chilled Water Flow Rate [m3/s]",
                                           tmpEvapVolFlowRate);
                    }
                } else { // Hard-size with sizing data
                    if (ElectricEIRChiller(EIRChillNum).EvapVolFlowRate > 0.0 && tmpEvapVolFlowRate > 0.0) {
                        Real64 EvapVolFlowRateUser = ElectricEIRChiller(EIRChillNum).EvapVolFlowRate;
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput("Chiller:Electric:EIR",
                                               ElectricEIRChiller(EIRChillNum).Name,
                                               "Design Size Reference Chilled Water Flow Rate [m3/s]",
                                               tmpEvapVolFlowRate,
                                               "User-Specified Reference Chilled Water Flow Rate [m3/s]",
                                               EvapVolFlowRateUser);
                            if (DataGlobals::DisplayExtraWarnings) {
                                if ((std::abs(tmpEvapVolFlowRate - EvapVolFlowRateUser) / EvapVolFlowRateUser) > DataSizing::AutoVsHardSizingThreshold) {
                                    ShowMessage("SizeChillerElectricEIR: Potential issue with equipment sizing for " +
                                                ElectricEIRChiller(EIRChillNum).Name);
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
            if (ElectricEIRChiller(EIRChillNum).EvapVolFlowRateWasAutoSized && DataPlant::PlantFirstSizesOkayToFinalize) {
                ShowSevereError("Autosizing of Electric Chiller evap flow rate requires a loop Sizing:Plant object");
                ShowContinueError("Occurs in Electric Chiller object=" + ElectricEIRChiller(EIRChillNum).Name);
                ErrorsFound = true;
            }
            if (!ElectricEIRChiller(EIRChillNum).EvapVolFlowRateWasAutoSized && DataPlant::PlantFinalSizesOkayToReport &&
                (ElectricEIRChiller(EIRChillNum).EvapVolFlowRate > 0.0)) {
                ReportSizingManager::ReportSizingOutput("Chiller:Electric:EIR",
                                   ElectricEIRChiller(EIRChillNum).Name,
                                   "User-Specified Reference Chilled Water Flow Rate [m3/s]",
                                   ElectricEIRChiller(EIRChillNum).EvapVolFlowRate);
            }
        }

        PlantUtilities::RegisterPlantCompDesignFlow(ElectricEIRChiller(EIRChillNum).EvapInletNodeNum, tmpEvapVolFlowRate);

        if (PltSizNum > 0) {
            if (DataSizing::PlantSizData(PltSizNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                Real64 Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(ElectricEIRChiller(EIRChillNum).CWLoopNum).FluidName,
                                           DataGlobals::CWInitConvTemp,
                                           DataPlant::PlantLoop(ElectricEIRChiller(EIRChillNum).CWLoopNum).FluidIndex,
                                           RoutineName);

                Real64 rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(ElectricEIRChiller(EIRChillNum).CWLoopNum).FluidName,
                                       DataGlobals::CWInitConvTemp,
                                       DataPlant::PlantLoop(ElectricEIRChiller(EIRChillNum).CWLoopNum).FluidIndex,
                                       RoutineName);
                tmpNomCap = Cp * rho * DataSizing::PlantSizData(PltSizNum).DeltaT * tmpEvapVolFlowRate;
            } else {
                tmpNomCap = 0.0;
            }
            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                if (ElectricEIRChiller(EIRChillNum).RefCapWasAutoSized) {
                    ElectricEIRChiller(EIRChillNum).RefCap = tmpNomCap;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(
                            "Chiller:Electric:EIR", ElectricEIRChiller(EIRChillNum).Name, "Design Size Reference Capacity [W]", tmpNomCap);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(
                            "Chiller:Electric:EIR", ElectricEIRChiller(EIRChillNum).Name, "Initial Design Size Reference Capacity [W]", tmpNomCap);
                    }
                } else { // Hard-sized with sizing data
                    if (ElectricEIRChiller(EIRChillNum).RefCap > 0.0 && tmpNomCap > 0.0) {
                        Real64 RefCapUser = ElectricEIRChiller(EIRChillNum).RefCap;
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput("Chiller:Electric:EIR",
                                               ElectricEIRChiller(EIRChillNum).Name,
                                               "Design Size Reference Capacity [W]",
                                               tmpNomCap,
                                               "User-Specified Reference Capacity [W]",
                                               RefCapUser);
                            if (DataGlobals::DisplayExtraWarnings) {
                                if ((std::abs(tmpNomCap - RefCapUser) / RefCapUser) > DataSizing::AutoVsHardSizingThreshold) {
                                    ShowMessage("SizeChillerElectricEIR: Potential issue with equipment sizing for " +
                                                ElectricEIRChiller(EIRChillNum).Name);
                                    ShowContinueError("User-Specified Reference Capacity of " + General::RoundSigDigits(RefCapUser, 2) + " [W]");
                                    ShowContinueError("differs from Design Size Reference Capacity of " + General::RoundSigDigits(tmpNomCap, 2) + " [W]");
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
            if (ElectricEIRChiller(EIRChillNum).RefCapWasAutoSized && DataPlant::PlantFirstSizesOkayToFinalize) {
                ShowSevereError("Autosizing of Electric Chiller reference capacity requires a loop Sizing:Plant object");
                ShowContinueError("Occurs in Electric Chiller object=" + ElectricEIRChiller(EIRChillNum).Name);
                ErrorsFound = true;
            }
            if (!ElectricEIRChiller(EIRChillNum).RefCapWasAutoSized && DataPlant::PlantFinalSizesOkayToReport &&
                (ElectricEIRChiller(EIRChillNum).RefCap > 0.0)) { // Hard-sized with no sizing data
                ReportSizingManager::ReportSizingOutput("Chiller:Electric:EIR",
                                   ElectricEIRChiller(EIRChillNum).Name,
                                   "User-Specified Reference Capacity [W]",
                                   ElectricEIRChiller(EIRChillNum).RefCap);
            }
        }

        if (PltSizCondNum > 0 && PltSizNum > 0) {
            if (DataSizing::PlantSizData(PltSizNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow && tmpNomCap > 0.0) {

                Real64 rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(ElectricEIRChiller(EIRChillNum).CDLoopNum).FluidName,
                                       DataGlobals::CWInitConvTemp,
                                       DataPlant::PlantLoop(ElectricEIRChiller(EIRChillNum).CDLoopNum).FluidIndex,
                                       RoutineName);
                Real64 Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(ElectricEIRChiller(EIRChillNum).CDLoopNum).FluidName,
                                           ElectricEIRChiller(EIRChillNum).TempRefCondIn,
                                           DataPlant::PlantLoop(ElectricEIRChiller(EIRChillNum).CDLoopNum).FluidIndex,
                                           RoutineName);
                tmpCondVolFlowRate =
                    tmpNomCap * (1.0 + (1.0 / ElectricEIRChiller(EIRChillNum).RefCOP) * ElectricEIRChiller(EIRChillNum).CompPowerToCondenserFrac) /
                    (DataSizing::PlantSizData(PltSizCondNum).DeltaT * Cp * rho);

            } else {
                if (ElectricEIRChiller(EIRChillNum).CondVolFlowRateWasAutoSized) tmpCondVolFlowRate = 0.0;
            }
            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                if (ElectricEIRChiller(EIRChillNum).CondVolFlowRateWasAutoSized) {
                    ElectricEIRChiller(EIRChillNum).CondVolFlowRate = tmpCondVolFlowRate;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput("Chiller:Electric:EIR",
                                           ElectricEIRChiller(EIRChillNum).Name,
                                           "Design Size Reference Condenser Fluid Flow Rate [m3/s]",
                                           tmpCondVolFlowRate);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput("Chiller:Electric:EIR",
                                           ElectricEIRChiller(EIRChillNum).Name,
                                           "Initial Design Size Reference Condenser Fluid Flow Rate [m3/s]",
                                           tmpCondVolFlowRate);
                    }
                } else {
                    if (ElectricEIRChiller(EIRChillNum).CondVolFlowRate > 0.0 && tmpCondVolFlowRate > 0.0) {
                        Real64 CondVolFlowRateUser = ElectricEIRChiller(EIRChillNum).CondVolFlowRate;
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput("Chiller:Electric:EIR",
                                               ElectricEIRChiller(EIRChillNum).Name,
                                               "Design Size Reference Condenser Fluid Flow Rate [m3/s]",
                                               tmpCondVolFlowRate,
                                               "User-Specified Reference Condenser Fluid Flow Rate [m3/s]",
                                               CondVolFlowRateUser);
                            if (DataGlobals::DisplayExtraWarnings) {
                                if ((std::abs(tmpCondVolFlowRate - CondVolFlowRateUser) / CondVolFlowRateUser) > DataSizing::AutoVsHardSizingThreshold) {
                                    ShowMessage("SizeChillerElectricEIR: Potential issue with equipment sizing for " +
                                                ElectricEIRChiller(EIRChillNum).Name);
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
            if (ElectricEIRChiller(EIRChillNum).CondenserType == WaterCooled) {

                if (ElectricEIRChiller(EIRChillNum).CondVolFlowRateWasAutoSized && DataPlant::PlantFirstSizesOkayToFinalize) {
                    ShowSevereError("Autosizing of Electric EIR Chiller condenser fluid flow rate requires a condenser");
                    ShowContinueError("loop Sizing:Plant object");
                    ShowContinueError("Occurs in Electric EIR Chiller object=" + ElectricEIRChiller(EIRChillNum).Name);
                    ErrorsFound = true;
                }
                if (!ElectricEIRChiller(EIRChillNum).CondVolFlowRateWasAutoSized && DataPlant::PlantFinalSizesOkayToReport &&
                    (ElectricEIRChiller(EIRChillNum).CondVolFlowRate > 0.0)) {
                    ReportSizingManager::ReportSizingOutput("Chiller:Electric:EIR",
                                       ElectricEIRChiller(EIRChillNum).Name,
                                       "User-Specified Reference Condenser Fluid Flow Rate [m3/s]",
                                       ElectricEIRChiller(EIRChillNum).CondVolFlowRate);
                }

            } else {

                // Auto size condenser air flow to Total Capacity * 0.000114 m3/s/w (850 cfm/ton)
                if (DataPlant::PlantFinalSizesOkayToReport) {
                    int SizingMethod = DataHVACGlobals::AutoCalculateSizing;
                    std::string CompType = DataPlant::ccSimPlantEquipTypes(DataPlant::TypeOf_Chiller_ElectricEIR);
                    std::string SizingString = "Reference Condenser Fluid Flow Rate  [m3/s]";
                    DataSizing::DataConstantUsedForSizing = ElectricEIRChiller(EIRChillNum).RefCap;
                    DataSizing::DataFractionUsedForSizing = 0.000114;
                    Real64 TempSize = ElectricEIRChiller(EIRChillNum).CondVolFlowRate;
                    bool bPRINT = true;       // TRUE if sizing is reported to output (eio)
                    ReportSizingManager::RequestSizing(CompType, ElectricEIRChiller(EIRChillNum).Name, SizingMethod, SizingString, TempSize, bPRINT, RoutineName);
                    ElectricEIRChiller(EIRChillNum).CondVolFlowRate = TempSize;
                    DataSizing::DataConstantUsedForSizing = 0.0;
                    DataSizing::DataFractionUsedForSizing = 0.0;
                }
            }
        }

        // save the reference condenser water volumetric flow rate for use by the condenser water loop sizing algorithms
        PlantUtilities::RegisterPlantCompDesignFlow(ElectricEIRChiller(EIRChillNum).CondInletNodeNum, tmpCondVolFlowRate);

        // now do heat recovery flow rate sizing if active
        if (ElectricEIRChiller(EIRChillNum).HeatRecActive) {
            Real64 tempHeatRecVolFlowRate = tmpCondVolFlowRate * ElectricEIRChiller(EIRChillNum).HeatRecCapacityFraction;
            if (ElectricEIRChiller(EIRChillNum).DesignHeatRecVolFlowRateWasAutoSized) {

                if (DataPlant::PlantFirstSizesOkayToFinalize) {
                    ElectricEIRChiller(EIRChillNum).DesignHeatRecVolFlowRate = tempHeatRecVolFlowRate;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput("Chiller:Electric:EIR",
                                           ElectricEIRChiller(EIRChillNum).Name,
                                           "Design Size Heat Recovery Water Flow Rate [m3/s]",
                                           tempHeatRecVolFlowRate);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput("Chiller:Electric:EIR",
                                           ElectricEIRChiller(EIRChillNum).Name,
                                           "Intial Design Size Heat Recovery Water Flow Rate [m3/s]",
                                           tempHeatRecVolFlowRate);
                    }
                }
            } else {
                if (ElectricEIRChiller(EIRChillNum).DesignHeatRecVolFlowRate > 0.0 && tempHeatRecVolFlowRate > 0.0) {
                    Real64 nomHeatRecVolFlowRateUser = ElectricEIRChiller(EIRChillNum).DesignHeatRecVolFlowRate;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        if (DataGlobals::DoPlantSizing) {
                            ReportSizingManager::ReportSizingOutput("Chiller:Electric:EIR",
                                               ElectricEIRChiller(EIRChillNum).Name,
                                               "Design Size Heat Recovery Water Flow Rate [m3/s]",
                                               tempHeatRecVolFlowRate,
                                               "User-Specified Heat Recovery Water Flow Rate [m3/s]",
                                               nomHeatRecVolFlowRateUser);
                        } else {
                            ReportSizingManager::ReportSizingOutput("Chiller:Electric:EIR",
                                               ElectricEIRChiller(EIRChillNum).Name,
                                               "User-Specified Heat Recovery Water Flow Rate [m3/s]",
                                               nomHeatRecVolFlowRateUser);
                        }

                        if (DataGlobals::DisplayExtraWarnings) {
                            if ((std::abs(tempHeatRecVolFlowRate - nomHeatRecVolFlowRateUser) / nomHeatRecVolFlowRateUser) >
                                DataSizing::AutoVsHardSizingThreshold) {
                                ShowMessage("SizeChillerElectricEIR: Potential issue with equipment sizing for " +
                                            ElectricEIRChiller(EIRChillNum).Name);
                                ShowContinueError("User-Specified Heat Recovery Water Flow Rate of " + General::RoundSigDigits(nomHeatRecVolFlowRateUser, 5) +
                                                  " [m3/s]");
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
            if (!ElectricEIRChiller(EIRChillNum).DesignHeatRecVolFlowRateWasAutoSized)
                tempHeatRecVolFlowRate = ElectricEIRChiller(EIRChillNum).DesignHeatRecVolFlowRate;
            PlantUtilities::RegisterPlantCompDesignFlow(ElectricEIRChiller(EIRChillNum).HeatRecInletNodeNum, tempHeatRecVolFlowRate);
        } // Heat recovery active

        if (DataPlant::PlantFinalSizesOkayToReport) {
            if (ElectricEIRChiller(EIRChillNum).IPLVFlag) {
                Real64 IPLV;
                StandardRatings::CalcChillerIPLV(ElectricEIRChiller(EIRChillNum).Name,
                                DataPlant::TypeOf_Chiller_ElectricEIR,
                                ElectricEIRChiller(EIRChillNum).RefCap,
                                ElectricEIRChiller(EIRChillNum).RefCOP,
                                ElectricEIRChiller(EIRChillNum).CondenserType,
                                ElectricEIRChiller(EIRChillNum).ChillerCapFTIndex,
                                ElectricEIRChiller(EIRChillNum).ChillerEIRFTIndex,
                                ElectricEIRChiller(EIRChillNum).ChillerEIRFPLRIndex,
                                ElectricEIRChiller(EIRChillNum).MinUnloadRat,
                                IPLV);
                ElectricEIRChiller(EIRChillNum).IPLVFlag = false;
            }
            // create predefined report
            OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchMechType, ElectricEIRChiller(EIRChillNum).Name, "Chiller:Electric:EIR");
            OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchMechNomEff, ElectricEIRChiller(EIRChillNum).Name, ElectricEIRChiller(EIRChillNum).RefCOP);
            OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchMechNomCap, ElectricEIRChiller(EIRChillNum).Name, ElectricEIRChiller(EIRChillNum).RefCap);
        }

        if (ErrorsFound) {
            ShowFatalError("Preceding sizing errors cause program termination");
        }
    }

    void CalcElectricEIRChillerModel(int &EIRChillNum,                     // Chiller number
                                     Real64 &MyLoad,                       // Operating load
                                     bool const RunFlag,                   // TRUE when chiller operating
                                     bool const EP_UNUSED(FirstIteration), // TRUE when first iteration of timestep
                                     int const EquipFlowCtrl               // Flow control mode for the equipment
    )
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

        static ObjexxFCL::gio::Fmt OutputFormat("(F6.2)");
        static std::string const RoutineName("CalcElectricEIRChillerModel");

        Real64 FRAC;                        // Chiller cycling ratio
        Real64 MinPartLoadRat;              // Min allowed operating fraction of full load
        Real64 MinUnloadRat;                // Min allowed unloading fraction of full load
        Real64 MaxPartLoadRat;              // Max allowed operating fraction of full load
        Real64 CondInletTemp;               // Condenser inlet temperature [C]
        Real64 EvapOutletTempSetPoint(0.0); // Evaporator outlet temperature setpoint [C]
        Real64 AvailChillerCap;             // Chiller available capacity at current operating conditions [W]
        Real64 ChillerRefCap;               // Chiller reference capacity
        Real64 EvapDeltaTemp(0.0);          // Evaporator temperature difference [C]
        Real64 ReferenceCOP;                // Reference coefficient of performance, from user input
        Real64 PartLoadRat;                 // Operating part load ratio
        Real64 TempLowLimitEout;            // Evaporator low temp. limit cut off [C]
        Real64 EvapMassFlowRateMax;         // Max reference evaporator mass flow rate converted from volume flow rate [kg/s]
        int EvapOutletNode;                 // Evaporator outlet node number
        Real64 TempLoad(0.0);               // Actual load to be met by chiller. This value is compared to MyLoad
        // and reset when necessary since this chiller can cycle, the load passed
        // should be the actual load. Instead the minimum PLR * RefCap is
        // passed in. [W]
        int PlantLoopNum; // Plant loop which contains the current chiller
        int LoopSideNum;  // Plant loop side which contains the current chiller (usually supply side)
        int BranchNum;
        int CompNum;
        Real64 CurrentEndTime;                 // end time of time step for current simulation time step
        static std::string OutputChar;         // character string for warning messages
        Real64 Cp;                             // local fluid specific heat
        Real64 RhoAir;                         // air density [kg/m3]

        // Set module level inlet and outlet nodes and initialize other local variables
        ElectricEIRChiller(EIRChillNum).CondMassFlowRate = 0.0;
        EvapOutletNode = ElectricEIRChiller(EIRChillNum).EvapOutletNodeNum;
        PlantLoopNum = ElectricEIRChiller(EIRChillNum).CWLoopNum;
        LoopSideNum = ElectricEIRChiller(EIRChillNum).CWLoopSideNum;
        BranchNum = ElectricEIRChiller(EIRChillNum).CWBranchNum;
        CompNum = ElectricEIRChiller(EIRChillNum).CWCompNum;
        FRAC = 1.0;

        // Set performance curve outputs to 0.0 when chiller is off
        ElectricEIRChiller(EIRChillNum).ChillerCapFT = 0.0;
        ElectricEIRChiller(EIRChillNum).ChillerEIRFT = 0.0;
        ElectricEIRChiller(EIRChillNum).ChillerEIRFPLR = 0.0;

        // calculate end time of current time step
        CurrentEndTime = DataGlobals::CurrentTime + DataHVACGlobals::SysTimeElapsed;

        // Print warning messages only when valid and only for the first occurrence. Let summary provide statistics.
        // Wait for next time step to print warnings. If simulation iterates, print out
        // the warning for the last iteration only. Must wait for next time step to accomplish this.
        // If a warning occurs and the simulation down shifts, the warning is not valid.
        if (CurrentEndTime > ElectricEIRChiller(EIRChillNum).CurrentEndTimeLast && DataHVACGlobals::TimeStepSys >= ElectricEIRChiller(EIRChillNum).TimeStepSysLast) {
            if (ElectricEIRChiller(EIRChillNum).PrintMessage) {
                ++ElectricEIRChiller(EIRChillNum).MsgErrorCount;
                //     Show single warning and pass additional info to ShowRecurringWarningErrorAtEnd
                if (ElectricEIRChiller(EIRChillNum).MsgErrorCount < 2) {
                    ShowWarningError(ElectricEIRChiller(EIRChillNum).MsgBuffer1 + '.');
                    ShowContinueError(ElectricEIRChiller(EIRChillNum).MsgBuffer2);
                } else {
                    ShowRecurringWarningErrorAtEnd(ElectricEIRChiller(EIRChillNum).MsgBuffer1 + " error continues.",
                                                   ElectricEIRChiller(EIRChillNum).ErrCount1,
                                                   ElectricEIRChiller(EIRChillNum).MsgDataLast,
                                                   ElectricEIRChiller(EIRChillNum).MsgDataLast,
                                                   _,
                                                   "[C]",
                                                   "[C]");
                }
            }
        }

        // save last system time step and last end time of current time step (used to determine if warning is valid)
        ElectricEIRChiller(EIRChillNum).TimeStepSysLast = DataHVACGlobals::TimeStepSys;
        ElectricEIRChiller(EIRChillNum).CurrentEndTimeLast = CurrentEndTime;

        // If no loop demand or chiller OFF, return
        // If Chiller load is 0 or chiller is not running then leave the subroutine.Before leaving
        // if the component control is SERIESACTIVE we set the component flow to inlet flow so that
        // flow resolver will not shut down the branch
        if (MyLoad >= 0 || !RunFlag) {
            if (EquipFlowCtrl == DataBranchAirLoopPlant::ControlType_SeriesActive || DataPlant::PlantLoop(PlantLoopNum).LoopSide(LoopSideNum).FlowLock == 1) {
                ElectricEIRChiller(EIRChillNum).EvapMassFlowRate = DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).EvapInletNodeNum).MassFlowRate;
            }
            if (ElectricEIRChiller(EIRChillNum).CondenserType == WaterCooled) {
                if (DataPlant::PlantLoop(ElectricEIRChiller(EIRChillNum).CDLoopNum)
                        .LoopSide(ElectricEIRChiller(EIRChillNum).CDLoopSideNum)
                        .Branch(ElectricEIRChiller(EIRChillNum).CDBranchNum)
                        .Comp(ElectricEIRChiller(EIRChillNum).CDCompNum)
                        .FlowCtrl == DataBranchAirLoopPlant::ControlType_SeriesActive) {
                    ElectricEIRChiller(EIRChillNum).CondMassFlowRate = DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).CondInletNodeNum).MassFlowRate;
                }
            }
            if (ElectricEIRChiller(EIRChillNum).CondenserType == EvapCooled) {
                CalcBasinHeaterPower(ElectricEIRChiller(EIRChillNum).BasinHeaterPowerFTempDiff,
                                     ElectricEIRChiller(EIRChillNum).BasinHeaterSchedulePtr,
                                     ElectricEIRChiller(EIRChillNum).BasinHeaterSetPointTemp,
                                     ElectricEIRChiller(EIRChillNum).BasinHeaterPower);
            }
            ElectricEIRChiller(EIRChillNum).PrintMessage = false;
            return;
        }

        // initialize outlet air humidity ratio of air or evap cooled chillers
        ElectricEIRChiller(EIRChillNum).CondOutletHumRat = DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).CondInletNodeNum).HumRat;

        if (ElectricEIRChiller(EIRChillNum).CondenserType == AirCooled) { // Condenser inlet temp = outdoor temp
            DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).CondInletNodeNum).Temp = DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).CondInletNodeNum).OutAirDryBulb;

            // Warn user if entering condenser dry-bulb temperature falls below 0 C
            if (DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).CondInletNodeNum).Temp < 0.0 && std::abs(MyLoad) > 0 && RunFlag && !DataGlobals::WarmupFlag) {
                ElectricEIRChiller(EIRChillNum).PrintMessage = true;
                ObjexxFCL::gio::write(OutputChar, OutputFormat) << DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).CondInletNodeNum).Temp;
                ElectricEIRChiller(EIRChillNum).MsgBuffer1 = "ElectricEIRChillerModel - CHILLER:ELECTRIC:EIR \"" +
                                                             ElectricEIRChiller(EIRChillNum).Name +
                                                             "\" - Air Cooled Condenser Inlet Temperature below 0C";
                ElectricEIRChiller(EIRChillNum).MsgBuffer2 = "... Outdoor Dry-bulb Condition = " + OutputChar +
                                                             " C. Occurrence info = " + DataEnvironment::EnvironmentName + ", " + DataEnvironment::CurMnDy + ' ' +
                                                             General::CreateSysTimeIntervalString();
                ElectricEIRChiller(EIRChillNum).MsgDataLast = DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).CondInletNodeNum).Temp;
            } else {
                ElectricEIRChiller(EIRChillNum).PrintMessage = false;
            }
        } else if (ElectricEIRChiller(EIRChillNum).CondenserType == EvapCooled) { // Condenser inlet temp = (outdoor wet bulb)
            DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).CondInletNodeNum).Temp = DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).CondInletNodeNum).OutAirWetBulb;
            //  line above assumes evaporation pushes condenser inlet air humidity ratio to saturation
            ElectricEIRChiller(EIRChillNum).CondOutletHumRat = Psychrometrics::PsyWFnTdbTwbPb(DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).CondInletNodeNum).Temp, DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).CondInletNodeNum).Temp, DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).CondInletNodeNum).Press);

            // Warn user if evap condenser wet-bulb temperature falls below 10 C
            if (DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).CondInletNodeNum).Temp < 10.0 && std::abs(MyLoad) > 0 && RunFlag && !DataGlobals::WarmupFlag) {
                ElectricEIRChiller(EIRChillNum).PrintMessage = true;
                ObjexxFCL::gio::write(OutputChar, OutputFormat) << DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).CondInletNodeNum).Temp;
                ElectricEIRChiller(EIRChillNum).MsgBuffer1 = "ElectricEIRChillerModel - CHILLER:ELECTRIC:EIR \"" +
                                                             ElectricEIRChiller(EIRChillNum).Name +
                                                             "\" - Air Cooled Condenser Inlet Temperature below 10C";
                ElectricEIRChiller(EIRChillNum).MsgBuffer2 = "... Outdoor Wet-bulb Condition = " + OutputChar +
                                                             " C. Occurrence info = " + DataEnvironment::EnvironmentName + ", " + DataEnvironment::CurMnDy + ' ' +
                                                             General::CreateSysTimeIntervalString();
                ElectricEIRChiller(EIRChillNum).MsgDataLast = DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).CondInletNodeNum).Temp;
            } else {
                ElectricEIRChiller(EIRChillNum).PrintMessage = false;
            }
        } // End of the Air Cooled/Evap Cooled Logic block

        // If not air or evap cooled then set to the condenser node that is attached to a cooling tower
        CondInletTemp = DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).CondInletNodeNum).Temp;

        // LOAD LOCAL VARIABLES FROM DATA STRUCTURE (for code readability)
        MinPartLoadRat = ElectricEIRChiller(EIRChillNum).MinPartLoadRat;
        MaxPartLoadRat = ElectricEIRChiller(EIRChillNum).MaxPartLoadRat;
        MinUnloadRat = ElectricEIRChiller(EIRChillNum).MinUnloadRat;
        ChillerRefCap = ElectricEIRChiller(EIRChillNum).RefCap;
        ReferenceCOP = ElectricEIRChiller(EIRChillNum).RefCOP;
        ElectricEIRChiller(EIRChillNum).EvapOutletTemp = DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).EvapOutletNodeNum).Temp;
        TempLowLimitEout = ElectricEIRChiller(EIRChillNum).TempLowLimitEvapOut;
        EvapMassFlowRateMax = ElectricEIRChiller(EIRChillNum).EvapMassFlowRateMax;

        // If there is a fault of chiller fouling (zrp_Nov2016)
        if (ElectricEIRChiller(EIRChillNum).FaultyChillerFoulingFlag && (!DataGlobals::WarmupFlag) && (!DataGlobals::DoingSizing) && (!DataGlobals::KickOffSimulation)) {
            int FaultIndex = ElectricEIRChiller(EIRChillNum).FaultyChillerFoulingIndex;
            Real64 NomCap_ff = ChillerRefCap;
            Real64 ReferenceCOP_ff = ReferenceCOP;

            // calculate the Faulty Chiller Fouling Factor using fault information
            ElectricEIRChiller(EIRChillNum).FaultyChillerFoulingFactor = FaultsManager::FaultsChillerFouling(FaultIndex).CalFoulingFactor();

            // update the Chiller nominal capacity and COP at faulty cases
            ChillerRefCap = NomCap_ff * ElectricEIRChiller(EIRChillNum).FaultyChillerFoulingFactor;
            ReferenceCOP = ReferenceCOP_ff * ElectricEIRChiller(EIRChillNum).FaultyChillerFoulingFactor;
        }

        // Set mass flow rates
        if (ElectricEIRChiller(EIRChillNum).CondenserType == WaterCooled) {
            ElectricEIRChiller(EIRChillNum).CondMassFlowRate = ElectricEIRChiller(EIRChillNum).CondMassFlowRateMax;
            PlantUtilities::SetComponentFlowRate(ElectricEIRChiller(EIRChillNum).CondMassFlowRate,
                                 ElectricEIRChiller(EIRChillNum).CondInletNodeNum,
                                 ElectricEIRChiller(EIRChillNum).CondOutletNodeNum,
                                 ElectricEIRChiller(EIRChillNum).CDLoopNum,
                                 ElectricEIRChiller(EIRChillNum).CDLoopSideNum,
                                 ElectricEIRChiller(EIRChillNum).CDBranchNum,
                                 ElectricEIRChiller(EIRChillNum).CDCompNum);
            PlantUtilities::PullCompInterconnectTrigger(ElectricEIRChiller(EIRChillNum).CWLoopNum,
                                        ElectricEIRChiller(EIRChillNum).CWLoopSideNum,
                                        ElectricEIRChiller(EIRChillNum).CWBranchNum,
                                        ElectricEIRChiller(EIRChillNum).CWCompNum,
                                        ElectricEIRChiller(EIRChillNum).CondMassFlowIndex,
                                        ElectricEIRChiller(EIRChillNum).CDLoopNum,
                                        ElectricEIRChiller(EIRChillNum).CDLoopSideNum,
                                        DataPlant::CriteriaType_MassFlowRate,
                                                        ElectricEIRChiller(EIRChillNum).CondMassFlowRate);

            if (ElectricEIRChiller(EIRChillNum).CondMassFlowRate < DataBranchAirLoopPlant::MassFlowTolerance) {
                if (ElectricEIRChiller(EIRChillNum).EvapMassFlowRate < DataBranchAirLoopPlant::MassFlowTolerance) {
                    // Use PlantUtilities::SetComponentFlowRate to decide actual flow
                    PlantUtilities::SetComponentFlowRate(ElectricEIRChiller(EIRChillNum).EvapMassFlowRate,
                                         ElectricEIRChiller(EIRChillNum).EvapInletNodeNum,
                                         EvapOutletNode,
                                         ElectricEIRChiller(EIRChillNum).CWLoopNum,
                                         ElectricEIRChiller(EIRChillNum).CWLoopSideNum,
                                         ElectricEIRChiller(EIRChillNum).CWBranchNum,
                                         ElectricEIRChiller(EIRChillNum).CWCompNum);
                }
                return;
            }
        }

        {
            auto const SELECT_CASE_var(DataPlant::PlantLoop(ElectricEIRChiller(EIRChillNum).CWLoopNum).LoopDemandCalcScheme);
            if (SELECT_CASE_var == DataPlant::SingleSetPoint) {
                if ((ElectricEIRChiller(EIRChillNum).FlowMode == LeavingSetPointModulated) ||
                    (DataPlant::PlantLoop(PlantLoopNum).LoopSide(LoopSideNum).Branch(BranchNum).Comp(CompNum).CurOpSchemeType == DataPlant::CompSetPtBasedSchemeType) ||
                    (DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).EvapOutletNodeNum).TempSetPoint != DataLoopNode::SensedNodeFlagValue)) {
                    // there will be a valid setpoint on outlet
                    EvapOutletTempSetPoint = DataLoopNode::Node(EvapOutletNode).TempSetPoint;
                } else { // use plant loop overall setpoint
                    EvapOutletTempSetPoint = DataLoopNode::Node(DataPlant::PlantLoop(PlantLoopNum).TempSetPointNodeNum).TempSetPoint;
                }
            } else if (SELECT_CASE_var == DataPlant::DualSetPointDeadBand) {
                if ((ElectricEIRChiller(EIRChillNum).FlowMode == LeavingSetPointModulated) ||
                    (DataPlant::PlantLoop(PlantLoopNum).LoopSide(LoopSideNum).Branch(BranchNum).Comp(CompNum).CurOpSchemeType == DataPlant::CompSetPtBasedSchemeType) ||
                    (DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).EvapOutletNodeNum).TempSetPointHi != DataLoopNode::SensedNodeFlagValue)) {
                    // there will be a valid setpoint on outlet
                    EvapOutletTempSetPoint = DataLoopNode::Node(EvapOutletNode).TempSetPointHi;
                } else { // use plant loop overall setpoint
                    EvapOutletTempSetPoint = DataLoopNode::Node(DataPlant::PlantLoop(PlantLoopNum).TempSetPointNodeNum).TempSetPointHi;
                }
            } else {
                assert(false);
            }
        }

        // If there is a fault of Chiller SWT Sensor (zrp_Jun2016)
        if (ElectricEIRChiller(EIRChillNum).FaultyChillerSWTFlag && (!DataGlobals::WarmupFlag) && (!DataGlobals::DoingSizing) && (!DataGlobals::KickOffSimulation)) {
            int FaultIndex = ElectricEIRChiller(EIRChillNum).FaultyChillerSWTIndex;
            Real64 EvapOutletTempSetPoint_ff = EvapOutletTempSetPoint;

            // calculate the sensor offset using fault information
            ElectricEIRChiller(EIRChillNum).FaultyChillerSWTOffset = FaultsManager::FaultsChillerSWTSensor(FaultIndex).CalFaultOffsetAct();
            // update the EvapOutletTempSetPoint
            EvapOutletTempSetPoint =
                max(ElectricEIRChiller(EIRChillNum).TempLowLimitEvapOut,
                    min(DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).EvapInletNodeNum).Temp, EvapOutletTempSetPoint_ff - ElectricEIRChiller(EIRChillNum).FaultyChillerSWTOffset));
            ElectricEIRChiller(EIRChillNum).FaultyChillerSWTOffset = EvapOutletTempSetPoint_ff - EvapOutletTempSetPoint;
        }

        // correct temperature if using heat recovery
        // use report values for latest valid calculation, lagged somewhat
        Real64 AvgCondSinkTemp;
        if (ElectricEIRChiller(EIRChillNum).HeatRecActive) {
            if ((ElectricEIRChiller(EIRChillNum).QHeatRecovered + ElectricEIRChiller(EIRChillNum).QCondenser) > 0.0) { // protect div by zero
                AvgCondSinkTemp = (ElectricEIRChiller(EIRChillNum).QHeatRecovered * ElectricEIRChiller(EIRChillNum).HeatRecInletTemp +
                        ElectricEIRChiller(EIRChillNum).QCondenser * ElectricEIRChiller(EIRChillNum).CondInletTemp) /
                                  (ElectricEIRChiller(EIRChillNum).QHeatRecovered + ElectricEIRChiller(EIRChillNum).QCondenser);
            } else {
                AvgCondSinkTemp = CondInletTemp;
            }
        } else {
            AvgCondSinkTemp = CondInletTemp;
        }

        // Get capacity curve info with respect to CW setpoint and entering condenser water temps
        ElectricEIRChiller(EIRChillNum).ChillerCapFT = CurveManager::CurveValue(ElectricEIRChiller(EIRChillNum).ChillerCapFTIndex, EvapOutletTempSetPoint, AvgCondSinkTemp);

        if (ElectricEIRChiller(EIRChillNum).ChillerCapFT < 0) {
            if (ElectricEIRChiller(EIRChillNum).ChillerCapFTError < 1 && DataPlant::PlantLoop(PlantLoopNum).LoopSide(LoopSideNum).FlowLock != 0 && !DataGlobals::WarmupFlag) {
                ++ElectricEIRChiller(EIRChillNum).ChillerCapFTError;
                ShowWarningError("CHILLER:ELECTRIC:EIR \"" + ElectricEIRChiller(EIRChillNum).Name + "\":");
                ShowContinueError(" Chiller Capacity as a Function of Temperature curve output is negative (" + General::RoundSigDigits(ElectricEIRChiller(EIRChillNum).ChillerCapFT, 3) +
                                  ").");
                ShowContinueError(" Negative value occurs using an Evaporator Outlet Temp of " + General::RoundSigDigits(EvapOutletTempSetPoint, 1) +
                                  " and a Condenser Inlet Temp of " + General::RoundSigDigits(CondInletTemp, 1) + '.');
                ShowContinueErrorTimeStamp(" Resetting curve output to zero and continuing simulation.");
            } else if (DataPlant::PlantLoop(PlantLoopNum).LoopSide(LoopSideNum).FlowLock != 0 && !DataGlobals::WarmupFlag) {
                ++ElectricEIRChiller(EIRChillNum).ChillerCapFTError;
                ShowRecurringWarningErrorAtEnd("CHILLER:ELECTRIC:EIR \"" + ElectricEIRChiller(EIRChillNum).Name +
                                                   "\": Chiller Capacity as a Function of Temperature curve output is negative warning continues...",
                                               ElectricEIRChiller(EIRChillNum).ChillerCapFTErrorIndex,
                                               ElectricEIRChiller(EIRChillNum).ChillerCapFT,
                                               ElectricEIRChiller(EIRChillNum).ChillerCapFT);
            }
            ElectricEIRChiller(EIRChillNum).ChillerCapFT = 0.0;
        }

        // Available chiller capacity as a function of temperature
        AvailChillerCap = ChillerRefCap * ElectricEIRChiller(EIRChillNum).ChillerCapFT;

        // Only perform this check for temperature setpoint control
        if (DataPlant::PlantLoop(PlantLoopNum).LoopSide(LoopSideNum).Branch(BranchNum).Comp(CompNum).CurOpSchemeType == DataPlant::CompSetPtBasedSchemeType) {
            // Calculate water side load

            Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(ElectricEIRChiller(EIRChillNum).CWLoopNum).FluidName,
                                       DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).EvapInletNodeNum).Temp,
                                       DataPlant::PlantLoop(ElectricEIRChiller(EIRChillNum).CWLoopNum).FluidIndex,
                                       RoutineName);
            ElectricEIRChiller(EIRChillNum).EvapMassFlowRate = DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).EvapInletNodeNum).MassFlowRate;
            {
                auto const SELECT_CASE_var(DataPlant::PlantLoop(ElectricEIRChiller(EIRChillNum).CWLoopNum).LoopDemandCalcScheme);
                if (SELECT_CASE_var == DataPlant::SingleSetPoint) {
                    TempLoad = ElectricEIRChiller(EIRChillNum).EvapMassFlowRate * Cp * (DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).EvapInletNodeNum).Temp - DataLoopNode::Node(EvapOutletNode).TempSetPoint);
                } else if (SELECT_CASE_var == DataPlant::DualSetPointDeadBand) {
                    TempLoad = ElectricEIRChiller(EIRChillNum).EvapMassFlowRate * Cp * (DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).EvapInletNodeNum).Temp - DataLoopNode::Node(EvapOutletNode).TempSetPointHi);
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
        if (AvailChillerCap > 0) {
            PartLoadRat = max(0.0, min(std::abs(MyLoad) / AvailChillerCap, MaxPartLoadRat));
        } else {
            PartLoadRat = 0.0;
        }

        Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(ElectricEIRChiller(EIRChillNum).CWLoopNum).FluidName,
                                   DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).EvapInletNodeNum).Temp,
                                   DataPlant::PlantLoop(ElectricEIRChiller(EIRChillNum).CWLoopNum).FluidIndex,
                                   RoutineName);

        if (DataPlant::PlantLoop(PlantLoopNum).LoopSide(LoopSideNum).Branch(BranchNum).Comp(CompNum).CurOpSchemeType == DataPlant::CompSetPtBasedSchemeType) {
            ElectricEIRChiller(EIRChillNum).PossibleSubcooling = false;
        } else {
            ElectricEIRChiller(EIRChillNum).PossibleSubcooling = true;
        }
        // Set evaporator heat transfer rate
        ElectricEIRChiller(EIRChillNum).QEvaporator = AvailChillerCap * PartLoadRat;

        // Either set the flow to the Constant value or calculate the flow for the variable volume
        if ((ElectricEIRChiller(EIRChillNum).FlowMode == ConstantFlow) || (ElectricEIRChiller(EIRChillNum).FlowMode == NotModulated)) {
            // Set the evaporator mass flow rate to design
            // Start by assuming max (design) flow
            ElectricEIRChiller(EIRChillNum).EvapMassFlowRate = EvapMassFlowRateMax;
            // Use PlantUtilities::SetComponentFlowRate to decide actual flow
            PlantUtilities::SetComponentFlowRate(ElectricEIRChiller(EIRChillNum).EvapMassFlowRate,
                                 ElectricEIRChiller(EIRChillNum).EvapInletNodeNum,
                                 EvapOutletNode,
                                 ElectricEIRChiller(EIRChillNum).CWLoopNum,
                                 ElectricEIRChiller(EIRChillNum).CWLoopSideNum,
                                 ElectricEIRChiller(EIRChillNum).CWBranchNum,
                                 ElectricEIRChiller(EIRChillNum).CWCompNum);
            if (ElectricEIRChiller(EIRChillNum).EvapMassFlowRate != 0.0) {
                EvapDeltaTemp = ElectricEIRChiller(EIRChillNum).QEvaporator / ElectricEIRChiller(EIRChillNum).EvapMassFlowRate / Cp;
            } else {
                EvapDeltaTemp = 0.0;
            }
            // Evaluate outlet temp based on delta
            ElectricEIRChiller(EIRChillNum).EvapOutletTemp = DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).EvapInletNodeNum).Temp - EvapDeltaTemp;

        } else if (ElectricEIRChiller(EIRChillNum).FlowMode == LeavingSetPointModulated) {

            // Calculate the Delta Temp from the inlet temp to the chiller outlet setpoint
            {
                auto const SELECT_CASE_var(DataPlant::PlantLoop(ElectricEIRChiller(EIRChillNum).CWLoopNum).LoopDemandCalcScheme);
                if (SELECT_CASE_var == DataPlant::SingleSetPoint) {
                    EvapDeltaTemp = DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).EvapInletNodeNum).Temp - DataLoopNode::Node(EvapOutletNode).TempSetPoint;
                } else if (SELECT_CASE_var == DataPlant::DualSetPointDeadBand) {
                    EvapDeltaTemp = DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).EvapInletNodeNum).Temp - DataLoopNode::Node(EvapOutletNode).TempSetPointHi;
                } else {
                    assert(false);
                }
            }

            if (EvapDeltaTemp != 0) {
                // Calculate desired flow to request based on load
                ElectricEIRChiller(EIRChillNum).EvapMassFlowRate = std::abs(ElectricEIRChiller(EIRChillNum).QEvaporator / Cp / EvapDeltaTemp);
                if ((ElectricEIRChiller(EIRChillNum).EvapMassFlowRate - EvapMassFlowRateMax) > DataBranchAirLoopPlant::MassFlowTolerance) ElectricEIRChiller(EIRChillNum).PossibleSubcooling = true;
                // Check to see if the Maximum is exceeded, if so set to maximum
                ElectricEIRChiller(EIRChillNum).EvapMassFlowRate = min(EvapMassFlowRateMax, ElectricEIRChiller(EIRChillNum).EvapMassFlowRate);
                // Use PlantUtilities::SetComponentFlowRate to decide actual flow
                PlantUtilities::SetComponentFlowRate(ElectricEIRChiller(EIRChillNum).EvapMassFlowRate,
                                     ElectricEIRChiller(EIRChillNum).EvapInletNodeNum,
                                     EvapOutletNode,
                                     ElectricEIRChiller(EIRChillNum).CWLoopNum,
                                     ElectricEIRChiller(EIRChillNum).CWLoopSideNum,
                                     ElectricEIRChiller(EIRChillNum).CWBranchNum,
                                     ElectricEIRChiller(EIRChillNum).CWCompNum);
                // Should we recalculate this with the corrected setpoint?
                {
                    auto const SELECT_CASE_var(DataPlant::PlantLoop(ElectricEIRChiller(EIRChillNum).CWLoopNum).LoopDemandCalcScheme);
                    if (SELECT_CASE_var == DataPlant::SingleSetPoint) {
                        ElectricEIRChiller(EIRChillNum).EvapOutletTemp = DataLoopNode::Node(EvapOutletNode).TempSetPoint;
                    } else if (SELECT_CASE_var == DataPlant::DualSetPointDeadBand) {
                        ElectricEIRChiller(EIRChillNum).EvapOutletTemp = DataLoopNode::Node(EvapOutletNode).TempSetPointHi;
                    }
                }
                ElectricEIRChiller(EIRChillNum).QEvaporator = max(0.0, (ElectricEIRChiller(EIRChillNum).EvapMassFlowRate * Cp * EvapDeltaTemp));
            } else {
                // Try to request zero flow
                ElectricEIRChiller(EIRChillNum).EvapMassFlowRate = 0.0;
                // Use PlantUtilities::SetComponentFlowRate to decide actual flow
                PlantUtilities::SetComponentFlowRate(ElectricEIRChiller(EIRChillNum).EvapMassFlowRate,
                                     ElectricEIRChiller(EIRChillNum).EvapInletNodeNum,
                                     EvapOutletNode,
                                     ElectricEIRChiller(EIRChillNum).CWLoopNum,
                                     ElectricEIRChiller(EIRChillNum).CWLoopSideNum,
                                     ElectricEIRChiller(EIRChillNum).CWBranchNum,
                                     ElectricEIRChiller(EIRChillNum).CWCompNum);
                // No deltaT since component is not running
                ElectricEIRChiller(EIRChillNum).EvapOutletTemp = DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).EvapInletNodeNum).Temp;
                ElectricEIRChiller(EIRChillNum).QEvaporator = 0.0;
                PartLoadRat = 0.0;
                ElectricEIRChiller(EIRChillNum).ChillerPartLoadRatio = PartLoadRat;

                // DSU? so what if the delta T is zero?  On FlowLock==0, the inlet temp could = setpoint, right?
                if (ElectricEIRChiller(EIRChillNum).DeltaTErrCount < 1 && !DataGlobals::WarmupFlag) {
                    ++ElectricEIRChiller(EIRChillNum).DeltaTErrCount;
                    ShowWarningError("Evaporator DeltaTemp = 0 in mass flow calculation (Tevapin = Tsetpoint).");
                    ShowContinueErrorTimeStamp("");
                } else if (!DataGlobals::WarmupFlag) {
                    ++ElectricEIRChiller(EIRChillNum).ChillerCapFTError;
                    ShowRecurringWarningErrorAtEnd("CHILLER:ELECTRIC:EIR \"" + ElectricEIRChiller(EIRChillNum).Name +
                                                       "\": Evaporator DeltaTemp = 0 in mass flow calculation warning continues...",
                                                   ElectricEIRChiller(EIRChillNum).DeltaTErrCountIndex,
                                                   EvapDeltaTemp,
                                                   EvapDeltaTemp);
                }
            }
        } // End of Constant Variable Flow If Block

        if (ElectricEIRChiller(EIRChillNum).EvapMassFlowRate == 0.0) {
            MyLoad = 0.0;
            if (ElectricEIRChiller(EIRChillNum).CondenserType == EvapCooled) {
                CalcBasinHeaterPower(ElectricEIRChiller(EIRChillNum).BasinHeaterPowerFTempDiff,
                                     ElectricEIRChiller(EIRChillNum).BasinHeaterSchedulePtr,
                                     ElectricEIRChiller(EIRChillNum).BasinHeaterSetPointTemp,
                                     ElectricEIRChiller(EIRChillNum).BasinHeaterPower);
            }
            ElectricEIRChiller(EIRChillNum).PrintMessage = false;
            return;
        }
        if (ElectricEIRChiller(EIRChillNum).PossibleSubcooling) {
            ElectricEIRChiller(EIRChillNum).QEvaporator = std::abs(MyLoad);
            EvapDeltaTemp = ElectricEIRChiller(EIRChillNum).QEvaporator / ElectricEIRChiller(EIRChillNum).EvapMassFlowRate / Cp;
            ElectricEIRChiller(EIRChillNum).EvapOutletTemp = DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).EvapInletNodeNum).Temp - EvapDeltaTemp;
        } else {
            EvapDeltaTemp = DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).EvapInletNodeNum).Temp - EvapOutletTempSetPoint;
            ElectricEIRChiller(EIRChillNum).QEvaporator = max(0.0, (ElectricEIRChiller(EIRChillNum).EvapMassFlowRate * Cp * EvapDeltaTemp));
            ElectricEIRChiller(EIRChillNum).EvapOutletTemp = EvapOutletTempSetPoint;
        }

        // Check that the Evap outlet temp honors both plant loop temp low limit and also the chiller low limit
        if (ElectricEIRChiller(EIRChillNum).EvapOutletTemp < TempLowLimitEout) {
            if ((DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).EvapInletNodeNum).Temp - TempLowLimitEout) > DataPlant::DeltaTempTol) {
                ElectricEIRChiller(EIRChillNum).EvapOutletTemp = TempLowLimitEout;
                EvapDeltaTemp = DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).EvapInletNodeNum).Temp - ElectricEIRChiller(EIRChillNum).EvapOutletTemp;
                ElectricEIRChiller(EIRChillNum).QEvaporator = ElectricEIRChiller(EIRChillNum).EvapMassFlowRate * Cp * EvapDeltaTemp;
            } else {
                ElectricEIRChiller(EIRChillNum).EvapOutletTemp = DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).EvapInletNodeNum).Temp;
                EvapDeltaTemp = DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).EvapInletNodeNum).Temp - ElectricEIRChiller(EIRChillNum).EvapOutletTemp;
                ElectricEIRChiller(EIRChillNum).QEvaporator = ElectricEIRChiller(EIRChillNum).EvapMassFlowRate * Cp * EvapDeltaTemp;
            }
        }
        if (ElectricEIRChiller(EIRChillNum).EvapOutletTemp < DataLoopNode::Node(EvapOutletNode).TempMin) {
            if ((DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).EvapInletNodeNum).Temp - DataLoopNode::Node(EvapOutletNode).TempMin) > DataPlant::DeltaTempTol) {
                ElectricEIRChiller(EIRChillNum).EvapOutletTemp = DataLoopNode::Node(EvapOutletNode).TempMin;
                EvapDeltaTemp = DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).EvapInletNodeNum).Temp - ElectricEIRChiller(EIRChillNum).EvapOutletTemp;
                ElectricEIRChiller(EIRChillNum).QEvaporator = ElectricEIRChiller(EIRChillNum).EvapMassFlowRate * Cp * EvapDeltaTemp;
            } else {
                ElectricEIRChiller(EIRChillNum).EvapOutletTemp = DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).EvapInletNodeNum).Temp;
                EvapDeltaTemp = DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).EvapInletNodeNum).Temp - ElectricEIRChiller(EIRChillNum).EvapOutletTemp;
                ElectricEIRChiller(EIRChillNum).QEvaporator = ElectricEIRChiller(EIRChillNum).EvapMassFlowRate * Cp * EvapDeltaTemp;
            }
        }
        // If load exceeds the distributed load set to the distributed load
        if (ElectricEIRChiller(EIRChillNum).QEvaporator > std::abs(MyLoad)) {
            if (ElectricEIRChiller(EIRChillNum).EvapMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance) {
                ElectricEIRChiller(EIRChillNum).QEvaporator = std::abs(MyLoad);
                EvapDeltaTemp = ElectricEIRChiller(EIRChillNum).QEvaporator / ElectricEIRChiller(EIRChillNum).EvapMassFlowRate / Cp;
                ElectricEIRChiller(EIRChillNum).EvapOutletTemp = DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).EvapInletNodeNum).Temp - EvapDeltaTemp;
            } else {
                ElectricEIRChiller(EIRChillNum).QEvaporator = 0.0;
                ElectricEIRChiller(EIRChillNum).EvapOutletTemp = DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).EvapInletNodeNum).Temp;
            }
        }

        // If there is a fault of Chiller SWT Sensor (zrp_Jun2016)
        if (ElectricEIRChiller(EIRChillNum).FaultyChillerSWTFlag && (!DataGlobals::WarmupFlag) && (!DataGlobals::DoingSizing) && (!DataGlobals::KickOffSimulation) &&
            (ElectricEIRChiller(EIRChillNum).EvapMassFlowRate > 0)) {
            // calculate directly affected variables at faulty case: EvapOutletTemp, EvapMassFlowRate, QEvaporator
            int FaultIndex = ElectricEIRChiller(EIRChillNum).FaultyChillerSWTIndex;
            bool VarFlowFlag = (ElectricEIRChiller(EIRChillNum).FlowMode == LeavingSetPointModulated);
            FaultsManager::FaultsChillerSWTSensor(FaultIndex)
                .CalFaultChillerSWT(VarFlowFlag,
                                    ElectricEIRChiller(EIRChillNum).FaultyChillerSWTOffset,
                                    Cp,
                                    DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).EvapInletNodeNum).Temp,
                                    ElectricEIRChiller(EIRChillNum).EvapOutletTemp,
                                    ElectricEIRChiller(EIRChillNum).EvapMassFlowRate,
                                    ElectricEIRChiller(EIRChillNum).QEvaporator);
            // update corresponding variables at faulty case
            PartLoadRat = (AvailChillerCap > 0.0) ? (ElectricEIRChiller(EIRChillNum).QEvaporator / AvailChillerCap) : 0.0;
            PartLoadRat = max(0.0, min(PartLoadRat, MaxPartLoadRat));
            ElectricEIRChiller(EIRChillNum).ChillerPartLoadRatio = PartLoadRat;
        }

        // Checks QEvaporator on the basis of the machine limits.
        if (ElectricEIRChiller(EIRChillNum).QEvaporator > (AvailChillerCap * MaxPartLoadRat)) {
            if (ElectricEIRChiller(EIRChillNum).EvapMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance) {
                ElectricEIRChiller(EIRChillNum).QEvaporator = AvailChillerCap * MaxPartLoadRat;
                EvapDeltaTemp = ElectricEIRChiller(EIRChillNum).QEvaporator / ElectricEIRChiller(EIRChillNum).EvapMassFlowRate / Cp;
                // evaporator outlet temperature is allowed to float upwards (recalculate AvailChillerCap? iterate?)
                ElectricEIRChiller(EIRChillNum).EvapOutletTemp = DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).EvapInletNodeNum).Temp - EvapDeltaTemp;
            } else {
                ElectricEIRChiller(EIRChillNum).QEvaporator = 0.0;
                ElectricEIRChiller(EIRChillNum).EvapOutletTemp = DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).EvapInletNodeNum).Temp;
            }
        }

        if (AvailChillerCap > 0.0) {
            PartLoadRat = max(0.0, min((ElectricEIRChiller(EIRChillNum).QEvaporator / AvailChillerCap), MaxPartLoadRat));
        } else {
            PartLoadRat = 0.0;
        }

        // Chiller cycles below minimum part load ratio, FRAC = amount of time chiller is ON during this time step
        if (PartLoadRat < MinPartLoadRat) FRAC = min(1.0, (PartLoadRat / MinPartLoadRat));

        // set the module level variable used for reporting FRAC
        ElectricEIRChiller(EIRChillNum).ChillerCyclingRatio = FRAC;

        // Chiller is false loading below PLR = minimum unloading ratio, find PLR used for energy calculation
        if (AvailChillerCap > 0.0) {
            PartLoadRat = max(PartLoadRat, MinUnloadRat);
        } else {
            PartLoadRat = 0.0;
        }

        // set the module level variable used for reporting PLR
        ElectricEIRChiller(EIRChillNum).ChillerPartLoadRatio = PartLoadRat;

        // calculate the load due to false loading on chiller over and above water side load
        ElectricEIRChiller(EIRChillNum).ChillerFalseLoadRate = (AvailChillerCap * PartLoadRat * FRAC) - ElectricEIRChiller(EIRChillNum).QEvaporator;
        if (ElectricEIRChiller(EIRChillNum).ChillerFalseLoadRate < DataHVACGlobals::SmallLoad) {
            ElectricEIRChiller(EIRChillNum).ChillerFalseLoadRate = 0.0;
        }
        if (ElectricEIRChiller(EIRChillNum).QEvaporator == 0.0 && ElectricEIRChiller(EIRChillNum).CondenserType == EvapCooled) {
            CalcBasinHeaterPower(ElectricEIRChiller(EIRChillNum).BasinHeaterPowerFTempDiff,
                                 ElectricEIRChiller(EIRChillNum).BasinHeaterSchedulePtr,
                                 ElectricEIRChiller(EIRChillNum).BasinHeaterSetPointTemp,
                                 ElectricEIRChiller(EIRChillNum).BasinHeaterPower);
        }

        ElectricEIRChiller(EIRChillNum).ChillerEIRFT = CurveManager::CurveValue(ElectricEIRChiller(EIRChillNum).ChillerEIRFTIndex, ElectricEIRChiller(EIRChillNum).EvapOutletTemp, AvgCondSinkTemp);
        if (ElectricEIRChiller(EIRChillNum).ChillerEIRFT < 0.0) {
            if (ElectricEIRChiller(EIRChillNum).ChillerEIRFTError < 1 && DataPlant::PlantLoop(PlantLoopNum).LoopSide(LoopSideNum).FlowLock != 0 && !DataGlobals::WarmupFlag) {
                ++ElectricEIRChiller(EIRChillNum).ChillerEIRFTError;
                ShowWarningError("CHILLER:ELECTRIC:EIR \"" + ElectricEIRChiller(EIRChillNum).Name + "\":");
                ShowContinueError(" Chiller EIR as a Function of Temperature curve output is negative (" + General::RoundSigDigits(ElectricEIRChiller(EIRChillNum).ChillerEIRFT, 3) + ").");
                ShowContinueError(" Negative value occurs using an Evaporator Outlet Temp of " + General::RoundSigDigits(ElectricEIRChiller(EIRChillNum).EvapOutletTemp, 1) +
                                  " and a Condenser Inlet Temp of " + General::RoundSigDigits(CondInletTemp, 1) + '.');
                ShowContinueErrorTimeStamp(" Resetting curve output to zero and continuing simulation.");
            } else if (DataPlant::PlantLoop(PlantLoopNum).LoopSide(LoopSideNum).FlowLock != 0 && !DataGlobals::WarmupFlag) {
                ++ElectricEIRChiller(EIRChillNum).ChillerEIRFTError;
                ShowRecurringWarningErrorAtEnd("CHILLER:ELECTRIC:EIR \"" + ElectricEIRChiller(EIRChillNum).Name +
                                                   "\": Chiller EIR as a Function of Temperature curve output is negative warning continues...",
                                               ElectricEIRChiller(EIRChillNum).ChillerEIRFTErrorIndex,
                                               ElectricEIRChiller(EIRChillNum).ChillerEIRFT,
                                               ElectricEIRChiller(EIRChillNum).ChillerEIRFT);
            }
            ElectricEIRChiller(EIRChillNum).ChillerEIRFT = 0.0;
        }

        ElectricEIRChiller(EIRChillNum).ChillerEIRFPLR = CurveManager::CurveValue(ElectricEIRChiller(EIRChillNum).ChillerEIRFPLRIndex, PartLoadRat);
        if (ElectricEIRChiller(EIRChillNum).ChillerEIRFPLR < 0.0) {
            if (ElectricEIRChiller(EIRChillNum).ChillerEIRFPLRError < 1 && DataPlant::PlantLoop(PlantLoopNum).LoopSide(LoopSideNum).FlowLock != 0 &&
                !DataGlobals::WarmupFlag) {
                ++ElectricEIRChiller(EIRChillNum).ChillerEIRFPLRError;
                ShowWarningError("CHILLER:ELECTRIC:EIR \"" + ElectricEIRChiller(EIRChillNum).Name + "\":");
                ShowContinueError(" Chiller EIR as a function of PLR curve output is negative (" + General::RoundSigDigits(ElectricEIRChiller(EIRChillNum).ChillerEIRFPLR, 3) + ").");
                ShowContinueError(" Negative value occurs using a part-load ratio of " + General::RoundSigDigits(PartLoadRat, 3) + '.');
                ShowContinueErrorTimeStamp(" Resetting curve output to zero and continuing simulation.");
            } else if (DataPlant::PlantLoop(PlantLoopNum).LoopSide(LoopSideNum).FlowLock != 0 && !DataGlobals::WarmupFlag) {
                ++ElectricEIRChiller(EIRChillNum).ChillerEIRFPLRError;
                ShowRecurringWarningErrorAtEnd("CHILLER:ELECTRIC:EIR \"" + ElectricEIRChiller(EIRChillNum).Name +
                                                   "\": Chiller EIR as a function of PLR curve output is negative warning continues...",
                                               ElectricEIRChiller(EIRChillNum).ChillerEIRFPLRErrorIndex,
                                               ElectricEIRChiller(EIRChillNum).ChillerEIRFPLR,
                                               ElectricEIRChiller(EIRChillNum).ChillerEIRFPLR);
            }
            ElectricEIRChiller(EIRChillNum).ChillerEIRFPLR = 0.0;
        }

        ElectricEIRChiller(EIRChillNum).Power = (AvailChillerCap / ReferenceCOP) * ElectricEIRChiller(EIRChillNum).ChillerEIRFPLR * ElectricEIRChiller(EIRChillNum).ChillerEIRFT * FRAC;

        ElectricEIRChiller(EIRChillNum).QCondenser = ElectricEIRChiller(EIRChillNum).Power * ElectricEIRChiller(EIRChillNum).CompPowerToCondenserFrac + ElectricEIRChiller(EIRChillNum).QEvaporator + ElectricEIRChiller(EIRChillNum).ChillerFalseLoadRate;

        if (ElectricEIRChiller(EIRChillNum).CondenserType == WaterCooled) {
            if (ElectricEIRChiller(EIRChillNum).CondMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance) {
                // If Heat Recovery specified for this vapor compression chiller, then Qcondenser will be adjusted by this subroutine
                if (ElectricEIRChiller(EIRChillNum).HeatRecActive)
                    EIRChillerHeatRecovery(EIRChillNum, ElectricEIRChiller(EIRChillNum).QCondenser, ElectricEIRChiller(EIRChillNum).CondMassFlowRate, CondInletTemp, ElectricEIRChiller(EIRChillNum).QHeatRecovered);
                Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(ElectricEIRChiller(EIRChillNum).CDLoopNum).FluidName,
                                           CondInletTemp,
                                           DataPlant::PlantLoop(ElectricEIRChiller(EIRChillNum).CDLoopNum).FluidIndex,
                                           RoutineName);

                ElectricEIRChiller(EIRChillNum).CondOutletTemp = ElectricEIRChiller(EIRChillNum).QCondenser / ElectricEIRChiller(EIRChillNum).CondMassFlowRate / Cp + CondInletTemp;
            } else {
                ShowSevereError("CalcElectricEIRChillerModel: Condenser flow = 0, for ElectricEIRChiller=" + ElectricEIRChiller(EIRChillNum).Name);
                ShowContinueErrorTimeStamp("");
                // DSU? maybe this could be handled earlier, check if this component has a load and an evap flow rate
                // then if cond flow is zero, just make a request to the condenser,
                // then just say it couldn't run until condenser loop wakes up.
                // CALL ShowFatalError('Program Terminates due to previous error condition.')
            }
        } else { // Air Cooled or Evap Cooled

            if (ElectricEIRChiller(EIRChillNum).QCondenser > 0.0) {
                ElectricEIRChiller(EIRChillNum).CondMassFlowRate = ElectricEIRChiller(EIRChillNum).CondMassFlowRateMax * PartLoadRat;
            } else {
                ElectricEIRChiller(EIRChillNum).CondMassFlowRate = 0.0;
            }

            // If Heat Recovery specified for this vapor compression chiller, then Qcondenser will be adjusted by this subroutine
            if (ElectricEIRChiller(EIRChillNum).HeatRecActive)
                EIRChillerHeatRecovery(EIRChillNum, ElectricEIRChiller(EIRChillNum).QCondenser, ElectricEIRChiller(EIRChillNum).CondMassFlowRate, CondInletTemp, ElectricEIRChiller(EIRChillNum).QHeatRecovered);

            if (ElectricEIRChiller(EIRChillNum).CondMassFlowRate > 0.0) {
                Cp = Psychrometrics::PsyCpAirFnWTdb(DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).CondInletNodeNum).HumRat, CondInletTemp);
                ElectricEIRChiller(EIRChillNum).CondOutletTemp = CondInletTemp + ElectricEIRChiller(EIRChillNum).QCondenser / ElectricEIRChiller(EIRChillNum).CondMassFlowRate / Cp;
            } else {
                ElectricEIRChiller(EIRChillNum).CondOutletTemp = CondInletTemp;
            }

            if (ElectricEIRChiller(EIRChillNum).CondenserType == EvapCooled) {
                RhoAir = Psychrometrics::PsyRhoAirFnPbTdbW(DataEnvironment::StdBaroPress, CondInletTemp, DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).CondInletNodeNum).HumRat, RoutineName);
                // CondMassFlowRate is already multiplied by PLR, convert to water use rate
                ElectricEIRChiller(EIRChillNum).EvapWaterConsumpRate = ((ElectricEIRChiller(EIRChillNum).CondOutletHumRat - DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).CondInletNodeNum).HumRat) * ElectricEIRChiller(EIRChillNum).CondMassFlowRate) / RhoAir;
            }
        }

        // Calculate condenser fan power
        if (ElectricEIRChiller(EIRChillNum).ChillerCapFT > 0.0) {
            ElectricEIRChiller(EIRChillNum).CondenserFanPower = ChillerRefCap * ElectricEIRChiller(EIRChillNum).CondenserFanPowerRatio * FRAC;
        } else {
            ElectricEIRChiller(EIRChillNum).CondenserFanPower = 0.0;
        }
    }

    void EIRChillerHeatRecovery(int const EIRChillNum,      // Number of the current electric EIR chiller being simulated
                                Real64 &QCond,              // Current condenser load [W]
                                Real64 const CondMassFlow,  // Current condenser mass flow [kg/s]
                                Real64 const CondInletTemp, // Current condenser inlet temp [C]
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

        int HeatRecInNode;            // Node number of heat recovery water inlet node
        Real64 QTotal;                // Total condenser heat [W]
        Real64 HeatRecInletTemp;      // Heat reclaim inlet temp [C]
        Real64 HeatRecMassFlowRate;   // Heat reclaim mass flow rate [m3/s]
        Real64 TAvgIn;                // Average inlet temperature of heat reclaim inlet and condenser inlet [C]
        Real64 TAvgOut;               // Average outlet temperature [C]
        Real64 CpHeatRec;             // Heat reclaim water inlet specific heat [J/kg-K]
        Real64 CpCond;                // Condenser water inlet specific heat [J/kg-K]
        Real64 THeatRecSetPoint(0.0); // local value for heat recovery leaving setpoint [C]
        Real64 QHeatRecToSetPoint;    // load to heat recovery setpoint
        Real64 HeatRecHighInletLimit; // local value for inlet limit for heat recovery [C]

        // Begin routine
        HeatRecInNode = ElectricEIRChiller(EIRChillNum).HeatRecInletNodeNum;

        // Inlet node to the heat recovery heat exchanger
        HeatRecInletTemp = DataLoopNode::Node(HeatRecInNode).Temp;
        HeatRecMassFlowRate = DataLoopNode::Node(HeatRecInNode).MassFlowRate;

        CpHeatRec = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(ElectricEIRChiller(EIRChillNum).HRLoopNum).FluidName,
                                          HeatRecInletTemp,
                                          DataPlant::PlantLoop(ElectricEIRChiller(EIRChillNum).HRLoopNum).FluidIndex,
                                          RoutineName);
        CpCond = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(ElectricEIRChiller(EIRChillNum).CDLoopNum).FluidName,
                                       CondInletTemp,
                                       DataPlant::PlantLoop(ElectricEIRChiller(EIRChillNum).CDLoopNum).FluidIndex,
                                       RoutineName);

        // Before we modify the QCondenser, the total or original value is transferred to QTot
        QTotal = QCond;

        if (ElectricEIRChiller(EIRChillNum).HeatRecSetPointNodeNum == 0) { // use original algorithm that blends temps
            TAvgIn = (HeatRecMassFlowRate * CpHeatRec * HeatRecInletTemp + CondMassFlow * CpCond * CondInletTemp) /
                     (HeatRecMassFlowRate * CpHeatRec + CondMassFlow * CpCond);

            TAvgOut = QTotal / (HeatRecMassFlowRate * CpHeatRec + CondMassFlow * CpCond) + TAvgIn;

            QHeatRec = HeatRecMassFlowRate * CpHeatRec * (TAvgOut - HeatRecInletTemp);
            QHeatRec = max(QHeatRec, 0.0); // ensure non negative
            // check if heat flow too large for physical size of bundle
            QHeatRec = min(QHeatRec, ElectricEIRChiller(EIRChillNum).HeatRecMaxCapacityLimit);
        } else { // use new algorithm to meet setpoint
            {
                auto const SELECT_CASE_var(DataPlant::PlantLoop(ElectricEIRChiller(EIRChillNum).HRLoopNum).LoopDemandCalcScheme);

                if (SELECT_CASE_var == DataPlant::SingleSetPoint) {
                    THeatRecSetPoint = DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).HeatRecSetPointNodeNum).TempSetPoint;
                } else if (SELECT_CASE_var == DataPlant::DualSetPointDeadBand) {
                    THeatRecSetPoint = DataLoopNode::Node(ElectricEIRChiller(EIRChillNum).HeatRecSetPointNodeNum).TempSetPointHi;
                } else {
                    assert(false);
                }
            }

            QHeatRecToSetPoint = HeatRecMassFlowRate * CpHeatRec * (THeatRecSetPoint - HeatRecInletTemp);
            QHeatRecToSetPoint = max(QHeatRecToSetPoint, 0.0);
            QHeatRec = min(QTotal, QHeatRecToSetPoint);
            // check if heat flow too large for physical size of bundle
            QHeatRec = min(QHeatRec, ElectricEIRChiller(EIRChillNum).HeatRecMaxCapacityLimit);
        }

        // check if limit on inlet is present and exceeded.
        if (ElectricEIRChiller(EIRChillNum).HeatRecInletLimitSchedNum > 0) {
            HeatRecHighInletLimit = ScheduleManager::GetCurrentScheduleValue(ElectricEIRChiller(EIRChillNum).HeatRecInletLimitSchedNum);
            if (HeatRecInletTemp > HeatRecHighInletLimit) { // shut down heat recovery
                QHeatRec = 0.0;
            }
        }

        QCond = QTotal - QHeatRec;

        // Calculate a new Heat Recovery Coil Outlet Temp
        if (HeatRecMassFlowRate > 0.0) {
            ElectricEIRChiller(EIRChillNum).HeatRecOutletTemp = QHeatRec / (HeatRecMassFlowRate * CpHeatRec) + HeatRecInletTemp;
        } else {
            ElectricEIRChiller(EIRChillNum).HeatRecOutletTemp = HeatRecInletTemp;
        }
    }

    void UpdateElectricEIRChillerRecords(Real64 const MyLoad, // Current load [W]
                                         bool const RunFlag,  // TRUE if chiller operating
                                         int const Num        // Chiller number
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Richard Raustad, FSEC
        //       DATE WRITTEN:    June 2004

        // PURPOSE OF THIS SUBROUTINE:
        //  Reporting

        int EvapOutletNode;       // Evaporator outlet node number
        int HeatRecInNode;        // Node number of heat recovery water inlet node
        int HeatRecOutNode;       // Node number of heat recovery water outlet node
        Real64 ReportingConstant; // Number of seconds per HVAC system time step, to convert from W (J/s) to J

        ReportingConstant = DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

        EvapOutletNode = ElectricEIRChiller(Num).EvapOutletNodeNum;
        HeatRecInNode = ElectricEIRChiller(Num).HeatRecInletNodeNum;
        HeatRecOutNode = ElectricEIRChiller(Num).HeatRecOutletNodeNum;

        if (MyLoad >= 0 || !RunFlag) { // Chiller not running so pass inlet states to outlet states
            // Set node conditions
            DataLoopNode::Node(EvapOutletNode).Temp = DataLoopNode::Node(ElectricEIRChiller(Num).EvapInletNodeNum).Temp;
            DataLoopNode::Node(ElectricEIRChiller(Num).CondOutletNodeNum).Temp = DataLoopNode::Node(ElectricEIRChiller(Num).CondInletNodeNum).Temp;
            if (ElectricEIRChiller(Num).CondenserType != WaterCooled) {
                DataLoopNode::Node(ElectricEIRChiller(Num).CondOutletNodeNum).HumRat = DataLoopNode::Node(ElectricEIRChiller(Num).CondInletNodeNum).HumRat;
                DataLoopNode::Node(ElectricEIRChiller(Num).CondOutletNodeNum).Enthalpy = DataLoopNode::Node(ElectricEIRChiller(Num).CondInletNodeNum).Enthalpy;
                DataLoopNode::Node(ElectricEIRChiller(Num).CondInletNodeNum).MassFlowRate = 0.0;
                DataLoopNode::Node(ElectricEIRChiller(Num).CondOutletNodeNum).MassFlowRate = 0.0;
            }

            ElectricEIRChiller(Num).ChillerPartLoadRatio = 0.0;
            ElectricEIRChiller(Num).ChillerCyclingRatio = 0.0;
            ElectricEIRChiller(Num).ChillerFalseLoadRate = 0.0;
            ElectricEIRChiller(Num).ChillerFalseLoad = 0.0;
            ElectricEIRChiller(Num).Power = 0.0;
            ElectricEIRChiller(Num).QEvaporator = 0.0;
            ElectricEIRChiller(Num).QCondenser = 0.0;
            ElectricEIRChiller(Num).Energy = 0.0;
            ElectricEIRChiller(Num).EvapEnergy = 0.0;
            ElectricEIRChiller(Num).CondEnergy = 0.0;
            ElectricEIRChiller(Num).EvapInletTemp = DataLoopNode::Node(ElectricEIRChiller(Num).EvapInletNodeNum).Temp;
            ElectricEIRChiller(Num).CondInletTemp = DataLoopNode::Node(ElectricEIRChiller(Num).CondInletNodeNum).Temp;
            ElectricEIRChiller(Num).CondOutletTemp = DataLoopNode::Node(ElectricEIRChiller(Num).CondOutletNodeNum).Temp;
            ElectricEIRChiller(Num).EvapOutletTemp = DataLoopNode::Node(EvapOutletNode).Temp;
            ElectricEIRChiller(Num).ActualCOP = 0.0;
            ElectricEIRChiller(Num).CondenserFanPower = 0.0;
            ElectricEIRChiller(Num).CondenserFanEnergyConsumption = 0.0;
            if (ElectricEIRChiller(Num).CondenserType == EvapCooled) {
                ElectricEIRChiller(Num).BasinHeaterConsumption = ElectricEIRChiller(Num).BasinHeaterPower * ReportingConstant;
                ElectricEIRChiller(Num).EvapWaterConsump = 0.0;
            }

            if (ElectricEIRChiller(Num).HeatRecActive) {

                PlantUtilities::SafeCopyPlantNode(HeatRecInNode, HeatRecOutNode);

                ElectricEIRChiller(Num).QHeatRecovered = 0.0;
                ElectricEIRChiller(Num).EnergyHeatRecovery = 0.0;
                ElectricEIRChiller(Num).HeatRecInletTemp = DataLoopNode::Node(HeatRecInNode).Temp;
                ElectricEIRChiller(Num).HeatRecOutletTemp = DataLoopNode::Node(HeatRecOutNode).Temp;
                ElectricEIRChiller(Num).HeatRecMassFlow = DataLoopNode::Node(HeatRecInNode).MassFlowRate;
            }

        } else { // Chiller is running, so pass calculated values
            // Set node temperatures
            if (ElectricEIRChiller(Num).CondMassFlowRate < DataBranchAirLoopPlant::MassFlowTolerance && ElectricEIRChiller(Num).EvapMassFlowRate < DataBranchAirLoopPlant::MassFlowTolerance) {
                DataLoopNode::Node(EvapOutletNode).Temp = DataLoopNode::Node(ElectricEIRChiller(Num).EvapInletNodeNum).Temp;
                DataLoopNode::Node(ElectricEIRChiller(Num).CondOutletNodeNum).Temp = DataLoopNode::Node(ElectricEIRChiller(Num).CondInletNodeNum).Temp;
                if (ElectricEIRChiller(Num).CondenserType != WaterCooled) {
                    DataLoopNode::Node(ElectricEIRChiller(Num).CondOutletNodeNum).HumRat = DataLoopNode::Node(ElectricEIRChiller(Num).CondInletNodeNum).HumRat;
                    DataLoopNode::Node(ElectricEIRChiller(Num).CondOutletNodeNum).Enthalpy = DataLoopNode::Node(ElectricEIRChiller(Num).CondInletNodeNum).Enthalpy;
                    DataLoopNode::Node(ElectricEIRChiller(Num).CondInletNodeNum).MassFlowRate = 0.0;
                    DataLoopNode::Node(ElectricEIRChiller(Num).CondOutletNodeNum).MassFlowRate = 0.0;
                }
            } else {
                DataLoopNode::Node(EvapOutletNode).Temp = ElectricEIRChiller(Num).EvapOutletTemp;
                DataLoopNode::Node(ElectricEIRChiller(Num).CondOutletNodeNum).Temp = ElectricEIRChiller(Num).CondOutletTemp;
                if (ElectricEIRChiller(Num).CondenserType != WaterCooled) {
                    DataLoopNode::Node(ElectricEIRChiller(Num).CondOutletNodeNum).HumRat = ElectricEIRChiller(Num).CondOutletHumRat;
                    DataLoopNode::Node(ElectricEIRChiller(Num).CondOutletNodeNum).Enthalpy = Psychrometrics::PsyHFnTdbW(ElectricEIRChiller(Num).CondOutletTemp, ElectricEIRChiller(Num).CondOutletHumRat);
                    DataLoopNode::Node(ElectricEIRChiller(Num).CondInletNodeNum).MassFlowRate = ElectricEIRChiller(Num).CondMassFlowRate;
                    DataLoopNode::Node(ElectricEIRChiller(Num).CondOutletNodeNum).MassFlowRate = ElectricEIRChiller(Num).CondMassFlowRate;
                }
            }

            // Set node flow rates;  for these load based models
            // assume that sufficient evaporator flow rate is available
            ElectricEIRChiller(Num).ChillerFalseLoad = ElectricEIRChiller(Num).ChillerFalseLoadRate * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
            ElectricEIRChiller(Num).Energy = ElectricEIRChiller(Num).Power * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
            ElectricEIRChiller(Num).EvapEnergy = ElectricEIRChiller(Num).QEvaporator * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
            ElectricEIRChiller(Num).CondEnergy = ElectricEIRChiller(Num).QCondenser * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
            ElectricEIRChiller(Num).EvapInletTemp = DataLoopNode::Node(ElectricEIRChiller(Num).EvapInletNodeNum).Temp;
            ElectricEIRChiller(Num).CondInletTemp = DataLoopNode::Node(ElectricEIRChiller(Num).CondInletNodeNum).Temp;
            ElectricEIRChiller(Num).CondOutletTemp = DataLoopNode::Node(ElectricEIRChiller(Num).CondOutletNodeNum).Temp;
            ElectricEIRChiller(Num).EvapOutletTemp = DataLoopNode::Node(EvapOutletNode).Temp;
            ElectricEIRChiller(Num).CondenserFanEnergyConsumption = ElectricEIRChiller(Num).CondenserFanPower * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
            if (ElectricEIRChiller(Num).Power != 0.0) {
                ElectricEIRChiller(Num).ActualCOP = (ElectricEIRChiller(Num).QEvaporator + ElectricEIRChiller(Num).ChillerFalseLoadRate) / ElectricEIRChiller(Num).Power;
            } else {
                ElectricEIRChiller(Num).ActualCOP = 0.0;
            }
            if (ElectricEIRChiller(Num).CondenserType == EvapCooled) {
                ElectricEIRChiller(Num).BasinHeaterConsumption = ElectricEIRChiller(Num).BasinHeaterPower * ReportingConstant;
                ElectricEIRChiller(Num).EvapWaterConsump = ElectricEIRChiller(Num).EvapWaterConsumpRate * ReportingConstant;
            }

            if (ElectricEIRChiller(Num).HeatRecActive) {

                PlantUtilities::SafeCopyPlantNode(HeatRecInNode, HeatRecOutNode);
                ElectricEIRChiller(Num).EnergyHeatRecovery = ElectricEIRChiller(Num).QHeatRecovered * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
                DataLoopNode::Node(HeatRecOutNode).Temp = ElectricEIRChiller(Num).HeatRecOutletTemp;
                ElectricEIRChiller(Num).HeatRecInletTemp = DataLoopNode::Node(HeatRecInNode).Temp;
                ElectricEIRChiller(Num).HeatRecMassFlow = DataLoopNode::Node(HeatRecInNode).MassFlowRate;
            }
        }
    }

} // namespace ChillerElectricEIR

} // namespace EnergyPlus
