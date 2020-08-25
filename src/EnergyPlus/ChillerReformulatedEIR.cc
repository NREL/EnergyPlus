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

// EnergyPlus Headers
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/ChillerReformulatedEIR.hh>
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
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/IOFiles.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/Plant/PlantLocation.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ReportSizingManager.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/StandardRatings.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

namespace ChillerReformulatedEIR {

    // The Electric EIR and Reformulated EIR chiller models are similar.
    // They only differ in the independent variable used to evaluate the performance curves.
    // Since the Reformulated EIR chiller uses outlet condenser water temperature as an
    // independent variable, iteration is required to converge on a solution.

    // MODULE INFORMATION:
    //       AUTHOR         Lixing Gu
    //       DATE WRITTEN   August 2006
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    //       MODIFIED
    //			Aug.  2014, Rongpeng Zhang, added An additional part-load performance curve type

    // PURPOSE OF THIS MODULE:
    //  This module simulates the performance of the electric vapor compression
    //  chiller using a reformulated model based on the DOE-2 EIR chiller.

    // METHODOLOGY EMPLOYED:
    //  Once the PlantLoopManager determines that the Reformulated EIR chiller
    //  is available to meet a loop cooling demand, it calls SimReformulatedEIRChiller
    //  which in turn calls the reformulated EIR chiller model.
    //  The ReformulatedEIR chiller model is based on polynomial fits of chiller
    //  performance data.

    // REFERENCES:
    // 1. Hydeman, M., P. Sreedharan, N. Webb, and S. Blanc. 2002. "Development and Testing of a Reformulated
    //    Regression-Based Electric Chiller Model". ASHRAE Transactions, HI-02-18-2, Vol 108, Part 2, pp. 1118-1127.

    PlantComponent *ReformulatedEIRChillerSpecs::factory(ChillerReformulatedEIRData &chillers, std::string const &objectName)
    {
        // Process the input data if it hasn't been done already
        if (chillers.GetInputREIR) {
            GetElecReformEIRChillerInput(chillers);
            chillers.GetInputREIR = false;
        }
        // Now look for this particular object in the list
        for (auto &obj : chillers.ElecReformEIRChiller) {
            if (obj.Name == objectName) {
                return &obj;
            }
        }
        // If we didn't find it, fatal
        ShowFatalError("LocalReformulatedElectEIRChillerFactory: Error getting inputs for object named: " + objectName); // LCOV_EXCL_LINE
        // Shut up the compiler
        return nullptr; // LCOV_EXCL_LINE
    }

    void ReformulatedEIRChillerSpecs::getDesignCapacities(const PlantLocation &calledFromLocation, Real64 &MaxLoad, Real64 &MinLoad, Real64 &OptLoad)
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

    void ReformulatedEIRChillerSpecs::getDesignTemperatures(Real64 &TempDesCondIn, Real64 &TempDesEvapOut)
    {
        TempDesEvapOut = this->TempRefEvapOut;
        TempDesCondIn = this->TempRefCondIn;
    }

    void ReformulatedEIRChillerSpecs::getSizingFactor(Real64 &sizFac)
    {
        sizFac = this->SizFac;
    }

    void ReformulatedEIRChillerSpecs::onInitLoopEquip(EnergyPlusData &state, const PlantLocation &calledFromLocation)
    {
        bool runFlag = true;
        Real64 myLoad = 0.0;
        this->initialize(state.dataBranchInputManager, runFlag, myLoad);

        if (calledFromLocation.loopNum == this->CWLoopNum) {
            this->size(state.files);
        }
    }

    void ReformulatedEIRChillerSpecs::simulate(EnergyPlusData &state, const PlantLocation &calledFromLocation, bool FirstHVACIteration, Real64 &CurLoad, bool RunFlag)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   July 2004
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        //  This is the reformulated EIR chiller model driver. It gets the input for the
        //  models, initializes simulation variables, calls the appropriate model and sets
        //  up reporting variables.

        if (calledFromLocation.loopNum == this->CWLoopNum) {
            this->initialize(state.dataBranchInputManager, RunFlag, CurLoad);
            this->control(CurLoad, RunFlag, FirstHVACIteration);
            this->update(CurLoad, RunFlag);
        } else if (calledFromLocation.loopNum == this->CDLoopNum) {
            int LoopSide = this->CDLoopSideNum;
            PlantUtilities::UpdateChillerComponentCondenserSide(calledFromLocation.loopNum,
                                                                LoopSide,
                                                                DataPlant::TypeOf_Chiller_ElectricReformEIR,
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
                                                            DataPlant::TypeOf_Chiller_ElectricReformEIR,
                                                            this->HeatRecInletNodeNum,
                                                            this->HeatRecOutletNodeNum,
                                                            this->QHeatRecovery,
                                                            this->HeatRecInletTemp,
                                                            this->HeatRecOutletTemp,
                                                            this->HeatRecMassFlow,
                                                            FirstHVACIteration);
        }
    }

    void GetElecReformEIRChillerInput(ChillerReformulatedEIRData &chillers)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Lixing Gu, FSEC
        //       DATE WRITTEN:    July 2006

        //       MODIFIED
        //			Aug.  2014, Rongpeng Zhang, added an additional part-load performance curve type

        // PURPOSE OF THIS SUBROUTINE:
        //  This routine will get the input required by the Reformulated Electric EIR Chiller model

        static std::string const RoutineName("GetElecReformEIRChillerInput: "); // include trailing blank space

        bool ErrorsFound(false); // True when input errors found

        DataIPShortCuts::cCurrentModuleObject = "Chiller:Electric:ReformulatedEIR";
        chillers.NumElecReformEIRChillers = inputProcessor->getNumObjectsFound(DataIPShortCuts::cCurrentModuleObject);

        if (chillers.NumElecReformEIRChillers <= 0) {
            ShowSevereError("No " + DataIPShortCuts::cCurrentModuleObject + " equipment specified in input file");
            ErrorsFound = true;
        }

        // ALLOCATE ARRAYS
        chillers.ElecReformEIRChiller.allocate(chillers.NumElecReformEIRChillers);

        // Load arrays with reformulated electric EIR chiller data
        for (int EIRChillerNum = 1; EIRChillerNum <= chillers.NumElecReformEIRChillers; ++EIRChillerNum) {
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

            auto &thisChiller = chillers.ElecReformEIRChiller(EIRChillerNum);
            thisChiller.Name = DataIPShortCuts::cAlphaArgs(1);
            // Performance curves
            thisChiller.ChillerCapFTIndex = CurveManager::GetCurveIndex(DataIPShortCuts::cAlphaArgs(2));
            thisChiller.CAPFTName = DataIPShortCuts::cAlphaArgs(2);
            if (thisChiller.ChillerCapFTIndex == 0) {
                ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\"");
                ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(2) + '=' + DataIPShortCuts::cAlphaArgs(2));
                ErrorsFound = true;
            }

            thisChiller.ChillerEIRFTIndex = CurveManager::GetCurveIndex(DataIPShortCuts::cAlphaArgs(3));
            thisChiller.EIRFTName = DataIPShortCuts::cAlphaArgs(3);
            if (thisChiller.ChillerEIRFTIndex == 0) {
                ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\"");
                ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(3) + '=' + DataIPShortCuts::cAlphaArgs(3));
                ErrorsFound = true;
            }

            // The default type of part-load curve is: LeavingCondenserWaterTemperature
            std::string PartLoadCurveType; // Part load curve type
            if (DataIPShortCuts::lAlphaFieldBlanks(4)) {
                PartLoadCurveType = "LeavingCondenserWaterTemperature";
            } else {
                PartLoadCurveType = DataIPShortCuts::cAlphaArgs(4);
            }

            thisChiller.EIRFPLRName = DataIPShortCuts::cAlphaArgs(5);
            thisChiller.ChillerEIRFPLRIndex = CurveManager::GetCurveIndex(DataIPShortCuts::cAlphaArgs(5));
            if (thisChiller.ChillerEIRFPLRIndex == 0) {
                ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\"");
                ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(5) + '=' + DataIPShortCuts::cAlphaArgs(5));
                ErrorsFound = true;
            }

            // Check the type of part-load curves implemented: 1_LeavingCondenserWaterTemperature, 2_Lift
            if (UtilityRoutines::SameString(PartLoadCurveType, "LeavingCondenserWaterTemperature") &&
                CurveManager::PerfCurve(thisChiller.ChillerEIRFPLRIndex).NumDims == 2) {
                thisChiller.PartLoadCurveType = PLR::LeavingCondenserWaterTemperature;
            } else if (UtilityRoutines::SameString(PartLoadCurveType, "Lift") &&
                       CurveManager::PerfCurve(thisChiller.ChillerEIRFPLRIndex).NumDims == 3) {
                thisChiller.PartLoadCurveType = PLR::Lift;
            } else {
                ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\"");
                ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(5) + '=' + DataIPShortCuts::cAlphaArgs(5) + " for " +
                                  DataIPShortCuts::cAlphaFieldNames(4) + '=' + DataIPShortCuts::cAlphaArgs(4));
                ErrorsFound = true;
            }

            // Chilled water inlet/outlet node names are necessary
            if (DataIPShortCuts::lAlphaFieldBlanks(6)) {
                ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\"");
                ShowContinueError(DataIPShortCuts::cAlphaFieldNames(6) + " is blank.");
                ErrorsFound = true;
            }
            if (DataIPShortCuts::lAlphaFieldBlanks(7)) {
                ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\"");
                ShowContinueError(DataIPShortCuts::cAlphaFieldNames(7) + " is blank.");
                ErrorsFound = true;
            }

            thisChiller.EvapInletNodeNum = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(6),
                                                                                                       ErrorsFound,
                                                                                                       DataIPShortCuts::cCurrentModuleObject,
                                                                                                       DataIPShortCuts::cAlphaArgs(1),
                                                                                                       DataLoopNode::NodeType_Water,
                                                                                                       DataLoopNode::NodeConnectionType_Inlet,
                                                                                                       1,
                                                                                                       DataLoopNode::ObjectIsNotParent);
            thisChiller.EvapOutletNodeNum = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(7),
                                                                                                        ErrorsFound,
                                                                                                        DataIPShortCuts::cCurrentModuleObject,
                                                                                                        DataIPShortCuts::cAlphaArgs(1),
                                                                                                        DataLoopNode::NodeType_Water,
                                                                                                        DataLoopNode::NodeConnectionType_Outlet,
                                                                                                        1,
                                                                                                        DataLoopNode::ObjectIsNotParent);
            BranchNodeConnections::TestCompSet(DataIPShortCuts::cCurrentModuleObject,
                                               DataIPShortCuts::cAlphaArgs(1),
                                               DataIPShortCuts::cAlphaArgs(6),
                                               DataIPShortCuts::cAlphaArgs(7),
                                               "Chilled Water Nodes");

            thisChiller.CondenserType = DataPlant::CondenserType::WATERCOOLED;

            // Condenser inlet/outlet node names are necessary
            if (DataIPShortCuts::lAlphaFieldBlanks(8)) {
                ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\"");
                ShowContinueError(DataIPShortCuts::cAlphaFieldNames(8) + " is blank.");
                ErrorsFound = true;
            }
            if (DataIPShortCuts::lAlphaFieldBlanks(9)) {
                ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\"");
                ShowContinueError(DataIPShortCuts::cAlphaFieldNames(9) + " is blank.");
                ErrorsFound = true;
            }

            thisChiller.CondInletNodeNum = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(8),
                                                                                                       ErrorsFound,
                                                                                                       DataIPShortCuts::cCurrentModuleObject,
                                                                                                       DataIPShortCuts::cAlphaArgs(1),
                                                                                                       DataLoopNode::NodeType_Water,
                                                                                                       DataLoopNode::NodeConnectionType_Inlet,
                                                                                                       2,
                                                                                                       DataLoopNode::ObjectIsNotParent);
            thisChiller.CondOutletNodeNum = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(9),
                                                                                                        ErrorsFound,
                                                                                                        DataIPShortCuts::cCurrentModuleObject,
                                                                                                        DataIPShortCuts::cAlphaArgs(1),
                                                                                                        DataLoopNode::NodeType_Water,
                                                                                                        DataLoopNode::NodeConnectionType_Outlet,
                                                                                                        2,
                                                                                                        DataLoopNode::ObjectIsNotParent);

            BranchNodeConnections::TestCompSet(DataIPShortCuts::cCurrentModuleObject,
                                               DataIPShortCuts::cAlphaArgs(1),
                                               DataIPShortCuts::cAlphaArgs(8),
                                               DataIPShortCuts::cAlphaArgs(9),
                                               "Condenser Water Nodes");

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
            thisChiller.TempRefCondOut = DataIPShortCuts::rNumericArgs(4);
            if (thisChiller.TempRefEvapOut >= thisChiller.TempRefCondOut) {
                ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\"");
                ShowContinueError(DataIPShortCuts::cNumericFieldNames(3) + " [" + General::RoundSigDigits(DataIPShortCuts::rNumericArgs(3), 2) +
                                  "] >= " + DataIPShortCuts::cNumericFieldNames(4) + " [" +
                                  General::RoundSigDigits(DataIPShortCuts::rNumericArgs(4), 2) + ']');
                ShowContinueError("Reference Leaving Chilled Water Temperature must be less than Reference Leaving Condenser Water Temperature ");
                ErrorsFound = true;
            }

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
            thisChiller.SizFac = DataIPShortCuts::rNumericArgs(14);
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

            thisChiller.CompPowerToCondenserFrac = DataIPShortCuts::rNumericArgs(11);

            if (thisChiller.CompPowerToCondenserFrac < 0.0 ||
                thisChiller.CompPowerToCondenserFrac > 1.0) {
                ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\"");
                ShowContinueError(DataIPShortCuts::cNumericFieldNames(11) + " = " + General::RoundSigDigits(DataIPShortCuts::rNumericArgs(11), 3));
                ShowContinueError(DataIPShortCuts::cNumericFieldNames(11) + " must be greater than or equal to zero");
                ShowContinueError(DataIPShortCuts::cNumericFieldNames(11) + " must be less than or equal to one");
                ErrorsFound = true;
            }

            thisChiller.TempLowLimitEvapOut = DataIPShortCuts::rNumericArgs(12);

            // These are the optional heat recovery inputs
            thisChiller.DesignHeatRecVolFlowRate = DataIPShortCuts::rNumericArgs(13);
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
                thisChiller.HeatRecOutletNodeNum =
                    NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(12),
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

                if (thisChiller.DesignHeatRecVolFlowRate > 0.0) {
                    PlantUtilities::RegisterPlantCompDesignFlow(thisChiller.HeatRecInletNodeNum,
                                                                thisChiller.DesignHeatRecVolFlowRate);
                }
                if (NumNums > 14) {
                    if (!DataIPShortCuts::lNumericFieldBlanks(15)) {
                        thisChiller.HeatRecCapacityFraction = DataIPShortCuts::rNumericArgs(15);
                    } else {
                        thisChiller.HeatRecCapacityFraction = 1.0;
                    }
                } else {
                    thisChiller.HeatRecCapacityFraction = 1.0;
                }

                if (NumAlphas > 12) {
                    if (!DataIPShortCuts::lAlphaFieldBlanks(13)) {
                        thisChiller.HeatRecInletLimitSchedNum =
                            ScheduleManager::GetScheduleIndex(DataIPShortCuts::cAlphaArgs(13));
                        if (thisChiller.HeatRecInletLimitSchedNum == 0) {
                            ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\"");
                            ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(13) + '=' + DataIPShortCuts::cAlphaArgs(13));
                            ErrorsFound = true;
                        }
                    } else {
                        thisChiller.HeatRecInletLimitSchedNum = 0;
                    }
                } else {
                    thisChiller.HeatRecInletLimitSchedNum = 0;
                }

                if (NumAlphas > 13) {
                    if (!DataIPShortCuts::lAlphaFieldBlanks(14)) {
                        thisChiller.HeatRecSetPointNodeNum =
                            NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(14),
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
                if ((!DataIPShortCuts::lAlphaFieldBlanks(11)) || (!DataIPShortCuts::lAlphaFieldBlanks(12))) {
                    ShowWarningError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\"");
                    ShowWarningError("Since Reference Heat Reclaim Volume Flow Rate = 0.0, heat recovery is inactive.");
                    ShowContinueError("However, node names were specified for heat recovery inlet or outlet nodes.");
                }
            }

            if (NumAlphas > 14) {
                thisChiller.EndUseSubcategory = DataIPShortCuts::cAlphaArgs(15);
            } else {
                thisChiller.EndUseSubcategory = "General";
            }
        }

        if (ErrorsFound) {
            ShowFatalError("Errors found in processing input for " + DataIPShortCuts::cCurrentModuleObject);
        }
    }

    void ReformulatedEIRChillerSpecs::setupOutputVars()
    {
        SetupOutputVariable("Chiller Part Load Ratio", OutputProcessor::Unit::None, this->ChillerPartLoadRatio, "System", "Average", this->Name);

        SetupOutputVariable("Chiller Cycling Ratio", OutputProcessor::Unit::None, this->ChillerCyclingRatio, "System", "Average", this->Name);

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

        SetupOutputVariable("Chiller Condenser Inlet Temperature", OutputProcessor::Unit::C, this->CondInletTemp, "System", "Average", this->Name);

        SetupOutputVariable("Chiller Condenser Outlet Temperature", OutputProcessor::Unit::C, this->CondOutletTemp, "System", "Average", this->Name);

        SetupOutputVariable("Chiller Condenser Mass Flow Rate", OutputProcessor::Unit::kg_s, this->CondMassFlowRate, "System", "Average", this->Name);

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

            SetupOutputVariable(
                "Chiller Heat Recovery Inlet Temperature", OutputProcessor::Unit::C, this->HeatRecInletTemp, "System", "Average", this->Name);

            SetupOutputVariable(
                "Chiller Heat Recovery Outlet Temperature", OutputProcessor::Unit::C, this->HeatRecOutletTemp, "System", "Average", this->Name);

            SetupOutputVariable(
                "Chiller Heat Recovery Mass Flow Rate", OutputProcessor::Unit::kg_s, this->HeatRecMassFlow, "System", "Average", this->Name);

            SetupOutputVariable(
                "Chiller Effective Heat Rejection Temperature", OutputProcessor::Unit::C, this->ChillerCondAvgTemp, "System", "Average", this->Name);
        }

        if (DataGlobals::AnyEnergyManagementSystemInModel) {
            SetupEMSInternalVariable("Chiller Nominal Capacity", this->Name, "[W]", this->RefCap);
        }
    }

    void ReformulatedEIRChillerSpecs::initialize(BranchInputManagerData &dataBranchInputManager, bool const RunFlag, Real64 const MyLoad)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Lixing Gu, FSEC
        //       DATE WRITTEN   July 2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        //  This subroutine is for initializations of the Reformulated Electric EIR Chiller variables

        // METHODOLOGY EMPLOYED:
        //  Uses the status flags to trigger initializations.

        static std::string const RoutineName("InitElecReformEIRChiller");

        // Init more variables
        if (this->MyInitFlag) {

            this->setupOutputVars();

            // Locate the chillers on the plant loops for later usage
            bool errFlag = false;
            PlantUtilities::ScanPlantLoopsForObject(dataBranchInputManager,
                                                    this->Name,
                                                    DataPlant::TypeOf_Chiller_ElectricReformEIR,
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
            if (this->CondenserType != DataPlant::CondenserType::AIRCOOLED) {
                PlantUtilities::ScanPlantLoopsForObject(dataBranchInputManager,
                                                        this->Name,
                                                        DataPlant::TypeOf_Chiller_ElectricReformEIR,
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
                    this->CWLoopNum, this->CWLoopSideNum, this->CDLoopNum, this->CDLoopSideNum, DataPlant::TypeOf_Chiller_ElectricReformEIR, true);
            }
            if (this->HeatRecActive) {
                PlantUtilities::ScanPlantLoopsForObject(dataBranchInputManager,
                                                        this->Name,
                                                        DataPlant::TypeOf_Chiller_ElectricReformEIR,
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
                    this->CWLoopNum, this->CWLoopSideNum, this->HRLoopNum, this->HRLoopSideNum, DataPlant::TypeOf_Chiller_ElectricReformEIR, true);
            }

            if ((this->CondenserType != DataPlant::CondenserType::AIRCOOLED) && (this->HeatRecActive)) {
                PlantUtilities::InterConnectTwoPlantLoopSides(
                    this->CDLoopNum, this->CDLoopSideNum, this->HRLoopNum, this->HRLoopSideNum, DataPlant::TypeOf_Chiller_ElectricReformEIR, false);
            }

            if (errFlag) {
                ShowFatalError("InitElecReformEIRChiller: Program terminated due to previous condition(s).");
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
            this->MyInitFlag = false;
        }

        this->EquipFlowCtrl =
            DataPlant::PlantLoop(this->CWLoopNum).LoopSide(this->CWLoopSideNum).Branch(this->CWBranchNum).Comp(this->CWCompNum).FlowCtrl;

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
                DataLoopNode::Node(this->CondInletNodeNum).MassFlowRate =
                    this->CondVolFlowRate * Psychrometrics::PsyRhoAirFnPbTdbW(DataEnvironment::StdBaroPress, this->TempRefCondIn, 0.0, RoutineName);
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
                // overall capacity limit
                this->HeatRecMaxCapacityLimit = this->HeatRecCapacityFraction * (this->RefCap + this->RefCap / this->RefCOP);
            }

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

            // check if inlet limit active and if exceeded.
            bool HeatRecRunFlag;
            if (this->HeatRecInletLimitSchedNum > 0) {
                Real64 HeatRecHighInletLimit = ScheduleManager::GetCurrentScheduleValue(this->HeatRecInletLimitSchedNum);
                if (DataLoopNode::Node(this->HeatRecInletNodeNum).Temp > HeatRecHighInletLimit) { // shut down heat recovery
                    HeatRecRunFlag = false;
                } else {
                    HeatRecRunFlag = RunFlag;
                }
            } else {
                HeatRecRunFlag = RunFlag;
            }

            if (HeatRecRunFlag) {
                mdot = this->DesignHeatRecMassFlowRate;
            } else {
                mdot = 0.0;
            }

            PlantUtilities::SetComponentFlowRate(
                mdot, this->HeatRecInletNodeNum, this->HeatRecOutletNodeNum, LoopNum, LoopSideNum, BranchIndex, CompIndex);
        }
    }

    void ReformulatedEIRChillerSpecs::size(IOFiles &ioFiles)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   June 2004
        //       MODIFIED       July 2006, L. Gu, modified for reformulated EIR chiller
        //                      November 2013 Daeho Kang, add component sizing table entries
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        //  This subroutine is for sizing Reformulated Electric EIR Chiller Components for which capacities and flow rates
        //  have not been specified in the input.

        // METHODOLOGY EMPLOYED:
        //  Obtains evaporator flow rate from the plant sizing array. Calculates reference capacity from
        //  the evaporator flow rate and the chilled water loop design delta T. The condenser flow rate
        //  is calculated from the reference capacity, the COP, and the condenser loop design delta T.

        static std::string const RoutineName("SizeElecReformEIRChiller");

        bool ErrorsFound(false); // If errors detected in input

        Real64 tmpNomCap = this->RefCap;
        Real64 tmpEvapVolFlowRate = this->EvapVolFlowRate;
        Real64 tmpCondVolFlowRate = this->CondVolFlowRate;

        int PltSizCondNum(0); // Plant Sizing index for condenser loop
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
                        ReportSizingManager::ReportSizingOutput("Chiller:Electric:ReformulatedEIR",
                                                                this->Name,
                                                                "Design Size Reference Chilled Water Flow Rate [m3/s]",
                                                                tmpEvapVolFlowRate);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput("Chiller:Electric:ReformulatedEIR",
                                                                this->Name,
                                                                "Initial Design Size Reference Chilled Water Flow Rate [m3/s]",
                                                                tmpEvapVolFlowRate);
                    }
                } else { // Hard-size with sizing data
                    if (this->EvapVolFlowRate > 0.0 && tmpEvapVolFlowRate > 0.0) {
                        Real64 EvapVolFlowRateUser = this->EvapVolFlowRate;
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput("Chiller:Electric:ReformulatedEIR",
                                                                    this->Name,
                                                                    "Design Size Reference Chilled Water Flow Rate [m3/s]",
                                                                    tmpEvapVolFlowRate,
                                                                    "User-Specified Reference Chilled Water Flow Rate [m3/s]",
                                                                    EvapVolFlowRateUser);
                            if (DataGlobals::DisplayExtraWarnings) {
                                if ((std::abs(tmpEvapVolFlowRate - EvapVolFlowRateUser) / EvapVolFlowRateUser) >
                                    DataSizing::AutoVsHardSizingThreshold) {
                                    ShowMessage("SizeChillerElectricReformulatedEIR: Potential issue with equipment sizing for " + this->Name);
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
                ShowSevereError("Autosizing of Reformulated Electric Chiller evap flow rate requires a loop Sizing:Plant object");
                ShowContinueError("Occurs in Reformulated Electric Chiller object=" + this->Name);
                ErrorsFound = true;
            }
            if (!this->EvapVolFlowRateWasAutoSized && DataPlant::PlantFinalSizesOkayToReport &&
                (this->EvapVolFlowRate > 0.0)) { // Hard-size with sizing data
                ReportSizingManager::ReportSizingOutput(
                    "Chiller:Electric:ReformulatedEIR", this->Name, "User-Specified Reference Chilled Water Flow Rate [m3/s]", this->EvapVolFlowRate);
            }
        }

        PlantUtilities::RegisterPlantCompDesignFlow(this->EvapInletNodeNum, tmpEvapVolFlowRate);

        if (PltSizNum > 0) {
            if (DataSizing::PlantSizData(PltSizNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                Real64 SizingEvapOutletTemp; // Plant Sizing outlet temperature for CurLoopNum [C]
                Real64 SizingCondOutletTemp; // Plant Sizing outlet temperature for condenser loop [C]
                if (PltSizCondNum > 0 && PltSizNum > 0) {
                    SizingEvapOutletTemp = DataSizing::PlantSizData(PltSizNum).ExitTemp;
                    SizingCondOutletTemp = DataSizing::PlantSizData(PltSizCondNum).ExitTemp + DataSizing::PlantSizData(PltSizCondNum).DeltaT;
                } else {
                    SizingEvapOutletTemp = this->TempRefEvapOut;
                    SizingCondOutletTemp = this->TempRefCondOut;
                }
                Real64 Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(this->CWLoopNum).FluidName,
                                                                   DataGlobals::CWInitConvTemp,
                                                                   DataPlant::PlantLoop(this->CWLoopNum).FluidIndex,
                                                                   RoutineName);
                Real64 rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->CWLoopNum).FluidName,
                                                               DataGlobals::CWInitConvTemp,
                                                               DataPlant::PlantLoop(this->CWLoopNum).FluidIndex,
                                                               RoutineName);
                Real64 RefCapFT = CurveManager::CurveValue(this->ChillerCapFTIndex, SizingEvapOutletTemp, SizingCondOutletTemp);
                tmpNomCap = (Cp * rho * DataSizing::PlantSizData(PltSizNum).DeltaT * tmpEvapVolFlowRate) / RefCapFT;
            } else {
                if (this->RefCapWasAutoSized) tmpNomCap = 0.0;
            }
            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                if (this->RefCapWasAutoSized) {
                    this->RefCap = tmpNomCap;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(
                            "Chiller:Electric:ReformulatedEIR", this->Name, "Design Size Reference Capacity [W]", tmpNomCap);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(
                            "Chiller:Electric:ReformulatedEIR", this->Name, "Initial Design Size Reference Capacity [W]", tmpNomCap);
                    }
                } else {
                    if (this->RefCap > 0.0 && tmpNomCap > 0.0) {
                        Real64 RefCapUser = this->RefCap;
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput("Chiller:Electric:ReformulatedEIR",
                                                                    this->Name,
                                                                    "Design Size Reference Capacity [W]",
                                                                    tmpNomCap,
                                                                    "User-Specified Reference Capacity [W]",
                                                                    RefCapUser);
                            if (DataGlobals::DisplayExtraWarnings) {
                                if ((std::abs(tmpNomCap - RefCapUser) / RefCapUser) > DataSizing::AutoVsHardSizingThreshold) {
                                    ShowMessage("Size:ChillerElectricReformulatedEIR: Potential issue with equipment sizing for " + this->Name);
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
                ShowSevereError("Autosizing of Reformulated Electric Chiller reference capacity requires a loop Sizing:Plant object");
                ShowContinueError("Occurs in Reformulated Electric Chiller object=" + this->Name);
                ErrorsFound = true;
            }
            if (!this->RefCapWasAutoSized && DataPlant::PlantFinalSizesOkayToReport && (this->RefCap > 0.0)) {
                ReportSizingManager::ReportSizingOutput(
                    "Chiller:Electric:ReformulatedEIR", this->Name, "User-Specified Reference Capacity [W]", this->RefCap);
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
                // IF (DataPlant::PlantFirstSizesOkayToFinalize) ElecReformEIRChiller(EIRChillNum)%CondVolFlowRate = tmpCondVolFlowRate
            } else {
                if (this->CondVolFlowRateWasAutoSized) tmpCondVolFlowRate = 0.0;
                // IF (DataPlant::PlantFirstSizesOkayToFinalize) ElecReformEIRChiller(EIRChillNum)%CondVolFlowRate = tmpCondVolFlowRate
            }
            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                if (this->CondVolFlowRateWasAutoSized) {
                    this->CondVolFlowRate = tmpCondVolFlowRate;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput("Chiller:Electric:ReformulatedEIR",
                                                                this->Name,
                                                                "Design Size Reference Condenser Water Flow Rate [m3/s]",
                                                                tmpCondVolFlowRate);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput("Chiller:Electric:ReformulatedEIR",
                                                                this->Name,
                                                                "Initial Design Size Reference Condenser Water Flow Rate [m3/s]",
                                                                tmpCondVolFlowRate);
                    }
                } else {
                    if (this->CondVolFlowRate > 0.0 && tmpCondVolFlowRate > 0.0) {
                        Real64 CondVolFlowRateUser = this->CondVolFlowRate;
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput("Chiller:Electric:ReformulatedEIR",
                                                                    this->Name,
                                                                    "Design Size Reference Condenser Water Flow Rate [m3/s]",
                                                                    tmpCondVolFlowRate,
                                                                    "User-Specified Reference Condenser Water Flow Rate [m3/s]",
                                                                    CondVolFlowRateUser);
                            if (DataGlobals::DisplayExtraWarnings) {
                                if ((std::abs(tmpCondVolFlowRate - CondVolFlowRateUser) / CondVolFlowRateUser) >
                                    DataSizing::AutoVsHardSizingThreshold) {
                                    ShowMessage("Size:ChillerElectricReformulatedEIR: Potential issue with equipment sizing for " + this->Name);
                                    ShowContinueError("User-Specified Reference Condenser Water Flow Rate of " +
                                                      General::RoundSigDigits(CondVolFlowRateUser, 5) + " [m3/s]");
                                    ShowContinueError("differs from Design Size Reference Condenser Water Flow Rate of " +
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
                ShowSevereError("Autosizing of Reformulated Electric EIR Chiller condenser flow rate requires a condenser");
                ShowContinueError("loop Sizing:Plant object");
                ShowContinueError("Occurs in Reformulated Electric EIR Chiller object=" + this->Name);
                ErrorsFound = true;
            }
            if (!this->CondVolFlowRateWasAutoSized && DataPlant::PlantFinalSizesOkayToReport && (this->CondVolFlowRate > 0.0)) {
                ReportSizingManager::ReportSizingOutput("Chiller:Electric:ReformulatedEIR",
                                                        this->Name,
                                                        "User-Specified Reference Condenser Water Flow Rate [m3/s]",
                                                        this->CondVolFlowRate);
            }
        }

        // save the reference condenser water volumetric flow rate for use by the condenser water loop sizing algorithms
        PlantUtilities::RegisterPlantCompDesignFlow(this->CondInletNodeNum, tmpCondVolFlowRate);

        if (this->HeatRecActive) {
            Real64 tmpHeatRecVolFlowRate = tmpCondVolFlowRate * this->HeatRecCapacityFraction;
            if (!this->DesignHeatRecVolFlowRateWasAutoSized) tmpHeatRecVolFlowRate = this->DesignHeatRecVolFlowRate;
            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                if (this->DesignHeatRecVolFlowRateWasAutoSized) {
                    this->DesignHeatRecVolFlowRate = tmpHeatRecVolFlowRate;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput("Chiller:Electric:ReformulatedEIR",
                                                                this->Name,
                                                                "Design Size Design Heat Recovery Fluid Flow Rate [m3/s]",
                                                                tmpHeatRecVolFlowRate);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput("Chiller:Electric:ReformulatedEIR",
                                                                this->Name,
                                                                "Initial Design Size Design Heat Recovery Fluid Flow Rate [m3/s]",
                                                                tmpHeatRecVolFlowRate);
                    }
                } else {
                    if (this->DesignHeatRecVolFlowRate > 0.0 && tmpHeatRecVolFlowRate > 0.0) {
                        Real64 DesignHeatRecVolFlowRateUser = this->DesignHeatRecVolFlowRate;
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput("Chiller:Electric:ReformulatedEIR",
                                                                    this->Name,
                                                                    "Design Size Design Heat Recovery Fluid Flow Rate [m3/s]",
                                                                    tmpHeatRecVolFlowRate,
                                                                    "User-Specified Design Heat Recovery Fluid Flow Rate [m3/s]",
                                                                    DesignHeatRecVolFlowRateUser);
                            if (DataGlobals::DisplayExtraWarnings) {
                                if ((std::abs(tmpHeatRecVolFlowRate - DesignHeatRecVolFlowRateUser) / DesignHeatRecVolFlowRateUser) >
                                    DataSizing::AutoVsHardSizingThreshold) {
                                    ShowMessage("Size:ChillerElectricReformulatedEIR: Potential issue with equipment sizing for " + this->Name);
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
            // save the reference heat recovery fluid volumetric flow rate
            PlantUtilities::RegisterPlantCompDesignFlow(this->HeatRecInletNodeNum, tmpHeatRecVolFlowRate);
        }

        std::string equipName; // Name of chiller
        if (DataPlant::PlantFinalSizesOkayToReport) {
            if (this->MySizeFlag) {
                Real64 IPLV;
                StandardRatings::CalcChillerIPLV(ioFiles,
                                                 this->Name,
                                                 DataPlant::TypeOf_Chiller_ElectricReformEIR,
                                                 this->RefCap,
                                                 this->RefCOP,
                                                 this->CondenserType,
                                                 this->ChillerCapFTIndex,
                                                 this->ChillerEIRFTIndex,
                                                 this->ChillerEIRFPLRIndex,
                                                 this->MinUnloadRat,
                                                 IPLV,
                                                 this->EvapVolFlowRate,
                                                 this->CDLoopNum,
                                                 this->CompPowerToCondenserFrac);
                this->MySizeFlag = false;
            }
            // create predefined report
            equipName = this->Name;
            OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchMechType, equipName, "Chiller:Electric:ReformulatedEIR");
            OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchMechNomEff, equipName, this->RefCOP);
            OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchMechNomCap, equipName, this->RefCap);
        }

        // Only check performance curves if Capacity and volumetric flow rate are greater than 0
        if (this->RefCap > 0.0 && this->CondVolFlowRate > 0.0) {
            //   Check the CAP-FT, EIR-FT, and PLR curves at reference conditions and warn user if different from 1.0 by more than +-10%
            if (this->ChillerCapFTIndex > 0) {
                Real64 CurveVal = CurveManager::CurveValue(this->ChillerCapFTIndex, this->TempRefEvapOut, this->TempRefCondOut);
                if (CurveVal > 1.10 || CurveVal < 0.90) {
                    ShowWarningError("Capacity ratio as a function of temperature curve output is not equal to 1.0");
                    ShowContinueError("(+ or - 10%) at reference conditions for Chiller:Electric:ReformulatedEIR = " + equipName);
                    ShowContinueError("Curve output at reference conditions = " + General::TrimSigDigits(CurveVal, 3));
                }
                CurveManager::GetCurveMinMaxValues(this->ChillerCapFTIndex,
                                                   this->ChillerCAPFTXTempMin,
                                                   this->ChillerCAPFTXTempMax,
                                                   this->ChillerCAPFTYTempMin,
                                                   this->ChillerCAPFTYTempMax);
            }

            if (this->ChillerEIRFTIndex > 0) {
                Real64 CurveVal = CurveManager::CurveValue(this->ChillerEIRFTIndex, this->TempRefEvapOut, this->TempRefCondOut);
                if (CurveVal > 1.10 || CurveVal < 0.90) {
                    ShowWarningError("Energy input ratio as a function of temperature curve output is not equal to 1.0");
                    ShowContinueError("(+ or - 10%) at reference conditions for Chiller:Electric:ReformulatedEIR = " + equipName);
                    ShowContinueError("Curve output at reference conditions = " + General::TrimSigDigits(CurveVal, 3));
                }
                CurveManager::GetCurveMinMaxValues(this->ChillerEIRFTIndex,
                                                   this->ChillerEIRFTXTempMin,
                                                   this->ChillerEIRFTXTempMax,
                                                   this->ChillerEIRFTYTempMin,
                                                   this->ChillerEIRFTYTempMax);
            }

            if (this->ChillerEIRFPLRIndex > 0) {
                Real64 CurveVal(0.0); // Used to verify EIR-FT/CAP-FT curves = 1 at reference conditions
                if (this->PartLoadCurveType == PLR::LeavingCondenserWaterTemperature) {
                    CurveVal = CurveManager::CurveValue(this->ChillerEIRFPLRIndex, this->TempRefCondOut, 1.0);
                } else if (this->PartLoadCurveType == PLR::Lift) {
                    CurveVal = CurveManager::CurveValue(this->ChillerEIRFPLRIndex, 1.0, 1.0, 0.0);
                }
                if (CurveVal > 1.10 || CurveVal < 0.90) {
                    ShowWarningError("Energy input ratio as a function of part-load ratio curve output is not equal to 1.0");
                    ShowContinueError("(+ or - 10%) at reference conditions for Chiller:Electric:ReformulatedEIR = " + equipName);
                    ShowContinueError("Curve output at reference conditions = " + General::TrimSigDigits(CurveVal, 3));
                }

                if (this->PartLoadCurveType == PLR::LeavingCondenserWaterTemperature) {
                    CurveManager::GetCurveMinMaxValues(this->ChillerEIRFPLRIndex,
                                                       this->ChillerEIRFPLRTempMin,
                                                       this->ChillerEIRFPLRTempMax,
                                                       this->ChillerEIRFPLRPLRMin,
                                                       this->ChillerEIRFPLRPLRMax);
                } else if (this->PartLoadCurveType == PLR::Lift) {
                    CurveManager::GetCurveMinMaxValues(this->ChillerEIRFPLRIndex,
                                                       this->ChillerLiftNomMin,
                                                       this->ChillerLiftNomMax,
                                                       this->ChillerEIRFPLRPLRMin,
                                                       this->ChillerEIRFPLRPLRMax,
                                                       this->ChillerTdevNomMin,
                                                       this->ChillerTdevNomMax);
                }

                if (this->ChillerEIRFPLRPLRMin < 0 || this->ChillerEIRFPLRPLRMin >= this->ChillerEIRFPLRPLRMax || this->ChillerEIRFPLRPLRMin > 1) {
                    ShowSevereError("Invalid minimum value of PLR = " + General::TrimSigDigits(this->ChillerEIRFPLRPLRMin, 3) +
                                    " in bicubic curve = " + this->EIRFPLRName + " which is used");
                    ShowContinueError("by Chiller:Electric:ReformulatedEIR = " + equipName + '.');
                    ShowContinueError("The minimum value of PLR [y] must be from zero to 1, and less than the maximum value of PLR.");
                    ErrorsFound = true;
                }
                if (this->ChillerEIRFPLRPLRMax > 1.1 || this->ChillerEIRFPLRPLRMax <= this->ChillerEIRFPLRPLRMin || this->ChillerEIRFPLRPLRMax < 0) {
                    ShowSevereError("Invalid maximum value of PLR = " + General::TrimSigDigits(this->ChillerEIRFPLRPLRMax, 3) +
                                    " in bicubic curve = " + this->EIRFPLRName + " which is used");
                    ShowContinueError("by Chiller:Electric:ReformulatedEIR = " + equipName + '.');
                    ShowContinueError("The maximum value of PLR [y] must be from zero to 1.1, and greater than the minimum value of PLR.");
                    ErrorsFound = true;
                }
                //     calculate the condenser outlet temp proportional to PLR and test the EIRFPLR curve output for negative numbers.
            }

            //  Initialize condenser reference inlet temperature (not a user input)
            Real64 Density = FluidProperties::GetDensityGlycol(
                DataPlant::PlantLoop(this->CDLoopNum).FluidName, this->TempRefCondOut, DataPlant::PlantLoop(this->CDLoopNum).FluidIndex, RoutineName);

            Real64 SpecificHeat = FluidProperties::GetSpecificHeatGlycol(
                DataPlant::PlantLoop(this->CDLoopNum).FluidName, this->TempRefCondOut, DataPlant::PlantLoop(this->CDLoopNum).FluidIndex, RoutineName);
            Real64 CondenserCapacity = this->RefCap * (1.0 + (1.0 / this->RefCOP) * this->CompPowerToCondenserFrac);
            Real64 DeltaTCond = (CondenserCapacity) / (this->CondVolFlowRate * Density * SpecificHeat);
            this->TempRefCondIn = this->TempRefCondOut - DeltaTCond;

            if (this->PartLoadCurveType == PLR::LeavingCondenserWaterTemperature) {
                //     Check EIRFPLR curve output. Calculate condenser inlet temp based on reference condenser outlet temp,
                //     chiller capacity, and mass flow rate. Starting with the calculated condenser inlet temp and PLR = 0,
                //     calculate the condenser outlet temp proportional to PLR and test the EIRFPLR curve output for negative numbers.
                bool FoundNegValue = false;
                Array1D<Real64> CurveValArray(11, 0.0); // Used to evaluate EIRFPLR curve objects
                Array1D<Real64> CondTempArray(11, 0.0); // Used to evaluate EIRFPLR curve objects

                if (this->ChillerEIRFPLRIndex > 0) {
                    CondTempArray = 0.0;
                    for (int CurveCheck = 0; CurveCheck <= 10; ++CurveCheck) {
                        Real64 PLRTemp = CurveCheck / 10.0;
                        Real64 CondTemp = this->TempRefCondIn + (DeltaTCond * PLRTemp);
                        CondTemp = min(CondTemp, this->ChillerEIRFPLRTempMax);
                        CondTemp = max(CondTemp, this->ChillerEIRFPLRTempMin);
                        Real64 CurveValTmp; // Used to evaluate EIRFPLR curve objects
                        if (PLRTemp < this->ChillerEIRFPLRPLRMin) {
                            CurveValTmp = CurveManager::CurveValue(this->ChillerEIRFPLRIndex, CondTemp, this->ChillerEIRFPLRPLRMin);
                        } else {
                            CurveValTmp = CurveManager::CurveValue(this->ChillerEIRFPLRIndex, CondTemp, PLRTemp);
                        }
                        if (CurveValTmp < 0.0) FoundNegValue = true;
                        CurveValArray(CurveCheck + 1) = int(CurveValTmp * 100.0) / 100.0;
                        CondTempArray(CurveCheck + 1) = int(CondTemp * 100.0) / 100.0;
                    }
                }

                //     Output warning message if negative values are found in the EIRFPLR curve output. Results in Fatal error.
                if (FoundNegValue) {
                    ShowWarningError("Energy input to cooing output ratio function of part-load ratio curve shows negative values ");
                    ShowContinueError("for  Chiller:Electric:ReformulatedEIR = " + equipName + '.');
                    ShowContinueError(
                        "EIR as a function of PLR curve output at various part-load ratios and condenser water temperatures shown below:");
                    ShowContinueError("PLR           =    0.00   0.10   0.20   0.30   0.40   0.50   0.60   0.70   0.80   0.90   1.00");

                    ShowContinueError(format("Cond Temp(C) = {:7.2F}", fmt::join(CondTempArray, " ")));

                    ShowContinueError(format("Curve Output = {:7.2F}", fmt::join(CurveValArray, " ")));

                    ErrorsFound = true;
                }
            }
        } else { // just get curve min/max values if capacity or cond volume flow rate = 0
            CurveManager::GetCurveMinMaxValues(this->ChillerCapFTIndex,
                                               this->ChillerCAPFTXTempMin,
                                               this->ChillerCAPFTXTempMax,
                                               this->ChillerCAPFTYTempMin,
                                               this->ChillerCAPFTYTempMax);
            CurveManager::GetCurveMinMaxValues(this->ChillerEIRFTIndex,
                                               this->ChillerEIRFTXTempMin,
                                               this->ChillerEIRFTXTempMax,
                                               this->ChillerEIRFTYTempMin,
                                               this->ChillerEIRFTYTempMax);
            if (this->PartLoadCurveType == PLR::LeavingCondenserWaterTemperature) {
                CurveManager::GetCurveMinMaxValues(this->ChillerEIRFPLRIndex,
                                                   this->ChillerEIRFPLRTempMin,
                                                   this->ChillerEIRFPLRTempMax,
                                                   this->ChillerEIRFPLRPLRMin,
                                                   this->ChillerEIRFPLRPLRMax);
            } else if (this->PartLoadCurveType == PLR::Lift) {
                CurveManager::GetCurveMinMaxValues(this->ChillerEIRFPLRIndex,
                                                   this->ChillerLiftNomMin,
                                                   this->ChillerLiftNomMax,
                                                   this->ChillerEIRFPLRPLRMin,
                                                   this->ChillerEIRFPLRPLRMax,
                                                   this->ChillerTdevNomMin,
                                                   this->ChillerTdevNomMax);
            }
        }

        if (ErrorsFound) {
            ShowFatalError("Preceding sizing errors cause program termination");
        }
    }

    void ReformulatedEIRChillerSpecs::control(Real64 &MyLoad, bool const RunFlag, bool const FirstIteration)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Lixing Gu, FSEC
        //       DATE WRITTEN   July 2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Simulate a vapor compression chiller using the reformulated model developed by Mark Hydeman

        // METHODOLOGY EMPLOYED:
        // Use empirical curve fits to model performance at off-design conditions. This subroutine
        // calls Subroutines CalcReformEIRChillerModel and General::SolveRoot to obtain solution.
        // The actual chiller performance calculations are in Subroutine CalcReformEIRChillerModel.

        // REFERENCES:
        // 1. Hydeman, M., P. Sreedharan, N. Webb, and S. Blanc. 2002. "Development and Testing of a Reformulated
        //    Regression-Based Electric Chiller Model". ASHRAE Transactions, HI-02-18-2, Vol 108, Part 2, pp. 1118-1127.

        Real64 const Acc(0.0001); // Accuracy control for General::SolveRoot
        int const MaxIter(500);   // Iteration control for General::SolveRoot

        if (MyLoad >= 0.0 || !RunFlag) {
            this->calculate(MyLoad, RunFlag, DataLoopNode::Node(this->CondInletNodeNum).Temp);
        } else {

            //  Find min/max condenser outlet temperature used by curve objects

            // Minimum condenser leaving temperature allowed by CAPFT curve [C]
            Real64 CAPFTYTmin = this->ChillerCAPFTYTempMin;

            // Minimum condenser leaving temperature allowed by EIRFT curve [C]
            Real64 Tmin(-99); // Minimum condenser leaving temperature allowed by curve objects [C]

            Real64 EIRFTYTmin = this->ChillerEIRFTYTempMin;
            if (this->PartLoadCurveType == PLR::LeavingCondenserWaterTemperature) {
                // Minimum condenser leaving temperature allowed by EIRFPLR curve [C]
                Real64 EIRFPLRTmin = this->ChillerEIRFPLRTempMin;
                Tmin = min(CAPFTYTmin, EIRFTYTmin, EIRFPLRTmin);
            } else if (this->PartLoadCurveType == PLR::Lift) {
                Tmin = min(CAPFTYTmin, EIRFTYTmin);
            }

            // Maximum condenser leaving temperature allowed by CAPFT curve [C]
            Real64 CAPFTYTmax = this->ChillerCAPFTYTempMax;

            Real64 Tmax(-99); // Maximum condenser leaving temperature allowed by curve objects [C]

            // Maximum condenser leaving temperature allowed by EIRFT curve [C]
            Real64 EIRFTYTmax = this->ChillerEIRFTYTempMax;
            if (this->PartLoadCurveType == PLR::LeavingCondenserWaterTemperature) {
                // Maximum condenser leaving temperature allowed by EIRFPLR curve [C]
                Real64 EIRFPLRTmax = this->ChillerEIRFPLRTempMax;
                Tmax = max(CAPFTYTmax, EIRFTYTmax, EIRFPLRTmax);
            } else if (this->PartLoadCurveType == PLR::Lift) {
                Tmax = max(CAPFTYTmax, EIRFTYTmax);
            }

            //  Check that condenser outlet temperature is within curve object limits prior to calling RegulaFalsi
            this->calculate(MyLoad, RunFlag, Tmin);

            // Condenser outlet temperature when using Tmin as input to calculate [C]
            Real64 CondTempMin = this->CondOutletTemp;

            //  Check that condenser outlet temperature is within curve object limits prior to calling RegulaFalsi
            this->calculate(MyLoad, RunFlag, Tmax);

            // Condenser outlet temperature when using Tmax as input to CalcReformEIRChillerModel [C]
            Real64 CondTempMax = this->CondOutletTemp;

            if (CondTempMin > Tmin && CondTempMax < Tmax) {

                Array1D<Real64> Par(6); // Pass parameters for RegulaFalsi solver

                //    Initialize iteration parameters for RegulaFalsi function
                Par(2) = MyLoad;
                if (RunFlag) {
                    Par(3) = 1.0;
                } else {
                    Par(3) = 0.0;
                }
                if (FirstIteration) {
                    Par(4) = 1.0;
                } else {
                    Par(4) = 0.0;
                }

                int SolFla;              // Feedback flag from General::SolveRoot
                Real64 FalsiCondOutTemp; // RegulaFalsi condenser outlet temperature result [C]
                auto f = std::bind(&ReformulatedEIRChillerSpecs::condOutTempResidual, this, std::placeholders::_1, std::placeholders::_2);
                General::SolveRoot(Acc, MaxIter, SolFla, FalsiCondOutTemp, f, Tmin, Tmax, Par);

                if (SolFla == -1) {
                    if (!DataGlobals::WarmupFlag) {
                        ++this->IterLimitExceededNum;
                        if (this->IterLimitExceededNum == 1) {
                            ShowWarningError(
                                this->Name +
                                ": Iteration limit exceeded calculating condenser outlet temperature and non-converged temperature is used");
                        } else {
                            ShowRecurringWarningErrorAtEnd(this->Name + ": Iteration limit exceeded calculating condenser outlet temperature.",
                                                           this->IterLimitErrIndex,
                                                           this->CondOutletTemp,
                                                           this->CondOutletTemp);
                        }
                    }
                } else if (SolFla == -2) {
                    if (!DataGlobals::WarmupFlag) {
                        ++this->IterFailed;
                        if (this->IterFailed == 1) {
                            ShowWarningError(this->Name + ": Solution found when calculating condenser outlet "
                                                          "temperature. The inlet temperature will used and the "
                                                          "simulation continues...");
                            ShowContinueError("Please check minimum and maximum values of x in EIRFPLR Curve " + this->EIRFPLRName);
                        } else {
                            ShowRecurringWarningErrorAtEnd(this->Name + ": Solution is not found in calculating condenser outlet temperature.",
                                                           this->IterFailedIndex,
                                                           this->CondOutletTemp,
                                                           this->CondOutletTemp);
                        }
                    }
                    this->calculate(MyLoad, RunFlag, DataLoopNode::Node(this->CondInletNodeNum).Temp);
                }
            } else {
                //    If iteration is not possible, average the min/max condenser outlet temperature and manually determine solution
                this->calculate(MyLoad, RunFlag, (CondTempMin + CondTempMax) / 2.0);
                this->calculate(MyLoad, RunFlag, this->CondOutletTemp);
            }

            //  Call subroutine to evaluate all performance curve min/max values against evaporator/condenser outlet temps and PLR
            this->checkMinMaxCurveBoundaries(FirstIteration);
        }
    }

    void ReformulatedEIRChillerSpecs::calcHeatRecovery(Real64 &QCond,              // Current condenser load [W]
                                                       Real64 const CondMassFlow,  // Current condenser mass flow [kg/s]
                                                       Real64 const condInletTemp, // Current condenser inlet temp [C]
                                                       Real64 &QHeatRec            // Amount of heat recovered [W]
    )
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Lixing Gu, FSEC
        //       DATE WRITTEN:    July 2006
        //       MODIFIED:        na

        // PURPOSE OF THIS SUBROUTINE:
        //  Calculate the heat recovered from the chiller condenser

        static std::string const RoutineName("EIRChillerHeatRecovery");

        // inlet node to the heat recovery heat exchanger
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
        } else { // use new algorithm to meet setpoint
            Real64 THeatRecSetPoint(0.0);

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

    void ReformulatedEIRChillerSpecs::update(Real64 const MyLoad, bool const RunFlag)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Lixing Gu, FSEC
        //       DATE WRITTEN:    July 2006

        // PURPOSE OF THIS SUBROUTINE:
        //  Reporting

        if (MyLoad >= 0.0 || !RunFlag) { // Chiller not running so pass inlet states to outlet states
            // Set node temperatures
            DataLoopNode::Node(this->EvapOutletNodeNum).Temp = DataLoopNode::Node(this->EvapInletNodeNum).Temp;
            DataLoopNode::Node(this->CondOutletNodeNum).Temp = DataLoopNode::Node(this->CondInletNodeNum).Temp;

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

            if (this->HeatRecActive) {

                PlantUtilities::SafeCopyPlantNode(this->HeatRecInletNodeNum, this->HeatRecOutletNodeNum);
                this->QHeatRecovery = 0.0;
                this->EnergyHeatRecovery = 0.0;
                this->HeatRecInletTemp = DataLoopNode::Node(this->HeatRecInletNodeNum).Temp;
                this->HeatRecOutletTemp = DataLoopNode::Node(this->HeatRecOutletNodeNum).Temp;
                this->HeatRecMassFlow = DataLoopNode::Node(this->HeatRecInletNodeNum).MassFlowRate;
            }

        } else { // Chiller is running, so pass calculated values
            // Set node temperatures
            DataLoopNode::Node(this->EvapOutletNodeNum).Temp = this->EvapOutletTemp;
            DataLoopNode::Node(this->CondOutletNodeNum).Temp = this->CondOutletTemp;
            // Set node flow rates;  for these load based models
            // assume that sufficient evaporator flow rate is available
            this->ChillerFalseLoad = this->ChillerFalseLoadRate * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
            this->Energy = this->Power * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
            this->EvapEnergy = this->QEvaporator * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
            this->CondEnergy = this->QCondenser * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
            this->EvapInletTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp;
            this->CondInletTemp = DataLoopNode::Node(this->CondInletNodeNum).Temp;
            this->CondOutletTemp = DataLoopNode::Node(this->CondOutletNodeNum).Temp;
            if (this->Power != 0.0) {
                this->ActualCOP = (this->QEvaporator + this->ChillerFalseLoadRate) / this->Power;
            } else {
                this->ActualCOP = 0.0;
            }

            if (this->HeatRecActive) {

                PlantUtilities::SafeCopyPlantNode(this->HeatRecInletNodeNum, this->HeatRecOutletNodeNum);
                this->EnergyHeatRecovery = this->QHeatRecovery * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
                DataLoopNode::Node(this->HeatRecOutletNodeNum).Temp = this->HeatRecOutletTemp;
                this->HeatRecInletTemp = DataLoopNode::Node(this->HeatRecInletNodeNum).Temp;
                this->HeatRecOutletTemp = DataLoopNode::Node(this->HeatRecOutletNodeNum).Temp;
                this->HeatRecMassFlow = DataLoopNode::Node(this->HeatRecInletNodeNum).MassFlowRate;
            }
        }
    }

    Real64 ReformulatedEIRChillerSpecs::condOutTempResidual(Real64 const FalsiCondOutTemp, Array1D<Real64> const &Par)
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Richard Raustad
        //       DATE WRITTEN   May 2006
        //       MODIFIED       L.Gu, May 2006
        //       RE-ENGINEERED

        // PURPOSE OF THIS FUNCTION:
        //  Calculates residual function (desired condenser outlet temperature)
        //  Reformulated EIR chiller requires condenser outlet temperature to calculate capacity and power.

        // METHODOLOGY EMPLOYED:
        //  Regula Falsi solver is used to calculate condenser outlet temperature.

        Real64 MyLoad = Par(2);
        bool RunFlag = (int(Par(3)) == 1);

        this->calculate(MyLoad, RunFlag, FalsiCondOutTemp);
        Real64 CondOutTempResidual = FalsiCondOutTemp - this->CondOutletTemp; // CondOutletTemp is module level variable, final value used for reporting

        return CondOutTempResidual;
    }

    void ReformulatedEIRChillerSpecs::calculate(Real64 &MyLoad, bool const RunFlag, Real64 const FalsiCondOutTemp)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Lixing Gu, FSEC
        //       DATE WRITTEN   July 2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        //       MODIFIED
        //			Aug. 2014, Rongpeng Zhang, added an additional part-load performance curve type
        //			Jun. 2016, Rongpeng Zhang, applied the chiller supply water temperature sensor fault model
        //			Nov. 2016, Rongpeng Zhang, added fouling chiller fault

        // PURPOSE OF THIS SUBROUTINE:
        //  Simulate a vapor compression chiller using the reformulated model developed by Mark Hydeman

        // METHODOLOGY EMPLOYED:
        //  Use empirical curve fits to model performance at off-design conditions

        // REFERENCES:
        // 1. Hydeman, M., P. Sreedharan, N. Webb, and S. Blanc. 2002. "Development and Testing of a Reformulated
        //    Regression-Based Electric Chiller Model". ASHRAE Transactions, HI-02-18-2, Vol 108, Part 2, pp. 1118-1127.

        static std::string const RoutineName("CalcElecReformEIRChillerModel");

        this->ChillerPartLoadRatio = 0.0;
        this->ChillerCyclingRatio = 0.0;
        this->ChillerFalseLoadRate = 0.0;
        this->EvapMassFlowRate = 0.0;
        this->CondMassFlowRate = 0.0;
        this->Power = 0.0;
        this->QCondenser = 0.0;
        this->QEvaporator = 0.0;
        this->QHeatRecovery = 0.0;
        int PlantLoopNum = this->CWLoopNum;
        int LoopSideNum = this->CWLoopSideNum;
        int BranchNum = this->CWBranchNum;
        int CompNum = this->CWCompNum;

        // Set performance curve outputs to 0.0 when chiller is off
        this->ChillerCapFT = 0.0;
        this->ChillerEIRFT = 0.0;
        this->ChillerEIRFPLR = 0.0;

        // Set module-level chiller evap and condenser inlet temperature variables
        Real64 condInletTemp = DataLoopNode::Node(this->CondInletNodeNum).Temp;

        // If no loop demand or chiller OFF, return
        // If chiller load is 0 or chiller is not running then leave the subroutine. Before leaving
        //  if the component control is SERIESACTIVE we set the component flow to inlet flow so that
        //  flow resolver will not shut down the branch
        if (MyLoad >= 0 || !RunFlag) {
            if (this->EquipFlowCtrl == DataBranchAirLoopPlant::ControlType_SeriesActive ||
                DataPlant::PlantLoop(PlantLoopNum).LoopSide(LoopSideNum).FlowLock == 1) {
                this->EvapMassFlowRate = DataLoopNode::Node(this->EvapInletNodeNum).MassFlowRate;
            }
            if (this->CondenserType == DataPlant::CondenserType::WATERCOOLED) {
                if (DataPlant::PlantLoop(this->CDLoopNum).LoopSide(this->CDLoopSideNum).Branch(this->CDBranchNum).Comp(this->CDCompNum).FlowCtrl ==
                    DataBranchAirLoopPlant::ControlType_SeriesActive) {
                    this->CondMassFlowRate = DataLoopNode::Node(this->CondInletNodeNum).MassFlowRate;
                }
            }

            return;
        }

        // Chiller reference capacity [W]
        Real64 ChillerRefCap = this->RefCap;

        // Reference coefficient of performance, from user input
        Real64 ReferenceCOP = this->RefCOP;
        this->EvapOutletTemp = DataLoopNode::Node(this->EvapOutletNodeNum).Temp;

        // Evaporator low temp. limit cut off [C]
        Real64 TempLowLimitEout = this->TempLowLimitEvapOut;

        // If there is a fault of chiller fouling
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

            if (this->CondMassFlowRate < DataBranchAirLoopPlant::MassFlowTolerance) return;
        }
        Real64 FRAC = 1.0;
        Real64 EvapOutletTempSetPoint(0.0); // Evaporator outlet temperature setpoint [C]
        {
            auto const SELECT_CASE_var(DataPlant::PlantLoop(PlantLoopNum).LoopDemandCalcScheme);
            if (SELECT_CASE_var == DataPlant::SingleSetPoint) {
                if ((this->FlowMode == DataPlant::FlowMode::LEAVINGSETPOINTMODULATED) ||
                    (DataPlant::PlantLoop(PlantLoopNum).LoopSide(LoopSideNum).Branch(BranchNum).Comp(CompNum).CurOpSchemeType ==
                     DataPlant::CompSetPtBasedSchemeType) ||
                    (DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPoint != DataLoopNode::SensedNodeFlagValue)) {
                    // there will be a valid setpoint on outlet
                    EvapOutletTempSetPoint = DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPoint;
                } else { // use plant loop overall setpoint
                    EvapOutletTempSetPoint = DataLoopNode::Node(DataPlant::PlantLoop(PlantLoopNum).TempSetPointNodeNum).TempSetPoint;
                }
            } else if (SELECT_CASE_var == DataPlant::DualSetPointDeadBand) {
                if ((this->FlowMode == DataPlant::FlowMode::LEAVINGSETPOINTMODULATED) ||
                    (DataPlant::PlantLoop(PlantLoopNum).LoopSide(LoopSideNum).Branch(BranchNum).Comp(CompNum).CurOpSchemeType ==
                     DataPlant::CompSetPtBasedSchemeType) ||
                    (DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPointHi != DataLoopNode::SensedNodeFlagValue)) {
                    // there will be a valid setpoint on outlet
                    EvapOutletTempSetPoint = DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPointHi;
                } else { // use plant loop overall setpoint
                    EvapOutletTempSetPoint = DataLoopNode::Node(DataPlant::PlantLoop(PlantLoopNum).TempSetPointNodeNum).TempSetPointHi;
                }
            } else {
                assert(false);
            }
        }

        // If there is a fault of Chiller SWT Sensor
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
        if (this->HeatRecActive) {
            if ((this->QHeatRecovery + this->QCondenser) > 0.0) { // protect div by zero
                this->ChillerCondAvgTemp = (this->QHeatRecovery * this->HeatRecOutletTemp + this->QCondenser * this->CondOutletTemp) /
                                           (this->QHeatRecovery + this->QCondenser);
            } else {
                this->ChillerCondAvgTemp = FalsiCondOutTemp;
            }
        } else {
            this->ChillerCondAvgTemp = FalsiCondOutTemp;
        }

        // Get capacity curve info with respect to CW setpoint and leaving condenser water temps
        this->ChillerCapFT = max(0.0, CurveManager::CurveValue(this->ChillerCapFTIndex, EvapOutletTempSetPoint, this->ChillerCondAvgTemp));

        // Available chiller capacity as a function of temperature
        Real64 AvailChillerCap = ChillerRefCap * this->ChillerCapFT;

        this->EvapMassFlowRate = DataLoopNode::Node(this->EvapInletNodeNum).MassFlowRate;
        //   Some other component set the flow to 0. No reason to continue with calculations.
        if (this->EvapMassFlowRate == 0.0) {
            MyLoad = 0.0;
            return;
        }

        // This chiller is currently has only a water-cooled condenser

        // Calculate water side load
        Real64 Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(this->CWLoopNum).FluidName,
                                                           DataLoopNode::Node(this->EvapInletNodeNum).Temp,
                                                           DataPlant::PlantLoop(this->CWLoopNum).FluidIndex,
                                                           RoutineName);

        Real64 TempLoad; // actual load to be met by chiller. This value is compared to MyLoad
        // and reset when necessary since this chiller can cycle, the load passed
        // should be the actual load.  Instead the minimum PLR * RefCap is
        // passed in.

        TempLoad = DataLoopNode::Node(this->EvapInletNodeNum).MassFlowRateMaxAvail * Cp *
                   (DataLoopNode::Node(this->EvapInletNodeNum).Temp - EvapOutletTempSetPoint);

        TempLoad = max(0.0, TempLoad);

        // MyLoad is capped at minimum PLR * RefCap, adjust load to actual water side load because this chiller can cycle
        if (std::abs(MyLoad) > TempLoad) {
            MyLoad = sign(TempLoad, MyLoad);
        }

        // Part load ratio based on load and available chiller capacity, cap at max part load ratio
        Real64 PartLoadRat; // Operating part load ratio
        if (AvailChillerCap > 0) {
            PartLoadRat = max(0.0, min(std::abs(MyLoad) / AvailChillerCap, this->MaxPartLoadRat));
        } else {
            PartLoadRat = 0.0;
        }

        // Set evaporator heat transfer rate
        this->QEvaporator = AvailChillerCap * PartLoadRat;
        this->ChillerPartLoadRatio = PartLoadRat;
        // If FlowLock is False (0), the chiller sets the plant loop mdot
        // If FlowLock is True (1),  the new resolved plant loop mdot is used
        if (DataPlant::PlantLoop(PlantLoopNum).LoopSide(LoopSideNum).FlowLock == 0) {
            this->PossibleSubcooling = !(DataPlant::PlantLoop(PlantLoopNum).LoopSide(LoopSideNum).Branch(BranchNum).Comp(CompNum).CurOpSchemeType ==
                                         DataPlant::CompSetPtBasedSchemeType);

            Real64 EvapDeltaTemp(0.0); // Evaporator temperature difference [C]

            // Either set the flow to the Constant value or calculate the flow for the variable volume case
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
                this->EvapOutletTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp - EvapDeltaTemp;
            } else if (this->FlowMode == DataPlant::FlowMode::LEAVINGSETPOINTMODULATED) {
                {
                    auto const SELECT_CASE_var(DataPlant::PlantLoop(PlantLoopNum).LoopDemandCalcScheme);
                    if (SELECT_CASE_var == DataPlant::SingleSetPoint) {
                        // Calculate the Delta Temp from the inlet temp to the chiller outlet setpoint
                        EvapDeltaTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp - DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPoint;
                    } else if (SELECT_CASE_var == DataPlant::DualSetPointDeadBand) {
                        EvapDeltaTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp - DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPointHi;
                    } else {
                        assert(false);
                    }
                }

                if (EvapDeltaTemp != 0) {
                    this->EvapMassFlowRate = max(0.0, (this->QEvaporator / Cp / EvapDeltaTemp));
                    if ((this->EvapMassFlowRate - this->EvapMassFlowRateMax) > DataBranchAirLoopPlant::MassFlowTolerance)
                        this->PossibleSubcooling = true;
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
                        auto const SELECT_CASE_var(DataPlant::PlantLoop(PlantLoopNum).LoopDemandCalcScheme);
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

                    if (this->DeltaTErrCount < 1 && !DataGlobals::WarmupFlag) {
                        ++this->DeltaTErrCount;
                        ShowWarningError("Evaporator DeltaTemp = 0 in mass flow calculation (Tevapin = Tevapout setpoint temp).");
                        ShowContinueErrorTimeStamp("");
                    } else if (!DataGlobals::WarmupFlag) {
                        ++this->ChillerCapFTError;
                        ShowRecurringWarningErrorAtEnd("CHILLER:ELECTRIC:REFORMULATEDEIR \"" + this->Name +
                                                           "\": Evaporator DeltaTemp = 0 in mass flow calculation warning continues...",
                                                       this->DeltaTErrCountIndex,
                                                       EvapDeltaTemp,
                                                       EvapDeltaTemp);
                    }
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
                // update corresponding variables at faulty case
                PartLoadRat = (AvailChillerCap > 0.0) ? (this->QEvaporator / AvailChillerCap) : 0.0;
                PartLoadRat = max(0.0, min(PartLoadRat, this->MaxPartLoadRat));
                this->ChillerPartLoadRatio = PartLoadRat;
            }

        } else { // If FlowLock is True
            this->EvapMassFlowRate = DataLoopNode::Node(this->EvapInletNodeNum).MassFlowRate;
            PlantUtilities::SetComponentFlowRate(this->EvapMassFlowRate,
                                                 this->EvapInletNodeNum,
                                                 this->EvapOutletNodeNum,
                                                 this->CWLoopNum,
                                                 this->CWLoopSideNum,
                                                 this->CWBranchNum,
                                                 this->CWCompNum);
            //       Some other component set the flow to 0. No reason to continue with calculations.
            if (this->EvapMassFlowRate == 0.0) {
                MyLoad = 0.0;
                return;
            }

            Real64 EvapDeltaTemp;

            if (this->PossibleSubcooling) {
                this->QEvaporator = std::abs(MyLoad);
                EvapDeltaTemp = this->QEvaporator / this->EvapMassFlowRate / Cp;
                this->EvapOutletTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp - EvapDeltaTemp;
            } else {
                EvapDeltaTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp - EvapOutletTempSetPoint;
                this->QEvaporator = max(0.0, (this->EvapMassFlowRate * Cp * EvapDeltaTemp));
                this->EvapOutletTemp = EvapOutletTempSetPoint;
            }
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
                if ((DataLoopNode::Node(this->EvapInletNodeNum).Temp - DataLoopNode::Node(this->EvapOutletNodeNum).TempMin) >
                    DataPlant::DeltaTempTol) {
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
                // update corresponding variables at faulty case
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

        } // This is the end of the FlowLock Block

        this->ChillerEIRFT = max(0.0, CurveManager::CurveValue(this->ChillerEIRFTIndex, this->EvapOutletTemp, this->ChillerCondAvgTemp));

        // Part Load Ratio Curve Type: 1_LeavingCondenserWaterTemperature; 2_Lift
        if (this->PartLoadCurveType == PLR::LeavingCondenserWaterTemperature) {
            this->ChillerEIRFPLR = max(0.0, CurveManager::CurveValue(this->ChillerEIRFPLRIndex, this->ChillerCondAvgTemp, PartLoadRat));
        } else if (this->PartLoadCurveType == PLR::Lift) {

            // Chiller lift
            Real64 ChillerLift = this->ChillerCondAvgTemp - this->EvapOutletTemp;

            // Deviation of leaving chilled water temperature from the reference condition
            Real64 ChillerTdev = std::abs(this->EvapOutletTemp - this->TempRefEvapOut);

            // Chiller lift under the reference condition
            Real64 ChillerLiftRef = this->TempRefCondOut - this->TempRefEvapOut;

            if (ChillerLiftRef <= 0) ChillerLiftRef = 35 - 6.67;

            // Normalized chiller lift
            Real64 ChillerLiftNom = ChillerLift / ChillerLiftRef;

            // Normalized ChillerTdev
            Real64 ChillerTdevNom = ChillerTdev / ChillerLiftRef;

            this->ChillerEIRFPLR = max(0.0, CurveManager::CurveValue(this->ChillerEIRFPLRIndex, ChillerLiftNom, PartLoadRat, ChillerTdevNom));
        }

        if (ReferenceCOP <= 0) ReferenceCOP = 5.5;
        this->Power = (AvailChillerCap / ReferenceCOP) * this->ChillerEIRFPLR * this->ChillerEIRFT * FRAC;

        this->QCondenser = this->Power * this->CompPowerToCondenserFrac + this->QEvaporator + this->ChillerFalseLoadRate;

        //  Currently only water cooled chillers are allowed for the reformulated EIR chiller model
        if (this->CondMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance) {
            // If Heat Recovery specified for this vapor compression chiller, then Qcondenser will be adjusted by this subroutine
            if (this->HeatRecActive) this->calcHeatRecovery(this->QCondenser, this->CondMassFlowRate, condInletTemp, this->QHeatRecovery);
            Cp = FluidProperties::GetSpecificHeatGlycol(
                DataPlant::PlantLoop(this->CDLoopNum).FluidName, condInletTemp, DataPlant::PlantLoop(this->CDLoopNum).FluidIndex, RoutineName);
            this->CondOutletTemp = this->QCondenser / this->CondMassFlowRate / Cp + condInletTemp;
        } else {
            ShowSevereError("ControlReformEIRChillerModel: Condenser flow = 0, for ElecReformEIRChiller=" + this->Name);
            ShowContinueErrorTimeStamp("");
        }
    }

    void ReformulatedEIRChillerSpecs::checkMinMaxCurveBoundaries(bool const FirstIteration)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR:          R Raustad, FSEC
        //       DATE WRITTEN:    August 2006

        // PURPOSE OF THIS SUBROUTINE:
        //  To compare the evaporator/condenser outlet temperatures to curve object min/max values

        // Do not print out warnings if chiller not operating or FirstIteration/DataGlobals::WarmupFlag/FlowLock
        int PlantLoopNum = this->CWLoopNum;
        int LoopSideNum = this->CWLoopSideNum;
        int BranchNum = this->CWBranchNum;
        int CompNum = this->CWCompNum;

        if (FirstIteration || DataGlobals::WarmupFlag || DataPlant::PlantLoop(PlantLoopNum).LoopSide(LoopSideNum).FlowLock == 0) return;

        // Minimum evaporator leaving temperature allowed by CAPFT curve [C]
        Real64 CAPFTXTmin = this->ChillerCAPFTXTempMin;

        // Maximum evaporator leaving temperature allowed by CAPFT curve [C]
        Real64 CAPFTXTmax = this->ChillerCAPFTXTempMax;

        // Minimum evaporator leaving temperature allowed by EIRFT curve [C]
        Real64 EIRFTXTmin = this->ChillerEIRFTXTempMin;

        // Maximum evaporator leaving temperature allowed by EIRFT curve [C]
        Real64 EIRFTXTmax = this->ChillerEIRFTXTempMax;

        // Check bounds for curves, lump min/max into same check since min/max values are reported in recurring warning messages
        if (this->EvapOutletTemp < CAPFTXTmin || this->EvapOutletTemp > CAPFTXTmax) {
            ++this->CAPFTXIter;
            if (this->CAPFTXIter == 1) {
                ShowWarningError("CHILLER:ELECTRIC:REFORMULATEDEIR \"" + this->Name + "\": The evaporator outlet temperature (" +
                                 General::TrimSigDigits(this->EvapOutletTemp, 2) +
                                 " C) is outside the range of evaporator outlet temperatures (X var) given in Cooling Capacity Function of "
                                 "Temperature biquadratic curve = " +
                                 this->CAPFTName);
                ShowContinueErrorTimeStamp("The range specified = " + General::TrimSigDigits(CAPFTXTmin, 2) + " C to " +
                                           General::TrimSigDigits(CAPFTXTmax, 2) + " C.");
                ShowRecurringWarningErrorAtEnd("CHILLER:ELECTRIC:REFORMULATEDEIR \"" + this->Name +
                                                   "\": The evap outlet temp range in Cooling Capacity Function of Temp curve error continues.",
                                               this->CAPFTXIterIndex,
                                               this->EvapOutletTemp,
                                               this->EvapOutletTemp);
            } else {
                ShowRecurringWarningErrorAtEnd("CHILLER:ELECTRIC:REFORMULATEDEIR \"" + this->Name +
                                                   "\": The evap outlet temp range in Cooling Capacity Function of Temp curve error continues.",
                                               this->CAPFTXIterIndex,
                                               this->EvapOutletTemp,
                                               this->EvapOutletTemp);
            }
        }

        if (this->EvapOutletTemp < EIRFTXTmin || this->EvapOutletTemp > EIRFTXTmax) {
            ++this->EIRFTXIter;
            if (this->EIRFTXIter == 1) {
                ShowWarningError("CHILLER:ELECTRIC:REFORMULATEDEIR \"" + this->Name + "\": The evaporator outlet temperature (" +
                                 General::TrimSigDigits(this->EvapOutletTemp, 2) +
                                 " C) is outside the range of evaporator outlet temperatures (X var) given in Electric Input to Cooling Output Ratio "
                                 "Function of Temperature biquadratic curve = " +
                                 this->EIRFTName);
                ShowContinueErrorTimeStamp("The range specified = " + General::TrimSigDigits(EIRFTXTmin, 2) + " C to " +
                                           General::TrimSigDigits(EIRFTXTmax, 2) + " C.");
                ShowRecurringWarningErrorAtEnd(
                    "CHILLER:ELECTRIC:REFORMULATEDEIR \"" + this->Name +
                        "\": The evap outlet temp range in Electric Input to Cooling Output Ratio Function of Temp curve error continues.",
                    this->EIRFTXIterIndex,
                    this->EvapOutletTemp,
                    this->EvapOutletTemp);
            } else {
                ShowRecurringWarningErrorAtEnd(
                    "CHILLER:ELECTRIC:REFORMULATEDEIR \"" + this->Name +
                        "\": The evap outlet temp range in Electric Input to Cooling Output Ratio Function of Temp curve error continues.",
                    this->EIRFTXIterIndex,
                    this->EvapOutletTemp,
                    this->EvapOutletTemp);
            }
        }

        // Move CAPFT, EIRFT, and EIRFPLR min/max condenser outlet temperature values to local variables

        // Minimum condenser  leaving temperature allowed by CAPFT curve [C]
        Real64 CAPFTYTmin = this->ChillerCAPFTYTempMin;

        // Maximum condenser  leaving temperature allowed by CAPFT curve [C]
        Real64 CAPFTYTmax = this->ChillerCAPFTYTempMax;

        // Minimum condenser  leaving temperature allowed by EIRFT curve [C]
        Real64 EIRFTYTmin = this->ChillerEIRFTYTempMin;

        // Maximum condenser  leaving temperature allowed by EIRFT curve [C]
        Real64 EIRFTYTmax = this->ChillerEIRFTYTempMax;

        Real64 EIRFPLRTmin(0.0); // Minimum condenser  leaving temperature allowed by EIRFPLR curve [C]
        Real64 EIRFPLRTmax(0.0); // Maximum condenser  leaving temperature allowed by EIRFPLR curve [C]

        if (this->PartLoadCurveType == PLR::LeavingCondenserWaterTemperature) {
            EIRFPLRTmin = this->ChillerEIRFPLRTempMin;
            EIRFPLRTmax = this->ChillerEIRFPLRTempMax;
        }

        // Move EIRFPLR min/max part-load ratio values to local variables

        // Minimum PLR allowed by EIRFPLR curve
        Real64 EIRFPLRPLRmin = this->ChillerEIRFPLRPLRMin;

        // Maximum PLR allowed by EIRFPLR curve
        Real64 EIRFPLRPLRmax = this->ChillerEIRFPLRPLRMax;

        // Check bounds for curves, lump min/max into same check since min/max values are reported in recurring warning messages
        if (this->CondOutletTemp < CAPFTYTmin || this->CondOutletTemp > CAPFTYTmax) {
            ++this->CAPFTYIter;
            if (this->CAPFTYIter == 1) {
                ShowWarningError("CHILLER:ELECTRIC:REFORMULATEDEIR \"" + this->Name + "\": The condenser outlet temperature (" +
                                 General::TrimSigDigits(this->CondOutletTemp, 2) +
                                 " C) is outside the range of condenser outlet temperatures (Y var) given in Cooling Capacity Function of "
                                 "Temperature biquadratic curve = " +
                                 this->CAPFTName);
                ShowContinueErrorTimeStamp("The range specified = " + General::TrimSigDigits(CAPFTYTmin, 2) + " C to " +
                                           General::TrimSigDigits(CAPFTYTmax, 2) + " C.");
                ShowRecurringWarningErrorAtEnd("CHILLER:ELECTRIC:REFORMULATEDEIR \"" + this->Name +
                                                   "\": The cond outlet temp range in Cooling Capacity Function of Temp curve error continues.",
                                               this->CAPFTYIterIndex,
                                               this->CondOutletTemp,
                                               this->CondOutletTemp);
            } else {
                ShowRecurringWarningErrorAtEnd("CHILLER:ELECTRIC:REFORMULATEDEIR \"" + this->Name +
                                                   "\": The cond outlet temp range in Cooling Capacity Function of Temp curve error continues.",
                                               this->CAPFTYIterIndex,
                                               this->CondOutletTemp,
                                               this->CondOutletTemp);
            }
        }

        if (this->CondOutletTemp < EIRFTYTmin || this->CondOutletTemp > EIRFTYTmax) {
            ++this->EIRFTYIter;
            if (this->EIRFTYIter == 1) {
                ShowWarningError("CHILLER:ELECTRIC:REFORMULATEDEIR \"" + this->Name + "\": The condenser outlet temperature (" +
                                 General::TrimSigDigits(this->CondOutletTemp, 2) +
                                 " C) is outside the range of condenser outlet temperatures (Y var) given in Electric Input to Cooling Output Ratio "
                                 "Function of Temperature biquadratic curve = " +
                                 this->EIRFTName);
                ShowContinueErrorTimeStamp("The range specified = " + General::TrimSigDigits(EIRFTYTmin, 2) + " C to " +
                                           General::TrimSigDigits(EIRFTYTmax, 2) + " C.");
                ShowRecurringWarningErrorAtEnd(
                    "CHILLER:ELECTRIC:REFORMULATEDEIR \"" + this->Name +
                        "\": The cond outlet temp range in Electric Input to Cooling Output Ratio as a Function of Temp curve error continues.",
                    this->EIRFTYIterIndex,
                    this->CondOutletTemp,
                    this->CondOutletTemp);
            } else {
                ShowRecurringWarningErrorAtEnd(
                    "CHILLER:ELECTRIC:REFORMULATEDEIR \"" + this->Name +
                        "\": The cond outlet temp range in Electric Input to Cooling Output Ratio as a Function of Temp curve error continues.",
                    this->EIRFTYIterIndex,
                    this->CondOutletTemp,
                    this->CondOutletTemp);
            }
        }

        if (this->PartLoadCurveType == PLR::LeavingCondenserWaterTemperature) {
            if (this->CondOutletTemp < EIRFPLRTmin || this->CondOutletTemp > EIRFPLRTmax) {
                ++this->EIRFPLRTIter;
                if (this->EIRFPLRTIter == 1) {
                    ShowWarningError("CHILLER:ELECTRIC:REFORMULATEDEIR \"" + this->Name + "\": The condenser outlet temperature (" +
                                     General::TrimSigDigits(this->CondOutletTemp, 2) +
                                     " C) is outside the range of condenser outlet temperatures (X var) given in Electric Input to Cooling Output "
                                     "Ratio Function of Part-load Ratio bicubic curve = " +
                                     this->EIRFPLRName);
                    ShowContinueErrorTimeStamp("The range specified = " + General::TrimSigDigits(EIRFPLRTmin, 2) + " C to " +
                                               General::TrimSigDigits(EIRFPLRTmax, 2) + " C.");
                    ShowRecurringWarningErrorAtEnd(
                        "CHILLER:ELECTRIC:REFORMULATEDEIR \"" + this->Name +
                            "\": The cond outlet temp range in Electric Input to Cooling Output Ratio Function of PLR curve error continues.",
                        this->EIRFPLRTIterIndex,
                        this->CondOutletTemp,
                        this->CondOutletTemp);
                } else {
                    ShowRecurringWarningErrorAtEnd(
                        "CHILLER:ELECTRIC:REFORMULATEDEIR \"" + this->Name +
                            "\": The cond outlet temp range in Electric Input to Cooling Output Ratio Function of PLR curve error continues.",
                        this->EIRFPLRTIterIndex,
                        this->CondOutletTemp,
                        this->CondOutletTemp);
                }
            }
        }

        if (this->ChillerPartLoadRatio < EIRFPLRPLRmin || this->ChillerPartLoadRatio > EIRFPLRPLRmax) {
            ++this->EIRFPLRPLRIter;
            if (this->EIRFPLRPLRIter == 1) {
                ShowWarningError("CHILLER:ELECTRIC:REFORMULATEDEIR \"" + this->Name + "\": The part-load ratio (" +
                                 General::TrimSigDigits(this->ChillerPartLoadRatio, 3) +
                                 ") is outside the range of part-load ratios (Y var) given in Electric Input to Cooling Output Ratio Function of "
                                 "Part-load Ratio bicubic curve = " +
                                 this->EIRFPLRName);
                ShowContinueErrorTimeStamp("The range specified = " + General::TrimSigDigits(EIRFPLRPLRmin, 3) + " to " +
                                           General::TrimSigDigits(EIRFPLRPLRmax, 3) + '.');
                ShowRecurringWarningErrorAtEnd(
                    "CHILLER:ELECTRIC:REFORMULATEDEIR \"" + this->Name +
                        "\": The part-load ratio range in Electric Input to Cooling Output Ratio Function of PLRatio curve error continues.",
                    this->EIRFPLRPLRIterIndex,
                    this->ChillerPartLoadRatio,
                    this->ChillerPartLoadRatio);
            } else {
                ShowRecurringWarningErrorAtEnd(
                    "CHILLER:ELECTRIC:REFORMULATEDEIR \"" + this->Name +
                        "\": The part-load ratio range in Electric Input to Cooling Output Ratio Function of PLRatio curve error continues.",
                    this->EIRFPLRPLRIterIndex,
                    this->ChillerPartLoadRatio,
                    this->ChillerPartLoadRatio);
            }
        }

        Real64 EvapOutletTempSetPoint(0.0); // Evaporator outlet temperature setpoint [C]

        {
            auto const SELECT_CASE_var(DataPlant::PlantLoop(PlantLoopNum).LoopDemandCalcScheme);
            if (SELECT_CASE_var == DataPlant::SingleSetPoint) {
                if ((this->FlowMode == DataPlant::FlowMode::LEAVINGSETPOINTMODULATED) ||
                    (DataPlant::PlantLoop(PlantLoopNum).LoopSide(LoopSideNum).Branch(BranchNum).Comp(CompNum).CurOpSchemeType ==
                     DataPlant::CompSetPtBasedSchemeType) ||
                    (DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPoint != DataLoopNode::SensedNodeFlagValue)) {
                    // there will be a valid setpoint on outlet
                    EvapOutletTempSetPoint = DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPoint;
                } else { // use plant loop overall setpoint
                    EvapOutletTempSetPoint = DataLoopNode::Node(DataPlant::PlantLoop(PlantLoopNum).TempSetPointNodeNum).TempSetPoint;
                }
            } else if (SELECT_CASE_var == DataPlant::DualSetPointDeadBand) {
                if ((this->FlowMode == DataPlant::FlowMode::LEAVINGSETPOINTMODULATED) ||
                    (DataPlant::PlantLoop(PlantLoopNum).LoopSide(LoopSideNum).Branch(BranchNum).Comp(CompNum).CurOpSchemeType ==
                     DataPlant::CompSetPtBasedSchemeType) ||
                    (DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPointHi != DataLoopNode::SensedNodeFlagValue)) {
                    // there will be a valid setpoint on outlet
                    EvapOutletTempSetPoint = DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPointHi;
                } else { // use plant loop overall setpoint
                    EvapOutletTempSetPoint = DataLoopNode::Node(DataPlant::PlantLoop(PlantLoopNum).TempSetPointNodeNum).TempSetPointHi;
                }
            } else {
                assert(false);
            }
        }

        this->ChillerCapFT = CurveManager::CurveValue(this->ChillerCapFTIndex, EvapOutletTempSetPoint, this->CondOutletTemp);

        if (this->ChillerCapFT < 0) {
            if (this->ChillerCapFTError < 1 && DataPlant::PlantLoop(PlantLoopNum).LoopSide(LoopSideNum).FlowLock != 0 && !DataGlobals::WarmupFlag) {
                ++this->ChillerCapFTError;
                ShowWarningError("CHILLER:ELECTRIC:REFORMULATEDEIR \"" + this->Name + "\":");
                ShowContinueError(" Chiller Capacity as a Function of Temperature curve output is negative (" +
                                  General::RoundSigDigits(this->ChillerCapFT, 3) + ").");
                ShowContinueError(" Negative value occurs using an Evaporator Leaving Temp of " + General::RoundSigDigits(EvapOutletTempSetPoint, 1) +
                                  " and a Condenser Leaving Temp of " + General::RoundSigDigits(this->CondOutletTemp, 1) + '.');
                ShowContinueErrorTimeStamp(" Resetting curve output to zero and continuing simulation.");
            } else if (DataPlant::PlantLoop(PlantLoopNum).LoopSide(LoopSideNum).FlowLock != 0 && !DataGlobals::WarmupFlag) {
                ++this->ChillerCapFTError;
                ShowRecurringWarningErrorAtEnd("CHILLER:ELECTRIC:REFORMULATEDEIR \"" + this->Name +
                                                   "\": Chiller Capacity as a Function of Temperature curve output is negative warning continues...",
                                               this->ChillerCapFTErrorIndex,
                                               this->ChillerCapFT,
                                               this->ChillerCapFT);
            }
        }

        this->ChillerEIRFT = CurveManager::CurveValue(this->ChillerEIRFTIndex, this->EvapOutletTemp, this->CondOutletTemp);

        if (this->ChillerEIRFT < 0.0) {
            if (this->ChillerEIRFTError < 1 && DataPlant::PlantLoop(PlantLoopNum).LoopSide(LoopSideNum).FlowLock != 0 && !DataGlobals::WarmupFlag) {
                ++this->ChillerEIRFTError;
                ShowWarningError("CHILLER:ELECTRIC:REFORMULATEDEIR \"" + this->Name + "\":");
                ShowContinueError(" Reformulated Chiller EIR as a Function of Temperature curve output is negative (" +
                                  General::RoundSigDigits(this->ChillerEIRFT, 3) + ").");
                ShowContinueError(" Negative value occurs using an Evaporator Leaving Temp of " + General::RoundSigDigits(this->EvapOutletTemp, 1) +
                                  " and a Condenser Leaving Temp of " + General::RoundSigDigits(this->CondOutletTemp, 1) + '.');
                ShowContinueErrorTimeStamp(" Resetting curve output to zero and continuing simulation.");
            } else if (DataPlant::PlantLoop(PlantLoopNum).LoopSide(LoopSideNum).FlowLock != 0 && !DataGlobals::WarmupFlag) {
                ++this->ChillerEIRFTError;
                ShowRecurringWarningErrorAtEnd("CHILLER:ELECTRIC:REFORMULATEDEIR \"" + this->Name +
                                                   "\": Chiller EIR as a Function of Temperature curve output is negative warning continues...",
                                               this->ChillerEIRFTErrorIndex,
                                               this->ChillerEIRFT,
                                               this->ChillerEIRFT);
            }
        }

        if (this->PartLoadCurveType == PLR::LeavingCondenserWaterTemperature) {
            this->ChillerEIRFPLR = CurveManager::CurveValue(this->ChillerEIRFPLRIndex, this->CondOutletTemp, this->ChillerPartLoadRatio);
        } else if (this->PartLoadCurveType == PLR::Lift) {

            // Chiller lift  [C]
            Real64 ChillerLift = this->CondOutletTemp - this->EvapOutletTemp;

            // Deviation of leaving chilled water temperature from the reference condition
            Real64 ChillerTdev = std::abs(this->EvapOutletTemp - this->TempRefEvapOut);

            // Chiller lift under the reference condition  [C]
            Real64 ChillerLiftRef = this->TempRefCondOut - this->TempRefEvapOut;

            if (ChillerLiftRef <= 0) ChillerLiftRef = 35 - 6.67;

            // Normalized chiller lift
            Real64 ChillerLiftNom = ChillerLift / ChillerLiftRef;

            // Normalized ChillerTdev
            Real64 ChillerTdevNom = ChillerTdev / ChillerLiftRef;

            this->ChillerEIRFPLR = CurveManager::CurveValue(this->ChillerEIRFPLRIndex, ChillerLiftNom, this->ChillerPartLoadRatio, ChillerTdevNom);
        }

        if (this->ChillerEIRFPLR < 0.0) {
            if (this->ChillerEIRFPLRError < 1 && DataPlant::PlantLoop(PlantLoopNum).LoopSide(LoopSideNum).FlowLock != 0 && !DataGlobals::WarmupFlag) {
                ++this->ChillerEIRFPLRError;
                ShowWarningError("CHILLER:ELECTRIC:REFORMULATEDEIR \"" + this->Name + "\":");
                ShowContinueError(" Chiller EIR as a function of PLR and condenser water temperature curve output is negative (" +
                                  General::RoundSigDigits(this->ChillerEIRFPLR, 3) + ").");
                ShowContinueError(" Negative value occurs using a part-load ratio of " + General::RoundSigDigits(this->ChillerPartLoadRatio, 3) +
                                  " and a Condenser Leaving Temp of " + General::RoundSigDigits(this->CondOutletTemp, 1) + " C.");
                ShowContinueErrorTimeStamp(" Resetting curve output to zero and continuing simulation.");
            } else if (DataPlant::PlantLoop(PlantLoopNum).LoopSide(LoopSideNum).FlowLock != 0 && !DataGlobals::WarmupFlag) {
                ++this->ChillerEIRFPLRError;
                ShowRecurringWarningErrorAtEnd("CHILLER:ELECTRIC:REFORMULATEDEIR \"" + this->Name +
                                                   "\": Chiller EIR as a function of PLR curve output is negative warning continues...",
                                               this->ChillerEIRFPLRErrorIndex,
                                               this->ChillerEIRFPLR,
                                               this->ChillerEIRFPLR);
            }
        }
    }

} // namespace ChillerReformulatedEIR

} // namespace EnergyPlus
