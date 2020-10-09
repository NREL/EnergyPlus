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

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EnergyPlus/Autosizing/Base.hh>
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/ChillerAbsorption.hh>
#include <EnergyPlus/DataBranchAirLoopPlant.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/EMSManager.hh>
#include <EnergyPlus/FaultsManager.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/Plant/PlantLocation.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

namespace ChillerAbsorption {

    // MODULE INFORMATION:
    //       AUTHOR         Dan Fisher
    //       DATE WRITTEN   Nov. 2000
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // This module simulates the performance of the BLAST
    // absorbers.

    // METHODOLOGY EMPLOYED:
    // Once the PlantLoopManager determines that the BLAST absorber
    // is available to meet a loop cooling demand, it calls SimBLAST
    // absorber which in turn calls the appropriate Absorption Chiller model.
    // All Absorption Chiller models are based on a polynomial fit of Absorber
    // performance data.

    // REFERENCES:
    // 1. BLAST Users Manual

    // OTHER NOTES:
    // The Absorber program from the BLAST family of software can be used
    // to generate the coefficients for the model.

    int constexpr waterIndex(1);
    const char * calcChillerAbsorption("CALC Chiller:Absorption ");
    const char * moduleObjectType("Chiller:Absorption");

    const char * fluidNameWater = "WATER";
    const char * fluidNameSteam = "STEAM";

    PlantComponent *BLASTAbsorberSpecs::factory(EnergyPlusData &state, std::string const &objectName)
    {
        // Process the input data
        if (state.dataChillerAbsorber->getInput) {
            GetBLASTAbsorberInput(state);
            state.dataChillerAbsorber->getInput = false;
        }
        // Now look for this particular object
        for (auto &thisAbs : state.dataChillerAbsorber->absorptionChillers) {
            if (thisAbs.Name == objectName) {
                return &thisAbs;
            }
        }
        // If we didn't find it, fatal
        ShowFatalError("LocalBlastAbsorberFactory: Error getting inputs for object named: " + objectName); // LCOV_EXCL_LINE
        // Shut up the compiler
        return nullptr; // LCOV_EXCL_LINE
    }

    void BLASTAbsorberSpecs::simulate(EnergyPlusData &state, const PlantLocation &calledFromLocation, bool FirstHVACIteration, Real64 &CurLoad, bool RunFlag)
    {

        this->EquipFlowCtrl = DataPlant::PlantLoop(calledFromLocation.loopNum).LoopSide(calledFromLocation.loopSideNum).Branch(calledFromLocation.branchNum).Comp(calledFromLocation.compNum).FlowCtrl;

        if (calledFromLocation.loopNum == this->CWLoopNum) {
            // called from dominant chilled water connection loop side

            // Calculate Load
            this->initialize(state, RunFlag, CurLoad);
            this->calculate(state, CurLoad, RunFlag);
            this->updateRecords(CurLoad, RunFlag);

        } else if (calledFromLocation.loopNum == this->CDLoopNum) {
            // Called from non-dominant condenser water connection loop side
            PlantUtilities::UpdateChillerComponentCondenserSide(state,
                                                                calledFromLocation.loopNum,
                                                                calledFromLocation.loopSideNum,
                                                                DataPlant::TypeOf_Chiller_Absorption,
                                                                this->CondInletNodeNum,
                                                                this->CondOutletNodeNum,
                                                                this->Report.QCond,
                                                                this->Report.CondInletTemp,
                                                                this->Report.CondOutletTemp,
                                                                this->Report.Condmdot,
                                                                FirstHVACIteration);

        } else if (calledFromLocation.loopNum == this->GenLoopNum) {
            // Called from non-dominant generator hot water or steam connection loop side
            PlantUtilities::UpdateAbsorberChillerComponentGeneratorSide(calledFromLocation.loopNum,
                                                                        calledFromLocation.loopSideNum,
                                                                        DataPlant::TypeOf_Chiller_Absorption,
                                                                        this->GeneratorInletNodeNum,
                                                                        this->GeneratorOutletNodeNum,
                                                                        this->GenHeatSourceType,
                                                                        this->Report.QGenerator,
                                                                        this->Report.SteamMdot,
                                                                        FirstHVACIteration);

        } else {
            ShowFatalError("SimBLASTAbsorber: Invalid LoopNum passed=" + General::TrimSigDigits(calledFromLocation.loopNum) +
                           ", Unit name=" + this->Name + ", stored chilled water loop=" + General::TrimSigDigits(this->CWLoopNum) +
                           ", stored condenser water loop=" + General::TrimSigDigits(this->CDLoopNum) +
                           ", stored generator loop=" + General::TrimSigDigits(this->GenLoopNum));
        }
    }

    void BLASTAbsorberSpecs::onInitLoopEquip(EnergyPlusData &state, const PlantLocation &calledFromLocation)
    {
        bool runFlag = true;
        Real64 myLoad = 0.0;

        this->initialize(state, runFlag, myLoad);

        if (calledFromLocation.loopNum == this->CWLoopNum) {
            this->sizeChiller(state);
        }
    }

    void BLASTAbsorberSpecs::getDesignCapacities(EnergyPlusData &state, const PlantLocation &calledFromLocation, Real64 &MaxLoad, Real64 &MinLoad, Real64 &OptLoad)
    {
        if (calledFromLocation.loopNum == this->CWLoopNum) {
            this->sizeChiller(state);
            MinLoad = this->NomCap * this->MinPartLoadRat;
            MaxLoad = this->NomCap * this->MaxPartLoadRat;
            OptLoad = this->NomCap * this->OptPartLoadRat;
        } else {
            MinLoad = 0.0;
            MaxLoad = 0.0;
            OptLoad = 0.0;
        }
    }

    void BLASTAbsorberSpecs::getSizingFactor(Real64 &sizFac)
    {
        sizFac = this->SizFac;
    }

    void BLASTAbsorberSpecs::getDesignTemperatures(Real64 &tempDesCondIn, Real64 &EP_UNUSED(TempDesEvapOut))
    {
        tempDesCondIn = this->TempDesCondIn;
    }

    void GetBLASTAbsorberInput(EnergyPlusData &state)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Dan Fisher
        //       DATE WRITTEN:    April 1998
        //       MODIFIED:        R. Raustad May 2008 - added generator nodes

        // PURPOSE OF THIS SUBROUTINE:
        // This routine will get the input
        // required by the BLAST Absorption chiller models as shown below:

        // METHODOLOGY EMPLOYED:
        // EnergyPlus input processor

        static std::string const RoutineName("GetBLASTAbsorberInput: "); // include trailing blank space

        int AbsorberNum; // Absorber counter
        int NumAlphas;   // Number of elements in the alpha array
        int NumNums;     // Number of elements in the numeric array
        int IOStat;      // IO Status when calling get input subroutine
        bool ErrorsFound(false);

        DataIPShortCuts::cCurrentModuleObject = moduleObjectType;

        state.dataChillerAbsorber->numAbsorbers = inputProcessor->getNumObjectsFound(DataIPShortCuts::cCurrentModuleObject);

        if (state.dataChillerAbsorber->numAbsorbers <= 0) {
            ShowSevereError("No " + DataIPShortCuts::cCurrentModuleObject + " equipment specified in input file");
            // See if load distribution manager has already gotten the input
            ErrorsFound = true;
        }

        if (allocated(state.dataChillerAbsorber->absorptionChillers)) return;

        state.dataChillerAbsorber->absorptionChillers.allocate(state.dataChillerAbsorber->numAbsorbers);

        // LOAD ARRAYS WITH BLAST CURVE FIT Absorber DATA
        for (AbsorberNum = 1; AbsorberNum <= state.dataChillerAbsorber->numAbsorbers; ++AbsorberNum) {
            inputProcessor->getObjectItem(state,
                                          DataIPShortCuts::cCurrentModuleObject,
                                          AbsorberNum,
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
            GlobalNames::VerifyUniqueChillerName(
                DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), ErrorsFound, DataIPShortCuts::cCurrentModuleObject + " Name");

            auto &thisChiller = state.dataChillerAbsorber->absorptionChillers(AbsorberNum);
            thisChiller.Name = DataIPShortCuts::cAlphaArgs(1);
            thisChiller.NomCap = DataIPShortCuts::rNumericArgs(1);
            if (thisChiller.NomCap == DataSizing::AutoSize) {
                thisChiller.NomCapWasAutoSized = true;
            }
            thisChiller.NomPumpPower = DataIPShortCuts::rNumericArgs(2);
            if (thisChiller.NomPumpPower == DataSizing::AutoSize) {
                thisChiller.NomPumpPowerWasAutoSized = true;
            }
            if (DataIPShortCuts::rNumericArgs(1) == 0.0) {
                ShowSevereError("Invalid " + DataIPShortCuts::cNumericFieldNames(1) + '=' +
                                General::RoundSigDigits(DataIPShortCuts::rNumericArgs(1), 2));
                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                ErrorsFound = true;
            }
            // Assign Node Numbers to specified nodes
            thisChiller.EvapInletNodeNum = NodeInputManager::GetOnlySingleNode(state, DataIPShortCuts::cAlphaArgs(2),
                                                                                              ErrorsFound,
                                                                                              DataIPShortCuts::cCurrentModuleObject,
                                                                                              DataIPShortCuts::cAlphaArgs(1),
                                                                                              DataLoopNode::NodeType_Water,
                                                                                              DataLoopNode::NodeConnectionType_Inlet,
                                                                                              1,
                                                                                              DataLoopNode::ObjectIsNotParent);
            thisChiller.EvapOutletNodeNum = NodeInputManager::GetOnlySingleNode(state, DataIPShortCuts::cAlphaArgs(3),
                                                                                               ErrorsFound,
                                                                                               DataIPShortCuts::cCurrentModuleObject,
                                                                                               DataIPShortCuts::cAlphaArgs(1),
                                                                                               DataLoopNode::NodeType_Water,
                                                                                               DataLoopNode::NodeConnectionType_Outlet,
                                                                                               1,
                                                                                               DataLoopNode::ObjectIsNotParent);
            BranchNodeConnections::TestCompSet(DataIPShortCuts::cCurrentModuleObject,
                                               DataIPShortCuts::cAlphaArgs(1),
                                               DataIPShortCuts::cAlphaArgs(2),
                                               DataIPShortCuts::cAlphaArgs(3),
                                               "Chilled Water Nodes");

            thisChiller.CondInletNodeNum = NodeInputManager::GetOnlySingleNode(state, DataIPShortCuts::cAlphaArgs(4),
                                                                                              ErrorsFound,
                                                                                              DataIPShortCuts::cCurrentModuleObject,
                                                                                              DataIPShortCuts::cAlphaArgs(1),
                                                                                              DataLoopNode::NodeType_Water,
                                                                                              DataLoopNode::NodeConnectionType_Inlet,
                                                                                              2,
                                                                                              DataLoopNode::ObjectIsNotParent);
            thisChiller.CondOutletNodeNum = NodeInputManager::GetOnlySingleNode(state, DataIPShortCuts::cAlphaArgs(5),
                                                                                               ErrorsFound,
                                                                                               DataIPShortCuts::cCurrentModuleObject,
                                                                                               DataIPShortCuts::cAlphaArgs(1),
                                                                                               DataLoopNode::NodeType_Water,
                                                                                               DataLoopNode::NodeConnectionType_Outlet,
                                                                                               2,
                                                                                               DataLoopNode::ObjectIsNotParent);
            BranchNodeConnections::TestCompSet(DataIPShortCuts::cCurrentModuleObject,
                                               DataIPShortCuts::cAlphaArgs(1),
                                               DataIPShortCuts::cAlphaArgs(4),
                                               DataIPShortCuts::cAlphaArgs(5),
                                               "Condenser (not tested) Nodes");

            if (NumAlphas > 8) {
                if (UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(9), "HotWater") ||
                    UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(9), "HotWater")) {
                    thisChiller.GenHeatSourceType = DataLoopNode::NodeType_Water;
                } else if (UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(9), fluidNameSteam) || DataIPShortCuts::cAlphaArgs(9).empty()) {
                    thisChiller.GenHeatSourceType = DataLoopNode::NodeType_Steam;
                } else {
                    ShowSevereError("Invalid " + DataIPShortCuts::cAlphaFieldNames(9) + '=' + DataIPShortCuts::cAlphaArgs(9));
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                    ShowContinueError("...Generator heat source type must be Steam or Hot Water.");
                    ErrorsFound = true;
                }
            } else {
                thisChiller.GenHeatSourceType = DataLoopNode::NodeType_Steam;
            }

            if (!DataIPShortCuts::lAlphaFieldBlanks(6) && !DataIPShortCuts::lAlphaFieldBlanks(7)) {
                thisChiller.GenInputOutputNodesUsed = true;
                if (thisChiller.GenHeatSourceType == DataLoopNode::NodeType_Water) {
                    thisChiller.GeneratorInletNodeNum = NodeInputManager::GetOnlySingleNode(state, DataIPShortCuts::cAlphaArgs(6),
                                                                                                           ErrorsFound,
                                                                                                           DataIPShortCuts::cCurrentModuleObject,
                                                                                                           DataIPShortCuts::cAlphaArgs(1),
                                                                                                           DataLoopNode::NodeType_Water,
                                                                                                           DataLoopNode::NodeConnectionType_Inlet,
                                                                                                           3,
                                                                                                           DataLoopNode::ObjectIsNotParent);
                    thisChiller.GeneratorOutletNodeNum = NodeInputManager::GetOnlySingleNode(state, DataIPShortCuts::cAlphaArgs(7),
                                                                                                            ErrorsFound,
                                                                                                            DataIPShortCuts::cCurrentModuleObject,
                                                                                                            DataIPShortCuts::cAlphaArgs(1),
                                                                                                            DataLoopNode::NodeType_Water,
                                                                                                            DataLoopNode::NodeConnectionType_Outlet,
                                                                                                            3,
                                                                                                            DataLoopNode::ObjectIsNotParent);
                    BranchNodeConnections::TestCompSet(DataIPShortCuts::cCurrentModuleObject,
                                                       DataIPShortCuts::cAlphaArgs(1),
                                                       DataIPShortCuts::cAlphaArgs(6),
                                                       DataIPShortCuts::cAlphaArgs(7),
                                                       "Hot Water Nodes");
                } else {
                    thisChiller.SteamFluidIndex = FluidProperties::FindRefrigerant(state, fluidNameSteam);
                    thisChiller.GeneratorInletNodeNum = NodeInputManager::GetOnlySingleNode(state, DataIPShortCuts::cAlphaArgs(6),
                                                                                                           ErrorsFound,
                                                                                                           DataIPShortCuts::cCurrentModuleObject,
                                                                                                           DataIPShortCuts::cAlphaArgs(1),
                                                                                                           DataLoopNode::NodeType_Steam,
                                                                                                           DataLoopNode::NodeConnectionType_Inlet,
                                                                                                           3,
                                                                                                           DataLoopNode::ObjectIsNotParent);
                    thisChiller.GeneratorOutletNodeNum = NodeInputManager::GetOnlySingleNode(state, DataIPShortCuts::cAlphaArgs(7),
                                                                                                            ErrorsFound,
                                                                                                            DataIPShortCuts::cCurrentModuleObject,
                                                                                                            DataIPShortCuts::cAlphaArgs(1),
                                                                                                            DataLoopNode::NodeType_Steam,
                                                                                                            DataLoopNode::NodeConnectionType_Outlet,
                                                                                                            3,
                                                                                                            DataLoopNode::ObjectIsNotParent);
                    BranchNodeConnections::TestCompSet(DataIPShortCuts::cCurrentModuleObject,
                                                       DataIPShortCuts::cAlphaArgs(1),
                                                       DataIPShortCuts::cAlphaArgs(6),
                                                       DataIPShortCuts::cAlphaArgs(7),
                                                       "Steam Nodes");
                }
            } else if ((DataIPShortCuts::lAlphaFieldBlanks(6) && !DataIPShortCuts::lAlphaFieldBlanks(7)) ||
                       (!DataIPShortCuts::lAlphaFieldBlanks(6) && DataIPShortCuts::lAlphaFieldBlanks(7))) {
                ShowSevereError(DataIPShortCuts::cCurrentModuleObject + ", Name=" + DataIPShortCuts::cAlphaArgs(1));
                ShowContinueError("...Generator fluid nodes must both be entered (or both left blank).");
                ShowContinueError("..." + DataIPShortCuts::cAlphaFieldNames(6) + " = " + DataIPShortCuts::cAlphaArgs(6));
                ShowContinueError("..." + DataIPShortCuts::cAlphaFieldNames(7) + " = " + DataIPShortCuts::cAlphaArgs(7));
                ErrorsFound = true;
            } else {
                if (thisChiller.GenHeatSourceType == DataLoopNode::NodeType_Water) {
                    ShowWarningError(DataIPShortCuts::cCurrentModuleObject + ", Name=" + DataIPShortCuts::cAlphaArgs(1));
                    ShowContinueError("...Generator fluid type must be Steam if generator inlet/outlet nodes are blank.");
                    ShowContinueError("...Generator fluid type is set to Steam and the simulation continues.");
                    thisChiller.GenHeatSourceType = DataLoopNode::NodeType_Steam;
                }
            }

            // Get remaining data
            thisChiller.MinPartLoadRat = DataIPShortCuts::rNumericArgs(3);
            thisChiller.MaxPartLoadRat = DataIPShortCuts::rNumericArgs(4);
            thisChiller.OptPartLoadRat = DataIPShortCuts::rNumericArgs(5);
            thisChiller.TempDesCondIn = DataIPShortCuts::rNumericArgs(6);
            thisChiller.EvapVolFlowRate = DataIPShortCuts::rNumericArgs(7);
            if (thisChiller.EvapVolFlowRate == DataSizing::AutoSize) {
                thisChiller.EvapVolFlowRateWasAutoSized = true;
            }
            thisChiller.CondVolFlowRate = DataIPShortCuts::rNumericArgs(8);
            if (thisChiller.CondVolFlowRate == DataSizing::AutoSize) {
                thisChiller.CondVolFlowRateWasAutoSized = true;
            }
            thisChiller.SteamLoadCoef(1) = DataIPShortCuts::rNumericArgs(9);
            thisChiller.SteamLoadCoef(2) = DataIPShortCuts::rNumericArgs(10);
            thisChiller.SteamLoadCoef(3) = DataIPShortCuts::rNumericArgs(11);
            thisChiller.PumpPowerCoef(1) = DataIPShortCuts::rNumericArgs(12);
            thisChiller.PumpPowerCoef(2) = DataIPShortCuts::rNumericArgs(13);
            thisChiller.PumpPowerCoef(3) = DataIPShortCuts::rNumericArgs(14);
            thisChiller.TempLowLimitEvapOut = DataIPShortCuts::rNumericArgs(15);

            {
                auto const SELECT_CASE_var(DataIPShortCuts::cAlphaArgs(8));
                if (SELECT_CASE_var == "CONSTANTFLOW") {
                    thisChiller.FlowMode = DataPlant::FlowMode::CONSTANT;
                } else if (SELECT_CASE_var == "LEAVINGSETPOINTMODULATED") {
                    thisChiller.FlowMode = DataPlant::FlowMode::LEAVINGSETPOINTMODULATED;
                } else if (SELECT_CASE_var == "NOTMODULATED") {
                    thisChiller.FlowMode = DataPlant::FlowMode::NOTMODULATED;
                } else {
                    ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\",");
                    ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(8) + '=' + DataIPShortCuts::cAlphaArgs(8));
                    ShowContinueError("Available choices are ConstantFlow, NotModulated, or LeavingSetpointModulated");
                    ShowContinueError("Flow mode NotModulated is assumed and the simulation continues.");
                    thisChiller.FlowMode = DataPlant::FlowMode::NOTMODULATED;
                }
            }

            if (NumNums > 15) {
                thisChiller.GeneratorVolFlowRate = DataIPShortCuts::rNumericArgs(16);
                if (thisChiller.GeneratorVolFlowRate == DataSizing::AutoSize) {
                    thisChiller.GeneratorVolFlowRateWasAutoSized = true;
                }
            }

            if (thisChiller.GeneratorVolFlowRate == 0.0 &&
                thisChiller.GenHeatSourceType == DataLoopNode::NodeType_Water) {
                ShowSevereError("Invalid " + DataIPShortCuts::cNumericFieldNames(16) + '=' +
                                General::RoundSigDigits(DataIPShortCuts::rNumericArgs(16), 2));
                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                ShowContinueError("...Generator water flow rate must be greater than 0 when absorber generator fluid type is hot water.");
                ErrorsFound = true;
            }

            if (NumNums > 16) {
                thisChiller.GeneratorSubcool = DataIPShortCuts::rNumericArgs(17);
            } else {
                thisChiller.GeneratorSubcool = 1.0;
            }

            if (NumNums > 17) {
                thisChiller.SizFac = DataIPShortCuts::rNumericArgs(18);
            } else {
                thisChiller.SizFac = 1.0;
            }
        }

        if (ErrorsFound) {
            ShowFatalError("Errors found in processing input for " + DataIPShortCuts::cCurrentModuleObject);
        }
    }

    void BLASTAbsorberSpecs::setupOutputVars(EnergyPlusData &state)
    {
        SetupOutputVariable(state, "Chiller Electricity Rate", OutputProcessor::Unit::W, this->Report.PumpingPower, "System", "Average", this->Name);
        SetupOutputVariable(state, "Chiller Electricity Energy",
                            OutputProcessor::Unit::J,
                            this->Report.PumpingEnergy,
                            "System",
                            "Sum",
                            this->Name,
                            _,
                            "ELECTRICITY",
                            "Cooling",
                            _,
                            "Plant");
        SetupOutputVariable(state, "Chiller Evaporator Cooling Rate", OutputProcessor::Unit::W, this->Report.QEvap, "System", "Average", this->Name);
        SetupOutputVariable(state, "Chiller Evaporator Cooling Energy",
                            OutputProcessor::Unit::J,
                            this->Report.EvapEnergy,
                            "System",
                            "Sum",
                            this->Name,
                            _,
                            "ENERGYTRANSFER",
                            "CHILLERS",
                            _,
                            "Plant");
        SetupOutputVariable(state,
            "Chiller Evaporator Inlet Temperature", OutputProcessor::Unit::C, this->Report.EvapInletTemp, "System", "Average", this->Name);
        SetupOutputVariable(state,
            "Chiller Evaporator Outlet Temperature", OutputProcessor::Unit::C, this->Report.EvapOutletTemp, "System", "Average", this->Name);
        SetupOutputVariable(state, "Chiller Evaporator Mass Flow Rate", OutputProcessor::Unit::kg_s, this->Report.Evapmdot, "System", "Average", this->Name);

        SetupOutputVariable(state, "Chiller Condenser Heat Transfer Rate", OutputProcessor::Unit::W, this->Report.QCond, "System", "Average", this->Name);
        SetupOutputVariable(state, "Chiller Condenser Heat Transfer Energy",
                            OutputProcessor::Unit::J,
                            this->Report.CondEnergy,
                            "System",
                            "Sum",
                            this->Name,
                            _,
                            "ENERGYTRANSFER",
                            "HEATREJECTION",
                            _,
                            "Plant");
        SetupOutputVariable(state,
            "Chiller Condenser Inlet Temperature", OutputProcessor::Unit::C, this->Report.CondInletTemp, "System", "Average", this->Name);
        SetupOutputVariable(state,
            "Chiller Condenser Outlet Temperature", OutputProcessor::Unit::C, this->Report.CondOutletTemp, "System", "Average", this->Name);
        SetupOutputVariable(state, "Chiller Condenser Mass Flow Rate", OutputProcessor::Unit::kg_s, this->Report.Condmdot, "System", "Average", this->Name);

        if (this->GenHeatSourceType == DataLoopNode::NodeType_Water) {
            SetupOutputVariable(state,
                "Chiller Hot Water Consumption Rate", OutputProcessor::Unit::W, this->Report.QGenerator, "System", "Average", this->Name);
            SetupOutputVariable(state, "Chiller Source Hot Water Energy",
                                OutputProcessor::Unit::J,
                                this->Report.GeneratorEnergy,
                                "System",
                                "Sum",
                                this->Name,
                                _,
                                "PLANTLOOPHEATINGDEMAND",
                                "CHILLERS",
                                _,
                                "Plant");
        } else {
            if (this->GenInputOutputNodesUsed) {
                SetupOutputVariable(state, "Chiller Source Steam Rate", OutputProcessor::Unit::W, this->Report.QGenerator, "System", "Average", this->Name);
                SetupOutputVariable(state, "Chiller Source Steam Energy",
                                    OutputProcessor::Unit::J,
                                    this->Report.GeneratorEnergy,
                                    "System",
                                    "Sum",
                                    this->Name,
                                    _,
                                    "PLANTLOOPHEATINGDEMAND",
                                    "CHILLERS",
                                    _,
                                    "Plant");
            } else {
                SetupOutputVariable(state, "Chiller Source Steam Rate", OutputProcessor::Unit::W, this->Report.QGenerator, "System", "Average", this->Name);
                SetupOutputVariable(state, "Chiller Source Steam Energy",
                                    OutputProcessor::Unit::J,
                                    this->Report.GeneratorEnergy,
                                    "System",
                                    "Sum",
                                    this->Name,
                                    _,
                                    fluidNameSteam,
                                    "Cooling",
                                    _,
                                    "Plant");
            }
        }

        SetupOutputVariable(state, "Chiller COP", OutputProcessor::Unit::W_W, this->Report.ActualCOP, "System", "Average", this->Name);

        if (DataGlobals::AnyEnergyManagementSystemInModel) {
            SetupEMSInternalVariable("Chiller Nominal Capacity", this->Name, "[W]", this->NomCap);
        }
    }

    void BLASTAbsorberSpecs::initialize(EnergyPlusData &state,
                                        bool RunFlag, // TRUE when chiller operating
                                        Real64 MyLoad)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad
        //       DATE WRITTEN   September 2009
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for initializations of the Electric Chiller components

        // METHODOLOGY EMPLOYED:
        // Uses the status flags to trigger initializations.

        static std::string const RoutineName("InitBLASTAbsorberModel");

        // Init more variables
        if (this->MyOneTimeFlag) {

            this->setupOutputVars(state);

            // Locate the chillers on the plant loops for later usage
            bool errFlag = false;
            PlantUtilities::ScanPlantLoopsForObject(state,
                                                    this->Name,
                                                    DataPlant::TypeOf_Chiller_Absorption,
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
            if (this->CondInletNodeNum > 0) {
                PlantUtilities::ScanPlantLoopsForObject(state,
                                                        this->Name,
                                                        DataPlant::TypeOf_Chiller_Absorption,
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
                    this->CWLoopNum, this->CWLoopSideNum, this->CDLoopNum, this->CDLoopSideNum, DataPlant::TypeOf_Chiller_Absorption, true);
            }
            if (this->GeneratorInletNodeNum > 0) {
                PlantUtilities::ScanPlantLoopsForObject(state,
                                                        this->Name,
                                                        DataPlant::TypeOf_Chiller_Absorption,
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
                    this->CWLoopNum, this->CWLoopSideNum, this->GenLoopNum, this->GenCompNum, DataPlant::TypeOf_Chiller_Absorption, true);
            }

            // Fill in connection data
            if ((this->CondInletNodeNum > 0) && (this->GeneratorInletNodeNum > 0)) {
                PlantUtilities::InterConnectTwoPlantLoopSides(
                    this->CDLoopNum, this->CDLoopSideNum, this->GenLoopNum, this->GenCompNum, DataPlant::TypeOf_Chiller_Absorption, false);
            }
            if (errFlag) {
                ShowFatalError("InitBLASTAbsorberModel: Program terminated due to previous condition(s).");
            }

            if (this->FlowMode == DataPlant::FlowMode::CONSTANT) {
                DataPlant::PlantLoop(this->CWLoopNum).LoopSide(this->CWLoopSideNum).Branch(this->CWBranchNum).Comp(this->CWCompNum).FlowPriority =
                    DataPlant::LoopFlowStatus_NeedyIfLoopOn;
            }

            if (this->FlowMode == DataPlant::FlowMode::LEAVINGSETPOINTMODULATED) {
                DataPlant::PlantLoop(this->CWLoopNum).LoopSide(this->CWLoopSideNum).Branch(this->CWBranchNum).Comp(this->CWCompNum).FlowPriority =
                    DataPlant::LoopFlowStatus_NeedyIfLoopOn;

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
                        DataLoopNode::NodeSetpointCheck(this->EvapOutletNodeNum).needsSetpointChecking = false;
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
                    DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPoint =
                        DataLoopNode::Node(DataPlant::PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPoint;
                    DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPointHi =
                        DataLoopNode::Node(DataPlant::PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPointHi;
                }
            }

            this->MyOneTimeFlag = false;
        }

        if (this->MyEnvrnFlag && DataGlobals::BeginEnvrnFlag && (DataPlant::PlantFirstSizesOkayToFinalize)) {

            Real64 rho = FluidProperties::GetDensityGlycol(state,
                                                           DataPlant::PlantLoop(this->CWLoopNum).FluidName,
                                                           DataGlobalConstants::CWInitConvTemp(),
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

            rho = FluidProperties::GetDensityGlycol(state,
                                                    DataPlant::PlantLoop(this->CDLoopNum).FluidName,
                                                    DataGlobalConstants::CWInitConvTemp(),
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
            DataLoopNode::Node(this->CondInletNodeNum).Temp = this->TempDesCondIn;

            if (this->GeneratorInletNodeNum > 0) {

                if (this->GenHeatSourceType == DataLoopNode::NodeType_Water) {
                    rho = FluidProperties::GetDensityGlycol(state,
                                                            DataPlant::PlantLoop(this->GenLoopNum).FluidName,
                                                            DataGlobalConstants::HWInitConvTemp(),
                                                            DataPlant::PlantLoop(this->GenLoopNum).FluidIndex,
                                                            RoutineName);

                    this->GenMassFlowRateMax = rho * this->GeneratorVolFlowRate;
                } else if (this->GenHeatSourceType == DataLoopNode::NodeType_Steam) {

                    this->QGenerator = (this->SteamLoadCoef(1) + this->SteamLoadCoef(2) + this->SteamLoadCoef(3)) * this->NomCap;

                    // dry enthalpy of steam (quality = 1)
                    Real64 EnthSteamOutDry = FluidProperties::GetSatEnthalpyRefrig(state,
                                                                                   fluidNameSteam,
                                                                                   DataLoopNode::Node(this->GeneratorInletNodeNum).Temp,
                                                                                   1.0,
                                                                                   this->SteamFluidIndex,
                                                                                   calcChillerAbsorption + this->Name);

                    // wet enthalpy of steam (quality = 0)
                    Real64 EnthSteamOutWet = FluidProperties::GetSatEnthalpyRefrig(state,
                                                                                   fluidNameSteam,
                                                                                   DataLoopNode::Node(this->GeneratorInletNodeNum).Temp,
                                                                                   0.0,
                                                                                   this->SteamFluidIndex,
                                                                                   calcChillerAbsorption + this->Name);
                    Real64 SteamDeltaT = this->GeneratorSubcool;
                    Real64 SteamOutletTemp = DataLoopNode::Node(this->GeneratorInletNodeNum).Temp - SteamDeltaT;
                    Real64 HfgSteam = EnthSteamOutDry - EnthSteamOutWet;
                    Real64 CpWater = FluidProperties::GetDensityGlycol(
                        state, fluidNameWater, SteamOutletTemp, const_cast<int &>(waterIndex), calcChillerAbsorption + this->Name);
                    this->GenMassFlowRateMax = this->QGenerator / (HfgSteam + CpWater * SteamDeltaT);
                }

                PlantUtilities::InitComponentNodes(0.0,
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
        if (!DataGlobals::BeginEnvrnFlag) {
            this->MyEnvrnFlag = true;
        }

        // every time inits

        if ((this->FlowMode == DataPlant::FlowMode::LEAVINGSETPOINTMODULATED) && this->ModulatedFlowSetToLoop) {
            // fix for clumsy old input that worked because loop setpoint was spread.
            //  could be removed with transition, testing , model change, period of being obsolete.
            DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPoint =
                DataLoopNode::Node(DataPlant::PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPoint;
            DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPointHi =
                DataLoopNode::Node(DataPlant::PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPointHi;
        }

        Real64 mdotEvap; // local fluid mass flow rate thru evaporator
        Real64 mdotCond; // local fluid mass flow rate thru condenser
        Real64 mdotGen;  // local fluid mass flow rate thru generator

        if ((MyLoad < 0.0) && RunFlag) {
            mdotEvap = this->EvapMassFlowRateMax;
            mdotCond = this->CondMassFlowRateMax;
            mdotGen = this->GenMassFlowRateMax;
        } else {
            mdotEvap = 0.0;
            mdotCond = 0.0;
            mdotGen = 0.0;
        }

        PlantUtilities::SetComponentFlowRate(
            mdotEvap, this->EvapInletNodeNum, this->EvapOutletNodeNum, this->CWLoopNum, this->CWLoopSideNum, this->CWBranchNum, this->CWCompNum);

        PlantUtilities::SetComponentFlowRate(
            mdotCond, this->CondInletNodeNum, this->CondOutletNodeNum, this->CDLoopNum, this->CDLoopSideNum, this->CDBranchNum, this->CDCompNum);

        if (this->GeneratorInletNodeNum > 0) {

            PlantUtilities::SetComponentFlowRate(mdotGen,
                                                 this->GeneratorInletNodeNum,
                                                 this->GeneratorOutletNodeNum,
                                                 this->GenLoopNum,
                                                 this->GenLoopSideNum,
                                                 this->GenBranchNum,
                                                 this->GenCompNum);
        }
    }

    void BLASTAbsorberSpecs::sizeChiller(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   March 2008
        //       MODIFIED:      R. Raustad May 2008 - added generator node sizing
        //                      November 2013 Daeho Kang, add component sizing table entries
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for sizing Constant COP Chiller Components for which capacities and flow rates
        // have not been specified in the input.

        // METHODOLOGY EMPLOYED:
        // Obtains evaporator flow rate from the plant sizing array. Calculates nominal capacity from
        // the evaporator flow rate and the chilled water loop design delta T. The condenser flow rate
        // is calculated from the nominal capacity, the COP, and the condenser loop design delta T.

        //        Real64 SteamMassFlowRate; // steam mass flow rate through generator

        static std::string const RoutineName("SizeAbsorpChiller");
        static std::string const RoutineNameLong("SizeAbsorptionChiller");

        int PltSizSteamNum(0);   // Plant Sizing index for steam heating loop
        int PltSizHeatingNum(0); // Plant Sizing index for how water heating loop
        bool ErrorsFound(false); // If errors detected in input
        bool LoopErrorsFound;

        // nominal energy input ratio (steam or hot water)
        Real64 SteamInputRatNom = this->SteamLoadCoef(1) + this->SteamLoadCoef(2) + this->SteamLoadCoef(3);
        // init local temporary version in case of partial/mixed autosizing

        // local nominal capacity cooling power
        Real64 tmpNomCap = this->NomCap;

        // local evaporator design volume flow rate
        Real64 tmpEvapVolFlowRate = this->EvapVolFlowRate;

        // local condenser design volume flow rate
        Real64 tmpCondVolFlowRate = this->CondVolFlowRate;

        // local generator design volume flow rate
        Real64 tmpGeneratorVolFlowRate = this->GeneratorVolFlowRate;

        // find the appropriate Plant Sizing object
        int PltSizNum = DataPlant::PlantLoop(this->CWLoopNum).PlantSizNum;
        int PltSizCondNum = DataPlant::PlantLoop(this->CDLoopNum).PlantSizNum;

        if (this->GenHeatSourceType == DataLoopNode::NodeType_Steam) {
            if (this->GeneratorInletNodeNum > 0 && this->GeneratorOutletNodeNum > 0) {
                PltSizSteamNum = PlantUtilities::MyPlantSizingIndex(
                    moduleObjectType, this->Name, this->GeneratorInletNodeNum, this->GeneratorOutletNodeNum, LoopErrorsFound);
            } else {
                for (int PltSizIndex = 1; PltSizIndex <= DataSizing::NumPltSizInput; ++PltSizIndex) {
                    if (DataSizing::PlantSizData(PltSizIndex).LoopType == DataSizing::SteamLoop) {
                        PltSizSteamNum = PltSizIndex;
                    }
                }
            }
        } else {
            if (this->GeneratorInletNodeNum > 0 && this->GeneratorOutletNodeNum > 0) {
                PltSizHeatingNum = PlantUtilities::MyPlantSizingIndex(
                    moduleObjectType, this->Name, this->GeneratorInletNodeNum, this->GeneratorOutletNodeNum, LoopErrorsFound);
            } else {
                for (int PltSizIndex = 1; PltSizIndex <= DataSizing::NumPltSizInput; ++PltSizIndex) {
                    if (DataSizing::PlantSizData(PltSizIndex).LoopType == DataSizing::HeatingLoop) {
                        PltSizHeatingNum = PltSizIndex;
                    }
                }
            }
        }

        if (PltSizNum > 0) {
            if (DataSizing::PlantSizData(PltSizNum).DesVolFlowRate >= DataHVACGlobals::TimeStepSys) {

                Real64 Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                                   DataPlant::PlantLoop(this->CWLoopNum).FluidName,
                                                                   DataGlobalConstants::CWInitConvTemp(),
                                                                   DataPlant::PlantLoop(this->CWLoopNum).FluidIndex,
                                                                   RoutineName);

                Real64 rho = FluidProperties::GetDensityGlycol(state,
                                                               DataPlant::PlantLoop(this->CWLoopNum).FluidName,
                                                               DataGlobalConstants::CWInitConvTemp(),
                                                               DataPlant::PlantLoop(this->CWLoopNum).FluidIndex,
                                                               RoutineName);
                tmpNomCap = Cp * rho * DataSizing::PlantSizData(PltSizNum).DeltaT * DataSizing::PlantSizData(PltSizNum).DesVolFlowRate * this->SizFac;
                if (!this->NomCapWasAutoSized) tmpNomCap = this->NomCap;
            } else {
                if (this->NomCapWasAutoSized) tmpNomCap = 0.0;
            }
            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                if (this->NomCapWasAutoSized) {
                    this->NomCap = tmpNomCap;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(moduleObjectType, this->Name, "Design Size Nominal Capacity [W]", tmpNomCap);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(moduleObjectType, this->Name, "Initial Design Size Nominal Capacity [W]", tmpNomCap);
                    }
                } else {
                    if (this->NomCap > 0.0 && tmpNomCap > 0.0) {
                        Real64 NomCapUser = this->NomCap;
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            BaseSizer::reportSizerOutput(moduleObjectType,
                                                                    this->Name,
                                                                    "Design Size Nominal Capacity [W]",
                                                                    tmpNomCap,
                                                                    "User-Specified Nominal Capacity [W]",
                                                                    NomCapUser);
                            if (DataGlobals::DisplayExtraWarnings) {
                                if ((std::abs(tmpNomCap - NomCapUser) / NomCapUser) > DataSizing::AutoVsHardSizingThreshold) {
                                    ShowMessage("SizeChillerAbsorption: Potential issue with equipment sizing for " + this->Name);
                                    ShowContinueError("User-Specified Nominal Capacity of " + General::RoundSigDigits(NomCapUser, 2) + " [W]");
                                    ShowContinueError("differs from Design Size Nominal Capacity of " + General::RoundSigDigits(tmpNomCap, 2) +
                                                      " [W]");
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
                ShowSevereError("Autosizing of Absorption Chiller nominal capacity requires a loop Sizing:Plant object");
                ShowContinueError("Occurs in Chiller:Absorption object=" + this->Name);
                ErrorsFound = true;
            }
            if (!this->NomCapWasAutoSized && DataPlant::PlantFinalSizesOkayToReport && this->NomCap > 0.0) {
                BaseSizer::reportSizerOutput(moduleObjectType, this->Name, "User-Specified Nominal Capacity [W]", this->NomCap);
            }
        }

        // local nominal pump power
        Real64 tmpNomPumpPower = 0.0045 * this->NomCap;

        if (DataPlant::PlantFirstSizesOkayToFinalize) {
            // the DOE-2 EIR for single stage absorption chiller
            if (this->NomPumpPowerWasAutoSized) {
                this->NomPumpPower = tmpNomPumpPower;
                if (DataPlant::PlantFinalSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(moduleObjectType, this->Name, "Design Size Nominal Pumping Power [W]", tmpNomPumpPower);
                }
                if (DataPlant::PlantFirstSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(
                        moduleObjectType, this->Name, "Initial Design Size Nominal Pumping Power [W]", tmpNomPumpPower);
                }
            } else {
                if (this->NomPumpPower > 0.0 && tmpNomPumpPower > 0.0) {
                    // Hardsized nominal pump power for reporting
                    Real64 NomPumpPowerUser = this->NomPumpPower;

                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(moduleObjectType,
                                                                this->Name,
                                                                "Design Size Nominal Pumping Power [W]",
                                                                tmpNomPumpPower,
                                                                "User-Specified Nominal Pumping Power [W]",
                                                                NomPumpPowerUser);
                        if (DataGlobals::DisplayExtraWarnings) {
                            if ((std::abs(tmpNomPumpPower - NomPumpPowerUser) / NomPumpPowerUser) > DataSizing::AutoVsHardSizingThreshold) {
                                ShowMessage("SizeChillerAbsorption: Potential issue with equipment sizing for " + this->Name);
                                ShowContinueError("User-Specified Nominal Pumping Power of " + General::RoundSigDigits(NomPumpPowerUser, 2) + " [W]");
                                ShowContinueError("differs from Design Size Nominal Pumping Power of " + General::RoundSigDigits(tmpNomPumpPower, 2) +
                                                  " [W]");
                                ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                                ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                            }
                        }
                    }
                    tmpNomPumpPower = NomPumpPowerUser;
                }
            }
        }

        if (PltSizNum > 0) {
            if (DataSizing::PlantSizData(PltSizNum).DesVolFlowRate >= DataHVACGlobals::TimeStepSys) {
                tmpEvapVolFlowRate = DataSizing::PlantSizData(PltSizNum).DesVolFlowRate * this->SizFac;
                if (!this->EvapVolFlowRateWasAutoSized) tmpEvapVolFlowRate = this->EvapVolFlowRate;
            } else {
                if (this->EvapVolFlowRateWasAutoSized) tmpEvapVolFlowRate = 0.0;
            }
            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                if (this->EvapVolFlowRateWasAutoSized) {
                    this->EvapVolFlowRate = tmpEvapVolFlowRate;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(
                            moduleObjectType, this->Name, "Design Size Design Chilled Water Flow Rate [m3/s]", tmpEvapVolFlowRate);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(
                            moduleObjectType, this->Name, "Initial Design Size Design Chilled Water Flow Rate [m3/s]", tmpEvapVolFlowRate);
                    }
                } else {
                    if (this->EvapVolFlowRate > 0.0 && tmpEvapVolFlowRate > 0.0) {
                        // Hardsized evaporator volume flow rate for reporting
                        Real64 EvapVolFlowRateUser = this->EvapVolFlowRate;
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            BaseSizer::reportSizerOutput(moduleObjectType,
                                                                    this->Name,
                                                                    "Design Size Design Chilled Water Flow Rate [m3/s]",
                                                                    tmpEvapVolFlowRate,
                                                                    "User-Specified Design Chilled Water Flow Rate [m3/s]",
                                                                    EvapVolFlowRateUser);
                            if (DataGlobals::DisplayExtraWarnings) {
                                if ((std::abs(tmpEvapVolFlowRate - EvapVolFlowRateUser) / EvapVolFlowRateUser) >
                                    DataSizing::AutoVsHardSizingThreshold) {
                                    ShowMessage("SizeChillerAbsorption: Potential issue with equipment sizing for " + this->Name);
                                    ShowContinueError("User-Specified Design Chilled Water Flow Rate of " +
                                                      General::RoundSigDigits(EvapVolFlowRateUser, 5) + " [m3/s]");
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
                ShowSevereError("Autosizing of Absorption Chiller evap flow rate requires a loop Sizing:Plant object");
                ShowContinueError("Occurs in CHILLER:ABSORPTION object=" + this->Name);
                ErrorsFound = true;
            }
            if (!this->EvapVolFlowRateWasAutoSized && DataPlant::PlantFinalSizesOkayToReport && this->EvapVolFlowRate > 0.0) {
                BaseSizer::reportSizerOutput(
                    moduleObjectType, this->Name, "User-Specified Design Chilled Water Flow Rate [m3/s]", this->EvapVolFlowRate);
            }
        }

        PlantUtilities::RegisterPlantCompDesignFlow(this->EvapInletNodeNum, tmpEvapVolFlowRate);

        if (PltSizCondNum > 0 && PltSizNum > 0) {
            if (this->EvapVolFlowRate >= DataHVACGlobals::TimeStepSys && tmpNomCap > 0.0) {
                //       QCondenser = QEvaporator + QGenerator + PumpingPower

                Real64 Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                                   DataPlant::PlantLoop(this->CDLoopNum).FluidName,
                                                                   this->TempDesCondIn,
                                                                   DataPlant::PlantLoop(this->CDLoopNum).FluidIndex,
                                                                   RoutineName);

                Real64 rho = FluidProperties::GetDensityGlycol(state,
                                                               DataPlant::PlantLoop(this->CDLoopNum).FluidName,
                                                               DataGlobalConstants::CWInitConvTemp(),
                                                               DataPlant::PlantLoop(this->CDLoopNum).FluidIndex,
                                                               RoutineName);
                tmpCondVolFlowRate =
                    tmpNomCap * (1.0 + SteamInputRatNom + tmpNomPumpPower / tmpNomCap) / (DataSizing::PlantSizData(PltSizCondNum).DeltaT * Cp * rho);
                if (!this->CondVolFlowRateWasAutoSized) tmpCondVolFlowRate = this->CondVolFlowRate;

            } else {
                if (this->CondVolFlowRateWasAutoSized) tmpCondVolFlowRate = 0.0;
            }
            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                if (this->CondVolFlowRateWasAutoSized) {
                    this->CondVolFlowRate = tmpCondVolFlowRate;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(
                            moduleObjectType, this->Name, "Design Size Design Condenser Water Flow Rate [m3/s]", tmpCondVolFlowRate);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(
                            moduleObjectType, this->Name, "Initial Design Size Design Condenser Water Flow Rate [m3/s]", tmpCondVolFlowRate);
                    }
                } else {
                    if (this->CondVolFlowRate > 0.0 && tmpCondVolFlowRate > 0.0) {
                        // Hardsized condenser flow rate for reporting
                        Real64 CondVolFlowRateUser = this->CondVolFlowRate;
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            BaseSizer::reportSizerOutput(moduleObjectType,
                                                                    this->Name,
                                                                    "Design Size Design Condenser Water Flow Rate [m3/s]",
                                                                    tmpCondVolFlowRate,
                                                                    "User-Specified Design Condenser Water Flow Rate [m3/s]",
                                                                    CondVolFlowRateUser);
                            if (DataGlobals::DisplayExtraWarnings) {
                                if ((std::abs(tmpCondVolFlowRate - CondVolFlowRateUser) / CondVolFlowRateUser) >
                                    DataSizing::AutoVsHardSizingThreshold) {
                                    ShowMessage("SizeChillerAbsorption: Potential issue with equipment sizing for " + this->Name);
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
                ShowSevereError("Autosizing of Absorption Chiller condenser flow rate requires a condenser");
                ShowContinueError("loop Sizing:Plant object");
                ShowContinueError("Occurs in CHILLER:ABSORPTION object=" + this->Name);
                ErrorsFound = true;
            }
            if (!this->CondVolFlowRateWasAutoSized && DataPlant::PlantFirstSizesOkayToFinalize && (this->CondVolFlowRate > 0.0)) {
                BaseSizer::reportSizerOutput(
                    moduleObjectType, this->Name, "User-Specified Design Condenser Water Flow Rate [m3/s]", this->CondVolFlowRate);
            }
        }

        // save the design condenser water volumetric flow rate for use by the condenser water loop sizing algorithms
        PlantUtilities::RegisterPlantCompDesignFlow(this->CondInletNodeNum, tmpCondVolFlowRate);

        if ((PltSizSteamNum > 0 && this->GenHeatSourceType == DataLoopNode::NodeType_Steam) ||
            (PltSizHeatingNum > 0 && this->GenHeatSourceType == DataLoopNode::NodeType_Water)) {
            if (this->EvapVolFlowRate >= DataHVACGlobals::TimeStepSys && tmpNomCap > 0.0) {
                if (this->GenHeatSourceType == DataLoopNode::NodeType_Water) {
                    Real64 CpWater = FluidProperties::GetSpecificHeatGlycol(state,
                                                                            DataPlant::PlantLoop(this->GenLoopNum).FluidName,
                                                                            DataSizing::PlantSizData(PltSizHeatingNum).ExitTemp,
                                                                            DataPlant::PlantLoop(this->GenLoopNum).FluidIndex,
                                                                            RoutineName);
                    Real64 SteamDeltaT = max(0.5, DataSizing::PlantSizData(PltSizHeatingNum).DeltaT);
                    Real64 RhoWater = FluidProperties::GetDensityGlycol(state,
                                                                        DataPlant::PlantLoop(this->GenLoopNum).FluidName,
                                                                        (DataSizing::PlantSizData(PltSizHeatingNum).ExitTemp - SteamDeltaT),
                                                                        DataPlant::PlantLoop(this->GenLoopNum).FluidIndex,
                                                                        RoutineName);
                    tmpGeneratorVolFlowRate = (this->NomCap * SteamInputRatNom) / (CpWater * SteamDeltaT * RhoWater);
                    if (!this->GeneratorVolFlowRateWasAutoSized) tmpGeneratorVolFlowRate = this->GeneratorVolFlowRate;
                    if (DataPlant::PlantFirstSizesOkayToFinalize) {
                        if (this->GeneratorVolFlowRateWasAutoSized) {
                            this->GeneratorVolFlowRate = tmpGeneratorVolFlowRate;
                            if (DataPlant::PlantFinalSizesOkayToReport) {
                                BaseSizer::reportSizerOutput(
                                    moduleObjectType, this->Name, "Design Size Design Generator Fluid Flow Rate [m3/s]", tmpGeneratorVolFlowRate);
                            }
                            if (DataPlant::PlantFirstSizesOkayToReport) {
                                BaseSizer::reportSizerOutput(moduleObjectType,
                                                                        this->Name,
                                                                        "Iniital Design Size Design Generator Fluid Flow Rate [m3/s]",
                                                                        tmpGeneratorVolFlowRate);
                            }
                        } else {
                            if (this->GeneratorVolFlowRate > 0.0 && tmpGeneratorVolFlowRate > 0.0) {
                                // Hardsized generator flow rate for reporting
                                Real64 GeneratorVolFlowRateUser = this->GeneratorVolFlowRate;
                                if (DataPlant::PlantFinalSizesOkayToReport) {
                                    BaseSizer::reportSizerOutput(moduleObjectType,
                                                                            this->Name,
                                                                            "Design Size Design Generator Fluid Flow Rate [m3/s]",
                                                                            tmpGeneratorVolFlowRate,
                                                                            "User-Specified Design Generator Fluid Flow Rate [m3/s]",
                                                                            GeneratorVolFlowRateUser);
                                    if (DataGlobals::DisplayExtraWarnings) {
                                        if ((std::abs(tmpGeneratorVolFlowRate - GeneratorVolFlowRateUser) / GeneratorVolFlowRateUser) >
                                            DataSizing::AutoVsHardSizingThreshold) {
                                            ShowMessage("SizeChillerAbsorption: Potential issue with equipment sizing for " + this->Name);
                                            ShowContinueError("User-Specified Design Generator Fluid Flow Rate of " +
                                                              General::RoundSigDigits(GeneratorVolFlowRateUser, 5) + " [m3/s]");
                                            ShowContinueError("differs from Design Size Design Generator Fluid Flow Rate of " +
                                                              General::RoundSigDigits(tmpGeneratorVolFlowRate, 5) + " [m3/s]");
                                            ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                                            ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                                        }
                                    }
                                }
                                tmpGeneratorVolFlowRate = GeneratorVolFlowRateUser;
                            }
                        }
                    }
                } else {
                    Real64 SteamDensity = FluidProperties::GetSatDensityRefrig(
                        state, fluidNameSteam, DataSizing::PlantSizData(PltSizSteamNum).ExitTemp, 1.0, this->SteamFluidIndex, RoutineNameLong);
                    Real64 SteamDeltaT = DataSizing::PlantSizData(PltSizSteamNum).DeltaT;
                    Real64 GeneratorOutletTemp = DataSizing::PlantSizData(PltSizSteamNum).ExitTemp - SteamDeltaT;

                    Real64 EnthSteamOutDry = FluidProperties::GetSatEnthalpyRefrig(
                        state, fluidNameSteam, DataSizing::PlantSizData(PltSizSteamNum).ExitTemp, 1.0, this->SteamFluidIndex, moduleObjectType + this->Name);
                    Real64 EnthSteamOutWet = FluidProperties::GetSatEnthalpyRefrig(
                        state, fluidNameSteam, DataSizing::PlantSizData(PltSizSteamNum).ExitTemp, 0.0, this->SteamFluidIndex, moduleObjectType + this->Name);
                    Real64 CpWater =
                        FluidProperties::GetSpecificHeatGlycol(state, fluidNameWater, GeneratorOutletTemp, const_cast<int &>(waterIndex), RoutineName);
                    Real64 HfgSteam = EnthSteamOutDry - EnthSteamOutWet;
                    this->SteamMassFlowRate = (this->NomCap * SteamInputRatNom) / ((HfgSteam) + (SteamDeltaT * CpWater));
                    tmpGeneratorVolFlowRate = this->SteamMassFlowRate / SteamDensity;

                    if (!this->GeneratorVolFlowRateWasAutoSized) tmpGeneratorVolFlowRate = this->GeneratorVolFlowRate;
                    if (DataPlant::PlantFirstSizesOkayToFinalize) {

                        if (this->GeneratorVolFlowRateWasAutoSized) {
                            this->GeneratorVolFlowRate = tmpGeneratorVolFlowRate;
                            if (DataPlant::PlantFinalSizesOkayToReport) {
                                BaseSizer::reportSizerOutput(
                                    moduleObjectType, this->Name, "Design Size Design Generator Fluid Flow Rate [m3/s]", tmpGeneratorVolFlowRate);
                            }
                            if (DataPlant::PlantFirstSizesOkayToReport) {
                                BaseSizer::reportSizerOutput(moduleObjectType,
                                                                        this->Name,
                                                                        "Initial Design Size Design Generator Fluid Flow Rate [m3/s]",
                                                                        tmpGeneratorVolFlowRate);
                            }
                        } else {
                            if (this->GeneratorVolFlowRate > 0.0 && tmpGeneratorVolFlowRate > 0.0) {
                                // Hardsized generator flow rate for reporting
                                Real64 GeneratorVolFlowRateUser = this->GeneratorVolFlowRate;
                                if (DataPlant::PlantFinalSizesOkayToReport) {
                                    BaseSizer::reportSizerOutput(moduleObjectType,
                                                                            this->Name,
                                                                            "Design Size Design Generator Fluid Flow Rate [m3/s]",
                                                                            tmpGeneratorVolFlowRate,
                                                                            "User-Specified Design Generator Fluid Flow Rate [m3/s]",
                                                                            GeneratorVolFlowRateUser);
                                    if (DataGlobals::DisplayExtraWarnings) {
                                        if ((std::abs(tmpGeneratorVolFlowRate - GeneratorVolFlowRateUser) / GeneratorVolFlowRateUser) >
                                            DataSizing::AutoVsHardSizingThreshold) {
                                            ShowMessage("SizeChillerAbsorption: Potential issue with equipment sizing for " + this->Name);
                                            ShowContinueError("User-Specified Design Generator Fluid Flow Rate of " +
                                                              General::RoundSigDigits(GeneratorVolFlowRateUser, 5) + " [m3/s]");
                                            ShowContinueError("differs from Design Size Design Generator Fluid Flow Rate of " +
                                                              General::RoundSigDigits(tmpGeneratorVolFlowRate, 5) + " [m3/s]");
                                            ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                                            ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
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
                    if (DataPlant::PlantFirstSizesOkayToFinalize) {
                        this->GeneratorVolFlowRate = 0.0;
                    } else {
                        tmpGeneratorVolFlowRate = 0.0;
                    }
                }
            }
        } else {
            if (this->GeneratorVolFlowRateWasAutoSized && DataPlant::PlantFirstSizesOkayToFinalize) {
                ShowSevereError("Autosizing of Absorption Chiller generator flow rate requires a loop Sizing:Plant object.");
                ShowContinueError(" For steam loops, use a steam Sizing:Plant object.");
                ShowContinueError(" For hot water loops, use a heating Sizing:Plant object.");
                ShowContinueError("Occurs in Chiller:Absorption object=" + this->Name);
                ErrorsFound = true;
            }
            if (!this->GeneratorVolFlowRateWasAutoSized && DataPlant::PlantFinalSizesOkayToReport && (this->GeneratorVolFlowRate > 0.0)) {
                BaseSizer::reportSizerOutput(
                    moduleObjectType, this->Name, "User-Specified Design Generator Fluid Flow Rate [m3/s]", this->GeneratorVolFlowRate);
            }
        }

        // save the design steam or hot water volumetric flow rate for use by the steam or hot water loop sizing algorithms
        if (DataPlant::PlantFirstSizesOkayToFinalize) {
            PlantUtilities::RegisterPlantCompDesignFlow(this->GeneratorInletNodeNum, this->GeneratorVolFlowRate);
        } else {
            PlantUtilities::RegisterPlantCompDesignFlow(this->GeneratorInletNodeNum, tmpGeneratorVolFlowRate);
        }

        if (this->GeneratorDeltaTempWasAutoSized) {
            if (PltSizHeatingNum > 0 && this->GenHeatSourceType == DataLoopNode::NodeType_Water) {
                this->GeneratorDeltaTemp = max(0.5, DataSizing::PlantSizData(PltSizHeatingNum).DeltaT);
            } else if (this->GenHeatSourceType == DataLoopNode::NodeType_Water) {
                if (DataPlant::PlantFirstSizesOkayToFinalize) {
                    Real64 Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                                       DataPlant::PlantLoop(this->GenLoopNum).FluidName,
                                                                       DataGlobalConstants::HWInitConvTemp(),
                                                                       DataPlant::PlantLoop(this->GenLoopNum).FluidIndex,
                                                                       RoutineName);
                    Real64 rho = FluidProperties::GetDensityGlycol(state,
                                                                   DataPlant::PlantLoop(this->GenLoopNum).FluidName,
                                                                   DataGlobalConstants::HWInitConvTemp(),
                                                                   DataPlant::PlantLoop(this->GenLoopNum).FluidIndex,
                                                                   RoutineName);

                    this->GeneratorDeltaTemp = (SteamInputRatNom * this->NomCap) / (Cp * rho * this->GeneratorVolFlowRate);
                }
            }
        }

        if (ErrorsFound) {
            ShowFatalError("Preceding sizing errors cause program termination");
        }

        if (DataPlant::PlantFinalSizesOkayToReport) {
            // create predefined report
            std::string equipName = this->Name;
            OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchMechType, equipName, moduleObjectType);
            OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchMechNomEff, equipName, "n/a");
            OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchMechNomCap, equipName, this->NomCap);
        }
    }

    void BLASTAbsorberSpecs::calculate(EnergyPlusData &state, Real64 &MyLoad, bool RunFlag)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Dan Fisher
        //       DATE WRITTEN   Sept. 1998
        //       MODIFIED       Apr. 1999, May 2000- Taecheol Kim
        //                      May. 2008, R. Raustad, Added generator nodes
        //                      Jun. 2016, Rongpeng Zhang, Applied the chiller supply water temperature sensor fault model
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // simulate a vapor compression Absorber using the BLAST model

        // METHODOLOGY EMPLOYED:
        // curve fit of performance data:

        // REFERENCES:
        // 1.  BLAST User Manual
        // 2.  Absorber User Manual

        static std::string const RoutineName("CalcBLASTAbsorberModel");

        Real64 EvapDeltaTemp(0.0); // C - evaporator temperature difference, water side

        // If no loop demand or Absorber OFF, return
        if (MyLoad >= 0.0 || !RunFlag) { // off or heating
            if (this->EquipFlowCtrl == DataBranchAirLoopPlant::ControlType_SeriesActive)
                this->EvapMassFlowRate = DataLoopNode::Node(this->EvapInletNodeNum).MassFlowRate;
            return;
        }

        // Set the condenser mass flow rates
        this->CondMassFlowRate = DataLoopNode::Node(this->CondInletNodeNum).MassFlowRate;

        Real64 TempEvapOut = DataLoopNode::Node(this->EvapOutletNodeNum).Temp;

        Real64 CpFluid = FluidProperties::GetSpecificHeatGlycol(state,
                                                                DataPlant::PlantLoop(this->CWLoopNum).FluidName,
                                                                DataLoopNode::Node(this->EvapInletNodeNum).Temp,
                                                                DataPlant::PlantLoop(this->CWLoopNum).FluidIndex,
                                                                RoutineName);

        // If there is a fault of Chiller SWT Sensor (zrp_Jun2016)
        if (this->FaultyChillerSWTFlag && (!DataGlobals::WarmupFlag) && (!DataGlobals::DoingSizing) && (!DataGlobals::KickOffSimulation)) {
            int FaultIndex = this->FaultyChillerSWTIndex;
            Real64 EvapOutletTemp_ff = TempEvapOut;

            // calculate the sensor offset using fault information
            this->FaultyChillerSWTOffset = FaultsManager::FaultsChillerSWTSensor(FaultIndex).CalFaultOffsetAct();
            // update the TempEvapOut
            TempEvapOut = max(this->TempLowLimitEvapOut,
                              min(DataLoopNode::Node(this->EvapInletNodeNum).Temp, EvapOutletTemp_ff - this->FaultyChillerSWTOffset));
            this->FaultyChillerSWTOffset = EvapOutletTemp_ff - TempEvapOut;
        }

        // If FlowLock is True, the new resolved mdot is used to update Power, QEvap, Qcond, and
        // condenser side outlet temperature.
        if (DataPlant::PlantLoop(this->CWLoopNum).LoopSide(this->CWLoopSideNum).FlowLock == 0) {
            this->PossibleSubcooling = false;
            this->QEvaporator = std::abs(MyLoad);
            // limit by max capacity
            this->QEvaporator = min(this->QEvaporator, (this->MaxPartLoadRat * this->NomCap));

            // Either set the flow to the Constant value or caluclate the flow for the variable volume
            if ((this->FlowMode == DataPlant::FlowMode::CONSTANT) || (this->FlowMode == DataPlant::FlowMode::NOTMODULATED)) {
                this->EvapMassFlowRate = DataLoopNode::Node(this->EvapInletNodeNum).MassFlowRate;

                if (this->EvapMassFlowRate != 0.0) {
                    EvapDeltaTemp = this->QEvaporator / this->EvapMassFlowRate / CpFluid;
                } else {
                    EvapDeltaTemp = 0.0;
                }
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

                    this->EvapMassFlowRate = std::abs(this->QEvaporator / CpFluid / EvapDeltaTemp);
                    if ((this->EvapMassFlowRate - this->EvapMassFlowRateMax) > DataBranchAirLoopPlant::MassFlowTolerance)
                        this->PossibleSubcooling = true;
                    // Check to see if the Maximum is exceeded, if so set to maximum
                    this->EvapMassFlowRate = min(this->EvapMassFlowRateMax, this->EvapMassFlowRate);
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
                    this->EvapMassFlowRate = 0.0;

                    this->EvapOutletTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp;

                    ShowRecurringWarningErrorAtEnd("CalcBLASTAbsorberModel: Name=\"" + this->Name +
                                                       "\" Evaporative Condenser Delta Temperature = 0 in mass flow calculation.",
                                                   this->ErrCount2);
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
                                        CpFluid,
                                        DataLoopNode::Node(this->EvapInletNodeNum).Temp,
                                        this->EvapOutletTemp,
                                        this->EvapMassFlowRate,
                                        this->QEvaporator);
                // update corresponding variables at faulty case
                // PartLoadRat = ( AvailChillerCap > 0.0 ) ? ( QEvaporator / AvailChillerCap ) : 0.0;
                // PartLoadRat = max( 0.0, min( PartLoadRat, MaxPartLoadRat ));
                // ChillerPartLoadRatio = PartLoadRat;
            }

        } else { // If FlowLock is True

            this->EvapMassFlowRate = DataLoopNode::Node(this->EvapInletNodeNum).MassFlowRate;
            if (this->PossibleSubcooling) {
                this->QEvaporator = std::abs(MyLoad);
                EvapDeltaTemp = this->QEvaporator / this->EvapMassFlowRate / CpFluid;
                this->EvapOutletTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp - EvapDeltaTemp;
            } else {
                Real64 TempEvapOutSetPoint(0.0); // C - evaporator outlet temperature setpoint

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
                            TempEvapOutSetPoint = DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPoint;
                        } else {
                            TempEvapOutSetPoint = DataLoopNode::Node(DataPlant::PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPoint;
                        }
                    } else if (SELECT_CASE_var == DataPlant::DualSetPointDeadBand) {
                        if ((this->FlowMode == DataPlant::FlowMode::LEAVINGSETPOINTMODULATED) ||
                            (DataPlant::PlantLoop(this->CWLoopNum)
                                 .LoopSide(this->CWLoopSideNum)
                                 .Branch(this->CWBranchNum)
                                 .Comp(this->CWCompNum)
                                 .CurOpSchemeType == DataPlant::CompSetPtBasedSchemeType) ||
                            (DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPointHi != DataLoopNode::SensedNodeFlagValue)) {
                            TempEvapOutSetPoint = DataLoopNode::Node(this->EvapOutletNodeNum).TempSetPointHi;
                        } else {
                            TempEvapOutSetPoint = DataLoopNode::Node(DataPlant::PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPointHi;
                        }
                    } else {
                        assert(false);
                    }
                }
                EvapDeltaTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp - TempEvapOutSetPoint;
                this->QEvaporator = std::abs(this->EvapMassFlowRate * CpFluid * EvapDeltaTemp);
                this->EvapOutletTemp = TempEvapOutSetPoint;
            }
            // Check that the Evap outlet temp honors both plant loop temp low limit and also the chiller low limit
            if (this->EvapOutletTemp < this->TempLowLimitEvapOut) {
                if ((DataLoopNode::Node(this->EvapInletNodeNum).Temp - this->TempLowLimitEvapOut) > DataPlant::DeltaTempTol) {
                    this->EvapOutletTemp = this->TempLowLimitEvapOut;
                    EvapDeltaTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp - this->EvapOutletTemp;
                    this->QEvaporator = this->EvapMassFlowRate * CpFluid * EvapDeltaTemp;
                } else {
                    this->EvapOutletTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp;
                    EvapDeltaTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp - this->EvapOutletTemp;
                    this->QEvaporator = this->EvapMassFlowRate * CpFluid * EvapDeltaTemp;
                }
            }
            if (this->EvapOutletTemp < DataLoopNode::Node(this->EvapOutletNodeNum).TempMin) {
                if ((DataLoopNode::Node(this->EvapInletNodeNum).Temp - DataLoopNode::Node(this->EvapOutletNodeNum).TempMin) >
                    DataPlant::DeltaTempTol) {
                    this->EvapOutletTemp = DataLoopNode::Node(this->EvapOutletNodeNum).TempMin;
                    EvapDeltaTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp - this->EvapOutletTemp;
                    this->QEvaporator = this->EvapMassFlowRate * CpFluid * EvapDeltaTemp;
                } else {
                    this->EvapOutletTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp;
                    EvapDeltaTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp - this->EvapOutletTemp;
                    this->QEvaporator = this->EvapMassFlowRate * CpFluid * EvapDeltaTemp;
                }
            }

            // Checks QEvaporator on the basis of the machine limits.
            if (this->QEvaporator > std::abs(MyLoad)) {
                if (this->EvapMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance) {
                    this->QEvaporator = std::abs(MyLoad);
                    EvapDeltaTemp = this->QEvaporator / this->EvapMassFlowRate / CpFluid;
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
                                        CpFluid,
                                        DataLoopNode::Node(this->EvapInletNodeNum).Temp,
                                        this->EvapOutletTemp,
                                        this->EvapMassFlowRate,
                                        this->QEvaporator);
                // update corresponding variables at faulty case
            }

        } // This is the end of the FlowLock Block

        // Calculate part load ratio for efficiency calcs. If this part load ratio is greater than
        // Min PLR it will be used for calculations too.
        Real64 PartLoadRat = max(this->MinPartLoadRat, min(this->QEvaporator / this->NomCap, this->MaxPartLoadRat));

        // In case MyLoad is less than the Min PLR load, the power and steam input should be adjusted
        // for cycling. The ratios used however are based on MinPLR.
        Real64 OperPartLoadRat = this->QEvaporator / this->NomCap;

        Real64 FRAC;
        if (OperPartLoadRat < PartLoadRat) {
            FRAC = min(1.0, OperPartLoadRat / this->MinPartLoadRat);
        } else {
            FRAC = 1.0;
        }

        // Calculate steam input ratio
        Real64 SteamInputRat = this->SteamLoadCoef(1) / PartLoadRat + this->SteamLoadCoef(2) + this->SteamLoadCoef(3) * PartLoadRat;

        // Calculate electric input ratio
        Real64 ElectricInputRat = this->PumpPowerCoef(1) + this->PumpPowerCoef(2) * PartLoadRat + this->PumpPowerCoef(3) * pow_2(PartLoadRat);

        // Calculate electric energy input
        this->PumpingPower = ElectricInputRat * this->NomPumpPower * FRAC;

        // Calculate steam load
        this->QGenerator = SteamInputRat * this->QEvaporator * FRAC;

        if (this->EvapMassFlowRate == 0.0) {
            this->QGenerator = 0.0;
            this->EvapOutletTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp;
            this->PumpingPower = 0.0;
        }

        this->QCondenser = this->QEvaporator + this->QGenerator + this->PumpingPower;

        CpFluid = FluidProperties::GetSpecificHeatGlycol(state,
                                                         DataPlant::PlantLoop(this->CDLoopNum).FluidName,
                                                         DataLoopNode::Node(this->CondInletNodeNum).Temp,
                                                         DataPlant::PlantLoop(this->CDLoopNum).FluidIndex,
                                                         RoutineName);

        if (this->CondMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance) {
            this->CondOutletTemp = this->QCondenser / this->CondMassFlowRate / CpFluid + DataLoopNode::Node(this->CondInletNodeNum).Temp;
        } else {

            this->CondOutletTemp = DataLoopNode::Node(this->CondInletNodeNum).Temp;
            this->CondMassFlowRate = 0.0;
            this->QCondenser = 0.0;
            return;
            // V7 plant upgrade, no longer fatal here anymore, set some things and return
        }

        if (this->GeneratorInletNodeNum > 0) {
            if (this->GenHeatSourceType == DataLoopNode::NodeType_Water) {
                Real64 GenMassFlowRate;
                //  Hot water plant is used for the generator
                CpFluid = FluidProperties::GetSpecificHeatGlycol(state,
                                                                 DataPlant::PlantLoop(this->GenLoopNum).FluidName,
                                                                 DataLoopNode::Node(this->GeneratorInletNodeNum).Temp,
                                                                 DataPlant::PlantLoop(GenLoopSideNum).FluidIndex,
                                                                 RoutineName);
                if (DataPlant::PlantLoop(this->GenLoopNum).LoopSide(this->GenLoopSideNum).FlowLock == 0) {
                    if ((this->FlowMode == DataPlant::FlowMode::CONSTANT) || (this->FlowMode == DataPlant::FlowMode::NOTMODULATED)) {
                        GenMassFlowRate = this->GenMassFlowRateMax;
                    } else { // LeavingSetpointModulated
                        // since the .FlowMode applies to the chiller evaporator, the generater mass flow rate will be proportional to the evaporator
                        // mass flow rate
                        Real64 GenFlowRatio = this->EvapMassFlowRate / this->EvapMassFlowRateMax;
                        GenMassFlowRate = min(this->GenMassFlowRateMax, GenFlowRatio * this->GenMassFlowRateMax);
                    }
                } else { // If FlowLock is True
                    GenMassFlowRate = DataLoopNode::Node(this->GeneratorInletNodeNum).MassFlowRate;
                }
                PlantUtilities::SetComponentFlowRate(GenMassFlowRate,
                                                     this->GeneratorInletNodeNum,
                                                     this->GeneratorOutletNodeNum,
                                                     this->GenLoopNum,
                                                     this->GenLoopSideNum,
                                                     this->GenBranchNum,
                                                     this->GenCompNum);

                if (GenMassFlowRate <= 0.0) {
                    this->GenOutletTemp = DataLoopNode::Node(this->GeneratorInletNodeNum).Temp;
                    this->SteamOutletEnthalpy = DataLoopNode::Node(this->GeneratorInletNodeNum).Enthalpy;
                } else {
                    this->GenOutletTemp = DataLoopNode::Node(this->GeneratorInletNodeNum).Temp - this->QGenerator / (CpFluid * GenMassFlowRate);
                    this->SteamOutletEnthalpy = DataLoopNode::Node(this->GeneratorInletNodeNum).Enthalpy - this->QGenerator / GenMassFlowRate;
                }
                DataLoopNode::Node(this->GeneratorOutletNodeNum).Temp = this->GenOutletTemp;
                DataLoopNode::Node(this->GeneratorOutletNodeNum).Enthalpy = this->SteamOutletEnthalpy;
                DataLoopNode::Node(this->GeneratorOutletNodeNum).MassFlowRate = GenMassFlowRate;

            } else { // using a steam plant for the generator

                // enthalpy of dry steam at generator inlet
                Real64 EnthSteamOutDry = FluidProperties::GetSatEnthalpyRefrig(state,
                                                                               fluidNameSteam,
                                                                               DataLoopNode::Node(this->GeneratorInletNodeNum).Temp,
                                                                               1.0,
                                                                               this->SteamFluidIndex,
                                                                               calcChillerAbsorption + this->Name);

                // enthalpy of wet steam at generator inlet
                Real64 EnthSteamOutWet = FluidProperties::GetSatEnthalpyRefrig(state,
                                                                               fluidNameSteam,
                                                                               DataLoopNode::Node(this->GeneratorInletNodeNum).Temp,
                                                                               0.0,
                                                                               this->SteamFluidIndex,
                                                                               calcChillerAbsorption + this->Name);
                Real64 SteamDeltaT = this->GeneratorSubcool;
                Real64 SteamOutletTemp = DataLoopNode::Node(this->GeneratorInletNodeNum).Temp - SteamDeltaT;
                Real64 HfgSteam = EnthSteamOutDry - EnthSteamOutWet;
                CpFluid = FluidProperties::GetSpecificHeatGlycol(
                    state, fluidNameWater, SteamOutletTemp, const_cast<int &>(waterIndex), calcChillerAbsorption + this->Name);
                this->SteamMassFlowRate = this->QGenerator / (HfgSteam + CpFluid * SteamDeltaT);
                PlantUtilities::SetComponentFlowRate(this->SteamMassFlowRate,
                                                     this->GeneratorInletNodeNum,
                                                     this->GeneratorOutletNodeNum,
                                                     this->GenLoopNum,
                                                     this->GenLoopSideNum,
                                                     this->GenBranchNum,
                                                     this->GenCompNum);

                if (this->SteamMassFlowRate <= 0.0) {
                    this->GenOutletTemp = DataLoopNode::Node(this->GeneratorInletNodeNum).Temp;
                    this->SteamOutletEnthalpy = DataLoopNode::Node(this->GeneratorInletNodeNum).Enthalpy;
                } else {
                    this->GenOutletTemp = DataLoopNode::Node(this->GeneratorInletNodeNum).Temp - SteamDeltaT;
                    this->SteamOutletEnthalpy = FluidProperties::GetSatEnthalpyRefrig(
                        state, fluidNameSteam, this->GenOutletTemp, 0.0, this->SteamFluidIndex, moduleObjectType + this->Name);
                    this->SteamOutletEnthalpy -= CpFluid * SteamDeltaT;
                }
            }
        } // IF(GeneratorInletNode .GT. 0)THEN

        // convert power to energy
        this->GeneratorEnergy = this->QGenerator * DataHVACGlobals::TimeStepSys * DataGlobalConstants::SecInHour();
        this->EvaporatorEnergy = this->QEvaporator * DataHVACGlobals::TimeStepSys * DataGlobalConstants::SecInHour();
        this->CondenserEnergy = this->QCondenser * DataHVACGlobals::TimeStepSys * DataGlobalConstants::SecInHour();
        this->PumpingEnergy = this->PumpingPower * DataHVACGlobals::TimeStepSys * DataGlobalConstants::SecInHour();
    }

    void BLASTAbsorberSpecs::updateRecords(Real64 MyLoad, bool RunFlag)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Dan Fisher
        //       DATE WRITTEN:    October 1998

        // PURPOSE OF THIS SUBROUTINE:
        // reporting

        if (MyLoad >= 0 || !RunFlag) {
            // set node conditions
            PlantUtilities::SafeCopyPlantNode(this->EvapInletNodeNum, this->EvapOutletNodeNum);
            PlantUtilities::SafeCopyPlantNode(this->CondInletNodeNum, this->CondOutletNodeNum);

            this->Report.PumpingPower = 0.0;
            this->Report.QEvap = 0.0;
            this->Report.QCond = 0.0;
            this->Report.QGenerator = 0.0;
            this->Report.PumpingEnergy = 0.0;
            this->Report.EvapEnergy = 0.0;
            this->Report.CondEnergy = 0.0;
            this->Report.GeneratorEnergy = 0.0;
            this->Report.EvapInletTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp;
            this->Report.CondInletTemp = DataLoopNode::Node(this->CondInletNodeNum).Temp;
            this->Report.CondOutletTemp = DataLoopNode::Node(this->CondOutletNodeNum).Temp;
            this->Report.EvapOutletTemp = DataLoopNode::Node(this->EvapOutletNodeNum).Temp;
            this->Report.Evapmdot = 0.0;
            this->Report.Condmdot = 0.0;
            this->Report.Genmdot = 0.0;
            this->Report.ActualCOP = 0.0;

            if (this->GeneratorInletNodeNum > 0) {
                PlantUtilities::SafeCopyPlantNode(this->GeneratorInletNodeNum, this->GeneratorOutletNodeNum);
            }

        } else {
            // set node conditions
            PlantUtilities::SafeCopyPlantNode(this->EvapInletNodeNum, this->EvapOutletNodeNum);
            PlantUtilities::SafeCopyPlantNode(this->CondInletNodeNum, this->CondOutletNodeNum);
            DataLoopNode::Node(this->EvapOutletNodeNum).Temp = this->EvapOutletTemp;
            DataLoopNode::Node(this->CondOutletNodeNum).Temp = this->CondOutletTemp;

            this->Report.PumpingPower = this->PumpingPower;
            this->Report.QEvap = this->QEvaporator;
            this->Report.QCond = this->QCondenser;
            this->Report.QGenerator = this->QGenerator;
            this->Report.PumpingEnergy = this->PumpingEnergy;
            this->Report.EvapEnergy = this->EvaporatorEnergy;
            this->Report.CondEnergy = this->CondenserEnergy;
            this->Report.GeneratorEnergy = this->GeneratorEnergy;
            this->Report.EvapInletTemp = DataLoopNode::Node(this->EvapInletNodeNum).Temp;
            this->Report.CondInletTemp = DataLoopNode::Node(this->CondInletNodeNum).Temp;
            this->Report.CondOutletTemp = DataLoopNode::Node(this->CondOutletNodeNum).Temp;
            this->Report.EvapOutletTemp = DataLoopNode::Node(this->EvapOutletNodeNum).Temp;
            this->Report.Evapmdot = this->EvapMassFlowRate;
            this->Report.Condmdot = this->CondMassFlowRate;
            this->Report.Genmdot = this->SteamMassFlowRate;
            if (this->QGenerator != 0.0) {
                this->Report.ActualCOP = this->QEvaporator / this->QGenerator;
            } else {
                this->Report.ActualCOP = 0.0;
            }

            if (this->GeneratorInletNodeNum > 0) {
                PlantUtilities::SafeCopyPlantNode(this->GeneratorInletNodeNum, this->GeneratorOutletNodeNum);
                DataLoopNode::Node(this->GeneratorOutletNodeNum).Temp = this->GenOutletTemp;
            }
        }
    }

} // namespace ChillerAbsorption

} // namespace EnergyPlus
