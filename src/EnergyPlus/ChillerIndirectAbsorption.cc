// EnergyPlus, Copyright (c) 1996-2019, The Board of Trustees of the University of Illinois,
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
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/ChillerIndirectAbsorption.hh>
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
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/ReportSizingManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

namespace ChillerIndirectAbsorption {

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

    int const FlowModeNotSet(200);
    int const ConstantFlow(201);
    int const NotModulated(202);
    int const LeavingSetPointModulated(203);

    static std::string const BlankString;
    static std::string const fluidNameSteam("STEAM");
    static std::string const fluidNameWater("WATER");
    static std::string const calcChillerAbsorptionIndirect("CALC Chiller:Absorption:Indirect ");

    int NumIndirectAbsorbers(0);         // number of Absorption Chillers specified in input
    Real64 EnergyLossToEnvironment(0.0); // J - piping energy loss from generator outlet to pump inlet

    bool GetInput(true); // when TRUE, calls subroutine to read input file.

    Array1D<IndirectAbsorberSpecs> IndirectAbsorber; // dimension to number of machines
    Array1D<ReportVars> IndirectAbsorberReport;

    void clear_state()
    {
        NumIndirectAbsorbers = 0;
        EnergyLossToEnvironment = 0.0;
        IndirectAbsorber.deallocate();
        IndirectAbsorberReport.deallocate();
    }

    void SimIndirectAbsorber(std::string const &EP_UNUSED(AbsorberType), // type of Absorber
                             std::string const &AbsorberName,            // user specified name of Absorber
                             int const EquipFlowCtrl,                    // Flow control mode for the equipment
                             int const LoopNum,                          // Plant loop index for where called from
                             int const LoopSide,                         // Plant loop side index for where called from
                             int &CompIndex,                             // Chiller number pointer
                             bool const RunFlag,                         // simulate Absorber when TRUE
                             bool const FirstIteration,                  // initialize variables when TRUE
                             bool &InitLoopEquip,                        // If not zero, calculate the max load for operating conditions
                             Real64 &MyLoad,                             // loop demand component will meet
                             Real64 &MaxCap,                             // W - maximum operating capacity of Absorber
                             Real64 &MinCap,                             // W - minimum operating capacity of Absorber
                             Real64 &OptCap,                             // W - optimal operating capacity of Absorber
                             bool const GetSizingFactor,                 // TRUE when just the sizing factor is requested
                             Real64 &SizingFactor,                       // sizing factor
                             Real64 &TempCondInDesign)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         R. Raustad (FSEC)
        //       DATE WRITTEN   May 2008
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE: This is the Indirect Absorption Chiller model driver.  It
        // gets the input for the models, initializes simulation variables, call
        // the appropriate model and sets up reporting variables.

        int ChillNum; // Chiller number pointer

        if (CompIndex != 0) {
            TempCondInDesign = IndirectAbsorber(CompIndex).TempDesCondIn;
        }

        // Get Absorber data from input file
        if (GetInput) {
            GetIndirectAbsorberInput();
            GetInput = false;
        }

        // Find the correct Chiller
        if (CompIndex == 0) {
            ChillNum = UtilityRoutines::FindItemInList(AbsorberName, IndirectAbsorber);
            if (ChillNum == 0) {
                ShowFatalError("SimIndirectAbsorber: Specified chiller not one of Valid Absorption Chillers=" + AbsorberName);
            }
            CompIndex = ChillNum;
        } else {
            ChillNum = CompIndex;
            if (ChillNum > NumIndirectAbsorbers || ChillNum < 1) {
                ShowFatalError("SimIndirectAbsorber:  Invalid CompIndex passed=" + General::TrimSigDigits(ChillNum) +
                               ", Number of Units=" + General::TrimSigDigits(NumIndirectAbsorbers) + ", Entered Unit name=" + AbsorberName);
            }
            if (AbsorberName != IndirectAbsorber(ChillNum).Name) {
                ShowFatalError("SimIndirectAbsorber: Invalid CompIndex passed=" + General::TrimSigDigits(ChillNum) + ", Unit name=" + AbsorberName +
                               ", stored Unit Name for that index=" + IndirectAbsorber(ChillNum).Name);
            }
        }

        // Initialize Loop Equipment
        if (InitLoopEquip) {
            InitIndirectAbsorpChiller(ChillNum, RunFlag, MyLoad);

            if (LoopNum == IndirectAbsorber(ChillNum).CWLoopNum) {
                SizeIndirectAbsorpChiller(ChillNum); // only size when called from chilled water loop
                MinCap = IndirectAbsorber(ChillNum).NomCap * IndirectAbsorber(ChillNum).MinPartLoadRat;
                MaxCap = IndirectAbsorber(ChillNum).NomCap * IndirectAbsorber(ChillNum).MaxPartLoadRat;
                OptCap = IndirectAbsorber(ChillNum).NomCap * IndirectAbsorber(ChillNum).OptPartLoadRat;
            } else {
                MinCap = 0.0;
                MaxCap = 0.0;
                OptCap = 0.0;
            }
            if (GetSizingFactor) {
                ChillNum = UtilityRoutines::FindItemInList(AbsorberName, IndirectAbsorber);
                if (ChillNum != 0) {
                    SizingFactor = IndirectAbsorber(ChillNum).SizFac;
                }
            }
            return;
        }

        if (LoopNum == IndirectAbsorber(ChillNum).CWLoopNum) {

            InitIndirectAbsorpChiller(ChillNum, RunFlag, MyLoad);
            CalcIndirectAbsorberModel(ChillNum, MyLoad, RunFlag, FirstIteration, EquipFlowCtrl);
            UpdateIndirectAbsorberRecords(MyLoad, RunFlag, ChillNum);

        } else if (LoopNum == IndirectAbsorber(ChillNum).CDLoopNum) {
            // Called from non-dominant condenser water connection loop side
            PlantUtilities::UpdateChillerComponentCondenserSide(LoopNum,
                                                LoopSide,
                                                DataPlant::TypeOf_Chiller_Indirect_Absorption,
                                                IndirectAbsorber(ChillNum).CondInletNodeNum,
                                                IndirectAbsorber(ChillNum).CondOutletNodeNum,
                                                IndirectAbsorberReport(ChillNum).QCond,
                                                IndirectAbsorberReport(ChillNum).CondInletTemp,
                                                IndirectAbsorberReport(ChillNum).CondOutletTemp,
                                                IndirectAbsorberReport(ChillNum).Condmdot,
                                                FirstIteration);

        } else if (LoopNum == IndirectAbsorber(ChillNum).GenLoopNum) {
            // Called from non-dominant generator hot water or steam connection loop side
            PlantUtilities::UpdateAbsorberChillerComponentGeneratorSide(LoopNum,
                                                        LoopSide,
                                                        DataPlant::TypeOf_Chiller_Indirect_Absorption,
                                                        IndirectAbsorber(ChillNum).GeneratorInletNodeNum,
                                                        IndirectAbsorber(ChillNum).GeneratorOutletNodeNum,
                                                        IndirectAbsorber(ChillNum).GenHeatSourceType,
                                                        IndirectAbsorberReport(ChillNum).QGenerator,
                                                        IndirectAbsorberReport(ChillNum).SteamMdot,
                                                        FirstIteration);

        } else {
            ShowFatalError("SimIndirectAbsorber: Invalid LoopNum passed=" + General::TrimSigDigits(LoopNum) + ", Unit name=" + AbsorberName +
                           ", stored chilled water loop=" + General::TrimSigDigits(IndirectAbsorber(ChillNum).CWLoopNum) +
                           ", stored condenser water loop=" + General::TrimSigDigits(IndirectAbsorber(ChillNum).CDLoopNum) +
                           ", stored generator loop=" + General::TrimSigDigits(IndirectAbsorber(ChillNum).GenLoopNum));
        }
    }

    void GetIndirectAbsorberInput()
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR:          R. Raustad (FSEC)
        //       DATE WRITTEN:    May 2008

        // PURPOSE OF THIS SUBROUTINE:
        // This routine will get the input
        // required by the Indirect Absorption chiller models as shown below:

        // METHODOLOGY EMPLOYED:
        // EnergyPlus input processor

        static std::string const RoutineName("GetIndirectAbsorberInput: "); // include trailing blank space

        int AbsorberNum; // Absorber counter
        int NumAlphas;   // Number of elements in the alpha array
        int NumNums;     // Number of elements in the numeric array
        int IOStat;      // IO Status when calling get input subroutine
        static bool ErrorsFound(false);
        Array1D_bool GenInputOutputNodesUsed; // Used for SetupOutputVariable

        DataIPShortCuts::cCurrentModuleObject = "Chiller:Absorption:Indirect";
        NumIndirectAbsorbers = inputProcessor->getNumObjectsFound(DataIPShortCuts::cCurrentModuleObject);

        if (NumIndirectAbsorbers <= 0) {
            ShowSevereError("No " + DataIPShortCuts::cCurrentModuleObject + " equipment specified in input file");
            // See if load distribution manager has already gotten the input
            ErrorsFound = true;
        }

        if (allocated(IndirectAbsorber)) return;
        // ALLOCATE ARRAYS
        IndirectAbsorber.allocate(NumIndirectAbsorbers);

        IndirectAbsorberReport.allocate(NumIndirectAbsorbers);

        GenInputOutputNodesUsed.dimension(NumIndirectAbsorbers, false);

        // LOAD ARRAYS WITH BLAST CURVE FIT Absorber DATA
        for (AbsorberNum = 1; AbsorberNum <= NumIndirectAbsorbers; ++AbsorberNum) {
            inputProcessor->getObjectItem(DataIPShortCuts::cCurrentModuleObject,
                                          AbsorberNum,
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

            IndirectAbsorber(AbsorberNum).Name = DataIPShortCuts::cAlphaArgs(1);
            IndirectAbsorber(AbsorberNum).NomCap = DataIPShortCuts::rNumericArgs(1);
            if (IndirectAbsorber(AbsorberNum).NomCap == DataSizing::AutoSize) {
                IndirectAbsorber(AbsorberNum).NomCapWasAutoSized = true;
            }
            IndirectAbsorber(AbsorberNum).NomPumpPower = DataIPShortCuts::rNumericArgs(2);
            if (IndirectAbsorber(AbsorberNum).NomPumpPower == DataSizing::AutoSize) {
                IndirectAbsorber(AbsorberNum).NomPumpPowerWasAutoSized = true;
            }
            if (DataIPShortCuts::rNumericArgs(1) == 0.0) {
                ShowSevereError("Invalid " + DataIPShortCuts::cNumericFieldNames(1) + '=' + General::RoundSigDigits(DataIPShortCuts::rNumericArgs(1), 2));
                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                ErrorsFound = true;
            }
            // Assign Node Numbers to specified nodes
            IndirectAbsorber(AbsorberNum).EvapInletNodeNum = NodeInputManager::GetOnlySingleNode(
                DataIPShortCuts::cAlphaArgs(2), ErrorsFound, DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Inlet, 1, DataLoopNode::ObjectIsNotParent);
            IndirectAbsorber(AbsorberNum).EvapOutletNodeNum = NodeInputManager::GetOnlySingleNode(
                DataIPShortCuts::cAlphaArgs(3), ErrorsFound, DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Outlet, 1, DataLoopNode::ObjectIsNotParent);
            BranchNodeConnections::TestCompSet(DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataIPShortCuts::cAlphaArgs(2), DataIPShortCuts::cAlphaArgs(3), "Chilled Water Nodes");

            IndirectAbsorber(AbsorberNum).CondInletNodeNum = NodeInputManager::GetOnlySingleNode(
                DataIPShortCuts::cAlphaArgs(4), ErrorsFound, DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Inlet, 2, DataLoopNode::ObjectIsNotParent);
            IndirectAbsorber(AbsorberNum).CondOutletNodeNum = NodeInputManager::GetOnlySingleNode(
                DataIPShortCuts::cAlphaArgs(5), ErrorsFound, DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Outlet, 2, DataLoopNode::ObjectIsNotParent);
            BranchNodeConnections::TestCompSet(DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataIPShortCuts::cAlphaArgs(4), DataIPShortCuts::cAlphaArgs(5), "Condenser (not tested) Nodes");

            IndirectAbsorber(AbsorberNum).GeneratorInputCurvePtr = CurveManager::GetCurveIndex(DataIPShortCuts::cAlphaArgs(7));
            if (IndirectAbsorber(AbsorberNum).GeneratorInputCurvePtr > 0) {
                // Verify Curve Object, only legal types are Quadratic or Cubic
                ErrorsFound |= CurveManager::CheckCurveDims(
                    IndirectAbsorber(AbsorberNum).GeneratorInputCurvePtr,   // Curve index
                    {1},                            // Valid dimensions
                    RoutineName,                    // Routine name
                    DataIPShortCuts::cCurrentModuleObject,            // Object Type
                    IndirectAbsorber(AbsorberNum).Name,  // Object Name
                    DataIPShortCuts::cAlphaFieldNames(7));               // Field Name
            }

            IndirectAbsorber(AbsorberNum).PumpPowerCurvePtr = CurveManager::GetCurveIndex(DataIPShortCuts::cAlphaArgs(8));
            if (IndirectAbsorber(AbsorberNum).PumpPowerCurvePtr > 0) {
                // Verify Curve Object, only legal types are Quadratic or Cubic
                ErrorsFound |= CurveManager::CheckCurveDims(
                    IndirectAbsorber(AbsorberNum).PumpPowerCurvePtr,   // Curve index
                    {1},                            // Valid dimensions
                    RoutineName,                    // Routine name
                    DataIPShortCuts::cCurrentModuleObject,            // Object Type
                    IndirectAbsorber(AbsorberNum).Name,  // Object Name
                    DataIPShortCuts::cAlphaFieldNames(8));               // Field Name
            }

            if (NumAlphas > 15) {
                if (UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(16), "HotWater") || UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(16), "HotWater")) {
                    IndirectAbsorber(AbsorberNum).GenHeatSourceType = DataLoopNode::NodeType_Water;
                    //       Default to Steam if left blank
                } else if (UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(16), "Steam") || DataIPShortCuts::cAlphaArgs(16).empty()) {
                    IndirectAbsorber(AbsorberNum).GenHeatSourceType = DataLoopNode::NodeType_Steam;
                } else {
                    ShowWarningError(DataIPShortCuts::cCurrentModuleObject + ", Name=" + DataIPShortCuts::cAlphaArgs(1));
                    ShowContinueError("...Generator heat source type must be Steam or Hot Water.");
                    ShowContinueError("...Entered generator heat source type = " + DataIPShortCuts::cAlphaArgs(16));
                    ErrorsFound = true;
                }
            } else {
                //     Default to Steam if not entered as input
                IndirectAbsorber(AbsorberNum).GenHeatSourceType = DataLoopNode::NodeType_Steam;
            }

            if ((!DataIPShortCuts::cAlphaArgs(9).empty()) && (!DataIPShortCuts::cAlphaArgs(10).empty())) {
                GenInputOutputNodesUsed(AbsorberNum) = true;
                if (IndirectAbsorber(AbsorberNum).GenHeatSourceType == DataLoopNode::NodeType_Water) {
                    IndirectAbsorber(AbsorberNum).GeneratorInletNodeNum = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(9),
                                                                                            ErrorsFound,
                                                                                            DataIPShortCuts::cCurrentModuleObject,
                                                                                            DataIPShortCuts::cAlphaArgs(1),
                                                                                            DataLoopNode::NodeType_Water,
                                                                                            DataLoopNode::NodeConnectionType_Inlet,
                                                                                            3,
                                                                                            DataLoopNode::ObjectIsNotParent);
                    IndirectAbsorber(AbsorberNum).GeneratorOutletNodeNum = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(10),
                                                                                             ErrorsFound,
                                                                                             DataIPShortCuts::cCurrentModuleObject,
                                                                                             DataIPShortCuts::cAlphaArgs(1),
                                                                                             DataLoopNode::NodeType_Water,
                                                                                             DataLoopNode::NodeConnectionType_Outlet,
                                                                                             3,
                                                                                             DataLoopNode::ObjectIsNotParent);
                    BranchNodeConnections::TestCompSet(DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataIPShortCuts::cAlphaArgs(9), DataIPShortCuts::cAlphaArgs(10), "Hot Water Nodes");
                } else {
                    IndirectAbsorber(AbsorberNum).SteamFluidIndex = FluidProperties::FindRefrigerant("Steam");
                    IndirectAbsorber(AbsorberNum).GeneratorInletNodeNum = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(9),
                                                                                            ErrorsFound,
                                                                                            DataIPShortCuts::cCurrentModuleObject,
                                                                                            DataIPShortCuts::cAlphaArgs(1),
                                                                                            DataLoopNode::NodeType_Steam,
                                                                                            DataLoopNode::NodeConnectionType_Inlet,
                                                                                            3,
                                                                                            DataLoopNode::ObjectIsNotParent);
                    IndirectAbsorber(AbsorberNum).GeneratorOutletNodeNum = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(10),
                                                                                             ErrorsFound,
                                                                                             DataIPShortCuts::cCurrentModuleObject,
                                                                                             DataIPShortCuts::cAlphaArgs(1),
                                                                                             DataLoopNode::NodeType_Steam,
                                                                                             DataLoopNode::NodeConnectionType_Outlet,
                                                                                             3,
                                                                                             DataLoopNode::ObjectIsNotParent);
                    BranchNodeConnections::TestCompSet(DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataIPShortCuts::cAlphaArgs(9), DataIPShortCuts::cAlphaArgs(10), "Steam Nodes");
                }
            } else if (DataIPShortCuts::cAlphaArgs(9).empty() != DataIPShortCuts::cAlphaArgs(10).empty()) {
                ShowWarningError(DataIPShortCuts::cCurrentModuleObject + ", Name=" + DataIPShortCuts::cAlphaArgs(1));
                ShowContinueError("...Generator fluid nodes must both be entered (or both left blank).");
                ShowContinueError("...Generator fluid inlet node  = " + DataIPShortCuts::cAlphaArgs(9));
                ShowContinueError("...Generator fluid outlet node = " + DataIPShortCuts::cAlphaArgs(10));
                ErrorsFound = true;
            } else {
                //     Generator fluid type must be steam if generator inlet/outlet nodes are not used
                if (IndirectAbsorber(AbsorberNum).GenHeatSourceType == DataLoopNode::NodeType_Water) {
                    ShowWarningError(DataIPShortCuts::cCurrentModuleObject + ", Name=" + DataIPShortCuts::cAlphaArgs(1));
                    ShowContinueError("...Generator fluid type must be Steam if generator inlet/outlet nodes are blank.");
                    ShowContinueError("...Generator fluid type is set to Steam and the simulation continues.");
                    IndirectAbsorber(AbsorberNum).GenHeatSourceType = DataLoopNode::NodeType_Steam;
                }
            }

            {
                auto const SELECT_CASE_var(DataIPShortCuts::cAlphaArgs(6));
                if (SELECT_CASE_var == "CONSTANTFLOW") {
                    IndirectAbsorber(AbsorberNum).FlowMode = ConstantFlow;
                } else if (SELECT_CASE_var == "LEAVINGSETPOINTMODULATED") {
                    IndirectAbsorber(AbsorberNum).FlowMode = LeavingSetPointModulated;
                } else if (SELECT_CASE_var == "NOTMODULATED") {
                    IndirectAbsorber(AbsorberNum).FlowMode = NotModulated;
                } else {
                    ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\",");
                    ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(6) + '=' + DataIPShortCuts::cAlphaArgs(6));
                    ShowContinueError("Available choices are ConstantFlow, NotModulated, or LeavingSetpointModulated");
                    ShowContinueError("Flow mode NotModulated is assumed and the simulation continues.");
                    IndirectAbsorber(AbsorberNum).FlowMode = NotModulated;
                }
            }

            IndirectAbsorber(AbsorberNum).CapFCondenserTempPtr = CurveManager::GetCurveIndex(DataIPShortCuts::cAlphaArgs(11));
            if (IndirectAbsorber(AbsorberNum).CapFCondenserTempPtr > 0) {
                // Verify Curve Object, only legal types are Quadratic or Cubic
                ErrorsFound |= CurveManager::CheckCurveDims(
                    IndirectAbsorber(AbsorberNum).CapFCondenserTempPtr,   // Curve index
                    {1},                            // Valid dimensions
                    RoutineName,                    // Routine name
                    DataIPShortCuts::cCurrentModuleObject,            // Object Type
                    IndirectAbsorber(AbsorberNum).Name,  // Object Name
                    DataIPShortCuts::cAlphaFieldNames(11));               // Field Name
            }

            IndirectAbsorber(AbsorberNum).CapFEvaporatorTempPtr = CurveManager::GetCurveIndex(DataIPShortCuts::cAlphaArgs(12));
            if (IndirectAbsorber(AbsorberNum).CapFEvaporatorTempPtr > 0) {
                // Verify Curve Object, only legal types are Quadratic or Cubic
                ErrorsFound |= CurveManager::CheckCurveDims(
                    IndirectAbsorber(AbsorberNum).CapFEvaporatorTempPtr,   // Curve index
                    {1},                            // Valid dimensions
                    RoutineName,                    // Routine name
                    DataIPShortCuts::cCurrentModuleObject,            // Object Type
                    IndirectAbsorber(AbsorberNum).Name,  // Object Name
                    DataIPShortCuts::cAlphaFieldNames(12));               // Field Name
            }

            IndirectAbsorber(AbsorberNum).CapFGeneratorTempPtr = CurveManager::GetCurveIndex(DataIPShortCuts::cAlphaArgs(13));
            if (IndirectAbsorber(AbsorberNum).CapFGeneratorTempPtr > 0) {
                // Verify Curve Object, only legal types are Quadratic or Cubic
                ErrorsFound |= CurveManager::CheckCurveDims(
                    IndirectAbsorber(AbsorberNum).CapFGeneratorTempPtr,   // Curve index
                    {1},                            // Valid dimensions
                    RoutineName,                    // Routine name
                    DataIPShortCuts::cCurrentModuleObject,            // Object Type
                    IndirectAbsorber(AbsorberNum).Name,  // Object Name
                    DataIPShortCuts::cAlphaFieldNames(13));               // Field Name
            }

            IndirectAbsorber(AbsorberNum).HeatInputFCondTempPtr = CurveManager::GetCurveIndex(DataIPShortCuts::cAlphaArgs(14));
            if (IndirectAbsorber(AbsorberNum).HeatInputFCondTempPtr > 0) {
                // Verify Curve Object, only legal types are Quadratic or Cubic
                ErrorsFound |= CurveManager::CheckCurveDims(
                    IndirectAbsorber(AbsorberNum).HeatInputFCondTempPtr,   // Curve index
                    {1},                            // Valid dimensions
                    RoutineName,                    // Routine name
                    DataIPShortCuts::cCurrentModuleObject,            // Object Type
                    IndirectAbsorber(AbsorberNum).Name,  // Object Name
                    DataIPShortCuts::cAlphaFieldNames(14));               // Field Name
            }

            IndirectAbsorber(AbsorberNum).HeatInputFEvapTempPtr = CurveManager::GetCurveIndex(DataIPShortCuts::cAlphaArgs(15));
            if (IndirectAbsorber(AbsorberNum).HeatInputFEvapTempPtr > 0) {
                // Verify Curve Object, only legal types are Quadratic or Cubic
                ErrorsFound |= CurveManager::CheckCurveDims(
                    IndirectAbsorber(AbsorberNum).HeatInputFEvapTempPtr,   // Curve index
                    {1},                            // Valid dimensions
                    RoutineName,                    // Routine name
                    DataIPShortCuts::cCurrentModuleObject,            // Object Type
                    IndirectAbsorber(AbsorberNum).Name,  // Object Name
                    DataIPShortCuts::cAlphaFieldNames(15));               // Field Name
            }

            // Get remaining data
            IndirectAbsorber(AbsorberNum).MinPartLoadRat = DataIPShortCuts::rNumericArgs(3);
            IndirectAbsorber(AbsorberNum).MaxPartLoadRat = DataIPShortCuts::rNumericArgs(4);
            IndirectAbsorber(AbsorberNum).OptPartLoadRat = DataIPShortCuts::rNumericArgs(5);
            IndirectAbsorber(AbsorberNum).TempDesCondIn = DataIPShortCuts::rNumericArgs(6);
            IndirectAbsorber(AbsorberNum).MinCondInletTemp = DataIPShortCuts::rNumericArgs(7);
            IndirectAbsorber(AbsorberNum).TempLowLimitEvapOut = DataIPShortCuts::rNumericArgs(8);
            IndirectAbsorber(AbsorberNum).EvapVolFlowRate = DataIPShortCuts::rNumericArgs(9);
            if (IndirectAbsorber(AbsorberNum).EvapVolFlowRate == DataSizing::AutoSize) {
                IndirectAbsorber(AbsorberNum).EvapVolFlowRateWasAutoSized = true;
            }
            IndirectAbsorber(AbsorberNum).CondVolFlowRate = DataIPShortCuts::rNumericArgs(10);
            if (IndirectAbsorber(AbsorberNum).CondVolFlowRate == DataSizing::AutoSize) {
                IndirectAbsorber(AbsorberNum).CondVolFlowRateWasAutoSized = true;
            }
            if (NumNums > 10) {
                IndirectAbsorber(AbsorberNum).GeneratorVolFlowRate = DataIPShortCuts::rNumericArgs(11);
                if (IndirectAbsorber(AbsorberNum).GeneratorVolFlowRate == DataSizing::AutoSize) {
                    IndirectAbsorber(AbsorberNum).GeneratorVolFlowRateWasAutoSized = true;
                }
            }

            if (IndirectAbsorber(AbsorberNum).GeneratorVolFlowRate == 0.0 && IndirectAbsorber(AbsorberNum).GenHeatSourceType == DataLoopNode::NodeType_Water) {
                ShowWarningError(DataIPShortCuts::cCurrentModuleObject + ", Name=" + DataIPShortCuts::cAlphaArgs(1));
                ShowContinueError("...Generator water flow rate must be greater than 0 when absorber generator fluid type is hot water.");
                ErrorsFound = true;
            }

            if (NumNums > 11) {
                IndirectAbsorber(AbsorberNum).MinGeneratorInletTemp = DataIPShortCuts::rNumericArgs(12);
            } else {
                IndirectAbsorber(AbsorberNum).MinGeneratorInletTemp = 0.0;
            }

            if (NumNums > 12) {
                IndirectAbsorber(AbsorberNum).GeneratorSubcool = DataIPShortCuts::rNumericArgs(13);
            } else {
                IndirectAbsorber(AbsorberNum).GeneratorSubcool = 0.0;
            }

            if (NumNums > 13) {
                IndirectAbsorber(AbsorberNum).LoopSubcool = DataIPShortCuts::rNumericArgs(14);
            } else {
                IndirectAbsorber(AbsorberNum).LoopSubcool = 0.0;
            }

            if (NumNums > 14) {
                IndirectAbsorber(AbsorberNum).SizFac = DataIPShortCuts::rNumericArgs(15);
            } else {
                IndirectAbsorber(AbsorberNum).SizFac = 1.0;
            }
        }

        if (ErrorsFound) {
            ShowFatalError("Errors found in getting Chiller:Absorption:Indirect");
        }

        for (AbsorberNum = 1; AbsorberNum <= NumIndirectAbsorbers; ++AbsorberNum) {
            SetupOutputVariable("Chiller Electric Power",
                                OutputProcessor::Unit::W,
                                IndirectAbsorberReport(AbsorberNum).PumpingPower,
                                "System",
                                "Average",
                                IndirectAbsorber(AbsorberNum).Name);
            SetupOutputVariable("Chiller Electric Energy",
                                OutputProcessor::Unit::J,
                                IndirectAbsorberReport(AbsorberNum).PumpingEnergy,
                                "System",
                                "Sum",
                                IndirectAbsorber(AbsorberNum).Name,
                                _,
                                "ELECTRICITY",
                                "Cooling",
                                _,
                                "Plant");
            SetupOutputVariable("Chiller Evaporator Cooling Rate",
                                OutputProcessor::Unit::W,
                                IndirectAbsorberReport(AbsorberNum).QEvap,
                                "System",
                                "Average",
                                IndirectAbsorber(AbsorberNum).Name);
            SetupOutputVariable("Chiller Evaporator Cooling Energy",
                                OutputProcessor::Unit::J,
                                IndirectAbsorberReport(AbsorberNum).EvapEnergy,
                                "System",
                                "Sum",
                                IndirectAbsorber(AbsorberNum).Name,
                                _,
                                "ENERGYTRANSFER",
                                "CHILLERS",
                                _,
                                "Plant");
            SetupOutputVariable("Chiller Evaporator Inlet Temperature",
                                OutputProcessor::Unit::C,
                                IndirectAbsorberReport(AbsorberNum).EvapInletTemp,
                                "System",
                                "Average",
                                IndirectAbsorber(AbsorberNum).Name);
            SetupOutputVariable("Chiller Evaporator Outlet Temperature",
                                OutputProcessor::Unit::C,
                                IndirectAbsorberReport(AbsorberNum).EvapOutletTemp,
                                "System",
                                "Average",
                                IndirectAbsorber(AbsorberNum).Name);
            SetupOutputVariable("Chiller Evaporator Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                IndirectAbsorberReport(AbsorberNum).Evapmdot,
                                "System",
                                "Average",
                                IndirectAbsorber(AbsorberNum).Name);

            SetupOutputVariable("Chiller Condenser Heat Transfer Rate",
                                OutputProcessor::Unit::W,
                                IndirectAbsorberReport(AbsorberNum).QCond,
                                "System",
                                "Average",
                                IndirectAbsorber(AbsorberNum).Name);
            SetupOutputVariable("Chiller Condenser Heat Transfer Energy",
                                OutputProcessor::Unit::J,
                                IndirectAbsorberReport(AbsorberNum).CondEnergy,
                                "System",
                                "Sum",
                                IndirectAbsorber(AbsorberNum).Name,
                                _,
                                "ENERGYTRANSFER",
                                "HEATREJECTION",
                                _,
                                "Plant");
            SetupOutputVariable("Chiller Condenser Inlet Temperature",
                                OutputProcessor::Unit::C,
                                IndirectAbsorberReport(AbsorberNum).CondInletTemp,
                                "System",
                                "Average",
                                IndirectAbsorber(AbsorberNum).Name);
            SetupOutputVariable("Chiller Condenser Outlet Temperature",
                                OutputProcessor::Unit::C,
                                IndirectAbsorberReport(AbsorberNum).CondOutletTemp,
                                "System",
                                "Average",
                                IndirectAbsorber(AbsorberNum).Name);
            SetupOutputVariable("Chiller Condenser Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                IndirectAbsorberReport(AbsorberNum).Condmdot,
                                "System",
                                "Average",
                                IndirectAbsorber(AbsorberNum).Name);

            if (IndirectAbsorber(AbsorberNum).GenHeatSourceType == DataLoopNode::NodeType_Water) {
                SetupOutputVariable("Chiller Hot Water Consumption Rate",
                                    OutputProcessor::Unit::W,
                                    IndirectAbsorberReport(AbsorberNum).QGenerator,
                                    "System",
                                    "Average",
                                    IndirectAbsorber(AbsorberNum).Name);
                SetupOutputVariable("Chiller Source Hot Water Energy",
                                    OutputProcessor::Unit::J,
                                    IndirectAbsorberReport(AbsorberNum).GeneratorEnergy,
                                    "System",
                                    "Sum",
                                    IndirectAbsorber(AbsorberNum).Name,
                                    _,
                                    "EnergyTransfer",
                                    "Cooling",
                                    _,
                                    "Plant");
            } else {
                if (GenInputOutputNodesUsed(AbsorberNum)) {
                    SetupOutputVariable("Chiller Source Steam Rate",
                                        OutputProcessor::Unit::W,
                                        IndirectAbsorberReport(AbsorberNum).QGenerator,
                                        "System",
                                        "Average",
                                        IndirectAbsorber(AbsorberNum).Name);
                    SetupOutputVariable("Chiller Source Steam Energy",
                                        OutputProcessor::Unit::J,
                                        IndirectAbsorberReport(AbsorberNum).GeneratorEnergy,
                                        "System",
                                        "Sum",
                                        IndirectAbsorber(AbsorberNum).Name,
                                        _,
                                        "PLANTLOOPHEATINGDEMAND",
                                        "CHILLERS",
                                        _,
                                        "Plant");
                } else {
                    SetupOutputVariable("Chiller Source Steam Rate",
                                        OutputProcessor::Unit::W,
                                        IndirectAbsorberReport(AbsorberNum).QGenerator,
                                        "System",
                                        "Average",
                                        IndirectAbsorber(AbsorberNum).Name);
                    SetupOutputVariable("Chiller Source Steam Energy",
                                        OutputProcessor::Unit::J,
                                        IndirectAbsorberReport(AbsorberNum).GeneratorEnergy,
                                        "System",
                                        "Sum",
                                        IndirectAbsorber(AbsorberNum).Name,
                                        _,
                                        "Steam",
                                        "Cooling",
                                        _,
                                        "Plant");
                }
            }

            SetupOutputVariable("Chiller COP",
                                OutputProcessor::Unit::W_W,
                                IndirectAbsorberReport(AbsorberNum).ActualCOP,
                                "System",
                                "Average",
                                IndirectAbsorber(AbsorberNum).Name);
            SetupOutputVariable("Chiller Part Load Ratio",
                                OutputProcessor::Unit::None,
                                IndirectAbsorberReport(AbsorberNum).ChillerPartLoadRatio,
                                "System",
                                "Average",
                                IndirectAbsorber(AbsorberNum).Name);
            SetupOutputVariable("Chiller Cycling Ratio",
                                OutputProcessor::Unit::None,
                                IndirectAbsorberReport(AbsorberNum).ChillerCyclingFrac,
                                "System",
                                "Average",
                                IndirectAbsorber(AbsorberNum).Name);

            SetupOutputVariable("Chiller Steam Heat Loss Rate",
                                OutputProcessor::Unit::W,
                                IndirectAbsorberReport(AbsorberNum).LoopLoss,
                                "System",
                                "Average",
                                IndirectAbsorber(AbsorberNum).Name);

            if (DataGlobals::AnyEnergyManagementSystemInModel) {
                SetupEMSInternalVariable("Chiller Nominal Capacity", IndirectAbsorber(AbsorberNum).Name, "[W]", IndirectAbsorber(AbsorberNum).NomCap);
            }
        }

        if (allocated(GenInputOutputNodesUsed)) GenInputOutputNodesUsed.deallocate();
    }

    void InitIndirectAbsorpChiller(int const ChillNum, // number of the current electric chiller being simulated
                                   bool const RunFlag, // TRUE when chiller operating
                                   Real64 const MyLoad // requested load
    )
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

        static std::string const RoutineName("InitIndirectAbsorpChiller");

        static bool MyOneTimeFlag(true);
        static Array1D_bool MyFlag;
        static Array1D_bool MyEnvrnFlag;
        int CondInletNode;  // node number of water inlet node to the condenser
        int CondOutletNode; // node number of water outlet node from the condenser
        bool errFlag;
        bool FatalError;
        Real64 rho;          // local fluid density
        Real64 SteamDensity; // density of generator steam (when connected to a steam loop)
        Real64 mdotEvap;     // local fluid mass flow rate thru evaporator
        Real64 mdotCond;     // local fluid mass flow rate thru condenser
        Real64 mdotGen;      // local fluid mass flow rate thru generator

        // Do the one time initializations
        if (MyOneTimeFlag) {
            MyFlag.allocate(NumIndirectAbsorbers);
            MyEnvrnFlag.allocate(NumIndirectAbsorbers);
            MyFlag = true;
            MyEnvrnFlag = true;
            MyOneTimeFlag = false;
        }
        // Init more variables
        if (MyFlag(ChillNum)) {
            // Locate the chillers on the plant loops for later usage
            errFlag = false;
            PlantUtilities::ScanPlantLoopsForObject(IndirectAbsorber(ChillNum).Name,
                                    DataPlant::TypeOf_Chiller_Indirect_Absorption,
                                    IndirectAbsorber(ChillNum).CWLoopNum,
                                    IndirectAbsorber(ChillNum).CWLoopSideNum,
                                    IndirectAbsorber(ChillNum).CWBranchNum,
                                    IndirectAbsorber(ChillNum).CWCompNum,
                                    errFlag,
                                    IndirectAbsorber(ChillNum).TempLowLimitEvapOut,
                                    _,
                                    _,
                                    IndirectAbsorber(ChillNum).EvapInletNodeNum,
                                    _);

            PlantUtilities::ScanPlantLoopsForObject(IndirectAbsorber(ChillNum).Name,
                                    DataPlant::TypeOf_Chiller_Indirect_Absorption,
                                    IndirectAbsorber(ChillNum).CDLoopNum,
                                    IndirectAbsorber(ChillNum).CDLoopSideNum,
                                    IndirectAbsorber(ChillNum).CDBranchNum,
                                    IndirectAbsorber(ChillNum).CDCompNum,
                                    errFlag,
                                    _,
                                    _,
                                    _,
                                    IndirectAbsorber(ChillNum).CondInletNodeNum,
                                    _);
            PlantUtilities::InterConnectTwoPlantLoopSides(IndirectAbsorber(ChillNum).CWLoopNum,
                                          IndirectAbsorber(ChillNum).CWLoopSideNum,
                                          IndirectAbsorber(ChillNum).CDLoopNum,
                                          IndirectAbsorber(ChillNum).CDLoopSideNum,
                                          DataPlant::TypeOf_Chiller_Indirect_Absorption,
                                          true);

            if (IndirectAbsorber(ChillNum).GeneratorInletNodeNum > 0) {
                PlantUtilities::ScanPlantLoopsForObject(IndirectAbsorber(ChillNum).Name,
                                        DataPlant::TypeOf_Chiller_Indirect_Absorption,
                                        IndirectAbsorber(ChillNum).GenLoopNum,
                                        IndirectAbsorber(ChillNum).GenLoopSideNum,
                                        IndirectAbsorber(ChillNum).GenBranchNum,
                                        IndirectAbsorber(ChillNum).GenCompNum,
                                        errFlag,
                                        _,
                                        _,
                                        _,
                                        IndirectAbsorber(ChillNum).GeneratorInletNodeNum,
                                        _);
                PlantUtilities::InterConnectTwoPlantLoopSides(IndirectAbsorber(ChillNum).CWLoopNum,
                                              IndirectAbsorber(ChillNum).CWLoopSideNum,
                                              IndirectAbsorber(ChillNum).GenLoopNum,
                                              IndirectAbsorber(ChillNum).GenCompNum,
                                              DataPlant::TypeOf_Chiller_Indirect_Absorption,
                                              true);
            }

            if ((IndirectAbsorber(ChillNum).CondInletNodeNum > 0) && (IndirectAbsorber(ChillNum).GeneratorInletNodeNum > 0)) {
                PlantUtilities::InterConnectTwoPlantLoopSides(IndirectAbsorber(ChillNum).CDLoopNum,
                                              IndirectAbsorber(ChillNum).CDLoopSideNum,
                                              IndirectAbsorber(ChillNum).GenLoopNum,
                                              IndirectAbsorber(ChillNum).GenCompNum,
                                              DataPlant::TypeOf_Chiller_Indirect_Absorption,
                                              false);
            }
            if (errFlag) {
                ShowFatalError("InitIndirectAbsorpChiller: Program terminated due to previous condition(s).");
            }

            if (IndirectAbsorber(ChillNum).FlowMode == ConstantFlow) {
                // reset flow priority
                DataPlant::PlantLoop(IndirectAbsorber(ChillNum).CWLoopNum)
                    .LoopSide(IndirectAbsorber(ChillNum).CWLoopSideNum)
                    .Branch(IndirectAbsorber(ChillNum).CWBranchNum)
                    .Comp(IndirectAbsorber(ChillNum).CWCompNum)
                    .FlowPriority = DataPlant::LoopFlowStatus_NeedyIfLoopOn;
            }

            if (IndirectAbsorber(ChillNum).FlowMode == LeavingSetPointModulated) {
                // reset flow priority
                DataPlant::PlantLoop(IndirectAbsorber(ChillNum).CWLoopNum)
                    .LoopSide(IndirectAbsorber(ChillNum).CWLoopSideNum)
                    .Branch(IndirectAbsorber(ChillNum).CWBranchNum)
                    .Comp(IndirectAbsorber(ChillNum).CWCompNum)
                    .FlowPriority = DataPlant::LoopFlowStatus_NeedyIfLoopOn;

                if ((DataLoopNode::Node(IndirectAbsorber(ChillNum).EvapOutletNodeNum).TempSetPoint == DataLoopNode::SensedNodeFlagValue) &&
                    (DataLoopNode::Node(IndirectAbsorber(ChillNum).EvapOutletNodeNum).TempSetPointHi == DataLoopNode::SensedNodeFlagValue)) {
                    if (!DataGlobals::AnyEnergyManagementSystemInModel) {
                        if (!IndirectAbsorber(ChillNum).ModulatedFlowErrDone) {
                            ShowWarningError("Missing temperature setpoint for LeavingSetpointModulated mode chiller named " +
                                             IndirectAbsorber(ChillNum).Name);
                            ShowContinueError(
                                "  A temperature setpoint is needed at the outlet node of a chiller in variable flow mode, use a SetpointManager");
                            ShowContinueError("  The overall loop setpoint will be assumed for chiller. The simulation continues ... ");
                            IndirectAbsorber(ChillNum).ModulatedFlowErrDone = true;
                        }
                    } else {
                        // need call to EMS to check node
                        FatalError = false; // but not really fatal yet, but should be.
                        EMSManager::CheckIfNodeSetPointManagedByEMS(IndirectAbsorber(ChillNum).EvapOutletNodeNum, EMSManager::iTemperatureSetPoint, FatalError);
                        if (FatalError) {
                            if (!IndirectAbsorber(ChillNum).ModulatedFlowErrDone) {
                                ShowWarningError("Missing temperature setpoint for LeavingSetpointModulated mode chiller named " +
                                                 IndirectAbsorber(ChillNum).Name);
                                ShowContinueError(
                                    "  A temperature setpoint is needed at the outlet node of a chiller evaporator in variable flow mode");
                                ShowContinueError("  use a Setpoint Manager to establish a setpoint at the chiller evaporator outlet node ");
                                ShowContinueError("  or use an EMS actuator to establish a setpoint at the outlet node ");
                                ShowContinueError("  The overall loop setpoint will be assumed for chiller. The simulation continues ... ");
                                IndirectAbsorber(ChillNum).ModulatedFlowErrDone = true;
                            }
                        }
                    }

                    IndirectAbsorber(ChillNum).ModulatedFlowSetToLoop = true;
                    DataLoopNode::Node(IndirectAbsorber(ChillNum).EvapOutletNodeNum).TempSetPoint =
                        DataLoopNode::Node(DataPlant::PlantLoop(IndirectAbsorber(ChillNum).CWLoopNum).TempSetPointNodeNum).TempSetPoint;
                    DataLoopNode::Node(IndirectAbsorber(ChillNum).EvapOutletNodeNum).TempSetPointHi =
                        DataLoopNode::Node(DataPlant::PlantLoop(IndirectAbsorber(ChillNum).CWLoopNum).TempSetPointNodeNum).TempSetPointHi;
                }
            }

            MyFlag(ChillNum) = false;
        }

        CondInletNode = IndirectAbsorber(ChillNum).CondInletNodeNum;
        CondOutletNode = IndirectAbsorber(ChillNum).CondOutletNodeNum;

        // Initialize Supply Side Variables
        if (MyEnvrnFlag(ChillNum) && DataGlobals::BeginEnvrnFlag && (DataPlant::PlantFirstSizesOkayToFinalize)) {

            rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(IndirectAbsorber(ChillNum).CWLoopNum).FluidName,
                                   DataGlobals::CWInitConvTemp,
                                   DataPlant::PlantLoop(IndirectAbsorber(ChillNum).CWLoopNum).FluidIndex,
                                   RoutineName);

            IndirectAbsorber(ChillNum).EvapMassFlowRateMax = IndirectAbsorber(ChillNum).EvapVolFlowRate * rho;

            PlantUtilities::InitComponentNodes(0.0,
                               IndirectAbsorber(ChillNum).EvapMassFlowRateMax,
                               IndirectAbsorber(ChillNum).EvapInletNodeNum,
                               IndirectAbsorber(ChillNum).EvapOutletNodeNum,
                               IndirectAbsorber(ChillNum).CWLoopNum,
                               IndirectAbsorber(ChillNum).CWLoopSideNum,
                               IndirectAbsorber(ChillNum).CWBranchNum,
                               IndirectAbsorber(ChillNum).CWCompNum);

            rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(IndirectAbsorber(ChillNum).CDLoopNum).FluidName,
                                   DataGlobals::CWInitConvTemp,
                                   DataPlant::PlantLoop(IndirectAbsorber(ChillNum).CDLoopNum).FluidIndex,
                                   RoutineName);

            IndirectAbsorber(ChillNum).CondMassFlowRateMax = rho * IndirectAbsorber(ChillNum).CondVolFlowRate;

            PlantUtilities::InitComponentNodes(0.0,
                               IndirectAbsorber(ChillNum).CondMassFlowRateMax,
                               CondInletNode,
                               CondOutletNode,
                               IndirectAbsorber(ChillNum).CDLoopNum,
                               IndirectAbsorber(ChillNum).CDLoopSideNum,
                               IndirectAbsorber(ChillNum).CDBranchNum,
                               IndirectAbsorber(ChillNum).CDCompNum);

            DataLoopNode::Node(CondInletNode).Temp = IndirectAbsorber(ChillNum).TempDesCondIn;

            if (IndirectAbsorber(ChillNum).GeneratorInletNodeNum > 0) {

                if (IndirectAbsorber(ChillNum).GenHeatSourceType == DataLoopNode::NodeType_Water) {

                    rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(IndirectAbsorber(ChillNum).GenLoopNum).FluidName,
                                           DataGlobals::HWInitConvTemp,
                                           DataPlant::PlantLoop(IndirectAbsorber(ChillNum).GenLoopNum).FluidIndex,
                                           RoutineName);
                    IndirectAbsorber(ChillNum).GenMassFlowRateMax = rho * IndirectAbsorber(ChillNum).GeneratorVolFlowRate;

                } else {
                    SteamDensity = FluidProperties::GetSatDensityRefrig(fluidNameSteam,
                                                       DataLoopNode::Node(IndirectAbsorber(ChillNum).GeneratorInletNodeNum).Temp,
                                                       1.0,
                                                       IndirectAbsorber(ChillNum).SteamFluidIndex,
                                                       calcChillerAbsorptionIndirect + IndirectAbsorber(ChillNum).Name);
                    IndirectAbsorber(ChillNum).GenMassFlowRateMax = SteamDensity * IndirectAbsorber(ChillNum).GeneratorVolFlowRate;
                }

                PlantUtilities::InitComponentNodes(0.0,
                                   IndirectAbsorber(ChillNum).GenMassFlowRateMax,
                                   IndirectAbsorber(ChillNum).GeneratorInletNodeNum,
                                   IndirectAbsorber(ChillNum).GeneratorOutletNodeNum,
                                   IndirectAbsorber(ChillNum).GenLoopNum,
                                   IndirectAbsorber(ChillNum).GenLoopSideNum,
                                   IndirectAbsorber(ChillNum).GenBranchNum,
                                   IndirectAbsorber(ChillNum).GenCompNum);
            }
            MyEnvrnFlag(ChillNum) = false;
        }
        if (!DataGlobals::BeginEnvrnFlag) {
            MyEnvrnFlag(ChillNum) = true;
        }

        if ((IndirectAbsorber(ChillNum).FlowMode == LeavingSetPointModulated) && IndirectAbsorber(ChillNum).ModulatedFlowSetToLoop) {
            // fix for clumsy old input that worked because loop setpoint was spread.
            //  could be removed with transition, testing , model change, period of being obsolete.
            DataLoopNode::Node(IndirectAbsorber(ChillNum).EvapOutletNodeNum).TempSetPoint =
                DataLoopNode::Node(DataPlant::PlantLoop(IndirectAbsorber(ChillNum).CWLoopNum).TempSetPointNodeNum).TempSetPoint;
            DataLoopNode::Node(IndirectAbsorber(ChillNum).EvapOutletNodeNum).TempSetPointHi =
                DataLoopNode::Node(DataPlant::PlantLoop(IndirectAbsorber(ChillNum).CWLoopNum).TempSetPointNodeNum).TempSetPointHi;
        }

        if ((MyLoad < 0.0) && RunFlag) {
            mdotEvap = IndirectAbsorber(ChillNum).EvapMassFlowRateMax;
            mdotCond = IndirectAbsorber(ChillNum).CondMassFlowRateMax;
            mdotGen = IndirectAbsorber(ChillNum).GenMassFlowRateMax;
        } else {
            mdotEvap = 0.0;
            mdotCond = 0.0;
            mdotGen = 0.0;
        }

        PlantUtilities::SetComponentFlowRate(mdotEvap,
                             IndirectAbsorber(ChillNum).EvapInletNodeNum,
                             IndirectAbsorber(ChillNum).EvapOutletNodeNum,
                             IndirectAbsorber(ChillNum).CWLoopNum,
                             IndirectAbsorber(ChillNum).CWLoopSideNum,
                             IndirectAbsorber(ChillNum).CWBranchNum,
                             IndirectAbsorber(ChillNum).CWCompNum);

        PlantUtilities::SetComponentFlowRate(mdotCond,
                             CondInletNode,
                             CondOutletNode,
                             IndirectAbsorber(ChillNum).CDLoopNum,
                             IndirectAbsorber(ChillNum).CDLoopSideNum,
                             IndirectAbsorber(ChillNum).CDBranchNum,
                             IndirectAbsorber(ChillNum).CDCompNum);

        if (IndirectAbsorber(ChillNum).GeneratorInletNodeNum > 0) {

            PlantUtilities::SetComponentFlowRate(mdotGen,
                                 IndirectAbsorber(ChillNum).GeneratorInletNodeNum,
                                 IndirectAbsorber(ChillNum).GeneratorOutletNodeNum,
                                 IndirectAbsorber(ChillNum).GenLoopNum,
                                 IndirectAbsorber(ChillNum).GenLoopSideNum,
                                 IndirectAbsorber(ChillNum).GenBranchNum,
                                 IndirectAbsorber(ChillNum).GenCompNum);
        }
    }

    void SizeIndirectAbsorpChiller(int const ChillNum)
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

        static std::string const RoutineName("SizeIndirectAbsorpChiller");
        static std::string const SizeChillerAbsorptionIndirect("SIZE Chiller:Absorption:Indirect");

        int PltSizIndex;            // Plant Sizing Do loop index
        int PltSizNum;              // Plant Sizing index corresponding to CurLoopNum
        int PltSizCondNum;          // Plant Sizing index for condenser loop
        int PltSizSteamNum;         // Plant Sizing index for steam heating loop
        int PltSizHeatingNum;       // Plant Sizing index for how water heating loop
        Real64 SteamInputRatNom;    // nominal energy input ratio (steam or hot water)
        Real64 SteamDensity;        // density of generator steam (when connected to a steam loop)
        Real64 EnthSteamOutDry;     // dry enthalpy of steam (quality = 1)
        Real64 EnthSteamOutWet;     // wet enthalpy of steam (quality = 0)
        Real64 HfgSteam;            // latent heat of steam at constant pressure
        Real64 SteamDeltaT;         // amount of sub-cooling of steam condensate
        Real64 SteamMassFlowRate;   // steam mass flow rate through generator
        Real64 CpWater;             // specific heat of generator fluid (when connected to a hot water loop)
        Real64 RhoWater;            // density of water (kg/m3)
        Real64 GeneratorOutletTemp; // outlet temperature of generator
        bool ErrorsFound;           // If errors detected in input
        bool LoopErrorsFound;
        std::string equipName;
        Real64 rho;                     // local fluid density
        Real64 Cp;                      // local specific heat
        Real64 tmpNomCap;               // local nominal capacity cooling power
        Real64 tmpNomPumpPower;         // local nominal pump power
        Real64 tmpEvapVolFlowRate;      // local evaporator design volume flow rate
        Real64 tmpCondVolFlowRate;      // local condenser design volume flow rate
        Real64 tmpGeneratorVolFlowRate; // local generator design volume flow rate
        static int DummWaterIndex(1);
        Real64 NomCapUser;               // Hardsized nominal capacity cooling power for reporting
        Real64 NomPumpPowerUser;         // Hardsized local nominal pump power for reporting
        Real64 EvapVolFlowRateUser;      // Hardsized local evaporator design volume flow rate for reporting
        Real64 CondVolFlowRateUser;      // Hardsized local condenser design volume flow rate for reporting
        Real64 GeneratorVolFlowRateUser; // Hardsized local generator design volume flow rate for reporting

        PltSizCondNum = 0;
        PltSizHeatingNum = 0;
        PltSizSteamNum = 0;
        ErrorsFound = false;
        // init local temporary version in case of partial/mixed autosizing
        tmpNomCap = IndirectAbsorber(ChillNum).NomCap;
        tmpEvapVolFlowRate = IndirectAbsorber(ChillNum).EvapVolFlowRate;
        tmpCondVolFlowRate = IndirectAbsorber(ChillNum).CondVolFlowRate;
        tmpGeneratorVolFlowRate = IndirectAbsorber(ChillNum).GeneratorVolFlowRate;

        if (IndirectAbsorber(ChillNum).GeneratorInputCurvePtr > 0) {
            SteamInputRatNom = CurveManager::CurveValue(IndirectAbsorber(ChillNum).GeneratorInputCurvePtr, 1.0);
        } else {
            SteamInputRatNom = 1.0;
        }

        // find the appropriate Plant Sizing object
        // IF (CurLoopNum > 0) THEN
        PltSizNum = DataPlant::PlantLoop(IndirectAbsorber(ChillNum).CWLoopNum).PlantSizNum;
        // END IF

        // IF (IndirectAbsorber(ChillNum)%CondVolFlowRate == AutoSize) THEN
        if (PltSizNum >
            0) { // Autodesk:Std An integer can't be used in a boolean context (most compilers will allow this non-standard usage): Added > 0 patch
            PltSizCondNum = PlantUtilities::MyPlantSizingIndex("Chiller:Absorption:Indirect",
                                               IndirectAbsorber(ChillNum).Name,
                                               IndirectAbsorber(ChillNum).CondInletNodeNum,
                                               IndirectAbsorber(ChillNum).CondOutletNodeNum,
                                               LoopErrorsFound);
        }

        if (IndirectAbsorber(ChillNum).GenHeatSourceType == DataLoopNode::NodeType_Steam) {
            if (IndirectAbsorber(ChillNum).GeneratorInletNodeNum > 0 && IndirectAbsorber(ChillNum).GeneratorOutletNodeNum > 0) {
                PltSizSteamNum = PlantUtilities::MyPlantSizingIndex("Chiller:Absorption:Indirect",
                                                    IndirectAbsorber(ChillNum).Name,
                                                    IndirectAbsorber(ChillNum).GeneratorInletNodeNum,
                                                    IndirectAbsorber(ChillNum).GeneratorOutletNodeNum,
                                                    LoopErrorsFound);
            } else {
                for (PltSizIndex = 1; PltSizIndex <= DataSizing::NumPltSizInput; ++PltSizIndex) {
                    if (DataSizing::PlantSizData(PltSizIndex).LoopType == DataSizing::SteamLoop) {
                        PltSizSteamNum = PltSizIndex;
                    }
                }
            }
        } else {
            if (IndirectAbsorber(ChillNum).GeneratorInletNodeNum > 0 && IndirectAbsorber(ChillNum).GeneratorOutletNodeNum > 0) {
                PltSizHeatingNum = PlantUtilities::MyPlantSizingIndex("Chiller:Absorption:Indirect",
                                                      IndirectAbsorber(ChillNum).Name,
                                                      IndirectAbsorber(ChillNum).GeneratorInletNodeNum,
                                                      IndirectAbsorber(ChillNum).GeneratorOutletNodeNum,
                                                      LoopErrorsFound);
            } else {
                for (PltSizIndex = 1; PltSizIndex <= DataSizing::NumPltSizInput; ++PltSizIndex) {
                    if (DataSizing::PlantSizData(PltSizIndex).LoopType == DataSizing::HeatingLoop) {
                        PltSizHeatingNum = PltSizIndex;
                    }
                }
            }
        }

        if (PltSizNum > 0) {
            if (DataSizing::PlantSizData(PltSizNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {

                Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(IndirectAbsorber(ChillNum).CWLoopNum).FluidName,
                                           DataGlobals::CWInitConvTemp,
                                           DataPlant::PlantLoop(IndirectAbsorber(ChillNum).CWLoopNum).FluidIndex,
                                           RoutineName);

                rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(IndirectAbsorber(ChillNum).CWLoopNum).FluidName,
                                       DataGlobals::CWInitConvTemp,
                                       DataPlant::PlantLoop(IndirectAbsorber(ChillNum).CWLoopNum).FluidIndex,
                                       RoutineName);
                tmpNomCap = Cp * rho * DataSizing::PlantSizData(PltSizNum).DeltaT * DataSizing::PlantSizData(PltSizNum).DesVolFlowRate * IndirectAbsorber(ChillNum).SizFac;
                if (!IndirectAbsorber(ChillNum).NomCapWasAutoSized) tmpNomCap = IndirectAbsorber(ChillNum).NomCap;
            } else {
                if (IndirectAbsorber(ChillNum).NomCapWasAutoSized) tmpNomCap = 0.0;
            }
            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                if (IndirectAbsorber(ChillNum).NomCapWasAutoSized) {
                    IndirectAbsorber(ChillNum).NomCap = tmpNomCap;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(
                            "Chiller:Absorption:Indirect", IndirectAbsorber(ChillNum).Name, "Design Size Nominal Capacity [W]", tmpNomCap);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(
                            "Chiller:Absorption:Indirect", IndirectAbsorber(ChillNum).Name, "Initial Design Size Nominal Capacity [W]", tmpNomCap);
                    }
                } else {
                    if (IndirectAbsorber(ChillNum).NomCap > 0.0 && tmpNomCap > 0.0) {
                        NomCapUser = IndirectAbsorber(ChillNum).NomCap;
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput("Chiller:Absorption:Indirect",
                                               IndirectAbsorber(ChillNum).Name,
                                               "Design Size Nominal Capacity [W]",
                                               tmpNomCap,
                                               "User-Specified Nominal Capacity [W]",
                                               NomCapUser);
                            if (DataGlobals::DisplayExtraWarnings) {
                                if ((std::abs(tmpNomCap - NomCapUser) / NomCapUser) > DataSizing::AutoVsHardSizingThreshold) {
                                    ShowMessage("SizeChillerAbsorptionIndirect: Potential issue with equipment sizing for " +
                                                IndirectAbsorber(ChillNum).Name);
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
            if (IndirectAbsorber(ChillNum).NomCapWasAutoSized) {
                if (DataPlant::PlantFirstSizesOkayToFinalize) {
                    ShowSevereError("Autosizing of Absorption Chiller nominal capacity requires a loop Sizing:Plant object");
                    ShowContinueError("Occurs in Chiller:Absorption:Indirect object=" + IndirectAbsorber(ChillNum).Name);
                    ErrorsFound = true;
                }
            } else {
                if (DataPlant::PlantFinalSizesOkayToReport) {
                    if (IndirectAbsorber(ChillNum).NomCap > 0.0) {
                        ReportSizingManager::ReportSizingOutput("Chiller:Absorption:Indirect",
                                           IndirectAbsorber(ChillNum).Name,
                                           "User-Specified Nominal Capacity [W]",
                                           IndirectAbsorber(ChillNum).NomCap);
                    }
                }
            }
        }

        tmpNomPumpPower = 0.0045 * tmpNomCap;
        if (DataPlant::PlantFirstSizesOkayToFinalize) {
            // the DOE-2 EIR for single stage absorption chiller
            if (IndirectAbsorber(ChillNum).NomPumpPowerWasAutoSized) {
                IndirectAbsorber(ChillNum).NomPumpPower = tmpNomPumpPower; // 0.0045d0 * IndirectAbsorber(ChillNum)%NomCap
                if (DataPlant::PlantFinalSizesOkayToReport) {
                    ReportSizingManager::ReportSizingOutput(
                        "Chiller:Absorption:Indirect", IndirectAbsorber(ChillNum).Name, "Design Size Nominal Pumping Power [W]", tmpNomPumpPower);
                }
                if (DataPlant::PlantFirstSizesOkayToReport) {
                    ReportSizingManager::ReportSizingOutput("Chiller:Absorption:Indirect",
                                       IndirectAbsorber(ChillNum).Name,
                                       "Initial Design Size Nominal Pumping Power [W]",
                                       tmpNomPumpPower);
                }
            } else {
                if (IndirectAbsorber(ChillNum).NomPumpPower > 0.0 && tmpNomPumpPower > 0.0) {
                    NomPumpPowerUser = IndirectAbsorber(ChillNum).NomPumpPower;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput("Chiller:Absorption:Indirect",
                                           IndirectAbsorber(ChillNum).Name,
                                           "Design Size Nominal Pumping Power [W]",
                                           tmpNomPumpPower,
                                           "User-Specified Nominal Pumping Power [W]",
                                           NomPumpPowerUser);
                        if (DataGlobals::DisplayExtraWarnings) {
                            if ((std::abs(tmpNomPumpPower - NomPumpPowerUser) / NomPumpPowerUser) > DataSizing::AutoVsHardSizingThreshold) {
                                ShowMessage("SizeChillerAbsorptionIndirect: Potential issue with equipment sizing for " +
                                            IndirectAbsorber(ChillNum).Name);
                                ShowContinueError("User-Specified Nominal Pumping Power of " + General::RoundSigDigits(NomPumpPowerUser, 2) + " [W]");
                                ShowContinueError("differs from Design Size Nominal Pumping Power of " + General::RoundSigDigits(tmpNomPumpPower, 2) + " [W]");
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
            if (DataSizing::PlantSizData(PltSizNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                tmpEvapVolFlowRate = DataSizing::PlantSizData(PltSizNum).DesVolFlowRate * IndirectAbsorber(ChillNum).SizFac;
                if (!IndirectAbsorber(ChillNum).EvapVolFlowRateWasAutoSized) tmpEvapVolFlowRate = IndirectAbsorber(ChillNum).EvapVolFlowRate;
            } else {
                if (IndirectAbsorber(ChillNum).EvapVolFlowRateWasAutoSized) tmpEvapVolFlowRate = 0.0;
            }
            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                if (IndirectAbsorber(ChillNum).EvapVolFlowRateWasAutoSized) {
                    IndirectAbsorber(ChillNum).EvapVolFlowRate = tmpEvapVolFlowRate;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput("Chiller:Absorption:Indirect",
                                           IndirectAbsorber(ChillNum).Name,
                                           "Design Size Design Chilled Water Flow Rate [m3/s]",
                                           tmpEvapVolFlowRate);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput("Chiller:Absorption:Indirect",
                                           IndirectAbsorber(ChillNum).Name,
                                           "Initial Design Size Design Chilled Water Flow Rate [m3/s]",
                                           tmpEvapVolFlowRate);
                    }
                } else {
                    if (IndirectAbsorber(ChillNum).EvapVolFlowRate > 0.0 && tmpEvapVolFlowRate > 0.0) {
                        EvapVolFlowRateUser = IndirectAbsorber(ChillNum).EvapVolFlowRate;
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput("Chiller:Absorption:Indirect",
                                               IndirectAbsorber(ChillNum).Name,
                                               "Design Size Design Chilled Water Flow Rate [m3/s]",
                                               tmpEvapVolFlowRate,
                                               "User-Specified Design Chilled Water Flow Rate [m3/s]",
                                               EvapVolFlowRateUser);
                            if (DataGlobals::DisplayExtraWarnings) {
                                if ((std::abs(tmpEvapVolFlowRate - EvapVolFlowRateUser) / EvapVolFlowRateUser) > DataSizing::AutoVsHardSizingThreshold) {
                                    ShowMessage("SizeChillerElectricIndirect: Potential issue with equipment sizing for " +
                                                IndirectAbsorber(ChillNum).Name);
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
            if (IndirectAbsorber(ChillNum).EvapVolFlowRateWasAutoSized) {
                if (DataPlant::PlantFirstSizesOkayToFinalize) {
                    ShowSevereError("Autosizing of Absorption Chiller evap flow rate requires a loop Sizing:Plant object");
                    ShowContinueError("Occurs in Chiller:Absorption:Indirect object=" + IndirectAbsorber(ChillNum).Name);
                    ErrorsFound = true;
                }
            } else {
                if (DataPlant::PlantFinalSizesOkayToReport) {
                    if (IndirectAbsorber(ChillNum).EvapVolFlowRate > 0.0) {
                        ReportSizingManager::ReportSizingOutput("Chiller:Absorption:Indirect",
                                           IndirectAbsorber(ChillNum).Name,
                                           "User-Specified Design Chilled Water Flow Rate [m3/s]",
                                           IndirectAbsorber(ChillNum).EvapVolFlowRate);
                    }
                }
            }
        }

        if (DataPlant::PlantFirstSizesOkayToFinalize) {
            PlantUtilities::RegisterPlantCompDesignFlow(IndirectAbsorber(ChillNum).EvapInletNodeNum, IndirectAbsorber(ChillNum).EvapVolFlowRate);
        } else {
            PlantUtilities::RegisterPlantCompDesignFlow(IndirectAbsorber(ChillNum).EvapInletNodeNum, tmpEvapVolFlowRate);
        }

        if (PltSizCondNum > 0 && PltSizNum > 0) {
            if (IndirectAbsorber(ChillNum).EvapVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow && tmpNomCap > 0.0) {
                //       QCondenser = QEvaporator + QGenerator + PumpingPower

                Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(IndirectAbsorber(ChillNum).CDLoopNum).FluidName,
                                           DataGlobals::CWInitConvTemp,
                                           DataPlant::PlantLoop(IndirectAbsorber(ChillNum).CDLoopNum).FluidIndex,
                                           RoutineName);

                rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(IndirectAbsorber(ChillNum).CDLoopNum).FluidName,
                                       DataGlobals::CWInitConvTemp,
                                       DataPlant::PlantLoop(IndirectAbsorber(ChillNum).CDLoopNum).FluidIndex,
                                       RoutineName);
                tmpCondVolFlowRate =
                    tmpNomCap * (1.0 + SteamInputRatNom + tmpNomPumpPower / tmpNomCap) / (DataSizing::PlantSizData(PltSizCondNum).DeltaT * Cp * rho);
                if (!IndirectAbsorber(ChillNum).CondVolFlowRateWasAutoSized) tmpCondVolFlowRate = IndirectAbsorber(ChillNum).CondVolFlowRate;
            } else {
                if (IndirectAbsorber(ChillNum).CondVolFlowRateWasAutoSized) tmpCondVolFlowRate = 0.0;
            }
            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                if (IndirectAbsorber(ChillNum).CondVolFlowRateWasAutoSized) {
                    IndirectAbsorber(ChillNum).CondVolFlowRate = tmpCondVolFlowRate;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput("Chiller:Absorption:Indirect",
                                           IndirectAbsorber(ChillNum).Name,
                                           "Design Size Design Condenser Water Flow Rate [m3/s]",
                                           tmpCondVolFlowRate);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput("Chiller:Absorption:Indirect",
                                           IndirectAbsorber(ChillNum).Name,
                                           "Initial Design Size Design Condenser Water Flow Rate [m3/s]",
                                           tmpCondVolFlowRate);
                    }
                } else {
                    if (IndirectAbsorber(ChillNum).CondVolFlowRate > 0.0 && tmpCondVolFlowRate > 0.0) {
                        CondVolFlowRateUser = IndirectAbsorber(ChillNum).CondVolFlowRate;
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput("Chiller:Absorption:Indirect",
                                               IndirectAbsorber(ChillNum).Name,
                                               "Design Size Design Condenser Water Flow Rate [m3/s]",
                                               tmpCondVolFlowRate,
                                               "User-Specified Design Condenser Water Flow Rate [m3/s]",
                                               CondVolFlowRateUser);
                            if (DataGlobals::DisplayExtraWarnings) {
                                if ((std::abs(tmpCondVolFlowRate - CondVolFlowRateUser) / CondVolFlowRateUser) > DataSizing::AutoVsHardSizingThreshold) {
                                    ShowMessage("SizeChillerAbsorptionIndirect: Potential issue with equipment sizing for " +
                                                IndirectAbsorber(ChillNum).Name);
                                    ShowContinueError("User-Specified Design Condenser Water Flow Rate of " + General::RoundSigDigits(CondVolFlowRateUser, 5) +
                                                      " [m3/s]");
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
            if (IndirectAbsorber(ChillNum).CondVolFlowRateWasAutoSized) {
                if (DataPlant::PlantFirstSizesOkayToFinalize) {
                    ShowSevereError("Autosizing of Absorption Chiller condenser flow rate requires a condenser");
                    ShowContinueError("loop Sizing:Plant object");
                    ShowContinueError("Occurs in Chiller:Absorption:Indirect object=" + IndirectAbsorber(ChillNum).Name);
                    ErrorsFound = true;
                }
            } else {
                if (DataPlant::PlantFinalSizesOkayToReport) {
                    if (IndirectAbsorber(ChillNum).CondVolFlowRate > 0.0) {
                        ReportSizingManager::ReportSizingOutput("Chiller:Absorption:Indirect",
                                           IndirectAbsorber(ChillNum).Name,
                                           "User-Specified Design Condenser Water Flow Rate [m3/s]",
                                           IndirectAbsorber(ChillNum).CondVolFlowRate);
                    }
                }
            }
        }

        // save the design condenser water volumetric flow rate for use by the condenser water loop sizing algorithms
        if (DataPlant::PlantFirstSizesOkayToFinalize) {
            PlantUtilities::RegisterPlantCompDesignFlow(IndirectAbsorber(ChillNum).CondInletNodeNum, IndirectAbsorber(ChillNum).CondVolFlowRate);
        } else {
            PlantUtilities::RegisterPlantCompDesignFlow(IndirectAbsorber(ChillNum).CondInletNodeNum, tmpCondVolFlowRate);
        }

        if ((PltSizSteamNum > 0 && IndirectAbsorber(ChillNum).GenHeatSourceType == DataLoopNode::NodeType_Steam) ||
            (PltSizHeatingNum > 0 && IndirectAbsorber(ChillNum).GenHeatSourceType == DataLoopNode::NodeType_Water)) {
            if (IndirectAbsorber(ChillNum).EvapVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow && tmpNomCap > 0.0) {
                if (IndirectAbsorber(ChillNum).GenHeatSourceType == DataLoopNode::NodeType_Water) {
                    CpWater = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(IndirectAbsorber(ChillNum).GenLoopNum).FluidName,
                                                    DataSizing::PlantSizData(PltSizHeatingNum).ExitTemp,
                                                    DataPlant::PlantLoop(IndirectAbsorber(ChillNum).GenLoopNum).FluidIndex,
                                                    RoutineName);
                    SteamDeltaT = max(0.5, DataSizing::PlantSizData(PltSizHeatingNum).DeltaT);

                    RhoWater = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(IndirectAbsorber(ChillNum).GenLoopNum).FluidName,
                                                (DataSizing::PlantSizData(PltSizHeatingNum).ExitTemp - SteamDeltaT),
                                                DataPlant::PlantLoop(IndirectAbsorber(ChillNum).GenLoopNum).FluidIndex,
                                                RoutineName);
                    tmpGeneratorVolFlowRate = (tmpNomCap * SteamInputRatNom) / (CpWater * SteamDeltaT * RhoWater);
                    if (!IndirectAbsorber(ChillNum).GeneratorVolFlowRateWasAutoSized)
                        tmpGeneratorVolFlowRate = IndirectAbsorber(ChillNum).GeneratorVolFlowRate;
                    if (DataPlant::PlantFirstSizesOkayToFinalize) {
                        if (IndirectAbsorber(ChillNum).GeneratorVolFlowRateWasAutoSized) {
                            IndirectAbsorber(ChillNum).GeneratorVolFlowRate = tmpGeneratorVolFlowRate;
                            if (DataPlant::PlantFinalSizesOkayToReport) {
                                ReportSizingManager::ReportSizingOutput("Chiller:Absorption:Indirect",
                                                   IndirectAbsorber(ChillNum).Name,
                                                   "Design Size Design Generator Fluid Flow Rate [m3/s]",
                                                   tmpGeneratorVolFlowRate);
                            }
                            if (DataPlant::PlantFirstSizesOkayToReport) {
                                ReportSizingManager::ReportSizingOutput("Chiller:Absorption:Indirect",
                                                   IndirectAbsorber(ChillNum).Name,
                                                   "Initial Design Size Design Generator Fluid Flow Rate [m3/s]",
                                                   tmpGeneratorVolFlowRate);
                            }
                        } else {
                            if (IndirectAbsorber(ChillNum).GeneratorVolFlowRate > 0.0 && tmpGeneratorVolFlowRate > 0.0) {
                                GeneratorVolFlowRateUser = IndirectAbsorber(ChillNum).GeneratorVolFlowRate;
                                if (DataPlant::PlantFinalSizesOkayToReport) {
                                    ReportSizingManager::ReportSizingOutput("Chiller:Absorption:Indirect",
                                                       IndirectAbsorber(ChillNum).Name,
                                                       "Design Size Design Generator Fluid Flow Rate [m3/s]",
                                                       tmpGeneratorVolFlowRate,
                                                       "User-Specified Design Generator Fluid Flow Rate [m3/s]",
                                                       GeneratorVolFlowRateUser);
                                    if (DataGlobals::DisplayExtraWarnings) {
                                        if ((std::abs(tmpGeneratorVolFlowRate - GeneratorVolFlowRateUser) / GeneratorVolFlowRateUser) >
                                            DataSizing::AutoVsHardSizingThreshold) {
                                            ShowMessage("SizeChillerAbsorptionIndirect: Potential issue with equipment sizing for " +
                                                        IndirectAbsorber(ChillNum).Name);
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
                    SteamDensity = FluidProperties::GetSatDensityRefrig(fluidNameSteam,
                                                       DataSizing::PlantSizData(PltSizSteamNum).ExitTemp,
                                                       1.0,
                                                       IndirectAbsorber(ChillNum).SteamFluidIndex,
                                                       SizeChillerAbsorptionIndirect + IndirectAbsorber(ChillNum).Name);
                    SteamDeltaT = DataSizing::PlantSizData(PltSizSteamNum).DeltaT;
                    GeneratorOutletTemp = DataSizing::PlantSizData(PltSizSteamNum).ExitTemp - SteamDeltaT;

                    EnthSteamOutDry = FluidProperties::GetSatEnthalpyRefrig(fluidNameSteam,
                                                           DataSizing::PlantSizData(PltSizSteamNum).ExitTemp,
                                                           1.0,
                                                           IndirectAbsorber(ChillNum).SteamFluidIndex,
                                                           SizeChillerAbsorptionIndirect + IndirectAbsorber(ChillNum).Name);
                    EnthSteamOutWet = FluidProperties::GetSatEnthalpyRefrig(fluidNameSteam,
                                                           DataSizing::PlantSizData(PltSizSteamNum).ExitTemp,
                                                           0.0,
                                                           IndirectAbsorber(ChillNum).SteamFluidIndex,
                                                           SizeChillerAbsorptionIndirect + IndirectAbsorber(ChillNum).Name);
                    CpWater = FluidProperties::GetSpecificHeatGlycol(fluidNameWater, GeneratorOutletTemp, DummWaterIndex, RoutineName);
                    HfgSteam = EnthSteamOutDry - EnthSteamOutWet;
                    //         calculate the mass flow rate through the generator
                    SteamMassFlowRate = (tmpNomCap * SteamInputRatNom) / ((HfgSteam) + (SteamDeltaT * CpWater));
                    //         calculate the steam volumetric flow rate
                    tmpGeneratorVolFlowRate = SteamMassFlowRate / SteamDensity;
                    if (!IndirectAbsorber(ChillNum).GeneratorVolFlowRateWasAutoSized)
                        tmpGeneratorVolFlowRate = IndirectAbsorber(ChillNum).GeneratorVolFlowRate;
                    if (DataPlant::PlantFirstSizesOkayToFinalize) {
                        if (IndirectAbsorber(ChillNum).GeneratorVolFlowRateWasAutoSized) {
                            IndirectAbsorber(ChillNum).GeneratorVolFlowRate = tmpGeneratorVolFlowRate;
                            if (DataPlant::PlantFinalSizesOkayToReport) {
                                ReportSizingManager::ReportSizingOutput("Chiller:Absorption:Indirect",
                                                   IndirectAbsorber(ChillNum).Name,
                                                   "Design Size Design Generator Fluid Flow Rate [m3/s]",
                                                   tmpGeneratorVolFlowRate);
                            }
                            if (DataPlant::PlantFirstSizesOkayToReport) {
                                ReportSizingManager::ReportSizingOutput("Chiller:Absorption:Indirect",
                                                   IndirectAbsorber(ChillNum).Name,
                                                   "Initial Design Size Design Generator Fluid Flow Rate [m3/s]",
                                                   tmpGeneratorVolFlowRate);
                            }
                        } else {
                            if (IndirectAbsorber(ChillNum).GeneratorVolFlowRate > 0.0 && tmpGeneratorVolFlowRate > 0.0) {
                                GeneratorVolFlowRateUser = IndirectAbsorber(ChillNum).GeneratorVolFlowRate;
                                if (DataPlant::PlantFinalSizesOkayToReport) {
                                    ReportSizingManager::ReportSizingOutput("Chiller:Absorption:Indirect",
                                                       IndirectAbsorber(ChillNum).Name,
                                                       "Design Size Design Generator Fluid Flow Rate [m3/s]",
                                                       tmpGeneratorVolFlowRate,
                                                       "User-Specified Design Generator Fluid Flow Rate [m3/s]",
                                                       GeneratorVolFlowRateUser);
                                    if (DataGlobals::DisplayExtraWarnings) {
                                        if ((std::abs(tmpGeneratorVolFlowRate - GeneratorVolFlowRateUser) / GeneratorVolFlowRateUser) >
                                            DataSizing::AutoVsHardSizingThreshold) {
                                            ShowMessage("SizeChillerAbsorptionIndirect: Potential issue with equipment sizing for " +
                                                        IndirectAbsorber(ChillNum).Name);
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
                if (IndirectAbsorber(ChillNum).GeneratorVolFlowRateWasAutoSized) {
                    if (DataPlant::PlantFirstSizesOkayToFinalize) {
                        IndirectAbsorber(ChillNum).GeneratorVolFlowRate = 0.0;
                    } else {
                        tmpGeneratorVolFlowRate = 0.0;
                    }
                }
            }
        } else {
            if (IndirectAbsorber(ChillNum).GeneratorVolFlowRateWasAutoSized) {
                if (DataPlant::PlantFirstSizesOkayToFinalize) {
                    ShowSevereError("Autosizing of Absorption Chiller generator flow rate requires a loop Sizing:Plant object.");
                    ShowContinueError(" For steam loops, use a steam Sizing:Plant object.");
                    ShowContinueError(" For hot water loops, use a heating Sizing:Plant object.");
                    ShowContinueError("Occurs in Chiller:Absorption:Indirect object=" + IndirectAbsorber(ChillNum).Name);
                    ErrorsFound = true;
                }
            } else {
                if (DataPlant::PlantFinalSizesOkayToReport) {
                    if (IndirectAbsorber(ChillNum).GeneratorVolFlowRate > 0.0) {
                        ReportSizingManager::ReportSizingOutput("Chiller:Absorption:Indirect",
                                           IndirectAbsorber(ChillNum).Name,
                                           "User-Specified Design Generator Fluid Flow Rate [m3/s]",
                                           IndirectAbsorber(ChillNum).GeneratorVolFlowRate);
                    }
                }
            }
        }

        // save the design steam or hot water volumetric flow rate for use by the steam or hot water loop sizing algorithms
        if (DataPlant::PlantFirstSizesOkayToFinalize) {
            PlantUtilities::RegisterPlantCompDesignFlow(IndirectAbsorber(ChillNum).GeneratorInletNodeNum, IndirectAbsorber(ChillNum).GeneratorVolFlowRate);
        } else {
            PlantUtilities::RegisterPlantCompDesignFlow(IndirectAbsorber(ChillNum).GeneratorInletNodeNum, tmpGeneratorVolFlowRate);
        }

        if (IndirectAbsorber(ChillNum).GeneratorDeltaTempWasAutoSized) {
            if (PltSizHeatingNum > 0 && IndirectAbsorber(ChillNum).GenHeatSourceType == DataLoopNode::NodeType_Water) {
                IndirectAbsorber(ChillNum).GeneratorDeltaTemp = max(0.5, DataSizing::PlantSizData(PltSizHeatingNum).DeltaT);
            } else if (IndirectAbsorber(ChillNum).GenHeatSourceType == DataLoopNode::NodeType_Water) {
                rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(IndirectAbsorber(ChillNum).GenLoopNum).FluidName,
                                       DataGlobals::HWInitConvTemp,
                                       DataPlant::PlantLoop(IndirectAbsorber(ChillNum).GenLoopNum).FluidIndex,
                                       RoutineName);
                CpWater = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(IndirectAbsorber(ChillNum).GenLoopNum).FluidName,
                                                DataSizing::PlantSizData(PltSizHeatingNum).ExitTemp,
                                                DataPlant::PlantLoop(IndirectAbsorber(ChillNum).GenLoopNum).FluidIndex,
                                                RoutineName);
                if (DataPlant::PlantFirstSizesOkayToFinalize) {
                    IndirectAbsorber(ChillNum).GeneratorDeltaTemp =
                        (SteamInputRatNom * IndirectAbsorber(ChillNum).NomCap) / (CpWater * rho * IndirectAbsorber(ChillNum).GeneratorVolFlowRate);
                }
            }
        }

        if (ErrorsFound) {
            ShowFatalError("Preceding sizing errors cause program termination");
        }

        if (DataPlant::PlantFinalSizesOkayToReport) {
            // create predefined report
            equipName = IndirectAbsorber(ChillNum).Name;
            OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchMechType, equipName, "Chiller:Absorption:Indirect");
            OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchMechNomEff, equipName, "n/a");
            OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchMechNomCap, equipName, IndirectAbsorber(ChillNum).NomCap);
        }
    }

    void CalcIndirectAbsorberModel(int const ChillNum,                   // Absorber number
                                   Real64 const MyLoad,                  // operating load
                                   bool const RunFlag,                   // TRUE when Absorber operating
                                   bool const EP_UNUSED(FirstIteration), // TRUE when first iteration of timestep !unused1208
                                   int const EquipFlowCtrl               // Flow control mode for the equipment
    )
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

        static std::string const RoutineName("CalcIndirectAbsorberModel");
        static std::string const LoopLossesChillerAbsorptionIndirect("Loop Losses: Chiller:Absorption:Indirect");
        static std::string const LoopLossesChillerAbsorptionIndirectSpace("Loop Losses: Chiller:Absorption:Indirect ");

        Real64 MinPartLoadRat;           // min allowed operating frac full load
        Real64 TempCondIn;               // C - (BLAST ADJTC(1)The design secondary loop fluid
        Real64 EvapInletTemp;            // C - evaporator inlet temperature, water side
        Real64 CondInletTemp;            // C - condenser inlet temperature, water side
        Real64 TempEvapOut;              // C - evaporator outlet temperature, water side
        Real64 TempEvapOutSetPoint(0.0); // C - evaporator outlet temperature setpoint
        Real64 AbsorberNomCap;           // Absorber nominal capacity
        Real64 NomPumpPower;             // Absorber nominal pumping power
        Real64 PartLoadRat;              // part load ratio for efficiency calc
        Real64 OperPartLoadRat;          // Operating part load ratio
        Real64 EvapDeltaTemp(0.0);       // C - evaporator temperature difference, water side
        Real64 TempLowLimitEout;         // C - Evaporator low temp. limit cut off
        Real64 HeatInputRat;             // generator heat input ratio
        Real64 ElectricInputRat;         // energy input ratio
        int EvapInletNode;               // evaporator inlet node number, water side
        int EvapOutletNode;              // evaporator outlet node number, water side
        int CondInletNode;               // condenser inlet node number, water side
        int GeneratorInletNode;          // generator inlet node number, steam/water side
        int GeneratorOutletNode;         // generator outlet node number, steam/water side
        Real64 EnthSteamOutDry;          // enthalpy of dry steam at generator inlet
        Real64 EnthSteamOutWet;          // enthalpy of wet steam at generator inlet
        Real64 HfgSteam;                 // heat of vaporization of steam
        static Array1D_bool MyEnvironFlag;
        static Array1D_bool MyEnvironSteamFlag;
        static bool OneTimeFlag(true);
        Real64 FRAC;                    // fraction of time step chiller cycles
        static bool PossibleSubcooling; // flag to determine if supply water temperature is below setpoint
        Real64 CpFluid;                 // specific heat of generator fluid
        Real64 SteamDeltaT;             // temperature difference of fluid through generator
        Real64 SteamOutletTemp;         // generator outlet temperature
        Real64 CapacityfAbsorberTemp;   // performance curve output
        Real64 CapacityfEvaporatorTemp; // performance curve output
        Real64 CapacityfGeneratorTemp;  // performance curve output
        Real64 HeatInputfCondTemp;      // performance curve output
        Real64 HeatInputfEvapTemp;      // performance curve output
        Real64 TempWaterAtmPress;       // temperature of condensed steam leaving generator (after condensate trap)
        Real64 TempLoopOutToPump;       // temperature of condensed steam entering pump (includes loop losses)
        Real64 EnthAtAtmPress;          // enthalpy  of condensed steam leaving generator (after condensate trap)
        Real64 EnthPumpInlet;           // enthalpy of condensed steam entering pump (includes loop losses)
        int LoopSideNum;
        int LoopNum;
        static int DummyWaterIndex(1);

        if (OneTimeFlag) {
            MyEnvironFlag.allocate(NumIndirectAbsorbers);
            MyEnvironSteamFlag.allocate(NumIndirectAbsorbers);
            MyEnvironFlag = true;
            MyEnvironSteamFlag = true;
            OneTimeFlag = false;
        }

        // set module level inlet and outlet nodes
        IndirectAbsorber(ChillNum).EvapMassFlowRate = 0.0;
        IndirectAbsorber(ChillNum).CondMassFlowRate = 0.0;
        IndirectAbsorber(ChillNum).GenMassFlowRate = 0.0;
        IndirectAbsorber(ChillNum).QCondenser = 0.0;
        IndirectAbsorber(ChillNum).QEvaporator = 0.0;
        IndirectAbsorber(ChillNum).QGenerator = 0.0;
        IndirectAbsorber(ChillNum).PumpingEnergy = 0.0;
        IndirectAbsorber(ChillNum).CondenserEnergy = 0.0;
        IndirectAbsorber(ChillNum).EvaporatorEnergy = 0.0;
        IndirectAbsorber(ChillNum).GeneratorEnergy = 0.0;
        IndirectAbsorber(ChillNum).PumpingPower = 0.0;
        IndirectAbsorber(ChillNum).ChillerONOFFCyclingFrac = 0.0;
        EvapInletNode = IndirectAbsorber(ChillNum).EvapInletNodeNum;
        EvapOutletNode = IndirectAbsorber(ChillNum).EvapOutletNodeNum;
        CondInletNode = IndirectAbsorber(ChillNum).CondInletNodeNum;
        GeneratorInletNode = IndirectAbsorber(ChillNum).GeneratorInletNodeNum;
        GeneratorOutletNode = IndirectAbsorber(ChillNum).GeneratorOutletNodeNum;

        //  If no loop demand or Absorber OFF, return
        if (MyLoad >= 0.0 || !RunFlag) {
            if (EquipFlowCtrl == DataBranchAirLoopPlant::ControlType_SeriesActive) IndirectAbsorber(ChillNum).EvapMassFlowRate = DataLoopNode::Node(EvapInletNode).MassFlowRate;
            return;
        }

        // Warn if entering condenser water temperature is below minimum
        if (DataLoopNode::Node(CondInletNode).Temp < IndirectAbsorber(ChillNum).MinCondInletTemp) {
            if (!DataGlobals::WarmupFlag) {
                if (IndirectAbsorber(ChillNum).MinCondInletTempCtr < 1) {
                    ++IndirectAbsorber(ChillNum).MinCondInletTempCtr;
                    ShowWarningError("Chiller:Absorption:Indirect \"" + IndirectAbsorber(ChillNum).Name + "\"");
                    ShowContinueError("...Entering condenser water temperature below specified minimum (" +
                                      General::RoundSigDigits(IndirectAbsorber(ChillNum).MinCondInletTemp, 3) + " C).");
                    ShowContinueError("...Entering condenser water temperature = " + General::RoundSigDigits(DataLoopNode::Node(CondInletNode).Temp, 3) + " C.");
                    ShowContinueErrorTimeStamp("...simulation continues.");
                } else {
                    ShowRecurringWarningErrorAtEnd("Entering condenser water temperature below specified minimum error continues.",
                                                   IndirectAbsorber(ChillNum).MinCondInletTempIndex,
                                                   DataLoopNode::Node(CondInletNode).Temp,
                                                   DataLoopNode::Node(CondInletNode).Temp);
                }
            }
        }

        // Warn if entering generator fluid temperature is below minimum
        if (GeneratorInletNode > 0) {
            if (DataLoopNode::Node(GeneratorInletNode).Temp < IndirectAbsorber(ChillNum).MinGeneratorInletTemp) {
                if (!DataGlobals::WarmupFlag) {
                    if (IndirectAbsorber(ChillNum).MinGenInletTempCtr < 1) {
                        ++IndirectAbsorber(ChillNum).MinGenInletTempCtr;
                        ShowWarningError("Chiller:Absorption:Indirect \"" + IndirectAbsorber(ChillNum).Name + "\"");
                        ShowContinueError("...Entering generator fluid temperature below specified minimum (" +
                                          General::RoundSigDigits(IndirectAbsorber(ChillNum).MinGeneratorInletTemp, 3) + " C).");
                        ShowContinueError("...Entering generator fluid temperature = " + General::RoundSigDigits(DataLoopNode::Node(GeneratorInletNode).Temp, 3) + " C.");
                        ShowContinueErrorTimeStamp("...simulation continues.");
                    } else {
                        ShowRecurringWarningErrorAtEnd("Entering generator fluid temperature below specified minimum error continues.",
                                                       IndirectAbsorber(ChillNum).MinGenInletTempIndex,
                                                       DataLoopNode::Node(GeneratorInletNode).Temp,
                                                       DataLoopNode::Node(GeneratorInletNode).Temp);
                    }
                }
            }
        }

        // Set module level Absorber inlet and temperature variables
        EvapInletTemp = DataLoopNode::Node(EvapInletNode).Temp;
        CondInletTemp = DataLoopNode::Node(CondInletNode).Temp;

        // Set the condenser mass flow rates
        IndirectAbsorber(ChillNum).CondMassFlowRate = DataLoopNode::Node(CondInletNode).MassFlowRate;

        // LOAD LOCAL VARIABLES FROM DATA STRUCTURE (for code readability)
        MinPartLoadRat = IndirectAbsorber(ChillNum).MinPartLoadRat;
        AbsorberNomCap = IndirectAbsorber(ChillNum).NomCap;
        NomPumpPower = IndirectAbsorber(ChillNum).NomPumpPower;
        TempCondIn = DataLoopNode::Node(IndirectAbsorber(ChillNum).CondInletNodeNum).Temp;
        TempEvapOut = DataLoopNode::Node(IndirectAbsorber(ChillNum).EvapOutletNodeNum).Temp;
        TempLowLimitEout = IndirectAbsorber(ChillNum).TempLowLimitEvapOut;
        LoopNum = IndirectAbsorber(ChillNum).CWLoopNum;
        LoopSideNum = IndirectAbsorber(ChillNum).CWLoopSideNum;

        CpFluid = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(IndirectAbsorber(ChillNum).CWLoopNum).FluidName,
                                        EvapInletTemp,
                                        DataPlant::PlantLoop(IndirectAbsorber(ChillNum).CWLoopNum).FluidIndex,
                                        RoutineName);

        // If there is a fault of Chiller SWT Sensor (zrp_Jun2016)
        if (IndirectAbsorber(ChillNum).FaultyChillerSWTFlag && (!DataGlobals::WarmupFlag) && (!DataGlobals::DoingSizing) && (!DataGlobals::KickOffSimulation)) {
            int FaultIndex = IndirectAbsorber(ChillNum).FaultyChillerSWTIndex;
            Real64 EvapOutletTemp_ff = TempEvapOut;

            // calculate the sensor offset using fault information
            IndirectAbsorber(ChillNum).FaultyChillerSWTOffset = FaultsManager::FaultsChillerSWTSensor(FaultIndex).CalFaultOffsetAct();
            // update the TempEvapOut
            TempEvapOut = max(IndirectAbsorber(ChillNum).TempLowLimitEvapOut,
                              min(DataLoopNode::Node(EvapInletNode).Temp, EvapOutletTemp_ff - IndirectAbsorber(ChillNum).FaultyChillerSWTOffset));
            IndirectAbsorber(ChillNum).FaultyChillerSWTOffset = EvapOutletTemp_ff - TempEvapOut;
        }

        if (IndirectAbsorber(ChillNum).CapFCondenserTempPtr > 0) {
            CapacityfAbsorberTemp = CurveManager::CurveValue(IndirectAbsorber(ChillNum).CapFCondenserTempPtr, TempCondIn);
        } else {
            CapacityfAbsorberTemp = 1.0;
        }
        if (IndirectAbsorber(ChillNum).CapFEvaporatorTempPtr > 0) {
            CapacityfEvaporatorTemp = CurveManager::CurveValue(IndirectAbsorber(ChillNum).CapFEvaporatorTempPtr, TempEvapOut);
        } else {
            CapacityfEvaporatorTemp = 1.0;
        }
        if (IndirectAbsorber(ChillNum).CapFGeneratorTempPtr > 0) {
            if (GeneratorInletNode > 0) {
                if (IndirectAbsorber(ChillNum).GenHeatSourceType == DataLoopNode::NodeType_Water) {
                    CapacityfGeneratorTemp = CurveManager::CurveValue(IndirectAbsorber(ChillNum).CapFGeneratorTempPtr, DataLoopNode::Node(GeneratorInletNode).Temp);
                } else {
                    CapacityfGeneratorTemp = 1.0;
                }
            } else {
                CapacityfGeneratorTemp = 1.0;
            }
        } else {
            CapacityfGeneratorTemp = 1.0;
        }

        AbsorberNomCap *= CapacityfAbsorberTemp * CapacityfEvaporatorTemp * CapacityfGeneratorTemp;

        // If FlowLock is True, the new resolved mdot is used to update Power, QEvap, Qcond, and
        // condenser side outlet temperature.
        if (DataPlant::PlantLoop(LoopNum).LoopSide(LoopSideNum).FlowLock == 0) {
            IndirectAbsorber(ChillNum).PossibleSubcooling = false;
            IndirectAbsorber(ChillNum).QEvaporator = std::abs(MyLoad);

            // Either set the flow to the Constant value or caluclate the flow for the variable volume
            if ((IndirectAbsorber(ChillNum).FlowMode == ConstantFlow) || (IndirectAbsorber(ChillNum).FlowMode == NotModulated)) {
                IndirectAbsorber(ChillNum).EvapMassFlowRate = DataLoopNode::Node(EvapInletNode).MassFlowRate;

                if (IndirectAbsorber(ChillNum).EvapMassFlowRate != 0.0) {
                    EvapDeltaTemp = IndirectAbsorber(ChillNum).QEvaporator / IndirectAbsorber(ChillNum).EvapMassFlowRate / CpFluid;
                } else {
                    EvapDeltaTemp = 0.0;
                }
                IndirectAbsorber(ChillNum).EvapOutletTemp = DataLoopNode::Node(EvapInletNode).Temp - EvapDeltaTemp;

            } else if (IndirectAbsorber(ChillNum).FlowMode == LeavingSetPointModulated) {
                // Calculate the Delta Temp from the inlet temp to the chiller outlet setpoint
                {
                    auto const SELECT_CASE_var(DataPlant::PlantLoop(IndirectAbsorber(ChillNum).CWLoopNum).LoopDemandCalcScheme);
                    if (SELECT_CASE_var == DataPlant::SingleSetPoint) {
                        EvapDeltaTemp = DataLoopNode::Node(EvapInletNode).Temp - DataLoopNode::Node(EvapOutletNode).TempSetPoint;
                    } else if (SELECT_CASE_var == DataPlant::DualSetPointDeadBand) {
                        EvapDeltaTemp = DataLoopNode::Node(EvapInletNode).Temp - DataLoopNode::Node(EvapOutletNode).TempSetPointHi;
                    } else {
                        assert(false);
                    }
                }

                if (EvapDeltaTemp != 0) {
                    IndirectAbsorber(ChillNum).EvapMassFlowRate = std::abs(IndirectAbsorber(ChillNum).QEvaporator / CpFluid / EvapDeltaTemp);
                    if ((IndirectAbsorber(ChillNum).EvapMassFlowRate - IndirectAbsorber(ChillNum).EvapMassFlowRateMax) > DataBranchAirLoopPlant::MassFlowTolerance) PossibleSubcooling = true;
                    // Check to see if the Maximum is exceeded, if so set to maximum
                    IndirectAbsorber(ChillNum).EvapMassFlowRate = min(IndirectAbsorber(ChillNum).EvapMassFlowRateMax, IndirectAbsorber(ChillNum).EvapMassFlowRate);
                    PlantUtilities::SetComponentFlowRate(IndirectAbsorber(ChillNum).EvapMassFlowRate,
                                         IndirectAbsorber(ChillNum).EvapInletNodeNum,
                                         IndirectAbsorber(ChillNum).EvapOutletNodeNum,
                                         IndirectAbsorber(ChillNum).CWLoopNum,
                                         IndirectAbsorber(ChillNum).CWLoopSideNum,
                                         IndirectAbsorber(ChillNum).CWBranchNum,
                                         IndirectAbsorber(ChillNum).CWCompNum);
                    {
                        auto const SELECT_CASE_var(DataPlant::PlantLoop(IndirectAbsorber(ChillNum).CWLoopNum).LoopDemandCalcScheme);
                        if (SELECT_CASE_var == DataPlant::SingleSetPoint) {
                            IndirectAbsorber(ChillNum).EvapOutletTemp = DataLoopNode::Node(EvapOutletNode).TempSetPoint;
                        } else if (SELECT_CASE_var == DataPlant::DualSetPointDeadBand) {
                            IndirectAbsorber(ChillNum).EvapOutletTemp = DataLoopNode::Node(EvapOutletNode).TempSetPointHi;
                        }
                    }
                } else {
                    IndirectAbsorber(ChillNum).EvapMassFlowRate = 0.0;
                    IndirectAbsorber(ChillNum).EvapOutletTemp = DataLoopNode::Node(EvapInletNode).Temp;

                    ShowRecurringWarningErrorAtEnd("CalcIndirectAbsorberModel: Name=\"" + IndirectAbsorber(ChillNum).Name +
                                                       "\" Evaporative Condenser Delta Temperature = 0 in mass flow calculation.",
                                                   IndirectAbsorber(ChillNum).ErrCount2);
                }
            } // End of Constant Variable Flow If Block

            // If there is a fault of Chiller SWT Sensor (zrp_Jun2016)
            if (IndirectAbsorber(ChillNum).FaultyChillerSWTFlag && (!DataGlobals::WarmupFlag) && (!DataGlobals::DoingSizing) && (!DataGlobals::KickOffSimulation) &&
                (IndirectAbsorber(ChillNum).EvapMassFlowRate > 0)) {
                // calculate directly affected variables at faulty case: EvapOutletTemp, EvapMassFlowRate, QEvaporator
                int FaultIndex = IndirectAbsorber(ChillNum).FaultyChillerSWTIndex;
                bool VarFlowFlag = (IndirectAbsorber(ChillNum).FlowMode == LeavingSetPointModulated);
                FaultsManager::FaultsChillerSWTSensor(FaultIndex)
                    .CalFaultChillerSWT(VarFlowFlag,
                                        IndirectAbsorber(ChillNum).FaultyChillerSWTOffset,
                                        CpFluid,
                                        DataLoopNode::Node(EvapInletNode).Temp,
                                        IndirectAbsorber(ChillNum).EvapOutletTemp,
                                        IndirectAbsorber(ChillNum).EvapMassFlowRate,
                                        IndirectAbsorber(ChillNum).QEvaporator);
                // update corresponding variables at faulty case
                // PartLoadRat = ( AvailChillerCap > 0.0 ) ? ( QEvaporator / AvailChillerCap ) : 0.0;
                // PartLoadRat = max( 0.0, min( PartLoadRat, MaxPartLoadRat ));
                // ChillerPartLoadRatio = PartLoadRat;
            }

        } else { // If FlowLock is True

            IndirectAbsorber(ChillNum).EvapMassFlowRate = DataLoopNode::Node(EvapInletNode).MassFlowRate;
            if (PossibleSubcooling) {
                IndirectAbsorber(ChillNum).QEvaporator = std::abs(MyLoad);
                EvapDeltaTemp = IndirectAbsorber(ChillNum).QEvaporator / IndirectAbsorber(ChillNum).EvapMassFlowRate / CpFluid;
                IndirectAbsorber(ChillNum).EvapOutletTemp = DataLoopNode::Node(EvapInletNode).Temp - EvapDeltaTemp;
            } else {
                {
                    auto const SELECT_CASE_var(DataPlant::PlantLoop(LoopNum).LoopDemandCalcScheme);
                    if (SELECT_CASE_var == DataPlant::SingleSetPoint) {
                        if ((IndirectAbsorber(ChillNum).FlowMode == LeavingSetPointModulated) ||
                            (DataPlant::PlantLoop(LoopNum)
                                 .LoopSide(LoopSideNum)
                                 .Branch(IndirectAbsorber(ChillNum).CWBranchNum)
                                 .Comp(IndirectAbsorber(ChillNum).CWCompNum)
                                 .CurOpSchemeType == DataPlant::CompSetPtBasedSchemeType) ||
                            (DataLoopNode::Node(EvapOutletNode).TempSetPoint != DataLoopNode::SensedNodeFlagValue)) {
                            TempEvapOutSetPoint = DataLoopNode::Node(EvapOutletNode).TempSetPoint;
                        } else {
                            TempEvapOutSetPoint = DataLoopNode::Node(DataPlant::PlantLoop(LoopNum).TempSetPointNodeNum).TempSetPoint;
                        }
                    } else if (SELECT_CASE_var == DataPlant::DualSetPointDeadBand) {
                        if ((IndirectAbsorber(ChillNum).FlowMode == LeavingSetPointModulated) ||
                            (DataPlant::PlantLoop(LoopNum)
                                 .LoopSide(LoopSideNum)
                                 .Branch(IndirectAbsorber(ChillNum).CWBranchNum)
                                 .Comp(IndirectAbsorber(ChillNum).CWCompNum)
                                 .CurOpSchemeType == DataPlant::CompSetPtBasedSchemeType) ||
                            (DataLoopNode::Node(EvapOutletNode).TempSetPointHi != DataLoopNode::SensedNodeFlagValue)) {
                            TempEvapOutSetPoint = DataLoopNode::Node(EvapOutletNode).TempSetPointHi;
                        } else {
                            TempEvapOutSetPoint = DataLoopNode::Node(DataPlant::PlantLoop(LoopNum).TempSetPointNodeNum).TempSetPointHi;
                        }
                    } else {
                        assert(false);
                    }
                }
                EvapDeltaTemp = DataLoopNode::Node(EvapInletNode).Temp - TempEvapOutSetPoint;
                IndirectAbsorber(ChillNum).QEvaporator = std::abs(IndirectAbsorber(ChillNum).EvapMassFlowRate * CpFluid * EvapDeltaTemp);
                IndirectAbsorber(ChillNum).EvapOutletTemp = TempEvapOutSetPoint;
            }
            // Check that the Evap outlet temp honors both plant loop temp low limit and also the chiller low limit
            if (IndirectAbsorber(ChillNum).EvapOutletTemp < TempLowLimitEout) {
                if ((DataLoopNode::Node(EvapInletNode).Temp - TempLowLimitEout) > DataPlant::DeltaTempTol) {
                    IndirectAbsorber(ChillNum).EvapOutletTemp = TempLowLimitEout;
                    EvapDeltaTemp = DataLoopNode::Node(EvapInletNode).Temp - IndirectAbsorber(ChillNum).EvapOutletTemp;
                    IndirectAbsorber(ChillNum).QEvaporator = IndirectAbsorber(ChillNum).EvapMassFlowRate * CpFluid * EvapDeltaTemp;
                } else {
                    IndirectAbsorber(ChillNum).EvapOutletTemp = DataLoopNode::Node(EvapInletNode).Temp;
                    EvapDeltaTemp = DataLoopNode::Node(EvapInletNode).Temp - IndirectAbsorber(ChillNum).EvapOutletTemp;
                    IndirectAbsorber(ChillNum).QEvaporator = IndirectAbsorber(ChillNum).EvapMassFlowRate * CpFluid * EvapDeltaTemp;
                }
            }
            if (IndirectAbsorber(ChillNum).EvapOutletTemp < DataLoopNode::Node(EvapOutletNode).TempMin) {
                if ((DataLoopNode::Node(EvapInletNode).Temp - DataLoopNode::Node(EvapOutletNode).TempMin) > DataPlant::DeltaTempTol) {
                    IndirectAbsorber(ChillNum).EvapOutletTemp = DataLoopNode::Node(EvapOutletNode).TempMin;
                    EvapDeltaTemp = DataLoopNode::Node(EvapInletNode).Temp - IndirectAbsorber(ChillNum).EvapOutletTemp;
                    IndirectAbsorber(ChillNum).QEvaporator = IndirectAbsorber(ChillNum).EvapMassFlowRate * CpFluid * EvapDeltaTemp;
                } else {
                    IndirectAbsorber(ChillNum).EvapOutletTemp = DataLoopNode::Node(EvapInletNode).Temp;
                    EvapDeltaTemp = DataLoopNode::Node(EvapInletNode).Temp - IndirectAbsorber(ChillNum).EvapOutletTemp;
                    IndirectAbsorber(ChillNum).QEvaporator = IndirectAbsorber(ChillNum).EvapMassFlowRate * CpFluid * EvapDeltaTemp;
                }
            }

            // Checks QEvaporator on the basis of the machine limits.
            if (IndirectAbsorber(ChillNum).QEvaporator > std::abs(MyLoad)) {
                if (IndirectAbsorber(ChillNum).EvapMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance) {
                    IndirectAbsorber(ChillNum).QEvaporator = std::abs(MyLoad);
                    EvapDeltaTemp = IndirectAbsorber(ChillNum).QEvaporator / IndirectAbsorber(ChillNum).EvapMassFlowRate / CpFluid;
                    IndirectAbsorber(ChillNum).EvapOutletTemp = DataLoopNode::Node(EvapInletNode).Temp - EvapDeltaTemp;
                } else {
                    IndirectAbsorber(ChillNum).QEvaporator = 0.0;
                    IndirectAbsorber(ChillNum).EvapOutletTemp = DataLoopNode::Node(EvapInletNode).Temp;
                }
            }

            // If there is a fault of Chiller SWT Sensor
            if (IndirectAbsorber(ChillNum).FaultyChillerSWTFlag && (!DataGlobals::WarmupFlag) && (!DataGlobals::DoingSizing) && (!DataGlobals::KickOffSimulation) &&
                (IndirectAbsorber(ChillNum).EvapMassFlowRate > 0)) {
                // calculate directly affected variables at faulty case: EvapOutletTemp, EvapMassFlowRate, QEvaporator
                int FaultIndex = IndirectAbsorber(ChillNum).FaultyChillerSWTIndex;
                bool VarFlowFlag = false;
                FaultsManager::FaultsChillerSWTSensor(FaultIndex)
                    .CalFaultChillerSWT(VarFlowFlag,
                                        IndirectAbsorber(ChillNum).FaultyChillerSWTOffset,
                                        CpFluid,
                                        DataLoopNode::Node(EvapInletNode).Temp,
                                        IndirectAbsorber(ChillNum).EvapOutletTemp,
                                        IndirectAbsorber(ChillNum).EvapMassFlowRate,
                                        IndirectAbsorber(ChillNum).QEvaporator);
                // update corresponding variables at faulty case
            }

        } // This is the end of the FlowLock Block

        OperPartLoadRat = IndirectAbsorber(ChillNum).QEvaporator / AbsorberNomCap;
        PartLoadRat = max(MinPartLoadRat, OperPartLoadRat);
        IndirectAbsorberReport(ChillNum).ChillerPartLoadRatio = OperPartLoadRat;

        if (OperPartLoadRat < PartLoadRat) {
            FRAC = min(1.0, OperPartLoadRat / MinPartLoadRat);
        } else {
            FRAC = 1.0;
        }

        IndirectAbsorber(ChillNum).ChillerONOFFCyclingFrac = FRAC;

        if (GeneratorInletNode > 0) {
            if (IndirectAbsorber(ChillNum).HeatInputFCondTempPtr > 0) {
                HeatInputfCondTemp = CurveManager::CurveValue(IndirectAbsorber(ChillNum).HeatInputFCondTempPtr, DataLoopNode::Node(GeneratorInletNode).Temp);
            } else {
                HeatInputfCondTemp = 1.0;
            }
        } else {
            HeatInputfCondTemp = 1.0;
        }
        if (IndirectAbsorber(ChillNum).HeatInputFEvapTempPtr > 0) {
            HeatInputfEvapTemp = CurveManager::CurveValue(IndirectAbsorber(ChillNum).HeatInputFEvapTempPtr, DataLoopNode::Node(EvapOutletNode).Temp);
        } else {
            HeatInputfEvapTemp = 1.0;
        }

        // Calculate steam input ratio. Include impact of generator and evaporator temperatures
        if (IndirectAbsorber(ChillNum).GeneratorInputCurvePtr > 0) {
            HeatInputRat = CurveManager::CurveValue(IndirectAbsorber(ChillNum).GeneratorInputCurvePtr, PartLoadRat) * HeatInputfCondTemp * HeatInputfEvapTemp;
        } else {
            HeatInputRat = HeatInputfCondTemp * HeatInputfEvapTemp;
        }

        // Calculate electric input ratio
        if (IndirectAbsorber(ChillNum).PumpPowerCurvePtr > 0) {
            ElectricInputRat = CurveManager::CurveValue(IndirectAbsorber(ChillNum).PumpPowerCurvePtr, PartLoadRat);
        } else {
            ElectricInputRat = 1.0;
        }

        IndirectAbsorber(ChillNum).QGenerator = HeatInputRat * AbsorberNomCap * FRAC;
        IndirectAbsorber(ChillNum).PumpingPower = ElectricInputRat * NomPumpPower * FRAC;

        if (IndirectAbsorber(ChillNum).EvapMassFlowRate == 0.0) {
            IndirectAbsorber(ChillNum).QGenerator = 0.0;
            IndirectAbsorber(ChillNum).EvapOutletTemp = DataLoopNode::Node(EvapInletNode).Temp;
            IndirectAbsorber(ChillNum).PumpingPower = 0.0;
        }

        IndirectAbsorber(ChillNum).QCondenser = IndirectAbsorber(ChillNum).QEvaporator + IndirectAbsorber(ChillNum).QGenerator + IndirectAbsorber(ChillNum).PumpingPower;

        CpFluid = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(IndirectAbsorber(ChillNum).CDLoopNum).FluidName,
                                        CondInletTemp,
                                        DataPlant::PlantLoop(IndirectAbsorber(ChillNum).CDLoopNum).FluidIndex,
                                        RoutineName);

        if (IndirectAbsorber(ChillNum).CondMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance) {
            IndirectAbsorber(ChillNum).CondOutletTemp = IndirectAbsorber(ChillNum).QCondenser / IndirectAbsorber(ChillNum).CondMassFlowRate / CpFluid + CondInletTemp;
        } else {
            IndirectAbsorber(ChillNum).CondOutletTemp = CondInletTemp;
            IndirectAbsorber(ChillNum).CondMassFlowRate = 0.0;
            IndirectAbsorber(ChillNum).QCondenser = 0.0;
            return;
            // V7 plant upgrade, no longer fatal here anymore... set some things and return
        }

        if (GeneratorInletNode > 0) {
            //   Hot water plant is used for the generator
            if (IndirectAbsorber(ChillNum).GenHeatSourceType == DataLoopNode::NodeType_Water) {

                CpFluid = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(IndirectAbsorber(ChillNum).GenLoopNum).FluidName,
                                                DataLoopNode::Node(GeneratorInletNode).Temp,
                                                DataPlant::PlantLoop(IndirectAbsorber(ChillNum).GenLoopNum).FluidIndex,
                                                RoutineName);
                if ((IndirectAbsorber(ChillNum).FlowMode == ConstantFlow) || (IndirectAbsorber(ChillNum).FlowMode == NotModulated)) {
                    IndirectAbsorber(ChillNum).GenMassFlowRate = IndirectAbsorber(ChillNum).GenMassFlowRateMax;
                } else {
                    IndirectAbsorber(ChillNum).GenMassFlowRate = IndirectAbsorber(ChillNum).QGenerator / CpFluid / IndirectAbsorber(ChillNum).GeneratorDeltaTemp;
                }

                PlantUtilities::SetComponentFlowRate(IndirectAbsorber(ChillNum).GenMassFlowRate,
                                     GeneratorInletNode,
                                     GeneratorOutletNode,
                                     IndirectAbsorber(ChillNum).GenLoopNum,
                                     IndirectAbsorber(ChillNum).GenLoopSideNum,
                                     IndirectAbsorber(ChillNum).GenBranchNum,
                                     IndirectAbsorber(ChillNum).GenCompNum);

                if (IndirectAbsorber(ChillNum).GenMassFlowRate <= 0.0) {
                    IndirectAbsorber(ChillNum).GenOutletTemp = DataLoopNode::Node(GeneratorInletNode).Temp;
                    IndirectAbsorber(ChillNum).SteamOutletEnthalpy = DataLoopNode::Node(GeneratorInletNode).Enthalpy;
                } else {
                    IndirectAbsorber(ChillNum).GenOutletTemp = DataLoopNode::Node(GeneratorInletNode).Temp - IndirectAbsorber(ChillNum).QGenerator / (CpFluid * IndirectAbsorber(ChillNum).GenMassFlowRate);
                    IndirectAbsorber(ChillNum).SteamOutletEnthalpy = DataLoopNode::Node(GeneratorInletNode).Enthalpy - IndirectAbsorber(ChillNum).QGenerator / IndirectAbsorber(ChillNum).GenMassFlowRate;
                }

            } else { // using a steam plant for the generator

                EnthSteamOutDry = FluidProperties::GetSatEnthalpyRefrig(fluidNameSteam,
                                                       DataLoopNode::Node(GeneratorInletNode).Temp,
                                                       1.0,
                                                       IndirectAbsorber(ChillNum).SteamFluidIndex,
                                                       calcChillerAbsorptionIndirect + IndirectAbsorber(ChillNum).Name);
                EnthSteamOutWet = FluidProperties::GetSatEnthalpyRefrig(fluidNameSteam,
                                                       DataLoopNode::Node(GeneratorInletNode).Temp,
                                                       0.0,
                                                       IndirectAbsorber(ChillNum).SteamFluidIndex,
                                                       calcChillerAbsorptionIndirect + IndirectAbsorber(ChillNum).Name);
                SteamDeltaT = IndirectAbsorber(ChillNum).GeneratorSubcool;
                SteamOutletTemp = DataLoopNode::Node(GeneratorInletNode).Temp - SteamDeltaT;
                HfgSteam = EnthSteamOutDry - EnthSteamOutWet;
                CpFluid = FluidProperties::GetSpecificHeatGlycol(
                    fluidNameWater, SteamOutletTemp, DummyWaterIndex, calcChillerAbsorptionIndirect + IndirectAbsorber(ChillNum).Name);
                IndirectAbsorber(ChillNum).GenMassFlowRate = IndirectAbsorber(ChillNum).QGenerator / (HfgSteam + CpFluid * SteamDeltaT);
                PlantUtilities::SetComponentFlowRate(IndirectAbsorber(ChillNum).GenMassFlowRate,
                                     GeneratorInletNode,
                                     GeneratorOutletNode,
                                     IndirectAbsorber(ChillNum).GenLoopNum,
                                     IndirectAbsorber(ChillNum).GenLoopSideNum,
                                     IndirectAbsorber(ChillNum).GenBranchNum,
                                     IndirectAbsorber(ChillNum).GenCompNum);

                if (IndirectAbsorber(ChillNum).GenMassFlowRate <= 0.0) {
                    IndirectAbsorber(ChillNum).GenOutletTemp = DataLoopNode::Node(GeneratorInletNode).Temp;
                    IndirectAbsorber(ChillNum).SteamOutletEnthalpy = DataLoopNode::Node(GeneratorInletNode).Enthalpy;
                } else {
                    IndirectAbsorber(ChillNum).GenOutletTemp = DataLoopNode::Node(GeneratorInletNode).Temp - SteamDeltaT;
                    IndirectAbsorber(ChillNum).SteamOutletEnthalpy = FluidProperties::GetSatEnthalpyRefrig(fluidNameSteam,
                                                               DataLoopNode::Node(GeneratorInletNode).Temp,
                                                               0.0,
                                                               IndirectAbsorber(ChillNum).SteamFluidIndex,
                                                               LoopLossesChillerAbsorptionIndirect + IndirectAbsorber(ChillNum).Name);
                    CpFluid = FluidProperties::GetSpecificHeatGlycol(fluidNameWater,
                                                    DataLoopNode::Node(GeneratorInletNode).Temp,
                                                    DummyWaterIndex,
                                                    calcChillerAbsorptionIndirect + IndirectAbsorber(ChillNum).Name);

                    IndirectAbsorber(ChillNum).SteamOutletEnthalpy -= CpFluid * SteamDeltaT;

                    //************************* Loop Losses *****************************
                    TempWaterAtmPress = FluidProperties::GetSatTemperatureRefrig(fluidNameSteam,
                                                                DataEnvironment::OutBaroPress,
                                                                IndirectAbsorber(ChillNum).SteamFluidIndex,
                                                                LoopLossesChillerAbsorptionIndirect + IndirectAbsorber(ChillNum).Name);

                    EnthAtAtmPress = FluidProperties::GetSatEnthalpyRefrig(fluidNameSteam,
                                                          TempWaterAtmPress,
                                                          0.0,
                                                          IndirectAbsorber(ChillNum).SteamFluidIndex,
                                                          LoopLossesChillerAbsorptionIndirectSpace + IndirectAbsorber(ChillNum).Name);

                    // Point 4 at atm - loop delta subcool during return journery back to pump
                    TempLoopOutToPump = TempWaterAtmPress - IndirectAbsorber(ChillNum).LoopSubcool;

                    // Reported value of coil outlet enthalpy at the node to match the node outlet temperature
                    EnthPumpInlet = EnthAtAtmPress - CpFluid * IndirectAbsorber(ChillNum).LoopSubcool;

                    // Point 3-Point 5,
                    EnergyLossToEnvironment = IndirectAbsorber(ChillNum).GenMassFlowRate * (IndirectAbsorber(ChillNum).SteamOutletEnthalpy - EnthPumpInlet);

                    //************************* Loop Losses *****************************

                    IndirectAbsorber(ChillNum).GenOutletTemp = TempLoopOutToPump;
                    IndirectAbsorber(ChillNum).SteamOutletEnthalpy = EnthPumpInlet;

                } // IF(GenMassFlowRate .LE. 0.0d0)THEN

            } // IF(IndirectAbsorber(ChillNum)%GenHeatSourceType == NodeType_Water)THEN

        } // IF(GeneratorInletNode .GT. 0)THEN

        // convert power to energy
        IndirectAbsorber(ChillNum).GeneratorEnergy = IndirectAbsorber(ChillNum).QGenerator * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
        IndirectAbsorber(ChillNum).EvaporatorEnergy = IndirectAbsorber(ChillNum).QEvaporator * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
        IndirectAbsorber(ChillNum).CondenserEnergy = IndirectAbsorber(ChillNum).QCondenser * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
        IndirectAbsorber(ChillNum).PumpingEnergy = IndirectAbsorber(ChillNum).PumpingPower * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

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

    void UpdateIndirectAbsorberRecords(Real64 const MyLoad, // current load
                                       bool const RunFlag,  // TRUE if Absorber operating
                                       int const ChillNum        // Absorber number
    )
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR:          R. Raustad (FSEC)
        //       DATE WRITTEN:    May 2008

        // PURPOSE OF THIS SUBROUTINE:
        // reporting

        int EvapInletNode;       // evaporator inlet node number, water side
        int EvapOutletNode;      // evaporator outlet node number, water side
        int CondInletNode;       // condenser inlet node number, water side
        int CondOutletNode;      // condenser outlet node number, water side
        int GeneratorInletNode;  // generator inlet node number, steam/water side
        int GeneratorOutletNode; // generator outlet node number, steam/water side

        EvapInletNode = IndirectAbsorber(ChillNum).EvapInletNodeNum;
        EvapOutletNode = IndirectAbsorber(ChillNum).EvapOutletNodeNum;
        CondInletNode = IndirectAbsorber(ChillNum).CondInletNodeNum;
        CondOutletNode = IndirectAbsorber(ChillNum).CondOutletNodeNum;
        GeneratorInletNode = IndirectAbsorber(ChillNum).GeneratorInletNodeNum;
        GeneratorOutletNode = IndirectAbsorber(ChillNum).GeneratorOutletNodeNum;

        if (MyLoad >= 0 || !RunFlag) {
            // set node temperature
            PlantUtilities::SafeCopyPlantNode(EvapInletNode, EvapOutletNode);
            PlantUtilities::SafeCopyPlantNode(CondInletNode, CondOutletNode);

            IndirectAbsorberReport(ChillNum).PumpingPower = 0.0;
            IndirectAbsorberReport(ChillNum).QEvap = 0.0;
            IndirectAbsorberReport(ChillNum).QCond = 0.0;
            IndirectAbsorberReport(ChillNum).QGenerator = 0.0;
            IndirectAbsorberReport(ChillNum).PumpingEnergy = 0.0;
            IndirectAbsorberReport(ChillNum).EvapEnergy = 0.0;
            IndirectAbsorberReport(ChillNum).CondEnergy = 0.0;
            IndirectAbsorberReport(ChillNum).GeneratorEnergy = 0.0;
            IndirectAbsorberReport(ChillNum).EvapInletTemp = DataLoopNode::Node(EvapInletNode).Temp;
            IndirectAbsorberReport(ChillNum).CondInletTemp = DataLoopNode::Node(CondInletNode).Temp;
            IndirectAbsorberReport(ChillNum).CondOutletTemp = DataLoopNode::Node(CondOutletNode).Temp;
            IndirectAbsorberReport(ChillNum).EvapOutletTemp = DataLoopNode::Node(EvapOutletNode).Temp;
            IndirectAbsorberReport(ChillNum).Evapmdot = 0.0;
            IndirectAbsorberReport(ChillNum).Condmdot = 0.0;
            IndirectAbsorberReport(ChillNum).Genmdot = 0.0;
            IndirectAbsorberReport(ChillNum).ActualCOP = 0.0;
            IndirectAbsorberReport(ChillNum).ChillerPartLoadRatio = 0.0;
            IndirectAbsorberReport(ChillNum).LoopLoss = 0.0;
            IndirectAbsorberReport(ChillNum).ChillerCyclingFrac = 0.0;

            if (GeneratorInletNode > 0) {
                PlantUtilities::SafeCopyPlantNode(GeneratorInletNode, GeneratorOutletNode);
            }

        } else {
            // set node temperatures
            PlantUtilities::SafeCopyPlantNode(EvapInletNode, EvapOutletNode);
            PlantUtilities::SafeCopyPlantNode(CondInletNode, CondOutletNode);
            DataLoopNode::Node(EvapOutletNode).Temp = IndirectAbsorber(ChillNum).EvapOutletTemp;
            DataLoopNode::Node(CondOutletNode).Temp = IndirectAbsorber(ChillNum).CondOutletTemp;

            IndirectAbsorberReport(ChillNum).PumpingPower = IndirectAbsorber(ChillNum).PumpingPower;
            IndirectAbsorberReport(ChillNum).QEvap = IndirectAbsorber(ChillNum).QEvaporator;
            IndirectAbsorberReport(ChillNum).QCond = IndirectAbsorber(ChillNum).QCondenser;
            IndirectAbsorberReport(ChillNum).QGenerator = IndirectAbsorber(ChillNum).QGenerator;
            IndirectAbsorberReport(ChillNum).PumpingEnergy = IndirectAbsorber(ChillNum).PumpingEnergy;
            IndirectAbsorberReport(ChillNum).EvapEnergy = IndirectAbsorber(ChillNum).EvaporatorEnergy;
            IndirectAbsorberReport(ChillNum).CondEnergy = IndirectAbsorber(ChillNum).CondenserEnergy;
            IndirectAbsorberReport(ChillNum).GeneratorEnergy = IndirectAbsorber(ChillNum).GeneratorEnergy;
            IndirectAbsorberReport(ChillNum).EvapInletTemp = DataLoopNode::Node(EvapInletNode).Temp;
            IndirectAbsorberReport(ChillNum).CondInletTemp = DataLoopNode::Node(CondInletNode).Temp;
            IndirectAbsorberReport(ChillNum).CondOutletTemp = DataLoopNode::Node(CondOutletNode).Temp;
            IndirectAbsorberReport(ChillNum).EvapOutletTemp = DataLoopNode::Node(EvapOutletNode).Temp;
            IndirectAbsorberReport(ChillNum).Evapmdot = IndirectAbsorber(ChillNum).EvapMassFlowRate;
            IndirectAbsorberReport(ChillNum).Condmdot = IndirectAbsorber(ChillNum).CondMassFlowRate;
            IndirectAbsorberReport(ChillNum).Genmdot = IndirectAbsorber(ChillNum).GenMassFlowRate;
            IndirectAbsorberReport(ChillNum).LoopLoss = EnergyLossToEnvironment;
            IndirectAbsorberReport(ChillNum).ChillerCyclingFrac = IndirectAbsorber(ChillNum).ChillerONOFFCyclingFrac;

            if (IndirectAbsorber(ChillNum).QGenerator != 0.0) {
                IndirectAbsorberReport(ChillNum).ActualCOP = IndirectAbsorber(ChillNum).QEvaporator / IndirectAbsorber(ChillNum).QGenerator;
            } else {
                IndirectAbsorberReport(ChillNum).ActualCOP = 0.0;
            }

            if (GeneratorInletNode > 0) {
                PlantUtilities::SafeCopyPlantNode(GeneratorInletNode, GeneratorOutletNode);
                DataLoopNode::Node(GeneratorOutletNode).Temp = IndirectAbsorber(ChillNum).GenOutletTemp;
            }
        }
    }

} // namespace ChillerIndirectAbsorption

} // namespace EnergyPlus
