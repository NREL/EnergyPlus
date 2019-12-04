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
#include <EnergyPlus/ChillerAbsorption.hh>
#include <EnergyPlus/DataBranchAirLoopPlant.hh>
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

    int const FlowModeNotSet(200);
    int const ConstantFlow(201);
    int const NotModulated(202);
    int const LeavingSetPointModulated(203);

    int NumBLASTAbsorbers(0); // number of Absorption Chillers specified in input

    Real64 CondMassFlowRate(0.0);    // Kg/s - condenser mass flow rate, water side
    Real64 EvapMassFlowRate(0.0);    // Kg/s - evaporator mass flow rate, water side
    Real64 SteamMassFlowRate(0.0);   // Kg/s - steam mass flow rate, water side
    Real64 CondOutletTemp(0.0);      // C - condenser outlet temperature, water side
    Real64 EvapOutletTemp(0.0);      // C - evaporator outlet temperature, water side
    Real64 GenOutletTemp(0.0);       // C - generator fluid outlet temperature
    Real64 SteamOutletEnthalpy(0.0); // J/kg - generator fluid outlet enthalpy
    Real64 PumpingPower(0.0);        // W - rate of Absorber energy use
    Real64 PumpingEnergy(0.0);       // J - Absorber energy use
    Real64 QGenerator(0.0);          // W - rate of Absorber steam use
    Real64 GeneratorEnergy(0.0);     // J - Absorber steam use
    Real64 QEvaporator(0.0);         // W - rate of heat transfer to the evaporator coil
    Real64 EvaporatorEnergy(0.0);    // J - heat transfer to the evaporator coil
    Real64 QCondenser(0.0);          // W - rate of heat transfer to the condenser coil
    Real64 CondenserEnergy(0.0);     // J - heat transfer to the condenser coil

    bool GetInput(true); // when TRUE, calls subroutine to read input file.

    static std::string const BlankString;
    static std::string const fluidNameSteam("STEAM");
    static std::string const fluidNameWater("WATER");
    static std::string const moduleObjectType("Chiller:Absorption");
    static std::string const calcChillerAbsorption("CALC Chiller:Absorption ");

    Array1D_bool CheckEquipName;

    Array1D<BLASTAbsorberSpecs> BLASTAbsorber; // dimension to number of machines
    Array1D<ReportVars> BLASTAbsorberReport;

    void SimBLASTAbsorber(std::string const &EP_UNUSED(AbsorberType), // type of Absorber
                          std::string const &AbsorberName,            // user specified name of Absorber
                          int const EquipFlowCtrl,                    // Flow control mode for the equipment
                          int const LoopNum,                          // Plant loop index for where called from
                          int const LoopSide,                         // Plant loop side index for where called from
                          int &CompIndex,                             // Chiller number pointer
                          bool const RunFlag,                         // simulate Absorber when TRUE
                          bool const FirstIteration,                  // initialize variables when TRUE
                          bool &InitLoopEquip,                        // If not zero, calculate the max load for operating conditions
                          Real64 &MyLoad,                             // loop demand component will meet
                          Real64 &MaxCap,                             // Maximum operating capacity of chiller [W]
                          Real64 &MinCap,                             // Minimum operating capacity of chiller [W]
                          Real64 &OptCap,                             // Optimal operating capacity of chiller [W]
                          bool const GetSizingFactor,                 // TRUE when just the sizing factor is requested
                          Real64 &SizingFactor,                       // sizing factor
                          Real64 &TempCondInDesign)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Dan Fisher
        //       DATE WRITTEN   Nov. 2000
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE: This is the Absorption Chiller model driver.  It
        // gets the input for the models, initializes simulation variables, call
        // the appropriate model and sets up reporting variables.

        int ChillNum; // Chiller number pointer

        // Get Absorber data from input file
        if (GetInput) {
            GetBLASTAbsorberInput();
            GetInput = false;
        }

        // Find the correct Chiller
        if (CompIndex == 0) {
            ChillNum = UtilityRoutines::FindItemInList(AbsorberName, BLASTAbsorber);
            if (ChillNum == 0) {
                ShowFatalError("SimBLASTAbsorber: Specified Absorber not one of Valid Absorption Chillers=" + AbsorberName);
            }
            CompIndex = ChillNum;
        } else {
            ChillNum = CompIndex;
            if (ChillNum > NumBLASTAbsorbers || ChillNum < 1) {
                ShowFatalError("SimBLASTAbsorber:  Invalid CompIndex passed=" + General::TrimSigDigits(ChillNum) +
                               ", Number of Units=" + General::TrimSigDigits(NumBLASTAbsorbers) + ", Entered Unit name=" + AbsorberName);
            }
            if (CheckEquipName(ChillNum)) {
                if (AbsorberName != BLASTAbsorber(ChillNum).Name) {
                    ShowFatalError("SimBLASTAbsorber: Invalid CompIndex passed=" + General::TrimSigDigits(ChillNum) + ", Unit name=" + AbsorberName +
                                   ", stored Unit Name for that index=" + BLASTAbsorber(ChillNum).Name);
                }
                CheckEquipName(ChillNum) = false;
            }
        }

        // Initialize Loop Equipment
        if (InitLoopEquip) {
            TempCondInDesign = BLASTAbsorber(ChillNum).TempDesCondIn;
            InitBLASTAbsorberModel(ChillNum, RunFlag, MyLoad);

            if (LoopNum == BLASTAbsorber(ChillNum).CWLoopNum) {
                SizeAbsorpChiller(ChillNum);
                MinCap = BLASTAbsorber(ChillNum).NomCap * BLASTAbsorber(ChillNum).MinPartLoadRat;
                MaxCap = BLASTAbsorber(ChillNum).NomCap * BLASTAbsorber(ChillNum).MaxPartLoadRat;
                OptCap = BLASTAbsorber(ChillNum).NomCap * BLASTAbsorber(ChillNum).OptPartLoadRat;
            } else {
                MinCap = 0.0;
                MaxCap = 0.0;
                OptCap = 0.0;
            }
            if (GetSizingFactor) {
                SizingFactor = BLASTAbsorber(ChillNum).SizFac;
            }
            return;
        }

        // different actions depending on which loop the component was called from

        if (LoopNum == BLASTAbsorber(ChillNum).CWLoopNum) {
            // called from dominant chilled water connection loop side

            // Calculate Load
            InitBLASTAbsorberModel(ChillNum, RunFlag, MyLoad);
            CalcBLASTAbsorberModel(ChillNum, MyLoad, RunFlag, FirstIteration, EquipFlowCtrl);
            UpdateBLASTAbsorberRecords(MyLoad, RunFlag, ChillNum);

        } else if (LoopNum == BLASTAbsorber(ChillNum).CDLoopNum) {
            // Called from non-dominant condenser water connection loop side
            PlantUtilities::UpdateChillerComponentCondenserSide(LoopNum,
                                                LoopSide,
                                                DataPlant::TypeOf_Chiller_Absorption,
                                                BLASTAbsorber(ChillNum).CondInletNodeNum,
                                                BLASTAbsorber(ChillNum).CondOutletNodeNum,
                                                BLASTAbsorberReport(ChillNum).QCond,
                                                BLASTAbsorberReport(ChillNum).CondInletTemp,
                                                BLASTAbsorberReport(ChillNum).CondOutletTemp,
                                                BLASTAbsorberReport(ChillNum).Condmdot,
                                                FirstIteration);

        } else if (LoopNum == BLASTAbsorber(ChillNum).GenLoopNum) {
            // Called from non-dominant generator hot water or steam connection loop side
            PlantUtilities::UpdateAbsorberChillerComponentGeneratorSide(LoopNum,
                                                        LoopSide,
                                                        DataPlant::TypeOf_Chiller_Absorption,
                                                        BLASTAbsorber(ChillNum).GeneratorInletNodeNum,
                                                        BLASTAbsorber(ChillNum).GeneratorOutletNodeNum,
                                                        BLASTAbsorber(ChillNum).GenHeatSourceType,
                                                        BLASTAbsorberReport(ChillNum).QGenerator,
                                                        BLASTAbsorberReport(ChillNum).SteamMdot,
                                                        FirstIteration);

        } else {
            ShowFatalError("SimBLASTAbsorber: Invalid LoopNum passed=" + General::TrimSigDigits(LoopNum) + ", Unit name=" + AbsorberName +
                           ", stored chilled water loop=" + General::TrimSigDigits(BLASTAbsorber(ChillNum).CWLoopNum) +
                           ", stored condenser water loop=" + General::TrimSigDigits(BLASTAbsorber(ChillNum).CDLoopNum) +
                           ", stored generator loop=" + General::TrimSigDigits(BLASTAbsorber(ChillNum).GenLoopNum));
        }
    }

    void GetBLASTAbsorberInput()
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

        // LOCAL VARIABLES
        int AbsorberNum;                      // Absorber counter
        int NumAlphas;                        // Number of elements in the alpha array
        int NumNums;                          // Number of elements in the numeric array
        int IOStat;                           // IO Status when calling get input subroutine
        Array1D_bool GenInputOutputNodesUsed; // Used for SetupOutputVariable
        static bool ErrorsFound(false);

        DataIPShortCuts::cCurrentModuleObject = moduleObjectType;

        NumBLASTAbsorbers = inputProcessor->getNumObjectsFound(DataIPShortCuts::cCurrentModuleObject);

        if (NumBLASTAbsorbers <= 0) {
            ShowSevereError("No " + DataIPShortCuts::cCurrentModuleObject + " equipment specified in input file");
            // See if load distribution manager has already gotten the input
            ErrorsFound = true;
        }

        if (allocated(BLASTAbsorber)) return;
        // ALLOCATE ARRAYS
        BLASTAbsorber.allocate(NumBLASTAbsorbers);
        CheckEquipName.dimension(NumBLASTAbsorbers, true);
        GenInputOutputNodesUsed.dimension(NumBLASTAbsorbers, false);

        BLASTAbsorberReport.allocate(NumBLASTAbsorbers);

        // LOAD ARRAYS WITH BLAST CURVE FIT Absorber DATA
        for (AbsorberNum = 1; AbsorberNum <= NumBLASTAbsorbers; ++AbsorberNum) {
            inputProcessor->getObjectItem(DataIPShortCuts::cCurrentModuleObject,
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
            GlobalNames::VerifyUniqueChillerName(DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), ErrorsFound, DataIPShortCuts::cCurrentModuleObject + " Name");

            BLASTAbsorber(AbsorberNum).Name = DataIPShortCuts::cAlphaArgs(1);
            BLASTAbsorber(AbsorberNum).NomCap = DataIPShortCuts::rNumericArgs(1);
            if (BLASTAbsorber(AbsorberNum).NomCap == DataSizing::AutoSize) {
                BLASTAbsorber(AbsorberNum).NomCapWasAutoSized = true;
            }
            BLASTAbsorber(AbsorberNum).NomPumpPower = DataIPShortCuts::rNumericArgs(2);
            if (BLASTAbsorber(AbsorberNum).NomPumpPower == DataSizing::AutoSize) {
                BLASTAbsorber(AbsorberNum).NomPumpPowerWasAutoSized = true;
            }
            if (DataIPShortCuts::rNumericArgs(1) == 0.0) {
                ShowSevereError("Invalid " + DataIPShortCuts::cNumericFieldNames(1) + '=' + General::RoundSigDigits(DataIPShortCuts::rNumericArgs(1), 2));
                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                ErrorsFound = true;
            }
            // Assign Node Numbers to specified nodes
            BLASTAbsorber(AbsorberNum).EvapInletNodeNum = NodeInputManager::GetOnlySingleNode(
                DataIPShortCuts::cAlphaArgs(2), ErrorsFound, DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Inlet, 1, DataLoopNode::ObjectIsNotParent);
            BLASTAbsorber(AbsorberNum).EvapOutletNodeNum = NodeInputManager::GetOnlySingleNode(
                DataIPShortCuts::cAlphaArgs(3), ErrorsFound, DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Outlet, 1, DataLoopNode::ObjectIsNotParent);
            BranchNodeConnections::TestCompSet(DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataIPShortCuts::cAlphaArgs(2), DataIPShortCuts::cAlphaArgs(3), "Chilled Water Nodes");

            BLASTAbsorber(AbsorberNum).CondInletNodeNum = NodeInputManager::GetOnlySingleNode(
                DataIPShortCuts::cAlphaArgs(4), ErrorsFound, DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Inlet, 2, DataLoopNode::ObjectIsNotParent);
            BLASTAbsorber(AbsorberNum).CondOutletNodeNum = NodeInputManager::GetOnlySingleNode(
                DataIPShortCuts::cAlphaArgs(5), ErrorsFound, DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Outlet, 2, DataLoopNode::ObjectIsNotParent);
            BranchNodeConnections::TestCompSet(DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataIPShortCuts::cAlphaArgs(4), DataIPShortCuts::cAlphaArgs(5), "Condenser (not tested) Nodes");

            if (NumAlphas > 8) {
                if (UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(9), "HotWater") || UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(9), "HotWater")) {
                    BLASTAbsorber(AbsorberNum).GenHeatSourceType = DataLoopNode::NodeType_Water;
                } else if (UtilityRoutines::SameString(DataIPShortCuts::cAlphaArgs(9), "Steam") || DataIPShortCuts::cAlphaArgs(9).empty()) {
                    BLASTAbsorber(AbsorberNum).GenHeatSourceType = DataLoopNode::NodeType_Steam;
                } else {
                    ShowSevereError("Invalid " + DataIPShortCuts::cAlphaFieldNames(9) + '=' + DataIPShortCuts::cAlphaArgs(9));
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                    ShowContinueError("...Generator heat source type must be Steam or Hot Water.");
                    ErrorsFound = true;
                }
            } else {
                BLASTAbsorber(AbsorberNum).GenHeatSourceType = DataLoopNode::NodeType_Steam;
            }

            if (!DataIPShortCuts::lAlphaFieldBlanks(6) && !DataIPShortCuts::lAlphaFieldBlanks(7)) {
                GenInputOutputNodesUsed(AbsorberNum) = true;
                if (BLASTAbsorber(AbsorberNum).GenHeatSourceType == DataLoopNode::NodeType_Water) {
                    BLASTAbsorber(AbsorberNum).GeneratorInletNodeNum = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(6),
                                                                                         ErrorsFound,
                                                                                         DataIPShortCuts::cCurrentModuleObject,
                                                                                         DataIPShortCuts::cAlphaArgs(1),
                                                                                         DataLoopNode::NodeType_Water,
                                                                                         DataLoopNode::NodeConnectionType_Inlet,
                                                                                         3,
                                                                                         DataLoopNode::ObjectIsNotParent);
                    BLASTAbsorber(AbsorberNum).GeneratorOutletNodeNum = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(7),
                                                                                          ErrorsFound,
                                                                                          DataIPShortCuts::cCurrentModuleObject,
                                                                                          DataIPShortCuts::cAlphaArgs(1),
                                                                                          DataLoopNode::NodeType_Water,
                                                                                          DataLoopNode::NodeConnectionType_Outlet,
                                                                                          3,
                                                                                          DataLoopNode::ObjectIsNotParent);
                    BranchNodeConnections::TestCompSet(DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataIPShortCuts::cAlphaArgs(6), DataIPShortCuts::cAlphaArgs(7), "Hot Water Nodes");
                } else {
                    BLASTAbsorber(AbsorberNum).SteamFluidIndex = FluidProperties::FindRefrigerant("STEAM");
                    BLASTAbsorber(AbsorberNum).GeneratorInletNodeNum = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(6),
                                                                                         ErrorsFound,
                                                                                         DataIPShortCuts::cCurrentModuleObject,
                                                                                         DataIPShortCuts::cAlphaArgs(1),
                                                                                         DataLoopNode::NodeType_Steam,
                                                                                         DataLoopNode::NodeConnectionType_Inlet,
                                                                                         3,
                                                                                         DataLoopNode::ObjectIsNotParent);
                    BLASTAbsorber(AbsorberNum).GeneratorOutletNodeNum = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(7),
                                                                                          ErrorsFound,
                                                                                          DataIPShortCuts::cCurrentModuleObject,
                                                                                          DataIPShortCuts::cAlphaArgs(1),
                                                                                          DataLoopNode::NodeType_Steam,
                                                                                          DataLoopNode::NodeConnectionType_Outlet,
                                                                                          3,
                                                                                          DataLoopNode::ObjectIsNotParent);
                    BranchNodeConnections::TestCompSet(DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataIPShortCuts::cAlphaArgs(6), DataIPShortCuts::cAlphaArgs(7), "Steam Nodes");
                }
            } else if ((DataIPShortCuts::lAlphaFieldBlanks(6) && !DataIPShortCuts::lAlphaFieldBlanks(7)) || (!DataIPShortCuts::lAlphaFieldBlanks(6) && DataIPShortCuts::lAlphaFieldBlanks(7))) {
                ShowSevereError(DataIPShortCuts::cCurrentModuleObject + ", Name=" + DataIPShortCuts::cAlphaArgs(1));
                ShowContinueError("...Generator fluid nodes must both be entered (or both left blank).");
                ShowContinueError("..." + DataIPShortCuts::cAlphaFieldNames(6) + " = " + DataIPShortCuts::cAlphaArgs(6));
                ShowContinueError("..." + DataIPShortCuts::cAlphaFieldNames(7) + " = " + DataIPShortCuts::cAlphaArgs(7));
                ErrorsFound = true;
            } else {
                if (BLASTAbsorber(AbsorberNum).GenHeatSourceType == DataLoopNode::NodeType_Water) {
                    ShowWarningError(DataIPShortCuts::cCurrentModuleObject + ", Name=" + DataIPShortCuts::cAlphaArgs(1));
                    ShowContinueError("...Generator fluid type must be Steam if generator inlet/outlet nodes are blank.");
                    ShowContinueError("...Generator fluid type is set to Steam and the simulation continues.");
                    BLASTAbsorber(AbsorberNum).GenHeatSourceType = DataLoopNode::NodeType_Steam;
                }
            }

            // Get remaining data
            BLASTAbsorber(AbsorberNum).MinPartLoadRat = DataIPShortCuts::rNumericArgs(3);
            BLASTAbsorber(AbsorberNum).MaxPartLoadRat = DataIPShortCuts::rNumericArgs(4);
            BLASTAbsorber(AbsorberNum).OptPartLoadRat = DataIPShortCuts::rNumericArgs(5);
            BLASTAbsorber(AbsorberNum).TempDesCondIn = DataIPShortCuts::rNumericArgs(6);
            BLASTAbsorber(AbsorberNum).EvapVolFlowRate = DataIPShortCuts::rNumericArgs(7);
            if (BLASTAbsorber(AbsorberNum).EvapVolFlowRate == DataSizing::AutoSize) {
                BLASTAbsorber(AbsorberNum).EvapVolFlowRateWasAutoSized = true;
            }
            BLASTAbsorber(AbsorberNum).CondVolFlowRate = DataIPShortCuts::rNumericArgs(8);
            if (BLASTAbsorber(AbsorberNum).CondVolFlowRate == DataSizing::AutoSize) {
                BLASTAbsorber(AbsorberNum).CondVolFlowRateWasAutoSized = true;
            }
            BLASTAbsorber(AbsorberNum).SteamLoadCoef(1) = DataIPShortCuts::rNumericArgs(9);
            BLASTAbsorber(AbsorberNum).SteamLoadCoef(2) = DataIPShortCuts::rNumericArgs(10);
            BLASTAbsorber(AbsorberNum).SteamLoadCoef(3) = DataIPShortCuts::rNumericArgs(11);
            BLASTAbsorber(AbsorberNum).PumpPowerCoef(1) = DataIPShortCuts::rNumericArgs(12);
            BLASTAbsorber(AbsorberNum).PumpPowerCoef(2) = DataIPShortCuts::rNumericArgs(13);
            BLASTAbsorber(AbsorberNum).PumpPowerCoef(3) = DataIPShortCuts::rNumericArgs(14);
            BLASTAbsorber(AbsorberNum).TempLowLimitEvapOut = DataIPShortCuts::rNumericArgs(15);

            {
                auto const SELECT_CASE_var(DataIPShortCuts::cAlphaArgs(8));
                if (SELECT_CASE_var == "CONSTANTFLOW") {
                    BLASTAbsorber(AbsorberNum).FlowMode = ConstantFlow;
                } else if (SELECT_CASE_var == "LEAVINGSETPOINTMODULATED") {
                    BLASTAbsorber(AbsorberNum).FlowMode = LeavingSetPointModulated;
                } else if (SELECT_CASE_var == "NOTMODULATED") {
                    BLASTAbsorber(AbsorberNum).FlowMode = NotModulated;
                } else {
                    ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\",");
                    ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(8) + '=' + DataIPShortCuts::cAlphaArgs(8));
                    ShowContinueError("Available choices are ConstantFlow, NotModulated, or LeavingSetpointModulated");
                    ShowContinueError("Flow mode NotModulated is assumed and the simulation continues.");
                    BLASTAbsorber(AbsorberNum).FlowMode = NotModulated;
                }
            }

            if (NumNums > 15) {
                BLASTAbsorber(AbsorberNum).GeneratorVolFlowRate = DataIPShortCuts::rNumericArgs(16);
                if (BLASTAbsorber(AbsorberNum).GeneratorVolFlowRate == DataSizing::AutoSize) {
                    BLASTAbsorber(AbsorberNum).GeneratorVolFlowRateWasAutoSized = true;
                }
            }

            if (BLASTAbsorber(AbsorberNum).GeneratorVolFlowRate == 0.0 && BLASTAbsorber(AbsorberNum).GenHeatSourceType == DataLoopNode::NodeType_Water) {
                ShowSevereError("Invalid " + DataIPShortCuts::cNumericFieldNames(16) + '=' + General::RoundSigDigits(DataIPShortCuts::rNumericArgs(16), 2));
                ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                ShowContinueError("...Generator water flow rate must be greater than 0 when absorber generator fluid type is hot water.");
                ErrorsFound = true;
            }

            if (NumNums > 16) {
                BLASTAbsorber(AbsorberNum).GeneratorSubcool = DataIPShortCuts::rNumericArgs(17);
            } else {
                BLASTAbsorber(AbsorberNum).GeneratorSubcool = 1.0;
            }

            if (NumNums > 17) {
                BLASTAbsorber(AbsorberNum).SizFac = DataIPShortCuts::rNumericArgs(18);
            } else {
                BLASTAbsorber(AbsorberNum).SizFac = 1.0;
            }
        }

        if (ErrorsFound) {
            ShowFatalError("Errors found in processing input for " + DataIPShortCuts::cCurrentModuleObject);
        }

        for (AbsorberNum = 1; AbsorberNum <= NumBLASTAbsorbers; ++AbsorberNum) {
            SetupOutputVariable("Chiller Electric Power",
                                OutputProcessor::Unit::W,
                                BLASTAbsorberReport(AbsorberNum).PumpingPower,
                                "System",
                                "Average",
                                BLASTAbsorber(AbsorberNum).Name);
            SetupOutputVariable("Chiller Electric Energy",
                                OutputProcessor::Unit::J,
                                BLASTAbsorberReport(AbsorberNum).PumpingEnergy,
                                "System",
                                "Sum",
                                BLASTAbsorber(AbsorberNum).Name,
                                _,
                                "ELECTRICITY",
                                "Cooling",
                                _,
                                "Plant");
            SetupOutputVariable("Chiller Evaporator Cooling Rate",
                                OutputProcessor::Unit::W,
                                BLASTAbsorberReport(AbsorberNum).QEvap,
                                "System",
                                "Average",
                                BLASTAbsorber(AbsorberNum).Name);
            SetupOutputVariable("Chiller Evaporator Cooling Energy",
                                OutputProcessor::Unit::J,
                                BLASTAbsorberReport(AbsorberNum).EvapEnergy,
                                "System",
                                "Sum",
                                BLASTAbsorber(AbsorberNum).Name,
                                _,
                                "ENERGYTRANSFER",
                                "CHILLERS",
                                _,
                                "Plant");
            SetupOutputVariable("Chiller Evaporator Inlet Temperature",
                                OutputProcessor::Unit::C,
                                BLASTAbsorberReport(AbsorberNum).EvapInletTemp,
                                "System",
                                "Average",
                                BLASTAbsorber(AbsorberNum).Name);
            SetupOutputVariable("Chiller Evaporator Outlet Temperature",
                                OutputProcessor::Unit::C,
                                BLASTAbsorberReport(AbsorberNum).EvapOutletTemp,
                                "System",
                                "Average",
                                BLASTAbsorber(AbsorberNum).Name);
            SetupOutputVariable("Chiller Evaporator Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                BLASTAbsorberReport(AbsorberNum).Evapmdot,
                                "System",
                                "Average",
                                BLASTAbsorber(AbsorberNum).Name);

            SetupOutputVariable("Chiller Condenser Heat Transfer Rate",
                                OutputProcessor::Unit::W,
                                BLASTAbsorberReport(AbsorberNum).QCond,
                                "System",
                                "Average",
                                BLASTAbsorber(AbsorberNum).Name);
            SetupOutputVariable("Chiller Condenser Heat Transfer Energy",
                                OutputProcessor::Unit::J,
                                BLASTAbsorberReport(AbsorberNum).CondEnergy,
                                "System",
                                "Sum",
                                BLASTAbsorber(AbsorberNum).Name,
                                _,
                                "ENERGYTRANSFER",
                                "HEATREJECTION",
                                _,
                                "Plant");
            SetupOutputVariable("Chiller Condenser Inlet Temperature",
                                OutputProcessor::Unit::C,
                                BLASTAbsorberReport(AbsorberNum).CondInletTemp,
                                "System",
                                "Average",
                                BLASTAbsorber(AbsorberNum).Name);
            SetupOutputVariable("Chiller Condenser Outlet Temperature",
                                OutputProcessor::Unit::C,
                                BLASTAbsorberReport(AbsorberNum).CondOutletTemp,
                                "System",
                                "Average",
                                BLASTAbsorber(AbsorberNum).Name);
            SetupOutputVariable("Chiller Condenser Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                BLASTAbsorberReport(AbsorberNum).Condmdot,
                                "System",
                                "Average",
                                BLASTAbsorber(AbsorberNum).Name);

            if (BLASTAbsorber(AbsorberNum).GenHeatSourceType == DataLoopNode::NodeType_Water) {
                SetupOutputVariable("Chiller Hot Water Consumption Rate",
                                    OutputProcessor::Unit::W,
                                    BLASTAbsorberReport(AbsorberNum).QGenerator,
                                    "System",
                                    "Average",
                                    BLASTAbsorber(AbsorberNum).Name);
                SetupOutputVariable("Chiller Source Hot Water Energy",
                                    OutputProcessor::Unit::J,
                                    BLASTAbsorberReport(AbsorberNum).GeneratorEnergy,
                                    "System",
                                    "Sum",
                                    BLASTAbsorber(AbsorberNum).Name,
                                    _,
                                    "PLANTLOOPHEATINGDEMAND",
                                    "CHILLERS",
                                    _,
                                    "Plant");
            } else {
                if (GenInputOutputNodesUsed(AbsorberNum)) {
                    SetupOutputVariable("Chiller Source Steam Rate",
                                        OutputProcessor::Unit::W,
                                        BLASTAbsorberReport(AbsorberNum).QGenerator,
                                        "System",
                                        "Average",
                                        BLASTAbsorber(AbsorberNum).Name);
                    SetupOutputVariable("Chiller Source Steam Energy",
                                        OutputProcessor::Unit::J,
                                        BLASTAbsorberReport(AbsorberNum).GeneratorEnergy,
                                        "System",
                                        "Sum",
                                        BLASTAbsorber(AbsorberNum).Name,
                                        _,
                                        "PLANTLOOPHEATINGDEMAND",
                                        "CHILLERS",
                                        _,
                                        "Plant");
                } else {
                    SetupOutputVariable("Chiller Source Steam Rate",
                                        OutputProcessor::Unit::W,
                                        BLASTAbsorberReport(AbsorberNum).QGenerator,
                                        "System",
                                        "Average",
                                        BLASTAbsorber(AbsorberNum).Name);
                    SetupOutputVariable("Chiller Source Steam Energy",
                                        OutputProcessor::Unit::J,
                                        BLASTAbsorberReport(AbsorberNum).GeneratorEnergy,
                                        "System",
                                        "Sum",
                                        BLASTAbsorber(AbsorberNum).Name,
                                        _,
                                        "Steam",
                                        "Cooling",
                                        _,
                                        "Plant");
                }
            }

            SetupOutputVariable("Chiller COP",
                                OutputProcessor::Unit::W_W,
                                BLASTAbsorberReport(AbsorberNum).ActualCOP,
                                "System",
                                "Average",
                                BLASTAbsorber(AbsorberNum).Name);

            if (DataGlobals::AnyEnergyManagementSystemInModel) {
                SetupEMSInternalVariable("Chiller Nominal Capacity", BLASTAbsorber(AbsorberNum).Name, "[W]", BLASTAbsorber(AbsorberNum).NomCap);
            }
        }

        if (allocated(GenInputOutputNodesUsed)) GenInputOutputNodesUsed.deallocate();
    }

    void InitBLASTAbsorberModel(int const ChillNum, // number of the current electric chiller being simulated
                                bool const RunFlag, // TRUE when chiller operating
                                Real64 const MyLoad)
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

        static bool MyOneTimeFlag(true);
        static Array1D_bool MyFlag;
        static Array1D_bool MyEnvrnFlag;
        int CondInletNode;  // node number of water inlet node to the condenser
        int CondOutletNode; // node number of water outlet node from the condenser
        bool errFlag;
        bool FatalError;
        Real64 rho;             // local fluid density
        Real64 CpWater;         // local specific heat
        Real64 EnthSteamOutDry; // dry enthalpy of steam (quality = 1)
        Real64 EnthSteamOutWet; // wet enthalpy of steam (quality = 0)
        Real64 HfgSteam;        // latent heat of steam at constant pressure
        Real64 SteamDeltaT;     // amount of sub-cooling of steam condensate
        int GeneratorInletNode; // generator inlet node number, steam/water side
        Real64 SteamOutletTemp;
        static int DummyWaterIndex(1);
        Real64 mdotEvap; // local fluid mass flow rate thru evaporator
        Real64 mdotCond; // local fluid mass flow rate thru condenser
        Real64 mdotGen;  // local fluid mass flow rate thru generator

        // Do the one time initializations
        if (MyOneTimeFlag) {
            MyFlag.allocate(NumBLASTAbsorbers);
            MyEnvrnFlag.allocate(NumBLASTAbsorbers);
            MyFlag = true;
            MyEnvrnFlag = true;
            MyOneTimeFlag = false;
        }

        // Init more variables
        if (MyFlag(ChillNum)) {
            // Locate the chillers on the plant loops for later usage
            errFlag = false;
            PlantUtilities::ScanPlantLoopsForObject(BLASTAbsorber(ChillNum).Name,
                                    DataPlant::TypeOf_Chiller_Absorption,
                                    BLASTAbsorber(ChillNum).CWLoopNum,
                                    BLASTAbsorber(ChillNum).CWLoopSideNum,
                                    BLASTAbsorber(ChillNum).CWBranchNum,
                                    BLASTAbsorber(ChillNum).CWCompNum,
                                    errFlag,
                                    BLASTAbsorber(ChillNum).TempLowLimitEvapOut,
                                    _,
                                    _,
                                    BLASTAbsorber(ChillNum).EvapInletNodeNum,
                                    _);
            if (BLASTAbsorber(ChillNum).CondInletNodeNum > 0) {
                PlantUtilities::ScanPlantLoopsForObject(BLASTAbsorber(ChillNum).Name,
                                        DataPlant::TypeOf_Chiller_Absorption,
                                        BLASTAbsorber(ChillNum).CDLoopNum,
                                        BLASTAbsorber(ChillNum).CDLoopSideNum,
                                        BLASTAbsorber(ChillNum).CDBranchNum,
                                        BLASTAbsorber(ChillNum).CDCompNum,
                                        errFlag,
                                        _,
                                        _,
                                        _,
                                        BLASTAbsorber(ChillNum).CondInletNodeNum,
                                        _);
                PlantUtilities::InterConnectTwoPlantLoopSides(BLASTAbsorber(ChillNum).CWLoopNum,
                                              BLASTAbsorber(ChillNum).CWLoopSideNum,
                                              BLASTAbsorber(ChillNum).CDLoopNum,
                                              BLASTAbsorber(ChillNum).CDLoopSideNum,
                                              DataPlant::TypeOf_Chiller_Absorption,
                                              true);
            }
            if (BLASTAbsorber(ChillNum).GeneratorInletNodeNum > 0) {
                PlantUtilities::ScanPlantLoopsForObject(BLASTAbsorber(ChillNum).Name,
                                        DataPlant::TypeOf_Chiller_Absorption,
                                        BLASTAbsorber(ChillNum).GenLoopNum,
                                        BLASTAbsorber(ChillNum).GenLoopSideNum,
                                        BLASTAbsorber(ChillNum).GenBranchNum,
                                        BLASTAbsorber(ChillNum).GenCompNum,
                                        errFlag,
                                        _,
                                        _,
                                        _,
                                        BLASTAbsorber(ChillNum).GeneratorInletNodeNum,
                                        _);
                PlantUtilities::InterConnectTwoPlantLoopSides(BLASTAbsorber(ChillNum).CWLoopNum,
                                              BLASTAbsorber(ChillNum).CWLoopSideNum,
                                              BLASTAbsorber(ChillNum).GenLoopNum,
                                              BLASTAbsorber(ChillNum).GenCompNum,
                                              DataPlant::TypeOf_Chiller_Absorption,
                                              true);
            }

            // Fill in connection data
            if ((BLASTAbsorber(ChillNum).CondInletNodeNum > 0) && (BLASTAbsorber(ChillNum).GeneratorInletNodeNum > 0)) {
                PlantUtilities::InterConnectTwoPlantLoopSides(BLASTAbsorber(ChillNum).CDLoopNum,
                                              BLASTAbsorber(ChillNum).CDLoopSideNum,
                                              BLASTAbsorber(ChillNum).GenLoopNum,
                                              BLASTAbsorber(ChillNum).GenCompNum,
                                              DataPlant::TypeOf_Chiller_Absorption,
                                              false);
            }
            if (errFlag) {
                ShowFatalError("InitBLASTAbsorberModel: Program terminated due to previous condition(s).");
            }

            if (BLASTAbsorber(ChillNum).FlowMode == ConstantFlow) {
                DataPlant::PlantLoop(BLASTAbsorber(ChillNum).CWLoopNum)
                    .LoopSide(BLASTAbsorber(ChillNum).CWLoopSideNum)
                    .Branch(BLASTAbsorber(ChillNum).CWBranchNum)
                    .Comp(BLASTAbsorber(ChillNum).CWCompNum)
                    .FlowPriority = DataPlant::LoopFlowStatus_NeedyIfLoopOn;
            }

            if (BLASTAbsorber(ChillNum).FlowMode == LeavingSetPointModulated) {
                DataPlant::PlantLoop(BLASTAbsorber(ChillNum).CWLoopNum)
                    .LoopSide(BLASTAbsorber(ChillNum).CWLoopSideNum)
                    .Branch(BLASTAbsorber(ChillNum).CWBranchNum)
                    .Comp(BLASTAbsorber(ChillNum).CWCompNum)
                    .FlowPriority = DataPlant::LoopFlowStatus_NeedyIfLoopOn;

                if ((DataLoopNode::Node(BLASTAbsorber(ChillNum).EvapOutletNodeNum).TempSetPoint == DataLoopNode::SensedNodeFlagValue) &&
                    (DataLoopNode::Node(BLASTAbsorber(ChillNum).EvapOutletNodeNum).TempSetPointHi == DataLoopNode::SensedNodeFlagValue)) {
                    if (!DataGlobals::AnyEnergyManagementSystemInModel) {
                        if (!BLASTAbsorber(ChillNum).ModulatedFlowErrDone) {
                            ShowWarningError("Missing temperature setpoint for LeavingSetpointModulated mode chiller named " +
                                             BLASTAbsorber(ChillNum).Name);
                            ShowContinueError(
                                "  A temperature setpoint is needed at the outlet node of a chiller in variable flow mode, use a SetpointManager");
                            ShowContinueError("  The overall loop setpoint will be assumed for chiller. The simulation continues ... ");
                            BLASTAbsorber(ChillNum).ModulatedFlowErrDone = true;
                        }
                    } else {
                        // need call to EMS to check node
                        FatalError = false; // but not really fatal yet, but should be.
                        EMSManager::CheckIfNodeSetPointManagedByEMS(BLASTAbsorber(ChillNum).EvapOutletNodeNum, EMSManager::iTemperatureSetPoint, FatalError);
                        if (FatalError) {
                            if (!BLASTAbsorber(ChillNum).ModulatedFlowErrDone) {
                                ShowWarningError("Missing temperature setpoint for LeavingSetpointModulated mode chiller named " +
                                                 BLASTAbsorber(ChillNum).Name);
                                ShowContinueError(
                                    "  A temperature setpoint is needed at the outlet node of a chiller evaporator in variable flow mode");
                                ShowContinueError("  use a Setpoint Manager to establish a setpoint at the chiller evaporator outlet node ");
                                ShowContinueError("  or use an EMS actuator to establish a setpoint at the outlet node ");
                                ShowContinueError("  The overall loop setpoint will be assumed for chiller. The simulation continues ... ");
                                BLASTAbsorber(ChillNum).ModulatedFlowErrDone = true;
                            }
                        }
                    }

                    BLASTAbsorber(ChillNum).ModulatedFlowSetToLoop = true;
                    DataLoopNode::Node(BLASTAbsorber(ChillNum).EvapOutletNodeNum).TempSetPoint =
                        DataLoopNode::Node(DataPlant::PlantLoop(BLASTAbsorber(ChillNum).CWLoopNum).TempSetPointNodeNum).TempSetPoint;
                    DataLoopNode::Node(BLASTAbsorber(ChillNum).EvapOutletNodeNum).TempSetPointHi =
                        DataLoopNode::Node(DataPlant::PlantLoop(BLASTAbsorber(ChillNum).CWLoopNum).TempSetPointNodeNum).TempSetPointHi;
                }
            }

            MyFlag(ChillNum) = false;
        }

        CondInletNode = BLASTAbsorber(ChillNum).CondInletNodeNum;
        CondOutletNode = BLASTAbsorber(ChillNum).CondOutletNodeNum;

        if (MyEnvrnFlag(ChillNum) && DataGlobals::BeginEnvrnFlag && (DataPlant::PlantFirstSizesOkayToFinalize)) {

            rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(BLASTAbsorber(ChillNum).CWLoopNum).FluidName,
                                   DataGlobals::CWInitConvTemp,
                                   DataPlant::PlantLoop(BLASTAbsorber(ChillNum).CWLoopNum).FluidIndex,
                                   RoutineName);

            BLASTAbsorber(ChillNum).EvapMassFlowRateMax = BLASTAbsorber(ChillNum).EvapVolFlowRate * rho;

            PlantUtilities::InitComponentNodes(0.0,
                               BLASTAbsorber(ChillNum).EvapMassFlowRateMax,
                               BLASTAbsorber(ChillNum).EvapInletNodeNum,
                               BLASTAbsorber(ChillNum).EvapOutletNodeNum,
                               BLASTAbsorber(ChillNum).CWLoopNum,
                               BLASTAbsorber(ChillNum).CWLoopSideNum,
                               BLASTAbsorber(ChillNum).CWBranchNum,
                               BLASTAbsorber(ChillNum).CWCompNum);

            rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(BLASTAbsorber(ChillNum).CDLoopNum).FluidName,
                                   DataGlobals::CWInitConvTemp,
                                   DataPlant::PlantLoop(BLASTAbsorber(ChillNum).CDLoopNum).FluidIndex,
                                   RoutineName);

            BLASTAbsorber(ChillNum).CondMassFlowRateMax = rho * BLASTAbsorber(ChillNum).CondVolFlowRate;

            PlantUtilities::InitComponentNodes(0.0,
                               BLASTAbsorber(ChillNum).CondMassFlowRateMax,
                               CondInletNode,
                               CondOutletNode,
                               BLASTAbsorber(ChillNum).CDLoopNum,
                               BLASTAbsorber(ChillNum).CDLoopSideNum,
                               BLASTAbsorber(ChillNum).CDBranchNum,
                               BLASTAbsorber(ChillNum).CDCompNum);
            DataLoopNode::Node(CondInletNode).Temp = BLASTAbsorber(ChillNum).TempDesCondIn;

            if (BLASTAbsorber(ChillNum).GeneratorInletNodeNum > 0) {

                if (BLASTAbsorber(ChillNum).GenHeatSourceType == DataLoopNode::NodeType_Water) {
                    rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(BLASTAbsorber(ChillNum).GenLoopNum).FluidName,
                                           DataGlobals::HWInitConvTemp,
                                           DataPlant::PlantLoop(BLASTAbsorber(ChillNum).GenLoopNum).FluidIndex,
                                           RoutineName);

                    BLASTAbsorber(ChillNum).GenMassFlowRateMax = rho * BLASTAbsorber(ChillNum).GeneratorVolFlowRate;
                } else if (BLASTAbsorber(ChillNum).GenHeatSourceType == DataLoopNode::NodeType_Steam) {

                    QGenerator = (BLASTAbsorber(ChillNum).SteamLoadCoef(1) + BLASTAbsorber(ChillNum).SteamLoadCoef(2) +
                                  BLASTAbsorber(ChillNum).SteamLoadCoef(3)) *
                                 BLASTAbsorber(ChillNum).NomCap;
                    GeneratorInletNode = BLASTAbsorber(ChillNum).GeneratorInletNodeNum;
                    EnthSteamOutDry = FluidProperties::GetSatEnthalpyRefrig(fluidNameSteam,
                                                           DataLoopNode::Node(GeneratorInletNode).Temp,
                                                           1.0,
                                                           BLASTAbsorber(ChillNum).SteamFluidIndex,
                                                           calcChillerAbsorption + BLASTAbsorber(ChillNum).Name);
                    EnthSteamOutWet = FluidProperties::GetSatEnthalpyRefrig(fluidNameSteam,
                                                           DataLoopNode::Node(GeneratorInletNode).Temp,
                                                           0.0,
                                                           BLASTAbsorber(ChillNum).SteamFluidIndex,
                                                           calcChillerAbsorption + BLASTAbsorber(ChillNum).Name);
                    SteamDeltaT = BLASTAbsorber(ChillNum).GeneratorSubcool;
                    SteamOutletTemp = DataLoopNode::Node(GeneratorInletNode).Temp - SteamDeltaT;
                    HfgSteam = EnthSteamOutDry - EnthSteamOutWet;
                    CpWater =
                        FluidProperties::GetDensityGlycol(fluidNameWater, SteamOutletTemp, DummyWaterIndex, calcChillerAbsorption + BLASTAbsorber(ChillNum).Name);
                    BLASTAbsorber(ChillNum).GenMassFlowRateMax = QGenerator / (HfgSteam + CpWater * SteamDeltaT);
                }

                PlantUtilities::InitComponentNodes(0.0,
                                   BLASTAbsorber(ChillNum).GenMassFlowRateMax,
                                   BLASTAbsorber(ChillNum).GeneratorInletNodeNum,
                                   BLASTAbsorber(ChillNum).GeneratorOutletNodeNum,
                                   BLASTAbsorber(ChillNum).GenLoopNum,
                                   BLASTAbsorber(ChillNum).GenLoopSideNum,
                                   BLASTAbsorber(ChillNum).GenBranchNum,
                                   BLASTAbsorber(ChillNum).GenCompNum);
            }

            MyEnvrnFlag(ChillNum) = false;
        }
        if (!DataGlobals::BeginEnvrnFlag) {
            MyEnvrnFlag(ChillNum) = true;
        }

        // every time inits

        if ((BLASTAbsorber(ChillNum).FlowMode == LeavingSetPointModulated) && BLASTAbsorber(ChillNum).ModulatedFlowSetToLoop) {
            // fix for clumsy old input that worked because loop setpoint was spread.
            //  could be removed with transition, testing , model change, period of being obsolete.
            DataLoopNode::Node(BLASTAbsorber(ChillNum).EvapOutletNodeNum).TempSetPoint =
                DataLoopNode::Node(DataPlant::PlantLoop(BLASTAbsorber(ChillNum).CWLoopNum).TempSetPointNodeNum).TempSetPoint;
            DataLoopNode::Node(BLASTAbsorber(ChillNum).EvapOutletNodeNum).TempSetPointHi =
                DataLoopNode::Node(DataPlant::PlantLoop(BLASTAbsorber(ChillNum).CWLoopNum).TempSetPointNodeNum).TempSetPointHi;
        }

        if ((MyLoad < 0.0) && RunFlag) {
            mdotEvap = BLASTAbsorber(ChillNum).EvapMassFlowRateMax;
            mdotCond = BLASTAbsorber(ChillNum).CondMassFlowRateMax;
            mdotGen = BLASTAbsorber(ChillNum).GenMassFlowRateMax;
        } else {
            mdotEvap = 0.0;
            mdotCond = 0.0;
            mdotGen = 0.0;
        }

        PlantUtilities::SetComponentFlowRate(mdotEvap,
                             BLASTAbsorber(ChillNum).EvapInletNodeNum,
                             BLASTAbsorber(ChillNum).EvapOutletNodeNum,
                             BLASTAbsorber(ChillNum).CWLoopNum,
                             BLASTAbsorber(ChillNum).CWLoopSideNum,
                             BLASTAbsorber(ChillNum).CWBranchNum,
                             BLASTAbsorber(ChillNum).CWCompNum);

        PlantUtilities::SetComponentFlowRate(mdotCond,
                             CondInletNode,
                             CondOutletNode,
                             BLASTAbsorber(ChillNum).CDLoopNum,
                             BLASTAbsorber(ChillNum).CDLoopSideNum,
                             BLASTAbsorber(ChillNum).CDBranchNum,
                             BLASTAbsorber(ChillNum).CDCompNum);

        if (BLASTAbsorber(ChillNum).GeneratorInletNodeNum > 0) {

            PlantUtilities::SetComponentFlowRate(mdotGen,
                                 BLASTAbsorber(ChillNum).GeneratorInletNodeNum,
                                 BLASTAbsorber(ChillNum).GeneratorOutletNodeNum,
                                 BLASTAbsorber(ChillNum).GenLoopNum,
                                 BLASTAbsorber(ChillNum).GenLoopSideNum,
                                 BLASTAbsorber(ChillNum).GenBranchNum,
                                 BLASTAbsorber(ChillNum).GenCompNum);
        }
    }

    void SizeAbsorpChiller(int const ChillNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   March 2008
        //       MODIFIED:      R. Raustad May 2008 - added generator node sizing
        //                      November 2013 Daeho Kang, add component sizing table entries
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for sizing Constabt COP Chiller Components for which capacities and flow rates
        // have not been specified in the input.

        // METHODOLOGY EMPLOYED:
        // Obtains evaporator flow rate from the plant sizing array. Calculates nominal capacity from
        // the evaporator flow rate and the chilled water loop design delta T. The condenser flow rate
        // is calculated from the nominal capacity, the COP, and the condenser loop design delta T.

        Real64 SteamMassFlowRate; // steam mass flow rate through generator

        static std::string const RoutineName("SizeAbsorpChiller");
        static std::string const RoutineNameLong("SizeAbsorptionChiller");

        int PltSizIndex;            // Plant Sizing Do loop index
        int PltSizNum(0);           // Plant Sizing index corresponding to CurLoopNum
        int PltSizCondNum(0);       // Plant Sizing index for condenser loop
        int PltSizSteamNum(0);      // Plant Sizing index for steam heating loop
        int PltSizHeatingNum(0);    // Plant Sizing index for how water heating loop
        Real64 SteamInputRatNom;    // nominal energy input ratio (steam or hot water)
        Real64 SteamDensity;        // density of generator steam (when connected to a steam loop)
        Real64 EnthSteamOutDry;     // dry enthalpy of steam (quality = 1)
        Real64 EnthSteamOutWet;     // wet enthalpy of steam (quality = 0)
        Real64 HfgSteam;            // latent heat of steam at constant pressure
        Real64 SteamDeltaT;         // amount of sub-cooling of steam condensate
        Real64 CpWater;             // specific heat of generator fluid (when connected to a hot water loop)
        Real64 RhoWater;            // density of water
        Real64 GeneratorOutletTemp; // outlet temperature of generator
        bool ErrorsFound(false);    // If errors detected in input
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

        Real64 NomCapUser(0.0);               // Hardsized nominal capacity for reporting
        Real64 NomPumpPowerUser(0.0);         // Hardsized nominal pump power for reporting
        Real64 EvapVolFlowRateUser(0.0);      // Hardsized evaporator volume flow rate for reporting
        Real64 CondVolFlowRateUser(0.0);      // Hardsized condenser flow rate for reporting
        Real64 GeneratorVolFlowRateUser(0.0); // Hardsized generator flow rate for reporting

        SteamInputRatNom =
            BLASTAbsorber(ChillNum).SteamLoadCoef(1) + BLASTAbsorber(ChillNum).SteamLoadCoef(2) + BLASTAbsorber(ChillNum).SteamLoadCoef(3);
        // init local temporary version in case of partial/mixed autosizing
        tmpNomCap = BLASTAbsorber(ChillNum).NomCap;
        tmpNomPumpPower = BLASTAbsorber(ChillNum).NomPumpPower;
        tmpEvapVolFlowRate = BLASTAbsorber(ChillNum).EvapVolFlowRate;
        tmpCondVolFlowRate = BLASTAbsorber(ChillNum).CondVolFlowRate;
        tmpGeneratorVolFlowRate = BLASTAbsorber(ChillNum).GeneratorVolFlowRate;

        // find the appropriate Plant Sizing object
        PltSizNum = DataPlant::PlantLoop(BLASTAbsorber(ChillNum).CWLoopNum).PlantSizNum;
        PltSizCondNum = DataPlant::PlantLoop(BLASTAbsorber(ChillNum).CDLoopNum).PlantSizNum;

        if (BLASTAbsorber(ChillNum).GenHeatSourceType == DataLoopNode::NodeType_Steam) {
            if (BLASTAbsorber(ChillNum).GeneratorInletNodeNum > 0 && BLASTAbsorber(ChillNum).GeneratorOutletNodeNum > 0) {
                PltSizSteamNum = PlantUtilities::MyPlantSizingIndex(moduleObjectType,
                                                    BLASTAbsorber(ChillNum).Name,
                                                    BLASTAbsorber(ChillNum).GeneratorInletNodeNum,
                                                    BLASTAbsorber(ChillNum).GeneratorOutletNodeNum,
                                                    LoopErrorsFound);
            } else {
                for (PltSizIndex = 1; PltSizIndex <= DataSizing::NumPltSizInput; ++PltSizIndex) {
                    if (DataSizing::PlantSizData(PltSizIndex).LoopType == DataSizing::SteamLoop) {
                        PltSizSteamNum = PltSizIndex;
                    }
                }
            }
        } else {
            if (BLASTAbsorber(ChillNum).GeneratorInletNodeNum > 0 && BLASTAbsorber(ChillNum).GeneratorOutletNodeNum > 0) {
                PltSizHeatingNum = PlantUtilities::MyPlantSizingIndex(moduleObjectType,
                                                      BLASTAbsorber(ChillNum).Name,
                                                      BLASTAbsorber(ChillNum).GeneratorInletNodeNum,
                                                      BLASTAbsorber(ChillNum).GeneratorOutletNodeNum,
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
            if (DataSizing::PlantSizData(PltSizNum).DesVolFlowRate >= DataHVACGlobals::TimeStepSys) {

                Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(BLASTAbsorber(ChillNum).CWLoopNum).FluidName,
                                           DataGlobals::CWInitConvTemp,
                                           DataPlant::PlantLoop(BLASTAbsorber(ChillNum).CWLoopNum).FluidIndex,
                                           RoutineName);

                rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(BLASTAbsorber(ChillNum).CWLoopNum).FluidName,
                                       DataGlobals::CWInitConvTemp,
                                       DataPlant::PlantLoop(BLASTAbsorber(ChillNum).CWLoopNum).FluidIndex,
                                       RoutineName);
                tmpNomCap = Cp * rho * DataSizing::PlantSizData(PltSizNum).DeltaT * DataSizing::PlantSizData(PltSizNum).DesVolFlowRate * BLASTAbsorber(ChillNum).SizFac;
                if (!BLASTAbsorber(ChillNum).NomCapWasAutoSized) tmpNomCap = BLASTAbsorber(ChillNum).NomCap;
            } else {
                if (BLASTAbsorber(ChillNum).NomCapWasAutoSized) tmpNomCap = 0.0;
            }
            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                if (BLASTAbsorber(ChillNum).NomCapWasAutoSized) {
                    BLASTAbsorber(ChillNum).NomCap = tmpNomCap;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(moduleObjectType, BLASTAbsorber(ChillNum).Name, "Design Size Nominal Capacity [W]", tmpNomCap);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(moduleObjectType, BLASTAbsorber(ChillNum).Name, "Initial Design Size Nominal Capacity [W]", tmpNomCap);
                    }
                } else {
                    if (BLASTAbsorber(ChillNum).NomCap > 0.0 && tmpNomCap > 0.0) {
                        NomCapUser = BLASTAbsorber(ChillNum).NomCap;
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput(moduleObjectType,
                                               BLASTAbsorber(ChillNum).Name,
                                               "Design Size Nominal Capacity [W]",
                                               tmpNomCap,
                                               "User-Specified Nominal Capacity [W]",
                                               NomCapUser);
                            if (DataGlobals::DisplayExtraWarnings) {
                                if ((std::abs(tmpNomCap - NomCapUser) / NomCapUser) > DataSizing::AutoVsHardSizingThreshold) {
                                    ShowMessage("SizeChillerAbsorption: Potential issue with equipment sizing for " + BLASTAbsorber(ChillNum).Name);
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
            if (BLASTAbsorber(ChillNum).NomCapWasAutoSized && DataPlant::PlantFirstSizesOkayToFinalize) {
                ShowSevereError("Autosizing of Absorption Chiller nominal capacity requires a loop Sizing:Plant object");
                ShowContinueError("Occurs in Chiller:Absorption object=" + BLASTAbsorber(ChillNum).Name);
                ErrorsFound = true;
            }
            if (!BLASTAbsorber(ChillNum).NomCapWasAutoSized && DataPlant::PlantFinalSizesOkayToReport && BLASTAbsorber(ChillNum).NomCap > 0.0) {
                ReportSizingManager::ReportSizingOutput(
                    moduleObjectType, BLASTAbsorber(ChillNum).Name, "User-Specified Nominal Capacity [W]", BLASTAbsorber(ChillNum).NomCap);
            }
        }

        tmpNomPumpPower = 0.0045 * BLASTAbsorber(ChillNum).NomCap;

        if (DataPlant::PlantFirstSizesOkayToFinalize) {
            // the DOE-2 EIR for single stage absorption chiller
            if (BLASTAbsorber(ChillNum).NomPumpPowerWasAutoSized) {
                BLASTAbsorber(ChillNum).NomPumpPower = tmpNomPumpPower;
                if (DataPlant::PlantFinalSizesOkayToReport) {
                    ReportSizingManager::ReportSizingOutput(moduleObjectType, BLASTAbsorber(ChillNum).Name, "Design Size Nominal Pumping Power [W]", tmpNomPumpPower);
                }
                if (DataPlant::PlantFirstSizesOkayToReport) {
                    ReportSizingManager::ReportSizingOutput(
                        moduleObjectType, BLASTAbsorber(ChillNum).Name, "Initial Design Size Nominal Pumping Power [W]", tmpNomPumpPower);
                }
            } else {
                if (BLASTAbsorber(ChillNum).NomPumpPower > 0.0 && tmpNomPumpPower > 0.0) {
                    NomPumpPowerUser = BLASTAbsorber(ChillNum).NomPumpPower;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(moduleObjectType,
                                           BLASTAbsorber(ChillNum).Name,
                                           "Design Size Nominal Pumping Power [W]",
                                           tmpNomPumpPower,
                                           "User-Specified Nominal Pumping Power [W]",
                                           NomPumpPowerUser);
                        if (DataGlobals::DisplayExtraWarnings) {
                            if ((std::abs(tmpNomPumpPower - NomPumpPowerUser) / NomPumpPowerUser) > DataSizing::AutoVsHardSizingThreshold) {
                                ShowMessage("SizeChillerAbsorption: Potential issue with equipment sizing for " + BLASTAbsorber(ChillNum).Name);
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
            if (DataSizing::PlantSizData(PltSizNum).DesVolFlowRate >= DataHVACGlobals::TimeStepSys) {
                tmpEvapVolFlowRate = DataSizing::PlantSizData(PltSizNum).DesVolFlowRate * BLASTAbsorber(ChillNum).SizFac;
                if (!BLASTAbsorber(ChillNum).EvapVolFlowRateWasAutoSized) tmpEvapVolFlowRate = BLASTAbsorber(ChillNum).EvapVolFlowRate;
            } else {
                if (BLASTAbsorber(ChillNum).EvapVolFlowRateWasAutoSized) tmpEvapVolFlowRate = 0.0;
            }
            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                if (BLASTAbsorber(ChillNum).EvapVolFlowRateWasAutoSized) {
                    BLASTAbsorber(ChillNum).EvapVolFlowRate = tmpEvapVolFlowRate;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(
                            moduleObjectType, BLASTAbsorber(ChillNum).Name, "Design Size Design Chilled Water Flow Rate [m3/s]", tmpEvapVolFlowRate);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(moduleObjectType,
                                           BLASTAbsorber(ChillNum).Name,
                                           "Initial Design Size Design Chilled Water Flow Rate [m3/s]",
                                           tmpEvapVolFlowRate);
                    }
                } else {
                    if (BLASTAbsorber(ChillNum).EvapVolFlowRate > 0.0 && tmpEvapVolFlowRate > 0.0) {
                        EvapVolFlowRateUser = BLASTAbsorber(ChillNum).EvapVolFlowRate;
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput(moduleObjectType,
                                               BLASTAbsorber(ChillNum).Name,
                                               "Design Size Design Chilled Water Flow Rate [m3/s]",
                                               tmpEvapVolFlowRate,
                                               "User-Specified Design Chilled Water Flow Rate [m3/s]",
                                               EvapVolFlowRateUser);
                            if (DataGlobals::DisplayExtraWarnings) {
                                if ((std::abs(tmpEvapVolFlowRate - EvapVolFlowRateUser) / EvapVolFlowRateUser) > DataSizing::AutoVsHardSizingThreshold) {
                                    ShowMessage("SizeChillerAbsorption: Potential issue with equipment sizing for " + BLASTAbsorber(ChillNum).Name);
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
            if (BLASTAbsorber(ChillNum).EvapVolFlowRateWasAutoSized && DataPlant::PlantFirstSizesOkayToFinalize) {
                ShowSevereError("Autosizing of Absorption Chiller evap flow rate requires a loop Sizing:Plant object");
                ShowContinueError("Occurs in CHILLER:ABSORPTION object=" + BLASTAbsorber(ChillNum).Name);
                ErrorsFound = true;
            }
            if (!BLASTAbsorber(ChillNum).EvapVolFlowRateWasAutoSized && DataPlant::PlantFinalSizesOkayToReport &&
                BLASTAbsorber(ChillNum).EvapVolFlowRate > 0.0) {
                ReportSizingManager::ReportSizingOutput(moduleObjectType,
                                   BLASTAbsorber(ChillNum).Name,
                                   "User-Specified Design Chilled Water Flow Rate [m3/s]",
                                   BLASTAbsorber(ChillNum).EvapVolFlowRate);
            }
        }

        PlantUtilities::RegisterPlantCompDesignFlow(BLASTAbsorber(ChillNum).EvapInletNodeNum, tmpEvapVolFlowRate);

        if (PltSizCondNum > 0 && PltSizNum > 0) {
            if (BLASTAbsorber(ChillNum).EvapVolFlowRate >= DataHVACGlobals::TimeStepSys && tmpNomCap > 0.0) {
                //       QCondenser = QEvaporator + QGenerator + PumpingPower

                Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(BLASTAbsorber(ChillNum).CDLoopNum).FluidName,
                                           BLASTAbsorber(ChillNum).TempDesCondIn,
                                           DataPlant::PlantLoop(BLASTAbsorber(ChillNum).CDLoopNum).FluidIndex,
                                           RoutineName);

                rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(BLASTAbsorber(ChillNum).CDLoopNum).FluidName,
                                       DataGlobals::CWInitConvTemp,
                                       DataPlant::PlantLoop(BLASTAbsorber(ChillNum).CDLoopNum).FluidIndex,
                                       RoutineName);
                tmpCondVolFlowRate =
                    tmpNomCap * (1.0 + SteamInputRatNom + tmpNomPumpPower / tmpNomCap) / (DataSizing::PlantSizData(PltSizCondNum).DeltaT * Cp * rho);
                if (!BLASTAbsorber(ChillNum).CondVolFlowRateWasAutoSized) tmpCondVolFlowRate = BLASTAbsorber(ChillNum).CondVolFlowRate;

            } else {
                if (BLASTAbsorber(ChillNum).CondVolFlowRateWasAutoSized) tmpCondVolFlowRate = 0.0;
            }
            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                if (BLASTAbsorber(ChillNum).CondVolFlowRateWasAutoSized) {
                    BLASTAbsorber(ChillNum).CondVolFlowRate = tmpCondVolFlowRate;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(moduleObjectType,
                                           BLASTAbsorber(ChillNum).Name,
                                           "Design Size Design Condenser Water Flow Rate [m3/s]",
                                           tmpCondVolFlowRate);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(moduleObjectType,
                                           BLASTAbsorber(ChillNum).Name,
                                           "Initial Design Size Design Condenser Water Flow Rate [m3/s]",
                                           tmpCondVolFlowRate);
                    }
                } else {
                    if (BLASTAbsorber(ChillNum).CondVolFlowRate > 0.0 && tmpCondVolFlowRate > 0.0) {
                        CondVolFlowRateUser = BLASTAbsorber(ChillNum).CondVolFlowRate;
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput(moduleObjectType,
                                               BLASTAbsorber(ChillNum).Name,
                                               "Design Size Design Condenser Water Flow Rate [m3/s]",
                                               tmpCondVolFlowRate,
                                               "User-Specified Design Condenser Water Flow Rate [m3/s]",
                                               CondVolFlowRateUser);
                            if (DataGlobals::DisplayExtraWarnings) {
                                if ((std::abs(tmpCondVolFlowRate - CondVolFlowRateUser) / CondVolFlowRateUser) > DataSizing::AutoVsHardSizingThreshold) {
                                    ShowMessage("SizeChillerAbsorption: Potential issue with equipment sizing for " + BLASTAbsorber(ChillNum).Name);
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
            if (BLASTAbsorber(ChillNum).CondVolFlowRateWasAutoSized && DataPlant::PlantFirstSizesOkayToFinalize) {
                ShowSevereError("Autosizing of Absorption Chiller condenser flow rate requires a condenser");
                ShowContinueError("loop Sizing:Plant object");
                ShowContinueError("Occurs in CHILLER:ABSORPTION object=" + BLASTAbsorber(ChillNum).Name);
                ErrorsFound = true;
            }
            if (!BLASTAbsorber(ChillNum).CondVolFlowRateWasAutoSized && DataPlant::PlantFirstSizesOkayToFinalize &&
                (BLASTAbsorber(ChillNum).CondVolFlowRate > 0.0)) {
                ReportSizingManager::ReportSizingOutput(moduleObjectType,
                                   BLASTAbsorber(ChillNum).Name,
                                   "User-Specified Design Condenser Water Flow Rate [m3/s]",
                                   BLASTAbsorber(ChillNum).CondVolFlowRate);
            }
        }

        // save the design condenser water volumetric flow rate for use by the condenser water loop sizing algorithms
        PlantUtilities::RegisterPlantCompDesignFlow(BLASTAbsorber(ChillNum).CondInletNodeNum, tmpCondVolFlowRate);

        if ((PltSizSteamNum > 0 && BLASTAbsorber(ChillNum).GenHeatSourceType == DataLoopNode::NodeType_Steam) ||
            (PltSizHeatingNum > 0 && BLASTAbsorber(ChillNum).GenHeatSourceType == DataLoopNode::NodeType_Water)) {
            if (BLASTAbsorber(ChillNum).EvapVolFlowRate >= DataHVACGlobals::TimeStepSys && tmpNomCap > 0.0) {
                if (BLASTAbsorber(ChillNum).GenHeatSourceType == DataLoopNode::NodeType_Water) {
                    CpWater = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(BLASTAbsorber(ChillNum).GenLoopNum).FluidName,
                                                    DataSizing::PlantSizData(PltSizHeatingNum).ExitTemp,
                                                    DataPlant::PlantLoop(BLASTAbsorber(ChillNum).GenLoopNum).FluidIndex,
                                                    RoutineName);
                    SteamDeltaT = max(0.5, DataSizing::PlantSizData(PltSizHeatingNum).DeltaT);
                    RhoWater = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(BLASTAbsorber(ChillNum).GenLoopNum).FluidName,
                                                (DataSizing::PlantSizData(PltSizHeatingNum).ExitTemp - SteamDeltaT),
                                                DataPlant::PlantLoop(BLASTAbsorber(ChillNum).GenLoopNum).FluidIndex,
                                                RoutineName);
                    tmpGeneratorVolFlowRate = (BLASTAbsorber(ChillNum).NomCap * SteamInputRatNom) / (CpWater * SteamDeltaT * RhoWater);
                    if (!BLASTAbsorber(ChillNum).GeneratorVolFlowRateWasAutoSized)
                        tmpGeneratorVolFlowRate = BLASTAbsorber(ChillNum).GeneratorVolFlowRate;
                    if (DataPlant::PlantFirstSizesOkayToFinalize) {
                        if (BLASTAbsorber(ChillNum).GeneratorVolFlowRateWasAutoSized) {
                            BLASTAbsorber(ChillNum).GeneratorVolFlowRate = tmpGeneratorVolFlowRate;
                            if (DataPlant::PlantFinalSizesOkayToReport) {
                                ReportSizingManager::ReportSizingOutput(moduleObjectType,
                                                   BLASTAbsorber(ChillNum).Name,
                                                   "Design Size Design Generator Fluid Flow Rate [m3/s]",
                                                   tmpGeneratorVolFlowRate);
                            }
                            if (DataPlant::PlantFirstSizesOkayToReport) {
                                ReportSizingManager::ReportSizingOutput(moduleObjectType,
                                                   BLASTAbsorber(ChillNum).Name,
                                                   "Iniital Design Size Design Generator Fluid Flow Rate [m3/s]",
                                                   tmpGeneratorVolFlowRate);
                            }
                        } else {
                            if (BLASTAbsorber(ChillNum).GeneratorVolFlowRate > 0.0 && tmpGeneratorVolFlowRate > 0.0) {
                                GeneratorVolFlowRateUser = BLASTAbsorber(ChillNum).GeneratorVolFlowRate;
                                if (DataPlant::PlantFinalSizesOkayToReport) {
                                    ReportSizingManager::ReportSizingOutput(moduleObjectType,
                                                       BLASTAbsorber(ChillNum).Name,
                                                       "Design Size Design Generator Fluid Flow Rate [m3/s]",
                                                       tmpGeneratorVolFlowRate,
                                                       "User-Specified Design Generator Fluid Flow Rate [m3/s]",
                                                       GeneratorVolFlowRateUser);
                                    if (DataGlobals::DisplayExtraWarnings) {
                                        if ((std::abs(tmpGeneratorVolFlowRate - GeneratorVolFlowRateUser) / GeneratorVolFlowRateUser) >
                                            DataSizing::AutoVsHardSizingThreshold) {
                                            ShowMessage("SizeChillerAbsorption: Potential issue with equipment sizing for " +
                                                        BLASTAbsorber(ChillNum).Name);
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
                    SteamDensity = FluidProperties::GetSatDensityRefrig(
                        fluidNameSteam, DataSizing::PlantSizData(PltSizSteamNum).ExitTemp, 1.0, BLASTAbsorber(ChillNum).SteamFluidIndex, RoutineNameLong);
                    SteamDeltaT = DataSizing::PlantSizData(PltSizSteamNum).DeltaT;
                    GeneratorOutletTemp = DataSizing::PlantSizData(PltSizSteamNum).ExitTemp - SteamDeltaT;

                    EnthSteamOutDry = FluidProperties::GetSatEnthalpyRefrig(fluidNameSteam,
                                                           DataSizing::PlantSizData(PltSizSteamNum).ExitTemp,
                                                           1.0,
                                                           BLASTAbsorber(ChillNum).SteamFluidIndex,
                                                           moduleObjectType + BLASTAbsorber(ChillNum).Name);
                    EnthSteamOutWet = FluidProperties::GetSatEnthalpyRefrig(fluidNameSteam,
                                                           DataSizing::PlantSizData(PltSizSteamNum).ExitTemp,
                                                           0.0,
                                                           BLASTAbsorber(ChillNum).SteamFluidIndex,
                                                           moduleObjectType + BLASTAbsorber(ChillNum).Name);
                    CpWater = FluidProperties::GetSpecificHeatGlycol(fluidNameWater, GeneratorOutletTemp, DummWaterIndex, RoutineName);
                    HfgSteam = EnthSteamOutDry - EnthSteamOutWet;
                    SteamMassFlowRate = (BLASTAbsorber(ChillNum).NomCap * SteamInputRatNom) / ((HfgSteam) + (SteamDeltaT * CpWater));
                    tmpGeneratorVolFlowRate = SteamMassFlowRate / SteamDensity;

                    if (!BLASTAbsorber(ChillNum).GeneratorVolFlowRateWasAutoSized)
                        tmpGeneratorVolFlowRate = BLASTAbsorber(ChillNum).GeneratorVolFlowRate;
                    if (DataPlant::PlantFirstSizesOkayToFinalize) {

                        if (BLASTAbsorber(ChillNum).GeneratorVolFlowRateWasAutoSized) {
                            BLASTAbsorber(ChillNum).GeneratorVolFlowRate = tmpGeneratorVolFlowRate;
                            if (DataPlant::PlantFinalSizesOkayToReport) {
                                ReportSizingManager::ReportSizingOutput(moduleObjectType,
                                                   BLASTAbsorber(ChillNum).Name,
                                                   "Design Size Design Generator Fluid Flow Rate [m3/s]",
                                                   tmpGeneratorVolFlowRate);
                            }
                            if (DataPlant::PlantFirstSizesOkayToReport) {
                                ReportSizingManager::ReportSizingOutput(moduleObjectType,
                                                   BLASTAbsorber(ChillNum).Name,
                                                   "Initial Design Size Design Generator Fluid Flow Rate [m3/s]",
                                                   tmpGeneratorVolFlowRate);
                            }
                        } else {
                            if (BLASTAbsorber(ChillNum).GeneratorVolFlowRate > 0.0 && tmpGeneratorVolFlowRate > 0.0) {
                                GeneratorVolFlowRateUser = BLASTAbsorber(ChillNum).GeneratorVolFlowRate;
                                if (DataPlant::PlantFinalSizesOkayToReport) {
                                    ReportSizingManager::ReportSizingOutput(moduleObjectType,
                                                       BLASTAbsorber(ChillNum).Name,
                                                       "Design Size Design Generator Fluid Flow Rate [m3/s]",
                                                       tmpGeneratorVolFlowRate,
                                                       "User-Specified Design Generator Fluid Flow Rate [m3/s]",
                                                       GeneratorVolFlowRateUser);
                                    if (DataGlobals::DisplayExtraWarnings) {
                                        if ((std::abs(tmpGeneratorVolFlowRate - GeneratorVolFlowRateUser) / GeneratorVolFlowRateUser) >
                                            DataSizing::AutoVsHardSizingThreshold) {
                                            ShowMessage("SizeChillerAbsorption: Potential issue with equipment sizing for " +
                                                        BLASTAbsorber(ChillNum).Name);
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
                if (BLASTAbsorber(ChillNum).GeneratorVolFlowRateWasAutoSized) {
                    if (DataPlant::PlantFirstSizesOkayToFinalize) {
                        BLASTAbsorber(ChillNum).GeneratorVolFlowRate = 0.0;
                    } else {
                        tmpGeneratorVolFlowRate = 0.0;
                    }
                }
            }
        } else {
            if (BLASTAbsorber(ChillNum).GeneratorVolFlowRateWasAutoSized && DataPlant::PlantFirstSizesOkayToFinalize) {
                ShowSevereError("Autosizing of Absorption Chiller generator flow rate requires a loop Sizing:Plant object.");
                ShowContinueError(" For steam loops, use a steam Sizing:Plant object.");
                ShowContinueError(" For hot water loops, use a heating Sizing:Plant object.");
                ShowContinueError("Occurs in Chiller:Absorption object=" + BLASTAbsorber(ChillNum).Name);
                ErrorsFound = true;
            }
            if (!BLASTAbsorber(ChillNum).GeneratorVolFlowRateWasAutoSized && DataPlant::PlantFinalSizesOkayToReport &&
                (BLASTAbsorber(ChillNum).GeneratorVolFlowRate > 0.0)) {
                ReportSizingManager::ReportSizingOutput(moduleObjectType,
                                   BLASTAbsorber(ChillNum).Name,
                                   "User-Specified Design Generator Fluid Flow Rate [m3/s]",
                                   BLASTAbsorber(ChillNum).GeneratorVolFlowRate);
            }
        }

        // save the design steam or hot water volumetric flow rate for use by the steam or hot water loop sizing algorithms
        if (DataPlant::PlantFirstSizesOkayToFinalize) {
            PlantUtilities::RegisterPlantCompDesignFlow(BLASTAbsorber(ChillNum).GeneratorInletNodeNum, BLASTAbsorber(ChillNum).GeneratorVolFlowRate);
        } else {
            PlantUtilities::RegisterPlantCompDesignFlow(BLASTAbsorber(ChillNum).GeneratorInletNodeNum, tmpGeneratorVolFlowRate);
        }

        if (BLASTAbsorber(ChillNum).GeneratorDeltaTempWasAutoSized) {
            if (PltSizHeatingNum > 0 && BLASTAbsorber(ChillNum).GenHeatSourceType == DataLoopNode::NodeType_Water) {
                BLASTAbsorber(ChillNum).GeneratorDeltaTemp = max(0.5, DataSizing::PlantSizData(PltSizHeatingNum).DeltaT);
            } else if (BLASTAbsorber(ChillNum).GenHeatSourceType == DataLoopNode::NodeType_Water) {
                if (DataPlant::PlantFirstSizesOkayToFinalize) {
                    Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(BLASTAbsorber(ChillNum).GenLoopNum).FluidName,
                                               DataGlobals::HWInitConvTemp,
                                               DataPlant::PlantLoop(BLASTAbsorber(ChillNum).GenLoopNum).FluidIndex,
                                               RoutineName);
                    rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(BLASTAbsorber(ChillNum).GenLoopNum).FluidName,
                                           DataGlobals::HWInitConvTemp,
                                           DataPlant::PlantLoop(BLASTAbsorber(ChillNum).GenLoopNum).FluidIndex,
                                           RoutineName);

                    BLASTAbsorber(ChillNum).GeneratorDeltaTemp =
                        (SteamInputRatNom * BLASTAbsorber(ChillNum).NomCap) / (Cp * rho * BLASTAbsorber(ChillNum).GeneratorVolFlowRate);
                }
            }
        }

        if (ErrorsFound) {
            ShowFatalError("Preceding sizing errors cause program termination");
        }

        if (DataPlant::PlantFinalSizesOkayToReport) {
            // create predefined report
            equipName = BLASTAbsorber(ChillNum).Name;
            OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchMechType, equipName, moduleObjectType);
            OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchMechNomEff, equipName, "n/a");
            OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchMechNomCap, equipName, BLASTAbsorber(ChillNum).NomCap);
        }
    }

    void CalcBLASTAbsorberModel(int &ChillNum,                        // Absorber number
                                Real64 &MyLoad,                       // operating load
                                bool const RunFlag,                   // TRUE when Absorber operating
                                bool const EP_UNUSED(FirstIteration), // TRUE when first iteration of timestep !unused1208
                                int const EquipFlowCtrl               // Flow control mode for the equipment
    )
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

        Array1D<Real64> SteamLoadFactor(3);    // coefficients to poly curve fit
        Array1D<Real64> ElectricLoadFactor(3); // coefficients to poly curve fit
        Real64 MinPartLoadRat;                 // min allowed operating frac full load
        Real64 MaxPartLoadRat;                 // max allowed operating frac full load
        Real64 TempCondIn;                     // C - (BLAST ADJTC(1)The design secondary loop fluid
        Real64 TempCondInDesign;               // C - (BLAST ADJTC(1)The design secondary loop fluid
        Real64 EvapInletTemp;                  // C - evaporator inlet temperature, water side
        Real64 CondInletTemp;                  // C - condenser inlet temperature, water side
        Real64 TempEvapOut;                    // C - evaporator outlet temperature, water side
        Real64 TempEvapOutSetPoint(0.0);       // C - evaporator outlet temperature setpoint
        Real64 AbsorberNomCap;                 // Absorber nominal capacity
        Real64 NomPumpPower;                   // Absorber nominal pumping power
        Real64 PartLoadRat;                    // part load ratio for efficiency calc
        Real64 OperPartLoadRat;                // Operating part load ratio
        Real64 EvapDeltaTemp(0.0);             // C - evaporator temperature difference, water side
        Real64 TempLowLimitEout;               // C - Evaporator low temp. limit cut off
        Real64 SteamInputRat;                  // energy input ratio
        Real64 ElectricInputRat;               // energy input ratio
        int EvapInletNode;                     // evaporator inlet node number, water side
        int EvapOutletNode;                    // evaporator outlet node number, water side
        int CondInletNode;                     // condenser inlet node number, water side
        int CondOutletNode;                    // condenser outlet node number, water side
        int GeneratorInletNode;                // generator inlet node number, steam/water side
        int GeneratorOutletNode;               // generator outlet node number, steam/water side
        Real64 EnthSteamOutDry;                // enthalpy of dry steam at generator inlet
        Real64 EnthSteamOutWet;                // enthalpy of wet steam at generator inlet
        Real64 HfgSteam;                       // heat of vaporization of steam
        static Array1D_bool MyEnvironFlag;
        static Array1D_bool MyEnvironSteamFlag;
        Real64 FRAC;
        Real64 CpFluid; // local specific heat of fluid
        Real64 SteamDeltaT;
        Real64 SteamOutletTemp;
        int LoopNum;
        int LoopSideNum;
        static int DummyWaterIndex(1);

        // set module level inlet and outlet nodes
        EvapMassFlowRate = 0.0;
        CondMassFlowRate = 0.0;
        SteamMassFlowRate = 0.0;
        QCondenser = 0.0;
        QEvaporator = 0.0;
        QGenerator = 0.0;
        PumpingEnergy = 0.0;
        CondenserEnergy = 0.0;
        EvaporatorEnergy = 0.0;
        GeneratorEnergy = 0.0;
        PumpingPower = 0.0;
        FRAC = 1.0;
        EvapInletNode = BLASTAbsorber(ChillNum).EvapInletNodeNum;
        EvapOutletNode = BLASTAbsorber(ChillNum).EvapOutletNodeNum;
        CondInletNode = BLASTAbsorber(ChillNum).CondInletNodeNum;
        CondOutletNode = BLASTAbsorber(ChillNum).CondOutletNodeNum;
        GeneratorInletNode = BLASTAbsorber(ChillNum).GeneratorInletNodeNum;
        GeneratorOutletNode = BLASTAbsorber(ChillNum).GeneratorOutletNodeNum;

        // If no loop demand or Absorber OFF, return
        if (MyLoad >= 0.0 || !RunFlag) { // off or heating
            if (EquipFlowCtrl == DataBranchAirLoopPlant::ControlType_SeriesActive) EvapMassFlowRate = DataLoopNode::Node(EvapInletNode).MassFlowRate;
            return;
        }

        // set module level Absorber inlet and temperature variables
        EvapInletTemp = DataLoopNode::Node(EvapInletNode).Temp;
        CondInletTemp = DataLoopNode::Node(CondInletNode).Temp;

        // Set the condenser mass flow rates
        CondMassFlowRate = DataLoopNode::Node(CondInletNode).MassFlowRate;

        //  LOAD LOCAL VARIABLES FROM DATA STRUCTURE (for code readability)
        SteamLoadFactor = BLASTAbsorber(ChillNum).SteamLoadCoef;
        ElectricLoadFactor = BLASTAbsorber(ChillNum).PumpPowerCoef;
        MinPartLoadRat = BLASTAbsorber(ChillNum).MinPartLoadRat;
        MaxPartLoadRat = BLASTAbsorber(ChillNum).MaxPartLoadRat;
        TempCondInDesign = BLASTAbsorber(ChillNum).TempDesCondIn;
        AbsorberNomCap = BLASTAbsorber(ChillNum).NomCap;
        NomPumpPower = BLASTAbsorber(ChillNum).NomPumpPower;
        TempCondIn = DataLoopNode::Node(BLASTAbsorber(ChillNum).CondInletNodeNum).Temp;
        TempEvapOut = DataLoopNode::Node(BLASTAbsorber(ChillNum).EvapOutletNodeNum).Temp;
        TempLowLimitEout = BLASTAbsorber(ChillNum).TempLowLimitEvapOut;
        LoopNum = BLASTAbsorber(ChillNum).CWLoopNum;
        LoopSideNum = BLASTAbsorber(ChillNum).CWLoopSideNum;

        CpFluid = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(BLASTAbsorber(ChillNum).CWLoopNum).FluidName,
                                        EvapInletTemp,
                                        DataPlant::PlantLoop(BLASTAbsorber(ChillNum).CWLoopNum).FluidIndex,
                                        RoutineName);

        // If there is a fault of Chiller SWT Sensor (zrp_Jun2016)
        if (BLASTAbsorber(ChillNum).FaultyChillerSWTFlag && (!DataGlobals::WarmupFlag) && (!DataGlobals::DoingSizing) && (!DataGlobals::KickOffSimulation)) {
            int FaultIndex = BLASTAbsorber(ChillNum).FaultyChillerSWTIndex;
            Real64 EvapOutletTemp_ff = TempEvapOut;

            // calculate the sensor offset using fault information
            BLASTAbsorber(ChillNum).FaultyChillerSWTOffset = FaultsManager::FaultsChillerSWTSensor(FaultIndex).CalFaultOffsetAct();
            // update the TempEvapOut
            TempEvapOut = max(BLASTAbsorber(ChillNum).TempLowLimitEvapOut,
                              min(DataLoopNode::Node(EvapInletNode).Temp, EvapOutletTemp_ff - BLASTAbsorber(ChillNum).FaultyChillerSWTOffset));
            BLASTAbsorber(ChillNum).FaultyChillerSWTOffset = EvapOutletTemp_ff - TempEvapOut;
        }

        // If FlowLock is True, the new resolved mdot is used to update Power, QEvap, Qcond, and
        // condenser side outlet temperature.
        if (DataPlant::PlantLoop(LoopNum).LoopSide(LoopSideNum).FlowLock == 0) {
            BLASTAbsorber(ChillNum).PossibleSubcooling = false;
            QEvaporator = std::abs(MyLoad);
            // limit by max capacity
            QEvaporator = min(QEvaporator, (BLASTAbsorber(ChillNum).MaxPartLoadRat * BLASTAbsorber(ChillNum).NomCap));

            // Either set the flow to the Constant value or caluclate the flow for the variable volume
            if ((BLASTAbsorber(ChillNum).FlowMode == ConstantFlow) || (BLASTAbsorber(ChillNum).FlowMode == NotModulated)) {
                EvapMassFlowRate = DataLoopNode::Node(EvapInletNode).MassFlowRate;

                if (EvapMassFlowRate != 0.0) {

                    EvapDeltaTemp = QEvaporator / EvapMassFlowRate / CpFluid;
                } else {
                    EvapDeltaTemp = 0.0;
                }
                EvapOutletTemp = EvapInletTemp - EvapDeltaTemp;

            } else if (BLASTAbsorber(ChillNum).FlowMode == LeavingSetPointModulated) {
                // Calculate the Delta Temp from the inlet temp to the chiller outlet setpoint
                {
                    auto const SELECT_CASE_var(DataPlant::PlantLoop(BLASTAbsorber(ChillNum).CWLoopNum).LoopDemandCalcScheme);
                    if (SELECT_CASE_var == DataPlant::SingleSetPoint) {
                        EvapDeltaTemp = DataLoopNode::Node(EvapInletNode).Temp - DataLoopNode::Node(EvapOutletNode).TempSetPoint;
                    } else if (SELECT_CASE_var == DataPlant::DualSetPointDeadBand) {
                        EvapDeltaTemp = DataLoopNode::Node(EvapInletNode).Temp - DataLoopNode::Node(EvapOutletNode).TempSetPointHi;
                    } else {
                        assert(false);
                    }
                }
                if (EvapDeltaTemp != 0) {

                    EvapMassFlowRate = std::abs(QEvaporator / CpFluid / EvapDeltaTemp);
                    if ((EvapMassFlowRate - BLASTAbsorber(ChillNum).EvapMassFlowRateMax) > DataBranchAirLoopPlant::MassFlowTolerance)
                        BLASTAbsorber(ChillNum).PossibleSubcooling = true;
                    // Check to see if the Maximum is exceeded, if so set to maximum
                    EvapMassFlowRate = min(BLASTAbsorber(ChillNum).EvapMassFlowRateMax, EvapMassFlowRate);
                    PlantUtilities::SetComponentFlowRate(EvapMassFlowRate,
                                         BLASTAbsorber(ChillNum).EvapInletNodeNum,
                                         BLASTAbsorber(ChillNum).EvapOutletNodeNum,
                                         BLASTAbsorber(ChillNum).CWLoopNum,
                                         BLASTAbsorber(ChillNum).CWLoopSideNum,
                                         BLASTAbsorber(ChillNum).CWBranchNum,
                                         BLASTAbsorber(ChillNum).CWCompNum);
                    {
                        auto const SELECT_CASE_var(DataPlant::PlantLoop(BLASTAbsorber(ChillNum).CWLoopNum).LoopDemandCalcScheme);
                        if (SELECT_CASE_var == DataPlant::SingleSetPoint) {
                            EvapOutletTemp = DataLoopNode::Node(EvapOutletNode).TempSetPoint;
                        } else if (SELECT_CASE_var == DataPlant::DualSetPointDeadBand) {
                            EvapOutletTemp = DataLoopNode::Node(EvapOutletNode).TempSetPointHi;
                        }
                    }
                } else {
                    EvapMassFlowRate = 0.0;

                    EvapOutletTemp = DataLoopNode::Node(EvapInletNode).Temp;

                    ShowRecurringWarningErrorAtEnd("CalcBLASTAbsorberModel: Name=\"" + BLASTAbsorber(ChillNum).Name +
                                                       "\" Evaporative Condenser Delta Temperature = 0 in mass flow calculation.",
                                                   BLASTAbsorber(ChillNum).ErrCount2);
                }
            } // End of Constant Variable Flow If Block

            // If there is a fault of Chiller SWT Sensor (zrp_Jun2016)
            if (BLASTAbsorber(ChillNum).FaultyChillerSWTFlag && (!DataGlobals::WarmupFlag) && (!DataGlobals::DoingSizing) && (!DataGlobals::KickOffSimulation) && (EvapMassFlowRate > 0)) {
                // calculate directly affected variables at faulty case: EvapOutletTemp, EvapMassFlowRate, QEvaporator
                int FaultIndex = BLASTAbsorber(ChillNum).FaultyChillerSWTIndex;
                bool VarFlowFlag = (BLASTAbsorber(ChillNum).FlowMode == LeavingSetPointModulated);
                FaultsManager::FaultsChillerSWTSensor(FaultIndex)
                    .CalFaultChillerSWT(VarFlowFlag,
                                        BLASTAbsorber(ChillNum).FaultyChillerSWTOffset,
                                        CpFluid,
                                        DataLoopNode::Node(EvapInletNode).Temp,
                                        EvapOutletTemp,
                                        EvapMassFlowRate,
                                        QEvaporator);
                // update corresponding variables at faulty case
                // PartLoadRat = ( AvailChillerCap > 0.0 ) ? ( QEvaporator / AvailChillerCap ) : 0.0;
                // PartLoadRat = max( 0.0, min( PartLoadRat, MaxPartLoadRat ));
                // ChillerPartLoadRatio = PartLoadRat;
                EvapDeltaTemp = DataLoopNode::Node(EvapInletNode).Temp - EvapOutletTemp;
            }

        } else { // If FlowLock is True

            EvapMassFlowRate = DataLoopNode::Node(EvapInletNode).MassFlowRate;
            if (BLASTAbsorber(ChillNum).PossibleSubcooling) {
                QEvaporator = std::abs(MyLoad);
                EvapDeltaTemp = QEvaporator / EvapMassFlowRate / CpFluid;
                EvapOutletTemp = DataLoopNode::Node(EvapInletNode).Temp - EvapDeltaTemp;
            } else {
                {
                    auto const SELECT_CASE_var(DataPlant::PlantLoop(LoopNum).LoopDemandCalcScheme);
                    if (SELECT_CASE_var == DataPlant::SingleSetPoint) {
                        if ((BLASTAbsorber(ChillNum).FlowMode == LeavingSetPointModulated) ||
                            (DataPlant::PlantLoop(LoopNum)
                                 .LoopSide(LoopSideNum)
                                 .Branch(BLASTAbsorber(ChillNum).CWBranchNum)
                                 .Comp(BLASTAbsorber(ChillNum).CWCompNum)
                                 .CurOpSchemeType == DataPlant::CompSetPtBasedSchemeType) ||
                            (DataLoopNode::Node(EvapOutletNode).TempSetPoint != DataLoopNode::SensedNodeFlagValue)) {
                            TempEvapOutSetPoint = DataLoopNode::Node(EvapOutletNode).TempSetPoint;
                        } else {
                            TempEvapOutSetPoint = DataLoopNode::Node(DataPlant::PlantLoop(LoopNum).TempSetPointNodeNum).TempSetPoint;
                        }
                    } else if (SELECT_CASE_var == DataPlant::DualSetPointDeadBand) {
                        if ((BLASTAbsorber(ChillNum).FlowMode == LeavingSetPointModulated) ||
                            (DataPlant::PlantLoop(LoopNum)
                                 .LoopSide(LoopSideNum)
                                 .Branch(BLASTAbsorber(ChillNum).CWBranchNum)
                                 .Comp(BLASTAbsorber(ChillNum).CWCompNum)
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
                QEvaporator = std::abs(EvapMassFlowRate * CpFluid * EvapDeltaTemp);
                EvapOutletTemp = TempEvapOutSetPoint;
            }
            // Check that the Evap outlet temp honors both plant loop temp low limit and also the chiller low limit
            if (EvapOutletTemp < TempLowLimitEout) {
                if ((DataLoopNode::Node(EvapInletNode).Temp - TempLowLimitEout) > DataPlant::DeltaTempTol) {
                    EvapOutletTemp = TempLowLimitEout;
                    EvapDeltaTemp = DataLoopNode::Node(EvapInletNode).Temp - EvapOutletTemp;
                    QEvaporator = EvapMassFlowRate * CpFluid * EvapDeltaTemp;
                } else {
                    EvapOutletTemp = DataLoopNode::Node(EvapInletNode).Temp;
                    EvapDeltaTemp = DataLoopNode::Node(EvapInletNode).Temp - EvapOutletTemp;
                    QEvaporator = EvapMassFlowRate * CpFluid * EvapDeltaTemp;
                }
            }
            if (EvapOutletTemp < DataLoopNode::Node(EvapOutletNode).TempMin) {
                if ((DataLoopNode::Node(EvapInletNode).Temp - DataLoopNode::Node(EvapOutletNode).TempMin) > DataPlant::DeltaTempTol) {
                    EvapOutletTemp = DataLoopNode::Node(EvapOutletNode).TempMin;
                    EvapDeltaTemp = DataLoopNode::Node(EvapInletNode).Temp - EvapOutletTemp;
                    QEvaporator = EvapMassFlowRate * CpFluid * EvapDeltaTemp;
                } else {
                    EvapOutletTemp = DataLoopNode::Node(EvapInletNode).Temp;
                    EvapDeltaTemp = DataLoopNode::Node(EvapInletNode).Temp - EvapOutletTemp;
                    QEvaporator = EvapMassFlowRate * CpFluid * EvapDeltaTemp;
                }
            }

            // Checks QEvaporator on the basis of the machine limits.
            if (QEvaporator > std::abs(MyLoad)) {
                if (EvapMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance) {
                    QEvaporator = std::abs(MyLoad);
                    EvapDeltaTemp = QEvaporator / EvapMassFlowRate / CpFluid;
                    EvapOutletTemp = DataLoopNode::Node(EvapInletNode).Temp - EvapDeltaTemp;
                } else {
                    QEvaporator = 0.0;
                    EvapOutletTemp = DataLoopNode::Node(EvapInletNode).Temp;
                }
            }

            // If there is a fault of Chiller SWT Sensor (zrp_Jun2016)
            if (BLASTAbsorber(ChillNum).FaultyChillerSWTFlag && (!DataGlobals::WarmupFlag) && (!DataGlobals::DoingSizing) && (!DataGlobals::KickOffSimulation) && (EvapMassFlowRate > 0)) {
                // calculate directly affected variables at faulty case: EvapOutletTemp, EvapMassFlowRate, QEvaporator
                int FaultIndex = BLASTAbsorber(ChillNum).FaultyChillerSWTIndex;
                bool VarFlowFlag = false;
                FaultsManager::FaultsChillerSWTSensor(FaultIndex)
                    .CalFaultChillerSWT(VarFlowFlag,
                                        BLASTAbsorber(ChillNum).FaultyChillerSWTOffset,
                                        CpFluid,
                                        DataLoopNode::Node(EvapInletNode).Temp,
                                        EvapOutletTemp,
                                        EvapMassFlowRate,
                                        QEvaporator);
                // update corresponding variables at faulty case
                EvapDeltaTemp = DataLoopNode::Node(EvapInletNode).Temp - EvapOutletTemp;
            }

        } // This is the end of the FlowLock Block

        // Calculate part load ratio for efficiency calcs. If this part load ratio is greater than
        // Min PLR it will be used for calculations too.
        PartLoadRat = max(MinPartLoadRat, min(QEvaporator / AbsorberNomCap, MaxPartLoadRat));

        // In case MyLoad is less than the Min PLR load, the power and steam input should be adjusted
        // for cycling. The ratios used however are based on MinPLR.
        OperPartLoadRat = QEvaporator / AbsorberNomCap;

        if (OperPartLoadRat < PartLoadRat) {
            FRAC = min(1.0, OperPartLoadRat / MinPartLoadRat);
        } else {
            FRAC = 1.0;
        }

        // Calculate steam input ratio
        SteamInputRat = SteamLoadFactor(1) / PartLoadRat + SteamLoadFactor(2) + SteamLoadFactor(3) * PartLoadRat;

        // Calculate electric input ratio
        ElectricInputRat = ElectricLoadFactor(1) + ElectricLoadFactor(2) * PartLoadRat + ElectricLoadFactor(3) * pow_2(PartLoadRat);

        // Calculate electric energy input
        PumpingPower = ElectricInputRat * NomPumpPower * FRAC;

        // Calculate steam load
        QGenerator = SteamInputRat * QEvaporator * FRAC;

        if (EvapMassFlowRate == 0.0) {
            QGenerator = 0.0;
            EvapOutletTemp = DataLoopNode::Node(EvapInletNode).Temp;
            PumpingPower = 0.0;
        }

        QCondenser = QEvaporator + QGenerator + PumpingPower;

        CpFluid = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(BLASTAbsorber(ChillNum).CDLoopNum).FluidName,
                                        CondInletTemp,
                                        DataPlant::PlantLoop(BLASTAbsorber(ChillNum).CDLoopNum).FluidIndex,
                                        RoutineName);

        if (CondMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance) {
            CondOutletTemp = QCondenser / CondMassFlowRate / CpFluid + CondInletTemp;
        } else {

            CondOutletTemp = CondInletTemp;
            CondMassFlowRate = 0.0;
            QCondenser = 0.0;
            return;
            // V7 plant upgrade, no longer fatal here anymore, set some things and return
        }

        if (GeneratorInletNode > 0) {
            if (BLASTAbsorber(ChillNum).GenHeatSourceType == DataLoopNode::NodeType_Water) {
                Real64 GenMassFlowRate = 0.0;
                int GenLoopNum = BLASTAbsorber(ChillNum).GenLoopNum;
                int GenLoopSideNum = BLASTAbsorber(ChillNum).GenLoopSideNum;
                //  Hot water plant is used for the generator
                CpFluid = FluidProperties::GetSpecificHeatGlycol(
                    DataPlant::PlantLoop(GenLoopNum).FluidName, DataLoopNode::Node(GeneratorInletNode).Temp, DataPlant::PlantLoop(GenLoopSideNum).FluidIndex, RoutineName);
                if (DataPlant::PlantLoop(GenLoopNum).LoopSide(GenLoopSideNum).FlowLock == 0) {
                    if ((BLASTAbsorber(ChillNum).FlowMode == ConstantFlow) || (BLASTAbsorber(ChillNum).FlowMode == NotModulated)) {
                        GenMassFlowRate = BLASTAbsorber(ChillNum).GenMassFlowRateMax;
                    } else { // LeavingSetpointModulated
                        // since the .FlowMode applies to the chiller evaporator, the generater mass flow rate will be proportional to the evaporator
                        // mass flow rate
                        Real64 GenFlowRatio = EvapMassFlowRate / BLASTAbsorber(ChillNum).EvapMassFlowRateMax;
                        GenMassFlowRate = min(BLASTAbsorber(ChillNum).GenMassFlowRateMax, GenFlowRatio * BLASTAbsorber(ChillNum).GenMassFlowRateMax);
                    }
                } else { // If FlowLock is True
                    GenMassFlowRate = DataLoopNode::Node(GeneratorInletNode).MassFlowRate;
                }
                PlantUtilities::SetComponentFlowRate(GenMassFlowRate,
                                     GeneratorInletNode,
                                     GeneratorOutletNode,
                                     GenLoopNum,
                                     GenLoopSideNum,
                                     BLASTAbsorber(ChillNum).GenBranchNum,
                                     BLASTAbsorber(ChillNum).GenCompNum);

                if (GenMassFlowRate <= 0.0) {
                    GenOutletTemp = DataLoopNode::Node(GeneratorInletNode).Temp;
                    SteamOutletEnthalpy = DataLoopNode::Node(GeneratorInletNode).Enthalpy;
                } else {
                    GenOutletTemp = DataLoopNode::Node(GeneratorInletNode).Temp - QGenerator / (CpFluid * GenMassFlowRate);
                    SteamOutletEnthalpy = DataLoopNode::Node(GeneratorInletNode).Enthalpy - QGenerator / GenMassFlowRate;
                }
                DataLoopNode::Node(GeneratorOutletNode).Temp = GenOutletTemp;
                DataLoopNode::Node(GeneratorOutletNode).Enthalpy = SteamOutletEnthalpy;
                DataLoopNode::Node(GeneratorOutletNode).MassFlowRate = GenMassFlowRate;

            } else { // using a steam plant for the generator

                EnthSteamOutDry = FluidProperties::GetSatEnthalpyRefrig(fluidNameSteam,
                                                       DataLoopNode::Node(GeneratorInletNode).Temp,
                                                       1.0,
                                                       BLASTAbsorber(ChillNum).SteamFluidIndex,
                                                       calcChillerAbsorption + BLASTAbsorber(ChillNum).Name);
                EnthSteamOutWet = FluidProperties::GetSatEnthalpyRefrig(fluidNameSteam,
                                                       DataLoopNode::Node(GeneratorInletNode).Temp,
                                                       0.0,
                                                       BLASTAbsorber(ChillNum).SteamFluidIndex,
                                                       calcChillerAbsorption + BLASTAbsorber(ChillNum).Name);
                SteamDeltaT = BLASTAbsorber(ChillNum).GeneratorSubcool;
                SteamOutletTemp = DataLoopNode::Node(GeneratorInletNode).Temp - SteamDeltaT;
                HfgSteam = EnthSteamOutDry - EnthSteamOutWet;
                CpFluid =
                    FluidProperties::GetSpecificHeatGlycol(fluidNameWater, SteamOutletTemp, DummyWaterIndex, calcChillerAbsorption + BLASTAbsorber(ChillNum).Name);
                SteamMassFlowRate = QGenerator / (HfgSteam + CpFluid * SteamDeltaT);
                PlantUtilities::SetComponentFlowRate(SteamMassFlowRate,
                                     GeneratorInletNode,
                                     GeneratorOutletNode,
                                     BLASTAbsorber(ChillNum).GenLoopNum,
                                     BLASTAbsorber(ChillNum).GenLoopSideNum,
                                     BLASTAbsorber(ChillNum).GenBranchNum,
                                     BLASTAbsorber(ChillNum).GenCompNum);

                if (SteamMassFlowRate <= 0.0) {
                    GenOutletTemp = DataLoopNode::Node(GeneratorInletNode).Temp;
                    SteamOutletEnthalpy = DataLoopNode::Node(GeneratorInletNode).Enthalpy;
                } else {
                    GenOutletTemp = DataLoopNode::Node(GeneratorInletNode).Temp - SteamDeltaT;
                    SteamOutletEnthalpy = FluidProperties::GetSatEnthalpyRefrig(
                        fluidNameSteam, GenOutletTemp, 0.0, BLASTAbsorber(ChillNum).SteamFluidIndex, moduleObjectType + BLASTAbsorber(ChillNum).Name);
                    SteamOutletEnthalpy -= CpFluid * SteamDeltaT;
                }
            }
        } // IF(GeneratorInletNode .GT. 0)THEN

        // convert power to energy
        GeneratorEnergy = QGenerator * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
        EvaporatorEnergy = QEvaporator * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
        CondenserEnergy = QCondenser * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
        PumpingEnergy = PumpingPower * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
    }

    void UpdateBLASTAbsorberRecords(Real64 const MyLoad, // current load
                                    bool const RunFlag,  // TRUE if Absorber operating
                                    int const Num        // Absorber number
    )
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Dan Fisher
        //       DATE WRITTEN:    October 1998

        // PURPOSE OF THIS SUBROUTINE:
        // reporting

        int EvapInletNode;       // evaporator inlet node number, water side
        int EvapOutletNode;      // evaporator outlet node number, water side
        int CondInletNode;       // condenser inlet node number, water side
        int CondOutletNode;      // condenser outlet node number, water side
        int GeneratorInletNode;  // generator inlet node number, steam/water side
        int GeneratorOutletNode; // generator outlet node number, steam/water side

        EvapInletNode = BLASTAbsorber(Num).EvapInletNodeNum;
        EvapOutletNode = BLASTAbsorber(Num).EvapOutletNodeNum;
        CondInletNode = BLASTAbsorber(Num).CondInletNodeNum;
        CondOutletNode = BLASTAbsorber(Num).CondOutletNodeNum;
        GeneratorInletNode = BLASTAbsorber(Num).GeneratorInletNodeNum;
        GeneratorOutletNode = BLASTAbsorber(Num).GeneratorOutletNodeNum;

        if (MyLoad >= 0 || !RunFlag) {
            // set node conditions
            PlantUtilities::SafeCopyPlantNode(EvapInletNode, EvapOutletNode);
            PlantUtilities::SafeCopyPlantNode(CondInletNode, CondOutletNode);

            BLASTAbsorberReport(Num).PumpingPower = 0.0;
            BLASTAbsorberReport(Num).QEvap = 0.0;
            BLASTAbsorberReport(Num).QCond = 0.0;
            BLASTAbsorberReport(Num).QGenerator = 0.0;
            BLASTAbsorberReport(Num).PumpingEnergy = 0.0;
            BLASTAbsorberReport(Num).EvapEnergy = 0.0;
            BLASTAbsorberReport(Num).CondEnergy = 0.0;
            BLASTAbsorberReport(Num).GeneratorEnergy = 0.0;
            BLASTAbsorberReport(Num).EvapInletTemp = DataLoopNode::Node(EvapInletNode).Temp;
            BLASTAbsorberReport(Num).CondInletTemp = DataLoopNode::Node(CondInletNode).Temp;
            BLASTAbsorberReport(Num).CondOutletTemp = DataLoopNode::Node(CondOutletNode).Temp;
            BLASTAbsorberReport(Num).EvapOutletTemp = DataLoopNode::Node(EvapOutletNode).Temp;
            BLASTAbsorberReport(Num).Evapmdot = 0.0;
            BLASTAbsorberReport(Num).Condmdot = 0.0;
            BLASTAbsorberReport(Num).Genmdot = 0.0;
            BLASTAbsorberReport(Num).ActualCOP = 0.0;

            if (GeneratorInletNode > 0) {
                PlantUtilities::SafeCopyPlantNode(GeneratorInletNode, GeneratorOutletNode);
            }

        } else {
            // set node conditions
            PlantUtilities::SafeCopyPlantNode(EvapInletNode, EvapOutletNode);
            PlantUtilities::SafeCopyPlantNode(CondInletNode, CondOutletNode);
            DataLoopNode::Node(EvapOutletNode).Temp = EvapOutletTemp;
            DataLoopNode::Node(CondOutletNode).Temp = CondOutletTemp;

            BLASTAbsorberReport(Num).PumpingPower = PumpingPower;
            BLASTAbsorberReport(Num).QEvap = QEvaporator;
            BLASTAbsorberReport(Num).QCond = QCondenser;
            BLASTAbsorberReport(Num).QGenerator = QGenerator;
            BLASTAbsorberReport(Num).PumpingEnergy = PumpingEnergy;
            BLASTAbsorberReport(Num).EvapEnergy = EvaporatorEnergy;
            BLASTAbsorberReport(Num).CondEnergy = CondenserEnergy;
            BLASTAbsorberReport(Num).GeneratorEnergy = GeneratorEnergy;
            BLASTAbsorberReport(Num).EvapInletTemp = DataLoopNode::Node(EvapInletNode).Temp;
            BLASTAbsorberReport(Num).CondInletTemp = DataLoopNode::Node(CondInletNode).Temp;
            BLASTAbsorberReport(Num).CondOutletTemp = DataLoopNode::Node(CondOutletNode).Temp;
            BLASTAbsorberReport(Num).EvapOutletTemp = DataLoopNode::Node(EvapOutletNode).Temp;
            BLASTAbsorberReport(Num).Evapmdot = EvapMassFlowRate;
            BLASTAbsorberReport(Num).Condmdot = CondMassFlowRate;
            BLASTAbsorberReport(Num).Genmdot = SteamMassFlowRate;
            if (QGenerator != 0.0) {
                BLASTAbsorberReport(Num).ActualCOP = QEvaporator / QGenerator;
            } else {
                BLASTAbsorberReport(Num).ActualCOP = 0.0;
            }

            if (GeneratorInletNode > 0) {
                PlantUtilities::SafeCopyPlantNode(GeneratorInletNode, GeneratorOutletNode);
                DataLoopNode::Node(GeneratorOutletNode).Temp = GenOutletTemp;
            }
        }
    }

} // namespace ChillerAbsorption

} // namespace EnergyPlus
