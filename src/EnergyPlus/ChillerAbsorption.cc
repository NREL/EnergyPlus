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
#include <EnergyPlus/DataPrecisionGlobals.hh>
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

    // Using/Aliasing
    using namespace DataPrecisionGlobals;
    using namespace DataLoopNode;
    // using DataGlobals::DisplayExtraWarnings;
    // using DataHVACGlobals::SmallWaterVolFlow;
    // using General::RoundSigDigits;
    // using General::TrimSigDigits;

    // Data
    // MODULE PARAMETER DEFINITIONS:
    // chiller flow modes
    int const FlowModeNotSet(200);
    int const ConstantFlow(201);
    int const NotModulated(202);
    int const LeavingSetPointModulated(203);

    // MODULE VARIABLE DECLARATIONS:
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

    // SUBROUTINE SPECIFICATIONS FOR MODULE:

    // Object Data
    Array1D<BLASTAbsorberSpecs> BLASTAbsorber; // dimension to number of machines
    Array1D<ReportVars> BLASTAbsorberReport;

    // Functions
    void BLASTAbsorberSpecs::simulate(const PlantLocation &calledFromLocation, bool FirstHVACIteration, Real64 &CurLoad, bool RunFlag)
    {
    }

    void
    BLASTAbsorberSpecs::getDesignCapacities(const PlantLocation &EP_UNUSED(calledFromLocation), Real64 &MaxLoad, Real64 &MinLoad, Real64 &OptLoad)
    {
    }

    void BLASTAbsorberSpecs::getSizingFactor(Real64 &SizFac)
    {
    }

    void BLASTAbsorberSpecs::onInitLoopEquip(const PlantLocation &EP_UNUSED(calledFromLocation))
    {
    }


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

        // Using/Aliasing
        // using DataPlant::TypeOf_Chiller_Absorption;
        // using PlantUtilities::UpdateAbsorberChillerComponentGeneratorSide;
        // using PlantUtilities::UpdateChillerComponentCondenserSide;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
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

        auto &thisChillerAbsorption = BLASTAbsorber(ChillNum);

        // Initialize Loop Equipment
        if (InitLoopEquip) {

            TempCondInDesign = BLASTAbsorber(ChillNum).TempDesCondIn;

            thisChillerAbsorption.InitBLASTAbsorberModel(ChillNum, RunFlag, MyLoad);

            if (LoopNum == thisChillerAbsorption.CWLoopNum) {
                thisChillerAbsorption.SizeAbsorpChiller(ChillNum);
                MinCap = thisChillerAbsorption.NomCap * thisChillerAbsorption.MinPartLoadRat;
                MaxCap = thisChillerAbsorption.NomCap * thisChillerAbsorption.MaxPartLoadRat;
                OptCap = thisChillerAbsorption.NomCap * thisChillerAbsorption.OptPartLoadRat;
            } else {
                MinCap = 0.0;
                MaxCap = 0.0;
                OptCap = 0.0;
            }
            if (GetSizingFactor) {
                SizingFactor = thisChillerAbsorption.SizFac;
            }
            return;
        }

        // different actions depending on which loop the component was called from

        if (LoopNum == thisChillerAbsorption.CWLoopNum) {
            // called from dominant chilled water connection loop side
            // Calculate Load
            thisChillerAbsorption.InitBLASTAbsorberModel(ChillNum, RunFlag, MyLoad);
            thisChillerAbsorption.CalcBLASTAbsorberModel(ChillNum, MyLoad, RunFlag, FirstIteration, EquipFlowCtrl);
            UpdateBLASTAbsorberRecords(MyLoad, RunFlag, ChillNum);

        } else if (LoopNum == thisChillerAbsorption.CDLoopNum) {
            // Called from non-dominant condenser water connection loop side
            PlantUtilities::UpdateChillerComponentCondenserSide(LoopNum,
                                                                LoopSide,
                                                                DataPlant::TypeOf_Chiller_Absorption,
                                                                thisChillerAbsorption.CondInletNodeNum,
                                                                thisChillerAbsorption.CondOutletNodeNum,
                                                                BLASTAbsorberReport(ChillNum).QCond,
                                                                BLASTAbsorberReport(ChillNum).CondInletTemp,
                                                                BLASTAbsorberReport(ChillNum).CondOutletTemp,
                                                                BLASTAbsorberReport(ChillNum).Condmdot,
                                                                FirstIteration);

        } else if (LoopNum == thisChillerAbsorption.GenLoopNum) {
            // Called from non-dominant generator hot water or steam connection loop side
            PlantUtilities::UpdateAbsorberChillerComponentGeneratorSide(LoopNum,
                                                                        LoopSide,
                                                                        DataPlant::TypeOf_Chiller_Absorption,
                                                                        thisChillerAbsorption.GeneratorInletNodeNum,
                                                                        thisChillerAbsorption.GeneratorOutletNodeNum,
                                                                        thisChillerAbsorption.GenHeatSourceType,
                                                                        BLASTAbsorberReport(ChillNum).QGenerator,
                                                                        BLASTAbsorberReport(ChillNum).SteamMdot,
                                                                        FirstIteration);

        } else {
            ShowFatalError("SimBLASTAbsorber: Invalid LoopNum passed=" + General::TrimSigDigits(LoopNum) + ", Unit name=" + AbsorberName +
                           ", stored chilled water loop=" + General::TrimSigDigits(thisChillerAbsorption.CWLoopNum) +
                           ", stored condenser water loop=" + General::TrimSigDigits(thisChillerAbsorption.CDLoopNum) +
                           ", stored generator loop=" + General::TrimSigDigits(thisChillerAbsorption.GenLoopNum));
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

        // Using/Aliasing
        using namespace DataIPShortCuts; // Data for field names, blank numerics
        // using BranchNodeConnections::TestCompSet;
        // using GlobalNames::VerifyUniqueChillerName;
        // using NodeInputManager::GetOnlySingleNode;
        using namespace OutputReportPredefined;
        // using DataGlobals::AnyEnergyManagementSystemInModel;
        // using DataSizing::AutoSize;
        // using FluidProperties::FindRefrigerant;
        // using General::RoundSigDigits;

        // Locals
        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("GetBLASTAbsorberInput: "); // include trailing blank space

        // LOCAL VARIABLES
        int AbsorberNum;                      // Absorber counter
        int NumAlphas;                        // Number of elements in the alpha array
        int NumNums;                          // Number of elements in the numeric array
        int IOStat;                           // IO Status when calling get input subroutine
        Array1D_bool GenInputOutputNodesUsed; // Used for SetupOutputVariable
        static bool ErrorsFound(false);
        //  CHARACTER(len=MaxNameLength) :: CurrentModuleObject  ! for ease in renaming.

        // FLOW
        cCurrentModuleObject = moduleObjectType;

        NumBLASTAbsorbers = inputProcessor->getNumObjectsFound(cCurrentModuleObject);

        if (NumBLASTAbsorbers <= 0) {
            ShowSevereError("No " + cCurrentModuleObject + " equipment specified in input file");
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
            inputProcessor->getObjectItem(cCurrentModuleObject,
                                          AbsorberNum,
                                          cAlphaArgs,
                                          NumAlphas,
                                          rNumericArgs,
                                          NumNums,
                                          IOStat,
                                          _,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            UtilityRoutines::IsNameEmpty(cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

            // ErrorsFound will be set to True if problem was found, left untouched otherwise
            GlobalNames::VerifyUniqueChillerName(cCurrentModuleObject, cAlphaArgs(1), ErrorsFound, cCurrentModuleObject + " Name");

            BLASTAbsorber(AbsorberNum).Name = cAlphaArgs(1);
            BLASTAbsorber(AbsorberNum).NomCap = rNumericArgs(1);
            if (BLASTAbsorber(AbsorberNum).NomCap == DataSizing::AutoSize) {
                BLASTAbsorber(AbsorberNum).NomCapWasAutoSized = true;
            }
            BLASTAbsorber(AbsorberNum).NomPumpPower = rNumericArgs(2);
            if (BLASTAbsorber(AbsorberNum).NomPumpPower == DataSizing::AutoSize) {
                BLASTAbsorber(AbsorberNum).NomPumpPowerWasAutoSized = true;
            }
            if (rNumericArgs(1) == 0.0) {
                ShowSevereError("Invalid " + cNumericFieldNames(1) + '=' + General::RoundSigDigits(rNumericArgs(1), 2));
                ShowContinueError("Entered in " + cCurrentModuleObject + '=' + cAlphaArgs(1));
                ErrorsFound = true;
            }
            // Assign Node Numbers to specified nodes
            BLASTAbsorber(AbsorberNum).EvapInletNodeNum = NodeInputManager::GetOnlySingleNode(
                cAlphaArgs(2), ErrorsFound, cCurrentModuleObject, cAlphaArgs(1), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent);
            BLASTAbsorber(AbsorberNum).EvapOutletNodeNum = NodeInputManager::GetOnlySingleNode(
                cAlphaArgs(3), ErrorsFound, cCurrentModuleObject, cAlphaArgs(1), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent);
            BranchNodeConnections::TestCompSet(cCurrentModuleObject, cAlphaArgs(1), cAlphaArgs(2), cAlphaArgs(3), "Chilled Water Nodes");

            BLASTAbsorber(AbsorberNum).CondInletNodeNum = NodeInputManager::GetOnlySingleNode(
                cAlphaArgs(4), ErrorsFound, cCurrentModuleObject, cAlphaArgs(1), NodeType_Water, NodeConnectionType_Inlet, 2, ObjectIsNotParent);
            BLASTAbsorber(AbsorberNum).CondOutletNodeNum = NodeInputManager::GetOnlySingleNode(
                cAlphaArgs(5), ErrorsFound, cCurrentModuleObject, cAlphaArgs(1), NodeType_Water, NodeConnectionType_Outlet, 2, ObjectIsNotParent);
            BranchNodeConnections::TestCompSet(cCurrentModuleObject, cAlphaArgs(1), cAlphaArgs(4), cAlphaArgs(5), "Condenser (not tested) Nodes");

            if (NumAlphas > 8) {
                if (UtilityRoutines::SameString(cAlphaArgs(9), "HotWater") || UtilityRoutines::SameString(cAlphaArgs(9), "HotWater")) {
                    BLASTAbsorber(AbsorberNum).GenHeatSourceType = NodeType_Water;
                } else if (UtilityRoutines::SameString(cAlphaArgs(9), "Steam") || cAlphaArgs(9).empty()) {
                    BLASTAbsorber(AbsorberNum).GenHeatSourceType = NodeType_Steam;
                } else {
                    ShowSevereError("Invalid " + cAlphaFieldNames(9) + '=' + cAlphaArgs(9));
                    ShowContinueError("Entered in " + cCurrentModuleObject + '=' + cAlphaArgs(1));
                    ShowContinueError("...Generator heat source type must be Steam or Hot Water.");
                    ErrorsFound = true;
                }
            } else {
                BLASTAbsorber(AbsorberNum).GenHeatSourceType = NodeType_Steam;
            }

            if (!lAlphaFieldBlanks(6) && !lAlphaFieldBlanks(7)) {
                GenInputOutputNodesUsed(AbsorberNum) = true;
                if (BLASTAbsorber(AbsorberNum).GenHeatSourceType == NodeType_Water) {
                    BLASTAbsorber(AbsorberNum).GeneratorInletNodeNum = NodeInputManager::GetOnlySingleNode(cAlphaArgs(6),
                                                                                                           ErrorsFound,
                                                                                                           cCurrentModuleObject,
                                                                                                           cAlphaArgs(1),
                                                                                                           NodeType_Water,
                                                                                                           NodeConnectionType_Inlet,
                                                                                                           3,
                                                                                                           ObjectIsNotParent);
                    BLASTAbsorber(AbsorberNum).GeneratorOutletNodeNum = NodeInputManager::GetOnlySingleNode(cAlphaArgs(7),
                                                                                                            ErrorsFound,
                                                                                                            cCurrentModuleObject,
                                                                                                            cAlphaArgs(1),
                                                                                                            NodeType_Water,
                                                                                                            NodeConnectionType_Outlet,
                                                                                                            3,
                                                                                                            ObjectIsNotParent);
                    BranchNodeConnections::TestCompSet(cCurrentModuleObject, cAlphaArgs(1), cAlphaArgs(6), cAlphaArgs(7), "Hot Water Nodes");
                } else {
                    BLASTAbsorber(AbsorberNum).SteamFluidIndex = FluidProperties::FindRefrigerant("STEAM");
                    BLASTAbsorber(AbsorberNum).GeneratorInletNodeNum = NodeInputManager::GetOnlySingleNode(cAlphaArgs(6),
                                                                                                           ErrorsFound,
                                                                                                           cCurrentModuleObject,
                                                                                                           cAlphaArgs(1),
                                                                                                           NodeType_Steam,
                                                                                                           NodeConnectionType_Inlet,
                                                                                                           3,
                                                                                                           ObjectIsNotParent);
                    BLASTAbsorber(AbsorberNum).GeneratorOutletNodeNum = NodeInputManager::GetOnlySingleNode(cAlphaArgs(7),
                                                                                                            ErrorsFound,
                                                                                                            cCurrentModuleObject,
                                                                                                            cAlphaArgs(1),
                                                                                                            NodeType_Steam,
                                                                                                            NodeConnectionType_Outlet,
                                                                                                            3,
                                                                                                            ObjectIsNotParent);
                    BranchNodeConnections::TestCompSet(cCurrentModuleObject, cAlphaArgs(1), cAlphaArgs(6), cAlphaArgs(7), "Steam Nodes");
                }
            } else if ((lAlphaFieldBlanks(6) && !lAlphaFieldBlanks(7)) || (!lAlphaFieldBlanks(6) && lAlphaFieldBlanks(7))) {
                ShowSevereError(cCurrentModuleObject + ", Name=" + cAlphaArgs(1));
                ShowContinueError("...Generator fluid nodes must both be entered (or both left blank).");
                ShowContinueError("..." + cAlphaFieldNames(6) + " = " + cAlphaArgs(6));
                ShowContinueError("..." + cAlphaFieldNames(7) + " = " + cAlphaArgs(7));
                ErrorsFound = true;
            } else {
                if (BLASTAbsorber(AbsorberNum).GenHeatSourceType == NodeType_Water) {
                    ShowWarningError(cCurrentModuleObject + ", Name=" + cAlphaArgs(1));
                    ShowContinueError("...Generator fluid type must be Steam if generator inlet/outlet nodes are blank.");
                    ShowContinueError("...Generator fluid type is set to Steam and the simulation continues.");
                    BLASTAbsorber(AbsorberNum).GenHeatSourceType = NodeType_Steam;
                }
            }

            // Get remaining data
            BLASTAbsorber(AbsorberNum).MinPartLoadRat = rNumericArgs(3);
            BLASTAbsorber(AbsorberNum).MaxPartLoadRat = rNumericArgs(4);
            BLASTAbsorber(AbsorberNum).OptPartLoadRat = rNumericArgs(5);
            BLASTAbsorber(AbsorberNum).TempDesCondIn = rNumericArgs(6);
            BLASTAbsorber(AbsorberNum).EvapVolFlowRate = rNumericArgs(7);
            if (BLASTAbsorber(AbsorberNum).EvapVolFlowRate == DataSizing::AutoSize) {
                BLASTAbsorber(AbsorberNum).EvapVolFlowRateWasAutoSized = true;
            }
            BLASTAbsorber(AbsorberNum).CondVolFlowRate = rNumericArgs(8);
            if (BLASTAbsorber(AbsorberNum).CondVolFlowRate == DataSizing::AutoSize) {
                BLASTAbsorber(AbsorberNum).CondVolFlowRateWasAutoSized = true;
            }
            BLASTAbsorber(AbsorberNum).SteamLoadCoef(1) = rNumericArgs(9);
            BLASTAbsorber(AbsorberNum).SteamLoadCoef(2) = rNumericArgs(10);
            BLASTAbsorber(AbsorberNum).SteamLoadCoef(3) = rNumericArgs(11);
            BLASTAbsorber(AbsorberNum).PumpPowerCoef(1) = rNumericArgs(12);
            BLASTAbsorber(AbsorberNum).PumpPowerCoef(2) = rNumericArgs(13);
            BLASTAbsorber(AbsorberNum).PumpPowerCoef(3) = rNumericArgs(14);
            BLASTAbsorber(AbsorberNum).TempLowLimitEvapOut = rNumericArgs(15);

            {
                auto const SELECT_CASE_var(cAlphaArgs(8));
                if (SELECT_CASE_var == "CONSTANTFLOW") {
                    BLASTAbsorber(AbsorberNum).FlowMode = ConstantFlow;
                } else if (SELECT_CASE_var == "VARIABLEFLOW") {
                    BLASTAbsorber(AbsorberNum).FlowMode = LeavingSetPointModulated;
                    ShowWarningError(RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\",");
                    ShowContinueError("Invalid " + cAlphaFieldNames(8) + '=' + cAlphaArgs(8));
                    ShowContinueError("Key choice is now called \"LeavingSetpointModulated\" and the simulation continues");
                } else if (SELECT_CASE_var == "LEAVINGSETPOINTMODULATED") {
                    BLASTAbsorber(AbsorberNum).FlowMode = LeavingSetPointModulated;
                } else if (SELECT_CASE_var == "NOTMODULATED") {
                    BLASTAbsorber(AbsorberNum).FlowMode = NotModulated;
                } else {
                    ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\",");
                    ShowContinueError("Invalid " + cAlphaFieldNames(8) + '=' + cAlphaArgs(8));
                    ShowContinueError("Available choices are ConstantFlow, NotModulated, or LeavingSetpointModulated");
                    ShowContinueError("Flow mode NotModulated is assumed and the simulation continues.");
                    BLASTAbsorber(AbsorberNum).FlowMode = NotModulated;
                }
            }

            if (NumNums > 15) {
                BLASTAbsorber(AbsorberNum).GeneratorVolFlowRate = rNumericArgs(16);
                if (BLASTAbsorber(AbsorberNum).GeneratorVolFlowRate == DataSizing::AutoSize) {
                    BLASTAbsorber(AbsorberNum).GeneratorVolFlowRateWasAutoSized = true;
                }
            }

            if (BLASTAbsorber(AbsorberNum).GeneratorVolFlowRate == 0.0 && BLASTAbsorber(AbsorberNum).GenHeatSourceType == NodeType_Water) {
                ShowSevereError("Invalid " + cNumericFieldNames(16) + '=' + General::RoundSigDigits(rNumericArgs(16), 2));
                ShowContinueError("Entered in " + cCurrentModuleObject + '=' + cAlphaArgs(1));
                ShowContinueError("...Generator water flow rate must be greater than 0 when absorber generator fluid type is hot water.");
                ErrorsFound = true;
            }

            if (NumNums > 16) {
                BLASTAbsorber(AbsorberNum).GeneratorSubcool = rNumericArgs(17);
            } else {
                BLASTAbsorber(AbsorberNum).GeneratorSubcool = 1.0;
            }

            if (NumNums > 17) {
                BLASTAbsorber(AbsorberNum).SizFac = rNumericArgs(18);
            } else {
                BLASTAbsorber(AbsorberNum).SizFac = 1.0;
            }
        }

        if (ErrorsFound) {
            ShowFatalError("Errors found in processing input for " + cCurrentModuleObject);
        }



        SetupOutputVars(GenInputOutputNodesUsed);

        if (allocated(GenInputOutputNodesUsed)) GenInputOutputNodesUsed.deallocate();
    }

    void SetupOutputVars(Array1D_bool GenInputOutputNodesUsed)
    {
        int AbsorberNum;
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

            if (BLASTAbsorber(AbsorberNum).GenHeatSourceType == NodeType_Water) {
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
    }

    void BLASTAbsorberSpecs::InitBLASTAbsorberModel(int const ChillNum, // number of the current absorption chiller being simulated
                                                    bool const RunFlag, // TRUE when chiller operating
                                                    Real64 const MyLoad)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad
        //       DATE WRITTEN   September 2009
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for initializations of the Absorption Chiller components

        // METHODOLOGY EMPLOYED:
        // Uses the status flags to trigger initializations.

        // Using/Aliasing
        // using DataGlobals::AnyEnergyManagementSystemInModel;
        // using DataGlobals::BeginEnvrnFlag;
        // using DataPlant::LoopFlowStatus_NeedyIfLoopOn;
        // using DataPlant::PlantFirstSizesOkayToFinalize;
        // using DataPlant::PlantLoop;
        // using DataPlant::TypeOf_Chiller_Absorption;
        // using EMSManager::CheckIfNodeSetPointManagedByEMS;
        // using EMSManager::iTemperatureSetPoint;
        // using FluidProperties::GetDensityGlycol;
        // using FluidProperties::GetSatDensityRefrig;
        // using FluidProperties::GetSatEnthalpyRefrig;
        // using PlantUtilities::InitComponentNodes;
        // using PlantUtilities::InterConnectTwoPlantLoopSides;
        // using PlantUtilities::ScanPlantLoopsForObject;
        // using PlantUtilities::SetComponentFlowRate;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("InitBLASTAbsorberModel");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        // static bool MyOneTimeFlag(true);
        static Array1D_bool MyFlag;
        static Array1D_bool MyEnvrnFlag;
        int CondInletNode;  // node number of water inlet node to the condenser
        int CondOutletNode; // node number of water outlet node from the condenser
        bool errFlag;
        bool FatalError;
        Real64 rho;             // local fluid density
        Real64 CpWater;         // local specific heat
        Real64 SteamDensity;    // density of generator steam (when connected to a steam loop)
        Real64 EnthSteamOutDry; // dry enthalpy of steam (quality = 1)
        Real64 EnthSteamOutWet; // wet enthalpy of steam (quality = 0)
        Real64 HfgSteam;        // latent heat of steam at constant pressure
        Real64 SteamDeltaT;     // amount of sub-cooling of steam condensate
        int GeneratorInletNode; // generator inlet node number, steam/water side
        Real64 SteamOutletTemp;
        // static int DummyWaterIndex(1);
        int DummyWaterIndex;
        Real64 mdotEvap; // local fluid mass flow rate thru evaporator
        Real64 mdotCond; // local fluid mass flow rate thru condenser
        Real64 mdotGen;  // local fluid mass flow rate thru generator
        // this-> = BLASTAbsorber(ChillNum)
        // Do the one time initializations
        if (this->MyOneTimeFlag) {
            MyFlag.allocate(NumBLASTAbsorbers);
            MyEnvrnFlag.allocate(NumBLASTAbsorbers);
            MyFlag = true;
            MyEnvrnFlag = true;
            // MyOneTimeFlag = false;
        }

        // Init more variables
        if (MyFlag(ChillNum)) {
            // Locate the chillers on the plant loops for later usage
            errFlag = false;
            PlantUtilities::ScanPlantLoopsForObject(this->Name,
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
                PlantUtilities::ScanPlantLoopsForObject(this->Name,
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
                PlantUtilities::InterConnectTwoPlantLoopSides(this->CWLoopNum,
                                                              this->CWLoopSideNum,
                                                              this->CDLoopNum,
                                                              this->CDLoopSideNum,
                                                              DataPlant::TypeOf_Chiller_Absorption,
                                                              true);
            }
            if (this->GeneratorInletNodeNum > 0) {
                PlantUtilities::ScanPlantLoopsForObject(this->Name,
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
                PlantUtilities::InterConnectTwoPlantLoopSides(this->CWLoopNum,
                                                              this->CWLoopSideNum,
                                                              this->GenLoopNum,
                                                              this->GenCompNum,
                                                              DataPlant::TypeOf_Chiller_Absorption,
                                                              true);
            }

            // Fill in connection data
            if ((this->CondInletNodeNum > 0) && (this->GeneratorInletNodeNum > 0)) {
                PlantUtilities::InterConnectTwoPlantLoopSides(this->CDLoopNum,
                                                              this->CDLoopSideNum,
                                                              this->GenLoopNum,
                                                              this->GenCompNum,
                                                              DataPlant::TypeOf_Chiller_Absorption,
                                                              false);
            }
            if (errFlag) {
                ShowFatalError("InitBLASTAbsorberModel: Program terminated due to previous condition(s).");
            }

            if (this->FlowMode == ConstantFlow) {
                DataPlant::PlantLoop(this->CWLoopNum)
                    .LoopSide(this->CWLoopSideNum)
                    .Branch(this->CWBranchNum)
                    .Comp(this->CWCompNum)
                    .FlowPriority = DataPlant::LoopFlowStatus_NeedyIfLoopOn;
            }

            if (this->FlowMode == LeavingSetPointModulated) {
                DataPlant::PlantLoop(this->CWLoopNum)
                    .LoopSide(this->CWLoopSideNum)
                    .Branch(this->CWBranchNum)
                    .Comp(this->CWCompNum)
                    .FlowPriority = DataPlant::LoopFlowStatus_NeedyIfLoopOn;

                if ((Node(this->EvapOutletNodeNum).TempSetPoint == SensedNodeFlagValue) &&
                    (Node(this->EvapOutletNodeNum).TempSetPointHi == SensedNodeFlagValue)) {
                    if (!DataGlobals::AnyEnergyManagementSystemInModel) {
                        if (!this->ModulatedFlowErrDone) {
                            ShowWarningError("Missing temperature setpoint for LeavingSetpointModulated mode chiller named " +
                                             this->Name);
                            ShowContinueError(
                                "  A temperature setpoint is needed at the outlet node of a chiller in variable flow mode, use a SetpointManager");
                            ShowContinueError("  The overall loop setpoint will be assumed for chiller. The simulation continues ... ");
                            this->ModulatedFlowErrDone = true;
                        }
                    } else {
                        // need call to EMS to check node
                        FatalError = false; // but not really fatal yet, but should be.
                        EMSManager::CheckIfNodeSetPointManagedByEMS(
                            this->EvapOutletNodeNum, EMSManager::iTemperatureSetPoint, FatalError);
                        if (FatalError) {
                            if (!this->ModulatedFlowErrDone) {
                                ShowWarningError("Missing temperature setpoint for LeavingSetpointModulated mode chiller named " +
                                                 this->Name);
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
                    Node(this->EvapOutletNodeNum).TempSetPoint =
                        Node(DataPlant::PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPoint;
                    Node(this->EvapOutletNodeNum).TempSetPointHi =
                        Node(DataPlant::PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPointHi;
                }
            }

            MyFlag = false;
        }

        CondInletNode = this->CondInletNodeNum;
        CondOutletNode = this->CondOutletNodeNum;

        if (MyEnvrnFlag(ChillNum) && DataGlobals::BeginEnvrnFlag && (DataPlant::PlantFirstSizesOkayToFinalize)) {

            rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->CWLoopNum).FluidName,
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

            rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->CDLoopNum).FluidName,
                                                    DataGlobals::CWInitConvTemp,
                                                    DataPlant::PlantLoop(this->CDLoopNum).FluidIndex,
                                                    RoutineName);

            this->CondMassFlowRateMax = rho * this->CondVolFlowRate;

            PlantUtilities::InitComponentNodes(0.0,
                                               this->CondMassFlowRateMax,
                                               CondInletNode,
                                               CondOutletNode,
                                               this->CDLoopNum,
                                               this->CDLoopSideNum,
                                               this->CDBranchNum,
                                               this->CDCompNum);
            Node(CondInletNode).Temp = this->TempDesCondIn;

            if (this->GeneratorInletNodeNum > 0) {

                if (this->GenHeatSourceType == NodeType_Water) {
                    rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->GenLoopNum).FluidName,
                                                            DataGlobals::HWInitConvTemp,
                                                            DataPlant::PlantLoop(this->GenLoopNum).FluidIndex,
                                                            RoutineName);

                    this->GenMassFlowRateMax = rho * this->GeneratorVolFlowRate;
                } else if (this->GenHeatSourceType == NodeType_Steam) {

                    QGenerator = (this->SteamLoadCoef(1) + this->SteamLoadCoef(2) +
                                  this->SteamLoadCoef(3)) *
                                 this->NomCap;
                    GeneratorInletNode = this->GeneratorInletNodeNum;
                    EnthSteamOutDry = FluidProperties::GetSatEnthalpyRefrig(fluidNameSteam,
                                                                            Node(GeneratorInletNode).Temp,
                                                                            1.0,
                                                                            this->SteamFluidIndex,
                                                                            calcChillerAbsorption + this->Name);
                    EnthSteamOutWet = FluidProperties::GetSatEnthalpyRefrig(fluidNameSteam,
                                                                            Node(GeneratorInletNode).Temp,
                                                                            0.0,
                                                                            this->SteamFluidIndex,
                                                                            calcChillerAbsorption + this->Name);
                    SteamDeltaT = this->GeneratorSubcool;
                    SteamOutletTemp = Node(GeneratorInletNode).Temp - SteamDeltaT;
                    HfgSteam = EnthSteamOutDry - EnthSteamOutWet;
                    SteamDensity = FluidProperties::GetSatDensityRefrig(fluidNameSteam,
                                                                        Node(GeneratorInletNode).Temp,
                                                                        1.0,
                                                                        this->SteamFluidIndex,
                                                                        calcChillerAbsorption + this->Name);
                    CpWater = FluidProperties::GetDensityGlycol(
                        fluidNameWater, SteamOutletTemp, DummyWaterIndex, calcChillerAbsorption + this->Name);
                    this->GenMassFlowRateMax = QGenerator / (HfgSteam + CpWater * SteamDeltaT);
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

            MyEnvrnFlag = false;
        }
        if (!DataGlobals::BeginEnvrnFlag) {
            MyEnvrnFlag = true;
        }

        // every time inits

        if ((this->FlowMode == LeavingSetPointModulated) && this->ModulatedFlowSetToLoop) {
            // fix for clumsy old input that worked because loop setpoint was spread.
            //  could be removed with transition, testing , model change, period of being obsolete.
            Node(this->EvapOutletNodeNum).TempSetPoint =
                Node(DataPlant::PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPoint;
            Node(this->EvapOutletNodeNum).TempSetPointHi =
                Node(DataPlant::PlantLoop(this->CWLoopNum).TempSetPointNodeNum).TempSetPointHi;
        }

        if ((MyLoad < 0.0) && RunFlag) {
            mdotEvap = this->EvapMassFlowRateMax;
            mdotCond = this->CondMassFlowRateMax;
            mdotGen = this->GenMassFlowRateMax;
        } else {
            mdotEvap = 0.0;
            mdotCond = 0.0;
            mdotGen = 0.0;
        }

        PlantUtilities::SetComponentFlowRate(mdotEvap,
                                             this->EvapInletNodeNum,
                                             this->EvapOutletNodeNum,
                                             this->CWLoopNum,
                                             this->CWLoopSideNum,
                                             this->CWBranchNum,
                                             this->CWCompNum);

        PlantUtilities::SetComponentFlowRate(mdotCond,
                                             CondInletNode,
                                             CondOutletNode,
                                             this->CDLoopNum,
                                             this->CDLoopSideNum,
                                             this->CDBranchNum,
                                             this->CDCompNum);

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


    void BLASTAbsorberSpecs::SizeAbsorpChiller(int const ChillNum)
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

        // Using/Aliasing
        using namespace DataSizing;
        // using DataPlant::PlantFinalSizesOkayToReport;
        // using DataPlant::PlantFirstSizesOkayToFinalize;
        // using DataPlant::PlantFirstSizesOkayToReport;
        // using DataPlant::PlantLoop;
        // using PlantUtilities::MyPlantSizingIndex;
        // using PlantUtilities::RegisterPlantCompDesignFlow;
        // using ReportSizingManager::ReportSizingOutput;
        using namespace OutputReportPredefined;
        using namespace FluidProperties;

        // Locals
        Real64 SteamMassFlowRate; // steam mass flow rate through generator

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("SizeAbsorpChiller");
        static std::string const RoutineNameLong("SizeAbsorptionChiller");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
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
        // static int DummWaterIndex(1);
        int DummWaterIndex;
        Real64 NomCapUser(0.0);               // Hardsized nominal capacity for reporting
        Real64 NomPumpPowerUser(0.0);         // Hardsized nominal pump power for reporting
        Real64 EvapVolFlowRateUser(0.0);      // Hardsized evaporator volume flow rate for reporting
        Real64 CondVolFlowRateUser(0.0);      // Hardsized condenser flow rate for reporting
        Real64 GeneratorVolFlowRateUser(0.0); // Hardsized generator flow rate for reporting

        //this-> = BLASTAbsorber(ChillNum)
        SteamInputRatNom = 
            this->SteamLoadCoef(1) + this->SteamLoadCoef(2) + this->SteamLoadCoef(3);
        // init local temporary version in case of partial/mixed autosizing
        tmpNomCap = this->NomCap;
        tmpNomPumpPower = this->NomPumpPower;
        tmpEvapVolFlowRate = this->EvapVolFlowRate;
        tmpCondVolFlowRate = this->CondVolFlowRate;
        tmpGeneratorVolFlowRate = this->GeneratorVolFlowRate;

        // find the appropriate Plant Sizing object
        PltSizNum = DataPlant::PlantLoop(this->CWLoopNum).PlantSizNum;
        PltSizCondNum = DataPlant::PlantLoop(this->CDLoopNum).PlantSizNum;

        if (this->GenHeatSourceType == NodeType_Steam) {
            if (this->GeneratorInletNodeNum > 0 && this->GeneratorOutletNodeNum > 0) {
                PltSizSteamNum = PlantUtilities::MyPlantSizingIndex(moduleObjectType,
                                                                    this->Name,
                                                                    this->GeneratorInletNodeNum,
                                                                    this->GeneratorOutletNodeNum,
                                                                    LoopErrorsFound);
            } else {
                for (PltSizIndex = 1; PltSizIndex <= NumPltSizInput; ++PltSizIndex) {
                    if (PlantSizData(PltSizIndex).LoopType == SteamLoop) {
                        PltSizSteamNum = PltSizIndex;
                    }
                }
            }
        } else {
            if (this->GeneratorInletNodeNum > 0 && this->GeneratorOutletNodeNum > 0) {
                PltSizHeatingNum = PlantUtilities::MyPlantSizingIndex(moduleObjectType,
                                                                      this->Name,
                                                                      this->GeneratorInletNodeNum,
                                                                      this->GeneratorOutletNodeNum,
                                                                      LoopErrorsFound);
            } else {
                for (PltSizIndex = 1; PltSizIndex <= NumPltSizInput; ++PltSizIndex) {
                    if (PlantSizData(PltSizIndex).LoopType == HeatingLoop) {
                        PltSizHeatingNum = PltSizIndex;
                    }
                }
            }
        }

        if (PltSizNum > 0) {
            if (PlantSizData(PltSizNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {

                Cp = GetSpecificHeatGlycol(DataPlant::PlantLoop(this->CWLoopNum).FluidName,
                                           DataGlobals::CWInitConvTemp,
                                           DataPlant::PlantLoop(this->CWLoopNum).FluidIndex,
                                           RoutineName);

                rho = GetDensityGlycol(DataPlant::PlantLoop(this->CWLoopNum).FluidName,
                                       DataGlobals::CWInitConvTemp,
                                       DataPlant::PlantLoop(this->CWLoopNum).FluidIndex,
                                       RoutineName);
                tmpNomCap = Cp * rho * PlantSizData(PltSizNum).DeltaT * PlantSizData(PltSizNum).DesVolFlowRate * this->SizFac;
                if (!this->NomCapWasAutoSized) tmpNomCap = this->NomCap;
            } else {
                if (this->NomCapWasAutoSized) tmpNomCap = 0.0;
            }
            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                if (this->NomCapWasAutoSized) {
                    this->NomCap = tmpNomCap;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(
                            moduleObjectType, this->Name, "Design Size Nominal Capacity [W]", tmpNomCap);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(
                            moduleObjectType, this->Name, "Initial Design Size Nominal Capacity [W]", tmpNomCap);
                    }
                } else {
                    if (this->NomCap > 0.0 && tmpNomCap > 0.0) {
                        NomCapUser = this->NomCap;
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput(moduleObjectType,
                                                                    this->Name,
                                                                    "Design Size Nominal Capacity [W]",
                                                                    tmpNomCap,
                                                                    "User-Specified Nominal Capacity [W]",
                                                                    NomCapUser);
                            if (DataGlobals::DisplayExtraWarnings) {
                                if ((std::abs(tmpNomCap - NomCapUser) / NomCapUser) > AutoVsHardSizingThreshold) {
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
                ReportSizingManager::ReportSizingOutput(
                    moduleObjectType, this->Name, "User-Specified Nominal Capacity [W]", this->NomCap);
            }
        }

        tmpNomPumpPower = 0.0045 * this->NomCap;

        if (DataPlant::PlantFirstSizesOkayToFinalize) {
            // the DOE-2 EIR for single stage absorption chiller
            if (this->NomPumpPowerWasAutoSized) {
                this->NomPumpPower = tmpNomPumpPower;
                if (DataPlant::PlantFinalSizesOkayToReport) {
                    ReportSizingManager::ReportSizingOutput(
                        moduleObjectType, this->Name, "Design Size Nominal Pumping Power [W]", tmpNomPumpPower);
                }
                if (DataPlant::PlantFirstSizesOkayToReport) {
                    ReportSizingManager::ReportSizingOutput(
                        moduleObjectType, this->Name, "Initial Design Size Nominal Pumping Power [W]", tmpNomPumpPower);
                }
            } else {
                if (this->NomPumpPower > 0.0 && tmpNomPumpPower > 0.0) {
                    NomPumpPowerUser = this->NomPumpPower;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(moduleObjectType,
                                                                this->Name,
                                                                "Design Size Nominal Pumping Power [W]",
                                                                tmpNomPumpPower,
                                                                "User-Specified Nominal Pumping Power [W]",
                                                                NomPumpPowerUser);
                        if (DataGlobals::DisplayExtraWarnings) {
                            if ((std::abs(tmpNomPumpPower - NomPumpPowerUser) / NomPumpPowerUser) > AutoVsHardSizingThreshold) {
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
            if (PlantSizData(PltSizNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                tmpEvapVolFlowRate = PlantSizData(PltSizNum).DesVolFlowRate * this->SizFac;
                if (!this->EvapVolFlowRateWasAutoSized) tmpEvapVolFlowRate = this->EvapVolFlowRate;
            } else {
                if (this->EvapVolFlowRateWasAutoSized) tmpEvapVolFlowRate = 0.0;
            }
            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                if (this->EvapVolFlowRateWasAutoSized) {
                    this->EvapVolFlowRate = tmpEvapVolFlowRate;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(
                            moduleObjectType, this->Name, "Design Size Design Chilled Water Flow Rate [m3/s]", tmpEvapVolFlowRate);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(moduleObjectType,
                                                                this->Name,
                                                                "Initial Design Size Design Chilled Water Flow Rate [m3/s]",
                                                                tmpEvapVolFlowRate);
                    }
                } else {
                    if (this->EvapVolFlowRate > 0.0 && tmpEvapVolFlowRate > 0.0) {
                        EvapVolFlowRateUser = this->EvapVolFlowRate;
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput(moduleObjectType,
                                                                    this->Name,
                                                                    "Design Size Design Chilled Water Flow Rate [m3/s]",
                                                                    tmpEvapVolFlowRate,
                                                                    "User-Specified Design Chilled Water Flow Rate [m3/s]",
                                                                    EvapVolFlowRateUser);
                            if (DataGlobals::DisplayExtraWarnings) {
                                if ((std::abs(tmpEvapVolFlowRate - EvapVolFlowRateUser) / EvapVolFlowRateUser) > AutoVsHardSizingThreshold) {
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
            if (!this->EvapVolFlowRateWasAutoSized && DataPlant::PlantFinalSizesOkayToReport &&
                this->EvapVolFlowRate > 0.0) {
                ReportSizingManager::ReportSizingOutput(moduleObjectType,
                                                        this->Name,
                                                        "User-Specified Design Chilled Water Flow Rate [m3/s]",
                                                        this->EvapVolFlowRate);
            }
        }

        PlantUtilities::RegisterPlantCompDesignFlow(this->EvapInletNodeNum, tmpEvapVolFlowRate);

        if (PltSizCondNum > 0 && PltSizNum > 0) {
            if (this->EvapVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow && tmpNomCap > 0.0) {
                //       QCondenser = QEvaporator + QGenerator + PumpingPower

                Cp = GetSpecificHeatGlycol(DataPlant::PlantLoop(this->CDLoopNum).FluidName,
                                           this->TempDesCondIn,
                                           DataPlant::PlantLoop(this->CDLoopNum).FluidIndex,
                                           RoutineName);

                rho = GetDensityGlycol(DataPlant::PlantLoop(this->CDLoopNum).FluidName,
                                       DataGlobals::CWInitConvTemp,
                                       DataPlant::PlantLoop(this->CDLoopNum).FluidIndex,
                                       RoutineName);
                tmpCondVolFlowRate =
                    tmpNomCap * (1.0 + SteamInputRatNom + tmpNomPumpPower / tmpNomCap) / (PlantSizData(PltSizCondNum).DeltaT * Cp * rho);
                if (!this->CondVolFlowRateWasAutoSized) tmpCondVolFlowRate = this->CondVolFlowRate;

            } else {
                if (this->CondVolFlowRateWasAutoSized) tmpCondVolFlowRate = 0.0;
            }
            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                if (this->CondVolFlowRateWasAutoSized) {
                    this->CondVolFlowRate = tmpCondVolFlowRate;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(moduleObjectType,
                                                                this->Name,
                                                                "Design Size Design Condenser Water Flow Rate [m3/s]",
                                                                tmpCondVolFlowRate);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(moduleObjectType,
                                                                this->Name,
                                                                "Initial Design Size Design Condenser Water Flow Rate [m3/s]",
                                                                tmpCondVolFlowRate);
                    }
                } else {
                    if (this->CondVolFlowRate > 0.0 && tmpCondVolFlowRate > 0.0) {
                        CondVolFlowRateUser = this->CondVolFlowRate;
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput(moduleObjectType,
                                                                    this->Name,
                                                                    "Design Size Design Condenser Water Flow Rate [m3/s]",
                                                                    tmpCondVolFlowRate,
                                                                    "User-Specified Design Condenser Water Flow Rate [m3/s]",
                                                                    CondVolFlowRateUser);
                            if (DataGlobals::DisplayExtraWarnings) {
                                if ((std::abs(tmpCondVolFlowRate - CondVolFlowRateUser) / CondVolFlowRateUser) > AutoVsHardSizingThreshold) {
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
            if (!this->CondVolFlowRateWasAutoSized && DataPlant::PlantFirstSizesOkayToFinalize &&
                (this->CondVolFlowRate > 0.0)) {
                ReportSizingManager::ReportSizingOutput(moduleObjectType,
                                                        this->Name,
                                                        "User-Specified Design Condenser Water Flow Rate [m3/s]",
                                                        this->CondVolFlowRate);
            }
        }

        // save the design condenser water volumetric flow rate for use by the condenser water loop sizing algorithms
        PlantUtilities::RegisterPlantCompDesignFlow(this->CondInletNodeNum, tmpCondVolFlowRate);

        if ((PltSizSteamNum > 0 && this->GenHeatSourceType == NodeType_Steam) ||
            (PltSizHeatingNum > 0 && this->GenHeatSourceType == NodeType_Water)) {
            if (this->EvapVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow && tmpNomCap > 0.0) {
                if (this->GenHeatSourceType == NodeType_Water) {
                    CpWater = GetSpecificHeatGlycol(DataPlant::PlantLoop(this->GenLoopNum).FluidName,
                                                    PlantSizData(PltSizHeatingNum).ExitTemp,
                                                    DataPlant::PlantLoop(this->GenLoopNum).FluidIndex,
                                                    RoutineName);
                    SteamDeltaT = max(0.5, PlantSizData(PltSizHeatingNum).DeltaT);
                    RhoWater = GetDensityGlycol(DataPlant::PlantLoop(this->GenLoopNum).FluidName,
                                                (PlantSizData(PltSizHeatingNum).ExitTemp - SteamDeltaT),
                                                DataPlant::PlantLoop(this->GenLoopNum).FluidIndex,
                                                RoutineName);
                    tmpGeneratorVolFlowRate = (this->NomCap * SteamInputRatNom) / (CpWater * SteamDeltaT * RhoWater);
                    if (!this->GeneratorVolFlowRateWasAutoSized)
                        tmpGeneratorVolFlowRate = this->GeneratorVolFlowRate;
                    if (DataPlant::PlantFirstSizesOkayToFinalize) {
                        if (this->GeneratorVolFlowRateWasAutoSized) {
                            this->GeneratorVolFlowRate = tmpGeneratorVolFlowRate;
                            if (DataPlant::PlantFinalSizesOkayToReport) {
                                ReportSizingManager::ReportSizingOutput(moduleObjectType,
                                                                        this->Name,
                                                                        "Design Size Design Generator Fluid Flow Rate [m3/s]",
                                                                        tmpGeneratorVolFlowRate);
                            }
                            if (DataPlant::PlantFirstSizesOkayToReport) {
                                ReportSizingManager::ReportSizingOutput(moduleObjectType,
                                                                        this->Name,
                                                                        "Iniital Design Size Design Generator Fluid Flow Rate [m3/s]",
                                                                        tmpGeneratorVolFlowRate);
                            }
                        } else {
                            if (this->GeneratorVolFlowRate > 0.0 && tmpGeneratorVolFlowRate > 0.0) {
                                GeneratorVolFlowRateUser = this->GeneratorVolFlowRate;
                                if (DataPlant::PlantFinalSizesOkayToReport) {
                                    ReportSizingManager::ReportSizingOutput(moduleObjectType,
                                                                            this->Name,
                                                                            "Design Size Design Generator Fluid Flow Rate [m3/s]",
                                                                            tmpGeneratorVolFlowRate,
                                                                            "User-Specified Design Generator Fluid Flow Rate [m3/s]",
                                                                            GeneratorVolFlowRateUser);
                                    if (DataGlobals::DisplayExtraWarnings) {
                                        if ((std::abs(tmpGeneratorVolFlowRate - GeneratorVolFlowRateUser) / GeneratorVolFlowRateUser) >
                                            AutoVsHardSizingThreshold) {
                                            ShowMessage("SizeChillerAbsorption: Potential issue with equipment sizing for " +
                                                        this->Name);
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
                    SteamDensity = GetSatDensityRefrig(
                        fluidNameSteam, PlantSizData(PltSizSteamNum).ExitTemp, 1.0, this->SteamFluidIndex, RoutineNameLong);
                    SteamDeltaT = PlantSizData(PltSizSteamNum).DeltaT;
                    GeneratorOutletTemp = PlantSizData(PltSizSteamNum).ExitTemp - SteamDeltaT;

                    EnthSteamOutDry = GetSatEnthalpyRefrig(fluidNameSteam,
                                                           PlantSizData(PltSizSteamNum).ExitTemp,
                                                           1.0,
                                                           this->SteamFluidIndex,
                                                           moduleObjectType + this->Name);
                    EnthSteamOutWet = GetSatEnthalpyRefrig(fluidNameSteam,
                                                           PlantSizData(PltSizSteamNum).ExitTemp,
                                                           0.0,
                                                           this->SteamFluidIndex,
                                                           moduleObjectType + this->Name);
                    CpWater = GetSpecificHeatGlycol(fluidNameWater, GeneratorOutletTemp, DummWaterIndex, RoutineName);
                    HfgSteam = EnthSteamOutDry - EnthSteamOutWet;
                    SteamMassFlowRate = (this->NomCap * SteamInputRatNom) / ((HfgSteam) + (SteamDeltaT * CpWater));
                    tmpGeneratorVolFlowRate = SteamMassFlowRate / SteamDensity;

                    if (!this->GeneratorVolFlowRateWasAutoSized)
                        tmpGeneratorVolFlowRate = this->GeneratorVolFlowRate;
                    if (DataPlant::PlantFirstSizesOkayToFinalize) {

                        if (this->GeneratorVolFlowRateWasAutoSized) {
                            this->GeneratorVolFlowRate = tmpGeneratorVolFlowRate;
                            if (DataPlant::PlantFinalSizesOkayToReport) {
                                ReportSizingManager::ReportSizingOutput(moduleObjectType,
                                                                        this->Name,
                                                                        "Design Size Design Generator Fluid Flow Rate [m3/s]",
                                                                        tmpGeneratorVolFlowRate);
                            }
                            if (DataPlant::PlantFirstSizesOkayToReport) {
                                ReportSizingManager::ReportSizingOutput(moduleObjectType,
                                                                        this->Name,
                                                                        "Initial Design Size Design Generator Fluid Flow Rate [m3/s]",
                                                                        tmpGeneratorVolFlowRate);
                            }
                        } else {
                            if (this->GeneratorVolFlowRate > 0.0 && tmpGeneratorVolFlowRate > 0.0) {
                                GeneratorVolFlowRateUser = this->GeneratorVolFlowRate;
                                if (DataPlant::PlantFinalSizesOkayToReport) {
                                    ReportSizingManager::ReportSizingOutput(moduleObjectType,
                                                                            this->Name,
                                                                            "Design Size Design Generator Fluid Flow Rate [m3/s]",
                                                                            tmpGeneratorVolFlowRate,
                                                                            "User-Specified Design Generator Fluid Flow Rate [m3/s]",
                                                                            GeneratorVolFlowRateUser);
                                    if (DataGlobals::DisplayExtraWarnings) {
                                        if ((std::abs(tmpGeneratorVolFlowRate - GeneratorVolFlowRateUser) / GeneratorVolFlowRateUser) >
                                            AutoVsHardSizingThreshold) {
                                            ShowMessage("SizeChillerAbsorption: Potential issue with equipment sizing for " +
                                                        this->Name);
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
            if (!this->GeneratorVolFlowRateWasAutoSized && DataPlant::PlantFinalSizesOkayToReport &&
                (this->GeneratorVolFlowRate > 0.0)) {
                ReportSizingManager::ReportSizingOutput(moduleObjectType,
                                                        this->Name,
                                                        "User-Specified Design Generator Fluid Flow Rate [m3/s]",
                                                        this->GeneratorVolFlowRate);
            }
        }

        // save the design steam or hot water volumetric flow rate for use by the steam or hot water loop sizing algorithms
        if (DataPlant::PlantFirstSizesOkayToFinalize) {
            PlantUtilities::RegisterPlantCompDesignFlow(this->GeneratorInletNodeNum, this->GeneratorVolFlowRate);
        } else {
            PlantUtilities::RegisterPlantCompDesignFlow(this->GeneratorInletNodeNum, tmpGeneratorVolFlowRate);
        }

        if (this->GeneratorDeltaTempWasAutoSized) {
            if (PltSizHeatingNum > 0 && this->GenHeatSourceType == NodeType_Water) {
                this->GeneratorDeltaTemp = max(0.5, PlantSizData(PltSizHeatingNum).DeltaT);
            } else if (this->GenHeatSourceType == NodeType_Water) {
                if (DataPlant::PlantFirstSizesOkayToFinalize) {
                    Cp = GetSpecificHeatGlycol(DataPlant::PlantLoop(this->GenLoopNum).FluidName,
                                               DataGlobals::HWInitConvTemp,
                                               DataPlant::PlantLoop(this->GenLoopNum).FluidIndex,
                                               RoutineName);
                    rho = GetDensityGlycol(DataPlant::PlantLoop(this->GenLoopNum).FluidName,
                                           DataGlobals::HWInitConvTemp,
                                           DataPlant::PlantLoop(this->GenLoopNum).FluidIndex,
                                           RoutineName);

                    this->GeneratorDeltaTemp =
                        (SteamInputRatNom * this->NomCap) / (Cp * rho * this->GeneratorVolFlowRate);
                }
            }
        }

        if (ErrorsFound) {
            ShowFatalError("Preceding sizing errors cause program termination");
        }

        if (DataPlant::PlantFinalSizesOkayToReport) {
            // create predefined report
            equipName = this->Name;
            PreDefTableEntry(pdchMechType, equipName, moduleObjectType);
            PreDefTableEntry(pdchMechNomEff, equipName, "n/a");
            PreDefTableEntry(pdchMechNomCap, equipName, this->NomCap);
        }
    }

    void BLASTAbsorberSpecs::CalcBLASTAbsorberModel(int &ChillNum,                        // Absorber number
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

        // Using/Aliasing
        using namespace FluidProperties;
        // using DataBranchAirLoopPlant::ControlType_SeriesActive;
        // using DataBranchAirLoopPlant::MassFlowTolerance;
        // using DataGlobals::BeginEnvrnFlag;
        // using DataGlobals::DoingSizing;
        // using DataGlobals::KickOffSimulation;
        // using DataGlobals::SecInHour;
        // using DataGlobals::WarmupFlag;
        // using DataHVACGlobals::TimeStepSys;
        // using DataPlant::CompSetPtBasedSchemeType;
        // using DataPlant::DeltaTempTol;
        // using DataPlant::DualSetPointDeadBand;
        // using DataPlant::PlantLoop;
        // using DataPlant::SingleSetPoint;
        // using FaultsManager::FaultsChillerSWTSensor;
        // using General::TrimSigDigits;
        // using PlantUtilities::SetComponentFlowRate;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("CalcBLASTAbsorberModel");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
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
        // static Array1D_bool MyEnvironFlag;
        // static Array1D_bool MyEnvironSteamFlag;
        Real64 FRAC;
        //  LOGICAL,SAVE           :: PossibleSubcooling
        Real64 CpFluid; // local specific heat of fluid
        Real64 SteamDeltaT;
        Real64 SteamOutletTemp;
        int LoopNum;
        int LoopSideNum;
        // static int DummyWaterIndex(1);
        int DummyWaterIndex;
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

        // this-> = BLASTAbsorber(ChillNum)

        EvapInletNode = this->EvapInletNodeNum;
        EvapOutletNode = this->EvapOutletNodeNum;
        CondInletNode = this->CondInletNodeNum;
        CondOutletNode = this->CondOutletNodeNum;
        GeneratorInletNode = this->GeneratorInletNodeNum;
        GeneratorOutletNode = this->GeneratorOutletNodeNum;

        // If no loop demand or Absorber OFF, return
        if (MyLoad >= 0.0 || !RunFlag) { // off or heating
            if (EquipFlowCtrl == DataBranchAirLoopPlant::ControlType_SeriesActive) EvapMassFlowRate = Node(EvapInletNode).MassFlowRate;
            return;
        }

        // set module level Absorber inlet and temperature variables
        EvapInletTemp = Node(EvapInletNode).Temp;
        CondInletTemp = Node(CondInletNode).Temp;

        // Set the condenser mass flow rates
        CondMassFlowRate = Node(CondInletNode).MassFlowRate;

        //  LOAD LOCAL VARIABLES FROM DATA STRUCTURE (for code readability)
        
        SteamLoadFactor = this->SteamLoadCoef;
        ElectricLoadFactor = this->PumpPowerCoef;
        MinPartLoadRat = this->MinPartLoadRat;
        MaxPartLoadRat = this->MaxPartLoadRat;
        TempCondInDesign = this->TempDesCondIn;
        AbsorberNomCap = this->NomCap;
        NomPumpPower = this->NomPumpPower;
        TempCondIn = Node(this->CondInletNodeNum).Temp;
        TempEvapOut = Node(this->EvapOutletNodeNum).Temp;
        TempLowLimitEout = this->TempLowLimitEvapOut;
        LoopNum = this->CWLoopNum;
        LoopSideNum = this->CWLoopSideNum;

        CpFluid = GetSpecificHeatGlycol(DataPlant::PlantLoop(this->CWLoopNum).FluidName,
                                        EvapInletTemp,
                                        DataPlant::PlantLoop(this->CWLoopNum).FluidIndex,
                                        RoutineName);

        // If there is a fault of Chiller SWT Sensor (zrp_Jun2016)
        if (this->FaultyChillerSWTFlag && (!DataGlobals::WarmupFlag) && (!DataGlobals::DoingSizing) &&
            (!DataGlobals::KickOffSimulation)) {
            int FaultIndex = this->FaultyChillerSWTIndex;
            Real64 EvapOutletTemp_ff = TempEvapOut;

            // calculate the sensor offset using fault information
            this->FaultyChillerSWTOffset = FaultsManager::FaultsChillerSWTSensor(FaultIndex).CalFaultOffsetAct();
            // update the TempEvapOut
            TempEvapOut = max(this->TempLowLimitEvapOut,
                              min(Node(EvapInletNode).Temp, EvapOutletTemp_ff - this->FaultyChillerSWTOffset));
            this->FaultyChillerSWTOffset = EvapOutletTemp_ff - TempEvapOut;
        }

        // If FlowLock is True, the new resolved mdot is used to update Power, QEvap, Qcond, and
        // condenser side outlet temperature.
        if (DataPlant::PlantLoop(LoopNum).LoopSide(LoopSideNum).FlowLock == 0) {
            this->PossibleSubcooling = false;
            QEvaporator = std::abs(MyLoad);
            // limit by max capacity
            QEvaporator = min(QEvaporator, (this->MaxPartLoadRat * this->NomCap));

            // Either set the flow to the Constant value or caluclate the flow for the variable volume
            if ((this->FlowMode == ConstantFlow) || (this->FlowMode == NotModulated)) {
                EvapMassFlowRate = Node(EvapInletNode).MassFlowRate;

                if (EvapMassFlowRate != 0.0) {

                    EvapDeltaTemp = QEvaporator / EvapMassFlowRate / CpFluid;
                } else {
                    EvapDeltaTemp = 0.0;
                }
                EvapOutletTemp = EvapInletTemp - EvapDeltaTemp;

            } else if (this->FlowMode == LeavingSetPointModulated) {
                // Calculate the Delta Temp from the inlet temp to the chiller outlet setpoint
                {
                    auto const SELECT_CASE_var(DataPlant::PlantLoop(this->CWLoopNum).LoopDemandCalcScheme);
                    if (SELECT_CASE_var == DataPlant::SingleSetPoint) {
                        EvapDeltaTemp = Node(EvapInletNode).Temp - Node(EvapOutletNode).TempSetPoint;
                    } else if (SELECT_CASE_var == DataPlant::DualSetPointDeadBand) {
                        EvapDeltaTemp = Node(EvapInletNode).Temp - Node(EvapOutletNode).TempSetPointHi;
                    } else {
                        assert(false);
                    }
                }
                if (EvapDeltaTemp != 0) {

                    EvapMassFlowRate = std::abs(QEvaporator / CpFluid / EvapDeltaTemp);
                    if ((EvapMassFlowRate - this->EvapMassFlowRateMax) > DataBranchAirLoopPlant::MassFlowTolerance)
                        this->PossibleSubcooling = true;
                    // Check to see if the Maximum is exceeded, if so set to maximum
                    EvapMassFlowRate = min(this->EvapMassFlowRateMax, EvapMassFlowRate);
                    PlantUtilities::SetComponentFlowRate(EvapMassFlowRate,
                                                         this->EvapInletNodeNum,
                                                         this->EvapOutletNodeNum,
                                                         this->CWLoopNum,
                                                         this->CWLoopSideNum,
                                                         this->CWBranchNum,
                                                         this->CWCompNum);
                    {
                        auto const SELECT_CASE_var(DataPlant::PlantLoop(this->CWLoopNum).LoopDemandCalcScheme);
                        if (SELECT_CASE_var == DataPlant::SingleSetPoint) {
                            EvapOutletTemp = Node(EvapOutletNode).TempSetPoint;
                        } else if (SELECT_CASE_var == DataPlant::DualSetPointDeadBand) {
                            EvapOutletTemp = Node(EvapOutletNode).TempSetPointHi;
                        }
                    }
                } else {
                    EvapMassFlowRate = 0.0;

                    EvapOutletTemp = Node(EvapInletNode).Temp;

                    ShowRecurringWarningErrorAtEnd("CalcBLASTAbsorberModel: Name=\"" + this->Name +
                                                       "\" Evaporative Condenser Delta Temperature = 0 in mass flow calculation.",
                                                   this->ErrCount2);
                }
            } // End of Constant Variable Flow If Block

            // If there is a fault of Chiller SWT Sensor (zrp_Jun2016)
            if (this->FaultyChillerSWTFlag && (!DataGlobals::WarmupFlag) && (!DataGlobals::DoingSizing) &&
                (!DataGlobals::KickOffSimulation) && (EvapMassFlowRate > 0)) {
                // calculate directly affected variables at faulty case: EvapOutletTemp, EvapMassFlowRate, QEvaporator
                int FaultIndex = this->FaultyChillerSWTIndex;
                bool VarFlowFlag = (this->FlowMode == LeavingSetPointModulated);
                FaultsManager::FaultsChillerSWTSensor(FaultIndex)
                    .CalFaultChillerSWT(VarFlowFlag,
                                        this->FaultyChillerSWTOffset,
                                        CpFluid,
                                        Node(EvapInletNode).Temp,
                                        EvapOutletTemp,
                                        EvapMassFlowRate,
                                        QEvaporator);
                // update corresponding variables at faulty case
                // PartLoadRat = ( AvailChillerCap > 0.0 ) ? ( QEvaporator / AvailChillerCap ) : 0.0;
                // PartLoadRat = max( 0.0, min( PartLoadRat, MaxPartLoadRat ));
                // ChillerPartLoadRatio = PartLoadRat;
                EvapDeltaTemp = Node(EvapInletNode).Temp - EvapOutletTemp;
            }

        } else { // If FlowLock is True

            EvapMassFlowRate = Node(EvapInletNode).MassFlowRate;
            if (this->PossibleSubcooling) {
                QEvaporator = std::abs(MyLoad);
                EvapDeltaTemp = QEvaporator / EvapMassFlowRate / CpFluid;
                EvapOutletTemp = Node(EvapInletNode).Temp - EvapDeltaTemp;
            } else {
                {
                    auto const SELECT_CASE_var(DataPlant::PlantLoop(LoopNum).LoopDemandCalcScheme);
                    if (SELECT_CASE_var == DataPlant::SingleSetPoint) {
                        if ((this->FlowMode == LeavingSetPointModulated) ||
                            (DataPlant::PlantLoop(LoopNum)
                                 .LoopSide(LoopSideNum)
                                 .Branch(this->CWBranchNum)
                                 .Comp(this->CWCompNum)
                                 .CurOpSchemeType == DataPlant::CompSetPtBasedSchemeType) ||
                            (Node(EvapOutletNode).TempSetPoint != SensedNodeFlagValue)) {
                            TempEvapOutSetPoint = Node(EvapOutletNode).TempSetPoint;
                        } else {
                            TempEvapOutSetPoint = Node(DataPlant::PlantLoop(LoopNum).TempSetPointNodeNum).TempSetPoint;
                        }
                    } else if (SELECT_CASE_var == DataPlant::DualSetPointDeadBand) {
                        if ((this->FlowMode == LeavingSetPointModulated) ||
                            (DataPlant::PlantLoop(LoopNum)
                                 .LoopSide(LoopSideNum)
                                 .Branch(this->CWBranchNum)
                                 .Comp(this->CWCompNum)
                                 .CurOpSchemeType == DataPlant::CompSetPtBasedSchemeType) ||
                            (Node(EvapOutletNode).TempSetPointHi != SensedNodeFlagValue)) {
                            TempEvapOutSetPoint = Node(EvapOutletNode).TempSetPointHi;
                        } else {
                            TempEvapOutSetPoint = Node(DataPlant::PlantLoop(LoopNum).TempSetPointNodeNum).TempSetPointHi;
                        }
                    } else {
                        assert(false);
                    }
                }
                EvapDeltaTemp = Node(EvapInletNode).Temp - TempEvapOutSetPoint;
                QEvaporator = std::abs(EvapMassFlowRate * CpFluid * EvapDeltaTemp);
                EvapOutletTemp = TempEvapOutSetPoint;
            }
            // Check that the Evap outlet temp honors both plant loop temp low limit and also the chiller low limit
            if (EvapOutletTemp < TempLowLimitEout) {
                if ((Node(EvapInletNode).Temp - TempLowLimitEout) > DataPlant::DeltaTempTol) {
                    EvapOutletTemp = TempLowLimitEout;
                    EvapDeltaTemp = Node(EvapInletNode).Temp - EvapOutletTemp;
                    QEvaporator = EvapMassFlowRate * CpFluid * EvapDeltaTemp;
                } else {
                    EvapOutletTemp = Node(EvapInletNode).Temp;
                    EvapDeltaTemp = Node(EvapInletNode).Temp - EvapOutletTemp;
                    QEvaporator = EvapMassFlowRate * CpFluid * EvapDeltaTemp;
                }
            }
            if (EvapOutletTemp < Node(EvapOutletNode).TempMin) {
                if ((Node(EvapInletNode).Temp - Node(EvapOutletNode).TempMin) > DataPlant::DeltaTempTol) {
                    EvapOutletTemp = Node(EvapOutletNode).TempMin;
                    EvapDeltaTemp = Node(EvapInletNode).Temp - EvapOutletTemp;
                    QEvaporator = EvapMassFlowRate * CpFluid * EvapDeltaTemp;
                } else {
                    EvapOutletTemp = Node(EvapInletNode).Temp;
                    EvapDeltaTemp = Node(EvapInletNode).Temp - EvapOutletTemp;
                    QEvaporator = EvapMassFlowRate * CpFluid * EvapDeltaTemp;
                }
            }

            // Checks QEvaporator on the basis of the machine limits.
            if (QEvaporator > std::abs(MyLoad)) {
                if (EvapMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance) {
                    QEvaporator = std::abs(MyLoad);
                    EvapDeltaTemp = QEvaporator / EvapMassFlowRate / CpFluid;
                    EvapOutletTemp = Node(EvapInletNode).Temp - EvapDeltaTemp;
                } else {
                    QEvaporator = 0.0;
                    EvapOutletTemp = Node(EvapInletNode).Temp;
                }
            }

            // If there is a fault of Chiller SWT Sensor (zrp_Jun2016)
            if (this->FaultyChillerSWTFlag && (!DataGlobals::WarmupFlag) && (!DataGlobals::DoingSizing) &&
                (!DataGlobals::KickOffSimulation) && (EvapMassFlowRate > 0)) {
                // calculate directly affected variables at faulty case: EvapOutletTemp, EvapMassFlowRate, QEvaporator
                int FaultIndex = this->FaultyChillerSWTIndex;
                bool VarFlowFlag = false;
                FaultsManager::FaultsChillerSWTSensor(FaultIndex)
                    .CalFaultChillerSWT(VarFlowFlag,
                                        this->FaultyChillerSWTOffset,
                                        CpFluid,
                                        Node(EvapInletNode).Temp,
                                        EvapOutletTemp,
                                        EvapMassFlowRate,
                                        QEvaporator);
                // update corresponding variables at faulty case
                EvapDeltaTemp = Node(EvapInletNode).Temp - EvapOutletTemp;
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
            EvapOutletTemp = Node(EvapInletNode).Temp;
            PumpingPower = 0.0;
        }

        QCondenser = QEvaporator + QGenerator + PumpingPower;

        CpFluid = GetSpecificHeatGlycol(DataPlant::PlantLoop(this->CDLoopNum).FluidName,
                                        CondInletTemp,
                                        DataPlant::PlantLoop(this->CDLoopNum).FluidIndex,
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
            if (this->GenHeatSourceType == NodeType_Water) {
                Real64 GenMassFlowRate = 0.0;
                int GenLoopNum = this->GenLoopNum;
                int GenLoopSideNum = this->GenLoopSideNum;
                //  Hot water plant is used for the generator
                CpFluid = GetSpecificHeatGlycol(DataPlant::PlantLoop(GenLoopNum).FluidName,
                                                Node(GeneratorInletNode).Temp,
                                                DataPlant::PlantLoop(GenLoopSideNum).FluidIndex,
                                                RoutineName);
                if (DataPlant::PlantLoop(GenLoopNum).LoopSide(GenLoopSideNum).FlowLock == 0) {
                    if ((this->FlowMode == ConstantFlow) || (this->FlowMode == NotModulated)) {
                        GenMassFlowRate = this->GenMassFlowRateMax;
                    } else { // LeavingSetpointModulated
                        // since the .FlowMode applies to the chiller evaporator, the generater mass flow rate will be proportional to the evaporator
                        // mass flow rate
                        Real64 GenFlowRatio = EvapMassFlowRate / this->EvapMassFlowRateMax;
                        GenMassFlowRate = min(this->GenMassFlowRateMax, GenFlowRatio * this->GenMassFlowRateMax);
                    }
                } else { // If FlowLock is True
                    GenMassFlowRate = Node(GeneratorInletNode).MassFlowRate;
                }
                PlantUtilities::SetComponentFlowRate(GenMassFlowRate,
                                                     GeneratorInletNode,
                                                     GeneratorOutletNode,
                                                     GenLoopNum,
                                                     GenLoopSideNum,
                                                     this->GenBranchNum,
                                                     this->GenCompNum);

                if (GenMassFlowRate <= 0.0) {
                    GenOutletTemp = Node(GeneratorInletNode).Temp;
                    SteamOutletEnthalpy = Node(GeneratorInletNode).Enthalpy;
                } else {
                    GenOutletTemp = Node(GeneratorInletNode).Temp - QGenerator / (CpFluid * GenMassFlowRate);
                    SteamOutletEnthalpy = Node(GeneratorInletNode).Enthalpy - QGenerator / GenMassFlowRate;
                }
                Node(GeneratorOutletNode).Temp = GenOutletTemp;
                Node(GeneratorOutletNode).Enthalpy = SteamOutletEnthalpy;
                Node(GeneratorOutletNode).MassFlowRate = GenMassFlowRate;

            } else { // using a steam plant for the generator

                EnthSteamOutDry = GetSatEnthalpyRefrig(fluidNameSteam,
                                                       Node(GeneratorInletNode).Temp,
                                                       1.0,
                                                       this->SteamFluidIndex,
                                                       calcChillerAbsorption + this->Name);
                EnthSteamOutWet = GetSatEnthalpyRefrig(fluidNameSteam,
                                                       Node(GeneratorInletNode).Temp,
                                                       0.0,
                                                       this->SteamFluidIndex,
                                                       calcChillerAbsorption + this->Name);
                SteamDeltaT = this->GeneratorSubcool;
                SteamOutletTemp = Node(GeneratorInletNode).Temp - SteamDeltaT;
                HfgSteam = EnthSteamOutDry - EnthSteamOutWet;
                CpFluid =
                    GetSpecificHeatGlycol(fluidNameWater, SteamOutletTemp, DummyWaterIndex, calcChillerAbsorption + this->Name);
                SteamMassFlowRate = QGenerator / (HfgSteam + CpFluid * SteamDeltaT);
                PlantUtilities::SetComponentFlowRate(SteamMassFlowRate,
                                                     GeneratorInletNode,
                                                     GeneratorOutletNode,
                                                     this->GenLoopNum,
                                                     this->GenLoopSideNum,
                                                     this->GenBranchNum,
                                                     this->GenCompNum);

                if (SteamMassFlowRate <= 0.0) {
                    GenOutletTemp = Node(GeneratorInletNode).Temp;
                    SteamOutletEnthalpy = Node(GeneratorInletNode).Enthalpy;
                } else {
                    GenOutletTemp = Node(GeneratorInletNode).Temp - SteamDeltaT;
                    SteamOutletEnthalpy = GetSatEnthalpyRefrig(
                        fluidNameSteam, GenOutletTemp, 0.0, this->SteamFluidIndex, moduleObjectType + this->Name);
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

        // PURPOSE OF THIS SUBROUTINE: reporting

        // Using/Aliasing
        // using PlantUtilities::SafeCopyPlantNode;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
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
            BLASTAbsorberReport(Num).EvapInletTemp = Node(EvapInletNode).Temp;
            BLASTAbsorberReport(Num).CondInletTemp = Node(CondInletNode).Temp;
            BLASTAbsorberReport(Num).CondOutletTemp = Node(CondOutletNode).Temp;
            BLASTAbsorberReport(Num).EvapOutletTemp = Node(EvapOutletNode).Temp;
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
            Node(EvapOutletNode).Temp = EvapOutletTemp;
            Node(CondOutletNode).Temp = CondOutletTemp;

            BLASTAbsorberReport(Num).PumpingPower = PumpingPower;
            BLASTAbsorberReport(Num).QEvap = QEvaporator;
            BLASTAbsorberReport(Num).QCond = QCondenser;
            BLASTAbsorberReport(Num).QGenerator = QGenerator;
            BLASTAbsorberReport(Num).PumpingEnergy = PumpingEnergy;
            BLASTAbsorberReport(Num).EvapEnergy = EvaporatorEnergy;
            BLASTAbsorberReport(Num).CondEnergy = CondenserEnergy;
            BLASTAbsorberReport(Num).GeneratorEnergy = GeneratorEnergy;
            BLASTAbsorberReport(Num).EvapInletTemp = Node(EvapInletNode).Temp;
            BLASTAbsorberReport(Num).CondInletTemp = Node(CondInletNode).Temp;
            BLASTAbsorberReport(Num).CondOutletTemp = Node(CondOutletNode).Temp;
            BLASTAbsorberReport(Num).EvapOutletTemp = Node(EvapOutletNode).Temp;
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
                Node(GeneratorOutletNode).Temp = GenOutletTemp;
            }
        }
    }

} // namespace ChillerAbsorption

} // namespace EnergyPlus
