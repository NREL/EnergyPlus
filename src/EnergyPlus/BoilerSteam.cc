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
#include <BoilerSteam.hh>
#include <BranchNodeConnections.hh>
#include <DataBranchAirLoopPlant.hh>
#include <DataGlobalConstants.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataSizing.hh>
#include <EMSManager.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <GlobalNames.hh>
#include <InputProcessing/InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <OutputReportPredefined.hh>
#include <PlantUtilities.hh>
#include <ReportSizingManager.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace BoilerSteam {

    // Module containing the routines dealing with the Boilers

    // MODULE INFORMATION:
    //    AUTHOR         Rahul Chillar
    //    DATE WRITTEN   Dec 2004
    //    MODIFIED       na
    //    RE-ENGINEERED  na
    // PURPOSE OF THIS MODULE:
    // Performs steam boiler simulation for plant simulation

    // MODULE VARIABLE DECLARATIONS:
    int NumBoilers(0);                  // Number of boilers
    bool getSteamBoilerInput = true;
    static std::string const FluidNameSteam("STEAM");

    Array1D_bool CheckEquipName;
    Array1D<BoilerSpecs> Boiler; // dimension to number of machines

    void clear_state()
    {
        NumBoilers = 0;
        getSteamBoilerInput = true;
        CheckEquipName.deallocate();
        Boiler.deallocate();
    }

    void SimSteamBoiler(std::string const &EP_UNUSED(BoilerType), // boiler type (used in CASE statement)
                        std::string const &BoilerName,            // boiler identifier
                        int const EquipFlowCtrl,                  // Flow control mode for the equipment
                        int &CompIndex,                           // boiler counter/identifier
                        bool const RunFlag,                       // if TRUE run boiler simulation--boiler is ON
                        bool const FirstHVACIteration,            // TRUE if First iteration of simulation
                        bool &InitLoopEquip,                      // If not zero, calculate the max load for operating conditions
                        Real64 &MyLoad,                           // W - Actual demand boiler must satisfy--calculated by load dist. routine
                        Real64 &MaxCap,                           // W - maximum boiler operating capacity
                        Real64 &MinCap,                           // W - minimum boiler operating capacity
                        Real64 &OptCap,                           // W - optimal boiler operating capacity
                        bool const GetSizingFactor,               // TRUE when just the sizing factor is requested
                        Real64 &SizingFactor                      // sizing factor
    )
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rahul Chillar
        //       DATE WRITTEN
        //       MODIFIED
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine controls the boiler component simulation

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int BoilerNum;              // boiler counter/identifier

        // Get Input
        if (getSteamBoilerInput) {
            GetBoilerInput();
            getSteamBoilerInput = false;
        }

        // Find the correct Equipment
        if (CompIndex == 0) {
            BoilerNum = UtilityRoutines::FindItemInList(BoilerName, Boiler);
            if (BoilerNum == 0) {
                ShowFatalError("SimBoiler: Unit not found=" + BoilerName);
            }
            CompIndex = BoilerNum;
        } else {
            BoilerNum = CompIndex;
            if (BoilerNum > NumBoilers || BoilerNum < 1) {
                ShowFatalError("SimBoiler:  Invalid CompIndex passed=" + General::TrimSigDigits(BoilerNum) + ", Number of Units=" + General::TrimSigDigits(NumBoilers) +
                               ", Entered Unit name=" + BoilerName);
            }
            if (CheckEquipName(BoilerNum)) {
                if (BoilerName != Boiler(BoilerNum).Name) {
                    ShowFatalError("SimBoiler: Invalid CompIndex passed=" + General::TrimSigDigits(BoilerNum) + ", Unit name=" + BoilerName +
                                   ", stored Unit Name for that index=" + Boiler(BoilerNum).Name);
                }
                CheckEquipName(BoilerNum) = false;
            }
        }

        // Initialize Loop Equipment
        if (InitLoopEquip) {
            InitBoiler(BoilerNum);
            SizeBoiler(BoilerNum);
            MinCap = Boiler(BoilerNum).NomCap * Boiler(BoilerNum).MinPartLoadRat;
            MaxCap = Boiler(BoilerNum).NomCap * Boiler(BoilerNum).MaxPartLoadRat;
            OptCap = Boiler(BoilerNum).NomCap * Boiler(BoilerNum).OptPartLoadRat;
            if (GetSizingFactor) {
                SizingFactor = Boiler(BoilerNum).SizFac;
            }
            return;
        }

        // Calculate Load
        // Select boiler type and call boiler model
        InitBoiler(BoilerNum);
        CalcBoilerModel(BoilerNum, MyLoad, RunFlag, EquipFlowCtrl);
        UpdateBoilerRecords(MyLoad, RunFlag, BoilerNum, FirstHVACIteration);
    }

    void GetBoilerInput()
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rahul Chillar
        //       DATE WRITTEN   Dec 2004
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Get all boiler data from input file

        // Locals
        static std::string const RoutineName("GetBoilerInput: ");

        // LOCAL VARIABLES
        int BoilerNum;       // boiler identifier
        int NumAlphas;       // Number of elements in the alpha array
        int NumNums;         // Number of elements in the numeric array
        int IOStat;          // IO Status when calling get input subroutine
        int SteamFluidIndex; // Fluid Index for Steam
        bool ErrorsFound(false);
        Array1D_string BoilerFuelTypeForOutputVariable; // used to set up report variables

        SteamFluidIndex = 0;
        DataIPShortCuts::cCurrentModuleObject = "Boiler:Steam";
        NumBoilers = inputProcessor->getNumObjectsFound(DataIPShortCuts::cCurrentModuleObject);

        if (NumBoilers <= 0) {
            ShowSevereError("No " + DataIPShortCuts::cCurrentModuleObject + " equipment specified in input file");
            ErrorsFound = true;
        }

        // See if load distribution manager has already gotten the input
        if (allocated(Boiler)) return;

        // Boiler will have fuel input to it , that is it !
        Boiler.allocate(NumBoilers);
        CheckEquipName.dimension(NumBoilers, true);
        BoilerFuelTypeForOutputVariable.allocate(NumBoilers);

        // LOAD ARRAYS WITH CURVE FIT Boiler DATA
        for (BoilerNum = 1; BoilerNum <= NumBoilers; ++BoilerNum) {
            inputProcessor->getObjectItem(
                DataIPShortCuts::cCurrentModuleObject, BoilerNum, DataIPShortCuts::cAlphaArgs, NumAlphas, DataIPShortCuts::rNumericArgs, NumNums, IOStat, _, _, DataIPShortCuts::cAlphaFieldNames, DataIPShortCuts::cNumericFieldNames);
            UtilityRoutines::IsNameEmpty(DataIPShortCuts::cAlphaArgs(1), DataIPShortCuts::cCurrentModuleObject, ErrorsFound);
            // ErrorsFound will be set to True if problem was found, left untouched otherwise
            GlobalNames::VerifyUniqueBoilerName(DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), ErrorsFound, DataIPShortCuts::cCurrentModuleObject + " Name");
            Boiler(BoilerNum).Name = DataIPShortCuts::cAlphaArgs(1);

            {
                auto const SELECT_CASE_var(DataIPShortCuts::cAlphaArgs(2));

                if ((SELECT_CASE_var == "ELECTRICITY") || (SELECT_CASE_var == "ELECTRIC") || (SELECT_CASE_var == "ELEC")) {
                    BoilerFuelTypeForOutputVariable(BoilerNum) = "Electric";
                    Boiler(BoilerNum).FuelType = DataGlobalConstants::AssignResourceTypeNum("ELECTRICITY");

                } else if ((SELECT_CASE_var == "GAS") || (SELECT_CASE_var == "NATURALGAS") || (SELECT_CASE_var == "NATURAL GAS")) {
                    BoilerFuelTypeForOutputVariable(BoilerNum) = "Gas";
                    Boiler(BoilerNum).FuelType = DataGlobalConstants::AssignResourceTypeNum("NATURALGAS");

                } else if (SELECT_CASE_var == "DIESEL") {
                    BoilerFuelTypeForOutputVariable(BoilerNum) = "Diesel";
                    Boiler(BoilerNum).FuelType = DataGlobalConstants::AssignResourceTypeNum("DIESEL");

                } else if (SELECT_CASE_var == "GASOLINE") {
                    BoilerFuelTypeForOutputVariable(BoilerNum) = "Gasoline";
                    Boiler(BoilerNum).FuelType = DataGlobalConstants::AssignResourceTypeNum("GASOLINE");

                } else if (SELECT_CASE_var == "COAL") {
                    BoilerFuelTypeForOutputVariable(BoilerNum) = "Coal";
                    Boiler(BoilerNum).FuelType = DataGlobalConstants::AssignResourceTypeNum("COAL");

                } else if ((SELECT_CASE_var == "FUEL OIL #1") || (SELECT_CASE_var == "FUELOIL#1") || (SELECT_CASE_var == "FUEL OIL") ||
                           (SELECT_CASE_var == "DISTILLATE OIL")) {
                    BoilerFuelTypeForOutputVariable(BoilerNum) = "FuelOil#1";
                    Boiler(BoilerNum).FuelType = DataGlobalConstants::AssignResourceTypeNum("DISTILLATE OIL");

                } else if ((SELECT_CASE_var == "FUEL OIL #2") || (SELECT_CASE_var == "FUELOIL#2") || (SELECT_CASE_var == "RESIDUAL OIL")) {
                    BoilerFuelTypeForOutputVariable(BoilerNum) = "FuelOil#2";
                    Boiler(BoilerNum).FuelType = DataGlobalConstants::AssignResourceTypeNum("RESIDUAL OIL");

                } else if ((SELECT_CASE_var == "PROPANE") || (SELECT_CASE_var == "LPG") || (SELECT_CASE_var == "PROPANEGAS") ||
                           (SELECT_CASE_var == "PROPANE GAS")) {
                    BoilerFuelTypeForOutputVariable(BoilerNum) = "Propane";
                    Boiler(BoilerNum).FuelType = DataGlobalConstants::AssignResourceTypeNum("PROPANE");

                } else if (SELECT_CASE_var == "OTHERFUEL1") {
                    BoilerFuelTypeForOutputVariable(BoilerNum) = "OtherFuel1";
                    Boiler(BoilerNum).FuelType = DataGlobalConstants::AssignResourceTypeNum("OTHERFUEL1");

                } else if (SELECT_CASE_var == "OTHERFUEL2") {
                    BoilerFuelTypeForOutputVariable(BoilerNum) = "OtherFuel2";
                    Boiler(BoilerNum).FuelType = DataGlobalConstants::AssignResourceTypeNum("OTHERFUEL2");

                } else {
                    ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\",");
                    ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(2) + '=' + DataIPShortCuts::cAlphaArgs(2));

                    // Set to Electric to avoid errors when setting up output variables
                    BoilerFuelTypeForOutputVariable(BoilerNum) = "Electric";
                    ErrorsFound = true;
                }
            }

            // INPUTS from the IDF file
            Boiler(BoilerNum).BoilerMaxOperPress = DataIPShortCuts::rNumericArgs(1);
            if (Boiler(BoilerNum).BoilerMaxOperPress < 1e5) {
                ShowWarningMessage(DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\"");
                ShowContinueError("Field: Maximum Operation Pressure units are Pa. Verify units.");
            }
            Boiler(BoilerNum).Effic = DataIPShortCuts::rNumericArgs(2);
            Boiler(BoilerNum).TempUpLimitBoilerOut = DataIPShortCuts::rNumericArgs(3);
            Boiler(BoilerNum).NomCap = DataIPShortCuts::rNumericArgs(4);
            if (Boiler(BoilerNum).NomCap == DataSizing::AutoSize) {
                Boiler(BoilerNum).NomCapWasAutoSized = true;
            }
            Boiler(BoilerNum).MinPartLoadRat = DataIPShortCuts::rNumericArgs(5);
            Boiler(BoilerNum).MaxPartLoadRat = DataIPShortCuts::rNumericArgs(6);
            Boiler(BoilerNum).OptPartLoadRat = DataIPShortCuts::rNumericArgs(7);
            Boiler(BoilerNum).FullLoadCoef(1) = DataIPShortCuts::rNumericArgs(8);
            Boiler(BoilerNum).FullLoadCoef(2) = DataIPShortCuts::rNumericArgs(9);
            Boiler(BoilerNum).FullLoadCoef(3) = DataIPShortCuts::rNumericArgs(10);
            Boiler(BoilerNum).SizFac = DataIPShortCuts::rNumericArgs(11);
            if (Boiler(BoilerNum).SizFac <= 0.0) Boiler(BoilerNum).SizFac = 1.0;

            if ((DataIPShortCuts::rNumericArgs(8) + DataIPShortCuts::rNumericArgs(9) + DataIPShortCuts::rNumericArgs(10)) == 0.0) {
                ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\",");
                ShowContinueError(" Sum of fuel use curve coefficients = 0.0");
                ErrorsFound = true;
            }

            if (DataIPShortCuts::rNumericArgs(5) < 0.0) {
                ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\",");
                ShowContinueError("Invalid " + DataIPShortCuts::cNumericFieldNames(5) + '=' + General::RoundSigDigits(DataIPShortCuts::rNumericArgs(5), 3));
                ErrorsFound = true;
            }

            if (DataIPShortCuts::rNumericArgs(3) == 0.0) {
                ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\",");
                ShowContinueError("Invalid " + DataIPShortCuts::cNumericFieldNames(3) + '=' + General::RoundSigDigits(DataIPShortCuts::rNumericArgs(3), 3));
                ErrorsFound = true;
            }
            Boiler(BoilerNum).BoilerInletNodeNum = NodeInputManager::GetOnlySingleNode(
                DataIPShortCuts::cAlphaArgs(3), ErrorsFound, DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataLoopNode::NodeType_Steam, DataLoopNode::NodeConnectionType_Inlet, 1, DataLoopNode::ObjectIsNotParent);
            Boiler(BoilerNum).BoilerOutletNodeNum = NodeInputManager::GetOnlySingleNode(
                DataIPShortCuts::cAlphaArgs(4), ErrorsFound, DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataLoopNode::NodeType_Steam, DataLoopNode::NodeConnectionType_Outlet, 1, DataLoopNode::ObjectIsNotParent);
            BranchNodeConnections::TestCompSet(DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), DataIPShortCuts::cAlphaArgs(3), DataIPShortCuts::cAlphaArgs(4), "Hot Steam Nodes");

            if (SteamFluidIndex == 0 && BoilerNum == 1) {
                SteamFluidIndex = FluidProperties::FindRefrigerant("Steam");
                if (SteamFluidIndex == 0) {
                    ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\",");
                    ShowContinueError("Steam Properties not found; Steam Fluid Properties must be included in the input file.");
                    ErrorsFound = true;
                }
            }

            Boiler(BoilerNum).FluidIndex = SteamFluidIndex;

            if (NumAlphas > 4) {
                Boiler(BoilerNum).EndUseSubcategory = DataIPShortCuts::cAlphaArgs(5);
            } else {
                Boiler(BoilerNum).EndUseSubcategory = "General";
            }
        }

        if (ErrorsFound) {
            ShowFatalError(RoutineName + "Errors found in processing " + DataIPShortCuts::cCurrentModuleObject + " input.");
        }

        for (BoilerNum = 1; BoilerNum <= NumBoilers; ++BoilerNum) {
            SetupOutputVariable(
                "Boiler Heating Rate", OutputProcessor::Unit::W, Boiler(BoilerNum).BoilerLoad, "System", "Average", Boiler(BoilerNum).Name);
            SetupOutputVariable("Boiler Heating Energy",
                                OutputProcessor::Unit::J,
                                Boiler(BoilerNum).BoilerEnergy,
                                "System",
                                "Sum",
                                Boiler(BoilerNum).Name,
                                _,
                                "ENERGYTRANSFER",
                                "BOILERS",
                                _,
                                "Plant");
            if (UtilityRoutines::SameString(BoilerFuelTypeForOutputVariable(BoilerNum), "Electric")) {
                SetupOutputVariable("Boiler " + BoilerFuelTypeForOutputVariable(BoilerNum) + " Power",
                                    OutputProcessor::Unit::W,
                                    Boiler(BoilerNum).FuelUsed,
                                    "System",
                                    "Average",
                                    Boiler(BoilerNum).Name);
            } else {
                SetupOutputVariable("Boiler " + BoilerFuelTypeForOutputVariable(BoilerNum) + " Rate",
                                    OutputProcessor::Unit::W,
                                    Boiler(BoilerNum).FuelUsed,
                                    "System",
                                    "Average",
                                    Boiler(BoilerNum).Name);
            }
            SetupOutputVariable("Boiler " + BoilerFuelTypeForOutputVariable(BoilerNum) + " Energy",
                                OutputProcessor::Unit::J,
                                Boiler(BoilerNum).FuelConsumed,
                                "System",
                                "Sum",
                                Boiler(BoilerNum).Name,
                                _,
                                BoilerFuelTypeForOutputVariable(BoilerNum),
                                "Heating",
                                Boiler(BoilerNum).EndUseSubcategory,
                                "Plant");
            SetupOutputVariable("Boiler Steam Inlet Temperature",
                                OutputProcessor::Unit::C,
                                Boiler(BoilerNum).BoilerInletTemp,
                                "System",
                                "Average",
                                Boiler(BoilerNum).Name);
            SetupOutputVariable("Boiler Steam Outlet Temperature",
                                OutputProcessor::Unit::C,
                                Boiler(BoilerNum).BoilerOutletTemp,
                                "System",
                                "Average",
                                Boiler(BoilerNum).Name);
            SetupOutputVariable("Boiler Steam Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                Boiler(BoilerNum).BoilerMassFlowRate,
                                "System",
                                "Average",
                                Boiler(BoilerNum).Name);
        }

        BoilerFuelTypeForOutputVariable.deallocate();
    }

    void InitBoiler(int const BoilerNum) // number of the current electric chiller being simulated
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rahul Chillar
        //       DATE WRITTEN   Dec 2004
        //       MODIFIED       na
        //       RE-ENGINEERED  D. Shirey, rework for plant upgrade

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for initializations of the Boiler components

        // METHODOLOGY EMPLOYED:
        // Uses the status flags to trigger initializations.

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("InitBoiler");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        bool FatalError;
        Real64 TempUpLimitBoilerOut; // C - Boiler outlet maximum temperature limit
        Real64 EnthSteamOutWet;
        Real64 EnthSteamOutDry;
        Real64 LatentEnthSteam;
        Real64 CpWater;       // Heat capacity of condensed steam (liquid)
        int BoilerInletNode;  // Boiler inlet node number
        int BoilerOutletNode; // Boiler outlet node number
        bool errFlag;

        // Init more variables
        if (Boiler(BoilerNum).myFlag) {
            // Locate the chillers on the plant loops for later usage
            errFlag = false;
            PlantUtilities::ScanPlantLoopsForObject(Boiler(BoilerNum).Name,
                                    DataPlant::TypeOf_Boiler_Steam,
                                    Boiler(BoilerNum).LoopNum,
                                    Boiler(BoilerNum).LoopSideNum,
                                    Boiler(BoilerNum).BranchNum,
                                    Boiler(BoilerNum).CompNum,
                                    errFlag,
                                    _,
                                    _,
                                    _,
                                    _,
                                    _);
            if (errFlag) {
                ShowFatalError("InitBoiler: Program terminated due to previous condition(s).");
            }

            Boiler(BoilerNum).myFlag = false;
        }

        BoilerInletNode = Boiler(BoilerNum).BoilerInletNodeNum;
        BoilerOutletNode = Boiler(BoilerNum).BoilerOutletNodeNum;

        if (DataGlobals::BeginEnvrnFlag && Boiler(BoilerNum).myEnvrnFlag && (DataPlant::PlantFirstSizesOkayToFinalize)) {

            // BoilerOutletTemp     = Node(BoilerOutletNode)%TempSetPoint
            // TempUpLimitBoilerOut =Boiler(BoilerNum)%TempUpLimitBoilerOut
            //      TempUpLimitBoilerOut = Node(BoilerOutletNode)%TempSetPoint
            TempUpLimitBoilerOut = Boiler(BoilerNum).TempUpLimitBoilerOut; // Design Outlet Steam Temperature
            EnthSteamOutDry = FluidProperties::GetSatEnthalpyRefrig(FluidNameSteam, TempUpLimitBoilerOut, 1.0, Boiler(BoilerNum).FluidIndex, RoutineName);
            EnthSteamOutWet = FluidProperties::GetSatEnthalpyRefrig(FluidNameSteam, TempUpLimitBoilerOut, 0.0, Boiler(BoilerNum).FluidIndex, RoutineName);
            LatentEnthSteam = EnthSteamOutDry - EnthSteamOutWet;

            CpWater = FluidProperties::GetSatSpecificHeatRefrig(FluidNameSteam, TempUpLimitBoilerOut, 0.0, Boiler(BoilerNum).FluidIndex, RoutineName);

            Boiler(BoilerNum).DesMassFlowRate =
                Boiler(BoilerNum).NomCap / (LatentEnthSteam + CpWater * (TempUpLimitBoilerOut - DataLoopNode::Node(BoilerInletNode).Temp));

            PlantUtilities::InitComponentNodes(0.0,
                               Boiler(BoilerNum).DesMassFlowRate,
                               Boiler(BoilerNum).BoilerInletNodeNum,
                               Boiler(BoilerNum).BoilerOutletNodeNum,
                               Boiler(BoilerNum).LoopNum,
                               Boiler(BoilerNum).LoopSideNum,
                               Boiler(BoilerNum).BranchNum,
                               Boiler(BoilerNum).CompNum);

            Boiler(BoilerNum).BoilerPressCheck = 0.0;
            Boiler(BoilerNum).FuelUsed = 0.0;
            Boiler(BoilerNum).BoilerLoad = 0.0;
            Boiler(BoilerNum).BoilerOutletTemp = 0.0;
            Boiler(BoilerNum).BoilerMaxPress = 0.0;

            if ((DataLoopNode::Node(Boiler(BoilerNum).BoilerOutletNodeNum).TempSetPoint == DataLoopNode::SensedNodeFlagValue) &&
                (DataLoopNode::Node(Boiler(BoilerNum).BoilerOutletNodeNum).TempSetPointLo == DataLoopNode::SensedNodeFlagValue)) {
                if (!DataGlobals::AnyEnergyManagementSystemInModel) {
                    if (!Boiler(BoilerNum).MissingSetPointErrDone) {
                        ShowWarningError("Missing temperature setpoint for Boiler:Steam = " + Boiler(BoilerNum).Name);
                        ShowContinueError(" A temperature setpoint is needed at the outlet node of the boiler, use a SetpointManager");
                        ShowContinueError(" The overall loop setpoint will be assumed for this boiler. The simulation continues ...");
                        Boiler(BoilerNum).MissingSetPointErrDone = true;
                    }
                } else {
                    // need call to EMS to check node
                    FatalError = false; // but not really fatal yet, but should be.
                    EMSManager::CheckIfNodeSetPointManagedByEMS(Boiler(BoilerNum).BoilerOutletNodeNum, EMSManager::iTemperatureSetPoint, FatalError);
                    if (FatalError) {
                        if (!Boiler(BoilerNum).MissingSetPointErrDone) {
                            ShowWarningError("Missing temperature setpoint for VariableFlow mode Boiler named " + Boiler(BoilerNum).Name);
                            ShowContinueError(" A temperature setpoint is needed at the outlet node of the boiler.");
                            ShowContinueError(" Use a Setpoint Manager to establish a setpoint at the boiler outlet node ");
                            ShowContinueError(" or use an EMS actuator to establish a setpoint at the boiler outlet node.");
                            ShowContinueError(" The overall loop setpoint will be assumed for this boiler. The simulation continues...");
                            Boiler(BoilerNum).MissingSetPointErrDone = true;
                        }
                    }
                }
                Boiler(BoilerNum).UseLoopSetPoint = true; // this is for backward compatibility and could be removed
            }

            Boiler(BoilerNum).myEnvrnFlag = false;

        } // End If for the Begin Environment initializations

        if (!DataGlobals::BeginEnvrnFlag) {
            Boiler(BoilerNum).myEnvrnFlag = true;
        }

        if (Boiler(BoilerNum).UseLoopSetPoint) {
            //  At some point, need to circle back and get from plant data structure instead of node
            // fix for clumsy old input that worked because loop setpoint was spread.
            //  could be removed with transition, testing , model change, period of being obsolete.
            {
                auto const SELECT_CASE_var(DataPlant::PlantLoop(Boiler(BoilerNum).LoopNum).LoopDemandCalcScheme);
                if (SELECT_CASE_var == DataPlant::SingleSetPoint) {
                    DataLoopNode::Node(BoilerOutletNode).TempSetPoint = DataLoopNode::Node(DataPlant::PlantLoop(Boiler(BoilerNum).LoopNum).TempSetPointNodeNum).TempSetPoint;
                } else if (SELECT_CASE_var == DataPlant::DualSetPointDeadBand) {
                    DataLoopNode::Node(BoilerOutletNode).TempSetPointLo = DataLoopNode::Node(DataPlant::PlantLoop(Boiler(BoilerNum).LoopNum).TempSetPointNodeNum).TempSetPointLo;
                }
            }
        }
    }

    void SizeBoiler(int const BoilerNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rahul Chillar
        //       DATE WRITTEN   Dec 2004
        //       MODIFIED       November 2013 Daeho Kang, add component sizing table entries
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for sizing Boiler Components for which capacities and flow rates
        // have not been specified in the input.

        // METHODOLOGY EMPLOYED:
        // Obtains Steam flow rate from the plant sizing array. Calculates nominal capacity from
        // the hot water flow rate and the hot water loop design delta T.

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("SizeBoiler");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int PltSizNum(0);        // Plant Sizing index corresponding to CurLoopNum
        bool ErrorsFound(false); // If errors detected in input
        Real64 SteamDensity;
        Real64 EnthSteamOutWet;
        Real64 EnthSteamOutDry;
        Real64 LatentEnthSteam;
        Real64 SizingTemp;
        Real64 CpWater; // Heat capacity of condensed steam
        std::string equipName;
        Real64 tmpNomCap;       // local nominal capacity cooling power
        Real64 NomCapUser(0.0); // Hardsized nominal capacity for reporting

        tmpNomCap = Boiler(BoilerNum).NomCap;

        // Find the appropriate Plant Sizing object
        PltSizNum = DataPlant::PlantLoop(Boiler(BoilerNum).LoopNum).PlantSizNum;

        if (PltSizNum > 0) {
            if (DataSizing::PlantSizData(PltSizNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                SizingTemp = Boiler(BoilerNum).TempUpLimitBoilerOut;
                SteamDensity = FluidProperties::GetSatDensityRefrig(FluidNameSteam, SizingTemp, 1.0, Boiler(BoilerNum).FluidIndex, RoutineName);
                EnthSteamOutDry = FluidProperties::GetSatEnthalpyRefrig(FluidNameSteam, SizingTemp, 1.0, Boiler(BoilerNum).FluidIndex, RoutineName);
                EnthSteamOutWet = FluidProperties::GetSatEnthalpyRefrig(FluidNameSteam, SizingTemp, 0.0, Boiler(BoilerNum).FluidIndex, RoutineName);
                LatentEnthSteam = EnthSteamOutDry - EnthSteamOutWet;
                CpWater = FluidProperties::GetSatSpecificHeatRefrig(FluidNameSteam, SizingTemp, 0.0, Boiler(BoilerNum).FluidIndex, RoutineName);
                tmpNomCap =
                    (CpWater * SteamDensity * Boiler(BoilerNum).SizFac * DataSizing::PlantSizData(PltSizNum).DeltaT * DataSizing::PlantSizData(PltSizNum).DesVolFlowRate +
                        DataSizing::PlantSizData(PltSizNum).DesVolFlowRate * SteamDensity * LatentEnthSteam);
            } else {
                if (Boiler(BoilerNum).NomCapWasAutoSized) tmpNomCap = 0.0;
            }
            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                if (Boiler(BoilerNum).NomCapWasAutoSized) {
                    Boiler(BoilerNum).NomCap = tmpNomCap;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput("Boiler:Steam", Boiler(BoilerNum).Name, "Design Size Nominal Capacity [W]", tmpNomCap);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput("Boiler:Steam", Boiler(BoilerNum).Name, "Initial Design Size Nominal Capacity [W]", tmpNomCap);
                    }
                } else { // Hard-sized with sizing data
                    if (Boiler(BoilerNum).NomCap > 0.0 && tmpNomCap > 0.0) {
                        NomCapUser = Boiler(BoilerNum).NomCap;
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput("Boiler:Steam",
                                               Boiler(BoilerNum).Name,
                                               "Design Size Nominal Capacity [W]",
                                               tmpNomCap,
                                               "User-Specified Nominal Capacity [W]",
                                               NomCapUser);
                            if (DataGlobals::DisplayExtraWarnings) {
                                if ((std::abs(tmpNomCap - NomCapUser) / NomCapUser) > DataSizing::AutoVsHardSizingThreshold) {
                                    ShowMessage("SizePump: Potential issue with equipment sizing for " + Boiler(BoilerNum).Name);
                                    ShowContinueError("User-Specified Nominal Capacity of " + General::RoundSigDigits(NomCapUser, 2) + " [W]");
                                    ShowContinueError("differs from Design Size Nominal Capacity of " + General::RoundSigDigits(tmpNomCap, 2) + " [W]");
                                    ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                        }
                    }
                }
            }
        } else {
            if (Boiler(BoilerNum).NomCapWasAutoSized && DataPlant::PlantFirstSizesOkayToFinalize) {
                ShowSevereError("Autosizing of Boiler nominal capacity requires a loop Sizing:Plant object");
                ShowContinueError("Occurs in Boiler:Steam object=" + Boiler(BoilerNum).Name);
                ErrorsFound = true;
            }
            if (!Boiler(BoilerNum).NomCapWasAutoSized && Boiler(BoilerNum).NomCap > 0.0 && DataPlant::PlantFinalSizesOkayToReport) {
                ReportSizingManager::ReportSizingOutput("Boiler:Steam", Boiler(BoilerNum).Name, "User-Specified Nominal Capacity [W]", Boiler(BoilerNum).NomCap);
            }
        }

        if (DataPlant::PlantFinalSizesOkayToReport) {
            // create predefined report
            equipName = Boiler(BoilerNum).Name;
            OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchMechType, equipName, "Boiler:Steam");
            OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchMechNomEff, equipName, Boiler(BoilerNum).Effic);
            OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchMechNomCap, equipName, Boiler(BoilerNum).NomCap);
        }

        if (ErrorsFound) {
            ShowFatalError("Preceding sizing errors cause program termination");
        }
    }

    void CalcBoilerModel(int &BoilerNum,         // boiler identifier
                         Real64 &MyLoad,         // W - hot water demand to be met by boiler
                         bool const RunFlag,     // TRUE if boiler operating
                         int const EquipFlowCtrl // Flow control mode for the equipment
    )
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rahul Chillar
        //       DATE WRITTEN   Dec 2004
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine calculates the boiler fuel consumption and the associated
        // hot water demand met by the boiler

        // METHODOLOGY EMPLOYED:
        // The model is based on a single combustion efficiency (=1 for electric)
        // and a second order polynomial fit of performance data to obtain part
        // load performance

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("CalcBoilerModel");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 BoilerEff;             // boiler efficiency
        Real64 BoilerNomCap;          // W - boiler nominal capacity
        Real64 BoilerMaxPLR;          // boiler maximum part load ratio
        Real64 BoilerMinPLR;          // boiler minimum part load ratio
        Real64 TheorFuelUse;          // Theoretical (stoichiometric) fuel use
        Real64 OperPLR;               // operating part load ratio
        Real64 BoilerDeltaTemp(0.0);  // C - boiler inlet to outlet temperature difference
        Real64 TempUpLimitBout;       // C - boiler high temperature limit
        Real64 EnthSteamOutDry;
        Real64 EnthSteamOutWet;
        Real64 LatentEnthSteam;
        Array1D<Real64> LoadCoef(3); // coefficients of the fuel use/part load curve
        Real64 CpWater;              // Heat capacity of condensed steam
        int BoilerInletNode;         // Boiler inlet node number
        int BoilerOutletNode;        // Boiler outlet node number
        int LoopNum;
        int LoopSideNum;

        // Loading the variables derived type in to local variables
        Boiler(BoilerNum).BoilerLoad = 0.0;
        Boiler(BoilerNum).BoilerMassFlowRate = 0.0;
        BoilerInletNode = Boiler(BoilerNum).BoilerInletNodeNum;
        BoilerOutletNode = Boiler(BoilerNum).BoilerOutletNodeNum;
        BoilerNomCap = Boiler(BoilerNum).NomCap;
        BoilerMaxPLR = Boiler(BoilerNum).MaxPartLoadRat;
        BoilerMinPLR = Boiler(BoilerNum).MinPartLoadRat;
        LoadCoef = Boiler(BoilerNum).FullLoadCoef;
        TempUpLimitBout = Boiler(BoilerNum).TempUpLimitBoilerOut;
        Boiler(BoilerNum).BoilerMaxPress = Boiler(BoilerNum).BoilerMaxOperPress;
        BoilerEff = Boiler(BoilerNum).Effic;

        LoopNum = Boiler(BoilerNum).LoopNum;
        LoopSideNum = Boiler(BoilerNum).LoopSideNum;
        {
            auto const SELECT_CASE_var(DataPlant::PlantLoop(LoopNum).LoopDemandCalcScheme);
            if (SELECT_CASE_var == DataPlant::SingleSetPoint) {
                Boiler(BoilerNum).BoilerOutletTemp = DataLoopNode::Node(BoilerOutletNode).TempSetPoint;
            } else if (SELECT_CASE_var == DataPlant::DualSetPointDeadBand) {
                Boiler(BoilerNum).BoilerOutletTemp = DataLoopNode::Node(BoilerOutletNode).TempSetPointLo;
            }
        }
        // If the specified load is 0.0 or the boiler should not run then we leave this subroutine.Before leaving
        // if the component control is SERIESACTIVE we set the component flow to inlet flow so that flow resolver
        // will not shut down the branch
        if (MyLoad <= 0.0 || !RunFlag) {
            if (EquipFlowCtrl == DataBranchAirLoopPlant::ControlType_SeriesActive) Boiler(BoilerNum).BoilerMassFlowRate = DataLoopNode::Node(BoilerInletNode).MassFlowRate;
            return;
        }

        // Set the current load equal to the boiler load
        Boiler(BoilerNum).BoilerLoad = MyLoad;

        Boiler(BoilerNum).BoilerPressCheck = FluidProperties::GetSatPressureRefrig(FluidNameSteam, Boiler(BoilerNum).BoilerOutletTemp, Boiler(BoilerNum).FluidIndex, RoutineName);

        if ((Boiler(BoilerNum).BoilerPressCheck) > Boiler(BoilerNum).BoilerMaxPress) {
            if (Boiler(BoilerNum).PressErrIndex == 0) {
                ShowSevereError("Boiler:Steam=\"" + Boiler(BoilerNum).Name + "\", Saturation Pressure is greater than Maximum Operating Pressure,");
                ShowContinueError("Lower Input Temperature");
                ShowContinueError("Steam temperature=[" + General::RoundSigDigits(Boiler(BoilerNum).BoilerOutletTemp, 2) + "] C");
                ShowContinueError("Refrigerant Saturation Pressure =[" + General::RoundSigDigits(Boiler(BoilerNum).BoilerPressCheck, 0) + "] Pa");
            }
            ShowRecurringSevereErrorAtEnd("Boiler:Steam=\"" + Boiler(BoilerNum).Name +
                                              "\", Saturation Pressure is greater than Maximum Operating Pressure..continues",
                                          Boiler(BoilerNum).PressErrIndex,
                                          Boiler(BoilerNum).BoilerPressCheck,
                                          Boiler(BoilerNum).BoilerPressCheck,
                                          _,
                                          "[Pa]",
                                          "[Pa]");
        }

        CpWater = FluidProperties::GetSatSpecificHeatRefrig(FluidNameSteam, DataLoopNode::Node(BoilerInletNode).Temp, 0.0, Boiler(BoilerNum).FluidIndex, RoutineName);

        if (DataPlant::PlantLoop(LoopNum).LoopSide(LoopSideNum).FlowLock == 0) {
            // Calculate the flow for the boiler

            {
                auto const SELECT_CASE_var(DataPlant::PlantLoop(Boiler(BoilerNum).LoopNum).LoopDemandCalcScheme);
                if (SELECT_CASE_var == DataPlant::SingleSetPoint) {
                    BoilerDeltaTemp = DataLoopNode::Node(BoilerOutletNode).TempSetPoint - DataLoopNode::Node(BoilerInletNode).Temp;
                } else if (SELECT_CASE_var == DataPlant::DualSetPointDeadBand) {
                    BoilerDeltaTemp = DataLoopNode::Node(BoilerOutletNode).TempSetPointLo - DataLoopNode::Node(BoilerInletNode).Temp;
                } else {
                    assert(false);
                }
            }
            Boiler(BoilerNum).BoilerOutletTemp = BoilerDeltaTemp + DataLoopNode::Node(BoilerInletNode).Temp;

            EnthSteamOutDry = FluidProperties::GetSatEnthalpyRefrig(FluidNameSteam, Boiler(BoilerNum).BoilerOutletTemp, 1.0, Boiler(BoilerNum).FluidIndex, RoutineName);

            EnthSteamOutWet = FluidProperties::GetSatEnthalpyRefrig(FluidNameSteam, Boiler(BoilerNum).BoilerOutletTemp, 0.0, Boiler(BoilerNum).FluidIndex, RoutineName);

            LatentEnthSteam = EnthSteamOutDry - EnthSteamOutWet;

            Boiler(BoilerNum).BoilerMassFlowRate = Boiler(BoilerNum).BoilerLoad / (LatentEnthSteam + (CpWater * BoilerDeltaTemp));

            PlantUtilities::SetComponentFlowRate(Boiler(BoilerNum).BoilerMassFlowRate,
                                 BoilerInletNode,
                                 BoilerOutletNode,
                                 Boiler(BoilerNum).LoopNum,
                                 Boiler(BoilerNum).LoopSideNum,
                                 Boiler(BoilerNum).BranchNum,
                                 Boiler(BoilerNum).CompNum);

        } else { // If FlowLock is True
            // Set the boiler flow rate from inlet node and then check performance
            Boiler(BoilerNum).BoilerMassFlowRate = DataLoopNode::Node(BoilerInletNode).MassFlowRate;
            // Assume that it can meet the setpoint
            {
                auto const SELECT_CASE_var(DataPlant::PlantLoop(Boiler(BoilerNum).LoopNum).LoopDemandCalcScheme);
                if (SELECT_CASE_var == DataPlant::SingleSetPoint) {
                    BoilerDeltaTemp = DataLoopNode::Node(BoilerOutletNode).TempSetPoint - DataLoopNode::Node(BoilerInletNode).Temp;
                } else if (SELECT_CASE_var == DataPlant::DualSetPointDeadBand) {
                    BoilerDeltaTemp = DataLoopNode::Node(BoilerOutletNode).TempSetPointLo - DataLoopNode::Node(BoilerInletNode).Temp;
                }
            }
            // If boiler outlet temp is already greater than setpoint than it does not need to operate this iteration
            if (BoilerDeltaTemp < 0.0) {
                {
                    auto const SELECT_CASE_var(DataPlant::PlantLoop(Boiler(BoilerNum).LoopNum).LoopDemandCalcScheme);
                    if (SELECT_CASE_var == DataPlant::SingleSetPoint) {
                        Boiler(BoilerNum).BoilerOutletTemp = DataLoopNode::Node(BoilerOutletNode).TempSetPoint;
                    } else if (SELECT_CASE_var == DataPlant::DualSetPointDeadBand) {
                        Boiler(BoilerNum).BoilerOutletTemp = DataLoopNode::Node(BoilerOutletNode).TempSetPointLo;
                    }
                }
                EnthSteamOutDry = FluidProperties::GetSatEnthalpyRefrig(FluidNameSteam, Boiler(BoilerNum).BoilerOutletTemp, 1.0, Boiler(BoilerNum).FluidIndex, RoutineName);
                EnthSteamOutWet = FluidProperties::GetSatEnthalpyRefrig(FluidNameSteam, Boiler(BoilerNum).BoilerOutletTemp, 0.0, Boiler(BoilerNum).FluidIndex, RoutineName);

                LatentEnthSteam = EnthSteamOutDry - EnthSteamOutWet;

                Boiler(BoilerNum).BoilerLoad = (Boiler(BoilerNum).BoilerMassFlowRate * LatentEnthSteam);

            } else {

                {
                    auto const SELECT_CASE_var(DataPlant::PlantLoop(Boiler(BoilerNum).LoopNum).LoopDemandCalcScheme);
                    if (SELECT_CASE_var == DataPlant::SingleSetPoint) {
                        Boiler(BoilerNum).BoilerOutletTemp = DataLoopNode::Node(BoilerOutletNode).TempSetPoint;
                    } else if (SELECT_CASE_var == DataPlant::DualSetPointDeadBand) {
                        Boiler(BoilerNum).BoilerOutletTemp = DataLoopNode::Node(BoilerOutletNode).TempSetPointLo;
                    }
                }

                EnthSteamOutDry = FluidProperties::GetSatEnthalpyRefrig(FluidNameSteam, Boiler(BoilerNum).BoilerOutletTemp, 1.0, Boiler(BoilerNum).FluidIndex, RoutineName);
                EnthSteamOutWet = FluidProperties::GetSatEnthalpyRefrig(FluidNameSteam, Boiler(BoilerNum).BoilerOutletTemp, 0.0, Boiler(BoilerNum).FluidIndex, RoutineName);

                LatentEnthSteam = EnthSteamOutDry - EnthSteamOutWet;

                // Calculate the boiler load with the specified flow rate.
                Boiler(BoilerNum).BoilerLoad = std::abs(Boiler(BoilerNum).BoilerMassFlowRate * LatentEnthSteam) + std::abs(Boiler(BoilerNum).BoilerMassFlowRate * CpWater * BoilerDeltaTemp);
            }

            // If load exceeds the distributed load set to the distributed load
            if (Boiler(BoilerNum).BoilerLoad > MyLoad) {
                Boiler(BoilerNum).BoilerLoad = MyLoad;

                // Reset later , here just for calculating latent heat
                {
                    auto const SELECT_CASE_var(DataPlant::PlantLoop(Boiler(BoilerNum).LoopNum).LoopDemandCalcScheme);
                    if (SELECT_CASE_var == DataPlant::SingleSetPoint) {
                        Boiler(BoilerNum).BoilerOutletTemp = DataLoopNode::Node(BoilerOutletNode).TempSetPoint;
                    } else if (SELECT_CASE_var == DataPlant::DualSetPointDeadBand) {
                        Boiler(BoilerNum).BoilerOutletTemp = DataLoopNode::Node(BoilerOutletNode).TempSetPointLo;
                    }
                }

                EnthSteamOutDry = FluidProperties::GetSatEnthalpyRefrig(FluidNameSteam, Boiler(BoilerNum).BoilerOutletTemp, 1.0, Boiler(BoilerNum).FluidIndex, RoutineName);

                EnthSteamOutWet = FluidProperties::GetSatEnthalpyRefrig(FluidNameSteam, Boiler(BoilerNum).BoilerOutletTemp, 0.0, Boiler(BoilerNum).FluidIndex, RoutineName);

                LatentEnthSteam = EnthSteamOutDry - EnthSteamOutWet;

                BoilerDeltaTemp = Boiler(BoilerNum).BoilerOutletTemp - DataLoopNode::Node(BoilerInletNode).Temp;

                Boiler(BoilerNum).BoilerMassFlowRate = Boiler(BoilerNum).BoilerLoad / (LatentEnthSteam + CpWater * BoilerDeltaTemp);

                PlantUtilities::SetComponentFlowRate(Boiler(BoilerNum).BoilerMassFlowRate,
                                     BoilerInletNode,
                                     BoilerOutletNode,
                                     Boiler(BoilerNum).LoopNum,
                                     Boiler(BoilerNum).LoopSideNum,
                                     Boiler(BoilerNum).BranchNum,
                                     Boiler(BoilerNum).CompNum);
            }

            // Checks Boiler Load on the basis of the machine limits.
            if (Boiler(BoilerNum).BoilerLoad > BoilerNomCap) {
                if (Boiler(BoilerNum).BoilerMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance) {
                    Boiler(BoilerNum).BoilerLoad = BoilerNomCap;

                    EnthSteamOutDry = FluidProperties::GetSatEnthalpyRefrig(FluidNameSteam, Boiler(BoilerNum).BoilerOutletTemp, 1.0, Boiler(BoilerNum).FluidIndex, RoutineName);
                    EnthSteamOutWet = FluidProperties::GetSatEnthalpyRefrig(FluidNameSteam, Boiler(BoilerNum).BoilerOutletTemp, 0.0, Boiler(BoilerNum).FluidIndex, RoutineName);

                    LatentEnthSteam = EnthSteamOutDry - EnthSteamOutWet;

                    BoilerDeltaTemp = Boiler(BoilerNum).BoilerOutletTemp - DataLoopNode::Node(BoilerInletNode).Temp;

                    Boiler(BoilerNum).BoilerMassFlowRate = Boiler(BoilerNum).BoilerLoad / (LatentEnthSteam + CpWater * BoilerDeltaTemp);

                    PlantUtilities::SetComponentFlowRate(Boiler(BoilerNum).BoilerMassFlowRate,
                                         BoilerInletNode,
                                         BoilerOutletNode,
                                         Boiler(BoilerNum).LoopNum,
                                         Boiler(BoilerNum).LoopSideNum,
                                         Boiler(BoilerNum).BranchNum,
                                         Boiler(BoilerNum).CompNum);
                } else {
                    Boiler(BoilerNum).BoilerLoad = 0.0;
                    Boiler(BoilerNum).BoilerOutletTemp = DataLoopNode::Node(BoilerInletNode).Temp;
                }
            }

        } // End of the FlowLock If block

        // Limit BoilerOutletTemp.  If > max temp, trip boiler.
        if (Boiler(BoilerNum).BoilerOutletTemp > TempUpLimitBout) {
            Boiler(BoilerNum).BoilerLoad = 0.0;
            Boiler(BoilerNum).BoilerOutletTemp = DataLoopNode::Node(BoilerInletNode).Temp;
            //  Does BoilerMassFlowRate need to be set????
        }

        OperPLR = Boiler(BoilerNum).BoilerLoad / BoilerNomCap;
        OperPLR = min(OperPLR, BoilerMaxPLR);
        OperPLR = max(OperPLR, BoilerMinPLR);
        TheorFuelUse = Boiler(BoilerNum).BoilerLoad / BoilerEff;

        // Calculate fuel used
        Boiler(BoilerNum).FuelUsed = TheorFuelUse / (LoadCoef(1) + LoadCoef(2) * OperPLR + LoadCoef(3) * pow_2(OperPLR));
    }

    // Beginning of Record Keeping subroutines for the BOILER:SIMPLE Module

    void UpdateBoilerRecords(Real64 const MyLoad,                     // boiler operating load
                             bool const RunFlag,                      // boiler on when TRUE
                             int const Num,                           // boiler number
                             bool const EP_UNUSED(FirstHVACIteration) // TRUE if First iteration of simulation
    )
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rahul Chillar
        //       DATE WRITTEN   Dec 2004
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Boiler simulation reporting

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int BoilerInletNode;  // Boiler inlet node number
        int BoilerOutletNode; // Boiler outlet node number
        Real64 ReportingConstant;

        ReportingConstant = DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

        BoilerInletNode = Boiler(Num).BoilerInletNodeNum;
        BoilerOutletNode = Boiler(Num).BoilerOutletNodeNum;

        if (MyLoad <= 0.0 || !RunFlag) {
            // set node temperatures
            PlantUtilities::SafeCopyPlantNode(BoilerInletNode, BoilerOutletNode);
            DataLoopNode::Node(BoilerOutletNode).Temp = DataLoopNode::Node(BoilerInletNode).Temp;
            Boiler(Num).BoilerOutletTemp = DataLoopNode::Node(BoilerInletNode).Temp;
            Boiler(Num).BoilerLoad = 0.0;
            Boiler(Num).FuelUsed = 0.0;
            DataLoopNode::Node(BoilerInletNode).Press = Boiler(Num).BoilerPressCheck;
            DataLoopNode::Node(BoilerOutletNode).Press = DataLoopNode::Node(BoilerInletNode).Press;
            DataLoopNode::Node(BoilerInletNode).Quality = 0.0;
            DataLoopNode::Node(BoilerOutletNode).Quality = DataLoopNode::Node(BoilerInletNode).Quality;

        } else {
            // set node temperatures
            PlantUtilities::SafeCopyPlantNode(BoilerInletNode, BoilerOutletNode);
            DataLoopNode::Node(BoilerOutletNode).Temp = Boiler(Num).BoilerOutletTemp;
            Boiler(Num).BoilerOutletTemp = Boiler(Num).BoilerOutletTemp;
            Boiler(Num).BoilerLoad = Boiler(Num).BoilerLoad;
            Boiler(Num).FuelUsed = Boiler(Num).FuelUsed;
            DataLoopNode::Node(BoilerInletNode).Press = Boiler(Num).BoilerPressCheck; //???
            DataLoopNode::Node(BoilerOutletNode).Press = DataLoopNode::Node(BoilerInletNode).Press;
            DataLoopNode::Node(BoilerOutletNode).Quality = 1.0; // Model assumes saturated steam exiting the boiler
        }

        Boiler(Num).BoilerInletTemp = DataLoopNode::Node(BoilerInletNode).Temp;
        Boiler(Num).BoilerMassFlowRate = DataLoopNode::Node(BoilerOutletNode).MassFlowRate;
        Boiler(Num).BoilerEnergy = Boiler(Num).BoilerLoad * ReportingConstant;
        Boiler(Num).FuelConsumed = Boiler(Num).FuelUsed * ReportingConstant;
    }

} // namespace BoilerSteam

} // namespace EnergyPlus
