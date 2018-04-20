// EnergyPlus, Copyright (c) 1996-2018, The Board of Trustees of the University of Illinois,
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
#include <Boilers.hh>
#include <BranchNodeConnections.hh>
#include <CurveManager.hh>
#include <DataBranchAirLoopPlant.hh>
#include <DataGlobalConstants.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <EMSManager.hh>
#include <FaultsManager.hh>
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

namespace Boilers {

    // Module containing the routines dealing with the Boilers

    // MODULE INFORMATION:
    //       AUTHOR         Dan Fisher, Taecheol Kim
    //       DATE WRITTEN   1998, 2000
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // Perform boiler simulation for plant simulation

    // METHODOLOGY EMPLOYED:
    // The BLAST/DOE-2 empirical model based on mfg. data

    // Using/Aliasing
    using DataLoopNode::Node;
    using DataHVACGlobals::SmallWaterVolFlow;
    using DataHVACGlobals::TimeStepSys;
    using DataBranchAirLoopPlant::ControlType_SeriesActive;
    using DataGlobals::DisplayExtraWarnings;
    using DataGlobals::SecInHour;
    using DataPlant::PlantLoop;
    using DataPlant::TypeOf_Boiler_Simple;
    using General::RoundSigDigits;
    using General::TrimSigDigits;
    using PlantUtilities::ScanPlantLoopsForObject;

    // MODULE VARIABLE DECLARATIONS:
    int NumBoilers(0);              // Number of boilers
    bool GetBoilerInputFlag(true);

    // SUBROUTINE SPECIFICATIONS FOR MODULE Boilers

    // Object Data
    Array1D<BoilerSpecs> Boiler;      // boiler data - dimension to number of machines
    Array1D<ReportVars> BoilerReport; // report vars - dimension to number of machines

    // MODULE SUBROUTINES:

    // Beginning of Boiler Module Driver Subroutines
    //*************************************************************************

    // Functions
    PlantComponent *BoilerSpecs::factory(std::string objectName)
    {
        if (GetBoilerInputFlag) {
            GetBoilerInput();
            GetBoilerInputFlag = false;
        }
        // Now look for this particular boiler in the list
        for (auto &boiler : Boiler) {
            if (boiler.Name == objectName) {
                return &boiler;
            }
        }
        // If we didn't find it, fatal
        ShowFatalError("Boiler::factory: Error getting inputs for boiler named: " + objectName);
        return nullptr;
    }

    void BoilerSpecs::simulate(const PlantLocation &calledFromLocation, bool const FirstHVACIteration, Real64 &CurLoad, bool const RunFlag)
    {
        InitBoiler();
        CalcBoilerModel(CurLoad, RunFlag, EquipFlowCtrl);
        UpdateBoilerRecords(CurLoad, RunFlag);
    }

    void BoilerSpecs::getDesignCapacities(const PlantLocation &calledFromLocation, Real64 &MaxLoad, Real64 &MinLoad, Real64 &OptLoad)
    {
        MinLoad = NomCap * MinPartLoadRat;
        MaxLoad = NomCap * MaxPartLoadRat;
        OptLoad = NomCap * OptPartLoadRat;
    }

    void BoilerSpecs::getSizingFactor(Real64 &SizingFactor)
    {
        SizingFactor = SizFac;
    }

    void BoilerSpecs::onInitLoopEquip(const PlantLocation &calledFromLocation)
    {
        InitBoiler();
        SizeBoiler();
    }

    void clear_state()
    {
        NumBoilers = 0;
        Boiler.deallocate();
        GetBoilerInputFlag = true;
    }

    void GetBoilerInput()
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Dan Fisher
        //       DATE WRITTEN:    April 1998
        //       MODIFIED:        R. Raustad - FSEC, June 2008: added boiler efficiency curve object
        //       RE-ENGINEERED:   na

        // PURPOSE OF THIS SUBROUTINE:
        // get all boiler data from input file

        // METHODOLOGY EMPLOYED:
        // standard EnergyPlus input retrieval using input Processor

        // REFERENCES: na

        // Using/Aliasing
        using DataGlobals::AnyEnergyManagementSystemInModel;
        using DataGlobalConstants::AssignResourceTypeNum;
        using namespace DataIPShortCuts; // Data for field names, blank numerics
        using BranchNodeConnections::TestCompSet;
        using CurveManager::GetCurveIndex;
        using CurveManager::GetCurveType;
        using DataSizing::AutoSize;
        using General::RoundSigDigits;
        using GlobalNames::VerifyUniqueBoilerName;
        using NodeInputManager::GetOnlySingleNode;

        // Locals
        // PARAMETERS
        static std::string const RoutineName("GetBoilerInput: ");

        // LOCAL VARIABLES
        int BoilerNum(0);                               // boiler identifier
        int NumAlphas;                                  // Number of elements in the alpha array
        int NumNums;                                    // Number of elements in the numeric array
        int IOStat;                                     // IO Status when calling get input subroutine
        static bool ErrorsFound(false);                 // Flag to show errors were found during GetInput
        bool errFlag;                                   // Flag to show errors were found during function call
        Array1D_string BoilerFuelTypeForOutputVariable; // used to set up report variables

        // GET NUMBER OF ALL EQUIPMENT
        cCurrentModuleObject = "Boiler:HotWater";
        NumBoilers = inputProcessor->getNumObjectsFound(cCurrentModuleObject);

        if (NumBoilers <= 0) {
            ShowSevereError("No " + cCurrentModuleObject + " Equipment specified in input file");
            ErrorsFound = true;
        }

        // See if load distribution manager has already gotten the input
        if (allocated(Boiler)) return;

        Boiler.allocate(NumBoilers);
        BoilerFuelTypeForOutputVariable.allocate(NumBoilers);
        BoilerFuelTypeForOutputVariable = "";

        // LOAD ARRAYS WITH CURVE FIT Boiler DATA

        for (auto &boiler : Boiler) {
            ++BoilerNum;
            inputProcessor->getObjectItem(cCurrentModuleObject, BoilerNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks,
                                          lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames);
            UtilityRoutines::IsNameEmpty(cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);
            VerifyUniqueBoilerName(cCurrentModuleObject, cAlphaArgs(1), errFlag, cCurrentModuleObject + " Name");
            if (errFlag) {
                ErrorsFound = true;
            }
            boiler.Name = cAlphaArgs(1);
            boiler.TypeNum = TypeOf_Boiler_Simple;

            {
                auto const SELECT_CASE_var(cAlphaArgs(2));

                if ((SELECT_CASE_var == "ELECTRICITY") || (SELECT_CASE_var == "ELECTRIC") || (SELECT_CASE_var == "ELEC")) {
                    BoilerFuelTypeForOutputVariable(BoilerNum) = "Electric";
                    boiler.FuelType = AssignResourceTypeNum("ELECTRICITY");

                } else if ((SELECT_CASE_var == "GAS") || (SELECT_CASE_var == "NATURALGAS") || (SELECT_CASE_var == "NATURAL GAS")) {
                    BoilerFuelTypeForOutputVariable(BoilerNum) = "Gas";
                    boiler.FuelType = AssignResourceTypeNum("NATURALGAS");

                } else if (SELECT_CASE_var == "DIESEL") {
                    BoilerFuelTypeForOutputVariable(BoilerNum) = "Diesel";
                    boiler.FuelType = AssignResourceTypeNum("DIESEL");

                } else if (SELECT_CASE_var == "GASOLINE") {
                    BoilerFuelTypeForOutputVariable(BoilerNum) = "Gasoline";
                    boiler.FuelType = AssignResourceTypeNum("GASOLINE");

                } else if (SELECT_CASE_var == "COAL") {
                    BoilerFuelTypeForOutputVariable(BoilerNum) = "Coal";
                    boiler.FuelType = AssignResourceTypeNum("COAL");

                } else if ((SELECT_CASE_var == "FUEL OIL #1") || (SELECT_CASE_var == "FUELOIL#1") || (SELECT_CASE_var == "FUEL OIL") ||
                           (SELECT_CASE_var == "DISTILLATE OIL")) {
                    BoilerFuelTypeForOutputVariable(BoilerNum) = "FuelOil#1";
                    boiler.FuelType = AssignResourceTypeNum("DISTILLATE OIL");

                } else if ((SELECT_CASE_var == "FUEL OIL #2") || (SELECT_CASE_var == "FUELOIL#2") || (SELECT_CASE_var == "RESIDUAL OIL")) {
                    BoilerFuelTypeForOutputVariable(BoilerNum) = "FuelOil#2";
                    boiler.FuelType = AssignResourceTypeNum("RESIDUAL OIL");

                } else if ((SELECT_CASE_var == "PROPANE") || (SELECT_CASE_var == "LPG") || (SELECT_CASE_var == "PROPANEGAS") ||
                           (SELECT_CASE_var == "PROPANE GAS")) {
                    BoilerFuelTypeForOutputVariable(BoilerNum) = "Propane";
                    boiler.FuelType = AssignResourceTypeNum("PROPANE");

                } else if (SELECT_CASE_var == "OTHERFUEL1") {
                    BoilerFuelTypeForOutputVariable(BoilerNum) = "OtherFuel1";
                    boiler.FuelType = AssignResourceTypeNum("OTHERFUEL1");

                } else if (SELECT_CASE_var == "OTHERFUEL2") {
                    BoilerFuelTypeForOutputVariable(BoilerNum) = "OtherFuel2";
                    boiler.FuelType = AssignResourceTypeNum("OTHERFUEL2");

                } else {
                    ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\",");
                    ShowContinueError("Invalid " + cAlphaFieldNames(2) + '=' + cAlphaArgs(2));
                    // Set to Electric to avoid errors when setting up output variables
                    BoilerFuelTypeForOutputVariable(BoilerNum) = "Electric";
                    boiler.FuelType = AssignResourceTypeNum("ELECTRICITY");
                    ErrorsFound = true;
                }
            }

            boiler.NomCap = rNumericArgs(1);
            if (rNumericArgs(1) == 0.0) {
                ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\",");
                ShowContinueError("Invalid " + cNumericFieldNames(1) + '=' + RoundSigDigits(rNumericArgs(1), 2));
                ShowContinueError("..." + cNumericFieldNames(1) + " must be greater than 0.0");
                ErrorsFound = true;
            }
            if (boiler.NomCap == AutoSize) {
                boiler.NomCapWasAutoSized = true;
            }

            boiler.Effic = rNumericArgs(2);
            if (rNumericArgs(2) == 0.0) {
                ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\",");
                ShowContinueError("Invalid " + cNumericFieldNames(2) + '=' + RoundSigDigits(rNumericArgs(2), 3));
                ShowSevereError("..." + cNumericFieldNames(2) + " must be greater than 0.0");
                ErrorsFound = true;
            }

            {
                auto const SELECT_CASE_var(cAlphaArgs(3));

                if (SELECT_CASE_var == "ENTERINGBOILER") {
                    boiler.CurveTempMode = TemperatureEvaluationModeType::Entering;
                } else if (SELECT_CASE_var == "LEAVINGBOILER") {
                    boiler.CurveTempMode = TemperatureEvaluationModeType::Leaving;
                } else {
                    boiler.CurveTempMode = TemperatureEvaluationModeType::NotSet;
                }
            }

            boiler.EfficiencyCurvePtr = GetCurveIndex(cAlphaArgs(4));
            if (boiler.EfficiencyCurvePtr > 0) {
                {
                    auto const SELECT_CASE_var(GetCurveType(boiler.EfficiencyCurvePtr));
                    if (SELECT_CASE_var == "LINEAR") {
                        boiler.EfficiencyCurveType = EfficiencyCurveType::Linear;
                    } else if (SELECT_CASE_var == "QUADRATIC") {
                        boiler.EfficiencyCurveType = EfficiencyCurveType::Quadratic;
                    } else if (SELECT_CASE_var == "QUADRATICLINEAR") {
                        boiler.EfficiencyCurveType = EfficiencyCurveType::QuadraticLinear;
                    } else if (SELECT_CASE_var == "CUBIC") {
                        boiler.EfficiencyCurveType = EfficiencyCurveType::Cubic;
                    } else if (SELECT_CASE_var == "BICUBIC") {
                        boiler.EfficiencyCurveType = EfficiencyCurveType::BiCubic;
                    } else if (SELECT_CASE_var == "BIQUADRATIC") {
                        boiler.EfficiencyCurveType = EfficiencyCurveType::BiQuadratic;
                    } else {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\",");
                        ShowContinueError("Invalid " + cAlphaFieldNames(4) + '=' + cAlphaArgs(4));
                        ShowContinueError("...Curve type for " + cAlphaFieldNames(4) + "  = " + GetCurveType(boiler.EfficiencyCurvePtr));
                        ErrorsFound = true;
                    }
                }
            } else if (!lAlphaFieldBlanks(4)) {
                ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\",");
                ShowContinueError("Invalid " + cAlphaFieldNames(4) + '=' + cAlphaArgs(4));
                ShowSevereError("..." + cAlphaFieldNames(4) + " not found.");
                ErrorsFound = true;
            }

            // if curve uses temperature, make sure water temp mode has been set
            {
                auto const SELECT_CASE_var(boiler.EfficiencyCurveType);
                if ((SELECT_CASE_var == EfficiencyCurveType::BiQuadratic) || (SELECT_CASE_var == EfficiencyCurveType::QuadraticLinear) ||
                    (SELECT_CASE_var == EfficiencyCurveType::BiCubic)) {                                // curve uses water temperature
                    if (boiler.CurveTempMode == TemperatureEvaluationModeType::NotSet) { // throw error
                        if (!lAlphaFieldBlanks(3)) {
                            ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\",");
                            ShowContinueError("Invalid " + cAlphaFieldNames(3) + '=' + cAlphaArgs(3));
                            ShowContinueError("Boiler using curve type of " + GetCurveType(boiler.EfficiencyCurvePtr) + " must specify " +
                                              cAlphaFieldNames(3));
                            ShowContinueError("Available choices are EnteringBoiler or LeavingBoiler");
                        } else {
                            ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\",");
                            ShowContinueError("Field " + cAlphaFieldNames(3) + " is blank");
                            ShowContinueError("Boiler using curve type of " + GetCurveType(boiler.EfficiencyCurvePtr) +
                                              " must specify either EnteringBoiler or LeavingBoiler");
                        }
                        ErrorsFound = true;
                    }
                }
            }

            boiler.TempDesBoilerOut = rNumericArgs(3);
            boiler.VolFlowRate = rNumericArgs(4);
            if (boiler.VolFlowRate == AutoSize) {
                boiler.VolFlowRateWasAutoSized = true;
            }
            boiler.MinPartLoadRat = rNumericArgs(5);
            boiler.MaxPartLoadRat = rNumericArgs(6);
            boiler.OptPartLoadRat = rNumericArgs(7);

            boiler.TempUpLimitBoilerOut = rNumericArgs(8);
            // default to 99.9C if upper temperature limit is left blank.
            if (boiler.TempUpLimitBoilerOut <= 0.0) {
                boiler.TempUpLimitBoilerOut = 99.9;
            }

            boiler.ParasiticElecLoad = rNumericArgs(9);
            boiler.SizFac = rNumericArgs(10);
            if (boiler.SizFac == 0.0) boiler.SizFac = 1.0;

            boiler.BoilerInletNodeNum = GetOnlySingleNode(cAlphaArgs(5), ErrorsFound, cCurrentModuleObject, cAlphaArgs(1), DataLoopNode::NodeType_Water,
                                                          DataLoopNode::NodeConnectionType_Inlet, 1, DataLoopNode::ObjectIsNotParent);
            boiler.BoilerOutletNodeNum = GetOnlySingleNode(cAlphaArgs(6), ErrorsFound, cCurrentModuleObject, cAlphaArgs(1), DataLoopNode::NodeType_Water,
                                                           DataLoopNode::NodeConnectionType_Outlet, 1, DataLoopNode::ObjectIsNotParent);
            TestCompSet(cCurrentModuleObject, cAlphaArgs(1), cAlphaArgs(5), cAlphaArgs(6), "Hot Water Nodes");

            {
                auto const SELECT_CASE_var(cAlphaArgs(7));
                if (SELECT_CASE_var == "CONSTANTFLOW") {
                    boiler.FlowMode = FlowModeType::Constant;
                } else if (SELECT_CASE_var == "VARIABLEFLOW") { // backward compatible, clean out eventually
                    boiler.FlowMode = FlowModeType::LeavingSetPointModulated;
                    ShowWarningError(RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\",");
                    ShowContinueError("Invalid " + cAlphaFieldNames(7) + '=' + cAlphaArgs(7));
                    ShowContinueError("Key choice is now called \"LeavingSetpointModulated\" and the simulation continues");
                } else if (SELECT_CASE_var == "LEAVINGSETPOINTMODULATED") {
                    boiler.FlowMode = FlowModeType::LeavingSetPointModulated;
                } else if (SELECT_CASE_var == "NOTMODULATED") {
                    boiler.FlowMode = FlowModeType::NotModulated;
                } else {
                    ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\",");
                    ShowContinueError("Invalid " + cAlphaFieldNames(7) + '=' + cAlphaArgs(7));
                    ShowContinueError("Available choices are ConstantFlow, NotModulated, or LeavingSetpointModulated");
                    ShowContinueError("Flow mode NotModulated is assumed and the simulation continues.");
                    // We will assume variable flow if not specified
                    boiler.FlowMode = FlowModeType::NotModulated;
                }
            }

            if (NumAlphas > 7) {
                boiler.EndUseSubcategory = cAlphaArgs(8);
            } else {
                boiler.EndUseSubcategory = "Boiler"; // leave this as "boiler" instead of "general" like other end use subcategories since
                                                     // it appears this way in existing output files.
            }
        }

        if (ErrorsFound) {
            ShowFatalError(RoutineName + "Errors found in processing " + cCurrentModuleObject + " input.");
        }

        for (auto &boiler : Boiler) {
            ReportVars report(boiler.reportVariables);

            SetupOutputVariable("Boiler Heating Rate", OutputProcessor::Unit::W, report.BoilerLoad, "System", "Average", boiler.Name);
            SetupOutputVariable("Boiler Heating Energy", OutputProcessor::Unit::J, report.BoilerEnergy, "System", "Sum",
                                boiler.Name, _, "ENERGYTRANSFER", "BOILERS", _, "Plant");

            if (UtilityRoutines::SameString(BoilerFuelTypeForOutputVariable(BoilerNum), "Electric")) {
                SetupOutputVariable("Boiler " + BoilerFuelTypeForOutputVariable(BoilerNum) + " Power", OutputProcessor::Unit::W,
                                    report.FuelUsed, "System", "Average", boiler.Name);
            } else {
                SetupOutputVariable("Boiler " + BoilerFuelTypeForOutputVariable(BoilerNum) + " Rate", OutputProcessor::Unit::W,
                                    report.FuelUsed, "System", "Average", boiler.Name);
            }

            SetupOutputVariable("Boiler " + BoilerFuelTypeForOutputVariable(BoilerNum) + " Energy", OutputProcessor::Unit::J,
                                report.FuelConsumed, "System", "Sum", boiler.Name, _,
                                BoilerFuelTypeForOutputVariable(BoilerNum), "Heating", boiler.EndUseSubcategory, "Plant");
            SetupOutputVariable("Boiler Inlet Temperature", OutputProcessor::Unit::C, report.BoilerInletTemp, "System", "Average", boiler.Name);
            SetupOutputVariable("Boiler Outlet Temperature", OutputProcessor::Unit::C, report.BoilerOutletTemp, "System", "Average", boiler.Name);
            SetupOutputVariable("Boiler Mass Flow Rate", OutputProcessor::Unit::kg_s, report.Mdot, "System", "Average", boiler.Name);
            SetupOutputVariable("Boiler Ancillary Electric Power", OutputProcessor::Unit::W, report.ParasiticElecPower, "System", "Average", boiler.Name);
            SetupOutputVariable("Boiler Ancillary Electric Energy", OutputProcessor::Unit::J, report.ParasiticElecConsumption,
                                "System", "Sum", boiler.Name, _, "ELECTRICITY", "Heating", "Boiler Parasitic", "Plant");
            SetupOutputVariable("Boiler Part Load Ratio", OutputProcessor::Unit::None, report.BoilerPLR, "System", "Average", boiler.Name);

            if (AnyEnergyManagementSystemInModel) {
                SetupEMSInternalVariable("Boiler Nominal Capacity", boiler.Name, "[W]", boiler.NomCap);
            }
        }

        BoilerFuelTypeForOutputVariable.deallocate();
    }

    void BoilerSpecs::InitBoiler() // number of the current boiler being simulated
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   April 2002
        //       MODIFIED       na
        //       RE-ENGINEERED  Brent Griffith, rework for plant upgrade

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for initializations of the Boiler components

        // METHODOLOGY EMPLOYED:
        // Uses the status flags to trigger initializations.

        // Using/Aliasing
        using DataGlobals::AnyEnergyManagementSystemInModel;
        using DataGlobals::BeginEnvrnFlag;
        using DataPlant::DualSetPointDeadBand;
        using DataPlant::LoopFlowStatus_NeedyIfLoopOn;
        using DataPlant::PlantFirstSizesOkayToFinalize;
        using DataPlant::SingleSetPoint;
        using DataPlant::TypeOf_Boiler_Simple;
        using EMSManager::CheckIfNodeSetPointManagedByEMS;
        using EMSManager::iTemperatureSetPoint;
        using FluidProperties::GetDensityGlycol;
        using PlantUtilities::InitComponentNodes;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("InitBoiler");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 rho;
        bool FatalError;
        bool errFlag;

        // Init more variables
        if (doOneTimeInitialisation) {
            // Locate the boilers on the plant loops for later usage
            errFlag = false;
            ScanPlantLoopsForObject(Name, TypeOf_Boiler_Simple, LoopNum, LoopSideNum,
                                    BranchNum, CompNum, _, TempUpLimitBoilerOut, _, _, _,
                                    errFlag);
            if (errFlag) {
                ShowFatalError("InitBoiler: Program terminated due to previous condition(s).");
            }

            if ((FlowMode == FlowModeType::LeavingSetPointModulated) || (FlowMode == FlowModeType::Constant)) {
                // reset flow priority
                PlantLoop(LoopNum)
                    .LoopSide(LoopSideNum)
                    .Branch(BranchNum)
                    .Comp(CompNum)
                    .FlowPriority = LoopFlowStatus_NeedyIfLoopOn;
            }

            doOneTimeInitialisation = false;
        }

        if (doEnvironmentInitialisation && BeginEnvrnFlag && (PlantFirstSizesOkayToFinalize)) {
            // if ( ! PlantFirstSizeCompleted ) SizeBoiler( BoilerNum );
            rho = GetDensityGlycol(PlantLoop(LoopNum).FluidName, DataGlobals::CWInitConvTemp,
                                   PlantLoop(LoopNum).FluidIndex, RoutineName);
            DesMassFlowRate = VolFlowRate * rho;

            InitComponentNodes(0.0, DesMassFlowRate, BoilerInletNodeNum, BoilerOutletNodeNum,
                               LoopNum, LoopSideNum, BranchNum, CompNum);

            if (FlowMode == FlowModeType::LeavingSetPointModulated) { // check if setpoint on outlet node
                if ((Node(BoilerOutletNodeNum).TempSetPoint == DataLoopNode::SensedNodeFlagValue) &&
                    (Node(BoilerOutletNodeNum).TempSetPointLo == DataLoopNode::SensedNodeFlagValue)) {
                    if (!AnyEnergyManagementSystemInModel) {
                        if (!ModulatedFlowErrDone) {
                            ShowWarningError("Missing temperature setpoint for LeavingSetpointModulated mode Boiler named " + Name);
                            ShowContinueError(
                                "  A temperature setpoint is needed at the outlet node of a boiler in variable flow mode, use a SetpointManager");
                            ShowContinueError("  The overall loop setpoint will be assumed for Boiler. The simulation continues ... ");
                            ModulatedFlowErrDone = true;
                        }
                    } else {
                        // need call to EMS to check node
                        FatalError = false; // but not really fatal yet, but should be.
                        CheckIfNodeSetPointManagedByEMS(BoilerOutletNodeNum, iTemperatureSetPoint, FatalError);
                        if (FatalError) {
                            if (!ModulatedFlowErrDone) {
                                ShowWarningError("Missing temperature setpoint for LeavingSetpointModulated mode Boiler named " +
                                                 Name);
                                ShowContinueError("  A temperature setpoint is needed at the outlet node of a boiler in variable flow mode");
                                ShowContinueError("  use a Setpoint Manager to establish a setpoint at the boiler outlet node ");
                                ShowContinueError("  or use an EMS actuator to establish a setpoint at the boiler outlet node ");
                                ShowContinueError("  The overall loop setpoint will be assumed for Boiler. The simulation continues ... ");
                                ModulatedFlowErrDone = true;
                            }
                        }
                    }
                    ModulatedFlowSetToLoop = true; // this is for backward compatibility and could be removed
                }
            }

            doEnvironmentInitialisation = false;
        }

        if (!BeginEnvrnFlag) {
            doEnvironmentInitialisation = true;
        }

        // every iteration inits.  (most in calc routine)

        if ((FlowMode == FlowModeType::LeavingSetPointModulated) && ModulatedFlowSetToLoop) {
            // fix for clumsy old input that worked because loop setpoint was spread.
            //  could be removed with transition, testing , model change, period of being obsolete.
            {
                auto const SELECT_CASE_var(PlantLoop(LoopNum).LoopDemandCalcScheme);
                if (SELECT_CASE_var == SingleSetPoint) {
                    Node(BoilerOutletNodeNum).TempSetPoint =
                        Node(PlantLoop(LoopNum).TempSetPointNodeNum).TempSetPoint;
                } else if (SELECT_CASE_var == DualSetPointDeadBand) {
                    Node(BoilerOutletNodeNum).TempSetPointLo =
                        Node(PlantLoop(LoopNum).TempSetPointNodeNum).TempSetPointLo;
                }
            }
        }
    }

    void BoilerSpecs::SizeBoiler()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   April 2002
        //       MODIFIED       November 2013 Daeho Kang, add component sizing table entries
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for sizing Boiler Components for which capacities and flow rates
        // have not been specified in the input.

        // METHODOLOGY EMPLOYED:
        // Obtains hot water flow rate from the plant sizing array. Calculates nominal capacity from
        // the hot water flow rate and the hot water loop design delta T.

        // Using/Aliasing
        using DataSizing::PlantSizData;
        using DataSizing::AutoVsHardSizingThreshold;
        using DataPlant::PlantFinalSizesOkayToReport;
        using DataPlant::PlantFirstSizesOkayToFinalize;
        using DataPlant::PlantFirstSizesOkayToReport;
        using DataPlant::PlantLoop;
        using FluidProperties::GetDensityGlycol;
        using FluidProperties::GetSpecificHeatGlycol;
        using PlantUtilities::RegisterPlantCompDesignFlow;
        using ReportSizingManager::ReportSizingOutput;
        using namespace OutputReportPredefined;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("SizeBoiler");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int PltSizNum(0);        // Plant Sizing index corresponding to CurLoopNum
        bool ErrorsFound(false); // If errors detected in input
        Real64 rho;
        Real64 Cp;
        Real64 tmpNomCap;            // local nominal capacity cooling power
        Real64 tmpBoilerVolFlowRate; // local boiler design volume flow rate
        Real64 NomCapUser(0.0);      // Hardsized nominal capacity for reporting
        Real64 VolFlowRateUser(0.0); // Hardsized volume flow for reporting

        tmpNomCap = NomCap;
        tmpBoilerVolFlowRate = VolFlowRate;

        PltSizNum = PlantLoop(LoopNum).PlantSizNum;

        if (PltSizNum > 0) {
            if (PlantSizData(PltSizNum).DesVolFlowRate >= SmallWaterVolFlow) {

                rho = GetDensityGlycol(PlantLoop(LoopNum).FluidName, DataGlobals::CWInitConvTemp,
                                       PlantLoop(LoopNum).FluidIndex, RoutineName);
                Cp = GetSpecificHeatGlycol(PlantLoop(LoopNum).FluidName, TempDesBoilerOut,
                                           PlantLoop(LoopNum).FluidIndex, RoutineName);
                tmpNomCap = Cp * rho * SizFac * PlantSizData(PltSizNum).DeltaT * PlantSizData(PltSizNum).DesVolFlowRate;
            } else {
                if (NomCapWasAutoSized) tmpNomCap = 0.0;
            }
            if (PlantFirstSizesOkayToFinalize) {
                if (NomCapWasAutoSized) {
                    NomCap = tmpNomCap;
                    if (PlantFinalSizesOkayToReport) {
                        ReportSizingOutput("Boiler:HotWater", Name, "Design Size Nominal Capacity [W]", tmpNomCap);
                    }
                    if (PlantFirstSizesOkayToReport) {
                        ReportSizingOutput("Boiler:HotWater", Name, "Initial Design Size Nominal Capacity [W]", tmpNomCap);
                    }
                } else { // Hard-sized with sizing data
                    if (NomCap > 0.0 && tmpNomCap > 0.0) {
                        NomCapUser = NomCap;
                        if (PlantFinalSizesOkayToReport) {
                            ReportSizingOutput("Boiler:HotWater", Name, "Design Size Nominal Capacity [W]", tmpNomCap,
                                               "User-Specified Nominal Capacity [W]", NomCapUser);
                            if (DisplayExtraWarnings) {
                                if ((std::abs(tmpNomCap - NomCapUser) / NomCapUser) > AutoVsHardSizingThreshold) {
                                    ShowMessage("SizeBoilerHotWater: Potential issue with equipment sizing for " + Name);
                                    ShowContinueError("User-Specified Nominal Capacity of " + RoundSigDigits(NomCapUser, 2) + " [W]");
                                    ShowContinueError("differs from Design Size Nominal Capacity of " + RoundSigDigits(tmpNomCap, 2) + " [W]");
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
            if (NomCapWasAutoSized && PlantFirstSizesOkayToFinalize) {
                ShowSevereError("Autosizing of Boiler nominal capacity requires a loop Sizing:Plant object");
                ShowContinueError("Occurs in Boiler object=" + Name);
                ErrorsFound = true;
            }
            if (!NomCapWasAutoSized && PlantFinalSizesOkayToReport &&
                (NomCap > 0.0)) { // Hard-sized with no sizing data
                ReportSizingOutput("Boiler:HotWater", Name, "User-Specified Nominal Capacity [W]", NomCap);
            }
        }

        if (PltSizNum > 0) {
            if (PlantSizData(PltSizNum).DesVolFlowRate >= SmallWaterVolFlow) {
                tmpBoilerVolFlowRate = PlantSizData(PltSizNum).DesVolFlowRate * SizFac;
            } else {
                if (VolFlowRateWasAutoSized) tmpBoilerVolFlowRate = 0.0;
            }
            if (PlantFirstSizesOkayToFinalize) {
                if (VolFlowRateWasAutoSized) {
                    VolFlowRate = tmpBoilerVolFlowRate;
                    if (PlantFinalSizesOkayToReport) {
                        ReportSizingOutput("Boiler:HotWater", Name, "Design Size Design Water Flow Rate [m3/s]",
                                           tmpBoilerVolFlowRate);
                    }
                    if (PlantFirstSizesOkayToReport) {
                        ReportSizingOutput("Boiler:HotWater", Name, "Initial Design Size Design Water Flow Rate [m3/s]",
                                           tmpBoilerVolFlowRate);
                    }
                } else {
                    if (VolFlowRate > 0.0 && tmpBoilerVolFlowRate > 0.0) {
                        VolFlowRateUser = VolFlowRate;
                        if (PlantFinalSizesOkayToReport) {
                            ReportSizingOutput("Boiler:HotWater", Name, "Design Size Design Water Flow Rate [m3/s]",
                                               tmpBoilerVolFlowRate, "User-Specified Design Water Flow Rate [m3/s]", VolFlowRateUser);
                            if (DisplayExtraWarnings) {
                                if ((std::abs(tmpBoilerVolFlowRate - VolFlowRateUser) / VolFlowRateUser) > AutoVsHardSizingThreshold) {
                                    ShowMessage("SizeBoilerHotWater: Potential issue with equipment sizing for " + Name);
                                    ShowContinueError("User-Specified Design Water Flow Rate of " + RoundSigDigits(VolFlowRateUser, 2) + " [m3/s]");
                                    ShowContinueError("differs from Design Size Design Water Flow Rate of " +
                                                      RoundSigDigits(tmpBoilerVolFlowRate, 2) + " [m3/s]");
                                    ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                        }
                        tmpBoilerVolFlowRate = VolFlowRateUser;
                    }
                }
            }
        } else {
            if (VolFlowRateWasAutoSized && PlantFirstSizesOkayToFinalize) {
                ShowSevereError("Autosizing of Boiler design flow rate requires a loop Sizing:Plant object");
                ShowContinueError("Occurs in Boiler object=" + Name);
                ErrorsFound = true;
            }
            if (!VolFlowRateWasAutoSized && PlantFinalSizesOkayToReport &&
                (VolFlowRate > 0.0)) { // Hard-sized with no sizing data
                ReportSizingOutput("Boiler:HotWater", Name, "User-Specified Design Water Flow Rate [m3/s]", VolFlowRate);
            }
        }

        RegisterPlantCompDesignFlow(BoilerInletNodeNum, tmpBoilerVolFlowRate);

        if (PlantFinalSizesOkayToReport) {
            // create predefined report
            PreDefTableEntry(pdchMechType, Name, "Boiler:HotWater");
            PreDefTableEntry(pdchMechNomEff, Name, Effic);
            PreDefTableEntry(pdchMechNomCap, Name, NomCap);
        }

        if (ErrorsFound) {
            ShowFatalError("Preceding sizing errors cause program termination");
        }
    }

    void BoilerSpecs::CalcBoilerModel(Real64 const MyLoad,    // W - hot water demand to be met by boiler
                                      bool const RunFlag,     // TRUE if boiler operating
                                      int const EquipFlowCtrl // Flow control mode for the equipment
    )
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Dan Fisher
        //       DATE WRITTEN   April 1999
        //       MODIFIED       Taecheol Kim,May 2000
        //                      Jun. 2008, R. Raustad, FSEC. Added boiler efficiency curve object
        //                      Aug. 2011, B. Griffith, NREL. Added switch for temperature to use in curve
        //                      Nov. 2016, R. Zhang, LBNL. Applied the boiler fouling fault model
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine calculates the boiler fuel consumption and the associated
        // hot water demand met by the boiler

        // METHODOLOGY EMPLOYED:
        // The model is based on a single combustion efficiency (=1 for electric)
        // and a second order polynomial fit of performance data to obtain part
        // load performance

        // REFERENCES:

        // Using/Aliasing
        using CurveManager::CurveValue;
        using DataBranchAirLoopPlant::ControlType_SeriesActive;
        using DataGlobals::BeginEnvrnFlag;
        using DataGlobals::DoingSizing;
        using DataGlobals::KickOffSimulation;
        using DataGlobals::WarmupFlag;
        using DataPlant::DualSetPointDeadBand;
        using DataPlant::SingleSetPoint;
        using FaultsManager::FaultsBoilerFouling;
        using FluidProperties::GetSpecificHeatGlycol;
        using General::TrimSigDigits;
        using PlantUtilities::SetComponentFlowRate;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("CalcBoilerModel");

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 BoilerEff;             // boiler efficiency
        Real64 BoilerNomCap;          // W - boiler nominal capacity
        Real64 BoilerMaxPLR;          // boiler maximum part load ratio
        Real64 BoilerMinPLR;          // boiler minimum part load ratio
        Real64 TheorFuelUse;          // Theoretical (stoichiometric) fuel use
        Real64 OperPLR;               // operating part load ratio
        Real64 BoilerDeltaTemp(0.0);  // C - boiler inlet to outlet temperature difference
        Real64 TempUpLimitBout;       // C - boiler high temperature limit
        int BoilerInletNode;          // Boiler inlet node number
        int BoilerOutletNode;         // Boiler outlet node number
        Real64 BoilerMassFlowRateMax; // Max Design Boiler Mass Flow Rate converted from Volume Flow Rate
        Real64 EffCurveOutput;        // Output of boiler efficiency curve
        Real64 Cp;

        // FLOW

        BoilerLoad = 0.0;
        ParasiticElecPower = 0.0;
        BoilerMassFlowRate = 0.0;
        BoilerInletNode = BoilerInletNodeNum;
        BoilerOutletNode = BoilerOutletNodeNum;
        BoilerNomCap = NomCap;
        BoilerMaxPLR = MaxPartLoadRat;
        BoilerMinPLR = MinPartLoadRat;
        BoilerEff = Effic;
        TempUpLimitBout = TempUpLimitBoilerOut;
        BoilerMassFlowRateMax = DesMassFlowRate;

        Cp = GetSpecificHeatGlycol(PlantLoop(LoopNum).FluidName, Node(BoilerInletNode).Temp,
                                   PlantLoop(LoopNum).FluidIndex, RoutineName);

        // If the specified load is 0.0 or the boiler should not run then we leave this subroutine. Before leaving
        // if the component control is SERIESACTIVE we set the component flow to inlet flow so that flow resolver
        // will not shut down the branch
        if (MyLoad <= 0.0 || !RunFlag) {
            if (EquipFlowCtrl == ControlType_SeriesActive) BoilerMassFlowRate = Node(BoilerInletNode).MassFlowRate;
            return;
        }

        // If there is a fault of boiler fouling (zrp_Nov2016)
        if (FaultyBoilerFoulingFlag && (!WarmupFlag) && (!DoingSizing) && (!KickOffSimulation)) {
            int FaultIndex = FaultyBoilerFoulingIndex;
            Real64 NomCap_ff = BoilerNomCap;
            Real64 BoilerEff_ff = BoilerEff;

            // calculate the Faulty Boiler Fouling Factor using fault information
            FaultyBoilerFoulingFactor = FaultsBoilerFouling(FaultIndex).CalFoulingFactor();

            // update the boiler nominal capacity at faulty cases
            BoilerNomCap = NomCap_ff * FaultyBoilerFoulingFactor;
            BoilerEff = BoilerEff_ff * FaultyBoilerFoulingFactor;
        }

        // Set the current load equal to the boiler load
        BoilerLoad = MyLoad;

        if (PlantLoop(LoopNum).LoopSide(LoopSideNum).FlowLock == 0) {
            // Either set the flow to the Constant value or caluclate the flow for the variable volume
            if ((FlowMode == FlowModeType::Constant) || (FlowMode == FlowModeType::NotModulated)) {
                // Then find the flow rate and outlet temp
                BoilerMassFlowRate = BoilerMassFlowRateMax;
                SetComponentFlowRate(BoilerMassFlowRate, BoilerInletNode, BoilerOutletNode, LoopNum, LoopSideNum,
                                     BranchNum, CompNum);

                if ((BoilerMassFlowRate != 0.0) && (MyLoad > 0.0)) {
                    BoilerDeltaTemp = BoilerLoad / BoilerMassFlowRate / Cp;
                } else {
                    BoilerDeltaTemp = 0.0;
                }

                BoilerOutletTemp = BoilerDeltaTemp + Node(BoilerInletNode).Temp;

            } else if (FlowMode == FlowModeType::LeavingSetPointModulated) {
                // Calculate the Delta Temp from the inlet temp to the boiler outlet setpoint
                // Then find the flow rate and outlet temp

                {
                    auto const SELECT_CASE_var(PlantLoop(LoopNum).LoopDemandCalcScheme);
                    if (SELECT_CASE_var == SingleSetPoint) {
                        BoilerDeltaTemp = Node(BoilerOutletNode).TempSetPoint - Node(BoilerInletNode).Temp;
                    } else if (SELECT_CASE_var == DualSetPointDeadBand) {
                        BoilerDeltaTemp = Node(BoilerOutletNode).TempSetPointLo - Node(BoilerInletNode).Temp;
                    } else {
                        assert(false);
                    }
                }

                BoilerOutletTemp = BoilerDeltaTemp + Node(BoilerInletNode).Temp;

                if ((BoilerDeltaTemp > 0.0) && (BoilerLoad > 0.0)) {
                    BoilerMassFlowRate = BoilerLoad / Cp / BoilerDeltaTemp;

                    BoilerMassFlowRate = min(BoilerMassFlowRateMax, BoilerMassFlowRate);

                } else {
                    BoilerMassFlowRate = 0.0;
                }
                SetComponentFlowRate(BoilerMassFlowRate, BoilerInletNode, BoilerOutletNode, LoopNum, LoopSideNum,
                                     BranchNum, CompNum);

            } // End of Constant/Variable Flow If Block

        } else { // If FlowLock is True
            // Set the boiler flow rate from inlet node and then check performance
            BoilerMassFlowRate = Node(BoilerInletNode).MassFlowRate;

            if ((MyLoad > 0.0) && (BoilerMassFlowRate > 0.0)) { // this boiler has a heat load
                BoilerLoad = MyLoad;
                if (BoilerLoad > BoilerNomCap * BoilerMaxPLR) BoilerLoad = BoilerNomCap * BoilerMaxPLR;
                if (BoilerLoad < BoilerNomCap * BoilerMinPLR) BoilerLoad = BoilerNomCap * BoilerMinPLR;
                BoilerOutletTemp = Node(BoilerInletNode).Temp + BoilerLoad / (BoilerMassFlowRate * Cp);
                BoilerDeltaTemp = BoilerOutletTemp - Node(BoilerInletNode).Temp;
            } else {
                BoilerLoad = 0.0;
                BoilerOutletTemp = Node(BoilerInletNode).Temp;
            }

        } // End of the FlowLock If block

        // Limit BoilerOutletTemp.  If > max temp, trip boiler off
        if (BoilerOutletTemp > TempUpLimitBout) {
            BoilerDeltaTemp = 0.0;
            BoilerLoad = 0.0;
            BoilerOutletTemp = Node(BoilerInletNode).Temp;
        }

        OperPLR = BoilerLoad / BoilerNomCap;
        OperPLR = min(OperPLR, BoilerMaxPLR);
        OperPLR = max(OperPLR, BoilerMinPLR);

        // set report variable
        BoilerPLR = OperPLR;

        // calculate theoretical fuel use based on nominal thermal efficiency
        TheorFuelUse = BoilerLoad / BoilerEff;

        // calculate normalized efficiency based on curve object type
        if (EfficiencyCurvePtr > 0) {
            if (EfficiencyCurveType == EfficiencyCurveType::BiQuadratic ||
                EfficiencyCurveType == EfficiencyCurveType::QuadraticLinear ||
                EfficiencyCurveType == EfficiencyCurveType::BiCubic) {

                if (CurveTempMode == TemperatureEvaluationModeType::Entering) {
                    EffCurveOutput = CurveValue(EfficiencyCurvePtr, OperPLR, Node(BoilerInletNode).Temp);
                } else if (CurveTempMode == TemperatureEvaluationModeType::Leaving) {
                    EffCurveOutput = CurveValue(EfficiencyCurvePtr, OperPLR, BoilerOutletTemp);
                }

            } else {
                EffCurveOutput = CurveValue(EfficiencyCurvePtr, OperPLR);
            }
        } else {
            EffCurveOutput = 1.0;
        }

        // warn if efficiency curve produces zero or negative results
        if (!WarmupFlag && EffCurveOutput <= 0.0) {
            if (BoilerLoad > 0.0) {
                if (EffCurveOutputError < 1) {
                    ++EffCurveOutputError;
                    ShowWarningError("Boiler:HotWater \"" + Name + "\"");
                    ShowContinueError("...Normalized Boiler Efficiency Curve output is less than or equal to 0.");
                    ShowContinueError("...Curve input x value (PLR)     = " + TrimSigDigits(OperPLR, 5));
                    if (EfficiencyCurveType == EfficiencyCurveType::BiQuadratic ||
                        EfficiencyCurveType == EfficiencyCurveType::QuadraticLinear ||
                        EfficiencyCurveType == EfficiencyCurveType::BiCubic) {
                        if (CurveTempMode == TemperatureEvaluationModeType::Entering) {
                            ShowContinueError("...Curve input y value (Tinlet) = " + TrimSigDigits(Node(BoilerInletNode).Temp, 2));
                        } else if (CurveTempMode == TemperatureEvaluationModeType::Leaving) {
                            ShowContinueError("...Curve input y value (Toutlet) = " + TrimSigDigits(BoilerOutletTemp, 2));
                        }
                    }
                    ShowContinueError("...Curve output (normalized eff) = " + TrimSigDigits(EffCurveOutput, 5));
                    ShowContinueError("...Calculated Boiler efficiency  = " + TrimSigDigits(EffCurveOutput * BoilerEff, 5) +
                                      " (Boiler efficiency = Nominal Thermal Efficiency * Normalized Boiler Efficiency Curve output)");
                    ShowContinueErrorTimeStamp("...Curve output reset to 0.01 and simulation continues.");
                } else {
                    ShowRecurringWarningErrorAtEnd("Boiler:HotWater \"" + Name +
                                                       "\": Boiler Efficiency Curve output is less than or equal to 0 warning continues...",
                                                   EffCurveOutputIndex, EffCurveOutput, EffCurveOutput);
                }
            }
            EffCurveOutput = 0.01;
        }

        // warn if overall efficiency greater than 1.1
        if (!WarmupFlag && EffCurveOutput * BoilerEff > 1.1) {
            if (BoilerLoad > 0.0 && EfficiencyCurvePtr > 0) {
                if (CalculatedEffError < 1) {
                    ++CalculatedEffError;
                    ShowWarningError("Boiler:HotWater \"" + Name + "\"");
                    ShowContinueError("...Calculated Boiler Efficiency is greater than 1.1.");
                    ShowContinueError("...Boiler Efficiency calculations shown below.");
                    ShowContinueError("...Curve input x value (PLR)     = " + TrimSigDigits(OperPLR, 5));
                    if (EfficiencyCurveType == EfficiencyCurveType::BiQuadratic ||
                        EfficiencyCurveType == EfficiencyCurveType::QuadraticLinear ||
                        EfficiencyCurveType == EfficiencyCurveType::BiCubic) {
                        if (CurveTempMode == TemperatureEvaluationModeType::Entering) {
                            ShowContinueError("...Curve input y value (Tinlet) = " + TrimSigDigits(Node(BoilerInletNode).Temp, 2));
                        } else if (CurveTempMode == TemperatureEvaluationModeType::Leaving) {
                            ShowContinueError("...Curve input y value (Toutlet) = " + TrimSigDigits(BoilerOutletTemp, 2));
                        }
                    }
                    ShowContinueError("...Curve output (normalized eff) = " + TrimSigDigits(EffCurveOutput, 5));
                    ShowContinueError("...Calculated Boiler efficiency  = " + TrimSigDigits(EffCurveOutput * BoilerEff, 5) +
                                      " (Boiler efficiency = Nominal Thermal Efficiency * Normalized Boiler Efficiency Curve output)");
                    ShowContinueErrorTimeStamp("...Curve output reset to 1.1 and simulation continues.");
                } else {
                    ShowRecurringWarningErrorAtEnd("Boiler:HotWater \"" + Name +
                                                       "\": Calculated Boiler Efficiency is greater than 1.1 warning continues...",
                                                   CalculatedEffIndex, EffCurveOutput * BoilerEff, EffCurveOutput * BoilerEff);
                }
            }
            EffCurveOutput = 1.1;
        }

        // calculate fuel used based on normalized boiler efficiency curve (=1 when no curve used)
        FuelUsed = TheorFuelUse / EffCurveOutput;
        if (BoilerLoad > 0.0) ParasiticElecPower = ParasiticElecLoad * OperPLR;
    }

    // Beginning of Record Keeping subroutines for the BOILER:HOTWATER Module
    // *****************************************************************************

    void BoilerSpecs::UpdateBoilerRecords(Real64 const MyLoad, // boiler operating load
                                          bool const RunFlag  // boiler on when TRUE
    )
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Dan Fisher
        //       DATE WRITTEN:    October 1998

        // PURPOSE OF THIS SUBROUTINE:
        // boiler simulation reporting

        // Using/Aliasing
        using PlantUtilities::SafeCopyPlantNode;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 ReportingConstant; // constant for converting power to energy
        ReportingConstant = TimeStepSys * SecInHour;

        if (MyLoad <= 0 || !RunFlag) {
            // set node temperatures
            SafeCopyPlantNode(BoilerInletNodeNum, BoilerOutletNodeNum);
            Node(BoilerOutletNodeNum).Temp = Node(BoilerInletNodeNum).Temp;
            reportVariables.BoilerOutletTemp = Node(BoilerInletNodeNum).Temp;
            reportVariables.BoilerLoad = 0.0;
            reportVariables.FuelUsed = 0.0;
            reportVariables.ParasiticElecPower = 0.0;
            reportVariables.BoilerPLR = 0.0;

        } else {
            // set node temperatures
            SafeCopyPlantNode(BoilerInletNodeNum, BoilerOutletNodeNum);
            Node(BoilerOutletNodeNum).Temp = BoilerOutletTemp;
            reportVariables.BoilerOutletTemp = BoilerOutletTemp;
            reportVariables.BoilerLoad = BoilerLoad;
            reportVariables.FuelUsed = FuelUsed;
            reportVariables.ParasiticElecPower = ParasiticElecPower;
            reportVariables.BoilerPLR = BoilerPLR;
        }

        reportVariables.BoilerInletTemp = Node(BoilerInletNodeNum).Temp;
        reportVariables.Mdot = Node(BoilerOutletNodeNum).MassFlowRate;

        reportVariables.BoilerEnergy = reportVariables.BoilerLoad * ReportingConstant;
        reportVariables.FuelConsumed = reportVariables.FuelUsed * ReportingConstant;
        reportVariables.ParasiticElecConsumption = reportVariables.ParasiticElecPower * ReportingConstant;
    }

    // End of Record Keeping subroutines for the BOILER:HOTWATER Module
    // *****************************************************************************

} // namespace Boilers

} // namespace EnergyPlus
