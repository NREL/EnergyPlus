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

// EnergyPlus Headers
#include <EnergyPlus/Boilers.hh>
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataBranchAirLoopPlant.hh>
#include <EnergyPlus/DataGlobalConstants.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
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
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/Plant/PlantLocation.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/ReportSizingManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

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

    PlantComponent *BoilerSpecs::factory(EnergyPlusData &state, std::string const &objectName)
    {
        // Process the input data for boilers if it hasn't been done already
        if (state.dataBoilers.getBoilerInputFlag) {
            GetBoilerInput(state, state.dataBoilers);
            state.dataBoilers.getBoilerInputFlag = false;
        }
        // Now look for this particular pipe in the list
        for (auto &boiler : state.dataBoilers.Boiler) {
            if (boiler.Name == objectName) {
                return &boiler;
            }
        }
        // If we didn't find it, fatal
        ShowFatalError("LocalBoilerFactory: Error getting inputs for boiler named: " + objectName); // LCOV_EXCL_LINE
        // Shut up the compiler
        return nullptr; // LCOV_EXCL_LINE
    }

    void BoilerSpecs::simulate(EnergyPlusData &state,
                               const PlantLocation &EP_UNUSED(calledFromLocation),
                               bool const EP_UNUSED(FirstHVACIteration),
                               Real64 &CurLoad,
                               bool const RunFlag)
    {
        auto &sim_component(DataPlant::PlantLoop(this->LoopNum).LoopSide(this->LoopSideNum).Branch(this->BranchNum).Comp(this->CompNum));
        this->InitBoiler(state.dataBranchInputManager);
        this->CalcBoilerModel(state, CurLoad, RunFlag, sim_component.FlowCtrl);
        this->UpdateBoilerRecords(CurLoad, RunFlag);
    }

    void BoilerSpecs::getDesignCapacities(const PlantLocation &EP_UNUSED(calledFromLocation), Real64 &MaxLoad, Real64 &MinLoad, Real64 &OptLoad)
    {
        MinLoad = this->NomCap * this->MinPartLoadRat;
        MaxLoad = this->NomCap * this->MaxPartLoadRat;
        OptLoad = this->NomCap * this->OptPartLoadRat;
    }

    void BoilerSpecs::getSizingFactor(Real64 &SizFactor)
    {
        SizFactor = this->SizFac;
    }

    void BoilerSpecs::onInitLoopEquip(EnergyPlusData &state, const PlantLocation &EP_UNUSED(calledFromLocation))
    {
        this->InitBoiler(state.dataBranchInputManager);
        this->SizeBoiler();
    }

    void GetBoilerInput(EnergyPlusData &state, BoilersData &boilers)
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

        // Locals
        static std::string const RoutineName("GetBoilerInput: ");

        // LOCAL VARIABLES
        bool ErrorsFound(false); // Flag to show errors were found during GetInput

        // GET NUMBER OF ALL EQUIPMENT
        DataIPShortCuts::cCurrentModuleObject = "Boiler:HotWater";
        boilers.numBoilers = inputProcessor->getNumObjectsFound(DataIPShortCuts::cCurrentModuleObject);

        if (boilers.numBoilers <= 0) {
            ShowSevereError("No " + DataIPShortCuts::cCurrentModuleObject + " Equipment specified in input file");
            ErrorsFound = true;
        }

        // See if load distribution manager has already gotten the input
        if (allocated(boilers.Boiler)) return;

        boilers.Boiler.allocate(boilers.numBoilers);

        // LOAD ARRAYS WITH CURVE FIT Boiler DATA

        for (int BoilerNum = 1; BoilerNum <= boilers.numBoilers; ++BoilerNum) {
            int NumAlphas; // Number of elements in the alpha array
            int NumNums;   // Number of elements in the numeric array
            int IOStat;    // IO Status when calling get input subroutine
            inputProcessor->getObjectItem(DataIPShortCuts::cCurrentModuleObject,
                                          BoilerNum,
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
            GlobalNames::VerifyUniqueBoilerName(
                DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs(1), ErrorsFound, DataIPShortCuts::cCurrentModuleObject + " Name");
            auto &thisBoiler = boilers.Boiler(BoilerNum);
            thisBoiler.Name = DataIPShortCuts::cAlphaArgs(1);
            thisBoiler.TypeNum = DataPlant::TypeOf_Boiler_Simple;

            // Validate fuel type input
            bool FuelTypeError(false);
            UtilityRoutines::ValidateFuelTypeWithAssignResourceTypeNum(
                DataIPShortCuts::cAlphaArgs(2), thisBoiler.BoilerFuelTypeForOutputVariable, thisBoiler.FuelType, FuelTypeError);
            if (FuelTypeError) {
                ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\",");
                ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(2) + '=' + DataIPShortCuts::cAlphaArgs(2));
                // Set to Electric to avoid errors when setting up output variables
                thisBoiler.BoilerFuelTypeForOutputVariable = "Electricity";
                thisBoiler.FuelType = DataGlobalConstants::AssignResourceTypeNum("ELECTRICITY");
                ErrorsFound = true;
                FuelTypeError = false;
            }

            thisBoiler.NomCap = DataIPShortCuts::rNumericArgs(1);
            if (DataIPShortCuts::rNumericArgs(1) == 0.0) {
                ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\",");
                ShowContinueError("Invalid " + DataIPShortCuts::cNumericFieldNames(1) + '=' +
                                  General::RoundSigDigits(DataIPShortCuts::rNumericArgs(1), 2));
                ShowContinueError("..." + DataIPShortCuts::cNumericFieldNames(1) + " must be greater than 0.0");
                ErrorsFound = true;
            }
            if (thisBoiler.NomCap == DataSizing::AutoSize) {
                thisBoiler.NomCapWasAutoSized = true;
            }

            thisBoiler.NomEffic = DataIPShortCuts::rNumericArgs(2);
            if (DataIPShortCuts::rNumericArgs(2) == 0.0) {
                ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\",");
                ShowContinueError("Invalid " + DataIPShortCuts::cNumericFieldNames(2) + '=' +
                                  General::RoundSigDigits(DataIPShortCuts::rNumericArgs(2), 3));
                ShowSevereError("..." + DataIPShortCuts::cNumericFieldNames(2) + " must be greater than 0.0");
                ErrorsFound = true;
            }

            if (DataIPShortCuts::cAlphaArgs(3) == "ENTERINGBOILER") {
                thisBoiler.CurveTempMode = TempMode::ENTERINGBOILERTEMP;
            } else if (DataIPShortCuts::cAlphaArgs(3) == "LEAVINGBOILER") {
                thisBoiler.CurveTempMode = TempMode::LEAVINGBOILERTEMP;
            } else {
                thisBoiler.CurveTempMode = TempMode::NOTSET;
            }

            thisBoiler.EfficiencyCurvePtr = CurveManager::GetCurveIndex(state, DataIPShortCuts::cAlphaArgs(4));
            if (thisBoiler.EfficiencyCurvePtr > 0) {
                ErrorsFound |= CurveManager::CheckCurveDims(state,
                                                            thisBoiler.EfficiencyCurvePtr,         // Curve index
                                                            {1, 2},                                // Valid dimensions
                                                            RoutineName,                           // Routine name
                                                            DataIPShortCuts::cCurrentModuleObject, // Object Type
                                                            thisBoiler.Name,                       // Object Name
                                                            DataIPShortCuts::cAlphaFieldNames(4)); // Field Name

                // if curve uses temperature, make sure water temp mode has been set
                if (state.dataCurveManager->PerfCurve(thisBoiler.EfficiencyCurvePtr).NumDims == 2) { // curve uses water temperature
                    if (thisBoiler.CurveTempMode == TempMode::NOTSET) {                    // throw error
                        if (!DataIPShortCuts::lAlphaFieldBlanks(3)) {
                            ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\",");
                            ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(3) + '=' + DataIPShortCuts::cAlphaArgs(3));
                            ShowContinueError("boilers.Boiler using curve type of " +
                                              state.dataCurveManager->PerfCurve(thisBoiler.EfficiencyCurvePtr).ObjectType + " must specify " +
                                              DataIPShortCuts::cAlphaFieldNames(3));
                            ShowContinueError("Available choices are EnteringBoiler or LeavingBoiler");
                        } else {
                            ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\",");
                            ShowContinueError("Field " + DataIPShortCuts::cAlphaFieldNames(3) + " is blank");
                            ShowContinueError("boilers.Boiler using curve type of " +
                                              state.dataCurveManager->PerfCurve(thisBoiler.EfficiencyCurvePtr).ObjectType +
                                              " must specify either EnteringBoiler or LeavingBoiler");
                        }
                        ErrorsFound = true;
                    }
                }

            } else if (!DataIPShortCuts::lAlphaFieldBlanks(4)) {
                ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\",");
                ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(4) + '=' + DataIPShortCuts::cAlphaArgs(4));
                ShowSevereError("..." + DataIPShortCuts::cAlphaFieldNames(4) + " not found.");
                ErrorsFound = true;
            }
            thisBoiler.VolFlowRate = DataIPShortCuts::rNumericArgs(3);
            if (thisBoiler.VolFlowRate == DataSizing::AutoSize) {
                thisBoiler.VolFlowRateWasAutoSized = true;
            }
            thisBoiler.MinPartLoadRat = DataIPShortCuts::rNumericArgs(4);
            thisBoiler.MaxPartLoadRat = DataIPShortCuts::rNumericArgs(5);
            thisBoiler.OptPartLoadRat = DataIPShortCuts::rNumericArgs(6);

            thisBoiler.TempUpLimitBoilerOut = DataIPShortCuts::rNumericArgs(7);
            // default to 99.9C if upper temperature limit is left blank.
            if (thisBoiler.TempUpLimitBoilerOut <= 0.0) {
                thisBoiler.TempUpLimitBoilerOut = 99.9;
            }

            thisBoiler.ParasiticElecLoad = DataIPShortCuts::rNumericArgs(8);
            thisBoiler.SizFac = DataIPShortCuts::rNumericArgs(9);
            if (thisBoiler.SizFac == 0.0) thisBoiler.SizFac = 1.0;

            thisBoiler.BoilerInletNodeNum = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(5),
                                                                                ErrorsFound,
                                                                                DataIPShortCuts::cCurrentModuleObject,
                                                                                DataIPShortCuts::cAlphaArgs(1),
                                                                                DataLoopNode::NodeType_Water,
                                                                                DataLoopNode::NodeConnectionType_Inlet,
                                                                                1,
                                                                                DataLoopNode::ObjectIsNotParent);
            thisBoiler.BoilerOutletNodeNum = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(6),
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
                                               "Hot Water Nodes");

            if (DataIPShortCuts::cAlphaArgs(7) == "CONSTANTFLOW") {
                thisBoiler.FlowMode = DataPlant::FlowMode::CONSTANT;
            } else if (DataIPShortCuts::cAlphaArgs(7) == "LEAVINGSETPOINTMODULATED") {
                thisBoiler.FlowMode = DataPlant::FlowMode::LEAVINGSETPOINTMODULATED;
            } else if (DataIPShortCuts::cAlphaArgs(7) == "NOTMODULATED") {
                thisBoiler.FlowMode = DataPlant::FlowMode::NOTMODULATED;
            } else {
                ShowSevereError(RoutineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs(1) + "\",");
                ShowContinueError("Invalid " + DataIPShortCuts::cAlphaFieldNames(7) + '=' + DataIPShortCuts::cAlphaArgs(7));
                ShowContinueError("Available choices are ConstantFlow, NotModulated, or LeavingSetpointModulated");
                ShowContinueError("Flow mode NotModulated is assumed and the simulation continues.");
                // We will assume variable flow if not specified
                thisBoiler.FlowMode = DataPlant::FlowMode::NOTMODULATED;
            }

            if (NumAlphas > 7) {
                thisBoiler.EndUseSubcategory = DataIPShortCuts::cAlphaArgs(8);
            } else {
                thisBoiler.EndUseSubcategory = "Boiler"; // leave this as "boiler" instead of "general" like other end use subcategories since
                                                         // it appears this way in existing output files.
            }
        }

        if (ErrorsFound) {
            ShowFatalError(RoutineName + "Errors found in processing " + DataIPShortCuts::cCurrentModuleObject + " input.");
        }
    }

    void BoilerSpecs::SetupOutputVars()
    {
        SetupOutputVariable("Boiler Heating Rate", OutputProcessor::Unit::W, this->BoilerLoad, "System", "Average", this->Name);
        SetupOutputVariable("Boiler Heating Energy",
                            OutputProcessor::Unit::J,
                            this->BoilerEnergy,
                            "System",
                            "Sum",
                            this->Name,
                            _,
                            "ENERGYTRANSFER",
                            "BOILERS",
                            _,
                            "Plant");
        SetupOutputVariable("Boiler " + this->BoilerFuelTypeForOutputVariable + " Rate",
                            OutputProcessor::Unit::W,
                            this->FuelUsed,
                            "System",
                            "Average",
                            this->Name);
        SetupOutputVariable("Boiler " + this->BoilerFuelTypeForOutputVariable + " Energy",
                            OutputProcessor::Unit::J,
                            this->FuelConsumed,
                            "System",
                            "Sum",
                            this->Name,
                            _,
                            this->BoilerFuelTypeForOutputVariable,
                            "Heating",
                            this->EndUseSubcategory,
                            "Plant");
        SetupOutputVariable("Boiler Inlet Temperature", OutputProcessor::Unit::C, this->BoilerInletTemp, "System", "Average", this->Name);
        SetupOutputVariable("Boiler Outlet Temperature", OutputProcessor::Unit::C, this->BoilerOutletTemp, "System", "Average", this->Name);
        SetupOutputVariable("Boiler Mass Flow Rate", OutputProcessor::Unit::kg_s, this->BoilerMassFlowRate, "System", "Average", this->Name);
        SetupOutputVariable("Boiler Ancillary Electricity Rate", OutputProcessor::Unit::W, this->ParasiticElecPower, "System", "Average", this->Name);
        SetupOutputVariable("Boiler Ancillary Electricity Energy",
                            OutputProcessor::Unit::J,
                            this->ParasiticElecConsumption,
                            "System",
                            "Sum",
                            this->Name,
                            _,
                            "ELECTRICITY",
                            "Heating",
                            "Boiler Parasitic",
                            "Plant");
        SetupOutputVariable("Boiler Part Load Ratio", OutputProcessor::Unit::None, this->BoilerPLR, "System", "Average", this->Name);
        SetupOutputVariable("Boiler Efficiency", OutputProcessor::Unit::None, this->BoilerEff, "System", "Average", this->Name);
        if (DataGlobals::AnyEnergyManagementSystemInModel) {
            SetupEMSInternalVariable("Boiler Nominal Capacity", this->Name, "[W]", this->NomCap);
        }
    }

    void BoilerSpecs::InitBoiler(BranchInputManagerData &dataBranchInputManager) // number of the current boiler being simulated
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

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("InitBoiler");

        // Init more variables
        if (this->MyFlag) {

            // setup the output variable pointers
            this->SetupOutputVars();

            // Locate the boilers on the plant loops for later usage
            bool errFlag = false;
            PlantUtilities::ScanPlantLoopsForObject(dataBranchInputManager,
                                                    this->Name,
                                                    DataPlant::TypeOf_Boiler_Simple,
                                                    this->LoopNum,
                                                    this->LoopSideNum,
                                                    this->BranchNum,
                                                    this->CompNum,
                                                    errFlag,
                                                    _,
                                                    this->TempUpLimitBoilerOut,
                                                    _,
                                                    _,
                                                    _);
            if (errFlag) {
                ShowFatalError("InitBoiler: Program terminated due to previous condition(s).");
            }

            if ((this->FlowMode == DataPlant::FlowMode::LEAVINGSETPOINTMODULATED) || (this->FlowMode == DataPlant::FlowMode::CONSTANT)) {
                // reset flow priority
                DataPlant::PlantLoop(this->LoopNum).LoopSide(this->LoopSideNum).Branch(this->BranchNum).Comp(this->CompNum).FlowPriority =
                    DataPlant::LoopFlowStatus_NeedyIfLoopOn;
            }

            this->MyFlag = false;
        }

        if (this->MyEnvrnFlag && DataGlobals::BeginEnvrnFlag && (DataPlant::PlantFirstSizesOkayToFinalize)) {
            // if ( ! PlantFirstSizeCompleted ) SizeBoiler( BoilerNum );
            Real64 const rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->LoopNum).FluidName,
                                                                 DataGlobals::HWInitConvTemp,
                                                                 DataPlant::PlantLoop(this->LoopNum).FluidIndex,
                                                                 RoutineName);
            this->DesMassFlowRate = this->VolFlowRate * rho;

            PlantUtilities::InitComponentNodes(0.0,
                                               this->DesMassFlowRate,
                                               this->BoilerInletNodeNum,
                                               this->BoilerOutletNodeNum,
                                               this->LoopNum,
                                               this->LoopSideNum,
                                               this->BranchNum,
                                               this->CompNum);

            if (this->FlowMode == DataPlant::FlowMode::LEAVINGSETPOINTMODULATED) { // check if setpoint on outlet node
                if ((DataLoopNode::Node(this->BoilerOutletNodeNum).TempSetPoint == DataLoopNode::SensedNodeFlagValue) &&
                    (DataLoopNode::Node(this->BoilerOutletNodeNum).TempSetPointLo == DataLoopNode::SensedNodeFlagValue)) {
                    if (!DataGlobals::AnyEnergyManagementSystemInModel) {
                        if (!this->ModulatedFlowErrDone) {
                            ShowWarningError("Missing temperature setpoint for LeavingSetpointModulated mode Boiler named " + this->Name);
                            ShowContinueError(
                                "  A temperature setpoint is needed at the outlet node of a boiler in variable flow mode, use a SetpointManager");
                            ShowContinueError("  The overall loop setpoint will be assumed for Boiler. The simulation continues ... ");
                            this->ModulatedFlowErrDone = true;
                        }
                    } else {
                        // need call to EMS to check node
                        bool FatalError = false; // but not really fatal yet, but should be.
                        EMSManager::CheckIfNodeSetPointManagedByEMS(this->BoilerOutletNodeNum, EMSManager::iTemperatureSetPoint, FatalError);
                        DataLoopNode::NodeSetpointCheck(this->BoilerOutletNodeNum).needsSetpointChecking = false;
                        if (FatalError) {
                            if (!this->ModulatedFlowErrDone) {
                                ShowWarningError("Missing temperature setpoint for LeavingSetpointModulated mode Boiler named " + this->Name);
                                ShowContinueError("  A temperature setpoint is needed at the outlet node of a boiler in variable flow mode");
                                ShowContinueError("  use a Setpoint Manager to establish a setpoint at the boiler outlet node ");
                                ShowContinueError("  or use an EMS actuator to establish a setpoint at the boiler outlet node ");
                                ShowContinueError("  The overall loop setpoint will be assumed for Boiler. The simulation continues ... ");
                                this->ModulatedFlowErrDone = true;
                            }
                        }
                    }
                    this->ModulatedFlowSetToLoop = true; // this is for backward compatibility and could be removed
                }
            }

            this->MyEnvrnFlag = false;
        }

        if (!DataGlobals::BeginEnvrnFlag) {
            this->MyEnvrnFlag = true;
        }

        // every iteration inits.  (most in calc routine)

        if ((this->FlowMode == DataPlant::FlowMode::LEAVINGSETPOINTMODULATED) && this->ModulatedFlowSetToLoop) {
            // fix for clumsy old input that worked because loop setpoint was spread.
            //  could be removed with transition, testing , model change, period of being obsolete.
            if (DataPlant::PlantLoop(this->LoopNum).LoopDemandCalcScheme == DataPlant::SingleSetPoint) {
                DataLoopNode::Node(this->BoilerOutletNodeNum).TempSetPoint =
                    DataLoopNode::Node(DataPlant::PlantLoop(this->LoopNum).TempSetPointNodeNum).TempSetPoint;
            } else { // DataPlant::DualSetPointDeadBand
                DataLoopNode::Node(this->BoilerOutletNodeNum).TempSetPointLo =
                    DataLoopNode::Node(DataPlant::PlantLoop(this->LoopNum).TempSetPointNodeNum).TempSetPointLo;
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

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("SizeBoiler");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        bool ErrorsFound(false); // If errors detected in input

        // grab some initial values for capacity and flow rate
        Real64 tmpNomCap = this->NomCap;                 // local nominal capacity cooling power
        Real64 tmpBoilerVolFlowRate = this->VolFlowRate; // local boiler design volume flow rate

        int const PltSizNum = DataPlant::PlantLoop(this->LoopNum).PlantSizNum; // Plant Sizing index corresponding to CurLoopNum

        if (PltSizNum > 0) {
            if (DataSizing::PlantSizData(PltSizNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {

                Real64 const rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->LoopNum).FluidName,
                                                                     DataGlobals::HWInitConvTemp,
                                                                     DataPlant::PlantLoop(this->LoopNum).FluidIndex,
                                                                     RoutineName);
                Real64 const Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(this->LoopNum).FluidName,
                                                                         DataGlobals::HWInitConvTemp,
                                                                         DataPlant::PlantLoop(this->LoopNum).FluidIndex,
                                                                         RoutineName);
                tmpNomCap = Cp * rho * this->SizFac * DataSizing::PlantSizData(PltSizNum).DeltaT * DataSizing::PlantSizData(PltSizNum).DesVolFlowRate;
            } else {
                if (this->NomCapWasAutoSized) tmpNomCap = 0.0;
            }
            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                if (this->NomCapWasAutoSized) {
                    this->NomCap = tmpNomCap;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput("Boiler:HotWater", this->Name, "Design Size Nominal Capacity [W]", tmpNomCap);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput("Boiler:HotWater", this->Name, "Initial Design Size Nominal Capacity [W]", tmpNomCap);
                    }
                } else { // Hard-sized with sizing data
                    if (this->NomCap > 0.0 && tmpNomCap > 0.0) {
                        Real64 const NomCapUser = this->NomCap; // Hardsized nominal capacity for reporting
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput("Boiler:HotWater",
                                                                    this->Name,
                                                                    "Design Size Nominal Capacity [W]",
                                                                    tmpNomCap,
                                                                    "User-Specified Nominal Capacity [W]",
                                                                    NomCapUser);
                            if (DataGlobals::DisplayExtraWarnings) {
                                if ((std::abs(tmpNomCap - NomCapUser) / NomCapUser) > DataSizing::AutoVsHardSizingThreshold) {
                                    ShowMessage("SizeBoilerHotWater: Potential issue with equipment sizing for " + this->Name);
                                    ShowContinueError("User-Specified Nominal Capacity of " + General::RoundSigDigits(NomCapUser, 2) + " [W]");
                                    ShowContinueError("differs from Design Size Nominal Capacity of " + General::RoundSigDigits(tmpNomCap, 2) +
                                                      " [W]");
                                    ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                        }
                    }
                }
            }
        } else {
            if (this->NomCapWasAutoSized && DataPlant::PlantFirstSizesOkayToFinalize) {
                ShowSevereError("Autosizing of Boiler nominal capacity requires a loop Sizing:Plant object");
                ShowContinueError("Occurs in Boiler object=" + this->Name);
                ErrorsFound = true;
            }
            if (!this->NomCapWasAutoSized && DataPlant::PlantFinalSizesOkayToReport && (this->NomCap > 0.0)) { // Hard-sized with no sizing data
                ReportSizingManager::ReportSizingOutput("Boiler:HotWater", this->Name, "User-Specified Nominal Capacity [W]", this->NomCap);
            }
        }

        if (PltSizNum > 0) {
            if (DataSizing::PlantSizData(PltSizNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                tmpBoilerVolFlowRate = DataSizing::PlantSizData(PltSizNum).DesVolFlowRate * this->SizFac;
            } else {
                if (this->VolFlowRateWasAutoSized) tmpBoilerVolFlowRate = 0.0;
            }
            if (DataPlant::PlantFirstSizesOkayToFinalize) {
                if (this->VolFlowRateWasAutoSized) {
                    this->VolFlowRate = tmpBoilerVolFlowRate;
                    if (DataPlant::PlantFinalSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(
                            "Boiler:HotWater", this->Name, "Design Size Design Water Flow Rate [m3/s]", tmpBoilerVolFlowRate);
                    }
                    if (DataPlant::PlantFirstSizesOkayToReport) {
                        ReportSizingManager::ReportSizingOutput(
                            "Boiler:HotWater", this->Name, "Initial Design Size Design Water Flow Rate [m3/s]", tmpBoilerVolFlowRate);
                    }
                } else {
                    if (this->VolFlowRate > 0.0 && tmpBoilerVolFlowRate > 0.0) {
                        Real64 VolFlowRateUser = this->VolFlowRate; // Hardsized volume flow for reporting
                        if (DataPlant::PlantFinalSizesOkayToReport) {
                            ReportSizingManager::ReportSizingOutput("Boiler:HotWater",
                                                                    this->Name,
                                                                    "Design Size Design Water Flow Rate [m3/s]",
                                                                    tmpBoilerVolFlowRate,
                                                                    "User-Specified Design Water Flow Rate [m3/s]",
                                                                    VolFlowRateUser);
                            if (DataGlobals::DisplayExtraWarnings) {
                                if ((std::abs(tmpBoilerVolFlowRate - VolFlowRateUser) / VolFlowRateUser) > DataSizing::AutoVsHardSizingThreshold) {
                                    ShowMessage("SizeBoilerHotWater: Potential issue with equipment sizing for " + this->Name);
                                    ShowContinueError("User-Specified Design Water Flow Rate of " + General::RoundSigDigits(VolFlowRateUser, 2) +
                                                      " [m3/s]");
                                    ShowContinueError("differs from Design Size Design Water Flow Rate of " +
                                                      General::RoundSigDigits(tmpBoilerVolFlowRate, 2) + " [m3/s]");
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
            if (this->VolFlowRateWasAutoSized && DataPlant::PlantFirstSizesOkayToFinalize) {
                ShowSevereError("Autosizing of Boiler design flow rate requires a loop Sizing:Plant object");
                ShowContinueError("Occurs in Boiler object=" + this->Name);
                ErrorsFound = true;
            }
            if (!this->VolFlowRateWasAutoSized && DataPlant::PlantFinalSizesOkayToReport &&
                (this->VolFlowRate > 0.0)) { // Hard-sized with no sizing data
                ReportSizingManager::ReportSizingOutput(
                    "Boiler:HotWater", this->Name, "User-Specified Design Water Flow Rate [m3/s]", this->VolFlowRate);
            }
        }

        PlantUtilities::RegisterPlantCompDesignFlow(this->BoilerInletNodeNum, tmpBoilerVolFlowRate);

        if (DataPlant::PlantFinalSizesOkayToReport) {
            // create predefined report
            std::string const equipName = this->Name;
            OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchMechType, equipName, "Boiler:HotWater");
            OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchMechNomEff, equipName, this->NomEffic);
            OutputReportPredefined::PreDefTableEntry(OutputReportPredefined::pdchMechNomCap, equipName, this->NomCap);
        }

        if (ErrorsFound) {
            ShowFatalError("Preceding sizing errors cause program termination");
        }
    }

    void BoilerSpecs::CalcBoilerModel(EnergyPlusData &state,
                                      Real64 const MyLoad,    // W - hot water demand to be met by boiler
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

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("CalcBoilerModel");

        // clean up some operating conditions, may not be necessary
        this->BoilerLoad = 0.0;
        this->ParasiticElecPower = 0.0;
        this->BoilerMassFlowRate = 0.0;

        int const BoilerInletNode = this->BoilerInletNodeNum;
        int const BoilerOutletNode = this->BoilerOutletNodeNum;
        Real64 BoilerNomCap = this->NomCap;                         // W - boiler nominal capacity
        Real64 const BoilerMaxPLR = this->MaxPartLoadRat;           // boiler maximum part load ratio
        Real64 const BoilerMinPLR = this->MinPartLoadRat;           // boiler minimum part load ratio
        Real64 BoilerNomEff = this->NomEffic;                             // boiler efficiency
        Real64 const TempUpLimitBout = this->TempUpLimitBoilerOut;  // C - boiler high temperature limit
        Real64 const BoilerMassFlowRateMax = this->DesMassFlowRate; // Max Design Boiler Mass Flow Rate converted from Volume Flow Rate

        Real64 Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(this->LoopNum).FluidName,
                                                           DataLoopNode::Node(BoilerInletNode).Temp,
                                                           DataPlant::PlantLoop(this->LoopNum).FluidIndex,
                                                           RoutineName);

        // If the specified load is 0.0 or the boiler should not run then we leave this subroutine. Before leaving
        // if the component control is SERIESACTIVE we set the component flow to inlet flow so that flow resolver
        // will not shut down the branch
        if (MyLoad <= 0.0 || !RunFlag) {
            if (EquipFlowCtrl == DataBranchAirLoopPlant::ControlType_SeriesActive)
                this->BoilerMassFlowRate = DataLoopNode::Node(BoilerInletNode).MassFlowRate;
            return;
        }

        // If there is a fault of boiler fouling (zrp_Nov2016)
        if (this->FaultyBoilerFoulingFlag && (!DataGlobals::WarmupFlag) && (!DataGlobals::DoingSizing) && (!DataGlobals::KickOffSimulation)) {
            int FaultIndex = this->FaultyBoilerFoulingIndex;
            Real64 NomCap_ff = BoilerNomCap;
            Real64 BoilerNomEff_ff = BoilerNomEff;

            // calculate the Faulty Boiler Fouling Factor using fault information
            this->FaultyBoilerFoulingFactor = FaultsManager::FaultsBoilerFouling(FaultIndex).CalFoulingFactor();

            // update the boiler nominal capacity at faulty cases
            BoilerNomCap = NomCap_ff * this->FaultyBoilerFoulingFactor;
            BoilerNomEff = BoilerNomEff_ff * this->FaultyBoilerFoulingFactor;
        }

        // Set the current load equal to the boiler load
        this->BoilerLoad = MyLoad;

        // Initialize the delta temperature to zero
        Real64 BoilerDeltaTemp; // C - boiler inlet to outlet temperature difference, set in all necessary code paths so no initialization required

        if (DataPlant::PlantLoop(this->LoopNum).LoopSide(this->LoopSideNum).FlowLock == 0) {
            // Either set the flow to the Constant value or calculate the flow for the variable volume
            if ((this->FlowMode == DataPlant::FlowMode::CONSTANT) || (this->FlowMode == DataPlant::FlowMode::NOTMODULATED)) {
                // Then find the flow rate and outlet temp
                this->BoilerMassFlowRate = BoilerMassFlowRateMax;
                PlantUtilities::SetComponentFlowRate(
                    this->BoilerMassFlowRate, BoilerInletNode, BoilerOutletNode, this->LoopNum, this->LoopSideNum, this->BranchNum, this->CompNum);

                if ((this->BoilerMassFlowRate != 0.0) && (MyLoad > 0.0)) {
                    BoilerDeltaTemp = this->BoilerLoad / this->BoilerMassFlowRate / Cp;
                } else {
                    BoilerDeltaTemp = 0.0;
                }
                this->BoilerOutletTemp = BoilerDeltaTemp + DataLoopNode::Node(BoilerInletNode).Temp;

            } else if (this->FlowMode == DataPlant::FlowMode::LEAVINGSETPOINTMODULATED) {
                // Calculate the Delta Temp from the inlet temp to the boiler outlet setpoint
                // Then find the flow rate and outlet temp

                if (DataPlant::PlantLoop(this->LoopNum).LoopDemandCalcScheme == DataPlant::SingleSetPoint) {
                    BoilerDeltaTemp = DataLoopNode::Node(BoilerOutletNode).TempSetPoint - DataLoopNode::Node(BoilerInletNode).Temp;
                } else { // DataPlant::DualSetPointDeadBand
                    BoilerDeltaTemp = DataLoopNode::Node(BoilerOutletNode).TempSetPointLo - DataLoopNode::Node(BoilerInletNode).Temp;
                }

                this->BoilerOutletTemp = BoilerDeltaTemp + DataLoopNode::Node(BoilerInletNode).Temp;

                if ((BoilerDeltaTemp > 0.0) && (this->BoilerLoad > 0.0)) {
                    this->BoilerMassFlowRate = this->BoilerLoad / Cp / BoilerDeltaTemp;
                    this->BoilerMassFlowRate = min(BoilerMassFlowRateMax, this->BoilerMassFlowRate);
                } else {
                    this->BoilerMassFlowRate = 0.0;
                }
                PlantUtilities::SetComponentFlowRate(
                    this->BoilerMassFlowRate, BoilerInletNode, BoilerOutletNode, this->LoopNum, this->LoopSideNum, this->BranchNum, this->CompNum);

            } // End of Constant/Variable Flow If Block

        } else { // If FlowLock is True
            // Set the boiler flow rate from inlet node and then check performance
            this->BoilerMassFlowRate = DataLoopNode::Node(BoilerInletNode).MassFlowRate;

            if ((MyLoad > 0.0) && (this->BoilerMassFlowRate > 0.0)) { // this boiler has a heat load
                this->BoilerLoad = MyLoad;
                if (this->BoilerLoad > BoilerNomCap * BoilerMaxPLR) this->BoilerLoad = BoilerNomCap * BoilerMaxPLR;
                if (this->BoilerLoad < BoilerNomCap * BoilerMinPLR) this->BoilerLoad = BoilerNomCap * BoilerMinPLR;
                this->BoilerOutletTemp = DataLoopNode::Node(BoilerInletNode).Temp + this->BoilerLoad / (this->BoilerMassFlowRate * Cp);
            } else {
                this->BoilerLoad = 0.0;
                this->BoilerOutletTemp = DataLoopNode::Node(BoilerInletNode).Temp;
            }
        }

        // Limit BoilerOutletTemp.  If > max temp, trip boiler off
        if (this->BoilerOutletTemp > TempUpLimitBout) {
            this->BoilerLoad = 0.0;
            this->BoilerOutletTemp = DataLoopNode::Node(BoilerInletNode).Temp;
        }
        this->BoilerPLR = this->BoilerLoad / BoilerNomCap; // operating part load ratio
        this->BoilerPLR = min(this->BoilerPLR, BoilerMaxPLR);
        this->BoilerPLR = max(this->BoilerPLR, BoilerMinPLR);

        // calculate theoretical fuel use based on nominal thermal efficiency
        Real64 const TheorFuelUse = this->BoilerLoad / BoilerNomEff; // Theoretical (stoichiometric) fuel use
        Real64 EffCurveOutput = 1.0;                              // Output of boiler efficiency curve

        // calculate normalized efficiency based on curve object type
        if (this->EfficiencyCurvePtr > 0) {
            if (state.dataCurveManager->PerfCurve(this->EfficiencyCurvePtr).NumDims == 2) {
                if (this->CurveTempMode == TempMode::ENTERINGBOILERTEMP) {
                    EffCurveOutput = CurveManager::CurveValue(state, this->EfficiencyCurvePtr, this->BoilerPLR, DataLoopNode::Node(BoilerInletNode).Temp);
                } else if (this->CurveTempMode == TempMode::LEAVINGBOILERTEMP) {
                    EffCurveOutput = CurveManager::CurveValue(state, this->EfficiencyCurvePtr, this->BoilerPLR, this->BoilerOutletTemp);
                }
            } else {
                EffCurveOutput = CurveManager::CurveValue(state, this->EfficiencyCurvePtr, this->BoilerPLR);
            }
        }
        BoilerEff = EffCurveOutput * BoilerNomEff;

        // warn if efficiency curve produces zero or negative results
        if (!DataGlobals::WarmupFlag && EffCurveOutput <= 0.0) {
            if (this->BoilerLoad > 0.0) {
                if (this->EffCurveOutputError < 1) {
                    ++this->EffCurveOutputError;
                    ShowWarningError("Boiler:HotWater \"" + this->Name + "\"");
                    ShowContinueError("...Normalized Boiler Efficiency Curve output is less than or equal to 0.");
                    ShowContinueError("...Curve input x value (PLR)     = " + General::TrimSigDigits(this->BoilerPLR, 5));
                    if (state.dataCurveManager->PerfCurve(this->EfficiencyCurvePtr).NumDims == 2) {
                        if (this->CurveTempMode == TempMode::ENTERINGBOILERTEMP) {
                            ShowContinueError("...Curve input y value (Tinlet) = " +
                                              General::TrimSigDigits(DataLoopNode::Node(BoilerInletNode).Temp, 2));
                        } else if (this->CurveTempMode == TempMode::LEAVINGBOILERTEMP) {
                            ShowContinueError("...Curve input y value (Toutlet) = " + General::TrimSigDigits(this->BoilerOutletTemp, 2));
                        }
                    }
                    ShowContinueError("...Curve output (normalized eff) = " + General::TrimSigDigits(EffCurveOutput, 5));
                    ShowContinueError("...Calculated Boiler efficiency  = " + General::TrimSigDigits(BoilerEff, 5) +
                                      " (Boiler efficiency = Nominal Thermal Efficiency * Normalized Boiler Efficiency Curve output)");
                    ShowContinueErrorTimeStamp("...Curve output reset to 0.01 and simulation continues.");
                } else {
                    ShowRecurringWarningErrorAtEnd("Boiler:HotWater \"" + this->Name +
                                                       "\": Boiler Efficiency Curve output is less than or equal to 0 warning continues...",
                                                   this->EffCurveOutputIndex,
                                                   EffCurveOutput,
                                                   EffCurveOutput);
                }
            }
            EffCurveOutput = 0.01;
        }

        // warn if overall efficiency greater than 1.1
        if (!DataGlobals::WarmupFlag && BoilerEff > 1.1) {
            if (this->BoilerLoad > 0.0 && this->EfficiencyCurvePtr > 0) {
                if (this->CalculatedEffError < 1) {
                    ++this->CalculatedEffError;
                    ShowWarningError("Boiler:HotWater \"" + this->Name + "\"");
                    ShowContinueError("...Calculated Boiler Efficiency is greater than 1.1.");
                    ShowContinueError("...Boiler Efficiency calculations shown below.");
                    ShowContinueError("...Curve input x value (PLR)     = " + General::TrimSigDigits(this->BoilerPLR, 5));
                    if (state.dataCurveManager->PerfCurve(this->EfficiencyCurvePtr).NumDims == 2) {
                        if (this->CurveTempMode == TempMode::ENTERINGBOILERTEMP) {
                            ShowContinueError("...Curve input y value (Tinlet) = " +
                                              General::TrimSigDigits(DataLoopNode::Node(BoilerInletNode).Temp, 2));
                        } else if (this->CurveTempMode == TempMode::LEAVINGBOILERTEMP) {
                            ShowContinueError("...Curve input y value (Toutlet) = " + General::TrimSigDigits(this->BoilerOutletTemp, 2));
                        }
                    }
                    ShowContinueError("...Curve output (normalized eff) = " + General::TrimSigDigits(EffCurveOutput, 5));
                    ShowContinueError("...Calculated Boiler efficiency  = " + General::TrimSigDigits(BoilerEff, 5) +
                                      " (Boiler efficiency = Nominal Thermal Efficiency * Normalized Boiler Efficiency Curve output)");
                    ShowContinueErrorTimeStamp("...Curve output reset to 1.1 and simulation continues.");
                } else {
                    ShowRecurringWarningErrorAtEnd("Boiler:HotWater \"" + this->Name +
                                                       "\": Calculated Boiler Efficiency is greater than 1.1 warning continues...",
                                                   this->CalculatedEffIndex,
                                                   BoilerEff,
                                                   BoilerEff);
                }
            }
            EffCurveOutput = 1.1;
        }

        // calculate fuel used based on normalized boiler efficiency curve (=1 when no curve used)
        this->FuelUsed = TheorFuelUse / EffCurveOutput;
        if (this->BoilerLoad > 0.0) this->ParasiticElecPower = this->ParasiticElecLoad * this->BoilerPLR;
    }

    void BoilerSpecs::UpdateBoilerRecords(Real64 const MyLoad, // boiler operating load
                                          bool const RunFlag   // boiler on when TRUE
    )
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Dan Fisher
        //       DATE WRITTEN:    October 1998

        // PURPOSE OF THIS SUBROUTINE:
        // boiler simulation reporting

        Real64 const ReportingConstant = DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
        int const BoilerInletNode = this->BoilerInletNodeNum;
        int const BoilerOutletNode = this->BoilerOutletNodeNum;

        if (MyLoad <= 0 || !RunFlag) {
            PlantUtilities::SafeCopyPlantNode(BoilerInletNode, BoilerOutletNode);
            DataLoopNode::Node(BoilerOutletNode).Temp = DataLoopNode::Node(BoilerInletNode).Temp;
            this->BoilerOutletTemp = DataLoopNode::Node(BoilerInletNode).Temp;
            this->BoilerLoad = 0.0;
            this->FuelUsed = 0.0;
            this->ParasiticElecPower = 0.0;
            this->BoilerPLR = 0.0;
            this->BoilerEff = 0.0;
        } else {
            PlantUtilities::SafeCopyPlantNode(BoilerInletNode, BoilerOutletNode);
            DataLoopNode::Node(BoilerOutletNode).Temp = this->BoilerOutletTemp;
        }

        this->BoilerInletTemp = DataLoopNode::Node(BoilerInletNode).Temp;
        this->BoilerMassFlowRate = DataLoopNode::Node(BoilerOutletNode).MassFlowRate;
        this->BoilerEnergy = this->BoilerLoad * ReportingConstant;
        this->FuelConsumed = this->FuelUsed * ReportingConstant;
        this->ParasiticElecConsumption = this->ParasiticElecPower * ReportingConstant;
    }

} // namespace Boilers

} // namespace EnergyPlus
