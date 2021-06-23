// EnergyPlus, Copyright (c) 1996-2021, The Board of Trustees of the University of Illinois,
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
#include <EnergyPlus/Autosizing/Base.hh>
#include <EnergyPlus/BoilerSteam.hh>
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataBranchAirLoopPlant.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/EMSManager.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/UtilityRoutines.hh>

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

    const char *fluidNameSteam = "STEAM";

    PlantComponent *BoilerSpecs::factory(EnergyPlusData &state, std::string const &objectName)
    {
        // Process the input data for boilers if it hasn't been done already
        if (state.dataBoilerSteam->getSteamBoilerInput) {
            GetBoilerInput(state);
            state.dataBoilerSteam->getSteamBoilerInput = false;
        }

        // Now look for this particular pipe in the list
        for (auto &boiler : state.dataBoilerSteam->Boiler) {
            if (boiler.Name == objectName) {
                return &boiler;
            }
        }
        // If we didn't find it, fatal
        ShowFatalError(state, "LocalBoilerSteamFactory: Error getting inputs for steam boiler named: " + objectName); // LCOV_EXCL_LINE
        // Shut up the compiler
        return nullptr; // LCOV_EXCL_LINE
    }

    void BoilerSpecs::simulate(
        EnergyPlusData &state, [[maybe_unused]] const PlantLocation &calledFromLocation, bool FirstHVACIteration, Real64 &CurLoad, bool RunFlag)
    {
        this->initialize(state);
        auto &sim_component(state.dataPlnt->PlantLoop(this->LoopNum).LoopSide(this->LoopSideNum).Branch(this->BranchNum).Comp(this->CompNum));
        this->calculate(state, CurLoad, RunFlag, sim_component.FlowCtrl);
        this->update(state, CurLoad, RunFlag, FirstHVACIteration);
    }

    void
    BoilerSpecs::getDesignCapacities([[maybe_unused]] EnergyPlusData &state, const PlantLocation &, Real64 &MaxLoad, Real64 &MinLoad, Real64 &OptLoad)
    {
        MinLoad = this->NomCap * this->MinPartLoadRat;
        MaxLoad = this->NomCap * this->MaxPartLoadRat;
        OptLoad = this->NomCap * this->OptPartLoadRat;
    }

    void BoilerSpecs::getSizingFactor(Real64 &sizFac)
    {
        sizFac = this->SizFac;
    }

    void BoilerSpecs::onInitLoopEquip(EnergyPlusData &state, const PlantLocation &)
    {
        this->initialize(state);
        this->autosize(state);
    }

    void GetBoilerInput(EnergyPlusData &state)
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

        SteamFluidIndex = 0;
        state.dataIPShortCut->cCurrentModuleObject = "Boiler:Steam";
        state.dataBoilerSteam->numBoilers =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, state.dataIPShortCut->cCurrentModuleObject);

        if (state.dataBoilerSteam->numBoilers <= 0) {
            ShowSevereError(state, "No " + state.dataIPShortCut->cCurrentModuleObject + " equipment specified in input file");
            ErrorsFound = true;
        }

        // See if load distribution manager has already gotten the input
        if (allocated(state.dataBoilerSteam->Boiler)) return;

        // Boiler will have fuel input to it , that is it !
        state.dataBoilerSteam->Boiler.allocate(state.dataBoilerSteam->numBoilers);

        // LOAD ARRAYS WITH CURVE FIT Boiler DATA
        for (BoilerNum = 1; BoilerNum <= state.dataBoilerSteam->numBoilers; ++BoilerNum) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     state.dataIPShortCut->cCurrentModuleObject,
                                                                     BoilerNum,
                                                                     state.dataIPShortCut->cAlphaArgs,
                                                                     NumAlphas,
                                                                     state.dataIPShortCut->rNumericArgs,
                                                                     NumNums,
                                                                     IOStat,
                                                                     _,
                                                                     _,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            UtilityRoutines::IsNameEmpty(state, state.dataIPShortCut->cAlphaArgs(1), state.dataIPShortCut->cCurrentModuleObject, ErrorsFound);
            // ErrorsFound will be set to True if problem was found, left untouched otherwise
            GlobalNames::VerifyUniqueBoilerName(state,
                                                state.dataIPShortCut->cCurrentModuleObject,
                                                state.dataIPShortCut->cAlphaArgs(1),
                                                ErrorsFound,
                                                state.dataIPShortCut->cCurrentModuleObject + " Name");
            auto &thisBoiler = state.dataBoilerSteam->Boiler(BoilerNum);
            thisBoiler.Name = state.dataIPShortCut->cAlphaArgs(1);

            // Validate fuel type input
            bool FuelTypeError(false);
            UtilityRoutines::ValidateFuelTypeWithAssignResourceTypeNum(
                state.dataIPShortCut->cAlphaArgs(2), thisBoiler.BoilerFuelTypeForOutputVariable, thisBoiler.FuelType, FuelTypeError);
            if (FuelTypeError) {
                ShowSevereError(state,
                                RoutineName + state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\",");
                ShowContinueError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(2) + '=' + state.dataIPShortCut->cAlphaArgs(2));
                // Set to Electric to avoid errors when setting up output variables
                thisBoiler.BoilerFuelTypeForOutputVariable = "Electricity";
                ErrorsFound = true;
                FuelTypeError = false;
            }

            // INPUTS from the IDF file
            thisBoiler.BoilerMaxOperPress = state.dataIPShortCut->rNumericArgs(1);
            if (thisBoiler.BoilerMaxOperPress < 1e5) {
                ShowWarningMessage(state, state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\"");
                ShowContinueError(state, "Field: Maximum Operation Pressure units are Pa. Verify units.");
            }
            thisBoiler.NomEffic = state.dataIPShortCut->rNumericArgs(2);
            thisBoiler.TempUpLimitBoilerOut = state.dataIPShortCut->rNumericArgs(3);
            thisBoiler.NomCap = state.dataIPShortCut->rNumericArgs(4);
            if (thisBoiler.NomCap == DataSizing::AutoSize) {
                thisBoiler.NomCapWasAutoSized = true;
            }
            thisBoiler.MinPartLoadRat = state.dataIPShortCut->rNumericArgs(5);
            thisBoiler.MaxPartLoadRat = state.dataIPShortCut->rNumericArgs(6);
            thisBoiler.OptPartLoadRat = state.dataIPShortCut->rNumericArgs(7);
            thisBoiler.FullLoadCoef(1) = state.dataIPShortCut->rNumericArgs(8);
            thisBoiler.FullLoadCoef(2) = state.dataIPShortCut->rNumericArgs(9);
            thisBoiler.FullLoadCoef(3) = state.dataIPShortCut->rNumericArgs(10);
            thisBoiler.SizFac = state.dataIPShortCut->rNumericArgs(11);
            if (thisBoiler.SizFac <= 0.0) thisBoiler.SizFac = 1.0;

            if ((state.dataIPShortCut->rNumericArgs(8) + state.dataIPShortCut->rNumericArgs(9) + state.dataIPShortCut->rNumericArgs(10)) == 0.0) {
                ShowSevereError(state,
                                RoutineName + state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\",");
                ShowContinueError(state, " Sum of fuel use curve coefficients = 0.0");
                ErrorsFound = true;
            }

            if (state.dataIPShortCut->rNumericArgs(5) < 0.0) {
                ShowSevereError(state,
                                RoutineName + state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\",");
                ShowContinueError(state,
                                  format("Invalid {}={:.3R}", state.dataIPShortCut->cNumericFieldNames(5), state.dataIPShortCut->rNumericArgs(5)));
                ErrorsFound = true;
            }

            if (state.dataIPShortCut->rNumericArgs(3) == 0.0) {
                ShowSevereError(state,
                                RoutineName + state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\",");
                ShowContinueError(state,
                                  format("Invalid {}={:.3R}", state.dataIPShortCut->cNumericFieldNames(3), state.dataIPShortCut->rNumericArgs(3)));
                ErrorsFound = true;
            }
            thisBoiler.BoilerInletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                                                state.dataIPShortCut->cAlphaArgs(3),
                                                                                ErrorsFound,
                                                                                state.dataIPShortCut->cCurrentModuleObject,
                                                                                state.dataIPShortCut->cAlphaArgs(1),
                                                                                DataLoopNode::NodeFluidType::Steam,
                                                                                DataLoopNode::NodeConnectionType::Inlet,
                                                                                NodeInputManager::compFluidStream::Primary,
                                                                                DataLoopNode::ObjectIsNotParent);
            thisBoiler.BoilerOutletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                                                 state.dataIPShortCut->cAlphaArgs(4),
                                                                                 ErrorsFound,
                                                                                 state.dataIPShortCut->cCurrentModuleObject,
                                                                                 state.dataIPShortCut->cAlphaArgs(1),
                                                                                 DataLoopNode::NodeFluidType::Steam,
                                                                                 DataLoopNode::NodeConnectionType::Outlet,
                                                                                 NodeInputManager::compFluidStream::Primary,
                                                                                 DataLoopNode::ObjectIsNotParent);
            BranchNodeConnections::TestCompSet(state,
                                               state.dataIPShortCut->cCurrentModuleObject,
                                               state.dataIPShortCut->cAlphaArgs(1),
                                               state.dataIPShortCut->cAlphaArgs(3),
                                               state.dataIPShortCut->cAlphaArgs(4),
                                               "Hot Steam Nodes");

            if (SteamFluidIndex == 0 && BoilerNum == 1) {
                SteamFluidIndex = FluidProperties::FindRefrigerant(state, fluidNameSteam);
                if (SteamFluidIndex == 0) {
                    ShowSevereError(state,
                                    RoutineName + state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\",");
                    ShowContinueError(state, "Steam Properties not found; Steam Fluid Properties must be included in the input file.");
                    ErrorsFound = true;
                }
            }

            thisBoiler.FluidIndex = SteamFluidIndex;

            if (NumAlphas > 4) {
                thisBoiler.EndUseSubcategory = state.dataIPShortCut->cAlphaArgs(5);
            } else {
                thisBoiler.EndUseSubcategory = "General";
            }
        }

        if (ErrorsFound) {
            ShowFatalError(state, RoutineName + "Errors found in processing " + state.dataIPShortCut->cCurrentModuleObject + " input.");
        }
    }

    void BoilerSpecs::oneTimeInit(EnergyPlusData &state)
    {
        bool errFlag = false;
        PlantUtilities::ScanPlantLoopsForObject(state,
                                                this->Name,
                                                DataPlant::TypeOf_Boiler_Steam,
                                                this->LoopNum,
                                                this->LoopSideNum,
                                                this->BranchNum,
                                                this->CompNum,
                                                errFlag,
                                                _,
                                                _,
                                                _,
                                                _,
                                                _);
        if (errFlag) {
            ShowFatalError(state, "InitBoiler: Program terminated due to previous condition(s).");
        }
    }

    void BoilerSpecs::initEachEnvironment(EnergyPlusData &state)
    {
        static std::string const RoutineName("BoilerSpecs::initEachEnvironment");

        int BoilerInletNode = this->BoilerInletNodeNum;

        Real64 EnthSteamOutDry =
            FluidProperties::GetSatEnthalpyRefrig(state, fluidNameSteam, this->TempUpLimitBoilerOut, 1.0, this->FluidIndex, RoutineName);
        Real64 EnthSteamOutWet =
            FluidProperties::GetSatEnthalpyRefrig(state, fluidNameSteam, this->TempUpLimitBoilerOut, 0.0, this->FluidIndex, RoutineName);
        Real64 LatentEnthSteam = EnthSteamOutDry - EnthSteamOutWet;

        Real64 CpWater =
            FluidProperties::GetSatSpecificHeatRefrig(state, fluidNameSteam, this->TempUpLimitBoilerOut, 0.0, this->FluidIndex, RoutineName);

        this->DesMassFlowRate =
            this->NomCap / (LatentEnthSteam + CpWater * (this->TempUpLimitBoilerOut - state.dataLoopNodes->Node(BoilerInletNode).Temp));

        PlantUtilities::InitComponentNodes(state,
                                           0.0,
                                           this->DesMassFlowRate,
                                           this->BoilerInletNodeNum,
                                           this->BoilerOutletNodeNum,
                                           this->LoopNum,
                                           this->LoopSideNum,
                                           this->BranchNum,
                                           this->CompNum);

        this->BoilerPressCheck = 0.0;
        this->FuelUsed = 0.0;
        this->BoilerLoad = 0.0;
        this->BoilerEff = 0.0;
        this->BoilerOutletTemp = 0.0;

        if ((state.dataLoopNodes->Node(this->BoilerOutletNodeNum).TempSetPoint == DataLoopNode::SensedNodeFlagValue) &&
            (state.dataLoopNodes->Node(this->BoilerOutletNodeNum).TempSetPointLo == DataLoopNode::SensedNodeFlagValue)) {
            if (!state.dataGlobal->AnyEnergyManagementSystemInModel) {
                if (!this->MissingSetPointErrDone) {
                    ShowWarningError(state, "Missing temperature setpoint for Boiler:Steam = " + this->Name);
                    ShowContinueError(state, " A temperature setpoint is needed at the outlet node of the boiler, use a SetpointManager");
                    ShowContinueError(state, " The overall loop setpoint will be assumed for this boiler. The simulation continues ...");
                    this->MissingSetPointErrDone = true;
                }
            } else {
                // need call to EMS to check node
                bool FatalError = false; // but not really fatal yet, but should be.
                EMSManager::CheckIfNodeSetPointManagedByEMS(
                    state, this->BoilerOutletNodeNum, EMSManager::SPControlType::iTemperatureSetPoint, FatalError);
                state.dataLoopNodes->NodeSetpointCheck(this->BoilerOutletNodeNum).needsSetpointChecking = false;
                if (FatalError) {
                    if (!this->MissingSetPointErrDone) {
                        ShowWarningError(state, "Missing temperature setpoint for LeavingSetpointModulated mode Boiler named " + this->Name);
                        ShowContinueError(state, " A temperature setpoint is needed at the outlet node of the boiler.");
                        ShowContinueError(state, " Use a Setpoint Manager to establish a setpoint at the boiler outlet node ");
                        ShowContinueError(state, " or use an EMS actuator to establish a setpoint at the boiler outlet node.");
                        ShowContinueError(state, " The overall loop setpoint will be assumed for this boiler. The simulation continues...");
                        this->MissingSetPointErrDone = true;
                    }
                }
            }
            this->UseLoopSetPoint = true; // this is for backward compatibility and could be removed
        }
    }

    void BoilerSpecs::initialize(EnergyPlusData &state) // number of the current electric chiller being simulated
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

        // Init more variables
        if (this->myFlag) {
            this->setupOutputVars(state);
            this->oneTimeInit(state);
            this->myFlag = false;
        }

        if (state.dataGlobal->BeginEnvrnFlag && this->myEnvrnFlag && (state.dataPlnt->PlantFirstSizesOkayToFinalize)) {
            this->initEachEnvironment(state);
            this->myEnvrnFlag = false;
        }
        if (!state.dataGlobal->BeginEnvrnFlag) {
            this->myEnvrnFlag = true;
        }

        if (this->UseLoopSetPoint) {
            //  At some point, need to circle back and get from plant data structure instead of node
            // fix for clumsy old input that worked because loop setpoint was spread.
            //  could be removed with transition, testing , model change, period of being obsolete.
            int BoilerOutletNode = this->BoilerOutletNodeNum;
            {
                auto const SELECT_CASE_var(state.dataPlnt->PlantLoop(this->LoopNum).LoopDemandCalcScheme);
                if (SELECT_CASE_var == DataPlant::iLoopDemandCalcScheme::SingleSetPoint) {
                    state.dataLoopNodes->Node(BoilerOutletNode).TempSetPoint =
                        state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(this->LoopNum).TempSetPointNodeNum).TempSetPoint;
                } else if (SELECT_CASE_var == DataPlant::iLoopDemandCalcScheme::DualSetPointDeadBand) {
                    state.dataLoopNodes->Node(BoilerOutletNode).TempSetPointLo =
                        state.dataLoopNodes->Node(state.dataPlnt->PlantLoop(this->LoopNum).TempSetPointNodeNum).TempSetPointLo;
                }
            }
        }
    }

    void BoilerSpecs::setupOutputVars(EnergyPlusData &state)
    {
        SetupOutputVariable(state, "Boiler Heating Rate", OutputProcessor::Unit::W, this->BoilerLoad, "System", "Average", this->Name);
        SetupOutputVariable(state,
                            "Boiler Heating Energy",
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
        SetupOutputVariable(state,
                            "Boiler " + this->BoilerFuelTypeForOutputVariable + " Rate",
                            OutputProcessor::Unit::W,
                            this->FuelUsed,
                            "System",
                            "Average",
                            this->Name);
        SetupOutputVariable(state,
                            "Boiler " + this->BoilerFuelTypeForOutputVariable + " Energy",
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
        SetupOutputVariable(state, "Boiler Steam Efficiency", OutputProcessor::Unit::None, this->BoilerEff, "System", "Average", this->Name);
        SetupOutputVariable(
            state, "Boiler Steam Inlet Temperature", OutputProcessor::Unit::C, this->BoilerInletTemp, "System", "Average", this->Name);
        SetupOutputVariable(
            state, "Boiler Steam Outlet Temperature", OutputProcessor::Unit::C, this->BoilerOutletTemp, "System", "Average", this->Name);
        SetupOutputVariable(
            state, "Boiler Steam Mass Flow Rate", OutputProcessor::Unit::kg_s, this->BoilerMassFlowRate, "System", "Average", this->Name);
    }

    void BoilerSpecs::autosize(EnergyPlusData &state)
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
        bool ErrorsFound(false); // If errors detected in input
        Real64 tmpNomCap = this->NomCap;
        int PltSizNum = state.dataPlnt->PlantLoop(this->LoopNum).PlantSizNum;

        if (PltSizNum > 0) {
            if (state.dataSize->PlantSizData(PltSizNum).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow) {
                Real64 SizingTemp = this->TempUpLimitBoilerOut;
                Real64 SteamDensity = FluidProperties::GetSatDensityRefrig(state, fluidNameSteam, SizingTemp, 1.0, this->FluidIndex, RoutineName);
                Real64 EnthSteamOutDry = FluidProperties::GetSatEnthalpyRefrig(state, fluidNameSteam, SizingTemp, 1.0, this->FluidIndex, RoutineName);
                Real64 EnthSteamOutWet = FluidProperties::GetSatEnthalpyRefrig(state, fluidNameSteam, SizingTemp, 0.0, this->FluidIndex, RoutineName);
                Real64 LatentEnthSteam = EnthSteamOutDry - EnthSteamOutWet;
                Real64 CpWater = FluidProperties::GetSatSpecificHeatRefrig(state, fluidNameSteam, SizingTemp, 0.0, this->FluidIndex, RoutineName);
                tmpNomCap = (CpWater * SteamDensity * this->SizFac * state.dataSize->PlantSizData(PltSizNum).DeltaT *
                                 state.dataSize->PlantSizData(PltSizNum).DesVolFlowRate +
                             state.dataSize->PlantSizData(PltSizNum).DesVolFlowRate * SteamDensity * LatentEnthSteam);
            } else {
                if (this->NomCapWasAutoSized) tmpNomCap = 0.0;
            }
            if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                if (this->NomCapWasAutoSized) {
                    this->NomCap = tmpNomCap;
                    if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(state, "Boiler:Steam", this->Name, "Design Size Nominal Capacity [W]", tmpNomCap);
                    }
                    if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(state, "Boiler:Steam", this->Name, "Initial Design Size Nominal Capacity [W]", tmpNomCap);
                    }
                } else { // Hard-sized with sizing data
                    if (this->NomCap > 0.0 && tmpNomCap > 0.0) {
                        Real64 NomCapUser = this->NomCap;
                        if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                            BaseSizer::reportSizerOutput(state,
                                                         "Boiler:Steam",
                                                         this->Name,
                                                         "Design Size Nominal Capacity [W]",
                                                         tmpNomCap,
                                                         "User-Specified Nominal Capacity [W]",
                                                         NomCapUser);
                            if (state.dataGlobal->DisplayExtraWarnings) {
                                if ((std::abs(tmpNomCap - NomCapUser) / NomCapUser) > state.dataSize->AutoVsHardSizingThreshold) {
                                    ShowMessage(state, "SizePump: Potential issue with equipment sizing for " + this->Name);
                                    ShowContinueError(state, format("User-Specified Nominal Capacity of {:.2R} [W]", NomCapUser));
                                    ShowContinueError(state, format("differs from Design Size Nominal Capacity of {:.2R} [W]", tmpNomCap));
                                    ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                        }
                    }
                }
            }
        } else {
            if (this->NomCapWasAutoSized && state.dataPlnt->PlantFirstSizesOkayToFinalize) {
                ShowSevereError(state, "Autosizing of Boiler nominal capacity requires a loop Sizing:Plant object");
                ShowContinueError(state, "Occurs in Boiler:Steam object=" + this->Name);
                ErrorsFound = true;
            }
            if (!this->NomCapWasAutoSized && this->NomCap > 0.0 && state.dataPlnt->PlantFinalSizesOkayToReport) {
                BaseSizer::reportSizerOutput(state, "Boiler:Steam", this->Name, "User-Specified Nominal Capacity [W]", this->NomCap);
            }
        }

        if (state.dataPlnt->PlantFinalSizesOkayToReport) {
            // create predefined report
            OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchMechType, this->Name, "Boiler:Steam");
            OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchMechNomEff, this->Name, this->NomEffic);
            OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchMechNomCap, this->Name, this->NomCap);
        }

        if (ErrorsFound) {
            ShowFatalError(state, "Preceding sizing errors cause program termination");
        }
    }

    void BoilerSpecs::calculate(EnergyPlusData &state,
                                Real64 &MyLoad,                                             // W - hot water demand to be met by boiler
                                bool const RunFlag,                                         // TRUE if boiler operating
                                DataBranchAirLoopPlant::ControlTypeEnum const EquipFlowCtrl // Flow control mode for the equipment
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
        Real64 BoilerDeltaTemp(0.0); // C - boiler inlet to outlet temperature difference
        Real64 CpWater;              // Heat capacity of condensed steam

        // Loading the variables derived type in to local variables
        this->BoilerLoad = 0.0;
        this->BoilerMassFlowRate = 0.0;

        {
            auto const SELECT_CASE_var(state.dataPlnt->PlantLoop(this->LoopNum).LoopDemandCalcScheme);
            if (SELECT_CASE_var == DataPlant::iLoopDemandCalcScheme::SingleSetPoint) {
                this->BoilerOutletTemp = state.dataLoopNodes->Node(this->BoilerOutletNodeNum).TempSetPoint;
            } else if (SELECT_CASE_var == DataPlant::iLoopDemandCalcScheme::DualSetPointDeadBand) {
                this->BoilerOutletTemp = state.dataLoopNodes->Node(this->BoilerOutletNodeNum).TempSetPointLo;
            }
        }
        // If the specified load is 0.0 or the boiler should not run then we leave this subroutine.Before leaving
        // if the component control is SERIESACTIVE we set the component flow to inlet flow so that flow resolver
        // will not shut down the branch
        if (MyLoad <= 0.0 || !RunFlag) {
            if (EquipFlowCtrl == DataBranchAirLoopPlant::ControlTypeEnum::SeriesActive)
                this->BoilerMassFlowRate = state.dataLoopNodes->Node(this->BoilerInletNodeNum).MassFlowRate;
            return;
        }

        // Set the current load equal to the boiler load
        this->BoilerLoad = MyLoad;

        this->BoilerPressCheck = FluidProperties::GetSatPressureRefrig(state, fluidNameSteam, this->BoilerOutletTemp, this->FluidIndex, RoutineName);

        if ((this->BoilerPressCheck) > this->BoilerMaxOperPress) {
            if (this->PressErrIndex == 0) {
                ShowSevereError(state, "Boiler:Steam=\"" + this->Name + "\", Saturation Pressure is greater than Maximum Operating Pressure,");
                ShowContinueError(state, "Lower Input Temperature");
                ShowContinueError(state, format("Steam temperature=[{:.2R}] C", this->BoilerOutletTemp));
                ShowContinueError(state, format("Refrigerant Saturation Pressure =[{:.0R}] Pa", this->BoilerPressCheck));
            }
            ShowRecurringSevereErrorAtEnd(state,
                                          "Boiler:Steam=\"" + this->Name +
                                              "\", Saturation Pressure is greater than Maximum Operating Pressure..continues",
                                          this->PressErrIndex,
                                          this->BoilerPressCheck,
                                          this->BoilerPressCheck,
                                          _,
                                          "[Pa]",
                                          "[Pa]");
        }

        CpWater = FluidProperties::GetSatSpecificHeatRefrig(
            state, fluidNameSteam, state.dataLoopNodes->Node(this->BoilerInletNodeNum).Temp, 0.0, this->FluidIndex, RoutineName);

        if (state.dataPlnt->PlantLoop(this->LoopNum).LoopSide(this->LoopSideNum).FlowLock ==
            DataPlant::iFlowLock::Unlocked) { // TODO: Components shouldn't check FlowLock
            // Calculate the flow for the boiler

            {
                auto const SELECT_CASE_var(state.dataPlnt->PlantLoop(this->LoopNum).LoopDemandCalcScheme);
                if (SELECT_CASE_var == DataPlant::iLoopDemandCalcScheme::SingleSetPoint) {
                    BoilerDeltaTemp =
                        state.dataLoopNodes->Node(this->BoilerOutletNodeNum).TempSetPoint - state.dataLoopNodes->Node(this->BoilerInletNodeNum).Temp;
                } else { // DataPlant::iLoopDemandCalcScheme::DualSetPointDeadBand
                    BoilerDeltaTemp = state.dataLoopNodes->Node(this->BoilerOutletNodeNum).TempSetPointLo -
                                      state.dataLoopNodes->Node(this->BoilerInletNodeNum).Temp;
                }
            }
            this->BoilerOutletTemp = BoilerDeltaTemp + state.dataLoopNodes->Node(this->BoilerInletNodeNum).Temp;

            Real64 const EnthSteamOutDry =
                FluidProperties::GetSatEnthalpyRefrig(state, fluidNameSteam, this->BoilerOutletTemp, 1.0, this->FluidIndex, RoutineName);
            Real64 const EnthSteamOutWet =
                FluidProperties::GetSatEnthalpyRefrig(state, fluidNameSteam, this->BoilerOutletTemp, 0.0, this->FluidIndex, RoutineName);
            Real64 const LatentEnthSteam = EnthSteamOutDry - EnthSteamOutWet;
            this->BoilerMassFlowRate = this->BoilerLoad / (LatentEnthSteam + (CpWater * BoilerDeltaTemp));

            PlantUtilities::SetComponentFlowRate(state,
                                                 this->BoilerMassFlowRate,
                                                 this->BoilerInletNodeNum,
                                                 this->BoilerOutletNodeNum,
                                                 this->LoopNum,
                                                 this->LoopSideNum,
                                                 this->BranchNum,
                                                 this->CompNum);

        } else { // If FlowLock is True
            // Set the boiler flow rate from inlet node and then check performance
            this->BoilerMassFlowRate = state.dataLoopNodes->Node(this->BoilerInletNodeNum).MassFlowRate;
            // Assume that it can meet the setpoint
            {
                auto const SELECT_CASE_var(state.dataPlnt->PlantLoop(this->LoopNum).LoopDemandCalcScheme);
                if (SELECT_CASE_var == DataPlant::iLoopDemandCalcScheme::SingleSetPoint) {
                    BoilerDeltaTemp =
                        state.dataLoopNodes->Node(this->BoilerOutletNodeNum).TempSetPoint - state.dataLoopNodes->Node(this->BoilerInletNodeNum).Temp;
                } else if (SELECT_CASE_var == DataPlant::iLoopDemandCalcScheme::DualSetPointDeadBand) {
                    BoilerDeltaTemp = state.dataLoopNodes->Node(this->BoilerOutletNodeNum).TempSetPointLo -
                                      state.dataLoopNodes->Node(this->BoilerInletNodeNum).Temp;
                }
            }
            // If boiler outlet temp is already greater than setpoint than it does not need to operate this iteration
            if (BoilerDeltaTemp < 0.0) {
                {
                    auto const SELECT_CASE_var(state.dataPlnt->PlantLoop(this->LoopNum).LoopDemandCalcScheme);
                    if (SELECT_CASE_var == DataPlant::iLoopDemandCalcScheme::SingleSetPoint) {
                        this->BoilerOutletTemp = state.dataLoopNodes->Node(this->BoilerOutletNodeNum).TempSetPoint;
                    } else if (SELECT_CASE_var == DataPlant::iLoopDemandCalcScheme::DualSetPointDeadBand) {
                        this->BoilerOutletTemp = state.dataLoopNodes->Node(this->BoilerOutletNodeNum).TempSetPointLo;
                    }
                }
                Real64 const EnthSteamOutDry =
                    FluidProperties::GetSatEnthalpyRefrig(state, fluidNameSteam, this->BoilerOutletTemp, 1.0, this->FluidIndex, RoutineName);
                Real64 const EnthSteamOutWet =
                    FluidProperties::GetSatEnthalpyRefrig(state, fluidNameSteam, this->BoilerOutletTemp, 0.0, this->FluidIndex, RoutineName);
                Real64 const LatentEnthSteam = EnthSteamOutDry - EnthSteamOutWet;
                this->BoilerLoad = (this->BoilerMassFlowRate * LatentEnthSteam);

            } else {

                {
                    auto const SELECT_CASE_var(state.dataPlnt->PlantLoop(this->LoopNum).LoopDemandCalcScheme);
                    if (SELECT_CASE_var == DataPlant::iLoopDemandCalcScheme::SingleSetPoint) {
                        this->BoilerOutletTemp = state.dataLoopNodes->Node(this->BoilerOutletNodeNum).TempSetPoint;
                    } else if (SELECT_CASE_var == DataPlant::iLoopDemandCalcScheme::DualSetPointDeadBand) {
                        this->BoilerOutletTemp = state.dataLoopNodes->Node(this->BoilerOutletNodeNum).TempSetPointLo;
                    }
                }

                Real64 const EnthSteamOutDry =
                    FluidProperties::GetSatEnthalpyRefrig(state, fluidNameSteam, this->BoilerOutletTemp, 1.0, this->FluidIndex, RoutineName);
                Real64 const EnthSteamOutWet =
                    FluidProperties::GetSatEnthalpyRefrig(state, fluidNameSteam, this->BoilerOutletTemp, 0.0, this->FluidIndex, RoutineName);
                Real64 const LatentEnthSteam = EnthSteamOutDry - EnthSteamOutWet;
                this->BoilerLoad =
                    std::abs(this->BoilerMassFlowRate * LatentEnthSteam) + std::abs(this->BoilerMassFlowRate * CpWater * BoilerDeltaTemp);
            }

            // If load exceeds the distributed load set to the distributed load
            if (this->BoilerLoad > MyLoad) {
                this->BoilerLoad = MyLoad;

                // Reset later , here just for calculating latent heat
                {
                    auto const SELECT_CASE_var(state.dataPlnt->PlantLoop(this->LoopNum).LoopDemandCalcScheme);
                    if (SELECT_CASE_var == DataPlant::iLoopDemandCalcScheme::SingleSetPoint) {
                        this->BoilerOutletTemp = state.dataLoopNodes->Node(this->BoilerOutletNodeNum).TempSetPoint;
                    } else if (SELECT_CASE_var == DataPlant::iLoopDemandCalcScheme::DualSetPointDeadBand) {
                        this->BoilerOutletTemp = state.dataLoopNodes->Node(this->BoilerOutletNodeNum).TempSetPointLo;
                    }
                }

                Real64 const EnthSteamOutDry =
                    FluidProperties::GetSatEnthalpyRefrig(state, fluidNameSteam, this->BoilerOutletTemp, 1.0, this->FluidIndex, RoutineName);
                Real64 const EnthSteamOutWet =
                    FluidProperties::GetSatEnthalpyRefrig(state, fluidNameSteam, this->BoilerOutletTemp, 0.0, this->FluidIndex, RoutineName);
                Real64 const LatentEnthSteam = EnthSteamOutDry - EnthSteamOutWet;
                BoilerDeltaTemp = this->BoilerOutletTemp - state.dataLoopNodes->Node(this->BoilerInletNodeNum).Temp;
                this->BoilerMassFlowRate = this->BoilerLoad / (LatentEnthSteam + CpWater * BoilerDeltaTemp);

                PlantUtilities::SetComponentFlowRate(state,
                                                     this->BoilerMassFlowRate,
                                                     this->BoilerInletNodeNum,
                                                     this->BoilerOutletNodeNum,
                                                     this->LoopNum,
                                                     this->LoopSideNum,
                                                     this->BranchNum,
                                                     this->CompNum);
            }

            // Checks Boiler Load on the basis of the machine limits.
            if (this->BoilerLoad > this->NomCap) {
                if (this->BoilerMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance) {
                    this->BoilerLoad = this->NomCap;

                    Real64 const EnthSteamOutDry =
                        FluidProperties::GetSatEnthalpyRefrig(state, fluidNameSteam, this->BoilerOutletTemp, 1.0, this->FluidIndex, RoutineName);
                    Real64 const EnthSteamOutWet =
                        FluidProperties::GetSatEnthalpyRefrig(state, fluidNameSteam, this->BoilerOutletTemp, 0.0, this->FluidIndex, RoutineName);
                    Real64 const LatentEnthSteam = EnthSteamOutDry - EnthSteamOutWet;
                    BoilerDeltaTemp = this->BoilerOutletTemp - state.dataLoopNodes->Node(this->BoilerInletNodeNum).Temp;
                    this->BoilerMassFlowRate = this->BoilerLoad / (LatentEnthSteam + CpWater * BoilerDeltaTemp);

                    PlantUtilities::SetComponentFlowRate(state,
                                                         this->BoilerMassFlowRate,
                                                         this->BoilerInletNodeNum,
                                                         this->BoilerOutletNodeNum,
                                                         this->LoopNum,
                                                         this->LoopSideNum,
                                                         this->BranchNum,
                                                         this->CompNum);
                } else {
                    this->BoilerLoad = 0.0;
                    this->BoilerOutletTemp = state.dataLoopNodes->Node(this->BoilerInletNodeNum).Temp;
                }
            }

        } // End of the FlowLock If block

        // Limit BoilerOutletTemp.  If > max temp, trip boiler.
        if (this->BoilerOutletTemp > this->TempUpLimitBoilerOut) {
            this->BoilerLoad = 0.0;
            this->BoilerOutletTemp = state.dataLoopNodes->Node(this->BoilerInletNodeNum).Temp;
            //  Does BoilerMassFlowRate need to be set????
        }

        Real64 OperPLR = this->BoilerLoad / this->NomCap;
        OperPLR = min(OperPLR, this->MaxPartLoadRat);
        OperPLR = max(OperPLR, this->MinPartLoadRat);
        Real64 TheorFuelUse = this->BoilerLoad / this->NomEffic;

        // Calculate fuel used
        this->FuelUsed = TheorFuelUse / (this->FullLoadCoef(1) + this->FullLoadCoef(2) * OperPLR + this->FullLoadCoef(3) * pow_2(OperPLR));
        // Calculate boiler efficiency
        this->BoilerEff = this->BoilerLoad / this->FuelUsed;
    }

    // Beginning of Record Keeping subroutines for the BOILER:SIMPLE Module

    void BoilerSpecs::update(EnergyPlusData &state,
                             Real64 const MyLoad,                           // boiler operating load
                             bool const RunFlag,                            // boiler on when TRUE
                             [[maybe_unused]] bool const FirstHVACIteration // TRUE if First iteration of simulation
    )
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rahul Chillar
        //       DATE WRITTEN   Dec 2004
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Boiler simulation reporting

        Real64 ReportingConstant = state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
        int BoilerInletNode = this->BoilerInletNodeNum;
        int BoilerOutletNode = this->BoilerOutletNodeNum;

        if (MyLoad <= 0.0 || !RunFlag) {
            // set node temperatures
            PlantUtilities::SafeCopyPlantNode(state, BoilerInletNode, BoilerOutletNode);
            state.dataLoopNodes->Node(BoilerOutletNode).Temp = state.dataLoopNodes->Node(BoilerInletNode).Temp;
            this->BoilerOutletTemp = state.dataLoopNodes->Node(BoilerInletNode).Temp;
            this->BoilerLoad = 0.0;
            this->FuelUsed = 0.0;
            this->BoilerEff = 0.0;
            state.dataLoopNodes->Node(BoilerInletNode).Press = this->BoilerPressCheck;
            state.dataLoopNodes->Node(BoilerOutletNode).Press = state.dataLoopNodes->Node(BoilerInletNode).Press;
            state.dataLoopNodes->Node(BoilerInletNode).Quality = 0.0;
            state.dataLoopNodes->Node(BoilerOutletNode).Quality = state.dataLoopNodes->Node(BoilerInletNode).Quality;

        } else {
            // set node temperatures
            PlantUtilities::SafeCopyPlantNode(state, BoilerInletNode, BoilerOutletNode);
            state.dataLoopNodes->Node(BoilerOutletNode).Temp = this->BoilerOutletTemp;
            state.dataLoopNodes->Node(BoilerInletNode).Press = this->BoilerPressCheck; //???
            state.dataLoopNodes->Node(BoilerOutletNode).Press = state.dataLoopNodes->Node(BoilerInletNode).Press;
            state.dataLoopNodes->Node(BoilerOutletNode).Quality = 1.0; // Model assumes saturated steam exiting the boiler
        }

        this->BoilerInletTemp = state.dataLoopNodes->Node(BoilerInletNode).Temp;
        this->BoilerMassFlowRate = state.dataLoopNodes->Node(BoilerOutletNode).MassFlowRate;
        this->BoilerEnergy = this->BoilerLoad * ReportingConstant;
        this->FuelConsumed = this->FuelUsed * ReportingConstant;
    }

} // namespace BoilerSteam

} // namespace EnergyPlus
