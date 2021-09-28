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
#include <cmath>
#include <iostream>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/Construction.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalSurface.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataPrecisionGlobals.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataSurfaceLists.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/HeatBalanceSurfaceManager.hh>
#include <EnergyPlus/IndoorIceRink.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/Plant/PlantLocation.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/ZoneTempPredictorCorrector.hh>

namespace EnergyPlus { // NOLINT(modernize-concat-nested-namespaces)
namespace IceRink {

    PlantComponent *IceRinkData::factory(EnergyPlusData &state, std::string const &objectName)
    {
        // Process the input data for ice rinks if it hasn't been done yet
        if (state.dataIceRink->GetInput) {
            GetIndoorIceRink(state);
            state.dataIceRink->GetInput = false;
        }
        // Now look for this particular rink in the list
        for (auto &rink : state.dataIceRink->Rink) {
            if (rink.Name == objectName) {
                return &rink;
            }
        }
        // If it is not found, fatal error
        ShowFatalError(state, "IceRinkFactory: Error getting inputs for rink named: " + objectName);
        // Shut up the compiler
        return nullptr;
    }

    void IceRinkData::simulate(EnergyPlusData &state,
                               [[maybe_unused]] const PlantLocation &calledFromLocation,
                               [[maybe_unused]] bool FirstHVACIteration,
                               Real64 &CurLoad,
                               [[maybe_unused]] bool const RunFlag)
    {
        this->initialize(state);
        if (this->RinkType_Num == DataPlant::TypeOf_IceRink) {
            this->calculateIceRink(state, CurLoad); //"false" is for unit testing
        }
        // this->update();
        this->report(state);
    }

    void IceRinkData::oneTimeInit([[maybe_unused]] EnergyPlusData &state)
    {
        state.dataSurface->SurfIsRadSurfOrVentSlabOrPool(this->SurfacePtr) = true;
        state.dataSurface->SurfIsPool(this->SurfacePtr) = true;
    }

    void GetIndoorIceRink(EnergyPlusData &state)
    {
        // SUBROUTINE PARAMETER DEFINITIONS:
        static constexpr std::string_view RoutineName("GetIndoorIceRink: ");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        bool ErrorsFound(false); // Set to true if errors in input,
        int IOStatus;            // Used in GetObjectItem
        int NumAlphas;           // Number of Alphas for each GetObjectItem call
        int NumNumbers;          // Number of Numbers for each GetObjectItem call
        int NumArgs;             // Unused variable that is part of a subroutine call

        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, "IceRink:Indoor", NumArgs, NumAlphas, NumNumbers);

        state.dataIceRink->NumOfRinks = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "IceRink:Indoor");

        if (state.dataIceRink->NumOfRinks <= 0) ShowFatalError(state, "No Rink objects found in input.");

        auto &Surface(state.dataSurface->Surface);
        auto &Rink = state.dataIceRink->Rink;
        auto &NumOfRinks = state.dataIceRink->NumOfRinks;

        if (allocated(Rink)) Rink.deallocate();
        Rink.allocate(NumOfRinks);

        // Obtain all the user data related to rinks
        for (int Item = 1; Item <= NumOfRinks; ++Item) {
            state.dataIPShortCut->cCurrentModuleObject = "IceRink:Indoor";
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     state.dataIPShortCut->cCurrentModuleObject,
                                                                     Item,
                                                                     state.dataIPShortCut->cAlphaArgs,
                                                                     NumAlphas,
                                                                     state.dataIPShortCut->rNumericArgs,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     _,
                                                                     _,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);

            Rink(Item).Name = state.dataIPShortCut->cAlphaArgs(1);

            Rink(Item).SchedName = state.dataIPShortCut->cAlphaArgs(2);
            Rink(Item).SchedPtr = ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(2));
            if ((Rink(Item).SchedPtr == 0) && (!state.dataIPShortCut->lAlphaFieldBlanks(2))) {
                ShowSevereError(state, state.dataIPShortCut->cAlphaFieldNames(2) + " not found: " + state.dataIPShortCut->cAlphaArgs(2));
                ShowContinueError(state, "Occurs in " + state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1));
                // ErrorsFound = true;
                Rink(Item).IceSetptSchedPtr = 1;
            }

            Rink(Item).ZoneName = state.dataIPShortCut->cAlphaArgs(3);
            Rink(Item).ZonePtr = UtilityRoutines::FindItemInList(state.dataIPShortCut->cAlphaArgs(3), state.dataHeatBal->Zone);
            if (Rink(Item).ZonePtr == 0) {
                ShowSevereError(
                    state,
                    format("{}: Invalid {} = {}", RoutineName, state.dataIPShortCut->cAlphaFieldNames(3), state.dataIPShortCut->cAlphaArgs(3)));
                ShowContinueError(state, "Occurs in " + state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1));
                ErrorsFound = true;
            }

            Rink(Item).SurfaceName = state.dataIPShortCut->cAlphaArgs(4);
            Rink(Item).SurfacePtr = UtilityRoutines::FindItemInList(state.dataIPShortCut->cAlphaArgs(4), Surface);
            if ((Rink(Item).SurfacePtr == 0) || (Surface(Rink(Item).SurfacePtr).Class != DataSurfaces::SurfaceClass::Floor) ||
                (Surface(Rink(Item).SurfacePtr).Class == DataSurfaces::SurfaceClass::Window) ||
                (state.dataSurface->SurfIsRadSurfOrVentSlabOrPool(Surface(Rink(Item).SurfacePtr).Construction) ||
                 (!state.dataConstruction->Construct(Surface(Rink(Item).SurfacePtr).Construction).SourceSinkPresent))) {
                ShowSevereError(state,
                                format("{}: {}=\"{}\", Invalid Surface",
                                       RoutineName,
                                       state.dataIPShortCut->cCurrentModuleObject,
                                       state.dataIPShortCut->cAlphaArgs(1)));
                ShowContinueError(state,
                                  state.dataIPShortCut->cAlphaFieldNames(4) + "=\"" + state.dataIPShortCut->cAlphaArgs(4) +
                                      "\" either is not defines as a floor or is defined as a window or is a part of ventilated slab or has no "
                                      "source/sink defines in it");

                ErrorsFound = true;
            }

            Rink(Item).TubeDiameter = state.dataIPShortCut->rNumericArgs(1);
            Rink(Item).TubeLength = state.dataIPShortCut->rNumericArgs(2);

            if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(5), "BOTC")) { // TODO: These need to be more verbose strings
                Rink(Item).ControlStrategy = ControlType::BrineOutletTemp;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(5), "STC")) {
                Rink(Item).ControlStrategy = ControlType::SurfaceTemp;
            }

            Rink(Item).hrstofreeze = state.dataIPShortCut->rNumericArgs(3);
            Rink(Item).deltatemp = state.dataIPShortCut->rNumericArgs(4);

            Rink(Item).RefrigInNode = state.dataIPShortCut->cAlphaArgs(6);
            Rink(Item).InNode = GetOnlySingleNode(state,
                                                  state.dataIPShortCut->cAlphaArgs(6),
                                                  ErrorsFound,
                                                  state.dataIPShortCut->cCurrentModuleObject,
                                                  state.dataIPShortCut->cAlphaArgs(1),
                                                  DataLoopNode::NodeFluidType::Water,
                                                  DataLoopNode::NodeConnectionType::Inlet,
                                                  NodeInputManager::compFluidStream::Primary,
                                                  DataLoopNode::ObjectIsNotParent);
            if (Rink(Item).InNode == 0) {
                ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(6) + '=' + state.dataIPShortCut->cAlphaArgs(6));
                ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
                ErrorsFound = true;
            }

            Rink(Item).OutNode = GetOnlySingleNode(state,
                                                   state.dataIPShortCut->cAlphaArgs(7),
                                                   ErrorsFound,
                                                   state.dataIPShortCut->cCurrentModuleObject,
                                                   state.dataIPShortCut->cAlphaArgs(1),
                                                   DataLoopNode::NodeFluidType::Water,
                                                   DataLoopNode::NodeConnectionType::Outlet,
                                                   NodeInputManager::compFluidStream::Primary,
                                                   DataLoopNode::ObjectIsNotParent);
            if (Rink(Item).OutNode == 0) {
                ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(7) + '=' + state.dataIPShortCut->cAlphaArgs(7));
                ShowContinueError(state, "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
                ErrorsFound = true;
            }

            BranchNodeConnections::TestCompSet(state,
                                               state.dataIPShortCut->cCurrentModuleObject,
                                               state.dataIPShortCut->cAlphaArgs(1),
                                               state.dataIPShortCut->cAlphaArgs(6),
                                               state.dataIPShortCut->cAlphaArgs(7),
                                               "Ice Rink Nodes");

            Rink(Item).ResurfacingSchedName = state.dataIPShortCut->cAlphaArgs(8);
            Rink(Item).ResurfacingSchedPtr = ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(8));
            if ((Rink(Item).ResurfacingSchedPtr == 0) && (!state.dataIPShortCut->lAlphaFieldBlanks(8))) {
                ShowSevereError(state, state.dataIPShortCut->cAlphaFieldNames(8) + " not found: " + state.dataIPShortCut->cAlphaArgs(8));
                ShowContinueError(state, "Occurs in " + state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1));
                ErrorsFound = true;
            }

            Rink(Item).LengthRink = state.dataIPShortCut->rNumericArgs(5);
            if (Rink(Item).LengthRink <= 0.0) {
                ShowWarningError(state,
                                 format("{}: {}=\"{}\" was entered with zero or negative rink length.",
                                        RoutineName,
                                        state.dataIPShortCut->cCurrentModuleObject,
                                        state.dataIPShortCut->cAlphaArgs(1)));
                ShowContinueError(state, "The rink length has been reset to 60.");
                Rink(Item).LengthRink = 60.0;
            }

            Rink(Item).WidthRink = state.dataIPShortCut->rNumericArgs(6);
            if (Rink(Item).WidthRink <= 0.0) {
                ShowWarningError(state,
                                 format("{}: {}=\"{}\" was entered with zero or negative rink width.",
                                        RoutineName,
                                        state.dataIPShortCut->cCurrentModuleObject,
                                        state.dataIPShortCut->cAlphaArgs(1)));
                ShowContinueError(state, "The rink width has been reset to 30.");
                Rink(Item).WidthRink = 30.0;
            }

            Rink(Item).WaterTemp = state.dataIPShortCut->rNumericArgs(7);
            if (Rink(Item).WaterTemp <= -21.0) {
                ShowWarningError(state,
                                 format("{}: {}=\"{}\" was entered with too low water temperature.",
                                        RoutineName,
                                        state.dataIPShortCut->cCurrentModuleObject,
                                        state.dataIPShortCut->cAlphaArgs(1)));
                ShowContinueError(state, "The rink water has been reset to 22");
                Rink(Item).WaterTemp = 22.0;
            }

            Rink(Item).IceThickness = state.dataIPShortCut->rNumericArgs(8);
            if (Rink(Item).IceThickness <= 0.0) {
                ShowWarningError(state,
                                 format("{}: {}=\"{}\" was entered with zero or negative ice thickness.",
                                        RoutineName,
                                        state.dataIPShortCut->cCurrentModuleObject,
                                        state.dataIPShortCut->cAlphaArgs(1)));
                ShowContinueError(state, "The rink ice thickness has been reset to 0.1.");
                Rink(Item).IceThickness = 0.0254;
            }

            Rink(Item).COP = state.dataIPShortCut->rNumericArgs(9);
            if (Rink(Item).COP <= 0.0) {
                ShowWarningError(state,
                                 format("{}: {}=\"{}\" was entered with zero or negative COP.",
                                        RoutineName,
                                        state.dataIPShortCut->cCurrentModuleObject,
                                        state.dataIPShortCut->cAlphaArgs(1)));
                ShowContinueError(state, "The COP is reset to 2.5.");
                Rink(Item).COP = 2.5;
            }

            Rink(Item).IceSetPointTemp = state.dataIPShortCut->rNumericArgs(10);
            if (Rink(Item).IceSetPointTemp >= 0.0) {
                ShowWarningError(state,
                                 format("{}: {}=\"{}\" was entered with zero or positive ice rink set-point temperature.",
                                        RoutineName,
                                        state.dataIPShortCut->cCurrentModuleObject,
                                        state.dataIPShortCut->cAlphaArgs(1)));
                ShowContinueError(state, "The ice rink setpoint temperature has been reset to -3 C.");
                Rink(Item).IceSetPointTemp = -3;
            }

            Rink(Item).HXSpacing = state.dataIPShortCut->rNumericArgs(11);
            if (Rink(Item).HXSpacing <= 0.0) {
                ShowWarningError(state,
                                 format("{}: {}=\"{}\" was entered with zero or negative HX spacing.",
                                        RoutineName,
                                        state.dataIPShortCut->cCurrentModuleObject,
                                        state.dataIPShortCut->cAlphaArgs(1)));
                ShowContinueError(state, "The HX spacing has been reset to 0.01m.");
                Rink(Item).HXSpacing = 0.01;
            }
            Rink(Item).ResurfTank = state.dataIPShortCut->rNumericArgs(12);
            if (Rink(Item).ResurfTank <= 0.0) {
                ShowWarningError(state,
                                 format("{}: {}=\"{}\" was entered with zero or negative resurfacer tank volume.",
                                        RoutineName,
                                        state.dataIPShortCut->cCurrentModuleObject,
                                        state.dataIPShortCut->cAlphaArgs(1)));
                ShowContinueError(state, "The resurfacer tank volume has been reset to 0.55m3.");
                Rink(Item).ResurfTank = 0.55;
            }

            Rink(Item).InitialWaterTemp = state.dataIPShortCut->rNumericArgs(13);
            if (Rink(Item).InitialWaterTemp <= 0.0) {
                ShowWarningError(state,
                                 format("{}: {}=\"{}\" was entered with zero or negative initial resurfacer water temp.",
                                        RoutineName,
                                        state.dataIPShortCut->cCurrentModuleObject,
                                        state.dataIPShortCut->cAlphaArgs(1)));
                ShowContinueError(state, "The resurfacer initial water temperature has been reset to 18C.");
                Rink(Item).InitialWaterTemp = 18;
            }

            Rink(Item).ResurfWaterTemp = state.dataIPShortCut->rNumericArgs(14);
            if (Rink(Item).ResurfWaterTemp <= 0.0) {
                ShowWarningError(state,
                                 format("{}: {}=\"{}\" was entered with zero or negative resurfacer water temp.",
                                        RoutineName,
                                        state.dataIPShortCut->cCurrentModuleObject,
                                        state.dataIPShortCut->cAlphaArgs(1)));
                ShowContinueError(state, "The resurfacer hot water temperature has been reset to 55C.");
                Rink(Item).ResurfWaterTemp = 55;
            }

            Rink(Item).DesignSetPoint = state.dataIPShortCut->rNumericArgs(15);
            if (Rink(Item).DesignSetPoint >= 0.0) {
                ShowWarningError(state,
                                 format("{}: {}=\"{}\" was entered with zero or positive design set point temperature.",
                                        RoutineName,
                                        state.dataIPShortCut->cCurrentModuleObject,
                                        state.dataIPShortCut->cAlphaArgs(1)));
                ShowContinueError(state, "The design setpoint has been reset to -10C");
                Rink(Item).DesignSetPoint = -10;
            }
        }
        if (ErrorsFound) {
            ShowFatalError(state, "Errors found in input.");
        }
    }

    void IceRinkData::initialize(EnergyPlusData &state)
    {
        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 constexpr DesignVelocity(0.5); // Hypothetical design max pipe velocity [m/s]
        static constexpr std::string_view RoutineName("initialize");

        if (this->MyFlag) {
            bool errFlag = false;
            this->setupOutputVariables(state);
            PlantUtilities::ScanPlantLoopsForObject(
                state, this->Name, DataPlant::TypeOf_IceRink, this->LoopNum, this->LoopSide, this->BranchNum, this->CompNum, errFlag, _, _, _, _, _);
            if (errFlag) {
                ShowFatalError(state, "Initialize: Program terminated due to previous condition(s).");
            }
            Real64 rho = FluidProperties::GetDensityGlycol(state,
                                                           state.dataPlnt->PlantLoop(this->LoopNum).FluidName,
                                                           DataPrecisionGlobals::constant_zero,
                                                           state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex,
                                                           RoutineName);

            this->DesignMassFlowRate = DataGlobalConstants::Pi / 4.0 * pow_2(this->TubeDiameter) * DesignVelocity * rho * this->TubeLength;

            PlantUtilities::InitComponentNodes(state,
                                               0.0,
                                               this->DesignMassFlowRate,
                                               this->InNode,
                                               this->OutNode,
                                               this->LoopNum,
                                               this->LoopSide,
                                               this->BranchNum,
                                               this->CompNum); // initialization

            PlantUtilities::RegisterPlantCompDesignFlow(state, this->InNode, this->DesignMassFlowRate / rho); // setting maximum flow-rate

            PlantUtilities::SetComponentFlowRate(
                state, this->PastRefrigMassFlow, InNode, OutNode, this->LoopNum, this->LoopSide, this->BranchNum, this->CompNum);

            this->MyFlag = false;
        }

        if (state.dataIceRink->FirstTimeInit) {

            state.dataIceRink->QRadSysSrcAvg.dimension(state.dataSurface->TotSurfaces, 0.0);
            state.dataIceRink->LastQRadSysSrc.dimension(state.dataSurface->TotSurfaces, 0.0);
            state.dataIceRink->LastSysTimeElapsed.dimension(state.dataSurface->TotSurfaces, 0.0);
            state.dataIceRink->LastTimeStepSys.dimension(state.dataSurface->TotSurfaces, 0.0);
            this->PastRefrigMassFlow = 0.01;
            this->RefrigTempIn = -10;
            this->CpRefrig = FluidProperties::GetSpecificHeatGlycol(state,
                                                                    state.dataPlnt->PlantLoop(this->LoopNum).FluidName,
                                                                    this->RefrigTempIn,
                                                                    state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex,
                                                                    RoutineName);
            this->Qsrcmax = -(IceRinkFreezing(state)); // To calculate maximum Q source in order to obtain design capacity.
            this->maxmdot = fabs(this->Qsrcmax) / (this->CpRefrig * this->deltatemp); // To calculate maximum mass flow rate of the system.

            state.dataIceRink->FirstTimeInit = false;
            this->circuits = this->LengthRink / (this->TubeDiameter + this->HXSpacing);
        }

        if (this->MyEnvrnFlag && state.dataGlobal->BeginEnvrnFlag) {
            state.dataIceRink->QRadSysSrcAvg = 0.0;
            state.dataIceRink->LastQRadSysSrc = 0.0;
            state.dataIceRink->LastSysTimeElapsed = 0.0;
            state.dataIceRink->LastTimeStepSys = 0.0;
            this->FreezingLoad = 0.0;
            this->Qsource2 = 0;
            this->Qsetpoint = 0;
            this->MyEnvrnFlag = false;
        }

        if (state.dataGlobal->BeginTimeStepFlag) {
            state.dataIceRink->QRadSysSrcAvg(this->SurfacePtr) = 0.0;
        }

        if (!(state.dataGlobal->BeginEnvrnFlag)) this->MyEnvrnFlag = true;

        this->TotalSurfaceArea = this->LengthRink * this->WidthRink;
        this->InletTemp = state.dataLoopNodes->Node(this->InNode).Temp;
        this->OutletTemp = state.dataLoopNodes->Node(this->OutNode).Temp;
        this->FlowRate = state.dataLoopNodes->Node(this->InNode).MassFlowRate;
    }

    void IceRinkData::setupOutputVariables(EnergyPlusData &state)
    {
        // Set up output variables CurrentModuleObject='IceRink:Indoor'
        SetupOutputVariable(state,
                            "Ice Rink Refrigerant Outlet Temperature", // TODO: These names need to be more verbose
                            OutputProcessor::Unit::C,
                            this->TRefigOutCheck,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);
        SetupOutputVariable(state,
                            "Ice Rink Refrigerant Inlet Temperature",
                            OutputProcessor::Unit::C,
                            this->RefrigTempIn,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);
        SetupOutputVariable(state,
                            "Ice Rink Calculated Specific Heat",
                            OutputProcessor::Unit::J_kgK,
                            this->CpRefrig,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);
        SetupOutputVariable(state,
                            "Ice Rink Refrigerant Mass Flow Rate",
                            OutputProcessor::Unit::kg_s,
                            this->RefrigMassFlow,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);
        SetupOutputVariable(state,
                            "Ice Rink Maximum Mass Flow Rate",
                            OutputProcessor::Unit::kg_s,
                            this->maxmdot,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);
        SetupOutputVariable(state,
                            "Ice Rink Inside Surface Temperature",
                            OutputProcessor::Unit::C,
                            this->Tsurfin1,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);
        SetupOutputVariable(state,
                            "Ice Rink Effective Source Temperature",
                            OutputProcessor::Unit::C,
                            this->Tsrc,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);
        SetupOutputVariable(state,
                            "Ice Rink Maximum Cooling Capacity to Freeze Water",
                            OutputProcessor::Unit::W,
                            this->Qsrcmax,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);
        SetupOutputVariable(state,
                            "Ice Rink Cooling Rate Required to Meet Setpoint",
                            OutputProcessor::Unit::W,
                            this->Qsetpoint,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);
        SetupOutputVariable(state,
                            "Ice Rink Cooling Rate",
                            OutputProcessor::Unit::W,
                            this->Q,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);
        SetupOutputVariable(state,
                            "Ice Rink Resurfacer Heat Transfer Rate",
                            OutputProcessor::Unit::W,
                            this->QResurface,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);
        SetupOutputVariable(state,
                            "Ice Rink Heat Exchanger Effectiveness",
                            OutputProcessor::Unit::None,
                            this->Effectiveness,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);
        SetupOutputVariable(state,
                            "Ice Rink Refrigeration Power",
                            OutputProcessor::Unit::W,
                            this->CoolPower,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            this->Name);
        SetupOutputVariable(state,
                            "Ice Rink Refrigeration Energy",
                            OutputProcessor::Unit::J,
                            this->CoolEnergy,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            this->Name,
                            _,
                            "Electricity",
                            "REFRIGERATION",
                            _,
                            "System");
        SetupOutputVariable(state,
                            "Ice Rink Resurfacer Energy",
                            OutputProcessor::Unit::J,
                            this->HeatingWater,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            this->Name,
                            _,
                            "Electricity",
                            "WaterSystem",
                            _,
                            "System");
    }

    Real64 IceRinkData::IceRinkFreezing(EnergyPlusData &state)
    {
        Real64 constexpr QFusion(333550.00);
        Real64 constexpr CpIce(2108.00);
        static constexpr std::string_view RoutineName("IceRinkFreezing");

        this->RhoWater = FluidProperties::GetDensityGlycol(state, "WATER", this->WaterTemp, this->WaterIndex, RoutineName);
        this->CpWater = FluidProperties::GetSpecificHeatGlycol(state, "WATER", this->WaterTemp, this->WaterIndex, RoutineName);
        Real64 Volume = this->LengthRink * this->WidthRink * this->IceThickness;

        return 1.3 * (RhoWater * Volume * ((CpWater * this->WaterTemp) + (QFusion) + (CpIce * (0 - this->DesignSetPoint))) /
                      (hrstofreeze * DataGlobalConstants::SecInHour));
    }

    Real64 IceRinkData::calcEffectiveness(EnergyPlusData &state,
                                          Real64 const Temperature,           // Temperature of refrigerant entering the floor radiant system, in C
                                          Real64 const lRefrigMassFlow) const // Mass flow rate of refrigerant in the floor radiant system, in kg/s
    {
        static constexpr std::string_view RoutineName("IceRink:calcEffectiveness");

        // Get properties
        Real64 const SpecificHeat = FluidProperties::GetSpecificHeatGlycol(
            state, state.dataPlnt->PlantLoop(this->LoopNum).FluidName, Temperature, state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex, RoutineName);
        Real64 const Conductivity = FluidProperties::GetConductivityGlycol(
            state, state.dataPlnt->PlantLoop(this->LoopNum).FluidName, Temperature, state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex, RoutineName);
        Real64 const Viscosity = FluidProperties::GetViscosityGlycol(
            state, state.dataPlnt->PlantLoop(this->LoopNum).FluidName, Temperature, state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex, RoutineName);

        // Calculate the Reynold's number from RE=(4*MassFlow)/(Pi*Mu*Diameter)
        Real64 const ReynoldsNum =
            4.0 * lRefrigMassFlow / (DataGlobalConstants::Pi * Viscosity * this->TubeDiameter * this->circuits); // circuits is defined in .hh file.

        Real64 const PrandtlNum = Viscosity * SpecificHeat / Conductivity;

        // Calculate the Nusselt number based on what flow regime one is in. h = (k)(Nu)/D
        Real64 NusseltNum = 3.66;              // Laminar flow --> use constant surface temperature relation
        Real64 constexpr MaxLaminarRe(2300.0); // Maximum Reynolds number for laminar flow
        if (ReynoldsNum >= MaxLaminarRe) {     // Turbulent flow --> use Dittus-Boelter equation
            NusseltNum = 0.023 * std::pow(ReynoldsNum, 0.8) * std::pow(PrandtlNum, 0.33);
        }

        Real64 const NTU =
            DataGlobalConstants::Pi * Conductivity * NusseltNum * (this->TubeLength * this->circuits) / (lRefrigMassFlow * SpecificHeat);
        Real64 constexpr MaxExpPower(50.0);
        if (NTU <= MaxExpPower) {
            return 1.0 - std::exp(-NTU);
        }
        return 1.0;
    }

    void IceRinkData::calculateIceRink(EnergyPlusData &state, Real64 &CurLoad) // should this begin in the second time-step?
    {
        static constexpr std::string_view RoutineName("IceRink:calculateDirectIceRink");
        auto &SurfNum = this->SurfacePtr;

        this->operation = ScheduleManager::GetCurrentScheduleValue(state, this->SchedPtr) >= 1 ? 1 : 0;
        Real64 const PipeArea = (DataGlobalConstants::Pi * this->TubeLength * this->circuits * this->TubeDiameter); // pipe surface area

        if (this->InNode == 0) {
            ShowSevereError(state, "Illegal inlet node for the refrigerant in the direct system");
            ShowFatalError(state, "Preceding condition causes termination");
        }

        this->Tsurfin1 = state.dataHeatBalSurf->SurfInsideTempHist(1)(SurfNum);
        this->Tsrc = state.dataHeatBalSurf->SurfTempSource(SurfNum);
        this->CpRefrig = FluidProperties::GetSpecificHeatGlycol(state,
                                                                state.dataPlnt->PlantLoop(this->LoopNum).FluidName,
                                                                this->RefrigTempIn,
                                                                state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex,
                                                                RoutineName);

        int ConstrNum = state.dataSurface->Surface(SurfNum).Construction;
        this->coeffs.Ca = state.dataHeatBalFanSys->RadSysTiHBConstCoef(SurfNum);
        this->coeffs.Cb = state.dataHeatBalFanSys->RadSysTiHBToutCoef(SurfNum);
        this->coeffs.Cc = state.dataHeatBalFanSys->RadSysTiHBQsrcCoef(SurfNum);

        this->coeffs.Cd = state.dataHeatBalFanSys->RadSysToHBConstCoef(SurfNum);
        this->coeffs.Ce = state.dataHeatBalFanSys->RadSysToHBTinCoef(SurfNum);
        this->coeffs.Cf = state.dataHeatBalFanSys->RadSysToHBQsrcCoef(SurfNum);

        this->coeffs.Cg = state.dataHeatBalFanSys->CTFTsrcConstPart(SurfNum);
        this->coeffs.Ch = state.dataConstruction->Construct(ConstrNum).CTFTSourceQ(0);
        this->coeffs.Ci = state.dataConstruction->Construct(ConstrNum).CTFTSourceIn(0);
        this->coeffs.Cj = state.dataConstruction->Construct(ConstrNum).CTFTSourceOut(0);

        this->coeffs.Ck = this->coeffs.Cg + ((this->coeffs.Ci * (this->coeffs.Ca + this->coeffs.Cb * this->coeffs.Cd) +
                                              this->coeffs.Cj * (this->coeffs.Cd + this->coeffs.Ce * this->coeffs.Ca)) /
                                             (1.0 - this->coeffs.Ce * this->coeffs.Cb));
        this->coeffs.Cl = this->coeffs.Ch + ((this->coeffs.Ci * (this->coeffs.Cc + this->coeffs.Cb * this->coeffs.Cf) +
                                              this->coeffs.Cj * (this->coeffs.Cf + this->coeffs.Ce * this->coeffs.Cc)) /
                                             (1.0 - this->coeffs.Ce * this->coeffs.Cb));

        this->RefrigTempIn = state.dataLoopNodes->Node(this->InNode).Temp;

        this->RefrigMassFlow = this->PastRefrigMassFlow;
        this->Qsrcmax2 = this->PastRefrigMassFlow * CpRefrig * (this->RefrigTempIn - this->Tsrc);
        this->Qsetpoint =
            ((((1 - (this->coeffs.Cb * this->coeffs.Ce)) * this->IceSetPointTemp) - this->coeffs.Ca - (this->coeffs.Cb * this->coeffs.Cd)) /
             (this->coeffs.Cc + (this->coeffs.Cb * this->coeffs.Cf)));
        this->Effectiveness = 1.0; // TODO: Edwin added this temporarily -- the Effectiveness is used in this equation but it isn't assigned until later...
        this->ReqMassFlow =
            (((this->coeffs.Ck - this->RefrigTempIn) / (this->deltatemp)) - (1 / Effectiveness)) * (PipeArea / (this->CpRefrig * this->coeffs.Cl));
        // Floor Surface temperatures. Only the current temperature is used. The others are for tracking and reporting purposes of the simulation.

        this->RhoWater = FluidProperties::GetDensityGlycol(state, "WATER", this->Tsurfin1, this->WaterIndex, RoutineName);
        this->CpWater = FluidProperties::GetSpecificHeatGlycol(state, "WATER", this->Tsurfin1, this->WaterIndex, RoutineName);
        Real64 EpsMassFlowRateCp;
        this->RhoWater = FluidProperties::GetDensityGlycol(
            state, "WATER", this->InitialWaterTemp, this->WaterIndex, RoutineName); // this is used in resurfacer heating water operation
        this->CpWater = FluidProperties::GetSpecificHeatGlycol(
            state, "WATER", this->InitialWaterTemp, this->WaterIndex, RoutineName); // this is used in resurfacer heating water operation
        this->ResurfaceON = ScheduleManager::GetCurrentScheduleValue(state, this->ResurfacingSchedPtr) >= 1 ? 1 : 0;

        if (this->operation >= 1) { // If schedule's value is greater than or equal to 1 then Ice Rink is ON.

            this->QResurface = this->ResurfaceON * 1000.0 * 1000.0 * this->ResurfTank *
                               ((4.2 * this->ResurfWaterTemp) + (334.0) - (2.0 * this->Tsurfin1)) /
                               ((1.0 / state.dataGlobal->NumOfTimeStepInHour) *
                                (DataGlobalConstants::SecInHour)); // 1000*1000 just to convert to Joules from KJoules - check ASHRAE
            this->HeatingWater = this->ResurfaceON * this->ResurfTank * RhoWater * CpWater * (this->ResurfWaterTemp - this->InitialWaterTemp);
            if (this->Tsurfin1 > 0) {
                this->QResurface = 0;
                this->HeatingWater = 0;
            }

            if (this->Tsurfin1 <= this->IceSetPointTemp) {
                // If the current temperature is lower than the ice setpoint then do nothing.

                state.dataHeatBalFanSys->QRadSysSource(SurfNum) = 0.0;
                this->Q = 0;
                this->RefrigMassFlow = 0.0;
                this->ReqMassFlow = 0;
                this->PastRefrigMassFlow = this->RefrigMassFlow;
                PlantUtilities::SetComponentFlowRate(
                    state, this->PastRefrigMassFlow, InNode, OutNode, this->LoopNum, this->LoopSide, this->BranchNum, this->CompNum);

            } else {

                if (this->Qsrcmax2 <= this->Qsetpoint) {
                    // If current temperature is above 0 then refrigeration is needed.

                    this->RefrigMassFlow = this->maxmdot;
                    this->PastRefrigMassFlow = this->RefrigMassFlow;
                    PlantUtilities::SetComponentFlowRate(
                        state, this->RefrigMassFlow, InNode, OutNode, this->LoopNum, this->LoopSide, this->BranchNum, this->CompNum);

                    if (this->RefrigMassFlow <= 0) {
                        state.dataHeatBalFanSys->QRadSysSource(SurfNum) = 0;
                    } else {

                        this->Effectiveness = calcEffectiveness(state, this->RefrigTempIn, this->RefrigMassFlow);
                        EpsMassFlowRateCp = this->Effectiveness * this->RefrigMassFlow * this->CpRefrig;
                        this->Q =
                            EpsMassFlowRateCp * (this->RefrigTempIn - this->coeffs.Ck) / (1.0 + (EpsMassFlowRateCp * this->coeffs.Cl / PipeArea));
                        state.dataHeatBalFanSys->QRadSysSource(SurfNum) =
                            this->QResurface + (this->Q); // This the Q to update the EnergyPlus heat balance to get new surface temperatures
                    }

                } else {
                    if (this->ReqMassFlow < 0) {
                        state.dataHeatBalFanSys->QRadSysSource(SurfNum) = 0.0;
                        this->Q = 0;
                        this->RefrigMassFlow = 0.0;
                        this->ReqMassFlow = 0;
                        this->PastRefrigMassFlow = this->RefrigMassFlow;
                        PlantUtilities::SetComponentFlowRate(
                            state, this->RefrigMassFlow, InNode, OutNode, this->LoopNum, this->LoopSide, this->BranchNum, this->CompNum);
                    } else {
                        this->RefrigMassFlow = this->ReqMassFlow;
                        this->PastRefrigMassFlow = this->RefrigMassFlow;
                        PlantUtilities::SetComponentFlowRate(
                            state, this->RefrigMassFlow, InNode, OutNode, this->LoopNum, this->LoopSide, this->BranchNum, this->CompNum);

                        if (this->RefrigMassFlow <= 0) {
                            state.dataHeatBalFanSys->QRadSysSource(SurfNum) = 0;
                        } else {
                            this->Effectiveness = calcEffectiveness(state, this->RefrigTempIn, this->RefrigMassFlow);

                            EpsMassFlowRateCp = this->Effectiveness * this->RefrigMassFlow * this->CpRefrig;
                            this->Q =
                                EpsMassFlowRateCp * (this->RefrigTempIn - this->coeffs.Ck) / (1.0 + (EpsMassFlowRateCp * this->coeffs.Cl / PipeArea));

                            state.dataHeatBalFanSys->QRadSysSource(SurfNum) =
                                this->QResurface + (this->Q); // This the Q to update the EnergyPlus heat balance to get new surface temperatures
                        }
                    }
                }
            }

        } else { // If schedule 's value equals to zero then ice rink is OFF.
            state.dataHeatBalFanSys->QRadSysSource(SurfNum) = 0.0;
            this->Q = 0;
            this->RefrigMassFlow = 0.0;
            this->ReqMassFlow = 0;
            this->PastRefrigMassFlow = this->RefrigMassFlow;
        }

        if (this->RefrigMassFlow > 0) // If mass flow is found then calculate outlet refrigerant temperature.
            this->TRefigOutCheck = this->RefrigTempIn - ((state.dataHeatBalFanSys->QRadSysSource(SurfNum)) / (this->RefrigMassFlow * this->CpRefrig));

        HeatBalanceSurfaceManager::CalcHeatBalanceOutsideSurf(
            state, this->ZonePtr); // This subroutine performs a heat balance on the outside face of each surface in the building
        HeatBalanceSurfaceManager::CalcHeatBalanceInsideSurf(
            state, this->ZonePtr); // This subroutine performs a heat balance on the outside face of each surface in the building
        this->Qsource2 = state.dataHeatBalFanSys->QRadSysSource(SurfNum);

        this->LoadMet = state.dataHeatBalFanSys->QRadSysSource(SurfNum);
        CurLoad = this->LoadMet;
    }

    void IceRinkData::report(EnergyPlusData &state)
    {
        this->CoolPower = fabs(this->LoadMet) / this->COP;
        this->CoolEnergy = this->CoolPower * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
    }
} // namespace IceRink
} // namespace EnergyPlus
