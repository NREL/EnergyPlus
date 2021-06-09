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
#include <iostream>
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EnergyPlus/Autosizing/Base.hh>
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutsideEnergySources.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus::OutsideEnergySources {

// MODULE INFORMATION:
//       AUTHOR         Dan Fisher
//       DATE WRITTEN   Unknown
//       MODIFIED       na
//       RE-ENGINEERED  Brent Griffith, Sept 2010, revised plant interactions.

// PURPOSE OF THIS MODULE:
// Module containing the routines dealing with the OutsideEnergySources

PlantComponent *OutsideEnergySourceSpecs::factory(EnergyPlusData &state, int objectType, std::string objectName)
{
    // Process the input data for outside energy sources if it hasn't been done already
    if (state.dataOutsideEnergySrcs->SimOutsideEnergyGetInputFlag) {
        GetOutsideEnergySourcesInput(state);
        state.dataOutsideEnergySrcs->SimOutsideEnergyGetInputFlag = false;
    }
    // Now look for this particular pipe in the list
    for (auto &source : state.dataOutsideEnergySrcs->EnergySource) {
        if (source.EnergyType == objectType && source.Name == objectName) {
            return &source;
        }
    }
    // If we didn't find it, fatal
    ShowFatalError(state, "OutsideEnergySourceSpecsFactory: Error getting inputs for source named: " + objectName); // LCOV_EXCL_LINE
    // Shut up the compiler
    return nullptr; // LCOV_EXCL_LINE
}

void OutsideEnergySourceSpecs::simulate(EnergyPlusData &state,
                                        [[maybe_unused]] const PlantLocation &calledFromLocation,
                                        [[maybe_unused]] bool FirstHVACIteration,
                                        Real64 &CurLoad,
                                        bool RunFlag)
{
    this->initialize(state, CurLoad);
    this->calculate(state, RunFlag, CurLoad);
}

void OutsideEnergySourceSpecs::onInitLoopEquip(EnergyPlusData &state, const PlantLocation &)
{
    this->initialize(state, 0.0);
    this->size(state);
}

void OutsideEnergySourceSpecs::getDesignCapacities([[maybe_unused]] EnergyPlusData &state,
                                                   [[maybe_unused]] const PlantLocation &calledFromLocation,
                                                   Real64 &MaxLoad,
                                                   Real64 &MinLoad,
                                                   Real64 &OptLoad)
{
    MinLoad = 0.0;
    MaxLoad = this->NomCap;
    OptLoad = this->NomCap;
}

void GetOutsideEnergySourcesInput(EnergyPlusData &state)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Dan Fisher
    //       DATE WRITTEN   April 1998
    //       MODIFIED       May 2010; Edwin Lee; Linda Lawrie (consolidation)
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This routine obtains the input data puts it into the
    // component arrays. Data items in the component arrays
    // are initialized. Output variables are set up.

    // GET NUMBER OF ALL EQUIPMENT TYPES
    int const NumDistrictUnitsHeat = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "DistrictHeating");
    int const NumDistrictUnitsCool = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "DistrictCooling");
    state.dataOutsideEnergySrcs->NumDistrictUnits = NumDistrictUnitsHeat + NumDistrictUnitsCool;

    if (allocated(state.dataOutsideEnergySrcs->EnergySource)) return;

    state.dataOutsideEnergySrcs->EnergySource.allocate(state.dataOutsideEnergySrcs->NumDistrictUnits);
    state.dataOutsideEnergySrcs->EnergySourceUniqueNames.reserve(static_cast<unsigned>(state.dataOutsideEnergySrcs->NumDistrictUnits));

    bool ErrorsFound(false); // If errors detected in input
    int heatIndex = 0;
    int coolIndex = 0;

    for (int EnergySourceNum = 1; EnergySourceNum <= state.dataOutsideEnergySrcs->NumDistrictUnits; ++EnergySourceNum) {

        std::string reportVarPrefix;
        std::string nodeNames;
        int typeOf;
        int thisIndex;
        if (EnergySourceNum <= NumDistrictUnitsHeat) {
            state.dataIPShortCut->cCurrentModuleObject = "DistrictHeating";
            reportVarPrefix = "District Heating ";
            nodeNames = "Hot Water Nodes";
            typeOf = DataPlant::TypeOf_PurchHotWater;
            heatIndex++;
            thisIndex = heatIndex;
        } else {
            state.dataIPShortCut->cCurrentModuleObject = "DistrictCooling";
            reportVarPrefix = "District Cooling ";
            nodeNames = "Chilled Water Nodes";
            typeOf = DataPlant::TypeOf_PurchChilledWater;
            coolIndex++;
            thisIndex = coolIndex;
        }

        int NumAlphas = 0, NumNums = 0, IOStat = 0;
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 state.dataIPShortCut->cCurrentModuleObject,
                                                                 thisIndex,
                                                                 state.dataIPShortCut->cAlphaArgs,
                                                                 NumAlphas,
                                                                 state.dataIPShortCut->rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 _,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames);

        if (EnergySourceNum > 1) {
            GlobalNames::VerifyUniqueInterObjectName(state,
                                                     state.dataOutsideEnergySrcs->EnergySourceUniqueNames,
                                                     state.dataIPShortCut->cAlphaArgs(1),
                                                     state.dataIPShortCut->cCurrentModuleObject,
                                                     state.dataIPShortCut->cAlphaFieldNames(1),
                                                     ErrorsFound);
        }
        state.dataOutsideEnergySrcs->EnergySource(EnergySourceNum).Name = state.dataIPShortCut->cAlphaArgs(1);
        state.dataOutsideEnergySrcs->EnergySource(EnergySourceNum).InletNodeNum =
            NodeInputManager::GetOnlySingleNode(state,
                                                state.dataIPShortCut->cAlphaArgs(2),
                                                ErrorsFound,
                                                state.dataIPShortCut->cCurrentModuleObject,
                                                state.dataIPShortCut->cAlphaArgs(1),
                                                DataLoopNode::NodeFluidType::Water,
                                                DataLoopNode::NodeConnectionType::Inlet,
                                                NodeInputManager::compFluidStream::Primary,
                                                DataLoopNode::ObjectIsNotParent);
        state.dataOutsideEnergySrcs->EnergySource(EnergySourceNum).OutletNodeNum =
            NodeInputManager::GetOnlySingleNode(state,
                                                state.dataIPShortCut->cAlphaArgs(3),
                                                ErrorsFound,
                                                state.dataIPShortCut->cCurrentModuleObject,
                                                state.dataIPShortCut->cAlphaArgs(1),
                                                DataLoopNode::NodeFluidType::Water,
                                                DataLoopNode::NodeConnectionType::Outlet,
                                                NodeInputManager::compFluidStream::Primary,
                                                DataLoopNode::ObjectIsNotParent);
        BranchNodeConnections::TestCompSet(state,
                                           state.dataIPShortCut->cCurrentModuleObject,
                                           state.dataIPShortCut->cAlphaArgs(1),
                                           state.dataIPShortCut->cAlphaArgs(2),
                                           state.dataIPShortCut->cAlphaArgs(3),
                                           nodeNames);
        state.dataOutsideEnergySrcs->EnergySource(EnergySourceNum).NomCap = state.dataIPShortCut->rNumericArgs(1);
        if (state.dataOutsideEnergySrcs->EnergySource(EnergySourceNum).NomCap == DataSizing::AutoSize) {
            state.dataOutsideEnergySrcs->EnergySource(EnergySourceNum).NomCapWasAutoSized = true;
        }
        state.dataOutsideEnergySrcs->EnergySource(EnergySourceNum).EnergyTransfer = 0.0;
        state.dataOutsideEnergySrcs->EnergySource(EnergySourceNum).EnergyRate = 0.0;
        state.dataOutsideEnergySrcs->EnergySource(EnergySourceNum).EnergyType = typeOf;
        if (!state.dataIPShortCut->lAlphaFieldBlanks(4)) {
            state.dataOutsideEnergySrcs->EnergySource(EnergySourceNum).CapFractionSchedNum =
                ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(4));
            if (state.dataOutsideEnergySrcs->EnergySource(EnergySourceNum).CapFractionSchedNum == 0) {
                ShowSevereError(state,
                                state.dataIPShortCut->cCurrentModuleObject + "=\"" + state.dataOutsideEnergySrcs->EnergySource(EnergySourceNum).Name +
                                    "\", is not valid");
                ShowContinueError(state,
                                  state.dataIPShortCut->cAlphaFieldNames(4) + "=\"" + state.dataIPShortCut->cAlphaArgs(4) + "\" was not found.");
                ErrorsFound = true;
            }
            if (!ScheduleManager::CheckScheduleValueMinMax(
                    state, state.dataOutsideEnergySrcs->EnergySource(EnergySourceNum).CapFractionSchedNum, ">=", 0.0)) {
                ShowWarningError(state,
                                 state.dataIPShortCut->cCurrentModuleObject + "=\"" +
                                     state.dataOutsideEnergySrcs->EnergySource(EnergySourceNum).Name + "\", is not valid");
                ShowContinueError(state,
                                  state.dataIPShortCut->cAlphaFieldNames(4) + "=\"" + state.dataIPShortCut->cAlphaArgs(4) +
                                      "\" should not have negative values.");
                ShowContinueError(state, "Negative values will be treated as zero, and the simulation continues.");
            }
        } else {
            state.dataOutsideEnergySrcs->EnergySource(EnergySourceNum).CapFractionSchedNum = DataGlobalConstants::ScheduleAlwaysOn;
        }
    }

    if (ErrorsFound) {
        ShowFatalError(state,
                       "Errors found in processing input for " + state.dataIPShortCut->cCurrentModuleObject +
                           ", Preceding condition caused termination.");
    }
}

void OutsideEnergySourceSpecs::initialize(EnergyPlusData &state, Real64 MyLoad)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR:          Dan Fisher
    //       DATE WRITTEN:    October 1998
    //       MODIFIED       May 2010; Edwin Lee; Linda Lawrie (consolidation)
    //       RE-ENGINEERED  Sept 2010, Brent Griffith, plant rewrite

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine does one-time inits and sets the operating mass flow rate of this machine

    // METHODOLOGY EMPLOYED:
    // One time inits include validating source type (should happen in getinput?) and locating this
    //  component on the PlantLoop topology.
    // The mass flow rate is determined based on component load, and making use of
    //  the SetComponentFlowRate routine.
    // The mass flow rate could be an inter-connected-loop side trigger. This is not really the type of
    //  interconnect that that routine was written for, but it is the clearest example of using it.

    if (this->OneTimeInitFlag) {
        // Locate the unit on the plant loops for later usage
        bool errFlag = false;
        PlantUtilities::ScanPlantLoopsForObject(
            state, this->Name, this->EnergyType, this->LoopNum, this->LoopSideNum, this->BranchNum, this->CompNum, errFlag, _, _, _, _, _);
        if (errFlag) {
            ShowFatalError(state, "InitSimVars: Program terminated due to previous condition(s).");
        }
        // set limits on outlet node temps to plant loop limits
        state.dataPlnt->PlantLoop(this->LoopNum).LoopSide(this->LoopSideNum).Branch(this->BranchNum).Comp(this->CompNum).MinOutletTemp =
            state.dataPlnt->PlantLoop(this->LoopNum).MinTemp;
        state.dataPlnt->PlantLoop(this->LoopNum).LoopSide(this->LoopSideNum).Branch(this->BranchNum).Comp(this->CompNum).MaxOutletTemp =
            state.dataPlnt->PlantLoop(this->LoopNum).MaxTemp;
        // Register design flow rate for inlet node (helps to autosize comp setpoint op scheme flows
        PlantUtilities::RegisterPlantCompDesignFlow(state, this->InletNodeNum, state.dataPlnt->PlantLoop(this->LoopNum).MaxVolFlowRate);

        this->OneTimeInitFlag = false;

        // this may need some help, if the objects change location later, due to a push_back,
        //  then the pointers to these output variables will be bad
        // for (int EnergySourceNum = 1; EnergySourceNum <= NumDistrictUnits; ++EnergySourceNum) {
        std::string hotOrChilled = "Hot ";
        std::string reportVarPrefix = "District Heating ";
        std::string heatingOrCooling = "Heating";
        std::string typeName = DataPlant::ccSimPlantEquipTypes(DataPlant::TypeOf_PurchHotWater);
        if (this->EnergyType == DataPlant::TypeOf_PurchChilledWater) {
            hotOrChilled = "Chilled ";
            reportVarPrefix = "District Cooling ";
            heatingOrCooling = "Cooling";
            typeName = DataPlant::ccSimPlantEquipTypes(DataPlant::TypeOf_PurchChilledWater);
        }

        SetupOutputVariable(state,
                            reportVarPrefix + hotOrChilled + "Water Energy",
                            OutputProcessor::Unit::J,
                            this->EnergyTransfer,
                            "System",
                            "Sum",
                            this->Name,
                            _,
                            typeName,
                            heatingOrCooling,
                            _,
                            "Plant");
        SetupOutputVariable(
            state, reportVarPrefix + hotOrChilled + "Water Rate", OutputProcessor::Unit::W, this->EnergyRate, "System", "Average", this->Name);

        SetupOutputVariable(state, reportVarPrefix + "Rate", OutputProcessor::Unit::W, this->EnergyRate, "System", "Average", this->Name);
        SetupOutputVariable(state, reportVarPrefix + "Inlet Temperature", OutputProcessor::Unit::C, this->InletTemp, "System", "Average", this->Name);
        SetupOutputVariable(
            state, reportVarPrefix + "Outlet Temperature", OutputProcessor::Unit::C, this->OutletTemp, "System", "Average", this->Name);
        SetupOutputVariable(
            state, reportVarPrefix + "Mass Flow Rate", OutputProcessor::Unit::kg_s, this->MassFlowRate, "System", "Average", this->Name);
    }

    //}

    // begin environment inits
    if (state.dataGlobal->BeginEnvrnFlag && this->BeginEnvrnInitFlag) {
        // component model has not design flow rates, using data for overall plant loop
        PlantUtilities::InitComponentNodes(state,
                                           state.dataPlnt->PlantLoop(this->LoopNum).MinMassFlowRate,
                                           state.dataPlnt->PlantLoop(this->LoopNum).MaxMassFlowRate,
                                           this->InletNodeNum,
                                           this->OutletNodeNum,
                                           this->LoopNum,
                                           this->LoopSideNum,
                                           this->BranchNum,
                                           this->CompNum);
        this->BeginEnvrnInitFlag = false;
    }
    if (!state.dataGlobal->BeginEnvrnFlag) this->BeginEnvrnInitFlag = true;

    Real64 TempPlantMassFlow(0.0);
    if (std::abs(MyLoad) > 0.0) {
        TempPlantMassFlow = state.dataPlnt->PlantLoop(this->LoopNum).MaxMassFlowRate;
    }

    // get actual mass flow to use, hold in MassFlowRate variable
    PlantUtilities::SetComponentFlowRate(
        state, TempPlantMassFlow, this->InletNodeNum, this->OutletNodeNum, this->LoopNum, this->LoopSideNum, this->BranchNum, this->CompNum);

    this->InletTemp = state.dataLoopNodes->Node(this->InletNodeNum).Temp;
    this->MassFlowRate = TempPlantMassFlow;
}

void OutsideEnergySourceSpecs::size(EnergyPlusData &state)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Daeho Kang
    //       DATE WRITTEN   April 2014
    //       MODIFIED
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    //  This subroutine is for sizing capacities of district cooling and heating objects.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    bool ErrorsFound(false); // If errors detected in input

    // Type name string variable to collapse the sizing for cooling and heating into one block
    std::string typeName;
    if (this->EnergyType == DataPlant::TypeOf_PurchChilledWater) {
        typeName = "Cooling";
    } else { // Heating
        typeName = "Heating";
    }

    int const PltSizNum = state.dataPlnt->PlantLoop(this->LoopNum).PlantSizNum;
    if (PltSizNum > 0) {
        Real64 const rho = FluidProperties::GetDensityGlycol(state,
                                                             state.dataPlnt->PlantLoop(this->LoopNum).FluidName,
                                                             DataGlobalConstants::InitConvTemp,
                                                             state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex,
                                                             "SizeDistrict" + typeName);
        Real64 const Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                                 state.dataPlnt->PlantLoop(this->LoopNum).FluidName,
                                                                 DataGlobalConstants::InitConvTemp,
                                                                 state.dataPlnt->PlantLoop(this->LoopNum).FluidIndex,
                                                                 "SizeDistrict" + typeName);
        Real64 const NomCapDes = Cp * rho * state.dataSize->PlantSizData(PltSizNum).DeltaT * state.dataSize->PlantSizData(PltSizNum).DesVolFlowRate;
        if (state.dataPlnt->PlantFirstSizesOkayToFinalize) {
            if (this->NomCapWasAutoSized) {
                this->NomCap = NomCapDes;
                if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(state, "District" + typeName, this->Name, "Design Size Nominal Capacity [W]", NomCapDes);
                }
                if (state.dataPlnt->PlantFirstSizesOkayToReport) {
                    BaseSizer::reportSizerOutput(state, "District" + typeName, this->Name, "Initial Design Size Nominal Capacity [W]", NomCapDes);
                }
            } else { // Hard-size with sizing data
                if (this->NomCap > 0.0 && NomCapDes > 0.0) {
                    Real64 const NomCapUser = this->NomCap;
                    if (state.dataPlnt->PlantFinalSizesOkayToReport) {
                        BaseSizer::reportSizerOutput(state,
                                                     "District" + typeName,
                                                     this->Name,
                                                     "Design Size Nominal Capacity [W]",
                                                     NomCapDes,
                                                     "User-Specified Nominal Capacity [W]",
                                                     NomCapUser);
                        if (state.dataGlobal->DisplayExtraWarnings) {
                            if ((std::abs(NomCapDes - NomCapUser) / NomCapUser) > state.dataSize->AutoVsHardSizingThreshold) {
                                ShowMessage(state, "SizeDistrict" + typeName + ": Potential issue with equipment sizing for " + this->Name);
                                ShowContinueError(state, format("User-Specified Nominal Capacity of {:.2R} [W]", NomCapUser));
                                ShowContinueError(state, format("differs from Design Size Nominal Capacity of {:.2R} [W]", NomCapDes));
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
            ShowSevereError(state, "Autosizing of District " + typeName + " nominal capacity requires a loop Sizing:Plant object");
            ShowContinueError(state, "Occurs in District" + typeName + " object=" + this->Name);
            ErrorsFound = true;
        }
        if (!this->NomCapWasAutoSized && this->NomCap > 0.0 && state.dataPlnt->PlantFinalSizesOkayToReport) {
            BaseSizer::reportSizerOutput(state, "District" + typeName, this->Name, "User-Specified Nominal Capacity [W]", this->NomCap);
        }
    }
    if (ErrorsFound) {
        ShowFatalError(state, "Preceding sizing errors cause program termination");
    }
}

void OutsideEnergySourceSpecs::calculate(EnergyPlusData &state, bool runFlag, Real64 MyLoad)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Dan Fisher
    //       DATE WRITTEN   July 1998
    //       MODIFIED       May 2010; Edwin Lee; Linda Lawrie (consolidation)
    //       RE-ENGINEERED  Sept 2010, Brent Griffith, plant rewrite

    // SUBROUTINE PARAMETER DEFINITIONS:
    static std::string const RoutineName("SimDistrictEnergy");

    // set inlet and outlet nodes
    int const LoopNum = this->LoopNum;
    Real64 const LoopMinTemp = state.dataPlnt->PlantLoop(LoopNum).MinTemp;
    Real64 const LoopMaxTemp = state.dataPlnt->PlantLoop(LoopNum).MaxTemp;

    Real64 const Cp = FluidProperties::GetSpecificHeatGlycol(
        state, state.dataPlnt->PlantLoop(LoopNum).FluidName, this->InletTemp, state.dataPlnt->PlantLoop(LoopNum).FluidIndex, RoutineName);

    //  apply power limit from input
    Real64 CapFraction = ScheduleManager::GetCurrentScheduleValue(state, this->CapFractionSchedNum);
    CapFraction = max(0.0, CapFraction); // ensure non negative
    Real64 const CurrentCap = this->NomCap * CapFraction;
    if (std::abs(MyLoad) > CurrentCap) {
        MyLoad = sign(CurrentCap, MyLoad);
    }

    if (this->EnergyType == DataPlant::TypeOf_PurchChilledWater) {
        if (MyLoad > 0.0) MyLoad = 0.0;
    } else if (this->EnergyType == DataPlant::TypeOf_PurchHotWater) {
        if (MyLoad < 0.0) MyLoad = 0.0;
    }

    // determine outlet temp based on inlet temp, cp, and MyLoad
    if ((this->MassFlowRate > 0.0) && runFlag) {
        this->OutletTemp = (MyLoad + this->MassFlowRate * Cp * this->InletTemp) / (this->MassFlowRate * Cp);
        // apply loop limits on temperature result to keep in check
        if (this->OutletTemp < LoopMinTemp) {
            this->OutletTemp = max(this->OutletTemp, LoopMinTemp);
            MyLoad = this->MassFlowRate * Cp * (this->OutletTemp - this->InletTemp);
        }
        if (this->OutletTemp > LoopMaxTemp) {
            this->OutletTemp = min(this->OutletTemp, LoopMaxTemp);
            MyLoad = this->MassFlowRate * Cp * (this->OutletTemp - this->InletTemp);
        }
    } else {
        this->OutletTemp = this->InletTemp;
        MyLoad = 0.0;
    }
    int const OutletNode = this->OutletNodeNum;
    state.dataLoopNodes->Node(OutletNode).Temp = this->OutletTemp;
    this->EnergyRate = std::abs(MyLoad);
    this->EnergyTransfer = this->EnergyRate * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
}

} // namespace EnergyPlus::OutsideEnergySources
