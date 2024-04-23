// EnergyPlus, Copyright (c) 1996-2024, The Board of Trustees of the University of Illinois,
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

// EnergyPlus Headers
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/EMSManager.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/PlantLoadProfile.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus::PlantLoadProfile {

// MODULE INFORMATION:
//       AUTHOR         Peter Graham Ellis
//       DATE WRITTEN   January 2004
//       MODIFIED       Brent Griffith, plant rewrite, general fluid types
//                      allow flow requests with out load requests
//       RE-ENGINEERED  na

// PURPOSE OF THIS MODULE:
// This module simulates a scheduled load profile on the demand side of the plant loop.

// METHODOLOGY EMPLOYED:
// The plant load profile object provides a scheduled load on the plant loop.  Unlike most plant equipment
// on the demand side, i.e. zone equipment, this object does not have a zone associated with it.
// For this reason the plant load profile can only be called for simulation by the non-zone equipment
// manager (see NonZoneEquipmentManager.cc).

constexpr std::array<std::string_view, static_cast<int>(PlantLoopFluidType::Num)> PlantLoopFluidTypeNamesUC{"WATER", "STEAM"};

PlantComponent *PlantProfileData::factory(EnergyPlusData &state, std::string const &objectName)
{
    if (state.dataPlantLoadProfile->GetPlantLoadProfileInputFlag) {
        GetPlantProfileInput(state);
        state.dataPlantLoadProfile->GetPlantLoadProfileInputFlag = false;
    }
    // Now look for this particular pipe in the list
    for (auto &plp : state.dataPlantLoadProfile->PlantProfile) {
        if (plp.Name == objectName) {
            return &plp;
        }
    }
    // If we didn't find it, fatal
    ShowFatalError(state, format("PlantLoadProfile::factory: Error getting inputs for pipe named: {}", objectName));
    // Shut up the compiler
    return nullptr;
}

void PlantProfileData::onInitLoopEquip(EnergyPlusData &state, [[maybe_unused]] const PlantLocation &calledFromLocation)
{
    this->InitPlantProfile(state);
}

void PlantProfileData::simulate(EnergyPlusData &state,
                                [[maybe_unused]] const PlantLocation &calledFromLocation,
                                [[maybe_unused]] bool const FirstHVACIteration,
                                [[maybe_unused]] Real64 &CurLoad,
                                [[maybe_unused]] bool const RunFlag)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Peter Graham Ellis
    //       DATE WRITTEN   January 2004
    //       MODIFIED       Brent Griffith, generalize fluid cp
    //                      June 2021, Dareum Nam, Add steam loop version
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Simulates the plant load profile object.

    // METHODOLOGY EMPLOYED:
    // This is a very simple simulation.  InitPlantProfile does the work of getting the scheduled load and flow rate.
    // Flow is requested and the actual available flow is set.  As for water loops, the outlet temperature is calculated. As for steam loops, the mass
    // flow rate of steam and the outlet temperature are calculated.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    static constexpr std::string_view RoutineName("SimulatePlantProfile");
    Real64 DeltaTemp;

    this->InitPlantProfile(state);

    if (this->FluidType == PlantLoopFluidType::Water) {
        if (this->MassFlowRate > 0.0) {
            Real64 Cp = FluidProperties::GetSpecificHeatGlycol(state,
                                                               state.dataPlnt->PlantLoop(this->plantLoc.loopNum).FluidName,
                                                               this->InletTemp,
                                                               state.dataPlnt->PlantLoop(this->plantLoc.loopNum).FluidIndex,
                                                               RoutineName);
            DeltaTemp = this->Power / (this->MassFlowRate * Cp);
        } else {
            this->Power = 0.0;
            DeltaTemp = 0.0;
        }
        this->OutletTemp = this->InletTemp - DeltaTemp;
    } else if (this->FluidType == PlantLoopFluidType::Steam) {
        if (this->MassFlowRate > 0.0 && this->Power > 0.0) {
            Real64 EnthSteamInDry = FluidProperties::GetSatEnthalpyRefrig(state,
                                                                          state.dataPlnt->PlantLoop(this->plantLoc.loopNum).FluidName,
                                                                          this->InletTemp,
                                                                          1.0,
                                                                          state.dataPlnt->PlantLoop(this->plantLoc.loopNum).FluidIndex,
                                                                          RoutineName);
            Real64 EnthSteamOutWet = FluidProperties::GetSatEnthalpyRefrig(state,
                                                                           state.dataPlnt->PlantLoop(this->plantLoc.loopNum).FluidName,
                                                                           this->InletTemp,
                                                                           0.0,
                                                                           state.dataPlnt->PlantLoop(this->plantLoc.loopNum).FluidIndex,
                                                                           RoutineName);
            Real64 LatentHeatSteam = EnthSteamInDry - EnthSteamOutWet;
            Real64 SatTemp = FluidProperties::GetSatTemperatureRefrig(state,
                                                                      state.dataPlnt->PlantLoop(this->plantLoc.loopNum).FluidName,
                                                                      DataEnvironment::StdPressureSeaLevel,
                                                                      state.dataPlnt->PlantLoop(this->plantLoc.loopNum).FluidIndex,
                                                                      RoutineName);
            Real64 CpWater = FluidProperties::GetSpecificHeatGlycol(state,
                                                                    state.dataPlnt->PlantLoop(this->plantLoc.loopNum).FluidName,
                                                                    SatTemp,
                                                                    state.dataPlnt->PlantLoop(this->plantLoc.loopNum).FluidIndex,
                                                                    RoutineName);

            // Steam Mass Flow Rate Required
            this->MassFlowRate = this->Power / (LatentHeatSteam + this->DegOfSubcooling * CpWater);
            PlantUtilities::SetComponentFlowRate(state, this->MassFlowRate, this->InletNode, this->OutletNode, this->plantLoc);
            state.dataLoopNodes->Node(this->OutletNode).Quality = 0.0;
            // In practice Sensible & Superheated heat transfer is negligible compared to latent part.
            // This is required for outlet water temperature, otherwise it will be saturation temperature.
            // Steam Trap drains off all the Water formed.
            // Here Degree of Subcooling is used to calculate hot water return temperature.

            // Calculating Condensate outlet temperature
            this->OutletTemp = SatTemp - this->LoopSubcoolReturn;
        } else {
            this->Power = 0.0;
        }
    }

    this->UpdatePlantProfile(state);
    this->ReportPlantProfile(state);

} // simulate()

void PlantProfileData::InitPlantProfile(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Peter Graham Ellis
    //       DATE WRITTEN   January 2004
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Initializes the plant load profile object during the plant simulation.

    // METHODOLOGY EMPLOYED:
    // Inlet and outlet nodes are initialized.  The scheduled load and flow rate is obtained, flow is requested, and the
    // actual available flow is set.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    static constexpr std::string_view RoutineName("InitPlantProfile");
    Real64 FluidDensityInit;

    // Do the one time initializations

    if (!state.dataGlobal->SysSizingCalc && this->InitSizing) {
        PlantUtilities::RegisterPlantCompDesignFlow(state, InletNode, this->PeakVolFlowRate);
        this->InitSizing = false;
    }

    if (state.dataGlobal->BeginEnvrnFlag && this->Init) {
        // Clear node initial conditions
        state.dataLoopNodes->Node(OutletNode).Temp = 0.0;

        if (this->FluidType == PlantLoopFluidType::Water) {
            FluidDensityInit = FluidProperties::GetDensityGlycol(state,
                                                                 state.dataPlnt->PlantLoop(this->plantLoc.loopNum).FluidName,
                                                                 Constant::InitConvTemp,
                                                                 state.dataPlnt->PlantLoop(this->plantLoc.loopNum).FluidIndex,
                                                                 RoutineName);
        } else { //(this->FluidType == PlantLoopFluidType::Steam)
            Real64 SatTempAtmPress = FluidProperties::GetSatTemperatureRefrig(state,
                                                                              state.dataPlnt->PlantLoop(this->plantLoc.loopNum).FluidName,
                                                                              DataEnvironment::StdPressureSeaLevel,
                                                                              state.dataPlnt->PlantLoop(this->plantLoc.loopNum).FluidIndex,
                                                                              RoutineName);
            FluidDensityInit = FluidProperties::GetSatDensityRefrig(state,
                                                                    state.dataPlnt->PlantLoop(this->plantLoc.loopNum).FluidName,
                                                                    SatTempAtmPress,
                                                                    1.0,
                                                                    state.dataPlnt->PlantLoop(this->plantLoc.loopNum).FluidIndex,
                                                                    RoutineName);
        }

        Real64 MaxFlowMultiplier = ScheduleManager::GetScheduleMaxValue(state, this->FlowRateFracSchedule);

        PlantUtilities::InitComponentNodes(
            state, 0.0, this->PeakVolFlowRate * FluidDensityInit * MaxFlowMultiplier, this->InletNode, this->OutletNode);

        this->EMSOverrideMassFlow = false;
        this->EMSMassFlowValue = 0.0;
        this->EMSOverridePower = false;
        this->EMSPowerValue = 0.0;
        this->Init = false;
    }

    if (!state.dataGlobal->BeginEnvrnFlag) this->Init = true;

    this->InletTemp = state.dataLoopNodes->Node(InletNode).Temp;
    this->Power = ScheduleManager::GetCurrentScheduleValue(state, this->LoadSchedule);

    if (this->EMSOverridePower) this->Power = this->EMSPowerValue;

    if (this->FluidType == PlantLoopFluidType::Water) {
        FluidDensityInit = FluidProperties::GetDensityGlycol(state,
                                                             state.dataPlnt->PlantLoop(this->plantLoc.loopNum).FluidName,
                                                             this->InletTemp,
                                                             state.dataPlnt->PlantLoop(this->plantLoc.loopNum).FluidIndex,
                                                             RoutineName);
    } else { //(this->FluidType == PlantLoopFluidType::Steam)
        FluidDensityInit = FluidProperties::GetSatDensityRefrig(state,
                                                                state.dataPlnt->PlantLoop(this->plantLoc.loopNum).FluidName,
                                                                this->InletTemp,
                                                                1.0,
                                                                state.dataPlnt->PlantLoop(this->plantLoc.loopNum).FluidIndex,
                                                                RoutineName);
    }

    // Get the scheduled mass flow rate
    this->VolFlowRate = this->PeakVolFlowRate * ScheduleManager::GetCurrentScheduleValue(state, this->FlowRateFracSchedule);

    this->MassFlowRate = this->VolFlowRate * FluidDensityInit;

    if (this->EMSOverrideMassFlow) this->MassFlowRate = this->EMSMassFlowValue;

    // Request the mass flow rate from the plant component flow utility routine
    PlantUtilities::SetComponentFlowRate(state, this->MassFlowRate, InletNode, OutletNode, this->plantLoc);

    this->VolFlowRate = this->MassFlowRate / FluidDensityInit;

} // InitPlantProfile()

void PlantProfileData::UpdatePlantProfile(EnergyPlusData &state) const
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Peter Graham Ellis
    //       DATE WRITTEN   January 2004
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Updates the node variables with local variables.

    // Set outlet node variables that are possibly changed
    state.dataLoopNodes->Node(this->OutletNode).Temp = this->OutletTemp;
}

void PlantProfileData::ReportPlantProfile(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Peter Graham Ellis
    //       DATE WRITTEN   January 2004
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Calculates report variables.

    // Using/Aliasing
    Real64 TimeStepSysSec = state.dataHVACGlobal->TimeStepSysSec;

    this->Energy = this->Power * TimeStepSysSec;

    if (this->Energy >= 0.0) {
        this->HeatingEnergy = this->Energy;
        this->CoolingEnergy = 0.0;
    } else {
        this->HeatingEnergy = 0.0;
        this->CoolingEnergy = std::abs(this->Energy);
    }
}
void PlantProfileData::oneTimeInit_new(EnergyPlusData &state)
{
    bool errFlag;

    if (allocated(state.dataPlnt->PlantLoop)) {
        errFlag = false;
        PlantUtilities::ScanPlantLoopsForObject(state, this->Name, this->Type, this->plantLoc, errFlag, _, _, _, _, _);
        if (errFlag) {
            ShowFatalError(state, "InitPlantProfile: Program terminated for previous conditions.");
        }
    }
}
void PlantProfileData::oneTimeInit([[maybe_unused]] EnergyPlusData &state)
{
}

void PlantProfileData::getCurrentPower([[maybe_unused]] EnergyPlusData &state, Real64 &power)
{
    power = this->Power;
    return;
}

// Functions
void GetPlantProfileInput(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Peter Graham Ellis
    //       DATE WRITTEN   January 2004
    //       MODIFIED       June 2021, Dareum Nam, Add steam loop version
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Gets the plant load profile input from the input file and sets up the objects.

    // Using/Aliasing
    using namespace DataLoopNode;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    bool ErrorsFound(false); // Set to true if errors in input, fatal at end of routine
    int IOStatus;            // Used in GetObjectItem
    int NumAlphas;           // Number of Alphas for each GetObjectItem call
    int NumNumbers;          // Number of Numbers for each GetObjectItem call
    int ProfileNum;          // PLANT LOAD PROFILE (PlantProfile) object number
    auto &cCurrentModuleObject = state.dataIPShortCut->cCurrentModuleObject;

    cCurrentModuleObject = "LoadProfile:Plant";
    state.dataPlantLoadProfile->NumOfPlantProfile = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

    if (state.dataPlantLoadProfile->NumOfPlantProfile > 0) {
        state.dataPlantLoadProfile->PlantProfile.allocate(state.dataPlantLoadProfile->NumOfPlantProfile);

        for (ProfileNum = 1; ProfileNum <= state.dataPlantLoadProfile->NumOfPlantProfile; ++ProfileNum) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     cCurrentModuleObject,
                                                                     ProfileNum,
                                                                     state.dataIPShortCut->cAlphaArgs,
                                                                     NumAlphas,
                                                                     state.dataIPShortCut->rNumericArgs,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     _,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            Util::IsNameEmpty(state, state.dataIPShortCut->cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

            state.dataPlantLoadProfile->PlantProfile(ProfileNum).Name = state.dataIPShortCut->cAlphaArgs(1);
            state.dataPlantLoadProfile->PlantProfile(ProfileNum).Type =
                DataPlant::PlantEquipmentType::PlantLoadProfile; // parameter assigned in DataPlant

            state.dataPlantLoadProfile->PlantProfile(ProfileNum).FluidType =
                static_cast<PlantLoopFluidType>(getEnumValue(PlantLoopFluidTypeNamesUC, Util::makeUPPER(state.dataIPShortCut->cAlphaArgs(6))));
            if (state.dataPlantLoadProfile->PlantProfile(ProfileNum).FluidType == PlantLoopFluidType::Invalid) {
                state.dataPlantLoadProfile->PlantProfile(ProfileNum).FluidType = PlantLoopFluidType::Water;
            }

            if (state.dataPlantLoadProfile->PlantProfile(ProfileNum).FluidType == PlantLoopFluidType::Water) {
                state.dataPlantLoadProfile->PlantProfile(ProfileNum).InletNode =
                    NodeInputManager::GetOnlySingleNode(state,
                                                        state.dataIPShortCut->cAlphaArgs(2),
                                                        ErrorsFound,
                                                        DataLoopNode::ConnectionObjectType::LoadProfilePlant,
                                                        state.dataIPShortCut->cAlphaArgs(1),
                                                        DataLoopNode::NodeFluidType::Water,
                                                        DataLoopNode::ConnectionType::Inlet,
                                                        NodeInputManager::CompFluidStream::Primary,
                                                        ObjectIsNotParent);
                state.dataPlantLoadProfile->PlantProfile(ProfileNum).OutletNode =
                    NodeInputManager::GetOnlySingleNode(state,
                                                        state.dataIPShortCut->cAlphaArgs(3),
                                                        ErrorsFound,
                                                        DataLoopNode::ConnectionObjectType::LoadProfilePlant,
                                                        state.dataIPShortCut->cAlphaArgs(1),
                                                        DataLoopNode::NodeFluidType::Water,
                                                        DataLoopNode::ConnectionType::Outlet,
                                                        NodeInputManager::CompFluidStream::Primary,
                                                        ObjectIsNotParent);
            } else { // state.dataPlantLoadProfile->PlantProfile(ProfileNum).FluidType == PlantLoopFluidType::Steam
                state.dataPlantLoadProfile->PlantProfile(ProfileNum).InletNode =
                    NodeInputManager::GetOnlySingleNode(state,
                                                        state.dataIPShortCut->cAlphaArgs(2),
                                                        ErrorsFound,
                                                        DataLoopNode::ConnectionObjectType::LoadProfilePlant,
                                                        state.dataIPShortCut->cAlphaArgs(1),
                                                        DataLoopNode::NodeFluidType::Steam,
                                                        DataLoopNode::ConnectionType::Inlet,
                                                        NodeInputManager::CompFluidStream::Primary,
                                                        ObjectIsNotParent);
                state.dataPlantLoadProfile->PlantProfile(ProfileNum).OutletNode =
                    NodeInputManager::GetOnlySingleNode(state,
                                                        state.dataIPShortCut->cAlphaArgs(3),
                                                        ErrorsFound,
                                                        DataLoopNode::ConnectionObjectType::LoadProfilePlant,
                                                        state.dataIPShortCut->cAlphaArgs(1),
                                                        DataLoopNode::NodeFluidType::Steam,
                                                        DataLoopNode::ConnectionType::Outlet,
                                                        NodeInputManager::CompFluidStream::Primary,
                                                        ObjectIsNotParent);
            }

            state.dataPlantLoadProfile->PlantProfile(ProfileNum).LoadSchedule =
                ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(4));

            if (state.dataPlantLoadProfile->PlantProfile(ProfileNum).LoadSchedule == 0) {
                ShowSevereError(state,
                                format("{}=\"{}\"  The Schedule for {} called {} was not found.",
                                       cCurrentModuleObject,
                                       state.dataIPShortCut->cAlphaArgs(1),
                                       state.dataIPShortCut->cAlphaFieldNames(4),
                                       state.dataIPShortCut->cAlphaArgs(4)));
                ErrorsFound = true;
            }

            state.dataPlantLoadProfile->PlantProfile(ProfileNum).PeakVolFlowRate = state.dataIPShortCut->rNumericArgs(1);

            state.dataPlantLoadProfile->PlantProfile(ProfileNum).FlowRateFracSchedule =
                ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(5));

            if (state.dataPlantLoadProfile->PlantProfile(ProfileNum).FlowRateFracSchedule == 0) {
                ShowSevereError(state,
                                format("{}=\"{}\"  The Schedule for {} called {} was not found.",
                                       cCurrentModuleObject,
                                       state.dataIPShortCut->cAlphaArgs(1),
                                       state.dataIPShortCut->cAlphaFieldNames(5),
                                       state.dataIPShortCut->cAlphaArgs(5)));

                ErrorsFound = true;
            }

            if (state.dataPlantLoadProfile->PlantProfile(ProfileNum).FluidType == PlantLoopFluidType::Steam) {
                if (!state.dataIPShortCut->lNumericFieldBlanks(2)) {
                    state.dataPlantLoadProfile->PlantProfile(ProfileNum).DegOfSubcooling = state.dataIPShortCut->rNumericArgs(2);
                } else {
                    state.dataPlantLoadProfile->PlantProfile(ProfileNum).DegOfSubcooling = 5.0; // default value
                }

                if (!state.dataIPShortCut->lNumericFieldBlanks(3)) {
                    state.dataPlantLoadProfile->PlantProfile(ProfileNum).LoopSubcoolReturn = state.dataIPShortCut->rNumericArgs(3);
                } else {
                    state.dataPlantLoadProfile->PlantProfile(ProfileNum).LoopSubcoolReturn = 20.0; // default value
                }
            }

            // Check plant connections
            BranchNodeConnections::TestCompSet(state,
                                               cCurrentModuleObject,
                                               state.dataIPShortCut->cAlphaArgs(1),
                                               state.dataIPShortCut->cAlphaArgs(2),
                                               state.dataIPShortCut->cAlphaArgs(3),
                                               cCurrentModuleObject + " Nodes");

            // Setup report variables
            SetupOutputVariable(state,
                                "Plant Load Profile Mass Flow Rate",
                                Constant::Units::kg_s,
                                state.dataPlantLoadProfile->PlantProfile(ProfileNum).MassFlowRate,
                                OutputProcessor::TimeStepType::System,
                                OutputProcessor::StoreType::Average,
                                state.dataPlantLoadProfile->PlantProfile(ProfileNum).Name);

            SetupOutputVariable(state,
                                "Plant Load Profile Heat Transfer Rate",
                                Constant::Units::W,
                                state.dataPlantLoadProfile->PlantProfile(ProfileNum).Power,
                                OutputProcessor::TimeStepType::System,
                                OutputProcessor::StoreType::Average,
                                state.dataPlantLoadProfile->PlantProfile(ProfileNum).Name);

            SetupOutputVariable(state,
                                "Plant Load Profile Heat Transfer Energy",
                                Constant::Units::J,
                                state.dataPlantLoadProfile->PlantProfile(ProfileNum).Energy,
                                OutputProcessor::TimeStepType::System,
                                OutputProcessor::StoreType::Sum,
                                state.dataPlantLoadProfile->PlantProfile(ProfileNum).Name,
                                Constant::eResource::EnergyTransfer,
                                OutputProcessor::Group::Plant,
                                OutputProcessor::EndUseCat::Heating); // is EndUseKey right?

            SetupOutputVariable(state,
                                "Plant Load Profile Heating Energy",
                                Constant::Units::J,
                                state.dataPlantLoadProfile->PlantProfile(ProfileNum).HeatingEnergy,
                                OutputProcessor::TimeStepType::System,
                                OutputProcessor::StoreType::Sum,
                                state.dataPlantLoadProfile->PlantProfile(ProfileNum).Name,
                                Constant::eResource::PlantLoopHeatingDemand,
                                OutputProcessor::Group::Plant,
                                OutputProcessor::EndUseCat::Heating);

            SetupOutputVariable(state,
                                "Plant Load Profile Cooling Energy",
                                Constant::Units::J,
                                state.dataPlantLoadProfile->PlantProfile(ProfileNum).CoolingEnergy,
                                OutputProcessor::TimeStepType::System,
                                OutputProcessor::StoreType::Sum,
                                state.dataPlantLoadProfile->PlantProfile(ProfileNum).Name,
                                Constant::eResource::PlantLoopCoolingDemand,
                                OutputProcessor::Group::Plant,
                                OutputProcessor::EndUseCat::Cooling);

            if (state.dataGlobal->AnyEnergyManagementSystemInModel) {
                SetupEMSActuator(state,
                                 "Plant Load Profile",
                                 state.dataPlantLoadProfile->PlantProfile(ProfileNum).Name,
                                 "Mass Flow Rate",
                                 "[kg/s]",
                                 state.dataPlantLoadProfile->PlantProfile(ProfileNum).EMSOverrideMassFlow,
                                 state.dataPlantLoadProfile->PlantProfile(ProfileNum).EMSMassFlowValue);
                SetupEMSActuator(state,
                                 "Plant Load Profile",
                                 state.dataPlantLoadProfile->PlantProfile(ProfileNum).Name,
                                 "Power",
                                 "[W]",
                                 state.dataPlantLoadProfile->PlantProfile(ProfileNum).EMSOverridePower,
                                 state.dataPlantLoadProfile->PlantProfile(ProfileNum).EMSPowerValue);
            }

            if (state.dataPlantLoadProfile->PlantProfile(ProfileNum).FluidType == PlantLoopFluidType::Steam) {
                SetupOutputVariable(state,
                                    "Plant Load Profile Steam Outlet Temperature",
                                    Constant::Units::C,
                                    state.dataPlantLoadProfile->PlantProfile(ProfileNum).OutletTemp,
                                    OutputProcessor::TimeStepType::System,
                                    OutputProcessor::StoreType::Average,
                                    state.dataPlantLoadProfile->PlantProfile(ProfileNum).Name);
            }

            if (ErrorsFound) ShowFatalError(state, format("Errors in {} input.", cCurrentModuleObject));

        } // ProfileNum
    }
}

} // namespace EnergyPlus::PlantLoadProfile
