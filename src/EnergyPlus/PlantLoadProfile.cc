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

// EnergyPlus Headers
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
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

// Using/Aliasing
using DataPlant::TypeOf_PlantLoadProfile;
using PlantUtilities::InitComponentNodes;
using PlantUtilities::ScanPlantLoopsForObject;
using PlantUtilities::SetComponentFlowRate;

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
    ShowFatalError(state, "PlantLoadProfile::factory: Error getting inputs for pipe named: " + objectName);
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
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Simulates the plant load profile object.

    // METHODOLOGY EMPLOYED:
    // This is a very simple simulation.  InitPlantProfile does the work of getting the scheduled load and flow rate.
    // Flow is requested and the actual available flow is set.  The outlet temperature is calculated.

    // Using/Aliasing
    using FluidProperties::GetSpecificHeatGlycol;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    static std::string const RoutineName("SimulatePlantProfile");
    Real64 DeltaTemp;

    this->InitPlantProfile(state);

    if (this->MassFlowRate > 0.0) {
        Real64 Cp = GetSpecificHeatGlycol(state,
                                          state.dataPlnt->PlantLoop(this->WLoopNum).FluidName,
                                          this->InletTemp,
                                          state.dataPlnt->PlantLoop(this->WLoopNum).FluidIndex,
                                          RoutineName);
        DeltaTemp = this->Power / (this->MassFlowRate * Cp);
    } else {
        this->Power = 0.0;
        DeltaTemp = 0.0;
    }

    this->OutletTemp = this->InletTemp - DeltaTemp;

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

    // Using/Aliasing
    using FluidProperties::GetDensityGlycol;
    using PlantUtilities::RegisterPlantCompDesignFlow;
    using ScheduleManager::GetCurrentScheduleValue;
    using ScheduleManager::GetScheduleMaxValue;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    static std::string const RoutineName("InitPlantProfile");
    Real64 FluidDensityInit;
    bool errFlag;

    // Do the one time initializations
    if (this->SetLoopIndexFlag) {
        if (allocated(state.dataPlnt->PlantLoop)) {
            errFlag = false;
            ScanPlantLoopsForObject(state,
                                    this->Name,
                                    this->TypeNum,
                                    this->WLoopNum,
                                    this->WLoopSideNum,
                                    this->WLoopBranchNum,
                                    this->WLoopCompNum,
                                    errFlag,
                                    _,
                                    _,
                                    _,
                                    _,
                                    _);
            if (errFlag) {
                ShowFatalError(state, "InitPlantProfile: Program terminated for previous conditions.");
            }

            this->SetLoopIndexFlag = false;
        }
    }

    if (!state.dataGlobal->SysSizingCalc && this->InitSizing) {
        RegisterPlantCompDesignFlow(state, InletNode, this->PeakVolFlowRate);
        this->InitSizing = false;
    }

    if (state.dataGlobal->BeginEnvrnFlag && this->Init) {
        // Clear node initial conditions
        state.dataLoopNodes->Node(OutletNode).Temp = 0.0;

        FluidDensityInit = GetDensityGlycol(state,
                                            state.dataPlnt->PlantLoop(this->WLoopNum).FluidName,
                                            DataGlobalConstants::InitConvTemp,
                                            state.dataPlnt->PlantLoop(this->WLoopNum).FluidIndex,
                                            RoutineName);

        Real64 MaxFlowMultiplier = GetScheduleMaxValue(state, this->FlowRateFracSchedule);

        InitComponentNodes(state,
                           0.0,
                           this->PeakVolFlowRate * FluidDensityInit * MaxFlowMultiplier,
                           this->InletNode,
                           this->OutletNode,
                           this->WLoopNum,
                           this->WLoopSideNum,
                           this->WLoopBranchNum,
                           this->WLoopCompNum);

        this->EMSOverrideMassFlow = false;
        this->EMSMassFlowValue = 0.0;
        this->EMSOverridePower = false;
        this->EMSPowerValue = 0.0;
        this->Init = false;
    }

    if (!state.dataGlobal->BeginEnvrnFlag) this->Init = true;

    this->InletTemp = state.dataLoopNodes->Node(InletNode).Temp;
    this->Power = GetCurrentScheduleValue(state, this->LoadSchedule);

    if (this->EMSOverridePower) this->Power = this->EMSPowerValue;

    FluidDensityInit = GetDensityGlycol(state,
                                        state.dataPlnt->PlantLoop(this->WLoopNum).FluidName,
                                        this->InletTemp,
                                        state.dataPlnt->PlantLoop(this->WLoopNum).FluidIndex,
                                        RoutineName);

    // Get the scheduled mass flow rate
    this->VolFlowRate = this->PeakVolFlowRate * GetCurrentScheduleValue(state, this->FlowRateFracSchedule);

    this->MassFlowRate = this->VolFlowRate * FluidDensityInit;

    if (this->EMSOverrideMassFlow) this->MassFlowRate = this->EMSMassFlowValue;

    // Request the mass flow rate from the plant component flow utility routine
    SetComponentFlowRate(
        state, this->MassFlowRate, InletNode, OutletNode, this->WLoopNum, this->WLoopSideNum, this->WLoopBranchNum, this->WLoopCompNum);

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
    auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;

    this->Energy = this->Power * TimeStepSys * DataGlobalConstants::SecInHour;

    if (this->Energy >= 0.0) {
        this->HeatingEnergy = this->Energy;
        this->CoolingEnergy = 0.0;
    } else {
        this->HeatingEnergy = 0.0;
        this->CoolingEnergy = std::abs(this->Energy);
    }
}

// Functions
void GetPlantProfileInput(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Peter Graham Ellis
    //       DATE WRITTEN   January 2004
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Gets the plant load profile input from the input file and sets up the objects.

    // Using/Aliasing
    using BranchNodeConnections::TestCompSet;
    using NodeInputManager::GetOnlySingleNode;
    using ScheduleManager::GetScheduleIndex;
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
            UtilityRoutines::IsNameEmpty(state, state.dataIPShortCut->cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

            state.dataPlantLoadProfile->PlantProfile(ProfileNum).Name = state.dataIPShortCut->cAlphaArgs(1);
            state.dataPlantLoadProfile->PlantProfile(ProfileNum).TypeNum = TypeOf_PlantLoadProfile; // parameter assigned in DataPlant

            state.dataPlantLoadProfile->PlantProfile(ProfileNum).InletNode = GetOnlySingleNode(state,
                                                                                               state.dataIPShortCut->cAlphaArgs(2),
                                                                                               ErrorsFound,
                                                                                               cCurrentModuleObject,
                                                                                               state.dataIPShortCut->cAlphaArgs(1),
                                                                                               DataLoopNode::NodeFluidType::Water,
                                                                                               DataLoopNode::NodeConnectionType::Inlet,
                                                                                               NodeInputManager::compFluidStream::Primary,
                                                                                               ObjectIsNotParent);
            state.dataPlantLoadProfile->PlantProfile(ProfileNum).OutletNode = GetOnlySingleNode(state,
                                                                                                state.dataIPShortCut->cAlphaArgs(3),
                                                                                                ErrorsFound,
                                                                                                cCurrentModuleObject,
                                                                                                state.dataIPShortCut->cAlphaArgs(1),
                                                                                                DataLoopNode::NodeFluidType::Water,
                                                                                                DataLoopNode::NodeConnectionType::Outlet,
                                                                                                NodeInputManager::compFluidStream::Primary,
                                                                                                ObjectIsNotParent);

            state.dataPlantLoadProfile->PlantProfile(ProfileNum).LoadSchedule = GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(4));

            if (state.dataPlantLoadProfile->PlantProfile(ProfileNum).LoadSchedule == 0) {
                ShowSevereError(state,
                                cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\"  The Schedule for " +
                                    state.dataIPShortCut->cAlphaFieldNames(4) + " called " + state.dataIPShortCut->cAlphaArgs(4) + " was not found.");
                ErrorsFound = true;
            }

            state.dataPlantLoadProfile->PlantProfile(ProfileNum).PeakVolFlowRate = state.dataIPShortCut->rNumericArgs(1);

            state.dataPlantLoadProfile->PlantProfile(ProfileNum).FlowRateFracSchedule = GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(5));

            if (state.dataPlantLoadProfile->PlantProfile(ProfileNum).FlowRateFracSchedule == 0) {
                ShowSevereError(state,
                                cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\"  The Schedule for " +
                                    state.dataIPShortCut->cAlphaFieldNames(5) + " called " + state.dataIPShortCut->cAlphaArgs(5) + " was not found.");

                ErrorsFound = true;
            }

            // Check plant connections
            TestCompSet(state,
                        cCurrentModuleObject,
                        state.dataIPShortCut->cAlphaArgs(1),
                        state.dataIPShortCut->cAlphaArgs(2),
                        state.dataIPShortCut->cAlphaArgs(3),
                        cCurrentModuleObject + " Nodes");

            // Setup report variables
            SetupOutputVariable(state,
                                "Plant Load Profile Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                state.dataPlantLoadProfile->PlantProfile(ProfileNum).MassFlowRate,
                                "System",
                                "Average",
                                state.dataPlantLoadProfile->PlantProfile(ProfileNum).Name);

            SetupOutputVariable(state,
                                "Plant Load Profile Heat Transfer Rate",
                                OutputProcessor::Unit::W,
                                state.dataPlantLoadProfile->PlantProfile(ProfileNum).Power,
                                "System",
                                "Average",
                                state.dataPlantLoadProfile->PlantProfile(ProfileNum).Name);

            SetupOutputVariable(state,
                                "Plant Load Profile Heat Transfer Energy",
                                OutputProcessor::Unit::J,
                                state.dataPlantLoadProfile->PlantProfile(ProfileNum).Energy,
                                "System",
                                "Sum",
                                state.dataPlantLoadProfile->PlantProfile(ProfileNum).Name,
                                _,
                                "ENERGYTRANSFER",
                                "Heating",
                                _,
                                "Plant"); // is EndUseKey right?

            SetupOutputVariable(state,
                                "Plant Load Profile Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataPlantLoadProfile->PlantProfile(ProfileNum).HeatingEnergy,
                                "System",
                                "Sum",
                                state.dataPlantLoadProfile->PlantProfile(ProfileNum).Name,
                                _,
                                "PLANTLOOPHEATINGDEMAND",
                                "Heating",
                                _,
                                "Plant");

            SetupOutputVariable(state,
                                "Plant Load Profile Cooling Energy",
                                OutputProcessor::Unit::J,
                                state.dataPlantLoadProfile->PlantProfile(ProfileNum).CoolingEnergy,
                                "System",
                                "Sum",
                                state.dataPlantLoadProfile->PlantProfile(ProfileNum).Name,
                                _,
                                "PLANTLOOPCOOLINGDEMAND",
                                "Cooling",
                                _,
                                "Plant");

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

            if (ErrorsFound) ShowFatalError(state, "Errors in " + cCurrentModuleObject + " input.");

        } // ProfileNum
    }
}

} // namespace EnergyPlus::PlantLoadProfile
