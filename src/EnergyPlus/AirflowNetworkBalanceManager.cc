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
#include <algorithm>
#include <cmath>
#include <set>
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Array2D.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EnergyPlus/AirflowNetwork/include/AirflowNetwork/Elements.hpp>
#include <EnergyPlus/AirflowNetwork/include/AirflowNetwork/Solver.hpp>
#include <EnergyPlus/AirflowNetworkBalanceManager.hh>
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/Coils/CoilCoolingDX.hh>
#include <EnergyPlus/Construction.hh>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/DXCoils.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataAirLoop.hh>
#include <EnergyPlus/DataAirSystems.hh>
#include <EnergyPlus/DataBranchNodeConnections.hh>
#include <EnergyPlus/DataContaminantBalance.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalSurface.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataRoomAirModel.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/EMSManager.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/HVACFan.hh>
#include <EnergyPlus/HVACHXAssistedCoolingCoil.hh>
#include <EnergyPlus/HVACStandAloneERV.hh>
#include <EnergyPlus/HeatingCoils.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/MixedAir.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutAirNodeManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/RoomAirModelManager.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SingleDuct.hh>
#include <EnergyPlus/SplitterComponent.hh>
#include <EnergyPlus/ThermalComfort.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/WaterThermalTanks.hh>
#include <EnergyPlus/ZoneDehumidifier.hh>

namespace EnergyPlus {

namespace AirflowNetworkBalanceManager {

    // MODULE INFORMATION:
    //       AUTHOR         Lixing Gu, Don Shirey, and Muthusamy V. Swami
    //       DATE WRITTEN   July 28, 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // This module is used to simulate performance of air distribution system with a single HVAC system and a constant
    // volume supply fan.

    // Using/Aliasing
    using CurveManager::CurveValue;
    using CurveManager::GetCurveIndex;
    using DataEnvironment::OutDryBulbTempAt;
    using DataHVACGlobals::ContFanCycCoil;
    using DataHVACGlobals::CycFanCycCoil;
    using DataHVACGlobals::FanType_SimpleConstVolume;
    using DataHVACGlobals::FanType_SimpleOnOff;
    using DataHVACGlobals::FanType_SimpleVAV;
    using DataHVACGlobals::FanType_ZoneExhaust;
    using DataSurfaces::cExtBoundCondition;
    using DataSurfaces::ExternalEnvironment;
    using DataSurfaces::OtherSideCoefNoCalcExt;
    using DataSurfaces::SurfaceClass;
    using Fans::GetFanIndex;
    using Fans::GetFanInletNode;
    using Fans::GetFanOutletNode;
    using Fans::GetFanType;
    using Fans::GetFanVolFlow;
    using Psychrometrics::PsyCpAirFnW;
    using Psychrometrics::PsyHFnTdbW;
    using Psychrometrics::PsyRhoAirFnPbTdbW;
    using ScheduleManager::GetCurrentScheduleValue;
    using ScheduleManager::GetScheduleIndex;
    using namespace AirflowNetwork;

    // AirflowNetwork::Solver solver;

    // Functions

    int constexpr NumOfVentCtrTypes(6); // Number of zone level venting control types

    void ManageAirflowNetworkBalance(EnergyPlusData &state,
                                     Optional_bool_const FirstHVACIteration, // True when solution technique on first iteration
                                     Optional_int_const Iter,                // Iteration number
                                     Optional_bool ResimulateAirZone         // True when solution technique on third iteration
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   July 28, 2005
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine performs simulation of air distribution system.

        // Using/Aliasing
        auto &TurnFansOn = state.dataHVACGlobal->TurnFansOn;
        using DataHVACGlobals::VerySmallMassFlow;

        // Locals
        int i;
        int AFNSupplyFanType = 0;

        if (state.dataAirflowNetworkBalanceManager->AirflowNetworkGetInputFlag) {
            GetAirflowNetworkInput(state);
            state.dataAirflowNetworkBalanceManager->AirflowNetworkGetInputFlag = false;
            return;
        }

        if (present(ResimulateAirZone)) {
            ResimulateAirZone = false;
        }

        if (state.dataAirflowNetwork->SimulateAirflowNetwork < AirflowNetworkControlMultizone) return;

        if (state.dataGlobal->BeginEnvrnFlag) {
            TurnFansOn = false; // The FAN should be off when BeginEnvrnFlag = .True.
        }

        state.dataAirflowNetworkBalanceManager->initialize(state);

        auto &NetworkNumOfNodes = state.dataAFNSolver->NetworkNumOfNodes;
        auto &NetworkNumOfLinks = state.dataAFNSolver->NetworkNumOfLinks;

        NetworkNumOfNodes = state.dataAirflowNetwork->NumOfNodesMultiZone;
        NetworkNumOfLinks = state.dataAirflowNetwork->NumOfLinksMultiZone;

        state.dataAirflowNetwork->AirflowNetworkFanActivated = false;

        if (present(FirstHVACIteration) && state.dataAirflowNetwork->SimulateAirflowNetwork >= AirflowNetworkControlSimpleADS) {
            if (FirstHVACIteration) {
                if (allocated(state.dataAirLoop->AirLoopAFNInfo)) {
                    for (i = 1; i <= state.dataAirflowNetworkBalanceManager->DisSysNumOfCVFs; i++) {
                        state.dataAirLoop->AirLoopAFNInfo(i).AFNLoopHeatingCoilMaxRTF = 0.0;
                        state.dataAirLoop->AirLoopAFNInfo(i).AFNLoopOnOffFanRTF = 0.0;
                        state.dataAirLoop->AirLoopAFNInfo(i).AFNLoopDXCoilRTF = 0.0;
                        state.dataAirLoop->AirLoopAFNInfo(i).LoopOnOffFanPartLoadRatio = 0.0;
                    }
                }
            }
            Real64 FanMassFlowRate = 0.0;
            int FanOperModeCyc = 0;
            AFNSupplyFanType = 0;

            for (i = 1; i <= state.dataAirflowNetworkBalanceManager->DisSysNumOfCVFs; i++) {
                AFNSupplyFanType = state.dataAirflowNetwork->DisSysCompCVFData(i).FanTypeNum;
                FanMassFlowRate =
                    max(FanMassFlowRate, state.dataLoopNodes->Node(state.dataAirflowNetwork->DisSysCompCVFData(i).OutletNode).MassFlowRate);
                // VAV take high priority
                if (state.dataAirflowNetwork->DisSysCompCVFData(i).FanTypeNum == FanType_SimpleVAV) {
                    AFNSupplyFanType = state.dataAirflowNetwork->DisSysCompCVFData(i).FanTypeNum;
                    break;
                }
                if (FanMassFlowRate > VerySmallMassFlow && state.dataAirLoop->AirLoopAFNInfo(i).LoopFanOperationMode == CycFanCycCoil &&
                    state.dataAirLoop->AirLoopAFNInfo(i).LoopSystemOnMassFlowrate > 0.0) {
                    FanOperModeCyc = CycFanCycCoil;
                    AFNSupplyFanType = state.dataAirflowNetwork->DisSysCompCVFData(i).FanTypeNum;
                    if (AFNSupplyFanType == FanType_SimpleOnOff) {
                        break;
                    }
                }
            }
            //            Revised to meet heat exchanger requirement
            if ((FanMassFlowRate > VerySmallMassFlow) && (!FirstHVACIteration)) {
                if (AFNSupplyFanType == FanType_SimpleOnOff && FanOperModeCyc == CycFanCycCoil) {
                    state.dataAirflowNetwork->AirflowNetworkFanActivated = true;
                } else if (AFNSupplyFanType == FanType_SimpleVAV) {
                    if (present(Iter) && Iter > 1) state.dataAirflowNetwork->AirflowNetworkFanActivated = true;
                } else if (state.dataAirflowNetwork->AirflowNetworkUnitarySystem) {
                    if (present(Iter) && Iter > 1) state.dataAirflowNetwork->AirflowNetworkFanActivated = true;
                } else {
                    state.dataAirflowNetwork->AirflowNetworkFanActivated = true;
                }
            }
        }
        if (allocated(state.dataZoneEquip->ZoneEquipConfig) && state.dataHVACGlobal->NumHybridVentSysAvailMgrs > 0 &&
            allocated(state.dataAirSystemsData->PrimaryAirSystems))
            HybridVentilationControl(state);
        if (state.dataAirflowNetworkBalanceManager->VentilationCtrl == 1 && state.dataHVACGlobal->NumHybridVentSysAvailMgrs > 0)
            state.dataAirflowNetwork->AirflowNetworkFanActivated = false;

        if (present(Iter) && present(ResimulateAirZone) && state.dataAirflowNetwork->SimulateAirflowNetwork >= AirflowNetworkControlSimpleADS) {
            if (state.dataAirflowNetwork->AirflowNetworkFanActivated && Iter < 3 && AFNSupplyFanType == FanType_SimpleOnOff) {
                ResimulateAirZone = true;
            }
            if (AFNSupplyFanType == FanType_SimpleVAV) {
                if (!state.dataAirflowNetwork->AirflowNetworkFanActivated && Iter < 3) ResimulateAirZone = true;
            }
            if (state.dataAirflowNetwork->AirflowNetworkUnitarySystem) {
                if (!state.dataAirflowNetwork->AirflowNetworkFanActivated && Iter < 3) ResimulateAirZone = true;
            }
        }
        if (state.dataAirflowNetwork->AirflowNetworkFanActivated &&
            state.dataAirflowNetwork->SimulateAirflowNetwork > AirflowNetworkControlMultizone) {
            NetworkNumOfNodes = state.dataAirflowNetwork->AirflowNetworkNumOfNodes;
            NetworkNumOfLinks = state.dataAirflowNetwork->AirflowNetworkNumOfLinks;
        }

        if (allocated(state.dataZoneEquip->ZoneEquipConfig)) ValidateExhaustFanInput(state);

        // VAV terminal set only
        if (present(FirstHVACIteration) && FirstHVACIteration) state.dataAirflowNetwork->VAVTerminalRatio = 0.0;

        // Set AirLoop Number for fans
        if (FirstHVACIteration && state.dataAirflowNetworkBalanceManager->AssignFanAirLoopNumFlag) {
            AssignFanAirLoopNum(state);
            state.dataAirflowNetworkBalanceManager->AssignFanAirLoopNumFlag = false;
        }

        if (state.dataAirflowNetwork->AirflowNetworkFanActivated &&
            state.dataAirflowNetwork->SimulateAirflowNetwork > AirflowNetworkControlMultizone) {
            if (state.dataAirflowNetworkBalanceManager->ValidateDistributionSystemFlag) {
                ValidateDistributionSystem(state);
                ValidateFanFlowRate(state);
                state.dataAirflowNetworkBalanceManager->ValidateDistributionSystemFlag = false;
            }
        }
        CalcAirflowNetworkAirBalance(state);

        if (state.dataAirflowNetwork->AirflowNetworkFanActivated &&
            state.dataAirflowNetwork->SimulateAirflowNetwork > AirflowNetworkControlMultizone) {

            state.dataAirflowNetworkBalanceManager->LoopOnOffFlag = false;
            for (i = 1; i <= state.dataAirflowNetworkBalanceManager->DisSysNumOfCVFs; i++) {
                if (state.dataAirflowNetwork->DisSysCompCVFData(i).AirLoopNum > 0) {
                    if (state.dataLoopNodes->Node(state.dataAirflowNetwork->DisSysCompCVFData(i).InletNode).MassFlowRate > 0.0) {
                        state.dataAirflowNetworkBalanceManager->LoopOnOffFlag(state.dataAirflowNetwork->DisSysCompCVFData(i).AirLoopNum) = true;
                    }
                }
            }

            CalcAirflowNetworkHeatBalance(state);
            CalcAirflowNetworkMoisBalance(state);
            if (state.dataContaminantBalance->Contaminant.CO2Simulation) CalcAirflowNetworkCO2Balance(state);
            if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) CalcAirflowNetworkGCBalance(state);
        }

        UpdateAirflowNetwork(state, FirstHVACIteration);
    }

    static bool getAirflowElementInput(EnergyPlusData &state)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Jason DeGraw
        //       DATE WRITTEN   Oct. 2018
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine reads airflow element inputs (eventually)

        static constexpr std::string_view RoutineName{"getAirflowElementInput"};
        std::string CurrentModuleObject;
        bool success{true};

        // *** Read AirflowNetwork simulation reference crack conditions
        std::unordered_map<std::string, ReferenceConditions> referenceConditions; // Map for lookups
        ReferenceConditions defaultReferenceConditions("Default");                // Defaulted conditions
        bool conditionsAreDefaulted(true);                                        // Conditions are defaulted?
        CurrentModuleObject = "AirflowNetwork:MultiZone:ReferenceCrackConditions";
        auto instances = state.dataInputProcessing->inputProcessor->epJSON.find(CurrentModuleObject);
        if (instances != state.dataInputProcessing->inputProcessor->epJSON.end()) {
            // globalSolverObject.referenceConditions.clear();
            auto &instancesValue = instances.value();
            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
                auto const &fields = instance.value();
                auto const &thisObjectName = UtilityRoutines::MakeUPPERCase(instance.key());

                Real64 temperature{fields.at("reference_temperature")};
                Real64 pressure(101325.0);
                if (fields.find("reference_barometric_pressure") != fields.end()) { // not required field, has default value
                    pressure = fields.at("reference_barometric_pressure");
                    if (std::abs((pressure - state.dataEnvrn->StdBaroPress) / state.dataEnvrn->StdBaroPress) > 0.1) { // 10% off
                        ShowWarningError(state,
                                         format("{}: {}: Pressure = {:.0R} differs by more than 10% from Standard Barometric Pressure = {:.0R}.",
                                                RoutineName,
                                                CurrentModuleObject,
                                                pressure,
                                                state.dataEnvrn->StdBaroPress));
                        ShowContinueError(state, "...occurs in " + CurrentModuleObject + " = " + thisObjectName);
                    }
                    if (pressure <= 31000.0) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + ": " + CurrentModuleObject + ": " + thisObjectName +
                                            ". Reference Barometric Pressure must be greater than 31000 Pa.");
                        success = false;
                    }
                }
                Real64 humidity{fields.at("reference_humidity_ratio")};
                // globalSolverObject.referenceConditions.emplace_back(thisObjectName, temperature, pressure, humidity);
                referenceConditions.emplace(std::piecewise_construct,
                                            std::forward_as_tuple(thisObjectName),
                                            std::forward_as_tuple(instance.key(), temperature, pressure, humidity));
            }
            // Check that there is more than one
            if (referenceConditions.size() == 1) {
                state.dataInputProcessing->inputProcessor->markObjectAsUsed("AirflowNetwork:MultiZone:ReferenceCrackConditions",
                                                                            referenceConditions.begin()->second.name);
                defaultReferenceConditions = referenceConditions.begin()->second;

            } else {
                conditionsAreDefaulted = false;
            }
        }
        if (!success) {
            return false;
        }

        auto &solver = state.dataAFNSolver->solver;

        // *** Read AirflowNetwork simulation surface crack component
        CurrentModuleObject = "AirflowNetwork:MultiZone:Surface:Crack";
        state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfSurCracks =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject); // Temporary workaround
        instances = state.dataInputProcessing->inputProcessor->epJSON.find(CurrentModuleObject);
        if (instances != state.dataInputProcessing->inputProcessor->epJSON.end()) {
            int i = 1; // Temporary workaround
            state.dataAirflowNetwork->MultizoneSurfaceCrackData.allocate(
                state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfSurCracks); // Temporary workaround
            auto &instancesValue = instances.value();
            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
                auto const &fields = instance.value();
                auto const &thisObjectName = UtilityRoutines::MakeUPPERCase(instance.key());
                state.dataInputProcessing->inputProcessor->markObjectAsUsed(CurrentModuleObject, instance.key()); // Temporary workaround

                Real64 coeff{fields.at("air_mass_flow_coefficient_at_reference_conditions")}; // Required field
                Real64 expnt{0.65};
                if (fields.find("air_mass_flow_exponent") != fields.end()) { // not required field, has default value
                    expnt = fields.at("air_mass_flow_exponent");
                }
                Real64 refT = defaultReferenceConditions.temperature;
                Real64 refP = defaultReferenceConditions.pressure;
                Real64 refW = defaultReferenceConditions.humidityRatio;
                if (!conditionsAreDefaulted) {
                    if (fields.find("reference_crack_conditions") != fields.end()) { // not required field, *should* have default value
                        auto result = referenceConditions.find(fields.at("reference_crack_conditions"));
                        if (result == referenceConditions.end()) {
                            ShowSevereError(state,
                                            std::string{RoutineName} + CurrentModuleObject + ": " + thisObjectName +
                                                ". Cannot find reference crack conditions object \"" +
                                                fields.at("reference_crack_conditions").get<std::string>() + "\".");
                            success = false;
                        } else {
                            refT = result->second.temperature;
                            refP = result->second.pressure;
                            refW = result->second.humidityRatio;
                            state.dataInputProcessing->inputProcessor->markObjectAsUsed("AirflowNetwork:MultiZone:ReferenceCrackConditions",
                                                                                        result->second.name);
                        }
                    }
                }
                // globalSolverObject.cracks[thisObjectName] = SurfaceCrack(coeff, expnt, refT, refP, refW);
                state.dataAirflowNetwork->MultizoneSurfaceCrackData(i).name = thisObjectName; // Name of surface crack component
                state.dataAirflowNetwork->MultizoneSurfaceCrackData(i).FlowCoef = coeff;      // Air Mass Flow Coefficient
                state.dataAirflowNetwork->MultizoneSurfaceCrackData(i).FlowExpo = expnt;      // Air Mass Flow exponent
                state.dataAirflowNetwork->MultizoneSurfaceCrackData(i).StandardT = refT;
                state.dataAirflowNetwork->MultizoneSurfaceCrackData(i).StandardP = refP;
                state.dataAirflowNetwork->MultizoneSurfaceCrackData(i).StandardW = refW;

                // This is the first element that is being added to the lookup table, so no check of naming overlaps
                solver.elements[thisObjectName] = &state.dataAirflowNetwork->MultizoneSurfaceCrackData(i); // Yet another workaround

                ++i;
            }
        }

        // *** Read AirflowNetwork simulation zone exhaust fan component
        CurrentModuleObject = "AirflowNetwork:MultiZone:Component:ZoneExhaustFan";
        state.dataAirflowNetwork->AirflowNetworkNumOfExhFan =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject); // Temporary workaround
        state.dataAirflowNetworkBalanceManager->NumOfExhaustFans =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Fan:ZoneExhaust"); // Temporary workaround
        instances = state.dataInputProcessing->inputProcessor->epJSON.find(CurrentModuleObject);
        if (instances != state.dataInputProcessing->inputProcessor->epJSON.end()) {
            int i = 1; // Temporary workaround
            state.dataAirflowNetwork->MultizoneCompExhaustFanData.allocate(
                state.dataAirflowNetwork->AirflowNetworkNumOfExhFan); // Temporary workaround
            auto &instancesValue = instances.value();
            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
                auto const &fields = instance.value();
                auto const &thisObjectName = UtilityRoutines::MakeUPPERCase(instance.key());
                state.dataInputProcessing->inputProcessor->markObjectAsUsed(CurrentModuleObject, instance.key()); // Temporary workaround

                Real64 coeff{fields.at("air_mass_flow_coefficient_when_the_zone_exhaust_fan_is_off_at_reference_conditions")}; // Required field
                Real64 expnt{0.65};
                if (fields.find("air_mass_flow_exponent_when_the_zone_exhaust_fan_is_off") != fields.end()) { // not required field, has default value
                    expnt = fields.at("air_mass_flow_exponent_when_the_zone_exhaust_fan_is_off");
                }

                // This breaks the component model, need to fix
                bool fanErrorFound = false;
                int fanIndex;
                GetFanIndex(state, thisObjectName, fanIndex, fanErrorFound, ObjexxFCL::Optional_string_const());
                if (fanErrorFound) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + ": " + CurrentModuleObject + " = " + thisObjectName + " is not found in Fan:ZoneExhaust objects.");
                    success = false;
                }
                Real64 flowRate;

                GetFanVolFlow(state, fanIndex, flowRate);
                flowRate *= state.dataEnvrn->StdRhoAir;
                bool nodeErrorsFound{false};
                int inletNode = GetFanInletNode(state, "Fan:ZoneExhaust", thisObjectName, nodeErrorsFound);
                int outletNode = GetFanOutletNode(state, "Fan:ZoneExhaust", thisObjectName, nodeErrorsFound);
                if (nodeErrorsFound) {
                    success = false;
                }
                int fanType_Num;
                GetFanType(state, thisObjectName, fanType_Num, fanErrorFound);
                if (fanType_Num != FanType_ZoneExhaust) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + CurrentModuleObject + " = " + thisObjectName + ". The specified " + "Name" +
                                        " is not found as a valid Fan:ZoneExhaust object.");
                    success = false;
                }

                Real64 refT = defaultReferenceConditions.temperature;
                Real64 refP = defaultReferenceConditions.pressure;
                Real64 refW = defaultReferenceConditions.humidityRatio;
                if (!conditionsAreDefaulted) {
                    if (fields.find("reference_crack_conditions") != fields.end()) { // not required field, *should* have default value
                        auto result = referenceConditions.find(fields.at("reference_crack_conditions"));
                        if (result == referenceConditions.end()) {
                            ShowSevereError(state,
                                            std::string{RoutineName} + CurrentModuleObject + ": " + thisObjectName +
                                                ". Cannot find reference crack conditions object \"" +
                                                fields.at("reference_crack_conditions").get<std::string>() + "\".");
                            success = false;
                        } else {
                            refT = result->second.temperature;
                            refP = result->second.pressure;
                            refW = result->second.humidityRatio;
                            state.dataInputProcessing->inputProcessor->markObjectAsUsed("AirflowNetwork:MultiZone:ReferenceCrackConditions",
                                                                                        result->second.name);
                        }
                    }
                }

                state.dataAirflowNetwork->MultizoneCompExhaustFanData(i).name = thisObjectName; // Name of zone exhaust fan component
                state.dataAirflowNetwork->MultizoneCompExhaustFanData(i).FlowCoef = coeff;      // flow coefficient
                state.dataAirflowNetwork->MultizoneCompExhaustFanData(i).FlowExpo = expnt;      // Flow exponent

                state.dataAirflowNetwork->MultizoneCompExhaustFanData(i).FlowRate = flowRate;
                state.dataAirflowNetwork->MultizoneCompExhaustFanData(i).InletNode = inletNode;
                state.dataAirflowNetwork->MultizoneCompExhaustFanData(i).OutletNode = outletNode;

                state.dataAirflowNetwork->MultizoneCompExhaustFanData(i).StandardT = refT;
                state.dataAirflowNetwork->MultizoneCompExhaustFanData(i).StandardP = refP;
                state.dataAirflowNetwork->MultizoneCompExhaustFanData(i).StandardW = refW;

                // Add the element to the lookup table, check for name overlaps
                if (solver.elements.find(thisObjectName) == solver.elements.end()) {
                    solver.elements[thisObjectName] = &state.dataAirflowNetwork->MultizoneCompExhaustFanData(i); // Yet another workaround
                } else {
                    ShowSevereError(state, std::string{RoutineName} + "Duplicated airflow element names are found = " + thisObjectName);
                    // ShowContinueError(state, "A unique component name is required in both objects " + std::string{CompName}(1) + " and " + std::string{CompName}(2));
                    success = false;
                }

                ++i;
            }
        }

        // Read Outdoor Airflow object
        CurrentModuleObject = "AirflowNetwork:Distribution:Component:OutdoorAirFlow";
        state.dataAirflowNetworkBalanceManager->NumOfOAFans =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject); // Temporary workaround
        instances = state.dataInputProcessing->inputProcessor->epJSON.find(CurrentModuleObject);
        if (instances != state.dataInputProcessing->inputProcessor->epJSON.end()) {
            int i = 1;                                                                                                        // Temporary workaround
            state.dataAirflowNetwork->DisSysCompOutdoorAirData.allocate(state.dataAirflowNetworkBalanceManager->NumOfOAFans); // Temporary workaround
            auto &instancesValue = instances.value();
            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
                auto const &fields = instance.value();
                auto const &thisObjectName = UtilityRoutines::MakeUPPERCase(instance.key());
                state.dataInputProcessing->inputProcessor->markObjectAsUsed(CurrentModuleObject, instance.key()); // Temporary workaround

                std::string mixer_name = UtilityRoutines::MakeUPPERCase(AsString(fields.at("outdoor_air_mixer_name")));
                Real64 coeff{fields.at("air_mass_flow_coefficient_when_no_outdoor_air_flow_at_reference_conditions")};
                Real64 expnt{0.65};
                if (fields.find("air_mass_flow_exponent_when_no_outdoor_air_flow") != fields.end()) {
                    expnt = fields.at("air_mass_flow_exponent_when_no_outdoor_air_flow");
                }

                int OAMixerNum = MixedAir::GetOAMixerNumber(state, mixer_name);
                if (OAMixerNum == 0) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + ": " + CurrentModuleObject + " object " + thisObjectName + ". Invalid " + "Outdoor Air Mixer Name" +
                                        " \"" + mixer_name + "\" given.");
                    success = false;
                }

                Real64 refT = defaultReferenceConditions.temperature;
                Real64 refP = defaultReferenceConditions.pressure;
                Real64 refW = defaultReferenceConditions.humidityRatio;
                if (!conditionsAreDefaulted) {
                    if (fields.find("reference_crack_conditions") != fields.end()) { // not required field, *should* have default value
                        auto result = referenceConditions.find(fields.at("reference_crack_conditions"));
                        if (result == referenceConditions.end()) {
                            ShowSevereError(state,
                                            std::string{RoutineName} + CurrentModuleObject + ": " + thisObjectName +
                                                ". Cannot find reference crack conditions object \"" +
                                                fields.at("reference_crack_conditions").get<std::string>() + "\".");
                            success = false;
                        } else {
                            refT = result->second.temperature;
                            refP = result->second.pressure;
                            refW = result->second.humidityRatio;
                            state.dataInputProcessing->inputProcessor->markObjectAsUsed("AirflowNetwork:MultiZone:ReferenceCrackConditions",
                                                                                        result->second.name);
                        }
                    }
                }

                state.dataAirflowNetwork->DisSysCompOutdoorAirData(i).name = thisObjectName; // Name of zone exhaust fan component
                state.dataAirflowNetwork->DisSysCompOutdoorAirData(i).FlowCoef = coeff;      // flow coefficient
                state.dataAirflowNetwork->DisSysCompOutdoorAirData(i).FlowExpo = expnt;      // Flow exponent

                state.dataAirflowNetwork->DisSysCompOutdoorAirData(i).OAMixerNum = OAMixerNum;

                state.dataAirflowNetwork->DisSysCompOutdoorAirData(i).StandardT = refT;
                state.dataAirflowNetwork->DisSysCompOutdoorAirData(i).StandardP = refP;
                state.dataAirflowNetwork->DisSysCompOutdoorAirData(i).StandardW = refW;

                // Add the element to the lookup table, check for name overlaps
                if (solver.elements.find(thisObjectName) == solver.elements.end()) {
                    solver.elements[thisObjectName] = &state.dataAirflowNetwork->DisSysCompOutdoorAirData(i); // Yet another workaround
                } else {
                    ShowSevereError(state, std::string{RoutineName} + "Duplicated airflow element names are found = " + thisObjectName);
                    // ShowContinueError(state, "A unique component name is required in both objects " + std::string{CompName}(1) + " and " + std::string{CompName}(2));
                    success = false;
                }

                ++i;
            }
        }

        // Read Relief Airflow object
        CurrentModuleObject = "AirflowNetwork:Distribution:Component:ReliefAirFlow";
        state.dataAirflowNetworkBalanceManager->NumOfReliefFans =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject); // Temporary workaround
        instances = state.dataInputProcessing->inputProcessor->epJSON.find(CurrentModuleObject);
        if (instances != state.dataInputProcessing->inputProcessor->epJSON.end()) {
            int i = 1; // Temporary workaround
            state.dataAirflowNetwork->DisSysCompReliefAirData.allocate(
                state.dataAirflowNetworkBalanceManager->NumOfReliefFans); // Temporary workaround
            auto &instancesValue = instances.value();
            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
                auto const &fields = instance.value();
                auto const &thisObjectName = UtilityRoutines::MakeUPPERCase(instance.key());
                state.dataInputProcessing->inputProcessor->markObjectAsUsed(CurrentModuleObject, instance.key()); // Temporary workaround

                std::string mixer_name = UtilityRoutines::MakeUPPERCase(AsString(fields.at("outdoor_air_mixer_name")));
                Real64 coeff{fields.at("air_mass_flow_coefficient_when_no_outdoor_air_flow_at_reference_conditions")};
                Real64 expnt{0.65};
                if (fields.find("air_mass_flow_exponent_when_no_outdoor_air_flow") != fields.end()) {
                    expnt = fields.at("air_mass_flow_exponent_when_no_outdoor_air_flow");
                }

                int OAMixerNum{MixedAir::GetOAMixerNumber(state, mixer_name)};
                if (OAMixerNum == 0) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + ": " + CurrentModuleObject + " object " + thisObjectName + ". Invalid " + "Outdoor Air Mixer Name" +
                                        " \"" + mixer_name + "\" given.");
                    success = false;
                }

                Real64 refT = defaultReferenceConditions.temperature;
                Real64 refP = defaultReferenceConditions.pressure;
                Real64 refW = defaultReferenceConditions.humidityRatio;
                if (!conditionsAreDefaulted) {
                    if (fields.find("reference_crack_conditions") != fields.end()) { // not required field, *should* have default value
                        auto result = referenceConditions.find(fields.at("reference_crack_conditions"));
                        if (result == referenceConditions.end()) {
                            ShowSevereError(state,
                                            std::string{RoutineName} + CurrentModuleObject + ": " + thisObjectName +
                                                ". Cannot find reference crack conditions object \"" +
                                                fields.at("reference_crack_conditions").get<std::string>() + "\".");
                            success = false;
                        } else {
                            refT = result->second.temperature;
                            refP = result->second.pressure;
                            refW = result->second.humidityRatio;
                            state.dataInputProcessing->inputProcessor->markObjectAsUsed("AirflowNetwork:MultiZone:ReferenceCrackConditions",
                                                                                        result->second.name);
                        }
                    }
                }

                state.dataAirflowNetwork->DisSysCompReliefAirData(i).name = thisObjectName; // Name of zone exhaust fan component
                state.dataAirflowNetwork->DisSysCompReliefAirData(i).FlowCoef = coeff;      // flow coefficient
                state.dataAirflowNetwork->DisSysCompReliefAirData(i).FlowExpo = expnt;      // Flow exponent
                state.dataAirflowNetwork->DisSysCompReliefAirData(i).OAMixerNum = OAMixerNum;
                state.dataAirflowNetwork->DisSysCompReliefAirData(i).StandardT = refT;
                state.dataAirflowNetwork->DisSysCompReliefAirData(i).StandardP = refP;
                state.dataAirflowNetwork->DisSysCompReliefAirData(i).StandardW = refW;

                // Add the element to the lookup table, check for name overlaps
                if (solver.elements.find(thisObjectName) == solver.elements.end()) {
                    solver.elements[thisObjectName] = &state.dataAirflowNetwork->DisSysCompReliefAirData(i); // Yet another workaround
                } else {
                    ShowSevereError(state, std::string{RoutineName} + "Duplicated airflow element names are found = " + thisObjectName);
                    // ShowContinueError(state, "A unique component name is required in both objects " + std::string{CompName}(1) + " and " + std::string{CompName}(2));
                    success = false;
                }

                ++i;
            }
        }

        // Read AirflowNetwork simulation detailed openings
        CurrentModuleObject = "AirflowNetwork:MultiZone:Component:DetailedOpening";
        state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfDetOpenings =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject); // Temporary workaround
        instances = state.dataInputProcessing->inputProcessor->epJSON.find(CurrentModuleObject);
        if (instances != state.dataInputProcessing->inputProcessor->epJSON.end()) {
            int i = 1; // Temporary workaround
            state.dataAirflowNetwork->MultizoneCompDetOpeningData.allocate(
                state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfDetOpenings); // Temporary workaround
            auto &instancesValue = instances.value();
            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
                auto const &fields = instance.value();
                auto const &thisObjectName = UtilityRoutines::MakeUPPERCase(instance.key());
                state.dataInputProcessing->inputProcessor->markObjectAsUsed(CurrentModuleObject, instance.key()); // Temporary workaround

                Real64 coeff{fields.at("air_mass_flow_coefficient_when_opening_is_closed")};
                Real64 expnt{0.65};
                if (fields.find("air_mass_flow_exponent_when_opening_is_closed") != fields.end()) {
                    expnt = fields.at("air_mass_flow_exponent_when_opening_is_closed");
                }

                int LVOtype{1};
                std::string LVOstring;
                if (fields.find("type_of_rectangular_large_vertical_opening_lvo_") != fields.end()) {
                    LVOstring = fields.at("type_of_rectangular_large_vertical_opening_lvo_").get<std::string>();
                    if (UtilityRoutines::SameString(LVOstring, "NonPivoted") || UtilityRoutines::SameString(LVOstring, "1")) {
                        LVOtype = 1; // Large vertical opening type number
                    } else if (UtilityRoutines::SameString(LVOstring, "HorizontallyPivoted") || UtilityRoutines::SameString(LVOstring, "2")) {
                        LVOtype = 2; // Large vertical opening type number
                    } else {
                        ShowSevereError(state,
                                        std::string{RoutineName} + "Invalid Type of Rectangular Large Vertical Opening (LVO) = " + LVOstring + "in " +
                                            CurrentModuleObject + " = " + thisObjectName);
                        ShowContinueError(state, "Valid choices are NonPivoted and HorizontallyPivoted.");
                        success = false;
                    }
                }

                Real64 extra{0.0};
                if (fields.find("extra_crack_length_or_height_of_pivoting_axis") != fields.end()) {
                    extra = fields.at("extra_crack_length_or_height_of_pivoting_axis");
                }

                Real64 N{fields.at("number_of_sets_of_opening_factor_data")};

                std::vector<Real64> factors(N);
                std::vector<Real64> cds(N);
                std::vector<Real64> width_factors(N);
                std::vector<Real64> height_factors(N);
                std::vector<Real64> start_height_factors(N);

                // Real64 factor{0.0};
                // if (fields.find("opening_factor_1") != fields.end()) {
                //    factor = fields.at("opening_factor_1");
                //}
                Real64 cd{0.001};
                if (fields.find("discharge_coefficient_for_opening_factor_1") != fields.end()) {
                    cd = fields.at("discharge_coefficient_for_opening_factor_1");
                }
                Real64 width_factor{0.0};
                if (fields.find("width_factor_for_opening_factor_1") != fields.end()) {
                    width_factor = fields.at("width_factor_for_opening_factor_1");
                }
                Real64 height_factor{0.0};
                if (fields.find("height_factor_for_opening_factor_1") != fields.end()) {
                    height_factor = fields.at("height_factor_for_opening_factor_1");
                }
                Real64 start_height_factor{0.0};
                if (fields.find("start_height_factor_for_opening_factor_1") != fields.end()) {
                    start_height_factor = fields.at("start_height_factor_for_opening_factor_1");
                }

                factors[0] = 0.0; // factor; // This factor must be zero
                cds[0] = cd;
                width_factors[0] = width_factor;
                height_factors[0] = height_factor;
                start_height_factors[0] = start_height_factor;

                Real64 factor{fields.at("opening_factor_2")};
                cd = 1.0;
                if (fields.find("discharge_coefficient_for_opening_factor_2") != fields.end()) {
                    cd = fields.at("discharge_coefficient_for_opening_factor_2");
                }
                width_factor = 1.0;
                if (fields.find("width_factor_for_opening_factor_2") != fields.end()) {
                    width_factor = fields.at("width_factor_for_opening_factor_2");
                }
                height_factor = 1.0;
                if (fields.find("height_factor_for_opening_factor_2") != fields.end()) {
                    height_factor = fields.at("height_factor_for_opening_factor_2");
                }
                start_height_factor = 0.0;
                if (fields.find("start_height_factor_for_opening_factor_2") != fields.end()) {
                    start_height_factor = fields.at("start_height_factor_for_opening_factor_2");
                }

                factors[1] = factor;
                cds[1] = cd;
                width_factors[1] = width_factor;
                height_factors[1] = height_factor;
                start_height_factors[1] = start_height_factor;

                if (N >= 3) {
                    factor = fields.at("opening_factor_3");
                    cd = 0.0;
                    if (fields.find("discharge_coefficient_for_opening_factor_3") != fields.end()) {
                        cd = fields.at("discharge_coefficient_for_opening_factor_3");
                    }
                    width_factor = 0.0;
                    if (fields.find("width_factor_for_opening_factor_3") != fields.end()) {
                        width_factor = fields.at("width_factor_for_opening_factor_3");
                    }
                    height_factor = 0.0;
                    if (fields.find("height_factor_for_opening_factor_3") != fields.end()) {
                        height_factor = fields.at("height_factor_for_opening_factor_3");
                    }
                    start_height_factor = 0.0;
                    if (fields.find("start_height_factor_for_opening_factor_3") != fields.end()) {
                        start_height_factor = fields.at("start_height_factor_for_opening_factor_3");
                    }

                    factors[2] = factor;
                    cds[2] = cd;
                    width_factors[2] = width_factor;
                    height_factors[2] = height_factor;
                    start_height_factors[2] = start_height_factor;

                    if (N >= 4) {
                        factor = fields.at("opening_factor_4");
                        cd = 0.0;
                        if (fields.find("discharge_coefficient_for_opening_factor_4") != fields.end()) {
                            cd = fields.at("discharge_coefficient_for_opening_factor_4");
                        }
                        width_factor = 0.0;
                        if (fields.find("width_factor_for_opening_factor_4") != fields.end()) {
                            width_factor = fields.at("width_factor_for_opening_factor_4");
                        }
                        height_factor = 0.0;
                        if (fields.find("height_factor_for_opening_factor_4") != fields.end()) {
                            height_factor = fields.at("height_factor_for_opening_factor_4");
                        }
                        start_height_factor = 0.0;
                        if (fields.find("start_height_factor_for_opening_factor_4") != fields.end()) {
                            start_height_factor = fields.at("start_height_factor_for_opening_factor_4");
                        }

                        factors[3] = factor;
                        cds[3] = cd;
                        width_factors[3] = width_factor;
                        height_factors[3] = height_factor;
                        start_height_factors[3] = start_height_factor;
                    }
                }

                state.dataAirflowNetwork->MultizoneCompDetOpeningData(i).name = thisObjectName; // Name of large detailed opening component
                state.dataAirflowNetwork->MultizoneCompDetOpeningData(i).FlowCoef = coeff; // Air Mass Flow Coefficient When Window or Door Is Closed
                state.dataAirflowNetwork->MultizoneCompDetOpeningData(i).FlowExpo = expnt; // Air Mass Flow exponent When Window or Door Is Closed
                state.dataAirflowNetwork->MultizoneCompDetOpeningData(i).TypeName = LVOstring; // Large vertical opening type
                state.dataAirflowNetwork->MultizoneCompDetOpeningData(i).LVOType = LVOtype;    // Large vertical opening type number
                state.dataAirflowNetwork->MultizoneCompDetOpeningData(i).LVOValue = extra; // Extra crack length for LVO type 1 with multiple openable
                                                                                           // parts, or Height of pivoting axis for LVO type 2

                state.dataAirflowNetwork->MultizoneCompDetOpeningData(i).NumFac = N; // Number of Opening Factor Values

                state.dataAirflowNetwork->MultizoneCompDetOpeningData(i).OpenFac1 = factors[0];        // Opening factor #1
                state.dataAirflowNetwork->MultizoneCompDetOpeningData(i).DischCoeff1 = cds[0];         // Discharge coefficient for opening factor #1
                state.dataAirflowNetwork->MultizoneCompDetOpeningData(i).WidthFac1 = width_factors[0]; // Width factor for for Opening factor #1
                state.dataAirflowNetwork->MultizoneCompDetOpeningData(i).HeightFac1 = height_factors[0]; // Height factor for opening factor #1
                state.dataAirflowNetwork->MultizoneCompDetOpeningData(i).StartHFac1 =
                    start_height_factors[0];                                                           // Start height factor for opening factor #1
                state.dataAirflowNetwork->MultizoneCompDetOpeningData(i).OpenFac2 = factors[1];        // Opening factor #2
                state.dataAirflowNetwork->MultizoneCompDetOpeningData(i).DischCoeff2 = cds[1];         // Discharge coefficient for opening factor #2
                state.dataAirflowNetwork->MultizoneCompDetOpeningData(i).WidthFac2 = width_factors[1]; // Width factor for for Opening factor #2
                state.dataAirflowNetwork->MultizoneCompDetOpeningData(i).HeightFac2 = height_factors[1]; // Height factor for opening factor #2
                state.dataAirflowNetwork->MultizoneCompDetOpeningData(i).StartHFac2 =
                    start_height_factors[1]; // Start height factor for opening factor #2

                state.dataAirflowNetwork->MultizoneCompDetOpeningData(i).OpenFac3 = 0.0;    // Opening factor #3
                state.dataAirflowNetwork->MultizoneCompDetOpeningData(i).DischCoeff3 = 0.0; // Discharge coefficient for opening factor #3
                state.dataAirflowNetwork->MultizoneCompDetOpeningData(i).WidthFac3 = 0.0;   // Width factor for for Opening factor #3
                state.dataAirflowNetwork->MultizoneCompDetOpeningData(i).HeightFac3 = 0.0;  // Height factor for opening factor #3
                state.dataAirflowNetwork->MultizoneCompDetOpeningData(i).StartHFac3 = 0.0;  // Start height factor for opening factor #3
                state.dataAirflowNetwork->MultizoneCompDetOpeningData(i).OpenFac4 = 0.0;    // Opening factor #4
                state.dataAirflowNetwork->MultizoneCompDetOpeningData(i).DischCoeff4 = 0.0; // Discharge coefficient for opening factor #4
                state.dataAirflowNetwork->MultizoneCompDetOpeningData(i).WidthFac4 = 0.0;   // Width factor for for Opening factor #4
                state.dataAirflowNetwork->MultizoneCompDetOpeningData(i).HeightFac4 = 0.0;  // Height factor for opening factor #4
                state.dataAirflowNetwork->MultizoneCompDetOpeningData(i).StartHFac4 = 0.0;  // Start height factor for opening factor #4
                if (N == 2) {
                    if (factors[1] != 1.0) {
                        ShowWarningError(state, std::string{RoutineName} + ": " + CurrentModuleObject + " = " + thisObjectName);
                        ShowContinueError(
                            state,
                            "..This object specifies that only 3 opening factors will be used. So, the value of Opening Factor #2 is set to 1.0.");
                        ShowContinueError(state,
                                          format("..Input value was {:.2R}", state.dataAirflowNetwork->MultizoneCompDetOpeningData(i).OpenFac2));
                        state.dataAirflowNetwork->MultizoneCompDetOpeningData(i).OpenFac2 = 1.0;
                    }
                } else if (N >= 3) {
                    state.dataAirflowNetwork->MultizoneCompDetOpeningData(i).OpenFac3 = factors[2]; // Opening factor #3
                    state.dataAirflowNetwork->MultizoneCompDetOpeningData(i).DischCoeff3 = cds[2];  // Discharge coefficient for opening factor #3
                    state.dataAirflowNetwork->MultizoneCompDetOpeningData(i).WidthFac3 = width_factors[2];   // Width factor for for Opening factor #3
                    state.dataAirflowNetwork->MultizoneCompDetOpeningData(i).HeightFac3 = height_factors[2]; // Height factor for opening factor #3
                    state.dataAirflowNetwork->MultizoneCompDetOpeningData(i).StartHFac3 =
                        start_height_factors[2]; // Start height factor for opening factor #3
                    if (N >= 4) {
                        state.dataAirflowNetwork->MultizoneCompDetOpeningData(i).OpenFac4 = factors[3]; // Opening factor #4
                        if (factors[3] != 1.0) {
                            ShowWarningError(state, std::string{RoutineName} + ": " + CurrentModuleObject + " = " + thisObjectName);
                            ShowContinueError(state,
                                              "..This object specifies that 4 opening factors will be used. So, the value of Opening Factor #4 "
                                              "is set to 1.0.");
                            ShowContinueError(state,
                                              format("..Input value was {:.2R}", state.dataAirflowNetwork->MultizoneCompDetOpeningData(i).OpenFac4));
                            state.dataAirflowNetwork->MultizoneCompDetOpeningData(i).OpenFac4 = 1.0;
                        }
                        state.dataAirflowNetwork->MultizoneCompDetOpeningData(i).DischCoeff4 = cds[3]; // Discharge coefficient for opening factor #4
                        state.dataAirflowNetwork->MultizoneCompDetOpeningData(i).WidthFac4 =
                            width_factors[3]; // Width factor for for Opening factor #4
                        state.dataAirflowNetwork->MultizoneCompDetOpeningData(i).HeightFac4 =
                            height_factors[3]; // Height factor for opening factor #4
                        state.dataAirflowNetwork->MultizoneCompDetOpeningData(i).StartHFac4 =
                            start_height_factors[3]; // Start height factor for opening factor #4
                    } else {
                        if (factors[2] != 1.0) {
                            ShowWarningError(state, std::string{RoutineName} + ": " + CurrentModuleObject + " = " + thisObjectName);
                            ShowContinueError(state,
                                              "..This object specifies that only 3 opening factors will be used. So, the value of Opening Factor #3 "
                                              "is set to 1.0.");
                            ShowContinueError(state,
                                              format("..Input value was {:.2R}", state.dataAirflowNetwork->MultizoneCompDetOpeningData(i).OpenFac3));
                            state.dataAirflowNetwork->MultizoneCompDetOpeningData(i).OpenFac3 = 1.0;
                        }
                    }
                }

                // Sanity checks, check sum of Height Factor and the Start Height Factor
                if (state.dataAirflowNetwork->MultizoneCompDetOpeningData(i).HeightFac1 +
                        state.dataAirflowNetwork->MultizoneCompDetOpeningData(i).StartHFac1 >
                    1.0) {
                    ShowSevereError(state, std::string{RoutineName} + ": " + CurrentModuleObject + " = " + thisObjectName);
                    ShowContinueError(
                        state, "..The sum of Height Factor for Opening Factor 1 and Start Height Factor for Opening Factor 1 is greater than 1.0");
                    success = false;
                }
                if (state.dataAirflowNetwork->MultizoneCompDetOpeningData(i).HeightFac2 +
                        state.dataAirflowNetwork->MultizoneCompDetOpeningData(i).StartHFac2 >
                    1.0) {
                    ShowSevereError(state, std::string{RoutineName} + ": " + CurrentModuleObject + " = " + thisObjectName);
                    ShowContinueError(
                        state, "..The sum of Height Factor for Opening Factor 2 and Start Height Factor for Opening Factor 2 is greater than 1.0");
                    success = false;
                }
                if (state.dataAirflowNetwork->MultizoneCompDetOpeningData(i).NumFac > 2) {
                    if (state.dataAirflowNetwork->MultizoneCompDetOpeningData(i).OpenFac2 >=
                        state.dataAirflowNetwork->MultizoneCompDetOpeningData(i).OpenFac3) {
                        ShowSevereError(state, std::string{RoutineName} + ": " + CurrentModuleObject + " = " + thisObjectName);
                        ShowContinueError(state, "..The value of Opening Factor #2 >= the value of Opening Factor #3");
                        success = false;
                    }
                    if (state.dataAirflowNetwork->MultizoneCompDetOpeningData(i).HeightFac3 +
                            state.dataAirflowNetwork->MultizoneCompDetOpeningData(i).StartHFac3 >
                        1.0) {
                        ShowSevereError(state, std::string{RoutineName} + ": " + CurrentModuleObject + " = " + thisObjectName);
                        ShowContinueError(
                            state,
                            "..The sum of Height Factor for Opening Factor 3 and Start Height Factor for Opening Factor 3 is greater than 1.0");
                        success = false;
                    }
                    if (state.dataAirflowNetwork->MultizoneCompDetOpeningData(i).NumFac == 4) {
                        if (state.dataAirflowNetwork->MultizoneCompDetOpeningData(i).OpenFac3 >=
                            state.dataAirflowNetwork->MultizoneCompDetOpeningData(i).OpenFac4) {
                            ShowSevereError(state, std::string{RoutineName} + ": " + CurrentModuleObject + " = " + thisObjectName);
                            ShowContinueError(state, "..The value of Opening Factor #3 >= the value of Opening Factor #4");
                            success = false;
                        }
                        if (state.dataAirflowNetwork->MultizoneCompDetOpeningData(i).HeightFac4 +
                                state.dataAirflowNetwork->MultizoneCompDetOpeningData(i).StartHFac4 >
                            1.0) {
                            ShowSevereError(state, std::string{RoutineName} + ": " + CurrentModuleObject + " = " + thisObjectName);
                            ShowContinueError(
                                state,
                                "..The sum of Height Factor for Opening Factor 4 and Start Height Factor for Opening Factor 4 is greater than 1.0");
                            success = false;
                        }
                    }
                }

                // Add the element to the lookup table, check for name overlaps
                if (solver.elements.find(thisObjectName) == solver.elements.end()) {
                    solver.elements[thisObjectName] = &state.dataAirflowNetwork->MultizoneCompDetOpeningData(i); // Yet another workaround
                } else {
                    ShowSevereError(state, std::string{RoutineName} + "Duplicated airflow element names are found = " + thisObjectName);
                    // ShowContinueError(state, "A unique component name is required in both objects " + std::string{CompName}(1) + " and " + std::string{CompName}(2));
                    success = false;
                }

                ++i;
            }
        }

        // Read AirflowNetwork simulation simple openings
        CurrentModuleObject = "AirflowNetwork:MultiZone:Component:SimpleOpening";
        state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfSimOpenings =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject); // Temporary workaround
        instances = state.dataInputProcessing->inputProcessor->epJSON.find(CurrentModuleObject);
        if (instances != state.dataInputProcessing->inputProcessor->epJSON.end()) {
            int i = 1; // Temporary workaround
            state.dataAirflowNetwork->MultizoneCompSimpleOpeningData.allocate(
                state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfSimOpenings); // Temporary workaround
            auto &instancesValue = instances.value();
            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
                auto const &fields = instance.value();
                auto const &thisObjectName = UtilityRoutines::MakeUPPERCase(instance.key());
                state.dataInputProcessing->inputProcessor->markObjectAsUsed(CurrentModuleObject, instance.key()); // Temporary workaround

                Real64 coeff{fields.at("air_mass_flow_coefficient_when_opening_is_closed")};
                Real64 expnt{0.65};
                if (fields.find("air_mass_flow_exponent_when_opening_is_closed") != fields.end()) {
                    expnt = fields.at("air_mass_flow_exponent_when_opening_is_closed");
                }
                Real64 diff{fields.at("minimum_density_difference_for_two_way_flow")};
                Real64 dischargeCoeff{fields.at("discharge_coefficient")};

                state.dataAirflowNetwork->MultizoneCompSimpleOpeningData(i).name = thisObjectName; // Name of large simple opening component
                state.dataAirflowNetwork->MultizoneCompSimpleOpeningData(i).FlowCoef =
                    coeff; // Air Mass Flow Coefficient When Window or Door Is Closed
                state.dataAirflowNetwork->MultizoneCompSimpleOpeningData(i).FlowExpo = expnt;  // Air Mass Flow exponent When Window or Door Is Closed
                state.dataAirflowNetwork->MultizoneCompSimpleOpeningData(i).MinRhoDiff = diff; // Minimum density difference for two-way flow
                state.dataAirflowNetwork->MultizoneCompSimpleOpeningData(i).DischCoeff = dischargeCoeff; // Discharge coefficient at full opening

                // Add the element to the lookup table, check for name overlaps
                if (solver.elements.find(thisObjectName) == solver.elements.end()) {
                    solver.elements[thisObjectName] = &state.dataAirflowNetwork->MultizoneCompSimpleOpeningData(i); // Yet another workaround
                } else {
                    ShowSevereError(state, std::string{RoutineName} + "Duplicated airflow element names are found = " + thisObjectName);
                    // ShowContinueError(state, "A unique component name is required in both objects " + std::string{CompName}(1) + " and " + std::string{CompName}(2));
                    success = false;
                }

                ++i;
            }
        }

        // Read AirflowNetwork simulation horizontal openings
        CurrentModuleObject = "AirflowNetwork:MultiZone:Component:HorizontalOpening";
        state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfHorOpenings =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject); // Temporary workaround
        instances = state.dataInputProcessing->inputProcessor->epJSON.find(CurrentModuleObject);
        if (instances != state.dataInputProcessing->inputProcessor->epJSON.end()) {
            int i = 1; // Temporary workaround
            state.dataAirflowNetwork->MultizoneCompHorOpeningData.allocate(
                state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfHorOpenings); // Temporary workaround
            auto &instancesValue = instances.value();
            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
                auto const &fields = instance.value();
                auto const &thisObjectName = UtilityRoutines::MakeUPPERCase(instance.key());
                state.dataInputProcessing->inputProcessor->markObjectAsUsed(CurrentModuleObject, instance.key()); // Temporary workaround

                Real64 coeff{fields.at("air_mass_flow_coefficient_when_opening_is_closed")};
                Real64 expnt{0.65};
                if (fields.find("air_mass_flow_exponent_when_opening_is_closed") != fields.end()) {
                    expnt = fields.at("air_mass_flow_exponent_when_opening_is_closed");
                }
                Real64 angle{90.0};
                if (fields.find("sloping_plane_angle") != fields.end()) {
                    angle = fields.at("sloping_plane_angle");
                }
                Real64 dischargeCoeff{fields.at("discharge_coefficient")};

                state.dataAirflowNetwork->MultizoneCompHorOpeningData(i).name = thisObjectName; // Name of large simple opening component
                state.dataAirflowNetwork->MultizoneCompHorOpeningData(i).FlowCoef = coeff; // Air Mass Flow Coefficient When Window or Door Is Closed
                state.dataAirflowNetwork->MultizoneCompHorOpeningData(i).FlowExpo = expnt; // Air Mass Flow exponent When Window or Door Is Closed
                state.dataAirflowNetwork->MultizoneCompHorOpeningData(i).Slope = angle;    // Sloping plane angle
                state.dataAirflowNetwork->MultizoneCompHorOpeningData(i).DischCoeff = dischargeCoeff; // Discharge coefficient at full opening

                // Add the element to the lookup table, check for name overlaps
                if (solver.elements.find(thisObjectName) == solver.elements.end()) {
                    solver.elements[thisObjectName] = &state.dataAirflowNetwork->MultizoneCompHorOpeningData(i); // Yet another workaround
                } else {
                    ShowSevereError(state, std::string{RoutineName} + "Duplicated airflow element names are found = " + thisObjectName);
                    // ShowContinueError(state, "A unique component name is required in both objects " + std::string{CompName}(1) + " and " + std::string{CompName}(2));
                    success = false;
                }

                ++i;
            }
        }

        // *** Read AirflowNetwork simulation surface effective leakage area component
        CurrentModuleObject = "AirflowNetwork:MultiZone:Surface:EffectiveLeakageArea";
        state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfSurELA =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject); // Temporary workaround
        instances = state.dataInputProcessing->inputProcessor->epJSON.find(CurrentModuleObject);
        if (instances != state.dataInputProcessing->inputProcessor->epJSON.end()) {
            int i = 1; // Temporary workaround
            state.dataAirflowNetwork->MultizoneSurfaceELAData.allocate(
                state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfSurELA); // Temporary workaround
            auto &instancesValue = instances.value();
            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
                auto const &fields = instance.value();
                auto const &thisObjectName = UtilityRoutines::MakeUPPERCase(instance.key());
                state.dataInputProcessing->inputProcessor->markObjectAsUsed(CurrentModuleObject, instance.key()); // Temporary workaround

                Real64 ela{fields.at("effective_leakage_area")};
                Real64 cd{1.0};
                if (fields.find("discharge_coefficient") != fields.end()) {
                    cd = fields.at("discharge_coefficient");
                }
                Real64 dp{4.0};
                if (fields.find("reference_pressure_difference") != fields.end()) {
                    dp = fields.at("reference_pressure_difference");
                }
                Real64 expnt{0.65};
                if (fields.find("air_mass_flow_exponent") != fields.end()) {
                    expnt = fields.at("air_mass_flow_exponent");
                }

                state.dataAirflowNetwork->MultizoneSurfaceELAData(i).name = thisObjectName; // Name of surface effective leakage area component
                state.dataAirflowNetwork->MultizoneSurfaceELAData(i).ELA = ela;             // Effective leakage area
                state.dataAirflowNetwork->MultizoneSurfaceELAData(i).DischCoeff = cd;       // Discharge coefficient
                state.dataAirflowNetwork->MultizoneSurfaceELAData(i).RefDeltaP = dp;        // Reference pressure difference
                state.dataAirflowNetwork->MultizoneSurfaceELAData(i).FlowExpo = expnt;      // Air Mass Flow exponent
                state.dataAirflowNetwork->MultizoneSurfaceELAData(i).TestDeltaP = 0.0;      // Testing pressure difference
                state.dataAirflowNetwork->MultizoneSurfaceELAData(i).TestDisCoef = 0.0;     // Testing Discharge coefficient

                // Add the element to the lookup table, check for name overlaps
                if (solver.elements.find(thisObjectName) == solver.elements.end()) {
                    solver.elements[thisObjectName] = &state.dataAirflowNetwork->MultizoneSurfaceELAData(i); // Yet another workaround
                } else {
                    ShowSevereError(state, std::string{RoutineName} + "Duplicated airflow element names are found = " + thisObjectName);
                    // ShowContinueError(state, "A unique component name is required in both objects " + std::string{CompName}(1) + " and " + std::string{CompName}(2));
                    success = false;
                }

                ++i;
            }
        }

        // Read AirflowNetwork Distribution system component: duct leakage
        CurrentModuleObject = "AirflowNetwork:Distribution:Component:Leak";
        state.dataAirflowNetworkBalanceManager->DisSysNumOfLeaks =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject); // Temporary workaround
        instances = state.dataInputProcessing->inputProcessor->epJSON.find(CurrentModuleObject);
        if (instances != state.dataInputProcessing->inputProcessor->epJSON.end()) {
            int i = 1;                                                                                                       // Temporary workaround
            state.dataAirflowNetwork->DisSysCompLeakData.allocate(state.dataAirflowNetworkBalanceManager->DisSysNumOfLeaks); // Temporary workaround
            auto &instancesValue = instances.value();
            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
                auto const &fields = instance.value();
                auto const &thisObjectName = UtilityRoutines::MakeUPPERCase(instance.key());
                state.dataInputProcessing->inputProcessor->markObjectAsUsed(CurrentModuleObject, instance.key()); // Temporary workaround

                Real64 coeff{fields.at("air_mass_flow_coefficient")};
                Real64 expnt{0.65};
                if (fields.find("air_mass_flow_exponent") != fields.end()) {
                    expnt = fields.at("air_mass_flow_exponent");
                }

                state.dataAirflowNetwork->DisSysCompLeakData(i).name = thisObjectName; // Name of duct leak component
                state.dataAirflowNetwork->DisSysCompLeakData(i).FlowCoef = coeff;      // Air Mass Flow Coefficient
                state.dataAirflowNetwork->DisSysCompLeakData(i).FlowExpo = expnt;      // Air Mass Flow exponent

                // Add the element to the lookup table, check for name overlaps
                if (solver.elements.find(thisObjectName) == solver.elements.end()) {
                    solver.elements[thisObjectName] = &state.dataAirflowNetwork->DisSysCompLeakData(i); // Yet another workaround
                } else {
                    ShowSevereError(state, std::string{RoutineName} + "Duplicated airflow element names are found = " + thisObjectName);
                    // ShowContinueError(state, "A unique component name is required in both objects " + std::string{CompName}(1) + " and " + std::string{CompName}(2));
                    success = false;
                }

                ++i;
            }
        }

        // Read AirflowNetwork Distribution system component: duct effective leakage ratio
        CurrentModuleObject = "AirflowNetwork:Distribution:Component:LeakageRatio";
        state.dataAirflowNetworkBalanceManager->DisSysNumOfELRs =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject); // Temporary workaround
        instances = state.dataInputProcessing->inputProcessor->epJSON.find(CurrentModuleObject);
        if (instances != state.dataInputProcessing->inputProcessor->epJSON.end()) {
            int i = 1;                                                                                                     // Temporary workaround
            state.dataAirflowNetwork->DisSysCompELRData.allocate(state.dataAirflowNetworkBalanceManager->DisSysNumOfELRs); // Temporary workaround
            auto &instancesValue = instances.value();
            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
                auto const &fields = instance.value();
                auto const &thisObjectName = UtilityRoutines::MakeUPPERCase(instance.key());
                state.dataInputProcessing->inputProcessor->markObjectAsUsed(CurrentModuleObject, instance.key()); // Temporary workaround

                Real64 elr{fields.at("effective_leakage_ratio")};
                Real64 maxflow{fields.at("maximum_flow_rate")};
                Real64 dp{fields.at("reference_pressure_difference")};
                Real64 expnt{0.65};
                if (fields.find("air_mass_flow_exponent") != fields.end()) {
                    expnt = fields.at("air_mass_flow_exponent");
                }

                state.dataAirflowNetwork->DisSysCompELRData(i).name = thisObjectName; // Name of duct effective leakage ratio component
                state.dataAirflowNetwork->DisSysCompELRData(i).ELR = elr;             // Value of effective leakage ratio
                state.dataAirflowNetwork->DisSysCompELRData(i).FlowRate = maxflow * state.dataEnvrn->StdRhoAir; // Maximum airflow rate
                state.dataAirflowNetwork->DisSysCompELRData(i).RefPres = dp;                                    // Reference pressure difference
                state.dataAirflowNetwork->DisSysCompELRData(i).FlowExpo = expnt;                                // Air Mass Flow exponent

                // Add the element to the lookup table, check for name overlaps
                if (solver.elements.find(thisObjectName) == solver.elements.end()) {
                    solver.elements[thisObjectName] = &state.dataAirflowNetwork->DisSysCompELRData(i); // Yet another workaround
                } else {
                    ShowSevereError(state, std::string{RoutineName} + "Duplicated airflow element names are found = " + thisObjectName);
                    // ShowContinueError(state, "A unique component name is required in both objects " + std::string{CompName}(1) + " and " + std::string{CompName}(2));
                    success = false;
                }

                ++i;
            }
        }

        // Read AirflowNetwork Distribution system component: duct
        CurrentModuleObject = "AirflowNetwork:Distribution:Component:Duct";
        state.dataAirflowNetworkBalanceManager->DisSysNumOfDucts =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject); // Temporary workaround
        instances = state.dataInputProcessing->inputProcessor->epJSON.find(CurrentModuleObject);
        if (instances != state.dataInputProcessing->inputProcessor->epJSON.end()) {
            int i = 1;                                                                                                       // Temporary workaround
            state.dataAirflowNetwork->DisSysCompDuctData.allocate(state.dataAirflowNetworkBalanceManager->DisSysNumOfDucts); // Temporary workaround
            auto &instancesValue = instances.value();
            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
                auto const &fields = instance.value();
                auto const &thisObjectName = UtilityRoutines::MakeUPPERCase(instance.key());
                state.dataInputProcessing->inputProcessor->markObjectAsUsed(CurrentModuleObject, instance.key()); // Temporary workaround

                Real64 L{fields.at("duct_length")};
                Real64 D{fields.at("hydraulic_diameter")};
                Real64 A{fields.at("cross_section_area")};
                Real64 e{0.0009};
                if (fields.find("surface_roughness") != fields.end()) {
                    e = fields.at("surface_roughness");
                }
                Real64 dlc{0.0};
                if (fields.find("coefficient_for_local_dynamic_loss_due_to_fitting") != fields.end()) {
                    dlc = fields.at("coefficient_for_local_dynamic_loss_due_to_fitting");
                }
                Real64 U{0.943};
                if (fields.find("heat_transmittance_coefficient_u_factor_for_duct_wall_construction") != fields.end()) {
                    U = fields.at("heat_transmittance_coefficient_u_factor_for_duct_wall_construction");
                }
                Real64 Um{0.001};
                if (fields.find("overall_moisture_transmittance_coefficient_from_air_to_air") != fields.end()) {
                    Um = fields.at("overall_moisture_transmittance_coefficient_from_air_to_air");
                }
                Real64 hout{0.0};
                if (fields.find("outside_convection_coefficient") != fields.end()) {
                    hout = fields.at("outside_convection_coefficient");
                }
                Real64 hin{0.0};
                if (fields.find("inside_convection_coefficient") != fields.end()) {
                    hin = fields.at("inside_convection_coefficient");
                }

                state.dataAirflowNetwork->DisSysCompDuctData(i).name = thisObjectName;   // Name of duct effective leakage ratio component
                state.dataAirflowNetwork->DisSysCompDuctData(i).L = L;                   // Duct length [m]
                state.dataAirflowNetwork->DisSysCompDuctData(i).hydraulicDiameter = D;   // Hydraulic diameter [m]
                state.dataAirflowNetwork->DisSysCompDuctData(i).A = A;                   // Cross section area [m2]
                state.dataAirflowNetwork->DisSysCompDuctData(i).roughness = e;           // Surface roughness [m]
                state.dataAirflowNetwork->DisSysCompDuctData(i).TurDynCoef = dlc;        // Turbulent dynamic loss coefficient
                state.dataAirflowNetwork->DisSysCompDuctData(i).UThermConduct = U;       // Conduction heat transmittance [W/m2.K]
                state.dataAirflowNetwork->DisSysCompDuctData(i).UMoisture = Um;          // Overall moisture transmittance [kg/m2]
                state.dataAirflowNetwork->DisSysCompDuctData(i).OutsideConvCoeff = hout; // Outside convection coefficient [W/m2.K]
                state.dataAirflowNetwork->DisSysCompDuctData(i).InsideConvCoeff = hin;   // Inside convection coefficient [W/m2.K]
                state.dataAirflowNetwork->DisSysCompDuctData(i).MThermal = 0.0;          // Thermal capacity [J/K]
                state.dataAirflowNetwork->DisSysCompDuctData(i).MMoisture = 0.0;         // Moisture capacity [kg]
                state.dataAirflowNetwork->DisSysCompDuctData(i).LamDynCoef = 64.0;       // Laminar dynamic loss coefficient
                state.dataAirflowNetwork->DisSysCompDuctData(i).LamFriCoef = dlc;        // Laminar friction loss coefficient
                state.dataAirflowNetwork->DisSysCompDuctData(i).InitLamCoef = 128.0;     // Coefficient of linear initialization
                state.dataAirflowNetwork->DisSysCompDuctData(i).RelRough = e / D;        // e/D: relative roughness
                state.dataAirflowNetwork->DisSysCompDuctData(i).RelL = L / D;            // L/D: relative length
                state.dataAirflowNetwork->DisSysCompDuctData(i).A1 =
                    1.14 - 0.868589 * std::log(state.dataAirflowNetwork->DisSysCompDuctData(i).RelRough); // 1.14 - 0.868589*ln(e/D)
                state.dataAirflowNetwork->DisSysCompDuctData(i).g =
                    state.dataAirflowNetwork->DisSysCompDuctData(i).A1; // 1/sqrt(Darcy friction factor)

                // Add the element to the lookup table, check for name overlaps
                if (solver.elements.find(thisObjectName) == solver.elements.end()) {
                    solver.elements[thisObjectName] = &state.dataAirflowNetwork->DisSysCompDuctData(i); // Yet another workaround
                } else {
                    ShowSevereError(state, std::string{RoutineName} + "Duplicated airflow element names are found = " + thisObjectName);
                    // ShowContinueError(state, "A unique component name is required in both objects " + std::string{CompName}(1) + " and " + std::string{CompName}(2));
                    success = false;
                }

                ++i;
            }
        }

        // Read AirflowNetwork Distribution system component: constant volume fan
        CurrentModuleObject = "AirflowNetwork:Distribution:Component:Fan";
        state.dataAirflowNetworkBalanceManager->DisSysNumOfCVFs =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
        if (state.dataAirflowNetworkBalanceManager->DisSysNumOfCVFs > 0 &&
            state.dataAirflowNetworkBalanceManager->DisSysNumOfCVFs !=
                state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "AirLoopHVAC")) {
            ShowSevereError(state,
                            format("The number of entered AirflowNetwork:Distribution:Component:Fan objects is {}",
                                   state.dataAirflowNetworkBalanceManager->DisSysNumOfCVFs));
            ShowSevereError(state,
                            format("The number of entered AirLoopHVAC objects is {}",
                                   state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "AirLoopHVAC")));
            ShowContinueError(state, "Both numbers should be equal. Please check your inputs.");
            success = false;
        }

        instances = state.dataInputProcessing->inputProcessor->epJSON.find(CurrentModuleObject);
        if (instances != state.dataInputProcessing->inputProcessor->epJSON.end()) {
            int i = 1;                                                                                                     // Temporary workaround
            state.dataAirflowNetwork->DisSysCompCVFData.allocate(state.dataAirflowNetworkBalanceManager->DisSysNumOfCVFs); // Temporary workaround
            auto &instancesValue = instances.value();
            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
                auto const &fields = instance.value();
                auto const &thisObjectName = UtilityRoutines::MakeUPPERCase(instance.key());
                state.dataInputProcessing->inputProcessor->markObjectAsUsed(CurrentModuleObject, instance.key()); // Temporary workaround

                std::string fan_name = UtilityRoutines::MakeUPPERCase(AsString(fields.at("fan_name")));
                std::string fan_type = fields.at("supply_fan_object_type");

                bool FanErrorFound = false;
                int fanIndex;
                Real64 flowRate = 0.0;
                int fanType_Num = 0;
                int inletNode;
                int outletNode;

                if (UtilityRoutines::SameString(UtilityRoutines::MakeUPPERCase(fan_type), "FAN:SYSTEMMODEL")) {
                    state.dataHVACFan->fanObjs.emplace_back(new HVACFan::FanSystem(state, fan_name));
                    fanIndex = HVACFan::getFanObjectVectorIndex(state, fan_name);
                    if (fanIndex < 0) {
                        ShowSevereError(state, "...occurs in " + CurrentModuleObject + " = " + state.dataAirflowNetwork->DisSysCompCVFData(i).name);
                        success = false;
                    } else {
                        flowRate = state.dataHVACFan->fanObjs[fanIndex]->designAirVolFlowRate;
                        flowRate *= state.dataEnvrn->StdRhoAir;
                        state.dataAirflowNetwork->DisSysCompCVFData(i).FanModelFlag = true;
                        inletNode = state.dataHVACFan->fanObjs[fanIndex]->inletNodeNum;
                        outletNode = state.dataHVACFan->fanObjs[fanIndex]->outletNodeNum;
                        if (state.dataHVACFan->fanObjs[fanIndex]->speedControl == HVACFan::FanSystem::SpeedControlMethod::Continuous) {
                            fanType_Num = FanType_SimpleVAV;
                            state.dataAirflowNetwork->VAVSystem = true;
                        } else {
                            fanType_Num = FanType_SimpleOnOff;
                        }
                        state.dataAirflowNetworkBalanceManager->SupplyFanType = fanType_Num;
                    }

                } else {

                    GetFanIndex(state, fan_name, fanIndex, FanErrorFound, ObjexxFCL::Optional_string_const());

                    if (FanErrorFound) {
                        ShowSevereError(state, "...occurs in " + CurrentModuleObject + " = " + state.dataAirflowNetwork->DisSysCompCVFData(i).name);
                        success = false;
                    }

                    GetFanVolFlow(state, fanIndex, flowRate);
                    flowRate *= state.dataEnvrn->StdRhoAir;

                    GetFanType(state, fan_name, fanType_Num, FanErrorFound);
                    state.dataAirflowNetworkBalanceManager->SupplyFanType = fanType_Num;
                }

                if (!(fanType_Num == FanType_SimpleConstVolume || fanType_Num == FanType_SimpleOnOff || fanType_Num == FanType_SimpleVAV)) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + "The Supply Fan Object Type in " + CurrentModuleObject + " = " + thisObjectName +
                                        " is not a valid fan type.");
                    ShowContinueError(state, "Valid fan types are  Fan:ConstantVolume, Fan:OnOff, Fan:VariableVolume, or Fan:SystemModel.");
                    success = false;
                } else {
                    if (UtilityRoutines::SameString(fan_type, "Fan:ConstantVolume") && fanType_Num == FanType_SimpleOnOff) {
                        ShowSevereError(state, "The Supply Fan Object Type defined in " + CurrentModuleObject + " is " + fan_type);
                        ShowContinueError(state, "The Supply Fan Object Type defined in an AirLoopHVAC is Fan:OnOff");
                        success = false;
                    }
                    if (UtilityRoutines::SameString(fan_type, "Fan:OnOff") && fanType_Num == FanType_SimpleConstVolume) {
                        ShowSevereError(state, "The Supply Fan Object Type defined in " + CurrentModuleObject + " is " + fan_type);
                        ShowContinueError(state, "The Supply Fan Object Type defined in an AirLoopHVAC is Fan:ConstantVolume");
                        success = false;
                    }
                }
                bool ErrorsFound{false};
                if (fanType_Num == FanType_SimpleConstVolume) {
                    inletNode = GetFanInletNode(state, "Fan:ConstantVolume", fan_name, ErrorsFound);
                    outletNode = GetFanOutletNode(state, "Fan:ConstantVolume", fan_name, ErrorsFound);
                }
                if (fanType_Num == FanType_SimpleOnOff && !state.dataAirflowNetwork->DisSysCompCVFData(i).FanModelFlag) {
                    inletNode = GetFanInletNode(state, "Fan:OnOff", fan_name, ErrorsFound);
                    outletNode = GetFanOutletNode(state, "Fan:OnOff", fan_name, ErrorsFound);
                }
                if (fanType_Num == FanType_SimpleVAV && !state.dataAirflowNetwork->DisSysCompCVFData(i).FanModelFlag) {
                    inletNode = GetFanInletNode(state, "Fan:VariableVolume", fan_name, ErrorsFound);
                    outletNode = GetFanOutletNode(state, "Fan:VariableVolume", fan_name, ErrorsFound);
                    state.dataAirflowNetwork->VAVSystem = true;
                }

                if (ErrorsFound) {
                    success = false;
                }

                state.dataAirflowNetwork->DisSysCompCVFData(i).name = fan_name; // Name of duct effective leakage ratio component
                state.dataAirflowNetwork->DisSysCompCVFData(i).Ctrl = 1.0;      // Control ratio
                state.dataAirflowNetwork->DisSysCompCVFData(i).FanIndex = fanIndex;
                state.dataAirflowNetwork->DisSysCompCVFData(i).FlowRate = flowRate;
                state.dataAirflowNetwork->DisSysCompCVFData(i).FanTypeNum = fanType_Num;
                state.dataAirflowNetwork->DisSysCompCVFData(i).InletNode = inletNode;
                state.dataAirflowNetwork->DisSysCompCVFData(i).OutletNode = outletNode;

                // Add the element to the lookup table, check for name overlaps
                if (solver.elements.find(fan_name) == solver.elements.end()) {
                    solver.elements[fan_name] = &state.dataAirflowNetwork->DisSysCompCVFData(i); // Yet another workaround
                } else {
                    ShowSevereError(state, std::string{RoutineName} + "Duplicated airflow element names are found = " + fan_name);
                    // ShowContinueError(state, "A unique component name is required in both objects " + std::string{CompName}(1) + " and " + std::string{CompName}(2));
                    success = false;
                }

                ++i;
            }
        }

        // Read AirflowNetwork Distribution system component: coil
        CurrentModuleObject = "AirflowNetwork:Distribution:Component:Coil";
        state.dataAirflowNetworkBalanceManager->DisSysNumOfCoils =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject); // Temporary workaround
        instances = state.dataInputProcessing->inputProcessor->epJSON.find(CurrentModuleObject);
        if (instances != state.dataInputProcessing->inputProcessor->epJSON.end()) {
            int i = 1;                                                                                                       // Temporary workaround
            state.dataAirflowNetwork->DisSysCompCoilData.allocate(state.dataAirflowNetworkBalanceManager->DisSysNumOfCoils); // Temporary workaround
            auto &instancesValue = instances.value();
            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
                auto const &fields = instance.value();
                // auto const &thisObjectName = UtilityRoutines::MakeUPPERCase(instance.key());
                state.dataInputProcessing->inputProcessor->markObjectAsUsed(CurrentModuleObject, instance.key()); // Temporary workaround

                std::string coil_name = fields.at("coil_name");
                std::string coil_type = fields.at("coil_object_type");
                Real64 L{fields.at("air_path_length")};
                Real64 D{fields.at("air_path_hydraulic_diameter")};

                state.dataAirflowNetwork->DisSysCompCoilData(i).name =
                    UtilityRoutines::MakeUPPERCase(coil_name);                         // Name of associated EPlus coil component
                state.dataAirflowNetwork->DisSysCompCoilData(i).EPlusType = coil_type; // coil type
                state.dataAirflowNetwork->DisSysCompCoilData(i).L = L;                 // Air path length
                state.dataAirflowNetwork->DisSysCompCoilData(i).hydraulicDiameter = D; // Air path hydraulic diameter

                // Add the element to the lookup table, check for name overlaps
                if (solver.elements.find(state.dataAirflowNetwork->DisSysCompCoilData(i).name) == solver.elements.end()) {
                    solver.elements[state.dataAirflowNetwork->DisSysCompCoilData(i).name] =
                        &state.dataAirflowNetwork->DisSysCompCoilData(i); // Yet another workaround
                } else {
                    ShowSevereError(
                        state, std::string{RoutineName} + "Duplicated airflow element names are found = " + state.dataAirflowNetwork->DisSysCompCoilData(i).name);
                    // ShowContinueError(state, "A unique component name is required in both objects " + std::string{CompName}(1) + " and " + std::string{CompName}(2));
                    success = false;
                }

                ++i;
            }
        }

        // Read AirflowNetwork Distribution system component: heat exchanger
        CurrentModuleObject = "AirflowNetwork:Distribution:Component:HeatExchanger";
        state.dataAirflowNetworkBalanceManager->DisSysNumOfHXs =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject); // Temporary workaround
        instances = state.dataInputProcessing->inputProcessor->epJSON.find(CurrentModuleObject);
        if (instances != state.dataInputProcessing->inputProcessor->epJSON.end()) {
            int i = 1;                                                                                                   // Temporary workaround
            state.dataAirflowNetwork->DisSysCompHXData.allocate(state.dataAirflowNetworkBalanceManager->DisSysNumOfHXs); // Temporary workaround
            auto &instancesValue = instances.value();
            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
                auto const &fields = instance.value();
                // auto const &thisObjectName = UtilityRoutines::MakeUPPERCase(instance.key());
                state.dataInputProcessing->inputProcessor->markObjectAsUsed(CurrentModuleObject, instance.key()); // Temporary workaround

                std::string hx_name = fields.at("heatexchanger_name");
                std::string hx_type = fields.at("heatexchanger_object_type");
                Real64 L{fields.at("air_path_length")};
                Real64 D{fields.at("air_path_hydraulic_diameter")};

                state.dataAirflowNetwork->DisSysCompHXData(i).name =
                    UtilityRoutines::MakeUPPERCase(hx_name);                         // Name of associated EPlus heat exchange component
                state.dataAirflowNetwork->DisSysCompHXData(i).EPlusType = hx_type;   // coil type
                state.dataAirflowNetwork->DisSysCompHXData(i).L = L;                 // Air path length
                state.dataAirflowNetwork->DisSysCompHXData(i).hydraulicDiameter = D; // Air path hydraulic diameter
                state.dataAirflowNetwork->DisSysCompHXData(i).CoilParentExists =
                    HVACHXAssistedCoolingCoil::VerifyHeatExchangerParent(state, hx_type, hx_name);

                // Add the element to the lookup table, check for name overlaps
                if (solver.elements.find(state.dataAirflowNetwork->DisSysCompHXData(i).name) == solver.elements.end()) {
                    solver.elements[state.dataAirflowNetwork->DisSysCompHXData(i).name] =
                        &state.dataAirflowNetwork->DisSysCompHXData(i); // Yet another workaround
                } else {
                    ShowSevereError(
                        state, std::string{RoutineName} + "Duplicated airflow element names are found = " + state.dataAirflowNetwork->DisSysCompHXData(i).name);
                    // ShowContinueError(state, "A unique component name is required in both objects " + std::string{CompName}(1) + " and " + std::string{CompName}(2));
                    success = false;
                }
                ++i;
            }
        }

        // Read AirflowNetwork Distribution system component: terminal unit
        CurrentModuleObject = "AirflowNetwork:Distribution:Component:TerminalUnit";
        state.dataAirflowNetworkBalanceManager->DisSysNumOfTermUnits =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject); // Temporary workaround
        instances = state.dataInputProcessing->inputProcessor->epJSON.find(CurrentModuleObject);
        if (instances != state.dataInputProcessing->inputProcessor->epJSON.end()) {
            int i = 1; // Temporary workaround
            state.dataAirflowNetwork->DisSysCompTermUnitData.allocate(
                state.dataAirflowNetworkBalanceManager->DisSysNumOfTermUnits); // Temporary workaround
            auto &instancesValue = instances.value();
            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
                auto const &fields = instance.value();
                // auto const &thisObjectName = UtilityRoutines::MakeUPPERCase(instance.key());
                state.dataInputProcessing->inputProcessor->markObjectAsUsed(CurrentModuleObject, instance.key()); // Temporary workaround

                std::string tu_name = fields.at("terminal_unit_name");
                std::string tu_type = fields.at("terminal_unit_object_type");
                Real64 L{fields.at("air_path_length")};
                Real64 D{fields.at("air_path_hydraulic_diameter")};

                state.dataAirflowNetwork->DisSysCompTermUnitData(i).name =
                    UtilityRoutines::MakeUPPERCase(tu_name);                               // Name of associated EPlus coil component
                state.dataAirflowNetwork->DisSysCompTermUnitData(i).EPlusType = tu_type;   // Terminal unit type
                state.dataAirflowNetwork->DisSysCompTermUnitData(i).L = L;                 // Air path length
                state.dataAirflowNetwork->DisSysCompTermUnitData(i).hydraulicDiameter = D; // Air path hydraulic diameter

                // Add the element to the lookup table, check for name overlaps
                if (solver.elements.find(state.dataAirflowNetwork->DisSysCompTermUnitData(i).name) == solver.elements.end()) {
                    solver.elements[state.dataAirflowNetwork->DisSysCompTermUnitData(i).name] =
                        &state.dataAirflowNetwork->DisSysCompTermUnitData(i); // Yet another workaround
                } else {
                    ShowSevereError(state,
                                    std::string{RoutineName} +
                                        "Duplicated airflow element names are found = " + state.dataAirflowNetwork->DisSysCompTermUnitData(i).name);
                    // ShowContinueError(state, "A unique component name is required in both objects " + std::string{CompName}(1) + " and " + std::string{CompName}(2));
                    success = false;
                }

                ++i;
            }
        }

        // Get input data of constant pressure drop component
        CurrentModuleObject = "AirflowNetwork:Distribution:Component:ConstantPressureDrop";
        state.dataAirflowNetworkBalanceManager->DisSysNumOfCPDs =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject); // Temporary workaround
        instances = state.dataInputProcessing->inputProcessor->epJSON.find(CurrentModuleObject);
        if (instances != state.dataInputProcessing->inputProcessor->epJSON.end()) {
            int i = 1;                                                                                                     // Temporary workaround
            state.dataAirflowNetwork->DisSysCompCPDData.allocate(state.dataAirflowNetworkBalanceManager->DisSysNumOfCPDs); // Temporary workaround
            auto &instancesValue = instances.value();
            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
                auto const &fields = instance.value();
                auto const &thisObjectName = UtilityRoutines::MakeUPPERCase(instance.key());
                state.dataInputProcessing->inputProcessor->markObjectAsUsed(CurrentModuleObject, instance.key()); // Temporary workaround

                Real64 dp{fields.at("pressure_difference_across_the_component")};

                state.dataAirflowNetwork->DisSysCompCPDData(i).name = thisObjectName; // Name of constant pressure drop component
                state.dataAirflowNetwork->DisSysCompCPDData(i).A = 1.0;               // cross section area
                state.dataAirflowNetwork->DisSysCompCPDData(i).DP = dp;               // Pressure difference across the component

                // Add the element to the lookup table, check for name overlaps
                if (solver.elements.find(thisObjectName) == solver.elements.end()) {
                    solver.elements[thisObjectName] = &state.dataAirflowNetwork->DisSysCompCPDData(i); // Yet another workaround
                } else {
                    ShowSevereError(state, std::string{RoutineName} + "Duplicated airflow element names are found = " + thisObjectName);
                    // ShowContinueError(state, "A unique component name is required in both objects " + std::string{CompName}(1) + " and " + std::string{CompName}(2));
                    success = false;
                }

                ++i;
            }
        }

        return success;
    }

    void GetAirflowNetworkInput(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   Aug. 2003
        //       MODIFIED       Aug. 2005
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine reads inputs of air distribution system

        // Using/Aliasing
        using CurveManager::GetCurveIndex;
        using DataLoopNode::ObjectIsParent;
        using HVACHXAssistedCoolingCoil::VerifyHeatExchangerParent;
        using MixedAir::GetOAMixerNumber;
        using NodeInputManager::GetOnlySingleNode;
        using OutAirNodeManager::SetOutAirNodes;
        using RoomAirModelManager::GetRAFNNodeNum;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static constexpr std::string_view RoutineName("GetAirflowNetworkInput: "); // include trailing blank space

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        // int i;
        int n;
        int j;
        int k;
        int m;
        int count;
        bool NodeFound;
        bool CompFound;
        bool ErrorsFound;
        bool found;
        bool NodeFound1;
        bool NodeFound2;
        int NumAPL;
        Array1D_string CompName(2);
        std::string SimAirNetworkKey;
        bool SimObjectError;
        std::string StringOut;
        int ZoneNum;
        int NodeNum;

        // Declare variables used in this subroutine for debug purpose
        bool AirflowNetworkInitFlag;
        Array1D_int ZoneCheck;
        Array1D_int ZoneBCCheck;
        bool SurfaceFound;

        int NumAlphas;  // Number of Alphas for each GetObjectItem call
        int NumNumbers; // Number of Numbers for each GetObjectItem call
        int IOStatus;   // Used in GetObjectItem
        std::string CurrentModuleObject;
        Array1D_string Alphas;         // Alpha input items for object
        Array1D_string cAlphaFields;   // Alpha field names
        Array1D_string cNumericFields; // Numeric field names
        Array1D<Real64> Numbers;       // Numeric input items for object
        Array1D_bool lAlphaBlanks;     // Logical array, alpha field input BLANK = .TRUE.
        Array1D_bool lNumericBlanks;   // Logical array, numeric field input BLANK = .TRUE.
        int MaxNums(0);                // Maximum number of numeric input fields
        int MaxAlphas(0);              // Maximum number of alpha input fields
        int TotalArgs(0);              // Total number of alpha and numeric arguments (max) for a
        bool Errorfound1;
        Real64 minHeight;
        Real64 maxHeight;
        Real64 baseratio;

        auto &Node(state.dataLoopNodes->Node);

        // Formats
        static constexpr fmt::string_view Format_110("! <AirflowNetwork Model:Control>, No Multizone or Distribution/Multizone with Distribution/Multizone "
                                         "without Distribution/Multizone with Distribution only during Fan Operation\n");
        static constexpr fmt::string_view Format_120("AirflowNetwork Model:Control,{}\n");

        // Set the maximum numbers of input fields
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, "AirflowNetwork:SimulationControl", TotalArgs, NumAlphas, NumNumbers);
        MaxNums = max(MaxNums, NumNumbers);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, "AirflowNetwork:MultiZone:Zone", TotalArgs, NumAlphas, NumNumbers);
        MaxNums = max(MaxNums, NumNumbers);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, "AirflowNetwork:MultiZone:Surface", TotalArgs, NumAlphas, NumNumbers);
        MaxNums = max(MaxNums, NumNumbers);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(
            state, "AirflowNetwork:MultiZone:Component:DetailedOpening", TotalArgs, NumAlphas, NumNumbers);
        MaxNums = max(MaxNums, NumNumbers);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(
            state, "AirflowNetwork:MultiZone:ExternalNode", TotalArgs, NumAlphas, NumNumbers);
        MaxNums = max(MaxNums, NumNumbers);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(
            state, "AirflowNetwork:MultiZone:WindPressureCoefficientArray", TotalArgs, NumAlphas, NumNumbers);
        MaxNums = max(MaxNums, NumNumbers);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(
            state, "AirflowNetwork:MultiZone:WindPressureCoefficientValues", TotalArgs, NumAlphas, NumNumbers);
        MaxNums = max(MaxNums, NumNumbers);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, "AirflowNetwork:Distribution:Node", TotalArgs, NumAlphas, NumNumbers);
        MaxNums = max(MaxNums, NumNumbers);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(
            state, "AirflowNetwork:Distribution:DuctViewFactors", TotalArgs, NumAlphas, NumNumbers);
        MaxNums = max(MaxNums, NumNumbers);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(
            state, "AirflowNetwork:Distribution:Linkage", TotalArgs, NumAlphas, NumNumbers);
        MaxNums = max(MaxNums, NumNumbers);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(
            state, "AirflowNetwork:OccupantVentilationControl", TotalArgs, NumAlphas, NumNumbers);
        MaxNums = max(MaxNums, NumNumbers);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, "AirflowNetwork:IntraZone:Node", TotalArgs, NumAlphas, NumNumbers);
        MaxNums = max(MaxNums, NumNumbers);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, "AirflowNetwork:IntraZone:Linkage", TotalArgs, NumAlphas, NumNumbers);
        MaxNums = max(MaxNums, NumNumbers);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(
            state, "AirflowNetwork:ZoneControl:PressureController", TotalArgs, NumAlphas, NumNumbers);
        MaxNums = max(MaxNums, NumNumbers);
        MaxAlphas = max(MaxAlphas, NumAlphas);

        Alphas.allocate(MaxAlphas);
        cAlphaFields.allocate(MaxAlphas);
        cNumericFields.allocate(MaxNums);
        Numbers.dimension(MaxNums, 0.0);
        lAlphaBlanks.dimension(MaxAlphas, true);
        lNumericBlanks.dimension(MaxNums, true);

        ErrorsFound = false;
        AirflowNetworkInitFlag = false;

        auto &Zone(state.dataHeatBal->Zone);

        // Read AirflowNetwork OccupantVentilationControl before reading other AirflowNetwork objects, so that this object can be called by other
        // simple ventilation objects
        CurrentModuleObject = "AirflowNetwork:OccupantVentilationControl";
        state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfOccuVentCtrls =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
        if (state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfOccuVentCtrls > 0) {
            state.dataAirflowNetworkBalanceManager->OccupantVentilationControl.allocate(
                state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfOccuVentCtrls);
            for (int i = 1; i <= state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfOccuVentCtrls; ++i) {
                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         CurrentModuleObject,
                                                                         i,
                                                                         Alphas,
                                                                         NumAlphas,
                                                                         Numbers,
                                                                         NumNumbers,
                                                                         IOStatus,
                                                                         lNumericBlanks,
                                                                         lAlphaBlanks,
                                                                         cAlphaFields,
                                                                         cNumericFields);
                UtilityRoutines::IsNameEmpty(state, Alphas(1), CurrentModuleObject, ErrorsFound);
                state.dataAirflowNetworkBalanceManager->OccupantVentilationControl(i).Name = Alphas(1); // Name of object
                state.dataAirflowNetworkBalanceManager->OccupantVentilationControl(i).MinOpeningTime = Numbers(1);
                if (state.dataAirflowNetworkBalanceManager->OccupantVentilationControl(i).MinOpeningTime < 0.0) {
                    ShowWarningError(state, std::string{RoutineName} + CurrentModuleObject + " object, " + cNumericFields(1) + " < 0.0");
                    ShowContinueError(state,
                                      format("..Input value = {:.1R}, Value will be reset to 0.0",
                                             state.dataAirflowNetworkBalanceManager->OccupantVentilationControl(i).MinOpeningTime));
                    ShowContinueError(
                        state, "..for " + cAlphaFields(1) + " = \"" + state.dataAirflowNetworkBalanceManager->OccupantVentilationControl(i).Name);
                    state.dataAirflowNetworkBalanceManager->OccupantVentilationControl(i).MinOpeningTime = 0.0;
                }
                state.dataAirflowNetworkBalanceManager->OccupantVentilationControl(i).MinClosingTime = Numbers(2);
                if (state.dataAirflowNetworkBalanceManager->OccupantVentilationControl(i).MinClosingTime < 0.0) {
                    ShowWarningError(state, std::string{RoutineName} + CurrentModuleObject + " object, " + cNumericFields(2) + " < 0.0");
                    ShowContinueError(state,
                                      format("..Input value = {:.1R}, Value will be reset to 0.0",
                                             state.dataAirflowNetworkBalanceManager->OccupantVentilationControl(i).MinClosingTime));
                    ShowContinueError(
                        state, "..for " + cAlphaFields(1) + " = \"" + state.dataAirflowNetworkBalanceManager->OccupantVentilationControl(i).Name);
                    state.dataAirflowNetworkBalanceManager->OccupantVentilationControl(i).MinClosingTime = 0.0;
                }
                if (NumAlphas == 1 && NumNumbers == 2) {
                    state.dataAirflowNetworkBalanceManager->OccupantVentilationControl(i).MinTimeControlOnly = true;
                }
                if (!lAlphaBlanks(2)) {
                    state.dataAirflowNetworkBalanceManager->OccupantVentilationControl(i).ComfortLowTempCurveName = Alphas(2);
                    state.dataAirflowNetworkBalanceManager->OccupantVentilationControl(i).ComfortLowTempCurveNum =
                        GetCurveIndex(state, Alphas(2)); // convert curve name to number
                    if (state.dataAirflowNetworkBalanceManager->OccupantVentilationControl(i).ComfortLowTempCurveNum == 0) {
                        state.dataAirflowNetworkBalanceManager->OccupantVentilationControl(i).MinTimeControlOnly = true;
                        ShowWarningError(state,
                                         std::string{RoutineName} + CurrentModuleObject + " object, " + cAlphaFields(2) + " not found = " +
                                             state.dataAirflowNetworkBalanceManager->OccupantVentilationControl(i).ComfortLowTempCurveName);
                        ShowContinueError(state, "..for specified " + cAlphaFields(1) + " = " + Alphas(1));
                        ShowContinueError(
                            state,
                            "Thermal comfort will not be performed and minimum opening and closing times are checked only. Simulation continues.");
                    } else {
                        ErrorsFound |= CurveManager::CheckCurveDims(
                            state,
                            state.dataAirflowNetworkBalanceManager->OccupantVentilationControl(i).ComfortLowTempCurveNum, // Curve index
                            {1},                                                                                          // Valid dimensions
                            RoutineName,                                                                                  // Routine name
                            CurrentModuleObject,                                                                          // Object Type
                            state.dataAirflowNetworkBalanceManager->OccupantVentilationControl(i).Name,                   // Object Name
                            cAlphaFields(2));                                                                             // Field Name
                    }
                }
                if (!lAlphaBlanks(3)) {
                    state.dataAirflowNetworkBalanceManager->OccupantVentilationControl(i).ComfortHighTempCurveName = Alphas(3);
                    state.dataAirflowNetworkBalanceManager->OccupantVentilationControl(i).ComfortHighTempCurveNum =
                        GetCurveIndex(state, Alphas(3)); // convert curve name to number
                    if (state.dataAirflowNetworkBalanceManager->OccupantVentilationControl(i).ComfortHighTempCurveNum > 0) {
                        ErrorsFound |= CurveManager::CheckCurveDims(
                            state,
                            state.dataAirflowNetworkBalanceManager->OccupantVentilationControl(i).ComfortHighTempCurveNum, // Curve index
                            {1},                                                                                           // Valid dimensions
                            RoutineName,                                                                                   // Routine name
                            CurrentModuleObject,                                                                           // Object Type
                            state.dataAirflowNetworkBalanceManager->OccupantVentilationControl(i).Name,                    // Object Name
                            cAlphaFields(3));                                                                              // Field Name
                    } else {
                        ShowWarningError(state,
                                         std::string{RoutineName} + CurrentModuleObject + " object, " + cAlphaFields(3) + " not found = " +
                                             state.dataAirflowNetworkBalanceManager->OccupantVentilationControl(i).ComfortHighTempCurveName);
                        ShowContinueError(state, "..for specified " + cAlphaFields(1) + " = " + Alphas(1));
                        ShowContinueError(state, "A single curve of thermal comfort low temperature is used only. Simulation continues.");
                    }
                }
                if (state.dataAirflowNetworkBalanceManager->OccupantVentilationControl(i).ComfortHighTempCurveNum > 0) {
                    state.dataAirflowNetworkBalanceManager->OccupantVentilationControl(i).ComfortBouPoint = Numbers(3);
                    if (state.dataAirflowNetworkBalanceManager->OccupantVentilationControl(i).ComfortBouPoint < 0.0) {
                        ShowWarningError(state, std::string{RoutineName} + CurrentModuleObject + " object, " + cNumericFields(3) + " < 0.0");
                        ShowContinueError(state,
                                          format("..Input value = {:.1R}, Value will be reset to 10.0 as default",
                                                 state.dataAirflowNetworkBalanceManager->OccupantVentilationControl(i).ComfortBouPoint));
                        ShowContinueError(
                            state, "..for " + cAlphaFields(1) + " = \"" + state.dataAirflowNetworkBalanceManager->OccupantVentilationControl(i).Name);
                        state.dataAirflowNetworkBalanceManager->OccupantVentilationControl(i).ComfortBouPoint = 10.0;
                    }
                }
                // Check continuity of both curves at boundary point
                if (state.dataAirflowNetworkBalanceManager->OccupantVentilationControl(i).ComfortLowTempCurveNum > 0 &&
                    state.dataAirflowNetworkBalanceManager->OccupantVentilationControl(i).ComfortHighTempCurveNum) {
                    if (std::abs(CurveValue(state,
                                            state.dataAirflowNetworkBalanceManager->OccupantVentilationControl(i).ComfortLowTempCurveNum,
                                            Numbers(3)) -
                                 CurveValue(state,
                                            state.dataAirflowNetworkBalanceManager->OccupantVentilationControl(i).ComfortHighTempCurveNum,
                                            Numbers(3))) > 0.1) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + " object: The difference of both curve values at boundary point > 0.1");
                        ShowContinueError(state, "Both curve names are = " + cAlphaFields(2) + " and " + cAlphaFields(3));
                        ShowContinueError(state,
                                          format("The input value of {} = {:.1R}",
                                                 cNumericFields(3),
                                                 state.dataAirflowNetworkBalanceManager->OccupantVentilationControl(i).ComfortBouPoint));
                        ErrorsFound = true;
                    }
                }
                if (!lNumericBlanks(4)) {
                    state.dataAirflowNetworkBalanceManager->OccupantVentilationControl(i).MaxPPD = Numbers(4);
                    if (state.dataAirflowNetworkBalanceManager->OccupantVentilationControl(i).MaxPPD < 0.0 ||
                        state.dataAirflowNetworkBalanceManager->OccupantVentilationControl(i).MaxPPD > 100.0) {
                        ShowWarningError(state, std::string{RoutineName} + CurrentModuleObject + " object, " + cNumericFields(4) + " beyond 0.0 and 100.0");
                        ShowContinueError(state,
                                          format("..Input value = {:.1R}, Value will be reset to 10.0 as default",
                                                 state.dataAirflowNetworkBalanceManager->OccupantVentilationControl(i).MaxPPD));
                        ShowContinueError(
                            state, "..for " + cAlphaFields(1) + " = \"" + state.dataAirflowNetworkBalanceManager->OccupantVentilationControl(i).Name);
                        state.dataAirflowNetworkBalanceManager->OccupantVentilationControl(i).MaxPPD = 10.0;
                    }
                }
                if (!lAlphaBlanks(4)) {
                    if (UtilityRoutines::SameString(Alphas(4), "Yes")) {
                        state.dataAirflowNetworkBalanceManager->OccupantVentilationControl(i).OccupancyCheck = true;
                    } else if (UtilityRoutines::SameString(Alphas(4), "No")) {
                        state.dataAirflowNetworkBalanceManager->OccupantVentilationControl(i).OccupancyCheck = false;
                    } else {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" + Alphas(1) + "\" invalid " + cAlphaFields(2) + "=\"" + Alphas(2) +
                                            "\" illegal key.");
                        ShowContinueError(state, "Valid keys are: Yes or No");
                        ErrorsFound = true;
                    }
                }
                if (!lAlphaBlanks(5)) {
                    state.dataAirflowNetworkBalanceManager->OccupantVentilationControl(i).OpeningProbSchName =
                        Alphas(5); // a schedule name for opening probability
                    state.dataAirflowNetworkBalanceManager->OccupantVentilationControl(i).OpeningProbSchNum =
                        GetScheduleIndex(state, state.dataAirflowNetworkBalanceManager->OccupantVentilationControl(i).OpeningProbSchName);
                    if (state.dataAirflowNetworkBalanceManager->OccupantVentilationControl(i).OpeningProbSchNum == 0) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + " object, " + cAlphaFields(5) + " not found = " +
                                            state.dataAirflowNetworkBalanceManager->OccupantVentilationControl(i).OpeningProbSchName);
                        ShowContinueError(state, "..for specified " + cAlphaFields(1) + " = " + Alphas(1));
                        ErrorsFound = true;
                    }
                }
                if (!lAlphaBlanks(6)) {
                    state.dataAirflowNetworkBalanceManager->OccupantVentilationControl(i).ClosingProbSchName =
                        Alphas(6); // a schedule name for closing probability
                    state.dataAirflowNetworkBalanceManager->OccupantVentilationControl(i).ClosingProbSchNum =
                        GetScheduleIndex(state, state.dataAirflowNetworkBalanceManager->OccupantVentilationControl(i).ClosingProbSchName);
                    if (state.dataAirflowNetworkBalanceManager->OccupantVentilationControl(i).OpeningProbSchNum == 0) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + " object, " + cAlphaFields(6) + " not found = " +
                                            state.dataAirflowNetworkBalanceManager->OccupantVentilationControl(i).ClosingProbSchName);
                        ShowContinueError(state, "..for specified " + cAlphaFields(1) + " = " + Alphas(1));
                        ErrorsFound = true;
                    }
                }
            }
        }

        if (ErrorsFound) {
            ShowFatalError(state, std::string{RoutineName} + "Errors found getting inputs. Previous error(s) cause program termination.");
        }

        // *** Read AirflowNetwork simulation parameters
        CurrentModuleObject = "AirflowNetwork:SimulationControl";
        state.dataAirflowNetworkBalanceManager->NumAirflowNetwork =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
        if (state.dataAirflowNetworkBalanceManager->NumAirflowNetwork == 0) {
            if (state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "AirflowNetwork:MultiZone:Zone") >= 1 &&
                state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "AirflowNetwork:MultiZone:Surface") >= 2) {
                state.dataAirflowNetwork->AFNDefaultControlFlag = true;
                state.dataAirflowNetwork->AirflowNetworkSimu.AirflowNetworkSimuName = "AFNDefaultControl";
                state.dataAirflowNetwork->AirflowNetworkSimu.Control = "MULTIZONEWITHOUTDISTRIBUTION";
                state.dataAirflowNetwork->AirflowNetworkSimu.WPCCntr = "SURFACEAVERAGECALCULATION";
                state.dataAirflowNetwork->AirflowNetworkSimu.HeightOption = "OPENINGHEIGHT";
                state.dataAirflowNetwork->AirflowNetworkSimu.BldgType = "LOWRISE";
                state.dataAirflowNetwork->AirflowNetworkSimu.InitType = "ZERONODEPRESSURES";
                state.dataAirflowNetwork->AirflowNetworkSimu.TExtHeightDep = false;
                state.dataAirflowNetwork->AirflowNetworkSimu.solver = AirflowNetworkSimuProp::Solver::SkylineLU;
                // Use default values for numerical fields
                state.dataAirflowNetwork->AirflowNetworkSimu.MaxIteration = 500;
                state.dataAirflowNetwork->AirflowNetworkSimu.RelTol = 1.E-4;
                state.dataAirflowNetwork->AirflowNetworkSimu.AbsTol = 1.E-6;
                state.dataAirflowNetwork->AirflowNetworkSimu.ConvLimit = -0.5;
                state.dataAirflowNetwork->AirflowNetworkSimu.Azimuth = 0.0;
                state.dataAirflowNetwork->AirflowNetworkSimu.AspectRatio = 1.0;
                state.dataAirflowNetwork->AirflowNetworkSimu.MaxPressure = 500.0; // Maximum pressure difference by default
                state.dataAirflowNetwork->SimulateAirflowNetwork = AirflowNetworkControlMultizone;
                SimAirNetworkKey = "MultizoneWithoutDistribution";
                state.dataAirflowNetwork->AirflowNetworkSimu.InitFlag = 1;
                ShowWarningError(state, std::string{RoutineName} + CurrentModuleObject + " object is not found ");
                ShowContinueError(state, "..The default behaviour values are assigned. Please see details in Input Output Reference.");
            } else {
                state.dataAirflowNetwork->SimulateAirflowNetwork = AirflowNetworkControlSimple;
                print(state.files.eio, Format_110);
                print(state.files.eio, Format_120, "NoMultizoneOrDistribution");
                return;
            }
        }
        if (state.dataAirflowNetworkBalanceManager->NumAirflowNetwork > 1) {
            ShowFatalError(state, std::string{RoutineName} + "Only one (\"1\") " + CurrentModuleObject + " object per simulation is allowed.");
        }

        SimObjectError = false;
        if (!state.dataAirflowNetwork->AFNDefaultControlFlag) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     state.dataAirflowNetworkBalanceManager->NumAirflowNetwork,
                                                                     Alphas,
                                                                     NumAlphas,
                                                                     Numbers,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     lNumericBlanks,
                                                                     lAlphaBlanks,
                                                                     cAlphaFields,
                                                                     cNumericFields);

            state.dataAirflowNetwork->AirflowNetworkSimu.AirflowNetworkSimuName = Alphas(1);
            state.dataAirflowNetwork->AirflowNetworkSimu.Control = Alphas(2);
            state.dataAirflowNetwork->AirflowNetworkSimu.WPCCntr = Alphas(3);
            state.dataAirflowNetwork->AirflowNetworkSimu.HeightOption = Alphas(4);
            state.dataAirflowNetwork->AirflowNetworkSimu.BldgType = Alphas(5);

            // Retrieve flag allowing the support of zone equipment
            state.dataAirflowNetwork->AirflowNetworkSimu.AllowSupportZoneEqp = false;
            if (UtilityRoutines::SameString(Alphas(9), "Yes")) {
                state.dataAirflowNetwork->AirflowNetworkSimu.AllowSupportZoneEqp = true;
            }

            // Find a flag for possible combination of vent and distribution system
            {
                auto const SELECT_CASE_var(UtilityRoutines::MakeUPPERCase(state.dataAirflowNetwork->AirflowNetworkSimu.Control));
                if (SELECT_CASE_var == "NOMULTIZONEORDISTRIBUTION") {
                    state.dataAirflowNetwork->SimulateAirflowNetwork = AirflowNetworkControlSimple;
                    SimAirNetworkKey = "NoMultizoneOrDistribution";
                } else if (SELECT_CASE_var == "MULTIZONEWITHOUTDISTRIBUTION") {
                    state.dataAirflowNetwork->SimulateAirflowNetwork = AirflowNetworkControlMultizone;
                    SimAirNetworkKey = "MultizoneWithoutDistribution";
                } else if (SELECT_CASE_var == "MULTIZONEWITHDISTRIBUTIONONLYDURINGFANOPERATION") {
                    state.dataAirflowNetwork->SimulateAirflowNetwork = AirflowNetworkControlSimpleADS;
                    SimAirNetworkKey = "MultizoneWithDistributionOnlyDuringFanOperation";
                } else if (SELECT_CASE_var == "MULTIZONEWITHDISTRIBUTION") {
                    state.dataAirflowNetwork->SimulateAirflowNetwork = AirflowNetworkControlMultiADS;
                    SimAirNetworkKey = "MultizoneWithDistribution";
                } else { // Error
                    ShowSevereError(state,
                                    std::string{RoutineName} + CurrentModuleObject + " object, The entered choice for " + cAlphaFields(2) + " is not valid = \"" +
                                        state.dataAirflowNetwork->AirflowNetworkSimu.Control + "\"");
                    ShowContinueError(state,
                                      "Valid choices are \"NO MULTIZONE OR DISTRIBUTION\",\"MULTIZONE WITH DISTRIBUTION ONLY DURING FAN OPERATION\"");
                    ShowContinueError(state, "\"MULTIZONE WITH DISTRIBUTION\", or \"MULTIZONE WITHOUT DISTRIBUTION\"");
                    ShowContinueError(state,
                                      "..specified in " + CurrentModuleObject + ' ' + cAlphaFields(1) + " = " +
                                          state.dataAirflowNetwork->AirflowNetworkSimu.AirflowNetworkSimuName);
                    ErrorsFound = true;
                }
            }
        }

        // Check the number of primary air loops
        if (state.dataAirflowNetwork->SimulateAirflowNetwork == AirflowNetworkControlSimpleADS ||
            state.dataAirflowNetwork->SimulateAirflowNetwork == AirflowNetworkControlMultiADS) {
            NumAPL = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "AirLoopHVAC");
            if (NumAPL > 0) {
                state.dataAirflowNetworkBalanceManager->LoopPartLoadRatio.allocate(NumAPL);
                state.dataAirflowNetworkBalanceManager->LoopOnOffFanRunTimeFraction.allocate(NumAPL);
                state.dataAirflowNetworkBalanceManager->LoopOnOffFlag.allocate(NumAPL);
                state.dataAirflowNetworkBalanceManager->LoopPartLoadRatio = 0.0;
                state.dataAirflowNetworkBalanceManager->LoopOnOffFanRunTimeFraction = 0.0;
                state.dataAirflowNetworkBalanceManager->LoopOnOffFlag = false;
            }
        }
        print(state.files.eio, Format_110);
        print(state.files.eio, Format_120, SimAirNetworkKey);

        if (state.dataAirflowNetwork->AFNDefaultControlFlag) {
            cAlphaFields(2) = "AirflowNetwork Control";
        }

        // Check whether there are any objects from infiltration, ventilation, mixing and cross mixing
        if (state.dataAirflowNetwork->SimulateAirflowNetwork == AirflowNetworkControlSimple ||
            state.dataAirflowNetwork->SimulateAirflowNetwork == AirflowNetworkControlSimpleADS) {
            if (state.dataHeatBal->TotInfiltration + state.dataHeatBal->TotVentilation + state.dataHeatBal->TotMixing +
                    state.dataHeatBal->TotCrossMixing + state.dataHeatBal->TotZoneAirBalance +
                    state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "ZoneEarthtube") +
                    state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "ZoneThermalChimney") +
                    state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "ZoneCoolTower:Shower") ==
                0) {
                ShowWarningError(state, std::string{RoutineName} + cAlphaFields(2) + " = \"" + SimAirNetworkKey + "\".");
                ShowContinueError(
                    state,
                    "..but there are no Infiltration, Ventilation, Mixing, Cross Mixing or ZoneAirBalance objects. The simulation continues...");
            }
        }

        // Check whether a user wants to perform SIMPLE calculation only or not
        if (state.dataAirflowNetwork->SimulateAirflowNetwork == AirflowNetworkControlSimple) return;

        if (state.dataAirflowNetwork->SimulateAirflowNetwork == AirflowNetworkControlMultizone ||
            state.dataAirflowNetwork->SimulateAirflowNetwork == AirflowNetworkControlMultiADS) {
            if (state.dataHeatBal->TotInfiltration > 0) {
                ShowWarningError(state, std::string{RoutineName} + CurrentModuleObject + " object, ");
                ShowContinueError(state,
                                  "..Specified " + cAlphaFields(2) + " = \"" + SimAirNetworkKey + "\" and ZoneInfiltration:* objects are present.");
                ShowContinueError(state, "..ZoneInfiltration objects will not be simulated.");
            }
            if (state.dataHeatBal->TotVentilation > 0) {
                ShowWarningError(state, std::string{RoutineName} + CurrentModuleObject + " object, ");
                ShowContinueError(state,
                                  "..Specified " + cAlphaFields(2) + " = \"" + SimAirNetworkKey + "\" and ZoneVentilation:* objects are present.");
                ShowContinueError(state, "..ZoneVentilation objects will not be simulated.");
            }
            if (state.dataHeatBal->TotMixing > 0) {
                ShowWarningError(state, std::string{RoutineName} + CurrentModuleObject + " object, ");
                ShowContinueError(state, "..Specified " + cAlphaFields(2) + " = \"" + SimAirNetworkKey + "\" and ZoneMixing objects are present.");
                ShowContinueError(state, "..ZoneMixing objects will not be simulated.");
            }
            if (state.dataHeatBal->TotCrossMixing > 0) {
                ShowWarningError(state, std::string{RoutineName} + CurrentModuleObject + " object, ");
                ShowContinueError(state,
                                  "..Specified " + cAlphaFields(2) + " = \"" + SimAirNetworkKey + "\" and ZoneCrossMixing objects are present.");
                ShowContinueError(state, "..ZoneCrossMixing objects will not be simulated.");
            }
            if (state.dataHeatBal->TotZoneAirBalance > 0) {
                ShowWarningError(state, std::string{RoutineName} + CurrentModuleObject + " object, ");
                ShowContinueError(
                    state, "..Specified " + cAlphaFields(2) + " = \"" + SimAirNetworkKey + "\" and ZoneAirBalance:OutdoorAir objects are present.");
                ShowContinueError(state, "..ZoneAirBalance:OutdoorAir objects will not be simulated.");
            }
            if (state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "ZoneEarthtube") > 0) {
                ShowWarningError(state, std::string{RoutineName} + CurrentModuleObject + " object, ");
                ShowContinueError(state, "..Specified " + cAlphaFields(2) + " = \"" + SimAirNetworkKey + "\" and ZoneEarthtube objects are present.");
                ShowContinueError(state, "..ZoneEarthtube objects will not be simulated.");
            }
            if (state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "ZoneThermalChimney") > 0) {
                ShowWarningError(state, std::string{RoutineName} + CurrentModuleObject + " object, ");
                ShowContinueError(state,
                                  "..Specified " + cAlphaFields(2) + " = \"" + SimAirNetworkKey + "\" and ZoneThermalChimney objects are present.");
                ShowContinueError(state, "..ZoneThermalChimney objects will not be simulated.");
            }
            if (state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "ZoneCoolTower:Shower") > 0) {
                ShowWarningError(state, std::string{RoutineName} + CurrentModuleObject + " object, ");
                ShowContinueError(state,
                                  "..Specified " + cAlphaFields(2) + " = \"" + SimAirNetworkKey + "\" and ZoneCoolTower:Shower objects are present.");
                ShowContinueError(state, "..ZoneCoolTower:Shower objects will not be simulated.");
            }
        }

        SetOutAirNodes(state);
        if (!state.dataAirflowNetwork->AFNDefaultControlFlag) {
            if (UtilityRoutines::SameString(state.dataAirflowNetwork->AirflowNetworkSimu.WPCCntr, "Input")) {
                state.dataAirflowNetwork->AirflowNetworkSimu.iWPCCnt = iWPCCntr::Input;
                if (lAlphaBlanks(4)) {
                    ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + " object, " + cAlphaFields(3) + " = INPUT.");
                    ShowContinueError(state, ".." + cAlphaFields(4) + " was not entered.");
                    ErrorsFound = true;
                    SimObjectError = true;
                } else {
                    if (!(UtilityRoutines::SameString(state.dataAirflowNetwork->AirflowNetworkSimu.HeightOption, "ExternalNode") ||
                          UtilityRoutines::SameString(state.dataAirflowNetwork->AirflowNetworkSimu.HeightOption, "OpeningHeight"))) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + " object, " + cAlphaFields(4) + " = " + Alphas(4) + " is invalid.");
                        ShowContinueError(state,
                                          "Valid choices are ExternalNode or OpeningHeight. " + CurrentModuleObject + ": " + cAlphaFields(1) + " = " +
                                              state.dataAirflowNetwork->AirflowNetworkSimu.AirflowNetworkSimuName);
                        ErrorsFound = true;
                        SimObjectError = true;
                    }
                }
            } else if (UtilityRoutines::SameString(state.dataAirflowNetwork->AirflowNetworkSimu.WPCCntr, "SurfaceAverageCalculation")) {
                state.dataAirflowNetwork->AirflowNetworkSimu.iWPCCnt = iWPCCntr::SurfAvg;
                if (!(UtilityRoutines::SameString(state.dataAirflowNetwork->AirflowNetworkSimu.BldgType, "LowRise") ||
                      UtilityRoutines::SameString(state.dataAirflowNetwork->AirflowNetworkSimu.BldgType, "HighRise"))) {
                    ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + " object, " + cAlphaFields(5) + " = " + Alphas(5) + " is invalid.");
                    ShowContinueError(state,
                                      "Valid choices are LowRise or HighRise. " + CurrentModuleObject + ": " + cAlphaFields(1) + " = " +
                                          state.dataAirflowNetwork->AirflowNetworkSimu.AirflowNetworkSimuName);
                    ErrorsFound = true;
                    SimObjectError = true;
                }
                for (k = 1; k <= state.dataLoopNodes->NumOfNodes; ++k) {
                    if (Node(k).IsLocalNode) {
                        ShowSevereError(state, std::string{RoutineName} + "Invalid " + cAlphaFields(3) + "=" + Alphas(3));
                        ShowContinueError(state,
                                          "A local air node is defined to INPUT the wind pressure coefficient curve, while Wind Pressure Coefficient "
                                          "Type is set to SurfaceAverageCalculation.");
                        ShowContinueError(state, "It requires  the Wind Pressure Coefficient Type be set to INPUT to use the local air node.");
                        ErrorsFound = true;
                        SimObjectError = true;
                        break;
                    }
                }
            } else {
                ShowSevereError(state,
                                std::string{RoutineName} + CurrentModuleObject + " object, " + cAlphaFields(3) + " = " +
                                    state.dataAirflowNetwork->AirflowNetworkSimu.WPCCntr + " is not valid.");
                ShowContinueError(state,
                                  "Valid choices are Input or SurfaceAverageCalculation. " + CurrentModuleObject + " = " +
                                      state.dataAirflowNetwork->AirflowNetworkSimu.AirflowNetworkSimuName);
                ErrorsFound = true;
                SimObjectError = true;
            }

            state.dataAirflowNetwork->AirflowNetworkSimu.InitType = Alphas(6);
            if (UtilityRoutines::SameString(state.dataAirflowNetwork->AirflowNetworkSimu.InitType, "LinearInitializationMethod")) {
                state.dataAirflowNetwork->AirflowNetworkSimu.InitFlag = 0;
            } else if (UtilityRoutines::SameString(state.dataAirflowNetwork->AirflowNetworkSimu.InitType, "ZeroNodePressures")) {
                state.dataAirflowNetwork->AirflowNetworkSimu.InitFlag = 1;
            } else if (UtilityRoutines::SameString(state.dataAirflowNetwork->AirflowNetworkSimu.InitType, "0")) {
                state.dataAirflowNetwork->AirflowNetworkSimu.InitFlag = 0;
            } else if (UtilityRoutines::SameString(state.dataAirflowNetwork->AirflowNetworkSimu.InitType, "1")) {
                state.dataAirflowNetwork->AirflowNetworkSimu.InitFlag = 1;
            } else {
                ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + " object, " + cAlphaFields(6) + " = " + Alphas(6) + " is invalid.");
                ShowContinueError(state,
                                  "Valid choices are LinearInitializationMethod or ZeroNodePressures. " + CurrentModuleObject + " = " +
                                      state.dataAirflowNetwork->AirflowNetworkSimu.AirflowNetworkSimuName);
                ErrorsFound = true;
                SimObjectError = true;
            }

            if (!lAlphaBlanks(7) && UtilityRoutines::SameString(Alphas(7), "Yes")) state.dataAirflowNetwork->AirflowNetworkSimu.TExtHeightDep = true;

            if (lAlphaBlanks(8)) {
                state.dataAirflowNetwork->AirflowNetworkSimu.solver = AirflowNetworkSimuProp::Solver::SkylineLU;
            } else if (UtilityRoutines::SameString(Alphas(8), "SkylineLU")) {
                state.dataAirflowNetwork->AirflowNetworkSimu.solver = AirflowNetworkSimuProp::Solver::SkylineLU;
            } else if (UtilityRoutines::SameString(Alphas(8), "ConjugateGradient")) {
                state.dataAirflowNetwork->AirflowNetworkSimu.solver = AirflowNetworkSimuProp::Solver::ConjugateGradient;
            } else {
                state.dataAirflowNetwork->AirflowNetworkSimu.solver = AirflowNetworkSimuProp::Solver::SkylineLU;
                ShowWarningError(state, std::string{RoutineName} + CurrentModuleObject + " object, ");
                ShowContinueError(state, "..Specified " + cAlphaFields(8) + " = \"" + Alphas(8) + "\" is unrecognized.");
                ShowContinueError(state, "..Default value \"SkylineLU\" will be used.");
            }

            if (SimObjectError) {
                ShowFatalError(state,
                               std::string{RoutineName} + "Errors found getting " + CurrentModuleObject + " object. Previous error(s) cause program termination.");
            }

            state.dataAirflowNetwork->AirflowNetworkSimu.MaxIteration = Numbers(1);
            state.dataAirflowNetwork->AirflowNetworkSimu.RelTol = Numbers(2);
            state.dataAirflowNetwork->AirflowNetworkSimu.AbsTol = Numbers(3);
            state.dataAirflowNetwork->AirflowNetworkSimu.ConvLimit = Numbers(4);
            state.dataAirflowNetwork->AirflowNetworkSimu.Azimuth = Numbers(5);
            state.dataAirflowNetwork->AirflowNetworkSimu.AspectRatio = Numbers(6);
            state.dataAirflowNetwork->AirflowNetworkSimu.MaxPressure = 500.0; // Maximum pressure difference by default
        }

        // *** Read AirflowNetwork simulation zone data
        CurrentModuleObject = "AirflowNetwork:MultiZone:Zone";
        state.dataAirflowNetwork->AirflowNetworkNumOfZones =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
        if (state.dataAirflowNetwork->AirflowNetworkNumOfZones > 0) {
            state.dataAirflowNetwork->MultizoneZoneData.allocate(state.dataAirflowNetwork->AirflowNetworkNumOfZones);
            state.dataAirflowNetwork->AirflowNetworkZoneFlag.dimension(state.dataGlobal->NumOfZones, false); // AirflowNetwork zone flag
            for (int i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfZones; ++i) {
                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         CurrentModuleObject,
                                                                         i,
                                                                         Alphas,
                                                                         NumAlphas,
                                                                         Numbers,
                                                                         NumNumbers,
                                                                         IOStatus,
                                                                         lNumericBlanks,
                                                                         lAlphaBlanks,
                                                                         cAlphaFields,
                                                                         cNumericFields);
                UtilityRoutines::IsNameEmpty(state, Alphas(1), CurrentModuleObject, ErrorsFound);
                state.dataAirflowNetwork->MultizoneZoneData(i).ZoneName = Alphas(1); // Name of Associated EnergyPlus Thermal Zone
                if (!lAlphaBlanks(2))
                    state.dataAirflowNetwork->MultizoneZoneData(i).VentControl = Alphas(2); // Ventilation Control Mode: "Temperature", "Enthalpy",
                // "ASHRAE55ADAPTIVE", "CEN15251AdaptiveComfort,
                // "Constant", or "NoVent"
                state.dataAirflowNetwork->MultizoneZoneData(i).VentSchName = Alphas(3); // Name of ventilation temperature control schedule
                state.dataAirflowNetwork->MultizoneZoneData(i).OpenFactor =
                    Numbers(1); // Limit Value on Multiplier for Modulating Venting Open Factor,
                // Not applicable if Vent Control Mode = CONSTANT or NOVENT
                state.dataAirflowNetwork->MultizoneZoneData(i).LowValueTemp = Numbers(2); // Lower Value on Inside/Outside Temperature Difference
                // for Modulating the Venting Open Factor with temp control
                state.dataAirflowNetwork->MultizoneZoneData(i).UpValueTemp = Numbers(3); // Upper Value on Inside/Outside Temperature Difference
                // for Modulating the Venting Open Factor with temp control
                state.dataAirflowNetwork->MultizoneZoneData(i).LowValueEnth = Numbers(4); // Lower Value on Inside/Outside Temperature Difference
                // for Modulating the Venting Open Factor with Enthalpy control
                state.dataAirflowNetwork->MultizoneZoneData(i).UpValueEnth = Numbers(5); // Upper Value on Inside/Outside Temperature Difference
                // for Modulating the Venting Open Factor with Enthalpy control
                state.dataAirflowNetwork->MultizoneZoneData(i).VentCtrNum = VentControlType::None;
                state.dataAirflowNetwork->MultizoneZoneData(i).SingleSidedCpType = Alphas(5);
                state.dataAirflowNetwork->MultizoneZoneData(i).BuildWidth = Numbers(6);

                if (!lAlphaBlanks(6)) {
                    state.dataAirflowNetwork->MultizoneZoneData(i).OccupantVentilationControlName = Alphas(6);
                    state.dataAirflowNetwork->MultizoneZoneData(i).OccupantVentilationControlNum =
                        UtilityRoutines::FindItemInList(state.dataAirflowNetwork->MultizoneZoneData(i).OccupantVentilationControlName,
                                                        state.dataAirflowNetworkBalanceManager->OccupantVentilationControl);
                    if (state.dataAirflowNetwork->MultizoneZoneData(i).OccupantVentilationControlNum == 0) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + " object, " + cAlphaFields(6) +
                                            " not found = " + state.dataAirflowNetwork->MultizoneZoneData(i).OccupantVentilationControlName);
                        ShowContinueError(state, "..for specified " + cAlphaFields(1) + " = " + Alphas(1));
                        ErrorsFound = true;
                    }
                }
                if (UtilityRoutines::SameString(state.dataAirflowNetwork->MultizoneZoneData(i).VentControl, "Temperature"))
                    state.dataAirflowNetwork->MultizoneZoneData(i).VentCtrNum = VentControlType::Temp;
                if (UtilityRoutines::SameString(state.dataAirflowNetwork->MultizoneZoneData(i).VentControl, "Enthalpy"))
                    state.dataAirflowNetwork->MultizoneZoneData(i).VentCtrNum = VentControlType::Enth;
                if (UtilityRoutines::SameString(state.dataAirflowNetwork->MultizoneZoneData(i).VentControl, "Constant"))
                    state.dataAirflowNetwork->MultizoneZoneData(i).VentCtrNum = VentControlType::Const;
                if (UtilityRoutines::SameString(state.dataAirflowNetwork->MultizoneZoneData(i).VentControl, "ASHRAE55Adaptive"))
                    state.dataAirflowNetwork->MultizoneZoneData(i).VentCtrNum = VentControlType::ASH55;
                if (UtilityRoutines::SameString(state.dataAirflowNetwork->MultizoneZoneData(i).VentControl, "CEN15251Adaptive"))
                    state.dataAirflowNetwork->MultizoneZoneData(i).VentCtrNum = VentControlType::CEN15251;
                if (UtilityRoutines::SameString(state.dataAirflowNetwork->MultizoneZoneData(i).VentControl, "NoVent"))
                    state.dataAirflowNetwork->MultizoneZoneData(i).VentCtrNum = VentControlType::NoVent;

                if (state.dataAirflowNetwork->MultizoneZoneData(i).VentCtrNum < NumOfVentCtrTypes) {
                    if (NumAlphas >= 4 && (!lAlphaBlanks(4))) {
                        state.dataAirflowNetwork->MultizoneZoneData(i).VentingSchName = Alphas(4);
                        state.dataAirflowNetwork->MultizoneZoneData(i).VentingSchNum =
                            GetScheduleIndex(state, state.dataAirflowNetwork->MultizoneZoneData(i).VentingSchName);
                        if (state.dataAirflowNetwork->MultizoneZoneData(i).VentingSchNum == 0) {
                            ShowSevereError(state,
                                            std::string{RoutineName} + CurrentModuleObject + " object, " + cAlphaFields(4) +
                                                " not found = " + state.dataAirflowNetwork->MultizoneZoneData(i).VentingSchName);
                            ShowContinueError(state, "..for specified " + cAlphaFields(1) + " = " + Alphas(1));
                            ErrorsFound = true;
                        }
                    }
                } else {
                    state.dataAirflowNetwork->MultizoneZoneData(i).VentingSchName = std::string();
                    state.dataAirflowNetwork->MultizoneZoneData(i).VentingSchNum = 0;
                }
            }
        } else {
            ShowSevereError(state,
                            std::string{RoutineName} + "For an AirflowNetwork Simulation, at least one " + CurrentModuleObject +
                                " object is required but none were found.");
            ShowFatalError(state,
                           std::string{RoutineName} + "Errors found getting " + CurrentModuleObject + " object. Previous error(s) cause program termination.");
        }

        // ==> Zone data validation
        for (int i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfZones; ++i) {
            // Zone name validation
            state.dataAirflowNetwork->MultizoneZoneData(i).ZoneNum =
                UtilityRoutines::FindItemInList(state.dataAirflowNetwork->MultizoneZoneData(i).ZoneName, Zone);
            if (state.dataAirflowNetwork->MultizoneZoneData(i).ZoneNum == 0) {
                ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + " object, invalid " + cAlphaFields(1) + " given.");
                ShowContinueError(state, "..invalid " + cAlphaFields(1) + " = \"" + state.dataAirflowNetwork->MultizoneZoneData(i).ZoneName + "\"");
                ErrorsFound = true;
            } else {
                state.dataAirflowNetwork->AirflowNetworkZoneFlag(state.dataAirflowNetwork->MultizoneZoneData(i).ZoneNum) = true;
                state.dataAirflowNetwork->MultizoneZoneData(i).Height =
                    Zone(state.dataAirflowNetwork->MultizoneZoneData(i).ZoneNum).Centroid.z; // Nodal height
            }
            if (state.dataAirflowNetwork->MultizoneZoneData(i).VentCtrNum == VentControlType::None) {
                ShowSevereError(state,
                                std::string{RoutineName} + CurrentModuleObject + " object, invalid " + cAlphaFields(2) + " = " +
                                    state.dataAirflowNetwork->MultizoneZoneData(i).VentControl);
                ShowContinueError(state, "Valid choices are Temperature, Enthalpy, Constant, or NoVent");
                ShowContinueError(state, ".. in " + cAlphaFields(1) + " = \"" + state.dataAirflowNetwork->MultizoneZoneData(i).ZoneName + "\"");
                ErrorsFound = true;
            }
            if (UtilityRoutines::SameString(state.dataAirflowNetwork->MultizoneZoneData(i).VentControl, "Temperature") ||
                UtilityRoutines::SameString(state.dataAirflowNetwork->MultizoneZoneData(i).VentControl, "Enthalpy")) {
                // .or. &
                // UtilityRoutines::SameString(state.dataAirflowNetwork->MultizoneZoneData(i)%VentControl,'ASHRAE55Adaptive') .or. &
                // UtilityRoutines::SameString(state.dataAirflowNetwork->MultizoneZoneData(i)%VentControl,'CEN15251Adaptive')) then
                state.dataAirflowNetwork->MultizoneZoneData(i).VentSchNum =
                    GetScheduleIndex(state, state.dataAirflowNetwork->MultizoneZoneData(i).VentSchName);
                if (state.dataAirflowNetwork->MultizoneZoneData(i).VentSchName == std::string()) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + CurrentModuleObject + " object, No " + cAlphaFields(3) + " was found, but is required when " +
                                        cAlphaFields(2) + " is Temperature or Enthalpy.");
                    ShowContinueError(state,
                                      "..for " + cAlphaFields(1) + " = \"" + state.dataAirflowNetwork->MultizoneZoneData(i).ZoneName + "\", with " +
                                          cAlphaFields(2) + " = \"" + state.dataAirflowNetwork->MultizoneZoneData(i).VentControl + "\"");
                    ErrorsFound = true;
                } else if (state.dataAirflowNetwork->MultizoneZoneData(i).VentSchNum == 0) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + CurrentModuleObject + " object, invalid " + cAlphaFields(3) + ", required when " + cAlphaFields(2) +
                                        " is Temperature or Enthalpy.");
                    ShowContinueError(state, ".." + cAlphaFields(3) + " in error = " + state.dataAirflowNetwork->MultizoneZoneData(i).VentSchName);
                    ShowContinueError(state,
                                      "..for " + cAlphaFields(1) + " = \"" + state.dataAirflowNetwork->MultizoneZoneData(i).ZoneName + "\", with " +
                                          cAlphaFields(2) + " = \"" + state.dataAirflowNetwork->MultizoneZoneData(i).VentControl + "\"");
                    ErrorsFound = true;
                }
            } else {
                state.dataAirflowNetwork->MultizoneZoneData(i).VentSchNum =
                    GetScheduleIndex(state, state.dataAirflowNetwork->MultizoneZoneData(i).VentSchName);
                if (state.dataAirflowNetwork->MultizoneZoneData(i).VentSchNum > 0) {
                    ShowWarningError(state,
                                     std::string{RoutineName} + CurrentModuleObject + " object, " + cAlphaFields(3) + " not required, when " + cAlphaFields(2) +
                                         " is neither Temperature nor Enthalpy.");
                    ShowContinueError(state, ".." + cAlphaFields(3) + " specified = " + state.dataAirflowNetwork->MultizoneZoneData(i).VentSchName);
                    ShowContinueError(state,
                                      "..for " + cAlphaFields(1) + " = \"" + state.dataAirflowNetwork->MultizoneZoneData(i).ZoneName + "\", with " +
                                          cAlphaFields(2) + " = \"" + state.dataAirflowNetwork->MultizoneZoneData(i).VentControl + "\"");
                    state.dataAirflowNetwork->MultizoneZoneData(i).VentSchNum = 0;
                    state.dataAirflowNetwork->MultizoneZoneData(i).VentSchName = std::string();
                }
            }
            if (state.dataAirflowNetwork->MultizoneZoneData(i).OpenFactor > 1.0 || state.dataAirflowNetwork->MultizoneZoneData(i).OpenFactor < 0.0) {
                ShowWarningError(state, std::string{RoutineName} + CurrentModuleObject + " object, " + cNumericFields(1) + " is out of range [0.0,1.0]");
                ShowContinueError(
                    state, format("..Input value = {:.2R}, Value will be set to 1.0", state.dataAirflowNetwork->MultizoneZoneData(i).OpenFactor));
                state.dataAirflowNetwork->MultizoneZoneData(i).OpenFactor = 1.0;
            }

            {
                auto const SELECT_CASE_var(UtilityRoutines::MakeUPPERCase(state.dataAirflowNetwork->MultizoneZoneData(i).VentControl));
                if (SELECT_CASE_var == "TEMPERATURE") { // checks on Temperature control
                    if (state.dataAirflowNetwork->MultizoneZoneData(i).LowValueTemp < 0.0) {
                        ShowWarningError(state, std::string{RoutineName} + CurrentModuleObject + " object, " + cNumericFields(2) + " < 0.0");
                        ShowContinueError(
                            state,
                            format("..Input value = {:.1R}, Value will be set to 0.0", state.dataAirflowNetwork->MultizoneZoneData(i).LowValueTemp));
                        ShowContinueError(state, "..for " + cAlphaFields(1) + " = \"" + state.dataAirflowNetwork->MultizoneZoneData(i).ZoneName);
                        state.dataAirflowNetwork->MultizoneZoneData(i).LowValueTemp = 0.0;
                    }
                    if (state.dataAirflowNetwork->MultizoneZoneData(i).LowValueTemp >= 100.0) {
                        ShowWarningError(state, std::string{RoutineName} + CurrentModuleObject + " object, " + cNumericFields(2) + " >= 100.0");
                        ShowContinueError(state,
                                          format("..Input value = {:.1R}, Value will be reset to 0.0",
                                                 state.dataAirflowNetwork->MultizoneZoneData(i).LowValueTemp));
                        ShowContinueError(state, "..for " + cAlphaFields(1) + " = \"" + state.dataAirflowNetwork->MultizoneZoneData(i).ZoneName);
                        state.dataAirflowNetwork->MultizoneZoneData(i).LowValueTemp = 0.0;
                    }
                    if (state.dataAirflowNetwork->MultizoneZoneData(i).UpValueTemp <= state.dataAirflowNetwork->MultizoneZoneData(i).LowValueTemp) {
                        ShowWarningError(state, std::string{RoutineName} + CurrentModuleObject + " object, " + cNumericFields(3) + " <= " + cNumericFields(2));
                        ShowContinueError(state,
                                          format("..Input value for {} = {:.1R}, Value will be reset to 100.0",
                                                 cNumericFields(3),
                                                 state.dataAirflowNetwork->MultizoneZoneData(i).UpValueTemp));
                        ShowContinueError(state, "..for " + cAlphaFields(1) + " = \"" + state.dataAirflowNetwork->MultizoneZoneData(i).ZoneName);
                        state.dataAirflowNetwork->MultizoneZoneData(i).UpValueTemp = 100.0;
                    }

                } else if (SELECT_CASE_var == "ENTHALPY") { // checks for Enthalpy control
                    if (state.dataAirflowNetwork->MultizoneZoneData(i).LowValueEnth < 0.0) {
                        ShowWarningError(state, std::string{RoutineName} + CurrentModuleObject + " object, " + cNumericFields(4) + " < 0.0");
                        ShowContinueError(state,
                                          format("..Input value = {:.1R}, Value will be reset to 0.0",
                                                 state.dataAirflowNetwork->MultizoneZoneData(i).LowValueEnth));
                        ShowContinueError(state, "..for " + cAlphaFields(1) + " = \"" + state.dataAirflowNetwork->MultizoneZoneData(i).ZoneName);
                        state.dataAirflowNetwork->MultizoneZoneData(i).LowValueEnth = 0.0;
                    }
                    if (state.dataAirflowNetwork->MultizoneZoneData(i).LowValueEnth >= 300000.0) {
                        ShowWarningError(state, std::string{RoutineName} + CurrentModuleObject + " object, " + cNumericFields(4) + " >= 300000.0");
                        ShowContinueError(state,
                                          format("..Input value = {:.1R}, Value will be reset to 0.0.",
                                                 state.dataAirflowNetwork->MultizoneZoneData(i).LowValueEnth));
                        ShowContinueError(state, "..for " + cAlphaFields(1) + " = \"" + state.dataAirflowNetwork->MultizoneZoneData(i).ZoneName);
                        state.dataAirflowNetwork->MultizoneZoneData(i).LowValueEnth = 0.0;
                    }
                    if (state.dataAirflowNetwork->MultizoneZoneData(i).UpValueEnth <= state.dataAirflowNetwork->MultizoneZoneData(i).LowValueEnth) {
                        ShowWarningError(state, std::string{RoutineName} + CurrentModuleObject + " object, " + cNumericFields(5) + " <= " + cNumericFields(4));
                        ShowContinueError(state,
                                          format("..Input value for {}= {:.1R}, Value will be reset to 300000.0",
                                                 cNumericFields(5),
                                                 state.dataAirflowNetwork->MultizoneZoneData(i).UpValueEnth));
                        ShowContinueError(state, "..for " + cAlphaFields(1) + " = \"" + state.dataAirflowNetwork->MultizoneZoneData(i).ZoneName);
                        state.dataAirflowNetwork->MultizoneZoneData(i).UpValueEnth = 300000.0;
                    }
                } else if (SELECT_CASE_var == "ASHRAE55ADAPTIVE") {
                    // Check that for the given zone, there is a people object for which ASHRAE 55 calculations are carried out
                    ZoneNum = state.dataAirflowNetwork->MultizoneZoneData(i).ZoneNum;
                    for (j = 1; j <= state.dataHeatBal->TotPeople; ++j) {
                        if (ZoneNum == state.dataHeatBal->People(j).ZonePtr && state.dataHeatBal->People(j).AdaptiveASH55) {
                            state.dataAirflowNetwork->MultizoneZoneData(i).ASH55PeopleInd = j;
                        }
                    }
                    if (state.dataAirflowNetwork->MultizoneZoneData(i).ASH55PeopleInd == 0) {
                        ShowFatalError(state,
                                       "ASHRAE55 ventilation control for zone " + state.dataAirflowNetwork->MultizoneZoneData(i).ZoneName +
                                           " requires a people object with respective model calculations.");
                    }
                } else if (SELECT_CASE_var == "CEN15251ADAPTIVE") {
                    // Check that for the given zone, there is a people object for which CEN-15251 calculations are carried out
                    ZoneNum = state.dataAirflowNetwork->MultizoneZoneData(i).ZoneNum;
                    for (j = 1; j <= state.dataHeatBal->TotPeople; ++j) {
                        if (ZoneNum == state.dataHeatBal->People(j).ZonePtr && state.dataHeatBal->People(j).AdaptiveCEN15251) {
                            state.dataAirflowNetwork->MultizoneZoneData(i).CEN15251PeopleInd = j;
                            break;
                        }
                    }
                    if (state.dataAirflowNetwork->MultizoneZoneData(i).CEN15251PeopleInd == 0) {
                        ShowFatalError(state,
                                       "CEN15251 ventilation control for zone " + state.dataAirflowNetwork->MultizoneZoneData(i).ZoneName +
                                           " requires a people object with respective model calculations.");
                    }
                } else {
                }
            }
        }

        // *** Read AirflowNetwork external node
        if (state.dataAirflowNetwork->AirflowNetworkSimu.iWPCCnt == iWPCCntr::Input) {
            // Wind coefficient == Surface-Average does not need inputs of external nodes
            state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfExtNode =
                state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "AirflowNetwork:MultiZone:ExternalNode");
            if (state.dataGlobal->AnyLocalEnvironmentsInModel) {
                state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfOutAirNode =
                    state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "OutdoorAir:Node");
                state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfExtNode +=
                    state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfOutAirNode;
            }

            if (state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfExtNode > 0) {
                state.dataAirflowNetwork->MultizoneExternalNodeData.allocate(state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfExtNode);
                CurrentModuleObject = "AirflowNetwork:MultiZone:ExternalNode";
                for (int i = 1; i <= state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfExtNode -
                                         state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfOutAirNode;
                     ++i) {
                    state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                             CurrentModuleObject,
                                                                             i,
                                                                             Alphas,
                                                                             NumAlphas,
                                                                             Numbers,
                                                                             NumNumbers,
                                                                             IOStatus,
                                                                             lNumericBlanks,
                                                                             lAlphaBlanks,
                                                                             cAlphaFields,
                                                                             cNumericFields);
                    UtilityRoutines::IsNameEmpty(state, Alphas(1), CurrentModuleObject, ErrorsFound);
                    state.dataAirflowNetwork->MultizoneExternalNodeData(i).Name = Alphas(1);    // Name of external node
                    state.dataAirflowNetwork->MultizoneExternalNodeData(i).height = Numbers(1); // Nodal height
                    if (UtilityRoutines::SameString(state.dataAirflowNetwork->AirflowNetworkSimu.HeightOption, "ExternalNode") && lNumericBlanks(1)) {
                        ShowWarningError(state,
                                         std::string{RoutineName} + CurrentModuleObject + " object =" + Alphas(1) + ". The input of " + cNumericFields(1) +
                                             " is required, but a blank is found.");
                        ShowContinueError(state, format("The default value is assigned as {:.1R}", Numbers(1)));
                    }
                    state.dataAirflowNetwork->MultizoneExternalNodeData(i).ExtNum =
                        state.dataAirflowNetwork->AirflowNetworkNumOfZones + i; // External node number
                    state.dataAirflowNetwork->MultizoneExternalNodeData(i).curve =
                        CurveManager::GetCurveIndex(state, Alphas(2)); // Wind pressure curve
                    if (state.dataAirflowNetwork->MultizoneExternalNodeData(i).curve == 0) {
                        ShowSevereError(state, std::string{RoutineName} + "Invalid " + cAlphaFields(2) + "=" + Alphas(2));
                        ShowContinueError(state, "Entered in " + CurrentModuleObject + '=' + Alphas(1));
                        ErrorsFound = true;
                    }
                    if (NumAlphas >= 3 && !lAlphaBlanks(3)) { // Symmetric curve
                        if (UtilityRoutines::SameString(Alphas(3), "Yes")) {
                            state.dataAirflowNetwork->MultizoneExternalNodeData(i).symmetricCurve = true;
                        } else if (!UtilityRoutines::SameString(Alphas(3), "No")) {
                            ShowWarningError(state,
                                             std::string{RoutineName} + CurrentModuleObject + " object, Invalid input " + cAlphaFields(3) + " = " + Alphas(3));
                            ShowContinueError(state, "The default value is assigned as No.");
                        }
                    }
                    if (NumAlphas == 4 && !lAlphaBlanks(4)) { // Relative or absolute wind angle
                        if (UtilityRoutines::SameString(Alphas(4), "Relative")) {
                            state.dataAirflowNetwork->MultizoneExternalNodeData(i).useRelativeAngle = true;
                        } else if (!UtilityRoutines::SameString(Alphas(4), "Absolute")) {
                            ShowWarningError(state,
                                             std::string{RoutineName} + CurrentModuleObject + " object, Invalid input " + cAlphaFields(4) + " = " + Alphas(4));
                            ShowContinueError(state, "The default value is assigned as Absolute.");
                        }
                    }
                }
                if (state.dataGlobal->AnyLocalEnvironmentsInModel) {

                    CurrentModuleObject = "OutdoorAir:Node";
                    for (int i = state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfExtNode -
                                 state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfOutAirNode + 1;
                         i <= state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfExtNode;
                         ++i) {
                        state.dataInputProcessing->inputProcessor->getObjectItem(
                            state,
                            CurrentModuleObject,
                            i - (state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfExtNode -
                                 state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfOutAirNode),
                            Alphas,
                            NumAlphas,
                            Numbers,
                            NumNumbers,
                            IOStatus,
                            lNumericBlanks,
                            lAlphaBlanks,
                            cAlphaFields,
                            cNumericFields);
                        UtilityRoutines::IsNameEmpty(state, Alphas(1), CurrentModuleObject, ErrorsFound);
                        // HACK: Need to verify name is unique between "OutdoorAir:Node" and "AirflowNetwork:MultiZone:ExternalNode"

                        if (NumAlphas > 5 && !lAlphaBlanks(6)) { // Wind pressure curve
                            state.dataAirflowNetwork->MultizoneExternalNodeData(i).curve = GetCurveIndex(state, Alphas(6));
                            if (state.dataAirflowNetwork->MultizoneExternalNodeData(i).curve == 0) {
                                ShowSevereError(state, std::string{RoutineName} + "Invalid " + cAlphaFields(6) + "=" + Alphas(6));
                                ShowContinueError(state, "Entered in " + CurrentModuleObject + '=' + Alphas(1));
                                ErrorsFound = true;
                            }
                        }

                        if (NumAlphas > 6 && !lAlphaBlanks(7)) { // Symmetric curve
                            if (UtilityRoutines::SameString(Alphas(7), "Yes")) {
                                state.dataAirflowNetwork->MultizoneExternalNodeData(i).symmetricCurve = true;
                            } else if (!UtilityRoutines::SameString(Alphas(7), "No")) {
                                ShowWarningError(state,
                                                 std::string{RoutineName} + CurrentModuleObject + " object, Invalid input " + cAlphaFields(7) + " = " + Alphas(7));
                                ShowContinueError(state, "The default value is assigned as No.");
                            }
                        }

                        if (NumAlphas > 7 && !lAlphaBlanks(8)) { // Relative or absolute wind angle
                            if (UtilityRoutines::SameString(Alphas(8), "Relative")) {
                                state.dataAirflowNetwork->MultizoneExternalNodeData(i).useRelativeAngle = true;
                            } else if (!UtilityRoutines::SameString(Alphas(8), "Absolute")) {
                                ShowWarningError(state,
                                                 std::string{RoutineName} + CurrentModuleObject + " object, Invalid input " + cAlphaFields(8) + " = " + Alphas(8));
                                ShowContinueError(state, "The default value is assigned as Absolute.");
                            }
                        }

                        state.dataAirflowNetwork->MultizoneExternalNodeData(i).Name = Alphas(1); // Name of external node
                        NodeNum = GetOnlySingleNode(state,
                                                    Alphas(1),
                                                    ErrorsFound,
                                                    CurrentModuleObject,
                                                    "AirflowNetwork:Multizone:Surface",
                                                    DataLoopNode::NodeFluidType::Air,
                                                    DataLoopNode::NodeConnectionType::Inlet,
                                                    1,
                                                    ObjectIsParent);
                        state.dataAirflowNetwork->MultizoneExternalNodeData(i).OutAirNodeNum = NodeNum;       // Name of outdoor air node
                        state.dataAirflowNetwork->MultizoneExternalNodeData(i).height = Node(NodeNum).Height; // Nodal height
                        state.dataAirflowNetwork->MultizoneExternalNodeData(i).ExtNum =
                            state.dataAirflowNetwork->AirflowNetworkNumOfZones + i; // External node number
                    }
                }
            } else {
                ShowSevereError(state,
                                std::string{RoutineName} + "An " + CurrentModuleObject +
                                    " object is required but not found when Wind Pressure Coefficient Type = Input.");
                ErrorsFound = true;
            }
        }

        // *** Read AirflowNetwork element data
        ErrorsFound = ErrorsFound || !getAirflowElementInput(state);

        // *** Read AirflowNetwork simulation surface data
        CurrentModuleObject = "AirflowNetwork:MultiZone:Surface";
        state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
        if (state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces > 0) {
            state.dataAirflowNetwork->MultizoneSurfaceData.allocate(state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces);
            for (int i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces; ++i) {
                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         CurrentModuleObject,
                                                                         i,
                                                                         Alphas,
                                                                         NumAlphas,
                                                                         Numbers,
                                                                         NumNumbers,
                                                                         IOStatus,
                                                                         lNumericBlanks,
                                                                         lAlphaBlanks,
                                                                         cAlphaFields,
                                                                         cNumericFields);
                UtilityRoutines::IsNameEmpty(state, Alphas(1), CurrentModuleObject, ErrorsFound);
                state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfName = Alphas(1);    // Name of Associated EnergyPlus surface
                state.dataAirflowNetwork->MultizoneSurfaceData(i).OpeningName = Alphas(2); // Name of crack or opening component,
                // either simple or detailed large opening, or crack
                state.dataAirflowNetwork->MultizoneSurfaceData(i).ExternalNodeName = Alphas(3); // Name of external node, but not used at WPC="INPUT"
                if (UtilityRoutines::FindItemInList(Alphas(3), state.dataAirflowNetwork->MultizoneExternalNodeData) &&
                    state.dataAirflowNetwork
                            ->MultizoneExternalNodeData(
                                UtilityRoutines::FindItemInList(Alphas(3), state.dataAirflowNetwork->MultizoneExternalNodeData))
                            .curve == 0) {
                    ShowSevereError(state, std::string{RoutineName} + "Invalid " + cAlphaFields(3) + "=" + Alphas(3));
                    ShowContinueError(state,
                                      "A valid wind pressure coefficient curve name is required but not found when Wind Pressure "
                                      "Coefficient Type = Input.");
                    ErrorsFound = true;
                }
                state.dataAirflowNetwork->MultizoneSurfaceData(i).Factor = Numbers(1); // Crack Actual Value or Window Open Factor for Ventilation
                if (state.dataAirflowNetwork->MultizoneSurfaceData(i).Factor > 1.0 ||
                    state.dataAirflowNetwork->MultizoneSurfaceData(i).Factor <= 0.0) {
                    ShowWarningError(state,
                                     std::string{RoutineName} + CurrentModuleObject + " object=" + state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfName +
                                         ", " + cNumericFields(1) + " is out of range (0.0,1.0]");
                    ShowContinueError(
                        state, format("..Input value = {:.2R}, Value will be set to 1.0", state.dataAirflowNetwork->MultizoneSurfaceData(i).Factor));
                    state.dataAirflowNetwork->MultizoneSurfaceData(i).Factor = 1.0;
                }
                // Get input of ventilation control and associated data
                if (NumAlphas >= 4) {
                    // Ventilation Control Mode: "TEMPERATURE", "ENTHALPY",
                    //   "CONSTANT", "ZONELEVEL", "NOVENT", "ADJACENTTEMPERATURE",
                    //   or "ADJACENTENTHALPY"
                    if (!lAlphaBlanks(4)) state.dataAirflowNetwork->MultizoneSurfaceData(i).VentControl = Alphas(4);
                    // Name of ventilation temperature control schedule
                    if (!lAlphaBlanks(5)) state.dataAirflowNetwork->MultizoneSurfaceData(i).VentSchName = Alphas(5);
                    {
                        auto const SELECT_CASE_var(UtilityRoutines::MakeUPPERCase(state.dataAirflowNetwork->MultizoneSurfaceData(i).VentControl));
                        if (SELECT_CASE_var == "TEMPERATURE") {
                            state.dataAirflowNetwork->MultizoneSurfaceData(i).VentSurfCtrNum = VentControlType::Temp;
                            state.dataAirflowNetwork->MultizoneSurfaceData(i).IndVentControl = true;
                        } else if (SELECT_CASE_var == "ENTHALPY") {
                            state.dataAirflowNetwork->MultizoneSurfaceData(i).VentSurfCtrNum = VentControlType::Enth;
                            state.dataAirflowNetwork->MultizoneSurfaceData(i).IndVentControl = true;
                        } else if (SELECT_CASE_var == "CONSTANT") {
                            state.dataAirflowNetwork->MultizoneSurfaceData(i).VentSurfCtrNum = VentControlType::Const;
                            state.dataAirflowNetwork->MultizoneSurfaceData(i).IndVentControl = true;
                        } else if (SELECT_CASE_var == "ASHRAE55ADAPTIVE") {
                            state.dataAirflowNetwork->MultizoneSurfaceData(i).VentSurfCtrNum = VentControlType::ASH55;
                            state.dataAirflowNetwork->MultizoneSurfaceData(i).IndVentControl = true;
                        } else if (SELECT_CASE_var == "CEN15251ADAPTIVE") {
                            state.dataAirflowNetwork->MultizoneSurfaceData(i).VentSurfCtrNum = VentControlType::CEN15251;
                            state.dataAirflowNetwork->MultizoneSurfaceData(i).IndVentControl = true;
                        } else if (SELECT_CASE_var == "NOVENT") {
                            state.dataAirflowNetwork->MultizoneSurfaceData(i).VentSurfCtrNum = VentControlType::NoVent;
                            state.dataAirflowNetwork->MultizoneSurfaceData(i).IndVentControl = true;
                        } else if (SELECT_CASE_var == "ZONELEVEL") {
                            state.dataAirflowNetwork->MultizoneSurfaceData(i).VentSurfCtrNum = VentControlType::ZoneLevel;
                            state.dataAirflowNetwork->MultizoneSurfaceData(i).IndVentControl = false;
                        } else if (SELECT_CASE_var == "ADJACENTTEMPERATURE") {
                            state.dataAirflowNetwork->MultizoneSurfaceData(i).VentSurfCtrNum = VentControlType::AdjTemp;
                            state.dataAirflowNetwork->MultizoneSurfaceData(i).IndVentControl = true;
                        } else if (SELECT_CASE_var == "ADJACENTENTHALPY") {
                            state.dataAirflowNetwork->MultizoneSurfaceData(i).VentSurfCtrNum = VentControlType::AdjEnth;
                            state.dataAirflowNetwork->MultizoneSurfaceData(i).IndVentControl = true;
                        } else {
                            ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + " object, Invalid " + cAlphaFields(4));
                            ShowContinueError(state,
                                              ".." + cAlphaFields(1) + " = " + state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfName +
                                                  ", Specified " + cAlphaFields(4) + " = " + Alphas(4));
                            ShowContinueError(state,
                                              "..The valid choices are \"Temperature\", \"Enthalpy\", \"Constant\", \"NoVent\", \"ZoneLevel\", "
                                              "\"AdjancentTemperature\" or \"AdjacentEnthalpy\"");
                            ErrorsFound = true;
                        }
                    }
                }
                state.dataAirflowNetwork->MultizoneSurfaceData(i).ModulateFactor =
                    Numbers(2); // Limit Value on Multiplier for Modulating Venting Open Factor
                state.dataAirflowNetwork->MultizoneSurfaceData(i).LowValueTemp =
                    Numbers(3); // Lower temperature value for modulation of temperature control
                state.dataAirflowNetwork->MultizoneSurfaceData(i).UpValueTemp =
                    Numbers(4); // Upper temperature value for modulation of temperature control
                state.dataAirflowNetwork->MultizoneSurfaceData(i).LowValueEnth =
                    Numbers(5);                                                             // Lower Enthalpy value for modulation of Enthalpy control
                state.dataAirflowNetwork->MultizoneSurfaceData(i).UpValueEnth = Numbers(6); // Lower Enthalpy value for modulation of Enthalpy control
                if (state.dataAirflowNetwork->MultizoneSurfaceData(i).VentSurfCtrNum < 4 ||
                    state.dataAirflowNetwork->MultizoneSurfaceData(i).VentSurfCtrNum == VentControlType::AdjTemp ||
                    state.dataAirflowNetwork->MultizoneSurfaceData(i).VentSurfCtrNum == VentControlType::AdjEnth) {
                    if (!lAlphaBlanks(6)) {
                        state.dataAirflowNetwork->MultizoneSurfaceData(i).VentingSchName = Alphas(6); // Name of ventilation availability schedule
                    }
                }
                if (!lAlphaBlanks(7)) {
                    state.dataAirflowNetwork->MultizoneSurfaceData(i).OccupantVentilationControlName = Alphas(7);
                    state.dataAirflowNetwork->MultizoneSurfaceData(i).OccupantVentilationControlNum =
                        UtilityRoutines::FindItemInList(state.dataAirflowNetwork->MultizoneSurfaceData(i).OccupantVentilationControlName,
                                                        state.dataAirflowNetworkBalanceManager->OccupantVentilationControl);
                    if (state.dataAirflowNetwork->MultizoneSurfaceData(i).OccupantVentilationControlNum == 0) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + " object, " + cAlphaFields(7) +
                                            " not found = " + state.dataAirflowNetwork->MultizoneSurfaceData(i).OccupantVentilationControlName);
                        ShowContinueError(state, "..for specified " + cAlphaFields(1) + " = " + Alphas(1));
                        ErrorsFound = true;
                    }
                }
                // Get data of polygonal surface
                if (!lAlphaBlanks(8)) {
                    if (Alphas(8) == "POLYGONHEIGHT") {
                        state.dataAirflowNetwork->MultizoneSurfaceData(i).EquivRecMethod = EquivRec::Height;
                    } else if (Alphas(8) == "BASESURFACEASPECTRATIO") {
                        state.dataAirflowNetwork->MultizoneSurfaceData(i).EquivRecMethod = EquivRec::BaseAspectRatio;
                    } else if (Alphas(8) == "USERDEFINEDASPECTRATIO") {
                        state.dataAirflowNetwork->MultizoneSurfaceData(i).EquivRecMethod = EquivRec::UserAspectRatio;
                    } else {
                        ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + " object, Invalid " + cAlphaFields(8));
                        ShowContinueError(state,
                                          ".." + cAlphaFields(1) + " = " + state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfName +
                                              ", Specified " + cAlphaFields(8) + " = " + Alphas(8));
                        ShowContinueError(state,
                                          "..The valid choices are \"PolygonHeight\", \"BaseSurfaceAspectRatio\", or \"UserDefinedAspectRatio\"");
                        ErrorsFound = true;
                    }
                } else {
                    state.dataAirflowNetwork->MultizoneSurfaceData(i).EquivRecMethod = EquivRec::Height;
                }
                if (!lNumericBlanks(7)) {
                    state.dataAirflowNetwork->MultizoneSurfaceData(i).EquivRecUserAspectRatio = Numbers(7);
                } else {
                    state.dataAirflowNetwork->MultizoneSurfaceData(i).EquivRecUserAspectRatio = 1.0;
                }
            }
        } else {
            ShowSevereError(state, std::string{RoutineName} + "An " + CurrentModuleObject + " object is required but not found.");
            ErrorsFound = true;
        }

        // remove extra OutdoorAir:Node, not assigned to External Node Name
        if (state.dataGlobal->AnyLocalEnvironmentsInModel && state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfOutAirNode > 0) {
            for (int i = state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfExtNode -
                         state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfOutAirNode + 1;
                 i <= state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfExtNode;
                 ++i) {
                found = false;
                for (j = 1; j <= state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces; ++j) {
                    if (UtilityRoutines::SameString(state.dataAirflowNetwork->MultizoneSurfaceData(j).ExternalNodeName,
                                                    state.dataAirflowNetwork->MultizoneExternalNodeData(i).Name)) {
                        found = true;
                    }
                }
                if (!found) {
                    if (i < state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfExtNode) {
                        for (k = i; k <= state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfExtNode - 1; ++k) {
                            state.dataAirflowNetwork->MultizoneExternalNodeData(k).Name =
                                state.dataAirflowNetwork->MultizoneExternalNodeData(k + 1).Name;
                            state.dataAirflowNetwork->MultizoneExternalNodeData(k).OutAirNodeNum =
                                state.dataAirflowNetwork->MultizoneExternalNodeData(k + 1).OutAirNodeNum;
                            state.dataAirflowNetwork->MultizoneExternalNodeData(k).height =
                                state.dataAirflowNetwork->MultizoneExternalNodeData(k + 1).height;
                            state.dataAirflowNetwork->MultizoneExternalNodeData(k).ExtNum =
                                state.dataAirflowNetwork->MultizoneExternalNodeData(k + 1).ExtNum - 1;
                        }
                        i -= 1;
                    }
                    state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfOutAirNode -= 1;
                    state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfExtNode -= 1;
                    state.dataAirflowNetwork->MultizoneExternalNodeData.redimension(
                        state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfExtNode);
                }
            }
        }

        // ==> Validate AirflowNetwork simulation surface data
        state.dataAirflowNetworkBalanceManager->NumOfExtNodes = 0;
        for (int i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces; ++i) {
            // Check a valid surface defined earlier
            state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum =
                UtilityRoutines::FindItemInList(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfName, state.dataSurface->Surface);
            if (state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum == 0) {
                ShowSevereError(state,
                                std::string{RoutineName} + CurrentModuleObject + " object, Invalid " + cAlphaFields(1) +
                                    " given = " + state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfName);
                ShowFatalError(state, std::string{RoutineName} + "Errors found getting inputs. Previous error(s) cause program termination.");
            }
            if (!state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).HeatTransSurf &&
                !state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).IsAirBoundarySurf) {
                ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + " object");
                ShowContinueError(state,
                                  "..The surface specified must be a heat transfer surface. Invalid " + cAlphaFields(1) + " = " +
                                      state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfName);
                ErrorsFound = true;
                continue;
            }
            // Ensure an interior surface does not face itself
            if (state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).ExtBoundCond >= 1) {
                // Check the surface is a subsurface or not
                if (state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).BaseSurf ==
                    state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum) {
                    if (state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum ==
                        state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).ExtBoundCond) {
                        ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + " object");
                        ShowContinueError(state,
                                          "..The surface facing itself is not allowed. Invalid " + cAlphaFields(1) + " = " +
                                              state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfName);
                        ErrorsFound = true;
                    }
                } else {
                    if (state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).BaseSurf ==
                        state.dataSurface->Surface(state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).BaseSurf)
                            .ExtBoundCond) {
                        ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + " object");
                        ShowContinueError(state,
                                          "..The base surface facing itself is not allowed. Invalid " + cAlphaFields(1) + " = " +
                                              state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfName);
                        ErrorsFound = true;
                    }
                }
            }
            // Ensure zones defined in inside and outside environment are used in the object of AIRFLOWNETWORK:MULTIZONE:ZONE
            found = false;
            n = state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).Zone;
            for (j = 1; j <= state.dataAirflowNetwork->AirflowNetworkNumOfZones; ++j) {
                if (state.dataAirflowNetwork->MultizoneZoneData(j).ZoneNum == n) {
                    found = true;
                    break;
                }
            }
            // find a surface geometry
            state.dataAirflowNetwork->MultizoneSurfaceData(i).Height =
                state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).Height;
            state.dataAirflowNetwork->MultizoneSurfaceData(i).Width =
                state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).Width;
            state.dataAirflowNetwork->MultizoneSurfaceData(i).CHeight =
                state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).Centroid.z;
            if (found) {
                state.dataAirflowNetwork->MultizoneSurfaceData(i).NodeNums[0] = j;
            } else {
                ShowSevereError(state,
                                std::string{RoutineName} + CurrentModuleObject + " object, " + cAlphaFields(1) + " = " +
                                    state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfName);
                ShowContinueError(state,
                                  "..Zone for inside surface must be defined in a AirflowNetwork:MultiZone:Zone object.  Could not find Zone = " +
                                      Zone(state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).Zone).Name);
                ShowFatalError(state, std::string{RoutineName} + "Errors found getting inputs. Previous error(s) cause program termination.");
            }

            // Calculate equivalent width and height
            if (state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).Sides != 4) {
                state.dataAirflowNetwork->MultizoneSurfaceData(i).NonRectangular = true;
                if (state.dataAirflowNetwork->MultizoneSurfaceData(i).EquivRecMethod == EquivRec::Height) {
                    if (state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).Tilt < 1.0 ||
                        state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).Tilt > 179.0) { // horizontal surface
                        // check base surface shape
                        if (state.dataSurface->Surface(state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).BaseSurf)
                                .Sides == 4) {
                            baseratio = state.dataSurface
                                            ->Surface(state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).BaseSurf)
                                            .Width /
                                        state.dataSurface
                                            ->Surface(state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).BaseSurf)
                                            .Height;
                            state.dataAirflowNetwork->MultizoneSurfaceData(i).Width =
                                sqrt(state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).Area * baseratio);
                            state.dataAirflowNetwork->MultizoneSurfaceData(i).Height =
                                state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).Area /
                                state.dataAirflowNetwork->MultizoneSurfaceData(i).Width;
                            if (state.dataGlobal->DisplayExtraWarnings) {
                                ShowWarningError(state,
                                                 std::string{RoutineName} + CurrentModuleObject +
                                                     " object = " + state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfName);
                                ShowContinueError(state,
                                                  "The entered choice of Equivalent Rectangle Method is PolygonHeight. This choice is not valid for "
                                                  "a horizontal surface.");
                                ShowContinueError(state, "The BaseSurfaceAspectRatio choice is used. Simulation continues.");
                            }
                        } else {
                            state.dataAirflowNetwork->MultizoneSurfaceData(i).Width =
                                sqrt(state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).Area *
                                     state.dataAirflowNetwork->MultizoneSurfaceData(i).EquivRecUserAspectRatio);
                            state.dataAirflowNetwork->MultizoneSurfaceData(i).Height =
                                state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).Area /
                                state.dataAirflowNetwork->MultizoneSurfaceData(i).Width;
                            // add warning
                            if (state.dataGlobal->DisplayExtraWarnings) {
                                ShowWarningError(state,
                                                 std::string{RoutineName} + CurrentModuleObject +
                                                     " object = " + state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfName);
                                ShowContinueError(state,
                                                  "The entered choice of Equivalent Rectangle Method is PolygonHeight. This choice is not valid for "
                                                  "a horizontal surface with a polygonal base surface.");
                                ShowContinueError(state, "The default aspect ratio at 1 is used. Simulation continues.");
                            }
                        }
                    } else {
                        minHeight = min(state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).Vertex(1).z,
                                        state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).Vertex(2).z);
                        maxHeight = max(state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).Vertex(1).z,
                                        state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).Vertex(2).z);
                        for (j = 3; j <= state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).Sides; ++j) {
                            minHeight = min(minHeight,
                                            min(state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).Vertex(j - 1).z,
                                                state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).Vertex(j).z));
                            maxHeight = max(maxHeight,
                                            max(state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).Vertex(j - 1).z,
                                                state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).Vertex(j).z));
                        }
                        if (maxHeight > minHeight) {
                            state.dataAirflowNetwork->MultizoneSurfaceData(i).Height = maxHeight - minHeight;
                            state.dataAirflowNetwork->MultizoneSurfaceData(i).Width =
                                state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).Area / (maxHeight - minHeight);
                        }
                    }
                }
                if (state.dataAirflowNetwork->MultizoneSurfaceData(i).EquivRecMethod == EquivRec::BaseAspectRatio) {
                    if (state.dataSurface->Surface(state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).BaseSurf)
                            .Sides == 4) {
                        baseratio =
                            state.dataSurface->Surface(state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).BaseSurf)
                                .Width /
                            state.dataSurface->Surface(state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).BaseSurf)
                                .Height;
                        state.dataAirflowNetwork->MultizoneSurfaceData(i).Width =
                            sqrt(state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).Area * baseratio);
                        state.dataAirflowNetwork->MultizoneSurfaceData(i).Height =
                            state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).Area /
                            state.dataAirflowNetwork->MultizoneSurfaceData(i).Width;
                    } else {
                        minHeight = min(state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).Vertex(1).z,
                                        state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).Vertex(2).z);
                        maxHeight = max(state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).Vertex(1).z,
                                        state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).Vertex(2).z);
                        for (j = 3; j <= state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).Sides; ++j) {
                            minHeight = min(minHeight,
                                            min(state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).Vertex(j - 1).z,
                                                state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).Vertex(j).z));
                            maxHeight = max(maxHeight,
                                            max(state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).Vertex(j - 1).z,
                                                state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).Vertex(j).z));
                        }
                        if (maxHeight > minHeight) {
                            state.dataAirflowNetwork->MultizoneSurfaceData(i).Height = maxHeight - minHeight;
                            state.dataAirflowNetwork->MultizoneSurfaceData(i).Width =
                                state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).Area / (maxHeight - minHeight);
                            // add warning
                            if (state.dataGlobal->DisplayExtraWarnings) {
                                ShowWarningError(state,
                                                 std::string{RoutineName} + CurrentModuleObject +
                                                     " object = " + state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfName);
                                ShowContinueError(state,
                                                  "The entered choice of Equivalent Rectangle Method is BaseSurfaceAspectRatio. This choice is not "
                                                  "valid for a polygonal base surface.");
                                ShowContinueError(state, "The PolygonHeight choice is used. Simulation continues.");
                            }
                        } else {
                            state.dataAirflowNetwork->MultizoneSurfaceData(i).Width =
                                sqrt(state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).Area *
                                     state.dataAirflowNetwork->MultizoneSurfaceData(i).EquivRecUserAspectRatio);
                            state.dataAirflowNetwork->MultizoneSurfaceData(i).Height =
                                state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).Area /
                                state.dataAirflowNetwork->MultizoneSurfaceData(i).Width;
                            // add warning
                            if (state.dataGlobal->DisplayExtraWarnings) {
                                ShowWarningError(state,
                                                 std::string{RoutineName} + CurrentModuleObject +
                                                     " object = " + state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfName);
                                ShowContinueError(state,
                                                  "The entered choice of Equivalent Rectangle Method is BaseSurfaceAspectRatio. This choice is not "
                                                  "valid for a horizontal surface with a polygonal base surface.");
                                ShowContinueError(state, "The default aspect ratio at 1 is used. Simulation continues.");
                            }
                        }
                    }
                }
                if (state.dataAirflowNetwork->MultizoneSurfaceData(i).EquivRecMethod == EquivRec::UserAspectRatio) {
                    state.dataAirflowNetwork->MultizoneSurfaceData(i).Width =
                        sqrt(state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).Area *
                             state.dataAirflowNetwork->MultizoneSurfaceData(i).EquivRecUserAspectRatio);
                    state.dataAirflowNetwork->MultizoneSurfaceData(i).Height =
                        state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).Area /
                        state.dataAirflowNetwork->MultizoneSurfaceData(i).Width;
                }
            }

            // Get the number of external surfaces
            if (state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).ExtBoundCond == ExternalEnvironment ||
                (state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).ExtBoundCond == OtherSideCoefNoCalcExt &&
                 state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).ExtWind)) {
                ++state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfExtSurfaces;
            }

            // Outside face environment
            if (state.dataAirflowNetwork->AirflowNetworkSimu.iWPCCnt == iWPCCntr::Input) {
                n = state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).ExtBoundCond;
                if (n == ExternalEnvironment ||
                    (n == OtherSideCoefNoCalcExt && state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).ExtWind)) {
                    ++state.dataAirflowNetworkBalanceManager->NumOfExtNodes;
                    if (state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfExtNode > 0) {
                        found = false;
                        for (j = 1; j <= state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfExtNode; ++j) {
                            if (UtilityRoutines::SameString(state.dataAirflowNetwork->MultizoneSurfaceData(i).ExternalNodeName,
                                                            state.dataAirflowNetwork->MultizoneExternalNodeData(j).Name)) {
                                state.dataAirflowNetwork->MultizoneSurfaceData(i).NodeNums[1] =
                                    state.dataAirflowNetwork->MultizoneExternalNodeData(j).ExtNum;
                                found = true;
                                break;
                            }
                        }
                        if (!found) {
                            ShowSevereError(state,
                                            std::string{RoutineName} + CurrentModuleObject + ": Invalid " + cAlphaFields(3) + " = " +
                                                state.dataAirflowNetwork->MultizoneSurfaceData(i).ExternalNodeName);
                            ShowContinueError(state, "A valid " + cAlphaFields(3) + " is required when Wind Pressure Coefficient Type = Input");
                            ErrorsFound = true;
                        }
                    } else {
                        //          state.dataAirflowNetwork->MultizoneSurfaceData(i)%NodeNums[1] =
                        //          state.dataAirflowNetwork->AirflowNetworkNumOfZones+NumOfExtNodes
                    }
                    continue;
                } else {
                    if (n < ExternalEnvironment &&
                        !(state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).ExtBoundCond ==
                              OtherSideCoefNoCalcExt &&
                          state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).ExtWind)) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + ": Invalid " + cAlphaFields(1) + " = " +
                                            state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfName);
                        ShowContinueError(state, "This type of surface (has ground, etc exposure) cannot be used in the AiflowNetwork model.");
                        ErrorsFound = true;
                    }
                }
                found = false;
                for (j = 1; j <= state.dataAirflowNetwork->AirflowNetworkNumOfZones; ++j) {
                    if (state.dataAirflowNetwork->MultizoneZoneData(j).ZoneNum == state.dataSurface->Surface(n).Zone) {
                        found = true;
                        break;
                    }
                }
                if (found) {
                    state.dataAirflowNetwork->MultizoneSurfaceData(i).NodeNums[1] = j;
                } else {
                    ShowSevereError(state,
                                    std::string{RoutineName} + CurrentModuleObject + " object, " + cAlphaFields(1) + " = " +
                                        state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfName);
                    ShowContinueError(
                        state,
                        "..Zone for outside surface must be defined in a AirflowNetwork:MultiZone:Zone object.  Could not find Zone = " +
                            Zone(state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).Zone).Name);
                    ErrorsFound = true;
                    continue;
                }
            }
            if (UtilityRoutines::SameString(state.dataAirflowNetwork->AirflowNetworkSimu.WPCCntr, "SurfaceAverageCalculation")) {
                n = state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).ExtBoundCond;
                if (n >= 1) { // exterior boundary condition is a surface
                    found = false;
                    for (j = 1; j <= state.dataAirflowNetwork->AirflowNetworkNumOfZones; ++j) {
                        if (state.dataAirflowNetwork->MultizoneZoneData(j).ZoneNum == state.dataSurface->Surface(n).Zone) {
                            found = true;
                            break;
                        }
                    }
                    if (found) {
                        state.dataAirflowNetwork->MultizoneSurfaceData(i).NodeNums[1] = j;
                    } else {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + " = " + state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfName);
                        ShowContinueError(state,
                                          "An adjacent zone = " + Zone(state.dataSurface->Surface(n).Zone).Name +
                                              " is not described in AIRFLOWNETWORK:MULTIZONE:ZONE");
                        ErrorsFound = true;
                        continue;
                    }
                }
            }
            if (!(state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).ExtBoundCond == -2 &&
                  state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).ExtWind)) {
                if (state.dataAirflowNetwork->MultizoneSurfaceData(i).NodeNums[1] == 0 &&
                    state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).ExtBoundCond < 0) {
                    ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + " = " + state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfName);
                    ShowContinueError(
                        state,
                        "Outside boundary condition and object are " +
                            cExtBoundCondition(state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).ExtBoundCond) +
                            " and " + state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).ExtBoundCondName + ".");
                    ShowContinueError(state, "The outside boundary condition must be exposed to either the outside or an adjacent zone.");
                    ErrorsFound = true;
                    continue;
                }
            }
        }

        // write outputs in eio file
        found = true;
        for (int i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces; ++i) {
            if (state.dataAirflowNetwork->MultizoneSurfaceData(i).NonRectangular) {
                if (found) {
                    print(state.files.eio,
                          "! <AirflowNetwork Model:Equivalent Rectangle Surface>, Name, Equivalent Height {{m}}, Equivalent Width {{m}} "
                          "AirflowNetwork "
                          "Model:Equivalent Rectangle\n");
                    found = false;
                }
                print(state.files.eio,
                      "AirflowNetwork Model:Equivalent Rectangle Surface, {}, {:.2R},{:.2R}\n",
                      state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfName,
                      state.dataAirflowNetwork->MultizoneSurfaceData(i).Height,
                      state.dataAirflowNetwork->MultizoneSurfaceData(i).Width);
            }
        }

        // Validate adjacent temperature and Enthalpy control for an interior surface only
        for (int i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces; ++i) {
            if (state.dataAirflowNetwork->MultizoneSurfaceData(i).VentSurfCtrNum == VentControlType::AdjTemp) {
                if (!(state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).ExtBoundCond >= 1)) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + CurrentModuleObject + " object, " + cAlphaFields(1) + " = " +
                                        state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfName);
                    ShowContinueError(state, "..AdjacentTemperature venting control must be defined for an interzone surface.");
                    ErrorsFound = true;
                }
            }
            if (state.dataAirflowNetwork->MultizoneSurfaceData(i).VentSurfCtrNum == VentControlType::AdjEnth) {
                if (!(state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).ExtBoundCond >= 1)) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + CurrentModuleObject + " object, " + cAlphaFields(1) + " = " +
                                        state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfName);
                    ShowContinueError(state, "..AdjacentEnthalpy venting control must be defined for an interzone surface.");
                    ErrorsFound = true;
                }
            }
        }

        // Ensure the number of external node = the number of external surface with HeightOption choice = OpeningHeight
        if (UtilityRoutines::SameString(state.dataAirflowNetwork->AirflowNetworkSimu.HeightOption, "OpeningHeight") &&
            state.dataAirflowNetwork->AirflowNetworkSimu.iWPCCnt == iWPCCntr::Input) {
            if (state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfExtSurfaces !=
                state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfExtNode) {
                ShowSevereError(state,
                                std::string{RoutineName} +
                                    "When the choice of Height Selection for Local Wind Speed Calculation is OpeningHeight, the number of external "
                                    "surfaces defined in " +
                                    CurrentModuleObject + " objects ");
                ShowContinueError(state, "has to be equal to the number of AirflowNetwork:MultiZone:ExternalNode objects.");
                ShowContinueError(state,
                                  format("The entered number of external nodes is {}. The entered number of external surfaces is {}.",
                                         state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfExtNode,
                                         state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfExtSurfaces));
                ErrorsFound = true;
            }
        }

        // Read AirflowNetwork simulation detailed openings
        // Moved into getAirflowElementInput

        // Validate opening component and assign opening dimension
        if (state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfDetOpenings > 0) {
            for (int i = 1; i <= state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfDetOpenings; ++i) {
                found = false;
                for (j = 1; j <= state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces; ++j) {
                    if (state.dataAirflowNetwork->MultizoneCompDetOpeningData(i).name ==
                        state.dataAirflowNetwork->MultizoneSurfaceData(j).OpeningName) {
                        //           state.dataAirflowNetwork->MultizoneCompDetOpeningData(i)%Width =
                        //           Surface(state.dataAirflowNetwork->MultizoneSurfaceData(j)%SurfNum)%Width
                        //           state.dataAirflowNetwork->MultizoneCompDetOpeningData(i)%Height =
                        //           Surface(state.dataAirflowNetwork->MultizoneSurfaceData(j)%SurfNum)%Height
                        found = true;
                    }
                }
            }
        }

        // Read AirflowNetwork simulation simple openings
        // Moved into getAirflowElementInput

        // Read AirflowNetwork simulation horizontal openings
        // Moved into getAirflowElementInput
        auto &solver = state.dataAFNSolver->solver;

        // Check status of control level for each surface with an opening
        j = 0;
        CurrentModuleObject = "AirflowNetwork:MultiZone:Surface";
        for (int i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces; ++i) {
            if (state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum == 0) continue;
            bool has_Opening{false};
            // This is terrible, should not do it this way
            auto afe = solver.elements.find(state.dataAirflowNetwork->MultizoneSurfaceData(i).OpeningName);
            if (afe != solver.elements.end()) {
                auto type = afe->second->type();
                has_Opening = (type == ComponentType::DOP) || (type == ComponentType::SOP) || (type == ComponentType::HOP);
            }
            // Obtain schedule number and check surface shape
            if (has_Opening) {
                if (state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).Sides == 3) {
                    ShowWarningError(state,
                                     std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfName + "\".");
                    ShowContinueError(state,
                                      "The opening is a Triangular subsurface. A rectangular subsurface will be used with equivalent "
                                      "width and height.");
                }
                // Venting controls are not allowed for an air boundary surface
                if ((state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).IsAirBoundarySurf) &&
                    (state.dataAirflowNetwork->MultizoneSurfaceData(i).VentSurfCtrNum != VentControlType::Const)) {
                    ShowWarningError(state,
                                     std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfName +
                                         "\" is an air boundary surface.");
                    ShowContinueError(state, "Ventilation Control Mode = " + Alphas(4) + " is not valid. Resetting to Constant.");
                    state.dataAirflowNetwork->MultizoneSurfaceData(i).VentSurfCtrNum = VentControlType::Const;
                    state.dataAirflowNetwork->MultizoneSurfaceData(i).IndVentControl = true;
                }
                if (!state.dataAirflowNetwork->MultizoneSurfaceData(i).VentingSchName.empty()) {
                    state.dataAirflowNetwork->MultizoneSurfaceData(i).VentingSchNum =
                        GetScheduleIndex(state, state.dataAirflowNetwork->MultizoneSurfaceData(i).VentingSchName);
                    if (state.dataAirflowNetwork->MultizoneSurfaceData(i).VentingSchNum == 0) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfName +
                                            "\", invalid schedule.");
                        ShowContinueError(state,
                                          "Venting Schedule not found=\"" + state.dataAirflowNetwork->MultizoneSurfaceData(i).VentingSchName + "\".");
                        ErrorsFound = true;
                    } else if (state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).IsAirBoundarySurf) {
                        ShowWarningError(state,
                                         std::string{RoutineName} + CurrentModuleObject + "=\"" + state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfName +
                                             "\" is an air boundary surface.");
                        ShowContinueError(state, "Venting Availability Schedule will be ignored, venting is always available.");
                        state.dataAirflowNetwork->MultizoneSurfaceData(i).VentingSchName = "";
                        state.dataAirflowNetwork->MultizoneSurfaceData(i).VentingSchNum = 0;
                    }
                } else {
                    state.dataAirflowNetwork->MultizoneSurfaceData(i).VentingSchName = "";
                    state.dataAirflowNetwork->MultizoneSurfaceData(i).VentingSchNum = 0;
                }
                {
                    auto const SELECT_CASE_var(state.dataAirflowNetwork->MultizoneSurfaceData(i).VentSurfCtrNum);
                    if ((SELECT_CASE_var == VentControlType::Temp) || (SELECT_CASE_var == VentControlType::AdjTemp)) {
                        state.dataAirflowNetwork->MultizoneSurfaceData(i).VentSchNum =
                            GetScheduleIndex(state, state.dataAirflowNetwork->MultizoneSurfaceData(i).VentSchName);
                        if (state.dataAirflowNetwork->MultizoneSurfaceData(i).VentSchName == std::string()) {
                            ShowSevereError(state,
                                            std::string{RoutineName} + CurrentModuleObject +
                                                " object, No Ventilation Schedule was found, but is required when ventilation control is "
                                                "Temperature.");
                            ShowContinueError(state, "..for Surface = \"" + state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfName + "\"");
                            ErrorsFound = true;
                        } else if (state.dataAirflowNetwork->MultizoneSurfaceData(i).VentSchNum == 0) {
                            ShowSevereError(state,
                                            std::string{RoutineName} + CurrentModuleObject +
                                                " object, Invalid Ventilation Schedule, required when ventilation control is Temperature.");
                            ShowContinueError(state, "..Schedule name in error = " + state.dataAirflowNetwork->MultizoneSurfaceData(i).VentSchName);
                            ShowContinueError(state, "..for Surface = \"" + state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfName + "\"");
                            ErrorsFound = true;
                        }
                        if (state.dataAirflowNetwork->MultizoneSurfaceData(i).LowValueTemp < 0.0) {
                            ShowWarningError(state, std::string{RoutineName} + CurrentModuleObject + " object, Low Temperature difference value < 0.0d0");
                            ShowContinueError(state,
                                              format("..Input value={:.1R}, Value will be reset to 0.0.",
                                                     state.dataAirflowNetwork->MultizoneSurfaceData(i).LowValueTemp));
                            ShowContinueError(state, "..for Surface = \"" + state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfName + "\"");
                            state.dataAirflowNetwork->MultizoneSurfaceData(i).LowValueTemp = 0.0;
                        }
                        if (state.dataAirflowNetwork->MultizoneSurfaceData(i).LowValueTemp >= 100.0) {
                            ShowWarningError(state, std::string{RoutineName} + CurrentModuleObject + " object, Low Temperature difference value >= 100.0d0");
                            ShowContinueError(state,
                                              format("..Input value = {:.1R}, Value will be reset to 0.0",
                                                     state.dataAirflowNetwork->MultizoneSurfaceData(i).LowValueTemp));
                            ShowContinueError(state, "..for Surface = \"" + state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfName + "\"");
                            state.dataAirflowNetwork->MultizoneZoneData(i).LowValueTemp = 0.0;
                        }
                        if (state.dataAirflowNetwork->MultizoneSurfaceData(i).UpValueTemp <=
                            state.dataAirflowNetwork->MultizoneSurfaceData(i).LowValueTemp) {
                            ShowWarningError(state,
                                             std::string{RoutineName} + CurrentModuleObject + " object, Upper Temperature <= Lower Temperature difference value.");
                            ShowContinueError(state,
                                              format("..Input value = {:.1R}, Value will be reset to 100.0",
                                                     state.dataAirflowNetwork->MultizoneSurfaceData(i).UpValueTemp));
                            ShowContinueError(state, "..for Surface = \"" + state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfName + "\"");
                            state.dataAirflowNetwork->MultizoneSurfaceData(i).UpValueTemp = 100.0;
                        }

                    } else if ((SELECT_CASE_var == VentControlType::Enth) || (SELECT_CASE_var == VentControlType::AdjEnth)) {
                        state.dataAirflowNetwork->MultizoneSurfaceData(i).VentSchNum =
                            GetScheduleIndex(state, state.dataAirflowNetwork->MultizoneSurfaceData(i).VentSchName);
                        if (state.dataAirflowNetwork->MultizoneSurfaceData(i).VentSchName == std::string()) {
                            ShowSevereError(state,
                                            std::string{RoutineName} + CurrentModuleObject +
                                                " object, No Ventilation Schedule was found, but is required when ventilation control is Enthalpy.");
                            ShowContinueError(state, "..for Surface = \"" + state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfName + "\"");
                            ErrorsFound = true;
                        } else if (state.dataAirflowNetwork->MultizoneSurfaceData(i).VentSchNum == 0) {
                            ShowSevereError(state,
                                            std::string{RoutineName} + CurrentModuleObject +
                                                " object, Invalid Ventilation Schedule, required when ventilation control is Enthalpy.");
                            ShowContinueError(state, "..Schedule name in error = " + state.dataAirflowNetwork->MultizoneSurfaceData(i).VentSchName);
                            ShowContinueError(state, "..for Surface = \"" + state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfName + "\"");
                            ErrorsFound = true;
                        }
                        if (state.dataAirflowNetwork->MultizoneSurfaceData(i).LowValueEnth < 0.0) {
                            ShowWarningError(state, std::string{RoutineName} + CurrentModuleObject + " object, Low Enthalpy difference value < 0.0d0");
                            ShowContinueError(state,
                                              format("..Input value = {:.1R}, Value will be reset to 0.0",
                                                     state.dataAirflowNetwork->MultizoneSurfaceData(i).LowValueEnth));
                            ShowContinueError(state, "..for Surface = \"" + state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfName + "\"");
                            state.dataAirflowNetwork->MultizoneSurfaceData(i).LowValueEnth = 0.0;
                        }
                        if (state.dataAirflowNetwork->MultizoneSurfaceData(i).LowValueEnth >= 300000.0) {
                            ShowWarningError(state, std::string{RoutineName} + CurrentModuleObject + " object, Low Enthalpy difference value >= 300000.0");
                            ShowContinueError(state,
                                              format("..Input value = {:.1R}, Value will be reset to 0.0",
                                                     state.dataAirflowNetwork->MultizoneSurfaceData(i).LowValueEnth));
                            ShowContinueError(state, "..for Surface = \"" + state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfName + "\"");
                            state.dataAirflowNetwork->MultizoneZoneData(i).LowValueEnth = 0.0;
                        }
                        if (state.dataAirflowNetwork->MultizoneSurfaceData(i).UpValueEnth <=
                            state.dataAirflowNetwork->MultizoneSurfaceData(i).LowValueEnth) {
                            ShowWarningError(state,
                                             std::string{RoutineName} + CurrentModuleObject + " object, Upper Enthalpy <= Lower Enthalpy difference value.");
                            ShowContinueError(state,
                                              format("..Input value = {:.1R}, Value will be set to 300000.0",
                                                     state.dataAirflowNetwork->MultizoneSurfaceData(i).UpValueEnth));
                            ShowContinueError(state, "..for Surface = \"" + state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfName + "\"");
                            state.dataAirflowNetwork->MultizoneSurfaceData(i).UpValueEnth = 300000.0;
                        }

                    } else if (SELECT_CASE_var == VentControlType::Const) {
                        state.dataAirflowNetwork->MultizoneSurfaceData(i).VentSchNum = 0;
                        state.dataAirflowNetwork->MultizoneSurfaceData(i).VentSchName = "";

                    } else if (SELECT_CASE_var == VentControlType::ASH55) {
                        state.dataAirflowNetwork->MultizoneSurfaceData(i).VentSchNum = 0;
                        state.dataAirflowNetwork->MultizoneSurfaceData(i).VentSchName = "";

                    } else if (SELECT_CASE_var == VentControlType::CEN15251) {
                        state.dataAirflowNetwork->MultizoneSurfaceData(i).VentSchNum = 0;
                        state.dataAirflowNetwork->MultizoneSurfaceData(i).VentSchName = "";

                    } else if (SELECT_CASE_var == VentControlType::NoVent) {
                        state.dataAirflowNetwork->MultizoneSurfaceData(i).VentSchNum = 0;
                        state.dataAirflowNetwork->MultizoneSurfaceData(i).VentSchName = "";

                    } else if (SELECT_CASE_var == VentControlType::ZoneLevel) {
                        state.dataAirflowNetwork->MultizoneSurfaceData(i).VentSchNum = 0;
                        state.dataAirflowNetwork->MultizoneSurfaceData(i).VentSchName = "";

                    } else {
                    }
                }
            }
        }

        // Validate opening component and assign opening dimension
        if (state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfSimOpenings > 0) {
            for (int i = 1; i <= state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfSimOpenings; ++i) {
                found = false;
                for (j = 1; j <= state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces; ++j) {
                    if (state.dataAirflowNetwork->MultizoneCompSimpleOpeningData(i).name ==
                        state.dataAirflowNetwork->MultizoneSurfaceData(j).OpeningName) {
                        //           state.dataAirflowNetwork->MultizoneCompSimpleOpeningData(i)%Width =
                        //           Surface(state.dataAirflowNetwork->MultizoneSurfaceData(j)%SurfNum)%Width
                        //           state.dataAirflowNetwork->MultizoneCompSimpleOpeningData(i)%Height =
                        //           Surface(state.dataAirflowNetwork->MultizoneSurfaceData(j)%SurfNum)%Height
                        found = true;
                    }
                }
            }
        }

        // Calculate CP values
        if (UtilityRoutines::SameString(state.dataAirflowNetwork->AirflowNetworkSimu.WPCCntr, "SurfaceAverageCalculation")) {
            state.dataAirflowNetworkBalanceManager->calculateWindPressureCoeffs(state);
            // Ensure automatic generation is OK
            n = 0;
            for (j = 1; j <= 5; ++j) {
                found = false;
                for (int i = 1; i <= state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfExtNode; ++i) {
                    if (state.dataAirflowNetwork->MultizoneExternalNodeData(i).facadeNum == j) {
                        found = true;
                        break;
                    }
                }
                if (found) ++n;
                if (j == 5 && (!found)) {
                    found = true;
                    if (state.dataGlobal->DisplayExtraWarnings) {
                        ShowWarningError(state,
                                         std::string{RoutineName} + "SurfaceAverageCalculation is entered for field = Wind Pressure Coefficient Type, but no roof "
                                                       "surface is defined using an AirflowNetwork:MultiZone:Surface object.");
                        ShowContinueError(state, "Reconsider if this is your modeling intent. Simulation continues.");
                    }
                }
            }
            if (n < 5 && state.dataGlobal->DisplayExtraWarnings) {
                ShowWarningError(state, std::string{RoutineName} + "SurfaceAverageCalculation is entered for field = Wind Pressure Coefficient Type.");
                ShowContinueError(state,
                                  "The AirflowNetwork model provides wind pressure coefficients for 4 vertical exterior orientations and "
                                  "1 horizontal roof.");
                ShowContinueError(state,
                                  format(" There are only {} exterior surface orientations defined in this input file using "
                                         "AirflowNetwork:MultiZone:Surface objects.",
                                         n));
                ShowContinueError(state, "Reconsider if this is your modeling intent. Simulation continues.");
            }
        }

        // Assign external node height
        if (UtilityRoutines::SameString(state.dataAirflowNetwork->AirflowNetworkSimu.WPCCntr, "SurfaceAverageCalculation") ||
            UtilityRoutines::SameString(state.dataAirflowNetwork->AirflowNetworkSimu.HeightOption, "OpeningHeight")) {
            for (int i = 1; i <= state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfExtNode; ++i) {
                for (j = 1; j <= state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces; ++j) {
                    if (state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(j).SurfNum).ExtBoundCond == ExternalEnvironment ||
                        (state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(j).SurfNum).ExtBoundCond ==
                             OtherSideCoefNoCalcExt &&
                         state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(j).SurfNum).ExtWind)) {
                        if (UtilityRoutines::SameString(state.dataAirflowNetwork->MultizoneSurfaceData(j).ExternalNodeName,
                                                        state.dataAirflowNetwork->MultizoneExternalNodeData(i).Name)) {
                            state.dataAirflowNetwork->MultizoneExternalNodeData(i).height =
                                state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(j).SurfNum).Centroid.z;
                            break;
                        }
                    }
                }
            }
        }

        // Assign external node azimuth, should consider combining this with the above to avoid the repeated search
        for (int i = 1; i <= state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfExtNode; ++i) {
            for (j = 1; j <= state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces; ++j) {
                if (state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(j).SurfNum).ExtBoundCond == ExternalEnvironment ||
                    (state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(j).SurfNum).ExtBoundCond == OtherSideCoefNoCalcExt &&
                     state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(j).SurfNum).ExtWind)) {
                    if (UtilityRoutines::SameString(state.dataAirflowNetwork->MultizoneSurfaceData(j).ExternalNodeName,
                                                    state.dataAirflowNetwork->MultizoneExternalNodeData(i).Name)) {
                        state.dataAirflowNetwork->MultizoneExternalNodeData(i).azimuth =
                            state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(j).SurfNum).Azimuth;
                        break;
                    }
                }
            }
        }

        if (ErrorsFound) ShowFatalError(state, std::string{RoutineName} + "Errors found getting inputs. Previous error(s) cause program termination.");

        // Write wind pressure coefficients in the EIO file
        print(state.files.eio, "! <AirflowNetwork Model:Wind Direction>, Wind Direction #1 to n (degree)\n");
        print(state.files.eio, "AirflowNetwork Model:Wind Direction, ");

        int numWinDirs = 11;
        Real64 angleDelta = 30.0;
        if (state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfSingleSideZones > 0) {
            numWinDirs = 35;
            angleDelta = 10.0;
        }

        for (int i = 0; i < numWinDirs; ++i) {
            print(state.files.eio, "{:.1R},", i * angleDelta);
        }
        print(state.files.eio, "{:.1R}\n", numWinDirs * angleDelta);

        print(state.files.eio, "! <AirflowNetwork Model:Wind Pressure Coefficients>, Name, Wind Pressure Coefficients #1 to n (dimensionless)\n");

        // The old version used to write info with single-sided natural ventilation specific labeling, this version no longer does that.
        std::set<int> curves;
        for (int i = 1; i <= state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfExtNode; ++i) {
            curves.insert(state.dataAirflowNetwork->MultizoneExternalNodeData(i).curve);
        }
        for (auto index : curves) {
            print(state.files.eio, "AirflowNetwork Model:Wind Pressure Coefficients, {}, ", CurveManager::GetCurveName(state, index));

            for (j = 0; j < numWinDirs; ++j) {
                print(state.files.eio, "{:.2R},", CurveManager::CurveValue(state, index, j * angleDelta));
            }
            print(state.files.eio, "{:.2R}\n", CurveManager::CurveValue(state, index, numWinDirs * angleDelta));
        }

        if (state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfSingleSideZones > 0) {
            for (int i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfZones; ++i) {
                if (state.dataAirflowNetwork->MultizoneZoneData(i).SingleSidedCpType == "ADVANCED") {
                    print(state.files.eio,
                          "AirflowNetwork: Advanced Single-Sided Model: Difference in Opening Wind Pressure Coefficients (DeltaCP), ");
                    print(state.files.eio, "{}, ", state.dataAirflowNetwork->MultizoneZoneData(i).ZoneName);
                    for (unsigned j = 1; j <= state.dataAirflowNetwork->EPDeltaCP(i).WindDir.size() - 1; ++j) {
                        print(state.files.eio, "{:.2R},", state.dataAirflowNetwork->EPDeltaCP(i).WindDir(j));
                    }
                    print(state.files.eio,
                          "{:.2R}\n",
                          state.dataAirflowNetwork->EPDeltaCP(i).WindDir(state.dataAirflowNetwork->EPDeltaCP(i).WindDir.size()));
                }
            }
        }

        // If no zone object, exit
        if (state.dataAirflowNetwork->AirflowNetworkNumOfZones == 0) {
            ShowFatalError(state, std::string{RoutineName} + "Errors found getting inputs. Previous error(s) cause program termination.");
        }
        // If zone node number =0, exit.
        for (j = 1; j <= state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces; ++j) {
            if (state.dataAirflowNetwork->MultizoneSurfaceData(j).NodeNums[0] == 0 && ErrorsFound) {
                ShowFatalError(state, std::string{RoutineName} + "Errors found getting inputs. Previous error(s) cause program termination.");
            }
            if (state.dataAirflowNetwork->MultizoneSurfaceData(j).NodeNums[1] == 0 && ErrorsFound) {
                ShowFatalError(state, std::string{RoutineName} + "Errors found getting inputs. Previous error(s) cause program termination.");
            }
        }

        // Ensure at least two surfaces are exposed to a zone
        ZoneCheck.allocate(state.dataAirflowNetwork->AirflowNetworkNumOfZones);
        ZoneBCCheck.allocate(state.dataAirflowNetwork->AirflowNetworkNumOfZones);
        ZoneCheck = 0;
        ZoneBCCheck = 0;
        CurrentModuleObject = "AirflowNetwork:MultiZone:Surface";
        for (j = 1; j <= state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces; ++j) {
            if (state.dataAirflowNetwork->MultizoneSurfaceData(j).NodeNums[0] <= state.dataAirflowNetwork->AirflowNetworkNumOfZones) {
                ++ZoneCheck(state.dataAirflowNetwork->MultizoneSurfaceData(j).NodeNums[0]);
                ZoneBCCheck(state.dataAirflowNetwork->MultizoneSurfaceData(j).NodeNums[0]) =
                    state.dataAirflowNetwork->MultizoneSurfaceData(j).NodeNums[1];
            }
            if (state.dataAirflowNetwork->MultizoneSurfaceData(j).NodeNums[1] <= state.dataAirflowNetwork->AirflowNetworkNumOfZones) {
                ++ZoneCheck(state.dataAirflowNetwork->MultizoneSurfaceData(j).NodeNums[1]);
                ZoneBCCheck(state.dataAirflowNetwork->MultizoneSurfaceData(j).NodeNums[1]) =
                    state.dataAirflowNetwork->MultizoneSurfaceData(j).NodeNums[0];
            }
        }
        for (int i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfZones; ++i) {
            if (ZoneCheck(i) == 0) {
                ShowSevereError(state, std::string{RoutineName} + "AirflowNetwork:Multizone:Zone = " + state.dataAirflowNetwork->MultizoneZoneData(i).ZoneName);
                ShowContinueError(state, " does not have any surfaces defined in " + CurrentModuleObject);
                ShowContinueError(state, "Each zone should have at least two surfaces defined in " + CurrentModuleObject);
                ErrorsFound = true;
            }
            if (ZoneCheck(i) == 1) {
                ShowSevereError(state, std::string{RoutineName} + "AirflowNetwork:Multizone:Zone = " + state.dataAirflowNetwork->MultizoneZoneData(i).ZoneName);
                ShowContinueError(state, " has only one surface defined in " + CurrentModuleObject);
                ShowContinueError(state, " Each zone should have at least two surfaces defined in " + CurrentModuleObject);
                ErrorsFound = true;
            }
            if (ZoneCheck(i) > 1) {
                SurfaceFound = false;
                for (j = 1; j <= state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces; ++j) {
                    if (state.dataAirflowNetwork->MultizoneSurfaceData(j).NodeNums[0] == i) {
                        if (ZoneBCCheck(i) != state.dataAirflowNetwork->MultizoneSurfaceData(j).NodeNums[1]) {
                            SurfaceFound = true;
                            break;
                        }
                    }
                    if (state.dataAirflowNetwork->MultizoneSurfaceData(j).NodeNums[1] == i) {
                        if (ZoneBCCheck(i) != state.dataAirflowNetwork->MultizoneSurfaceData(j).NodeNums[0]) {
                            SurfaceFound = true;
                            break;
                        }
                    }
                }
                if (!SurfaceFound) {
                    ShowWarningError(state,
                                     std::string{RoutineName} + "AirflowNetwork:Multizone:Zone = " + state.dataAirflowNetwork->MultizoneZoneData(i).ZoneName);
                    ShowContinueError(state,
                                      "has more than one surface defined in " + CurrentModuleObject + ", but has the same boundary conditions");
                    ShowContinueError(state, "Please check inputs of " + CurrentModuleObject);
                }
            }
        }
        ZoneCheck.deallocate();
        ZoneBCCheck.deallocate();

        // Validate CP Value number
        if (state.dataAirflowNetwork->AirflowNetworkSimu.iWPCCnt == iWPCCntr::Input) { // Surface-Average does not need inputs of external nodes
            // Ensure different curve is used to avoid a single side boundary condition
            found = false;
            bool differentAngle = false;
            for (j = 2; j <= state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfExtNode; ++j) {
                if (state.dataAirflowNetwork->MultizoneExternalNodeData(j - 1).curve !=
                    state.dataAirflowNetwork->MultizoneExternalNodeData(j).curve) {
                    found = true;
                    break;
                } else {
                    // If the curves are the same, then check to see if the azimuths are different
                    if (state.dataAirflowNetwork->MultizoneExternalNodeData(j - 1).azimuth !=
                        state.dataAirflowNetwork->MultizoneExternalNodeData(j).azimuth) {
                        differentAngle = state.dataAirflowNetwork->MultizoneExternalNodeData(j - 1).symmetricCurve ||
                                         state.dataAirflowNetwork->MultizoneExternalNodeData(j).symmetricCurve;
                    }
                }
            }
            if (!found && !differentAngle) {
                ShowSevereError(state, "The same Wind Pressure Coefficient Curve name is used in all AirflowNetwork:MultiZone:ExternalNode objects.");
                ShowContinueError(
                    state, "Please input at least two different Wind Pressure Coefficient Curve names to avoid single side boundary condition.");
                ErrorsFound = true;
            }
        }

        // Assign occupant ventilation control number from zone to surface
        for (int i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces; ++i) {
            j = state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum;
            if (state.dataSurface->SurfWinOriginalClass(j) == SurfaceClass::Window ||
                state.dataSurface->SurfWinOriginalClass(j) == SurfaceClass::Door ||
                state.dataSurface->SurfWinOriginalClass(j) == SurfaceClass::GlassDoor) {
                for (n = 1; n <= state.dataAirflowNetwork->AirflowNetworkNumOfZones; ++n) {
                    if (state.dataAirflowNetwork->MultizoneZoneData(n).ZoneNum == state.dataSurface->Surface(j).Zone) {
                        if (state.dataAirflowNetwork->MultizoneZoneData(n).OccupantVentilationControlNum > 0 &&
                            state.dataAirflowNetwork->MultizoneSurfaceData(i).OccupantVentilationControlNum == 0) {
                            state.dataAirflowNetwork->MultizoneSurfaceData(i).OccupantVentilationControlNum =
                                state.dataAirflowNetwork->MultizoneZoneData(n).OccupantVentilationControlNum;
                        }
                    }
                }
            }
        }

        // Read AirflowNetwork Intra zone node
        CurrentModuleObject = "AirflowNetwork:IntraZone:Node";
        state.dataAirflowNetworkBalanceManager->IntraZoneNumOfNodes =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
        if (state.dataAirflowNetworkBalanceManager->IntraZoneNumOfNodes > 0) {
            state.dataAirflowNetwork->IntraZoneNodeData.allocate(state.dataAirflowNetworkBalanceManager->IntraZoneNumOfNodes);
            for (int i = 1; i <= state.dataAirflowNetworkBalanceManager->IntraZoneNumOfNodes; ++i) {
                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         CurrentModuleObject,
                                                                         i,
                                                                         Alphas,
                                                                         NumAlphas,
                                                                         Numbers,
                                                                         NumNumbers,
                                                                         IOStatus,
                                                                         lNumericBlanks,
                                                                         lAlphaBlanks,
                                                                         cAlphaFields,
                                                                         cNumericFields);
                UtilityRoutines::IsNameEmpty(state, Alphas(1), CurrentModuleObject, ErrorsFound);
                state.dataAirflowNetwork->IntraZoneNodeData(i).Name = Alphas(1);         // Name of node
                state.dataAirflowNetwork->IntraZoneNodeData(i).RAFNNodeName = Alphas(2); // Name of RoomAir node
                state.dataAirflowNetwork->IntraZoneNodeData(i).Height = Numbers(1);      // Nodal height
                // verify RoomAir model node names(May be too early to check and move to another subroutine)
                GetRAFNNodeNum(state,
                               state.dataAirflowNetwork->IntraZoneNodeData(i).RAFNNodeName,
                               state.dataAirflowNetwork->IntraZoneNodeData(i).ZoneNum,
                               state.dataAirflowNetwork->IntraZoneNodeData(i).RAFNNodeNum,
                               Errorfound1);
                if (Errorfound1) ErrorsFound = true;
                if (state.dataAirflowNetwork->IntraZoneNodeData(i).RAFNNodeNum == 0) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + CurrentModuleObject + "='" + Alphas(1) + "' invalid name " + cAlphaFields(2) + "='" + Alphas(2));
                    ErrorsFound = true;
                }
                state.dataAirflowNetwork->IntraZoneNodeData(i).AFNZoneNum =
                    UtilityRoutines::FindItemInList(Alphas(3),
                                                    state.dataAirflowNetwork->MultizoneZoneData,
                                                    &MultizoneZoneProp::ZoneName,
                                                    state.dataAirflowNetwork->AirflowNetworkNumOfZones);
                if (state.dataAirflowNetwork->MultizoneZoneData(state.dataAirflowNetwork->IntraZoneNodeData(i).AFNZoneNum).RAFNNodeNum == 0) {
                    GetRAFNNodeNum(state,
                                   state.dataAirflowNetwork->MultizoneZoneData(state.dataAirflowNetwork->IntraZoneNodeData(i).AFNZoneNum).ZoneName,
                                   state.dataAirflowNetwork->IntraZoneNodeData(i).ZoneNum,
                                   state.dataAirflowNetwork->MultizoneZoneData(state.dataAirflowNetwork->IntraZoneNodeData(i).AFNZoneNum).RAFNNodeNum,
                                   Errorfound1);
                }
                if (state.dataAirflowNetwork->IntraZoneNodeData(i).ZoneNum == 0) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + CurrentModuleObject + "='" + Alphas(1) + "' the Zone is not defined for " + cAlphaFields(3) + "='" +
                                        Alphas(3));
                    ErrorsFound = true;
                }
            }
        }

        // check model compatibility
        if (state.dataAirflowNetworkBalanceManager->IntraZoneNumOfNodes > 0) {
            if (!UtilityRoutines::SameString(SimAirNetworkKey, "MultizoneWithoutDistribution")) {
                ShowSevereError(state,
                                std::string{RoutineName} + CurrentModuleObject +
                                    " model requires Simulation Control = MultizoneWithoutDistribution, while the input choice is " +
                                    SimAirNetworkKey + ".");
                ErrorsFound = true;
                ShowFatalError(state,
                               std::string{RoutineName} + "Errors found getting " + CurrentModuleObject +
                                   " object."
                                   " Previous error(s) cause program termination.");
            }
        }

        state.dataAirflowNetwork->NumOfNodesIntraZone = state.dataAirflowNetworkBalanceManager->IntraZoneNumOfNodes;
        // check zone node
        state.dataAirflowNetworkBalanceManager->IntraZoneNumOfZones = 0;
        for (int i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfZones; ++i) {
            if (state.dataAirflowNetwork->MultizoneZoneData(i).RAFNNodeNum > 0) {
                state.dataAirflowNetworkBalanceManager->IntraZoneNumOfZones += 1;
            }
        }

        // Override error check due to RoomAirNode for the time being

        // Read AirflowNetwork Intra linkage
        CurrentModuleObject = "AirflowNetwork:IntraZone:Linkage";
        state.dataAirflowNetworkBalanceManager->IntraZoneNumOfLinks =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
        if (state.dataAirflowNetworkBalanceManager->IntraZoneNumOfLinks > 0) {
            state.dataAirflowNetwork->IntraZoneLinkageData.allocate(state.dataAirflowNetworkBalanceManager->IntraZoneNumOfLinks);
            state.dataAirflowNetworkBalanceManager->UniqueAirflowNetworkSurfaceName.reserve(
                state.dataAirflowNetworkBalanceManager->IntraZoneNumOfLinks);
            for (int i = 1; i <= state.dataAirflowNetworkBalanceManager->IntraZoneNumOfLinks; ++i) {
                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         CurrentModuleObject,
                                                                         i,
                                                                         Alphas,
                                                                         NumAlphas,
                                                                         Numbers,
                                                                         NumNumbers,
                                                                         IOStatus,
                                                                         lNumericBlanks,
                                                                         lAlphaBlanks,
                                                                         cAlphaFields,
                                                                         cNumericFields);
                UtilityRoutines::IsNameEmpty(state, Alphas(1), CurrentModuleObject, ErrorsFound);
                state.dataAirflowNetwork->IntraZoneLinkageData(i).Name = Alphas(1); // Name of linkage
                state.dataAirflowNetwork->IntraZoneLinkageData(i).NodeNames[0] = Alphas(2);
                state.dataAirflowNetwork->IntraZoneLinkageData(i).NodeHeights[0] = 0.0;
                state.dataAirflowNetwork->IntraZoneLinkageData(i).NodeNames[1] = Alphas(3);
                state.dataAirflowNetwork->IntraZoneLinkageData(i).NodeHeights[1] = 0.0;
                state.dataAirflowNetwork->IntraZoneLinkageData(i).CompName = Alphas(4);
                if (!lAlphaBlanks(5)) {
                    // Perform simple test first.The comprehensive input validation will occur later
                    // Check valid surface name
                    state.dataAirflowNetwork->IntraZoneLinkageData(i).SurfaceName = Alphas(5);
                    state.dataAirflowNetwork->IntraZoneLinkageData(i).LinkNum =
                        UtilityRoutines::FindItemInList(Alphas(5),
                                                        state.dataAirflowNetwork->MultizoneSurfaceData,
                                                        &MultizoneSurfaceProp::SurfName,
                                                        state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces);
                    if (state.dataAirflowNetwork->IntraZoneLinkageData(i).LinkNum == 0) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "='" + Alphas(1) + "': Invalid " + cAlphaFields(5) +
                                            " given = " + Alphas(5) + " in AirflowNetwork:MultiZone:Surface objects");
                        ErrorsFound = true;
                    }
                    GlobalNames::VerifyUniqueInterObjectName(state,
                                                             state.dataAirflowNetworkBalanceManager->UniqueAirflowNetworkSurfaceName,
                                                             Alphas(5),
                                                             CurrentModuleObject,
                                                             cAlphaFields(5),
                                                             ErrorsFound);
                }
                if (UtilityRoutines::SameString(Alphas(2), Alphas(3))) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + CurrentModuleObject + "='" + Alphas(1) + "': Invalid inputs of both node name with " + Alphas(2) +
                                        " = " + Alphas(3));
                    ErrorsFound = true;
                }
                // Check valid node names
                state.dataAirflowNetwork->IntraZoneLinkageData(i).NodeNums[0] = UtilityRoutines::FindItemInList(
                    Alphas(2), state.dataAirflowNetwork->IntraZoneNodeData, state.dataAirflowNetworkBalanceManager->IntraZoneNumOfNodes);
                if (state.dataAirflowNetwork->IntraZoneLinkageData(i).NodeNums[0] == 0) {
                    state.dataAirflowNetwork->IntraZoneLinkageData(i).NodeNums[0] =
                        UtilityRoutines::FindItemInList(Alphas(2),
                                                        state.dataAirflowNetwork->MultizoneZoneData,
                                                        &MultizoneZoneProp::ZoneName,
                                                        state.dataAirflowNetwork->AirflowNetworkNumOfZones);
                    state.dataAirflowNetwork->IntraZoneLinkageData(i).NodeHeights[0] =
                        Zone(state.dataAirflowNetwork->MultizoneZoneData(state.dataAirflowNetwork->IntraZoneLinkageData(i).NodeNums[0]).ZoneNum)
                            .Centroid.z;
                    if (state.dataAirflowNetwork->IntraZoneLinkageData(i).NodeNums[0] == 0) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "='" + Alphas(1) + "': Invalid " + cAlphaFields(2) +
                                            " given = " + Alphas(2) + " in AirflowNetwork:IntraZone:Node and AirflowNetwork:MultiZone:Zone objects");
                        ErrorsFound = true;
                    }
                } else {
                    state.dataAirflowNetwork->IntraZoneLinkageData(i).NodeHeights[0] =
                        state.dataAirflowNetwork->IntraZoneNodeData(state.dataAirflowNetwork->IntraZoneLinkageData(i).NodeNums[0]).Height;
                    state.dataAirflowNetwork->IntraZoneLinkageData(i).NodeNums[0] =
                        state.dataAirflowNetwork->IntraZoneLinkageData(i).NodeNums[0] + state.dataAirflowNetwork->AirflowNetworkNumOfZones +
                        state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfExtNode;
                }
                state.dataAirflowNetwork->IntraZoneLinkageData(i).NodeNums[1] = UtilityRoutines::FindItemInList(
                    Alphas(3), state.dataAirflowNetwork->IntraZoneNodeData, state.dataAirflowNetworkBalanceManager->IntraZoneNumOfNodes);
                if (state.dataAirflowNetwork->IntraZoneLinkageData(i).NodeNums[1] == 0) {
                    state.dataAirflowNetwork->IntraZoneLinkageData(i).NodeNums[1] =
                        UtilityRoutines::FindItemInList(Alphas(3),
                                                        state.dataAirflowNetwork->MultizoneZoneData,
                                                        &MultizoneZoneProp::ZoneName,
                                                        state.dataAirflowNetwork->AirflowNetworkNumOfZones);
                    if (state.dataAirflowNetwork->IntraZoneLinkageData(i).NodeNums[1] > 0) {
                        state.dataAirflowNetwork->IntraZoneLinkageData(i).NodeHeights[1] =
                            Zone(state.dataAirflowNetwork->MultizoneZoneData(state.dataAirflowNetwork->IntraZoneLinkageData(i).NodeNums[1]).ZoneNum)
                                .Centroid.z;
                    } else {
                        if (state.dataAirflowNetwork->AirflowNetworkSimu.iWPCCnt ==
                            iWPCCntr::Input) { // Surface-Average does not need inputs of external nodes
                            state.dataAirflowNetwork->IntraZoneLinkageData(i).NodeNums[1] =
                                state.dataAirflowNetwork->MultizoneSurfaceData(state.dataAirflowNetwork->IntraZoneLinkageData(i).LinkNum).NodeNums[1];
                            if (state.dataAirflowNetwork->IntraZoneLinkageData(i).NodeNums[1] == 0) {
                                ShowSevereError(state,
                                                std::string{RoutineName} + CurrentModuleObject + "='" + Alphas(1) + "': Invalid " + cAlphaFields(3) +
                                                    " given = " + Alphas(3) +
                                                    " in AirflowNetwork:IntraZone:Node or AirflowNetwork:MultiZone:Zone or "
                                                    "AirflowNetwork:MultiZone:ExternalNode objects");
                                ErrorsFound = true;
                            }
                        }
                        if (state.dataAirflowNetwork->AirflowNetworkSimu.iWPCCnt == iWPCCntr::SurfAvg) {
                            if (!lAlphaBlanks(3)) {
                                ShowWarningError(state,
                                                 std::string{RoutineName} + CurrentModuleObject + "='" + Alphas(1) + " The input of " + cAlphaFields(3) +
                                                     " is not needed, ");
                                ShowContinueError(state,
                                                  " since AirflowNetwork Wind Pressure Coefficient Type = SURFACE-AVERAGE CALCULATION. The "
                                                  "simulation continues...");
                            }
                            state.dataAirflowNetwork->IntraZoneLinkageData(i).NodeNums[1] =
                                state.dataAirflowNetwork->MultizoneSurfaceData(state.dataAirflowNetwork->IntraZoneLinkageData(i).LinkNum).NodeNums[1];
                        }
                    }
                } else {
                    state.dataAirflowNetwork->IntraZoneLinkageData(i).NodeHeights[1] =
                        state.dataAirflowNetwork->IntraZoneNodeData(state.dataAirflowNetwork->IntraZoneLinkageData(i).NodeNums[1]).Height;
                    state.dataAirflowNetwork->IntraZoneLinkageData(i).NodeNums[1] =
                        state.dataAirflowNetwork->IntraZoneLinkageData(i).NodeNums[1] + state.dataAirflowNetwork->AirflowNetworkNumOfZones +
                        state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfExtNode;
                }
                // Ensure the both linked nodes for a surface are not zone nodes.One of nodes has to be an intrazone node
                if (state.dataAirflowNetwork->IntraZoneLinkageData(i).NodeNums[1] <= state.dataAirflowNetwork->AirflowNetworkNumOfZones &&
                    state.dataAirflowNetwork->IntraZoneLinkageData(i).NodeNums[0] <= state.dataAirflowNetwork->AirflowNetworkNumOfZones) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + CurrentModuleObject + "='" + Alphas(1) + "': Invalid node inputs " + Alphas(2) + " and " +
                                        Alphas(3) + " are zone nodes");
                    ErrorsFound = true;
                }
                if (state.dataAirflowNetwork->IntraZoneLinkageData(i).NodeNums[0] <= state.dataAirflowNetwork->AirflowNetworkNumOfZones &&
                    state.dataAirflowNetwork->IntraZoneLinkageData(i).NodeNums[1] >
                        state.dataAirflowNetwork->AirflowNetworkNumOfZones + state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfExtNode &&
                    lAlphaBlanks(5)) {
                    if (state.dataAirflowNetwork->IntraZoneLinkageData(i).NodeNums[0] !=
                        state.dataAirflowNetwork
                            ->IntraZoneNodeData(state.dataAirflowNetwork->IntraZoneLinkageData(i).NodeNums[1] -
                                                state.dataAirflowNetwork->AirflowNetworkNumOfZones -
                                                state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfExtNode)
                            .ZoneNum) {
                        ShowSevereError(
                            state,
                            std::string{RoutineName} + CurrentModuleObject + "='" + Alphas(1) + ": Invalid zone inputs between Node and Link " + Alphas(2) +
                                " and " +
                                state.dataAirflowNetwork
                                    ->MultizoneZoneData(
                                        state.dataAirflowNetwork->IntraZoneNodeData(state.dataAirflowNetwork->IntraZoneLinkageData(i).NodeNums[0])
                                            .ZoneNum)
                                    .ZoneName);
                        ErrorsFound = true;
                    }
                }
                if (state.dataAirflowNetwork->IntraZoneLinkageData(i).NodeNums[1] <= state.dataAirflowNetwork->AirflowNetworkNumOfZones &&
                    state.dataAirflowNetwork->IntraZoneLinkageData(i).NodeNums[0] >
                        state.dataAirflowNetwork->AirflowNetworkNumOfZones + state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfExtNode &&
                    lAlphaBlanks(5)) {
                    if (state.dataAirflowNetwork->IntraZoneLinkageData(i).NodeNums[1] !=
                        state.dataAirflowNetwork
                            ->IntraZoneNodeData(state.dataAirflowNetwork->IntraZoneLinkageData(i).NodeNums[0] -
                                                state.dataAirflowNetwork->AirflowNetworkNumOfZones -
                                                state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfExtNode)
                            .ZoneNum) {
                        ShowSevereError(
                            state,
                            std::string{RoutineName} + CurrentModuleObject + "='" + Alphas(1) + ": Invalid zone inputs between Node and Link " + Alphas(3) +
                                " and " +
                                state.dataAirflowNetwork
                                    ->MultizoneZoneData(
                                        state.dataAirflowNetwork->IntraZoneNodeData(state.dataAirflowNetwork->IntraZoneLinkageData(i).NodeNums[1])
                                            .ZoneNum)
                                    .ZoneName);
                        ErrorsFound = true;
                    }
                }
            }

            // Reset the number of intrazone links for a given surface
            state.dataAirflowNetwork->NumOfLinksIntraZone = state.dataAirflowNetworkBalanceManager->IntraZoneNumOfLinks;
            for (int i = 1; i <= state.dataAirflowNetworkBalanceManager->IntraZoneNumOfLinks; ++i) {
                j = state.dataAirflowNetwork->IntraZoneLinkageData(i).LinkNum;
                if (j > 0) {
                    // Revise data in multizone object
                    state.dataAirflowNetwork->NumOfLinksIntraZone = state.dataAirflowNetwork->NumOfLinksIntraZone - 1;
                    if (state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(j).SurfNum).ExtBoundCond == 0) {
                        // Exterior surface NodeNums[1] should be equal
                        if (state.dataAirflowNetwork->IntraZoneLinkageData(i).NodeNums[0] >
                            state.dataAirflowNetwork->AirflowNetworkNumOfZones + state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfExtNode) {
                            state.dataAirflowNetwork->MultizoneSurfaceData(j).RAFNflag = true;
                            state.dataAirflowNetwork->MultizoneSurfaceData(j).ZonePtr = state.dataAirflowNetwork->MultizoneSurfaceData(j).NodeNums[0];
                            state.dataAirflowNetwork->MultizoneSurfaceData(j).NodeNums[0] =
                                state.dataAirflowNetwork->IntraZoneLinkageData(i).NodeNums[0];
                        } else if (state.dataAirflowNetwork->IntraZoneLinkageData(i).NodeNums[1] >
                                   state.dataAirflowNetwork->AirflowNetworkNumOfZones +
                                       state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfExtNode) {
                            state.dataAirflowNetwork->MultizoneSurfaceData(j).RAFNflag = true;
                            state.dataAirflowNetwork->MultizoneSurfaceData(j).ZonePtr = state.dataAirflowNetwork->MultizoneSurfaceData(j).NodeNums[0];
                            state.dataAirflowNetwork->MultizoneSurfaceData(j).NodeNums[0] =
                                state.dataAirflowNetwork->IntraZoneLinkageData(i).NodeNums[1];
                        } else {
                            ShowSevereError(state,
                                            std::string{RoutineName} + "The InterZone link is not found between AirflowNetwork:IntraZone:Linkage =" +
                                                state.dataAirflowNetwork->IntraZoneLinkageData(i).Name + " and AirflowNetwork:Multizone:Surface = " +
                                                state.dataAirflowNetwork->MultizoneSurfaceData(j).SurfName);
                            ErrorsFound = true;
                        }
                    } else {
                        // Interior surface
                        if (state.dataAirflowNetwork->IntraZoneLinkageData(i).NodeNums[0] >
                                state.dataAirflowNetwork->AirflowNetworkNumOfZones +
                                    state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfExtNode &&
                            state.dataAirflowNetwork->IntraZoneLinkageData(i).NodeNums[1] >
                                state.dataAirflowNetwork->AirflowNetworkNumOfZones +
                                    state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfExtNode) {
                            state.dataAirflowNetwork->MultizoneSurfaceData(j).RAFNflag = true;
                            if (state.dataAirflowNetwork->MultizoneZoneData(state.dataAirflowNetwork->MultizoneSurfaceData(j).NodeNums[0]).ZoneNum ==
                                state.dataAirflowNetwork
                                    ->IntraZoneNodeData(state.dataAirflowNetwork->IntraZoneLinkageData(i).NodeNums[0] -
                                                        state.dataAirflowNetwork->AirflowNetworkNumOfZones -
                                                        state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfExtNode)
                                    .ZoneNum) {
                                state.dataAirflowNetwork->MultizoneSurfaceData(j).ZonePtr =
                                    state.dataAirflowNetwork->MultizoneSurfaceData(j).NodeNums[0];
                                state.dataAirflowNetwork->MultizoneSurfaceData(j).NodeNums[0] =
                                    state.dataAirflowNetwork->IntraZoneLinkageData(i).NodeNums[0];
                                state.dataAirflowNetwork->MultizoneSurfaceData(j).NodeNums[1] =
                                    state.dataAirflowNetwork->IntraZoneLinkageData(i).NodeNums[1];
                            } else {
                                state.dataAirflowNetwork->MultizoneSurfaceData(j).ZonePtr =
                                    state.dataAirflowNetwork->MultizoneSurfaceData(j).NodeNums[0];
                                state.dataAirflowNetwork->MultizoneSurfaceData(j).NodeNums[0] =
                                    state.dataAirflowNetwork->IntraZoneLinkageData(i).NodeNums[1];
                                state.dataAirflowNetwork->MultizoneSurfaceData(j).NodeNums[1] =
                                    state.dataAirflowNetwork->IntraZoneLinkageData(i).NodeNums[0];
                            }
                        } else if (state.dataAirflowNetwork->IntraZoneLinkageData(i).NodeNums[0] >
                                   state.dataAirflowNetwork->AirflowNetworkNumOfZones +
                                       state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfExtNode) {
                            state.dataAirflowNetwork->MultizoneSurfaceData(j).RAFNflag = true;
                            if (state.dataAirflowNetwork->IntraZoneLinkageData(i).NodeNums[1] ==
                                state.dataAirflowNetwork->MultizoneSurfaceData(j).NodeNums[0]) {
                                state.dataAirflowNetwork->MultizoneSurfaceData(j).NodeNums[1] =
                                    state.dataAirflowNetwork->IntraZoneLinkageData(i).NodeNums[0];
                            } else if (state.dataAirflowNetwork->IntraZoneLinkageData(i).NodeNums[1] ==
                                       state.dataAirflowNetwork->MultizoneSurfaceData(j).NodeNums[1]) {
                                state.dataAirflowNetwork->MultizoneSurfaceData(j).ZonePtr =
                                    state.dataAirflowNetwork->MultizoneSurfaceData(j).NodeNums[0];
                                state.dataAirflowNetwork->MultizoneSurfaceData(j).NodeNums[0] =
                                    state.dataAirflowNetwork->IntraZoneLinkageData(i).NodeNums[0];
                            } else {
                                ShowSevereError(
                                    state,
                                    std::string{RoutineName} + "The InterZone link is not found between AirflowNetwork:IntraZone:Linkage =" +
                                        state.dataAirflowNetwork->IntraZoneLinkageData(i).Name +
                                        " and AirflowNetwork:Multizone:Surface = " + state.dataAirflowNetwork->MultizoneSurfaceData(j).SurfName);
                                ErrorsFound = true;
                            }
                        } else if (state.dataAirflowNetwork->IntraZoneLinkageData(i).NodeNums[1] >
                                   state.dataAirflowNetwork->AirflowNetworkNumOfZones +
                                       state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfExtNode) {
                            state.dataAirflowNetwork->MultizoneSurfaceData(j).RAFNflag = true;
                            if (state.dataAirflowNetwork->IntraZoneLinkageData(i).NodeNums[0] ==
                                state.dataAirflowNetwork->MultizoneSurfaceData(j).NodeNums[0]) {
                                state.dataAirflowNetwork->MultizoneSurfaceData(j).NodeNums[1] =
                                    state.dataAirflowNetwork->IntraZoneLinkageData(i).NodeNums[1];
                            } else if (state.dataAirflowNetwork->IntraZoneLinkageData(i).NodeNums[0] ==
                                       state.dataAirflowNetwork->MultizoneSurfaceData(j).NodeNums[1]) {
                                state.dataAirflowNetwork->MultizoneSurfaceData(j).ZonePtr =
                                    state.dataAirflowNetwork->MultizoneSurfaceData(j).NodeNums[0];
                                state.dataAirflowNetwork->MultizoneSurfaceData(j).NodeNums[0] =
                                    state.dataAirflowNetwork->IntraZoneLinkageData(i).NodeNums[1];
                            } else {
                                ShowSevereError(
                                    state,
                                    std::string{RoutineName} + "The InterZone link is not found between AirflowNetwork:IntraZone:Linkage =" +
                                        state.dataAirflowNetwork->IntraZoneLinkageData(i).Name +
                                        " and AirflowNetwork:Multizone:Surface = " + state.dataAirflowNetwork->MultizoneSurfaceData(j).SurfName);
                                ErrorsFound = true;
                            }
                        }
                    }
                }
            }
            // Remove links with surface defined in Multizone : Surface objects
            int link = 1;
            if (state.dataAirflowNetwork->NumOfLinksIntraZone < state.dataAirflowNetworkBalanceManager->IntraZoneNumOfLinks) {
                while (link <= state.dataAirflowNetwork->NumOfLinksIntraZone) {
                    if (state.dataAirflowNetwork->IntraZoneLinkageData(link).LinkNum > 0) {
                        if (state.dataGlobal->DisplayExtraWarnings) {
                            ShowWarningError(state,
                                             std::string{RoutineName} + CurrentModuleObject + "='" + state.dataAirflowNetwork->IntraZoneLinkageData(link).Name +
                                                 " is reomoved from the list due to the surface conncetion from Intrazone to Interzone.");
                        }
                        for (j = link; j <= state.dataAirflowNetworkBalanceManager->IntraZoneNumOfLinks - 1; ++j) {
                            state.dataAirflowNetwork->IntraZoneLinkageData(j) = state.dataAirflowNetwork->IntraZoneLinkageData(j + 1);
                        }
                    }
                    if (state.dataAirflowNetwork->IntraZoneLinkageData(link).LinkNum == 0) link = link + 1;
                }
                if (state.dataAirflowNetwork->IntraZoneLinkageData(link).LinkNum > 0) {
                    if (state.dataGlobal->DisplayExtraWarnings) {
                        ShowWarningError(state,
                                         std::string{RoutineName} + CurrentModuleObject + "='" + state.dataAirflowNetwork->IntraZoneLinkageData(link).Name +
                                             " is removed from the list due to the surface connection from Intrazone to Interzone.");
                    }
                }
            }
        }

        // Read AirflowNetwork Distribution system node
        CurrentModuleObject = "AirflowNetwork:Distribution:Node";
        state.dataAirflowNetworkBalanceManager->DisSysNumOfNodes =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
        if (state.dataAirflowNetworkBalanceManager->DisSysNumOfNodes > 0) {
            state.dataAirflowNetwork->DisSysNodeData.allocate(state.dataAirflowNetworkBalanceManager->DisSysNumOfNodes);
            for (int i = 1; i <= state.dataAirflowNetworkBalanceManager->DisSysNumOfNodes; ++i) {
                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         CurrentModuleObject,
                                                                         i,
                                                                         Alphas,
                                                                         NumAlphas,
                                                                         Numbers,
                                                                         NumNumbers,
                                                                         IOStatus,
                                                                         lNumericBlanks,
                                                                         lAlphaBlanks,
                                                                         cAlphaFields,
                                                                         cNumericFields);
                UtilityRoutines::IsNameEmpty(state, Alphas(1), CurrentModuleObject, ErrorsFound);
                state.dataAirflowNetwork->DisSysNodeData(i).Name = Alphas(1);      // Name of node
                state.dataAirflowNetwork->DisSysNodeData(i).EPlusName = Alphas(2); // Name of associated EnergyPlus node
                state.dataAirflowNetwork->DisSysNodeData(i).EPlusType = Alphas(3); // Name of associated EnergyPlus type
                state.dataAirflowNetwork->DisSysNodeData(i).Height = Numbers(1);   // Nodal height
                state.dataAirflowNetwork->DisSysNodeData(i).EPlusNodeNum = 0;      // EPlus node number
                // verify EnergyPlus object type
                if (UtilityRoutines::SameString(Alphas(3), "AirLoopHVAC:ZoneMixer") ||
                    UtilityRoutines::SameString(Alphas(3), "AirLoopHVAC:ZoneSplitter") ||
                    UtilityRoutines::SameString(Alphas(3), "AirLoopHVAC:OutdoorAirSystem") ||
                    UtilityRoutines::SameString(Alphas(3), "OAMixerOutdoorAirStreamNode") ||
                    UtilityRoutines::SameString(Alphas(3), "OutdoorAir:NodeList") || UtilityRoutines::SameString(Alphas(3), "OutdoorAir:Node") ||
                    UtilityRoutines::SameString(Alphas(3), "Other") || lAlphaBlanks(3)) {
                } else {
                    ShowSevereError(state,
                                    std::string{RoutineName} + CurrentModuleObject + "=\"" + Alphas(1) + "\" invalid " + cAlphaFields(3) + "=\"" + Alphas(3) +
                                        "\" illegal key.");
                    ShowContinueError(state,
                                      "Valid keys are: AirLoopHVAC:ZoneMixer, AirLoopHVAC:ZoneSplitter, AirLoopHVAC:OutdoorAirSystem, "
                                      "OAMixerOutdoorAirStreamNode, OutdoorAir:NodeList, OutdoorAir:Node or Other.");
                    ErrorsFound = true;
                }
                // Avoid duplication of EPlusName
                for (j = 1; j < i; ++j) {
                    if (!UtilityRoutines::SameString(Alphas(2), "")) {
                        if (UtilityRoutines::SameString(state.dataAirflowNetwork->DisSysNodeData(j).EPlusName, Alphas(2))) {
                            ShowSevereError(state,
                                            std::string{RoutineName} + CurrentModuleObject + "=\"" + Alphas(1) + "\" Duplicated " + cAlphaFields(2) + "=\"" +
                                                Alphas(2) + "\". Please make a correction.");
                            ErrorsFound = true;
                        }
                    }
                }
            }
        } else {
            if (state.dataAirflowNetwork->SimulateAirflowNetwork > AirflowNetworkControlMultizone + 1) {
                ShowSevereError(state, std::string{RoutineName} + "An " + CurrentModuleObject + " object is required but not found.");
                ErrorsFound = true;
            }
        }

        CurrentModuleObject = "AirflowNetwork:Distribution:Component:Duct";
        if (state.dataAirflowNetworkBalanceManager->DisSysNumOfDucts == 0) {
            if (state.dataAirflowNetwork->SimulateAirflowNetwork > AirflowNetworkControlMultizone + 1) {
                ShowSevereError(state, std::string{RoutineName} + "An " + CurrentModuleObject + " object is required but not found.");
                ErrorsFound = true;
            }
        }

        // Read AirflowNetwork distribution system component: DuctViewFactors
        CurrentModuleObject = "AirflowNetwork:Distribution:DuctViewFactors";
        state.dataAirflowNetworkBalanceManager->DisSysNumOfDuctViewFactors =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
        if (state.dataAirflowNetworkBalanceManager->DisSysNumOfDuctViewFactors > 0) {
            state.dataAirflowNetwork->AirflowNetworkLinkageViewFactorData.allocate(
                state.dataAirflowNetworkBalanceManager->DisSysNumOfDuctViewFactors);
            for (int i = 1; i <= state.dataAirflowNetworkBalanceManager->DisSysNumOfDuctViewFactors; ++i) {
                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         CurrentModuleObject,
                                                                         i,
                                                                         Alphas,
                                                                         NumAlphas,
                                                                         Numbers,
                                                                         NumNumbers,
                                                                         IOStatus,
                                                                         lNumericBlanks,
                                                                         lAlphaBlanks,
                                                                         cAlphaFields,
                                                                         cNumericFields);
                UtilityRoutines::IsNameEmpty(state, Alphas(1), CurrentModuleObject, ErrorsFound);

                auto &this_VF_object(state.dataAirflowNetwork->AirflowNetworkLinkageViewFactorData(i));

                this_VF_object.LinkageName = Alphas(1); // Name of linkage

                // Surface exposure fraction
                if (Numbers(2) > 1) {
                    ShowWarningError(state,
                                     "Duct surface exposure fraction greater than 1. Check input in: " + CurrentModuleObject + " " +
                                         this_VF_object.LinkageName);
                    ShowContinueError(state, "Using value of 1 for surface exposure fraction");
                    this_VF_object.DuctExposureFraction = 1;
                } else if (Numbers(2) < 0) {
                    ShowWarningError(
                        state, "Surface exposure fraction less than 0. Check input in: " + CurrentModuleObject + " " + this_VF_object.LinkageName);
                    ShowContinueError(state, "Using value of 0 for surface exposure fraction");
                    this_VF_object.DuctExposureFraction = 0;
                } else {
                    this_VF_object.DuctExposureFraction = Numbers(1);
                }

                // Duct surface emittance
                if (Numbers(2) > 1) {
                    ShowWarningError(
                        state, "Duct surface emittance greater than 1. Check input in: " + CurrentModuleObject + " " + this_VF_object.LinkageName);
                    ShowContinueError(state, "Using value of 1 for surface emittance");
                    this_VF_object.DuctEmittance = 1;
                } else if (Numbers(2) < 0) {
                    ShowWarningError(
                        state, "Surface exposure fraction less than 0. Check input in: " + CurrentModuleObject + " " + this_VF_object.LinkageName);
                    ShowContinueError(state, "Using value of 0 for surface exposure fraction");
                    this_VF_object.DuctEmittance = 0;
                } else {
                    this_VF_object.DuctEmittance = Numbers(2);
                }

                this_VF_object.ObjectNum = i;

                int numSurfaces = NumAlphas - 1;

                this_VF_object.LinkageSurfaceData.allocate(numSurfaces);

                for (int surfNum = 1; surfNum < NumAlphas; ++surfNum) {
                    this_VF_object.LinkageSurfaceData(surfNum).SurfaceName = Alphas(surfNum + 1); // Surface name
                    this_VF_object.LinkageSurfaceData(surfNum).SurfaceNum =
                        UtilityRoutines::FindItemInList(Alphas(surfNum + 1), state.dataSurface->Surface);

                    if (this_VF_object.LinkageSurfaceData(surfNum).SurfaceNum == 0) {
                        ShowFatalError(
                            state, "Surface " + Alphas(surfNum + 1) + " not found. See: " + CurrentModuleObject + " " + this_VF_object.LinkageName);
                    }

                    // Surface view factor
                    if (!state.dataSurface->Surface(this_VF_object.LinkageSurfaceData(surfNum).SurfaceNum).HeatTransSurf) {
                        ShowWarningError(state,
                                         "Surface=" + Alphas(surfNum + 1) + " is not a heat transfer surface. Check input in: " +
                                             CurrentModuleObject + " " + this_VF_object.LinkageName);
                        ShowContinueError(state, "Using value of 0 for view factor");
                        this_VF_object.LinkageSurfaceData(surfNum).ViewFactor = 0;
                    } else if (Numbers(surfNum + 2) > 1) {
                        ShowWarningError(state,
                                         "View factor for surface " + Alphas(surfNum + 1) +
                                             " greater than 1. Check input in: " + CurrentModuleObject + " " + this_VF_object.LinkageName);
                        ShowContinueError(state, "Using value of 1 for view factor");
                        this_VF_object.LinkageSurfaceData(surfNum).ViewFactor = 1;
                    } else if (Numbers(surfNum + 2) < 0) {
                        ShowWarningError(state,
                                         "View factor for surface " + Alphas(surfNum + 1) + " less than 0. Check input in: " + CurrentModuleObject +
                                             " " + this_VF_object.LinkageName);
                        ShowContinueError(state, "Using value of 0 for view factor");
                        this_VF_object.LinkageSurfaceData(surfNum).ViewFactor = 0;
                    } else {
                        this_VF_object.LinkageSurfaceData(surfNum).ViewFactor = Numbers(surfNum + 2);
                    }
                }
            }
        }

        CurrentModuleObject = "AirflowNetwork:Distribution:Component:Fan";
        if (state.dataAirflowNetworkBalanceManager->DisSysNumOfCVFs == 0) {
            if (state.dataAirflowNetwork->SimulateAirflowNetwork > AirflowNetworkControlMultizone + 1) {
                ShowSevereError(state, std::string{RoutineName} + "An " + CurrentModuleObject + " object is required but not found.");
                ErrorsFound = true;
            }
        }

        // Read PressureController
        CurrentModuleObject = "AirflowNetwork:ZoneControl:PressureController";
        state.dataAirflowNetworkBalanceManager->NumOfPressureControllers =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
        if (state.dataAirflowNetworkBalanceManager->NumOfPressureControllers > 1) {
            ShowSevereError(state,
                            std::string{RoutineName} + "More " + CurrentModuleObject + " are found. Currently only one( \"1\") " + CurrentModuleObject +
                                " object per simulation is allowed when using AirflowNetwork Distribution Systems.");
            ShowFatalError(state,
                           std::string{RoutineName} + "Errors found getting " + CurrentModuleObject + " object. Previous error(s) cause program termination.");
        }

        if (state.dataAirflowNetworkBalanceManager->NumOfPressureControllers > 0) {
            state.dataAirflowNetwork->PressureControllerData.allocate(state.dataAirflowNetworkBalanceManager->NumOfPressureControllers);
            for (int i = 1; i <= state.dataAirflowNetworkBalanceManager->NumOfPressureControllers; ++i) {
                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         CurrentModuleObject,
                                                                         i,
                                                                         Alphas,
                                                                         NumAlphas,
                                                                         Numbers,
                                                                         NumNumbers,
                                                                         IOStatus,
                                                                         lNumericBlanks,
                                                                         lAlphaBlanks,
                                                                         cAlphaFields,
                                                                         cNumericFields);
                UtilityRoutines::IsNameEmpty(state, Alphas(1), CurrentModuleObject, ErrorsFound);
                state.dataAirflowNetwork->PressureControllerData(i).Name = Alphas(1);     // Object Name
                state.dataAirflowNetwork->PressureControllerData(i).ZoneName = Alphas(2); // Zone name
                state.dataAirflowNetwork->PressureControllerData(i).ZoneNum = UtilityRoutines::FindItemInList(Alphas(2), Zone);
                state.dataAirflowNetwork->PressureControllerData(i).AFNNodeNum =
                    UtilityRoutines::FindItemInList(Alphas(2),
                                                    state.dataAirflowNetwork->MultizoneZoneData,
                                                    &MultizoneZoneProp::ZoneName,
                                                    state.dataAirflowNetwork->AirflowNetworkNumOfZones);
                if (state.dataAirflowNetwork->PressureControllerData(i).ZoneNum == 0) {
                    ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + " object, invalid " + cAlphaFields(2) + " given.");
                    ShowContinueError(state,
                                      "..invalid " + cAlphaFields(2) + " = \"" + state.dataAirflowNetwork->PressureControllerData(i).ZoneName + "\"");
                    ErrorsFound = true;
                }

                state.dataAirflowNetwork->PressureControllerData(i).ControlObjectType = Alphas(3); // Control Object Type
                state.dataAirflowNetwork->PressureControllerData(i).ControlObjectName = Alphas(4); // Control Object Name

                {
                    auto const SELECT_CASE_var(UtilityRoutines::MakeUPPERCase(Alphas(3)));
                    if (SELECT_CASE_var == "AIRFLOWNETWORK:MULTIZONE:COMPONENT:ZONEEXHAUSTFAN") {
                        state.dataAirflowNetwork->PressureControllerData(i).ControlTypeSet = PressureCtrlExhaust;
                    } else if (SELECT_CASE_var == "AIRFLOWNETWORK:DISTRIBUTION:COMPONENT:RELIEFAIRFLOW") {
                        state.dataAirflowNetwork->PressureControllerData(i).ControlTypeSet = PressureCtrlRelief;
                    } else { // Error
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + " object, The entered choice for " + cAlphaFields(3) +
                                            " is not valid = \"" + state.dataAirflowNetwork->PressureControllerData(i).Name + "\"");
                        ShowContinueError(state,
                                          "Valid choices are "
                                          "\"AirflowNetwork:MultiZone:Component:ZoneExhaustFan\",\"AirflowNetwork:Distribution:Component:"
                                          "ReliefAirFlow\"");
                        ShowContinueError(state, "The input choice is " + Alphas(3));
                        ErrorsFound = true;
                    }
                }

                if (state.dataAirflowNetwork->PressureControllerData(i).ControlTypeSet == PressureCtrlExhaust) {
                    // This is not great
                    bool is_EXF{false};
                    auto afe = solver.elements.find(Alphas(4));
                    if (afe != solver.elements.end()) {
                        is_EXF = afe->second->type() == ComponentType::EXF;
                    }
                    if (!is_EXF) {
                        ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + " object, an invalid name is given:");
                        ShowContinueError(state, ".. invalid " + cAlphaFields(4) + " = \"" + Alphas(4) + "\".");
                        ErrorsFound = true;
                    }
                }
                if (state.dataAirflowNetwork->PressureControllerData(i).ControlTypeSet == PressureCtrlRelief) {
                    // This is not great
                    bool is_REL{false};
                    auto afe = solver.elements.find(Alphas(4));
                    if (afe != solver.elements.end()) {
                        is_REL = afe->second->type() == ComponentType::REL;
                    }
                    if (!is_REL) {
                        ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + " object, an invalid name is given:");
                        ShowContinueError(state, ".. invalid " + cAlphaFields(4) + " = \"" + Alphas(4) + "\".");
                        ErrorsFound = true;
                    }
                }

                if (lAlphaBlanks(5)) {
                    state.dataAirflowNetwork->PressureControllerData(i).AvailSchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
                } else {
                    state.dataAirflowNetwork->PressureControllerData(i).AvailSchedPtr = GetScheduleIndex(state, Alphas(5));
                    if (state.dataAirflowNetwork->PressureControllerData(i).AvailSchedPtr == 0) {
                        ShowSevereError(state,
                                        CurrentModuleObject + ", \"" + state.dataAirflowNetwork->PressureControllerData(i).Name + "\" " +
                                            cAlphaFields(5) + " not found: " + Alphas(5));
                        ErrorsFound = true;
                    }
                }
                state.dataAirflowNetwork->PressureControllerData(i).PresSetpointSchedPtr = GetScheduleIndex(state, Alphas(6));
                if (state.dataAirflowNetwork->PressureControllerData(i).PresSetpointSchedPtr == 0) {
                    ShowSevereError(state,
                                    CurrentModuleObject + ", \"" + state.dataAirflowNetwork->PressureControllerData(i).Name + "\" " +
                                        cAlphaFields(6) + " not found: " + Alphas(6));
                    ErrorsFound = true;
                }
            }
        }

        // Assign numbers of nodes and linkages
        if (state.dataAirflowNetwork->SimulateAirflowNetwork > AirflowNetworkControlSimple) {
            if (state.dataAirflowNetwork->AirflowNetworkSimu.iWPCCnt == iWPCCntr::Input) {
                state.dataAirflowNetwork->NumOfNodesMultiZone =
                    state.dataAirflowNetwork->AirflowNetworkNumOfZones + state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfExtNode;
            } else {
                state.dataAirflowNetwork->NumOfNodesMultiZone =
                    state.dataAirflowNetwork->AirflowNetworkNumOfZones + state.dataAirflowNetworkBalanceManager->NumOfExtNodes;
            }
            state.dataAirflowNetwork->NumOfLinksMultiZone = state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces;
            state.dataAirflowNetwork->AirflowNetworkNumOfNodes = state.dataAirflowNetwork->NumOfNodesMultiZone;
            if (state.dataAirflowNetwork->NumOfNodesIntraZone > 0)
                state.dataAirflowNetwork->AirflowNetworkNumOfNodes =
                    state.dataAirflowNetwork->AirflowNetworkNumOfNodes + state.dataAirflowNetwork->NumOfNodesIntraZone;
            state.dataAirflowNetwork->AirflowNetworkNumOfLinks = state.dataAirflowNetwork->NumOfLinksMultiZone;
            if (state.dataAirflowNetwork->NumOfLinksIntraZone > 0)
                state.dataAirflowNetwork->AirflowNetworkNumOfLinks =
                    state.dataAirflowNetwork->AirflowNetworkNumOfLinks + state.dataAirflowNetwork->NumOfLinksIntraZone;
        }
        if (state.dataAirflowNetwork->SimulateAirflowNetwork > AirflowNetworkControlMultizone + 1) {
            state.dataAirflowNetwork->AirflowNetworkNumOfNodes = state.dataAirflowNetwork->NumOfNodesMultiZone +
                                                                 state.dataAirflowNetworkBalanceManager->DisSysNumOfNodes +
                                                                 state.dataAirflowNetwork->NumOfNodesIntraZone;
        }

        // Assign node data
        state.dataAirflowNetwork->AirflowNetworkNodeData.allocate(state.dataAirflowNetwork->AirflowNetworkNumOfNodes);
        // Zone node
        for (int i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfZones; ++i) {
            state.dataAirflowNetwork->AirflowNetworkNodeData(i).Name = state.dataAirflowNetwork->MultizoneZoneData(i).ZoneName;
            state.dataAirflowNetwork->AirflowNetworkNodeData(i).NodeTypeNum = 0;
            state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusZoneNum = state.dataAirflowNetwork->MultizoneZoneData(i).ZoneNum;
            state.dataAirflowNetwork->AirflowNetworkNodeData(i).NodeHeight = state.dataAirflowNetwork->MultizoneZoneData(i).Height;
        }
        // External node
        if (state.dataAirflowNetwork->AirflowNetworkSimu.iWPCCnt == iWPCCntr::Input) {
            for (int i = state.dataAirflowNetwork->AirflowNetworkNumOfZones + 1; i <= state.dataAirflowNetwork->NumOfNodesMultiZone; ++i) {
                state.dataAirflowNetwork->AirflowNetworkNodeData(i).Name =
                    state.dataAirflowNetwork->MultizoneExternalNodeData(i - state.dataAirflowNetwork->AirflowNetworkNumOfZones).Name;
                state.dataAirflowNetwork->AirflowNetworkNodeData(i).NodeTypeNum = 1;
                state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusZoneNum = 0;
                state.dataAirflowNetwork->AirflowNetworkNodeData(i).NodeHeight =
                    state.dataAirflowNetwork->MultizoneExternalNodeData(i - state.dataAirflowNetwork->AirflowNetworkNumOfZones).height;
                state.dataAirflowNetwork->AirflowNetworkNodeData(i).ExtNodeNum = i - state.dataAirflowNetwork->AirflowNetworkNumOfZones;
                state.dataAirflowNetwork->AirflowNetworkNodeData(i).OutAirNodeNum =
                    state.dataAirflowNetwork->MultizoneExternalNodeData(i - state.dataAirflowNetwork->AirflowNetworkNumOfZones).OutAirNodeNum;
            }
        } else { // Surface-Average input
            for (int i = state.dataAirflowNetwork->AirflowNetworkNumOfZones + 1; i <= state.dataAirflowNetwork->NumOfNodesMultiZone; ++i) {
                n = i - state.dataAirflowNetwork->AirflowNetworkNumOfZones;
                state.dataAirflowNetwork->AirflowNetworkNodeData(i).Name = state.dataAirflowNetwork->MultizoneExternalNodeData(n).Name;
                state.dataAirflowNetwork->AirflowNetworkNodeData(i).NodeTypeNum = 1;
                state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusZoneNum = 0;
                state.dataAirflowNetwork->AirflowNetworkNodeData(i).ExtNodeNum = n;
            }
        }

        // Intrazone node
        if (state.dataAirflowNetwork->NumOfNodesIntraZone > 0) {
            for (int i = state.dataAirflowNetwork->NumOfNodesMultiZone + 1;
                 i <= state.dataAirflowNetwork->NumOfNodesMultiZone + state.dataAirflowNetwork->NumOfNodesIntraZone;
                 ++i) {
                n = i - state.dataAirflowNetwork->NumOfNodesMultiZone;
                state.dataAirflowNetwork->AirflowNetworkNodeData(i).Name = state.dataAirflowNetwork->IntraZoneNodeData(n).Name;
                state.dataAirflowNetwork->AirflowNetworkNodeData(i).NodeTypeNum = 0;
                state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusZoneNum = state.dataAirflowNetwork->IntraZoneNodeData(n).ZoneNum;
                state.dataAirflowNetwork->AirflowNetworkNodeData(i).NodeHeight = state.dataAirflowNetwork->IntraZoneNodeData(n).Height;
                state.dataAirflowNetwork->AirflowNetworkNodeData(i).RAFNNodeNum = state.dataAirflowNetwork->IntraZoneNodeData(n).RAFNNodeNum;
            }
            for (int i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfZones; ++i) {
                if (state.dataAirflowNetwork->MultizoneZoneData(i).RAFNNodeNum > 0) {
                    state.dataAirflowNetwork->AirflowNetworkNodeData(i).RAFNNodeNum = state.dataAirflowNetwork->MultizoneZoneData(i).RAFNNodeNum;
                }
            }
        }
        state.dataAirflowNetwork->NumOfNodesMultiZone = state.dataAirflowNetwork->NumOfNodesMultiZone + state.dataAirflowNetwork->NumOfNodesIntraZone;

        // Check whether Distribution system is simulated
        if (state.dataAirflowNetwork->AirflowNetworkNumOfNodes > state.dataAirflowNetwork->NumOfNodesMultiZone) {
            // Search node types: OAMixerOutdoorAirStreamNode, OutdoorAir:NodeList, and OutdoorAir:Node
            j = 0;
            for (int i = state.dataAirflowNetwork->NumOfNodesMultiZone + 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfNodes; ++i) {
                if (UtilityRoutines::SameString(state.dataAirflowNetwork->DisSysNodeData(i - state.dataAirflowNetwork->NumOfNodesMultiZone).EPlusType,
                                                "OAMixerOutdoorAirStreamNode")) {
                    ++j;
                }
                if (UtilityRoutines::SameString(state.dataAirflowNetwork->DisSysNodeData(i - state.dataAirflowNetwork->NumOfNodesMultiZone).EPlusType,
                                                "OutdoorAir:NodeList")) {
                    ++j;
                }
                if (UtilityRoutines::SameString(state.dataAirflowNetwork->DisSysNodeData(i - state.dataAirflowNetwork->NumOfNodesMultiZone).EPlusType,
                                                "OutdoorAir:Node")) {
                    ++j;
                }
            }

            for (int i = state.dataAirflowNetwork->NumOfNodesMultiZone + 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfNodes; ++i) {
                state.dataAirflowNetwork->AirflowNetworkNodeData(i).Name =
                    state.dataAirflowNetwork->DisSysNodeData(i - state.dataAirflowNetwork->NumOfNodesMultiZone).Name;
                state.dataAirflowNetwork->AirflowNetworkNodeData(i).NodeTypeNum = 0;
                state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusZoneNum = 0;
                state.dataAirflowNetwork->AirflowNetworkNodeData(i).NodeHeight =
                    state.dataAirflowNetwork->DisSysNodeData(i - state.dataAirflowNetwork->NumOfNodesMultiZone).Height;
                state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusNodeNum =
                    state.dataAirflowNetwork->DisSysNodeData(i - state.dataAirflowNetwork->NumOfNodesMultiZone).EPlusNodeNum;
                // Get mixer information
                if (UtilityRoutines::SameString(state.dataAirflowNetwork->DisSysNodeData(i - state.dataAirflowNetwork->NumOfNodesMultiZone).EPlusType,
                                                "AirLoopHVAC:ZoneMixer")) {
                    state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusTypeNum = iEPlusNodeType::MIX;
                }
                // Get splitter information
                if (UtilityRoutines::SameString(state.dataAirflowNetwork->DisSysNodeData(i - state.dataAirflowNetwork->NumOfNodesMultiZone).EPlusType,
                                                "AirLoopHVAC:ZoneSplitter")) {
                    state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusTypeNum = iEPlusNodeType::SPL;
                }
                // Get outside air system information
                if (UtilityRoutines::SameString(state.dataAirflowNetwork->DisSysNodeData(i - state.dataAirflowNetwork->NumOfNodesMultiZone).EPlusType,
                                                "AirLoopHVAC:OutdoorAirSystem")) {
                    state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusTypeNum = iEPlusNodeType::OAN;
                }
                // Get OA system inlet information 'OAMixerOutdoorAirStreamNode' was specified as an outdoor air node implicitly
                if (UtilityRoutines::SameString(state.dataAirflowNetwork->DisSysNodeData(i - state.dataAirflowNetwork->NumOfNodesMultiZone).EPlusType,
                                                "OAMixerOutdoorAirStreamNode")) {
                    state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusTypeNum = iEPlusNodeType::EXT;
                    state.dataAirflowNetwork->AirflowNetworkNodeData(i).ExtNodeNum =
                        state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfExtNode + 1;
                    state.dataAirflowNetwork->AirflowNetworkNodeData(i).NodeTypeNum = 1;
                }
                if (UtilityRoutines::SameString(state.dataAirflowNetwork->DisSysNodeData(i - state.dataAirflowNetwork->NumOfNodesMultiZone).EPlusType,
                                                "OutdoorAir:NodeList") ||
                    UtilityRoutines::SameString(state.dataAirflowNetwork->DisSysNodeData(i - state.dataAirflowNetwork->NumOfNodesMultiZone).EPlusType,
                                                "OutdoorAir:Node")) {
                    if (j > 1) {
                        state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusTypeNum = iEPlusNodeType::EXT;
                        state.dataAirflowNetwork->AirflowNetworkNodeData(i).ExtNodeNum =
                            state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfExtNode + 1;
                        state.dataAirflowNetwork->AirflowNetworkNodeData(i).NodeTypeNum = 1;
                    } else {
                        ShowSevereError(state,
                                        std::string{RoutineName} + "AirflowNetwork:Distribution:Node: The outdoor air node is found at " +
                                            state.dataAirflowNetwork->AirflowNetworkNodeData(i).Name);
                        ShowContinueError(state,
                                          "The node with Component Object Type = OAMixerOutdoorAirStreamNode is not found. Please check inputs.");
                        ErrorsFound = true;
                    }
                }
            }
        }

        // Start to assembly AirflowNetwork Components
        state.dataAirflowNetwork->AirflowNetworkNumOfComps =
            state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfDetOpenings +
            state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfSimOpenings +
            state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfSurCracks + state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfSurELA +
            state.dataAirflowNetworkBalanceManager->DisSysNumOfLeaks + state.dataAirflowNetworkBalanceManager->DisSysNumOfELRs +
            state.dataAirflowNetworkBalanceManager->DisSysNumOfDucts + state.dataAirflowNetworkBalanceManager->DisSysNumOfDampers +
            state.dataAirflowNetworkBalanceManager->DisSysNumOfCVFs + state.dataAirflowNetworkBalanceManager->DisSysNumOfDetFans +
            state.dataAirflowNetworkBalanceManager->DisSysNumOfCPDs + state.dataAirflowNetworkBalanceManager->DisSysNumOfCoils +
            state.dataAirflowNetworkBalanceManager->DisSysNumOfTermUnits + state.dataAirflowNetwork->AirflowNetworkNumOfExhFan +
            state.dataAirflowNetworkBalanceManager->DisSysNumOfHXs + state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfHorOpenings +
            state.dataAirflowNetworkBalanceManager->NumOfOAFans + state.dataAirflowNetworkBalanceManager->NumOfReliefFans;
        state.dataAirflowNetwork->AirflowNetworkCompData.allocate(state.dataAirflowNetwork->AirflowNetworkNumOfComps);

        for (int i = 1; i <= state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfDetOpenings; ++i) { // Detailed opening component
            state.dataAirflowNetwork->AirflowNetworkCompData(i).Name = state.dataAirflowNetwork->MultizoneCompDetOpeningData(i).name;
            solver.compnum[state.dataAirflowNetwork->AirflowNetworkCompData(i).Name] = i;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).CompTypeNum = iComponentTypeNum::DOP;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).TypeNum = i;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).EPlusName = "";
            state.dataAirflowNetwork->AirflowNetworkCompData(i).EPlusCompName = "";
            state.dataAirflowNetwork->AirflowNetworkCompData(i).EPlusType = "";
            state.dataAirflowNetwork->AirflowNetworkCompData(i).CompNum = i;
        }

        j = state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfDetOpenings;
        for (int i = 1 + j; i <= state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfSimOpenings + j; ++i) { // Simple opening component
            n = i - j;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).Name = state.dataAirflowNetwork->MultizoneCompSimpleOpeningData(n).name;
            solver.compnum[state.dataAirflowNetwork->AirflowNetworkCompData(i).Name] = i;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).CompTypeNum = iComponentTypeNum::SOP;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).TypeNum = n;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).EPlusName = "";
            state.dataAirflowNetwork->AirflowNetworkCompData(i).EPlusCompName = "";
            state.dataAirflowNetwork->AirflowNetworkCompData(i).EPlusType = "";
            state.dataAirflowNetwork->AirflowNetworkCompData(i).CompNum = i;
        }

        j += state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfSimOpenings;
        for (int i = 1 + j; i <= state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfSurCracks + j; ++i) { // Surface crack component
            n = i - j;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).Name = state.dataAirflowNetwork->MultizoneSurfaceCrackData(n).name;
            solver.compnum[state.dataAirflowNetwork->AirflowNetworkCompData(i).Name] = i;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).CompTypeNum = iComponentTypeNum::SCR;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).TypeNum = n;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).EPlusName = "";
            state.dataAirflowNetwork->AirflowNetworkCompData(i).EPlusCompName = "";
            state.dataAirflowNetwork->AirflowNetworkCompData(i).EPlusType = "";
            state.dataAirflowNetwork->AirflowNetworkCompData(i).CompNum = i;
        }

        j += state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfSurCracks;
        for (int i = 1 + j; i <= state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfSurELA + j; ++i) { // Surface crack component
            n = i - j;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).Name = state.dataAirflowNetwork->MultizoneSurfaceELAData(n).name;
            solver.compnum[state.dataAirflowNetwork->AirflowNetworkCompData(i).Name] = i;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).CompTypeNum = iComponentTypeNum::SEL;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).TypeNum = n;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).EPlusName = "";
            state.dataAirflowNetwork->AirflowNetworkCompData(i).EPlusCompName = "";
            state.dataAirflowNetwork->AirflowNetworkCompData(i).EPlusType = "";
            state.dataAirflowNetwork->AirflowNetworkCompData(i).CompNum = i;
        }

        j += state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfSurELA;
        for (int i = 1 + j; i <= state.dataAirflowNetwork->AirflowNetworkNumOfExhFan + j; ++i) { // Zone exhaust fan component
            n = i - j;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).Name = state.dataAirflowNetwork->MultizoneCompExhaustFanData(n).name;
            solver.compnum[state.dataAirflowNetwork->AirflowNetworkCompData(i).Name] = i;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).CompTypeNum = iComponentTypeNum::EXF;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).TypeNum = n;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).EPlusName = "";
            state.dataAirflowNetwork->AirflowNetworkCompData(i).EPlusCompName = "";
            state.dataAirflowNetwork->AirflowNetworkCompData(i).EPlusType = "";
            state.dataAirflowNetwork->AirflowNetworkCompData(i).CompNum = i;
        }

        j += state.dataAirflowNetwork->AirflowNetworkNumOfExhFan;
        for (int i = 1 + j; i <= state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfHorOpenings + j;
             ++i) { // Distribution system crack component
            n = i - j;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).Name = state.dataAirflowNetwork->MultizoneCompHorOpeningData(n).name;
            solver.compnum[state.dataAirflowNetwork->AirflowNetworkCompData(i).Name] = i;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).CompTypeNum = iComponentTypeNum::HOP;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).TypeNum = n;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).EPlusName = "";
            state.dataAirflowNetwork->AirflowNetworkCompData(i).EPlusCompName = "";
            state.dataAirflowNetwork->AirflowNetworkCompData(i).EPlusType = "";
            state.dataAirflowNetwork->AirflowNetworkCompData(i).CompNum = i;
        }

        j += state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfHorOpenings;
        for (int i = 1 + j; i <= state.dataAirflowNetworkBalanceManager->DisSysNumOfLeaks + j; ++i) { // Distribution system crack component
            n = i - j;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).Name = state.dataAirflowNetwork->DisSysCompLeakData(n).name;
            solver.compnum[state.dataAirflowNetwork->AirflowNetworkCompData(i).Name] = i;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).CompTypeNum = iComponentTypeNum::PLR;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).TypeNum = n;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).EPlusName = "";
            state.dataAirflowNetwork->AirflowNetworkCompData(i).EPlusCompName = "";
            state.dataAirflowNetwork->AirflowNetworkCompData(i).EPlusType = "";
            state.dataAirflowNetwork->AirflowNetworkCompData(i).CompNum = i;
        }

        j += state.dataAirflowNetworkBalanceManager->DisSysNumOfLeaks;
        for (int i = 1 + j; i <= state.dataAirflowNetworkBalanceManager->DisSysNumOfELRs + j;
             ++i) { // Distribution system effective leakage ratio component
            n = i - j;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).Name = state.dataAirflowNetwork->DisSysCompELRData(n).name;
            solver.compnum[state.dataAirflowNetwork->AirflowNetworkCompData(i).Name] = i;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).CompTypeNum = iComponentTypeNum::ELR;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).TypeNum = n;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).EPlusName = "";
            state.dataAirflowNetwork->AirflowNetworkCompData(i).EPlusCompName = "";
            state.dataAirflowNetwork->AirflowNetworkCompData(i).EPlusType = "";
            state.dataAirflowNetwork->AirflowNetworkCompData(i).CompNum = i;
        }

        j += state.dataAirflowNetworkBalanceManager->DisSysNumOfELRs;
        for (int i = 1 + j; i <= state.dataAirflowNetworkBalanceManager->DisSysNumOfDucts + j;
             ++i) { // Distribution system effective leakage ratio component
            n = i - j;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).Name = state.dataAirflowNetwork->DisSysCompDuctData(n).name;
            solver.compnum[state.dataAirflowNetwork->AirflowNetworkCompData(i).Name] = i;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).CompTypeNum = iComponentTypeNum::DWC;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).TypeNum = n;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).EPlusName = "";
            state.dataAirflowNetwork->AirflowNetworkCompData(i).EPlusCompName = "";
            state.dataAirflowNetwork->AirflowNetworkCompData(i).EPlusType = "";
            state.dataAirflowNetwork->AirflowNetworkCompData(i).CompNum = i;
        }

        j += state.dataAirflowNetworkBalanceManager->DisSysNumOfDucts;
        for (int i = 1 + j; i <= state.dataAirflowNetworkBalanceManager->DisSysNumOfDampers + j;
             ++i) { // Distribution system effective leakage ratio component
            n = i - j;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).Name = state.dataAirflowNetwork->DisSysCompDamperData(n).name;
            solver.compnum[state.dataAirflowNetwork->AirflowNetworkCompData(i).Name] = i;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).CompTypeNum = iComponentTypeNum::DMP;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).TypeNum = n;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).EPlusName = "";
            state.dataAirflowNetwork->AirflowNetworkCompData(i).EPlusCompName = "";
            state.dataAirflowNetwork->AirflowNetworkCompData(i).EPlusType = "";
            state.dataAirflowNetwork->AirflowNetworkCompData(i).CompNum = i;
        }

        j += state.dataAirflowNetworkBalanceManager->DisSysNumOfDampers;
        for (int i = 1 + j; i <= state.dataAirflowNetworkBalanceManager->DisSysNumOfCVFs + j;
             ++i) { // Distribution system constant volume fan component
            n = i - j;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).Name = state.dataAirflowNetwork->DisSysCompCVFData(n).name;
            solver.compnum[state.dataAirflowNetwork->AirflowNetworkCompData(i).Name] = i;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).CompTypeNum = iComponentTypeNum::CVF;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).TypeNum = n;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).EPlusName = "";
            state.dataAirflowNetwork->AirflowNetworkCompData(i).EPlusCompName = "";
            state.dataAirflowNetwork->AirflowNetworkCompData(i).EPlusType = "";
            state.dataAirflowNetwork->AirflowNetworkCompData(i).CompNum = i;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).EPlusTypeNum = iEPlusComponentType::FAN;
        }

        j += state.dataAirflowNetworkBalanceManager->DisSysNumOfCVFs;
        for (int i = 1 + j; i <= state.dataAirflowNetworkBalanceManager->DisSysNumOfDetFans + j; ++i) { // Distribution system fan component
            n = i - j;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).Name = state.dataAirflowNetwork->DisSysCompDetFanData(n).name;
            solver.compnum[state.dataAirflowNetwork->AirflowNetworkCompData(i).Name] = i;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).CompTypeNum = iComponentTypeNum::FAN;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).TypeNum = n;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).EPlusName = "";
            state.dataAirflowNetwork->AirflowNetworkCompData(i).EPlusCompName = "";
            state.dataAirflowNetwork->AirflowNetworkCompData(i).EPlusType = "";
            state.dataAirflowNetwork->AirflowNetworkCompData(i).CompNum = i;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).EPlusTypeNum = iEPlusComponentType::FAN;
        }

        j += state.dataAirflowNetworkBalanceManager->DisSysNumOfDetFans;
        for (int i = 1 + j; i <= state.dataAirflowNetworkBalanceManager->DisSysNumOfCPDs + j;
             ++i) { // Distribution system constant pressure drop component
            n = i - j;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).Name = state.dataAirflowNetwork->DisSysCompCPDData(n).name;
            solver.compnum[state.dataAirflowNetwork->AirflowNetworkCompData(i).Name] = i;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).CompTypeNum = iComponentTypeNum::CPD;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).TypeNum = n;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).EPlusName = "";
            state.dataAirflowNetwork->AirflowNetworkCompData(i).EPlusCompName = "";
            state.dataAirflowNetwork->AirflowNetworkCompData(i).EPlusType = "";
            state.dataAirflowNetwork->AirflowNetworkCompData(i).CompNum = i;
        }

        j += state.dataAirflowNetworkBalanceManager->DisSysNumOfCPDs;
        for (int i = 1 + j; i <= state.dataAirflowNetworkBalanceManager->DisSysNumOfCoils + j; ++i) { // Distribution system coil component
            n = i - j;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).Name = state.dataAirflowNetwork->DisSysCompCoilData(n).name;
            solver.compnum[state.dataAirflowNetwork->AirflowNetworkCompData(i).Name] = i;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).CompTypeNum = iComponentTypeNum::COI;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).TypeNum = n;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).EPlusName = "";
            state.dataAirflowNetwork->AirflowNetworkCompData(i).EPlusCompName = "";
            state.dataAirflowNetwork->AirflowNetworkCompData(i).EPlusType = "";
            state.dataAirflowNetwork->AirflowNetworkCompData(i).CompNum = i;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).EPlusTypeNum = iEPlusComponentType::COI;
        }

        j += state.dataAirflowNetworkBalanceManager->DisSysNumOfCoils;
        for (int i = 1 + j; i <= state.dataAirflowNetworkBalanceManager->DisSysNumOfTermUnits + j; ++i) { // Terminal unit component
            n = i - j;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).Name = state.dataAirflowNetwork->DisSysCompTermUnitData(n).name;
            solver.compnum[state.dataAirflowNetwork->AirflowNetworkCompData(i).Name] = i;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).CompTypeNum = iComponentTypeNum::TMU;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).TypeNum = n;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).EPlusName = "";
            state.dataAirflowNetwork->AirflowNetworkCompData(i).EPlusCompName = "";
            state.dataAirflowNetwork->AirflowNetworkCompData(i).EPlusType = "";
            state.dataAirflowNetwork->AirflowNetworkCompData(i).CompNum = i;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).EPlusTypeNum = iEPlusComponentType::RHT;
        }

        j += state.dataAirflowNetworkBalanceManager->DisSysNumOfTermUnits;
        for (int i = 1 + j; i <= state.dataAirflowNetworkBalanceManager->DisSysNumOfHXs + j; ++i) { // Distribution system heat exchanger component
            n = i - j;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).Name = state.dataAirflowNetwork->DisSysCompHXData(n).name;
            solver.compnum[state.dataAirflowNetwork->AirflowNetworkCompData(i).Name] = i;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).CompTypeNum = iComponentTypeNum::HEX;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).TypeNum = n;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).EPlusName = "";
            state.dataAirflowNetwork->AirflowNetworkCompData(i).EPlusCompName = "";
            state.dataAirflowNetwork->AirflowNetworkCompData(i).EPlusType = "";
            state.dataAirflowNetwork->AirflowNetworkCompData(i).CompNum = i;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).EPlusTypeNum = iEPlusComponentType::HEX;
        }

        j += state.dataAirflowNetworkBalanceManager->DisSysNumOfHXs;
        for (int i = 1 + j; i <= state.dataAirflowNetworkBalanceManager->NumOfOAFans + j; ++i) { // OA fan component
            n = i - j;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).Name = state.dataAirflowNetwork->DisSysCompOutdoorAirData(n).name;
            solver.compnum[state.dataAirflowNetwork->AirflowNetworkCompData(i).Name] = i;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).CompTypeNum = iComponentTypeNum::OAF;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).TypeNum = n;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).EPlusName = "";
            state.dataAirflowNetwork->AirflowNetworkCompData(i).EPlusCompName = "";
            state.dataAirflowNetwork->AirflowNetworkCompData(i).EPlusType = "";
            state.dataAirflowNetwork->AirflowNetworkCompData(i).CompNum = i;
        }

        j += state.dataAirflowNetworkBalanceManager->NumOfOAFans;
        for (int i = 1 + j; i <= state.dataAirflowNetworkBalanceManager->NumOfReliefFans + j; ++i) { // OA fan component
            n = i - j;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).Name = state.dataAirflowNetwork->DisSysCompReliefAirData(n).name;
            solver.compnum[state.dataAirflowNetwork->AirflowNetworkCompData(i).Name] = i;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).CompTypeNum = iComponentTypeNum::REL;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).TypeNum = n;
            state.dataAirflowNetwork->AirflowNetworkCompData(i).EPlusName = "";
            state.dataAirflowNetwork->AirflowNetworkCompData(i).EPlusCompName = "";
            state.dataAirflowNetwork->AirflowNetworkCompData(i).EPlusType = "";
            state.dataAirflowNetwork->AirflowNetworkCompData(i).CompNum = i;
        }

        // Assign linkage data

        // Read AirflowNetwork linkage data
        CurrentModuleObject = "AirflowNetwork:Distribution:Linkage";
        state.dataAirflowNetworkBalanceManager->DisSysNumOfLinks =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
        if (state.dataAirflowNetworkBalanceManager->DisSysNumOfLinks > 0 &&
            state.dataAirflowNetwork->SimulateAirflowNetwork > AirflowNetworkControlMultizone) { // Multizone + Distribution
            state.dataAirflowNetwork->AirflowNetworkNumOfLinks =
                state.dataAirflowNetwork->NumOfLinksMultiZone + state.dataAirflowNetworkBalanceManager->DisSysNumOfLinks;
            state.dataAirflowNetwork->AirflowNetworkLinkageData.allocate(state.dataAirflowNetworkBalanceManager->DisSysNumOfLinks +
                                                                         state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces);
        } else { // Multizone + IntraZone only
            //    state.dataAirflowNetwork->AirflowNetworkLinkageData.allocate( state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces );
            state.dataAirflowNetwork->AirflowNetworkLinkageData.allocate(state.dataAirflowNetwork->AirflowNetworkNumOfLinks);
        }

        // Assign Multizone linkage based on surfaces, by assuming every surface has a crack or opening
        j = 0;
        for (count = 1; count <= state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces; ++count) {
            if (state.dataAirflowNetwork->MultizoneSurfaceData(count).SurfNum == 0) continue;
            state.dataAirflowNetwork->AirflowNetworkLinkageData(count).Name = state.dataAirflowNetwork->MultizoneSurfaceData(count).SurfName;
            state.dataAirflowNetwork->AirflowNetworkLinkageData(count).NodeNums[0] =
                state.dataAirflowNetwork->MultizoneSurfaceData(count).NodeNums[0];
            state.dataAirflowNetwork->AirflowNetworkLinkageData(count).NodeNums[1] =
                state.dataAirflowNetwork->MultizoneSurfaceData(count).NodeNums[1];
            state.dataAirflowNetwork->AirflowNetworkLinkageData(count).CompName = state.dataAirflowNetwork->MultizoneSurfaceData(count).OpeningName;
            state.dataAirflowNetwork->AirflowNetworkLinkageData(count).ZoneNum = 0;
            state.dataAirflowNetwork->AirflowNetworkLinkageData(count).LinkNum = count;
            state.dataAirflowNetwork->AirflowNetworkLinkageData(count).NodeHeights[0] = state.dataAirflowNetwork->MultizoneSurfaceData(count).CHeight;
            state.dataAirflowNetwork->AirflowNetworkLinkageData(count).NodeHeights[1] = state.dataAirflowNetwork->MultizoneSurfaceData(count).CHeight;
            if (!state.dataSurface->WorldCoordSystem) {
                if (state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(count).NodeNums[0])
                        .EPlusZoneNum > 0) {
                    state.dataAirflowNetwork->AirflowNetworkLinkageData(count).NodeHeights[0] -=
                        Zone(state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(count).NodeNums[0])
                                 .EPlusZoneNum)
                            .OriginZ;
                }
                if (state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(count).NodeNums[1])
                        .EPlusZoneNum > 0) {
                    state.dataAirflowNetwork->AirflowNetworkLinkageData(count).NodeHeights[1] -=
                        Zone(state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(count).NodeNums[1])
                                 .EPlusZoneNum)
                            .OriginZ;
                }
            }
            // Find component number
            auto afe = solver.elements.find(state.dataAirflowNetwork->AirflowNetworkLinkageData(count).CompName);
            if (afe != solver.elements.end()) {
                // found = false;
                // for (i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfComps; ++i) {
                state.dataAirflowNetwork->AirflowNetworkLinkageData(count).element = afe->second;
                // Get CompTypeNum here, this is a hack to hold us over until the introspection is dealt with
                auto compnum_iter = solver.compnum.find(state.dataAirflowNetwork->AirflowNetworkLinkageData(count).CompName);
                assert(compnum_iter != solver.compnum.end());
                int compnum = compnum_iter->second;
                state.dataAirflowNetwork->AirflowNetworkLinkageData(count).CompNum = compnum;

                switch (state.dataAirflowNetwork->AirflowNetworkLinkageData(count).element->type()) {
                case ComponentType::DOP: {
                    // if (state.dataAirflowNetwork->AirflowNetworkLinkageData(count).CompName ==
                    // state.dataAirflowNetwork->AirflowNetworkCompData(i).Name) {
                    //    state.dataAirflowNetwork->AirflowNetworkLinkageData(count).CompNum = i;
                    //    found = true;
                    //    if (state.dataAirflowNetwork->AirflowNetworkCompData(i).CompTypeNum == iComponentTypeNum::DOP) {
                    ++j;
                    state.dataAirflowNetwork->AirflowNetworkLinkageData(count).DetOpenNum = j;
                    state.dataAirflowNetwork->MultizoneSurfaceData(count).Multiplier =
                        state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(count).SurfNum).Multiplier;
                    if (state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(count).SurfNum).Tilt < 10.0 ||
                        state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(count).SurfNum).Tilt > 170.0) {
                        ShowWarningError(state, "An AirflowNetwork:Multizone:Surface object has an air-flow opening corresponding to");
                        ShowContinueError(
                            state, "window or door = " + state.dataAirflowNetwork->MultizoneSurfaceData(count).SurfName + ", which is within ");
                        ShowContinueError(state, "10 deg of being horizontal. Airflows through large horizontal openings are poorly");
                        ShowContinueError(state, "modeled in the AirflowNetwork model resulting in only one-way airflow.");
                    }
                    if (!(state.dataSurface->SurfWinOriginalClass(state.dataAirflowNetwork->MultizoneSurfaceData(count).SurfNum) ==
                              SurfaceClass::Window ||
                          state.dataSurface->SurfWinOriginalClass(state.dataAirflowNetwork->MultizoneSurfaceData(count).SurfNum) ==
                              SurfaceClass::GlassDoor ||
                          state.dataSurface->SurfWinOriginalClass(state.dataAirflowNetwork->MultizoneSurfaceData(count).SurfNum) ==
                              SurfaceClass::Door ||
                          state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(count).SurfNum).IsAirBoundarySurf)) {
                        ShowSevereError(state,
                                        std::string{RoutineName} +
                                            "AirflowNetworkComponent: The opening must be assigned to a window, door, glassdoor or air boundary at " +
                                            state.dataAirflowNetwork->AirflowNetworkLinkageData(count).Name);
                        ErrorsFound = true;
                    }
                    if (state.dataSurface->SurfWinOriginalClass(state.dataAirflowNetwork->MultizoneSurfaceData(count).SurfNum) ==
                            SurfaceClass::Door ||
                        state.dataSurface->SurfWinOriginalClass(state.dataAirflowNetwork->MultizoneSurfaceData(count).SurfNum) ==
                            SurfaceClass::GlassDoor) {
                        if (state.dataAirflowNetwork->MultizoneCompDetOpeningData(state.dataAirflowNetwork->AirflowNetworkCompData(compnum).TypeNum)
                                .LVOType == 2) {
                            ShowSevereError(state,
                                            std::string{RoutineName} +
                                                "AirflowNetworkComponent: The opening with horizontally pivoted type must be assigned to a "
                                                "window surface at " +
                                                state.dataAirflowNetwork->AirflowNetworkLinkageData(count).Name);
                            ErrorsFound = true;
                        }
                    }
                } break;
                case ComponentType::SOP: {
                    // if (state.dataAirflowNetwork->AirflowNetworkCompData(i).CompTypeNum == iComponentTypeNum::SOP) {
                    state.dataAirflowNetwork->MultizoneSurfaceData(count).Multiplier =
                        state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(count).SurfNum).Multiplier;
                    if (state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(count).SurfNum).Tilt < 10.0 ||
                        state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(count).SurfNum).Tilt > 170.0) {
                        ShowSevereError(state, "An AirflowNetwork:Multizone:Surface object has an air-flow opening corresponding to");
                        ShowContinueError(state,
                                          "window or door = " + state.dataAirflowNetwork->MultizoneSurfaceData(count).SurfName + ", which is within");
                        ShowContinueError(state, "10 deg of being horizontal. Airflows through horizontal openings are not allowed.");
                        ShowContinueError(state,
                                          "AirflowNetwork:Multizone:Component:SimpleOpening = " +
                                              state.dataAirflowNetwork->AirflowNetworkCompData(compnum).Name);
                        ErrorsFound = true;
                    }

                    if (!(state.dataSurface->SurfWinOriginalClass(state.dataAirflowNetwork->MultizoneSurfaceData(count).SurfNum) ==
                              SurfaceClass::Window ||
                          state.dataSurface->SurfWinOriginalClass(state.dataAirflowNetwork->MultizoneSurfaceData(count).SurfNum) ==
                              SurfaceClass::GlassDoor ||
                          state.dataSurface->SurfWinOriginalClass(state.dataAirflowNetwork->MultizoneSurfaceData(count).SurfNum) ==
                              SurfaceClass::Door ||
                          state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(count).SurfNum).IsAirBoundarySurf)) {
                        ShowSevereError(state,
                                        std::string{RoutineName} +
                                            "AirflowNetworkComponent: The opening must be assigned to a window, door, glassdoor or air boundary at " +
                                            state.dataAirflowNetwork->AirflowNetworkLinkageData(count).Name);
                        ErrorsFound = true;
                    }
                } break;
                case ComponentType::HOP: {
                    // if (state.dataAirflowNetwork->AirflowNetworkCompData(i).CompTypeNum == iComponentTypeNum::HOP) {
                    state.dataAirflowNetwork->MultizoneSurfaceData(count).Multiplier =
                        state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(count).SurfNum).Multiplier;
                    // Get linkage height from upper and lower zones
                    if (state.dataAirflowNetwork->MultizoneZoneData(state.dataAirflowNetwork->AirflowNetworkLinkageData(count).NodeNums[0]).ZoneNum >
                        0) {
                        state.dataAirflowNetwork->AirflowNetworkLinkageData(count).NodeHeights[0] =
                            Zone(state.dataAirflowNetwork->MultizoneZoneData(state.dataAirflowNetwork->AirflowNetworkLinkageData(count).NodeNums[0])
                                     .ZoneNum)
                                .Centroid.z;
                    }
                    if (state.dataAirflowNetwork->AirflowNetworkLinkageData(count).NodeNums[1] <=
                        state.dataAirflowNetwork->AirflowNetworkNumOfZones) {
                        if (state.dataAirflowNetwork->MultizoneZoneData(state.dataAirflowNetwork->AirflowNetworkLinkageData(count).NodeNums[1])
                                .ZoneNum > 0) {
                            state.dataAirflowNetwork->AirflowNetworkLinkageData(count).NodeHeights[1] =
                                Zone(state.dataAirflowNetwork
                                         ->MultizoneZoneData(state.dataAirflowNetwork->AirflowNetworkLinkageData(count).NodeNums[1])
                                         .ZoneNum)
                                    .Centroid.z;
                        }
                    }
                    if (state.dataAirflowNetwork->AirflowNetworkLinkageData(count).NodeNums[1] > state.dataAirflowNetwork->AirflowNetworkNumOfZones) {
                        ShowSevereError(state,
                                        std::string{RoutineName} +
                                            "AirflowNetworkComponent: The horizontal opening must be located between two thermal zones at " +
                                            state.dataAirflowNetwork->AirflowNetworkLinkageData(count).Name);
                        ShowContinueError(state, "This component is exposed to outdoors.");
                        ErrorsFound = true;
                    } else {
                        if (!(state.dataAirflowNetwork->MultizoneZoneData(state.dataAirflowNetwork->AirflowNetworkLinkageData(count).NodeNums[0])
                                      .ZoneNum > 0 &&
                              state.dataAirflowNetwork->MultizoneZoneData(state.dataAirflowNetwork->AirflowNetworkLinkageData(count).NodeNums[1])
                                      .ZoneNum > 0)) {
                            ShowSevereError(state,
                                            std::string{RoutineName} +
                                                "AirflowNetworkComponent: The horizontal opening must be located between two thermal zones at " +
                                                state.dataAirflowNetwork->AirflowNetworkLinkageData(count).Name);
                            ErrorsFound = true;
                        }
                    }
                    if (!(state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(count).SurfNum).Tilt > 170.0 &&
                          state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(count).SurfNum).Tilt < 190.0) &&
                        !(state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(count).SurfNum).Tilt > -10.0 &&
                          state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(count).SurfNum).Tilt < 10.0)) {
                        ShowWarningError(state, "An AirflowNetwork:Multizone:Surface object has an air-flow opening corresponding to");
                        ShowContinueError(state,
                                          "window or door = " + state.dataAirflowNetwork->MultizoneSurfaceData(count).SurfName + ", which is above");
                        ShowContinueError(state, "10 deg of being horizontal. Airflows through non-horizontal openings are not modeled");
                        ShowContinueError(state,
                                          "with the object of AirflowNetwork:Multizone:Component:HorizontalOpening = " +
                                              state.dataAirflowNetwork->AirflowNetworkCompData(compnum).Name);
                    }
                    if (!(state.dataSurface->SurfWinOriginalClass(state.dataAirflowNetwork->MultizoneSurfaceData(count).SurfNum) ==
                              SurfaceClass::Window ||
                          state.dataSurface->SurfWinOriginalClass(state.dataAirflowNetwork->MultizoneSurfaceData(count).SurfNum) ==
                              SurfaceClass::GlassDoor ||
                          state.dataSurface->SurfWinOriginalClass(state.dataAirflowNetwork->MultizoneSurfaceData(count).SurfNum) ==
                              SurfaceClass::Door ||
                          state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(count).SurfNum).IsAirBoundarySurf)) {
                        ShowSevereError(state,
                                        std::string{RoutineName} +
                                            "AirflowNetworkComponent: The opening must be assigned to a window, door, glassdoor or air boundary at " +
                                            state.dataAirflowNetwork->AirflowNetworkLinkageData(count).Name);
                        ErrorsFound = true;
                    }
                } break;
                default:
                    // Nothing to do here
                    break;
                }
            } else {
                ShowSevereError(state,
                                std::string{RoutineName} + CurrentModuleObject + ": The component is not defined in " +
                                    state.dataAirflowNetwork->AirflowNetworkLinkageData(count).Name);
                ErrorsFound = true;
            }
        }

        // Assign intrazone links
        for (count = 1 + state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces;
             count <= state.dataAirflowNetwork->NumOfLinksIntraZone + state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces;
             ++count) {
            state.dataAirflowNetwork->AirflowNetworkLinkageData(count).Name =
                state.dataAirflowNetwork->IntraZoneLinkageData(count - state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces).Name;
            state.dataAirflowNetwork->AirflowNetworkLinkageData(count).NodeNums[0] =
                state.dataAirflowNetwork->IntraZoneLinkageData(count - state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces).NodeNums[0];
            state.dataAirflowNetwork->AirflowNetworkLinkageData(count).NodeNums[1] =
                state.dataAirflowNetwork->IntraZoneLinkageData(count - state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces).NodeNums[1];
            state.dataAirflowNetwork->AirflowNetworkLinkageData(count).CompName =
                state.dataAirflowNetwork->IntraZoneLinkageData(count - state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces).CompName;
            state.dataAirflowNetwork->AirflowNetworkLinkageData(count).ZoneNum = 0;
            state.dataAirflowNetwork->AirflowNetworkLinkageData(count).LinkNum = count;
            state.dataAirflowNetwork->AirflowNetworkLinkageData(count).NodeHeights[0] =
                state.dataAirflowNetwork->IntraZoneLinkageData(count - state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces).NodeHeights[0];
            state.dataAirflowNetwork->AirflowNetworkLinkageData(count).NodeHeights[1] =
                state.dataAirflowNetwork->IntraZoneLinkageData(count - state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces).NodeHeights[1];
            // Find component number
            auto afe = solver.elements.find(state.dataAirflowNetwork->AirflowNetworkLinkageData(count).CompName);
            if (afe != solver.elements.end()) {
                state.dataAirflowNetwork->AirflowNetworkLinkageData(count).element = afe->second;
                // Get CompTypeNum here, this is a hack to hold us over until the introspection is dealt with
                auto compnum_iter = solver.compnum.find(state.dataAirflowNetwork->AirflowNetworkLinkageData(count).CompName);
                assert(compnum_iter != solver.compnum.end());
                int compnum = compnum_iter->second;
                state.dataAirflowNetwork->AirflowNetworkLinkageData(count).CompNum = compnum;
                if (state.dataAirflowNetwork->AirflowNetworkLinkageData(count).element->type() != ComponentType::SCR &&
                    state.dataAirflowNetwork->AirflowNetworkLinkageData(count).element->type() != ComponentType::SEL) {

                    ShowSevereError(state,
                                    std::string{RoutineName} + state.dataAirflowNetwork->AirflowNetworkLinkageData(count).CompName +
                                        ": The component is not allowed in " + state.dataAirflowNetwork->AirflowNetworkLinkageData(count).Name);
                    ShowContinueError(state,
                                      "The allowed component type is either AirflowNetwork:MultiZone:Surface:Crack or "
                                      "AirflowNetwork:MultiZone:Surface:EffectiveLeakageArea.");
                    ErrorsFound = true;
                }
            } else {
                ShowSevereError(state,
                                std::string{RoutineName} + state.dataAirflowNetwork->AirflowNetworkLinkageData(count).CompName +
                                    ": The component is not defined in " + state.dataAirflowNetwork->AirflowNetworkLinkageData(count).Name);
                ErrorsFound = true;
            }
        }

        // Reset state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces by including state.dataAirflowNetwork->NumOfLinksIntraZone
        state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces =
            state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces + state.dataAirflowNetwork->NumOfLinksIntraZone;
        if (state.dataAirflowNetwork->NumOfLinksIntraZone > 0)
            state.dataAirflowNetwork->NumOfLinksMultiZone = state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces;

        // Assign AirflowNetwork info in RoomAirflowNetworkZoneInfo
        if (state.dataAirflowNetwork->NumOfNodesIntraZone > 0) {
            for (int i = 1; i <= state.dataAirflowNetwork->NumOfNodesMultiZone; ++i) {
                n = state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusZoneNum;
                state.dataAirflowNetwork->AirflowNetworkNodeData(i).NumOfLinks = 0;
                if (n > 0 && state.dataAirflowNetwork->AirflowNetworkNodeData(i).RAFNNodeNum > 0) {
                    state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(n)
                        .Node(state.dataAirflowNetwork->AirflowNetworkNodeData(i).RAFNNodeNum)
                        .AirflowNetworkNodeID = i;
                    for (j = 1; j <= state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces; ++j) {
                        if (state.dataAirflowNetwork->AirflowNetworkLinkageData(j).NodeNums[0] == i) {
                            state.dataAirflowNetwork->AirflowNetworkNodeData(i).NumOfLinks =
                                state.dataAirflowNetwork->AirflowNetworkNodeData(i).NumOfLinks + 1;
                        } else if (state.dataAirflowNetwork->AirflowNetworkLinkageData(j).NodeNums[1] == i) {
                            state.dataAirflowNetwork->AirflowNetworkNodeData(i).NumOfLinks =
                                state.dataAirflowNetwork->AirflowNetworkNodeData(i).NumOfLinks + 1;
                        } else {
                        }
                    }
                }
                if (state.dataAirflowNetwork->AirflowNetworkNodeData(i).RAFNNodeNum > 0) {
                    for (j = 1; j <= state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(n).NumOfAirNodes; ++j) {
                        if (state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(n).Node(j).AirflowNetworkNodeID == i) {
                            state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(n).Node(j).NumOfAirflowLinks =
                                state.dataAirflowNetwork->AirflowNetworkNodeData(i).NumOfLinks;
                            state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(n).Node(j).Link.allocate(
                                state.dataAirflowNetwork->AirflowNetworkNodeData(i).NumOfLinks);
                            k = 1;
                            for (m = 1; m <= state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces; ++m) {
                                if (state.dataAirflowNetwork->AirflowNetworkLinkageData(m).NodeNums[0] == i) {
                                    state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(n).Node(j).Link(k).AirflowNetworkLinkSimuID = m;
                                    state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(n).Node(j).Link(k).AirflowNetworkLinkageDataID = m;
                                    k = k + 1;
                                    if (k > state.dataAirflowNetwork->AirflowNetworkNodeData(i).NumOfLinks) break;
                                }
                                if (state.dataAirflowNetwork->AirflowNetworkLinkageData(m).NodeNums[1] == i) {
                                    state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(n).Node(j).Link(k).AirflowNetworkLinkSimuID = m;
                                    state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(n).Node(j).Link(k).AirflowNetworkLinkageDataID = m;
                                    k = k + 1;
                                    if (k > state.dataAirflowNetwork->AirflowNetworkNodeData(i).NumOfLinks) break;
                                }
                            }
                        }
                    }
                }
            }
        }

        if (state.dataAirflowNetworkBalanceManager->DisSysNumOfLinks > 0 &&
            state.dataAirflowNetwork->SimulateAirflowNetwork > AirflowNetworkControlMultizone) { // Distribution

            for (auto &e : state.dataAirflowNetwork->AirflowNetworkLinkageData)
                e.ZoneNum = 0;

            for (count = state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces + 1; count <= state.dataAirflowNetwork->AirflowNetworkNumOfLinks;
                 ++count) {

                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         CurrentModuleObject,
                                                                         count - state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces,
                                                                         Alphas,
                                                                         NumAlphas,
                                                                         Numbers,
                                                                         NumNumbers,
                                                                         IOStatus,
                                                                         lNumericBlanks,
                                                                         lAlphaBlanks,
                                                                         cAlphaFields,
                                                                         cNumericFields);
                UtilityRoutines::IsNameEmpty(state, Alphas(1), CurrentModuleObject, ErrorsFound);
                state.dataAirflowNetwork->AirflowNetworkLinkageData(count).Name = Alphas(1);
                state.dataAirflowNetwork->AirflowNetworkLinkageData(count).NodeNames[0] = Alphas(2);
                state.dataAirflowNetwork->AirflowNetworkLinkageData(count).NodeHeights[0] = 0.0;
                state.dataAirflowNetwork->AirflowNetworkLinkageData(count).NodeNames[1] = Alphas(3);
                state.dataAirflowNetwork->AirflowNetworkLinkageData(count).NodeHeights[1] = 0.0;
                state.dataAirflowNetwork->AirflowNetworkLinkageData(count).CompName = Alphas(4);
                state.dataAirflowNetwork->AirflowNetworkLinkageData(count).ZoneName = Alphas(5);
                state.dataAirflowNetwork->AirflowNetworkLinkageData(count).LinkNum = count;

                for (int i = 1; i <= state.dataAirflowNetworkBalanceManager->DisSysNumOfDuctViewFactors; ++i) {
                    if (state.dataAirflowNetwork->AirflowNetworkLinkageData(count).Name ==
                        state.dataAirflowNetwork->AirflowNetworkLinkageViewFactorData(i).LinkageName) {
                        state.dataAirflowNetwork->AirflowNetworkLinkageData(count).LinkageViewFactorObjectNum =
                            state.dataAirflowNetwork->AirflowNetworkLinkageViewFactorData(i).ObjectNum;
                        break;
                    }
                }

                if (!lAlphaBlanks(5)) {
                    state.dataAirflowNetwork->AirflowNetworkLinkageData(count).ZoneNum =
                        UtilityRoutines::FindItemInList(state.dataAirflowNetwork->AirflowNetworkLinkageData(count).ZoneName, Zone);
                    if (state.dataAirflowNetwork->AirflowNetworkLinkageData(count).ZoneNum == 0) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + ": Invalid " + cAlphaFields(5) +
                                            " given = " + state.dataAirflowNetwork->AirflowNetworkLinkageData(count).ZoneName);
                        ErrorsFound = true;
                    }
                }
                if (Alphas(2) == Alphas(3)) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + CurrentModuleObject + ", " + cAlphaFields(2) + " = " + cAlphaFields(3) + " in " +
                                        state.dataAirflowNetwork->AirflowNetworkLinkageData(count).Name);
                    ErrorsFound = true;
                }
                // Find component number
                auto afe = solver.elements.find(state.dataAirflowNetwork->AirflowNetworkLinkageData(count).CompName);
                if (afe != solver.elements.end()) {
                    state.dataAirflowNetwork->AirflowNetworkLinkageData(count).element = afe->second;

                    // Get CompTypeNum here, this is a hack to hold us over until the introspection is dealt with
                    auto compnum_iter = solver.compnum.find(state.dataAirflowNetwork->AirflowNetworkLinkageData(count).CompName);
                    assert(compnum_iter != solver.compnum.end());
                    int compnum = compnum_iter->second;
                    state.dataAirflowNetwork->AirflowNetworkLinkageData(count).CompNum = compnum;
                } else {
                    ShowSevereError(state,
                                    std::string{RoutineName} + CurrentModuleObject + ": The " + cAlphaFields(4) + " is not defined in " +
                                        state.dataAirflowNetwork->AirflowNetworkLinkageData(count).Name);
                    ErrorsFound = true;
                }
                // Find Node number
                found = false;
                for (int i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfNodes; ++i) {
                    if (state.dataAirflowNetwork->AirflowNetworkLinkageData(count).NodeNames[0] ==
                        state.dataAirflowNetwork->AirflowNetworkNodeData(i).Name) {
                        state.dataAirflowNetwork->AirflowNetworkLinkageData(count).NodeNums[0] = i;
                        state.dataAirflowNetwork->AirflowNetworkLinkageData(count).NodeHeights[0] +=
                            state.dataAirflowNetwork->AirflowNetworkNodeData(i).NodeHeight;
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + CurrentModuleObject + ": The " + cAlphaFields(2) + " is not found in the node data " +
                                        state.dataAirflowNetwork->AirflowNetworkLinkageData(count).Name);
                    ErrorsFound = true;
                }
                for (int i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfNodes; ++i) {
                    if (state.dataAirflowNetwork->AirflowNetworkLinkageData(count).NodeNames[1] ==
                        state.dataAirflowNetwork->AirflowNetworkNodeData(i).Name) {
                        state.dataAirflowNetwork->AirflowNetworkLinkageData(count).NodeNums[1] = i;
                        state.dataAirflowNetwork->AirflowNetworkLinkageData(count).NodeHeights[1] +=
                            state.dataAirflowNetwork->AirflowNetworkNodeData(i).NodeHeight;
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + CurrentModuleObject + ": The " + cAlphaFields(3) + " is not found in the node data " +
                                        state.dataAirflowNetwork->AirflowNetworkLinkageData(count).Name);
                    ErrorsFound = true;
                }
            }
        } else {

            if (state.dataAirflowNetwork->SimulateAirflowNetwork > AirflowNetworkControlMultizone + 1) {
                ShowSevereError(state, std::string{RoutineName} + "An " + CurrentModuleObject + " object is required but not found.");
                ErrorsFound = true;
            }
        }

        // Ensure no duplicated names in AirflowNetwork component objects
        /*
        for (i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfComps; ++i) {
            for (j = i + 1; j <= state.dataAirflowNetwork->AirflowNetworkNumOfComps; ++j) {
                if (UtilityRoutines::SameString(state.dataAirflowNetwork->AirflowNetworkCompData(i).Name,
        state.dataAirflowNetwork->AirflowNetworkCompData(j).Name)) {
                    // SurfaceAirflowLeakageNames
                    if (i <= 4 && j <= 4) {
                        if (state.dataAirflowNetwork->AirflowNetworkCompData(i).CompTypeNum == iComponentTypeNum::DOP)
                            CompName(1) = "AirflowNetwork:MultiZone:Component:DetailedOpening";
                        if (state.dataAirflowNetwork->AirflowNetworkCompData(i).CompTypeNum == iComponentTypeNum::SOP)
                            CompName(1) = "AirflowNetwork:MultiZone:Component:SimpleOpening";
                        if (state.dataAirflowNetwork->AirflowNetworkCompData(i).CompTypeNum == iComponentTypeNum::SCR) CompName(1) =
        "AirflowNetwork:MultiZone:Surface:Crack"; if (state.dataAirflowNetwork->AirflowNetworkCompData(i).CompTypeNum ==
        iComponentTypeNum::SEL) CompName(1) = "AirflowNetwork:MultiZone:Surface:EffectiveLeakageArea"; if
        (state.dataAirflowNetwork->AirflowNetworkCompData(j).CompTypeNum == iComponentTypeNum::DOP) CompName(2) =
        "AirflowNetwork:MultiZone:Component:DetailedOpening"; if (state.dataAirflowNetwork->AirflowNetworkCompData(j).CompTypeNum ==
        iComponentTypeNum::SOP) CompName(2) = "AirflowNetwork:MultiZone:Component:SimpleOpening"; if
        (state.dataAirflowNetwork->AirflowNetworkCompData(j).CompTypeNum == iComponentTypeNum::SCR) CompName(2) =
        "AirflowNetwork:MultiZone:Surface:Crack"; if (state.dataAirflowNetwork->AirflowNetworkCompData(j).CompTypeNum ==
        iComponentTypeNum::SEL) CompName(2) = "AirflowNetwork:MultiZone:Surface:EffectiveLeakageArea"; ShowSevereError(state, RoutineName
        + "Duplicated component names are found = " + state.dataAirflowNetwork->AirflowNetworkCompData(i).Name); ShowContinueError(state,
        "A unique component name is required in both objects " + std::string{CompName}(1) + " and " + std::string{CompName}(2)); ErrorsFound = true;
                    }
                    // Distribution component
                    if (i > 4 && j > 4) {
                        if (state.dataAirflowNetwork->AirflowNetworkCompData(i).CompTypeNum == iComponentTypeNum::PLR) CompName(1) =
        "AirflowNetwork:Distribution:Component:Leak"; if (state.dataAirflowNetwork->AirflowNetworkCompData(i).CompTypeNum ==
        iComponentTypeNum::DWC) CompName(1) = "AirflowNetwork:Distribution:Component:Duct"; if
        (state.dataAirflowNetwork->AirflowNetworkCompData(i).CompTypeNum == iComponentTypeNum::ELR) CompName(1) =
        "AirflowNetwork:Distribution:Component:LeakageRatio"; if (state.dataAirflowNetwork->AirflowNetworkCompData(i).CompTypeNum ==
        iComponentTypeNum::DMP) CompName(1) = "AIRFLOWNETWORK:DISTRIBUTION:COMPONENT DAMPER"; if
        (state.dataAirflowNetwork->AirflowNetworkCompData(i).CompTypeNum == iComponentTypeNum::CVF) CompName(1) =
        "AirflowNetwork:Distribution:Component:Fan"; if (state.dataAirflowNetwork->AirflowNetworkCompData(i).CompTypeNum ==
        iComponentTypeNum::CPD) CompName(1) = "AirflowNetwork:Distribution:Component:ConstantPressureDrop"; if
        (state.dataAirflowNetwork->AirflowNetworkCompData(i).CompTypeNum == iComponentTypeNum::COI) CompName(1) =
        "AirflowNetwork:Distribution:Component:Coil"; if (state.dataAirflowNetwork->AirflowNetworkCompData(i).CompTypeNum ==
        iComponentTypeNum::TMU) CompName(1) = "AirflowNetwork:Distribution:Component:TerminalUnit"; if
        (state.dataAirflowNetwork->AirflowNetworkCompData(i).CompTypeNum == iComponentTypeNum::HEX) CompName(1) =
        "AirflowNetwork:Distribution:Component:HeatExchanger"; if (state.dataAirflowNetwork->AirflowNetworkCompData(j).CompTypeNum ==
        iComponentTypeNum::PLR) CompName(2) = "AirflowNetwork:Distribution:Component:Leak"; if
        (state.dataAirflowNetwork->AirflowNetworkCompData(j).CompTypeNum == iComponentTypeNum::DWC) CompName(2) =
        "AirflowNetwork:Distribution:Component:Duct"; if (state.dataAirflowNetwork->AirflowNetworkCompData(j).CompTypeNum ==
        iComponentTypeNum::ELR) CompName(2) = "AirflowNetwork:Distribution:Component:LeakageRatio"; if
        (state.dataAirflowNetwork->AirflowNetworkCompData(j).CompTypeNum == iComponentTypeNum::DMP) CompName(2) =
        "AIRFLOWNETWORK:DISTRIBUTION:COMPONENT DAMPER"; if (state.dataAirflowNetwork->AirflowNetworkCompData(j).CompTypeNum ==
        iComponentTypeNum::CVF) CompName(2) = "AirflowNetwork:Distribution:Component:Fan"; if
        (state.dataAirflowNetwork->AirflowNetworkCompData(j).CompTypeNum == iComponentTypeNum::CPD) CompName(2) =
        "AirflowNetwork:Distribution:Component:ConstantPressureDrop"; if (state.dataAirflowNetwork->AirflowNetworkCompData(j).CompTypeNum
        == iComponentTypeNum::COI) CompName(2) = "AirflowNetwork:Distribution:Component:Coil"; if
        (state.dataAirflowNetwork->AirflowNetworkCompData(j).CompTypeNum == iComponentTypeNum::TMU) CompName(2) =
        "AirflowNetwork:Distribution:Component:TerminalUnit"; if (state.dataAirflowNetwork->AirflowNetworkCompData(j).CompTypeNum ==
        iComponentTypeNum::HEX) CompName(2) = "AirflowNetwork:Distribution:Component:HeatExchanger"; ShowSevereError(state, std::string{RoutineName} +
        "Duplicated component names are found = " + state.dataAirflowNetwork->AirflowNetworkCompData(i).Name); ShowContinueError(state, "A
        unique component name is required in both objects " + std::string{CompName}(1) + " and " + std::string{CompName}(2)); ErrorsFound = true;
                    }
                }
            }
        }
        */

        // Node and component validation
        for (count = 1; count <= state.dataAirflowNetwork->AirflowNetworkNumOfLinks; ++count) {
            NodeFound = false;
            for (int i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfNodes; ++i) {
                if (i == state.dataAirflowNetwork->AirflowNetworkLinkageData(count).NodeNums[0]) {
                    NodeFound = true;
                    break;
                }
            }
            if (!NodeFound) {
                if (count <= state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + state.dataAirflowNetwork->AirflowNetworkLinkageData(count).NodeNames[0] +
                                        " in AIRFLOWNETWORK:MULTIZONE:SURFACE = " + state.dataAirflowNetwork->AirflowNetworkLinkageData(count).Name +
                                        " is not found");
                } else {
                    ShowSevereError(
                        state,
                        std::string{RoutineName} + state.dataAirflowNetwork->AirflowNetworkLinkageData(count).NodeNames[0] +
                            " in AIRFLOWNETWORK:DISTRIBUTION:LINKAGE = " + state.dataAirflowNetwork->AirflowNetworkLinkageData(count).Name +
                            " is not found in AIRFLOWNETWORK:DISTRIBUTION:NODE objects.");
                }
                ErrorsFound = true;
            }
            NodeFound = false;
            for (int i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfNodes; ++i) {
                if (i == state.dataAirflowNetwork->AirflowNetworkLinkageData(count).NodeNums[1]) {
                    NodeFound = true;
                    break;
                }
            }
            if (!NodeFound) {
                if (count <= state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + state.dataAirflowNetwork->AirflowNetworkLinkageData(count).NodeNames[0] +
                                        " in AIRFLOWNETWORK:MULTIZONE:SURFACE = " + state.dataAirflowNetwork->AirflowNetworkLinkageData(count).Name +
                                        " is not found");
                } else {
                    ShowSevereError(
                        state,
                        std::string{RoutineName} + state.dataAirflowNetwork->AirflowNetworkLinkageData(count).NodeNames[1] +
                            " in AIRFLOWNETWORK:DISTRIBUTION:LINKAGE = " + state.dataAirflowNetwork->AirflowNetworkLinkageData(count).Name +
                            " is not found in AIRFLOWNETWORK:DISTRIBUTION:NODE objects.");
                }
                ErrorsFound = true;
            }
            CompFound = false;
            for (int i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfComps; ++i) {
                if (i == state.dataAirflowNetwork->AirflowNetworkLinkageData(count).CompNum) {
                    CompFound = true;
                }
            }
            if (!CompFound) {
                ShowSevereError(state,
                                std::string{RoutineName} + "Component = " + state.dataAirflowNetwork->AirflowNetworkLinkageData(count).CompName +
                                    " in AIRFLOWNETWORK:DISTRIBUTION:LINKAGE = " + state.dataAirflowNetwork->AirflowNetworkLinkageData(count).Name +
                                    " is not found in AirflowNetwork Component Data objects.");
                ErrorsFound = true;
            }
        }

        // Ensure every AirflowNetworkNode is used in AirflowNetworkLinkage
        for (count = 1; count <= state.dataAirflowNetwork->AirflowNetworkNumOfNodes; ++count) {
            NodeFound1 = false;
            NodeFound2 = false;
            for (int i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfLinks; ++i) {
                if (count == state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0]) {
                    NodeFound1 = true;
                }
                if (count == state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1]) {
                    NodeFound2 = true;
                }
            }
            if ((!NodeFound1) && count > state.dataAirflowNetwork->NumOfNodesMultiZone &&
                state.dataAirflowNetwork->AirflowNetworkNodeData(count).ExtNodeNum == 0) {
                ShowSevereError(state,
                                std::string{RoutineName} + "AIRFLOWNETWORK:DISTRIBUTION:NODE = " + state.dataAirflowNetwork->AirflowNetworkNodeData(count).Name +
                                    " is not found as Node 1 Name in AIRFLOWNETWORK:DISTRIBUTION:LINKAGE");
                ShowContinueError(state,
                                  "Each non-external AIRFLOWNETWORK:DISTRIBUTION:NODE has to be defined as Node 1 once in "
                                  "AIRFLOWNETWORK:DISTRIBUTION:LINKAGE");
                ErrorsFound = true;
            }
            if ((!NodeFound2) && count > state.dataAirflowNetwork->NumOfNodesMultiZone &&
                state.dataAirflowNetwork->AirflowNetworkNodeData(count).ExtNodeNum == 0) {
                ShowSevereError(state,
                                std::string{RoutineName} + "AIRFLOWNETWORK:DISTRIBUTION:NODE = " + state.dataAirflowNetwork->AirflowNetworkNodeData(count).Name +
                                    " is not found as Node 2 Name in AIRFLOWNETWORK:DISTRIBUTION:LINKAGE");
                ShowContinueError(state,
                                  "Each non-external AIRFLOWNETWORK:DISTRIBUTION:NODE has to be defined as Node 2 once in "
                                  "AIRFLOWNETWORK:DISTRIBUTION:LINKAGE");
                ErrorsFound = true;
            }
            if ((!NodeFound1) && (!NodeFound2) && count > state.dataAirflowNetwork->NumOfNodesMultiZone &&
                state.dataAirflowNetwork->AirflowNetworkNodeData(count).ExtNodeNum > 0) {
                ShowSevereError(state,
                                std::string{RoutineName} + "AIRFLOWNETWORK:DISTRIBUTION:NODE = " + state.dataAirflowNetwork->AirflowNetworkNodeData(count).Name +
                                    " is not found as Node 1 Name or Node 2 Name in AIRFLOWNETWORK:DISTRIBUTION:LINKAGE");
                ShowContinueError(state, "This external AIRFLOWNETWORK:DISTRIBUTION:NODE has to be defined in AIRFLOWNETWORK:DISTRIBUTION:LINKAGE");
                ErrorsFound = true;
            }
        }

        // Ensure there is at least one node defined as EXTERNAL node
        NodeFound = false;
        for (count = 1; count <= state.dataAirflowNetwork->AirflowNetworkNumOfNodes; ++count) {
            if (state.dataAirflowNetwork->AirflowNetworkNodeData(count).ExtNodeNum > 0) {
                NodeFound = true;
            }
        }
        if (!NodeFound) {
            ShowSevereError(state,
                            std::string{RoutineName} +
                                "No External Nodes found in AirflowNetwork:Multizone:ExternalNode. There must be at least 1 external node defined.");
            ErrorsFound = true;
        }

        if (state.dataAirflowNetwork->AirflowNetworkSimu.iWPCCnt == iWPCCntr::Input) {
            for (count = 1; count <= state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces; ++count) {
                if (state.dataAirflowNetwork->AirflowNetworkLinkageData(count).NodeNums[0] == 0) {
                    ShowSevereError(state,
                                    "The surface is not found in AIRFLOWNETWORK:MULTIZONE:SURFACE = " +
                                        state.dataAirflowNetwork->AirflowNetworkLinkageData(count).Name);
                    ErrorsFound = true;
                }
                if (state.dataAirflowNetwork->AirflowNetworkLinkageData(count).NodeNums[1] == 0) {
                    ShowSevereError(state,
                                    "The external node is not found in AIRFLOWNETWORK:MULTIZONE:SURFACE = " +
                                        state.dataAirflowNetwork->AirflowNetworkLinkageData(count).Name);
                    ErrorsFound = true;
                }
            }
        }

        // Provide a warning when a door component is assigned as envelope leakage
        /*
        if (!ErrorsFound) {
            for (count = 1; count <= state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces; ++count) {
                if
        (state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(count).NodeNums[0]).ExtNodeNum
        > 0 &&
                    state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(count).NodeNums[1]).EPlusZoneNum
        > 0 && state.dataAirflowNetwork->AirflowNetworkLinkageData(count).CompNum > 0) { if
        (state.dataAirflowNetwork->AirflowNetworkCompData(state.dataAirflowNetwork->AirflowNetworkLinkageData(count).CompNum).CompTypeNum
        == iComponentTypeNum::SOP) {
                    }
                }
                if
        (state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(count).NodeNums[1]).ExtNodeNum
        > 0 &&
                    state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(count).NodeNums[0]).EPlusZoneNum
        > 0 && state.dataAirflowNetwork->AirflowNetworkLinkageData(count).CompNum > 0) { if
        (state.dataAirflowNetwork->AirflowNetworkCompData(state.dataAirflowNetwork->AirflowNetworkLinkageData(count).CompNum).CompTypeNum
        == iComponentTypeNum::SOP) {
                    }
                }
            }
        }
        */

        // Ensure the name of each heat exchanger is shown either once or twice in the field of
        if (state.dataAirflowNetwork->SimulateAirflowNetwork == AirflowNetworkControlSimpleADS ||
            state.dataAirflowNetwork->SimulateAirflowNetwork == AirflowNetworkControlMultiADS) {
            for (int i = 1; i <= state.dataAirflowNetworkBalanceManager->DisSysNumOfHXs; ++i) {
                count = 0;
                for (j = 1; j <= state.dataAirflowNetwork->AirflowNetworkNumOfLinks; ++j) {
                    if (UtilityRoutines::SameString(state.dataAirflowNetwork->AirflowNetworkLinkageData(j).CompName,
                                                    state.dataAirflowNetwork->DisSysCompHXData(i).name)) {
                        ++count;
                    }
                }

                if (state.dataAirflowNetwork->DisSysCompHXData(i).CoilParentExists && count != 2) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + "The inputs of component name field as a heat exchanger in "
                                                  "AIRFLOWNETWORK:DISTRIBUTION:LINKAGE is not correct");
                    ShowContinueError(state,
                                      "The entered name of heat exchanger is " + state.dataAirflowNetwork->DisSysCompHXData(i).name +
                                          " in AirflowNetwork:Distribution:Component:HeatExchanger objects");
                    ShowContinueError(state, format("The correct appearance number is 2. The entered appearance number is {}", count));
                    ErrorsFound = true;
                }
                if ((!state.dataAirflowNetwork->DisSysCompHXData(i).CoilParentExists) && count != 1) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + "The inputs of component name field as a heat exchanger in "
                                                  "AIRFLOWNETWORK:DISTRIBUTION:LINKAGE is not correct");
                    ShowContinueError(state,
                                      "The entered name of heat exchanger is " + state.dataAirflowNetwork->DisSysCompHXData(i).name +
                                          " in AirflowNetwork:Distribution:Component:HeatExchanger objects");
                    ShowContinueError(state, format("The correct appearance number is 1. The entered appearance number is {}", count));
                    ErrorsFound = true;
                }
            }
        }

        // Check node assignments using AirflowNetwork:Distribution:Component:OutdoorAirFlow or
        // AirflowNetwork:Distribution:Component:ReliefAirFlow
        for (count = state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces + 1; count <= state.dataAirflowNetwork->AirflowNetworkNumOfLinks;
             ++count) {
            int i = state.dataAirflowNetwork->AirflowNetworkLinkageData(count).CompNum;
            j = state.dataAirflowNetwork->AirflowNetworkLinkageData(count).NodeNums[0];
            k = state.dataAirflowNetwork->AirflowNetworkLinkageData(count).NodeNums[1];

            if (state.dataAirflowNetwork->AirflowNetworkCompData(i).CompTypeNum == iComponentTypeNum::OAF) {
                if (!UtilityRoutines::SameString(
                        state.dataAirflowNetwork->DisSysNodeData(j - state.dataAirflowNetwork->NumOfNodesMultiZone).EPlusType,
                        "OAMixerOutdoorAirStreamNode")) {
                    ShowSevereError(state,
                                    std::string{RoutineName} +
                                        "AirflowNetwork:Distribution:Linkage: When the component type is "
                                        "AirflowNetwork:Distribution:Component:OutdoorAirFlow at " +
                                        state.dataAirflowNetwork->AirflowNetworkNodeData(j).Name + ",");
                    ShowContinueError(state,
                                      "the component type in the first node should be OAMixerOutdoorAirStreamNode at " +
                                          state.dataAirflowNetwork->AirflowNetworkNodeData(j).Name);
                    ErrorsFound = true;
                }
                if (!UtilityRoutines::SameString(
                        state.dataAirflowNetwork->DisSysNodeData(k - state.dataAirflowNetwork->NumOfNodesMultiZone).EPlusType,
                        "AirLoopHVAC:OutdoorAirSystem")) {
                    ShowSevereError(state,
                                    std::string{RoutineName} +
                                        "AirflowNetwork:Distribution:Linkage: When the component type is "
                                        "AirflowNetwork:Distribution:Component:OutdoorAirFlow at " +
                                        state.dataAirflowNetwork->AirflowNetworkNodeData(k).Name + ",");
                    ShowContinueError(state,
                                      "the component object type in the second node should be AirLoopHVAC:OutdoorAirSystem at " +
                                          state.dataAirflowNetwork->AirflowNetworkNodeData(k).Name);
                    ErrorsFound = true;
                }
            }

            if (state.dataAirflowNetwork->AirflowNetworkCompData(i).CompTypeNum == iComponentTypeNum::REL) {
                if (!UtilityRoutines::SameString(
                        state.dataAirflowNetwork->DisSysNodeData(j - state.dataAirflowNetwork->NumOfNodesMultiZone).EPlusType,
                        "AirLoopHVAC:OutdoorAirSystem")) {
                    ShowSevereError(state,
                                    std::string{RoutineName} +
                                        "AirflowNetwork:Distribution:Linkage: When the component type is "
                                        "AirflowNetwork:Distribution:Component:OutdoorAirFlow at " +
                                        state.dataAirflowNetwork->AirflowNetworkNodeData(j).Name + ",");
                    ShowContinueError(state,
                                      "the component object type in the first node should be AirLoopHVAC:OutdoorAirSystem at " +
                                          state.dataAirflowNetwork->AirflowNetworkNodeData(j).Name);
                    ErrorsFound = true;
                }
                if (!UtilityRoutines::SameString(
                        state.dataAirflowNetwork->DisSysNodeData(k - state.dataAirflowNetwork->NumOfNodesMultiZone).EPlusType,
                        "OAMixerOutdoorAirStreamNode")) {
                    ShowSevereError(state,
                                    std::string{RoutineName} +
                                        "AirflowNetwork:Distribution:Linkage: When the component type is "
                                        "AirflowNetwork:Distribution:Component:OutdoorAirFlow at " +
                                        state.dataAirflowNetwork->AirflowNetworkNodeData(k).Name + ",");
                    ShowContinueError(state,
                                      "the component type in the second node should be OAMixerOutdoorAirStreamNode at " +
                                          state.dataAirflowNetwork->AirflowNetworkNodeData(k).Name);
                    ErrorsFound = true;
                }
            }
        }

        if (ErrorsFound) {
            ShowFatalError(state, std::string{RoutineName} + "Errors found getting inputs. Previous error(s) cause program termination.");
        }

        Alphas.deallocate();
        cAlphaFields.deallocate();
        cNumericFields.deallocate();
        Numbers.deallocate();
        lAlphaBlanks.deallocate();
        lNumericBlanks.deallocate();

        if (!ErrorsFound) {
            AllocateAndInitData(state);
        }
    }

} // namespace AirflowNetworkBalanceManager

void AirflowNetworkBalanceManagerData::initialize(EnergyPlusData &state)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Lixing Gu
    //       DATE WRITTEN   Aug. 2003
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine initializes variables of additional zone loads caused by ADS.

    // USE STATEMENTS:
    auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int i;
    int j;
    int ZoneNum;
    auto &Zone(state.dataHeatBal->Zone);

    if (initializeOneTimeFlag) {
        exchangeData.allocate(state.dataGlobal->NumOfZones); // AirflowNetwork exchange data due to air-forced system
        for (i = 1; i <= state.dataAirflowNetworkBalanceManager->DisSysNumOfCVFs; i++) {
            if (state.dataAirflowNetwork->DisSysCompCVFData(i).FanTypeNum == AirflowNetworkBalanceManager::FanType_SimpleOnOff) {
                multiExchangeData.allocate(state.dataGlobal->NumOfZones);
                break;
            }
        }

        initializeOneTimeFlag = false;
        if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
            for (i = 1; i <= state.dataGlobal->NumOfZones; ++i) {
                SetupOutputVariable(state,
                                    "AFN Zone Outdoor Air Mass Flow Rate",
                                    OutputProcessor::Unit::kg_s,
                                    exchangeData(i).SumMHr,
                                    "System",
                                    "Average",
                                    Zone(i).Name);
                SetupOutputVariable(
                    state, "AFN Zone Mixing Mass Flow Rate", OutputProcessor::Unit::kg_s, exchangeData(i).SumMMHr, "System", "Average", Zone(i).Name);
                SetupOutputVariable(state,
                                    "AFN Zone Outdoor Air CO2 Mass Flow Rate",
                                    OutputProcessor::Unit::kg_s,
                                    exchangeData(i).SumMHrCO,
                                    "System",
                                    "Average",
                                    Zone(i).Name);
                SetupOutputVariable(state,
                                    "AFN Zone Mixing CO2 Mass Flow Rate",
                                    OutputProcessor::Unit::kg_s,
                                    exchangeData(i).SumMMHrCO,
                                    "System",
                                    "Average",
                                    Zone(i).Name);
                SetupOutputVariable(state,
                                    "AFN Zone Total CO2 Mass Flow Rate",
                                    OutputProcessor::Unit::kg_s,
                                    exchangeData(i).TotalCO2,
                                    "System",
                                    "Average",
                                    Zone(i).Name);
            }
        }
        if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
            for (i = 1; i <= state.dataGlobal->NumOfZones; ++i) {
                if (!state.dataContaminantBalance->Contaminant.CO2Simulation) {
                    SetupOutputVariable(state,
                                        "AFN Zone Outdoor Air Mass Flow Rate",
                                        OutputProcessor::Unit::kg_s,
                                        exchangeData(i).SumMHr,
                                        "System",
                                        "Average",
                                        Zone(i).Name);
                    SetupOutputVariable(state,
                                        "AFN Zone Mixing Mass Flow Rate",
                                        OutputProcessor::Unit::kg_s,
                                        exchangeData(i).SumMMHr,
                                        "System",
                                        "Average",
                                        Zone(i).Name);
                }
                SetupOutputVariable(state,
                                    "AFN Zone Outdoor Air Generic Air Contaminant Mass Flow Rate",
                                    OutputProcessor::Unit::kg_s,
                                    exchangeData(i).SumMHrGC,
                                    "System",
                                    "Average",
                                    Zone(i).Name);
                SetupOutputVariable(state,
                                    "AFN Zone Mixing Generic Air Contaminant Mass Flow Rate",
                                    OutputProcessor::Unit::kg_s,
                                    exchangeData(i).SumMMHrGC,
                                    "System",
                                    "Average",
                                    Zone(i).Name);
                SetupOutputVariable(state,
                                    "AFN Zone Total Generic Air Contaminant Mass Flow Rate",
                                    OutputProcessor::Unit::kg_s,
                                    exchangeData(i).TotalGC,
                                    "System",
                                    "Average",
                                    Zone(i).Name);
            }
        }
    }

    if (state.dataGlobal->BeginEnvrnFlag && initializeMyEnvrnFlag) {
        // Assign node values
        for (i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfNodes; ++i) {
            state.dataAirflowNetwork->AirflowNetworkNodeSimu(i).TZ = 23.0;
            state.dataAirflowNetwork->AirflowNetworkNodeSimu(i).WZ = 0.00084;
            state.dataAirflowNetwork->AirflowNetworkNodeSimu(i).PZ = 0.0;
            state.dataAirflowNetwork->AirflowNetworkNodeSimu(i).TZlast = state.dataAirflowNetwork->AirflowNetworkNodeSimu(i).TZ;
            state.dataAirflowNetwork->AirflowNetworkNodeSimu(i).WZlast = state.dataAirflowNetwork->AirflowNetworkNodeSimu(i).WZ;
            if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
                state.dataAirflowNetwork->AirflowNetworkNodeSimu(i).CO2Z = state.dataContaminantBalance->OutdoorCO2;
                state.dataAirflowNetwork->AirflowNetworkNodeSimu(i).CO2Zlast = state.dataAirflowNetwork->AirflowNetworkNodeSimu(i).CO2Z;
            }
            if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
                state.dataAirflowNetwork->AirflowNetworkNodeSimu(i).GCZ = state.dataContaminantBalance->OutdoorGC;
                state.dataAirflowNetwork->AirflowNetworkNodeSimu(i).GCZlast = state.dataAirflowNetwork->AirflowNetworkNodeSimu(i).GCZ;
            }
            if (state.dataAirflowNetwork->AirflowNetworkNodeData(i).RAFNNodeNum > 0) {
                ZoneNum = state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusZoneNum;
                state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum)
                    .Node(state.dataAirflowNetwork->AirflowNetworkNodeData(i).RAFNNodeNum)
                    .AirTemp = 23.0;
                state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum)
                    .Node(state.dataAirflowNetwork->AirflowNetworkNodeData(i).RAFNNodeNum)
                    .HumRat = 0.0;
            }
        }

        for (i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfLinks; ++i) {
            state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW = 0.0;
            state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2 = 0.0;
        }

        for (i = 1; i <= state.dataGlobal->NumOfZones; ++i) {
            state.dataAirflowNetwork->ANZT(i) = state.dataHeatBalFanSys->MAT(i);
            state.dataAirflowNetwork->ANZW(i) = state.dataHeatBalFanSys->ZoneAirHumRat(i);
            if (state.dataContaminantBalance->Contaminant.CO2Simulation)
                state.dataAirflowNetwork->ANCO(i) = state.dataContaminantBalance->ZoneAirCO2(i);
            if (state.dataContaminantBalance->Contaminant.GenericContamSimulation)
                state.dataAirflowNetwork->ANGC(i) = state.dataContaminantBalance->ZoneAirGC(i);
        }
        if (state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfOccuVentCtrls > 0) {
            for (i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces; ++i) {
                if (state.dataAirflowNetwork->MultizoneSurfaceData(i).OccupantVentilationControlNum > 0) {
                    state.dataAirflowNetwork->MultizoneSurfaceData(i).PrevOpeningstatus = AirflowNetwork::OpenStatus::FreeOperation;
                    state.dataAirflowNetwork->MultizoneSurfaceData(i).CloseElapsedTime = 0.0;
                    state.dataAirflowNetwork->MultizoneSurfaceData(i).OpenElapsedTime = 0.0;
                    state.dataAirflowNetwork->MultizoneSurfaceData(i).OpeningStatus = AirflowNetwork::OpenStatus::FreeOperation;
                    state.dataAirflowNetwork->MultizoneSurfaceData(i).OpeningProbStatus = AirflowNetwork::ProbabilityCheck::NoAction;
                    state.dataAirflowNetwork->MultizoneSurfaceData(i).ClosingProbStatus = 0;
                }
            }
        }

        initializeMyEnvrnFlag = false;
    }
    if (!state.dataGlobal->BeginEnvrnFlag) {
        initializeMyEnvrnFlag = true;
        if (state.dataAirflowNetwork->SimulateAirflowNetwork > AirflowNetworkBalanceManager::AirflowNetworkControlSimple) {
            if (state.dataAirflowNetwork->RollBackFlag) {
                for (i = 1; i <= state.dataGlobal->NumOfZones; ++i) {
                    state.dataAirflowNetwork->ANZT(i) = state.dataHeatBalFanSys->XMAT(i);
                    state.dataAirflowNetwork->ANZW(i) = state.dataHeatBalFanSys->WZoneTimeMinus1(i);
                    if (state.dataContaminantBalance->Contaminant.CO2Simulation)
                        state.dataAirflowNetwork->ANCO(i) = state.dataContaminantBalance->CO2ZoneTimeMinus1(i);
                    if (state.dataContaminantBalance->Contaminant.GenericContamSimulation)
                        state.dataAirflowNetwork->ANGC(i) = state.dataContaminantBalance->GCZoneTimeMinus1(i);
                }
            } else {
                for (i = 1; i <= state.dataGlobal->NumOfZones; ++i) {
                    state.dataAirflowNetwork->ANZT(i) = state.dataHeatBalFanSys->MAT(i);
                    state.dataAirflowNetwork->ANZW(i) = state.dataHeatBalFanSys->ZoneAirHumRat(i);
                    if (state.dataContaminantBalance->Contaminant.CO2Simulation)
                        state.dataAirflowNetwork->ANCO(i) = state.dataContaminantBalance->ZoneAirCO2(i);
                    if (state.dataContaminantBalance->Contaminant.GenericContamSimulation)
                        state.dataAirflowNetwork->ANGC(i) = state.dataContaminantBalance->ZoneAirGC(i);
                }
            }

            for (i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfNodes; ++i) {
                if (state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusZoneNum > 0) {
                    state.dataAirflowNetwork->AirflowNetworkNodeSimu(i).TZ =
                        state.dataAirflowNetwork->ANZT(state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusZoneNum);
                    state.dataAirflowNetwork->AirflowNetworkNodeSimu(i).WZ =
                        state.dataAirflowNetwork->ANZW(state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusZoneNum);
                    if (state.dataContaminantBalance->Contaminant.CO2Simulation)
                        state.dataAirflowNetwork->AirflowNetworkNodeSimu(i).CO2Z =
                            state.dataAirflowNetwork->ANCO(state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusZoneNum);
                    if (state.dataContaminantBalance->Contaminant.GenericContamSimulation)
                        state.dataAirflowNetwork->AirflowNetworkNodeSimu(i).GCZ =
                            state.dataAirflowNetwork->ANGC(state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusZoneNum);
                }
                if (state.dataAirflowNetwork->AirflowNetworkNodeData(i).ExtNodeNum > 0) {
                    if (state.dataAirflowNetwork->AirflowNetworkNodeData(i).OutAirNodeNum > 0 &&
                        state.dataLoopNodes->Node(state.dataAirflowNetwork->AirflowNetworkNodeData(i).OutAirNodeNum).IsLocalNode) {
                        state.dataAirflowNetwork->AirflowNetworkNodeSimu(i).TZ =
                            state.dataLoopNodes->Node(state.dataAirflowNetwork->AirflowNetworkNodeData(i).OutAirNodeNum).OutAirDryBulb;
                        state.dataAirflowNetwork->AirflowNetworkNodeSimu(i).WZ =
                            state.dataLoopNodes->Node(state.dataAirflowNetwork->AirflowNetworkNodeData(i).OutAirNodeNum).HumRat;
                    } else {
                        state.dataAirflowNetwork->AirflowNetworkNodeSimu(i).TZ =
                            AirflowNetworkBalanceManager::OutDryBulbTempAt(state, state.dataAirflowNetwork->AirflowNetworkNodeData(i).NodeHeight);
                        state.dataAirflowNetwork->AirflowNetworkNodeSimu(i).WZ = state.dataEnvrn->OutHumRat;
                    }

                    if (state.dataContaminantBalance->Contaminant.CO2Simulation)
                        state.dataAirflowNetwork->AirflowNetworkNodeSimu(i).CO2Z = state.dataContaminantBalance->OutdoorCO2;
                    if (state.dataContaminantBalance->Contaminant.GenericContamSimulation)
                        state.dataAirflowNetwork->AirflowNetworkNodeSimu(i).GCZ = state.dataContaminantBalance->OutdoorGC;
                }

                if (state.dataAirflowNetwork->AirflowNetworkNodeData(i).RAFNNodeNum > 0) {
                    ZoneNum = state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusZoneNum;
                    if (state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum)
                            .Node(state.dataAirflowNetwork->AirflowNetworkNodeData(i).RAFNNodeNum)
                            .AirflowNetworkNodeID == i) {
                        state.dataAirflowNetwork->AirflowNetworkNodeSimu(i).TZ =
                            state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum)
                                .Node(state.dataAirflowNetwork->AirflowNetworkNodeData(i).RAFNNodeNum)
                                .AirTemp;
                        state.dataAirflowNetwork->AirflowNetworkNodeSimu(i).WZ =
                            state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum)
                                .Node(state.dataAirflowNetwork->AirflowNetworkNodeData(i).RAFNNodeNum)
                                .HumRat;
                    }
                }
            }
        }
    }

    for (auto &e : exchangeData) {
        e.TotalSen = 0.0;
        e.TotalLat = 0.0;
        e.MultiZoneSen = 0.0;
        e.MultiZoneLat = 0.0;
        e.LeakSen = 0.0;
        e.LeakLat = 0.0;
        e.CondSen = 0.0;
        e.DiffLat = 0.0;
        e.RadGain = 0.0;
    }
    if (state.dataContaminantBalance->Contaminant.CO2Simulation)
        for (auto &e : exchangeData)
            e.TotalCO2 = 0.0;
    if (state.dataContaminantBalance->Contaminant.GenericContamSimulation)
        for (auto &e : exchangeData)
            e.TotalGC = 0.0;

    // Occupant ventilation control
    Real64 CurrentEndTime = state.dataGlobal->CurrentTime + state.dataHVACGlobal->SysTimeElapsed;
    if (CurrentEndTime > state.dataAirflowNetworkBalanceManager->CurrentEndTimeLast &&
        TimeStepSys >= state.dataAirflowNetworkBalanceManager->TimeStepSysLast) {
        for (i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces; ++i) {
            if (i > state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces - state.dataAirflowNetwork->NumOfLinksIntraZone) continue;
            if (state.dataAirflowNetwork->MultizoneSurfaceData(i).OccupantVentilationControlNum > 0) {
                state.dataAirflowNetwork->MultizoneSurfaceData(i).PrevOpeningstatus = state.dataAirflowNetwork->MultizoneSurfaceData(i).OpeningStatus;
                state.dataAirflowNetwork->MultizoneSurfaceData(i).OpenFactorLast = state.dataAirflowNetwork->MultizoneSurfaceData(i).OpenFactor;
                if (state.dataAirflowNetwork->MultizoneSurfaceData(i).OpenFactor > 0.0) {
                    state.dataAirflowNetwork->MultizoneSurfaceData(i).OpenElapsedTime +=
                        (CurrentEndTime - state.dataAirflowNetworkBalanceManager->CurrentEndTimeLast) * 60.0;
                    state.dataAirflowNetwork->MultizoneSurfaceData(i).CloseElapsedTime = 0.0;
                } else {
                    state.dataAirflowNetwork->MultizoneSurfaceData(i).OpenElapsedTime = 0.0;
                    state.dataAirflowNetwork->MultizoneSurfaceData(i).CloseElapsedTime +=
                        (CurrentEndTime - state.dataAirflowNetworkBalanceManager->CurrentEndTimeLast) * 60.0;
                }
                j = state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum;
                state.dataAirflowNetworkBalanceManager
                    ->OccupantVentilationControl(state.dataAirflowNetwork->MultizoneSurfaceData(i).OccupantVentilationControlNum)
                    .calc(state,
                          state.dataSurface->Surface(j).Zone,
                          state.dataAirflowNetwork->MultizoneSurfaceData(i).OpenElapsedTime,
                          state.dataAirflowNetwork->MultizoneSurfaceData(i).CloseElapsedTime,
                          state.dataAirflowNetwork->MultizoneSurfaceData(i).OpeningStatus,
                          state.dataAirflowNetwork->MultizoneSurfaceData(i).OpeningProbStatus,
                          state.dataAirflowNetwork->MultizoneSurfaceData(i).ClosingProbStatus);
                if (state.dataAirflowNetwork->MultizoneSurfaceData(i).OpeningStatus == AirflowNetwork::OpenStatus::MinCheckForceOpen) {
                    state.dataAirflowNetwork->MultizoneSurfaceData(i).OpenFactor = state.dataAirflowNetwork->MultizoneSurfaceData(i).OpenFactorLast;
                }
                if (state.dataAirflowNetwork->MultizoneSurfaceData(i).OpeningStatus == AirflowNetwork::OpenStatus::MinCheckForceClose) {
                    state.dataAirflowNetwork->MultizoneSurfaceData(i).OpenFactor = 0.0;
                }
            }
        }
    }
    TimeStepSysLast = TimeStepSys;
    CurrentEndTimeLast = CurrentEndTime;
}

namespace AirflowNetworkBalanceManager {

    void AllocateAndInitData(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   Aug. 2003
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine initializes variables and allocates dynamic arrays.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int i;
        int ZoneNum;
        int n;
        int SurfNum;
        auto &Zone(state.dataHeatBal->Zone);

        state.dataAirflowNetwork->AirflowNetworkNodeSimu.allocate(
            state.dataAirflowNetwork->AirflowNetworkNumOfNodes); // Node simulation variable in air distribution system
        state.dataAirflowNetwork->AirflowNetworkLinkSimu.allocate(
            state.dataAirflowNetwork->AirflowNetworkNumOfLinks); // Link simulation variable in air distribution system
        state.dataAirflowNetworkBalanceManager->linkReport.allocate(
            state.dataAirflowNetwork->AirflowNetworkNumOfLinks); // Report link simulation variable in air distribution system

        for (i = 1; i <= state.dataAirflowNetworkBalanceManager->DisSysNumOfCVFs; i++) {
            if (state.dataAirflowNetwork->DisSysCompCVFData(i).FanTypeNum == FanType_SimpleOnOff) {
                state.dataAirflowNetworkBalanceManager->nodeReport.allocate(state.dataAirflowNetwork->AirflowNetworkNumOfZones);
                state.dataAirflowNetworkBalanceManager->linkReport1.allocate(state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces);
                break;
            }
        }

        state.dataAirflowNetworkBalanceManager->MA.allocate(state.dataAirflowNetwork->AirflowNetworkNumOfNodes *
                                                            state.dataAirflowNetwork->AirflowNetworkNumOfNodes);
        state.dataAirflowNetworkBalanceManager->MV.allocate(state.dataAirflowNetwork->AirflowNetworkNumOfNodes);
        state.dataAirflowNetworkBalanceManager->IVEC.allocate(state.dataAirflowNetwork->AirflowNetworkNumOfNodes + 20);

        state.dataAirflowNetwork->AirflowNetworkReportData.allocate(state.dataGlobal->NumOfZones);          // Report variables
        state.dataAirflowNetworkBalanceManager->AirflowNetworkZnRpt.allocate(state.dataGlobal->NumOfZones); // Report variables

        state.dataAirflowNetwork->ANZT.allocate(state.dataGlobal->NumOfZones); // Local zone air temperature for rollback use
        state.dataAirflowNetwork->ANZW.allocate(state.dataGlobal->NumOfZones); // Local zone humidity ratio for rollback use
        if (state.dataContaminantBalance->Contaminant.CO2Simulation)
            state.dataAirflowNetwork->ANCO.allocate(state.dataGlobal->NumOfZones); // Local zone CO2 for rollback use
        if (state.dataContaminantBalance->Contaminant.GenericContamSimulation)
            state.dataAirflowNetwork->ANGC.allocate(state.dataGlobal->NumOfZones); // Local zone generic contaminant for rollback use
        auto &solver = state.dataAFNSolver->solver;
        solver.allocate(state);

        bool OnOffFanFlag = false;
        for (i = 1; i <= state.dataAirflowNetworkBalanceManager->DisSysNumOfCVFs; i++) {
            if (state.dataAirflowNetwork->DisSysCompCVFData(i).FanTypeNum == FanType_SimpleOnOff) {
                OnOffFanFlag = true;
            }
        }

        // CurrentModuleObject='AirflowNetwork Simulations'
        for (i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfNodes; ++i) {
            SetupOutputVariable(state,
                                "AFN Node Temperature",
                                OutputProcessor::Unit::C,
                                state.dataAirflowNetwork->AirflowNetworkNodeSimu(i).TZ,
                                "System",
                                "Average",
                                state.dataAirflowNetwork->AirflowNetworkNodeData(i).Name);
            SetupOutputVariable(state,
                                "AFN Node Humidity Ratio",
                                OutputProcessor::Unit::kgWater_kgDryAir,
                                state.dataAirflowNetwork->AirflowNetworkNodeSimu(i).WZ,
                                "System",
                                "Average",
                                state.dataAirflowNetwork->AirflowNetworkNodeData(i).Name);
            if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
                SetupOutputVariable(state,
                                    "AFN Node CO2 Concentration",
                                    OutputProcessor::Unit::ppm,
                                    state.dataAirflowNetwork->AirflowNetworkNodeSimu(i).CO2Z,
                                    "System",
                                    "Average",
                                    state.dataAirflowNetwork->AirflowNetworkNodeData(i).Name);
            }
            if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
                SetupOutputVariable(state,
                                    "AFN Node Generic Air Contaminant Concentration",
                                    OutputProcessor::Unit::ppm,
                                    state.dataAirflowNetwork->AirflowNetworkNodeSimu(i).GCZ,
                                    "System",
                                    "Average",
                                    state.dataAirflowNetwork->AirflowNetworkNodeData(i).Name);
            }
            if (!(state.dataAirflowNetworkBalanceManager->SupplyFanType == FanType_SimpleOnOff &&
                  i <= state.dataAirflowNetwork->AirflowNetworkNumOfZones)) {
                SetupOutputVariable(state,
                                    "AFN Node Total Pressure",
                                    OutputProcessor::Unit::Pa,
                                    state.dataAirflowNetwork->AirflowNetworkNodeSimu(i).PZ,
                                    "System",
                                    "Average",
                                    state.dataAirflowNetwork->AirflowNetworkNodeData(i).Name);
            }
            if (state.dataAirflowNetwork->AirflowNetworkNodeData(i).ExtNodeNum > 0) {
                SetupOutputVariable(state,
                                    "AFN Node Wind Pressure",
                                    OutputProcessor::Unit::Pa,
                                    state.dataAirflowNetwork->AirflowNetworkNodeSimu(i).PZ,
                                    "System",
                                    "Average",
                                    state.dataAirflowNetwork->AirflowNetworkNodeData(i).Name);
            }
        }

        for (i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfLinks; ++i) {
            if (!(state.dataAirflowNetworkBalanceManager->SupplyFanType == FanType_SimpleOnOff &&
                  i <= state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces)) {
                SetupOutputVariable(state,
                                    "AFN Linkage Node 1 to Node 2 Mass Flow Rate",
                                    OutputProcessor::Unit::kg_s,
                                    state.dataAirflowNetworkBalanceManager->linkReport(i).FLOW,
                                    "System",
                                    "Average",
                                    state.dataAirflowNetwork->AirflowNetworkLinkageData(i).Name);
                SetupOutputVariable(state,
                                    "AFN Linkage Node 2 to Node 1 Mass Flow Rate",
                                    OutputProcessor::Unit::kg_s,
                                    state.dataAirflowNetworkBalanceManager->linkReport(i).FLOW2,
                                    "System",
                                    "Average",
                                    state.dataAirflowNetwork->AirflowNetworkLinkageData(i).Name);
                SetupOutputVariable(state,
                                    "AFN Linkage Node 1 to Node 2 Volume Flow Rate",
                                    OutputProcessor::Unit::m3_s,
                                    state.dataAirflowNetworkBalanceManager->linkReport(i).VolFLOW,
                                    "System",
                                    "Average",
                                    state.dataAirflowNetwork->AirflowNetworkLinkageData(i).Name);
                SetupOutputVariable(state,
                                    "AFN Linkage Node 2 to Node 1 Volume Flow Rate",
                                    OutputProcessor::Unit::m3_s,
                                    state.dataAirflowNetworkBalanceManager->linkReport(i).VolFLOW2,
                                    "System",
                                    "Average",
                                    state.dataAirflowNetwork->AirflowNetworkLinkageData(i).Name);
                SetupOutputVariable(state,
                                    "AFN Linkage Node 1 to Node 2 Pressure Difference",
                                    OutputProcessor::Unit::Pa,
                                    state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).DP,
                                    "System",
                                    "Average",
                                    state.dataAirflowNetwork->AirflowNetworkLinkageData(i).Name);
            }
        }

        for (i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces; ++i) {
            if (state.dataAirflowNetwork->AirflowNetworkLinkageData(i).element == nullptr) {
                // This is not great
                continue;
            }
            n = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).CompNum;
            if (state.dataAirflowNetwork->AirflowNetworkLinkageData(i).element->type() == ComponentType::DOP ||
                state.dataAirflowNetwork->AirflowNetworkLinkageData(i).element->type() == ComponentType::SOP ||
                state.dataAirflowNetwork->AirflowNetworkLinkageData(i).element->type() == ComponentType::HOP) {
                SurfNum = state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum;
                SetupOutputVariable(state,
                                    "AFN Surface Venting Window or Door Opening Factor",
                                    OutputProcessor::Unit::None,
                                    state.dataAirflowNetwork->MultizoneSurfaceData(i).OpenFactor,
                                    "System",
                                    "Average",
                                    state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfName);
                if (state.dataGlobal->AnyEnergyManagementSystemInModel) {
                    SetupEMSActuator(state,
                                     "AirFlow Network Window/Door Opening",
                                     state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfName,
                                     "Venting Opening Factor",
                                     "[Fraction]",
                                     state.dataAirflowNetwork->MultizoneSurfaceData(i).EMSOpenFactorActuated,
                                     state.dataAirflowNetwork->MultizoneSurfaceData(i).EMSOpenFactor);
                }
                SetupOutputVariable(state,
                                    "AFN Surface Venting Window or Door Opening Modulation Multiplier",
                                    OutputProcessor::Unit::None,
                                    state.dataSurface->SurfWinVentingOpenFactorMultRep(SurfNum),
                                    "System",
                                    "Average",
                                    state.dataSurface->Surface(SurfNum).Name);
                SetupOutputVariable(state,
                                    "AFN Surface Venting Inside Setpoint Temperature",
                                    OutputProcessor::Unit::C,
                                    state.dataSurface->SurfWinInsideTempForVentingRep(SurfNum),
                                    "System",
                                    "Average",
                                    state.dataSurface->Surface(SurfNum).Name);
                SetupOutputVariable(state,
                                    "AFN Surface Venting Availability Status",
                                    OutputProcessor::Unit::None,
                                    state.dataSurface->SurfWinVentingAvailabilityRep(SurfNum),
                                    "System",
                                    "Average",
                                    state.dataSurface->Surface(SurfNum).Name);
                if (state.dataAirflowNetwork->MultizoneSurfaceData(i).OccupantVentilationControlNum > 0) {
                    SetupOutputVariable(state,
                                        "AFN Surface Venting Window or Door Opening Factor at Previous Time Step",
                                        OutputProcessor::Unit::None,
                                        state.dataAirflowNetwork->MultizoneSurfaceData(i).OpenFactorLast,
                                        "System",
                                        "Average",
                                        state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfName);
                    SetupOutputVariable(state,
                                        "AFN Surface Opening Elapsed Time",
                                        OutputProcessor::Unit::min,
                                        state.dataAirflowNetwork->MultizoneSurfaceData(i).OpenElapsedTime,
                                        "System",
                                        "Average",
                                        state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfName);
                    SetupOutputVariable(state,
                                        "AFN Surface Closing Elapsed Time",
                                        OutputProcessor::Unit::min,
                                        state.dataAirflowNetwork->MultizoneSurfaceData(i).CloseElapsedTime,
                                        "System",
                                        "Average",
                                        state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfName);
                    SetupOutputVariable(state,
                                        "AFN Surface Opening Status at Previous Time Step",
                                        OutputProcessor::Unit::None,
                                        state.dataAirflowNetwork->MultizoneSurfaceData(i).PrevOpeningstatus,
                                        "System",
                                        "Average",
                                        state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfName);
                    SetupOutputVariable(state,
                                        "AFN Surface Opening Status",
                                        OutputProcessor::Unit::None,
                                        state.dataAirflowNetwork->MultizoneSurfaceData(i).OpeningStatus,
                                        "System",
                                        "Average",
                                        state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfName);
                    SetupOutputVariable(state,
                                        "AFN Surface Opening Probability Status",
                                        OutputProcessor::Unit::None,
                                        state.dataAirflowNetwork->MultizoneSurfaceData(i).OpeningProbStatus,
                                        "System",
                                        "Average",
                                        state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfName);
                    SetupOutputVariable(state,
                                        "AFN Surface Closing Probability Status",
                                        OutputProcessor::Unit::None,
                                        state.dataAirflowNetwork->MultizoneSurfaceData(i).ClosingProbStatus,
                                        "System",
                                        "Average",
                                        state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfName);
                }
            }
        }

        for (i = 1; i <= state.dataGlobal->NumOfZones; ++i) {
            // Multizone losses due to force air systems
            SetupOutputVariable(state,
                                "AFN Zone Infiltration Sensible Heat Gain Rate",
                                OutputProcessor::Unit::W,
                                state.dataAirflowNetwork->AirflowNetworkReportData(i).MultiZoneInfiSenGainW,
                                "System",
                                "Average",
                                Zone(i).Name);
            SetupOutputVariable(state,
                                "AFN Zone Infiltration Sensible Heat Gain Energy",
                                OutputProcessor::Unit::J,
                                state.dataAirflowNetwork->AirflowNetworkReportData(i).MultiZoneInfiSenGainJ,
                                "System",
                                "Sum",
                                Zone(i).Name);
            SetupOutputVariable(state,
                                "AFN Zone Ventilation Sensible Heat Gain Rate",
                                OutputProcessor::Unit::W,
                                state.dataAirflowNetwork->AirflowNetworkReportData(i).MultiZoneVentSenGainW,
                                "System",
                                "Average",
                                Zone(i).Name);
            SetupOutputVariable(state,
                                "AFN Zone Ventilation Sensible Heat Gain Energy",
                                OutputProcessor::Unit::J,
                                state.dataAirflowNetwork->AirflowNetworkReportData(i).MultiZoneVentSenGainJ,
                                "System",
                                "Sum",
                                Zone(i).Name);
            SetupOutputVariable(state,
                                "AFN Zone Mixing Sensible Heat Gain Rate",
                                OutputProcessor::Unit::W,
                                state.dataAirflowNetwork->AirflowNetworkReportData(i).MultiZoneMixSenGainW,
                                "System",
                                "Average",
                                Zone(i).Name);
            SetupOutputVariable(state,
                                "AFN Zone Mixing Sensible Heat Gain Energy",
                                OutputProcessor::Unit::J,
                                state.dataAirflowNetwork->AirflowNetworkReportData(i).MultiZoneMixSenGainJ,
                                "System",
                                "Sum",
                                Zone(i).Name);
            SetupOutputVariable(state,
                                "AFN Zone Infiltration Sensible Heat Loss Rate",
                                OutputProcessor::Unit::W,
                                state.dataAirflowNetwork->AirflowNetworkReportData(i).MultiZoneInfiSenLossW,
                                "System",
                                "Average",
                                Zone(i).Name);
            SetupOutputVariable(state,
                                "AFN Zone Infiltration Sensible Heat Loss Energy",
                                OutputProcessor::Unit::J,
                                state.dataAirflowNetwork->AirflowNetworkReportData(i).MultiZoneInfiSenLossJ,
                                "System",
                                "Sum",
                                Zone(i).Name);
            SetupOutputVariable(state,
                                "AFN Zone Ventilation Sensible Heat Loss Rate",
                                OutputProcessor::Unit::W,
                                state.dataAirflowNetwork->AirflowNetworkReportData(i).MultiZoneVentSenLossW,
                                "System",
                                "Average",
                                Zone(i).Name);
            SetupOutputVariable(state,
                                "AFN Zone Ventilation Sensible Heat Loss Energy",
                                OutputProcessor::Unit::J,
                                state.dataAirflowNetwork->AirflowNetworkReportData(i).MultiZoneVentSenLossJ,
                                "System",
                                "Sum",
                                Zone(i).Name);
            SetupOutputVariable(state,
                                "AFN Zone Mixing Sensible Heat Loss Rate",
                                OutputProcessor::Unit::W,
                                state.dataAirflowNetwork->AirflowNetworkReportData(i).MultiZoneMixSenLossW,
                                "System",
                                "Average",
                                Zone(i).Name);
            SetupOutputVariable(state,
                                "AFN Zone Mixing Sensible Heat Loss Energy",
                                OutputProcessor::Unit::J,
                                state.dataAirflowNetwork->AirflowNetworkReportData(i).MultiZoneMixSenLossJ,
                                "System",
                                "Sum",
                                Zone(i).Name);
            SetupOutputVariable(state,
                                "AFN Zone Infiltration Latent Heat Gain Rate",
                                OutputProcessor::Unit::W,
                                state.dataAirflowNetwork->AirflowNetworkReportData(i).MultiZoneInfiLatGainW,
                                "System",
                                "Average",
                                Zone(i).Name);
            SetupOutputVariable(state,
                                "AFN Zone Infiltration Latent Heat Gain Energy",
                                OutputProcessor::Unit::J,
                                state.dataAirflowNetwork->AirflowNetworkReportData(i).MultiZoneInfiLatGainJ,
                                "System",
                                "Sum",
                                Zone(i).Name);
            SetupOutputVariable(state,
                                "AFN Zone Infiltration Latent Heat Loss Rate",
                                OutputProcessor::Unit::W,
                                state.dataAirflowNetwork->AirflowNetworkReportData(i).MultiZoneInfiLatLossW,
                                "System",
                                "Average",
                                Zone(i).Name);
            SetupOutputVariable(state,
                                "AFN Zone Infiltration Latent Heat Loss Energy",
                                OutputProcessor::Unit::J,
                                state.dataAirflowNetwork->AirflowNetworkReportData(i).MultiZoneInfiLatLossJ,
                                "System",
                                "Sum",
                                Zone(i).Name);
            SetupOutputVariable(state,
                                "AFN Zone Ventilation Latent Heat Gain Rate",
                                OutputProcessor::Unit::W,
                                state.dataAirflowNetwork->AirflowNetworkReportData(i).MultiZoneVentLatGainW,
                                "System",
                                "Average",
                                Zone(i).Name);
            SetupOutputVariable(state,
                                "AFN Zone Ventilation Latent Heat Gain Energy",
                                OutputProcessor::Unit::J,
                                state.dataAirflowNetwork->AirflowNetworkReportData(i).MultiZoneVentLatGainJ,
                                "System",
                                "Sum",
                                Zone(i).Name);
            SetupOutputVariable(state,
                                "AFN Zone Ventilation Latent Heat Loss Rate",
                                OutputProcessor::Unit::W,
                                state.dataAirflowNetwork->AirflowNetworkReportData(i).MultiZoneVentLatLossW,
                                "System",
                                "Average",
                                Zone(i).Name);
            SetupOutputVariable(state,
                                "AFN Zone Ventilation Latent Heat Loss Energy",
                                OutputProcessor::Unit::J,
                                state.dataAirflowNetwork->AirflowNetworkReportData(i).MultiZoneVentLatLossJ,
                                "System",
                                "Sum",
                                Zone(i).Name);
            SetupOutputVariable(state,
                                "AFN Zone Mixing Latent Heat Gain Rate",
                                OutputProcessor::Unit::W,
                                state.dataAirflowNetwork->AirflowNetworkReportData(i).MultiZoneMixLatGainW,
                                "System",
                                "Average",
                                Zone(i).Name);
            SetupOutputVariable(state,
                                "AFN Zone Mixing Latent Heat Gain Energy",
                                OutputProcessor::Unit::J,
                                state.dataAirflowNetwork->AirflowNetworkReportData(i).MultiZoneMixLatGainJ,
                                "System",
                                "Sum",
                                Zone(i).Name);
            SetupOutputVariable(state,
                                "AFN Zone Mixing Latent Heat Loss Rate",
                                OutputProcessor::Unit::W,
                                state.dataAirflowNetwork->AirflowNetworkReportData(i).MultiZoneMixLatLossW,
                                "System",
                                "Average",
                                Zone(i).Name);
            SetupOutputVariable(state,
                                "AFN Zone Mixing Latent Heat Loss Energy",
                                OutputProcessor::Unit::J,
                                state.dataAirflowNetwork->AirflowNetworkReportData(i).MultiZoneInfiLatLossJ,
                                "System",
                                "Sum",
                                Zone(i).Name);
            // Supply leak losses due to force air systems
            SetupOutputVariable(state,
                                "AFN Zone Duct Leaked Air Sensible Heat Gain Rate",
                                OutputProcessor::Unit::W,
                                state.dataAirflowNetwork->AirflowNetworkReportData(i).LeakSenGainW,
                                "System",
                                "Average",
                                Zone(i).Name);
            SetupOutputVariable(state,
                                "AFN Zone Duct Leaked Air Sensible Heat Gain Energy",
                                OutputProcessor::Unit::J,
                                state.dataAirflowNetwork->AirflowNetworkReportData(i).LeakSenGainJ,
                                "System",
                                "Sum",
                                Zone(i).Name);
            SetupOutputVariable(state,
                                "AFN Zone Duct Leaked Air Sensible Heat Loss Rate",
                                OutputProcessor::Unit::W,
                                state.dataAirflowNetwork->AirflowNetworkReportData(i).LeakSenLossW,
                                "System",
                                "Average",
                                Zone(i).Name);
            SetupOutputVariable(state,
                                "AFN Zone Duct Leaked Air Sensible Heat Loss Energy",
                                OutputProcessor::Unit::J,
                                state.dataAirflowNetwork->AirflowNetworkReportData(i).LeakSenLossJ,
                                "System",
                                "Sum",
                                Zone(i).Name);
            SetupOutputVariable(state,
                                "AFN Zone Duct Leaked Air Latent Heat Gain Rate",
                                OutputProcessor::Unit::W,
                                state.dataAirflowNetwork->AirflowNetworkReportData(i).LeakLatGainW,
                                "System",
                                "Average",
                                Zone(i).Name);
            SetupOutputVariable(state,
                                "AFN Zone Duct Leaked Air Latent Heat Gain Energy",
                                OutputProcessor::Unit::J,
                                state.dataAirflowNetwork->AirflowNetworkReportData(i).LeakLatGainJ,
                                "System",
                                "Sum",
                                Zone(i).Name);
            SetupOutputVariable(state,
                                "AFN Zone Duct Leaked Air Latent Heat Loss Rate",
                                OutputProcessor::Unit::W,
                                state.dataAirflowNetwork->AirflowNetworkReportData(i).LeakLatLossW,
                                "System",
                                "Average",
                                Zone(i).Name);
            SetupOutputVariable(state,
                                "AFN Zone Duct Leaked Air Latent Heat Loss Energy",
                                OutputProcessor::Unit::J,
                                state.dataAirflowNetwork->AirflowNetworkReportData(i).LeakLatLossJ,
                                "System",
                                "Sum",
                                Zone(i).Name);
            // Conduction losses due to force air systems
            SetupOutputVariable(state,
                                "AFN Zone Duct Conduction Sensible Heat Gain Rate",
                                OutputProcessor::Unit::W,
                                state.dataAirflowNetwork->AirflowNetworkReportData(i).CondSenGainW,
                                "System",
                                "Average",
                                Zone(i).Name);
            SetupOutputVariable(state,
                                "AFN Zone Duct Conduction Sensible Heat Gain Energy",
                                OutputProcessor::Unit::J,
                                state.dataAirflowNetwork->AirflowNetworkReportData(i).CondSenGainJ,
                                "System",
                                "Sum",
                                Zone(i).Name);
            SetupOutputVariable(state,
                                "AFN Zone Duct Conduction Sensible Heat Loss Rate",
                                OutputProcessor::Unit::W,
                                state.dataAirflowNetwork->AirflowNetworkReportData(i).CondSenLossW,
                                "System",
                                "Average",
                                Zone(i).Name);
            SetupOutputVariable(state,
                                "AFN Zone Duct Conduction Sensible Heat Loss Energy",
                                OutputProcessor::Unit::J,
                                state.dataAirflowNetwork->AirflowNetworkReportData(i).CondSenLossJ,
                                "System",
                                "Sum",
                                Zone(i).Name);
            SetupOutputVariable(state,
                                "AFN Zone Duct Diffusion Latent Heat Gain Rate",
                                OutputProcessor::Unit::W,
                                state.dataAirflowNetwork->AirflowNetworkReportData(i).DiffLatGainW,
                                "System",
                                "Average",
                                Zone(i).Name);
            SetupOutputVariable(state,
                                "AFN Zone Duct Diffusion Latent Heat Gain Energy",
                                OutputProcessor::Unit::J,
                                state.dataAirflowNetwork->AirflowNetworkReportData(i).DiffLatGainJ,
                                "System",
                                "Sum",
                                Zone(i).Name);
            SetupOutputVariable(state,
                                "AFN Zone Duct Diffusion Latent Heat Loss Rate",
                                OutputProcessor::Unit::W,
                                state.dataAirflowNetwork->AirflowNetworkReportData(i).DiffLatLossW,
                                "System",
                                "Average",
                                Zone(i).Name);
            SetupOutputVariable(state,
                                "AFN Zone Duct Diffusion Latent Heat Loss Energy",
                                OutputProcessor::Unit::J,
                                state.dataAirflowNetwork->AirflowNetworkReportData(i).DiffLatLossJ,
                                "System",
                                "Sum",
                                Zone(i).Name);
            // Radiation losses due to forced air systems
            SetupOutputVariable(state,
                                "AFN Zone Duct Radiation Heat Gain Rate",
                                OutputProcessor::Unit::W,
                                state.dataAirflowNetwork->AirflowNetworkReportData(i).RadGainW,
                                "System",
                                "Average",
                                Zone(i).Name);
            SetupOutputVariable(state,
                                "AFN Zone Duct Radiation Sensible Heat Gain Energy",
                                OutputProcessor::Unit::J,
                                state.dataAirflowNetwork->AirflowNetworkReportData(i).RadGainJ,
                                "System",
                                "Sum",
                                Zone(i).Name);
            SetupOutputVariable(state,
                                "AFN Zone Duct Radiation Heat Loss Rate",
                                OutputProcessor::Unit::W,
                                state.dataAirflowNetwork->AirflowNetworkReportData(i).RadLossW,
                                "System",
                                "Average",
                                Zone(i).Name);
            SetupOutputVariable(state,
                                "AFN Zone Duct Radiation Sensible Heat Loss Energy",
                                OutputProcessor::Unit::J,
                                state.dataAirflowNetwork->AirflowNetworkReportData(i).RadLossJ,
                                "System",
                                "Sum",
                                Zone(i).Name);
            // Total losses due to force air systems
            SetupOutputVariable(state,
                                "AFN Distribution Sensible Heat Gain Rate",
                                OutputProcessor::Unit::W,
                                state.dataAirflowNetwork->AirflowNetworkReportData(i).TotalSenGainW,
                                "System",
                                "Average",
                                Zone(i).Name);
            SetupOutputVariable(state,
                                "AFN Distribution Sensible Heat Gain Energy",
                                OutputProcessor::Unit::J,
                                state.dataAirflowNetwork->AirflowNetworkReportData(i).TotalSenGainJ,
                                "System",
                                "Sum",
                                Zone(i).Name);
            SetupOutputVariable(state,
                                "AFN Distribution Sensible Heat Loss Rate",
                                OutputProcessor::Unit::W,
                                state.dataAirflowNetwork->AirflowNetworkReportData(i).TotalSenLossW,
                                "System",
                                "Average",
                                Zone(i).Name);
            SetupOutputVariable(state,
                                "AFN Distribution Sensible Heat Loss Energy",
                                OutputProcessor::Unit::J,
                                state.dataAirflowNetwork->AirflowNetworkReportData(i).TotalSenLossJ,
                                "System",
                                "Sum",
                                Zone(i).Name);
            SetupOutputVariable(state,
                                "AFN Distribution Latent Heat Gain Rate",
                                OutputProcessor::Unit::W,
                                state.dataAirflowNetwork->AirflowNetworkReportData(i).TotalLatGainW,
                                "System",
                                "Average",
                                Zone(i).Name);
            SetupOutputVariable(state,
                                "AFN Distribution Latent Heat Gain Energy",
                                OutputProcessor::Unit::J,
                                state.dataAirflowNetwork->AirflowNetworkReportData(i).TotalLatGainJ,
                                "System",
                                "Sum",
                                Zone(i).Name);
            SetupOutputVariable(state,
                                "AFN Distribution Latent Heat Loss Rate",
                                OutputProcessor::Unit::W,
                                state.dataAirflowNetwork->AirflowNetworkReportData(i).TotalLatLossW,
                                "System",
                                "Average",
                                Zone(i).Name);
            SetupOutputVariable(state,
                                "AFN Distribution Latent Heat Loss Energy",
                                OutputProcessor::Unit::J,
                                state.dataAirflowNetwork->AirflowNetworkReportData(i).TotalLatLossJ,
                                "System",
                                "Sum",
                                Zone(i).Name);
        }

        for (i = 1; i <= state.dataGlobal->NumOfZones; ++i) {
            SetupOutputVariable(state,
                                "AFN Zone Infiltration Volume",
                                OutputProcessor::Unit::m3,
                                state.dataAirflowNetworkBalanceManager->AirflowNetworkZnRpt(i).InfilVolume,
                                "System",
                                "Sum",
                                Zone(i).Name);
            SetupOutputVariable(state,
                                "AFN Zone Infiltration Mass",
                                OutputProcessor::Unit::kg,
                                state.dataAirflowNetworkBalanceManager->AirflowNetworkZnRpt(i).InfilMass,
                                "System",
                                "Sum",
                                Zone(i).Name);
            SetupOutputVariable(state,
                                "AFN Zone Infiltration Air Change Rate",
                                OutputProcessor::Unit::ach,
                                state.dataAirflowNetworkBalanceManager->AirflowNetworkZnRpt(i).InfilAirChangeRate,
                                "System",
                                "Average",
                                Zone(i).Name);
            SetupOutputVariable(state,
                                "AFN Zone Ventilation Volume",
                                OutputProcessor::Unit::m3,
                                state.dataAirflowNetworkBalanceManager->AirflowNetworkZnRpt(i).VentilVolume,
                                "System",
                                "Sum",
                                Zone(i).Name);
            SetupOutputVariable(state,
                                "AFN Zone Ventilation Mass",
                                OutputProcessor::Unit::kg,
                                state.dataAirflowNetworkBalanceManager->AirflowNetworkZnRpt(i).VentilMass,
                                "System",
                                "Sum",
                                Zone(i).Name);
            SetupOutputVariable(state,
                                "AFN Zone Ventilation Air Change Rate",
                                OutputProcessor::Unit::ach,
                                state.dataAirflowNetworkBalanceManager->AirflowNetworkZnRpt(i).VentilAirChangeRate,
                                "System",
                                "Average",
                                Zone(i).Name);
            SetupOutputVariable(state,
                                "AFN Zone Mixing Volume",
                                OutputProcessor::Unit::m3,
                                state.dataAirflowNetworkBalanceManager->AirflowNetworkZnRpt(i).MixVolume,
                                "System",
                                "Sum",
                                Zone(i).Name);
            SetupOutputVariable(state,
                                "AFN Zone Mixing Mass",
                                OutputProcessor::Unit::kg,
                                state.dataAirflowNetworkBalanceManager->AirflowNetworkZnRpt(i).MixMass,
                                "System",
                                "Sum",
                                Zone(i).Name);

            SetupOutputVariable(state,
                                "AFN Zone Exfiltration Heat Transfer Rate",
                                OutputProcessor::Unit::W,
                                state.dataAirflowNetworkBalanceManager->AirflowNetworkZnRpt(i).ExfilTotalLoss,
                                "System",
                                "Average",
                                Zone(i).Name);
            SetupOutputVariable(state,
                                "AFN Zone Exfiltration Sensible Heat Transfer Rate",
                                OutputProcessor::Unit::W,
                                state.dataAirflowNetworkBalanceManager->AirflowNetworkZnRpt(i).ExfilSensiLoss,
                                "System",
                                "Average",
                                Zone(i).Name);
            SetupOutputVariable(state,
                                "AFN Zone Exfiltration Latent Heat Transfer Rate",
                                OutputProcessor::Unit::W,
                                state.dataAirflowNetworkBalanceManager->AirflowNetworkZnRpt(i).ExfilLatentLoss,
                                "System",
                                "Average",
                                Zone(i).Name);
        }

        if (OnOffFanFlag) {
            for (i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfZones; ++i) {
                SetupOutputVariable(state,
                                    "AFN Zone Average Pressure",
                                    OutputProcessor::Unit::Pa,
                                    state.dataAirflowNetworkBalanceManager->nodeReport(i).PZ,
                                    "System",
                                    "Average",
                                    Zone(i).Name);
                SetupOutputVariable(state,
                                    "AFN Zone On Cycle Pressure",
                                    OutputProcessor::Unit::Pa,
                                    state.dataAirflowNetworkBalanceManager->nodeReport(i).PZON,
                                    "System",
                                    "Average",
                                    Zone(i).Name);
                SetupOutputVariable(state,
                                    "AFN Zone Off Cycle Pressure",
                                    OutputProcessor::Unit::Pa,
                                    state.dataAirflowNetworkBalanceManager->nodeReport(i).PZOFF,
                                    "System",
                                    "Average",
                                    Zone(i).Name);
            }
            for (i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces; ++i) {
                SetupOutputVariable(state,
                                    "AFN Linkage Node 1 to 2 Average Mass Flow Rate",
                                    OutputProcessor::Unit::kg_s,
                                    state.dataAirflowNetworkBalanceManager->linkReport1(i).FLOW,
                                    "System",
                                    "Average",
                                    state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfName);
                SetupOutputVariable(state,
                                    "AFN Linkage Node 2 to 1 Average Mass Flow Rate",
                                    OutputProcessor::Unit::kg_s,
                                    state.dataAirflowNetworkBalanceManager->linkReport1(i).FLOW2,
                                    "System",
                                    "Average",
                                    state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfName);
                SetupOutputVariable(state,
                                    "AFN Linkage Node 1 to 2 Average Volume Flow Rate",
                                    OutputProcessor::Unit::m3_s,
                                    state.dataAirflowNetworkBalanceManager->linkReport1(i).VolFLOW,
                                    "System",
                                    "Average",
                                    state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfName);
                SetupOutputVariable(state,
                                    "AFN Linkage Node 2 to 1 Average Volume Flow Rate",
                                    OutputProcessor::Unit::m3_s,
                                    state.dataAirflowNetworkBalanceManager->linkReport1(i).VolFLOW2,
                                    "System",
                                    "Average",
                                    state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfName);
                SetupOutputVariable(state,
                                    "AFN Surface Average Pressure Difference",
                                    OutputProcessor::Unit::Pa,
                                    state.dataAirflowNetworkBalanceManager->linkReport1(i).DP,
                                    "System",
                                    "Average",
                                    state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfName);
                SetupOutputVariable(state,
                                    "AFN Surface On Cycle Pressure Difference",
                                    OutputProcessor::Unit::Pa,
                                    state.dataAirflowNetworkBalanceManager->linkReport1(i).DPON,
                                    "System",
                                    "Average",
                                    state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfName);
                SetupOutputVariable(state,
                                    "AFN Surface Off Cycle Pressure Difference",
                                    OutputProcessor::Unit::Pa,
                                    state.dataAirflowNetworkBalanceManager->linkReport1(i).DPOFF,
                                    "System",
                                    "Average",
                                    state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfName);
            }
        }

        // Assign node reference height
        for (i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfNodes; ++i) {
            if (!state.dataAirflowNetwork->AirflowNetworkSimu.TExtHeightDep) state.dataAirflowNetwork->AirflowNetworkNodeData(i).NodeHeight = 0.0;
            ZoneNum = state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusZoneNum;
            if (ZoneNum > 0) {
                if (state.dataSurface->WorldCoordSystem) {
                    state.dataAirflowNetwork->AirflowNetworkNodeData(i).NodeHeight = 0.0;
                } else {
                    state.dataAirflowNetwork->AirflowNetworkNodeData(i).NodeHeight = Zone(ZoneNum).OriginZ;
                }
            }
        }
    }

    void CalcAirflowNetworkAirBalance(EnergyPlusData &state)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   Oct. 2005
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine performs simulations of nodal pressures and linkage airflows.

        // Using/Aliasing
        using DataHVACGlobals::VerySmallMassFlow;
        using General::SolveRoot;

        // SUBROUTINE PARAMETER DEFINITIONS:
        int const CycFanCycComp(1); // fan cycles with compressor operation

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int i;
        int j;
        int n;
        int NodeNum;
        Real64 GlobalOpenFactor;
        Real64 ZonePressure1;
        Real64 ZonePressure2;
        Real64 PressureSet;
        Real64 LocalAzimuth;
        Real64 LocalWindSpeed;
        Real64 LocalWindDir;
        Real64 LocalHumRat;
        Real64 LocalDryBulb;
        Array1D<Real64> Par; // Pressure setpoint
        Real64 const ErrorToler(0.00001);
        int const MaxIte(20);
        int SolFla;
        Real64 MinExhaustMassFlowrate;
        Real64 MaxExhaustMassFlowrate;
        Real64 MinReliefMassFlowrate;
        Real64 MaxReliefMassFlowrate;
        int AirLoopNum;

        auto &Node(state.dataLoopNodes->Node);

        // Validate supply and return connections
        if (state.dataAirflowNetworkBalanceManager->CalcAirflowNetworkAirBalanceOneTimeFlag) {
            state.dataAirflowNetworkBalanceManager->CalcAirflowNetworkAirBalanceOneTimeFlag = false;
            if (state.dataAirflowNetworkBalanceManager->CalcAirflowNetworkAirBalanceErrorsFound) {
                ShowFatalError(state, "GetAirflowNetworkInput: Program terminates for preceding reason(s).");
            }
        }

        for (n = 1; n <= state.dataAFNSolver->NetworkNumOfNodes; ++n) {
            if (state.dataAirflowNetwork->AirflowNetworkNodeData(n).NodeTypeNum == 0) {
                state.dataAirflowNetwork->AirflowNetworkNodeSimu(n).PZ = 0.0;
            } else {
                // Assigning ambient conditions to external nodes
                i = state.dataAirflowNetwork->AirflowNetworkNodeData(n).ExtNodeNum;
                if (i > 0) {
                    state.dataAirflowNetwork->AirflowNetworkNodeSimu(n).TZ =
                        OutDryBulbTempAt(state, state.dataAirflowNetwork->AirflowNetworkNodeData(n).NodeHeight);
                    state.dataAirflowNetwork->AirflowNetworkNodeSimu(n).WZ = state.dataEnvrn->OutHumRat;
                    if (i <= state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfExtNode) {
                        if (state.dataAirflowNetwork->MultizoneExternalNodeData(i).OutAirNodeNum == 0) {
                            LocalWindSpeed = DataEnvironment::WindSpeedAt(state, state.dataAirflowNetwork->MultizoneExternalNodeData(i).height);
                            LocalDryBulb = OutDryBulbTempAt(state, state.dataAirflowNetwork->AirflowNetworkNodeData(n).NodeHeight);
                            LocalAzimuth = state.dataAirflowNetwork->MultizoneExternalNodeData(i).azimuth;
                            state.dataAirflowNetwork->AirflowNetworkNodeSimu(n).PZ =
                                CalcWindPressure(state,
                                                 state.dataAirflowNetwork->MultizoneExternalNodeData(i).curve,
                                                 state.dataAirflowNetwork->MultizoneExternalNodeData(i).symmetricCurve,
                                                 state.dataAirflowNetwork->MultizoneExternalNodeData(i).useRelativeAngle,
                                                 LocalAzimuth,
                                                 LocalWindSpeed,
                                                 state.dataEnvrn->WindDir,
                                                 LocalDryBulb,
                                                 state.dataEnvrn->OutHumRat);
                        } else {
                            // If and outdoor air node object is defined as the External Node Name in AirflowNetwork:MultiZone:Surface,
                            // the node object requires to define the Wind Pressure Coefficient Curve Name.
                            NodeNum = state.dataAirflowNetwork->MultizoneExternalNodeData(i).OutAirNodeNum;
                            LocalWindSpeed = Node((NodeNum)).OutAirWindSpeed;
                            LocalWindDir = Node((NodeNum)).OutAirWindDir;
                            LocalHumRat = Node((NodeNum)).HumRat;
                            LocalDryBulb = Node((NodeNum)).OutAirDryBulb;
                            LocalAzimuth = state.dataAirflowNetwork->MultizoneExternalNodeData(i).azimuth;
                            state.dataAirflowNetwork->AirflowNetworkNodeSimu(n).PZ =
                                CalcWindPressure(state,
                                                 state.dataAirflowNetwork->MultizoneExternalNodeData(i).curve,
                                                 state.dataAirflowNetwork->MultizoneExternalNodeData(i).symmetricCurve,
                                                 state.dataAirflowNetwork->MultizoneExternalNodeData(i).useRelativeAngle,
                                                 LocalAzimuth,
                                                 LocalWindSpeed,
                                                 LocalWindDir,
                                                 LocalDryBulb,
                                                 LocalHumRat);
                            state.dataAirflowNetwork->AirflowNetworkNodeSimu(n).TZ = LocalDryBulb;
                            state.dataAirflowNetwork->AirflowNetworkNodeSimu(n).WZ = LocalHumRat;
                        }
                    }

                } else {
                    ShowSevereError(state,
                                    "GetAirflowNetworkInput: AIRFLOWNETWORK:DISTRIBUTION:NODE: Invalid external node = " +
                                        state.dataAirflowNetwork->AirflowNetworkNodeData(n).Name);
                    state.dataAirflowNetworkBalanceManager->CalcAirflowNetworkAirBalanceErrorsFound = true;
                }
            }
        }

        for (i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces; ++i) {
            if (i > state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces - state.dataAirflowNetwork->NumOfLinksIntraZone) continue;
            if (state.dataAirflowNetwork->MultizoneSurfaceData(i).OccupantVentilationControlNum == 0)
                state.dataAirflowNetwork->MultizoneSurfaceData(i).OpenFactor = 0.0;
            j = state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum;
            if (state.dataSurface->SurfWinOriginalClass(j) == SurfaceClass::Window ||
                state.dataSurface->SurfWinOriginalClass(j) == SurfaceClass::Door ||
                state.dataSurface->SurfWinOriginalClass(j) == SurfaceClass::GlassDoor || state.dataSurface->Surface(j).IsAirBoundarySurf) {
                if (state.dataAirflowNetwork->MultizoneSurfaceData(i).OccupantVentilationControlNum > 0) {
                    if (state.dataAirflowNetwork->MultizoneSurfaceData(i).OpeningStatus == OpenStatus::FreeOperation) {
                        if (state.dataAirflowNetwork->MultizoneSurfaceData(i).OpeningProbStatus == ProbabilityCheck::ForceChange) {
                            state.dataAirflowNetwork->MultizoneSurfaceData(i).OpenFactor = state.dataAirflowNetwork->MultizoneSurfaceData(i).Factor;
                        } else if (state.dataAirflowNetwork->MultizoneSurfaceData(i).ClosingProbStatus == ProbabilityCheck::ForceChange) {
                            state.dataAirflowNetwork->MultizoneSurfaceData(i).OpenFactor = 0.0;
                        } else if (state.dataAirflowNetwork->MultizoneSurfaceData(i).ClosingProbStatus == ProbabilityCheck::KeepStatus ||
                                   state.dataAirflowNetwork->MultizoneSurfaceData(i).OpeningProbStatus == ProbabilityCheck::KeepStatus) {
                            state.dataAirflowNetwork->MultizoneSurfaceData(i).OpenFactor =
                                state.dataAirflowNetwork->MultizoneSurfaceData(i).OpenFactorLast;
                        } else {
                            AirflowNetworkVentingControl(state, i, state.dataAirflowNetwork->MultizoneSurfaceData(i).OpenFactor);
                        }
                    }
                } else {
                    AirflowNetworkVentingControl(state, i, state.dataAirflowNetwork->MultizoneSurfaceData(i).OpenFactor);
                }
                state.dataAirflowNetwork->MultizoneSurfaceData(i).OpenFactor *= state.dataAirflowNetwork->MultizoneSurfaceData(i).WindModifier;
                if (state.dataAirflowNetwork->MultizoneSurfaceData(i).HybridVentClose) {
                    state.dataAirflowNetwork->MultizoneSurfaceData(i).OpenFactor = 0.0;
                    if (state.dataSurface->SurfWinVentingOpenFactorMultRep(j) > 0.0) state.dataSurface->SurfWinVentingOpenFactorMultRep(j) = 0.0;
                }
                if (state.dataAirflowNetwork->AirflowNetworkCompData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).CompNum).CompTypeNum ==
                        iComponentTypeNum::DOP ||
                    state.dataAirflowNetwork->AirflowNetworkCompData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).CompNum).CompTypeNum ==
                        iComponentTypeNum::SOP ||
                    state.dataAirflowNetwork->AirflowNetworkCompData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).CompNum).CompTypeNum ==
                        iComponentTypeNum::HOP) {
                    if (state.dataAirflowNetwork->AirflowNetworkFanActivated &&
                        (state.dataAirflowNetwork->SimulateAirflowNetwork > AirflowNetworkControlMultizone) &&
                        state.dataAirflowNetwork->MultizoneSurfaceData(i).OpenFactor > 0.0 &&
                        (state.dataSurface->Surface(j).ExtBoundCond == ExternalEnvironment ||
                         (state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).ExtBoundCond ==
                              OtherSideCoefNoCalcExt &&
                          state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).ExtWind)) &&
                        !state.dataGlobal->WarmupFlag) {
                        // Exterior Large opening only
                        ++state.dataAirflowNetwork->MultizoneSurfaceData(i).ExtLargeOpeningErrCount;
                        if (state.dataAirflowNetwork->MultizoneSurfaceData(i).ExtLargeOpeningErrCount < 2) {
                            ShowWarningError(state,
                                             "AirflowNetwork: The window or door is open during HVAC system operation " +
                                                 state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfName);
                            ShowContinueError(
                                state,
                                format("The window or door opening factor is {:.2R}", state.dataAirflowNetwork->MultizoneSurfaceData(i).OpenFactor));
                            ShowContinueErrorTimeStamp(state, "");
                        } else {
                            ShowRecurringWarningErrorAtEnd(state,
                                                           "AirFlowNetwork: " + state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfName +
                                                               " The window or door is open during HVAC system operation error continues...",
                                                           state.dataAirflowNetwork->MultizoneSurfaceData(i).ExtLargeOpeningErrIndex,
                                                           state.dataAirflowNetwork->MultizoneSurfaceData(i).OpenFactor,
                                                           state.dataAirflowNetwork->MultizoneSurfaceData(i).OpenFactor);
                        }
                    }
                }
                if (state.dataAirflowNetwork->MultizoneSurfaceData(i).OpenFactor > 1.0) {
                    ++state.dataAirflowNetwork->MultizoneSurfaceData(i).OpenFactorErrCount;
                    if (state.dataAirflowNetwork->MultizoneSurfaceData(i).OpenFactorErrCount < 2) {
                        ShowWarningError(state,
                                         "AirflowNetwork: The window or door opening factor is greater than 1.0 " +
                                             state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfName);
                        ShowContinueErrorTimeStamp(state, "");
                    } else {
                        ShowRecurringWarningErrorAtEnd(state,
                                                       "AirFlowNetwork: " + state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfName +
                                                           " The window or door opening factor is greater than 1.0 error continues...",
                                                       state.dataAirflowNetwork->MultizoneSurfaceData(i).OpenFactorErrIndex,
                                                       state.dataAirflowNetwork->MultizoneSurfaceData(i).OpenFactor,
                                                       state.dataAirflowNetwork->MultizoneSurfaceData(i).OpenFactor);
                    }
                }
            }
        }

        // Check if the global ventilation control is applied or not
        GlobalOpenFactor = -1.0;
        for (i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces; ++i) {
            if (i > state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces - state.dataAirflowNetwork->NumOfLinksIntraZone) continue;
            if (state.dataAirflowNetwork->MultizoneSurfaceData(i).HybridCtrlMaster) {
                GlobalOpenFactor = state.dataAirflowNetwork->MultizoneSurfaceData(i).OpenFactor;
                break;
            }
        }
        if (GlobalOpenFactor >= 0.0) {
            for (i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces; ++i) {
                if (i > state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces - state.dataAirflowNetwork->NumOfLinksIntraZone) continue;
                j = state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum;
                if (state.dataSurface->SurfWinOriginalClass(j) == SurfaceClass::Window ||
                    state.dataSurface->SurfWinOriginalClass(j) == SurfaceClass::Door ||
                    state.dataSurface->SurfWinOriginalClass(j) == SurfaceClass::GlassDoor) {
                    if (state.dataAirflowNetwork->MultizoneSurfaceData(i).HybridCtrlGlobal) {
                        state.dataAirflowNetwork->MultizoneSurfaceData(i).OpenFactor = GlobalOpenFactor;
                    }
                }
            }
        }

        if (!Par.allocated()) {
            Par.allocate(1);
            Par = 0.0;
        }

        state.dataAirflowNetwork->PressureSetFlag = 0;

        if (state.dataAirflowNetworkBalanceManager->NumOfPressureControllers == 1) {
            if (state.dataAirflowNetwork->PressureControllerData(1).AvailSchedPtr == DataGlobalConstants::ScheduleAlwaysOn) {
                state.dataAirflowNetwork->PressureSetFlag = state.dataAirflowNetwork->PressureControllerData(1).ControlTypeSet;
            } else {
                if (GetCurrentScheduleValue(state, state.dataAirflowNetwork->PressureControllerData(1).AvailSchedPtr) > 0.0) {
                    state.dataAirflowNetwork->PressureSetFlag = state.dataAirflowNetwork->PressureControllerData(1).ControlTypeSet;
                }
            }
            if (state.dataAirflowNetwork->PressureSetFlag > 0) {
                PressureSet = GetCurrentScheduleValue(state, state.dataAirflowNetwork->PressureControllerData(1).PresSetpointSchedPtr);
            }
        }
        auto &solver = state.dataAFNSolver->solver;
        solver.initialize(state);

        if (!(state.dataAirflowNetwork->PressureSetFlag > 0 && state.dataAirflowNetwork->AirflowNetworkFanActivated)) {
            solver.airmov(state);
        } else if (state.dataAirflowNetwork->PressureSetFlag == PressureCtrlExhaust) {
            AirLoopNum = state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->PressureControllerData(1).AFNNodeNum).AirLoopNum;
            MinExhaustMassFlowrate = 2.0 * VerySmallMassFlow;
            MaxExhaustMassFlowrate = Node(state.dataAirflowNetwork->PressureControllerData(1).OANodeNum).MassFlowRate;
            if (state.dataAirLoop->AirLoopAFNInfo(AirLoopNum).LoopFanOperationMode == CycFanCycComp &&
                state.dataAirLoop->AirLoopAFNInfo(AirLoopNum).LoopOnOffFanPartLoadRatio > 0.0) {
                MaxExhaustMassFlowrate = MaxExhaustMassFlowrate / state.dataAirLoop->AirLoopAFNInfo(AirLoopNum).LoopOnOffFanPartLoadRatio;
            }
            state.dataAirflowNetwork->ExhaustFanMassFlowRate = MinExhaustMassFlowrate;
            solver.airmov(state);
            ZonePressure1 = state.dataAirflowNetwork->AirflowNetworkNodeSimu(state.dataAirflowNetwork->PressureControllerData(1).AFNNodeNum).PZ;
            if (ZonePressure1 <= PressureSet) {
                // The highest pressure due to minimum flow rate could not reach Pressure set, bypass pressure set calculation
                if (!state.dataGlobal->WarmupFlag) {
                    if (state.dataAirflowNetworkBalanceManager->ErrCountLowPre == 0) {
                        ++state.dataAirflowNetworkBalanceManager->ErrCountLowPre;
                        ShowWarningError(state,
                                         "The calculated pressure with minimum exhaust fan rate is lower than the pressure setpoint. The pressure "
                                         "control is unable to perform.");
                        ShowContinueErrorTimeStamp(state,
                                                   format("Calculated pressure = {:.2R}[Pa], Pressure setpoint ={:.2R}", ZonePressure1, PressureSet));
                    } else {
                        ++state.dataAirflowNetworkBalanceManager->ErrCountLowPre;
                        ShowRecurringWarningErrorAtEnd(
                            state,
                            state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->PressureControllerData(1).AFNNodeNum).Name +
                                ": The AFN model continues not to perform pressure control due to lower zone pressure...",
                            state.dataAirflowNetworkBalanceManager->ErrIndexLowPre,
                            ZonePressure1,
                            ZonePressure1);
                    }
                }
            } else {
                state.dataAirflowNetwork->ExhaustFanMassFlowRate = MaxExhaustMassFlowrate;
                solver.airmov(state);
                ZonePressure2 = state.dataAirflowNetwork->AirflowNetworkNodeSimu(state.dataAirflowNetwork->PressureControllerData(1).AFNNodeNum).PZ;
                if (ZonePressure2 >= PressureSet) {
                    // The lowest pressure due to maximum flow rate is still higher than Pressure set, bypass pressure set calculation
                    if (!state.dataGlobal->WarmupFlag) {
                        if (state.dataAirflowNetworkBalanceManager->ErrCountHighPre == 0) {
                            ++state.dataAirflowNetworkBalanceManager->ErrCountHighPre;
                            ShowWarningError(state,
                                             "The calculated pressure with maximum exhaust fan rate is higher than the pressure setpoint. The "
                                             "pressure control is unable to perform.");
                            ShowContinueErrorTimeStamp(
                                state, format("Calculated pressure = {:.2R}[Pa], Pressure setpoint = {:.2R}", ZonePressure2, PressureSet));
                        } else {
                            ++state.dataAirflowNetworkBalanceManager->ErrCountHighPre;
                            ShowRecurringWarningErrorAtEnd(
                                state,
                                state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->PressureControllerData(1).AFNNodeNum)
                                        .Name +
                                    ": The AFN model continues not to perform pressure control due to higher zone pressure...",
                                state.dataAirflowNetworkBalanceManager->ErrIndexHighPre,
                                ZonePressure2,
                                ZonePressure2);
                        }
                    }
                } else {
                    //    if ( ZonePressure1 > PressureSet && ZonePressure2 < PressureSet ) {
                    Par(1) = PressureSet;
                    General::SolveRoot(state,
                                       ErrorToler,
                                       MaxIte,
                                       SolFla,
                                       state.dataAirflowNetwork->ExhaustFanMassFlowRate,
                                       AFNPressureResidual,
                                       MinExhaustMassFlowrate,
                                       MaxExhaustMassFlowrate,
                                       Par);
                    if (SolFla == -1) {
                        if (!state.dataGlobal->WarmupFlag) {
                            if (state.dataAirflowNetworkBalanceManager->ErrCountVar == 0) {
                                ++state.dataAirflowNetworkBalanceManager->ErrCountVar;
                                ShowWarningError(state, "Iteration limit exceeded pressure setpoint using an exhaust fan. Simulation continues.");
                                ShowContinueErrorTimeStamp(
                                    state, format("Exhaust fan flow rate = {:.4R}", state.dataAirflowNetwork->ExhaustFanMassFlowRate));
                            } else {
                                ++state.dataAirflowNetworkBalanceManager->ErrCountVar;
                                ShowRecurringWarningErrorAtEnd(state,
                                                               state.dataAirflowNetwork->PressureControllerData(1).Name +
                                                                   "\": Iteration limit warning exceeding pressure setpoint continues...",
                                                               state.dataAirflowNetworkBalanceManager->ErrIndexVar,
                                                               state.dataAirflowNetwork->ExhaustFanMassFlowRate,
                                                               state.dataAirflowNetwork->ExhaustFanMassFlowRate);
                            }
                        }
                    } else if (SolFla == -2) {
                        ShowFatalError(state,
                                       "Zone pressure control failed using an exhaust fan: no solution is reached, for " +
                                           state.dataAirflowNetwork->PressureControllerData(1).Name);
                    }
                }
            }
        } else { // PressureCtrlRelief - Pressure control type is Relief Flow
            AirLoopNum = state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->PressureControllerData(1).AFNNodeNum).AirLoopNum;
            MinReliefMassFlowrate = 2.0 * VerySmallMassFlow;
            MaxReliefMassFlowrate = Node(state.dataAirflowNetwork->PressureControllerData(1).OANodeNum).MassFlowRate;
            if (state.dataAirLoop->AirLoopAFNInfo(AirLoopNum).LoopFanOperationMode == CycFanCycComp &&
                state.dataAirLoop->AirLoopAFNInfo(AirLoopNum).LoopOnOffFanPartLoadRatio > 0.0) {
                MaxReliefMassFlowrate = MaxReliefMassFlowrate / state.dataAirLoop->AirLoopAFNInfo(AirLoopNum).LoopOnOffFanPartLoadRatio;
            }
            state.dataAirflowNetwork->ReliefMassFlowRate = MinReliefMassFlowrate;
            solver.initialize(state);
            solver.airmov(state);
            ZonePressure1 = state.dataAirflowNetwork->AirflowNetworkNodeSimu(state.dataAirflowNetwork->PressureControllerData(1).AFNNodeNum).PZ;

            if (ZonePressure1 <= PressureSet) {
                // The highest pressure due to minimum flow rate could not reach Pressure set, bypass pressure set calculation
                if (!state.dataGlobal->WarmupFlag) {
                    if (state.dataAirflowNetworkBalanceManager->ErrCountLowPre == 0) {
                        ++state.dataAirflowNetworkBalanceManager->ErrCountLowPre;
                        ShowWarningError(state,
                                         "The calculated pressure with minimum relief air rate is lower than the pressure setpoint. The pressure "
                                         "control is unable to perform.");
                        ShowContinueErrorTimeStamp(state,
                                                   format("Calculated pressure = {:.2R}[Pa], Pressure setpoint ={:.2R}", ZonePressure1, PressureSet));
                    } else {
                        ++state.dataAirflowNetworkBalanceManager->ErrCountLowPre;
                        ShowRecurringWarningErrorAtEnd(
                            state,
                            state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->PressureControllerData(1).AFNNodeNum).Name +
                                ": The AFN model continues not to perform pressure control due to lower zone pressure...",
                            state.dataAirflowNetworkBalanceManager->ErrIndexLowPre,
                            ZonePressure1,
                            ZonePressure1);
                    }
                }
            } else {
                state.dataAirflowNetwork->ReliefMassFlowRate = MaxReliefMassFlowrate;
                solver.initialize(state);
                solver.airmov(state);
                ZonePressure2 = state.dataAirflowNetwork->AirflowNetworkNodeSimu(state.dataAirflowNetwork->PressureControllerData(1).AFNNodeNum).PZ;
                if (ZonePressure2 >= PressureSet) {
                    // The lowest pressure due to maximum flow rate is still higher than Pressure set, bypass pressure set calculation
                    if (!state.dataGlobal->WarmupFlag) {
                        if (state.dataAirflowNetworkBalanceManager->ErrCountHighPre == 0) {
                            ++state.dataAirflowNetworkBalanceManager->ErrCountHighPre;
                            ShowWarningError(state,
                                             "The calculated pressure with maximum relief air rate is higher than the pressure setpoint. The "
                                             "pressure control is unable to perform.");
                            ShowContinueErrorTimeStamp(
                                state, format("Calculated pressure = {:.2R}[Pa], Pressure setpoint = {:.2R}", ZonePressure2, PressureSet));
                        } else {
                            ++state.dataAirflowNetworkBalanceManager->ErrCountHighPre;
                            ShowRecurringWarningErrorAtEnd(
                                state,
                                state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->PressureControllerData(1).AFNNodeNum)
                                        .Name +
                                    ": The AFN model continues not to perform pressure control due to higher zone pressure...",
                                state.dataAirflowNetworkBalanceManager->ErrIndexHighPre,
                                ZonePressure2,
                                ZonePressure2);
                        }
                    }
                } else {
                    //    if ( ZonePressure1 > PressureSet && ZonePressure2 < PressureSet ) {
                    Par(1) = PressureSet;
                    General::SolveRoot(state,
                                       ErrorToler,
                                       MaxIte,
                                       SolFla,
                                       state.dataAirflowNetwork->ReliefMassFlowRate,
                                       AFNPressureResidual,
                                       MinReliefMassFlowrate,
                                       MaxReliefMassFlowrate,
                                       Par);
                    if (SolFla == -1) {
                        if (!state.dataGlobal->WarmupFlag) {
                            if (state.dataAirflowNetworkBalanceManager->ErrCountVar == 0) {
                                ++state.dataAirflowNetworkBalanceManager->ErrCountVar;
                                ShowWarningError(state, "Iteration limit exceeded pressure setpoint using relief air. Simulation continues.");
                                ShowContinueErrorTimeStamp(state,
                                                           format("Relief air flow rate = {:.4R}", state.dataAirflowNetwork->ReliefMassFlowRate));
                            } else {
                                ++state.dataAirflowNetworkBalanceManager->ErrCountVar;
                                ShowRecurringWarningErrorAtEnd(state,
                                                               state.dataAirflowNetwork->PressureControllerData(1).Name +
                                                                   "\": Iteration limit warning exceeding pressure setpoint continues...",
                                                               state.dataAirflowNetworkBalanceManager->ErrIndexVar,
                                                               state.dataAirflowNetwork->ReliefMassFlowRate,
                                                               state.dataAirflowNetwork->ReliefMassFlowRate);
                            }
                        }
                    } else if (SolFla == -2) {
                        ShowFatalError(state,
                                       "Zone pressure control failed using relief air: no solution is reached, for " +
                                           state.dataAirflowNetwork->PressureControllerData(1).Name);
                    }
                }
            }
        }
    }

    Real64 AFNPressureResidual(EnergyPlusData &state,
                               Real64 const ControllerMassFlowRate, // Pressure setpoint
                               Array1D<Real64> const &Par           // par(1) = PressureSet
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   April 2016
        //       MODIFIED       NA
        //       RE-ENGINEERED  NA

        // PURPOSE OF THIS FUNCTION:
        //  Calculates residual function ((ZonePressure - PressureSet)/PressureSet)

        // METHODOLOGY EMPLOYED:
        //  Calls AIRMOV to get the pressure in the controlled zone and calculates the residual as defined above

        // Return value
        Real64 AFNPressureResidual;

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        Real64 PressureSet;
        Real64 ZonePressure;

        PressureSet = Par(1);

        if (state.dataAirflowNetwork->PressureSetFlag == PressureCtrlExhaust) {
            state.dataAirflowNetwork->ExhaustFanMassFlowRate = ControllerMassFlowRate;
        }

        if (state.dataAirflowNetwork->PressureSetFlag == PressureCtrlRelief) {
            state.dataAirflowNetwork->ReliefMassFlowRate = ControllerMassFlowRate;
        }
        auto &solver = state.dataAFNSolver->solver;
        solver.initialize(state);
        solver.airmov(state);

        ZonePressure = state.dataAirflowNetwork->AirflowNetworkNodeSimu(state.dataAirflowNetwork->PressureControllerData(1).AFNNodeNum).PZ;

        if (PressureSet != 0.0) {
            AFNPressureResidual = (ZonePressure - PressureSet) / PressureSet;
        } else {
            AFNPressureResidual = (ZonePressure - PressureSet);
        }
        return AFNPressureResidual;
    }

    static int makeTable(EnergyPlusData &state, const std::string &name, const int gridIndex, const std::vector<Real64> &y)
    {
        // Add a new table and performance curve
        std::string contextString = "CalcWindPressureCoeffs: Creating table \"" + name + "\"";
        std::pair<EnergyPlusData *, std::string> callbackPair{&state, contextString};
        Btwxt::setMessageCallback(CurveManager::BtwxtMessageCallback, &callbackPair);

        int CurveNum = state.dataCurveManager->PerfCurve.size() + 1;
        state.dataCurveManager->PerfCurve.push_back(CurveManager::PerformanceCurveData());

        state.dataCurveManager->PerfCurve(CurveNum).Name = name;
        state.dataCurveManager->PerfCurve(CurveNum).ObjectType = "Table:Lookup";
        state.dataCurveManager->PerfCurve(CurveNum).NumDims = 1;

        state.dataCurveManager->PerfCurve(CurveNum).InterpolationType = CurveManager::InterpTypeEnum::BtwxtMethod;

        state.dataCurveManager->PerfCurve(CurveNum).Var1Min = 0.0;
        state.dataCurveManager->PerfCurve(CurveNum).Var1MinPresent = true;
        state.dataCurveManager->PerfCurve(CurveNum).Var1Max = 360.0;
        state.dataCurveManager->PerfCurve(CurveNum).Var1MaxPresent = true;

        state.dataCurveManager->PerfCurve(CurveNum).TableIndex = gridIndex;
        state.dataCurveManager->PerfCurve(CurveNum).GridValueIndex = state.dataCurveManager->btwxtManager.addOutputValues(gridIndex, y);

        state.dataCurveManager->NumCurves += 1;
        return CurveNum;
    }

} // namespace AirflowNetworkBalanceManager

void AirflowNetworkBalanceManagerData::calculateWindPressureCoeffs(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Winkelmann
    //       DATE WRITTEN   May 2003
    //       MODIFIED       Revised by L. Gu, Nov. 2005, to meet requirements of AirflowNetwork
    //       MODIFIED       Revised by L. Gu, Dec. 2008, to set the number of external nodes based on
    //                      the number of external surfaces
    //       MODIFIED       Revised by J. DeGraw, Feb. 2017, to use tables
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Calculates surface-average wind pressure coefficients for
    // the walls and roof of a rectangular building.

    // METHODOLOGY EMPLOYED:
    // Interpolates correlations between surface-average wind pressure coefficient and wind direction based on
    // measurements (see REFERENCES). Applicable only to rectangular buildings.

    // REFERENCES:
    // For low-rise buildings: M.V. Swami and S. Chandra, Correlations for Pressure Distribution
    // on Buildings and Calculation of Natural-Ventilation Airflow. ASHRAE Transactions 94 (1): 243-266.
    // For high-rise buildings: 2001 ASHRAE Fundamentals Handbook, p. 16.5, Fig. 7, "Surface Averaged
    // Wall Pressure Coefficients for Tall Buildings" and p.16.6, Fig. 9, "Surface Averaged Roof Pressure
    // Coefficients for Tall Buildings; from R.E. Akins, J.A. Peterka, and J.E. Cermak. 1979.
    // Averaged Pressure Coefficients for Rectangular Buildings. Wind Engineering. Proc. Fifth
    // International Conference 7:369-80, Fort Collins, CO. Pergamon Press, NY.

    // Using/Aliasing
    using namespace DataSurfaces;

    //  index 1 is wind incidence angle (0,30,60,...,300,330 deg)
    //  index 2 is side ratio (0.25,1.0,4.0),
    static Array2D<Real64> const CPHighRiseWall(
        3,
        12,
        reshape2<Real64, int>({0.60, 0.54, 0.23,  -0.25, -0.61, -0.55, -0.51, -0.55, -0.61, -0.25, 0.23,  0.54,
                               0.60, 0.48, 0.04,  -0.56, -0.56, -0.42, -0.37, -0.42, -0.56, -0.56, 0.04,  0.48,
                               0.60, 0.44, -0.26, -0.70, -0.53, -0.32, -0.22, -0.32, -0.53, -0.70, -0.26, 0.44},
                              {3, 12})); // Surface-averaged wind-pressure coefficient array for walls // Explicit reshape2 template args
                                         // are work-around for VC++2013 bug
    //  index 1 is wind incidence angle (0,30,60,...,300,330 deg)
    //  index 2 is side ratio (0.25,0.5,1.0),
    static Array2D<Real64> const CPHighRiseRoof(
        3,
        12,
        reshape2<Real64, int>({-0.28, -0.69, -0.72, -0.76, -0.72, -0.69, -0.28, -0.69, -0.72, -0.76, -0.72, -0.69,
                               -0.47, -0.52, -0.70, -0.76, -0.70, -0.52, -0.47, -0.52, -0.70, -0.76, -0.70, -0.52,
                               -0.70, -0.55, -0.55, -0.70, -0.55, -0.55, -0.70, -0.55, -0.55, -0.70, -0.55, -0.55},
                              {3, 12})); // Surface-averaged wind-pressure coefficient array for roof // Explicit reshape2 template args
                                         // are work-around for VC++2013 bug

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int FacadeNum;         // Facade number
    int ExtNum;            // External number
    int AFNZnNum;          // Zone number
    Real64 SideRatio;      // For vertical facades, width of facade / width of adjacent facade
    Real64 SR;             // SideRatio restricted to 0.25 to 4.0 range
    Real64 SideRatioFac;   // LOG(SideRatio)
    Real64 IncRad;         // IncAng in radians
    int IAng;              // Incidence angle index; used in interpolation
    Real64 DelAng;         // Incidence angle difference; used in interpolation
    Real64 WtAng;          // Incidence angle weighting factor; used in interpolation
    int ISR;               // Side ratio index, for interpolation
    Real64 WtSR;           // Side ratio weighting factor; used in interpolation
    int SurfNum;           // Surface number
    int SurfDatNum;        // Surface data number
    Real64 SurfAng;        // Azimuth angle of surface normal (degrees clockwise from North)
    int FacadeNumThisSurf; // Facade number for a particular surface
    Real64 AngDiff;        // Angle difference between wind and surface direction (deg)
    Real64 AngDiffMin;     // Minimum angle difference between wind and surface direction (deg)
    std::string Name;      // External node name
    std::vector<int> curveIndex = {0, 0, 0, 0, 0};

    // Facade azimuth angle
    for (FacadeNum = 1; FacadeNum <= 4; ++FacadeNum) {
        FacadeAng(FacadeNum) = state.dataAirflowNetwork->AirflowNetworkSimu.Azimuth + (FacadeNum - 1) * 90.0;
        if (FacadeAng(FacadeNum) >= 360.0) {
            FacadeAng(FacadeNum) -= 360.0;
        }
    }

    FacadeAng(5) = state.dataAirflowNetwork->AirflowNetworkSimu.Azimuth + 90.0;

    // Create AirflowNetwork external node objects -- one for each of the external surfaces

    state.dataAirflowNetwork->MultizoneExternalNodeData.allocate(AirflowNetworkNumOfExtSurfaces);
    AirflowNetworkNumOfExtNode = AirflowNetworkNumOfExtSurfaces;
    NumOfExtNodes = AirflowNetworkNumOfExtSurfaces;
    for (ExtNum = 1; ExtNum <= NumOfExtNodes; ++ExtNum) {
        state.dataAirflowNetwork->MultizoneExternalNodeData(ExtNum).ExtNum = state.dataAirflowNetwork->AirflowNetworkNumOfZones + ExtNum;
        state.dataAirflowNetwork->MultizoneExternalNodeData(ExtNum).Name = format("ExtNode{:4}", ExtNum);
    }

    // Associate each external node with SurfaceData

    ExtNum = 0;
    for (SurfDatNum = 1; SurfDatNum <= state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces; ++SurfDatNum) {
        if (SurfDatNum > state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces - state.dataAirflowNetwork->NumOfLinksIntraZone) {
            continue;
        }
        SurfNum = state.dataAirflowNetwork->MultizoneSurfaceData(SurfDatNum).SurfNum;
        if (SurfNum == 0) {
            continue; // Error caught earlier
        }
        if (state.dataSurface->Surface(SurfNum).ExtBoundCond == ExternalEnvironment ||
            (state.dataSurface->Surface(SurfNum).ExtBoundCond == OtherSideCoefNoCalcExt && state.dataSurface->Surface(SurfNum).ExtWind)) {
            ++ExtNum;
            if (state.dataSurface->Surface(SurfNum).Tilt >= 45.0) { // "Vertical" surface
                SurfAng = state.dataSurface->Surface(SurfNum).Azimuth;
                FacadeNumThisSurf = 1;
                AngDiffMin = std::abs(SurfAng - FacadeAng(1));
                if (AngDiffMin > 359.0) {
                    AngDiffMin = std::abs(AngDiffMin - 360.0);
                }
                for (FacadeNum = 2; FacadeNum <= 4; ++FacadeNum) {
                    AngDiff = std::abs(SurfAng - FacadeAng(FacadeNum));
                    if (AngDiff > 359.0) {
                        AngDiff = std::abs(AngDiff - 360.0);
                    }
                    if (AngDiff < AngDiffMin) {
                        AngDiffMin = AngDiff;
                        FacadeNumThisSurf = FacadeNum;
                    }
                }
                state.dataAirflowNetwork->MultizoneExternalNodeData(ExtNum).facadeNum = FacadeNumThisSurf;
            } else { // "Roof" surface
                state.dataAirflowNetwork->MultizoneExternalNodeData(ExtNum).facadeNum = 5;
            }
            state.dataAirflowNetwork->MultizoneSurfaceData(SurfDatNum).NodeNums[1] =
                state.dataAirflowNetwork->MultizoneExternalNodeData(ExtNum).ExtNum;
            state.dataAirflowNetwork->MultizoneSurfaceData(SurfDatNum).ExternalNodeName =
                state.dataAirflowNetwork->MultizoneExternalNodeData(ExtNum).Name;
        }
    }

    // Check if using the advanced single sided model
    for (AFNZnNum = 1; AFNZnNum <= state.dataAirflowNetwork->AirflowNetworkNumOfZones; ++AFNZnNum) {
        if (state.dataAirflowNetwork->MultizoneZoneData(AFNZnNum).SingleSidedCpType == "ADVANCED") {
            ++AirflowNetworkNumOfSingleSideZones;
        }
    }

    std::vector<Real64> dirs30 = {0, 30, 60, 90, 120, 150, 180, 210, 240, 270, 300, 330, 360};
    std::vector<Btwxt::GridAxis> dirs30Axes;
    dirs30Axes.emplace_back(dirs30, Btwxt::Method::LINEAR, Btwxt::Method::LINEAR, std::pair<double, double>{0.0, 360.0});

    auto dirs30GridIndex = state.dataCurveManager->btwxtManager.addGrid("30 Degree Increments", Btwxt::GriddedData(dirs30Axes));

    if (AirflowNetworkNumOfSingleSideZones == 0) { // do the standard surface average coefficient calculation
        // Create the array of wind directions

        // Create a curve for each facade
        for (FacadeNum = 1; FacadeNum <= 5; ++FacadeNum) {
            if (FacadeNum == 1 || FacadeNum == 3 || FacadeNum == 5) {
                SideRatio = state.dataAirflowNetwork->AirflowNetworkSimu.AspectRatio;
            } else { // FacadeNum = 2 or 4
                SideRatio = 1.0 / state.dataAirflowNetwork->AirflowNetworkSimu.AspectRatio;
            }
            if (UtilityRoutines::SameString(state.dataAirflowNetwork->AirflowNetworkSimu.BldgType, "HighRise") && FacadeNum != 5) {
                SideRatio = 1.0 / SideRatio;
            }
            SideRatioFac = std::log(SideRatio);
            std::vector<Real64> vals(13);
            for (int windDirNum = 1; windDirNum <= 12; ++windDirNum) {
                Real64 WindAng = (windDirNum - 1) * 30.0;
                IncAng = std::abs(WindAng - FacadeAng(FacadeNum));
                if (IncAng > 180.0) IncAng = 360.0 - IncAng;
                IAng = int(IncAng / 30.0) + 1;
                DelAng = mod(IncAng, 30.0);
                WtAng = 1.0 - DelAng / 30.0;

                // Wind-pressure coefficients for vertical facades, low-rise building

                if (UtilityRoutines::SameString(state.dataAirflowNetwork->AirflowNetworkSimu.BldgType, "LowRise") && FacadeNum <= 4) {
                    IncRad = IncAng * DataGlobalConstants::DegToRadians;
                    Real64 const cos_IncRad_over_2(std::cos(IncRad / 2.0));
                    vals[windDirNum - 1] = 0.6 * std::log(1.248 - 0.703 * std::sin(IncRad / 2.0) - 1.175 * pow_2(std::sin(IncRad)) +
                                                          0.131 * pow_3(std::sin(2.0 * IncRad * SideRatioFac)) + 0.769 * cos_IncRad_over_2 +
                                                          0.07 * pow_2(SideRatioFac * std::sin(IncRad / 2.0)) + 0.717 * pow_2(cos_IncRad_over_2));
                }

                // Wind-pressure coefficients for vertical facades, high-rise building

                else if (UtilityRoutines::SameString(state.dataAirflowNetwork->AirflowNetworkSimu.BldgType, "HighRise") && FacadeNum <= 4) {
                    SR = min(max(SideRatio, 0.25), 4.0);
                    if (SR >= 0.25 && SR < 1.0) {
                        ISR = 1;
                        WtSR = (1.0 - SR) / 0.75;
                    } else { // 1.0 <= SR <= 4.0
                        ISR = 2;
                        WtSR = (4.0 - SR) / 3.0;
                    }
                    vals[windDirNum - 1] = WtSR * (WtAng * CPHighRiseWall(ISR, IAng) + (1.0 - WtAng) * CPHighRiseWall(ISR, IAng + 1)) +
                                           (1.0 - WtSR) * (WtAng * CPHighRiseWall(ISR + 1, IAng) + (1.0 - WtAng) * CPHighRiseWall(ISR + 1, IAng + 1));
                }

                // Wind-pressure coefficients for roof (assumed same for low-rise and high-rise buildings)

                else if ((UtilityRoutines::SameString(state.dataAirflowNetwork->AirflowNetworkSimu.BldgType, "HighRise") ||
                          UtilityRoutines::SameString(state.dataAirflowNetwork->AirflowNetworkSimu.BldgType, "LowRise")) &&
                         FacadeNum == 5) {
                    SR = min(max(SideRatio, 0.25), 1.0);
                    if (SR >= 0.25 && SR < 0.5) {
                        ISR = 1;
                        WtSR = (0.5 - SR) / 0.25;
                    } else { // 0.5 <= SR <= 1.0
                        ISR = 2;
                        WtSR = (1.0 - SR) / 0.5;
                    }
                    vals[windDirNum - 1] = WtSR * (WtAng * CPHighRiseRoof(ISR, IAng) + (1.0 - WtAng) * CPHighRiseRoof(ISR, IAng + 1)) +
                                           (1.0 - WtSR) * (WtAng * CPHighRiseRoof(ISR + 1, IAng) + (1.0 - WtAng) * CPHighRiseRoof(ISR + 1, IAng + 1));
                }

            } // End of wind direction loop
            // Add new table
            vals[12] = vals[0]; // Enforce periodicity
            curveIndex[FacadeNum - 1] = AirflowNetworkBalanceManager::makeTable(state, format("!WPCTABLE{}", FacadeNum), dirs30GridIndex, vals);
        } // End of facade number loop

    } else { //-calculate the advanced single sided wind pressure coefficients

        // Calculate the wind pressure coefficients vs. wind direction for each external node
        // The wind pressure coeffients are stored temporarily in the "valsByFacade" vector and then
        // converted into a table near the end of this else. There will be at least seven profiles
        // (four sides plus one roof plus two for each pair of windows). The name is thus a little
        // misleading, as it isn't really the values by facade once you get beyond the first five.
        std::vector<std::vector<Real64>> valsByFacade(5);
        for (FacadeNum = 0; FacadeNum < 4; ++FacadeNum) {
            valsByFacade[FacadeNum] = std::vector<Real64>(36);
        }
        FacadeNum = 4;
        valsByFacade[FacadeNum] = std::vector<Real64>(12);
        for (FacadeNum = 1; FacadeNum <= 4; ++FacadeNum) {
            if (FacadeNum == 1 || FacadeNum == 3) {
                SideRatio = state.dataAirflowNetwork->AirflowNetworkSimu.AspectRatio;
            } else { // FacadeNum = 2 or 4
                SideRatio = 1.0 / state.dataAirflowNetwork->AirflowNetworkSimu.AspectRatio;
            }
            if (UtilityRoutines::SameString(state.dataAirflowNetwork->AirflowNetworkSimu.BldgType, "HighRise") && FacadeNum != 5) {
                SideRatio = 1.0 / SideRatio;
            }
            SideRatioFac = std::log(SideRatio);
            for (int windDirNum = 1; windDirNum <= 36; ++windDirNum) {
                Real64 WindAng = (windDirNum - 1) * 10.0;
                IncAng = std::abs(WindAng - FacadeAng(FacadeNum));
                if (IncAng > 180.0) IncAng = 360.0 - IncAng;
                IAng = int(IncAng / 10.0) + 1;
                DelAng = mod(IncAng, 10.0);
                WtAng = 1.0 - DelAng / 10.0;
                // Wind-pressure coefficients for vertical facades, low-rise building
                IncRad = IncAng * DataGlobalConstants::DegToRadians;
                valsByFacade[FacadeNum - 1][windDirNum - 1] =
                    0.6 * std::log(1.248 - 0.703 * std::sin(IncRad / 2.0) - 1.175 * pow_2(std::sin(IncRad)) +
                                   0.131 * pow_3(std::sin(2.0 * IncRad * SideRatioFac)) + 0.769 * std::cos(IncRad / 2.0) +
                                   0.07 * pow_2(SideRatioFac * std::sin(IncRad / 2.0)) + 0.717 * pow_2(std::cos(IncRad / 2.0)));
            } // End of wind direction loop
        }     // End of facade number loop
        // Add a roof
        FacadeNum = 5;
        SR = min(max(SideRatio, 0.25), 1.0);
        if (SR >= 0.25 && SR < 0.5) {
            ISR = 1;
            WtSR = (0.5 - SR) / 0.25;
        } else { // 0.5 <= SR <= 1.0
            ISR = 2;
            WtSR = (1.0 - SR) / 0.5;
        }
        for (int windDirNum = 1; windDirNum <= 12; ++windDirNum) {
            Real64 WindAng = (windDirNum - 1) * 30.0;
            IncAng = std::abs(WindAng - FacadeAng(FacadeNum));
            if (IncAng > 180.0) IncAng = 360.0 - IncAng;
            IAng = int(IncAng / 30.0) + 1;
            DelAng = mod(IncAng, 30.0);
            WtAng = 1.0 - DelAng / 30.0;
            // Wind-pressure coefficients for roof (assumed same for low-rise and high-rise buildings)
            valsByFacade[FacadeNum - 1][windDirNum - 1] =
                WtSR * (WtAng * CPHighRiseRoof(ISR, IAng) + (1.0 - WtAng) * CPHighRiseRoof(ISR, IAng + 1)) +
                (1.0 - WtSR) * (WtAng * CPHighRiseRoof(ISR + 1, IAng) + (1.0 - WtAng) * CPHighRiseRoof(ISR + 1, IAng + 1));
        }
        AirflowNetworkBalanceManager::CalcSingleSidedCps(state,
                                                         valsByFacade); // run the advanced single sided subroutine if at least one zone calls for it
        // Resize the curve index array
        curveIndex.resize(valsByFacade.size());
        // Create the curves

        std::vector<Real64> dirs10 = {0,   10,  20,  30,  40,  50,  60,  70,  80,  90,  100, 110, 120, 130, 140, 150, 160, 170, 180,
                                      190, 200, 210, 220, 230, 240, 250, 260, 270, 280, 290, 300, 310, 320, 330, 340, 350, 360};

        std::vector<Btwxt::GridAxis> dirs10Axes;
        dirs10Axes.emplace_back(dirs10, Btwxt::Method::LINEAR, Btwxt::Method::LINEAR, std::pair<double, double>{0.0, 360.0});

        auto dirs10GridIndex = state.dataCurveManager->btwxtManager.addGrid("10 Degree Increments", Btwxt::GriddedData(dirs10Axes));

        for (FacadeNum = 1; FacadeNum <= 4; ++FacadeNum) {
            valsByFacade[FacadeNum - 1].push_back(valsByFacade[FacadeNum - 1][0]); // Enforce periodicity
            curveIndex[FacadeNum - 1] = AirflowNetworkBalanceManager::makeTable(
                state, format("!SSWPCTABLEFACADE{}", FacadeNum), dirs10GridIndex, valsByFacade[FacadeNum - 1]);
        }
        FacadeNum = 5;
        valsByFacade[FacadeNum - 1].push_back(valsByFacade[FacadeNum - 1][0]); // Enforce periodicity
        curveIndex[FacadeNum - 1] =
            AirflowNetworkBalanceManager::makeTable(state, format("!SSWPCTABLEFACADE{}", FacadeNum), dirs30GridIndex, valsByFacade[FacadeNum - 1]);
        for (unsigned facadeNum = 6; facadeNum <= valsByFacade.size(); ++facadeNum) {
            valsByFacade[facadeNum - 1].push_back(valsByFacade[facadeNum - 1][0]); // Enforce periodicity
            curveIndex[facadeNum - 1] =
                AirflowNetworkBalanceManager::makeTable(state, format("!SSWPCTABLE{}", facadeNum), dirs10GridIndex, valsByFacade[facadeNum - 1]);
        }
    }
    // Connect the external nodes to the new curves
    for (ExtNum = 1; ExtNum <= NumOfExtNodes; ++ExtNum) {
        state.dataAirflowNetwork->MultizoneExternalNodeData(ExtNum).curve =
            curveIndex[state.dataAirflowNetwork->MultizoneExternalNodeData(ExtNum).facadeNum - 1];
    }
}

namespace AirflowNetworkBalanceManager {

    Real64 CalcWindPressure(EnergyPlusData &state,
                            int const curve,           // Curve index, change this to pointer after curve refactor
                            bool const symmetricCurve, // True if the curve is symmetric (0 to 180)
                            bool const relativeAngle,  // True if the Cp curve angle is measured relative to the surface
                            Real64 const azimuth,      // Azimuthal angle of surface
                            Real64 const windSpeed,    // Wind velocity
                            Real64 const windDir,      // Wind direction
                            Real64 const dryBulbTemp,  // Air node dry bulb temperature
                            Real64 const humRat        // Air node humidity ratio
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   Oct. 2005
        //       MODIFIED       Jason DeGraw, Feb. 2017, modify to use curves
        //       MODIFIED       Xuan Luo, Aug. 2017, modify to use local air condition
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Calculates surface wind pressure based on given CP values

        // REFERENCES:
        // COMIS Fundamentals

        // Return value is wind pressure[Pa]

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        Real64 angle(windDir);
        Real64 rho; // Outdoor air density
        Real64 Cp;  // Cp value at given wind direction

        // Calculate outdoor density
        rho = PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, dryBulbTemp, humRat);

        // Calculate pressure coefficient
        if (relativeAngle) {
            angle = angle - azimuth;
            if (angle < 0.0) {
                angle += 360.0;
            }
        }
        if (symmetricCurve) {
            if (angle > 180.0) {
                angle = 360.0 - angle;
            }
        }
        Cp = CurveManager::CurveValue(state, curve, angle);

        return Cp * 0.5 * rho * windSpeed * windSpeed;
    }

    Real64 CalcDuctInsideConvResist(Real64 const Tair, // Average air temperature
                                    Real64 const mdot, // Mass flow rate
                                    Real64 const Dh,   // Hydraulic diameter
                                    Real64 const hIn   // User defined convection coefficient
    )
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Matt Mitchell, Tony Fontanini
        //       DATE WRITTEN   Feb. 2017
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Calculates duct inside convection coefficients

        // REFERENCES:
        // ASTM C1340
        // Jakob, F.E.,  Fischer, R.D., Flanigan, L.J. 1987. "Experimental Validation of the Duct Submodel for the SP43 Simulation Model."
        // ASHRAE Trans. pp 1499-1514.

        Real64 hIn_final = 0;

        if (hIn == 0) {

            Real64 Tair_IP = Tair * 1.8 + 32.0;     // Convert C to F
            Real64 mdot_IP = mdot * 2.20462 * 3600; // Convert kg/s to lb/hr
            Real64 Dh_IP = Dh * 3.28084;            // Convert m to ft
            Real64 Ai_IP = pow_2(Dh_IP) * DataGlobalConstants::Pi / 4;

            Real64 CorrelationCoeff = 0.00368 + 1.5e-6 * (Tair_IP - 80);
            Real64 MassFlux = mdot_IP / Ai_IP; // lb/hr-ft2

            Real64 DuctInsideConvCoeff_IP = CorrelationCoeff * pow(MassFlux, 0.8) / pow(Dh_IP, 0.2); // BTU/hr-ft2-F

            hIn_final = DuctInsideConvCoeff_IP * pow_2(3.28084) * 1.8 * 1055.06 / 3600; // Convert BTU/hr-ft2-F to W/m2-K

        } else {
            hIn_final = hIn;
        }

        if (hIn_final == 0) {
            return 0;
        } else {
            return 1 / hIn_final;
        }
    }

    Real64 CalcDuctOutsideConvResist(EnergyPlusData &state,
                                     Real64 const Ts,      // Surface temperature
                                     Real64 const Tamb,    // Free air temperature
                                     Real64 const Wamb,    // Free air humidity ratio
                                     Real64 const Pamb,    // Free air barometric pressure
                                     Real64 const Dh,      // Hydraulic diameter
                                     Real64 const ZoneNum, // Zone number
                                     Real64 const hOut     // User defined convection coefficient
    )
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Matt Mitchell, Tony Fontanini
        //       DATE WRITTEN   Feb. 2017
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Calculates duct outside convection coefficients

        // REFERENCES:
        // ASTM C1340

        Real64 k = airThermConductivity(state, Ts);
        auto &Zone(state.dataHeatBal->Zone);

        Real64 hOut_final = 0;

        if (hOut == 0) {

            // Free convection
            Real64 Pr = airPrandtl(state, (Ts + Tamb) / 2, Wamb, Pamb);
            Real64 KinVisc = airKinematicVisc(state, (Ts + Tamb) / 2, Wamb, Pamb);
            Real64 Beta = 2.0 / ((Tamb + DataGlobalConstants::KelvinConv) + (Ts + DataGlobalConstants::KelvinConv));
            Real64 Gr = DataGlobalConstants::GravityConstant * Beta * std::abs(Ts - Tamb) * pow_3(Dh) / pow_2(KinVisc);
            Real64 Ra = Gr * Pr;
            Real64 Nu_free(0);

            if (Ra < 10e9) {
                Nu_free = 0.53 * pow(Ra, 0.25);
            } else {
                Nu_free = 0.13 * pow(Ra, 0.333);
            }

            Real64 V = 0;
            // Forced convection
            if (ZoneNum > 0) {
                Real64 ACH = GetZoneOutdoorAirChangeRate(state, ZoneNum); // Zone air change rate [1/hr]
                Real64 Vol = Zone(ZoneNum).Volume;                        // Zone volume [m3]
                V = pow(Vol, 0.333) * ACH / 3600;                         // Average air speed in zone [m/s]
            } else {
                V = state.dataEnvrn->WindSpeed;
            }

            Real64 Re = V * Dh / KinVisc; // Reynolds number
            Real64 c = 0;
            Real64 n = 0;

            if (Re <= 4) {
                c = 0.989;
                n = 0.33;
            } else if (4 < Re && Re <= 40) {
                c = 0.911;
                n = 0.385;
            } else if (40 < Re && Re <= 4000) {
                c = 0.683;
                n = 0.466;
            } else if (4000 < Re && Re <= 40000) {
                c = 0.193;
                n = 0.618;
            } else if (40000 < Re) {
                c = 0.0266;
                n = 0.805;
            }

            Real64 Nu_forced = c * pow(Re, n) * pow(Pr, 0.333);

            Real64 Nu_combined = pow(pow_3(Nu_free) + pow_3(Nu_forced), 0.333);
            hOut_final = Nu_combined * k / Dh;

        } else {
            hOut_final = hOut;
        }

        if (hOut_final == 0) {
            return 0;
        } else {
            return 1 / hOut_final;
        }
    }

    void CalcAirflowNetworkHeatBalance(EnergyPlusData &state)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   Oct. 2005
        //       MODIFIED       na
        //       RE-ENGINEERED  Revised based on Subroutine CalcADSHeatBalance

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine performs AirflowNetwork thermal simulations.

        // USE STATEMENTS:
        auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int i;
        int j;
        int LF;
        int LT;
        int CompNum;
        int NF;
        int NT;
        iComponentTypeNum CompTypeNum;
        int TypeNum;
        int ExtNodeNum;
        std::string CompName;
        Real64 Ei;
        Real64 DirSign;
        Real64 Tamb;
        Real64 Wamb;
        Real64 Pamb;
        Real64 CpAir;
        Real64 TZON;
        Real64 load;
        int ZoneNum;
        bool found;
        bool OANode;

        auto &Node(state.dataLoopNodes->Node);

        state.dataAirflowNetworkBalanceManager->MA = 0.0;
        state.dataAirflowNetworkBalanceManager->MV = 0.0;

        for (i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfLinks; ++i) {
            CompNum = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).CompNum;
            CompTypeNum = state.dataAirflowNetwork->AirflowNetworkCompData(CompNum).CompTypeNum;
            CompName = state.dataAirflowNetwork->AirflowNetworkCompData(CompNum).EPlusName;
            CpAir = PsyCpAirFnW(
                (state.dataAirflowNetwork->AirflowNetworkNodeSimu(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0]).WZ +
                 state.dataAirflowNetwork->AirflowNetworkNodeSimu(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1]).WZ) /
                2.0);
            // Calculate duct conduction loss
            if (CompTypeNum == iComponentTypeNum::DWC && CompName == std::string()) { // Duct element only
                TypeNum = state.dataAirflowNetwork->AirflowNetworkCompData(CompNum).TypeNum;
                if (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW > 0.0) { // flow direction is the same as input from node 1 to node 2
                    LF = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
                    LT = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
                    DirSign = 1.0;
                } else { // flow direction is the opposite as input from node 2 to node 1
                    LF = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
                    LT = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
                    DirSign = -1.0;
                }
                // Fatal error when return flow is opposite to the desired direction
                if (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW == 0.0 &&
                    state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2 > 0.0) {
                    if (state.dataAirflowNetworkBalanceManager->LoopOnOffFlag(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).AirLoopNum)) {
                        ShowSevereError(state,
                                        "AirflowNetwork: The airflow direction is opposite to the intended direction (from node 1 to node 2) in "
                                        "AirflowNetwork:Distribution:Linkage = " +
                                            state.dataAirflowNetwork->AirflowNetworkLinkageData(i).Name);
                        ShowContinueErrorTimeStamp(state, "");
                        ShowContinueError(state,
                                          "The sum of the airflows entering the zone is greater than the airflows leaving the zone (e.g., wind "
                                          "and stack effect).");
                        ShowContinueError(state,
                                          "Please check wind speed or reduce values of \"Window/Door Opening Factor, or Crack Factor\" defined in "
                                          "AirflowNetwork:MultiZone:Surface objects.");
                        //                    ShowFatalError(state,  "AirflowNetwork: The previous error causes termination." );
                    }
                }

                if (state.dataAirflowNetwork->AirflowNetworkLinkageData(i).ZoneNum < 0) {
                    ExtNodeNum = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
                    if (state.dataAirflowNetwork->AirflowNetworkNodeData(ExtNodeNum).OutAirNodeNum > 0 &&
                        Node(state.dataAirflowNetwork->AirflowNetworkNodeData(ExtNodeNum).OutAirNodeNum).IsLocalNode) {
                        Tamb = Node(state.dataAirflowNetwork->AirflowNetworkNodeData(ExtNodeNum).OutAirNodeNum).OutAirDryBulb;
                        Wamb = Node(state.dataAirflowNetwork->AirflowNetworkNodeData(ExtNodeNum).OutAirNodeNum).HumRat;
                    } else {
                        Tamb = OutDryBulbTempAt(state, state.dataAirflowNetwork->AirflowNetworkNodeData(ExtNodeNum).NodeHeight);
                        Wamb = state.dataEnvrn->OutHumRat;
                    }
                } else if (state.dataAirflowNetwork->AirflowNetworkLinkageData(i).ZoneNum == 0) {
                    Tamb = state.dataAirflowNetwork->AirflowNetworkNodeSimu(LT).TZ;
                    Wamb = state.dataAirflowNetwork->AirflowNetworkNodeSimu(LT).WZ;
                } else {
                    Tamb = state.dataAirflowNetwork->ANZT(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).ZoneNum);
                    Wamb = state.dataAirflowNetwork->ANZW(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).ZoneNum);
                }

                Pamb = state.dataEnvrn->OutBaroPress;

                Real64 const tolerance = 0.001;
                Real64 UThermal(10); // Initialize. This will get updated.
                Real64 UThermal_iter = 0;
                Real64 Tsurr = Tamb;
                Real64 Tsurr_K = Tsurr + DataGlobalConstants::KelvinConv;
                Real64 Tin = state.dataAirflowNetwork->AirflowNetworkNodeSimu(LF).TZ;
                Real64 TDuctSurf = (Tamb + Tin) / 2.0;
                Real64 TDuctSurf_K = TDuctSurf + DataGlobalConstants::KelvinConv;
                Real64 DuctSurfArea = state.dataAirflowNetwork->DisSysCompDuctData(TypeNum).L *
                                      state.dataAirflowNetwork->DisSysCompDuctData(TypeNum).hydraulicDiameter * DataGlobalConstants::Pi;

                // If user defined view factors not present, calculate air-to-air heat transfer
                if (state.dataAirflowNetwork->AirflowNetworkLinkageData(i).LinkageViewFactorObjectNum == 0) {

                    // Calculate convection coefficient if one or both not present
                    if (state.dataAirflowNetwork->DisSysCompDuctData(TypeNum).InsideConvCoeff == 0 &&
                        state.dataAirflowNetwork->DisSysCompDuctData(TypeNum).OutsideConvCoeff == 0) {
                        while (std::abs(UThermal - UThermal_iter) > tolerance) {
                            UThermal_iter = UThermal;

                            Real64 RThermConvIn = CalcDuctInsideConvResist(Tin,
                                                                           state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW,
                                                                           state.dataAirflowNetwork->DisSysCompDuctData(TypeNum).hydraulicDiameter,
                                                                           state.dataAirflowNetwork->DisSysCompDuctData(TypeNum).InsideConvCoeff);
                            Real64 RThermConvOut = CalcDuctOutsideConvResist(state,
                                                                             TDuctSurf,
                                                                             Tamb,
                                                                             Wamb,
                                                                             Pamb,
                                                                             state.dataAirflowNetwork->DisSysCompDuctData(TypeNum).hydraulicDiameter,
                                                                             state.dataAirflowNetwork->AirflowNetworkLinkageData(i).ZoneNum,
                                                                             state.dataAirflowNetwork->DisSysCompDuctData(TypeNum).OutsideConvCoeff);
                            Real64 RThermConduct = 1.0 / state.dataAirflowNetwork->DisSysCompDuctData(TypeNum).UThermConduct;
                            Real64 RThermTotal = RThermConvIn + RThermConvOut + RThermConduct;
                            UThermal = pow(RThermTotal, -1);

                            // Duct conduction, assuming effectiveness = 1 - exp(-NTU)
                            Ei = General::epexp(-UThermal * DuctSurfArea,
                                                (DirSign * state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW * CpAir));
                            Real64 QCondDuct = std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW) * CpAir * (Tamb - Tin) * (1 - Ei);

                            TDuctSurf = Tamb - QCondDuct * RThermConvOut / DuctSurfArea;
                        }
                    } else { // Air-to-air only. U and h values are all known
                        Real64 RThermConvIn = CalcDuctInsideConvResist(Tin,
                                                                       state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW,
                                                                       state.dataAirflowNetwork->DisSysCompDuctData(TypeNum).hydraulicDiameter,
                                                                       state.dataAirflowNetwork->DisSysCompDuctData(TypeNum).InsideConvCoeff);
                        Real64 RThermConvOut = CalcDuctOutsideConvResist(state,
                                                                         TDuctSurf,
                                                                         Tamb,
                                                                         Wamb,
                                                                         Pamb,
                                                                         state.dataAirflowNetwork->DisSysCompDuctData(TypeNum).hydraulicDiameter,
                                                                         state.dataAirflowNetwork->AirflowNetworkLinkageData(i).ZoneNum,
                                                                         state.dataAirflowNetwork->DisSysCompDuctData(TypeNum).OutsideConvCoeff);
                        Real64 RThermConduct = 1.0 / state.dataAirflowNetwork->DisSysCompDuctData(TypeNum).UThermConduct;
                        Real64 RThermTotal = RThermConvIn + RThermConvOut + RThermConduct;
                        UThermal = pow(RThermTotal, -1);
                    }

                    Tsurr = Tamb;

                } else { // Air-to-air + radiation heat transfer

                    auto &VFObj(state.dataAirflowNetwork->AirflowNetworkLinkageViewFactorData(
                        state.dataAirflowNetwork->AirflowNetworkLinkageData(i).LinkageViewFactorObjectNum));
                    VFObj.QRad = 0;
                    VFObj.QConv = 0;

                    Real64 Tin_ave = Tin;
                    Real64 hOut = 0;

                    while (std::abs(UThermal - UThermal_iter) > tolerance) {
                        UThermal_iter = UThermal;

                        Real64 RThermConvIn = CalcDuctInsideConvResist(Tin_ave,
                                                                       state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW,
                                                                       state.dataAirflowNetwork->DisSysCompDuctData(TypeNum).hydraulicDiameter,
                                                                       state.dataAirflowNetwork->DisSysCompDuctData(TypeNum).InsideConvCoeff);
                        Real64 RThermConvOut = CalcDuctOutsideConvResist(state,
                                                                         TDuctSurf,
                                                                         Tamb,
                                                                         Wamb,
                                                                         Pamb,
                                                                         state.dataAirflowNetwork->DisSysCompDuctData(TypeNum).hydraulicDiameter,
                                                                         state.dataAirflowNetwork->AirflowNetworkLinkageData(i).ZoneNum,
                                                                         state.dataAirflowNetwork->DisSysCompDuctData(TypeNum).OutsideConvCoeff);

                        if (RThermConvOut > 0.0) {
                            hOut = 1 / RThermConvOut;
                        }

                        Real64 RThermConduct = 1.0 / state.dataAirflowNetwork->DisSysCompDuctData(TypeNum).UThermConduct;

                        Real64 hrjTj_sum = 0;
                        Real64 hrj_sum = 0;

                        for (int j = 1; j <= VFObj.LinkageSurfaceData.u(); ++j) {

                            int ZoneSurfNum = VFObj.LinkageSurfaceData(j).SurfaceNum;

                            Real64 TSurfj = state.dataHeatBalSurf->TH(1, 1, ZoneSurfNum);
                            Real64 TSurfj_K = TSurfj + DataGlobalConstants::KelvinConv;

                            Real64 ZoneSurfEmissivity =
                                state.dataConstruction->Construct(state.dataSurface->Surface(ZoneSurfNum).Construction).InsideAbsorpThermal;
                            Real64 ZoneSurfArea = state.dataSurface->Surface(ZoneSurfNum).Area;

                            Real64 DuctEmissivity = VFObj.DuctEmittance;
                            Real64 DuctExposureFrac = VFObj.DuctExposureFraction;
                            Real64 DuctToZoneSurfViewFactor = VFObj.LinkageSurfaceData(j).ViewFactor;

                            Real64 DuctSurfResistance = (1 - DuctEmissivity) / (DuctExposureFrac * DuctSurfArea * DuctEmissivity);
                            Real64 SpaceResistance = 1 / (DuctExposureFrac * DuctSurfArea * DuctToZoneSurfViewFactor);
                            Real64 ZoneSurfResistance = (1 - ZoneSurfEmissivity) / (ZoneSurfArea * ZoneSurfEmissivity);

                            VFObj.LinkageSurfaceData(j).SurfaceResistanceFactor =
                                DataGlobalConstants::StefanBoltzmann / (DuctSurfResistance + SpaceResistance + ZoneSurfResistance);

                            Real64 hrj = VFObj.LinkageSurfaceData(j).SurfaceResistanceFactor * (TDuctSurf_K + TSurfj_K) *
                                         (pow_2(TDuctSurf_K) + pow_2(TSurfj_K)) / DuctSurfArea;

                            hrjTj_sum += hrj * TSurfj;
                            hrj_sum += hrj;
                        }

                        Tsurr = (hOut * Tamb + hrjTj_sum) / (hOut + hrj_sum); // Surroundings temperature [C]
                        Tsurr_K = Tsurr + DataGlobalConstants::KelvinConv;

                        Real64 RThermTotal = RThermConvIn + RThermConduct + 1 / (hOut + hrj_sum);
                        UThermal = pow(RThermTotal, -1);

                        Real64 NTU = UThermal * DuctSurfArea / (DirSign * state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW * CpAir);
                        Tin_ave = Tsurr + (Tin - Tsurr) * (1 / NTU) * (1 - exp(-NTU));

                        TDuctSurf = Tin_ave - UThermal * (RThermConvIn + RThermConduct) * (Tin_ave - Tsurr);
                        TDuctSurf_K = TDuctSurf + DataGlobalConstants::KelvinConv;
                    }

                    for (int j = 1; j <= VFObj.LinkageSurfaceData.u(); ++j) {
                        int ZoneSurfNum = VFObj.LinkageSurfaceData(j).SurfaceNum;
                        Real64 TSurfj = state.dataHeatBalSurf->TH(1, 1, ZoneSurfNum);
                        Real64 TSurfj_K = TSurfj + DataGlobalConstants::KelvinConv;
                        VFObj.LinkageSurfaceData(j).SurfaceRadLoad = VFObj.LinkageSurfaceData(j).SurfaceResistanceFactor *
                                                                     (pow_4(TDuctSurf_K) - pow_4(TSurfj_K)); // Radiant load for this surface [W]
                        int SurfNum = VFObj.LinkageSurfaceData(j).SurfaceNum;
                        Real64 ZoneSurfaceArea = state.dataSurface->Surface(SurfNum).Area;
                        state.dataHeatBalFanSys->QRadSurfAFNDuct(SurfNum) += VFObj.LinkageSurfaceData(j).SurfaceRadLoad * TimeStepSys *
                                                                             DataGlobalConstants::SecInHour /
                                                                             ZoneSurfaceArea; // Energy to each surface per unit area [J/m2]
                        VFObj.QRad += VFObj.LinkageSurfaceData(j).SurfaceRadLoad; // Total radiant load from all surfaces for this system timestep [W]
                    }

                    VFObj.QConv = hOut * DuctSurfArea * (TDuctSurf - Tamb);
                    UThermal = (VFObj.QRad + VFObj.QConv) / (DuctSurfArea * std::abs(Tsurr - Tin));
                }

                if (!state.dataAirflowNetworkBalanceManager->LoopOnOffFlag(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).AirLoopNum) &&
                    state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW <= 0.0) {
                    Ei = General::epexp(-UThermal * DuctSurfArea, (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2 * CpAir));
                    state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LT) +=
                        std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2) * CpAir;
                    state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LF) =
                        -std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2) * CpAir * Ei;
                    state.dataAirflowNetworkBalanceManager->MV(LT) +=
                        std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2) * Tsurr * (1.0 - Ei) * CpAir;
                } else {
                    Ei = General::epexp(-UThermal * DuctSurfArea, (DirSign * state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW * CpAir));
                    state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LT) +=
                        std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW) * CpAir;
                    state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LF) =
                        -std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW) * CpAir * Ei;
                    state.dataAirflowNetworkBalanceManager->MV(LT) +=
                        std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW) * Tsurr * (1.0 - Ei) * CpAir;
                }
            }
            if (CompTypeNum == iComponentTypeNum::TMU) { // Reheat unit: SINGLE DUCT:CONST VOLUME:REHEAT
                TypeNum = state.dataAirflowNetwork->AirflowNetworkCompData(CompNum).TypeNum;
                if (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW > 0.0) { // flow direction is the same as input from node 1 to node 2
                    LF = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
                    LT = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
                    DirSign = 1.0;
                } else { // flow direction is the opposite as input from node 2 to node 1
                    LF = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
                    LT = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
                    DirSign = -1.0;
                }
                Ei = General::epexp(-0.001 * state.dataAirflowNetwork->DisSysCompTermUnitData(TypeNum).L *
                                        state.dataAirflowNetwork->DisSysCompTermUnitData(TypeNum).hydraulicDiameter * DataGlobalConstants::Pi,
                                    (DirSign * state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW * CpAir));
                Tamb = state.dataAirflowNetwork->AirflowNetworkNodeSimu(LT).TZ;
                if (!state.dataAirflowNetworkBalanceManager->LoopOnOffFlag(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).AirLoopNum) &&
                    state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW <= 0.0) {
                    Ei = General::epexp(-0.001 * state.dataAirflowNetwork->DisSysCompTermUnitData(TypeNum).L *
                                            state.dataAirflowNetwork->DisSysCompTermUnitData(TypeNum).hydraulicDiameter * DataGlobalConstants::Pi,
                                        (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2 * CpAir));
                    state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LT) +=
                        std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2) * CpAir;
                    state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LF) =
                        -std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2) * CpAir * Ei;
                    state.dataAirflowNetworkBalanceManager->MV(LT) +=
                        std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2) * Tamb * (1.0 - Ei) * CpAir;
                } else {
                    state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LT) +=
                        std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW) * CpAir;
                    state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LF) =
                        -std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW) * CpAir * Ei;
                    state.dataAirflowNetworkBalanceManager->MV(LT) +=
                        std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW) * Tamb * (1.0 - Ei) * CpAir;
                }
            }
            if (CompTypeNum == iComponentTypeNum::COI) { // heating or cooling coil
                TypeNum = state.dataAirflowNetwork->AirflowNetworkCompData(CompNum).TypeNum;
                if (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW > 0.0) { // flow direction is the same as input from node 1 to node 2
                    LF = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
                    LT = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
                    DirSign = 1.0;
                } else { // flow direction is the opposite as input from node 2 to node 1
                    LF = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
                    LT = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
                    DirSign = -1.0;
                }
            }
            // Calculate temp in a constant pressure drop element
            if (CompTypeNum == iComponentTypeNum::CPD && CompName == std::string()) { // constant pressure element only
                if (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW > 0.0) { // flow direction is the same as input from node 1 to node 2
                    LF = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
                    LT = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
                } else { // flow direction is the opposite as input from node 2 to node 1
                    LF = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
                    LT = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
                }
                if (!state.dataAirflowNetworkBalanceManager->LoopOnOffFlag(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).AirLoopNum) &&
                    state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW <= 0.0) {
                    state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LT) +=
                        std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2) * CpAir;
                    state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LF) =
                        -std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2) * CpAir;
                } else {
                    state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LT) +=
                        std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW) * CpAir;
                    state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LF) =
                        -std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW) * CpAir;
                }
                state.dataAirflowNetworkBalanceManager->MV(LT) = 0.0;
            }
            // Calculate return leak
            if ((CompTypeNum == iComponentTypeNum::PLR || CompTypeNum == iComponentTypeNum::ELR) && CompName == std::string()) {
                // Return leak element only
                if ((state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0])
                         .EPlusZoneNum > 0) &&
                    (state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1])
                         .EPlusZoneNum == 0) &&
                    (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW > 0.0)) {
                    LF = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
                    LT = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
                    state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LT) +=
                        std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW) * CpAir;
                    state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LF) =
                        -std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW) * CpAir;
                }
                if ((state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0]).ExtNodeNum >
                     0) &&
                    (state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1])
                         .EPlusZoneNum == 0) &&
                    (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW > 0.0)) {
                    LF = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
                    LT = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
                    state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LT) +=
                        std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW) * CpAir;
                    state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LF) =
                        -std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW) * CpAir;
                }
                if ((state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1])
                         .EPlusZoneNum > 0) &&
                    (state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0])
                         .EPlusZoneNum == 0) &&
                    (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2 > 0.0)) {
                    LF = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
                    LT = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
                    state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LT) +=
                        std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2) * CpAir;
                    state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LF) =
                        -std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2) * CpAir;
                }
                if ((state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1]).ExtNodeNum >
                     0) &&
                    (state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0])
                         .EPlusZoneNum == 0) &&
                    (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2 > 0.0)) {
                    LF = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
                    LT = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
                    state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LT) +=
                        std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2) * CpAir;
                    state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LF) =
                        -std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2) * CpAir;
                }
            }
            // Check reheat unit or coil
            if (state.dataAirflowNetwork->AirflowNetworkCompData(CompNum).EPlusTypeNum == iEPlusComponentType::RHT &&
                (!state.dataAirflowNetwork->AirflowNetworkLinkageData(i).VAVTermDamper)) {
                NF = 0;
                NT = 0;
                if (state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0])
                        .EPlusNodeNum > 0) {
                    NF = state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0])
                             .EPlusNodeNum;
                }
                if (state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1])
                        .EPlusNodeNum > 0) {
                    NT = state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1])
                             .EPlusNodeNum;
                }
                if ((NF == 0) || (NT == 0)) {
                    ShowFatalError(state,
                                   "Node number in the primary air loop is not found in AIRFLOWNETWORK:DISTRIBUTION:NODE = " +
                                       state.dataAirflowNetwork->AirflowNetworkLinkageData(i).Name);
                }
                if (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW > 0.0) {
                    LF = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
                    LT = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
                    load = Node(NT).Temp - Node(NF).Temp;
                } else {
                    LF = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
                    LT = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
                    load = Node(NF).Temp - Node(NT).Temp;
                }
                CpAir = PsyCpAirFnW(Node(NT).HumRat);
                state.dataAirflowNetworkBalanceManager->MV(LT) += state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW * CpAir * load;
            }
        }

        // Prescribe temperature for EPlus nodes
        for (i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfNodes; ++i) {
            found = false;
            OANode = false;
            for (j = 1; j <= state.dataAirflowNetwork->AirflowNetworkNumOfLinks; ++j) {
                if (state.dataAirflowNetwork->AirflowNetworkLinkageData(j).NodeNums[0] == i ||
                    state.dataAirflowNetwork->AirflowNetworkLinkageData(j).NodeNums[1] == i) {
                    CompNum = state.dataAirflowNetwork->AirflowNetworkLinkageData(j).CompNum;
                    if (state.dataAirflowNetwork->AirflowNetworkCompData(CompNum).EPlusTypeNum == iEPlusComponentType::RHT &&
                        (!state.dataAirflowNetwork->AirflowNetworkLinkageData(j).VAVTermDamper)) {
                        found = true;
                        break;
                    }
                    // Overwrite fan outlet node
                    if (state.dataAirflowNetwork->AirflowNetworkCompData(CompNum).EPlusTypeNum == iEPlusComponentType::FAN &&
                        state.dataAirflowNetwork->AirflowNetworkLinkageData(j).NodeNums[1] == i) {
                        found = false;
                        break;
                    }
                    // Overwrite return connection outlet
                    if (state.dataAirflowNetwork->AirflowNetworkLinkageData(j).ConnectionFlag == iEPlusComponentType::RCN) { // Modified on 9/2/09
                        found = true;
                        break;
                    }
                    if (state.dataAirflowNetwork->AirflowNetworkLinkageData(j).ConnectionFlag == iEPlusComponentType::SCN &&
                        state.dataAirflowNetwork->AirflowNetworkLinkageData(j).NodeNums[1] == i) { // Modified on 9/2/09
                        found = true;
                        break;
                    }
                }
                if (state.dataAirflowNetwork->AirflowNetworkLinkageData(j).NodeNums[1] == i &&
                    state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(j).NodeNums[0])
                            .EPlusTypeNum == iEPlusNodeType::OAN) {
                    OANode = true;
                    break;
                }
            }
            if (found) continue;
            if (state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusZoneNum == 0 &&
                state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusTypeNum == iEPlusNodeType::ZIN)
                continue;
            j = state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusNodeNum;

            if (j > 0 && (state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusZoneNum > 0 ||
                          state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusTypeNum == iEPlusNodeType::FOU ||
                          state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusTypeNum == iEPlusNodeType::COU ||
                          state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusTypeNum == iEPlusNodeType::HXO)) {
                state.dataAirflowNetworkBalanceManager->MA((i - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + i) = 1.0e10;
                state.dataAirflowNetworkBalanceManager->MV(i) = Node(j).Temp * 1.0e10;
            }
            if (j > 0 && OANode) {
                state.dataAirflowNetworkBalanceManager->MA((i - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + i) = 1.0e10;
                state.dataAirflowNetworkBalanceManager->MV(i) = Node(j).Temp * 1.0e10;
            }
            if (state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusZoneNum > 0 &&
                state.dataAirflowNetworkBalanceManager->MA((i - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + i) < 0.9e10) {
                ZoneNum = state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusZoneNum;
                state.dataAirflowNetworkBalanceManager->MA((i - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + i) = 1.0e10;
                state.dataAirflowNetworkBalanceManager->MV(i) = state.dataAirflowNetwork->ANZT(ZoneNum) * 1.0e10;
            }
            if (state.dataAirflowNetwork->AirflowNetworkNodeData(i).ExtNodeNum > 0 &&
                state.dataAirflowNetworkBalanceManager->MA((i - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + i) < 0.9e10) {
                state.dataAirflowNetworkBalanceManager->MA((i - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + i) = 1.0e10;
                if (state.dataAirflowNetwork->AirflowNetworkNodeData(i).OutAirNodeNum > 0) {
                    state.dataAirflowNetworkBalanceManager->MV(i) =
                        Node(state.dataAirflowNetwork->AirflowNetworkNodeData(i).OutAirNodeNum).OutAirDryBulb * 1.0e10;
                } else {
                    state.dataAirflowNetworkBalanceManager->MV(i) =
                        OutDryBulbTempAt(state, state.dataAirflowNetwork->AirflowNetworkNodeData(i).NodeHeight) * 1.0e10;
                }
            }
            if (state.dataAirflowNetwork->AirflowNetworkNodeData(i).RAFNNodeNum > 0 &&
                state.dataAirflowNetworkBalanceManager->MA((i - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + i) < 0.9e10) {
                state.dataAirflowNetworkBalanceManager->MA((i - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + i) = 1.0e10;
                ZoneNum = state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusZoneNum;
                if (state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum)
                        .Node(state.dataAirflowNetwork->AirflowNetworkNodeData(i).RAFNNodeNum)
                        .AirflowNetworkNodeID == i) {
                    state.dataAirflowNetworkBalanceManager->MV(i) = state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum)
                                                                        .Node(state.dataAirflowNetwork->AirflowNetworkNodeData(i).RAFNNodeNum)
                                                                        .AirTemp *
                                                                    1.0e10;
                }
            }
        }

        // Assign node value to distribution nodes with fan off
        for (i = 1 + state.dataAirflowNetwork->NumOfNodesMultiZone; i <= state.dataAirflowNetwork->AirflowNetworkNumOfNodes; ++i) {
            j = state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusNodeNum;
            if (j > 0 && !state.dataAirflowNetworkBalanceManager->LoopOnOffFlag(state.dataAirflowNetwork->AirflowNetworkNodeData(i).AirLoopNum) &&
                state.dataAirflowNetworkBalanceManager->MA((i - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + i) < 1.0e9) {
                state.dataAirflowNetworkBalanceManager->MA((i - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + i) = 1.0e10;
                state.dataAirflowNetworkBalanceManager->MV(i) = Node(j).Temp * 1.0e10;
            }
            if (j == 0 && i > state.dataAirflowNetwork->NumOfNodesMultiZone &&
                !state.dataAirflowNetworkBalanceManager->LoopOnOffFlag(state.dataAirflowNetwork->AirflowNetworkNodeData(i).AirLoopNum)) {
                state.dataAirflowNetworkBalanceManager->MA((i - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + i) = 1.0e10;
                state.dataAirflowNetworkBalanceManager->MV(i) = state.dataAirflowNetwork->AirflowNetworkNodeSimu(i).TZlast * 1.0e10;
            }
        }

        // Check singularity
        for (i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfNodes; ++i) {
            if (state.dataAirflowNetworkBalanceManager->MA((i - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + i) < 1.0e-6) {
                if (i > state.dataAirflowNetwork->NumOfNodesMultiZone &&
                    !state.dataAirflowNetworkBalanceManager->LoopOnOffFlag(state.dataAirflowNetwork->AirflowNetworkNodeData(i).AirLoopNum)) {
                    state.dataAirflowNetworkBalanceManager->MA((i - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + i) = 1.0e10;
                    state.dataAirflowNetworkBalanceManager->MV(i) = state.dataAirflowNetwork->AirflowNetworkNodeSimu(i).TZlast * 1.0e10;
                } else {
                    ShowFatalError(state,
                                   "CalcAirflowNetworkHeatBalance: A diagonal entity is zero in AirflowNetwork matrix at node " +
                                       state.dataAirflowNetwork->AirflowNetworkNodeData(i).Name);
                }
            }
        }

        // Get an inverse matrix
        MRXINV(state, state.dataAirflowNetwork->AirflowNetworkNumOfNodes);

        // Calculate node temperatures
        for (i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfNodes; ++i) {
            TZON = 0.0;
            for (j = 1; j <= state.dataAirflowNetwork->AirflowNetworkNumOfNodes; ++j) {
                TZON += state.dataAirflowNetworkBalanceManager->MA((i - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + j) *
                        state.dataAirflowNetworkBalanceManager->MV(j);
            }
            state.dataAirflowNetwork->AirflowNetworkNodeSimu(i).TZ = TZON;
        }
    }

    void CalcAirflowNetworkMoisBalance(EnergyPlusData &state)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   Oct. 2005
        //       MODIFIED       na
        //       RE-ENGINEERED  Revised based on Subroutine CalcADSMoistureBalance

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine performs AirflowNetwork moisture simulations.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int i;
        int j;
        int LF;
        int LT;
        int CompNum;
        int NF;
        int NT;
        iComponentTypeNum CompTypeNum;
        int TypeNum;
        std::string CompName;
        Real64 Ei;
        Real64 DirSign;
        Real64 Wamb;
        Real64 WZON;
        Real64 load;
        int ZoneNum;
        bool found;
        bool OANode;

        auto &Node(state.dataLoopNodes->Node);

        state.dataAirflowNetworkBalanceManager->MA = 0.0;
        state.dataAirflowNetworkBalanceManager->MV = 0.0;
        for (i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfLinks; ++i) {
            CompNum = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).CompNum;
            CompTypeNum = state.dataAirflowNetwork->AirflowNetworkCompData(CompNum).CompTypeNum;
            CompName = state.dataAirflowNetwork->AirflowNetworkCompData(CompNum).EPlusName;
            // Calculate duct moisture diffusion loss
            if (CompTypeNum == iComponentTypeNum::DWC && CompName == std::string()) { // Duct component only
                TypeNum = state.dataAirflowNetwork->AirflowNetworkCompData(CompNum).TypeNum;
                if (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW > 0.0) { // flow direction is the same as input from node 1 to node 2
                    LF = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
                    LT = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
                    DirSign = 1.0;
                } else { // flow direction is the opposite as input from node 2 to node 1
                    LF = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
                    LT = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
                    DirSign = -1.0;
                }
                Ei = General::epexp(-state.dataAirflowNetwork->DisSysCompDuctData(TypeNum).UMoisture *
                                        state.dataAirflowNetwork->DisSysCompDuctData(TypeNum).L *
                                        state.dataAirflowNetwork->DisSysCompDuctData(TypeNum).hydraulicDiameter * DataGlobalConstants::Pi,
                                    (DirSign * state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW));
                if (state.dataAirflowNetwork->AirflowNetworkLinkageData(i).ZoneNum < 0) {
                    Wamb = state.dataEnvrn->OutHumRat;
                } else if (state.dataAirflowNetwork->AirflowNetworkLinkageData(i).ZoneNum == 0) {
                    Wamb = state.dataAirflowNetwork->AirflowNetworkNodeSimu(LT).WZ;
                } else {
                    Wamb = state.dataAirflowNetwork->ANZW(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).ZoneNum);
                }
                if (!state.dataAirflowNetworkBalanceManager->LoopOnOffFlag(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).AirLoopNum) &&
                    state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW <= 0.0) {
                    Ei = General::epexp(-state.dataAirflowNetwork->DisSysCompDuctData(TypeNum).UMoisture *
                                            state.dataAirflowNetwork->DisSysCompDuctData(TypeNum).L *
                                            state.dataAirflowNetwork->DisSysCompDuctData(TypeNum).hydraulicDiameter * DataGlobalConstants::Pi,
                                        (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2));
                    state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LT) +=
                        std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2);
                    state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LF) =
                        -std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2) * Ei;
                    state.dataAirflowNetworkBalanceManager->MV(LT) +=
                        std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2) * Wamb * (1.0 - Ei);
                } else {
                    state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LT) +=
                        std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW);
                    state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LF) =
                        -std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW) * Ei;
                    state.dataAirflowNetworkBalanceManager->MV(LT) +=
                        std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW) * Wamb * (1.0 - Ei);
                }
            }
            if (CompTypeNum == iComponentTypeNum::TMU) { // Reheat unit: SINGLE DUCT:CONST VOLUME:REHEAT
                TypeNum = state.dataAirflowNetwork->AirflowNetworkCompData(CompNum).TypeNum;
                if (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW > 0.0) { // flow direction is the same as input from node 1 to node 2
                    LF = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
                    LT = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
                    DirSign = 1.0;
                } else { // flow direction is the opposite as input from node 2 to node 1
                    LF = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
                    LT = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
                    DirSign = -1.0;
                }
                Ei = General::epexp(-0.0001 * state.dataAirflowNetwork->DisSysCompTermUnitData(TypeNum).L *
                                        state.dataAirflowNetwork->DisSysCompTermUnitData(TypeNum).hydraulicDiameter * DataGlobalConstants::Pi,
                                    (DirSign * state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW));
                Wamb = state.dataAirflowNetwork->AirflowNetworkNodeSimu(LT).WZ;
                if (!state.dataAirflowNetworkBalanceManager->LoopOnOffFlag(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).AirLoopNum) &&
                    state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW <= 0.0) {

                    Ei = General::epexp(-0.0001 * state.dataAirflowNetwork->DisSysCompTermUnitData(TypeNum).L *
                                            state.dataAirflowNetwork->DisSysCompTermUnitData(TypeNum).hydraulicDiameter * DataGlobalConstants::Pi,
                                        (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2));
                    state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LT) +=
                        std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2);
                    state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LF) =
                        -std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2) * Ei;
                    state.dataAirflowNetworkBalanceManager->MV(LT) +=
                        std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2) * Wamb * (1.0 - Ei);
                } else {
                    state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LT) +=
                        std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW);
                    state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LF) =
                        -std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW) * Ei;
                    state.dataAirflowNetworkBalanceManager->MV(LT) +=
                        std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW) * Wamb * (1.0 - Ei);
                }
            }
            if (CompTypeNum == iComponentTypeNum::COI) { // heating or cooling coil
                TypeNum = state.dataAirflowNetwork->AirflowNetworkCompData(CompNum).TypeNum;
                if (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW > 0.0) { // flow direction is the same as input from node 1 to node 2
                    LF = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
                    LT = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
                    DirSign = 1.0;
                } else { // flow direction is the opposite as input from node 2 to node 1
                    LF = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
                    LT = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
                    DirSign = -1.0;
                }
            }
            // Calculate temp in a constant pressure drop component
            if (CompTypeNum == iComponentTypeNum::CPD && CompName == std::string()) { // constant pressure element only
                if (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW > 0.0) { // flow direction is the same as input from node 1 to node 2
                    LF = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
                    LT = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
                } else { // flow direction is the opposite as input from node 2 to node 1
                    LF = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
                    LT = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
                }
                if (!state.dataAirflowNetworkBalanceManager->LoopOnOffFlag(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).AirLoopNum) &&
                    state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW <= 0.0) {
                    state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LT) +=
                        std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2);
                    state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LF) =
                        -std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2);
                } else {
                    state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LT) +=
                        std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW);
                    state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LF) =
                        -std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW);
                }
                state.dataAirflowNetworkBalanceManager->MV(LT) = 0.0;
            }
            // Calculate return leak
            if ((CompTypeNum == iComponentTypeNum::PLR || CompTypeNum == iComponentTypeNum::ELR) && CompName == std::string()) {
                // Return leak component only
                if ((state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0])
                         .EPlusZoneNum > 0) &&
                    (state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1])
                         .EPlusZoneNum == 0) &&
                    (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW > 0.0)) {
                    LF = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
                    LT = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
                    state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LT) +=
                        std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW);
                    state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LF) =
                        -std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW);
                }
                if ((state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0]).ExtNodeNum >
                     0) &&
                    (state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1])
                         .EPlusZoneNum == 0) &&
                    (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW > 0.0)) {
                    LF = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
                    LT = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
                    state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LT) +=
                        std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW);
                    state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LF) =
                        -std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW);
                }
                if ((state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1])
                         .EPlusZoneNum > 0) &&
                    (state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0])
                         .EPlusZoneNum == 0) &&
                    (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2 > 0.0)) {
                    LF = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
                    LT = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
                    state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LT) +=
                        std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2);
                    state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LF) =
                        -std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2);
                }
                if ((state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1]).ExtNodeNum >
                     0) &&
                    (state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0])
                         .EPlusZoneNum == 0) &&
                    (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2 > 0.0)) {
                    LF = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
                    LT = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
                    state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LT) +=
                        std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2);
                    state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LF) =
                        -std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2);
                }
            }
            // Check reheat unit
            if (state.dataAirflowNetwork->AirflowNetworkCompData(CompNum).EPlusTypeNum == iEPlusComponentType::RHT &&
                (!state.dataAirflowNetwork->AirflowNetworkLinkageData(i).VAVTermDamper)) {
                NF = 0;
                NT = 0;
                if (state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0])
                        .EPlusNodeNum > 0) {
                    NF = state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0])
                             .EPlusNodeNum;
                }
                if (state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1])
                        .EPlusNodeNum > 0) {
                    NT = state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1])
                             .EPlusNodeNum;
                }
                if ((NF == 0) || (NT == 0)) {
                    ShowFatalError(state,
                                   "Node number in the primary air loop is not found in AIRFLOWNETWORK:DISTRIBUTION:NODE = " +
                                       state.dataAirflowNetwork->AirflowNetworkLinkageData(i).Name);
                }
                if (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW > 0.0) {
                    LF = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
                    LT = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
                    load = Node(NT).HumRat - Node(NF).HumRat;
                } else {
                    LF = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
                    LT = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
                    load = Node(NF).HumRat - Node(NT).HumRat;
                }
                state.dataAirflowNetworkBalanceManager->MV(LT) += state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW * load;
            }
        }

        // Prescribe temperature for EPlus nodes
        for (i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfNodes; ++i) {
            found = false;
            OANode = false;
            for (j = 1; j <= state.dataAirflowNetwork->AirflowNetworkNumOfLinks; ++j) {
                if (state.dataAirflowNetwork->AirflowNetworkLinkageData(j).NodeNums[0] == i ||
                    state.dataAirflowNetwork->AirflowNetworkLinkageData(j).NodeNums[1] == i) {
                    CompNum = state.dataAirflowNetwork->AirflowNetworkLinkageData(j).CompNum;
                    if (state.dataAirflowNetwork->AirflowNetworkCompData(CompNum).EPlusTypeNum == iEPlusComponentType::RHT &&
                        (!state.dataAirflowNetwork->AirflowNetworkLinkageData(j).VAVTermDamper)) {
                        found = true;
                        break;
                    }
                    // Overwrite fan outlet node
                    if (state.dataAirflowNetwork->AirflowNetworkCompData(CompNum).EPlusTypeNum == iEPlusComponentType::FAN &&
                        state.dataAirflowNetwork->AirflowNetworkLinkageData(j).NodeNums[1] == i) {
                        found = false;
                        break;
                    }
                    // Overwrite return connection outlet
                    if (state.dataAirflowNetwork->AirflowNetworkLinkageData(j).ConnectionFlag == iEPlusComponentType::RCN) { // Modified on 9/2/09
                        found = true;
                        break;
                    }
                    if (state.dataAirflowNetwork->AirflowNetworkLinkageData(j).ConnectionFlag == iEPlusComponentType::SCN &&
                        state.dataAirflowNetwork->AirflowNetworkLinkageData(j).NodeNums[1] == i) { // Modified on 9/2/09
                        found = true;
                        break;
                    }
                }
                if (state.dataAirflowNetwork->AirflowNetworkLinkageData(j).NodeNums[1] == i &&
                    state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(j).NodeNums[0])
                            .EPlusTypeNum == iEPlusNodeType::OAN) {
                    OANode = true;
                    break;
                }
            }
            if (found) continue;
            if (state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusZoneNum == 0 &&
                state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusTypeNum == iEPlusNodeType::ZIN)
                continue;
            j = state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusNodeNum;
            if (j > 0 && (state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusZoneNum > 0 ||
                          state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusTypeNum == iEPlusNodeType::FOU ||
                          state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusTypeNum == iEPlusNodeType::COU ||
                          state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusTypeNum == iEPlusNodeType::HXO)) {
                state.dataAirflowNetworkBalanceManager->MA((i - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + i) = 1.0e10;
                state.dataAirflowNetworkBalanceManager->MV(i) = Node(j).HumRat * 1.0e10;
            }
            if (j > 0 && OANode) {
                state.dataAirflowNetworkBalanceManager->MA((i - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + i) = 1.0e10;
                state.dataAirflowNetworkBalanceManager->MV(i) = Node(j).HumRat * 1.0e10;
            }
            if (state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusZoneNum > 0 &&
                state.dataAirflowNetworkBalanceManager->MA((i - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + i) < 0.9e10) {
                ZoneNum = state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusZoneNum;
                state.dataAirflowNetworkBalanceManager->MA((i - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + i) = 1.0e10;
                state.dataAirflowNetworkBalanceManager->MV(i) = state.dataAirflowNetwork->ANZW(ZoneNum) * 1.0e10;
            }
            if (state.dataAirflowNetwork->AirflowNetworkNodeData(i).ExtNodeNum > 0) {
                state.dataAirflowNetworkBalanceManager->MA((i - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + i) = 1.0e10;
                state.dataAirflowNetworkBalanceManager->MV(i) = state.dataEnvrn->OutHumRat * 1.0e10;
            }
            if (state.dataAirflowNetwork->AirflowNetworkNodeData(i).RAFNNodeNum > 0 &&
                state.dataAirflowNetworkBalanceManager->MA((i - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + i) < 0.9e10) {
                state.dataAirflowNetworkBalanceManager->MA((i - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + i) = 1.0e10;
                ZoneNum = state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusZoneNum;
                if (state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum)
                        .Node(state.dataAirflowNetwork->AirflowNetworkNodeData(i).RAFNNodeNum)
                        .AirflowNetworkNodeID == i) {
                    state.dataAirflowNetworkBalanceManager->MV(i) = state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum)
                                                                        .Node(state.dataAirflowNetwork->AirflowNetworkNodeData(i).RAFNNodeNum)
                                                                        .HumRat *
                                                                    1.0e10;
                }
            }
        }

        // Assign node value to distribution nodes with fan off
        for (i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfNodes; ++i) {
            j = state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusNodeNum;
            if (j > 0 && !state.dataAirflowNetworkBalanceManager->LoopOnOffFlag(state.dataAirflowNetwork->AirflowNetworkNodeData(i).AirLoopNum) &&
                state.dataAirflowNetworkBalanceManager->MA((i - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + i) < 1.0e9) {
                state.dataAirflowNetworkBalanceManager->MA((i - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + i) = 1.0e10;
                state.dataAirflowNetworkBalanceManager->MV(i) = Node(j).HumRat * 1.0e10;
            }
            if (j == 0 && i > state.dataAirflowNetwork->NumOfNodesMultiZone &&
                !state.dataAirflowNetworkBalanceManager->LoopOnOffFlag(state.dataAirflowNetwork->AirflowNetworkNodeData(i).AirLoopNum)) {
                state.dataAirflowNetworkBalanceManager->MA((i - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + i) = 1.0e10;
                state.dataAirflowNetworkBalanceManager->MV(i) = state.dataAirflowNetwork->AirflowNetworkNodeSimu(i).WZlast * 1.0e10;
            }
        }

        // Check singularity
        for (i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfNodes; ++i) {
            if (state.dataAirflowNetworkBalanceManager->MA((i - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + i) < 1.0e-8) {
                ShowFatalError(state,
                               "CalcAirflowNetworkMoisBalance: A diagonal entity is zero in AirflowNetwork matrix at node " +
                                   state.dataAirflowNetwork->AirflowNetworkNodeData(i).Name);
            }
        }

        // Get an inverse matrix
        MRXINV(state, state.dataAirflowNetwork->AirflowNetworkNumOfNodes);

        // Calculate node temperatures
        for (i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfNodes; ++i) {
            WZON = 0.0;
            for (j = 1; j <= state.dataAirflowNetwork->AirflowNetworkNumOfNodes; ++j) {
                WZON += state.dataAirflowNetworkBalanceManager->MA((i - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + j) *
                        state.dataAirflowNetworkBalanceManager->MV(j);
            }
            state.dataAirflowNetwork->AirflowNetworkNodeSimu(i).WZ = WZON;
        }
    }

    void CalcAirflowNetworkCO2Balance(EnergyPlusData &state)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   June. 2010
        //       MODIFIED       na
        //       RE-ENGINEERED  Revised based on Subroutine CalcAirflowNetworkMoisBalance

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine performs AirflowNetwork CO2 simulations.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int i;
        int j;
        int LF;
        int LT;
        int CompNum;
        iComponentTypeNum CompTypeNum;
        int TypeNum;
        std::string CompName;
        Real64 DirSign;
        Real64 COZN;
        int ZoneNum;
        bool found;
        bool OANode;

        state.dataAirflowNetworkBalanceManager->MA = 0.0;
        state.dataAirflowNetworkBalanceManager->MV = 0.0;
        for (i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfLinks; ++i) {
            CompNum = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).CompNum;
            CompTypeNum = state.dataAirflowNetwork->AirflowNetworkCompData(CompNum).CompTypeNum;
            CompName = state.dataAirflowNetwork->AirflowNetworkCompData(CompNum).EPlusName;
            // Calculate duct moisture diffusion loss
            if (CompTypeNum == iComponentTypeNum::DWC && CompName == std::string()) { // Duct component only
                TypeNum = state.dataAirflowNetwork->AirflowNetworkCompData(CompNum).TypeNum;
                if (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW > 0.0) { // flow direction is the same as input from node 1 to node 2
                    LF = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
                    LT = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
                    DirSign = 1.0;
                } else { // flow direction is the opposite as input from node 2 to node 1
                    LF = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
                    LT = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
                    DirSign = -1.0;
                }
                state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LT) +=
                    std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW);
                state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LF) =
                    -std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW);
            }
            if (CompTypeNum == iComponentTypeNum::TMU) { // Reheat unit: SINGLE DUCT:CONST VOLUME:REHEAT
                TypeNum = state.dataAirflowNetwork->AirflowNetworkCompData(CompNum).TypeNum;
                if (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW > 0.0) { // flow direction is the same as input from node 1 to node 2
                    LF = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
                    LT = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
                    DirSign = 1.0;
                } else { // flow direction is the opposite as input from node 2 to node 1
                    LF = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
                    LT = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
                    DirSign = -1.0;
                }
                state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LT) +=
                    std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW);
                state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LF) =
                    -std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW);
            }
            if (CompTypeNum == iComponentTypeNum::COI) { // heating or cooling coil
                TypeNum = state.dataAirflowNetwork->AirflowNetworkCompData(CompNum).TypeNum;
                if (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW > 0.0) { // flow direction is the same as input from node 1 to node 2
                    LF = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
                    LT = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
                    DirSign = 1.0;
                } else { // flow direction is the opposite as input from node 2 to node 1
                    LF = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
                    LT = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
                    DirSign = -1.0;
                }
            }
            // Calculate temp in a constant pressure drop component
            if (CompTypeNum == iComponentTypeNum::CPD && CompName == std::string()) { // constant pressure element only
                if (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW > 0.0) { // flow direction is the same as input from node 1 to node 2
                    LF = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
                    LT = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
                } else { // flow direction is the opposite as input from node 2 to node 1
                    LF = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
                    LT = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
                }
                state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LT) +=
                    std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW);
                state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LF) =
                    -std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW);
                state.dataAirflowNetworkBalanceManager->MV(LT) = 0.0;
            }
            // Calculate return leak
            if ((CompTypeNum == iComponentTypeNum::PLR || CompTypeNum == iComponentTypeNum::ELR) && CompName == std::string()) {
                // Return leak component only
                if ((state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0])
                         .EPlusZoneNum > 0) &&
                    (state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1])
                         .EPlusZoneNum == 0) &&
                    (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW > 0.0)) {
                    LF = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
                    LT = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
                    state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LT) +=
                        std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW);
                    state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LF) =
                        -std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW);
                }
                if ((state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0]).ExtNodeNum >
                     0) &&
                    (state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1])
                         .EPlusZoneNum == 0) &&
                    (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW > 0.0)) {
                    LF = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
                    LT = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
                    state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LT) +=
                        std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW);
                    state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LF) =
                        -std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW);
                }
                if ((state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1])
                         .EPlusZoneNum > 0) &&
                    (state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0])
                         .EPlusZoneNum == 0) &&
                    (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2 > 0.0)) {
                    LF = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
                    LT = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
                    state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LT) +=
                        std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2);
                    state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LF) =
                        -std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2);
                }
                if ((state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1]).ExtNodeNum >
                     0) &&
                    (state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0])
                         .EPlusZoneNum == 0) &&
                    (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2 > 0.0)) {
                    LF = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
                    LT = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
                    state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LT) +=
                        std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2);
                    state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LF) =
                        -std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2);
                }
            }
        }

        // Prescribe temperature for EPlus nodes
        for (i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfNodes; ++i) {
            found = false;
            OANode = false;
            for (j = 1; j <= state.dataAirflowNetwork->AirflowNetworkNumOfLinks; ++j) {
                if (state.dataAirflowNetwork->AirflowNetworkLinkageData(j).NodeNums[0] == i ||
                    state.dataAirflowNetwork->AirflowNetworkLinkageData(j).NodeNums[1] == i) {
                    CompNum = state.dataAirflowNetwork->AirflowNetworkLinkageData(j).CompNum;
                    if (state.dataAirflowNetwork->AirflowNetworkCompData(CompNum).EPlusTypeNum == iEPlusComponentType::RHT &&
                        (!state.dataAirflowNetwork->AirflowNetworkLinkageData(j).VAVTermDamper)) {
                        found = true;
                        break;
                    }
                    // Overwrite fan outlet node
                    if (state.dataAirflowNetwork->AirflowNetworkCompData(CompNum).EPlusTypeNum == iEPlusComponentType::FAN &&
                        state.dataAirflowNetwork->AirflowNetworkLinkageData(j).NodeNums[1] == i) {
                        found = false;
                        break;
                    }
                    // Overwrite return connection outlet
                    if (state.dataAirflowNetwork->AirflowNetworkLinkageData(j).ConnectionFlag == iEPlusComponentType::RCN) { // Modified on 9/2/09
                        found = true;
                        break;
                    }
                    if (state.dataAirflowNetwork->AirflowNetworkLinkageData(j).ConnectionFlag == iEPlusComponentType::SCN &&
                        state.dataAirflowNetwork->AirflowNetworkLinkageData(j).NodeNums[1] == i) { // Modified on 9/2/09
                        found = true;
                        break;
                    }
                }
                if (state.dataAirflowNetwork->AirflowNetworkLinkageData(j).NodeNums[1] == i &&
                    state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(j).NodeNums[0])
                            .EPlusTypeNum == iEPlusNodeType::OAN) {
                    OANode = true;
                    break;
                }
            }
            if (found) continue;
            if (state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusZoneNum == 0 &&
                state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusTypeNum == iEPlusNodeType::ZIN)
                continue;
            j = state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusNodeNum;
            if (j > 0 && (state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusZoneNum > 0 ||
                          state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusTypeNum == iEPlusNodeType::FOU ||
                          state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusTypeNum == iEPlusNodeType::COU ||
                          state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusTypeNum == iEPlusNodeType::HXO)) {
                state.dataAirflowNetworkBalanceManager->MA((i - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + i) = 1.0e10;
                state.dataAirflowNetworkBalanceManager->MV(i) = state.dataLoopNodes->Node(j).CO2 * 1.0e10;
            }
            if (j > 0 && OANode) {
                state.dataAirflowNetworkBalanceManager->MA((i - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + i) = 1.0e10;
                state.dataAirflowNetworkBalanceManager->MV(i) = state.dataLoopNodes->Node(j).CO2 * 1.0e10;
            }
            if (state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusZoneNum > 0 &&
                state.dataAirflowNetworkBalanceManager->MA((i - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + i) < 0.9e10) {
                ZoneNum = state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusZoneNum;
                state.dataAirflowNetworkBalanceManager->MA((i - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + i) = 1.0e10;
                state.dataAirflowNetworkBalanceManager->MV(i) = state.dataAirflowNetwork->ANCO(ZoneNum) * 1.0e10;
            }
            if (state.dataAirflowNetwork->AirflowNetworkNodeData(i).ExtNodeNum > 0) {
                state.dataAirflowNetworkBalanceManager->MA((i - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + i) = 1.0e10;
                state.dataAirflowNetworkBalanceManager->MV(i) = state.dataContaminantBalance->OutdoorCO2 * 1.0e10;
            }
        }

        // Assign node value to distribution nodes with fan off
        for (i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfNodes; ++i) {
            j = state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusNodeNum;
            if (j > 0 && !state.dataAirflowNetworkBalanceManager->LoopOnOffFlag(state.dataAirflowNetwork->AirflowNetworkNodeData(i).AirLoopNum) &&
                state.dataAirflowNetworkBalanceManager->MA((i - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + i) < 1.0e9) {
                state.dataAirflowNetworkBalanceManager->MA((i - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + i) = 1.0e10;
                state.dataAirflowNetworkBalanceManager->MV(i) = state.dataLoopNodes->Node(j).CO2 * 1.0e10;
            }
            if (j == 0 && i > state.dataAirflowNetwork->NumOfNodesMultiZone &&
                !state.dataAirflowNetworkBalanceManager->LoopOnOffFlag(state.dataAirflowNetwork->AirflowNetworkNodeData(i).AirLoopNum)) {
                state.dataAirflowNetworkBalanceManager->MA((i - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + i) = 1.0e10;
                state.dataAirflowNetworkBalanceManager->MV(i) = state.dataAirflowNetwork->AirflowNetworkNodeSimu(i).CO2Zlast * 1.0e10;
            }
        }

        // Check singularity
        for (i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfNodes; ++i) {
            if (state.dataAirflowNetworkBalanceManager->MA((i - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + i) < 1.0e-6) {
                ShowFatalError(state,
                               "CalcAirflowNetworkCO2Balance: A diagonal entity is zero in AirflowNetwork matrix at node " +
                                   state.dataAirflowNetwork->AirflowNetworkNodeData(i).Name);
            }
        }

        // Get an inverse matrix
        MRXINV(state, state.dataAirflowNetwork->AirflowNetworkNumOfNodes);

        // Calculate node temperatures
        for (i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfNodes; ++i) {
            COZN = 0.0;
            for (j = 1; j <= state.dataAirflowNetwork->AirflowNetworkNumOfNodes; ++j) {
                COZN += state.dataAirflowNetworkBalanceManager->MA((i - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + j) *
                        state.dataAirflowNetworkBalanceManager->MV(j);
            }
            state.dataAirflowNetwork->AirflowNetworkNodeSimu(i).CO2Z = COZN;
        }
    }

    void CalcAirflowNetworkGCBalance(EnergyPlusData &state)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   Jan. 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  Revised based on Subroutine CalcAirflowNetworkCO2Balance

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine performs AirflowNetwork generic contaminant simulations.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int i;
        int j;
        int LF;
        int LT;
        int CompNum;
        iComponentTypeNum CompTypeNum;
        int TypeNum;
        std::string CompName;
        Real64 DirSign;
        Real64 COZN;
        int ZoneNum;
        bool found;
        bool OANode;

        state.dataAirflowNetworkBalanceManager->MA = 0.0;
        state.dataAirflowNetworkBalanceManager->MV = 0.0;
        for (i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfLinks; ++i) {
            CompNum = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).CompNum;
            CompTypeNum = state.dataAirflowNetwork->AirflowNetworkCompData(CompNum).CompTypeNum;
            CompName = state.dataAirflowNetwork->AirflowNetworkCompData(CompNum).EPlusName;
            // Calculate duct moisture diffusion loss
            if (CompTypeNum == iComponentTypeNum::DWC && CompName == std::string()) { // Duct component only
                TypeNum = state.dataAirflowNetwork->AirflowNetworkCompData(CompNum).TypeNum;
                if (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW > 0.0) { // flow direction is the same as input from node 1 to node 2
                    LF = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
                    LT = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
                    DirSign = 1.0;
                } else { // flow direction is the opposite as input from node 2 to node 1
                    LF = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
                    LT = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
                    DirSign = -1.0;
                }
                state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LT) +=
                    std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW);
                state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LF) =
                    -std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW);
            }
            if (CompTypeNum == iComponentTypeNum::TMU) { // Reheat unit: SINGLE DUCT:CONST VOLUME:REHEAT
                TypeNum = state.dataAirflowNetwork->AirflowNetworkCompData(CompNum).TypeNum;
                if (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW > 0.0) { // flow direction is the same as input from node 1 to node 2
                    LF = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
                    LT = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
                    DirSign = 1.0;
                } else { // flow direction is the opposite as input from node 2 to node 1
                    LF = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
                    LT = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
                    DirSign = -1.0;
                }
                state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LT) +=
                    std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW);
                state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LF) =
                    -std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW);
            }
            if (CompTypeNum == iComponentTypeNum::COI) { // heating or cooling coil
                TypeNum = state.dataAirflowNetwork->AirflowNetworkCompData(CompNum).TypeNum;
                if (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW > 0.0) { // flow direction is the same as input from node 1 to node 2
                    LF = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
                    LT = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
                    DirSign = 1.0;
                } else { // flow direction is the opposite as input from node 2 to node 1
                    LF = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
                    LT = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
                    DirSign = -1.0;
                }
            }
            // Calculate temp in a constant pressure drop component
            if (CompTypeNum == iComponentTypeNum::CPD && CompName == std::string()) { // constant pressure element only
                if (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW > 0.0) { // flow direction is the same as input from node 1 to node 2
                    LF = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
                    LT = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
                } else { // flow direction is the opposite as input from node 2 to node 1
                    LF = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
                    LT = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
                }
                state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LT) +=
                    std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW);
                state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LF) =
                    -std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW);
                state.dataAirflowNetworkBalanceManager->MV(LT) = 0.0;
            }
            // Calculate return leak
            if ((CompTypeNum == iComponentTypeNum::PLR || CompTypeNum == iComponentTypeNum::ELR) && CompName == std::string()) {
                // Return leak component only
                if ((state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0])
                         .EPlusZoneNum > 0) &&
                    (state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1])
                         .EPlusZoneNum == 0) &&
                    (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW > 0.0)) {
                    LF = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
                    LT = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
                    state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LT) +=
                        std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW);
                    state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LF) =
                        -std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW);
                }
                if ((state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0]).ExtNodeNum >
                     0) &&
                    (state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1])
                         .EPlusZoneNum == 0) &&
                    (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW > 0.0)) {
                    LF = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
                    LT = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
                    state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LT) +=
                        std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW);
                    state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LF) =
                        -std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW);
                }
                if ((state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1])
                         .EPlusZoneNum > 0) &&
                    (state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0])
                         .EPlusZoneNum == 0) &&
                    (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2 > 0.0)) {
                    LF = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
                    LT = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
                    state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LT) +=
                        std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2);
                    state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LF) =
                        -std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2);
                }
                if ((state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1]).ExtNodeNum >
                     0) &&
                    (state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0])
                         .EPlusZoneNum == 0) &&
                    (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2 > 0.0)) {
                    LF = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
                    LT = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
                    state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LT) +=
                        std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2);
                    state.dataAirflowNetworkBalanceManager->MA((LT - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + LF) =
                        -std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2);
                }
            }
        }

        // Prescribe temperature for EPlus nodes
        for (i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfNodes; ++i) {
            found = false;
            OANode = false;
            for (j = 1; j <= state.dataAirflowNetwork->AirflowNetworkNumOfLinks; ++j) {
                if (state.dataAirflowNetwork->AirflowNetworkLinkageData(j).NodeNums[0] == i ||
                    state.dataAirflowNetwork->AirflowNetworkLinkageData(j).NodeNums[1] == i) {
                    CompNum = state.dataAirflowNetwork->AirflowNetworkLinkageData(j).CompNum;
                    if (state.dataAirflowNetwork->AirflowNetworkCompData(CompNum).EPlusTypeNum == iEPlusComponentType::RHT &&
                        (!state.dataAirflowNetwork->AirflowNetworkLinkageData(j).VAVTermDamper)) {
                        found = true;
                        break;
                    }
                    // Overwrite fan outlet node
                    if (state.dataAirflowNetwork->AirflowNetworkCompData(CompNum).EPlusTypeNum == iEPlusComponentType::FAN &&
                        state.dataAirflowNetwork->AirflowNetworkLinkageData(j).NodeNums[1] == i) {
                        found = false;
                        break;
                    }
                    // Overwrite return connection outlet
                    if (state.dataAirflowNetwork->AirflowNetworkLinkageData(j).ConnectionFlag == iEPlusComponentType::RCN) { // Modified on 9/2/09
                        found = true;
                        break;
                    }
                    if (state.dataAirflowNetwork->AirflowNetworkLinkageData(j).ConnectionFlag == iEPlusComponentType::SCN &&
                        state.dataAirflowNetwork->AirflowNetworkLinkageData(j).NodeNums[1] == i) { // Modified on 9/2/09
                        found = true;
                        break;
                    }
                }
                if (state.dataAirflowNetwork->AirflowNetworkLinkageData(j).NodeNums[1] == i &&
                    state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(j).NodeNums[0])
                            .EPlusTypeNum == iEPlusNodeType::OAN) {
                    OANode = true;
                    break;
                }
            }
            if (found) continue;
            if (state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusZoneNum == 0 &&
                state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusTypeNum == iEPlusNodeType::ZIN)
                continue;
            j = state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusNodeNum;
            if (j > 0 && (state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusZoneNum > 0 ||
                          state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusTypeNum == iEPlusNodeType::FOU ||
                          state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusTypeNum == iEPlusNodeType::COU ||
                          state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusTypeNum == iEPlusNodeType::HXO)) {
                state.dataAirflowNetworkBalanceManager->MA((i - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + i) = 1.0e10;
                state.dataAirflowNetworkBalanceManager->MV(i) = state.dataLoopNodes->Node(j).GenContam * 1.0e10;
            }
            if (j > 0 && OANode) {
                state.dataAirflowNetworkBalanceManager->MA((i - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + i) = 1.0e10;
                state.dataAirflowNetworkBalanceManager->MV(i) = state.dataLoopNodes->Node(j).GenContam * 1.0e10;
            }
            if (state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusZoneNum > 0 &&
                state.dataAirflowNetworkBalanceManager->MA((i - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + i) < 0.9e10) {
                ZoneNum = state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusZoneNum;
                state.dataAirflowNetworkBalanceManager->MA((i - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + i) = 1.0e10;
                state.dataAirflowNetworkBalanceManager->MV(i) = state.dataAirflowNetwork->ANGC(ZoneNum) * 1.0e10;
            }
            if (state.dataAirflowNetwork->AirflowNetworkNodeData(i).ExtNodeNum > 0) {
                state.dataAirflowNetworkBalanceManager->MA((i - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + i) = 1.0e10;
                state.dataAirflowNetworkBalanceManager->MV(i) = state.dataContaminantBalance->OutdoorGC * 1.0e10;
            }
        }

        // Assign node value to distribution nodes with fan off
        for (i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfNodes; ++i) {
            j = state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusNodeNum;
            if (j > 0 && !state.dataAirflowNetworkBalanceManager->LoopOnOffFlag(state.dataAirflowNetwork->AirflowNetworkNodeData(i).AirLoopNum) &&
                state.dataAirflowNetworkBalanceManager->MA((i - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + i) < 1.0e9) {
                state.dataAirflowNetworkBalanceManager->MA((i - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + i) = 1.0e10;
                state.dataAirflowNetworkBalanceManager->MV(i) = state.dataLoopNodes->Node(j).GenContam * 1.0e10;
            }
            if (j == 0 && i > state.dataAirflowNetwork->NumOfNodesMultiZone &&
                !state.dataAirflowNetworkBalanceManager->LoopOnOffFlag(state.dataAirflowNetwork->AirflowNetworkNodeData(i).AirLoopNum)) {
                state.dataAirflowNetworkBalanceManager->MA((i - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + i) = 1.0e10;
                state.dataAirflowNetworkBalanceManager->MV(i) = state.dataAirflowNetwork->AirflowNetworkNodeSimu(i).GCZlast * 1.0e10;
            }
        }

        // Check singularity
        for (i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfNodes; ++i) {
            if (state.dataAirflowNetworkBalanceManager->MA((i - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + i) < 1.0e-6) {
                ShowFatalError(state,
                               "CalcAirflowNetworkGCBalance: A diagonal entity is zero in AirflowNetwork matrix at node " +
                                   state.dataAirflowNetwork->AirflowNetworkNodeData(i).Name);
            }
        }

        // Get an inverse matrix
        MRXINV(state, state.dataAirflowNetwork->AirflowNetworkNumOfNodes);

        // Calculate node temperatures
        for (i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfNodes; ++i) {
            COZN = 0.0;
            for (j = 1; j <= state.dataAirflowNetwork->AirflowNetworkNumOfNodes; ++j) {
                COZN += state.dataAirflowNetworkBalanceManager->MA((i - 1) * state.dataAirflowNetwork->AirflowNetworkNumOfNodes + j) *
                        state.dataAirflowNetworkBalanceManager->MV(j);
            }
            state.dataAirflowNetwork->AirflowNetworkNodeSimu(i).GCZ = COZN;
        }
    }

    void MRXINV(EnergyPlusData &state, int const NORDER)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   Oct. 2005
        //       MODIFIED       na
        //       RE-ENGINEERED  Revised based on Subroutine ADSINV

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine inverses a matrix

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int i;
        int j;
        int K;
        int M;
        Real64 R1;
        Real64 S;

        state.dataAirflowNetworkBalanceManager->IVEC = 0;
        for (i = 1; i <= NORDER; ++i) {
            state.dataAirflowNetworkBalanceManager->IVEC(i + 20) = i;
        }
        for (i = 1; i <= NORDER; ++i) {
            R1 = 0.0;
            M = i;
            for (j = i; j <= NORDER; ++j) {
                if (std::abs(R1) < std::abs(state.dataAirflowNetworkBalanceManager->MA((i - 1) * NORDER + j))) {
                    M = j;
                    R1 = state.dataAirflowNetworkBalanceManager->MA((i - 1) * NORDER + j);
                }
            }
            if (i != M) {
                K = state.dataAirflowNetworkBalanceManager->IVEC(M + 20);
                state.dataAirflowNetworkBalanceManager->IVEC(M + 20) = state.dataAirflowNetworkBalanceManager->IVEC(i + 20);
                state.dataAirflowNetworkBalanceManager->IVEC(i + 20) = K;
                for (j = 1; j <= NORDER; ++j) {
                    S = state.dataAirflowNetworkBalanceManager->MA((j - 1) * NORDER + i);
                    state.dataAirflowNetworkBalanceManager->MA((j - 1) * NORDER + i) =
                        state.dataAirflowNetworkBalanceManager->MA((j - 1) * NORDER + M);
                    state.dataAirflowNetworkBalanceManager->MA((j - 1) * NORDER + M) = S;
                }
            }
            state.dataAirflowNetworkBalanceManager->MA((i - 1) * NORDER + i) = 1.0;
            for (j = 1; j <= NORDER; ++j) {
                state.dataAirflowNetworkBalanceManager->MA((i - 1) * NORDER + j) /= R1;
            }
            for (j = 1; j <= NORDER; ++j) {
                if (i == j) continue;
                R1 = state.dataAirflowNetworkBalanceManager->MA((j - 1) * NORDER + i);
                if (std::abs(R1) <= 1.0E-20) continue;
                state.dataAirflowNetworkBalanceManager->MA((j - 1) * NORDER + i) = 0.0;
                for (K = 1; K <= NORDER; ++K) {
                    state.dataAirflowNetworkBalanceManager->MA((j - 1) * NORDER + K) -=
                        R1 * state.dataAirflowNetworkBalanceManager->MA((i - 1) * NORDER + K);
                }
            }
        }
        for (i = 1; i <= NORDER; ++i) {
            if (state.dataAirflowNetworkBalanceManager->IVEC(i + 20) == i) continue;
            M = i;
            while (NORDER > M) {
                ++M;
                if (state.dataAirflowNetworkBalanceManager->IVEC(M + 20) == i) break;
            }
            state.dataAirflowNetworkBalanceManager->IVEC(M + 20) = state.dataAirflowNetworkBalanceManager->IVEC(i + 20);
            for (j = 1; j <= NORDER; ++j) {
                R1 = state.dataAirflowNetworkBalanceManager->MA((i - 1) * NORDER + j);
                state.dataAirflowNetworkBalanceManager->MA((i - 1) * NORDER + j) = state.dataAirflowNetworkBalanceManager->MA((M - 1) * NORDER + j);
                state.dataAirflowNetworkBalanceManager->MA((M - 1) * NORDER + j) = R1;
            }
            state.dataAirflowNetworkBalanceManager->IVEC(i + 20) = i;
        }
    }

    void ReportAirflowNetwork(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   2/1/04
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine reports outputs of air distribution systems

        // Using/Aliasing
        auto &NumPrimaryAirSys = state.dataHVACGlobal->NumPrimaryAirSys;
        auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;

        auto &Zone(state.dataHeatBal->Zone);

        // SUBROUTINE PARAMETER DEFINITIONS:
        constexpr Real64 Lam(2.5e6); // Heat of vaporization (J/kg)

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int i;
        int n;
        int M;
        int ZN1;
        int ZN2;
        Real64 AirDensity;
        Real64 CpAir;
        Real64 Tamb;
        Real64 hg; // latent heat of vaporization
        Real64 ReportingConstant;
        Real64 ReportingFraction;
        int AirLoopNum;
        int FanNum;
        Real64 RepOnOffFanRunTimeFraction;

        if (state.dataAirflowNetwork->SimulateAirflowNetwork < AirflowNetworkControlMultizone) return;

        if (!state.dataAirflowNetworkBalanceManager->onetime) {
            state.dataAirflowNetworkBalanceManager->onceZoneFlag.dimension(state.dataGlobal->NumOfZones, false);
            state.dataAirflowNetworkBalanceManager->onceSurfFlag.dimension(state.dataAirflowNetwork->AirflowNetworkNumOfLinks, false);
            state.dataAirflowNetworkBalanceManager->onetime = true;
        }
        ReportingConstant = TimeStepSys * DataGlobalConstants::SecInHour;

        state.dataHeatBal->ZoneTotalExfiltrationHeatLoss = 0.0;

        for (auto &e : state.dataAirflowNetwork->AirflowNetworkReportData) {
            e.MultiZoneInfiSenGainW = 0.0;
            e.MultiZoneInfiSenGainJ = 0.0;
            e.MultiZoneInfiSenLossW = 0.0;
            e.MultiZoneInfiSenLossJ = 0.0;
            e.MultiZoneInfiLatGainW = 0.0;
            e.MultiZoneInfiLatGainJ = 0.0;
            e.MultiZoneInfiLatLossW = 0.0;
            e.MultiZoneInfiLatLossJ = 0.0;
            e.MultiZoneVentSenGainW = 0.0;
            e.MultiZoneVentSenGainJ = 0.0;
            e.MultiZoneVentSenLossW = 0.0;
            e.MultiZoneVentSenLossJ = 0.0;
            e.MultiZoneVentLatGainW = 0.0;
            e.MultiZoneVentLatGainJ = 0.0;
            e.MultiZoneVentLatLossW = 0.0;
            e.MultiZoneVentLatLossJ = 0.0;
            e.MultiZoneMixSenGainW = 0.0;
            e.MultiZoneMixSenGainJ = 0.0;
            e.MultiZoneMixSenLossW = 0.0;
            e.MultiZoneMixSenLossJ = 0.0;
            e.MultiZoneMixLatGainW = 0.0;
            e.MultiZoneMixLatGainJ = 0.0;
            e.MultiZoneMixLatLossW = 0.0;
            e.MultiZoneMixLatLossJ = 0.0;
            e.LeakSenGainW = 0.0;
            e.LeakSenGainJ = 0.0;
            e.LeakSenLossW = 0.0;
            e.LeakSenLossJ = 0.0;
            e.LeakLatGainW = 0.0;
            e.LeakLatGainJ = 0.0;
            e.LeakLatLossW = 0.0;
            e.LeakLatLossJ = 0.0;
            e.CondSenGainW = 0.0;
            e.CondSenGainJ = 0.0;
            e.CondSenLossW = 0.0;
            e.CondSenLossJ = 0.0;
            e.DiffLatGainW = 0.0;
            e.DiffLatGainJ = 0.0;
            e.DiffLatLossW = 0.0;
            e.DiffLatLossJ = 0.0;
            e.RadGainW = 0.0;
            e.RadGainJ = 0.0;
            e.RadLossW = 0.0;
            e.RadLossJ = 0.0;
            e.TotalSenGainW = 0.0;
            e.TotalSenGainJ = 0.0;
            e.TotalSenLossW = 0.0;
            e.TotalSenLossJ = 0.0;
            e.TotalLatGainW = 0.0;
            e.TotalLatGainJ = 0.0;
            e.TotalLatLossW = 0.0;
            e.TotalLatLossJ = 0.0;
        }

        // Calculate sensible and latent loads in each zone from multizone airflows
        if (state.dataAirflowNetwork->SimulateAirflowNetwork == AirflowNetworkControlMultizone ||
            state.dataAirflowNetwork->SimulateAirflowNetwork == AirflowNetworkControlMultiADS ||
            (state.dataAirflowNetwork->SimulateAirflowNetwork == AirflowNetworkControlSimpleADS &&
             state.dataAirflowNetwork->AirflowNetworkFanActivated)) {
            for (i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces; ++i) { // Multizone airflow energy
                n = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
                M = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
                ZN1 = state.dataAirflowNetwork->AirflowNetworkNodeData(n).EPlusZoneNum;
                ZN2 = state.dataAirflowNetwork->AirflowNetworkNodeData(M).EPlusZoneNum;
                // Find a linkage from a zone to outdoors
                if (ZN1 > 0 && ZN2 == 0) {
                    if (state.dataSurface->SurfHasLinkedOutAirNode(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum)) {
                        Tamb = state.dataSurface->SurfOutDryBulbTemp(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum);
                        CpAir = PsyCpAirFnW(Psychrometrics::PsyWFnTdbTwbPb(
                            state,
                            Tamb,
                            state.dataSurface->SurfOutWetBulbTemp(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum),
                            state.dataEnvrn->OutBaroPress));
                    } else {
                        Tamb = Zone(ZN1).OutDryBulbTemp;
                        CpAir = PsyCpAirFnW(state.dataEnvrn->OutHumRat);
                    }
                    hg = Psychrometrics::PsyHgAirFnWTdb(state.dataHeatBalFanSys->ZoneAirHumRat(ZN1), state.dataHeatBalFanSys->MAT(ZN1));

                    if (state.dataAirflowNetwork->AirflowNetworkCompData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).CompNum)
                                .CompTypeNum == iComponentTypeNum::SCR ||
                        state.dataAirflowNetwork->AirflowNetworkCompData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).CompNum)
                                .CompTypeNum == iComponentTypeNum::SEL) {
                        if (Tamb > state.dataHeatBalFanSys->MAT(ZN1)) {
                            state.dataAirflowNetwork->AirflowNetworkReportData(ZN1).MultiZoneInfiSenGainW +=
                                (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2 * CpAir * (Tamb - state.dataHeatBalFanSys->MAT(ZN1)));
                            state.dataAirflowNetwork->AirflowNetworkReportData(ZN1).MultiZoneInfiSenGainJ +=
                                (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2 * CpAir * (Tamb - state.dataHeatBalFanSys->MAT(ZN1))) *
                                ReportingConstant;
                        } else {
                            state.dataAirflowNetwork->AirflowNetworkReportData(ZN1).MultiZoneInfiSenLossW +=
                                (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2 * CpAir * (state.dataHeatBalFanSys->MAT(ZN1) - Tamb));
                            state.dataAirflowNetwork->AirflowNetworkReportData(ZN1).MultiZoneInfiSenLossJ +=
                                (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2 * CpAir * (state.dataHeatBalFanSys->MAT(ZN1) - Tamb)) *
                                ReportingConstant;
                        }
                        if (state.dataEnvrn->OutHumRat > state.dataHeatBalFanSys->ZoneAirHumRat(ZN1)) {
                            state.dataAirflowNetwork->AirflowNetworkReportData(ZN1).MultiZoneInfiLatGainW +=
                                (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2 *
                                 (state.dataEnvrn->OutHumRat - state.dataHeatBalFanSys->ZoneAirHumRat(ZN1))) *
                                hg;
                            state.dataAirflowNetwork->AirflowNetworkReportData(ZN1).MultiZoneInfiLatGainJ +=
                                (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2 *
                                 (state.dataEnvrn->OutHumRat - state.dataHeatBalFanSys->ZoneAirHumRat(ZN1))) *
                                hg * ReportingConstant;
                        } else {
                            state.dataAirflowNetwork->AirflowNetworkReportData(ZN1).MultiZoneInfiLatLossW +=
                                (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2 *
                                 (state.dataHeatBalFanSys->ZoneAirHumRat(ZN1) - state.dataEnvrn->OutHumRat)) *
                                hg;
                            state.dataAirflowNetwork->AirflowNetworkReportData(ZN1).MultiZoneInfiLatLossJ +=
                                (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2 *
                                 (state.dataHeatBalFanSys->ZoneAirHumRat(ZN1) - state.dataEnvrn->OutHumRat)) *
                                hg * ReportingConstant;
                        }
                    } else {
                        if (Tamb > state.dataHeatBalFanSys->MAT(ZN1)) {
                            state.dataAirflowNetwork->AirflowNetworkReportData(ZN1).MultiZoneVentSenGainW +=
                                (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2 * CpAir * (Tamb - state.dataHeatBalFanSys->MAT(ZN1)));
                            state.dataAirflowNetwork->AirflowNetworkReportData(ZN1).MultiZoneVentSenGainJ +=
                                (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2 * CpAir * (Tamb - state.dataHeatBalFanSys->MAT(ZN1))) *
                                ReportingConstant;
                        } else {
                            state.dataAirflowNetwork->AirflowNetworkReportData(ZN1).MultiZoneVentSenLossW +=
                                (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2 * CpAir * (state.dataHeatBalFanSys->MAT(ZN1) - Tamb));
                            state.dataAirflowNetwork->AirflowNetworkReportData(ZN1).MultiZoneVentSenLossJ +=
                                (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2 * CpAir * (state.dataHeatBalFanSys->MAT(ZN1) - Tamb)) *
                                ReportingConstant;
                        }
                        if (state.dataEnvrn->OutHumRat > state.dataHeatBalFanSys->ZoneAirHumRat(ZN1)) {
                            state.dataAirflowNetwork->AirflowNetworkReportData(ZN1).MultiZoneVentLatGainW +=
                                (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2 *
                                 (state.dataEnvrn->OutHumRat - state.dataHeatBalFanSys->ZoneAirHumRat(ZN1))) *
                                hg;
                            state.dataAirflowNetwork->AirflowNetworkReportData(ZN1).MultiZoneVentLatGainJ +=
                                (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2 *
                                 (state.dataEnvrn->OutHumRat - state.dataHeatBalFanSys->ZoneAirHumRat(ZN1))) *
                                hg * ReportingConstant;
                        } else {
                            state.dataAirflowNetwork->AirflowNetworkReportData(ZN1).MultiZoneVentLatLossW +=
                                (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2 *
                                 (state.dataHeatBalFanSys->ZoneAirHumRat(ZN1) - state.dataEnvrn->OutHumRat)) *
                                hg;
                            state.dataAirflowNetwork->AirflowNetworkReportData(ZN1).MultiZoneVentLatLossJ +=
                                (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2 *
                                 (state.dataHeatBalFanSys->ZoneAirHumRat(ZN1) - state.dataEnvrn->OutHumRat)) *
                                hg * ReportingConstant;
                        }
                    }
                }
                if (ZN1 == 0 && ZN2 > 0) {
                    if (state.dataSurface->SurfHasLinkedOutAirNode(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum)) {
                        Tamb = state.dataSurface->SurfOutDryBulbTemp(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum);
                        CpAir = PsyCpAirFnW(Psychrometrics::PsyWFnTdbTwbPb(
                            state,
                            Tamb,
                            state.dataSurface->SurfOutWetBulbTemp(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum),
                            state.dataEnvrn->OutBaroPress));
                    } else {
                        Tamb = Zone(ZN2).OutDryBulbTemp;
                        CpAir = PsyCpAirFnW(state.dataEnvrn->OutHumRat);
                    }
                    hg = Psychrometrics::PsyHgAirFnWTdb(state.dataHeatBalFanSys->ZoneAirHumRat(ZN2), state.dataHeatBalFanSys->MAT(ZN2));

                    if (state.dataAirflowNetwork->AirflowNetworkCompData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).CompNum)
                                .CompTypeNum == iComponentTypeNum::SCR ||
                        state.dataAirflowNetwork->AirflowNetworkCompData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).CompNum)
                                .CompTypeNum == iComponentTypeNum::SEL) {
                        if (Tamb > state.dataHeatBalFanSys->MAT(ZN2)) {
                            state.dataAirflowNetwork->AirflowNetworkReportData(ZN2).MultiZoneInfiSenGainW +=
                                (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW * CpAir * (Tamb - state.dataHeatBalFanSys->MAT(ZN2)));
                            state.dataAirflowNetwork->AirflowNetworkReportData(ZN2).MultiZoneInfiSenGainJ +=
                                (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW * CpAir * (Tamb - state.dataHeatBalFanSys->MAT(ZN2))) *
                                ReportingConstant;
                        } else {
                            state.dataAirflowNetwork->AirflowNetworkReportData(ZN2).MultiZoneInfiSenLossW +=
                                (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW * CpAir * (state.dataHeatBalFanSys->MAT(ZN2) - Tamb));
                            state.dataAirflowNetwork->AirflowNetworkReportData(ZN2).MultiZoneInfiSenLossJ +=
                                (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW * CpAir * (state.dataHeatBalFanSys->MAT(ZN2) - Tamb)) *
                                ReportingConstant;
                        }
                        if (state.dataEnvrn->OutHumRat > state.dataHeatBalFanSys->ZoneAirHumRat(ZN2)) {
                            state.dataAirflowNetwork->AirflowNetworkReportData(ZN2).MultiZoneInfiLatGainW +=
                                (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW *
                                 (state.dataEnvrn->OutHumRat - state.dataHeatBalFanSys->ZoneAirHumRat(ZN2))) *
                                hg;
                            state.dataAirflowNetwork->AirflowNetworkReportData(ZN2).MultiZoneInfiLatGainJ +=
                                (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW *
                                 (state.dataEnvrn->OutHumRat - state.dataHeatBalFanSys->ZoneAirHumRat(ZN2))) *
                                hg * ReportingConstant;
                        } else {
                            state.dataAirflowNetwork->AirflowNetworkReportData(ZN2).MultiZoneInfiLatLossW +=
                                (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW *
                                 (state.dataHeatBalFanSys->ZoneAirHumRat(ZN2) - state.dataEnvrn->OutHumRat)) *
                                hg;
                            state.dataAirflowNetwork->AirflowNetworkReportData(ZN2).MultiZoneInfiLatLossJ +=
                                (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW *
                                 (state.dataHeatBalFanSys->ZoneAirHumRat(ZN2) - state.dataEnvrn->OutHumRat)) *
                                hg * ReportingConstant;
                        }
                    } else {
                        if (Tamb > state.dataHeatBalFanSys->MAT(ZN2)) {
                            state.dataAirflowNetwork->AirflowNetworkReportData(ZN2).MultiZoneVentSenGainW +=
                                (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW * CpAir * (Tamb - state.dataHeatBalFanSys->MAT(ZN2)));
                            state.dataAirflowNetwork->AirflowNetworkReportData(ZN2).MultiZoneVentSenGainJ +=
                                (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW * CpAir * (Tamb - state.dataHeatBalFanSys->MAT(ZN2))) *
                                ReportingConstant;
                        } else {
                            state.dataAirflowNetwork->AirflowNetworkReportData(ZN2).MultiZoneVentSenLossW +=
                                (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW * CpAir * (state.dataHeatBalFanSys->MAT(ZN2) - Tamb));
                            state.dataAirflowNetwork->AirflowNetworkReportData(ZN2).MultiZoneVentSenLossJ +=
                                (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW * CpAir * (state.dataHeatBalFanSys->MAT(ZN2) - Tamb)) *
                                ReportingConstant;
                        }
                        if (state.dataEnvrn->OutHumRat > state.dataHeatBalFanSys->ZoneAirHumRat(ZN2)) {
                            state.dataAirflowNetwork->AirflowNetworkReportData(ZN2).MultiZoneVentLatGainW +=
                                (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW *
                                 (state.dataEnvrn->OutHumRat - state.dataHeatBalFanSys->ZoneAirHumRat(ZN2))) *
                                hg;
                            state.dataAirflowNetwork->AirflowNetworkReportData(ZN2).MultiZoneVentLatGainJ +=
                                (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW *
                                 (state.dataEnvrn->OutHumRat - state.dataHeatBalFanSys->ZoneAirHumRat(ZN2))) *
                                hg * ReportingConstant;
                        } else {
                            state.dataAirflowNetwork->AirflowNetworkReportData(ZN2).MultiZoneVentLatLossW +=
                                (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW *
                                 (state.dataHeatBalFanSys->ZoneAirHumRat(ZN2) - state.dataEnvrn->OutHumRat)) *
                                hg;
                            state.dataAirflowNetwork->AirflowNetworkReportData(ZN2).MultiZoneVentLatLossJ +=
                                (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW *
                                 (state.dataHeatBalFanSys->ZoneAirHumRat(ZN2) - state.dataEnvrn->OutHumRat)) *
                                hg * ReportingConstant;
                        }
                    }
                }

                if (ZN1 > 0 && ZN2 > 0) {
                    CpAir = PsyCpAirFnW((state.dataHeatBalFanSys->ZoneAirHumRat(ZN1) + state.dataHeatBalFanSys->ZoneAirHumRat(ZN2)) / 2.0);
                    hg = Psychrometrics::PsyHgAirFnWTdb((state.dataHeatBalFanSys->ZoneAirHumRat(ZN1) + state.dataHeatBalFanSys->ZoneAirHumRat(ZN2)) /
                                                            2.0,
                                                        (state.dataHeatBalFanSys->MAT(ZN1) + state.dataHeatBalFanSys->MAT(ZN2)) / 2.0);
                    if (state.dataHeatBalFanSys->MAT(ZN1) > state.dataHeatBalFanSys->MAT(ZN2)) {
                        state.dataAirflowNetwork->AirflowNetworkReportData(ZN2).MultiZoneMixSenGainW +=
                            (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW * CpAir *
                             (state.dataHeatBalFanSys->MAT(ZN1) - state.dataHeatBalFanSys->MAT(ZN2)));
                        state.dataAirflowNetwork->AirflowNetworkReportData(ZN2).MultiZoneMixSenGainJ +=
                            (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW * CpAir *
                             (state.dataHeatBalFanSys->MAT(ZN1) - state.dataHeatBalFanSys->MAT(ZN2))) *
                            ReportingConstant;
                    } else {
                        state.dataAirflowNetwork->AirflowNetworkReportData(ZN2).MultiZoneMixSenLossW +=
                            (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW * CpAir *
                             (state.dataHeatBalFanSys->MAT(ZN2) - state.dataHeatBalFanSys->MAT(ZN1)));
                        state.dataAirflowNetwork->AirflowNetworkReportData(ZN2).MultiZoneMixSenLossJ +=
                            (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW * CpAir *
                             (state.dataHeatBalFanSys->MAT(ZN2) - state.dataHeatBalFanSys->MAT(ZN1))) *
                            ReportingConstant;
                    }
                    if (state.dataHeatBalFanSys->ZoneAirHumRat(ZN1) > state.dataHeatBalFanSys->ZoneAirHumRat(ZN2)) {
                        state.dataAirflowNetwork->AirflowNetworkReportData(ZN2).MultiZoneMixLatGainW +=
                            (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW *
                             (state.dataHeatBalFanSys->ZoneAirHumRat(ZN1) - state.dataHeatBalFanSys->ZoneAirHumRat(ZN2))) *
                            hg;
                        state.dataAirflowNetwork->AirflowNetworkReportData(ZN2).MultiZoneMixLatGainJ +=
                            (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW *
                             (state.dataHeatBalFanSys->ZoneAirHumRat(ZN1) - state.dataHeatBalFanSys->ZoneAirHumRat(ZN2))) *
                            hg * ReportingConstant;
                    } else {
                        state.dataAirflowNetwork->AirflowNetworkReportData(ZN2).MultiZoneMixLatLossW +=
                            (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW *
                             (state.dataHeatBalFanSys->ZoneAirHumRat(ZN2) - state.dataHeatBalFanSys->ZoneAirHumRat(ZN1))) *
                            hg;
                        state.dataAirflowNetwork->AirflowNetworkReportData(ZN2).MultiZoneMixLatLossJ +=
                            (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW *
                             (state.dataHeatBalFanSys->ZoneAirHumRat(ZN2) - state.dataHeatBalFanSys->ZoneAirHumRat(ZN1))) *
                            hg * ReportingConstant;
                    }
                    if (state.dataHeatBalFanSys->MAT(ZN2) > state.dataHeatBalFanSys->MAT(ZN1)) {
                        state.dataAirflowNetwork->AirflowNetworkReportData(ZN1).MultiZoneMixSenGainW +=
                            (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2 * CpAir *
                             (state.dataHeatBalFanSys->MAT(ZN2) - state.dataHeatBalFanSys->MAT(ZN1)));
                        state.dataAirflowNetwork->AirflowNetworkReportData(ZN1).MultiZoneMixSenGainJ +=
                            (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2 * CpAir *
                             (state.dataHeatBalFanSys->MAT(ZN2) - state.dataHeatBalFanSys->MAT(ZN1))) *
                            ReportingConstant;
                    } else {
                        state.dataAirflowNetwork->AirflowNetworkReportData(ZN1).MultiZoneMixSenLossW +=
                            (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2 * CpAir *
                             (state.dataHeatBalFanSys->MAT(ZN1) - state.dataHeatBalFanSys->MAT(ZN2)));
                        state.dataAirflowNetwork->AirflowNetworkReportData(ZN1).MultiZoneMixSenLossJ +=
                            (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2 * CpAir *
                             (state.dataHeatBalFanSys->MAT(ZN1) - state.dataHeatBalFanSys->MAT(ZN2))) *
                            ReportingConstant;
                    }
                    if (state.dataHeatBalFanSys->ZoneAirHumRat(ZN2) > state.dataHeatBalFanSys->ZoneAirHumRat(ZN1)) {
                        state.dataAirflowNetwork->AirflowNetworkReportData(ZN1).MultiZoneMixLatGainW +=
                            (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2 *
                             (state.dataHeatBalFanSys->ZoneAirHumRat(ZN2) - state.dataHeatBalFanSys->ZoneAirHumRat(ZN1))) *
                            hg;
                        state.dataAirflowNetwork->AirflowNetworkReportData(ZN1).MultiZoneMixLatGainJ +=
                            (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2 *
                             (state.dataHeatBalFanSys->ZoneAirHumRat(ZN2) - state.dataHeatBalFanSys->ZoneAirHumRat(ZN1))) *
                            hg * ReportingConstant;
                    } else {
                        state.dataAirflowNetwork->AirflowNetworkReportData(ZN1).MultiZoneMixLatLossW +=
                            std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2 *
                                     (state.dataHeatBalFanSys->ZoneAirHumRat(ZN1) - state.dataHeatBalFanSys->ZoneAirHumRat(ZN2))) *
                            hg;
                        state.dataAirflowNetwork->AirflowNetworkReportData(ZN1).MultiZoneMixLatLossJ +=
                            (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2 *
                             (state.dataHeatBalFanSys->ZoneAirHumRat(ZN1) - state.dataHeatBalFanSys->ZoneAirHumRat(ZN2))) *
                            hg * ReportingConstant;
                    }
                }
            }
        }

        // Assign data for report
        if (state.dataAirflowNetwork->SimulateAirflowNetwork > AirflowNetworkControlMultizone) {
            for (i = 1; i <= state.dataGlobal->NumOfZones; ++i) {
                if (state.dataAirflowNetworkBalanceManager->exchangeData(i).LeakSen > 0.0) {
                    state.dataAirflowNetwork->AirflowNetworkReportData(i).LeakSenGainW =
                        state.dataAirflowNetworkBalanceManager->exchangeData(i).LeakSen;
                    state.dataAirflowNetwork->AirflowNetworkReportData(i).LeakSenGainJ =
                        state.dataAirflowNetworkBalanceManager->exchangeData(i).LeakSen * ReportingConstant;
                } else {
                    state.dataAirflowNetwork->AirflowNetworkReportData(i).LeakSenLossW =
                        -state.dataAirflowNetworkBalanceManager->exchangeData(i).LeakSen;
                    state.dataAirflowNetwork->AirflowNetworkReportData(i).LeakSenLossJ =
                        -state.dataAirflowNetworkBalanceManager->exchangeData(i).LeakSen * ReportingConstant;
                }
                if (state.dataAirflowNetworkBalanceManager->exchangeData(i).LeakLat > 0.0) {
                    state.dataAirflowNetwork->AirflowNetworkReportData(i).LeakLatGainW =
                        state.dataAirflowNetworkBalanceManager->exchangeData(i).LeakLat * Lam;
                    state.dataAirflowNetwork->AirflowNetworkReportData(i).LeakLatGainJ =
                        state.dataAirflowNetworkBalanceManager->exchangeData(i).LeakLat * Lam * ReportingConstant;
                } else {
                    state.dataAirflowNetwork->AirflowNetworkReportData(i).LeakLatLossW =
                        -state.dataAirflowNetworkBalanceManager->exchangeData(i).LeakLat * Lam;
                    state.dataAirflowNetwork->AirflowNetworkReportData(i).LeakLatLossJ =
                        -state.dataAirflowNetworkBalanceManager->exchangeData(i).LeakLat * Lam * ReportingConstant;
                }
                if (state.dataAirflowNetworkBalanceManager->exchangeData(i).CondSen > 0.0) {
                    state.dataAirflowNetwork->AirflowNetworkReportData(i).CondSenGainW =
                        state.dataAirflowNetworkBalanceManager->exchangeData(i).CondSen;
                    state.dataAirflowNetwork->AirflowNetworkReportData(i).CondSenGainJ =
                        state.dataAirflowNetworkBalanceManager->exchangeData(i).CondSen * ReportingConstant;
                } else {
                    state.dataAirflowNetwork->AirflowNetworkReportData(i).CondSenLossW =
                        -state.dataAirflowNetworkBalanceManager->exchangeData(i).CondSen;
                    state.dataAirflowNetwork->AirflowNetworkReportData(i).CondSenLossJ =
                        -state.dataAirflowNetworkBalanceManager->exchangeData(i).CondSen * ReportingConstant;
                }
                if (state.dataAirflowNetworkBalanceManager->exchangeData(i).DiffLat > 0.0) {
                    state.dataAirflowNetwork->AirflowNetworkReportData(i).DiffLatGainW =
                        state.dataAirflowNetworkBalanceManager->exchangeData(i).DiffLat * Lam;
                    state.dataAirflowNetwork->AirflowNetworkReportData(i).DiffLatGainJ =
                        state.dataAirflowNetworkBalanceManager->exchangeData(i).DiffLat * Lam * ReportingConstant;
                } else {
                    state.dataAirflowNetwork->AirflowNetworkReportData(i).DiffLatLossW =
                        -state.dataAirflowNetworkBalanceManager->exchangeData(i).DiffLat * Lam;
                    state.dataAirflowNetwork->AirflowNetworkReportData(i).DiffLatLossJ =
                        -state.dataAirflowNetworkBalanceManager->exchangeData(i).DiffLat * Lam * ReportingConstant;
                }
                if (state.dataAirflowNetworkBalanceManager->exchangeData(i).RadGain < 0.0) {
                    state.dataAirflowNetwork->AirflowNetworkReportData(i).RadGainW = -state.dataAirflowNetworkBalanceManager->exchangeData(i).RadGain;
                    state.dataAirflowNetwork->AirflowNetworkReportData(i).RadGainJ =
                        -state.dataAirflowNetworkBalanceManager->exchangeData(i).RadGain * ReportingConstant;
                } else {
                    state.dataAirflowNetwork->AirflowNetworkReportData(i).RadLossW = state.dataAirflowNetworkBalanceManager->exchangeData(i).RadGain;
                    state.dataAirflowNetwork->AirflowNetworkReportData(i).RadLossJ =
                        state.dataAirflowNetworkBalanceManager->exchangeData(i).RadGain * ReportingConstant;
                }
                if (state.dataAirflowNetworkBalanceManager->exchangeData(i).TotalSen > 0.0) {
                    state.dataAirflowNetwork->AirflowNetworkReportData(i).TotalSenGainW =
                        state.dataAirflowNetworkBalanceManager->exchangeData(i).TotalSen;
                    state.dataAirflowNetwork->AirflowNetworkReportData(i).TotalSenGainJ =
                        state.dataAirflowNetworkBalanceManager->exchangeData(i).TotalSen * ReportingConstant;
                } else {
                    state.dataAirflowNetwork->AirflowNetworkReportData(i).TotalSenLossW =
                        -state.dataAirflowNetworkBalanceManager->exchangeData(i).TotalSen;
                    state.dataAirflowNetwork->AirflowNetworkReportData(i).TotalSenLossJ =
                        -state.dataAirflowNetworkBalanceManager->exchangeData(i).TotalSen * ReportingConstant;
                }
                if (state.dataAirflowNetworkBalanceManager->exchangeData(i).TotalLat > 0.0) {
                    state.dataAirflowNetwork->AirflowNetworkReportData(i).TotalLatGainW =
                        state.dataAirflowNetworkBalanceManager->exchangeData(i).TotalLat * Lam;
                    state.dataAirflowNetwork->AirflowNetworkReportData(i).TotalLatGainJ =
                        state.dataAirflowNetworkBalanceManager->exchangeData(i).TotalLat * Lam * ReportingConstant;
                } else {
                    state.dataAirflowNetwork->AirflowNetworkReportData(i).TotalLatLossW =
                        -state.dataAirflowNetworkBalanceManager->exchangeData(i).TotalLat * Lam;
                    state.dataAirflowNetwork->AirflowNetworkReportData(i).TotalLatLossJ =
                        -state.dataAirflowNetworkBalanceManager->exchangeData(i).TotalLat * Lam * ReportingConstant;
                }
            }
        }

        // Zone report

        for (auto &e : state.dataAirflowNetworkBalanceManager->AirflowNetworkZnRpt) {
            e.InfilVolume = 0.0;
            e.InfilMass = 0.0;
            e.InfilAirChangeRate = 0.0;
            e.VentilVolume = 0.0;
            e.VentilMass = 0.0;
            e.VentilAirChangeRate = 0.0;
            e.MixVolume = 0.0;
            e.MixMass = 0.0;
        }

        for (AirLoopNum = 1; AirLoopNum <= NumPrimaryAirSys; ++AirLoopNum) {
            if (state.dataAirflowNetworkBalanceManager->DisSysNumOfCVFs == 0) continue;
            for (FanNum = 1; FanNum <= state.dataAirflowNetworkBalanceManager->DisSysNumOfCVFs; ++FanNum) {
                if (state.dataAirflowNetwork->DisSysCompCVFData(FanNum).AirLoopNum == AirLoopNum) break;
            }
            if (state.dataAirflowNetwork->DisSysCompCVFData(FanNum).FanTypeNum == FanType_SimpleOnOff &&
                state.dataAirflowNetworkBalanceManager->LoopOnOffFanRunTimeFraction(AirLoopNum) < 1.0 &&
                state.dataAirflowNetworkBalanceManager->LoopOnOffFanRunTimeFraction(AirLoopNum) > 0.0) {
                // ON Cycle calculation
                state.dataAirflowNetworkBalanceManager->onceZoneFlag = false;
                for (i = 1; i <= state.dataGlobal->NumOfZones; ++i) {
                    if (state.dataAirflowNetwork->AirflowNetworkNodeData(i).AirLoopNum > 0 &&
                        state.dataAirflowNetwork->AirflowNetworkNodeData(i).AirLoopNum != AirLoopNum)
                        continue;
                    if (state.dataAirflowNetwork->AirflowNetworkNodeData(i).AirLoopNum == AirLoopNum) {
                        RepOnOffFanRunTimeFraction = state.dataAirflowNetworkBalanceManager->LoopOnOffFanRunTimeFraction(AirLoopNum);
                    }
                    if (state.dataAirflowNetwork->AirflowNetworkNodeData(i).AirLoopNum == 0) {
                        RepOnOffFanRunTimeFraction = state.dataAirflowNetworkBalanceManager->MaxOnOffFanRunTimeFraction;
                    }
                    if (state.dataAirflowNetwork->AirflowNetworkNodeData(i).AirLoopNum == 0 &&
                        state.dataAirflowNetworkBalanceManager->onceZoneFlag(i))
                        continue;
                    state.dataAirflowNetwork->AirflowNetworkReportData(i).MultiZoneInfiSenGainW *= RepOnOffFanRunTimeFraction;
                    state.dataAirflowNetwork->AirflowNetworkReportData(i).MultiZoneInfiSenGainJ *= RepOnOffFanRunTimeFraction;
                    state.dataAirflowNetwork->AirflowNetworkReportData(i).MultiZoneInfiSenLossW *= RepOnOffFanRunTimeFraction;
                    state.dataAirflowNetwork->AirflowNetworkReportData(i).MultiZoneInfiSenLossJ *= RepOnOffFanRunTimeFraction;
                    state.dataAirflowNetwork->AirflowNetworkReportData(i).MultiZoneInfiLatGainW *= RepOnOffFanRunTimeFraction;
                    state.dataAirflowNetwork->AirflowNetworkReportData(i).MultiZoneInfiLatGainJ *= RepOnOffFanRunTimeFraction;
                    state.dataAirflowNetwork->AirflowNetworkReportData(i).MultiZoneInfiLatLossW *= RepOnOffFanRunTimeFraction;
                    state.dataAirflowNetwork->AirflowNetworkReportData(i).MultiZoneInfiLatLossJ *= RepOnOffFanRunTimeFraction;
                    state.dataAirflowNetwork->AirflowNetworkReportData(i).MultiZoneVentSenGainW *= RepOnOffFanRunTimeFraction;
                    state.dataAirflowNetwork->AirflowNetworkReportData(i).MultiZoneVentSenGainJ *= RepOnOffFanRunTimeFraction;
                    state.dataAirflowNetwork->AirflowNetworkReportData(i).MultiZoneVentSenLossW *= RepOnOffFanRunTimeFraction;
                    state.dataAirflowNetwork->AirflowNetworkReportData(i).MultiZoneVentSenLossJ *= RepOnOffFanRunTimeFraction;
                    state.dataAirflowNetwork->AirflowNetworkReportData(i).MultiZoneVentLatGainW *= RepOnOffFanRunTimeFraction;
                    state.dataAirflowNetwork->AirflowNetworkReportData(i).MultiZoneVentLatGainJ *= RepOnOffFanRunTimeFraction;
                    state.dataAirflowNetwork->AirflowNetworkReportData(i).MultiZoneVentLatLossW *= RepOnOffFanRunTimeFraction;
                    state.dataAirflowNetwork->AirflowNetworkReportData(i).MultiZoneVentLatLossJ *= RepOnOffFanRunTimeFraction;
                    state.dataAirflowNetwork->AirflowNetworkReportData(i).MultiZoneMixSenGainW *= RepOnOffFanRunTimeFraction;
                    state.dataAirflowNetwork->AirflowNetworkReportData(i).MultiZoneMixSenGainJ *= RepOnOffFanRunTimeFraction;
                    state.dataAirflowNetwork->AirflowNetworkReportData(i).MultiZoneMixSenLossW *= RepOnOffFanRunTimeFraction;
                    state.dataAirflowNetwork->AirflowNetworkReportData(i).MultiZoneMixSenLossJ *= RepOnOffFanRunTimeFraction;
                    state.dataAirflowNetwork->AirflowNetworkReportData(i).MultiZoneMixLatGainW *= RepOnOffFanRunTimeFraction;
                    state.dataAirflowNetwork->AirflowNetworkReportData(i).MultiZoneMixLatGainJ *= RepOnOffFanRunTimeFraction;
                    state.dataAirflowNetwork->AirflowNetworkReportData(i).MultiZoneMixLatLossW *= RepOnOffFanRunTimeFraction;
                    state.dataAirflowNetwork->AirflowNetworkReportData(i).MultiZoneMixLatLossJ *= RepOnOffFanRunTimeFraction;
                    if (state.dataAirflowNetwork->AirflowNetworkNodeData(i).AirLoopNum == 0) {
                        state.dataAirflowNetworkBalanceManager->onceZoneFlag(i) = true;
                    }
                }
                // Off Cycle addon
                state.dataAirflowNetworkBalanceManager->onceSurfFlag = false;
                for (i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces; ++i) { // Multizone airflow energy
                    n = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
                    M = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
                    ZN1 = state.dataAirflowNetwork->AirflowNetworkNodeData(n).EPlusZoneNum;
                    ZN2 = state.dataAirflowNetwork->AirflowNetworkNodeData(M).EPlusZoneNum;
                    // Find a linkage from a zone to outdoors
                    if (ZN1 > 0 && ZN2 == 0) {
                        if (state.dataAirflowNetwork->AirflowNetworkNodeData(n).AirLoopNum > 0 &&
                            state.dataAirflowNetwork->AirflowNetworkNodeData(n).AirLoopNum != AirLoopNum)
                            continue;
                        if (state.dataAirflowNetwork->AirflowNetworkNodeData(n).AirLoopNum == AirLoopNum) {
                            RepOnOffFanRunTimeFraction = state.dataAirflowNetworkBalanceManager->LoopOnOffFanRunTimeFraction(AirLoopNum);
                        }
                        if (state.dataAirflowNetwork->AirflowNetworkNodeData(n).AirLoopNum == 0) {
                            RepOnOffFanRunTimeFraction = state.dataAirflowNetworkBalanceManager->MaxOnOffFanRunTimeFraction;
                        }
                        if (state.dataAirflowNetwork->AirflowNetworkNodeData(n).AirLoopNum == 0 &&
                            state.dataAirflowNetworkBalanceManager->onceSurfFlag(i))
                            continue;
                        ReportingFraction = (1.0 - RepOnOffFanRunTimeFraction);
                        Tamb = Zone(ZN1).OutDryBulbTemp;
                        CpAir = PsyCpAirFnW(state.dataEnvrn->OutHumRat);
                        if (state.dataAirflowNetwork->AirflowNetworkCompData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).CompNum)
                                    .CompTypeNum == iComponentTypeNum::SCR ||
                            state.dataAirflowNetwork->AirflowNetworkCompData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).CompNum)
                                    .CompTypeNum == iComponentTypeNum::SEL) {
                            if (Tamb > state.dataHeatBalFanSys->MAT(ZN1)) {
                                state.dataAirflowNetwork->AirflowNetworkReportData(ZN1).MultiZoneInfiSenGainW +=
                                    (state.dataAirflowNetworkBalanceManager->linkReport1(i).FLOW2OFF * CpAir *
                                     (Tamb - state.dataHeatBalFanSys->MAT(ZN1))) *
                                    (1.0 - RepOnOffFanRunTimeFraction);
                                state.dataAirflowNetwork->AirflowNetworkReportData(ZN1).MultiZoneInfiSenGainJ +=
                                    (state.dataAirflowNetworkBalanceManager->linkReport1(i).FLOW2OFF * CpAir *
                                     (Tamb - state.dataHeatBalFanSys->MAT(ZN1))) *
                                    ReportingConstant * ReportingFraction;
                            } else {
                                state.dataAirflowNetwork->AirflowNetworkReportData(ZN1).MultiZoneInfiSenLossW +=
                                    (state.dataAirflowNetworkBalanceManager->linkReport1(i).FLOW2OFF * CpAir *
                                     (state.dataHeatBalFanSys->MAT(ZN1) - Tamb)) *
                                    (1.0 - RepOnOffFanRunTimeFraction);
                                state.dataAirflowNetwork->AirflowNetworkReportData(ZN1).MultiZoneInfiSenLossJ +=
                                    (state.dataAirflowNetworkBalanceManager->linkReport1(i).FLOW2OFF * CpAir *
                                     (state.dataHeatBalFanSys->MAT(ZN1) - Tamb)) *
                                    ReportingConstant * ReportingFraction;
                            }
                            if (state.dataEnvrn->OutHumRat > state.dataHeatBalFanSys->ZoneAirHumRat(ZN1)) {
                                state.dataAirflowNetwork->AirflowNetworkReportData(ZN1).MultiZoneInfiLatGainW +=
                                    (state.dataAirflowNetworkBalanceManager->linkReport1(i).FLOW2OFF *
                                     (state.dataEnvrn->OutHumRat - state.dataHeatBalFanSys->ZoneAirHumRat(ZN1))) *
                                    ReportingFraction;
                                state.dataAirflowNetwork->AirflowNetworkReportData(ZN1).MultiZoneInfiLatGainJ +=
                                    (state.dataAirflowNetworkBalanceManager->linkReport1(i).FLOW2OFF *
                                     (state.dataEnvrn->OutHumRat - state.dataHeatBalFanSys->ZoneAirHumRat(ZN1))) *
                                    ReportingConstant * ReportingFraction;
                            } else {
                                state.dataAirflowNetwork->AirflowNetworkReportData(ZN1).MultiZoneInfiLatLossW +=
                                    (state.dataAirflowNetworkBalanceManager->linkReport1(i).FLOW2OFF *
                                     (state.dataHeatBalFanSys->ZoneAirHumRat(ZN1) - state.dataEnvrn->OutHumRat)) *
                                    ReportingFraction;
                                state.dataAirflowNetwork->AirflowNetworkReportData(ZN1).MultiZoneInfiLatLossJ +=
                                    (state.dataAirflowNetworkBalanceManager->linkReport1(i).FLOW2OFF *
                                     (state.dataHeatBalFanSys->ZoneAirHumRat(ZN1) - state.dataEnvrn->OutHumRat)) *
                                    ReportingConstant * ReportingFraction;
                            }
                        } else {
                            if (Tamb > state.dataHeatBalFanSys->MAT(ZN1)) {
                                state.dataAirflowNetwork->AirflowNetworkReportData(ZN1).MultiZoneVentSenGainW +=
                                    (state.dataAirflowNetworkBalanceManager->linkReport1(i).FLOW2OFF * CpAir *
                                     (Tamb - state.dataHeatBalFanSys->MAT(ZN1))) *
                                    (1.0 - RepOnOffFanRunTimeFraction);
                                state.dataAirflowNetwork->AirflowNetworkReportData(ZN1).MultiZoneVentSenGainJ +=
                                    (state.dataAirflowNetworkBalanceManager->linkReport1(i).FLOW2OFF * CpAir *
                                     (Tamb - state.dataHeatBalFanSys->MAT(ZN1))) *
                                    ReportingConstant * ReportingFraction;
                            } else {
                                state.dataAirflowNetwork->AirflowNetworkReportData(ZN1).MultiZoneVentSenLossW +=
                                    (state.dataAirflowNetworkBalanceManager->linkReport1(i).FLOW2OFF * CpAir *
                                     (state.dataHeatBalFanSys->MAT(ZN1) - Tamb)) *
                                    (1.0 - RepOnOffFanRunTimeFraction);
                                state.dataAirflowNetwork->AirflowNetworkReportData(ZN1).MultiZoneVentSenLossJ +=
                                    (state.dataAirflowNetworkBalanceManager->linkReport1(i).FLOW2OFF * CpAir *
                                     (state.dataHeatBalFanSys->MAT(ZN1) - Tamb)) *
                                    ReportingConstant * ReportingFraction;
                            }
                            if (state.dataEnvrn->OutHumRat > state.dataHeatBalFanSys->ZoneAirHumRat(ZN1)) {
                                state.dataAirflowNetwork->AirflowNetworkReportData(ZN1).MultiZoneVentLatGainW +=
                                    (state.dataAirflowNetworkBalanceManager->linkReport1(i).FLOW2OFF *
                                     (state.dataEnvrn->OutHumRat - state.dataHeatBalFanSys->ZoneAirHumRat(ZN1))) *
                                    ReportingFraction;
                                state.dataAirflowNetwork->AirflowNetworkReportData(ZN1).MultiZoneVentLatGainJ +=
                                    (state.dataAirflowNetworkBalanceManager->linkReport1(i).FLOW2OFF *
                                     (state.dataEnvrn->OutHumRat - state.dataHeatBalFanSys->ZoneAirHumRat(ZN1))) *
                                    ReportingConstant * ReportingFraction;
                            } else {
                                state.dataAirflowNetwork->AirflowNetworkReportData(ZN1).MultiZoneVentLatLossW +=
                                    (state.dataAirflowNetworkBalanceManager->linkReport1(i).FLOW2OFF *
                                     (state.dataHeatBalFanSys->ZoneAirHumRat(ZN1) - state.dataEnvrn->OutHumRat)) *
                                    ReportingFraction;
                                state.dataAirflowNetwork->AirflowNetworkReportData(ZN1).MultiZoneVentLatLossJ +=
                                    (state.dataAirflowNetworkBalanceManager->linkReport1(i).FLOW2OFF *
                                     (state.dataHeatBalFanSys->ZoneAirHumRat(ZN1) - state.dataEnvrn->OutHumRat)) *
                                    ReportingConstant * ReportingFraction;
                            }
                        }
                        if (state.dataAirflowNetwork->AirflowNetworkNodeData(n).AirLoopNum == 0) {
                            state.dataAirflowNetworkBalanceManager->onceSurfFlag(i) = true;
                        }
                    }
                    if (ZN1 == 0 && ZN2 > 0) {
                        if (state.dataAirflowNetwork->AirflowNetworkNodeData(M).AirLoopNum > 0 &&
                            state.dataAirflowNetwork->AirflowNetworkNodeData(M).AirLoopNum != AirLoopNum)
                            continue;
                        if (state.dataAirflowNetwork->AirflowNetworkNodeData(M).AirLoopNum == AirLoopNum) {
                            RepOnOffFanRunTimeFraction = state.dataAirflowNetworkBalanceManager->LoopOnOffFanRunTimeFraction(AirLoopNum);
                        }
                        if (state.dataAirflowNetwork->AirflowNetworkNodeData(M).AirLoopNum == 0) {
                            RepOnOffFanRunTimeFraction = state.dataAirflowNetworkBalanceManager->MaxOnOffFanRunTimeFraction;
                        }
                        if (state.dataAirflowNetwork->AirflowNetworkNodeData(M).AirLoopNum == 0 &&
                            state.dataAirflowNetworkBalanceManager->onceSurfFlag(i))
                            continue;
                        ReportingFraction = (1.0 - RepOnOffFanRunTimeFraction);
                        Tamb = Zone(ZN2).OutDryBulbTemp;
                        CpAir = PsyCpAirFnW(state.dataEnvrn->OutHumRat);
                        if (state.dataAirflowNetwork->AirflowNetworkCompData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).CompNum)
                                    .CompTypeNum == iComponentTypeNum::SCR ||
                            state.dataAirflowNetwork->AirflowNetworkCompData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).CompNum)
                                    .CompTypeNum == iComponentTypeNum::SEL) {
                            if (Tamb > state.dataHeatBalFanSys->MAT(ZN2)) {
                                state.dataAirflowNetwork->AirflowNetworkReportData(ZN2).MultiZoneInfiSenGainW +=
                                    (state.dataAirflowNetworkBalanceManager->linkReport1(i).FLOWOFF * CpAir *
                                     (Tamb - state.dataHeatBalFanSys->MAT(ZN2))) *
                                    ReportingFraction;
                                state.dataAirflowNetwork->AirflowNetworkReportData(ZN2).MultiZoneInfiSenGainJ +=
                                    (state.dataAirflowNetworkBalanceManager->linkReport1(i).FLOWOFF * CpAir *
                                     (Tamb - state.dataHeatBalFanSys->MAT(ZN2))) *
                                    ReportingConstant * ReportingFraction;
                            } else {
                                state.dataAirflowNetwork->AirflowNetworkReportData(ZN2).MultiZoneInfiSenLossW +=
                                    (state.dataAirflowNetworkBalanceManager->linkReport1(i).FLOWOFF * CpAir *
                                     (state.dataHeatBalFanSys->MAT(ZN2) - Tamb)) *
                                    ReportingFraction;
                                state.dataAirflowNetwork->AirflowNetworkReportData(ZN2).MultiZoneInfiSenLossJ +=
                                    (state.dataAirflowNetworkBalanceManager->linkReport1(i).FLOWOFF * CpAir *
                                     (state.dataHeatBalFanSys->MAT(ZN2) - Tamb)) *
                                    ReportingConstant * ReportingFraction;
                            }
                            if (state.dataEnvrn->OutHumRat > state.dataHeatBalFanSys->ZoneAirHumRat(ZN2)) {
                                state.dataAirflowNetwork->AirflowNetworkReportData(ZN2).MultiZoneInfiLatGainW +=
                                    (state.dataAirflowNetworkBalanceManager->linkReport1(i).FLOWOFF *
                                     (state.dataEnvrn->OutHumRat - state.dataHeatBalFanSys->ZoneAirHumRat(ZN2))) *
                                    ReportingFraction;
                                state.dataAirflowNetwork->AirflowNetworkReportData(ZN2).MultiZoneInfiLatGainJ +=
                                    (state.dataAirflowNetworkBalanceManager->linkReport1(i).FLOWOFF *
                                     (state.dataEnvrn->OutHumRat - state.dataHeatBalFanSys->ZoneAirHumRat(ZN2))) *
                                    ReportingConstant * ReportingFraction;
                            } else {
                                state.dataAirflowNetwork->AirflowNetworkReportData(ZN2).MultiZoneInfiLatLossW +=
                                    (state.dataAirflowNetworkBalanceManager->linkReport1(i).FLOWOFF *
                                     (state.dataHeatBalFanSys->ZoneAirHumRat(ZN2) - state.dataEnvrn->OutHumRat)) *
                                    ReportingFraction;
                                state.dataAirflowNetwork->AirflowNetworkReportData(ZN2).MultiZoneInfiLatLossJ +=
                                    (state.dataAirflowNetworkBalanceManager->linkReport1(i).FLOWOFF *
                                     (state.dataHeatBalFanSys->ZoneAirHumRat(ZN2) - state.dataEnvrn->OutHumRat)) *
                                    ReportingConstant * ReportingFraction;
                            }
                        } else {
                            if (Tamb > state.dataHeatBalFanSys->MAT(ZN2)) {
                                state.dataAirflowNetwork->AirflowNetworkReportData(ZN2).MultiZoneVentSenGainW +=
                                    (state.dataAirflowNetworkBalanceManager->linkReport1(i).FLOWOFF * CpAir *
                                     (Tamb - state.dataHeatBalFanSys->MAT(ZN2))) *
                                    ReportingFraction;
                                state.dataAirflowNetwork->AirflowNetworkReportData(ZN2).MultiZoneVentSenGainJ +=
                                    (state.dataAirflowNetworkBalanceManager->linkReport1(i).FLOWOFF * CpAir *
                                     (Tamb - state.dataHeatBalFanSys->MAT(ZN2))) *
                                    ReportingConstant * ReportingFraction;
                            } else {
                                state.dataAirflowNetwork->AirflowNetworkReportData(ZN2).MultiZoneVentSenLossW +=
                                    (state.dataAirflowNetworkBalanceManager->linkReport1(i).FLOWOFF * CpAir *
                                     (state.dataHeatBalFanSys->MAT(ZN2) - Tamb)) *
                                    ReportingFraction;
                                state.dataAirflowNetwork->AirflowNetworkReportData(ZN2).MultiZoneVentSenLossJ +=
                                    (state.dataAirflowNetworkBalanceManager->linkReport1(i).FLOWOFF * CpAir *
                                     (state.dataHeatBalFanSys->MAT(ZN2) - Tamb)) *
                                    ReportingConstant * ReportingFraction;
                            }
                            if (state.dataEnvrn->OutHumRat > state.dataHeatBalFanSys->ZoneAirHumRat(ZN2)) {
                                state.dataAirflowNetwork->AirflowNetworkReportData(ZN2).MultiZoneVentLatGainW +=
                                    (state.dataAirflowNetworkBalanceManager->linkReport1(i).FLOWOFF *
                                     (state.dataEnvrn->OutHumRat - state.dataHeatBalFanSys->ZoneAirHumRat(ZN2))) *
                                    ReportingFraction;
                                state.dataAirflowNetwork->AirflowNetworkReportData(ZN2).MultiZoneVentLatGainJ +=
                                    (state.dataAirflowNetworkBalanceManager->linkReport1(i).FLOWOFF *
                                     (state.dataEnvrn->OutHumRat - state.dataHeatBalFanSys->ZoneAirHumRat(ZN2))) *
                                    ReportingConstant * ReportingFraction;
                            } else {
                                state.dataAirflowNetwork->AirflowNetworkReportData(ZN2).MultiZoneVentLatLossW +=
                                    (state.dataAirflowNetworkBalanceManager->linkReport1(i).FLOWOFF *
                                     (state.dataHeatBalFanSys->ZoneAirHumRat(ZN2) - state.dataEnvrn->OutHumRat)) *
                                    ReportingFraction;
                                state.dataAirflowNetwork->AirflowNetworkReportData(ZN2).MultiZoneVentLatLossJ +=
                                    (state.dataAirflowNetworkBalanceManager->linkReport1(i).FLOWOFF *
                                     (state.dataHeatBalFanSys->ZoneAirHumRat(ZN2) - state.dataEnvrn->OutHumRat)) *
                                    ReportingConstant * ReportingFraction;
                            }
                        }
                        if (state.dataAirflowNetwork->AirflowNetworkNodeData(M).AirLoopNum == 0) {
                            state.dataAirflowNetworkBalanceManager->onceSurfFlag(i) = true;
                        }
                    }

                    if (ZN1 > 0 && ZN2 > 0) {
                        ReportingFraction = (1.0 - state.dataAirflowNetworkBalanceManager->MaxOnOffFanRunTimeFraction);
                        CpAir = PsyCpAirFnW(state.dataHeatBalFanSys->ZoneAirHumRat(ZN1));
                        if (state.dataHeatBalFanSys->MAT(ZN1) > state.dataHeatBalFanSys->MAT(ZN2)) {
                            state.dataAirflowNetwork->AirflowNetworkReportData(ZN2).MultiZoneMixSenGainW +=
                                (state.dataAirflowNetworkBalanceManager->linkReport1(i).FLOWOFF * CpAir *
                                 (state.dataHeatBalFanSys->MAT(ZN1) - state.dataHeatBalFanSys->MAT(ZN2))) *
                                ReportingFraction;
                            state.dataAirflowNetwork->AirflowNetworkReportData(ZN2).MultiZoneMixSenGainJ +=
                                (state.dataAirflowNetworkBalanceManager->linkReport1(i).FLOWOFF * CpAir *
                                 (state.dataHeatBalFanSys->MAT(ZN1) - state.dataHeatBalFanSys->MAT(ZN2))) *
                                ReportingConstant * ReportingFraction;
                        } else {
                            state.dataAirflowNetwork->AirflowNetworkReportData(ZN2).MultiZoneMixSenLossW +=
                                (state.dataAirflowNetworkBalanceManager->linkReport1(i).FLOWOFF * CpAir *
                                 (state.dataHeatBalFanSys->MAT(ZN2) - state.dataHeatBalFanSys->MAT(ZN1))) *
                                ReportingFraction;
                            state.dataAirflowNetwork->AirflowNetworkReportData(ZN2).MultiZoneMixSenLossJ +=
                                (state.dataAirflowNetworkBalanceManager->linkReport1(i).FLOWOFF * CpAir *
                                 (state.dataHeatBalFanSys->MAT(ZN2) - state.dataHeatBalFanSys->MAT(ZN1))) *
                                ReportingConstant * ReportingFraction;
                        }
                        if (state.dataHeatBalFanSys->ZoneAirHumRat(ZN1) > state.dataHeatBalFanSys->ZoneAirHumRat(ZN2)) {
                            state.dataAirflowNetwork->AirflowNetworkReportData(ZN2).MultiZoneMixLatGainW +=
                                (state.dataAirflowNetworkBalanceManager->linkReport1(i).FLOWOFF *
                                 (state.dataHeatBalFanSys->ZoneAirHumRat(ZN1) - state.dataHeatBalFanSys->ZoneAirHumRat(ZN2))) *
                                ReportingFraction;
                            state.dataAirflowNetwork->AirflowNetworkReportData(ZN2).MultiZoneMixLatGainJ +=
                                (state.dataAirflowNetworkBalanceManager->linkReport1(i).FLOWOFF *
                                 (state.dataHeatBalFanSys->ZoneAirHumRat(ZN1) - state.dataHeatBalFanSys->ZoneAirHumRat(ZN2))) *
                                ReportingConstant * ReportingFraction;
                        } else {
                            state.dataAirflowNetwork->AirflowNetworkReportData(ZN2).MultiZoneMixLatLossW +=
                                (state.dataAirflowNetworkBalanceManager->linkReport1(i).FLOWOFF *
                                 (state.dataHeatBalFanSys->ZoneAirHumRat(ZN2) - state.dataHeatBalFanSys->ZoneAirHumRat(ZN1))) *
                                ReportingFraction;
                            state.dataAirflowNetwork->AirflowNetworkReportData(ZN2).MultiZoneMixLatLossJ +=
                                (state.dataAirflowNetworkBalanceManager->linkReport1(i).FLOWOFF *
                                 (state.dataHeatBalFanSys->ZoneAirHumRat(ZN2) - state.dataHeatBalFanSys->ZoneAirHumRat(ZN1))) *
                                ReportingConstant * ReportingFraction;
                        }
                        CpAir = PsyCpAirFnW(state.dataHeatBalFanSys->ZoneAirHumRat(ZN2));
                        if (state.dataHeatBalFanSys->MAT(ZN2) > state.dataHeatBalFanSys->MAT(ZN1)) {
                            state.dataAirflowNetwork->AirflowNetworkReportData(ZN1).MultiZoneMixSenGainW +=
                                (state.dataAirflowNetworkBalanceManager->linkReport1(i).FLOW2OFF * CpAir *
                                 (state.dataHeatBalFanSys->MAT(ZN2) - state.dataHeatBalFanSys->MAT(ZN1))) *
                                ReportingFraction;
                            state.dataAirflowNetwork->AirflowNetworkReportData(ZN1).MultiZoneMixSenGainJ +=
                                (state.dataAirflowNetworkBalanceManager->linkReport1(i).FLOW2OFF * CpAir *
                                 (state.dataHeatBalFanSys->MAT(ZN2) - state.dataHeatBalFanSys->MAT(ZN1))) *
                                ReportingConstant * ReportingFraction;
                        } else {
                            state.dataAirflowNetwork->AirflowNetworkReportData(ZN1).MultiZoneMixSenLossW +=
                                (state.dataAirflowNetworkBalanceManager->linkReport1(i).FLOW2OFF * CpAir *
                                 (state.dataHeatBalFanSys->MAT(ZN1) - state.dataHeatBalFanSys->MAT(ZN2))) *
                                ReportingFraction;
                            state.dataAirflowNetwork->AirflowNetworkReportData(ZN1).MultiZoneMixSenLossJ +=
                                (state.dataAirflowNetworkBalanceManager->linkReport1(i).FLOW2OFF * CpAir *
                                 (state.dataHeatBalFanSys->MAT(ZN1) - state.dataHeatBalFanSys->MAT(ZN2))) *
                                ReportingConstant * ReportingFraction;
                        }

                        if (state.dataHeatBalFanSys->ZoneAirHumRat(ZN2) > state.dataHeatBalFanSys->ZoneAirHumRat(ZN1)) {
                            state.dataAirflowNetwork->AirflowNetworkReportData(ZN1).MultiZoneMixLatGainW +=
                                (state.dataAirflowNetworkBalanceManager->linkReport1(i).FLOW2OFF *
                                 (state.dataHeatBalFanSys->ZoneAirHumRat(ZN2) - state.dataHeatBalFanSys->ZoneAirHumRat(ZN1))) *
                                ReportingFraction;
                            state.dataAirflowNetwork->AirflowNetworkReportData(ZN1).MultiZoneMixLatGainJ +=
                                (state.dataAirflowNetworkBalanceManager->linkReport1(i).FLOW2OFF *
                                 (state.dataHeatBalFanSys->ZoneAirHumRat(ZN2) - state.dataHeatBalFanSys->ZoneAirHumRat(ZN1))) *
                                ReportingConstant * ReportingFraction;
                        } else {
                            state.dataAirflowNetwork->AirflowNetworkReportData(ZN1).MultiZoneMixLatLossW +=
                                std::abs(state.dataAirflowNetworkBalanceManager->linkReport1(i).FLOW2OFF *
                                         (state.dataHeatBalFanSys->ZoneAirHumRat(ZN1) - state.dataHeatBalFanSys->ZoneAirHumRat(ZN2))) *
                                ReportingFraction;
                            state.dataAirflowNetwork->AirflowNetworkReportData(ZN1).MultiZoneMixLatLossJ +=
                                (state.dataAirflowNetworkBalanceManager->linkReport1(i).FLOW2OFF *
                                 (state.dataHeatBalFanSys->ZoneAirHumRat(ZN1) - state.dataHeatBalFanSys->ZoneAirHumRat(ZN2))) *
                                ReportingConstant * ReportingFraction;
                        }
                    }
                }
            }
        }

        if (!(state.dataAirflowNetwork->SimulateAirflowNetwork == AirflowNetworkControlMultizone ||
              state.dataAirflowNetwork->SimulateAirflowNetwork == AirflowNetworkControlMultiADS))
            return;

        for (i = 1; i <= state.dataGlobal->NumOfZones; ++i) { // Start of zone loads report variable update loop ...
            Tamb = Zone(i).OutDryBulbTemp;
            CpAir = PsyCpAirFnW(state.dataHeatBalFanSys->ZoneAirHumRatAvg(i));
            AirDensity = PsyRhoAirFnPbTdbW(
                state, state.dataEnvrn->OutBaroPress, state.dataHeatBalFanSys->MAT(i), state.dataHeatBalFanSys->ZoneAirHumRatAvg(i));

            state.dataAirflowNetworkBalanceManager->AirflowNetworkZnRpt(i).InfilMass =
                (state.dataAirflowNetworkBalanceManager->exchangeData(i).SumMCp / CpAir) * ReportingConstant;
            state.dataAirflowNetworkBalanceManager->AirflowNetworkZnRpt(i).InfilVolume =
                state.dataAirflowNetworkBalanceManager->AirflowNetworkZnRpt(i).InfilMass / AirDensity;
            state.dataAirflowNetworkBalanceManager->AirflowNetworkZnRpt(i).InfilAirChangeRate =
                state.dataAirflowNetworkBalanceManager->AirflowNetworkZnRpt(i).InfilVolume / (TimeStepSys * Zone(i).Volume);
            state.dataAirflowNetworkBalanceManager->AirflowNetworkZnRpt(i).VentilMass =
                (state.dataAirflowNetworkBalanceManager->exchangeData(i).SumMVCp / CpAir) * ReportingConstant;
            state.dataAirflowNetworkBalanceManager->AirflowNetworkZnRpt(i).VentilVolume =
                state.dataAirflowNetworkBalanceManager->AirflowNetworkZnRpt(i).VentilMass / AirDensity;
            state.dataAirflowNetworkBalanceManager->AirflowNetworkZnRpt(i).VentilAirChangeRate =
                state.dataAirflowNetworkBalanceManager->AirflowNetworkZnRpt(i).VentilVolume / (TimeStepSys * Zone(i).Volume);
            state.dataAirflowNetworkBalanceManager->AirflowNetworkZnRpt(i).MixMass =
                (state.dataAirflowNetworkBalanceManager->exchangeData(i).SumMMCp / CpAir) * ReportingConstant;
            state.dataAirflowNetworkBalanceManager->AirflowNetworkZnRpt(i).MixVolume =
                state.dataAirflowNetworkBalanceManager->AirflowNetworkZnRpt(i).MixMass / AirDensity;
            // save values for predefined report
            Real64 stdDensAFNInfilVolume = state.dataAirflowNetworkBalanceManager->AirflowNetworkZnRpt(i).InfilMass / state.dataEnvrn->StdRhoAir;
            Real64 stdDensAFNNatVentVolume = state.dataAirflowNetworkBalanceManager->AirflowNetworkZnRpt(i).VentilMass / state.dataEnvrn->StdRhoAir;
            state.dataHeatBal->ZonePreDefRep(i).AFNVentVolStdDen = stdDensAFNNatVentVolume;
            state.dataHeatBal->ZonePreDefRep(i).AFNVentVolTotalStdDen += stdDensAFNNatVentVolume;
            state.dataHeatBal->ZonePreDefRep(i).AFNInfilVolTotalStdDen += stdDensAFNInfilVolume;
            if (state.dataHeatBal->ZonePreDefRep(i).isOccupied) {
                state.dataHeatBal->ZonePreDefRep(i).AFNVentVolTotalOccStdDen += stdDensAFNNatVentVolume;
                state.dataHeatBal->ZonePreDefRep(i).AFNInfilVolTotalOccStdDen += stdDensAFNInfilVolume;
                state.dataHeatBal->ZonePreDefRep(i).AFNInfilVolTotalOcc +=
                    (state.dataAirflowNetworkBalanceManager->AirflowNetworkZnRpt(i).InfilVolume +
                     state.dataAirflowNetworkBalanceManager->AirflowNetworkZnRpt(i).VentilVolume) *
                    Zone(i).Multiplier * Zone(i).ListMultiplier;
                if ((state.dataAirflowNetworkBalanceManager->AirflowNetworkZnRpt(i).InfilVolume +
                     state.dataAirflowNetworkBalanceManager->AirflowNetworkZnRpt(i).VentilVolume) <
                    state.dataHeatBal->ZonePreDefRep(i).AFNInfilVolMin) {
                    state.dataHeatBal->ZonePreDefRep(i).AFNInfilVolMin =
                        (state.dataAirflowNetworkBalanceManager->AirflowNetworkZnRpt(i).InfilVolume +
                         state.dataAirflowNetworkBalanceManager->AirflowNetworkZnRpt(i).VentilVolume) *
                        Zone(i).Multiplier * Zone(i).ListMultiplier;
                }
            }

            Real64 H2OHtOfVap = Psychrometrics::PsyHgAirFnWTdb(state.dataEnvrn->OutHumRat, Zone(i).OutDryBulbTemp);
            state.dataAirflowNetworkBalanceManager->AirflowNetworkZnRpt(i).InletMass = 0;
            state.dataAirflowNetworkBalanceManager->AirflowNetworkZnRpt(i).OutletMass = 0;
            if (state.dataZoneEquip->ZoneEquipConfig(i).IsControlled) {
                for (int j = 1; j <= state.dataZoneEquip->ZoneEquipConfig(i).NumInletNodes; ++j) {
                    state.dataAirflowNetworkBalanceManager->AirflowNetworkZnRpt(i).InletMass +=
                        state.dataLoopNodes->Node(state.dataZoneEquip->ZoneEquipConfig(i).InletNode(j)).MassFlowRate * ReportingConstant;
                }
                for (int j = 1; j <= state.dataZoneEquip->ZoneEquipConfig(i).NumExhaustNodes; ++j) {
                    state.dataAirflowNetworkBalanceManager->AirflowNetworkZnRpt(i).OutletMass +=
                        state.dataLoopNodes->Node(state.dataZoneEquip->ZoneEquipConfig(i).ExhaustNode(j)).MassFlowRate * ReportingConstant;
                }
                for (int j = 1; j <= state.dataZoneEquip->ZoneEquipConfig(i).NumReturnNodes; ++j) {
                    state.dataAirflowNetworkBalanceManager->AirflowNetworkZnRpt(i).OutletMass +=
                        state.dataLoopNodes->Node(state.dataZoneEquip->ZoneEquipConfig(i).ReturnNode(j)).MassFlowRate * ReportingConstant;
                }
            }
            state.dataAirflowNetworkBalanceManager->AirflowNetworkZnRpt(i).ExfilMass =
                state.dataAirflowNetworkBalanceManager->AirflowNetworkZnRpt(i).InfilMass +
                state.dataAirflowNetworkBalanceManager->AirflowNetworkZnRpt(i).VentilMass +
                state.dataAirflowNetworkBalanceManager->AirflowNetworkZnRpt(i).MixMass +
                state.dataAirflowNetworkBalanceManager->AirflowNetworkZnRpt(i).InletMass -
                state.dataAirflowNetworkBalanceManager->AirflowNetworkZnRpt(i).OutletMass;
            state.dataAirflowNetworkBalanceManager->AirflowNetworkZnRpt(i).ExfilSensiLoss =
                state.dataAirflowNetworkBalanceManager->AirflowNetworkZnRpt(i).ExfilMass / ReportingConstant *
                (state.dataHeatBalFanSys->MAT(i) - Tamb) * CpAir;
            state.dataAirflowNetworkBalanceManager->AirflowNetworkZnRpt(i).ExfilLatentLoss =
                state.dataAirflowNetworkBalanceManager->AirflowNetworkZnRpt(i).ExfilMass / ReportingConstant *
                (state.dataHeatBalFanSys->ZoneAirHumRat(i) - state.dataEnvrn->OutHumRat) * H2OHtOfVap;
            state.dataAirflowNetworkBalanceManager->AirflowNetworkZnRpt(i).ExfilTotalLoss =
                state.dataAirflowNetworkBalanceManager->AirflowNetworkZnRpt(i).ExfilSensiLoss +
                state.dataAirflowNetworkBalanceManager->AirflowNetworkZnRpt(i).ExfilLatentLoss;

            state.dataHeatBal->ZoneTotalExfiltrationHeatLoss +=
                state.dataAirflowNetworkBalanceManager->AirflowNetworkZnRpt(i).ExfilTotalLoss * ReportingConstant;
        } // ... end of zone loads report variable update loop.

        // Rewrite AirflowNetwork airflow rate
        for (AirLoopNum = 1; AirLoopNum <= NumPrimaryAirSys; ++AirLoopNum) {
            if (state.dataAirflowNetworkBalanceManager->DisSysNumOfCVFs == 0) continue;
            for (FanNum = 1; FanNum <= state.dataAirflowNetworkBalanceManager->DisSysNumOfCVFs; ++FanNum) {
                if (state.dataAirflowNetwork->DisSysCompCVFData(FanNum).AirLoopNum == AirLoopNum) break;
            }
            state.dataAirflowNetworkBalanceManager->onceSurfFlag = false;

            for (i = 1; i <= state.dataAirflowNetwork->NumOfLinksMultiZone; ++i) {
                if (state.dataAirflowNetworkBalanceManager->onceSurfFlag(i)) continue;
                if (state.dataAirflowNetwork->DisSysCompCVFData(FanNum).AirLoopNum == AirLoopNum) {
                    Tamb = OutDryBulbTempAt(state, state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeHeights[0]);
                    AirDensity = PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, Tamb, state.dataEnvrn->OutHumRat);
                    if (state.dataAirflowNetwork->DisSysCompCVFData(FanNum).FanTypeNum == FanType_SimpleOnOff &&
                        state.dataAirflowNetworkBalanceManager->LoopOnOffFanRunTimeFraction(AirLoopNum) < 1.0 &&
                        state.dataAirflowNetworkBalanceManager->LoopOnOffFanRunTimeFraction(AirLoopNum) > 0.0) {
                        state.dataAirflowNetworkBalanceManager->linkReport(i).VolFLOW =
                            state.dataAirflowNetworkBalanceManager->linkReport1(i).FLOW / AirDensity;
                        state.dataAirflowNetworkBalanceManager->linkReport(i).VolFLOW2 =
                            state.dataAirflowNetworkBalanceManager->linkReport1(i).FLOW2 / AirDensity;
                    } else {
                        state.dataAirflowNetworkBalanceManager->linkReport(i).VolFLOW =
                            state.dataAirflowNetworkBalanceManager->linkReport(i).FLOW / AirDensity;
                        state.dataAirflowNetworkBalanceManager->linkReport(i).VolFLOW2 =
                            state.dataAirflowNetworkBalanceManager->linkReport(i).FLOW2 / AirDensity;
                    }
                    state.dataAirflowNetworkBalanceManager->onceSurfFlag(i) = true;
                }
            }

            if (state.dataAirflowNetwork->AirflowNetworkNumOfLinks > state.dataAirflowNetwork->NumOfLinksMultiZone) {
                for (i = state.dataAirflowNetwork->NumOfLinksMultiZone + 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfLinks; ++i) {
                    if (state.dataAirflowNetworkBalanceManager->onceSurfFlag(i)) continue;
                    if (state.dataAirflowNetwork->DisSysCompCVFData(FanNum).AirLoopNum == AirLoopNum) {
                        n = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
                        M = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
                        AirDensity = PsyRhoAirFnPbTdbW(
                            state,
                            (state.dataAirflowNetwork->AirflowNetworkNodeSimu(n).PZ + state.dataAirflowNetwork->AirflowNetworkNodeSimu(M).PZ) / 2.0 +
                                state.dataEnvrn->OutBaroPress,
                            (state.dataAirflowNetwork->AirflowNetworkNodeSimu(n).TZ + state.dataAirflowNetwork->AirflowNetworkNodeSimu(M).TZ) / 2.0,
                            (state.dataAirflowNetwork->AirflowNetworkNodeSimu(n).WZ + state.dataAirflowNetwork->AirflowNetworkNodeSimu(M).WZ) / 2.0);
                        if (state.dataAirflowNetwork->DisSysCompCVFData(FanNum).FanTypeNum == FanType_SimpleOnOff &&
                            state.dataAirflowNetworkBalanceManager->LoopOnOffFanRunTimeFraction(AirLoopNum) < 1.0 &&
                            state.dataAirflowNetworkBalanceManager->LoopOnOffFanRunTimeFraction(AirLoopNum) > 0.0) {
                            state.dataAirflowNetworkBalanceManager->linkReport(i).VolFLOW =
                                state.dataAirflowNetworkBalanceManager->linkReport(i).FLOW / AirDensity *
                                (1.0 - state.dataAirflowNetworkBalanceManager->LoopOnOffFanRunTimeFraction(AirLoopNum));
                            state.dataAirflowNetworkBalanceManager->linkReport(i).VolFLOW2 =
                                state.dataAirflowNetworkBalanceManager->linkReport(i).FLOW2 / AirDensity *
                                (1.0 - state.dataAirflowNetworkBalanceManager->LoopOnOffFanRunTimeFraction(AirLoopNum));
                            state.dataAirflowNetworkBalanceManager->onceSurfFlag(i) = true;
                        } else {
                            state.dataAirflowNetworkBalanceManager->linkReport(i).VolFLOW =
                                state.dataAirflowNetworkBalanceManager->linkReport(i).FLOW / AirDensity;
                            state.dataAirflowNetworkBalanceManager->linkReport(i).VolFLOW2 =
                                state.dataAirflowNetworkBalanceManager->linkReport(i).FLOW2 / AirDensity;
                        }
                    }
                }
            }
        }
    }

    void UpdateAirflowNetwork(EnergyPlusData &state,
                              Optional_bool_const FirstHVACIteration) // True when solution technique on first iteration
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   12/10/05
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine update variables used in the AirflowNetwork model.

        // Using/Aliasing
        auto &NumPrimaryAirSys = state.dataHVACGlobal->NumPrimaryAirSys;
        using DataHVACGlobals::VerySmallMassFlow;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int i;
        int j;
        int n;
        int M;
        int ZN1;
        int ZN2;
        int Node1;
        int Node2;
        int Node3;
        Real64 CpAir;
        Real64 Qsen;
        Real64 Qlat;
        Real64 AirDensity;
        Real64 Tamb;
        Real64 PartLoadRatio;
        Real64 OnOffRatio;
        Real64 NodeMass;
        Real64 AFNMass;
        bool WriteFlag;

        auto &Zone(state.dataHeatBal->Zone);
        auto &Node(state.dataLoopNodes->Node);

        for (auto &e : state.dataAirflowNetworkBalanceManager->exchangeData) {
            e.SumMCp = 0.0;
            e.SumMCpT = 0.0;
            e.SumMVCp = 0.0;
            e.SumMVCpT = 0.0;
            e.SumMHr = 0.0;
            e.SumMHrW = 0.0;
            e.SumMMCp = 0.0;
            e.SumMMCpT = 0.0;
            e.SumMMHr = 0.0;
            e.SumMMHrW = 0.0;
        }
        if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
            for (auto &e : state.dataAirflowNetworkBalanceManager->exchangeData) {
                e.SumMHrCO = 0.0;
                e.SumMMHrCO = 0.0;
            }
        }
        if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
            for (auto &e : state.dataAirflowNetworkBalanceManager->exchangeData) {
                e.SumMHrGC = 0.0;
                e.SumMMHrGC = 0.0;
            }
        }

        // Calculate sensible and latent loads in each zone from multizone airflows
        if (state.dataAirflowNetwork->SimulateAirflowNetwork == AirflowNetworkControlMultizone ||
            state.dataAirflowNetwork->SimulateAirflowNetwork == AirflowNetworkControlMultiADS ||
            (state.dataAirflowNetwork->SimulateAirflowNetwork == AirflowNetworkControlSimpleADS &&
             state.dataAirflowNetwork->AirflowNetworkFanActivated)) {
            for (i = 1; i <= state.dataAirflowNetwork->NumOfLinksMultiZone; ++i) { // Multizone airflow energy
                n = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
                M = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
                ZN1 = state.dataAirflowNetwork->AirflowNetworkNodeData(n).EPlusZoneNum;
                ZN2 = state.dataAirflowNetwork->AirflowNetworkNodeData(M).EPlusZoneNum;
                if (ZN1 > 0 && ZN2 == 0) {
                    // Find a linkage from outdoors to this zone
                    Tamb = Zone(ZN1).OutDryBulbTemp;
                    CpAir = PsyCpAirFnW(state.dataEnvrn->OutHumRat);
                    if (state.dataAirflowNetwork->AirflowNetworkCompData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).CompNum)
                                .CompTypeNum == iComponentTypeNum::SCR ||
                        state.dataAirflowNetwork->AirflowNetworkCompData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).CompNum)
                                .CompTypeNum == iComponentTypeNum::SEL) {
                        state.dataAirflowNetworkBalanceManager->exchangeData(ZN1).SumMCp +=
                            state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2 * CpAir;
                        state.dataAirflowNetworkBalanceManager->exchangeData(ZN1).SumMCpT +=
                            state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2 * CpAir * Tamb;
                    } else {
                        state.dataAirflowNetworkBalanceManager->exchangeData(ZN1).SumMVCp +=
                            state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2 * CpAir;
                        state.dataAirflowNetworkBalanceManager->exchangeData(ZN1).SumMVCpT +=
                            state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2 * CpAir * Tamb;
                    }
                    state.dataAirflowNetworkBalanceManager->exchangeData(ZN1).SumMHr += state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2;
                    state.dataAirflowNetworkBalanceManager->exchangeData(ZN1).SumMHrW +=
                        state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2 * state.dataEnvrn->OutHumRat;
                    if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
                        state.dataAirflowNetworkBalanceManager->exchangeData(ZN1).SumMHrCO +=
                            state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2 * state.dataContaminantBalance->OutdoorCO2;
                    }
                    if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
                        state.dataAirflowNetworkBalanceManager->exchangeData(ZN1).SumMHrGC +=
                            state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2 * state.dataContaminantBalance->OutdoorGC;
                    }
                }
                if (ZN1 == 0 && ZN2 > 0) {
                    // Find a linkage from outdoors to this zone
                    Tamb = Zone(ZN2).OutDryBulbTemp;
                    CpAir = PsyCpAirFnW(state.dataEnvrn->OutHumRat);
                    if (state.dataAirflowNetwork->AirflowNetworkCompData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).CompNum)
                                .CompTypeNum == iComponentTypeNum::SCR ||
                        state.dataAirflowNetwork->AirflowNetworkCompData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).CompNum)
                                .CompTypeNum == iComponentTypeNum::SEL) {
                        state.dataAirflowNetworkBalanceManager->exchangeData(ZN2).SumMCp +=
                            state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW * CpAir;
                        state.dataAirflowNetworkBalanceManager->exchangeData(ZN2).SumMCpT +=
                            state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW * CpAir * Tamb;
                    } else {
                        state.dataAirflowNetworkBalanceManager->exchangeData(ZN2).SumMVCp +=
                            state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW * CpAir;
                        state.dataAirflowNetworkBalanceManager->exchangeData(ZN2).SumMVCpT +=
                            state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW * CpAir * Tamb;
                    }
                    state.dataAirflowNetworkBalanceManager->exchangeData(ZN2).SumMHr += state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW;
                    state.dataAirflowNetworkBalanceManager->exchangeData(ZN2).SumMHrW +=
                        state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW * state.dataEnvrn->OutHumRat;
                    if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
                        state.dataAirflowNetworkBalanceManager->exchangeData(ZN2).SumMHrCO +=
                            state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW * state.dataContaminantBalance->OutdoorCO2;
                    }
                    if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
                        state.dataAirflowNetworkBalanceManager->exchangeData(ZN2).SumMHrGC +=
                            state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW * state.dataContaminantBalance->OutdoorGC;
                    }
                }
                if (ZN1 > 0 && ZN2 > 0) {
                    // Find a linkage from outdoors to this zone
                    CpAir = PsyCpAirFnW(state.dataAirflowNetwork->ANZW(ZN1));
                    state.dataAirflowNetworkBalanceManager->exchangeData(ZN2).SumMMCp +=
                        state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW * CpAir;
                    state.dataAirflowNetworkBalanceManager->exchangeData(ZN2).SumMMCpT +=
                        state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW * CpAir * state.dataAirflowNetwork->ANZT(ZN1);
                    state.dataAirflowNetworkBalanceManager->exchangeData(ZN2).SumMMHr += state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW;
                    state.dataAirflowNetworkBalanceManager->exchangeData(ZN2).SumMMHrW +=
                        state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW * state.dataAirflowNetwork->ANZW(ZN1);
                    if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
                        state.dataAirflowNetworkBalanceManager->exchangeData(ZN2).SumMMHrCO +=
                            state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW * state.dataAirflowNetwork->ANCO(ZN1);
                    }
                    if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
                        state.dataAirflowNetworkBalanceManager->exchangeData(ZN2).SumMMHrGC +=
                            state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW * state.dataAirflowNetwork->ANGC(ZN1);
                    }
                    CpAir = PsyCpAirFnW(state.dataAirflowNetwork->ANZW(ZN2));
                    state.dataAirflowNetworkBalanceManager->exchangeData(ZN1).SumMMCp +=
                        state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2 * CpAir;
                    state.dataAirflowNetworkBalanceManager->exchangeData(ZN1).SumMMCpT +=
                        state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2 * CpAir * state.dataAirflowNetwork->ANZT(ZN2);
                    state.dataAirflowNetworkBalanceManager->exchangeData(ZN1).SumMMHr += state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2;
                    state.dataAirflowNetworkBalanceManager->exchangeData(ZN1).SumMMHrW +=
                        state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2 * state.dataAirflowNetwork->ANZW(ZN2);
                    if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
                        state.dataAirflowNetworkBalanceManager->exchangeData(ZN1).SumMMHrCO +=
                            state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2 * state.dataAirflowNetwork->ANCO(ZN2);
                    }
                    if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
                        state.dataAirflowNetworkBalanceManager->exchangeData(ZN1).SumMMHrGC +=
                            state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2 * state.dataAirflowNetwork->ANGC(ZN2);
                    }
                }
            }
        }
        // End of update of multizone airflow calculations

        // Initialize these values
        for (auto &e : state.dataAirflowNetworkBalanceManager->exchangeData) {
            e.LeakSen = 0.0;
            e.CondSen = 0.0;
            e.LeakLat = 0.0;
            e.DiffLat = 0.0;
            e.MultiZoneSen = 0.0;
            e.MultiZoneLat = 0.0;
            e.RadGain = 0.0;
        }

        // Rewrite AirflowNetwork airflow rate
        for (i = 1; i <= state.dataAirflowNetwork->NumOfLinksMultiZone; ++i) {
            Tamb = OutDryBulbTempAt(state, state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeHeights[0]);
            AirDensity = PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, Tamb, state.dataEnvrn->OutHumRat);
            state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).VolFLOW = state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW / AirDensity;
            state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).VolFLOW2 = state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2 / AirDensity;
        }

        for (std::size_t i = 0; i < state.dataAirflowNetworkBalanceManager->linkReport.size(); ++i) {
            auto &r(state.dataAirflowNetworkBalanceManager->linkReport[i]);
            auto &s(state.dataAirflowNetwork->AirflowNetworkLinkSimu[i]);
            r.FLOW = s.FLOW;
            r.FLOW2 = s.FLOW2;
            r.VolFLOW = s.VolFLOW;
            r.VolFLOW2 = s.VolFLOW2;
        }

        // Save zone loads from multizone calculation for later summation
        bool OnOffFanFlag = false;
        for (i = 1; i <= state.dataAirflowNetworkBalanceManager->DisSysNumOfCVFs; i++) {
            if (state.dataAirflowNetwork->DisSysCompCVFData(i).FanTypeNum == FanType_SimpleOnOff) {
                OnOffFanFlag = true;
                break;
            }
        }
        if (present(FirstHVACIteration)) {
            if (FirstHVACIteration && OnOffFanFlag) {
                state.dataAirflowNetworkBalanceManager->multiExchangeData = state.dataAirflowNetworkBalanceManager->exchangeData;
                for (i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfZones; ++i) {
                    state.dataAirflowNetworkBalanceManager->nodeReport(i).PZ = state.dataAirflowNetwork->AirflowNetworkNodeSimu(i).PZ;
                    state.dataAirflowNetworkBalanceManager->nodeReport(i).PZOFF = state.dataAirflowNetwork->AirflowNetworkNodeSimu(i).PZ;
                    state.dataAirflowNetworkBalanceManager->nodeReport(i).PZON = 0.0;
                }
                for (i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces; ++i) {
                    state.dataAirflowNetworkBalanceManager->linkReport1(i).FLOW = state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW;
                    state.dataAirflowNetworkBalanceManager->linkReport1(i).FLOW2 = state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2;
                    state.dataAirflowNetworkBalanceManager->linkReport1(i).VolFLOW = state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).VolFLOW;
                    state.dataAirflowNetworkBalanceManager->linkReport1(i).VolFLOW2 = state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).VolFLOW2;
                    state.dataAirflowNetworkBalanceManager->linkReport1(i).FLOWOFF = state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW;
                    state.dataAirflowNetworkBalanceManager->linkReport1(i).FLOW2OFF = state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2;
                    state.dataAirflowNetworkBalanceManager->linkReport1(i).VolFLOWOFF = state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).VolFLOW;
                    state.dataAirflowNetworkBalanceManager->linkReport1(i).VolFLOW2OFF = state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).VolFLOW2;
                    state.dataAirflowNetworkBalanceManager->linkReport1(i).DP = state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).DP;
                    state.dataAirflowNetworkBalanceManager->linkReport1(i).DPOFF = state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).DP;
                    state.dataAirflowNetworkBalanceManager->linkReport1(i).DPON = 0.0;
                }
            }
        }

        if (!state.dataAirflowNetwork->AirflowNetworkFanActivated &&
            (state.dataAirflowNetwork->SimulateAirflowNetwork > AirflowNetworkControlMultizone)) {
            for (i = state.dataAirflowNetwork->NumOfNodesMultiZone + state.dataAirflowNetwork->NumOfNodesIntraZone + 1;
                 i <= state.dataAirflowNetwork->AirflowNetworkNumOfNodes;
                 ++i) {
                state.dataAirflowNetwork->AirflowNetworkNodeSimu(i).PZ = 0.0;
            }
            for (i = state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces + 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfLinks; ++i) {
                state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).DP = 0.0;
                state.dataAirflowNetworkBalanceManager->linkReport(i).FLOW = 0.0;
                state.dataAirflowNetworkBalanceManager->linkReport(i).FLOW2 = 0.0;
                state.dataAirflowNetworkBalanceManager->linkReport(i).VolFLOW = 0.0;
                state.dataAirflowNetworkBalanceManager->linkReport(i).VolFLOW2 = 0.0;
            }
        }

        if (!(state.dataAirflowNetwork->AirflowNetworkFanActivated &&
              state.dataAirflowNetwork->SimulateAirflowNetwork > AirflowNetworkControlMultizone))
            return;

        if (state.dataAirflowNetwork->SimulateAirflowNetwork > AirflowNetworkControlMultizone + 1) {
            for (i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces; ++i) { // Multizone airflow energy
                n = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
                M = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
                ZN1 = state.dataAirflowNetwork->AirflowNetworkNodeData(n).EPlusZoneNum;
                ZN2 = state.dataAirflowNetwork->AirflowNetworkNodeData(M).EPlusZoneNum;
                // Find a linkage from a zone to outdoors
                if (ZN1 > 0 && ZN2 == 0) {
                    Tamb = Zone(ZN1).OutDryBulbTemp;
                    CpAir = PsyCpAirFnW(state.dataEnvrn->OutHumRat);
                    state.dataAirflowNetworkBalanceManager->exchangeData(ZN1).MultiZoneSen +=
                        state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2 * CpAir * (Tamb - state.dataAirflowNetwork->ANZT(ZN1));
                    state.dataAirflowNetworkBalanceManager->exchangeData(ZN1).MultiZoneLat +=
                        state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2 *
                        (state.dataEnvrn->OutHumRat - state.dataAirflowNetwork->ANZW(ZN1));
                }
                if (ZN1 == 0 && ZN2 > 0) {
                    Tamb = Zone(ZN2).OutDryBulbTemp;
                    CpAir = PsyCpAirFnW(state.dataEnvrn->OutHumRat);
                    state.dataAirflowNetworkBalanceManager->exchangeData(ZN2).MultiZoneSen +=
                        state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW * CpAir * (Tamb - state.dataAirflowNetwork->ANZT(ZN2));
                    state.dataAirflowNetworkBalanceManager->exchangeData(ZN2).MultiZoneLat +=
                        state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW * (state.dataEnvrn->OutHumRat - state.dataAirflowNetwork->ANZW(ZN2));
                }

                if (ZN1 > 0 && ZN2 > 0) {
                    if (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW > 0) { // Flow from ZN1 to ZN2
                        CpAir = PsyCpAirFnW(state.dataAirflowNetwork->ANZW(ZN1));
                        state.dataAirflowNetworkBalanceManager->exchangeData(ZN2).MultiZoneSen +=
                            state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW * CpAir *
                            (state.dataAirflowNetwork->ANZT(ZN1) - state.dataAirflowNetwork->ANZT(ZN2));
                        state.dataAirflowNetworkBalanceManager->exchangeData(ZN2).MultiZoneLat +=
                            state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW *
                            (state.dataAirflowNetwork->ANZW(ZN1) - state.dataAirflowNetwork->ANZW(ZN2));
                        CpAir = PsyCpAirFnW(state.dataAirflowNetwork->ANZW(ZN2));
                        state.dataAirflowNetworkBalanceManager->exchangeData(ZN1).MultiZoneSen +=
                            std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2) * CpAir *
                            (state.dataAirflowNetwork->ANZT(ZN2) - state.dataAirflowNetwork->ANZT(ZN1));
                        state.dataAirflowNetworkBalanceManager->exchangeData(ZN1).MultiZoneLat +=
                            std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2) *
                            (state.dataAirflowNetwork->ANZW(ZN2) - state.dataAirflowNetwork->ANZW(ZN1));
                    } else {
                        CpAir = PsyCpAirFnW(state.dataAirflowNetwork->ANZW(ZN2));
                        state.dataAirflowNetworkBalanceManager->exchangeData(ZN1).MultiZoneSen +=
                            std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2) * CpAir *
                            (state.dataAirflowNetwork->ANZT(ZN2) - state.dataAirflowNetwork->ANZT(ZN1));
                        state.dataAirflowNetworkBalanceManager->exchangeData(ZN1).MultiZoneLat +=
                            std::abs(state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2) *
                            (state.dataAirflowNetwork->ANZW(ZN2) - state.dataAirflowNetwork->ANZW(ZN1));
                    }
                }
            }
        }

        int AirLoopNum;
        int FanNum;
        Real64 MaxPartLoadRatio = 0.0;
        Real64 OnOffFanRunTimeFraction = 0.0;
        state.dataAirflowNetworkBalanceManager->MaxOnOffFanRunTimeFraction = 0.0;
        for (AirLoopNum = 1; AirLoopNum <= NumPrimaryAirSys; ++AirLoopNum) {
            MaxPartLoadRatio = max(MaxPartLoadRatio, state.dataAirLoop->AirLoopAFNInfo(AirLoopNum).LoopOnOffFanPartLoadRatio);
            state.dataAirflowNetworkBalanceManager->MaxOnOffFanRunTimeFraction =
                max(state.dataAirflowNetworkBalanceManager->MaxOnOffFanRunTimeFraction,
                    state.dataAirflowNetworkBalanceManager->LoopOnOffFanRunTimeFraction(AirLoopNum));
        }
        for (AirLoopNum = 1; AirLoopNum <= NumPrimaryAirSys; ++AirLoopNum) {
            for (FanNum = 1; FanNum <= state.dataAirflowNetworkBalanceManager->DisSysNumOfCVFs; ++FanNum) {
                if (state.dataAirflowNetwork->DisSysCompCVFData(FanNum).AirLoopNum == AirLoopNum) break;
            }
            PartLoadRatio = 1.0;
            state.dataAirflowNetworkBalanceManager->LoopPartLoadRatio(AirLoopNum) = 1.0;
            OnOffFanRunTimeFraction = 1.0;
            state.dataAirflowNetworkBalanceManager->LoopOnOffFanRunTimeFraction(AirLoopNum) = 1.0;
            // Calculate the part load ratio, can't be greater than 1 for a simple ONOFF fan
            if (state.dataAirflowNetwork->DisSysCompCVFData(FanNum).FanTypeNum == FanType_SimpleOnOff &&
                Node(state.dataAirflowNetwork->DisSysCompCVFData(FanNum).InletNode).MassFlowRate > VerySmallMassFlow &&
                state.dataAirLoop->AirLoopAFNInfo(AirLoopNum).LoopFanOperationMode == CycFanCycCoil) {
                // Hard code here
                PartLoadRatio = state.dataAirLoop->AirLoopAFNInfo(AirLoopNum).LoopOnOffFanPartLoadRatio;
                state.dataAirflowNetworkBalanceManager->LoopPartLoadRatio(AirLoopNum) =
                    state.dataAirLoop->AirLoopAFNInfo(AirLoopNum).LoopOnOffFanPartLoadRatio;
                OnOffFanRunTimeFraction = max(state.dataAirLoop->AirLoopAFNInfo(AirLoopNum).AFNLoopHeatingCoilMaxRTF,
                                              state.dataAirLoop->AirLoopAFNInfo(AirLoopNum).AFNLoopOnOffFanRTF,
                                              state.dataAirLoop->AirLoopAFNInfo(AirLoopNum).AFNLoopDXCoilRTF);
                state.dataAirflowNetworkBalanceManager->LoopOnOffFanRunTimeFraction(AirLoopNum) =
                    max(state.dataAirLoop->AirLoopAFNInfo(AirLoopNum).AFNLoopHeatingCoilMaxRTF,
                        state.dataAirLoop->AirLoopAFNInfo(AirLoopNum).AFNLoopOnOffFanRTF,
                        state.dataAirLoop->AirLoopAFNInfo(AirLoopNum).AFNLoopDXCoilRTF);
            }
            state.dataAirLoop->AirLoopAFNInfo(AirLoopNum).AFNLoopHeatingCoilMaxRTF = 0.0;

            if (state.dataAirflowNetwork->DisSysCompCVFData(FanNum).FanTypeNum == FanType_SimpleOnOff &&
                state.dataAirflowNetworkBalanceManager->LoopPartLoadRatio(AirLoopNum) < 1.0) {
                for (std::size_t i = 0; i < state.dataAirflowNetworkBalanceManager->linkReport.size(); ++i) {
                    auto &r(state.dataAirflowNetworkBalanceManager->linkReport[i]);
                    auto &s(state.dataAirflowNetwork->AirflowNetworkLinkSimu[i]);
                    auto &t(state.dataAirflowNetwork->AirflowNetworkLinkageData[i]);
                    if (t.AirLoopNum == AirLoopNum) {
                        r.FLOW = s.FLOW * state.dataAirflowNetworkBalanceManager->LoopPartLoadRatio(AirLoopNum);
                        r.FLOW2 = s.FLOW2 * state.dataAirflowNetworkBalanceManager->LoopPartLoadRatio(AirLoopNum);
                        r.VolFLOW = s.VolFLOW * state.dataAirflowNetworkBalanceManager->LoopPartLoadRatio(AirLoopNum);
                        r.VolFLOW2 = s.VolFLOW2 * state.dataAirflowNetworkBalanceManager->LoopPartLoadRatio(AirLoopNum);
                    }
                    if (t.AirLoopNum == 0) {
                        r.FLOW = s.FLOW * MaxPartLoadRatio;
                        r.FLOW2 = s.FLOW2 * MaxPartLoadRatio;
                        r.VolFLOW = s.VolFLOW * MaxPartLoadRatio;
                        r.VolFLOW2 = s.VolFLOW2 * MaxPartLoadRatio;
                    }
                }
            }
        }

        // One time warning
        if (state.dataAirflowNetworkBalanceManager->UpdateAirflowNetworkMyOneTimeFlag) {
            for (AirLoopNum = 1; AirLoopNum <= NumPrimaryAirSys; ++AirLoopNum) {
                for (FanNum = 1; FanNum <= state.dataAirflowNetworkBalanceManager->DisSysNumOfCVFs; ++FanNum) {
                    if (state.dataAirflowNetwork->DisSysCompCVFData(FanNum).AirLoopNum == AirLoopNum) break;
                }
                if (state.dataAirflowNetwork->DisSysCompCVFData(FanNum).FanTypeNum == FanType_SimpleOnOff &&
                    state.dataAirLoop->AirLoopAFNInfo(AirLoopNum).LoopFanOperationMode == ContFanCycCoil) {
                    OnOffRatio = std::abs((state.dataAirLoop->AirLoopAFNInfo(AirLoopNum).LoopSystemOnMassFlowrate -
                                           state.dataAirLoop->AirLoopAFNInfo(AirLoopNum).LoopSystemOffMassFlowrate) /
                                          state.dataAirLoop->AirLoopAFNInfo(AirLoopNum).LoopSystemOnMassFlowrate);
                    if (OnOffRatio > 0.1) {
                        ShowWarningError(state,
                                         "The absolute percent difference of supply air mass flow rate between HVAC operation and No HVAC operation "
                                         "is above 10% with fan operation mode = ContFanCycCoil.");
                        ShowContinueError(state,
                                          "The added zone loads using the AirflowNetwork model may not be accurate because the zone loads are "
                                          "calculated based on the mass flow rate during HVAC operation.");
                        ShowContinueError(
                            state,
                            format("The mass flow rate during HVAC operation = {:.2R} The mass flow rate during no HVAC operation = {:.2R}",
                                   state.dataAirLoop->AirLoopAFNInfo(AirLoopNum).LoopSystemOnMassFlowrate,
                                   state.dataAirLoop->AirLoopAFNInfo(AirLoopNum).LoopSystemOffMassFlowrate));
                        state.dataAirflowNetworkBalanceManager->UpdateAirflowNetworkMyOneTimeFlag = false;
                    }
                }
            }
        }

        // Check mass flow differences in the zone inlet zones and splitter nodes between node and AFN links
        if (state.dataAirflowNetworkBalanceManager->UpdateAirflowNetworkMyOneTimeFlag1) {
            if ((!state.dataAirflowNetwork->VAVSystem) && state.dataGlobal->DisplayExtraWarnings) {
                WriteFlag = false;
                for (i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfLinks; ++i) {
                    Node1 = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
                    Node2 = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
                    if (state.dataAirflowNetwork->AirflowNetworkNodeData(Node1).EPlusTypeNum == iEPlusNodeType::SPI ||
                        state.dataAirflowNetwork->AirflowNetworkNodeData(Node2).EPlusTypeNum == iEPlusNodeType::SPO ||
                        state.dataAirflowNetwork->AirflowNetworkNodeData(Node2).EPlusTypeNum == iEPlusNodeType::ZIN) {
                        if (state.dataAirflowNetwork->AirflowNetworkNodeData(Node1).EPlusTypeNum == iEPlusNodeType::SPI) {
                            Node3 = Node1;
                        } else {
                            Node3 = Node2;
                        }
                        if (state.dataAirflowNetwork->AirflowNetworkNodeData(Node2).EPlusTypeNum == iEPlusNodeType::ZIN) {
                            if (state.dataAirflowNetwork->AirflowNetworkCompData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).CompNum)
                                    .EPlusTypeNum == iEPlusComponentType::Unassigned)
                                continue;
                        }
                        NodeMass = Node(state.dataAirflowNetwork->AirflowNetworkNodeData(Node3).EPlusNodeNum).MassFlowRate;
                        AFNMass = state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW;
                        if (NodeMass > 0.0 && AFNMass > NodeMass + 0.01) {
                            ShowWarningError(state,
                                             "The mass flow rate difference is found between System Node = '" +
                                                 state.dataLoopNodes->NodeID(state.dataAirflowNetwork->AirflowNetworkNodeData(Node3).EPlusNodeNum) +
                                                 "' and AFN Link = '" + state.dataAirflowNetwork->AirflowNetworkLinkageData(i).Name + "'.");
                            ShowContinueError(state,
                                              format("The system node max mass flow rate = {:.3R} kg/s. The AFN node mass flow rate = {:.3R} kg.s.",
                                                     NodeMass,
                                                     AFNMass));
                            WriteFlag = true;
                        }
                    }
                }
                state.dataAirflowNetworkBalanceManager->UpdateAirflowNetworkMyOneTimeFlag1 = false;
                if (WriteFlag) {
                    ShowWarningError(state,
                                     "Please adjust the rate of Maximum Air Flow Rate field in the terminal objects or duct pressure resistance.");
                }
            } else {
                state.dataAirflowNetworkBalanceManager->UpdateAirflowNetworkMyOneTimeFlag1 = false;
            }
        }

        // Assign airflows to EPLus nodes
        for (i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfLinks; ++i) {
            if (state.dataAirflowNetwork->AirflowNetworkCompData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).CompNum).CompTypeNum ==
                    iComponentTypeNum::DWC ||
                state.dataAirflowNetwork->AirflowNetworkLinkageData(i).VAVTermDamper) {
                // Exclude envelope leakage Crack element
                Node1 = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
                Node2 = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];

                j = state.dataAirflowNetwork->AirflowNetworkNodeData(Node1).EPlusNodeNum;
                if (j > 0 && state.dataAirflowNetwork->AirflowNetworkNodeData(Node1).EPlusZoneNum == 0) {
                    Node(j).MassFlowRate =
                        state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW *
                        state.dataAirflowNetworkBalanceManager->LoopPartLoadRatio(state.dataAirflowNetwork->AirflowNetworkNodeData(Node1).AirLoopNum);
                    if (!state.dataAirflowNetworkBalanceManager->LoopOnOffFlag(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).AirLoopNum))
                        Node(j).MassFlowRate = 0.0;
                    if (!(state.dataAirflowNetwork->AirflowNetworkNodeData(Node1).EPlusTypeNum == iEPlusNodeType::DIN ||
                          state.dataAirflowNetwork->AirflowNetworkNodeData(Node1).EPlusTypeNum == iEPlusNodeType::DOU)) {
                        Node(j).MassFlowRateMaxAvail = state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW *
                                                       state.dataAirflowNetworkBalanceManager->LoopPartLoadRatio(
                                                           state.dataAirflowNetwork->AirflowNetworkNodeData(Node1).AirLoopNum);
                        Node(j).MassFlowRateMax = state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW;
                    }
                }

                j = state.dataAirflowNetwork->AirflowNetworkNodeData(Node2).EPlusNodeNum;
                if (j > 0) {
                    Node(j).MassFlowRate =
                        state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW *
                        state.dataAirflowNetworkBalanceManager->LoopPartLoadRatio(state.dataAirflowNetwork->AirflowNetworkNodeData(Node2).AirLoopNum);
                    if (!state.dataAirflowNetworkBalanceManager->LoopOnOffFlag(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).AirLoopNum))
                        Node(j).MassFlowRate = 0.0;
                    if (!(state.dataAirflowNetwork->AirflowNetworkNodeData(Node2).EPlusTypeNum == iEPlusNodeType::DIN ||
                          state.dataAirflowNetwork->AirflowNetworkNodeData(Node2).EPlusTypeNum == iEPlusNodeType::DOU)) {
                        Node(j).MassFlowRateMaxAvail = state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW *
                                                       state.dataAirflowNetworkBalanceManager->LoopPartLoadRatio(
                                                           state.dataAirflowNetwork->AirflowNetworkNodeData(Node2).AirLoopNum);
                        Node(j).MassFlowRateMax = state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW;
                    }
                }
            }
        }

        // Assign AirflowNetwork nodal values to Node array
        for (i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfNodes; ++i) {
            j = state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusNodeNum;
            if (j > 0) {
                Node(j).Enthalpy =
                    PsyHFnTdbW(state.dataAirflowNetwork->AirflowNetworkNodeSimu(i).TZ, state.dataAirflowNetwork->AirflowNetworkNodeSimu(i).WZ);
                Node(j).Temp = state.dataAirflowNetwork->AirflowNetworkNodeSimu(i).TZ;
                Node(j).HumRat = state.dataAirflowNetwork->AirflowNetworkNodeSimu(i).WZ;
                if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
                    Node(j).CO2 = state.dataAirflowNetwork->AirflowNetworkNodeSimu(i).CO2Z;
                }
                if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
                    Node(j).GenContam = state.dataAirflowNetwork->AirflowNetworkNodeSimu(i).GCZ;
                }
            }
        }

        // Calculate sensible loads from forced air flow
        for (i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfLinks; ++i) {
            Node1 = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
            Node2 = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
            CpAir = PsyCpAirFnW(
                (state.dataAirflowNetwork->AirflowNetworkNodeSimu(Node1).WZ + state.dataAirflowNetwork->AirflowNetworkNodeSimu(Node2).WZ) / 2.0);
            // Calculate sensible loads from duct conduction losses and loads from duct radiation
            if (state.dataAirflowNetwork->AirflowNetworkLinkageData(i).ZoneNum > 0 &&
                state.dataAirflowNetwork->AirflowNetworkCompData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).CompNum).CompTypeNum ==
                    iComponentTypeNum::DWC) {
                Qsen = state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW * CpAir *
                       (state.dataAirflowNetwork->AirflowNetworkNodeSimu(Node2).TZ - state.dataAirflowNetwork->AirflowNetworkNodeSimu(Node1).TZ);
                if (state.dataAirflowNetwork->AirflowNetworkLinkageData(i).LinkageViewFactorObjectNum != 0) {
                    auto &DuctRadObj(state.dataAirflowNetwork->AirflowNetworkLinkageViewFactorData(
                        state.dataAirflowNetwork->AirflowNetworkLinkageData(i).LinkageViewFactorObjectNum));
                    Qsen -= DuctRadObj.QRad;
                    state.dataAirflowNetworkBalanceManager->exchangeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).ZoneNum).RadGain -=
                        DuctRadObj.QRad;
                }
                // When the Airloop is shut off, no duct sensible losses
                if (!state.dataAirflowNetworkBalanceManager->LoopOnOffFlag(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).AirLoopNum))
                    Qsen = 0.0;
                state.dataAirflowNetworkBalanceManager->exchangeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).ZoneNum).CondSen -= Qsen;
            }
            // Calculate sensible leakage losses
            if (state.dataAirflowNetwork->AirflowNetworkCompData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).CompNum).CompTypeNum ==
                    iComponentTypeNum::PLR ||
                state.dataAirflowNetwork->AirflowNetworkCompData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).CompNum).CompTypeNum ==
                    iComponentTypeNum::ELR) {
                // Calculate supply leak sensible losses
                if ((state.dataAirflowNetwork->AirflowNetworkNodeData(Node2).EPlusZoneNum > 0) &&
                    (state.dataAirflowNetwork->AirflowNetworkNodeData(Node1).EPlusNodeNum == 0) &&
                    (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW > 0.0)) {
                    ZN2 = state.dataAirflowNetwork->AirflowNetworkNodeData(Node2).EPlusZoneNum;
                    Qsen = state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW * CpAir *
                           (state.dataAirflowNetwork->AirflowNetworkNodeSimu(Node1).TZ - state.dataAirflowNetwork->AirflowNetworkNodeSimu(Node2).TZ);
                    if (!state.dataAirflowNetworkBalanceManager->LoopOnOffFlag(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).AirLoopNum))
                        Qsen = 0.0;
                    state.dataAirflowNetworkBalanceManager->exchangeData(ZN2).LeakSen += Qsen;
                }
                if ((state.dataAirflowNetwork->AirflowNetworkNodeData(Node1).EPlusZoneNum > 0) &&
                    (state.dataAirflowNetwork->AirflowNetworkNodeData(Node2).EPlusNodeNum == 0) &&
                    (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2 > 0.0)) {
                    ZN1 = state.dataAirflowNetwork->AirflowNetworkNodeData(Node1).EPlusZoneNum;
                    Qsen = state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2 * CpAir *
                           (state.dataAirflowNetwork->AirflowNetworkNodeSimu(Node2).TZ - state.dataAirflowNetwork->AirflowNetworkNodeSimu(Node1).TZ);
                    if (!state.dataAirflowNetworkBalanceManager->LoopOnOffFlag(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).AirLoopNum))
                        Qsen = 0.0;
                    state.dataAirflowNetworkBalanceManager->exchangeData(ZN1).LeakSen += Qsen;
                }
            }
        }

        // Calculate latent loads from forced air flow
        for (i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfLinks; ++i) {
            Node1 = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
            Node2 = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
            // Calculate latent loads from duct conduction losses
            if (state.dataAirflowNetwork->AirflowNetworkLinkageData(i).ZoneNum > 0 &&
                state.dataAirflowNetwork->AirflowNetworkCompData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).CompNum).CompTypeNum ==
                    iComponentTypeNum::DWC) {
                Qlat = state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW *
                       (state.dataAirflowNetwork->AirflowNetworkNodeSimu(Node2).WZ - state.dataAirflowNetwork->AirflowNetworkNodeSimu(Node1).WZ);
                if (!state.dataAirflowNetworkBalanceManager->LoopOnOffFlag(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).AirLoopNum))
                    Qlat = 0.0;
                state.dataAirflowNetworkBalanceManager->exchangeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).ZoneNum).DiffLat -= Qlat;
            }
            // Calculate latent leakage losses
            if (state.dataAirflowNetwork->AirflowNetworkCompData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).CompNum).CompTypeNum ==
                    iComponentTypeNum::PLR ||
                state.dataAirflowNetwork->AirflowNetworkCompData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).CompNum).CompTypeNum ==
                    iComponentTypeNum::ELR) {
                // Calculate supply leak latent losses
                if ((state.dataAirflowNetwork->AirflowNetworkNodeData(Node2).EPlusZoneNum > 0) &&
                    (state.dataAirflowNetwork->AirflowNetworkNodeData(Node1).EPlusNodeNum == 0) &&
                    (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW > 0.0)) {
                    ZN2 = state.dataAirflowNetwork->AirflowNetworkNodeData(Node2).EPlusZoneNum;
                    Qlat = state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW *
                           (state.dataAirflowNetwork->AirflowNetworkNodeSimu(Node1).WZ - state.dataAirflowNetwork->AirflowNetworkNodeSimu(Node2).WZ);
                    if (!state.dataAirflowNetworkBalanceManager->LoopOnOffFlag(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).AirLoopNum))
                        Qlat = 0.0;
                    state.dataAirflowNetworkBalanceManager->exchangeData(ZN2).LeakLat += Qlat;
                    if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
                        state.dataAirflowNetworkBalanceManager->exchangeData(ZN2).TotalCO2 +=
                            state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW * (state.dataAirflowNetwork->AirflowNetworkNodeSimu(Node1).CO2Z -
                                                                                        state.dataAirflowNetwork->AirflowNetworkNodeSimu(Node2).CO2Z);
                    }
                    if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
                        state.dataAirflowNetworkBalanceManager->exchangeData(ZN2).TotalGC +=
                            state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW * (state.dataAirflowNetwork->AirflowNetworkNodeSimu(Node1).GCZ -
                                                                                        state.dataAirflowNetwork->AirflowNetworkNodeSimu(Node2).GCZ);
                    }
                }
                if ((state.dataAirflowNetwork->AirflowNetworkNodeData(Node1).EPlusZoneNum > 0) &&
                    (state.dataAirflowNetwork->AirflowNetworkNodeData(Node2).EPlusNodeNum == 0) &&
                    (state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2 > 0.0)) {
                    ZN1 = state.dataAirflowNetwork->AirflowNetworkNodeData(Node1).EPlusZoneNum;
                    Qlat = state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2 *
                           (state.dataAirflowNetwork->AirflowNetworkNodeSimu(Node2).WZ - state.dataAirflowNetwork->AirflowNetworkNodeSimu(Node1).WZ);
                    if (!state.dataAirflowNetworkBalanceManager->LoopOnOffFlag(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).AirLoopNum))
                        Qlat = 0.0;
                    state.dataAirflowNetworkBalanceManager->exchangeData(ZN1).LeakLat += Qlat;
                    if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
                        state.dataAirflowNetworkBalanceManager->exchangeData(ZN1).TotalCO2 +=
                            state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2 *
                            (state.dataAirflowNetwork->AirflowNetworkNodeSimu(Node2).CO2Z -
                             state.dataAirflowNetwork->AirflowNetworkNodeSimu(Node1).CO2Z);
                    }
                    if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
                        state.dataAirflowNetworkBalanceManager->exchangeData(ZN1).TotalGC +=
                            state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2 * (state.dataAirflowNetwork->AirflowNetworkNodeSimu(Node2).GCZ -
                                                                                         state.dataAirflowNetwork->AirflowNetworkNodeSimu(Node1).GCZ);
                    }
                }
            }
        }

        // Sum all the loads
        for (i = 1; i <= state.dataGlobal->NumOfZones; ++i) {
            state.dataAirflowNetworkBalanceManager->exchangeData(i).TotalSen = state.dataAirflowNetworkBalanceManager->exchangeData(i).LeakSen +
                                                                               state.dataAirflowNetworkBalanceManager->exchangeData(i).CondSen +
                                                                               state.dataAirflowNetworkBalanceManager->exchangeData(i).RadGain;
            state.dataAirflowNetworkBalanceManager->exchangeData(i).TotalLat =
                state.dataAirflowNetworkBalanceManager->exchangeData(i).LeakLat + state.dataAirflowNetworkBalanceManager->exchangeData(i).DiffLat;
        }

        // Simple ONOFF fan
        for (AirLoopNum = 1; AirLoopNum <= NumPrimaryAirSys; ++AirLoopNum) {
            for (FanNum = 1; FanNum <= state.dataAirflowNetworkBalanceManager->DisSysNumOfCVFs; ++FanNum) {
                if (state.dataAirflowNetwork->DisSysCompCVFData(FanNum).AirLoopNum == AirLoopNum) break;
            }
            if (state.dataAirflowNetwork->DisSysCompCVFData(FanNum).FanTypeNum == FanType_SimpleOnOff && OnOffFanRunTimeFraction < 1.0) {
                for (i = 1; i <= state.dataGlobal->NumOfZones; ++i) {
                    state.dataAirflowNetworkBalanceManager->exchangeData(i).MultiZoneSen *= OnOffFanRunTimeFraction;
                    state.dataAirflowNetworkBalanceManager->exchangeData(i).MultiZoneLat *= OnOffFanRunTimeFraction;
                    state.dataAirflowNetworkBalanceManager->exchangeData(i).LeakSen *= OnOffFanRunTimeFraction;
                    state.dataAirflowNetworkBalanceManager->exchangeData(i).LeakLat *= OnOffFanRunTimeFraction;
                    state.dataAirflowNetworkBalanceManager->exchangeData(i).CondSen *= OnOffFanRunTimeFraction;
                    state.dataAirflowNetworkBalanceManager->exchangeData(i).DiffLat *= OnOffFanRunTimeFraction;
                    state.dataAirflowNetworkBalanceManager->exchangeData(i).RadGain *= OnOffFanRunTimeFraction;
                    state.dataAirflowNetworkBalanceManager->exchangeData(i).TotalSen *= OnOffFanRunTimeFraction;
                    state.dataAirflowNetworkBalanceManager->exchangeData(i).TotalLat *= OnOffFanRunTimeFraction;
                    state.dataAirflowNetworkBalanceManager->exchangeData(i).SumMCp *= OnOffFanRunTimeFraction;
                    state.dataAirflowNetworkBalanceManager->exchangeData(i).SumMCpT *= OnOffFanRunTimeFraction;
                    state.dataAirflowNetworkBalanceManager->exchangeData(i).SumMVCp *= OnOffFanRunTimeFraction;
                    state.dataAirflowNetworkBalanceManager->exchangeData(i).SumMVCpT *= OnOffFanRunTimeFraction;
                    state.dataAirflowNetworkBalanceManager->exchangeData(i).SumMHr *= OnOffFanRunTimeFraction;
                    state.dataAirflowNetworkBalanceManager->exchangeData(i).SumMHrW *= OnOffFanRunTimeFraction;
                    state.dataAirflowNetworkBalanceManager->exchangeData(i).SumMMCp *= OnOffFanRunTimeFraction;
                    state.dataAirflowNetworkBalanceManager->exchangeData(i).SumMMCpT *= OnOffFanRunTimeFraction;
                    state.dataAirflowNetworkBalanceManager->exchangeData(i).SumMMHr *= OnOffFanRunTimeFraction;
                    state.dataAirflowNetworkBalanceManager->exchangeData(i).SumMMHrW *= OnOffFanRunTimeFraction;
                    if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
                        state.dataAirflowNetworkBalanceManager->exchangeData(i).SumMHrCO *= OnOffFanRunTimeFraction;
                        state.dataAirflowNetworkBalanceManager->exchangeData(i).SumMMHrCO *= OnOffFanRunTimeFraction;
                    }
                    if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
                        state.dataAirflowNetworkBalanceManager->exchangeData(i).SumMHrGC *= OnOffFanRunTimeFraction;
                        state.dataAirflowNetworkBalanceManager->exchangeData(i).SumMMHrGC *= OnOffFanRunTimeFraction;
                    }
                }
                if (state.dataAirLoop->AirLoopAFNInfo(AirLoopNum).LoopFanOperationMode == CycFanCycCoil) {
                    for (i = 1; i <= state.dataGlobal->NumOfZones; ++i) {
                        state.dataAirflowNetworkBalanceManager->exchangeData(i).SumMCp +=
                            state.dataAirflowNetworkBalanceManager->multiExchangeData(i).SumMCp * (1.0 - OnOffFanRunTimeFraction);
                        state.dataAirflowNetworkBalanceManager->exchangeData(i).SumMCpT +=
                            state.dataAirflowNetworkBalanceManager->multiExchangeData(i).SumMCpT * (1.0 - OnOffFanRunTimeFraction);
                        state.dataAirflowNetworkBalanceManager->exchangeData(i).SumMVCp +=
                            state.dataAirflowNetworkBalanceManager->multiExchangeData(i).SumMVCp * (1.0 - OnOffFanRunTimeFraction);
                        state.dataAirflowNetworkBalanceManager->exchangeData(i).SumMVCpT +=
                            state.dataAirflowNetworkBalanceManager->multiExchangeData(i).SumMVCpT * (1.0 - OnOffFanRunTimeFraction);
                        state.dataAirflowNetworkBalanceManager->exchangeData(i).SumMHr +=
                            state.dataAirflowNetworkBalanceManager->multiExchangeData(i).SumMHr * (1.0 - OnOffFanRunTimeFraction);
                        state.dataAirflowNetworkBalanceManager->exchangeData(i).SumMHrW +=
                            state.dataAirflowNetworkBalanceManager->multiExchangeData(i).SumMHrW * (1.0 - OnOffFanRunTimeFraction);
                        state.dataAirflowNetworkBalanceManager->exchangeData(i).SumMMCp +=
                            state.dataAirflowNetworkBalanceManager->multiExchangeData(i).SumMMCp * (1.0 - OnOffFanRunTimeFraction);
                        state.dataAirflowNetworkBalanceManager->exchangeData(i).SumMMCpT +=
                            state.dataAirflowNetworkBalanceManager->multiExchangeData(i).SumMMCpT * (1.0 - OnOffFanRunTimeFraction);
                        state.dataAirflowNetworkBalanceManager->exchangeData(i).SumMMHr +=
                            state.dataAirflowNetworkBalanceManager->multiExchangeData(i).SumMMHr * (1.0 - OnOffFanRunTimeFraction);
                        state.dataAirflowNetworkBalanceManager->exchangeData(i).SumMMHrW +=
                            state.dataAirflowNetworkBalanceManager->multiExchangeData(i).SumMMHrW * (1.0 - OnOffFanRunTimeFraction);
                        if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
                            state.dataAirflowNetworkBalanceManager->exchangeData(i).SumMHrCO +=
                                state.dataAirflowNetworkBalanceManager->multiExchangeData(i).SumMHrCO * (1.0 - OnOffFanRunTimeFraction);
                            state.dataAirflowNetworkBalanceManager->exchangeData(i).SumMMHrCO +=
                                state.dataAirflowNetworkBalanceManager->multiExchangeData(i).SumMMHrCO * (1.0 - OnOffFanRunTimeFraction);
                        }
                        if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
                            state.dataAirflowNetworkBalanceManager->exchangeData(i).SumMHrGC +=
                                state.dataAirflowNetworkBalanceManager->multiExchangeData(i).SumMHrGC * (1.0 - OnOffFanRunTimeFraction);
                            state.dataAirflowNetworkBalanceManager->exchangeData(i).SumMMHrGC +=
                                state.dataAirflowNetworkBalanceManager->multiExchangeData(i).SumMMHrGC * (1.0 - OnOffFanRunTimeFraction);
                        }
                    }
                }
            }

            if (state.dataAirflowNetwork->DisSysCompCVFData(FanNum).FanTypeNum == FanType_SimpleOnOff) {
                for (i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfZones; ++i) {
                    if (state.dataAirflowNetwork->AirflowNetworkNodeData(i).AirLoopNum == AirLoopNum) {
                        state.dataAirflowNetworkBalanceManager->nodeReport(i).PZ =
                            state.dataAirflowNetwork->AirflowNetworkNodeSimu(i).PZ *
                                state.dataAirflowNetworkBalanceManager->LoopPartLoadRatio(AirLoopNum) +
                            state.dataAirflowNetworkBalanceManager->nodeReport(i).PZOFF *
                                (1.0 - state.dataAirflowNetworkBalanceManager->LoopPartLoadRatio(AirLoopNum));
                        state.dataAirflowNetworkBalanceManager->nodeReport(i).PZON = state.dataAirflowNetwork->AirflowNetworkNodeSimu(i).PZ;
                    }
                }
                for (i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces; ++i) {
                    PartLoadRatio = MaxPartLoadRatio;
                    for (j = 1; j <= state.dataAirflowNetwork->AirflowNetworkNumOfZones; ++j) {
                        if (state.dataAirflowNetwork->MultizoneZoneData(j).ZoneNum == state.dataAirflowNetwork->MultizoneSurfaceData(i).ZonePtr) {
                            if (state.dataAirflowNetwork->AirflowNetworkNodeData(j).AirLoopNum == AirLoopNum) {
                                PartLoadRatio = state.dataAirflowNetworkBalanceManager->LoopPartLoadRatio(AirLoopNum);
                                break;
                            }
                        }
                    }
                    state.dataAirflowNetworkBalanceManager->linkReport1(i).FLOW =
                        state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW * PartLoadRatio +
                        state.dataAirflowNetworkBalanceManager->linkReport1(i).FLOWOFF * (1.0 - PartLoadRatio);
                    state.dataAirflowNetworkBalanceManager->linkReport1(i).FLOW2 =
                        state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2 * PartLoadRatio +
                        state.dataAirflowNetworkBalanceManager->linkReport1(i).FLOW2OFF * (1.0 - PartLoadRatio);
                    state.dataAirflowNetworkBalanceManager->linkReport1(i).VolFLOW =
                        state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).VolFLOW * PartLoadRatio +
                        state.dataAirflowNetworkBalanceManager->linkReport1(i).VolFLOWOFF * (1.0 - PartLoadRatio);
                    state.dataAirflowNetworkBalanceManager->linkReport1(i).VolFLOW2 =
                        state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).VolFLOW2 * PartLoadRatio +
                        state.dataAirflowNetworkBalanceManager->linkReport1(i).VolFLOW2OFF * (1.0 - PartLoadRatio);
                    state.dataAirflowNetworkBalanceManager->linkReport1(i).DP =
                        state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).DP * PartLoadRatio +
                        state.dataAirflowNetworkBalanceManager->linkReport1(i).DPOFF * (1.0 - PartLoadRatio);
                    state.dataAirflowNetworkBalanceManager->linkReport1(i).DPON = state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).DP;
                }
            }
        }

        // Save values
        for (i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfNodes; ++i) {
            state.dataAirflowNetwork->AirflowNetworkNodeSimu(i).TZlast = state.dataAirflowNetwork->AirflowNetworkNodeSimu(i).TZ;
            state.dataAirflowNetwork->AirflowNetworkNodeSimu(i).WZlast = state.dataAirflowNetwork->AirflowNetworkNodeSimu(i).WZ;
            if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
                state.dataAirflowNetwork->AirflowNetworkNodeSimu(i).CO2Zlast = state.dataAirflowNetwork->AirflowNetworkNodeSimu(i).CO2Z;
            }
            if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
                state.dataAirflowNetwork->AirflowNetworkNodeSimu(i).GCZlast = state.dataAirflowNetwork->AirflowNetworkNodeSimu(i).GCZ;
            }
        }
    }

    void AirflowNetworkVentingControl(EnergyPlusData &state,
                                      int const i,       // AirflowNetwork surface number
                                      Real64 &OpenFactor // Window or door opening factor (used to calculate airflow)
    )
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Winkelmann
        //       DATE WRITTEN   April 2003
        //       MODIFIED       Feb 2004, FCW: allow venting control of interior window/door
        //       MODIFIED       Nov. 2005, LG: to fit the requirement for AirflowNetwork Model
        //       RE-ENGINEERED

        // PURPOSE OF THIS SUBROUTINE:
        // Determines the venting opening factor for an exterior or interior window or door
        // as determined by the venting control method.

        // Using/Aliasing
        using ScheduleManager::GetCurrentScheduleValue;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 VentTemp;                // Venting temperature (C)
        Real64 ZoneAirEnthalpy;         // Enthalpy of zone air (J/kg)
        Real64 OpenFactorMult;          // Window/door opening modulation multiplier on venting open factor
        Real64 DelTemp;                 // Inside-outside air temperature difference (K)
        Real64 DelEnthal;               // Inside-outside air enthalpy difference (J/kg)
        int IZ;                         // AirflowNetwork zone number
        int ZoneNum;                    // EnergyPlus zone number
        int SurfNum;                    // Heat transfer surface number
        Real64 LimValVentOpenFacMult;   // Limiting value of venting opening factor multiplier
        Real64 LowerValInOutTempDiff;   // Lower value of inside/outside temperature difference for opening factor modulation
        Real64 UpperValInOutTempDiff;   // Upper value of inside/outside temperature difference for opening factor modulation
        Real64 LowerValInOutEnthalDiff; // Lower value of inside/outside enthalpy difference for opening factor modulation
        Real64 UpperValInOutEnthalDiff; // Upper value of inside/outside enthalpy difference for opening factor modulation
        bool VentingAllowed;            // True if venting schedule allows venting
        int VentCtrlNum;                // Venting control strategy 1: Temperature control; 2: Enthalpy control
        Real64 VentingSchVal;           // Current time step value of venting schedule
        Real64 Tamb;                    // Outdoor dry bulb temperature at surface centroid height
        int PeopleInd;

        if (state.dataAirflowNetwork->MultizoneSurfaceData(i).EMSOpenFactorActuated) { // EMS sets value to use
            OpenFactor = state.dataAirflowNetwork->MultizoneSurfaceData(i).EMSOpenFactor;
            SurfNum = state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum;
            if (state.dataAirflowNetwork->MultizoneSurfaceData(i).Factor > 0.0) {
                state.dataSurface->SurfWinVentingOpenFactorMultRep(SurfNum) = OpenFactor / state.dataAirflowNetwork->MultizoneSurfaceData(i).Factor;
            } else {
                state.dataSurface->SurfWinVentingOpenFactorMultRep(SurfNum) = OpenFactor;
            }
            return;
        }

        SurfNum = state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum;

        state.dataSurface->SurfWinVentingOpenFactorMultRep(SurfNum) = -1.0;

        // Get venting temperature and venting strategy for exterior window or door
        // and determine whether venting is allowed

        state.dataSurface->SurfWinVentingAvailabilityRep(SurfNum) = 1.0;
        VentingAllowed = true;
        IZ = state.dataAirflowNetwork->MultizoneSurfaceData(i).NodeNums[0];
        // Revise for RoomAirflowNetwork model
        if (state.dataAirflowNetwork->MultizoneSurfaceData(i).RAFNflag) IZ = state.dataAirflowNetwork->MultizoneSurfaceData(i).ZonePtr;
        ZoneNum = state.dataAirflowNetwork->MultizoneZoneData(IZ).ZoneNum;

        // Note in the following that individual venting control for a window/door takes
        // precedence over zone-level control
        if (state.dataAirflowNetwork->MultizoneSurfaceData(i).IndVentControl) {
            VentTemp = GetCurrentScheduleValue(state, state.dataAirflowNetwork->MultizoneSurfaceData(i).VentSchNum);
            VentCtrlNum = state.dataAirflowNetwork->MultizoneSurfaceData(i).VentSurfCtrNum;
            if (state.dataAirflowNetwork->MultizoneSurfaceData(i).VentingSchNum > 0) {
                VentingSchVal = GetCurrentScheduleValue(state, state.dataAirflowNetwork->MultizoneSurfaceData(i).VentingSchNum);
                if (VentingSchVal <= 0.0) {
                    VentingAllowed = false;
                    state.dataSurface->SurfWinVentingAvailabilityRep(SurfNum) = 0.0;
                }
            }
        } else {
            // Zone level only by Gu on Nov. 8, 2005
            VentTemp = GetCurrentScheduleValue(state, state.dataAirflowNetwork->MultizoneZoneData(IZ).VentSchNum);
            VentCtrlNum = state.dataAirflowNetwork->MultizoneZoneData(IZ).VentCtrNum;
            if (state.dataAirflowNetwork->MultizoneZoneData(IZ).VentingSchNum > 0) {
                VentingSchVal = GetCurrentScheduleValue(state, state.dataAirflowNetwork->MultizoneZoneData(IZ).VentingSchNum);
                if (VentingSchVal <= 0.0) {
                    VentingAllowed = false;
                    state.dataSurface->SurfWinVentingAvailabilityRep(SurfNum) = 0.0;
                }
            }
        }

        state.dataSurface->SurfWinInsideTempForVentingRep(SurfNum) = VentTemp;
        OpenFactor = 0.0;

        // Venting based on inside-outside air temperature difference

        if ((VentCtrlNum == VentControlType::Temp || VentCtrlNum == VentControlType::AdjTemp) && VentingAllowed) {
            Tamb = state.dataSurface->SurfOutDryBulbTemp(SurfNum);
            // Check whether this surface is an interior wall or not. If Yes, use adjacent zone conditions
            if (VentCtrlNum == VentControlType::AdjTemp && state.dataAirflowNetwork->MultizoneSurfaceData(i).IndVentControl) {
                Tamb = state.dataAirflowNetwork->ANZT(
                    state.dataAirflowNetwork->MultizoneZoneData(state.dataAirflowNetwork->MultizoneSurfaceData(i).NodeNums[1]).ZoneNum);
            }
            if (state.dataAirflowNetwork->ANZT(ZoneNum) > Tamb && state.dataAirflowNetwork->ANZT(ZoneNum) > VentTemp) {
                OpenFactor = state.dataAirflowNetwork->MultizoneSurfaceData(i).Factor;
                state.dataSurface->SurfWinVentingOpenFactorMultRep(SurfNum) = 1.0;
                // Modulation of OpenFactor
                if (state.dataAirflowNetwork->MultizoneSurfaceData(i).IndVentControl) {
                    LimValVentOpenFacMult = state.dataAirflowNetwork->MultizoneSurfaceData(i).ModulateFactor;
                    LowerValInOutTempDiff = state.dataAirflowNetwork->MultizoneSurfaceData(i).LowValueTemp;
                    UpperValInOutTempDiff = state.dataAirflowNetwork->MultizoneSurfaceData(i).UpValueTemp;
                } else {
                    LimValVentOpenFacMult = state.dataAirflowNetwork->MultizoneZoneData(IZ).OpenFactor;
                    LowerValInOutTempDiff = state.dataAirflowNetwork->MultizoneZoneData(IZ).LowValueTemp;
                    UpperValInOutTempDiff = state.dataAirflowNetwork->MultizoneZoneData(IZ).UpValueTemp;
                }
                if (LimValVentOpenFacMult != 1.0) {
                    DelTemp = state.dataAirflowNetwork->ANZT(ZoneNum) - Tamb;
                    if (DelTemp <= LowerValInOutTempDiff) {
                        OpenFactorMult = 1.0;
                    } else if (DelTemp >= UpperValInOutTempDiff) {
                        OpenFactorMult = LimValVentOpenFacMult;
                    } else {
                        OpenFactorMult =
                            LimValVentOpenFacMult +
                            ((UpperValInOutTempDiff - DelTemp) / (UpperValInOutTempDiff - LowerValInOutTempDiff)) * (1 - LimValVentOpenFacMult);
                    }
                    OpenFactor *= OpenFactorMult;
                    state.dataSurface->SurfWinVentingOpenFactorMultRep(SurfNum) = OpenFactorMult;
                }
            } else {
                OpenFactor = 0.0;
                state.dataSurface->SurfWinVentingOpenFactorMultRep(SurfNum) = -1.0;
            }
        }

        // Venting based on inside-outside air enthalpy difference

        if ((VentCtrlNum == VentControlType::Enth || VentCtrlNum == VentControlType::AdjEnth) && VentingAllowed) {
            ZoneAirEnthalpy = PsyHFnTdbW(state.dataAirflowNetwork->ANZT(ZoneNum), state.dataAirflowNetwork->ANZW(ZoneNum));
            // Check whether this surface is an interior wall or not. If Yes, use adjacent zone conditions
            if (VentCtrlNum == VentControlType::AdjEnth && state.dataAirflowNetwork->MultizoneSurfaceData(i).IndVentControl) {
                state.dataEnvrn->OutEnthalpy = PsyHFnTdbW(
                    state.dataAirflowNetwork->ANZT(
                        state.dataAirflowNetwork->MultizoneZoneData(state.dataAirflowNetwork->MultizoneSurfaceData(i).NodeNums[1]).ZoneNum),
                    state.dataAirflowNetwork->ANZW(
                        state.dataAirflowNetwork->MultizoneZoneData(state.dataAirflowNetwork->MultizoneSurfaceData(i).NodeNums[1]).ZoneNum));
            }
            if (ZoneAirEnthalpy > state.dataEnvrn->OutEnthalpy && state.dataAirflowNetwork->ANZT(ZoneNum) > VentTemp) {
                OpenFactor = state.dataAirflowNetwork->MultizoneSurfaceData(i).Factor;
                // Modulation of OpenFactor
                if (state.dataAirflowNetwork->MultizoneSurfaceData(i).IndVentControl) {
                    LimValVentOpenFacMult = state.dataAirflowNetwork->MultizoneSurfaceData(i).ModulateFactor;
                    LowerValInOutEnthalDiff = state.dataAirflowNetwork->MultizoneSurfaceData(i).LowValueEnth;
                    UpperValInOutEnthalDiff = state.dataAirflowNetwork->MultizoneSurfaceData(i).UpValueEnth;
                } else {
                    LimValVentOpenFacMult = state.dataAirflowNetwork->MultizoneZoneData(IZ).OpenFactor;
                    LowerValInOutEnthalDiff = state.dataAirflowNetwork->MultizoneZoneData(IZ).LowValueEnth;
                    UpperValInOutEnthalDiff = state.dataAirflowNetwork->MultizoneZoneData(IZ).UpValueEnth;
                }
                state.dataSurface->SurfWinVentingOpenFactorMultRep(SurfNum) = 1.0;

                if (LimValVentOpenFacMult != 1.0) {
                    DelEnthal = ZoneAirEnthalpy - state.dataEnvrn->OutEnthalpy;
                    if (DelEnthal <= LowerValInOutEnthalDiff) {
                        OpenFactorMult = 1.0;
                    } else if (DelEnthal >= UpperValInOutEnthalDiff) {
                        OpenFactorMult = LimValVentOpenFacMult;
                    } else {
                        OpenFactorMult =
                            LimValVentOpenFacMult + ((UpperValInOutEnthalDiff - DelEnthal) / (UpperValInOutEnthalDiff - LowerValInOutEnthalDiff)) *
                                                        (1 - LimValVentOpenFacMult);
                    }
                    OpenFactor *= OpenFactorMult;
                    state.dataSurface->SurfWinVentingOpenFactorMultRep(SurfNum) = OpenFactorMult;
                }
            } else {
                OpenFactor = 0.0;
                state.dataSurface->SurfWinVentingOpenFactorMultRep(SurfNum) = -1.0;
            }
        }

        // Constant venting (opening factor as specified in IDF) - C-PH - added by Philip Haves 3/8/01
        // subject to venting availability

        if (VentCtrlNum == VentControlType::Const && VentingAllowed) { // Constant
            OpenFactor = state.dataAirflowNetwork->MultizoneSurfaceData(i).Factor;
            state.dataSurface->SurfWinVentingOpenFactorMultRep(SurfNum) = 1.0;
        }

        if (VentCtrlNum == VentControlType::ASH55) {
            if (VentingAllowed && (!state.dataGlobal->BeginEnvrnFlag) && (!state.dataGlobal->WarmupFlag)) {
                PeopleInd = state.dataAirflowNetwork->MultizoneZoneData(IZ).ASH55PeopleInd;
                if (PeopleInd > 0 && state.dataThermalComforts->ThermalComfortData(PeopleInd).ThermalComfortAdaptiveASH5590 != -1) {
                    if (state.dataThermalComforts->ThermalComfortData(PeopleInd).ThermalComfortOpTemp >
                        state.dataThermalComforts->ThermalComfortData(PeopleInd).TComfASH55) {
                        OpenFactor = state.dataAirflowNetwork->MultizoneSurfaceData(i).Factor;
                        state.dataSurface->SurfWinVentingOpenFactorMultRep(SurfNum) = 1.0;
                    } else {
                        OpenFactor = 0.0;
                    }
                } else {
                    OpenFactor = 0.0;
                }
            } else {
                OpenFactor = 0.0;
            }
        }

        if (VentCtrlNum == VentControlType::CEN15251) {
            if (VentingAllowed && (!state.dataGlobal->BeginEnvrnFlag) && (!state.dataGlobal->WarmupFlag)) {
                PeopleInd = state.dataAirflowNetwork->MultizoneZoneData(IZ).CEN15251PeopleInd;
                if (PeopleInd > 0 && state.dataThermalComforts->ThermalComfortData(PeopleInd).ThermalComfortAdaptiveCEN15251CatI != -1) {
                    if (state.dataThermalComforts->ThermalComfortData(PeopleInd).ThermalComfortOpTemp >
                        state.dataThermalComforts->ThermalComfortData(PeopleInd).TComfCEN15251) {
                        OpenFactor = state.dataAirflowNetwork->MultizoneSurfaceData(i).Factor;
                        state.dataSurface->SurfWinVentingOpenFactorMultRep(SurfNum) = 1.0;
                    } else {
                        OpenFactor = 0.0;
                    }
                } else {
                    OpenFactor = 0.0;
                }
            } else {
                OpenFactor = 0.0;
            }
        }

        // No venting, i.e, window/door always closed - added YJH 8 Aug 02

        if (VentCtrlNum == VentControlType::NoVent) { // Novent
            OpenFactor = 0.0;
            state.dataSurface->SurfWinVentingOpenFactorMultRep(SurfNum) = -1.0;
        }
    }

    void AssignFanAirLoopNum(EnergyPlusData &state)
    {
        // Assign the system Fan AirLoop Number based on the zone inlet node

        for (int i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfZones; i++) {
            for (int j = 1; j <= state.dataGlobal->NumOfZones; j++) {
                if (!state.dataZoneEquip->ZoneEquipConfig(j).IsControlled) continue;
                if ((state.dataAirflowNetwork->MultizoneZoneData(i).ZoneNum == j) && (state.dataZoneEquip->ZoneEquipConfig(j).NumInletNodes > 0)) {
                    for (int k = 1; k <= state.dataAirflowNetworkBalanceManager->DisSysNumOfCVFs; k++) {
                        if (state.dataAirflowNetwork->DisSysCompCVFData(k).AirLoopNum == 0) {
                            state.dataAirflowNetwork->DisSysCompCVFData(k).AirLoopNum =
                                state.dataZoneEquip->ZoneEquipConfig(j).InletNodeAirLoopNum(1);
                        }
                    }
                }
            }
        }
    }

    void ValidateDistributionSystem(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   Oct. 2005
        //       MODIFIED       L. Gu, Jan. 2009: allow a desuperheater coil and three heat exchangers
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine validates the inputs of distribution system, since node data from a primary airloop
        // are not available in the first call during reading input data of airflownetwork objects.
        // Note: this routine shouldn't be called more than once

        // Using/Aliasing
        using BranchNodeConnections::GetNodeConnectionType;
        using MixedAir::GetNumOAMixers;
        using MixedAir::GetOAMixerInletNodeNumber;
        using MixedAir::GetOAMixerReliefNodeNumber;
        using SingleDuct::GetHVACSingleDuctSysIndex;
        using namespace DataLoopNode;
        auto &NumPrimaryAirSys = state.dataHVACGlobal->NumPrimaryAirSys;
        using DXCoils::SetDXCoilAirLoopNumber;
        using Fans::SetFanAirLoopNumber;
        using HeatingCoils::SetHeatingCoilAirLoopNumber;
        using HVACStandAloneERV::GetStandAloneERVNodeNumber;
        using SplitterComponent::GetSplitterNodeNumbers;
        using SplitterComponent::GetSplitterOutletNumber;
        using WaterThermalTanks::GetHeatPumpWaterHeaterNodeNumber;
        using ZoneDehumidifier::GetZoneDehumidifierNodeNumber;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static constexpr std::string_view RoutineName("ValidateDistributionSystem: "); // include trailing blank space

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int i;
        int j;
        int k;
        int n;
        int S1;
        int S2;
        int R1;
        int R2;
        bool LocalError;
        Array1D_bool NodeFound;

        bool ErrorsFound(false);
        bool IsNotOK(false);
        bool errFlag(false);
        Array1D_int NodeConnectionType; // Specifies the type of node connection
        std::string CurrentModuleObject;

        bool HPWHFound(false);          // Flag for HPWH identification
        bool StandaloneERVFound(false); // Flag for Standalone ERV (ZoneHVAC:EnergyRecoveryVentilator) identification

        // Validate supply and return connections
        NodeFound.dimension(state.dataLoopNodes->NumOfNodes, false);
        // Validate inlet and outlet nodes for zone exhaust fans
        for (i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfExhFan; ++i) {
            NodeFound(state.dataAirflowNetwork->MultizoneCompExhaustFanData(i).InletNode) = true;
            NodeFound(state.dataAirflowNetwork->MultizoneCompExhaustFanData(i).OutletNode) = true;
        }
        // Validate EPlus Node names and types
        for (i = 1; i <= state.dataAirflowNetworkBalanceManager->DisSysNumOfNodes; ++i) {
            if (UtilityRoutines::SameString(state.dataAirflowNetwork->DisSysNodeData(i).EPlusName, "") ||
                UtilityRoutines::SameString(state.dataAirflowNetwork->DisSysNodeData(i).EPlusName, "Other"))
                continue;
            LocalError = false;
            for (j = 1; j <= state.dataLoopNodes->NumOfNodes; ++j) { // NodeID
                if (state.dataAirflowNetwork->DisSysNodeData(i).EPlusName == state.dataLoopNodes->NodeID(j)) {
                    state.dataAirflowNetwork->DisSysNodeData(i).AirLoopNum = GetAirLoopNumber(state, j);
                    if (state.dataAirflowNetwork->DisSysNodeData(i).AirLoopNum == 0) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + "The Node or Component Name defined in " + state.dataAirflowNetwork->DisSysNodeData(i).Name +
                                            " is not found in the AirLoopHVAC.");
                        ShowContinueError(state,
                                          "The entered name is " + state.dataAirflowNetwork->DisSysNodeData(i).EPlusName +
                                              " in an AirflowNetwork:Distribution:Node object.");
                        ErrorsFound = true;
                    }
                    state.dataAirflowNetwork->DisSysNodeData(i).EPlusNodeNum = j;
                    state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->NumOfNodesMultiZone + i).EPlusNodeNum = j;
                    state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->NumOfNodesMultiZone + i).AirLoopNum =
                        state.dataAirflowNetwork->DisSysNodeData(i).AirLoopNum;
                    NodeFound(j) = true;
                    LocalError = true;
                    break;
                }
            }
            // Check outdoor air node
            if (UtilityRoutines::SameString(state.dataAirflowNetwork->DisSysNodeData(i).EPlusType, "OutdoorAir:NodeList") ||
                UtilityRoutines::SameString(state.dataAirflowNetwork->DisSysNodeData(i).EPlusType, "OutdoorAir:Node")) {
                if (!LocalError) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + "The Node or Component Name defined in " + state.dataAirflowNetwork->DisSysNodeData(i).Name +
                                        " is not found in the " + state.dataAirflowNetwork->DisSysNodeData(i).EPlusType);
                    ShowContinueError(state,
                                      "The entered name is " + state.dataAirflowNetwork->DisSysNodeData(i).EPlusName +
                                          " in an AirflowNetwork:Distribution:Node object.");
                    ErrorsFound = true;
                }
            }
            if (state.dataAirflowNetwork->DisSysNodeData(i).EPlusNodeNum == 0) {
                ShowSevereError(state,
                                std::string{RoutineName} + "Primary Air Loop Node is not found in AIRFLOWNETWORK:DISTRIBUTION:NODE = " +
                                    state.dataAirflowNetwork->DisSysNodeData(i).Name);
                ErrorsFound = true;
            }
        }

        // Determine node numbers for zone inlets
        for (i = 1; i <= state.dataGlobal->NumOfZones; ++i) {
            if (!state.dataZoneEquip->ZoneEquipConfig(i).IsControlled) continue;
            for (j = 1; j <= state.dataZoneEquip->ZoneEquipConfig(i).NumInletNodes; ++j) {
                for (k = 1; k <= state.dataAirflowNetwork->AirflowNetworkNumOfNodes; ++k) {
                    if (state.dataZoneEquip->ZoneEquipConfig(i).InletNode(j) == state.dataAirflowNetwork->AirflowNetworkNodeData(k).EPlusNodeNum) {
                        state.dataAirflowNetwork->AirflowNetworkNodeData(k).EPlusTypeNum = iEPlusNodeType::ZIN;
                        break;
                    }
                }
            }
        }

        // Eliminate node not related to AirLoopHVAC
        for (k = 1; k <= state.dataBranchNodeConnections->NumOfNodeConnections; ++k) {
            if (NodeFound(state.dataBranchNodeConnections->NodeConnections(k).NodeNumber)) continue;
            if (state.dataBranchNodeConnections->NodeConnections(k).FluidStream == 2) {
                NodeFound(state.dataBranchNodeConnections->NodeConnections(k).NodeNumber) = true;
            }
        }

        // Eliminate nodes with fluidtype = water
        for (k = 1; k <= state.dataLoopNodes->NumOfNodes; ++k) {
            if (NodeFound(k)) continue;
            if (state.dataLoopNodes->Node(k).FluidType == DataLoopNode::NodeFluidType::Water) {
                NodeFound(k) = true;
            }
        }

        // Eliminate local external air node for network
        for (k = 1; k <= state.dataLoopNodes->NumOfNodes; ++k) {
            if (NodeFound(k)) continue;
            if (state.dataLoopNodes->Node(k).IsLocalNode) NodeFound(k) = true;
        }

        // Ensure all the nodes used in Eplus are a subset of AirflowNetwork Nodes
        for (i = 1; i <= state.dataLoopNodes->NumOfNodes; ++i) {
            if (NodeFound(i)) continue;
            // Skip the inlet and outlet nodes of zone dehumidifiers
            if (GetZoneDehumidifierNodeNumber(state, i)) NodeFound(i) = true;

            if (state.dataAirflowNetwork->AirflowNetworkSimu.AllowSupportZoneEqp) {
                // Skip HPWH nodes that don't have to be included in the AFN
                if (GetHeatPumpWaterHeaterNodeNumber(state, i)) {
                    NodeFound(i) = true;
                    HPWHFound = true;
                }

                // Skip Standalone ERV nodes that don't have to be included in the AFN
                if (GetStandAloneERVNodeNumber(state, i)) {
                    NodeFound(i) = true;
                    StandaloneERVFound = true;
                }
            }

            for (j = 1; j <= state.dataGlobal->NumOfZones; ++j) {
                if (!state.dataZoneEquip->ZoneEquipConfig(j).IsControlled) continue;
                if (state.dataZoneEquip->ZoneEquipConfig(j).ZoneNode == i) {
                    if (state.dataZoneEquip->ZoneEquipConfig(j).ActualZoneNum > state.dataAirflowNetwork->AirflowNetworkNumOfNodes) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + "'" + state.dataLoopNodes->NodeID(i) +
                                            "' is not defined as an AirflowNetwork:Distribution:Node object.");
                        ShowContinueError(state,
                                          "This Node is the zone air node for Zone '" + state.dataZoneEquip->ZoneEquipConfig(j).ZoneName + "'.");
                        ErrorsFound = true;
                    } else {
                        NodeFound(i) = true;
                        state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataZoneEquip->ZoneEquipConfig(j).ActualZoneNum).EPlusNodeNum = i;
                    }
                    break;
                }
            }

            //   skip nodes that are not part of an airflow network

            //     DX COIL CONDENSER NODE TEST:
            //     Outside air nodes are used for DX coil condenser inlet nodes, these are specified in an outside air node or
            //     OutdoorAir:NodeList object (and classified with NodeConnectionType as OutsideAir). In addition,
            //     this same node is specified in a Coil:DX:CoolingBypassFactorEmpirical object (and classified with
            //     NodeConnectionType as OutsideAirReference). In the NodeConnectionType structure, both of these nodes have a
            //     unique index but have the same node number. The Outside Air Node will usually be listed first. Search for all
            //     indexes with the same node number and check if it is classified as NodeConnectionType = OutsideAirReference.
            //     Mark this node as found since it is not used in an airflownetwork simulation.
            //     Example (using AirflowNetwork_MultiZone_SmallOffice.idf with a single OA Mixer):
            //             (the example shown below is identical to AirflowNetwork_SimpleHouse.idf with no OA Mixer except
            //              that the NodeConnections indexes are (7) and (31), respectively and the NodeNumber = 6)
            //   The GetNodeConnectionType CALL below returns DataLoopNode::NodeConnectionType::OutsideAir = 7 and
            //   DataLoopNode::NodeConnectionType::OutsideAirReference = 14.
            //     NodeConnections info from OUTSIDE AIR NODE object read:
            //     NodeConnections(9)NodeNumber      = 10
            //     NodeConnections(9)NodeName        = ACDXCOIL 1 CONDENSER NODE
            //     NodeConnections(9)ObjectType      = OUTSIDE AIR NODE
            //     NodeConnections(9)ObjectName      = OUTSIDE AIR NODE
            //     NodeConnections(9)ConnectionType  = OutsideAir
            //     NodeConnections info from Coil:DX:CoolingBypassFactorEmpirical object read:
            //     NodeConnections(64)NodeNumber     = 10
            //     NodeConnections(64)NodeName       = ACDXCOIL 1 CONDENSER NODE
            //     NodeConnections(64)ObjectType     = COIL:DX:COOLINGBYPASSFACTOREMPIRICAL
            //     NodeConnections(64)ObjectName     = ACDXCOIL 1
            //     NodeConnections(64)ConnectionType = OutsideAirReference

            errFlag = false;
            GetNodeConnectionType(state, i, NodeConnectionType, errFlag); // Gets all connection types for a given node number
            if (errFlag) {
                ShowContinueError(state, "...occurs in Airflow Network simulation.");
            } else {
                //   skip nodes for air cooled condensers
                for (j = 1; j <= isize(NodeConnectionType); ++j) {
                    if (NodeConnectionType(j) == static_cast<int>(DataLoopNode::NodeConnectionType::OutsideAirReference)) {
                        NodeFound(i) = true;
                    }
                }
            }

            if (!NodeFound(i)) {
                // Check if this node is the OA relief node. For the time being, OA relief node is not used
                if (GetNumOAMixers(state) > 1) {
                    //                        ShowSevereError(state,  std::string{RoutineName} + "Only one OutdoorAir:Mixer is allowed in the
                    // AirflowNetwork model." );                         ErrorsFound = true;
                    int OAFanNum;
                    int OARelNum;
                    int OAMixerNum;

                    for (OAFanNum = 1; OAFanNum <= state.dataAirflowNetworkBalanceManager->NumOfOAFans; ++OAFanNum) {
                        state.dataAirflowNetwork->DisSysCompOutdoorAirData(OAFanNum).InletNode =
                            GetOAMixerInletNodeNumber(state, state.dataAirflowNetwork->DisSysCompOutdoorAirData(OAFanNum).OAMixerNum);
                        //                            NodeFound( state.dataAirflowNetwork->DisSysCompOutdoorAirData( OAFanNum ).InletNode
                        //                            ) = true;
                    }
                    for (OARelNum = 1; OARelNum <= state.dataAirflowNetworkBalanceManager->NumOfReliefFans; ++OARelNum) {
                        state.dataAirflowNetwork->DisSysCompReliefAirData(OARelNum).OutletNode =
                            GetOAMixerInletNodeNumber(state, state.dataAirflowNetwork->DisSysCompReliefAirData(OARelNum).OAMixerNum);
                        //                            NodeFound( state.dataAirflowNetwork->DisSysCompOutdoorAirData( OAFanNum ).InletNode
                        //                            ) = true;
                    }
                    // Check NodeFound status
                    for (OAMixerNum = 1; OAMixerNum <= GetNumOAMixers(state); ++OAMixerNum) {
                        if (i == GetOAMixerReliefNodeNumber(state, OAMixerNum)) {
                            NodeFound(i) = true;
                            break;
                        } else if (i == GetOAMixerInletNodeNumber(state, OAMixerNum)) {
                            NodeFound(i) = true;
                            break;
                        } else {
                            if (OAMixerNum == GetNumOAMixers(state)) {
                                ShowSevereError(state,
                                                std::string{RoutineName} + "'" + state.dataLoopNodes->NodeID(i) +
                                                    "' is not defined as an AirflowNetwork:Distribution:Node object.");
                                ErrorsFound = true;
                            }
                        }
                    }
                } else if (GetNumOAMixers(state) == 0) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + "'" + state.dataLoopNodes->NodeID(i) +
                                        "' is not defined as an AirflowNetwork:Distribution:Node object.");
                    ErrorsFound = true;
                } else {
                    // TODO: I fail to see how you could enter this block given than NumOAMixers (returned by GetNumOAMixers())
                    // is initialized to zero, and we check above if '> 0' or '== 0'
                    if (state.dataAirflowNetworkBalanceManager->NumOfOAFans == 1 &&
                        state.dataAirflowNetwork->DisSysCompOutdoorAirData(1).InletNode == 0) {
                        state.dataAirflowNetwork->DisSysCompOutdoorAirData(1).InletNode = GetOAMixerInletNodeNumber(state, 1);
                    }
                    if (state.dataAirflowNetworkBalanceManager->NumOfReliefFans == 1 &&
                        state.dataAirflowNetwork->DisSysCompReliefAirData(1).OutletNode == 0) {
                        state.dataAirflowNetwork->DisSysCompReliefAirData(1).OutletNode = GetOAMixerInletNodeNumber(state, 1);
                    }
                    if (i == GetOAMixerReliefNodeNumber(state, 1)) {
                        NodeFound(i) = true;
                    } else if (i == GetOAMixerInletNodeNumber(state, 1)) {
                        NodeFound(i) = true;
                    } else {
                        ShowSevereError(state,
                                        std::string{RoutineName} + "'" + state.dataLoopNodes->NodeID(i) +
                                            "' is not defined as an AirflowNetwork:Distribution:Node object.");
                        ErrorsFound = true;
                    }
                }
            }
        }
        if (HPWHFound) {
            ShowWarningError(state,
                             "ValidateDistributionSystem: Heat pump water heater is simulated along with an AirflowNetwork but is not included in "
                             "the AirflowNetwork.");
        }
        if (StandaloneERVFound) {
            ShowWarningError(state,
                             "ValidateDistributionSystem: A ZoneHVAC:EnergyRecoveryVentilator is simulated along with an AirflowNetwork but is not "
                             "included in the AirflowNetwork.");
        }
        NodeFound.deallocate();

        // Assign AirLoop Number to every node and linkage
        // Zone first
        for (i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfZones; i++) {
            for (j = 1; j <= state.dataGlobal->NumOfZones; j++) {
                if (!state.dataZoneEquip->ZoneEquipConfig(j).IsControlled) continue;
                if ((state.dataAirflowNetwork->MultizoneZoneData(i).ZoneNum == j) && (state.dataZoneEquip->ZoneEquipConfig(j).NumInletNodes > 0)) {
                    // No multiple Airloop
                    state.dataAirflowNetwork->AirflowNetworkNodeData(i).AirLoopNum = state.dataZoneEquip->ZoneEquipConfig(j).InletNodeAirLoopNum(1);
                }
            }
        }
        // Air Distribution system
        for (i = state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces + 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfLinks; ++i) {
            j = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
            k = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
            if (state.dataAirflowNetwork->AirflowNetworkNodeData(j).AirLoopNum == 0 &&
                state.dataAirflowNetwork->AirflowNetworkNodeData(k).AirLoopNum == 0) {
                // Error messaage
                ShowSevereError(state,
                                "ValidateDistributionSystem: AIRFLOWNETWORK:DISTRIBUTION:LINKAGE = " +
                                    state.dataAirflowNetwork->AirflowNetworkLinkageData(i).Name + " is not valid for AirLoopNum assignment");
                ShowContinueError(
                    state,
                    "AirLoopNum is not found in both nodes for the linkage: " + state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNames[0] +
                        " and " + state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNames[1]);
                ShowContinueError(state,
                                  "Please ensure one of two AIRFLOWNETWORK:DISTRIBUTION:NODEs in the first AIRFLOWNETWORK:DISTRIBUTION:LINKAGE "
                                  "object should be defined as EnergyPlus NodeID.");
                ErrorsFound = true;
            }
            if (state.dataAirflowNetwork->AirflowNetworkNodeData(j).AirLoopNum > 0 &&
                state.dataAirflowNetwork->AirflowNetworkNodeData(k).AirLoopNum == 0) {
                state.dataAirflowNetwork->AirflowNetworkNodeData(k).AirLoopNum = state.dataAirflowNetwork->AirflowNetworkNodeData(j).AirLoopNum;
            }
            if (state.dataAirflowNetwork->AirflowNetworkNodeData(j).AirLoopNum == 0 &&
                state.dataAirflowNetwork->AirflowNetworkNodeData(k).AirLoopNum > 0) {
                state.dataAirflowNetwork->AirflowNetworkNodeData(j).AirLoopNum = state.dataAirflowNetwork->AirflowNetworkNodeData(k).AirLoopNum;
            }
            if (state.dataAirflowNetwork->AirflowNetworkNodeData(j).AirLoopNum == state.dataAirflowNetwork->AirflowNetworkNodeData(k).AirLoopNum) {
                state.dataAirflowNetwork->AirflowNetworkLinkageData(i).AirLoopNum = state.dataAirflowNetwork->AirflowNetworkNodeData(j).AirLoopNum;
            }
            if (state.dataAirflowNetwork->AirflowNetworkNodeData(j).AirLoopNum != state.dataAirflowNetwork->AirflowNetworkNodeData(k).AirLoopNum &&
                state.dataAirflowNetwork->AirflowNetworkNodeData(j).AirLoopNum > 0 &&
                state.dataAirflowNetwork->AirflowNetworkNodeData(k).AirLoopNum > 0) {
                state.dataAirflowNetwork->AirflowNetworkLinkageData(i).AirLoopNum = state.dataAirflowNetwork->AirflowNetworkNodeData(j).AirLoopNum;
                ShowSevereError(state,
                                "The AirLoopNum defined in both AIRFLOWNETWORK:DISTRIBUTION:NODE objects in " +
                                    state.dataAirflowNetwork->AirflowNetworkLinkageData(i).Name +
                                    " are not the same. Please make sure both nodes should be listed in the same AirLoop as a valid linkage.");
                ShowContinueError(
                    state,
                    "AirLoop defined in " + state.dataAirflowNetwork->AirflowNetworkNodeData(j).Name + " is " +
                        state.dataAirSystemsData->PrimaryAirSystems(state.dataAirflowNetwork->AirflowNetworkNodeData(j).AirLoopNum).Name +
                        ", and AirLoop defined in " + state.dataAirflowNetwork->AirflowNetworkNodeData(k).Name + " is " +
                        state.dataAirSystemsData->PrimaryAirSystems(state.dataAirflowNetwork->AirflowNetworkNodeData(k).AirLoopNum).Name);
                ErrorsFound = true;
            }
            // Set AirLoopNum to fans and coils
            if (state.dataAirflowNetwork->AirflowNetworkCompData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).CompNum).EPlusTypeNum ==
                iEPlusComponentType::FAN) {
                n = state.dataAirflowNetwork
                        ->DisSysCompCVFData(
                            state.dataAirflowNetwork->AirflowNetworkCompData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).CompNum).TypeNum)
                        .FanIndex;
                state.dataAirflowNetwork
                    ->DisSysCompCVFData(
                        state.dataAirflowNetwork->AirflowNetworkCompData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).CompNum).TypeNum)
                    .AirLoopNum = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).AirLoopNum;
                if (state.dataAirflowNetwork
                        ->DisSysCompCVFData(
                            state.dataAirflowNetwork->AirflowNetworkCompData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).CompNum).TypeNum)
                        .FanModelFlag) {
                    state.dataHVACFan
                        ->fanObjs[state.dataAirflowNetwork
                                      ->DisSysCompCVFData(state.dataAirflowNetwork
                                                              ->AirflowNetworkCompData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).CompNum)
                                                              .TypeNum)
                                      .FanIndex]
                        ->AirLoopNum = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).AirLoopNum;
                } else {
                    SetFanAirLoopNumber(state, n, state.dataAirflowNetwork->AirflowNetworkLinkageData(i).AirLoopNum);
                }
            }
            if (state.dataAirflowNetwork->AirflowNetworkCompData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).CompNum).EPlusTypeNum ==
                iEPlusComponentType::COI) {
                state.dataAirflowNetwork
                    ->DisSysCompCoilData(
                        state.dataAirflowNetwork->AirflowNetworkCompData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).CompNum).TypeNum)
                    .AirLoopNum = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).AirLoopNum;
            }
        }

        // Validate coil name and type
        CurrentModuleObject = "AirflowNetwork:Distribution:Component:Coil";
        state.dataAirflowNetwork->MultiSpeedHPIndicator = 0;
        for (i = 1; i <= state.dataAirflowNetworkBalanceManager->DisSysNumOfCoils; ++i) {
            {
                auto const SELECT_CASE_var(UtilityRoutines::MakeUPPERCase(state.dataAirflowNetwork->DisSysCompCoilData(i).EPlusType));

                if (SELECT_CASE_var == "COIL:COOLING:DX") {
                    ValidateComponent(
                        state, "Coil:Cooling:DX", state.dataAirflowNetwork->DisSysCompCoilData(i).name, IsNotOK, std::string{RoutineName} + CurrentModuleObject);
                    if (IsNotOK) {
                        ErrorsFound = true;
                    } else {
                        // Replace the convenience function with in-place code
                        std::string mycoil = state.dataAirflowNetwork->DisSysCompCoilData(i).name;
                        auto it = std::find_if(state.dataCoilCooingDX->coilCoolingDXs.begin(),
                                               state.dataCoilCooingDX->coilCoolingDXs.end(),
                                               [&mycoil](const CoilCoolingDX &coil) { return coil.name == mycoil; });
                        if (it != state.dataCoilCooingDX->coilCoolingDXs.end()) {
                            // Set the airloop number on the CoilCoolingDX object, which is used to collect the runtime fraction
                            it->airLoopNum = state.dataAirflowNetwork->DisSysCompCoilData(i).AirLoopNum;
                        } else {
                            ShowSevereError(state,
                                            "SetDXCoilAirLoopNumber: Could not find Coil \"Name=\"" +
                                                state.dataAirflowNetwork->DisSysCompCoilData(i).name + "\"");
                        }
                        // SetDXCoilAirLoopNumber(state.dataAirflowNetwork->DisSysCompCoilData(i).name,
                        // state.dataAirflowNetwork->DisSysCompCoilData(i).AirLoopNum);
                    }
                } else if (SELECT_CASE_var == "COIL:COOLING:DX:SINGLESPEED") {
                    ValidateComponent(state,
                                      "Coil:Cooling:DX:SingleSpeed",
                                      state.dataAirflowNetwork->DisSysCompCoilData(i).name,
                                      IsNotOK,
                                      std::string{RoutineName} + CurrentModuleObject);
                    if (IsNotOK) {
                        ErrorsFound = true;
                    } else {
                        SetDXCoilAirLoopNumber(
                            state, state.dataAirflowNetwork->DisSysCompCoilData(i).name, state.dataAirflowNetwork->DisSysCompCoilData(i).AirLoopNum);
                    }

                } else if (SELECT_CASE_var == "COIL:HEATING:DX:SINGLESPEED") {
                    ValidateComponent(state,
                                      "Coil:Heating:DX:SingleSpeed",
                                      state.dataAirflowNetwork->DisSysCompCoilData(i).name,
                                      IsNotOK,
                                      std::string{RoutineName} + CurrentModuleObject);
                    if (IsNotOK) {
                        ErrorsFound = true;
                    } else {
                        SetDXCoilAirLoopNumber(
                            state, state.dataAirflowNetwork->DisSysCompCoilData(i).name, state.dataAirflowNetwork->DisSysCompCoilData(i).AirLoopNum);
                    }

                } else if (SELECT_CASE_var == "COIL:HEATING:FUEL") {
                    ValidateComponent(
                        state, "Coil:Heating:Fuel", state.dataAirflowNetwork->DisSysCompCoilData(i).name, IsNotOK, std::string{RoutineName} + CurrentModuleObject);
                    if (IsNotOK) {
                        ErrorsFound = true;
                    } else {
                        SetHeatingCoilAirLoopNumber(state,
                                                    state.dataAirflowNetwork->DisSysCompCoilData(i).name,
                                                    state.dataAirflowNetwork->DisSysCompCoilData(i).AirLoopNum,
                                                    ErrorsFound);
                    }

                } else if (SELECT_CASE_var == "COIL:HEATING:ELECTRIC") {
                    ValidateComponent(state,
                                      "Coil:Heating:Electric",
                                      state.dataAirflowNetwork->DisSysCompCoilData(i).name,
                                      IsNotOK,
                                      std::string{RoutineName} + CurrentModuleObject);
                    if (IsNotOK) {
                        ErrorsFound = true;
                    } else {
                        SetHeatingCoilAirLoopNumber(state,
                                                    state.dataAirflowNetwork->DisSysCompCoilData(i).name,
                                                    state.dataAirflowNetwork->DisSysCompCoilData(i).AirLoopNum,
                                                    ErrorsFound);
                    }

                } else if (SELECT_CASE_var == "COIL:COOLING:WATER") {
                    ValidateComponent(state,
                                      "Coil:Cooling:Water",
                                      state.dataAirflowNetwork->DisSysCompCoilData(i).name,
                                      IsNotOK,
                                      std::string{RoutineName} + CurrentModuleObject);
                    if (IsNotOK) {
                        ErrorsFound = true;
                    }

                } else if (SELECT_CASE_var == "COIL:HEATING:WATER") {
                    ValidateComponent(state,
                                      "Coil:Heating:Water",
                                      state.dataAirflowNetwork->DisSysCompCoilData(i).name,
                                      IsNotOK,
                                      std::string{RoutineName} + CurrentModuleObject);
                    if (IsNotOK) {
                        ErrorsFound = true;
                    }

                } else if (SELECT_CASE_var == "COIL:COOLING:WATER:DETAILEDGEOMETRY") {
                    ValidateComponent(state,
                                      "Coil:Cooling:Water:DetailedGeometry",
                                      state.dataAirflowNetwork->DisSysCompCoilData(i).name,
                                      IsNotOK,
                                      std::string{RoutineName} + CurrentModuleObject);
                    if (IsNotOK) {
                        ErrorsFound = true;
                    }

                } else if (SELECT_CASE_var == "COIL:COOLING:DX:TWOSTAGEWITHHUMIDITYCONTROLMODE") {
                    ValidateComponent(state,
                                      "Coil:Cooling:DX:TwoStageWithHumidityControlMode",
                                      state.dataAirflowNetwork->DisSysCompCoilData(i).name,
                                      IsNotOK,
                                      std::string{RoutineName} + CurrentModuleObject);
                    if (IsNotOK) {
                        ErrorsFound = true;
                    } else {
                        SetDXCoilAirLoopNumber(
                            state, state.dataAirflowNetwork->DisSysCompCoilData(i).name, state.dataAirflowNetwork->DisSysCompCoilData(i).AirLoopNum);
                    }

                } else if (SELECT_CASE_var == "COIL:COOLING:DX:MULTISPEED") {
                    ValidateComponent(state,
                                      "Coil:Cooling:DX:MultiSpeed",
                                      state.dataAirflowNetwork->DisSysCompCoilData(i).name,
                                      IsNotOK,
                                      std::string{RoutineName} + CurrentModuleObject);
                    ++state.dataAirflowNetwork->MultiSpeedHPIndicator;
                    if (IsNotOK) {
                        ErrorsFound = true;
                    } else {
                        SetDXCoilAirLoopNumber(
                            state, state.dataAirflowNetwork->DisSysCompCoilData(i).name, state.dataAirflowNetwork->DisSysCompCoilData(i).AirLoopNum);
                    }

                } else if (SELECT_CASE_var == "COIL:HEATING:DX:MULTISPEED") {
                    ValidateComponent(state,
                                      "Coil:Heating:DX:MultiSpeed",
                                      state.dataAirflowNetwork->DisSysCompCoilData(i).name,
                                      IsNotOK,
                                      std::string{RoutineName} + CurrentModuleObject);
                    ++state.dataAirflowNetwork->MultiSpeedHPIndicator;
                    if (IsNotOK) {
                        ErrorsFound = true;
                    } else {
                        SetDXCoilAirLoopNumber(
                            state, state.dataAirflowNetwork->DisSysCompCoilData(i).name, state.dataAirflowNetwork->DisSysCompCoilData(i).AirLoopNum);
                    }

                } else if (SELECT_CASE_var == "COIL:HEATING:DESUPERHEATER") {
                    ValidateComponent(state,
                                      "Coil:Heating:Desuperheater",
                                      state.dataAirflowNetwork->DisSysCompCoilData(i).name,
                                      IsNotOK,
                                      std::string{RoutineName} + CurrentModuleObject);
                    if (IsNotOK) {
                        ErrorsFound = true;
                    }

                } else if (SELECT_CASE_var == "COIL:COOLING:DX:TWOSPEED") {
                    ValidateComponent(state,
                                      "Coil:Cooling:DX:TwoSpeed",
                                      state.dataAirflowNetwork->DisSysCompCoilData(i).name,
                                      IsNotOK,
                                      std::string{RoutineName} + CurrentModuleObject);
                    if (IsNotOK) {
                        ErrorsFound = true;
                    } else {
                        SetDXCoilAirLoopNumber(
                            state, state.dataAirflowNetwork->DisSysCompCoilData(i).name, state.dataAirflowNetwork->DisSysCompCoilData(i).AirLoopNum);
                    }

                } else {
                    ShowSevereError(
                        state, std::string{RoutineName} + CurrentModuleObject + " Invalid coil type = " + state.dataAirflowNetwork->DisSysCompCoilData(i).name);
                    ErrorsFound = true;
                }
            }
        }

        // Validate terminal unit name and type
        for (i = 1; i <= state.dataAirflowNetworkBalanceManager->DisSysNumOfTermUnits; ++i) {
            if (UtilityRoutines::SameString(state.dataAirflowNetwork->DisSysCompTermUnitData(i).EPlusType,
                                            "AirTerminal:SingleDuct:ConstantVolume:Reheat") ||
                UtilityRoutines::SameString(state.dataAirflowNetwork->DisSysCompTermUnitData(i).EPlusType, "AirTerminal:SingleDuct:VAV:Reheat")) {
                LocalError = false;
                if (UtilityRoutines::SameString(state.dataAirflowNetwork->DisSysCompTermUnitData(i).EPlusType,
                                                "AirTerminal:SingleDuct:ConstantVolume:Reheat"))
                    GetHVACSingleDuctSysIndex(state,
                                              state.dataAirflowNetwork->DisSysCompTermUnitData(i).name,
                                              n,
                                              LocalError,
                                              "AirflowNetwork:Distribution:Component:TerminalUnit");
                if (UtilityRoutines::SameString(state.dataAirflowNetwork->DisSysCompTermUnitData(i).EPlusType, "AirTerminal:SingleDuct:VAV:Reheat"))
                    GetHVACSingleDuctSysIndex(state,
                                              state.dataAirflowNetwork->DisSysCompTermUnitData(i).name,
                                              n,
                                              LocalError,
                                              "AirflowNetwork:Distribution:Component:TerminalUnit",
                                              state.dataAirflowNetwork->DisSysCompTermUnitData(i).DamperInletNode,
                                              state.dataAirflowNetwork->DisSysCompTermUnitData(i).DamperOutletNode);
                if (LocalError) ErrorsFound = true;
                if (state.dataAirflowNetwork->VAVSystem) {
                    for (j = 1; j <= state.dataAirflowNetworkBalanceManager->DisSysNumOfCVFs; j++) {
                        if (state.dataAirflowNetwork->DisSysCompCVFData(j).FanTypeNum == FanType_SimpleVAV) {
                            if (state.dataAirflowNetwork->DisSysCompCVFData(j).AirLoopNum ==
                                    state.dataAirflowNetwork->DisSysCompTermUnitData(i).AirLoopNum &&
                                !UtilityRoutines::SameString(state.dataAirflowNetwork->DisSysCompTermUnitData(i).EPlusType,
                                                             "AirTerminal:SingleDuct:VAV:Reheat")) {
                                ShowSevereError(state,
                                                std::string{RoutineName} + CurrentModuleObject + " Invalid terminal type for a VAV system = " +
                                                    state.dataAirflowNetwork->DisSysCompTermUnitData(i).name);
                                ShowContinueError(state, "The input type = " + state.dataAirflowNetwork->DisSysCompTermUnitData(i).EPlusType);
                                ShowContinueError(state, "A VAV system requires all terminal units with type = AirTerminal:SingleDuct:VAV:Reheat");
                                ErrorsFound = true;
                            }
                        }
                    }
                }
            } else {
                ShowSevereError(state,
                                std::string{RoutineName} + "AIRFLOWNETWORK:DISTRIBUTION:COMPONENT TERMINAL UNIT: Invalid Terminal unit type = " +
                                    state.dataAirflowNetwork->DisSysCompTermUnitData(i).name);
                ErrorsFound = true;
            }
        }

        // Validate heat exchanger name and type
        CurrentModuleObject = "AirflowNetwork:Distribution:Component:HeatExchanger";
        for (i = 1; i <= state.dataAirflowNetworkBalanceManager->DisSysNumOfHXs; ++i) {
            {
                auto const SELECT_CASE_var(UtilityRoutines::MakeUPPERCase(state.dataAirflowNetwork->DisSysCompHXData(i).EPlusType));

                if (SELECT_CASE_var == "HEATEXCHANGER:AIRTOAIR:FLATPLATE") {
                    ValidateComponent(state,
                                      "HeatExchanger:AirToAir:FlatPlate",
                                      state.dataAirflowNetwork->DisSysCompHXData(i).name,
                                      IsNotOK,
                                      std::string{RoutineName} + CurrentModuleObject);
                    if (IsNotOK) {
                        ErrorsFound = true;
                    }

                } else if (SELECT_CASE_var == "HEATEXCHANGER:AIRTOAIR:SENSIBLEANDLATENT") {
                    ValidateComponent(state,
                                      "HeatExchanger:AirToAir:SensibleAndLatent",
                                      state.dataAirflowNetwork->DisSysCompHXData(i).name,
                                      IsNotOK,
                                      std::string{RoutineName} + CurrentModuleObject);
                    if (IsNotOK) {
                        ErrorsFound = true;
                    }

                } else if (SELECT_CASE_var == "HEATEXCHANGER:DESICCANT:BALANCEDFLOW") {
                    ValidateComponent(state,
                                      "HeatExchanger:Desiccant:BalancedFlow",
                                      state.dataAirflowNetwork->DisSysCompHXData(i).name,
                                      IsNotOK,
                                      std::string{RoutineName} + CurrentModuleObject);
                    if (IsNotOK) {
                        ErrorsFound = true;
                    }

                } else {
                    ShowSevereError(state,
                                    std::string{RoutineName} + CurrentModuleObject +
                                        " Invalid heat exchanger type = " + state.dataAirflowNetwork->DisSysCompHXData(i).EPlusType);
                    ErrorsFound = true;
                }
            }
        }

        // Assign supply and return connection
        for (j = 1; j <= NumPrimaryAirSys; ++j) {
            S1 = 0;
            S2 = 0;
            R1 = 0;
            R2 = 0;
            for (i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfNodes; ++i) {
                if (state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusNodeNum ==
                    state.dataAirLoop->AirToZoneNodeInfo(j).AirLoopSupplyNodeNum(1))
                    S1 = i;
                if (state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusNodeNum ==
                    state.dataAirLoop->AirToZoneNodeInfo(j).ZoneEquipSupplyNodeNum(1))
                    S2 = i;
                if (state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusNodeNum ==
                    state.dataAirLoop->AirToZoneNodeInfo(j).ZoneEquipReturnNodeNum(1))
                    R1 = i;
                if (state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusNodeNum ==
                    state.dataAirLoop->AirToZoneNodeInfo(j).AirLoopReturnNodeNum(1))
                    R2 = i;
            }
            for (i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfLinks; ++i) {
                if (state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0] == R1 &&
                    state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1] == R2) {
                    state.dataAirflowNetwork->AirflowNetworkLinkageData(i).ConnectionFlag = iEPlusComponentType::RCN;
                }
                if (state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0] == S1 &&
                    state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1] == S2) {
                    state.dataAirflowNetwork->AirflowNetworkLinkageData(i).ConnectionFlag = iEPlusComponentType::SCN;
                }
            }
        }

        // Assign fan inlet and outlet node, and coil outlet
        for (i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfLinks; ++i) {
            j = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).CompNum;
            if (state.dataAirflowNetwork->AirflowNetworkCompData(j).CompTypeNum == iComponentTypeNum::CVF) {
                if (state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0])
                        .EPlusTypeNum == iEPlusNodeType::Unassigned)
                    state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0])
                        .EPlusTypeNum = iEPlusNodeType::FIN;
                state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1]).EPlusTypeNum =
                    iEPlusNodeType::FOU;
            }
            if (state.dataAirflowNetwork->AirflowNetworkCompData(j).EPlusTypeNum == iEPlusComponentType::COI) {
                state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1]).EPlusTypeNum =
                    iEPlusNodeType::COU;
            }
            if (state.dataAirflowNetwork->AirflowNetworkCompData(j).EPlusTypeNum == iEPlusComponentType::HEX) {
                state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1]).EPlusTypeNum =
                    iEPlusNodeType::HXO;
            }
            if (state.dataAirflowNetwork->AirflowNetworkCompData(j).CompTypeNum == iComponentTypeNum::TMU) {
                if (state.dataAirflowNetwork->DisSysCompTermUnitData(state.dataAirflowNetwork->AirflowNetworkCompData(j).TypeNum).DamperInletNode >
                    0) {
                    if (state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0])
                                .EPlusNodeNum ==
                            state.dataAirflowNetwork->DisSysCompTermUnitData(state.dataAirflowNetwork->AirflowNetworkCompData(j).TypeNum)
                                .DamperInletNode &&
                        state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1])
                                .EPlusNodeNum ==
                            state.dataAirflowNetwork->DisSysCompTermUnitData(state.dataAirflowNetwork->AirflowNetworkCompData(j).TypeNum)
                                .DamperOutletNode) {
                        state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0])
                            .EPlusTypeNum = iEPlusNodeType::DIN;
                        state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1])
                            .EPlusTypeNum = iEPlusNodeType::DOU;
                        state.dataAirflowNetwork->AirflowNetworkLinkageData(i).VAVTermDamper = true;
                    }
                }
            }
        }

        // Validate the position of constant pressure drop component
        CurrentModuleObject = "AirflowNetwork:Distribution:Component:ConstantPressureDrop";
        for (i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfLinks; ++i) {
            if (state.dataAirflowNetwork->AirflowNetworkCompData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).CompNum).CompTypeNum ==
                iComponentTypeNum::CPD) {
                for (j = 1; j <= state.dataAirflowNetwork->AirflowNetworkNumOfLinks; ++j) {
                    if (state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0] ==
                        state.dataAirflowNetwork->AirflowNetworkLinkageData(j).NodeNums[1]) {
                        if (state.dataAirflowNetwork->AirflowNetworkCompData(state.dataAirflowNetwork->AirflowNetworkLinkageData(j).CompNum)
                                .CompTypeNum != iComponentTypeNum::DWC) {
                            ShowSevereError(state,
                                            std::string{RoutineName} + "An " + CurrentModuleObject + " object (" +
                                                state.dataAirflowNetwork->AirflowNetworkLinkageData(i).CompName + ')');
                            ShowContinueError(state,
                                              "must connect a duct component upstream and not " +
                                                  state.dataAirflowNetwork->AirflowNetworkLinkageData(j).Name);
                            ErrorsFound = true;
                        }
                    }
                }
                if (state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0])
                        .EPlusTypeNum == iEPlusNodeType::SPL) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + "An " + CurrentModuleObject + " object (" +
                                        state.dataAirflowNetwork->AirflowNetworkLinkageData(i).CompName + ')');
                    ShowContinueError(
                        state,
                        "does not allow a AirLoopHVAC:ZoneSplitter node = " +
                            state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0])
                                .Name);
                    ErrorsFound = true;
                }
                if (state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1])
                        .EPlusTypeNum == iEPlusNodeType::SPL) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + "An " + CurrentModuleObject + " object (" +
                                        state.dataAirflowNetwork->AirflowNetworkLinkageData(i).CompName + ')');
                    ShowContinueError(
                        state,
                        "does not allow a AirLoopHVAC:ZoneSplitter node = " +
                            state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1])
                                .Name);
                    ErrorsFound = true;
                }
                if (state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0])
                        .EPlusTypeNum == iEPlusNodeType::MIX) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + "An " + CurrentModuleObject + " object (" +
                                        state.dataAirflowNetwork->AirflowNetworkLinkageData(i).CompName + ')');
                    ShowContinueError(
                        state,
                        "does not allow a AirLoopHVAC:ZoneMixer node = " +
                            state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0])
                                .Name);
                    ErrorsFound = true;
                }
                if (state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1])
                        .EPlusTypeNum == iEPlusNodeType::MIX) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + "An " + CurrentModuleObject + " object (" +
                                        state.dataAirflowNetwork->AirflowNetworkLinkageData(i).CompName + ')');
                    ShowContinueError(
                        state,
                        "does not allow a AirLoopHVAC:ZoneMixer node = " +
                            state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1])
                                .Name);
                    ErrorsFound = true;
                }
                if (state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0])
                        .EPlusNodeNum > 0) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + "An " + CurrentModuleObject + " object (" +
                                        state.dataAirflowNetwork->AirflowNetworkLinkageData(i).CompName + ')');
                    ShowContinueError(
                        state,
                        "does not allow to connect an EnergyPlus node = " +
                            state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0])
                                .Name);
                    ErrorsFound = true;
                }
                if (state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1])
                        .EPlusNodeNum > 0) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + "An " + CurrentModuleObject + " object (" +
                                        state.dataAirflowNetwork->AirflowNetworkLinkageData(i).CompName + ')');
                    ShowContinueError(
                        state,
                        "does not allow to connect an EnergyPlus node = " +
                            state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1])
                                .Name);
                    ErrorsFound = true;
                }
                if (state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0])
                        .EPlusZoneNum > 0) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + "An " + CurrentModuleObject + " object (" +
                                        state.dataAirflowNetwork->AirflowNetworkLinkageData(i).CompName + ')');
                    ShowContinueError(
                        state,
                        "does not allow to connect an EnergyPlus zone = " +
                            state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0])
                                .Name);
                    ErrorsFound = true;
                }
                if (state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1])
                        .EPlusZoneNum > 0) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + "An " + CurrentModuleObject + " object (" +
                                        state.dataAirflowNetwork->AirflowNetworkLinkageData(i).CompName + ')');
                    ShowContinueError(
                        state,
                        "does not allow to connect an EnergyPlus zone = " +
                            state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1])
                                .Name);
                    ErrorsFound = true;
                }
            }
        }

        for (i = state.dataAirflowNetwork->NumOfNodesMultiZone + 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfNodes; ++i) {
            if (state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusTypeNum == iEPlusNodeType::SPL) {
                LocalError = false;
                j = GetSplitterOutletNumber(state, "", 1, LocalError);
                state.dataAirflowNetworkBalanceManager->SplitterNodeNumbers.allocate(j + 2);
                state.dataAirflowNetworkBalanceManager->SplitterNodeNumbers = GetSplitterNodeNumbers(state, "", 1, LocalError);
                if (LocalError) ErrorsFound = true;
            }
        }

        // Assigning inlet and outlet nodes for a splitter
        for (i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfNodes; ++i) {
            if (state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusNodeNum == state.dataAirflowNetworkBalanceManager->SplitterNodeNumbers(1)) {
                if (state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusTypeNum == iEPlusNodeType::Unassigned)
                    state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusTypeNum = iEPlusNodeType::SPI;
            }
            for (j = 1; j <= state.dataAirflowNetworkBalanceManager->SplitterNodeNumbers(2); ++j) {
                if (state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusNodeNum ==
                    state.dataAirflowNetworkBalanceManager->SplitterNodeNumbers(j + 2)) {
                    if (state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusTypeNum == iEPlusNodeType::Unassigned)
                        state.dataAirflowNetwork->AirflowNetworkNodeData(i).EPlusTypeNum = iEPlusNodeType::SPO;
                }
            }
        }

        // Add additional output variables
        if (state.dataAirflowNetworkBalanceManager->DisSysNumOfCVFs > 1) {
            bool OnOffFanFlag = false;
            for (i = 1; i <= state.dataAirflowNetworkBalanceManager->DisSysNumOfCVFs; i++) {
                if (state.dataAirflowNetwork->DisSysCompCVFData(i).FanTypeNum == FanType_SimpleOnOff &&
                    !state.dataAirflowNetwork->DisSysCompCVFData(i).FanModelFlag) {
                    OnOffFanFlag = true;
                    break;
                }
                if (state.dataAirflowNetwork->DisSysCompCVFData(i).FanModelFlag &&
                    state.dataAirflowNetwork->DisSysCompCVFData(i).FanTypeNum == FanType_SimpleOnOff) {
                    int fanIndex = HVACFan::getFanObjectVectorIndex(state, state.dataAirflowNetwork->DisSysCompCVFData(i).name);
                    if (state.dataHVACFan->fanObjs[fanIndex]->AirPathFlag) {
                        state.dataAirflowNetwork->DisSysCompCVFData(i).FanTypeNum = FanType_SimpleConstVolume;
                    } else {
                        OnOffFanFlag = true;
                        break;
                    }
                }
            }
            if (OnOffFanFlag) {
                for (j = 1; j <= state.dataAirflowNetwork->AirflowNetworkNumOfZones; ++j) {
                    if (!state.dataZoneEquip->ZoneEquipConfig(state.dataAirflowNetwork->AirflowNetworkNodeData(j).EPlusZoneNum).IsControlled)
                        continue;
                    for (i = 1; i <= state.dataAirflowNetworkBalanceManager->DisSysNumOfCVFs; i++) {
                        if (state.dataAirflowNetwork->DisSysCompCVFData(i).AirLoopNum ==
                                state.dataAirflowNetwork->AirflowNetworkNodeData(j).AirLoopNum &&
                            state.dataAirflowNetwork->DisSysCompCVFData(i).FanTypeNum != FanType_SimpleOnOff) {
                            SetupOutputVariable(state,
                                                "AFN Node Total Pressure",
                                                OutputProcessor::Unit::Pa,
                                                state.dataAirflowNetwork->AirflowNetworkNodeSimu(j).PZ,
                                                "System",
                                                "Average",
                                                state.dataAirflowNetwork->AirflowNetworkNodeData(j).Name);
                        }
                    }
                }
                for (i = 1; i <= state.dataAirflowNetwork->NumOfLinksMultiZone; ++i) {
                    if (!state.dataZoneEquip
                             ->ZoneEquipConfig(
                                 state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0])
                                     .EPlusZoneNum)
                             .IsControlled)
                        continue;
                    for (j = 1; j <= state.dataAirflowNetworkBalanceManager->DisSysNumOfCVFs; j++) {
                        if (state.dataAirflowNetwork->DisSysCompCVFData(j).AirLoopNum ==
                                state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0])
                                    .AirLoopNum &&
                            state.dataAirflowNetwork->DisSysCompCVFData(j).FanTypeNum != FanType_SimpleOnOff) {
                            SetupOutputVariable(state,
                                                "AFN Linkage Node 1 to Node 2 Mass Flow Rate",
                                                OutputProcessor::Unit::kg_s,
                                                state.dataAirflowNetworkBalanceManager->linkReport(i).FLOW,
                                                "System",
                                                "Average",
                                                state.dataAirflowNetwork->AirflowNetworkLinkageData(i).Name);
                            SetupOutputVariable(state,
                                                "AFN Linkage Node 2 to Node 1 Mass Flow Rate",
                                                OutputProcessor::Unit::kg_s,
                                                state.dataAirflowNetworkBalanceManager->linkReport(i).FLOW2,
                                                "System",
                                                "Average",
                                                state.dataAirflowNetwork->AirflowNetworkLinkageData(i).Name);
                            SetupOutputVariable(state,
                                                "AFN Linkage Node 1 to Node 2 Volume Flow Rate",
                                                OutputProcessor::Unit::m3_s,
                                                state.dataAirflowNetworkBalanceManager->linkReport(i).VolFLOW,
                                                "System",
                                                "Average",
                                                state.dataAirflowNetwork->AirflowNetworkLinkageData(i).Name);
                            SetupOutputVariable(state,
                                                "AFN Linkage Node 2 to Node 1 Volume Flow Rate",
                                                OutputProcessor::Unit::m3_s,
                                                state.dataAirflowNetworkBalanceManager->linkReport(i).VolFLOW2,
                                                "System",
                                                "Average",
                                                state.dataAirflowNetwork->AirflowNetworkLinkageData(i).Name);
                            SetupOutputVariable(state,
                                                "AFN Linkage Node 1 to Node 2 Pressure Difference",
                                                OutputProcessor::Unit::Pa,
                                                state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).DP,
                                                "System",
                                                "Average",
                                                state.dataAirflowNetwork->AirflowNetworkLinkageData(i).Name);
                        }
                    }
                }
            }
        }
        bool FanModelConstFlag = false;
        for (i = 1; i <= state.dataAirflowNetworkBalanceManager->DisSysNumOfCVFs; i++) {
            if (state.dataAirflowNetwork->DisSysCompCVFData(i).FanModelFlag) {
                int fanIndex = HVACFan::getFanObjectVectorIndex(state, state.dataAirflowNetwork->DisSysCompCVFData(i).name);
                if (state.dataAirflowNetwork->DisSysCompCVFData(i).FanTypeNum == FanType_SimpleOnOff &&
                    state.dataHVACFan->fanObjs[fanIndex]->AirPathFlag) {
                    state.dataAirflowNetwork->DisSysCompCVFData(i).FanTypeNum = FanType_SimpleConstVolume;
                    state.dataAirflowNetworkBalanceManager->SupplyFanType = FanType_SimpleConstVolume;
                    FanModelConstFlag = true;
                    break;
                }
            }
        }
        if (FanModelConstFlag) {
            for (i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces; ++i) {
                if (state.dataAirflowNetworkBalanceManager->SupplyFanType == FanType_SimpleConstVolume) {
                    SetupOutputVariable(state,
                                        "AFN Linkage Node 1 to Node 2 Mass Flow Rate",
                                        OutputProcessor::Unit::kg_s,
                                        state.dataAirflowNetworkBalanceManager->linkReport(i).FLOW,
                                        "System",
                                        "Average",
                                        state.dataAirflowNetwork->AirflowNetworkLinkageData(i).Name);
                    SetupOutputVariable(state,
                                        "AFN Linkage Node 2 to Node 1 Mass Flow Rate",
                                        OutputProcessor::Unit::kg_s,
                                        state.dataAirflowNetworkBalanceManager->linkReport(i).FLOW2,
                                        "System",
                                        "Average",
                                        state.dataAirflowNetwork->AirflowNetworkLinkageData(i).Name);
                    SetupOutputVariable(state,
                                        "AFN Linkage Node 1 to Node 2 Volume Flow Rate",
                                        OutputProcessor::Unit::m3_s,
                                        state.dataAirflowNetworkBalanceManager->linkReport(i).VolFLOW,
                                        "System",
                                        "Average",
                                        state.dataAirflowNetwork->AirflowNetworkLinkageData(i).Name);
                    SetupOutputVariable(state,
                                        "AFN Linkage Node 2 to Node 1 Volume Flow Rate",
                                        OutputProcessor::Unit::m3_s,
                                        state.dataAirflowNetworkBalanceManager->linkReport(i).VolFLOW2,
                                        "System",
                                        "Average",
                                        state.dataAirflowNetwork->AirflowNetworkLinkageData(i).Name);
                    SetupOutputVariable(state,
                                        "AFN Linkage Node 1 to Node 2 Pressure Difference",
                                        OutputProcessor::Unit::Pa,
                                        state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).DP,
                                        "System",
                                        "Average",
                                        state.dataAirflowNetwork->AirflowNetworkLinkageData(i).Name);
                }
            }
        }

        // Add AirLoopNum to pressure control object
        for (i = 1; i <= state.dataAirflowNetworkBalanceManager->NumOfPressureControllers; ++i) {
            for (j = 1; j <= state.dataGlobal->NumOfZones; ++j) {
                if (state.dataAirflowNetwork->PressureControllerData(i).ZoneNum == j) {
                    for (k = 1; k <= state.dataZoneEquip->ZoneEquipConfig(j).NumInletNodes; ++k) {
                        if (state.dataZoneEquip->ZoneEquipConfig(j).InletNodeAirLoopNum(k) > 0) {
                            state.dataAirflowNetwork->PressureControllerData(i).AirLoopNum =
                                state.dataZoneEquip->ZoneEquipConfig(j).InletNodeAirLoopNum(k);
                            if (state.dataAirflowNetwork->PressureControllerData(i).ControlTypeSet == PressureCtrlRelief) {
                                state.dataAirflowNetwork->PressureControllerData(i).OANodeNum =
                                    state.dataAirSystemsData->PrimaryAirSystems(state.dataAirflowNetwork->PressureControllerData(i).AirLoopNum)
                                        .OAMixOAInNodeNum;
                                for (n = 1; n <= state.dataAirflowNetworkBalanceManager->NumOfReliefFans; ++n) {
                                    if (state.dataAirflowNetwork->DisSysCompReliefAirData(n).OutletNode ==
                                        state.dataAirflowNetwork->PressureControllerData(i).OANodeNum) {
                                        state.dataAirflowNetwork->DisSysCompReliefAirData(n).PressCtrlNum = i;
                                    }
                                }
                            }
                            if (state.dataAirflowNetwork->PressureControllerData(i).ControlTypeSet == PressureCtrlExhaust) {
                                state.dataAirflowNetwork->PressureControllerData(i).OANodeNum =
                                    state.dataZoneEquip->ZoneEquipConfig(state.dataAirflowNetwork->PressureControllerData(i).ZoneNum).ExhaustNode(1);
                                for (n = 1; n <= state.dataAirflowNetwork->AirflowNetworkNumOfExhFan; ++n) {
                                    if (state.dataAirflowNetwork->MultizoneCompExhaustFanData(n).EPlusZoneNum ==
                                        state.dataAirflowNetwork->PressureControllerData(i).ZoneNum) {
                                        state.dataAirflowNetwork->MultizoneCompExhaustFanData(n).PressCtrlNum = i;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        // Check number of fans specified in an AirLoop #6748
        int BranchNum;
        int CompNum;
        int NumOfFans;
        std::string FanNames;
        for (BranchNum = 1; BranchNum <= state.dataAirSystemsData->PrimaryAirSystems(1).NumBranches; ++BranchNum) {
            NumOfFans = 0;
            FanNames = "";
            for (CompNum = 1; CompNum <= state.dataAirSystemsData->PrimaryAirSystems(1).Branch(BranchNum).TotalComponents; ++CompNum) {
                if (UtilityRoutines::SameString(state.dataAirSystemsData->PrimaryAirSystems(1).Branch(BranchNum).Comp(CompNum).TypeOf,
                                                "Fan:ConstantVolume") ||
                    UtilityRoutines::SameString(state.dataAirSystemsData->PrimaryAirSystems(1).Branch(BranchNum).Comp(CompNum).TypeOf, "Fan:OnOff") ||
                    UtilityRoutines::SameString(state.dataAirSystemsData->PrimaryAirSystems(1).Branch(BranchNum).Comp(CompNum).TypeOf,
                                                "Fan:VariableVolume")) {
                    NumOfFans++;
                    if (NumOfFans > 1) {
                        FanNames += state.dataAirSystemsData->PrimaryAirSystems(1).Branch(BranchNum).Comp(CompNum).Name;
                        break;
                    } else {
                        FanNames += state.dataAirSystemsData->PrimaryAirSystems(1).Branch(BranchNum).Comp(CompNum).Name + ",";
                    }
                }
            }
            if (NumOfFans > 1) break;
        }
        if (NumOfFans > 1) {
            ShowSevereError(state,
                            std::string{RoutineName} + "An AirLoop branch, " + state.dataAirSystemsData->PrimaryAirSystems(1).Branch(BranchNum).Name +
                                ", has two or more fans: " + FanNames);
            ShowContinueError(state,
                              "The AirflowNetwork model allows a single supply fan in an AirLoop only. Please make changes in the input "
                              "file accordingly.");
            ErrorsFound = true;
        }

        if (ErrorsFound) {
            ShowFatalError(state, std::string{RoutineName} + "Program terminates for preceding reason(s).");
        }
    }

    void ValidateFanFlowRate(EnergyPlusData &state)
    {

        // Catch a fan flow rate from EPlus input file and add a flag for VAV terminal damper
        for (int i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfLinks; ++i) {
            {
                auto const SELECT_CASE_var(
                    state.dataAirflowNetwork->AirflowNetworkCompData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).CompNum).CompTypeNum);
                if (SELECT_CASE_var == iComponentTypeNum::CVF) { // 'CVF'
                    int typeNum =
                        state.dataAirflowNetwork->AirflowNetworkCompData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).CompNum).TypeNum;
                    if (state.dataAirflowNetwork->DisSysCompCVFData(typeNum).FanTypeNum == FanType_SimpleVAV) {
                        if (state.dataAirflowNetwork->DisSysCompCVFData(typeNum).FanModelFlag) {
                            state.dataAirflowNetwork->DisSysCompCVFData(typeNum).MaxAirMassFlowRate =
                                state.dataHVACFan->fanObjs[state.dataAirflowNetwork->DisSysCompCVFData(typeNum).FanIndex]->designAirVolFlowRate *
                                state.dataEnvrn->StdRhoAir;
                        } else {
                            Real64 FanFlow; // Return type
                            GetFanVolFlow(state, state.dataAirflowNetwork->DisSysCompCVFData(typeNum).FanIndex, FanFlow);
                            state.dataAirflowNetwork->DisSysCompCVFData(typeNum).MaxAirMassFlowRate = FanFlow * state.dataEnvrn->StdRhoAir;
                        }
                    }
                } else if (SELECT_CASE_var == iComponentTypeNum::FAN) { //'FAN'
                                                                        // Check ventilation status for large openings
                } else if (SELECT_CASE_var == iComponentTypeNum::SOP) { //'Simple opening'
                } else if (SELECT_CASE_var == iComponentTypeNum::TMU) { // Terminal unit
                } else {
                }
            }
        }
    }

    void ValidateExhaustFanInput(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   Dec. 2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine validate zone exhaust fan and associated surface

        // Using/Aliasing
        using DataZoneEquipment::ZoneExhaustFan_Num;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static constexpr std::string_view RoutineName("ValidateExhaustFanInput: "); // include trailing blank space

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int i;
        int j;
        int k;
        bool ErrorsFound(false);
        bool found;
        int EquipTypeNum; // Equipment type number
        std::string CurrentModuleObject;

        // Validate supply and return connections
        if (state.dataAirflowNetworkBalanceManager->ValidateExhaustFanInputOneTimeFlag) {
            CurrentModuleObject = "AirflowNetwork:MultiZone:Component:ZoneExhaustFan";
            if (std::any_of(state.dataZoneEquip->ZoneEquipConfig.begin(),
                            state.dataZoneEquip->ZoneEquipConfig.end(),
                            [](DataZoneEquipment::EquipConfiguration const &e) { return e.IsControlled; })) {
                state.dataAirflowNetwork->AirflowNetworkZoneExhaustFan.dimension(state.dataGlobal->NumOfZones, false);
            }
            // Ensure the number of exhaust fan defined in the AirflowNetwork model matches the number of Zone Exhaust Fan objects
            if (state.dataAirflowNetworkBalanceManager->NumOfExhaustFans != state.dataAirflowNetwork->AirflowNetworkNumOfExhFan) {
                ShowSevereError(state,
                                std::string{RoutineName} + "The number of " + CurrentModuleObject +
                                    " is not equal to the number of Fan:ZoneExhaust fans defined in ZoneHVAC:EquipmentConnections");
                ShowContinueError(state, format("The number of {} is {}", CurrentModuleObject, state.dataAirflowNetwork->AirflowNetworkNumOfExhFan));
                ShowContinueError(state,
                                  format("The number of Zone exhaust fans defined in ZoneHVAC:EquipmentConnections is {}",
                                         state.dataAirflowNetworkBalanceManager->NumOfExhaustFans));
                ErrorsFound = true;
            }

            for (i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfExhFan; ++i) {
                // Get zone number
                for (j = 1; j <= state.dataGlobal->NumOfZones; ++j) {
                    if (!state.dataZoneEquip->ZoneEquipConfig(j).IsControlled) continue;
                    for (k = 1; k <= state.dataZoneEquip->ZoneEquipConfig(j).NumExhaustNodes; ++k) {
                        if (state.dataZoneEquip->ZoneEquipConfig(j).ExhaustNode(k) ==
                            state.dataAirflowNetwork->MultizoneCompExhaustFanData(i).InletNode) {
                            state.dataAirflowNetwork->MultizoneCompExhaustFanData(i).EPlusZoneNum =
                                state.dataZoneEquip->ZoneEquipConfig(j).ActualZoneNum;
                            break;
                        }
                    }
                }
                if (state.dataAirflowNetwork->MultizoneCompExhaustFanData(i).EPlusZoneNum == 0) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + "Zone name in " + CurrentModuleObject + "  = " +
                                        state.dataAirflowNetwork->MultizoneCompExhaustFanData(i).name +
                                        " does not match the zone name in ZoneHVAC:EquipmentConnections");
                    ErrorsFound = true;
                }
                // Ensure a surface using zone exhaust fan to expose to the same zone
                found = false;
                for (j = 1; j <= state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces; ++j) {
                    if (UtilityRoutines::SameString(state.dataAirflowNetwork->MultizoneSurfaceData(j).OpeningName,
                                                    state.dataAirflowNetwork->MultizoneCompExhaustFanData(i).name)) {
                        found = true;
                        if (state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(j).SurfNum).ExtBoundCond !=
                                ExternalEnvironment &&
                            !(state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).ExtBoundCond ==
                                  OtherSideCoefNoCalcExt &&
                              state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).ExtWind)) {
                            ShowSevereError(state,
                                            std::string{RoutineName} + "The surface using " + CurrentModuleObject +
                                                " is not an exterior surface: " + state.dataAirflowNetwork->MultizoneSurfaceData(j).SurfName);
                            ErrorsFound = true;
                        }
                        break;
                    }
                }
                if (!found) {
                    ShowSevereError(state,
                                    CurrentModuleObject + "  = " + state.dataAirflowNetwork->MultizoneCompExhaustFanData(i).name +
                                        " is defined and never used.");
                    ErrorsFound = true;
                } else {
                    if (state.dataAirflowNetwork->MultizoneCompExhaustFanData(i).EPlusZoneNum !=
                        state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(j).SurfNum).Zone) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + "Zone name in " + CurrentModuleObject + "  = " +
                                            state.dataAirflowNetwork->MultizoneCompExhaustFanData(i).name + " does not match the zone name");
                        ShowContinueError(state,
                                          "the surface is exposed to " +
                                              state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(j).SurfNum).Name);
                        ErrorsFound = true;
                    } else {
                        state.dataAirflowNetwork->AirflowNetworkZoneExhaustFan(
                            state.dataAirflowNetwork->MultizoneCompExhaustFanData(i).EPlusZoneNum) = true;
                    }
                }
            }

            // Ensure all zone exhaust fans are defined
            for (j = 1; j <= state.dataGlobal->NumOfZones; ++j) {
                if (!state.dataZoneEquip->ZoneEquipConfig(j).IsControlled) continue;
                for (EquipTypeNum = 1; EquipTypeNum <= state.dataZoneEquip->ZoneEquipList(j).NumOfEquipTypes; ++EquipTypeNum) {
                    if (state.dataZoneEquip->ZoneEquipList(j).EquipType_Num(EquipTypeNum) == ZoneExhaustFan_Num) {
                        found = false;
                        for (k = 1; k <= state.dataZoneEquip->ZoneEquipConfig(j).NumExhaustNodes; ++k) {
                            for (i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfExhFan; ++i) {
                                if (state.dataZoneEquip->ZoneEquipConfig(j).ExhaustNode(k) ==
                                    state.dataAirflowNetwork->MultizoneCompExhaustFanData(i).InletNode) {
                                    state.dataAirflowNetwork->MultizoneCompExhaustFanData(i).EPlusZoneNum =
                                        state.dataZoneEquip->ZoneEquipConfig(j).ActualZoneNum;
                                    found = true;
                                }
                            }
                            if (!found) {
                                ShowSevereError(state, std::string{RoutineName} + "Fan:ZoneExhaust is not defined in " + CurrentModuleObject);
                                ShowContinueError(state,
                                                  "Zone Air Exhaust Node in ZoneHVAC:EquipmentConnections =" +
                                                      state.dataLoopNodes->NodeID(state.dataZoneEquip->ZoneEquipConfig(j).ExhaustNode(k)));
                                ErrorsFound = true;
                            }
                        }
                    }
                }
            }

            state.dataAirflowNetworkBalanceManager->ValidateExhaustFanInputOneTimeFlag = false;
            if (ErrorsFound) {
                ShowFatalError(state, std::string{RoutineName} + "Program terminates for preceding reason(s).");
            }
        } // End if OneTimeFlag_FindFirstLastPtr
    }

    void HybridVentilationControl(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   Dec. 2006
        //       MODIFIED       July 2012, Chandan Sharma - FSEC: Added zone hybrid ventilation managers
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine performs hybrid ventilation control

        auto &HybridVentSysAvailActualZoneNum = state.dataHVACGlobal->HybridVentSysAvailActualZoneNum;
        auto &HybridVentSysAvailAirLoopNum = state.dataHVACGlobal->HybridVentSysAvailAirLoopNum;
        auto &HybridVentSysAvailANCtrlStatus = state.dataHVACGlobal->HybridVentSysAvailANCtrlStatus;
        auto &HybridVentSysAvailMaster = state.dataHVACGlobal->HybridVentSysAvailMaster;
        auto &HybridVentSysAvailVentCtrl = state.dataHVACGlobal->HybridVentSysAvailVentCtrl;
        auto &HybridVentSysAvailWindModifier = state.dataHVACGlobal->HybridVentSysAvailWindModifier;
        auto &NumHybridVentSysAvailMgrs = state.dataHVACGlobal->NumHybridVentSysAvailMgrs;

        // SUBROUTINE PARAMETER DEFINITIONS:
        int const HybridVentCtrl_Close(2);                                  // Open windows or doors
        int const IndividualCtrlType(0);                                    // Individual window or door control
        int const GlobalCtrlType(1);                                        // Global window or door control
        static constexpr std::string_view RoutineName("HybridVentilationControl: "); // include trailing blank space

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int SysAvailNum;       // Hybrid ventilation control number
        int AirLoopNum;        // Airloop number
        int ControlledZoneNum; // Controlled zone number
        int ActualZoneNum;     // Actual zone number
        int ANSurfaceNum;      // AirflowNetwork Surface Number
        int SurfNum;           // Surface number
        int ControlType;       // Hybrid ventilation control type: 0 individual; 1 global
        bool Found;            // Logical to indicate whether a master surface is found or not

        for (auto &e : state.dataAirflowNetwork->MultizoneSurfaceData) {
            e.HybridVentClose = false;
            e.HybridCtrlGlobal = false;
            e.HybridCtrlMaster = false;
            e.WindModifier = 1.0;
        }
        ControlType = IndividualCtrlType;

        for (SysAvailNum = 1; SysAvailNum <= NumHybridVentSysAvailMgrs; ++SysAvailNum) {
            AirLoopNum = HybridVentSysAvailAirLoopNum(SysAvailNum);
            state.dataAirflowNetworkBalanceManager->VentilationCtrl = HybridVentSysAvailVentCtrl(SysAvailNum);
            if (HybridVentSysAvailANCtrlStatus(SysAvailNum) > 0) {
                ControlType = GetCurrentScheduleValue(state, HybridVentSysAvailANCtrlStatus(SysAvailNum));
            }
            Found = false;
            ActualZoneNum = 0;
            for (ControlledZoneNum = 1; ControlledZoneNum <= state.dataGlobal->NumOfZones; ++ControlledZoneNum) {
                if (!state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).IsControlled) continue;
                // Ensure all the zones served by this AirLoopHVAC to be controlled by the hybrid ventilation
                for (int zoneInNode = 1; zoneInNode <= state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).NumInletNodes; ++zoneInNode) {
                    if (AirLoopNum > 0) {
                        if (AirLoopNum == state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).InletNodeAirLoopNum(zoneInNode)) {
                            ActualZoneNum = state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ActualZoneNum;
                            break;
                        }
                    } else {
                        if (HybridVentSysAvailActualZoneNum(SysAvailNum) == state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ActualZoneNum) {
                            ActualZoneNum = HybridVentSysAvailActualZoneNum(SysAvailNum);
                        }
                    }
                }
                if (ActualZoneNum > 0) {
                    for (ANSurfaceNum = 1; ANSurfaceNum <= state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces; ++ANSurfaceNum) {
                        SurfNum = state.dataAirflowNetwork->MultizoneSurfaceData(ANSurfaceNum).SurfNum;
                        if (state.dataSurface->Surface(SurfNum).Zone == ActualZoneNum) {
                            if (state.dataAirflowNetworkBalanceManager->VentilationCtrl == HybridVentCtrl_Close) {
                                state.dataAirflowNetwork->MultizoneSurfaceData(ANSurfaceNum).HybridVentClose = true;
                            } else {
                                if (HybridVentSysAvailWindModifier(SysAvailNum) >= 0) {
                                    state.dataAirflowNetwork->MultizoneSurfaceData(ANSurfaceNum).WindModifier =
                                        HybridVentSysAvailWindModifier(SysAvailNum);
                                }
                                if (ControlType == GlobalCtrlType) {
                                    state.dataAirflowNetwork->MultizoneSurfaceData(ANSurfaceNum).HybridCtrlGlobal = true;
                                    if (HybridVentSysAvailMaster(SysAvailNum) == ActualZoneNum) {
                                        if ((state.dataSurface->SurfWinOriginalClass(SurfNum) == SurfaceClass::Window ||
                                             state.dataSurface->SurfWinOriginalClass(SurfNum) == SurfaceClass::Door ||
                                             state.dataSurface->SurfWinOriginalClass(SurfNum) == SurfaceClass::GlassDoor) &&
                                            state.dataSurface->Surface(SurfNum).ExtBoundCond == ExternalEnvironment) {
                                            state.dataAirflowNetwork->MultizoneSurfaceData(ANSurfaceNum).HybridCtrlMaster = true;
                                            Found = true;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            if (ControlType == GlobalCtrlType && !Found && !state.dataGlobal->WarmupFlag &&
                state.dataAirflowNetworkBalanceManager->VentilationCtrl != HybridVentCtrl_Close) {
                ++state.dataAirflowNetworkBalanceManager->HybridGlobalErrCount;
                if (state.dataAirflowNetworkBalanceManager->HybridGlobalErrCount < 2) {
                    ShowWarningError(state,
                                     std::string{RoutineName} +
                                         "The hybrid ventilation control schedule value indicates global control in the controlled zone = " +
                                         state.dataHeatBal->Zone(HybridVentSysAvailMaster(SysAvailNum)).Name);
                    ShowContinueError(state,
                                      "The exterior surface containing an opening component in the controlled zone is not found.  No global control "
                                      "will not be modeled.");
                    ShowContinueError(state, "The individual control is assumed.");
                    ShowContinueErrorTimeStamp(state, "");
                } else {
                    ShowRecurringWarningErrorAtEnd(
                        state,
                        std::string{RoutineName} + "The hybrid ventilation control requires a global control. The individual control continues...",
                        state.dataAirflowNetworkBalanceManager->HybridGlobalErrIndex,
                        double(ControlType),
                        double(ControlType));
                }
            }
        }
    }

    void CalcSingleSidedCps(EnergyPlusData &state, std::vector<std::vector<Real64>> &valsByFacade, int numWindDir)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Sam Brunswick
        //       DATE WRITTEN   September 2013
        //       MODIFIED       Revised by J. DeGraw, May 2017, to use tables
        //       RE-ENGINEERED  n/a

        // PURPOSE OF THIS SUBROUTINE:
        // Modify the wind pressure coefficients for single sided ventilation.

        // Using/Aliasing
        using namespace DataEnvironment;

        // Locals
        int windDirNum;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int AFNZnNum; // counters
        int SrfNum;
        int ExtOpenNum;
        int ZnNum;
        int DetOpenNum; // row index of surface in state.dataAirflowNetwork->MultizoneCompDetOpeningData
        int SimOpenNum; // row index of surface in MultizoneCompSimOpeningData
        int MZDZoneNum; // row index of surface zone in state.dataAirflowNetwork->MultizoneZoneData
        Real64 X1;
        Real64 Y1;
        Real64 X2;
        Real64 Y2;
        Real64 ZoneAng1;
        Real64 ZoneAng2;
        Real64 ZoneAngDiff;
        Array1D<Real64> ZoneAng;        // Azimuth angle of the exterior wall of the zone
        Array1D<Real64> PiFormula;      // Formula for the mean pressure difference
        Array1D<Real64> SigmaFormula;   // Formula for the fluctuating pressure difference
        Array1D<Real64> Sprime;         // The dimensionless ratio of the window separation to the building width
        Array1D<Real64> CPV1;           // Wind pressure coefficient for the first opening in the zone
        Array1D<Real64> CPV2;           // Wind pressure coefficient for the second opening in the zone
        std::string Name;               // External node name
        Array1D_int NumofExtSurfInZone; // List of the number of exterior openings in each zone

        struct AFNExtSurfacesProp // External opening information
        {
            // Members
            int SurfNum;          // row index of the external opening in the Surface array
            std::string SurfName; // Surface name
            int MSDNum;           // row index of the external opening in the state.dataAirflowNetwork->MultizoneSurfaceData array
            int ZoneNum;          // EnergyPlus zone number
            int MZDZoneNum;       // row index of the zone in the state.dataAirflowNetwork->MultizoneZoneData array
            int ExtNodeNum;       // External node number; = row index in state.dataAirflowNetwork->MultizoneExternalNodeData array +
                                  // state.dataAirflowNetwork->AirflowNetworkNumOfZones
            std::string ZoneName; // EnergyPlus zone name
            int facadeNum;
            int curve;                     // wind pressure coefficient curve index
            iComponentTypeNum CompTypeNum; // Opening type (detailed, simple, etc.)
            Real64 NodeHeight;             // Elevation of the opening node
            Real64 OpeningArea;            // Opening area (=Height*Width)
            Real64 Height;                 // Opening height = state.dataAirflowNetwork->MultizoneSurfaceData()%Height
            Real64 Width;                  // Opening width  = state.dataAirflowNetwork->MultizoneSurfaceData()%Width
            Real64 DischCoeff;             // Opening discharge coefficient

            // Default Constructor
            AFNExtSurfacesProp()
                : SurfNum(0), MSDNum(0), ZoneNum(0), MZDZoneNum(0), ExtNodeNum(0), facadeNum(0), curve(0), CompTypeNum(iComponentTypeNum::Unassigned),
                  NodeHeight(0.0), OpeningArea(0.0), Height(0.0), Width(0.0), DischCoeff(0.0)
            {
            }
        };

        // Object Data
        Array1D<AFNExtSurfacesProp> AFNExtSurfaces; // Surface numbers of all exterior openings
        auto &solver = state.dataAFNSolver->solver;
        // Count the total number of exterior simple and detailed openings and the number in each zone
        // Verify that each zone with "ADVANCED" single sided wind pressure coefficients has exactly two openings.
        // If it doesn't have two openings, change "ADVANCED" to "STANDARD"
        NumofExtSurfInZone.dimension(state.dataAirflowNetwork->AirflowNetworkNumOfZones, 0);
        for (AFNZnNum = 1; AFNZnNum <= state.dataAirflowNetwork->AirflowNetworkNumOfZones; ++AFNZnNum) {
            if (state.dataAirflowNetwork->MultizoneZoneData(AFNZnNum).SingleSidedCpType == "ADVANCED") {
                for (SrfNum = 1; SrfNum <= state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces; ++SrfNum) {
                    if (state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(SrfNum).SurfNum).ExtBoundCond ==
                        ExternalEnvironment) { // check if outdoor boundary condition
                        MZDZoneNum = UtilityRoutines::FindItemInList(
                            state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(SrfNum).SurfNum).ZoneName,
                            state.dataAirflowNetwork->MultizoneZoneData,
                            &MultizoneZoneProp::ZoneName);
                        if (MZDZoneNum == AFNZnNum) {
                            // This is terrible, should not do it this way
                            auto afe = solver.elements.find(state.dataAirflowNetwork->MultizoneSurfaceData(SrfNum).OpeningName);
                            if (afe != solver.elements.end()) {
                                auto type = afe->second->type();
                                if (type == ComponentType::DOP) {
                                    ++state.dataAirflowNetworkBalanceManager->AFNNumOfExtOpenings;
                                    ++NumofExtSurfInZone(AFNZnNum);
                                } else if (type == ComponentType::SOP) {
                                    ++state.dataAirflowNetworkBalanceManager->AFNNumOfExtOpenings;
                                    ++NumofExtSurfInZone(AFNZnNum);
                                }
                            }
                        }
                    }
                }
                if (NumofExtSurfInZone(AFNZnNum) == 0) {
                    ShowWarningError(state,
                                     "AirflowNetwork:Multizone:Zone = " + state.dataAirflowNetwork->MultizoneZoneData(AFNZnNum).ZoneName +
                                         " has single side wind pressure coefficient type \"ADVANCED\", but has no exterior "
                                         "AirflowNetwork:MultiZone:Component:DetailedOpening and/or AirflowNetwork:MultiZone:Component:SimpleOpening "
                                         "objects.");
                    ShowContinueError(state,
                                      "Zones must have exactly two exterior openings in order for the \"ADVANCED\" single sided wind pressure "
                                      "coefficient model to be used.");
                    ShowContinueError(state,
                                      "The wind pressure coefficient model for this zone will be set to \"STANDARD\" and simulation continues.");
                    state.dataAirflowNetwork->MultizoneZoneData(AFNZnNum).SingleSidedCpType = "STANDARD";
                } else if (NumofExtSurfInZone(AFNZnNum) == 1) {
                    ShowWarningError(state, "AirflowNetwork:Multizone:Zone = " + state.dataAirflowNetwork->MultizoneZoneData(AFNZnNum).ZoneName);
                    ShowContinueError(state,
                                      "has single side wind pressure coefficient type \"ADVANCED\", but has only one exterior "
                                      "AirflowNetwork:MultiZone:Component:DetailedOpening and/or "
                                      "AirflowNetwork:MultiZone:Component:SimpleOpening objects.");
                    ShowContinueError(state,
                                      "Zones must have exactly two openings in order for the \"ADVANCED\" single side wind pressure coefficient "
                                      "model to be used.");
                    ShowContinueError(state,
                                      "The wind pressure coefficient model for this zone will be set to \"STANDARD\" and simulation continues.");
                    state.dataAirflowNetwork->MultizoneZoneData(AFNZnNum).SingleSidedCpType = "STANDARD";
                } else if (NumofExtSurfInZone(AFNZnNum) > 2) {
                    ShowWarningError(state,
                                     format("AirflowNetwork:Multizone:Zone = {} has single side wind pressure coefficient type "
                                            "\"ADVANCED\", but has {} exterior "
                                            "AirflowNetwork:MultiZone:Component:DetailedOpening and/or "
                                            "AirflowNetwork:MultiZone:Component:SimpleOpening objects.",
                                            state.dataAirflowNetwork->MultizoneZoneData(AFNZnNum).ZoneName,
                                            NumofExtSurfInZone(AFNZnNum)));
                    ShowContinueError(state,
                                      "Zones must have exactly two openings in order for the \"ADVANCED\" single side wind pressure coefficient "
                                      "model to be used.");
                    ShowContinueError(state,
                                      "The wind pressure coefficient model for this zone will be set to \"STANDARD\" and simulation continues.");
                    state.dataAirflowNetwork->MultizoneZoneData(AFNZnNum).SingleSidedCpType = "STANDARD";
                }
            }
        }
        if (state.dataAirflowNetworkBalanceManager->AFNNumOfExtOpenings == 0) return;
        // Recount the number of single sided zones
        state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfSingleSideZones = 0;
        for (AFNZnNum = 1; AFNZnNum <= state.dataAirflowNetwork->AirflowNetworkNumOfZones; ++AFNZnNum) {
            if (state.dataAirflowNetwork->MultizoneZoneData(AFNZnNum).SingleSidedCpType == "ADVANCED") {
                ++state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfSingleSideZones;
            }
        }
        if (state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfSingleSideZones == 0)
            return; // Bail if no zones call for the advanced single sided model.
        // Recount the number of detailed and simple exterior openings in zones with "ADVANCED" single sided wind pressure coefficients
        state.dataAirflowNetworkBalanceManager->AFNNumOfExtOpenings = 0;
        for (SrfNum = 1; SrfNum <= state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces; ++SrfNum) {
            MZDZoneNum =
                UtilityRoutines::FindItemInList(state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(SrfNum).SurfNum).ZoneName,
                                                state.dataAirflowNetwork->MultizoneZoneData,
                                                &MultizoneZoneProp::ZoneName);
            if (state.dataAirflowNetwork->MultizoneZoneData(MZDZoneNum).SingleSidedCpType == "ADVANCED") {
                if (state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(SrfNum).SurfNum).ExtBoundCond ==
                    ExternalEnvironment) { // check if outdoor boundary condition
                    // This is terrible, should not do it this way
                    auto afe = solver.elements.find(state.dataAirflowNetwork->MultizoneSurfaceData(SrfNum).OpeningName);
                    if (afe != solver.elements.end()) {
                        auto type = afe->second->type();
                        if (type == ComponentType::DOP) {
                            ++state.dataAirflowNetworkBalanceManager->AFNNumOfExtOpenings;
                        } else if (type == ComponentType::SOP) {
                            ++state.dataAirflowNetworkBalanceManager->AFNNumOfExtOpenings;
                        }
                    }
                }
            }
        }
        AFNExtSurfaces.allocate(state.dataAirflowNetworkBalanceManager->AFNNumOfExtOpenings);
        // Create array of properties for all the exterior single sided openings
        ExtOpenNum = 1;
        for (SrfNum = 1; SrfNum <= state.dataAirflowNetwork->AirflowNetworkNumOfSurfaces; ++SrfNum) {
            if (state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(SrfNum).SurfNum).ExtBoundCond == ExternalEnvironment) {
                if (state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfDetOpenings > 0) {
                    DetOpenNum = UtilityRoutines::FindItemInList(state.dataAirflowNetwork->MultizoneSurfaceData(SrfNum).OpeningName,
                                                                 state.dataAirflowNetwork->MultizoneCompDetOpeningData,
                                                                 &AirflowNetwork::DetailedOpening::name);
                    MZDZoneNum = UtilityRoutines::FindItemInList(
                        state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(SrfNum).SurfNum).ZoneName,
                        state.dataAirflowNetwork->MultizoneZoneData,
                        &MultizoneZoneProp::ZoneName);
                    if (state.dataAirflowNetwork->MultizoneZoneData(MZDZoneNum).SingleSidedCpType == "ADVANCED") {
                        if (DetOpenNum > 0) {
                            AFNExtSurfaces(ExtOpenNum).MSDNum = SrfNum;
                            AFNExtSurfaces(ExtOpenNum).SurfNum = state.dataAirflowNetwork->MultizoneSurfaceData(SrfNum).SurfNum;
                            AFNExtSurfaces(ExtOpenNum).NodeHeight = state.dataSurface->Surface(AFNExtSurfaces(ExtOpenNum).SurfNum).Centroid.z;
                            AFNExtSurfaces(ExtOpenNum).SurfName =
                                state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(SrfNum).SurfNum).Name;
                            AFNExtSurfaces(ExtOpenNum).ZoneNum =
                                state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(SrfNum).SurfNum).Zone;
                            AFNExtSurfaces(ExtOpenNum).ZoneName =
                                state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(SrfNum).SurfNum).ZoneName;
                            AFNExtSurfaces(ExtOpenNum).MZDZoneNum = UtilityRoutines::FindItemInList(
                                AFNExtSurfaces(ExtOpenNum).ZoneName, state.dataAirflowNetwork->MultizoneZoneData, &MultizoneZoneProp::ZoneName);
                            AFNExtSurfaces(ExtOpenNum).CompTypeNum = iComponentTypeNum::DOP;
                            AFNExtSurfaces(ExtOpenNum).Height = state.dataAirflowNetwork->MultizoneSurfaceData(SrfNum).Height;
                            AFNExtSurfaces(ExtOpenNum).Width = state.dataAirflowNetwork->MultizoneSurfaceData(SrfNum).Width;
                            AFNExtSurfaces(ExtOpenNum).OpeningArea = state.dataAirflowNetwork->MultizoneSurfaceData(SrfNum).Width *
                                                                     state.dataAirflowNetwork->MultizoneSurfaceData(SrfNum).Height *
                                                                     state.dataAirflowNetwork->MultizoneSurfaceData(SrfNum).OpenFactor;
                            AFNExtSurfaces(ExtOpenNum).ExtNodeNum = state.dataAirflowNetwork->MultizoneSurfaceData(ExtOpenNum).NodeNums[1];
                            AFNExtSurfaces(ExtOpenNum).facadeNum = state.dataAirflowNetwork
                                                                       ->MultizoneExternalNodeData(AFNExtSurfaces(ExtOpenNum).ExtNodeNum -
                                                                                                   state.dataAirflowNetwork->AirflowNetworkNumOfZones)
                                                                       .facadeNum;
                            AFNExtSurfaces(ExtOpenNum).curve = state.dataAirflowNetwork
                                                                   ->MultizoneExternalNodeData(AFNExtSurfaces(ExtOpenNum).ExtNodeNum -
                                                                                               state.dataAirflowNetwork->AirflowNetworkNumOfZones)
                                                                   .curve;
                            AFNExtSurfaces(ExtOpenNum).DischCoeff = state.dataAirflowNetwork->MultizoneCompDetOpeningData(DetOpenNum).DischCoeff2;
                            ++ExtOpenNum;
                        }
                    }
                } else if (state.dataAirflowNetworkBalanceManager->AirflowNetworkNumOfSimOpenings > 0) {
                    SimOpenNum = UtilityRoutines::FindItemInList(state.dataAirflowNetwork->MultizoneSurfaceData(SrfNum).OpeningName,
                                                                 state.dataAirflowNetwork->MultizoneCompSimpleOpeningData,
                                                                 &AirflowNetwork::SimpleOpening::name);
                    if (SimOpenNum > 0) {
                        AFNExtSurfaces(ExtOpenNum).MSDNum = SrfNum;
                        AFNExtSurfaces(ExtOpenNum).SurfNum = state.dataAirflowNetwork->MultizoneSurfaceData(SrfNum).SurfNum;
                        AFNExtSurfaces(ExtOpenNum).SurfName =
                            state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(SrfNum).SurfNum).Name;
                        AFNExtSurfaces(ExtOpenNum).ZoneNum =
                            state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(SrfNum).SurfNum).Zone;
                        AFNExtSurfaces(ExtOpenNum).ZoneName =
                            state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(SrfNum).SurfNum).ZoneName;
                        AFNExtSurfaces(ExtOpenNum).MZDZoneNum = UtilityRoutines::FindItemInList(
                            AFNExtSurfaces(ExtOpenNum).ZoneName, state.dataAirflowNetwork->MultizoneZoneData, &MultizoneZoneProp::ZoneName);
                        AFNExtSurfaces(ExtOpenNum).CompTypeNum = iComponentTypeNum::SOP;
                        AFNExtSurfaces(ExtOpenNum).Height = state.dataAirflowNetwork->MultizoneSurfaceData(SrfNum).Height;
                        AFNExtSurfaces(ExtOpenNum).Width = state.dataAirflowNetwork->MultizoneSurfaceData(SrfNum).Width;
                        AFNExtSurfaces(ExtOpenNum).OpeningArea = state.dataAirflowNetwork->MultizoneSurfaceData(SrfNum).Width *
                                                                 state.dataAirflowNetwork->MultizoneSurfaceData(SrfNum).Height *
                                                                 state.dataAirflowNetwork->MultizoneSurfaceData(SrfNum).OpenFactor;
                        AFNExtSurfaces(ExtOpenNum).ExtNodeNum = state.dataAirflowNetwork->MultizoneSurfaceData(ExtOpenNum).NodeNums[1];
                        AFNExtSurfaces(ExtOpenNum).curve = state.dataAirflowNetwork
                                                               ->MultizoneExternalNodeData(AFNExtSurfaces(ExtOpenNum).ExtNodeNum -
                                                                                           state.dataAirflowNetwork->AirflowNetworkNumOfZones)
                                                               .curve;
                        AFNExtSurfaces(ExtOpenNum).DischCoeff = state.dataAirflowNetwork->MultizoneCompSimpleOpeningData(SimOpenNum).DischCoeff;
                        ++ExtOpenNum;
                    }
                }
            }
        }
        // Calculate the azimuth and the coordinates of the centroid of each opening.
        // Calculate Sprime and state.dataAirflowNetwork->DeltaCp for each zone.
        PiFormula.allocate(numWindDir);
        SigmaFormula.allocate(numWindDir);
        state.dataAirflowNetwork->DeltaCp.allocate(state.dataAirflowNetwork->AirflowNetworkNumOfZones);
        state.dataAirflowNetwork->EPDeltaCP.allocate(state.dataAirflowNetwork->AirflowNetworkNumOfZones);
        Sprime.allocate(state.dataAirflowNetwork->AirflowNetworkNumOfZones);
        ZoneAng.allocate(state.dataAirflowNetwork->AirflowNetworkNumOfZones);
        for (ZnNum = 1; ZnNum <= state.dataAirflowNetwork->AirflowNetworkNumOfZones; ++ZnNum) {
            state.dataAirflowNetwork->DeltaCp(ZnNum).WindDir.allocate(numWindDir);
            state.dataAirflowNetwork->EPDeltaCP(ZnNum).WindDir.allocate(numWindDir);
            for (windDirNum = 1; windDirNum <= numWindDir; ++windDirNum) {
                state.dataAirflowNetwork->DeltaCp(ZnNum).WindDir(windDirNum) = 0.0;
                state.dataAirflowNetwork->EPDeltaCP(ZnNum).WindDir(windDirNum) = 0.0;
            }
        }
        Sprime = 0.0;
        ZoneAng = 0.0;
        for (ZnNum = 1; ZnNum <= state.dataAirflowNetwork->AirflowNetworkNumOfZones; ++ZnNum) {
            if (state.dataAirflowNetwork->MultizoneZoneData(ZnNum).SingleSidedCpType == "ADVANCED") {
                state.dataAirflowNetworkBalanceManager->OpenNuminZone = 1;
                for (ExtOpenNum = 1; ExtOpenNum <= state.dataAirflowNetworkBalanceManager->AFNNumOfExtOpenings; ++ExtOpenNum) {
                    if (state.dataAirflowNetworkBalanceManager->OpenNuminZone > 2) break; // Tuned
                    if (AFNExtSurfaces(ExtOpenNum).MZDZoneNum == ZnNum) {
                        if (state.dataAirflowNetworkBalanceManager->OpenNuminZone == 1) {
                            X1 = state.dataSurface->Surface(AFNExtSurfaces(ExtOpenNum).SurfNum).Centroid.x;
                            Y1 = state.dataSurface->Surface(AFNExtSurfaces(ExtOpenNum).SurfNum).Centroid.y;
                            ZoneAng1 = state.dataSurface->Surface(AFNExtSurfaces(ExtOpenNum).SurfNum).Azimuth;
                            ++state.dataAirflowNetworkBalanceManager->OpenNuminZone;
                        } else if (state.dataAirflowNetworkBalanceManager->OpenNuminZone == 2) {
                            X2 = state.dataSurface->Surface(AFNExtSurfaces(ExtOpenNum).SurfNum).Centroid.x;
                            Y2 = state.dataSurface->Surface(AFNExtSurfaces(ExtOpenNum).SurfNum).Centroid.y;
                            ZoneAng2 = state.dataSurface->Surface(AFNExtSurfaces(ExtOpenNum).SurfNum).Azimuth;
                            ++state.dataAirflowNetworkBalanceManager->OpenNuminZone;
                        }
                    }
                }
                ZoneAngDiff = ZoneAng1 - ZoneAng2;
                if (ZoneAngDiff > 0.01) {
                    ShowWarningError(state,
                                     "AirflowNetwork:Multizone:Zone = " + state.dataAirflowNetwork->MultizoneZoneData(AFNZnNum).ZoneName +
                                         " has single side wind pressure coefficient type \"ADVANCED\", but has openings which are not coplanar.");
                    ShowContinueError(state, "The openings should be coplanar for the model to be valid. Simulation Continues.");
                }
                ZoneAng(ZnNum) = ZoneAng1;
                Sprime(ZnNum) = std::sqrt(pow_2(X1 - X2) + pow_2(Y1 - Y2)) / state.dataAirflowNetwork->MultizoneZoneData(ZnNum).BuildWidth;
                // Calculate state.dataAirflowNetwork->DeltaCp for each wind direction for each zone
                for (windDirNum = 1; windDirNum <= numWindDir; ++windDirNum) {
                    state.dataEnvrn->WindDir = (windDirNum - 1) * 10.0;
                    Real64 WindAng = (windDirNum - 1) * 10.0;
                    state.dataAirflowNetworkBalanceManager->IncAng = std::abs(WindAng - ZoneAng(ZnNum));
                    if (std::abs(state.dataAirflowNetworkBalanceManager->IncAng) > 180.0) state.dataAirflowNetworkBalanceManager->IncAng -= 360.0;
                    if (UtilityRoutines::SameString(state.dataAirflowNetwork->AirflowNetworkSimu.WPCCntr, "SurfaceAverageCalculation")) {
                        if (std::abs(state.dataAirflowNetworkBalanceManager->IncAng) <= 67.5) {
                            PiFormula(windDirNum) =
                                0.44 *
                                sign(std::sin(2.67 * std::abs(state.dataAirflowNetworkBalanceManager->IncAng) * DataGlobalConstants::Pi / 180.0),
                                     state.dataAirflowNetworkBalanceManager->IncAng);
                        } else if (std::abs(state.dataAirflowNetworkBalanceManager->IncAng) <= 180.0) {
                            PiFormula(windDirNum) = -0.69 * sign(std::sin((288 - 1.6 * std::abs(state.dataAirflowNetworkBalanceManager->IncAng)) *
                                                                          DataGlobalConstants::Pi / 180.0),
                                                                 state.dataAirflowNetworkBalanceManager->IncAng);
                        }
                        SigmaFormula(windDirNum) = 0.423 - 0.00163 * std::abs(state.dataAirflowNetworkBalanceManager->IncAng);
                        state.dataAirflowNetwork->DeltaCp(ZnNum).WindDir(windDirNum) =
                            (0.02 + (0.346 * std::abs(PiFormula(windDirNum)) + 0.084 * SigmaFormula(windDirNum)) * Sprime(ZnNum));
                    }
                }
            }
        }

        // Calculate the single sided Cp arrays from state.dataAirflowNetwork->DeltaCp for each single sided opening
        CPV1.allocate(numWindDir); // These two arrays should probably be removed
        CPV2.allocate(numWindDir);
        CPV1 = 0.0;
        CPV2 = 0.0;
        SrfNum = 6;
        for (ZnNum = 1; ZnNum <= state.dataAirflowNetwork->AirflowNetworkNumOfZones; ++ZnNum) {
            if (state.dataAirflowNetwork->MultizoneZoneData(ZnNum).SingleSidedCpType == "ADVANCED") {
                state.dataAirflowNetworkBalanceManager->OpenNuminZone = 1;
                for (ExtOpenNum = 1; ExtOpenNum <= state.dataAirflowNetworkBalanceManager->AFNNumOfExtOpenings; ++ExtOpenNum) {
                    if (state.dataAirflowNetworkBalanceManager->OpenNuminZone > 2) break; // Tuned
                    if (AFNExtSurfaces(ExtOpenNum).MZDZoneNum == ZnNum) {
                        Real64 const VelRatio_2(std::pow(10.0 / AFNExtSurfaces(ExtOpenNum).NodeHeight, 2.0 * state.dataEnvrn->SiteWindExp));
                        Real64 const AFNEExtSurface_fac(0.5 * (1.0 / pow_2(AFNExtSurfaces(ExtOpenNum).DischCoeff)));
                        if (state.dataAirflowNetworkBalanceManager->OpenNuminZone == 1) {
                            std::vector<Real64> cpvalues(numWindDir);
                            for (windDirNum = 1; windDirNum <= numWindDir; ++windDirNum) {
                                Real64 unmodifiedValue = valsByFacade[AFNExtSurfaces(ExtOpenNum).facadeNum - 1][windDirNum - 1] +
                                                         AFNEExtSurface_fac * state.dataAirflowNetwork->DeltaCp(ZnNum).WindDir(windDirNum);
                                cpvalues[windDirNum - 1] = CPV1(windDirNum) = VelRatio_2 * unmodifiedValue;
                            }
                            valsByFacade.push_back(cpvalues);
                            state.dataAirflowNetwork
                                ->MultizoneExternalNodeData(AFNExtSurfaces(ExtOpenNum).ExtNodeNum -
                                                            state.dataAirflowNetwork->AirflowNetworkNumOfZones)
                                .facadeNum = SrfNum;
                            ++state.dataAirflowNetworkBalanceManager->OpenNuminZone;
                            ++SrfNum;
                        } else if (state.dataAirflowNetworkBalanceManager->OpenNuminZone == 2) {
                            std::vector<Real64> cpvalues(numWindDir);
                            for (windDirNum = 1; windDirNum <= numWindDir; ++windDirNum) {
                                Real64 unmodifiedValue = valsByFacade[AFNExtSurfaces(ExtOpenNum).facadeNum - 1][windDirNum - 1] -
                                                         AFNEExtSurface_fac * state.dataAirflowNetwork->DeltaCp(ZnNum).WindDir(windDirNum);
                                cpvalues[windDirNum - 1] = CPV2(windDirNum) = VelRatio_2 * unmodifiedValue;
                                state.dataAirflowNetwork->EPDeltaCP(ZnNum).WindDir(windDirNum) = std::abs(CPV2(windDirNum) - CPV1(windDirNum));
                            }
                            valsByFacade.push_back(cpvalues);
                            state.dataAirflowNetwork
                                ->MultizoneExternalNodeData(AFNExtSurfaces(ExtOpenNum).ExtNodeNum -
                                                            state.dataAirflowNetwork->AirflowNetworkNumOfZones)
                                .facadeNum = SrfNum;
                            ++state.dataAirflowNetworkBalanceManager->OpenNuminZone;
                            ++SrfNum;
                        }
                    }
                }
            }
        }
        // Rewrite the CPVNum for all nodes that correspond with a simple or detailed opening
        // Does this loop really do anything?
        for (ZnNum = 1; ZnNum <= state.dataAirflowNetwork->AirflowNetworkNumOfZones; ++ZnNum) {
            state.dataAirflowNetworkBalanceManager->OpenNuminZone = 1;
            for (ExtOpenNum = 1; ExtOpenNum <= state.dataAirflowNetworkBalanceManager->AFNNumOfExtOpenings; ++ExtOpenNum) {
                if (AFNExtSurfaces(ExtOpenNum).MZDZoneNum == ZnNum) {
                    if (state.dataAirflowNetworkBalanceManager->OpenNuminZone == 1) {
                        ++state.dataAirflowNetworkBalanceManager->OpenNuminZone;
                    } else if (state.dataAirflowNetworkBalanceManager->OpenNuminZone == 2) {
                        ++state.dataAirflowNetworkBalanceManager->OpenNuminZone;
                    }
                }
            }
        }
    }

    Real64 GetZoneOutdoorAirChangeRate(EnergyPlusData &state, int const ZoneNum) // hybrid ventilation system controlled zone number
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   May. 2007
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This function outputs air change per hour in a given zone

        // Using/Aliasing
        auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;

        // Return value
        Real64 ACH; // Zone air change rate [ACH]

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 InfilVolume; // Zone infiltration volume
        Real64 RhoAir;      // Zone air density [kg/m3]
        Real64 CpAir;       // Zone air specific heat

        CpAir = PsyCpAirFnW(state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum));
        RhoAir = PsyRhoAirFnPbTdbW(
            state, state.dataEnvrn->OutBaroPress, state.dataHeatBalFanSys->MAT(ZoneNum), state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum));
        InfilVolume = ((state.dataAirflowNetworkBalanceManager->exchangeData(ZoneNum).SumMCp +
                        state.dataAirflowNetworkBalanceManager->exchangeData(ZoneNum).SumMVCp) /
                       CpAir / RhoAir) *
                      TimeStepSys * DataGlobalConstants::SecInHour;
        ACH = InfilVolume / (TimeStepSys * state.dataHeatBal->Zone(ZoneNum).Volume);

        return ACH;
    }

    int GetAirLoopNumber(EnergyPlusData &state, int const NodeNumber) // Get air loop number for each distribution node and linkage
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   Feb. 2018

        // PURPOSE OF THIS SUBROUTINE:
        // This function outputs an AirLoopNum based on node number

        // Using/Aliasing
        using BranchNodeConnections::GetChildrenData;
        using BranchNodeConnections::GetNumChildren;
        using BranchNodeConnections::IsParentObject;
        auto &NumPrimaryAirSys = state.dataHVACGlobal->NumPrimaryAirSys;
        using SingleDuct::GetHVACSingleDuctSysIndex;

        // Return value
        int AirLoopNumber = 0;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int BranchNum;
        int NumOfNodes;
        int NodeNum;
        int OutNum;
        int SupAirPath;
        int SupAirPathOutNodeNum;
        int CtrlZoneNum;
        int ZoneInNum;
        int ZoneOutNum;
        int AirLoopNum;
        int TUNum = 0;
        int TermNum = 0;
        bool LocalError;
        int NumOfComp;
        int NumOfSubComp;
        bool ErrorsFound;
        std::string TypeOfComp;
        std::string NameOfComp;
        int NumOfSubSubComp;

        for (AirLoopNum = 1; AirLoopNum <= NumPrimaryAirSys; ++AirLoopNum) {
            // Check OAMixer OA inlet node
            if (NodeNumber == state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).OAMixOAInNodeNum) {
                return AirLoopNum;
            }
            // Check branch
            for (BranchNum = 1; BranchNum <= state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).NumBranches; ++BranchNum) {
                NumOfNodes = state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).Branch(BranchNum).TotalNodes;
                for (NodeNum = 1; NodeNum <= NumOfNodes; ++NodeNum) {
                    if (NodeNumber == state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).Branch(BranchNum).NodeNum(NodeNum)) {
                        return AirLoopNum;
                    }
                }
                for (NumOfComp = 1; NumOfComp <= state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).Branch(BranchNum).TotalComponents;
                     ++NumOfComp) {
                    if (NodeNumber == state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).Branch(BranchNum).Comp(NumOfComp).NodeNumIn) {
                        return AirLoopNum;
                    }
                    if (NodeNumber == state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).Branch(BranchNum).Comp(NumOfComp).NodeNumOut) {
                        return AirLoopNum;
                    }
                    if (state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).Branch(BranchNum).Comp(NumOfComp).NumSubComps == 0) {
                        std::string TypeOfComp = state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).Branch(BranchNum).Comp(NumOfComp).TypeOf;
                        std::string NameOfComp = state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).Branch(BranchNum).Comp(NumOfComp).Name;
                        if (IsParentObject(state, TypeOfComp, NameOfComp)) {

                            int NumChildren = GetNumChildren(state, TypeOfComp, NameOfComp);
                            Array1D_string SubCompTypes;
                            Array1D_string SubCompNames;
                            Array1D_string InletNodeNames;
                            Array1D_int InletNodeNumbers;
                            Array1D_string OutletNodeNames;
                            Array1D_int OutletNodeNumbers;

                            SubCompTypes.allocate(NumChildren);
                            SubCompNames.allocate(NumChildren);
                            InletNodeNames.allocate(NumChildren);
                            InletNodeNumbers.allocate(NumChildren);
                            OutletNodeNames.allocate(NumChildren);
                            OutletNodeNumbers.allocate(NumChildren);

                            GetChildrenData(state,
                                            TypeOfComp,
                                            NameOfComp,
                                            NumChildren,
                                            SubCompTypes,
                                            SubCompNames,
                                            InletNodeNames,
                                            InletNodeNumbers,
                                            OutletNodeNames,
                                            OutletNodeNumbers,
                                            ErrorsFound);

                            for (NumOfSubComp = 1; NumOfSubComp <= NumChildren; ++NumOfSubComp) {
                                if (NodeNumber == InletNodeNumbers(NumOfSubComp)) {
                                    SubCompTypes.deallocate();
                                    SubCompNames.deallocate();
                                    InletNodeNames.deallocate();
                                    InletNodeNumbers.deallocate();
                                    OutletNodeNames.deallocate();
                                    OutletNodeNumbers.deallocate();
                                    return AirLoopNum;
                                }
                                if (NodeNumber == OutletNodeNumbers(NumOfSubComp)) {
                                    SubCompTypes.deallocate();
                                    SubCompNames.deallocate();
                                    InletNodeNames.deallocate();
                                    InletNodeNumbers.deallocate();
                                    OutletNodeNames.deallocate();
                                    OutletNodeNumbers.deallocate();
                                    return AirLoopNum;
                                }
                            }
                            for (NumOfSubComp = 1; NumOfSubComp <= NumChildren; ++NumOfSubComp) {
                                std::string TypeOfComp = SubCompTypes(NumOfSubComp);
                                std::string NameOfComp = SubCompNames(NumOfSubComp);
                                if (IsParentObject(state, TypeOfComp, NameOfComp)) {

                                    int NumGrandChildren = GetNumChildren(state, TypeOfComp, NameOfComp);
                                    Array1D_string SubSubCompTypes;
                                    Array1D_string SubSubCompNames;
                                    Array1D_string SubSubInletNodeNames;
                                    Array1D_int SubSubInletNodeNumbers;
                                    Array1D_string SubSubOutletNodeNames;
                                    Array1D_int SubSubOutletNodeNumbers;

                                    SubSubCompTypes.allocate(NumGrandChildren);
                                    SubSubCompNames.allocate(NumGrandChildren);
                                    SubSubInletNodeNames.allocate(NumGrandChildren);
                                    SubSubInletNodeNumbers.allocate(NumGrandChildren);
                                    SubSubOutletNodeNames.allocate(NumGrandChildren);
                                    SubSubOutletNodeNumbers.allocate(NumGrandChildren);

                                    GetChildrenData(state,
                                                    TypeOfComp,
                                                    NameOfComp,
                                                    NumGrandChildren,
                                                    SubSubCompTypes,
                                                    SubSubCompNames,
                                                    SubSubInletNodeNames,
                                                    SubSubInletNodeNumbers,
                                                    SubSubOutletNodeNames,
                                                    SubSubOutletNodeNumbers,
                                                    ErrorsFound);
                                    for (int SubSubCompNum = 1; SubSubCompNum <= NumGrandChildren; ++SubSubCompNum) {
                                        if (NodeNumber == SubSubInletNodeNumbers(SubSubCompNum)) {
                                            SubSubCompTypes.deallocate();
                                            SubSubCompNames.deallocate();
                                            SubSubInletNodeNames.deallocate();
                                            SubSubInletNodeNumbers.deallocate();
                                            SubSubOutletNodeNames.deallocate();
                                            SubSubOutletNodeNumbers.deallocate();
                                            SubCompTypes.deallocate();
                                            SubCompNames.deallocate();
                                            InletNodeNames.deallocate();
                                            InletNodeNumbers.deallocate();
                                            OutletNodeNames.deallocate();
                                            OutletNodeNumbers.deallocate();
                                            return AirLoopNum;
                                        }
                                        if (NodeNumber == SubSubOutletNodeNumbers(SubSubCompNum)) {
                                            SubSubCompTypes.deallocate();
                                            SubSubCompNames.deallocate();
                                            SubSubInletNodeNames.deallocate();
                                            SubSubInletNodeNumbers.deallocate();
                                            SubSubOutletNodeNames.deallocate();
                                            SubSubOutletNodeNumbers.deallocate();
                                            SubCompTypes.deallocate();
                                            SubCompNames.deallocate();
                                            InletNodeNames.deallocate();
                                            InletNodeNumbers.deallocate();
                                            OutletNodeNames.deallocate();
                                            OutletNodeNumbers.deallocate();
                                            return AirLoopNum;
                                        }
                                    }
                                    SubSubCompTypes.deallocate();
                                    SubSubCompNames.deallocate();
                                    SubSubInletNodeNames.deallocate();
                                    SubSubInletNodeNumbers.deallocate();
                                    SubSubOutletNodeNames.deallocate();
                                    SubSubOutletNodeNumbers.deallocate();
                                }
                            }

                            SubCompTypes.deallocate();
                            SubCompNames.deallocate();
                            InletNodeNames.deallocate();
                            InletNodeNumbers.deallocate();
                            OutletNodeNames.deallocate();
                            OutletNodeNumbers.deallocate();
                        }
                    } else {
                        for (NumOfSubComp = 1;
                             NumOfSubComp <= state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).Branch(BranchNum).Comp(NumOfComp).NumSubComps;
                             ++NumOfSubComp) {
                            if (NodeNumber == state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum)
                                                  .Branch(BranchNum)
                                                  .Comp(NumOfComp)
                                                  .SubComp(NumOfSubComp)
                                                  .NodeNumIn) {
                                return AirLoopNum;
                            }
                            if (NodeNumber == state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum)
                                                  .Branch(BranchNum)
                                                  .Comp(NumOfComp)
                                                  .SubComp(NumOfSubComp)
                                                  .NodeNumOut) {
                                return AirLoopNum;
                            }
                            for (NumOfSubSubComp = 1; NumOfSubSubComp <= state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum)
                                                                             .Branch(BranchNum)
                                                                             .Comp(NumOfComp)
                                                                             .SubComp(NumOfSubComp)
                                                                             .NumSubSubComps;
                                 ++NumOfSubSubComp) {
                                if (NodeNumber == state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum)
                                                      .Branch(BranchNum)
                                                      .Comp(NumOfComp)
                                                      .SubComp(NumOfSubComp)
                                                      .SubSubComp(NumOfSubSubComp)
                                                      .NodeNumIn) {
                                    return AirLoopNum;
                                }
                                if (NodeNumber == state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum)
                                                      .Branch(BranchNum)
                                                      .Comp(NumOfComp)
                                                      .SubComp(NumOfSubComp)
                                                      .SubSubComp(NumOfSubSubComp)
                                                      .NodeNumOut) {
                                    return AirLoopNum;
                                }
                            }
                        }
                    }
                }
            }

            // Check connection between supply and demand
            for (OutNum = 1; OutNum <= state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).NumSupplyNodes; ++OutNum) {
                // AirLoop supply outlet node
                if (state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).AirLoopSupplyNodeNum(OutNum) == NodeNumber) {
                    return AirLoopNum;
                }
                if (state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).ZoneEquipSupplyNodeNum(OutNum) == NodeNumber) {
                    return AirLoopNum;
                }
                // supply path
                for (SupAirPath = 1; SupAirPath <= state.dataZoneEquip->NumSupplyAirPaths; ++SupAirPath) {
                    if (state.dataZoneEquip->SupplyAirPath(SupAirPath).InletNodeNum ==
                        state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).ZoneEquipSupplyNodeNum(OutNum)) {
                        for (SupAirPathOutNodeNum = 1; SupAirPathOutNodeNum <= state.dataZoneEquip->SupplyAirPath(SupAirPath).NumOutletNodes;
                             ++SupAirPathOutNodeNum) {
                            if (state.dataZoneEquip->SupplyAirPath(SupAirPath).OutletNode(SupAirPathOutNodeNum) == NodeNumber) {
                                return AirLoopNum;
                            }
                            for (TUNum = 1; TUNum <= state.dataAirflowNetworkBalanceManager->DisSysNumOfTermUnits; ++TUNum) {
                                if (UtilityRoutines::SameString(state.dataAirflowNetwork->DisSysCompTermUnitData(TUNum).EPlusType,
                                                                "AirTerminal:SingleDuct:VAV:Reheat")) {
                                    LocalError = false;
                                    GetHVACSingleDuctSysIndex(state,
                                                              state.dataAirflowNetwork->DisSysCompTermUnitData(TUNum).name,
                                                              TermNum,
                                                              LocalError,
                                                              "AirflowNetwork:Distribution:Component:TerminalUnit",
                                                              state.dataAirflowNetwork->DisSysCompTermUnitData(TUNum).DamperInletNode,
                                                              state.dataAirflowNetwork->DisSysCompTermUnitData(TUNum).DamperOutletNode);
                                    if (state.dataZoneEquip->SupplyAirPath(SupAirPath).OutletNode(SupAirPathOutNodeNum) ==
                                        state.dataAirflowNetwork->DisSysCompTermUnitData(TUNum).DamperInletNode) {
                                        if (state.dataAirflowNetwork->DisSysCompTermUnitData(TUNum).DamperOutletNode == NodeNumber) {
                                            state.dataAirflowNetwork->DisSysCompTermUnitData(TUNum).AirLoopNum = AirLoopNum;
                                            return AirLoopNum;
                                        }
                                    }
                                    if (LocalError) {
                                    }
                                }
                            }
                        }
                    }
                }
                // return path
                if (state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).AirLoopReturnNodeNum(OutNum) == NodeNumber) {
                    return AirLoopNum;
                }
                if (state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).ZoneEquipReturnNodeNum(OutNum) == NodeNumber) {
                    return AirLoopNum;
                }
                for (int retPathNum = 1; retPathNum <= state.dataZoneEquip->NumReturnAirPaths; ++retPathNum) {
                    if (state.dataZoneEquip->ReturnAirPath(retPathNum).OutletNodeNum ==
                        state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum).ZoneEquipReturnNodeNum(1)) {
                        if (state.dataZoneEquip->ReturnAirPath(retPathNum).OutletNodeNum == NodeNumber) {
                            return AirLoopNum;
                        }
                    }
                }
                // Supply inlet node

                // Terminal damper node
            }
        }

        for (CtrlZoneNum = 1; CtrlZoneNum <= state.dataGlobal->NumOfZones; ++CtrlZoneNum) {
            if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).IsControlled) continue;
            for (ZoneInNum = 1; ZoneInNum <= state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).NumInletNodes; ++ZoneInNum) {
                if (state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).InletNode(ZoneInNum) == NodeNumber) {
                    return state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).InletNodeAirLoopNum(ZoneInNum);
                }
            }
            for (ZoneOutNum = 1; ZoneOutNum <= state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).NumReturnNodes; ++ZoneOutNum) {
                if (state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).ReturnNode(ZoneOutNum) == NodeNumber) {
                    return state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).ReturnNodeAirLoopNum(ZoneOutNum);
                }
            }
        }

        return AirLoopNumber;
    }

    void OccupantVentilationControlProp::calc(EnergyPlusData &state,
                                              int const ZoneNum,
                                              Real64 const TimeOpenDuration,
                                              Real64 const TimeCloseDuration,
                                              int &OpeningStatus,
                                              int &OpeningProbStatus,
                                              int &ClosingProbStatus)
    {

        Real64 Tcomfort;    // Thermal comfort temperature
        Real64 ComfortBand; // Thermal comfort band
        Real64 Toperative;  // Operative temperature
        Real64 OutDryBulb;  // Outdoor dry-bulb temperature

        auto &Zone(state.dataHeatBal->Zone);

        if (TimeOpenDuration > 0) {
            if (TimeOpenDuration >= MinOpeningTime) {
                OpeningStatus = OpenStatus::FreeOperation; // free operation
            } else {
                OpeningStatus = OpenStatus::MinCheckForceOpen; // forced to open
            }
        }
        if (TimeCloseDuration > 0) {
            if (TimeCloseDuration >= MinClosingTime) {
                OpeningStatus = OpenStatus::FreeOperation; // free operation
            } else {
                OpeningStatus = OpenStatus::MinCheckForceClose; // forced to close
            }
        }

        if (MinTimeControlOnly) return;

        if (Zone(ZoneNum).HasLinkedOutAirNode) {
            OutDryBulb = Zone(ZoneNum).OutDryBulbTemp;
        } else {
            OutDryBulb = OutDryBulbTempAt(state, Zone(ZoneNum).Centroid.z);
        }

        if (OutDryBulb < ComfortBouPoint) {
            Tcomfort = CurveValue(state, ComfortLowTempCurveNum, OutDryBulb);
        } else {
            Tcomfort = CurveValue(state, ComfortHighTempCurveNum, OutDryBulb);
        }
        ComfortBand = -0.0028 * (100 - MaxPPD) * (100 - MaxPPD) + 0.3419 * (100 - MaxPPD) - 6.6275;
        Toperative = 0.5 * (state.dataHeatBalFanSys->MAT(ZoneNum) + state.dataHeatBal->ZoneMRT(ZoneNum));

        if (Toperative > (Tcomfort + ComfortBand)) {
            if (openingProbability(state, ZoneNum, TimeCloseDuration)) {
                OpeningProbStatus = ProbabilityCheck::ForceChange;
                ; // forced to open
            } else {
                OpeningProbStatus = ProbabilityCheck::KeepStatus; // Keep previous status
            }
        } else {
            OpeningProbStatus = ProbabilityCheck::NoAction; // free operation
        }

        if (Toperative < (Tcomfort - ComfortBand)) {
            if (closingProbability(state, TimeOpenDuration)) {
                ClosingProbStatus = ProbabilityCheck::ForceChange; // forced to close
            } else {
                ClosingProbStatus = ProbabilityCheck::KeepStatus; // Keep previous status
            }
        } else {
            ClosingProbStatus = ProbabilityCheck::NoAction; // free operation
        }
    }

    bool OccupantVentilationControlProp::openingProbability(EnergyPlusData &state,
                                                            int const ZoneNum,
                                                            Real64 const TimeCloseDuration) // function to perform calculations of opening probability
    {
        using DataHVACGlobals::DualSetPointWithDeadBand;
        using DataHVACGlobals::SingleCoolingSetPoint;
        using DataHVACGlobals::SingleHeatCoolSetPoint;
        using DataHVACGlobals::SingleHeatingSetPoint;

        Real64 SchValue;
        Real64 RandomValue;

        if (TimeCloseDuration < MinClosingTime) {
            return false;
        }
        if (OccupancyCheck) {
            if (state.dataHeatBal->ZoneIntGain(ZoneNum).NOFOCC <= 0.0) {
                return false;
            }
        }

        {
            auto const SELECT_CASE_var(state.dataHeatBalFanSys->TempControlType(ZoneNum)); // Check zone setpoints
            if (SELECT_CASE_var == 0) {                                                    // Uncontrolled

            } else if (SELECT_CASE_var == SingleHeatingSetPoint) {
                if (state.dataHeatBalFanSys->MAT(ZoneNum) <= state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ZoneNum)) {
                    return false;
                }
            } else if (SELECT_CASE_var == SingleCoolingSetPoint) {
                if (state.dataHeatBalFanSys->MAT(ZoneNum) >= state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ZoneNum)) {
                    return false;
                }
            } else if (SELECT_CASE_var == SingleHeatCoolSetPoint) {
                return false;
            } else if (SELECT_CASE_var == DualSetPointWithDeadBand) {
                if (state.dataHeatBalFanSys->MAT(ZoneNum) < state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ZoneNum) ||
                    state.dataHeatBalFanSys->MAT(ZoneNum) > state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ZoneNum)) {
                    return false;
                }
            }
        }

        if (OpeningProbSchNum == 0) {
            return true;
        } else {
            SchValue = GetCurrentScheduleValue(state, OpeningProbSchNum);
            RandomValue = Real64(rand()) / RAND_MAX;
            if (SchValue > RandomValue) {
                return true;
            } else {
                return false;
            }
        }
    }

    bool OccupantVentilationControlProp::closingProbability(EnergyPlusData &state,
                                                            Real64 const TimeOpenDuration) // function to perform calculations of closing probability
    {
        Real64 SchValue;
        Real64 RandomValue;

        if (TimeOpenDuration < MinOpeningTime) {
            return false;
        }
        if (ClosingProbSchNum == 0) {
            return true;
        } else {
            SchValue = GetCurrentScheduleValue(state, ClosingProbSchNum);
            RandomValue = Real64(rand()) / RAND_MAX;
            if (SchValue > RandomValue) {
                return true;
            } else {
                return false;
            }
        }
    }

} // namespace AirflowNetworkBalanceManager

} // namespace EnergyPlus
