// EnergyPlus, Copyright (c) 1996-2023, The Board of Trustees of the University of Illinois,
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
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <AirflowNetwork/Solver.hpp>
#include <EnergyPlus/Coils/CoilCoolingDX.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataAirLoop.hh>
#include <EnergyPlus/DataAirSystems.hh>
#include <EnergyPlus/DataConvergParams.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataReportingFlags.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataSystemVariables.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/DemandManager.hh>
#include <EnergyPlus/DisplayRoutines.hh>
#include <EnergyPlus/EMSManager.hh>
#include <EnergyPlus/ElectricPowerServiceManager.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/HVACManager.hh>
#include <EnergyPlus/HVACSizingSimulationManager.hh>
#include <EnergyPlus/IceThermalStorage.hh>
#include <EnergyPlus/InternalHeatGains.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/NonZoneEquipmentManager.hh>
#include <EnergyPlus/OutAirNodeManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutputReportTabular.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/Plant/PlantManager.hh>
#include <EnergyPlus/PlantCondLoopOperation.hh>
#include <EnergyPlus/PlantLoopHeatPumpEIR.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/PollutionModule.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/RefrigeratedCase.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SetPointManager.hh>
#include <EnergyPlus/SimAirServingZones.hh>
#include <EnergyPlus/SizingManager.hh>
#include <EnergyPlus/SystemAvailabilityManager.hh>
#include <EnergyPlus/SystemReports.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/WaterManager.hh>
#include <EnergyPlus/ZoneContaminantPredictorCorrector.hh>
#include <EnergyPlus/ZoneEquipmentManager.hh>
#include <EnergyPlus/ZoneTempPredictorCorrector.hh>

namespace EnergyPlus::HVACManager {

// PURPOSE OF THIS MODULE:
// This module contains the high level HVAC control
// subroutines.  Subroutine ManageHVAC, which is called from the heat balance,
// calls the HVAC simulation and is the most probable insertion point for
// connections to other HVAC engines.  ManageHVAC also controls the system
// timestep, automatically shortening the timestep to meet convergence criteria.

// METHODOLOGY EMPLOYED:
// The basic solution technique is iteration with lagging.
// The timestep is shortened using a bisection method.

static constexpr std::array<Real64, DataPlant::NumConvergenceHistoryTerms> ConvergenceHistoryARR = {0.0, -1.0, -2.0, -3.0, -4.0};
constexpr Real64 sum_ConvergenceHistoryARR(-10.0);
constexpr Real64 square_sum_ConvergenceHistoryARR(100.0);
constexpr Real64 sum_square_ConvergenceHistoryARR(30.0);

void ManageHVAC(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHORS:  Russ Taylor, Dan Fisher
    //       DATE WRITTEN:  Jan. 1998
    //       MODIFIED       Jul 2003 (CC) added a subroutine call for air models
    //       RE-ENGINEERED  May 2008, Brent Griffith, revised variable time step method and zone conditions history

    // PURPOSE OF THIS SUBROUTINE:
    // This routine effectively replaces the IBLAST
    // "SystemDriver" routine.  The main function of the routine
    // is to set the system timestep, "TimeStepSys", call the models related to zone
    // air temperatures, and .

    // METHODOLOGY EMPLOYED:
    //  manage calls to Predictor and Corrector and other updates in ZoneTempPredictorCorrector
    //  manage variable time step and when zone air histories are updated.

    // SUBROUTINE PARAMETER DEFINITIONS:
    static constexpr std::string_view EndOfHeaderString("End of Data Dictionary");                          // End of data dictionary marker
    static constexpr std::string_view EnvironmentStampFormatStr("{},{},{:7.2F},{:7.2F},{:7.2F},{:7.2F}\n"); // Format descriptor for environ stamp

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 PriorTimeStep;       // magnitude of time step for previous history terms
    Real64 ZoneTempChange(0.0); // change in zone air temperature from timestep t-1 to t

    // SYSTEM INITIALIZATION
    if (state.dataHVACMgr->TriggerGetAFN) {
        state.dataHVACMgr->TriggerGetAFN = false;
        DisplayString(state, "Initializing HVAC");
        state.afn->manage_balance(); // first call only gets input and returns.
    }

    for (auto &thisZoneHB : state.dataZoneTempPredictorCorrector->zoneHeatBalance) {
        thisZoneHB.ZT = thisZoneHB.MAT;
        // save for use with thermal comfort control models (Fang, Pierce, and KSU)
        thisZoneHB.ZTAV = 0.0;
        thisZoneHB.airHumRatAvg = 0.0;
    }
    for (auto &thisSpaceHB : state.dataZoneTempPredictorCorrector->spaceHeatBalance) {
        thisSpaceHB.ZT = thisSpaceHB.MAT;
        // save for use with thermal comfort control models (Fang, Pierce, and KSU)
        thisSpaceHB.ZTAV = 0.0;
        thisSpaceHB.airHumRatAvg = 0.0;
    }
    state.dataHeatBalFanSys->ZoneThermostatSetPointHiAver = 0.0;
    state.dataHeatBalFanSys->ZoneThermostatSetPointLoAver = 0.0;
    state.dataHVACMgr->PrintedWarmup = false;
    if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
        state.dataContaminantBalance->OutdoorCO2 =
            ScheduleManager::GetCurrentScheduleValue(state, state.dataContaminantBalance->Contaminant.CO2OutdoorSchedPtr);
        state.dataContaminantBalance->ZoneAirCO2Avg = 0.0;
    }
    if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
        state.dataContaminantBalance->OutdoorGC =
            ScheduleManager::GetCurrentScheduleValue(state, state.dataContaminantBalance->Contaminant.GenericContamOutdoorSchedPtr);
        if (allocated(state.dataContaminantBalance->ZoneAirGCAvg)) state.dataContaminantBalance->ZoneAirGCAvg = 0.0;
    }

    if (state.dataGlobal->BeginEnvrnFlag && state.dataHVACMgr->MyEnvrnFlag) {
        state.dataHVACGlobal->AirLoopsSimOnce = false;
        state.dataHVACMgr->MyEnvrnFlag = false;
        state.dataHVACGlobal->NumOfSysTimeStepsLastZoneTimeStep = 1;
        state.dataHVACGlobal->PreviousTimeStep = state.dataGlobal->TimeStepZone;
    }
    if (!state.dataGlobal->BeginEnvrnFlag) {
        state.dataHVACMgr->MyEnvrnFlag = true;
    }

    state.dataHeatBalFanSys->QRadSurfAFNDuct = 0.0;

    state.dataHVACGlobal->SysTimeElapsed = 0.0;
    state.dataHVACGlobal->TimeStepSys = state.dataGlobal->TimeStepZone;
    state.dataHVACGlobal->TimeStepSysSec = state.dataHVACGlobal->TimeStepSys * Constant::SecInHour;
    state.dataHVACGlobal->FirstTimeStepSysFlag = true;
    state.dataHVACGlobal->ShortenTimeStepSys = false;
    state.dataHVACGlobal->UseZoneTimeStepHistory = true;
    PriorTimeStep = state.dataGlobal->TimeStepZone;
    state.dataHVACGlobal->NumOfSysTimeSteps = 1;
    state.dataHVACGlobal->FracTimeStepZone = state.dataHVACGlobal->TimeStepSys / state.dataGlobal->TimeStepZone;

    bool anyEMSRan;
    EMSManager::ManageEMS(state, EMSManager::EMSCallFrom::BeginTimestepBeforePredictor, anyEMSRan, ObjexxFCL::Optional_int_const()); // calling point

    OutAirNodeManager::SetOutAirNodes(state);

    RefrigeratedCase::ManageRefrigeratedCaseRacks(state);

    // ZONE INITIALIZATION  'Get Zone Setpoints'
    ZoneTempPredictorCorrector::ManageZoneAirUpdates(state,
                                                     DataHeatBalFanSys::PredictorCorrectorCtrl::GetZoneSetPoints,
                                                     ZoneTempChange,
                                                     state.dataHVACGlobal->ShortenTimeStepSys,
                                                     state.dataHVACGlobal->UseZoneTimeStepHistory,
                                                     PriorTimeStep);
    if (state.dataContaminantBalance->Contaminant.SimulateContaminants)
        ZoneContaminantPredictorCorrector::ManageZoneContaminanUpdates(state,
                                                                       DataHeatBalFanSys::PredictorCorrectorCtrl::GetZoneSetPoints,
                                                                       state.dataHVACGlobal->ShortenTimeStepSys,
                                                                       state.dataHVACGlobal->UseZoneTimeStepHistory,
                                                                       PriorTimeStep);

    SystemAvailabilityManager::ManageHybridVentilation(state);

    ZoneEquipmentManager::CalcAirFlowSimple(state);
    if (state.afn->simulation_control.type != AirflowNetwork::ControlType::NoMultizoneOrDistribution) {
        state.afn->RollBackFlag = false;
        state.afn->manage_balance(false);
    }

    SetHeatToReturnAirFlag(state);

    for (int zoneNum = 1; zoneNum <= state.dataGlobal->NumOfZones; ++zoneNum) {
        auto &thisZoneHB = state.dataZoneTempPredictorCorrector->zoneHeatBalance(zoneNum);
        thisZoneHB.SysDepZoneLoadsLagged = thisZoneHB.SysDepZoneLoads;
        if (state.dataHeatBal->doSpaceHeatBalance) {
            for (int spaceNum : state.dataHeatBal->Zone(zoneNum).spaceIndexes) {
                // SpaceHB ToDo: For now allocate by space volume frac
                state.dataZoneTempPredictorCorrector->spaceHeatBalance(spaceNum).SysDepZoneLoadsLagged =
                    thisZoneHB.SysDepZoneLoads * state.dataHeatBal->space(spaceNum).fracZoneVolume;
            }
        }
    }

    InternalHeatGains::UpdateInternalGainValues(state, true, true);

    ZoneTempPredictorCorrector::ManageZoneAirUpdates(state,
                                                     DataHeatBalFanSys::PredictorCorrectorCtrl::PredictStep,
                                                     ZoneTempChange,
                                                     state.dataHVACGlobal->ShortenTimeStepSys,
                                                     state.dataHVACGlobal->UseZoneTimeStepHistory,
                                                     PriorTimeStep);

    if (state.dataContaminantBalance->Contaminant.SimulateContaminants)
        ZoneContaminantPredictorCorrector::ManageZoneContaminanUpdates(state,
                                                                       DataHeatBalFanSys::PredictorCorrectorCtrl::PredictStep,
                                                                       state.dataHVACGlobal->ShortenTimeStepSys,
                                                                       state.dataHVACGlobal->UseZoneTimeStepHistory,
                                                                       PriorTimeStep);

    SimHVAC(state);

    if (state.dataGlobal->AnyIdealCondEntSetPointInModel && state.dataGlobal->MetersHaveBeenInitialized && !state.dataGlobal->WarmupFlag) {
        state.dataGlobal->RunOptCondEntTemp = true;
        while (state.dataGlobal->RunOptCondEntTemp) {
            SimHVAC(state);
        }
    }

    WaterManager::ManageWaterInits(state);

    // Only simulate once per zone timestep; must be after SimHVAC
    if (state.dataHVACGlobal->FirstTimeStepSysFlag && state.dataGlobal->MetersHaveBeenInitialized) {
        DemandManager::ManageDemand(state);
    }

    state.dataGlobal->BeginTimeStepFlag = false; // At this point, we have been through the first pass through SimHVAC so this needs to be set

    ZoneTempPredictorCorrector::ManageZoneAirUpdates(state,
                                                     DataHeatBalFanSys::PredictorCorrectorCtrl::CorrectStep,
                                                     ZoneTempChange,
                                                     state.dataHVACGlobal->ShortenTimeStepSys,
                                                     state.dataHVACGlobal->UseZoneTimeStepHistory,
                                                     PriorTimeStep);
    if (state.dataContaminantBalance->Contaminant.SimulateContaminants)
        ZoneContaminantPredictorCorrector::ManageZoneContaminanUpdates(state,
                                                                       DataHeatBalFanSys::PredictorCorrectorCtrl::CorrectStep,
                                                                       state.dataHVACGlobal->ShortenTimeStepSys,
                                                                       state.dataHVACGlobal->UseZoneTimeStepHistory,
                                                                       PriorTimeStep);

    if (ZoneTempChange > state.dataConvergeParams->MaxZoneTempDiff && !state.dataGlobal->KickOffSimulation) {
        // determine value of adaptive system time step
        // model how many system timesteps we want in zone timestep
        int ZTempTrendsNumSysSteps = int(ZoneTempChange / state.dataConvergeParams->MaxZoneTempDiff + 1.0); // add 1 for truncation
        state.dataHVACGlobal->NumOfSysTimeSteps = min(ZTempTrendsNumSysSteps, state.dataHVACGlobal->LimitNumSysSteps);
        // then determine timestep length for even distribution, protect div by zero
        if (state.dataHVACGlobal->NumOfSysTimeSteps > 0) {
            state.dataHVACGlobal->TimeStepSys = state.dataGlobal->TimeStepZone / state.dataHVACGlobal->NumOfSysTimeSteps;
        }
        state.dataHVACGlobal->TimeStepSys = max(state.dataHVACGlobal->TimeStepSys, state.dataConvergeParams->MinTimeStepSys);
        state.dataHVACGlobal->TimeStepSysSec = state.dataHVACGlobal->TimeStepSys * Constant::SecInHour;
        state.dataHVACGlobal->UseZoneTimeStepHistory = false;
        state.dataHVACGlobal->ShortenTimeStepSys = true;

    } else {
        state.dataHVACGlobal->NumOfSysTimeSteps = 1;
        state.dataHVACGlobal->UseZoneTimeStepHistory = true;
    }

    if (state.dataHVACGlobal->UseZoneTimeStepHistory) state.dataHVACGlobal->PreviousTimeStep = state.dataGlobal->TimeStepZone;
    for (int SysTimestepLoop = 1; SysTimestepLoop <= state.dataHVACGlobal->NumOfSysTimeSteps; ++SysTimestepLoop) {
        if (state.dataGlobal->stopSimulation) break;

        if (state.dataHVACGlobal->TimeStepSys < state.dataGlobal->TimeStepZone) {

            SystemAvailabilityManager::ManageHybridVentilation(state);
            ZoneEquipmentManager::CalcAirFlowSimple(state, SysTimestepLoop);
            if (state.afn->simulation_control.type != AirflowNetwork::ControlType::NoMultizoneOrDistribution) {
                state.afn->RollBackFlag = false;
                state.afn->manage_balance(false);
            }

            InternalHeatGains::UpdateInternalGainValues(state, true, true);

            ZoneTempPredictorCorrector::ManageZoneAirUpdates(state,
                                                             DataHeatBalFanSys::PredictorCorrectorCtrl::PredictStep,
                                                             ZoneTempChange,
                                                             state.dataHVACGlobal->ShortenTimeStepSys,
                                                             state.dataHVACGlobal->UseZoneTimeStepHistory,
                                                             PriorTimeStep);

            if (state.dataContaminantBalance->Contaminant.SimulateContaminants)
                ZoneContaminantPredictorCorrector::ManageZoneContaminanUpdates(state,
                                                                               DataHeatBalFanSys::PredictorCorrectorCtrl::PredictStep,
                                                                               state.dataHVACGlobal->ShortenTimeStepSys,
                                                                               state.dataHVACGlobal->UseZoneTimeStepHistory,
                                                                               PriorTimeStep);
            SimHVAC(state);

            if (state.dataGlobal->AnyIdealCondEntSetPointInModel && state.dataGlobal->MetersHaveBeenInitialized && !state.dataGlobal->WarmupFlag) {
                state.dataGlobal->RunOptCondEntTemp = true;
                while (state.dataGlobal->RunOptCondEntTemp) {
                    SimHVAC(state);
                }
            }

            WaterManager::ManageWaterInits(state);

            // Need to set the flag back since we do not need to shift the temps back again in the correct step.
            state.dataHVACGlobal->ShortenTimeStepSys = false;

            ZoneTempPredictorCorrector::ManageZoneAirUpdates(state,
                                                             DataHeatBalFanSys::PredictorCorrectorCtrl::CorrectStep,
                                                             ZoneTempChange,
                                                             state.dataHVACGlobal->ShortenTimeStepSys,
                                                             state.dataHVACGlobal->UseZoneTimeStepHistory,
                                                             PriorTimeStep);
            if (state.dataContaminantBalance->Contaminant.SimulateContaminants)
                ZoneContaminantPredictorCorrector::ManageZoneContaminanUpdates(state,
                                                                               DataHeatBalFanSys::PredictorCorrectorCtrl::CorrectStep,
                                                                               state.dataHVACGlobal->ShortenTimeStepSys,
                                                                               state.dataHVACGlobal->UseZoneTimeStepHistory,
                                                                               PriorTimeStep);

            ZoneTempPredictorCorrector::ManageZoneAirUpdates(state,
                                                             DataHeatBalFanSys::PredictorCorrectorCtrl::PushSystemTimestepHistories,
                                                             ZoneTempChange,
                                                             state.dataHVACGlobal->ShortenTimeStepSys,
                                                             state.dataHVACGlobal->UseZoneTimeStepHistory,
                                                             PriorTimeStep);
            if (state.dataContaminantBalance->Contaminant.SimulateContaminants)
                ZoneContaminantPredictorCorrector::ManageZoneContaminanUpdates(state,
                                                                               DataHeatBalFanSys::PredictorCorrectorCtrl::PushSystemTimestepHistories,
                                                                               state.dataHVACGlobal->ShortenTimeStepSys,
                                                                               state.dataHVACGlobal->UseZoneTimeStepHistory,
                                                                               PriorTimeStep);
            state.dataHVACGlobal->PreviousTimeStep = state.dataHVACGlobal->TimeStepSys;
        }

        state.dataHVACGlobal->FracTimeStepZone = state.dataHVACGlobal->TimeStepSys / state.dataGlobal->TimeStepZone;

        for (int ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
            auto &thisZoneHB = state.dataZoneTempPredictorCorrector->zoneHeatBalance(ZoneNum);
            thisZoneHB.ZTAV += thisZoneHB.ZT * state.dataHVACGlobal->FracTimeStepZone;
            thisZoneHB.airHumRatAvg += thisZoneHB.airHumRat * state.dataHVACGlobal->FracTimeStepZone;
            // Space temps are always used, regardless of doSpaceHeatBalance setting
            for (int spaceNum : state.dataHeatBal->Zone(ZoneNum).spaceIndexes) {
                auto &thisSpaceHB = state.dataZoneTempPredictorCorrector->spaceHeatBalance(spaceNum);
                thisSpaceHB.ZTAV += thisSpaceHB.ZT * state.dataHVACGlobal->FracTimeStepZone;
                thisSpaceHB.airHumRatAvg += thisSpaceHB.airHumRat * state.dataHVACGlobal->FracTimeStepZone;
            }
            if (state.dataContaminantBalance->Contaminant.CO2Simulation)
                state.dataContaminantBalance->ZoneAirCO2Avg(ZoneNum) +=
                    state.dataContaminantBalance->ZoneAirCO2(ZoneNum) * state.dataHVACGlobal->FracTimeStepZone;
            if (state.dataContaminantBalance->Contaminant.GenericContamSimulation)
                state.dataContaminantBalance->ZoneAirGCAvg(ZoneNum) +=
                    state.dataContaminantBalance->ZoneAirGC(ZoneNum) * state.dataHVACGlobal->FracTimeStepZone;
            if (state.dataZoneTempPredictorCorrector->NumOnOffCtrZone > 0) {
                state.dataHeatBalFanSys->ZoneThermostatSetPointHiAver(ZoneNum) +=
                    state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ZoneNum) * state.dataHVACGlobal->FracTimeStepZone;
                state.dataHeatBalFanSys->ZoneThermostatSetPointLoAver(ZoneNum) +=
                    state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ZoneNum) * state.dataHVACGlobal->FracTimeStepZone;
            }
        }

        ZoneTempPredictorCorrector::DetectOscillatingZoneTemp(state);
        UpdateZoneListAndGroupLoads(state);           // Must be called before UpdateDataandReport(OutputProcessor::TimeStepType::TimeStepSystem)
        IceThermalStorage::UpdateIceFractions(state); // Update fraction of ice stored in TES
        WaterManager::ManageWater(state);
        // update electricity data for net, purchased, sold etc.
        bool DummyLogical = false;
        state.dataElectPwrSvcMgr->facilityElectricServiceObj->manageElectricPowerService(state, false, DummyLogical, true);

        // Update the plant and condenser loop capacitance model temperature history.
        PlantManager::UpdateNodeThermalHistory(state);

        if (state.dataOutRptTab->displayHeatEmissionsSummary) {
            OutputReportTabular::CalcHeatEmissionReport(state);
        }

        EMSManager::ManageEMS(
            state, EMSManager::EMSCallFrom::EndSystemTimestepBeforeHVACReporting, anyEMSRan, ObjexxFCL::Optional_int_const()); // EMS calling point

        // This is where output processor data is updated for System Timestep reporting
        if (!state.dataGlobal->WarmupFlag) {
            if (state.dataGlobal->DoOutputReporting && !state.dataGlobal->ZoneSizingCalc) {
                NodeInputManager::CalcMoreNodeInfo(state);
                PollutionModule::CalculatePollution(state);
                SystemReports::InitEnergyReports(state);
                SystemReports::ReportSystemEnergyUse(state);
            }
            if (state.dataGlobal->DoOutputReporting || (state.dataGlobal->ZoneSizingCalc && state.dataGlobal->CompLoadReportIsReq)) {
                ReportAirHeatBalance(state);
                if (state.dataGlobal->ZoneSizingCalc) OutputReportTabular::GatherComponentLoadsHVAC(state);
            }
            if (state.dataGlobal->DoOutputReporting) {
                SystemReports::ReportVentilationLoads(state);
                UpdateDataandReport(state, OutputProcessor::TimeStepType::System);
                if (state.dataGlobal->KindOfSim == Constant::KindOfSim::HVACSizeDesignDay ||
                    state.dataGlobal->KindOfSim == Constant::KindOfSim::HVACSizeRunPeriodDesign) {
                    if (state.dataHVACSizingSimMgr->hvacSizingSimulationManager)
                        state.dataHVACSizingSimMgr->hvacSizingSimulationManager->UpdateSizingLogsSystemStep(state);
                }
                OutputReportTabular::UpdateTabularReports(state, OutputProcessor::TimeStepType::System);
            }
            if (state.dataGlobal->ZoneSizingCalc) {
                ZoneEquipmentManager::UpdateZoneSizing(state, Constant::CallIndicator::DuringDay);
                SizingManager::UpdateFacilitySizing(state, Constant::CallIndicator::DuringDay);
            }
            EIRPlantLoopHeatPumps::EIRPlantLoopHeatPump::checkConcurrentOperation(state);
        } else if (!state.dataGlobal->KickOffSimulation && state.dataGlobal->DoOutputReporting && state.dataSysVars->ReportDuringWarmup) {
            if (state.dataGlobal->BeginDayFlag && !state.dataEnvrn->PrintEnvrnStampWarmupPrinted) {
                state.dataEnvrn->PrintEnvrnStampWarmup = true;
                state.dataEnvrn->PrintEnvrnStampWarmupPrinted = true;
            }
            if (!state.dataGlobal->BeginDayFlag) state.dataEnvrn->PrintEnvrnStampWarmupPrinted = false;
            if (state.dataEnvrn->PrintEnvrnStampWarmup) {
                if (state.dataReportFlag->PrintEndDataDictionary && state.dataGlobal->DoOutputReporting && !state.dataHVACMgr->PrintedWarmup) {
                    print(state.files.eso, "{}\n", EndOfHeaderString);
                    print(state.files.mtr, "{}\n", EndOfHeaderString);
                    state.dataReportFlag->PrintEndDataDictionary = false;
                }
                if (state.dataGlobal->DoOutputReporting && !state.dataHVACMgr->PrintedWarmup) {

                    print(state.files.eso,
                          EnvironmentStampFormatStr,
                          "1",
                          "Warmup {" + state.dataReportFlag->cWarmupDay + "} " + state.dataEnvrn->EnvironmentName,
                          state.dataEnvrn->Latitude,
                          state.dataEnvrn->Longitude,
                          state.dataEnvrn->TimeZoneNumber,
                          state.dataEnvrn->Elevation);
                    print(state.files.mtr,
                          EnvironmentStampFormatStr,
                          "1",
                          "Warmup {" + state.dataReportFlag->cWarmupDay + "} " + state.dataEnvrn->EnvironmentName,
                          state.dataEnvrn->Latitude,
                          state.dataEnvrn->Longitude,
                          state.dataEnvrn->TimeZoneNumber,
                          state.dataEnvrn->Elevation);
                    state.dataEnvrn->PrintEnvrnStampWarmup = false;
                }
                state.dataHVACMgr->PrintedWarmup = true;
            }
            if (!state.dataGlobal->DoingSizing) {
                NodeInputManager::CalcMoreNodeInfo(state);
            }
            UpdateDataandReport(state, OutputProcessor::TimeStepType::System);
            if (state.dataGlobal->KindOfSim == Constant::KindOfSim::HVACSizeDesignDay ||
                state.dataGlobal->KindOfSim == Constant::KindOfSim::HVACSizeRunPeriodDesign) {
                if (state.dataHVACSizingSimMgr->hvacSizingSimulationManager)
                    state.dataHVACSizingSimMgr->hvacSizingSimulationManager->UpdateSizingLogsSystemStep(state);
            }
        } else if (state.dataSysVars->UpdateDataDuringWarmupExternalInterface) { // added for FMI
            if (state.dataGlobal->BeginDayFlag && !state.dataEnvrn->PrintEnvrnStampWarmupPrinted) {
                state.dataEnvrn->PrintEnvrnStampWarmup = true;
                state.dataEnvrn->PrintEnvrnStampWarmupPrinted = true;
            }
            if (!state.dataGlobal->BeginDayFlag) state.dataEnvrn->PrintEnvrnStampWarmupPrinted = false;
            if (state.dataEnvrn->PrintEnvrnStampWarmup) {
                if (state.dataReportFlag->PrintEndDataDictionary && state.dataGlobal->DoOutputReporting && !state.dataHVACMgr->PrintedWarmup) {
                    print(state.files.eso, "{}\n", EndOfHeaderString);
                    print(state.files.mtr, "{}\n", EndOfHeaderString);
                    state.dataReportFlag->PrintEndDataDictionary = false;
                }
                if (state.dataGlobal->DoOutputReporting && !state.dataHVACMgr->PrintedWarmup) {
                    print(state.files.eso,
                          EnvironmentStampFormatStr,
                          "1",
                          "Warmup {" + state.dataReportFlag->cWarmupDay + "} " + state.dataEnvrn->EnvironmentName,
                          state.dataEnvrn->Latitude,
                          state.dataEnvrn->Longitude,
                          state.dataEnvrn->TimeZoneNumber,
                          state.dataEnvrn->Elevation);
                    print(state.files.mtr,
                          EnvironmentStampFormatStr,
                          "1",
                          "Warmup {" + state.dataReportFlag->cWarmupDay + "} " + state.dataEnvrn->EnvironmentName,
                          state.dataEnvrn->Latitude,
                          state.dataEnvrn->Longitude,
                          state.dataEnvrn->TimeZoneNumber,
                          state.dataEnvrn->Elevation);
                    state.dataEnvrn->PrintEnvrnStampWarmup = false;
                }
                state.dataHVACMgr->PrintedWarmup = true;
            }
            UpdateDataandReport(state, OutputProcessor::TimeStepType::System);
        }
        EMSManager::ManageEMS(
            state, EMSManager::EMSCallFrom::EndSystemTimestepAfterHVACReporting, anyEMSRan, ObjexxFCL::Optional_int_const()); // EMS calling point
        // UPDATE SYSTEM CLOCKS
        state.dataHVACGlobal->SysTimeElapsed += state.dataHVACGlobal->TimeStepSys;

        state.dataHVACGlobal->FirstTimeStepSysFlag = false;
    } // system time step  loop (loops once if no downstepping)

    for (auto &thisZoneHB : state.dataZoneTempPredictorCorrector->zoneHeatBalance) {
        thisZoneHB.ZTAVComf = thisZoneHB.ZTAV;
        thisZoneHB.airHumRatAvgComf = thisZoneHB.airHumRatAvg;
    }
    for (auto &thisSpaceHB : state.dataZoneTempPredictorCorrector->spaceHeatBalance) {
        thisSpaceHB.ZTAVComf = thisSpaceHB.ZTAV;
        thisSpaceHB.airHumRatAvgComf = thisSpaceHB.airHumRatAvg;
    }

    ZoneTempPredictorCorrector::ManageZoneAirUpdates(state,
                                                     DataHeatBalFanSys::PredictorCorrectorCtrl::PushZoneTimestepHistories,
                                                     ZoneTempChange,
                                                     state.dataHVACGlobal->ShortenTimeStepSys,
                                                     state.dataHVACGlobal->UseZoneTimeStepHistory,
                                                     PriorTimeStep);
    if (state.dataContaminantBalance->Contaminant.SimulateContaminants)
        ZoneContaminantPredictorCorrector::ManageZoneContaminanUpdates(state,
                                                                       DataHeatBalFanSys::PredictorCorrectorCtrl::PushZoneTimestepHistories,
                                                                       state.dataHVACGlobal->ShortenTimeStepSys,
                                                                       state.dataHVACGlobal->UseZoneTimeStepHistory,
                                                                       PriorTimeStep);

    state.dataHVACGlobal->NumOfSysTimeStepsLastZoneTimeStep = state.dataHVACGlobal->NumOfSysTimeSteps;

    DemandManager::UpdateDemandManagers(state);

    // DO FINAL UPDATE OF RECORD KEEPING VARIABLES
    // Report the Node Data to Aid in Debugging
    if (state.dataReportFlag->DebugOutput) {
        bool ReportDebug;
        if (state.dataReportFlag->EvenDuringWarmup) {
            ReportDebug = true;
        } else {
            ReportDebug = !state.dataGlobal->WarmupFlag;
        }
        if ((ReportDebug) && (state.dataGlobal->DayOfSim > 0)) { // Report the node data
            if (size(state.dataLoopNodes->Node) > 0 && !state.dataHVACMgr->DebugNamesReported) {
                print(state.files.debug, "{}\n", "node #   Name");
                for (int NodeNum = 1; NodeNum <= isize(state.dataLoopNodes->Node); ++NodeNum) {
                    print(state.files.debug, " {:3}     {}\n", NodeNum, state.dataLoopNodes->NodeID(NodeNum));
                }
                state.dataHVACMgr->DebugNamesReported = true;
            }
            if (size(state.dataLoopNodes->Node) > 0) {
                print(state.files.debug, "\n\n Day of Sim     Hour of Day    Time\n");
                print(state.files.debug,
                      "{:12}{:12} {:22.15N} \n",
                      state.dataGlobal->DayOfSim,
                      state.dataGlobal->HourOfDay,
                      state.dataGlobal->TimeStep * state.dataGlobal->TimeStepZone);
                print(state.files.debug,
                      "{}\n",
                      "node #   Temp   MassMinAv  MassMaxAv TempSP      MassFlow       MassMin       MassMax        MassSP    Press        "
                      "Enthal     HumRat Fluid Type");
            }
            for (int NodeNum = 1; NodeNum <= isize(state.dataLoopNodes->Node); ++NodeNum) {
                static constexpr std::string_view Format_20{
                    " {:3} {:8.2F}  {:8.3F}  {:8.3F}  {:8.2F} {:13.2F} {:13.2F} {:13.2F} {:13.2F}  {:#7.0F}  {:11.2F}  {:9.5F}  {}\n"};

                print(state.files.debug,
                      Format_20,
                      NodeNum,
                      state.dataLoopNodes->Node(NodeNum).Temp,
                      state.dataLoopNodes->Node(NodeNum).MassFlowRateMinAvail,
                      state.dataLoopNodes->Node(NodeNum).MassFlowRateMaxAvail,
                      state.dataLoopNodes->Node(NodeNum).TempSetPoint,
                      state.dataLoopNodes->Node(NodeNum).MassFlowRate,
                      state.dataLoopNodes->Node(NodeNum).MassFlowRateMin,
                      state.dataLoopNodes->Node(NodeNum).MassFlowRateMax,
                      state.dataLoopNodes->Node(NodeNum).MassFlowRateSetPoint,
                      state.dataLoopNodes->Node(NodeNum).Press,
                      state.dataLoopNodes->Node(NodeNum).Enthalpy,
                      state.dataLoopNodes->Node(NodeNum).HumRat,
                      DataLoopNode::NodeFluidTypeNames[static_cast<int>(state.dataLoopNodes->Node(NodeNum).FluidType)]);
            }
        }
    }
}

void SimHVAC(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR:          Dan Fisher
    //       DATE WRITTEN:    April 1997
    //       DATE MODIFIED:   May 1998 (RKS,RDT)

    // PURPOSE OF THIS SUBROUTINE: Selects and calls the HVAC loop managers

    // METHODOLOGY EMPLOYED: Each loop manager is called or passed over
    // in succession based on the logical flags associated with the manager.
    // The logical flags are set in the manager routines and passed
    // as parameters to this routine.  Each loop manager potentially
    // affects a different set of other loop managers.

    // Future development could involve specifying any number of user
    // selectable control schemes based on the logical flags used in
    // this default control algorithm.

    // SUBROUTINE PARAMETER DEFINITIONS:
    bool constexpr SimWithPlantFlowUnlocked(false);
    bool constexpr SimWithPlantFlowLocked(true);

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    bool FirstHVACIteration; // True when solution technique on first iteration
    Real64 SlopeHumRat;
    Real64 SlopeMdot;
    Real64 SlopeTemps;
    Real64 AvgValue;

    static constexpr std::array<Real64, DataConvergParams::ConvergLogStackDepth> ConvergLogStackARR = {
        0.0, -1.0, -2.0, -3.0, -4.0, -5.0, -6.0, -7.0, -8.0, -9.0};
    Real64 constexpr sum_ConvergLogStackARR(-45);
    Real64 constexpr square_sum_ConvergLogStackARR(2025);
    Real64 constexpr sum_square_ConvergLogStackARR(285);

    int NumPrimaryAirSys = state.dataHVACGlobal->NumPrimaryAirSys;

    // Initialize all of the simulation flags to true for the first iteration
    state.dataHVACGlobal->SimZoneEquipmentFlag = true;
    state.dataHVACGlobal->SimNonZoneEquipmentFlag = true;
    state.dataHVACGlobal->SimAirLoopsFlag = true;
    state.dataHVACGlobal->SimPlantLoopsFlag = true;
    state.dataHVACGlobal->SimElecCircuitsFlag = true;
    FirstHVACIteration = true;

    if (state.dataAirLoop->AirLoopInputsFilled) {
        for (auto &e : state.dataAirLoop->AirLoopControlInfo) {
            // Reset air loop control info for cooling coil active flag (used in TU's for reheat air flow control)
            e.CoolingActiveFlag = false;
            // Reset air loop control info for heating coil active flag (used in OA controller for HX control)
            e.HeatingActiveFlag = false;
            // reset outside air system HX to off first time through
            e.HeatRecoveryBypass = true;
            // set HX check status flag to check for custom control in MixedAir.cc
            e.CheckHeatRecoveryBypassStatus = true;
            // set OA comp simulated flag to false
            e.OASysComponentsSimulated = false;
            // set economizer flow locked flag to false, will reset if custom HX control is used
            e.EconomizerFlowLocked = false;
            // set air loop resim flags for when heat recovery is used and air loop needs another iteration
            e.HeatRecoveryResimFlag = true;
            e.HeatRecoveryResimFlag2 = false;
            e.ResimAirLoopFlag = false;
        }
    }

    // This setups the reports for the Iteration variable that limits how many times
    //  it goes through all of the HVAC managers before moving on.
    // The plant loop 'get inputs' and initialization are also done here in order to allow plant loop connected components
    // simulated by managers other than the plant manager to run correctly.
    state.dataHVACMgr->HVACManageIteration = 0;
    state.dataPlnt->PlantManageSubIterations = 0;
    state.dataPlnt->PlantManageHalfLoopCalls = 0;
    PlantUtilities::SetAllPlantSimFlagsToValue(state, true);
    if (!state.dataHVACMgr->SimHVACIterSetup) {
        SetupOutputVariable(state,
                            "HVAC System Solver Iteration Count",
                            OutputProcessor::Unit::None,
                            state.dataHVACMgr->HVACManageIteration,
                            OutputProcessor::SOVTimeStepType::HVAC,
                            OutputProcessor::SOVStoreType::Summed,
                            "SimHVAC");
        SetupOutputVariable(state,
                            "Air System Solver Iteration Count",
                            OutputProcessor::Unit::None,
                            state.dataHVACMgr->RepIterAir,
                            OutputProcessor::SOVTimeStepType::HVAC,
                            OutputProcessor::SOVStoreType::Summed,
                            "SimHVAC");
        SetupOutputVariable(state,
                            "Air System Relief Air Total Heat Loss Energy",
                            OutputProcessor::Unit::J,
                            state.dataHeatBal->SysTotalHVACReliefHeatLoss,
                            OutputProcessor::SOVTimeStepType::HVAC,
                            OutputProcessor::SOVStoreType::Summed,
                            "SimHVAC");
        SetupOutputVariable(state,
                            "HVAC System Total Heat Rejection Energy",
                            OutputProcessor::Unit::J,
                            state.dataHeatBal->SysTotalHVACRejectHeatLoss,
                            OutputProcessor::SOVTimeStepType::HVAC,
                            OutputProcessor::SOVStoreType::Summed,
                            "SimHVAC");
        SetPointManager::ManageSetPoints(state); // need to call this before getting plant loop data so setpoint checks can complete okay
        PlantManager::GetPlantLoopData(state);
        PlantManager::GetPlantInput(state);
        PlantManager::SetupInitialPlantCallingOrder(state);
        PlantManager::SetupBranchControlTypes(state); // new routine to do away with input for branch control type
        PlantManager::SetupReports(state);
        if (state.dataGlobal->AnyEnergyManagementSystemInModel) {
            PlantCondLoopOperation::SetupPlantEMSActuators(state);
        }

        if (state.dataPlnt->TotNumLoops > 0) {
            SetupOutputVariable(state,
                                "Plant Solver Sub Iteration Count",
                                OutputProcessor::Unit::None,
                                state.dataPlnt->PlantManageSubIterations,
                                OutputProcessor::SOVTimeStepType::HVAC,
                                OutputProcessor::SOVStoreType::Summed,
                                "SimHVAC");
            SetupOutputVariable(state,
                                "Plant Solver Half Loop Calls Count",
                                OutputProcessor::Unit::None,
                                state.dataPlnt->PlantManageHalfLoopCalls,
                                OutputProcessor::SOVTimeStepType::HVAC,
                                OutputProcessor::SOVStoreType::Summed,
                                "SimHVAC");
            for (int LoopNum = 1; LoopNum <= state.dataPlnt->TotNumLoops; ++LoopNum) {
                // init plant sizing numbers in main plant data structure
                PlantManager::InitOneTimePlantSizingInfo(state, LoopNum);
            }
        }
        state.dataHVACMgr->SimHVACIterSetup = true;
    }

    if (state.dataGlobal->ZoneSizingCalc) {
        ZoneEquipmentManager::ManageZoneEquipment(
            state, FirstHVACIteration, state.dataHVACGlobal->SimZoneEquipmentFlag, state.dataHVACGlobal->SimAirLoopsFlag);
        // need to call non zone equipment so water use zone gains can be included in sizing calcs
        NonZoneEquipmentManager::ManageNonZoneEquipment(state, FirstHVACIteration, state.dataHVACGlobal->SimNonZoneEquipmentFlag);
        state.dataElectPwrSvcMgr->facilityElectricServiceObj->manageElectricPowerService(
            state, FirstHVACIteration, state.dataHVACGlobal->SimElecCircuitsFlag, false);
        return;
    }

    // Before the HVAC simulation, reset control flags and specified flow
    // rates that might have been set by the set point and availability
    // managers.

    ResetHVACControl(state);

    // Before the HVAC simulation, call ManageSetPoints to set all the HVAC node setpoints
    bool anyEMSRan = false;
    EMSManager::ManageEMS(state, EMSManager::EMSCallFrom::BeforeHVACManagers, anyEMSRan, ObjexxFCL::Optional_int_const()); // calling point

    SetPointManager::ManageSetPoints(state);

    // re-initialize plant loop and nodes.
    PlantManager::ReInitPlantLoopsAtFirstHVACIteration(state);

    // Before the HVAC simulation, call ManageSystemAvailability to set the system on/off flags
    SystemAvailabilityManager::ManageSystemAvailability(state);

    EMSManager::ManageEMS(state, EMSManager::EMSCallFrom::AfterHVACManagers, anyEMSRan, ObjexxFCL::Optional_int_const()); // calling point
    EMSManager::ManageEMS(state, EMSManager::EMSCallFrom::HVACIterationLoop, anyEMSRan, ObjexxFCL::Optional_int_const()); // calling point id

    // first explicitly call each system type with FirstHVACIteration,

    // Manages the various component simulations
    SimSelectedEquipment(state,
                         state.dataHVACGlobal->SimAirLoopsFlag,
                         state.dataHVACGlobal->SimZoneEquipmentFlag,
                         state.dataHVACGlobal->SimNonZoneEquipmentFlag,
                         state.dataHVACGlobal->SimPlantLoopsFlag,
                         state.dataHVACGlobal->SimElecCircuitsFlag,
                         FirstHVACIteration,
                         SimWithPlantFlowUnlocked);

    // Eventually, when all of the flags are set to false, the
    // simulation has converged for this system time step.

    state.dataHVACGlobal->SimPlantLoopsFlag = true;
    PlantUtilities::SetAllPlantSimFlagsToValue(state, true); // set so loop to simulate at least once on non-first hvac

    FirstHVACIteration = false;

    // then iterate among all systems after first HVAC iteration is over

    // Main iteration loop for HVAC.  If any of the simulation flags are
    // true, then specific components must be resimulated.
    while ((state.dataHVACGlobal->SimAirLoopsFlag || state.dataHVACGlobal->SimZoneEquipmentFlag || state.dataHVACGlobal->SimNonZoneEquipmentFlag ||
            state.dataHVACGlobal->SimPlantLoopsFlag || state.dataHVACGlobal->SimElecCircuitsFlag) &&
           (state.dataHVACMgr->HVACManageIteration <= state.dataConvergeParams->MaxIter)) {

        if (state.dataGlobal->stopSimulation) break;

        EMSManager::ManageEMS(state, EMSManager::EMSCallFrom::HVACIterationLoop, anyEMSRan, ObjexxFCL::Optional_int_const()); // calling point id

        // Manages the various component simulations
        SimSelectedEquipment(state,
                             state.dataHVACGlobal->SimAirLoopsFlag,
                             state.dataHVACGlobal->SimZoneEquipmentFlag,
                             state.dataHVACGlobal->SimNonZoneEquipmentFlag,
                             state.dataHVACGlobal->SimPlantLoopsFlag,
                             state.dataHVACGlobal->SimElecCircuitsFlag,
                             FirstHVACIteration,
                             SimWithPlantFlowUnlocked);

        // Eventually, when all of the flags are set to false, the
        // simulation has converged for this system time step.

        UpdateZoneInletConvergenceLog(state);

        ++state.dataHVACMgr->HVACManageIteration; // Increment the iteration counter

        if (anyEMSRan && state.dataHVACMgr->HVACManageIteration <= 2) {
            // the calling point emsCallFromHVACIterationLoop is only effective for air loops if this while loop runs at least twice
            state.dataHVACGlobal->SimAirLoopsFlag = true;
        }
        if (state.dataHVACMgr->HVACManageIteration < state.dataHVACGlobal->MinAirLoopIterationsAfterFirst) {
            // sequenced zone loads for airloops may require extra iterations depending upon zone equipment order and load distribution type
            state.dataHVACGlobal->SimAirLoopsFlag = true;
            state.dataHVACGlobal->SimZoneEquipmentFlag = true;
        }
    }
    if (state.dataGlobal->AnyPlantInModel) {
        if (PlantUtilities::AnyPlantSplitterMixerLacksContinuity(state)) {
            // rerun systems in a "Final flow lock/last iteration" mode
            // now call for one second to last plant simulation
            state.dataHVACGlobal->SimAirLoopsFlag = false;
            state.dataHVACGlobal->SimZoneEquipmentFlag = false;
            state.dataHVACGlobal->SimNonZoneEquipmentFlag = false;
            state.dataHVACGlobal->SimPlantLoopsFlag = true;
            state.dataHVACGlobal->SimElecCircuitsFlag = false;
            SimSelectedEquipment(state,
                                 state.dataHVACGlobal->SimAirLoopsFlag,
                                 state.dataHVACGlobal->SimZoneEquipmentFlag,
                                 state.dataHVACGlobal->SimNonZoneEquipmentFlag,
                                 state.dataHVACGlobal->SimPlantLoopsFlag,
                                 state.dataHVACGlobal->SimElecCircuitsFlag,
                                 FirstHVACIteration,
                                 SimWithPlantFlowUnlocked);
            // now call for all non-plant simulation, but with plant flow lock on
            state.dataHVACGlobal->SimAirLoopsFlag = true;
            state.dataHVACGlobal->SimZoneEquipmentFlag = true;
            state.dataHVACGlobal->SimNonZoneEquipmentFlag = true;
            state.dataHVACGlobal->SimPlantLoopsFlag = false;
            state.dataHVACGlobal->SimElecCircuitsFlag = true;
            SimSelectedEquipment(state,
                                 state.dataHVACGlobal->SimAirLoopsFlag,
                                 state.dataHVACGlobal->SimZoneEquipmentFlag,
                                 state.dataHVACGlobal->SimNonZoneEquipmentFlag,
                                 state.dataHVACGlobal->SimPlantLoopsFlag,
                                 state.dataHVACGlobal->SimElecCircuitsFlag,
                                 FirstHVACIteration,
                                 SimWithPlantFlowLocked);
            UpdateZoneInletConvergenceLog(state);
            // now call for a last plant simulation
            state.dataHVACGlobal->SimAirLoopsFlag = false;
            state.dataHVACGlobal->SimZoneEquipmentFlag = false;
            state.dataHVACGlobal->SimNonZoneEquipmentFlag = false;
            state.dataHVACGlobal->SimPlantLoopsFlag = true;
            state.dataHVACGlobal->SimElecCircuitsFlag = false;
            SimSelectedEquipment(state,
                                 state.dataHVACGlobal->SimAirLoopsFlag,
                                 state.dataHVACGlobal->SimZoneEquipmentFlag,
                                 state.dataHVACGlobal->SimNonZoneEquipmentFlag,
                                 state.dataHVACGlobal->SimPlantLoopsFlag,
                                 state.dataHVACGlobal->SimElecCircuitsFlag,
                                 FirstHVACIteration,
                                 SimWithPlantFlowUnlocked);
            // now call for a last all non-plant simulation, but with plant flow lock on
            state.dataHVACGlobal->SimAirLoopsFlag = true;
            state.dataHVACGlobal->SimZoneEquipmentFlag = true;
            state.dataHVACGlobal->SimNonZoneEquipmentFlag = true;
            state.dataHVACGlobal->SimPlantLoopsFlag = false;
            state.dataHVACGlobal->SimElecCircuitsFlag = true;
            SimSelectedEquipment(state,
                                 state.dataHVACGlobal->SimAirLoopsFlag,
                                 state.dataHVACGlobal->SimZoneEquipmentFlag,
                                 state.dataHVACGlobal->SimNonZoneEquipmentFlag,
                                 state.dataHVACGlobal->SimPlantLoopsFlag,
                                 state.dataHVACGlobal->SimElecCircuitsFlag,
                                 FirstHVACIteration,
                                 SimWithPlantFlowLocked);
            UpdateZoneInletConvergenceLog(state);
        }
    }

    // Test plant loop for errors
    for (int LoopNum = 1; LoopNum <= state.dataPlnt->TotNumLoops; ++LoopNum) {
        for (DataPlant::LoopSideLocation LoopSide : DataPlant::LoopSideKeys) {
            PlantUtilities::CheckPlantMixerSplitterConsistency(state, LoopNum, LoopSide, FirstHVACIteration);
            PlantUtilities::CheckForRunawayPlantTemps(state, LoopNum, LoopSide);
        }
    }

    if ((state.dataHVACMgr->HVACManageIteration > state.dataConvergeParams->MaxIter) && (!state.dataGlobal->WarmupFlag)) {
        ++state.dataHVACMgr->ErrCount;
        if (state.dataHVACMgr->ErrCount < 15) {
            state.dataHVACMgr->ErrEnvironmentName = state.dataEnvrn->EnvironmentName;
            ShowWarningError(state,
                             format("SimHVAC: Maximum iterations ({}) exceeded for all HVAC loops, at {}, {} {}",
                                    state.dataConvergeParams->MaxIter,
                                    state.dataEnvrn->EnvironmentName,
                                    state.dataEnvrn->CurMnDy,
                                    General::CreateSysTimeIntervalString(state)));
            if (state.dataHVACGlobal->SimAirLoopsFlag) {
                ShowContinueError(state, "The solution for one or more of the Air Loop HVAC systems did not appear to converge");
            }
            if (state.dataHVACGlobal->SimZoneEquipmentFlag) {
                ShowContinueError(state, "The solution for zone HVAC equipment did not appear to converge");
            }
            if (state.dataHVACGlobal->SimNonZoneEquipmentFlag) {
                ShowContinueError(state, "The solution for non-zone equipment did not appear to converge");
            }
            if (state.dataHVACGlobal->SimPlantLoopsFlag) {
                ShowContinueError(state, "The solution for one or more plant systems did not appear to converge");
            }
            if (state.dataHVACGlobal->SimElecCircuitsFlag) {
                ShowContinueError(state, "The solution for on-site electric generators did not appear to converge");
            }
            if (state.dataHVACMgr->ErrCount == 1 && !state.dataGlobal->DisplayExtraWarnings) {
                ShowContinueError(state, "...use Output:Diagnostics,DisplayExtraWarnings; to show more details on each max iteration exceeded.");
            }
            if (state.dataGlobal->DisplayExtraWarnings) {

                for (int AirSysNum = 1; AirSysNum <= NumPrimaryAirSys; ++AirSysNum) {

                    auto &arrayRef = state.dataConvergeParams->AirLoopConvergence(AirSysNum).HVACMassFlowNotConverged;
                    if (std::any_of(std::begin(arrayRef), std::end(arrayRef), [](bool i) { return i; })) {

                        ShowContinueError(state,
                                          format("Air System Named = {} did not converge for mass flow rate",
                                                 state.dataAirLoop->AirToZoneNodeInfo(AirSysNum).AirLoopName));
                        ShowContinueError(state, "Check values should be zero. Most Recent values listed first.");
                        std::string HistoryTrace = "";
                        for (int StackDepth = 0; StackDepth < DataConvergParams::ConvergLogStackDepth; ++StackDepth) {
                            HistoryTrace +=
                                format("{:.6R},", state.dataConvergeParams->AirLoopConvergence(AirSysNum).HVACFlowDemandToSupplyTolValue[StackDepth]);
                        }

                        ShowContinueError(state,
                                          format("Demand-to-Supply interface mass flow rate check value iteration history trace: {}", HistoryTrace));
                        HistoryTrace = "";
                        for (int StackDepth = 0; StackDepth < DataConvergParams::ConvergLogStackDepth; ++StackDepth) {
                            HistoryTrace += format(
                                "{:.6R},", state.dataConvergeParams->AirLoopConvergence(AirSysNum).HVACFlowSupplyDeck1ToDemandTolValue[StackDepth]);
                        }
                        ShowContinueError(
                            state, format("Supply-to-demand interface deck 1 mass flow rate check value iteration history trace: {}", HistoryTrace));

                        if (state.dataAirLoop->AirToZoneNodeInfo(AirSysNum).NumSupplyNodes >= 2) {
                            HistoryTrace = "";
                            for (int StackDepth = 0; StackDepth < DataConvergParams::ConvergLogStackDepth; ++StackDepth) {
                                HistoryTrace +=
                                    format("{:.6R},",
                                           state.dataConvergeParams->AirLoopConvergence(AirSysNum).HVACFlowSupplyDeck2ToDemandTolValue[StackDepth]);
                            }
                            ShowContinueError(
                                state,
                                format("Supply-to-demand interface deck 2 mass flow rate check value iteration history trace: {}", HistoryTrace));
                        }
                    } // mass flow rate not converged

                    auto &arrayRef2 = state.dataConvergeParams->AirLoopConvergence(AirSysNum).HVACHumRatNotConverged;
                    if (std::any_of(std::begin(arrayRef2), std::end(arrayRef2), [](bool i) { return i; })) {

                        ShowContinueError(state,
                                          format("Air System Named = {} did not converge for humidity ratio",
                                                 state.dataAirLoop->AirToZoneNodeInfo(AirSysNum).AirLoopName));
                        ShowContinueError(state, "Check values should be zero. Most Recent values listed first.");
                        std::string HistoryTrace = "";
                        for (int StackDepth = 0; StackDepth < DataConvergParams::ConvergLogStackDepth; ++StackDepth) {
                            HistoryTrace +=
                                format("{:.6R},", state.dataConvergeParams->AirLoopConvergence(AirSysNum).HVACHumDemandToSupplyTolValue[StackDepth]);
                        }
                        ShowContinueError(state,
                                          format("Demand-to-Supply interface humidity ratio check value iteration history trace: {}", HistoryTrace));
                        HistoryTrace = "";
                        for (int StackDepth = 0; StackDepth < DataConvergParams::ConvergLogStackDepth; ++StackDepth) {
                            HistoryTrace += format(
                                "{:.6R},", state.dataConvergeParams->AirLoopConvergence(AirSysNum).HVACHumSupplyDeck1ToDemandTolValue[StackDepth]);
                        }
                        ShowContinueError(
                            state, format("Supply-to-demand interface deck 1 humidity ratio check value iteration history trace: {}", HistoryTrace));

                        if (state.dataAirLoop->AirToZoneNodeInfo(AirSysNum).NumSupplyNodes >= 2) {
                            HistoryTrace = "";
                            for (int StackDepth = 0; StackDepth < DataConvergParams::ConvergLogStackDepth; ++StackDepth) {
                                HistoryTrace +=
                                    format("{:.6R},",
                                           state.dataConvergeParams->AirLoopConvergence(AirSysNum).HVACHumSupplyDeck2ToDemandTolValue[StackDepth]);
                            }
                            ShowContinueError(
                                state,
                                format("Supply-to-demand interface deck 2 humidity ratio check value iteration history trace: {}", HistoryTrace));
                        }
                    } // humidity ratio not converged

                    auto &arrayRef3 = state.dataConvergeParams->AirLoopConvergence(AirSysNum).HVACTempNotConverged;
                    if (std::any_of(std::begin(arrayRef3), std::end(arrayRef3), [](bool i) { return i; })) {

                        ShowContinueError(state,
                                          format("Air System Named = {} did not converge for temperature",
                                                 state.dataAirLoop->AirToZoneNodeInfo(AirSysNum).AirLoopName));
                        ShowContinueError(state, "Check values should be zero. Most Recent values listed first.");
                        std::string HistoryTrace = "";
                        for (int StackDepth = 0; StackDepth < DataConvergParams::ConvergLogStackDepth; ++StackDepth) {
                            HistoryTrace +=
                                format("{:.6R},", state.dataConvergeParams->AirLoopConvergence(AirSysNum).HVACTempDemandToSupplyTolValue[StackDepth]);
                        }
                        ShowContinueError(state,
                                          format("Demand-to-Supply interface temperature check value iteration history trace: {}", HistoryTrace));
                        HistoryTrace = "";
                        for (int StackDepth = 0; StackDepth < DataConvergParams::ConvergLogStackDepth; ++StackDepth) {
                            HistoryTrace += format(
                                "{:.6R},", state.dataConvergeParams->AirLoopConvergence(AirSysNum).HVACTempSupplyDeck1ToDemandTolValue[StackDepth]);
                        }
                        ShowContinueError(
                            state, format("Supply-to-demand interface deck 1 temperature check value iteration history trace: {}", HistoryTrace));

                        if (state.dataAirLoop->AirToZoneNodeInfo(AirSysNum).NumSupplyNodes >= 2) {
                            HistoryTrace = "";
                            for (int StackDepth = 0; StackDepth < DataConvergParams::ConvergLogStackDepth; ++StackDepth) {
                                HistoryTrace +=
                                    format("{:.6R},",
                                           state.dataConvergeParams->AirLoopConvergence(AirSysNum).HVACTempSupplyDeck1ToDemandTolValue[StackDepth]);
                            }
                            ShowContinueError(
                                state, format("Supply-to-demand interface deck 2 temperature check value iteration history trace: {}", HistoryTrace));
                        }
                    } // Temps not converged

                    auto &arrayRef4 = state.dataConvergeParams->AirLoopConvergence(AirSysNum).HVACEnergyNotConverged;
                    if (std::any_of(std::begin(arrayRef4), std::end(arrayRef4), [](bool i) { return i; })) {

                        ShowContinueError(
                            state,
                            format("Air System Named = {} did not converge for energy", state.dataAirLoop->AirToZoneNodeInfo(AirSysNum).AirLoopName));
                        ShowContinueError(state, "Check values should be zero. Most Recent values listed first.");
                        std::string HistoryTrace = "";
                        for (int StackDepth = 0; StackDepth < DataConvergParams::ConvergLogStackDepth; ++StackDepth) {
                            HistoryTrace += format(
                                "{:.6R},", state.dataConvergeParams->AirLoopConvergence(AirSysNum).HVACEnergyDemandToSupplyTolValue[StackDepth]);
                        }
                        ShowContinueError(state, format("Demand-to-Supply interface energy check value iteration history trace: {}", HistoryTrace));
                        HistoryTrace = "";
                        for (int StackDepth = 0; StackDepth < DataConvergParams::ConvergLogStackDepth; ++StackDepth) {
                            HistoryTrace += format(
                                "{:.6R},", state.dataConvergeParams->AirLoopConvergence(AirSysNum).HVACEnergySupplyDeck1ToDemandTolValue[StackDepth]);
                        }
                        ShowContinueError(state,
                                          format("Supply-to-demand interface deck 1 energy check value iteration history trace: {}", HistoryTrace));

                        if (state.dataAirLoop->AirToZoneNodeInfo(AirSysNum).NumSupplyNodes >= 2) {
                            HistoryTrace = "";
                            for (int StackDepth = 0; StackDepth < DataConvergParams::ConvergLogStackDepth; ++StackDepth) {
                                HistoryTrace +=
                                    format("{:.6R},",
                                           state.dataConvergeParams->AirLoopConvergence(AirSysNum).HVACEnergySupplyDeck2ToDemandTolValue[StackDepth]);
                            }
                            ShowContinueError(
                                state, format("Supply-to-demand interface deck 2 energy check value iteration history trace: {}", HistoryTrace));
                        }
                    } // energy not converged

                } // loop over air loop systems

                // loop over zones and check for issues with zone inlet nodes
                for (int ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {

                    for (int NodeIndex = 1; NodeIndex <= state.dataConvergeParams->ZoneInletConvergence(ZoneNum).NumInletNodes; ++NodeIndex) {

                        auto &humRatInletNode = state.dataConvergeParams->ZoneInletConvergence(ZoneNum).InletNode(NodeIndex).HumidityRatio;
                        auto &mdotInletNode = state.dataConvergeParams->ZoneInletConvergence(ZoneNum).InletNode(NodeIndex).MassFlowRate;
                        auto &inletTemp = state.dataConvergeParams->ZoneInletConvergence(ZoneNum).InletNode(NodeIndex).Temperature;

                        // Check humidity ratio
                        bool FoundOscillationByDuplicate = false;
                        bool MonotonicDecreaseFound = false;
                        bool MonotonicIncreaseFound = false;
                        // check for evidence of oscillation by identifying duplicates when latest value not equal to average
                        Real64 summation = 0.0;
                        summation = std::accumulate(humRatInletNode.begin(), humRatInletNode.end(), 0.0);
                        AvgValue = summation / double(DataConvergParams::ConvergLogStackDepth);
                        if (std::abs(humRatInletNode[0] - AvgValue) >
                            DataConvergParams::HVACHumRatOscillationToler) { // last iterate differs from average
                            FoundOscillationByDuplicate = false;
                            for (int StackDepth = 1; StackDepth < DataConvergParams::ConvergLogStackDepth; ++StackDepth) {
                                if (std::abs(humRatInletNode[0] - humRatInletNode[StackDepth]) < DataConvergParams::HVACHumRatOscillationToler) {
                                    FoundOscillationByDuplicate = true;
                                    ShowContinueError(
                                        state,
                                        format("Node named {} shows oscillating humidity ratio across iterations with a repeated value of {:.6R}",
                                               state.dataLoopNodes->NodeID(
                                                   state.dataConvergeParams->ZoneInletConvergence(ZoneNum).InletNode(NodeIndex).NodeNum),
                                               humRatInletNode[0]));
                                    break;
                                }
                            }
                            if (!FoundOscillationByDuplicate) {

                                Real64 humRatInletNodDotProd = std::inner_product(
                                    std::begin(ConvergLogStackARR), std::end(ConvergLogStackARR), std::begin(humRatInletNode), 0.0);
                                Real64 summation2 = 0.0;
                                summation2 = std::accumulate(humRatInletNode.begin(), humRatInletNode.end(), 0.0);
                                SlopeHumRat =
                                    (sum_ConvergLogStackARR * summation2 - double(DataConvergParams::ConvergLogStackDepth) * humRatInletNodDotProd) /
                                    (square_sum_ConvergLogStackARR - double(DataConvergParams::ConvergLogStackDepth) * sum_square_ConvergLogStackARR);
                                if (std::abs(SlopeHumRat) > DataConvergParams::HVACHumRatSlopeToler) {

                                    if (SlopeHumRat < 0.0) { // check for monotonic decrease
                                        MonotonicDecreaseFound = true;
                                        for (int StackDepth = 1; StackDepth < DataConvergParams::ConvergLogStackDepth; ++StackDepth) {
                                            if (humRatInletNode[StackDepth - 1] > humRatInletNode[StackDepth]) {
                                                MonotonicDecreaseFound = false;
                                                break;
                                            }
                                        }
                                        if (MonotonicDecreaseFound) {
                                            ShowContinueError(
                                                state,
                                                format("Node named {} shows monotonically decreasing humidity ratio with a trend "
                                                       "rate across iterations of {:.6R} [ kg-water/kg-dryair/iteration]",
                                                       state.dataLoopNodes->NodeID(
                                                           state.dataConvergeParams->ZoneInletConvergence(ZoneNum).InletNode(NodeIndex).NodeNum),
                                                       SlopeHumRat));
                                        }
                                    } else { // check for monotonic increase
                                        MonotonicIncreaseFound = true;
                                        for (int StackDepth = 1; StackDepth < DataConvergParams::ConvergLogStackDepth; ++StackDepth) {
                                            if (humRatInletNode[StackDepth - 1] < humRatInletNode[StackDepth]) {
                                                MonotonicIncreaseFound = false;
                                                break;
                                            }
                                        }
                                        if (MonotonicIncreaseFound) {
                                            ShowContinueError(
                                                state,
                                                format("Node named {} shows monotonically increasing humidity ratio with a trend "
                                                       "rate across iterations of {:.6R} [ kg-water/kg-dryair/iteration]",
                                                       state.dataLoopNodes->NodeID(
                                                           state.dataConvergeParams->ZoneInletConvergence(ZoneNum).InletNode(NodeIndex).NodeNum),
                                                       SlopeHumRat));
                                        }
                                    }
                                } // significant slope in iterates
                            }     // no osciallation
                        }         // last value does not equal average of stack.

                        if (MonotonicDecreaseFound || MonotonicIncreaseFound || FoundOscillationByDuplicate) {
                            std::string HistoryTrace = "";
                            for (int StackDepth = 0; StackDepth < DataConvergParams::ConvergLogStackDepth; ++StackDepth) {
                                HistoryTrace += format("{:.6R},", humRatInletNode[StackDepth]);
                            }
                            ShowContinueError(
                                state,
                                format(
                                    "Node named {} humidity ratio [kg-water/kg-dryair] iteration history trace (most recent first): {}",
                                    state.dataLoopNodes->NodeID(state.dataConvergeParams->ZoneInletConvergence(ZoneNum).InletNode(NodeIndex).NodeNum),
                                    HistoryTrace));
                        } // need to report trace
                        // end humidity ratio

                        // Check Mass flow rate
                        FoundOscillationByDuplicate = false;
                        MonotonicDecreaseFound = false;
                        MonotonicIncreaseFound = false;
                        // check for evidence of oscillation by identify duplicates when latest value not equal to average
                        Real64 summation2 = 0.0;
                        summation2 = std::accumulate(mdotInletNode.begin(), mdotInletNode.end(), 0.0);
                        AvgValue = summation2 / double(DataConvergParams::ConvergLogStackDepth);
                        if (std::abs(mdotInletNode[0] - AvgValue) >
                            DataConvergParams::HVACFlowRateOscillationToler) { // last iterate differs from average
                            FoundOscillationByDuplicate = false;
                            for (int StackDepth = 1; StackDepth < DataConvergParams::ConvergLogStackDepth; ++StackDepth) {
                                if (std::abs(mdotInletNode[0] - mdotInletNode[StackDepth]) < DataConvergParams::HVACFlowRateOscillationToler) {
                                    FoundOscillationByDuplicate = true;
                                    ShowContinueError(
                                        state,
                                        format("Node named {} shows oscillating mass flow rate across iterations with a repeated value of {:.6R}",
                                               state.dataLoopNodes->NodeID(
                                                   state.dataConvergeParams->ZoneInletConvergence(ZoneNum).InletNode(NodeIndex).NodeNum),
                                               mdotInletNode[0]));
                                    break;
                                }
                            }
                            if (!FoundOscillationByDuplicate) {

                                Real64 humRatInletNodDotProd =
                                    std::inner_product(std::begin(ConvergLogStackARR), std::end(ConvergLogStackARR), std::begin(mdotInletNode), 0.0);
                                Real64 summation3 = 0.0;
                                summation3 = std::accumulate(mdotInletNode.begin(), mdotInletNode.end(), 0.0);
                                SlopeMdot =
                                    (sum_ConvergLogStackARR * summation3 - double(DataConvergParams::ConvergLogStackDepth) * humRatInletNodDotProd) /
                                    (square_sum_ConvergLogStackARR - double(DataConvergParams::ConvergLogStackDepth) * sum_square_ConvergLogStackARR);
                                if (std::abs(SlopeMdot) > DataConvergParams::HVACFlowRateSlopeToler) {
                                    if (SlopeMdot < 0.0) { // check for monotonic decrease
                                        MonotonicDecreaseFound = true;
                                        for (int StackDepth = 1; StackDepth < DataConvergParams::ConvergLogStackDepth; ++StackDepth) {
                                            if (mdotInletNode[StackDepth - 1] > mdotInletNode[StackDepth]) {
                                                MonotonicDecreaseFound = false;
                                                break;
                                            }
                                        }
                                        if (MonotonicDecreaseFound) {
                                            ShowContinueError(
                                                state,
                                                format("Node named {} shows monotonically decreasing mass flow rate with a trend "
                                                       "rate across iterations of {:.6R} [kg/s/iteration]",
                                                       state.dataLoopNodes->NodeID(
                                                           state.dataConvergeParams->ZoneInletConvergence(ZoneNum).InletNode(NodeIndex).NodeNum),
                                                       SlopeMdot));
                                        }
                                    } else { // check for monotonic increase
                                        MonotonicIncreaseFound = true;
                                        for (int StackDepth = 1; StackDepth < DataConvergParams::ConvergLogStackDepth; ++StackDepth) {
                                            if (mdotInletNode[StackDepth - 1] < mdotInletNode[StackDepth]) {
                                                MonotonicIncreaseFound = false;
                                                break;
                                            }
                                        }
                                        if (MonotonicIncreaseFound) {
                                            ShowContinueError(
                                                state,
                                                format("Node named {} shows monotonically increasing mass flow rate with a trend "
                                                       "rate across iterations of {:.6R} [kg/s/iteration]",
                                                       state.dataLoopNodes->NodeID(
                                                           state.dataConvergeParams->ZoneInletConvergence(ZoneNum).InletNode(NodeIndex).NodeNum),
                                                       SlopeMdot));
                                        }
                                    }
                                } // significant slope in iterates
                            }     // no oscillation
                        }         // last value does not equal average of stack.

                        if (MonotonicDecreaseFound || MonotonicIncreaseFound || FoundOscillationByDuplicate) {
                            std::string HistoryTrace = "";
                            for (int StackDepth = 0; StackDepth < DataConvergParams::ConvergLogStackDepth; ++StackDepth) {
                                HistoryTrace += format("{:.6R},", mdotInletNode[StackDepth]);
                            }
                            ShowContinueError(state,
                                              format("Node named {} mass flow rate [kg/s] iteration history trace (most recent first): {}",
                                                     state.dataLoopNodes->NodeID(
                                                         state.dataConvergeParams->ZoneInletConvergence(ZoneNum).InletNode(NodeIndex).NodeNum),
                                                     HistoryTrace));
                        } // need to report trace
                        // end mass flow rate

                        // Check Temperatures
                        FoundOscillationByDuplicate = false;
                        MonotonicDecreaseFound = false;
                        MonotonicIncreaseFound = false;
                        // check for evidence of oscillation by identify duplicates when latest value not equal to average
                        Real64 summation3 = 0.0;
                        summation3 = std::accumulate(inletTemp.begin(), inletTemp.end(), 0.0);
                        AvgValue = summation3 / double(DataConvergParams::ConvergLogStackDepth);
                        if (std::abs(inletTemp[0] - AvgValue) >
                            DataConvergParams::HVACTemperatureOscillationToler) { // last iterate differs from average
                            FoundOscillationByDuplicate = false;
                            for (int StackDepth = 1; StackDepth < DataConvergParams::ConvergLogStackDepth; ++StackDepth) {
                                if (std::abs(inletTemp[0] - inletTemp[StackDepth]) < DataConvergParams::HVACTemperatureOscillationToler) {
                                    FoundOscillationByDuplicate = true;
                                    ShowContinueError(
                                        state,
                                        format("Node named {} shows oscillating temperatures across iterations with a repeated value of {:.6R}",
                                               state.dataLoopNodes->NodeID(
                                                   state.dataConvergeParams->ZoneInletConvergence(ZoneNum).InletNode(NodeIndex).NodeNum),
                                               inletTemp[0]));
                                    break;
                                }
                            }
                            if (!FoundOscillationByDuplicate) {

                                Real64 inletTempDotProd =
                                    std::inner_product(std::begin(ConvergLogStackARR), std::end(ConvergLogStackARR), std::begin(inletTemp), 0.0);

                                Real64 summation4 = 0.0;
                                summation4 = std::accumulate(inletTemp.begin(), inletTemp.end(), 0.0);
                                SlopeTemps =
                                    (sum_ConvergLogStackARR * summation4 - double(DataConvergParams::ConvergLogStackDepth) * inletTempDotProd) /
                                    (square_sum_ConvergLogStackARR - double(DataConvergParams::ConvergLogStackDepth) * sum_square_ConvergLogStackARR);
                                if (std::abs(SlopeTemps) > DataConvergParams::HVACTemperatureSlopeToler) {
                                    if (SlopeTemps < 0.0) { // check for monotonic decrease
                                        MonotonicDecreaseFound = true;
                                        for (int StackDepth = 1; StackDepth < DataConvergParams::ConvergLogStackDepth; ++StackDepth) {
                                            if (inletTemp[StackDepth - 1] > inletTemp[StackDepth]) {
                                                MonotonicDecreaseFound = false;
                                                break;
                                            }
                                        }
                                        if (MonotonicDecreaseFound) {
                                            ShowContinueError(
                                                state,
                                                format("Node named {} shows monotonically decreasing temperature with a trend rate "
                                                       "across iterations of {:.4R} [C/iteration]",
                                                       state.dataLoopNodes->NodeID(
                                                           state.dataConvergeParams->ZoneInletConvergence(ZoneNum).InletNode(NodeIndex).NodeNum),
                                                       SlopeTemps));
                                        }
                                    } else { // check for monotonic increase
                                        MonotonicIncreaseFound = true;
                                        for (int StackDepth = 1; StackDepth < DataConvergParams::ConvergLogStackDepth; ++StackDepth) {
                                            if (inletTemp[StackDepth - 1] < inletTemp[StackDepth]) {
                                                MonotonicIncreaseFound = false;
                                                break;
                                            }
                                        }
                                        if (MonotonicIncreaseFound) {
                                            ShowContinueError(
                                                state,
                                                format("Node named {} shows monotonically increasing temperatures with a trend "
                                                       "rate across iterations of {:.4R} [C/iteration]",
                                                       state.dataLoopNodes->NodeID(
                                                           state.dataConvergeParams->ZoneInletConvergence(ZoneNum).InletNode(NodeIndex).NodeNum),
                                                       SlopeTemps));
                                        }
                                    }
                                } // significant slope in iterates
                            }     // no osciallation
                        }         // last value does not equal average of stack.

                        if (MonotonicDecreaseFound || MonotonicIncreaseFound || FoundOscillationByDuplicate) {
                            std::string HistoryTrace = "";
                            for (int StackDepth = 0; StackDepth < DataConvergParams::ConvergLogStackDepth; ++StackDepth) {
                                HistoryTrace += format("{:.6R},", inletTemp[StackDepth]);
                            }
                            ShowContinueError(state,
                                              format("Node named {} temperature [C] iteration history trace (most recent first): {}",
                                                     state.dataLoopNodes->NodeID(
                                                         state.dataConvergeParams->ZoneInletConvergence(ZoneNum).InletNode(NodeIndex).NodeNum),
                                                     HistoryTrace));
                        } // need to report trace
                          // end Temperature checks

                    } // loop over zone inlet nodes
                }     // loop over zones

                for (int LoopNum = 1; LoopNum <= state.dataPlnt->TotNumLoops; ++LoopNum) {
                    bool FoundOscillationByDuplicate;
                    bool MonotonicIncreaseFound;
                    bool MonotonicDecreaseFound;

                    if (state.dataConvergeParams->PlantConvergence(LoopNum).PlantMassFlowNotConverged) {
                        ShowContinueError(
                            state, format("Plant System Named = {} did not converge for mass flow rate", state.dataPlnt->PlantLoop(LoopNum).Name));
                        ShowContinueError(state, "Check values should be zero. Most Recent values listed first.");
                        std::string HistoryTrace = "";
                        for (int StackDepth = 0; StackDepth < DataConvergParams::ConvergLogStackDepth; ++StackDepth) {
                            HistoryTrace +=
                                format("{:.6R},", state.dataConvergeParams->PlantConvergence(LoopNum).PlantFlowDemandToSupplyTolValue[StackDepth]);
                        }
                        ShowContinueError(state,
                                          format("Demand-to-Supply interface mass flow rate check value iteration history trace: {}", HistoryTrace));
                        HistoryTrace = "";
                        for (int StackDepth = 0; StackDepth < DataConvergParams::ConvergLogStackDepth; ++StackDepth) {
                            HistoryTrace +=
                                format("{:.6R},", state.dataConvergeParams->PlantConvergence(LoopNum).PlantFlowSupplyToDemandTolValue[StackDepth]);
                        }
                        ShowContinueError(state,
                                          format("Supply-to-Demand interface mass flow rate check value iteration history trace: {}", HistoryTrace));

                        // now work with history logs for mass flow to detect issues
                        for (DataPlant::LoopSideLocation ThisLoopSide : DataPlant::LoopSideKeys) {

                            auto &mdotHistInletNode = state.dataPlnt->PlantLoop(LoopNum).LoopSide(ThisLoopSide).InletNode.MassFlowRateHistory;
                            auto &mdotHistOutletNode = state.dataPlnt->PlantLoop(LoopNum).LoopSide(ThisLoopSide).OutletNode.MassFlowRateHistory;

                            // loop side inlet node
                            FoundOscillationByDuplicate = false;
                            MonotonicDecreaseFound = false;
                            MonotonicIncreaseFound = false;
                            AvgValue = sum(mdotHistInletNode) / double(DataPlant::NumConvergenceHistoryTerms);
                            if (std::abs(mdotHistInletNode(1) - AvgValue) > DataConvergParams::PlantFlowRateOscillationToler) {
                                FoundOscillationByDuplicate = false;
                                for (int StackDepth = 2; StackDepth <= DataPlant::NumConvergenceHistoryTerms; ++StackDepth) {
                                    if (std::abs(mdotHistInletNode(1) - mdotHistInletNode(StackDepth)) <
                                        DataConvergParams::PlantFlowRateOscillationToler) {
                                        FoundOscillationByDuplicate = true;
                                        ShowContinueError(
                                            state,
                                            format("Node named {} shows oscillating flow rates across iterations with a repeated value of {:.7R}",
                                                   state.dataPlnt->PlantLoop(LoopNum).LoopSide(ThisLoopSide).NodeNameIn,
                                                   mdotHistInletNode(1)));
                                        break;
                                    }
                                }
                            }
                            if (!FoundOscillationByDuplicate) {

                                Real64 mdotHistInletNodeDotProd = std::inner_product(
                                    std::begin(ConvergenceHistoryARR), std::end(ConvergenceHistoryARR), std::begin(mdotHistInletNode), 0.0);

                                SlopeMdot = (sum_ConvergenceHistoryARR * sum(mdotHistInletNode) -
                                             double(DataPlant::NumConvergenceHistoryTerms) * mdotHistInletNodeDotProd) /
                                            (square_sum_ConvergenceHistoryARR -
                                             double(DataPlant::NumConvergenceHistoryTerms) * sum_square_ConvergenceHistoryARR);
                                if (std::abs(SlopeMdot) > DataConvergParams::PlantFlowRateSlopeToler) {
                                    if (SlopeMdot < 0.0) { // check for monotonic decrease
                                        MonotonicDecreaseFound = true;
                                        for (int StackDepth = 2; StackDepth <= DataPlant::NumConvergenceHistoryTerms; ++StackDepth) {
                                            if (mdotHistInletNode(StackDepth - 1) > mdotHistInletNode(StackDepth)) {
                                                MonotonicDecreaseFound = false;
                                                break;
                                            }
                                        }
                                        if (MonotonicDecreaseFound) {
                                            ShowContinueError(state,
                                                              format("Node named {} shows monotonically decreasing mass flow rate with a trend "
                                                                     "rate across iterations of {:.7R} [kg/s/iteration]",
                                                                     state.dataPlnt->PlantLoop(LoopNum).LoopSide(ThisLoopSide).NodeNameIn,
                                                                     SlopeMdot));
                                        }
                                    } else { // check for monotonic increase
                                        MonotonicIncreaseFound = true;
                                        for (int StackDepth = 2; StackDepth <= DataPlant::NumConvergenceHistoryTerms; ++StackDepth) {
                                            if (mdotHistInletNode(StackDepth - 1) < mdotHistInletNode(StackDepth)) {
                                                MonotonicIncreaseFound = false;
                                                break;
                                            }
                                        }
                                        if (MonotonicIncreaseFound) {
                                            ShowContinueError(state,
                                                              format("Node named {} shows monotonically increasing mass flow rate with a trend "
                                                                     "rate across iterations of {:.7R} [kg/s/iteration]",
                                                                     state.dataPlnt->PlantLoop(LoopNum).LoopSide(ThisLoopSide).NodeNameIn,
                                                                     SlopeMdot));
                                        }
                                    }
                                } // significant slope found
                            }     // no oscillation found

                            if (MonotonicDecreaseFound || MonotonicIncreaseFound || FoundOscillationByDuplicate) {
                                HistoryTrace = "";
                                for (int StackDepth = 1; StackDepth <= DataPlant::NumConvergenceHistoryTerms; ++StackDepth) {
                                    HistoryTrace += format("{:.7R},", mdotHistInletNode(StackDepth));
                                }
                                ShowContinueError(state,
                                                  format("Node named {} mass flow rate [kg/s] iteration history trace (most recent first): {}",
                                                         state.dataPlnt->PlantLoop(LoopNum).LoopSide(ThisLoopSide).NodeNameIn,
                                                         HistoryTrace));
                            } // need to report trace
                            // end of inlet node

                            // loop side outlet node
                            FoundOscillationByDuplicate = false;
                            MonotonicDecreaseFound = false;
                            MonotonicIncreaseFound = false;
                            AvgValue = sum(mdotHistOutletNode) / double(DataPlant::NumConvergenceHistoryTerms);
                            if (std::abs(mdotHistOutletNode(1) - AvgValue) > DataConvergParams::PlantFlowRateOscillationToler) {
                                FoundOscillationByDuplicate = false;
                                for (int StackDepth = 2; StackDepth <= DataPlant::NumConvergenceHistoryTerms; ++StackDepth) {
                                    if (std::abs(mdotHistOutletNode(1) - mdotHistOutletNode(StackDepth)) <
                                        DataConvergParams::PlantFlowRateOscillationToler) {
                                        FoundOscillationByDuplicate = true;
                                        ShowContinueError(
                                            state,
                                            format("Node named {} shows oscillating flow rates across iterations with a repeated value of {:.7R}",
                                                   state.dataPlnt->PlantLoop(LoopNum).LoopSide(ThisLoopSide).NodeNameOut,
                                                   mdotHistOutletNode(1)));
                                        break;
                                    }
                                }
                            }
                            if (!FoundOscillationByDuplicate) {

                                Real64 mdotHistOutletNodeDotProd = std::inner_product(
                                    std::begin(ConvergenceHistoryARR), std::end(ConvergenceHistoryARR), std::begin(mdotHistOutletNode), 0.0);

                                SlopeMdot = (sum_ConvergenceHistoryARR * sum(mdotHistOutletNode) -
                                             double(DataPlant::NumConvergenceHistoryTerms) * mdotHistOutletNodeDotProd) /
                                            (square_sum_ConvergenceHistoryARR -
                                             double(DataPlant::NumConvergenceHistoryTerms) * sum_square_ConvergenceHistoryARR);
                                if (std::abs(SlopeMdot) > DataConvergParams::PlantFlowRateSlopeToler) {
                                    if (SlopeMdot < 0.0) { // check for monotonic decrease
                                        MonotonicDecreaseFound = true;
                                        for (int StackDepth = 2; StackDepth <= DataPlant::NumConvergenceHistoryTerms; ++StackDepth) {
                                            if (mdotHistOutletNode(StackDepth - 1) > mdotHistOutletNode(StackDepth)) {
                                                MonotonicDecreaseFound = false;
                                                break;
                                            }
                                        }
                                        if (MonotonicDecreaseFound) {
                                            ShowContinueError(state,
                                                              format("Node named {} shows monotonically decreasing mass flow rate with a trend "
                                                                     "rate across iterations of {:.7R} [kg/s/iteration]",
                                                                     state.dataPlnt->PlantLoop(LoopNum).LoopSide(ThisLoopSide).NodeNameOut,
                                                                     SlopeMdot));
                                        }
                                    } else { // check for monotonic increase
                                        MonotonicIncreaseFound = true;
                                        for (int StackDepth = 2; StackDepth <= DataPlant::NumConvergenceHistoryTerms; ++StackDepth) {
                                            if (mdotHistOutletNode(StackDepth - 1) < mdotHistOutletNode(StackDepth)) {
                                                MonotonicIncreaseFound = false;
                                                break;
                                            }
                                        }
                                        if (MonotonicIncreaseFound) {
                                            ShowContinueError(state,
                                                              format("Node named {} shows monotonically increasing mass flow rate with a trend "
                                                                     "rate across iterations of {:.7R} [kg/s/iteration]",
                                                                     state.dataPlnt->PlantLoop(LoopNum).LoopSide(ThisLoopSide).NodeNameOut,
                                                                     SlopeMdot));
                                        }
                                    }
                                } // significant slope found
                            }     // no oscillation found

                            if (MonotonicDecreaseFound || MonotonicIncreaseFound || FoundOscillationByDuplicate) {
                                HistoryTrace = "";
                                for (int StackDepth = 1; StackDepth <= DataPlant::NumConvergenceHistoryTerms; ++StackDepth) {
                                    HistoryTrace += format("{:.7R},", mdotHistOutletNode(StackDepth));
                                }
                                ShowContinueError(state,
                                                  format("Node named {} mass flow rate [kg/s] iteration history trace (most recent first): {}",
                                                         state.dataPlnt->PlantLoop(LoopNum).LoopSide(ThisLoopSide).NodeNameOut,
                                                         HistoryTrace));
                            } // need to report trace
                              // end of Outlet node

                        } // plant loop sides

                    } // mass flow not converged

                    if (state.dataConvergeParams->PlantConvergence(LoopNum).PlantTempNotConverged) {
                        ShowContinueError(
                            state, format("Plant System Named = {} did not converge for temperature", state.dataPlnt->PlantLoop(LoopNum).Name));
                        ShowContinueError(state, "Check values should be zero. Most Recent values listed first.");
                        std::string HistoryTrace = "";
                        for (int StackDepth = 0; StackDepth < DataConvergParams::ConvergLogStackDepth; ++StackDepth) {
                            HistoryTrace +=
                                format("{:.6R},", state.dataConvergeParams->PlantConvergence(LoopNum).PlantTempDemandToSupplyTolValue[StackDepth]);
                        }
                        ShowContinueError(state,
                                          format("Demand-to-Supply interface temperature check value iteration history trace: {}", HistoryTrace));
                        HistoryTrace = "";
                        for (int StackDepth = 0; StackDepth < DataConvergParams::ConvergLogStackDepth; ++StackDepth) {
                            HistoryTrace +=
                                format("{:.6R},", state.dataConvergeParams->PlantConvergence(LoopNum).PlantTempSupplyToDemandTolValue[StackDepth]);
                        }
                        ShowContinueError(state,
                                          format("Supply-to-Demand interface temperature check value iteration history trace: {}", HistoryTrace));

                        // now work with history logs for mass flow to detect issues
                        for (DataPlant::LoopSideLocation ThisLoopSide : DataPlant::LoopSideKeys) {

                            auto &tempHistInletNode = state.dataPlnt->PlantLoop(LoopNum).LoopSide(ThisLoopSide).InletNode.TemperatureHistory;
                            auto &tempHistOutletNode = state.dataPlnt->PlantLoop(LoopNum).LoopSide(ThisLoopSide).OutletNode.TemperatureHistory;

                            // loop side inlet node
                            FoundOscillationByDuplicate = false;
                            MonotonicDecreaseFound = false;
                            MonotonicIncreaseFound = false;
                            AvgValue = sum(tempHistInletNode) / double(DataPlant::NumConvergenceHistoryTerms);
                            if (std::abs(tempHistInletNode(1) - AvgValue) > DataConvergParams::PlantTemperatureOscillationToler) {
                                FoundOscillationByDuplicate = false;
                                for (int StackDepth = 2; StackDepth <= DataPlant::NumConvergenceHistoryTerms; ++StackDepth) {
                                    if (std::abs(tempHistInletNode(1) - tempHistInletNode(StackDepth)) <
                                        DataConvergParams::PlantTemperatureOscillationToler) {
                                        FoundOscillationByDuplicate = true;
                                        ShowContinueError(
                                            state,
                                            format("Node named {} shows oscillating temperatures across iterations with a repeated value of {:.5R}",
                                                   state.dataPlnt->PlantLoop(LoopNum).LoopSide(ThisLoopSide).NodeNameIn,
                                                   tempHistInletNode(1)));
                                        break;
                                    }
                                }
                            }
                            if (!FoundOscillationByDuplicate) {

                                Real64 tempHistInletNodeDotProd = std::inner_product(
                                    std::begin(ConvergenceHistoryARR), std::end(ConvergenceHistoryARR), std::begin(tempHistInletNode), 0.0);

                                SlopeTemps = (sum_ConvergenceHistoryARR * sum(tempHistInletNode) -
                                              double(DataPlant::NumConvergenceHistoryTerms) * tempHistInletNodeDotProd) /
                                             (square_sum_ConvergenceHistoryARR -
                                              double(DataPlant::NumConvergenceHistoryTerms) * sum_square_ConvergenceHistoryARR);
                                if (std::abs(SlopeTemps) > DataConvergParams::PlantTemperatureSlopeToler) {
                                    if (SlopeTemps < 0.0) { // check for monotonic decrease
                                        MonotonicDecreaseFound = true;
                                        for (int StackDepth = 2; StackDepth <= DataPlant::NumConvergenceHistoryTerms; ++StackDepth) {
                                            if (tempHistInletNode(StackDepth - 1) > tempHistInletNode(StackDepth)) {
                                                MonotonicDecreaseFound = false;
                                                break;
                                            }
                                        }
                                        if (MonotonicDecreaseFound) {
                                            ShowContinueError(state,
                                                              format("Node named {} shows monotonically decreasing temperatures with a trend "
                                                                     "rate across iterations of {:.5R} [C/iteration]",
                                                                     state.dataPlnt->PlantLoop(LoopNum).LoopSide(ThisLoopSide).NodeNameIn,
                                                                     SlopeTemps));
                                        }
                                    } else { // check for monotonic increase
                                        MonotonicIncreaseFound = true;
                                        for (int StackDepth = 2; StackDepth <= DataPlant::NumConvergenceHistoryTerms; ++StackDepth) {
                                            if (tempHistInletNode(StackDepth - 1) < tempHistInletNode(StackDepth)) {
                                                MonotonicIncreaseFound = false;
                                                break;
                                            }
                                        }
                                        if (MonotonicIncreaseFound) {
                                            ShowContinueError(state,
                                                              format("Node named {} shows monotonically increasing temperatures with a trend "
                                                                     "rate across iterations of {:.5R} [C/iteration]",
                                                                     state.dataPlnt->PlantLoop(LoopNum).LoopSide(ThisLoopSide).NodeNameIn,
                                                                     SlopeTemps));
                                        }
                                    }
                                } // significant slope found
                            }     // no oscillation found

                            if (MonotonicDecreaseFound || MonotonicIncreaseFound || FoundOscillationByDuplicate) {
                                HistoryTrace = "";
                                for (int StackDepth = 1; StackDepth <= DataPlant::NumConvergenceHistoryTerms; ++StackDepth) {
                                    HistoryTrace += format("{:.5R},", tempHistInletNode(StackDepth));
                                }
                                ShowContinueError(state,
                                                  format("Node named {} temperature [C] iteration history trace (most recent first): {}",
                                                         state.dataPlnt->PlantLoop(LoopNum).LoopSide(ThisLoopSide).NodeNameIn,
                                                         HistoryTrace));
                            } // need to report trace
                            // end of inlet node

                            // loop side outlet node
                            FoundOscillationByDuplicate = false;
                            MonotonicDecreaseFound = false;
                            MonotonicIncreaseFound = false;
                            AvgValue = sum(tempHistOutletNode) / double(DataPlant::NumConvergenceHistoryTerms);
                            if (std::abs(tempHistOutletNode(1) - AvgValue) > DataConvergParams::PlantTemperatureOscillationToler) {
                                FoundOscillationByDuplicate = false;
                                for (int StackDepth = 2; StackDepth <= DataPlant::NumConvergenceHistoryTerms; ++StackDepth) {
                                    if (std::abs(tempHistOutletNode(1) - tempHistOutletNode(StackDepth)) <
                                        DataConvergParams::PlantTemperatureOscillationToler) {
                                        FoundOscillationByDuplicate = true;
                                        ShowContinueError(
                                            state,
                                            format("Node named {} shows oscillating temperatures across iterations with a repeated value of {:.5R}",
                                                   state.dataPlnt->PlantLoop(LoopNum).LoopSide(ThisLoopSide).NodeNameOut,
                                                   tempHistOutletNode(1)));
                                        break;
                                    }
                                }
                            }
                            if (!FoundOscillationByDuplicate) {

                                Real64 tempHistOutletNodeDotProd = std::inner_product(
                                    std::begin(ConvergenceHistoryARR), std::end(ConvergenceHistoryARR), std::begin(tempHistOutletNode), 0.0);

                                SlopeTemps = (sum_ConvergenceHistoryARR * sum(tempHistOutletNode) -
                                              double(DataPlant::NumConvergenceHistoryTerms) * tempHistOutletNodeDotProd) /
                                             (square_sum_ConvergenceHistoryARR -
                                              double(DataPlant::NumConvergenceHistoryTerms) * sum_square_ConvergenceHistoryARR);
                                if (std::abs(SlopeTemps) > DataConvergParams::PlantFlowRateSlopeToler) {
                                    if (SlopeTemps < 0.0) { // check for monotonic decrease
                                        MonotonicDecreaseFound = true;
                                        for (int StackDepth = 2; StackDepth <= DataPlant::NumConvergenceHistoryTerms; ++StackDepth) {
                                            if (state.dataPlnt->PlantLoop(LoopNum)
                                                    .LoopSide(ThisLoopSide)
                                                    .OutletNode.TemperatureHistory(StackDepth - 1) > tempHistOutletNode(StackDepth)) {
                                                MonotonicDecreaseFound = false;
                                                break;
                                            }
                                        }
                                        if (MonotonicDecreaseFound) {
                                            ShowContinueError(state,
                                                              format("Node named {} shows monotonically decreasing temperatures with a trend "
                                                                     "rate across iterations of {:.5R} [C/iteration]",
                                                                     state.dataPlnt->PlantLoop(LoopNum).LoopSide(ThisLoopSide).NodeNameOut,
                                                                     SlopeTemps));
                                        }
                                    } else { // check for monotonic increase
                                        MonotonicIncreaseFound = true;
                                        for (int StackDepth = 2; StackDepth <= DataPlant::NumConvergenceHistoryTerms; ++StackDepth) {
                                            if (state.dataPlnt->PlantLoop(LoopNum)
                                                    .LoopSide(ThisLoopSide)
                                                    .OutletNode.TemperatureHistory(StackDepth - 1) < tempHistOutletNode(StackDepth)) {
                                                MonotonicIncreaseFound = false;
                                                break;
                                            }
                                        }
                                        if (MonotonicIncreaseFound) {
                                            ShowContinueError(state,
                                                              format("Node named {} shows monotonically increasing temperatures with a trend "
                                                                     "rate across iterations of {:.5R} [C/iteration]",
                                                                     state.dataPlnt->PlantLoop(LoopNum).LoopSide(ThisLoopSide).NodeNameOut,
                                                                     SlopeTemps));
                                        }
                                    }
                                } // significant slope found
                            }     // no oscillation found

                            if (MonotonicDecreaseFound || MonotonicIncreaseFound || FoundOscillationByDuplicate) {
                                HistoryTrace = "";
                                for (int StackDepth = 1; StackDepth <= DataPlant::NumConvergenceHistoryTerms; ++StackDepth) {
                                    HistoryTrace += format("{:.5R},", tempHistOutletNode(StackDepth));
                                }
                                ShowContinueError(state,
                                                  format("Node named {} temperature [C] iteration history trace (most recent first): {}",
                                                         state.dataPlnt->PlantLoop(LoopNum).LoopSide(ThisLoopSide).NodeNameOut,
                                                         HistoryTrace));
                            } // need to report trace
                              // end of Outlet node

                        } // plant loop sides

                    } // temperature not converged
                }     // loop over plant loop systems
            }
        } else {
            if (state.dataEnvrn->EnvironmentName == state.dataHVACMgr->ErrEnvironmentName) {
                ShowRecurringWarningErrorAtEnd(
                    state,
                    format("SimHVAC: Exceeding Maximum iterations for all HVAC loops, during {} continues", state.dataEnvrn->EnvironmentName),
                    state.dataHVACMgr->MaxErrCount);
            } else {
                state.dataHVACMgr->MaxErrCount = 0;
                state.dataHVACMgr->ErrEnvironmentName = state.dataEnvrn->EnvironmentName;
                ShowRecurringWarningErrorAtEnd(
                    state,
                    format("SimHVAC: Exceeding Maximum iterations for all HVAC loops, during {} continues", state.dataEnvrn->EnvironmentName),
                    state.dataHVACMgr->MaxErrCount);
            }
        }
    }

    CheckAirLoopFlowBalance(state);

    // Set node setpoints to a flag value so that controllers can check whether their sensed nodes
    // have a setpoint
    if (!state.dataGlobal->ZoneSizingCalc && !state.dataGlobal->SysSizingCalc) {
        if (state.dataHVACMgr->MySetPointInit) {
            if (state.dataLoopNodes->NumOfNodes > 0) {
                for (auto &e : state.dataLoopNodes->Node) {
                    e.TempSetPoint = DataLoopNode::SensedNodeFlagValue;
                    e.HumRatSetPoint = DataLoopNode::SensedNodeFlagValue;
                    e.HumRatMin = DataLoopNode::SensedNodeFlagValue;
                    e.HumRatMax = DataLoopNode::SensedNodeFlagValue;
                    e.MassFlowRateSetPoint = DataLoopNode::SensedNodeFlagValue; // BG 5-26-2009 (being checked in HVACControllers.cc)
                }
                state.dataLoopNodes->DefaultNodeValues.TempSetPoint = DataLoopNode::SensedNodeFlagValue;
                state.dataLoopNodes->DefaultNodeValues.HumRatSetPoint = DataLoopNode::SensedNodeFlagValue;
                state.dataLoopNodes->DefaultNodeValues.HumRatMin = DataLoopNode::SensedNodeFlagValue;
                state.dataLoopNodes->DefaultNodeValues.HumRatMax = DataLoopNode::SensedNodeFlagValue;
                state.dataLoopNodes->DefaultNodeValues.MassFlowRateSetPoint =
                    DataLoopNode::SensedNodeFlagValue; // BG 5-26-2009 (being checked in HVACControllers.cc)
            }
            state.dataHVACMgr->MySetPointInit = false;
            state.dataHVACGlobal->DoSetPointTest = true;
        } else {
            state.dataHVACGlobal->DoSetPointTest = false;
        }

        if (state.dataCoilCooingDX->stillNeedToReportStandardRatings) {
            if (!state.dataGlobal->WarmupFlag) {
                CoilCoolingDX::reportAllStandardRatings(state);
            }
        }
    }
    if (state.dataHVACGlobal->SetPointErrorFlag) {
        ShowFatalError(state, "Previous severe set point errors cause program termination");
    }
}

void SimSelectedEquipment(EnergyPlusData &state,
                          bool &SimAirLoops,         // True when the air loops need to be (re)simulated
                          bool &SimZoneEquipment,    // True when zone equipment components need to be (re)simulated
                          bool &SimNonZoneEquipment, // True when non-zone equipment components need to be (re)simulated
                          bool &SimPlantLoops,       // True when the main plant loops need to be (re)simulated
                          bool &SimElecCircuits,     // True when electric circuits need to be (re)simulated
                          bool &FirstHVACIteration,  // True when solution technique on first iteration
                          bool const LockPlantFlows)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Russ Taylor, Rick Strand
    //       DATE WRITTEN   May 1998

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine receives the flags from SimHVAC which determines
    // which middle-level managers must be called.

    // METHODOLOGY EMPLOYED:
    // Each flag is checked and the appropriate manager is then called.

    // SUBROUTINE PARAMETER DEFINITIONS:
    int constexpr MaxAir(5); // Iteration Max for Air Simulation Iterations

    // Set all plant flow locks to UNLOCKED to allow air side components to operate properly
    // This requires that the plant flow resolver carefully set the min/max avail limits on
    //  air side components to ensure they request within bounds.
    if (LockPlantFlows) {
        PlantUtilities::SetAllFlowLocks(state, DataPlant::FlowLock::Locked);
    } else {
        PlantUtilities::SetAllFlowLocks(state, DataPlant::FlowLock::Unlocked);
    }
    PlantUtilities::ResetAllPlantInterConnectFlags(state);

    if (state.dataGlobal->BeginEnvrnFlag && state.dataHVACMgr->MyEnvrnFlag2) {
        // Following comment is incorrect!  (LKL) Even the first time through this does more than read in data.
        // Zone equipment data needs to be read in before air loop data to allow the
        // determination of which zones are connected to which air loops.
        // This call of ManageZoneEquipment does nothing except force the
        // zone equipment data to be read in.
        ZoneEquipmentManager::ManageZoneEquipment(state, FirstHVACIteration, SimZoneEquipment, SimAirLoops);
        state.dataHVACMgr->MyEnvrnFlag2 = false;
    }
    if (!state.dataGlobal->BeginEnvrnFlag) {
        state.dataHVACMgr->MyEnvrnFlag2 = true;
    }

    if (FirstHVACIteration) {
        state.dataHVACMgr->RepIterAir = 0;
        // Call AirflowNetwork simulation to calculate air flows and pressures
        if (state.afn->simulation_control.type != AirflowNetwork::ControlType::NoMultizoneOrDistribution) {
            state.afn->manage_balance(FirstHVACIteration);
        }
        SimAirServingZones::ManageAirLoops(state, FirstHVACIteration, SimAirLoops, SimZoneEquipment);
        state.dataAirLoop->AirLoopInputsFilled = true; // all air loop inputs have been read in
        SimAirLoops = true; // Need to make sure that SimAirLoop is simulated at min twice to calculate PLR in some air loop equipment
        state.dataHVACGlobal->AirLoopsSimOnce = true; // air loops simulated once for this environment
        ResetTerminalUnitFlowLimits(state);
        state.dataHVACMgr->FlowMaxAvailAlreadyReset = true;
        ZoneEquipmentManager::ManageZoneEquipment(state, FirstHVACIteration, SimZoneEquipment, SimAirLoops);
        SimZoneEquipment = true; // needs to be simulated at least twice for flow resolution to propagate to this routine
        NonZoneEquipmentManager::ManageNonZoneEquipment(state, FirstHVACIteration, SimNonZoneEquipment);
        state.dataElectPwrSvcMgr->facilityElectricServiceObj->manageElectricPowerService(
            state, FirstHVACIteration, state.dataHVACGlobal->SimElecCircuitsFlag, false);

        PlantManager::ManagePlantLoops(state, FirstHVACIteration, SimAirLoops, SimZoneEquipment, SimNonZoneEquipment, SimPlantLoops, SimElecCircuits);

        state.dataErrTracking->AskForPlantCheckOnAbort = true; // need to make a first pass through plant calcs before this check make sense
        state.dataElectPwrSvcMgr->facilityElectricServiceObj->manageElectricPowerService(
            state, FirstHVACIteration, state.dataHVACGlobal->SimElecCircuitsFlag, false);
    } else {
        state.dataHVACMgr->FlowResolutionNeeded = false;
        int IterAir = 0;
        while ((SimAirLoops || SimZoneEquipment) && (IterAir <= MaxAir)) {
            ++IterAir; // Increment the iteration counter
            // Call AirflowNetwork simulation to calculate air flows and pressures
            bool ResimulateAirZone = false;
            if (state.afn->simulation_control.type != AirflowNetwork::ControlType::NoMultizoneOrDistribution) {
                state.afn->manage_balance(FirstHVACIteration, IterAir, ResimulateAirZone);
            }
            if (SimAirLoops) {
                SimAirServingZones::ManageAirLoops(state, FirstHVACIteration, SimAirLoops, SimZoneEquipment);
                SimElecCircuits = true; // If this was simulated there are possible electric changes that need to be simulated
            }

            // make sure flow resolution gets done
            if (state.dataHVACMgr->FlowResolutionNeeded) {
                SimZoneEquipment = true;
            }
            if (SimZoneEquipment) {
                if ((IterAir == 1) && (!state.dataHVACMgr->FlowMaxAvailAlreadyReset)) { // don't do reset if already done in FirstHVACIteration
                    // ResetTerminalUnitFlowLimits(); // don't do reset at all - interferes with convergence and terminal unit flow controls
                    state.dataHVACMgr->FlowResolutionNeeded = true;
                } else {
                    ResolveAirLoopFlowLimits(state);
                    state.dataHVACMgr->FlowResolutionNeeded = false;
                }
                ZoneEquipmentManager::ManageZoneEquipment(state, FirstHVACIteration, SimZoneEquipment, SimAirLoops);
                SimElecCircuits = true; // If this was simulated there are possible electric changes that need to be simulated
            }
            state.dataHVACMgr->FlowMaxAvailAlreadyReset = false;

            //      IterAir = IterAir + 1   ! Increment the iteration counter
            if (state.afn->simulation_control.type != AirflowNetwork::ControlType::NoMultizoneOrDistribution) {
                if (ResimulateAirZone) { // Need to make sure that SimAirLoop and SimZoneEquipment are simulated
                    SimAirLoops = true;  // at min three times using ONOFF fan with the AirflowNetwork model
                    SimZoneEquipment = true;
                }
            }
        }

        state.dataHVACMgr->RepIterAir += IterAir;
        // Check to see if any components have been locked out. If so, SimAirLoops will be reset to TRUE.
        ResolveLockoutFlags(state, SimAirLoops);

        if (SimNonZoneEquipment) {
            NonZoneEquipmentManager::ManageNonZoneEquipment(state, FirstHVACIteration, SimNonZoneEquipment);
            SimElecCircuits = true; // If this was simulated there are possible electric changes that need to be simulated
        }

        if (SimElecCircuits) {
            state.dataElectPwrSvcMgr->facilityElectricServiceObj->manageElectricPowerService(
                state, FirstHVACIteration, state.dataHVACGlobal->SimElecCircuitsFlag, false);
        }

        if (!SimPlantLoops) {
            // check to see if any air side component may have requested plant resim
            if (PlantUtilities::AnyPlantLoopSidesNeedSim(state)) {
                SimPlantLoops = true;
            }
        }

        if (SimPlantLoops) {
            PlantManager::ManagePlantLoops(
                state, FirstHVACIteration, SimAirLoops, SimZoneEquipment, SimNonZoneEquipment, SimPlantLoops, SimElecCircuits);
        }

        if (SimElecCircuits) {
            state.dataElectPwrSvcMgr->facilityElectricServiceObj->manageElectricPowerService(
                state, FirstHVACIteration, state.dataHVACGlobal->SimElecCircuitsFlag, false);
        }
    }
}

void ResetTerminalUnitFlowLimits(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   Feb 2010

    // PURPOSE OF THIS SUBROUTINE:
    // Reset the max flow available limits at the inlet nodes of terminal units

    // METHODOLOGY EMPLOYED:
    // Loops through all air loops, finds the inlet nodes of the terminal units
    // served by each air loop, and resets the node MassFlowRateMaxAvail (and MinAvail) to
    // the hard max and mins.

    for (int AirLoopIndex = 1; AirLoopIndex <= state.dataHVACGlobal->NumPrimaryAirSys; ++AirLoopIndex) { // loop over the primary air loops
        for (int ZonesCooledIndex = 1; ZonesCooledIndex <= state.dataAirLoop->AirToZoneNodeInfo(AirLoopIndex).NumZonesCooled;
             ++ZonesCooledIndex) { // loop over the zones cooled by this air loop
            int TermInletNode = state.dataAirLoop->AirToZoneNodeInfo(AirLoopIndex).TermUnitCoolInletNodes(ZonesCooledIndex);
            // reset the max avail flow rate at the terminal unit cold air inlet to the max
            state.dataLoopNodes->Node(TermInletNode).MassFlowRateMaxAvail = state.dataLoopNodes->Node(TermInletNode).MassFlowRateMax;
            state.dataLoopNodes->Node(TermInletNode).MassFlowRateMinAvail = state.dataLoopNodes->Node(TermInletNode).MassFlowRateMin;
        }
        for (int ZonesHeatedIndex = 1; ZonesHeatedIndex <= state.dataAirLoop->AirToZoneNodeInfo(AirLoopIndex).NumZonesHeated;
             ++ZonesHeatedIndex) { // loop over the zones heated by this air loop
            int TermInletNode = state.dataAirLoop->AirToZoneNodeInfo(AirLoopIndex).TermUnitHeatInletNodes(ZonesHeatedIndex);
            // reset the max avail flow rate at the terminal unit hot air inlet to the max
            state.dataLoopNodes->Node(TermInletNode).MassFlowRateMaxAvail = state.dataLoopNodes->Node(TermInletNode).MassFlowRateMax;
            state.dataLoopNodes->Node(TermInletNode).MassFlowRateMinAvail = state.dataLoopNodes->Node(TermInletNode).MassFlowRateMin;
        }
    }
}

void ResolveAirLoopFlowLimits(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   August 2003
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine is for resolving hard flow mismatches between zone equipment and
    // the primary air loop. Such a mismatch can occur when the air terminal units are
    // requesting more air than the central air system can supply.

    // METHODOLOGY EMPLOYED:
    // Sets the MassFlowRateMaxAvail on the terminal unit inlet nodes to match the
    // maximum available from the primary air loop.

    for (int AirLoopIndex = 1; AirLoopIndex <= state.dataHVACGlobal->NumPrimaryAirSys; ++AirLoopIndex) { // loop over the primary air loops

        auto &AirToZoneNodeInfo = state.dataAirLoop->AirToZoneNodeInfo(AirLoopIndex);

        for (int SupplyIndex = 1; SupplyIndex <= AirToZoneNodeInfo.NumSupplyNodes; ++SupplyIndex) {       // loop over the air loop supply outlets
            if (AirToZoneNodeInfo.SupplyDuctType(SupplyIndex) == DataHVACGlobals::AirDuctType::Cooling) { // check for cooling duct
                // check if terminal units requesting more air than air loop can supply; if so, set terminal unit inlet
                // node mass flow max avail to what air loop can supply
                int SupplyNode = AirToZoneNodeInfo.AirLoopSupplyNodeNum(SupplyIndex);
                if (state.dataLoopNodes->Node(SupplyNode).MassFlowRate > 0.0) {
                    // must include bypass flow for ChangeoverBypass system so that terminal units are not restricted (e.g., MaxAvail is lowered)
                    if ((state.dataLoopNodes->Node(SupplyNode).MassFlowRateSetPoint - state.dataLoopNodes->Node(SupplyNode).MassFlowRate -
                         state.dataAirLoop->AirLoopFlow(AirLoopIndex).BypassMassFlow) > DataConvergParams::HVACFlowRateToler * 0.01) {
                        Real64 FlowRatio =
                            state.dataLoopNodes->Node(SupplyNode).MassFlowRate / state.dataLoopNodes->Node(SupplyNode).MassFlowRateSetPoint;
                        for (int ZonesCooledIndex = 1; ZonesCooledIndex <= AirToZoneNodeInfo.NumZonesCooled; ++ZonesCooledIndex) {
                            int TermInletNode = AirToZoneNodeInfo.TermUnitCoolInletNodes(ZonesCooledIndex);
                            state.dataLoopNodes->Node(TermInletNode).MassFlowRateMaxAvail =
                                state.dataLoopNodes->Node(TermInletNode).MassFlowRate * FlowRatio;
                            state.dataLoopNodes->Node(TermInletNode).MassFlowRateMinAvail =
                                min(state.dataLoopNodes->Node(TermInletNode).MassFlowRateMaxAvail,
                                    state.dataLoopNodes->Node(TermInletNode).MassFlowRateMinAvail);
                        }
                    }
                    if ((state.dataLoopNodes->Node(SupplyNode).MassFlowRateSetPoint - state.dataLoopNodes->Node(SupplyNode).MassFlowRate -
                         state.dataAirLoop->AirLoopFlow(AirLoopIndex).BypassMassFlow) < -DataConvergParams::HVACFlowRateToler * 0.01) {
                        if (state.dataLoopNodes->Node(SupplyNode).MassFlowRateSetPoint == 0.0) {
                            //               CALL ShowFatalError('ResolveAirLoopFlowLimits: Node MassFlowRateSetPoint = 0.0, Node='//  &
                            //                                   TRIM(state.dataLoopNodes->NodeID(SupplyNode))//  &
                            //                                   ', check for Node Connection Errors in the following messages.')
                            for (int ZonesCooledIndex = 1; ZonesCooledIndex <= AirToZoneNodeInfo.NumZonesCooled; ++ZonesCooledIndex) {
                                int TermInletNode = AirToZoneNodeInfo.TermUnitCoolInletNodes(ZonesCooledIndex);
                                state.dataLoopNodes->Node(TermInletNode).MassFlowRateMaxAvail =
                                    state.dataLoopNodes->Node(TermInletNode).MassFlowRateMax;
                                state.dataLoopNodes->Node(TermInletNode).MassFlowRateMinAvail =
                                    state.dataLoopNodes->Node(SupplyNode).MassFlowRate / double(AirToZoneNodeInfo.NumZonesCooled);
                            }
                        } else {
                            Real64 FlowRatio =
                                state.dataLoopNodes->Node(SupplyNode).MassFlowRate / state.dataLoopNodes->Node(SupplyNode).MassFlowRateSetPoint;
                            for (int ZonesCooledIndex = 1; ZonesCooledIndex <= AirToZoneNodeInfo.NumZonesCooled; ++ZonesCooledIndex) {
                                int TermInletNode = AirToZoneNodeInfo.TermUnitCoolInletNodes(ZonesCooledIndex);
                                state.dataLoopNodes->Node(TermInletNode).MassFlowRateMinAvail =
                                    state.dataLoopNodes->Node(TermInletNode).MassFlowRate * FlowRatio;
                                state.dataLoopNodes->Node(TermInletNode).MassFlowRateMaxAvail =
                                    max(state.dataLoopNodes->Node(TermInletNode).MassFlowRateMaxAvail,
                                        state.dataLoopNodes->Node(TermInletNode).MassFlowRateMinAvail);
                            }
                        }
                    }
                }
            }
        }
        for (int SupplyIndex = 1; SupplyIndex <= AirToZoneNodeInfo.NumSupplyNodes; ++SupplyIndex) {       // loop over the air loop supply outlets
            if (AirToZoneNodeInfo.SupplyDuctType(SupplyIndex) == DataHVACGlobals::AirDuctType::Heating) { // check for heating duct
                // check if terminal units requesting more air than air loop can supply; if so, set terminal unit inlet
                // node mass flow max avail to what air loop can supply
                int SupplyNode = AirToZoneNodeInfo.AirLoopSupplyNodeNum(SupplyIndex);
                if (state.dataLoopNodes->Node(SupplyNode).MassFlowRate > 0.0) {
                    // must include bypass flow for ChangeoverBypass system so that terminal units are not restricted (e.g., MaxAvail is lowered)
                    if ((state.dataLoopNodes->Node(SupplyNode).MassFlowRateSetPoint - state.dataLoopNodes->Node(SupplyNode).MassFlowRate -
                         state.dataAirLoop->AirLoopFlow(AirLoopIndex).BypassMassFlow) > DataConvergParams::HVACFlowRateToler * 0.01) {
                        Real64 FlowRatio =
                            state.dataLoopNodes->Node(SupplyNode).MassFlowRate / state.dataLoopNodes->Node(SupplyNode).MassFlowRateSetPoint;
                        for (int ZonesHeatedIndex = 1; ZonesHeatedIndex <= AirToZoneNodeInfo.NumZonesHeated; ++ZonesHeatedIndex) {
                            int TermInletNode = AirToZoneNodeInfo.TermUnitHeatInletNodes(ZonesHeatedIndex);
                            state.dataLoopNodes->Node(TermInletNode).MassFlowRateMaxAvail =
                                state.dataLoopNodes->Node(TermInletNode).MassFlowRate * FlowRatio;
                            state.dataLoopNodes->Node(TermInletNode).MassFlowRateMinAvail =
                                min(state.dataLoopNodes->Node(TermInletNode).MassFlowRateMaxAvail,
                                    state.dataLoopNodes->Node(TermInletNode).MassFlowRateMinAvail);
                        }
                    }
                    if ((state.dataLoopNodes->Node(SupplyNode).MassFlowRateSetPoint - state.dataLoopNodes->Node(SupplyNode).MassFlowRate -
                         state.dataAirLoop->AirLoopFlow(AirLoopIndex).BypassMassFlow) < -DataConvergParams::HVACFlowRateToler * 0.01) {
                        if (state.dataLoopNodes->Node(SupplyNode).MassFlowRateSetPoint == 0.0) {
                            // ', check for Node Connection Errors in the following messages.')
                            for (int ZonesHeatedIndex = 1; ZonesHeatedIndex <= AirToZoneNodeInfo.NumZonesHeated; ++ZonesHeatedIndex) {
                                int TermInletNode = AirToZoneNodeInfo.TermUnitHeatInletNodes(ZonesHeatedIndex);
                                state.dataLoopNodes->Node(TermInletNode).MassFlowRateMaxAvail =
                                    state.dataLoopNodes->Node(TermInletNode).MassFlowRateMax;
                                state.dataLoopNodes->Node(TermInletNode).MassFlowRateMinAvail =
                                    state.dataLoopNodes->Node(SupplyNode).MassFlowRate / double(AirToZoneNodeInfo.NumZonesCooled);
                            }
                        } else {
                            Real64 FlowRatio =
                                state.dataLoopNodes->Node(SupplyNode).MassFlowRate / state.dataLoopNodes->Node(SupplyNode).MassFlowRateSetPoint;
                            for (int ZonesHeatedIndex = 1; ZonesHeatedIndex <= AirToZoneNodeInfo.NumZonesHeated; ++ZonesHeatedIndex) {
                                int TermInletNode = AirToZoneNodeInfo.TermUnitHeatInletNodes(ZonesHeatedIndex);
                                state.dataLoopNodes->Node(TermInletNode).MassFlowRateMinAvail =
                                    state.dataLoopNodes->Node(TermInletNode).MassFlowRate * FlowRatio;
                                state.dataLoopNodes->Node(TermInletNode).MassFlowRateMaxAvail =
                                    max(state.dataLoopNodes->Node(TermInletNode).MassFlowRateMaxAvail,
                                        state.dataLoopNodes->Node(TermInletNode).MassFlowRateMinAvail);
                            }
                        }
                    }
                }
            }
        }
    }
}

void ResolveLockoutFlags(EnergyPlusData &state, bool &SimAir) // TRUE means air loops must be (re)simulated
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   December 2003

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine checks for components lockout flags and asks for air loop resimulation
    // if any components have been locked out

    // METHODOLOGY EMPLOYED:
    // Checks if loop lockout flags are .TRUE.; if so, sets SimAirLoops to .TRUE.

    for (int AirLoopIndex = 1; AirLoopIndex <= state.dataHVACGlobal->NumPrimaryAirSys; ++AirLoopIndex) { // loop over the primary air loops
        auto &airLoopControlInfo = state.dataAirLoop->AirLoopControlInfo(AirLoopIndex);

        // check if economizer ia active and if there is a request that it be locked out
        if (airLoopControlInfo.EconoActive &&
            (airLoopControlInfo.ReqstEconoLockoutWithCompressor || airLoopControlInfo.ReqstEconoLockoutWithHeating)) {
            airLoopControlInfo.EconoLockout = true;
            SimAir = true;
        }
    }
}

void ResetHVACControl(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   December 2004

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine resets loop control flags and specified flow rates that may
    // have been set by the set point and availability managers in the previous
    // time step

    if (state.dataHVACGlobal->NumPrimaryAirSys == 0) return;
    for (auto &e : state.dataAirLoop->AirLoopControlInfo) {
        e.NightVent = false;
        e.LoopFlowRateSet = false;
    }
    for (auto &e : state.dataAirLoop->AirLoopFlow)
        e.ReqSupplyFrac = 1.0;
}

void ResetNodeData(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   March 2005

    // PURPOSE OF THIS SUBROUTINE:
    // This routine resets all node data to "initial" conditions.

    if (state.dataLoopNodes->NumOfNodes <= 0) return;

    for (auto &e : state.dataLoopNodes->Node) {
        e.Temp = state.dataLoopNodes->DefaultNodeValues.Temp;
        e.TempMin = state.dataLoopNodes->DefaultNodeValues.TempMin;
        e.TempMax = state.dataLoopNodes->DefaultNodeValues.TempMax;
        e.TempSetPoint = state.dataLoopNodes->DefaultNodeValues.TempSetPoint;
        e.MassFlowRate = state.dataLoopNodes->DefaultNodeValues.MassFlowRate;
        e.MassFlowRateMin = state.dataLoopNodes->DefaultNodeValues.MassFlowRateMin;
        e.MassFlowRateMax = state.dataLoopNodes->DefaultNodeValues.MassFlowRateMax;
        e.MassFlowRateMinAvail = state.dataLoopNodes->DefaultNodeValues.MassFlowRateMinAvail;
        e.MassFlowRateMaxAvail = state.dataLoopNodes->DefaultNodeValues.MassFlowRateMaxAvail;
        e.MassFlowRateSetPoint = state.dataLoopNodes->DefaultNodeValues.MassFlowRateSetPoint;
        e.Quality = state.dataLoopNodes->DefaultNodeValues.Quality;
        e.Press = state.dataLoopNodes->DefaultNodeValues.Press;
        e.Enthalpy = state.dataLoopNodes->DefaultNodeValues.Enthalpy;
        e.HumRat = state.dataLoopNodes->DefaultNodeValues.HumRat;
        e.HumRatMin = state.dataLoopNodes->DefaultNodeValues.HumRatMin;
        e.HumRatMax = state.dataLoopNodes->DefaultNodeValues.HumRatMax;
        e.HumRatSetPoint = state.dataLoopNodes->DefaultNodeValues.HumRatSetPoint;
        e.TempSetPointHi = state.dataLoopNodes->DefaultNodeValues.TempSetPointHi;
        e.TempSetPointLo = state.dataLoopNodes->DefaultNodeValues.TempSetPointLo;
    }

    if (allocated(state.dataLoopNodes->MoreNodeInfo)) {
        for (auto &e : state.dataLoopNodes->MoreNodeInfo) {
            e.WetBulbTemp = state.dataLoopNodes->DefaultNodeValues.Temp;
            e.RelHumidity = 0.0;
            e.ReportEnthalpy = state.dataLoopNodes->DefaultNodeValues.Enthalpy;
            e.VolFlowRateStdRho = 0.0;
            e.VolFlowRateCrntRho = 0.0;
            e.Density = 0.0;
        }
    }
}

void UpdateZoneListAndGroupLoads(EnergyPlusData &state)
{

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int ZoneNum;
    int ListNum;
    int GroupNum;
    int Mult;

    // Sum ZONE LIST and ZONE GROUP report variables
    for (ListNum = 1; ListNum <= state.dataHeatBal->NumOfZoneLists; ++ListNum) {
        state.dataHeatBal->ZoneListSNLoadHeatEnergy(ListNum) = 0.0;
        state.dataHeatBal->ZoneListSNLoadCoolEnergy(ListNum) = 0.0;
        state.dataHeatBal->ZoneListSNLoadHeatRate(ListNum) = 0.0;
        state.dataHeatBal->ZoneListSNLoadCoolRate(ListNum) = 0.0;
    }

    for (ListNum = 1; ListNum <= state.dataHeatBal->NumOfZoneLists; ++ListNum) {
        auto &zoneList = state.dataHeatBal->ZoneList(ListNum);
        for (ZoneNum = 1; ZoneNum <= zoneList.NumOfZones; ++ZoneNum) {
            auto const &zoneSysEnergyDemand = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(zoneList.Zone(ZoneNum));
            Mult = state.dataHeatBal->Zone(ZoneNum).Multiplier;
            state.dataHeatBal->ZoneListSNLoadHeatEnergy(ListNum) += zoneSysEnergyDemand.airSysHeatEnergy * Mult;
            state.dataHeatBal->ZoneListSNLoadCoolEnergy(ListNum) += zoneSysEnergyDemand.airSysCoolEnergy * Mult;
            state.dataHeatBal->ZoneListSNLoadHeatRate(ListNum) += zoneSysEnergyDemand.airSysHeatRate * Mult;
            state.dataHeatBal->ZoneListSNLoadCoolRate(ListNum) += zoneSysEnergyDemand.airSysCoolRate * Mult;
        } // ZoneNum
    }     // ListNum

    for (GroupNum = 1; GroupNum <= state.dataHeatBal->NumOfZoneGroups; ++GroupNum) {
        auto &zoneGroup = state.dataHeatBal->ZoneGroup(GroupNum);
        Mult = zoneGroup.Multiplier;
        state.dataHeatBal->ZoneGroupSNLoadHeatEnergy(GroupNum) = state.dataHeatBal->ZoneListSNLoadHeatEnergy(zoneGroup.ZoneList) * Mult;
        state.dataHeatBal->ZoneGroupSNLoadCoolEnergy(GroupNum) = state.dataHeatBal->ZoneListSNLoadCoolEnergy(zoneGroup.ZoneList) * Mult;
        state.dataHeatBal->ZoneGroupSNLoadHeatRate(GroupNum) = state.dataHeatBal->ZoneListSNLoadHeatRate(zoneGroup.ZoneList) * Mult;
        state.dataHeatBal->ZoneGroupSNLoadCoolRate(GroupNum) = state.dataHeatBal->ZoneListSNLoadCoolRate(zoneGroup.ZoneList) * Mult;
    } // GroupNum
}

void ReportInfiltrations(EnergyPlusData &state)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Yueyue Zhou
    //       DATE WRITTEN   July 2021

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine currently creates the values for standard Infiltration object level reporting

    // SUBROUTINE PARAMETER DEFINITIONS:
    static constexpr std::string_view RoutineName = "ReportInfiltrations";

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 AirDensity;          // Density of air (kg/m^3)
    Real64 CpAir;               // Heat capacity of air (J/kg-C)
    Real64 TotalLoad;           // Total loss or gain
    Real64 H2OHtOfVap;          // Heat of vaporization of air
    Real64 ADSCorrectionFactor; // Correction factor of air flow model values when ADS is simulated
    Real64 TimeStepSys = state.dataHVACGlobal->TimeStepSys;
    Real64 TimeStepSysSec = state.dataHVACGlobal->TimeStepSysSec;

    for (auto &thisInfiltration : state.dataHeatBal->Infiltration) {

        int spaceNum = thisInfiltration.spaceIndex;
        auto &thisSpaceHB = state.dataZoneTempPredictorCorrector->spaceHeatBalance(spaceNum);
        int NZ = thisInfiltration.ZonePtr;
        auto const &thisZone = state.dataHeatBal->Zone(NZ);
        ADSCorrectionFactor = 1.0;
        if (state.afn->simulation_control.type == AirflowNetwork::ControlType::MultizoneWithDistributionOnlyDuringFanOperation) {
            // CR7608 IF (TurnFansOn .AND. AirflowNetworkZoneFlag(NZ)) ADSCorrectionFactor=0
            if ((state.dataZoneEquip->ZoneEquipAvail(NZ) == DataHVACGlobals::CycleOn ||
                 state.dataZoneEquip->ZoneEquipAvail(NZ) == DataHVACGlobals::CycleOnZoneFansOnly) &&
                state.afn->AirflowNetworkZoneFlag(NZ))
                ADSCorrectionFactor = 0.0;
        }

        CpAir = Psychrometrics::PsyCpAirFnW(state.dataEnvrn->OutHumRat);
        thisInfiltration.InfilMdot = thisInfiltration.MCpI_temp / CpAir * ADSCorrectionFactor;
        thisInfiltration.InfilMass = thisInfiltration.InfilMdot * TimeStepSysSec;

        if (thisSpaceHB.MAT > thisZone.OutDryBulbTemp) {

            thisInfiltration.InfilHeatLoss =
                thisInfiltration.MCpI_temp * (thisSpaceHB.MAT - thisZone.OutDryBulbTemp) * TimeStepSysSec * ADSCorrectionFactor;
            thisInfiltration.InfilHeatGain = 0.0;

        } else {

            thisInfiltration.InfilHeatGain =
                thisInfiltration.MCpI_temp * (thisZone.OutDryBulbTemp - thisSpaceHB.MAT) * TimeStepSysSec * ADSCorrectionFactor;
            thisInfiltration.InfilHeatLoss = 0.0;
        }

        // Report infiltration latent gains and losses
        H2OHtOfVap = Psychrometrics::PsyHgAirFnWTdb(thisSpaceHB.airHumRat, thisSpaceHB.MAT);
        if (thisSpaceHB.airHumRat > state.dataEnvrn->OutHumRat) {

            thisInfiltration.InfilLatentLoss =
                thisInfiltration.InfilMdot * (thisSpaceHB.airHumRat - state.dataEnvrn->OutHumRat) * H2OHtOfVap * TimeStepSysSec;
            thisInfiltration.InfilLatentGain = 0.0;

        } else {

            thisInfiltration.InfilLatentGain =
                thisInfiltration.InfilMdot * (state.dataEnvrn->OutHumRat - thisSpaceHB.airHumRat) * H2OHtOfVap * TimeStepSysSec;
            thisInfiltration.InfilLatentLoss = 0.0;
        }
        // Total infiltration losses and gains
        TotalLoad =
            thisInfiltration.InfilHeatGain + thisInfiltration.InfilLatentGain - thisInfiltration.InfilHeatLoss - thisInfiltration.InfilLatentLoss;
        if (TotalLoad > 0) {
            thisInfiltration.InfilTotalGain = TotalLoad;
            thisInfiltration.InfilTotalLoss = 0.0;
        } else {
            thisInfiltration.InfilTotalGain = 0.0;
            thisInfiltration.InfilTotalLoss = -TotalLoad;
        }
        // CR7751  second, calculate using indoor conditions for density property
        AirDensity = Psychrometrics::PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, thisSpaceHB.MAT, thisSpaceHB.airHumRatAvg, RoutineName);
        thisInfiltration.InfilVdotCurDensity = thisInfiltration.InfilMdot / AirDensity;
        thisInfiltration.InfilVolumeCurDensity = thisInfiltration.InfilVdotCurDensity * TimeStepSysSec;
        thisInfiltration.InfilAirChangeRate = thisInfiltration.InfilVolumeCurDensity / (TimeStepSys * thisZone.Volume);

        // CR7751 third, calculate using standard dry air at nominal elevation
        AirDensity = state.dataEnvrn->StdRhoAir;
        thisInfiltration.InfilVdotStdDensity = thisInfiltration.InfilMdot / AirDensity;
        thisInfiltration.InfilVolumeStdDensity = thisInfiltration.InfilVdotStdDensity * TimeStepSysSec;
    }
}

void ReportAirHeatBalance(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   July 2000
    //       MODIFIED       Shirey, Jan 2008 (MIXING/CROSS MIXING outputs)

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine updates the report variables for the AirHeatBalance.

    static constexpr std::string_view RoutineName3("ReportAirHeatBalance:3");

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 AirDensity;          // Density of air (kg/m^3)
    Real64 CpAir;               // Heat capacity of air (J/kg-C)
    Real64 ADSCorrectionFactor; // Correction factor of air flow model values when ADS is simulated
    Real64 H2OHtOfVap;          // Heat of vaporization of air
    Real64 TotalLoad;           // Total loss or gain

    state.dataHeatBal->ZoneTotalExfiltrationHeatLoss = 0.0;
    state.dataHeatBal->ZoneTotalExhaustHeatLoss = 0.0;

    Real64 TimeStepSys = state.dataHVACGlobal->TimeStepSys;
    Real64 TimeStepSysSec = state.dataHVACGlobal->TimeStepSysSec;

    if (state.afn->simulation_control.type != AirflowNetwork::ControlType::NoMultizoneOrDistribution) {
        state.afn->report();
    }

    // Reports zone exhaust loss by exhaust fans
    for (int ZoneLoop = 1; ZoneLoop <= state.dataGlobal->NumOfZones; ++ZoneLoop) { // Start of zone loads report variable update loop ...
        auto &zone = state.dataHeatBal->Zone(ZoneLoop);
        auto &znAirRpt = state.dataHeatBal->ZnAirRpt(ZoneLoop);
        auto &zoneEquipConfig = state.dataZoneEquip->ZoneEquipConfig(ZoneLoop);
        CpAir = Psychrometrics::PsyCpAirFnW(state.dataEnvrn->OutHumRat);
        H2OHtOfVap = Psychrometrics::PsyHgAirFnWTdb(state.dataEnvrn->OutHumRat, zone.OutDryBulbTemp);
        ADSCorrectionFactor = 1.0;
        if (state.afn->simulation_control.type == AirflowNetwork::ControlType::MultizoneWithDistributionOnlyDuringFanOperation) {
            if ((state.dataZoneEquip->ZoneEquipAvail(ZoneLoop) == DataHVACGlobals::CycleOn ||
                 state.dataZoneEquip->ZoneEquipAvail(ZoneLoop) == DataHVACGlobals::CycleOnZoneFansOnly) &&
                state.afn->AirflowNetworkZoneFlag(ZoneLoop)) {
                ADSCorrectionFactor = 0.0;
            }
        }

        znAirRpt.ExhTotalLoss = 0;
        znAirRpt.ExhSensiLoss = 0;

        for (int FanNum = 1; FanNum <= state.dataFans->NumFans; ++FanNum) {
            auto const &thisFan = state.dataFans->Fan(FanNum);
            //  Add reportable vars
            if (thisFan.FanType_Num == DataHVACGlobals::FanType_ZoneExhaust) {
                for (int ExhNum = 1; ExhNum <= zoneEquipConfig.NumExhaustNodes; ExhNum++) {
                    if (thisFan.InletNodeNum == zoneEquipConfig.ExhaustNode(ExhNum)) {
                        znAirRpt.ExhTotalLoss +=
                            thisFan.OutletAirMassFlowRate * (thisFan.OutletAirEnthalpy - state.dataEnvrn->OutEnthalpy) * ADSCorrectionFactor;
                        znAirRpt.ExhSensiLoss +=
                            thisFan.OutletAirMassFlowRate * CpAir * (thisFan.OutletAirTemp - zone.OutDryBulbTemp) * ADSCorrectionFactor;
                        break;
                    }
                }
            }
        }

        znAirRpt.ExhLatentLoss = znAirRpt.ExhTotalLoss - znAirRpt.ExhSensiLoss;
    }

    // Report results for SIMPLE option only
    if (!(state.afn->simulation_control.type == AirflowNetwork::ControlType::NoMultizoneOrDistribution ||
          state.afn->simulation_control.type == AirflowNetwork::ControlType::MultizoneWithDistributionOnlyDuringFanOperation)) {
        return;
    }

    if (state.dataHVACMgr->ReportAirHeatBalanceFirstTimeFlag) {
        state.dataHVACMgr->MixSenLoad.allocate(state.dataGlobal->NumOfZones);
        state.dataHVACMgr->MixLatLoad.allocate(state.dataGlobal->NumOfZones);
        state.dataHVACMgr->ReportAirHeatBalanceFirstTimeFlag = false;
    }

    ReportInfiltrations(state);

    for (int ZoneLoop = 1; ZoneLoop <= state.dataGlobal->NumOfZones; ++ZoneLoop) { // Start of zone loads report variable update loop ...
        auto &zone = state.dataHeatBal->Zone(ZoneLoop);
        auto &znAirRpt = state.dataHeatBal->ZnAirRpt(ZoneLoop);
        auto &zoneEquipConfig = state.dataZoneEquip->ZoneEquipConfig(ZoneLoop);
        auto &thisZoneHB = state.dataZoneTempPredictorCorrector->zoneHeatBalance(ZoneLoop);
        auto &mixSenLoad = state.dataHVACMgr->MixSenLoad(ZoneLoop); // Mixing sensible loss or gain
        auto &mixLatLoad = state.dataHVACMgr->MixLatLoad(ZoneLoop); // Mixing latent loss or gain

        // Break the infiltration load into heat gain and loss components
        ADSCorrectionFactor = 1.0;

        if (state.afn->simulation_control.type == AirflowNetwork::ControlType::MultizoneWithDistributionOnlyDuringFanOperation) {
            // CR7608 IF (TurnFansOn .AND. AirflowNetworkZoneFlag(ZoneLoop)) ADSCorrectionFactor=0
            if ((state.dataZoneEquip->ZoneEquipAvail(ZoneLoop) == DataHVACGlobals::CycleOn ||
                 state.dataZoneEquip->ZoneEquipAvail(ZoneLoop) == DataHVACGlobals::CycleOnZoneFansOnly) &&
                state.afn->AirflowNetworkZoneFlag(ZoneLoop))
                ADSCorrectionFactor = 0.0;
        }

        if (thisZoneHB.MAT > zone.OutDryBulbTemp) {

            znAirRpt.InfilHeatLoss = thisZoneHB.MCPI * (thisZoneHB.MAT - zone.OutDryBulbTemp) * TimeStepSysSec * ADSCorrectionFactor;
            znAirRpt.InfilHeatGain = 0.0;

        } else {

            znAirRpt.InfilHeatGain = thisZoneHB.MCPI * (zone.OutDryBulbTemp - thisZoneHB.MAT) * TimeStepSysSec * ADSCorrectionFactor;
            znAirRpt.InfilHeatLoss = 0.0;
        }
        // Report infiltration latent gains and losses
        CpAir = Psychrometrics::PsyCpAirFnW(state.dataEnvrn->OutHumRat);
        H2OHtOfVap = Psychrometrics::PsyHgAirFnWTdb(thisZoneHB.airHumRat, thisZoneHB.MAT);
        if (thisZoneHB.airHumRat > state.dataEnvrn->OutHumRat) {

            znAirRpt.InfilLatentLoss =
                thisZoneHB.MCPI / CpAir * (thisZoneHB.airHumRat - state.dataEnvrn->OutHumRat) * H2OHtOfVap * TimeStepSysSec * ADSCorrectionFactor;
            znAirRpt.InfilLatentGain = 0.0;

        } else {

            znAirRpt.InfilLatentGain =
                thisZoneHB.MCPI / CpAir * (state.dataEnvrn->OutHumRat - thisZoneHB.airHumRat) * H2OHtOfVap * TimeStepSysSec * ADSCorrectionFactor;
            znAirRpt.InfilLatentLoss = 0.0;
        }
        // Total infiltration losses and gains
        TotalLoad = znAirRpt.InfilHeatGain + znAirRpt.InfilLatentGain - znAirRpt.InfilHeatLoss - znAirRpt.InfilLatentLoss;
        if (TotalLoad > 0) {
            znAirRpt.InfilTotalGain = TotalLoad * ADSCorrectionFactor;
            znAirRpt.InfilTotalLoss = 0.0;
        } else {
            znAirRpt.InfilTotalGain = 0.0;
            znAirRpt.InfilTotalLoss = -TotalLoad * ADSCorrectionFactor;
        }

        // first calculate mass flows using outside air heat capacity for consistency with input to heat balance
        znAirRpt.InfilMdot = (thisZoneHB.MCPI / CpAir) * ADSCorrectionFactor;
        znAirRpt.InfilMass = znAirRpt.InfilMdot * TimeStepSysSec;
        znAirRpt.VentilMdot = (thisZoneHB.MCPV / CpAir) * ADSCorrectionFactor;
        znAirRpt.VentilMass = znAirRpt.VentilMdot * TimeStepSysSec;

        // CR7751  second, calculate using indoor conditions for density property
        AirDensity = Psychrometrics::PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, thisZoneHB.MAT, thisZoneHB.airHumRatAvg, RoutineName3);
        znAirRpt.InfilVdotCurDensity = znAirRpt.InfilMdot / AirDensity;
        znAirRpt.InfilVolumeCurDensity = znAirRpt.InfilVdotCurDensity * TimeStepSysSec;
        znAirRpt.InfilAirChangeRate = znAirRpt.InfilVolumeCurDensity / (TimeStepSys * zone.Volume);
        znAirRpt.VentilVdotCurDensity = znAirRpt.VentilMdot / AirDensity;
        znAirRpt.VentilVolumeCurDensity = znAirRpt.VentilVdotCurDensity * TimeStepSysSec;
        znAirRpt.VentilAirChangeRate = znAirRpt.VentilVolumeCurDensity / (TimeStepSys * zone.Volume);

        // CR7751 third, calculate using standard dry air at nominal elevation
        AirDensity = state.dataEnvrn->StdRhoAir;
        znAirRpt.InfilVdotStdDensity = znAirRpt.InfilMdot / AirDensity;
        znAirRpt.InfilVolumeStdDensity = znAirRpt.InfilVdotStdDensity * TimeStepSysSec;
        znAirRpt.VentilVdotStdDensity = znAirRpt.VentilMdot / AirDensity;
        znAirRpt.VentilVolumeStdDensity = znAirRpt.VentilVdotStdDensity * TimeStepSysSec;

        //    znAirRpt%VentilFanElec = 0.0
        znAirRpt.VentilAirTemp = 0.0;
        znAirRpt.VentilHeatLoss = 0.0;
        znAirRpt.VentilHeatGain = 0.0;
        int VentZoneNum = 0;           // Number of ventilation object per zone
        Real64 VentZoneMassflow = 0.0; // Total mass flow rate per zone
        Real64 VentZoneAirTemp = 0.0;  // Average Zone inlet temperature

        for (int VentNum = 1; VentNum <= state.dataHeatBal->TotVentilation; ++VentNum) {
            auto const &ventilation = state.dataHeatBal->Ventilation(VentNum);
            if (ventilation.ZonePtr == ZoneLoop) {
                if (ADSCorrectionFactor > 0) {
                    znAirRpt.VentilAirTemp += ventilation.AirTemp * ventilation.MCP;
                    VentZoneMassflow += ventilation.MCP;
                    VentZoneAirTemp += ventilation.AirTemp;
                } else {
                    znAirRpt.VentilAirTemp = zone.OutDryBulbTemp;
                }
                // Break the ventilation load into heat gain and loss components
                if (thisZoneHB.MAT > ventilation.AirTemp) {
                    znAirRpt.VentilHeatLoss += ventilation.MCP * (thisZoneHB.MAT - ventilation.AirTemp) * TimeStepSysSec * ADSCorrectionFactor;
                } else {
                    znAirRpt.VentilHeatGain += ventilation.MCP * (ventilation.AirTemp - thisZoneHB.MAT) * TimeStepSysSec * ADSCorrectionFactor;
                }

                ++VentZoneNum;
                if (VentZoneNum > 1) continue;

                // Report ventilation latent gains and losses
                H2OHtOfVap = Psychrometrics::PsyHgAirFnWTdb(thisZoneHB.airHumRat, thisZoneHB.MAT);
                if (thisZoneHB.airHumRat > state.dataEnvrn->OutHumRat) {
                    znAirRpt.VentilLatentLoss =
                        znAirRpt.VentilMdot * (thisZoneHB.airHumRat - state.dataEnvrn->OutHumRat) * H2OHtOfVap * TimeStepSysSec;
                    znAirRpt.VentilLatentGain = 0.0;
                } else {
                    znAirRpt.VentilLatentGain =
                        znAirRpt.VentilMdot * (state.dataEnvrn->OutHumRat - thisZoneHB.airHumRat) * H2OHtOfVap * TimeStepSysSec;
                    znAirRpt.VentilLatentLoss = 0.0;
                }
                // Total ventilation losses and gains
                TotalLoad = znAirRpt.VentilHeatGain + znAirRpt.VentilLatentGain - znAirRpt.VentilHeatLoss - znAirRpt.VentilLatentLoss;
                if (TotalLoad > 0) {
                    znAirRpt.VentilTotalGain = TotalLoad * ADSCorrectionFactor;
                    znAirRpt.VentilTotalLoss = 0.0;
                } else {
                    znAirRpt.VentilTotalGain = 0.0;
                    znAirRpt.VentilTotalLoss = -TotalLoad * ADSCorrectionFactor;
                }
            }
        }

        if (ADSCorrectionFactor > 0 && VentZoneNum > 1 && VentZoneMassflow > 0.0) {
            znAirRpt.VentilAirTemp /= VentZoneMassflow;
        } else if (ADSCorrectionFactor > 0 && VentZoneNum == 1) {
            znAirRpt.VentilAirTemp = VentZoneAirTemp;
        } else { // Just in case
            znAirRpt.VentilAirTemp = zone.OutDryBulbTemp;
        }

        // Report mixing sensible and latent loads
        mixSenLoad = 0.0; // Initialize arrays to zero before starting to sum
        mixLatLoad = 0.0;
        znAirRpt.MixVolume = 0.0;         // zero reported volume prior to summations below
        znAirRpt.MixVdotCurDensity = 0.0; // zero reported volume flow rate prior to summations below
        znAirRpt.MixVdotStdDensity = 0.0; // zero reported volume flow rate prior to summations below
        znAirRpt.MixMass = 0.0;           // ! zero reported mass prior to summations below
        znAirRpt.MixMdot = 0.0;           // ! zero reported mass flow rate prior to summations below
        //    MixingLoad = 0.0d0

        for (int MixNum = 1; MixNum <= state.dataHeatBal->TotMixing; ++MixNum) {
            auto &mixing = state.dataHeatBal->Mixing(MixNum);
            if ((mixing.ZonePtr == ZoneLoop) && mixing.ReportFlag) {
                auto const &fromZoneHB = state.dataZoneTempPredictorCorrector->zoneHeatBalance(mixing.FromZone);
                //        MixSenLoad(ZoneLoop) = MixSenLoad(ZoneLoop)+MCPM(ZoneLoop)*MAT(mixing%FromZone)
                //        H2OHtOfVap = Psychrometrics::PsyHgAirFnWTdb(ZoneAirHumRat(ZoneLoop), MAT(ZoneLoop))
                //        Per Jan 17, 2008 conference call, agreed to use average conditions for Rho, Cp and Hfg
                //           and to recalculate the report variable using end of time step temps and humrats
                AirDensity = Psychrometrics::PsyRhoAirFnPbTdbW(state,
                                                               state.dataEnvrn->OutBaroPress,
                                                               (thisZoneHB.MAT + fromZoneHB.MAT) / 2.0,
                                                               (thisZoneHB.airHumRat + fromZoneHB.airHumRat) / 2.0,
                                                               std::string());
                CpAir = Psychrometrics::PsyCpAirFnW((thisZoneHB.airHumRat + fromZoneHB.airHumRat) / 2.0);
                znAirRpt.MixVolume += mixing.DesiredAirFlowRate * TimeStepSysSec * ADSCorrectionFactor;
                znAirRpt.MixVdotCurDensity += mixing.DesiredAirFlowRate * ADSCorrectionFactor;
                znAirRpt.MixMass += mixing.DesiredAirFlowRate * AirDensity * TimeStepSysSec * ADSCorrectionFactor;
                znAirRpt.MixMdot += mixing.DesiredAirFlowRate * AirDensity * ADSCorrectionFactor;
                znAirRpt.MixVdotStdDensity += mixing.DesiredAirFlowRate * (AirDensity / state.dataEnvrn->StdRhoAir) * ADSCorrectionFactor;
                mixSenLoad += mixing.DesiredAirFlowRate * AirDensity * CpAir * (thisZoneHB.MAT - fromZoneHB.MAT);
                H2OHtOfVap =
                    Psychrometrics::PsyHgAirFnWTdb((thisZoneHB.airHumRat + fromZoneHB.airHumRat) / 2.0, (thisZoneHB.MAT + fromZoneHB.MAT) / 2.0);
                //        MixLatLoad(ZoneLoop) = MixLatLoad(ZoneLoop)+MixingMassFlowzone*(ZoneAirHumRat(ZoneLoop)- &
                //                     ZoneAirHumRat(mixing%FromZone))*H2OHtOfVap
                mixLatLoad += mixing.DesiredAirFlowRate * AirDensity * (thisZoneHB.airHumRat - fromZoneHB.airHumRat) * H2OHtOfVap;
            }
        }

        for (int MixNum = 1; MixNum <= state.dataHeatBal->TotCrossMixing; ++MixNum) {
            auto &crossMixing = state.dataHeatBal->CrossMixing(MixNum);
            if ((crossMixing.ZonePtr == ZoneLoop) && crossMixing.ReportFlag) {
                auto const &fromZoneHB = state.dataZoneTempPredictorCorrector->zoneHeatBalance(crossMixing.FromZone);
                //        MixSenLoad(ZoneLoop) = MixSenLoad(ZoneLoop)+MCPM(ZoneLoop)*MAT(crossMixing%FromZone)
                //        Per Jan 17, 2008 conference call, agreed to use average conditions for Rho, Cp and Hfg
                //           and to recalculate the report variable using end of time step temps and humrats
                AirDensity = Psychrometrics::PsyRhoAirFnPbTdbW(state,
                                                               state.dataEnvrn->OutBaroPress,
                                                               (thisZoneHB.MAT + fromZoneHB.MAT) / 2.0,
                                                               (thisZoneHB.airHumRat + fromZoneHB.airHumRat) / 2.0,
                                                               std::string());
                CpAir = Psychrometrics::PsyCpAirFnW((thisZoneHB.airHumRat + fromZoneHB.airHumRat) / 2.0);
                znAirRpt.MixVolume += crossMixing.DesiredAirFlowRate * TimeStepSysSec * ADSCorrectionFactor;
                znAirRpt.MixVdotCurDensity += crossMixing.DesiredAirFlowRate * ADSCorrectionFactor;
                znAirRpt.MixMass += crossMixing.DesiredAirFlowRate * AirDensity * TimeStepSysSec * ADSCorrectionFactor;
                znAirRpt.MixMdot += crossMixing.DesiredAirFlowRate * AirDensity * ADSCorrectionFactor;
                znAirRpt.MixVdotStdDensity += crossMixing.DesiredAirFlowRate * (AirDensity / state.dataEnvrn->StdRhoAir) * ADSCorrectionFactor;
                mixSenLoad += crossMixing.DesiredAirFlowRate * AirDensity * CpAir * (thisZoneHB.MAT - fromZoneHB.MAT);
                H2OHtOfVap =
                    Psychrometrics::PsyHgAirFnWTdb((thisZoneHB.airHumRat + fromZoneHB.airHumRat) / 2.0, (thisZoneHB.MAT + fromZoneHB.MAT) / 2.0);
                //       MixLatLoad(ZoneLoop) = MixLatLoad(ZoneLoop)+MixingMassFlowzone*(ZoneAirHumRat(ZoneLoop)- &
                //                     ZoneAirHumRat(crossMixing%FromZone))*H2OHtOfVap
                mixLatLoad += crossMixing.DesiredAirFlowRate * AirDensity * (thisZoneHB.airHumRat - fromZoneHB.airHumRat) * H2OHtOfVap;
            }
            if ((crossMixing.FromZone == ZoneLoop) && crossMixing.ReportFlag) {
                auto const &mixingZoneHB = state.dataZoneTempPredictorCorrector->zoneHeatBalance(crossMixing.ZonePtr);
                AirDensity = Psychrometrics::PsyRhoAirFnPbTdbW(state,
                                                               state.dataEnvrn->OutBaroPress,
                                                               (thisZoneHB.MAT + mixingZoneHB.MAT) / 2.0,
                                                               (thisZoneHB.airHumRat + mixingZoneHB.airHumRat) / 2.0,
                                                               std::string());
                CpAir = Psychrometrics::PsyCpAirFnW((thisZoneHB.airHumRat + mixingZoneHB.airHumRat) / 2.0);
                znAirRpt.MixVolume += crossMixing.DesiredAirFlowRate * TimeStepSysSec * ADSCorrectionFactor;
                znAirRpt.MixVdotCurDensity += crossMixing.DesiredAirFlowRate * ADSCorrectionFactor;
                znAirRpt.MixMass += crossMixing.DesiredAirFlowRate * AirDensity * TimeStepSysSec * ADSCorrectionFactor;
                znAirRpt.MixMdot += crossMixing.DesiredAirFlowRate * AirDensity * ADSCorrectionFactor;
                znAirRpt.MixVdotStdDensity += crossMixing.DesiredAirFlowRate * (AirDensity / state.dataEnvrn->StdRhoAir) * ADSCorrectionFactor;
                mixSenLoad += crossMixing.DesiredAirFlowRate * AirDensity * CpAir * (thisZoneHB.MAT - mixingZoneHB.MAT);
                H2OHtOfVap =
                    Psychrometrics::PsyHgAirFnWTdb((thisZoneHB.airHumRat + mixingZoneHB.airHumRat) / 2.0, (thisZoneHB.MAT + mixingZoneHB.MAT) / 2.0);
                mixLatLoad += crossMixing.DesiredAirFlowRate * AirDensity * (thisZoneHB.airHumRat - mixingZoneHB.airHumRat) * H2OHtOfVap;
            }
        }

        if (state.dataHeatBal->TotRefDoorMixing > 0) {
            // IF(ZoneLoop .NE. NumOfZones)THEN  !Refrigeration Door Mixing
            // Note - do each Pair a Single time, so must do increment reports for both zones
            //       Can't have a pair that has ZoneA zone number = NumOfZones because organized
            //       in input with lowest zone # first no matter how input in idf
            auto &refDoorMixing = state.dataHeatBal->RefDoorMixing(ZoneLoop);
            if (refDoorMixing.RefDoorMixFlag) { // .TRUE. for both zoneA and zoneB
                if (refDoorMixing.ZonePtr == ZoneLoop) {
                    for (int j = 1; j <= refDoorMixing.NumRefDoorConnections; ++j) {
                        //    Capture impact when zoneloop is the 'primary zone'
                        //    that is, the zone of a pair with the lower zone number
                        if (refDoorMixing.VolRefDoorFlowRate(j) > 0.0) {
                            int ZoneB = refDoorMixing.MateZonePtr(j);
                            auto const &zoneBHB = state.dataZoneTempPredictorCorrector->zoneHeatBalance(ZoneB);
                            AirDensity = Psychrometrics::PsyRhoAirFnPbTdbW(state,
                                                                           state.dataEnvrn->OutBaroPress,
                                                                           (thisZoneHB.MAT + zoneBHB.MAT) / 2.0,
                                                                           (thisZoneHB.airHumRat + zoneBHB.airHumRat) / 2.0,
                                                                           std::string());
                            CpAir = Psychrometrics::PsyCpAirFnW((thisZoneHB.airHumRat + zoneBHB.airHumRat) / 2.0);
                            H2OHtOfVap = Psychrometrics::PsyHgAirFnWTdb((thisZoneHB.airHumRat + zoneBHB.airHumRat) / 2.0,
                                                                        (thisZoneHB.MAT + zoneBHB.MAT) / 2.0);
                            znAirRpt.MixVolume += refDoorMixing.VolRefDoorFlowRate(j) * TimeStepSysSec * ADSCorrectionFactor;
                            znAirRpt.MixVdotCurDensity += refDoorMixing.VolRefDoorFlowRate(j) * ADSCorrectionFactor;
                            znAirRpt.MixMass += refDoorMixing.VolRefDoorFlowRate(j) * AirDensity * TimeStepSysSec * ADSCorrectionFactor;
                            znAirRpt.MixMdot += refDoorMixing.VolRefDoorFlowRate(j) * AirDensity * ADSCorrectionFactor;
                            znAirRpt.MixVdotStdDensity +=
                                refDoorMixing.VolRefDoorFlowRate(j) * (AirDensity / state.dataEnvrn->StdRhoAir) * ADSCorrectionFactor;
                            mixSenLoad += refDoorMixing.VolRefDoorFlowRate(j) * AirDensity * CpAir * (thisZoneHB.MAT - zoneBHB.MAT);
                            mixLatLoad += refDoorMixing.VolRefDoorFlowRate(j) * AirDensity * (thisZoneHB.airHumRat - zoneBHB.airHumRat) * H2OHtOfVap;
                        } // flow > 0
                    }     // J-1, numref connections
                }         // zone A (zoneptr = zoneloop)
                for (int ZoneA = 1; ZoneA <= (ZoneLoop - 1); ++ZoneA) {
                    auto &refDoorMixingA = state.dataHeatBal->RefDoorMixing(ZoneA);
                    auto const &zoneAHB = state.dataZoneTempPredictorCorrector->zoneHeatBalance(ZoneA);
                    //    Capture impact when zoneloop is the 'mating zone'
                    //    that is, the zone of a pair with the higher zone number(matezoneptr = zoneloop)
                    if (refDoorMixingA.RefDoorMixFlag) {
                        for (int j = 1; j <= refDoorMixingA.NumRefDoorConnections; ++j) {
                            if (refDoorMixingA.MateZonePtr(j) == ZoneLoop) {
                                if (refDoorMixingA.VolRefDoorFlowRate(j) > 0.0) {
                                    AirDensity = Psychrometrics::PsyRhoAirFnPbTdbW(state,
                                                                                   state.dataEnvrn->OutBaroPress,
                                                                                   (thisZoneHB.MAT + zoneAHB.MAT) / 2.0,
                                                                                   (thisZoneHB.airHumRat + zoneAHB.airHumRat) / 2.0,
                                                                                   std::string());
                                    CpAir = Psychrometrics::PsyCpAirFnW((thisZoneHB.airHumRat + zoneAHB.airHumRat) / 2.0);
                                    H2OHtOfVap = Psychrometrics::PsyHgAirFnWTdb((thisZoneHB.airHumRat + zoneAHB.airHumRat) / 2.0,
                                                                                (thisZoneHB.MAT + zoneAHB.MAT) / 2.0);
                                    znAirRpt.MixVolume += refDoorMixingA.VolRefDoorFlowRate(j) * TimeStepSysSec * ADSCorrectionFactor;
                                    znAirRpt.MixVdotCurDensity += refDoorMixingA.VolRefDoorFlowRate(j) * ADSCorrectionFactor;
                                    znAirRpt.MixMass += refDoorMixingA.VolRefDoorFlowRate(j) * AirDensity * TimeStepSysSec * ADSCorrectionFactor;
                                    znAirRpt.MixMdot += refDoorMixingA.VolRefDoorFlowRate(j) * AirDensity * ADSCorrectionFactor;
                                    znAirRpt.MixVdotStdDensity +=
                                        refDoorMixingA.VolRefDoorFlowRate(j) * (AirDensity / state.dataEnvrn->StdRhoAir) * ADSCorrectionFactor;
                                    mixSenLoad += refDoorMixingA.VolRefDoorFlowRate(j) * AirDensity * CpAir * (thisZoneHB.MAT - zoneAHB.MAT);
                                    mixLatLoad +=
                                        refDoorMixingA.VolRefDoorFlowRate(j) * AirDensity * (thisZoneHB.airHumRat - zoneAHB.airHumRat) * H2OHtOfVap;
                                } // volflowrate > 0
                            }     // matezoneptr (zoneB) = Zonelooop
                        }         // NumRefDoorConnections
                    }             // Refdoormix flag on ZoneA
                }                 // zone A from 1 to (zoneloop - 1)
            }                     // Refdoormix flag on zoneloop
        }                         //(TotRefDoorMixing .GT. 0)
        // end refrigeration door mixing reports

        //    MixingLoad(ZoneLoop) = MCPM(ZoneLoop)*MAT(ZoneLoop) - MixSenLoad(ZoneLoop)
        if (mixSenLoad > 0.0) {
            znAirRpt.MixHeatLoss = mixSenLoad * TimeStepSysSec * ADSCorrectionFactor;
            znAirRpt.MixHeatGain = 0.0;
        } else {
            znAirRpt.MixHeatLoss = 0.0;
            znAirRpt.MixHeatGain = -mixSenLoad * TimeStepSysSec * ADSCorrectionFactor;
        }
        // Report mixing latent loads
        //    MixingLoad(ZoneLoop) = MixLatLoad(ZoneLoop)
        if (mixLatLoad > 0.0) {
            znAirRpt.MixLatentLoss = mixLatLoad * TimeStepSysSec * ADSCorrectionFactor;
            znAirRpt.MixLatentGain = 0.0;
        } else {
            znAirRpt.MixLatentLoss = 0.0;
            znAirRpt.MixLatentGain = -mixLatLoad * TimeStepSysSec * ADSCorrectionFactor;
        }
        // Total Mixing losses and gains
        TotalLoad = znAirRpt.MixHeatGain + znAirRpt.MixLatentGain - znAirRpt.MixHeatLoss - znAirRpt.MixLatentLoss;
        if (TotalLoad > 0) {
            znAirRpt.MixTotalGain = TotalLoad * ADSCorrectionFactor;
            znAirRpt.MixTotalLoss = 0.0;
        } else {
            znAirRpt.MixTotalGain = 0.0;
            znAirRpt.MixTotalLoss = -TotalLoad * ADSCorrectionFactor;
        }

        // Reporting combined outdoor air flows
        for (int j = 1; j <= state.dataHeatBal->TotZoneAirBalance; ++j) {
            if (state.dataHeatBal->ZoneAirBalance(j).BalanceMethod == DataHeatBalance::AirBalance::Quadrature &&
                ZoneLoop == state.dataHeatBal->ZoneAirBalance(j).ZonePtr) {
                if (thisZoneHB.MAT > zone.OutDryBulbTemp) {
                    znAirRpt.OABalanceHeatLoss = thisZoneHB.MDotCPOA * (thisZoneHB.MAT - zone.OutDryBulbTemp) * TimeStepSysSec * ADSCorrectionFactor;
                    znAirRpt.OABalanceHeatGain = 0.0;
                } else {
                    znAirRpt.OABalanceHeatLoss = 0.0;
                    znAirRpt.OABalanceHeatGain = -thisZoneHB.MDotCPOA * (thisZoneHB.MAT - zone.OutDryBulbTemp) * TimeStepSysSec * ADSCorrectionFactor;
                }
                H2OHtOfVap = Psychrometrics::PsyHgAirFnWTdb(state.dataEnvrn->OutHumRat, zone.OutDryBulbTemp);
                if (thisZoneHB.airHumRat > state.dataEnvrn->OutHumRat) {
                    znAirRpt.OABalanceLatentLoss =
                        thisZoneHB.MDotOA * (thisZoneHB.airHumRat - state.dataEnvrn->OutHumRat) * H2OHtOfVap * TimeStepSysSec * ADSCorrectionFactor;
                    znAirRpt.OABalanceLatentGain = 0.0;
                } else {
                    znAirRpt.OABalanceLatentGain =
                        thisZoneHB.MDotOA * (state.dataEnvrn->OutHumRat - thisZoneHB.airHumRat) * H2OHtOfVap * TimeStepSysSec * ADSCorrectionFactor;
                    znAirRpt.OABalanceLatentLoss = 0.0;
                }
                // Total ventilation losses and gains
                TotalLoad = znAirRpt.OABalanceHeatGain + znAirRpt.OABalanceLatentGain - znAirRpt.OABalanceHeatLoss - znAirRpt.OABalanceLatentLoss;
                if (TotalLoad > 0) {
                    znAirRpt.OABalanceTotalGain = TotalLoad * ADSCorrectionFactor;
                    znAirRpt.OABalanceTotalLoss = 0.0;
                } else {
                    znAirRpt.OABalanceTotalGain = 0.0;
                    znAirRpt.OABalanceTotalLoss = -TotalLoad * ADSCorrectionFactor;
                }
                znAirRpt.OABalanceMass = (thisZoneHB.MDotOA) * TimeStepSysSec * ADSCorrectionFactor;
                znAirRpt.OABalanceMdot = (thisZoneHB.MDotOA) * ADSCorrectionFactor;
                AirDensity =
                    Psychrometrics::PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, thisZoneHB.MAT, thisZoneHB.airHumRatAvg, std::string());
                znAirRpt.OABalanceVolumeCurDensity = (thisZoneHB.MDotOA / AirDensity) * TimeStepSysSec * ADSCorrectionFactor;
                znAirRpt.OABalanceAirChangeRate = znAirRpt.OABalanceVolumeCurDensity / (TimeStepSys * zone.Volume);
                znAirRpt.OABalanceVdotCurDensity = (thisZoneHB.MDotOA / AirDensity) * ADSCorrectionFactor;
                AirDensity = state.dataEnvrn->StdRhoAir;
                znAirRpt.OABalanceVolumeStdDensity = (thisZoneHB.MDotOA / AirDensity) * TimeStepSysSec * ADSCorrectionFactor;
                znAirRpt.OABalanceVdotStdDensity = (thisZoneHB.MDotOA / AirDensity) * ADSCorrectionFactor;
                znAirRpt.OABalanceFanElec = znAirRpt.VentilFanElec;
            }
        }
        // Reports exfiltration loss
        H2OHtOfVap = Psychrometrics::PsyHgAirFnWTdb(state.dataEnvrn->OutHumRat, zone.OutDryBulbTemp);
        znAirRpt.SysInletMass = 0;
        znAirRpt.SysOutletMass = 0;
        if (!zoneEquipConfig.IsControlled) {
            for (int k = 1; k <= zoneEquipConfig.NumInletNodes; ++k) {
                znAirRpt.SysInletMass += state.dataLoopNodes->Node(zoneEquipConfig.InletNode(k)).MassFlowRate * TimeStepSysSec * ADSCorrectionFactor;
            }
            for (int k = 1; k <= zoneEquipConfig.NumExhaustNodes; ++k) {
                znAirRpt.SysOutletMass +=
                    state.dataLoopNodes->Node(zoneEquipConfig.ExhaustNode(k)).MassFlowRate * TimeStepSysSec * ADSCorrectionFactor;
            }
            for (int k = 1; k <= zoneEquipConfig.NumReturnNodes; ++k) {
                znAirRpt.SysOutletMass +=
                    state.dataLoopNodes->Node(zoneEquipConfig.ReturnNode(k)).MassFlowRate * TimeStepSysSec * ADSCorrectionFactor;
            }
        }

        znAirRpt.ExfilMass = znAirRpt.InfilMass + znAirRpt.VentilMass + znAirRpt.MixMass + znAirRpt.OABalanceMass + znAirRpt.SysInletMass -
                             znAirRpt.SysOutletMass; // kg
        // I am not happy with these un-parenthesized divisions and multiplications.  Someone clean this up.
        znAirRpt.ExfilSensiLoss = znAirRpt.ExfilMass / TimeStepSysSec * (thisZoneHB.MAT - zone.OutDryBulbTemp) * CpAir; // W
        znAirRpt.ExfilLatentLoss = znAirRpt.ExfilMass / TimeStepSysSec * (thisZoneHB.airHumRat - state.dataEnvrn->OutHumRat) * H2OHtOfVap;
        znAirRpt.ExfilTotalLoss = znAirRpt.ExfilLatentLoss + znAirRpt.ExfilSensiLoss;

        state.dataHeatBal->ZoneTotalExfiltrationHeatLoss += znAirRpt.ExfilTotalLoss * TimeStepSysSec;
        state.dataHeatBal->ZoneTotalExhaustHeatLoss += znAirRpt.ExhTotalLoss * TimeStepSysSec;
    }
}

void SetHeatToReturnAirFlag(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   February 2008

    // PURPOSE OF THIS SUBROUTINE:
    // This sets some flags at the air loop and zone level: these flags indicate
    // whether an air loop represents a "unitary" system, and whether the system is operating
    // in a on/off (cycling fan) mode. At the zone level flags are set to indicate whether
    // the zone is served by a zonal system only, and whether the air loop serving the zone (idf any)
    // is in cycling fan mode. Using this information, the subroutine sets a flag at the zone level
    // to tell ManageZoneAirUpdates (predict and correct) what to do with the heat to return air.

    // METHODOLOGY EMPLOYED:
    // Uses program data structures AirLoopControlInfo and ZoneEquipInfo

    if (!state.dataHVACGlobal->AirLoopsSimOnce) return;

    int NumPrimaryAirSys = state.dataHVACGlobal->NumPrimaryAirSys;

    if (state.dataHVACMgr->MyOneTimeFlag) {
        // set the air loop Any Continuous Fan flag
        for (int AirLoopNum = 1; AirLoopNum <= NumPrimaryAirSys; ++AirLoopNum) {
            auto &airLoopControlInfo = state.dataAirLoop->AirLoopControlInfo(AirLoopNum);

            if (airLoopControlInfo.UnitarySys) { // for unitary systems check the cycling fan schedule
                if (airLoopControlInfo.CycFanSchedPtr > 0) {
                    Real64 CycFanMaxVal = ScheduleManager::GetScheduleMaxValue(state, airLoopControlInfo.CycFanSchedPtr);
                    if (CycFanMaxVal > 0.0) {
                        airLoopControlInfo.AnyContFan = true;
                    } else {
                        airLoopControlInfo.AnyContFan = false;
                    }
                } else { // no schedule means always cycling fan
                    airLoopControlInfo.AnyContFan = false;
                }
            } else { // for nonunitary (central) all systems are continuous fan
                airLoopControlInfo.AnyContFan = true;
            }
        }
        // check to see if a controlled zone is served exclusively by a zonal system
        for (int ControlledZoneNum = 1; ControlledZoneNum <= state.dataGlobal->NumOfZones; ++ControlledZoneNum) {
            auto &zoneEquipConfig = state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum);
            bool airLoopFound = false;
            for (int zoneInNode = 1; zoneInNode <= zoneEquipConfig.NumInletNodes; ++zoneInNode) {
                if (zoneEquipConfig.InletNodeAirLoopNum(zoneInNode) > 0) {
                    airLoopFound = true;
                }
            }
            if (!airLoopFound && zoneEquipConfig.NumInletNodes == zoneEquipConfig.NumExhaustNodes) {
                zoneEquipConfig.ZonalSystemOnly = true;
            }
        }
        // issue warning messages if zone is served by a zonal system or a cycling system and the input calls for
        // heat gain to return air
        for (int ControlledZoneNum = 1; ControlledZoneNum <= state.dataGlobal->NumOfZones; ++ControlledZoneNum) {
            auto &zoneEquipConfig = state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum);
            if (!zoneEquipConfig.IsControlled) continue;
            bool CyclingFan = false; // TRUE means air loop operates in cycling fan mode at some point
            for (int zoneInNode = 1; zoneInNode <= zoneEquipConfig.NumInletNodes; ++zoneInNode) {
                int AirLoopNum = zoneEquipConfig.InletNodeAirLoopNum(zoneInNode);
                if (AirLoopNum > 0) {
                    if (state.dataAirLoop->AirLoopControlInfo(AirLoopNum).CycFanSchedPtr > 0) {
                        CyclingFan =
                            ScheduleManager::CheckScheduleValue(state, state.dataAirLoop->AirLoopControlInfo(AirLoopNum).CycFanSchedPtr, 0.0);
                    }
                }
            }
            if (zoneEquipConfig.ZonalSystemOnly || CyclingFan) {
                auto const &thisZone = state.dataHeatBal->Zone(ControlledZoneNum);
                if (thisZone.RefrigCaseRA) {
                    ShowWarningError(state,
                                     format("For zone={} return air cooling by refrigerated cases will be applied to the zone air.", thisZone.Name));
                    ShowContinueError(state, "  This zone has no return air or is served by an on/off HVAC system.");
                }
                for (int LightNum = 1; LightNum <= state.dataHeatBal->TotLights; ++LightNum) {
                    if (state.dataHeatBal->Lights(LightNum).ZonePtr != ControlledZoneNum) continue;
                    if (state.dataHeatBal->Lights(LightNum).FractionReturnAir > 0.0) {
                        ShowWarningError(state,
                                         format("For zone={} return air heat gain from lights will be applied to the zone air.", thisZone.Name));
                        ShowContinueError(state, "  This zone has no return air or is served by an on/off HVAC system.");
                        break;
                    }
                }
                for (int spaceNum : thisZone.spaceIndexes) {
                    auto const &thisSpace = state.dataHeatBal->space(spaceNum);
                    for (int SurfNum = thisSpace.HTSurfaceFirst; SurfNum <= thisSpace.HTSurfaceLast; ++SurfNum) {
                        if (state.dataSurface->SurfWinAirflowDestination(SurfNum) == DataSurfaces::WindowAirFlowDestination::Return) {
                            ShowWarningError(
                                state,
                                format("For zone={} return air heat gain from air flow windows will be applied to the zone air.", thisZone.Name));
                            ShowContinueError(state, "  This zone has no return air or is served by an on/off HVAC system.");
                        }
                    }
                }
            }
        }
        state.dataHVACMgr->MyOneTimeFlag = false;
    }

    // set the air loop fan operation mode
    for (int AirLoopNum = 1; AirLoopNum <= NumPrimaryAirSys; ++AirLoopNum) {
        auto &airLoopControlInfo = state.dataAirLoop->AirLoopControlInfo(AirLoopNum);
        if (airLoopControlInfo.CycFanSchedPtr > 0) {
            if (ScheduleManager::GetCurrentScheduleValue(state, airLoopControlInfo.CycFanSchedPtr) == 0.0) {
                airLoopControlInfo.FanOpMode = DataHVACGlobals::CycFanCycCoil;
            } else {
                airLoopControlInfo.FanOpMode = DataHVACGlobals::ContFanCycCoil;
            }
        }
    }
    // set the zone level NoHeatToReturnAir flag
    // if any air loop in the zone is continuous fan, then set NoHeatToReturnAir = false and sort it out node-by-node
    for (int ControlledZoneNum = 1; ControlledZoneNum <= state.dataGlobal->NumOfZones; ++ControlledZoneNum) {
        auto &zoneEquipConfig = state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum);
        auto &thisZone = state.dataHeatBal->Zone(ControlledZoneNum);
        if (!zoneEquipConfig.IsControlled) continue;
        thisZone.NoHeatToReturnAir = true;
        if (!zoneEquipConfig.ZonalSystemOnly) {
            for (int zoneInNode = 1; zoneInNode <= zoneEquipConfig.NumInletNodes; ++zoneInNode) {
                int AirLoopNum = zoneEquipConfig.InletNodeAirLoopNum(zoneInNode);
                if (AirLoopNum > 0) {
                    if (state.dataAirLoop->AirLoopControlInfo(AirLoopNum).FanOpMode == DataHVACGlobals::ContFanCycCoil) {
                        thisZone.NoHeatToReturnAir = false;
                        break;
                    }
                }
            }
        }
    }
}

void UpdateZoneInletConvergenceLog(EnergyPlusData &state)
{

    std::array<Real64, DataConvergParams::ConvergLogStackDepth> tmpRealARR;

    for (int ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {

        for (int NodeIndex = 1; NodeIndex <= state.dataConvergeParams->ZoneInletConvergence(ZoneNum).NumInletNodes; ++NodeIndex) {
            int NodeNum = state.dataConvergeParams->ZoneInletConvergence(ZoneNum).InletNode(NodeIndex).NodeNum;

            tmpRealARR = state.dataConvergeParams->ZoneInletConvergence(ZoneNum).InletNode(NodeIndex).HumidityRatio;
            state.dataConvergeParams->ZoneInletConvergence(ZoneNum).InletNode(NodeIndex).HumidityRatio[0] = state.dataLoopNodes->Node(NodeNum).HumRat;
            for (int logIndex = 1; logIndex < DataConvergParams::ConvergLogStackDepth; logIndex++) {
                state.dataConvergeParams->ZoneInletConvergence(ZoneNum).InletNode(NodeIndex).HumidityRatio[logIndex] = tmpRealARR[logIndex - 1];
            }

            tmpRealARR = state.dataConvergeParams->ZoneInletConvergence(ZoneNum).InletNode(NodeIndex).MassFlowRate;
            state.dataConvergeParams->ZoneInletConvergence(ZoneNum).InletNode(NodeIndex).MassFlowRate[0] =
                state.dataLoopNodes->Node(NodeNum).MassFlowRate;
            for (int logIndex = 1; logIndex < DataConvergParams::ConvergLogStackDepth; logIndex++) {
                state.dataConvergeParams->ZoneInletConvergence(ZoneNum).InletNode(NodeIndex).MassFlowRate[logIndex] = tmpRealARR[logIndex - 1];
            }

            tmpRealARR = state.dataConvergeParams->ZoneInletConvergence(ZoneNum).InletNode(NodeIndex).Temperature;
            state.dataConvergeParams->ZoneInletConvergence(ZoneNum).InletNode(NodeIndex).Temperature[0] = state.dataLoopNodes->Node(NodeNum).Temp;
            for (int logIndex = 1; logIndex < DataConvergParams::ConvergLogStackDepth; logIndex++) {
                state.dataConvergeParams->ZoneInletConvergence(ZoneNum).InletNode(NodeIndex).Temperature[logIndex] = tmpRealARR[logIndex - 1];
            }
        }
    }
}

void CheckAirLoopFlowBalance(EnergyPlusData &state)
{
    // Check for unbalanced airloop
    if (!state.dataGlobal->WarmupFlag && state.dataHVACGlobal->AirLoopsSimOnce) {
        for (int AirLoopNum = 1; AirLoopNum <= state.dataHVACGlobal->NumPrimaryAirSys; ++AirLoopNum) {
            auto &thisAirLoopFlow = state.dataAirLoop->AirLoopFlow(AirLoopNum);
            if (!thisAirLoopFlow.FlowError) {
                Real64 unbalancedExhaustDelta = thisAirLoopFlow.SupFlow - thisAirLoopFlow.OAFlow - thisAirLoopFlow.SysRetFlow;
                if (unbalancedExhaustDelta > DataHVACGlobals::SmallMassFlow) {
                    ShowSevereError(state,
                                    format("CheckAirLoopFlowBalance: AirLoopHVAC {} is unbalanced. Supply is > return plus outdoor air.",
                                           state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).Name));
                    ShowContinueErrorTimeStamp(state, "");
                    ShowContinueError(state,
                                      format("  Flows [m3/s at standard density]: Supply={:.6R}  Return={:.6R}  Outdoor Air={:.6R}",
                                             thisAirLoopFlow.SupFlow / state.dataEnvrn->StdRhoAir,
                                             thisAirLoopFlow.SysRetFlow / state.dataEnvrn->StdRhoAir,
                                             thisAirLoopFlow.OAFlow / state.dataEnvrn->StdRhoAir));
                    ShowContinueError(state, format("  Imbalance={:.6R}", unbalancedExhaustDelta / state.dataEnvrn->StdRhoAir));
                    ShowContinueError(state, "  This error will only be reported once per system.");
                    thisAirLoopFlow.FlowError = true;
                }
            }
        }
    }
}

} // namespace EnergyPlus::HVACManager
