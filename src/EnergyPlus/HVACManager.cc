// EnergyPlus, Copyright (c) 1996-2022, The Board of Trustees of the University of Illinois,
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

using namespace DataEnvironment;
using namespace DataHVACGlobals;
using namespace DataLoopNode;
using namespace DataAirLoop;

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

    // Using/Aliasing
    using DemandManager::ManageDemand;
    using DemandManager::UpdateDemandManagers;
    using EMSManager::ManageEMS;
    using InternalHeatGains::UpdateInternalGainValues;
    using NodeInputManager::CalcMoreNodeInfo;
    using OutAirNodeManager::SetOutAirNodes;
    using OutputReportTabular::GatherComponentLoadsHVAC;
    using OutputReportTabular::UpdateTabularReports; // added for writing tabular output reports
    using PollutionModule::CalculatePollution;
    using RefrigeratedCase::ManageRefrigeratedCaseRacks;
    using ScheduleManager::GetCurrentScheduleValue;
    using SizingManager::UpdateFacilitySizing;
    using SystemAvailabilityManager::ManageHybridVentilation;
    using SystemReports::InitEnergyReports;
    using SystemReports::ReportMaxVentilationLoads;
    using SystemReports::ReportSystemEnergyUse;
    using WaterManager::ManageWater;
    using WaterManager::ManageWaterInits;
    using ZoneContaminantPredictorCorrector::ManageZoneContaminanUpdates;
    using ZoneEquipmentManager::CalcAirFlowSimple;
    using ZoneEquipmentManager::UpdateZoneSizing;
    using ZoneTempPredictorCorrector::DetectOscillatingZoneTemp;
    using ZoneTempPredictorCorrector::ManageZoneAirUpdates;

    // SUBROUTINE PARAMETER DEFINITIONS:
    static constexpr std::string_view EndOfHeaderString("End of Data Dictionary");                          // End of data dictionary marker
    static constexpr std::string_view EnvironmentStampFormatStr("{},{},{:7.2F},{:7.2F},{:7.2F},{:7.2F}\n"); // Format descriptor for environ stamp

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 PriorTimeStep;       // magnitude of time step for previous history terms
    Real64 ZoneTempChange(0.0); // change in zone air temperature from timestep t-1 to t
    int NodeNum;
    bool ReportDebug;
    int ZoneNum;

    bool DummyLogical;

    auto &AirLoopsSimOnce = state.dataHVACGlobal->AirLoopsSimOnce;
    auto &NumOfSysTimeStepsLastZoneTimeStep = state.dataHVACGlobal->NumOfSysTimeStepsLastZoneTimeStep;
    auto &SysTimeElapsed = state.dataHVACGlobal->SysTimeElapsed;
    auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;
    auto &FirstTimeStepSysFlag = state.dataHVACGlobal->FirstTimeStepSysFlag;
    auto &ShortenTimeStepSys = state.dataHVACGlobal->ShortenTimeStepSys;
    auto &UseZoneTimeStepHistory = state.dataHVACGlobal->UseZoneTimeStepHistory;
    auto &NumOfSysTimeSteps = state.dataHVACGlobal->NumOfSysTimeSteps;
    auto &FracTimeStepZone = state.dataHVACGlobal->FracTimeStepZone;
    auto &LimitNumSysSteps = state.dataHVACGlobal->LimitNumSysSteps;

    // SYSTEM INITIALIZATION
    if (state.dataHVACMgr->TriggerGetAFN) {
        state.dataHVACMgr->TriggerGetAFN = false;
        DisplayString(state, "Initializing HVAC");
        state.afn->manage_balance(); // first call only gets input and returns.
    }

    state.dataHeatBalFanSys->ZT = state.dataHeatBalFanSys->MAT;
    // save for use with thermal comfort control models (Fang, Pierce, and KSU)
    state.dataHeatBalFanSys->ZTAV = 0.0;
    state.dataHeatBalFanSys->ZoneThermostatSetPointHiAver = 0.0;
    state.dataHeatBalFanSys->ZoneThermostatSetPointLoAver = 0.0;
    state.dataHeatBalFanSys->ZoneAirHumRatAvg = 0.0;
    state.dataHVACMgr->PrintedWarmup = false;
    if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
        state.dataContaminantBalance->OutdoorCO2 = GetCurrentScheduleValue(state, state.dataContaminantBalance->Contaminant.CO2OutdoorSchedPtr);
        state.dataContaminantBalance->ZoneAirCO2Avg = 0.0;
    }
    if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
        state.dataContaminantBalance->OutdoorGC =
            GetCurrentScheduleValue(state, state.dataContaminantBalance->Contaminant.GenericContamOutdoorSchedPtr);
        if (allocated(state.dataContaminantBalance->ZoneAirGCAvg)) state.dataContaminantBalance->ZoneAirGCAvg = 0.0;
    }

    if (state.dataGlobal->BeginEnvrnFlag && state.dataHVACMgr->MyEnvrnFlag) {
        AirLoopsSimOnce = false;
        state.dataHVACMgr->MyEnvrnFlag = false;
        NumOfSysTimeStepsLastZoneTimeStep = 1;
        state.dataHVACGlobal->PreviousTimeStep = state.dataGlobal->TimeStepZone;
    }
    if (!state.dataGlobal->BeginEnvrnFlag) {
        state.dataHVACMgr->MyEnvrnFlag = true;
    }

    state.dataHeatBalFanSys->QRadSurfAFNDuct = 0.0;
    SysTimeElapsed = 0.0;
    TimeStepSys = state.dataGlobal->TimeStepZone;
    FirstTimeStepSysFlag = true;
    ShortenTimeStepSys = false;
    UseZoneTimeStepHistory = true;
    PriorTimeStep = state.dataGlobal->TimeStepZone;
    NumOfSysTimeSteps = 1;
    FracTimeStepZone = TimeStepSys / state.dataGlobal->TimeStepZone;

    bool anyEMSRan;
    ManageEMS(state, EMSManager::EMSCallFrom::BeginTimestepBeforePredictor, anyEMSRan, ObjexxFCL::Optional_int_const()); // calling point

    SetOutAirNodes(state);

    ManageRefrigeratedCaseRacks(state);

    // ZONE INITIALIZATION  'Get Zone Setpoints'
    ManageZoneAirUpdates(state,
                         DataHeatBalFanSys::PredictorCorrectorCtrl::GetZoneSetPoints,
                         ZoneTempChange,
                         ShortenTimeStepSys,
                         UseZoneTimeStepHistory,
                         PriorTimeStep);
    if (state.dataContaminantBalance->Contaminant.SimulateContaminants)
        ManageZoneContaminanUpdates(
            state, DataHeatBalFanSys::PredictorCorrectorCtrl::GetZoneSetPoints, ShortenTimeStepSys, UseZoneTimeStepHistory, PriorTimeStep);

    ManageHybridVentilation(state);

    CalcAirFlowSimple(state);
    if (state.afn->SimulateAirflowNetwork > AirflowNetwork::AirflowNetworkControlSimple) {
        state.afn->RollBackFlag = false;
        state.afn->manage_balance(false);
    }

    SetHeatToReturnAirFlag(state);

    state.dataHeatBalFanSys->SysDepZoneLoadsLagged = state.dataHeatBalFanSys->SysDepZoneLoads;

    UpdateInternalGainValues(state, true, true);

    ManageZoneAirUpdates(
        state, DataHeatBalFanSys::PredictorCorrectorCtrl::PredictStep, ZoneTempChange, ShortenTimeStepSys, UseZoneTimeStepHistory, PriorTimeStep);

    if (state.dataContaminantBalance->Contaminant.SimulateContaminants)
        ManageZoneContaminanUpdates(
            state, DataHeatBalFanSys::PredictorCorrectorCtrl::PredictStep, ShortenTimeStepSys, UseZoneTimeStepHistory, PriorTimeStep);

    SimHVAC(state);

    if (state.dataGlobal->AnyIdealCondEntSetPointInModel && state.dataGlobal->MetersHaveBeenInitialized && !state.dataGlobal->WarmupFlag) {
        state.dataGlobal->RunOptCondEntTemp = true;
        while (state.dataGlobal->RunOptCondEntTemp) {
            SimHVAC(state);
        }
    }

    ManageWaterInits(state);

    // Only simulate once per zone timestep; must be after SimHVAC
    if (FirstTimeStepSysFlag && state.dataGlobal->MetersHaveBeenInitialized) {
        ManageDemand(state);
    }

    state.dataGlobal->BeginTimeStepFlag = false; // At this point, we have been through the first pass through SimHVAC so this needs to be set

    ManageZoneAirUpdates(
        state, DataHeatBalFanSys::PredictorCorrectorCtrl::CorrectStep, ZoneTempChange, ShortenTimeStepSys, UseZoneTimeStepHistory, PriorTimeStep);
    if (state.dataContaminantBalance->Contaminant.SimulateContaminants)
        ManageZoneContaminanUpdates(
            state, DataHeatBalFanSys::PredictorCorrectorCtrl::CorrectStep, ShortenTimeStepSys, UseZoneTimeStepHistory, PriorTimeStep);

    if (ZoneTempChange > state.dataConvergeParams->MaxZoneTempDiff && !state.dataGlobal->KickOffSimulation) {
        // determine value of adaptive system time step
        // model how many system timesteps we want in zone timestep
        int ZTempTrendsNumSysSteps = int(ZoneTempChange / state.dataConvergeParams->MaxZoneTempDiff + 1.0); // add 1 for truncation
        NumOfSysTimeSteps = min(ZTempTrendsNumSysSteps, LimitNumSysSteps);
        // then determine timestep length for even distribution, protect div by zero
        if (NumOfSysTimeSteps > 0) TimeStepSys = state.dataGlobal->TimeStepZone / NumOfSysTimeSteps;
        TimeStepSys = max(TimeStepSys, state.dataConvergeParams->MinTimeStepSys);
        UseZoneTimeStepHistory = false;
        ShortenTimeStepSys = true;
    } else {
        NumOfSysTimeSteps = 1;
        UseZoneTimeStepHistory = true;
    }

    if (UseZoneTimeStepHistory) state.dataHVACGlobal->PreviousTimeStep = state.dataGlobal->TimeStepZone;
    for (int SysTimestepLoop = 1; SysTimestepLoop <= NumOfSysTimeSteps; ++SysTimestepLoop) {
        if (state.dataGlobal->stopSimulation) break;

        if (TimeStepSys < state.dataGlobal->TimeStepZone) {

            ManageHybridVentilation(state);
            CalcAirFlowSimple(state, SysTimestepLoop);
            if (state.afn->SimulateAirflowNetwork > AirflowNetwork::AirflowNetworkControlSimple) {
                state.afn->RollBackFlag = false;
                state.afn->manage_balance(false);
            }

            UpdateInternalGainValues(state, true, true);

            ManageZoneAirUpdates(state,
                                 DataHeatBalFanSys::PredictorCorrectorCtrl::PredictStep,
                                 ZoneTempChange,
                                 ShortenTimeStepSys,
                                 UseZoneTimeStepHistory,
                                 PriorTimeStep);

            if (state.dataContaminantBalance->Contaminant.SimulateContaminants)
                ManageZoneContaminanUpdates(
                    state, DataHeatBalFanSys::PredictorCorrectorCtrl::PredictStep, ShortenTimeStepSys, UseZoneTimeStepHistory, PriorTimeStep);
            SimHVAC(state);

            if (state.dataGlobal->AnyIdealCondEntSetPointInModel && state.dataGlobal->MetersHaveBeenInitialized && !state.dataGlobal->WarmupFlag) {
                state.dataGlobal->RunOptCondEntTemp = true;
                while (state.dataGlobal->RunOptCondEntTemp) {
                    SimHVAC(state);
                }
            }

            ManageWaterInits(state);

            // Need to set the flag back since we do not need to shift the temps back again in the correct step.
            ShortenTimeStepSys = false;

            ManageZoneAirUpdates(state,
                                 DataHeatBalFanSys::PredictorCorrectorCtrl::CorrectStep,
                                 ZoneTempChange,
                                 ShortenTimeStepSys,
                                 UseZoneTimeStepHistory,
                                 PriorTimeStep);
            if (state.dataContaminantBalance->Contaminant.SimulateContaminants)
                ManageZoneContaminanUpdates(
                    state, DataHeatBalFanSys::PredictorCorrectorCtrl::CorrectStep, ShortenTimeStepSys, UseZoneTimeStepHistory, PriorTimeStep);

            ManageZoneAirUpdates(state,
                                 DataHeatBalFanSys::PredictorCorrectorCtrl::PushSystemTimestepHistories,
                                 ZoneTempChange,
                                 ShortenTimeStepSys,
                                 UseZoneTimeStepHistory,
                                 PriorTimeStep);
            if (state.dataContaminantBalance->Contaminant.SimulateContaminants)
                ManageZoneContaminanUpdates(state,
                                            DataHeatBalFanSys::PredictorCorrectorCtrl::PushSystemTimestepHistories,
                                            ShortenTimeStepSys,
                                            UseZoneTimeStepHistory,
                                            PriorTimeStep);
            state.dataHVACGlobal->PreviousTimeStep = TimeStepSys;
        }

        FracTimeStepZone = TimeStepSys / state.dataGlobal->TimeStepZone;

        for (ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
            state.dataHeatBalFanSys->ZTAV(ZoneNum) += state.dataHeatBalFanSys->ZT(ZoneNum) * FracTimeStepZone;
            state.dataHeatBalFanSys->ZoneAirHumRatAvg(ZoneNum) += state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum) * FracTimeStepZone;
            if (state.dataContaminantBalance->Contaminant.CO2Simulation)
                state.dataContaminantBalance->ZoneAirCO2Avg(ZoneNum) += state.dataContaminantBalance->ZoneAirCO2(ZoneNum) * FracTimeStepZone;
            if (state.dataContaminantBalance->Contaminant.GenericContamSimulation)
                state.dataContaminantBalance->ZoneAirGCAvg(ZoneNum) += state.dataContaminantBalance->ZoneAirGC(ZoneNum) * FracTimeStepZone;
            if (state.dataZoneTempPredictorCorrector->NumOnOffCtrZone > 0) {
                state.dataHeatBalFanSys->ZoneThermostatSetPointHiAver(ZoneNum) +=
                    state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ZoneNum) * FracTimeStepZone;
                state.dataHeatBalFanSys->ZoneThermostatSetPointLoAver(ZoneNum) +=
                    state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ZoneNum) * FracTimeStepZone;
            }
        }

        DetectOscillatingZoneTemp(state);
        UpdateZoneListAndGroupLoads(state);           // Must be called before UpdateDataandReport(OutputProcessor::TimeStepType::TimeStepSystem)
        IceThermalStorage::UpdateIceFractions(state); // Update fraction of ice stored in TES
        ManageWater(state);
        // update electricity data for net, purchased, sold etc.
        DummyLogical = false;
        state.dataElectPwrSvcMgr->facilityElectricServiceObj->manageElectricPowerService(state, false, DummyLogical, true);

        // Update the plant and condenser loop capacitance model temperature history.
        PlantManager::UpdateNodeThermalHistory(state);

        if (state.dataOutRptTab->displayHeatEmissionsSummary) {
            OutputReportTabular::CalcHeatEmissionReport(state);
        }

        ManageEMS(
            state, EMSManager::EMSCallFrom::EndSystemTimestepBeforeHVACReporting, anyEMSRan, ObjexxFCL::Optional_int_const()); // EMS calling point

        // This is where output processor data is updated for System Timestep reporting
        if (!state.dataGlobal->WarmupFlag) {
            if (state.dataGlobal->DoOutputReporting) {
                CalcMoreNodeInfo(state);
                CalculatePollution(state);
                InitEnergyReports(state);
                ReportSystemEnergyUse(state);
            }
            if (state.dataGlobal->DoOutputReporting || (state.dataGlobal->ZoneSizingCalc && state.dataGlobal->CompLoadReportIsReq)) {
                ReportAirHeatBalance(state);
                if (state.dataGlobal->ZoneSizingCalc) GatherComponentLoadsHVAC(state);
            }
            if (state.dataGlobal->DoOutputReporting) {
                ReportMaxVentilationLoads(state);
                UpdateDataandReport(state, OutputProcessor::TimeStepType::System);
                if (state.dataGlobal->KindOfSim == DataGlobalConstants::KindOfSim::HVACSizeDesignDay ||
                    state.dataGlobal->KindOfSim == DataGlobalConstants::KindOfSim::HVACSizeRunPeriodDesign) {
                    if (state.dataHVACSizingSimMgr->hvacSizingSimulationManager)
                        state.dataHVACSizingSimMgr->hvacSizingSimulationManager->UpdateSizingLogsSystemStep(state);
                }
                UpdateTabularReports(state, OutputProcessor::TimeStepType::System);
            }
            if (state.dataGlobal->ZoneSizingCalc) {
                UpdateZoneSizing(state, DataGlobalConstants::CallIndicator::DuringDay);
                UpdateFacilitySizing(state, DataGlobalConstants::CallIndicator::DuringDay);
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
            CalcMoreNodeInfo(state);
            UpdateDataandReport(state, OutputProcessor::TimeStepType::System);
            if (state.dataGlobal->KindOfSim == DataGlobalConstants::KindOfSim::HVACSizeDesignDay ||
                state.dataGlobal->KindOfSim == DataGlobalConstants::KindOfSim::HVACSizeRunPeriodDesign) {
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
        ManageEMS(
            state, EMSManager::EMSCallFrom::EndSystemTimestepAfterHVACReporting, anyEMSRan, ObjexxFCL::Optional_int_const()); // EMS calling point
        // UPDATE SYSTEM CLOCKS
        SysTimeElapsed += TimeStepSys;

        FirstTimeStepSysFlag = false;
    } // system time step  loop (loops once if no downstepping)

    state.dataHeatBalFanSys->ZTAVComf = state.dataHeatBalFanSys->ZTAV;
    state.dataHeatBalFanSys->ZoneAirHumRatAvgComf = state.dataHeatBalFanSys->ZoneAirHumRatAvg;

    ManageZoneAirUpdates(state,
                         DataHeatBalFanSys::PredictorCorrectorCtrl::PushZoneTimestepHistories,
                         ZoneTempChange,
                         ShortenTimeStepSys,
                         UseZoneTimeStepHistory,
                         PriorTimeStep);
    if (state.dataContaminantBalance->Contaminant.SimulateContaminants)
        ManageZoneContaminanUpdates(
            state, DataHeatBalFanSys::PredictorCorrectorCtrl::PushZoneTimestepHistories, ShortenTimeStepSys, UseZoneTimeStepHistory, PriorTimeStep);

    NumOfSysTimeStepsLastZoneTimeStep = NumOfSysTimeSteps;

    UpdateDemandManagers(state);

    // DO FINAL UPDATE OF RECORD KEEPING VARIABLES
    // Report the Node Data to Aid in Debugging
    if (state.dataReportFlag->DebugOutput) {
        if (state.dataReportFlag->EvenDuringWarmup) {
            ReportDebug = true;
        } else {
            ReportDebug = !state.dataGlobal->WarmupFlag;
        }
        if ((ReportDebug) && (state.dataGlobal->DayOfSim > 0)) { // Report the node data
            if (size(state.dataLoopNodes->Node) > 0 && !state.dataHVACMgr->DebugNamesReported) {
                print(state.files.debug, "{}\n", "node #   Name");
                for (NodeNum = 1; NodeNum <= isize(state.dataLoopNodes->Node); ++NodeNum) {
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
            for (NodeNum = 1; NodeNum <= isize(state.dataLoopNodes->Node); ++NodeNum) {
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

    // Using/Aliasing
    using DataPlant::NumConvergenceHistoryTerms;
    using EMSManager::ManageEMS;
    using General::CreateSysTimeIntervalString;
    using NonZoneEquipmentManager::ManageNonZoneEquipment;
    using PlantCondLoopOperation::SetupPlantEMSActuators;
    using PlantManager::GetPlantInput;
    using PlantManager::GetPlantLoopData;
    using PlantManager::InitOneTimePlantSizingInfo;
    using PlantManager::ReInitPlantLoopsAtFirstHVACIteration;
    using PlantManager::SetupBranchControlTypes;
    using PlantManager::SetupInitialPlantCallingOrder;
    using PlantManager::SetupReports;
    using PlantUtilities::AnyPlantSplitterMixerLacksContinuity;
    using PlantUtilities::CheckForRunawayPlantTemps;
    using PlantUtilities::CheckPlantMixerSplitterConsistency;
    using PlantUtilities::SetAllPlantSimFlagsToValue;
    using SetPointManager::ManageSetPoints;
    using SystemAvailabilityManager::ManageSystemAvailability;
    using ZoneEquipmentManager::ManageZoneEquipment;

    // SUBROUTINE PARAMETER DEFINITIONS:
    bool constexpr SimWithPlantFlowUnlocked(false);
    bool constexpr SimWithPlantFlowLocked(true);

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    bool FirstHVACIteration; // True when solution technique on first iteration
    auto &ErrCount = state.dataHVACMgr->ErrCount;
    auto &MaxErrCount = state.dataHVACMgr->MaxErrCount;
    auto &ErrEnvironmentName = state.dataHVACMgr->ErrEnvironmentName;
    int LoopNum;

    int AirSysNum;
    int StackDepth;
    std::string HistoryTrace;
    Real64 SlopeHumRat;
    Real64 SlopeMdot;
    Real64 SlopeTemps;
    Real64 AvgValue;
    bool FoundOscillationByDuplicate;
    int ZoneNum;
    int NodeIndex;
    bool MonotonicIncreaseFound;
    bool MonotonicDecreaseFound;

    static constexpr std::array<Real64, DataConvergParams::ConvergLogStackDepth> ConvergLogStackARR = {
        0.0, -1.0, -2.0, -3.0, -4.0, -5.0, -6.0, -7.0, -8.0, -9.0};
    Real64 constexpr sum_ConvergLogStackARR(-45);
    Real64 constexpr square_sum_ConvergLogStackARR(2025);
    Real64 constexpr sum_square_ConvergLogStackARR(285);

    auto &SimZoneEquipmentFlag = state.dataHVACGlobal->SimZoneEquipmentFlag;
    auto &SimNonZoneEquipmentFlag = state.dataHVACGlobal->SimNonZoneEquipmentFlag;
    auto &SimAirLoopsFlag = state.dataHVACGlobal->SimAirLoopsFlag;
    auto &SimPlantLoopsFlag = state.dataHVACGlobal->SimPlantLoopsFlag;
    auto &SimElecCircuitsFlag = state.dataHVACGlobal->SimElecCircuitsFlag;
    auto &NumPrimaryAirSys = state.dataHVACGlobal->NumPrimaryAirSys;
    auto &DoSetPointTest = state.dataHVACGlobal->DoSetPointTest;
    auto &SetPointErrorFlag = state.dataHVACGlobal->SetPointErrorFlag;

    // Initialize all of the simulation flags to true for the first iteration
    SimZoneEquipmentFlag = true;
    SimNonZoneEquipmentFlag = true;
    SimAirLoopsFlag = true;
    SimPlantLoopsFlag = true;
    SimElecCircuitsFlag = true;
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
    SetAllPlantSimFlagsToValue(state, true);
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
        ManageSetPoints(state); // need to call this before getting plant loop data so setpoint checks can complete okay
        GetPlantLoopData(state);
        GetPlantInput(state);
        SetupInitialPlantCallingOrder(state);
        SetupBranchControlTypes(state); // new routine to do away with input for branch control type
        //    CALL CheckPlantLoopData
        SetupReports(state);
        if (state.dataGlobal->AnyEnergyManagementSystemInModel) {
            SetupPlantEMSActuators(state);
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
            for (LoopNum = 1; LoopNum <= state.dataPlnt->TotNumLoops; ++LoopNum) {
                // init plant sizing numbers in main plant data structure
                InitOneTimePlantSizingInfo(state, LoopNum);
            }
        }
        state.dataHVACMgr->SimHVACIterSetup = true;
    }

    if (state.dataGlobal->ZoneSizingCalc) {
        ManageZoneEquipment(state, FirstHVACIteration, SimZoneEquipmentFlag, SimAirLoopsFlag);
        // need to call non zone equipment so water use zone gains can be included in sizing calcs
        ManageNonZoneEquipment(state, FirstHVACIteration, SimNonZoneEquipmentFlag);
        state.dataElectPwrSvcMgr->facilityElectricServiceObj->manageElectricPowerService(state, FirstHVACIteration, SimElecCircuitsFlag, false);
        return;
    }

    // Before the HVAC simulation, reset control flags and specified flow
    // rates that might have been set by the set point and availability
    // managers.

    ResetHVACControl(state);

    // Before the HVAC simulation, call ManageSetPoints to set all the HVAC
    // node setpoints
    bool anyEMSRan = false;
    ManageEMS(state, EMSManager::EMSCallFrom::BeforeHVACManagers, anyEMSRan, ObjexxFCL::Optional_int_const()); // calling point

    ManageSetPoints(state);

    // re-initialize plant loop and nodes.
    ReInitPlantLoopsAtFirstHVACIteration(state);

    // Before the HVAC simulation, call ManageSystemAvailability to set
    // the system on/off flags
    ManageSystemAvailability(state);

    ManageEMS(state, EMSManager::EMSCallFrom::AfterHVACManagers, anyEMSRan, ObjexxFCL::Optional_int_const()); // calling point
    ManageEMS(state, EMSManager::EMSCallFrom::HVACIterationLoop, anyEMSRan, ObjexxFCL::Optional_int_const()); // calling point id

    // first explicitly call each system type with FirstHVACIteration,

    // Manages the various component simulations
    SimSelectedEquipment(state,
                         SimAirLoopsFlag,
                         SimZoneEquipmentFlag,
                         SimNonZoneEquipmentFlag,
                         SimPlantLoopsFlag,
                         SimElecCircuitsFlag,
                         FirstHVACIteration,
                         SimWithPlantFlowUnlocked);

    // Eventually, when all of the flags are set to false, the
    // simulation has converged for this system time step.

    SimPlantLoopsFlag = true;
    SetAllPlantSimFlagsToValue(state, true); // set so loop to simulate at least once on non-first hvac

    FirstHVACIteration = false;

    // then iterate among all systems after first HVAC iteration is over

    // Main iteration loop for HVAC.  If any of the simulation flags are
    // true, then specific components must be resimulated.
    while ((SimAirLoopsFlag || SimZoneEquipmentFlag || SimNonZoneEquipmentFlag || SimPlantLoopsFlag || SimElecCircuitsFlag) &&
           (state.dataHVACMgr->HVACManageIteration <= state.dataConvergeParams->MaxIter)) {

        if (state.dataGlobal->stopSimulation) break;

        ManageEMS(state, EMSManager::EMSCallFrom::HVACIterationLoop, anyEMSRan, ObjexxFCL::Optional_int_const()); // calling point id

        // Manages the various component simulations
        SimSelectedEquipment(state,
                             SimAirLoopsFlag,
                             SimZoneEquipmentFlag,
                             SimNonZoneEquipmentFlag,
                             SimPlantLoopsFlag,
                             SimElecCircuitsFlag,
                             FirstHVACIteration,
                             SimWithPlantFlowUnlocked);

        // Eventually, when all of the flags are set to false, the
        // simulation has converged for this system time step.

        UpdateZoneInletConvergenceLog(state);

        ++state.dataHVACMgr->HVACManageIteration; // Increment the iteration counter

        if (anyEMSRan && state.dataHVACMgr->HVACManageIteration <= 2) {
            // the calling point emsCallFromHVACIterationLoop is only effective for air loops if this while loop runs at least twice
            SimAirLoopsFlag = true;
        }
        if (state.dataHVACMgr->HVACManageIteration < state.dataHVACGlobal->MinAirLoopIterationsAfterFirst) {
            // sequenced zone loads for airloops may require extra iterations depending upon zone equipment order and load distribution type
            SimAirLoopsFlag = true;
            SimZoneEquipmentFlag = true;
        }
    }
    if (state.dataGlobal->AnyPlantInModel) {
        if (AnyPlantSplitterMixerLacksContinuity(state)) {
            // rerun systems in a "Final flow lock/last iteration" mode
            // now call for one second to last plant simulation
            SimAirLoopsFlag = false;
            SimZoneEquipmentFlag = false;
            SimNonZoneEquipmentFlag = false;
            SimPlantLoopsFlag = true;
            SimElecCircuitsFlag = false;
            SimSelectedEquipment(state,
                                 SimAirLoopsFlag,
                                 SimZoneEquipmentFlag,
                                 SimNonZoneEquipmentFlag,
                                 SimPlantLoopsFlag,
                                 SimElecCircuitsFlag,
                                 FirstHVACIteration,
                                 SimWithPlantFlowUnlocked);
            // now call for all non-plant simulation, but with plant flow lock on
            SimAirLoopsFlag = true;
            SimZoneEquipmentFlag = true;
            SimNonZoneEquipmentFlag = true;
            SimPlantLoopsFlag = false;
            SimElecCircuitsFlag = true;
            SimSelectedEquipment(state,
                                 SimAirLoopsFlag,
                                 SimZoneEquipmentFlag,
                                 SimNonZoneEquipmentFlag,
                                 SimPlantLoopsFlag,
                                 SimElecCircuitsFlag,
                                 FirstHVACIteration,
                                 SimWithPlantFlowLocked);
            UpdateZoneInletConvergenceLog(state);
            // now call for a last plant simulation
            SimAirLoopsFlag = false;
            SimZoneEquipmentFlag = false;
            SimNonZoneEquipmentFlag = false;
            SimPlantLoopsFlag = true;
            SimElecCircuitsFlag = false;
            SimSelectedEquipment(state,
                                 SimAirLoopsFlag,
                                 SimZoneEquipmentFlag,
                                 SimNonZoneEquipmentFlag,
                                 SimPlantLoopsFlag,
                                 SimElecCircuitsFlag,
                                 FirstHVACIteration,
                                 SimWithPlantFlowUnlocked);
            // now call for a last all non-plant simulation, but with plant flow lock on
            SimAirLoopsFlag = true;
            SimZoneEquipmentFlag = true;
            SimNonZoneEquipmentFlag = true;
            SimPlantLoopsFlag = false;
            SimElecCircuitsFlag = true;
            SimSelectedEquipment(state,
                                 SimAirLoopsFlag,
                                 SimZoneEquipmentFlag,
                                 SimNonZoneEquipmentFlag,
                                 SimPlantLoopsFlag,
                                 SimElecCircuitsFlag,
                                 FirstHVACIteration,
                                 SimWithPlantFlowLocked);
            UpdateZoneInletConvergenceLog(state);
        }
    }

    // Test plant loop for errors
    for (LoopNum = 1; LoopNum <= state.dataPlnt->TotNumLoops; ++LoopNum) {
        for (DataPlant::LoopSideLocation LoopSide : DataPlant::LoopSideKeys) {
            CheckPlantMixerSplitterConsistency(state, LoopNum, LoopSide, FirstHVACIteration);
            CheckForRunawayPlantTemps(state, LoopNum, LoopSide);
        }
    }

    if ((state.dataHVACMgr->HVACManageIteration > state.dataConvergeParams->MaxIter) && (!state.dataGlobal->WarmupFlag)) {
        ++ErrCount;
        if (ErrCount < 15) {
            ErrEnvironmentName = state.dataEnvrn->EnvironmentName;
            ShowWarningError(state,
                             format("SimHVAC: Maximum iterations ({}) exceeded for all HVAC loops, at {}, {} {}",
                                    state.dataConvergeParams->MaxIter,
                                    state.dataEnvrn->EnvironmentName,
                                    state.dataEnvrn->CurMnDy,
                                    CreateSysTimeIntervalString(state)));
            if (SimAirLoopsFlag) {
                ShowContinueError(state, "The solution for one or more of the Air Loop HVAC systems did not appear to converge");
            }
            if (SimZoneEquipmentFlag) {
                ShowContinueError(state, "The solution for zone HVAC equipment did not appear to converge");
            }
            if (SimNonZoneEquipmentFlag) {
                ShowContinueError(state, "The solution for non-zone equipment did not appear to converge");
            }
            if (SimPlantLoopsFlag) {
                ShowContinueError(state, "The solution for one or more plant systems did not appear to converge");
            }
            if (SimElecCircuitsFlag) {
                ShowContinueError(state, "The solution for on-site electric generators did not appear to converge");
            }
            if (ErrCount == 1 && !state.dataGlobal->DisplayExtraWarnings) {
                ShowContinueError(state, "...use Output:Diagnostics,DisplayExtraWarnings; to show more details on each max iteration exceeded.");
            }
            if (state.dataGlobal->DisplayExtraWarnings) {

                for (AirSysNum = 1; AirSysNum <= NumPrimaryAirSys; ++AirSysNum) {

                    auto &arrayRef = state.dataConvergeParams->AirLoopConvergence(AirSysNum).HVACMassFlowNotConverged;
                    if (std::any_of(std::begin(arrayRef), std::end(arrayRef), [](bool i) { return i; })) {

                        ShowContinueError(state,
                                          "Air System Named = " + state.dataAirLoop->AirToZoneNodeInfo(AirSysNum).AirLoopName +
                                              " did not converge for mass flow rate");
                        ShowContinueError(state, "Check values should be zero. Most Recent values listed first.");
                        HistoryTrace = "";
                        for (StackDepth = 0; StackDepth < DataConvergParams::ConvergLogStackDepth; ++StackDepth) {
                            HistoryTrace +=
                                format("{:.6R},", state.dataConvergeParams->AirLoopConvergence(AirSysNum).HVACFlowDemandToSupplyTolValue[StackDepth]);
                        }

                        ShowContinueError(state, "Demand-to-Supply interface mass flow rate check value iteration history trace: " + HistoryTrace);
                        HistoryTrace = "";
                        for (StackDepth = 0; StackDepth < DataConvergParams::ConvergLogStackDepth; ++StackDepth) {
                            HistoryTrace += format(
                                "{:.6R},", state.dataConvergeParams->AirLoopConvergence(AirSysNum).HVACFlowSupplyDeck1ToDemandTolValue[StackDepth]);
                        }
                        ShowContinueError(state,
                                          "Supply-to-demand interface deck 1 mass flow rate check value iteration history trace: " + HistoryTrace);

                        if (state.dataAirLoop->AirToZoneNodeInfo(AirSysNum).NumSupplyNodes >= 2) {
                            HistoryTrace = "";
                            for (StackDepth = 0; StackDepth < DataConvergParams::ConvergLogStackDepth; ++StackDepth) {
                                HistoryTrace +=
                                    format("{:.6R},",
                                           state.dataConvergeParams->AirLoopConvergence(AirSysNum).HVACFlowSupplyDeck2ToDemandTolValue[StackDepth]);
                            }
                            ShowContinueError(
                                state, "Supply-to-demand interface deck 2 mass flow rate check value iteration history trace: " + HistoryTrace);
                        }
                    } // mass flow rate not converged

                    auto &arrayRef2 = state.dataConvergeParams->AirLoopConvergence(AirSysNum).HVACHumRatNotConverged;
                    if (std::any_of(std::begin(arrayRef2), std::end(arrayRef2), [](bool i) { return i; })) {

                        ShowContinueError(state,
                                          "Air System Named = " + state.dataAirLoop->AirToZoneNodeInfo(AirSysNum).AirLoopName +
                                              " did not converge for humidity ratio");
                        ShowContinueError(state, "Check values should be zero. Most Recent values listed first.");
                        HistoryTrace = "";
                        for (StackDepth = 0; StackDepth < DataConvergParams::ConvergLogStackDepth; ++StackDepth) {
                            HistoryTrace +=
                                format("{:.6R},", state.dataConvergeParams->AirLoopConvergence(AirSysNum).HVACHumDemandToSupplyTolValue[StackDepth]);
                        }
                        ShowContinueError(state, "Demand-to-Supply interface humidity ratio check value iteration history trace: " + HistoryTrace);
                        HistoryTrace = "";
                        for (StackDepth = 0; StackDepth < DataConvergParams::ConvergLogStackDepth; ++StackDepth) {
                            HistoryTrace += format(
                                "{:.6R},", state.dataConvergeParams->AirLoopConvergence(AirSysNum).HVACHumSupplyDeck1ToDemandTolValue[StackDepth]);
                        }
                        ShowContinueError(state,
                                          "Supply-to-demand interface deck 1 humidity ratio check value iteration history trace: " + HistoryTrace);

                        if (state.dataAirLoop->AirToZoneNodeInfo(AirSysNum).NumSupplyNodes >= 2) {
                            HistoryTrace = "";
                            for (StackDepth = 0; StackDepth < DataConvergParams::ConvergLogStackDepth; ++StackDepth) {
                                HistoryTrace +=
                                    format("{:.6R},",
                                           state.dataConvergeParams->AirLoopConvergence(AirSysNum).HVACHumSupplyDeck2ToDemandTolValue[StackDepth]);
                            }
                            ShowContinueError(
                                state, "Supply-to-demand interface deck 2 humidity ratio check value iteration history trace: " + HistoryTrace);
                        }
                    } // humidity ratio not converged

                    auto &arrayRef3 = state.dataConvergeParams->AirLoopConvergence(AirSysNum).HVACTempNotConverged;
                    if (std::any_of(std::begin(arrayRef3), std::end(arrayRef3), [](bool i) { return i; })) {

                        ShowContinueError(state,
                                          "Air System Named = " + state.dataAirLoop->AirToZoneNodeInfo(AirSysNum).AirLoopName +
                                              " did not converge for temperature");
                        ShowContinueError(state, "Check values should be zero. Most Recent values listed first.");
                        HistoryTrace = "";
                        for (StackDepth = 0; StackDepth < DataConvergParams::ConvergLogStackDepth; ++StackDepth) {
                            HistoryTrace +=
                                format("{:.6R},", state.dataConvergeParams->AirLoopConvergence(AirSysNum).HVACTempDemandToSupplyTolValue[StackDepth]);
                        }
                        ShowContinueError(state, "Demand-to-Supply interface temperature check value iteration history trace: " + HistoryTrace);
                        HistoryTrace = "";
                        for (StackDepth = 0; StackDepth < DataConvergParams::ConvergLogStackDepth; ++StackDepth) {
                            HistoryTrace += format(
                                "{:.6R},", state.dataConvergeParams->AirLoopConvergence(AirSysNum).HVACTempSupplyDeck1ToDemandTolValue[StackDepth]);
                        }
                        ShowContinueError(state,
                                          "Supply-to-demand interface deck 1 temperature check value iteration history trace: " + HistoryTrace);

                        if (state.dataAirLoop->AirToZoneNodeInfo(AirSysNum).NumSupplyNodes >= 2) {
                            HistoryTrace = "";
                            for (StackDepth = 0; StackDepth < DataConvergParams::ConvergLogStackDepth; ++StackDepth) {
                                HistoryTrace +=
                                    format("{:.6R},",
                                           state.dataConvergeParams->AirLoopConvergence(AirSysNum).HVACTempSupplyDeck1ToDemandTolValue[StackDepth]);
                            }
                            ShowContinueError(state,
                                              "Supply-to-demand interface deck 2 temperature check value iteration history trace: " + HistoryTrace);
                        }
                    } // Temps not converged

                    auto &arrayRef4 = state.dataConvergeParams->AirLoopConvergence(AirSysNum).HVACEnergyNotConverged;
                    if (std::any_of(std::begin(arrayRef4), std::end(arrayRef4), [](bool i) { return i; })) {

                        ShowContinueError(state,
                                          "Air System Named = " + state.dataAirLoop->AirToZoneNodeInfo(AirSysNum).AirLoopName +
                                              " did not converge for energy");
                        ShowContinueError(state, "Check values should be zero. Most Recent values listed first.");
                        HistoryTrace = "";
                        for (StackDepth = 0; StackDepth < DataConvergParams::ConvergLogStackDepth; ++StackDepth) {
                            HistoryTrace += format(
                                "{:.6R},", state.dataConvergeParams->AirLoopConvergence(AirSysNum).HVACEnergyDemandToSupplyTolValue[StackDepth]);
                        }
                        ShowContinueError(state, "Demand-to-Supply interface energy check value iteration history trace: " + HistoryTrace);
                        HistoryTrace = "";
                        for (StackDepth = 0; StackDepth < DataConvergParams::ConvergLogStackDepth; ++StackDepth) {
                            HistoryTrace += format(
                                "{:.6R},", state.dataConvergeParams->AirLoopConvergence(AirSysNum).HVACEnergySupplyDeck1ToDemandTolValue[StackDepth]);
                        }
                        ShowContinueError(state, "Supply-to-demand interface deck 1 energy check value iteration history trace: " + HistoryTrace);

                        if (state.dataAirLoop->AirToZoneNodeInfo(AirSysNum).NumSupplyNodes >= 2) {
                            HistoryTrace = "";
                            for (StackDepth = 0; StackDepth < DataConvergParams::ConvergLogStackDepth; ++StackDepth) {
                                HistoryTrace +=
                                    format("{:.6R},",
                                           state.dataConvergeParams->AirLoopConvergence(AirSysNum).HVACEnergySupplyDeck2ToDemandTolValue[StackDepth]);
                            }
                            ShowContinueError(state, "Supply-to-demand interface deck 2 energy check value iteration history trace: " + HistoryTrace);
                        }
                    } // energy not converged

                } // loop over air loop systems

                // loop over zones and check for issues with zone inlet nodes
                for (ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {

                    for (NodeIndex = 1; NodeIndex <= state.dataConvergeParams->ZoneInletConvergence(ZoneNum).NumInletNodes; ++NodeIndex) {

                        auto &humRatInletNode = state.dataConvergeParams->ZoneInletConvergence(ZoneNum).InletNode(NodeIndex).HumidityRatio;
                        auto &mdotInletNode = state.dataConvergeParams->ZoneInletConvergence(ZoneNum).InletNode(NodeIndex).MassFlowRate;
                        auto &inletTemp = state.dataConvergeParams->ZoneInletConvergence(ZoneNum).InletNode(NodeIndex).Temperature;

                        // Check humidity ratio
                        FoundOscillationByDuplicate = false;
                        MonotonicDecreaseFound = false;
                        MonotonicIncreaseFound = false;
                        // check for evidence of oscillation by identifying duplicates when latest value not equal to average
                        Real64 summation = 0.0;
                        summation = std::accumulate(humRatInletNode.begin(), humRatInletNode.end(), 0.0);
                        AvgValue = summation / double(DataConvergParams::ConvergLogStackDepth);
                        if (std::abs(humRatInletNode[0] - AvgValue) >
                            DataConvergParams::HVACHumRatOscillationToler) { // last iterate differs from average
                            FoundOscillationByDuplicate = false;
                            for (StackDepth = 1; StackDepth < DataConvergParams::ConvergLogStackDepth; ++StackDepth) {
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

                                auto humRatInletNodDotProd = std::inner_product(
                                    std::begin(ConvergLogStackARR), std::end(ConvergLogStackARR), std::begin(humRatInletNode), 0.0);
                                Real64 summation2 = 0.0;
                                summation2 = std::accumulate(humRatInletNode.begin(), humRatInletNode.end(), 0.0);
                                SlopeHumRat =
                                    (sum_ConvergLogStackARR * summation2 - double(DataConvergParams::ConvergLogStackDepth) * humRatInletNodDotProd) /
                                    (square_sum_ConvergLogStackARR - double(DataConvergParams::ConvergLogStackDepth) * sum_square_ConvergLogStackARR);
                                if (std::abs(SlopeHumRat) > DataConvergParams::HVACHumRatSlopeToler) {

                                    if (SlopeHumRat < 0.0) { // check for monotonic decrease
                                        MonotonicDecreaseFound = true;
                                        for (StackDepth = 1; StackDepth < DataConvergParams::ConvergLogStackDepth; ++StackDepth) {
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
                                        for (StackDepth = 1; StackDepth < DataConvergParams::ConvergLogStackDepth; ++StackDepth) {
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
                            HistoryTrace = "";
                            for (StackDepth = 0; StackDepth < DataConvergParams::ConvergLogStackDepth; ++StackDepth) {
                                HistoryTrace += format("{:.6R},", humRatInletNode[StackDepth]);
                            }
                            ShowContinueError(
                                state,
                                "Node named " +
                                    state.dataLoopNodes->NodeID(
                                        state.dataConvergeParams->ZoneInletConvergence(ZoneNum).InletNode(NodeIndex).NodeNum) +
                                    " humidity ratio [kg-water/kg-dryair] iteration history trace (most recent first): " + HistoryTrace);
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
                            for (StackDepth = 1; StackDepth < DataConvergParams::ConvergLogStackDepth; ++StackDepth) {
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

                                auto humRatInletNodDotProd =
                                    std::inner_product(std::begin(ConvergLogStackARR), std::end(ConvergLogStackARR), std::begin(mdotInletNode), 0.0);
                                Real64 summation3 = 0.0;
                                summation3 = std::accumulate(mdotInletNode.begin(), mdotInletNode.end(), 0.0);
                                SlopeMdot =
                                    (sum_ConvergLogStackARR * summation3 - double(DataConvergParams::ConvergLogStackDepth) * humRatInletNodDotProd) /
                                    (square_sum_ConvergLogStackARR - double(DataConvergParams::ConvergLogStackDepth) * sum_square_ConvergLogStackARR);
                                if (std::abs(SlopeMdot) > DataConvergParams::HVACFlowRateSlopeToler) {
                                    if (SlopeMdot < 0.0) { // check for monotonic decrease
                                        MonotonicDecreaseFound = true;
                                        for (StackDepth = 1; StackDepth < DataConvergParams::ConvergLogStackDepth; ++StackDepth) {
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
                                        for (StackDepth = 1; StackDepth < DataConvergParams::ConvergLogStackDepth; ++StackDepth) {
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
                            HistoryTrace = "";
                            for (StackDepth = 0; StackDepth < DataConvergParams::ConvergLogStackDepth; ++StackDepth) {
                                HistoryTrace += format("{:.6R},", mdotInletNode[StackDepth]);
                            }
                            ShowContinueError(state,
                                              "Node named " +
                                                  state.dataLoopNodes->NodeID(
                                                      state.dataConvergeParams->ZoneInletConvergence(ZoneNum).InletNode(NodeIndex).NodeNum) +
                                                  " mass flow rate [kg/s] iteration history trace (most recent first): " + HistoryTrace);
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
                            for (StackDepth = 1; StackDepth < DataConvergParams::ConvergLogStackDepth; ++StackDepth) {
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

                                auto inletTempDotProd =
                                    std::inner_product(std::begin(ConvergLogStackARR), std::end(ConvergLogStackARR), std::begin(inletTemp), 0.0);

                                Real64 summation4 = 0.0;
                                summation4 = std::accumulate(inletTemp.begin(), inletTemp.end(), 0.0);
                                SlopeTemps =
                                    (sum_ConvergLogStackARR * summation4 - double(DataConvergParams::ConvergLogStackDepth) * inletTempDotProd) /
                                    (square_sum_ConvergLogStackARR - double(DataConvergParams::ConvergLogStackDepth) * sum_square_ConvergLogStackARR);
                                if (std::abs(SlopeTemps) > DataConvergParams::HVACTemperatureSlopeToler) {
                                    if (SlopeTemps < 0.0) { // check for monotonic decrease
                                        MonotonicDecreaseFound = true;
                                        for (StackDepth = 1; StackDepth < DataConvergParams::ConvergLogStackDepth; ++StackDepth) {
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
                                        for (StackDepth = 1; StackDepth < DataConvergParams::ConvergLogStackDepth; ++StackDepth) {
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
                            HistoryTrace = "";
                            for (StackDepth = 0; StackDepth < DataConvergParams::ConvergLogStackDepth; ++StackDepth) {
                                HistoryTrace += format("{:.6R},", inletTemp[StackDepth]);
                            }
                            ShowContinueError(state,
                                              "Node named " +
                                                  state.dataLoopNodes->NodeID(
                                                      state.dataConvergeParams->ZoneInletConvergence(ZoneNum).InletNode(NodeIndex).NodeNum) +
                                                  " temperature [C] iteration history trace (most recent first): " + HistoryTrace);
                        } // need to report trace
                          // end Temperature checks

                    } // loop over zone inlet nodes
                }     // loop over zones

                for (LoopNum = 1; LoopNum <= state.dataPlnt->TotNumLoops; ++LoopNum) {

                    if (state.dataConvergeParams->PlantConvergence(LoopNum).PlantMassFlowNotConverged) {
                        ShowContinueError(state,
                                          "Plant System Named = " + state.dataPlnt->PlantLoop(LoopNum).Name + " did not converge for mass flow rate");
                        ShowContinueError(state, "Check values should be zero. Most Recent values listed first.");
                        HistoryTrace = "";
                        for (StackDepth = 0; StackDepth < DataConvergParams::ConvergLogStackDepth; ++StackDepth) {
                            HistoryTrace +=
                                format("{:.6R},", state.dataConvergeParams->PlantConvergence(LoopNum).PlantFlowDemandToSupplyTolValue[StackDepth]);
                        }
                        ShowContinueError(state, "Demand-to-Supply interface mass flow rate check value iteration history trace: " + HistoryTrace);
                        HistoryTrace = "";
                        for (StackDepth = 0; StackDepth < DataConvergParams::ConvergLogStackDepth; ++StackDepth) {
                            HistoryTrace +=
                                format("{:.6R},", state.dataConvergeParams->PlantConvergence(LoopNum).PlantFlowSupplyToDemandTolValue[StackDepth]);
                        }
                        ShowContinueError(state, "Supply-to-Demand interface mass flow rate check value iteration history trace: " + HistoryTrace);

                        // now work with history logs for mass flow to detect issues
                        for (auto ThisLoopSide : DataPlant::LoopSideKeys) {

                            auto &mdotHistInletNode = state.dataPlnt->PlantLoop(LoopNum).LoopSide(ThisLoopSide).InletNode.MassFlowRateHistory;
                            auto &mdotHistOutletNode = state.dataPlnt->PlantLoop(LoopNum).LoopSide(ThisLoopSide).OutletNode.MassFlowRateHistory;

                            // loop side inlet node
                            FoundOscillationByDuplicate = false;
                            MonotonicDecreaseFound = false;
                            MonotonicIncreaseFound = false;
                            AvgValue = sum(mdotHistInletNode) / double(NumConvergenceHistoryTerms);
                            if (std::abs(mdotHistInletNode(1) - AvgValue) > DataConvergParams::PlantFlowRateOscillationToler) {
                                FoundOscillationByDuplicate = false;
                                for (StackDepth = 2; StackDepth <= NumConvergenceHistoryTerms; ++StackDepth) {
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

                                SlopeMdot =
                                    (sum_ConvergenceHistoryARR * sum(mdotHistInletNode) -
                                     double(NumConvergenceHistoryTerms) * mdotHistInletNodeDotProd) /
                                    (square_sum_ConvergenceHistoryARR - double(NumConvergenceHistoryTerms) * sum_square_ConvergenceHistoryARR);
                                if (std::abs(SlopeMdot) > DataConvergParams::PlantFlowRateSlopeToler) {
                                    if (SlopeMdot < 0.0) { // check for monotonic decrease
                                        MonotonicDecreaseFound = true;
                                        for (StackDepth = 2; StackDepth <= NumConvergenceHistoryTerms; ++StackDepth) {
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
                                        for (StackDepth = 2; StackDepth <= NumConvergenceHistoryTerms; ++StackDepth) {
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
                                for (StackDepth = 1; StackDepth <= NumConvergenceHistoryTerms; ++StackDepth) {
                                    HistoryTrace += format("{:.7R},", mdotHistInletNode(StackDepth));
                                }
                                ShowContinueError(state,
                                                  "Node named " + state.dataPlnt->PlantLoop(LoopNum).LoopSide(ThisLoopSide).NodeNameIn +
                                                      " mass flow rate [kg/s] iteration history trace (most recent first): " + HistoryTrace);
                            } // need to report trace
                            // end of inlet node

                            // loop side outlet node
                            FoundOscillationByDuplicate = false;
                            MonotonicDecreaseFound = false;
                            MonotonicIncreaseFound = false;
                            AvgValue = sum(mdotHistOutletNode) / double(NumConvergenceHistoryTerms);
                            if (std::abs(mdotHistOutletNode(1) - AvgValue) > DataConvergParams::PlantFlowRateOscillationToler) {
                                FoundOscillationByDuplicate = false;
                                for (StackDepth = 2; StackDepth <= NumConvergenceHistoryTerms; ++StackDepth) {
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

                                SlopeMdot =
                                    (sum_ConvergenceHistoryARR * sum(mdotHistOutletNode) -
                                     double(NumConvergenceHistoryTerms) * mdotHistOutletNodeDotProd) /
                                    (square_sum_ConvergenceHistoryARR - double(NumConvergenceHistoryTerms) * sum_square_ConvergenceHistoryARR);
                                if (std::abs(SlopeMdot) > DataConvergParams::PlantFlowRateSlopeToler) {
                                    if (SlopeMdot < 0.0) { // check for monotonic decrease
                                        MonotonicDecreaseFound = true;
                                        for (StackDepth = 2; StackDepth <= NumConvergenceHistoryTerms; ++StackDepth) {
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
                                        for (StackDepth = 2; StackDepth <= NumConvergenceHistoryTerms; ++StackDepth) {
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
                                for (StackDepth = 1; StackDepth <= NumConvergenceHistoryTerms; ++StackDepth) {
                                    HistoryTrace += format("{:.7R},", mdotHistOutletNode(StackDepth));
                                }
                                ShowContinueError(state,
                                                  "Node named " + state.dataPlnt->PlantLoop(LoopNum).LoopSide(ThisLoopSide).NodeNameOut +
                                                      " mass flow rate [kg/s] iteration history trace (most recent first): " + HistoryTrace);
                            } // need to report trace
                              // end of Outlet node

                        } // plant loop sides

                    } // mass flow not converged

                    if (state.dataConvergeParams->PlantConvergence(LoopNum).PlantTempNotConverged) {
                        ShowContinueError(state,
                                          "Plant System Named = " + state.dataPlnt->PlantLoop(LoopNum).Name + " did not converge for temperature");
                        ShowContinueError(state, "Check values should be zero. Most Recent values listed first.");
                        HistoryTrace = "";
                        for (StackDepth = 0; StackDepth < DataConvergParams::ConvergLogStackDepth; ++StackDepth) {
                            HistoryTrace +=
                                format("{:.6R},", state.dataConvergeParams->PlantConvergence(LoopNum).PlantTempDemandToSupplyTolValue[StackDepth]);
                        }
                        ShowContinueError(state, "Demand-to-Supply interface temperature check value iteration history trace: " + HistoryTrace);
                        HistoryTrace = "";
                        for (StackDepth = 0; StackDepth < DataConvergParams::ConvergLogStackDepth; ++StackDepth) {
                            HistoryTrace +=
                                format("{:.6R},", state.dataConvergeParams->PlantConvergence(LoopNum).PlantTempSupplyToDemandTolValue[StackDepth]);
                        }
                        ShowContinueError(state, "Supply-to-Demand interface temperature check value iteration history trace: " + HistoryTrace);

                        // now work with history logs for mass flow to detect issues
                        for (auto ThisLoopSide : DataPlant::LoopSideKeys) {

                            auto &tempHistInletNode = state.dataPlnt->PlantLoop(LoopNum).LoopSide(ThisLoopSide).InletNode.TemperatureHistory;
                            auto &tempHistOutletNode = state.dataPlnt->PlantLoop(LoopNum).LoopSide(ThisLoopSide).OutletNode.TemperatureHistory;

                            // loop side inlet node
                            FoundOscillationByDuplicate = false;
                            MonotonicDecreaseFound = false;
                            MonotonicIncreaseFound = false;
                            AvgValue = sum(tempHistInletNode) / double(NumConvergenceHistoryTerms);
                            if (std::abs(tempHistInletNode(1) - AvgValue) > DataConvergParams::PlantTemperatureOscillationToler) {
                                FoundOscillationByDuplicate = false;
                                for (StackDepth = 2; StackDepth <= NumConvergenceHistoryTerms; ++StackDepth) {
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

                                SlopeTemps =
                                    (sum_ConvergenceHistoryARR * sum(tempHistInletNode) -
                                     double(NumConvergenceHistoryTerms) * tempHistInletNodeDotProd) /
                                    (square_sum_ConvergenceHistoryARR - double(NumConvergenceHistoryTerms) * sum_square_ConvergenceHistoryARR);
                                if (std::abs(SlopeTemps) > DataConvergParams::PlantTemperatureSlopeToler) {
                                    if (SlopeTemps < 0.0) { // check for monotonic decrease
                                        MonotonicDecreaseFound = true;
                                        for (StackDepth = 2; StackDepth <= NumConvergenceHistoryTerms; ++StackDepth) {
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
                                        for (StackDepth = 2; StackDepth <= NumConvergenceHistoryTerms; ++StackDepth) {
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
                                for (StackDepth = 1; StackDepth <= NumConvergenceHistoryTerms; ++StackDepth) {
                                    HistoryTrace += format("{:.5R},", tempHistInletNode(StackDepth));
                                }
                                ShowContinueError(state,
                                                  "Node named " + state.dataPlnt->PlantLoop(LoopNum).LoopSide(ThisLoopSide).NodeNameIn +
                                                      " temperature [C] iteration history trace (most recent first): " + HistoryTrace);
                            } // need to report trace
                            // end of inlet node

                            // loop side outlet node
                            FoundOscillationByDuplicate = false;
                            MonotonicDecreaseFound = false;
                            MonotonicIncreaseFound = false;
                            AvgValue = sum(tempHistOutletNode) / double(NumConvergenceHistoryTerms);
                            if (std::abs(tempHistOutletNode(1) - AvgValue) > DataConvergParams::PlantTemperatureOscillationToler) {
                                FoundOscillationByDuplicate = false;
                                for (StackDepth = 2; StackDepth <= NumConvergenceHistoryTerms; ++StackDepth) {
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

                                SlopeTemps =
                                    (sum_ConvergenceHistoryARR * sum(tempHistOutletNode) -
                                     double(NumConvergenceHistoryTerms) * tempHistOutletNodeDotProd) /
                                    (square_sum_ConvergenceHistoryARR - double(NumConvergenceHistoryTerms) * sum_square_ConvergenceHistoryARR);
                                if (std::abs(SlopeTemps) > DataConvergParams::PlantFlowRateSlopeToler) {
                                    if (SlopeTemps < 0.0) { // check for monotonic decrease
                                        MonotonicDecreaseFound = true;
                                        for (StackDepth = 2; StackDepth <= NumConvergenceHistoryTerms; ++StackDepth) {
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
                                        for (StackDepth = 2; StackDepth <= NumConvergenceHistoryTerms; ++StackDepth) {
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
                                for (StackDepth = 1; StackDepth <= NumConvergenceHistoryTerms; ++StackDepth) {
                                    HistoryTrace += format("{:.5R},", tempHistOutletNode(StackDepth));
                                }
                                ShowContinueError(state,
                                                  "Node named " + state.dataPlnt->PlantLoop(LoopNum).LoopSide(ThisLoopSide).NodeNameOut +
                                                      " temperature [C] iteration history trace (most recent first): " + HistoryTrace);
                            } // need to report trace
                              // end of Outlet node

                        } // plant loop sides

                    } // temperature not converged
                }     // loop over plant loop systems
            }
        } else {
            if (state.dataEnvrn->EnvironmentName == ErrEnvironmentName) {
                ShowRecurringWarningErrorAtEnd(state,
                                               "SimHVAC: Exceeding Maximum iterations for all HVAC loops, during " +
                                                   state.dataEnvrn->EnvironmentName + " continues",
                                               MaxErrCount);
            } else {
                MaxErrCount = 0;
                ErrEnvironmentName = state.dataEnvrn->EnvironmentName;
                ShowRecurringWarningErrorAtEnd(state,
                                               "SimHVAC: Exceeding Maximum iterations for all HVAC loops, during " +
                                                   state.dataEnvrn->EnvironmentName + " continues",
                                               MaxErrCount);
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
                    e.TempSetPoint = SensedNodeFlagValue;
                    e.HumRatSetPoint = SensedNodeFlagValue;
                    e.HumRatMin = SensedNodeFlagValue;
                    e.HumRatMax = SensedNodeFlagValue;
                    e.MassFlowRateSetPoint = SensedNodeFlagValue; // BG 5-26-2009 (being checked in HVACControllers.cc)
                }
                state.dataLoopNodes->DefaultNodeValues.TempSetPoint = SensedNodeFlagValue;
                state.dataLoopNodes->DefaultNodeValues.HumRatSetPoint = SensedNodeFlagValue;
                state.dataLoopNodes->DefaultNodeValues.HumRatMin = SensedNodeFlagValue;
                state.dataLoopNodes->DefaultNodeValues.HumRatMax = SensedNodeFlagValue;
                state.dataLoopNodes->DefaultNodeValues.MassFlowRateSetPoint =
                    SensedNodeFlagValue; // BG 5-26-2009 (being checked in HVACControllers.cc)
            }
            state.dataHVACMgr->MySetPointInit = false;
            DoSetPointTest = true;
        } else {
            DoSetPointTest = false;
        }

        if (state.dataCoilCooingDX->stillNeedToReportStandardRatings) {
            if (!state.dataGlobal->ZoneSizingCalc && !state.dataGlobal->SysSizingCalc && !state.dataGlobal->WarmupFlag) {
                CoilCoolingDX::reportAllStandardRatings(state);
            }
        }
    }
    if (SetPointErrorFlag) {
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
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine receives the flags from SimHVAC which determines
    // which middle-level managers must be called.

    // METHODOLOGY EMPLOYED:
    // Each flag is checked and the appropriate manager is then called.

    // Using/Aliasing
    using NonZoneEquipmentManager::ManageNonZoneEquipment;
    using PlantManager::ManagePlantLoops;
    using PlantUtilities::AnyPlantLoopSidesNeedSim;
    using PlantUtilities::ResetAllPlantInterConnectFlags;
    using PlantUtilities::SetAllFlowLocks;
    using SimAirServingZones::ManageAirLoops;
    using ZoneEquipmentManager::ManageZoneEquipment;

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:
    bool ResimulateAirZone; // True when solution technique on third iteration used in AirflowNetwork

    // SUBROUTINE PARAMETER DEFINITIONS:
    int constexpr MaxAir(5); // Iteration Max for Air Simulation Iterations

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int IterAir; // counts iterations to enforce maximum iteration limit

    IterAir = 0;

    // Set all plant flow locks to UNLOCKED to allow air side components to operate properly
    // This requires that the plant flow resolver carefully set the min/max avail limits on
    //  air side components to ensure they request within bounds.
    if (LockPlantFlows) {
        SetAllFlowLocks(state, DataPlant::FlowLock::Locked);
    } else {
        SetAllFlowLocks(state, DataPlant::FlowLock::Unlocked);
    }
    ResetAllPlantInterConnectFlags(state);

    if (state.dataGlobal->BeginEnvrnFlag && state.dataHVACMgr->MyEnvrnFlag2) {
        // Following comment is incorrect!  (LKL) Even the first time through this does more than read in data.
        // Zone equipment data needs to be read in before air loop data to allow the
        // determination of which zones are connected to which air loops.
        // This call of ManageZoneEquipment does nothing except force the
        // zone equipment data to be read in.
        ManageZoneEquipment(state, FirstHVACIteration, SimZoneEquipment, SimAirLoops);
        state.dataHVACMgr->MyEnvrnFlag2 = false;
    }
    if (!state.dataGlobal->BeginEnvrnFlag) {
        state.dataHVACMgr->MyEnvrnFlag2 = true;
    }

    if (FirstHVACIteration) {
        state.dataHVACMgr->RepIterAir = 0;
        // Call AirflowNetwork simulation to calculate air flows and pressures
        if (state.afn->SimulateAirflowNetwork > AirflowNetwork::AirflowNetworkControlSimple) {
            state.afn->manage_balance(FirstHVACIteration);
        }
        ManageAirLoops(state, FirstHVACIteration, SimAirLoops, SimZoneEquipment);
        state.dataAirLoop->AirLoopInputsFilled = true; // all air loop inputs have been read in
        SimAirLoops = true; // Need to make sure that SimAirLoop is simulated at min twice to calculate PLR in some air loop equipment
        state.dataHVACGlobal->AirLoopsSimOnce = true; // air loops simulated once for this environment
        ResetTerminalUnitFlowLimits(state);
        state.dataHVACMgr->FlowMaxAvailAlreadyReset = true;
        ManageZoneEquipment(state, FirstHVACIteration, SimZoneEquipment, SimAirLoops);
        SimZoneEquipment = true; // needs to be simulated at least twice for flow resolution to propagate to this routine
        ManageNonZoneEquipment(state, FirstHVACIteration, SimNonZoneEquipment);
        state.dataElectPwrSvcMgr->facilityElectricServiceObj->manageElectricPowerService(
            state, FirstHVACIteration, state.dataHVACGlobal->SimElecCircuitsFlag, false);

        ManagePlantLoops(state, FirstHVACIteration, SimAirLoops, SimZoneEquipment, SimNonZoneEquipment, SimPlantLoops, SimElecCircuits);

        state.dataErrTracking->AskForPlantCheckOnAbort = true; // need to make a first pass through plant calcs before this check make sense
        state.dataElectPwrSvcMgr->facilityElectricServiceObj->manageElectricPowerService(
            state, FirstHVACIteration, state.dataHVACGlobal->SimElecCircuitsFlag, false);
    } else {
        state.dataHVACMgr->FlowResolutionNeeded = false;
        while ((SimAirLoops || SimZoneEquipment) && (IterAir <= MaxAir)) {
            ++IterAir; // Increment the iteration counter
            // Call AirflowNetwork simulation to calculate air flows and pressures
            ResimulateAirZone = false;
            if (state.afn->SimulateAirflowNetwork > AirflowNetwork::AirflowNetworkControlSimple) {
                state.afn->manage_balance(FirstHVACIteration, IterAir, ResimulateAirZone);
            }
            if (SimAirLoops) {
                ManageAirLoops(state, FirstHVACIteration, SimAirLoops, SimZoneEquipment);
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
                ManageZoneEquipment(state, FirstHVACIteration, SimZoneEquipment, SimAirLoops);
                SimElecCircuits = true; // If this was simulated there are possible electric changes that need to be simulated
            }
            state.dataHVACMgr->FlowMaxAvailAlreadyReset = false;

            //      IterAir = IterAir + 1   ! Increment the iteration counter
            if (state.afn->SimulateAirflowNetwork > AirflowNetwork::AirflowNetworkControlSimple) {
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
            ManageNonZoneEquipment(state, FirstHVACIteration, SimNonZoneEquipment);
            SimElecCircuits = true; // If this was simulated there are possible electric changes that need to be simulated
        }

        if (SimElecCircuits) {
            state.dataElectPwrSvcMgr->facilityElectricServiceObj->manageElectricPowerService(
                state, FirstHVACIteration, state.dataHVACGlobal->SimElecCircuitsFlag, false);
        }

        if (!SimPlantLoops) {
            // check to see if any air side component may have requested plant resim
            if (AnyPlantLoopSidesNeedSim(state)) {
                SimPlantLoops = true;
            }
        }

        if (SimPlantLoops) {
            ManagePlantLoops(state, FirstHVACIteration, SimAirLoops, SimZoneEquipment, SimNonZoneEquipment, SimPlantLoops, SimElecCircuits);
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
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Reset the max flow available limits at the inlet nodes of terminal units

    // METHODOLOGY EMPLOYED:
    // Loops through all air loops, finds the inlet nodes of the terminal units
    // served by each air loop, and resets the node MassFlowRateMaxAvail (and MinAvail) to
    // the hard max and mins.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int AirLoopIndex;
    int ZonesCooledIndex;
    int ZonesHeatedIndex;
    int TermInletNode;

    for (AirLoopIndex = 1; AirLoopIndex <= state.dataHVACGlobal->NumPrimaryAirSys; ++AirLoopIndex) { // loop over the primary air loops
        for (ZonesCooledIndex = 1; ZonesCooledIndex <= state.dataAirLoop->AirToZoneNodeInfo(AirLoopIndex).NumZonesCooled;
             ++ZonesCooledIndex) { // loop over the zones cooled by this air loop
            TermInletNode = state.dataAirLoop->AirToZoneNodeInfo(AirLoopIndex).TermUnitCoolInletNodes(ZonesCooledIndex);
            // reset the max avail flow rate at the terminal unit cold air inlet to the max
            state.dataLoopNodes->Node(TermInletNode).MassFlowRateMaxAvail = state.dataLoopNodes->Node(TermInletNode).MassFlowRateMax;
            state.dataLoopNodes->Node(TermInletNode).MassFlowRateMinAvail = state.dataLoopNodes->Node(TermInletNode).MassFlowRateMin;
        }
        for (ZonesHeatedIndex = 1; ZonesHeatedIndex <= state.dataAirLoop->AirToZoneNodeInfo(AirLoopIndex).NumZonesHeated;
             ++ZonesHeatedIndex) { // loop over the zones heated by this air loop
            TermInletNode = state.dataAirLoop->AirToZoneNodeInfo(AirLoopIndex).TermUnitHeatInletNodes(ZonesHeatedIndex);
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
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine is for resolving hard flow mismatches between zone equipment and
    // the primary air loop. Such a mismatch can occur when the air terminal units are
    // requesting more air than the central air system can supply.

    // METHODOLOGY EMPLOYED:
    // Sets the MassFlowRateMaxAvail on the terminal unit inlet nodes to match the
    // maximum available from the primary air loop.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int AirLoopIndex;
    int ZonesCooledIndex;
    int ZonesHeatedIndex;
    int TermInletNode;
    int SupplyIndex;
    int SupplyNode;
    Real64 FlowRatio;

    auto &AirToZoneNodeInfo(state.dataAirLoop->AirToZoneNodeInfo);

    for (AirLoopIndex = 1; AirLoopIndex <= state.dataHVACGlobal->NumPrimaryAirSys; ++AirLoopIndex) {          // loop over the primary air loops
        for (SupplyIndex = 1; SupplyIndex <= AirToZoneNodeInfo(AirLoopIndex).NumSupplyNodes; ++SupplyIndex) { // loop over the air loop supply outlets
            if (AirToZoneNodeInfo(AirLoopIndex).SupplyDuctType(SupplyIndex) == Cooling) {                     // check for cooling duct
                // check if terminal units requesting more air than air loop can supply; if so, set terminal unit inlet
                // node mass flow max avail to what air loop can supply
                SupplyNode = AirToZoneNodeInfo(AirLoopIndex).AirLoopSupplyNodeNum(SupplyIndex);
                if (state.dataLoopNodes->Node(SupplyNode).MassFlowRate > 0.0) {
                    // must include bypass flow for ChangeoverBypass system so that terminal units are not restricted (e.g., MaxAvail is lowered)
                    if ((state.dataLoopNodes->Node(SupplyNode).MassFlowRateSetPoint - state.dataLoopNodes->Node(SupplyNode).MassFlowRate -
                         state.dataAirLoop->AirLoopFlow(AirLoopIndex).BypassMassFlow) > DataConvergParams::HVACFlowRateToler * 0.01) {
                        FlowRatio = state.dataLoopNodes->Node(SupplyNode).MassFlowRate / state.dataLoopNodes->Node(SupplyNode).MassFlowRateSetPoint;
                        for (ZonesCooledIndex = 1; ZonesCooledIndex <= AirToZoneNodeInfo(AirLoopIndex).NumZonesCooled; ++ZonesCooledIndex) {
                            TermInletNode = AirToZoneNodeInfo(AirLoopIndex).TermUnitCoolInletNodes(ZonesCooledIndex);
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
                            for (ZonesCooledIndex = 1; ZonesCooledIndex <= AirToZoneNodeInfo(AirLoopIndex).NumZonesCooled; ++ZonesCooledIndex) {
                                TermInletNode = AirToZoneNodeInfo(AirLoopIndex).TermUnitCoolInletNodes(ZonesCooledIndex);
                                state.dataLoopNodes->Node(TermInletNode).MassFlowRateMaxAvail =
                                    state.dataLoopNodes->Node(TermInletNode).MassFlowRateMax;
                                state.dataLoopNodes->Node(TermInletNode).MassFlowRateMinAvail =
                                    state.dataLoopNodes->Node(SupplyNode).MassFlowRate / double(AirToZoneNodeInfo(AirLoopIndex).NumZonesCooled);
                            }
                        } else {
                            FlowRatio =
                                state.dataLoopNodes->Node(SupplyNode).MassFlowRate / state.dataLoopNodes->Node(SupplyNode).MassFlowRateSetPoint;
                            for (ZonesCooledIndex = 1; ZonesCooledIndex <= AirToZoneNodeInfo(AirLoopIndex).NumZonesCooled; ++ZonesCooledIndex) {
                                TermInletNode = AirToZoneNodeInfo(AirLoopIndex).TermUnitCoolInletNodes(ZonesCooledIndex);
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
        for (SupplyIndex = 1; SupplyIndex <= AirToZoneNodeInfo(AirLoopIndex).NumSupplyNodes; ++SupplyIndex) { // loop over the air loop supply outlets
            if (AirToZoneNodeInfo(AirLoopIndex).SupplyDuctType(SupplyIndex) == Heating) {                     // check for heating duct
                // check if terminal units requesting more air than air loop can supply; if so, set terminal unit inlet
                // node mass flow max avail to what air loop can supply
                SupplyNode = AirToZoneNodeInfo(AirLoopIndex).AirLoopSupplyNodeNum(SupplyIndex);
                if (state.dataLoopNodes->Node(SupplyNode).MassFlowRate > 0.0) {
                    // must include bypass flow for ChangeoverBypass system so that terminal units are not restricted (e.g., MaxAvail is lowered)
                    if ((state.dataLoopNodes->Node(SupplyNode).MassFlowRateSetPoint - state.dataLoopNodes->Node(SupplyNode).MassFlowRate -
                         state.dataAirLoop->AirLoopFlow(AirLoopIndex).BypassMassFlow) > DataConvergParams::HVACFlowRateToler * 0.01) {
                        FlowRatio = state.dataLoopNodes->Node(SupplyNode).MassFlowRate / state.dataLoopNodes->Node(SupplyNode).MassFlowRateSetPoint;
                        for (ZonesHeatedIndex = 1; ZonesHeatedIndex <= AirToZoneNodeInfo(AirLoopIndex).NumZonesHeated; ++ZonesHeatedIndex) {
                            TermInletNode = AirToZoneNodeInfo(AirLoopIndex).TermUnitHeatInletNodes(ZonesHeatedIndex);
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
                            for (ZonesHeatedIndex = 1; ZonesHeatedIndex <= AirToZoneNodeInfo(AirLoopIndex).NumZonesHeated; ++ZonesHeatedIndex) {
                                TermInletNode = AirToZoneNodeInfo(AirLoopIndex).TermUnitHeatInletNodes(ZonesHeatedIndex);
                                state.dataLoopNodes->Node(TermInletNode).MassFlowRateMaxAvail =
                                    state.dataLoopNodes->Node(TermInletNode).MassFlowRateMax;
                                state.dataLoopNodes->Node(TermInletNode).MassFlowRateMinAvail =
                                    state.dataLoopNodes->Node(SupplyNode).MassFlowRate / double(AirToZoneNodeInfo(AirLoopIndex).NumZonesCooled);
                            }
                        } else {
                            FlowRatio =
                                state.dataLoopNodes->Node(SupplyNode).MassFlowRate / state.dataLoopNodes->Node(SupplyNode).MassFlowRateSetPoint;
                            for (ZonesHeatedIndex = 1; ZonesHeatedIndex <= AirToZoneNodeInfo(AirLoopIndex).NumZonesHeated; ++ZonesHeatedIndex) {
                                TermInletNode = AirToZoneNodeInfo(AirLoopIndex).TermUnitHeatInletNodes(ZonesHeatedIndex);
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
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine checks for components lockout flags and asks for air loop resimulation
    // if any components have been locked out

    // METHODOLOGY EMPLOYED:
    // Checks if loop lockout flags are .TRUE.; if so, sets SimAirLoops to .TRUE.

    auto &AirLoopControlInfo(state.dataAirLoop->AirLoopControlInfo);

    for (int AirLoopIndex = 1; AirLoopIndex <= state.dataHVACGlobal->NumPrimaryAirSys; ++AirLoopIndex) { // loop over the primary air loops
        // check if economizer ia active and if there is a request that it be locked out
        if (AirLoopControlInfo(AirLoopIndex).EconoActive &&
            (AirLoopControlInfo(AirLoopIndex).ReqstEconoLockoutWithCompressor || AirLoopControlInfo(AirLoopIndex).ReqstEconoLockoutWithHeating)) {
            AirLoopControlInfo(AirLoopIndex).EconoLockout = true;
            SimAir = true;
        }
    }
}

void ResetHVACControl(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   December 2004
    //       MODIFIED       na
    //       RE-ENGINEERED  na

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
    //       MODIFIED       na
    //       RE-ENGINEERED  na

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

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Apparently someone who doesn't believe in documenting.
    //       DATE WRITTEN   ???
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // Using/Aliasing
    using namespace DataHeatBalance;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int ZoneNum;
    int ListNum;
    int GroupNum;
    int Mult;

    auto &ZoneList(state.dataHeatBal->ZoneList);
    auto &ZoneGroup(state.dataHeatBal->ZoneGroup);
    auto &ZoneListSNLoadHeatEnergy(state.dataHeatBal->ZoneListSNLoadHeatEnergy);
    auto &ZoneListSNLoadCoolEnergy(state.dataHeatBal->ZoneListSNLoadCoolEnergy);
    auto &ZoneListSNLoadHeatRate(state.dataHeatBal->ZoneListSNLoadHeatRate);
    auto &ZoneListSNLoadCoolRate(state.dataHeatBal->ZoneListSNLoadCoolRate);

    // Sum ZONE LIST and ZONE GROUP report variables
    for (ListNum = 1; ListNum <= state.dataHeatBal->NumOfZoneLists; ++ListNum) {
        ZoneListSNLoadHeatEnergy(ListNum) = 0.0;
        ZoneListSNLoadCoolEnergy(ListNum) = 0.0;
        ZoneListSNLoadHeatRate(ListNum) = 0.0;
        ZoneListSNLoadCoolRate(ListNum) = 0.0;
    }

    for (ListNum = 1; ListNum <= state.dataHeatBal->NumOfZoneLists; ++ListNum) {
        for (ZoneNum = 1; ZoneNum <= ZoneList(ListNum).NumOfZones; ++ZoneNum) {
            Mult = state.dataHeatBal->Zone(ZoneNum).Multiplier;
            ZoneListSNLoadHeatEnergy(ListNum) += state.dataHeatBal->ZoneSNLoadHeatEnergy(ZoneList(ListNum).Zone(ZoneNum)) * Mult;
            ZoneListSNLoadCoolEnergy(ListNum) += state.dataHeatBal->ZoneSNLoadCoolEnergy(ZoneList(ListNum).Zone(ZoneNum)) * Mult;
            ZoneListSNLoadHeatRate(ListNum) += state.dataHeatBal->ZoneSNLoadHeatRate(ZoneList(ListNum).Zone(ZoneNum)) * Mult;
            ZoneListSNLoadCoolRate(ListNum) += state.dataHeatBal->ZoneSNLoadCoolRate(ZoneList(ListNum).Zone(ZoneNum)) * Mult;
        } // ZoneNum
    }     // ListNum

    for (GroupNum = 1; GroupNum <= state.dataHeatBal->NumOfZoneGroups; ++GroupNum) {
        Mult = state.dataHeatBal->ZoneGroup(GroupNum).Multiplier;
        state.dataHeatBal->ZoneGroupSNLoadHeatEnergy(GroupNum) = ZoneListSNLoadHeatEnergy(ZoneGroup(GroupNum).ZoneList) * Mult;
        state.dataHeatBal->ZoneGroupSNLoadCoolEnergy(GroupNum) = ZoneListSNLoadCoolEnergy(ZoneGroup(GroupNum).ZoneList) * Mult;
        state.dataHeatBal->ZoneGroupSNLoadHeatRate(GroupNum) = ZoneListSNLoadHeatRate(ZoneGroup(GroupNum).ZoneList) * Mult;
        state.dataHeatBal->ZoneGroupSNLoadCoolRate(GroupNum) = ZoneListSNLoadCoolRate(ZoneGroup(GroupNum).ZoneList) * Mult;
    } // GroupNum
}

void ReportInfiltrations(EnergyPlusData &state)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Yueyue Zhou
    //       DATE WRITTEN   July 2021

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine currently creates the values for standard Infiltration object level reporting

    // METHODOLOGY EMPLOYED:

    // REFERENCES:

    using DataHVACGlobals::CycleOn;
    using DataHVACGlobals::CycleOnZoneFansOnly;
    using Psychrometrics::PsyCpAirFnW;
    using Psychrometrics::PsyHgAirFnWTdb;
    using Psychrometrics::PsyRhoAirFnPbTdbW;
    int j;  // Loop Counter
    int NZ; // A pointer
    // SUBROUTINE PARAMETER DEFINITIONS:
    static std::string const RoutineName("ReportInfiltrations");

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 AirDensity;          // Density of air (kg/m^3)
    Real64 CpAir;               // Heat capacity of air (J/kg-C)
    Real64 TotalLoad;           // Total loss or gain
    Real64 H2OHtOfVap;          // Heat of vaporization of air
    Real64 ADSCorrectionFactor; // Correction factor of air flow model values when ADS is simulated
    auto &Zone(state.dataHeatBal->Zone);
    auto &Infiltration(state.dataHeatBal->Infiltration);
    auto &TimeStepSys(state.dataHVACGlobal->TimeStepSys);

    for (j = 1; j <= state.dataHeatBal->TotInfiltration; ++j) {

        NZ = state.dataHeatBal->Infiltration(j).ZonePtr;
        ADSCorrectionFactor = 1.0;
        if (state.afn->SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlSimpleADS) {
            // CR7608 IF (TurnFansOn .AND. AirflowNetworkZoneFlag(NZ)) ADSCorrectionFactor=0
            if ((state.dataZoneEquip->ZoneEquipAvail(NZ) == CycleOn || state.dataZoneEquip->ZoneEquipAvail(NZ) == CycleOnZoneFansOnly) &&
                state.afn->AirflowNetworkZoneFlag(NZ))
                ADSCorrectionFactor = 0.0;
        }

        CpAir = PsyCpAirFnW(state.dataEnvrn->OutHumRat);
        Infiltration(j).InfilMdot = Infiltration(j).MCpI_temp / CpAir * ADSCorrectionFactor;
        Infiltration(j).InfilMass = Infiltration(j).InfilMdot * TimeStepSys * DataGlobalConstants::SecInHour;

        if (state.dataHeatBalFanSys->MAT(NZ) > Zone(NZ).OutDryBulbTemp) {

            Infiltration(j).InfilHeatLoss = Infiltration(j).MCpI_temp * (state.dataHeatBalFanSys->MAT(NZ) - Zone(NZ).OutDryBulbTemp) * TimeStepSys *
                                            DataGlobalConstants::SecInHour * ADSCorrectionFactor;
            Infiltration(j).InfilHeatGain = 0.0;

        } else if (state.dataHeatBalFanSys->MAT(NZ) <= Zone(NZ).OutDryBulbTemp) {

            Infiltration(j).InfilHeatGain = Infiltration(j).MCpI_temp * (Zone(NZ).OutDryBulbTemp - state.dataHeatBalFanSys->MAT(NZ)) * TimeStepSys *
                                            DataGlobalConstants::SecInHour * ADSCorrectionFactor;
            Infiltration(j).InfilHeatLoss = 0.0;
        }

        // Report infiltration latent gains and losses
        H2OHtOfVap = PsyHgAirFnWTdb(state.dataHeatBalFanSys->ZoneAirHumRat(NZ), state.dataHeatBalFanSys->MAT(NZ));
        if (state.dataHeatBalFanSys->ZoneAirHumRat(NZ) > state.dataEnvrn->OutHumRat) {

            Infiltration(j).InfilLatentLoss = Infiltration(j).InfilMdot * (state.dataHeatBalFanSys->ZoneAirHumRat(NZ) - state.dataEnvrn->OutHumRat) *
                                              H2OHtOfVap * TimeStepSys * DataGlobalConstants::SecInHour;
            Infiltration(j).InfilLatentGain = 0.0;

        } else if (state.dataHeatBalFanSys->ZoneAirHumRat(NZ) <= state.dataEnvrn->OutHumRat) {

            Infiltration(j).InfilLatentGain = Infiltration(j).InfilMdot * (state.dataEnvrn->OutHumRat - state.dataHeatBalFanSys->ZoneAirHumRat(NZ)) *
                                              H2OHtOfVap * TimeStepSys * DataGlobalConstants::SecInHour;
            Infiltration(j).InfilLatentLoss = 0.0;
        }
        // Total infiltration losses and gains
        TotalLoad = Infiltration(j).InfilHeatGain + Infiltration(j).InfilLatentGain - Infiltration(j).InfilHeatLoss - Infiltration(j).InfilLatentLoss;
        if (TotalLoad > 0) {
            Infiltration(j).InfilTotalGain = TotalLoad;
            Infiltration(j).InfilTotalLoss = 0.0;
        } else {
            Infiltration(j).InfilTotalGain = 0.0;
            Infiltration(j).InfilTotalLoss = -TotalLoad;
        }
        // CR7751  second, calculate using indoor conditions for density property
        AirDensity = PsyRhoAirFnPbTdbW(
            state, state.dataEnvrn->OutBaroPress, state.dataHeatBalFanSys->MAT(NZ), state.dataHeatBalFanSys->ZoneAirHumRatAvg(NZ), RoutineName);
        Infiltration(j).InfilVdotCurDensity = Infiltration(j).InfilMdot / AirDensity;
        Infiltration(j).InfilVolumeCurDensity = Infiltration(j).InfilVdotCurDensity * TimeStepSys * DataGlobalConstants::SecInHour;
        Infiltration(j).InfilAirChangeRate = Infiltration(j).InfilVolumeCurDensity / (TimeStepSys * Zone(NZ).Volume);

        // CR7751 third, calculate using standard dry air at nominal elevation
        AirDensity = state.dataEnvrn->StdRhoAir;
        Infiltration(j).InfilVdotStdDensity = Infiltration(j).InfilMdot / AirDensity;
        Infiltration(j).InfilVolumeStdDensity = Infiltration(j).InfilVdotStdDensity * TimeStepSys * DataGlobalConstants::SecInHour;
    }
}

void ReportAirHeatBalance(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   July 2000
    //       MODIFIED       Shirey, Jan 2008 (MIXING/CROSS MIXING outputs)
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine updates the report variables for the AirHeatBalance.

    // Using/Aliasing
    using DataHVACGlobals::CycleOn;
    using DataHVACGlobals::CycleOnZoneFansOnly;
    using DataHVACGlobals::FanType_ZoneExhaust;
    using Psychrometrics::PsyCpAirFnW;
    using Psychrometrics::PsyHgAirFnWTdb;
    using Psychrometrics::PsyRhoAirFnPbTdbW;

    // SUBROUTINE PARAMETER DEFINITIONS:
    static constexpr std::string_view RoutineName3("ReportAirHeatBalance:3");

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int ZoneLoop;                                     // Counter for the # of zones (nz)
    int ZoneA;                                        // Mated zone number for pair pf zones sharing refrigeration door opening
    int ZoneB;                                        // Mated zone number for pair pf zones sharing refrigeration door opening
    int VentNum;                                      // Counter for ventilation statements
    int FanNum;                                       // Counter for exhaust fans
    Real64 AirDensity;                                // Density of air (kg/m^3)
    Real64 CpAir;                                     // Heat capacity of air (J/kg-C)
    Real64 ADSCorrectionFactor;                       // Correction factor of air flow model values when ADS is simulated
    Real64 H2OHtOfVap;                                // Heat of vaporization of air
    Real64 TotalLoad;                                 // Total loss or gain
    int MixNum;                                       // Counter for MIXING and Cross Mixing statements
    auto &MixSenLoad = state.dataHVACMgr->MixSenLoad; // Mixing sensible loss or gain
    auto &MixLatLoad = state.dataHVACMgr->MixLatLoad; // Mixing latent loss or gain
    int j;                                            // Index in a do-loop
    int VentZoneNum;                                  // Number of ventilation object per zone
    Real64 VentZoneMassflow;                          // Total mass flow rate per zone
    Real64 VentZoneAirTemp;                           // Average Zone inlet temperature

    state.dataHeatBal->ZoneTotalExfiltrationHeatLoss = 0.0;
    state.dataHeatBal->ZoneTotalExhaustHeatLoss = 0.0;

    auto &Zone(state.dataHeatBal->Zone);
    auto &ZnAirRpt(state.dataHeatBal->ZnAirRpt);
    auto &Ventilation(state.dataHeatBal->Ventilation);
    auto &Mixing(state.dataHeatBal->Mixing);
    auto &CrossMixing(state.dataHeatBal->CrossMixing);
    auto &RefDoorMixing(state.dataHeatBal->RefDoorMixing);
    auto &ZoneEquipConfig(state.dataZoneEquip->ZoneEquipConfig);
    auto &Fan(state.dataFans->Fan);
    auto &TimeStepSys(state.dataHVACGlobal->TimeStepSys);

    // Ensure no airflownetwork and simple calculations
    if (state.afn->SimulateAirflowNetwork == 0) return;

    if (state.afn->SimulateAirflowNetwork > AirflowNetwork::AirflowNetworkControlSimple) state.afn->report();

    // Reports zone exhaust loss by exhaust fans
    for (ZoneLoop = 1; ZoneLoop <= state.dataGlobal->NumOfZones; ++ZoneLoop) { // Start of zone loads report variable update loop ...
        CpAir = PsyCpAirFnW(state.dataEnvrn->OutHumRat);
        H2OHtOfVap = PsyHgAirFnWTdb(state.dataEnvrn->OutHumRat, Zone(ZoneLoop).OutDryBulbTemp);
        ADSCorrectionFactor = 1.0;
        if (state.afn->SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlSimpleADS) {
            if ((state.dataZoneEquip->ZoneEquipAvail(ZoneLoop) == CycleOn || state.dataZoneEquip->ZoneEquipAvail(ZoneLoop) == CycleOnZoneFansOnly) &&
                state.afn->AirflowNetworkZoneFlag(ZoneLoop)) {
                ADSCorrectionFactor = 0.0;
            }
        }

        ZnAirRpt(ZoneLoop).ExhTotalLoss = 0;
        ZnAirRpt(ZoneLoop).ExhSensiLoss = 0;

        for (FanNum = 1; FanNum <= state.dataFans->NumFans; ++FanNum) {
            //  Add reportable vars
            if (Fan(FanNum).FanType_Num == FanType_ZoneExhaust) {
                for (int ExhNum = 1; ExhNum <= ZoneEquipConfig(ZoneLoop).NumExhaustNodes; ExhNum++) {
                    if (Fan(FanNum).InletNodeNum == ZoneEquipConfig(ZoneLoop).ExhaustNode(ExhNum)) {
                        ZnAirRpt(ZoneLoop).ExhTotalLoss +=
                            Fan(FanNum).OutletAirMassFlowRate * (Fan(FanNum).OutletAirEnthalpy - state.dataEnvrn->OutEnthalpy) * ADSCorrectionFactor;
                        ZnAirRpt(ZoneLoop).ExhSensiLoss += Fan(FanNum).OutletAirMassFlowRate * CpAir *
                                                           (Fan(FanNum).OutletAirTemp - Zone(ZoneLoop).OutDryBulbTemp) * ADSCorrectionFactor;
                        break;
                    }
                }
            }
        }

        ZnAirRpt(ZoneLoop).ExhLatentLoss = ZnAirRpt(ZoneLoop).ExhTotalLoss - ZnAirRpt(ZoneLoop).ExhSensiLoss;
    }

    // Report results for SIMPLE option only
    if (!(state.afn->SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlSimple ||
          state.afn->SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlSimpleADS))
        return;

    if (state.dataHVACMgr->ReportAirHeatBalanceFirstTimeFlag) {
        MixSenLoad.allocate(state.dataGlobal->NumOfZones);
        MixLatLoad.allocate(state.dataGlobal->NumOfZones);
        state.dataHVACMgr->ReportAirHeatBalanceFirstTimeFlag = false;
    }

    ReportInfiltrations(state);

    for (ZoneLoop = 1; ZoneLoop <= state.dataGlobal->NumOfZones; ++ZoneLoop) { // Start of zone loads report variable update loop ...

        // Break the infiltration load into heat gain and loss components
        ADSCorrectionFactor = 1.0;

        if (state.afn->SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlSimpleADS) {
            // CR7608 IF (TurnFansOn .AND. AirflowNetworkZoneFlag(ZoneLoop)) ADSCorrectionFactor=0
            if ((state.dataZoneEquip->ZoneEquipAvail(ZoneLoop) == CycleOn || state.dataZoneEquip->ZoneEquipAvail(ZoneLoop) == CycleOnZoneFansOnly) &&
                state.afn->AirflowNetworkZoneFlag(ZoneLoop))
                ADSCorrectionFactor = 0.0;
        }

        if (state.dataHeatBalFanSys->MAT(ZoneLoop) > Zone(ZoneLoop).OutDryBulbTemp) {

            ZnAirRpt(ZoneLoop).InfilHeatLoss = state.dataHeatBalFanSys->MCPI(ZoneLoop) *
                                               (state.dataHeatBalFanSys->MAT(ZoneLoop) - Zone(ZoneLoop).OutDryBulbTemp) * TimeStepSys *
                                               DataGlobalConstants::SecInHour * ADSCorrectionFactor;
            ZnAirRpt(ZoneLoop).InfilHeatGain = 0.0;

        } else if (state.dataHeatBalFanSys->MAT(ZoneLoop) <= Zone(ZoneLoop).OutDryBulbTemp) {

            ZnAirRpt(ZoneLoop).InfilHeatGain = state.dataHeatBalFanSys->MCPI(ZoneLoop) *
                                               (Zone(ZoneLoop).OutDryBulbTemp - state.dataHeatBalFanSys->MAT(ZoneLoop)) * TimeStepSys *
                                               DataGlobalConstants::SecInHour * ADSCorrectionFactor;
            ZnAirRpt(ZoneLoop).InfilHeatLoss = 0.0;
        }
        // Report infiltration latent gains and losses
        CpAir = PsyCpAirFnW(state.dataEnvrn->OutHumRat);
        H2OHtOfVap = PsyHgAirFnWTdb(state.dataHeatBalFanSys->ZoneAirHumRat(ZoneLoop), state.dataHeatBalFanSys->MAT(ZoneLoop));
        if (state.dataHeatBalFanSys->ZoneAirHumRat(ZoneLoop) > state.dataEnvrn->OutHumRat) {

            ZnAirRpt(ZoneLoop).InfilLatentLoss = state.dataHeatBalFanSys->MCPI(ZoneLoop) / CpAir *
                                                 (state.dataHeatBalFanSys->ZoneAirHumRat(ZoneLoop) - state.dataEnvrn->OutHumRat) * H2OHtOfVap *
                                                 TimeStepSys * DataGlobalConstants::SecInHour * ADSCorrectionFactor;
            ZnAirRpt(ZoneLoop).InfilLatentGain = 0.0;

        } else if (state.dataHeatBalFanSys->ZoneAirHumRat(ZoneLoop) <= state.dataEnvrn->OutHumRat) {

            ZnAirRpt(ZoneLoop).InfilLatentGain = state.dataHeatBalFanSys->MCPI(ZoneLoop) / CpAir *
                                                 (state.dataEnvrn->OutHumRat - state.dataHeatBalFanSys->ZoneAirHumRat(ZoneLoop)) * H2OHtOfVap *
                                                 TimeStepSys * DataGlobalConstants::SecInHour * ADSCorrectionFactor;
            ZnAirRpt(ZoneLoop).InfilLatentLoss = 0.0;
        }
        // Total infiltration losses and gains
        TotalLoad = ZnAirRpt(ZoneLoop).InfilHeatGain + ZnAirRpt(ZoneLoop).InfilLatentGain - ZnAirRpt(ZoneLoop).InfilHeatLoss -
                    ZnAirRpt(ZoneLoop).InfilLatentLoss;
        if (TotalLoad > 0) {
            ZnAirRpt(ZoneLoop).InfilTotalGain = TotalLoad * ADSCorrectionFactor;
            ZnAirRpt(ZoneLoop).InfilTotalLoss = 0.0;
        } else {
            ZnAirRpt(ZoneLoop).InfilTotalGain = 0.0;
            ZnAirRpt(ZoneLoop).InfilTotalLoss = -TotalLoad * ADSCorrectionFactor;
        }

        // first calculate mass flows using outside air heat capacity for consistency with input to heat balance
        ZnAirRpt(ZoneLoop).InfilMdot = (state.dataHeatBalFanSys->MCPI(ZoneLoop) / CpAir) * ADSCorrectionFactor;
        ZnAirRpt(ZoneLoop).InfilMass = ZnAirRpt(ZoneLoop).InfilMdot * TimeStepSys * DataGlobalConstants::SecInHour;
        ZnAirRpt(ZoneLoop).VentilMdot = (state.dataHeatBalFanSys->MCPV(ZoneLoop) / CpAir) * ADSCorrectionFactor;
        ZnAirRpt(ZoneLoop).VentilMass = ZnAirRpt(ZoneLoop).VentilMdot * TimeStepSys * DataGlobalConstants::SecInHour;

        // CR7751  second, calculate using indoor conditions for density property
        AirDensity = PsyRhoAirFnPbTdbW(state,
                                       state.dataEnvrn->OutBaroPress,
                                       state.dataHeatBalFanSys->MAT(ZoneLoop),
                                       state.dataHeatBalFanSys->ZoneAirHumRatAvg(ZoneLoop),
                                       RoutineName3);
        ZnAirRpt(ZoneLoop).InfilVdotCurDensity = ZnAirRpt(ZoneLoop).InfilMdot / AirDensity;
        ZnAirRpt(ZoneLoop).InfilVolumeCurDensity = ZnAirRpt(ZoneLoop).InfilVdotCurDensity * TimeStepSys * DataGlobalConstants::SecInHour;
        ZnAirRpt(ZoneLoop).InfilAirChangeRate = ZnAirRpt(ZoneLoop).InfilVolumeCurDensity / (TimeStepSys * Zone(ZoneLoop).Volume);
        ZnAirRpt(ZoneLoop).VentilVdotCurDensity = ZnAirRpt(ZoneLoop).VentilMdot / AirDensity;
        ZnAirRpt(ZoneLoop).VentilVolumeCurDensity = ZnAirRpt(ZoneLoop).VentilVdotCurDensity * TimeStepSys * DataGlobalConstants::SecInHour;
        ZnAirRpt(ZoneLoop).VentilAirChangeRate = ZnAirRpt(ZoneLoop).VentilVolumeCurDensity / (TimeStepSys * Zone(ZoneLoop).Volume);

        // CR7751 third, calculate using standard dry air at nominal elevation
        AirDensity = state.dataEnvrn->StdRhoAir;
        ZnAirRpt(ZoneLoop).InfilVdotStdDensity = ZnAirRpt(ZoneLoop).InfilMdot / AirDensity;
        ZnAirRpt(ZoneLoop).InfilVolumeStdDensity = ZnAirRpt(ZoneLoop).InfilVdotStdDensity * TimeStepSys * DataGlobalConstants::SecInHour;
        ZnAirRpt(ZoneLoop).VentilVdotStdDensity = ZnAirRpt(ZoneLoop).VentilMdot / AirDensity;
        ZnAirRpt(ZoneLoop).VentilVolumeStdDensity = ZnAirRpt(ZoneLoop).VentilVdotStdDensity * TimeStepSys * DataGlobalConstants::SecInHour;

        //    ZnAirRpt(ZoneLoop)%VentilFanElec = 0.0
        ZnAirRpt(ZoneLoop).VentilAirTemp = 0.0;
        ZnAirRpt(ZoneLoop).VentilHeatLoss = 0.0;
        ZnAirRpt(ZoneLoop).VentilHeatGain = 0.0;
        VentZoneNum = 0;
        VentZoneMassflow = 0.0;
        VentZoneAirTemp = 0.0;

        for (VentNum = 1; VentNum <= state.dataHeatBal->TotVentilation; ++VentNum) {
            if (Ventilation(VentNum).ZonePtr == ZoneLoop) {
                // moved into CalcAirFlowSimple
                //        ZnAirRpt(ZoneLoop)%VentilFanElec  =
                //        ZnAirRpt(ZoneLoop)%VentilFanElec+Ventilation(VentNum)%FanPower*TimeStepSys*DataGlobalConstants::SecInHour()
                //        &
                //          *ADSCorrectionFactor
                if (ADSCorrectionFactor > 0) {
                    ZnAirRpt(ZoneLoop).VentilAirTemp += Ventilation(VentNum).AirTemp * state.dataZoneEquip->VentMCP(VentNum);
                    VentZoneMassflow += state.dataZoneEquip->VentMCP(VentNum);
                    VentZoneAirTemp += Ventilation(VentNum).AirTemp;
                } else {
                    ZnAirRpt(ZoneLoop).VentilAirTemp = Zone(ZoneLoop).OutDryBulbTemp;
                }
                // Break the ventilation load into heat gain and loss components
                if (state.dataHeatBalFanSys->MAT(ZoneLoop) > Ventilation(VentNum).AirTemp) {
                    ZnAirRpt(ZoneLoop).VentilHeatLoss += state.dataZoneEquip->VentMCP(VentNum) *
                                                         (state.dataHeatBalFanSys->MAT(ZoneLoop) - Ventilation(VentNum).AirTemp) * TimeStepSys *
                                                         DataGlobalConstants::SecInHour * ADSCorrectionFactor;
                } else if (state.dataHeatBalFanSys->MAT(ZoneLoop) <= Ventilation(VentNum).AirTemp) {
                    ZnAirRpt(ZoneLoop).VentilHeatGain += state.dataZoneEquip->VentMCP(VentNum) *
                                                         (Ventilation(VentNum).AirTemp - state.dataHeatBalFanSys->MAT(ZoneLoop)) * TimeStepSys *
                                                         DataGlobalConstants::SecInHour * ADSCorrectionFactor;
                }

                ++VentZoneNum;
                if (VentZoneNum > 1) continue;

                // Report ventilation latent gains and losses
                H2OHtOfVap = PsyHgAirFnWTdb(state.dataHeatBalFanSys->ZoneAirHumRat(ZoneLoop), state.dataHeatBalFanSys->MAT(ZoneLoop));
                if (state.dataHeatBalFanSys->ZoneAirHumRat(ZoneLoop) > state.dataEnvrn->OutHumRat) {
                    ZnAirRpt(ZoneLoop).VentilLatentLoss = ZnAirRpt(ZoneLoop).VentilMdot *
                                                          (state.dataHeatBalFanSys->ZoneAirHumRat(ZoneLoop) - state.dataEnvrn->OutHumRat) *
                                                          H2OHtOfVap * TimeStepSys * DataGlobalConstants::SecInHour;
                    ZnAirRpt(ZoneLoop).VentilLatentGain = 0.0;
                } else if (state.dataHeatBalFanSys->ZoneAirHumRat(ZoneLoop) <= state.dataEnvrn->OutHumRat) {
                    ZnAirRpt(ZoneLoop).VentilLatentGain = ZnAirRpt(ZoneLoop).VentilMdot *
                                                          (state.dataEnvrn->OutHumRat - state.dataHeatBalFanSys->ZoneAirHumRat(ZoneLoop)) *
                                                          H2OHtOfVap * TimeStepSys * DataGlobalConstants::SecInHour;
                    ZnAirRpt(ZoneLoop).VentilLatentLoss = 0.0;
                }
                // Total ventilation losses and gains
                TotalLoad = ZnAirRpt(ZoneLoop).VentilHeatGain + ZnAirRpt(ZoneLoop).VentilLatentGain - ZnAirRpt(ZoneLoop).VentilHeatLoss -
                            ZnAirRpt(ZoneLoop).VentilLatentLoss;
                if (TotalLoad > 0) {
                    ZnAirRpt(ZoneLoop).VentilTotalGain = TotalLoad * ADSCorrectionFactor;
                    ZnAirRpt(ZoneLoop).VentilTotalLoss = 0.0;
                } else {
                    ZnAirRpt(ZoneLoop).VentilTotalGain = 0.0;
                    ZnAirRpt(ZoneLoop).VentilTotalLoss = -TotalLoad * ADSCorrectionFactor;
                }
            }
        }

        if (ADSCorrectionFactor > 0 && VentZoneNum > 1 && VentZoneMassflow > 0.0) {
            ZnAirRpt(ZoneLoop).VentilAirTemp /= VentZoneMassflow;
        } else if (ADSCorrectionFactor > 0 && VentZoneNum == 1) {
            ZnAirRpt(ZoneLoop).VentilAirTemp = VentZoneAirTemp;
        } else { // Just in case
            ZnAirRpt(ZoneLoop).VentilAirTemp = Zone(ZoneLoop).OutDryBulbTemp;
        }

        // Report mixing sensible and latent loads
        MixSenLoad(ZoneLoop) = 0.0; // Initialize arrays to zero before starting to sum
        MixLatLoad(ZoneLoop) = 0.0;
        ZnAirRpt(ZoneLoop).MixVolume = 0.0;         // zero reported volume prior to summations below
        ZnAirRpt(ZoneLoop).MixVdotCurDensity = 0.0; // zero reported volume flow rate prior to summations below
        ZnAirRpt(ZoneLoop).MixVdotStdDensity = 0.0; // zero reported volume flow rate prior to summations below
        ZnAirRpt(ZoneLoop).MixMass = 0.0;           // ! zero reported mass prior to summations below
        ZnAirRpt(ZoneLoop).MixMdot = 0.0;           // ! zero reported mass flow rate prior to summations below
        //    MixingLoad = 0.0d0

        for (MixNum = 1; MixNum <= state.dataHeatBal->TotMixing; ++MixNum) {
            if ((Mixing(MixNum).ZonePtr == ZoneLoop) && state.dataZoneEquip->MixingReportFlag(MixNum)) {
                //        MixSenLoad(ZoneLoop) = MixSenLoad(ZoneLoop)+MCPM(ZoneLoop)*MAT(Mixing(MixNum)%FromZone)
                //        H2OHtOfVap = PsyHgAirFnWTdb(ZoneAirHumRat(ZoneLoop), MAT(ZoneLoop))
                //        Per Jan 17, 2008 conference call, agreed to use average conditions for Rho, Cp and Hfg
                //           and to recalculate the report variable using end of time step temps and humrats
                AirDensity = PsyRhoAirFnPbTdbW(
                    state,
                    state.dataEnvrn->OutBaroPress,
                    (state.dataHeatBalFanSys->MAT(ZoneLoop) + state.dataHeatBalFanSys->MAT(Mixing(MixNum).FromZone)) / 2.0,
                    (state.dataHeatBalFanSys->ZoneAirHumRat(ZoneLoop) + state.dataHeatBalFanSys->ZoneAirHumRat(Mixing(MixNum).FromZone)) / 2.0,
                    std::string());
                CpAir = PsyCpAirFnW(
                    (state.dataHeatBalFanSys->ZoneAirHumRat(ZoneLoop) + state.dataHeatBalFanSys->ZoneAirHumRat(Mixing(MixNum).FromZone)) / 2.0);
                ZnAirRpt(ZoneLoop).MixVolume +=
                    Mixing(MixNum).DesiredAirFlowRate * TimeStepSys * DataGlobalConstants::SecInHour * ADSCorrectionFactor;
                ZnAirRpt(ZoneLoop).MixVdotCurDensity += Mixing(MixNum).DesiredAirFlowRate * ADSCorrectionFactor;
                ZnAirRpt(ZoneLoop).MixMass +=
                    Mixing(MixNum).DesiredAirFlowRate * AirDensity * TimeStepSys * DataGlobalConstants::SecInHour * ADSCorrectionFactor;
                ZnAirRpt(ZoneLoop).MixMdot += Mixing(MixNum).DesiredAirFlowRate * AirDensity * ADSCorrectionFactor;
                ZnAirRpt(ZoneLoop).MixVdotStdDensity +=
                    Mixing(MixNum).DesiredAirFlowRate * (AirDensity / state.dataEnvrn->StdRhoAir) * ADSCorrectionFactor;
                MixSenLoad(ZoneLoop) += Mixing(MixNum).DesiredAirFlowRate * AirDensity * CpAir *
                                        (state.dataHeatBalFanSys->MAT(ZoneLoop) - state.dataHeatBalFanSys->MAT(Mixing(MixNum).FromZone));
                H2OHtOfVap = PsyHgAirFnWTdb(
                    (state.dataHeatBalFanSys->ZoneAirHumRat(ZoneLoop) + state.dataHeatBalFanSys->ZoneAirHumRat(Mixing(MixNum).FromZone)) / 2.0,
                    (state.dataHeatBalFanSys->MAT(ZoneLoop) + state.dataHeatBalFanSys->MAT(Mixing(MixNum).FromZone)) / 2.0);
                //        MixLatLoad(ZoneLoop) = MixLatLoad(ZoneLoop)+MixingMassFlowZone(ZoneLoop)*(ZoneAirHumRat(ZoneLoop)- &
                //                     ZoneAirHumRat(Mixing(MixNum)%FromZone))*H2OHtOfVap
                MixLatLoad(ZoneLoop) +=
                    Mixing(MixNum).DesiredAirFlowRate * AirDensity *
                    (state.dataHeatBalFanSys->ZoneAirHumRat(ZoneLoop) - state.dataHeatBalFanSys->ZoneAirHumRat(Mixing(MixNum).FromZone)) * H2OHtOfVap;
            }
        }

        for (MixNum = 1; MixNum <= state.dataHeatBal->TotCrossMixing; ++MixNum) {
            if ((CrossMixing(MixNum).ZonePtr == ZoneLoop) && state.dataZoneEquip->CrossMixingReportFlag(MixNum)) {
                //        MixSenLoad(ZoneLoop) = MixSenLoad(ZoneLoop)+MCPM(ZoneLoop)*MAT(CrossMixing(MixNum)%FromZone)
                //        Per Jan 17, 2008 conference call, agreed to use average conditions for Rho, Cp and Hfg
                //           and to recalculate the report variable using end of time step temps and humrats
                AirDensity = PsyRhoAirFnPbTdbW(
                    state,
                    state.dataEnvrn->OutBaroPress,
                    (state.dataHeatBalFanSys->MAT(ZoneLoop) + state.dataHeatBalFanSys->MAT(CrossMixing(MixNum).FromZone)) / 2.0,
                    (state.dataHeatBalFanSys->ZoneAirHumRat(ZoneLoop) + state.dataHeatBalFanSys->ZoneAirHumRat(CrossMixing(MixNum).FromZone)) / 2.0,
                    std::string());
                CpAir = PsyCpAirFnW(
                    (state.dataHeatBalFanSys->ZoneAirHumRat(ZoneLoop) + state.dataHeatBalFanSys->ZoneAirHumRat(CrossMixing(MixNum).FromZone)) / 2.0);
                ZnAirRpt(ZoneLoop).MixVolume +=
                    CrossMixing(MixNum).DesiredAirFlowRate * TimeStepSys * DataGlobalConstants::SecInHour * ADSCorrectionFactor;
                ZnAirRpt(ZoneLoop).MixVdotCurDensity += CrossMixing(MixNum).DesiredAirFlowRate * ADSCorrectionFactor;
                ZnAirRpt(ZoneLoop).MixMass +=
                    CrossMixing(MixNum).DesiredAirFlowRate * AirDensity * TimeStepSys * DataGlobalConstants::SecInHour * ADSCorrectionFactor;
                ZnAirRpt(ZoneLoop).MixMdot += CrossMixing(MixNum).DesiredAirFlowRate * AirDensity * ADSCorrectionFactor;
                ZnAirRpt(ZoneLoop).MixVdotStdDensity +=
                    CrossMixing(MixNum).DesiredAirFlowRate * (AirDensity / state.dataEnvrn->StdRhoAir) * ADSCorrectionFactor;
                MixSenLoad(ZoneLoop) += CrossMixing(MixNum).DesiredAirFlowRate * AirDensity * CpAir *
                                        (state.dataHeatBalFanSys->MAT(ZoneLoop) - state.dataHeatBalFanSys->MAT(CrossMixing(MixNum).FromZone));
                H2OHtOfVap = PsyHgAirFnWTdb(
                    (state.dataHeatBalFanSys->ZoneAirHumRat(ZoneLoop) + state.dataHeatBalFanSys->ZoneAirHumRat(CrossMixing(MixNum).FromZone)) / 2.0,
                    (state.dataHeatBalFanSys->MAT(ZoneLoop) + state.dataHeatBalFanSys->MAT(CrossMixing(MixNum).FromZone)) / 2.0);
                //       MixLatLoad(ZoneLoop) = MixLatLoad(ZoneLoop)+MixingMassFlowZone(ZoneLoop)*(ZoneAirHumRat(ZoneLoop)- &
                //                     ZoneAirHumRat(CrossMixing(MixNum)%FromZone))*H2OHtOfVap
                MixLatLoad(ZoneLoop) +=
                    CrossMixing(MixNum).DesiredAirFlowRate * AirDensity *
                    (state.dataHeatBalFanSys->ZoneAirHumRat(ZoneLoop) - state.dataHeatBalFanSys->ZoneAirHumRat(CrossMixing(MixNum).FromZone)) *
                    H2OHtOfVap;
            }
            if ((CrossMixing(MixNum).FromZone == ZoneLoop) && state.dataZoneEquip->CrossMixingReportFlag(MixNum)) {
                AirDensity = PsyRhoAirFnPbTdbW(
                    state,
                    state.dataEnvrn->OutBaroPress,
                    (state.dataHeatBalFanSys->MAT(ZoneLoop) + state.dataHeatBalFanSys->MAT(CrossMixing(MixNum).ZonePtr)) / 2.0,
                    (state.dataHeatBalFanSys->ZoneAirHumRat(ZoneLoop) + state.dataHeatBalFanSys->ZoneAirHumRat(CrossMixing(MixNum).ZonePtr)) / 2.0,
                    std::string());
                CpAir = PsyCpAirFnW(
                    (state.dataHeatBalFanSys->ZoneAirHumRat(ZoneLoop) + state.dataHeatBalFanSys->ZoneAirHumRat(CrossMixing(MixNum).ZonePtr)) / 2.0);
                ZnAirRpt(ZoneLoop).MixVolume +=
                    CrossMixing(MixNum).DesiredAirFlowRate * TimeStepSys * DataGlobalConstants::SecInHour * ADSCorrectionFactor;
                ZnAirRpt(ZoneLoop).MixVdotCurDensity += CrossMixing(MixNum).DesiredAirFlowRate * ADSCorrectionFactor;
                ZnAirRpt(ZoneLoop).MixMass +=
                    CrossMixing(MixNum).DesiredAirFlowRate * AirDensity * TimeStepSys * DataGlobalConstants::SecInHour * ADSCorrectionFactor;
                ZnAirRpt(ZoneLoop).MixMdot += CrossMixing(MixNum).DesiredAirFlowRate * AirDensity * ADSCorrectionFactor;
                ZnAirRpt(ZoneLoop).MixVdotStdDensity +=
                    CrossMixing(MixNum).DesiredAirFlowRate * (AirDensity / state.dataEnvrn->StdRhoAir) * ADSCorrectionFactor;
                MixSenLoad(ZoneLoop) += CrossMixing(MixNum).DesiredAirFlowRate * AirDensity * CpAir *
                                        (state.dataHeatBalFanSys->MAT(ZoneLoop) - state.dataHeatBalFanSys->MAT(CrossMixing(MixNum).ZonePtr));
                H2OHtOfVap = PsyHgAirFnWTdb(
                    (state.dataHeatBalFanSys->ZoneAirHumRat(ZoneLoop) + state.dataHeatBalFanSys->ZoneAirHumRat(CrossMixing(MixNum).ZonePtr)) / 2.0,
                    (state.dataHeatBalFanSys->MAT(ZoneLoop) + state.dataHeatBalFanSys->MAT(CrossMixing(MixNum).ZonePtr)) / 2.0);
                MixLatLoad(ZoneLoop) +=
                    CrossMixing(MixNum).DesiredAirFlowRate * AirDensity *
                    (state.dataHeatBalFanSys->ZoneAirHumRat(ZoneLoop) - state.dataHeatBalFanSys->ZoneAirHumRat(CrossMixing(MixNum).ZonePtr)) *
                    H2OHtOfVap;
            }
        }

        if (state.dataHeatBal->TotRefDoorMixing > 0) {
            // IF(ZoneLoop .NE. NumOfZones)THEN  !Refrigeration Door Mixing
            // Note - do each Pair a Single time, so must do increment reports for both zones
            //       Can't have a pair that has ZoneA zone number = NumOfZones because organized
            //       in input with lowest zone # first no matter how input in idf
            if (RefDoorMixing(ZoneLoop).RefDoorMixFlag) { // .TRUE. for both zoneA and zoneB
                if (RefDoorMixing(ZoneLoop).ZonePtr == ZoneLoop) {
                    for (j = 1; j <= RefDoorMixing(ZoneLoop).NumRefDoorConnections; ++j) {
                        //    Capture impact when zoneloop is the 'primary zone'
                        //    that is, the zone of a pair with the lower zone number
                        if (RefDoorMixing(ZoneLoop).VolRefDoorFlowRate(j) > 0.0) {
                            ZoneB = RefDoorMixing(ZoneLoop).MateZonePtr(j);
                            AirDensity = PsyRhoAirFnPbTdbW(
                                state,
                                state.dataEnvrn->OutBaroPress,
                                (state.dataHeatBalFanSys->MAT(ZoneLoop) + state.dataHeatBalFanSys->MAT(ZoneB)) / 2.0,
                                (state.dataHeatBalFanSys->ZoneAirHumRat(ZoneLoop) + state.dataHeatBalFanSys->ZoneAirHumRat(ZoneB)) / 2.0,
                                std::string());
                            CpAir =
                                PsyCpAirFnW((state.dataHeatBalFanSys->ZoneAirHumRat(ZoneLoop) + state.dataHeatBalFanSys->ZoneAirHumRat(ZoneB)) / 2.0);
                            H2OHtOfVap = PsyHgAirFnWTdb(
                                (state.dataHeatBalFanSys->ZoneAirHumRat(ZoneLoop) + state.dataHeatBalFanSys->ZoneAirHumRat(ZoneB)) / 2.0,
                                (state.dataHeatBalFanSys->MAT(ZoneLoop) + state.dataHeatBalFanSys->MAT(ZoneB)) / 2.0);
                            ZnAirRpt(ZoneLoop).MixVolume +=
                                RefDoorMixing(ZoneLoop).VolRefDoorFlowRate(j) * TimeStepSys * DataGlobalConstants::SecInHour * ADSCorrectionFactor;
                            ZnAirRpt(ZoneLoop).MixVdotCurDensity += RefDoorMixing(ZoneLoop).VolRefDoorFlowRate(j) * ADSCorrectionFactor;
                            ZnAirRpt(ZoneLoop).MixMass += RefDoorMixing(ZoneLoop).VolRefDoorFlowRate(j) * AirDensity * TimeStepSys *
                                                          DataGlobalConstants::SecInHour * ADSCorrectionFactor;
                            ZnAirRpt(ZoneLoop).MixMdot += RefDoorMixing(ZoneLoop).VolRefDoorFlowRate(j) * AirDensity * ADSCorrectionFactor;
                            ZnAirRpt(ZoneLoop).MixVdotStdDensity +=
                                RefDoorMixing(ZoneLoop).VolRefDoorFlowRate(j) * (AirDensity / state.dataEnvrn->StdRhoAir) * ADSCorrectionFactor;
                            MixSenLoad(ZoneLoop) += RefDoorMixing(ZoneLoop).VolRefDoorFlowRate(j) * AirDensity * CpAir *
                                                    (state.dataHeatBalFanSys->MAT(ZoneLoop) - state.dataHeatBalFanSys->MAT(ZoneB));
                            MixLatLoad(ZoneLoop) +=
                                RefDoorMixing(ZoneLoop).VolRefDoorFlowRate(j) * AirDensity *
                                (state.dataHeatBalFanSys->ZoneAirHumRat(ZoneLoop) - state.dataHeatBalFanSys->ZoneAirHumRat(ZoneB)) * H2OHtOfVap;
                        } // flow > 0
                    }     // J-1, numref connections
                }         // zone A (zoneptr = zoneloop)
                for (ZoneA = 1; ZoneA <= (ZoneLoop - 1); ++ZoneA) {
                    //    Capture impact when zoneloop is the 'mating zone'
                    //    that is, the zone of a pair with the higher zone number(matezoneptr = zoneloop)
                    if (RefDoorMixing(ZoneA).RefDoorMixFlag) {
                        for (j = 1; j <= RefDoorMixing(ZoneA).NumRefDoorConnections; ++j) {
                            if (RefDoorMixing(ZoneA).MateZonePtr(j) == ZoneLoop) {
                                if (RefDoorMixing(ZoneA).VolRefDoorFlowRate(j) > 0.0) {
                                    AirDensity = PsyRhoAirFnPbTdbW(
                                        state,
                                        state.dataEnvrn->OutBaroPress,
                                        (state.dataHeatBalFanSys->MAT(ZoneLoop) + state.dataHeatBalFanSys->MAT(ZoneA)) / 2.0,
                                        (state.dataHeatBalFanSys->ZoneAirHumRat(ZoneLoop) + state.dataHeatBalFanSys->ZoneAirHumRat(ZoneA)) / 2.0,
                                        std::string());
                                    CpAir = PsyCpAirFnW(
                                        (state.dataHeatBalFanSys->ZoneAirHumRat(ZoneLoop) + state.dataHeatBalFanSys->ZoneAirHumRat(ZoneA)) / 2.0);
                                    H2OHtOfVap = PsyHgAirFnWTdb(
                                        (state.dataHeatBalFanSys->ZoneAirHumRat(ZoneLoop) + state.dataHeatBalFanSys->ZoneAirHumRat(ZoneA)) / 2.0,
                                        (state.dataHeatBalFanSys->MAT(ZoneLoop) + state.dataHeatBalFanSys->MAT(ZoneA)) / 2.0);
                                    ZnAirRpt(ZoneLoop).MixVolume += RefDoorMixing(ZoneA).VolRefDoorFlowRate(j) * TimeStepSys *
                                                                    DataGlobalConstants::SecInHour * ADSCorrectionFactor;
                                    ZnAirRpt(ZoneLoop).MixVdotCurDensity += RefDoorMixing(ZoneA).VolRefDoorFlowRate(j) * ADSCorrectionFactor;
                                    ZnAirRpt(ZoneLoop).MixMass += RefDoorMixing(ZoneA).VolRefDoorFlowRate(j) * AirDensity * TimeStepSys *
                                                                  DataGlobalConstants::SecInHour * ADSCorrectionFactor;
                                    ZnAirRpt(ZoneLoop).MixMdot += RefDoorMixing(ZoneA).VolRefDoorFlowRate(j) * AirDensity * ADSCorrectionFactor;
                                    ZnAirRpt(ZoneLoop).MixVdotStdDensity +=
                                        RefDoorMixing(ZoneA).VolRefDoorFlowRate(j) * (AirDensity / state.dataEnvrn->StdRhoAir) * ADSCorrectionFactor;
                                    MixSenLoad(ZoneLoop) += RefDoorMixing(ZoneA).VolRefDoorFlowRate(j) * AirDensity * CpAir *
                                                            (state.dataHeatBalFanSys->MAT(ZoneLoop) - state.dataHeatBalFanSys->MAT(ZoneA));
                                    MixLatLoad(ZoneLoop) +=
                                        RefDoorMixing(ZoneA).VolRefDoorFlowRate(j) * AirDensity *
                                        (state.dataHeatBalFanSys->ZoneAirHumRat(ZoneLoop) - state.dataHeatBalFanSys->ZoneAirHumRat(ZoneA)) *
                                        H2OHtOfVap;
                                } // volflowrate > 0
                            }     // matezoneptr (zoneB) = Zonelooop
                        }         // NumRefDoorConnections
                    }             // Refdoormix flag on ZoneA
                }                 // zone A from 1 to (zoneloop - 1)
            }                     // Refdoormix flag on zoneloop
        }                         //(TotRefDoorMixing .GT. 0)
        // end refrigeration door mixing reports

        //    MixingLoad(ZoneLoop) = MCPM(ZoneLoop)*MAT(ZoneLoop) - MixSenLoad(ZoneLoop)
        if (MixSenLoad(ZoneLoop) > 0.0) {
            ZnAirRpt(ZoneLoop).MixHeatLoss = MixSenLoad(ZoneLoop) * TimeStepSys * DataGlobalConstants::SecInHour * ADSCorrectionFactor;
            ZnAirRpt(ZoneLoop).MixHeatGain = 0.0;
        } else {
            ZnAirRpt(ZoneLoop).MixHeatLoss = 0.0;
            ZnAirRpt(ZoneLoop).MixHeatGain = -MixSenLoad(ZoneLoop) * TimeStepSys * DataGlobalConstants::SecInHour * ADSCorrectionFactor;
        }
        // Report mixing latent loads
        //    MixingLoad(ZoneLoop) = MixLatLoad(ZoneLoop)
        if (MixLatLoad(ZoneLoop) > 0.0) {
            ZnAirRpt(ZoneLoop).MixLatentLoss = MixLatLoad(ZoneLoop) * TimeStepSys * DataGlobalConstants::SecInHour * ADSCorrectionFactor;
            ZnAirRpt(ZoneLoop).MixLatentGain = 0.0;
        } else {
            ZnAirRpt(ZoneLoop).MixLatentLoss = 0.0;
            ZnAirRpt(ZoneLoop).MixLatentGain = -MixLatLoad(ZoneLoop) * TimeStepSys * DataGlobalConstants::SecInHour * ADSCorrectionFactor;
        }
        // Total Mixing losses and gains
        TotalLoad =
            ZnAirRpt(ZoneLoop).MixHeatGain + ZnAirRpt(ZoneLoop).MixLatentGain - ZnAirRpt(ZoneLoop).MixHeatLoss - ZnAirRpt(ZoneLoop).MixLatentLoss;
        if (TotalLoad > 0) {
            ZnAirRpt(ZoneLoop).MixTotalGain = TotalLoad * ADSCorrectionFactor;
            ZnAirRpt(ZoneLoop).MixTotalLoss = 0.0;
        } else {
            ZnAirRpt(ZoneLoop).MixTotalGain = 0.0;
            ZnAirRpt(ZoneLoop).MixTotalLoss = -TotalLoad * ADSCorrectionFactor;
        }

        // Reporting combined outdoor air flows
        for (j = 1; j <= state.dataHeatBal->TotZoneAirBalance; ++j) {
            if (state.dataHeatBal->ZoneAirBalance(j).BalanceMethod == DataHeatBalance::AirBalance::Quadrature &&
                ZoneLoop == state.dataHeatBal->ZoneAirBalance(j).ZonePtr) {
                if (state.dataHeatBalFanSys->MAT(ZoneLoop) > Zone(ZoneLoop).OutDryBulbTemp) {
                    ZnAirRpt(ZoneLoop).OABalanceHeatLoss = state.dataHeatBalFanSys->MDotCPOA(ZoneLoop) *
                                                           (state.dataHeatBalFanSys->MAT(ZoneLoop) - Zone(ZoneLoop).OutDryBulbTemp) * TimeStepSys *
                                                           DataGlobalConstants::SecInHour * ADSCorrectionFactor;
                    ZnAirRpt(ZoneLoop).OABalanceHeatGain = 0.0;
                } else {
                    ZnAirRpt(ZoneLoop).OABalanceHeatLoss = 0.0;
                    ZnAirRpt(ZoneLoop).OABalanceHeatGain = -state.dataHeatBalFanSys->MDotCPOA(ZoneLoop) *
                                                           (state.dataHeatBalFanSys->MAT(ZoneLoop) - Zone(ZoneLoop).OutDryBulbTemp) * TimeStepSys *
                                                           DataGlobalConstants::SecInHour * ADSCorrectionFactor;
                }
                H2OHtOfVap = PsyHgAirFnWTdb(state.dataEnvrn->OutHumRat, Zone(ZoneLoop).OutDryBulbTemp);
                if (state.dataHeatBalFanSys->ZoneAirHumRat(ZoneLoop) > state.dataEnvrn->OutHumRat) {
                    ZnAirRpt(ZoneLoop).OABalanceLatentLoss = state.dataHeatBalFanSys->MDotOA(ZoneLoop) *
                                                             (state.dataHeatBalFanSys->ZoneAirHumRat(ZoneLoop) - state.dataEnvrn->OutHumRat) *
                                                             H2OHtOfVap * TimeStepSys * DataGlobalConstants::SecInHour * ADSCorrectionFactor;
                    ZnAirRpt(ZoneLoop).OABalanceLatentGain = 0.0;
                } else if (state.dataHeatBalFanSys->ZoneAirHumRat(ZoneLoop) <= state.dataEnvrn->OutHumRat) {
                    ZnAirRpt(ZoneLoop).OABalanceLatentGain = state.dataHeatBalFanSys->MDotOA(ZoneLoop) *
                                                             (state.dataEnvrn->OutHumRat - state.dataHeatBalFanSys->ZoneAirHumRat(ZoneLoop)) *
                                                             H2OHtOfVap * TimeStepSys * DataGlobalConstants::SecInHour * ADSCorrectionFactor;
                    ZnAirRpt(ZoneLoop).OABalanceLatentLoss = 0.0;
                }
                // Total ventilation losses and gains
                TotalLoad = ZnAirRpt(ZoneLoop).OABalanceHeatGain + ZnAirRpt(ZoneLoop).OABalanceLatentGain - ZnAirRpt(ZoneLoop).OABalanceHeatLoss -
                            ZnAirRpt(ZoneLoop).OABalanceLatentLoss;
                if (TotalLoad > 0) {
                    ZnAirRpt(ZoneLoop).OABalanceTotalGain = TotalLoad * ADSCorrectionFactor;
                    ZnAirRpt(ZoneLoop).OABalanceTotalLoss = 0.0;
                } else {
                    ZnAirRpt(ZoneLoop).OABalanceTotalGain = 0.0;
                    ZnAirRpt(ZoneLoop).OABalanceTotalLoss = -TotalLoad * ADSCorrectionFactor;
                }
                ZnAirRpt(ZoneLoop).OABalanceMass =
                    (state.dataHeatBalFanSys->MDotOA(ZoneLoop)) * TimeStepSys * DataGlobalConstants::SecInHour * ADSCorrectionFactor;
                ZnAirRpt(ZoneLoop).OABalanceMdot = (state.dataHeatBalFanSys->MDotOA(ZoneLoop)) * ADSCorrectionFactor;
                AirDensity = PsyRhoAirFnPbTdbW(state,
                                               state.dataEnvrn->OutBaroPress,
                                               state.dataHeatBalFanSys->MAT(ZoneLoop),
                                               state.dataHeatBalFanSys->ZoneAirHumRatAvg(ZoneLoop),
                                               std::string());
                ZnAirRpt(ZoneLoop).OABalanceVolumeCurDensity =
                    (state.dataHeatBalFanSys->MDotOA(ZoneLoop) / AirDensity) * TimeStepSys * DataGlobalConstants::SecInHour * ADSCorrectionFactor;
                ZnAirRpt(ZoneLoop).OABalanceAirChangeRate = ZnAirRpt(ZoneLoop).OABalanceVolumeCurDensity / (TimeStepSys * Zone(ZoneLoop).Volume);
                ZnAirRpt(ZoneLoop).OABalanceVdotCurDensity = (state.dataHeatBalFanSys->MDotOA(ZoneLoop) / AirDensity) * ADSCorrectionFactor;
                AirDensity = state.dataEnvrn->StdRhoAir;
                ZnAirRpt(ZoneLoop).OABalanceVolumeStdDensity =
                    (state.dataHeatBalFanSys->MDotOA(ZoneLoop) / AirDensity) * TimeStepSys * DataGlobalConstants::SecInHour * ADSCorrectionFactor;
                ZnAirRpt(ZoneLoop).OABalanceVdotStdDensity = (state.dataHeatBalFanSys->MDotOA(ZoneLoop) / AirDensity) * ADSCorrectionFactor;
                ZnAirRpt(ZoneLoop).OABalanceFanElec = ZnAirRpt(ZoneLoop).VentilFanElec;
            }
        }
        // Reports exfiltration loss
        H2OHtOfVap = PsyHgAirFnWTdb(state.dataEnvrn->OutHumRat, Zone(ZoneLoop).OutDryBulbTemp);
        ZnAirRpt(ZoneLoop).SysInletMass = 0;
        ZnAirRpt(ZoneLoop).SysOutletMass = 0;
        if (!ZoneEquipConfig(ZoneLoop).IsControlled) {
            for (int k = 1; k <= ZoneEquipConfig(ZoneLoop).NumInletNodes; ++k) {
                ZnAirRpt(ZoneLoop).SysInletMass += state.dataLoopNodes->Node(ZoneEquipConfig(ZoneLoop).InletNode(k)).MassFlowRate * TimeStepSys *
                                                   DataGlobalConstants::SecInHour * ADSCorrectionFactor;
            }
            for (int k = 1; k <= ZoneEquipConfig(ZoneLoop).NumExhaustNodes; ++k) {
                ZnAirRpt(ZoneLoop).SysOutletMass += state.dataLoopNodes->Node(ZoneEquipConfig(ZoneLoop).ExhaustNode(k)).MassFlowRate * TimeStepSys *
                                                    DataGlobalConstants::SecInHour * ADSCorrectionFactor;
            }
            for (int k = 1; k <= ZoneEquipConfig(ZoneLoop).NumReturnNodes; ++k) {
                ZnAirRpt(ZoneLoop).SysOutletMass += state.dataLoopNodes->Node(ZoneEquipConfig(ZoneLoop).ReturnNode(k)).MassFlowRate * TimeStepSys *
                                                    DataGlobalConstants::SecInHour * ADSCorrectionFactor;
            }
        }

        ZnAirRpt(ZoneLoop).ExfilMass = ZnAirRpt(ZoneLoop).InfilMass + ZnAirRpt(ZoneLoop).VentilMass + ZnAirRpt(ZoneLoop).MixMass +
                                       ZnAirRpt(ZoneLoop).OABalanceMass + ZnAirRpt(ZoneLoop).SysInletMass - ZnAirRpt(ZoneLoop).SysOutletMass; // kg
        ZnAirRpt(ZoneLoop).ExfilSensiLoss = ZnAirRpt(ZoneLoop).ExfilMass / (TimeStepSys * DataGlobalConstants::SecInHour) *
                                            (state.dataHeatBalFanSys->MAT(ZoneLoop) - Zone(ZoneLoop).OutDryBulbTemp) * CpAir; // W
        ZnAirRpt(ZoneLoop).ExfilLatentLoss = ZnAirRpt(ZoneLoop).ExfilMass / (TimeStepSys * DataGlobalConstants::SecInHour) *
                                             (state.dataHeatBalFanSys->ZoneAirHumRat(ZoneLoop) - state.dataEnvrn->OutHumRat) * H2OHtOfVap;
        ZnAirRpt(ZoneLoop).ExfilTotalLoss = ZnAirRpt(ZoneLoop).ExfilLatentLoss + ZnAirRpt(ZoneLoop).ExfilSensiLoss;

        state.dataHeatBal->ZoneTotalExfiltrationHeatLoss += ZnAirRpt(ZoneLoop).ExfilTotalLoss * TimeStepSys * DataGlobalConstants::SecInHour;
        state.dataHeatBal->ZoneTotalExhaustHeatLoss += ZnAirRpt(ZoneLoop).ExhTotalLoss * TimeStepSys * DataGlobalConstants::SecInHour;
    }
}

void SetHeatToReturnAirFlag(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   February 2008
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This sets some flags at the air loop and zone level: these flags indicate
    // whether an air loop represents a "unitary" system, and whether the system is operating
    // in a on/off (cycling fan) mode. At the zone level flags are set to indicate whether
    // the zone is served by a zonal system only, and whether the air loop serving the zone (idf any)
    // is in cycling fan mode. Using this information, the subroutine sets a flag at the zone level
    // to tell ManageZoneAirUpdates (predict and correct) what to do with the heat to return air.

    // METHODOLOGY EMPLOYED:
    // Uses program data structures AirLoopControlInfo and ZoneEquipInfo

    // Using/Aliasing
    auto &NumPrimaryAirSys = state.dataHVACGlobal->NumPrimaryAirSys;
    using DataSurfaces::AirFlowWindow_Destination_ReturnAir;
    using ScheduleManager::CheckScheduleValue;
    using ScheduleManager::GetCurrentScheduleValue;
    using ScheduleManager::GetScheduleMaxValue;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int AirLoopNum(0);      // the air loop index
    int ZoneNum(0);         // zone index
    int ControlledZoneNum;  // controlled zone index
    bool CyclingFan(false); // TRUE means air loop operates in cycling fan mode at some point
    int LightNum;           // Lights object index
    int SurfNum;            // Surface index

    auto &Zone(state.dataHeatBal->Zone);
    auto &AirLoopControlInfo(state.dataAirLoop->AirLoopControlInfo);
    auto &ZoneEquipConfig(state.dataZoneEquip->ZoneEquipConfig);

    if (!state.dataHVACGlobal->AirLoopsSimOnce) return;

    if (state.dataHVACMgr->MyOneTimeFlag) {
        // set the air loop Any Continuous Fan flag
        for (AirLoopNum = 1; AirLoopNum <= NumPrimaryAirSys; ++AirLoopNum) {
            if (AirLoopControlInfo(AirLoopNum).UnitarySys) { // for unitary systems check the cycling fan schedule
                if (AirLoopControlInfo(AirLoopNum).CycFanSchedPtr > 0) {
                    Real64 CycFanMaxVal = GetScheduleMaxValue(state, AirLoopControlInfo(AirLoopNum).CycFanSchedPtr);
                    if (CycFanMaxVal > 0.0) {
                        AirLoopControlInfo(AirLoopNum).AnyContFan = true;
                    } else {
                        AirLoopControlInfo(AirLoopNum).AnyContFan = false;
                    }
                } else { // no schedule means always cycling fan
                    AirLoopControlInfo(AirLoopNum).AnyContFan = false;
                }
            } else { // for nonunitary (central) all systems are continuous fan
                AirLoopControlInfo(AirLoopNum).AnyContFan = true;
            }
        }
        // check to see if a controlled zone is served exclusively by a zonal system
        for (ControlledZoneNum = 1; ControlledZoneNum <= state.dataGlobal->NumOfZones; ++ControlledZoneNum) {
            ZoneNum = ZoneEquipConfig(ControlledZoneNum).ActualZoneNum;
            bool airLoopFound = false;
            for (int zoneInNode = 1; zoneInNode <= ZoneEquipConfig(ControlledZoneNum).NumInletNodes; ++zoneInNode) {
                if (ZoneEquipConfig(ControlledZoneNum).InletNodeAirLoopNum(zoneInNode) > 0) {
                    airLoopFound = true;
                }
            }
            if (!airLoopFound && ZoneEquipConfig(ControlledZoneNum).NumInletNodes == ZoneEquipConfig(ControlledZoneNum).NumExhaustNodes) {
                ZoneEquipConfig(ControlledZoneNum).ZonalSystemOnly = true;
            }
        }
        // issue warning messages if zone is served by a zonal system or a cycling system and the input calls for
        // heat gain to return air
        for (ControlledZoneNum = 1; ControlledZoneNum <= state.dataGlobal->NumOfZones; ++ControlledZoneNum) {
            if (!ZoneEquipConfig(ControlledZoneNum).IsControlled) continue;
            ZoneNum = ZoneEquipConfig(ControlledZoneNum).ActualZoneNum;
            CyclingFan = false;
            for (int zoneInNode = 1; zoneInNode <= ZoneEquipConfig(ControlledZoneNum).NumInletNodes; ++zoneInNode) {
                AirLoopNum = ZoneEquipConfig(ControlledZoneNum).InletNodeAirLoopNum(zoneInNode);
                if (AirLoopNum > 0) {
                    if (AirLoopControlInfo(AirLoopNum).CycFanSchedPtr > 0) {
                        CyclingFan = CheckScheduleValue(state, AirLoopControlInfo(AirLoopNum).CycFanSchedPtr, 0.0);
                    }
                }
            }
            if (ZoneEquipConfig(ControlledZoneNum).ZonalSystemOnly || CyclingFan) {
                if (Zone(ZoneNum).RefrigCaseRA) {
                    ShowWarningError(state,
                                     "For zone=" + Zone(ZoneNum).Name + " return air cooling by refrigerated cases will be applied to the zone air.");
                    ShowContinueError(state, "  This zone has no return air or is served by an on/off HVAC system.");
                }
                for (LightNum = 1; LightNum <= state.dataHeatBal->TotLights; ++LightNum) {
                    if (state.dataHeatBal->Lights(LightNum).ZonePtr != ZoneNum) continue;
                    if (state.dataHeatBal->Lights(LightNum).FractionReturnAir > 0.0) {
                        ShowWarningError(state,
                                         "For zone=" + Zone(ZoneNum).Name + " return air heat gain from lights will be applied to the zone air.");
                        ShowContinueError(state, "  This zone has no return air or is served by an on/off HVAC system.");
                        break;
                    }
                }
                for (SurfNum = Zone(ZoneNum).HTSurfaceFirst; SurfNum <= Zone(ZoneNum).HTSurfaceLast; ++SurfNum) {
                    if (state.dataSurface->SurfWinAirflowDestination(SurfNum) == AirFlowWindow_Destination_ReturnAir) {
                        ShowWarningError(
                            state, "For zone=" + Zone(ZoneNum).Name + " return air heat gain from air flow windows will be applied to the zone air.");
                        ShowContinueError(state, "  This zone has no return air or is served by an on/off HVAC system.");
                    }
                }
            }
        }
        state.dataHVACMgr->MyOneTimeFlag = false;
    }

    // set the air loop fan operation mode
    for (AirLoopNum = 1; AirLoopNum <= NumPrimaryAirSys; ++AirLoopNum) {
        if (AirLoopControlInfo(AirLoopNum).CycFanSchedPtr > 0) {
            if (GetCurrentScheduleValue(state, AirLoopControlInfo(AirLoopNum).CycFanSchedPtr) == 0.0) {
                AirLoopControlInfo(AirLoopNum).FanOpMode = CycFanCycCoil;
            } else {
                AirLoopControlInfo(AirLoopNum).FanOpMode = ContFanCycCoil;
            }
        }
    }
    // set the zone level NoHeatToReturnAir flag
    // if any air loop in the zone is continuous fan, then set NoHeatToReturnAir = false and sort it out node-by-node
    for (ControlledZoneNum = 1; ControlledZoneNum <= state.dataGlobal->NumOfZones; ++ControlledZoneNum) {
        if (!ZoneEquipConfig(ControlledZoneNum).IsControlled) continue;
        ZoneNum = ZoneEquipConfig(ControlledZoneNum).ActualZoneNum;
        Zone(ZoneNum).NoHeatToReturnAir = true;
        if (!ZoneEquipConfig(ControlledZoneNum).ZonalSystemOnly) {
            for (int zoneInNode = 1; zoneInNode <= ZoneEquipConfig(ControlledZoneNum).NumInletNodes; ++zoneInNode) {
                AirLoopNum = ZoneEquipConfig(ControlledZoneNum).InletNodeAirLoopNum(zoneInNode);
                if (AirLoopNum > 0) {
                    if (AirLoopControlInfo(AirLoopNum).FanOpMode == ContFanCycCoil) {
                        Zone(ZoneNum).NoHeatToReturnAir = false;
                        break;
                    }
                }
            }
        }
    }
}

void UpdateZoneInletConvergenceLog(EnergyPlusData &state)
{

    std::array<Real64, DataConvergParams::ConvergLogStackDepth> tmpRealARR = {0};

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
            auto &thisAirLoopFlow(state.dataAirLoop->AirLoopFlow(AirLoopNum));
            if (!thisAirLoopFlow.FlowError) {
                Real64 unbalancedExhaustDelta = thisAirLoopFlow.SupFlow - thisAirLoopFlow.OAFlow - thisAirLoopFlow.SysRetFlow;
                if (unbalancedExhaustDelta > SmallMassFlow) {
                    ShowSevereError(state,
                                    "CheckAirLoopFlowBalance: AirLoopHVAC " + state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).Name +
                                        " is unbalanced. Supply is > return plus outdoor air.");
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
