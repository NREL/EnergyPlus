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

// EnergyPlus::ZoneTempPredictorCorrector Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

#include "Fixtures/EnergyPlusFixture.hh"

// EnergyPlus Headers
#include <AirflowNetwork/Solver.hpp>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalSurface.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataPrecisionGlobals.hh>
#include <EnergyPlus/DataRoomAirModel.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataZoneControls.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/HybridModel.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/ZoneContaminantPredictorCorrector.hh>
#include <EnergyPlus/ZonePlenum.hh>
#include <EnergyPlus/ZoneTempPredictorCorrector.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::DataHeatBalance;
using namespace EnergyPlus::DataContaminantBalance;
using namespace EnergyPlus::DataHeatBalFanSys;
using namespace EnergyPlus::DataZoneControls;
using namespace EnergyPlus::DataZoneEquipment;
using namespace EnergyPlus::DataZoneEnergyDemands;
using namespace EnergyPlus::DataSizing;
using namespace EnergyPlus::HeatBalanceManager;
using namespace EnergyPlus::ZonePlenum;
using namespace EnergyPlus::ZoneTempPredictorCorrector;
using namespace EnergyPlus::ZoneContaminantPredictorCorrector;
using namespace EnergyPlus::DataLoopNode;
using namespace EnergyPlus::DataHVACGlobals;
using namespace EnergyPlus::DataSurfaces;
using namespace EnergyPlus::DataEnvironment;
using namespace EnergyPlus::Psychrometrics;
using namespace EnergyPlus::ScheduleManager;
using namespace EnergyPlus::DataRoomAirModel;
using namespace EnergyPlus::HybridModel;
using namespace EnergyPlus::DataPrecisionGlobals;

TEST_F(EnergyPlusFixture, HybridModel_CorrectZoneAirTempTest)
{

    // ZoneTempPredictorCorrector variable initialization
    state->dataHeatBal->Zone.allocate(1);
    state->dataHybridModel->HybridModelZone.allocate(1);
    state->dataRoomAirMod->AirModel.allocate(1);
    state->dataHeatBalFanSys->ZTM1.allocate(1);
    state->dataHeatBalFanSys->ZTM2.allocate(1);
    state->dataHeatBalFanSys->ZTM3.allocate(1);
    state->dataHeatBalFanSys->XMAT.allocate(1);
    state->dataHeatBalFanSys->XM2T.allocate(1);
    state->dataHeatBalFanSys->XM3T.allocate(1);
    state->dataRoomAirMod->ZTOC.allocate(1);
    state->dataRoomAirMod->ZTMX.allocate(1);
    state->dataRoomAirMod->ZTM1MX.allocate(1);
    state->dataHeatBalFanSys->WZoneTimeMinus1Temp.allocate(1);
    state->dataHeatBalFanSys->WZoneTimeMinus2Temp.allocate(1);
    state->dataHeatBalFanSys->WZoneTimeMinus3Temp.allocate(1);
    state->dataHeatBalFanSys->WZoneTimeMinus1.allocate(1);
    state->dataHeatBalFanSys->WZoneTimeMinus2.allocate(1);
    state->dataHeatBalFanSys->WZoneTimeMinus3.allocate(1);
    state->dataHeatBalFanSys->AIRRAT.allocate(1);
    state->dataHeatBalFanSys->ZoneAirHumRat.allocate(1);
    state->dataHeatBalFanSys->NonAirSystemResponse.allocate(1);
    state->dataHeatBalFanSys->NonAirSystemResponse(1) = 0.0;
    state->dataHeatBalFanSys->SysDepZoneLoadsLagged.allocate(1);
    state->dataHeatBalFanSys->SysDepZoneLoadsLagged(1) = 0.0;
    state->afn->exchangeData.allocate(1);
    state->dataLoopNodes->Node.allocate(1);
    state->dataHeatBalFanSys->TempTstatAir.allocate(1);
    state->dataHeatBalFanSys->LoadCorrectionFactor.allocate(1);
    state->dataHeatBalFanSys->MAT.allocate(1);
    state->dataHeatBalFanSys->ZT.allocate(1);
    state->dataHeatBalFanSys->PreviousMeasuredZT1.allocate(1);
    state->dataHeatBalFanSys->PreviousMeasuredZT2.allocate(1);
    state->dataHeatBalFanSys->PreviousMeasuredZT3.allocate(1);
    state->dataHeatBalFanSys->PreviousMeasuredHumRat1.allocate(1);
    state->dataHeatBalFanSys->PreviousMeasuredHumRat2.allocate(1);
    state->dataHeatBalFanSys->PreviousMeasuredHumRat3.allocate(1);
    state->dataScheduleMgr->Schedule.allocate(6);

    // CalcZoneComponentLoadSums variable initialization
    state->dataHeatBalFanSys->MCPI.allocate(1);
    state->dataHeatBalFanSys->MCPI(1) = 0.0;
    state->dataHeatBalFanSys->MCPV.allocate(1);
    state->dataHeatBalFanSys->MCPM.allocate(1);
    state->dataHeatBalFanSys->MCPM(1) = 0.0;
    state->dataHeatBalFanSys->MCPE.allocate(1);
    state->dataHeatBalFanSys->MCPE(1) = 0.0;
    state->dataHeatBalFanSys->MCPC.allocate(1);
    state->dataHeatBalFanSys->MCPC(1) = 0.0;
    state->dataHeatBalFanSys->MDotCPOA.allocate(1);
    state->dataHeatBalFanSys->MDotCPOA(1) = 0.0;
    state->dataHeatBalFanSys->MDotOA.allocate(1);
    state->dataHeatBalFanSys->MDotOA(1) = 0.0;
    state->dataHeatBalFanSys->MCPTI.allocate(1);
    state->dataHeatBalFanSys->MCPTI(1) = 0.0;
    state->dataHeatBalFanSys->MCPTV.allocate(1);
    state->dataHeatBalFanSys->MCPTM.allocate(1);
    state->dataHeatBalFanSys->MCPTM(1) = 0.0;
    state->dataHeatBalFanSys->MCPTE.allocate(1);
    state->dataHeatBalFanSys->MCPTE(1) = 0.0;
    state->dataHeatBalFanSys->MCPTC.allocate(1);
    state->dataHeatBalFanSys->MCPTC(1) = 0.0;
    state->dataSurface->SurfaceWindow.allocate(1);
    state->dataSurface->Surface.allocate(2);
    state->dataHeatBalSurf->SurfHConvInt.allocate(1);
    state->dataHeatBal->ZoneSNLoadHeatRate.allocate(1);
    state->dataHeatBal->ZoneSNLoadCoolRate.allocate(1);
    state->dataHeatBal->ZoneSNLoadHeatEnergy.allocate(1);
    state->dataHeatBal->ZoneSNLoadCoolEnergy.allocate(1);
    state->dataZoneTempPredictorCorrector->ZoneAirRelHum.allocate(1);
    state->dataRoomAirMod->IsZoneDV.dimension(1, false);
    state->dataRoomAirMod->IsZoneCV.dimension(1, false);
    state->dataRoomAirMod->IsZoneUI.dimension(1, false);
    state->dataRoomAirMod->ZoneDVMixedFlag.allocate(1);
    state->dataHeatBal->ZnAirRpt.allocate(1);
    state->dataZoneEquip->ZoneEquipConfig.allocate(1);
    state->dataZoneEquip->ZoneEquipConfig(1).ActualZoneNum = 1;
    state->dataHeatBal->ZoneIntGain.allocate(1);
    state->dataSize->ZoneEqSizing.allocate(1);

    // CorrectZoneHumRat variable initialization
    state->dataHeatBalFanSys->ZoneLatentGain.allocate(1);
    state->dataHeatBalFanSys->ZoneLatentGain(1) = 0.0;
    state->dataHeatBalFanSys->ZoneLatentGainExceptPeople.allocate(1);
    state->dataHeatBalFanSys->ZoneLatentGainExceptPeople(1) = 0.0;
    state->dataHeatBalFanSys->SumLatentHTRadSys.allocate(1);
    state->dataHeatBalFanSys->SumLatentHTRadSys(1) = 0.0;
    state->dataHeatBalFanSys->SumHmARaW.allocate(1);
    state->dataHeatBalFanSys->SumHmARaW(1) = 0.0;
    state->dataHeatBalFanSys->SumConvHTRadSys.allocate(1);
    state->dataHeatBalFanSys->SumConvHTRadSys(1) = 0.0;
    state->dataHeatBalFanSys->SumConvPool.allocate(1);
    state->dataHeatBalFanSys->SumConvPool(1) = 0.0;
    state->dataHeatBalFanSys->SumHmARa.allocate(1);
    state->dataHeatBalFanSys->SumHmARa(1) = 0.0;
    state->dataHeatBalFanSys->MixingMassFlowXHumRat.allocate(1);
    state->dataHeatBalFanSys->MixingMassFlowXHumRat(1) = 0.0;
    state->dataHeatBalFanSys->MixingMassFlowZone.allocate(1);
    state->dataHeatBalFanSys->MixingMassFlowZone(1) = 0.0;
    state->dataHeatBalFanSys->ZoneW1.allocate(1);
    state->dataHeatBalFanSys->ZoneAirHumRatTemp.allocate(1);
    state->dataHeatBalFanSys->SumLatentPool.allocate(1);
    state->dataHeatBalFanSys->SumLatentPool(1) = 0.0;
    state->dataHeatBalFanSys->OAMFL.allocate(1);
    state->dataHeatBalFanSys->OAMFL(1) = 0.0;
    state->dataHeatBalFanSys->VAMFL.allocate(1);
    state->dataHeatBalFanSys->VAMFL(1) = 0.0;
    state->dataHeatBalFanSys->EAMFL.allocate(1);
    state->dataHeatBalFanSys->EAMFL(1) = 0.0;
    state->dataHeatBalFanSys->EAMFLxHumRat.allocate(1);
    state->dataHeatBalFanSys->EAMFLxHumRat(1) = 0.0;
    state->dataHeatBalFanSys->CTMFL.allocate(1);
    state->dataHeatBalFanSys->CTMFL(1) = 0.0;
    state->dataHeatBalFanSys->ZT.allocate(1);
    state->dataHeatBalFanSys->ZT(1) = 0.0;

    // CorrectZoneContaminants variable initialization
    state->dataContaminantBalance->AZ.allocate(1);
    state->dataContaminantBalance->BZ.allocate(1);
    state->dataContaminantBalance->CZ.allocate(1);
    state->dataContaminantBalance->AZGC.allocate(1);
    state->dataContaminantBalance->BZGC.allocate(1);
    state->dataContaminantBalance->CZGC.allocate(1);
    state->dataContaminantBalance->AZ(1) = 0.0;
    state->dataContaminantBalance->BZ(1) = 0.0;
    state->dataContaminantBalance->CZ(1) = 0.0;
    state->dataContaminantBalance->AZGC(1) = 0.0;
    state->dataContaminantBalance->BZGC(1) = 0.0;
    state->dataContaminantBalance->CZGC(1) = 0.0;
    state->dataContaminantBalance->ZoneAirDensityCO.allocate(1);
    state->dataContaminantBalance->ZoneAirDensityCO(1) = 0.0;
    state->dataContaminantBalance->ZoneGCGain.allocate(1);
    state->dataContaminantBalance->ZoneGCGain(1) = 0.0;

    // Parameter setup
    state->dataGlobal->NumOfZones = 1;
    state->dataSize->CurZoneEqNum = 1;
    state->dataZonePlenum->NumZoneReturnPlenums = 0;
    state->dataZonePlenum->NumZoneSupplyPlenums = 0;
    state->afn->SimulateAirflowNetwork = 0;
    state->dataHeatBal->Zone(1).IsControlled = true;
    state->dataHeatBal->Zone(1).ZoneEqNum = 1;
    state->dataHeatBal->Zone(1).Multiplier = 1;
    state->dataHeatBal->Zone(1).SystemZoneNodeNumber = 1;
    state->dataHeatBal->Zone(1).HTSurfaceFirst = 0; // No HT surface here.
    state->dataHeatBal->Zone(1).HTSurfaceLast = -1;
    state->dataHeatBal->Zone(1).Volume = 1061.88;
    state->dataGlobal->TimeStepZone = 10.0 / 60.0; // Zone timestep in hours
    state->dataHVACGlobal->TimeStepSys = 10.0 / 60.0;
    Real64 ZoneTempChange;

    // Hybrid modeling trigger
    state->dataHybridModel->FlagHybridModel_TM = true;
    state->dataGlobal->WarmupFlag = false;
    state->dataGlobal->DoingSizing = false;
    state->dataEnvrn->DayOfYear = 1;

    // Case 1: Hybrid model internal thermal mass (free-floating)

    state->dataHybridModel->HybridModelZone(1).InternalThermalMassCalc_T = true;
    state->dataHybridModel->HybridModelZone(1).InfiltrationCalc_T = false;
    state->dataHybridModel->HybridModelZone(1).InfiltrationCalc_H = false;
    state->dataHybridModel->HybridModelZone(1).InfiltrationCalc_C = false;
    state->dataHybridModel->HybridModelZone(1).PeopleCountCalc_T = false;
    state->dataHybridModel->HybridModelZone(1).PeopleCountCalc_H = false;
    state->dataHybridModel->HybridModelZone(1).PeopleCountCalc_C = false;
    state->dataHybridModel->HybridModelZone(1).HybridStartDayOfYear = 1;
    state->dataHybridModel->HybridModelZone(1).HybridEndDayOfYear = 2;
    state->dataHeatBalFanSys->MAT(1) = 0.0;
    state->dataHeatBalFanSys->PreviousMeasuredZT1(1) = 0.1;
    state->dataHeatBalFanSys->PreviousMeasuredZT2(1) = 0.2;
    state->dataHeatBalFanSys->PreviousMeasuredZT3(1) = 0.3;
    state->dataHeatBal->Zone(1).OutDryBulbTemp = -5.21;
    state->dataHeatBalFanSys->ZoneAirHumRat(1) = 0.002083;
    state->dataHeatBalFanSys->MCPV(1) = 1414.60;   // Assign TempDepCoef
    state->dataHeatBalFanSys->MCPTV(1) = -3335.10; // Assign TempIndCoef
    state->dataEnvrn->OutBaroPress = 99166.67;

    CorrectZoneAirTemp(*state, ZoneTempChange, false, true, 10 / 60);
    EXPECT_NEAR(15.13, state->dataHeatBal->Zone(1).ZoneVolCapMultpSensHM, 0.01);

    // Case 2: Hybrid model infiltration with measured temperature (free-floating)

    state->dataHybridModel->HybridModelZone(1).InternalThermalMassCalc_T = false;
    state->dataHybridModel->HybridModelZone(1).InfiltrationCalc_T = true;
    state->dataHybridModel->HybridModelZone(1).InfiltrationCalc_H = false;
    state->dataHybridModel->HybridModelZone(1).InfiltrationCalc_C = false;
    state->dataHybridModel->HybridModelZone(1).PeopleCountCalc_T = false;
    state->dataHybridModel->HybridModelZone(1).PeopleCountCalc_H = false;
    state->dataHybridModel->HybridModelZone(1).PeopleCountCalc_C = false;
    state->dataHybridModel->HybridModelZone(1).HybridStartDayOfYear = 1;
    state->dataHybridModel->HybridModelZone(1).HybridEndDayOfYear = 2;
    state->dataHeatBalFanSys->MAT(1) = 0.0;
    state->dataHeatBalFanSys->PreviousMeasuredZT1(1) = 0.02;
    state->dataHeatBalFanSys->PreviousMeasuredZT2(1) = 0.04;
    state->dataHeatBalFanSys->PreviousMeasuredZT3(1) = 0.06;
    state->dataHeatBal->Zone(1).ZoneVolCapMultpSens = 8.0;
    state->dataHeatBal->Zone(1).OutDryBulbTemp = -6.71;
    state->dataHeatBalFanSys->ZoneAirHumRat(1) = 0.002083;
    state->dataHeatBalFanSys->MCPV(1) = 539.49;  // Assign TempDepCoef
    state->dataHeatBalFanSys->MCPTV(1) = 270.10; // Assign TempIndCoef
    state->dataEnvrn->OutBaroPress = 99250;

    CorrectZoneAirTemp(*state, ZoneTempChange, false, true, 10 / 60);
    EXPECT_NEAR(0.2444, state->dataHeatBal->Zone(1).InfilOAAirChangeRateHM, 0.01);

    // Case 3: Hybrid model infiltration with measured humidity ratio (free-floating)

    state->dataHybridModel->HybridModelZone(1).InternalThermalMassCalc_T = false;
    state->dataHybridModel->HybridModelZone(1).InfiltrationCalc_T = false;
    state->dataHybridModel->HybridModelZone(1).InfiltrationCalc_H = true;
    state->dataHybridModel->HybridModelZone(1).InfiltrationCalc_C = false;
    state->dataHybridModel->HybridModelZone(1).PeopleCountCalc_T = false;
    state->dataHybridModel->HybridModelZone(1).PeopleCountCalc_H = false;
    state->dataHybridModel->HybridModelZone(1).PeopleCountCalc_C = false;
    state->dataHybridModel->HybridModelZone(1).HybridStartDayOfYear = 1;
    state->dataHybridModel->HybridModelZone(1).HybridEndDayOfYear = 2;
    state->dataHeatBal->Zone(1).Volume = 4000;
    state->dataHeatBal->Zone(1).OutDryBulbTemp = -10.62;
    state->dataHeatBal->Zone(1).ZoneVolCapMultpMoist = 1.0;
    state->dataHeatBalFanSys->ZoneAirHumRat(1) = 0.001120003;
    state->dataHeatBalFanSys->ZT(1) = -6.08;
    state->dataEnvrn->OutHumRat = 0.0011366887816818931;
    state->dataHeatBalFanSys->PreviousMeasuredHumRat1(1) = 0.0011186324286;
    state->dataHeatBalFanSys->PreviousMeasuredHumRat2(1) = 0.0011172070768;
    state->dataHeatBalFanSys->PreviousMeasuredHumRat3(1) = 0.0011155109625;
    state->dataHybridModel->HybridModelZone(1).ZoneMeasuredHumidityRatioSchedulePtr = 1;
    state->dataScheduleMgr->Schedule(state->dataHybridModel->HybridModelZone(1).ZoneMeasuredHumidityRatioSchedulePtr).CurrentValue = 0.001120003;
    state->dataHeatBalFanSys->MCPV(1) = 539.49;
    state->dataHeatBalFanSys->MCPTV(1) = 270.10;
    state->dataEnvrn->OutBaroPress = 99500;

    CorrectZoneHumRat(*state, 1);
    EXPECT_NEAR(0.5, state->dataHeatBal->Zone(1).InfilOAAirChangeRateHM, 0.01);

    // Case 4: Hybrid model people count with measured temperature (free-floating)

    state->dataHybridModel->HybridModelZone(1).InternalThermalMassCalc_T = false;
    state->dataHybridModel->HybridModelZone(1).InfiltrationCalc_T = false;
    state->dataHybridModel->HybridModelZone(1).InfiltrationCalc_H = false;
    state->dataHybridModel->HybridModelZone(1).InfiltrationCalc_C = false;
    state->dataHybridModel->HybridModelZone(1).PeopleCountCalc_T = true;
    state->dataHybridModel->HybridModelZone(1).PeopleCountCalc_H = false;
    state->dataHybridModel->HybridModelZone(1).PeopleCountCalc_C = false;
    state->dataHybridModel->HybridModelZone(1).HybridStartDayOfYear = 1;
    state->dataHybridModel->HybridModelZone(1).HybridEndDayOfYear = 2;

    state->dataHeatBalFanSys->MAT(1) = -2.89;
    state->dataHeatBalFanSys->PreviousMeasuredZT1(1) = -2.887415174;
    state->dataHeatBalFanSys->PreviousMeasuredZT2(1) = -2.897557416;
    state->dataHeatBalFanSys->PreviousMeasuredZT3(1) = -2.909294101;
    state->dataHeatBal->Zone(1).ZoneVolCapMultpSens = 1.0;
    state->dataHeatBal->Zone(1).OutDryBulbTemp = -6.71;
    state->dataHeatBalFanSys->ZoneAirHumRat(1) = 0.0024964;
    state->dataEnvrn->OutBaroPress = 98916.7;
    state->dataHeatBalFanSys->MCPV(1) = 5163.5;    // Assign TempDepCoef
    state->dataHeatBalFanSys->MCPTV(1) = -15956.8; // Assign TempIndCoef
    state->dataHybridModel->HybridModelZone(1).ZoneMeasuredTemperatureSchedulePtr = 1;
    state->dataScheduleMgr->Schedule(state->dataHybridModel->HybridModelZone(1).ZoneMeasuredTemperatureSchedulePtr).CurrentValue = -2.923892218;

    CorrectZoneAirTemp(*state, ZoneTempChange, false, true, 10 / 60);
    EXPECT_NEAR(0, state->dataHeatBal->Zone(1).NumOccHM, 0.1); // Need to initialize SumIntGain

    // Case 5: Hybrid model people count with measured humidity ratio (free-floating)

    state->dataHybridModel->HybridModelZone(1).InternalThermalMassCalc_T = false;
    state->dataHybridModel->HybridModelZone(1).InfiltrationCalc_T = false;
    state->dataHybridModel->HybridModelZone(1).InfiltrationCalc_H = false;
    state->dataHybridModel->HybridModelZone(1).InfiltrationCalc_C = false;
    state->dataHybridModel->HybridModelZone(1).PeopleCountCalc_T = false;
    state->dataHybridModel->HybridModelZone(1).PeopleCountCalc_H = true;
    state->dataHybridModel->HybridModelZone(1).PeopleCountCalc_C = false;
    state->dataHybridModel->HybridModelZone(1).HybridStartDayOfYear = 1;
    state->dataHybridModel->HybridModelZone(1).HybridEndDayOfYear = 2;
    state->dataHeatBal->Zone(1).Volume = 4000;
    state->dataHeatBal->Zone(1).OutDryBulbTemp = -10.62;
    state->dataHeatBal->Zone(1).ZoneVolCapMultpMoist = 1.0;
    state->dataHeatBalFanSys->ZoneAirHumRat(1) = 0.0024964;
    state->dataHeatBalFanSys->ZT(1) = -2.92;
    state->dataEnvrn->OutHumRat = 0.0025365002784602363;
    state->dataEnvrn->OutBaroPress = 98916.7;
    state->dataHeatBalFanSys->OAMFL(1) = 0.700812;
    state->dataHeatBalFanSys->ZoneLatentGain(1) = 211.2;
    state->dataHeatBalFanSys->ZoneLatentGainExceptPeople(1) = 0.0;
    state->dataHeatBalFanSys->PreviousMeasuredHumRat1(1) = 0.002496356;
    state->dataHeatBalFanSys->PreviousMeasuredHumRat2(1) = 0.002489048;
    state->dataHeatBalFanSys->PreviousMeasuredHumRat3(1) = 0.002480404;
    state->dataHybridModel->HybridModelZone(1).ZoneMeasuredHumidityRatioSchedulePtr = 1;
    state->dataScheduleMgr->Schedule(state->dataHybridModel->HybridModelZone(1).ZoneMeasuredHumidityRatioSchedulePtr).CurrentValue =
        0.002506251487737;

    CorrectZoneHumRat(*state, 1);
    EXPECT_NEAR(4, state->dataHeatBal->Zone(1).NumOccHM, 0.1);

    // Case 6: Hybrid model infiltration with measured temperature (with HVAC)

    state->dataHybridModel->HybridModelZone(1).InternalThermalMassCalc_T = false;
    state->dataHybridModel->HybridModelZone(1).InfiltrationCalc_T = true;
    state->dataHybridModel->HybridModelZone(1).InfiltrationCalc_H = false;
    state->dataHybridModel->HybridModelZone(1).InfiltrationCalc_C = false;
    state->dataHybridModel->HybridModelZone(1).PeopleCountCalc_T = false;
    state->dataHybridModel->HybridModelZone(1).PeopleCountCalc_H = false;
    state->dataHybridModel->HybridModelZone(1).PeopleCountCalc_C = false;
    state->dataHybridModel->HybridModelZone(1).IncludeSystemSupplyParameters = true;
    state->dataHybridModel->HybridModelZone(1).HybridStartDayOfYear = 1;
    state->dataHybridModel->HybridModelZone(1).HybridEndDayOfYear = 2;
    state->dataHeatBalFanSys->MAT(1) = 15.56;
    state->dataHeatBalFanSys->PreviousMeasuredZT1(1) = 15.56;
    state->dataHeatBalFanSys->PreviousMeasuredZT2(1) = 15.56;
    state->dataHeatBalFanSys->PreviousMeasuredZT3(1) = 15.56;
    state->dataHeatBal->Zone(1).ZoneVolCapMultpSens = 1.0;
    state->dataHeatBal->Zone(1).OutDryBulbTemp = -10.62;
    state->dataHeatBalFanSys->ZoneAirHumRat(1) = 0.0077647;
    state->dataHeatBalFanSys->MCPV(1) = 4456;   // Assign TempDepCoef
    state->dataHeatBalFanSys->MCPTV(1) = 60650; // Assign TempIndCoef
    state->dataEnvrn->OutBaroPress = 99500;
    state->dataEnvrn->OutHumRat = 0.00113669;
    state->dataHybridModel->HybridModelZone(1).ZoneMeasuredTemperatureSchedulePtr = 1;
    state->dataHybridModel->HybridModelZone(1).ZoneSupplyAirTemperatureSchedulePtr = 2;
    state->dataHybridModel->HybridModelZone(1).ZoneSupplyAirMassFlowRateSchedulePtr = 3;
    state->dataScheduleMgr->Schedule(state->dataHybridModel->HybridModelZone(1).ZoneMeasuredTemperatureSchedulePtr).CurrentValue = 15.56;
    state->dataScheduleMgr->Schedule(state->dataHybridModel->HybridModelZone(1).ZoneSupplyAirTemperatureSchedulePtr).CurrentValue = 50;
    state->dataScheduleMgr->Schedule(state->dataHybridModel->HybridModelZone(1).ZoneSupplyAirMassFlowRateSchedulePtr).CurrentValue = 0.7974274;

    CorrectZoneAirTemp(*state, ZoneTempChange, false, true, 10 / 60);
    EXPECT_NEAR(0.49, state->dataHeatBal->Zone(1).InfilOAAirChangeRateHM, 0.01);

    // Case 7: Hybrid model infiltration with measured humidity ratio (with HVAC)

    state->dataHybridModel->HybridModelZone(1).InternalThermalMassCalc_T = false;
    state->dataHybridModel->HybridModelZone(1).InfiltrationCalc_T = false;
    state->dataHybridModel->HybridModelZone(1).InfiltrationCalc_H = true;
    state->dataHybridModel->HybridModelZone(1).InfiltrationCalc_C = false;
    state->dataHybridModel->HybridModelZone(1).PeopleCountCalc_T = false;
    state->dataHybridModel->HybridModelZone(1).PeopleCountCalc_H = false;
    state->dataHybridModel->HybridModelZone(1).PeopleCountCalc_C = false;
    state->dataHybridModel->HybridModelZone(1).IncludeSystemSupplyParameters = true;
    state->dataHybridModel->HybridModelZone(1).HybridStartDayOfYear = 1;
    state->dataHybridModel->HybridModelZone(1).HybridEndDayOfYear = 2;
    state->dataHeatBal->Zone(1).Volume = 4000;
    state->dataHeatBal->Zone(1).OutDryBulbTemp = -10.62;
    state->dataHeatBal->Zone(1).ZoneVolCapMultpMoist = 1.0;
    state->dataHeatBalFanSys->ZoneAirHumRat(1) = 0.001120003;
    state->dataHeatBalFanSys->ZT(1) = -6.08;
    state->dataEnvrn->OutHumRat = 0.0011366887816818931;
    state->dataHeatBalFanSys->PreviousMeasuredHumRat1(1) = 0.007855718;
    state->dataHeatBalFanSys->PreviousMeasuredHumRat2(1) = 0.007852847;
    state->dataHeatBalFanSys->PreviousMeasuredHumRat3(1) = 0.007850236;
    state->dataHybridModel->HybridModelZone(1).ZoneMeasuredHumidityRatioSchedulePtr = 1;
    state->dataHybridModel->HybridModelZone(1).ZoneSupplyAirHumidityRatioSchedulePtr = 2;
    state->dataHybridModel->HybridModelZone(1).ZoneSupplyAirMassFlowRateSchedulePtr = 3;
    state->dataScheduleMgr->Schedule(state->dataHybridModel->HybridModelZone(1).ZoneMeasuredHumidityRatioSchedulePtr).CurrentValue = 0.00792;
    state->dataScheduleMgr->Schedule(state->dataHybridModel->HybridModelZone(1).ZoneSupplyAirHumidityRatioSchedulePtr).CurrentValue = 0.015;
    state->dataScheduleMgr->Schedule(state->dataHybridModel->HybridModelZone(1).ZoneSupplyAirMassFlowRateSchedulePtr).CurrentValue = 0.8345;
    state->dataEnvrn->OutBaroPress = 99500;

    CorrectZoneHumRat(*state, 1);
    EXPECT_NEAR(0.5, state->dataHeatBal->Zone(1).InfilOAAirChangeRateHM, 0.01);

    // Case 8: Hybrid model people count with measured temperature (with HVAC)

    state->dataHybridModel->HybridModelZone(1).InternalThermalMassCalc_T = false;
    state->dataHybridModel->HybridModelZone(1).InfiltrationCalc_T = false;
    state->dataHybridModel->HybridModelZone(1).InfiltrationCalc_H = false;
    state->dataHybridModel->HybridModelZone(1).InfiltrationCalc_C = false;
    state->dataHybridModel->HybridModelZone(1).PeopleCountCalc_T = true;
    state->dataHybridModel->HybridModelZone(1).PeopleCountCalc_H = false;
    state->dataHybridModel->HybridModelZone(1).PeopleCountCalc_C = false;
    state->dataHybridModel->HybridModelZone(1).IncludeSystemSupplyParameters = true;
    state->dataHybridModel->HybridModelZone(1).HybridStartDayOfYear = 1;
    state->dataHybridModel->HybridModelZone(1).HybridEndDayOfYear = 2;
    state->dataHeatBalFanSys->MAT(1) = -2.89;
    state->dataHeatBalFanSys->PreviousMeasuredZT1(1) = 21.11;
    state->dataHeatBalFanSys->PreviousMeasuredZT2(1) = 21.11;
    state->dataHeatBalFanSys->PreviousMeasuredZT3(1) = 21.11;
    state->dataHeatBal->Zone(1).ZoneVolCapMultpSens = 1.0;
    state->dataHeatBal->Zone(1).OutDryBulbTemp = -6.71;
    state->dataHeatBalFanSys->ZoneAirHumRat(1) = 0.0024964;
    state->dataEnvrn->OutBaroPress = 98916.7;
    state->dataHeatBalFanSys->MCPV(1) = 6616;      // Assign TempDepCoef
    state->dataHeatBalFanSys->MCPTV(1) = 138483.2; // Assign TempIndCoef
    state->dataHybridModel->HybridModelZone(1).ZoneMeasuredTemperatureSchedulePtr = 1;
    state->dataHybridModel->HybridModelZone(1).ZoneSupplyAirTemperatureSchedulePtr = 2;
    state->dataHybridModel->HybridModelZone(1).ZoneSupplyAirMassFlowRateSchedulePtr = 3;
    state->dataHybridModel->HybridModelZone(1).ZonePeopleActivityLevelSchedulePtr = 4;
    state->dataHybridModel->HybridModelZone(1).ZonePeopleSensibleFractionSchedulePtr = 5;
    state->dataHybridModel->HybridModelZone(1).ZonePeopleRadiationFractionSchedulePtr = 6;
    state->dataScheduleMgr->Schedule(state->dataHybridModel->HybridModelZone(1).ZoneMeasuredTemperatureSchedulePtr).CurrentValue = 21.11;
    state->dataScheduleMgr->Schedule(state->dataHybridModel->HybridModelZone(1).ZoneSupplyAirTemperatureSchedulePtr).CurrentValue = 50;
    state->dataScheduleMgr->Schedule(state->dataHybridModel->HybridModelZone(1).ZoneSupplyAirMassFlowRateSchedulePtr).CurrentValue = 1.446145794;
    state->dataScheduleMgr->Schedule(state->dataHybridModel->HybridModelZone(1).ZonePeopleActivityLevelSchedulePtr).CurrentValue = 120;
    state->dataScheduleMgr->Schedule(state->dataHybridModel->HybridModelZone(1).ZonePeopleSensibleFractionSchedulePtr).CurrentValue = 0.6;
    state->dataScheduleMgr->Schedule(state->dataHybridModel->HybridModelZone(1).ZonePeopleRadiationFractionSchedulePtr).CurrentValue = 0.3;

    CorrectZoneAirTemp(*state, ZoneTempChange, false, true, 10 / 60);
    EXPECT_NEAR(0, state->dataHeatBal->Zone(1).NumOccHM, 0.1); // Need to initialize SumIntGain

    // Case 9: Hybrid model people count with measured humidity ratio (with HVAC)
    state->dataHybridModel->HybridModelZone(1).InternalThermalMassCalc_T = false;
    state->dataHybridModel->HybridModelZone(1).InfiltrationCalc_T = false;
    state->dataHybridModel->HybridModelZone(1).InfiltrationCalc_H = false;
    state->dataHybridModel->HybridModelZone(1).InfiltrationCalc_C = false;
    state->dataHybridModel->HybridModelZone(1).PeopleCountCalc_T = false;
    state->dataHybridModel->HybridModelZone(1).PeopleCountCalc_H = true;
    state->dataHybridModel->HybridModelZone(1).PeopleCountCalc_C = false;
    state->dataHybridModel->HybridModelZone(1).IncludeSystemSupplyParameters = true;
    state->dataHybridModel->HybridModelZone(1).HybridStartDayOfYear = 1;
    state->dataHybridModel->HybridModelZone(1).HybridEndDayOfYear = 2;
    state->dataHeatBal->Zone(1).Volume = 4000;
    state->dataHeatBal->Zone(1).OutDryBulbTemp = -10.62;
    state->dataHeatBal->Zone(1).ZoneVolCapMultpMoist = 1.0;
    state->dataHeatBalFanSys->ZoneAirHumRat(1) = 0.001120003;
    state->dataHeatBalFanSys->ZT(1) = -6.08;
    state->dataEnvrn->OutHumRat = 0.0011366887816818931;
    state->dataHeatBalFanSys->PreviousMeasuredHumRat1(1) = 0.011085257;
    state->dataHeatBalFanSys->PreviousMeasuredHumRat2(1) = 0.011084959;
    state->dataHeatBalFanSys->PreviousMeasuredHumRat3(1) = 0.011072322;
    state->dataHybridModel->HybridModelZone(1).ZoneMeasuredHumidityRatioSchedulePtr = 1;
    state->dataHybridModel->HybridModelZone(1).ZoneSupplyAirHumidityRatioSchedulePtr = 2;
    state->dataHybridModel->HybridModelZone(1).ZoneSupplyAirMassFlowRateSchedulePtr = 3;
    state->dataHybridModel->HybridModelZone(1).ZonePeopleActivityLevelSchedulePtr = 4;
    state->dataHybridModel->HybridModelZone(1).ZonePeopleSensibleFractionSchedulePtr = 5;
    state->dataHybridModel->HybridModelZone(1).ZonePeopleRadiationFractionSchedulePtr = 6;
    state->dataScheduleMgr->Schedule(state->dataHybridModel->HybridModelZone(1).ZoneMeasuredHumidityRatioSchedulePtr).CurrentValue = 0.01107774;
    state->dataScheduleMgr->Schedule(state->dataHybridModel->HybridModelZone(1).ZoneSupplyAirHumidityRatioSchedulePtr).CurrentValue = 0.015;
    state->dataScheduleMgr->Schedule(state->dataHybridModel->HybridModelZone(1).ZoneSupplyAirMassFlowRateSchedulePtr).CurrentValue = 1.485334886;
    state->dataScheduleMgr->Schedule(state->dataHybridModel->HybridModelZone(1).ZonePeopleActivityLevelSchedulePtr).CurrentValue = 120;
    state->dataScheduleMgr->Schedule(state->dataHybridModel->HybridModelZone(1).ZonePeopleSensibleFractionSchedulePtr).CurrentValue = 0.6;
    state->dataScheduleMgr->Schedule(state->dataHybridModel->HybridModelZone(1).ZonePeopleRadiationFractionSchedulePtr).CurrentValue = 0.3;
    state->dataEnvrn->OutBaroPress = 99500;

    CorrectZoneHumRat(*state, 1);
    EXPECT_NEAR(4, state->dataHeatBal->Zone(1).NumOccHM, 0.1);

    // Deallocate everything
    state->dataHybridModel->clear_state();
    state->dataHeatBal->Zone.deallocate();
    state->dataRoomAirMod->AirModel.deallocate();
    state->dataHeatBalFanSys->ZTM1.deallocate();
    state->dataHeatBalFanSys->ZTM2.deallocate();
    state->dataHeatBalFanSys->ZTM3.deallocate();
    state->dataHeatBalFanSys->XMAT.deallocate();
    state->dataHeatBalFanSys->XM2T.deallocate();
    state->dataHeatBalFanSys->XM3T.deallocate();
    state->dataRoomAirMod->ZTOC.deallocate();
    state->dataRoomAirMod->ZTMX.deallocate();
    state->dataRoomAirMod->ZTM1MX.deallocate();
    state->dataHeatBalFanSys->WZoneTimeMinus1Temp.deallocate();
    state->dataHeatBalFanSys->WZoneTimeMinus2Temp.deallocate();
    state->dataHeatBalFanSys->WZoneTimeMinus3Temp.deallocate();
    state->dataHeatBalFanSys->WZoneTimeMinus1.deallocate();
    state->dataHeatBalFanSys->WZoneTimeMinus2.deallocate();
    state->dataHeatBalFanSys->WZoneTimeMinus3.deallocate();
    state->dataHeatBalFanSys->PreviousMeasuredHumRat1.deallocate();
    state->dataHeatBalFanSys->PreviousMeasuredHumRat2.deallocate();
    state->dataHeatBalFanSys->PreviousMeasuredHumRat3.deallocate();
    state->dataHeatBalFanSys->AIRRAT.deallocate();
    state->dataHeatBalFanSys->ZoneAirHumRat.deallocate();
    state->dataHeatBalFanSys->NonAirSystemResponse.deallocate();
    state->dataHeatBalFanSys->SysDepZoneLoadsLagged.deallocate();
    state->afn->exchangeData.deallocate();
    state->dataLoopNodes->Node.deallocate();
    state->dataHeatBalFanSys->TempTstatAir.deallocate();
    state->dataHeatBalFanSys->LoadCorrectionFactor.deallocate();
    state->dataHeatBalFanSys->MAT.deallocate();
    state->dataHeatBalFanSys->ZT.deallocate();
    state->dataHeatBalFanSys->PreviousMeasuredZT1.deallocate();
    state->dataHeatBalFanSys->PreviousMeasuredZT2.deallocate();
    state->dataHeatBalFanSys->PreviousMeasuredZT3.deallocate();
    state->dataHeatBalFanSys->MCPI.deallocate();
    state->dataHeatBalFanSys->MCPV.deallocate();
    state->dataHeatBalFanSys->MCPM.deallocate();
    state->dataHeatBalFanSys->MCPE.deallocate();
    state->dataHeatBalFanSys->MCPC.deallocate();
    state->dataHeatBalFanSys->MDotCPOA.deallocate();
    state->dataHeatBalFanSys->MDotOA.deallocate();
    state->dataHeatBalFanSys->MCPTI.deallocate();
    state->dataHeatBalFanSys->MCPTV.deallocate();
    state->dataHeatBalFanSys->MCPTM.deallocate();
    state->dataHeatBalFanSys->MCPTE.deallocate();
    state->dataHeatBalFanSys->MCPTC.deallocate();
    state->dataSurface->SurfaceWindow.deallocate();
    state->dataSurface->Surface.deallocate();
    state->dataHeatBalSurf->SurfHConvInt.deallocate();
    state->dataZoneTempPredictorCorrector->ZoneAirRelHum.deallocate();
    state->dataRoomAirMod->IsZoneDV.deallocate();
    state->dataRoomAirMod->IsZoneCV.deallocate();
    state->dataRoomAirMod->IsZoneUI.deallocate();
    state->dataRoomAirMod->ZoneDVMixedFlag.deallocate();
    state->dataHeatBal->ZnAirRpt.deallocate();
    state->dataZoneEquip->ZoneEquipConfig.deallocate();
    state->dataHeatBal->ZoneIntGain.deallocate();
    state->dataSize->ZoneEqSizing.deallocate();
    state->dataHeatBalFanSys->ZoneLatentGain.deallocate();
    state->dataHeatBalFanSys->SumLatentHTRadSys.deallocate();
    state->dataHeatBalFanSys->SumHmARaW.deallocate();
    state->dataHeatBalFanSys->SumConvHTRadSys.deallocate();
    state->dataHeatBalFanSys->SumConvPool.deallocate();
    state->dataHeatBalFanSys->SumHmARa.deallocate();
    state->dataHeatBalFanSys->MixingMassFlowXHumRat.deallocate();
    state->dataHeatBalFanSys->MixingMassFlowZone.deallocate();
    state->dataHeatBalFanSys->ZoneW1.deallocate();
    state->dataHeatBalFanSys->ZoneAirHumRatTemp.deallocate();
    state->dataHeatBalFanSys->SumLatentPool.deallocate();
    state->dataHeatBalFanSys->OAMFL.deallocate();
    state->dataHeatBalFanSys->VAMFL.deallocate();
    state->dataHeatBalFanSys->EAMFL.deallocate();
    state->dataHeatBalFanSys->EAMFLxHumRat.deallocate();
    state->dataHeatBalFanSys->CTMFL.deallocate();
    state->dataContaminantBalance->ZoneAirDensityCO.deallocate();
    state->dataContaminantBalance->ZoneGCGain.deallocate();
    state->dataScheduleMgr->Schedule.deallocate();
}

TEST_F(EnergyPlusFixture, HybridModel_CorrectZoneContaminantsTest)
{

    // ZoneContaminantPredictorCorrector variable initialization
    state->dataHeatBal->Zone.allocate(1);
    state->dataHybridModel->HybridModelZone.allocate(1);
    state->dataRoomAirMod->AirModel.allocate(1);
    state->dataRoomAirMod->ZTOC.allocate(1);
    state->dataHeatBalFanSys->AIRRAT.allocate(1);
    state->dataHeatBalFanSys->ZoneAirHumRat.allocate(1);
    state->dataHeatBalFanSys->NonAirSystemResponse.allocate(1);
    state->dataHeatBalFanSys->NonAirSystemResponse(1) = 0.0;
    state->dataHeatBalFanSys->SysDepZoneLoadsLagged.allocate(1);
    state->dataHeatBalFanSys->SysDepZoneLoadsLagged(1) = 0.0;
    state->afn->exchangeData.allocate(1);
    state->dataLoopNodes->Node.allocate(1);
    state->dataHeatBalFanSys->TempTstatAir.allocate(1);
    state->dataHeatBalFanSys->LoadCorrectionFactor.allocate(1);
    state->dataHeatBalFanSys->MAT.allocate(1);
    state->dataHeatBalFanSys->ZT.allocate(1);
    state->dataHeatBalFanSys->PreviousMeasuredZT1.allocate(1);
    state->dataHeatBalFanSys->PreviousMeasuredZT2.allocate(1);
    state->dataHeatBalFanSys->PreviousMeasuredZT3.allocate(1);
    state->dataContaminantBalance->CO2ZoneTimeMinus1Temp.allocate(1);
    state->dataContaminantBalance->CO2ZoneTimeMinus2Temp.allocate(1);
    state->dataContaminantBalance->CO2ZoneTimeMinus3Temp.allocate(1);
    state->dataContaminantBalance->CO2ZoneTimeMinus1.allocate(1);
    state->dataContaminantBalance->CO2ZoneTimeMinus2.allocate(1);
    state->dataContaminantBalance->CO2ZoneTimeMinus3.allocate(1);
    state->dataScheduleMgr->Schedule.allocate(7);

    // CalcZoneComponentLoadSums variable initialization
    state->dataHeatBalFanSys->MCPI.allocate(1);
    state->dataHeatBalFanSys->MCPI(1) = 0.0;
    state->dataHeatBalFanSys->MCPV.allocate(1);
    state->dataHeatBalFanSys->MCPM.allocate(1);
    state->dataHeatBalFanSys->MCPM(1) = 0.0;
    state->dataHeatBalFanSys->MCPE.allocate(1);
    state->dataHeatBalFanSys->MCPE(1) = 0.0;
    state->dataHeatBalFanSys->MCPC.allocate(1);
    state->dataHeatBalFanSys->MCPC(1) = 0.0;
    state->dataHeatBalFanSys->MDotCPOA.allocate(1);
    state->dataHeatBalFanSys->MDotCPOA(1) = 0.0;
    state->dataHeatBalFanSys->MDotOA.allocate(1);
    state->dataHeatBalFanSys->MDotOA(1) = 0.0;
    state->dataHeatBalFanSys->MCPTI.allocate(1);
    state->dataHeatBalFanSys->MCPTI(1) = 0.0;
    state->dataHeatBalFanSys->MCPTV.allocate(1);
    state->dataHeatBalFanSys->MCPTM.allocate(1);
    state->dataHeatBalFanSys->MCPTM(1) = 0.0;
    state->dataHeatBalFanSys->MCPTE.allocate(1);
    state->dataHeatBalFanSys->MCPTE(1) = 0.0;
    state->dataHeatBalFanSys->MCPTC.allocate(1);
    state->dataHeatBalFanSys->MCPTC(1) = 0.0;
    state->dataSurface->SurfaceWindow.allocate(1);
    state->dataSurface->Surface.allocate(2);
    state->dataHeatBalSurf->SurfHConvInt.allocate(1);
    state->dataZoneTempPredictorCorrector->ZoneAirRelHum.allocate(1);
    state->dataRoomAirMod->IsZoneDV.dimension(1, false);
    state->dataRoomAirMod->IsZoneCV.dimension(1, false);
    state->dataRoomAirMod->IsZoneUI.dimension(1, false);
    state->dataRoomAirMod->ZoneDVMixedFlag.allocate(1);
    state->dataHeatBal->ZnAirRpt.allocate(1);
    state->dataZoneEquip->ZoneEquipConfig.allocate(1);
    state->dataZoneEquip->ZoneEquipConfig(1).ActualZoneNum = 1;
    state->dataSize->ZoneEqSizing.allocate(1);

    // CorrectZoneContaminants variable initialization
    state->dataHeatBalFanSys->MixingMassFlowZone.allocate(1);
    state->dataHeatBalFanSys->MixingMassFlowZone(1) = 0.0;
    state->dataHeatBalFanSys->ZoneW1.allocate(1);
    state->dataHeatBalFanSys->ZoneAirHumRatTemp.allocate(1);
    state->dataHeatBalFanSys->OAMFL.allocate(1);
    state->dataHeatBalFanSys->OAMFL(1) = 0.0;
    state->dataHeatBalFanSys->VAMFL.allocate(1);
    state->dataHeatBalFanSys->VAMFL(1) = 0.0;
    state->dataHeatBalFanSys->EAMFL.allocate(1);
    state->dataHeatBalFanSys->EAMFL(1) = 0.0;
    state->dataHeatBalFanSys->EAMFLxHumRat.allocate(1);
    state->dataHeatBalFanSys->EAMFLxHumRat(1) = 0.0;
    state->dataHeatBalFanSys->CTMFL.allocate(1);
    state->dataHeatBalFanSys->CTMFL(1) = 0.0;
    state->dataHeatBalFanSys->ZT.allocate(1);
    state->dataHeatBalFanSys->ZT(1) = 0.0;
    state->dataContaminantBalance->AZ.allocate(1);
    state->dataContaminantBalance->BZ.allocate(1);
    state->dataContaminantBalance->CZ.allocate(1);
    state->dataContaminantBalance->AZGC.allocate(1);
    state->dataContaminantBalance->BZGC.allocate(1);
    state->dataContaminantBalance->CZGC.allocate(1);
    state->dataContaminantBalance->AZ(1) = 0.0;
    state->dataContaminantBalance->BZ(1) = 0.0;
    state->dataContaminantBalance->CZ(1) = 0.0;
    state->dataContaminantBalance->AZGC(1) = 0.0;
    state->dataContaminantBalance->BZGC(1) = 0.0;
    state->dataContaminantBalance->CZGC(1) = 0.0;
    state->dataContaminantBalance->ZoneAirCO2.allocate(1);
    state->dataContaminantBalance->ZoneAirCO2(1) = 0.0;
    state->dataContaminantBalance->ZoneAirCO2Temp.allocate(1);
    state->dataContaminantBalance->ZoneAirCO2Temp(1) = 0.0;
    state->dataContaminantBalance->ZoneAirDensityCO.allocate(1);
    state->dataContaminantBalance->ZoneAirDensityCO(1) = 0.0;
    state->dataContaminantBalance->ZoneCO2Gain.allocate(1);
    state->dataContaminantBalance->ZoneCO2Gain(1) = 0.0;
    state->dataContaminantBalance->ZoneCO2GainExceptPeople.allocate(1);
    state->dataContaminantBalance->ZoneCO2GainExceptPeople(1) = 0.0;
    state->dataContaminantBalance->ZoneGCGain.allocate(1);
    state->dataContaminantBalance->ZoneGCGain(1) = 0.0;
    state->dataContaminantBalance->MixingMassFlowCO2.allocate(1);
    state->dataContaminantBalance->MixingMassFlowCO2(1) = 0.0;

    // Parameter setup
    state->dataGlobal->NumOfZones = 1;
    state->dataSize->CurZoneEqNum = 1;
    state->dataZonePlenum->NumZoneReturnPlenums = 0;
    state->dataZonePlenum->NumZoneSupplyPlenums = 0;
    state->afn->SimulateAirflowNetwork = 0;
    state->dataHeatBal->Zone(1).IsControlled = true;
    state->dataHeatBal->Zone(1).ZoneEqNum = 1;
    state->dataHeatBal->Zone(1).Multiplier = 1;
    state->dataHeatBal->Zone(1).SystemZoneNodeNumber = 1;
    state->dataHeatBal->Zone(1).HTSurfaceFirst = 0;
    state->dataHeatBal->Zone(1).HTSurfaceLast = -1;
    state->dataHeatBal->Zone(1).Volume = 4000;
    state->dataGlobal->TimeStepZone = 10.0 / 60.0; // Zone timestep in hours
    state->dataHVACGlobal->TimeStepSys = 10.0 / 60.0;

    // Hybrid modeling trigger
    state->dataHybridModel->FlagHybridModel_TM = false;
    state->dataGlobal->WarmupFlag = false;
    state->dataGlobal->DoingSizing = false;
    state->dataEnvrn->DayOfYear = 1;

    // Case 1: Hybrid model infiltration with measured CO2 concentration (free-floating)

    state->dataContaminantBalance->Contaminant.CO2Simulation = true;
    state->dataHybridModel->HybridModelZone(1).InternalThermalMassCalc_T = false;
    state->dataHybridModel->HybridModelZone(1).InfiltrationCalc_T = false;
    state->dataHybridModel->HybridModelZone(1).InfiltrationCalc_H = false;
    state->dataHybridModel->HybridModelZone(1).InfiltrationCalc_C = true;
    state->dataHybridModel->HybridModelZone(1).PeopleCountCalc_T = false;
    state->dataHybridModel->HybridModelZone(1).PeopleCountCalc_H = false;
    state->dataHybridModel->HybridModelZone(1).PeopleCountCalc_C = false;
    state->dataHybridModel->HybridModelZone(1).HybridStartDayOfYear = 1;
    state->dataHybridModel->HybridModelZone(1).HybridEndDayOfYear = 2;
    state->dataHeatBal->Zone(1).ZoneVolCapMultpCO2 = 1.0;
    state->dataHeatBalFanSys->ZoneAirHumRat(1) = 0.001120003;
    state->dataContaminantBalance->OutdoorCO2 = 387.6064554;
    state->dataEnvrn->OutHumRat = 0.001147;
    state->dataEnvrn->OutBaroPress = 99500;
    state->dataContaminantBalance->CO2ZoneTimeMinus1(1) = 388.595225;
    state->dataContaminantBalance->CO2ZoneTimeMinus2(1) = 389.084601;
    state->dataContaminantBalance->CO2ZoneTimeMinus3(1) = 388.997009;
    state->dataHybridModel->HybridModelZone(1).ZoneMeasuredCO2ConcentrationSchedulePtr = 1;
    state->dataScheduleMgr->Schedule(state->dataHybridModel->HybridModelZone(1).ZoneMeasuredCO2ConcentrationSchedulePtr).CurrentValue = 388.238646;

    CorrectZoneContaminants(*state, false, true, 10 / 60);
    EXPECT_NEAR(0.5, state->dataHeatBal->Zone(1).InfilOAAirChangeRateHM, 0.01);

    // Case 2: Hybrid model people count with measured CO2 concentration (free-floating)

    state->dataContaminantBalance->Contaminant.CO2Simulation = true;
    state->dataHybridModel->HybridModelZone(1).InternalThermalMassCalc_T = false;
    state->dataHybridModel->HybridModelZone(1).InfiltrationCalc_T = false;
    state->dataHybridModel->HybridModelZone(1).InfiltrationCalc_H = false;
    state->dataHybridModel->HybridModelZone(1).InfiltrationCalc_C = false;
    state->dataHybridModel->HybridModelZone(1).PeopleCountCalc_T = false;
    state->dataHybridModel->HybridModelZone(1).PeopleCountCalc_H = false;
    state->dataHybridModel->HybridModelZone(1).PeopleCountCalc_C = true;
    state->dataHybridModel->HybridModelZone(1).HybridStartDayOfYear = 1;
    state->dataHybridModel->HybridModelZone(1).HybridEndDayOfYear = 2;
    state->dataHeatBal->Zone(1).Volume = 4000;
    state->dataHeatBal->Zone(1).ZoneVolCapMultpCO2 = 1.0;
    state->dataHeatBal->Zone(1).OutDryBulbTemp = -1.0394166434012677;
    state->dataHeatBalFanSys->ZT(1) = -2.92;
    state->dataHeatBalFanSys->ZoneAirHumRat(1) = 0.00112;
    state->dataContaminantBalance->OutdoorCO2 = 387.6064554;
    state->dataEnvrn->OutBaroPress = 98916.7;
    state->dataHeatBalFanSys->OAMFL(1) = 0.700812;
    state->dataContaminantBalance->ZoneCO2Gain(1) = 0.00001989;
    state->dataContaminantBalance->CO2ZoneTimeMinus1(1) = 387.9962885;
    state->dataContaminantBalance->CO2ZoneTimeMinus2(1) = 387.676037;
    state->dataContaminantBalance->CO2ZoneTimeMinus3(1) = 387.2385685;
    state->dataHybridModel->HybridModelZone(1).ZoneMeasuredCO2ConcentrationSchedulePtr = 1;
    state->dataScheduleMgr->Schedule(state->dataHybridModel->HybridModelZone(1).ZoneMeasuredCO2ConcentrationSchedulePtr).CurrentValue = 389.8511796;
    CorrectZoneContaminants(*state, false, true, 10 / 60);
    EXPECT_NEAR(4, state->dataHeatBal->Zone(1).NumOccHM, 0.1);

    // Case 3: Hybrid model infiltration with measured CO2 concentration (with HVAC)
    state->dataContaminantBalance->Contaminant.CO2Simulation = true;
    state->dataHybridModel->HybridModelZone(1).InternalThermalMassCalc_T = false;
    state->dataHybridModel->HybridModelZone(1).InfiltrationCalc_T = false;
    state->dataHybridModel->HybridModelZone(1).InfiltrationCalc_H = false;
    state->dataHybridModel->HybridModelZone(1).InfiltrationCalc_C = true;
    state->dataHybridModel->HybridModelZone(1).PeopleCountCalc_T = false;
    state->dataHybridModel->HybridModelZone(1).PeopleCountCalc_H = false;
    state->dataHybridModel->HybridModelZone(1).PeopleCountCalc_C = false;
    state->dataHybridModel->HybridModelZone(1).IncludeSystemSupplyParameters = true;
    state->dataHybridModel->HybridModelZone(1).HybridStartDayOfYear = 1;
    state->dataHybridModel->HybridModelZone(1).HybridEndDayOfYear = 2;
    state->dataHeatBal->Zone(1).ZoneVolCapMultpCO2 = 1.0;
    state->dataHeatBalFanSys->ZT(1) = 15.56;
    state->dataHeatBalFanSys->ZoneAirHumRat(1) = 0.00809;
    state->dataHeatBal->Zone(1).OutDryBulbTemp = -10.7;
    state->dataEnvrn->OutBaroPress = 99500;
    state->dataContaminantBalance->ZoneCO2Gain(1) = 0.0;
    state->dataContaminantBalance->CO2ZoneTimeMinus1(1) = 388.54049;
    state->dataContaminantBalance->CO2ZoneTimeMinus2(1) = 389.0198771;
    state->dataContaminantBalance->CO2ZoneTimeMinus3(1) = 388.9201464;
    state->dataHybridModel->HybridModelZone(1).ZoneMeasuredCO2ConcentrationSchedulePtr = 1;
    state->dataHybridModel->HybridModelZone(1).ZoneSupplyAirCO2ConcentrationSchedulePtr = 2;
    state->dataHybridModel->HybridModelZone(1).ZoneSupplyAirMassFlowRateSchedulePtr = 3;
    state->dataScheduleMgr->Schedule(state->dataHybridModel->HybridModelZone(1).ZoneMeasuredCO2ConcentrationSchedulePtr).CurrentValue = 388.2075472;
    state->dataScheduleMgr->Schedule(state->dataHybridModel->HybridModelZone(1).ZoneSupplyAirCO2ConcentrationSchedulePtr).CurrentValue = 388.54049;
    state->dataScheduleMgr->Schedule(state->dataHybridModel->HybridModelZone(1).ZoneSupplyAirMassFlowRateSchedulePtr).CurrentValue = 0.898375186;

    CorrectZoneContaminants(*state, false, true, 10 / 60);
    EXPECT_NEAR(0.5, state->dataHeatBal->Zone(1).InfilOAAirChangeRateHM, 0.01);

    // Case 4: Hybrid model people count with measured CO2 concentration (with HVAC)

    state->dataContaminantBalance->Contaminant.CO2Simulation = true;
    state->dataHybridModel->HybridModelZone(1).InternalThermalMassCalc_T = false;
    state->dataHybridModel->HybridModelZone(1).InfiltrationCalc_T = false;
    state->dataHybridModel->HybridModelZone(1).InfiltrationCalc_H = false;
    state->dataHybridModel->HybridModelZone(1).InfiltrationCalc_C = false;
    state->dataHybridModel->HybridModelZone(1).PeopleCountCalc_T = false;
    state->dataHybridModel->HybridModelZone(1).PeopleCountCalc_H = false;
    state->dataHybridModel->HybridModelZone(1).PeopleCountCalc_C = true;
    state->dataHybridModel->HybridModelZone(1).IncludeSystemSupplyParameters = true;
    state->dataHybridModel->HybridModelZone(1).HybridStartDayOfYear = 1;
    state->dataHybridModel->HybridModelZone(1).HybridEndDayOfYear = 2;
    state->dataHeatBal->Zone(1).ZoneVolCapMultpCO2 = 1.0;
    state->dataHeatBalFanSys->ZT(1) = 21.1;
    state->dataHeatBalFanSys->ZoneAirHumRat(1) = 0.01102;
    state->dataEnvrn->OutBaroPress = 98933.3;
    state->dataContaminantBalance->ZoneCO2Gain(1) = 0.00003333814;
    state->dataContaminantBalance->ZoneCO2GainExceptPeople(1) = 0.0;
    state->dataContaminantBalance->CO2ZoneTimeMinus1(1) = 387.2253194;
    state->dataContaminantBalance->CO2ZoneTimeMinus2(1) = 387.1898423;
    state->dataContaminantBalance->CO2ZoneTimeMinus3(1) = 387.4064128;
    state->dataHybridModel->HybridModelZone(1).ZoneMeasuredCO2ConcentrationSchedulePtr = 1;
    state->dataHybridModel->HybridModelZone(1).ZoneSupplyAirCO2ConcentrationSchedulePtr = 2;
    state->dataHybridModel->HybridModelZone(1).ZoneSupplyAirMassFlowRateSchedulePtr = 3;
    state->dataHybridModel->HybridModelZone(1).ZonePeopleActivityLevelSchedulePtr = 4;
    state->dataHybridModel->HybridModelZone(1).ZonePeopleSensibleFractionSchedulePtr = 5;
    state->dataHybridModel->HybridModelZone(1).ZonePeopleRadiationFractionSchedulePtr = 6;
    state->dataHybridModel->HybridModelZone(1).ZonePeopleCO2GenRateSchedulePtr = 7;
    state->dataScheduleMgr->Schedule(state->dataHybridModel->HybridModelZone(1).ZoneMeasuredCO2ConcentrationSchedulePtr).CurrentValue = 389.795807;
    state->dataScheduleMgr->Schedule(state->dataHybridModel->HybridModelZone(1).ZoneSupplyAirCO2ConcentrationSchedulePtr).CurrentValue = 387.2253194;
    state->dataScheduleMgr->Schedule(state->dataHybridModel->HybridModelZone(1).ZoneSupplyAirMassFlowRateSchedulePtr).CurrentValue = 1.427583795;
    state->dataScheduleMgr->Schedule(state->dataHybridModel->HybridModelZone(1).ZonePeopleActivityLevelSchedulePtr).CurrentValue = 120;
    state->dataScheduleMgr->Schedule(state->dataHybridModel->HybridModelZone(1).ZonePeopleSensibleFractionSchedulePtr).CurrentValue = 0.6;
    state->dataScheduleMgr->Schedule(state->dataHybridModel->HybridModelZone(1).ZonePeopleRadiationFractionSchedulePtr).CurrentValue = 0.3;
    state->dataScheduleMgr->Schedule(state->dataHybridModel->HybridModelZone(1).ZonePeopleCO2GenRateSchedulePtr).CurrentValue = 0.0000000382;

    CorrectZoneContaminants(*state, false, true, 10 / 60);
    EXPECT_NEAR(7.27, state->dataHeatBal->Zone(1).NumOccHM, 0.1);

    // Deallocate everything
    state->dataHeatBal->Zone.deallocate();
    state->dataHybridModel->HybridModelZone.deallocate();
    state->dataRoomAirMod->AirModel.deallocate();
    state->dataRoomAirMod->ZTOC.deallocate();
    state->dataContaminantBalance->CO2ZoneTimeMinus1Temp.deallocate();
    state->dataContaminantBalance->CO2ZoneTimeMinus2Temp.deallocate();
    state->dataContaminantBalance->CO2ZoneTimeMinus3Temp.deallocate();
    state->dataContaminantBalance->CO2ZoneTimeMinus1.deallocate();
    state->dataContaminantBalance->CO2ZoneTimeMinus2.deallocate();
    state->dataContaminantBalance->CO2ZoneTimeMinus3.deallocate();
    state->dataHeatBalFanSys->AIRRAT.deallocate();
    state->dataHeatBalFanSys->ZoneAirHumRat.deallocate();
    state->dataHeatBalFanSys->NonAirSystemResponse.deallocate();
    state->dataHeatBalFanSys->SysDepZoneLoadsLagged.deallocate();
    state->afn->exchangeData.deallocate();
    state->dataLoopNodes->Node.deallocate();
    state->dataHeatBalFanSys->TempTstatAir.deallocate();
    state->dataHeatBalFanSys->LoadCorrectionFactor.deallocate();
    state->dataHeatBalFanSys->MAT.deallocate();
    state->dataHeatBalFanSys->ZT.deallocate();
    state->dataHeatBalFanSys->PreviousMeasuredZT1.deallocate();
    state->dataHeatBalFanSys->PreviousMeasuredZT2.deallocate();
    state->dataHeatBalFanSys->PreviousMeasuredZT3.deallocate();
    state->dataHeatBalFanSys->MCPI.deallocate();
    state->dataHeatBalFanSys->MCPV.deallocate();
    state->dataHeatBalFanSys->MCPM.deallocate();
    state->dataHeatBalFanSys->MCPE.deallocate();
    state->dataHeatBalFanSys->MCPC.deallocate();
    state->dataHeatBalFanSys->MDotCPOA.deallocate();
    state->dataHeatBalFanSys->MDotOA.deallocate();
    state->dataHeatBalFanSys->MCPTI.deallocate();
    state->dataHeatBalFanSys->MCPTV.deallocate();
    state->dataHeatBalFanSys->MCPTM.deallocate();
    state->dataHeatBalFanSys->MCPTE.deallocate();
    state->dataHeatBalFanSys->MCPTC.deallocate();
    state->dataSurface->SurfaceWindow.deallocate();
    state->dataSurface->Surface.deallocate();
    state->dataHeatBalSurf->SurfHConvInt.deallocate();
    state->dataZoneTempPredictorCorrector->ZoneAirRelHum.deallocate();
    state->dataRoomAirMod->IsZoneDV.deallocate();
    state->dataRoomAirMod->IsZoneCV.deallocate();
    state->dataRoomAirMod->IsZoneUI.deallocate();
    state->dataRoomAirMod->ZoneDVMixedFlag.deallocate();
    state->dataHeatBal->ZnAirRpt.deallocate();
    state->dataZoneEquip->ZoneEquipConfig.deallocate();
    state->dataSize->ZoneEqSizing.deallocate();
    state->dataHeatBalFanSys->MixingMassFlowZone.deallocate();
    state->dataHeatBalFanSys->ZoneW1.deallocate();
    state->dataHeatBalFanSys->ZoneAirHumRatTemp.deallocate();
    state->dataHeatBalFanSys->OAMFL.deallocate();
    state->dataHeatBalFanSys->VAMFL.deallocate();
    state->dataHeatBalFanSys->EAMFL.deallocate();
    state->dataHeatBalFanSys->EAMFLxHumRat.deallocate();
    state->dataHeatBalFanSys->CTMFL.deallocate();
    state->dataContaminantBalance->ZoneAirCO2.deallocate();
    state->dataContaminantBalance->ZoneAirCO2Temp.deallocate();
    state->dataContaminantBalance->ZoneAirDensityCO.deallocate();
    state->dataContaminantBalance->ZoneCO2Gain.deallocate();
    state->dataContaminantBalance->ZoneCO2GainExceptPeople.deallocate();
    state->dataContaminantBalance->ZoneGCGain.deallocate();
    state->dataContaminantBalance->MixingMassFlowCO2.deallocate();
    state->dataScheduleMgr->Schedule.deallocate();
}
