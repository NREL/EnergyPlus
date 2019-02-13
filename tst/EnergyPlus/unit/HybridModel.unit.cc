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

// EnergyPlus::ZoneTempPredictorCorrector Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

#include "Fixtures/EnergyPlusFixture.hh"

// EnergyPlus Headers
#include <AirflowNetwork/Elements.hpp>
#include <EnergyPlus/DataContaminantBalance.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
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
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/ZoneContaminantPredictorCorrector.hh>
#include <EnergyPlus/ZonePlenum.hh>
#include <EnergyPlus/ZoneTempPredictorCorrector.hh>

using namespace EnergyPlus;
using namespace ObjexxFCL;
using namespace EnergyPlus::DataHeatBalance;
using namespace EnergyPlus::DataContaminantBalance;
using namespace EnergyPlus::DataHeatBalFanSys;
using namespace DataGlobals;
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

TEST_F(EnergyPlusFixture, ZoneTempPredictorCorrector_CorrectZoneAirTempTest)
{

    // ZoneTempPredictorCorrector variable initialization
    Zone.allocate(1);
    HybridModelZone.allocate(1);
    AirModel.allocate(1);
    ZTM1.allocate(1);
    ZTM2.allocate(1);
    ZTM3.allocate(1);
    XMAT.allocate(1);
    XM2T.allocate(1);
    XM3T.allocate(1);
    ZTOC.allocate(1);
    ZTMX.allocate(1);
    ZTM1MX.allocate(1);
    WZoneTimeMinus1Temp.allocate(1);
    WZoneTimeMinus2Temp.allocate(1);
    WZoneTimeMinus3Temp.allocate(1);
    WZoneTimeMinus1.allocate(1);
    WZoneTimeMinus2.allocate(1);
    WZoneTimeMinus3.allocate(1);
    AIRRAT.allocate(1);
    ZoneAirHumRat.allocate(1);
    NonAirSystemResponse.allocate(1);
    NonAirSystemResponse(1) = 0.0;
    SysDepZoneLoadsLagged.allocate(1);
    SysDepZoneLoadsLagged(1) = 0.0;
    AirflowNetwork::AirflowNetworkExchangeData.allocate(1);
    Node.allocate(1);
    TempTstatAir.allocate(1);
    LoadCorrectionFactor.allocate(1);
    MAT.allocate(1);
    ZT.allocate(1);
    PreviousMeasuredZT1.allocate(1);
    PreviousMeasuredZT2.allocate(1);
    PreviousMeasuredZT3.allocate(1);
    PreviousMeasuredHumRat1.allocate(1);
    PreviousMeasuredHumRat2.allocate(1);
    PreviousMeasuredHumRat3.allocate(1);
    CO2ZoneTimeMinus1Temp.allocate(1);
    CO2ZoneTimeMinus2Temp.allocate(1);
    CO2ZoneTimeMinus3Temp.allocate(1);
    CO2ZoneTimeMinus1.allocate(1);
    CO2ZoneTimeMinus2.allocate(1);
    CO2ZoneTimeMinus3.allocate(1);
    Schedule.allocate(1);

    // CalcZoneComponentLoadSums variable initialization
    MCPI.allocate(1);
    MCPI(1) = 0.0;
    MCPV.allocate(1);
    MCPM.allocate(1);
    MCPM(1) = 0.0;
    MCPE.allocate(1);
    MCPE(1) = 0.0;
    MCPC.allocate(1);
    MCPC(1) = 0.0;
    MDotCPOA.allocate(1);
    MDotCPOA(1) = 0.0;
    MDotOA.allocate(1);
    MDotOA(1) = 0.0;
    MCPTI.allocate(1);
    MCPTI(1) = 0.0;
    MCPTV.allocate(1);
    MCPTM.allocate(1);
    MCPTM(1) = 0.0;
    MCPTE.allocate(1);
    MCPTE(1) = 0.0;
    MCPTC.allocate(1);
    MCPTC(1) = 0.0;
    SurfaceWindow.allocate(1);
    Surface.allocate(2);
    HConvIn.allocate(1);
    SNLoadHeatRate.allocate(1);
    SNLoadCoolRate.allocate(1);
    SNLoadHeatEnergy.allocate(1);
    SNLoadCoolEnergy.allocate(1);
    ZoneAirRelHum.allocate(1);
    IsZoneDV.dimension(1, false);
    IsZoneCV.dimension(1, false);
    IsZoneUI.dimension(1, false);
    ZoneDVMixedFlag.allocate(1);
    ZnAirRpt.allocate(1);
    ZoneEquipConfig.allocate(1);
    ZoneEquipConfig(1).ActualZoneNum = 1;
    ZoneIntGain.allocate(1);
    ZoneIntGain(1).NumberOfDevices = 0;
    ZoneEqSizing.allocate(1);

    // CorrectZoneHumRat variable initialization
    ZoneLatentGain.allocate(1);
    ZoneLatentGain(1) = 0.0;
    ZoneLatentGainExceptPeople.allocate(1);
    ZoneLatentGainExceptPeople(1) = 0.0;
    SumLatentHTRadSys.allocate(1);
    SumLatentHTRadSys(1) = 0.0;
    SumHmARaW.allocate(1);
    SumHmARaW(1) = 0.0;
    SumConvHTRadSys.allocate(1);
    SumConvHTRadSys(1) = 0.0;
    SumConvPool.allocate(1);
    SumConvPool(1) = 0.0;
    SumHmARa.allocate(1);
    SumHmARa(1) = 0.0;
    MixingMassFlowXHumRat.allocate(1);
    MixingMassFlowXHumRat(1) = 0.0;
    MixingMassFlowZone.allocate(1);
    MixingMassFlowZone(1) = 0.0;
    ZoneW1.allocate(1);
    ZoneAirHumRatTemp.allocate(1);
    SumLatentPool.allocate(1);
    SumLatentPool(1) = 0.0;
    OAMFL.allocate(1);
    OAMFL(1) = 0.0;
    VAMFL.allocate(1);
    VAMFL(1) = 0.0;
    EAMFL.allocate(1);
    EAMFL(1) = 0.0;
    EAMFLxHumRat.allocate(1);
    EAMFLxHumRat(1) = 0.0;
    CTMFL.allocate(1);
    CTMFL(1) = 0.0;
    ZT.allocate(1);
    ZT(1) = 0.0;

    // CorrectZoneContaminants variable initialization
    AZ.allocate(1);
    BZ.allocate(1);
    CZ.allocate(1);
    AZGC.allocate(1);
    BZGC.allocate(1);
    CZGC.allocate(1);
    AZ(1) = 0.0;
    BZ(1) = 0.0;
    CZ(1) = 0.0;
    AZGC(1) = 0.0;
    BZGC(1) = 0.0;
    CZGC(1) = 0.0;
    ZoneAirCO2.allocate(1);
    ZoneAirCO2(1) = 0.0;
    ZoneAirCO2Temp.allocate(1);
    ZoneAirCO2Temp(1) = 0.0;
    ZoneAirDensityCO.allocate(1);
    ZoneAirDensityCO(1) = 0.0;
    ZoneCO2Gain.allocate(1);
    ZoneCO2Gain(1) = 0.0;
    ZoneCO2GainExceptPeople.allocate(1);
    ZoneCO2GainExceptPeople(1) = 0.0;
    ZoneGCGain.allocate(1);
    ZoneGCGain(1) = 0.0;
    MixingMassFlowCO2.allocate(1);
    MixingMassFlowCO2(1) = 0.0;
    Real64 CO2MassFlowRate(0.0);

    // Parameter setup
    NumOfZones = 1;
    CurZoneEqNum = 1;
    NumZoneReturnPlenums = 0;
    NumZoneSupplyPlenums = 0;
    AirflowNetwork::SimulateAirflowNetwork = 0;
    Zone(1).IsControlled = true;
    Zone(1).ZoneEqNum = 1;
    Zone(1).Multiplier = 1;
    Zone(1).SystemZoneNodeNumber = 1;
    Zone(1).SurfaceFirst = 1;
    Zone(1).SurfaceLast = 2;
    Zone(1).Volume = 1061.88;
    TimeStepZone = 10.0 / 60.0; // Zone timestep in hours
    TimeStepSys = 10.0 / 60.0;
    Real64 ZoneTempChange;

    // Hybrid modeling trigger
    FlagHybridModel_TM = true;
    WarmupFlag = false;
    DoingSizing = false;
    DayOfYear = 1;

    // Case 1: Hybrid model internal thermal mass

    HybridModelZone(1).InternalThermalMassCalc_T = true;
    HybridModelZone(1).InfiltrationCalc_T = false;
    HybridModelZone(1).InfiltrationCalc_H = false;
    HybridModelZone(1).InfiltrationCalc_C = false;
    HybridModelZone(1).PeopleCountCalc_T = false;
    HybridModelZone(1).PeopleCountCalc_H = false;
    HybridModelZone(1).PeopleCountCalc_C = false;
    HybridModelZone(1).HybridStartDayOfYear = 1;
    HybridModelZone(1).HybridEndDayOfYear = 2;
    MAT(1) = 0.0;
    PreviousMeasuredZT1(1) = 0.1;
    PreviousMeasuredZT2(1) = 0.2;
    PreviousMeasuredZT3(1) = 0.3;
    Zone(1).OutDryBulbTemp = -5.21;
    ZoneAirHumRat(1) = 0.002083;
    MCPV(1) = 1414.60;   // Assign TempDepCoef
    MCPTV(1) = -3335.10; // Assign TempIndCoef
    OutBaroPress = 99166.67;

    CorrectZoneAirTemp(ZoneTempChange, false, true, 10 / 60);
    EXPECT_NEAR(15.13, Zone(1).ZoneVolCapMultpSensHM, 0.01);

    // Case 2: Hybrid model infiltration with measured temperature

    HybridModelZone(1).InternalThermalMassCalc_T = false;
    HybridModelZone(1).InfiltrationCalc_T = true;
    HybridModelZone(1).InfiltrationCalc_H = false;
    HybridModelZone(1).InfiltrationCalc_C = false;
    HybridModelZone(1).PeopleCountCalc_T = false;
    HybridModelZone(1).PeopleCountCalc_H = false;
    HybridModelZone(1).PeopleCountCalc_C = false;
    HybridModelZone(1).HybridStartDayOfYear = 1;
    HybridModelZone(1).HybridEndDayOfYear = 2;
    MAT(1) = 0.0;
    PreviousMeasuredZT1(1) = 0.02;
    PreviousMeasuredZT2(1) = 0.04;
    PreviousMeasuredZT3(1) = 0.06;
    Zone(1).ZoneVolCapMultpSens = 8.0;
    Zone(1).OutDryBulbTemp = -6.71;
    ZoneAirHumRat(1) = 0.002083;
    MCPV(1) = 539.49;  // Assign TempDepCoef
    MCPTV(1) = 270.10; // Assign TempIndCoef
    OutBaroPress = 99250;

    CorrectZoneAirTemp(ZoneTempChange, false, true, 10 / 60);
    EXPECT_NEAR(0.2444, Zone(1).InfilOAAirChangeRateHM, 0.01);

    // Case 3: Hybrid model infiltration with measured humidity ratio

    HybridModelZone(1).InternalThermalMassCalc_T = false;
    HybridModelZone(1).InfiltrationCalc_T = false;
    HybridModelZone(1).InfiltrationCalc_H = true;
    HybridModelZone(1).InfiltrationCalc_C = false;
    HybridModelZone(1).PeopleCountCalc_T = false;
    HybridModelZone(1).PeopleCountCalc_H = false;
    HybridModelZone(1).PeopleCountCalc_C = false;
    HybridModelZone(1).HybridStartDayOfYear = 1;
    HybridModelZone(1).HybridEndDayOfYear = 2;
    Zone(1).Volume = 4000;
    Zone(1).OutDryBulbTemp = -10.62;
    Zone(1).ZoneVolCapMultpMoist = 1.0;
    ZoneAirHumRat(1) = 0.001120003;
    ZT(1) = -6.08;
    OutHumRat = 0.0011366887816818931;
    PreviousMeasuredHumRat1(1) = 0.0011186324286;
    PreviousMeasuredHumRat2(1) = 0.0011172070768;
    PreviousMeasuredHumRat3(1) = 0.0011155109625;
    HybridModelZone(1).ZoneMeasuredHumidityRatioSchedulePtr = 1;
    Schedule(HybridModelZone(1).ZoneMeasuredHumidityRatioSchedulePtr).CurrentValue = 0.001120003;
    MCPV(1) = 539.49;
    MCPTV(1) = 270.10;
    OutBaroPress = 99500;

    CorrectZoneHumRat(1);
    EXPECT_NEAR(0.5, Zone(1).InfilOAAirChangeRateHM, 0.01);

    // Case 4: Hybrid model infiltration with measured CO2 concentration

    Contaminant.CO2Simulation = true;
    HybridModelZone(1).InternalThermalMassCalc_T = false;
    HybridModelZone(1).InfiltrationCalc_T = false;
    HybridModelZone(1).InfiltrationCalc_H = false;
    HybridModelZone(1).InfiltrationCalc_C = true;
    HybridModelZone(1).PeopleCountCalc_T = false;
    HybridModelZone(1).PeopleCountCalc_H = false;
    HybridModelZone(1).PeopleCountCalc_C = false;
    HybridModelZone(1).HybridStartDayOfYear = 1;
    HybridModelZone(1).HybridEndDayOfYear = 2;
    Zone(1).Volume = 4000;
    Zone(1).ZoneVolCapMultpCO2 = 1.0;
    OutdoorCO2 = 387.6064554;
    OutHumRat = 0.001147;
    OutBaroPress = 99500;
    CO2ZoneTimeMinus1(1) = 388.595225;
    CO2ZoneTimeMinus2(1) = 389.084601;
    CO2ZoneTimeMinus3(1) = 388.997009;
    HybridModelZone(1).ZoneMeasuredCO2ConcentrationSchedulePtr = 1;
    Schedule(HybridModelZone(1).ZoneMeasuredCO2ConcentrationSchedulePtr).CurrentValue = 388.238646;

    CorrectZoneContaminants(false, true, 10 / 60);
    EXPECT_NEAR(0.4965, Zone(1).InfilOAAirChangeRateHM, 0.01);

    // Case 5: Hybrid model people count with measured temperature



    // Case 6: Hybrid model people count with measured humidity ratio

    HybridModelZone(1).InternalThermalMassCalc_T = false;
    HybridModelZone(1).InfiltrationCalc_T = false;
    HybridModelZone(1).InfiltrationCalc_H = false;
    HybridModelZone(1).InfiltrationCalc_C = false;
    HybridModelZone(1).PeopleCountCalc_T = false;
    HybridModelZone(1).PeopleCountCalc_H = true;
    HybridModelZone(1).PeopleCountCalc_C = false;
    HybridModelZone(1).HybridStartDayOfYear = 1;
    HybridModelZone(1).HybridEndDayOfYear = 2;
    Zone(1).Volume = 4000;
    Zone(1).OutDryBulbTemp = -10.62;
    Zone(1).ZoneVolCapMultpMoist = 1.0;
    ZoneAirHumRat(1) = 0.0024964;
    ZT(1) = -2.92;
    OutHumRat = 0.0025365002784602363;
	OutBaroPress = 98916.7;
    OAMFL(1) = 0.700812;
    ZoneLatentGain(1) = 211.2;
    ZoneLatentGainExceptPeople(1) = 0.0;
    PreviousMeasuredHumRat1(1) = 0.002496356;
    PreviousMeasuredHumRat2(1) = 0.002489048;
    PreviousMeasuredHumRat3(1) = 0.002480404;
    HybridModelZone(1).ZoneMeasuredHumidityRatioSchedulePtr = 1;
    Schedule(HybridModelZone(1).ZoneMeasuredHumidityRatioSchedulePtr).CurrentValue = 0.002506251487737;
    
    CorrectZoneHumRat(1);
    EXPECT_NEAR(4, Zone(1).NumOccHM, 0.1);

    // Case 7: Hybrid model people count with measured CO2 concentration
    Contaminant.CO2Simulation = true;
    HybridModelZone(1).InternalThermalMassCalc_T = false;
    HybridModelZone(1).InfiltrationCalc_T = false;
    HybridModelZone(1).InfiltrationCalc_H = false;
    HybridModelZone(1).InfiltrationCalc_C = false;
    HybridModelZone(1).PeopleCountCalc_T = false;
    HybridModelZone(1).PeopleCountCalc_H = false;
    HybridModelZone(1).PeopleCountCalc_C = true;
    HybridModelZone(1).HybridStartDayOfYear = 1;
    HybridModelZone(1).HybridEndDayOfYear = 2;
    Zone(1).Volume = 4000;
    Zone(1).ZoneVolCapMultpCO2 = 1.0;
    Zone(1).OutDryBulbTemp = -1.0394166434012677;
    ZT(1) = -2.92;
    ZoneAirHumRat(1) = 0.00112;
    OutdoorCO2 = 387.6064554;
    OutBaroPress = 98916.7;
    OAMFL(1) = 0.700812;
    Real64 CO2GainExceptPeople(0.0);
    ZoneCO2Gain(1) = 0.00001989;
    CO2ZoneTimeMinus1(1) = 387.9962885;
    CO2ZoneTimeMinus2(1) = 387.676037;
    CO2ZoneTimeMinus3(1) = 387.2385685;
    HybridModelZone(1).ZoneMeasuredCO2ConcentrationSchedulePtr = 1;
    Schedule(HybridModelZone(1).ZoneMeasuredCO2ConcentrationSchedulePtr).CurrentValue = 389.8511796;

    CorrectZoneContaminants(false, true, 10 / 60);
    EXPECT_NEAR(4, Zone(1).NumOccHM, 0.1);

    // Deallocate everything
	HybridModel::clear_state();
    Zone.deallocate();
    AirModel.deallocate();
    ZTM1.deallocate();
    ZTM2.deallocate();
    ZTM3.deallocate();
    XMAT.deallocate();
    XM2T.deallocate();
    XM3T.deallocate();
    ZTOC.deallocate();
    ZTMX.deallocate();
    ZTM1MX.deallocate();
    WZoneTimeMinus1Temp.deallocate();
    WZoneTimeMinus2Temp.deallocate();
    WZoneTimeMinus3Temp.deallocate();
    WZoneTimeMinus1.deallocate();
    WZoneTimeMinus2.deallocate();
    WZoneTimeMinus3.deallocate();
    PreviousMeasuredHumRat1.deallocate();
    PreviousMeasuredHumRat2.deallocate();
    PreviousMeasuredHumRat3.deallocate();
    CO2ZoneTimeMinus1Temp.deallocate();
    CO2ZoneTimeMinus2Temp.deallocate();
    CO2ZoneTimeMinus3Temp.deallocate();
    CO2ZoneTimeMinus1.deallocate();
    CO2ZoneTimeMinus2.deallocate();
    CO2ZoneTimeMinus3.deallocate();
    AIRRAT.deallocate();
    ZoneAirHumRat.deallocate();
    NonAirSystemResponse.deallocate();
    SysDepZoneLoadsLagged.deallocate();
    AirflowNetwork::AirflowNetworkExchangeData.deallocate();
    Node.deallocate();
    TempTstatAir.deallocate();
    LoadCorrectionFactor.deallocate();
    MAT.deallocate();
    ZT.deallocate();
    PreviousMeasuredZT1.deallocate();
    PreviousMeasuredZT2.deallocate();
    PreviousMeasuredZT3.deallocate();
    MCPI.deallocate();
    MCPV.deallocate();
    MCPM.deallocate();
    MCPE.deallocate();
    MCPC.deallocate();
    MDotCPOA.deallocate();
    MDotOA.deallocate();
    MCPTI.deallocate();
    MCPTV.deallocate();
    MCPTM.deallocate();
    MCPTE.deallocate();
    MCPTC.deallocate();
    SurfaceWindow.deallocate();
    Surface.deallocate();
    HConvIn.deallocate();
    SNLoadHeatRate.deallocate();
    SNLoadCoolRate.deallocate();
    SNLoadHeatEnergy.deallocate();
    SNLoadCoolEnergy.deallocate();
    ZoneAirRelHum.deallocate();
    IsZoneDV.deallocate();
    IsZoneCV.deallocate();
    IsZoneUI.deallocate();
    ZoneDVMixedFlag.deallocate();
    ZnAirRpt.deallocate();
    ZoneEquipConfig.deallocate();
    ZoneIntGain.deallocate();
    ZoneEqSizing.deallocate();
    ZoneLatentGain.deallocate();
    SumLatentHTRadSys.deallocate();
    SumHmARaW.deallocate();
    SumConvHTRadSys.deallocate();
    SumConvPool.deallocate();
    SumHmARa.deallocate();
    MixingMassFlowXHumRat.deallocate();
    MixingMassFlowZone.deallocate();
    ZoneW1.deallocate();
    ZoneAirHumRatTemp.deallocate();
    SumLatentPool.deallocate();
    OAMFL.deallocate();
    VAMFL.deallocate();
    EAMFL.deallocate();
    EAMFLxHumRat.deallocate();
    CTMFL.deallocate();
    ZoneAirCO2.deallocate();
    ZoneAirCO2Temp.deallocate();
    ZoneAirDensityCO.deallocate();
    ZoneCO2Gain.deallocate();
    ZoneCO2GainExceptPeople.deallocate();
    ZoneGCGain.deallocate();
    MixingMassFlowCO2.deallocate();
    Schedule.deallocate();
}
