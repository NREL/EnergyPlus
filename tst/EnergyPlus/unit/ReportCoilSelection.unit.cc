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

// EnergyPlus::Standalone ERV Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

#include "Fixtures/EnergyPlusFixture.hh"
#include "Fixtures/SQLiteFixture.hh"

// EnergyPlus Headers
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataAirLoop.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataPrecisionGlobals.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/HVACFan.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ReportCoilSelection.hh>

using namespace EnergyPlus;

TEST_F(EnergyPlusFixture, ReportCoilSelection_ChWCoil)
{
    std::string coil1Name("Coil 1");             // user-defined name of the coil
    std::string coil1Type("Coil:Cooling:Water"); // idf input object class name of coil
    int chWInletNodeNum = 9;
    int chWOutletNodeNum = 15;

    state->dataPlnt->TotNumLoops = 1;
    state->dataPlnt->PlantLoop.allocate(1);
    state->dataPlnt->PlantLoop(1).Name = "Chilled Water Loop";
    state->dataPlnt->PlantLoop(1).FluidName = "Water";
    state->dataPlnt->PlantLoop(1).FluidIndex = 1;
    state->dataPlnt->PlantLoop(1).MaxMassFlowRate = 0.1;
    state->dataPlnt->PlantLoop(1).LoopSide.allocate(2);
    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch.allocate(1);
    state->dataPlnt->PlantLoop(1).LoopSide(1).TotalBranches = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch(1).Comp.allocate(1);
    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch(1).TotalComponents = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch.allocate(1);
    state->dataPlnt->PlantLoop(1).LoopSide(2).TotalBranches = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).Comp.allocate(1);
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).TotalComponents = 1;

    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumIn = 0;
    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumOut = 0;
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).Comp(1).NodeNumIn = chWInletNodeNum;
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).Comp(1).NodeNumOut = chWOutletNodeNum;

    Real64 airVdot(0.052);   // air flow rate in m3/s
    bool isAutoSized(false); // true if autosized
    state->dataRptCoilSelection->coilSelectionReportObj->setCoilAirFlow(*state, coil1Name, coil1Type, airVdot, isAutoSized);
    auto &c1(state->dataRptCoilSelection->coilSelectionReportObj->coilSelectionDataObjs[0]);
    EXPECT_EQ(coil1Name, c1->coilName_);
    EXPECT_EQ(coil1Type, c1->coilObjName);
    EXPECT_EQ(airVdot, c1->coilDesVolFlow);
    EXPECT_EQ(isAutoSized, c1->volFlowIsAutosized);

    int loopNum = 1;
    Real64 waterVdot = 0.05;
    // First with no plant sizing objects defined
    isAutoSized = false; // true if autosized
    state->dataRptCoilSelection->coilSelectionReportObj->setCoilWaterFlowNodeNums(
        *state, coil1Name, coil1Type, waterVdot, isAutoSized, chWInletNodeNum, chWOutletNodeNum, loopNum);
    EXPECT_EQ(-999, c1->pltSizNum);
    EXPECT_EQ(loopNum, c1->waterLoopNum);
    EXPECT_EQ(state->dataPlnt->PlantLoop(1).Name, c1->plantLoopName);
    EXPECT_EQ(-999, c1->rhoFluid);
    EXPECT_EQ(-999, c1->cpFluid);
    EXPECT_EQ(-999, c1->coilDesWaterMassFlow);
    EXPECT_EQ("No", c1->coilWaterFlowAutoMsg);

    // Exercise report writing with mostly defaults
    state->dataRptCoilSelection->coilSelectionReportObj->finishCoilSummaryReportTable(*state);

    // Use the other form for coil 2
    std::string coil2Name("Coil 2");             // user-defined name of the coil
    std::string coil2Type("Coil:Cooling:Water"); // idf input object class name of coil
    int pltSizNum = -999;
    state->dataRptCoilSelection->coilSelectionReportObj->setCoilWaterFlowPltSizNum(
        *state, coil2Name, coil2Type, waterVdot, isAutoSized, pltSizNum, loopNum);
    auto &c2(state->dataRptCoilSelection->coilSelectionReportObj->coilSelectionDataObjs[1]);
    EXPECT_EQ(-999, c2->pltSizNum);
    EXPECT_EQ(loopNum, c2->waterLoopNum);
    EXPECT_EQ(state->dataPlnt->PlantLoop(1).Name, c2->plantLoopName);
    EXPECT_EQ(-999, c2->rhoFluid);
    EXPECT_EQ(-999, c2->cpFluid);
    EXPECT_EQ(-999, c2->coilDesWaterMassFlow);
    EXPECT_EQ("No", c2->coilWaterFlowAutoMsg);

    // Now add a plant sizing object
    state->dataSize->NumPltSizInput = 1;
    state->dataSize->PlantSizData.allocate(1);
    state->dataSize->PlantSizData(1).PlantLoopName = "Chilled Water Loop";
    isAutoSized = true; // true if autosized
    state->dataRptCoilSelection->coilSelectionReportObj->setCoilWaterFlowNodeNums(
        *state, coil1Name, coil1Type, waterVdot, isAutoSized, chWInletNodeNum, chWOutletNodeNum, loopNum);
    auto &c1b(state->dataRptCoilSelection->coilSelectionReportObj->coilSelectionDataObjs[0]);
    EXPECT_EQ(1, c1b->pltSizNum);
    EXPECT_EQ(loopNum, c1b->waterLoopNum);
    EXPECT_EQ(state->dataPlnt->PlantLoop(1).Name, c1b->plantLoopName);
    EXPECT_NEAR(999.9, c1b->rhoFluid, 0.1);
    EXPECT_NEAR(4197.9, c1b->cpFluid, 0.1);
    Real64 expFlow = waterVdot * c1b->rhoFluid;
    EXPECT_NEAR(expFlow, c1b->coilDesWaterMassFlow, 0.01);
    EXPECT_EQ("Yes", c1b->coilWaterFlowAutoMsg);

    Real64 uA = 1000.00;
    Real64 sizingCap = 500.0;
    int curSysNum = 1;
    int curZoneEqNum = 0;
    isAutoSized = true; // true if autosized
    state->dataAirSystemsData->PrimaryAirSystems.allocate(1);
    state->dataAirLoop->AirToZoneNodeInfo.allocate(1);
    state->dataAirLoop->AirToZoneNodeInfo(1).NumZonesHeated = 2;
    state->dataAirLoop->AirToZoneNodeInfo(1).HeatCtrlZoneNums.allocate(state->dataAirLoop->AirToZoneNodeInfo(1).NumZonesHeated);
    state->dataAirLoop->AirToZoneNodeInfo(1).HeatCtrlZoneNums(1) = 2;
    state->dataAirLoop->AirToZoneNodeInfo(1).HeatCtrlZoneNums(2) = 3;
    state->dataGlobal->NumOfZones = 3;
    state->dataHeatBal->Zone.allocate(state->dataGlobal->NumOfZones);
    state->dataHeatBal->Zone(1).Name = "Zone 1";
    state->dataHeatBal->Zone(2).Name = "Zone 2";
    state->dataHeatBal->Zone(3).Name = "Zone 3";

    // This triggers doAirLoopSetUp
    state->dataRptCoilSelection->coilSelectionReportObj->setCoilUA(*state, coil2Name, coil2Type, uA, sizingCap, isAutoSized, curSysNum, curZoneEqNum);
    EXPECT_EQ(uA, c2->coilUA);
    EXPECT_EQ(sizingCap, c2->coilTotCapAtPeak);
    EXPECT_EQ(curSysNum, c2->airloopNum);
    EXPECT_EQ(curZoneEqNum, c2->zoneEqNum);

    // This coil serves zones 2 and 3 - rmLatentAtPeak is summed for all applicable zones
    Real64 zoneCoolingLatentLoad = 1000.0;
    int zoneNum = 1;
    state->dataRptCoilSelection->coilSelectionReportObj->setZoneLatentLoadCoolingIdealPeak(zoneNum, zoneCoolingLatentLoad);
    EXPECT_EQ(0.0, c2->rmLatentAtPeak);
    zoneNum = 2;
    state->dataRptCoilSelection->coilSelectionReportObj->setZoneLatentLoadCoolingIdealPeak(zoneNum, zoneCoolingLatentLoad);
    EXPECT_EQ(1000.0, c2->rmLatentAtPeak);
    zoneNum = 3;
    state->dataRptCoilSelection->coilSelectionReportObj->setZoneLatentLoadCoolingIdealPeak(zoneNum, zoneCoolingLatentLoad);
    EXPECT_EQ(2000.0, c2->rmLatentAtPeak);

    // Add a heating coil
    std::string coil3Name("Coil 3");                // user-defined name of the coil
    std::string coil3Type("Coil:Heating:Electric"); // idf input object class name of coil
    uA = -999.0;
    sizingCap = 500.0;
    curSysNum = 1;
    curZoneEqNum = 0;
    isAutoSized = false; // true if autosized
    // This triggers doAirLoopSetUp
    state->dataRptCoilSelection->coilSelectionReportObj->setCoilUA(*state, coil3Name, coil3Type, uA, sizingCap, isAutoSized, curSysNum, curZoneEqNum);
    auto &c3(state->dataRptCoilSelection->coilSelectionReportObj->coilSelectionDataObjs[2]);
    EXPECT_EQ(uA, c3->coilUA);
    EXPECT_EQ(sizingCap, c3->coilTotCapAtPeak);
    EXPECT_EQ(curSysNum, c3->airloopNum);
    EXPECT_EQ(curZoneEqNum, c3->zoneEqNum);

    // This coil serves zones 2 and 3 - rmLatentAtPeak is summed for all applicable zones
    Real64 zoneHeatingLatentLoad = 100.0;
    zoneNum = 1;
    state->dataRptCoilSelection->coilSelectionReportObj->setZoneLatentLoadHeatingIdealPeak(zoneNum, zoneHeatingLatentLoad);
    EXPECT_EQ(0.0, c3->rmLatentAtPeak);
    zoneNum = 2;
    state->dataRptCoilSelection->coilSelectionReportObj->setZoneLatentLoadHeatingIdealPeak(zoneNum, zoneHeatingLatentLoad);
    EXPECT_EQ(100.0, c3->rmLatentAtPeak);
    zoneNum = 3;
    state->dataRptCoilSelection->coilSelectionReportObj->setZoneLatentLoadHeatingIdealPeak(zoneNum, zoneHeatingLatentLoad);
    EXPECT_EQ(200.0, c3->rmLatentAtPeak);

    // Exercise report writing again
    state->dataRptCoilSelection->coilSelectionReportObj->finishCoilSummaryReportTable(*state);
}

TEST_F(EnergyPlusFixture, ReportCoilSelection_SteamCoil)
{
    std::string coil1Name("Coil 1");             // user-defined name of the coil
    std::string coil1Type("Coil:Heating:Steam"); // idf input object class name of coil
    int wInletNodeNum = 9;
    int wOutletNodeNum = 15;

    state->dataPlnt->TotNumLoops = 1;
    state->dataPlnt->PlantLoop.allocate(1);
    state->dataPlnt->PlantLoop(1).Name = "Steam Loop";
    state->dataPlnt->PlantLoop(1).FluidName = "Steam";
    state->dataPlnt->PlantLoop(1).FluidIndex = 1;
    state->dataPlnt->PlantLoop(1).MaxMassFlowRate = 0.1;
    state->dataPlnt->PlantLoop(1).LoopSide.allocate(2);
    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch.allocate(1);
    state->dataPlnt->PlantLoop(1).LoopSide(1).TotalBranches = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch(1).Comp.allocate(1);
    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch(1).TotalComponents = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch.allocate(1);
    state->dataPlnt->PlantLoop(1).LoopSide(2).TotalBranches = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).Comp.allocate(1);
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).TotalComponents = 1;

    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumIn = 0;
    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumOut = 0;
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).Comp(1).NodeNumIn = wInletNodeNum;
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).Comp(1).NodeNumOut = wOutletNodeNum;

    Real64 airVdot(0.052);   // air flow rate in m3/s
    bool isAutoSized(false); // true if autosized
    state->dataRptCoilSelection->coilSelectionReportObj->setCoilAirFlow(*state, coil1Name, coil1Type, airVdot, isAutoSized);
    auto &c1(state->dataRptCoilSelection->coilSelectionReportObj->coilSelectionDataObjs[0]);
    EXPECT_EQ(coil1Name, c1->coilName_);
    EXPECT_EQ(coil1Type, c1->coilObjName);
    EXPECT_EQ(airVdot, c1->coilDesVolFlow);
    EXPECT_EQ(isAutoSized, c1->volFlowIsAutosized);

    int loopNum = 1;
    Real64 waterVdot = 0.05;
    // First with no plant sizing objects defined
    isAutoSized = false; // true if autosized
    state->dataRptCoilSelection->coilSelectionReportObj->setCoilWaterFlowNodeNums(
        *state, coil1Name, coil1Type, waterVdot, isAutoSized, wInletNodeNum, wOutletNodeNum, loopNum);
    EXPECT_EQ(-999, c1->pltSizNum);
    EXPECT_EQ(loopNum, c1->waterLoopNum);
    EXPECT_EQ(state->dataPlnt->PlantLoop(1).Name, c1->plantLoopName);
    EXPECT_EQ(-999, c1->rhoFluid);
    EXPECT_EQ(-999, c1->cpFluid);
    EXPECT_EQ(-999, c1->coilDesWaterMassFlow);
    EXPECT_EQ("No", c1->coilWaterFlowAutoMsg);

    // Now add a plant sizing object
    state->dataSize->NumPltSizInput = 1;
    state->dataSize->PlantSizData.allocate(1);
    state->dataSize->PlantSizData(1).PlantLoopName = "Steam Loop";
    state->dataSize->PlantSizData(1).LoopType = DataSizing::SteamLoop;
    isAutoSized = true; // true if autosized
    state->dataRptCoilSelection->coilSelectionReportObj->setCoilWaterFlowNodeNums(
        *state, coil1Name, coil1Type, waterVdot, isAutoSized, wInletNodeNum, wOutletNodeNum, loopNum);
    auto &c1b(state->dataRptCoilSelection->coilSelectionReportObj->coilSelectionDataObjs[0]);
    EXPECT_EQ(1, c1b->pltSizNum);
    EXPECT_EQ(loopNum, c1b->waterLoopNum);
    EXPECT_EQ(state->dataPlnt->PlantLoop(1).Name, c1b->plantLoopName);
    EXPECT_NEAR(0.6, c1b->rhoFluid, 0.01);
    EXPECT_NEAR(4216.0, c1b->cpFluid, 0.1);
    Real64 expFlow = waterVdot * c1b->rhoFluid;
    EXPECT_NEAR(expFlow, c1b->coilDesWaterMassFlow, 0.01);
    EXPECT_EQ("Yes", c1b->coilWaterFlowAutoMsg);

    // Exercise report writing again
    state->dataRptCoilSelection->coilSelectionReportObj->finishCoilSummaryReportTable(*state);
}

TEST_F(EnergyPlusFixture, ReportCoilSelection_ZoneEqCoil)
{
    std::string coil1Name("Coil 1");            // user-defined name of the coil
    std::string coil1Type("Coil:Heating:Fuel"); // idf input object class name of coil

    state->dataGlobal->NumOfZones = 3;
    state->dataHeatBal->Zone.allocate(state->dataGlobal->NumOfZones);
    state->dataHeatBal->Zone(1).Name = "Zone 1";
    state->dataHeatBal->Zone(2).Name = "Zone 2";
    state->dataHeatBal->Zone(3).Name = "Zone 3";

    int curSysNum = 0;
    int curZoneEqNum = 2;
    int curOASysNum = 0;
    state->dataZoneEquip->ZoneEquipList.allocate(3);
    state->dataZoneEquip->ZoneEquipList(curZoneEqNum).NumOfEquipTypes = 2;
    state->dataZoneEquip->ZoneEquipList(curZoneEqNum).EquipName.allocate(2);
    state->dataZoneEquip->ZoneEquipList(curZoneEqNum).EquipType.allocate(2);
    state->dataZoneEquip->ZoneEquipList(curZoneEqNum).EquipType_Num.allocate(2);
    state->dataZoneEquip->ZoneEquipList(curZoneEqNum).EquipName(1) = "Zone 2 Fan Coil";
    state->dataZoneEquip->ZoneEquipList(curZoneEqNum).EquipType(1) = "ZoneHVAC:FourPipeFanCoil";
    state->dataZoneEquip->ZoneEquipList(curZoneEqNum).EquipType_Num(1) = DataHVACGlobals::ZoneEquipTypeOf_FourPipeFanCoil;
    state->dataZoneEquip->ZoneEquipList(curZoneEqNum).EquipName(2) = "Zone 2 Unit Heater";
    state->dataZoneEquip->ZoneEquipList(curZoneEqNum).EquipType(2) = "ZoneHVAC:UnitHeater";
    state->dataZoneEquip->ZoneEquipList(curZoneEqNum).EquipType_Num(2) = DataHVACGlobals::ZoneEquipTypeOf_UnitVentilator;

    Real64 totGrossCap = 500.0;
    Real64 sensGrossCap = 500.0;
    Real64 airFlowRate = 0.11;
    Real64 waterFlowRate = 0.0;

    state->dataRptCoilSelection->coilSelectionReportObj->setCoilFinalSizes(
        *state, coil1Name, coil1Type, totGrossCap, sensGrossCap, airFlowRate, waterFlowRate);
    auto &c1(state->dataRptCoilSelection->coilSelectionReportObj->coilSelectionDataObjs[0]);
    EXPECT_EQ(totGrossCap, c1->coilTotCapFinal);
    EXPECT_EQ(sensGrossCap, c1->coilSensCapFinal);
    EXPECT_EQ(airFlowRate, c1->coilRefAirVolFlowFinal);
    EXPECT_EQ(waterFlowRate, c1->coilRefWaterVolFlowFinal);

    Real64 RatedCoilTotCap = 400.0;
    Real64 RatedCoilSensCap = 399.0;
    Real64 RatedAirMassFlow = 0.001;
    Real64 RatedCoilInDb = -999.0;
    Real64 RatedCoilInHumRat = -999.0;
    Real64 RatedCoilInWb = 20.0;
    Real64 RatedCoilOutDb = -999.0;
    Real64 RatedCoilOutHumRat = -999.0;
    Real64 RatedCoilOutWb = 30.0;
    Real64 RatedCoilOadbRef = 24.0;
    Real64 RatedCoilOawbRef = 16.0;
    Real64 RatedCoilBpFactor = 0.2;
    Real64 RatedCoilEff = 0.8;

    // First without setting coil inlet/outlet conditions
    state->dataRptCoilSelection->coilSelectionReportObj->setRatedCoilConditions(*state,
                                                                                coil1Name,
                                                                                coil1Type,
                                                                                RatedCoilTotCap,
                                                                                RatedCoilSensCap,
                                                                                RatedAirMassFlow,
                                                                                RatedCoilInDb,
                                                                                RatedCoilInHumRat,
                                                                                RatedCoilInWb,
                                                                                RatedCoilOutDb,
                                                                                RatedCoilOutHumRat,
                                                                                RatedCoilOutWb,
                                                                                RatedCoilOadbRef,
                                                                                RatedCoilOawbRef,
                                                                                RatedCoilBpFactor,
                                                                                RatedCoilEff);

    EXPECT_EQ(RatedCoilTotCap, c1->coilRatedTotCap);
    EXPECT_NEAR(c1->coilCapFTIdealPeak, 1.0, 0.000001);
    EXPECT_EQ(RatedCoilSensCap, c1->coilRatedSensCap);
    EXPECT_EQ(RatedAirMassFlow, c1->ratedAirMassFlow);
    EXPECT_EQ(RatedCoilInDb, c1->ratedCoilInDb);
    EXPECT_EQ(RatedCoilInWb, c1->ratedCoilInWb);
    EXPECT_EQ(RatedCoilInHumRat, c1->ratedCoilInHumRat);
    EXPECT_EQ(-999.0, c1->ratedCoilInEnth);
    EXPECT_EQ(RatedCoilOutDb, c1->ratedCoilOutDb);
    EXPECT_EQ(RatedCoilOutWb, c1->ratedCoilOutWb);
    EXPECT_EQ(RatedCoilOutHumRat, c1->ratedCoilOutHumRat);
    EXPECT_EQ(-999.0, c1->ratedCoilOutEnth);
    EXPECT_EQ(RatedCoilEff, c1->ratedCoilEff);
    EXPECT_EQ(RatedCoilBpFactor, c1->ratedCoilBpFactor);
    EXPECT_EQ(RatedCoilOadbRef, c1->ratedCoilOadbRef);
    EXPECT_EQ(RatedCoilOawbRef, c1->ratedCoilOawbRef);

    // again with setting coil inlet/outlet conditions
    RatedCoilInDb = 23.0;
    RatedCoilInHumRat = 0.008;
    RatedCoilOutDb = 40.0;
    RatedCoilOutHumRat = 0.009;
    state->dataRptCoilSelection->coilSelectionReportObj->setRatedCoilConditions(*state,
                                                                                coil1Name,
                                                                                coil1Type,
                                                                                RatedCoilTotCap,
                                                                                RatedCoilSensCap,
                                                                                RatedAirMassFlow,
                                                                                RatedCoilInDb,
                                                                                RatedCoilInHumRat,
                                                                                RatedCoilInWb,
                                                                                RatedCoilOutDb,
                                                                                RatedCoilOutHumRat,
                                                                                RatedCoilOutWb,
                                                                                RatedCoilOadbRef,
                                                                                RatedCoilOawbRef,
                                                                                RatedCoilBpFactor,
                                                                                RatedCoilEff);
    EXPECT_EQ(RatedCoilInDb, c1->ratedCoilInDb);
    EXPECT_EQ(RatedCoilInHumRat, c1->ratedCoilInHumRat);
    EXPECT_NEAR(43460.9, c1->ratedCoilInEnth, 0.1);
    EXPECT_EQ(RatedCoilOutDb, c1->ratedCoilOutDb);
    EXPECT_EQ(RatedCoilOutHumRat, c1->ratedCoilOutHumRat);
    EXPECT_NEAR(63371.3, c1->ratedCoilOutEnth, 0.1);

    Real64 entAirDryBulbTemp = 24.0;
    state->dataRptCoilSelection->coilSelectionReportObj->setCoilEntAirTemp(*state, coil1Name, coil1Type, entAirDryBulbTemp, curSysNum, curZoneEqNum);
    EXPECT_EQ(entAirDryBulbTemp, c1->coilDesEntTemp);
    EXPECT_EQ(curSysNum, c1->airloopNum);
    EXPECT_EQ(curZoneEqNum, c1->zoneEqNum);

    Real64 entAirHumRat = 0.004;
    state->dataRptCoilSelection->coilSelectionReportObj->setCoilEntAirHumRat(*state, coil1Name, coil1Type, entAirHumRat);
    EXPECT_EQ(entAirHumRat, c1->coilDesEntHumRat);

    Real64 entWaterTemp = 60.0;
    state->dataRptCoilSelection->coilSelectionReportObj->setCoilEntWaterTemp(*state, coil1Name, coil1Type, entWaterTemp);
    EXPECT_EQ(entWaterTemp, c1->coilDesWaterEntTemp);

    Real64 lvgWaterTemp = 50.0;
    state->dataRptCoilSelection->coilSelectionReportObj->setCoilLvgWaterTemp(*state, coil1Name, coil1Type, lvgWaterTemp);
    EXPECT_EQ(lvgWaterTemp, c1->coilDesWaterLvgTemp);

    Real64 CoilWaterDeltaT = 50.0;
    state->dataRptCoilSelection->coilSelectionReportObj->setCoilWaterDeltaT(*state, coil1Name, coil1Type, CoilWaterDeltaT);
    EXPECT_EQ(CoilWaterDeltaT, c1->coilDesWaterTempDiff);

    Real64 lvgAirDryBulbTemp = 12.0;
    state->dataRptCoilSelection->coilSelectionReportObj->setCoilLvgAirTemp(*state, coil1Name, coil1Type, lvgAirDryBulbTemp);
    EXPECT_EQ(lvgAirDryBulbTemp, c1->coilDesLvgTemp);

    Real64 lvgAirHumRat = 0.006;
    state->dataRptCoilSelection->coilSelectionReportObj->setCoilLvgAirHumRat(*state, coil1Name, coil1Type, lvgAirHumRat);
    EXPECT_EQ(lvgAirHumRat, c1->coilDesLvgHumRat);

    int zoneNum = 1;
    Real64 zoneCoolingLatentLoad = 1234.0;
    state->dataRptCoilSelection->coilSelectionReportObj->setZoneLatentLoadCoolingIdealPeak(zoneNum, zoneCoolingLatentLoad);
    // Expect zero because it's a heating coil
    EXPECT_EQ(0.0, c1->rmLatentAtPeak);

    Real64 zoneHeatingLatentLoad = 4321.0;
    state->dataRptCoilSelection->coilSelectionReportObj->setZoneLatentLoadHeatingIdealPeak(zoneNum, zoneHeatingLatentLoad);
    // Expect zero because doZoneEqSetup isn't currently executed
    EXPECT_EQ(0.0, c1->rmLatentAtPeak);

    // Exercise report writing again
    state->dataRptCoilSelection->coilSelectionReportObj->finishCoilSummaryReportTable(*state);

    // Test coil reporting
    curZoneEqNum = 1;
    state->dataSize->ZoneEqSizing.allocate(1);
    state->dataSize->TermUnitFinalZoneSizing.allocate(1);
    state->dataSize->CurTermUnitSizingNum = curZoneEqNum;
    state->dataSize->TermUnitFinalZoneSizing(state->dataSize->CurTermUnitSizingNum).DesHeatCoilInTempTU = RatedCoilInDb;
    state->dataSize->TermUnitFinalZoneSizing(state->dataSize->CurTermUnitSizingNum).DesHeatCoilInHumRatTU = RatedCoilInHumRat;
    state->dataZoneEquip->ZoneEquipConfig.allocate(1);
    state->dataZoneEquip->ZoneEquipConfig(curZoneEqNum).ActualZoneNum = 1;
    state->dataZoneEquip->ZoneEquipConfig(curZoneEqNum).ZoneName = state->dataHeatBal->Zone(1).Name;
    state->dataSize->FinalZoneSizing.allocate(1);
    state->dataSize->FinalZoneSizing(curZoneEqNum).HeatDesDay = "Heat Design Day";
    state->dataSize->FinalZoneSizing(curZoneEqNum).DesHeatLoad = RatedCoilSensCap;
    state->dataSize->FinalZoneSizing(curZoneEqNum).OutTempAtHeatPeak = RatedCoilOutDb;
    state->dataSize->FinalZoneSizing(curZoneEqNum).OutHumRatAtHeatPeak = RatedCoilOutHumRat;
    state->dataSize->FinalZoneSizing(curZoneEqNum).ZoneRetTempAtHeatPeak = 21.6;
    state->dataSize->FinalZoneSizing(curZoneEqNum).ZoneHumRatAtHeatPeak = 0.007;
    state->dataSize->FinalZoneSizing(curZoneEqNum).ZoneTempAtHeatPeak = 21.0;
    state->dataSize->FinalZoneSizing(curZoneEqNum).HeatDesTemp = 30.0;
    state->dataSize->FinalZoneSizing(curZoneEqNum).HeatDesHumRat = 0.007;

    Real64 fanHeatGain = 1.3;
    Real64 coilCapFunTempFac = 1.0;
    Real64 DXFlowPerCapMinRatio = 0.00004;
    Real64 DXFlowPerCapMaxRatio = 0.00006;
    state->dataEnvrn->StdRhoAir = 1.2;
    state->dataSize->DataFlowUsedForSizing = airFlowRate / state->dataEnvrn->StdRhoAir;

    // setCoilHeatingCapacity will not overwrite previously set temperature data
    state->dataRptCoilSelection->coilSelectionReportObj->setCoilHeatingCapacity(*state,
                                                                                coil1Name,
                                                                                coil1Type,
                                                                                RatedCoilTotCap,
                                                                                false,
                                                                                curSysNum,
                                                                                curZoneEqNum,
                                                                                curOASysNum,
                                                                                fanHeatGain,
                                                                                coilCapFunTempFac,
                                                                                DXFlowPerCapMinRatio,
                                                                                DXFlowPerCapMaxRatio);
    EXPECT_EQ(entAirDryBulbTemp, c1->coilDesEntTemp);

    state->dataRptCoilSelection->coilSelectionReportObj->setZoneLatentLoadHeatingIdealPeak(zoneNum, zoneHeatingLatentLoad);
    // Expect non-zero because this is a heating coil
    EXPECT_LT(0.0, zoneHeatingLatentLoad);
    EXPECT_NEAR(zoneHeatingLatentLoad, c1->rmLatentAtPeak, 0.000001);

    entAirDryBulbTemp = 21.0; // change coil entering air temp
    state->dataRptCoilSelection->coilSelectionReportObj->setCoilEntAirTemp(*state, coil1Name, coil1Type, entAirDryBulbTemp, curSysNum, curZoneEqNum);
    lvgAirDryBulbTemp = 30.0;
    state->dataRptCoilSelection->coilSelectionReportObj->setCoilLvgAirTemp(*state, coil1Name, coil1Type, lvgAirDryBulbTemp);
    EXPECT_EQ(entAirDryBulbTemp, c1->coilDesEntTemp);
    EXPECT_EQ(lvgAirDryBulbTemp, c1->coilDesLvgTemp);

    state->dataSize->TermUnitSingDuct = true;
    // now reset entering/leaving air temps so that setCoilHeatingCapacity will initialize any uninitialized temperature data
    // for example if a non-water coil is used in a terminal unit and setCoilEntAirTemp is not called
    c1->coilDesEntTemp = -999.0;
    c1->coilDesEntHumRat = -999.0;
    c1->coilDesLvgTemp = -999.0;
    c1->coilDesLvgHumRat = -999.0;
    state->dataRptCoilSelection->coilSelectionReportObj->setCoilHeatingCapacity(*state,
                                                                                coil1Name,
                                                                                coil1Type,
                                                                                RatedCoilTotCap,
                                                                                false,
                                                                                curSysNum,
                                                                                curZoneEqNum,
                                                                                curOASysNum,
                                                                                fanHeatGain,
                                                                                coilCapFunTempFac,
                                                                                DXFlowPerCapMinRatio,
                                                                                DXFlowPerCapMaxRatio);
    EXPECT_EQ(RatedCoilInDb, c1->coilDesEntTemp);
    EXPECT_EQ(RatedCoilInHumRat, c1->coilDesEntHumRat);
    EXPECT_EQ(state->dataSize->FinalZoneSizing(curZoneEqNum).HeatDesTemp, c1->coilDesLvgTemp);
    EXPECT_EQ(state->dataSize->FinalZoneSizing(curZoneEqNum).HeatDesHumRat, c1->coilDesLvgHumRat);

    // test heating capacity with default CapFT
    EXPECT_NEAR(RatedCoilTotCap * coilCapFunTempFac, c1->coilTotCapAtPeak, 0.000001);
    EXPECT_NEAR(coilCapFunTempFac, 1.0, 0.000001);
    EXPECT_NEAR(RatedCoilTotCap, c1->coilTotCapAtPeak, 0.000001);
    // test heating capacity adjustment with a CapFT curve
    coilCapFunTempFac = 1.15;
    state->dataRptCoilSelection->coilSelectionReportObj->setCoilHeatingCapacity(*state,
                                                                                coil1Name,
                                                                                coil1Type,
                                                                                RatedCoilTotCap,
                                                                                false,
                                                                                curSysNum,
                                                                                curZoneEqNum,
                                                                                curOASysNum,
                                                                                fanHeatGain,
                                                                                coilCapFunTempFac,
                                                                                DXFlowPerCapMinRatio,
                                                                                DXFlowPerCapMaxRatio);
    EXPECT_NEAR(RatedCoilTotCap * coilCapFunTempFac, c1->coilTotCapAtPeak, 0.000001);
    EXPECT_LT(RatedCoilTotCap, c1->coilTotCapAtPeak);
}

TEST_F(EnergyPlusFixture, ReportCoilSelection_ZoneEqCoolingCoil)
{
    std::string coil1Name("Coil 1");          // user-defined name of the coil
    std::string coil1Type("Coil:Cooling:DX"); // idf input object class name of coil

    state->dataGlobal->NumOfZones = 3;
    state->dataHeatBal->Zone.allocate(state->dataGlobal->NumOfZones);
    state->dataHeatBal->Zone(1).Name = "Zone 1";
    state->dataHeatBal->Zone(2).Name = "Zone 2";
    state->dataHeatBal->Zone(3).Name = "Zone 3";

    int curSysNum = 0;
    int curZoneEqNum = 2;
    int curOASysNum = 0;
    state->dataZoneEquip->ZoneEquipList.allocate(3);
    state->dataZoneEquip->ZoneEquipList(curZoneEqNum).NumOfEquipTypes = 2;
    state->dataZoneEquip->ZoneEquipList(curZoneEqNum).EquipName.allocate(2);
    state->dataZoneEquip->ZoneEquipList(curZoneEqNum).EquipType.allocate(2);
    state->dataZoneEquip->ZoneEquipList(curZoneEqNum).EquipType_Num.allocate(2);
    state->dataZoneEquip->ZoneEquipList(curZoneEqNum).EquipName(1) = "Zone 2 DX Eq";
    state->dataZoneEquip->ZoneEquipList(curZoneEqNum).EquipType(1) = "ZoneHVAC:WindowAirConditioner";
    state->dataZoneEquip->ZoneEquipList(curZoneEqNum).EquipType_Num(1) = DataHVACGlobals::ZoneEquipTypeOf_WindowAirConditioner;
    state->dataZoneEquip->ZoneEquipList(curZoneEqNum).EquipName(2) = "Zone 2 Unit Heater";
    state->dataZoneEquip->ZoneEquipList(curZoneEqNum).EquipType(2) = "ZoneHVAC:UnitHeater";
    state->dataZoneEquip->ZoneEquipList(curZoneEqNum).EquipType_Num(2) = DataHVACGlobals::ZoneEquipTypeOf_UnitVentilator;

    Real64 totGrossCap = 500.0;
    Real64 sensGrossCap = 400.0;
    Real64 airFlowRate = 0.11;
    Real64 waterFlowRate = 0.0;

    state->dataRptCoilSelection->coilSelectionReportObj->setCoilFinalSizes(
        *state, coil1Name, coil1Type, totGrossCap, sensGrossCap, airFlowRate, waterFlowRate);
    auto &c1(state->dataRptCoilSelection->coilSelectionReportObj->coilSelectionDataObjs[0]);
    EXPECT_EQ(totGrossCap, c1->coilTotCapFinal);
    EXPECT_EQ(sensGrossCap, c1->coilSensCapFinal);
    EXPECT_EQ(airFlowRate, c1->coilRefAirVolFlowFinal);
    EXPECT_EQ(waterFlowRate, c1->coilRefWaterVolFlowFinal);

    Real64 RatedCoilTotCap = 400.0;
    Real64 RatedCoilSensCap = 300.0;
    Real64 RatedAirMassFlow = 0.001;
    Real64 RatedCoilInDb = -999.0;
    Real64 RatedCoilInHumRat = -999.0;
    Real64 RatedCoilInWb = 20.0;
    Real64 RatedCoilOutDb = -999.0;
    Real64 RatedCoilOutHumRat = -999.0;
    Real64 RatedCoilOutWb = 30.0;
    Real64 RatedCoilOadbRef = 24.0;
    Real64 RatedCoilOawbRef = 16.0;
    Real64 RatedCoilBpFactor = 0.2;
    Real64 RatedCoilEff = 0.8;

    // First without setting coil inlet/outlet conditions
    state->dataRptCoilSelection->coilSelectionReportObj->setRatedCoilConditions(*state,
                                                                                coil1Name,
                                                                                coil1Type,
                                                                                RatedCoilTotCap,
                                                                                RatedCoilSensCap,
                                                                                RatedAirMassFlow,
                                                                                RatedCoilInDb,
                                                                                RatedCoilInHumRat,
                                                                                RatedCoilInWb,
                                                                                RatedCoilOutDb,
                                                                                RatedCoilOutHumRat,
                                                                                RatedCoilOutWb,
                                                                                RatedCoilOadbRef,
                                                                                RatedCoilOawbRef,
                                                                                RatedCoilBpFactor,
                                                                                RatedCoilEff);

    EXPECT_EQ(RatedCoilTotCap, c1->coilRatedTotCap);
    EXPECT_NEAR(c1->coilCapFTIdealPeak, 1.0, 0.000001);
    EXPECT_EQ(RatedCoilSensCap, c1->coilRatedSensCap);
    EXPECT_EQ(RatedAirMassFlow, c1->ratedAirMassFlow);
    EXPECT_EQ(RatedCoilInDb, c1->ratedCoilInDb);
    EXPECT_EQ(RatedCoilInWb, c1->ratedCoilInWb);
    EXPECT_EQ(RatedCoilInHumRat, c1->ratedCoilInHumRat);
    EXPECT_EQ(-999.0, c1->ratedCoilInEnth);
    EXPECT_EQ(RatedCoilOutDb, c1->ratedCoilOutDb);
    EXPECT_EQ(RatedCoilOutWb, c1->ratedCoilOutWb);
    EXPECT_EQ(RatedCoilOutHumRat, c1->ratedCoilOutHumRat);
    EXPECT_EQ(-999.0, c1->ratedCoilOutEnth);
    EXPECT_EQ(RatedCoilEff, c1->ratedCoilEff);
    EXPECT_EQ(RatedCoilBpFactor, c1->ratedCoilBpFactor);
    EXPECT_EQ(RatedCoilOadbRef, c1->ratedCoilOadbRef);
    EXPECT_EQ(RatedCoilOawbRef, c1->ratedCoilOawbRef);

    // again with setting coil inlet/outlet conditions
    RatedCoilInDb = 23.0;
    RatedCoilInHumRat = 0.008;
    RatedCoilOutDb = 12.0;
    RatedCoilOutHumRat = 0.006;
    state->dataRptCoilSelection->coilSelectionReportObj->setRatedCoilConditions(*state,
                                                                                coil1Name,
                                                                                coil1Type,
                                                                                RatedCoilTotCap,
                                                                                RatedCoilSensCap,
                                                                                RatedAirMassFlow,
                                                                                RatedCoilInDb,
                                                                                RatedCoilInHumRat,
                                                                                RatedCoilInWb,
                                                                                RatedCoilOutDb,
                                                                                RatedCoilOutHumRat,
                                                                                RatedCoilOutWb,
                                                                                RatedCoilOadbRef,
                                                                                RatedCoilOawbRef,
                                                                                RatedCoilBpFactor,
                                                                                RatedCoilEff);
    EXPECT_EQ(RatedCoilInDb, c1->ratedCoilInDb);
    EXPECT_EQ(RatedCoilInHumRat, c1->ratedCoilInHumRat);
    EXPECT_NEAR(43460.9, c1->ratedCoilInEnth, 0.1);
    EXPECT_EQ(RatedCoilOutDb, c1->ratedCoilOutDb);
    EXPECT_EQ(RatedCoilOutHumRat, c1->ratedCoilOutHumRat);
    EXPECT_NEAR(27197.5, c1->ratedCoilOutEnth, 0.1);

    Real64 entAirDryBulbTemp = 24.0;
    state->dataRptCoilSelection->coilSelectionReportObj->setCoilEntAirTemp(*state, coil1Name, coil1Type, entAirDryBulbTemp, curSysNum, curZoneEqNum);
    EXPECT_EQ(entAirDryBulbTemp, c1->coilDesEntTemp);
    EXPECT_EQ(curSysNum, c1->airloopNum);
    EXPECT_EQ(curZoneEqNum, c1->zoneEqNum);

    Real64 entAirHumRat = 0.004;
    state->dataRptCoilSelection->coilSelectionReportObj->setCoilEntAirHumRat(*state, coil1Name, coil1Type, entAirHumRat);
    EXPECT_EQ(entAirHumRat, c1->coilDesEntHumRat);

    Real64 lvgAirDryBulbTemp = 14.0;
    state->dataRptCoilSelection->coilSelectionReportObj->setCoilLvgAirTemp(*state, coil1Name, coil1Type, lvgAirDryBulbTemp);
    EXPECT_EQ(lvgAirDryBulbTemp, c1->coilDesLvgTemp);

    Real64 lvgAirHumRat = 0.005;
    state->dataRptCoilSelection->coilSelectionReportObj->setCoilLvgAirHumRat(*state, coil1Name, coil1Type, lvgAirHumRat);
    EXPECT_EQ(lvgAirHumRat, c1->coilDesLvgHumRat);

    int zoneNum = 1;
    Real64 zoneCoolingLatentLoad = 1234.0;
    state->dataRptCoilSelection->coilSelectionReportObj->setZoneLatentLoadCoolingIdealPeak(zoneNum, zoneCoolingLatentLoad);
    // Expect zero because doZoneEqSetup isn't currently executed
    EXPECT_EQ(0.0, c1->rmLatentAtPeak);

    Real64 zoneHeatingLatentLoad = 4321.0;
    state->dataRptCoilSelection->coilSelectionReportObj->setZoneLatentLoadHeatingIdealPeak(zoneNum, zoneHeatingLatentLoad);
    // Expect zero because doZoneEqSetup isn't currently executed
    EXPECT_EQ(0.0, c1->rmLatentAtPeak);

    // Exercise report writing again
    state->dataRptCoilSelection->coilSelectionReportObj->finishCoilSummaryReportTable(*state);

    // Test coil reporting
    curZoneEqNum = 1;
    state->dataSize->ZoneEqSizing.allocate(1);
    state->dataZoneEquip->ZoneEquipConfig.allocate(1);
    state->dataZoneEquip->ZoneEquipConfig(curZoneEqNum).ActualZoneNum = 1;
    state->dataZoneEquip->ZoneEquipConfig(curZoneEqNum).ZoneName = state->dataHeatBal->Zone(1).Name;
    state->dataSize->FinalZoneSizing.allocate(1);
    state->dataSize->FinalZoneSizing(curZoneEqNum).CoolDesDay = "Cool Design Day";
    state->dataSize->FinalZoneSizing(curZoneEqNum).DesCoolLoad = RatedCoilSensCap;
    state->dataSize->FinalZoneSizing(curZoneEqNum).OutTempAtCoolPeak = RatedCoilOutDb;
    state->dataSize->FinalZoneSizing(curZoneEqNum).OutHumRatAtCoolPeak = RatedCoilOutHumRat;
    state->dataSize->FinalZoneSizing(curZoneEqNum).DesCoolCoilInTemp = 25.0;
    state->dataSize->FinalZoneSizing(curZoneEqNum).DesCoolCoilInHumRat = 0.007;
    state->dataSize->FinalZoneSizing(curZoneEqNum).CoolDesTemp = 12.0;
    state->dataSize->FinalZoneSizing(curZoneEqNum).CoolDesHumRat = 0.007;

    Real64 fanHeatGain = 1.3;
    Real64 coilCapFunTempFac = 1.0;
    Real64 DXFlowPerCapMinRatio = 0.00004;
    Real64 DXFlowPerCapMaxRatio = 0.00006;
    state->dataEnvrn->StdRhoAir = 1.2;
    state->dataSize->DataFlowUsedForSizing = airFlowRate / state->dataEnvrn->StdRhoAir;

    // setCoilCoolingCapacity will not overwrite previously set temperature data
    state->dataRptCoilSelection->coilSelectionReportObj->setCoilCoolingCapacity(*state,
                                                                                coil1Name,
                                                                                coil1Type,
                                                                                RatedCoilTotCap,
                                                                                false,
                                                                                curSysNum,
                                                                                curZoneEqNum,
                                                                                curOASysNum,
                                                                                fanHeatGain,
                                                                                coilCapFunTempFac,
                                                                                DXFlowPerCapMinRatio,
                                                                                DXFlowPerCapMaxRatio);
    EXPECT_EQ(entAirDryBulbTemp, c1->coilDesEntTemp);
    EXPECT_EQ(entAirHumRat, c1->coilDesEntHumRat);
    EXPECT_EQ(lvgAirDryBulbTemp, c1->coilDesLvgTemp);
    EXPECT_EQ(lvgAirHumRat, c1->coilDesLvgHumRat);
    Real64 CpMoistAir = Psychrometrics::PsyCpAirFnW(c1->coilDesEntHumRat);
    EXPECT_EQ(CpMoistAir, c1->cpMoistAir);
    EXPECT_NEAR(c1->fanHeatGainIdealPeak, fanHeatGain, 0.000001);

    state->dataRptCoilSelection->coilSelectionReportObj->setZoneLatentLoadCoolingIdealPeak(zoneNum, zoneCoolingLatentLoad);
    // Expect non-zero zone cooling latent load
    EXPECT_EQ(zoneCoolingLatentLoad, c1->rmLatentAtPeak);

    entAirDryBulbTemp = 21.0; // change coil entering air temp
    state->dataRptCoilSelection->coilSelectionReportObj->setCoilEntAirTemp(*state, coil1Name, coil1Type, entAirDryBulbTemp, curSysNum, curZoneEqNum);
    lvgAirDryBulbTemp = 12.0;
    state->dataRptCoilSelection->coilSelectionReportObj->setCoilLvgAirTemp(*state, coil1Name, coil1Type, lvgAirDryBulbTemp);
    EXPECT_EQ(entAirDryBulbTemp, c1->coilDesEntTemp);
    EXPECT_EQ(lvgAirDryBulbTemp, c1->coilDesLvgTemp);

    // now reset entering/leaving air temps so that setCoilCoolingCapacity will initialize any uninitialized temperature data
    // for example if setCoilEntAirTemp is not called
    c1->coilDesEntTemp = -999.0;
    c1->coilDesEntHumRat = -999.0;
    c1->coilDesLvgTemp = -999.0;
    c1->coilDesLvgHumRat = -999.0;
    state->dataRptCoilSelection->coilSelectionReportObj->setCoilCoolingCapacity(*state,
                                                                                coil1Name,
                                                                                coil1Type,
                                                                                RatedCoilTotCap,
                                                                                false,
                                                                                curSysNum,
                                                                                curZoneEqNum,
                                                                                curOASysNum,
                                                                                fanHeatGain,
                                                                                coilCapFunTempFac,
                                                                                DXFlowPerCapMinRatio,
                                                                                DXFlowPerCapMaxRatio);
    EXPECT_EQ(state->dataSize->FinalZoneSizing(curZoneEqNum).DesCoolCoilInTemp, c1->coilDesEntTemp);
    EXPECT_EQ(state->dataSize->FinalZoneSizing(curZoneEqNum).DesCoolCoilInHumRat, c1->coilDesEntHumRat);
    EXPECT_EQ(state->dataSize->FinalZoneSizing(curZoneEqNum).CoolDesTemp, c1->coilDesLvgTemp);
    EXPECT_EQ(state->dataSize->FinalZoneSizing(curZoneEqNum).CoolDesHumRat, c1->coilDesLvgHumRat);

    // test cooling capacity with default CapFT = 1
    EXPECT_NEAR(RatedCoilTotCap * coilCapFunTempFac, c1->coilTotCapAtPeak, 0.000001);
    EXPECT_NEAR(coilCapFunTempFac, 1.0, 0.000001);
    EXPECT_NEAR(RatedCoilTotCap, c1->coilTotCapAtPeak, 0.000001);

    // test cooling capacity adjustment with a CapFT curve != 1
    coilCapFunTempFac = 1.15;
    state->dataRptCoilSelection->coilSelectionReportObj->setCoilCoolingCapacity(*state,
                                                                                coil1Name,
                                                                                coil1Type,
                                                                                RatedCoilTotCap,
                                                                                false,
                                                                                curSysNum,
                                                                                curZoneEqNum,
                                                                                curOASysNum,
                                                                                fanHeatGain,
                                                                                coilCapFunTempFac,
                                                                                DXFlowPerCapMinRatio,
                                                                                DXFlowPerCapMaxRatio);
    EXPECT_NEAR(RatedCoilTotCap * coilCapFunTempFac, c1->coilTotCapAtPeak, 0.000001);
    EXPECT_LT(RatedCoilTotCap, c1->coilTotCapAtPeak);
}
