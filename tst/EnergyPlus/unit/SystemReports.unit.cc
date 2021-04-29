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

// EnergyPlus::SystemReports Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataAirSystems.hh>
#include <EnergyPlus/DataGlobalConstants.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/FanCoilUnits.hh>
#include <EnergyPlus/HVACStandAloneERV.hh>
#include <EnergyPlus/HVACVariableRefrigerantFlow.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/HybridUnitaryAirConditioners.hh>
#include <EnergyPlus/IOFiles.hh>
#include <EnergyPlus/OutAirNodeManager.hh>
#include <EnergyPlus/OutdoorAirUnit.hh>
#include <EnergyPlus/PackagedTerminalHeatPump.hh>
#include <EnergyPlus/PurchasedAirManager.hh>
#include <EnergyPlus/SystemReports.hh>
#include <EnergyPlus/UnitVentilator.hh>
#include <EnergyPlus/UnitarySystem.hh>
#include <EnergyPlus/WindowAC.hh>
#include <EnergyPlus/ZoneTempPredictorCorrector.hh>

using namespace EnergyPlus::SystemReports;
using namespace EnergyPlus::DataGlobalConstants;
using namespace EnergyPlus::DataAirSystems;
using namespace EnergyPlus::DataGlobalConstants;

namespace EnergyPlus {

TEST_F(EnergyPlusFixture, SeparateGasOutputVariables)
{
    state->dataHVACGlobal->NumPrimaryAirSys = 1;
    state->dataAirSystemsData->PrimaryAirSystems.allocate(1);
    state->dataLoopNodes->Node.allocate(2);

    bool CompLoadFlag(false);
    int AirLoopNum(1);
    std::string CompType1;
    std::string CompType2;
    Real64 CompLoad(150.0);
    Real64 CompEnergyUse(100.0);

    state->dataAirSystemsData->PrimaryAirSystems(1).NumBranches = 1;
    state->dataAirSystemsData->PrimaryAirSystems(1).Branch.allocate(1);
    state->dataAirSystemsData->PrimaryAirSystems(1).Branch(1).TotalComponents = 2;
    state->dataAirSystemsData->PrimaryAirSystems(1).Branch(1).NodeNumOut = 1;

    state->dataAirSystemsData->PrimaryAirSystems(1).Branch(1).Comp.allocate(2);
    state->dataAirSystemsData->PrimaryAirSystems(1).Branch(1).Comp(1).Name = "Main Gas Humidifier";
    state->dataAirSystemsData->PrimaryAirSystems(1).Branch(1).Comp(1).TypeOf = "HUMIDIFIER:STEAM:GAS";
    state->dataAirSystemsData->PrimaryAirSystems(1).Branch(1).Comp(1).NodeNumIn = 1;
    state->dataAirSystemsData->PrimaryAirSystems(1).Branch(1).Comp(1).NodeNumOut = 1;
    state->dataAirSystemsData->PrimaryAirSystems(1).Branch(1).Comp(1).NumMeteredVars = 1;
    state->dataAirSystemsData->PrimaryAirSystems(1).Branch(1).Comp(1).MeteredVar.allocate(1);
    state->dataAirSystemsData->PrimaryAirSystems(1).Branch(1).Comp(1).MeteredVar(1).EndUse_CompMode = SystemReports::iEndUseType::CoolingOnly;
    state->dataAirSystemsData->PrimaryAirSystems(1).Branch(1).Comp(1).MeteredVar(1).CurMeterReading = 100.0;
    state->dataAirSystemsData->PrimaryAirSystems(1).Branch(1).Comp(1).MeteredVar(1).ResourceType = AssignResourceTypeNum("NaturalGas");

    state->dataAirSystemsData->PrimaryAirSystems(1).Branch(1).Comp(2).Name = "Main Gas Heating Coil";
    state->dataAirSystemsData->PrimaryAirSystems(1).Branch(1).Comp(2).TypeOf = "COIL:HEATING:DESUPERHEATER";
    state->dataAirSystemsData->PrimaryAirSystems(1).Branch(1).Comp(2).NodeNumIn = 2;
    state->dataAirSystemsData->PrimaryAirSystems(1).Branch(1).Comp(2).NodeNumOut = 2;
    state->dataAirSystemsData->PrimaryAirSystems(1).Branch(1).Comp(2).NumMeteredVars = 1;
    state->dataAirSystemsData->PrimaryAirSystems(1).Branch(1).Comp(2).MeteredVar.allocate(1);
    state->dataAirSystemsData->PrimaryAirSystems(1).Branch(1).Comp(2).MeteredVar(1).EndUse_CompMode = SystemReports::iEndUseType::CoolingOnly;
    state->dataAirSystemsData->PrimaryAirSystems(1).Branch(1).Comp(2).MeteredVar(1).CurMeterReading = 100.0;
    state->dataAirSystemsData->PrimaryAirSystems(1).Branch(1).Comp(2).MeteredVar(1).ResourceType = AssignResourceTypeNum("NaturalGas");

    state->dataLoopNodes->Node(1).MassFlowRate = 1.0;
    state->dataLoopNodes->Node(2).MassFlowRate = 1.0;

    state->dataSysRpts->SysHumidNaturalGas.allocate(1);
    state->dataSysRpts->SysHCCompNaturalGas.allocate(1);
    state->dataSysRpts->SysTotNaturalGas.allocate(1);
    state->dataSysRpts->SysTotPropane.allocate(1);
    state->dataSysRpts->SysHCCompPropane.allocate(1);
    state->dataSysRpts->SysHumidPropane.allocate(1);

    state->dataSysRpts->SysHumidNaturalGas(1) = 0;
    state->dataSysRpts->SysHCCompNaturalGas(1) = 0;
    state->dataSysRpts->SysTotNaturalGas(1) = 0;

    // Calculate SysHumidNaturalGas ("Air System Humidifier NaturalGas Energy" Output Variable)
    CalcSystemEnergyUse(*state,
                        CompLoadFlag,
                        AirLoopNum,
                        state->dataAirSystemsData->PrimaryAirSystems(1).Branch(1).Comp(1).TypeOf,
                        state->dataAirSystemsData->PrimaryAirSystems(1).Branch(1).Comp(1).MeteredVar(1).ResourceType,
                        CompLoad,
                        CompEnergyUse);

    // Calculate SysHCCompNaturalGas ("Air System Heating Coil NaturalGas Energy" Output Variable)
    CalcSystemEnergyUse(*state,
                        CompLoadFlag,
                        AirLoopNum,
                        state->dataAirSystemsData->PrimaryAirSystems(1).Branch(1).Comp(2).TypeOf,
                        state->dataAirSystemsData->PrimaryAirSystems(1).Branch(1).Comp(2).MeteredVar(1).ResourceType,
                        CompLoad,
                        CompEnergyUse);

    EXPECT_EQ(state->dataSysRpts->SysHumidNaturalGas(1), 100);
    EXPECT_EQ(state->dataSysRpts->SysHCCompNaturalGas(1), 100);

    // Allocate variables to run ReportSystemEnergyUse() function for SysTotNaturalGas ("Air System NaturalGas Energy")
    state->dataSysRpts->SysTotHTNG.allocate(1);
    state->dataSysRpts->SysFANCompHTNG.allocate(1);
    state->dataSysRpts->SysHCCompHTNG.allocate(1);
    state->dataSysRpts->SysHeatExHTNG.allocate(1);
    state->dataSysRpts->SysHumidHTNG.allocate(1);
    state->dataSysRpts->SysSolarCollectHeating.allocate(1);
    state->dataSysRpts->SysUserDefinedTerminalHeating.allocate(1);
    state->dataSysRpts->SysTotCLNG.allocate(1);
    state->dataSysRpts->SysCCCompCLNG.allocate(1);
    state->dataSysRpts->SysHeatExCLNG.allocate(1);
    state->dataSysRpts->SysEvapCLNG.allocate(1);
    state->dataSysRpts->DesDehumidCLNG.allocate(1);
    state->dataSysRpts->SysSolarCollectCooling.allocate(1);
    state->dataSysRpts->SysUserDefinedTerminalCooling.allocate(1);
    state->dataSysRpts->SysTotElec.allocate(1);
    state->dataSysRpts->SysFANCompElec.allocate(1);
    state->dataSysRpts->SysHCCompElec.allocate(1);
    state->dataSysRpts->SysCCCompElec.allocate(1);
    state->dataSysRpts->SysHCCompElecRes.allocate(1);
    state->dataSysRpts->SysHumidElec.allocate(1);
    state->dataSysRpts->DesDehumidElec.allocate(1);
    state->dataSysRpts->SysEvapElec.allocate(1);
    state->dataSysRpts->SysTotSteam.allocate(1);
    state->dataSysRpts->SysHCCompSteam.allocate(1);
    state->dataSysRpts->SysTotH2OCOLD.allocate(1);
    state->dataSysRpts->SysCCCompH2OCOLD.allocate(1);
    state->dataSysRpts->SysTotH2OHOT.allocate(1);
    state->dataSysRpts->SysHCCompH2OHOT.allocate(1);

    // Calculate SysTotNaturalGas ("Air System NaturalGas Energy")
    ReportSystemEnergyUse(*state);
    EXPECT_EQ(state->dataSysRpts->SysTotNaturalGas(1), 200);

    // Initialization for propane cases
    state->dataSysRpts->SysHumidNaturalGas(1) = 0;
    state->dataSysRpts->SysHCCompNaturalGas(1) = 0;
    state->dataSysRpts->SysTotNaturalGas(1) = 0;

    state->dataAirSystemsData->PrimaryAirSystems(1).Branch(1).Comp(1).MeteredVar(1).ResourceType = AssignResourceTypeNum("Propane");
    state->dataAirSystemsData->PrimaryAirSystems(1).Branch(1).Comp(2).MeteredVar(1).ResourceType = AssignResourceTypeNum("Propane");

    // Calculate SysHumidPropane ("Air System Humidifier Propane Energy" Output Variable)
    CalcSystemEnergyUse(*state,
                        CompLoadFlag,
                        AirLoopNum,
                        state->dataAirSystemsData->PrimaryAirSystems(1).Branch(1).Comp(1).TypeOf,
                        state->dataAirSystemsData->PrimaryAirSystems(1).Branch(1).Comp(1).MeteredVar(1).ResourceType,
                        CompLoad,
                        CompEnergyUse);

    // Calculate SysHCCompPropane ("Air System Heating Coil Propane Energy" Output Variable)
    CalcSystemEnergyUse(*state,
                        CompLoadFlag,
                        AirLoopNum,
                        state->dataAirSystemsData->PrimaryAirSystems(1).Branch(1).Comp(2).TypeOf,
                        state->dataAirSystemsData->PrimaryAirSystems(1).Branch(1).Comp(2).MeteredVar(1).ResourceType,
                        CompLoad,
                        CompEnergyUse);

    EXPECT_EQ(state->dataSysRpts->SysHumidPropane(1), 100);
    EXPECT_EQ(state->dataSysRpts->SysHCCompPropane(1), 100);

    // Calculate SysTotPropane ("Air System Propane Energy")
    ReportSystemEnergyUse(*state);
    EXPECT_EQ(state->dataSysRpts->SysTotPropane(1), 200);
}
TEST_F(EnergyPlusFixture, ReportMaxVentilationLoads_ZoneEquip)
{
    state->dataHVACGlobal->NumPrimaryAirSys = 0;
    state->dataAirSystemsData->PrimaryAirSystems.allocate(state->dataHVACGlobal->NumPrimaryAirSys);
    state->dataGlobal->NumOfZones = 1;
    state->dataHeatBal->Zone.allocate(state->dataGlobal->NumOfZones);
    state->dataHeatBal->ZonePreDefRep.allocate(state->dataGlobal->NumOfZones);
    state->dataHeatBal->ZnAirRpt.allocate(state->dataGlobal->NumOfZones);
    state->dataZoneEquip->ZoneEquipConfig.allocate(state->dataGlobal->NumOfZones);
    state->dataZoneEquip->ZoneEquipList.allocate(state->dataGlobal->NumOfZones);
    HeatBalanceManager::AllocateHeatBalArrays(*state);
    SystemReports::AllocateAndSetUpVentReports(*state);
    ZoneTempPredictorCorrector::InitZoneAirSetPoints(*state);
    state->dataLoopNodes->Node.allocate(20);

    // Set up OA requirements for one zone
    state->dataSize->NumOARequirements = 1;
    state->dataSize->OARequirements.allocate(state->dataSize->NumOARequirements);
    state->dataSize->OARequirements(1).OAFlowMethod = DataSizing::OAFlowSum;
    Real64 expectedVoz = 0.0;
    state->dataSize->OARequirements(1).OAFlowPerZone = 20;
    expectedVoz += state->dataSize->OARequirements(1).OAFlowPerZone;
    state->dataSize->OARequirements(1).OAFlowPerArea = 0.5;
    state->dataHeatBal->Zone(1).FloorArea = 1000.0;
    expectedVoz += state->dataSize->OARequirements(1).OAFlowPerArea * state->dataHeatBal->Zone(1).FloorArea;
    state->dataSize->OARequirements(1).OAFlowPerPerson = 0.1;
    state->dataHeatBal->ZoneIntGain.allocate(state->dataGlobal->NumOfZones);
    state->dataHeatBal->ZoneIntGain(1).NOFOCC = 100.0;
    expectedVoz += state->dataSize->OARequirements(1).OAFlowPerPerson * state->dataHeatBal->ZoneIntGain(1).NOFOCC;
    state->dataHeatBal->Zone(1).Multiplier = 2.0;
    state->dataHeatBal->Zone(1).ListMultiplier = 10.0;
    expectedVoz *= state->dataHeatBal->Zone(1).Multiplier;
    expectedVoz *= state->dataHeatBal->Zone(1).ListMultiplier;

    // Set up controlled zone equipment with just enough info for the ventilation report test
    state->dataZoneEquip->ZoneEquipConfig(1).IsControlled = true;
    state->dataZoneEquip->ZoneEquipConfig(1).ActualZoneNum = 1;
    state->dataZoneEquip->ZoneEquipConfig(1).ZoneDesignSpecOAIndex = 1;
    state->dataHeatBal->Zone(1).Volume = 10.0;
    state->dataZoneEquip->ZoneEquipConfig(1).EquipListIndex = 1;

    int NumEquip1 = 9;
    state->dataZoneEquip->ZoneEquipList(1).NumOfEquipTypes = NumEquip1;
    state->dataZoneEquip->ZoneEquipList(1).EquipType_Num.allocate(NumEquip1);
    state->dataZoneEquip->ZoneEquipList(1).EquipIndex.allocate(NumEquip1);

    // 1: WindowAC
    int equipNum = 1;
    int nodeNumOA = 1;
    state->dataZoneEquip->ZoneEquipList(1).EquipType_Num(equipNum) = DataZoneEquipment::WindowAC_Num;
    state->dataZoneEquip->ZoneEquipList(1).EquipIndex(equipNum) = 1;
    state->dataWindowAC->GetWindowACInputFlag = false;
    state->dataWindowAC->WindAC.allocate(1);
    state->dataWindowAC->WindAC(1).OutsideAirNode = nodeNumOA;
    state->dataLoopNodes->Node(nodeNumOA).MassFlowRate = 0.1;

    // 2: VRF
    ++equipNum;
    ++nodeNumOA;
    state->dataZoneEquip->ZoneEquipList(1).EquipType_Num(equipNum) = DataZoneEquipment::VRFTerminalUnit_Num;
    state->dataZoneEquip->ZoneEquipList(1).EquipIndex(equipNum) = 1;
    state->dataHVACVarRefFlow->GetVRFInputFlag = false;
    state->dataHVACVarRefFlow->NumVRFTU = 1;
    state->dataHVACVarRefFlow->VRFTU.allocate(1);
    state->dataHVACVarRefFlow->VRFTU(1).VRFTUOAMixerOANodeNum = nodeNumOA;
    state->dataLoopNodes->Node(nodeNumOA).MassFlowRate = 2.0;

    // 3: PTAC
    ++equipNum;
    ++nodeNumOA;
    state->dataZoneEquip->ZoneEquipList(1).EquipType_Num(equipNum) = DataZoneEquipment::PkgTermACAirToAir_Num;
    state->dataZoneEquip->ZoneEquipList(1).EquipIndex(equipNum) = 1;
    state->dataPTHP->GetPTUnitInputFlag = false;
    state->dataPTHP->NumPTUs = 1;
    state->dataPTHP->PTUnit.allocate(1);
    state->dataPTHP->PTUnit(1).OutsideAirNode = nodeNumOA;
    state->dataLoopNodes->Node(nodeNumOA).MassFlowRate = 30.0;

    // 4: FanCoil
    ++equipNum;
    ++nodeNumOA;
    state->dataZoneEquip->ZoneEquipList(1).EquipType_Num(equipNum) = DataZoneEquipment::FanCoil4Pipe_Num;
    state->dataZoneEquip->ZoneEquipList(1).EquipIndex(equipNum) = 1;
    state->dataFanCoilUnits->GetFanCoilInputFlag = false;
    state->dataFanCoilUnits->NumFanCoils = 1;
    state->dataFanCoilUnits->FanCoil.allocate(1);
    state->dataFanCoilUnits->FanCoil(1).OutsideAirNode = nodeNumOA;
    state->dataLoopNodes->Node(nodeNumOA).MassFlowRate = 400.0;

    // 5: Unit Ventilator
    ++equipNum;
    ++nodeNumOA;
    state->dataZoneEquip->ZoneEquipList(1).EquipType_Num(equipNum) = DataZoneEquipment::UnitVentilator_Num;
    state->dataZoneEquip->ZoneEquipList(1).EquipIndex(equipNum) = 1;
    state->dataUnitVentilators->GetUnitVentilatorInputFlag = false;
    state->dataUnitVentilators->NumOfUnitVents = 1;
    state->dataUnitVentilators->UnitVent.allocate(1);
    state->dataUnitVentilators->UnitVent(1).OutsideAirNode = nodeNumOA;
    state->dataLoopNodes->Node(nodeNumOA).MassFlowRate = 5000.0;

    // 6: Purchased Air (Ideal Loads)
    ++equipNum;
    ++nodeNumOA;
    state->dataZoneEquip->ZoneEquipList(1).EquipType_Num(equipNum) = DataZoneEquipment::PurchasedAir_Num;
    state->dataZoneEquip->ZoneEquipList(1).EquipIndex(equipNum) = 1;
    state->dataPurchasedAirMgr->GetPurchAirInputFlag = false;
    state->dataPurchasedAirMgr->NumPurchAir = 1;
    state->dataPurchasedAirMgr->PurchAir.allocate(1);
    state->dataPurchasedAirMgr->PurchAir(1).OutdoorAirMassFlowRate = 60000.0;

    // 7: ERV
    ++equipNum;
    ++nodeNumOA;
    state->dataZoneEquip->ZoneEquipList(1).EquipType_Num(equipNum) = DataZoneEquipment::ERVStandAlone_Num;
    state->dataZoneEquip->ZoneEquipList(1).EquipIndex(equipNum) = 1;
    state->dataHVACStandAloneERV->GetERVInputFlag = false;
    state->dataHVACStandAloneERV->NumStandAloneERVs = 1;
    state->dataHVACStandAloneERV->StandAloneERV.allocate(1);
    state->dataHVACStandAloneERV->StandAloneERV(1).SupplyAirInletNode = nodeNumOA;
    state->dataLoopNodes->Node(nodeNumOA).MassFlowRate = 700000.0;

    // 8: Outdoor air unit
    ++equipNum;
    ++nodeNumOA;
    state->dataZoneEquip->ZoneEquipList(1).EquipType_Num(equipNum) = DataZoneEquipment::OutdoorAirUnit_Num;
    state->dataZoneEquip->ZoneEquipList(1).EquipIndex(equipNum) = 1;
    state->dataOutdoorAirUnit->GetOutdoorAirUnitInputFlag = false;
    state->dataOutdoorAirUnit->NumOfOAUnits = 1;
    state->dataOutdoorAirUnit->OutAirUnit.allocate(1);
    state->dataOutdoorAirUnit->OutAirUnit(1).OutsideAirNode = nodeNumOA;
    state->dataLoopNodes->Node(nodeNumOA).MassFlowRate = 8000000.0;

    // 9: Zone Hybrid Unitary
    ++equipNum;
    ++nodeNumOA;
    state->dataZoneEquip->ZoneEquipList(1).EquipType_Num(equipNum) = DataZoneEquipment::ZoneHybridEvaporativeCooler_Num;
    state->dataZoneEquip->ZoneEquipList(1).EquipIndex(equipNum) = 1;
    state->dataHybridUnitaryAC->GetInputZoneHybridEvap = false;
    state->dataHybridUnitaryAC->NumZoneHybridEvap = 1;
    state->dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner.allocate(1);
    state->dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(1).SecondaryInletNode = nodeNumOA;
    state->dataLoopNodes->Node(nodeNumOA).MassFlowRate = 90000000.0;

    // Call reporting function
    state->dataSysRpts->VentReportStructureCreated = true;
    state->dataSysRpts->VentLoadsReportEnabled = true;
    SystemReports::ReportMaxVentilationLoads(*state);

    EXPECT_NEAR(state->dataSysRpts->ZoneTargetVentilationFlowVoz(1), expectedVoz, 0.001);
    EXPECT_NEAR(state->dataSysRpts->ZoneOAMassFlow(1), 98765432.1, 0.001);
}
} // namespace EnergyPlus
