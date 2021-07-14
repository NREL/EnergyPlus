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
#include <fstream>

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/ConfiguredFunctions.hh>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataAirLoop.hh>
#include <EnergyPlus/DataAirSystems.hh>
#include <EnergyPlus/DataContaminantBalance.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobalConstants.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataZoneControls.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/EvaporativeCoolers.hh>
#include <EnergyPlus/FileSystem.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/Humidifiers.hh>
#include <EnergyPlus/HybridEvapCoolingModel.hh>
#include <EnergyPlus/HybridUnitaryAirConditioners.hh>
#include <EnergyPlus/IOFiles.hh>
#include <EnergyPlus/MixedAir.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SizingManager.hh>
#include <EnergyPlus/SystemReports.hh>

using namespace EnergyPlus::MixedAir;
using namespace EnergyPlus::DataContaminantBalance;
using namespace EnergyPlus::DataAirLoop;
using namespace EnergyPlus::DataAirSystems;
using namespace EnergyPlus::DataSizing;
using namespace EnergyPlus::DataHeatBalance;
using namespace EnergyPlus::ScheduleManager;
using namespace EnergyPlus::DataEnvironment;
using namespace EnergyPlus::DataHeatBalFanSys;
using namespace EnergyPlus::DataZoneEquipment;
using namespace EnergyPlus::DataLoopNode;
using namespace EnergyPlus::DataZoneEnergyDemands;
using namespace EnergyPlus::DataZoneControls;
using namespace EnergyPlus::HeatBalanceManager;
using namespace EnergyPlus::Humidifiers;
using namespace EnergyPlus::OutputReportPredefined;
using namespace EnergyPlus::SizingManager;
using namespace EnergyPlus::SystemReports;

using namespace EnergyPlus::DataContaminantBalance;
using namespace EnergyPlus::MixedAir;
using namespace EnergyPlus;
using namespace EnergyPlus::SizingManager;
using EnergyPlus::CurveManager::CurveValue;
using EnergyPlus::CurveManager::GetCurveName;
using EnergyPlus::CurveManager::GetNormalPoint;
using EnergyPlus::Psychrometrics::PsyHFnTdbRhPb;
using EnergyPlus::Psychrometrics::PsyRhFnTdbWPb;
using EnergyPlus::Psychrometrics::PsyWFnTdbRhPb;
using namespace EnergyPlus::ScheduleManager;
using EnergyPlus::HybridEvapCoolingModel::CMode;
using EnergyPlus::HybridEvapCoolingModel::CSetting;
using EnergyPlus::HybridEvapCoolingModel::Model;
using namespace EnergyPlus::HybridUnitaryAirConditioners;

namespace EnergyPlus {
std::vector<std::string> getAllLinesInFile2(fs::path const &filePath)
{
    std::ifstream infile(filePath);
    std::vector<std::string> lines;
    std::string line;
    while (std::getline(infile, line)) {
        lines.push_back(line);
    }
    return lines;
}

std::vector<std::string> parseLine(std::string line)
{
    std::vector<std::string> vect;
    std::stringstream ss(line);
    std::string token;

    while (std::getline(ss, token, ',')) {
        vect.push_back(token);
    }
    return vect;
}

TEST_F(EnergyPlusFixture, Test_UnitaryHybridAirConditioner_Unittest)
{
    std::vector<std::string> snippet =
        getAllLinesInFile2(configured_source_directory() / "tst/EnergyPlus/unit/Resources/UnitaryHybridUnitTest_DOSA.idf");
    std::string string = delimited_string(snippet);
    ASSERT_TRUE(process_idf(string));
    // setup environment
    bool ErrorsFound(false);
    GetZoneData(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    // Initialize schedule values
    state->dataGlobal->TimeStep = 1;
    state->dataHVACGlobal->TimeStepSys = 1;
    state->dataGlobal->NumOfTimeStepInHour = 1;
    state->dataGlobal->MinutesPerTimeStep = 60;
    state->dataEnvrn->Month = 1;
    state->dataEnvrn->DayOfMonth = 21;
    state->dataGlobal->HourOfDay = 1;
    state->dataEnvrn->DSTIndicator = 0;
    state->dataEnvrn->DayOfWeek = 2;
    state->dataEnvrn->HolidayIndex = 0;
    state->dataGlobal->WarmupFlag = false;
    state->dataEnvrn->DayOfYear_Schedule = General::OrdinalDay(state->dataEnvrn->Month, state->dataEnvrn->DayOfMonth, 1);
    ScheduleManager::UpdateScheduleValues(*state);
    // Initialize zone areas and volumes - too many other things need to be set up to do these in the normal routines
    state->dataHeatBal->Zone(1).FloorArea = 232.26;
    state->dataEnvrn->StdRhoAir = 1.225;
    state->dataEnvrn->OutBaroPress = 101325;
    state->dataHeatBal->ZoneIntGain.allocate(1);

    SizingManager::GetOARequirements(*state);
    GetOAControllerInputs(*state);
    using DataZoneEquipment::CalcDesignSpecificationOutdoorAir;

    // Setup performance tables
    using namespace EnergyPlus::DataEnvironment;
    // process schedules
    ProcessScheduleInput(*state); // read schedules
    UpdateScheduleValues(*state);
    // Get Unitary system
    GetInputZoneHybridUnitaryAirConditioners(*state, ErrorsFound);
    // All to get OA requirements
    GetOARequirements(*state);

    EXPECT_FALSE(ErrorsFound);
    // Initialize unit
    InitZoneHybridUnitaryAirConditioners(*state, 1, 1);
    Model *pZoneHybridUnitaryAirConditioner = &state->dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(1);
    // setup local variables for model inputs
    Real64 Tosa, Tra, Wra, Wosa, RHosa, RHra, DesignMinVR, Requestedheating, RequestedCooling, Requested_Humidification, Requested_Dehumidification;
    RHosa = 0;
    std::string TimeDate;
    int modenumber = 0;
    Real64 MsaRatio, OSAF;
    MsaRatio = OSAF = 1;

    Requestedheating = RequestedCooling = Requested_Humidification = Requested_Dehumidification = 0;

    DesignMinVR = 1.622720855; // Zone Hybrid Unitary HVAC Requested Outdoor Air Ventilation Mass Flow Rate
    Tra = 22.93929413;         // Zone Hybrid Unitary HVAC Return Air Temperature
    Tosa = 26.67733333;        // Zone Hybrid Unitary HVAC Outside Air Temperature
    RHra = 17.3042157;         // Zone Hybrid Unitary HVAC Return Air Relative Humidity
    RHosa = 13.1602401;        // Zone Hybrid Unitary HVAC Outside Air Relative Humidity
    Wra = PsyWFnTdbRhPb(*state, Tra, RHra / 100, 101325);
    Wosa = PsyWFnTdbRhPb(*state, Tosa, RHosa / 100, 101325);
    pZoneHybridUnitaryAirConditioner->InletTemp = Tra;
    pZoneHybridUnitaryAirConditioner->InletHumRat = Wra;
    pZoneHybridUnitaryAirConditioner->InletEnthalpy = PsyHFnTdbRhPb(*state, Tra, RHra / 100, 101325, "test");
    pZoneHybridUnitaryAirConditioner->InletPressure = 101325;
    pZoneHybridUnitaryAirConditioner->InletRH = RHra / 100;
    pZoneHybridUnitaryAirConditioner->SecInletTemp = Tosa;
    pZoneHybridUnitaryAirConditioner->SecInletHumRat = Wosa;
    pZoneHybridUnitaryAirConditioner->SecInletEnthalpy = PsyHFnTdbRhPb(*state, Tosa, RHosa / 100, 101325, "test");
    pZoneHybridUnitaryAirConditioner->SecInletPressure = 101325;
    pZoneHybridUnitaryAirConditioner->SecInletRH = RHosa / 100;

    // Scenario 1: Hi Cooling

    Requestedheating = -122396.255;  // Watts (Zone Predicted Sensible Load to Heating Setpoint Heat Transfer Rate
    RequestedCooling = -58469.99445; // Watts (Zone Predicted Sensible Load to Cooling Setpoint Heat Transfer Rate
    pZoneHybridUnitaryAirConditioner->Initialize(1);
    pZoneHybridUnitaryAirConditioner->InitializeModelParams();
    pZoneHybridUnitaryAirConditioner->doStep(
        *state, RequestedCooling, Requestedheating, Requested_Humidification, Requested_Dehumidification, DesignMinVR);

    // output results
    Real64 NormalizationDivisor = 3.0176;
    Real64 ScaledMaxMsa = pZoneHybridUnitaryAirConditioner->ScaledSystemMaximumSupplyAirMassFlowRate;
    Real64 MinFlowFraction = DesignMinVR / ScaledMaxMsa;
    modenumber = pZoneHybridUnitaryAirConditioner->PrimaryMode;
    Real64 Tsa = pZoneHybridUnitaryAirConditioner->OutletTemp;
    Real64 Msa = pZoneHybridUnitaryAirConditioner->OutletMassFlowRate;
    Real64 deliveredSC = pZoneHybridUnitaryAirConditioner->UnitSensibleCoolingRate;
    Real64 deliveredSH = pZoneHybridUnitaryAirConditioner->UnitSensibleHeatingRate;
    Real64 averageOSAF = pZoneHybridUnitaryAirConditioner->averageOSAF;
    Real64 Electricpower = pZoneHybridUnitaryAirConditioner->FinalElectricalPower;

    // checks
    EXPECT_EQ(modenumber, 3); // IEC and DX2 Mode
    EXPECT_NEAR(1.0, averageOSAF, 0.001);
    EXPECT_GT(deliveredSC, 0);
    EXPECT_NEAR(0.0, deliveredSH, 0.001);
    EXPECT_LT(Tsa, Tra);
    EXPECT_GT(Msa, DesignMinVR);
    EXPECT_GT(Electricpower, 10500 / NormalizationDivisor * MinFlowFraction);
    EXPECT_LT(Electricpower, 12500 / NormalizationDivisor);

    // Scenario 2: high cooling larger system

    pZoneHybridUnitaryAirConditioner->Initialize(1);
    pZoneHybridUnitaryAirConditioner->InitializeModelParams();
    pZoneHybridUnitaryAirConditioner->ScalingFactor = pZoneHybridUnitaryAirConditioner->ScalingFactor * 2;
    pZoneHybridUnitaryAirConditioner->ScaledSystemMaximumSupplyAirMassFlowRate =
        pZoneHybridUnitaryAirConditioner->ScaledSystemMaximumSupplyAirMassFlowRate * 2;
    pZoneHybridUnitaryAirConditioner->doStep(
        *state, RequestedCooling, Requestedheating, Requested_Humidification, Requested_Dehumidification, DesignMinVR);

    // output results
    modenumber = pZoneHybridUnitaryAirConditioner->PrimaryMode;
    Tsa = pZoneHybridUnitaryAirConditioner->OutletTemp;
    Msa = pZoneHybridUnitaryAirConditioner->OutletMassFlowRate;
    deliveredSC = pZoneHybridUnitaryAirConditioner->UnitSensibleCoolingRate;
    deliveredSH = pZoneHybridUnitaryAirConditioner->UnitSensibleHeatingRate;
    averageOSAF = pZoneHybridUnitaryAirConditioner->averageOSAF;
    Electricpower = pZoneHybridUnitaryAirConditioner->FinalElectricalPower;

    // checks
    EXPECT_EQ(modenumber, 1); // IEC Mode
    EXPECT_NEAR(1.0, averageOSAF, 0.001);
    EXPECT_GT(deliveredSC, 0);
    EXPECT_NEAR(0.0, deliveredSH, 0.001);
    EXPECT_LT(Tsa, Tra);
    EXPECT_GT(Msa, DesignMinVR);
    EXPECT_GT(Electricpower, 4000 / NormalizationDivisor * MinFlowFraction);
    EXPECT_LT(Electricpower, 5000 / NormalizationDivisor);

    // Scenario 3: Outside of env conditions. should go to standby and have standby energy
    pZoneHybridUnitaryAirConditioner->Initialize(1);
    pZoneHybridUnitaryAirConditioner->InitializeModelParams();
    pZoneHybridUnitaryAirConditioner->ScalingFactor = pZoneHybridUnitaryAirConditioner->ScalingFactor / 2; // reset back to original values
    pZoneHybridUnitaryAirConditioner->ScaledSystemMaximumSupplyAirMassFlowRate =
        pZoneHybridUnitaryAirConditioner->ScaledSystemMaximumSupplyAirMassFlowRate / 2; // reset back to original values
    pZoneHybridUnitaryAirConditioner->SecInletTemp = 150;
    pZoneHybridUnitaryAirConditioner->SecInletHumRat = 0;
    pZoneHybridUnitaryAirConditioner->doStep(
        *state, RequestedCooling, Requestedheating, Requested_Humidification, Requested_Dehumidification, DesignMinVR);

    // output results
    modenumber = pZoneHybridUnitaryAirConditioner->PrimaryMode;
    Electricpower = pZoneHybridUnitaryAirConditioner->FinalElectricalPower;

    // checks
    EXPECT_EQ(modenumber, 0); // Standby Mode
    EXPECT_NEAR(Electricpower, 244 / NormalizationDivisor, 1);

    // Scenario 4: Low Cooling
    Requestedheating = -64358.68966; //-
    RequestedCooling = -633.6613591; // W
    /// add all the correct values to set in pZoneHybridUnitaryAirConditioner
    pZoneHybridUnitaryAirConditioner->Initialize(1);
    pZoneHybridUnitaryAirConditioner->InitializeModelParams();
    pZoneHybridUnitaryAirConditioner->SecInletTemp = Tosa;
    pZoneHybridUnitaryAirConditioner->SecInletHumRat = Wosa;
    pZoneHybridUnitaryAirConditioner->doStep(
        *state, RequestedCooling, Requestedheating, Requested_Humidification, Requested_Dehumidification, DesignMinVR);

    // output results
    modenumber = pZoneHybridUnitaryAirConditioner->PrimaryMode;
    Tsa = pZoneHybridUnitaryAirConditioner->OutletTemp;
    deliveredSC = pZoneHybridUnitaryAirConditioner->UnitSensibleCoolingRate;
    deliveredSH = pZoneHybridUnitaryAirConditioner->UnitSensibleHeatingRate;
    averageOSAF = pZoneHybridUnitaryAirConditioner->averageOSAF;
    Electricpower = pZoneHybridUnitaryAirConditioner->FinalElectricalPower;
    // checks
    EXPECT_EQ(modenumber, 1); // IEC Mode
    EXPECT_NEAR(1.0, averageOSAF, 0.001);
    EXPECT_GT(deliveredSC, 0);
    EXPECT_NEAR(0.0, deliveredSH, 0.001);
    EXPECT_LT(Tsa, Tra);
    EXPECT_GT(Electricpower, 4000 / NormalizationDivisor * MinFlowFraction);
    EXPECT_LT(Electricpower, 5000 / NormalizationDivisor);

    // Scenario 5: No Heating or Cooling, Minimum Ventilation
    Requestedheating = -55795.8058;
    RequestedCooling = 8171.47128;
    pZoneHybridUnitaryAirConditioner->Initialize(1);
    pZoneHybridUnitaryAirConditioner->InitializeModelParams();
    pZoneHybridUnitaryAirConditioner->SecInletTemp = Tosa;
    pZoneHybridUnitaryAirConditioner->SecInletHumRat = Wosa;
    pZoneHybridUnitaryAirConditioner->doStep(
        *state, RequestedCooling, Requestedheating, Requested_Humidification, Requested_Dehumidification, DesignMinVR);

    // output results
    modenumber = pZoneHybridUnitaryAirConditioner->PrimaryMode;
    Tsa = pZoneHybridUnitaryAirConditioner->OutletTemp;
    Msa = pZoneHybridUnitaryAirConditioner->OutletMassFlowRate;
    Electricpower = pZoneHybridUnitaryAirConditioner->FinalElectricalPower;
    // checks
    EXPECT_EQ(modenumber, 4); // Ventilation Mode
    EXPECT_NEAR(Tsa, Tosa, 1.0);
    EXPECT_NEAR(Msa, DesignMinVR, 0.001);
    EXPECT_GT(Electricpower, 4000 / NormalizationDivisor * MinFlowFraction);
    EXPECT_LT(Electricpower, 5000 / NormalizationDivisor);

    // check fan heat calculation in supply air stream if not included in lookup tables
    pZoneHybridUnitaryAirConditioner->FanHeatGain = true;
    pZoneHybridUnitaryAirConditioner->FanHeatGainLocation = "SUPPLYAIRSTREAM";
    pZoneHybridUnitaryAirConditioner->FanHeatInAirFrac = 1.0;
    pZoneHybridUnitaryAirConditioner->Initialize(1);
    pZoneHybridUnitaryAirConditioner->InitializeModelParams();
    pZoneHybridUnitaryAirConditioner->SecInletTemp = Tosa;
    pZoneHybridUnitaryAirConditioner->SecInletHumRat = Wosa;
    pZoneHybridUnitaryAirConditioner->doStep(
        *state, RequestedCooling, Requestedheating, Requested_Humidification, Requested_Dehumidification, DesignMinVR);

    // output results

    Tsa = pZoneHybridUnitaryAirConditioner->OutletTemp;
    // check that supply air temperature increases due to fan heat
    EXPECT_NEAR(Tsa, Tosa + 0.36, 0.1);

    // check fan heat calculation in mixed air stream if not included in lookup tables
    pZoneHybridUnitaryAirConditioner->FanHeatGain = true;
    pZoneHybridUnitaryAirConditioner->FanHeatGainLocation = "MIXEDAIRSTREAM";
    pZoneHybridUnitaryAirConditioner->FanHeatInAirFrac = 1.0;
    pZoneHybridUnitaryAirConditioner->Initialize(1);
    pZoneHybridUnitaryAirConditioner->InitializeModelParams();
    pZoneHybridUnitaryAirConditioner->SecInletTemp = Tosa;
    pZoneHybridUnitaryAirConditioner->SecInletHumRat = Wosa;
    pZoneHybridUnitaryAirConditioner->doStep(
        *state, RequestedCooling, Requestedheating, Requested_Humidification, Requested_Dehumidification, DesignMinVR);

    // output results
    Tsa = pZoneHybridUnitaryAirConditioner->OutletTemp;
    // check that supply air stream is increased due to fan heat added to the mixed air stream and passing through to the supply air stream
    EXPECT_NEAR(Tsa, Tosa + 0.36, 0.1);

    // Scenario 6: Availability Manager Off
    Requestedheating = -122396.255;  // Watts (Zone Predicted Sensible Load to Heating Setpoint Heat Transfer Rate
    RequestedCooling = -58469.99445; // Watts (Zone Predicted Sensible Load to Cooling Setpoint Heat Transfer Rate
    pZoneHybridUnitaryAirConditioner->Initialize(1);
    pZoneHybridUnitaryAirConditioner->InitializeModelParams();
    pZoneHybridUnitaryAirConditioner->SecInletTemp = Tosa;
    pZoneHybridUnitaryAirConditioner->SecInletHumRat = Wosa;
    pZoneHybridUnitaryAirConditioner->AvailStatus = 1;
    pZoneHybridUnitaryAirConditioner->doStep(
        *state, RequestedCooling, Requestedheating, Requested_Humidification, Requested_Dehumidification, DesignMinVR);

    // output results
    modenumber = pZoneHybridUnitaryAirConditioner->PrimaryMode;
    Msa = pZoneHybridUnitaryAirConditioner->OutletMassFlowRate;
    deliveredSC = pZoneHybridUnitaryAirConditioner->UnitSensibleCoolingRate;
    Electricpower = pZoneHybridUnitaryAirConditioner->FinalElectricalPower;

    // checks
    EXPECT_EQ(modenumber, 0); // Standby Mode
    EXPECT_EQ(Msa, 0);
    EXPECT_EQ(deliveredSC, 0);
    EXPECT_NEAR(Electricpower, 244 / NormalizationDivisor, 1);

    // Scenario 7: Check ventilation load is being accounted for
    state->dataGlobal->NumOfZones = 1;
    state->dataZoneEnergyDemand->ZoneSysEnergyDemand.allocate(state->dataGlobal->NumOfZones);
    state->dataZoneEnergyDemand->DeadBandOrSetback.allocate(state->dataGlobal->NumOfZones);

    HeatBalanceManager::GetZoneData(*state, ErrorsFound); // read zone data
    EXPECT_FALSE(ErrorsFound);                            // expect no errors
    DataZoneEquipment::GetZoneEquipmentData(*state);      // read zone equipment    SystemReports::ReportMaxVentilationLoads();
    state->dataZoneEquip->ZoneEquipInputsFilled = true;
    state->dataHeatBal->ZnAirRpt.allocate(state->dataGlobal->NumOfZones);
    state->dataHeatBalFanSys->MAT.allocate(state->dataGlobal->NumOfZones);
    state->dataHeatBalFanSys->ZoneAirHumRatAvg.allocate(state->dataGlobal->NumOfZones);
    SystemReports::AllocateAndSetUpVentReports(*state);
    state->dataZoneEnergyDemand->ZoneSysEnergyDemand(1).TotalOutputRequired = 58469.99445;
    state->dataZoneEnergyDemand->DeadBandOrSetback(1) = false;
    state->dataZoneEquip->ZoneEquipList(state->dataZoneEquip->ZoneEquipConfig(1).EquipListIndex).EquipIndex(1) = 1;
    CreateEnergyReportStructure(*state);

    SizingManager::GetOARequirements(*state);
    using DataZoneEquipment::CalcDesignSpecificationOutdoorAir;

    // Setup performance tables
    using namespace EnergyPlus::DataEnvironment;
    // process schedules
    ProcessScheduleInput(*state); // read schedules
    UpdateScheduleValues(*state);
    // Get Unitary system
    GetInputZoneHybridUnitaryAirConditioners(*state, ErrorsFound);
    // All to get OA requirements
    GetOARequirements(*state);

    Requestedheating = -122396.255;  // Watts (Zone Predicted Sensible Load to Heating Setpoint Heat Transfer Rate
    RequestedCooling = -58469.99445; // Watts (Zone Predicted Sensible Load to Cooling Setpoint Heat Transfer Rate
    pZoneHybridUnitaryAirConditioner->Initialize(1);
    pZoneHybridUnitaryAirConditioner->InitializeModelParams();
    pZoneHybridUnitaryAirConditioner->InletTemp = Tra;
    pZoneHybridUnitaryAirConditioner->SecInletTemp = Tosa;
    pZoneHybridUnitaryAirConditioner->SecInletMassFlowRate = DesignMinVR;
    pZoneHybridUnitaryAirConditioner->doStep(
        *state, RequestedCooling, Requestedheating, Requested_Humidification, Requested_Dehumidification, DesignMinVR);
    ReportZoneHybridUnitaryAirConditioners(*state, 1);

    SystemReports::ReportMaxVentilationLoads(*state);
    // output results
    Real64 zone_oa_mass_flow = state->dataSysRpts->ZoneOAMassFlow(1); // OA flow reported to the zone from the unitary hybrid system

    // checks
    EXPECT_EQ(zone_oa_mass_flow, DesignMinVR); // reported zone OA flow matches unitary hybrid OA flow

    // Scenario 8: Check output meters and report
    int NumFound;

    std::string TypeOfComp = "ZoneHVAC:HybridUnitaryHVAC";
    std::string NameOfComp = pZoneHybridUnitaryAirConditioner->Name;
    int NumVariables = GetNumMeteredVariables(*state, TypeOfComp, NameOfComp);
    Array1D_int VarIndexes(NumVariables);                            // Variable Numbers
    Array1D<OutputProcessor::VariableType> VarTypes(NumVariables);   // Variable Types (1=integer, 2=real, 3=meter)
    Array1D<OutputProcessor::TimeStepType> IndexTypes(NumVariables); // Variable Index Types (1=Zone,2=HVAC)
    Array1D<OutputProcessor::Unit> unitsForVar(NumVariables);        // units from enum for each variable
    std::map<int, DataGlobalConstants::ResourceType> ResourceTypes;  // ResourceTypes for each variable
    Array1D_string EndUses(NumVariables);                            // EndUses for each variable
    Array1D_string Groups(NumVariables);                             // Groups for each variable
    Array1D_string Names(NumVariables);                              // Variable Names for each variable

    for (int varN = 1; varN <= NumVariables; ++varN) {
        ResourceTypes.insert(std::pair<int, DataGlobalConstants::ResourceType>(varN, DataGlobalConstants::ResourceType::None));
    }

    GetMeteredVariables(
        *state, TypeOfComp, NameOfComp, VarIndexes, VarTypes, IndexTypes, unitsForVar, ResourceTypes, EndUses, Groups, Names, NumFound);

    // output results
    Real64 MaxFlow = pZoneHybridUnitaryAirConditioner->ScaledSystemMaximumSupplyAirVolumeFlowRate;

    // Check the meters associated with the ZoneHVAC:HybridUnitaryHVAC outputs
    EXPECT_EQ(21, NumFound);
    EXPECT_EQ(ResourceTypes.at(1), DataGlobalConstants::ResourceType::EnergyTransfer); // ENERGYTRANSFER - Cooling
    EXPECT_EQ(EndUses(1), "COOLINGCOILS");
    EXPECT_EQ(Groups(1), "HVAC");
    EXPECT_EQ(ResourceTypes.at(2), DataGlobalConstants::ResourceType::EnergyTransfer); // ENERGYTRANSFER - Heating
    EXPECT_EQ(EndUses(2), "HEATINGCOILS");
    EXPECT_EQ(Groups(2), "HVAC");
    EXPECT_EQ(ResourceTypes.at(3), DataGlobalConstants::ResourceType::Electricity); // ELECTRIC - Cooling Energy
    EXPECT_EQ(EndUses(3), "COOLING");
    EXPECT_EQ(Groups(3), "HVAC");
    EXPECT_EQ(ResourceTypes.at(4), DataGlobalConstants::ResourceType::Electricity); // ELECTRIC - Fan Energy
    EXPECT_EQ(EndUses(4), "FANS");
    EXPECT_EQ(Groups(4), "HVAC");
    EXPECT_EQ(ResourceTypes.at(5),
              DataGlobalConstants::ResourceType::Natural_Gas); // NATURALGAS - Secondary Fuel Type - specified in UnitaryHybridUnitTest_DOSA.idf
    EXPECT_EQ(EndUses(5), "COOLING");
    EXPECT_EQ(Groups(5), "HVAC");
    EXPECT_EQ(ResourceTypes.at(6),
              DataGlobalConstants::ResourceType::DistrictCooling); // DISTRICTCOOLING - Third Fuel Type - specified in UnitaryHybridUnitTest_DOSA.idf
    EXPECT_EQ(EndUses(6), "COOLING");
    EXPECT_EQ(Groups(6), "HVAC");
    EXPECT_EQ(ResourceTypes.at(7), DataGlobalConstants::ResourceType::Water); // WATER - Cooling Water Use
    EXPECT_EQ(EndUses(7), "COOLING");
    EXPECT_EQ(Groups(7), "HVAC");

    // Check that unit is included in Component Sizing Summary Report
    EXPECT_EQ("ZoneHVAC:HybridUnitaryHVAC", state->dataOutRptPredefined->CompSizeTableEntry(1).typeField);
    EXPECT_EQ("MUNTERSEPX5000", state->dataOutRptPredefined->CompSizeTableEntry(1).nameField);
    EXPECT_EQ("Scaled Maximum Supply Air Volume Flow Rate [m3/s]", state->dataOutRptPredefined->CompSizeTableEntry(1).description);
    EXPECT_EQ(MaxFlow, state->dataOutRptPredefined->CompSizeTableEntry(1).valField);
}

TEST_F(EnergyPlusFixture, Test_UnitaryHybridAirConditioner_ValidateFieldsParsing)
{
    std::string idf_objects = delimited_string({
        "ZoneHVAC:HybridUnitaryHVAC,",
        "Hybrid Unit 1,          !- Name",
        "ALWAYS_ON,               !- Availability Schedule Name",
        ",                        !- Availability Manager List Name",
        ",                        !- Minimum Supply Air Temperature Schedule Name",
        ",                        !- Maximum Supply Air Temperature Schedule Name",
        ",                        !- Minimum Supply Air Humidity Ratio Schedule Name",
        ",                        !- Maximum Supply Air Humidity Ratio Schedule Name",
        "AUTOMATIC,               !- Method to Choose Controlled Inputs and Part Runtime Fraction",
        "Return Air Node 1 Name,  !- Return Air Node Name",
        "Outside Air Inlet 1 Node,  !- Outside Air Node Name",
        "Zone Inlet 1 Node,    !- Supply Air Node Name",
        "Relief 1 Node,        !- Relief Node Name",
        "2.51,                    !- System Maximum Supply AirFlow Rate {m3/s}",
        ",                        !- External Static Pressure at System Maximum Supply Air Flow Rate {Pa}",
        "Yes,                     !- Fan Heat Included in Lookup Tables",
        ",                        !- Fan Heat Gain Location",
        ",                        !- Fan Heat Gain In Airstream Fraction",
        "1,                       !- Scaling Factor",
        "10,                      !- Minimum Time Between Mode Change {minutes}",
        "Electricity,             !- First fuel type",
        "NaturalGas,              !- Second fuel type",
        "DistrictCooling,         !- Third fuel type",
        ",                        !- Objective Function Minimizes",
        "SZ DSOA SPACE 1,        !- Design Specification Outdoor Air Object Name",
        "Mode0 Standby,           !- Mode0 Name",
        ",                        !- Mode0 Supply Air Temperature Lookup Table Name",
        ",                        !- Mode0 Supply Air Humidity Ratio Lookup Table Name",
        ",                        !- Mode0 System Electric Power Lookup Table Name",
        ",                        !- Mode0 Supply Fan Electric Power Lookup Table Name",
        ",                        !- Mode0 External Static Pressure Lookup Table Name",
        ",                        !- Mode0 System Second Fuel Consumption Lookup Table Name",
        ",                        !- Mode0 System Third Fuel Consumption Lookup Table Name",
        ",                        !- Mode0 System Water Use Lookup Table Name",
        "0,                       !- Mode0 Outside Air Fraction",
        "0,                       !- Mode0 Supply Air Mass Flow Rate Ratio",
        "Mode1_IEC,               !- Mode1 Name",
        ",                        !- Mode1 Supply Air Temperature Lookup Table Name",
        ",                        !- Mode1 Supply Air Humidity Ratio Lookup Table Name",
        ",                        !- Mode1 System Electric Power Lookup Table Name",
        ",                        !- Mode1 Supply Fan Electric Power Lookup Table Name",
        ",                        !- Mode1 External Static Pressure Lookup Table Name",
        ",                        !- Mode1 System Second Fuel Consumption Lookup Table Name",
        ",                        !- Mode1 System Third Fuel Consumption Lookup Table Name",
        ",                        !- Mode1 System Water Use Lookup Table Name",
        "-20,                     !- Mode1 Minimum Outside Air Temperature {C}",
        "100,                     !- Mode1 Maximum Outside Air Temperature {C}",
        "0,                       !- Mode1 Minimum Outside Air Humidity Ratio {kgWater/kgDryAir}",
        "0.03,                    !- Mode1 Maximum Outside Air Humidity Ratio {kgWater/kgDryAir}",
        "0,                       !- Mode1 Minimum Outside Air Relative Humidity {percent}",
        "100,                     !- Mode1 Maximum Outside Air Relative Humidity {percent}",
        "-20,                     !- Mode1 Minimum Return Air Temperature {C}",
        "100,                     !- Mode1 Maximum Return Air Temperature {C}",
        "0,                       !- Mode1 Minimum Return Air Humidity Ratio {kgWater/kgDryAir}",
        "0.03,                    !- Mode1 Maximum Return Air Humidity Ratio {kgWater/kgDryAir}",
        "0,                       !- Mode1 Minimum Return Air Relative Humidity {percent}",
        "100,                     !- Mode1 Maximum Return Air Relative Humidity {percent}",
        "1,                       !- Mode1 Minimum Outside Air Fraction",
        "1,                       !- Mode1 Maximum Outside Air Fraction",
        "0.715,                   !- Mode1 Minimum Supply Air Mass Flow Rate Ratio",
        "0.964;                   !- Mode1 Maximum Supply Air Mass Flow Rate Ratio",

        "ZoneHVAC:HybridUnitaryHVAC,",
        "Hybrid Unit 2,          !- Name",
        "ALWAYS_ON,               !- Availability Schedule Name",
        ",                        !- Availability Manager List Name",
        ",                        !- Minimum Supply Air Temperature Schedule Name",
        ",                        !- Maximum Supply Air Temperature Schedule Name",
        ",                        !- Minimum Supply Air Humidity Ratio Schedule Name",
        ",                        !- Maximum Supply Air Humidity Ratio Schedule Name",
        "AUTOMATIC,               !- Method to Choose Controlled Inputs and Part Runtime Fraction",
        "Return Air Node 2 Name,  !- Return Air Node Name",
        "Outside Air Inlet 2 Node,  !- Outside Air Node Name",
        "Zone Inlet 2 Node,    !- Supply Air Node Name",
        "Relief 2 Node,        !- Relief Node Name",
        "2.51,                    !- System Maximum Supply AirFlow Rate {m3/s}",
        ",                        !- External Static Pressure at System Maximum Supply Air Flow Rate {Pa}",
        "Yes,                     !- Fan Heat Included in Lookup Tables",
        ",                        !- Fan Heat Gain Location",
        ",                        !- Fan Heat Gain In Airstream Fraction",
        "1,                       !- Scaling Factor",
        "10,                      !- Minimum Time Between Mode Change {minutes}",
        "Electricity,             !- First fuel type",
        "NaturalGas,              !- Second fuel type",
        "DistrictCooling,         !- Third fuel type",
        ",                        !- Objective Function Minimizes",
        "SZ DSOA SPACE 2,        !- Design Specification Outdoor Air Object Name",
        "Mode0 Standby,           !- Mode0 Name",
        ",                        !- Mode0 Supply Air Temperature Lookup Table Name",
        ",                        !- Mode0 Supply Air Humidity Ratio Lookup Table Name",
        ",                        !- Mode0 System Electric Power Lookup Table Name",
        ",                        !- Mode0 Supply Fan Electric Power Lookup Table Name",
        ",                        !- Mode0 External Static Pressure Lookup Table Name",
        ",                        !- Mode0 System Second Fuel Consumption Lookup Table Name",
        ",                        !- Mode0 System Third Fuel Consumption Lookup Table Name",
        ",                        !- Mode0 System Water Use Lookup Table Name",
        "0,                       !- Mode0 Outside Air Fraction",
        "0,                       !- Mode0 Supply Air Mass Flow Rate Ratio",
        "Mode1_IEC,               !- Mode1 Name",
        ",                        !- Mode1 Supply Air Temperature Lookup Table Name",
        ",                        !- Mode1 Supply Air Humidity Ratio Lookup Table Name",
        ",                        !- Mode1 System Electric Power Lookup Table Name",
        ",                        !- Mode1 Supply Fan Electric Power Lookup Table Name",
        ",                        !- Mode1 External Static Pressure Lookup Table Name",
        ",                        !- Mode1 System Second Fuel Consumption Lookup Table Name",
        ",                        !- Mode1 System Third Fuel Consumption Lookup Table Name",
        ",                        !- Mode1 System Water Use Lookup Table Name",
        "-20,                     !- Mode1 Minimum Outside Air Temperature {C}",
        "100,                     !- Mode1 Maximum Outside Air Temperature {C}",
        "0,                       !- Mode1 Minimum Outside Air Humidity Ratio {kgWater/kgDryAir}",
        "0.03,                    !- Mode1 Maximum Outside Air Humidity Ratio {kgWater/kgDryAir}",
        "0,                       !- Mode1 Minimum Outside Air Relative Humidity {percent}",
        "100,                     !- Mode1 Maximum Outside Air Relative Humidity {percent}",
        "-20,                     !- Mode1 Minimum Return Air Temperature {C}",
        "100,                     !- Mode1 Maximum Return Air Temperature {C}",
        "0,                       !- Mode1 Minimum Return Air Humidity Ratio {kgWater/kgDryAir}",
        "0.03,                    !- Mode1 Maximum Return Air Humidity Ratio {kgWater/kgDryAir}",
        "0,                       !- Mode1 Minimum Return Air Relative Humidity {percent}",
        "100,                     !- Mode1 Maximum Return Air Relative Humidity {percent}",
        "1,                       !- Mode1 Minimum Outside Air Fraction",
        "1,                       !- Mode1 Maximum Outside Air Fraction",
        "0.715,                   !- Mode1 Minimum Supply Air Mass Flow Rate Ratio",
        "0.964;                   !- Mode1 Maximum Supply Air Mass Flow Rate Ratio",

    });

    ASSERT_TRUE(process_idf(idf_objects));
    bool ErrorsFound = false;
    GetInputZoneHybridUnitaryAirConditioners(*state, ErrorsFound);
    unsigned long expectedOperatingModesSize = 2;
    // check the number of operating modes
    EXPECT_EQ(state->dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(1).OperatingModes.size(), expectedOperatingModesSize);
    EXPECT_EQ(state->dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(2).OperatingModes.size(), expectedOperatingModesSize);
    // check if names for HybridUnitaryAC are converted to upper case
    EXPECT_EQ("HYBRID UNIT 1", state->dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(1).Name);
    EXPECT_EQ("HYBRID UNIT 2", state->dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(2).Name);
}

TEST_F(EnergyPlusFixture, Test_UnitaryHybridAirConditioner_ValidateMinimumIdfInput)
{
    std::string idf_objects = delimited_string({
        "ZoneHVAC:HybridUnitaryHVAC,",
        "MUNTERSEPX5000,          !- Name",
        "ALWAYS_ON,               !- Availability Schedule Name",
        ",                        !- Availability Manager List Name",
        ",                        !- Minimum Supply Air Temperature Schedule Name",
        ",                        !- Maximum Supply Air Temperature Schedule Name",
        ",                        !- Minimum Supply Air Humidity Ratio Schedule Name",
        ",                        !- Maximum Supply Air Humidity Ratio Schedule Name",
        "AUTOMATIC,               !- Method to Choose Controlled Inputs and Part Runtime Fraction",
        "Main Return Air Node Name,  !- Return Air Node Name",
        "Outside Air Inlet Node,  !- Outside Air Node Name",
        "Main Zone Inlet Node,    !- Supply Air Node Name",
        "Main Relief Node,        !- Relief Node Name",
        "2.51,                    !- System Maximum Supply AirFlow Rate {m3/s}",
        ",                        !- External Static Pressure at System Maximum Supply Air Flow Rate {Pa}",
        "Yes,                     !- Fan Heat Included in Lookup Tables",
        ",                        !- Fan Heat Gain Location",
        ",                        !- Fan Heat Gain In Airstream Fraction",
        "1,                       !- Scaling Factor",
        "10,                      !- Minimum Time Between Mode Change {minutes}",
        "Electricity,             !- First fuel type",
        "NaturalGas,              !- Second fuel type",
        "DistrictCooling,         !- Third fuel type",
        ",                        !- Objective Function Minimizes",
        "SZ DSOA SPACE2-1,        !- Design Specification Outdoor Air Object Name",
        "Mode0 Standby;           !- Mode0 Name",

    });

    ASSERT_TRUE(process_idf(idf_objects));
    bool ErrorsFound = false;
    GetInputZoneHybridUnitaryAirConditioners(*state, ErrorsFound);
    InitZoneHybridUnitaryAirConditioners(*state, 1, 1);
    Model *pZoneHybridUnitaryAirConditioner = &state->dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(1);
    pZoneHybridUnitaryAirConditioner->Initialize(1);
    pZoneHybridUnitaryAirConditioner->InitializeModelParams();
    constexpr unsigned long expectedOperatingModesSize = 1;
    EXPECT_EQ(pZoneHybridUnitaryAirConditioner->OperatingModes.size(), expectedOperatingModesSize);
}

TEST_F(EnergyPlusFixture, Test_UnitaryHybridAirConditioner_CalculateCurveVal)
{
    std::string const idf_objects = delimited_string({
        "ZoneHVAC:HybridUnitaryHVAC,",
        "MUNTERSEPX5000,          !- Name",
        "ALWAYS_ON,               !- Availability Schedule Name",
        ",                        !- Availability Manager List Name",
        ",                        !- Minimum Supply Air Temperature Schedule Name",
        ",                        !- Maximum Supply Air Temperature Schedule Name",
        ",                        !- Minimum Supply Air Humidity Ratio Schedule Name",
        ",                        !- Maximum Supply Air Humidity Ratio Schedule Name",
        "AUTOMATIC,               !- Method to Choose Controlled Inputs and Part Runtime Fraction",
        "Main Return Air Node Name,  !- Return Air Node Name",
        "Outside Air Inlet Node,  !- Outside Air Node Name",
        "Main Zone Inlet Node,    !- Supply Air Node Name",
        "Main Relief Node,        !- Relief Node Name",
        "2.51,                    !- System Maximum Supply AirFlow Rate {m3/s}",
        ",                        !- External Static Pressure at System Maximum Supply Air Flow Rate {Pa}",
        "Yes,                     !- Fan Heat Included in Lookup Tables",
        ",                        !- Fan Heat Gain Location",
        ",                        !- Fan Heat Gain In Airstream Fraction",
        "2.0,                     !- Scaling Factor",
        "10,                      !- Minimum Time Between Mode Change {minutes}",
        "Electricity,             !- First fuel type",
        "NaturalGas,              !- Second fuel type",
        "DistrictCooling,         !- Third fuel type",
        ",                        !- Objective Function Minimizes",
        "SZ DSOA SPACE2-1,        !- Design Specification Outdoor Air Object Name",
        "Mode0 Standby,           !- Mode0 Name",
        "Mode0_Tsa_lookup,        !- Mode0 Supply Air Temperature Lookup Table Name",
        "Mode0_Wsa_lookup,        !- Mode0 Supply Air Humidity Ratio Lookup Table Name",
        "Mode0_Power_lookup,      !- Mode0 System Electric Power Lookup Table Name",
        "Mode0_FanPower_lookup,   !- Mode0 Supply Fan Electric Power Lookup Table Name",
        ",                        !- Mode0 External Static Pressure Lookup Table Name",
        ",                        !- Mode0 System Second Fuel Consumption Lookup Table Name",
        ",                        !- Mode0 System Third Fuel Consumption Lookup Table Name",
        ",                        !- Mode0 System Water Use Lookup Table Name",
        "0,                       !- Mode0 Outside Air Fraction",
        "0;                       !- Mode0 Supply Air Mass Flow Rate Ratio",

        "Table:IndependentVariableList,",
        "Mode0_IndependentVariableList,  !- Name",
        "Mode0_Toa,                      !- Independent Variable 1 Name",
        "Mode0_Woa,                      !- Independent Variable 2 Name",
        "Mode0_Tra,                      !- Extended Field",
        "Mode0_Wra,                      !- Extended Field",
        "Mode0_Ma,                       !- Extended Field",
        "Mode0_OAF;                      !- Extended Field",

        "Table:IndependentVariable,",
        "Mode0_Toa,               !- Name",
        "Linear,                  !- Interpolation Method",
        "Constant,                !- Extrapolation Method",
        "-20,                     !- Minimum Value",
        "100,                     !- Maximum Value",
        ",                        !- Normalization Reference Value",
        "Dimensionless,           !- Unit Type",
        ",                        !- External File Name",
        ",                        !- External File Column Number",
        ",                        !- External File Starting Row Number",
        "10.0;                    !- Value 1",

        "Table:IndependentVariable,",
        "Mode0_Woa,               !- Name",
        "Linear,                  !- Interpolation Method",
        "Constant,                !- Extrapolation Method",
        "0,                       !- Minimum Value",
        "0.03,                    !- Maximum Value",
        ",                        !- Normalization Reference Value",
        "Dimensionless,           !- Unit Type",
        ",                        !- External File Name",
        ",                        !- External File Column Number",
        ",                        !- External File Starting Row Number",
        "0.005;                   !- Value 1",

        "Table:IndependentVariable,",
        "Mode0_Tra,               !- Name",
        "Linear,                  !- Interpolation Method",
        "Constant,                !- Extrapolation Method",
        "-20,                     !- Minimum Value",
        "100,                     !- Maximum Value",
        ",                        !- Normalization Reference Value",
        "Dimensionless,           !- Unit Type",
        ",                        !- External File Name",
        ",                        !- External File Column Number",
        ",                        !- External File Starting Row Number",
        "20.0;                    !- Value 1",

        "Table:IndependentVariable,",
        "Mode0_Wra,               !- Name",
        "Linear,                  !- Interpolation Method",
        "Constant,                !- Extrapolation Method",
        "0,                       !- Minimum Value",
        "0.03,                    !- Maximum Value",
        ",                        !- Normalization Reference Value",
        "Dimensionless,           !- Unit Type",
        ",                        !- External File Name",
        ",                        !- External File Column Number",
        ",                        !- External File Starting Row Number",
        "0.01;                    !- Value 1",

        "Table:IndependentVariable,",
        "Mode0_Ma,                !- Name",
        "Linear,                  !- Interpolation Method",
        "Constant,                !- Extrapolation Method",
        "0,                       !- Minimum Value",
        "1,                       !- Maximum Value",
        ",                        !- Normalization Reference Value",
        "Dimensionless,           !- Unit Type",
        ",                        !- External File Name",
        ",                        !- External File Column Number",
        ",                        !- External File Starting Row Number",
        "0.5;                     !- Value 1",

        "Table:IndependentVariable,",
        "Mode0_OAF,               !- Name",
        "Linear,                  !- Interpolation Method",
        "Constant,                !- Extrapolation Method",
        "0,                       !- Minimum Value",
        "1,                       !- Maximum Value",
        ",                        !- Normalization Reference Value",
        "Dimensionless,           !- Unit Type",
        ",                        !- External File Name",
        ",                        !- External File Column Number",
        ",                        !- External File Starting Row Number",
        "1;                       !- Value 1",

        "Table:Lookup,",
        "Mode0_Tsa_lookup,        !- Name",
        "Mode0_IndependentVariableList,  !- Independent Variable List Name",
        "DivisorOnly,             !- Normalization Method",
        ",                        !- Normalization Divisor",
        "-9999,                   !- Minimum Output",
        "9999,                    !- Maximum Output",
        "Dimensionless,           !- Output Unit Type",
        ",                        !- External File Name",
        ",                        !- External File Column Number",
        ",                        !- External File Starting Row Number",
        "5.0;                     !- Output Value 1",

        "Table:Lookup,",
        "Mode0_Wsa_lookup,        !- Name",
        "Mode0_IndependentVariableList,  !- Independent Variable List Name",
        "DivisorOnly,             !- Normalization Method",
        "3.0,                     !- Normalization Divisor",
        "-9999,                   !- Minimum Output",
        "9999,                    !- Maximum Output",
        "Dimensionless,           !- Output Unit Type",
        ",                        !- External File Name",
        ",                        !- External File Column Number",
        ",                        !- External File Starting Row Number",
        "0.005;                     !- Output Value 1",

        "Table:Lookup,",
        "Mode0_Power_lookup,      !- Name",
        "Mode0_IndependentVariableList,  !- Independent Variable List Name",
        "DivisorOnly,             !- Normalization Method",
        "3.0176,                  !- Normalization Divisor",
        "-9999,                   !- Minimum Output",
        "9999,                    !- Maximum Output",
        "Dimensionless,           !- Output Unit Type",
        ",                        !- External File Name",
        ",                        !- External File Column Number",
        ",                        !- External File Starting Row Number",
        "1000.0;                  !- Output Value 1",

        "Table:Lookup,",
        "Mode0_FanPower_lookup,   !- Name",
        "Mode0_IndependentVariableList,  !- Independent Variable List Name",
        "DivisorOnly,             !- Normalization Method",
        "3.0176,                  !- Normalization Divisor",
        "-9999,                   !- Minimum Output",
        "9999,                    !- Maximum Output",
        "Dimensionless,           !- Output Unit Type",
        ",                        !- External File Name",
        ",                        !- External File Column Number",
        ",                        !- External File Starting Row Number",
        "3.25;                    !- Output Value 1",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    CurveManager::GetCurveInput(*state);
    state->dataCurveManager->GetCurvesInputFlag = false;
    EXPECT_EQ(4, state->dataCurveManager->NumCurves);

    bool ErrorsFound(false);
    GetInputZoneHybridUnitaryAirConditioners(*state, ErrorsFound);
    GetOARequirements(*state);
    EXPECT_FALSE(ErrorsFound);

    InitZoneHybridUnitaryAirConditioners(*state, 1, 1);
    Model *pZoneHybridUnitaryAirConditioner = &state->dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(1);
    pZoneHybridUnitaryAirConditioner->Initialize(1);
    pZoneHybridUnitaryAirConditioner->InitializeModelParams();

    Real64 Toa = 10.0;
    Real64 Woa = 0.005;
    Real64 Tra = 20.0;
    Real64 Wra = 0.01;
    Real64 Ma = 0.5;
    Real64 OAF = 1.0;
    Real64 ExpectedTsa = 5.0;
    Real64 ExpectedWsa = 0.005 / 3.0;
    Real64 ExpectedPowerOutput = 1000.0 * 2.0 / 3.0176;
    Real64 ExpectedFanPowerOutput = 3.25 * 2.0 / 3.0176;

    std::vector<Real64> ExpectedResults{ExpectedTsa, ExpectedWsa, ExpectedPowerOutput, ExpectedFanPowerOutput};
    CMode mode0 = pZoneHybridUnitaryAirConditioner->OperatingModes[0];

    // The following loop is intended to loop through the valid curves in each operating mode, where i corresponds to the correct curve index in this
    // operating mode, as well as the correct expected return value in the ExpectedResults array. The values of the curves currently being tested are
    // as follow:
    //
    // TEMP_CURVE = 0;
    // W_CURVE = 1;
    // POWER_CURVE = 2;
    // SUPPLY_FAN_POWER = 3;

    for (std::size_t i = 0; i < ExpectedResults.size(); i++) {
        Real64 testCurveVal = mode0.CalculateCurveVal(*state, Toa, Woa, Tra, Wra, Ma, OAF, i);
        EXPECT_EQ(testCurveVal, ExpectedResults[i]);
    }
}

TEST_F(EnergyPlusFixture, Test_UnitaryHybridAirConditioner_ModelOperatingSettings_SolutionSpaceSearching)
{

    std::string const idf_objects = delimited_string({
        "ZoneHVAC:HybridUnitaryHVAC,",
        "MUNTERSEPX5000,          !- Name",
        "ALWAYS_ON,               !- Availability Schedule Name",
        ",                        !- Availability Manager List Name",
        ",                        !- Minimum Supply Air Temperature Schedule Name",
        ",                        !- Maximum Supply Air Temperature Schedule Name",
        ",                        !- Minimum Supply Air Humidity Ratio Schedule Name",
        "1.0,                     !- Maximum Supply Air Humidity Ratio Schedule Name",
        "AUTOMATIC,               !- Method to Choose Controlled Inputs and Part Runtime Fraction",
        "Main Return Air Node Name,  !- Return Air Node Name",
        "Outside Air Inlet Node,  !- Outside Air Node Name",
        "Main Zone Inlet Node,    !- Supply Air Node Name",
        "Main Relief Node,        !- Relief Node Name",
        "2.51,                    !- System Maximum Supply AirFlow Rate {m3/s}",
        ",                        !- External Static Pressure at System Maximum Supply Air Flow Rate {Pa}",
        "Yes,                     !- Fan Heat Included in Lookup Tables",
        ",                        !- Fan Heat Gain Location",
        ",                        !- Fan Heat Gain In Airstream Fraction",
        "2.0,                     !- Scaling Factor",
        "10,                      !- Minimum Time Between Mode Change {minutes}",
        "Electricity,             !- First fuel type",
        "NaturalGas,              !- Second fuel type",
        "DistrictCooling,         !- Third fuel type",
        ",                        !- Objective Function Minimizes",
        "SZ DSOA SPACE2-1,        !- Design Specification Outdoor Air Object Name",
        "Mode0 Standby,           !- Mode0 Name",
        "Mode0_Tsa_lookup,        !- Mode0 Supply Air Temperature Lookup Table Name",
        "Mode0_Wsa_lookup,        !- Mode0 Supply Air Humidity Ratio Lookup Table Name",
        "Mode0_Power_lookup,      !- Mode0 System Electric Power Lookup Table Name",
        "Mode0_FanPower_lookup,   !- Mode0 Supply Fan Electric Power Lookup Table Name",
        ",                        !- Mode0 External Static Pressure Lookup Table Name",
        ",                        !- Mode0 System Second Fuel Consumption Lookup Table Name",
        ",                        !- Mode0 System Third Fuel Consumption Lookup Table Name",
        ",                        !- Mode0 System Water Use Lookup Table Name",
        "0,                       !- Mode0 Outside Air Fraction",
        "0,                       !- Mode0 Supply Air Mass Flow Rate Ratio",
        "Mode1_IEC,               !- Mode1 Name",
        "Mode1_Tsa_lookup,        !- Mode1 Supply Air Temperature Lookup Table Name",
        "Mode1_Wsa_lookup,        !- Mode1 Supply Air Humidity Ratio Lookup Table Name",
        "Mode1_Power_lookup,      !- Mode1 System Electric Power Lookup Table Name",
        "Mode1_FanPower_lookup,   !- Mode1 Supply Fan Electric Power Lookup Table Name",
        ",                        !- Mode1 External Static Pressure Lookup Table Name",
        ",                        !- Mode1 System Second Fuel Consumption Lookup Table Name",
        ",                        !- Mode1 System Third Fuel Consumption Lookup Table Name",
        ",                        !- Mode1 System Water Use Lookup Table Name",
        "-20,                     !- Mode1 Minimum Outside Air Temperature {C}",
        "100,                     !- Mode1 Maximum Outside Air Temperature {C}",
        "0,                       !- Mode1 Minimum Outside Air Humidity Ratio {kgWater/kgDryAir}",
        "0.03,                    !- Mode1 Maximum Outside Air Humidity Ratio {kgWater/kgDryAir}",
        "0,                       !- Mode1 Minimum Outside Air Relative Humidity {percent}",
        "100,                     !- Mode1 Maximum Outside Air Relative Humidity {percent}",
        "-20,                     !- Mode1 Minimum Return Air Temperature {C}",
        "100,                     !- Mode1 Maximum Return Air Temperature {C}",
        "0,                       !- Mode1 Minimum Return Air Humidity Ratio {kgWater/kgDryAir}",
        "0.03,                    !- Mode1 Maximum Return Air Humidity Ratio {kgWater/kgDryAir}",
        "0,                       !- Mode1 Minimum Return Air Relative Humidity {percent}",
        "100,                     !- Mode1 Maximum Return Air Relative Humidity {percent}",
        "0,                       !- Mode1 Minimum Outside Air Fraction",
        "1,                       !- Mode1 Maximum Outside Air Fraction",
        "0.715,                   !- Mode1 Minimum Supply Air Mass Flow Rate Ratio",
        "0.964;                   !- Mode1 Maximum Supply Air Mass Flow Rate Ratio",

        "Schedule:Compact,",
        "ALWAYS_ON,               !- Name",
        "On/Off,                  !- Schedule Type Limits Name",
        "Through: 12/31,          !- Field 1",
        "For: AllDays,            !- Field 2",
        "Until: 24:00,1;          !- Field 3",

        "Table:IndependentVariableList,",
        "Mode0_IndependentVariableList,  !- Name",
        "Mode0_Toa,                      !- Independent Variable 1 Name",
        "Mode0_Woa,                      !- Independent Variable 2 Name",
        "Mode0_Tra,                      !- Extended Field",
        "Mode0_Wra,                      !- Extended Field",
        "Mode0_Ma,                       !- Extended Field",
        "Mode0_OAF;                      !- Extended Field",

        "Table:IndependentVariable,",
        "Mode0_Toa,               !- Name",
        "Linear,                  !- Interpolation Method",
        "Constant,                !- Extrapolation Method",
        "-20,                     !- Minimum Value",
        "100,                     !- Maximum Value",
        ",                        !- Normalization Reference Value",
        "Dimensionless,           !- Unit Type",
        ",                        !- External File Name",
        ",                        !- External File Column Number",
        ",                        !- External File Starting Row Number",
        "10.0;                    !- Value 1",

        "Table:IndependentVariable,",
        "Mode0_Woa,               !- Name",
        "Linear,                  !- Interpolation Method",
        "Constant,                !- Extrapolation Method",
        "0,                       !- Minimum Value",
        "0.03,                    !- Maximum Value",
        ",                        !- Normalization Reference Value",
        "Dimensionless,           !- Unit Type",
        ",                        !- External File Name",
        ",                        !- External File Column Number",
        ",                        !- External File Starting Row Number",
        "0.005;                   !- Value 1",

        "Table:IndependentVariable,",
        "Mode0_Tra,               !- Name",
        "Linear,                  !- Interpolation Method",
        "Constant,                !- Extrapolation Method",
        "-20,                     !- Minimum Value",
        "100,                     !- Maximum Value",
        ",                        !- Normalization Reference Value",
        "Dimensionless,           !- Unit Type",
        ",                        !- External File Name",
        ",                        !- External File Column Number",
        ",                        !- External File Starting Row Number",
        "20.0;                    !- Value 1",

        "Table:IndependentVariable,",
        "Mode0_Wra,               !- Name",
        "Linear,                  !- Interpolation Method",
        "Constant,                !- Extrapolation Method",
        "0,                       !- Minimum Value",
        "0.03,                    !- Maximum Value",
        ",                        !- Normalization Reference Value",
        "Dimensionless,           !- Unit Type",
        ",                        !- External File Name",
        ",                        !- External File Column Number",
        ",                        !- External File Starting Row Number",
        "0.01;                    !- Value 1",

        "Table:IndependentVariable,",
        "Mode0_Ma,                !- Name",
        "Linear,                  !- Interpolation Method",
        "Constant,                !- Extrapolation Method",
        "0,                       !- Minimum Value",
        "1,                       !- Maximum Value",
        ",                        !- Normalization Reference Value",
        "Dimensionless,           !- Unit Type",
        ",                        !- External File Name",
        ",                        !- External File Column Number",
        ",                        !- External File Starting Row Number",
        "0.5;                     !- Value 1",

        "Table:IndependentVariable,",
        "Mode0_OAF,               !- Name",
        "Linear,                  !- Interpolation Method",
        "Constant,                !- Extrapolation Method",
        "0,                       !- Minimum Value",
        "1,                       !- Maximum Value",
        ",                        !- Normalization Reference Value",
        "Dimensionless,           !- Unit Type",
        ",                        !- External File Name",
        ",                        !- External File Column Number",
        ",                        !- External File Starting Row Number",
        "1;                       !- Value 1",

        "Table:Lookup,",
        "Mode0_Tsa_lookup,        !- Name",
        "Mode0_IndependentVariableList,  !- Independent Variable List Name",
        "DivisorOnly,             !- Normalization Method",
        ",                        !- Normalization Divisor",
        "-9999,                   !- Minimum Output",
        "9999,                    !- Maximum Output",
        "Dimensionless,           !- Output Unit Type",
        ",                        !- External File Name",
        ",                        !- External File Column Number",
        ",                        !- External File Starting Row Number",
        "5.0;                     !- Output Value 1",

        "Table:Lookup,",
        "Mode0_Wsa_lookup,        !- Name",
        "Mode0_IndependentVariableList,  !- Independent Variable List Name",
        "DivisorOnly,             !- Normalization Method",
        "3.0,                     !- Normalization Divisor",
        "-9999,                   !- Minimum Output",
        "9999,                    !- Maximum Output",
        "Dimensionless,           !- Output Unit Type",
        ",                        !- External File Name",
        ",                        !- External File Column Number",
        ",                        !- External File Starting Row Number",
        "0.005;                     !- Output Value 1",

        "Table:Lookup,",
        "Mode0_Power_lookup,      !- Name",
        "Mode0_IndependentVariableList,  !- Independent Variable List Name",
        "DivisorOnly,             !- Normalization Method",
        "3.0176,                  !- Normalization Divisor",
        "-9999,                   !- Minimum Output",
        "9999,                    !- Maximum Output",
        "Dimensionless,           !- Output Unit Type",
        ",                        !- External File Name",
        ",                        !- External File Column Number",
        ",                        !- External File Starting Row Number",
        "1000.0;                  !- Output Value 1",

        "Table:Lookup,",
        "Mode0_FanPower_lookup,   !- Name",
        "Mode0_IndependentVariableList,  !- Independent Variable List Name",
        "DivisorOnly,             !- Normalization Method",
        "3.0176,                  !- Normalization Divisor",
        "-9999,                   !- Minimum Output",
        "9999,                    !- Maximum Output",
        "Dimensionless,           !- Output Unit Type",
        ",                        !- External File Name",
        ",                        !- External File Column Number",
        ",                        !- External File Starting Row Number",
        "3.25;                    !- Output Value 1",

        "Table:IndependentVariableList,",
        "Mode1_IndependentVariableList,  !- Name",
        "Mode1_Toa,                      !- Independent Variable 1 Name",
        "Mode1_Woa,                      !- Independent Variable 2 Name",
        "Mode1_Tra,                      !- Extended Field",
        "Mode1_Wra,                      !- Extended Field",
        "Mode1_Ma,                       !- Extended Field",
        "Mode1_OAF;                      !- Extended Field",

        "Table:IndependentVariable,",
        "Mode1_Toa,               !- Name",
        "Linear,                  !- Interpolation Method",
        "Constant,                !- Extrapolation Method",
        "-20,                     !- Minimum Value",
        "100,                     !- Maximum Value",
        ",                        !- Normalization Reference Value",
        "Dimensionless,           !- Unit Type",
        ",                        !- External File Name",
        ",                        !- External File Column Number",
        ",                        !- External File Starting Row Number",
        "10.0;                    !- Value 1",

        "Table:IndependentVariable,",
        "Mode1_Woa,               !- Name",
        "Linear,                  !- Interpolation Method",
        "Constant,                !- Extrapolation Method",
        "0,                       !- Minimum Value",
        "0.03,                    !- Maximum Value",
        ",                        !- Normalization Reference Value",
        "Dimensionless,           !- Unit Type",
        ",                        !- External File Name",
        ",                        !- External File Column Number",
        ",                        !- External File Starting Row Number",
        "0.005;                   !- Value 1",

        "Table:IndependentVariable,",
        "Mode1_Tra,               !- Name",
        "Linear,                  !- Interpolation Method",
        "Constant,                !- Extrapolation Method",
        "-20,                     !- Minimum Value",
        "100,                     !- Maximum Value",
        ",                        !- Normalization Reference Value",
        "Dimensionless,           !- Unit Type",
        ",                        !- External File Name",
        ",                        !- External File Column Number",
        ",                        !- External File Starting Row Number",
        "20.0;                    !- Value 1",

        "Table:IndependentVariable,",
        "Mode1_Wra,               !- Name",
        "Linear,                  !- Interpolation Method",
        "Constant,                !- Extrapolation Method",
        "0,                       !- Minimum Value",
        "0.03,                    !- Maximum Value",
        ",                        !- Normalization Reference Value",
        "Dimensionless,           !- Unit Type",
        ",                        !- External File Name",
        ",                        !- External File Column Number",
        ",                        !- External File Starting Row Number",
        "0.01;                    !- Value 1",

        "Table:IndependentVariable,",
        "Mode1_Ma,                !- Name",
        "Linear,                  !- Interpolation Method",
        "Constant,                !- Extrapolation Method",
        "0,                       !- Minimum Value",
        "1,                       !- Maximum Value",
        ",                        !- Normalization Reference Value",
        "Dimensionless,           !- Unit Type",
        ",                        !- External File Name",
        ",                        !- External File Column Number",
        ",                        !- External File Starting Row Number",
        "0.5;                     !- Value 1",

        "Table:IndependentVariable,",
        "Mode1_OAF,               !- Name",
        "Linear,                  !- Interpolation Method",
        "Constant,                !- Extrapolation Method",
        "0,                       !- Minimum Value",
        "1,                       !- Maximum Value",
        ",                        !- Normalization Reference Value",
        "Dimensionless,           !- Unit Type",
        ",                        !- External File Name",
        ",                        !- External File Column Number",
        ",                        !- External File Starting Row Number",
        "1;                       !- Value 1",

        "Table:Lookup,",
        "Mode1_Tsa_lookup,        !- Name",
        "Mode1_IndependentVariableList,  !- Independent Variable List Name",
        "DivisorOnly,             !- Normalization Method",
        ",                        !- Normalization Divisor",
        "-9999,                   !- Minimum Output",
        "9999,                    !- Maximum Output",
        "Dimensionless,           !- Output Unit Type",
        ",                        !- External File Name",
        ",                        !- External File Column Number",
        ",                        !- External File Starting Row Number",
        "15.0;                    !- Output Value 1",

        "Table:Lookup,",
        "Mode1_Wsa_lookup,        !- Name",
        "Mode1_IndependentVariableList,  !- Independent Variable List Name",
        "DivisorOnly,             !- Normalization Method",
        "3.0,                     !- Normalization Divisor",
        "-9999,                   !- Minimum Output",
        "9999,                    !- Maximum Output",
        "Dimensionless,           !- Output Unit Type",
        ",                        !- External File Name",
        ",                        !- External File Column Number",
        ",                        !- External File Starting Row Number",
        "0.005;                     !- Output Value 1",

        "Table:Lookup,",
        "Mode1_Power_lookup,      !- Name",
        "Mode1_IndependentVariableList,  !- Independent Variable List Name",
        "DivisorOnly,             !- Normalization Method",
        "3.0176,                  !- Normalization Divisor",
        "-9999,                   !- Minimum Output",
        "9999,                    !- Maximum Output",
        "Dimensionless,           !- Output Unit Type",
        ",                        !- External File Name",
        ",                        !- External File Column Number",
        ",                        !- External File Starting Row Number",
        "1000.0;                  !- Output Value 1",

        "Table:Lookup,",
        "Mode1_FanPower_lookup,   !- Name",
        "Mode1_IndependentVariableList,  !- Independent Variable List Name",
        "DivisorOnly,             !- Normalization Method",
        "3.0176,                  !- Normalization Divisor",
        "-9999,                   !- Minimum Output",
        "9999,                    !- Maximum Output",
        "Dimensionless,           !- Output Unit Type",
        ",                        !- External File Name",
        ",                        !- External File Column Number",
        ",                        !- External File Starting Row Number",
        "3.25;                    !- Output Value 1",
    });
    ASSERT_TRUE(process_idf(idf_objects));

    CurveManager::GetCurveInput(*state);
    state->dataCurveManager->GetCurvesInputFlag = false;
    EXPECT_EQ(8, state->dataCurveManager->NumCurves);

    bool ErrorsFound(false);
    GetInputZoneHybridUnitaryAirConditioners(*state, ErrorsFound);
    GetOARequirements(*state);
    EXPECT_FALSE(ErrorsFound);

    InitZoneHybridUnitaryAirConditioners(*state, 1, 2);

    Model *pZoneHybridUnitaryAirConditioner = &state->dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(1);

    Real64 DesignMinVR = 1.622720855; // Zone Hybrid Unitary HVAC Requested Outdoor Air Ventilation Mass Flow Rate
    Real64 Tra = 22.93929413;         // Zone Hybrid Unitary HVAC Return Air Temperature
    Real64 Tosa = 26.67733333;        // Zone Hybrid Unitary HVAC Outside Air Temperature
    Real64 RHra = 17.3042157;         // Zone Hybrid Unitary HVAC Return Air Relative Humidity
    Real64 RHosa = 13.1602401;        // Zone Hybrid Unitary HVAC Outside Air Relative Humidity
    Real64 Wra = PsyWFnTdbRhPb(*state, Tra, RHra / 100, 101325);
    Real64 Wosa = 0.001;
    pZoneHybridUnitaryAirConditioner->InletTemp = Tra;
    pZoneHybridUnitaryAirConditioner->InletHumRat = Wra;
    pZoneHybridUnitaryAirConditioner->InletEnthalpy = PsyHFnTdbRhPb(*state, Tra, RHra / 100, 101325, "test");
    pZoneHybridUnitaryAirConditioner->InletPressure = 101325;
    pZoneHybridUnitaryAirConditioner->InletRH = RHra / 100;
    pZoneHybridUnitaryAirConditioner->SecInletTemp = Tosa / 1000;
    pZoneHybridUnitaryAirConditioner->SecInletHumRat = Wosa;
    pZoneHybridUnitaryAirConditioner->SecInletEnthalpy = PsyHFnTdbRhPb(*state, Tosa, RHosa / 100, 101325, "test");
    pZoneHybridUnitaryAirConditioner->SecInletPressure = 101325;
    pZoneHybridUnitaryAirConditioner->SecInletRH = RHosa / 1000;

    Real64 Requestedheating = -122396.255;  // Watts (Zone Predicted Sensible Load to Heating Setpoint Heat Transfer Rate
    Real64 RequestedCooling = -58469.99445; // Watts (Zone Predicted Sensible Load to Cooling Setpoint Heat Transfer Rate
    Real64 Requested_Humidification = 0;
    Real64 Requested_Dehumidification = 0;

    pZoneHybridUnitaryAirConditioner->Initialize(1);
    pZoneHybridUnitaryAirConditioner->InitializeModelParams();
    pZoneHybridUnitaryAirConditioner->doStep(
        *state, RequestedCooling, Requestedheating, Requested_Humidification, Requested_Dehumidification, DesignMinVR);

    for (size_t i = 0; i < pZoneHybridUnitaryAirConditioner->Settings.size(); i++) {
        int MassFlowSolutionSize = pZoneHybridUnitaryAirConditioner->Settings[i].oMode.sol.MassFlowRatio.size();
        int OutdoorAirFractionSolutionSize = pZoneHybridUnitaryAirConditioner->Settings[i].oMode.sol.OutdoorAirFraction.size();

        EXPECT_EQ(6, MassFlowSolutionSize);
        EXPECT_EQ(6, OutdoorAirFractionSolutionSize);
    }
}

TEST_F(EnergyPlusFixture, Test_UnitaryHybridAirConditioner_ValidateOptionalErrors)
{
    std::string idf_objects = delimited_string({
        "ZoneHVAC:HybridUnitaryHVAC,",
        "MUNTERSEPX5000,          !- Name",
        ",                        !- Availability Schedule Name",
        ",                        !- Availability Manager List Name",
        ",                        !- Minimum Supply Air Temperature Schedule Name",
        ",                        !- Maximum Supply Air Temperature Schedule Name",
        ",                        !- Minimum Supply Air Humidity Ratio Schedule Name",
        ",                        !- Maximum Supply Air Humidity Ratio Schedule Name",
        "AUTOMATIC,               !- Method to Choose Controlled Inputs and Part Runtime Fraction",
        "Main Return Air Node Name,  !- Return Air Node Name",
        "Outside Air Inlet Node,  !- Outside Air Node Name",
        "Main Zone Inlet Node,    !- Supply Air Node Name",
        ",                        !- Relief Node Name",
        "2.51,                    !- System Maximum Supply AirFlow Rate {m3/s}",
        ",                        !- External Static Pressure at System Maximum Supply Air Flow Rate {Pa}",
        "Yes,                     !- Fan Heat Included in Lookup Tables",
        ",                        !- Fan Heat Gain Location",
        ",                        !- Fan Heat Gain In Airstream Fraction",
        ",                        !- Scaling Factor",
        ",                        !- Minimum Time Between Mode Change {minutes}",
        ",                        !- First fuel type",
        ",                        !- Second fuel type",
        ",                        !- Third fuel type",
        ",                        !- Objective Function Minimizes",
        "SZ DSOA SPACE2-1,        !- Design Specification Outdoor Air Object Name",
        "Mode0 Standby,           !- Mode0 Name",
        ",                        !- Mode0 Supply Air Temperature Lookup Table Name",
        ",                        !- Mode0 Supply Air Humidity Ratio Lookup Table Name",
        ",                        !- Mode0 System Electric Power Lookup Table Name",
        ",                        !- Mode0 Supply Fan Electric Power Lookup Table Name",
        ",                        !- Mode0 External Static Pressure Lookup Table Name",
        ",                        !- Mode0 System Second Fuel Consumption Lookup Table Name",
        ",                        !- Mode0 System Third Fuel Consumption Lookup Table Name",
        ",                        !- Mode0 System Water Use Lookup Table Name",
        "0,                       !- Mode0 Outside Air Fraction",
        "0,                       !- Mode0 Supply Air Mass Flow Rate Ratio",
        "Mode1_IEC,               !- Mode1 Name",
        ",                        !- Mode1 Supply Air Temperature Lookup Table Name",
        ",                        !- Mode1 Supply Air Humidity Ratio Lookup Table Name",
        ",                        !- Mode1 System Electric Power Lookup Table Name",
        ",                        !- Mode1 Supply Fan Electric Power Lookup Table Name",
        ",                        !- Mode1 External Static Pressure Lookup Table Name",
        ",                        !- Mode1 System Second Fuel Consumption Lookup Table Name",
        ",                        !- Mode1 System Third Fuel Consumption Lookup Table Name",
        ",                        !- Mode1 System Water Use Lookup Table Name",
        "-20,                     !- Mode1 Minimum Outside Air Temperature {C}",
        "100,                     !- Mode1 Maximum Outside Air Temperature {C}",
        "0,                       !- Mode1 Minimum Outside Air Humidity Ratio {kgWater/kgDryAir}",
        "0.03,                    !- Mode1 Maximum Outside Air Humidity Ratio {kgWater/kgDryAir}",
        "0,                       !- Mode1 Minimum Outside Air Relative Humidity {percent}",
        "100,                     !- Mode1 Maximum Outside Air Relative Humidity {percent}",
        "-20,                     !- Mode1 Minimum Return Air Temperature {C}",
        "100,                     !- Mode1 Maximum Return Air Temperature {C}",
        "0,                       !- Mode1 Minimum Return Air Humidity Ratio {kgWater/kgDryAir}",
        "0.03,                    !- Mode1 Maximum Return Air Humidity Ratio {kgWater/kgDryAir}",
        "0,                       !- Mode1 Minimum Return Air Relative Humidity {percent}",
        "100,                     !- Mode1 Maximum Return Air Relative Humidity {percent}",
        "1,                       !- Mode1 Minimum Outside Air Fraction",
        "1,                       !- Mode1 Maximum Outside Air Fraction",
        "0.715,                   !- Mode1 Minimum Supply Air Mass Flow Rate Ratio",
        "0.964;                   !- Mode1 Maximum Supply Air Mass Flow Rate Ratio",

        "Schedule:Compact,",
        "MinSupplyT,              !- Name",
        "Temperature,             !- Schedule Type Limits Name",
        "Through: 12/31,          !- Field 1",
        "For: AllDays,            !- Field 2",
        "Until: 24:00,            !- Field 3",
        "2;                       !- Field 4",

        "Schedule:Compact,",
        "MaxSupplyT,              !- Name",
        "Temperature,             !- Schedule Type Limits Name",
        "Through: 12/31,          !- Field 1",
        "For: AllDays,            !- Field 2",
        "Until: 24:00,            !- Field 3",
        "50;                      !- Field 4",

        "Schedule:Compact,",
        "MinSupplyHR,             !- Name",
        "Humidity,             !- Schedule Type Limits Name",
        "Through: 12/31,          !- Field 1",
        "For: AllDays,            !- Field 2",
        "Until: 24:00,            !- Field 3",
        "0;                       !- Field 4",

        "Schedule:Compact,",
        "MaxSupplyHR,             !- Name",
        "Humidity,             !- Schedule Type Limits Name",
        "Through: 12/31,          !- Field 1",
        "For: AllDays,            !- Field 2",
        "Until: 24:00,            !- Field 3",
        "0.03;                    !- Field 4",

        "ScheduleTypeLimits,",
        "Temperature,             !- Name",
        "-100,                    !- Lower Limit Value",
        "100,                     !- Upper Limit Value",
        "Continuous;              !- Numeric Type",

        "ScheduleTypeLimits,",
        "Humidity,                !- Name",
        "0.0,                     !- Lower Limit Value",
        "100.0,                   !- Upper Limit Value",
        "Continuous;              !- Numeric Type",
    });

    ASSERT_TRUE(process_idf(idf_objects));
    bool ErrorsFound = false;
    GetInputZoneHybridUnitaryAirConditioners(*state, ErrorsFound);
    // Design Specification Outdoor Air Object Name 'SZ DSOA SPACE2-1' is not defined in this model, thus an error is thrown
    std::string const error_string =
        delimited_string({"   ** Severe  ** GetInputZoneHybridUnitaryAirConditioners: ZoneHVAC:HybridUnitaryHVAC = MUNTERSEPX5000 invalid data",
                          "   **   ~~~   ** Invalid-not found Design Specification Outdoor Air Object Name=\"SZ DSOA SPACE2-1\"."});
    EXPECT_TRUE(compare_err_stream(error_string, true));
}

TEST_F(EnergyPlusFixture, Test_UnitaryHybridAirConditioner_RuntimeFraction_Initialization)
{
    std::vector<std::string> snippet =
        getAllLinesInFile2(configured_source_directory() / "tst/EnergyPlus/unit/Resources/UnitaryHybridUnitTest_DOSA.idf");
    std::string string = delimited_string(snippet);
    ASSERT_TRUE(process_idf(string));
    // setup environment
    bool ErrorsFound(false);
    GetZoneData(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    // Initialize schedule values
    state->dataGlobal->TimeStep = 1;
    state->dataHVACGlobal->TimeStepSys = 1;
    state->dataGlobal->NumOfTimeStepInHour = 1;
    state->dataGlobal->MinutesPerTimeStep = 60;
    state->dataEnvrn->Month = 1;
    state->dataEnvrn->DayOfMonth = 21;
    state->dataGlobal->HourOfDay = 1;
    state->dataEnvrn->DSTIndicator = 0;
    state->dataEnvrn->DayOfWeek = 2;
    state->dataEnvrn->HolidayIndex = 0;
    state->dataGlobal->WarmupFlag = false;
    state->dataEnvrn->DayOfYear_Schedule = General::OrdinalDay(state->dataEnvrn->Month, state->dataEnvrn->DayOfMonth, 1);
    ScheduleManager::UpdateScheduleValues(*state);
    // Initialize zone areas and volumes - too many other things need to be set up to do these in the normal routines
    state->dataHeatBal->Zone(1).FloorArea = 232.26;
    state->dataEnvrn->StdRhoAir = 1.225;
    state->dataEnvrn->OutBaroPress = 101325;
    state->dataHeatBal->ZoneIntGain.allocate(1);

    SizingManager::GetOARequirements(*state);
    GetOAControllerInputs(*state);
    using DataZoneEquipment::CalcDesignSpecificationOutdoorAir;

    // Setup performance tables
    using namespace EnergyPlus::DataEnvironment;
    // process schedules
    ProcessScheduleInput(*state); // read schedules
    UpdateScheduleValues(*state);
    // Get Unitary system
    GetInputZoneHybridUnitaryAirConditioners(*state, ErrorsFound);
    // All to get OA requirements
    GetOARequirements(*state);

    EXPECT_FALSE(ErrorsFound);
    // Initialize unit
    InitZoneHybridUnitaryAirConditioners(*state, 1, 1);
    Model *pZoneHybridUnitaryAirConditioner = &state->dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(1);
    // setup local variables for model inputs
    Real64 Tosa, Tra, Wra, Wosa, RHosa, RHra, DesignMinVR, Requestedheating, RequestedCooling, Requested_Humidification, Requested_Dehumidification;
    RHosa = 0;
    std::string TimeDate;
    Real64 MsaRatio, OSAF;
    MsaRatio = OSAF = 1;

    Requestedheating = RequestedCooling = Requested_Humidification = Requested_Dehumidification = 0;

    DesignMinVR = 1.622720855; // Zone Hybrid Unitary HVAC Requested Outdoor Air Ventilation Mass Flow Rate
    Tra = 22.93929413;         // Zone Hybrid Unitary HVAC Return Air Temperature
    Tosa = 26.67733333;        // Zone Hybrid Unitary HVAC Outside Air Temperature
    RHra = 17.3042157;         // Zone Hybrid Unitary HVAC Return Air Relative Humidity
    RHosa = 13.1602401;        // Zone Hybrid Unitary HVAC Outside Air Relative Humidity
    Wra = PsyWFnTdbRhPb(*state, Tra, RHra / 100, 101325);
    Wosa = PsyWFnTdbRhPb(*state, Tosa, RHosa / 100, 101325);
    pZoneHybridUnitaryAirConditioner->InletTemp = Tra;
    pZoneHybridUnitaryAirConditioner->InletHumRat = Wra;
    pZoneHybridUnitaryAirConditioner->InletEnthalpy = PsyHFnTdbRhPb(*state, Tra, RHra / 100, 101325, "test");
    pZoneHybridUnitaryAirConditioner->InletPressure = 101325;
    pZoneHybridUnitaryAirConditioner->InletRH = RHra / 100;
    pZoneHybridUnitaryAirConditioner->SecInletTemp = Tosa;
    pZoneHybridUnitaryAirConditioner->SecInletHumRat = Wosa;
    pZoneHybridUnitaryAirConditioner->SecInletEnthalpy = PsyHFnTdbRhPb(*state, Tosa, RHosa / 100, 101325, "test");
    pZoneHybridUnitaryAirConditioner->SecInletPressure = 101325;
    pZoneHybridUnitaryAirConditioner->SecInletRH = RHosa / 100;

    // Scenario 1: Low Cooling

    Requestedheating = -64358.68966; //-
    RequestedCooling = -633.6613591; // W
    pZoneHybridUnitaryAirConditioner->Initialize(1);
    pZoneHybridUnitaryAirConditioner->InitializeModelParams();
    pZoneHybridUnitaryAirConditioner->doStep(
        *state, RequestedCooling, Requestedheating, Requested_Humidification, Requested_Dehumidification, DesignMinVR);

    // output results
    Real64 Setting0Mode = pZoneHybridUnitaryAirConditioner->CurrentOperatingSettings[0].Mode;
    Real64 Setting0RuntimeFraction = pZoneHybridUnitaryAirConditioner->CurrentOperatingSettings[0].Runtime_Fraction;
    Real64 Setting1Mode = pZoneHybridUnitaryAirConditioner->CurrentOperatingSettings[1].Mode;
    Real64 Setting1RuntimeFraction = pZoneHybridUnitaryAirConditioner->CurrentOperatingSettings[1].Runtime_Fraction;

    // checks
    EXPECT_EQ(Setting0Mode, 1);                         // IEC Mode
    EXPECT_NEAR(Setting0RuntimeFraction, 0.547, 0.001); // IEC RTF
    EXPECT_EQ(Setting1Mode, 0);                         // Standby Mode
    EXPECT_NEAR(Setting1RuntimeFraction, 0.453, 0.001); // Standby Mode

    // Scenario 2: Outside of env conditions. should go to standby and have standby energy
    Requestedheating = -55795.8058;
    RequestedCooling = 8171.47128;
    pZoneHybridUnitaryAirConditioner->Initialize(1);
    pZoneHybridUnitaryAirConditioner->InitializeModelParams();
    pZoneHybridUnitaryAirConditioner->SecInletTemp = 150;
    pZoneHybridUnitaryAirConditioner->SecInletHumRat = 0;
    pZoneHybridUnitaryAirConditioner->doStep(*state, RequestedCooling, Requestedheating, Requested_Humidification, Requested_Dehumidification, 0);

    // output results
    Setting0Mode = pZoneHybridUnitaryAirConditioner->CurrentOperatingSettings[0].Mode;
    Setting0RuntimeFraction = pZoneHybridUnitaryAirConditioner->CurrentOperatingSettings[0].Runtime_Fraction;
    Setting1Mode = pZoneHybridUnitaryAirConditioner->CurrentOperatingSettings[1].Mode;
    Setting1RuntimeFraction = pZoneHybridUnitaryAirConditioner->CurrentOperatingSettings[1].Runtime_Fraction;

    // checks
    EXPECT_EQ(Setting0Mode, 0);            // Standby Mode
    EXPECT_EQ(Setting0RuntimeFraction, 1); // Standby RTF
    EXPECT_EQ(Setting1Mode, 0);            // Standby Mode
    EXPECT_EQ(Setting1RuntimeFraction, 0); // Standby RTF
}

} // namespace EnergyPlus
