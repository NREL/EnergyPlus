// EnergyPlus, Copyright (c) 1996-2020, The Board of Trustees of the University of Illinois,
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

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/ConfiguredFunctions.hh>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/DataAirLoop.hh>
#include <EnergyPlus/DataAirSystems.hh>
#include <EnergyPlus/DataContaminantBalance.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobalConstants.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataZoneControls.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/EvaporativeCoolers.hh>
#include <EnergyPlus/FileSystem.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/Humidifiers.hh>
#include <EnergyPlus/HybridEvapCoolingModel.hh>
#include <EnergyPlus/HybridUnitaryAirConditioners.hh>
#include <EnergyPlus/MixedAir.hh>
#include <EnergyPlus/OutputFiles.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ReportSizingManager.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SizingManager.hh>
#include <EnergyPlus/SystemReports.hh>
#include <fstream>

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
using namespace EnergyPlus::ReportSizingManager;
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
std::vector<std::string> getAllLinesInFile2(std::string filePath)
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
    std::vector<std::string> snippet = getAllLinesInFile2(configured_source_directory() + "/tst/EnergyPlus/unit/UnitaryHybridUnitTest_DOSA.idf");
    std::string string = delimited_string(snippet);
    ASSERT_TRUE(process_idf(string));
    // setup environment
    bool ErrorsFound(false);
    GetZoneData(ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    // Initialize schedule values
    DataGlobals::TimeStep = 1;
    DataHVACGlobals::TimeStepSys = 1;
    DataGlobals::NumOfTimeStepInHour = 1;
    DataGlobals::MinutesPerTimeStep = 60;
    DataEnvironment::Month = 1;
    DataEnvironment::DayOfMonth = 21;
    DataGlobals::HourOfDay = 1;
    DataEnvironment::DSTIndicator = 0;
    DataEnvironment::DayOfWeek = 2;
    DataEnvironment::HolidayIndex = 0;
    DataGlobals::WarmupFlag = false;
    DataEnvironment::DayOfYear_Schedule = General::OrdinalDay(Month, DayOfMonth, 1);
    ScheduleManager::UpdateScheduleValues();
    // Initialize zone areas and volumes - too many other things need to be set up to do these in the normal routines
    DataHeatBalance::Zone(1).FloorArea = 232.26;
    DataEnvironment::StdRhoAir = 1.225;
    DataEnvironment::OutBaroPress = 101325;
    DataHeatBalance::ZoneIntGain.allocate(1);

    SizingManager::GetOARequirements();
    GetOAControllerInputs(state, outputFiles());
    using DataZoneEquipment::CalcDesignSpecificationOutdoorAir;

    // Setup performance tables
    using namespace EnergyPlus::DataEnvironment;
    // process schedules
    ProcessScheduleInput(outputFiles()); // read schedules
    UpdateScheduleValues();
    // Get Unitary system
    GetInputZoneHybridUnitaryAirConditioners(ErrorsFound);
    // All to get OA requirements
    GetOARequirements();

    EXPECT_FALSE(ErrorsFound);
    // Initialize unit
    InitZoneHybridUnitaryAirConditioners(1, 1);
    Model *pZoneHybridUnitaryAirConditioner = &HybridUnitaryAirConditioners::ZoneHybridUnitaryAirConditioner(1);
    // setup local variables for model inputs
    Real64 Tosa, Tra, Wra, Wosa, RHosa, RHra, DesignMinVR, Requestedheating, RequestedCooling, Requested_Humidification, Requested_Dehumidification;
    RHosa = 0;
    std::string TimeDate;
    int modenumber = 0;
    Real64 MsaRatio, OSAF;
    MsaRatio = OSAF = 1;

    Requestedheating = RequestedCooling = Requested_Humidification = Requested_Dehumidification = 0;

    DesignMinVR = 1.622720855;       // Zone Hybrid Unitary HVAC Requested Outdoor Air Ventilation Mass Flow Rate
    Tra = 22.93929413;               // Zone Hybrid Unitary HVAC Return Air Temperature
    Tosa = 26.67733333;              // Zone Hybrid Unitary HVAC Outside Air Temperature
    RHra = 17.3042157;               // Zone Hybrid Unitary HVAC Return Air Relative Humidity
    RHosa = 13.1602401;              // Zone Hybrid Unitary HVAC Outside Air Relative Humidity
    Wra = PsyWFnTdbRhPb(Tra, RHra / 100, 101325);
    Wosa = PsyWFnTdbRhPb(Tosa, RHosa / 100, 101325);
    pZoneHybridUnitaryAirConditioner->InletTemp = Tra;
    pZoneHybridUnitaryAirConditioner->InletHumRat = Wra;
    pZoneHybridUnitaryAirConditioner->InletEnthalpy = PsyHFnTdbRhPb(Tra, RHra / 100, 101325, "test");
    pZoneHybridUnitaryAirConditioner->InletPressure = 101325;
    pZoneHybridUnitaryAirConditioner->InletRH = RHra / 100;
    pZoneHybridUnitaryAirConditioner->SecInletTemp = Tosa;
    pZoneHybridUnitaryAirConditioner->SecInletHumRat = Wosa;
    pZoneHybridUnitaryAirConditioner->SecInletEnthalpy = PsyHFnTdbRhPb(Tosa, RHosa / 100, 101325, "test");
    pZoneHybridUnitaryAirConditioner->SecInletPressure = 101325;
    pZoneHybridUnitaryAirConditioner->SecInletRH = RHosa / 100;

    // Scenario 1: Hi Cooling

    Requestedheating = -122396.255;  // Watts (Zone Predicted Sensible Load to Heating Setpoint Heat Transfer Rate
    RequestedCooling = -58469.99445; // Watts (Zone Predicted Sensible Load to Cooling Setpoint Heat Transfer Rate
    pZoneHybridUnitaryAirConditioner->Initialize(1);
    pZoneHybridUnitaryAirConditioner->InitializeModelParams();
    pZoneHybridUnitaryAirConditioner->doStep(RequestedCooling, Requestedheating, Requested_Humidification, Requested_Dehumidification, DesignMinVR);

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
    pZoneHybridUnitaryAirConditioner->doStep(RequestedCooling, Requestedheating, Requested_Humidification, Requested_Dehumidification, DesignMinVR);

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
    pZoneHybridUnitaryAirConditioner->doStep(RequestedCooling, Requestedheating, Requested_Humidification, Requested_Dehumidification, DesignMinVR);

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
    pZoneHybridUnitaryAirConditioner->doStep(RequestedCooling, Requestedheating, Requested_Humidification, Requested_Dehumidification, DesignMinVR);

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
    pZoneHybridUnitaryAirConditioner->doStep(RequestedCooling, Requestedheating, Requested_Humidification, Requested_Dehumidification, DesignMinVR);

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
    pZoneHybridUnitaryAirConditioner->FanHeatGainLocation = "SupplyAirStream";
    pZoneHybridUnitaryAirConditioner->FanHeatInAirFrac = 1.0;
    pZoneHybridUnitaryAirConditioner->Initialize(1);
    pZoneHybridUnitaryAirConditioner->InitializeModelParams();
    pZoneHybridUnitaryAirConditioner->SecInletTemp = Tosa;
    pZoneHybridUnitaryAirConditioner->SecInletHumRat = Wosa;
    pZoneHybridUnitaryAirConditioner->doStep(RequestedCooling, Requestedheating, Requested_Humidification, Requested_Dehumidification, DesignMinVR);

    // output results

    Tsa = pZoneHybridUnitaryAirConditioner->OutletTemp;
    // check that supply air temperature increases due to fan heat
    EXPECT_NEAR(Tsa, Tosa + 0.36, 0.1);

    // check fan heat calculation in mixed air stream if not included in lookup tables
    pZoneHybridUnitaryAirConditioner->FanHeatGain = true;
    pZoneHybridUnitaryAirConditioner->FanHeatGainLocation = "MixedAirStream";
    pZoneHybridUnitaryAirConditioner->FanHeatInAirFrac = 1.0;
    pZoneHybridUnitaryAirConditioner->Initialize(1);
    pZoneHybridUnitaryAirConditioner->InitializeModelParams();
    pZoneHybridUnitaryAirConditioner->SecInletTemp = Tosa;
    pZoneHybridUnitaryAirConditioner->SecInletHumRat = Wosa;
    pZoneHybridUnitaryAirConditioner->doStep(RequestedCooling, Requestedheating, Requested_Humidification, Requested_Dehumidification, DesignMinVR);

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
    pZoneHybridUnitaryAirConditioner->doStep(RequestedCooling, Requestedheating, Requested_Humidification, Requested_Dehumidification, DesignMinVR);

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
    NumOfZones = 1;
    ZoneSysEnergyDemand.allocate(NumOfZones);
    DeadBandOrSetback.allocate(NumOfZones);

    HeatBalanceManager::GetZoneData(ErrorsFound); // read zone data
    EXPECT_FALSE(ErrorsFound);                    // expect no errors
    DataZoneEquipment::GetZoneEquipmentData(state);    // read zone equipment    SystemReports::ReportMaxVentilationLoads();
    DataZoneEquipment::ZoneEquipInputsFilled = true;
    ZoneOAMassFlow.allocate(NumOfZones);
    ZoneOAMass.allocate(NumOfZones);
    ZoneOAVolFlowStdRho.allocate(NumOfZones);
    ZoneOAVolFlowCrntRho.allocate(NumOfZones);
    ZoneOAVolStdRho.allocate(NumOfZones);
    ZoneOAVolCrntRho.allocate(NumOfZones);
    ZoneMechACH.allocate(NumOfZones);
    MAT.allocate(NumOfZones);
    ZoneAirHumRatAvg.allocate(NumOfZones);
    MaxHeatingLoadMetByVent.allocate(NumOfZones);
    MaxOverheatingByVent.allocate(NumOfZones);
    MaxCoolingLoadMetByVent.allocate(NumOfZones);
    MaxOvercoolingByVent.allocate(NumOfZones);
    ZoneSysEnergyDemand(1).TotalOutputRequired = 58469.99445;
    DeadBandOrSetback(1) = false;
    ZoneEquipList(ZoneEquipConfig(1).EquipListIndex).EquipIndex(1) = 1;
    CreateEnergyReportStructure();

    SizingManager::GetOARequirements();
    using DataZoneEquipment::CalcDesignSpecificationOutdoorAir;

    // Setup performance tables
    using namespace EnergyPlus::DataEnvironment;
    // process schedules
    ProcessScheduleInput(OutputFiles::getSingleton()); // read schedules
    UpdateScheduleValues();
    // Get Unitary system
    GetInputZoneHybridUnitaryAirConditioners(ErrorsFound);
    // All to get OA requirements
    GetOARequirements();

    Requestedheating = -122396.255;  // Watts (Zone Predicted Sensible Load to Heating Setpoint Heat Transfer Rate
    RequestedCooling = -58469.99445; // Watts (Zone Predicted Sensible Load to Cooling Setpoint Heat Transfer Rate
    pZoneHybridUnitaryAirConditioner->Initialize(1);
    pZoneHybridUnitaryAirConditioner->InitializeModelParams();
    pZoneHybridUnitaryAirConditioner->InletTemp = Tra;
    pZoneHybridUnitaryAirConditioner->SecInletTemp = Tosa;
    pZoneHybridUnitaryAirConditioner->SecInletMassFlowRate = DesignMinVR;
    pZoneHybridUnitaryAirConditioner->doStep(RequestedCooling, Requestedheating, Requested_Humidification, Requested_Dehumidification, DesignMinVR);
    ReportZoneHybridUnitaryAirConditioners(1);

    SystemReports::ReportMaxVentilationLoads(state);
    // output results
    Real64 zone_oa_mass_flow = ZoneOAMassFlow(1); // OA flow reported to the zone from the unitary hybrid system

    // checks
    EXPECT_EQ(zone_oa_mass_flow, DesignMinVR); // reported zone OA flow matches unitary hybrid OA flow

    // Scenario 8: Check output meters and report
    int NumFound;

    std::string TypeOfComp = "ZoneHVAC:HybridUnitaryHVAC";
    std::string NameOfComp = pZoneHybridUnitaryAirConditioner->Name;
    int NumVariables = GetNumMeteredVariables(TypeOfComp, NameOfComp);
    Array1D_int VarIndexes(NumVariables);                     // Variable Numbers
    Array1D_int VarTypes(NumVariables);                       // Variable Types (1=integer, 2=real, 3=meter)
    Array1D<OutputProcessor::TimeStepType> IndexTypes(
            NumVariables);                     // Variable Index Types (1=Zone,2=HVAC)
    Array1D<OutputProcessor::Unit> unitsForVar(NumVariables); // units from enum for each variable
    Array1D_int ResourceTypes(NumVariables);                  // ResourceTypes for each variable
    Array1D_string EndUses(NumVariables);                     // EndUses for each variable
    Array1D_string Groups(NumVariables);                      // Groups for each variable
    Array1D_string Names(NumVariables);                       // Variable Names for each variable

    GetMeteredVariables(TypeOfComp, NameOfComp, VarIndexes, VarTypes, IndexTypes, unitsForVar, ResourceTypes,
                        EndUses, Groups, Names, NumFound);

    // output results
    Real64 MaxFlow = pZoneHybridUnitaryAirConditioner->ScaledSystemMaximumSupplyAirVolumeFlowRate;

    // Check the meters associated with the ZoneHVAC:HybridUnitaryHVAC outputs
    EXPECT_EQ(21, NumFound);
    EXPECT_EQ(ResourceTypes(1), 1010); // ENERGYTRANSFER - Cooling
    EXPECT_EQ(EndUses(1), "COOLINGCOILS");
    EXPECT_EQ(Groups(1), "HVAC");
    EXPECT_EQ(ResourceTypes(2), 1010); // ENERGYTRANSFER - Heating
    EXPECT_EQ(EndUses(2), "HEATINGCOILS");
    EXPECT_EQ(Groups(2), "HVAC");
    EXPECT_EQ(ResourceTypes(3), 1001); // ELECTRIC - Cooling Energy
    EXPECT_EQ(EndUses(3), "COOLING");
    EXPECT_EQ(Groups(3), "HVAC");
    EXPECT_EQ(ResourceTypes(4), 1001); // ELECTRIC - Fan Energy
    EXPECT_EQ(EndUses(4), "FANS");
    EXPECT_EQ(Groups(4), "HVAC");
    EXPECT_EQ(ResourceTypes(5), 1002); // NATURALGAS - Secondary Fuel Type - specified in UnitaryHybridUnitTest_DOSA.idf
    EXPECT_EQ(EndUses(5), "COOLING");
    EXPECT_EQ(Groups(5), "HVAC");
    EXPECT_EQ(ResourceTypes(6), 1012); // DISTRICTCOOLING - Third Fuel Type - specified in UnitaryHybridUnitTest_DOSA.idf
    EXPECT_EQ(EndUses(6), "COOLING");
    EXPECT_EQ(Groups(6), "HVAC");
    EXPECT_EQ(ResourceTypes(7), 1009); // WATER - Cooling Water Use
    EXPECT_EQ(EndUses(7), "COOLING");
    EXPECT_EQ(Groups(7), "HVAC");

    // Check that unit is included in Component Sizing Summary Report
    EXPECT_EQ("ZoneHVAC:HybridUnitaryHVAC", OutputReportPredefined::CompSizeTableEntry(1).typeField);
    EXPECT_EQ("MUNTERSEPX5000", OutputReportPredefined::CompSizeTableEntry(1).nameField);
    EXPECT_EQ("Scaled Maximum Supply Air Volume Flow Rate [m3/s]", OutputReportPredefined::CompSizeTableEntry(1).description);
    EXPECT_EQ(MaxFlow, OutputReportPredefined::CompSizeTableEntry(1).valField);

}


TEST_F(EnergyPlusFixture, Test_UnitaryHybridAirConditioner_ValidateFieldsParsing)
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
                                                         "2,                       !- Number of Operating Modes",
                                                         "10,                      !- Minimum Time Between Mode Change {minutes}",
                                                         "Electricity,             !- First fuel type",
                                                         "NaturalGas,              !- Second fuel type",
                                                         "DistrictCooling,         !- Third fuel type",
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

                                                     });

    ASSERT_TRUE(process_idf(idf_objects));
    bool ErrorsFound = false;
    GetInputZoneHybridUnitaryAirConditioners(ErrorsFound);
    InitZoneHybridUnitaryAirConditioners(1, 1);
    Model *pZoneHybridUnitaryAirConditioner = &HybridUnitaryAirConditioners::ZoneHybridUnitaryAirConditioner(1);
    pZoneHybridUnitaryAirConditioner->Initialize(1);
    pZoneHybridUnitaryAirConditioner->InitializeModelParams();
    unsigned long expectedOperatingModesSize = 2;
    EXPECT_EQ(pZoneHybridUnitaryAirConditioner->OperatingModes.size(), expectedOperatingModesSize);
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
                                       "1,                       !- Number of Operating Modes",
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
    GetInputZoneHybridUnitaryAirConditioners(ErrorsFound);
    InitZoneHybridUnitaryAirConditioners(1, 1);
    Model *pZoneHybridUnitaryAirConditioner = &HybridUnitaryAirConditioners::ZoneHybridUnitaryAirConditioner(1);
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
                                                         "1,                       !- Number of Operating Modes",
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

    CurveManager::GetCurveInput();
    CurveManager::GetCurvesInputFlag = false;
    EXPECT_EQ(4, CurveManager::NumCurves);

    bool ErrorsFound(false);
    GetInputZoneHybridUnitaryAirConditioners(ErrorsFound);
    GetOARequirements();
    EXPECT_FALSE(ErrorsFound);

    InitZoneHybridUnitaryAirConditioners(1, 1);
    Model *pZoneHybridUnitaryAirConditioner = &HybridUnitaryAirConditioners::ZoneHybridUnitaryAirConditioner(1);
    pZoneHybridUnitaryAirConditioner->Initialize(1);
    pZoneHybridUnitaryAirConditioner->InitializeModelParams();

    Real64 Toa = 10.0;
    Real64 Woa = 0.005;
    Real64 Tra = 20.0;
    Real64 Wra = 0.01;
    Real64 Ma = 0.5;
    Real64 OAF = 1.0;
    Real64 ExpectedTsa = 5.0;
    Real64 ExpectedWsa = 0.005/3.0;
    Real64 ExpectedPowerOutput = 1000.0*2.0/3.0176;
    Real64 ExpectedFanPowerOutput = 3.25*2.0/3.0176;

    std::vector<Real64> ExpectedResults{ExpectedTsa,ExpectedWsa,ExpectedPowerOutput,ExpectedFanPowerOutput };
    CMode mode0 = pZoneHybridUnitaryAirConditioner->OperatingModes[0];

    // The following loop is intended to loop through the valid curves in each operating mode, where i corresponds to the correct curve index in this operating mode, as well as the correct expected
    // return value in the ExpectedResults array. The values of the curves currently being tested are as follow:
    //
    // TEMP_CURVE = 0;
    // W_CURVE = 1;
    // POWER_CURVE = 2;
    // SUPPLY_FAN_POWER = 3;

    for (std::size_t i=0; i<ExpectedResults.size(); i++){
        Real64 testCurveVal = mode0.CalculateCurveVal(Toa, Woa, Tra, Wra, Ma, OAF, i);
        EXPECT_EQ(testCurveVal, ExpectedResults[i]);
    }
}

} // namespace EnergyPlus
