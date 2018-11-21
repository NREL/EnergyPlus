// EnergyPlus, Copyright (c) 1996-2018, The Board of Trustees of the University of Illinois,
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
#include <CurveManager.hh>
#include <HybridEvapCoolingModel.hh>

#include <EnergyPlus/General.hh>

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <ConfiguredFunctions.hh>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/DataAirLoop.hh>
#include <EnergyPlus/DataAirSystems.hh>
#include <EnergyPlus/DataContaminantBalance.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobalConstants.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataZoneControls.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/EvaporativeCoolers.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/Humidifiers.hh>
#include <EnergyPlus/HybridUnitaryAirConditioners.hh>
#include <EnergyPlus/MixedAir.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SizingManager.hh>
#include <FileSystem.hh>
#include <fstream>
using namespace EnergyPlus::MixedAir;
using namespace EnergyPlus::DataContaminantBalance;
using namespace EnergyPlus::DataAirLoop;
using namespace EnergyPlus::DataAirSystems;
using namespace EnergyPlus::DataSizing;
using namespace EnergyPlus::DataHeatBalance;
using namespace EnergyPlus::ScheduleManager;
using namespace EnergyPlus::DataEnvironment;
using namespace EnergyPlus::DataZoneEquipment;
using namespace EnergyPlus::DataLoopNode;
using namespace EnergyPlus::DataZoneEnergyDemands;
using namespace EnergyPlus::DataZoneControls;
using namespace EnergyPlus::HeatBalanceManager;
using namespace EnergyPlus::Humidifiers;
using namespace EnergyPlus::SizingManager;

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
    DataHeatBalance::ZoneIntGain.allocate(1);

    SizingManager::GetOARequirements();
    GetOAControllerInputs();
    using DataZoneEquipment::CalcDesignSpecificationOutdoorAir;

    // Setup performnace tables
    using namespace EnergyPlus::DataEnvironment;
    // process schedules
    ProcessScheduleInput(); // read schedules
    UpdateScheduleValues();
    // Get Unitary system
    GetInputZoneHybridUnitaryAirConditioners(ErrorsFound);
    // All to get OA requiremetns
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

    // Scenario 1: Hi Cooling

    DesignMinVR = 1.622720855;       // Zone Hybrid Unitary HVAC Requested Outdoor Air Ventilation Mass Flow Rate
    Tra = 22.93929413;               // Zone Hybrid Unitary HVAC Return Air Temperature
    Tosa = 26.67733333;              // Zone Hybrid Unitary HVAC Outside Air Temperature
    RHra = 17.3042157;               // Zone Hybrid Unitary HVAC Return Air Relative Humidity
    RHosa = 13.1602401;              // Zone Hybrid Unitary HVAC Outside Air Relative Humidity
    Requestedheating = -122396.255;  // Watts (Zone Predicted Sensible Load to Heating Setpoint Heat Transfer Rate
    RequestedCooling = -58469.99445; // Watts (Zone Predicted Sensible Load to Cooling Setpoint Heat Transfer Rate
    // Equivalent to a Zone Predicted Sensible Load to Setpoint Heat Transfer Rate [W] of -58470 w.
    // A positive value indicates a heating load, a negative value indicates a cooling load.
    Requested_Humidification = Requested_Dehumidification = 0;
    Wra = PsyWFnTdbRhPb(Tra, RHra / 100, 101.325);
    Wosa = PsyWFnTdbRhPb(Tosa, RHosa / 100, 101.325);
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
    pZoneHybridUnitaryAirConditioner->Initialize(1);
    pZoneHybridUnitaryAirConditioner->InitializeModelParams();

    pZoneHybridUnitaryAirConditioner->doStep(RequestedCooling, Requestedheating, Requested_Humidification, Requested_Dehumidification, DesignMinVR);
    // output results
    modenumber = pZoneHybridUnitaryAirConditioner->PrimaryMode;
    Real64 primaryRuntime = pZoneHybridUnitaryAirConditioner->PrimaryModeRuntimeFraction;
    Real64 Tsa = pZoneHybridUnitaryAirConditioner->OutletTemp;
    Real64 Wsa = pZoneHybridUnitaryAirConditioner->OutletHumRat;
    Real64 Msa = pZoneHybridUnitaryAirConditioner->OutletMassFlowRate;
    Real64 Y_val = pZoneHybridUnitaryAirConditioner->FinalElectricalPower / 1000;
    Real64 ErrorCode = pZoneHybridUnitaryAirConditioner->ErrorCode;
    Real64 deliveredSC = pZoneHybridUnitaryAirConditioner->UnitSensibleCoolingRate;
    Real64 deliveredSH = pZoneHybridUnitaryAirConditioner->UnitSensibleHeatingRate;
    Real64 Ventilation = pZoneHybridUnitaryAirConditioner->SupplyVentilationAir;
    Real64 returnOSAF = pZoneHybridUnitaryAirConditioner->averageOSAF;
    Real64 ElectricPower = pZoneHybridUnitaryAirConditioner->FinalElectricalPower;
    Real64 SupplyFanElectricPower = pZoneHybridUnitaryAirConditioner->SupplyFanElectricPower;             //
    Real64 SupplyFanElectricEnergy = pZoneHybridUnitaryAirConditioner->SupplyFanElectricEnergy;           //
    Real64 SecondaryFuelConsumptionRate = pZoneHybridUnitaryAirConditioner->SecondaryFuelConsumptionRate; ///
    Real64 SecondaryFuelConsumption = pZoneHybridUnitaryAirConditioner->SecondaryFuelConsumption;         //
    Real64 ThirdFuelConsumptionRate = pZoneHybridUnitaryAirConditioner->ThirdFuelConsumptionRate;         //
    Real64 ThirdFuelConsumption = pZoneHybridUnitaryAirConditioner->ThirdFuelConsumption;                 //
    Real64 WaterConsumptionRate = pZoneHybridUnitaryAirConditioner->WaterConsumptionRate;                 //
    Real64 WaterConsumption = pZoneHybridUnitaryAirConditioner->WaterConsumption;                         //
    Real64 ExternalStaticPressure = pZoneHybridUnitaryAirConditioner->ExternalStaticPressure;             //
    pZoneHybridUnitaryAirConditioner->Initialize(1);
    // checks
    EXPECT_NEAR(1.0, returnOSAF, 0.001);
    EXPECT_GT(deliveredSC, 0);
    EXPECT_NEAR(0.0, deliveredSH, 0.001);
    EXPECT_LT(Tsa, Tra);

    // Scenario 2: high cooling larger system

    pZoneHybridUnitaryAirConditioner->Initialize(1);
    pZoneHybridUnitaryAirConditioner->InitializeModelParams();
    pZoneHybridUnitaryAirConditioner->ScalingFactor = pZoneHybridUnitaryAirConditioner->ScalingFactor * 2;
    pZoneHybridUnitaryAirConditioner->ScaledSystemMaximumSupplyAirMassFlowRate =
        pZoneHybridUnitaryAirConditioner->ScaledSystemMaximumSupplyAirMassFlowRate * 2;
    pZoneHybridUnitaryAirConditioner->doStep(RequestedCooling, Requestedheating, Requested_Humidification, Requested_Dehumidification, DesignMinVR);

    // output results
    modenumber = pZoneHybridUnitaryAirConditioner->PrimaryMode;
    primaryRuntime = pZoneHybridUnitaryAirConditioner->PrimaryModeRuntimeFraction;
    Tsa = pZoneHybridUnitaryAirConditioner->OutletTemp;
    Wsa = pZoneHybridUnitaryAirConditioner->OutletHumRat;
    Msa = pZoneHybridUnitaryAirConditioner->OutletMassFlowRate;
    // double Msa_setting0= (pZoneHybridUnitaryAirConditioner->CurrentOperatingSettings[0].Supply_Air_Mass_Flow_Rate);
    Y_val = pZoneHybridUnitaryAirConditioner->FinalElectricalPower / 1000;
    ErrorCode = pZoneHybridUnitaryAirConditioner->ErrorCode;
    deliveredSC = pZoneHybridUnitaryAirConditioner->UnitSensibleCoolingRate;
    deliveredSH = pZoneHybridUnitaryAirConditioner->UnitSensibleHeatingRate;
    Ventilation = pZoneHybridUnitaryAirConditioner->SupplyVentilationAir;
    returnOSAF = pZoneHybridUnitaryAirConditioner->averageOSAF;
    SupplyFanElectricPower = pZoneHybridUnitaryAirConditioner->SupplyFanElectricPower;             //
    SupplyFanElectricEnergy = pZoneHybridUnitaryAirConditioner->SupplyFanElectricEnergy;           //
    SecondaryFuelConsumptionRate = pZoneHybridUnitaryAirConditioner->SecondaryFuelConsumptionRate; ///
    SecondaryFuelConsumption = pZoneHybridUnitaryAirConditioner->SecondaryFuelConsumption;         //
    ThirdFuelConsumptionRate = pZoneHybridUnitaryAirConditioner->ThirdFuelConsumptionRate;         //
    ThirdFuelConsumption = pZoneHybridUnitaryAirConditioner->ThirdFuelConsumption;                 //
    WaterConsumptionRate = pZoneHybridUnitaryAirConditioner->WaterConsumptionRate;                 //
    WaterConsumption = pZoneHybridUnitaryAirConditioner->WaterConsumption;                         //
    ExternalStaticPressure = pZoneHybridUnitaryAirConditioner->ExternalStaticPressure;             //
    // checks
    EXPECT_NEAR(1.0, returnOSAF, 0.001);
    EXPECT_GT(deliveredSC, 0);
    EXPECT_NEAR(0.0, deliveredSH, 0.001);
    EXPECT_LT(Tsa, Tra);

    // Scenario 3: Outside of env conditions. should go to standby and have standby energy
    pZoneHybridUnitaryAirConditioner->Initialize(1);
    pZoneHybridUnitaryAirConditioner->InitializeModelParams();
    pZoneHybridUnitaryAirConditioner->SecInletTemp = 150;
    pZoneHybridUnitaryAirConditioner->SecInletHumRat = 0;

    pZoneHybridUnitaryAirConditioner->doStep(RequestedCooling, Requestedheating, Requested_Humidification, Requested_Dehumidification, DesignMinVR);
    modenumber = pZoneHybridUnitaryAirConditioner->PrimaryMode;
    ElectricPower = pZoneHybridUnitaryAirConditioner->FinalElectricalPower;
    // checks
    EXPECT_EQ(0, modenumber);

    // Scenario 4: Low Cooling
    pZoneHybridUnitaryAirConditioner->Initialize(1);
    pZoneHybridUnitaryAirConditioner->InitializeModelParams();
    DesignMinVR = 1.622720855;
    Tra = 23.00655455; //
    Tosa = 26.67733333;
    RHra = 17.3791073;
    RHosa = 13.1602401;
    Requestedheating = -64358.68966; //-
    RequestedCooling = -633.6613591; // W

    /// add all the corre correct values to set in pZoneHybridUnitaryAirConditioner
    pZoneHybridUnitaryAirConditioner->SecInletTemp = Tosa;
    pZoneHybridUnitaryAirConditioner->SecInletHumRat = Wosa;
    pZoneHybridUnitaryAirConditioner->doStep(RequestedCooling, Requestedheating, Requested_Humidification, Requested_Dehumidification, DesignMinVR);

    // output results
    modenumber = pZoneHybridUnitaryAirConditioner->PrimaryMode;
    primaryRuntime = pZoneHybridUnitaryAirConditioner->PrimaryModeRuntimeFraction;
    Tsa = pZoneHybridUnitaryAirConditioner->OutletTemp;
    Wsa = pZoneHybridUnitaryAirConditioner->OutletHumRat;
    Msa = pZoneHybridUnitaryAirConditioner->OutletMassFlowRate;
    Y_val = pZoneHybridUnitaryAirConditioner->FinalElectricalPower / 1000;
    ErrorCode = pZoneHybridUnitaryAirConditioner->ErrorCode;
    deliveredSC = pZoneHybridUnitaryAirConditioner->UnitSensibleCoolingRate;
    deliveredSH = pZoneHybridUnitaryAirConditioner->UnitSensibleHeatingRate;
    Ventilation = pZoneHybridUnitaryAirConditioner->SupplyVentilationAir;
    returnOSAF = pZoneHybridUnitaryAirConditioner->averageOSAF;
    // checks
    EXPECT_NEAR(1.0, returnOSAF, 0.001);
    EXPECT_GT(deliveredSC, 0);
    EXPECT_NEAR(0.0, deliveredSH, 0.001);
    EXPECT_LT(Tsa, Tra);

    // Scenario 5: No Conditioning
    DesignMinVR = 1.622720855;
    Tra = 21.83325675;
    Tosa = 26.67733333;
    RHra = 18.894394;
    RHosa = 13.1602401;
    Requestedheating = -55795.8058;
    RequestedCooling = 8171.47128;
    pZoneHybridUnitaryAirConditioner->Initialize(1);
    pZoneHybridUnitaryAirConditioner->doStep(RequestedCooling, Requestedheating, Requested_Humidification, Requested_Dehumidification, DesignMinVR);

    // output results
    modenumber = pZoneHybridUnitaryAirConditioner->PrimaryMode;
    primaryRuntime = pZoneHybridUnitaryAirConditioner->PrimaryModeRuntimeFraction;
    Tsa = pZoneHybridUnitaryAirConditioner->OutletTemp;
    Wsa = pZoneHybridUnitaryAirConditioner->OutletHumRat;
    Msa = pZoneHybridUnitaryAirConditioner->OutletMassFlowRate;
    Y_val = pZoneHybridUnitaryAirConditioner->FinalElectricalPower / 1000;
    ErrorCode = pZoneHybridUnitaryAirConditioner->ErrorCode;
    deliveredSC = pZoneHybridUnitaryAirConditioner->UnitSensibleCoolingRate;
    deliveredSH = pZoneHybridUnitaryAirConditioner->UnitSensibleHeatingRate;
    Ventilation = pZoneHybridUnitaryAirConditioner->SupplyVentilationAir;
    returnOSAF = pZoneHybridUnitaryAirConditioner->averageOSAF;
    // checks
    EXPECT_NEAR(1.0, returnOSAF, 0.001);
    EXPECT_GT(Tsa, Tra);
}

} // namespace EnergyPlus
