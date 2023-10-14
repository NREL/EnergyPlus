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

#ifndef DataGlobalConstants_hh_INCLUDED
#define DataGlobalConstants_hh_INCLUDED

// EnergyPlus Headers
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/EnergyPlus.hh>

namespace EnergyPlus {

namespace Constant {

    enum class EndUse
    {
        Invalid = -1,
        Heating,
        Cooling,
        InteriorLights,
        ExteriorLights,
        InteriorEquipment,
        ExteriorEquipment,
        Fans,
        Pumps,
        HeatRejection,
        Humidification,
        HeatRecovery,
        WaterSystem,
        Refrigeration,
        Cogeneration,
        Num
    };

    enum class eResource
    {
        Invalid = -1,
        Electricity,
        NaturalGas,
        Gasoline,
        Diesel,
        Coal,
        Propane,
        FuelOilNo1,
        FuelOilNo2,
        OtherFuel1,
        OtherFuel2,
        DistrictCooling,
        DistrictHeatingWater,
        DistrictHeatingSteam,
        Water,
        None, // used for OtherEquipment object
        EnergyTransfer,
        ElectricityProduced,
        ElectricityPurchased,
        ElectricitySurplusSold,
        ElectricityNet,
        SolarWater,
        SolarAir,
        CarbonEquivalent,
        PlantLoopHeatingDemand,
        PlantLoopCoolingDemand,
        OnSiteWater,
        MainsWater,
        RainWater,
        WellWater,
        Condensate,
        WaterEnvironmentalFactors,
        Source,
        Generic, // only used by custom meters
        SO2,
        NOx,
        N2O,
        PM,
        PM2_5,
        PM10,
        CO,
        CO2,
        CH4,
        NH3,
        NMVOC,
        Hg,
        Pb,
        NuclearHigh,
        NuclearLow,
        Num
    };

    enum class eFuel
    {
        Invalid = -1,
        Electricity,
        NaturalGas,
        Gasoline,
        Diesel,
        Coal,
        Propane,
        FuelOilNo1,
        FuelOilNo2,
        OtherFuel1,
        OtherFuel2,
        DistrictCooling,
        DistrictHeatingWater,
        DistrictHeatingSteam,
        Water,
        None, // used for OtherEquipment object
        Num
    };

    enum class ePollutant
    {
        Invalid = -1,
        SO2,
        NOx,
        N2O,
        PM,
        PM2_5,
        PM10,
        CO,
        CO2,
        CH4,
        NH3,
        NMVOC,
        Hg,
        Pb,
        NuclearHigh,
        NuclearLow,
        Num
    };

    constexpr std::array<eFuel, static_cast<int>(eResource::Num)> eResource2eFuel = {eFuel::Electricity,
                                                                                     eFuel::NaturalGas,
                                                                                     eFuel::Gasoline,
                                                                                     eFuel::Diesel,
                                                                                     eFuel::Coal,
                                                                                     eFuel::Propane,
                                                                                     eFuel::FuelOilNo1,
                                                                                     eFuel::FuelOilNo2,
                                                                                     eFuel::OtherFuel1,
                                                                                     eFuel::OtherFuel2,
                                                                                     eFuel::DistrictCooling,
                                                                                     eFuel::DistrictHeatingWater,
                                                                                     eFuel::DistrictHeatingSteam,
                                                                                     eFuel::Water,
                                                                                     eFuel::None,
                                                                                     eFuel::Invalid,
                                                                                     eFuel::Invalid,
                                                                                     eFuel::Invalid,
                                                                                     eFuel::Invalid,
                                                                                     eFuel::Invalid,
                                                                                     eFuel::Invalid,
                                                                                     eFuel::Invalid,
                                                                                     eFuel::Invalid,
                                                                                     eFuel::Invalid,
                                                                                     eFuel::Invalid,
                                                                                     eFuel::Invalid,
                                                                                     eFuel::Invalid,
                                                                                     eFuel::Invalid,
                                                                                     eFuel::Invalid,
                                                                                     eFuel::Invalid,
                                                                                     eFuel::Invalid,
                                                                                     eFuel::Invalid,
                                                                                     eFuel::Invalid,
                                                                                     eFuel::Invalid,
                                                                                     eFuel::Invalid,
                                                                                     eFuel::Invalid,
                                                                                     eFuel::Invalid,
                                                                                     eFuel::Invalid,
                                                                                     eFuel::Invalid,
                                                                                     eFuel::Invalid,
                                                                                     eFuel::Invalid,
                                                                                     eFuel::Invalid,
                                                                                     eFuel::Invalid,
                                                                                     eFuel::Invalid,
                                                                                     eFuel::Invalid,
                                                                                     eFuel::Invalid,
                                                                                     eFuel::Invalid,
                                                                                     eFuel::Invalid};

    constexpr std::array<eResource, static_cast<int>(eFuel::Num)> eFuel2eResource = {eResource::Electricity,
                                                                                     eResource::NaturalGas,
                                                                                     eResource::Gasoline,
                                                                                     eResource::Diesel,
                                                                                     eResource::Coal,
                                                                                     eResource::Propane,
                                                                                     eResource::FuelOilNo1,
                                                                                     eResource::FuelOilNo2,
                                                                                     eResource::OtherFuel1,
                                                                                     eResource::OtherFuel2,
                                                                                     eResource::DistrictCooling,
                                                                                     eResource::DistrictHeatingWater,
                                                                                     eResource::DistrictHeatingSteam,
                                                                                     eResource::Water,
                                                                                     eResource::None};

    constexpr std::array<ePollutant, static_cast<int>(eResource::Num)> eResource2ePollutant = {
        ePollutant::Invalid, ePollutant::Invalid, ePollutant::Invalid, ePollutant::Invalid, ePollutant::Invalid,     ePollutant::Invalid,
        ePollutant::Invalid, ePollutant::Invalid, ePollutant::Invalid, ePollutant::Invalid, ePollutant::Invalid,     ePollutant::Invalid,
        ePollutant::Invalid, ePollutant::Invalid, ePollutant::Invalid, ePollutant::Invalid, ePollutant::Invalid,     ePollutant::Invalid,
        ePollutant::Invalid, ePollutant::Invalid, ePollutant::Invalid, ePollutant::Invalid, ePollutant::Invalid,     ePollutant::Invalid,
        ePollutant::Invalid, ePollutant::Invalid, ePollutant::Invalid, ePollutant::Invalid, ePollutant::Invalid,     ePollutant::Invalid,
        ePollutant::Invalid, ePollutant::Invalid, ePollutant::Invalid, ePollutant::SO2,     ePollutant::NOx,         ePollutant::N2O,
        ePollutant::PM,      ePollutant::PM2_5,   ePollutant::PM10,    ePollutant::CO,      ePollutant::CO2,         ePollutant::CH4,
        ePollutant::NH3,     ePollutant::NMVOC,   ePollutant::Hg,      ePollutant::Pb,      ePollutant::NuclearHigh, ePollutant::NuclearLow};

    constexpr std::array<eResource, static_cast<int>(ePollutant::Num)> ePollutant2eResource = {eResource::SO2,
                                                                                               eResource::NOx,
                                                                                               eResource::N2O,
                                                                                               eResource::PM,
                                                                                               eResource::PM2_5,
                                                                                               eResource::PM10,
                                                                                               eResource::CO,
                                                                                               eResource::CO2,
                                                                                               eResource::CH4,
                                                                                               eResource::NH3,
                                                                                               eResource::NMVOC,
                                                                                               eResource::Hg,
                                                                                               eResource::Pb,
                                                                                               eResource::NuclearHigh,
                                                                                               eResource::NuclearLow};

    static constexpr std::array<std::string_view, static_cast<int>(eResource::Num)> eResourceNamesUC = {"ELECTRICITY",
                                                                                                        "NATURALGAS",
                                                                                                        "GASOLINE",
                                                                                                        "DIESEL",
                                                                                                        "COAL",
                                                                                                        "PROPANE",
                                                                                                        "FUELOILNO1",
                                                                                                        "FUELOILNO2",
                                                                                                        "OTHERFUEL1",
                                                                                                        "OTHERFUEL2",
                                                                                                        "DISTRICTCOOLING",
                                                                                                        "DISTRICTHEATINGWATER",
                                                                                                        "DISTRICTHEATINGSTEAM",
                                                                                                        "WATER",
                                                                                                        "NONE",
                                                                                                        "ENERGYTRANSFER",
                                                                                                        "ELECTRICITYPRODUCED",
                                                                                                        "ELECTRICITYPURCHASED",
                                                                                                        "ELECTRICITYSURPLUSSOLD",
                                                                                                        "ELECTRICITYNET",
                                                                                                        "SOLARWATER",
                                                                                                        "SOLARAIR",
                                                                                                        "CARBON EQUIVALENT",
                                                                                                        "PLANTLOOPHEATINGDEMAND",
                                                                                                        "PLANTLOOPCOOLINGDEMAND",
                                                                                                        "ONSITEWATER",
                                                                                                        "MAINSWATER",
                                                                                                        "RAINWATER",
                                                                                                        "WELLWATER",
                                                                                                        "CONDENSATE",
                                                                                                        "WATERENVIRONMENTALFACTORS",
                                                                                                        "SOURCE",
                                                                                                        "GENERIC",
                                                                                                        "SO2",
                                                                                                        "NOX",
                                                                                                        "N2O",
                                                                                                        "PM",
                                                                                                        "PM2.5",
                                                                                                        "PM10",
                                                                                                        "CO",
                                                                                                        "CO2",
                                                                                                        "CH4",
                                                                                                        "NH3",
                                                                                                        "NMVOC",
                                                                                                        "HG",
                                                                                                        "PB",
                                                                                                        "NUCLEAR HIGH",
                                                                                                        "NUCLEAR LOW"};

    static constexpr std::array<std::string_view, static_cast<int>(eResource::Num)> eResourceNames = {"Electricity",
                                                                                                      "NaturalGas",
                                                                                                      "Gasoline",
                                                                                                      "Diesel",
                                                                                                      "Coal",
                                                                                                      "Propane",
                                                                                                      "FuelOilNo1",
                                                                                                      "FuelOilNo2",
                                                                                                      "OtherFuel1",
                                                                                                      "OtherFuel2",
                                                                                                      "DistrictCooling",
                                                                                                      "DistrictHeatingWater",
                                                                                                      "DistrictHeatingSteam",
                                                                                                      "Water",
                                                                                                      "None",
                                                                                                      "EnergyTransfer",
                                                                                                      "ElectricityProduced",
                                                                                                      "ElectricityPurchased",
                                                                                                      "ElectricitySurplusSold",
                                                                                                      "ElectricityNet",
                                                                                                      "SolarWater",
                                                                                                      "SolarAir",
                                                                                                      "Carbon Equivalent",
                                                                                                      "PlantLoopHeatingDemand",
                                                                                                      "PlantLoopCoolingDemand",
                                                                                                      "OnSiteWater",
                                                                                                      "MainsWater",
                                                                                                      "RainWater",
                                                                                                      "WellWater",
                                                                                                      "Condensate",
                                                                                                      "WaterEnvironmentalFactors",
                                                                                                      "Source",
                                                                                                      "Generic",
                                                                                                      "SO2",
                                                                                                      "NOx",
                                                                                                      "N2O",
                                                                                                      "PM",
                                                                                                      "PM2.5",
                                                                                                      "PM10",
                                                                                                      "CO",
                                                                                                      "CO2",
                                                                                                      "CH4",
                                                                                                      "NH3",
                                                                                                      "NMVOC",
                                                                                                      "Hg",
                                                                                                      "Pb",
                                                                                                      "Nuclear High",
                                                                                                      "Nuclear Low"};

    static constexpr std::array<std::string_view, static_cast<int>(eFuel::Num)> eFuelNamesUC = {
        eResourceNamesUC[static_cast<int>(eFuel2eResource[static_cast<int>(eFuel::Electricity)])],
        eResourceNamesUC[static_cast<int>(eFuel2eResource[static_cast<int>(eFuel::NaturalGas)])],
        eResourceNamesUC[static_cast<int>(eFuel2eResource[static_cast<int>(eFuel::Gasoline)])],
        eResourceNamesUC[static_cast<int>(eFuel2eResource[static_cast<int>(eFuel::Diesel)])],
        eResourceNamesUC[static_cast<int>(eFuel2eResource[static_cast<int>(eFuel::Coal)])],
        eResourceNamesUC[static_cast<int>(eFuel2eResource[static_cast<int>(eFuel::Propane)])],
        eResourceNamesUC[static_cast<int>(eFuel2eResource[static_cast<int>(eFuel::FuelOilNo1)])],
        eResourceNamesUC[static_cast<int>(eFuel2eResource[static_cast<int>(eFuel::FuelOilNo2)])],
        eResourceNamesUC[static_cast<int>(eFuel2eResource[static_cast<int>(eFuel::OtherFuel1)])],
        eResourceNamesUC[static_cast<int>(eFuel2eResource[static_cast<int>(eFuel::OtherFuel2)])],
        eResourceNamesUC[static_cast<int>(eFuel2eResource[static_cast<int>(eFuel::DistrictCooling)])],
        eResourceNamesUC[static_cast<int>(eFuel2eResource[static_cast<int>(eFuel::DistrictHeatingWater)])],
        eResourceNamesUC[static_cast<int>(eFuel2eResource[static_cast<int>(eFuel::DistrictHeatingSteam)])],
        eResourceNamesUC[static_cast<int>(eFuel2eResource[static_cast<int>(eFuel::Water)])],
        eResourceNamesUC[static_cast<int>(eFuel2eResource[static_cast<int>(eFuel::None)])]};

    static constexpr std::array<std::string_view, static_cast<int>(eFuel::Num)> eFuelNames = {
        eResourceNames[static_cast<int>(eFuel2eResource[static_cast<int>(eFuel::Electricity)])],
        eResourceNames[static_cast<int>(eFuel2eResource[static_cast<int>(eFuel::NaturalGas)])],
        eResourceNames[static_cast<int>(eFuel2eResource[static_cast<int>(eFuel::Gasoline)])],
        eResourceNames[static_cast<int>(eFuel2eResource[static_cast<int>(eFuel::Diesel)])],
        eResourceNames[static_cast<int>(eFuel2eResource[static_cast<int>(eFuel::Coal)])],
        eResourceNames[static_cast<int>(eFuel2eResource[static_cast<int>(eFuel::Propane)])],
        eResourceNames[static_cast<int>(eFuel2eResource[static_cast<int>(eFuel::FuelOilNo1)])],
        eResourceNames[static_cast<int>(eFuel2eResource[static_cast<int>(eFuel::FuelOilNo2)])],
        eResourceNames[static_cast<int>(eFuel2eResource[static_cast<int>(eFuel::OtherFuel1)])],
        eResourceNames[static_cast<int>(eFuel2eResource[static_cast<int>(eFuel::OtherFuel2)])],
        eResourceNames[static_cast<int>(eFuel2eResource[static_cast<int>(eFuel::DistrictCooling)])],
        eResourceNames[static_cast<int>(eFuel2eResource[static_cast<int>(eFuel::DistrictHeatingWater)])],
        eResourceNames[static_cast<int>(eFuel2eResource[static_cast<int>(eFuel::DistrictHeatingSteam)])],
        eResourceNames[static_cast<int>(eFuel2eResource[static_cast<int>(eFuel::Water)])],
        eResourceNames[static_cast<int>(eFuel2eResource[static_cast<int>(eFuel::None)])]};

    static constexpr std::array<std::string_view, static_cast<int>(ePollutant::Num)> ePollutantNamesUC = {
        eResourceNamesUC[static_cast<int>(ePollutant2eResource[static_cast<int>(ePollutant::SO2)])],
        eResourceNamesUC[static_cast<int>(ePollutant2eResource[static_cast<int>(ePollutant::NOx)])],
        eResourceNamesUC[static_cast<int>(ePollutant2eResource[static_cast<int>(ePollutant::N2O)])],
        eResourceNamesUC[static_cast<int>(ePollutant2eResource[static_cast<int>(ePollutant::PM)])],
        eResourceNamesUC[static_cast<int>(ePollutant2eResource[static_cast<int>(ePollutant::PM2_5)])],
        eResourceNamesUC[static_cast<int>(ePollutant2eResource[static_cast<int>(ePollutant::PM10)])],
        eResourceNamesUC[static_cast<int>(ePollutant2eResource[static_cast<int>(ePollutant::CO)])],
        eResourceNamesUC[static_cast<int>(ePollutant2eResource[static_cast<int>(ePollutant::CO2)])],
        eResourceNamesUC[static_cast<int>(ePollutant2eResource[static_cast<int>(ePollutant::CH4)])],
        eResourceNamesUC[static_cast<int>(ePollutant2eResource[static_cast<int>(ePollutant::NH3)])],
        eResourceNamesUC[static_cast<int>(ePollutant2eResource[static_cast<int>(ePollutant::NMVOC)])],
        eResourceNamesUC[static_cast<int>(ePollutant2eResource[static_cast<int>(ePollutant::Hg)])],
        eResourceNamesUC[static_cast<int>(ePollutant2eResource[static_cast<int>(ePollutant::Pb)])],
        eResourceNamesUC[static_cast<int>(ePollutant2eResource[static_cast<int>(ePollutant::NuclearHigh)])],
        eResourceNamesUC[static_cast<int>(ePollutant2eResource[static_cast<int>(ePollutant::NuclearLow)])]};

    static constexpr std::array<std::string_view, static_cast<int>(ePollutant::Num)> ePollutantNames = {
        eResourceNames[static_cast<int>(ePollutant2eResource[static_cast<int>(ePollutant::SO2)])],
        eResourceNames[static_cast<int>(ePollutant2eResource[static_cast<int>(ePollutant::NOx)])],
        eResourceNames[static_cast<int>(ePollutant2eResource[static_cast<int>(ePollutant::N2O)])],
        eResourceNames[static_cast<int>(ePollutant2eResource[static_cast<int>(ePollutant::PM)])],
        eResourceNames[static_cast<int>(ePollutant2eResource[static_cast<int>(ePollutant::PM2_5)])],
        eResourceNames[static_cast<int>(ePollutant2eResource[static_cast<int>(ePollutant::PM10)])],
        eResourceNames[static_cast<int>(ePollutant2eResource[static_cast<int>(ePollutant::CO)])],
        eResourceNames[static_cast<int>(ePollutant2eResource[static_cast<int>(ePollutant::CO2)])],
        eResourceNames[static_cast<int>(ePollutant2eResource[static_cast<int>(ePollutant::CH4)])],
        eResourceNames[static_cast<int>(ePollutant2eResource[static_cast<int>(ePollutant::NH3)])],
        eResourceNames[static_cast<int>(ePollutant2eResource[static_cast<int>(ePollutant::NMVOC)])],
        eResourceNames[static_cast<int>(ePollutant2eResource[static_cast<int>(ePollutant::Hg)])],
        eResourceNames[static_cast<int>(ePollutant2eResource[static_cast<int>(ePollutant::Pb)])],
        eResourceNames[static_cast<int>(ePollutant2eResource[static_cast<int>(ePollutant::NuclearHigh)])],
        eResourceNames[static_cast<int>(ePollutant2eResource[static_cast<int>(ePollutant::NuclearLow)])]};

    enum class CallIndicator
    {
        Invalid = -1,
        BeginDay,
        DuringDay,
        EndDay,
        EndZoneSizingCalc,
        EndSysSizingCalc,
        Num
    };

    // Parameters for KindOfSim
    enum class KindOfSim
    {
        // TODO: enum check
        Invalid = -1,
        DesignDay = 1,
        RunPeriodDesign = 2,
        RunPeriodWeather = 3,
        HVACSizeDesignDay = 4,       // a regular design day run during HVAC Sizing Simulation
        HVACSizeRunPeriodDesign = 5, // a weather period design day run during HVAC Sizing Simulation
        ReadAllWeatherData = 6,      // a weather period for reading all weather data prior to the simulation
        Num
    };

    Real64 constexpr OneThird = 1.0 / 3.0;   // 1/3 in highest precision
    Real64 constexpr OneFourth = 1.0 / 4.0;  // 1/4 in highest precision
    Real64 constexpr OneFifth = 1.0 / 5.0;   // 1/5 in highest precision
    Real64 constexpr OneSixth = 1.0 / 6.0;   // 1/6 in highest precision
    Real64 constexpr FourFifths = 4.0 / 5.0; // 4/5 in highest precision

    Real64 constexpr MaxEXPArg = 709.78;       // maximum exponent in EXP() function
    Real64 constexpr Pi = 3.14159265358979324; // Pi 3.1415926535897932384626435
    Real64 constexpr PiOvr2 = Pi / 2.0;        // Pi/2
    Real64 constexpr TwoPi = 2.0 * Pi;         // 2*Pi 6.2831853071795864769252868
    Real64 constexpr GravityConstant = 9.807;
    Real64 constexpr DegToRadians = Pi / 180.0;                           // Conversion for Degrees to Radians
    Real64 constexpr RadToDeg = 180.0 / Pi;                               // Conversion for Radians to Degrees
    Real64 constexpr SecInHour = 3600.0;                                  // Conversion for hours to seconds
    Real64 constexpr HoursInDay = 24.0;                                   // Number of Hours in Day
    Real64 constexpr SecsInDay = SecInHour * HoursInDay;                  // Number of seconds in Day
    Real64 constexpr BigNumber = std::numeric_limits<Real64>::max();      // Max Number real used for initializations
    Real64 constexpr rTinyValue = std::numeric_limits<Real64>::epsilon(); // Tiny value to replace use of TINY(x)
    std::string::size_type constexpr MaxNameLength =
        100;                              // Maximum Name Length in Characters -- should be the same as MaxAlphaArgLength in InputProcessor module
    Real64 constexpr KelvinConv = 273.15; // Conversion factor for C to K and K to C
    Real64 constexpr TriplePointOfWaterTempKelvin = 273.16; // The triple point of water, in Kelvin
    Real64 constexpr InitConvTemp = 5.05;                   // [deg C], standard init vol to mass flow conversion temp
    Real64 constexpr AutoCalculate = -99999.0;              // automatically calculate some fields.
    Real64 constexpr CWInitConvTemp = 5.05;                 // [deg C], standard init chilled water vol to mass flow conversion temp
    Real64 constexpr HWInitConvTemp = 60.0;                 // [deg C], standard init hot water vol to mass flow conversion temp
    Real64 constexpr SteamInitConvTemp = 100.0;             // [deg C], standard init steam vol to mass flow conversion temp
    Real64 constexpr StefanBoltzmann = 5.6697E-8;           // Stefan-Boltzmann constant in W/(m2*K4)
    Real64 constexpr UniversalGasConst = 8314.462175;       // Universal Gas Constant (J/mol*K)
    Real64 constexpr convertJtoGJ = 1.0E-9;                 // Conversion factor for J to GJ

} // namespace Constant

} // namespace EnergyPlus

#endif
