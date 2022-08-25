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
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataBranchAirLoopPlant.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/StandardRatings.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

namespace StandardRatings {

    // MODULE INFORMATION:
    //       AUTHOR         Chandan Sharma
    //       DATE WRITTEN   February 2012
    //       MODIFIED       February 2013, Bereket Nigusse
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // This module contains the subroutines required to calculate the following standard ratings of HVAC equipment
    // 1) Integrated Part Load Value (IPLV) rating for EIR and Reformulated EIR chillers
    // 2) a) Standard Rated (net) Cooling Capacity
    //    b) Seasonal Energy Efficiency Ratio (SEER)
    //    c) Energy Efficiency Ratio (EER),
    //    d) Integrated Energy Efficiency Ratio (IEER)
    //       for Air-to-Air Direct Expansion Air Conditioner and Heat Pumps having a single-speed compressor,
    //       fixed speed indoor supply air fan, and air-cooled condensers.
    // 3) Heating Seasonal Performance Factor (HSPF) for Air-Source Direct Expansion Heat Pumps having a single-speed compressor,
    //       fixed speed indoor supply air fan
    // 4) Seasonal Energy Efficiency Ratio (SEER) for Air-Source Direct Expansion multi-speed compressor Heat Pumps
    // 5) Heating Seasonal Performance Factor (HSPF) for Air-Source Direct Expansion multi-speed compressor Heat Pumps
    // METHODOLOGY EMPLOYED:
    // Using the user specified reference capacity, reference COP and performance curves, the chiller or DX coil models are executed
    // for standard test conditions as specified in ANSI/AHRI 550/590, 210/240 and 340/360. Then results of the simulated test points
    // are processed into standard ratings according to standard's procedures.

    // REFERENCES:
    // (1) AHRI Standard 550/590-2011:  Standard for Performance Rating of Water-Chilling Packages using the Vapor
    //                                  Compression Cycle. Arlington, VA:  Air-Conditioning, Heating,
    //                                  and Refrigeration Institute.
    // (2) ANSI/AHRI Standard 210/240-2008:  Standard for Performance Rating of Unitary Air-Conditioning and
    //                                       Air-Source Heat Pumps. Arlington, VA:  Air-Conditioning, Heating
    //                                       , and Refrigeration Institute.
    // (3) ANSI/AHRI Standard 340/360-2007:  Standard for Performance Rating of Commercial and Industrial
    //                                       Unitary Air-Conditioning and Heat Pump Equipment.  Arlington,
    //                                       VA:  Air-Conditioning, Heating, and Refrigeration Institute.

    // OTHER NOTES: none

    // Data
    Real64 constexpr IndoorCoilInletAirWetBulbTempRated(19.44);     // 19.44C (67F)  Tests A2, B2, B1, and F1
    Real64 constexpr OutdoorCoilInletAirDryBulbTempRated(35.0);     // 35.00C (95F)  Tests A2, B2, B1, and F1
    Real64 constexpr OutdoorCoilInletAirDryBulbTempTestA2(35.0);    // 35.00C (95F)  Test A2 (high speed)
    Real64 constexpr OutdoorCoilInletAirDryBulbTempTestB2(27.78);   // 27.78C (82F)  Test B2 (high speed)
    Real64 constexpr OutdoorCoilInletAirDryBulbTempTestB1(27.78);   // 27.78C (82F)  Test B1 (Low speed)
    Real64 constexpr OutdoorCoilInletAirDryBulbTempTestF1(19.44);   // 19.44C (67F)  Test B1 (Low speed)
    Real64 constexpr OutdoorCoilInletAirDryBulbTempTestEint(30.55); // 30.55 (87F) Test Eint

    // AHRI Standard 210/240-2008 Performance Test Conditions for Unitary Air-to-Air Air-Conditioning and Heat Pump Equipment
    // AHRI STANDARD 210/240 - 2023
    Real64 constexpr CoolingCoilInletAirWetBulbTempRated(19.44); // 19.44C (67F)  Tests A and B
    // AHRI STANDARD 210/240 - 2023
    Real64 constexpr OutdoorUnitInletAirDryBulbTempRated(35.0); // 35.00C (95F)  Test A (rated capacity)

    Real64 constexpr OutdoorUnitInletAirDryBulbTemp(27.78); // 27.78C (82F)  Test B (for SEER)
    Real64 constexpr AirMassFlowRatioRated(1.0);            // AHRI test is at the design flow rate and hence AirMassFlowRatio is 1.0
    // AHRI STANDARD 210/240 - 2017
    Real64 constexpr DefaultFanPowerPerEvapAirFlowRate(773.3); // 365 W/1000 scfm or 773.3 W/(m3/s). The AHRI standard
    // AHRI STANDARD 210/240 - 2023
    Real64 constexpr DefaultFanPowerPerEvapAirFlowRateSEER2(934.4); // 441 W/1000 scfm or 934.4 W/(m3/s). The AHRI standard
    // specifies a nominal/default fan electric power consumption per rated air
    // volume flow rate to account for indoor fan electric power consumption
    // when the standard tests are conducted on units that do not have an
    // indoor air circulating fan. Used if user doesn't enter a specific value.

    // AHRI STANDARD 210/240 - 2023
    Real64 constexpr PLRforSEER(0.5); // Part-load ratio for SEER calculation (single speed DX cooling coils)

    static constexpr std::array<Real64, 4> ReducedPLR = {1.0, 0.75, 0.50, 0.25};               // Reduced Capacity part-load conditions
    static constexpr std::array<Real64, 4> IEERWeightingFactor = {0.020, 0.617, 0.238, 0.125}; // EER Weighting factors (IEER)
    Real64 constexpr OADBTempLowReducedCapacityTest(18.3);                                     // Outdoor air dry-bulb temp in degrees C (65F)
    // Std. AHRI AHRI 340/360 Dry-bulb Temp at reduced capacity, <= 0.444

    // HSPF AHRI STANDARD 210/240 - 2017
    int constexpr TotalNumOfStandardDHRs(16);                                                  // Total number of standard design heating requirements
    static constexpr std::array<Real64, 6> TotalNumOfTemperatureBins = {9, 10, 13, 15, 18, 9}; // Total number of temperature bins for a region
    static constexpr std::array<Real64, 16> StandardDesignHeatingRequirement = {1465.36,
                                                                                2930.71,
                                                                                4396.07,
                                                                                5861.42,
                                                                                7326.78,
                                                                                8792.14,
                                                                                10257.49,
                                                                                11722.85,
                                                                                14653.56,
                                                                                17584.27,
                                                                                20514.98,
                                                                                23445.70,
                                                                                26376.41,
                                                                                29307.12,
                                                                                32237.83,
                                                                                38099.26};

    // HSPF2 AHRI STANDARD 210/240 - 2023
    // BPS: As Fractional Bin hours are not summing up to 1 we using an adjusted array of total number of Temperature Bins per region.
    // static constexpr std::array<Real64, 6> TotalNumOfTemperatureBinsHSPF2 = {8, 8, 11, 13, 16, 7}; // Total number of temperature bins for each
    // region
    static constexpr std::array<Real64, 6> TotalNumOfTemperatureBinsHSPF2 = {9, 10, 13, 15, 18, 9};

    // Standardized DHRs from ANSI/AHRI 210/240 - 2017
    Real64 constexpr CorrectionFactor(0.77); // A correction factor which tends to improve the agreement
    // Slope Adjustment Factor (Cx)  from ANSI/AHRI 210/240 - 2023 (Section 11.2.2 - Table 14)
    // 1. Variable Speed Load Factor (Cvs) | Variable Speed  Equipment
    static constexpr std::array<Real64, 6> VariableSpeedLoadFactor = {1.03, 0.99, 1.21, 1.07, 1.08, 1.03};
    // 2. Heating Load Line Equation Slope Factor (C) | other Equipment types
    static constexpr std::array<Real64, 6> SpeedLoadFactor = {1.10, 1.06, 1.30, 1.15, 1.16, 1.11};

    // between calculated and measured building loads, dimensionless.
    Real64 constexpr CyclicDegradationCoeff(0.25);
    // For Single Stage Systems, if the optional CFull and DFull tests are not performed, a
    // default value of 0.20 shall be used for the cooling Degradation Coefficient
    Real64 constexpr CyclicDegradationCoeffSEER2(0.20); // ANSI/AHRI 210/240 2023 Section 6.1.3.1

    // Default Heating Degradation Coefficient | HSPF2
    // ANSI/AHRI Standard 2023 Section 6.1.3.2.1 -> For Single Stage Systems, if the optional H1CFull test or H1CLow is not performed, a default value
    // of 0.25 shall be used for the heating Degradation Coefficient
    Real64 constexpr CyclicHeatingDegradationCoeffHSPF2(0.25);

    // AHRI STANDARD 210/240 - 2017 & 2023
    static constexpr std::array<Real64, 6> OutdoorDesignTemperature = {2.78, -2.78, -8.33, -15.0, -23.33, -1.11};

    // (Tzl)the zero-load temperature variation by climate region ANSI/AHRI 210/240 - 2023 (Section 11.2.2 - Table 14)
    // [ 58 57 56 55 55 57 ] in Fahrenheit
    static constexpr std::array<Real64, 6> ZoneLoadTemperature = {14.44, 13.89, 13.33, 12.78, 12.78, 13.89};

    // Outdoor design temperature for a region from ANSI/AHRI 210/240 - 2017 & 2023
    static constexpr std::array<Real64, 18> OutdoorBinTemperature = {
        16.67, 13.89, 11.11, 8.33, 5.56, 2.78, 0.00, -2.78, -5.56, -8.33, -11.11, -13.89, -16.67, -19.44, -22.22, -25.00, -27.78, -30.56};
    int constexpr NumberOfRegions{6};
    int constexpr NumberOfBins{18};
    // Fractional bin hours for different bin temperatures from ANSI/AHRI 210/240 - 2017
    static constexpr std::array<std::array<Real64, NumberOfBins>, NumberOfRegions> FracBinHoursAtOutdoorBinTemp = {
        {{0.291, 0.239, 0.194, 0.129, 0.081, 0.041, 0.019, 0.005, 0.001, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0},
         {0.215, 0.189, 0.163, 0.143, 0.112, 0.088, 0.056, 0.024, 0.008, 0.002, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0},
         {0.153, 0.142, 0.138, 0.137, 0.135, 0.118, 0.092, 0.047, 0.021, 0.009, 0.005, 0.002, 0.001, 0.0, 0.0, 0.0, 0.0, 0.0},
         {0.132, 0.111, 0.103, 0.093, 0.1, 0.109, 0.126, 0.087, 0.055, 0.036, 0.026, 0.013, 0.006, 0.002, 0.001, 0.0, 0.0, 0.0},
         {0.106, 0.092, 0.086, 0.076, 0.078, 0.087, 0.102, 0.094, 0.074, 0.055, 0.047, 0.038, 0.029, 0.018, 0.01, 0.005, 0.002, 0.001},
         {0.113, 0.206, 0.215, 0.204, 0.141, 0.076, 0.034, 0.008, 0.003, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0}}};

    // Fractional bin hours for different bin temperatures from ANSI/AHRI 210/240 - 2023 (Section 11.2.2 - Table 14)
    // As the Fractional Bin Hours for each region is not summing up to 1 we're using the adjusted one down below
    // static constexpr std::array<std::array<Real64, NumberOfBins>, NumberOfRegions> FracBinHoursAtOutdoorBinTempHSPF2 = {
    //{{0.0, 0.239, 0.194, 0.129, 0.081, 0.041, 0.019, 0.005, 0.001, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0},
    // {0.0, 0.0, 0.163, 0.143, 0.112, 0.088, 0.056, 0.024, 0.008, 0.002, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0},
    // {0.0, 0.0, 0.138, 0.137, 0.135, 0.118, 0.092, 0.047, 0.021, 0.009, 0.005, 0.002, 0.001, 0.0, 0.0, 0.0, 0.0, 0.0},
    // {0.0, 0.0, 0.103, 0.093, 0.1, 0.109, 0.126, 0.087, 0.055, 0.036, 0.026, 0.013, 0.006, 0.002, 0.001, 0.0, 0.0, 0.0},
    // {0.0, 0.0, 0.086, 0.076, 0.078, 0.087, 0.102, 0.094, 0.074, 0.055, 0.047, 0.038, 0.029, 0.018, 0.01, 0.005, 0.002, 0.001},
    // {0.0, 0.0, 0.215, 0.204, 0.141, 0.076, 0.034, 0.008, 0.003, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0}}};
    // Adjusted to sum up to 1
    static constexpr std::array<std::array<Real64, NumberOfBins>, NumberOfRegions> FracBinHoursAtOutdoorBinTempHSPF2 = {{{0.0,
                                                                                                                          0.337094499,
                                                                                                                          0.273624824,
                                                                                                                          0.181946403,
                                                                                                                          0.114245416,
                                                                                                                          0.057827927,
                                                                                                                          0.026798307,
                                                                                                                          0.007052186,
                                                                                                                          0.001410437,
                                                                                                                          0.0,
                                                                                                                          0.0,
                                                                                                                          0.0,
                                                                                                                          0.0,
                                                                                                                          0.0,
                                                                                                                          0.0,
                                                                                                                          0.0,
                                                                                                                          0.0,
                                                                                                                          0.0},
                                                                                                                         {0.0,
                                                                                                                          0.0,
                                                                                                                          0.273489933,
                                                                                                                          0.239932886,
                                                                                                                          0.187919463,
                                                                                                                          0.147651007,
                                                                                                                          0.093959732,
                                                                                                                          0.040268456,
                                                                                                                          0.013422819,
                                                                                                                          0.003355705,
                                                                                                                          0.0,
                                                                                                                          0.0,
                                                                                                                          0.0,
                                                                                                                          0.0,
                                                                                                                          0.0,
                                                                                                                          0.0,
                                                                                                                          0.0,
                                                                                                                          0.0},
                                                                                                                         {0.0,
                                                                                                                          0.0,
                                                                                                                          0.195744681,
                                                                                                                          0.194326241,
                                                                                                                          0.191489362,
                                                                                                                          0.167375887,
                                                                                                                          0.130496454,
                                                                                                                          0.066666667,
                                                                                                                          0.029787234,
                                                                                                                          0.012765957,
                                                                                                                          0.007092199,
                                                                                                                          0.002836879,
                                                                                                                          0.00141844,
                                                                                                                          0.0,
                                                                                                                          0.0,
                                                                                                                          0.0,
                                                                                                                          0.0,
                                                                                                                          0.0},
                                                                                                                         {0.0,
                                                                                                                          0.0,
                                                                                                                          0.136063408,
                                                                                                                          0.122853369,
                                                                                                                          0.132100396,
                                                                                                                          0.143989432,
                                                                                                                          0.166446499,
                                                                                                                          0.114927345,
                                                                                                                          0.072655218,
                                                                                                                          0.047556143,
                                                                                                                          0.034346103,
                                                                                                                          0.017173052,
                                                                                                                          0.007926024,
                                                                                                                          0.002642008,
                                                                                                                          0.001321004,
                                                                                                                          0.0,
                                                                                                                          0.0,
                                                                                                                          0.0},
                                                                                                                         {0.0,
                                                                                                                          0.0,
                                                                                                                          0.10723192,
                                                                                                                          0.094763092,
                                                                                                                          0.097256858,
                                                                                                                          0.108478803,
                                                                                                                          0.127182045,
                                                                                                                          0.117206983,
                                                                                                                          0.092269327,
                                                                                                                          0.068578554,
                                                                                                                          0.058603491,
                                                                                                                          0.047381546,
                                                                                                                          0.036159601,
                                                                                                                          0.02244389,
                                                                                                                          0.012468828,
                                                                                                                          0.006234414,
                                                                                                                          0.002493766,
                                                                                                                          0.001246883},
                                                                                                                         {0.0,
                                                                                                                          0.0,
                                                                                                                          0.315712188,
                                                                                                                          0.299559471,
                                                                                                                          0.207048458,
                                                                                                                          0.111600587,
                                                                                                                          0.049926579,
                                                                                                                          0.01174743,
                                                                                                                          0.004405286,
                                                                                                                          0.0,
                                                                                                                          0.0,
                                                                                                                          0.0,
                                                                                                                          0.0,
                                                                                                                          0.0,
                                                                                                                          0.0,
                                                                                                                          0.0,
                                                                                                                          0.0,
                                                                                                                          0.0}}};

    // Representative cooling season Outdoor air temperature bin from ANSI/AHRI 210/240-2008
    int constexpr NumOfOATempBins(8); // number of outdoor temperature bins for cooling season
    static constexpr std::array<Real64, NumOfOATempBins> OutdoorBinTemperatureSEER = {19.44, 22.22, 25.00, 27.78, 30.56, 33.33, 36.11, 38.89};
    // Fractional bin hours for different bin temperatures for cooling, from ANSI/AHRI 210/240 - 2008
    // AHRI STANDARD 210/240 - 2023 | This does sum up to 1
    static constexpr std::array<Real64, NumOfOATempBins> CoolFracBinHoursAtOutdoorBinTemp = {0.214, 0.231, 0.216, 0.161, 0.104, 0.052, 0.018, 0.004};

    Real64 constexpr HeatingIndoorCoilInletAirDBTempRated(21.11); // Heating coil entering air dry-bulb temperature in
    // degrees C (70F) Test H1, H2 and H3
    // (low and High Speed) Std. AHRI 210/240
    Real64 constexpr HeatingOutdoorCoilInletAirDBTempH0Test(16.67); // Outdoor air dry-bulb temp in degrees C (47F)
    // Test H0 (low and High Speed) Std. AHRI 210/240

    // ANSI/ASHRAE Standard 127-2012 -- Method of Testing for Rating Computer and Data Processing Room Unitary Air Conditioners
    //  Class 1 23.9C( 75.0F ) 23.9C( 75.0F ) 23.9C( 75.0F ) 23.9C( 75.0F )
    //  Class 2 29.4C( 85.0F ) 29.4C( 85.0F ) 29.4C( 85.0F ) 29.4C( 85.0F )
    //  Class 3 35.0C( 95.0F ) 35.0C( 95.0F ) 35.0C( 95.0F ) 35.0C( 95.0F )
    //  Class 4 40.5C( 105F ) 40.5C( 105F ) 40.5C( 105F ) 40.5C( 105F )
    static constexpr std::array<Real64, 4> IndoorDBTempClassI2IV = {23.9, 29.4, 35.0, 40.5};
    Real64 constexpr IndoorTDPA2D(11.1);
    // 35.0C( 95.0F ) 26.7C( 80.0F ) 18.3C( 65.0F ) 4.4C( 40.0F )
    static constexpr std::array<Real64, 4> OutdoorDBTempAllClassA2D = {35.0, 26.7, 18.3, 4.4};

    // Functions

    void CalcChillerIPLV(EnergyPlusData &state,
                         std::string const &ChillerName,               // Name of Chiller for which IPLV is calculated
                         DataPlant::PlantEquipmentType ChillerType,    // Type of Chiller - EIR or Reformulated EIR
                         Real64 const RefCap,                          // Reference capacity of chiller [W]
                         Real64 const RefCOP,                          // Reference coefficient of performance [W/W]
                         DataPlant::CondenserType const CondenserType, // Type of Condenser - Air Cooled, Water Cooled or Evap Cooled
                         int const CapFTempCurveIndex,                 // Index for the total cooling capacity modifier curve
                         int const EIRFTempCurveIndex,                 // Index for the energy input ratio modifier curve
                         int const EIRFPLRCurveIndex,                  // Index for the EIR vs part-load ratio curve
                         Real64 const MinUnloadRat,                    // Minimum unloading ratio
                         Real64 &IPLV,
                         Optional<Real64 const> EvapVolFlowRate,
                         Optional_int_const CondLoopNum,
                         Optional<Real64 const> OpenMotorEff)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Chandan Sharma, FSEC
        //       DATE WRITTEN   January 2012
        //       Modified       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        //     Calculates Integrated Part Load Value (IPLV) for EIR and reformulated EIR chillers.
        //     Writes the result to EIO file.
        // METHODOLOGY EMPLOYED:
        // (1) Obtains the reference cooling capacity, reference COP and performance curves of the chiller
        // (2) Evaluates the cooling capacity at AHRI test conditions (Per AHRI 551/591,2011 Table 3)
        // (3) Evaluates the EIR at AHRI test conditions (Per AHRI 551/591,2011 Table 3)
        // (4) The EER is evaluated from the total cooling capacity and total electric power
        //     evaluated at the standard rated test conditions.  The IPLV is a weighted value of the COP evaluated
        //     at four different capacities of 100%, 75%, 50% and 25%.  The reduced capacity COPs are evaluated
        //     at different outdoor coil entering temperatures.
        // REFERENCES:
        // (1) AHRI Standard 551/591-2011:  Standard for Performance Rating of Water-Chilling Packages using the Vapor
        //                                  Compression Cycle. Arlington, VA:  Air-Conditioning, Heating,
        //                                  and Refrigeration Institute.

        // Using/Aliasing
        using namespace OutputReportPredefined;
        using CurveManager::CurveValue;
        using CurveManager::GetCurveName;
        using FluidProperties::GetDensityGlycol;
        using FluidProperties::GetSpecificHeatGlycol;
        using General::SolveRoot;

        Real64 constexpr EvapOutletTemp(6.67); // (44F)
        Real64 constexpr Acc(0.0001);          // Accuracy of result
        int constexpr NumOfReducedCap(4);      // Number of reduced capacity test conditions (100%,75%,50%,and 25%)
        int constexpr IterMax(500);            // Maximum number of iterations
        static constexpr std::array<Real64, 4> IPLVWeightingFactor = {0.010, 0.42, 0.45, 0.12}; // EER Weighting factors (IPLV)
        static constexpr std::string_view RoutineName("CalcChillerIPLV");

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 AvailChillerCap(0.0);               // Chiller available capacity at current operating conditions [W]
        Real64 EnteringWaterTempReduced(0.0);      // Entering Condenser Water Temperature at reduced conditions [C]
        Real64 EnteringAirDryBulbTempReduced(0.0); // Outdoor unit entering air dry-bulb temperature
        // at reduced capacity [C]
        Real64 EnteringAirWetBulbTempReduced(0.0); // Outdoor unit entering air wet-bulb temperature
        // at reduced capacity [C]
        Real64 CondenserInletTemp(0.0);   // Entering Condenser Temperature at reduced conditions [C]
        Real64 CondenserOutletTemp0(0.0); // Lower bound for condenser outlet temperature [C]
        Real64 CondenserOutletTemp1(0.0); // Upper bound for condenser outlet temperature [C]
        Real64 CondenserOutletTemp(0.0);  // Calculated condenser outlet temperature which corresponds
        // to EnteringWaterTempReduced above [C]
        Real64 Cp(0.0);         // Water specific heat [J/(kg*C)]
        Real64 Rho(0.0);        // Water density [kg/m3]
        Real64 EIR(0.0);        // Inverse of COP at reduced capacity test conditions (100%, 75%, 50%, and 25%)
        Real64 Power(0.0);      // Power at reduced capacity test conditions (100%, 75%, 50%, and 25%)
        Real64 COPReduced(0.0); // COP at reduced capacity test conditions (100%, 75%, 50%, and 25%)
        Real64 LoadFactor(0.0); // Fractional "on" time for last stage at the desired reduced capacity,
        // (dimensionless)
        Real64 DegradationCoeff(0.0);   // Degradation coeficient, (dimenssionless)
        Real64 ChillerCapFT_rated(0.0); // Chiller capacity fraction at AHRI rated conditions (evaluated as a function of temperature)
        Real64 ChillerCapFT(0.0);       // Chiller capacity fraction (evaluated as a function of temperature)
        Real64 ChillerEIRFT_rated(0.0); // Chiller electric input ratio (EIR = 1 / COP) at AHRI rated conditions as a function of temperature
        Real64 ChillerEIRFT(0.0);       // Chiller electric input ratio (EIR = 1 / COP) as a function of temperature
        Real64 ChillerEIRFPLR(0.0);     // Chiller EIR as a function of part-load ratio (PLR)
        Real64 PartLoadRatio(0.0);      // Part load ratio (PLR) at which chiller is operatign at reduced capacity
        int RedCapNum;                  // Integer counter for reduced capacity
        int SolFla;                     // Flag of solver
        Array1D<Real64> Par(11);        // Parameter array need for RegulaFalsi routine

        // Initialize local variables
        AvailChillerCap = 0.0;
        EnteringWaterTempReduced = 0.0;
        EnteringAirDryBulbTempReduced = 0.0;
        EnteringAirWetBulbTempReduced = 0.0;
        CondenserInletTemp = 0.0;
        CondenserOutletTemp0 = 0.0;
        CondenserOutletTemp1 = 0.0;
        CondenserOutletTemp = 0.0;
        Cp = 0.0;
        Rho = 0.0;
        IPLV = 0.0;
        EIR = 0.0;
        Power = 0.0;
        COPReduced = 0.0;
        LoadFactor = 0.0;
        DegradationCoeff = 0.0;
        ChillerCapFT_rated = 0.0;
        ChillerCapFT = 0.0;
        ChillerEIRFT_rated = 0.0;
        ChillerEIRFT = 0.0;
        ChillerEIRFPLR = 0.0;
        PartLoadRatio = 0.0;

        CheckCurveLimitsForIPLV(state, ChillerName, ChillerType, CondenserType, CapFTempCurveIndex, EIRFTempCurveIndex);

        // IPLV calculations:
        for (RedCapNum = 0; RedCapNum < NumOfReducedCap; ++RedCapNum) {
            if (CondenserType == DataPlant::CondenserType::WaterCooled) {
                // get the entering water temperature for the reduced capacity test conditions
                if (ReducedPLR[RedCapNum] > 0.50) {
                    EnteringWaterTempReduced = 8.0 + 22.0 * ReducedPLR[RedCapNum];
                } else {
                    EnteringWaterTempReduced = 19.0;
                }
                CondenserInletTemp = EnteringWaterTempReduced;
            } else if (CondenserType == DataPlant::CondenserType::AirCooled) {
                // get the outdoor air dry bulb temperature for the reduced capacity test conditions
                if (ReducedPLR[RedCapNum] > 0.3125) {
                    EnteringAirDryBulbTempReduced = 3.0 + 32.0 * ReducedPLR[RedCapNum];
                } else {
                    EnteringAirDryBulbTempReduced = 13.0;
                }
                CondenserInletTemp = EnteringAirDryBulbTempReduced;
            } else { // EvaporativelyCooled Condenser
                // get the outdoor air wet bulb temperature for the reduced capacity test conditions
                EnteringAirWetBulbTempReduced = 10.0 + 14.0 * ReducedPLR[RedCapNum];
                CondenserInletTemp = EnteringAirWetBulbTempReduced;
            }

            if (ChillerType == DataPlant::PlantEquipmentType::Chiller_ElectricEIR) {
                if (RedCapNum == 0) {
                    // Get curve modifier values at rated conditions (load = 100%)
                    ChillerCapFT_rated = CurveValue(state, CapFTempCurveIndex, EvapOutletTemp, CondenserInletTemp);
                    ChillerEIRFT_rated = CurveValue(state, EIRFTempCurveIndex, EvapOutletTemp, CondenserInletTemp);

                    // Report rated capacity and chiller COP
                    PreDefTableEntry(state, state.dataOutRptPredefined->pdchMechRatCap, ChillerName, RefCap * ChillerCapFT_rated);
                    PreDefTableEntry(state, state.dataOutRptPredefined->pdchMechRatEff, ChillerName, RefCOP / ChillerEIRFT_rated);
                }

                // Get capacity curve info with respect to CW setpoint and entering condenser temps
                ChillerCapFT = CurveValue(state, CapFTempCurveIndex, EvapOutletTemp, CondenserInletTemp);

                ChillerEIRFT = CurveValue(state, EIRFTempCurveIndex, EvapOutletTemp, CondenserInletTemp);

                PartLoadRatio = ReducedPLR[RedCapNum] * ChillerCapFT_rated / ChillerCapFT;

                if (PartLoadRatio >= MinUnloadRat) {
                    ChillerEIRFPLR = CurveValue(state, EIRFPLRCurveIndex, PartLoadRatio);
                } else {
                    ChillerEIRFPLR = CurveValue(state, EIRFPLRCurveIndex, MinUnloadRat);
                    PartLoadRatio = MinUnloadRat;
                }

            } else if (ChillerType == DataPlant::PlantEquipmentType::Chiller_ElectricReformEIR) {
                Cp = GetSpecificHeatGlycol(state,
                                           state.dataPlnt->PlantLoop(CondLoopNum).FluidName,
                                           EnteringWaterTempReduced,
                                           state.dataPlnt->PlantLoop(CondLoopNum).FluidIndex,
                                           RoutineName);

                Rho = GetDensityGlycol(state,
                                       state.dataPlnt->PlantLoop(CondLoopNum).FluidName,
                                       EnteringWaterTempReduced,
                                       state.dataPlnt->PlantLoop(CondLoopNum).FluidIndex,
                                       RoutineName);

                Par(1) = EnteringWaterTempReduced;
                Par(2) = EvapOutletTemp;
                Par(3) = Cp;
                Par(4) = ReducedPLR[RedCapNum];
                Par(5) = EvapVolFlowRate * Rho;
                Par(6) = CapFTempCurveIndex;
                Par(7) = EIRFTempCurveIndex;
                Par(8) = EIRFPLRCurveIndex;
                Par(9) = RefCap;
                Par(10) = RefCOP;
                Par(11) = OpenMotorEff;
                CondenserOutletTemp0 = EnteringWaterTempReduced + 0.1;
                CondenserOutletTemp1 = EnteringWaterTempReduced + 10.0;
                General::SolveRoot(state,
                                   Acc,
                                   IterMax,
                                   SolFla,
                                   CondenserOutletTemp,
                                   ReformEIRChillerCondInletTempResidual,
                                   CondenserOutletTemp0,
                                   CondenserOutletTemp1,
                                   Par);
                if (SolFla == -1) {
                    ShowWarningError(state, "Iteration limit exceeded in calculating Reform Chiller IPLV");
                    ShowContinueError(state, "Reformulated Chiller IPLV calculation failed for " + ChillerName);
                } else if (SolFla == -2) {
                    ShowWarningError(state, "Bad starting values for calculating Reform Chiller IPLV");
                    ShowContinueError(state, "Reformulated Chiller IPLV calculation failed for " + ChillerName);
                }

                if (RedCapNum == 0) {
                    // Get curve modifier values at rated conditions (load = 100%)
                    ChillerCapFT_rated = CurveValue(state, CapFTempCurveIndex, EvapOutletTemp, CondenserOutletTemp);
                    ChillerEIRFT_rated = CurveValue(state, EIRFTempCurveIndex, EvapOutletTemp, CondenserOutletTemp);

                    // Report rated capacity and chiller COP
                    PreDefTableEntry(state, state.dataOutRptPredefined->pdchMechRatCap, ChillerName, RefCap * ChillerCapFT_rated);
                    PreDefTableEntry(state, state.dataOutRptPredefined->pdchMechRatEff, ChillerName, RefCOP / ChillerEIRFT_rated);
                }

                ChillerCapFT = CurveValue(state, CapFTempCurveIndex, EvapOutletTemp, CondenserOutletTemp);

                ChillerEIRFT = CurveValue(state, EIRFTempCurveIndex, EvapOutletTemp, CondenserOutletTemp);

                PartLoadRatio = ReducedPLR[RedCapNum] * ChillerCapFT_rated / ChillerCapFT;

                if (PartLoadRatio >= MinUnloadRat) {
                    ChillerEIRFPLR = CurveValue(state, EIRFPLRCurveIndex, CondenserOutletTemp, PartLoadRatio);
                } else {
                    ChillerEIRFPLR = CurveValue(state, EIRFPLRCurveIndex, CondenserOutletTemp, MinUnloadRat);
                    PartLoadRatio = MinUnloadRat;
                }
            } else {
                // should not come here, do nothing
            }

            // Available chiller capacity as a function of temperature
            if (RefCap > 0.0 && RefCOP > 0.0 && ChillerCapFT > 0.0 && ChillerEIRFT > 0.0) {
                AvailChillerCap = RefCap * ChillerCapFT;
                Power = (AvailChillerCap / RefCOP) * ChillerEIRFPLR * ChillerEIRFT;
                EIR = Power / (PartLoadRatio * AvailChillerCap);

                if (ReducedPLR[RedCapNum] >= MinUnloadRat) {
                    COPReduced = 1.0 / EIR;
                } else {
                    LoadFactor = (ReducedPLR[RedCapNum] * RefCap) / (MinUnloadRat * AvailChillerCap);
                    DegradationCoeff = 1.130 - 0.130 * LoadFactor;
                    COPReduced = 1.0 / (DegradationCoeff * EIR);
                }
                IPLV += IPLVWeightingFactor[RedCapNum] * COPReduced;
            } else {
                {
                    if (ChillerType == DataPlant::PlantEquipmentType::Chiller_ElectricEIR) {
                        ShowWarningError(state,
                                         "Chiller:Electric:EIR = " + ChillerName + ":  Integrated Part Load Value (IPLV) cannot be calculated.");
                    } else if (ChillerType == DataPlant::PlantEquipmentType::Chiller_ElectricReformEIR) {

                        ShowWarningError(state,
                                         "Chiller:Electric:ReformulatedEIR = " + ChillerName +
                                             ":  Integrated Part Load Value (IPLV) cannot be calculated.");
                    }
                }
                if (RefCap <= 0.0) {
                    ShowContinueError(
                        state,
                        format(" Check the chiller autosized or user specified capacity. Autosized or specified chiller capacity = {:.2R}", RefCap));
                }
                if (RefCOP <= 0.0) {
                    ShowContinueError(state, format(" Check the chiller reference or rated COP specified. Specified COP = {:.2R}", RefCOP));
                }
                if (ChillerCapFT <= 0.0) {
                    ShowContinueError(state,
                                      " Check limits in Cooling Capacity Function of Temperature Curve, Curve Type = " +
                                          state.dataCurveManager->PerfCurve(CapFTempCurveIndex).ObjectType +
                                          ", Curve Name = " + GetCurveName(state, CapFTempCurveIndex) + '.');
                    ShowContinueError(state, format(" ..ChillerCapFT value at standard test condition = {:.2R}", ChillerCapFT));
                }
                if (ChillerEIRFT <= 0.0) {
                    ShowContinueError(state,
                                      " Check limits in EIR Function of Temperature Curve, Curve Type = " +
                                          state.dataCurveManager->PerfCurve(EIRFTempCurveIndex).ObjectType +
                                          ", Curve Name = " + GetCurveName(state, EIRFTempCurveIndex) + '.');
                    ShowContinueError(state, format(" ..ChillerEIRFT value at standard test condition = {:.2R}", ChillerEIRFT));
                }
                IPLV = 0.0;
                break;
            }
        }

        // Writes the IPLV value to the EIO file and standard tabular output tables
        ReportChillerIPLV(state, ChillerName, ChillerType, IPLV, IPLV * ConvFromSIToIP);
    }

    Real64
    ReformEIRChillerCondInletTempResidual(EnergyPlusData &state,
                                          Real64 const CondenserOutletTemp, // Condenser outlet temperature (boundary condition or guess value) [C]
                                          Array1<Real64> const &Par         // par(1)  = Condenser inlet temperature at AHRI Standard
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         Chandan Sharma
        //       DATE WRITTEN   February 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // Calculates residual function as described below
        // Residuum = (CondenserInletTempAtAHRIConditions - CondenserInletTemp) / CondenserInletTempAtAHRIConditions.
        // CondenserInletTemp here depends on the CondenserOutletTemp which is being varied to zero the residual.

        // METHODOLOGY EMPLOYED:
        // Varies CondenserOutletTemp until a balance point exists where the model output corresponds to the desired
        // independent variable (i.e. CondenserInletTemp is within tolerance of CondenserInletTempAtAHRIConditions)

        // REFERENCES:

        // Using/Aliasing
        using CurveManager::CurveValue;

        // Return value
        Real64 Residuum; // Residual to be minimized to zero

        // Argument array dimensioning

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        //           551/591 conditons[C]
        // par(2)  = Evaporator outlet temperature [C]
        // par(3)  = Water specific heat [J/(kg*C)]
        // par(4)  = Part load ratio
        // par(5)  = Evaporator mass flow rate [kg/s]
        // par(6)  = Index for the total cooling capacity modifier curve
        // par(7)  = Index for the energy input ratio modifier curve
        // par(8)  = Index for the EIR vs part-load ratio curve
        // par(9)  = Reference capacity of chiller [W]
        // par(10) = Reference coefficient of performance [W/W]
        // par(11) = Open chiller motor efficiency [fraction, 0 to 1]

        // FUNCTION PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        Real64 AvailChillerCap(0.0);         // Chiller available capacity at current operating conditions [W]
        Real64 CondenserInletTemp(0.0);      // Calculated condenser inlet temperature [C]
        Real64 EvapOutletTemp(0.0);          // Evaporator outlet temperature temperature [C]
        Real64 QEvap(0.0);                   // Rate of heat transfer to the evaporator coil [W]
        Real64 QCond(0.0);                   // Rate of heat transfer to the condenser coil [W]
        Real64 Power(0.0);                   // Power at reduced capacity test conditions (100%, 75%, 50%, and 25%)
        Real64 ReformEIRChillerCapFT(0.0);   // Chiller capacity fraction (evaluated as a function of temperature)
        Real64 ReformEIRChillerEIRFT(0.0);   // Chiller electric input ratio (EIR = 1 / COP) as a function of temperature
        Real64 ReformEIRChillerEIRFPLR(0.0); // Chiller EIR as a function of part-load ratio (PLR)

        EvapOutletTemp = Par(2);

        ReformEIRChillerCapFT = CurveValue(state, int(Par(6)), EvapOutletTemp, CondenserOutletTemp);

        ReformEIRChillerEIRFT = CurveValue(state, int(Par(7)), EvapOutletTemp, CondenserOutletTemp);

        // Available chiller capacity as a function of temperature
        AvailChillerCap = Par(9) * ReformEIRChillerCapFT;

        ReformEIRChillerEIRFPLR = CurveValue(state, int(Par(8)), CondenserOutletTemp, Par(4));

        Power = (AvailChillerCap / Par(10)) * ReformEIRChillerEIRFPLR * ReformEIRChillerEIRFT;

        QEvap = AvailChillerCap * Par(4);

        QCond = Power * Par(11) + QEvap;

        if (Par(6) > DataBranchAirLoopPlant::MassFlowTolerance) {
            CondenserInletTemp = CondenserOutletTemp - QCond / Par(5) / Par(3);
        }

        Residuum = (Par(1) - CondenserInletTemp) / Par(1);

        return Residuum;
    }

    void ReportChillerIPLV(EnergyPlusData &state,
                           std::string const &ChillerName,            // Name of Chiller for which IPLV is calculated
                           DataPlant::PlantEquipmentType ChillerType, // Type of Chiller - EIR or Reformulated EIR
                           Real64 const IPLVValueSI,                  // IPLV value in SI units {W/W}
                           Real64 const IPLVValueIP                   // IPLV value in IP units {Btu/W-h}
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Chandan Sharma
        //       DATE WRITTEN   January 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine writes the IPLV values in SI and IP units to
        // the "eio" and tabular output files for EIR Chillers.

        // Using/Aliasing
        using namespace OutputReportPredefined;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        auto &StandardRatingsMyOneTimeFlag = state.dataHVACGlobal->StandardRatingsMyOneTimeFlag;

        // Formats

        if (StandardRatingsMyOneTimeFlag) {
            print(state.files.eio,
                  "{}\n",
                  "! <Chiller Standard Rating Information>, Component Type, Component Name, IPLV in SI Units {W/W}, IPLV in IP Units {Btu/W-h}");
            StandardRatingsMyOneTimeFlag = false;
        }

        {
            static constexpr std::string_view Format_991(" Chiller Standard Rating Information, {}, {}, {:.2R}, {:.2R}\n");
            if (ChillerType == DataPlant::PlantEquipmentType::Chiller_ElectricEIR) {

                print(state.files.eio, Format_991, "Chiller:Electric:EIR", ChillerName, IPLVValueSI, IPLVValueIP);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchMechType, ChillerName, "Chiller:Electric:EIR");

            } else if (ChillerType == DataPlant::PlantEquipmentType::Chiller_ElectricReformEIR) {

                print(state.files.eio, Format_991, "Chiller:Electric:ReformulatedEIR", ChillerName, IPLVValueSI, IPLVValueIP);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchMechType, ChillerName, "Chiller:Electric:ReformulatedEIR");
            }
        }

        // Note: We don't want unit conversio, here, but it's ok since W/W will convert to itself since the column heading has "SI" as a hint
        PreDefTableEntry(state, state.dataOutRptPredefined->pdchMechIPLVSI, ChillerName, IPLVValueSI, 2);
        PreDefTableEntry(state, state.dataOutRptPredefined->pdchMechIPLVIP, ChillerName, IPLVValueIP, 2);
    }

    void CheckCurveLimitsForIPLV(EnergyPlusData &state,
                                 std::string const &ChillerName,               // Name of Chiller
                                 DataPlant::PlantEquipmentType ChillerType,    // Type of Chiller - EIR or ReformulatedEIR
                                 DataPlant::CondenserType const CondenserType, // Type of Condenser - Air Cooled, Water Cooled or Evap Cooled
                                 int const CapFTempCurveIndex,                 // Index for the total cooling capacity modifier curve
                                 int const EIRFTempCurveIndex                  // Index for the energy input ratio modifier curve
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR            Chandan Sharma, FSEC
        //       DATE WRITTEN      January 2012
        //       MODIFIED          na
        //       RE-ENGINEERED     na

        // PURPOSE OF THIS SUBROUTINE:
        // Checks the limits of the various curves used in EIR chiller and returns .FALSE. if the limits do not include
        // the standard test condition(s).

        // Using/Aliasing
        using CurveManager::GetCurveMinMaxValues;
        using CurveManager::GetCurveName;

        // Following parameters are taken from AHRI 551/591,2011 Table 3
        Real64 constexpr HighEWTemp(30.0);       // Entering water temp in degrees C at full load capacity (85F)
        Real64 constexpr LowEWTemp(19.0);        // Entering water temp in degrees C at minimum reduced capacity (65F)
        Real64 constexpr OAHighEDBTemp(35.0);    // Outdoor air dry-bulb temp in degrees C at full load capacity (95F)
        Real64 constexpr OAHighEWBTemp(24.0);    // Outdoor air wet-bulb temp in degrees C at full load capacity (75F)
        Real64 constexpr LeavingWaterTemp(6.67); // Evaporator leaving water temperature in degrees C [44 F]

        //  Minimum and Maximum independent variable limits from Total Cooling Capacity Function of Temperature Curve
        Real64 CapacityLWTempMin(0.0);           // Capacity modifier Min value (leaving water temp), from the Curve:BiQuadratic object
        Real64 CapacityLWTempMax(0.0);           // Capacity modifier Max value (leaving water temp), from the Curve:BiQuadratic object
        Real64 CapacityEnteringCondTempMin(0.0); // Capacity modifier Min value (entering cond temp), from the Curve:BiQuadratic object
        Real64 CapacityEnteringCondTempMax(0.0); // Capacity modifier Max value (entering cond temp), from the Curve:BiQuadratic object

        //  Minimum and Maximum independent variable limits from Energy Input Ratio (EIR) Function of Temperature Curve
        Real64 EIRLWTempMin(0.0);           // EIR modifier Min value (leaving water temp), from the Curve:BiQuadratic object
        Real64 EIRLWTempMax(0.0);           // EIR modifier Max value (leaving water temp), from the Curve:BiQuadratic object
        Real64 EIREnteringCondTempMin(0.0); // EIR modifier Min value (entering cond temp), from the Curve:BiQuadratic object
        Real64 EIREnteringCondTempMax(0.0); // EIR modifier Max value (entering cond temp), from the Curve:BiQuadratic object

        Real64 HighCondenserEnteringTempLimit(0.0); // High limit of entering condenser temperature
        Real64 LowCondenserEnteringTempLimit(0.0);  // Low limit of entering condenser temperature

        bool CapCurveIPLVLimitsExceeded(false); // Logical for capacity curve temperature limits being exceeded (IPLV calcs)
        bool EIRCurveIPLVLimitsExceeded(false); // Logical for EIR temperature limits being exceeded (IPLV calcs)

        GetCurveMinMaxValues(
            state, CapFTempCurveIndex, CapacityLWTempMin, CapacityLWTempMax, CapacityEnteringCondTempMin, CapacityEnteringCondTempMax);
        GetCurveMinMaxValues(state, EIRFTempCurveIndex, EIRLWTempMin, EIRLWTempMax, EIREnteringCondTempMin, EIREnteringCondTempMax);

        if (CondenserType == DataPlant::CondenserType::WaterCooled) {
            HighCondenserEnteringTempLimit = HighEWTemp;
            LowCondenserEnteringTempLimit = LowEWTemp;
        } else if (CondenserType == DataPlant::CondenserType::AirCooled) {
            HighCondenserEnteringTempLimit = OAHighEDBTemp;
            LowCondenserEnteringTempLimit = OAHighEDBTemp;
        } else { // Evaporatively Cooled Condenser
            HighCondenserEnteringTempLimit = OAHighEWBTemp;
            LowCondenserEnteringTempLimit = OAHighEWBTemp;
        }

        // Checking the limits of capacity modifying curve for temperatures (IPLV high and low test conditions)
        if (CapacityEnteringCondTempMax < HighCondenserEnteringTempLimit || CapacityEnteringCondTempMin > LowCondenserEnteringTempLimit ||
            CapacityLWTempMax < LeavingWaterTemp || CapacityLWTempMin > LeavingWaterTemp) {
            CapCurveIPLVLimitsExceeded = true;
        }
        // Checking the limits of EIR modifying curve for temperatures (IPLV high and low test conditions)
        if (EIREnteringCondTempMax < HighCondenserEnteringTempLimit || EIREnteringCondTempMin > LowCondenserEnteringTempLimit ||
            EIRLWTempMax < LeavingWaterTemp || EIRLWTempMin > LeavingWaterTemp) {
            EIRCurveIPLVLimitsExceeded = true;
        }

        // For IPLV:
        if (CapCurveIPLVLimitsExceeded || EIRCurveIPLVLimitsExceeded) {
            if (state.dataGlobal->DisplayExtraWarnings) {

                if (ChillerType == DataPlant::PlantEquipmentType::Chiller_ElectricEIR) {

                    ShowWarningError(state,
                                     "Chiller:Electric:EIR = " + ChillerName +
                                         ":  Integrated Part Load Value (IPLV) calculated is not at the AHRI test condition.");
                } else if (ChillerType == DataPlant::PlantEquipmentType::Chiller_ElectricReformEIR) {

                    ShowWarningError(state,
                                     "Chiller:Electric:ReformulatedEIR = " + ChillerName +
                                         ":  Integrated Part Load Value (IPLV) calculated is not at the AHRI test condition.");
                }
                if (CapCurveIPLVLimitsExceeded) {
                    ShowContinueError(state,
                                      " Check limits in Cooling Capacity Function of Temperature Curve, Curve Type = " +
                                          state.dataCurveManager->PerfCurve(CapFTempCurveIndex).ObjectType +
                                          ", Curve Name = " + GetCurveName(state, CapFTempCurveIndex));
                }
                if (EIRCurveIPLVLimitsExceeded) {
                    ShowContinueError(state,
                                      " Check limits in EIR Function of Temperature Curve, Curve Type = " +
                                          state.dataCurveManager->PerfCurve(EIRFTempCurveIndex).ObjectType +
                                          ", Curve Name = " + GetCurveName(state, EIRFTempCurveIndex));
                }
            }
        }
    }

    void CalcDXCoilStandardRating(
        EnergyPlusData &state,
        std::string const &DXCoilName,                             // Name of DX coil for which HSPF is calculated
        std::string const &DXCoilType,                             // Type of DX coil for which HSPF is calculated
        int const DXCoilType_Num,                                  // Integer Type of DX coil - heating or cooling
        int const ns,                                              // Number of compressor speeds
        Array1A<Real64> const RatedTotalCapacity,                  // Reference capacity of DX coil [W]
        Array1A<Real64> const RatedCOP,                            // Reference coefficient of performance [W/W]
        Array1A_int const CapFFlowCurveIndex,                      // Index for the capacity as a function of flow fraction modifier curve
        Array1A_int const CapFTempCurveIndex,                      // Index for the capacity as a function of temperature modifier curve
        Array1A_int const EIRFFlowCurveIndex,                      // Index for the EIR as a function of flow fraction modifier curve
        Array1A_int const EIRFTempCurveIndex,                      // Index for the EIR as a function of temperature modifier curve
        Array1A_int const PLFFPLRCurveIndex,                       // Index for the PLF vs part-load ratio curve
        Array1A<Real64> const RatedAirVolFlowRate,                 // Reference air flow rate of DX coil [m3/s]
        Array1A<Real64> const FanPowerPerEvapAirFlowRateFromInput, // Reference fan power per evap air flow rate [W/(m3/s)]
        Array1A<Real64> const FanPowerPerEvapAirFlowRateFromInput_2023,
        Optional_int_const
            RegionNum, // Region number for calculating HSPF of single speed DX heating coil //Autodesk:OPTIONAL Used without PRESENT check
        Optional<Real64 const> MinOATCompressor, // Minimum OAT for heat pump compressor operation [C] //Autodesk:OPTIONAL Used without PRESENT check
        Optional<Real64 const>
            OATempCompressorOn, // The outdoor temperature when the compressor is automatically turned //Autodesk:OPTIONAL Used without PRESENT check
        Optional_bool_const
            OATempCompressorOnOffBlank, // Flag used to determine low temperature cut out factor //Autodesk:OPTIONAL Used without PRESENT check
        Optional<HPdefrostControl const> DefrostControl, // defrost control; 1=timed, 2=on-demand //Autodesk:OPTIONAL Used without PRESENT check
        Optional_bool_const ASHRAE127StdRprt             // true if user wishes to report ASHRAE 127 standard ratings
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Bereket Nigusse, Chandan Sharma FSEC
        //       DATE WRITTEN   February 2010,
        //                      B. Nigusse, May 2010  Added EER and IEER Calculation
        //                      C. Sharma, March 2012  Added HSPF Calculation for single speed HP
        //                      B. Nigusse, August 2012 Added SEER Calculation for Multi-speed HP
        //                      B. Nigusse, November 2012 Added HSPF Calculation for Multi-speed HP

        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        //     Calculates:
        //                 (1) Standard Rated (net) Cooling Capacity
        //                 (2) Seasonal Energy Efficiency Ratio (SEER)
        //                 (3) Energy Efficiency Ratio (EER),
        //                 (4) Integrated Energy Efficiency Ratio (IEER)
        //                 for Air-to-Air Direct Expansion Air Conditioner and Heat Pumps having a single-speed compressor,
        //                 fixed speed indoor supply air fan, and air-cooled condensers. Writes the result to EIO file.
        //                 (5) Heating Seasonal Performance Factor (HSPF) for Air-Source Direct Expansion Heat Pumps having
        //                  a single-speed compressor, fixed speed indoor supply air fan
        //                 (6) Standard Rated (net) Cooling Capacity; and
        //                 (7) Seasonal Energy Efficiency Ratio (SEER) for Air-to-Air Heat Pumps having multi-speed
        //                     compressor.
        //                 (8) Heating Seasonal Performance Factor (HSPF) for Air-to-Air Heat Pumps having multi-speed
        //                     compressor.
        // METHODOLOGY EMPLOYED:
        // (A) Methodology for calculating standard ratings for DX air conditioners
        //     (1) Obtains the rated condition parameters:
        //         Cooling capacity (User specified or Autosized Value)
        //         Rated Air volume flow rate through the DX Cooling Coil (User specified or autosized value)
        //     (2) Evaluates the total cooling coil capacity at AHRI test conditions 26.7C/19.4C/27.8C. Then net
        //         cooling capacity is determined from the total cooling capacity of the DX coil at the AHRI test
        //         conditions and accounting for the INDOOR supply air fan heat.
        //     (3) Calculates the electric power consumed by the DX Coil Unit (compressor + outdoor condenser fan).
        //         Evaluates the EIR capacity and flow fraction modifiers at 26.7C/19.4C/27.8C. The net electric
        //         power consumption is determined by adding the indoor fan electric power to the electric power
        //         consumption by the DX Coil Condenser Fan and Compressor at the AHRI test conditions.
        //     (4) The EER is evaluated from the total net cooling capacity and total electric power
        //         evaluated at the standard rated test conditions.  The IEER is a weighted value of the EER evaluated
        //         at four different capacities of 100%, 75%, 50% and 25%.  The reduced capacity EERs are evaluated
        //         at different outdoor coil entering air dry-bulb temperatures.
        // (B) Methodology for calculating standard ratings for DX air air source heat pumps
        //     (1) Obtains the rated condition parameters:
        //         heating capacity (User specified or Autosized Value), COP,  Rated Air volume flow rate through the
        //         DX Cooling Coil (User specified or autosized value) and Fan power per rated air flow rate
        //     (2) Evaluates the heating coil capacities for AHRI tests H1, H2 and H3 using the performance cuves and
        //         input values specified at (1) above. Then net heating capacity is determined from the total heating capacity
        //         of the DX coil at the AHRI test conditions and accounting for the INDOOR supply air fan heat.
        //     (3) Calculates the electric power consumed by the DX Coil Unit (compressor + outdoor condenser fan).
        //         The net electric power consumption is determined by adding the indoor fan electric power to the
        //         electric power consumption by the DX Coil Condenser Fan and Compressor at the AHRI test conditions.
        //     (4) High Temperature Heating Standard (Net) Rating Capacity and Low Temperature Heating Standard (Net)
        //         Rating Capacity capacity are determined using tests H1 adn H3 per ANSI/AHRI 210/240 2008.
        //     (5) The HSPF is evaluated from the total net heating capacity and total electric power
        //         evaluated at the standard rated test conditions. For user specified region number, the outdoor temperatures
        //         are Binned (grouped) and fractioanl bin hours for each bin over the entire heating season are taken
        //         from AHRI 210/240. Then for each bin, building load, heat pump energy adn resistance space heating enegry are
        //         calculated. The sum of building load divided by sum of heat pump and resistance space heating over the
        //         entire heating season gives the HSPF. The detailed calculation algorithms of calculating HSPF
        //         are described in Engineering Reference.
        // (C) Methodology for calculating standard ratings for Multi-Speed Heat Pumps
        //     Net Total Cooling Capacity and SEER
        //     (1) Obtains the rated condition parameters:
        //         Cooling capacity (User specified or Autosized Value)
        //         Rated Air volume flow rate through the DX Cooling Coil (User specified or autosized value)
        //     (2) Evaluates the total cooling coil capacity at AHRI A2 test conditions 26.7C/19.4C/35.0C. Then net
        //         cooling capacity is determined from the total cooling capacity of the DX coil at the AHRI A2 test
        //         conditions and accounting for the INDOOR supply air fan effect.  The net total cooling capacity
        //         is reported at the high (maximum) speed only.
        //     (3) Calculates the electric power consumed by the DX Coil Unit (compressor + outdoor condenser fan).
        //         Evaluates the EIR capacity and flow fraction modifiers at A2, B2, B1, and F1 test coditions per
        //         AHRI/ANSI Std. 210/240 test procedure for multi-speed compressor.  For any inter-
        //         mediate operating conditions (speed), the successive lower and the higher speed performnace are
        //         weighed per the standard.  Electric Power consumption is determined by adding the indoor fan
        //         electric power to the electric power consumption by the outdoor DX Coil Fan and Compressor Power
        //         at the AHRI test conditions.  The net total cooling capacity is also corrected for the fan heat
        //         effect for SEER calculation.
        //     Net Heatingg Capacity and HSPF
        //     (4) Obtains the rated condition parameters:
        //         Heating capacity (User specified or Autosized Value)
        //         Rated Air volume flow rate through the DX Heating Coil (User specified or autosized value)
        //     (5) Evaluates the heating coil capacity at AHRI H12 test conditions 21.1C/15.6C/8.33C. Then net
        //         heating capacity is determined from the total heating capacity of the DX coil at the AHRI H12
        //         test conditions and accounting for the supply supply air fan effect.  The net heating capacity
        //         is reported at the high (maximum) speed only.
        //     (6) Calculates the electric power consumed by the DX Coil Unit (compressor + outdoor condenser fan).
        //         Evaluates the EIR capacity and flow fraction modifiers per AHRI/ANSI Std. 210/240 test procedures
        //         for two speed compressor (H01, H11, H21, H31, H12, H22, and H32 ). This procedure was modified
        //         for multispeed heat pumps. For any inter-mediate operating conditions (speed), the successive
        //         lower and the higher speed performnace are weighed per the standard.
        //         Electric Power consumption is determined by adding the supply fan electric power to the electric
        //         power consumption by the outdoor DX Coil Fan and Compressor Power at the AHRI test conditions.
        //         The net heating capacity is also corrected for the fan heat effect for SEER calculation.
        // REFERENCES:
        // (1) ANSI/AHRI Standard 210/240-2008:  Standard for Performance Rating of Unitary Air-Conditioning and
        //                                       Air-Source Heat Pumps. Arlington, VA:  Air-Conditioning, Heating
        //                                       , and Refrigeration Institute.
        // (2) ANSI/AHRI Standard 340/360-2007:  Standard for Performance Rating of Commercial and Industrial
        //                                       Unitary Air-Conditioning and Heat Pump Equipment.  Arlington,
        //                                       VA:  Air-Conditioning, Heating, and Refrigeration Institute.

        // USE STATEMENTS:

        // Using/Aliasing
        using CurveManager::CurveValue;
        using CurveManager::GetCurveMinMaxValues;
        using DataHVACGlobals::CoilDX_CoolingSingleSpeed;
        using DataHVACGlobals::CoilDX_HeatingEmpirical;
        using DataHVACGlobals::CoilDX_MultiSpeedCooling;
        using DataHVACGlobals::CoilDX_MultiSpeedHeating;

        // Argument array dimensioning
        RatedTotalCapacity.dim(ns);
        RatedCOP.dim(ns);
        CapFFlowCurveIndex.dim(ns);
        CapFTempCurveIndex.dim(ns);
        EIRFFlowCurveIndex.dim(ns);
        EIRFTempCurveIndex.dim(ns);
        PLFFPLRCurveIndex.dim(ns);
        RatedAirVolFlowRate.dim(ns);
        FanPowerPerEvapAirFlowRateFromInput.dim(ns);

        ////TODO: this will be passed as argument in this method later on
        // Array1A<Real64> const FanPowerPerEvapAirFlowRateFromInputSEER2;
        FanPowerPerEvapAirFlowRateFromInput_2023.dim(ns);

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // back on, if applicable, following automatic shut off. This field is
        // used only for HSPF calculation. [C]

        // SUBROUTINE PARAMETER DEFINITIONS:

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Array1D<Real64> FanPowerPerEvapAirFlowRate(ns); // Fan power per air volume flow rate through the evaporator coil [W/(m3/s)]

        // Intermediate values calculated from the inputs in the idf file
        int spnum; // compressor speed number

        // Calculated and reported to the EIO file
        Real64 SEER_User(0.0);     // Seasonal Energy Efficiency Ratio using user PLF curve in SI [W/W]
        Real64 SEER_Standard(0.0); // Seasonal Energy Efficiency Ratio using AHRI 210/240 PLF default curve & C_D in SI [W/W]
        Real64 EER(0.0);           // Energy Efficiency Ratio in SI [W/W]
        Real64 IEER(0.0);          // Integerated Energy Efficiency Ratio in SI [W/W]

        // SEER2 ANSI/AHRI 210/240 Standard 2023 Ratings
        Real64 SEER2_User(0.0);     // Seasonal Energy Efficiency Ratio using user PLF curve in SI [W/W]
        Real64 SEER2_Standard(0.0); // Seasonal Energy Efficiency Ratio using AHRI 210/240 PLF default curve & C_D in SI [W/W]
        Real64 EER_2023(0.0);       // Energy Efficiency Ratio in SI [W/W]
        Real64 IEER_2023(0.0);      // Integerated Energy Efficiency Ratio in SI [W/W]

        Real64 HSPF(0.0);                       // Heating Seasonal Performance Factor in SI [W/W]
        Real64 NetHeatingCapRatedHighTemp(0.0); // Net Rated heating capacity at high temp [W]
        Real64 NetHeatingCapRatedLowTemp(0.0);  // Net Rated heating capacity at low temp [W]

        // HSPF2 ANSI/AHRI 210/240 Standard 2023 Ratings
        Real64 HSPF2_2023(0.0);                      // Heating Seasonal Performance Factor in SI [W/W]
        Real64 NetHeatingCapRatedHighTemp_2023(0.0); // Net Rated heating capacity at high temp [W]
        Real64 NetHeatingCapRatedLowTemp_2023(0.0);  // Net Rated heating capacity at low temp [W]

        Array1D<Real64> NetCoolingCapRated(ns);    // Net Cooling Coil capacity at Rated conditions, accounting for supply fan heat [W]
        Array1D<Real64> NetTotCoolingCapRated(16); // net total cooling capacity of DX Coils for the sixteen ASHRAE Std 127 Test conditions
        Array1D<Real64> TotElectricPowerRated(16); // total electric power of DX Coils for the sixteen ASHRAE Std 127 Test conditions

        Array1D<Real64> NetCoolingCapRated_2023(ns);    // Net Cooling Coil capacity at Rated conditions, accounting for supply fan heat [W]
        Array1D<Real64> NetTotCoolingCapRated_2023(16); // net total cooling capacity of DX Coils for the sixteen ASHRAE Std 127 Test conditions
        Array1D<Real64> TotElectricPowerRated_2023(16); // total electric power of DX Coils for the sixteen ASHRAE Std 127 Test conditions

        NetCoolingCapRated = 0.0;

        switch (DXCoilType_Num) {

        case CoilDX_CoolingSingleSpeed: { // Coil:Cooling:DX:SingleSpeed

            CheckCurveLimitsForStandardRatings(state,
                                               DXCoilName,
                                               DXCoilType,
                                               DXCoilType_Num,
                                               CapFTempCurveIndex(1),
                                               CapFFlowCurveIndex(1),
                                               EIRFTempCurveIndex(1),
                                               EIRFFlowCurveIndex(1),
                                               PLFFPLRCurveIndex(1));

            // Calculated Net Cooling Capacity, SEER, SEER Default, EER, and IEER of single speed DX cooling coils
            std::map<std::string, Real64> StandarRatingResults = SingleSpeedDXCoolingCoilStandardRatings(state,
                                                                                                         DXCoilName,
                                                                                                         DXCoilType,
                                                                                                         CapFTempCurveIndex(1),
                                                                                                         CapFFlowCurveIndex(1),
                                                                                                         EIRFTempCurveIndex(1),
                                                                                                         EIRFFlowCurveIndex(1),
                                                                                                         PLFFPLRCurveIndex(1),
                                                                                                         RatedTotalCapacity(1),
                                                                                                         RatedCOP(1),
                                                                                                         RatedAirVolFlowRate(1),
                                                                                                         FanPowerPerEvapAirFlowRateFromInput(1),
                                                                                                         FanPowerPerEvapAirFlowRateFromInput_2023(1));
            NetCoolingCapRated(1) = StandarRatingResults["NetCoolingCapRated"];
            SEER_User = StandarRatingResults["SEER_User"];
            SEER_Standard = StandarRatingResults["SEER_Standard"];
            EER = StandarRatingResults["EER"];
            IEER = StandarRatingResults["IEER"];

            NetCoolingCapRated_2023(1) = StandarRatingResults["NetCoolingCapRated2023"];
            SEER2_User = StandarRatingResults["SEER2_User"];
            SEER2_Standard = StandarRatingResults["SEER2_Standard"];
            EER_2023 = StandarRatingResults["EER_2023"];
            // IEER_2023 = StandarRatingResults["IEER_2023"];

            // Writes the net rated cooling capacity, SEER, SEER Default, EER and IEER values to the EIO file and standard tabular output tables
            ReportDXCoilRating(state,
                               DXCoilType,
                               DXCoilName,
                               DXCoilType_Num,
                               NetCoolingCapRated(1),
                               SEER_User * ConvFromSIToIP,
                               SEER_Standard * ConvFromSIToIP,
                               EER,
                               EER * ConvFromSIToIP,
                               IEER * ConvFromSIToIP,
                               NetHeatingCapRatedHighTemp,
                               NetHeatingCapRatedLowTemp,
                               HSPF * ConvFromSIToIP,
                               RegionNum,
                               false);

            // ANSI/AHRI 210/240 Std. 2023 Ratings
            // Writes the net rated cooling capacity, SEER2_USER, SEER2_Standard, EER and IEER values to the EIO file and standard tabular output
            // tables
            ReportDXCoilRating(state,
                               DXCoilType,
                               DXCoilName,
                               DXCoilType_Num,
                               NetCoolingCapRated_2023(1),
                               SEER2_User * ConvFromSIToIP,
                               SEER2_Standard * ConvFromSIToIP,
                               EER_2023,
                               EER_2023 * ConvFromSIToIP,
                               IEER_2023 * ConvFromSIToIP,
                               NetHeatingCapRatedHighTemp_2023,
                               NetHeatingCapRatedLowTemp_2023,
                               HSPF2_2023 * ConvFromSIToIP,
                               RegionNum,
                               true);

            if (ASHRAE127StdRprt) {
                DXCoolingCoilDataCenterStandardRatings(state,
                                                       DXCoilName,
                                                       DXCoilType,
                                                       CapFTempCurveIndex(1),
                                                       CapFFlowCurveIndex(1),
                                                       EIRFTempCurveIndex(1),
                                                       EIRFFlowCurveIndex(1),
                                                       PLFFPLRCurveIndex(1),
                                                       RatedTotalCapacity(1),
                                                       RatedCOP(1),
                                                       RatedAirVolFlowRate(1),
                                                       FanPowerPerEvapAirFlowRateFromInput(1),
                                                       NetTotCoolingCapRated,
                                                       TotElectricPowerRated);
                ReportDXCoolCoilDataCenterApplication(state, DXCoilType, DXCoilName, DXCoilType_Num, NetTotCoolingCapRated, TotElectricPowerRated);
            }
            break;
        }
        case CoilDX_HeatingEmpirical: { // Coil:Heating:DX:SingleSpeed

            CheckCurveLimitsForStandardRatings(state,
                                               DXCoilName,
                                               DXCoilType,
                                               DXCoilType_Num,
                                               CapFTempCurveIndex(1),
                                               CapFFlowCurveIndex(1),
                                               EIRFTempCurveIndex(1),
                                               EIRFFlowCurveIndex(1),
                                               PLFFPLRCurveIndex(1));
            // Calculate the standard ratings for single speed DX heating coil
            std::map<std::string, Real64> StandardRatingsResults =
                SingleSpeedDXHeatingCoilStandardRatings(state,
                                                        RatedTotalCapacity(1),
                                                        RatedCOP(1),
                                                        CapFFlowCurveIndex(1),
                                                        CapFTempCurveIndex(1),
                                                        EIRFFlowCurveIndex(1),
                                                        EIRFTempCurveIndex(1),
                                                        RatedAirVolFlowRate(1),
                                                        FanPowerPerEvapAirFlowRateFromInput(1),
                                                        FanPowerPerEvapAirFlowRateFromInput_2023(1),
                                                        RegionNum,
                                                        MinOATCompressor,
                                                        OATempCompressorOn,
                                                        OATempCompressorOnOffBlank,
                                                        DefrostControl);
            NetHeatingCapRatedHighTemp = StandardRatingsResults["NetHeatingCapRated"];
            NetHeatingCapRatedLowTemp = StandardRatingsResults["NetHeatingCapH3Test"];
            HSPF = StandardRatingsResults["HSPF"];

            NetHeatingCapRatedHighTemp_2023 = StandardRatingsResults["NetHeatingCapRated_2023"];
            NetHeatingCapRatedLowTemp_2023 = StandardRatingsResults["NetHeatingCapH3Test_2023"];
            HSPF2_2023 = StandardRatingsResults["HSPF2_2023"];

            // Writes the HSPF value to the EIO file and standard tabular output tables
            ReportDXCoilRating(state,
                               DXCoilType,
                               DXCoilName,
                               DXCoilType_Num,
                               NetCoolingCapRated(1),
                               SEER_User * ConvFromSIToIP,
                               SEER_Standard * ConvFromSIToIP,
                               EER,
                               EER * ConvFromSIToIP,
                               IEER * ConvFromSIToIP,
                               NetHeatingCapRatedHighTemp,
                               NetHeatingCapRatedLowTemp,
                               HSPF * ConvFromSIToIP,
                               RegionNum,
                               false);

            // ANSI/AHRI 210/240 Std. 2023 Ratings
            // Writes the HSPF2 value to the EIO file and standard tabular output tables
            ReportDXCoilRating(state,
                               DXCoilType,
                               DXCoilName,
                               DXCoilType_Num,
                               NetCoolingCapRated_2023(1),
                               SEER2_User * ConvFromSIToIP,
                               SEER2_Standard * ConvFromSIToIP,
                               EER_2023,
                               EER_2023 * ConvFromSIToIP,
                               IEER_2023 * ConvFromSIToIP,
                               NetHeatingCapRatedHighTemp_2023,
                               NetHeatingCapRatedLowTemp_2023,
                               HSPF2_2023 * ConvFromSIToIP,
                               RegionNum,
                               true);
            break;
        }
        case CoilDX_MultiSpeedCooling: { // Coil:Cooling:DX:MultiSpeed,

            for (spnum = 1; spnum <= ns; ++spnum) {
                CheckCurveLimitsForStandardRatings(state,
                                                   DXCoilName,
                                                   DXCoilType,
                                                   DXCoilType_Num,
                                                   CapFTempCurveIndex(spnum),
                                                   CapFFlowCurveIndex(spnum),
                                                   EIRFTempCurveIndex(spnum),
                                                   EIRFFlowCurveIndex(spnum),
                                                   PLFFPLRCurveIndex(spnum));
            }
            // Calculate the standard ratings for multispeed DX cooling coil
            std::map<std::string, Real64> StandardRatingsResult = MultiSpeedDXCoolingCoilStandardRatings(state,
                                                                                                         CapFTempCurveIndex,
                                                                                                         CapFFlowCurveIndex,
                                                                                                         EIRFTempCurveIndex,
                                                                                                         EIRFFlowCurveIndex,
                                                                                                         PLFFPLRCurveIndex,
                                                                                                         RatedTotalCapacity,
                                                                                                         RatedCOP,
                                                                                                         RatedAirVolFlowRate,
                                                                                                         FanPowerPerEvapAirFlowRateFromInput,
                                                                                                         FanPowerPerEvapAirFlowRateFromInput_2023,
                                                                                                         ns);
            NetCoolingCapRated(ns) = StandardRatingsResult["NetCoolingCapRatedMaxSpeed"];
            SEER_User = StandardRatingsResult["SEER_User"];
            SEER_Standard = StandardRatingsResult["SEER_Standard"];

            NetCoolingCapRated_2023(ns) = StandardRatingsResult["NetCoolingCapRatedMaxSpeed2023"];
            SEER2_User = StandardRatingsResult["SEER2_User"];
            SEER2_Standard = StandardRatingsResult["SEER2_Standard"];

            // Writes the SEER value to the EIO file and standard tabular output tables
            ReportDXCoilRating(state,
                               DXCoilType,
                               DXCoilName,
                               DXCoilType_Num,
                               NetCoolingCapRated(ns),
                               SEER_User * ConvFromSIToIP,
                               SEER_Standard * ConvFromSIToIP,
                               0.0,
                               0.0,
                               0.0,
                               0.0,
                               0.0,
                               0.0,
                               0,
                               false);

            // Writes the SEER value to the EIO file and standard tabular output tables
            ReportDXCoilRating(state,
                               DXCoilType,
                               DXCoilName,
                               DXCoilType_Num,
                               NetCoolingCapRated_2023(ns),
                               SEER2_User * ConvFromSIToIP,
                               SEER2_Standard * ConvFromSIToIP,
                               0.0,
                               0.0,
                               0.0,
                               0.0,
                               0.0,
                               0.0,
                               0,
                               true);

            break;
        }
        case CoilDX_MultiSpeedHeating: { // Coil:Heating:DX:MultiSpeed

            for (spnum = 1; spnum <= ns; ++spnum) {
                CheckCurveLimitsForStandardRatings(state,
                                                   DXCoilName,
                                                   DXCoilType,
                                                   DXCoilType_Num,
                                                   CapFTempCurveIndex(spnum),
                                                   CapFFlowCurveIndex(spnum),
                                                   EIRFTempCurveIndex(spnum),
                                                   EIRFFlowCurveIndex(spnum),
                                                   PLFFPLRCurveIndex(spnum));
            }
            // Calculate Net heatig capacity and HSPF & HSPF2 of multispeed DX heating coils
            std::map<std::string, Real64> StandardRatingsResult = MultiSpeedDXHeatingCoilStandardRatings(state,
                                                                                                         DXCoilName,
                                                                                                         DXCoilType,
                                                                                                         CapFTempCurveIndex,
                                                                                                         CapFFlowCurveIndex,
                                                                                                         EIRFTempCurveIndex,
                                                                                                         EIRFFlowCurveIndex,
                                                                                                         PLFFPLRCurveIndex,
                                                                                                         RatedTotalCapacity,
                                                                                                         RatedCOP,
                                                                                                         RatedAirVolFlowRate,
                                                                                                         FanPowerPerEvapAirFlowRateFromInput,
                                                                                                         FanPowerPerEvapAirFlowRateFromInput_2023,
                                                                                                         ns,
                                                                                                         RegionNum,
                                                                                                         MinOATCompressor,
                                                                                                         OATempCompressorOn,
                                                                                                         OATempCompressorOnOffBlank,
                                                                                                         DefrostControl);

            NetHeatingCapRatedHighTemp = StandardRatingsResult["NetHeatingCapRatedHighTemp"];
            NetHeatingCapRatedLowTemp = StandardRatingsResult["NetHeatingCapRatedLowTemp"];
            HSPF = StandardRatingsResult["HSPF"];

            NetHeatingCapRatedHighTemp_2023 = StandardRatingsResult["NetHeatingCapRatedHighTemp_2023"];
            NetHeatingCapRatedLowTemp_2023 = StandardRatingsResult["NetHeatingCapRatedLowTemp_2023"];
            HSPF2_2023 = StandardRatingsResult["HSPF2_2023"];

            // ANSI/AHRI Std. 2017 Ratings
            // Writes the HSPF value to the EIO file and standard tabular output tables
            ReportDXCoilRating(state,
                               DXCoilType,
                               DXCoilName,
                               DXCoilType_Num,
                               NetCoolingCapRated(ns),
                               SEER_User * ConvFromSIToIP,
                               SEER_Standard * ConvFromSIToIP,
                               EER,
                               EER * ConvFromSIToIP,
                               IEER * ConvFromSIToIP,
                               NetHeatingCapRatedHighTemp,
                               NetHeatingCapRatedLowTemp,
                               HSPF * ConvFromSIToIP,
                               RegionNum,
                               false);

            // ANSI/AHRI 210/240 Std. 2023 Ratings
            // Writes the HSPF2 value to the EIO file and standard tabular output tables
            ReportDXCoilRating(state,
                               DXCoilType,
                               DXCoilName,
                               DXCoilType_Num,
                               NetCoolingCapRated(ns),
                               SEER_User * ConvFromSIToIP,
                               SEER_Standard * ConvFromSIToIP,
                               EER,
                               EER * ConvFromSIToIP,
                               IEER * ConvFromSIToIP,
                               NetHeatingCapRatedHighTemp_2023,
                               NetHeatingCapRatedLowTemp_2023,
                               HSPF2_2023 * ConvFromSIToIP,
                               RegionNum,
                               true);

            break;
        }
        default:
            break; //... other DX Coil types will follow here
        }
    }

    Real64 SingleSpeedHeatingHSPF(const Real64 NetHeatingCapRated,
                                  Optional_int_const RegionNum,
                                  const Real64 NetHeatingCapH3Test,
                                  const Real64 ElecPowerH3Test,
                                  const Real64 ElecPowerRated,
                                  const Real64 NetHeatingCapH2Test,
                                  const Real64 ElecPowerH2Test,
                                  Optional<Real64 const> MinOATCompressor,
                                  Optional_bool_const OATempCompressorOnOffBlank,
                                  Optional<Real64 const> OATempCompressorOn,
                                  Optional<const HPdefrostControl> DefrostControl)
    {
        int BinNum;                           // bin number counter
        Int64 StandardDHRNum;                 // Integer counter for standardized DHRs
        Real64 CheckCOP(0.0);                 // Checking COP at an outdoor bin temperature against unity [-]
        Real64 DesignHeatingRequirement(0.0); // The amount of heating required to maintain a given indoor temperature
        // at a particular outdoor design temperature.  [W]
        Real64 DesignHeatingRequirementMin(0.0); // minimum design heating requirement [W]
        Real64 FractionalBinHours(0.0);          // Fractional bin hours for the heating season  [-]
        Real64 BuildingLoad(0.0);                // Building space conditioning load corresponding to an outdoor bin temperature [W]

        Real64 HeatingModeLoadFactor(0.0); // Heating mode load factor corresponding to an outdoor bin temperature  [-]
        Real64 NetHeatingCapReduced(0.0);  // Net Heating Coil capacity corresponding to an outdoor bin temperature [W]
        Real64 TotalBuildingLoad(0.0);     // Sum of building load over the entire heating season [W]

        Real64 TotalElectricalEnergy(0.0); // Sum of electrical energy consumed by the heatpump over the heating season [W]
        Real64 DemandDeforstCredit(1.0);   // A factor to adjust HSPF if coil has demand defrost control  [-]

        Real64 ElectricalPowerConsumption(0.0);            // Electrical power corresponding to an outdoor bin temperature [W]
        Real64 HeatPumpElectricalEnergy(0.0);              // Heatpump electrical energy corresponding to an outdoor bin temperature [W]
        Real64 TotalHeatPumpElectricalEnergy(0.0);         // Sum of Heatpump electrical energy over the entire heating season [W]
        Real64 ResistiveSpaceHeatingElectricalEnergy(0.0); // resistance heating electrical energy corresponding to an
        // outdoor bin temperature [W]
        Real64 TotalResistiveSpaceHeatingElectricalEnergy(0.0); // Sum of resistance heating electrical energy over the
        // entire heating season [W]
        Real64 PartLoadFactor;
        Real64 LowTempCutOutFactor(0.0); // Factor which corresponds to compressor operation depending on outdoor temperature
        Real64 OATempCompressorOff(0.0); // Minimum outdoor air temperature to turn the commpressor off, [C]
        Real64 HSPF(0.0);

        if (RegionNum == 5) {
            DesignHeatingRequirementMin = NetHeatingCapRated;
        } else {
            DesignHeatingRequirementMin = NetHeatingCapRated * 1.8 * (18.33 - OutdoorDesignTemperature[static_cast<int64_t>(RegionNum) - 1]) / 60.0;
        }

        for (StandardDHRNum = 0; StandardDHRNum < TotalNumOfStandardDHRs - 1; ++StandardDHRNum) {
            if (StandardDesignHeatingRequirement[StandardDHRNum] <= DesignHeatingRequirementMin &&
                StandardDesignHeatingRequirement[StandardDHRNum + 1] >= DesignHeatingRequirementMin) {
                if ((DesignHeatingRequirementMin - StandardDesignHeatingRequirement[StandardDHRNum]) >
                    (StandardDesignHeatingRequirement[StandardDHRNum + 1] - DesignHeatingRequirementMin)) {
                    DesignHeatingRequirementMin = StandardDesignHeatingRequirement[StandardDHRNum + 1];
                } else {
                    DesignHeatingRequirementMin = StandardDesignHeatingRequirement[StandardDHRNum];
                }
            }
        }
        if (StandardDesignHeatingRequirement[0] >= DesignHeatingRequirementMin) {
            DesignHeatingRequirement = StandardDesignHeatingRequirement[0];
        } else if (StandardDesignHeatingRequirement[TotalNumOfStandardDHRs - 1] <= DesignHeatingRequirementMin) {
            DesignHeatingRequirement = StandardDesignHeatingRequirement[TotalNumOfStandardDHRs - 1];
        } else {
            DesignHeatingRequirement = DesignHeatingRequirementMin;
        }

        for (BinNum = 0; BinNum < TotalNumOfTemperatureBins[static_cast<int64_t>(RegionNum) - 1]; ++BinNum) {

            FractionalBinHours = FracBinHoursAtOutdoorBinTemp[static_cast<int64_t>(RegionNum) - 1][BinNum];

            BuildingLoad = (18.33 - OutdoorBinTemperature[BinNum]) / (18.33 - OutdoorDesignTemperature[static_cast<int64_t>(RegionNum) - 1]) *
                           CorrectionFactor * DesignHeatingRequirement;

            if ((OutdoorBinTemperature[BinNum] <= -8.33) || (OutdoorBinTemperature[BinNum] >= 7.22)) {
                NetHeatingCapReduced =
                    NetHeatingCapH3Test + (NetHeatingCapRated - NetHeatingCapH3Test) * (OutdoorBinTemperature[BinNum] + 8.33) / (16.67);
                ElectricalPowerConsumption = ElecPowerH3Test + (ElecPowerRated - ElecPowerH3Test) * (OutdoorBinTemperature[BinNum] + 8.33) / (16.67);
            } else {
                NetHeatingCapReduced =
                    NetHeatingCapH3Test + (NetHeatingCapH2Test - NetHeatingCapH3Test) * (OutdoorBinTemperature[BinNum] + 8.33) / (10.0);
                ElectricalPowerConsumption = ElecPowerH3Test + (ElecPowerH2Test - ElecPowerH3Test) * (OutdoorBinTemperature[BinNum] + 8.33) / (10.0);
            }

            if (NetHeatingCapReduced != 0.0) {
                HeatingModeLoadFactor = BuildingLoad / NetHeatingCapReduced;
            }

            if (HeatingModeLoadFactor > 1.0) {
                HeatingModeLoadFactor = 1.0;
            }

            PartLoadFactor = 1 - CyclicDegradationCoeff * (1 - HeatingModeLoadFactor);

            if (ElectricalPowerConsumption != 0.0) {
                CheckCOP = NetHeatingCapReduced / ElectricalPowerConsumption;
            }

            OATempCompressorOff = MinOATCompressor;

            if (CheckCOP < 1.0) {
                LowTempCutOutFactor = 0.0;
            } else {
                if (!OATempCompressorOnOffBlank) {
                    if (OutdoorBinTemperature[BinNum] <= OATempCompressorOff) {
                        LowTempCutOutFactor = 0.0;
                    } else if (OutdoorBinTemperature[BinNum] > OATempCompressorOff && OutdoorBinTemperature[BinNum] <= OATempCompressorOn) {
                        LowTempCutOutFactor = 0.5;
                    } else {
                        LowTempCutOutFactor = 1.0;
                    }
                } else {
                    LowTempCutOutFactor = 1.0;
                }
            }

            if (PartLoadFactor != 0.0) {
                HeatPumpElectricalEnergy =
                    (HeatingModeLoadFactor * ElectricalPowerConsumption * LowTempCutOutFactor) * FractionalBinHours / PartLoadFactor;
            }

            ResistiveSpaceHeatingElectricalEnergy =
                (BuildingLoad - HeatingModeLoadFactor * NetHeatingCapReduced * LowTempCutOutFactor) * FractionalBinHours;

            TotalBuildingLoad += (BuildingLoad * FractionalBinHours);

            TotalHeatPumpElectricalEnergy += HeatPumpElectricalEnergy;

            TotalResistiveSpaceHeatingElectricalEnergy += ResistiveSpaceHeatingElectricalEnergy;
        }

        TotalElectricalEnergy = TotalHeatPumpElectricalEnergy + TotalResistiveSpaceHeatingElectricalEnergy;

        if (DefrostControl == HPdefrostControl::Timed) {
            DemandDeforstCredit = 1.0; // Timed defrost control
        } else {
            DemandDeforstCredit = 1.03; // Demand defrost control
        }

        if (TotalElectricalEnergy != 0.0) {
            HSPF = TotalBuildingLoad * DemandDeforstCredit / TotalElectricalEnergy;
        }
        return HSPF;
    }

    Real64 SingleSpeedHeatingHSPF2(const Real64 NetHeatingCapRated_2023,
                                   Optional_int_const RegionNum,
                                   const Real64 NetHeatingCapH3Test_2023,
                                   const Real64 ElecPowerH3Test2023,
                                   const Real64 ElecPowerRated2023,
                                   const Real64 NetHeatingCapH2Test2023,
                                   const Real64 ElecPowerH2Test2023,
                                   Optional<Real64 const> MinOATCompressor,
                                   Optional_bool_const OATempCompressorOnOffBlank,
                                   Optional<Real64 const> OATempCompressorOn,
                                   Optional<const HPdefrostControl> DefrostControl)
    {
        Real64 DesignHeatingRequirement2023(0.0);   // HSPF2 minimum design heating requirement [W]
        int BinNum2023;                             // HSPF2 bin number counter
        Real64 FractionalBinHours2023(0.0);         // HSPF2 Fractional bin hours for the heating season  [-]
        Real64 BuildingLoad2023(0.0);               // HSPF2 Building space conditioning load corresponding to an outdoor bin temperature [W]
        Real64 NetHeatingCapReduced2023(0.0);       // HSPF2 Net Heating Coil capacity corresponding to an outdoor bin temperature [W]
        Real64 ElectricalPowerConsumption2023(0.0); // HSPF2 Electrical power corresponding to an outdoor bin temperature [W]
        Real64 HeatingModeLoadFactor2023(0.0);      // HSPF2 Heating mode load factor corresponding to an outdoor bin temperature  [-]
        Real64 PartLoadFactor2023;
        Real64 CheckCOP2023(0.0);                              // HSPF2 Checking COP at an outdoor bin temperature against unity [-]
        Real64 OATempCompressorOff2023(0.0);                   // HSPF2 Minimum outdoor air temperature to turn the commpressor off, [C]
        Real64 LowTempCutOutFactor2023(0.0);                   // Factor which corresponds to compressor operation depending on outdoor temperature
        Real64 HeatPumpElectricalEnergy2023(0.0);              // HSPF2 Heatpump electrical energy corresponding to an outdoor bin temperature [W]
        Real64 ResistiveSpaceHeatingElectricalEnergy2023(0.0); // HSPF2 resistance heating electrical energy corresponding to an
        // outdoor bin temperature [W]
        Real64 TotalBuildingLoad2023(0.0);                          // Sum of building load over the entire heating season [W]
        Real64 TotalHeatPumpElectricalEnergy2023(0.0);              // HSPF2 Sum of Heatpump electrical energy over the entire heating season [W]
        Real64 TotalResistiveSpaceHeatingElectricalEnergy2023(0.0); // Sum of resistance heating electrical energy over the
        // entire heating season [W]
        Real64 TotalElectricalEnergy2023(0.0); // HSPF2 Sum of electrical energy consumed by the heatpump over the heating season [W]
        Real64 DemandDeforstCredit2023(1.0);   // A factor to adjust HSPF2 if coil has demand defrost control  [-]
        Real64 HSPF2_2023(0.0);

        // For ANSI/AHRI 210/240 Standard 2023 | Concept of DHRI min and max is removed
        // Section 11.2.2.1 Equation 11.104  which suggests QAFull is used instead of DHRI min
        DesignHeatingRequirement2023 = NetHeatingCapRated_2023;
        Int64 RN = static_cast<int64_t>(RegionNum);

        for (BinNum2023 = 0; BinNum2023 < TotalNumOfTemperatureBinsHSPF2[RN - 1]; ++BinNum2023) {

            FractionalBinHours2023 = FracBinHoursAtOutdoorBinTempHSPF2[RN - 1][BinNum2023];

            BuildingLoad2023 = (ZoneLoadTemperature[RN - 1] - OutdoorBinTemperature[BinNum2023]) /
                               (ZoneLoadTemperature[RN - 1] - OutdoorDesignTemperature[RN - 1]) * SpeedLoadFactor[RN - 1] *
                               DesignHeatingRequirement2023;
            if ((OutdoorBinTemperature[BinNum2023] <= -8.33) || (OutdoorBinTemperature[BinNum2023] >= 7.22)) {
                NetHeatingCapReduced2023 = NetHeatingCapH3Test_2023 + (NetHeatingCapRated_2023 - NetHeatingCapH3Test_2023) *
                                                                          (OutdoorBinTemperature[BinNum2023] + 8.33) / (16.67);
                ElectricalPowerConsumption2023 =
                    ElecPowerH3Test2023 + (ElecPowerRated2023 - ElecPowerH3Test2023) * (OutdoorBinTemperature[BinNum2023] + 8.33) / (16.67);
            } else {
                NetHeatingCapReduced2023 = NetHeatingCapH3Test_2023 +
                                           (NetHeatingCapH2Test2023 - NetHeatingCapH3Test_2023) * (OutdoorBinTemperature[BinNum2023] + 8.33) / (10.0);
                ElectricalPowerConsumption2023 =
                    ElecPowerH3Test2023 + (ElecPowerH2Test2023 - ElecPowerH3Test2023) * (OutdoorBinTemperature[BinNum2023] + 8.33) / (10.0);
            }
            if (NetHeatingCapReduced2023 != 0.0) {
                HeatingModeLoadFactor2023 = BuildingLoad2023 / NetHeatingCapReduced2023;
            }

            if (HeatingModeLoadFactor2023 > 1.0) {
                HeatingModeLoadFactor2023 = 1.0;
            }
            PartLoadFactor2023 = 1 - CyclicHeatingDegradationCoeffHSPF2 * (1 - HeatingModeLoadFactor2023);
            if (ElectricalPowerConsumption2023 != 0.0) {
                CheckCOP2023 = NetHeatingCapReduced2023 / ElectricalPowerConsumption2023;
            }
            OATempCompressorOff2023 = MinOATCompressor;
            if (CheckCOP2023 < 1.0) {
                LowTempCutOutFactor2023 = 0.0;
            } else {
                if (!OATempCompressorOnOffBlank) {
                    if (OutdoorBinTemperature[BinNum2023] <= OATempCompressorOff2023) {
                        LowTempCutOutFactor2023 = 0.0;
                    } else if (OutdoorBinTemperature[BinNum2023] > OATempCompressorOff2023 &&
                               OutdoorBinTemperature[BinNum2023] <= OATempCompressorOn) {
                        LowTempCutOutFactor2023 = 0.5;
                    } else {
                        LowTempCutOutFactor2023 = 1.0;
                    }
                } else {
                    LowTempCutOutFactor2023 = 1.0;
                }
            }

            if (PartLoadFactor2023 != 0.0) {
                HeatPumpElectricalEnergy2023 = (HeatingModeLoadFactor2023 * ElectricalPowerConsumption2023 * LowTempCutOutFactor2023) *
                                               FractionalBinHours2023 / PartLoadFactor2023;
            }

            ResistiveSpaceHeatingElectricalEnergy2023 =
                (BuildingLoad2023 - HeatingModeLoadFactor2023 * NetHeatingCapReduced2023 * LowTempCutOutFactor2023) * FractionalBinHours2023;

            TotalBuildingLoad2023 += (BuildingLoad2023 * FractionalBinHours2023);

            TotalHeatPumpElectricalEnergy2023 += HeatPumpElectricalEnergy2023;

            TotalResistiveSpaceHeatingElectricalEnergy2023 += ResistiveSpaceHeatingElectricalEnergy2023;
        }
        TotalElectricalEnergy2023 = TotalHeatPumpElectricalEnergy2023 + TotalResistiveSpaceHeatingElectricalEnergy2023;

        if (DefrostControl == HPdefrostControl::Timed) {
            DemandDeforstCredit2023 = 1.0; // Timed defrost control
        } else {
            DemandDeforstCredit2023 = 1.03; // Demand defrost control
        }

        if (TotalElectricalEnergy2023 != 0.0) {
            HSPF2_2023 = TotalBuildingLoad2023 * DemandDeforstCredit2023 / TotalElectricalEnergy2023;
        }
        return HSPF2_2023;
    }

    std::map<std::string, Real64> SingleSpeedDXHeatingCoilStandardRatings(
        EnergyPlusData &state,
        Real64 const RatedTotalCapacity,                       // Reference capacity of DX coil [W]
        Real64 const RatedCOP,                                 // Reference coefficient of performance [W/W]
        int const CapFFlowCurveIndex,                          // Index for the capacity as a function of flow fraction modifier curve
        int const CapFTempCurveIndex,                          // Index for the capacity as a function of temperature modifier curve
        int const EIRFFlowCurveIndex,                          // Index for the EIR as a function of flow fraction modifier curve
        int const EIRFTempCurveIndex,                          // Index for the EIR as a function of temperature modifier curve
        Real64 const RatedAirVolFlowRate,                      // Rated air volume flow rate [m3/s]
        Real64 const FanPowerPerEvapAirFlowRateFromInput,      // 2017 Fan power per air volume flow rate [W/(m3/s)]
        Real64 const FanPowerPerEvapAirFlowRateFromInput_2023, // 2023 Fan power per air volume flow rate [W/(m3/s)]
        Optional_int_const RegionNum,                          // Region number for calculating HSPF of single speed DX heating coil
        Optional<Real64 const> MinOATCompressor,               // Minimum OAT for heat pump compressor operation [C]
        Optional<Real64 const> OATempCompressorOn,             // The outdoor temperature when the compressor is automatically turned
        Optional_bool_const OATempCompressorOnOffBlank,        // Flag used to determine low temperature cut out factor
        Optional<HPdefrostControl const> DefrostControl        // defrost control; 1=timed, 2=on-demand

    )
    {
        Real64 NetHeatingCapRated(0.0);  // Net Heating Coil capacity at Rated conditions,
        Real64 NetHeatingCapH3Test(0.0); // Net Heating Coil capacity at H3 test conditions
        Real64 HSPF(0.0);                // seasonale energy efficiency ratio of multi speed DX cooling coil

        // ANSI/AHRI 210/240 Standard 2023
        Real64 NetHeatingCapRated_2023(0.0);  // Net Heating Coil capacity at Rated conditions,
        Real64 NetHeatingCapH3Test_2023(0.0); // Net Heating Coil capacity at H3 test conditions
        Real64 HSPF2_2023(0.0);               // seasonale energy efficiency ratio of multi speed DX cooling coil
        std::map<std::string, Real64> StandardRatingsResults;
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Chandan Sharma
        //       DATE WRITTEN   February 2012
        //       MODIFIED       B Nigusse, December 2012
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // na

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // Using/Aliasing
        using CurveManager::CurveValue;
        using CurveManager::GetCurveMinMaxValues;
        using CurveManager::GetCurveName;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // back on, if applicable, following automatic shut off. This field is
        // used only for HSPF calculation. [C]
        // accounting for supply fan heat [W]
        // accounting for supply fan heat [W]

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na
        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 TotalHeatingCapRated(0.0);            // Heating Coil capacity at Rated conditions, without accounting supply fan heat [W]
        Real64 EIRRated(0.0);                        // EIR at Rated conditions [-]
        Real64 TotCapTempModFacRated(0.0);           // Total capacity as a function of temerature modifier at rated conditions [-]
        Real64 EIRTempModFacRated(0.0);              // EIR as a function of temerature modifier at rated conditions [-]
        Real64 TotalHeatingCapH2Test(0.0);           // Heating Coil capacity at H2 test conditions, without accounting supply fan heat [W]
        Real64 TotalHeatingCapH3Test(0.0);           // Heating Coil capacity at H3 test conditions, without accounting supply fan heat [W]
        Real64 CapTempModFacH2Test(0.0);             // Total capacity as a function of temerature modifier at H2 test conditions [-]
        Real64 EIRTempModFacH2Test(0.0);             // EIR as a function of temerature modifier at H2 test conditions [-]
        Real64 EIRH2Test(0.0);                       // EIR at H2 test conditions [-]
        Real64 CapTempModFacH3Test(0.0);             // Total capacity as a function of temerature modifier at H3 test conditions [-]
        Real64 EIRTempModFacH3Test(0.0);             // EIR as a function of temerature modifier at H3 test conditions [-]
        Real64 EIRH3Test(0.0);                       // EIR at H3 test conditions [-]
        Real64 TotCapFlowModFac(0.0);                // Total capacity modifier (function of actual supply air flow vs rated flow)
        Real64 EIRFlowModFac(0.0);                   // EIR modifier (function of actual supply air flow vs rated flow)
        Real64 FanPowerPerEvapAirFlowRate(0.0);      // 2017 Fan power per air volume flow rate [W/(m3/s)]
        Real64 FanPowerPerEvapAirFlowRate_2023(0.0); // 2023 Fan power per air volume flow rate [W/(m3/s)]

        Real64 ElecPowerRated;  // Total system power at Rated conditions accounting for supply fan heat [W]
        Real64 ElecPowerH2Test; // Total system power at H2 test conditions accounting for supply fan heat [W]
        Real64 ElecPowerH3Test; // Total system power at H3 test conditions accounting for supply fan heat [W]

        // For ANSI/AHRI 210/240 Standard 2023
        Real64 ElecPowerRated2023;  // Total system power at Rated conditions accounting for supply fan heat [W]
        Real64 ElecPowerH2Test2023; // Total system power at H2 test conditions accounting for supply fan heat [W]
        Real64 ElecPowerH3Test2023; // Total system power at H3 test conditions accounting for supply fan heat [W]

        Real64 NetHeatingCapH2Test;     // Net Heating Coil capacity at H2 test conditions accounting for supply fan heat [W]
        Real64 NetHeatingCapH2Test2023; // (for 2023 Standard) Net Heating Coil capacity at H2 test conditions accounting for supply fan heat [W]

        // Calculate the supply air fan electric power consumption.  The electric power consumption is estimated
        // using either user supplied or AHRI default value for fan power per air volume flow rate
        if (FanPowerPerEvapAirFlowRateFromInput <= 0.0) {
            FanPowerPerEvapAirFlowRate = DefaultFanPowerPerEvapAirFlowRate;
        } else {
            FanPowerPerEvapAirFlowRate = FanPowerPerEvapAirFlowRateFromInput;
        }
        if (FanPowerPerEvapAirFlowRateFromInput_2023 <= 0.0) {
            FanPowerPerEvapAirFlowRate_2023 = DefaultFanPowerPerEvapAirFlowRateSEER2;
        } else {
            FanPowerPerEvapAirFlowRate_2023 = FanPowerPerEvapAirFlowRateFromInput_2023;
        }

        TotCapFlowModFac = CurveValue(state, CapFFlowCurveIndex, AirMassFlowRatioRated);
        EIRFlowModFac = CurveValue(state, EIRFFlowCurveIndex, AirMassFlowRatioRated);

        {
            if (state.dataCurveManager->PerfCurve(CapFTempCurveIndex).NumDims == 1) {
                TotCapTempModFacRated = CurveValue(state, CapFTempCurveIndex, HeatingOutdoorCoilInletAirDBTempRated);

                CapTempModFacH2Test = CurveValue(state, CapFTempCurveIndex, HeatingOutdoorCoilInletAirDBTempH2Test);

                CapTempModFacH3Test = CurveValue(state, CapFTempCurveIndex, HeatingOutdoorCoilInletAirDBTempH3Test);
            } else {
                TotCapTempModFacRated =
                    CurveValue(state, CapFTempCurveIndex, HeatingIndoorCoilInletAirDBTempRated, HeatingOutdoorCoilInletAirDBTempRated);

                CapTempModFacH2Test =
                    CurveValue(state, CapFTempCurveIndex, HeatingIndoorCoilInletAirDBTempRated, HeatingOutdoorCoilInletAirDBTempH2Test);

                CapTempModFacH3Test =
                    CurveValue(state, CapFTempCurveIndex, HeatingIndoorCoilInletAirDBTempRated, HeatingOutdoorCoilInletAirDBTempH3Test);
            }
        }

        {
            if (state.dataCurveManager->PerfCurve(EIRFTempCurveIndex).NumDims == 1) {
                EIRTempModFacRated = CurveValue(state, EIRFTempCurveIndex, HeatingOutdoorCoilInletAirDBTempRated);

                EIRTempModFacH2Test = CurveValue(state, EIRFTempCurveIndex, HeatingOutdoorCoilInletAirDBTempH2Test);

                EIRTempModFacH3Test = CurveValue(state, EIRFTempCurveIndex, HeatingOutdoorCoilInletAirDBTempH3Test);
            } else {
                EIRTempModFacRated =
                    CurveValue(state, EIRFTempCurveIndex, HeatingIndoorCoilInletAirDBTempRated, HeatingOutdoorCoilInletAirDBTempRated);

                EIRTempModFacH2Test =
                    CurveValue(state, EIRFTempCurveIndex, HeatingIndoorCoilInletAirDBTempRated, HeatingOutdoorCoilInletAirDBTempH2Test);

                EIRTempModFacH3Test =
                    CurveValue(state, EIRFTempCurveIndex, HeatingIndoorCoilInletAirDBTempRated, HeatingOutdoorCoilInletAirDBTempH3Test);
            }
        }

        TotalHeatingCapRated = RatedTotalCapacity * TotCapTempModFacRated * TotCapFlowModFac;
        NetHeatingCapRated = TotalHeatingCapRated + FanPowerPerEvapAirFlowRate * RatedAirVolFlowRate;
        NetHeatingCapRated_2023 = TotalHeatingCapRated + FanPowerPerEvapAirFlowRate_2023 * RatedAirVolFlowRate;

        TotalHeatingCapH2Test = RatedTotalCapacity * CapTempModFacH2Test * TotCapFlowModFac;
        NetHeatingCapH2Test = TotalHeatingCapH2Test + FanPowerPerEvapAirFlowRate * RatedAirVolFlowRate;
        NetHeatingCapH2Test2023 = TotalHeatingCapH2Test + FanPowerPerEvapAirFlowRate_2023 * RatedAirVolFlowRate;

        TotalHeatingCapH3Test = RatedTotalCapacity * CapTempModFacH3Test * TotCapFlowModFac;
        NetHeatingCapH3Test = TotalHeatingCapH3Test + FanPowerPerEvapAirFlowRate * RatedAirVolFlowRate;
        NetHeatingCapH3Test_2023 = TotalHeatingCapH3Test + FanPowerPerEvapAirFlowRate_2023 * RatedAirVolFlowRate;

        // check curves value
        if (TotCapTempModFacRated < 0.0 || CapTempModFacH2Test < 0.0 || CapTempModFacH3Test < 0.0 || EIRTempModFacRated < 0.0 ||
            EIRTempModFacH2Test < 0.0 || EIRTempModFacH3Test < 0.0) {
            if (TotCapTempModFacRated < 0.0) {
                ShowSevereError(
                    state,
                    format(" Invalid Total Heating Capacity Function of Temperature Curve value = {:.2R}, Curve Type = {}, Curve Name = {}",
                           TotCapTempModFacRated,
                           state.dataCurveManager->PerfCurve(CapFTempCurveIndex).ObjectType,
                           GetCurveName(state, CapFTempCurveIndex)));
                ShowContinueError(state,
                                  " ...Net heating capacity at high temperature is set to zero. The curve value must be > 0. Check the curve.");
                NetHeatingCapRated = 0.0;
                NetHeatingCapRated_2023 = 0.0;
            }
            if (CapTempModFacH3Test < 0.0) {
                ShowSevereError(
                    state,
                    format(" Invalid Total Heating Capacity Function of Temperature Curve value = {:.2R}, Curve Type = {}, Curve Name = {}",
                           CapTempModFacH3Test,
                           state.dataCurveManager->PerfCurve(CapFTempCurveIndex).ObjectType,
                           GetCurveName(state, CapFTempCurveIndex)));
                ShowContinueError(state, " ...Net heating capacity at low temperature is set to zero. The curve value must be > 0. Check the curve.");
                NetHeatingCapH3Test = 0.0;
                NetHeatingCapH3Test_2023 = 0.0;
            }
            if (CapTempModFacH2Test < 0.0) {
                ShowSevereError(
                    state,
                    format(" Invalid Total Heating Capacity Function of Temperature Curve value = {:.2R}, Curve Type = {}, Curve Name = {}",
                           CapTempModFacH2Test,
                           state.dataCurveManager->PerfCurve(CapFTempCurveIndex).ObjectType,
                           GetCurveName(state, CapFTempCurveIndex)));
                ShowContinueError(state, " ...HSPF calculation is incorrect. The curve value must be > 0. Check the curve.");
                NetHeatingCapH3Test = 0.0;
                NetHeatingCapH3Test_2023 = 0.0;
            }
            // check EIR curve values
            if (EIRTempModFacRated < 0.0) {
                ShowSevereError(state,
                                format(" Invalid EIR Function of Temperature Curve value = {:.2R}, Curve Type = {}, Curve Name = {}",
                                       EIRTempModFacRated,
                                       state.dataCurveManager->PerfCurve(EIRFTempCurveIndex).ObjectType,
                                       GetCurveName(state, EIRFTempCurveIndex)));
                ShowContinueError(state, " ...HSPF calculation is incorrect. The curve value must be > 0. Check the curve.");
            }
            if (EIRTempModFacH2Test < 0.0) {
                ShowSevereError(state,
                                format(" Invalid EIR Function of Temperature Curve value = {:.2R}, Curve Type = {}, Curve Name = {}",
                                       EIRTempModFacH2Test,
                                       state.dataCurveManager->PerfCurve(EIRFTempCurveIndex).ObjectType,
                                       GetCurveName(state, EIRFTempCurveIndex)));
                ShowContinueError(state, " ...HSPF calculation is incorrect. The curve value must be > 0. Check the curve.");
            }
            if (EIRTempModFacH3Test < 0.0) {
                ShowSevereError(state,
                                format(" Invalid EIR Function of Temperature Curve value = {:.2R}, Curve Type = {}, Curve Name = {}",
                                       EIRTempModFacH3Test,
                                       state.dataCurveManager->PerfCurve(EIRFTempCurveIndex).ObjectType,
                                       GetCurveName(state, EIRFTempCurveIndex)));
                ShowContinueError(state, " ...HSPF calculation is incorrect. The curve value must be > 0. Check the curve.");
            }
            ShowContinueError(state, " ...HSPF value has been reset to 0.0 and simulation is continuing.");
            HSPF = 0.0;
            HSPF2_2023 = 0.0;

            StandardRatingsResults["NetHeatingCapRated"] = NetHeatingCapRated;
            StandardRatingsResults["NetHeatingCapH3Test"] = NetHeatingCapH3Test;
            StandardRatingsResults["HSPF"] = HSPF;
            StandardRatingsResults["NetHeatingCapRated_2023"] = NetHeatingCapRated_2023;
            StandardRatingsResults["NetHeatingCapH3Test_2023"] = NetHeatingCapH3Test_2023;
            StandardRatingsResults["HSPF2_2023"] = HSPF2_2023;
            return StandardRatingsResults;
        }

        if (RatedCOP > 0.0) { // RatedCOP <= 0.0 is trapped in GetInput, but keep this as "safety"

            EIRRated = EIRTempModFacRated * EIRFlowModFac / RatedCOP;
            EIRH2Test = EIRTempModFacH2Test * EIRFlowModFac / RatedCOP;
            EIRH3Test = EIRTempModFacH3Test * EIRFlowModFac / RatedCOP;
        }

        ElecPowerRated = EIRRated * TotalHeatingCapRated + FanPowerPerEvapAirFlowRate * RatedAirVolFlowRate;
        ElecPowerH2Test = EIRH2Test * TotalHeatingCapH2Test + FanPowerPerEvapAirFlowRate * RatedAirVolFlowRate;
        ElecPowerH3Test = EIRH3Test * TotalHeatingCapH3Test + FanPowerPerEvapAirFlowRate * RatedAirVolFlowRate;

        ElecPowerRated2023 = EIRRated * TotalHeatingCapRated + FanPowerPerEvapAirFlowRate_2023 * RatedAirVolFlowRate;
        ElecPowerH2Test2023 = EIRH2Test * TotalHeatingCapH2Test + FanPowerPerEvapAirFlowRate_2023 * RatedAirVolFlowRate;
        ElecPowerH3Test2023 = EIRH3Test * TotalHeatingCapH3Test + FanPowerPerEvapAirFlowRate_2023 * RatedAirVolFlowRate;

        HSPF2_2023 = SingleSpeedHeatingHSPF2(NetHeatingCapRated_2023,
                                             RegionNum,
                                             NetHeatingCapH3Test_2023,
                                             ElecPowerH3Test2023,
                                             ElecPowerRated2023,
                                             NetHeatingCapH2Test2023,
                                             ElecPowerH2Test2023,
                                             MinOATCompressor,
                                             OATempCompressorOnOffBlank,
                                             OATempCompressorOn,
                                             DefrostControl);

        HSPF = SingleSpeedHeatingHSPF(NetHeatingCapRated,
                                      RegionNum,
                                      NetHeatingCapH3Test,
                                      ElecPowerH3Test,
                                      ElecPowerRated,
                                      NetHeatingCapH2Test,
                                      ElecPowerH2Test,
                                      MinOATCompressor,
                                      OATempCompressorOnOffBlank,
                                      OATempCompressorOn,
                                      DefrostControl);

        StandardRatingsResults["NetHeatingCapRated"] = NetHeatingCapRated;
        StandardRatingsResults["NetHeatingCapH3Test"] = NetHeatingCapH3Test;
        StandardRatingsResults["HSPF"] = HSPF;
        StandardRatingsResults["NetHeatingCapRated_2023"] = NetHeatingCapRated_2023;
        StandardRatingsResults["NetHeatingCapH3Test_2023"] = NetHeatingCapH3Test_2023;
        StandardRatingsResults["HSPF2_2023"] = HSPF2_2023;
        return StandardRatingsResults;
    }

    std::tuple<Real64, Real64> IEERSingleSpeedCooling(EnergyPlus::EnergyPlusData &state,
                                                      const int CapFTempCurveIndex,
                                                      const Real64 RatedTotalCapacity,
                                                      const Real64 TotCapFlowModFac,
                                                      const Real64 FanPowerPerEvapAirFlowRate,
                                                      const Real64 RatedAirVolFlowRate,
                                                      const int EIRFTempCurveIndex,
                                                      const Real64 RatedCOP,
                                                      const Real64 EIRFlowModFac)
    {
        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int constexpr NumOfReducedCap(4);                  // Number of reduced capacity test conditions (100%,75%,50%,and 25%)
        Real64 TotCapTempModFac(0.0);                      // Total capacity modifier (function of entering wetbulb, outside drybulb) [-]
        Real64 OutdoorUnitInletAirDryBulbTempReduced(0.0); // Outdoor unit entering air dry-bulb temperature at reduced capacity [C]
        Real64 NetCoolingCapReduced(0.0);                  // Net Cooling Coil capacity at reduced conditions, accounting for supply fan heat [W]
        Real64 EIRTempModFac(0.0);                         // EIR modifier (function of entering wetbulb, outside drybulb) [-]
        Real64 EIR(0.0);                                   // Energy Efficiency Ratio at AHRI test conditions for SEER [-]
        Real64 LoadFactor(0.0);                            // Fractional "on" time for last stage at the desired reduced capacity, (dimensionless)
        Real64 DegradationCoeff(0.0);                      // Degradation coeficient, (dimenssionless)
        Real64 ElecPowerReducedCap(0.0);                   // Net power consumption (Cond Fan+Compressor) at reduced test condition [W]
        Real64 EERReduced(0.0);                            // EER at reduced capacity test conditions (100%, 75%, 50%, and 25%)
        Real64 IEER = 0.0;                                 // Integareted energy efficiency ratio of single speed DX cooling coil
        Real64 NetCoolingCapRated = 0.0;                   // net cooling capacity of single speed DX cooling coil

        // Calculate the net cooling capacity at the rated conditions (19.44C WB and 35.0C DB )
        TotCapTempModFac =
            CurveManager::CurveValue(state, CapFTempCurveIndex, CoolingCoilInletAirWetBulbTempRated, OutdoorUnitInletAirDryBulbTempRated);
        NetCoolingCapRated = RatedTotalCapacity * TotCapTempModFac * TotCapFlowModFac - FanPowerPerEvapAirFlowRate * RatedAirVolFlowRate;
        // RedCapNum : Integer counter for reduced capacity
        for (int RedCapNum = 0; RedCapNum < NumOfReducedCap; ++RedCapNum) {
            // get the outdoor air dry bulb temperature for the reduced capacity test conditions
            if (ReducedPLR[RedCapNum] > 0.444) {
                OutdoorUnitInletAirDryBulbTempReduced = 5.0 + 30.0 * ReducedPLR[RedCapNum];
            } else {
                OutdoorUnitInletAirDryBulbTempReduced = OADBTempLowReducedCapacityTest;
            }
            TotCapTempModFac =
                CurveManager::CurveValue(state, CapFTempCurveIndex, CoolingCoilInletAirWetBulbTempRated, OutdoorUnitInletAirDryBulbTempReduced);
            NetCoolingCapReduced = RatedTotalCapacity * TotCapTempModFac * TotCapFlowModFac - FanPowerPerEvapAirFlowRate * RatedAirVolFlowRate;
            EIRTempModFac =
                CurveManager::CurveValue(state, EIRFTempCurveIndex, CoolingCoilInletAirWetBulbTempRated, OutdoorUnitInletAirDryBulbTempReduced);
            if (RatedCOP > 0.0) {
                EIR = EIRTempModFac * EIRFlowModFac / RatedCOP;
            } else {
                EIR = 0.0;
            }
            if (NetCoolingCapReduced > 0.0) {
                LoadFactor = ReducedPLR[RedCapNum] * NetCoolingCapRated / NetCoolingCapReduced;
            } else {
                LoadFactor = 1.0;
            }
            DegradationCoeff = 1.130 - 0.130 * LoadFactor;
            ElecPowerReducedCap = DegradationCoeff * EIR * (RatedTotalCapacity * TotCapTempModFac * TotCapFlowModFac);
            EERReduced = (LoadFactor * NetCoolingCapReduced) / (LoadFactor * ElecPowerReducedCap + FanPowerPerEvapAirFlowRate * RatedAirVolFlowRate);
            IEER += IEERWeightingFactor[RedCapNum] * EERReduced;
        }
        return std::make_tuple(IEER, NetCoolingCapRated);
    }

    Real64 EERSingleSpeedCooling(EnergyPlus::EnergyPlusData &state,
                                 const int CapFTempCurveIndex,
                                 const Real64 RatedTotalCapacity,
                                 const Real64 TotCapFlowModFac,
                                 const Real64 FanPowerPerEvapAirFlowRate,
                                 const Real64 RatedAirVolFlowRate,
                                 const int EIRFTempCurveIndex,
                                 const Real64 RatedCOP,
                                 const Real64 EIRFlowModFac)
    {
        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 EER = 0.0;                // energy efficiency ratio of single speed DX cooling coil
        Real64 TotCapTempModFac(0.0);    // Total capacity modifier (function of entering wetbulb, outside drybulb) [-]
        Real64 NetCoolingCapRated(0.0);  // net cooling capacity of single speed DX cooling coil
        Real64 EIRTempModFac(0.0);       // EIR modifier (function of entering wetbulb, outside drybulb) [-]
        Real64 EIR(0.0);                 // Energy Efficiency Ratio at AHRI test conditions for SEER [-]
        Real64 TotalElecPowerRated(0.0); // Net power consumption (Cond Fan+Compressor+Indoor Fan) at Rated test conditions [W]
        // Calculate the net cooling capacity at the rated conditions (19.44C WB and 35.0C DB )
        TotCapTempModFac =
            CurveManager::CurveValue(state, CapFTempCurveIndex, CoolingCoilInletAirWetBulbTempRated, OutdoorUnitInletAirDryBulbTempRated);
        NetCoolingCapRated = RatedTotalCapacity * TotCapTempModFac * TotCapFlowModFac - FanPowerPerEvapAirFlowRate * RatedAirVolFlowRate;
        // Calculate Energy Efficiency Ratio (EER) at (19.44C WB and 35.0C DB ), ANSI/AHRI Std. 340/360
        EIRTempModFac = CurveManager::CurveValue(state, EIRFTempCurveIndex, CoolingCoilInletAirWetBulbTempRated, OutdoorUnitInletAirDryBulbTempRated);
        if (RatedCOP > 0.0) {
            // RatedCOP <= 0.0 is trapped in GetInput, but keep this as "safety"
            EIR = EIRTempModFac * EIRFlowModFac / RatedCOP;
        } else {
            EIR = 0.0;
        }
        TotalElecPowerRated = EIR * (RatedTotalCapacity * TotCapTempModFac * TotCapFlowModFac) + FanPowerPerEvapAirFlowRate * RatedAirVolFlowRate;
        if (TotalElecPowerRated > 0.0) {
            EER = NetCoolingCapRated / TotalElecPowerRated;
        } else {
            EER = 0.0;
        }
        return EER;
    }

    std::tuple<Real64, Real64> SEERSingleStageCalculation(EnergyPlusData &state,
                                                          int CapFTempCurveIndex,
                                                          Real64 RatedTotalCapacity,
                                                          Real64 TotCapFlowModFac,
                                                          int EIRFTempCurveIndex,
                                                          Real64 EIRFlowModFac,
                                                          [[maybe_unused]] int EIRFFlowCurveIndex,
                                                          Real64 RatedCOP,
                                                          Real64 FanPowerPerEvapAirFlowRate,
                                                          Real64 RatedAirVolFlowRate,
                                                          int PLFFPLRCurveIndex,
                                                          Real64 const CyclicDegradationCoefficient)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Nigusse, FSEC
        //       DATE WRITTEN   December 2012
        //       MODIFIED
        //       RE-ENGINEERED  Brijendra Singh

        // PURPOSE OF THIS SUBROUTINE:
        // Calculates the SEER values for single speed based on AHRI 210/230 2017 & 2023

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        // SEER calculations:
        Real64 TotCapTempModFac(0.0);  // Total capacity modifier (function of entering wetbulb, outside drybulb) [-]
        Real64 TotCoolingCapAHRI(0.0); // Total Cooling Coil capacity (gross) at AHRI test conditions [W]
        Real64 EIRTempModFac(0.0);     // EIR modifier (function of entering wetbulb, outside drybulb) [-]
        Real64 NetCoolingCapAHRI(0.0); // Net Cooling Coil capacity at AHRI TestB conditions, accounting for fan heat [W]
        Real64 EIR(0.0);               // Energy Efficiency Ratio at AHRI test conditions for SEER [-]
        Real64 TotalElecPower(0.0);    // Net power consumption (Cond Fan+Compressor+Indoor Fan) at AHRI test conditions [W]
        Real64 PartLoadFactorUser(
            0.0); // Part load factor based on user-input PLF curve and C_D value that accounts for thermal lag at compressor startup [-]
        Real64 PartLoadFactorStandard(
            0.0); // part-load factor that accounts for the cyclic degradation from AHRI Standard 210/240-2008 default PLF curve and C_D value, [-]
        Real64 SEER_User = 0.0;     // seasonal energy efficiency ratio of single speed DX cooling coil, from user-input PLF curve and C_D value
        Real64 SEER_Standard = 0.0; // seasonal energy efficiency ratio of single speed DX cooling coil, from user-input PLF curve and C_D value

        TotCapTempModFac = CurveManager::CurveValue(state, CapFTempCurveIndex, CoolingCoilInletAirWetBulbTempRated, OutdoorUnitInletAirDryBulbTemp);
        TotCoolingCapAHRI = RatedTotalCapacity * TotCapTempModFac * TotCapFlowModFac;
        EIRTempModFac = CurveManager::CurveValue(state, EIRFTempCurveIndex, CoolingCoilInletAirWetBulbTempRated, OutdoorUnitInletAirDryBulbTemp);
        EIR = (RatedCOP > 0.0) ? EIRTempModFac * EIRFlowModFac / RatedCOP : 0.0;

        // Calculate net cooling capacity
        NetCoolingCapAHRI = TotCoolingCapAHRI - FanPowerPerEvapAirFlowRate * RatedAirVolFlowRate;
        TotalElecPower = EIR * TotCoolingCapAHRI + FanPowerPerEvapAirFlowRate * RatedAirVolFlowRate;
        // Calculate SEER value from the Energy Efficiency Ratio (EER) at the AHRI test conditions and the part load factor.
        // First evaluate the Part Load Factor curve at PLR = 0.5 (AHRI Standard 210/240)
        PartLoadFactorUser = CurveManager::CurveValue(state, PLFFPLRCurveIndex, PLRforSEER);
        PartLoadFactorStandard = 1.0 - (1 - PLRforSEER) * CyclicDegradationCoefficient;

        if (TotalElecPower > 0.0) {
            SEER_User = (NetCoolingCapAHRI / TotalElecPower) * PartLoadFactorUser;
            SEER_Standard = (NetCoolingCapAHRI / TotalElecPower) * PartLoadFactorStandard;
        }
        return std::make_tuple(SEER_User, SEER_Standard);
    }

    std::map<std::string, Real64> SingleSpeedDXCoolingCoilStandardRatings(
        EnergyPlusData &state,
        std::string const &DXCoilName,                        // Name of DX coil for which HSPF is calculated
        std::string const &DXCoilType,                        // Type of DX coil - heating or cooling
        int const CapFTempCurveIndex,                         // Index for the capacity as a function of temperature modifier curve
        int const CapFFlowCurveIndex,                         // Index for the capacity as a function of flow fraction modifier curve
        int const EIRFTempCurveIndex,                         // Index for the EIR as a function of temperature modifier curve
        int const EIRFFlowCurveIndex,                         // Index for the EIR as a function of flow fraction modifier curve
        int const PLFFPLRCurveIndex,                          // Index for the EIR vs part-load ratio curve
        Real64 const RatedTotalCapacity,                      // Rated gross total cooling capacity
        Real64 const RatedCOP,                                // Rated gross COP
        Real64 const RatedAirVolFlowRate,                     // air flow rate through the coil at rated condition
        Real64 const FanPowerPerEvapAirFlowRateFromInput,     // 2017 Fan power per air volume flow rate through the evaporator coil
        Real64 const FanPowerPerEvapAirFlowRateFromInput_2023 // 2023 Fan power per air volume flow rate through the evaportor coil
    )
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Nigusse, FSEC
        //       DATE WRITTEN   December 2012
        //       MODIFIED
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Calculates the standard ratings net cooling capacity and, SEER, EER and IEER values for single speed
        // DX cooling coils at the AHRI standard test condition(s).

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // Using/Aliasing
        using CurveManager::CurveValue;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 TotCapFlowModFac(0.0);           // Total capacity modifier f(actual flow vs rated flow) for each speed [-]
        Real64 EIRFlowModFac(0.0);              // EIR modifier f(actual supply air flow vs rated flow) for each speed [-]
        Real64 TotCapTempModFac(0.0);           // Total capacity modifier (function of entering wetbulb, outside drybulb) [-]
        Real64 FanPowerPerEvapAirFlowRate;      // 2017 Fan power per air volume flow rate through the evaporator coil [W/(m3/s)]
        Real64 FanPowerPerEvapAirFlowRate_2023; // 2023 Fan power per air volume flow rate through the evaporator coil [W/(m3/s)]

        Real64 NetCoolingCapRated(0.0); // net cooling capacity of single speed DX cooling coil
        Real64 SEER_User(0.0);          // seasonal energy efficiency ratio of single speed DX cooling coil, from user-input PLF curve and C_D value
        Real64 SEER_Standard(0.0);      // seasonal energy efficiency ratio of single speed DX cooling coil, from user-input PLF curve and C_D value
        Real64 EER(0.0);                // energy efficiency ratio of single speed DX cooling coil
        Real64 IEER(0.0);               // Integareted energy efficiency ratio of single speed DX cooling coil

        Real64 NetCoolingCapRated2023(0.0); // net cooling capacity of single speed DX cooling coil
        Real64 SEER2_User(0.0);     // seasonal energy efficiency ratio of single speed DX cooling coil, from user-input PLF curve and C_D value
        Real64 SEER2_Standard(0.0); // seasonal energy efficiency ratio of single speed DX cooling coil, from user-input PLF curve and C_D value
        Real64 EER_2023(0.0);       // energy efficiency ratio of single speed DX cooling coil
                                    //        Real64 IEER_2023(0.0);      // Integareted energy efficiency ratio of single speed DX cooling coil

        std::map<std::string, Real64> StandarRatingResults;
        // StandarRatingResults["NetCoolingCapRated"] = NetCoolingCapRated;
        // StandarRatingResults["SEER_User"] = SEER_User;
        // StandarRatingResults["SEER_Standard"] = SEER_Standard;
        // StandarRatingResults["EER"] = EER;
        // StandarRatingResults["IEER"] = IEER;
        // StandarRatingResults["NetCoolingCapRated2023"] = NetCoolingCapRated2023;
        // StandarRatingResults["SEER2_User"] = SEER2_User;
        // StandarRatingResults["SEER2_Standard"] = SEER2_Standard;
        // StandarRatingResults["EER_2023"] = EER_2023;
        // StandarRatingResults["IEER_2023"] = IEER_2023;

        if (FanPowerPerEvapAirFlowRateFromInput <= 0.0) {
            FanPowerPerEvapAirFlowRate = DefaultFanPowerPerEvapAirFlowRate;
        } else {
            FanPowerPerEvapAirFlowRate = FanPowerPerEvapAirFlowRateFromInput;
        }

        if (FanPowerPerEvapAirFlowRateFromInput_2023 <= 0.0) {
            FanPowerPerEvapAirFlowRate_2023 = DefaultFanPowerPerEvapAirFlowRateSEER2;
        } else {
            FanPowerPerEvapAirFlowRate_2023 = FanPowerPerEvapAirFlowRateFromInput_2023;
        }

        if (RatedTotalCapacity > 0.0) {

            // Standard Rating Cooling (net) Capacity calculations:
            TotCapFlowModFac = CurveValue(state, CapFFlowCurveIndex, AirMassFlowRatioRated);
            TotCapTempModFac = CurveValue(state, CapFTempCurveIndex, CoolingCoilInletAirWetBulbTempRated, OutdoorUnitInletAirDryBulbTempRated);
            NetCoolingCapRated = RatedTotalCapacity * TotCapTempModFac * TotCapFlowModFac - FanPowerPerEvapAirFlowRate * RatedAirVolFlowRate;
            NetCoolingCapRated2023 = RatedTotalCapacity * TotCapTempModFac * TotCapFlowModFac - FanPowerPerEvapAirFlowRate_2023 * RatedAirVolFlowRate;
            EIRFlowModFac = CurveManager::CurveValue(state, EIRFFlowCurveIndex, AirMassFlowRatioRated);

            std::tie(SEER_User, SEER_Standard) = SEERSingleStageCalculation(state,
                                                                            CapFTempCurveIndex,
                                                                            RatedTotalCapacity,
                                                                            TotCapFlowModFac,
                                                                            EIRFTempCurveIndex,
                                                                            EIRFlowModFac,
                                                                            EIRFFlowCurveIndex,
                                                                            RatedCOP,
                                                                            FanPowerPerEvapAirFlowRate,
                                                                            RatedAirVolFlowRate,
                                                                            PLFFPLRCurveIndex,
                                                                            CyclicDegradationCoeff);
            StandarRatingResults["SEER_User"] = SEER_User;
            StandarRatingResults["SEER_Standard"] = SEER_Standard;

            std::tie(SEER2_User, SEER2_Standard) = SEERSingleStageCalculation(state,
                                                                              CapFTempCurveIndex,
                                                                              RatedTotalCapacity,
                                                                              TotCapFlowModFac,
                                                                              EIRFTempCurveIndex,
                                                                              EIRFlowModFac,
                                                                              EIRFFlowCurveIndex,
                                                                              RatedCOP,
                                                                              FanPowerPerEvapAirFlowRate_2023,
                                                                              RatedAirVolFlowRate,
                                                                              PLFFPLRCurveIndex,
                                                                              CyclicDegradationCoeffSEER2);
            StandarRatingResults["SEER2_User"] = SEER2_User;
            StandarRatingResults["SEER2_Standard"] = SEER2_Standard;

            // EER calculations:
            EER = EERSingleSpeedCooling(state,
                                        CapFTempCurveIndex,
                                        RatedTotalCapacity,
                                        TotCapFlowModFac,
                                        FanPowerPerEvapAirFlowRate,
                                        RatedAirVolFlowRate,
                                        EIRFTempCurveIndex,
                                        RatedCOP,
                                        EIRFlowModFac);
            StandarRatingResults["EER"] = EER;

            // EER2 calculations:
            EER_2023 = EERSingleSpeedCooling(state,
                                             CapFTempCurveIndex,
                                             RatedTotalCapacity,
                                             TotCapFlowModFac,
                                             FanPowerPerEvapAirFlowRate_2023,
                                             RatedAirVolFlowRate,
                                             EIRFTempCurveIndex,
                                             RatedCOP,
                                             EIRFlowModFac);
            StandarRatingResults["EER_2023"] = EER_2023;

            // IEER calculations
            std::tie(IEER, NetCoolingCapRated) = IEERSingleSpeedCooling(state,
                                                                        CapFTempCurveIndex,
                                                                        RatedTotalCapacity,
                                                                        TotCapFlowModFac,
                                                                        FanPowerPerEvapAirFlowRate,
                                                                        RatedAirVolFlowRate,
                                                                        EIRFTempCurveIndex,
                                                                        RatedCOP,
                                                                        EIRFlowModFac);
            StandarRatingResults["IEER"] = IEER;
            StandarRatingResults["NetCoolingCapRated"] = NetCoolingCapRated;

            // IEER2 Calculations are deprecated in AHRI 2023 Std.
            // std::tie(IEER_2023, NetCoolingCapRated2023) = IEERSingleSpeedCooling(state,
            //                                                                     CapFTempCurveIndex,
            //                                                                     RatedTotalCapacity,
            //                                                                     TotCapFlowModFac,
            //                                                                     FanPowerPerEvapAirFlowRate_2023,
            //                                                                     RatedAirVolFlowRate,
            //                                                                     EIRFTempCurveIndex,
            //                                                                     RatedCOP,
            //                                                                     EIRFlowModFac);
            // StandarRatingResults["IEER_2023"] = IEER_2023;
            StandarRatingResults["NetCoolingCapRated2023"] = NetCoolingCapRated2023;

        } else {
            ShowSevereError(state,
                            "Standard Ratings: " + DXCoilType + ' ' + DXCoilName +
                                " has zero rated total cooling capacity. Standard ratings cannot be calculated.");
        }
        return StandarRatingResults;
    }

    void DXCoolingCoilDataCenterStandardRatings(
        EnergyPlusData &state,
        std::string const &DXCoilName,                    // Name of DX coil for which HSPF is calculated
        std::string const &DXCoilType,                    // Type of DX coil - heating or cooling
        int const CapFTempCurveIndex,                     // Index for the capacity as a function of temperature modifier curve
        int const CapFFlowCurveIndex,                     // Index for the capacity as a function of flow fraction modifier curve
        int const EIRFTempCurveIndex,                     // Index for the EIR as a function of temperature modifier curve
        int const EIRFFlowCurveIndex,                     // Index for the EIR as a function of flow fraction modifier curve
        [[maybe_unused]] int const PLFFPLRCurveIndex,     // Index for the EIR vs part-load ratio curve
        Real64 const RatedTotalCapacity,                  // Rated gross total cooling capacity
        Real64 const RatedCOP,                            // Rated gross COP
        Real64 const RatedAirVolFlowRate,                 // air flow rate through the coil at rated condition
        Real64 const FanPowerPerEvapAirFlowRateFromInput, // Fan power per air volume flow rate through the evaporator coil
        Array1D<Real64> &NetCoolingCapRated,              // net cooling capacity of single speed DX cooling coil
        Array1D<Real64> &TotElectricPowerRated            // total electric power including supply fan
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Nigusse, FSEC
        //       DATE WRITTEN   October 2014
        //       MODIFIED
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Calculates the standard ratings net cooling capacity and total electric power
        // for room unitary air conditioners single speed DX cooling coils.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // ANSI/ASHRAE Standard 127-2012 - Method of Testing for Rating Computer and Data Processing
        //                                 Room Unitary Air Conditioners

        // Using/Aliasing
        using CurveManager::CurveValue;
        using Psychrometrics::PsyTwbFnTdbWPb;
        using Psychrometrics::PsyWFnTdpPb;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 TotCapFlowModFac(0.0);      // Total capacity modifier f(actual flow vs rated flow) for each speed [-]
        Real64 EIRFlowModFac(0.0);         // EIR modifier f(actual supply air flow vs rated flow) for each speed [-]
        Real64 TotCapTempModFac(0.0);      // Total capacity modifier (function of entering wetbulb, outside drybulb) [-]
        Real64 EIRTempModFac(0.0);         // EIR modifier (function of entering wetbulb, outside drybulb) [-]
        Real64 TotCoolingCapRated(0.0);    // Total Cooling Coil capacity (gross) at one of the rated test conditions [W]
        Real64 EIR(0.0);                   // Energy Efficiency Ratio at AHRI test conditions for SEER [-]
        Real64 FanPowerPerEvapAirFlowRate; // Fan power per air volume flow rate through the evaporator coil [W/(m3/s)]

        Real64 TWBIndoor;  // indoor air dry bulb temperature
        Real64 TDBOutdoor; // outdor air dry bulb temperature
        int ClassNum;      // class number (Class I, II, II, IV)
        int TestNum;       // test number (Test A, B, C, D)
        int Num;           // text number counter

        if (FanPowerPerEvapAirFlowRateFromInput <= 0.0) {
            FanPowerPerEvapAirFlowRate = DefaultFanPowerPerEvapAirFlowRate;
        } else {
            FanPowerPerEvapAirFlowRate = FanPowerPerEvapAirFlowRateFromInput;
        }
        if (RatedTotalCapacity > 0.0) {

            for (ClassNum = 1; ClassNum <= 4; ++ClassNum) {
                TWBIndoor = PsyTwbFnTdbWPb(state,
                                           IndoorDBTempClassI2IV[ClassNum - 1],
                                           PsyWFnTdpPb(state, IndoorTDPA2D, state.dataEnvrn->StdBaroPress),
                                           state.dataEnvrn->StdBaroPress);
                for (TestNum = 1; TestNum <= 4; ++TestNum) {
                    TDBOutdoor = OutdoorDBTempAllClassA2D[TestNum - 1];
                    Num = (ClassNum - 1) * 4 + TestNum;
                    // Standard Rating Net Cooling Capacity at Test A:
                    TotCapFlowModFac = CurveValue(state, CapFFlowCurveIndex, AirMassFlowRatioRated);
                    TotCapTempModFac = CurveValue(state, CapFTempCurveIndex, TWBIndoor, TDBOutdoor);
                    TotCoolingCapRated = RatedTotalCapacity * TotCapTempModFac * TotCapFlowModFac;
                    NetCoolingCapRated(Num) = TotCoolingCapRated - FanPowerPerEvapAirFlowRate * RatedAirVolFlowRate;
                    // Standard Rating total electric power at Test A:
                    EIRTempModFac = CurveValue(state, EIRFTempCurveIndex, TWBIndoor, TDBOutdoor);
                    EIRFlowModFac = CurveValue(state, EIRFFlowCurveIndex, AirMassFlowRatioRated);
                    EIR = 0.0;
                    if (RatedCOP > 0.0) {
                        EIR = EIRTempModFac * EIRFlowModFac / RatedCOP;
                    }
                    // Calculate net cooling capacity at Test A:
                    TotElectricPowerRated(Num) = EIR * TotCoolingCapRated + FanPowerPerEvapAirFlowRate * RatedAirVolFlowRate;
                }
            }
        } else {
            ShowSevereError(state,
                            "Standard Ratings: " + DXCoilType + ' ' + DXCoilName +
                                " has zero rated total cooling capacity. Capacity and Power cannot be calculated.");
        }
    }

    std::tuple<Real64, Real64, Real64> MultiSpeedDXCoolingCoilSEER(EnergyPlusData &state,
                                                                   int const nsp,
                                                                   Array1A_int const CapFFlowCurveIndex,
                                                                   Array1A<Real64> const RatedTotalCapacity,
                                                                   Array1A_int const CapFTempCurveIndex,
                                                                   Array1A<Real64> const FanPowerPerEvapAirFlowRateFromInput,
                                                                   Array1A<Real64> const RatedAirVolFlowRate,
                                                                   Array1A_int const EIRFFlowCurveIndex,
                                                                   Array1A<Real64> const RatedCOP,
                                                                   Array1A_int EIRFTempCurveIndex,
                                                                   Array1A_int const PLFFPLRCurveIndex)
    {
        // Intermediate values calculated from the inputs in the idf file
        Array1D<Real64> FanPowerPerEvapAirFlowRate(nsp); // 2017 Fan power per air volume flow rate through the evaporator coil [W/(m3/s)]
        Array1D<Real64> TotCoolCapTestA2(nsp);           // Total cooling capacity at A2 test condition (High speed)
        Array1D<Real64> TotCoolCapTestB2(nsp);           // Total cooling capacity at B2 test condition (High speed)
        Array1D<Real64> TotCoolCapTestB1(nsp);           // Total cooling capacity at B1 test condition (Low speed)
        Array1D<Real64> TotCoolCapTestF1(nsp);           // Total cooling capacity at F1 test condition (Low speed)
        Array1D<Real64> OutdoorUnitPowerTestA2(nsp);     // Outdoor Unit electric power at A2 test condition (High speed)
        Array1D<Real64> OutdoorUnitPowerTestB2(nsp);     // Outdoor Unit electric power at B2 test condition (High speed)
        Array1D<Real64> OutdoorUnitPowerTestB1(nsp);     // Outdoor Unit electric power at B1 test condition (Low speed)
        Array1D<Real64> OutdoorUnitPowerTestF1(nsp);     // Outdoor Unit electric power at F1 test condition

        Real64 PartLoadRatio(0.0);      // compressor cycling ratio between successive speeds, [-]
        Real64 PartLoadFactorUser(0.0); // part-load factor based on user-input PLF curve and C_D value that accounts for the cyclic degradation, [-]
        Real64 PartLoadFactorStandard(
            0.0); // part-load factorn that accounts for the cyclic degradation from AHRI standard 210/240 default PLF curve and C_D value, [-]
        Real64 NetCoolingCapWeighted(0.0);              // net tot cooling cap weighted by the fraction of the binned cooling hours [W]
        Real64 TotCoolingElecPowerWeighted(0.0);        // net total cooling electric power input weighted by the fraction of the temperature bins
        Real64 TotCoolingElecPowerWeightedDefault(0.0); // net total cooling electric power input weighted by the fraction of the temperature bins
                                                        // from AHRI 201/240 default PLF curve and C_D value,

        // binned cooling hours
        Real64 BuildingCoolingLoad(0.0);    // Building space cooling load corresponding to an outdoor bin temperature [W]
        Real64 NetTotCoolCapBinned(0.0);    // Net tot cooling cap corresponding to an outdoor bin temperature [W]
        Real64 TotCoolElecPowerBinned(0.0); // Total cooling electric power corresponding to an outdoor bin temperature [W]
        Real64 TotCoolElecPowerBinnedDefault(
            0.0); // Total cooling electric power corresponding to an outdoor bin temperature from AHRI 201/240 default PLF curve and C_D value, [W]
        Real64 LoadFactor(0.0); // "on" time for last stage at the desired reduced capacity, (dimensionless)

        int BinNum;                              // bin number counter
        int spnum;                               // compressor speed number
        Array1D<Real64> NetCoolingCapRated(nsp); // net cooling capacity at each speed
        Array1D<Real64> TotCapFlowModFac(nsp);   // Total capacity modifier f(actual flow vs rated flow) for each speed [-]
        Array1D<Real64> EIRFlowModFac(nsp);      // EIR modifier f(actual supply air flow vs rated flow) for each speed [-]
        Real64 CoolingCapacityLS(0.0);           // cooling capacity of Mult-speed DX coil at lower speed, [W]
        Real64 CoolingCapacityHS(0.0);           // cooling capacity of Mult-speed DX coil at higher speed, [W]
        Real64 CoolingElecPowerLS(0.0);          // outdoor unit electric power input at low speed, [W]
        Real64 CoolingElecPowerHS(0.0);          // outdoor unit electric power input at high speed, [W]
        Real64 CoolingCapacityMax(0.0);          // cooling capacity of Mult-speed DX coil at max speed, [W]
        Real64 CoolingElecPowerMax(0.0);         // outdoor unit electric power input at Max speed, [W]

        Real64 constexpr SizingFactor(1.10); // sizing factor per AHRI Std 210/240-2008

        Real64 NetCoolingCapRatedMaxSpeed = 0.0;
        Real64 SEER_User = 0.0;
        Real64 SEER_Standard = 0.0;

        NetCoolingCapWeighted = 0.0;
        TotCoolingElecPowerWeighted = 0.0;
        TotCoolingElecPowerWeightedDefault = 0.0;

        for (int spnum = 1; spnum <= nsp; ++spnum) {
            FanPowerPerEvapAirFlowRate(spnum) = 0.0;
            if (FanPowerPerEvapAirFlowRateFromInput(spnum) <= 0.0) {
                FanPowerPerEvapAirFlowRate(spnum) = DefaultFanPowerPerEvapAirFlowRate;
            } else {
                FanPowerPerEvapAirFlowRate(spnum) = FanPowerPerEvapAirFlowRateFromInput(spnum);
            }
        }

        // Calculate the capacity and power for each speed
        for (spnum = 1; spnum <= nsp; ++spnum) {
            TotCapFlowModFac(spnum) = CurveManager::CurveValue(state, static_cast<int64_t>(CapFFlowCurveIndex(spnum)), AirMassFlowRatioRated);
            TotCoolCapTestA2(spnum) =
                RatedTotalCapacity(spnum) *
                    CurveManager::CurveValue(
                        state, CapFTempCurveIndex(spnum), IndoorCoilInletAirWetBulbTempRated, OutdoorCoilInletAirDryBulbTempTestA2) *
                    TotCapFlowModFac(spnum) -
                FanPowerPerEvapAirFlowRate(spnum) * RatedAirVolFlowRate(spnum);

            TotCoolCapTestB2(spnum) =
                RatedTotalCapacity(spnum) *
                    CurveManager::CurveValue(
                        state, CapFTempCurveIndex(spnum), IndoorCoilInletAirWetBulbTempRated, OutdoorCoilInletAirDryBulbTempTestB2) *
                    TotCapFlowModFac(spnum) -
                FanPowerPerEvapAirFlowRate(spnum) * RatedAirVolFlowRate(spnum);

            TotCoolCapTestB1(spnum) =
                RatedTotalCapacity(spnum) *
                    CurveManager::CurveValue(
                        state, CapFTempCurveIndex(spnum), IndoorCoilInletAirWetBulbTempRated, OutdoorCoilInletAirDryBulbTempTestB1) *
                    TotCapFlowModFac(spnum) -
                FanPowerPerEvapAirFlowRate(spnum) * RatedAirVolFlowRate(spnum);

            TotCoolCapTestF1(spnum) =
                RatedTotalCapacity(spnum) *
                    CurveManager::CurveValue(
                        state, CapFTempCurveIndex(spnum), IndoorCoilInletAirWetBulbTempRated, OutdoorCoilInletAirDryBulbTempTestF1) *
                    TotCapFlowModFac(spnum) -
                FanPowerPerEvapAirFlowRate(spnum) * RatedAirVolFlowRate(spnum);

            EIRFlowModFac(spnum) = CurveManager::CurveValue(state, EIRFFlowCurveIndex(spnum), AirMassFlowRatioRated);
            if (RatedCOP(spnum) > 0.0) {
                OutdoorUnitPowerTestA2(spnum) =
                    TotCoolCapTestA2(spnum) * EIRFlowModFac(spnum) *
                        CurveManager::CurveValue(
                            state, EIRFTempCurveIndex(spnum), IndoorCoilInletAirWetBulbTempRated, OutdoorCoilInletAirDryBulbTempTestA2) /
                        RatedCOP(spnum) +
                    FanPowerPerEvapAirFlowRate(spnum) * RatedAirVolFlowRate(spnum);

                OutdoorUnitPowerTestB2(spnum) =
                    TotCoolCapTestB2(spnum) * EIRFlowModFac(spnum) *
                        CurveManager::CurveValue(
                            state, EIRFTempCurveIndex(spnum), IndoorCoilInletAirWetBulbTempRated, OutdoorCoilInletAirDryBulbTempTestB2) /
                        RatedCOP(spnum) +
                    FanPowerPerEvapAirFlowRate(spnum) * RatedAirVolFlowRate(spnum);

                OutdoorUnitPowerTestB1(spnum) =
                    TotCoolCapTestB1(spnum) * EIRFlowModFac(spnum) *
                        CurveManager::CurveValue(
                            state, EIRFTempCurveIndex(spnum), IndoorCoilInletAirWetBulbTempRated, OutdoorCoilInletAirDryBulbTempTestB1) /
                        RatedCOP(spnum) +
                    FanPowerPerEvapAirFlowRate(spnum) * RatedAirVolFlowRate(spnum);

                OutdoorUnitPowerTestF1(spnum) =
                    TotCoolCapTestF1(spnum) * EIRFlowModFac(spnum) *
                        CurveManager::CurveValue(
                            state, EIRFTempCurveIndex(spnum), IndoorCoilInletAirWetBulbTempRated, OutdoorCoilInletAirDryBulbTempTestF1) /
                        RatedCOP(spnum) +
                    FanPowerPerEvapAirFlowRate(spnum) * RatedAirVolFlowRate(spnum);
            }
        }
        // Standard Rating cooling (net) capacity calculations:
        NetCoolingCapRated(nsp) = TotCoolCapTestA2(nsp);
        NetCoolingCapRatedMaxSpeed = NetCoolingCapRated(nsp);

        // Calculate the SEER value based on contribution of each outdoor air bin temperature
        for (BinNum = 0; BinNum < NumOfOATempBins; ++BinNum) {
            // Equation 11.60 (AHRI-2017)
            BuildingCoolingLoad = (OutdoorBinTemperatureSEER[BinNum] - 18.3) / (35.0 - 18.3) * (TotCoolCapTestA2(nsp) / SizingFactor);
            // determine the speed number
            CoolingCapacityMax = TotCoolCapTestB2(nsp) + ((TotCoolCapTestA2(nsp) - TotCoolCapTestB2(nsp)) /
                                                          (OutdoorCoilInletAirDryBulbTempTestA2 - OutdoorCoilInletAirDryBulbTempTestB2)) *
                                                             (OutdoorBinTemperatureSEER[BinNum] - OutdoorCoilInletAirDryBulbTempTestB2);
            CoolingElecPowerMax = OutdoorUnitPowerTestB2(nsp) + ((OutdoorUnitPowerTestA2(nsp) - OutdoorUnitPowerTestB2(nsp)) /
                                                                 (OutdoorCoilInletAirDryBulbTempTestA2 - OutdoorCoilInletAirDryBulbTempTestB2)) *
                                                                    (OutdoorBinTemperatureSEER[BinNum] - OutdoorCoilInletAirDryBulbTempTestB2);

            for (spnum = 1; spnum <= nsp - 1; ++spnum) {
                CoolingCapacityLS = TotCoolCapTestF1(spnum) + ((TotCoolCapTestB1(spnum) - TotCoolCapTestF1(spnum)) /
                                                               (OutdoorCoilInletAirDryBulbTempTestB1 - OutdoorCoilInletAirDryBulbTempTestF1)) *
                                                                  (OutdoorBinTemperatureSEER[BinNum] - OutdoorCoilInletAirDryBulbTempTestF1);
                CoolingElecPowerLS = OutdoorUnitPowerTestF1(spnum) + ((OutdoorUnitPowerTestB1(spnum) - OutdoorUnitPowerTestF1(spnum)) /
                                                                      (OutdoorCoilInletAirDryBulbTempTestB1 - OutdoorCoilInletAirDryBulbTempTestF1)) *
                                                                         (OutdoorBinTemperatureSEER[BinNum] - OutdoorCoilInletAirDryBulbTempTestF1);
                CoolingCapacityHS = TotCoolCapTestB2(spnum + 1) + ((TotCoolCapTestA2(spnum + 1) - TotCoolCapTestB2(spnum + 1)) /
                                                                   (OutdoorCoilInletAirDryBulbTempTestA2 - OutdoorCoilInletAirDryBulbTempTestB2)) *
                                                                      (OutdoorBinTemperatureSEER[BinNum] - OutdoorCoilInletAirDryBulbTempTestB2);
                CoolingElecPowerHS =
                    OutdoorUnitPowerTestB2(spnum + 1) + ((OutdoorUnitPowerTestA2(spnum + 1) - OutdoorUnitPowerTestB2(spnum + 1)) /
                                                         (OutdoorCoilInletAirDryBulbTempTestA2 - OutdoorCoilInletAirDryBulbTempTestB2)) *
                                                            (OutdoorBinTemperatureSEER[BinNum] - OutdoorCoilInletAirDryBulbTempTestB2);

                if (BuildingCoolingLoad <= CoolingCapacityLS) {
                    PartLoadRatio = min(1.0, BuildingCoolingLoad / CoolingCapacityLS);
                    NetTotCoolCapBinned = PartLoadRatio * CoolingCapacityLS;
                    PartLoadFactorUser = CurveManager::CurveValue(state, PLFFPLRCurveIndex(spnum), PartLoadRatio);
                    PartLoadFactorStandard = 1.0 - CyclicDegradationCoeff * (1.0 - PartLoadRatio);
                    TotCoolElecPowerBinned = (PartLoadRatio / PartLoadFactorUser) * CoolingElecPowerLS;
                    TotCoolElecPowerBinnedDefault = (PartLoadRatio / PartLoadFactorStandard) * CoolingElecPowerLS;
                    goto SpeedLoop_exit;
                } else if ((BuildingCoolingLoad > CoolingCapacityLS) && (BuildingCoolingLoad < CoolingCapacityHS)) {
                    // cycle between speed "spnum" and "spnum + 1"
                    LoadFactor = min(1.0, (CoolingCapacityHS - BuildingCoolingLoad) / (CoolingCapacityHS - CoolingCapacityLS));
                    LoadFactor = max(0.0, LoadFactor);
                    NetTotCoolCapBinned = LoadFactor * CoolingCapacityLS + (1.0 - LoadFactor) * CoolingCapacityHS;
                    TotCoolElecPowerBinned = LoadFactor * CoolingElecPowerLS + (1.0 - LoadFactor) * CoolingElecPowerHS;
                    TotCoolElecPowerBinnedDefault = TotCoolElecPowerBinned;

                    // TBD:  // Suggestion for improvement as per 2017 Standard MCQ & MCE are differently calculated
                    // and so does the load factor for Cooling and Power
                    // Real64 LoadFactorQEnt_2023 =
                    //     min(1.0, (TotCoolCapTestEint(spnum2023) - CoolingCapacityLS_2023) / (CoolingCapacityHS_2023 - CoolingCapacityLS_2023));
                    // LoadFactorQEnt_2023 = max(0.0, LoadFactorQEnt_2023);
                    // Real64 LoadFactorPEnt_2023 = min(
                    //     1.0, (OutdoorUnitPowerTestEint(spnum2023) - CoolingElecPowerLS_2023) / (CoolingElecPowerHS_2023 -
                    //     CoolingElecPowerLS_2023));
                    // LoadFactorPEnt_2023 = max(0.0, LoadFactorPEnt_2023);

                    // NetTotCoolCapBinned_2023 = LoadFactorQEnt_2023 * CoolingCapacityHS_2023 + (1.0 - LoadFactorQEnt_2023) * CoolingCapacityLS_2023;
                    // TotCoolElecPowerBinned_2023 =
                    //     LoadFactorPEnt_2023 * CoolingElecPowerHS_2023 + (1.0 - LoadFactorPEnt_2023) * CoolingElecPowerLS_2023;
                    // TotCoolElecPowerBinnedDefault_2023 = TotCoolElecPowerBinned_2023;
                    goto SpeedLoop_exit;
                } else if (BuildingCoolingLoad >= CoolingCapacityMax) {
                    NetTotCoolCapBinned = CoolingCapacityMax;
                    TotCoolElecPowerBinned = CoolingElecPowerMax;
                    TotCoolElecPowerBinnedDefault = CoolingElecPowerMax;
                    goto SpeedLoop_exit;
                }
            }
        SpeedLoop_exit:;

            NetCoolingCapWeighted += NetTotCoolCapBinned * CoolFracBinHoursAtOutdoorBinTemp[BinNum];
            TotCoolingElecPowerWeighted += TotCoolElecPowerBinned * CoolFracBinHoursAtOutdoorBinTemp[BinNum];
            TotCoolingElecPowerWeightedDefault += TotCoolElecPowerBinnedDefault * CoolFracBinHoursAtOutdoorBinTemp[BinNum];
        }
        SEER_User = 0.0;
        SEER_Standard = 0.0;
        if (TotCoolingElecPowerWeighted > 0.0) {
            SEER_User = NetCoolingCapWeighted / TotCoolingElecPowerWeighted;
            SEER_Standard = NetCoolingCapWeighted / TotCoolingElecPowerWeightedDefault;
        }

        return std::make_tuple(NetCoolingCapRatedMaxSpeed, SEER_User, SEER_Standard);
    }

    std::tuple<Real64, Real64, Real64> MultiSpeedDXCoolingCoilSEER2(EnergyPlusData &state,
                                                                    int const nsp,
                                                                    Array1A_int const CapFFlowCurveIndex,
                                                                    Array1A<Real64> const RatedTotalCapacity,
                                                                    Array1A_int const CapFTempCurveIndex,
                                                                    Array1A<Real64> const FanPowerPerEvapAirFlowRateFromInput_2023,
                                                                    Array1A<Real64> const RatedAirVolFlowRate,
                                                                    Array1A_int const EIRFFlowCurveIndex,
                                                                    Array1A<Real64> const RatedCOP,
                                                                    Array1A_int EIRFTempCurveIndex,
                                                                    Array1A_int const PLFFPLRCurveIndex)
    {
        // Intermediate values calculated from the inputs in the idf file
        // ANSI/AHRI 210/240 Std. 2023
        Array1D<Real64> FanPowerPerEvapAirFlowRate_2023(nsp); // 2023 Fan power per air volume flow rate through the evaporator coil [W/(m3/s)]
        Array1D<Real64> Q_A_Full(nsp);                        // Total cooling capacity at A2 test condition (High speed) | q_A_Full
        Array1D<Real64> Q_B_Full(nsp);                        // Total cooling capacity at B2 test condition (High speed) | q_B_Full
        Array1D<Real64> Q_B_Low(nsp);                         // Total cooling capacity at B1 test condition (Low speed) | q_B_Low
        Array1D<Real64> Q_F_Low(nsp);                         // Total cooling capacity at F1 test condition (Low speed) | q_F_Low
        Array1D<Real64> Q_E_Int(nsp);                         // Total cooling capacity at Eint (Ev) test condition | q_E_Int
        Array1D<Real64> P_A_Full(nsp);                        // Outdoor Unit electric power at A2 test condition (High speed) | p_A_Full
        Array1D<Real64> P_B_Full(nsp);                        // Outdoor Unit electric power at B2 test condition (High speed) | p_B_Full
        Array1D<Real64> P_B_Low(nsp);                         // Outdoor Unit electric power at B1 test condition (Low speed) | p_B_Low
        Array1D<Real64> P_F_Low(nsp);                         // Outdoor Unit electric power at F1 test condition | p_F_Low
        Array1D<Real64> P_E_Int(nsp);                         // Outdoor Unit electric power at Eint (Ev) test conditon | p_E_Int

        Real64 PartLoadFactorUser_2023(
            0.0); // part-load factor based on user-input PLF curve and C_D value that accounts for the cyclic degradation, [-]

        Real64 NetCoolingCapWeighted_2023(0.0);         // net tot cooling cap weighted by the fraction of the binned cooling hours [W]
        Real64 TotCoolingElecPowerWeighted_2023(0.0);   // net total cooling electric power input weighted by the fraction of the temperature bins
        Real64 TotCoolingElecPowerWeightedDefault(0.0); // net total cooling electric power input weighted by the fraction of the temperature bins
                                                        // from AHRI 201/240 default PLF curve and C_D value,

        // binned cooling hours
        Real64 BuildingCoolingLoad_2023(0.0);    // Building space cooling load corresponding to an outdoor bin temperature [W]
        Real64 NetTotCoolCapBinned_2023(0.0);    // Net tot cooling cap corresponding to an outdoor bin temperature [W]
        Real64 TotCoolElecPowerBinned_2023(0.0); // Total cooling electric power corresponding to an outdoor bin temperature [W]

        int spnum;                                    // compressor speed number
        Array1D<Real64> TotCapFlowModFac(nsp);        // Total capacity modifier f(actual flow vs rated flow) for each speed [-]
        Array1D<Real64> EIRFlowModFac(nsp);           // EIR modifier f(actual supply air flow vs rated flow) for each speed [-]
        Array1D<Real64> NetCoolingCapRated_2023(nsp); // net cooling capacity at each speed
        Real64 q_low(0.0);                            // cooling capacity of Mult-speed DX coil at lower speed, [W]
        Real64 q_full(0.0);                           // cooling capacity of Mult-speed DX coil at higher speed, [W]
        Real64 p_low(0.0);                            // outdoor unit electric power input at low speed, [W]
        Real64 p_full(0.0);                           // outdoor unit electric power input at high speed, [W]
        Real64 CoolingCapacityMax_2023(0.0);          // cooling capacity of Mult-speed DX coil at max speed, [W]
        Real64 CoolingElecPowerMax_2023(0.0);         // outdoor unit electric power input at Max speed, [W]

        Real64 constexpr SF(1.10); // Sizing Factor as per AHRI Std 210/240-2023 | equation 11.68
        Real64 constexpr V(1);     // V = 0.93 for Variable Speed Heat Pumps, otherwise V = 1.0

        Real64 NetCoolingCapRatedMaxSpeed2023 = 0.0;
        Real64 SEER2_User = 0.0;
        Real64 SEER2_Standard = 0.0;

        NetCoolingCapWeighted_2023 = 0.0;
        TotCoolingElecPowerWeighted_2023 = 0.0;
        TotCoolingElecPowerWeightedDefault = 0.0;

        for (int spnum = 1; spnum <= nsp; ++spnum) {
            FanPowerPerEvapAirFlowRate_2023(spnum) = 0.0;
            if (FanPowerPerEvapAirFlowRateFromInput_2023(spnum) <= 0.0) {
                FanPowerPerEvapAirFlowRate_2023(spnum) = DefaultFanPowerPerEvapAirFlowRateSEER2;
            } else {
                FanPowerPerEvapAirFlowRate_2023(spnum) = FanPowerPerEvapAirFlowRateFromInput_2023(spnum);
            }
        }

        // Calculate the capacity and power for each speed
        for (spnum = 1; spnum <= nsp; ++spnum) {
            TotCapFlowModFac(spnum) = CurveManager::CurveValue(state, CapFFlowCurveIndex(spnum), AirMassFlowRatioRated);

            Q_A_Full(spnum) = RatedTotalCapacity(spnum) *
                                  CurveManager::CurveValue(
                                      state, CapFTempCurveIndex(spnum), IndoorCoilInletAirWetBulbTempRated, OutdoorCoilInletAirDryBulbTempTestA2) *
                                  TotCapFlowModFac(spnum) -
                              FanPowerPerEvapAirFlowRate_2023(spnum) * RatedAirVolFlowRate(spnum);

            Q_B_Full(spnum) = RatedTotalCapacity(spnum) *
                                  CurveManager::CurveValue(
                                      state, CapFTempCurveIndex(spnum), IndoorCoilInletAirWetBulbTempRated, OutdoorCoilInletAirDryBulbTempTestB2) *
                                  TotCapFlowModFac(spnum) -
                              FanPowerPerEvapAirFlowRate_2023(spnum) * RatedAirVolFlowRate(spnum);

            Q_B_Low(spnum) = RatedTotalCapacity(spnum) *
                                 CurveManager::CurveValue(
                                     state, CapFTempCurveIndex(spnum), IndoorCoilInletAirWetBulbTempRated, OutdoorCoilInletAirDryBulbTempTestB1) *
                                 TotCapFlowModFac(spnum) -
                             FanPowerPerEvapAirFlowRate_2023(spnum) * RatedAirVolFlowRate(spnum);

            Q_F_Low(spnum) = RatedTotalCapacity(spnum) *
                                 CurveManager::CurveValue(
                                     state, CapFTempCurveIndex(spnum), IndoorCoilInletAirWetBulbTempRated, OutdoorCoilInletAirDryBulbTempTestF1) *
                                 TotCapFlowModFac(spnum) -
                             FanPowerPerEvapAirFlowRate_2023(spnum) * RatedAirVolFlowRate(spnum);

            Q_E_Int(spnum) = RatedTotalCapacity(spnum) *
                                 CurveManager::CurveValue(
                                     state, CapFTempCurveIndex(spnum), IndoorCoilInletAirWetBulbTempRated, OutdoorCoilInletAirDryBulbTempTestEint) *
                                 TotCapFlowModFac(spnum) -
                             FanPowerPerEvapAirFlowRate_2023(spnum) * RatedAirVolFlowRate(spnum);

            EIRFlowModFac(spnum) = CurveManager::CurveValue(state, EIRFFlowCurveIndex(spnum), AirMassFlowRatioRated);
            if (RatedCOP(spnum) > 0.0) {
                P_A_Full(spnum) =
                    Q_A_Full(spnum) * EIRFlowModFac(spnum) *
                        CurveManager::CurveValue(
                            state, EIRFTempCurveIndex(spnum), IndoorCoilInletAirWetBulbTempRated, OutdoorCoilInletAirDryBulbTempTestA2) /
                        RatedCOP(spnum) +
                    FanPowerPerEvapAirFlowRate_2023(spnum) * RatedAirVolFlowRate(spnum);

                P_B_Full(spnum) =
                    Q_B_Full(spnum) * EIRFlowModFac(spnum) *
                        CurveManager::CurveValue(
                            state, EIRFTempCurveIndex(spnum), IndoorCoilInletAirWetBulbTempRated, OutdoorCoilInletAirDryBulbTempTestB2) /
                        RatedCOP(spnum) +
                    FanPowerPerEvapAirFlowRate_2023(spnum) * RatedAirVolFlowRate(spnum);

                P_B_Low(spnum) = Q_B_Low(spnum) * EIRFlowModFac(spnum) *
                                     CurveManager::CurveValue(
                                         state, EIRFTempCurveIndex(spnum), IndoorCoilInletAirWetBulbTempRated, OutdoorCoilInletAirDryBulbTempTestB1) /
                                     RatedCOP(spnum) +
                                 FanPowerPerEvapAirFlowRate_2023(spnum) * RatedAirVolFlowRate(spnum);

                P_F_Low(spnum) = Q_F_Low(spnum) * EIRFlowModFac(spnum) *
                                     CurveManager::CurveValue(
                                         state, EIRFTempCurveIndex(spnum), IndoorCoilInletAirWetBulbTempRated, OutdoorCoilInletAirDryBulbTempTestF1) /
                                     RatedCOP(spnum) +
                                 FanPowerPerEvapAirFlowRate_2023(spnum) * RatedAirVolFlowRate(spnum);

                P_E_Int(spnum) =
                    Q_E_Int(spnum) * EIRFlowModFac(spnum) *
                        CurveManager::CurveValue(
                            state, EIRFTempCurveIndex(spnum), IndoorCoilInletAirWetBulbTempRated, OutdoorCoilInletAirDryBulbTempTestEint) /
                        RatedCOP(spnum) +
                    FanPowerPerEvapAirFlowRate_2023(spnum) * RatedAirVolFlowRate(spnum);
            }
        }
        // Standard Rating cooling (net) capacity calculations:
        NetCoolingCapRated_2023(nsp) = Q_A_Full(nsp);
        NetCoolingCapRatedMaxSpeed2023 = NetCoolingCapRated_2023(nsp);

        // Calculate the SEER value based on contribution of each outdoor air bin temperature
        Real64 q_sum = 0.0;
        Real64 e_sum = 0.0;
        Real64 NetCoolingCapWeighted2_2023 = 0.0;
        Real64 TotCoolingElecPowerWeighted2_2023 = 0.0;
        for (int BN = 0; BN < NumOfOATempBins; ++BN) {
            // Equation 11.67 (AHRI-2023)
            BuildingCoolingLoad_2023 = ((OutdoorBinTemperatureSEER[BN] - 18.3) / (35.0 - 18.3) * (Q_A_Full(nsp) / SF)) * V;
            // determine the speed number
            CoolingCapacityMax_2023 =
                Q_B_Full(nsp) + ((Q_A_Full(nsp) - Q_B_Full(nsp)) / (OutdoorCoilInletAirDryBulbTempTestA2 - OutdoorCoilInletAirDryBulbTempTestB2)) *
                                    (OutdoorBinTemperatureSEER[BN] - OutdoorCoilInletAirDryBulbTempTestB2);
            CoolingElecPowerMax_2023 =
                P_B_Full(nsp) + ((P_A_Full(nsp) - P_B_Full(nsp)) / (OutdoorCoilInletAirDryBulbTempTestA2 - OutdoorCoilInletAirDryBulbTempTestB2)) *
                                    (OutdoorBinTemperatureSEER[BN] - OutdoorCoilInletAirDryBulbTempTestB2);

            Real64 q(0.0);
            Real64 e(0.0);
            for (spnum = 1; spnum <= nsp - 1; ++spnum) {
                // Equation 11.69 (AHRI-2023)
                q_low = Q_F_Low(spnum) +
                        ((Q_B_Low(spnum) - Q_F_Low(spnum)) / (OutdoorCoilInletAirDryBulbTempTestB1 - OutdoorCoilInletAirDryBulbTempTestF1)) *
                            (OutdoorBinTemperatureSEER[BN] - OutdoorCoilInletAirDryBulbTempTestF1);
                // Equation 11.70 (AHRI-2023)
                p_low = P_F_Low(spnum) +
                        ((P_B_Low(spnum) - P_F_Low(spnum)) / (OutdoorCoilInletAirDryBulbTempTestB1 - OutdoorCoilInletAirDryBulbTempTestF1)) *
                            (OutdoorBinTemperatureSEER[BN] - OutdoorCoilInletAirDryBulbTempTestF1);
                // Equation 11.71 (AHRI-2023)
                q_full = Q_B_Full(spnum + 1) + ((Q_A_Full(spnum + 1) - Q_B_Full(spnum + 1)) /
                                                (OutdoorCoilInletAirDryBulbTempTestA2 - OutdoorCoilInletAirDryBulbTempTestB2)) *
                                                   (OutdoorBinTemperatureSEER[BN] - OutdoorCoilInletAirDryBulbTempTestB2);
                // Equation 11.72 (AHRI-2023)
                p_full = P_B_Full(spnum + 1) + ((P_A_Full(spnum + 1) - P_B_Full(spnum + 1)) /
                                                (OutdoorCoilInletAirDryBulbTempTestA2 - OutdoorCoilInletAirDryBulbTempTestB2)) *
                                                   (OutdoorBinTemperatureSEER[BN] - OutdoorCoilInletAirDryBulbTempTestB2);

                // # Intermediate Capacity
                Real64 q_A_full = Q_A_Full[spnum];
                Real64 q_B_full = Q_B_Full[spnum];
                Real64 q_B_low = Q_B_Low[spnum];
                Real64 q_F_low = Q_F_Low[spnum];
                Real64 q_E_int = Q_E_Int[spnum];
                // Equation 11.90 (AHRI-2023)
                Real64 q_87_low = q_F_low + (q_B_low - q_F_low) * ((OutdoorBinTemperatureSEER[BN] - 19.44 / 27.77 - 19.44));
                Real64 q_87_full = q_B_full + (q_A_full - q_B_full) * ((OutdoorBinTemperatureSEER[BN] - 19.44 / 27.77 - 19.44));

                // Equation 11.96 (AHRI-2023)
                Real64 N_Cq = (q_E_int - q_87_low) / (q_87_full - q_87_low);
                // Equation 11.95 (AHRI-2023)
                Real64 M_Cq = (q_B_low - q_F_low) / (27.77 - 19.44) * (1. - N_Cq) + (q_A_full - q_B_full) / (35.0 - 27.77) * N_Cq;

                // # Intermediate Power
                Real64 p_A_full = P_A_Full[spnum];
                Real64 p_B_full = P_B_Full[spnum];
                Real64 p_B_low = P_B_Low[spnum];
                Real64 p_F_low = P_F_Low[spnum];
                Real64 p_E_int = P_E_Int[spnum];
                // Equation 11.91 (AHRI-2023)
                Real64 p_87_low = p_F_low + (p_B_low - p_F_low) * ((OutdoorBinTemperatureSEER[BN] - 19.44 / 27.77 - 19.44));
                Real64 p_87_full = p_B_full + (p_A_full - p_B_full) * ((OutdoorBinTemperatureSEER[BN] - 19.44 / 27.77 - 19.44));

                // Equation 11.99 (AHRI-2023)
                Real64 N_CE = (p_E_int - p_87_low) / (p_87_full - p_87_low);
                // Equaition 11.98 (AHRI-2023)
                Real64 M_CE = (p_B_low - p_F_low) / (27.77 - 19.44) * (1. - N_CE) + (p_A_full - p_B_full) / (35.0 - 27.77) * N_CE;

                Real64 t = OutdoorBinTemperatureSEER[BN];
                Real64 n = CoolFracBinHoursAtOutdoorBinTemp[BN];
                Real64 bl = BuildingCoolingLoad_2023;

                // Equation 11.94 (AHRI-2023)
                Real64 q_int = q_E_int + M_Cq * (t - 30.55);
                // Equation 11.97 (AHRI-2023)
                Real64 p_int = p_E_int + M_CE * (t - 30.55);
                Real64 cop_low = q_low / p_low;
                Real64 cop_int = q_int / p_int;
                Real64 cop_full = q_full / p_full;

                // Section 11.2.1.3.1 CASE 1 - Building load is no greater than unit capacity at low speed.
                if (bl <= q_low) {
                    Real64 clf_low = bl / q_low;                                     // Equation 11.75 (AHRI-2023)
                    Real64 plf_low = 1.0 - CyclicDegradationCoeff * (1.0 - clf_low); // Equation 11.76 (AHRI-2023)
                    q = clf_low * q_low * n;                                         // Equation 11.73 (AHRI-2023)
                    e = clf_low * p_low * n / plf_low;                               // Equation 11.74 (AHRI-2023)

                    NetTotCoolCapBinned_2023 = clf_low * q_low * CoolFracBinHoursAtOutdoorBinTemp[BN];
                    PartLoadFactorUser_2023 = CurveManager::CurveValue(state, PLFFPLRCurveIndex(spnum), clf_low);
                    TotCoolElecPowerBinned_2023 = (clf_low / PartLoadFactorUser_2023) * p_low * CoolFracBinHoursAtOutdoorBinTemp[BN];
                    goto SpeedLoop3_exit;
                } else if (bl < q_int) {
                    // Section 11.2.1.3.2 CASE 2 - Building load can be matched by modulating the compressor speed between low speed & full Speed
                    Real64 LoadFactorQEnt_2023 = min(1.0, (Q_E_Int(spnum) - q_low) / (q_full - q_low));
                    LoadFactorQEnt_2023 = max(0.0, LoadFactorQEnt_2023);
                    Real64 LoadFactorPEnt_2023 = min(1.0, (P_E_Int(spnum) - p_low) / (p_full - p_low));
                    LoadFactorPEnt_2023 = max(0.0, LoadFactorPEnt_2023);
                    NetTotCoolCapBinned_2023 = LoadFactorQEnt_2023 * q_full + (1.0 - LoadFactorQEnt_2023) * q_low;
                    TotCoolElecPowerBinned_2023 = LoadFactorPEnt_2023 * p_full + (1.0 - LoadFactorPEnt_2023) * p_low;

                    // Low Speed
                    Real64 cop_int_bin = cop_low + (cop_int - cop_low) / (q_int - q_low) * (bl - q_low); // Equation 11.101 (AHRI-2023)
                    q = bl * n;                                                                          // 11.92 --> n is missing in the print ?
                    e = q / cop_int_bin;                                                                 // 11.93 --> adjusted to 11.101
                    goto SpeedLoop3_exit;
                } else if (bl <= q_full) {
                    // Section 11.2.1.3.2 CASE 2 - Building load can be matched by modulating the compressor speed between low speed & full Speed
                    Real64 LoadFactorQEnt_2023 = min(1.0, (Q_E_Int(spnum) - q_low) / (q_full - q_low));
                    LoadFactorQEnt_2023 = max(0.0, LoadFactorQEnt_2023);
                    Real64 LoadFactorPEnt_2023 = min(1.0, (P_E_Int(spnum) - p_low) / (p_full - p_low));
                    LoadFactorPEnt_2023 = max(0.0, LoadFactorPEnt_2023);
                    NetTotCoolCapBinned_2023 = LoadFactorQEnt_2023 * q_full + (1.0 - LoadFactorQEnt_2023) * q_low;
                    TotCoolElecPowerBinned_2023 = LoadFactorPEnt_2023 * p_full + (1.0 - LoadFactorPEnt_2023) * p_low;

                    // Full Speed
                    Real64 cop_int_bin = cop_int + (cop_full - cop_int) / (q_full - q_int) * (bl - q_int); // Equation 11.102 (AHRI-2023)
                    q = bl * n;                                                                            // 11.92 --> n is missing in the print ?
                    e = q / cop_int_bin;                                                                   // 11.93 --> adjusted to 11.102
                    goto SpeedLoop3_exit;
                } else { // bl >= q_full
                    // Section 11.2.1.3.3 CASE 3 - Building load is equal to or greater than unit capacity at full stage
                    NetTotCoolCapBinned_2023 = CoolingCapacityMax_2023 * CoolFracBinHoursAtOutdoorBinTemp[BN];
                    TotCoolElecPowerBinned_2023 = CoolingElecPowerMax_2023 * CoolFracBinHoursAtOutdoorBinTemp[BN];

                    q = q_full * n; // Equation 11.88 (AHRI-2023)
                    e = p_full * n; // Equation 11.89 (AHRI-2023)
                    goto SpeedLoop3_exit;
                }
            }
        SpeedLoop3_exit:;
            NetCoolingCapWeighted2_2023 += NetTotCoolCapBinned_2023;
            TotCoolingElecPowerWeighted2_2023 += TotCoolElecPowerBinned_2023;
            q_sum += q;
            e_sum += e;
        }
        SEER2_User = 0.0;
        SEER2_Standard = 0.0;
        if (e_sum > 0.0) {
            SEER2_User = NetCoolingCapWeighted2_2023 / TotCoolingElecPowerWeighted2_2023;
            SEER2_Standard = q_sum / e_sum; // Equation 11.66 (AHRI-2023)
        }

        return std::make_tuple(NetCoolingCapRatedMaxSpeed2023, SEER2_User, SEER2_Standard);
    }

    std::map<std::string, Real64> MultiSpeedDXCoolingCoilStandardRatings(
        EnergyPlusData &state,
        Array1A_int const CapFTempCurveIndex,                           // Index for the capacity as a function of temperature modifier curve
        Array1A_int const CapFFlowCurveIndex,                           // Index for the capacity as a function of flow fraction modifier curve
        Array1A_int const EIRFTempCurveIndex,                           // Index for the EIR as a function of temperature modifier curve
        Array1A_int const EIRFFlowCurveIndex,                           // Index for the EIR as a function of flow fraction modifier curve
        Array1A_int const PLFFPLRCurveIndex,                            // Index for the PLF vs part-load ratio curve
        Array1A<Real64> const RatedTotalCapacity,                       // Reference capacity of DX coil [W]
        Array1A<Real64> const RatedCOP,                                 // Reference coefficient of performance [W/W]
        Array1A<Real64> const RatedAirVolFlowRate,                      // Reference air flow rate of DX coil [m3/s]
        Array1A<Real64> const FanPowerPerEvapAirFlowRateFromInput,      // 2017 rated fan power per evap air flow rate [W/(m3/s)]
        Array1A<Real64> const FanPowerPerEvapAirFlowRateFromInput_2023, // 2023 rated fan power per evap air flow rate [W/(m3/s)]
        int const nsp                                                   // Number of compressor speeds
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Nigusse, FSEC
        //       DATE WRITTEN   December 2012
        //       MODIFIED
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Calculates the standard ratings net cooling capacity and SEER values for multi speed DX cooling coils
        // at the AHRI standard test condition(s).

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // Using/Aliasing
        using CurveManager::CurveValue;

        // Argument array dimensioning
        CapFTempCurveIndex.dim(nsp);
        CapFFlowCurveIndex.dim(nsp);
        EIRFTempCurveIndex.dim(nsp);
        EIRFFlowCurveIndex.dim(nsp);
        PLFFPLRCurveIndex.dim(nsp);
        RatedTotalCapacity.dim(nsp);
        RatedCOP.dim(nsp);
        RatedAirVolFlowRate.dim(nsp);
        FanPowerPerEvapAirFlowRateFromInput.dim(nsp);

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // CHARACTER(len=*), PARAMETER    :: RoutineName='MultiSpeedDXCoolingCoilStandardRatings: ' ! Include trailing blank space
        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        // Intermediate values calculated from the inputs in the idf file

        Real64 NetCoolingCapRatedMaxSpeed(0.0); // net cooling capacity at maximum speed
        Real64 SEER_User(0.0);     // seasonal energy efficiency ratio of multi speed DX cooling coil, from user-input PLF curve and C_D value
        Real64 SEER_Standard(0.0); // seasonal energy efficiency ratio of multi speed DX cooling coil, from AHRI Std 210/240-2008 default PLF
                                   // curve and
                                   // C_D value

        // Ratings Based on ANSI/AHRI 210/140
        Real64 NetCoolingCapRatedMaxSpeed2023(0.0); // net cooling capacity at maximum speed
        Real64 SEER2_User(0.0);     // seasonal energy efficiency ratio of multi speed DX cooling coil, from user-input PLF curve and C_D value
        Real64 SEER2_Standard(0.0); // seasonal energy efficiency ratio of multi speed DX cooling coil, from AHRI Std 210/240-2008 default PLF
                                    // curve and
                                    // C_D value

        std::map<std::string, Real64> StandardRatingsResult;
        // StandardRatingsResult["NetCoolingCapRatedMaxSpeed"] = NetCoolingCapRatedMaxSpeed;
        // StandardRatingsResult["SEER_User"] = SEER_User;
        // StandardRatingsResult["SEER_Standard"] = SEER_Standard;
        // StandardRatingsResult["NetCoolingCapRatedMaxSpeed2023"] = NetCoolingCapRatedMaxSpeed2023;
        // StandardRatingsResult["SEER2_User"] = SEER2_User;
        // StandardRatingsResult["SEER2_Standard"] = SEER2_Standard;

        std::tie(NetCoolingCapRatedMaxSpeed, SEER_User, SEER_Standard) = MultiSpeedDXCoolingCoilSEER(state,
                                                                                                     nsp,
                                                                                                     CapFFlowCurveIndex,
                                                                                                     RatedTotalCapacity,
                                                                                                     CapFTempCurveIndex,
                                                                                                     FanPowerPerEvapAirFlowRateFromInput,
                                                                                                     RatedAirVolFlowRate,
                                                                                                     EIRFFlowCurveIndex,
                                                                                                     RatedCOP,
                                                                                                     EIRFTempCurveIndex,
                                                                                                     PLFFPLRCurveIndex);

        // SEER2 Calculations ANSI/AHRI 210/240 Standard 2023
        std::tie(NetCoolingCapRatedMaxSpeed2023, SEER2_User, SEER2_Standard) = MultiSpeedDXCoolingCoilSEER2(state,
                                                                                                            nsp,
                                                                                                            CapFFlowCurveIndex,
                                                                                                            RatedTotalCapacity,
                                                                                                            CapFTempCurveIndex,
                                                                                                            FanPowerPerEvapAirFlowRateFromInput_2023,
                                                                                                            RatedAirVolFlowRate,
                                                                                                            EIRFFlowCurveIndex,
                                                                                                            RatedCOP,
                                                                                                            EIRFTempCurveIndex,
                                                                                                            PLFFPLRCurveIndex);

        StandardRatingsResult["NetCoolingCapRatedMaxSpeed"] = NetCoolingCapRatedMaxSpeed;
        StandardRatingsResult["SEER_User"] = SEER_User;
        StandardRatingsResult["SEER_Standard"] = SEER_Standard;

        StandardRatingsResult["NetCoolingCapRatedMaxSpeed2023"] = NetCoolingCapRatedMaxSpeed2023;
        StandardRatingsResult["SEER2_User"] = SEER2_User;
        StandardRatingsResult["SEER2_Standard"] = SEER2_Standard;
        return StandardRatingsResult;
    }

    std::tuple<Real64, Real64, Real64>
    MultiSpedDXHeatingCoilHSPF(EnergyPlusData &state,
                               int const nsp,                                           // Number of compressor speed
                               Array1A<Real64> const MSFanPowerPerEvapAirFlowRateInput, // 2017 rated fan power per evap air flow rate [W/(m3/s)]
                               Array1A_int const CapFTempCurveIndex,           // Index for the capacity as a function of temperature modifier curve
                               Array1A_int const CapFFlowCurveIndex,           // Index for the capacity as a function of flow fraction modifier curve
                               Array1A<Real64> const RatedTotalCapacity,       // Reference capacity of DX coil [W]
                               Array1A<Real64> const RatedAirVolFlowRate,      // Reference air flow rate of DX coil [m3/s]
                               Array1A_int const EIRFFlowCurveIndex,           // Index for the EIR as a function of flow fraction modifier curve
                               Array1A_int const EIRFTempCurveIndex,           // Index for the EIR as a function of temperature modifier curve
                               Array1A<Real64> const RatedCOP,                 // Reference coefficient of performance [W/W]
                               Optional_int_const RegionNum,                   // Region number for calculating HSPF of single speed DX heating coil
                               Optional<Real64 const> MinOATCompressor,        // Minimum OAT for heat pump compressor operation [C]
                               Optional<Real64 const> OATempCompressorOn,      // The outdoor temperature when the compressor is automatically turned
                               Optional_bool_const OATempCompressorOnOffBlank, // Flag used to determine low temperature cut out factor
                               Optional<HPdefrostControl const> DefrostControl) // defrost control; 1=timed, 2=on-demand
    {

        // Intermediate values calculated from the inputs in the idf file
        Real64 HSPF(0.0);
        Real64 NetHeatingCapRatedHighTemp(0.0);
        Real64 NetHeatingCapRatedLowTemp(0.0);

        int BinNum;         // bin number counter
        int spnum;          // compressor speed number
        int StandardDHRNum; // Integer counter for standardized DHRs

        Array1D<Real64> FanPowerPerEvapAirFlowRate(nsp); // Fan power per air volume flow rate through the evaporator coil [W/(m3/s)]
        Array1D<Real64> TotHeatCapTestH0(nsp);           // Total cooling capacity at A2 test condition (High speed)
        Array1D<Real64> TotHeatCapTestH1(nsp);           // Total cooling capacity at B2 test condition (High speed)
        Array1D<Real64> TotHeatCapTestH2(nsp);           // Total cooling capacity at B1 test condition (Low speed)
        Array1D<Real64> TotHeatCapTestH3(nsp);           // Total cooling capacity at F1 test condition (Low speed)
        Array1D<Real64> OutdoorUnitPowerTestH0(nsp);     // Outdoor Unit electric power at A2 test condition (High speed)
        Array1D<Real64> OutdoorUnitPowerTestH1(nsp);     // Outdoor Unit electric power at B2 test condition (High speed)
        Array1D<Real64> OutdoorUnitPowerTestH2(nsp);     // Outdoor Unit electric power at B1 test condition (Low speed)
        Array1D<Real64> OutdoorUnitPowerTestH3(nsp);     // Outdoor Unit electric power at F1 test condition (Low speed)
        Real64 HeatingCapacityLS;                        // cooling capacity of Mult-speed DX coil at lower speed, [W]
        Real64 HeatingCapacityHS;                        // cooling capacity of Mult-speed DX coil at higher speed, [W]
        Real64 HeatingElecPowerLS;                       // outdoor unit electric power input at low speed, [W]
        Real64 HeatingElecPowerHS;                       // outdoor unit electric power input at high speed, [W]
        Real64 HeatingCapacityMax;                       // cooling capacity of Mult-speed DX coil at max speed, [W]
        Real64 HeatingElecPowerMax;                      // outdoor unit electric power input at Max speed, [W]
        Array1D<Real64> TotHeatCapTestH1High(nsp);       // net heating capacity high speed at H1 test conditon, [W]

        // Intermediate values calculated from the inputs in the idf file
        Array1D<Real64> TotCapFlowModFac(nsp); // Total capacity modifier f(actual flow vs rated flow) for each speed [-]
        Array1D<Real64> EIRFlowModFac(nsp);    // EIR modifier f(actual supply air flow vs rated flow) for each speed [-]

        Real64 TotCapTempModFacH0(0.0); // Tot capacity modifier (function of entering wetbulb, outside drybulb) at H0 Test [-]
        Real64 EIRTempModFacH0(0.0);    // EIR modifier (function of entering wetbulb, outside drybulb)  at H0 Test[-]
        Real64 TotCapTempModFacH1(0.0); // Tot capacity modifier (function of entering wetbulb, outside drybulb) at H1 Test [-]
        Real64 EIRTempModFacH1(0.0);    // EIR modifier (function of entering wetbulb, outside drybulb)  at H1 Test[-]
        Real64 TotCapTempModFacH2(0.0); // Tot capacity modifier (function of entering wetbulb, outside drybulb) at H2 Test [-]
        Real64 EIRTempModFacH2(0.0);    // EIR modifier (function of entering wetbulb, outside drybulb)  at H2 Test[-]
        Real64 TotCapTempModFacH3(0.0); // Tot capacity modifier (function of entering wetbulb, outside drybulb) at H3 Test [-]
        Real64 EIRTempModFacH3(0.0);    // EIR modifier (function of entering wetbulb, outside drybulb)  at H3 Test[-]

        Real64 OATempCompressorOff(0.0); // Minimum outdoor air temperature to turn the commpressor off
        Real64 PartLoadRatio(0.0);       // compressor cycling ratio between successive speeds, [-]
        Real64 PartLoadFraction(0.0);    // part-load fraction that account for the cyclic degradation, [-]

        Real64 NetHeatingCapWeighted(0.0);       // net total heating cap weighted by the fraction of the binned cooling hours [W]
        Real64 TotHeatingElecPowerWeighted(0.0); // net total heat pump and resistance heating electric Energy input weighted by
        // the fraction of the binned cooling hours
        Real64 BuildingHeatingLoad(0.0);      // Building space heating load corresponding to an outdoor bin temperature [W]
        Real64 NetTotHeatCapBinned(0.0);      // Net tot heatinging cap corresponding to an outdoor bin temperature [W]
        Real64 TotHeatElecPowerBinnedHP(0.0); // Total Heat Pump heating electric power consumption at outdoor bin temp [W]
        Real64 TotHeatElecPowerBinnedRH(0.0); // Total Resistance heating electric power consumption at outdoor bin temp [W]

        Real64 LoadFactor;               // Fractional "on" time for last stage at the desired reduced capacity, (dimensionless)
        Real64 LowTempCutOutFactor(0.0); // Factor which corresponds to compressor operation depending on outdoor temperature

        Real64 FractionalBinHours(0.0);       // Fractional bin hours for the heating season  [-]
        Real64 DemandDeforstCredit(1.0);      // A factor to adjust HSPF if coil has demand defrost control  [-]
        Real64 DesignHeatingRequirement(0.0); // The amount of heating required to maintain a given indoor temperature
        // at a particular outdoor design temperature.  [W]
        Real64 DesignHeatingRequirementMin(0.0); // minimum design heating requirement [W]
        Real64 DesignHeatingRequirementMax(0.0); // maximum design heating requirement [W]

        NetHeatingCapWeighted = 0.0;
        TotHeatingElecPowerWeighted = 0.0;

        for (spnum = 1; spnum <= nsp; ++spnum) {
            FanPowerPerEvapAirFlowRate(spnum) = 0.0;
            if (MSFanPowerPerEvapAirFlowRateInput(spnum) <= 0.0) {
                FanPowerPerEvapAirFlowRate(spnum) = DefaultFanPowerPerEvapAirFlowRate;
            } else {
                FanPowerPerEvapAirFlowRate(spnum) = MSFanPowerPerEvapAirFlowRateInput(spnum);
            }
        }

        // Proceed withe HSPF value calculation
        for (spnum = 1; spnum <= nsp; ++spnum) {
            TotCapFlowModFac(spnum) = CurveManager::CurveValue(state, CapFFlowCurveIndex(spnum), AirMassFlowRatioRated);
            {
                if (state.dataCurveManager->PerfCurve(CapFTempCurveIndex(spnum)).NumDims == 1) {
                    TotCapTempModFacH0 = CurveManager::CurveValue(state, CapFTempCurveIndex(spnum), HeatingOutdoorCoilInletAirDBTempH0Test);
                    TotCapTempModFacH1 = CurveManager::CurveValue(state, CapFTempCurveIndex(spnum), HeatingOutdoorCoilInletAirDBTempRated);
                    TotCapTempModFacH2 = CurveManager::CurveValue(state, CapFTempCurveIndex(spnum), HeatingOutdoorCoilInletAirDBTempH2Test);
                    TotCapTempModFacH3 = CurveManager::CurveValue(state, CapFTempCurveIndex(spnum), HeatingOutdoorCoilInletAirDBTempH3Test);
                } else {
                    TotCapTempModFacH0 = CurveManager::CurveValue(
                        state, CapFTempCurveIndex(spnum), HeatingIndoorCoilInletAirDBTempRated, HeatingOutdoorCoilInletAirDBTempH0Test);
                    TotCapTempModFacH1 = CurveManager::CurveValue(
                        state, CapFTempCurveIndex(spnum), HeatingIndoorCoilInletAirDBTempRated, HeatingOutdoorCoilInletAirDBTempRated);
                    TotCapTempModFacH2 = CurveManager::CurveValue(
                        state, CapFTempCurveIndex(spnum), HeatingIndoorCoilInletAirDBTempRated, HeatingOutdoorCoilInletAirDBTempH2Test);
                    TotCapTempModFacH3 = CurveManager::CurveValue(
                        state, CapFTempCurveIndex(spnum), HeatingIndoorCoilInletAirDBTempRated, HeatingOutdoorCoilInletAirDBTempH3Test);
                }
            }

            TotHeatCapTestH0(spnum) = RatedTotalCapacity(spnum) * TotCapTempModFacH0 * TotCapFlowModFac(spnum) +
                                      FanPowerPerEvapAirFlowRate(spnum) * RatedAirVolFlowRate(spnum);
            TotHeatCapTestH1(spnum) = RatedTotalCapacity(spnum) * TotCapTempModFacH1 * TotCapFlowModFac(spnum) +
                                      FanPowerPerEvapAirFlowRate(spnum) * RatedAirVolFlowRate(spnum);
            TotHeatCapTestH2(spnum) = RatedTotalCapacity(spnum) * TotCapTempModFacH2 * TotCapFlowModFac(spnum) +
                                      FanPowerPerEvapAirFlowRate(spnum) * RatedAirVolFlowRate(spnum);
            TotHeatCapTestH3(spnum) = RatedTotalCapacity(spnum) * TotCapTempModFacH3 * TotCapFlowModFac(spnum) +
                                      FanPowerPerEvapAirFlowRate(spnum) * RatedAirVolFlowRate(spnum);

            EIRFlowModFac(spnum) = CurveManager::CurveValue(state, EIRFFlowCurveIndex(spnum), AirMassFlowRatioRated);

            {
                if (state.dataCurveManager->PerfCurve(EIRFTempCurveIndex(spnum)).NumDims == 1) {
                    EIRTempModFacH0 = CurveManager::CurveValue(state, EIRFTempCurveIndex(spnum), HeatingOutdoorCoilInletAirDBTempH0Test);
                    EIRTempModFacH1 = CurveManager::CurveValue(state, EIRFTempCurveIndex(spnum), HeatingOutdoorCoilInletAirDBTempRated);
                    EIRTempModFacH2 = CurveManager::CurveValue(state, EIRFTempCurveIndex(spnum), HeatingOutdoorCoilInletAirDBTempH2Test);
                    EIRTempModFacH3 = CurveManager::CurveValue(state, EIRFTempCurveIndex(spnum), HeatingOutdoorCoilInletAirDBTempH3Test);
                } else {
                    EIRTempModFacH0 = CurveManager::CurveValue(
                        state, EIRFTempCurveIndex(spnum), HeatingIndoorCoilInletAirDBTempRated, HeatingOutdoorCoilInletAirDBTempH0Test);
                    EIRTempModFacH1 = CurveManager::CurveValue(
                        state, EIRFTempCurveIndex(spnum), HeatingIndoorCoilInletAirDBTempRated, HeatingOutdoorCoilInletAirDBTempRated);
                    EIRTempModFacH2 = CurveManager::CurveValue(
                        state, EIRFTempCurveIndex(spnum), HeatingIndoorCoilInletAirDBTempRated, HeatingOutdoorCoilInletAirDBTempH2Test);
                    EIRTempModFacH3 = CurveManager::CurveValue(
                        state, EIRFTempCurveIndex(spnum), HeatingIndoorCoilInletAirDBTempRated, HeatingOutdoorCoilInletAirDBTempH3Test);
                }
            }
            if (RatedCOP(spnum) > 0.0) {
                OutdoorUnitPowerTestH0(spnum) = TotHeatCapTestH0(spnum) * EIRFlowModFac(spnum) * EIRTempModFacH0 / RatedCOP(spnum) +
                                                FanPowerPerEvapAirFlowRate(spnum) * RatedAirVolFlowRate(spnum);
                OutdoorUnitPowerTestH1(spnum) = TotHeatCapTestH1(spnum) * EIRFlowModFac(spnum) * EIRTempModFacH1 / RatedCOP(spnum) +
                                                FanPowerPerEvapAirFlowRate(spnum) * RatedAirVolFlowRate(spnum);
                OutdoorUnitPowerTestH2(spnum) = TotHeatCapTestH2(spnum) * EIRFlowModFac(spnum) * EIRTempModFacH2 / RatedCOP(spnum) +
                                                FanPowerPerEvapAirFlowRate(spnum) * RatedAirVolFlowRate(spnum);
                OutdoorUnitPowerTestH3(spnum) = TotHeatCapTestH3(spnum) * EIRFlowModFac(spnum) * EIRTempModFacH3 / RatedCOP(spnum) +
                                                FanPowerPerEvapAirFlowRate(spnum) * RatedAirVolFlowRate(spnum);
            }
        }

        // determine the HP capacity at the rated condition (AHRI H1 high speed test Condition); and determine the
        // the building heat requirement for the user specified region
        NetHeatingCapRatedHighTemp = TotHeatCapTestH1(nsp);
        NetHeatingCapRatedLowTemp = TotHeatCapTestH3(nsp);

        if (RegionNum == 5) {
            DesignHeatingRequirementMin = NetHeatingCapRatedHighTemp;
            DesignHeatingRequirementMax = 2.20 * NetHeatingCapRatedHighTemp;
        } else {
            DesignHeatingRequirementMin = NetHeatingCapRatedHighTemp * (18.33 - OutdoorDesignTemperature[RegionNum - 1]) / (60.0 / 1.80);
            DesignHeatingRequirementMax = 2.20 * DesignHeatingRequirementMin;
        }
        // Set the Design Heating Requirement to nearest standard value (From Table 18, AHRI/ANSI Std 210/240)
        for (StandardDHRNum = 0; StandardDHRNum < TotalNumOfStandardDHRs - 1; ++StandardDHRNum) {
            if (DesignHeatingRequirementMin < StandardDesignHeatingRequirement[0]) {

                DesignHeatingRequirement = min(StandardDesignHeatingRequirement[0], DesignHeatingRequirementMax);

            } else if (DesignHeatingRequirementMin >= StandardDesignHeatingRequirement[StandardDHRNum] &&
                       DesignHeatingRequirementMin < StandardDesignHeatingRequirement[StandardDHRNum + 1]) {
                if ((DesignHeatingRequirementMin - StandardDesignHeatingRequirement[StandardDHRNum]) >
                    (StandardDesignHeatingRequirement[StandardDHRNum + 1] - DesignHeatingRequirementMin)) {

                    DesignHeatingRequirement = min(StandardDesignHeatingRequirement[StandardDHRNum + 1], DesignHeatingRequirementMax);
                } else {
                    DesignHeatingRequirement = min(StandardDesignHeatingRequirement[StandardDHRNum], DesignHeatingRequirementMax);
                }
            } else if (DesignHeatingRequirementMin >= StandardDesignHeatingRequirement[TotalNumOfStandardDHRs - 1]) {
                DesignHeatingRequirement = min(StandardDesignHeatingRequirement[StandardDHRNum], DesignHeatingRequirementMax);
            }
        }
        // The minimum temperature below which the compressor is turned off
        OATempCompressorOff = MinOATCompressor;

        for (BinNum = 0; BinNum < TotalNumOfTemperatureBins[RegionNum - 1]; ++BinNum) { // NumOfOATempBins

            FractionalBinHours = FracBinHoursAtOutdoorBinTemp[RegionNum - 1][BinNum];

            // Calculate the building heating load
            BuildingHeatingLoad = (18.33 - OutdoorBinTemperature[BinNum]) / (18.33 - OutdoorDesignTemperature[RegionNum - 1]) * CorrectionFactor *
                                  DesignHeatingRequirement;

            if ((OutdoorBinTemperature[BinNum] <= -8.33) || (OutdoorBinTemperature[BinNum] >= 7.20)) {
                HeatingCapacityMax = TotHeatCapTestH3(nsp) + ((TotHeatCapTestH1(nsp) - TotHeatCapTestH3(nsp)) *
                                                              (OutdoorBinTemperature[BinNum] - HeatingOutdoorCoilInletAirDBTempH3Test) /
                                                              (HeatingOutdoorCoilInletAirDBTempRated - HeatingOutdoorCoilInletAirDBTempH3Test));
                HeatingElecPowerMax =
                    OutdoorUnitPowerTestH3(nsp) + ((OutdoorUnitPowerTestH1(nsp) - OutdoorUnitPowerTestH3(nsp)) *
                                                   (OutdoorBinTemperature[BinNum] - HeatingOutdoorCoilInletAirDBTempH3Test) /
                                                   (HeatingOutdoorCoilInletAirDBTempRated - HeatingOutdoorCoilInletAirDBTempH3Test));
            } else {
                HeatingCapacityMax = TotHeatCapTestH3(nsp) + ((TotHeatCapTestH2(nsp) - TotHeatCapTestH3(nsp)) *
                                                              (OutdoorBinTemperature[BinNum] - HeatingOutdoorCoilInletAirDBTempH3Test) /
                                                              (HeatingOutdoorCoilInletAirDBTempH2Test - HeatingOutdoorCoilInletAirDBTempH3Test));
                HeatingElecPowerMax =
                    OutdoorUnitPowerTestH3(nsp) + ((OutdoorUnitPowerTestH2(nsp) - OutdoorUnitPowerTestH3(nsp)) *
                                                   (OutdoorBinTemperature[BinNum] - HeatingOutdoorCoilInletAirDBTempH3Test) /
                                                   (HeatingOutdoorCoilInletAirDBTempH2Test - HeatingOutdoorCoilInletAirDBTempH3Test));
            }

            // determine the speed number
            for (spnum = 1; spnum <= nsp - 1; ++spnum) {
                // Low Speed
                if (OutdoorBinTemperature[BinNum] < -8.33) {
                    HeatingCapacityLS = TotHeatCapTestH3(spnum) + ((TotHeatCapTestH1(spnum) - TotHeatCapTestH3(spnum)) *
                                                                   (OutdoorBinTemperature[BinNum] - HeatingOutdoorCoilInletAirDBTempH3Test) /
                                                                   (HeatingOutdoorCoilInletAirDBTempRated - HeatingOutdoorCoilInletAirDBTempH3Test));
                    HeatingElecPowerLS =
                        OutdoorUnitPowerTestH3(spnum) + ((OutdoorUnitPowerTestH1(spnum) - OutdoorUnitPowerTestH3(spnum)) *
                                                         (OutdoorBinTemperature[BinNum] - HeatingOutdoorCoilInletAirDBTempH3Test) /
                                                         (HeatingOutdoorCoilInletAirDBTempRated - HeatingOutdoorCoilInletAirDBTempH3Test));

                } else if (OutdoorBinTemperature[BinNum] >= 4.44) {
                    HeatingCapacityLS = TotHeatCapTestH1(spnum) + ((TotHeatCapTestH0(spnum) - TotHeatCapTestH1(spnum)) *
                                                                   (OutdoorBinTemperature[BinNum] - HeatingOutdoorCoilInletAirDBTempRated) /
                                                                   (HeatingOutdoorCoilInletAirDBTempH0Test - HeatingOutdoorCoilInletAirDBTempRated));
                    HeatingElecPowerLS =
                        OutdoorUnitPowerTestH1(spnum) + ((OutdoorUnitPowerTestH0(spnum) - OutdoorUnitPowerTestH1(spnum)) *
                                                         (OutdoorBinTemperature[BinNum] - HeatingOutdoorCoilInletAirDBTempRated) /
                                                         (HeatingOutdoorCoilInletAirDBTempH0Test - HeatingOutdoorCoilInletAirDBTempRated));
                } else {
                    HeatingCapacityLS = TotHeatCapTestH3(spnum) + ((TotHeatCapTestH2(spnum) - TotHeatCapTestH3(spnum)) *
                                                                   (OutdoorBinTemperature[BinNum] - HeatingOutdoorCoilInletAirDBTempH3Test) /
                                                                   (HeatingOutdoorCoilInletAirDBTempH2Test - HeatingOutdoorCoilInletAirDBTempH3Test));
                    HeatingElecPowerLS =
                        OutdoorUnitPowerTestH3(spnum) + ((OutdoorUnitPowerTestH2(spnum) - OutdoorUnitPowerTestH3(spnum)) *
                                                         (OutdoorBinTemperature[BinNum] - HeatingOutdoorCoilInletAirDBTempH3Test) /
                                                         (HeatingOutdoorCoilInletAirDBTempH2Test - HeatingOutdoorCoilInletAirDBTempH3Test));
                }
                // High Speed
                if ((OutdoorBinTemperature[BinNum] <= -8.33) || (OutdoorBinTemperature[BinNum] >= 7.20)) {
                    HeatingCapacityHS =
                        TotHeatCapTestH3(spnum + 1) + ((TotHeatCapTestH1(spnum + 1) - TotHeatCapTestH3(spnum + 1)) *
                                                       (OutdoorBinTemperature[BinNum] - HeatingOutdoorCoilInletAirDBTempH3Test) /
                                                       (HeatingOutdoorCoilInletAirDBTempRated - HeatingOutdoorCoilInletAirDBTempH3Test));
                    HeatingElecPowerHS =
                        OutdoorUnitPowerTestH3(spnum + 1) + ((OutdoorUnitPowerTestH1(spnum + 1) - OutdoorUnitPowerTestH3(spnum + 1)) *
                                                             (OutdoorBinTemperature[BinNum] - HeatingOutdoorCoilInletAirDBTempH3Test) /
                                                             (HeatingOutdoorCoilInletAirDBTempRated - HeatingOutdoorCoilInletAirDBTempH3Test));
                } else {
                    HeatingCapacityHS =
                        TotHeatCapTestH3(spnum + 1) + ((TotHeatCapTestH2(spnum + 1) - TotHeatCapTestH3(spnum + 1)) *
                                                       (OutdoorBinTemperature[BinNum] - HeatingOutdoorCoilInletAirDBTempH3Test) /
                                                       (HeatingOutdoorCoilInletAirDBTempH2Test - HeatingOutdoorCoilInletAirDBTempH3Test));
                    HeatingElecPowerHS =
                        OutdoorUnitPowerTestH3(spnum + 1) + ((OutdoorUnitPowerTestH2(spnum + 1) - OutdoorUnitPowerTestH3(spnum + 1)) *
                                                             (OutdoorBinTemperature[BinNum] - HeatingOutdoorCoilInletAirDBTempH3Test) /
                                                             (HeatingOutdoorCoilInletAirDBTempH2Test - HeatingOutdoorCoilInletAirDBTempH3Test));
                }
                LowTempCutOutFactor = 0.0;
                if (!OATempCompressorOnOffBlank) {
                    if (OutdoorBinTemperature[BinNum] <= OATempCompressorOff) {
                        LowTempCutOutFactor = 0.0;
                    } else if (OutdoorBinTemperature[BinNum] > OATempCompressorOff && OutdoorBinTemperature[BinNum] <= OATempCompressorOn) {
                        LowTempCutOutFactor = 0.5;
                    } else {
                        LowTempCutOutFactor = 1.0;
                    }
                } else {
                    LowTempCutOutFactor = 1.0;
                }

                if (BuildingHeatingLoad <= HeatingCapacityLS) {
                    if (HeatingCapacityLS > 0.0) PartLoadRatio = min(1.0, BuildingHeatingLoad / HeatingCapacityLS);
                    NetTotHeatCapBinned = BuildingHeatingLoad;
                    PartLoadFraction = 1.0 - CyclicDegradationCoeff * (1.0 - PartLoadRatio);
                    TotHeatElecPowerBinnedHP = (PartLoadRatio / PartLoadFraction) * HeatingElecPowerLS * LowTempCutOutFactor;
                    TotHeatElecPowerBinnedRH = BuildingHeatingLoad * (1.0 - LowTempCutOutFactor);
                    goto HeatSpeedLoop_exit;

                } else if ((BuildingHeatingLoad > HeatingCapacityLS) && (BuildingHeatingLoad < HeatingCapacityHS)) {
                    // cycle between speed "spnum" and "spnum + 1"
                    LoadFactor = min(1.0, (HeatingCapacityHS - BuildingHeatingLoad) / (HeatingCapacityHS - HeatingCapacityLS));
                    LoadFactor = max(0.0, LoadFactor);

                    NetTotHeatCapBinned = BuildingHeatingLoad;
                    TotHeatElecPowerBinnedHP = LoadFactor * HeatingElecPowerLS + (1.0 - LoadFactor) * HeatingElecPowerHS;
                    TotHeatElecPowerBinnedHP *= LowTempCutOutFactor;
                    TotHeatElecPowerBinnedRH = BuildingHeatingLoad * (1.0 - LowTempCutOutFactor);
                    goto HeatSpeedLoop_exit;

                } else if (BuildingHeatingLoad >= HeatingCapacityMax) {
                    NetTotHeatCapBinned = BuildingHeatingLoad;
                    if (!OATempCompressorOnOffBlank && HeatingElecPowerMax > 0.0) {
                        if ((OutdoorBinTemperature[BinNum] <= OATempCompressorOff) || (HeatingCapacityMax / HeatingElecPowerMax < 1.0)) {
                            LowTempCutOutFactor = 0.0;
                        } else if ((OutdoorBinTemperature[BinNum] > OATempCompressorOff && OutdoorBinTemperature[BinNum] <= OATempCompressorOn) &&
                                   (HeatingCapacityMax / HeatingElecPowerMax > 1.0)) {
                            LowTempCutOutFactor = 0.5;
                        } else if ((OutdoorBinTemperature[BinNum] > OATempCompressorOn) && (HeatingCapacityMax / HeatingElecPowerMax > 1.0)) {
                            LowTempCutOutFactor = 1.0;
                        }
                    } else {
                        LowTempCutOutFactor = 1.0;
                    }

                    TotHeatElecPowerBinnedHP = HeatingElecPowerMax * LowTempCutOutFactor;
                    TotHeatElecPowerBinnedRH = BuildingHeatingLoad - HeatingCapacityMax * LowTempCutOutFactor;
                    goto HeatSpeedLoop_exit;
                }
            }
        HeatSpeedLoop_exit:;

            NetHeatingCapWeighted += NetTotHeatCapBinned * FractionalBinHours;
            TotHeatingElecPowerWeighted += (TotHeatElecPowerBinnedHP + TotHeatElecPowerBinnedRH) * FractionalBinHours;
        }

        if (DefrostControl == HPdefrostControl::Timed) {
            DemandDeforstCredit = 1.0; // Timed defrost control
        } else {
            DemandDeforstCredit = 1.03; // Demand defrost control
        }

        if (TotHeatingElecPowerWeighted > 0.0) {
            HSPF = NetHeatingCapWeighted * DemandDeforstCredit / TotHeatingElecPowerWeighted;
        } else {
            HSPF = 0.0;
        }

        return std::make_tuple(NetHeatingCapRatedHighTemp, NetHeatingCapRatedLowTemp, HSPF);
    }

    std::tuple<Real64, Real64, Real64> MultiSpedDXHeatingCoilHSPF2(
        EnergyPlusData &state,
        int const nsp,                                                // Number of compressor speed
        Array1A<Real64> const MSFanPowerPerEvapAirFlowRateInput_2023, // 2023 rated fan power per evap air flow rate [W/(m3/s)]
        Array1A_int const CapFTempCurveIndex,                         // Index for the capacity as a function of temperature modifier curve
        Array1A_int const CapFFlowCurveIndex,                         // Index for the capacity as a function of flow fraction modifier curve
        Array1A<Real64> const RatedTotalCapacity,                     // Reference capacity of DX coil [W]
        Array1A<Real64> const RatedAirVolFlowRate,                    // Reference air flow rate of DX coil [m3/s]
        Array1A_int const EIRFFlowCurveIndex,                         // Index for the EIR as a function of flow fraction modifier curve
        Array1A_int const EIRFTempCurveIndex,                         // Index for the EIR as a function of temperature modifier curve
        Array1A<Real64> const RatedCOP,                               // Reference coefficient of performance [W/W]
        Optional_int_const RegionNum,                                 // Region number for calculating HSPF of single speed DX heating coil
        Optional<Real64 const> MinOATCompressor,                      // Minimum OAT for heat pump compressor operation [C]
        Optional<Real64 const> OATempCompressorOn,                    // The outdoor temperature when the compressor is automatically turned
        Optional_bool_const OATempCompressorOnOffBlank,               // Flag used to determine low temperature cut out factor
        Optional<HPdefrostControl const> DefrostControl)              // defrost control; 1=timed, 2=on-demand
    {

        // Intermediate values calculated from the inputs in the idf file
        Real64 HSPF2_2023(0.0);
        Real64 NetHeatingCapRatedHighTemp_2023(0.0);
        Real64 NetHeatingCapRatedLowTemp_2023(0.0);

        int BinNum2023; // bin number counter
        int spnum;      // compressor speed number

        Array1D<Real64> FanPowerPerEvapAirFlowRate_2023(nsp); // Fan power per air volume flow rate through the evaporator coil [W/(m3/s)]

        // Real64 HeatingCapacityLS;                  // cooling capacity of Mult-speed DX coil at lower speed, [W]
        // Real64 HeatingCapacityHS;                  // cooling capacity of Mult-speed DX coil at higher speed, [W]
        // Real64 HeatingElecPowerLS;                 // outdoor unit electric power input at low speed, [W]
        // Real64 HeatingElecPowerHS;                 // outdoor unit electric power input at high speed, [W]
        // Real64 HeatingCapacityMax;                 // cooling capacity of Mult-speed DX coil at max speed, [W]
        // Real64 HeatingElecPowerMax;                // outdoor unit electric power input at Max speed, [W]
        // Array1D<Real64> TotHeatCapTestH1High(nsp); // net heating capacity high speed at H1 test conditon, [W]

        // Intermediate values calculated from the inputs in the idf file
        Array1D<Real64> TotCapFlowModFac(nsp); // Total capacity modifier f(actual flow vs rated flow) for each speed [-]
        Array1D<Real64> EIRFlowModFac(nsp);    // EIR modifier f(actual supply air flow vs rated flow) for each speed [-]

        Real64 TotCapTempModFacH0Low(0.0);  // Tot capacity modifier (function of entering wetbulb, outside drybulb) at H0 Low Test [-]
        Real64 EIRTempModFacH0Low(0.0);     // EIR modifier (function of entering wetbulb, outside drybulb) at H0 Low Test[-]
        Real64 TotCapTempModFacH1Low(0.0);  // Tot capacity modifier (function of entering wetbulb, outside drybulb) at H1 Low Test [-]
        Real64 EIRTempModFacH1Low(0.0);     // EIR modifier (function of entering wetbulb, outside drybulb) at H1 Low Test[-]
        Real64 TotCapTempModFacH2Int(0.0);  // Tot capacity modifier (function of entering wetbulb, outside drybulb) at H2 Int Test [-]
        Real64 EIRTempModFacH2Int(0.0);     // EIR modifier (function of entering wetbulb, outside drybulb) at H2 Int Test[-]
        Real64 TotCapTempModFacH1Full(0.0); // Tot capacity modifier (function of entering wetbulb, outside drybulb) at H1 Full Test [-]
        Real64 EIRTempModFacH1Full(0.0);    // EIR modifier (function of entering wetbulb, outside drybulb) at H1 Full Test[-]
        Real64 TotCapTempModFacH2Full(0.0); // Tot capacity modifier (function of entering wetbulb, outside drybulb) at H2 Full Test [-]
        Real64 EIRTempModFacH2Full(0.0);    // EIR modifier (function of entering wetbulb, outside drybulb) at H2 Full Test[-]
        Real64 TotCapTempModFacH3Full(0.0); // Tot capacity modifier (function of entering wetbulb, outside drybulb) at H3 Full Test [-]
        Real64 EIRTempModFacH3Full(0.0);    // EIR modifier (function of entering wetbulb, outside drybulb) at H3 Full Test[-]
        Real64 TotCapTempModFacH4Full(0.0); // Tot capacity modifier (function of entering wetbulb, outside drybulb) at H4 Full Test [-]
        Real64 EIRTempModFacH4Full(0.0);    // EIR modifier (function of entering wetbulb, outside drybulb) at H4 Full Test[-]

        Real64 OATempCompressorOff(0.0); // Minimum outdoor air temperature to turn the commpressor off

        Real64 NetHeatingCapWeighted(0.0);       // net total heating cap weighted by the fraction of the binned cooling hours [W]
        Real64 TotHeatingElecPowerWeighted(0.0); // net total heat pump and resistance heating electric Energy input weighted by
        // the fraction of the binned cooling hours

        Real64 n(0.0);     // Fractional bin hours for the heating season  [-]
        Real64 f_def(1.0); // Demand Defrost Credit, A factor to adjust HSPF if coil has demand defrost control  [-]

        NetHeatingCapWeighted = 0.0;
        TotHeatingElecPowerWeighted = 0.0;

        for (spnum = 1; spnum <= nsp; ++spnum) {
            FanPowerPerEvapAirFlowRate_2023(spnum) = 0.0;
            if (MSFanPowerPerEvapAirFlowRateInput_2023(spnum) <= 0.0) {
                FanPowerPerEvapAirFlowRate_2023(spnum) = DefaultFanPowerPerEvapAirFlowRateSEER2;
            } else {
                FanPowerPerEvapAirFlowRate_2023(spnum) = MSFanPowerPerEvapAirFlowRateInput_2023(spnum);
            }
        }

        Real64 HeatingOutdoorCoilInletAirDBTemp_H0LowTest = 16.66; // Outdoor air dry-bulb temp in degrees 16.66 C (62 F)
        Real64 HeatingIndoorCoilInletAirDBTemp_H0LowTest = 21.11;  // Indoor air dry-bulb temp in degrees 21.11 C (70 F)

        Real64 HeatingOutdoorCoilInletAirDBTemp_H1LowTest = 8.33; // Outdoor air dry-bulb temp in degrees 8.33 C (47 F)
        Real64 HeatingIndoorCoilInletAirDBTemp_H1LowTest = 21.11; // Indoor air dry-bulb temp in degrees 21.11 C (70 F)

        Real64 HeatingOutdoorCoilInletAirDBTemp_H2IntTest = 1.66; // Outdoor air dry-bulb temp in degrees 1.66 C (35 F)
        Real64 HeatingIndoorCoilInletAirDBTemp_H2IntTest = 21.11; // Indoor air dry-bulb temp in degrees 21.11 C (70 F)

        Real64 HeatingOutdoorCoilInletAirDBTemp_H1FullTest = 8.33; // Outdoor air dry-bulb temp in degrees 8.33 C (47 F)
        Real64 HeatingIndoorCoilInletAirDBTemp_H1FullTest = 21.11; // Indoor air dry-bulb temp in degrees 21.11 C (70 F)

        Real64 HeatingOutdoorCoilInletAirDBTemp_H2FullTest = 1.66; // Outdoor air dry-bulb temp in degrees  C (35 F)
        Real64 HeatingIndoorCoilInletAirDBTemp_H2FullTest = 21.11; // Indoor air dry-bulb temp in degrees 21.11 C (70 F)

        Real64 HeatingOutdoorCoilInletAirDBTemp_H3FullTest = -8.33; // Outdoor air dry-bulb temp in degrees  C (17 F)
        Real64 HeatingIndoorCoilInletAirDBTemp_H3FullTest = 21.11;  // Indoor air dry-bulb temp in degrees 21.11 C (70 F)

        Real64 HeatingOutdoorCoilInletAirDBTemp_H4FullTest = -15;  // Outdoor air dry-bulb temp in degrees  C (5 F)
        Real64 HeatingIndoorCoilInletAirDBTemp_H4FullTest = 21.11; // Indoor air dry-bulb temp in degrees 21.11 C (70 F)

        Array1D<Real64> Q_A_Full(nsp);

        Array1D<Real64> Q_H0_Low(nsp);  // Total cooling capacity at H0 Low test condition (Low speed)
        Array1D<Real64> Q_H1_Low(nsp);  // Total cooling capacity at H1 Low test condition (Low speed)
        Array1D<Real64> Q_H2_Int(nsp);  // Total cooling capacity at H2 Int test condition
        Array1D<Real64> Q_H1_Full(nsp); // Total cooling capacity at H1 Full test condition (High speed)
        Array1D<Real64> Q_H2_Full(nsp); // Total cooling capacity at H2 Full test condition (High speed)
        Array1D<Real64> Q_H3_Full(nsp); // Total cooling capacity at H3 Full test condition (High speed)
        Array1D<Real64> Q_H4_Full(nsp); // Total cooling capacity at H4 Full test condition (High speed)

        Array1D<Real64> P_H0_Low(nsp);  // Outdoor Unit electric power at H0 Low test condition (Low speed)
        Array1D<Real64> P_H1_Low(nsp);  // Outdoor Unit electric power at H1 Low test condition (Low speed)
        Array1D<Real64> P_H2_Int(nsp);  // Outdoor Unit electric power at H2 Int test condition
        Array1D<Real64> P_H1_Full(nsp); // Outdoor Unit electric power at H1 Full test condition (Full speed)
        Array1D<Real64> P_H2_Full(nsp); // Outdoor Unit electric power at H2 Full test condition (Full speed)
        Array1D<Real64> P_H3_Full(nsp); // Outdoor Unit electric power at H3 Full test condition (Full speed)
        Array1D<Real64> P_H4_Full(nsp); // Outdoor Unit electric power at H4 Full test condition (Full speed)

        // Proceed withe HSPF2 value calculation
        for (spnum = 1; spnum <= nsp; ++spnum) {
            TotCapFlowModFac(spnum) = CurveManager::CurveValue(state, CapFFlowCurveIndex(spnum), AirMassFlowRatioRated);

            if (state.dataCurveManager->PerfCurve(CapFTempCurveIndex(spnum)).NumDims == 1) {

                TotCapTempModFacH0Low = CurveManager::CurveValue(state, CapFTempCurveIndex(spnum), HeatingOutdoorCoilInletAirDBTemp_H0LowTest);
                TotCapTempModFacH1Low = CurveManager::CurveValue(state, CapFTempCurveIndex(spnum), HeatingOutdoorCoilInletAirDBTemp_H1LowTest);
                TotCapTempModFacH2Int = CurveManager::CurveValue(state, CapFTempCurveIndex(spnum), HeatingOutdoorCoilInletAirDBTemp_H2IntTest);
                TotCapTempModFacH1Full = CurveManager::CurveValue(state, CapFTempCurveIndex(spnum), HeatingOutdoorCoilInletAirDBTemp_H1FullTest);
                TotCapTempModFacH2Full = CurveManager::CurveValue(state, CapFTempCurveIndex(spnum), HeatingOutdoorCoilInletAirDBTemp_H2FullTest);
                TotCapTempModFacH3Full = CurveManager::CurveValue(state, CapFTempCurveIndex(spnum), HeatingOutdoorCoilInletAirDBTemp_H3FullTest);
                TotCapTempModFacH4Full = CurveManager::CurveValue(state, CapFTempCurveIndex(spnum), HeatingOutdoorCoilInletAirDBTemp_H4FullTest);
            } else {
                TotCapTempModFacH0Low = CurveManager::CurveValue(
                    state, CapFTempCurveIndex(spnum), HeatingIndoorCoilInletAirDBTemp_H0LowTest, HeatingOutdoorCoilInletAirDBTemp_H0LowTest);
                TotCapTempModFacH1Low = CurveManager::CurveValue(
                    state, CapFTempCurveIndex(spnum), HeatingIndoorCoilInletAirDBTemp_H1LowTest, HeatingOutdoorCoilInletAirDBTemp_H1LowTest);
                TotCapTempModFacH2Int = CurveManager::CurveValue(
                    state, CapFTempCurveIndex(spnum), HeatingIndoorCoilInletAirDBTemp_H2IntTest, HeatingOutdoorCoilInletAirDBTemp_H2IntTest);
                TotCapTempModFacH1Full = CurveManager::CurveValue(
                    state, CapFTempCurveIndex(spnum), HeatingIndoorCoilInletAirDBTemp_H1FullTest, HeatingOutdoorCoilInletAirDBTemp_H1FullTest);
                TotCapTempModFacH2Full = CurveManager::CurveValue(
                    state, CapFTempCurveIndex(spnum), HeatingIndoorCoilInletAirDBTemp_H2FullTest, HeatingOutdoorCoilInletAirDBTemp_H2FullTest);
                TotCapTempModFacH3Full = CurveManager::CurveValue(
                    state, CapFTempCurveIndex(spnum), HeatingIndoorCoilInletAirDBTemp_H3FullTest, HeatingOutdoorCoilInletAirDBTemp_H3FullTest);
                TotCapTempModFacH4Full = CurveManager::CurveValue(
                    state, CapFTempCurveIndex(spnum), HeatingIndoorCoilInletAirDBTemp_H4FullTest, HeatingOutdoorCoilInletAirDBTemp_H4FullTest);
            }

            Q_A_Full(spnum) = RatedTotalCapacity(spnum) *
                                  CurveManager::CurveValue(
                                      state, CapFTempCurveIndex(spnum), IndoorCoilInletAirWetBulbTempRated, OutdoorCoilInletAirDryBulbTempTestA2) *
                                  TotCapFlowModFac(spnum) -
                              FanPowerPerEvapAirFlowRate_2023(spnum) * RatedAirVolFlowRate(spnum);

            Q_H0_Low(spnum) = RatedTotalCapacity(spnum) * TotCapTempModFacH0Low * TotCapFlowModFac(spnum) +
                              FanPowerPerEvapAirFlowRate_2023(spnum) * RatedAirVolFlowRate(spnum);
            Q_H1_Low(spnum) = RatedTotalCapacity(spnum) * TotCapTempModFacH1Low * TotCapFlowModFac(spnum) +
                              FanPowerPerEvapAirFlowRate_2023(spnum) * RatedAirVolFlowRate(spnum);
            Q_H2_Int(spnum) = RatedTotalCapacity(spnum) * TotCapTempModFacH2Int * TotCapFlowModFac(spnum) +
                              FanPowerPerEvapAirFlowRate_2023(spnum) * RatedAirVolFlowRate(spnum);
            Q_H1_Full(spnum) = RatedTotalCapacity(spnum) * TotCapTempModFacH1Full * TotCapFlowModFac(spnum) +
                               FanPowerPerEvapAirFlowRate_2023(spnum) * RatedAirVolFlowRate(spnum);
            Q_H2_Full(spnum) = RatedTotalCapacity(spnum) * TotCapTempModFacH2Full * TotCapFlowModFac(spnum) +
                               FanPowerPerEvapAirFlowRate_2023(spnum) * RatedAirVolFlowRate(spnum);
            Q_H3_Full(spnum) = RatedTotalCapacity(spnum) * TotCapTempModFacH3Full * TotCapFlowModFac(spnum) +
                               FanPowerPerEvapAirFlowRate_2023(spnum) * RatedAirVolFlowRate(spnum);
            Q_H4_Full(spnum) = RatedTotalCapacity(spnum) * TotCapTempModFacH4Full * TotCapFlowModFac(spnum) +
                               FanPowerPerEvapAirFlowRate_2023(spnum) * RatedAirVolFlowRate(spnum);

            EIRFlowModFac(spnum) = CurveManager::CurveValue(state, EIRFFlowCurveIndex(spnum), AirMassFlowRatioRated);

            if (state.dataCurveManager->PerfCurve(EIRFTempCurveIndex(spnum)).NumDims == 1) {

                EIRTempModFacH0Low = CurveManager::CurveValue(state, EIRFTempCurveIndex(spnum), HeatingOutdoorCoilInletAirDBTemp_H0LowTest);

                EIRTempModFacH1Low = CurveManager::CurveValue(state, EIRFTempCurveIndex(spnum), HeatingOutdoorCoilInletAirDBTemp_H1LowTest);
                EIRTempModFacH2Int = CurveManager::CurveValue(state, EIRFTempCurveIndex(spnum), HeatingOutdoorCoilInletAirDBTemp_H2IntTest);
                EIRTempModFacH1Full = CurveManager::CurveValue(state, EIRFTempCurveIndex(spnum), HeatingOutdoorCoilInletAirDBTemp_H1FullTest);
                EIRTempModFacH2Full = CurveManager::CurveValue(state, EIRFTempCurveIndex(spnum), HeatingOutdoorCoilInletAirDBTemp_H2FullTest);
                EIRTempModFacH3Full = CurveManager::CurveValue(state, EIRFTempCurveIndex(spnum), HeatingOutdoorCoilInletAirDBTemp_H3FullTest);
                EIRTempModFacH4Full = CurveManager::CurveValue(state, EIRFTempCurveIndex(spnum), HeatingOutdoorCoilInletAirDBTemp_H4FullTest);

            } else {

                EIRTempModFacH0Low = CurveManager::CurveValue(
                    state, EIRFTempCurveIndex(spnum), HeatingIndoorCoilInletAirDBTemp_H0LowTest, HeatingOutdoorCoilInletAirDBTemp_H0LowTest);
                EIRTempModFacH1Low = CurveManager::CurveValue(
                    state, EIRFTempCurveIndex(spnum), HeatingIndoorCoilInletAirDBTemp_H1LowTest, HeatingOutdoorCoilInletAirDBTemp_H1LowTest);
                EIRTempModFacH2Int = CurveManager::CurveValue(
                    state, EIRFTempCurveIndex(spnum), HeatingIndoorCoilInletAirDBTemp_H2IntTest, HeatingOutdoorCoilInletAirDBTemp_H2IntTest);
                EIRTempModFacH1Full = CurveManager::CurveValue(
                    state, EIRFTempCurveIndex(spnum), HeatingIndoorCoilInletAirDBTemp_H1FullTest, HeatingOutdoorCoilInletAirDBTemp_H1FullTest);
                EIRTempModFacH2Full = CurveManager::CurveValue(
                    state, EIRFTempCurveIndex(spnum), HeatingIndoorCoilInletAirDBTemp_H2FullTest, HeatingOutdoorCoilInletAirDBTemp_H2FullTest);
                EIRTempModFacH3Full = CurveManager::CurveValue(
                    state, EIRFTempCurveIndex(spnum), HeatingIndoorCoilInletAirDBTemp_H3FullTest, HeatingOutdoorCoilInletAirDBTemp_H3FullTest);
                EIRTempModFacH4Full = CurveManager::CurveValue(
                    state, EIRFTempCurveIndex(spnum), HeatingIndoorCoilInletAirDBTemp_H4FullTest, HeatingOutdoorCoilInletAirDBTemp_H4FullTest);
            }

            if (RatedCOP(spnum) > 0.0) {

                P_H0_Low(spnum) = Q_H0_Low(spnum) * EIRFlowModFac(spnum) * EIRTempModFacH0Low / RatedCOP(spnum) +
                                  FanPowerPerEvapAirFlowRate_2023(spnum) * RatedAirVolFlowRate(spnum);
                P_H1_Low(spnum) = Q_H1_Low(spnum) * EIRFlowModFac(spnum) * EIRTempModFacH1Low / RatedCOP(spnum) +
                                  FanPowerPerEvapAirFlowRate_2023(spnum) * RatedAirVolFlowRate(spnum);
                P_H2_Int(spnum) = Q_H2_Int(spnum) * EIRFlowModFac(spnum) * EIRTempModFacH2Int / RatedCOP(spnum) +
                                  FanPowerPerEvapAirFlowRate_2023(spnum) * RatedAirVolFlowRate(spnum);
                P_H1_Full(spnum) = Q_H1_Full(spnum) * EIRFlowModFac(spnum) * EIRTempModFacH1Full / RatedCOP(spnum) +
                                   FanPowerPerEvapAirFlowRate_2023(spnum) * RatedAirVolFlowRate(spnum);
                P_H2_Full(spnum) = Q_H2_Full(spnum) * EIRFlowModFac(spnum) * EIRTempModFacH2Full / RatedCOP(spnum) +
                                   FanPowerPerEvapAirFlowRate_2023(spnum) * RatedAirVolFlowRate(spnum);
                P_H3_Full(spnum) = Q_H3_Full(spnum) * EIRFlowModFac(spnum) * EIRTempModFacH3Full / RatedCOP(spnum) +
                                   FanPowerPerEvapAirFlowRate_2023(spnum) * RatedAirVolFlowRate(spnum);
                P_H4_Full(spnum) = Q_H4_Full(spnum) * EIRFlowModFac(spnum) * EIRTempModFacH4Full / RatedCOP(spnum) +
                                   FanPowerPerEvapAirFlowRate_2023(spnum) * RatedAirVolFlowRate(spnum);
            }
        }

        // determine the HP capacity at the rated condition (AHRI H1 high speed test Condition); and determine the
        // the building heat requirement for the user specified region
        NetHeatingCapRatedHighTemp_2023 = Q_H1_Full(nsp);
        NetHeatingCapRatedLowTemp_2023 = Q_H3_Full(nsp);

        // The minimum temperature below which the compressor is turned off
        Real64 t_Off = MinOATCompressor; // Heating off | outdoor Minimum temperature below which the compressor ceases to operate
        // The outdoor temperature when the compressor is automatically turned
        Real64 t_On = OATempCompressorOn; // Heating On | outdoor temperature at which the compressor reinitiates operation

        Int64 RN = static_cast<int64_t>(RegionNum);

        Real64 q_sum(0.0);
        Real64 e_sum(0.0);
        Real64 rh_sum(0.0);

        // Equation 11.111 AHRI-2023
        Real64 t_ob = 7.22; //  temperature at which frosting influence on full stage performance begins 7.22 C (45 F)
        for (BinNum2023 = 0; BinNum2023 < 18; ++BinNum2023) { // NumOfOATempBins

            Real64 t = OutdoorBinTemperature[BinNum2023];
            n = FracBinHoursAtOutdoorBinTempHSPF2[RN - 1][BinNum2023];
            if (n == 0.0) {
                // we're skipping load calculations for any Temperature bin against which fractional hours are 0.0
                continue;
            }
            Real64 t_zl = ZoneLoadTemperature[RN - 1];
            Real64 t_od = OutdoorDesignTemperature[RN - 1];
            Real64 c_x = VariableSpeedLoadFactor[RN - 1];

            // For ANSI/AHRI 210/240 Standard 2023 | Concept of DHRI min and max is removed
            // Section 11.2.2.1 Equation 11.104  which suggests QAFull is used instead of DHRI min
            // While Calculaiting the Building load For heating-only heat pump units, replace Q_A_Full with Q_H_Full
            // Q_H_Full = the heating capacity at 47F determined from the H1N test for variable capacity systems and
            // from the H1Full test for other systems, Btu/h.
            Real64 bl = (t_zl - t) / (t_zl - t_od) * c_x * Q_H1_Full(nsp);

            Real64 q_full(0.0);
            Real64 p_full(0.0);
            Real64 cop_full(0.0);

            Real64 delta_full(0.0);
            Real64 hlf_full(0.0);
            Real64 e(0.0);
            Real64 rh(0.0);

            if (t >= t_ob) {
                q_full = Q_H3_Full(nsp) + (Q_H1_Full(nsp) - Q_H3_Full(nsp)) * ((t - (-8.33)) / (8.33 - (-8.33))); // Equation 11.112 AHRI-2023
                p_full = P_H3_Full(nsp) + (P_H1_Full(nsp) - P_H3_Full(nsp)) * ((t - (-8.33)) / (8.33 - (-8.33))); // Equation 11.117 AHRI-2023
            } else if (t >= (-8.33) && t < t_ob) {
                q_full = Q_H3_Full(nsp) + (Q_H2_Full(nsp) - Q_H3_Full(nsp)) * ((t - (-8.33)) / (1.66 - (-8.33)));  // Equation 11.113 AHRI-2023
                p_full = P_H3_Full(nsp) + (P_H2_Full(nsp) - P_H3_Full(nsp)) * ((t - (-8.33)) / (1.66 - (-8.33)));  // Equation 11.118 AHRI-2023
            } else if (t < (-8.33)) {                                                                              // if(t<(-8.33))
                q_full = Q_H4_Full(nsp) + (Q_H3_Full(nsp) - Q_H4_Full(nsp)) * ((t - (-8.33)) / ((-8.33) - (-15))); // Equation 11.114 AHRI-2023
                p_full = P_H4_Full(nsp) + (P_H3_Full(nsp) - P_H4_Full(nsp)) * ((t - (-8.33)) / ((-8.33) - (-15))); // Equation 11.119 AHRI-2023
            }

            cop_full = q_full / p_full;

            if (t <= t_Off || cop_full < 1.0) {
                delta_full = 0.0; // #Equation 11.125 AHRI-2023
            } else if (t > t_On) {
                delta_full = 1.0; // Equation 11.127 AHRI-2023
            } else {
                delta_full = 0.5; // #Equation 11.126 AHRI-2023
            }

            if (q_full > bl) {
                hlf_full = bl / q_full; // Equation 11.107 AHRI-2023
            } else {
                hlf_full = 1.0; // Equation 11.108 AHRI-2023
            }

            for (spnum = 1; spnum <= nsp - 1; ++spnum) {

                // Intermediate capacity
                Real64 q_H0_low = Q_H0_Low(spnum);
                Real64 q_H1_low = Q_H1_Low(spnum);
                Real64 q_H2_int = Q_H2_Int(spnum);
                Real64 q_H1_full = Q_H1_Full(spnum);
                Real64 q_H2_full = Q_H2_Full(spnum);
                Real64 q_H3_full = Q_H3_Full(spnum);
                Real64 q_H4_full = Q_H4_Full(spnum);
                // Equation 11.177 AHRI-2023
                //?? (replaced 62 with 35) in Ratio expression // (t=>35-47/62-47)
                Real64 q_35_low = // q_H1_low + (q_H0_low - q_H1_low) * ((t - (8.33)) / (1.66 - (8.33)));
                    q_H1_low + (q_H0_low - q_H1_low) * ((1.67 - (8.33)) / (16.67 - (8.33)));

                // Equation 11.191 AHRI-2023
                Real64 N_Hq = min(1.0, (q_H2_int - q_35_low) / (q_H2_full - q_35_low));
                N_Hq = max(0.0, N_Hq);
                // Equation 11.190 AHRI-2023
                Real64 M_Hq = (q_H0_low - q_H1_low) / (16.66 - 8.33) * (1.0 - N_Hq) + (q_H2_full - q_H3_full) / (1.66 - (-8.33)) * N_Hq;

                // Intermediate Power
                Real64 p_H0_low = P_H0_Low(spnum);
                Real64 p_H1_low = P_H1_Low(spnum);
                Real64 p_H2_int = P_H2_Int(spnum);
                Real64 p_H1_full = P_H1_Full(spnum);
                Real64 p_H2_full = P_H2_Full(spnum);
                Real64 p_H3_full = P_H3_Full(spnum);
                Real64 p_H4_full = P_H4_Full(spnum);
                // Equation 11.178 AHRI - 2023
                //?? (replaced 62 with 35) in Ratio expression (t=>35 F-47/35-47)
                Real64 p_35_low = // p_H1_low + (p_H0_low - p_H1_low) * ((t - (8.33)) / (1.66 - (8.33)));
                    p_H1_low + (p_H0_low - p_H1_low) * ((1.67 - (8.33)) / (16.67 - (8.33)));

                // Equation 11.194 AHRI-2023
                Real64 N_HE = min(1.0, (p_H2_int - p_35_low) / (p_H2_full - p_35_low));
                N_HE = max(0.0, N_HE);

                // Equation 11.193 AHRI-2023
                Real64 M_HE = (p_H0_low - p_H1_low) / (16.66 - 8.33) * (1.0 - N_HE) + (p_H2_full - p_H3_full) / (1.66 - (-8.33)) * N_HE;

                // Note: this is strange that there is no defrost cut in the low speed and doesn't use H2 or H3 low
                // Equation 11.177 AHRI-2023
                Real64 q_low = q_H1_low + (q_H0_low - q_H1_low) * ((t - (8.33)) / (16.66 - (8.33)));
                // Equation 11.178 AHRI-2023
                Real64 p_low = p_H1_low + (p_H0_low - p_H1_low) * ((t - (8.33)) / (16.66 - (8.33)));
                Real64 q_hs(0.0);
                Real64 p_hs(0.0);
                // Low Speed
                if (t < -8.33) {
                    q_low = Q_H3_Full(spnum) + ((Q_H1_Low(spnum) - Q_H3_Full(spnum)) * (t - HeatingOutdoorCoilInletAirDBTempH3Test) /
                                                (HeatingOutdoorCoilInletAirDBTempRated - HeatingOutdoorCoilInletAirDBTempH3Test));

                    p_low = P_H3_Full(spnum) + ((P_H1_Low(spnum) - P_H3_Full(spnum)) * (t - HeatingOutdoorCoilInletAirDBTempH3Test) /
                                                (HeatingOutdoorCoilInletAirDBTempRated - HeatingOutdoorCoilInletAirDBTempH3Test));
                } else if (t >= 4.44) {
                    q_low = Q_H1_Low(spnum) + ((Q_H0_Low(spnum) - Q_H1_Low(spnum)) * (t - HeatingOutdoorCoilInletAirDBTempRated) /
                                               (HeatingOutdoorCoilInletAirDBTempH0Test - HeatingOutdoorCoilInletAirDBTempRated));
                    p_low = P_H1_Low(spnum) + ((P_H0_Low(spnum) - P_H1_Low(spnum)) * (t - HeatingOutdoorCoilInletAirDBTempRated) /
                                               (HeatingOutdoorCoilInletAirDBTempH0Test - HeatingOutdoorCoilInletAirDBTempRated));
                } else {
                    q_low = Q_H3_Full(spnum) + ((Q_H2_Full(spnum) - Q_H3_Full(spnum)) * (t - HeatingOutdoorCoilInletAirDBTempH3Test) /
                                                (HeatingOutdoorCoilInletAirDBTempH2Test - HeatingOutdoorCoilInletAirDBTempH3Test));
                    p_low = P_H3_Full(spnum) + ((P_H2_Full(spnum) - P_H3_Full(spnum)) * (t - HeatingOutdoorCoilInletAirDBTempH3Test) /
                                                (HeatingOutdoorCoilInletAirDBTempH2Test - HeatingOutdoorCoilInletAirDBTempH3Test));
                }

                // High Speed
                if ((t <= -8.33) || (t >= 7.20)) {
                    q_hs = Q_H3_Full(spnum + 1) + ((Q_H1_Full(spnum + 1) - Q_H3_Full(spnum + 1)) * (t - HeatingOutdoorCoilInletAirDBTempH3Test) /
                                                   (HeatingOutdoorCoilInletAirDBTempRated - HeatingOutdoorCoilInletAirDBTempH3Test));
                    p_hs = P_H3_Full(spnum + 1) + ((P_H1_Full(spnum + 1) - P_H3_Full(spnum + 1)) * (t - HeatingOutdoorCoilInletAirDBTempH3Test) /
                                                   (HeatingOutdoorCoilInletAirDBTempRated - HeatingOutdoorCoilInletAirDBTempH3Test));
                } else {
                    q_hs = Q_H3_Full(spnum + 1) + ((Q_H2_Full(spnum + 1) - Q_H3_Full(spnum + 1)) * (t - HeatingOutdoorCoilInletAirDBTempH3Test) /
                                                   (HeatingOutdoorCoilInletAirDBTempH2Test - HeatingOutdoorCoilInletAirDBTempH3Test));
                    p_hs = P_H3_Full(spnum + 1) + ((P_H2_Full(spnum + 1) - P_H3_Full(spnum + 1)) * (t - HeatingOutdoorCoilInletAirDBTempH3Test) /
                                                   (HeatingOutdoorCoilInletAirDBTempH2Test - HeatingOutdoorCoilInletAirDBTempH3Test));
                }

                Real64 cop_low = q_low / p_low;

                Real64 q_int = q_H2_int + M_Hq * (t - (1.66));
                Real64 p_int = p_H2_int + M_HE * (t - (1.66));
                Real64 cop_int = q_int / p_int;

                Real64 delta_low(0.0);
                if (bl <= q_low) {
                    // CASE 1 : Section 11.2.2.3.1 AHRI-2023
                    // Building Load is less than the capacity of the unit at the Low Compressor Speed (q_low >= bl)
                    if (t <= t_Off || cop_low < 1.0) {
                        delta_low = 0.0; //  Equation 11.159 AHRI-2023
                    } else if (t > t_On) {
                        delta_low = 1.0; // Equation 11.160 AHRI-2023
                    } else {
                        delta_low = 0.5; // Equation 11.161 AHRI-2023
                    }

                    Real64 hlf_low(0.0); // Par tLoad Ratio
                    if (q_low > 0.0) {
                        hlf_low = min(1.0, bl / q_low); // Equation 11.155 AHRI-2023
                    }
                    // Section 6.1.3.2.3 (AHRI-2023) For Variable Capacity Systems, if the optional H1CFull and H1CLow tests are not
                    // performed, a default value of 0.25 shall be used for the heating Degradation Coefficient
                    Real64 CyclicMSHeatingDegradationCoeffHSPF2 = 0.25;
                    // Part Load Fration
                    Real64 plf_low = 1.0 - CyclicMSHeatingDegradationCoeffHSPF2 * (1.0 - hlf_low); // Equation 11.156 AHRI-2023
                    // e = p_low * hlf_low * delta_low * n / plf_low;                                 // Equation 11.153 AHRI-2023
                    e = (hlf_low / plf_low) * p_low * delta_low * n;
                    rh = bl * (1.0 - delta_low) * n; // Equation 11.154 AHRI-2023
                    goto HeatSpeedLoop4_exit;
                } else if (bl > q_low && bl < q_hs) {
                    // (bl > q_low && bl < q_full) {
                    // CASE 2 : 11.2.2.3.2 AHRI-2023
                    // Building load can be matched by modulating the compressor speed between low speed and full speed, q_low < bl < q_full
                    Real64 cop_int_bin(0.0);
                    Real64 delta_int_bin(0.0);
                    if (bl <= q_int) {
                        cop_int_bin = cop_low + ((cop_int - cop_low) / (q_int - q_low)) * (bl - q_low);   // Equation 11.187 AHRI-2023
                    } else {                                                                              // if bl > q_int
                        cop_int_bin = cop_int + ((cop_full - cop_int) / (q_full - q_int)) * (bl - q_int); // Equation 11.188 AHRI-2023
                    }

                    delta_int_bin = 0.0;
                    if (!OATempCompressorOnOffBlank) {
                        if (t <= t_Off) {
                            delta_int_bin = 0.0;
                        } else if (t > t_Off && t <= t_On) {
                            delta_int_bin = 0.5;
                        } else {
                            delta_int_bin = 1.0;
                        }
                    } else {
                        delta_int_bin = 1.0;
                    }

                    rh = bl * (1.0 - delta_int_bin) * n;
                    Real64 q = bl * n;                   // Equation 11.185 AHRI-2023
                    e = q / cop_int_bin * delta_int_bin; // Equaiton 11.186 AHRI-2023
                    goto HeatSpeedLoop4_exit;
                } else if (bl >= q_full) {
                    // CASE 3 : 11.2.2.3.3 AHRI-2023
                    // Building Load is greater than the capacity of the unit at the Full Compressor Speed, q_full <= bl or (bl >= q_full:)
                    if (t > (-15) || t <= (-8.33)) {
                        Real64 t_ratio = (t - (-15)) / ((-8.33) - (-15));
                        // Equation 11.203 AHRI-2023
                        q_full = q_H4_full + (q_H3_full - q_H4_full) * t_ratio;
                        // Equation 11.204 AHRI-2023
                        p_full = p_H4_full + (p_H3_full - p_H4_full) * t_ratio;
                    } else if (t < (-15)) {
                        Real64 t_ratio = (t - (-15)) / (8.33 - (-8.33));
                        // Equation 11.205 AHRI-2023
                        q_full = q_H4_full + (q_H1_full - q_H3_full) * t_ratio;
                        // Equation 11.206 AHRI-2023
                        p_full = p_H4_full + (p_H1_full - p_H3_full) * t_ratio;
                    }

                    // if not conducting H4 Test then use this block
                    // if (t >= t_ob || t <= (-8.33)) {
                    //     Real64 t_ratio = (t - (-8.33)) / ((8.33) - (-8.33));
                    //     // Equation 11.199 AHRI-2023
                    //     q_full = q_H3_full + (q_H1_full - q_H3_full) * t_ratio;
                    //     // Equation 11.200 AHRI-2023
                    //     p_full = p_H3_full + (p_H1_full - p_H3_full) * t_ratio;
                    // } else if ((-8.33) < t && t < t_ob) {
                    //     Real64 t_ratio = (t - (-8.33)) / (1.66 - (-8.33));
                    //     // Equation 11.201 AHRI-2023
                    //     q_full = q_H3_full + (q_H2_full - q_H3_full) * t_ratio;
                    //     // Equation 11.202 AHRI-2023
                    //     p_full = p_H3_full + (p_H2_full - p_H3_full) * t_ratio;
                    // }

                    if (!OATempCompressorOnOffBlank && p_full > 0.0) {
                        if ((t <= OATempCompressorOff) || (q_full / p_full < 1.0)) {
                            delta_full = 0.0;
                        } else if ((t > OATempCompressorOff && t <= OATempCompressorOn) && (q_full / p_full > 1.0)) {
                            delta_full = 0.5;
                        } else if ((t > OATempCompressorOn) && (q_full / p_full > 1.0)) {
                            delta_full = 1.0;
                        }
                    } else {
                        delta_full = 1.0;
                    }

                    hlf_full = 1.0; // Equation 11.170 AHRI-2023

                    // Equaiton 11.168 AHRI-2023
                    e = p_full * hlf_full * delta_full * n;
                    // Equation 11.169 AHRI - 2023
                    rh = (bl - q_full * hlf_full * delta_full) * n;
                    goto HeatSpeedLoop4_exit;
                }
            }

        HeatSpeedLoop4_exit:;
            q_sum += n * bl;
            e_sum += e;
            rh_sum += rh;
        }

        Real64 defrost_period = 90;   // Time between defrost terminations (for testing) in minutes
        Real64 defrost_MaxTime = 720; // Maximum time between defrosts allowed by controls in minutes
        Real64 t_Test_90 = max(defrost_period, 90.00);
        Real64 t_Max_90 = min(defrost_MaxTime, 720.00);
        // default t_Test_90/t_Max_90 = 0.125
        if (DefrostControl == HPdefrostControl::Timed) {
            // Equation 11.106 AHRI-2023
            f_def = 1.0; // Timed defrost control
        } else {
            // Equation 11.105 AHRI-2023
            f_def = 1 + 0.03 * (1 - (t_Test_90 / t_Max_90)); // Demand defrost control
            // f_def = 1.03;
        }

        if (e_sum > 0.0) {
            HSPF2_2023 = (q_sum / (e_sum + rh_sum)) * f_def; // Equation 11.103
        } else {
            HSPF2_2023 = 0.0;
        }

        return std::make_tuple(NetHeatingCapRatedHighTemp_2023, NetHeatingCapRatedLowTemp_2023, HSPF2_2023);
    }

    std::map<std::string, Real64> MultiSpeedDXHeatingCoilStandardRatings(
        EnergyPlusData &state,
        [[maybe_unused]] std::string const &DXCoilName,               // Name of DX coil for which HSPF is calculated
        [[maybe_unused]] std::string const &DXCoilType,               // Type of DX coil for which HSPF is calculated
        Array1A_int const CapFTempCurveIndex,                         // Index for the capacity as a function of temperature modifier curve
        Array1A_int const CapFFlowCurveIndex,                         // Index for the capacity as a function of flow fraction modifier curve
        Array1A_int const EIRFTempCurveIndex,                         // Index for the EIR as a function of temperature modifier curve
        Array1A_int const EIRFFlowCurveIndex,                         // Index for the EIR as a function of flow fraction modifier curve
        Array1A_int const PLFFPLRCurveIndex,                          // Index for the PLF vs part-load ratio curve
        Array1A<Real64> const RatedTotalCapacity,                     // Reference capacity of DX coil [W]
        Array1A<Real64> const RatedCOP,                               // Reference coefficient of performance [W/W]
        Array1A<Real64> const RatedAirVolFlowRate,                    // Reference air flow rate of DX coil [m3/s]
        Array1A<Real64> const MSFanPowerPerEvapAirFlowRateInput,      // 2017 rated fan power per evap air flow rate [W/(m3/s)]
        Array1A<Real64> const MSFanPowerPerEvapAirFlowRateInput_2023, // 2023 rated fan power per evap air flow rate [W/(m3/s)]
        int const nsp,                                                // Number of compressor speeds
        Optional_int_const RegionNum,                                 // Region number for calculating HSPF of single speed DX heating coil
        Optional<Real64 const> MinOATCompressor,                      // Minimum OAT for heat pump compressor operation [C]
        Optional<Real64 const> OATempCompressorOn,                    // The outdoor temperature when the compressor is automatically turned
        Optional_bool_const OATempCompressorOnOffBlank,               // Flag used to determine low temperature cut out factor
        Optional<HPdefrostControl const> DefrostControl               // defrost control; 1=timed, 2=on-demand
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Nigusse, FSEC
        //       DATE WRITTEN   December 2012
        //       MODIFIED
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Calculates the standard ratings net heating capacity and HSPF values for multi speed DX heating coils
        // at the AHRI standard test condition(s).

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // Using/Aliasing
        using CurveManager::CurveValue;

        // Argument array dimensioning
        CapFTempCurveIndex.dim(nsp);
        CapFFlowCurveIndex.dim(nsp);
        EIRFTempCurveIndex.dim(nsp);
        EIRFFlowCurveIndex.dim(nsp);
        PLFFPLRCurveIndex.dim(nsp);
        RatedTotalCapacity.dim(nsp);
        RatedCOP.dim(nsp);
        RatedAirVolFlowRate.dim(nsp);
        MSFanPowerPerEvapAirFlowRateInput.dim(nsp);
        MSFanPowerPerEvapAirFlowRateInput_2023.dim(nsp);

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // back on, if applicable, following automatic shut off. This field is
        // used only for HSPF calculation. [C]

        // SUBROUTINE PARAMETER DEFINITIONS:
        // CHARACTER(len=*), PARAMETER      :: RoutineName='MultiSpeedDXHeatingCoilStandardRatings: ' ! Include trailing blank space

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        Real64 NetHeatingCapRatedHighTemp = 0.0; // net heating capacity at maximum speed and High Temp
        Real64 NetHeatingCapRatedLowTemp = 0.0;  // net heating capacity at maximum speed and low Temp
        Real64 HSPF = 0.0;                       // seasonale energy efficiency ratio of multi speed DX cooling coil

        Real64 NetHeatingCapRatedHighTemp_2023 = 0.0; // net heating capacity at maximum speed and High Temp
        Real64 NetHeatingCapRatedLowTemp_2023 = 0.0;  // net heating capacity at maximum speed and low Temp
        Real64 HSPF2_2023 = 0.0;                      // seasonale energy efficiency ratio of multi speed DX cooling coil

        std::map<std::string, Real64> StandardRatingsResult;
        // StandardRatingsResult["NetHeatingCapRatedHighTemp"] = NetHeatingCapRatedHighTemp;
        // StandardRatingsResult["NetHeatingCapRatedLowTemp"] = NetHeatingCapRatedLowTemp;
        // StandardRatingsResult["HSPF"] = HSPF;
        // StandardRatingsResult["NetHeatingCapRatedHighTemp_2023"] = NetHeatingCapRatedHighTemp_2023;
        // StandardRatingsResult["NetHeatingCapRatedLowTemp_2023"] = NetHeatingCapRatedLowTemp_2023;
        // StandardRatingsResult["HSPF2_2023"] = HSPF2_2023;

        // HSPF Calculation | AHRI 2017 Std.
        std::tie(NetHeatingCapRatedHighTemp, NetHeatingCapRatedLowTemp, HSPF) = MultiSpedDXHeatingCoilHSPF(state,
                                                                                                           nsp,
                                                                                                           MSFanPowerPerEvapAirFlowRateInput,
                                                                                                           CapFTempCurveIndex,
                                                                                                           CapFFlowCurveIndex,
                                                                                                           RatedTotalCapacity,
                                                                                                           RatedAirVolFlowRate,
                                                                                                           EIRFFlowCurveIndex,
                                                                                                           EIRFTempCurveIndex,
                                                                                                           RatedCOP,
                                                                                                           RegionNum,
                                                                                                           MinOATCompressor,
                                                                                                           OATempCompressorOnOffBlank,
                                                                                                           OATempCompressorOn,
                                                                                                           DefrostControl);

        StandardRatingsResult["NetHeatingCapRatedHighTemp"] = NetHeatingCapRatedHighTemp;
        StandardRatingsResult["NetHeatingCapRatedLowTemp"] = NetHeatingCapRatedLowTemp;
        StandardRatingsResult["HSPF"] = HSPF;

        // HSPF2 Calculation | AHRI 2023 Std.
        std::tie(NetHeatingCapRatedHighTemp_2023, NetHeatingCapRatedLowTemp_2023, HSPF2_2023) =
            MultiSpedDXHeatingCoilHSPF2(state,
                                        nsp,
                                        MSFanPowerPerEvapAirFlowRateInput_2023,
                                        CapFTempCurveIndex,
                                        CapFFlowCurveIndex,
                                        RatedTotalCapacity,
                                        RatedAirVolFlowRate,
                                        EIRFFlowCurveIndex,
                                        EIRFTempCurveIndex,
                                        RatedCOP,
                                        RegionNum,
                                        MinOATCompressor,
                                        OATempCompressorOnOffBlank,
                                        OATempCompressorOn,
                                        DefrostControl);

        StandardRatingsResult["NetHeatingCapRatedHighTemp_2023"] = NetHeatingCapRatedHighTemp_2023;
        StandardRatingsResult["NetHeatingCapRatedLowTemp_2023"] = NetHeatingCapRatedLowTemp_2023;
        StandardRatingsResult["HSPF2_2023"] = HSPF2_2023;

        return StandardRatingsResult;
    }

    void ReportDXCoilRating(EnergyPlusData &state,
                            std::string const &CompType,    // Type of component
                            std::string_view CompName,      // Name of component
                            int const CompTypeNum,          // TypeNum of component
                            Real64 const CoolCapVal,        // Standard total (net) cooling capacity for AHRI Std. 210/240 {W}
                            Real64 const SEERUserIP,        // SEER value in IP units from user PLR curve {Btu/W-h}
                            Real64 const SEERStandardIP,    // SEER value in IP units from AHRI Std 210/240-2008 default PLF curve and C_D {Btu/W-h}
                            Real64 const EERValueSI,        // EER value in SI units {W/W}
                            Real64 const EERValueIP,        // EER value in IP units {Btu/W-h}
                            Real64 const IEERValueIP,       // IEER value in IP units {Btu/W-h}
                            Real64 const HighHeatingCapVal, // High Temperature Heating Standard (Net) Rating Capacity
                            Real64 const LowHeatingCapVal,  // Low Temperature Heating Standard (Net) Rating Capacity
                            Real64 const HSPFValueIP,       // IEER value in IP units {Btu/W-h}
                            int const RegionNum,            // Region Number for which HSPF is calculated
                            Optional_bool_const AHRI2023StandardRatings) // True if required AHRI/ANSI 210/240 Std. 2023 SEER2,HSPF2 Ratings.
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Bereket Nigusse, Chandan Sharma
        //       DATE WRITTEN   February 2010
        //       MODIFIED       May 2010 (Added EER and IEER entries)
        //                      March 2012 (Added HSPF and High/Low Heating Capacity entries)
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine writes the standard rating (net) cooling capacity, SEER, EER and IEER values to
        // the "eio" and tabular output files for Single Speed compressor DX Cooling Coils.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // Using/Aliasing

        using namespace OutputReportPredefined;
        using DataHVACGlobals::CoilDX_CoolingSingleSpeed;
        using DataHVACGlobals::CoilDX_HeatingEmpirical;
        using DataHVACGlobals::CoilDX_MultiSpeedCooling;
        using DataHVACGlobals::CoilDX_MultiSpeedHeating;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        //   or ANSI/AHRI Std. 340/360 {W}

        // for AHRI Std. 210/240 {W}
        // for AHRI Std. 210/240 {W}

        // SUBROUTINE PARAMETER DEFINITIONS:

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        auto &MyCoolOneTimeFlag = state.dataHVACGlobal->StandardRatingsMyCoolOneTimeFlag;
        auto &MyHeatOneTimeFlag = state.dataHVACGlobal->StandardRatingsMyHeatOneTimeFlag;

        switch (CompTypeNum) {

        case CoilDX_CoolingSingleSpeed: {
            if (!AHRI2023StandardRatings) {
                if (MyCoolOneTimeFlag) {
                    print(state.files.eio,
                          "{}",
                          "! <DX Cooling Coil Standard Rating Information>, Component Type, Component Name, Standard Rating (Net) "
                          "Cooling Capacity {W}, Standard Rated Net COP {W/W}, EER {Btu/W-h}, SEER User {Btu/W-h}, SEER Standard {Btu/W-h}, "
                          "IEER "
                          "{Btu/W-h}\n");
                    MyCoolOneTimeFlag = false;
                }

                static constexpr std::string_view Format_991(
                    " DX Cooling Coil Standard Rating Information, {}, {}, {:.1R}, {:.2R}, {:.2R}, {:.2R}, {:.2R}, {:.2R}\n");
                print(state.files.eio, Format_991, CompType, CompName, CoolCapVal, EERValueSI, EERValueIP, SEERUserIP, SEERStandardIP, IEERValueIP);

                PreDefTableEntry(state, state.dataOutRptPredefined->pdchDXCoolCoilType, CompName, CompType);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchDXCoolCoilNetCapSI, CompName, CoolCapVal, 1);
                // W/W is the same as Btuh/Btuh so that's fine too
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchDXCoolCoilCOP, CompName, EERValueSI, 2);
                // Btu/W-h will convert to itself
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchDXCoolCoilEERIP, CompName, EERValueIP, 2);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchDXCoolCoilSEERUserIP, CompName, SEERUserIP, 2);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchDXCoolCoilSEERStandardIP, CompName, SEERStandardIP, 2);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchDXCoolCoilIEERIP, CompName, IEERValueIP, 2);
                addFootNoteSubTable(state,
                                    state.dataOutRptPredefined->pdstDXCoolCoil,
                                    "ANSI/AHRI ratings account for supply air fan heat and electric power. "
                                    "SEER User is calculated using user-input PLF curve and cooling coefficient of degradation whereas SEER Standard "
                                    "is calculated using AHRI Std 210/240-2008 default PLF curve and cooling coefficient of degradation.");
            } else {
                // ANSI/AHRI 210/240 Standard 2023 Ratings | SEER2
                if (MyCoolOneTimeFlag) {
                    print(state.files.eio,
                          "{}",
                          "! <DX Cooling Coil Standard Rating Information>, Component Type, Component Name, Standard Rating (Net) "
                          "Cooling Capacity {W}, Standard Rated Net COP2 {W/W}, EER2 {Btu/W-h}, SEER2 User {Btu/W-h}, SEER2 Standard {Btu/W-h}, "
                          "IEER2 "
                          "{Btu/W-h}\n");
                    MyCoolOneTimeFlag = false;
                }

                static constexpr std::string_view Format_991_(
                    " DX Cooling Coil Standard Rating Information, {}, {}, {:.1R}, {:.2R}, {:.2R}, {:.2R}, {:.2R}, {}\n");
                print(state.files.eio, Format_991_, CompType, CompName, CoolCapVal, EERValueSI, EERValueIP, SEERUserIP, SEERStandardIP, ' ');

                PreDefTableEntry(state, state.dataOutRptPredefined->pdchDXCoolCoilType_2023, CompName, CompType);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchDXCoolCoilNetCapSI_2023, CompName, CoolCapVal, 1);
                // W/W is the same as Btuh/Btuh so that's fine too
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchDXCoolCoilCOP_2023, CompName, EERValueSI, 2);
                // Btu/W-h will convert to itself
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchDXCoolCoilEERIP_2023, CompName, EERValueIP, 2);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchDXCoolCoilSEER2UserIP_2023, CompName, SEERUserIP, 2);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchDXCoolCoilSEER2StandardIP_2023, CompName, SEERStandardIP, 2);
                // PreDefTableEntry(state, state.dataOutRptPredefined->pdchDXCoolCoilIEERIP_2023, CompName, IEERValueIP, 2);
                addFootNoteSubTable(
                    state,
                    state.dataOutRptPredefined->pdstDXCoolCoil_2023,
                    "ANSI/AHRI ratings account for supply air fan heat and electric power. "
                    "SEER2 User is calculated using user-input PLF curve and cooling coefficient of degradation whereas SEER2 Standard "
                    "is calculated using AHRI Std 210/240-2023 default PLF curve and cooling coefficient of degradation. "
                    "IEER Calculation was removed from the 2023 Version of the Standard.");
            }
            break;
        }
        case CoilDX_HeatingEmpirical:
        case CoilDX_MultiSpeedHeating: {
            if (!AHRI2023StandardRatings) {
                if (MyHeatOneTimeFlag) {
                    static constexpr std::string_view Format_992(
                        "! <DX Heating Coil Standard Rating Information>, Component Type, Component Name, High Temperature Heating "
                        "(net) Rating Capacity {W}, Low Temperature Heating (net) Rating Capacity {W}, HSPF {Btu/W-h}, Region "
                        "Number\n");
                    print(state.files.eio, "{}", Format_992);
                    MyHeatOneTimeFlag = false;
                }

                static constexpr std::string_view Format_993(" DX Heating Coil Standard Rating Information, {}, {}, {:.1R}, {:.1R}, {:.2R}, {}\n");
                print(state.files.eio, Format_993, CompType, CompName, HighHeatingCapVal, LowHeatingCapVal, HSPFValueIP, RegionNum);

                PreDefTableEntry(state, state.dataOutRptPredefined->pdchDXHeatCoilType, CompName, CompType);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchDXHeatCoilHighCap, CompName, HighHeatingCapVal, 1);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchDXHeatCoilLowCap, CompName, LowHeatingCapVal, 1);
                // Btu/W-h will convert to itself
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchDXHeatCoilHSPFIP, CompName, HSPFValueIP, 2);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchDXHeatCoilRegionNum, CompName, RegionNum);
                addFootNoteSubTable(
                    state, state.dataOutRptPredefined->pdstDXHeatCoil, "ANSI/AHRI ratings account for supply air fan heat and electric power.");
            } else {
                // ANSI/AHRI 210/240 Standard 2023 Ratings | HSPF2
                if (MyHeatOneTimeFlag) {
                    static constexpr std::string_view Format_992_(
                        "! <DX Heating Coil Standard Rating Information>, Component Type, Component Name, High Temperature Heating "
                        "(net) Rating Capacity {W}, Low Temperature Heating (net) Rating Capacity {W}, HSPF2 {Btu/W-h}, Region "
                        "Number\n");
                    print(state.files.eio, "{}", Format_992_);
                    MyHeatOneTimeFlag = false;
                }

                static constexpr std::string_view Format_993_(" DX Heating Coil Standard Rating Information, {}, {}, {:.1R}, {:.1R}, {:.2R}, {}\n");
                print(state.files.eio, Format_993_, CompType, CompName, HighHeatingCapVal, LowHeatingCapVal, HSPFValueIP, RegionNum);

                PreDefTableEntry(state, state.dataOutRptPredefined->pdchDXHeatCoilType_2023, CompName, CompType);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchDXHeatCoilHighCap_2023, CompName, HighHeatingCapVal, 1);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchDXHeatCoilLowCap_2023, CompName, LowHeatingCapVal, 1);
                // Btu/W-h will convert to itself
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchDXHeatCoilHSPF2IP_2023, CompName, HSPFValueIP, 2);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchDXHeatCoilRegionNum_2023, CompName, RegionNum);
                addFootNoteSubTable(state,
                                    state.dataOutRptPredefined->pdstDXHeatCoil_2023,
                                    "ANSI/AHRI 2023 (HSPF2) ratings account for supply air fan heat and electric power.");
            }
            break;
        }
        case CoilDX_MultiSpeedCooling: {
            if (!AHRI2023StandardRatings) {
                if (MyCoolOneTimeFlag) {
                    static constexpr std::string_view Format_994(
                        "! <DX Cooling Coil Standard Rating Information>, Component Type, Component Name, Standard Rating (Net) "
                        "Cooling Capacity {W}, Standard Rated Net COP {W/W}, EER {Btu/W-h}, SEER User {Btu/W-h}, SEER Standard {Btu/W-h}, "
                        "IEER "
                        "{Btu/W-h}");
                    print(state.files.eio, "{}\n", Format_994);
                    MyCoolOneTimeFlag = false;
                }

                static constexpr std::string_view Format_995(
                    " DX Cooling Coil Standard Rating Information, {}, {}, {:.1R}, {}, {}, {:.2R}, {:.2R}, {}\n");
                print(state.files.eio, Format_995, CompType, CompName, CoolCapVal, ' ', ' ', SEERUserIP, SEERStandardIP, ' ');

                PreDefTableEntry(state, state.dataOutRptPredefined->pdchDXCoolCoilType, CompName, CompType);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchDXCoolCoilNetCapSI, CompName, CoolCapVal, 1);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchDXCoolCoilSEERUserIP, CompName, SEERUserIP, 2);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchDXCoolCoilSEERStandardIP, CompName, SEERStandardIP, 2);
                addFootNoteSubTable(state,
                                    state.dataOutRptPredefined->pdstDXCoolCoil,
                                    "ANSI/AHRI ratings account for supply air fan heat and electric power. "
                                    "SEER User is calculated using user-input PLF curve and cooling coefficient of degradation whereas SEER Standard "
                                    "is calculated using AHRI Std 210/240-2008 default PLF curve and cooling coefficient of degradation.");
            } else {
                // ANSI/AHRI 210/240 Standard 2023 Ratings | SEER2
                if (MyCoolOneTimeFlag) {
                    static constexpr std::string_view Format_994_(
                        "! <DX Cooling Coil Standard Rating Information>, Component Type, Component Name, Standard Rating (Net) "
                        "Cooling Capacity {W}, Standard Rated Net COP {W/W}, EER2 {Btu/W-h}, SEER2 User {Btu/W-h}, SEER2 Standard {Btu/W-h}, "
                        "IEER2 "
                        "{Btu/W-h}");
                    print(state.files.eio, "{}\n", Format_994_);
                    MyCoolOneTimeFlag = false;
                }

                static constexpr std::string_view Format_995_(
                    " DX Cooling Coil Standard Rating Information, {}, {}, {:.1R}, {}, {}, {:.2R}, {:.2R}, {}\n");
                print(state.files.eio, Format_995_, CompType, CompName, CoolCapVal, ' ', ' ', SEERUserIP, SEERStandardIP, ' ');

                PreDefTableEntry(state, state.dataOutRptPredefined->pdchDXCoolCoilType_2023, CompName, CompType);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchDXCoolCoilNetCapSI_2023, CompName, CoolCapVal, 1);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchDXCoolCoilSEER2UserIP_2023, CompName, SEERUserIP, 2);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchDXCoolCoilSEER2StandardIP_2023, CompName, SEERStandardIP, 2);
                addFootNoteSubTable(
                    state,
                    state.dataOutRptPredefined->pdstDXCoolCoil_2023,
                    "ANSI/AHRI ratings account for supply air fan heat and electric power. "
                    "SEER2 User is calculated using user-input PLF curve and cooling coefficient of degradation whereas SEER2 Standard "
                    "is calculated using AHRI Std 210/240-2023 default PLF curve and cooling coefficient of degradation.");
            }

            break;
        }
        default:
            break;
        }
    }

    void ReportDXCoolCoilDataCenterApplication(EnergyPlusData &state,
                                               std::string const &CompType,           // Type of component
                                               std::string_view CompName,             // Name of component
                                               int const CompTypeNum,                 // TypeNum of component
                                               Array1D<Real64> &NetCoolingCapRated,   // net cooling capacity of single speed DX cooling coil
                                               Array1D<Real64> &TotElectricPowerRated // total electric power including supply fan
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Bereket Nigusse
        //       DATE WRITTEN   October 2014
        //       MODIFIED       na
        //
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine writes the standard rating (net) cooling capacity and Electric Power for
        // for room unitary air conditioners single speed DX cooling coils to the "eio" and tabular
        // output files.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // ANSI/ASHRAE Standard 127-2012 - Method of Testing for Rating Computer and Data Processing
        //                                 Room Unitary Air Conditioners

        // Using/Aliasing

        using namespace OutputReportPredefined;
        using DataHVACGlobals::CoilDX_CoolingSingleSpeed;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        auto &MyCoolOneTimeFlag = state.dataHVACGlobal->StandardRatingsMyCoolOneTimeFlag2;
        int ClassNum; // class number (Class I, II, II, IV)
        int Num;      // text number counter

        // Formats

        if (CompTypeNum == CoilDX_CoolingSingleSpeed) {
            if (MyCoolOneTimeFlag) {
                static constexpr std::string_view Format_101(
                    "! <DX Cooling Coil ASHRAE 127 Standard Ratings Information>, Component Type, Component Name, Standard 127 "
                    "Classification, Rated Net Cooling Capacity Test A {W}, Rated Total Electric Power Test A {W}, Rated Net "
                    "Cooling Capacity Test B {W}, Rated Total Electric Power Test B {W}, Rated Net Cooling Capacity Test C {W}, "
                    "Rated Total Electric Power Test C {W}, Rated Net Cooling Capacity Test D {W}, Rated Total Electric "
                    "Power Test D {W} \n");
                print(state.files.eio, "{}", Format_101);
                MyCoolOneTimeFlag = false;
            }
            for (ClassNum = 1; ClassNum <= 4; ++ClassNum) {
                Num = (ClassNum - 1) * 4;
                std::string ClassName = format("Class {}", ClassNum);
                std::string CompNameNew = fmt::format("{}({})", CompName, ClassName);
                static constexpr std::string_view Format_102(
                    " DX Cooling Coil ASHRAE 127 Standard Ratings Information, {}, {}, {}, {:.1R}, {:.1R}, {:.1R}, "
                    "{:.1R}, {:.1R}, {:.1R}, {:.1R}, {:.1R}\n");
                print(state.files.eio,
                      Format_102,
                      CompType,
                      CompName,
                      ClassName,
                      NetCoolingCapRated(Num + 1),
                      TotElectricPowerRated(Num + 1),
                      NetCoolingCapRated(Num + 2),
                      TotElectricPowerRated(Num + 2),
                      NetCoolingCapRated(Num + 3),
                      TotElectricPowerRated(Num + 3),
                      NetCoolingCapRated(Num + 4),
                      TotElectricPowerRated(Num + 4));
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchDXCoolCoilType, CompNameNew, CompType);
                // Note: If you call format("{:.1R}", NetCoolingCapRated(Num + 1)),
                // Then it's not the OutputReportPredefined::PreDefTableEntry prototype with Real64 that is called.
                // As a result, the entry isn't marked as being Real (origEntryIsReal) and unit conversion does not occur
                // Bad: PreDefTableEntry(state, pdchDXCoolCoilNetCapSIA, CompNameNew, format("{:.1R}", NetCoolingCapRated(Num + 1)));
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchDXCoolCoilNetCapSIA, CompNameNew, NetCoolingCapRated(Num + 1), 1);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchDXCoolCoilNetCapSIB, CompNameNew, NetCoolingCapRated(Num + 2), 1);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchDXCoolCoilNetCapSIC, CompNameNew, NetCoolingCapRated(Num + 3), 1);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchDXCoolCoilNetCapSID, CompNameNew, NetCoolingCapRated(Num + 4), 1);

                // These will stay in W, so it doesn't matter as much, but let's be consistent
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchDXCoolCoilElecPowerA, CompNameNew, TotElectricPowerRated(Num + 1), 1);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchDXCoolCoilElecPowerB, CompNameNew, TotElectricPowerRated(Num + 2), 1);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchDXCoolCoilElecPowerC, CompNameNew, TotElectricPowerRated(Num + 3), 1);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchDXCoolCoilElecPowerD, CompNameNew, TotElectricPowerRated(Num + 4), 1);

                addFootNoteSubTable(state,
                                    state.dataOutRptPredefined->pdstDXCoolCoil2,
                                    "ANSI/ASHRAE Standard 127 includes supply fan heat effect and electric power.");
            }
        }
    }

    void CheckCurveLimitsForStandardRatings(EnergyPlusData &state,
                                            std::string const &DXCoilName, // Name of DX coil for which HSPF is calculated
                                            std::string const &DXCoilType, // Type of DX coil - heating or cooling
                                            int const DXCoilTypeNum,       // Integer type of DX coil - heating or cooling
                                            int const CapFTempCurveIndex,  // Index for the capacity as a function of temperature modifier curve
                                            int const CapFFlowCurveIndex,  // Index for the capacity as a function of flow fraction modifier curve
                                            int const EIRFTempCurveIndex,  // Index for the EIR as a function of temperature modifier curve
                                            int const EIRFFlowCurveIndex,  // Index for the EIR as a function of flow fraction modifier curve
                                            int const PLFFPLRCurveIndex    // Index for the EIR vs part-load ratio curve
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         D. Shirey/B. Nigusse, FSEC
        //       DATE WRITTEN   May 2010
        //       MODIFIED       Chandan Sharma, March 2012
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Checks the limits of the various curves used in DXCoil and returns .FALSE. if the limits do not include
        // the standard test condition(s).

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // Using/Aliasing
        using CurveManager::CurveValue;
        using CurveManager::GetCurveIndex;
        using CurveManager::GetCurveMinMaxValues;
        using CurveManager::GetCurveName;
        using DataHVACGlobals::CoilDX_CoolingSingleSpeed;
        using DataHVACGlobals::CoilDX_HeatingEmpirical;
        using DataHVACGlobals::CoilDX_MultiSpeedCooling;
        using DataHVACGlobals::CoilDX_MultiSpeedHeating;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:

        static constexpr std::string_view RoutineName("CheckCurveLimitsForStandardRatings: "); // Include trailing blank space

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        //  Minimum and Maximum independent variable limits from Total Cooling Capacity Function of Temperature Curve
        Real64 CapacityWBTempMin(0.0); // Capacity modifier Min value (wet bulb temperature), from the Curve:BiQuadratic object
        Real64 CapacityWBTempMax(0.0); // Capacity modifier Max value (wet bulb temperature), from the Curve:BiQuadratic object
        Real64 CapacityDBTempMin(0.0); // Capacity modifier Min value (dry bulb temperature), from the Curve:BiQuadratic object
        Real64 CapacityDBTempMax(0.0); // Capacity modifier Max value (dry bulb temperature), from the Curve:BiQuadratic object

        //  Minimum and Maximum independent variable limits from Energy Input Ratio (EIR) Function of Temperature Curve
        Real64 EIRWBTempMin(0.0); // EIR modifier Min value (wet bulb temperature), from the Curve:BiQuadratic object
        Real64 EIRWBTempMax(0.0); // EIR modifier Max value (wet bulb temperature), from the Curve:BiQuadratic object
        Real64 EIRDBTempMin(0.0); // EIR modifier Min value (dry bulb temperature), from the Curve:BiQuadratic object
        Real64 EIRDBTempMax(0.0); // EIR modifier Max value (dry bulb temperature), from the Curve:BiQuadratic object

        //  Minimum and Maximum independent variable limits from Part Load Fraction Correlation Curve
        Real64 PLFFPLRMin(0.0); // Maximum value for Part Load Ratio, from the corresponding curve object
        Real64 PLFFPLRMax(0.0); // Minimum value for Part Load Ratio, from the corresponding curve object

        //  Minimum and Maximum independent variable limits from Total Cooling Capacity Function of Flow Fraction Curve
        Real64 CapacityFlowRatioMin(0.0); // Minimum value for flow fraction, from the corresponding curve object
        Real64 CapacityFlowRatioMax(0.0); // Maximum value for flow fraction, from the corresponding curve object

        //  Minimum and Maximum independent variable limits from Energy Input Ratio Function of Flow Fraction Curve
        Real64 EIRFlowRatioMin(0.0); // Minimum value for flow fraction, from the corresponding curve object
        Real64 EIRFlowRatioMax(0.0); // Maximum value for flow fraction, from the corresponding curve object

        //  Minimum and Maximum independent variable limits from Total Cooling Capacity Function of Temperature Curve
        Real64 HeatingCapODBTempMin(0.0); // Capacity modifier Min value (outdoor dry bulb temperature)
        Real64 HeatingCapODBTempMax(0.0); // Capacity modifier Max value (outdoor dry bulb temperature)
        Real64 HeatingCapIDBTempMin(0.0); // Capacity modifier Min value (indoor dry bulb temperature)
        Real64 HeatingCapIDBTempMax(0.0); // Capacity modifier Max value (indoor dry bulb temperature)

        //  Minimum and Maximum independent variable limits from Energy Input Ratio (EIR) Function of Temperature Curve
        Real64 HeatingEIRODBTempMin(0.0); // EIR modifier Min value (outdoor dry bulb temperature)
        Real64 HeatingEIRODBTempMax(0.0); // EIR modifier Max value (outdoor dry bulb temperature)
        Real64 HeatingEIRIDBTempMin(0.0); // EIR modifier Min value (indoor dry bulb temperature)
        Real64 HeatingEIRIDBTempMax(0.0); // EIR modifier Max value (indoor dry bulb temperature)

        bool CapCurveOATLimitsExceeded(false);     // Logical for capacity curve OD temp. limits being exceeded (low and High)
        bool CapCurveHighOATLimitsExceeded(false); // Logical for capacity curve temperature limits being exceeded (high temp)
        bool CapCurveFlowLimitsExceeded(false);    // Logical for capacity curve flow fraction limits being exceeded
        bool EIRCurveHighOATLimitsExceeded(false); // Logical for EIR curve temperature limits being exceeded (high temp)
        bool EIRCurveFlowLimitsExceeded(false);    // Logical for EIR curve flow fraction limits being exceeded

        bool CapCurveMidOATLimitsExceeded(false); // Logical for capacity curve temperature limits being exceeded (mid temp)
        bool EIRCurveMidOATLimitsExceeded(false); // Logical for EIR curve temperature limits being exceeded (mid temp)
        bool CapCurveLowOATLimitsExceeded(false); // Logical for capacity curve temperature limits being exceeded (low temp)
        bool EIRCurveLowOATLimitsExceeded(false); // Logical for EIR curve temperature limits being exceeded (Low temp)
        bool PLFfPLRforSEERLimitsExceeded(false); // Logical for PLF function of PLR limits being exceeded

        bool CapCurveIEERLimitsExceeded(false); // Logical for capacity curve temperature limits being exceeded (IEER calcs)
        bool EIRCurveIEERLimitsExceeded(false); // Logical for EIR temperature limits being exceeded (IEER calcs)

        bool HeatingCapCurveHSPFLimitsExceeded(false); // Logical for capacity curve temperature limits being exceeded
        // (HSPF calcs)
        bool HeatingEIRCurveHSPFLimitsExceeded(false); // Logical for EIR curve temperature limits being exceeded
        // (HSPF calcs)

        switch (DXCoilTypeNum) {

        case CoilDX_CoolingSingleSpeed: {
            GetCurveMinMaxValues(state, CapFTempCurveIndex, CapacityWBTempMin, CapacityWBTempMax, CapacityDBTempMin, CapacityDBTempMax);
            GetCurveMinMaxValues(state, EIRFTempCurveIndex, EIRWBTempMin, EIRWBTempMax, EIRDBTempMin, EIRDBTempMax);
            GetCurveMinMaxValues(state, CapFFlowCurveIndex, CapacityFlowRatioMin, CapacityFlowRatioMax);
            GetCurveMinMaxValues(state, EIRFFlowCurveIndex, EIRFlowRatioMin, EIRFlowRatioMax);
            GetCurveMinMaxValues(state, PLFFPLRCurveIndex, PLFFPLRMin, PLFFPLRMax);

            // Checking the limits of capacity modifying curve for temperatures
            if (CapacityDBTempMax < OutdoorCoilInletAirDryBulbTempRated || CapacityDBTempMin > OutdoorCoilInletAirDryBulbTempRated ||
                CapacityWBTempMax < CoolingCoilInletAirWetBulbTempRated || CapacityWBTempMin > CoolingCoilInletAirWetBulbTempRated) {
                CapCurveHighOATLimitsExceeded = true;
            }
            // Checking the limits of capacity modifying curve for flow fraction
            if (CapacityFlowRatioMax < AirMassFlowRatioRated || CapacityFlowRatioMin > AirMassFlowRatioRated) {
                CapCurveFlowLimitsExceeded = true;
            }
            // Checking the limits of EIR modifying curve for temperatures
            if (EIRDBTempMax < OutdoorCoilInletAirDryBulbTempRated || EIRDBTempMin > OutdoorCoilInletAirDryBulbTempRated ||
                EIRWBTempMax < CoolingCoilInletAirWetBulbTempRated || EIRWBTempMin > CoolingCoilInletAirWetBulbTempRated) {
                EIRCurveHighOATLimitsExceeded = true;
            }
            // Checking the limits of EIR modifying curve for flow fraction
            if (EIRFlowRatioMax < AirMassFlowRatioRated || EIRFlowRatioMin > AirMassFlowRatioRated) {
                EIRCurveFlowLimitsExceeded = true;
            }
            // Checking the limits of capacity modifying curve for temperatures (SEER calculation)
            if (CapacityDBTempMax < OutdoorCoilInletAirDryBulbTempTestB2 || CapacityDBTempMin > OutdoorCoilInletAirDryBulbTempTestB2 ||
                CapacityWBTempMax < CoolingCoilInletAirWetBulbTempRated || CapacityWBTempMin > CoolingCoilInletAirWetBulbTempRated) {
                CapCurveMidOATLimitsExceeded = true;
            }
            // Checking the limits of EIR modifying curve for temperatures (SEER calculation)
            if (EIRDBTempMax < OutdoorCoilInletAirDryBulbTempTestB2 || EIRDBTempMin > OutdoorCoilInletAirDryBulbTempTestB2 ||
                EIRWBTempMax < CoolingCoilInletAirWetBulbTempRated || EIRWBTempMin > CoolingCoilInletAirWetBulbTempRated) {
                EIRCurveMidOATLimitsExceeded = true;
            }
            // Checking the limits of Part Load Fraction for PLR (SEER calculation)
            if (PLFFPLRMax < PLRforSEER || PLFFPLRMin > PLRforSEER) {
                PLFfPLRforSEERLimitsExceeded = true;
            }
            // Checking the limits of capacity modifying curve for temperatures (IEER high and low test conditions)
            if (CapacityDBTempMax < OutdoorCoilInletAirDryBulbTempRated || CapacityDBTempMin > OADBTempLowReducedCapacityTest ||
                CapacityWBTempMax < CoolingCoilInletAirWetBulbTempRated || CapacityWBTempMin > CoolingCoilInletAirWetBulbTempRated) {
                CapCurveIEERLimitsExceeded = true;
            }
            // Checking the limits of EIR modifying curve for temperatures (IEER high and low test conditions)
            if (EIRDBTempMax < OutdoorCoilInletAirDryBulbTempRated || EIRDBTempMin > OADBTempLowReducedCapacityTest ||
                EIRWBTempMax < CoolingCoilInletAirWetBulbTempRated || EIRWBTempMin > CoolingCoilInletAirWetBulbTempRated) {
                EIRCurveIEERLimitsExceeded = true;
            }

            if (CapCurveHighOATLimitsExceeded || CapCurveFlowLimitsExceeded || EIRCurveHighOATLimitsExceeded || EIRCurveFlowLimitsExceeded ||
                CapCurveMidOATLimitsExceeded || EIRCurveMidOATLimitsExceeded || PLFfPLRforSEERLimitsExceeded || CapCurveIEERLimitsExceeded ||
                EIRCurveIEERLimitsExceeded) {

                ShowWarningError(state,
                                 "The Standard Ratings is calculated for " + DXCoilType + " = " + DXCoilName +
                                     " but not at the AHRI test condition due to curve out of bound.");
                ShowContinueError(state,
                                  " Review the Standard Ratings calculations in the Engineering Reference for this coil type. Also, use "
                                  "Output:Diagnostics, DisplayExtraWarnings for further guidance.");

                if (state.dataGlobal->DisplayExtraWarnings) {
                    ShowContinueError(state, std::string{RoutineName} + "The max and/or min limits specified in the corresponding curve objects");
                    ShowContinueError(state,
                                      " do not include the AHRI test conditions required to calculate one or more of the Standard Rating values.");
                }

                // For Standard Rating Cooling Capacity:
                if (CapCurveHighOATLimitsExceeded || CapCurveFlowLimitsExceeded) {
                    if (state.dataGlobal->DisplayExtraWarnings) {
                        ShowContinueError(state,
                                          DXCoilType + '=' + DXCoilName +
                                              ":  Standard Rating Cooling Capacity calculated is not at the AHRI test condition.");
                        if (CapCurveHighOATLimitsExceeded) {
                            ShowContinueError(state,
                                              " Check limits in Total Cooling Capacity Function of Temperature Curve, Curve Type = " +
                                                  state.dataCurveManager->PerfCurve(CapFTempCurveIndex).ObjectType +
                                                  ", Curve Name = " + GetCurveName(state, CapFTempCurveIndex));
                        }
                        if (CapCurveFlowLimitsExceeded) {
                            ShowContinueError(state,
                                              " Check limits in Total Cooling Capacity Function of Flow Fraction Curve, Curve Type = " +
                                                  state.dataCurveManager->PerfCurve(CapFFlowCurveIndex).ObjectType +
                                                  ", Curve Name = " + GetCurveName(state, CapFFlowCurveIndex));
                        }
                    }
                }

                // For EER:
                if (CapCurveHighOATLimitsExceeded || CapCurveFlowLimitsExceeded || EIRCurveHighOATLimitsExceeded || EIRCurveFlowLimitsExceeded) {
                    if (state.dataGlobal->DisplayExtraWarnings) {
                        ShowContinueError(
                            state, DXCoilType + '=' + DXCoilName + ":  Energy Efficiency Ratio (EER) calculated is not at the AHRI test condition.");
                        if (CapCurveHighOATLimitsExceeded) {
                            ShowContinueError(state,
                                              " Check limits in Total Cooling Capacity Function of Temperature Curve, Curve Type = " +
                                                  state.dataCurveManager->PerfCurve(CapFTempCurveIndex).ObjectType +
                                                  ", Curve Name = " + GetCurveName(state, CapFTempCurveIndex));
                        }
                        if (CapCurveFlowLimitsExceeded) {
                            ShowContinueError(state,
                                              " Check limits in Total Cooling Capacity Function of Flow Fraction Curve, Curve Type = " +
                                                  state.dataCurveManager->PerfCurve(CapFFlowCurveIndex).ObjectType +
                                                  ", Curve Name = " + GetCurveName(state, CapFFlowCurveIndex));
                        }
                        if (EIRCurveHighOATLimitsExceeded) {
                            ShowContinueError(state,
                                              " Check limits in Energy Input Ratio Function of Temperature Curve, Curve Type = " +
                                                  state.dataCurveManager->PerfCurve(EIRFTempCurveIndex).ObjectType +
                                                  ", Curve Name = " + GetCurveName(state, EIRFTempCurveIndex));
                        }
                        if (EIRCurveFlowLimitsExceeded) {
                            ShowContinueError(state,
                                              " Check limits in Energy Input Ratio Function of Flow Fraction Curve, Curve Type = " +
                                                  state.dataCurveManager->PerfCurve(EIRFFlowCurveIndex).ObjectType +
                                                  ", Curve Name = " + GetCurveName(state, EIRFFlowCurveIndex));
                        }
                    }
                }

                // For SEER:
                if (CapCurveMidOATLimitsExceeded || EIRCurveMidOATLimitsExceeded || CapCurveFlowLimitsExceeded || EIRCurveFlowLimitsExceeded ||
                    PLFfPLRforSEERLimitsExceeded) {
                    if (state.dataGlobal->DisplayExtraWarnings) {
                        ShowContinueError(state,
                                          DXCoilType + '=' + DXCoilName +
                                              ":  Seasonal Energy Efficiency Ratio (SEER) calculated is not at the AHRI test condition.");
                        if (CapCurveMidOATLimitsExceeded) {
                            ShowContinueError(state,
                                              " Check limits in Total Cooling Capacity Function of Temperature Curve, Curve Type = " +
                                                  state.dataCurveManager->PerfCurve(CapFTempCurveIndex).ObjectType +
                                                  ", Curve Name = " + GetCurveName(state, CapFTempCurveIndex));
                        }
                        if (CapCurveFlowLimitsExceeded) {
                            ShowContinueError(state,
                                              " Check limits in Total Cooling Capacity Function of Flow Fraction Curve, Curve Type = " +
                                                  state.dataCurveManager->PerfCurve(CapFFlowCurveIndex).ObjectType +
                                                  ", Curve Name = " + GetCurveName(state, CapFFlowCurveIndex));
                        }
                        if (EIRCurveMidOATLimitsExceeded) {
                            ShowContinueError(state,
                                              " Check limits in Energy Input Ratio Function of Temperature Curve, Curve Type = " +
                                                  state.dataCurveManager->PerfCurve(EIRFTempCurveIndex).ObjectType +
                                                  ", Curve Name = " + GetCurveName(state, EIRFTempCurveIndex));
                        }
                        if (EIRCurveFlowLimitsExceeded) {
                            ShowContinueError(state,
                                              " Check limits in Energy Input Ratio Function of Flow Fraction Curve, Curve Type = " +
                                                  state.dataCurveManager->PerfCurve(EIRFFlowCurveIndex).ObjectType +
                                                  ", Curve Name = " + GetCurveName(state, EIRFFlowCurveIndex));
                        }
                        if (PLFfPLRforSEERLimitsExceeded) {
                            ShowContinueError(state,
                                              " Check limits in Part Load Fraction Correlation Curve, Curve Type = " +
                                                  state.dataCurveManager->PerfCurve(PLFFPLRCurveIndex).ObjectType +
                                                  ", Curve Name = " + GetCurveName(state, PLFFPLRCurveIndex));
                        }
                    }
                }

                // For IEER:
                if (CapCurveIEERLimitsExceeded || CapCurveFlowLimitsExceeded || EIRCurveIEERLimitsExceeded || EIRCurveFlowLimitsExceeded) {
                    if (state.dataGlobal->DisplayExtraWarnings) {
                        ShowContinueError(state,
                                          DXCoilType + '=' + DXCoilName +
                                              ":  Integrated Energy Efficiency Ratio (IEER) calculated is not at the AHRI test condition.");
                        if (CapCurveIEERLimitsExceeded) {
                            ShowContinueError(state,
                                              " Check limits in Total Cooling Capacity Function of Temperature Curve, Curve Type = " +
                                                  state.dataCurveManager->PerfCurve(CapFTempCurveIndex).ObjectType +
                                                  ", Curve Name = " + GetCurveName(state, CapFTempCurveIndex));
                        }
                        if (CapCurveFlowLimitsExceeded) {
                            ShowContinueError(state,
                                              " Check limits in Total Cooling Capacity Function of Flow Fraction Curve, Curve Type = " +
                                                  state.dataCurveManager->PerfCurve(CapFFlowCurveIndex).ObjectType +
                                                  ", Curve Name = " + GetCurveName(state, CapFFlowCurveIndex));
                        }
                        if (EIRCurveIEERLimitsExceeded) {
                            ShowContinueError(state,
                                              " Check limits in EIR Function of Temperature Curve, Curve Type = " +
                                                  state.dataCurveManager->PerfCurve(EIRFTempCurveIndex).ObjectType +
                                                  ", Curve Name = " + GetCurveName(state, EIRFTempCurveIndex));
                        }
                        if (EIRCurveFlowLimitsExceeded) {
                            ShowContinueError(state,
                                              " Check limits in Energy Input Ratio Function of Flow Fraction Curve, Curve Type = " +
                                                  state.dataCurveManager->PerfCurve(EIRFFlowCurveIndex).ObjectType +
                                                  ", Curve Name = " + GetCurveName(state, EIRFFlowCurveIndex));
                        }
                    }
                }

            } // End of curve error messages
            break;
        }
        case CoilDX_HeatingEmpirical: {
            {
                if (state.dataCurveManager->PerfCurve(CapFTempCurveIndex).NumDims == 1) {
                    GetCurveMinMaxValues(state, CapFTempCurveIndex, HeatingCapODBTempMin, HeatingCapODBTempMax);

                    // Checking the limits of capacity modifying curve for temperatures (IEER high and low test conditions)
                    if (HeatingCapODBTempMax < HeatingOutdoorCoilInletAirDBTempRated ||
                        HeatingCapODBTempMin > HeatingOutdoorCoilInletAirDBTempH3Test) {
                        HeatingCapCurveHSPFLimitsExceeded = true;
                    }
                } else {
                    GetCurveMinMaxValues(
                        state, CapFTempCurveIndex, HeatingCapIDBTempMin, HeatingCapIDBTempMax, HeatingCapODBTempMin, HeatingCapODBTempMax);

                    // Checking the limits of capacity modifying curve for temperatures (IEER high and low test conditions)
                    if (HeatingCapODBTempMax < HeatingOutdoorCoilInletAirDBTempRated ||
                        HeatingCapODBTempMin > HeatingOutdoorCoilInletAirDBTempH3Test ||
                        HeatingCapIDBTempMax < HeatingIndoorCoilInletAirDBTempRated || HeatingCapIDBTempMin > HeatingIndoorCoilInletAirDBTempRated) {
                        HeatingCapCurveHSPFLimitsExceeded = true;
                    }
                }
            }
            {
                if (state.dataCurveManager->PerfCurve(EIRFTempCurveIndex).NumDims == 1) {
                    GetCurveMinMaxValues(state, EIRFTempCurveIndex, HeatingEIRODBTempMin, HeatingEIRODBTempMax);

                    // Checking the limits of EIR modifying curve for temperatures (HSPF high and low test conditions)
                    if (HeatingEIRODBTempMax < HeatingOutdoorCoilInletAirDBTempRated ||
                        HeatingEIRODBTempMin > HeatingOutdoorCoilInletAirDBTempH3Test) {
                        HeatingEIRCurveHSPFLimitsExceeded = true;
                    }
                } else {
                    GetCurveMinMaxValues(
                        state, EIRFTempCurveIndex, HeatingEIRIDBTempMin, HeatingEIRIDBTempMax, HeatingEIRODBTempMin, HeatingEIRODBTempMax);

                    // Checking the limits of EIR modifying curve for temperatures (HSPF high and low test conditions)
                    if (HeatingEIRODBTempMax < HeatingOutdoorCoilInletAirDBTempRated ||
                        HeatingEIRODBTempMin > HeatingOutdoorCoilInletAirDBTempH3Test ||
                        HeatingEIRIDBTempMax < HeatingIndoorCoilInletAirDBTempRated || HeatingEIRIDBTempMin > HeatingIndoorCoilInletAirDBTempRated) {
                        HeatingEIRCurveHSPFLimitsExceeded = true;
                    }
                }
            }
            if (HeatingCapCurveHSPFLimitsExceeded || HeatingEIRCurveHSPFLimitsExceeded) {
                ShowWarningError(state,
                                 "The Standard Ratings is calculated for " + DXCoilType + " = " + DXCoilName +
                                     " but not at the AHRI test condition due to curve out of bound.");
                ShowContinueError(state,
                                  " Review the Standard Ratings calculations in the Engineering Reference for this coil type. Also, use "
                                  "Output:Diagnostics, DisplayExtraWarnings for further guidance.");
                if (state.dataGlobal->DisplayExtraWarnings) {
                    ShowContinueError(state, std::string{RoutineName} + "The max and/or min limits specified in the corresponding curve objects");
                    ShowContinueError(state,
                                      " do not include the AHRI test conditions required to calculate one or more of the Standard Rating values.");
                }
                if (state.dataGlobal->DisplayExtraWarnings) {
                    ShowWarningError(state,
                                     DXCoilType + '=' + DXCoilName +
                                         ":  Heating Seasonal Performance Factor calculated is not at the AHRI test condition.");
                    ShowContinueError(state, " Review the Standard Ratings calculations in the Engineering Reference for this coil type.");
                    if (HeatingCapCurveHSPFLimitsExceeded) {
                        ShowContinueError(state,
                                          " Check limits in Total Heating Capacity Function of Temperature Curve, Curve Type = " +
                                              state.dataCurveManager->PerfCurve(CapFTempCurveIndex).ObjectType +
                                              ", Curve Name = " + GetCurveName(state, CapFTempCurveIndex));
                    }
                    if (HeatingEIRCurveHSPFLimitsExceeded) {
                        ShowContinueError(state,
                                          " Check limits in EIR Function of Temperature Curve, Curve Type = " +
                                              state.dataCurveManager->PerfCurve(EIRFTempCurveIndex).ObjectType +
                                              ", Curve Name = " + GetCurveName(state, EIRFTempCurveIndex));
                    }
                }
            }

            //   MultiSpeed DX Coil Net Cooling Capacity and SEER:
            break;
        }
        case CoilDX_MultiSpeedCooling: {
            GetCurveMinMaxValues(state, CapFTempCurveIndex, CapacityWBTempMin, CapacityWBTempMax, CapacityDBTempMin, CapacityDBTempMax);
            GetCurveMinMaxValues(state, EIRFTempCurveIndex, EIRWBTempMin, EIRWBTempMax, EIRDBTempMin, EIRDBTempMax);
            GetCurveMinMaxValues(state, CapFFlowCurveIndex, CapacityFlowRatioMin, CapacityFlowRatioMax);
            GetCurveMinMaxValues(state, EIRFFlowCurveIndex, EIRFlowRatioMin, EIRFlowRatioMax);

            // Checking the limits of capacity modifying curve for temperatures
            if (CapacityDBTempMax < OutdoorCoilInletAirDryBulbTempRated || CapacityDBTempMin > OutdoorCoilInletAirDryBulbTempRated ||
                CapacityWBTempMax < CoolingCoilInletAirWetBulbTempRated || CapacityWBTempMin > CoolingCoilInletAirWetBulbTempRated) {
                CapCurveHighOATLimitsExceeded = true;
            }
            // Checking the limits of capacity modifying curve for flow fraction
            if (CapacityFlowRatioMax < AirMassFlowRatioRated || CapacityFlowRatioMin > AirMassFlowRatioRated) {
                CapCurveFlowLimitsExceeded = true;
            }
            // Checking the limits of EIR modifying curve for temperatures
            if (EIRDBTempMax < OutdoorCoilInletAirDryBulbTempRated || EIRDBTempMin > OutdoorCoilInletAirDryBulbTempRated ||
                EIRWBTempMax < CoolingCoilInletAirWetBulbTempRated || EIRWBTempMin > CoolingCoilInletAirWetBulbTempRated) {
                EIRCurveHighOATLimitsExceeded = true;
            }
            // Checking the limits of EIR modifying curve for flow fraction
            if (EIRFlowRatioMax < AirMassFlowRatioRated || EIRFlowRatioMin > AirMassFlowRatioRated) {
                EIRCurveFlowLimitsExceeded = true;
            }
            // Checking the limits of capacity modifying curve for temperatures (SEER calculation)
            if (CapacityDBTempMax < OutdoorCoilInletAirDryBulbTempTestF1 || CapacityDBTempMin > OutdoorCoilInletAirDryBulbTempTestF1 ||
                CapacityWBTempMax < CoolingCoilInletAirWetBulbTempRated || CapacityWBTempMin > CoolingCoilInletAirWetBulbTempRated) {
                CapCurveLowOATLimitsExceeded = true;
            }
            // Checking the limits of EIR modifying curve for temperatures (SEER calculation)
            if (EIRDBTempMax < OutdoorCoilInletAirDryBulbTempTestF1 || EIRDBTempMin > OutdoorCoilInletAirDryBulbTempTestF1 ||
                EIRWBTempMax < CoolingCoilInletAirWetBulbTempRated || EIRWBTempMin > CoolingCoilInletAirWetBulbTempRated) {
                EIRCurveLowOATLimitsExceeded = true;
            }

            if (CapCurveHighOATLimitsExceeded || CapCurveFlowLimitsExceeded || EIRCurveHighOATLimitsExceeded || EIRCurveFlowLimitsExceeded ||
                CapCurveLowOATLimitsExceeded || EIRCurveLowOATLimitsExceeded) {

                ShowWarningError(state,
                                 "The Standard Ratings is calculated for " + DXCoilType + " = " + DXCoilName +
                                     " but not at the AHRI test condition due to curve out of bound.");
                ShowContinueError(state,
                                  " Review the Standard Ratings calculations in the Engineering Reference for this coil type. Also, use "
                                  "Output:Diagnostics, DisplayExtraWarnings for further guidance.");

                if (state.dataGlobal->DisplayExtraWarnings) {
                    ShowContinueError(state, std::string{RoutineName} + "The max and/or min limits specified in the corresponding curve objects");
                    ShowContinueError(state,
                                      " do not include the AHRI test conditions required to calculate one or more of the Standard Rating values.");
                }

                // For Standard Rating Cooling Capacity:
                if (CapCurveHighOATLimitsExceeded || CapCurveFlowLimitsExceeded) {
                    if (state.dataGlobal->DisplayExtraWarnings) {
                        ShowContinueError(state,
                                          DXCoilType + '=' + DXCoilName +
                                              ":  The Standard Rating Cooling Capacity calculated is not at the AHRI test condition.");
                        if (CapCurveHighOATLimitsExceeded) {
                            ShowContinueError(state,
                                              " Check limits in Total Cooling Capacity Function of Temperature Curve, Curve Type = " +
                                                  state.dataCurveManager->PerfCurve(CapFTempCurveIndex).ObjectType +
                                                  ", Curve Name = " + GetCurveName(state, CapFTempCurveIndex));
                        }
                        if (CapCurveFlowLimitsExceeded) {
                            ShowContinueError(state,
                                              " Check limits in Total Cooling Capacity Function of Flow Fraction Curve, Curve Type = " +
                                                  state.dataCurveManager->PerfCurve(CapFFlowCurveIndex).ObjectType +
                                                  ", Curve Name = " + GetCurveName(state, CapFFlowCurveIndex));
                        }
                    }
                }

                // For MultiSpeed DX Coil SEER:

                if (CapCurveLowOATLimitsExceeded || EIRCurveLowOATLimitsExceeded || CapCurveFlowLimitsExceeded || EIRCurveFlowLimitsExceeded) {
                    if (state.dataGlobal->DisplayExtraWarnings) {
                        ShowContinueError(state,
                                          DXCoilType + '=' + DXCoilName +
                                              ":  The Seasonal Energy Efficiency Ratio (SEER) calculated is not at the AHRI test condition.");
                        if (CapCurveLowOATLimitsExceeded) {
                            ShowContinueError(state,
                                              " Check limits in Total Cooling Capacity Function of Temperature Curve, Curve Type = " +
                                                  state.dataCurveManager->PerfCurve(CapFTempCurveIndex).ObjectType +
                                                  ", Curve Name = " + GetCurveName(state, CapFTempCurveIndex));
                        }
                        if (CapCurveFlowLimitsExceeded) {
                            ShowContinueError(state,
                                              " Check limits in Total Cooling Capacity Function of Flow Fraction Curve, Curve Type = " +
                                                  state.dataCurveManager->PerfCurve(CapFFlowCurveIndex).ObjectType +
                                                  ", Curve Name = " + GetCurveName(state, CapFFlowCurveIndex));
                        }
                        if (EIRCurveLowOATLimitsExceeded) {
                            ShowContinueError(state,
                                              " Check limits in Energy Input Ratio Function of Temperature Curve, Curve Type = " +
                                                  state.dataCurveManager->PerfCurve(EIRFTempCurveIndex).ObjectType +
                                                  ", Curve Name = " + GetCurveName(state, EIRFTempCurveIndex));
                        }
                        if (EIRCurveFlowLimitsExceeded) {
                            ShowContinueError(state,
                                              " Check limits in Energy Input Ratio Function of Flow Fraction Curve, Curve Type = " +
                                                  state.dataCurveManager->PerfCurve(EIRFFlowCurveIndex).ObjectType +
                                                  ", Curve Name = " + GetCurveName(state, EIRFFlowCurveIndex));
                        }
                    }
                }

            } // End of curve error messages

            break;
        }
        case CoilDX_MultiSpeedHeating: {

            {
                if (state.dataCurveManager->PerfCurve(CapFTempCurveIndex).NumDims == 1) {
                    GetCurveMinMaxValues(state, CapFTempCurveIndex, HeatingCapODBTempMin, HeatingCapODBTempMax);

                    if (HeatingCapODBTempMax < HeatingOutdoorCoilInletAirDBTempRated ||
                        HeatingCapODBTempMin > HeatingOutdoorCoilInletAirDBTempH3Test) {
                        CapCurveOATLimitsExceeded = true;
                    }
                    // Checking the limits of capacity modifying curve for temperatures (HSPF high and low test conditions)
                    if (HeatingCapODBTempMax < HeatingOutdoorCoilInletAirDBTempRated ||
                        HeatingCapODBTempMin > HeatingOutdoorCoilInletAirDBTempH3Test ||
                        HeatingCapODBTempMax < HeatingOutdoorCoilInletAirDBTempH0Test) {
                        HeatingCapCurveHSPFLimitsExceeded = true;
                    }

                } else {
                    GetCurveMinMaxValues(
                        state, CapFTempCurveIndex, HeatingCapIDBTempMin, HeatingCapIDBTempMax, HeatingCapODBTempMin, HeatingCapODBTempMax);

                    // Checking the limits of capacity modifying curve for temperatures (HSPF high and low test conditions)
                    if (HeatingCapODBTempMax < HeatingOutdoorCoilInletAirDBTempRated ||
                        HeatingCapODBTempMin > HeatingOutdoorCoilInletAirDBTempH3Test ||
                        HeatingCapIDBTempMax < HeatingIndoorCoilInletAirDBTempRated || HeatingCapIDBTempMin > HeatingIndoorCoilInletAirDBTempRated ||
                        HeatingCapODBTempMax < HeatingOutdoorCoilInletAirDBTempH0Test) {
                        HeatingCapCurveHSPFLimitsExceeded = true;
                    }
                }
            }

            {
                if (state.dataCurveManager->PerfCurve(EIRFTempCurveIndex).NumDims == 1) {
                    GetCurveMinMaxValues(state, EIRFTempCurveIndex, HeatingEIRODBTempMin, HeatingEIRODBTempMax);
                    // Checking the limits of EIR modifying curve for temperatures (HSPF high and low test conditions)
                    if (HeatingEIRODBTempMax < HeatingOutdoorCoilInletAirDBTempRated ||
                        HeatingEIRODBTempMin > HeatingOutdoorCoilInletAirDBTempH3Test ||
                        HeatingCapODBTempMax < HeatingOutdoorCoilInletAirDBTempH0Test) {
                        HeatingEIRCurveHSPFLimitsExceeded = true;
                    }
                } else {
                    GetCurveMinMaxValues(
                        state, EIRFTempCurveIndex, HeatingEIRIDBTempMin, HeatingEIRIDBTempMax, HeatingEIRODBTempMin, HeatingEIRODBTempMax);

                    // Checking the limits of EIR modifying curve for temperatures (HSPF high and low test conditions)
                    if (HeatingEIRODBTempMax < HeatingOutdoorCoilInletAirDBTempRated ||
                        HeatingEIRODBTempMin > HeatingOutdoorCoilInletAirDBTempH3Test ||
                        HeatingEIRIDBTempMax < HeatingIndoorCoilInletAirDBTempRated || HeatingEIRIDBTempMin > HeatingIndoorCoilInletAirDBTempRated ||
                        HeatingCapODBTempMax < HeatingOutdoorCoilInletAirDBTempH0Test) {
                        HeatingEIRCurveHSPFLimitsExceeded = true;
                    }
                }
            }
            if (HeatingCapCurveHSPFLimitsExceeded || HeatingEIRCurveHSPFLimitsExceeded || CapCurveOATLimitsExceeded) {

                ShowWarningError(state,
                                 "The Standard Ratings is calculated for " + DXCoilType + " = " + DXCoilName +
                                     " but not at the AHRI test condition due to curve out of bound.");
                ShowContinueError(state,
                                  " Review the Standard Ratings calculations in the Engineering Reference for this coil type. Also, use "
                                  "Output:Diagnostics, DisplayExtraWarnings for further guidance.");

                if (state.dataGlobal->DisplayExtraWarnings) {
                    ShowContinueError(state, std::string{RoutineName} + "The max and/or min limits specified in the corresponding curve objects");
                    ShowContinueError(state,
                                      " do not include the AHRI test conditions required to calculate one or more of the Standard Rating values.");
                }
            }
            if (CapCurveOATLimitsExceeded) {
                if (state.dataGlobal->DisplayExtraWarnings) {
                    ShowWarningError(state,
                                     DXCoilType + '=' + DXCoilName + ":  The Net Heating Capacity Calculated is not at the AHRI test condition.");
                    ShowContinueError(state,
                                      " Check limits in Total Heating Capacity Function of Temperature Curve, Curve Type = " +
                                          state.dataCurveManager->PerfCurve(CapFTempCurveIndex).ObjectType +
                                          ", Curve Name = " + GetCurveName(state, CapFTempCurveIndex));
                }
            }
            if (HeatingCapCurveHSPFLimitsExceeded || HeatingEIRCurveHSPFLimitsExceeded) {
                if (state.dataGlobal->DisplayExtraWarnings) {
                    ShowWarningError(state,
                                     DXCoilType + '=' + DXCoilName +
                                         ":  The Heating Seasonal Performance Factor calculated is not at the AHRI test condition.");
                    if (HeatingCapCurveHSPFLimitsExceeded) {
                        ShowContinueError(state,
                                          " Check limits in Total Heating Capacity Function of Temperature Curve, Curve Type = " +
                                              state.dataCurveManager->PerfCurve(CapFTempCurveIndex).ObjectType +
                                              ", Curve Name = " + GetCurveName(state, CapFTempCurveIndex));
                    }
                    if (HeatingEIRCurveHSPFLimitsExceeded) {
                        ShowContinueError(state,
                                          " Check limits in EIR Function of Temperature Curve, Curve Type = " +
                                              state.dataCurveManager->PerfCurve(EIRFTempCurveIndex).ObjectType +
                                              ", Curve Name = " + GetCurveName(state, EIRFTempCurveIndex));
                    }
                }
            }
            break;
        }
        default:
            break;
        }
    }

} // namespace StandardRatings

} // namespace EnergyPlus
