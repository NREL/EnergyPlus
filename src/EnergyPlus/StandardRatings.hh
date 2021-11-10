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

#ifndef StandardRatings_hh_INCLUDED
#define StandardRatings_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1A.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus/EnergyPlus.hh>
#include <EnergyPlus/Plant/Enums.hh>

namespace EnergyPlus {

// Forward declarations
struct EnergyPlusData;

namespace StandardRatings {

    // AHRI Standard 210/240-2008 Performance Test Conditions for Unitary Air-to-Air Air-Conditioning and Heat Pump Equipment
    extern Array1D<Real64> const IEERWeightingFactor; // EER Weighting factors (IEER)

    Real64 constexpr ConvFromSIToIP(3.412141633); // Conversion from SI to IP [3.412 Btu/hr-W]

    Real64 constexpr HeatingOutdoorCoilInletAirDBTempRated(8.33); // Outdoor air dry-bulb temp in degrees C (47F)
    // Test H1 or rated (low and High Speed) Std. AHRI 210/240

    Real64 constexpr HeatingOutdoorCoilInletAirDBTempH2Test(1.67); // Outdoor air dry-bulb temp in degrees C (35F)
    // Test H2 (low and High Speed) Std. AHRI 210/240

    Real64 constexpr HeatingOutdoorCoilInletAirDBTempH3Test(-8.33); // Outdoor air dry-bulb temp in degrees C (17F)
    // Test H3 (low and High Speed) Std. AHRI 210/240

    // Defrost strategy (heat pump only)
    enum class DefrostStrat
    {
        Unassigned = -1,
        ReverseCycle, // uses reverse cycle defrost strategy
        Resistive     // uses electric resistance heater for defrost
    };

    // Defrost control  (heat pump only)
    enum class HPdefrostControl : int
    {
        Unassigned = -1,
        Timed,   // defrost cycle is timed
        OnDemand // defrost cycle occurs only when required
    };

    extern Array1D_int const TotalNumOfTemperatureBins; // Total number of temperature
    // bins for a region
    extern Array1D<Real64> const StandardDesignHeatingRequirement;
    // Standardized DHRs from ANSI/AHRI 210/240
    extern Array1D<Real64> const OutdoorDesignTemperature;
    // Outdoor design temperature for a region from ANSI/AHRI 210/240
    extern Array1D<Real64> const OutdoorBinTemperature;
    // Fractional bin hours for different bin temperatures for region one, from ANSI/AHRI 210/240
    extern Array1D<Real64> const RegionOneFracBinHoursAtOutdoorBinTemp;
    // Fractional bin hours for different bin temperatures for region two, from ANSI/AHRI 210/240
    extern Array1D<Real64> const RegionTwoFracBinHoursAtOutdoorBinTemp;
    // Fractional bin hours for different bin temperatures for region three, from ANSI/AHRI 210/240
    extern Array1D<Real64> const RegionThreeFracBinHoursAtOutdoorBinTemp;
    // Fractional bin hours for different bin temperatures for region four, from ANSI/AHRI 210/240
    extern Array1D<Real64> const RegionFourFracBinHoursAtOutdoorBinTemp;
    // Fractional bin hours for different bin temperatures for region five, from ANSI/AHRI 210/240
    extern Array1D<Real64> const RegionFiveFracBinHoursAtOutdoorBinTemp;
    // Fractional bin hours for different bin temperatures for region six, from ANSI/AHRI 210/240
    extern Array1D<Real64> const RegionSixFracBinHoursAtOutdoorBinTemp;

    // Representative cooling season Outdoor air temperature bin from ANSI/AHRI 210/240-2008
    extern Array1D<Real64> const OutdoorBinTemperatureSEER;
    // Fractional bin hours for different bin temperatures for cooling, from ANSI/AHRI 210/240 - 2008
    extern Array1D<Real64> const CoolFracBinHoursAtOutdoorBinTemp;

    // ANSI/ASHRAE Standard 127-2012 -Method of Testing for Rating Computer and Data Processing Room Unitary Air Conditioners
    // indoor dry bulb temperatures for tests A, B, C and D and Classes I, II, III, and IV
    extern Array1D<Real64> const IndoorDBTempClassI2IV;
    // outdoor dry bulb temperatures for tests A, B, C and D
    extern Array1D<Real64> const OutdoorDBTempAllClassA2D;

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
                         Optional<Real64 const> OpenMotorEff);

    Real64
    ReformEIRChillerCondInletTempResidual(EnergyPlusData &state,
                                          Real64 const CondenserOutletTemp, // Condenser outlet temperature (boundary condition or guess value) [C]
                                          Array1<Real64> const &Par         // par(1)  = Condenser inlet temperature at AHRI Standard
    );

    void ReportChillerIPLV(EnergyPlusData &state,
                           std::string const &ChillerName,            // Name of Chiller for which IPLV is calculated
                           DataPlant::PlantEquipmentType ChillerType, // Type of Chiller - EIR or Reformulated EIR
                           Real64 const IPLVValueSI,                  // IPLV value in SI units {W/W}
                           Real64 const IPLVValueIP                   // IPLV value in IP units {Btu/W-h}
    );

    void CheckCurveLimitsForIPLV(EnergyPlusData &state,
                                 std::string const &ChillerName,               // Name of Chiller
                                 DataPlant::PlantEquipmentType ChillerType,    // Type of Chiller - EIR or ReformulatedEIR
                                 DataPlant::CondenserType const CondenserType, // Type of Condenser - Air Cooled, Water Cooled or Evap Cooled
                                 int const CapFTempCurveIndex,                 // Index for the total cooling capacity modifier curve
                                 int const EIRFTempCurveIndex                  // Index for the energy input ratio modifier curve
    );

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
        Optional_int_const RegionNum =
            _, // Region number for calculating HSPF of single speed DX heating coil //Autodesk:OPTIONAL Used without PRESENT check
        Optional<Real64 const> MinOATCompressor =
            _, // Minimum OAT for heat pump compressor operation [C] //Autodesk:OPTIONAL Used without PRESENT check
        Optional<Real64 const> OATempCompressorOn =
            _, // The outdoor temperature when the compressor is automatically turned //Autodesk:OPTIONAL Used without PRESENT check
        Optional_bool_const OATempCompressorOnOffBlank =
            _, // Flag used to determine low temperature cut out factor //Autodesk:OPTIONAL Used without PRESENT check
        Optional<HPdefrostControl const> DefrostControl = _, // defrost control; 1=timed, 2=on-demand //Autodesk:OPTIONAL Used without PRESENT check
        Optional_bool_const ASHRAE127StdRprt = _             // true if user wishes to report ASHRAE 127 standard ratings
    );

    void SingleSpeedDXHeatingCoilStandardRatings(
        EnergyPlusData &state,
        Real64 const RatedTotalCapacity,                    // Reference capacity of DX coil [W]
        Real64 const RatedCOP,                              // Reference coefficient of performance [W/W]
        int const CapFFlowCurveIndex,                       // Index for the capacity as a function of flow fraction modifier curve
        int const CapFTempCurveIndex,                       // Index for the capacity as a function of temperature modifier curve
        int const EIRFFlowCurveIndex,                       // Index for the EIR as a function of flow fraction modifier curve
        int const EIRFTempCurveIndex,                       // Index for the EIR as a function of temperature modifier curve
        Real64 const RatedAirVolFlowRate,                   // Rated air volume flow rate [m3/s]
        Real64 const FanPowerPerEvapAirFlowRateFromInput,   // Fan power per air volume flow rate [W/(m3/s)]
        Real64 &NetHeatingCapRated,                         // Net Heating Coil capacity at Rated conditions,
        Real64 &NetHeatingCapH3Test,                        // Net Heating Coil capacity at H3 test conditions
        Real64 &HSPF,                                       // seasonale energy efficiency ratio of multi speed DX cooling coil
        Optional_int_const RegionNum = _,                   // Region number for calculating HSPF of single speed DX heating coil
        Optional<Real64 const> MinOATCompressor = _,        // Minimum OAT for heat pump compressor operation [C]
        Optional<Real64 const> OATempCompressorOn = _,      // The outdoor temperature when the compressor is automatically turned
        Optional_bool_const OATempCompressorOnOffBlank = _, // Flag used to determine low temperature cut out factor
        Optional<HPdefrostControl const> DefrostControl = _ // defrost control; 1=timed, 2=on-demand
    );

    void SingleSpeedDXCoolingCoilStandardRatings(
        EnergyPlusData &state,
        std::string const &DXCoilName,                    // Name of DX coil for which HSPF is calculated
        std::string const &DXCoilType,                    // Type of DX coil - heating or cooling
        int const CapFTempCurveIndex,                     // Index for the capacity as a function of temperature modifier curve
        int const CapFFlowCurveIndex,                     // Index for the capacity as a function of flow fraction modifier curve
        int const EIRFTempCurveIndex,                     // Index for the EIR as a function of temperature modifier curve
        int const EIRFFlowCurveIndex,                     // Index for the EIR as a function of flow fraction modifier curve
        int const PLFFPLRCurveIndex,                      // Index for the EIR vs part-load ratio curve
        Real64 const RatedTotalCapacity,                  // Rated gross total cooling capacity
        Real64 const RatedCOP,                            // Rated gross COP
        Real64 const RatedAirVolFlowRate,                 // air flow rate through the coil at rated condition
        Real64 const FanPowerPerEvapAirFlowRateFromInput, // Fan power per air volume flow rate through the evaporator coil
        Real64 &NetCoolingCapRated,                       // net cooling capacity of single speed DX cooling coil
        Real64 &SEER_User,                                // seasonal energy efficiency ratio of single speed DX cooling coil, from user PLF curve
        Real64 &SEER_Standard, // seasonal energy efficiency ratio of single speed DX cooling coil, from AHRI Std 210/240-2008 default PLF curve and
                               // C_D value
        Real64 &EER,           // energy efficiency ratio of single speed DX cooling coil
        Real64 &IEER           // Integareted energy efficiency ratio of single speed DX cooling coil
    );

    void DXCoolingCoilDataCenterStandardRatings(
        EnergyPlusData &state,
        std::string const &DXCoilName,                    // Name of DX coil for which HSPF is calculated
        std::string const &DXCoilType,                    // Type of DX coil - heating or cooling
        int const CapFTempCurveIndex,                     // Index for the capacity as a function of temperature modifier curve
        int const CapFFlowCurveIndex,                     // Index for the capacity as a function of flow fraction modifier curve
        int const EIRFTempCurveIndex,                     // Index for the EIR as a function of temperature modifier curve
        int const EIRFFlowCurveIndex,                     // Index for the EIR as a function of flow fraction modifier curve
        int const PLFFPLRCurveIndex,                      // Index for the EIR vs part-load ratio curve
        Real64 const RatedTotalCapacity,                  // Rated gross total cooling capacity
        Real64 const RatedCOP,                            // Rated gross COP
        Real64 const RatedAirVolFlowRate,                 // air flow rate through the coil at rated condition
        Real64 const FanPowerPerEvapAirFlowRateFromInput, // Fan power per air volume flow rate through the evaporator coil
        Array1D<Real64> &NetCoolingCapRated,              // net cooling capacity of single speed DX cooling coil
        Array1D<Real64> &TotElectricPowerRated            // total electric power including supply fan
    );

    void MultiSpeedDXCoolingCoilStandardRatings(
        EnergyPlusData &state,
        Array1A_int const CapFTempCurveIndex,                      // Index for the capacity as a function of temperature modifier curve
        Array1A_int const CapFFlowCurveIndex,                      // Index for the capacity as a function of flow fraction modifier curve
        Array1A_int const EIRFTempCurveIndex,                      // Index for the EIR as a function of temperature modifier curve
        Array1A_int const EIRFFlowCurveIndex,                      // Index for the EIR as a function of flow fraction modifier curve
        Array1A_int const PLFFPLRCurveIndex,                       // Index for the PLF vs part-load ratio curve
        Array1A<Real64> const RatedTotalCapacity,                  // Reference capacity of DX coil [W]
        Array1A<Real64> const RatedCOP,                            // Reference coefficient of performance [W/W]
        Array1A<Real64> const RatedAirVolFlowRate,                 // Reference air flow rate of DX coil [m3/s]
        Array1A<Real64> const FanPowerPerEvapAirFlowRateFromInput, // rated fan power per evap air flow rate [W/(m3/s)]
        int const nsp,                                             // Number of compressor speeds
        Real64 &NetCoolingCapRatedMaxSpeed,                        // net cooling capacity at maximum speed
        Real64 &SEER_User,    // seasonal energy efficiency ratio of multi speed DX cooling coil, from user PLF curve
        Real64 &SEER_Standard // seasonal energy efficiency ratio of multi speed DX cooling coil, from AHRI Std 210/240-2008 default PLF curve and
                              // default C_D value
    );

    void MultiSpeedDXHeatingCoilStandardRatings(
        EnergyPlusData &state,
        std::string const &DXCoilName,                             // Name of DX coil for which HSPF is calculated
        std::string const &DXCoilType,                             // Type of DX coil for which HSPF is calculated
        Array1A_int const CapFTempCurveIndex,                      // Index for the capacity as a function of temperature modifier curve
        Array1A_int const CapFFlowCurveIndex,                      // Index for the capacity as a function of flow fraction modifier curve
        Array1A_int const EIRFTempCurveIndex,                      // Index for the EIR as a function of temperature modifier curve
        Array1A_int const EIRFFlowCurveIndex,                      // Index for the EIR as a function of flow fraction modifier curve
        Array1A_int const PLFFPLRCurveIndex,                       // Index for the PLF vs part-load ratio curve
        Array1A<Real64> const RatedTotalCapacity,                  // Reference capacity of DX coil [W]
        Array1A<Real64> const RatedCOP,                            // Reference coefficient of performance [W/W]
        Array1A<Real64> const RatedAirVolFlowRate,                 // Reference air flow rate of DX coil [m3/s]
        Array1A<Real64> const FanPowerPerEvapAirFlowRateFromInput, // rated fan power per evap air flow rate [W/(m3/s)]
        int const nsp,                                             // Number of compressor speeds
        Real64 &NetHeatingCapRatedHighTemp,                        // net heating capacity at maximum speed and High Temp
        Real64 &NetHeatingCapRatedLowTemp,                         // net heating capacity at maximum speed and low Temp
        Real64 &HSPF,                                              // seasonale energy efficiency ratio of multi speed DX cooling coil
        Optional_int_const RegionNum = _,                          // Region number for calculating HSPF of single speed DX heating coil
        Optional<Real64 const> MinOATCompressor = _,               // Minimum OAT for heat pump compressor operation [C]
        Optional<Real64 const> OATempCompressorOn = _,             // The outdoor temperature when the compressor is automatically turned
        Optional_bool_const OATempCompressorOnOffBlank = _,        // Flag used to determine low temperature cut out factor
        Optional<HPdefrostControl const> DefrostControl = _        // defrost control; 1=timed, 2=on-demand
    );

    void ReportDXCoilRating(EnergyPlusData &state,
                            std::string const &AirLoopHVAC,     // Type of component
                            std::string_view CompName,       // Name of component
                            int const CompTypeNum,           // TypeNum of component
                            Real64 const CoolCapVal,         // Standard total (net) cooling capacity for AHRI Std. 210/240 {W}
                            Real64 const SEERValueIP,        // SEER value in IP units from user PLR curve {Btu/W-h}
                            Real64 const SEERValueDefaultIP, // SEER value in IP units from AHRI Std 210/240-2008 default PLF curve and C_D {Btu/W-h}
                            Real64 const EERValueSI,         // EER value in SI units {W/W}
                            Real64 const EERValueIP,         // EER value in IP units {Btu/W-h}
                            Real64 const IEERValueIP,        // IEER value in IP units {Btu/W-h}
                            Real64 const HighHeatingCapVal,  // High Temperature Heating Standard (Net) Rating Capacity
                            Real64 const LowHeatingCapVal,   // Low Temperature Heating Standard (Net) Rating Capacity
                            Real64 const HSPFValueIP,        // IEER value in IP units {Btu/W-h}
                            int const RegionNum              // Region Number for which HSPF is calculated
    );

    void ReportDXCoolCoilDataCenterApplication(EnergyPlusData &state,
                                               std::string const &AirLoopHVAC,           // Type of component
                                               std::string_view CompName,             // Name of component
                                               int const CompTypeNum,                 // TypeNum of component
                                               Array1D<Real64> &NetCoolingCapRated,   // net cooling capacity of single speed DX cooling coil
                                               Array1D<Real64> &TotElectricPowerRated // total electric power including supply fan
    );

    void CheckCurveLimitsForStandardRatings(EnergyPlusData &state,
                                            std::string const &DXCoilName, // Name of DX coil for which HSPF is calculated
                                            std::string const &DXCoilType, // Type of DX coil - heating or cooling
                                            int const DXCoilTypeNum,       // Integer type of DX coil - heating or cooling
                                            int const CapFTempCurveIndex,  // Index for the capacity as a function of temperature modifier curve
                                            int const CapFFlowCurveIndex,  // Index for the capacity as a function of flow fraction modifier curve
                                            int const EIRFTempCurveIndex,  // Index for the EIR as a function of temperature modifier curve
                                            int const EIRFFlowCurveIndex,  // Index for the EIR as a function of flow fraction modifier curve
                                            int const PLFFPLRCurveIndex    // Index for the EIR vs part-load ratio curve
    );

} // namespace StandardRatings

} // namespace EnergyPlus

#endif
