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

#ifndef ThermalComfort_hh_INCLUDED
#define ThermalComfort_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/EnergyPlus.hh>

namespace EnergyPlus {

// Forward declarations
struct EnergyPlusData;

namespace ThermalComfort {

    struct ThermalComfortDataType
    {
        // Members
        Real64 FangerPMV;
        Real64 FangerPPD;
        Real64 CloSurfTemp; // clothing surface temp from iteration in FANGER calcs
        Real64 PiercePMVET;
        Real64 PiercePMVSET;
        Real64 PierceDISC;
        Real64 PierceTSENS;
        Real64 PierceSET;
        Real64 KsuTSV;
        Real64 ThermalComfortMRT;
        Real64 ThermalComfortOpTemp;
        Real64 ClothingValue;
        int ThermalComfortAdaptiveASH5590;
        int ThermalComfortAdaptiveASH5580;
        int ThermalComfortAdaptiveCEN15251CatI;
        int ThermalComfortAdaptiveCEN15251CatII;
        int ThermalComfortAdaptiveCEN15251CatIII;
        Real64 TComfASH55;
        Real64 TComfCEN15251;
        Real64 ASHRAE55RunningMeanOutdoorTemp;
        Real64 CEN15251RunningMeanOutdoorTemp;

        // Default Constructor
        ThermalComfortDataType()
            : FangerPMV(0.0), FangerPPD(0.0), CloSurfTemp(0.0), PiercePMVET(0.0), PiercePMVSET(0.0), PierceDISC(0.0), PierceTSENS(0.0),
              PierceSET(0.0), KsuTSV(0.0), ThermalComfortMRT(0.0), ThermalComfortOpTemp(0.0), ClothingValue(0.0),
              ThermalComfortAdaptiveASH5590(0), ThermalComfortAdaptiveASH5580(0), ThermalComfortAdaptiveCEN15251CatI(0),
              ThermalComfortAdaptiveCEN15251CatII(0), ThermalComfortAdaptiveCEN15251CatIII(0), TComfASH55(0.0), TComfCEN15251(0.0),
              ASHRAE55RunningMeanOutdoorTemp(0.0), CEN15251RunningMeanOutdoorTemp(0.0)
        {
        }
    };

    struct ThermalComfortInASH55Type
    {
        // Members
        // for debugging
        // REAL(r64)    :: dCurAirTemp
        // REAL(r64)    :: dCurMeanRadiantTemp
        // REAL(r64)    :: dOperTemp
        // REAL(r64)    :: dHumidRatio
        Real64 timeNotSummer;      // time when not in summer comfort range based on ASHRAE 55 simplified
        Real64 timeNotWinter;      // time when not in winter comfort range based on ASHRAE 55 simplified
        Real64 timeNotEither;      // time when  not in summer or winter comfort range based on ASHRAE 55 simplified
        Real64 totalTimeNotSummer; // sum for simulation for summer
        Real64 totalTimeNotWinter; // sum for simulation for winter
        Real64 totalTimeNotEither; // sum for simulation for either
        bool ZoneIsOccupied;       // flag if zone has people
        int warningIndex;          // variable to store pointer to the recurring warning
        int warningIndex2;         // variable to store pointer to the recurring warning
        bool Enable55Warning;      // flag if the warning should be able to be shown if appropriate

        // Default Constructor
        ThermalComfortInASH55Type()
            : timeNotSummer(0.0), timeNotWinter(0.0), timeNotEither(0.0), totalTimeNotSummer(0.0), totalTimeNotWinter(0.0), totalTimeNotEither(0.0),
              ZoneIsOccupied(false), warningIndex(0), warningIndex2(0), Enable55Warning(false)
        {
        }
    };

    struct ThermalComfortSetPointType
    {
        // Members
        Real64 notMetHeating;
        Real64 notMetCooling;
        Real64 notMetHeatingOccupied;
        Real64 notMetCoolingOccupied;
        Real64 totalNotMetHeating;
        Real64 totalNotMetCooling;
        Real64 totalNotMetHeatingOccupied;
        Real64 totalNotMetCoolingOccupied;

        // Default Constructor
        ThermalComfortSetPointType()
            : notMetHeating(0.0), notMetCooling(0.0), notMetHeatingOccupied(0.0), notMetCoolingOccupied(0.0), totalNotMetHeating(0.0),
              totalNotMetCooling(0.0), totalNotMetHeatingOccupied(0.0), totalNotMetCoolingOccupied(0.0)
        {
        }
    };

    struct AngleFactorData
    {
        // Members
        Array1D<Real64> AngleFactor; // Angle factor of each surface
        std::string Name;            // Angle factor list name
        Array1D_string SurfaceName;  // Names of the Surfces
        Array1D_int SurfacePtr;      // ALLOCATABLE to the names of the Surfces
        int TotAngleFacSurfaces;     // Total number of surfaces
        std::string ZoneName;        // Name of zone the system is serving
        int ZonePtr;                 // Point to this zone in the Zone derived type

        // Default Constructor
        AngleFactorData() : TotAngleFacSurfaces(0), ZonePtr(0)
        {
        }
    };

    void ManageThermalComfort(EnergyPlusData &state,
                              bool const InitializeOnly); // when called from ZTPC and calculations aren't needed

    void InitThermalComfort(EnergyPlusData &state);

    void CalcThermalComfortFanger(EnergyPlusData &state,
                                  Optional_int_const PNum = _,     // People number for thermal comfort control
                                  Optional<Real64 const> Tset = _, // Temperature setpoint for thermal comfort control
                                  Optional<Real64> PMVResult = _   // PMV value for thermal comfort control
    );

    void CalcThermalComfortPierce(EnergyPlusData &state);

    void CalcThermalComfortKSU(EnergyPlusData &state);

    void DERIV(EnergyPlusData &state, int &TempIndiceNum,         // Number of temperature indices  unused1208
               Array1D<Real64> &Temp,      // Temperature unused1208
               Array1D<Real64> &TempChange // Change of temperature
    );

    void RKG(EnergyPlusData &state, int &NEQ, Real64 &H, Real64 &X, Array1D<Real64> &Y, Array1D<Real64> &DY, Array1D<Real64> &C);

    void GetAngleFactorList(EnergyPlusData &state);

    Real64 CalcAngleFactorMRT(EnergyPlusData &state, int const AngleFacNum);

    Real64 CalcSurfaceWeightedMRT(EnergyPlusData &state, int const ZoneNum, int const SurfNum);

    Real64 CalcSatVapPressFromTemp(Real64 const Temp);

    Real64 CalcRadTemp(EnergyPlusData &state, int const PeopleListNum); // Type of MRT calculation (zone averaged or surface weighted)

    void CalcThermalComfortSimpleASH55(EnergyPlusData &state);

    void CalcIfSetPointMet(EnergyPlusData &state);

    void CalcThermalComfortAdaptiveASH55(
        EnergyPlusData &state,
        bool const initiate,                  // true if supposed to initiate
        Optional_bool_const wthrsim = _,      // true if this is a weather simulation
        Optional<Real64 const> avgdrybulb = _ // approximate avg drybulb for design day.  will be used as previous period in design day
    );

    void CalcThermalComfortAdaptiveCEN15251(
        EnergyPlusData &state,
        bool const initiate,                  // true if supposed to initiate
        Optional_bool_const wthrsim = _,      // true if this is a weather simulation
        Optional<Real64 const> avgdrybulb = _ // approximate avg drybulb for design day.  will be used as previous period in design day
    );

    void DynamicClothingModel(EnergyPlusData &state);

} // namespace ThermalComfort

struct ThermalComfortsData : BaseGlobalStruct {

    bool FirstTimeFlag = true;                // Flag set to make sure you get input once
    bool FirstTimeSurfaceWeightedFlag = true; // Flag set to make sure certain calcs related to surface weighted option are only done once

    // MODULE PARAMETER DEFINITIONS
    Real64 const TAbsConv = DataGlobals::KelvinConv; // Converter for absolute temperature
    Real64 const ActLevelConv = 58.2;   // Converter for activity level (1Met = 58.2 W/m2)
    Real64 const BodySurfArea = 1.8;    // Dubois body surface area of the human body (m2)
    Real64 const RadSurfEff = 0.72;     // Fraction of surface effective for radiation
    Real64 const StefanBoltz = 5.67e-8; // Stefan-Boltzmann constant (W/m2K4)

    static std::string const BlankString;

    // MODULE VARIABLE DECLARATIONS:
    Real64 AbsAirTemp = 0.0;                // Absolute air temperature; K
    Real64 AbsCloSurfTemp = 0.0;            // Absolute clothing surface temperature; K
    Real64 AbsRadTemp = 0.0;                // Absolute radiant temperature; K
    Real64 AcclPattern = 0.0;               // The pattern of acclimation
    Real64 ActLevel = 0.0;                  // Metabolic rate; w/m2
    Real64 AirVel = 0.0;                    // Air velocity; m/s
    Real64 AirTemp = 0.0;                   // Air temperature; C
    Real64 CloBodyRat = 0.0;                // Ratio of clothed body
    Real64 CloInsul = 0.0;                  // Clothing insulation
    Real64 CloPermeatEff = 0.0;             // Clothing permeation efficiency
    Real64 CloSurfTemp = 0.0;               // Clothing surface temperature; K
    Real64 CloThermEff = 0.0;               // The Burton thermal efficiency factor for clothing
    Real64 CloUnit = 0.0;                   // Clothing unit; CLO
    Real64 ConvHeatLoss = 0.0;              // Convective heat loss
    Real64 CoreTempChange = 0.0;            // Temperature change of core in 1 minute
    Real64 CoreTemp = 0.0;                  // Body core temperature
    Real64 CoreTempNeut = 0.0;              // Body core temperature of neutral state
    Real64 CoreThermCap = 0.0;              // Thermal capacity of core
    Real64 DryHeatLoss = 0.0;               // Heat loss from clothing surface due to both convection and radiation
    Real64 DryRespHeatLoss = 0.0;           // Dry respiration heat loss
    Real64 EvapHeatLoss = 0.0;              // Evaporative heat loss from skin
    Real64 EvapHeatLossDiff = 0.0;          // Evaporative heat loss due to moisture diffusion through skin
    Real64 EvapHeatLossMax = 0.0;           // Maximum evaporative heat loss
    Real64 EvapHeatLossRegComf = 0.0;       // Evaporative heat loss due to regulatory sweating at the state of comfort
    Real64 EvapHeatLossRegSweat = 0.0;      // Evaporative heat loss from regulatory sweating
    Real64 EvapHeatLossSweat = 0.0;         // Evaporative heat loss from the sweat secreted
    Real64 EvapHeatLossSweatPrev = 0.0;     // Old value of evaporative heat loss from the sweat secreted (KSU)
    Real64 H = 0.0;                         // Combined heat transfer coefficient
    Real64 Hc = 0.0;                        // Convective heat transfer coeffiency
    Real64 HcFor = 0.0;                     // Convective heat transfer coeffiency - Forced
    Real64 HcNat = 0.0;                     // Convective heat transfer coeffiency - Natural
    Real64 HeatFlow = 0.0;                  // Heat flow from core to skin
    Real64 Hr = 0.0;                        // Radiant heat transfer coeffiency
    Real64 IntHeatProd = 0.0;               // Internal heat production
    int IterNum = 0;                        // Number of iteration
    Real64 LatRespHeatLoss = 0.0;           // Latent respiration heat loss
    int MaxZoneNum = 0;                     // Number of zones
    int MRTCalcType = 0;                    // The type of MRT calculation (ZoneAveraged or SurfaceWeighted)
    Real64 OpTemp = 0.0;                    // Operative temperature
    int PeopleNum = 0;                      // People number
    Real64 RadHeatLoss = 0.0;               // Radiant heat loss
    Real64 RadTemp = 0.0;                   // Radiant temperature; C
    Real64 RelHum = 0.0;                    // Relative humidity; Fraction
    Real64 RespHeatLoss = 0.0;              // The rate of respiratory heat loss
    Real64 SatSkinVapPress = 0.0;           // Saturated vapor pressure at skin temperature
    Real64 ShivResponse = 0.0;              // Metalbolic heat production due to shivering
    Real64 SkinComfTemp = 0.0;              // Skin temperature required to achieve thermal comfort; C
    Real64 SkinComfVPress = 0.0;            // Saturated water vapor pressure at required skin temperature; Torr
    Real64 SkinTemp = 0.0;                  // Skin temperature
    Real64 SkinTempChange = 0.0;            // Temperature change of skin in 1 minute
    Real64 SkinTempNeut = 0.0;              // Skin temperature at neutral state
    Real64 SkinThermCap = 0.0;              // Thermal capacity of Skin
    Real64 SkinWetDiff = 0.0;               // Skin wettedness for nonsweating portion of skin
    Real64 SkinWetSweat = 0.0;              // Skin wettedness required to evaporate regulatory sweat
    Real64 SkinWetTot = 0.0;                // Total skin wettedness
    Real64 SkinVapPress = 0.0;              // Vapor pressure at skin
    Real64 SurfaceTemp = 0.0;               // Surface temperature when MRTType is 'SurfaceWeighted'
    Real64 ThermCndct = 0.0;                // Thermal conductance of skin
    Real64 ThermSensTransCoef = 0.0;        // Theraml sensation coefficient for PMV
    Real64 Time = 0.0;                      // Time, hr
    Real64 TimeChange = 0.0;                // Change of time, hr
    Real64 VapPress = 0.0;                  // Vapor pressure; Torr  ?? BG Oct 2005 humm, this should be kPa
    Real64 VasoconstrictFac = 0.0;          // Constriction factor of blood vessel
    Real64 VasodilationFac = 0.0;           // Dilation factor of blood vessel
    Real64 WorkEff = 0.0;                   // Energy cosumption by external work; w/m2
    int ZoneNum = 0;                        // Zone number
    Real64 TemporarySixAMTemperature = 0.0; // Temperature at 6am

                                           // time that any zone is not comfortable based on simple ASHRAE 55 using summer clothes
    Real64 AnyZoneTimeNotSimpleASH55Summer = 0.0;
    // time that any zone is not comfortable based on simple ASHRAE 55 using winter clothes
    Real64 AnyZoneTimeNotSimpleASH55Winter = 0.0;
    // time that any zone is not comfortable based on simple ASHRAE 55 using summer or winter clothes
    Real64 AnyZoneTimeNotSimpleASH55Either = 0.0;

    // time that any zone has unmet met loads
    Real64 AnyZoneNotMetHeating = 0.0;
    Real64 AnyZoneNotMetCooling = 0.0;
    Real64 AnyZoneNotMetHeatingOccupied = 0.0;
    Real64 AnyZoneNotMetCoolingOccupied = 0.0;
    Real64 AnyZoneNotMetOccupied = 0.0;
    // total time from beginning of simulation AnyZoneTimeNotSimpleASH55
    Real64 TotalAnyZoneTimeNotSimpleASH55Summer = 0.0;
    Real64 TotalAnyZoneTimeNotSimpleASH55Winter = 0.0;
    Real64 TotalAnyZoneTimeNotSimpleASH55Either = 0.0;
    // total time from beginning of simulation any zone not met
    Real64 TotalAnyZoneNotMetHeating = 0.0;
    Real64 TotalAnyZoneNotMetCooling = 0.0;
    Real64 TotalAnyZoneNotMetHeatingOccupied = 0.0;
    Real64 TotalAnyZoneNotMetCoolingOccupied = 0.0;
    Real64 TotalAnyZoneNotMetOccupied = 0.0;
    Array1D<Real64> ZoneOccHrs;
    bool useEpwData = false;
    Array1D<Real64> DailyAveOutTemp;

    Array1D<ThermalComfort::ThermalComfortInASH55Type> ThermalComfortInASH55;
    Array1D<ThermalComfort::ThermalComfortSetPointType> ThermalComfortSetPoint;
    Array1D<ThermalComfort::ThermalComfortDataType> ThermalComfortData;
    Array1D<ThermalComfort::AngleFactorData> AngleFactorList; // Angle Factor List data for each Angle Factor List

    Real64 runningAverageASH = 0.0;

    void clear_state() override
    {
        FirstTimeFlag = true;
        FirstTimeSurfaceWeightedFlag = true;
        runningAverageASH = 0.0;
        AbsAirTemp = 0.0;
        AbsCloSurfTemp = 0.0;
        AbsRadTemp = 0.0;
        AcclPattern = 0.0;
        ActLevel = 0.0;
        AirVel = 0.0;
        AirTemp = 0.0;
        CloBodyRat = 0.0;
        CloInsul = 0.0;
        CloPermeatEff = 0.0;
        CloSurfTemp = 0.0;
        CloThermEff = 0.0;
        CloUnit = 0.0;
        ConvHeatLoss = 0.0;
        CoreTempChange = 0.0;
        CoreTemp = 0.0;
        CoreTempNeut = 0.0;
        CoreThermCap = 0.0;
        DryHeatLoss = 0.0;
        DryRespHeatLoss = 0.0;
        EvapHeatLoss = 0.0;
        EvapHeatLossDiff = 0.0;
        EvapHeatLossMax = 0.0;
        EvapHeatLossRegComf = 0.0;
        EvapHeatLossRegSweat = 0.0;
        EvapHeatLossSweat = 0.0;
        EvapHeatLossSweatPrev = 0.0;
        H = 0.0;
        Hc = 0.0;
        HcFor = 0.0;
        HcNat = 0.0;
        HeatFlow = 0.0;
        Hr = 0.0;
        IntHeatProd = 0.0;
        IterNum = 0;
        LatRespHeatLoss = 0.0;
        MaxZoneNum = 0;
        MRTCalcType = 0;
        OpTemp = 0.0;
        PeopleNum = 0;
        RadHeatLoss = 0.0;
        RadTemp = 0.0;
        RelHum = 0.0;
        RespHeatLoss = 0.0;
        SatSkinVapPress = 0.0;
        ShivResponse = 0.0;
        SkinComfTemp = 0.0;
        SkinComfVPress = 0.0;
        SkinTemp = 0.0;
        SkinTempChange = 0.0;
        SkinTempNeut = 0.0;
        SkinThermCap = 0.0;
        SkinWetDiff = 0.0;
        SkinWetSweat = 0.0;
        SkinWetTot = 0.0;
        SkinVapPress = 0.0;
        SurfaceTemp = 0.0;
        ThermCndct = 0.0;
        ThermSensTransCoef = 0.0;
        Time = 0.0;
        TimeChange = 0.0;
        VapPress = 0.0;
        VasoconstrictFac = 0.0;
        VasodilationFac = 0.0;
        WorkEff = 0.0;
        ZoneNum = 0;
        TemporarySixAMTemperature = 0.0;
        AnyZoneTimeNotSimpleASH55Summer = 0.0;
        AnyZoneTimeNotSimpleASH55Winter = 0.0;
        AnyZoneTimeNotSimpleASH55Either = 0.0;
        AnyZoneNotMetHeating = 0.0;
        AnyZoneNotMetCooling = 0.0;
        AnyZoneNotMetHeatingOccupied = 0.0;
        AnyZoneNotMetCoolingOccupied = 0.0;
        AnyZoneNotMetOccupied = 0.0;
        TotalAnyZoneTimeNotSimpleASH55Summer = 0.0;
        TotalAnyZoneTimeNotSimpleASH55Winter = 0.0;
        TotalAnyZoneTimeNotSimpleASH55Either = 0.0;
        TotalAnyZoneNotMetHeating = 0.0;
        TotalAnyZoneNotMetCooling = 0.0;
        TotalAnyZoneNotMetHeatingOccupied = 0.0;
        TotalAnyZoneNotMetCoolingOccupied = 0.0;
        TotalAnyZoneNotMetOccupied = 0.0;
        ZoneOccHrs.deallocate();
        ThermalComfortInASH55.deallocate();
        ThermalComfortSetPoint.deallocate();
        ThermalComfortData.deallocate();
        AngleFactorList.deallocate();
    }

    // Default Constructor
    ThermalComfortsData() : DailyAveOutTemp(30, 0.0) 
    {}
};
} // namespace EnergyPlus

#endif
