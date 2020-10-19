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

// C++ Headers
#include <algorithm>
#include <cassert>
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <EnergyPlus/Construction.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalSurface.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataPrecisionGlobals.hh>
#include <EnergyPlus/DataRoomAirModel.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/FileSystem.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/OutputReportTabular.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/ThermalComfort.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/ZoneTempPredictorCorrector.hh>

namespace EnergyPlus {

namespace ThermalComfort {

    // Module containing the routines dealing with the CalcThermalComfortFanger,
    // CalcThermalComfortPierce, and CalcThermalComfortKSU

    // MODULE INFORMATION:
    //       AUTHOR         Jaewook Lee
    //       DATE WRITTEN   January 2000
    //       MODIFIED       Rick Strand (for E+ implementation February 2000)

    // PURPOSE OF THIS MODULE:
    // To calculate thermal comfort indices based on the
    // three thermal comfort prediction models (Fanger, Pierce, KSU)

    // METHODOLOGY EMPLOYED:
    // For each thermal comfort model type, the subroutines will loop through
    // the people statements and perform the requested thermal comfort evaluations

    // Using/Aliasing
    using namespace DataGlobals;
    using DataEnvironment::OutBaroPress;
    using DataEnvironment::OutDryBulbTemp;
    using DataHeatBalance::AngleFactor;
    using DataHeatBalance::MRT;
    using DataHeatBalance::People;
    using DataHeatBalance::PeopleData;
    using DataHeatBalance::SurfaceWeighted;
    using DataHeatBalance::TotPeople;
    using DataHeatBalance::Zone;
    using DataHeatBalance::ZoneAveraged;
    using DataHeatBalFanSys::MAT;
    using DataHeatBalFanSys::ZoneAirHumRat;
    using DataHeatBalFanSys::ZoneAirHumRatAvg;
    using DataHeatBalFanSys::ZoneAirHumRatAvgComf;
    using DataHeatBalFanSys::ZoneComfortControlsFanger;
    using DataHeatBalFanSys::ZTAV;
    using DataHeatBalFanSys::ZTAVComf;
    using DataRoomAirModel::IsZoneCV;
    using DataRoomAirModel::IsZoneDV;
    using DataRoomAirModel::IsZoneUI;
    using DataRoomAirModel::TCMF;
    using DataRoomAirModel::Ujet;
    using DataRoomAirModel::Urec;
    using DataRoomAirModel::VComfort_Jet;
    using DataRoomAirModel::VComfort_Recirculation;
    using DataRoomAirModel::ZoneUCSDCV;
    using DataRoomAirModel::ZTJET;
    using DataRoomAirModel::ZTREC;
    using Psychrometrics::PsyRhFnTdbWPb;
    using ScheduleManager::GetCurrentScheduleValue;

    void ManageThermalComfort(EnergyPlusData &state, bool const InitializeOnly) // when called from ZTPC and calculations aren't needed
    {

        // SUBROUTINE INFORMATION:
        //     AUTHOR         Rick Strand
        //     DATE WRITTEN   February 2000

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        static bool ASH55Flag(false);
        static bool CEN15251Flag(false);

        if (state.dataThermalComforts->FirstTimeFlag) {
            InitThermalComfort(state); // Mainly sets up output stuff
            state.dataThermalComforts->FirstTimeFlag = false;
            if (TotPeople > 0) {
                if (std::any_of(People.begin(), People.end(), [](PeopleData const &e) { return e.AdaptiveASH55; })) ASH55Flag = true;
                if (std::any_of(People.begin(), People.end(), [](PeopleData const &e) { return e.AdaptiveCEN15251; })) CEN15251Flag = true;
            }
        }

        if (DayOfSim == 1) {
            if (HourOfDay < 7) {
                state.dataThermalComforts->TemporarySixAMTemperature = 1.868132;
            } else if (HourOfDay == 7) {
                if (TimeStep == 1) {
                    state.dataThermalComforts->TemporarySixAMTemperature = OutDryBulbTemp;
                }
            }
        } else {
            if (HourOfDay == 7) {
                if (TimeStep == 1) {
                    state.dataThermalComforts->TemporarySixAMTemperature = OutDryBulbTemp;
                }
            }
        }

        if (InitializeOnly) return;

        if (BeginEnvrnFlag) {
            state.dataThermalComforts->ZoneOccHrs = 0.0;
        }

        if (!DoingSizing && !WarmupFlag) {
            CalcThermalComfortFanger(state);
            CalcThermalComfortPierce(state);
            CalcThermalComfortKSU(state);
            CalcThermalComfortSimpleASH55(state);
            CalcIfSetPointMet(state);
            if (ASH55Flag) CalcThermalComfortAdaptiveASH55(state, false);
            if (CEN15251Flag) CalcThermalComfortAdaptiveCEN15251(state, false);
        }
    }

    void InitThermalComfort(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //     AUTHOR         Rick Strand
        //     DATE WRITTEN   February 2000

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Loop; // DO loop counter
        std::string CurrentGroupName;

        state.dataThermalComforts->ThermalComfortData.allocate(TotPeople);

        for (Loop = 1; Loop <= TotPeople; ++Loop) {

            CurrentGroupName = People(Loop).Name;

            // CurrentModuleObject='People'
            if (People(Loop).Fanger) {
                SetupOutputVariable(state, "Zone Thermal Comfort Fanger Model PMV",
                                    OutputProcessor::Unit::None,
                                    state.dataThermalComforts->ThermalComfortData(Loop).FangerPMV,
                                    "Zone",
                                    "State",
                                    People(Loop).Name);
                SetupOutputVariable(state, "Zone Thermal Comfort Fanger Model PPD",
                                    OutputProcessor::Unit::Perc,
                                    state.dataThermalComforts->ThermalComfortData(Loop).FangerPPD,
                                    "Zone",
                                    "State",
                                    People(Loop).Name);
                SetupOutputVariable(state, "Zone Thermal Comfort Clothing Surface Temperature",
                                    OutputProcessor::Unit::C,
                                    state.dataThermalComforts->ThermalComfortData(Loop).CloSurfTemp,
                                    "Zone",
                                    "State",
                                    People(Loop).Name);
            }

            if (People(Loop).Pierce) {
                SetupOutputVariable(state, "Zone Thermal Comfort Pierce Model Effective Temperature PMV",
                                    OutputProcessor::Unit::None,
                                    state.dataThermalComforts->ThermalComfortData(Loop).PiercePMVET,
                                    "Zone",
                                    "State",
                                    People(Loop).Name);
                SetupOutputVariable(state, "Zone Thermal Comfort Pierce Model Standard Effective Temperature PMV",
                                    OutputProcessor::Unit::None,
                                    state.dataThermalComforts->ThermalComfortData(Loop).PiercePMVSET,
                                    "Zone",
                                    "State",
                                    People(Loop).Name);
                SetupOutputVariable(state, "Zone Thermal Comfort Pierce Model Discomfort Index",
                                    OutputProcessor::Unit::None,
                                    state.dataThermalComforts->ThermalComfortData(Loop).PierceDISC,
                                    "Zone",
                                    "State",
                                    People(Loop).Name);
                SetupOutputVariable(state, "Zone Thermal Comfort Pierce Model Thermal Sensation Index",
                                    OutputProcessor::Unit::None,
                                    state.dataThermalComforts->ThermalComfortData(Loop).PierceTSENS,
                                    "Zone",
                                    "State",
                                    People(Loop).Name);
                SetupOutputVariable(state, "Zone Thermal Comfort Pierce Model Standard Effective Temperature",
                                    OutputProcessor::Unit::C,
                                    state.dataThermalComforts->ThermalComfortData(Loop).PierceSET,
                                    "Zone",
                                    "State",
                                    People(Loop).Name);
            }

            if (People(Loop).KSU) {
                SetupOutputVariable(state, "Zone Thermal Comfort KSU Model Thermal Sensation Vote",
                                    OutputProcessor::Unit::None,
                                    state.dataThermalComforts->ThermalComfortData(Loop).KsuTSV,
                                    "Zone",
                                    "State",
                                    People(Loop).Name);
            }

            if ((People(Loop).Fanger) || (People(Loop).Pierce) || (People(Loop).KSU)) {
                SetupOutputVariable(state, "Zone Thermal Comfort Mean Radiant Temperature",
                                    OutputProcessor::Unit::C,
                                    state.dataThermalComforts->ThermalComfortData(Loop).ThermalComfortMRT,
                                    "Zone",
                                    "State",
                                    People(Loop).Name);
                SetupOutputVariable(state, "Zone Thermal Comfort Operative Temperature",
                                    OutputProcessor::Unit::C,
                                    state.dataThermalComforts->ThermalComfortData(Loop).ThermalComfortOpTemp,
                                    "Zone",
                                    "State",
                                    People(Loop).Name);
                SetupOutputVariable(state, "Zone Thermal Comfort Clothing Value",
                                    OutputProcessor::Unit::clo,
                                    state.dataThermalComforts->ThermalComfortData(Loop).ClothingValue,
                                    "Zone",
                                    "State",
                                    People(Loop).Name);
            }

            if (People(Loop).AdaptiveASH55) {
                SetupOutputVariable(state, "Zone Thermal Comfort ASHRAE 55 Adaptive Model 90% Acceptability Status",
                                    OutputProcessor::Unit::None,
                                    state.dataThermalComforts->ThermalComfortData(Loop).ThermalComfortAdaptiveASH5590,
                                    "Zone",
                                    "State",
                                    People(Loop).Name);
                SetupOutputVariable(state, "Zone Thermal Comfort ASHRAE 55 Adaptive Model 80% Acceptability Status",
                                    OutputProcessor::Unit::None,
                                    state.dataThermalComforts->ThermalComfortData(Loop).ThermalComfortAdaptiveASH5580,
                                    "Zone",
                                    "State",
                                    People(Loop).Name);
                SetupOutputVariable(state, "Zone Thermal Comfort ASHRAE 55 Adaptive Model Running Average Outdoor Air Temperature",
                                    OutputProcessor::Unit::C,
                                    state.dataThermalComforts->ThermalComfortData(Loop).ASHRAE55RunningMeanOutdoorTemp,
                                    "Zone",
                                    "State",
                                    People(Loop).Name);
                SetupOutputVariable(state, "Zone Thermal Comfort ASHRAE 55 Adaptive Model Temperature",
                                    OutputProcessor::Unit::C,
                                    state.dataThermalComforts->ThermalComfortData(Loop).TComfASH55,
                                    "Zone",
                                    "State",
                                    People(Loop).Name);
            }

            if (People(Loop).AdaptiveCEN15251) {
                SetupOutputVariable(state, "Zone Thermal Comfort CEN 15251 Adaptive Model Category I Status",
                                    OutputProcessor::Unit::None,
                                    state.dataThermalComforts->ThermalComfortData(Loop).ThermalComfortAdaptiveCEN15251CatI,
                                    "Zone",
                                    "State",
                                    People(Loop).Name);
                SetupOutputVariable(state, "Zone Thermal Comfort CEN 15251 Adaptive Model Category II Status",
                                    OutputProcessor::Unit::None,
                                    state.dataThermalComforts->ThermalComfortData(Loop).ThermalComfortAdaptiveCEN15251CatII,
                                    "Zone",
                                    "State",
                                    People(Loop).Name);
                SetupOutputVariable(state, "Zone Thermal Comfort CEN 15251 Adaptive Model Category III Status",
                                    OutputProcessor::Unit::None,
                                    state.dataThermalComforts->ThermalComfortData(Loop).ThermalComfortAdaptiveCEN15251CatIII,
                                    "Zone",
                                    "State",
                                    People(Loop).Name);
                SetupOutputVariable(state, "Zone Thermal Comfort CEN 15251 Adaptive Model Running Average Outdoor Air Temperature",
                                    OutputProcessor::Unit::C,
                                    state.dataThermalComforts->ThermalComfortData(Loop).CEN15251RunningMeanOutdoorTemp,
                                    "Zone",
                                    "State",
                                    People(Loop).Name);
                SetupOutputVariable(state, "Zone Thermal Comfort CEN 15251 Adaptive Model Temperature",
                                    OutputProcessor::Unit::C,
                                    state.dataThermalComforts->ThermalComfortData(Loop).TComfCEN15251,
                                    "Zone",
                                    "State",
                                    People(Loop).Name);
            }
        }
        state.dataThermalComforts->ThermalComfortInASH55.allocate(NumOfZones);

        // ASHRAE 55 Warning. If any people statement for a zone is true, set that zone to true
        for (Loop = 1; Loop <= TotPeople; ++Loop) {
            if (People(Loop).Show55Warning) {
                state.dataThermalComforts->ThermalComfortInASH55(People(Loop).ZonePtr).Enable55Warning = true;
            }
        }

        // CurrentModuleObject='Zone'
        for (Loop = 1; Loop <= NumOfZones; ++Loop) {
            SetupOutputVariable(state, "Zone Thermal Comfort ASHRAE 55 Simple Model Summer Clothes Not Comfortable Time",
                                OutputProcessor::Unit::hr,
                                state.dataThermalComforts->ThermalComfortInASH55(Loop).timeNotSummer,
                                "Zone",
                                "Sum",
                                Zone(Loop).Name);
            SetupOutputVariable(state, "Zone Thermal Comfort ASHRAE 55 Simple Model Winter Clothes Not Comfortable Time",
                                OutputProcessor::Unit::hr,
                                state.dataThermalComforts->ThermalComfortInASH55(Loop).timeNotWinter,
                                "Zone",
                                "Sum",
                                Zone(Loop).Name);
            SetupOutputVariable(state, "Zone Thermal Comfort ASHRAE 55 Simple Model Summer or Winter Clothes Not Comfortable Time",
                                OutputProcessor::Unit::hr,
                                state.dataThermalComforts->ThermalComfortInASH55(Loop).timeNotEither,
                                "Zone",
                                "Sum",
                                Zone(Loop).Name);
        }
        SetupOutputVariable(state, "Facility Thermal Comfort ASHRAE 55 Simple Model Summer Clothes Not Comfortable Time",
                            OutputProcessor::Unit::hr,
                            state.dataThermalComforts->AnyZoneTimeNotSimpleASH55Summer,
                            "Zone",
                            "Sum",
                            "Facility");
        SetupOutputVariable(state, "Facility Thermal Comfort ASHRAE 55 Simple Model Winter Clothes Not Comfortable Time",
                            OutputProcessor::Unit::hr,
                            state.dataThermalComforts->AnyZoneTimeNotSimpleASH55Winter,
                            "Zone",
                            "Sum",
                            "Facility");
        SetupOutputVariable(state, "Facility Thermal Comfort ASHRAE 55 Simple Model Summer or Winter Clothes Not Comfortable Time",
                            OutputProcessor::Unit::hr,
                            state.dataThermalComforts->AnyZoneTimeNotSimpleASH55Either,
                            "Zone",
                            "Sum",
                            "Facility");

        state.dataThermalComforts->ThermalComfortSetPoint.allocate(NumOfZones);
        for (Loop = 1; Loop <= NumOfZones; ++Loop) {
            SetupOutputVariable(state, "Zone Heating Setpoint Not Met Time",
                                OutputProcessor::Unit::hr,
                                state.dataThermalComforts->ThermalComfortSetPoint(Loop).notMetHeating,
                                "Zone",
                                "Sum",
                                Zone(Loop).Name);
            SetupOutputVariable(state, "Zone Heating Setpoint Not Met While Occupied Time",
                                OutputProcessor::Unit::hr,
                                state.dataThermalComforts->ThermalComfortSetPoint(Loop).notMetHeatingOccupied,
                                "Zone",
                                "Sum",
                                Zone(Loop).Name);
            SetupOutputVariable(state, "Zone Cooling Setpoint Not Met Time",
                                OutputProcessor::Unit::hr,
                                state.dataThermalComforts->ThermalComfortSetPoint(Loop).notMetCooling,
                                "Zone",
                                "Sum",
                                Zone(Loop).Name);
            SetupOutputVariable(state, "Zone Cooling Setpoint Not Met While Occupied Time",
                                OutputProcessor::Unit::hr,
                                state.dataThermalComforts->ThermalComfortSetPoint(Loop).notMetCoolingOccupied,
                                "Zone",
                                "Sum",
                                Zone(Loop).Name);
        }

        SetupOutputVariable(state, "Facility Heating Setpoint Not Met Time", OutputProcessor::Unit::hr, state.dataThermalComforts->AnyZoneNotMetHeating, "Zone", "Sum", "Facility");
        SetupOutputVariable(state, "Facility Cooling Setpoint Not Met Time", OutputProcessor::Unit::hr, state.dataThermalComforts->AnyZoneNotMetCooling, "Zone", "Sum", "Facility");
        SetupOutputVariable(state, "Facility Heating Setpoint Not Met While Occupied Time",
                            OutputProcessor::Unit::hr,
                            state.dataThermalComforts->AnyZoneNotMetHeatingOccupied,
                            "Zone",
                            "Sum",
                            "Facility");
        SetupOutputVariable(state, "Facility Cooling Setpoint Not Met While Occupied Time",
                            OutputProcessor::Unit::hr,
                            state.dataThermalComforts->AnyZoneNotMetCoolingOccupied,
                            "Zone",
                            "Sum",
                            "Facility");

        GetAngleFactorList(state);

        state.dataThermalComforts->ZoneOccHrs.dimension(NumOfZones, 0.0);
    }

    void CalcThermalComfortFanger(EnergyPlusData &state,
                                  Optional_int_const PNum,     // People number for thermal comfort control
                                  Optional<Real64 const> Tset, // Temperature setpoint for thermal comfort control
                                  Optional<Real64> PMVResult   // PMV value for thermal comfort control
    )
    {

        // SUBROUTINE INFORMATION:
        //     AUTHOR         Jaewook Lee
        //     DATE WRITTEN   January 2000
        //     MODIFIED       Rick Strand (for E+ implementation February 2000)
        //                    Brent Griffith modifications for CR 5641 (October 2005)
        //                    L. Gu, Added optional arguments for thermal comfort control (May 2006)
        //                    T. Hong, added Fanger PPD (April 2009)

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine calculates PMV(Predicted Mean Vote) using the Fanger thermal
        // comfort model. This subroutine is also used for thermal comfort control by determining
        // the temperature at which the PMV is equal to a PMV setpoint specified by the user.

        // METHODOLOGY EMPLOYED:
        // This subroutine is based heavily upon the work performed by Dan Maloney for
        // the BLAST program.  Many of the equations are based on the original Fanger
        // development.  See documentation for further details and references.

        // REFERENCES:
        // Maloney, Dan, M.S. Thesis, University of Illinois at Urbana-Champaign
        // BG note (10/21/2005),  This formulation is based on the the BASIC program
        // that is included in ASHRAE Standard 55 Normative Appendix D.

        // Using/Aliasing
        using Psychrometrics::PsyPsatFnTemp;

        // SUBROUTINE PARAMETER DEFINITIONS:
        int const MaxIter(150);             // Limit of iteration
        Real64 const StopIterCrit(0.00015); // Stop criteria for iteration

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 P1; // Intermediate variables to calculate clothed body ratio and clothing temperature
        Real64 P2; // Intermediate variables to calculate clothed body ratio and clothing temperature
        Real64 P3; // Intermediate variables to calculate clothed body ratio and clothing temperature
        Real64 P4; // Intermediate variables to calculate clothed body ratio and clothing temperature
        Real64 XF; // Intermediate variables to calculate clothed body ratio and clothing temperature
        Real64 XN; // Intermediate variables to calculate clothed body ratio and clothing temperature
        Real64 IntermediateClothing;
        Real64 PMV; // temporary variable to store calculated Fanger PMV value
        Real64 PPD; // temporary variable to store calculated Fanger PPD value

        for (state.dataThermalComforts->PeopleNum = 1; state.dataThermalComforts->PeopleNum <= TotPeople; ++state.dataThermalComforts->PeopleNum) {

            // Optional argument is used to access people object when thermal comfort control is used
            if (present(PNum)) {
                if (state.dataThermalComforts->PeopleNum != PNum) continue;
            }

            // If optional argument is used do not cycle regardless of thermal comfort reporting type
            if ((!People(state.dataThermalComforts->PeopleNum).Fanger) && (!present(PNum))) continue;

            state.dataThermalComforts->ZoneNum = People(state.dataThermalComforts->PeopleNum).ZonePtr;
            if (IsZoneDV(state.dataThermalComforts->ZoneNum) || IsZoneUI(state.dataThermalComforts->ZoneNum)) {
                state.dataThermalComforts->AirTemp = TCMF(state.dataThermalComforts->ZoneNum); // PH 3/7/04
                // UCSD-CV
            } else if (IsZoneCV(state.dataThermalComforts->ZoneNum)) {
                if (ZoneUCSDCV(state.dataThermalComforts->ZoneNum).VforComfort == VComfort_Jet) {
                    state.dataThermalComforts->AirTemp = ZTJET(state.dataThermalComforts->ZoneNum);
                } else if (ZoneUCSDCV(state.dataThermalComforts->ZoneNum).VforComfort == VComfort_Recirculation) {
                    state.dataThermalComforts->AirTemp = ZTJET(state.dataThermalComforts->ZoneNum);
                } else {
                    // Thermal comfort control uses Tset to determine PMV setpoint value, otherwise use zone temp
                    if (present(PNum)) {
                        state.dataThermalComforts->AirTemp = Tset;
                    } else {
                        state.dataThermalComforts->AirTemp = ZTAVComf(state.dataThermalComforts->ZoneNum);
                    }
                }
            } else {
                if (present(PNum)) {
                    state.dataThermalComforts->AirTemp = Tset;
                } else {
                    state.dataThermalComforts->AirTemp = ZTAVComf(state.dataThermalComforts->ZoneNum);
                }
            }
            state.dataThermalComforts->RadTemp = CalcRadTemp(state, state.dataThermalComforts->PeopleNum);
            // Use mean air temp for calculating RH when thermal comfort control is used
            if (present(PNum)) {
                state.dataThermalComforts->RelHum = PsyRhFnTdbWPb(MAT(state.dataThermalComforts->ZoneNum), ZoneAirHumRatAvgComf(state.dataThermalComforts->ZoneNum), OutBaroPress);
            } else {
                state.dataThermalComforts->RelHum = PsyRhFnTdbWPb(state.dataThermalComforts->AirTemp, ZoneAirHumRatAvgComf(state.dataThermalComforts->ZoneNum), OutBaroPress);
            }
            People(state.dataThermalComforts->PeopleNum).TemperatureInZone = state.dataThermalComforts->AirTemp;
            People(state.dataThermalComforts->PeopleNum).RelativeHumidityInZone = state.dataThermalComforts->RelHum * 100.0;

            // Metabolic rate of body (W/m2)
            state.dataThermalComforts->ActLevel = GetCurrentScheduleValue(People(state.dataThermalComforts->PeopleNum).ActivityLevelPtr) / state.dataThermalComforts->BodySurfArea;
            // Energy consumption by external work (W/m2)
            state.dataThermalComforts->WorkEff = GetCurrentScheduleValue(People(state.dataThermalComforts->PeopleNum).WorkEffPtr) * state.dataThermalComforts->ActLevel;
            // Clothing unit
            {
                auto const SELECT_CASE_var(People(state.dataThermalComforts->PeopleNum).ClothingType);
                if (SELECT_CASE_var == 1) {
                    state.dataThermalComforts->CloUnit = GetCurrentScheduleValue(People(state.dataThermalComforts->PeopleNum).ClothingPtr);
                } else if (SELECT_CASE_var == 2) {
                    state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).ThermalComfortOpTemp = (state.dataThermalComforts->RadTemp + state.dataThermalComforts->AirTemp) / 2.0;
                    state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).ClothingValue = state.dataThermalComforts->CloUnit;
                    DynamicClothingModel(state);
                    state.dataThermalComforts->CloUnit = state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).ClothingValue;
                } else if (SELECT_CASE_var == 3) {
                    IntermediateClothing = GetCurrentScheduleValue(People(state.dataThermalComforts->PeopleNum).ClothingMethodPtr);
                    if (IntermediateClothing == 1.0) {
                        state.dataThermalComforts->CloUnit = GetCurrentScheduleValue(People(state.dataThermalComforts->PeopleNum).ClothingPtr);
                        state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).ClothingValue = state.dataThermalComforts->CloUnit;
                    } else if (IntermediateClothing == 2.0) {
                        state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).ThermalComfortOpTemp = (state.dataThermalComforts->RadTemp + state.dataThermalComforts->AirTemp) / 2.0;
                        state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).ClothingValue = state.dataThermalComforts->CloUnit;
                        DynamicClothingModel(state);
                        state.dataThermalComforts->CloUnit = state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).ClothingValue;
                    } else {
                        state.dataThermalComforts->CloUnit = GetCurrentScheduleValue(People(state.dataThermalComforts->PeopleNum).ClothingPtr);
                        ShowWarningError("PEOPLE=\"" + People(state.dataThermalComforts->PeopleNum).Name +
                                         "\", Scheduled clothing value will be used rather than clothing calculation method.");
                    }
                } else {
                    ShowSevereError("PEOPLE=\"" + People(state.dataThermalComforts->PeopleNum).Name + "\", Incorrect Clothing Type");
                }
            }

            if (IsZoneCV(state.dataThermalComforts->ZoneNum)) {
                if (ZoneUCSDCV(state.dataThermalComforts->ZoneNum).VforComfort == VComfort_Jet) {
                    state.dataThermalComforts->AirVel = Ujet(state.dataThermalComforts->ZoneNum);
                } else if (ZoneUCSDCV(state.dataThermalComforts->ZoneNum).VforComfort == VComfort_Recirculation) {
                    state.dataThermalComforts->AirVel = Urec(state.dataThermalComforts->ZoneNum);
                } else {
                    state.dataThermalComforts->AirVel = 0.2;
                }
            } else {
                state.dataThermalComforts->AirVel = GetCurrentScheduleValue(People(state.dataThermalComforts->PeopleNum).AirVelocityPtr);
                // Ensure air velocity within the reasonable range. Otherwise reccusive warnings is provided
                if (present(PNum) && (state.dataThermalComforts->AirVel < 0.1 || state.dataThermalComforts->AirVel > 0.5)) {
                    if (People(state.dataThermalComforts->PeopleNum).AirVelErrIndex == 0) {
                        ShowWarningMessage("PEOPLE=\"" + People(state.dataThermalComforts->PeopleNum).Name +
                                           "\", Air velocity is beyond the reasonable range (0.1,0.5) for thermal comfort control.");
                        ShowContinueErrorTimeStamp("");
                    }
                    ShowRecurringWarningErrorAtEnd("PEOPLE=\"" + People(state.dataThermalComforts->PeopleNum).Name +
                                                       "\",Air velocity is still beyond the reasonable range (0.1,0.5)",
                                                   People(state.dataThermalComforts->PeopleNum).AirVelErrIndex,
                                                   state.dataThermalComforts->AirVel,
                                                   state.dataThermalComforts->AirVel,
                                                   _,
                                                   "[m/s]",
                                                   "[m/s]");
                }
            }

            // VapPress    = CalcSatVapPressFromTemp(AirTemp)  !original
            // VapPress    = RelHum*VapPress                   !original might be in torrs

            state.dataThermalComforts->VapPress = PsyPsatFnTemp(state.dataThermalComforts->AirTemp); // use psych routines inside E+ , returns Pa

            state.dataThermalComforts->VapPress *= state.dataThermalComforts->RelHum; // in units of [Pa]

            state.dataThermalComforts->IntHeatProd = state.dataThermalComforts->ActLevel - state.dataThermalComforts->WorkEff;

            // Compute the Corresponding Clothed Body Ratio
            state.dataThermalComforts->CloBodyRat = 1.05 + 0.1 * state.dataThermalComforts->CloUnit; // The ratio of the surface area of the clothed body
            // to the surface area of nude body

            if (state.dataThermalComforts->CloUnit < 0.5) state.dataThermalComforts->CloBodyRat = state.dataThermalComforts->CloBodyRat - 0.05 + 0.1 * state.dataThermalComforts->CloUnit;

            state.dataThermalComforts->AbsRadTemp = state.dataThermalComforts->RadTemp + state.dataThermalComforts->TAbsConv;
            state.dataThermalComforts->AbsAirTemp = state.dataThermalComforts->AirTemp + state.dataThermalComforts->TAbsConv;

            state.dataThermalComforts->CloInsul = state.dataThermalComforts->CloUnit * state.dataThermalComforts->CloBodyRat * 0.155; // Thermal resistance of the clothing

            P2 = state.dataThermalComforts->CloInsul * 3.96;
            P3 = state.dataThermalComforts->CloInsul * 100.0;
            P1 = state.dataThermalComforts->CloInsul * state.dataThermalComforts->AbsAirTemp;
            P4 = 308.7 - 0.028 * state.dataThermalComforts->IntHeatProd + P2 * pow_4(state.dataThermalComforts->AbsRadTemp / 100.0);

            // First guess for clothed surface tempeature
            state.dataThermalComforts->AbsCloSurfTemp = state.dataThermalComforts->AbsAirTemp + (35.5 - state.dataThermalComforts->AirTemp) / (3.5 * (state.dataThermalComforts->CloUnit + 0.1));
            XN = state.dataThermalComforts->AbsCloSurfTemp / 100.0;
            state.dataThermalComforts->HcFor = 12.1 * std::sqrt(state.dataThermalComforts->AirVel); // Heat transfer coefficient by forced convection
            state.dataThermalComforts->IterNum = 0;
            XF = XN;

            // COMPUTE SURFACE TEMPERATURE OF CLOTHING BY ITERATIONS
            while (((std::abs(XN - XF) > StopIterCrit) || (state.dataThermalComforts->IterNum == 0)) && (state.dataThermalComforts->IterNum < MaxIter)) {
                XF = (XF + XN) / 2.0;
                state.dataThermalComforts->HcNat = 2.38 * root_4(std::abs(100.0 * XF - state.dataThermalComforts->AbsAirTemp)); // Heat transfer coefficient by natural convection
                state.dataThermalComforts->Hc = max(state.dataThermalComforts->HcFor, state.dataThermalComforts->HcNat);                                   // Determination of convective heat transfer coefficient
                XN = (P4 + P1 * state.dataThermalComforts->Hc - P2 * pow_4(XF)) / (100.0 + P3 * state.dataThermalComforts->Hc);
                ++state.dataThermalComforts->IterNum;
                if (state.dataThermalComforts->IterNum > MaxIter) {
                    ShowWarningError("Max iteration exceeded in CalcThermalFanger");
                }
            }
            state.dataThermalComforts->AbsCloSurfTemp = 100.0 * XN;
            state.dataThermalComforts->CloSurfTemp = state.dataThermalComforts->AbsCloSurfTemp - state.dataThermalComforts->TAbsConv;

            // COMPUTE PREDICTED MEAN VOTE
            // Sensible heat loss
            // RadHeatLoss = RadSurfEff*CloBodyRat*SkinEmiss*StefanBoltz* &   !original
            //                            (AbsCloSurfTemp**4 - AbsRadTemp**4) ! Heat loss by radiation

            // following line is ln 480 in ASHRAE 55 append. D
            state.dataThermalComforts->RadHeatLoss = 3.96 * state.dataThermalComforts->CloBodyRat * (pow_4(state.dataThermalComforts->AbsCloSurfTemp * 0.01) - pow_4(state.dataThermalComforts->AbsRadTemp * 0.01));

            state.dataThermalComforts->ConvHeatLoss = state.dataThermalComforts->CloBodyRat * state.dataThermalComforts->Hc * (state.dataThermalComforts->CloSurfTemp - state.dataThermalComforts->AirTemp); // Heat loss by convection

            state.dataThermalComforts->DryHeatLoss = state.dataThermalComforts->RadHeatLoss + state.dataThermalComforts->ConvHeatLoss;

            // Evaporative heat loss
            // Heat loss by regulatory sweating
            state.dataThermalComforts->EvapHeatLossRegComf = 0.0;
            if (state.dataThermalComforts->IntHeatProd > 58.2) {
                state.dataThermalComforts->EvapHeatLossRegComf = 0.42 * (state.dataThermalComforts->IntHeatProd - state.dataThermalComforts->ActLevelConv);
            }
            // SkinTempComf = 35.7 - 0.028*IntHeatProd ! Skin temperature required to achieve thermal comfort
            // SatSkinVapPress = 1.92*SkinTempComf - 25.3 ! Water vapor pressure at required skin temperature
            // Heat loss by diffusion
            // EvapHeatLossDiff = 0.4148*(SatSkinVapPress - VapPress) !original
            state.dataThermalComforts->EvapHeatLossDiff = 3.05 * 0.001 * (5733.0 - 6.99 * state.dataThermalComforts->IntHeatProd - state.dataThermalComforts->VapPress); // ln 440 in ASHRAE 55 Append. D

            state.dataThermalComforts->EvapHeatLoss = state.dataThermalComforts->EvapHeatLossRegComf + state.dataThermalComforts->EvapHeatLossDiff;
            // Heat loss by respiration
            // original: LatRespHeatLoss = 0.0023*ActLevel*(44. - VapPress) ! Heat loss by latent respiration
            state.dataThermalComforts->LatRespHeatLoss = 1.7 * 0.00001 * state.dataThermalComforts->ActLevel * (5867.0 - state.dataThermalComforts->VapPress); // ln 460 in ASHRAE 55 Append. D

            // LatRespHeatLoss = 0.017251*ActLevel*(5.8662 - VapPress)
            // V-1.2.2 'fix' BG 3/2005 5th term in LHS Eq (58)  in 2001 HOF Ch. 8
            // this was wrong because VapPress needed to be kPa

            state.dataThermalComforts->DryRespHeatLoss = 0.0014 * state.dataThermalComforts->ActLevel * (34.0 - state.dataThermalComforts->AirTemp); // Heat loss by dry respiration.

            state.dataThermalComforts->RespHeatLoss = state.dataThermalComforts->LatRespHeatLoss + state.dataThermalComforts->DryRespHeatLoss;

            state.dataThermalComforts->ThermSensTransCoef = 0.303 * std::exp(-0.036 * state.dataThermalComforts->ActLevel) + 0.028; // Thermal transfer coefficient to calculate PMV

            PMV = state.dataThermalComforts->ThermSensTransCoef * (state.dataThermalComforts->IntHeatProd - state.dataThermalComforts->EvapHeatLoss - state.dataThermalComforts->RespHeatLoss - state.dataThermalComforts->DryHeatLoss);

            state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).FangerPMV = PMV;

            // Pass resulting PMV based on temperature setpoint (Tset) when using thermal comfort control
            if (present(PNum)) {
                PMVResult = PMV;
            }
            state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).ThermalComfortMRT = state.dataThermalComforts->RadTemp;
            state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).ThermalComfortOpTemp = (state.dataThermalComforts->RadTemp + state.dataThermalComforts->AirTemp) / 2.0;
            state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).CloSurfTemp = state.dataThermalComforts->CloSurfTemp;

            // Calculate the Fanger PPD (Predicted Percentage of Dissatisfied), as a %

            Real64 expTest1 = -0.03353 * pow_4(PMV) - 0.2179 * pow_2(PMV);
            if (expTest1 > DataPrecisionGlobals::EXP_LowerLimit) {
                PPD = 100.0 - 95.0 * std::exp(expTest1);
            } else {
                PPD = 100.0;
            }

            if (PPD < 0.0) {
                PPD = 0.0;
            } else if (PPD > 100.0) {
                PPD = 100.0;
            }

            state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).FangerPPD = PPD;
        }
    }

    void CalcThermalComfortPierce(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //     AUTHOR         Jaewook Lee
        //     DATE WRITTEN   January 2000
        //     MODIFIED       Rick Strand (for E+ implementation February 2000)

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine calculates PMVET, PMVSET, DISC, and TSENS using the Pierce
        // 2 Node model.

        // METHODOLOGY EMPLOYED:
        // This subroutine is based heavily upon the work performed by Dan Maloney for
        // the BLAST program.  Many of the equations are based on the original Pierce
        // development.  See documentation for further details and references.

        // REFERENCES:
        // Maloney, Dan, M.S. Thesis, University of Illinois at Urbana-Champaign

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const CloFac(0.25);              // Clothing factor determined experimentally
        Real64 const EvapEff(0.9);              // Evaporative efficiency
        Real64 const MaxSkinBloodFlow(90.0);    // Max. value of skin blood flow
        Real64 const RegSweatMax(670.0);        // Max. value of regulatory sweating; w/m2
        Real64 const SkinBloodFlowConst(200.0); // Skin blood flow coefficient for average person; l/m2.hr.k
        Real64 const StdAtm(1.0);               // Standard Atmospheres
        Real64 const Str(0.1);                  // Constriction constant of skin blood flow for average person
        Real64 const SweatContConst(170.0);     // Proportionality constant for sweat control; g/m2.hr
        Real64 const VapPressConv(0.1333227);   // Vapor pressure converter from torr to Kpa

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 AirEvapHeatResist; // Evaporative heat resistance of air
        Real64 ActMet;            // Metalbolic rate in MET
        Real64 ActLevelStart;     // Activity level at the start of the minute-by-minute iterations
        Real64 AvgBodyTemp;       // Average body temperature
        Real64 AvgBodyTempHigh;   // Average body temperature when HSI(Belding's classic heat sterss index) is 100
        Real64 AvgBodyTempLow;    // Average body temperature when DISC is 0
        Real64 AvgBodyTempSet;    // Setpoint for average body temperature
        Real64 BodyThermSigCold;  // Temperature difference of Body when BodyTempSet is higher than BodyTemp
        Real64 BodyTempChange;    // Temperature change of body in 1 minute
        Real64 BodyThermSigWarm;  // Temperature difference of Body when BodyTemp is higher than BodyTempSet
        Real64 CloCond;           // The conductance of the clothing
        Real64 CloEvapHeatResist; // Evaporative heat resistance of clothing
        Real64 CloSurfTempOld;    // Old value of clothing surface temperature
        Real64 CoreThermSigCold;  // Temperature difference of core when CoreTempSet is higher than CoreTemp
        Real64 CoreHeatStorage;   // Heat storage in core compartment
        Real64 CoreTempSet;       // Setpoint for body core temperature
        Real64 CoreThermSigWarm;  // Temperature difference of core when CoreTemp is higher than CoreTempSet
        Real64 DryHeatLossET;     // Heat loss from clothing surface due to both convection and radiation at ET
        Real64 DryHeatLossSET;    // Heat loss from clothing surface due to both convection and radiation at SET
        Real64 EffectCloThermEff; // Effective clothing thermal efficiency
        Real64 EffectCloUnit;     // Effective clothing unit; clo
        Real64 EnergyBalErrET;    // Stop criterion for iteration to solve energy balance
        Real64 EnergyBalErrSET;   // Stop criterion for iteration to solve energy balance
        Real64 ET;                // Effective temperature
        Real64 EvapHeatLossStart; // Starting value of evaporative heat loss
        bool FirstMinIter;
        Real64 HcAct;                // Convective heat transfer coefficient at high activity
        Real64 HcStd;                // Standard convective heat transfer coefficient
        Real64 HrStd;                // Standard radiant heat transfer coefficient
        Real64 HStd;                 // Standard combined heat transfer coefficient
        int IterMin;                 // Time period for the ieterative calculation
        Real64 LewisRat;             // Lewis ratio
        Real64 RegSweat;             // The rate of regulatory sweating
        Real64 SET;                  // Standard effective temperature
        Real64 SkinBloodFlow;        // The skin blood flow
        Real64 SkinThermSigCold;     // Temperature difference of skin when SkinTempSet is higher than SkinTemp
        Real64 SkinHeatLoss;         // Heat loss from skin
        Real64 SkinHeatStorage;      // Heat storage in skin compartment
        Real64 SkinMassRat;          // Actual skin mass to total body mass ratio
        Real64 SkinMassRatSet;       // Setpoint for skin mass to total body mass ratio
        Real64 SkinRelHum;           // Relative humidity at skin
        Real64 SkinTempSet;          // Setpoint for skin temperature
        Real64 SkinThermSigWarm;     // Temperature difference of skin when SkinTemp is higher than SkinTempSet
        Real64 StdCloBodyRat;        // Standard ratio of clothed body
        Real64 StdCloFac;            // Clothing factor determined experimentally at standard environment
        Real64 StdCloPermeatEff;     // Standard clothing permeation efficiency
        Real64 StdCloUnit;           // standard clothing unit
        Real64 StdEffectCloThermEff; // Standard effective clothing theraml efficiency
        Real64 StdEffectCloUnit;     // standard effective clothing unit
        Real64 StdVapPressET;        // Standard vapor pressure at effective temperature
        Real64 StdVapPressSET;       // Standard vapor pressure at standar effective temperature
        Real64 TotEvapHeatResist;    // Total evaporative heat resistance
        Real64 UnevapSweat;          // Unevaporated sweat; g/m2/hr
        Real64 IntermediateClothing;

        for (state.dataThermalComforts->PeopleNum = 1; state.dataThermalComforts->PeopleNum <= TotPeople; ++state.dataThermalComforts->PeopleNum) {

            if (!People(state.dataThermalComforts->PeopleNum).Pierce) continue;

            state.dataThermalComforts->ZoneNum = People(state.dataThermalComforts->PeopleNum).ZonePtr;
            if (IsZoneDV(state.dataThermalComforts->ZoneNum) || IsZoneUI(state.dataThermalComforts->ZoneNum)) {
                state.dataThermalComforts->AirTemp = TCMF(state.dataThermalComforts->ZoneNum); // PH 3/7/04
            } else {
                state.dataThermalComforts->AirTemp = ZTAVComf(state.dataThermalComforts->ZoneNum);
            }
            state.dataThermalComforts->RadTemp = CalcRadTemp(state, state.dataThermalComforts->PeopleNum);
            state.dataThermalComforts->RelHum = PsyRhFnTdbWPb(state.dataThermalComforts->AirTemp, ZoneAirHumRatAvgComf(state.dataThermalComforts->ZoneNum), OutBaroPress);
            // Metabolic rate of body (W/m2)
            state.dataThermalComforts->ActLevel = GetCurrentScheduleValue(People(state.dataThermalComforts->PeopleNum).ActivityLevelPtr) / state.dataThermalComforts->BodySurfArea;
            // Energy consumption by external work (W/m2)
            state.dataThermalComforts->WorkEff = GetCurrentScheduleValue(People(state.dataThermalComforts->PeopleNum).WorkEffPtr) * state.dataThermalComforts->ActLevel;
            // Clothing unit
            {
                auto const SELECT_CASE_var(People(state.dataThermalComforts->PeopleNum).ClothingType);
                if (SELECT_CASE_var == 1) {
                    state.dataThermalComforts->CloUnit = GetCurrentScheduleValue(People(state.dataThermalComforts->PeopleNum).ClothingPtr);
                } else if (SELECT_CASE_var == 2) {
                    state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).ThermalComfortOpTemp = (state.dataThermalComforts->RadTemp + state.dataThermalComforts->AirTemp) / 2.0;
                    state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).ClothingValue = state.dataThermalComforts->CloUnit;
                    DynamicClothingModel(state);
                    state.dataThermalComforts->CloUnit = state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).ClothingValue;
                } else if (SELECT_CASE_var == 3) {
                    IntermediateClothing = GetCurrentScheduleValue(People(state.dataThermalComforts->PeopleNum).ClothingMethodPtr);
                    if (IntermediateClothing == 1.0) {
                        state.dataThermalComforts->CloUnit = GetCurrentScheduleValue(People(state.dataThermalComforts->PeopleNum).ClothingPtr);
                        state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).ClothingValue = state.dataThermalComforts->CloUnit;
                    } else if (IntermediateClothing == 2.0) {
                        state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).ThermalComfortOpTemp = (state.dataThermalComforts->RadTemp + state.dataThermalComforts->AirTemp) / 2.0;
                        state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).ClothingValue = state.dataThermalComforts->CloUnit;
                        DynamicClothingModel(state);
                        state.dataThermalComforts->CloUnit = state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).ClothingValue;
                    } else {
                        state.dataThermalComforts->CloUnit = GetCurrentScheduleValue(People(state.dataThermalComforts->PeopleNum).ClothingPtr);
                        ShowWarningError("Scheduled clothing value will be used rather than clothing calculation method.");
                    }
                } else {
                    ShowSevereError("Incorrect Clothing Type");
                }
            }

            state.dataThermalComforts->AirVel = GetCurrentScheduleValue(People(state.dataThermalComforts->PeopleNum).AirVelocityPtr);

            state.dataThermalComforts->VapPress = CalcSatVapPressFromTemp(state.dataThermalComforts->AirTemp);
            state.dataThermalComforts->VapPress *= state.dataThermalComforts->RelHum;
            state.dataThermalComforts->VapPress *= VapPressConv; // Torr to KPa (5.8662 kPa=44 mmHg; .017251=.0023*760 mmHg/101.325 kPa)
            state.dataThermalComforts->IntHeatProd = state.dataThermalComforts->ActLevel - state.dataThermalComforts->WorkEff;
            ActMet = state.dataThermalComforts->ActLevel / state.dataThermalComforts->ActLevelConv;
            // CALCULATE VARIABLESS THAT REMAIN CONSTANT FOR AN HOUR
            state.dataThermalComforts->CloBodyRat = 1.0 + CloFac * state.dataThermalComforts->CloUnit;

            if (state.dataThermalComforts->CloUnit < 0.01) state.dataThermalComforts->CloUnit = 0.01;

            CloCond = 1.0 / (state.dataThermalComforts->CloUnit * 0.155);

            // INITIALIZE THE FOLLOWING VARIABLES
            if (state.dataThermalComforts->AirVel < 0.137) state.dataThermalComforts->AirVel = 0.137;

            state.dataThermalComforts->Hc = 8.6 * std::pow(state.dataThermalComforts->AirVel, 0.53);
            if (ActMet > 0.9) {
                HcAct = 5.66 * std::pow(ActMet - 0.85, 0.39);
                state.dataThermalComforts->Hc = max(HcAct, state.dataThermalComforts->Hc);
            }

            // Definition of vascular control signals
            // CoreTempSet, SkinTempSet, and AvgBodyTempSet are the setpoints for core, skin and
            // average body temperatures corresponding to physiol.  neutrality
            // SkinMassRatSet is the ratio of skin mass to total body mass (skin+core)
            // Typical values for CoreTempSet, SkinTempSet and SkinMassRatSet are 36.8, 33.7 and 0.10
            // SkinMassRat is the actual skin to total body mass ratio
            SkinTempSet = 33.7;
            CoreTempSet = 36.8;
            SkinMassRatSet = 0.10;
            AvgBodyTempSet = SkinMassRatSet * SkinTempSet + (1.0 - SkinMassRatSet) * CoreTempSet;

            // APPROXIMATE THE FOLLOWING VALUES TO START
            state.dataThermalComforts->SkinTemp = 33.7;
            state.dataThermalComforts->CoreTemp = 36.8;
            SkinBloodFlow = 6.3;
            EvapHeatLossStart = 5.0;
            state.dataThermalComforts->LatRespHeatLoss = 0.017251 * state.dataThermalComforts->ActLevel * (5.8662 - state.dataThermalComforts->VapPress);
            state.dataThermalComforts->EvapHeatLoss = (EvapHeatLossStart - state.dataThermalComforts->LatRespHeatLoss);
            SkinMassRat = 0.0417737 + 0.7451832 / (SkinBloodFlow + 0.585417);

            // GUESS CloSurfTemp TO START
            state.dataThermalComforts->CloSurfTemp = (state.dataThermalComforts->SkinTemp + state.dataThermalComforts->AirTemp) / 2.0;

            // SIMULATION OF TEMPERATURE REGULATION.
            // This SECTION simulates the temperature regulation over 1 minute.
            // Inputs are the physiological data from the previous time step and
            // the current environmental conditions.

            // BEGIN MINUTE BY MINUTE CALCULATIONS FOR ONE HOUR
            ActLevelStart = state.dataThermalComforts->ActLevel; // ActLevel gets increased by shivering in the following DO
            // loop and must be increased from the start level, not
            // perpetually increased
            for (IterMin = 1; IterMin <= 60; ++IterMin) {

                // Dry heat balance:  solve  for CloSurfTemp and Hr
                FirstMinIter = true;
                CloSurfTempOld = 0.0;
                while ((std::abs(state.dataThermalComforts->CloSurfTemp - CloSurfTempOld) > 0.01) || FirstMinIter) {
                    FirstMinIter = false;
                    CloSurfTempOld = state.dataThermalComforts->CloSurfTemp;
                    state.dataThermalComforts->Hr = 4.0 * state.dataThermalComforts->RadSurfEff * state.dataThermalComforts->StefanBoltz * pow_3((state.dataThermalComforts->CloSurfTemp + state.dataThermalComforts->RadTemp) / 2.0 + state.dataThermalComforts->TAbsConv);
                    state.dataThermalComforts->CloSurfTemp = (CloCond * state.dataThermalComforts->SkinTemp + state.dataThermalComforts->CloBodyRat * (state.dataThermalComforts->Hc * state.dataThermalComforts->AirTemp + state.dataThermalComforts->Hr * state.dataThermalComforts->RadTemp)) / (CloCond + state.dataThermalComforts->CloBodyRat * (state.dataThermalComforts->Hc + state.dataThermalComforts->Hr));
                }

                // CALCULATE THE COMBINED HEAT TRANSFER COEFF. (H)
                state.dataThermalComforts->H = state.dataThermalComforts->Hr + state.dataThermalComforts->Hc;
                // Heat flow from Clothing surface to environment
                state.dataThermalComforts->DryHeatLoss = state.dataThermalComforts->CloBodyRat * (state.dataThermalComforts->Hc * (state.dataThermalComforts->CloSurfTemp - state.dataThermalComforts->AirTemp) + state.dataThermalComforts->Hr * (state.dataThermalComforts->CloSurfTemp - state.dataThermalComforts->RadTemp));
                // dry and latent respiratory heat losses
                state.dataThermalComforts->LatRespHeatLoss = 0.017251 * state.dataThermalComforts->ActLevel * (5.8662 - state.dataThermalComforts->VapPress);
                state.dataThermalComforts->DryRespHeatLoss = 0.0014 * state.dataThermalComforts->ActLevel * (34.0 - state.dataThermalComforts->AirTemp) * StdAtm;
                state.dataThermalComforts->RespHeatLoss = state.dataThermalComforts->LatRespHeatLoss + state.dataThermalComforts->DryRespHeatLoss;
                // Heat flows to skin and core:
                state.dataThermalComforts->HeatFlow = (state.dataThermalComforts->CoreTemp - state.dataThermalComforts->SkinTemp) * (5.28 + 1.163 * SkinBloodFlow);
                // 5.28 is skin conductance in the
                // absence of skin blood flow
                SkinHeatStorage = state.dataThermalComforts->HeatFlow - state.dataThermalComforts->DryHeatLoss - state.dataThermalComforts->EvapHeatLoss;
                CoreHeatStorage = state.dataThermalComforts->ActLevel - (state.dataThermalComforts->CoreTemp - state.dataThermalComforts->SkinTemp) * (5.28 + 1.163 * SkinBloodFlow) - state.dataThermalComforts->RespHeatLoss - state.dataThermalComforts->WorkEff;

                // Thermal capacities (average man: 70 kg, 1.8 square meter).
                state.dataThermalComforts->CoreThermCap = state.dataThermalComforts->ActLevelConv * (1.0 - SkinMassRat) * 70.0;
                state.dataThermalComforts->SkinThermCap = state.dataThermalComforts->ActLevelConv * SkinMassRat * 70.0;

                // Temperature changes in 1 minute
                state.dataThermalComforts->SkinTempChange = (SkinHeatStorage * 1.8) / state.dataThermalComforts->SkinThermCap;
                state.dataThermalComforts->CoreTempChange = (CoreHeatStorage * 1.8) / state.dataThermalComforts->CoreThermCap;
                BodyTempChange = SkinMassRat * state.dataThermalComforts->SkinTempChange + (1.0 - SkinMassRat) * state.dataThermalComforts->CoreTempChange;
                state.dataThermalComforts->SkinTemp += state.dataThermalComforts->SkinTempChange;
                state.dataThermalComforts->CoreTemp += state.dataThermalComforts->CoreTempChange;
                AvgBodyTemp = SkinMassRat * state.dataThermalComforts->SkinTemp + (1.0 - SkinMassRat) * state.dataThermalComforts->CoreTemp;

                if (state.dataThermalComforts->SkinTemp > SkinTempSet) {
                    SkinThermSigWarm = state.dataThermalComforts->SkinTemp - SkinTempSet;
                    SkinThermSigCold = 0.0;
                } else {
                    SkinThermSigCold = SkinTempSet - state.dataThermalComforts->SkinTemp;
                    SkinThermSigWarm = 0.0;
                }

                if (state.dataThermalComforts->CoreTemp > CoreTempSet) {
                    CoreThermSigWarm = state.dataThermalComforts->CoreTemp - CoreTempSet;
                    CoreThermSigCold = 0.0;
                } else {
                    CoreThermSigCold = CoreTempSet - state.dataThermalComforts->CoreTemp;
                    CoreThermSigWarm = 0.0;
                }

                if (AvgBodyTemp > AvgBodyTempSet) {
                    BodyThermSigWarm = AvgBodyTemp - AvgBodyTempSet;
                    BodyThermSigCold = 0.0;
                } else {
                    BodyThermSigCold = AvgBodyTempSet - AvgBodyTemp;
                    BodyThermSigWarm = 0.0;
                }

                state.dataThermalComforts->VasodilationFac = SkinBloodFlowConst * CoreThermSigWarm;
                state.dataThermalComforts->VasoconstrictFac = Str * SkinThermSigCold;
                SkinBloodFlow = (6.3 + state.dataThermalComforts->VasodilationFac) / (1.0 + state.dataThermalComforts->VasoconstrictFac);

                // SkinBloodFlow is never below 0.5 liter/(m2.hr) nor above MaxSkinBloodFlow
                if (SkinBloodFlow < 0.5) SkinBloodFlow = 0.5;
                if (SkinBloodFlow > MaxSkinBloodFlow) SkinBloodFlow = MaxSkinBloodFlow;

                // ratio of skin-core masses change with SkinBloodFlow
                // (SkinMassRat,SkinBloodFlow) = (.15,6.3),(.45,1.24),(.05,90)
                SkinMassRat = 0.0417737 + 0.7451832 / (SkinBloodFlow + 0.585417);

                // control of regulatory sweating
                if (SkinThermSigWarm == 0.0) {
                    RegSweat = SweatContConst * BodyThermSigWarm;
                } else {
                    RegSweat = SweatContConst * BodyThermSigWarm * std::exp(SkinThermSigWarm / 10.7);
                }

                if (RegSweat > RegSweatMax) RegSweat = RegSweatMax;

                state.dataThermalComforts->EvapHeatLossRegSweat = 0.68 * RegSweat;

                // adjustment of metabolic heat due to shivering (Stolwijk, Hardy)
                state.dataThermalComforts->ShivResponse = 19.4 * SkinThermSigCold * CoreThermSigCold;
                state.dataThermalComforts->ActLevel = ActLevelStart + state.dataThermalComforts->ShivResponse;

                // Evaluation of heat transfer by evaporation at skin surface
                // LewisRat varies with SkinTemp.
                // LewisRat=2.02 C/mmHg or 15.1512 C/kPa at 0 C (lr=2.2 at 25 C)
                LewisRat = 15.1512 * (state.dataThermalComforts->SkinTemp + state.dataThermalComforts->TAbsConv) / state.dataThermalComforts->TAbsConv;

                // Mass transfer equation between skin and environment
                // TotEvapHeatResist is total vapor resistance of CloUnitthing + air layer
                // CloInsul is efficiency of mass transfer for CloUnitthing
                // CloInsul IS SET TO .45 (FOR WOVEN MATERIAL)
                // Reference:  Woodcock, Breckenridge and Goldman
                state.dataThermalComforts->CloInsul = 0.45;
                state.dataThermalComforts->CloThermEff = 1.0 / (1.0 + 0.155 * state.dataThermalComforts->CloBodyRat * state.dataThermalComforts->H * state.dataThermalComforts->CloUnit);

                AirEvapHeatResist = 1.0 / (LewisRat * state.dataThermalComforts->CloBodyRat * state.dataThermalComforts->Hc);
                CloEvapHeatResist = 0.155 * state.dataThermalComforts->CloUnit / (LewisRat * state.dataThermalComforts->CloInsul);
                TotEvapHeatResist = AirEvapHeatResist + CloEvapHeatResist;

                state.dataThermalComforts->SatSkinVapPress = CalcSatVapPressFromTemp(state.dataThermalComforts->SkinTemp);
                state.dataThermalComforts->SatSkinVapPress *= 0.1333227;
                state.dataThermalComforts->EvapHeatLossMax = (1.0 / TotEvapHeatResist) * (state.dataThermalComforts->SatSkinVapPress - state.dataThermalComforts->VapPress);
                state.dataThermalComforts->SkinWetSweat = state.dataThermalComforts->EvapHeatLossRegSweat / state.dataThermalComforts->EvapHeatLossMax;

                // 0.06 if SkinWetDiff for nonsweating skin --- Kerslake
                state.dataThermalComforts->SkinWetDiff = (1.0 - state.dataThermalComforts->SkinWetSweat) * 0.06;
                state.dataThermalComforts->EvapHeatLossDiff = state.dataThermalComforts->SkinWetDiff * state.dataThermalComforts->EvapHeatLossMax;
                state.dataThermalComforts->EvapHeatLoss = state.dataThermalComforts->EvapHeatLossRegSweat + state.dataThermalComforts->EvapHeatLossDiff;
                state.dataThermalComforts->SkinWetTot = state.dataThermalComforts->EvapHeatLoss / state.dataThermalComforts->EvapHeatLossMax;

                // Beginning of dripping (Sweat not evaporated on skin surface)
                if ((state.dataThermalComforts->SkinWetTot >= EvapEff) && (state.dataThermalComforts->EvapHeatLossMax >= 0)) {
                    state.dataThermalComforts->SkinWetTot = EvapEff;
                    state.dataThermalComforts->SkinWetSweat = (EvapEff - 0.06) / 0.94;
                    state.dataThermalComforts->EvapHeatLossRegSweat = state.dataThermalComforts->SkinWetSweat * state.dataThermalComforts->EvapHeatLossMax;
                    state.dataThermalComforts->SkinWetDiff = (1.0 - state.dataThermalComforts->SkinWetSweat) * 0.06;
                    state.dataThermalComforts->EvapHeatLossDiff = state.dataThermalComforts->SkinWetDiff * state.dataThermalComforts->EvapHeatLossMax;
                    state.dataThermalComforts->EvapHeatLoss = state.dataThermalComforts->EvapHeatLossRegSweat + state.dataThermalComforts->EvapHeatLossDiff;
                }

                // When EvapHeatLossMax<0. condensation on skin occurs.
                if (state.dataThermalComforts->EvapHeatLossMax <= 0.0) {
                    state.dataThermalComforts->SkinWetDiff = 0.0;
                    state.dataThermalComforts->EvapHeatLossDiff = 0.0;
                    state.dataThermalComforts->EvapHeatLoss = state.dataThermalComforts->EvapHeatLossMax;
                    state.dataThermalComforts->SkinWetTot = EvapEff;
                    state.dataThermalComforts->SkinWetSweat = EvapEff;
                    state.dataThermalComforts->EvapHeatLossRegSweat = 0.0;
                }

                // UnevapSweat = unevaporated sweat in grams/sq.m/hr
                UnevapSweat = (RegSweat * 0.68 - state.dataThermalComforts->SkinWetSweat * state.dataThermalComforts->EvapHeatLossMax) / 0.68;
                if (UnevapSweat <= 0.0) UnevapSweat = 0.0;

                // Vapor pressure at skin (as measured by dewpoint sensors)
                state.dataThermalComforts->SkinVapPress = state.dataThermalComforts->SkinWetTot * state.dataThermalComforts->SatSkinVapPress + (1.0 - state.dataThermalComforts->SkinWetTot) * state.dataThermalComforts->VapPress;

                // SkinRelHum is skin relative humidity
                SkinRelHum = state.dataThermalComforts->SkinVapPress / state.dataThermalComforts->SatSkinVapPress;

            } // END OF MINUTE BY MINUTE TEMPERATURE REGULATION LOOP

            // Computation of comfort indices.
            // Inputs to this SECTION are the physiological data from the simulation of
            // temperature regulation loop

            // PART I: Heat transfer indices in real environment
            state.dataThermalComforts->OpTemp = (state.dataThermalComforts->Hr * state.dataThermalComforts->RadTemp + state.dataThermalComforts->Hc * state.dataThermalComforts->AirTemp) / state.dataThermalComforts->H;
            EffectCloUnit = state.dataThermalComforts->CloUnit - (state.dataThermalComforts->CloBodyRat - 1.0) / (0.155 * state.dataThermalComforts->CloBodyRat * state.dataThermalComforts->H);
            EffectCloThermEff = 1.0 / (1.0 + 0.155 * state.dataThermalComforts->H * EffectCloUnit);
            state.dataThermalComforts->CloPermeatEff = 1.0 / (1.0 + (0.155 / state.dataThermalComforts->CloInsul) * state.dataThermalComforts->Hc * EffectCloUnit);

            // PART II: ET*(standardization humidity/REAL(r64) CloUnit, StdAtm and Hc)
            // calculation of skin heat Loss (SkinHeatLoss)
            SkinHeatLoss = state.dataThermalComforts->H * EffectCloThermEff * (state.dataThermalComforts->SkinTemp - state.dataThermalComforts->OpTemp) + state.dataThermalComforts->SkinWetTot * LewisRat * state.dataThermalComforts->Hc * state.dataThermalComforts->CloPermeatEff * (state.dataThermalComforts->SatSkinVapPress - state.dataThermalComforts->VapPress);
            // Get a low approximation for ET* and solve balance
            // equation by iteration
            ET = state.dataThermalComforts->SkinTemp - SkinHeatLoss / (state.dataThermalComforts->H * EffectCloThermEff);
            // THE STANDARD VAPOR PRESSURE AT THE EFFECTIVE TEMP : StdVapPressET

            while (true) {
                StdVapPressET = CalcSatVapPressFromTemp(ET);
                StdVapPressET *= VapPressConv;
                EnergyBalErrET = SkinHeatLoss - state.dataThermalComforts->H * EffectCloThermEff * (state.dataThermalComforts->SkinTemp - ET) -
                                 state.dataThermalComforts->SkinWetTot * LewisRat * state.dataThermalComforts->Hc * state.dataThermalComforts->CloPermeatEff * (state.dataThermalComforts->SatSkinVapPress - StdVapPressET / 2.0);
                if (EnergyBalErrET >= 0.0) break;
                ET += 0.1;
            }

            // Part III: Standard effective temperature SET*
            // standardized humidity.  Hc, CloUnit, StdAtm
            // normalized for given ActLeAirVelivity

            // Standard environment
            HrStd = state.dataThermalComforts->Hr;
            // HcStd = standard conv. heat tr. coeff. (level walking/still air)
            if (ActMet <= 0.86) ActMet = 0.86;
            HcStd = 5.66 * std::pow(ActMet - 0.85, 0.39);

            // minimum value of Hc at sea leAirVel = 3.0 (AirVel = .137 m/s)
            if (HcStd <= 3.0) HcStd = 3.0;

            // standard MET - StdCloUnit relation gives SET* = 24 C when PMV = 0
            StdCloUnit = 1.3264 / ((state.dataThermalComforts->ActLevel - state.dataThermalComforts->WorkEff) / state.dataThermalComforts->ActLevelConv + 0.7383) - 0.0953;
            StdCloFac = CloFac;
            StdCloBodyRat = 1.0 + StdCloFac * StdCloUnit;
            HStd = HrStd + HcStd;
            StdEffectCloUnit = StdCloUnit - (StdCloBodyRat - 1.0) / (0.155 * StdCloBodyRat * HStd);
            StdEffectCloThermEff = 1.0 / (1.0 + 0.155 * HStd * StdEffectCloUnit);
            StdCloPermeatEff = 1.0 / (1.0 + (0.155 / 0.45) * HcStd * StdEffectCloUnit);

            // Get a low approximation for SET*
            // and solve balance equ. by iteration
            SET = state.dataThermalComforts->SkinTemp - SkinHeatLoss / (HStd * StdEffectCloThermEff);

            while (true) {
                StdVapPressSET = CalcSatVapPressFromTemp(SET);
                StdVapPressSET *= VapPressConv;
                EnergyBalErrSET = SkinHeatLoss - HStd * StdEffectCloThermEff * (state.dataThermalComforts->SkinTemp - SET) -
                                  state.dataThermalComforts->SkinWetTot * LewisRat * HcStd * StdCloPermeatEff * (state.dataThermalComforts->SatSkinVapPress - StdVapPressSET / 2.0);
                if (EnergyBalErrSET >= 0.0) break;
                SET += 0.1;
            }

            // Part IV:  Fanger's comfort equation.
            // Thermal transfer coefficient to calculate PMV
            state.dataThermalComforts->ThermSensTransCoef = 0.303 * std::exp(-0.036 * state.dataThermalComforts->ActLevel) + 0.028;
            // Fanger's reg. sweating at comfort threshold (PMV=0) is:
            state.dataThermalComforts->EvapHeatLossRegComf = (state.dataThermalComforts->IntHeatProd - state.dataThermalComforts->ActLevelConv) * 0.42;

            // PMV*(PMVET in prgm) uses ET instead of OpTemp
            DryHeatLossET = HStd * StdEffectCloThermEff * (state.dataThermalComforts->SkinTemp - ET);
            state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).PiercePMVET =
                state.dataThermalComforts->ThermSensTransCoef * (state.dataThermalComforts->IntHeatProd - state.dataThermalComforts->RespHeatLoss - DryHeatLossET - state.dataThermalComforts->EvapHeatLossDiff - state.dataThermalComforts->EvapHeatLossRegComf);

            // SPMV*(PMVSET in prgm) uses SET instead of OpTemp
            DryHeatLossSET = HStd * StdEffectCloThermEff * (state.dataThermalComforts->SkinTemp - SET);
            state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).PiercePMVSET =
                state.dataThermalComforts->ThermSensTransCoef * (state.dataThermalComforts->IntHeatProd - state.dataThermalComforts->RespHeatLoss - DryHeatLossSET - state.dataThermalComforts->EvapHeatLossDiff - state.dataThermalComforts->EvapHeatLossRegComf);

            // Part V:  Heat stress and heat strain indices derived from EvapHeatLoss,
            // EvapHeatLossMax and W (skin wettedness)

            // EvapHeatLossMax is readjusted for EvapEff
            state.dataThermalComforts->EvapHeatLossMax *= EvapEff;
            // DISC (discomfort) varies with relative thermoregulatory strain
            state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).PierceDISC =
                5.0 * (state.dataThermalComforts->EvapHeatLossRegSweat - state.dataThermalComforts->EvapHeatLossRegComf) / (state.dataThermalComforts->EvapHeatLossMax - state.dataThermalComforts->EvapHeatLossRegComf - state.dataThermalComforts->EvapHeatLossDiff);

            // Part VI:  Thermal sensation TSENS as function of mean body temp.-
            // AvgBodyTempLow is AvgBodyTemp when DISC is 0. (lower limit of zone of evap. regul.)
            AvgBodyTempLow = (0.185 / state.dataThermalComforts->ActLevelConv) * (state.dataThermalComforts->ActLevel - state.dataThermalComforts->WorkEff) + 36.313;
            // AvgBodyTempHigh is AvgBodyTemp when HSI=100 (upper limit of zone of evap. regul.)
            AvgBodyTempHigh = (0.359 / state.dataThermalComforts->ActLevelConv) * (state.dataThermalComforts->ActLevel - state.dataThermalComforts->WorkEff) + 36.664;

            // TSENS=DISC=4.7 when HSI =1 00 (HSI is Belding's classic heat stress index)
            // In cold, DISC &TSENS are the same and neg. fct of AvgBodyTemp
            if (AvgBodyTemp > AvgBodyTempLow) {
                state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).PierceTSENS = 4.7 * (AvgBodyTemp - AvgBodyTempLow) / (AvgBodyTempHigh - AvgBodyTempLow);

            } else {
                state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).PierceTSENS = 0.68175 * (AvgBodyTemp - AvgBodyTempLow);
                state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).PierceDISC = state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).PierceTSENS;
            }

            state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).ThermalComfortMRT = state.dataThermalComforts->RadTemp;
            state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).ThermalComfortOpTemp = (state.dataThermalComforts->RadTemp + state.dataThermalComforts->AirTemp) / 2.0;
            state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).PierceSET = SET;
        }
    }

    void CalcThermalComfortKSU(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //     AUTHOR         Jaewook Lee
        //     DATE WRITTEN   January 2000
        //     MODIFIED       Rick Strand (for E+ implementation February 2000)

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine calculates TSV using the KSU 2 Node model.

        // METHODOLOGY EMPLOYED:
        // This subroutine is based heavily upon the work performed by Dan Maloney for
        // the BLAST program.  Many of the equations are based on the original Pierce
        // development.  See documentation for further details and references.

        // REFERENCES:
        // Maloney, Dan, M.S. Thesis, University of Illinois at Urbana-Champaign

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const CloEmiss(0.8); // Clothing Emissivity

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        static Array1D<Real64> Coeff(2);      // Coefficients used in Range-Kutta's Method
        static Array1D<Real64> Temp(2);       // Temperature
        static Array1D<Real64> TempChange(2); // Change of temperature
        Real64 BodyWt;                        // Weight of body, kg
        Real64 DayNum;                        // Number of days of acclimation
        int NumDay;                           // Loop counter for DayNum
        Real64 EmissAvg;                      // Average emissivity
        int IncreDayNum;                      // Number of days of increment in the outputs as desired
        Real64 IntHeatProdMet;                // Internal heat production in MET
        Real64 IntHeatProdMetMax;             // Maximum value of internal heat production in MET
        int LastDayNum;                       // Number of days for the last print out
        Real64 SkinWetFac;                    // Skin wettedness factor
        Real64 SkinWetNeut;                   // Skin wettedness at neutral state
        int StartDayNum;                      // Number of days for the first print out
        // Unacclimated man = 1, Acclimated man = 14
        Real64 SweatSuppFac; // Sweat suppression factor due to skin wettedness
        Real64 TempDiffer;   // Temperature difference between the rectal and esophageal temperatures
        // If not measured, set it to be 0.5 Deg. C.
        int TempIndiceNum;     // Number of temperature indices
        Real64 ThermCndctMin;  // Minimum value of thermal conductance
        Real64 ThermCndctNeut; // Thermal conductance at neutral state
        Real64 TimeExpos;      // Time period in the exposure, hr
        Real64 TimeInterval;   // Time interval of outputs desired, hr
        Real64 TSVMax;         // Maximum value of thermal sensation vote
        Real64 IntermediateClothing;

        TempIndiceNum = 2;

        // NEXT GROUP OF VARIABLE ARE FIXED FOR BLAST PROGRAM - UNACCLIMATED MAN
        // THE TSV MODEL CAN BE APPLIED TO UNACCLIMATED MAN ONLY.
        TimeInterval = 1.0;
        TSVMax = 4.0;
        StartDayNum = 1;
        LastDayNum = 1;
        IncreDayNum = 1;
        TimeExpos = 1.0;
        TempDiffer = 0.5;

        for (state.dataThermalComforts->PeopleNum = 1; state.dataThermalComforts->PeopleNum <= TotPeople; ++state.dataThermalComforts->PeopleNum) {
            // THE NEXT SIX VARIABLES WILL BE READ IN FROM INPUT DECK
            if (!People(state.dataThermalComforts->PeopleNum).KSU) continue;

            state.dataThermalComforts->ZoneNum = People(state.dataThermalComforts->PeopleNum).ZonePtr;
            if (IsZoneDV(state.dataThermalComforts->ZoneNum) || IsZoneUI(state.dataThermalComforts->ZoneNum)) {
                state.dataThermalComforts->AirTemp = TCMF(state.dataThermalComforts->ZoneNum); // PH 3/7/04
            } else {
                state.dataThermalComforts->AirTemp = ZTAVComf(state.dataThermalComforts->ZoneNum);
            }
            state.dataThermalComforts->RadTemp = CalcRadTemp(state, state.dataThermalComforts->PeopleNum);
            state.dataThermalComforts->RelHum = PsyRhFnTdbWPb(state.dataThermalComforts->AirTemp, ZoneAirHumRatAvgComf(state.dataThermalComforts->ZoneNum), OutBaroPress);
            state.dataThermalComforts->ActLevel = GetCurrentScheduleValue(People(state.dataThermalComforts->PeopleNum).ActivityLevelPtr) / state.dataThermalComforts->BodySurfArea;
            state.dataThermalComforts->WorkEff = GetCurrentScheduleValue(People(state.dataThermalComforts->PeopleNum).WorkEffPtr) * state.dataThermalComforts->ActLevel;
            {
                auto const SELECT_CASE_var(People(state.dataThermalComforts->PeopleNum).ClothingType);
                if (SELECT_CASE_var == 1) {
                    state.dataThermalComforts->CloUnit = GetCurrentScheduleValue(People(state.dataThermalComforts->PeopleNum).ClothingPtr);
                } else if (SELECT_CASE_var == 2) {
                    state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).ThermalComfortOpTemp = (state.dataThermalComforts->RadTemp + state.dataThermalComforts->AirTemp) / 2.0;
                    state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).ClothingValue = state.dataThermalComforts->CloUnit;
                    DynamicClothingModel(state);
                    state.dataThermalComforts->CloUnit = state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).ClothingValue;
                } else if (SELECT_CASE_var == 3) {
                    IntermediateClothing = GetCurrentScheduleValue(People(state.dataThermalComforts->PeopleNum).ClothingMethodPtr);
                    if (IntermediateClothing == 1.0) {
                        state.dataThermalComforts->CloUnit = GetCurrentScheduleValue(People(state.dataThermalComforts->PeopleNum).ClothingPtr);
                        state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).ClothingValue = state.dataThermalComforts->CloUnit;
                    } else if (IntermediateClothing == 2.0) {
                        state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).ThermalComfortOpTemp = (state.dataThermalComforts->RadTemp + state.dataThermalComforts->AirTemp) / 2.0;
                        state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).ClothingValue = state.dataThermalComforts->CloUnit;
                        DynamicClothingModel(state);
                        state.dataThermalComforts->CloUnit = state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).ClothingValue;
                    } else {
                        state.dataThermalComforts->CloUnit = GetCurrentScheduleValue(People(state.dataThermalComforts->PeopleNum).ClothingPtr);
                        ShowWarningError("PEOPLE=\"" + People(state.dataThermalComforts->PeopleNum).Name +
                                         "\", Scheduled clothing value will be used rather than clothing calculation method.");
                    }
                } else {
                    ShowSevereError("PEOPLE=\"" + People(state.dataThermalComforts->PeopleNum).Name + "\", Incorrect Clothing Type");
                }
            }

            state.dataThermalComforts->AirVel = GetCurrentScheduleValue(People(state.dataThermalComforts->PeopleNum).AirVelocityPtr);
            state.dataThermalComforts->IntHeatProd = state.dataThermalComforts->ActLevel - state.dataThermalComforts->WorkEff;
            // THE FOLLOWING ARE TYPICAL VALUES SET FOR BLAST RUNS
            // STANDARD MAN: 70. KG WEIGHT, 1.8 M2 SURFACE AREA
            BodyWt = 70.0;
            state.dataThermalComforts->CoreTemp = 37.0;
            state.dataThermalComforts->SkinTemp = 31.0;

            //   CALCULATIONS NEEDED FOR THE PASSIVE STATE EQUATIONS
            state.dataThermalComforts->CoreThermCap = 0.9 * BodyWt * 0.97 / state.dataThermalComforts->BodySurfArea;
            state.dataThermalComforts->SkinThermCap = 0.1 * BodyWt * 0.97 / state.dataThermalComforts->BodySurfArea;
            //   KERSLAKE'S FORMULA (0.05<AirVel<5. M/S)
            if (state.dataThermalComforts->AirVel < 0.137) state.dataThermalComforts->AirVel = 0.137;
            state.dataThermalComforts->Hc = 8.3 * std::sqrt(state.dataThermalComforts->AirVel);
            EmissAvg = state.dataThermalComforts->RadSurfEff * CloEmiss + (1.0 - state.dataThermalComforts->RadSurfEff) * 1.0;
            //   IBERALL EQUATION
            state.dataThermalComforts->Hr = EmissAvg * (3.87 + 0.031 * state.dataThermalComforts->RadTemp);
            state.dataThermalComforts->H = state.dataThermalComforts->Hr + state.dataThermalComforts->Hc;
            state.dataThermalComforts->OpTemp = (state.dataThermalComforts->Hc * state.dataThermalComforts->AirTemp + state.dataThermalComforts->Hr * state.dataThermalComforts->RadTemp) / state.dataThermalComforts->H;
            state.dataThermalComforts->VapPress = CalcSatVapPressFromTemp(state.dataThermalComforts->AirTemp);
            state.dataThermalComforts->VapPress *= state.dataThermalComforts->RelHum;
            state.dataThermalComforts->CloBodyRat = 1.0 + 0.2 * state.dataThermalComforts->CloUnit;
            state.dataThermalComforts->CloThermEff = 1.0 / (1.0 + 0.155 * state.dataThermalComforts->H * state.dataThermalComforts->CloBodyRat * state.dataThermalComforts->CloUnit);
            state.dataThermalComforts->CloPermeatEff = 1.0 / (1.0 + 0.143 * state.dataThermalComforts->Hc * state.dataThermalComforts->CloUnit);
            //  BASIC INFORMATION FOR THERMAL SENSATION.
            IntHeatProdMet = state.dataThermalComforts->IntHeatProd / state.dataThermalComforts->ActLevelConv;
            IntHeatProdMetMax = max(1.0, IntHeatProdMet);
            ThermCndctNeut = 12.05 * std::exp(0.2266 * (IntHeatProdMetMax - 1.0));
            SkinWetNeut = 0.02 + 0.4 * (1.0 - std::exp(-0.6 * (IntHeatProdMetMax - 1.0)));
            ThermCndctMin = (ThermCndctNeut - 5.3) * 0.26074074 + 5.3;
            Real64 const ThemCndct_75_fac(1.0 / (75.0 - ThermCndctNeut));
            Real64 const ThemCndct_fac(1.0 / (ThermCndctNeut - ThermCndctMin));
            //  CALCULATE THE PHYSIOLOGICAL REACTIONS OF AN UNACCLIMATED
            //  MAN (LastDayNum = 1), OR AN ACCLIMATED MAN (LastDayNum = 14, IncreDayNum = 13),
            assert(IncreDayNum > 0); // Autodesk:F2C++ Loop setup assumption
            for (NumDay = StartDayNum; NumDay <= LastDayNum; NumDay += IncreDayNum) {
                //  INITIAL CONDITIONS IN AN EXPOSURE
                DayNum = double(NumDay);
                state.dataThermalComforts->Time = 0.0;
                state.dataThermalComforts->TimeChange = 0.01;
                SweatSuppFac = 1.0;
                Temp(1) = state.dataThermalComforts->CoreTemp;
                Temp(2) = state.dataThermalComforts->SkinTemp;
                Coeff(1) = Coeff(2) = 0.0;
                //  PHYSIOLOGICAL ADJUSTMENTS IN HEAT ACCLIMATION.
                state.dataThermalComforts->AcclPattern = 1.0 - std::exp(-0.12 * (DayNum - 1.0));
                state.dataThermalComforts->CoreTempNeut = 36.9 - 0.6 * state.dataThermalComforts->AcclPattern;
                state.dataThermalComforts->SkinTempNeut = 33.8 - 1.6 * state.dataThermalComforts->AcclPattern;
                state.dataThermalComforts->ActLevel -= 0.07 * state.dataThermalComforts->ActLevel * state.dataThermalComforts->AcclPattern;
                Real64 const SkinTempNeut_fac(1.0 / (1.0 - SkinWetNeut));
                //  CALCULATION OF CoreTempChange/TempChange & SkinTempChange/TempChange
                DERIV(state, TempIndiceNum, Temp, TempChange);
                while (true) {
                    //  CALCULATION OF THERMAL SENSATION VOTE (TSV).
                    //  THE TSV MODEL CAN BE APPLIED TO UNACCLIMATED MAN ONLY.
                    SkinWetFac = (state.dataThermalComforts->SkinWetSweat - SkinWetNeut) * SkinTempNeut_fac;
                    state.dataThermalComforts->VasodilationFac = (state.dataThermalComforts->ThermCndct - ThermCndctNeut) * ThemCndct_75_fac;
                    state.dataThermalComforts->VasoconstrictFac = (ThermCndctNeut - state.dataThermalComforts->ThermCndct) * ThemCndct_fac;
                    //  IF VasodilationFac < 0.0, VASOCONSTRICTION OCCURS AND RESULTS IN COLD SENSATION.
                    //  OTHERWISE NORMAL BLOOD FLOW OR VASODILATION OCCURS AND RESULTS IN
                    //  THERMAL NEUTRALITY OR WARM SENSATION.
                    if (state.dataThermalComforts->VasodilationFac < 0) {
                        state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).KsuTSV =
                            -1.46153 * state.dataThermalComforts->VasoconstrictFac + 3.74721 * pow_2(state.dataThermalComforts->VasoconstrictFac) - 6.168856 * pow_3(state.dataThermalComforts->VasoconstrictFac);
                    } else {
                        state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).KsuTSV = (5.0 - 6.56 * (state.dataThermalComforts->RelHum - 0.50)) * SkinWetFac;
                        if (state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).KsuTSV > TSVMax) state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).KsuTSV = TSVMax;
                    }

                    state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).ThermalComfortMRT = state.dataThermalComforts->RadTemp;
                    state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).ThermalComfortOpTemp = (state.dataThermalComforts->RadTemp + state.dataThermalComforts->AirTemp) / 2.0;

                    state.dataThermalComforts->CoreTemp = Temp(1);
                    state.dataThermalComforts->SkinTemp = Temp(2);
                    state.dataThermalComforts->EvapHeatLossSweatPrev = state.dataThermalComforts->EvapHeatLossSweat;

                    RKG(state, TempIndiceNum, state.dataThermalComforts->TimeChange, state.dataThermalComforts->Time, Temp, TempChange, Coeff);

                    if (state.dataThermalComforts->Time > TimeExpos) break;
                }
            }
        }
    }

    void DERIV(EnergyPlusData &state, int &EP_UNUSED(TempIndiceNum),    // Number of temperature indices  unused1208
               Array1D<Real64> &EP_UNUSED(Temp), // Temperature unused1208
               Array1D<Real64> &TempChange       // Change of temperature
    )
    {

        // SUBROUTINE INFORMATION:
        //     AUTHOR         Jaewook Lee
        //     DATE WRITTEN   January 2000
        //     MODIFIED       Rick Strand (for E+ implementation February 2000)

        // PURPOSE OF THIS SUBROUTINE:
        // THIS SUBROUTINE CALCULATES HEAT TRANSFER TERMS INVOLVED IN THE
        // THERMOREGULATORY SYSTEM TO OBTAIN THE RATES OF CHANGE OF CoreTemp & SkinTemp
        // VIZ., CoreTempChange/TempChange & SkinTempChange/TempChange RESPECTIVELY.

        // METHODOLOGY EMPLOYED:
        // This subroutine is based heavily upon the work performed by Dan Maloney for
        // the BLAST program.  Many of the equations are based on the original Pierce
        // development.  See documentation for further details and references.

        // REFERENCES:
        // Maloney, Dan, M.S. Thesis, University of Illinois at Urbana-Champaign

        // Argument array dimensioning
        //EP_SIZE_CHECK(Temp, 2);
        EP_SIZE_CHECK(TempChange, 2);

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 ActLevelTot;             // Total activity level
        Real64 CoreSignalShiv;          // Core signal when shivering occurs
        Real64 CoreSignalShivMax;       // Maximum value of core signal when shivering occurs
        Real64 CoreSignalSkinSens;      // The sensitivity of the skin signal increases
        Real64 CoreSignalSweatMax;      // Maximum value of core signal when sweating occurs
        Real64 CoreSignalSweatWarm;     // Core signal when sweating occurs
        Real64 CoreTempSweat;           // Core temperature when sweating occurs
        Real64 CoreSignalWarm;          // Warm core signal
        Real64 CoreSignalWarmMax;       // Maximum value of warm core signal
        Real64 EvapHeatLossDrySweat;    // Evaporative heat loss by sweating when total skin wettedness < 0.4
        Real64 Err;                     // Stop criteria for iteration
        Real64 ErrPrev;                 // Previous value of stop criteria for iteration
        Real64 EvapHeatLossSweatEst;    // Estimated evaporative heat loss by sweating
        Real64 EvapHeatLossSweatEstNew; // New value of estimated evaporative heat loss by sweating
        Real64 IntHeatProdTot;          // Total internal heat production
        Real64 SkinCndctMax;            // Maximum value of skin conductance
        Real64 SkinSignalCold;          // Cold skin signal
        Real64 SkinSignalColdMax;       // Maximum value of cold skin signal
        Real64 SkinSignalSweatCold;     // Cold skin signal for sweat inhibition
        Real64 SkinSignalSweatColdMax;  // Maximum value of cold skin signal for sweat inhibition
        Real64 SkinCndctDilation;       // Overall skin conductance due to vasodilation
        Real64 SkinCndctConstriction;   // Overall skin conductance due to vasoconstriction
        Real64 SkinSignalShiv;          // Skin signal when shivering occurs
        Real64 SkinSignalShivMax;       // Maximum value of skin signal when shivering occurs
        Real64 SkinSignalSweatMax;      // Skin signal when sweating occurs
        Real64 SkinSignalSweatWarm;     // Maximum value of skin signal when sweating occurs
        Real64 SkinSignalWarm;          // Warm skin signal
        Real64 SkinSignalWarmMax;       // Maximum value of warm skin signal
        Real64 SkinTempSweat;           // Skin temperature when sweating occurs
        Real64 SkinWetSignal;           // Skin wettedness signal
        Real64 SweatCtrlFac;            // Sweat control factor
        Real64 SweatSuppFac;            // Sweat suppression factor due to skin wettedness
        Real64 WeighFac;                // Weighting factor of core siganl

        // THE CONTROLLING SYSTEM.
        // THE CONTROLLING SIGNALS :
        // SIGNALS FOR KS.
        CoreSignalWarm = state.dataThermalComforts->CoreTemp - 36.98;
        SkinSignalWarm = state.dataThermalComforts->SkinTemp - 33.8;
        SkinSignalCold = 32.1 - state.dataThermalComforts->SkinTemp;
        CoreSignalSkinSens = state.dataThermalComforts->CoreTemp - 35.15;
        CoreSignalWarmMax = max(0.0, CoreSignalWarm);
        SkinSignalWarmMax = max(0.0, SkinSignalWarm);
        SkinSignalColdMax = max(0.0, SkinSignalCold);

        // SIGNALS FOR EvapHeatLossSweat.
        CoreTempSweat = state.dataThermalComforts->CoreTemp;
        if (CoreTempSweat > 38.29) CoreTempSweat = 38.29;
        CoreSignalSweatWarm = CoreTempSweat - state.dataThermalComforts->CoreTempNeut;
        SkinTempSweat = state.dataThermalComforts->SkinTemp;
        if (SkinTempSweat > 36.1) SkinTempSweat = 36.1;
        SkinSignalSweatWarm = SkinTempSweat - state.dataThermalComforts->SkinTempNeut;
        CoreSignalSweatMax = max(0.0, CoreSignalSweatWarm);
        SkinSignalSweatMax = max(0.0, SkinSignalSweatWarm);
        SkinSignalSweatCold = 33.37 - state.dataThermalComforts->SkinTemp;
        if (state.dataThermalComforts->SkinTempNeut < 33.37) SkinSignalSweatCold = state.dataThermalComforts->SkinTempNeut - state.dataThermalComforts->SkinTemp;
        SkinSignalSweatColdMax = max(0.0, SkinSignalSweatCold);

        // SIGNALS FOR SHIVERING.
        CoreSignalShiv = 36.9 - state.dataThermalComforts->CoreTemp;
        SkinSignalShiv = 32.5 - state.dataThermalComforts->SkinTemp;
        CoreSignalShivMax = max(0.0, CoreSignalShiv);
        SkinSignalShivMax = max(0.0, SkinSignalShiv);

        // CONTROLLING FUNCTIONS :
        // SHIVERING RESPONSE IN W/M**2.
        state.dataThermalComforts->ShivResponse = 20.0 * CoreSignalShivMax * SkinSignalShivMax + 5.0 * SkinSignalShivMax;
        if (state.dataThermalComforts->CoreTemp >= 37.1) state.dataThermalComforts->ShivResponse = 0.0;

        // SWEAT FUNCTION IN W/M**2.
        WeighFac = 260.0 + 70.0 * state.dataThermalComforts->AcclPattern;
        SweatCtrlFac = 1.0 + 0.05 * std::pow(SkinSignalSweatColdMax, 2.4);

        // EvapHeatLossDrySweat = SWEAT WHEN SkinWetTot < 0.4.
        EvapHeatLossDrySweat =
            ((WeighFac * CoreSignalSweatMax + 0.1 * WeighFac * SkinSignalSweatMax) * std::exp(SkinSignalSweatMax / 8.5)) / SweatCtrlFac;

        // MAXIMUM EVAPORATIVE POWER, EvapHeatLossMax, IN W/M**2.
        state.dataThermalComforts->SkinVapPress = CalcSatVapPressFromTemp(state.dataThermalComforts->SkinTemp);
        state.dataThermalComforts->EvapHeatLossMax = 2.2 * state.dataThermalComforts->Hc * (state.dataThermalComforts->SkinVapPress - state.dataThermalComforts->VapPress) * state.dataThermalComforts->CloPermeatEff;
        if (state.dataThermalComforts->EvapHeatLossMax > 0.0) {
            state.dataThermalComforts->SkinWetSweat = EvapHeatLossDrySweat / state.dataThermalComforts->EvapHeatLossMax;
            state.dataThermalComforts->EvapHeatLossDiff = 0.408 * (state.dataThermalComforts->SkinVapPress - state.dataThermalComforts->VapPress);
            state.dataThermalComforts->EvapHeatLoss = state.dataThermalComforts->SkinWetSweat * state.dataThermalComforts->EvapHeatLossMax + (1.0 - state.dataThermalComforts->SkinWetSweat) * state.dataThermalComforts->EvapHeatLossDiff;
            state.dataThermalComforts->SkinWetTot = state.dataThermalComforts->EvapHeatLoss / state.dataThermalComforts->EvapHeatLossMax;
            if (state.dataThermalComforts->Time == 0.0) {
                state.dataThermalComforts->EvapHeatLossSweat = EvapHeatLossDrySweat;
                state.dataThermalComforts->EvapHeatLossSweatPrev = EvapHeatLossDrySweat;
            }
            if (state.dataThermalComforts->SkinWetTot > 0.4) {

                // ITERATION  FOR SWEAT WHEN SkinWetTot IS GREATER THAT 0.4.
                state.dataThermalComforts->IterNum = 0;
                if (state.dataThermalComforts->SkinWetSweat > 1.0) state.dataThermalComforts->SkinWetSweat = 1.0;
                while (true) {
                    EvapHeatLossSweatEst = state.dataThermalComforts->EvapHeatLossSweatPrev;
                    state.dataThermalComforts->SkinWetSweat = EvapHeatLossSweatEst / state.dataThermalComforts->EvapHeatLossMax;

                    if (state.dataThermalComforts->SkinWetSweat > 1.0) state.dataThermalComforts->SkinWetSweat = 1.0;

                    state.dataThermalComforts->EvapHeatLossDiff = 0.408 * (state.dataThermalComforts->SkinVapPress - state.dataThermalComforts->VapPress);
                    state.dataThermalComforts->EvapHeatLoss = (1.0 - state.dataThermalComforts->SkinWetTot) * state.dataThermalComforts->EvapHeatLossDiff + state.dataThermalComforts->EvapHeatLossSweat;
                    state.dataThermalComforts->SkinWetTot = state.dataThermalComforts->EvapHeatLoss / state.dataThermalComforts->EvapHeatLossMax;

                    if (state.dataThermalComforts->SkinWetTot > 1.0) state.dataThermalComforts->SkinWetTot = 1.0;

                    SkinWetSignal = max(0.0, state.dataThermalComforts->SkinWetTot - 0.4);
                    SweatSuppFac = 0.5 + 0.5 * std::exp(-5.6 * SkinWetSignal);
                    EvapHeatLossSweatEstNew = SweatSuppFac * EvapHeatLossDrySweat;

                    if (state.dataThermalComforts->IterNum == 0) state.dataThermalComforts->EvapHeatLossSweat = EvapHeatLossSweatEstNew;

                    Err = EvapHeatLossSweatEst - EvapHeatLossSweatEstNew;

                    if (state.dataThermalComforts->IterNum != 0) {
                        if ((ErrPrev * Err) < 0.0) state.dataThermalComforts->EvapHeatLossSweat = (EvapHeatLossSweatEst + EvapHeatLossSweatEstNew) / 2.0;
                        if ((ErrPrev * Err) >= 0.0) state.dataThermalComforts->EvapHeatLossSweat = EvapHeatLossSweatEstNew;
                    }

                    // STOP CRITERION FOR THE ITERATION.
                    if ((std::abs(Err) <= 0.5) || (state.dataThermalComforts->IterNum >= 10)) break;
                    ++state.dataThermalComforts->IterNum;
                    state.dataThermalComforts->EvapHeatLossSweatPrev = state.dataThermalComforts->EvapHeatLossSweat;
                    ErrPrev = Err;
                }

            } else {
                state.dataThermalComforts->EvapHeatLossSweat = EvapHeatLossDrySweat;
            }

        } else {
            state.dataThermalComforts->SkinWetSweat = 1.0;
            state.dataThermalComforts->SkinWetTot = 1.0;
            state.dataThermalComforts->EvapHeatLossSweat = 0.5 * EvapHeatLossDrySweat;
            state.dataThermalComforts->EvapHeatLoss = state.dataThermalComforts->EvapHeatLossSweat;
        }

        // OVERALL SKIN CONDUCTANCE, KS, IN W/M**2/C.
        // SkinCndctDilation = EFFECT DUE TO VASODILATION.
        // SkinCndctConstriction = EFFECT DUE TO VASOCONSTRICTION.
        SkinCndctDilation = 42.45 * CoreSignalWarmMax + 8.15 * std::pow(CoreSignalSkinSens, 0.8) * SkinSignalWarmMax;
        SkinCndctConstriction = 1.0 + 0.4 * SkinSignalColdMax;
        // ThermCndct IS EQUIVALENT TO KS
        state.dataThermalComforts->ThermCndct = 5.3 + (6.75 + SkinCndctDilation) / SkinCndctConstriction;
        SkinCndctMax = 75.0 + 10.0 * state.dataThermalComforts->AcclPattern;
        if (state.dataThermalComforts->ThermCndct > SkinCndctMax) state.dataThermalComforts->ThermCndct = SkinCndctMax;

        // PASSIVE ENERGY BALANCE EQUATIONS.
        // TOTAL METABOLIC HEAT PRODUCTION RATE, ActLevel, IN W/M**2.
        ActLevelTot = state.dataThermalComforts->ActLevel + state.dataThermalComforts->ShivResponse;
        IntHeatProdTot = ActLevelTot - state.dataThermalComforts->WorkEff;
        // RESPIRATION HEAT LOSS, RespHeatLoss, IN W/M**0.
        state.dataThermalComforts->LatRespHeatLoss = 0.0023 * ActLevelTot * (44.0 - state.dataThermalComforts->VapPress);
        state.dataThermalComforts->DryRespHeatLoss = 0.0014 * ActLevelTot * (34.0 - state.dataThermalComforts->AirTemp);
        state.dataThermalComforts->RespHeatLoss = state.dataThermalComforts->LatRespHeatLoss + state.dataThermalComforts->DryRespHeatLoss;
        // HEAT FLOW FROM CORE TO SKIN, HeatFlow, IN W/M**2.
        state.dataThermalComforts->HeatFlow = state.dataThermalComforts->ThermCndct * (state.dataThermalComforts->CoreTemp - state.dataThermalComforts->SkinTemp);
        // TempChange(1) = CoreTempChange/TempChange, IN C/HR.
        TempChange(1) = (IntHeatProdTot - state.dataThermalComforts->RespHeatLoss - state.dataThermalComforts->HeatFlow) / state.dataThermalComforts->CoreThermCap;
        if (state.dataThermalComforts->EvapHeatLoss > state.dataThermalComforts->EvapHeatLossMax) state.dataThermalComforts->EvapHeatLoss = state.dataThermalComforts->EvapHeatLossMax;

        // DRY HEAT EXCHANGE BY RADIATION & CONVECTION, R+C, IN W/M**2.
        state.dataThermalComforts->DryHeatLoss = state.dataThermalComforts->H * state.dataThermalComforts->CloBodyRat * state.dataThermalComforts->CloThermEff * (state.dataThermalComforts->SkinTemp - state.dataThermalComforts->OpTemp);
        // TempChange(2) = SkinTempChange/TempChange, IN C/HR.
        TempChange(2) = (state.dataThermalComforts->HeatFlow - state.dataThermalComforts->EvapHeatLoss - state.dataThermalComforts->DryHeatLoss) / state.dataThermalComforts->SkinThermCap;
    }

    void RKG(EnergyPlusData &state, int &NEQ, Real64 &H, Real64 &X, Array1D<Real64> &Y, Array1D<Real64> &DY, Array1D<Real64> &C)
    {

        // SUBROUTINE INFORMATION:
        //     AUTHOR         Jaewook Lee
        //     DATE WRITTEN   January 2000
        //     MODIFIED       Rick Strand (for E+ implementation February 2000)

        // PURPOSE OF THIS SUBROUTINE:
        // This is a subroutine for integration by Runga-Kutta's method.

        // METHODOLOGY EMPLOYED:
        // This subroutine is based heavily upon the work performed by Dan Maloney for
        // the BLAST program.  Many of the equations are based on the original Pierce
        // development.  See documentation for further details and references.

        // REFERENCES:
        // Maloney, Dan, M.S. Thesis, University of Illinois at Urbana-Champaign

        // Argument array dimensioning
        EP_SIZE_CHECK(Y, NEQ);
        EP_SIZE_CHECK(DY, NEQ);
        EP_SIZE_CHECK(C, NEQ);

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int I;
        int J;
        Real64 B;
        Real64 H2;
        static Array1D<Real64> const A(2, {0.29289321881345, 1.70710678118654});

        H2 = 0.5 * H;

        DERIV(state, NEQ, Y, DY);
        for (I = 1; I <= NEQ; ++I) {
            B = H2 * DY(I) - C(I);
            Y(I) += B;
            C(I) += 3.0 * B - H2 * DY(I);
        }

        X += H2;

        for (J = 1; J <= 2; ++J) {
            DERIV(state, NEQ, Y, DY);
            for (I = 1; I <= NEQ; ++I) {
                B = A(J) * (H * DY(I) - C(I));
                Y(I) += B;
                C(I) += 3.0 * B - A(J) * H * DY(I);
            }
        }

        X += H2;
        DERIV(state, NEQ, Y, DY);

        for (I = 1; I <= NEQ; ++I) {
            B = (H * DY(I) - 2.0 * C(I)) / 6.0;
            Y(I) += B;
            C(I) += 3.0 * B - H2 * DY(I);
        }

        DERIV(state, NEQ, Y, DY);
    }

    void GetAngleFactorList(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //     AUTHOR         Jaewook Lee
        //     DATE WRITTEN   July 2001

        // Using/Aliasing
        using namespace DataGlobals;
        using namespace DataHeatBalance;
        using DataSurfaces::Surface;
        using namespace DataIPShortCuts;
        using General::RoundSigDigits;

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const AngleFacLimit(0.01); // To set the limit of sum of angle factors
        int const MaxSurfaces(20);        // Maximum number of surfaces in each AngleFactor List

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 AllAngleFacSummed;       // Sum of angle factors in each zone
        static bool ErrorsFound(false); // Set to true if errors in input, fatal at end of routine
        int IOStatus;
        int Item;                  // Item to be "gotten"
        int NumAlphas;             // Number of Alphas from InputProcessor
        int NumNumbers;            // Number of Numbers from Input Processor
        int NumOfAngleFactorLists; // Number of Angle Factor Lists found in IDF
        int SurfNum;               // Surface number DO loop counter
        int WhichAFList;           // Used in validating AngleFactorList

        cCurrentModuleObject = "ComfortViewFactorAngles";
        NumOfAngleFactorLists = inputProcessor->getNumObjectsFound(cCurrentModuleObject);
        state.dataThermalComforts->AngleFactorList.allocate(NumOfAngleFactorLists);
        for (auto &e : state.dataThermalComforts->AngleFactorList) {
            e.Name.clear();
            e.ZoneName.clear();
            e.ZonePtr = 0;
        }

        for (Item = 1; Item <= NumOfAngleFactorLists; ++Item) {

            AllAngleFacSummed = 0.0;
            auto &thisAngFacList(state.dataThermalComforts->AngleFactorList(Item));

            inputProcessor->getObjectItem(state,
                                          cCurrentModuleObject,
                                          Item,
                                          cAlphaArgs,
                                          NumAlphas,
                                          rNumericArgs,
                                          NumNumbers,
                                          IOStatus,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);

            thisAngFacList.Name = cAlphaArgs(1); // no need for verification/uniqueness.
            thisAngFacList.ZoneName = cAlphaArgs(2);
            thisAngFacList.ZonePtr = UtilityRoutines::FindItemInList(cAlphaArgs(2), Zone);
            if (thisAngFacList.ZonePtr == 0) {
                ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid - not found");
                ShowContinueError("...invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) + "\".");
                ErrorsFound = true;
            }

            thisAngFacList.TotAngleFacSurfaces = NumNumbers;
            if (thisAngFacList.TotAngleFacSurfaces > MaxSurfaces) {
                ShowSevereError(cCurrentModuleObject + ": Too many surfaces specified in " + cAlphaFieldNames(1) + '=' + cAlphaArgs(1));
                ErrorsFound = true;
            }

            thisAngFacList.SurfaceName.allocate(thisAngFacList.TotAngleFacSurfaces);
            thisAngFacList.SurfacePtr.allocate(thisAngFacList.TotAngleFacSurfaces);
            thisAngFacList.AngleFactor.allocate(thisAngFacList.TotAngleFacSurfaces);

            for (SurfNum = 1; SurfNum <= thisAngFacList.TotAngleFacSurfaces; ++SurfNum) {
                thisAngFacList.SurfaceName(SurfNum) = cAlphaArgs(SurfNum + 2);
                thisAngFacList.SurfacePtr(SurfNum) = UtilityRoutines::FindItemInList(cAlphaArgs(SurfNum + 2), Surface);
                thisAngFacList.AngleFactor(SurfNum) = rNumericArgs(SurfNum);
                // Error trap for surfaces that do not exist or surfaces not in the zone
                if (thisAngFacList.SurfacePtr(SurfNum) == 0) {
                    ShowSevereError(cCurrentModuleObject + ": invalid " + cAlphaFieldNames(SurfNum + 2) +
                                    ", entered value=" + cAlphaArgs(SurfNum + 2));
                    ShowContinueError("ref " + cAlphaFieldNames(1) + '=' + cAlphaArgs(1) + " not found in " + cAlphaFieldNames(2) + '=' +
                                      cAlphaArgs(2));
                    ErrorsFound = true;
                } else if (thisAngFacList.ZonePtr != 0) { // don't look at invalid zones
                    // Found Surface, is it in same zone tagged for Angle Factor List?
                    if (thisAngFacList.ZonePtr != Surface(thisAngFacList.SurfacePtr(SurfNum)).Zone) {
                        ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid - mismatch " + cAlphaFieldNames(2) + "=\"" +
                                        cAlphaArgs(2) + "\"");
                        ShowContinueError("... does not match " + cAlphaFieldNames(2) + "=\"" +
                                          Zone(Surface(state.dataThermalComforts->AngleFactorList(Item).SurfacePtr(SurfNum)).Zone).Name + "\" for " +
                                          cAlphaFieldNames(SurfNum + 2) + "=\"" + cAlphaArgs(SurfNum + 2) + "\".");
                        ErrorsFound = true;
                    }
                }

                AllAngleFacSummed += thisAngFacList.AngleFactor(SurfNum);
            }

            if (std::abs(AllAngleFacSummed - 1.0) > AngleFacLimit) {
                ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid - Sum[AngleFactors]");
                ShowContinueError("...Sum of Angle Factors [" + RoundSigDigits(AllAngleFacSummed, 3) +
                                  "] exceed expected sum [1.0] by more than limit [" + RoundSigDigits(AngleFacLimit, 3) + "].");
                ErrorsFound = true;
            }
        }

        if (ErrorsFound) {
            ShowFatalError("GetAngleFactorList: Program terminated due to preceding errors.");
        }

        for (Item = 1; Item <= TotPeople; ++Item) {
            if (People(Item).MRTCalcType != AngleFactor) continue;
            People(Item).AngleFactorListPtr = UtilityRoutines::FindItemInList(People(Item).AngleFactorListName, state.dataThermalComforts->AngleFactorList);
            WhichAFList = People(Item).AngleFactorListPtr;
            if (WhichAFList == 0 && (People(Item).Fanger || People(Item).Pierce || People(Item).KSU)) {
                ShowSevereError(cCurrentModuleObject + "=\"" + People(Item).AngleFactorListName + "\", invalid");
                ShowSevereError("... Angle Factor List Name not found for PEOPLE= " + People(Item).Name);
                ErrorsFound = true;
            } else if (People(Item).ZonePtr != state.dataThermalComforts->AngleFactorList(WhichAFList).ZonePtr &&
                       (People(Item).Fanger || People(Item).Pierce || People(Item).KSU)) {
                ShowSevereError(cCurrentModuleObject + "=\"" + state.dataThermalComforts->AngleFactorList(WhichAFList).Name + " mismatch Zone Name");
                ShowContinueError("...Zone=\"" + state.dataThermalComforts->AngleFactorList(WhichAFList).ZoneName + " does not match Zone=\"" + Zone(People(Item).ZonePtr).Name +
                                  "\" in PEOPLE=\"" + People(Item).Name + "\".");
                ErrorsFound = true;
            }
        }

        if (ErrorsFound) {
            ShowFatalError("GetAngleFactorList: Program terminated due to preceding errors.");
        }
    }

    Real64 CalcAngleFactorMRT(EnergyPlusData &state, int const AngleFacNum)
    {

        // SUBROUTINE INFORMATION:
        //     AUTHOR         Jaewook Lee
        //     DATE WRITTEN   July 2001
        //     MODIFIED       November 2017 (R Strand): Added fourth power and emissivity to calculation

        // Using/Aliasing
        using DataHeatBalSurface::TH;
        using DataSurfaces::Surface;

        // Return value
        Real64 CalcAngleFactorMRT;

        // Locals
        Real64 SurfaceTemp;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int SurfNum;
        Real64 SurfTempEmissAngleFacSummed;
        Real64 SumSurfaceEmissAngleFactor;
        Real64 SurfEAF;

        SurfTempEmissAngleFacSummed = 0.0;
        SumSurfaceEmissAngleFactor = 0.0;
        SurfEAF = 0.0;

        auto &thisAngFacList(state.dataThermalComforts->AngleFactorList(AngleFacNum));

        for (SurfNum = 1; SurfNum <= thisAngFacList.TotAngleFacSurfaces; ++SurfNum) {
            SurfaceTemp = TH(2, 1, thisAngFacList.SurfacePtr(SurfNum)) + DataGlobalConstants::KelvinConv();
            SurfEAF = state.dataConstruction->Construct(Surface(thisAngFacList.SurfacePtr(SurfNum)).Construction).InsideAbsorpThermal * thisAngFacList.AngleFactor(SurfNum);
            SurfTempEmissAngleFacSummed += SurfEAF * pow_4(SurfaceTemp);
            SumSurfaceEmissAngleFactor += SurfEAF;
        }

        CalcAngleFactorMRT = root_4(SurfTempEmissAngleFacSummed / SumSurfaceEmissAngleFactor) - DataGlobalConstants::KelvinConv();

        return CalcAngleFactorMRT;
    }

    Real64 CalcSurfaceWeightedMRT(EnergyPlusData &state, int const ZoneNum, int const SurfNum)
    {

        // Purpose: Calculate a modified zone MRT that excludes the Surface( SurfNum ).
        //          This is necessary for the surface weighted option to not in essence
        //          double count SurfNum in the MRT calculation.  Other than that, the
        //          method here is the same as CalculateZoneMRT.  Once a modified zone
        //          MRT is calculated, the subroutine then calculates and returns the
        //          RadTemp (radiant temperature) for use by the thermal comfort routines
        //          that is the average of the surface temperature to be weighted and
        //          the modified zone MRT.

        // Using/Aliasing
        using DataHeatBalSurface::TH;
        using DataSurfaces::Surface;
        using DataSurfaces::TotSurfaces;

        // Return value
        Real64 CalcSurfaceWeightedMRT = 0.0;

        // Local variables
        int SurfNum2;                     // surface number used in "for" loop
        int ZoneNum2;                     // zone number index
        Real64 SumAET;                    // Intermediate calculational variable (area*emissivity*T) sum
        static Array1D<Real64> SurfaceAE; // Product of area and emissivity for each surface
        static Array1D<Real64> ZoneAESum; // Sum of area times emissivity for all zone surfaces
        static bool FirstTimeError;       // Only report the error message one time

        // Initialize ZoneAESum for all zones and SurfaceAE for all surfaces at the start of the simulation
        if (state.dataThermalComforts->FirstTimeSurfaceWeightedFlag) {
            FirstTimeError = true;
            state.dataThermalComforts->FirstTimeSurfaceWeightedFlag = false;
            SurfaceAE.allocate(TotSurfaces);
            ZoneAESum.allocate(NumOfZones);
            SurfaceAE = 0.0;
            ZoneAESum = 0.0;
            for (SurfNum2 = 1; SurfNum2 <= TotSurfaces; ++SurfNum2) {
                if (Surface(SurfNum2).HeatTransSurf) {
                    SurfaceAE(SurfNum2) = Surface(SurfNum2).Area * state.dataConstruction->Construct(Surface(SurfNum2).Construction).InsideAbsorpThermal;
                    ZoneNum2 = Surface(SurfNum2).Zone;
                    // Do NOT include the contribution of the Surface that is being surface weighted in this calculation since it will already be
                    // accounted for
                    if ((ZoneNum2 > 0) && (SurfNum2 != SurfNum)) ZoneAESum(ZoneNum2) += SurfaceAE(SurfNum2);
                }
            }
        }

        // Calculate the sum of area*emissivity and area*emissivity*temperature for all surfaces in the zone EXCEPT the surface being weighted
        // Note that area*emissivity needs to be recalculated because of the possibility of changes to the emissivity via the EMS
        SumAET = 0.0;
        ZoneAESum(ZoneNum) = 0.0;
        for (SurfNum2 = Zone(ZoneNum).SurfaceFirst; SurfNum2 <= Zone(ZoneNum).SurfaceLast; ++SurfNum2) {
            if ((Surface(SurfNum2).HeatTransSurf) && (SurfNum2 != SurfNum)) {
                SurfaceAE(SurfNum2) = Surface(SurfNum2).Area * state.dataConstruction->Construct(Surface(SurfNum2).Construction).InsideAbsorpThermal;
                SumAET += SurfaceAE(SurfNum2) * TH(2, 1, SurfNum2);
                ZoneAESum(ZoneNum) += SurfaceAE(SurfNum2);
            }
        }

        // Now weight the MRT--half comes from the surface used for weighting (SurfNum) and the rest from the adjusted MRT that excludes this surface
        if (ZoneAESum(ZoneNum) > 0.01) {
            CalcSurfaceWeightedMRT = 0.5 * (TH(2, 1, SurfNum) + (SumAET / ZoneAESum(ZoneNum)));
        } else {
            if (FirstTimeError) {
                ShowWarningError("Zone areas*inside surface emissivities are summing to zero, for Zone=\"" + Zone(ZoneNum).Name + "\"");
                ShowContinueError("As a result, MAT will be used for MRT when calculating a surface weighted MRT for this zone.");
                FirstTimeError = false;
                CalcSurfaceWeightedMRT = 0.5 * (TH(2, 1, SurfNum) + MAT(ZoneNum));
            }
        }

        return CalcSurfaceWeightedMRT;
    }

    Real64 CalcSatVapPressFromTemp(Real64 const Temp)
    {

        // FUNCTION INFORMATION:
        //     AUTHOR         Jaewook Lee
        //     DATE WRITTEN   January 2000
        //     MODIFIED       Rick Strand (for E+ implementation February 2000)

        // PURPOSE OF THIS FUNCTION:
        // THIS IS A FUNCTION TO CALCULATE THE SATURATED VAPOR PRESSURE
        // FROM AIR TEMPERATURE

        // METHODOLOGY EMPLOYED:
        // This function is based upon the work performed by Dan Maloney for
        // the BLAST program.
        // REFERENCES:
        // Maloney, Dan, M.S. Thesis, University of Illinois at Urbana-Champaign

        Real64 const XT(Temp / 100.0);
        return 6.16796 + 358.1855 * pow_2(XT) - 550.3543 * pow_3(XT) + 1048.8115 * pow_4(XT);
    }

    Real64 CalcRadTemp(EnergyPlusData &state, int const PeopleListNum) // Type of MRT calculation (zone averaged or surface weighted)
    {

        // FUNCTION INFORMATION:
        //     AUTHOR         Jaewook Lee
        //     DATE WRITTEN   November 2000
        //     MODIFIED       Rick Strand (for E+ implementation November 2000)
        //                    Rick Strand (for high temperature radiant heaters March 2001)

        // PURPOSE OF THIS FUNCTION:
        // THIS IS A FUNCTION TO CALCULATE EITHER ZONE AVERAGED MRT OR
        // SURFACE WEIGHTED MRT

        // METHODOLOGY EMPLOYED:
        // The method here is fairly straight-forward.  If the user has selected
        // a zone average MRT calculation, then there is nothing to do other than
        // to assign the function value because the zone MRT has already been
        // calculated.  Note that this value is an "area-emissivity" weighted value.
        // If the user wants to place the occupant "near" a particular surface,
        // then at the limit half of the radiant field will be from this surface.
        // As a result, an average of the zone MRT and the surface temperature
        // is taken to arrive at an approximate radiant temperature.
        // If a high temperature radiant heater is present, then this must also be
        // taken into account.  The equation used to account for this factor is
        // based on equation 49 on page 150 of Fanger's text (see reference below).
        // The additional assumptions for EnergyPlus are that the radiant energy
        // from the heater must be spread over the average area of a human being
        // (see parameter below) and that the emissivity and absorptivity of the
        // occupant are equivalent for the dominant wavelength of radiant energy
        // from the heater.  These assumptions might be off slightly, but it does
        // allow for an approximation of the effects of surfaces and heaters
        // within a space.  Future additions might include the effect of direct
        // solar energy on occupants.

        // Using/Aliasing
        using DataHeatBalFanSys::QCoolingPanelToPerson;
        using DataHeatBalFanSys::QElecBaseboardToPerson;
        using DataHeatBalFanSys::QHTRadSysToPerson;
        using DataHeatBalFanSys::QHWBaseboardToPerson;
        using DataHeatBalFanSys::QSteamBaseboardToPerson;
        using DataHeatBalSurface::TH;

        // Return value
        Real64 CalcRadTemp;

        // Locals
        Real64 SurfaceTemp;

        // FUNCTION PARAMETER DEFINITIONS:
        Real64 const AreaEff(1.8);                    // Effective area of a "standard" person in meters squared
        Real64 const StefanBoltzmannConst(5.6697e-8); // Stefan-Boltzmann constant in W/(m2*K4)

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        Real64 ZoneRadTemp;

        {
            auto const SELECT_CASE_var(People(PeopleListNum).MRTCalcType);

            if (SELECT_CASE_var == ZoneAveraged) {
                state.dataThermalComforts->RadTemp = MRT(state.dataThermalComforts->ZoneNum);
            } else if (SELECT_CASE_var == SurfaceWeighted) {
                ZoneRadTemp = MRT(state.dataThermalComforts->ZoneNum);
                SurfaceTemp = TH(2, 1, People(PeopleListNum).SurfacePtr);
                state.dataThermalComforts->RadTemp = CalcSurfaceWeightedMRT(state, state.dataThermalComforts->ZoneNum, People(PeopleListNum).SurfacePtr);
            } else if (SELECT_CASE_var == AngleFactor) {
                state.dataThermalComforts->RadTemp = CalcAngleFactorMRT(state, People(PeopleListNum).AngleFactorListPtr);
            }
        }

        // If high temperature radiant heater present and on, then must account for this in MRT calculation
        if (QHTRadSysToPerson(state.dataThermalComforts->ZoneNum) > 0.0 || QCoolingPanelToPerson(state.dataThermalComforts->ZoneNum) > 0.0 || QHWBaseboardToPerson(state.dataThermalComforts->ZoneNum) > 0.0 ||
            QSteamBaseboardToPerson(state.dataThermalComforts->ZoneNum) > 0.0 || QElecBaseboardToPerson(state.dataThermalComforts->ZoneNum) > 0.0) {
            state.dataThermalComforts->RadTemp += DataGlobalConstants::KelvinConv(); // Convert to Kelvin
            state.dataThermalComforts->RadTemp = root_4(pow_4(state.dataThermalComforts->RadTemp) + ((QHTRadSysToPerson(state.dataThermalComforts->ZoneNum) + QCoolingPanelToPerson(state.dataThermalComforts->ZoneNum) + QHWBaseboardToPerson(state.dataThermalComforts->ZoneNum) +
                                                QSteamBaseboardToPerson(state.dataThermalComforts->ZoneNum) + QElecBaseboardToPerson(state.dataThermalComforts->ZoneNum)) /
                                               AreaEff / StefanBoltzmannConst));
            state.dataThermalComforts->RadTemp -= DataGlobalConstants::KelvinConv(); // Convert back to Celsius
        }

        CalcRadTemp = state.dataThermalComforts->RadTemp;

        return CalcRadTemp;
    }

    void CalcThermalComfortSimpleASH55(EnergyPlusData &state)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Jason Glazer
        //       DATE WRITTEN   June 2005

        // PURPOSE OF THIS SUBROUTINE:
        //   Determines if the space is within the ASHRAE 55-2004 comfort region
        //   based on operative temperature and humidity ratio

        // Using/Aliasing
        using DataEnvironment::EnvironmentName;
        using DataEnvironment::EnvironmentStartEnd;
        using DataEnvironment::RunPeriodEnvironment;
        using General::RoundSigDigits;
        using OutputReportTabular::isInQuadrilateral;
        using namespace OutputReportPredefined;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 OperTemp;
        Real64 HumidRatio;
        Real64 CurAirTemp;
        Real64 CurMeanRadiantTemp;
        Real64 NumberOccupants;
        bool isComfortableWithSummerClothes;
        bool isComfortableWithWinterClothes;
        int iPeople;
        int iZone;
        Real64 allowedHours;
        bool showWarning;

        state.dataThermalComforts->AnyZoneTimeNotSimpleASH55Summer = 0.0;
        state.dataThermalComforts->AnyZoneTimeNotSimpleASH55Winter = 0.0;
        state.dataThermalComforts->AnyZoneTimeNotSimpleASH55Either = 0.0;

        // assume the zone is unoccupied
        for (auto &e : state.dataThermalComforts->ThermalComfortInASH55)
            e.ZoneIsOccupied = false;
        // loop through the people objects and determine if the zone is currently occupied
        for (iPeople = 1; iPeople <= TotPeople; ++iPeople) {
            state.dataThermalComforts->ZoneNum = People(iPeople).ZonePtr;
            NumberOccupants = People(iPeople).NumberOfPeople * GetCurrentScheduleValue(People(iPeople).NumberOfPeoplePtr);
            if (NumberOccupants > 0) {
                state.dataThermalComforts->ThermalComfortInASH55(state.dataThermalComforts->ZoneNum).ZoneIsOccupied = true;
            }
        }
        // loop through the zones and determine if in simple ashrae 55 comfort regions
        for (iZone = 1; iZone <= NumOfZones; ++iZone) {
            if (state.dataThermalComforts->ThermalComfortInASH55(iZone).ZoneIsOccupied) {
                // keep track of occupied hours
                state.dataThermalComforts->ZoneOccHrs(iZone) += TimeStepZone;
                if (IsZoneDV(iZone) || IsZoneUI(iZone)) {
                    CurAirTemp = TCMF(iZone);
                } else {
                    CurAirTemp = ZTAVComf(iZone);
                }
                CurMeanRadiantTemp = MRT(iZone);
                OperTemp = CurAirTemp * 0.5 + CurMeanRadiantTemp * 0.5;
                HumidRatio = ZoneAirHumRatAvgComf(iZone);
                // for debugging
                // ThermalComfortInASH55(iZone)%dCurAirTemp = CurAirTemp
                // ThermalComfortInASH55(iZone)%dCurMeanRadiantTemp = CurMeanRadiantTemp
                // ThermalComfortInASH55(iZone)%dOperTemp = OperTemp
                // ThermalComfortInASH55(iZone)%dHumidRatio = HumidRatio
                // From ASHRAE Standard 55-2004 Appendix D
                //  Run    AirTemp(C)   RH(%)  Season  HumidRatio
                //   1       19.6        86    Winter    0.012
                //   2       23.9        66    Winter    0.012
                //   3       25.7        15    Winter    0.003
                //   4       21.2        20    Winter    0.003
                //   5       23.6        67    Summer    0.012
                //   6       26.8        56    Summer    0.012
                //   7       27.9        13    Summer    0.003
                //   8       24.7        16    Summer    0.003
                // But the standard says "no recommended lower humidity limit" so it should
                // really extend down to the 0.0 Humidity ratio line.  Extrapolating we get
                // the values that are shown in the following table
                //  Run    AirTemp(C)    Season  HumidRatio
                //   1       19.6        Winter    0.012
                //   2       23.9        Winter    0.012
                //   3       26.3        Winter    0.000
                //   4       21.7        Winter    0.000
                //   5       23.6        Summer    0.012
                //   6       26.8        Summer    0.012
                //   7       28.3        Summer    0.000
                //   8       25.1        Summer    0.000
                // check summer clothing conditions
                isComfortableWithSummerClothes = isInQuadrilateral(OperTemp, HumidRatio, 25.1, 0.0, 23.6, 0.012, 26.8, 0.012, 28.3, 0.0);
                // check winter clothing conditions
                isComfortableWithWinterClothes = isInQuadrilateral(OperTemp, HumidRatio, 21.7, 0.0, 19.6, 0.012, 23.9, 0.012, 26.3, 0.0);
                if (isComfortableWithSummerClothes) {
                    state.dataThermalComforts->ThermalComfortInASH55(iZone).timeNotSummer = 0.0;
                } else {
                    state.dataThermalComforts->ThermalComfortInASH55(iZone).timeNotSummer = TimeStepZone;
                    state.dataThermalComforts->ThermalComfortInASH55(iZone).totalTimeNotSummer += TimeStepZone;
                    state.dataThermalComforts->AnyZoneTimeNotSimpleASH55Summer = TimeStepZone;
                }
                if (isComfortableWithWinterClothes) {
                    state.dataThermalComforts->ThermalComfortInASH55(iZone).timeNotWinter = 0.0;
                } else {
                    state.dataThermalComforts->ThermalComfortInASH55(iZone).timeNotWinter = TimeStepZone;
                    state.dataThermalComforts->ThermalComfortInASH55(iZone).totalTimeNotWinter += TimeStepZone;
                    state.dataThermalComforts->AnyZoneTimeNotSimpleASH55Winter = TimeStepZone;
                }
                if (isComfortableWithSummerClothes || isComfortableWithWinterClothes) {
                    state.dataThermalComforts->ThermalComfortInASH55(iZone).timeNotEither = 0.0;
                } else {
                    state.dataThermalComforts->ThermalComfortInASH55(iZone).timeNotEither = TimeStepZone;
                    state.dataThermalComforts->ThermalComfortInASH55(iZone).totalTimeNotEither += TimeStepZone;
                    state.dataThermalComforts->AnyZoneTimeNotSimpleASH55Either = TimeStepZone;
                }
            } else {
                // when no one present in that portion of the zone then no one can be uncomfortable
                state.dataThermalComforts->ThermalComfortInASH55(iZone).timeNotSummer = 0.0;
                state.dataThermalComforts->ThermalComfortInASH55(iZone).timeNotWinter = 0.0;
                state.dataThermalComforts->ThermalComfortInASH55(iZone).timeNotEither = 0.0;
            }
        }
        // accumulate total time
        state.dataThermalComforts->TotalAnyZoneTimeNotSimpleASH55Summer += state.dataThermalComforts->AnyZoneTimeNotSimpleASH55Summer;
        state.dataThermalComforts->TotalAnyZoneTimeNotSimpleASH55Winter += state.dataThermalComforts->AnyZoneTimeNotSimpleASH55Winter;
        state.dataThermalComforts->TotalAnyZoneTimeNotSimpleASH55Either += state.dataThermalComforts->AnyZoneTimeNotSimpleASH55Either;
        // was EndEnvrnsFlag prior to CR7562
        if (EndDesignDayEnvrnsFlag) {
            allowedHours = double(NumOfDayInEnvrn) * 24.0 * 0.04;
            // first check if warning should be printed
            showWarning = false;
            for (iZone = 1; iZone <= NumOfZones; ++iZone) {
                if (state.dataThermalComforts->ThermalComfortInASH55(iZone).Enable55Warning) {
                    if (state.dataThermalComforts->ThermalComfortInASH55(iZone).totalTimeNotEither > allowedHours) {
                        showWarning = true;
                    }
                }
            }
            // if any zones should be warning print it out
            if (showWarning) {
                ShowWarningError("More than 4% of time (" + RoundSigDigits(allowedHours, 1) + " hours) uncomfortable in one or more zones ");
                ShowContinueError("Based on ASHRAE 55-2004 graph (Section 5.2.1.1)");
                if (RunPeriodEnvironment) {
                    ShowContinueError("During Environment [" + EnvironmentStartEnd + "]: " + EnvironmentName);
                } else {
                    ShowContinueError("During SizingPeriod Environment [" + EnvironmentStartEnd + "]: " + EnvironmentName);
                }
                for (iZone = 1; iZone <= NumOfZones; ++iZone) {
                    if (state.dataThermalComforts->ThermalComfortInASH55(iZone).Enable55Warning) {
                        if (state.dataThermalComforts->ThermalComfortInASH55(iZone).totalTimeNotEither > allowedHours) {
                            ShowContinueError(RoundSigDigits(state.dataThermalComforts->ThermalComfortInASH55(iZone).totalTimeNotEither, 1) +
                                              " hours were uncomfortable in zone: " + Zone(iZone).Name);
                        }
                    }
                }
            }
            // put in predefined reports
            for (iZone = 1; iZone <= NumOfZones; ++iZone) {
                PreDefTableEntry(pdchSCwinterClothes, Zone(iZone).Name, state.dataThermalComforts->ThermalComfortInASH55(iZone).totalTimeNotWinter);
                PreDefTableEntry(pdchSCsummerClothes, Zone(iZone).Name, state.dataThermalComforts->ThermalComfortInASH55(iZone).totalTimeNotSummer);
                PreDefTableEntry(pdchSCeitherClothes, Zone(iZone).Name, state.dataThermalComforts->ThermalComfortInASH55(iZone).totalTimeNotEither);
            }
            PreDefTableEntry(pdchSCwinterClothes, "Facility", state.dataThermalComforts->TotalAnyZoneTimeNotSimpleASH55Winter);
            PreDefTableEntry(pdchSCsummerClothes, "Facility", state.dataThermalComforts->TotalAnyZoneTimeNotSimpleASH55Summer);
            PreDefTableEntry(pdchSCeitherClothes, "Facility", state.dataThermalComforts->TotalAnyZoneTimeNotSimpleASH55Either);
            // set value for ABUPS report
            TotalTimeNotSimpleASH55EitherForABUPS = state.dataThermalComforts->TotalAnyZoneTimeNotSimpleASH55Either;
            // reset accumulation for new environment
            for (iZone = 1; iZone <= NumOfZones; ++iZone) {
                state.dataThermalComforts->ThermalComfortInASH55(iZone).totalTimeNotWinter = 0.0;
                state.dataThermalComforts->ThermalComfortInASH55(iZone).totalTimeNotSummer = 0.0;
                state.dataThermalComforts->ThermalComfortInASH55(iZone).totalTimeNotEither = 0.0;
            }
            state.dataThermalComforts->TotalAnyZoneTimeNotSimpleASH55Winter = 0.0;
            state.dataThermalComforts->TotalAnyZoneTimeNotSimpleASH55Summer = 0.0;
            state.dataThermalComforts->TotalAnyZoneTimeNotSimpleASH55Either = 0.0;
            // report how the aggregation is conducted
            {
                auto const SELECT_CASE_var(state.dataGlobal->KindOfSim);
                if (SELECT_CASE_var == DataGlobalConstants::KindOfSim::DesignDay) {
                    addFootNoteSubTable(pdstSimpleComfort, "Aggregated over the Design Days");
                } else if (SELECT_CASE_var == DataGlobalConstants::KindOfSim::RunPeriodDesign) {
                    addFootNoteSubTable(pdstSimpleComfort, "Aggregated over the RunPeriods for Design");
                } else if (SELECT_CASE_var == DataGlobalConstants::KindOfSim::RunPeriodWeather) {
                    addFootNoteSubTable(pdstSimpleComfort, "Aggregated over the RunPeriods for Weather");
                }
            }
            // report number of occupied hours per week for LEED report
            for (iZone = 1; iZone <= NumOfZones; ++iZone) {
                PreDefTableEntry(pdchLeedSutHrsWeek, Zone(iZone).Name, 7 * 24 * (state.dataThermalComforts->ZoneOccHrs(iZone) / (NumOfDayInEnvrn * 24)));
            }
        }
    }

    void CalcIfSetPointMet(EnergyPlusData &state)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Jason Glazer
        //       DATE WRITTEN   July 2005

        // PURPOSE OF THIS SUBROUTINE:
        //   Report if the setpoint temperature has been met.
        //   Add calculation of how far away from setpoint and if setpoint was not met
        //   during all times and during occupancy.

        // Using/Aliasing
        using DataHeatBalFanSys::TempControlType;
        using DataHeatBalFanSys::TempTstatAir;
        using DataHeatBalFanSys::ZoneThermostatSetPointHi;
        using DataHeatBalFanSys::ZoneThermostatSetPointLo;
        using DataHeatBalFanSys::ZoneThermostatSetPointHiAver;
        using DataHeatBalFanSys::ZoneThermostatSetPointLoAver;
        using DataZoneEnergyDemands::ZoneSysEnergyDemand;
        using namespace OutputReportPredefined;
        using DataHVACGlobals::deviationFromSetPtThresholdClg;
        using DataHVACGlobals::deviationFromSetPtThresholdHtg;
        using DataHVACGlobals::DualSetPointWithDeadBand;
        using DataHVACGlobals::SingleCoolingSetPoint;
        using DataHVACGlobals::SingleHeatCoolSetPoint;
        using DataHVACGlobals::SingleHeatingSetPoint;
        using DataRoomAirModel::AirModel;
        using DataRoomAirModel::RoomAirModel_Mixing;
        //using ZoneTempPredictorCorrector::NumOnOffCtrZone;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 SensibleLoadPredictedNoAdj;
        Real64 deltaT;
        int iZone;
        bool testHeating;
        bool testCooling;

        // Get the load predicted - the sign will indicate if heating or cooling
        // was called for
        state.dataThermalComforts->AnyZoneNotMetHeating = 0.0;
        state.dataThermalComforts->AnyZoneNotMetCooling = 0.0;
        state.dataThermalComforts->AnyZoneNotMetOccupied = 0.0;
        state.dataThermalComforts->AnyZoneNotMetHeatingOccupied = 0.0;
        state.dataThermalComforts->AnyZoneNotMetCoolingOccupied = 0.0;
        for (iZone = 1; iZone <= NumOfZones; ++iZone) {
            SensibleLoadPredictedNoAdj = ZoneSysEnergyDemand(iZone).TotalOutputRequired;
            state.dataThermalComforts->ThermalComfortSetPoint(iZone).notMetCooling = 0.0;
            state.dataThermalComforts->ThermalComfortSetPoint(iZone).notMetHeating = 0.0;
            state.dataThermalComforts->ThermalComfortSetPoint(iZone).notMetCoolingOccupied = 0.0;
            state.dataThermalComforts->ThermalComfortSetPoint(iZone).notMetHeatingOccupied = 0.0;
            {
                auto const SELECT_CASE_var(TempControlType(iZone));
                if (SELECT_CASE_var == SingleHeatingSetPoint) {
                    testHeating = true;
                    testCooling = false;
                } else if (SELECT_CASE_var == SingleCoolingSetPoint) {
                    testHeating = false;
                    testCooling = true;
                } else if (SELECT_CASE_var == SingleHeatCoolSetPoint) {
                    testHeating = true;
                    testCooling = true;
                } else if (SELECT_CASE_var == DualSetPointWithDeadBand) {
                    testHeating = true;
                    testCooling = true;
                } else {
                    testHeating = true;
                    testCooling = true;
                }
            }
            if (testHeating && (SensibleLoadPredictedNoAdj > 0)) { // heating
                if (AirModel(iZone).AirModelType != RoomAirModel_Mixing) {
                    deltaT = TempTstatAir(iZone) - ZoneThermostatSetPointLo(iZone);
                } else {
                    if (state.dataZoneTempPredictorCorrector->NumOnOffCtrZone > 0) {
                        deltaT = ZTAV(iZone) - ZoneThermostatSetPointLoAver(iZone);
                    } else {
                        deltaT = ZTAV(iZone) - ZoneThermostatSetPointLo(iZone);
                    }
                }
                if (deltaT < deviationFromSetPtThresholdHtg) {
                    state.dataThermalComforts->ThermalComfortSetPoint(iZone).notMetHeating = TimeStepZone;
                    state.dataThermalComforts->ThermalComfortSetPoint(iZone).totalNotMetHeating += TimeStepZone;
                    if (state.dataThermalComforts->AnyZoneNotMetHeating == 0.0) state.dataThermalComforts->AnyZoneNotMetHeating = TimeStepZone;
                    if (state.dataThermalComforts->ThermalComfortInASH55(iZone).ZoneIsOccupied) {
                        state.dataThermalComforts->ThermalComfortSetPoint(iZone).notMetHeatingOccupied = TimeStepZone;
                        state.dataThermalComforts->ThermalComfortSetPoint(iZone).totalNotMetHeatingOccupied += TimeStepZone;
                        if (state.dataThermalComforts->AnyZoneNotMetHeatingOccupied == 0.0) state.dataThermalComforts->AnyZoneNotMetHeatingOccupied = TimeStepZone;
                        if (state.dataThermalComforts->AnyZoneNotMetOccupied == 0.0) state.dataThermalComforts->AnyZoneNotMetOccupied = TimeStepZone;
                    }
                }
            } else if (testCooling && (SensibleLoadPredictedNoAdj < 0)) { // cooling
                if (AirModel(iZone).AirModelType != RoomAirModel_Mixing) {
                    deltaT = TempTstatAir(iZone) - ZoneThermostatSetPointHi(iZone);
                } else {
                    if (state.dataZoneTempPredictorCorrector->NumOnOffCtrZone > 0) {
                        deltaT = ZTAV(iZone) - ZoneThermostatSetPointHiAver(iZone);
                    } else {
                        deltaT = ZTAV(iZone) - ZoneThermostatSetPointHi(iZone);
                    }
                }

                if (Zone(iZone).HasAdjustedReturnTempByITE) {
                    deltaT = TempTstatAir(iZone) - Zone(iZone).AdjustedReturnTempByITE;
                }
                if (deltaT > deviationFromSetPtThresholdClg) {
                    state.dataThermalComforts->ThermalComfortSetPoint(iZone).notMetCooling = TimeStepZone;
                    state.dataThermalComforts->ThermalComfortSetPoint(iZone).totalNotMetCooling += TimeStepZone;
                    if (state.dataThermalComforts->AnyZoneNotMetCooling == 0.0) state.dataThermalComforts->AnyZoneNotMetCooling = TimeStepZone;
                    if (state.dataThermalComforts->ThermalComfortInASH55(iZone).ZoneIsOccupied) {
                        state.dataThermalComforts->ThermalComfortSetPoint(iZone).notMetCoolingOccupied = TimeStepZone;
                        state.dataThermalComforts->ThermalComfortSetPoint(iZone).totalNotMetCoolingOccupied += TimeStepZone;
                        if (state.dataThermalComforts->AnyZoneNotMetCoolingOccupied == 0.0) state.dataThermalComforts->AnyZoneNotMetCoolingOccupied = TimeStepZone;
                        if (state.dataThermalComforts->AnyZoneNotMetOccupied == 0.0) state.dataThermalComforts->AnyZoneNotMetOccupied = TimeStepZone;
                    }
                }
            }
        }
        state.dataThermalComforts->TotalAnyZoneNotMetHeating += state.dataThermalComforts->AnyZoneNotMetHeating;
        state.dataThermalComforts->TotalAnyZoneNotMetCooling += state.dataThermalComforts->AnyZoneNotMetCooling;
        state.dataThermalComforts->TotalAnyZoneNotMetHeatingOccupied += state.dataThermalComforts->AnyZoneNotMetHeatingOccupied;
        state.dataThermalComforts->TotalAnyZoneNotMetCoolingOccupied += state.dataThermalComforts->AnyZoneNotMetCoolingOccupied;
        state.dataThermalComforts->TotalAnyZoneNotMetOccupied += state.dataThermalComforts->AnyZoneNotMetOccupied;

        // was EndEnvrnsFlag prior to CR7562
        if (EndDesignDayEnvrnsFlag) {
            for (iZone = 1; iZone <= NumOfZones; ++iZone) {
                PreDefTableEntry(pdchULnotMetHeat, Zone(iZone).Name, state.dataThermalComforts->ThermalComfortSetPoint(iZone).totalNotMetHeating);
                PreDefTableEntry(pdchULnotMetCool, Zone(iZone).Name, state.dataThermalComforts->ThermalComfortSetPoint(iZone).totalNotMetCooling);
                PreDefTableEntry(pdchULnotMetHeatOcc, Zone(iZone).Name, state.dataThermalComforts->ThermalComfortSetPoint(iZone).totalNotMetHeatingOccupied);
                PreDefTableEntry(pdchULnotMetCoolOcc, Zone(iZone).Name, state.dataThermalComforts->ThermalComfortSetPoint(iZone).totalNotMetCoolingOccupied);
            }
            PreDefTableEntry(pdchULnotMetHeat, "Facility", state.dataThermalComforts->TotalAnyZoneNotMetHeating);
            PreDefTableEntry(pdchULnotMetCool, "Facility", state.dataThermalComforts->TotalAnyZoneNotMetCooling);
            PreDefTableEntry(pdchULnotMetHeatOcc, "Facility", state.dataThermalComforts->TotalAnyZoneNotMetHeatingOccupied);
            PreDefTableEntry(pdchULnotMetCoolOcc, "Facility", state.dataThermalComforts->TotalAnyZoneNotMetCoolingOccupied);
            // set value for ABUPS report
            TotalNotMetHeatingOccupiedForABUPS = state.dataThermalComforts->TotalAnyZoneNotMetHeatingOccupied;
            TotalNotMetCoolingOccupiedForABUPS = state.dataThermalComforts->TotalAnyZoneNotMetCoolingOccupied;
            TotalNotMetOccupiedForABUPS = state.dataThermalComforts->TotalAnyZoneNotMetOccupied;
            // reset counters
            for (iZone = 1; iZone <= NumOfZones; ++iZone) {
                state.dataThermalComforts->ThermalComfortSetPoint(iZone).totalNotMetHeating = 0.0;
                state.dataThermalComforts->ThermalComfortSetPoint(iZone).totalNotMetCooling = 0.0;
                state.dataThermalComforts->ThermalComfortSetPoint(iZone).totalNotMetHeatingOccupied = 0.0;
                state.dataThermalComforts->ThermalComfortSetPoint(iZone).totalNotMetCoolingOccupied = 0.0;
            }
            state.dataThermalComforts->TotalAnyZoneNotMetHeating = 0.0;
            state.dataThermalComforts->TotalAnyZoneNotMetCooling = 0.0;
            state.dataThermalComforts->TotalAnyZoneNotMetHeatingOccupied = 0.0;
            state.dataThermalComforts->TotalAnyZoneNotMetCoolingOccupied = 0.0;
            state.dataThermalComforts->TotalAnyZoneNotMetOccupied = 0.0;
            // report how the aggregation is conducted
            {
                auto const SELECT_CASE_var(state.dataGlobal->KindOfSim);
                if (SELECT_CASE_var == DataGlobalConstants::KindOfSim::DesignDay) {
                    addFootNoteSubTable(pdstUnmetLoads, "Aggregated over the Design Days");
                } else if (SELECT_CASE_var == DataGlobalConstants::KindOfSim::RunPeriodDesign) {
                    addFootNoteSubTable(pdstUnmetLoads, "Aggregated over the RunPeriods for Design");
                } else if (SELECT_CASE_var == DataGlobalConstants::KindOfSim::RunPeriodWeather) {
                    addFootNoteSubTable(pdstUnmetLoads, "Aggregated over the RunPeriods for Weather");
                }
            }
        }
    }

    void CalcThermalComfortAdaptiveASH55(
        EnergyPlusData &state,
        bool const initiate,              // true if supposed to initiate
        Optional_bool_const wthrsim,      // true if this is a weather simulation
        Optional<Real64 const> avgdrybulb // approximate avg drybulb for design day.  will be used as previous period in design day
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Tyler Hoyt
        //       DATE WRITTEN   July 2011

        // PURPOSE OF THIS SUBROUTINE:
        // Sets up and carries out ASHRAE55-2010 adaptive comfort model calculations.
        // Output provided are state variables for the 80% and 90% acceptability limits
        // in the model, the comfort temperature, and the 30-day running average or
        // monthly average outdoor air temperature as parsed from the .STAT file.

        // METHODOLOGY EMPLOYED:
        // In order for the calculations to be possible the user must provide either
        // a .STAT file or .EPW file for the purpose of computing a monthly average
        // temperature or thirty-day running average. The subroutine need only open
        // the relevant file once to initialize, and then operates within the loop.

        // Using/Aliasing
        using DataEnvironment::DayOfYear;
        using DataEnvironment::Month;
        using DataEnvironment::OutDryBulbTemp;
        using DataHVACGlobals::SysTimeElapsed;
        using OutputReportTabular::GetColumnUsingTabs;
        using OutputReportTabular::StrToReal;

        // SUBROUTINE PARAMETER DEFINITIONS:

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        std::string lineAvg;
        std::string epwLine;
        static Real64 avgDryBulbASH(0.0);
        Real64 dryBulb;
        static Array1D<Real64> monthlyTemp(12, 0.0);
        Real64 tComf;
        Real64 numOccupants;
        static bool useStatData(false);
        int readStat;
        int jStartDay;
        int calcStartDay;
        int calcStartHr;
        int calcEndDay;
        int calcEndHr;
        std::string::size_type pos;
        int ind;
        int i;
        int j;
        bool weathersimulation;
        Real64 inavgdrybulb;

        if (initiate) { // not optional on initiate=true.  would otherwise check for presence
            weathersimulation = wthrsim;
            avgDryBulbASH = 0.0;
            state.dataThermalComforts->runningAverageASH = 0.0;
            monthlyTemp = 0.0;
            inavgdrybulb = avgdrybulb;
        } else {
            weathersimulation = false;
            inavgdrybulb = 0.0;
        }

        if (initiate && weathersimulation) {
            const bool statFileExists = FileSystem::fileExists(state.files.inStatFileName.fileName);
            const bool epwFileExists = FileSystem::fileExists(state.files.inputWeatherFileName.fileName);

            readStat = 0;
            if (statFileExists) {
                auto statFile = state.files.inStatFileName.open("CalcThermalComfortAdapctiveASH55");
                while (statFile.good()) {
                    auto lineIn = statFile.readLine();
                    if (has(lineIn.data, "Monthly Statistics for Dry Bulb temperatures")) {
                        for (i = 1; i <= 7; ++i) {
                            lineIn = statFile.readLine();
                        }
                        lineIn = statFile.readLine();
                        lineAvg = lineIn.data;
                        break;
                    }
                }
                for (i = 1; i <= 12; ++i) {
                    monthlyTemp(i) = StrToReal(GetColumnUsingTabs(lineAvg, i + 2));
                }
                useStatData = true;
            } else if (epwFileExists) {
                // determine number of days in year
                int DaysInYear;
                if (DataEnvironment::CurrentYearIsLeapYear) {
                    DaysInYear = 366;
                } else {
                    DaysInYear = 365;
                }
                state.dataThermalComforts->DailyAveOutTemp = 0.0;

                auto epwFile = state.files.inputWeatherFileName.open("CalcThermalComfortAdaptiveASH55");
                for (i = 1; i <= 8; ++i) { // Headers
                    epwLine = epwFile.readLine().data;
                }
                jStartDay = DayOfYear - 1;
                calcStartDay = jStartDay - 30;
                if (calcStartDay >= 0) {
                    calcStartHr = 24 * calcStartDay + 1;
                    for (i = 1; i <= calcStartHr - 1; ++i) {
                        epwFile.readLine();
                    }
                    for (i = 1; i <= 30; ++i) {
                        avgDryBulbASH = 0.0;
                        for (j = 1; j <= 24; ++j) {
                            epwLine = epwFile.readLine().data;
                            for (ind = 1; ind <= 6; ++ind) {
                                pos = index(epwLine, ',');
                                epwLine.erase(0, pos + 1);
                            }
                            pos = index(epwLine, ',');
                            dryBulb = StrToReal(epwLine.substr(0, pos));
                            avgDryBulbASH += (dryBulb / 24.0);
                        }
                        state.dataThermalComforts->DailyAveOutTemp(i) = avgDryBulbASH;
                    }
                } else { // Do special things for wrapping the epw
                    calcEndDay = jStartDay;
                    calcStartDay += DaysInYear;
                    calcEndHr = 24 * calcEndDay;
                    calcStartHr = 24 * calcStartDay + 1;
                    for (i = 1; i <= calcEndDay; ++i) {
                        avgDryBulbASH = 0.0;
                        for (j = 1; j <= 24; ++j) {
                            epwLine = epwFile.readLine().data;
                            for (ind = 1; ind <= 6; ++ind) {
                                pos = index(epwLine, ',');
                                epwLine.erase(0, pos + 1);
                            }
                            pos = index(epwLine, ',');
                            dryBulb = StrToReal(epwLine.substr(0, pos));
                            avgDryBulbASH += (dryBulb / 24.0);
                        }
                        state.dataThermalComforts->DailyAveOutTemp(i + 30 - calcEndDay) = avgDryBulbASH;
                    }
                    for (i = calcEndHr + 1; i <= calcStartHr - 1; ++i) {
                        epwLine = epwFile.readLine().data;
                    }
                    for (i = 1; i <= 30 - calcEndDay; ++i) {
                        avgDryBulbASH = 0.0;
                        for (j = 1; j <= 24; ++j) {
                            epwLine = epwFile.readLine().data;
                            for (ind = 1; ind <= 6; ++ind) {
                                pos = index(epwLine, ',');
                                epwLine.erase(0, pos + 1);
                            }
                            pos = index(epwLine, ',');
                            dryBulb = StrToReal(epwLine.substr(0, pos));
                            avgDryBulbASH += (dryBulb / 24.0);
                        }
                        state.dataThermalComforts->DailyAveOutTemp(i) = avgDryBulbASH;
                    }
                }
                state.dataThermalComforts->useEpwData = true;
            }
        } else if (initiate && !weathersimulation) {
            state.dataThermalComforts->runningAverageASH = inavgdrybulb;
            monthlyTemp = inavgdrybulb;
            avgDryBulbASH = 0.0;
        }

        if (initiate) return;

        if (BeginDayFlag && state.dataThermalComforts->useEpwData) {
            // Update the running average, reset the daily avg
            state.dataThermalComforts->DailyAveOutTemp(30) = avgDryBulbASH;
            Real64 sum = 0.0;
            for (i = 1; i <= 29; i++) {
                sum += state.dataThermalComforts->DailyAveOutTemp(i);
            }
            state.dataThermalComforts->runningAverageASH = (sum + avgDryBulbASH) / 30.0;
            for (i = 1; i <= 29; i++) {
                state.dataThermalComforts->DailyAveOutTemp(i) = state.dataThermalComforts->DailyAveOutTemp(i + 1);
            }
            avgDryBulbASH = 0.0;
        }

        // If exists BeginMonthFlag we can use it to call InvJulianDay once per month.
        if (BeginDayFlag && useStatData) {
            //  CALL InvJulianDay(DayOfYear,pMonth,pDay,0)
            //  runningAverageASH = monthlyTemp(pMonth)
            state.dataThermalComforts->runningAverageASH = monthlyTemp(Month);
        }

        // Update the daily average
        // IF (BeginHourFlag .and. useEpwData) THEN
        if (BeginHourFlag) {
            avgDryBulbASH += (OutDryBulbTemp / 24.0);
        }

        for (state.dataThermalComforts->PeopleNum = 1; state.dataThermalComforts->PeopleNum <= TotPeople; ++state.dataThermalComforts->PeopleNum) {
            if (!People(state.dataThermalComforts->PeopleNum).AdaptiveASH55) continue;
            state.dataThermalComforts->ZoneNum = People(state.dataThermalComforts->PeopleNum).ZonePtr;
            if (IsZoneDV(state.dataThermalComforts->ZoneNum) || IsZoneUI(state.dataThermalComforts->ZoneNum)) {
                state.dataThermalComforts->AirTemp = TCMF(state.dataThermalComforts->ZoneNum);
            } else {
                state.dataThermalComforts->AirTemp = ZTAVComf(state.dataThermalComforts->ZoneNum);
            }
            state.dataThermalComforts->RadTemp = CalcRadTemp(state, state.dataThermalComforts->PeopleNum);
            state.dataThermalComforts->OpTemp = (state.dataThermalComforts->AirTemp + state.dataThermalComforts->RadTemp) / 2.0;
            state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).ThermalComfortOpTemp = state.dataThermalComforts->OpTemp;
            state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).ASHRAE55RunningMeanOutdoorTemp = state.dataThermalComforts->runningAverageASH;
            if (state.dataThermalComforts->runningAverageASH >= 10.0 && state.dataThermalComforts->runningAverageASH <= 33.5) {
                // Calculate the comfort here  (people/output handling loop)
                numOccupants = People(state.dataThermalComforts->PeopleNum).NumberOfPeople * GetCurrentScheduleValue(People(state.dataThermalComforts->PeopleNum).NumberOfPeoplePtr);
                tComf = 0.31 * state.dataThermalComforts->runningAverageASH + 17.8;
                state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).TComfASH55 = tComf;
                if (numOccupants > 0) {
                    if (state.dataThermalComforts->OpTemp < tComf + 2.5 && state.dataThermalComforts->OpTemp > tComf - 2.5) {
                        // 80% and 90% limits okay
                        state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).ThermalComfortAdaptiveASH5590 = 1;
                        state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).ThermalComfortAdaptiveASH5580 = 1;
                    } else if (state.dataThermalComforts->OpTemp < tComf + 3.5 && state.dataThermalComforts->OpTemp > tComf - 3.5) {
                        // 80% only
                        state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).ThermalComfortAdaptiveASH5590 = 0;
                        state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).ThermalComfortAdaptiveASH5580 = 1;
                        People(state.dataThermalComforts->PeopleNum).TimeNotMetASH5590 += SysTimeElapsed;
                    } else {
                        // Neither
                        state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).ThermalComfortAdaptiveASH5590 = 0;
                        state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).ThermalComfortAdaptiveASH5580 = 0;
                        People(state.dataThermalComforts->PeopleNum).TimeNotMetASH5580 += SysTimeElapsed;
                        People(state.dataThermalComforts->PeopleNum).TimeNotMetASH5590 += SysTimeElapsed;
                    }
                } else {
                    // Unoccupied
                    state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).ThermalComfortAdaptiveASH5590 = -1;
                    state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).ThermalComfortAdaptiveASH5580 = -1;
                }
            } else {
                // Monthly temp out of range
                state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).ThermalComfortAdaptiveASH5590 = -1;
                state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).ThermalComfortAdaptiveASH5580 = -1;
                state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).TComfASH55 = -1.0;
            }
        }
    }

    void CalcThermalComfortAdaptiveCEN15251(
        EnergyPlusData &state,
        bool const initiate,              // true if supposed to initiate
        Optional_bool_const wthrsim,      // true if this is a weather simulation
        Optional<Real64 const> avgdrybulb // approximate avg drybulb for design day.  will be used as previous period in design day
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Tyler Hoyt
        //       DATE WRITTEN   July 2011

        // PURPOSE OF THIS SUBROUTINE:
        // Sets up and carries out CEN-15251 adaptive comfort model calculations.
        // Output provided are state variables for the Category I, II, and III
        // limits of the model, the comfort temperature, and the 5-day weighted
        // moving average of the outdoor air temperature.

        // Using/Aliasing
        using DataEnvironment::DayOfYear;
        using DataEnvironment::Month;
        using DataEnvironment::OutDryBulbTemp;
        using DataHVACGlobals::SysTimeElapsed;
        using OutputReportTabular::GetColumnUsingTabs;
        using OutputReportTabular::StrToReal;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static Real64 const alpha(0.8);
        static Array1D<Real64> const alpha_pow({pow_6(alpha), pow_5(alpha), pow_4(alpha), pow_3(alpha), pow_2(alpha), alpha, 1.0}); // alpha^(7-i)

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        std::string epwLine;
        static Real64 avgDryBulbCEN(0.0);
        Real64 dryBulb;
        Real64 tComf;
        Real64 tComfLow;
        static Real64 runningAverageCEN(0.0);
        Real64 numOccupants;
        static bool useEpwData(false);
        static bool firstDaySet(false); // first day is set with initiate -- so do not update
        int readStat;
        int jStartDay;
        int calcStartDay;
        int calcStartHr;
        int calcEndDay;
        int calcEndHr;
        std::string::size_type pos;
        int ind;
        int i;
        int j;
        bool weathersimulation;
        Real64 inavgdrybulb;

        if (initiate) { // not optional on initiate=true.  would otherwise check for presence
            weathersimulation = wthrsim;
            inavgdrybulb = avgdrybulb;
            avgDryBulbCEN = 0.0;
            runningAverageCEN = 0.0;
        } else {
            weathersimulation = false;
            inavgdrybulb = 0.0;
        }

        if (initiate && weathersimulation) {
            const bool epwFileExists = FileSystem::fileExists(state.files.inputWeatherFileName.fileName);
            readStat = 0;
            if (epwFileExists) {
                auto epwFile = state.files.inputWeatherFileName.open("CalcThermalComfortAdaptiveCEN15251");
                for (i = 1; i <= 9; ++i) { // Headers
                    epwFile.readLine();
                }
                jStartDay = DayOfYear - 1;
                calcStartDay = jStartDay - 7;
                if (calcStartDay > 0) {
                    calcStartHr = 24 * (calcStartDay - 1) + 1;
                    for (i = 1; i <= calcStartHr - 1; ++i) {
                        epwFile.readLine();
                    }
                    runningAverageCEN = 0.0;
                    for (i = 1; i <= 7; ++i) {
                        avgDryBulbCEN = 0.0;
                        for (j = 1; j <= 24; ++j) {
                            epwLine = epwFile.readLine().data;
                            for (ind = 1; ind <= 6; ++ind) {
                                pos = index(epwLine, ',');
                                epwLine.erase(0, pos + 1);
                            }
                            pos = index(epwLine, ',');
                            dryBulb = StrToReal(epwLine.substr(0, pos));
                            avgDryBulbCEN += (dryBulb / 24.0);
                        }
                        runningAverageCEN += alpha_pow(i) * avgDryBulbCEN;
                    }
                } else { // Do special things for wrapping the epw
                    calcEndDay = jStartDay;
                    calcStartDay += 365;
                    calcEndHr = 24 * calcEndDay;
                    calcStartHr = 24 * (calcStartDay - 1) + 1;
                    for (i = 1; i <= calcEndDay; ++i) {
                        avgDryBulbCEN = 0.0;
                        for (j = 1; j <= 24; ++j) {
                            epwLine = epwFile.readLine().data;
                            for (ind = 1; ind <= 6; ++ind) {
                                pos = index(epwLine, ',');
                                epwLine.erase(0, pos + 1);
                            }
                            pos = index(epwLine, ',');
                            dryBulb = StrToReal(epwLine.substr(0, pos));
                            avgDryBulbCEN += (dryBulb / 24.0);
                        }
                        runningAverageCEN += std::pow(alpha, calcEndDay - i) * avgDryBulbCEN;
                    }
                    for (i = calcEndHr + 1; i <= calcStartHr - 1; ++i) {
                        epwFile.readLine();
                    }
                    for (i = 1; i <= 7 - calcEndDay; ++i) {
                        avgDryBulbCEN = 0.0;
                        for (j = 1; j <= 24; ++j) {
                            epwLine = epwFile.readLine().data;
                            for (ind = 1; ind <= 6; ++ind) {
                                pos = index(epwLine, ',');
                                epwLine.erase(0, pos + 1);
                            }
                            pos = index(epwLine, ',');
                            dryBulb = StrToReal(epwLine.substr(0, pos));
                            avgDryBulbCEN += (dryBulb / 24.0);
                        }
                        runningAverageCEN += alpha_pow(i) * avgDryBulbCEN;
                    }
                }
                runningAverageCEN *= (1.0 - alpha);
                avgDryBulbCEN = 0.0;
                useEpwData = true;
                firstDaySet = true;
            }
        } else if (initiate && !weathersimulation) {
            runningAverageCEN = inavgdrybulb;
            avgDryBulbCEN = 0.0;
        }
        if (initiate) return;

        if (BeginDayFlag && !firstDaySet) {
            // Update the running average, reset the daily avg
            runningAverageCEN = 0.2 * runningAverageCEN + 0.8 * avgDryBulbCEN;
            avgDryBulbCEN = 0.0;
        }

        firstDaySet = false;

        // Update the daily average
        if (BeginHourFlag) {
            avgDryBulbCEN += (OutDryBulbTemp / 24.0);
        }

        for (state.dataThermalComforts->PeopleNum = 1; state.dataThermalComforts->PeopleNum <= TotPeople; ++state.dataThermalComforts->PeopleNum) {
            if (!People(state.dataThermalComforts->PeopleNum).AdaptiveCEN15251) continue;
            state.dataThermalComforts->ZoneNum = People(state.dataThermalComforts->PeopleNum).ZonePtr;
            if (IsZoneDV(state.dataThermalComforts->ZoneNum) || IsZoneUI(state.dataThermalComforts->ZoneNum)) {
                state.dataThermalComforts->AirTemp = TCMF(state.dataThermalComforts->ZoneNum);
            } else {
                state.dataThermalComforts->AirTemp = ZTAVComf(state.dataThermalComforts->ZoneNum);
            }
            state.dataThermalComforts->RadTemp = CalcRadTemp(state, state.dataThermalComforts->PeopleNum);
            state.dataThermalComforts->OpTemp = (state.dataThermalComforts->AirTemp + state.dataThermalComforts->RadTemp) / 2.0;
            state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).ThermalComfortOpTemp = state.dataThermalComforts->OpTemp;
            state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).CEN15251RunningMeanOutdoorTemp = runningAverageCEN;
            if (runningAverageCEN >= 10.0 && runningAverageCEN <= 30.0) {
                // Calculate the comfort here (people/output handling loop)
                numOccupants = People(state.dataThermalComforts->PeopleNum).NumberOfPeople * GetCurrentScheduleValue(People(state.dataThermalComforts->PeopleNum).NumberOfPeoplePtr);
                tComf = 0.33 * runningAverageCEN + 18.8;
                state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).TComfCEN15251 = tComf;
                if (numOccupants > 0) {
                    if (runningAverageCEN < 15) {
                        tComfLow = 23.75; // Lower limit is constant in this region
                    } else {
                        tComfLow = tComf;
                    }
                    if (state.dataThermalComforts->OpTemp < tComf + 2.0 && state.dataThermalComforts->OpTemp > tComfLow - 2.0) {
                        // Within Cat I, II, III Limits
                        state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).ThermalComfortAdaptiveCEN15251CatI = 1;
                        state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).ThermalComfortAdaptiveCEN15251CatII = 1;
                        state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).ThermalComfortAdaptiveCEN15251CatIII = 1;
                    } else if (state.dataThermalComforts->OpTemp < tComf + 3.0 && state.dataThermalComforts->OpTemp > tComfLow - 3.0) {
                        // Within Cat II, III Limits
                        state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).ThermalComfortAdaptiveCEN15251CatI = 0;
                        state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).ThermalComfortAdaptiveCEN15251CatII = 1;
                        state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).ThermalComfortAdaptiveCEN15251CatIII = 1;
                        People(state.dataThermalComforts->PeopleNum).TimeNotMetCEN15251CatI += SysTimeElapsed;
                    } else if (state.dataThermalComforts->OpTemp < tComf + 4.0 && state.dataThermalComforts->OpTemp > tComfLow - 4.0) {
                        // Within Cat III Limits
                        state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).ThermalComfortAdaptiveCEN15251CatI = 0;
                        state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).ThermalComfortAdaptiveCEN15251CatII = 0;
                        state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).ThermalComfortAdaptiveCEN15251CatIII = 1;
                        People(state.dataThermalComforts->PeopleNum).TimeNotMetCEN15251CatI += SysTimeElapsed;
                        People(state.dataThermalComforts->PeopleNum).TimeNotMetCEN15251CatII += SysTimeElapsed;
                    } else {
                        // None
                        state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).ThermalComfortAdaptiveCEN15251CatI = 0;
                        state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).ThermalComfortAdaptiveCEN15251CatII = 0;
                        state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).ThermalComfortAdaptiveCEN15251CatIII = 0;
                        People(state.dataThermalComforts->PeopleNum).TimeNotMetCEN15251CatI += SysTimeElapsed;
                        People(state.dataThermalComforts->PeopleNum).TimeNotMetCEN15251CatII += SysTimeElapsed;
                        People(state.dataThermalComforts->PeopleNum).TimeNotMetCEN15251CatIII += SysTimeElapsed;
                    }
                } else {
                    // Unoccupied
                    state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).ThermalComfortAdaptiveCEN15251CatI = -1;
                    state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).ThermalComfortAdaptiveCEN15251CatII = -1;
                    state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).ThermalComfortAdaptiveCEN15251CatIII = -1;
                }
            } else {
                // Monthly temp out of range
                state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).ThermalComfortAdaptiveCEN15251CatI = -1;
                state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).ThermalComfortAdaptiveCEN15251CatII = -1;
                state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).ThermalComfortAdaptiveCEN15251CatIII = -1;
                state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).TComfCEN15251 = -1.0;
            }
        }
    }

    void DynamicClothingModel(EnergyPlusData &state)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Kwang Ho Lee
        //       DATE WRITTEN   June 2013

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 TemporaryVariable;

        if (state.dataThermalComforts->TemporarySixAMTemperature < -5.0) {
            state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).ClothingValue = 1.0;
        } else if ((state.dataThermalComforts->TemporarySixAMTemperature >= -5.0) && (state.dataThermalComforts->TemporarySixAMTemperature < 5.0)) {
            state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).ClothingValue = 0.818 - 0.0364 * state.dataThermalComforts->TemporarySixAMTemperature;
        } else if ((state.dataThermalComforts->TemporarySixAMTemperature >= 5.0) && (state.dataThermalComforts->TemporarySixAMTemperature < 26.0)) {
            TemporaryVariable = -0.1635 - 0.0066 * state.dataThermalComforts->TemporarySixAMTemperature;
            state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).ClothingValue = std::pow(10.0, TemporaryVariable);
        } else if (state.dataThermalComforts->TemporarySixAMTemperature >= 26.0) {
            state.dataThermalComforts->ThermalComfortData(state.dataThermalComforts->PeopleNum).ClothingValue = 0.46;
        }
    }

} // namespace ThermalComfort

} // namespace EnergyPlus
