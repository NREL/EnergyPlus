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

// C++ Headers

// EnergyPlus Headers
#include <EnergyPlus/IndoorGreen.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/HeatBalanceInternalHeatGains.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/api/datatransfer.h>
#include <EnergyPlus/ZoneTempPredictorCorrector.hh>

namespace EnergyPlus {

namespace IndoorGreen {
    // Module containing the routines dealing with the Indoor Greenery Systems.

    // MODULE INFORMATION:  Liping Wang
    //       DATE WRITTEN   Oct 2023
    //       RE-ENGINEERED  na

    // Use statements for access to subroutines in other modules
    using namespace ScheduleManager;

    // MODULE PARAMETER DEFINITIONS
    const char *cCMO_IndoorGreen = "IndoorGreen";
    //const std::string indoorgreenModuleObject = "IndoorGreen";
    //constexpr Real64 SimpConvAirFlowSpeed(0.5); // m/s

    void SimIndoorGreen(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Liping Wang
        //       DATE WRITTEN   Oct 2023
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine simulates the Indoor Greenery Systems.
        // Assumptions: 1) one system per zone; 2) the effects of indoor greenery systems on surface heat balance are currently ignored. 


        if (state.dataIndoorGreen->getInputFlag) { 
            bool ErrorsFound(false);
            const char *RoutineName("IndoorGreenInputs: "); // include trailing blank space
            GetIndoorGreenInput(state,ErrorsFound);
            if (ErrorsFound) {
                ShowFatalError(state, format("{}Errors found in input.  Program terminates.", RoutineName));
            }
            state.dataIndoorGreen->getInputFlag = false;
        }
        if (state.dataIndoorGreen->NumIndoorGreen > 0) {
            InitIndoorGreen(state);

            // Simulate evapotranspiration from indoor greenery systems
            ETModel(state);
        } 
    }

    void GetIndoorGreenInput(EnergyPlusData &state, bool &ErrorsFound)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Liping Wang
        //       DATE WRITTEN   Oct 2023
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Get the input for the indoor greenery system data and store the input data in the indoorgreens array.

        // METHODOLOGY EMPLOYED:
        // Use the Get routines from the InputProcessor module.
        
        // Using/Aliasing
        using General::FindNumberInList;
       
        // Locals
        // SUBROUTINE PARAMETER DEFINITIONS:
        static constexpr std::string_view RoutineName("GetIndoorGreenInput: ");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        //Array1D_string cAlphaFieldNames;
        //Array1D_string cNumericFieldNames;
        //Array1D_bool lNumericFieldBlanks;
        //Array1D_bool lAlphaFieldBlanks;
        //Array1D_string cAlphaArgs;
        //Array1D<Real64> rNumericArgs;
        const std::string cCurrentModuleObject = "IndoorGreen";
       
                
        int NumNums;   // Number of real numbers returned by GetObjectItem
        int NumAlphas; // Number of alphanumerics returned by GetObjectItem
        int IndoorGreenNum;    // Indoor Green index
        int IOStat;         // Status flag from GetObjectItem
        Real64 SchMin;
        Real64 SchMax;

        state.dataIndoorGreen->NumIndoorGreen = 
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject); 
        // Get input for IndoorGreen objects
        if (state.dataIndoorGreen->NumIndoorGreen > 0)
            state.dataIndoorGreen->indoorgreens.allocate(
                state.dataIndoorGreen->NumIndoorGreen); // Allocate the IndoorGreen input data array

        // Input the data for each Indoor Greenery System
        for (IndoorGreenNum = 1; IndoorGreenNum <= state.dataIndoorGreen->NumIndoorGreen; ++IndoorGreenNum) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     cCurrentModuleObject,
                                                                     IndoorGreenNum,
                                                                     state.dataIPShortCut->cAlphaArgs,
                                                                     NumAlphas,
                                                                     state.dataIPShortCut->rNumericArgs,
                                                                     NumNums,
                                                                     IOStat,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            UtilityRoutines::IsNameEmpty(state, state.dataIPShortCut->cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);
            state.dataIndoorGreen->indoorgreens(IndoorGreenNum).IndoorGreenName = state.dataIPShortCut->cAlphaArgs(1);
            // LW to do space, space list
            state.dataIndoorGreen->indoorgreens(IndoorGreenNum).ZoneName = state.dataIPShortCut->cAlphaArgs(2);
            state.dataIndoorGreen->indoorgreens(IndoorGreenNum).ZonePtr =
                UtilityRoutines::FindItemInList(state.dataIPShortCut->cAlphaArgs(2), state.dataHeatBal->Zone);
            state.dataIndoorGreen->indoorgreens(IndoorGreenNum).ZoneListPtr =
                UtilityRoutines::FindItemInList(state.dataIPShortCut->cAlphaArgs(2), state.dataHeatBal->ZoneList);
            if (state.dataIndoorGreen->indoorgreens(IndoorGreenNum).ZonePtr <= 0 &&
                state.dataIndoorGreen->indoorgreens(IndoorGreenNum).ZoneListPtr <= 0) {
                ShowSevereError(
                    state,
                                format("{}=\"{}\", invalid {} entered={}",
                                       RoutineName,
                                       state.dataIPShortCut->cAlphaArgs(1),
                                       state.dataIPShortCut->cAlphaFieldNames(2),
                                       state.dataIPShortCut->cAlphaArgs(2)));
                ErrorsFound = true;
            }
            state.dataIndoorGreen->indoorgreens(IndoorGreenNum).SchedPtr =
                ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(3));
            if (state.dataIndoorGreen->indoorgreens(IndoorGreenNum).SchedPtr == 0) {
                if (state.dataIPShortCut->lAlphaFieldBlanks(3)) {
                    ShowSevereError(state,
                                    format("{} =\"{}\", {} is required.",
                                           RoutineName,
                                           state.dataIPShortCut->cAlphaArgs(1),
                                           state.dataIPShortCut->cAlphaFieldNames(3)));
                } else {
                    ShowSevereError(state,
                                    format("{} =\"{}\", invalid {} entered={}",
                                           RoutineName,
                                           state.dataIPShortCut->cAlphaArgs(1),
                                           state.dataIPShortCut->cAlphaFieldNames(3),
                                           state.dataIPShortCut->cAlphaArgs(3)));
                }
                ErrorsFound = true;
            } else { // check min/max on schedule
                SchMin = ScheduleManager::GetScheduleMinValue(state, state.dataIndoorGreen->indoorgreens(IndoorGreenNum).SchedPtr);
                SchMax = ScheduleManager::GetScheduleMaxValue(state, state.dataIndoorGreen->indoorgreens(IndoorGreenNum).SchedPtr);
                if (SchMin < 0.0 || SchMax < 0.0) {
                    if (SchMin < 0.0) {
                        ShowSevereError(state,
                                        format("{}=\"{}\", {}, minimum is < 0.0",
                                               RoutineName,
                                               state.dataIPShortCut->cAlphaArgs(1),
                                               state.dataIPShortCut->cAlphaFieldNames(3)));
                        ShowContinueError(
                            state,
                            format("Schedule=\"{}\". Minimum is [{:.1R}]. Values must be >= 0.0.", state.dataIPShortCut->cAlphaArgs(3), SchMin));
                        ErrorsFound = true;
                    }
                    if (SchMax < 0.0) {
                        ShowSevereError(state,
                                        format("{}=\"{}\", {}, maximum is < 0.0",
                                               RoutineName,
                                               state.dataIPShortCut->cAlphaArgs(1),
                                               state.dataIPShortCut->cAlphaFieldNames(3)));
                        ShowContinueError(
                            state,
                            format("Schedule=\"{}\". Maximum is [{:.1R}]. Values must be >= 0.0.", state.dataIPShortCut->cAlphaArgs(3), SchMin));
                        ErrorsFound = true;
                    }
                }
            }
            state.dataIndoorGreen->indoorgreens(IndoorGreenNum).ETCalculationMethod = 1; // default
            if (UtilityRoutines::SameString(UtilityRoutines::makeUPPER(state.dataIPShortCut->cAlphaArgs(4)), "PENMAN-MONTEITH")) {
                state.dataIndoorGreen->indoorgreens(IndoorGreenNum).ETCalculationMethod = 1; // default
            } else if (UtilityRoutines::SameString(UtilityRoutines::makeUPPER(state.dataIPShortCut->cAlphaArgs(4)), "STANGHELLINI")) {
                state.dataIndoorGreen->indoorgreens(IndoorGreenNum).ETCalculationMethod = 2;
            } else if (UtilityRoutines::SameString(UtilityRoutines::makeUPPER(state.dataIPShortCut->cAlphaArgs(4)), "DATAD-RIVEN")) {
                state.dataIndoorGreen->indoorgreens(IndoorGreenNum).ETCalculationMethod = 3;
            } else {
                ShowSevereError(
                    state,
                                format("{}=\"{}\", invalid {} entered={}",
                                       RoutineName,
                                       state.dataIPShortCut->cAlphaArgs(1),
                                       state.dataIPShortCut->cAlphaFieldNames(4),
                                       state.dataIPShortCut->cAlphaArgs(4)));
                ErrorsFound = true;
            }
            state.dataIndoorGreen->indoorgreens(IndoorGreenNum).LeafArea = state.dataIPShortCut->rNumericArgs(1);
            if (state.dataIndoorGreen->indoorgreens(IndoorGreenNum).LeafArea <= 0) {
                ShowSevereError(state,
                                format("{}=\"{}\", invalid {} entered={}",
                                       RoutineName,
                                       state.dataIPShortCut->cAlphaArgs(1),
                                       state.dataIPShortCut->cNumericFieldNames(1),
                                       state.dataIPShortCut->rNumericArgs(1)));
                ErrorsFound = true;
            }
        }
        state.dataIPShortCut->cAlphaFieldNames.deallocate();
        state.dataIPShortCut->cAlphaArgs.deallocate();
        state.dataIPShortCut->lAlphaFieldBlanks.deallocate();
        state.dataIPShortCut->cNumericFieldNames.deallocate();
        state.dataIPShortCut->rNumericArgs.deallocate();
        state.dataIPShortCut->lNumericFieldBlanks.deallocate();
        // Set up output variables
        for (IndoorGreenNum = 1; IndoorGreenNum <= state.dataIndoorGreen->NumIndoorGreen; ++IndoorGreenNum) {
            SetupOutputVariable(state,
                                "Sensible heat gain rate from indoor greenery system",
                                OutputProcessor::Unit::W,
                                state.dataIndoorGreen->indoorgreens(IndoorGreenNum).SensibleRate,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataIndoorGreen->indoorgreens(IndoorGreenNum).IndoorGreenName);
            SetupOutputVariable(state,
                                "Latent heat gain rate from indoor greenery system",
                                OutputProcessor::Unit::W,
                                state.dataIndoorGreen->indoorgreens(IndoorGreenNum).LatentRate,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.dataIndoorGreen->indoorgreens(IndoorGreenNum).IndoorGreenName);
        }
    }

    void InitIndoorGreen(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Liping Wang
        //       DATE WRITTEN   Oct 2023

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for initializations of the Indoor Greenery System objects.

        int IndoorGreenNum; // Indoor Green index
           
            for (IndoorGreenNum = 1; IndoorGreenNum <= state.dataIndoorGreen->NumIndoorGreen; ++IndoorGreenNum) {
            // Set the reporting variables to zero at each timestep.
                state.dataIndoorGreen->indoorgreens(IndoorGreenNum).SensibleRate = 0.0;
                state.dataIndoorGreen->indoorgreens(IndoorGreenNum).LatentRate = 0.0;
            // Set indoor environment conditions at the every time step initializations 
                //LW to do ZoneList, check if this is the right parameter for temperature
                state.dataIndoorGreen->indoorgreens(IndoorGreenNum).ZPreTemp = 0.0;
                    //        state.dataHeatBal->Zone(state.dataIndoorGreen->indoorgreens(IndoorGreenNum).ZonePtr).ZoneMeasuredTemperature;
                state.dataIndoorGreen->indoorgreens(IndoorGreenNum).ZPreHum = 0.0;
                    //        state.dataHeatBal->Zone(state.dataIndoorGreen->indoorgreens(IndoorGreenNum).ZonePtr).ZoneMeasuredHumidityRatio;
                state.dataIndoorGreen->indoorgreens(IndoorGreenNum).ZCO2 = 400; 
                state.dataIndoorGreen->indoorgreens(IndoorGreenNum).ZPPFD =0;                   
            }
  
    }
    void ETModel(EnergyPlusData &state)
    {
            // SUBROUTINE INFORMATION:
            //       AUTHOR         Liping Wang
            //       DATE WRITTEN   Oct 2023

            // PURPOSE OF THIS SUBROUTINE:
            // This subroutine is for the calculation of evapotranspiration effects from the Indoor Greenery System objects.
            // SUBROUTINE PARAMETER DEFINITIONS:
            static constexpr std::string_view RoutineName("ETModel: ");
            int IndoorGreenNum; // Indoor Green index
            Real64 ZonePreTemp;    // Indoor air temprature (C)
            Real64 ZonePreHum;     // Indoor humidity ratio (kg moisture / kg dry air)
            Real64 ZoneNewTemp;    // Indoor air temprature (C) after ET
            Real64 ZoneNewHum;     // Indoor humidity ratio (kg moisture / kg dry air) after ET
            Real64 ZoneSatHum;     // Saturated humidity ratio 
            Real64 ZoneCO2;        // Indoor zone co2 concentration (ppm)
            Real64 ZonePPFD;       // Indoor net radiation (PPFD)
            Real64 ETRate=0.0;         // mm/s; kg/(m2s)
            Real64 Timestep;       // s
            Real64 ETTotal;        // kg
            Real64 rhoair;         // kg/m3
            Real64 Tdp;            // dew point temperature
            Real64 HCons;           // enthalpy (J/kg)
            Real64 HMid;           // enthalpy 3rd point (J/kg)
            Real64 ZoneAirVol;     // zone air volume (m3)
            // Method for ET calculation: Penman-Monteith=1, Stanghellini=2, Data-driven=3
            for (IndoorGreenNum = 1; IndoorGreenNum <= state.dataIndoorGreen->NumIndoorGreen; ++IndoorGreenNum) {
                ZonePreTemp = state.dataZoneTempPredictorCorrector->zoneHeatBalance(state.dataIndoorGreen->indoorgreens(IndoorGreenNum).ZonePtr).ZT;
                ZonePreHum = state.dataZoneTempPredictorCorrector->zoneHeatBalance(state.dataIndoorGreen->indoorgreens(IndoorGreenNum).ZonePtr).airHumRat;
                // LW to do 
                ZoneCO2 = 400;  
                ZonePPFD= 100.0;  
                if (state.dataIndoorGreen->indoorgreens(IndoorGreenNum).ETCalculationMethod == 1) {
                ETRate=ETPenmanMonteith(state,ZonePreTemp,ZonePreHum,ZoneCO2,ZonePPFD);
                } 
                else if (state.dataIndoorGreen->indoorgreens(IndoorGreenNum).ETCalculationMethod == 2) {
                ETRate=ETStanghellini(state);
                } 
                else if (state.dataIndoorGreen->indoorgreens(IndoorGreenNum).ETCalculationMethod == 3) {
                ETRate=ETDatadriven(state);
                } 

                //adding ET effects 
                Timestep = state.dataHVACGlobal->TimeStepSysSec; // unit s
                ETTotal = ETRate * Timestep * state.dataIndoorGreen->indoorgreens(IndoorGreenNum).LeafArea *
                           ScheduleManager::GetCurrentScheduleValue(state, state.dataIndoorGreen->indoorgreens(IndoorGreenNum).SchedPtr) / 1000 /
                           1000;
                rhoair = Psychrometrics::PsyRhoAirFnPbTdbW(state, state.dataEnvrn->StdBaroPress, ZonePreTemp, ZonePreHum);
                ZoneAirVol = state.dataHeatBal->Zone(state.dataIndoorGreen->indoorgreens(IndoorGreenNum).ZonePtr).Volume;
                ZoneNewHum =
                    ZonePreHum + ETTotal / (rhoair * ZoneAirVol);
                Tdp = Psychrometrics::PsyTdpFnWPb(state, ZonePreHum, state.dataEnvrn->StdBaroPress); 
                ZoneSatHum = Psychrometrics::PsyWFnTdpPb(state, Tdp, state.dataEnvrn->OutBaroPress); //saturated humidity ratio
                HCons = Psychrometrics::PsyHFnTdbW(ZonePreTemp, ZonePreHum);
                if (ZoneNewHum < ZoneSatHum) {
                ZoneNewTemp = Psychrometrics::PsyTdbFnHW(HCons, ZoneNewHum);
                } else {
                ZoneNewTemp = Tdp;
                ZoneNewHum = ZoneSatHum;
                }
                HMid = Psychrometrics::PsyHFnTdbW(ZoneNewTemp, ZoneNewHum);
                state.dataIndoorGreen->indoorgreens(IndoorGreenNum).SensibleRate = ZoneAirVol * rhoair * (HMid - HCons)/Timestep; // unit W
                state.dataIndoorGreen->indoorgreens(IndoorGreenNum).LatentRate = ZoneAirVol * rhoair * (HCons - HMid)/Timestep; //unit W
                SetupZoneInternalGain(state,
                                      state.dataIndoorGreen->indoorgreens(IndoorGreenNum).ZonePtr,
                                      RoutineName,
                                      DataHeatBalance::IntGainType::IndoorGreen,
                                      &state.dataIndoorGreen->indoorgreens(IndoorGreenNum).SensibleRate,
                                      nullptr,
                                      &state.dataIndoorGreen->indoorgreens(IndoorGreenNum).LatentRate,
                                      nullptr,
                                      nullptr,
                                      nullptr);
            }
    }
    Real64 ETPenmanMonteith(EnergyPlusData &state, Real64 &ZonePreTemp, Real64 &ZonePreHum, Real64 &ZoneCO2, Real64 &ZonePPFD)
    {
            // SUBROUTINE INFORMATION:
            //       AUTHOR         Liping Wang
            //       DATE WRITTEN   Oct 2023
            
            // PURPOSE OF THIS SUBROUTINE:
            // This subroutine is for using Penman-Monteith ET model to calculate evapotranspiration rates from the Indoor Greenery System objects.
            
            // SUBROUTINE PARAMETER DEFINITIONS:
            static constexpr std::string_view RoutineName("ETPenmanMonteith: ");
            Real64 hfg = Psychrometrics::PsyHfgAirFnWTdb(ZonePreHum, ZonePreTemp)/std::pow(10,6); // Latent heat of vaporization (MJ/kg)
            Real64 slopepat = 0.200 * std::pow((0.00738 * ZonePreTemp + 0.8072), 7) - 0.000116; //Slope of the saturation vapor pressure-temperature curve (kPa/°C)
            Real64 CpAir = Psychrometrics::PsyCpAirFnW(ZonePreHum) / std::pow(10, 6); // specific heat of air at constant pressure (MJ kg−1 °C−1)
            Real64 StdPb = state.dataEnvrn->StdBaroPress / 1000;    // atmospheric pressure (kPa)
            Real64 const mw=0.622;//ratio molecular weight of water vapor / dry air = 0.622.
            Real64 psyconst = CpAir * StdPb / (hfg * mw); // Psychrometric constant (kPa/°C)
            Real64 rs =943; //stomatal resistance s/m
            Real64 ra =200; //aerodynamic resistance s/m
            Real64 In = ZonePPFD * 0.327 / std::pow(10, 6); // net radiation W/m2
            Real64 G = 0.0; //soil heat flux (MJ/(m2s))
            Real64 rhoair = Psychrometrics::PsyRhoAirFnPbTdbW(state, StdPb * 1000, ZonePreTemp, ZonePreHum);
            Real64 Tdp = Psychrometrics::PsyTdpFnWPb(state, ZonePreHum, StdPb * 1000); //dew point temperature
            Real64 vp = Psychrometrics::PsyPsatFnTemp(state, Tdp, RoutineName)/1000; // actual vapor pressure of the air (kpa)
            Real64 vpSat = Psychrometrics::PsyPsatFnTemp(state, ZonePreTemp, RoutineName)/1000;// saturation vapor pressure at air temperature (kpa)
            Real64 vpd = vpSat - vp; // vapor pressure deficit (kpa)
            Real64 ETRate; //mm/s; kg/(m2s)
            ETRate = (1 / hfg) * (slopepat * (In - G) + (rhoair * CpAir * vpd) / ra) / (slopepat + psyconst * (1 + rs / ra));//Penman-Monteith ET model
            return ETRate; //mm/s; kg/(m2s)
    }
    Real64 ETStanghellini(EnergyPlusData &state)
    {
        return 0;
    }
    Real64 ETDatadriven(EnergyPlusData &state)
    {
        return 0; 
    }
  
} // namespace Indoor Green

} // namespace EnergyPlus
