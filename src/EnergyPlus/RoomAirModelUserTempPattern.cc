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

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/ArrayS.functions.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/member.functions.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataErrorTracking.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataRoomAirModel.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/InternalHeatGains.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/RoomAirModelUserTempPattern.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus::RoomAirModelUserTempPattern {

// MODULE INFORMATION:
//       AUTHOR         Brent Griffith
//       DATE WRITTEN   August 2005 (started in January 2004)
//       RE-ENGINEERED

// PURPOSE OF THIS MODULE:
// This module is the main module for running the
// user-defined temperature pattern model.
// This "air model" doesn't predict anything about the room air
// but provides a method for users to model the
// impact of non-uniform air temps.  the distribution of air temperatures
// is defined by the user and referred to as a "pattern"

// METHODOLOGY EMPLOYED:
// This module contains all subroutines required by the
// user defined temperature pattern roomair modeling.
// See DataRoomAir.cc for variable declarations

// Using/Aliasing
using namespace DataRoomAirModel;

// Functions

void ManageUserDefinedPatterns(EnergyPlusData &state, int const ZoneNum) // index number for the specified zone
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   January 2004/Aug 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    //  manage the user-defined air temp. distribution model

    // METHODOLOGY EMPLOYED:
    // calls subroutines

    // transfer data from surface domain to air domain for the specified zone
    InitTempDistModel(state, ZoneNum);

    GetSurfHBDataForTempDistModel(state, ZoneNum);

    // perform TempDist model calculations
    CalcTempDistModel(state, ZoneNum);

    // transfer data from air domain back to surface domain for the specified zone
    SetSurfHBDataForTempDistModel(state, ZoneNum);
}

//****************************************************

void InitTempDistModel(EnergyPlusData &state, int const ZoneNum) // index number for the specified zone
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         <author>
    //       DATE WRITTEN   <date_written>
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    int SurfNum; // do loop counter

    if (state.dataRoomAirModelTempPattern->MyOneTimeFlag) {
        state.dataRoomAirModelTempPattern->MyEnvrnFlag.dimension(state.dataGlobal->NumOfZones, true);
        state.dataRoomAirModelTempPattern->MyOneTimeFlag = false;
    }

    if (state.dataGlobal->BeginEnvrnFlag && state.dataRoomAirModelTempPattern->MyEnvrnFlag(ZoneNum)) {
        state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).TairMean = 23.0;
        state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).Tstat = 23.0;
        state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).Tleaving = 23.0;
        state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).Texhaust = 23.0;
        state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).Gradient = 0.0;
        for (SurfNum = 1; SurfNum <= state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).totNumSurfs; ++SurfNum) {
            state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).Surf(SurfNum).TadjacentAir = 23.0;
        }
        state.dataRoomAirModelTempPattern->MyEnvrnFlag(ZoneNum) = false;
    }

    if (!state.dataGlobal->BeginEnvrnFlag) state.dataRoomAirModelTempPattern->MyEnvrnFlag(ZoneNum) = true;

    // init report variable
    state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).Gradient = 0.0;
}

void GetSurfHBDataForTempDistModel(EnergyPlusData &state, int const ZoneNum) // index number for the specified zone
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         B. Griffith
    //       DATE WRITTEN   August 2005
    //       MODIFIED
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    //  map data from Heat Balance domain to Room Air Modeling Domain
    //  for the current zone, (only need mean air temp)
    //  also acts as an init routine

    // METHODOLOGY EMPLOYED:
    // use ZT from DataHeatBalFanSys

    // Using/Aliasing

    // intialize in preperation for calculations
    state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).Tstat = state.dataHeatBalFanSys->MAT(ZoneNum);
    state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).Tleaving = state.dataHeatBalFanSys->MAT(ZoneNum);
    state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).Texhaust = state.dataHeatBalFanSys->MAT(ZoneNum);
    for (auto &e : state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).Surf)
        e.TadjacentAir = state.dataHeatBalFanSys->MAT(ZoneNum);

    // the only input this method needs is the zone MAT or ZT or ZTAV  ?  (original was ZT)
    state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).TairMean =
        state.dataHeatBalFanSys->MAT(ZoneNum); // this is lagged from previous corrector result
}

//*****************************************************************************************

void CalcTempDistModel(EnergyPlusData &state, int const ZoneNum) // index number for the specified zone
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   August 2005
    //       MODIFIED
    //       RE-ENGINEERED

    // PURPOSE OF THIS SUBROUTINE:
    // figure out which pattern is scheduled and call
    // appropriate subroutine

    // Using/Aliasing
    using General::FindNumberInList;
    using ScheduleManager::GetCurrentScheduleValue;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    // unused    INTEGER    :: thisZoneInfo
    Real64 AvailTest;
    int CurntPatternKey;
    int CurPatrnID;

    // first determine availability
    AvailTest = GetCurrentScheduleValue(state, state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).AvailSchedID);

    if ((AvailTest != 1.0) || (!state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).IsUsed)) {
        // model not to be used. Use complete mixing method

        state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).Tstat = state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).TairMean;
        state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).Tleaving = state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).TairMean;
        state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).Texhaust = state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).TairMean;
        for (auto &e : state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).Surf)
            e.TadjacentAir = state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).TairMean;

        return;

    } else { // choose pattern and call subroutine

        CurntPatternKey = GetCurrentScheduleValue(state, state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).PatternSchedID);

        CurPatrnID = FindNumberInList(CurntPatternKey, state.dataRoomAirMod->RoomAirPattern, &TemperaturePatternStruct::PatrnID);

        if (CurPatrnID == 0) {
            // throw error here ? way to test schedules before getting to this point?
            ShowFatalError(state, format("User defined room air pattern index not found: {}", CurntPatternKey));
            return;
        }

        {
            auto const SELECT_CASE_var(state.dataRoomAirMod->RoomAirPattern(CurPatrnID).PatternMode);

            if (SELECT_CASE_var == DataRoomAirModel::UserDefinedPatternType::ConstGradTempPattern) {

                FigureConstGradPattern(state, CurPatrnID, ZoneNum);

            } else if (SELECT_CASE_var == DataRoomAirModel::UserDefinedPatternType::TwoGradInterpPattern) {

                FigureTwoGradInterpPattern(state, CurPatrnID, ZoneNum);

            } else if (SELECT_CASE_var == DataRoomAirModel::UserDefinedPatternType::NonDimenHeightPattern) {

                FigureHeightPattern(state, CurPatrnID, ZoneNum);

            } else if (SELECT_CASE_var == DataRoomAirModel::UserDefinedPatternType::SurfMapTempPattern) {

                FigureSurfMapPattern(state, CurPatrnID, ZoneNum);

            } else {
                // should not come here
            }
        }

    } // availability control construct
}

void FigureSurfMapPattern(EnergyPlusData &state, int const PattrnID, int const ZoneNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         B Griffith
    //       DATE WRITTEN   August 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // main calculation routine for surface pattern

    // METHODOLOGY EMPLOYED:
    // simple polling and applying prescribed
    // delta Tai's to current mean air temp
    // on a surface by surface basis

    // Using/Aliasing
    using General::FindNumberInList;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 Tmean;
    int found;
    int i;

    Tmean = state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).TairMean;

    for (i = 1; i <= state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).totNumSurfs; ++i) {
        // cycle through zone surfaces and look for match
        found = FindNumberInList(state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).Surf(i).SurfID,
                                 state.dataRoomAirMod->RoomAirPattern(PattrnID).MapPatrn.SurfID,
                                 state.dataRoomAirMod->RoomAirPattern(PattrnID).MapPatrn.NumSurfs);
        if (found != 0) { // if surf is in map then assign, else give it MAT
            state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).Surf(i).TadjacentAir =
                state.dataRoomAirMod->RoomAirPattern(PattrnID).MapPatrn.DeltaTai(found) + Tmean;
        } else {
            state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).Surf(i).TadjacentAir = Tmean;
        }
    }

    state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).Tstat = state.dataRoomAirMod->RoomAirPattern(PattrnID).DeltaTstat + Tmean;
    state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).Tleaving = state.dataRoomAirMod->RoomAirPattern(PattrnID).DeltaTleaving + Tmean;
    state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).Texhaust = state.dataRoomAirMod->RoomAirPattern(PattrnID).DeltaTexhaust + Tmean;
}

void FigureHeightPattern(EnergyPlusData &state, int const PattrnID, int const ZoneNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         B Griffith
    //       DATE WRITTEN   August 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // calculate the pattern for non-dimensional vertical profile

    // METHODOLOGY EMPLOYED:
    // treat profile as lookup table and interpolate

    // Using/Aliasing
    using FluidProperties::FindArrayIndex;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 Tmean;
    int lowSideID;
    int highSideID;
    Real64 thisZeta;
    int i;
    Real64 lowSideZeta;
    Real64 hiSideZeta;
    Real64 fractBtwn;
    Real64 tmpDeltaTai;

    tmpDeltaTai = 0.0;
    Tmean = state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).TairMean;

    for (i = 1; i <= state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).totNumSurfs; ++i) {

        thisZeta = state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).Surf(i).Zeta;
        lowSideID = FindArrayIndex(thisZeta, state.dataRoomAirMod->RoomAirPattern(PattrnID).VertPatrn.ZetaPatrn);
        highSideID = lowSideID + 1;
        if (lowSideID == 0) lowSideID = 1; // protect against array bounds

        lowSideZeta = state.dataRoomAirMod->RoomAirPattern(PattrnID).VertPatrn.ZetaPatrn(lowSideID);
        if (highSideID <= isize(state.dataRoomAirMod->RoomAirPattern(PattrnID).VertPatrn.ZetaPatrn)) {
            hiSideZeta = state.dataRoomAirMod->RoomAirPattern(PattrnID).VertPatrn.ZetaPatrn(highSideID);
        } else { // trap array bounds
            hiSideZeta = lowSideZeta;
        }
        if ((hiSideZeta - lowSideZeta) != 0.0) {
            fractBtwn = (thisZeta - lowSideZeta) / (hiSideZeta - lowSideZeta);
            tmpDeltaTai = state.dataRoomAirMod->RoomAirPattern(PattrnID).VertPatrn.DeltaTaiPatrn(lowSideID) +
                          fractBtwn * (state.dataRoomAirMod->RoomAirPattern(PattrnID).VertPatrn.DeltaTaiPatrn(highSideID) -
                                       state.dataRoomAirMod->RoomAirPattern(PattrnID).VertPatrn.DeltaTaiPatrn(lowSideID));

        } else { // would divide by zero, using low side value

            tmpDeltaTai = state.dataRoomAirMod->RoomAirPattern(PattrnID).VertPatrn.DeltaTaiPatrn(lowSideID);
        }

        state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).Surf(i).TadjacentAir = tmpDeltaTai + Tmean;

    } // surfaces in this zone

    state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).Tstat = state.dataRoomAirMod->RoomAirPattern(PattrnID).DeltaTstat + Tmean;
    state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).Tleaving = state.dataRoomAirMod->RoomAirPattern(PattrnID).DeltaTleaving + Tmean;
    state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).Texhaust = state.dataRoomAirMod->RoomAirPattern(PattrnID).DeltaTexhaust + Tmean;
}

void FigureTwoGradInterpPattern(EnergyPlusData &state, int const PattrnID, int const ZoneNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         B Griffith
    //       DATE WRITTEN   Aug 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // calculate two gradient interpolation pattern

    // METHODOLOGY EMPLOYED:
    // Case statement controls how interpolations are done
    // based on user selected mode.
    // calculations vary by mode

    // Using/Aliasing

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 Tmean;        // MAT deg C
    Real64 Grad;         // vertical temperature gradient C/m
    Real64 DeltaT;       // temperature difference
    Real64 CoolLoad;     // sensible cooling load
    Real64 HeatLoad;     // sensible heating load
    Real64 ZetaTmean;    // non-dimensional height for mean air temp
    int i;               // do loop index
    Real64 thisZeta;     // non-dimensional height
    Real64 DeltaHeight;  // height difference in m
    Real64 tempDeltaTai; // temporary temperature difference

    if (state.dataRoomAirModelTempPattern->MyOneTimeFlag2) {
        state.dataRoomAirModelTempPattern->SetupOutputFlag.dimension(state.dataGlobal->NumOfZones, true); // init
        state.dataRoomAirModelTempPattern->MyOneTimeFlag2 = false;
    }

    if (state.dataRoomAirModelTempPattern->SetupOutputFlag(ZoneNum)) {
        SetupOutputVariable(state,
                            "Room Air Zone Vertical Temperature Gradient",
                            OutputProcessor::Unit::K_m,
                            state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).Gradient,
                            "HVAC",
                            "State",
                            state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).ZoneName);

        state.dataRoomAirModelTempPattern->SetupOutputFlag(ZoneNum) = false;
    }

    Tmean = state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).TairMean;

    // determine gradient depending on mode
    {
        auto const SELECT_CASE_var(state.dataRoomAirMod->RoomAirPattern(PattrnID).TwoGradPatrn.InterpolationMode);

        if (SELECT_CASE_var == DataRoomAirModel::UserDefinedPatternMode::OutdoorDryBulbMode) {

            Grad = OutdoorDryBulbGrad(state.dataHeatBal->Zone(ZoneNum).OutDryBulbTemp,
                                      state.dataRoomAirMod->RoomAirPattern(PattrnID).TwoGradPatrn.UpperBoundTempScale,
                                      state.dataRoomAirMod->RoomAirPattern(PattrnID).TwoGradPatrn.HiGradient,
                                      state.dataRoomAirMod->RoomAirPattern(PattrnID).TwoGradPatrn.LowerBoundTempScale,
                                      state.dataRoomAirMod->RoomAirPattern(PattrnID).TwoGradPatrn.LowGradient);

        } else if (SELECT_CASE_var == DataRoomAirModel::UserDefinedPatternMode::ZoneAirTempMode) {

            if (Tmean >= state.dataRoomAirMod->RoomAirPattern(PattrnID).TwoGradPatrn.UpperBoundTempScale) {
                Grad = state.dataRoomAirMod->RoomAirPattern(PattrnID).TwoGradPatrn.HiGradient;

            } else if (Tmean <= state.dataRoomAirMod->RoomAirPattern(PattrnID).TwoGradPatrn.LowerBoundTempScale) {

                Grad = state.dataRoomAirMod->RoomAirPattern(PattrnID).TwoGradPatrn.LowGradient;
            } else { // interpolate
                if ((state.dataRoomAirMod->RoomAirPattern(PattrnID).TwoGradPatrn.UpperBoundTempScale -
                     state.dataRoomAirMod->RoomAirPattern(PattrnID).TwoGradPatrn.LowerBoundTempScale) == 0.0) {
                    // bad user input, trapped during get input
                    Grad = state.dataRoomAirMod->RoomAirPattern(PattrnID).TwoGradPatrn.LowGradient;
                } else {

                    Grad = state.dataRoomAirMod->RoomAirPattern(PattrnID).TwoGradPatrn.LowGradient +
                           ((Tmean - state.dataRoomAirMod->RoomAirPattern(PattrnID).TwoGradPatrn.LowerBoundTempScale) /
                            (state.dataRoomAirMod->RoomAirPattern(PattrnID).TwoGradPatrn.UpperBoundTempScale -
                             state.dataRoomAirMod->RoomAirPattern(PattrnID).TwoGradPatrn.LowerBoundTempScale)) *
                               (state.dataRoomAirMod->RoomAirPattern(PattrnID).TwoGradPatrn.HiGradient -
                                state.dataRoomAirMod->RoomAirPattern(PattrnID).TwoGradPatrn.LowGradient);
                }
            }

        } else if (SELECT_CASE_var == DataRoomAirModel::UserDefinedPatternMode::DeltaOutdoorZone) {
            DeltaT = state.dataHeatBal->Zone(ZoneNum).OutDryBulbTemp - Tmean;
            if (DeltaT >= state.dataRoomAirMod->RoomAirPattern(PattrnID).TwoGradPatrn.UpperBoundTempScale) {
                Grad = state.dataRoomAirMod->RoomAirPattern(PattrnID).TwoGradPatrn.HiGradient;

            } else if (DeltaT <= state.dataRoomAirMod->RoomAirPattern(PattrnID).TwoGradPatrn.LowerBoundTempScale) {

                Grad = state.dataRoomAirMod->RoomAirPattern(PattrnID).TwoGradPatrn.LowGradient;
            } else { // interpolate
                if ((state.dataRoomAirMod->RoomAirPattern(PattrnID).TwoGradPatrn.UpperBoundTempScale -
                     state.dataRoomAirMod->RoomAirPattern(PattrnID).TwoGradPatrn.LowerBoundTempScale) == 0.0) {
                    // bad user input, trapped during get input
                    Grad = state.dataRoomAirMod->RoomAirPattern(PattrnID).TwoGradPatrn.LowGradient;
                } else {

                    Grad = state.dataRoomAirMod->RoomAirPattern(PattrnID).TwoGradPatrn.LowGradient +
                           ((DeltaT - state.dataRoomAirMod->RoomAirPattern(PattrnID).TwoGradPatrn.LowerBoundTempScale) /
                            (state.dataRoomAirMod->RoomAirPattern(PattrnID).TwoGradPatrn.UpperBoundTempScale -
                             state.dataRoomAirMod->RoomAirPattern(PattrnID).TwoGradPatrn.LowerBoundTempScale)) *
                               (state.dataRoomAirMod->RoomAirPattern(PattrnID).TwoGradPatrn.HiGradient -
                                state.dataRoomAirMod->RoomAirPattern(PattrnID).TwoGradPatrn.LowGradient);
                }
            }

        } else if (SELECT_CASE_var == DataRoomAirModel::UserDefinedPatternMode::SensibleCoolingMode) {

            CoolLoad = state.dataHeatBal->SNLoadCoolRate(ZoneNum);
            if (CoolLoad >= state.dataRoomAirMod->RoomAirPattern(PattrnID).TwoGradPatrn.UpperBoundHeatRateScale) {
                Grad = state.dataRoomAirMod->RoomAirPattern(PattrnID).TwoGradPatrn.HiGradient;

            } else if (CoolLoad <= state.dataRoomAirMod->RoomAirPattern(PattrnID).TwoGradPatrn.LowerBoundHeatRateScale) {

                Grad = state.dataRoomAirMod->RoomAirPattern(PattrnID).TwoGradPatrn.LowGradient;
            } else { // interpolate
                if ((state.dataRoomAirMod->RoomAirPattern(PattrnID).TwoGradPatrn.UpperBoundHeatRateScale -
                     state.dataRoomAirMod->RoomAirPattern(PattrnID).TwoGradPatrn.LowerBoundHeatRateScale) == 0.0) {
                    Grad = state.dataRoomAirMod->RoomAirPattern(PattrnID).TwoGradPatrn.LowGradient;
                } else {

                    Grad = state.dataRoomAirMod->RoomAirPattern(PattrnID).TwoGradPatrn.LowGradient +
                           ((CoolLoad - state.dataRoomAirMod->RoomAirPattern(PattrnID).TwoGradPatrn.LowerBoundHeatRateScale) /
                            (state.dataRoomAirMod->RoomAirPattern(PattrnID).TwoGradPatrn.UpperBoundHeatRateScale -
                             state.dataRoomAirMod->RoomAirPattern(PattrnID).TwoGradPatrn.LowerBoundHeatRateScale)) *
                               (state.dataRoomAirMod->RoomAirPattern(PattrnID).TwoGradPatrn.HiGradient -
                                state.dataRoomAirMod->RoomAirPattern(PattrnID).TwoGradPatrn.LowGradient);
                }
            }

        } else if (SELECT_CASE_var == DataRoomAirModel::UserDefinedPatternMode::SensibleHeatingMode) {

            HeatLoad = state.dataHeatBal->SNLoadHeatRate(ZoneNum);
            if (HeatLoad >= state.dataRoomAirMod->RoomAirPattern(PattrnID).TwoGradPatrn.UpperBoundHeatRateScale) {
                Grad = state.dataRoomAirMod->RoomAirPattern(PattrnID).TwoGradPatrn.HiGradient;

            } else if (HeatLoad <= state.dataRoomAirMod->RoomAirPattern(PattrnID).TwoGradPatrn.LowerBoundHeatRateScale) {

                Grad = state.dataRoomAirMod->RoomAirPattern(PattrnID).TwoGradPatrn.LowGradient;
            } else { // interpolate
                if ((state.dataRoomAirMod->RoomAirPattern(PattrnID).TwoGradPatrn.UpperBoundHeatRateScale -
                     state.dataRoomAirMod->RoomAirPattern(PattrnID).TwoGradPatrn.LowerBoundHeatRateScale) == 0.0) {
                    Grad = state.dataRoomAirMod->RoomAirPattern(PattrnID).TwoGradPatrn.LowGradient;
                } else {

                    Grad = state.dataRoomAirMod->RoomAirPattern(PattrnID).TwoGradPatrn.LowGradient +
                           ((HeatLoad - state.dataRoomAirMod->RoomAirPattern(PattrnID).TwoGradPatrn.LowerBoundHeatRateScale) /
                            (state.dataRoomAirMod->RoomAirPattern(PattrnID).TwoGradPatrn.UpperBoundHeatRateScale -
                             state.dataRoomAirMod->RoomAirPattern(PattrnID).TwoGradPatrn.LowerBoundHeatRateScale)) *
                               (state.dataRoomAirMod->RoomAirPattern(PattrnID).TwoGradPatrn.HiGradient -
                                state.dataRoomAirMod->RoomAirPattern(PattrnID).TwoGradPatrn.LowGradient);
                }
            }
        }
    }

    ZetaTmean = 0.5; // by definition,

    for (i = 1; i <= state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).totNumSurfs; ++i) {
        thisZeta = state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).Surf(i).Zeta;

        DeltaHeight = -1.0 * (ZetaTmean - thisZeta) * state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).ZoneHeight;

        tempDeltaTai = DeltaHeight * Grad;

        state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).Surf(i).TadjacentAir = tempDeltaTai + Tmean;
    }

    state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).Tstat = -1.0 *
                                                                  (0.5 * state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).ZoneHeight -
                                                                   state.dataRoomAirMod->RoomAirPattern(PattrnID).TwoGradPatrn.TstatHeight) *
                                                                  Grad +
                                                              Tmean;
    state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).Tleaving = -1.0 *
                                                                     (0.5 * state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).ZoneHeight -
                                                                      state.dataRoomAirMod->RoomAirPattern(PattrnID).TwoGradPatrn.TleavingHeight) *
                                                                     Grad +
                                                                 Tmean;
    state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).Texhaust = -1.0 *
                                                                     (0.5 * state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).ZoneHeight -
                                                                      state.dataRoomAirMod->RoomAirPattern(PattrnID).TwoGradPatrn.TexhaustHeight) *
                                                                     Grad +
                                                                 Tmean;

    state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).Gradient = Grad;
}
Real64 OutdoorDryBulbGrad(Real64 DryBulbTemp, // Zone(ZoneNum).OutDryBulbTemp
                          Real64 UpperBound,  // RoomAirPattern(PattrnID).TwoGradPatrn.UpperBoundTempScale
                          Real64 HiGradient,  // RoomAirPattern(PattrnID).TwoGradPatrn.HiGradient
                          Real64 LowerBound,  // RoomAirPattern(PattrnID).TwoGradPatrn.LowerBoundTempScale
                          Real64 LowGradient  // RoomAirPattern(PattrnID).TwoGradPatrn.LowGradient
)
{
    Real64 Grad;
    if (DryBulbTemp >= UpperBound) {
        Grad = HiGradient;

    } else if (DryBulbTemp <= LowerBound) {

        Grad = LowGradient;
    } else { // interpolate

        if ((UpperBound - LowerBound) == 0.0) {
            // bad user input. should be trapped during get input in RoomAirManager.cc
            Grad = LowGradient;
        } else {

            Grad = LowGradient + ((DryBulbTemp - LowerBound) / (UpperBound - LowerBound)) * (HiGradient - LowGradient);
        }
    }
    return Grad;
}

void FigureConstGradPattern(EnergyPlusData &state, int const PattrnID, int const ZoneNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         B. Griffith
    //       DATE WRITTEN   August 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    Real64 Tmean;        // MAT
    int i;               // loop counter
    Real64 Grad;         // vertical temperature gradient
    Real64 ZetaTmean;    // non-dimens. height for MAT, 0.5
    Real64 thisZeta;     // temporary non-dimens height
    Real64 DeltaHeight;  // temporary height difference
    Real64 tempDeltaTai; // temporary Delta Tai

    Tmean = state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).TairMean;
    Grad = state.dataRoomAirMod->RoomAirPattern(PattrnID).GradPatrn.Gradient;

    ZetaTmean = 0.5; // by definition,

    for (i = 1; i <= state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).totNumSurfs; ++i) {
        thisZeta = state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).Surf(i).Zeta;
        DeltaHeight = -1.0 * (ZetaTmean - thisZeta) * state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).ZoneHeight;
        tempDeltaTai = DeltaHeight * Grad;
        state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).Surf(i).TadjacentAir = tempDeltaTai + Tmean;
    }

    state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).Tstat = state.dataRoomAirMod->RoomAirPattern(PattrnID).DeltaTstat + Tmean;
    state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).Tleaving = state.dataRoomAirMod->RoomAirPattern(PattrnID).DeltaTleaving + Tmean;
    state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).Texhaust = state.dataRoomAirMod->RoomAirPattern(PattrnID).DeltaTexhaust + Tmean;
}

//*****************************************************************************************

Real64 FigureNDheightInZone(EnergyPlusData &state, int const thisHBsurf) // index in main Surface array
{
    // FUNCTION INFORMATION:
    //       AUTHOR         B.Griffith
    //       DATE WRITTEN   aug 2005, Jan2004
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // return a non-dimensional height zeta

    // METHODOLOGY EMPLOYED:
    // figure average floor height (follows code in surfacegeometry.cc
    // use ceiling height from Zone structure
    // non dimensionalize surface's centroid's Z value

    // Using/Aliasing
    using DataVectorTypes::Vector;

    // Return value
    Real64 FigureNDheightInZone;

    // FUNCTION PARAMETER DEFINITIONS:
    Real64 const TolValue(0.0001);

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    int thisZone;
    Real64 ZoneZorig;
    Real64 ZoneCeilHeight;
    Real64 Zcm;
    Real64 SurfMinZ;
    Real64 SurfMaxZ;
    Real64 Zeta;
    Real64 FloorCount;
    Real64 ZFlrAvg;
    Real64 ZMax;
    Real64 ZMin;
    int Count;
    int SurfNum;
    Real64 Z1;
    Real64 Z2;

    // Get the centroid height for the surface
    Zcm = state.dataSurface->Surface(thisHBsurf).Centroid.z;
    thisZone = state.dataSurface->Surface(thisHBsurf).Zone;

    // this next Do block is copied from SurfaceGeometry.cc with modification for just floor Z
    // used find floor z.
    FloorCount = 0.0;
    ZFlrAvg = 0.0;
    ZMax = 0.0;
    ZMin = 0.0;
    Count = 0;
    for (SurfNum = state.dataHeatBal->Zone(thisZone).HTSurfaceFirst; SurfNum <= state.dataHeatBal->Zone(thisZone).HTSurfaceLast; ++SurfNum) {
        if (state.dataSurface->Surface(SurfNum).Class == DataSurfaces::SurfaceClass::Floor) {
            // Use Average Z for surface, more important for roofs than floors...
            ++FloorCount;
            Z1 = minval(state.dataSurface->Surface(SurfNum).Vertex({1, state.dataSurface->Surface(SurfNum).Sides}), &Vector::z);
            Z2 = maxval(state.dataSurface->Surface(SurfNum).Vertex({1, state.dataSurface->Surface(SurfNum).Sides}), &Vector::z);
            ZFlrAvg += (Z1 + Z2) / 2.0;
        }
        if (state.dataSurface->Surface(SurfNum).Class == DataSurfaces::SurfaceClass::Wall) {
            // Use Wall calculation in case no floor in zone
            ++Count;
            if (Count == 1) {
                ZMax = state.dataSurface->Surface(SurfNum).Vertex(1).z;
                ZMin = ZMax;
            }
            ZMax = max(ZMax, maxval(state.dataSurface->Surface(SurfNum).Vertex({1, state.dataSurface->Surface(SurfNum).Sides}), &Vector::z));
            ZMin = min(ZMin, minval(state.dataSurface->Surface(SurfNum).Vertex({1, state.dataSurface->Surface(SurfNum).Sides}), &Vector::z));
        }
    }
    if (FloorCount > 0.0) {
        ZFlrAvg /= FloorCount;
    } else {
        ZFlrAvg = ZMin;
    }
    ZoneZorig = ZFlrAvg; // Z floor  [M]
    ZoneCeilHeight = state.dataHeatBal->Zone(thisZone).CeilingHeight;

    // first check if some basic things are reasonable

    SurfMinZ = minval(state.dataSurface->Surface(thisHBsurf).Vertex, &Vector::z);
    SurfMaxZ = maxval(state.dataSurface->Surface(thisHBsurf).Vertex, &Vector::z);

    if (SurfMinZ < (ZoneZorig - TolValue)) {
        if (state.dataGlobal->DisplayExtraWarnings) {
            ShowWarningError(state, "RoomAirModelUserTempPattern: Problem in non-dimensional height calculation");
            ShowContinueError(
                state, "too low surface: " + state.dataSurface->Surface(thisHBsurf).Name + " in zone: " + state.dataHeatBal->Zone(thisZone).Name);
            ShowContinueError(state, format("**** Average floor height of zone is: {:.3R}", ZoneZorig));
            ShowContinueError(state, format("**** Surface minimum height is: {:.3R}", SurfMinZ));
        } else {
            ++state.dataErrTracking->TotalRoomAirPatternTooLow;
        }
    }

    if (SurfMaxZ > (ZoneZorig + ZoneCeilHeight + TolValue)) {
        if (state.dataGlobal->DisplayExtraWarnings) {
            ShowWarningError(state, "RoomAirModelUserTempPattern: Problem in non-dimensional height calculation");
            ShowContinueError(
                state, " too high surface: " + state.dataSurface->Surface(thisHBsurf).Name + " in zone: " + state.dataHeatBal->Zone(thisZone).Name);
            ShowContinueError(state, format("**** Average Ceiling height of zone is: {:.3R}", (ZoneZorig + ZoneCeilHeight)));
            ShowContinueError(state, format("**** Surface Maximum height is: {:.3R}", SurfMaxZ));
        } else {
            ++state.dataErrTracking->TotalRoomAirPatternTooHigh;
        }
    }

    // non dimensionalize.
    Zeta = (Zcm - ZoneZorig) / ZoneCeilHeight;
    // bound so that floors and ceiling are just in from endpoints.

    if (Zeta > 0.99) Zeta = 0.99;

    if (Zeta < 0.01) Zeta = 0.01;

    FigureNDheightInZone = Zeta;

    return FigureNDheightInZone;
}

//***************************************************

void SetSurfHBDataForTempDistModel(EnergyPlusData &state, int const ZoneNum) // index number for the specified zone
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   August 2005,Feb. 2004
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    //  map data from air domain back to surface domain for each zone
    //  collects code couples to remote data structures

    // METHODOLOGY EMPLOYED:
    // sets values in Heat balance variables

    // Using/Aliasing
    using DataHVACGlobals::RetTempMax;
    using DataHVACGlobals::RetTempMin;
    using DataSurfaces::AdjacentAirTemp;
    using DataSurfaces::AirFlowWindow_Destination_ReturnAir;
    using InternalHeatGains::SumAllReturnAirConvectionGains;
    using InternalHeatGains::SumAllReturnAirLatentGains;
    using Psychrometrics::PsyCpAirFnW;
    using Psychrometrics::PsyHFnTdbW;
    using Psychrometrics::PsyHgAirFnWTdb;
    using Psychrometrics::PsyRhoAirFnPbTdbW;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int SurfFirst; // index number of the first surface in the zone
    int SurfLast;
    Real64 QRetAir;         // Heat to return air from lights
    Real64 CpAir;           // Air heat capacity [J/kg-K]
    Real64 TempRetAir;      // Return air temperature [C]
    Real64 TempZoneAir;     // Zone air temperature [C]
    int ZoneNode;           // Node number of controlled zone
    int SurfNum;            // Surface number
    Real64 MassFlowRA;      // Return air mass flow [kg/s]
    Real64 FlowThisTS;      // Window gap air mass flow [kg/s]
    Real64 WinGapFlowToRA;  // Mass flow to return air from all airflow windows in zone [kg/s]
    Real64 WinGapFlowTtoRA; // Sum of mass flow times outlet temp for all airflow windows in zone [(kg/s)-C]
    Real64 WinGapTtoRA;     // Temp of outlet flow mixture to return air from all airflow windows in zone [C]
    Real64 H2OHtOfVap;      // Heat of vaporization of water (W/kg)
    Real64 RhoAir;          // Density of air (Kg/m3)
    Real64 ZoneMult;
    Real64 SumRetAirLatentGainRate;

    SurfFirst = state.dataHeatBal->Zone(ZoneNum).HTSurfaceFirst;
    SurfLast = state.dataHeatBal->Zone(ZoneNum).HTSurfaceLast;

    // set air system leaving node conditions
    // this is not so easy.  THis task is normally done in CalcZoneLeavingConditions
    //  but efforts to do this update there were not succesful.
    //  Need to revisit how to best implement this. Ended up taking code from CalcZoneLeavingConditions
    //  ZoneNum is already equal to ActualZoneNum , changed block of source

    if (state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).ZoneNodeID != 0) {
        // the zone system node should get the conditions leaving the zone (but before return air heat gains are added).
        state.dataLoopNodes->Node(state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).ZoneNodeID).Temp =
            state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).Tleaving;
    }

    int zoneEquipNum = state.dataHeatBal->Zone(ZoneNum).ZoneEqNum;
    for (int nodeCount = 1; nodeCount <= state.dataZoneEquip->ZoneEquipConfig(zoneEquipNum).NumReturnNodes; ++nodeCount) {
        // BEGIN BLOCK of code from CalcZoneLeavingConditions*********************************
        int ReturnNode = state.dataZoneEquip->ZoneEquipConfig(zoneEquipNum).ReturnNode(nodeCount);
        ZoneNode = state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).ZoneNodeID;
        ZoneMult = state.dataHeatBal->Zone(ZoneNum).Multiplier * state.dataHeatBal->Zone(ZoneNum).ListMultiplier;
        // RETURN AIR HEAT GAIN from the Lights statement; this heat gain is stored in
        // Add sensible heat gain from refrigerated cases with under case returns
        SumAllReturnAirConvectionGains(state, ZoneNum, QRetAir, ReturnNode);

        CpAir = PsyCpAirFnW(state.dataLoopNodes->Node(ZoneNode).HumRat);

        // Need to add the energy to the return air from lights and from airflow windows. Where the heat
        // is added depends on if there is system flow or not.  If there is system flow the heat is added
        // to the Zone Return Node.  If there is no system flow then the heat is added back to the zone in the
        // Correct step through the SysDepZoneLoads variable.

        MassFlowRA = state.dataLoopNodes->Node(ReturnNode).MassFlowRate / ZoneMult;
        TempZoneAir = state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).Tleaving; // key difference from
        TempRetAir = TempZoneAir;
        WinGapFlowToRA = 0.0;
        WinGapTtoRA = 0.0;
        WinGapFlowTtoRA = 0.0;

        if (state.dataZoneEquip->ZoneEquipConfig(zoneEquipNum).ZoneHasAirFlowWindowReturn) {
            for (SurfNum = state.dataHeatBal->Zone(ZoneNum).HTSurfaceFirst; SurfNum <= state.dataHeatBal->Zone(ZoneNum).HTSurfaceLast; ++SurfNum) {
                if (state.dataSurface->SurfWinAirflowThisTS(SurfNum) > 0.0 &&
                    state.dataSurface->SurfWinAirflowDestination(SurfNum) == AirFlowWindow_Destination_ReturnAir) {
                    FlowThisTS = PsyRhoAirFnPbTdbW(state,
                                                   state.dataEnvrn->OutBaroPress,
                                                   state.dataSurface->SurfWinTAirflowGapOutlet(SurfNum),
                                                   state.dataLoopNodes->Node(ZoneNode).HumRat) *
                                 state.dataSurface->SurfWinAirflowThisTS(SurfNum) * state.dataSurface->Surface(SurfNum).Width;
                    WinGapFlowToRA += FlowThisTS;
                    WinGapFlowTtoRA += FlowThisTS * state.dataSurface->SurfWinTAirflowGapOutlet(SurfNum);
                }
            }
        }
        if (WinGapFlowToRA > 0.0) WinGapTtoRA = WinGapFlowTtoRA / WinGapFlowToRA;

        if (!state.dataHeatBal->Zone(ZoneNum).NoHeatToReturnAir) {
            if (MassFlowRA > 0.0) {
                if (WinGapFlowToRA > 0.0) {
                    // Add heat-to-return from window gap airflow
                    if (MassFlowRA >= WinGapFlowToRA) {
                        TempRetAir = (WinGapFlowTtoRA + (MassFlowRA - WinGapFlowToRA) * TempZoneAir) / MassFlowRA;
                    } else {
                        // All of return air comes from flow through airflow windows
                        TempRetAir = WinGapTtoRA;
                        // Put heat from window airflow that exceeds return air flow into zone air
                        state.dataHeatBalFanSys->SysDepZoneLoads(ZoneNum) += (WinGapFlowToRA - MassFlowRA) * CpAir * (WinGapTtoRA - TempZoneAir);
                    }
                }
                // Add heat-to-return from lights
                TempRetAir += QRetAir / (MassFlowRA * CpAir);
                if (TempRetAir > RetTempMax) {
                    state.dataLoopNodes->Node(ReturnNode).Temp = RetTempMax;
                    if (!state.dataGlobal->ZoneSizingCalc) {
                        state.dataHeatBalFanSys->SysDepZoneLoads(ZoneNum) += CpAir * MassFlowRA * (TempRetAir - RetTempMax);
                    }
                } else if (TempRetAir < RetTempMin) {
                    state.dataLoopNodes->Node(ReturnNode).Temp = RetTempMin;
                    if (!state.dataGlobal->ZoneSizingCalc) {
                        state.dataHeatBalFanSys->SysDepZoneLoads(ZoneNum) += CpAir * MassFlowRA * (TempRetAir - RetTempMin);
                    }
                } else {
                    state.dataLoopNodes->Node(ReturnNode).Temp = TempRetAir;
                }
            } else { // No return air flow
                // Assign all heat-to-return from window gap airflow to zone air
                if (WinGapFlowToRA > 0.0) state.dataHeatBalFanSys->SysDepZoneLoads(ZoneNum) += WinGapFlowToRA * CpAir * (WinGapTtoRA - TempZoneAir);
                // Assign all heat-to-return from lights to zone air
                if (QRetAir > 0.0) state.dataHeatBalFanSys->SysDepZoneLoads(ZoneNum) += QRetAir;
                state.dataLoopNodes->Node(ReturnNode).Temp = state.dataLoopNodes->Node(ZoneNode).Temp;
            }
        } else {
            state.dataLoopNodes->Node(ReturnNode).Temp = state.dataLoopNodes->Node(ZoneNode).Temp;
        }

        // Update the rest of the Return Air Node conditions, if the return air system exists!
        state.dataLoopNodes->Node(ReturnNode).Press = state.dataLoopNodes->Node(ZoneNode).Press;

        H2OHtOfVap = PsyHgAirFnWTdb(state.dataLoopNodes->Node(ZoneNode).HumRat, state.dataLoopNodes->Node(ReturnNode).Temp);
        RhoAir = PsyRhoAirFnPbTdbW(
            state, state.dataEnvrn->OutBaroPress, state.dataLoopNodes->Node(ReturnNode).Temp, state.dataLoopNodes->Node(ZoneNode).HumRat);

        // Include impact of under case returns for refrigerated display cases when updateing return node
        // humidity ratio
        if (!state.dataHeatBal->Zone(ZoneNum).NoHeatToReturnAir) {
            if (MassFlowRA > 0) {
                SumAllReturnAirLatentGains(state, ZoneNum, SumRetAirLatentGainRate, ReturnNode);
                state.dataLoopNodes->Node(ReturnNode).HumRat =
                    state.dataLoopNodes->Node(ZoneNode).HumRat + (SumRetAirLatentGainRate / (H2OHtOfVap * MassFlowRA));
            } else {
                // If no mass flow rate exists, include the latent HVAC case credit with the latent Zone case credit
                state.dataLoopNodes->Node(ReturnNode).HumRat = state.dataLoopNodes->Node(ZoneNode).HumRat;
                state.dataHeatBal->RefrigCaseCredit(ZoneNum).LatCaseCreditToZone += state.dataHeatBal->RefrigCaseCredit(ZoneNum).LatCaseCreditToHVAC;
                // shouldn't the HVAC term be zeroed out then?
                SumAllReturnAirLatentGains(state, ZoneNum, SumRetAirLatentGainRate, 0);
                state.dataHeatBalFanSys->ZoneLatentGain(ZoneNum) += SumRetAirLatentGainRate;
            }
        } else {
            state.dataLoopNodes->Node(ReturnNode).HumRat = state.dataLoopNodes->Node(ZoneNode).HumRat;
            state.dataHeatBal->RefrigCaseCredit(ZoneNum).LatCaseCreditToZone += state.dataHeatBal->RefrigCaseCredit(ZoneNum).LatCaseCreditToHVAC;
            // shouldn't the HVAC term be zeroed out then?
            SumAllReturnAirLatentGains(state, ZoneNum, SumRetAirLatentGainRate, ReturnNode);
            state.dataHeatBalFanSys->ZoneLatentGain(ZoneNum) += SumRetAirLatentGainRate;
        }

        state.dataLoopNodes->Node(ReturnNode).Enthalpy =
            PsyHFnTdbW(state.dataLoopNodes->Node(ReturnNode).Temp, state.dataLoopNodes->Node(ReturnNode).HumRat);

        // END BLOCK of code from CalcZoneLeavingConditions*********************************
    }

    // set exhaust node leaving temp if present
    if (allocated(state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).ExhaustAirNodeID)) {
        auto const &APZoneInfo(state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum));
        auto const &EANodeID(APZoneInfo.ExhaustAirNodeID);
        Real64 const Texhaust(APZoneInfo.Texhaust);
        for (int i = 1, ie = EANodeID.u(); i <= ie; ++i) {
            state.dataLoopNodes->Node(EANodeID(i)).Temp = Texhaust;
        }
    }

    // set thermostat reading for air system .
    state.dataHeatBalFanSys->TempTstatAir(ZoneNum) = state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).Tstat;

    // set results for all surface
    for (int i = SurfFirst, j = 1; i <= SurfLast; ++i, ++j) {
        state.dataHeatBal->SurfTempEffBulkAir(i) = state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).Surf(j).TadjacentAir;
    }

    // set flag for reference air temperature mode
    for (int i = SurfFirst; i <= SurfLast; ++i) {
        state.dataSurface->SurfTAirRef(i) = AdjacentAirTemp;
    }
}

//*****************************************************************************************

} // namespace EnergyPlus::RoomAirModelUserTempPattern
