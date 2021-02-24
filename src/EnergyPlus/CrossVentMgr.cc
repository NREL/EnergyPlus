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
#include <cassert>
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EnergyPlus/AirflowNetworkBalanceManager.hh>
#include <EnergyPlus/ConvectionCoefficients.hh>
#include <EnergyPlus/CrossVentMgr.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalSurface.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataRoomAirModel.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataUCSDSharedData.hh>
#include <EnergyPlus/InternalHeatGains.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

namespace CrossVentMgr {

    // MODULE INFORMATION:
    //       AUTHOR         G. Carrilho da Graca
    //       DATE WRITTEN   October 2004
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // Routines that implement the UCSD Cross Ventilation

    using namespace DataEnvironment;
    using namespace DataHeatBalance;
    using namespace DataHeatBalSurface;
    using namespace DataSurfaces;
    using namespace DataRoomAirModel;
    using ConvectionCoefficients::CalcDetailedHcInForDVModel;
    using namespace DataUCSDSharedData;

    Real64 constexpr Cjet1(1.873);     // First correlation constant for the jet velocity
    Real64 constexpr Cjet2(0.243);     // Second correlation constant for the jet velocity
    Real64 constexpr Crec1(0.591);     // First correlation constant for the recirculation velocity
    Real64 constexpr Crec2(0.070);     // Second correlation constant for the recirculation velocity
    Real64 constexpr CjetTemp(0.849);  // Correlation constant for the jet temperature rise
    Real64 constexpr CrecTemp(1.385);  // Correlation constant for the recirculation temperature rise
    Real64 constexpr CrecFlow1(0.415); // First correlation constant for the recirculation flow rate
    Real64 constexpr CrecFlow2(0.466); // Second correlation constant for the recirculation flow rate

    void ManageUCSDCVModel(EnergyPlusData &state,
                           int const ZoneNum) // index number for the specified zone
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         G. Carrilho da Graca
        //       DATE WRITTEN   October 2004
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        //   manage the UCSD Cross Ventilation model

        using DataHeatBalSurface::TempSurfIn;

        InitUCSDCV(state, ZoneNum);

        // perform Cross Ventilation model calculations
        CalcUCSDCV(state, ZoneNum);
    }

    void InitUCSDCV(EnergyPlusData &state, int const ZoneNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         G. Carrilho da Graca
        //       DATE WRITTEN   October 2004
        //       MODIFIED       -
        //       RE-ENGINEERED  -

        // PURPOSE OF THIS SUBROUTINE:
        // Low Energy Cooling by Ventilation initialization subroutine.
        // All the data preparation needed to run the LECV models.
        // The subroutines sets up arrays with the locations in the main EnergyPlus surface array of
        // ceiling, windows, doors and walls. The zone maximum and minimum height is calculated.

        using namespace DataRoomAirModel;

        // Do the one time initializations
        if (state.dataCrossVentMgr->InitUCSDCV_MyOneTimeFlag) {
            state.dataCrossVentMgr->InitUCSDCV_MyEnvrnFlag.dimension(state.dataGlobal->NumOfZones, true);
            state.dataCrossVentMgr->InitUCSDCV_MyOneTimeFlag = false;
        }

        // Do the begin environment initializations
        if (state.dataGlobal->BeginEnvrnFlag && state.dataCrossVentMgr->InitUCSDCV_MyEnvrnFlag(ZoneNum)) {
            state.dataCrossVentMgr->InitUCSDCV_MyEnvrnFlag(ZoneNum) = false;
        }

        if (!state.dataGlobal->BeginEnvrnFlag) {
            state.dataCrossVentMgr->InitUCSDCV_MyEnvrnFlag(ZoneNum) = true;
        }
    }

    void HcUCSDCV(EnergyPlusData &state,
                  int const ZoneNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         G. Carrilho da Graca
        //       DATE WRITTEN   October 2004
        //       MODIFIED       8/2013 - Sam Brunswick
        //                      To improve convection coefficient calculation
        //       RE-ENGINEERED  -

        // PURPOSE OF THIS SUBROUTINE:
        // Main subroutine for convection calculation in the UCSD Cross Ventilation model.
        // It calls CalcDetailedHcInForDVModel for convection coefficient
        // initial calculations and averages the final result comparing the position of the surface with
        // the interface subzone height.

        using namespace DataHeatBalFanSys;
        using namespace DataEnvironment;
        using namespace DataHeatBalance;
        using ScheduleManager::GetScheduleIndex; // , GetDayScheduleValues

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Ctd;     // DO loop counter for surfaces
        int SurfNum; // Surface number
        Real64 Hjet;
        Real64 Hrec;

        // Initialize HAT and HA
        state.dataCrossVentMgr->HAT_J = 0.0;
        state.dataCrossVentMgr->HAT_R = 0.0;
        state.dataCrossVentMgr->HA_J = 0.0;
        state.dataCrossVentMgr->HA_R = 0.0;

        // Is the air flow model for this zone set to UCSDCV Cross Ventilation?
        if (state.dataRoomAirMod->IsZoneCV(ZoneNum)) {
            // WALL Hc, HA and HAT calculation
            for (Ctd = PosZ_Wall((ZoneNum - 1) * 2 + 1); Ctd <= PosZ_Wall((ZoneNum - 1) * 2 + 2); ++Ctd) {
                SurfNum = APos_Wall(Ctd);
                Surface(SurfNum).TAirRef = AdjacentAirTemp;
                if (SurfNum == 0) continue;
                state.dataHeatBal->TempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTREC(ZoneNum);
                CalcDetailedHcInForDVModel(state, SurfNum, TempSurfIn, state.dataRoomAirMod->CVHcIn, state.dataRoomAirMod->Urec);
                HWall(Ctd) = state.dataRoomAirMod->CVHcIn(SurfNum);
                state.dataCrossVentMgr->HAT_R += Surface(SurfNum).Area * TempSurfIn(SurfNum) * HWall(Ctd);
                state.dataCrossVentMgr->HA_R += Surface(SurfNum).Area * HWall(Ctd);
            } // END WALL
            // WINDOW Hc, HA and HAT CALCULATION
            for (Ctd = PosZ_Window((ZoneNum - 1) * 2 + 1); Ctd <= PosZ_Window((ZoneNum - 1) * 2 + 2); ++Ctd) {
                SurfNum = APos_Window(Ctd);
                Surface(SurfNum).TAirRef = AdjacentAirTemp;
                if (SurfNum == 0) continue;
                if (Surface(SurfNum).Tilt > 10.0 && Surface(SurfNum).Tilt < 170.0) { // Window Wall
                    state.dataHeatBal->TempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTREC(ZoneNum);
                    CalcDetailedHcInForDVModel(state, SurfNum, TempSurfIn, state.dataRoomAirMod->CVHcIn, state.dataRoomAirMod->Urec);
                    HWindow(Ctd) = state.dataRoomAirMod->CVHcIn(SurfNum);
                    state.dataCrossVentMgr->HAT_R += Surface(SurfNum).Area * TempSurfIn(SurfNum) * HWindow(Ctd);
                    state.dataCrossVentMgr->HA_R += Surface(SurfNum).Area * HWindow(Ctd);
                }
                if (Surface(SurfNum).Tilt <= 10.0) { // Window Ceiling
                    state.dataHeatBal->TempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTJET(ZoneNum);
                    CalcDetailedHcInForDVModel(state, SurfNum, TempSurfIn, state.dataRoomAirMod->CVHcIn, state.dataRoomAirMod->Ujet);
                    Hjet = state.dataRoomAirMod->CVHcIn(SurfNum);
                    state.dataHeatBal->TempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTREC(ZoneNum);
                    CalcDetailedHcInForDVModel(state, SurfNum, TempSurfIn, state.dataRoomAirMod->CVHcIn, state.dataRoomAirMod->Urec);
                    Hrec = state.dataRoomAirMod->CVHcIn(SurfNum);
                    HWindow(Ctd) = state.dataRoomAirMod->JetRecAreaRatio(ZoneNum) * Hjet + (1 - state.dataRoomAirMod->JetRecAreaRatio(ZoneNum)) * Hrec;
                    state.dataCrossVentMgr->HAT_R += Surface(SurfNum).Area * (1.0 - state.dataRoomAirMod->JetRecAreaRatio(ZoneNum)) * TempSurfIn(SurfNum) * Hrec;
                    state.dataCrossVentMgr->HA_R += Surface(SurfNum).Area * (1.0 - state.dataRoomAirMod->JetRecAreaRatio(ZoneNum)) * Hrec;
                    state.dataCrossVentMgr->HAT_J += Surface(SurfNum).Area * state.dataRoomAirMod->JetRecAreaRatio(ZoneNum) * TempSurfIn(SurfNum) * Hjet;
                    state.dataCrossVentMgr->HA_J += Surface(SurfNum).Area * state.dataRoomAirMod->JetRecAreaRatio(ZoneNum) * Hjet;
                    state.dataHeatBal->TempEffBulkAir(SurfNum) = state.dataRoomAirMod->JetRecAreaRatio(ZoneNum) * state.dataRoomAirMod->ZTJET(ZoneNum) + (1 - state.dataRoomAirMod->JetRecAreaRatio(ZoneNum)) * state.dataRoomAirMod->ZTREC(ZoneNum);
                }
                if (Surface(SurfNum).Tilt >= 170.0) { // Window Floor
                    state.dataHeatBal->TempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTJET(ZoneNum);
                    CalcDetailedHcInForDVModel(state, SurfNum, TempSurfIn, state.dataRoomAirMod->CVHcIn, state.dataRoomAirMod->Ujet);
                    Hjet = state.dataRoomAirMod->CVHcIn(SurfNum);
                    state.dataHeatBal->TempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTREC(ZoneNum);
                    CalcDetailedHcInForDVModel(state, SurfNum, TempSurfIn, state.dataRoomAirMod->CVHcIn, state.dataRoomAirMod->Urec);
                    Hrec = state.dataRoomAirMod->CVHcIn(SurfNum);
                    HWindow(Ctd) = state.dataRoomAirMod->JetRecAreaRatio(ZoneNum) * Hjet + (1 - state.dataRoomAirMod->JetRecAreaRatio(ZoneNum)) * Hrec;
                    state.dataCrossVentMgr->HAT_R += Surface(SurfNum).Area * (1.0 - state.dataRoomAirMod->JetRecAreaRatio(ZoneNum)) * TempSurfIn(SurfNum) * Hrec;
                    state.dataCrossVentMgr->HA_R += Surface(SurfNum).Area * (1.0 - state.dataRoomAirMod->JetRecAreaRatio(ZoneNum)) * Hrec;
                    state.dataCrossVentMgr->HAT_J += Surface(SurfNum).Area * state.dataRoomAirMod->JetRecAreaRatio(ZoneNum) * TempSurfIn(SurfNum) * Hjet;
                    state.dataCrossVentMgr->HA_J += Surface(SurfNum).Area * state.dataRoomAirMod->JetRecAreaRatio(ZoneNum) * Hjet;
                    state.dataHeatBal->TempEffBulkAir(SurfNum) = state.dataRoomAirMod->JetRecAreaRatio(ZoneNum) * state.dataRoomAirMod->ZTJET(ZoneNum) + (1 - state.dataRoomAirMod->JetRecAreaRatio(ZoneNum)) * state.dataRoomAirMod->ZTREC(ZoneNum);
                }
                state.dataRoomAirMod->CVHcIn(SurfNum) = HWindow(Ctd);
            } // END WINDOW
            // DOOR Hc, HA and HAT CALCULATION
            for (Ctd = PosZ_Door((ZoneNum - 1) * 2 + 1); Ctd <= PosZ_Door((ZoneNum - 1) * 2 + 2); ++Ctd) { // DOOR
                SurfNum = APos_Door(Ctd);
                Surface(SurfNum).TAirRef = AdjacentAirTemp;
                if (SurfNum == 0) continue;
                state.dataHeatBal->TempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTREC(ZoneNum);
                CalcDetailedHcInForDVModel(state, SurfNum, TempSurfIn, state.dataRoomAirMod->CVHcIn, state.dataRoomAirMod->Urec);
                HDoor(Ctd) = state.dataRoomAirMod->CVHcIn(SurfNum);
                state.dataCrossVentMgr->HAT_R += Surface(SurfNum).Area * TempSurfIn(SurfNum) * HDoor(Ctd);
                state.dataCrossVentMgr->HA_R += Surface(SurfNum).Area * HDoor(Ctd);
            } // END DOOR
            // INTERNAL Hc, HA and HAT CALCULATION
            for (Ctd = PosZ_Internal((ZoneNum - 1) * 2 + 1); Ctd <= PosZ_Internal((ZoneNum - 1) * 2 + 2); ++Ctd) {
                SurfNum = APos_Internal(Ctd);
                Surface(SurfNum).TAirRef = AdjacentAirTemp;
                if (SurfNum == 0) continue;
                state.dataHeatBal->TempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTREC(ZoneNum);
                CalcDetailedHcInForDVModel(state, SurfNum, TempSurfIn, state.dataRoomAirMod->CVHcIn, state.dataRoomAirMod->Urec);
                HInternal(Ctd) = state.dataRoomAirMod->CVHcIn(SurfNum);
                state.dataCrossVentMgr->HAT_R += Surface(SurfNum).Area * TempSurfIn(SurfNum) * HInternal(Ctd);
                state.dataCrossVentMgr->HA_R += Surface(SurfNum).Area * HInternal(Ctd);
            } // END INTERNAL

            // CEILING Hc, HA and HAT CALCULATION
            for (Ctd = PosZ_Ceiling((ZoneNum - 1) * 2 + 1); Ctd <= PosZ_Ceiling((ZoneNum - 1) * 2 + 2); ++Ctd) {
                SurfNum = APos_Ceiling(Ctd);
                Surface(SurfNum).TAirRef = AdjacentAirTemp;
                if (SurfNum == 0) continue;
                state.dataHeatBal->TempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTJET(ZoneNum);
                CalcDetailedHcInForDVModel(state, SurfNum, TempSurfIn, state.dataRoomAirMod->CVHcIn, state.dataRoomAirMod->Ujet);
                Hjet = state.dataRoomAirMod->CVHcIn(SurfNum);
                state.dataHeatBal->TempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTREC(ZoneNum);
                CalcDetailedHcInForDVModel(state, SurfNum, TempSurfIn, state.dataRoomAirMod->CVHcIn, state.dataRoomAirMod->Urec);
                Hrec = state.dataRoomAirMod->CVHcIn(SurfNum);
                HCeiling(Ctd) = state.dataRoomAirMod->JetRecAreaRatio(ZoneNum) * Hjet + (1 - state.dataRoomAirMod->JetRecAreaRatio(ZoneNum)) * Hrec;
                state.dataCrossVentMgr->HAT_R += Surface(SurfNum).Area * (1 - state.dataRoomAirMod->JetRecAreaRatio(ZoneNum)) * TempSurfIn(SurfNum) * Hrec;
                state.dataCrossVentMgr->HA_R += Surface(SurfNum).Area * (1 - state.dataRoomAirMod->JetRecAreaRatio(ZoneNum)) * Hrec;
                state.dataCrossVentMgr->HAT_J += Surface(SurfNum).Area * state.dataRoomAirMod->JetRecAreaRatio(ZoneNum) * TempSurfIn(SurfNum) * Hjet;
                state.dataCrossVentMgr->HA_J += Surface(SurfNum).Area * state.dataRoomAirMod->JetRecAreaRatio(ZoneNum) * Hjet;
                state.dataHeatBal->TempEffBulkAir(SurfNum) = state.dataRoomAirMod->JetRecAreaRatio(ZoneNum) * state.dataRoomAirMod->ZTJET(ZoneNum) + (1 - state.dataRoomAirMod->JetRecAreaRatio(ZoneNum)) * state.dataRoomAirMod->ZTREC(ZoneNum);
                state.dataRoomAirMod->CVHcIn(SurfNum) = HCeiling(Ctd);
            } // END CEILING
            // FLOOR Hc, HA and HAT CALCULATION
            for (Ctd = PosZ_Floor((ZoneNum - 1) * 2 + 1); Ctd <= PosZ_Floor((ZoneNum - 1) * 2 + 2); ++Ctd) {
                SurfNum = APos_Floor(Ctd);
                Surface(SurfNum).TAirRef = AdjacentAirTemp;
                if (SurfNum == 0) continue;
                state.dataHeatBal->TempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTJET(ZoneNum);
                CalcDetailedHcInForDVModel(state, SurfNum, TempSurfIn, state.dataRoomAirMod->CVHcIn, state.dataRoomAirMod->Ujet);
                Hjet = state.dataRoomAirMod->CVHcIn(SurfNum);
                state.dataHeatBal->TempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTREC(ZoneNum);
                CalcDetailedHcInForDVModel(state, SurfNum, TempSurfIn, state.dataRoomAirMod->CVHcIn, state.dataRoomAirMod->Urec);
                Hrec = state.dataRoomAirMod->CVHcIn(SurfNum);
                HFloor(Ctd) = state.dataRoomAirMod->JetRecAreaRatio(ZoneNum) * Hjet + (1 - state.dataRoomAirMod->JetRecAreaRatio(ZoneNum)) * Hrec;
                state.dataCrossVentMgr->HAT_R += Surface(SurfNum).Area * (1 - state.dataRoomAirMod->JetRecAreaRatio(ZoneNum)) * TempSurfIn(SurfNum) * Hrec;
                state.dataCrossVentMgr->HA_R += Surface(SurfNum).Area * (1 - state.dataRoomAirMod->JetRecAreaRatio(ZoneNum)) * Hrec;
                state.dataCrossVentMgr->HAT_J += Surface(SurfNum).Area * state.dataRoomAirMod->JetRecAreaRatio(ZoneNum) * TempSurfIn(SurfNum) * Hjet;
                state.dataCrossVentMgr->HA_J += Surface(SurfNum).Area * state.dataRoomAirMod->JetRecAreaRatio(ZoneNum) * Hjet;
                state.dataHeatBal->TempEffBulkAir(SurfNum) = state.dataRoomAirMod->JetRecAreaRatio(ZoneNum) * state.dataRoomAirMod->ZTJET(ZoneNum) + (1 - state.dataRoomAirMod->JetRecAreaRatio(ZoneNum)) * state.dataRoomAirMod->ZTREC(ZoneNum);
                state.dataRoomAirMod->CVHcIn(SurfNum) = HFloor(Ctd);
            } // END FLOOR
        }
    }

    void EvolveParaUCSDCV(EnergyPlusData &state, int const ZoneNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         G. Carrilho da Graca
        //       DATE WRITTEN   October 2004
        //       MODIFIED       8/2013 - Sam Brunswick
        //                      To incorporate an improved model
        //                      and add modeling of multiple jets
        //       RE-ENGINEERED  -

        // PURPOSE OF THIS SUBROUTINE:
        // Subroutine for parameter actualization in the UCSD Cross Ventilation model.

        using namespace Psychrometrics;
        using namespace DataHeatBalFanSys;

        Real64 const MinUin(0.2);

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Ctd;               // counter
        int Ctd2;              // counter
        int OPtr;              // counter
        Real64 Uin;            // Inflow air velocity [m/s]
        Real64 CosPhi;         // Angle (in degrees) between the wind and the outward normal of the dominant surface
        Real64 SurfNorm;       // Outward normal of surface
        Real64 SumToZone(0.0); // Sum of velocities through
        Real64 MaxFlux(0.0);
        int MaxSurf(0);
        Real64 XX;
        Real64 YY;
        Real64 ZZ;
        Real64 XX_Wall;
        Real64 YY_Wall;
        Real64 ZZ_Wall;
        Real64 ActiveSurfNum;
        int NSides;      // Number of sides in surface
        Real64 Wroom;    // Room width
        Real64 Aroom;    // Room area cross section
        int NodeNum1(0); // The first node number in an AirflowNetwork linkage data
        int NodeNum2(0); // The Second node number in an AirflowNetwork linkage data

        auto &Zone(state.dataHeatBal->Zone);

        state.dataRoomAirMod->RecInflowRatio(ZoneNum) = 0.0;

        // Identify the dominant aperture:
        MaxSurf = state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(1, ZoneNum);
        if (Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).Zone == ZoneNum) {
            // this is a direct airflow network aperture
            SumToZone = AirflowNetwork::AirflowNetworkLinkSimu(state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(1, ZoneNum)).VolFLOW2;
            MaxFlux = AirflowNetwork::AirflowNetworkLinkSimu(state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(1, ZoneNum)).VolFLOW2;
        } else {
            // this is an indirect airflow network aperture
            SumToZone = AirflowNetwork::AirflowNetworkLinkSimu(state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(1, ZoneNum)).VolFLOW;
            MaxFlux = AirflowNetwork::AirflowNetworkLinkSimu(state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(1, ZoneNum)).VolFLOW;
        }

        for (Ctd2 = 2; Ctd2 <= state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(0, ZoneNum); ++Ctd2) {
            if (Surface(AirflowNetwork::MultizoneSurfaceData(state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(Ctd2, ZoneNum)).SurfNum).Zone == ZoneNum) {
                if (AirflowNetwork::AirflowNetworkLinkSimu(state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(Ctd2, ZoneNum)).VolFLOW2 > MaxFlux) {
                    MaxFlux = AirflowNetwork::AirflowNetworkLinkSimu(state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(Ctd2, ZoneNum)).VolFLOW2;
                    MaxSurf = state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(Ctd2, ZoneNum);
                }
                SumToZone += AirflowNetwork::AirflowNetworkLinkSimu(state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(Ctd2, ZoneNum)).VolFLOW2;
            } else {
                if (AirflowNetwork::AirflowNetworkLinkSimu(state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(Ctd2, ZoneNum)).VolFLOW > MaxFlux) {
                    MaxFlux = AirflowNetwork::AirflowNetworkLinkSimu(state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(Ctd2, ZoneNum)).VolFLOW;
                    MaxSurf = state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(Ctd2, ZoneNum);
                }
                SumToZone += AirflowNetwork::AirflowNetworkLinkSimu(state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(Ctd2, ZoneNum)).VolFLOW;
            }
        }

        // Check if wind direction is within +/- 90 degrees of the outward normal of the dominant surface
        SurfNorm = Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).Azimuth;
        CosPhi = std::cos((state.dataEnvrn->WindDir - SurfNorm) * DataGlobalConstants::DegToRadians);
        if (CosPhi <= 0) {
            state.dataRoomAirMod->AirModel(ZoneNum).SimAirModel = false;
            auto flows(state.dataRoomAirMod->CVJetRecFlows(_, ZoneNum));
            for (int i = 1, u = flows.u(); i <= u; ++i) {
                auto &e(flows(i));
                e.Ujet = e.Urec = 0.0;
            }
            state.dataRoomAirMod->Urec(ZoneNum) = 0.0;
            state.dataRoomAirMod->Ujet(ZoneNum) = 0.0;
            state.dataRoomAirMod->Qrec(ZoneNum) = 0.0;
            if (Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).ExtBoundCond > 0) {
                state.dataRoomAirMod->Tin(ZoneNum) = MAT(Surface(Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).ExtBoundCond).Zone);
            } else if (Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).ExtBoundCond == ExternalEnvironment) {
                state.dataRoomAirMod->Tin(ZoneNum) = Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).OutDryBulbTemp;
            } else if (Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).ExtBoundCond == Ground) {
                state.dataRoomAirMod->Tin(ZoneNum) = Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).OutDryBulbTemp;
            } else if (Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).ExtBoundCond == OtherSideCoefNoCalcExt ||
                       Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).ExtBoundCond == OtherSideCoefCalcExt) {
                OPtr = Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).OSCPtr;
                OSC(OPtr).OSCTempCalc = (OSC(OPtr).ZoneAirTempCoef * MAT(ZoneNum) +
                                         OSC(OPtr).ExtDryBulbCoef * Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).OutDryBulbTemp +
                                         OSC(OPtr).ConstTempCoef * OSC(OPtr).ConstTemp + OSC(OPtr).GroundTempCoef * state.dataEnvrn->GroundTemp +
                                         OSC(OPtr).WindSpeedCoef * Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).WindSpeed *
                                             Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).OutDryBulbTemp);
                state.dataRoomAirMod->Tin(ZoneNum) = OSC(OPtr).OSCTempCalc;
            } else {
                state.dataRoomAirMod->Tin(ZoneNum) = Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).OutDryBulbTemp;
            }
            return;
        }

        // Calculate the opening area for all apertures
        for (Ctd = 1; Ctd <= state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(0, ZoneNum); ++Ctd) {
            int cCompNum = AirflowNetwork::AirflowNetworkLinkageData(Ctd).CompNum;
            if (AirflowNetwork::AirflowNetworkCompData(cCompNum).CompTypeNum == AirflowNetwork::CompTypeNum_DOP) {
                state.dataRoomAirMod->CVJetRecFlows(Ctd, ZoneNum).Area =
                        state.dataRoomAirMod->SurfParametersCVDV(Ctd).Width * state.dataRoomAirMod->SurfParametersCVDV(Ctd).Height * AirflowNetwork::MultizoneSurfaceData(Ctd).OpenFactor;
            } else if (AirflowNetwork::AirflowNetworkCompData(cCompNum).CompTypeNum == AirflowNetwork::CompTypeNum_SCR) {
                state.dataRoomAirMod->CVJetRecFlows(Ctd, ZoneNum).Area = state.dataRoomAirMod->SurfParametersCVDV(Ctd).Width * state.dataRoomAirMod->SurfParametersCVDV(Ctd).Height;
            } else {
                ShowSevereError(state,
                    "RoomAirModelCrossVent:EvolveParaUCSDCV: Illegal leakage component referenced in the cross ventilation room air model");
                ShowContinueError(state, "Surface " + AirflowNetwork::AirflowNetworkLinkageData(Ctd).Name + " in zone " + Zone(ZoneNum).Name +
                                  " uses leakage component " + AirflowNetwork::AirflowNetworkLinkageData(Ctd).CompName);
                ShowContinueError(state, "Only leakage component types AirflowNetwork:MultiZone:Component:DetailedOpening and ");
                ShowContinueError(state, "AirflowNetwork:MultiZone:Surface:Crack can be used with the cross ventilation room air model");
                ShowFatalError(state, "Previous severe error causes program termination");
            }
        }

        // Calculate Droom, Wroom, Dstar
        // Droom the distance between the average point of the base surface of the airflow network Surface (if the base surface
        // is a Window or Door it looks for the second base surface).
        // Dstar is Droom corrected for wind angle
        Wroom = Zone(ZoneNum).Volume / Zone(ZoneNum).FloorArea;
        auto const &baseSurface(Surface(Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).BaseSurf));
        if ((baseSurface.Sides == 3) || (baseSurface.Sides == 4)) {
            XX = baseSurface.Centroid.x;
            YY = baseSurface.Centroid.y;
            ZZ = baseSurface.Centroid.z;
        } else {
            // If the surface has more than 4 vertex then average the vertex coordinates in X, Y and Z.
            NSides = baseSurface.Sides;
            assert(NSides > 0);
            XX = YY = ZZ = 0.0;
            for (int i = 1; i <= NSides; ++i) {
                auto const &v(baseSurface.Vertex(i));
                XX += v.x;
                YY += v.y;
                ZZ += v.z;
            }
            XX /= double(NSides);
            YY /= double(NSides);
            ZZ /= double(NSides);
        }

        Real64 const Wroom_2(pow_2(Wroom));
        for (Ctd = PosZ_Wall(2 * ZoneNum - 1); Ctd <= PosZ_Wall(2 * ZoneNum); ++Ctd) {
            if ((Surface(APos_Wall(Ctd)).Sides == 3) || (Surface(APos_Wall(Ctd)).Sides == 4)) {
                XX_Wall = Surface(APos_Wall(Ctd)).Centroid.x;
                YY_Wall = Surface(APos_Wall(Ctd)).Centroid.y;
                ZZ_Wall = Surface(APos_Wall(Ctd)).Centroid.z;
            } else {
                NSides = Surface(APos_Wall(Ctd)).Sides;
                assert(NSides > 0);
                XX_Wall = YY_Wall = ZZ_Wall = 0.0;
                for (int i = 1; i <= NSides; ++i) {
                    auto const &v(Surface(APos_Wall(Ctd)).Vertex(i));
                    XX_Wall += v.x;
                    YY_Wall += v.y;
                    ZZ_Wall += v.z;
                }
                XX_Wall /= double(NSides);
                YY_Wall /= double(NSides);
                ZZ_Wall /= double(NSides);
            }
            auto DroomTemp = std::sqrt(pow_2(XX - XX_Wall) + pow_2(YY - YY_Wall) + pow_2(ZZ - ZZ_Wall));
            if (DroomTemp > state.dataRoomAirMod->Droom(ZoneNum)) {
                state.dataRoomAirMod->Droom(ZoneNum) = DroomTemp;
            }
            state.dataRoomAirMod->Dstar(ZoneNum) = min(state.dataRoomAirMod->Droom(ZoneNum) / CosPhi, std::sqrt(Wroom_2 + pow_2(state.dataRoomAirMod->Droom(ZoneNum))));
        }

        // Room area
        Aroom = Zone(ZoneNum).Volume / state.dataRoomAirMod->Droom(ZoneNum);

        // Populate an array of inflow volume fluxes (Fin) for all apertures in the zone
        // Calculate inflow velocity (%Uin) for each aperture in the zone
        for (Ctd = 1; Ctd <= state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(0, ZoneNum); ++Ctd) {
            if (Surface(AirflowNetwork::MultizoneSurfaceData(Ctd).SurfNum).Zone == ZoneNum) {
                // this is a direct airflow network aperture
                state.dataRoomAirMod->CVJetRecFlows(Ctd, ZoneNum).Fin = AirflowNetwork::AirflowNetworkLinkSimu(state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(Ctd, ZoneNum)).VolFLOW2;
            } else {
                // this is an indirect airflow network aperture
                state.dataRoomAirMod->CVJetRecFlows(Ctd, ZoneNum).Fin = AirflowNetwork::AirflowNetworkLinkSimu(state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(Ctd, ZoneNum)).VolFLOW;
            }
            if (state.dataRoomAirMod->CVJetRecFlows(Ctd, ZoneNum).Area != 0) {
                state.dataRoomAirMod->CVJetRecFlows(Ctd, ZoneNum).Uin = state.dataRoomAirMod->CVJetRecFlows(Ctd, ZoneNum).Fin / state.dataRoomAirMod->CVJetRecFlows(Ctd, ZoneNum).Area;
            } else {
                state.dataRoomAirMod->CVJetRecFlows(Ctd, ZoneNum).Uin = 0.0;
            }
        }

        // Verify if Uin is higher than minimum for each aperture
        // Create a flow flag for each aperture
        // Calculate the total area of all active apertures
        ActiveSurfNum = 0.0;
        state.dataRoomAirMod->Ain(ZoneNum) = 0.0;
        for (Ctd = 1; Ctd <= state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(0, ZoneNum); ++Ctd) {
            if (state.dataRoomAirMod->CVJetRecFlows(Ctd, ZoneNum).Uin <= MinUin) {
                state.dataRoomAirMod->CVJetRecFlows(Ctd, ZoneNum).FlowFlag = 0;
            } else {
                state.dataRoomAirMod->CVJetRecFlows(Ctd, ZoneNum).FlowFlag = 1;
            }
            ActiveSurfNum += state.dataRoomAirMod->CVJetRecFlows(Ctd, ZoneNum).FlowFlag;
            state.dataRoomAirMod->Ain(ZoneNum) += state.dataRoomAirMod->CVJetRecFlows(Ctd, ZoneNum).Area * state.dataRoomAirMod->CVJetRecFlows(Ctd, ZoneNum).FlowFlag;
        }

        // Verify if any of the apertures have minimum flow
        if (ActiveSurfNum == 0) {
            state.dataRoomAirMod->AirModel(ZoneNum).SimAirModel = false;
            if (Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).ExtBoundCond > 0) {
                state.dataRoomAirMod->Tin(ZoneNum) = MAT(Surface(Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).ExtBoundCond).Zone);
            } else if (Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).ExtBoundCond == ExternalEnvironment) {
                state.dataRoomAirMod->Tin(ZoneNum) = Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).OutDryBulbTemp;
            } else if (Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).ExtBoundCond == Ground) {
                state.dataRoomAirMod->Tin(ZoneNum) = Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).OutDryBulbTemp;
            } else if (Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).ExtBoundCond == OtherSideCoefNoCalcExt ||
                       Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).ExtBoundCond == OtherSideCoefCalcExt) {
                OPtr = Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).OSCPtr;
                OSC(OPtr).OSCTempCalc = (OSC(OPtr).ZoneAirTempCoef * MAT(ZoneNum) +
                                         OSC(OPtr).ExtDryBulbCoef * Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).OutDryBulbTemp +
                                         OSC(OPtr).ConstTempCoef * OSC(OPtr).ConstTemp + OSC(OPtr).GroundTempCoef * state.dataEnvrn->GroundTemp +
                                         OSC(OPtr).WindSpeedCoef * Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).WindSpeed *
                                             Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).OutDryBulbTemp);
                state.dataRoomAirMod->Tin(ZoneNum) = OSC(OPtr).OSCTempCalc;
            } else {
                state.dataRoomAirMod->Tin(ZoneNum) = Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).OutDryBulbTemp;
            }
            state.dataRoomAirMod->Urec(ZoneNum) = 0.0;
            state.dataRoomAirMod->Ujet(ZoneNum) = 0.0;
            state.dataRoomAirMod->Qrec(ZoneNum) = 0.0;
            auto flows(state.dataRoomAirMod->CVJetRecFlows(_, ZoneNum));
            for (int i = 1, u = flows.u(); i <= u; ++i) {
                auto &e(flows(i));
                e.Ujet = e.Urec = 0.0;
            }
            return;
        }

        // Calculate Uin, the area weighted average velocity of all the active apertures in the zone
        // Calculate Qtot, the total volumetric flow rate through all active openings in the zone
        Uin = 0.0;

        for (Ctd = 1; Ctd <= state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(0, ZoneNum); ++Ctd) {
            Uin += state.dataRoomAirMod->CVJetRecFlows(Ctd, ZoneNum).Area * state.dataRoomAirMod->CVJetRecFlows(Ctd, ZoneNum).Uin * state.dataRoomAirMod->CVJetRecFlows(Ctd, ZoneNum).FlowFlag / state.dataRoomAirMod->Ain(ZoneNum);
        }

        // Verify if Uin is higher than minimum:
        if (Uin < MinUin) {
            state.dataRoomAirMod->AirModel(ZoneNum).SimAirModel = false;
            state.dataRoomAirMod->Urec(ZoneNum) = 0.0;
            state.dataRoomAirMod->Ujet(ZoneNum) = 0.0;
            state.dataRoomAirMod->Qrec(ZoneNum) = 0.0;
            state.dataRoomAirMod->RecInflowRatio(ZoneNum) = 0.0;
            auto flows(state.dataRoomAirMod->CVJetRecFlows(_, ZoneNum));
            for (int i = 1, u = flows.u(); i <= u; ++i) {
                auto &e(flows(i));
                e.Ujet = e.Urec = 0.0;
            }
            if (Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).ExtBoundCond > 0) {
                state.dataRoomAirMod->Tin(ZoneNum) = MAT(Surface(Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).ExtBoundCond).Zone);
            } else if (Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).ExtBoundCond == ExternalEnvironment) {
                state.dataRoomAirMod->Tin(ZoneNum) = Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).OutDryBulbTemp;
            } else if (Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).ExtBoundCond == Ground) {
                state.dataRoomAirMod->Tin(ZoneNum) = Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).OutDryBulbTemp;
            } else if (Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).ExtBoundCond == OtherSideCoefNoCalcExt ||
                       Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).ExtBoundCond == OtherSideCoefCalcExt) {
                OPtr = Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).OSCPtr;
                OSC(OPtr).OSCTempCalc = (OSC(OPtr).ZoneAirTempCoef * MAT(ZoneNum) +
                                         OSC(OPtr).ExtDryBulbCoef * Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).OutDryBulbTemp +
                                         OSC(OPtr).ConstTempCoef * OSC(OPtr).ConstTemp + OSC(OPtr).GroundTempCoef * state.dataEnvrn->GroundTemp +
                                         OSC(OPtr).WindSpeedCoef * Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).WindSpeed *
                                             Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).OutDryBulbTemp);
                state.dataRoomAirMod->Tin(ZoneNum) = OSC(OPtr).OSCTempCalc;

            } else {
                state.dataRoomAirMod->Tin(ZoneNum) = Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).OutDryBulbTemp;
            }
            return;
        }

        // Evaluate parameter that determines whether recirculations are present
        for (Ctd = 1; Ctd <= state.dataRoomAirMod->TotUCSDCV; ++Ctd) {
            if (ZoneNum == state.dataRoomAirMod->ZoneUCSDCV(Ctd).ZonePtr) {
                if (state.dataRoomAirMod->Ain(ZoneNum) / Aroom > 1.0 / 2.0) {
                    state.dataRoomAirMod->JetRecAreaRatio(ZoneNum) = 1.0;
                } else {
                    state.dataRoomAirMod->JetRecAreaRatio(ZoneNum) = std::sqrt(state.dataRoomAirMod->Ain(ZoneNum) / Aroom);
                }
            }
        }

        state.dataRoomAirMod->AirModel(ZoneNum).SimAirModel = true;
        // Calculate jet and recirculation velocities for all active apertures
        state.dataRoomAirMod->Ujet(ZoneNum) = 0.0;
        state.dataRoomAirMod->Urec(ZoneNum) = 0.0;
        state.dataRoomAirMod->Qrec(ZoneNum) = 0.0;
        state.dataRoomAirMod->Qtot(ZoneNum) = 0.0;
        auto flows(state.dataRoomAirMod->CVJetRecFlows(_, ZoneNum));
        for (int i = 1, u = flows.u(); i <= u; ++i) {
            auto &e(flows(i));
            e.Ujet = e.Urec = e.Qrec = 0.0;
        }
        for (Ctd = 1; Ctd <= state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(0, ZoneNum); ++Ctd) {
            if (state.dataRoomAirMod->CVJetRecFlows(Ctd, ZoneNum).Uin != 0) {
                Real64 dstarexp = max(state.dataRoomAirMod->Dstar(ZoneNum) / (6.0 * std::sqrt(state.dataRoomAirMod->CVJetRecFlows(Ctd, ZoneNum).Area)), 1.0);
                state.dataRoomAirMod->CVJetRecFlows(Ctd, ZoneNum).Vjet =
                        state.dataRoomAirMod->CVJetRecFlows(Ctd, ZoneNum).Uin * std::sqrt(state.dataRoomAirMod->CVJetRecFlows(Ctd, ZoneNum).Area) * 6.3 * std::log(dstarexp) / state.dataRoomAirMod->Dstar(ZoneNum);
                state.dataRoomAirMod->CVJetRecFlows(Ctd, ZoneNum).Yjet =
                    Cjet1 * std::sqrt(state.dataRoomAirMod->CVJetRecFlows(Ctd, ZoneNum).Area / Aroom) * state.dataRoomAirMod->CVJetRecFlows(Ctd, ZoneNum).Vjet / state.dataRoomAirMod->CVJetRecFlows(Ctd, ZoneNum).Uin +
                    Cjet2;
                state.dataRoomAirMod->CVJetRecFlows(Ctd, ZoneNum).Yrec =
                    Crec1 * std::sqrt(state.dataRoomAirMod->CVJetRecFlows(Ctd, ZoneNum).Area / Aroom) * state.dataRoomAirMod->CVJetRecFlows(Ctd, ZoneNum).Vjet / state.dataRoomAirMod->CVJetRecFlows(Ctd, ZoneNum).Uin +
                    Crec2;
                state.dataRoomAirMod->CVJetRecFlows(Ctd, ZoneNum).YQrec = CrecFlow1 * std::sqrt(state.dataRoomAirMod->CVJetRecFlows(Ctd, ZoneNum).Area * Aroom) *
                                                                                  state.dataRoomAirMod->CVJetRecFlows(Ctd, ZoneNum).Vjet / state.dataRoomAirMod->CVJetRecFlows(Ctd, ZoneNum).Uin +
                                                    CrecFlow2;
                state.dataRoomAirMod->CVJetRecFlows(Ctd, ZoneNum).Ujet =
                        state.dataRoomAirMod->CVJetRecFlows(Ctd, ZoneNum).FlowFlag * state.dataRoomAirMod->CVJetRecFlows(Ctd, ZoneNum).Yjet / state.dataRoomAirMod->CVJetRecFlows(Ctd, ZoneNum).Uin;
                state.dataRoomAirMod->CVJetRecFlows(Ctd, ZoneNum).Urec =
                        state.dataRoomAirMod->CVJetRecFlows(Ctd, ZoneNum).FlowFlag * state.dataRoomAirMod->CVJetRecFlows(Ctd, ZoneNum).Yrec / state.dataRoomAirMod->CVJetRecFlows(Ctd, ZoneNum).Uin;
                state.dataRoomAirMod->CVJetRecFlows(Ctd, ZoneNum).Qrec =
                        state.dataRoomAirMod->CVJetRecFlows(Ctd, ZoneNum).FlowFlag * state.dataRoomAirMod->CVJetRecFlows(Ctd, ZoneNum).YQrec / state.dataRoomAirMod->CVJetRecFlows(Ctd, ZoneNum).Uin;
                state.dataRoomAirMod->Ujet(ZoneNum) += state.dataRoomAirMod->CVJetRecFlows(Ctd, ZoneNum).Area * state.dataRoomAirMod->CVJetRecFlows(Ctd, ZoneNum).Ujet / state.dataRoomAirMod->Ain(ZoneNum);
                state.dataRoomAirMod->Urec(ZoneNum) += state.dataRoomAirMod->CVJetRecFlows(Ctd, ZoneNum).Area * state.dataRoomAirMod->CVJetRecFlows(Ctd, ZoneNum).Urec / state.dataRoomAirMod->Ain(ZoneNum);
                state.dataRoomAirMod->Qrec(ZoneNum) += state.dataRoomAirMod->CVJetRecFlows(Ctd, ZoneNum).Qrec;
                state.dataRoomAirMod->Qtot(ZoneNum) += state.dataRoomAirMod->CVJetRecFlows(Ctd, ZoneNum).Fin * state.dataRoomAirMod->CVJetRecFlows(Ctd, ZoneNum).FlowFlag;
                state.dataRoomAirMod->Urec(ZoneNum) += state.dataRoomAirMod->CVJetRecFlows(Ctd, ZoneNum).Area * state.dataRoomAirMod->CVJetRecFlows(Ctd, ZoneNum).Urec / state.dataRoomAirMod->Ain(ZoneNum);
            }
        }

        // Ratio between recirculation flow rate and total inflow rate
        if (state.dataRoomAirMod->Qtot(ZoneNum) != 0) {
            state.dataRoomAirMod->RecInflowRatio(ZoneNum) = state.dataRoomAirMod->Qrec(ZoneNum) / state.dataRoomAirMod->Qtot(ZoneNum);
        } else {
            state.dataRoomAirMod->RecInflowRatio(ZoneNum) = 0.0;
        }

        // Set Tin based on external conditions of the dominant aperture
        if (Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).ExtBoundCond <= 0) {
            if (Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).ExtBoundCond == ExternalEnvironment) {
                state.dataRoomAirMod->Tin(ZoneNum) = Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).OutDryBulbTemp;
            } else if (Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).ExtBoundCond == Ground) {
                state.dataRoomAirMod->Tin(ZoneNum) = Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).OutDryBulbTemp;
            } else if (Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).ExtBoundCond == OtherSideCoefNoCalcExt ||
                       Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).ExtBoundCond == OtherSideCoefCalcExt) {
                OPtr = Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).OSCPtr;
                OSC(OPtr).OSCTempCalc = (OSC(OPtr).ZoneAirTempCoef * MAT(ZoneNum) +
                                         OSC(OPtr).ExtDryBulbCoef * Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).OutDryBulbTemp +
                                         OSC(OPtr).ConstTempCoef * OSC(OPtr).ConstTemp + OSC(OPtr).GroundTempCoef * state.dataEnvrn->GroundTemp +
                                         OSC(OPtr).WindSpeedCoef * Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).WindSpeed *
                                             Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).OutDryBulbTemp);
                state.dataRoomAirMod->Tin(ZoneNum) = OSC(OPtr).OSCTempCalc;
            } else {
                state.dataRoomAirMod->Tin(ZoneNum) = Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).OutDryBulbTemp;
            }
        } else {
            // adiabatic surface
            if (AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum ==
                Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).ExtBoundCond) {
                NodeNum1 = AirflowNetwork::AirflowNetworkLinkageData(MaxSurf).NodeNums[0];
                NodeNum2 = AirflowNetwork::AirflowNetworkLinkageData(MaxSurf).NodeNums[1];
                if (Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).Zone == ZoneNum) {
                    if (AirflowNetwork::AirflowNetworkNodeData(NodeNum1).EPlusZoneNum <= 0) {
                        state.dataRoomAirMod->Tin(ZoneNum) = Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).OutDryBulbTemp;
                    } else if (state.dataRoomAirMod->AirModel(AirflowNetwork::AirflowNetworkNodeData(NodeNum1).EPlusZoneNum).AirModelType == DataRoomAirModel::RoomAirModel::UCSDCV) {
                        state.dataRoomAirMod->Tin(ZoneNum) = state.dataRoomAirMod->RoomOutflowTemp(AirflowNetwork::AirflowNetworkNodeData(NodeNum1).EPlusZoneNum);
                    } else {
                        state.dataRoomAirMod->Tin(ZoneNum) = MAT(AirflowNetwork::AirflowNetworkNodeData(NodeNum1).EPlusZoneNum);
                    }

                } else {

                    if (AirflowNetwork::AirflowNetworkNodeData(NodeNum2).EPlusZoneNum <= 0) {
                        state.dataRoomAirMod->Tin(ZoneNum) = Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).OutDryBulbTemp;
                    } else if (state.dataRoomAirMod->AirModel(AirflowNetwork::AirflowNetworkNodeData(NodeNum2).EPlusZoneNum).AirModelType == DataRoomAirModel::RoomAirModel::UCSDCV) {
                        state.dataRoomAirMod->Tin(ZoneNum) = state.dataRoomAirMod->RoomOutflowTemp(AirflowNetwork::AirflowNetworkNodeData(NodeNum2).EPlusZoneNum);
                    } else {
                        state.dataRoomAirMod->Tin(ZoneNum) = MAT(AirflowNetwork::AirflowNetworkNodeData(NodeNum2).EPlusZoneNum);
                    }
                }
            } else if ((Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).Zone == ZoneNum) &&
                       (state.dataRoomAirMod->AirModel(Surface(Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).ExtBoundCond).Zone).AirModelType ==
                               DataRoomAirModel::RoomAirModel::UCSDCV)) {
                state.dataRoomAirMod->Tin(ZoneNum) = state.dataRoomAirMod->RoomOutflowTemp(Surface(Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).ExtBoundCond).Zone);
            } else if ((Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).Zone != ZoneNum) &&
                       (state.dataRoomAirMod->AirModel(Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).Zone).AirModelType == DataRoomAirModel::RoomAirModel::UCSDCV)) {
                state.dataRoomAirMod->Tin(ZoneNum) = state.dataRoomAirMod->RoomOutflowTemp(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum);
            } else {
                if (Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).Zone == ZoneNum) {
                    state.dataRoomAirMod->Tin(ZoneNum) = MAT(Surface(Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).ExtBoundCond).Zone);
                } else {
                    state.dataRoomAirMod->Tin(ZoneNum) = MAT(Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).Zone);
                }
            }
        }
    }

    void CalcUCSDCV(EnergyPlusData &state,
                    int const ZoneNum) // Which Zonenum
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         G. Carrilho da Graca
        //       DATE WRITTEN   October 2004
        //       MODIFIED       8/2013 - Sam Brunswick
        //                      To incorporate improved temperature calculations
        //       RE-ENGINEERED  -

        // PURPOSE OF THIS SUBROUTINE:
        // Subroutine for cross ventilation modelling.

        // REFERENCES:
        // Model developed by Paul Linden (UCSD), G. Carrilho da Graca (UCSD) and P. Haves (LBL).
        // Work funded by the California Energy Comission. More information on the model can found in:
        // "Simplified Models for Heat Transfer in Rooms" G. Carrilho da Graca, Ph.D. thesis UCSD. December 2003.

        using namespace DataHeatBalFanSys;
        using namespace DataEnvironment;
        using namespace DataHeatBalance;
        using InternalHeatGains::SumAllInternalConvectionGains;
        using InternalHeatGains::SumAllReturnAirConvectionGains;
        using Psychrometrics::PsyCpAirFnW;
        using Psychrometrics::PsyRhoAirFnPbTdbW;
        using ScheduleManager::GetCurrentScheduleValue;
        using ScheduleManager::GetScheduleIndex;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 GainsFrac;    // Fraction of lower subzone internal gains that mix as opposed to forming plumes
        Real64 ConvGains;    // Total convective gains in the room
        Real64 ConvGainsJet; // Total convective gains released in jet subzone
        Real64 ConvGainsRec; // Total convective gains released in recirculation subzone
        Real64 MCp_Total;    // Total capacity rate into the zone - assumed to enter at low level
        Real64 ZTAveraged;

        auto &Zone(state.dataHeatBal->Zone);

        int Ctd;
        Real64 MCpT_Total;
        Real64 L;
        Real64 ZoneMult; // total zone multiplier
        Real64 RetAirConvGain;

        GainsFrac = 0.0;
        ZoneMult = Zone(ZoneNum).Multiplier * Zone(ZoneNum).ListMultiplier;

        for (Ctd = 1; Ctd <= state.dataRoomAirMod->TotUCSDCV; ++Ctd) {
            if (ZoneNum == state.dataRoomAirMod->ZoneUCSDCV(Ctd).ZonePtr) {
                GainsFrac = GetCurrentScheduleValue(state, state.dataRoomAirMod->ZoneUCSDCV(Ctd).SchedGainsPtr);
            }
        }

        SumAllInternalConvectionGains(state, ZoneNum, ConvGains);
        ConvGains += SumConvHTRadSys(ZoneNum) + SumConvPool(ZoneNum) + SysDepZoneLoadsLagged(ZoneNum) + NonAirSystemResponse(ZoneNum) / ZoneMult;

        // Add heat to return air if zonal system (no return air) or cycling system (return air frequently very low or zero)
        if (Zone(ZoneNum).NoHeatToReturnAir) {
            SumAllReturnAirConvectionGains(state, ZoneNum, RetAirConvGain, 0);
            ConvGains += RetAirConvGain;
        }

        ConvGainsJet = ConvGains * GainsFrac;
        ConvGainsRec = ConvGains * (1.0 - GainsFrac);
        MCp_Total = MCPI(ZoneNum) + MCPV(ZoneNum) + MCPM(ZoneNum) + MCPE(ZoneNum) + MCPC(ZoneNum) + MDotCPOA(ZoneNum);
        MCpT_Total =
            MCPTI(ZoneNum) + MCPTV(ZoneNum) + MCPTM(ZoneNum) + MCPTE(ZoneNum) + MCPTC(ZoneNum) + MDotCPOA(ZoneNum) * Zone(ZoneNum).OutDryBulbTemp;

        if (AirflowNetwork::SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlMultizone) {
            MCp_Total = state.dataAirflowNetworkBalanceManager->exchangeData(ZoneNum).SumMCp + state.dataAirflowNetworkBalanceManager->exchangeData(ZoneNum).SumMMCp;
            MCpT_Total = state.dataAirflowNetworkBalanceManager->exchangeData(ZoneNum).SumMCpT + state.dataAirflowNetworkBalanceManager->exchangeData(ZoneNum).SumMMCpT;
        }

        EvolveParaUCSDCV(state, ZoneNum);
        L = state.dataRoomAirMod->Droom(ZoneNum);

        if (state.dataRoomAirMod->AirModel(ZoneNum).SimAirModel) {
            //=============================== CROSS VENTILATION  Calculation ==============================================
            state.dataRoomAirMod->ZoneCVisMixing(ZoneNum) = 0.0;
            state.dataRoomAirMod->ZoneCVhasREC(ZoneNum) = 1.0;
            for (Ctd = 1; Ctd <= 4; ++Ctd) {
                HcUCSDCV(state, ZoneNum);
                if (state.dataRoomAirMod->JetRecAreaRatio(ZoneNum) != 1.0) {
                    state.dataRoomAirMod->ZTREC(ZoneNum) = (ConvGainsRec * CrecTemp + CrecTemp * state.dataCrossVentMgr->HAT_R + state.dataRoomAirMod->Tin(ZoneNum) * MCp_Total) / (CrecTemp * state.dataCrossVentMgr->HA_R + MCp_Total);
                }
                state.dataRoomAirMod->ZTJET(ZoneNum) = (ConvGainsJet * CjetTemp + ConvGainsRec * CjetTemp + CjetTemp * state.dataCrossVentMgr->HAT_J + CjetTemp * state.dataCrossVentMgr->HAT_R + state.dataRoomAirMod->Tin(ZoneNum) * MCp_Total -
                                  CjetTemp * state.dataCrossVentMgr->HA_R * state.dataRoomAirMod->ZTREC(ZoneNum)) /
                                 (CjetTemp * state.dataCrossVentMgr->HA_J + MCp_Total);
                state.dataRoomAirMod->RoomOutflowTemp(ZoneNum) =
                    (ConvGainsJet + ConvGainsRec + state.dataCrossVentMgr->HAT_J + state.dataCrossVentMgr->HAT_R + state.dataRoomAirMod->Tin(ZoneNum) * MCp_Total - state.dataCrossVentMgr->HA_J * state.dataRoomAirMod->ZTJET(ZoneNum) - state.dataCrossVentMgr->HA_R * state.dataRoomAirMod->ZTREC(ZoneNum)) /
                    MCp_Total;
            }
            if (state.dataRoomAirMod->JetRecAreaRatio(ZoneNum) == 1.0) {
                state.dataRoomAirMod->ZoneCVhasREC(ZoneNum) = 0.0;
                state.dataRoomAirMod->ZTREC(ZoneNum) = state.dataRoomAirMod->RoomOutflowTemp(ZoneNum);
                state.dataRoomAirMod->ZTREC(ZoneNum) = state.dataRoomAirMod->ZTJET(ZoneNum);
                state.dataRoomAirMod->ZTREC(ZoneNum) = state.dataRoomAirMod->ZTJET(ZoneNum);
            }
            // If temperature increase is above 1.5C then go to mixing
            if (state.dataRoomAirMod->RoomOutflowTemp(ZoneNum) - state.dataRoomAirMod->Tin(ZoneNum) > 1.5) {
                state.dataRoomAirMod->ZoneCVisMixing(ZoneNum) = 1.0;
                state.dataRoomAirMod->ZoneCVhasREC(ZoneNum) = 0.0;
                state.dataRoomAirMod->AirModel(ZoneNum).SimAirModel = false;
                state.dataRoomAirMod->Ujet(ZoneNum) = 0.0;
                state.dataRoomAirMod->Urec(ZoneNum) = 0.0;
                state.dataRoomAirMod->Qrec(ZoneNum) = 0.0;
                state.dataRoomAirMod->RecInflowRatio(ZoneNum) = 0.0;
                for (auto &e : state.dataRoomAirMod->CVJetRecFlows) {
                    e.Ujet = 0.0;
                    e.Urec = 0.0;
                }
                for (Ctd = 1; Ctd <= 3; ++Ctd) {
                    ZTAveraged = MAT(ZoneNum);
                    state.dataRoomAirMod->RoomOutflowTemp(ZoneNum) = ZTAveraged;
                    state.dataRoomAirMod->ZTJET(ZoneNum) = ZTAveraged;
                    state.dataRoomAirMod->ZTREC(ZoneNum) = ZTAveraged;
                    state.dataRoomAirMod->RoomOutflowTemp(ZoneNum) = ZTAveraged;
                    state.dataRoomAirMod->ZTREC(ZoneNum) = ZTAveraged;
                    state.dataRoomAirMod->ZTJET(ZoneNum) = ZTAveraged;
                    state.dataRoomAirMod->ZTREC(ZoneNum) = ZTAveraged;
                    HcUCSDCV(state, ZoneNum);
                    ZTAveraged = MAT(ZoneNum);
                    state.dataRoomAirMod->RoomOutflowTemp(ZoneNum) = ZTAveraged;
                    state.dataRoomAirMod->ZTJET(ZoneNum) = ZTAveraged;
                    state.dataRoomAirMod->ZTREC(ZoneNum) = ZTAveraged;
                    state.dataRoomAirMod->RoomOutflowTemp(ZoneNum) = ZTAveraged;
                    state.dataRoomAirMod->ZTREC(ZoneNum) = ZTAveraged;
                    state.dataRoomAirMod->ZTJET(ZoneNum) = ZTAveraged;
                    state.dataRoomAirMod->ZTREC(ZoneNum) = ZTAveraged;
                }
            }
        } else {
            //=============================== M I X E D  Calculation ======================================================
            state.dataRoomAirMod->ZoneCVisMixing(ZoneNum) = 1.0;
            state.dataRoomAirMod->ZoneCVhasREC(ZoneNum) = 0.0;
            state.dataRoomAirMod->Ujet(ZoneNum) = 0.0;
            state.dataRoomAirMod->Urec(ZoneNum) = 0.0;
            state.dataRoomAirMod->Qrec(ZoneNum) = 0.0;
            state.dataRoomAirMod->RecInflowRatio(ZoneNum) = 0.0;
            for (auto &e : state.dataRoomAirMod->CVJetRecFlows) {
                e.Ujet = 0.0;
                e.Urec = 0.0;
            }
            for (Ctd = 1; Ctd <= 3; ++Ctd) {
                ZTAveraged = MAT(ZoneNum);
                state.dataRoomAirMod->RoomOutflowTemp(ZoneNum) = ZTAveraged;
                state.dataRoomAirMod->ZTJET(ZoneNum) = ZTAveraged;
                state.dataRoomAirMod->ZTREC(ZoneNum) = ZTAveraged;
                state.dataRoomAirMod->RoomOutflowTemp(ZoneNum) = ZTAveraged;
                state.dataRoomAirMod->ZTREC(ZoneNum) = ZTAveraged;
                state.dataRoomAirMod->ZTJET(ZoneNum) = ZTAveraged;
                state.dataRoomAirMod->ZTREC(ZoneNum) = ZTAveraged;
                HcUCSDCV(state, ZoneNum);
                ZTAveraged = MAT(ZoneNum);
                state.dataRoomAirMod->RoomOutflowTemp(ZoneNum) = ZTAveraged;
                state.dataRoomAirMod->ZTJET(ZoneNum) = ZTAveraged;
                state.dataRoomAirMod->ZTREC(ZoneNum) = ZTAveraged;
                state.dataRoomAirMod->RoomOutflowTemp(ZoneNum) = ZTAveraged;
                state.dataRoomAirMod->ZTREC(ZoneNum) = ZTAveraged;
                state.dataRoomAirMod->ZTJET(ZoneNum) = ZTAveraged;
                state.dataRoomAirMod->ZTREC(ZoneNum) = ZTAveraged;
            }
        }
    }

} // namespace CrossVentMgr

} // namespace EnergyPlus
