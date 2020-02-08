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
#include <cassert>
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EnergyPlus/ConvectionCoefficients.hh>
#include <EnergyPlus/CrossVentMgr.hh>
#include <EnergyPlus/AirflowNetwork/include/AirflowNetwork/Elements.hpp>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalSurface.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataPrecisionGlobals.hh>
#include <EnergyPlus/DataRoomAirModel.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataUCSDSharedData.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
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

    // METHODOLOGY EMPLOYED:

    // REFERENCES:

    // OTHER NOTES: none

    // Using/Aliasing
    using namespace DataPrecisionGlobals;
    using namespace DataGlobals;
    using namespace DataLoopNode;
    using namespace DataEnvironment;
    using namespace DataHeatBalance;
    using namespace DataHeatBalSurface;
    using namespace DataSurfaces;
    using namespace DataRoomAirModel;
    using ConvectionCoefficients::CalcDetailedHcInForDVModel;
    using DataHVACGlobals::SysTimeElapsed;
    using namespace DataUCSDSharedData;

    // Data
    // MODULE PARAMETER DEFINITIONS:

    // DERIVED TYPE DEFINITIONS:
    // na

    // MODULE VARIABLE DECLARATIONS:
    Real64 HAT_J(0.0);             // HAT_J Convection Coefficient times Area times Temperature for Jet subzone
    Real64 HA_J(0.0);              // HA_J  Convection Coefficient times Area for Jet subzone
    Real64 HAT_R(0.0);             // HAT_R Convection Coefficient times Area times Temperature for Recirculation subzone
    Real64 HA_R(0.0);              // HA_J  Convection Coefficient times Area for Recirculation subzone
    Real64 const Cjet1(1.873);     // First correlation constant for the jet velocity
    Real64 const Cjet2(0.243);     // Second correlation constant for the jet velocity
    Real64 const Crec1(0.591);     // First correlation constant for the recirculation velocity
    Real64 const Crec2(0.070);     // Second correlation constant for the recirculation velocity
    Real64 const CjetTemp(0.849);  // Correlation constant for the jet temperature rise
    Real64 const CrecTemp(1.385);  // Correlation constant for the recirculation temperature rise
    Real64 const CrecFlow1(0.415); // First correlation constant for the recirculation flow rate
    Real64 const CrecFlow2(0.466); // Second correlation constant for the recirculation flow rate

    namespace {
        // These were static variables within different functions. They were pulled out into the namespace
        // to facilitate easier unit testing of those functions.
        // These are purposefully not in the header file as an extern variable. No one outside of this module should
        // use these. They are cleared by clear_state() for use by unit tests, but normal simulations should be unaffected.
        // This is purposefully in an anonymous namespace so nothing outside this implementation file can use it.
        bool InitUCSDCV_MyOneTimeFlag(true);
        Array1D_bool InitUCSDCV_MyEnvrnFlag;
    } // namespace

    // SUBROUTINE SPECIFICATIONS:

    // Functions

    void ManageUCSDCVModel(int const ZoneNum) // index number for the specified zone
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         G. Carrilho da Graca
        //       DATE WRITTEN   October 2004
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        //   manage the UCSD Cross Ventilation model

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na
        // Using/Aliasing
        using DataHeatBalSurface::TempSurfIn;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        // FLOW:

        // initialize Cross Ventilation model

        InitUCSDCV(ZoneNum);

        // perform Cross Ventilation model calculations
        CalcUCSDCV(ZoneNum);
    }

    //**************************************************************************************************

    void InitUCSDCV(int const ZoneNum)
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

        // -
        // METHODOLOGY EMPLOYED:
        // -
        // -
        // -
        // -

        // REFERENCES:
        // -
        // -

        // Using/Aliasing
        using namespace DataRoomAirModel;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        // Do the one time initializations
        if (InitUCSDCV_MyOneTimeFlag) {
            InitUCSDCV_MyEnvrnFlag.dimension(NumOfZones, true);
            InitUCSDCV_MyOneTimeFlag = false;
        }

        // Do the begin environment initializations
        if (BeginEnvrnFlag && InitUCSDCV_MyEnvrnFlag(ZoneNum)) {
            InitUCSDCV_MyEnvrnFlag(ZoneNum) = false;
        }

        if (!BeginEnvrnFlag) {
            InitUCSDCV_MyEnvrnFlag(ZoneNum) = true;
        }
    }

    //**************************************************************************************************

    void HcUCSDCV(int const ZoneNum)
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

        // Using/Aliasing
        using namespace DataHeatBalFanSys;
        using namespace DataEnvironment;
        using namespace DataHeatBalance;
        using DataGlobals::BeginEnvrnFlag;
        using ScheduleManager::GetScheduleIndex; // , GetDayScheduleValues

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Ctd;     // DO loop counter for surfaces
        int SurfNum; // Surface number
        Real64 Hjet;
        Real64 Hrec;

        // Initialize HAT and HA
        HAT_J = 0.0;
        HAT_R = 0.0;
        HA_J = 0.0;
        HA_R = 0.0;

        // Is the air flow model for this zone set to UCSDCV Cross Ventilation?
        if (IsZoneCV(ZoneNum)) {
            // WALL Hc, HA and HAT calculation
            for (Ctd = PosZ_Wall((ZoneNum - 1) * 2 + 1); Ctd <= PosZ_Wall((ZoneNum - 1) * 2 + 2); ++Ctd) {
                SurfNum = APos_Wall(Ctd);
                Surface(SurfNum).TAirRef = AdjacentAirTemp;
                if (SurfNum == 0) continue;
                TempEffBulkAir(SurfNum) = ZTREC(ZoneNum);
                CalcDetailedHcInForDVModel(SurfNum, TempSurfIn, CVHcIn, Urec);
                HWall(Ctd) = CVHcIn(SurfNum);
                HAT_R += Surface(SurfNum).Area * TempSurfIn(SurfNum) * HWall(Ctd);
                HA_R += Surface(SurfNum).Area * HWall(Ctd);
            } // END WALL
            // WINDOW Hc, HA and HAT CALCULATION
            for (Ctd = PosZ_Window((ZoneNum - 1) * 2 + 1); Ctd <= PosZ_Window((ZoneNum - 1) * 2 + 2); ++Ctd) {
                SurfNum = APos_Window(Ctd);
                Surface(SurfNum).TAirRef = AdjacentAirTemp;
                if (SurfNum == 0) continue;
                if (Surface(SurfNum).Tilt > 10.0 && Surface(SurfNum).Tilt < 170.0) { // Window Wall
                    TempEffBulkAir(SurfNum) = ZTREC(ZoneNum);
                    CalcDetailedHcInForDVModel(SurfNum, TempSurfIn, CVHcIn, Urec);
                    HWindow(Ctd) = CVHcIn(SurfNum);
                    HAT_R += Surface(SurfNum).Area * TempSurfIn(SurfNum) * HWindow(Ctd);
                    HA_R += Surface(SurfNum).Area * HWindow(Ctd);
                }
                if (Surface(SurfNum).Tilt <= 10.0) { // Window Ceiling
                    TempEffBulkAir(SurfNum) = ZTJET(ZoneNum);
                    CalcDetailedHcInForDVModel(SurfNum, TempSurfIn, CVHcIn, Ujet);
                    Hjet = CVHcIn(SurfNum);
                    TempEffBulkAir(SurfNum) = ZTREC(ZoneNum);
                    CalcDetailedHcInForDVModel(SurfNum, TempSurfIn, CVHcIn, Urec);
                    Hrec = CVHcIn(SurfNum);
                    HWindow(Ctd) = JetRecAreaRatio(ZoneNum) * Hjet + (1 - JetRecAreaRatio(ZoneNum)) * Hrec;
                    HAT_R += Surface(SurfNum).Area * (1.0 - JetRecAreaRatio(ZoneNum)) * TempSurfIn(SurfNum) * Hrec;
                    HA_R += Surface(SurfNum).Area * (1.0 - JetRecAreaRatio(ZoneNum)) * Hrec;
                    HAT_J += Surface(SurfNum).Area * JetRecAreaRatio(ZoneNum) * TempSurfIn(SurfNum) * Hjet;
                    HA_J += Surface(SurfNum).Area * JetRecAreaRatio(ZoneNum) * Hjet;
                    TempEffBulkAir(SurfNum) = JetRecAreaRatio(ZoneNum) * ZTJET(ZoneNum) + (1 - JetRecAreaRatio(ZoneNum)) * ZTREC(ZoneNum);
                }
                if (Surface(SurfNum).Tilt >= 170.0) { // Window Floor
                    TempEffBulkAir(SurfNum) = ZTJET(ZoneNum);
                    CalcDetailedHcInForDVModel(SurfNum, TempSurfIn, CVHcIn, Ujet);
                    Hjet = CVHcIn(SurfNum);
                    TempEffBulkAir(SurfNum) = ZTREC(ZoneNum);
                    CalcDetailedHcInForDVModel(SurfNum, TempSurfIn, CVHcIn, Urec);
                    Hrec = CVHcIn(SurfNum);
                    HWindow(Ctd) = JetRecAreaRatio(ZoneNum) * Hjet + (1 - JetRecAreaRatio(ZoneNum)) * Hrec;
                    HAT_R += Surface(SurfNum).Area * (1.0 - JetRecAreaRatio(ZoneNum)) * TempSurfIn(SurfNum) * Hrec;
                    HA_R += Surface(SurfNum).Area * (1.0 - JetRecAreaRatio(ZoneNum)) * Hrec;
                    HAT_J += Surface(SurfNum).Area * JetRecAreaRatio(ZoneNum) * TempSurfIn(SurfNum) * Hjet;
                    HA_J += Surface(SurfNum).Area * JetRecAreaRatio(ZoneNum) * Hjet;
                    TempEffBulkAir(SurfNum) = JetRecAreaRatio(ZoneNum) * ZTJET(ZoneNum) + (1 - JetRecAreaRatio(ZoneNum)) * ZTREC(ZoneNum);
                }
                CVHcIn(SurfNum) = HWindow(Ctd);
            } // END WINDOW
            // DOOR Hc, HA and HAT CALCULATION
            for (Ctd = PosZ_Door((ZoneNum - 1) * 2 + 1); Ctd <= PosZ_Door((ZoneNum - 1) * 2 + 2); ++Ctd) { // DOOR
                SurfNum = APos_Door(Ctd);
                Surface(SurfNum).TAirRef = AdjacentAirTemp;
                if (SurfNum == 0) continue;
                TempEffBulkAir(SurfNum) = ZTREC(ZoneNum);
                CalcDetailedHcInForDVModel(SurfNum, TempSurfIn, CVHcIn, Urec);
                HDoor(Ctd) = CVHcIn(SurfNum);
                HAT_R += Surface(SurfNum).Area * TempSurfIn(SurfNum) * HDoor(Ctd);
                HA_R += Surface(SurfNum).Area * HDoor(Ctd);
            } // END DOOR
            // INTERNAL Hc, HA and HAT CALCULATION
            for (Ctd = PosZ_Internal((ZoneNum - 1) * 2 + 1); Ctd <= PosZ_Internal((ZoneNum - 1) * 2 + 2); ++Ctd) {
                SurfNum = APos_Internal(Ctd);
                Surface(SurfNum).TAirRef = AdjacentAirTemp;
                if (SurfNum == 0) continue;
                TempEffBulkAir(SurfNum) = ZTREC(ZoneNum);
                CalcDetailedHcInForDVModel(SurfNum, TempSurfIn, CVHcIn, Urec);
                HInternal(Ctd) = CVHcIn(SurfNum);
                HAT_R += Surface(SurfNum).Area * TempSurfIn(SurfNum) * HInternal(Ctd);
                HA_R += Surface(SurfNum).Area * HInternal(Ctd);
            } // END INTERNAL

            // CEILING Hc, HA and HAT CALCULATION
            for (Ctd = PosZ_Ceiling((ZoneNum - 1) * 2 + 1); Ctd <= PosZ_Ceiling((ZoneNum - 1) * 2 + 2); ++Ctd) {
                SurfNum = APos_Ceiling(Ctd);
                Surface(SurfNum).TAirRef = AdjacentAirTemp;
                if (SurfNum == 0) continue;
                TempEffBulkAir(SurfNum) = ZTJET(ZoneNum);
                CalcDetailedHcInForDVModel(SurfNum, TempSurfIn, CVHcIn, Ujet);
                Hjet = CVHcIn(SurfNum);
                TempEffBulkAir(SurfNum) = ZTREC(ZoneNum);
                CalcDetailedHcInForDVModel(SurfNum, TempSurfIn, CVHcIn, Urec);
                Hrec = CVHcIn(SurfNum);
                HCeiling(Ctd) = JetRecAreaRatio(ZoneNum) * Hjet + (1 - JetRecAreaRatio(ZoneNum)) * Hrec;
                HAT_R += Surface(SurfNum).Area * (1 - JetRecAreaRatio(ZoneNum)) * TempSurfIn(SurfNum) * Hrec;
                HA_R += Surface(SurfNum).Area * (1 - JetRecAreaRatio(ZoneNum)) * Hrec;
                HAT_J += Surface(SurfNum).Area * JetRecAreaRatio(ZoneNum) * TempSurfIn(SurfNum) * Hjet;
                HA_J += Surface(SurfNum).Area * JetRecAreaRatio(ZoneNum) * Hjet;
                TempEffBulkAir(SurfNum) = JetRecAreaRatio(ZoneNum) * ZTJET(ZoneNum) + (1 - JetRecAreaRatio(ZoneNum)) * ZTREC(ZoneNum);
                CVHcIn(SurfNum) = HCeiling(Ctd);
            } // END CEILING
            // FLOOR Hc, HA and HAT CALCULATION
            for (Ctd = PosZ_Floor((ZoneNum - 1) * 2 + 1); Ctd <= PosZ_Floor((ZoneNum - 1) * 2 + 2); ++Ctd) {
                SurfNum = APos_Floor(Ctd);
                Surface(SurfNum).TAirRef = AdjacentAirTemp;
                if (SurfNum == 0) continue;
                TempEffBulkAir(SurfNum) = ZTJET(ZoneNum);
                CalcDetailedHcInForDVModel(SurfNum, TempSurfIn, CVHcIn, Ujet);
                Hjet = CVHcIn(SurfNum);
                TempEffBulkAir(SurfNum) = ZTREC(ZoneNum);
                CalcDetailedHcInForDVModel(SurfNum, TempSurfIn, CVHcIn, Urec);
                Hrec = CVHcIn(SurfNum);
                HFloor(Ctd) = JetRecAreaRatio(ZoneNum) * Hjet + (1 - JetRecAreaRatio(ZoneNum)) * Hrec;
                HAT_R += Surface(SurfNum).Area * (1 - JetRecAreaRatio(ZoneNum)) * TempSurfIn(SurfNum) * Hrec;
                HA_R += Surface(SurfNum).Area * (1 - JetRecAreaRatio(ZoneNum)) * Hrec;
                HAT_J += Surface(SurfNum).Area * JetRecAreaRatio(ZoneNum) * TempSurfIn(SurfNum) * Hjet;
                HA_J += Surface(SurfNum).Area * JetRecAreaRatio(ZoneNum) * Hjet;
                TempEffBulkAir(SurfNum) = JetRecAreaRatio(ZoneNum) * ZTJET(ZoneNum) + (1 - JetRecAreaRatio(ZoneNum)) * ZTREC(ZoneNum);
                CVHcIn(SurfNum) = HFloor(Ctd);
            } // END FLOOR
        }
    }

    //**************************************************************************************************

    void EvolveParaUCSDCV(int const ZoneNum)
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

        // METHODOLOGY EMPLOYED:
        // -
        // -
        // -
        // -

        // REFERENCES:
        // -
        // -

        // Using/Aliasing
        using namespace Psychrometrics;
        using namespace DataHeatBalFanSys;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:

        Real64 const MinUin(0.2);

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

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

        RecInflowRatio(ZoneNum) = 0.0;

        // Identify the dominant aperture:
        MaxSurf = AirflowNetworkSurfaceUCSDCV(1, ZoneNum);
        if (Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).Zone == ZoneNum) {
            // this is a direct airflow network aperture
            SumToZone = AirflowNetwork::AirflowNetworkLinkSimu(AirflowNetworkSurfaceUCSDCV(1, ZoneNum)).VolFLOW2;
            MaxFlux = AirflowNetwork::AirflowNetworkLinkSimu(AirflowNetworkSurfaceUCSDCV(1, ZoneNum)).VolFLOW2;
        } else {
            // this is an indirect airflow network aperture
            SumToZone = AirflowNetwork::AirflowNetworkLinkSimu(AirflowNetworkSurfaceUCSDCV(1, ZoneNum)).VolFLOW;
            MaxFlux = AirflowNetwork::AirflowNetworkLinkSimu(AirflowNetworkSurfaceUCSDCV(1, ZoneNum)).VolFLOW;
        }

        for (Ctd2 = 2; Ctd2 <= AirflowNetworkSurfaceUCSDCV(0, ZoneNum); ++Ctd2) {
            if (Surface(AirflowNetwork::MultizoneSurfaceData(AirflowNetworkSurfaceUCSDCV(Ctd2, ZoneNum)).SurfNum).Zone == ZoneNum) {
                if (AirflowNetwork::AirflowNetworkLinkSimu(AirflowNetworkSurfaceUCSDCV(Ctd2, ZoneNum)).VolFLOW2 > MaxFlux) {
                    MaxFlux = AirflowNetwork::AirflowNetworkLinkSimu(AirflowNetworkSurfaceUCSDCV(Ctd2, ZoneNum)).VolFLOW2;
                    MaxSurf = AirflowNetworkSurfaceUCSDCV(Ctd2, ZoneNum);
                }
                SumToZone += AirflowNetwork::AirflowNetworkLinkSimu(AirflowNetworkSurfaceUCSDCV(Ctd2, ZoneNum)).VolFLOW2;
            } else {
                if (AirflowNetwork::AirflowNetworkLinkSimu(AirflowNetworkSurfaceUCSDCV(Ctd2, ZoneNum)).VolFLOW > MaxFlux) {
                    MaxFlux = AirflowNetwork::AirflowNetworkLinkSimu(AirflowNetworkSurfaceUCSDCV(Ctd2, ZoneNum)).VolFLOW;
                    MaxSurf = AirflowNetworkSurfaceUCSDCV(Ctd2, ZoneNum);
                }
                SumToZone += AirflowNetwork::AirflowNetworkLinkSimu(AirflowNetworkSurfaceUCSDCV(Ctd2, ZoneNum)).VolFLOW;
            }
        }

        // Check if wind direction is within +/- 90 degrees of the outward normal of the dominant surface
        SurfNorm = Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).Azimuth;
        CosPhi = std::cos((WindDir - SurfNorm) * DegToRadians);
        if (CosPhi <= 0) {
            AirModel(ZoneNum).SimAirModel = false;
            auto flows(CVJetRecFlows(_, ZoneNum));
            for (int i = 1, u = flows.u(); i <= u; ++i) {
                auto &e(flows(i));
                e.Ujet = e.Urec = 0.0;
            }
            Urec(ZoneNum) = 0.0;
            Ujet(ZoneNum) = 0.0;
            Qrec(ZoneNum) = 0.0;
            if (Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).ExtBoundCond > 0) {
                Tin(ZoneNum) = MAT(Surface(Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).ExtBoundCond).Zone);
            } else if (Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).ExtBoundCond == ExternalEnvironment) {
                Tin(ZoneNum) = Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).OutDryBulbTemp;
            } else if (Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).ExtBoundCond == Ground) {
                Tin(ZoneNum) = Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).OutDryBulbTemp;
            } else if (Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).ExtBoundCond == OtherSideCoefNoCalcExt ||
                       Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).ExtBoundCond == OtherSideCoefCalcExt) {
                OPtr = Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).OSCPtr;
                OSC(OPtr).OSCTempCalc = (OSC(OPtr).ZoneAirTempCoef * MAT(ZoneNum) +
                                         OSC(OPtr).ExtDryBulbCoef * Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).OutDryBulbTemp +
                                         OSC(OPtr).ConstTempCoef * OSC(OPtr).ConstTemp + OSC(OPtr).GroundTempCoef * GroundTemp +
                                         OSC(OPtr).WindSpeedCoef * Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).WindSpeed *
                                             Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).OutDryBulbTemp);
                Tin(ZoneNum) = OSC(OPtr).OSCTempCalc;
            } else {
                Tin(ZoneNum) = Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).OutDryBulbTemp;
            }
            return;
        }

        // Calculate the opening area for all apertures
        for (Ctd = 1; Ctd <= AirflowNetworkSurfaceUCSDCV(0, ZoneNum); ++Ctd) {
            int cCompNum = AirflowNetwork::AirflowNetworkLinkageData(Ctd).CompNum;
            if (AirflowNetwork::AirflowNetworkCompData(cCompNum).CompTypeNum == AirflowNetwork::CompTypeNum_DOP) {
                CVJetRecFlows(Ctd, ZoneNum).Area =
                    SurfParametersCVDV(Ctd).Width * SurfParametersCVDV(Ctd).Height * AirflowNetwork::MultizoneSurfaceData(Ctd).OpenFactor;
            } else if (AirflowNetwork::AirflowNetworkCompData(cCompNum).CompTypeNum == AirflowNetwork::CompTypeNum_SCR) {
                CVJetRecFlows(Ctd, ZoneNum).Area = SurfParametersCVDV(Ctd).Width * SurfParametersCVDV(Ctd).Height;
            } else {
                ShowSevereError(
                    "RoomAirModelCrossVent:EvolveParaUCSDCV: Illegal leakage component referenced in the cross ventilation room air model");
                ShowContinueError("Surface " + AirflowNetwork::AirflowNetworkLinkageData(Ctd).Name + " in zone " + Zone(ZoneNum).Name +
                                  " uses leakage component " + AirflowNetwork::AirflowNetworkLinkageData(Ctd).CompName);
                ShowContinueError("Only leakage component types AirflowNetwork:MultiZone:Component:DetailedOpening and ");
                ShowContinueError("AirflowNetwork:MultiZone:Surface:Crack can be used with the cross ventilation room air model");
                ShowFatalError("Previous severe error causes program termination");
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
            if (DroomTemp > Droom(ZoneNum)) {
                Droom(ZoneNum) = DroomTemp;
            }
            Dstar(ZoneNum) = min(Droom(ZoneNum) / CosPhi, std::sqrt(Wroom_2 + pow_2(Droom(ZoneNum))));
        }

        // Room area
        Aroom = Zone(ZoneNum).Volume / Droom(ZoneNum);

        // Populate an array of inflow volume fluxes (Fin) for all apertures in the zone
        // Calculate inflow velocity (%Uin) for each aperture in the zone
        for (Ctd = 1; Ctd <= AirflowNetworkSurfaceUCSDCV(0, ZoneNum); ++Ctd) {
            if (Surface(AirflowNetwork::MultizoneSurfaceData(Ctd).SurfNum).Zone == ZoneNum) {
                // this is a direct airflow network aperture
                CVJetRecFlows(Ctd, ZoneNum).Fin = AirflowNetwork::AirflowNetworkLinkSimu(AirflowNetworkSurfaceUCSDCV(Ctd, ZoneNum)).VolFLOW2;
            } else {
                // this is an indirect airflow network aperture
                CVJetRecFlows(Ctd, ZoneNum).Fin = AirflowNetwork::AirflowNetworkLinkSimu(AirflowNetworkSurfaceUCSDCV(Ctd, ZoneNum)).VolFLOW;
            }
            if (CVJetRecFlows(Ctd, ZoneNum).Area != 0) {
                CVJetRecFlows(Ctd, ZoneNum).Uin = CVJetRecFlows(Ctd, ZoneNum).Fin / CVJetRecFlows(Ctd, ZoneNum).Area;
            } else {
                CVJetRecFlows(Ctd, ZoneNum).Uin = 0.0;
            }
        }

        // Verify if Uin is higher than minimum for each aperture
        // Create a flow flag for each aperture
        // Calculate the total area of all active apertures
        ActiveSurfNum = 0.0;
        Ain(ZoneNum) = 0.0;
        for (Ctd = 1; Ctd <= AirflowNetworkSurfaceUCSDCV(0, ZoneNum); ++Ctd) {
            if (CVJetRecFlows(Ctd, ZoneNum).Uin <= MinUin) {
                CVJetRecFlows(Ctd, ZoneNum).FlowFlag = 0;
            } else {
                CVJetRecFlows(Ctd, ZoneNum).FlowFlag = 1;
            }
            ActiveSurfNum += CVJetRecFlows(Ctd, ZoneNum).FlowFlag;
            Ain(ZoneNum) += CVJetRecFlows(Ctd, ZoneNum).Area * CVJetRecFlows(Ctd, ZoneNum).FlowFlag;
        }

        // Verify if any of the apertures have minimum flow
        if (ActiveSurfNum == 0) {
            AirModel(ZoneNum).SimAirModel = false;
            if (Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).ExtBoundCond > 0) {
                Tin(ZoneNum) = MAT(Surface(Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).ExtBoundCond).Zone);
            } else if (Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).ExtBoundCond == ExternalEnvironment) {
                Tin(ZoneNum) = Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).OutDryBulbTemp;
            } else if (Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).ExtBoundCond == Ground) {
                Tin(ZoneNum) = Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).OutDryBulbTemp;
            } else if (Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).ExtBoundCond == OtherSideCoefNoCalcExt ||
                       Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).ExtBoundCond == OtherSideCoefCalcExt) {
                OPtr = Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).OSCPtr;
                OSC(OPtr).OSCTempCalc = (OSC(OPtr).ZoneAirTempCoef * MAT(ZoneNum) +
                                         OSC(OPtr).ExtDryBulbCoef * Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).OutDryBulbTemp +
                                         OSC(OPtr).ConstTempCoef * OSC(OPtr).ConstTemp + OSC(OPtr).GroundTempCoef * GroundTemp +
                                         OSC(OPtr).WindSpeedCoef * Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).WindSpeed *
                                             Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).OutDryBulbTemp);
                Tin(ZoneNum) = OSC(OPtr).OSCTempCalc;
            } else {
                Tin(ZoneNum) = Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).OutDryBulbTemp;
            }
            Urec(ZoneNum) = 0.0;
            Ujet(ZoneNum) = 0.0;
            Qrec(ZoneNum) = 0.0;
            auto flows(CVJetRecFlows(_, ZoneNum));
            for (int i = 1, u = flows.u(); i <= u; ++i) {
                auto &e(flows(i));
                e.Ujet = e.Urec = 0.0;
            }
            return;
        }

        // Calculate Uin, the area weighted average velocity of all the active apertures in the zone
        // Calculate Qtot, the total volumetric flow rate through all active openings in the zone
        Uin = 0.0;

        for (Ctd = 1; Ctd <= AirflowNetworkSurfaceUCSDCV(0, ZoneNum); ++Ctd) {
            Uin += CVJetRecFlows(Ctd, ZoneNum).Area * CVJetRecFlows(Ctd, ZoneNum).Uin * CVJetRecFlows(Ctd, ZoneNum).FlowFlag / Ain(ZoneNum);
        }

        // Verify if Uin is higher than minimum:
        if (Uin < MinUin) {
            AirModel(ZoneNum).SimAirModel = false;
            Urec(ZoneNum) = 0.0;
            Ujet(ZoneNum) = 0.0;
            Qrec(ZoneNum) = 0.0;
            RecInflowRatio(ZoneNum) = 0.0;
            auto flows(CVJetRecFlows(_, ZoneNum));
            for (int i = 1, u = flows.u(); i <= u; ++i) {
                auto &e(flows(i));
                e.Ujet = e.Urec = 0.0;
            }
            if (Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).ExtBoundCond > 0) {
                Tin(ZoneNum) = MAT(Surface(Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).ExtBoundCond).Zone);
            } else if (Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).ExtBoundCond == ExternalEnvironment) {
                Tin(ZoneNum) = Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).OutDryBulbTemp;
            } else if (Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).ExtBoundCond == Ground) {
                Tin(ZoneNum) = Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).OutDryBulbTemp;
            } else if (Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).ExtBoundCond == OtherSideCoefNoCalcExt ||
                       Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).ExtBoundCond == OtherSideCoefCalcExt) {
                OPtr = Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).OSCPtr;
                OSC(OPtr).OSCTempCalc = (OSC(OPtr).ZoneAirTempCoef * MAT(ZoneNum) +
                                         OSC(OPtr).ExtDryBulbCoef * Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).OutDryBulbTemp +
                                         OSC(OPtr).ConstTempCoef * OSC(OPtr).ConstTemp + OSC(OPtr).GroundTempCoef * GroundTemp +
                                         OSC(OPtr).WindSpeedCoef * Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).WindSpeed *
                                             Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).OutDryBulbTemp);
                Tin(ZoneNum) = OSC(OPtr).OSCTempCalc;

            } else {
                Tin(ZoneNum) = Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).OutDryBulbTemp;
            }
            return;
        }

        // Evaluate parameter that determines whether recirculations are present
        for (Ctd = 1; Ctd <= TotUCSDCV; ++Ctd) {
            if (ZoneNum == ZoneUCSDCV(Ctd).ZonePtr) {
                if (Ain(ZoneNum) / Aroom > 1.0 / 2.0) {
                    JetRecAreaRatio(ZoneNum) = 1.0;
                } else {
                    JetRecAreaRatio(ZoneNum) = std::sqrt(Ain(ZoneNum) / Aroom);
                }
            }
        }

        AirModel(ZoneNum).SimAirModel = true;
        // Calculate jet and recirculation velocities for all active apertures
        Ujet(ZoneNum) = 0.0;
        Urec(ZoneNum) = 0.0;
        Qrec(ZoneNum) = 0.0;
        Qtot(ZoneNum) = 0.0;
        auto flows(CVJetRecFlows(_, ZoneNum));
        for (int i = 1, u = flows.u(); i <= u; ++i) {
            auto &e(flows(i));
            e.Ujet = e.Urec = e.Qrec = 0.0;
        }
        for (Ctd = 1; Ctd <= AirflowNetworkSurfaceUCSDCV(0, ZoneNum); ++Ctd) {
            if (CVJetRecFlows(Ctd, ZoneNum).Uin != 0) {
                Real64 dstarexp = max(Dstar(ZoneNum) / (6.0 * std::sqrt(CVJetRecFlows(Ctd, ZoneNum).Area)), 1.0);
                CVJetRecFlows(Ctd, ZoneNum).Vjet =
                    CVJetRecFlows(Ctd, ZoneNum).Uin * std::sqrt(CVJetRecFlows(Ctd, ZoneNum).Area) * 6.3 * std::log(dstarexp) / Dstar(ZoneNum);
                CVJetRecFlows(Ctd, ZoneNum).Yjet =
                    Cjet1 * std::sqrt(CVJetRecFlows(Ctd, ZoneNum).Area / Aroom) * CVJetRecFlows(Ctd, ZoneNum).Vjet / CVJetRecFlows(Ctd, ZoneNum).Uin +
                    Cjet2;
                CVJetRecFlows(Ctd, ZoneNum).Yrec =
                    Crec1 * std::sqrt(CVJetRecFlows(Ctd, ZoneNum).Area / Aroom) * CVJetRecFlows(Ctd, ZoneNum).Vjet / CVJetRecFlows(Ctd, ZoneNum).Uin +
                    Crec2;
                CVJetRecFlows(Ctd, ZoneNum).YQrec = CrecFlow1 * std::sqrt(CVJetRecFlows(Ctd, ZoneNum).Area * Aroom) *
                                                        CVJetRecFlows(Ctd, ZoneNum).Vjet / CVJetRecFlows(Ctd, ZoneNum).Uin +
                                                    CrecFlow2;
                CVJetRecFlows(Ctd, ZoneNum).Ujet =
                    CVJetRecFlows(Ctd, ZoneNum).FlowFlag * CVJetRecFlows(Ctd, ZoneNum).Yjet / CVJetRecFlows(Ctd, ZoneNum).Uin;
                CVJetRecFlows(Ctd, ZoneNum).Urec =
                    CVJetRecFlows(Ctd, ZoneNum).FlowFlag * CVJetRecFlows(Ctd, ZoneNum).Yrec / CVJetRecFlows(Ctd, ZoneNum).Uin;
                CVJetRecFlows(Ctd, ZoneNum).Qrec =
                    CVJetRecFlows(Ctd, ZoneNum).FlowFlag * CVJetRecFlows(Ctd, ZoneNum).YQrec / CVJetRecFlows(Ctd, ZoneNum).Uin;
                Ujet(ZoneNum) += CVJetRecFlows(Ctd, ZoneNum).Area * CVJetRecFlows(Ctd, ZoneNum).Ujet / Ain(ZoneNum);
                Urec(ZoneNum) += CVJetRecFlows(Ctd, ZoneNum).Area * CVJetRecFlows(Ctd, ZoneNum).Urec / Ain(ZoneNum);
                Qrec(ZoneNum) += CVJetRecFlows(Ctd, ZoneNum).Qrec;
                Qtot(ZoneNum) += CVJetRecFlows(Ctd, ZoneNum).Fin * CVJetRecFlows(Ctd, ZoneNum).FlowFlag;
                Urec(ZoneNum) += CVJetRecFlows(Ctd, ZoneNum).Area * CVJetRecFlows(Ctd, ZoneNum).Urec / Ain(ZoneNum);
            }
        }

        // Ratio between recirculation flow rate and total inflow rate
        if (Qtot(ZoneNum) != 0) {
            RecInflowRatio(ZoneNum) = Qrec(ZoneNum) / Qtot(ZoneNum);
        } else {
            RecInflowRatio(ZoneNum) = 0.0;
        }

        // Set Tin based on external conditions of the dominant aperture
        if (Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).ExtBoundCond <= 0) {
            if (Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).ExtBoundCond == ExternalEnvironment) {
                Tin(ZoneNum) = Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).OutDryBulbTemp;
            } else if (Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).ExtBoundCond == Ground) {
                Tin(ZoneNum) = Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).OutDryBulbTemp;
            } else if (Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).ExtBoundCond == OtherSideCoefNoCalcExt ||
                       Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).ExtBoundCond == OtherSideCoefCalcExt) {
                OPtr = Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).OSCPtr;
                OSC(OPtr).OSCTempCalc = (OSC(OPtr).ZoneAirTempCoef * MAT(ZoneNum) +
                                         OSC(OPtr).ExtDryBulbCoef * Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).OutDryBulbTemp +
                                         OSC(OPtr).ConstTempCoef * OSC(OPtr).ConstTemp + OSC(OPtr).GroundTempCoef * GroundTemp +
                                         OSC(OPtr).WindSpeedCoef * Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).WindSpeed *
                                             Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).OutDryBulbTemp);
                Tin(ZoneNum) = OSC(OPtr).OSCTempCalc;
            } else {
                Tin(ZoneNum) = Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).OutDryBulbTemp;
            }
        } else {
            // adiabatic surface
            if (AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum ==
                Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).ExtBoundCond) {
                NodeNum1 = AirflowNetwork::AirflowNetworkLinkageData(MaxSurf).NodeNums[0];
                NodeNum2 = AirflowNetwork::AirflowNetworkLinkageData(MaxSurf).NodeNums[1];
                if (Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).Zone == ZoneNum) {
                    if (AirflowNetwork::AirflowNetworkNodeData(NodeNum1).EPlusZoneNum <= 0) {
                        Tin(ZoneNum) = Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).OutDryBulbTemp;
                    } else if (AirModel(AirflowNetwork::AirflowNetworkNodeData(NodeNum1).EPlusZoneNum).AirModelType == RoomAirModel_UCSDCV) {
                        Tin(ZoneNum) = RoomOutflowTemp(AirflowNetwork::AirflowNetworkNodeData(NodeNum1).EPlusZoneNum);
                    } else {
                        Tin(ZoneNum) = MAT(AirflowNetwork::AirflowNetworkNodeData(NodeNum1).EPlusZoneNum);
                    }

                } else {

                    if (AirflowNetwork::AirflowNetworkNodeData(NodeNum2).EPlusZoneNum <= 0) {
                        Tin(ZoneNum) = Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).OutDryBulbTemp;
                    } else if (AirModel(AirflowNetwork::AirflowNetworkNodeData(NodeNum2).EPlusZoneNum).AirModelType == RoomAirModel_UCSDCV) {
                        Tin(ZoneNum) = RoomOutflowTemp(AirflowNetwork::AirflowNetworkNodeData(NodeNum2).EPlusZoneNum);
                    } else {
                        Tin(ZoneNum) = MAT(AirflowNetwork::AirflowNetworkNodeData(NodeNum2).EPlusZoneNum);
                    }
                }
            } else if ((Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).Zone == ZoneNum) &&
                       (AirModel(Surface(Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).ExtBoundCond).Zone).AirModelType ==
                        RoomAirModel_UCSDCV)) {
                Tin(ZoneNum) = RoomOutflowTemp(Surface(Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).ExtBoundCond).Zone);
            } else if ((Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).Zone != ZoneNum) &&
                       (AirModel(Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).Zone).AirModelType == RoomAirModel_UCSDCV)) {
                Tin(ZoneNum) = RoomOutflowTemp(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum);
            } else {
                if (Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).Zone == ZoneNum) {
                    Tin(ZoneNum) = MAT(Surface(Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).ExtBoundCond).Zone);
                } else {
                    Tin(ZoneNum) = MAT(Surface(AirflowNetwork::MultizoneSurfaceData(MaxSurf).SurfNum).Zone);
                }
            }
        }
    }

    //**************************************************************************************************

    void CalcUCSDCV(int const ZoneNum) // Which Zonenum
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         G. Carrilho da Graca
        //       DATE WRITTEN   October 2004
        //       MODIFIED       8/2013 - Sam Brunswick
        //                      To incorporate improved temperature calculations
        //       RE-ENGINEERED  -

        // PURPOSE OF THIS SUBROUTINE:
        // Subroutine for cross ventilation modelling.
        // METHODOLOGY EMPLOYED:
        // -
        // -
        // -
        // -

        // REFERENCES:
        // Model developed by Paul Linden (UCSD), G. Carrilho da Graca (UCSD) and P. Haves (LBL).
        // Work funded by the California Energy Comission. More information on the model can found in:
        // "Simplified Models for Heat Transfer in Rooms" G. Carrilho da Graca, Ph.D. thesis UCSD. December 2003.

        // Using/Aliasing
        using namespace DataHeatBalFanSys;
        using namespace DataEnvironment;
        using namespace DataHeatBalance;
        using InternalHeatGains::SumAllInternalConvectionGains;
        using InternalHeatGains::SumAllReturnAirConvectionGains;
        using Psychrometrics::PsyCpAirFnW;
        using Psychrometrics::PsyRhoAirFnPbTdbW;
        using ScheduleManager::GetCurrentScheduleValue;
        using ScheduleManager::GetScheduleIndex;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 GainsFrac;    // Fraction of lower subzone internal gains that mix as opposed to forming plumes
        Real64 ConvGains;    // Total convective gains in the room
        Real64 ConvGainsJet; // Total convective gains released in jet subzone
        Real64 ConvGainsRec; // Total convective gains released in recirculation subzone
        Real64 MCp_Total;    // Total capacity rate into the zone - assumed to enter at low level
        Real64 ZTAveraged;

        int Ctd;
        Real64 MCpT_Total;
        Real64 L;
        Real64 ZoneMult; // total zone multiplier
        Real64 RetAirConvGain;

        GainsFrac = 0.0;
        ZoneMult = Zone(ZoneNum).Multiplier * Zone(ZoneNum).ListMultiplier;

        for (Ctd = 1; Ctd <= TotUCSDCV; ++Ctd) {
            if (ZoneNum == ZoneUCSDCV(Ctd).ZonePtr) {
                GainsFrac = GetCurrentScheduleValue(ZoneUCSDCV(Ctd).SchedGainsPtr);
            }
        }

        SumAllInternalConvectionGains(ZoneNum, ConvGains);
        ConvGains += SumConvHTRadSys(ZoneNum) + SumConvPool(ZoneNum) + SysDepZoneLoadsLagged(ZoneNum) + NonAirSystemResponse(ZoneNum) / ZoneMult;

        // Add heat to return air if zonal system (no return air) or cycling system (return air frequently very low or zero)
        if (Zone(ZoneNum).NoHeatToReturnAir) {
            SumAllReturnAirConvectionGains(ZoneNum, RetAirConvGain, 0);
            ConvGains += RetAirConvGain;
        }

        ConvGainsJet = ConvGains * GainsFrac;
        ConvGainsRec = ConvGains * (1.0 - GainsFrac);
        MCp_Total = MCPI(ZoneNum) + MCPV(ZoneNum) + MCPM(ZoneNum) + MCPE(ZoneNum) + MCPC(ZoneNum) + MDotCPOA(ZoneNum);
        MCpT_Total =
            MCPTI(ZoneNum) + MCPTV(ZoneNum) + MCPTM(ZoneNum) + MCPTE(ZoneNum) + MCPTC(ZoneNum) + MDotCPOA(ZoneNum) * Zone(ZoneNum).OutDryBulbTemp;

        if (AirflowNetwork::SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlMultizone) {
            MCp_Total = AirflowNetwork::AirflowNetworkExchangeData(ZoneNum).SumMCp + AirflowNetwork::AirflowNetworkExchangeData(ZoneNum).SumMMCp;
            MCpT_Total = AirflowNetwork::AirflowNetworkExchangeData(ZoneNum).SumMCpT + AirflowNetwork::AirflowNetworkExchangeData(ZoneNum).SumMMCpT;
        }

        EvolveParaUCSDCV(ZoneNum);
        L = Droom(ZoneNum);

        if (AirModel(ZoneNum).SimAirModel) {
            //=============================== CROSS VENTILATION  Calculation ==============================================
            ZoneCVisMixing(ZoneNum) = 0.0;
            ZoneCVhasREC(ZoneNum) = 1.0;
            for (Ctd = 1; Ctd <= 4; ++Ctd) {
                HcUCSDCV(ZoneNum);
                if (JetRecAreaRatio(ZoneNum) != 1.0) {
                    ZTREC(ZoneNum) = (ConvGainsRec * CrecTemp + CrecTemp * HAT_R + Tin(ZoneNum) * MCp_Total) / (CrecTemp * HA_R + MCp_Total);
                }
                ZTJET(ZoneNum) = (ConvGainsJet * CjetTemp + ConvGainsRec * CjetTemp + CjetTemp * HAT_J + CjetTemp * HAT_R + Tin(ZoneNum) * MCp_Total -
                                  CjetTemp * HA_R * ZTREC(ZoneNum)) /
                                 (CjetTemp * HA_J + MCp_Total);
                RoomOutflowTemp(ZoneNum) =
                    (ConvGainsJet + ConvGainsRec + HAT_J + HAT_R + Tin(ZoneNum) * MCp_Total - HA_J * ZTJET(ZoneNum) - HA_R * ZTREC(ZoneNum)) /
                    MCp_Total;
            }
            if (JetRecAreaRatio(ZoneNum) == 1.0) {
                ZoneCVhasREC(ZoneNum) = 0.0;
                ZTREC(ZoneNum) = RoomOutflowTemp(ZoneNum);
                ZTREC(ZoneNum) = ZTJET(ZoneNum);
                ZTREC(ZoneNum) = ZTJET(ZoneNum);
            }
            // If temperature increase is above 1.5C then go to mixing
            if (RoomOutflowTemp(ZoneNum) - Tin(ZoneNum) > 1.5) {
                ZoneCVisMixing(ZoneNum) = 1.0;
                ZoneCVhasREC(ZoneNum) = 0.0;
                AirModel(ZoneNum).SimAirModel = false;
                Ujet(ZoneNum) = 0.0;
                Urec(ZoneNum) = 0.0;
                Qrec(ZoneNum) = 0.0;
                RecInflowRatio(ZoneNum) = 0.0;
                for (auto &e : CVJetRecFlows) {
                    e.Ujet = 0.0;
                    e.Urec = 0.0;
                }
                for (Ctd = 1; Ctd <= 3; ++Ctd) {
                    ZTAveraged = MAT(ZoneNum);
                    RoomOutflowTemp(ZoneNum) = ZTAveraged;
                    ZTJET(ZoneNum) = ZTAveraged;
                    ZTREC(ZoneNum) = ZTAveraged;
                    RoomOutflowTemp(ZoneNum) = ZTAveraged;
                    ZTREC(ZoneNum) = ZTAveraged;
                    ZTJET(ZoneNum) = ZTAveraged;
                    ZTREC(ZoneNum) = ZTAveraged;
                    HcUCSDCV(ZoneNum);
                    ZTAveraged = MAT(ZoneNum);
                    RoomOutflowTemp(ZoneNum) = ZTAveraged;
                    ZTJET(ZoneNum) = ZTAveraged;
                    ZTREC(ZoneNum) = ZTAveraged;
                    RoomOutflowTemp(ZoneNum) = ZTAveraged;
                    ZTREC(ZoneNum) = ZTAveraged;
                    ZTJET(ZoneNum) = ZTAveraged;
                    ZTREC(ZoneNum) = ZTAveraged;
                }
            }
        } else {
            //=============================== M I X E D  Calculation ======================================================
            ZoneCVisMixing(ZoneNum) = 1.0;
            ZoneCVhasREC(ZoneNum) = 0.0;
            Ujet(ZoneNum) = 0.0;
            Urec(ZoneNum) = 0.0;
            Qrec(ZoneNum) = 0.0;
            RecInflowRatio(ZoneNum) = 0.0;
            for (auto &e : CVJetRecFlows) {
                e.Ujet = 0.0;
                e.Urec = 0.0;
            }
            for (Ctd = 1; Ctd <= 3; ++Ctd) {
                ZTAveraged = MAT(ZoneNum);
                RoomOutflowTemp(ZoneNum) = ZTAveraged;
                ZTJET(ZoneNum) = ZTAveraged;
                ZTREC(ZoneNum) = ZTAveraged;
                RoomOutflowTemp(ZoneNum) = ZTAveraged;
                ZTREC(ZoneNum) = ZTAveraged;
                ZTJET(ZoneNum) = ZTAveraged;
                ZTREC(ZoneNum) = ZTAveraged;
                HcUCSDCV(ZoneNum);
                ZTAveraged = MAT(ZoneNum);
                RoomOutflowTemp(ZoneNum) = ZTAveraged;
                ZTJET(ZoneNum) = ZTAveraged;
                ZTREC(ZoneNum) = ZTAveraged;
                RoomOutflowTemp(ZoneNum) = ZTAveraged;
                ZTREC(ZoneNum) = ZTAveraged;
                ZTJET(ZoneNum) = ZTAveraged;
                ZTREC(ZoneNum) = ZTAveraged;
            }
        }
        //============================================================================================================
    }

    // Clears the global data in MixedAir.
    // Needed for unit tests, should not be normally called.
    void clear_state()
    {
        HAT_J = 0.0;
        HA_J = 0.0;
        HAT_R = 0.0;
        HA_R = 0.0;
        InitUCSDCV_MyOneTimeFlag = true;
        InitUCSDCV_MyEnvrnFlag.deallocate();
    }

} // namespace CrossVentMgr

} // namespace EnergyPlus
