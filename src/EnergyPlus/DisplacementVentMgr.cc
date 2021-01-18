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
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/member.functions.hh>

// EnergyPlus Headers
#include <EnergyPlus/AirflowNetworkBalanceManager.hh>
#include <EnergyPlus/ConvectionCoefficients.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalSurface.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataRoomAirModel.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataUCSDSharedData.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/DisplacementVentMgr.hh>
#include <EnergyPlus/InternalHeatGains.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

namespace DisplacementVentMgr {

    // MODULE INFORMATION:
    //       AUTHOR         G. Carrilho da Graca
    //       DATE WRITTEN   February 2004
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // Routines that implement the UCSD Displacement Ventilation

    // METHODOLOGY EMPLOYED:

    // REFERENCES:

    // OTHER NOTES: none

    // Using/Aliasing
    using namespace DataLoopNode;
    using namespace DataEnvironment;
    using namespace DataHeatBalance;
    using namespace DataHeatBalSurface;
    using namespace DataSurfaces;
    using namespace DataRoomAirModel;
    using ConvectionCoefficients::CalcDetailedHcInForDVModel;
    using DataHVACGlobals::PreviousTimeStep;
    using DataHVACGlobals::ShortenTimeStepSysRoomAir;
    using DataHVACGlobals::SysTimeElapsed;
    using namespace DataUCSDSharedData;

    // MODULE VARIABLE DECLARATIONS:
    Real64 HAT_MX;                       // HAT_MX Convection Coefficient times Area times Temperature for the upper subzone
    Real64 HA_MX;                        // HA_MX Convection Coefficient times Area for the upper subzone
    Real64 HAT_OC;                       // HAT_OC Convection Coefficient times Area times Temperature for the lower subzone
    Real64 HA_OC;                        // HA_OC Convection Coefficient times Area for the lower subzone
    Real64 HAT_FLOOR;                    // HAT_FLOOR Convection Coefficient times Area times Temperature for the floor(?) subzone
    Real64 HA_FLOOR;                     // HA_FLOOR Convection Coefficient times Area for the floor(?) subzone
    Real64 HeightFloorSubzoneTop(0.2);   // Assumed thickness of floor subzone
    Real64 ThickOccupiedSubzoneMin(0.2); // Minimum thickness of occupied subzone
    Real64 HeightIntMass(0.0);           // Height of internal mass surfaces, assumed vertical, cannot exceed ceiling height
    Real64 HeightIntMassDefault(2.0);    // Default height of internal mass surfaces
    bool InitUCSDDVMyOneTimeFlag(true);

    // SUBROUTINE SPECIFICATIONS:

    void clear_state() {
        InitUCSDDVMyOneTimeFlag = true;
    }

    void ManageUCSDDVModel(EnergyPlusData &state, int const ZoneNum) // index number for the specified zone
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         G. Carrilho da Graca
        //       DATE WRITTEN   February 2004
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        //   manage the UCSD Displacement Ventilation model

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

        // SUBROUTINE PARAMETER DEFINITIONS
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        // FLOW:

        // initialize Displacement Ventilation model
        InitUCSDDV(state, ZoneNum);

        // perform Displacement Ventilation model calculations
        CalcUCSDDV(state, ZoneNum);
    }

    //**************************************************************************************************

    void InitUCSDDV(EnergyPlusData &state, int const ZoneNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         G. Carrilho da Graca
        //       DATE WRITTEN   February 2004
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

        // USE STATEMENTS:

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        static Array1D_bool MyEnvrnFlag;

        // Do the one time initializations
        if (InitUCSDDVMyOneTimeFlag) {
            MyEnvrnFlag.dimension(state.dataGlobal->NumOfZones, true);
            HeightFloorSubzoneTop = 0.2;
            ThickOccupiedSubzoneMin = 0.2;
            HeightIntMassDefault = 2.0;
            InitUCSDDVMyOneTimeFlag = false;
        }

        // Do the begin environment initializations
        if (state.dataGlobal->BeginEnvrnFlag && MyEnvrnFlag(ZoneNum)) {
            HAT_MX = 0.0;
            HAT_OC = 0.0;
            HA_MX = 0.0;
            HA_OC = 0.0;
            HAT_FLOOR = 0.0;
            HA_FLOOR = 0.0;
            MyEnvrnFlag(ZoneNum) = false;
        }

        if (!state.dataGlobal->BeginEnvrnFlag) {
            MyEnvrnFlag(ZoneNum) = true;
        }

        // initialize these module variables every timestep
        HeightIntMass = HeightIntMassDefault;
    }

    //**************************************************************************************************

    void HcUCSDDV(EnergyPlusData &state, int const ZoneNum, Real64 const FractionHeight)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         G. Carrilho da Graca
        //       DATE WRITTEN   February 2004
        //       MODIFIED       -
        //       RE-ENGINEERED  -

        // PURPOSE OF THIS SUBROUTINE:
        // Main subroutine for convection calculation in the UCSD Displacement Ventilation model.
        // It calls CalcDetailedHcInForDVModel for convection coefficient
        // initial calculations and averages the final result comparing the position of the surface with
        // the interface subzone height.

        // Using/Aliasing
        using namespace DataHeatBalFanSys;
        using namespace DataEnvironment;
        using namespace DataHeatBalance;
        using ScheduleManager::GetScheduleIndex;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Ctd;         // DO loop counter for surfaces
        Real64 HLD;      // Convection coefficient for the lower area of surface
        Real64 TmedDV;   // Average temperature for DV
        Real64 Z1;       // auxiliary var for lowest height
        Real64 Z2;       // auxiliary var for highest height
        Real64 ZSupSurf; // highest height for this surface
        Real64 ZInfSurf; // lowest height for this surface
        Real64 HLU;      // Convection coefficient for the upper area of surface
        Real64 LayH;     // Height of the Occupied/Mixed subzone interface
        Real64 LayFrac;  // Fraction height of the Occupied/Mixed subzone interface
        int SurfNum;     // Surface number

        HAT_MX = 0.0;
        HAT_OC = 0.0;
        HA_MX = 0.0;
        HA_OC = 0.0;
        HAT_FLOOR = 0.0;
        HA_FLOOR = 0.0;
        // Is the air flow model for this zone set to UCSDDV Displacement Ventilation?
        if (state.dataRoomAirMod->IsZoneDV(ZoneNum)) {
            LayFrac = FractionHeight;
            LayH = FractionHeight * (state.dataRoomAirMod->ZoneCeilingHeight((ZoneNum - 1) * 2 + 2) - state.dataRoomAirMod->ZoneCeilingHeight((ZoneNum - 1) * 2 + 1));
            // WALL Hc, HA and HAT calculation
            for (Ctd = PosZ_Wall((ZoneNum - 1) * 2 + 1); Ctd <= PosZ_Wall((ZoneNum - 1) * 2 + 2); ++Ctd) {
                SurfNum = APos_Wall(Ctd);
                Surface(SurfNum).TAirRef = AdjacentAirTemp;
                if (SurfNum == 0) continue;
                Z1 = minval(Surface(SurfNum).Vertex({1, Surface(SurfNum).Sides}), &Vector::z);
                Z2 = maxval(Surface(SurfNum).Vertex({1, Surface(SurfNum).Sides}), &Vector::z);
                ZSupSurf = Z2 - state.dataRoomAirMod->ZoneCeilingHeight((ZoneNum - 1) * 2 + 1);
                ZInfSurf = Z1 - state.dataRoomAirMod->ZoneCeilingHeight((ZoneNum - 1) * 2 + 1);

                // The Wall surface is in the upper subzone
                if (ZInfSurf > LayH) {
                    TempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTMX(ZoneNum);
                    CalcDetailedHcInForDVModel(state, SurfNum, TempSurfIn, state.dataRoomAirMod->DVHcIn);
                    HWall(Ctd) = state.dataRoomAirMod->DVHcIn(SurfNum);
                    HAT_MX += Surface(SurfNum).Area * TempSurfIn(SurfNum) * HWall(Ctd);
                    HA_MX += Surface(SurfNum).Area * HWall(Ctd);
                }

                // The Wall surface is in the lower subzone
                if (ZSupSurf < LayH) {
                    TempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTOC(ZoneNum);
                    CalcDetailedHcInForDVModel(state, SurfNum, TempSurfIn, state.dataRoomAirMod->DVHcIn);
                    HWall(Ctd) = state.dataRoomAirMod->DVHcIn(SurfNum);
                    HAT_OC += Surface(SurfNum).Area * TempSurfIn(SurfNum) * HWall(Ctd);
                    HA_OC += Surface(SurfNum).Area * HWall(Ctd);
                }

                // The Wall surface is partially in upper and partially in lower subzone
                if (ZInfSurf <= LayH && ZSupSurf >= LayH) {
                    TempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTMX(ZoneNum);
                    CalcDetailedHcInForDVModel(state, SurfNum, TempSurfIn, state.dataRoomAirMod->DVHcIn);
                    HLU = state.dataRoomAirMod->DVHcIn(SurfNum);
                    TempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTOC(ZoneNum);
                    CalcDetailedHcInForDVModel(state, SurfNum, TempSurfIn, state.dataRoomAirMod->DVHcIn);
                    HLD = state.dataRoomAirMod->DVHcIn(SurfNum);
                    TmedDV = ((ZSupSurf - LayH) * state.dataRoomAirMod->ZTMX(ZoneNum) + (LayH - ZInfSurf) * state.dataRoomAirMod->ZTOC(ZoneNum)) / (ZSupSurf - ZInfSurf);
                    HWall(Ctd) = ((LayH - ZInfSurf) * HLD + (ZSupSurf - LayH) * HLU) / (ZSupSurf - ZInfSurf);
                    HAT_MX += Surface(SurfNum).Area * (ZSupSurf - LayH) / (ZSupSurf - ZInfSurf) * TempSurfIn(SurfNum) * HLU;
                    HA_MX += Surface(SurfNum).Area * (ZSupSurf - LayH) / (ZSupSurf - ZInfSurf) * HLU;
                    HAT_OC += Surface(SurfNum).Area * (LayH - ZInfSurf) / (ZSupSurf - ZInfSurf) * TempSurfIn(SurfNum) * HLD;
                    HA_OC += Surface(SurfNum).Area * (LayH - ZInfSurf) / (ZSupSurf - ZInfSurf) * HLD;
                    TempEffBulkAir(SurfNum) = TmedDV;
                }

                state.dataRoomAirMod->DVHcIn(SurfNum) = HWall(Ctd);

            } // END WALL

            // WINDOW Hc, HA and HAT CALCULATION
            for (Ctd = PosZ_Window((ZoneNum - 1) * 2 + 1); Ctd <= PosZ_Window((ZoneNum - 1) * 2 + 2); ++Ctd) {
                SurfNum = APos_Window(Ctd);
                Surface(SurfNum).TAirRef = AdjacentAirTemp;
                if (SurfNum == 0) continue;
                if (Surface(SurfNum).Tilt > 10.0 && Surface(SurfNum).Tilt < 170.0) { // Window Wall
                    Z1 = minval(Surface(SurfNum).Vertex({1, Surface(SurfNum).Sides}), &Vector::z);
                    Z2 = maxval(Surface(SurfNum).Vertex({1, Surface(SurfNum).Sides}), &Vector::z);
                    ZSupSurf = Z2 - state.dataRoomAirMod->ZoneCeilingHeight((ZoneNum - 1) * 2 + 1);
                    ZInfSurf = Z1 - state.dataRoomAirMod->ZoneCeilingHeight((ZoneNum - 1) * 2 + 1);

                    if (ZInfSurf > LayH) {
                        TempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTMX(ZoneNum);
                        CalcDetailedHcInForDVModel(state, SurfNum, TempSurfIn, state.dataRoomAirMod->DVHcIn);
                        HWindow(Ctd) = state.dataRoomAirMod->DVHcIn(SurfNum);
                        HAT_MX += Surface(SurfNum).Area * TempSurfIn(SurfNum) * HWindow(Ctd);
                        HA_MX += Surface(SurfNum).Area * HWindow(Ctd);
                    }

                    if (ZSupSurf < LayH) {
                        TempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTOC(ZoneNum);
                        CalcDetailedHcInForDVModel(state, SurfNum, TempSurfIn, state.dataRoomAirMod->DVHcIn);
                        HWindow(Ctd) = state.dataRoomAirMod->DVHcIn(SurfNum);
                        HAT_OC += Surface(SurfNum).Area * TempSurfIn(SurfNum) * HWindow(Ctd);
                        HA_OC += Surface(SurfNum).Area * HWindow(Ctd);
                    }

                    if (ZInfSurf <= LayH && ZSupSurf >= LayH) {
                        TempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTMX(ZoneNum);
                        CalcDetailedHcInForDVModel(state, SurfNum, TempSurfIn, state.dataRoomAirMod->DVHcIn);
                        HLU = state.dataRoomAirMod->DVHcIn(SurfNum);
                        TempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTOC(ZoneNum);
                        CalcDetailedHcInForDVModel(state, SurfNum, TempSurfIn, state.dataRoomAirMod->DVHcIn);
                        HLD = state.dataRoomAirMod->DVHcIn(SurfNum);
                        TmedDV = ((ZSupSurf - LayH) * state.dataRoomAirMod->ZTMX(ZoneNum) + (LayH - ZInfSurf) * state.dataRoomAirMod->ZTOC(ZoneNum)) / (ZSupSurf - ZInfSurf);
                        HWindow(Ctd) = ((LayH - ZInfSurf) * HLD + (ZSupSurf - LayH) * HLU) / (ZSupSurf - ZInfSurf);
                        HAT_MX += Surface(SurfNum).Area * (ZSupSurf - LayH) / (ZSupSurf - ZInfSurf) * TempSurfIn(SurfNum) * HLU;
                        HA_MX += Surface(SurfNum).Area * (ZSupSurf - LayH) / (ZSupSurf - ZInfSurf) * HLU;
                        HAT_OC += Surface(SurfNum).Area * (LayH - ZInfSurf) / (ZSupSurf - ZInfSurf) * TempSurfIn(SurfNum) * HLD;
                        HA_OC += Surface(SurfNum).Area * (LayH - ZInfSurf) / (ZSupSurf - ZInfSurf) * HLD;
                        TempEffBulkAir(SurfNum) = TmedDV;
                    }
                }

                if (Surface(SurfNum).Tilt <= 10.0) { // Window Ceiling
                    TempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTMX(ZoneNum);
                    CalcDetailedHcInForDVModel(state, SurfNum, TempSurfIn, state.dataRoomAirMod->DVHcIn);
                    HWindow(Ctd) = state.dataRoomAirMod->DVHcIn(SurfNum);
                    HAT_MX += Surface(SurfNum).Area * TempSurfIn(SurfNum) * HWindow(Ctd);
                    HA_MX += Surface(SurfNum).Area * HWindow(Ctd);
                }

                if (Surface(SurfNum).Tilt >= 170.0) { // Window Floor
                    TempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTOC(ZoneNum);
                    CalcDetailedHcInForDVModel(state, SurfNum, TempSurfIn, state.dataRoomAirMod->DVHcIn);
                    HWindow(Ctd) = state.dataRoomAirMod->DVHcIn(SurfNum);
                    HAT_OC += Surface(SurfNum).Area * TempSurfIn(SurfNum) * HWindow(Ctd);
                    HA_OC += Surface(SurfNum).Area * HWindow(Ctd);
                }

                state.dataRoomAirMod->DVHcIn(SurfNum) = HWindow(Ctd);

            } // END WINDOW

            // DOOR Hc, HA and HAT CALCULATION
            for (Ctd = PosZ_Door((ZoneNum - 1) * 2 + 1); Ctd <= PosZ_Door((ZoneNum - 1) * 2 + 2); ++Ctd) { // DOOR
                SurfNum = APos_Door(Ctd);
                Surface(SurfNum).TAirRef = AdjacentAirTemp;
                if (SurfNum == 0) continue;
                if (Surface(SurfNum).Tilt > 10.0 && Surface(SurfNum).Tilt < 170.0) { // Door Wall
                    Z1 = minval(Surface(SurfNum).Vertex({1, Surface(SurfNum).Sides}), &Vector::z);
                    Z2 = maxval(Surface(SurfNum).Vertex({1, Surface(SurfNum).Sides}), &Vector::z);
                    ZSupSurf = Z2 - state.dataRoomAirMod->ZoneCeilingHeight((ZoneNum - 1) * 2 + 1);
                    ZInfSurf = Z1 - state.dataRoomAirMod->ZoneCeilingHeight((ZoneNum - 1) * 2 + 1);

                    if (ZInfSurf > LayH) {
                        TempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTMX(ZoneNum);
                        CalcDetailedHcInForDVModel(state, SurfNum, TempSurfIn, state.dataRoomAirMod->DVHcIn);
                        HDoor(Ctd) = state.dataRoomAirMod->DVHcIn(SurfNum);
                        HAT_MX += Surface(SurfNum).Area * TempSurfIn(SurfNum) * HDoor(Ctd);
                        HA_MX += Surface(SurfNum).Area * HDoor(Ctd);
                    }

                    if (ZSupSurf < LayH) {
                        TempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTOC(ZoneNum);
                        CalcDetailedHcInForDVModel(state, SurfNum, TempSurfIn, state.dataRoomAirMod->DVHcIn);
                        HDoor(Ctd) = state.dataRoomAirMod->DVHcIn(SurfNum);
                        HAT_OC += Surface(SurfNum).Area * TempSurfIn(SurfNum) * HDoor(Ctd);
                        HA_OC += Surface(SurfNum).Area * HDoor(Ctd);
                    }

                    if (ZInfSurf <= LayH && ZSupSurf >= LayH) {
                        TempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTMX(ZoneNum);
                        CalcDetailedHcInForDVModel(state, SurfNum, TempSurfIn, state.dataRoomAirMod->DVHcIn);
                        HLU = state.dataRoomAirMod->DVHcIn(SurfNum);
                        TempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTOC(ZoneNum);
                        CalcDetailedHcInForDVModel(state, SurfNum, TempSurfIn, state.dataRoomAirMod->DVHcIn);
                        HLD = state.dataRoomAirMod->DVHcIn(SurfNum);
                        TmedDV = ((ZSupSurf - LayH) * state.dataRoomAirMod->ZTMX(ZoneNum) + (LayH - ZInfSurf) * state.dataRoomAirMod->ZTOC(ZoneNum)) / (ZSupSurf - ZInfSurf);
                        HDoor(Ctd) = ((LayH - ZInfSurf) * HLD + (ZSupSurf - LayH) * HLU) / (ZSupSurf - ZInfSurf);
                        HAT_MX += Surface(SurfNum).Area * (ZSupSurf - LayH) / (ZSupSurf - ZInfSurf) * TempSurfIn(SurfNum) * HLU;
                        HA_MX += Surface(SurfNum).Area * (ZSupSurf - LayH) / (ZSupSurf - ZInfSurf) * HLU;
                        HAT_OC += Surface(SurfNum).Area * (LayH - ZInfSurf) / (ZSupSurf - ZInfSurf) * TempSurfIn(SurfNum) * HLD;
                        HA_OC += Surface(SurfNum).Area * (LayH - ZInfSurf) / (ZSupSurf - ZInfSurf) * HLD;
                        TempEffBulkAir(SurfNum) = TmedDV;
                    }
                }

                if (Surface(SurfNum).Tilt <= 10.0) { // Door Ceiling
                    TempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTMX(ZoneNum);
                    CalcDetailedHcInForDVModel(state, SurfNum, TempSurfIn, state.dataRoomAirMod->DVHcIn);
                    HDoor(Ctd) = state.dataRoomAirMod->DVHcIn(SurfNum);
                    HAT_MX += Surface(SurfNum).Area * TempSurfIn(SurfNum) * HDoor(Ctd);
                    HA_MX += Surface(SurfNum).Area * HDoor(Ctd);
                }

                if (Surface(SurfNum).Tilt >= 170.0) { // Door Floor
                    TempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTOC(ZoneNum);
                    CalcDetailedHcInForDVModel(state, SurfNum, TempSurfIn, state.dataRoomAirMod->DVHcIn);
                    HDoor(Ctd) = state.dataRoomAirMod->DVHcIn(SurfNum);
                    HAT_OC += Surface(SurfNum).Area * TempSurfIn(SurfNum) * HDoor(Ctd);
                    HA_OC += Surface(SurfNum).Area * HDoor(Ctd);
                }

                state.dataRoomAirMod->DVHcIn(SurfNum) = HDoor(Ctd);

            } // END DOOR

            // INTERNAL Hc, HA and HAT CALCULATION
            HeightIntMass = min(HeightIntMassDefault, (state.dataRoomAirMod->ZoneCeilingHeight((ZoneNum - 1) * 2 + 2) - state.dataRoomAirMod->ZoneCeilingHeight((ZoneNum - 1) * 2 + 1)));
            for (Ctd = PosZ_Internal((ZoneNum - 1) * 2 + 1); Ctd <= PosZ_Internal((ZoneNum - 1) * 2 + 2); ++Ctd) {
                SurfNum = APos_Internal(Ctd);
                Surface(SurfNum).TAirRef = AdjacentAirTemp;
                if (SurfNum == 0) continue;
                ZSupSurf = HeightIntMass;
                ZInfSurf = 0.0;

                if (ZSupSurf < LayH) {
                    TempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTOC(ZoneNum);
                    CalcDetailedHcInForDVModel(state, SurfNum, TempSurfIn, state.dataRoomAirMod->DVHcIn);
                    HInternal(Ctd) = state.dataRoomAirMod->DVHcIn(SurfNum);
                    HAT_OC += Surface(SurfNum).Area * TempSurfIn(SurfNum) * HInternal(Ctd);
                    HA_OC += Surface(SurfNum).Area * HInternal(Ctd);
                }

                if (ZInfSurf <= LayH && ZSupSurf >= LayH) {
                    TempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTMX(ZoneNum);
                    CalcDetailedHcInForDVModel(state, SurfNum, TempSurfIn, state.dataRoomAirMod->DVHcIn);
                    HLU = state.dataRoomAirMod->DVHcIn(SurfNum);
                    TempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTOC(ZoneNum);
                    CalcDetailedHcInForDVModel(state, SurfNum, TempSurfIn, state.dataRoomAirMod->DVHcIn);
                    HLD = state.dataRoomAirMod->DVHcIn(SurfNum);
                    TmedDV = ((ZSupSurf - LayH) * state.dataRoomAirMod->ZTMX(ZoneNum) + (LayH - ZInfSurf) * state.dataRoomAirMod->ZTOC(ZoneNum)) / (ZSupSurf - ZInfSurf);
                    HInternal(Ctd) = ((LayH - ZInfSurf) * HLD + (ZSupSurf - LayH) * HLU) / (ZSupSurf - ZInfSurf);
                    HAT_MX += Surface(SurfNum).Area * (ZSupSurf - LayH) / (ZSupSurf - ZInfSurf) * TempSurfIn(SurfNum) * HLU;
                    HA_MX += Surface(SurfNum).Area * (ZSupSurf - LayH) / (ZSupSurf - ZInfSurf) * HLU;
                    HAT_OC += Surface(SurfNum).Area * (LayH - ZInfSurf) / (ZSupSurf - ZInfSurf) * TempSurfIn(SurfNum) * HLD;
                    HA_OC += Surface(SurfNum).Area * (LayH - ZInfSurf) / (ZSupSurf - ZInfSurf) * HLD;
                    TempEffBulkAir(SurfNum) = TmedDV;
                }

                state.dataRoomAirMod->DVHcIn(SurfNum) = HInternal(Ctd);
            } // END INTERNAL

            // CEILING Hc, HA and HAT CALCULATION
            for (Ctd = PosZ_Ceiling((ZoneNum - 1) * 2 + 1); Ctd <= PosZ_Ceiling((ZoneNum - 1) * 2 + 2); ++Ctd) {
                SurfNum = APos_Ceiling(Ctd);
                Surface(SurfNum).TAirRef = AdjacentAirTemp;
                if (SurfNum == 0) continue;
                TempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTMX(ZoneNum);
                CalcDetailedHcInForDVModel(state, SurfNum, TempSurfIn, state.dataRoomAirMod->DVHcIn);
                HCeiling(Ctd) = state.dataRoomAirMod->DVHcIn(SurfNum);
                HAT_MX += Surface(SurfNum).Area * TempSurfIn(SurfNum) * HCeiling(Ctd);
                HA_MX += Surface(SurfNum).Area * HCeiling(Ctd);
                state.dataRoomAirMod->DVHcIn(SurfNum) = HCeiling(Ctd);
            } // END CEILING

            // FLOOR Hc, HA and HAT CALCULATION
            for (Ctd = PosZ_Floor((ZoneNum - 1) * 2 + 1); Ctd <= PosZ_Floor((ZoneNum - 1) * 2 + 2); ++Ctd) {
                SurfNum = APos_Floor(Ctd);
                Surface(SurfNum).TAirRef = AdjacentAirTemp;
                if (SurfNum == 0) continue;
                TempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTFloor(ZoneNum);
                CalcDetailedHcInForDVModel(state, SurfNum, TempSurfIn, state.dataRoomAirMod->DVHcIn);
                HFloor(Ctd) = state.dataRoomAirMod->DVHcIn(SurfNum);
                HAT_FLOOR += Surface(SurfNum).Area * TempSurfIn(SurfNum) * HFloor(Ctd);
                HA_FLOOR += Surface(SurfNum).Area * HFloor(Ctd);
                TempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTFloor(ZoneNum);
                state.dataRoomAirMod->DVHcIn(SurfNum) = HFloor(Ctd);
            } // END FLOOR
        }
    }

    //**************************************************************************************************

    Real64 calculateThirdOrderFloorTemperature(Real64 temperatureHistoryTerm,
                                               Real64 HAT_floor,
                                               Real64 HA_floor,
                                               Real64 MCpT_Total,
                                               Real64 MCp_Total,
                                               Real64 occupiedTemp,
                                               Real64 nonAirSystemResponse,
                                               Real64 zoneMultiplier,
                                               Real64 airCap)
    {
        static const Real64 elevenOverSix = 11.0 / 6.0;
        return (temperatureHistoryTerm + HAT_floor + MCpT_Total + 0.6 * occupiedTemp * MCp_Total + nonAirSystemResponse / zoneMultiplier) /
               (elevenOverSix * airCap + HA_floor + 1.6 * MCp_Total);
    }

    void CalcUCSDDV(EnergyPlusData &state, int const ZoneNum) // Which Zonenum
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         G. Carrilho da Graca
        //       DATE WRITTEN   February 2004
        //       MODIFIED       Brent Griffith June 2008 for new interpolation and time history
        //       RE-ENGINEERED  -

        // PURPOSE OF THIS SUBROUTINE:
        // Subroutine for displacement ventilation modelling.
        // This subroutine calculates the mixed subzone height, surface heat transfer coefficients and
        // room air equivalent temperatures and three space temperatures (floor subzone, occupied zone and upper,
        // mixed subzone temperature)

        // REFERENCES:
        // Model developed by Paul Linden (UCSD), G. Carrilho da Graca (UCSD) and P. Haves (LBL).
        // Work funded by the California Energy Comission. More information on the model can found in:
        // "Simplified Models for Heat Transfer in Rooms" G. Carrilho da Graca, Ph.D. thesis UCSD. December 2003.

        // Using/Aliasing
        using namespace DataHeatBalFanSys;
        using namespace DataEnvironment;
        using namespace DataHeatBalance;
        using DataHVACGlobals::TimeStepSys;
        using DataHVACGlobals::UseZoneTimeStepHistory;
        using DataZoneEquipment::ZoneEquipConfig;
        using InternalHeatGains::SumInternalConvectionGainsByTypes;
        using InternalHeatGains::SumReturnAirConvectionGainsByTypes;
        using Psychrometrics::PsyCpAirFnW;
        using Psychrometrics::PsyRhoAirFnPbTdbW;
        using ScheduleManager::GetCurrentScheduleValue;
        using ScheduleManager::GetScheduleIndex;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static Real64 const OneThird(1.0 / 3.0);
        static Real64 const MinFlow_pow_fac(std::pow(1.0 / 24.55 * 1.0, 1.0 / 0.6));

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 HeightFrac;               // Fractional height of transition between occupied and mixed subzones
        Real64 GainsFrac;                // Fraction of lower subzone internal gains that mix as opposed to forming plumes
        Real64 ConvGains;                // Total convective gains in the room
        Real64 ConvGainsOccupiedSubzone; // Total convective gains released in occupied subzone
        Real64 ConvGainsMixedSubzone;    // Total convective gains released in mixed subzone
        Real64 MCp_Total;                // Total capacity rate into the zone - assumed to enter at low level
        Real64 ZTAveraged;
        Real64 TempDiffCritRep; // Minimum temperature difference between mixed and occupied subzones for reporting
        bool MIXFLAG;
        int Ctd;
        Real64 MinFlow;
        Real64 NumPLPP; // Number of plumes per person
        int NumberOfOccupants;
        Real64 MTGAUX;
        int ZoneEquipConfigNum;
        int NodeNum;
        Real64 PowerInPlumes;
        Real64 SumSysMCp;
        Real64 SumSysMCpT;
        Real64 NodeTemp;
        Real64 MassFlowRate;
        Real64 CpAir;
        Real64 MCpT_Total;
        int ZoneNodeNum; // index number of the zone node
        Real64 NumberOfPlumes;
        Real64 SumMCp;
        Real64 SumMCpT;
        Real64 AirCap;
        Real64 TempHistTerm;
        Real64 PowerPerPlume;
        Real64 HeightMixedSubzoneAve;    // Height of center of mixed air subzone
        Real64 HeightOccupiedSubzoneAve; // Height of center of occupied air subzone
        Real64 HeightFloorSubzoneAve;    // Height of center of floor air subzone
        Real64 HeightThermostat;         // Height of center of thermostat/temperature control sensor
        Real64 HeightComfort;            // Height at which air temperature value is used to calculate comfort
        Real64 CeilingHeight;
        Real64 ZoneMult; // total zone multiplier
        int Loop;
        int FlagApertures;
        static Real64 TempDepCoef(0.0); // Formerly CoefSumha, coef in zone temp equation with dimensions of h*A
        static Real64 TempIndCoef(0.0); // Formerly CoefSumhat, coef in zone temp equation with dimensions of h*A(T1
        static Array1D_int IntGainTypesOccupied(29,
                                                {IntGainTypeOf_People,
                                                 IntGainTypeOf_WaterHeaterMixed,
                                                 IntGainTypeOf_WaterHeaterStratified,
                                                 IntGainTypeOf_ThermalStorageChilledWaterMixed,
                                                 IntGainTypeOf_ThermalStorageChilledWaterStratified,
                                                 IntGainTypeOf_ElectricEquipment,
                                                 IntGainTypeOf_ElectricEquipmentITEAirCooled,
                                                 IntGainTypeOf_GasEquipment,
                                                 IntGainTypeOf_HotWaterEquipment,
                                                 IntGainTypeOf_SteamEquipment,
                                                 IntGainTypeOf_OtherEquipment,
                                                 IntGainTypeOf_ZoneBaseboardOutdoorTemperatureControlled,
                                                 IntGainTypeOf_GeneratorFuelCell,
                                                 IntGainTypeOf_WaterUseEquipment,
                                                 IntGainTypeOf_GeneratorMicroCHP,
                                                 IntGainTypeOf_ElectricLoadCenterTransformer,
                                                 IntGainTypeOf_ElectricLoadCenterInverterSimple,
                                                 IntGainTypeOf_ElectricLoadCenterInverterFunctionOfPower,
                                                 IntGainTypeOf_ElectricLoadCenterInverterLookUpTable,
                                                 IntGainTypeOf_ElectricLoadCenterStorageBattery,
                                                 IntGainTypeOf_ElectricLoadCenterStorageSimple,
                                                 IntGainTypeOf_PipeIndoor,
                                                 IntGainTypeOf_RefrigerationCase,
                                                 IntGainTypeOf_RefrigerationCompressorRack,
                                                 IntGainTypeOf_RefrigerationSystemAirCooledCondenser,
                                                 IntGainTypeOf_RefrigerationSystemSuctionPipe,
                                                 IntGainTypeOf_RefrigerationSecondaryReceiver,
                                                 IntGainTypeOf_RefrigerationSecondaryPipe,
                                                 IntGainTypeOf_RefrigerationWalkIn});

        static Array1D_int IntGainTypesMixedSubzone(2, {IntGainTypeOf_DaylightingDeviceTubular, IntGainTypeOf_Lights});
        Real64 RetAirGain;

        // Exact solution or Euler method
        if (ZoneAirSolutionAlgo != Use3rdOrder) {
            if (ShortenTimeStepSysRoomAir && TimeStepSys < state.dataGlobal->TimeStepZone) {
                if (PreviousTimeStep < state.dataGlobal->TimeStepZone) {
                    state.dataRoomAirMod->Zone1Floor(ZoneNum) = state.dataRoomAirMod->ZoneM2Floor(ZoneNum);
                    state.dataRoomAirMod->Zone1OC(ZoneNum) = state.dataRoomAirMod->ZoneM2OC(ZoneNum);
                    state.dataRoomAirMod->Zone1MX(ZoneNum) = state.dataRoomAirMod->ZoneM2MX(ZoneNum);
                } else {
                    state.dataRoomAirMod->Zone1Floor(ZoneNum) = state.dataRoomAirMod->ZoneMXFloor(ZoneNum);
                    state.dataRoomAirMod->Zone1OC(ZoneNum) = state.dataRoomAirMod->ZoneMXOC(ZoneNum);
                    state.dataRoomAirMod->Zone1MX(ZoneNum) = state.dataRoomAirMod->ZoneMXMX(ZoneNum);
                }
            } else {
                state.dataRoomAirMod->Zone1Floor(ZoneNum) = state.dataRoomAirMod->ZTFloor(ZoneNum);
                state.dataRoomAirMod->Zone1OC(ZoneNum) = state.dataRoomAirMod->ZTOC(ZoneNum);
                state.dataRoomAirMod->Zone1MX(ZoneNum) = state.dataRoomAirMod->ZTMX(ZoneNum);
            }
        }

        MIXFLAG = false;
        FlagApertures = 1;
        state.dataRoomAirMod->DVHcIn = HConvIn;
        CeilingHeight = state.dataRoomAirMod->ZoneCeilingHeight((ZoneNum - 1) * 2 + 2) - state.dataRoomAirMod->ZoneCeilingHeight((ZoneNum - 1) * 2 + 1);
        ZoneMult = Zone(ZoneNum).Multiplier * Zone(ZoneNum).ListMultiplier;

        for (Ctd = 1; Ctd <= state.dataRoomAirMod->TotUCSDDV; ++Ctd) {
            if (ZoneNum == state.dataRoomAirMod->ZoneUCSDDV(Ctd).ZonePtr) {
                GainsFrac = GetCurrentScheduleValue(state, state.dataRoomAirMod->ZoneUCSDDV(Ctd).SchedGainsPtr);
                NumPLPP = state.dataRoomAirMod->ZoneUCSDDV(Ctd).NumPlumesPerOcc;
                HeightThermostat = state.dataRoomAirMod->ZoneUCSDDV(Ctd).ThermostatHeight;
                HeightComfort = state.dataRoomAirMod->ZoneUCSDDV(Ctd).ComfortHeight;
                TempDiffCritRep = state.dataRoomAirMod->ZoneUCSDDV(Ctd).TempTrigger;
            }
        }

        SumInternalConvectionGainsByTypes(ZoneNum, IntGainTypesOccupied, ConvGainsOccupiedSubzone);

        ConvGainsOccupiedSubzone += 0.5 * SysDepZoneLoadsLagged(ZoneNum);

        // Add heat to return air if zonal system (no return air) or cycling system (return air frequently very
        // low or zero)
        if (Zone(ZoneNum).NoHeatToReturnAir) {
            SumReturnAirConvectionGainsByTypes(ZoneNum, IntGainTypesOccupied, RetAirGain);
            ConvGainsOccupiedSubzone += RetAirGain;
        }

        SumInternalConvectionGainsByTypes(ZoneNum, IntGainTypesMixedSubzone, ConvGainsMixedSubzone);
        ConvGainsMixedSubzone += SumConvHTRadSys(ZoneNum) + SumConvPool(ZoneNum) + 0.5 * SysDepZoneLoadsLagged(ZoneNum);
        if (Zone(ZoneNum).NoHeatToReturnAir) {
            SumReturnAirConvectionGainsByTypes(ZoneNum, IntGainTypesMixedSubzone, RetAirGain);
            ConvGainsMixedSubzone += RetAirGain;
        }

        ConvGains = ConvGainsOccupiedSubzone + ConvGainsMixedSubzone;

        //=================== Entering air system temperature and flow====================
        SumSysMCp = 0.0;
        SumSysMCpT = 0.0;
        // Check to make sure if this is a controlled zone and determine ZoneEquipConfigNum
        ZoneEquipConfigNum = ZoneNum;
        if (ZoneEquipConfig(ZoneEquipConfigNum).IsControlled) {
            for (NodeNum = 1; NodeNum <= ZoneEquipConfig(ZoneEquipConfigNum).NumInletNodes; ++NodeNum) {
                NodeTemp = Node(ZoneEquipConfig(ZoneEquipConfigNum).InletNode(NodeNum)).Temp;
                MassFlowRate = Node(ZoneEquipConfig(ZoneEquipConfigNum).InletNode(NodeNum)).MassFlowRate;
                CpAir = PsyCpAirFnW(ZoneAirHumRat(ZoneNum));
                SumSysMCp += MassFlowRate * CpAir;
                SumSysMCpT += MassFlowRate * CpAir * NodeTemp;
            }
        }

        SumMCp = MCPI(ZoneNum) + MCPV(ZoneNum) + MCPM(ZoneNum) + MCPE(ZoneNum) + MCPC(ZoneNum) + MDotCPOA(ZoneNum);
        SumMCpT =
            MCPTI(ZoneNum) + MCPTV(ZoneNum) + MCPTM(ZoneNum) + MCPTE(ZoneNum) + MCPTC(ZoneNum) + MDotCPOA(ZoneNum) * Zone(ZoneNum).OutDryBulbTemp;
        if (AirflowNetwork::SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlMultizone) {
            SumMCp = state.dataAirflowNetworkBalanceManager->exchangeData(ZoneNum).SumMCp + state.dataAirflowNetworkBalanceManager->exchangeData(ZoneNum).SumMMCp;
            SumMCpT = state.dataAirflowNetworkBalanceManager->exchangeData(ZoneNum).SumMCpT + state.dataAirflowNetworkBalanceManager->exchangeData(ZoneNum).SumMMCpT;
        }

        MCp_Total = SumMCp + SumSysMCp;
        MCpT_Total = SumMCpT + SumSysMCpT;

        if (TotPeople > 0) {
            NumberOfOccupants = 0;
            NumberOfPlumes = 0.0;
            for (Ctd = 1; Ctd <= TotPeople; ++Ctd) {
                if (People(Ctd).ZonePtr == ZoneNum) {
                    NumberOfOccupants += People(Ctd).NumberOfPeople; // *GetCurrentScheduleValue(state, People(Ctd)%NumberOfPeoplePtr)
                    NumberOfPlumes = NumberOfOccupants * NumPLPP;
                }
            }
            if (NumberOfPlumes == 0.0) {
                NumberOfPlumes = 1.0;
            }
            PowerInPlumes = (1.0 - GainsFrac) * ConvGainsOccupiedSubzone;
            PowerPerPlume = PowerInPlumes / NumberOfPlumes;
        } else {
            NumberOfPlumes = 1.0;
            PowerInPlumes = (1.0 - GainsFrac) * ConvGainsOccupiedSubzone;
            PowerPerPlume = PowerInPlumes / NumberOfPlumes;
        }

        // When AirflowNetwork is used verify if bottom apertures are inflowing and upper apertures are
        // outflowing. The lower apertures have to be located below 0.8m and the upper apertures
        // have to be located above 1.8m.

        if (AirflowNetwork::NumOfLinksMultiZone > 0) {
            for (Loop = 1; Loop <= state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(0, ZoneNum); ++Loop) {
                // direct AirflowNetwork surface

                if (Surface(AirflowNetwork::MultizoneSurfaceData(state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(Loop, ZoneNum)).SurfNum).Zone == ZoneNum) {

                    if ((state.dataRoomAirMod->SurfParametersCVDV(state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(Loop, ZoneNum)).Zmax < 0.8 &&
                         AirflowNetwork::AirflowNetworkLinkSimu(state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(Loop, ZoneNum)).VolFLOW > 0)) {
                        FlagApertures = 0;
                        break;
                    }
                    if (state.dataRoomAirMod->SurfParametersCVDV(state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(Loop, ZoneNum)).Zmin > 1.8 &&
                        AirflowNetwork::AirflowNetworkLinkSimu(state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(Loop, ZoneNum)).VolFLOW2 > 0) {
                        FlagApertures = 0;
                        break;
                    }

                    if ((state.dataRoomAirMod->SurfParametersCVDV(state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(Loop, ZoneNum)).Zmin > 0.8 &&
                            state.dataRoomAirMod->SurfParametersCVDV(state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(Loop, ZoneNum)).Zmin < 1.8) ||
                        (state.dataRoomAirMod->SurfParametersCVDV(state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(Loop, ZoneNum)).Zmax > 0.8 &&
                                state.dataRoomAirMod->SurfParametersCVDV(state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(Loop, ZoneNum)).Zmax < 1.8)) {
                        FlagApertures = 0;
                        break;
                    }
                    // indirect AirflowNetwork surface; this is an interzone surface
                } else {

                    if (state.dataRoomAirMod->SurfParametersCVDV(state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(Loop, ZoneNum)).Zmax +
                                Zone(Surface(AirflowNetwork::MultizoneSurfaceData(state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(Loop, ZoneNum)).SurfNum).Zone).OriginZ -
                                Zone(ZoneNum).OriginZ <
                            0.8 &&
                        AirflowNetwork::AirflowNetworkLinkSimu(state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(Loop, ZoneNum)).VolFLOW2 > 0) {
                        FlagApertures = 0;
                        break;
                    }
                    if (state.dataRoomAirMod->SurfParametersCVDV(state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(Loop, ZoneNum)).Zmin +
                                Zone(Surface(AirflowNetwork::MultizoneSurfaceData(state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(Loop, ZoneNum)).SurfNum).Zone).OriginZ -
                                Zone(ZoneNum).OriginZ >
                            1.8 &&
                        AirflowNetwork::AirflowNetworkLinkSimu(state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(Loop, ZoneNum)).VolFLOW > 0) {
                        FlagApertures = 0;
                        break;
                    }
                    if ((state.dataRoomAirMod->SurfParametersCVDV(state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(Loop, ZoneNum)).Zmin +
                                 Zone(Surface(AirflowNetwork::MultizoneSurfaceData(state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(Loop, ZoneNum)).SurfNum).Zone)
                                     .OriginZ -
                                 Zone(ZoneNum).OriginZ >
                             0.8 &&
                            state.dataRoomAirMod->SurfParametersCVDV(state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(Loop, ZoneNum)).Zmin +
                                 Zone(Surface(AirflowNetwork::MultizoneSurfaceData(state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(Loop, ZoneNum)).SurfNum).Zone)
                                     .OriginZ -
                                 Zone(ZoneNum).OriginZ <
                             1.8) ||
                        (state.dataRoomAirMod->SurfParametersCVDV(state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(Loop, ZoneNum)).Zmax +
                                 Zone(Surface(AirflowNetwork::MultizoneSurfaceData(state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(Loop, ZoneNum)).SurfNum).Zone)
                                     .OriginZ -
                                 Zone(ZoneNum).OriginZ >
                             0.8 &&
                                state.dataRoomAirMod->SurfParametersCVDV(state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(Loop, ZoneNum)).Zmax +
                                 Zone(Surface(AirflowNetwork::MultizoneSurfaceData(state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(Loop, ZoneNum)).SurfNum).Zone)
                                     .OriginZ -
                                 Zone(ZoneNum).OriginZ <
                             1.8)) {
                        FlagApertures = 0;
                        break;
                    }
                }
            }
        }

        if ((PowerInPlumes == 0.0) || (MCpT_Total == 0.0) || FlagApertures == 0) {
            // The system will mix
            HeightFrac = 0.0;
        } else {
            Real64 const plume_fac(NumberOfPlumes * std::pow(PowerPerPlume, OneThird));
            HeightFrac = min(24.55 * std::pow(MCp_Total * 0.000833 / plume_fac, 0.6) / CeilingHeight, 1.0);
            for (Ctd = 1; Ctd <= 4; ++Ctd) {
                HcUCSDDV(state, ZoneNum, HeightFrac);
                // HeightFrac = min( 24.55 * std::pow( MCp_Total * 0.000833 / ( NumberOfPlumes * std::pow( PowerPerPlume, OneThird ) ), 0.6 ) /
                // CeilingHeight, 1.0 ); //Tuned This does not vary in loop  EPTeam-replaces above (cause diffs)      HeightFrac =
                // MIN(24.55d0*(MCp_Total*0.000833d0/(NumberOfPlumes*PowerPerPlume**(1.0d0/3.d0)))**0.6 / CeilingHeight , 1.0d0)
                state.dataRoomAirMod->HeightTransition(ZoneNum) = HeightFrac * CeilingHeight;
                state.dataRoomAirMod->AIRRATFloor(ZoneNum) = Zone(ZoneNum).Volume * min(state.dataRoomAirMod->HeightTransition(ZoneNum), HeightFloorSubzoneTop) / CeilingHeight *
                                       Zone(ZoneNum).ZoneVolCapMultpSens *
                                       PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, state.dataRoomAirMod->MATFloor(ZoneNum), ZoneAirHumRat(ZoneNum)) *
                                       PsyCpAirFnW(ZoneAirHumRat(ZoneNum)) / (TimeStepSys * DataGlobalConstants::SecInHour);
                state.dataRoomAirMod->AIRRATOC(ZoneNum) = Zone(ZoneNum).Volume * (state.dataRoomAirMod->HeightTransition(ZoneNum) - min(state.dataRoomAirMod->HeightTransition(ZoneNum), 0.2)) / CeilingHeight *
                                    Zone(ZoneNum).ZoneVolCapMultpSens * PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, state.dataRoomAirMod->MATOC(ZoneNum), ZoneAirHumRat(ZoneNum)) *
                                    PsyCpAirFnW(ZoneAirHumRat(ZoneNum)) / (TimeStepSys * DataGlobalConstants::SecInHour);
                state.dataRoomAirMod->AIRRATMX(ZoneNum) = Zone(ZoneNum).Volume * (CeilingHeight - state.dataRoomAirMod->HeightTransition(ZoneNum)) / CeilingHeight *
                                    Zone(ZoneNum).ZoneVolCapMultpSens * PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, state.dataRoomAirMod->MATMX(ZoneNum), ZoneAirHumRat(ZoneNum)) *
                                    PsyCpAirFnW(ZoneAirHumRat(ZoneNum)) / (TimeStepSys * DataGlobalConstants::SecInHour);

                if (UseZoneTimeStepHistory) {
                    state.dataRoomAirMod->ZTM3Floor(ZoneNum) = state.dataRoomAirMod->XM3TFloor(ZoneNum);
                    state.dataRoomAirMod->ZTM2Floor(ZoneNum) = state.dataRoomAirMod->XM2TFloor(ZoneNum);
                    state.dataRoomAirMod->ZTM1Floor(ZoneNum) = state.dataRoomAirMod->XMATFloor(ZoneNum);

                    state.dataRoomAirMod->ZTM3OC(ZoneNum) = state.dataRoomAirMod->XM3TOC(ZoneNum);
                    state.dataRoomAirMod->ZTM2OC(ZoneNum) = state.dataRoomAirMod->XM2TOC(ZoneNum);
                    state.dataRoomAirMod->ZTM1OC(ZoneNum) = state.dataRoomAirMod->XMATOC(ZoneNum);

                    state.dataRoomAirMod->ZTM3MX(ZoneNum) = state.dataRoomAirMod->XM3TMX(ZoneNum);
                    state.dataRoomAirMod->ZTM2MX(ZoneNum) = state.dataRoomAirMod->XM2TMX(ZoneNum);
                    state.dataRoomAirMod->ZTM1MX(ZoneNum) = state.dataRoomAirMod->XMATMX(ZoneNum);

                } else {
                    state.dataRoomAirMod->ZTM3Floor(ZoneNum) = state.dataRoomAirMod->DSXM3TFloor(ZoneNum);
                    state.dataRoomAirMod->ZTM2Floor(ZoneNum) = state.dataRoomAirMod->DSXM2TFloor(ZoneNum);
                    state.dataRoomAirMod->ZTM1Floor(ZoneNum) = state.dataRoomAirMod->DSXMATFloor(ZoneNum);

                    state.dataRoomAirMod->ZTM3OC(ZoneNum) = state.dataRoomAirMod->DSXM3TOC(ZoneNum);
                    state.dataRoomAirMod->ZTM2OC(ZoneNum) = state.dataRoomAirMod->DSXM2TOC(ZoneNum);
                    state.dataRoomAirMod->ZTM1OC(ZoneNum) = state.dataRoomAirMod->DSXMATOC(ZoneNum);

                    state.dataRoomAirMod->ZTM3MX(ZoneNum) = state.dataRoomAirMod->DSXM3TMX(ZoneNum);
                    state.dataRoomAirMod->ZTM2MX(ZoneNum) = state.dataRoomAirMod->DSXM2TMX(ZoneNum);
                    state.dataRoomAirMod->ZTM1MX(ZoneNum) = state.dataRoomAirMod->DSXMATMX(ZoneNum);
                }

                AirCap = state.dataRoomAirMod->AIRRATFloor(ZoneNum);
                TempHistTerm = AirCap * (3.0 * state.dataRoomAirMod->ZTM1Floor(ZoneNum) - (3.0 / 2.0) * state.dataRoomAirMod->ZTM2Floor(ZoneNum) + OneThird * state.dataRoomAirMod->ZTM3Floor(ZoneNum));
                TempDepCoef = HA_FLOOR + MCp_Total;
                TempIndCoef = HAT_FLOOR + MCpT_Total + NonAirSystemResponse(ZoneNum) / ZoneMult;
                {
                    auto const SELECT_CASE_var(ZoneAirSolutionAlgo);
                    if (SELECT_CASE_var == Use3rdOrder) {
                        state.dataRoomAirMod->ZTFloor(ZoneNum) = calculateThirdOrderFloorTemperature(
                            TempHistTerm, HAT_FLOOR, HA_FLOOR, MCpT_Total, MCp_Total, state.dataRoomAirMod->ZTOC(ZoneNum), NonAirSystemResponse(ZoneNum), ZoneMult, AirCap);
                    } else if (SELECT_CASE_var == UseAnalyticalSolution) {
                        if (TempDepCoef == 0.0) { // B=0
                            state.dataRoomAirMod->ZTFloor(ZoneNum) = state.dataRoomAirMod->Zone1Floor(ZoneNum) + TempIndCoef / AirCap;
                        } else {
                            state.dataRoomAirMod->ZTFloor(ZoneNum) = (state.dataRoomAirMod->Zone1Floor(ZoneNum) - TempIndCoef / TempDepCoef) * std::exp(min(700.0, -TempDepCoef / AirCap)) +
                                               TempIndCoef / TempDepCoef;
                        }
                    } else if (SELECT_CASE_var == UseEulerMethod) {
                        state.dataRoomAirMod->ZTFloor(ZoneNum) = (AirCap * state.dataRoomAirMod->Zone1Floor(ZoneNum) + TempIndCoef) / (AirCap + TempDepCoef);
                    }
                }
                AirCap = state.dataRoomAirMod->AIRRATOC(ZoneNum);
                TempHistTerm = AirCap * (3.0 * state.dataRoomAirMod->ZTM1OC(ZoneNum) - (3.0 / 2.0) * state.dataRoomAirMod->ZTM2OC(ZoneNum) + OneThird * state.dataRoomAirMod->ZTM3OC(ZoneNum));
                TempDepCoef = HA_OC + MCp_Total;
                TempIndCoef = ConvGainsOccupiedSubzone * GainsFrac + HAT_OC + state.dataRoomAirMod->ZTFloor(ZoneNum) * MCp_Total;
                {
                    auto const SELECT_CASE_var(ZoneAirSolutionAlgo);
                    if (SELECT_CASE_var == Use3rdOrder) {
                        state.dataRoomAirMod->ZTOC(ZoneNum) = (TempHistTerm + ConvGainsOccupiedSubzone * GainsFrac + HAT_OC + 1.6 * state.dataRoomAirMod->ZTFloor(ZoneNum) * MCp_Total) /
                                        ((11.0 / 6.0) * AirCap + HA_OC + 1.6 * MCp_Total);
                    } else if (SELECT_CASE_var == UseAnalyticalSolution) {
                        if (TempDepCoef == 0.0) { // B=0
                            state.dataRoomAirMod->ZTOC(ZoneNum) = state.dataRoomAirMod->Zone1OC(ZoneNum) + TempIndCoef / AirCap;
                        } else {
                            if (AirCap == 0.0) {
                                state.dataRoomAirMod->ZTOC(ZoneNum) = TempIndCoef / TempDepCoef;
                            } else {
                                state.dataRoomAirMod->ZTOC(ZoneNum) = (state.dataRoomAirMod->Zone1OC(ZoneNum) - TempIndCoef / TempDepCoef) * std::exp(min(700.0, -TempDepCoef / AirCap)) +
                                                TempIndCoef / TempDepCoef;
                            }
                        }
                    } else if (SELECT_CASE_var == UseEulerMethod) {
                        state.dataRoomAirMod->ZTOC(ZoneNum) = (AirCap * state.dataRoomAirMod->Zone1OC(ZoneNum) + TempIndCoef) / (AirCap + TempDepCoef);
                    }
                }
                AirCap = state.dataRoomAirMod->AIRRATMX(ZoneNum);
                TempHistTerm = AirCap * (3.0 * state.dataRoomAirMod->ZTM1MX(ZoneNum) - (3.0 / 2.0) * state.dataRoomAirMod->ZTM2MX(ZoneNum) + OneThird * state.dataRoomAirMod->ZTM3MX(ZoneNum));
                TempDepCoef = HA_MX + MCp_Total;
                TempIndCoef = ConvGainsOccupiedSubzone * (1.0 - GainsFrac) + ConvGainsMixedSubzone + HAT_MX + state.dataRoomAirMod->ZTOC(ZoneNum) * MCp_Total;
                {
                    auto const SELECT_CASE_var(ZoneAirSolutionAlgo);
                    if (SELECT_CASE_var == Use3rdOrder) {
                        state.dataRoomAirMod->ZTMX(ZoneNum) = (TempHistTerm + ConvGainsOccupiedSubzone * (1.0 - GainsFrac) + ConvGainsMixedSubzone + HAT_MX +
                                state.dataRoomAirMod->ZTOC(ZoneNum) * MCp_Total) /
                                        ((11.0 / 6.0) * AirCap + HA_MX + MCp_Total);
                    } else if (SELECT_CASE_var == UseAnalyticalSolution) {
                        if (TempDepCoef == 0.0) { // B=0
                            state.dataRoomAirMod->ZTMX(ZoneNum) = state.dataRoomAirMod->Zone1MX(ZoneNum) + TempIndCoef / AirCap;
                        } else {
                            if (AirCap == 0.0) {
                                state.dataRoomAirMod->ZTMX(ZoneNum) = TempIndCoef / TempDepCoef;
                            } else {
                                state.dataRoomAirMod->ZTMX(ZoneNum) = (state.dataRoomAirMod->Zone1MX(ZoneNum) - TempIndCoef / TempDepCoef) * std::exp(min(700.0, -TempDepCoef / AirCap)) +
                                                TempIndCoef / TempDepCoef;
                            }
                        }
                    } else if (SELECT_CASE_var == UseEulerMethod) {
                        state.dataRoomAirMod->ZTMX(ZoneNum) = (AirCap * state.dataRoomAirMod->Zone1MX(ZoneNum) + TempIndCoef) / (AirCap + TempDepCoef);
                    }
                }
            }

            // MinFlow for interface layer at z = 1.0
            MinFlow = MinFlow_pow_fac * plume_fac;
            // EPTeam above replaces (cause diffs?)   MinFlow = (1.0d0/24.55d0*1.0d0)**(1.0d0/0.6d0)*NumberOfPlumes*PowerPerPlume**(1.0/3.0)
            if (MinFlow != 0.0) {
                state.dataRoomAirMod->FracMinFlow(ZoneNum) = MCp_Total * 0.000833 / MinFlow;
            } else {
                state.dataRoomAirMod->FracMinFlow(ZoneNum) = 9.999;
            }
            state.dataRoomAirMod->AirModel(ZoneNum).SimAirModel = true;
        }

        //=============================== M I X E D  Calculation ==============================================
        if (state.dataRoomAirMod->ZTMX(ZoneNum) < state.dataRoomAirMod->ZTOC(ZoneNum) || MCp_Total <= 0.0 || HeightFrac * CeilingHeight < (HeightFloorSubzoneTop + ThickOccupiedSubzoneMin)) {
            MIXFLAG = true;
            HeightFrac = 0.0;
            state.dataRoomAirMod->AvgTempGrad(ZoneNum) = 0.0;
            state.dataRoomAirMod->MaxTempGrad(ZoneNum) = 0.0;
            state.dataRoomAirMod->AirModel(ZoneNum).SimAirModel = false;
            AirCap = AIRRAT(ZoneNum);
            TempHistTerm = AirCap * (3.0 * ZTM1(ZoneNum) - (3.0 / 2.0) * ZTM2(ZoneNum) + OneThird * ZTM3(ZoneNum));

            for (Ctd = 1; Ctd <= 3; ++Ctd) {
                TempDepCoef = HA_MX + HA_OC + HA_FLOOR + MCp_Total;
                TempIndCoef = ConvGains + HAT_MX + HAT_OC + HAT_FLOOR + MCpT_Total;
                {
                    auto const SELECT_CASE_var(ZoneAirSolutionAlgo);
                    if (SELECT_CASE_var == Use3rdOrder) {
                        ZTAveraged = (TempHistTerm + ConvGains + HAT_MX + HAT_OC + HAT_FLOOR + MCpT_Total) /
                                     ((11.0 / 6.0) * AirCap + HA_MX + HA_OC + HA_FLOOR + MCp_Total);
                    } else if (SELECT_CASE_var == UseAnalyticalSolution) {
                        if (TempDepCoef == 0.0) { // B=0
                            ZTAveraged = ZoneT1(ZoneNum) + TempIndCoef / AirCap;
                        } else {
                            ZTAveraged = (ZoneT1(ZoneNum) - TempIndCoef / TempDepCoef) * std::exp(min(700.0, -TempDepCoef / AirCap)) +
                                         TempIndCoef / TempDepCoef;
                        }
                    } else if (SELECT_CASE_var == UseEulerMethod) {
                        ZTAveraged = (AirCap * ZoneT1(ZoneNum) + TempIndCoef) / (AirCap + TempDepCoef);
                    }
                }
                state.dataRoomAirMod->ZTOC(ZoneNum) = ZTAveraged;
                state.dataRoomAirMod->ZTMX(ZoneNum) = ZTAveraged;
                state.dataRoomAirMod->ZTFloor(ZoneNum) = ZTAveraged;
                HcUCSDDV(state, ZoneNum, HeightFrac);
                TempDepCoef = HA_MX + HA_OC + HA_FLOOR + MCp_Total;
                TempIndCoef = ConvGains + HAT_MX + HAT_OC + HAT_FLOOR + MCpT_Total;
                {
                    auto const SELECT_CASE_var(ZoneAirSolutionAlgo);
                    if (SELECT_CASE_var == Use3rdOrder) {
                        ZTAveraged = (TempHistTerm + ConvGains + HAT_MX + HAT_OC + HAT_FLOOR + MCpT_Total) /
                                     ((11.0 / 6.0) * AirCap + HA_MX + HA_OC + HA_FLOOR + MCp_Total);
                    } else if (SELECT_CASE_var == UseAnalyticalSolution) {
                        if (TempDepCoef == 0.0) { // B=0
                            ZTAveraged = ZoneT1(ZoneNum) + TempIndCoef / AirCap;
                        } else {
                            ZTAveraged = (ZoneT1(ZoneNum) - TempIndCoef / TempDepCoef) * std::exp(min(700.0, -TempDepCoef / AirCap)) +
                                         TempIndCoef / TempDepCoef;
                        }
                    } else if (SELECT_CASE_var == UseEulerMethod) {
                        ZTAveraged = (AirCap * ZoneT1(ZoneNum) + TempIndCoef) / (AirCap + TempDepCoef);
                    }
                }
                state.dataRoomAirMod->ZTOC(ZoneNum) = ZTAveraged;
                state.dataRoomAirMod->ZTMX(ZoneNum) = ZTAveraged;
                state.dataRoomAirMod->ZTFloor(ZoneNum) = ZTAveraged;
            }
        }
        //=========================================================================================

        // Comfort temperature and temperature at the thermostat/temperature control sensor

        state.dataRoomAirMod->HeightTransition(ZoneNum) = HeightFrac * CeilingHeight;
        HeightMixedSubzoneAve = (CeilingHeight + state.dataRoomAirMod->HeightTransition(ZoneNum)) / 2.0;
        HeightOccupiedSubzoneAve = (HeightFloorSubzoneTop + state.dataRoomAirMod->HeightTransition(ZoneNum)) / 2.0;
        HeightFloorSubzoneAve = HeightFloorSubzoneTop / 2.0;

        // Comfort temperature

        if (MIXFLAG) {
            state.dataRoomAirMod->TCMF(ZoneNum) = ZTAveraged;
        } else {
            if (HeightComfort >= 0.0 && HeightComfort < HeightFloorSubzoneAve) {
                ShowWarningError(state, "Displacement ventilation comfort height is in floor subzone in Zone: " + Zone(ZoneNum).Name);
                state.dataRoomAirMod->TCMF(ZoneNum) = state.dataRoomAirMod->ZTFloor(ZoneNum);
            } else if (HeightComfort >= HeightFloorSubzoneAve && HeightComfort < HeightOccupiedSubzoneAve) {
                state.dataRoomAirMod->TCMF(ZoneNum) =
                    (state.dataRoomAirMod->ZTFloor(ZoneNum) * (HeightOccupiedSubzoneAve - HeightComfort) + state.dataRoomAirMod->ZTOC(ZoneNum) * (HeightComfort - HeightFloorSubzoneAve)) /
                    (HeightOccupiedSubzoneAve - HeightFloorSubzoneAve);
                //!      TCMF(ZoneNum) = (ZTFloor(ZoneNum) * (HeightOccupiedSubzoneAve - HeightComfort) &
                //!                    + ZTMX(ZoneNum) * (HeightComfort - HeightFloorSubzoneAve)) &
                //!                    / (HeightOccupiedSubzoneAve - HeightFloorSubzoneAve)
            } else if (HeightComfort >= HeightOccupiedSubzoneAve && HeightComfort < HeightMixedSubzoneAve) {
                state.dataRoomAirMod->TCMF(ZoneNum) =
                    (state.dataRoomAirMod->ZTOC(ZoneNum) * (HeightMixedSubzoneAve - HeightComfort) + state.dataRoomAirMod->ZTMX(ZoneNum) * (HeightComfort - HeightOccupiedSubzoneAve)) /
                    (HeightMixedSubzoneAve - HeightOccupiedSubzoneAve);
            } else if (HeightComfort >= HeightMixedSubzoneAve && HeightComfort <= CeilingHeight) {
                state.dataRoomAirMod->TCMF(ZoneNum) = state.dataRoomAirMod->ZTMX(ZoneNum);
            } else {
                ShowFatalError(state, "Displacement ventilation comfort height is above ceiling or below floor in Zone: " + Zone(ZoneNum).Name);
            }
        }

        // Temperature at the thermostat/temperature control sensor

        if (MIXFLAG) {
            TempTstatAir(ZoneNum) = ZTAveraged;
        } else {
            if (HeightThermostat >= 0.0 && HeightThermostat < HeightFloorSubzoneAve) {
                ShowWarningError(state, "Displacement thermostat is in floor subzone in Zone: " + Zone(ZoneNum).Name);
                TempTstatAir(ZoneNum) = state.dataRoomAirMod->ZTFloor(ZoneNum);
            } else if (HeightThermostat >= HeightFloorSubzoneAve && HeightThermostat < HeightOccupiedSubzoneAve) {
                TempTstatAir(ZoneNum) =
                    (state.dataRoomAirMod->ZTFloor(ZoneNum) * (HeightOccupiedSubzoneAve - HeightThermostat) + state.dataRoomAirMod->ZTOC(ZoneNum) * (HeightThermostat - HeightFloorSubzoneAve)) /
                    (HeightOccupiedSubzoneAve - HeightFloorSubzoneAve);
                //!      TempTstatAir(ZoneNum) = (ZTFloor(ZoneNum) * (HeightOccupiedSubzoneAve - HeightThermostat) &
                //!                    + ZTMX(ZoneNum) * (HeightThermostat - HeightFloorSubzoneAve)) &
                //!                    / (HeightOccupiedSubzoneAve - HeightFloorSubzoneAve)
            } else if (HeightThermostat >= HeightOccupiedSubzoneAve && HeightThermostat < HeightMixedSubzoneAve) {
                TempTstatAir(ZoneNum) =
                    (state.dataRoomAirMod->ZTOC(ZoneNum) * (HeightMixedSubzoneAve - HeightThermostat) + state.dataRoomAirMod->ZTMX(ZoneNum) * (HeightThermostat - HeightOccupiedSubzoneAve)) /
                    (HeightMixedSubzoneAve - HeightOccupiedSubzoneAve);
            } else if (HeightThermostat >= HeightMixedSubzoneAve && HeightThermostat <= CeilingHeight) {
                TempTstatAir(ZoneNum) = state.dataRoomAirMod->ZTMX(ZoneNum);
            } else {
                ShowFatalError(state, "Displacement ventilation thermostat height is above ceiling or below floor in Zone: " + Zone(ZoneNum).Name);
            }
        }

        // Temperature gradients

        if ((HeightMixedSubzoneAve - HeightFloorSubzoneAve) > 0.1) {
            state.dataRoomAirMod->AvgTempGrad(ZoneNum) = (state.dataRoomAirMod->ZTMX(ZoneNum) - state.dataRoomAirMod->ZTFloor(ZoneNum)) / (HeightMixedSubzoneAve - HeightFloorSubzoneAve);
        } else {
            state.dataRoomAirMod->AvgTempGrad(ZoneNum) = -9.999;
        }
        if ((HeightOccupiedSubzoneAve - HeightFloorSubzoneAve) > 0.1) {
            state.dataRoomAirMod->MaxTempGrad(ZoneNum) = (state.dataRoomAirMod->ZTOC(ZoneNum) - state.dataRoomAirMod->ZTFloor(ZoneNum)) / (HeightOccupiedSubzoneAve - HeightFloorSubzoneAve);
        } else {
            state.dataRoomAirMod->MaxTempGrad(ZoneNum) = -9.999;
        }
        if ((HeightMixedSubzoneAve - HeightOccupiedSubzoneAve) > 0.1) {
            MTGAUX = (state.dataRoomAirMod->ZTMX(ZoneNum) - state.dataRoomAirMod->ZTOC(ZoneNum)) / (HeightMixedSubzoneAve - HeightOccupiedSubzoneAve);
        } else {
            MTGAUX = -9.999;
        }

        if (MTGAUX > state.dataRoomAirMod->MaxTempGrad(ZoneNum)) {
            state.dataRoomAirMod->MaxTempGrad(ZoneNum) = MTGAUX;
        }

        if (MIXFLAG) {
            state.dataRoomAirMod->ZoneDVMixedFlag(ZoneNum) = 1;
            state.dataRoomAirMod->AirModel(ZoneNum).SimAirModel = false;
        } else {
            state.dataRoomAirMod->ZoneDVMixedFlag(ZoneNum) = 0;
            state.dataRoomAirMod->AirModel(ZoneNum).SimAirModel = true;
        }

        if (ZoneEquipConfig(ZoneNum).IsControlled) {
            ZoneNodeNum = Zone(ZoneNum).SystemZoneNodeNumber;
            Node(ZoneNodeNum).Temp = state.dataRoomAirMod->ZTMX(ZoneNum);
        }

        // Mixed for reporting purposes
        if ((MIXFLAG) || ((state.dataRoomAirMod->ZTMX(ZoneNum) - state.dataRoomAirMod->ZTOC(ZoneNum)) < TempDiffCritRep)) {
            state.dataRoomAirMod->ZoneDVMixedFlagRep(ZoneNum) = 1.0;
            state.dataRoomAirMod->FracMinFlow(ZoneNum) = -1.0;
            state.dataRoomAirMod->HeightTransition(ZoneNum) = -9.999;
            state.dataRoomAirMod->AvgTempGrad(ZoneNum) = -9.999;
            state.dataRoomAirMod->MaxTempGrad(ZoneNum) = -9.999;
        } else {
            state.dataRoomAirMod->ZoneDVMixedFlagRep(ZoneNum) = 0.0;
        }
    }

} // namespace DisplacementVentMgr

} // namespace EnergyPlus
