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
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/member.functions.hh>

// EnergyPlus Headers
#include <AirflowNetwork/Solver.hpp>
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

namespace EnergyPlus::DisplacementVentMgr {

// MODULE INFORMATION:
//       AUTHOR         G. Carrilho da Graca
//       DATE WRITTEN   February 2004
//       MODIFIED       na
//       RE-ENGINEERED  na

// PURPOSE OF THIS MODULE:
// Routines that implement the UCSD Displacement Ventilation

// Using/Aliasing
using namespace DataLoopNode;
using namespace DataEnvironment;
using namespace DataHeatBalance;
using namespace DataHeatBalSurface;
using namespace DataSurfaces;
using namespace DataRoomAirModel;
using ConvectionCoefficients::CalcDetailedHcInForDVModel;

void ManageUCSDDVModel(EnergyPlusData &state, int const ZoneNum) // index number for the specified zone
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         G. Carrilho da Graca
    //       DATE WRITTEN   February 2004
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    //   manage the UCSD Displacement Ventilation model

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

    // Do the one time initializations
    if (state.dataDispVentMgr->InitUCSDDVMyOneTimeFlag) {
        state.dataDispVentMgr->MyEnvrnFlag.dimension(state.dataGlobal->NumOfZones, true);
        state.dataDispVentMgr->HeightFloorSubzoneTop = 0.2;
        state.dataDispVentMgr->ThickOccupiedSubzoneMin = 0.2;
        state.dataDispVentMgr->HeightIntMassDefault = 2.0;
        state.dataDispVentMgr->InitUCSDDVMyOneTimeFlag = false;
    }

    // Do the begin environment initializations
    if (state.dataGlobal->BeginEnvrnFlag && state.dataDispVentMgr->MyEnvrnFlag(ZoneNum)) {
        state.dataDispVentMgr->HAT_MX = 0.0;
        state.dataDispVentMgr->HAT_OC = 0.0;
        state.dataDispVentMgr->HA_MX = 0.0;
        state.dataDispVentMgr->HA_OC = 0.0;
        state.dataDispVentMgr->HAT_FLOOR = 0.0;
        state.dataDispVentMgr->HA_FLOOR = 0.0;
        state.dataDispVentMgr->MyEnvrnFlag(ZoneNum) = false;
    }

    if (!state.dataGlobal->BeginEnvrnFlag) {
        state.dataDispVentMgr->MyEnvrnFlag(ZoneNum) = true;
    }

    // initialize these module variables every timestep
    state.dataDispVentMgr->HeightIntMass = state.dataDispVentMgr->HeightIntMassDefault;
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

    state.dataDispVentMgr->HAT_MX = 0.0;
    state.dataDispVentMgr->HAT_OC = 0.0;
    state.dataDispVentMgr->HA_MX = 0.0;
    state.dataDispVentMgr->HA_OC = 0.0;
    state.dataDispVentMgr->HAT_FLOOR = 0.0;
    state.dataDispVentMgr->HA_FLOOR = 0.0;
    auto &SurfTempIn(state.dataHeatBalSurf->SurfTempIn);

    // Is the air flow model for this zone set to UCSDDV Displacement Ventilation?
    if (state.dataRoomAirMod->IsZoneDV(ZoneNum)) {
        LayFrac = FractionHeight;
        LayH = FractionHeight *
               (state.dataRoomAirMod->ZoneCeilingHeight((ZoneNum - 1) * 2 + 2) - state.dataRoomAirMod->ZoneCeilingHeight((ZoneNum - 1) * 2 + 1));
        // WALL Hc, HA and HAT calculation
        for (Ctd = state.dataUCSDShared->PosZ_Wall((ZoneNum - 1) * 2 + 1); Ctd <= state.dataUCSDShared->PosZ_Wall((ZoneNum - 1) * 2 + 2); ++Ctd) {
            SurfNum = state.dataUCSDShared->APos_Wall(Ctd);
            state.dataSurface->SurfTAirRef(SurfNum) = DataSurfaces::RefAirTemp::AdjacentAirTemp;
            state.dataSurface->SurfTAirRefRpt(SurfNum) = DataSurfaces::SurfTAirRefReportVals[state.dataSurface->SurfTAirRef(SurfNum)];
            if (SurfNum == 0) continue;
            Z1 = minval(state.dataSurface->Surface(SurfNum).Vertex({1, state.dataSurface->Surface(SurfNum).Sides}), &Vector::z);
            Z2 = maxval(state.dataSurface->Surface(SurfNum).Vertex({1, state.dataSurface->Surface(SurfNum).Sides}), &Vector::z);
            ZSupSurf = Z2 - state.dataRoomAirMod->ZoneCeilingHeight((ZoneNum - 1) * 2 + 1);
            ZInfSurf = Z1 - state.dataRoomAirMod->ZoneCeilingHeight((ZoneNum - 1) * 2 + 1);

            // The Wall surface is in the upper subzone
            if (ZInfSurf > LayH) {
                state.dataHeatBal->SurfTempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTMX(ZoneNum);
                CalcDetailedHcInForDVModel(state, SurfNum, SurfTempIn, state.dataRoomAirMod->DVHcIn);
                state.dataUCSDShared->HWall(Ctd) = state.dataRoomAirMod->DVHcIn(SurfNum);
                state.dataDispVentMgr->HAT_MX += state.dataSurface->Surface(SurfNum).Area * SurfTempIn(SurfNum) * state.dataUCSDShared->HWall(Ctd);
                state.dataDispVentMgr->HA_MX += state.dataSurface->Surface(SurfNum).Area * state.dataUCSDShared->HWall(Ctd);
            }

            // The Wall surface is in the lower subzone
            if (ZSupSurf < LayH) {
                state.dataHeatBal->SurfTempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTOC(ZoneNum);
                CalcDetailedHcInForDVModel(state, SurfNum, SurfTempIn, state.dataRoomAirMod->DVHcIn);
                state.dataUCSDShared->HWall(Ctd) = state.dataRoomAirMod->DVHcIn(SurfNum);
                state.dataDispVentMgr->HAT_OC += state.dataSurface->Surface(SurfNum).Area * SurfTempIn(SurfNum) * state.dataUCSDShared->HWall(Ctd);
                state.dataDispVentMgr->HA_OC += state.dataSurface->Surface(SurfNum).Area * state.dataUCSDShared->HWall(Ctd);
            }

            // The Wall surface is partially in upper and partially in lower subzone
            if (ZInfSurf <= LayH && ZSupSurf >= LayH) {
                state.dataHeatBal->SurfTempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTMX(ZoneNum);
                CalcDetailedHcInForDVModel(state, SurfNum, SurfTempIn, state.dataRoomAirMod->DVHcIn);
                HLU = state.dataRoomAirMod->DVHcIn(SurfNum);
                state.dataHeatBal->SurfTempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTOC(ZoneNum);
                CalcDetailedHcInForDVModel(state, SurfNum, SurfTempIn, state.dataRoomAirMod->DVHcIn);
                HLD = state.dataRoomAirMod->DVHcIn(SurfNum);
                TmedDV = ((ZSupSurf - LayH) * state.dataRoomAirMod->ZTMX(ZoneNum) + (LayH - ZInfSurf) * state.dataRoomAirMod->ZTOC(ZoneNum)) /
                         (ZSupSurf - ZInfSurf);
                state.dataUCSDShared->HWall(Ctd) = ((LayH - ZInfSurf) * HLD + (ZSupSurf - LayH) * HLU) / (ZSupSurf - ZInfSurf);
                state.dataDispVentMgr->HAT_MX +=
                    state.dataSurface->Surface(SurfNum).Area * (ZSupSurf - LayH) / (ZSupSurf - ZInfSurf) * SurfTempIn(SurfNum) * HLU;
                state.dataDispVentMgr->HA_MX += state.dataSurface->Surface(SurfNum).Area * (ZSupSurf - LayH) / (ZSupSurf - ZInfSurf) * HLU;
                state.dataDispVentMgr->HAT_OC +=
                    state.dataSurface->Surface(SurfNum).Area * (LayH - ZInfSurf) / (ZSupSurf - ZInfSurf) * SurfTempIn(SurfNum) * HLD;
                state.dataDispVentMgr->HA_OC += state.dataSurface->Surface(SurfNum).Area * (LayH - ZInfSurf) / (ZSupSurf - ZInfSurf) * HLD;
                state.dataHeatBal->SurfTempEffBulkAir(SurfNum) = TmedDV;
            }

            state.dataRoomAirMod->DVHcIn(SurfNum) = state.dataUCSDShared->HWall(Ctd);

        } // END WALL

        // WINDOW Hc, HA and HAT CALCULATION
        for (Ctd = state.dataUCSDShared->PosZ_Window((ZoneNum - 1) * 2 + 1); Ctd <= state.dataUCSDShared->PosZ_Window((ZoneNum - 1) * 2 + 2); ++Ctd) {
            SurfNum = state.dataUCSDShared->APos_Window(Ctd);
            state.dataSurface->SurfTAirRef(SurfNum) = DataSurfaces::RefAirTemp::AdjacentAirTemp;
            state.dataSurface->SurfTAirRefRpt(SurfNum) = DataSurfaces::SurfTAirRefReportVals[state.dataSurface->SurfTAirRef(SurfNum)];
            if (SurfNum == 0) continue;
            if (state.dataSurface->Surface(SurfNum).Tilt > 10.0 && state.dataSurface->Surface(SurfNum).Tilt < 170.0) { // Window Wall
                Z1 = minval(state.dataSurface->Surface(SurfNum).Vertex({1, state.dataSurface->Surface(SurfNum).Sides}), &Vector::z);
                Z2 = maxval(state.dataSurface->Surface(SurfNum).Vertex({1, state.dataSurface->Surface(SurfNum).Sides}), &Vector::z);
                ZSupSurf = Z2 - state.dataRoomAirMod->ZoneCeilingHeight((ZoneNum - 1) * 2 + 1);
                ZInfSurf = Z1 - state.dataRoomAirMod->ZoneCeilingHeight((ZoneNum - 1) * 2 + 1);

                if (ZInfSurf > LayH) {
                    state.dataHeatBal->SurfTempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTMX(ZoneNum);
                    CalcDetailedHcInForDVModel(state, SurfNum, SurfTempIn, state.dataRoomAirMod->DVHcIn);
                    state.dataUCSDShared->HWindow(Ctd) = state.dataRoomAirMod->DVHcIn(SurfNum);
                    state.dataDispVentMgr->HAT_MX +=
                        state.dataSurface->Surface(SurfNum).Area * SurfTempIn(SurfNum) * state.dataUCSDShared->HWindow(Ctd);
                    state.dataDispVentMgr->HA_MX += state.dataSurface->Surface(SurfNum).Area * state.dataUCSDShared->HWindow(Ctd);
                }

                if (ZSupSurf < LayH) {
                    state.dataHeatBal->SurfTempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTOC(ZoneNum);
                    CalcDetailedHcInForDVModel(state, SurfNum, SurfTempIn, state.dataRoomAirMod->DVHcIn);
                    state.dataUCSDShared->HWindow(Ctd) = state.dataRoomAirMod->DVHcIn(SurfNum);
                    state.dataDispVentMgr->HAT_OC +=
                        state.dataSurface->Surface(SurfNum).Area * SurfTempIn(SurfNum) * state.dataUCSDShared->HWindow(Ctd);
                    state.dataDispVentMgr->HA_OC += state.dataSurface->Surface(SurfNum).Area * state.dataUCSDShared->HWindow(Ctd);
                }

                if (ZInfSurf <= LayH && ZSupSurf >= LayH) {
                    state.dataHeatBal->SurfTempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTMX(ZoneNum);
                    CalcDetailedHcInForDVModel(state, SurfNum, SurfTempIn, state.dataRoomAirMod->DVHcIn);
                    HLU = state.dataRoomAirMod->DVHcIn(SurfNum);
                    state.dataHeatBal->SurfTempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTOC(ZoneNum);
                    CalcDetailedHcInForDVModel(state, SurfNum, SurfTempIn, state.dataRoomAirMod->DVHcIn);
                    HLD = state.dataRoomAirMod->DVHcIn(SurfNum);
                    TmedDV = ((ZSupSurf - LayH) * state.dataRoomAirMod->ZTMX(ZoneNum) + (LayH - ZInfSurf) * state.dataRoomAirMod->ZTOC(ZoneNum)) /
                             (ZSupSurf - ZInfSurf);
                    state.dataUCSDShared->HWindow(Ctd) = ((LayH - ZInfSurf) * HLD + (ZSupSurf - LayH) * HLU) / (ZSupSurf - ZInfSurf);
                    state.dataDispVentMgr->HAT_MX +=
                        state.dataSurface->Surface(SurfNum).Area * (ZSupSurf - LayH) / (ZSupSurf - ZInfSurf) * SurfTempIn(SurfNum) * HLU;
                    state.dataDispVentMgr->HA_MX += state.dataSurface->Surface(SurfNum).Area * (ZSupSurf - LayH) / (ZSupSurf - ZInfSurf) * HLU;
                    state.dataDispVentMgr->HAT_OC +=
                        state.dataSurface->Surface(SurfNum).Area * (LayH - ZInfSurf) / (ZSupSurf - ZInfSurf) * SurfTempIn(SurfNum) * HLD;
                    state.dataDispVentMgr->HA_OC += state.dataSurface->Surface(SurfNum).Area * (LayH - ZInfSurf) / (ZSupSurf - ZInfSurf) * HLD;
                    state.dataHeatBal->SurfTempEffBulkAir(SurfNum) = TmedDV;
                }
            }

            if (state.dataSurface->Surface(SurfNum).Tilt <= 10.0) { // Window Ceiling
                state.dataHeatBal->SurfTempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTMX(ZoneNum);
                CalcDetailedHcInForDVModel(state, SurfNum, SurfTempIn, state.dataRoomAirMod->DVHcIn);
                state.dataUCSDShared->HWindow(Ctd) = state.dataRoomAirMod->DVHcIn(SurfNum);
                state.dataDispVentMgr->HAT_MX += state.dataSurface->Surface(SurfNum).Area * SurfTempIn(SurfNum) * state.dataUCSDShared->HWindow(Ctd);
                state.dataDispVentMgr->HA_MX += state.dataSurface->Surface(SurfNum).Area * state.dataUCSDShared->HWindow(Ctd);
            }

            if (state.dataSurface->Surface(SurfNum).Tilt >= 170.0) { // Window Floor
                state.dataHeatBal->SurfTempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTOC(ZoneNum);
                CalcDetailedHcInForDVModel(state, SurfNum, SurfTempIn, state.dataRoomAirMod->DVHcIn);
                state.dataUCSDShared->HWindow(Ctd) = state.dataRoomAirMod->DVHcIn(SurfNum);
                state.dataDispVentMgr->HAT_OC += state.dataSurface->Surface(SurfNum).Area * SurfTempIn(SurfNum) * state.dataUCSDShared->HWindow(Ctd);
                state.dataDispVentMgr->HA_OC += state.dataSurface->Surface(SurfNum).Area * state.dataUCSDShared->HWindow(Ctd);
            }

            state.dataRoomAirMod->DVHcIn(SurfNum) = state.dataUCSDShared->HWindow(Ctd);

        } // END WINDOW

        // DOOR Hc, HA and HAT CALCULATION
        for (Ctd = state.dataUCSDShared->PosZ_Door((ZoneNum - 1) * 2 + 1); Ctd <= state.dataUCSDShared->PosZ_Door((ZoneNum - 1) * 2 + 2);
             ++Ctd) { // DOOR
            SurfNum = state.dataUCSDShared->APos_Door(Ctd);
            state.dataSurface->SurfTAirRef(SurfNum) = DataSurfaces::RefAirTemp::AdjacentAirTemp;
            state.dataSurface->SurfTAirRefRpt(SurfNum) = DataSurfaces::SurfTAirRefReportVals[state.dataSurface->SurfTAirRef(SurfNum)];
            if (SurfNum == 0) continue;
            if (state.dataSurface->Surface(SurfNum).Tilt > 10.0 && state.dataSurface->Surface(SurfNum).Tilt < 170.0) { // Door Wall
                Z1 = minval(state.dataSurface->Surface(SurfNum).Vertex({1, state.dataSurface->Surface(SurfNum).Sides}), &Vector::z);
                Z2 = maxval(state.dataSurface->Surface(SurfNum).Vertex({1, state.dataSurface->Surface(SurfNum).Sides}), &Vector::z);
                ZSupSurf = Z2 - state.dataRoomAirMod->ZoneCeilingHeight((ZoneNum - 1) * 2 + 1);
                ZInfSurf = Z1 - state.dataRoomAirMod->ZoneCeilingHeight((ZoneNum - 1) * 2 + 1);

                if (ZInfSurf > LayH) {
                    state.dataHeatBal->SurfTempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTMX(ZoneNum);
                    CalcDetailedHcInForDVModel(state, SurfNum, SurfTempIn, state.dataRoomAirMod->DVHcIn);
                    state.dataUCSDShared->HDoor(Ctd) = state.dataRoomAirMod->DVHcIn(SurfNum);
                    state.dataDispVentMgr->HAT_MX +=
                        state.dataSurface->Surface(SurfNum).Area * SurfTempIn(SurfNum) * state.dataUCSDShared->HDoor(Ctd);
                    state.dataDispVentMgr->HA_MX += state.dataSurface->Surface(SurfNum).Area * state.dataUCSDShared->HDoor(Ctd);
                }

                if (ZSupSurf < LayH) {
                    state.dataHeatBal->SurfTempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTOC(ZoneNum);
                    CalcDetailedHcInForDVModel(state, SurfNum, SurfTempIn, state.dataRoomAirMod->DVHcIn);
                    state.dataUCSDShared->HDoor(Ctd) = state.dataRoomAirMod->DVHcIn(SurfNum);
                    state.dataDispVentMgr->HAT_OC +=
                        state.dataSurface->Surface(SurfNum).Area * SurfTempIn(SurfNum) * state.dataUCSDShared->HDoor(Ctd);
                    state.dataDispVentMgr->HA_OC += state.dataSurface->Surface(SurfNum).Area * state.dataUCSDShared->HDoor(Ctd);
                }

                if (ZInfSurf <= LayH && ZSupSurf >= LayH) {
                    state.dataHeatBal->SurfTempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTMX(ZoneNum);
                    CalcDetailedHcInForDVModel(state, SurfNum, SurfTempIn, state.dataRoomAirMod->DVHcIn);
                    HLU = state.dataRoomAirMod->DVHcIn(SurfNum);
                    state.dataHeatBal->SurfTempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTOC(ZoneNum);
                    CalcDetailedHcInForDVModel(state, SurfNum, SurfTempIn, state.dataRoomAirMod->DVHcIn);
                    HLD = state.dataRoomAirMod->DVHcIn(SurfNum);
                    TmedDV = ((ZSupSurf - LayH) * state.dataRoomAirMod->ZTMX(ZoneNum) + (LayH - ZInfSurf) * state.dataRoomAirMod->ZTOC(ZoneNum)) /
                             (ZSupSurf - ZInfSurf);
                    state.dataUCSDShared->HDoor(Ctd) = ((LayH - ZInfSurf) * HLD + (ZSupSurf - LayH) * HLU) / (ZSupSurf - ZInfSurf);
                    state.dataDispVentMgr->HAT_MX +=
                        state.dataSurface->Surface(SurfNum).Area * (ZSupSurf - LayH) / (ZSupSurf - ZInfSurf) * SurfTempIn(SurfNum) * HLU;
                    state.dataDispVentMgr->HA_MX += state.dataSurface->Surface(SurfNum).Area * (ZSupSurf - LayH) / (ZSupSurf - ZInfSurf) * HLU;
                    state.dataDispVentMgr->HAT_OC +=
                        state.dataSurface->Surface(SurfNum).Area * (LayH - ZInfSurf) / (ZSupSurf - ZInfSurf) * SurfTempIn(SurfNum) * HLD;
                    state.dataDispVentMgr->HA_OC += state.dataSurface->Surface(SurfNum).Area * (LayH - ZInfSurf) / (ZSupSurf - ZInfSurf) * HLD;
                    state.dataHeatBal->SurfTempEffBulkAir(SurfNum) = TmedDV;
                }
            }

            if (state.dataSurface->Surface(SurfNum).Tilt <= 10.0) { // Door Ceiling
                state.dataHeatBal->SurfTempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTMX(ZoneNum);
                CalcDetailedHcInForDVModel(state, SurfNum, SurfTempIn, state.dataRoomAirMod->DVHcIn);
                state.dataUCSDShared->HDoor(Ctd) = state.dataRoomAirMod->DVHcIn(SurfNum);
                state.dataDispVentMgr->HAT_MX += state.dataSurface->Surface(SurfNum).Area * SurfTempIn(SurfNum) * state.dataUCSDShared->HDoor(Ctd);
                state.dataDispVentMgr->HA_MX += state.dataSurface->Surface(SurfNum).Area * state.dataUCSDShared->HDoor(Ctd);
            }

            if (state.dataSurface->Surface(SurfNum).Tilt >= 170.0) { // Door Floor
                state.dataHeatBal->SurfTempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTOC(ZoneNum);
                CalcDetailedHcInForDVModel(state, SurfNum, SurfTempIn, state.dataRoomAirMod->DVHcIn);
                state.dataUCSDShared->HDoor(Ctd) = state.dataRoomAirMod->DVHcIn(SurfNum);
                state.dataDispVentMgr->HAT_OC += state.dataSurface->Surface(SurfNum).Area * SurfTempIn(SurfNum) * state.dataUCSDShared->HDoor(Ctd);
                state.dataDispVentMgr->HA_OC += state.dataSurface->Surface(SurfNum).Area * state.dataUCSDShared->HDoor(Ctd);
            }

            state.dataRoomAirMod->DVHcIn(SurfNum) = state.dataUCSDShared->HDoor(Ctd);

        } // END DOOR

        // INTERNAL Hc, HA and HAT CALCULATION
        state.dataDispVentMgr->HeightIntMass =
            min(state.dataDispVentMgr->HeightIntMassDefault,
                (state.dataRoomAirMod->ZoneCeilingHeight((ZoneNum - 1) * 2 + 2) - state.dataRoomAirMod->ZoneCeilingHeight((ZoneNum - 1) * 2 + 1)));
        for (Ctd = state.dataUCSDShared->PosZ_Internal((ZoneNum - 1) * 2 + 1); Ctd <= state.dataUCSDShared->PosZ_Internal((ZoneNum - 1) * 2 + 2);
             ++Ctd) {
            SurfNum = state.dataUCSDShared->APos_Internal(Ctd);
            state.dataSurface->SurfTAirRef(SurfNum) = DataSurfaces::RefAirTemp::AdjacentAirTemp;
            state.dataSurface->SurfTAirRefRpt(SurfNum) = DataSurfaces::SurfTAirRefReportVals[state.dataSurface->SurfTAirRef(SurfNum)];
            if (SurfNum == 0) continue;
            ZSupSurf = state.dataDispVentMgr->HeightIntMass;
            ZInfSurf = 0.0;

            if (ZSupSurf < LayH) {
                state.dataHeatBal->SurfTempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTOC(ZoneNum);
                CalcDetailedHcInForDVModel(state, SurfNum, SurfTempIn, state.dataRoomAirMod->DVHcIn);
                state.dataUCSDShared->HInternal(Ctd) = state.dataRoomAirMod->DVHcIn(SurfNum);
                state.dataDispVentMgr->HAT_OC +=
                    state.dataSurface->Surface(SurfNum).Area * SurfTempIn(SurfNum) * state.dataUCSDShared->HInternal(Ctd);
                state.dataDispVentMgr->HA_OC += state.dataSurface->Surface(SurfNum).Area * state.dataUCSDShared->HInternal(Ctd);
            }

            if (ZInfSurf <= LayH && ZSupSurf >= LayH) {
                state.dataHeatBal->SurfTempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTMX(ZoneNum);
                CalcDetailedHcInForDVModel(state, SurfNum, SurfTempIn, state.dataRoomAirMod->DVHcIn);
                HLU = state.dataRoomAirMod->DVHcIn(SurfNum);
                state.dataHeatBal->SurfTempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTOC(ZoneNum);
                CalcDetailedHcInForDVModel(state, SurfNum, SurfTempIn, state.dataRoomAirMod->DVHcIn);
                HLD = state.dataRoomAirMod->DVHcIn(SurfNum);
                TmedDV = ((ZSupSurf - LayH) * state.dataRoomAirMod->ZTMX(ZoneNum) + (LayH - ZInfSurf) * state.dataRoomAirMod->ZTOC(ZoneNum)) /
                         (ZSupSurf - ZInfSurf);
                state.dataUCSDShared->HInternal(Ctd) = ((LayH - ZInfSurf) * HLD + (ZSupSurf - LayH) * HLU) / (ZSupSurf - ZInfSurf);
                state.dataDispVentMgr->HAT_MX +=
                    state.dataSurface->Surface(SurfNum).Area * (ZSupSurf - LayH) / (ZSupSurf - ZInfSurf) * SurfTempIn(SurfNum) * HLU;
                state.dataDispVentMgr->HA_MX += state.dataSurface->Surface(SurfNum).Area * (ZSupSurf - LayH) / (ZSupSurf - ZInfSurf) * HLU;
                state.dataDispVentMgr->HAT_OC +=
                    state.dataSurface->Surface(SurfNum).Area * (LayH - ZInfSurf) / (ZSupSurf - ZInfSurf) * SurfTempIn(SurfNum) * HLD;
                state.dataDispVentMgr->HA_OC += state.dataSurface->Surface(SurfNum).Area * (LayH - ZInfSurf) / (ZSupSurf - ZInfSurf) * HLD;
                state.dataHeatBal->SurfTempEffBulkAir(SurfNum) = TmedDV;
            }

            state.dataRoomAirMod->DVHcIn(SurfNum) = state.dataUCSDShared->HInternal(Ctd);
        } // END INTERNAL

        // CEILING Hc, HA and HAT CALCULATION
        for (Ctd = state.dataUCSDShared->PosZ_Ceiling((ZoneNum - 1) * 2 + 1); Ctd <= state.dataUCSDShared->PosZ_Ceiling((ZoneNum - 1) * 2 + 2);
             ++Ctd) {
            SurfNum = state.dataUCSDShared->APos_Ceiling(Ctd);
            state.dataSurface->SurfTAirRef(SurfNum) = DataSurfaces::RefAirTemp::AdjacentAirTemp;
            state.dataSurface->SurfTAirRefRpt(SurfNum) = DataSurfaces::SurfTAirRefReportVals[state.dataSurface->SurfTAirRef(SurfNum)];
            if (SurfNum == 0) continue;
            state.dataHeatBal->SurfTempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTMX(ZoneNum);
            CalcDetailedHcInForDVModel(state, SurfNum, SurfTempIn, state.dataRoomAirMod->DVHcIn);
            state.dataUCSDShared->HCeiling(Ctd) = state.dataRoomAirMod->DVHcIn(SurfNum);
            state.dataDispVentMgr->HAT_MX += state.dataSurface->Surface(SurfNum).Area * SurfTempIn(SurfNum) * state.dataUCSDShared->HCeiling(Ctd);
            state.dataDispVentMgr->HA_MX += state.dataSurface->Surface(SurfNum).Area * state.dataUCSDShared->HCeiling(Ctd);
            state.dataRoomAirMod->DVHcIn(SurfNum) = state.dataUCSDShared->HCeiling(Ctd);
        } // END CEILING

        // FLOOR Hc, HA and HAT CALCULATION
        for (Ctd = state.dataUCSDShared->PosZ_Floor((ZoneNum - 1) * 2 + 1); Ctd <= state.dataUCSDShared->PosZ_Floor((ZoneNum - 1) * 2 + 2); ++Ctd) {
            SurfNum = state.dataUCSDShared->APos_Floor(Ctd);
            state.dataSurface->SurfTAirRef(SurfNum) = DataSurfaces::RefAirTemp::AdjacentAirTemp;
            state.dataSurface->SurfTAirRefRpt(SurfNum) = DataSurfaces::SurfTAirRefReportVals[state.dataSurface->SurfTAirRef(SurfNum)];
            if (SurfNum == 0) continue;
            state.dataHeatBal->SurfTempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTFloor(ZoneNum);
            CalcDetailedHcInForDVModel(state, SurfNum, SurfTempIn, state.dataRoomAirMod->DVHcIn);
            state.dataUCSDShared->HFloor(Ctd) = state.dataRoomAirMod->DVHcIn(SurfNum);
            state.dataDispVentMgr->HAT_FLOOR += state.dataSurface->Surface(SurfNum).Area * SurfTempIn(SurfNum) * state.dataUCSDShared->HFloor(Ctd);
            state.dataDispVentMgr->HA_FLOOR += state.dataSurface->Surface(SurfNum).Area * state.dataUCSDShared->HFloor(Ctd);
            state.dataHeatBal->SurfTempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTFloor(ZoneNum);
            state.dataRoomAirMod->DVHcIn(SurfNum) = state.dataUCSDShared->HFloor(Ctd);
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
    const Real64 elevenOverSix = 11.0 / 6.0;
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
    auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;
    auto &UseZoneTimeStepHistory = state.dataHVACGlobal->UseZoneTimeStepHistory;
    using InternalHeatGains::SumInternalConvectionGainsByTypes;
    using InternalHeatGains::SumReturnAirConvectionGainsByTypes;
    using Psychrometrics::PsyCpAirFnW;
    using Psychrometrics::PsyRhoAirFnPbTdbW;
    using ScheduleManager::GetCurrentScheduleValue;
    using ScheduleManager::GetScheduleIndex;

    // SUBROUTINE PARAMETER DEFINITIONS:
    Real64 const OneThird(1.0 / 3.0);
    Real64 const MinFlow_pow_fac(std::pow(1.0 / 24.55 * 1.0, 1.0 / 0.6));

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

    auto &TempDepCoef = state.dataDispVentMgr->TempDepCoef;
    auto &TempIndCoef = state.dataDispVentMgr->TempIndCoef;

    Real64 RetAirGain;

    // Exact solution or Euler method
    if (state.dataHeatBal->ZoneAirSolutionAlgo != DataHeatBalance::SolutionAlgo::ThirdOrder) {
        if (state.dataHVACGlobal->ShortenTimeStepSysRoomAir && TimeStepSys < state.dataGlobal->TimeStepZone) {
            if (state.dataHVACGlobal->PreviousTimeStep < state.dataGlobal->TimeStepZone) {
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

    auto &Zone(state.dataHeatBal->Zone);

    MIXFLAG = false;
    FlagApertures = 1;
    state.dataRoomAirMod->DVHcIn = state.dataHeatBalSurf->SurfHConvInt;
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

    ConvGainsOccupiedSubzone = SumInternalConvectionGainsByTypes(state, ZoneNum, DisplacementVentMgr::IntGainTypesOccupied);

    ConvGainsOccupiedSubzone += 0.5 * state.dataHeatBalFanSys->SysDepZoneLoadsLagged(ZoneNum);

    // Add heat to return air if zonal system (no return air) or cycling system (return air frequently very
    // low or zero)
    if (Zone(ZoneNum).NoHeatToReturnAir) {
        RetAirGain = SumReturnAirConvectionGainsByTypes(state, ZoneNum, DisplacementVentMgr::IntGainTypesOccupied);
        ConvGainsOccupiedSubzone += RetAirGain;
    }

    ConvGainsMixedSubzone = SumInternalConvectionGainsByTypes(state, ZoneNum, DisplacementVentMgr::IntGainTypesMixedSubzone);
    ConvGainsMixedSubzone += state.dataHeatBalFanSys->SumConvHTRadSys(ZoneNum) + state.dataHeatBalFanSys->SumConvPool(ZoneNum) +
                             0.5 * state.dataHeatBalFanSys->SysDepZoneLoadsLagged(ZoneNum);
    if (Zone(ZoneNum).NoHeatToReturnAir) {
        RetAirGain = SumReturnAirConvectionGainsByTypes(state, ZoneNum, DisplacementVentMgr::IntGainTypesMixedSubzone);
        ConvGainsMixedSubzone += RetAirGain;
    }

    ConvGains = ConvGainsOccupiedSubzone + ConvGainsMixedSubzone;

    //=================== Entering air system temperature and flow====================
    SumSysMCp = 0.0;
    SumSysMCpT = 0.0;
    // Check to make sure if this is a controlled zone and determine ZoneEquipConfigNum
    ZoneEquipConfigNum = ZoneNum;
    if (state.dataZoneEquip->ZoneEquipConfig(ZoneEquipConfigNum).IsControlled) {
        for (NodeNum = 1; NodeNum <= state.dataZoneEquip->ZoneEquipConfig(ZoneEquipConfigNum).NumInletNodes; ++NodeNum) {
            NodeTemp = state.dataLoopNodes->Node(state.dataZoneEquip->ZoneEquipConfig(ZoneEquipConfigNum).InletNode(NodeNum)).Temp;
            MassFlowRate = state.dataLoopNodes->Node(state.dataZoneEquip->ZoneEquipConfig(ZoneEquipConfigNum).InletNode(NodeNum)).MassFlowRate;
            CpAir = PsyCpAirFnW(state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum));
            SumSysMCp += MassFlowRate * CpAir;
            SumSysMCpT += MassFlowRate * CpAir * NodeTemp;
        }
    }

    SumMCp = state.dataHeatBalFanSys->MCPI(ZoneNum) + state.dataHeatBalFanSys->MCPV(ZoneNum) + state.dataHeatBalFanSys->MCPM(ZoneNum) +
             state.dataHeatBalFanSys->MCPE(ZoneNum) + state.dataHeatBalFanSys->MCPC(ZoneNum) + state.dataHeatBalFanSys->MDotCPOA(ZoneNum);
    SumMCpT = state.dataHeatBalFanSys->MCPTI(ZoneNum) + state.dataHeatBalFanSys->MCPTV(ZoneNum) + state.dataHeatBalFanSys->MCPTM(ZoneNum) +
              state.dataHeatBalFanSys->MCPTE(ZoneNum) + state.dataHeatBalFanSys->MCPTC(ZoneNum) +
              state.dataHeatBalFanSys->MDotCPOA(ZoneNum) * Zone(ZoneNum).OutDryBulbTemp;
    if (state.afn->SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlMultizone) {
        SumMCp = state.afn->exchangeData(ZoneNum).SumMCp + state.afn->exchangeData(ZoneNum).SumMVCp + state.afn->exchangeData(ZoneNum).SumMMCp;
        SumMCpT = state.afn->exchangeData(ZoneNum).SumMCpT + state.afn->exchangeData(ZoneNum).SumMVCpT + state.afn->exchangeData(ZoneNum).SumMMCpT;
    }

    MCp_Total = SumMCp + SumSysMCp;
    MCpT_Total = SumMCpT + SumSysMCpT;

    if (state.dataHeatBal->TotPeople > 0) {
        NumberOfOccupants = 0;
        NumberOfPlumes = 0.0;
        for (Ctd = 1; Ctd <= state.dataHeatBal->TotPeople; ++Ctd) {
            if (state.dataHeatBal->People(Ctd).ZonePtr == ZoneNum) {
                NumberOfOccupants += state.dataHeatBal->People(Ctd).NumberOfPeople; // *GetCurrentScheduleValue(state, People(Ctd)%NumberOfPeoplePtr)
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

    if (state.afn->NumOfLinksMultiZone > 0) {
        for (Loop = 1; Loop <= state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(0, ZoneNum); ++Loop) {
            // direct AirflowNetwork surface

            if (state.dataSurface->Surface(state.afn->MultizoneSurfaceData(state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(Loop, ZoneNum)).SurfNum)
                    .Zone == ZoneNum) {

                if ((state.dataRoomAirMod->SurfParametersCVDV(state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(Loop, ZoneNum)).Zmax < 0.8 &&
                     state.afn->AirflowNetworkLinkSimu(state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(Loop, ZoneNum)).VolFLOW > 0)) {
                    FlagApertures = 0;
                    break;
                }
                if (state.dataRoomAirMod->SurfParametersCVDV(state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(Loop, ZoneNum)).Zmin > 1.8 &&
                    state.afn->AirflowNetworkLinkSimu(state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(Loop, ZoneNum)).VolFLOW2 > 0) {
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
                            Zone(state.dataSurface
                                     ->Surface(
                                         state.afn->MultizoneSurfaceData(state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(Loop, ZoneNum)).SurfNum)
                                     .Zone)
                                .OriginZ -
                            Zone(ZoneNum).OriginZ <
                        0.8 &&
                    state.afn->AirflowNetworkLinkSimu(state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(Loop, ZoneNum)).VolFLOW2 > 0) {
                    FlagApertures = 0;
                    break;
                }
                if (state.dataRoomAirMod->SurfParametersCVDV(state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(Loop, ZoneNum)).Zmin +
                            Zone(state.dataSurface
                                     ->Surface(
                                         state.afn->MultizoneSurfaceData(state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(Loop, ZoneNum)).SurfNum)
                                     .Zone)
                                .OriginZ -
                            Zone(ZoneNum).OriginZ >
                        1.8 &&
                    state.afn->AirflowNetworkLinkSimu(state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(Loop, ZoneNum)).VolFLOW > 0) {
                    FlagApertures = 0;
                    break;
                }
                if ((state.dataRoomAirMod->SurfParametersCVDV(state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(Loop, ZoneNum)).Zmin +
                             Zone(state.dataSurface
                                      ->Surface(
                                          state.afn->MultizoneSurfaceData(state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(Loop, ZoneNum)).SurfNum)
                                      .Zone)
                                 .OriginZ -
                             Zone(ZoneNum).OriginZ >
                         0.8 &&
                     state.dataRoomAirMod->SurfParametersCVDV(state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(Loop, ZoneNum)).Zmin +
                             Zone(state.dataSurface
                                      ->Surface(
                                          state.afn->MultizoneSurfaceData(state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(Loop, ZoneNum)).SurfNum)
                                      .Zone)
                                 .OriginZ -
                             Zone(ZoneNum).OriginZ <
                         1.8) ||
                    (state.dataRoomAirMod->SurfParametersCVDV(state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(Loop, ZoneNum)).Zmax +
                             Zone(state.dataSurface
                                      ->Surface(
                                          state.afn->MultizoneSurfaceData(state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(Loop, ZoneNum)).SurfNum)
                                      .Zone)
                                 .OriginZ -
                             Zone(ZoneNum).OriginZ >
                         0.8 &&
                     state.dataRoomAirMod->SurfParametersCVDV(state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(Loop, ZoneNum)).Zmax +
                             Zone(state.dataSurface
                                      ->Surface(
                                          state.afn->MultizoneSurfaceData(state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(Loop, ZoneNum)).SurfNum)
                                      .Zone)
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
            state.dataRoomAirMod->AIRRATFloor(ZoneNum) =
                Zone(ZoneNum).Volume * min(state.dataRoomAirMod->HeightTransition(ZoneNum), state.dataDispVentMgr->HeightFloorSubzoneTop) /
                CeilingHeight * Zone(ZoneNum).ZoneVolCapMultpSens *
                PsyRhoAirFnPbTdbW(
                    state, state.dataEnvrn->OutBaroPress, state.dataRoomAirMod->MATFloor(ZoneNum), state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum)) *
                PsyCpAirFnW(state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum)) / (TimeStepSys * DataGlobalConstants::SecInHour);
            state.dataRoomAirMod->AIRRATOC(ZoneNum) =
                Zone(ZoneNum).Volume * (state.dataRoomAirMod->HeightTransition(ZoneNum) - min(state.dataRoomAirMod->HeightTransition(ZoneNum), 0.2)) /
                CeilingHeight * Zone(ZoneNum).ZoneVolCapMultpSens *
                PsyRhoAirFnPbTdbW(
                    state, state.dataEnvrn->OutBaroPress, state.dataRoomAirMod->MATOC(ZoneNum), state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum)) *
                PsyCpAirFnW(state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum)) / (TimeStepSys * DataGlobalConstants::SecInHour);
            state.dataRoomAirMod->AIRRATMX(ZoneNum) =
                Zone(ZoneNum).Volume * (CeilingHeight - state.dataRoomAirMod->HeightTransition(ZoneNum)) / CeilingHeight *
                Zone(ZoneNum).ZoneVolCapMultpSens *
                PsyRhoAirFnPbTdbW(
                    state, state.dataEnvrn->OutBaroPress, state.dataRoomAirMod->MATMX(ZoneNum), state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum)) *
                PsyCpAirFnW(state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum)) / (TimeStepSys * DataGlobalConstants::SecInHour);

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
            TempHistTerm = AirCap * (3.0 * state.dataRoomAirMod->ZTM1Floor(ZoneNum) - (3.0 / 2.0) * state.dataRoomAirMod->ZTM2Floor(ZoneNum) +
                                     OneThird * state.dataRoomAirMod->ZTM3Floor(ZoneNum));
            TempDepCoef = state.dataDispVentMgr->HA_FLOOR + MCp_Total;
            TempIndCoef = state.dataDispVentMgr->HAT_FLOOR + MCpT_Total + state.dataHeatBalFanSys->NonAirSystemResponse(ZoneNum) / ZoneMult;
            switch (state.dataHeatBal->ZoneAirSolutionAlgo) {
            case DataHeatBalance::SolutionAlgo::ThirdOrder: {
                state.dataRoomAirMod->ZTFloor(ZoneNum) = calculateThirdOrderFloorTemperature(TempHistTerm,
                                                                                             state.dataDispVentMgr->HAT_FLOOR,
                                                                                             state.dataDispVentMgr->HA_FLOOR,
                                                                                             MCpT_Total,
                                                                                             MCp_Total,
                                                                                             state.dataRoomAirMod->ZTOC(ZoneNum),
                                                                                             state.dataHeatBalFanSys->NonAirSystemResponse(ZoneNum),
                                                                                             ZoneMult,
                                                                                             AirCap);
            } break;
            case DataHeatBalance::SolutionAlgo::AnalyticalSolution: {
                if (TempDepCoef == 0.0) { // B=0
                    state.dataRoomAirMod->ZTFloor(ZoneNum) = state.dataRoomAirMod->Zone1Floor(ZoneNum) + TempIndCoef / AirCap;
                } else {
                    state.dataRoomAirMod->ZTFloor(ZoneNum) =
                        (state.dataRoomAirMod->Zone1Floor(ZoneNum) - TempIndCoef / TempDepCoef) * std::exp(min(700.0, -TempDepCoef / AirCap)) +
                        TempIndCoef / TempDepCoef;
                }
            } break;
            case DataHeatBalance::SolutionAlgo::EulerMethod: {
                state.dataRoomAirMod->ZTFloor(ZoneNum) = (AirCap * state.dataRoomAirMod->Zone1Floor(ZoneNum) + TempIndCoef) / (AirCap + TempDepCoef);
            } break;
            default:
                break;
            }
            AirCap = state.dataRoomAirMod->AIRRATOC(ZoneNum);
            TempHistTerm = AirCap * (3.0 * state.dataRoomAirMod->ZTM1OC(ZoneNum) - (3.0 / 2.0) * state.dataRoomAirMod->ZTM2OC(ZoneNum) +
                                     OneThird * state.dataRoomAirMod->ZTM3OC(ZoneNum));
            TempDepCoef = state.dataDispVentMgr->HA_OC + MCp_Total;
            TempIndCoef = ConvGainsOccupiedSubzone * GainsFrac + state.dataDispVentMgr->HAT_OC + state.dataRoomAirMod->ZTFloor(ZoneNum) * MCp_Total;
            switch (state.dataHeatBal->ZoneAirSolutionAlgo) {
            case DataHeatBalance::SolutionAlgo::ThirdOrder: {
                state.dataRoomAirMod->ZTOC(ZoneNum) = (TempHistTerm + ConvGainsOccupiedSubzone * GainsFrac + state.dataDispVentMgr->HAT_OC +
                                                       1.6 * state.dataRoomAirMod->ZTFloor(ZoneNum) * MCp_Total) /
                                                      ((11.0 / 6.0) * AirCap + state.dataDispVentMgr->HA_OC + 1.6 * MCp_Total);
            } break;
            case DataHeatBalance::SolutionAlgo::AnalyticalSolution: {
                if (TempDepCoef == 0.0) { // B=0
                    state.dataRoomAirMod->ZTOC(ZoneNum) = state.dataRoomAirMod->Zone1OC(ZoneNum) + TempIndCoef / AirCap;
                } else {
                    if (AirCap == 0.0) {
                        state.dataRoomAirMod->ZTOC(ZoneNum) = TempIndCoef / TempDepCoef;
                    } else {
                        state.dataRoomAirMod->ZTOC(ZoneNum) =
                            (state.dataRoomAirMod->Zone1OC(ZoneNum) - TempIndCoef / TempDepCoef) * std::exp(min(700.0, -TempDepCoef / AirCap)) +
                            TempIndCoef / TempDepCoef;
                    }
                }
            } break;
            case DataHeatBalance::SolutionAlgo::EulerMethod: {
                state.dataRoomAirMod->ZTOC(ZoneNum) = (AirCap * state.dataRoomAirMod->Zone1OC(ZoneNum) + TempIndCoef) / (AirCap + TempDepCoef);
            } break;
            default:
                break;
            }
            AirCap = state.dataRoomAirMod->AIRRATMX(ZoneNum);
            TempHistTerm = AirCap * (3.0 * state.dataRoomAirMod->ZTM1MX(ZoneNum) - (3.0 / 2.0) * state.dataRoomAirMod->ZTM2MX(ZoneNum) +
                                     OneThird * state.dataRoomAirMod->ZTM3MX(ZoneNum));
            TempDepCoef = state.dataDispVentMgr->HA_MX + MCp_Total;
            TempIndCoef = ConvGainsOccupiedSubzone * (1.0 - GainsFrac) + ConvGainsMixedSubzone + state.dataDispVentMgr->HAT_MX +
                          state.dataRoomAirMod->ZTOC(ZoneNum) * MCp_Total;
            switch (state.dataHeatBal->ZoneAirSolutionAlgo) {
            case DataHeatBalance::SolutionAlgo::ThirdOrder: {
                state.dataRoomAirMod->ZTMX(ZoneNum) = (TempHistTerm + ConvGainsOccupiedSubzone * (1.0 - GainsFrac) + ConvGainsMixedSubzone +
                                                       state.dataDispVentMgr->HAT_MX + state.dataRoomAirMod->ZTOC(ZoneNum) * MCp_Total) /
                                                      ((11.0 / 6.0) * AirCap + state.dataDispVentMgr->HA_MX + MCp_Total);
            } break;
            case DataHeatBalance::SolutionAlgo::AnalyticalSolution: {
                if (TempDepCoef == 0.0) { // B=0
                    state.dataRoomAirMod->ZTMX(ZoneNum) = state.dataRoomAirMod->Zone1MX(ZoneNum) + TempIndCoef / AirCap;
                } else {
                    if (AirCap == 0.0) {
                        state.dataRoomAirMod->ZTMX(ZoneNum) = TempIndCoef / TempDepCoef;
                    } else {
                        state.dataRoomAirMod->ZTMX(ZoneNum) =
                            (state.dataRoomAirMod->Zone1MX(ZoneNum) - TempIndCoef / TempDepCoef) * std::exp(min(700.0, -TempDepCoef / AirCap)) +
                            TempIndCoef / TempDepCoef;
                    }
                }
            } break;
            case DataHeatBalance::SolutionAlgo::EulerMethod: {
                state.dataRoomAirMod->ZTMX(ZoneNum) = (AirCap * state.dataRoomAirMod->Zone1MX(ZoneNum) + TempIndCoef) / (AirCap + TempDepCoef);
            } break;
            default:
                break;
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
    if (state.dataRoomAirMod->ZTMX(ZoneNum) < state.dataRoomAirMod->ZTOC(ZoneNum) || MCp_Total <= 0.0 ||
        HeightFrac * CeilingHeight < (state.dataDispVentMgr->HeightFloorSubzoneTop + state.dataDispVentMgr->ThickOccupiedSubzoneMin)) {
        MIXFLAG = true;
        HeightFrac = 0.0;
        state.dataRoomAirMod->AvgTempGrad(ZoneNum) = 0.0;
        state.dataRoomAirMod->MaxTempGrad(ZoneNum) = 0.0;
        state.dataRoomAirMod->AirModel(ZoneNum).SimAirModel = false;
        AirCap = state.dataHeatBalFanSys->AIRRAT(ZoneNum);
        TempHistTerm = AirCap * (3.0 * state.dataHeatBalFanSys->ZTM1(ZoneNum) - (3.0 / 2.0) * state.dataHeatBalFanSys->ZTM2(ZoneNum) +
                                 OneThird * state.dataHeatBalFanSys->ZTM3(ZoneNum));

        for (Ctd = 1; Ctd <= 3; ++Ctd) {
            TempDepCoef = state.dataDispVentMgr->HA_MX + state.dataDispVentMgr->HA_OC + state.dataDispVentMgr->HA_FLOOR + MCp_Total;
            TempIndCoef = ConvGains + state.dataDispVentMgr->HAT_MX + state.dataDispVentMgr->HAT_OC + state.dataDispVentMgr->HAT_FLOOR + MCpT_Total;
            switch (state.dataHeatBal->ZoneAirSolutionAlgo) {
            case DataHeatBalance::SolutionAlgo::ThirdOrder: {
                ZTAveraged = (TempHistTerm + ConvGains + state.dataDispVentMgr->HAT_MX + state.dataDispVentMgr->HAT_OC +
                              state.dataDispVentMgr->HAT_FLOOR + MCpT_Total) /
                             ((11.0 / 6.0) * AirCap + state.dataDispVentMgr->HA_MX + state.dataDispVentMgr->HA_OC + state.dataDispVentMgr->HA_FLOOR +
                              MCp_Total);
            } break;
            case DataHeatBalance::SolutionAlgo::AnalyticalSolution: {
                if (TempDepCoef == 0.0) { // B=0
                    ZTAveraged = state.dataHeatBalFanSys->ZoneT1(ZoneNum) + TempIndCoef / AirCap;
                } else {
                    ZTAveraged =
                        (state.dataHeatBalFanSys->ZoneT1(ZoneNum) - TempIndCoef / TempDepCoef) * std::exp(min(700.0, -TempDepCoef / AirCap)) +
                        TempIndCoef / TempDepCoef;
                }
            } break;
            case DataHeatBalance::SolutionAlgo::EulerMethod: {
                ZTAveraged = (AirCap * state.dataHeatBalFanSys->ZoneT1(ZoneNum) + TempIndCoef) / (AirCap + TempDepCoef);
            } break;
            default:
                break;
            }
            state.dataRoomAirMod->ZTOC(ZoneNum) = ZTAveraged;
            state.dataRoomAirMod->ZTMX(ZoneNum) = ZTAveraged;
            state.dataRoomAirMod->ZTFloor(ZoneNum) = ZTAveraged;
            HcUCSDDV(state, ZoneNum, HeightFrac);
            TempDepCoef = state.dataDispVentMgr->HA_MX + state.dataDispVentMgr->HA_OC + state.dataDispVentMgr->HA_FLOOR + MCp_Total;
            TempIndCoef = ConvGains + state.dataDispVentMgr->HAT_MX + state.dataDispVentMgr->HAT_OC + state.dataDispVentMgr->HAT_FLOOR + MCpT_Total;
            switch (state.dataHeatBal->ZoneAirSolutionAlgo) {
            case DataHeatBalance::SolutionAlgo::ThirdOrder: {
                ZTAveraged = (TempHistTerm + ConvGains + state.dataDispVentMgr->HAT_MX + state.dataDispVentMgr->HAT_OC +
                              state.dataDispVentMgr->HAT_FLOOR + MCpT_Total) /
                             ((11.0 / 6.0) * AirCap + state.dataDispVentMgr->HA_MX + state.dataDispVentMgr->HA_OC + state.dataDispVentMgr->HA_FLOOR +
                              MCp_Total);
            } break;
            case DataHeatBalance::SolutionAlgo::AnalyticalSolution: {
                if (TempDepCoef == 0.0) { // B=0
                    ZTAveraged = state.dataHeatBalFanSys->ZoneT1(ZoneNum) + TempIndCoef / AirCap;
                } else {
                    ZTAveraged =
                        (state.dataHeatBalFanSys->ZoneT1(ZoneNum) - TempIndCoef / TempDepCoef) * std::exp(min(700.0, -TempDepCoef / AirCap)) +
                        TempIndCoef / TempDepCoef;
                }
            } break;
            case DataHeatBalance::SolutionAlgo::EulerMethod: {
                ZTAveraged = (AirCap * state.dataHeatBalFanSys->ZoneT1(ZoneNum) + TempIndCoef) / (AirCap + TempDepCoef);
            } break;
            default:
                break;
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
    HeightOccupiedSubzoneAve = (state.dataDispVentMgr->HeightFloorSubzoneTop + state.dataRoomAirMod->HeightTransition(ZoneNum)) / 2.0;
    HeightFloorSubzoneAve = state.dataDispVentMgr->HeightFloorSubzoneTop / 2.0;

    // Comfort temperature

    if (MIXFLAG) {
        state.dataRoomAirMod->TCMF(ZoneNum) = ZTAveraged;
    } else {
        if (HeightComfort >= 0.0 && HeightComfort < HeightFloorSubzoneAve) {
            ShowWarningError(state, "Displacement ventilation comfort height is in floor subzone in Zone: " + Zone(ZoneNum).Name);
            state.dataRoomAirMod->TCMF(ZoneNum) = state.dataRoomAirMod->ZTFloor(ZoneNum);
        } else if (HeightComfort >= HeightFloorSubzoneAve && HeightComfort < HeightOccupiedSubzoneAve) {
            state.dataRoomAirMod->TCMF(ZoneNum) = (state.dataRoomAirMod->ZTFloor(ZoneNum) * (HeightOccupiedSubzoneAve - HeightComfort) +
                                                   state.dataRoomAirMod->ZTOC(ZoneNum) * (HeightComfort - HeightFloorSubzoneAve)) /
                                                  (HeightOccupiedSubzoneAve - HeightFloorSubzoneAve);
            //!      TCMF(ZoneNum) = (ZTFloor(ZoneNum) * (HeightOccupiedSubzoneAve - HeightComfort) &
            //!                    + ZTMX(ZoneNum) * (HeightComfort - HeightFloorSubzoneAve)) &
            //!                    / (HeightOccupiedSubzoneAve - HeightFloorSubzoneAve)
        } else if (HeightComfort >= HeightOccupiedSubzoneAve && HeightComfort < HeightMixedSubzoneAve) {
            state.dataRoomAirMod->TCMF(ZoneNum) = (state.dataRoomAirMod->ZTOC(ZoneNum) * (HeightMixedSubzoneAve - HeightComfort) +
                                                   state.dataRoomAirMod->ZTMX(ZoneNum) * (HeightComfort - HeightOccupiedSubzoneAve)) /
                                                  (HeightMixedSubzoneAve - HeightOccupiedSubzoneAve);
        } else if (HeightComfort >= HeightMixedSubzoneAve && HeightComfort <= CeilingHeight) {
            state.dataRoomAirMod->TCMF(ZoneNum) = state.dataRoomAirMod->ZTMX(ZoneNum);
        } else {
            ShowFatalError(state, "Displacement ventilation comfort height is above ceiling or below floor in Zone: " + Zone(ZoneNum).Name);
        }
    }

    // Temperature at the thermostat/temperature control sensor

    if (MIXFLAG) {
        state.dataHeatBalFanSys->TempTstatAir(ZoneNum) = ZTAveraged;
    } else {
        if (HeightThermostat >= 0.0 && HeightThermostat < HeightFloorSubzoneAve) {
            ShowWarningError(state, "Displacement thermostat is in floor subzone in Zone: " + Zone(ZoneNum).Name);
            state.dataHeatBalFanSys->TempTstatAir(ZoneNum) = state.dataRoomAirMod->ZTFloor(ZoneNum);
        } else if (HeightThermostat >= HeightFloorSubzoneAve && HeightThermostat < HeightOccupiedSubzoneAve) {
            state.dataHeatBalFanSys->TempTstatAir(ZoneNum) = (state.dataRoomAirMod->ZTFloor(ZoneNum) * (HeightOccupiedSubzoneAve - HeightThermostat) +
                                                              state.dataRoomAirMod->ZTOC(ZoneNum) * (HeightThermostat - HeightFloorSubzoneAve)) /
                                                             (HeightOccupiedSubzoneAve - HeightFloorSubzoneAve);
            //!      TempTstatAir(ZoneNum) = (ZTFloor(ZoneNum) * (HeightOccupiedSubzoneAve - HeightThermostat) &
            //!                    + ZTMX(ZoneNum) * (HeightThermostat - HeightFloorSubzoneAve)) &
            //!                    / (HeightOccupiedSubzoneAve - HeightFloorSubzoneAve)
        } else if (HeightThermostat >= HeightOccupiedSubzoneAve && HeightThermostat < HeightMixedSubzoneAve) {
            state.dataHeatBalFanSys->TempTstatAir(ZoneNum) = (state.dataRoomAirMod->ZTOC(ZoneNum) * (HeightMixedSubzoneAve - HeightThermostat) +
                                                              state.dataRoomAirMod->ZTMX(ZoneNum) * (HeightThermostat - HeightOccupiedSubzoneAve)) /
                                                             (HeightMixedSubzoneAve - HeightOccupiedSubzoneAve);
        } else if (HeightThermostat >= HeightMixedSubzoneAve && HeightThermostat <= CeilingHeight) {
            state.dataHeatBalFanSys->TempTstatAir(ZoneNum) = state.dataRoomAirMod->ZTMX(ZoneNum);
        } else {
            ShowFatalError(state, "Displacement ventilation thermostat height is above ceiling or below floor in Zone: " + Zone(ZoneNum).Name);
        }
    }

    // Temperature gradients

    if ((HeightMixedSubzoneAve - HeightFloorSubzoneAve) > 0.1) {
        state.dataRoomAirMod->AvgTempGrad(ZoneNum) =
            (state.dataRoomAirMod->ZTMX(ZoneNum) - state.dataRoomAirMod->ZTFloor(ZoneNum)) / (HeightMixedSubzoneAve - HeightFloorSubzoneAve);
    } else {
        state.dataRoomAirMod->AvgTempGrad(ZoneNum) = -9.999;
    }
    if ((HeightOccupiedSubzoneAve - HeightFloorSubzoneAve) > 0.1) {
        state.dataRoomAirMod->MaxTempGrad(ZoneNum) =
            (state.dataRoomAirMod->ZTOC(ZoneNum) - state.dataRoomAirMod->ZTFloor(ZoneNum)) / (HeightOccupiedSubzoneAve - HeightFloorSubzoneAve);
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

    if (state.dataZoneEquip->ZoneEquipConfig(ZoneNum).IsControlled) {
        ZoneNodeNum = Zone(ZoneNum).SystemZoneNodeNumber;
        state.dataLoopNodes->Node(ZoneNodeNum).Temp = state.dataRoomAirMod->ZTMX(ZoneNum);
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

} // namespace EnergyPlus::DisplacementVentMgr
