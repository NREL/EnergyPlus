// EnergyPlus, Copyright (c) 1996-2018, The Board of Trustees of the University of Illinois,
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

// EnergyPlus::RoomAirModelUserTempPattern Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/DisplacementVentMgr.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataRoomAirModel.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/HeatBalanceAirManager.hh>
#include <EnergyPlus/DataUCSDSharedData.hh>
#include <EnergyPlus/DataHeatBalSurface.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::DataSurfaces;
using namespace EnergyPlus::DataHeatBalance;
using namespace EnergyPlus::DataUCSDSharedData;
using namespace EnergyPlus::DisplacementVentMgr;
using namespace EnergyPlus::DataRoomAirModel;


TEST_F( EnergyPlusFixture, DisplacementVentMgr_HcUCSDDV_Door_Test )
{

	// set up all conditions entering the HcUCSDDV for Issue #5533 to cause NAN when a floor door is entered
	// 3 door types are tested: wall, floor and ceiling
	int NumOfZones = 1;
	int TotSurfaces = 3;

	IsZoneDV.allocate( NumOfZones );
	IsZoneDV( 1 ) = true;
	Surface.allocate( TotSurfaces );
	TempEffBulkAir.allocate( TotSurfaces );
	EnergyPlus::DataHeatBalSurface::TempSurfIn.allocate( TotSurfaces );
	DVHcIn.allocate( TotSurfaces );
	ZTMX.allocate( NumOfZones );
	ZTOC.allocate( NumOfZones );

	// Surface 1 Vertical wall
	Surface( 1 ).Name = "Class1_Wall_6_0_0_0_0_0_Subsurface";
	Surface( 1 ).Class = SurfaceClass_Door;
	Surface( 1 ).Zone = 1;
	Surface( 1 ).HeatTransSurf = true;
	Surface( 1 ).Construction = 1;
	Surface( 1 ).BaseSurf = 1;
	Surface( 1 ).Sides = 4;
	Surface( 1 ).Area = 10;
	Surface( 1 ).Azimuth = 0;
	Surface( 1 ).Tilt = 90;
	Surface( 1 ).Vertex.allocate( Surface( 1 ).Sides );
	Surface( 1 ).Vertex( 1 ).x = -11.57740998;
	Surface( 1 ).Vertex( 1 ).y = -12.31054602;
	Surface( 1 ).Vertex( 1 ).z = 4.9804;
	Surface( 1 ).Vertex( 2 ).x = -10.95990365;
	Surface( 1 ).Vertex( 2 ).y = -12.31054602;
	Surface( 1 ).Vertex( 2 ).z = 4.9804;
	Surface( 1 ).Vertex( 3 ).x = -10.95990365;
	Surface( 1 ).Vertex( 3 ).y = -12.31054602;
	Surface( 1 ).Vertex( 3 ).z = 5.54536;
	Surface( 1 ).Vertex( 4 ).x = -11.57740998;
	Surface( 1 ).Vertex( 4 ).y = -12.31054602;
	Surface( 1 ).Vertex( 4 ).z = 5.54536;

	// Surface 2 Door floor
	Surface( 2 ).Name = "Class1_Floor_9_0_8_0_8_0_Subsurface";
	Surface( 2 ).Class = SurfaceClass_Door;
	Surface( 2 ).Zone = 1;
	Surface( 2 ).HeatTransSurf = true;
	Surface( 2 ).Construction = 1;
	Surface( 2 ).BaseSurf = 1;
	Surface( 2 ).Sides = 4;
	Surface( 2 ).Area = 10;
	Surface( 2 ).Azimuth = 0;
	Surface( 2 ).Tilt = 180;
	Surface( 2 ).Vertex.allocate( Surface( 1 ).Sides );
	Surface( 2 ).Vertex( 1 ).x = -4.80163552;
	Surface( 2 ).Vertex( 1 ).y = -9.86732154;
	Surface( 2 ).Vertex( 1 ).z = 4.9784;
	Surface( 2 ).Vertex( 2 ).x = -13.28288246;
	Surface( 2 ).Vertex( 2 ).y = -9.86732154;
	Surface( 2 ).Vertex( 2 ).z = 4.9784;
	Surface( 2 ).Vertex( 3 ).x = -13.28288246;
	Surface( 2 ).Vertex( 3 ).y = -1.66421151;
	Surface( 2 ).Vertex( 3 ).z = 4.9784;
	Surface( 2 ).Vertex( 4 ).x = -4.80163552;
	Surface( 2 ).Vertex( 4 ).y = -1.66421151;
	Surface( 2 ).Vertex( 4 ).z = 4.9784;

	// Surface 3 Door ceiling
	Surface( 3 ).Name = "Class1_Ceiling_0_0_9_0_9_0_Subsurface";
	Surface( 3 ).HeatTransSurf = true;
	Surface( 3 ).Zone = 1;
	Surface( 3 ).Class = SurfaceClass_Door;
	Surface( 3 ).Construction = 1;
	Surface( 3 ).BaseSurf = 1;
	Surface( 3 ).Sides = 4;
	Surface( 3 ).Area = 10;
	Surface( 3 ).Azimuth = 0;
	Surface( 3 ).Tilt = 0;
	Surface( 3 ).Vertex.allocate( Surface( 1 ).Sides );
	Surface( 3 ).Vertex( 1 ).x = -12.19542308;
	Surface( 3 ).Vertex( 1 ).y = -9.84254602;
	Surface( 3 ).Vertex( 1 ).z = 8.5343999852;
	Surface( 3 ).Vertex( 2 ).x = -3.83980708;
	Surface( 3 ).Vertex( 2 ).y = -9.84254602;
	Surface( 3 ).Vertex( 2 ).z = 8.5343999852;
	Surface( 3 ).Vertex( 3 ).x = -3.83980708;
	Surface( 3 ).Vertex( 3 ).y = -1.48693002;
	Surface( 3 ).Vertex( 3 ).z = 8.5343999852;
	Surface( 3 ).Vertex( 4 ).x = -12.19542308;
	Surface( 3 ).Vertex( 4 ).y = -1.48693002;
	Surface( 3 ).Vertex( 4 ).z = 8.5343999852;

	EnergyPlus::DataRoomAirModel::AirModel.allocate( NumOfZones );
	AirModel( 1 ).AirModelType = RoomAirModel_UCSDDV;

	APos_Wall.allocate( TotSurfaces );
	APos_Floor.allocate( TotSurfaces );
	APos_Ceiling.allocate( TotSurfaces );
	PosZ_Wall.allocate( NumOfZones * 2 );
	PosZ_Floor.allocate( NumOfZones * 2 );
	PosZ_Ceiling.allocate( NumOfZones * 2 );
	APos_Window.allocate( TotSurfaces );
	APos_Door.allocate( TotSurfaces );
	APos_Internal.allocate( TotSurfaces );
	PosZ_Window.allocate( NumOfZones * 2 );
	PosZ_Door.allocate( NumOfZones * 2 );
	PosZ_Internal.allocate( NumOfZones * 2 );
	HCeiling.allocate( TotSurfaces );
	HWall.allocate( TotSurfaces );
	HFloor.allocate( TotSurfaces );
	HInternal.allocate( TotSurfaces );
	HWindow.allocate( TotSurfaces );
	HDoor.allocate( TotSurfaces );

	ZoneCeilingHeight.allocate( NumOfZones * 2 );
	ZoneCeilingHeight( 1 ) = 4.9784;
	ZoneCeilingHeight( 2 ) = 4.9784;

	// Arrays initializations
	APos_Wall = 0;
	APos_Floor = 0;
	APos_Ceiling = 0;
	PosZ_Wall( 1 ) = 1;
	PosZ_Wall( 2 ) = 0;
	PosZ_Floor( 1 ) = 1;
	PosZ_Floor( 2 ) = 0;
	PosZ_Ceiling( 1 ) = 1;
	PosZ_Ceiling( 2 ) = 0;
	APos_Window = 0;
	APos_Door = 0;
	APos_Internal = 0;
	PosZ_Window( 1 ) = 1;
	PosZ_Window( 2 ) = 0;
	PosZ_Door( 1 ) = 1;
	PosZ_Door( 2 ) = 3;
	PosZ_Internal( 1 ) = 1;
	PosZ_Internal( 2 ) = 0;
	HCeiling = 0.0;
	HWall = 0.0;
	HFloor = 0.0;
	HInternal = 0.0;
	HWindow = 0.0;
	HDoor = 0.0;

	EnergyPlus::DataRoomAirModel::ZoneUCSDCV.allocate( NumOfZones );
	EnergyPlus::DataRoomAirModel::ZoneUCSDCV( 1 ).ZonePtr = 1;
	PosZ_Door( 1 ) = 1;
	PosZ_Door( 2 ) = 3;
	APos_Door( 1 ) = 1;
	APos_Door( 2 ) = 2;
	APos_Door( 3 ) = 3;

	ZTMX( 1 ) = 20.0;
	ZTOC( 1 ) = 21.0;
	EnergyPlus::DataHeatBalSurface::TempSurfIn( 1 ) = 23.0;
	EnergyPlus::DataHeatBalSurface::TempSurfIn( 2 ) = 23.0;
	EnergyPlus::DataHeatBalSurface::TempSurfIn( 3 ) = 23.0;

	HcUCSDDV( 1, 0.5 );

	EXPECT_NEAR( 1.889346, DVHcIn( 1 ), 0.0001 );
	EXPECT_NEAR( 1.650496, DVHcIn( 2 ), 0.0001 );
	EXPECT_NEAR( 1.889346, DVHcIn( 3 ), 0.0001 );
	EXPECT_NEAR( 379.614212, HAT_OC, 0.0001 );
	EXPECT_NEAR( 16.504965, HA_OC, 0.0001 );
	EXPECT_NEAR( 869.099591, HAT_MX, 0.0001 );
	EXPECT_NEAR( 37.786938, HA_MX, 0.0001 );

	IsZoneDV.deallocate( );
	Surface.deallocate( );
	TempEffBulkAir.deallocate( );
	EnergyPlus::DataHeatBalSurface::TempSurfIn.deallocate( );
	DVHcIn.deallocate( );
	ZTMX.deallocate( );
	ZTOC.deallocate( );
	EnergyPlus::DataRoomAirModel::AirModel.deallocate( );

	APos_Wall.deallocate( );
	APos_Floor.deallocate( );
	APos_Ceiling.deallocate( );
	PosZ_Wall.deallocate( );
	PosZ_Floor.deallocate( );
	PosZ_Ceiling.deallocate( );
	APos_Window.deallocate( );
	APos_Door.deallocate( );
	APos_Internal.deallocate( );
	PosZ_Window.deallocate( );
	PosZ_Door.deallocate( );
	PosZ_Internal.deallocate( );
	HCeiling.deallocate( );
	HWall.deallocate( );
	HFloor.deallocate( );
	HInternal.deallocate( );
	HWindow.deallocate( );
	HDoor.deallocate( );
	ZoneCeilingHeight.deallocate( );
	EnergyPlus::DataRoomAirModel::ZoneUCSDCV.deallocate( );

}

TEST_F( EnergyPlusFixture, DVThirdOrderFloorTempCalculation )
{
	Real64 const tempHistoryTerm = 0; // no history
	Real64 const HAT_floor = 20;
	Real64 const HA_floor = 1;
	Real64 const MCpT_Total = 40;
	Real64 const MCp_Total = 2;
	Real64 const occupiedTemp = 25;
	Real64 const nonAirSystemResponse = 0;
	Real64 const zoneMultiplier = 1;
	Real64 const airCap = 100;
	
	Real64 temp = calculateThirdOrderFloorTemperature( tempHistoryTerm, HAT_floor, HA_floor, MCpT_Total, MCp_Total, occupiedTemp, nonAirSystemResponse, zoneMultiplier, airCap );
	EXPECT_NEAR( 0.4799, temp, 0.0001 );
}	

