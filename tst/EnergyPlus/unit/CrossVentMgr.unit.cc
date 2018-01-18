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
#include <EnergyPlus/AirflowNetworkBalanceManager.hh>
#include <EnergyPlus/CrossVentMgr.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/DataAirflowNetwork.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataRoomAirModel.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/HeatBalanceAirManager.hh>
#include <EnergyPlus/SimulationManager.hh>
#include <EnergyPlus/DataUCSDSharedData.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::CrossVentMgr;
using namespace EnergyPlus::DataRoomAirModel;


TEST_F( EnergyPlusFixture, CrossVentMgr_EvolveParaUCSDCV_Test )
{

	// set up all conditions entering the EvolveParaUCSDCV when using the Issue #5520 test file on hitcount 9925 (where it used to crash)
	int NumOfZones = 2;
	int MaxSurf = 2;

	RecInflowRatio.allocate( NumOfZones );

	AirflowNetworkSurfaceUCSDCV.allocate( { 0, MaxSurf }, NumOfZones );
	AirflowNetworkSurfaceUCSDCV( 1, 1 ) = 1;
	AirflowNetworkSurfaceUCSDCV( 0, 1 ) = 1;
	AirflowNetworkSurfaceUCSDCV( 0, 2 ) = 2;

	EnergyPlus::DataAirflowNetwork::MultizoneSurfaceData.allocate( MaxSurf );
	EnergyPlus::DataAirflowNetwork::MultizoneSurfaceData( 1 ).SurfNum = 6;
	EnergyPlus::DataAirflowNetwork::MultizoneSurfaceData( 1 ).OpenFactor = 1.;
	EnergyPlus::DataAirflowNetwork::MultizoneSurfaceData( 2 ).SurfNum = 9;
	EnergyPlus::DataAirflowNetwork::MultizoneSurfaceData( 2 ).OpenFactor = 1.;

	EnergyPlus::DataSurfaces::Surface.allocate( 10 );
	EnergyPlus::DataSurfaces::Surface( 6 ).Zone = 1;
	EnergyPlus::DataSurfaces::Surface( 6 ).Azimuth = 0.0;
	EnergyPlus::DataSurfaces::Surface( 6 ).BaseSurf = 5;
	EnergyPlus::DataSurfaces::Surface( 5 ).Sides = 4;
	EnergyPlus::DataSurfaces::Surface( 5 ).Centroid.x = 13.143481000000001;
	EnergyPlus::DataSurfaces::Surface( 5 ).Centroid.y = 13.264719000000003;
	EnergyPlus::DataSurfaces::Surface( 5 ).Centroid.z = 1.6002000000000001;
	EnergyPlus::DataSurfaces::Surface( 7 ).Sides = 4;
	EnergyPlus::DataSurfaces::Surface( 7 ).Centroid.x = 25.415490999999996;
	EnergyPlus::DataSurfaces::Surface( 7 ).Centroid.y = 7.1687189999999994;
	EnergyPlus::DataSurfaces::Surface( 7 ).Centroid.z = 1.6763999999999999;
	EnergyPlus::DataSurfaces::Surface( 8 ).Sides = 4;
	EnergyPlus::DataSurfaces::Surface( 8 ).Centroid.x = 13.223490999999997;
	EnergyPlus::DataSurfaces::Surface( 8 ).Centroid.y = 1.0727189999999998;
	EnergyPlus::DataSurfaces::Surface( 8 ).Centroid.z = 1.6763999999999999;
	EnergyPlus::DataSurfaces::Surface( 10 ).Sides = 4;
	EnergyPlus::DataSurfaces::Surface( 10 ).Centroid.x = 1.0314909999999999;
	EnergyPlus::DataSurfaces::Surface( 10 ).Centroid.y = 7.1687189999999994;
	EnergyPlus::DataSurfaces::Surface( 10 ).Centroid.z = 1.6763999999999999;

	EnergyPlus::DataHeatBalance::Zone.allocate( 1 );
	EnergyPlus::DataHeatBalance::Zone( 1 ).Volume = 996.75300003839993;
	EnergyPlus::DataHeatBalance::Zone( 1 ).FloorArea = 297.28972800000003;

	EnergyPlus::DataAirflowNetwork::AirflowNetworkLinkSimu.allocate( 1 );
	EnergyPlus::DataAirflowNetwork::AirflowNetworkLinkSimu( 1 ).VolFLOW2 = 27.142934345451458;

	EnergyPlus::DataEnvironment::WindDir = 271.66666666666669;

	EnergyPlus::DataRoomAirModel::AirModel.allocate( NumOfZones );

	EnergyPlus::DataAirflowNetwork::AirflowNetworkLinkageData.allocate( 2 );
	EnergyPlus::DataAirflowNetwork::AirflowNetworkLinkageData( 1 ).CompNum = 1;
	EnergyPlus::DataAirflowNetwork::AirflowNetworkLinkageData( 2 ).CompNum = 1;

	EnergyPlus::DataAirflowNetwork::AirflowNetworkCompData.allocate( 3 );
	EnergyPlus::DataAirflowNetwork::AirflowNetworkCompData( 1 ).TypeNum = 1;
	EnergyPlus::DataAirflowNetwork::AirflowNetworkCompData( 1 ).CompTypeNum = 1;
	EnergyPlus::DataAirflowNetwork::AirflowNetworkCompData( 2 ).TypeNum = 1;
	EnergyPlus::DataAirflowNetwork::AirflowNetworkCompData( 2 ).CompTypeNum = 3;
	EnergyPlus::DataAirflowNetwork::AirflowNetworkCompData( 3 ).TypeNum = 2;
	EnergyPlus::DataAirflowNetwork::AirflowNetworkCompData( 3 ).CompTypeNum = 2;

	EnergyPlus::DataRoomAirModel::SurfParametersCVDV.allocate( 2 );
	EnergyPlus::DataRoomAirModel::SurfParametersCVDV( 1 ).Width = 22.715219999999999;
	EnergyPlus::DataRoomAirModel::SurfParametersCVDV( 1 ).Height = 1.3715999999999999;
	EnergyPlus::DataRoomAirModel::SurfParametersCVDV( 2 ).Width = 22.869143999999999;
	EnergyPlus::DataRoomAirModel::SurfParametersCVDV( 2 ).Height = 1.3715999999999999;

	CVJetRecFlows.allocate( { 0, MaxSurf }, 1 );

	EnergyPlus::DataUCSDSharedData::PosZ_Wall.allocate( 2 );
	EnergyPlus::DataUCSDSharedData::PosZ_Wall( 1 ) = 1;
	EnergyPlus::DataUCSDSharedData::PosZ_Wall( 2 ) = 4;

	EnergyPlus::DataUCSDSharedData::APos_Wall.allocate( 12 );
	EnergyPlus::DataUCSDSharedData::APos_Wall( 1 ) = 5;
	EnergyPlus::DataUCSDSharedData::APos_Wall( 2 ) = 7;
	EnergyPlus::DataUCSDSharedData::APos_Wall( 3 ) = 8;
	EnergyPlus::DataUCSDSharedData::APos_Wall( 4 ) = 10;

	EnergyPlus::DataRoomAirModel::Droom.allocate( NumOfZones );
	EnergyPlus::DataRoomAirModel::Droom( 1 ) = 13.631070390838719;

	EnergyPlus::DataRoomAirModel::Dstar.allocate( NumOfZones );

	EnergyPlus::DataRoomAirModel::Ain.allocate( NumOfZones );

	EnergyPlus::DataRoomAirModel::ZoneUCSDCV.allocate( NumOfZones );
	EnergyPlus::DataRoomAirModel::ZoneUCSDCV( 1 ).ZonePtr = 1;

	EnergyPlus::DataRoomAirModel::JetRecAreaRatio.allocate( NumOfZones );
	EnergyPlus::DataRoomAirModel::Ujet.allocate( NumOfZones );
	EnergyPlus::DataRoomAirModel::Urec.allocate( NumOfZones );
	EnergyPlus::DataRoomAirModel::Qrec.allocate( NumOfZones );
	EnergyPlus::DataRoomAirModel::Qtot.allocate( NumOfZones );
	EnergyPlus::DataRoomAirModel::Tin.allocate( NumOfZones );

	EvolveParaUCSDCV( 1 );

	EXPECT_NEAR( 27.14, CVJetRecFlows( 1, 1 ).Fin, 0.01 );
	EXPECT_NEAR( 0.871, CVJetRecFlows( 1, 1 ).Uin, 0.001 );
	EXPECT_NEAR( 0.000, CVJetRecFlows( 1, 1 ).Vjet, 0.001 );
	EXPECT_NEAR( 0.243, CVJetRecFlows( 1, 1 ).Yjet, 0.001 );
	EXPECT_NEAR( 0.279, CVJetRecFlows( 1, 1 ).Ujet, 0.001 );
	EXPECT_NEAR( 0.070, CVJetRecFlows( 1, 1 ).Yrec, 0.001 );
	EXPECT_NEAR( 0.080, CVJetRecFlows( 1, 1 ).Urec, 0.001 );
	EXPECT_NEAR( 0.466, CVJetRecFlows( 1, 1 ).YQrec, 0.001 );
	EXPECT_NEAR( 0.535, CVJetRecFlows( 1, 1 ).Qrec, 0.001 );

}


