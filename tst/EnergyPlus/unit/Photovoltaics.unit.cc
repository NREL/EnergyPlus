// EnergyPlus, Copyright (c) 1996-2017, The Board of Trustees of the University of Illinois and
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy). All rights
// reserved.
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

// EnergyPlus::ElectricPowerServiceManager Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <Photovoltaics.hh>
#include <DataPhotovoltaics.hh>
#include <DataHeatBalance.hh>
#include <DataSurfaces.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;

TEST_F( EnergyPlusFixture, PV_Sandia_AirMassAtHighZenith )
{
	// unit test for issue #5528, test behavior of function AbsoluteAirMass at zenith angle above 89.9 degrees.

	// first check above the degree threshold, compare to side calc
	Real64 zenithAngleDeg = 90.0;
	Real64 altitude  = 1;
	Real64 airMass = Photovoltaics::AbsoluteAirMass( zenithAngleDeg, altitude );
	EXPECT_NE( airMass, 999 ); // would have been true before fix
	EXPECT_NEAR( airMass, 36.31531 , 0.1 );

	// now check below the threshold, compare to side calc spreadhsheet result
	zenithAngleDeg = 89.0;
	airMass = Photovoltaics::AbsoluteAirMass( zenithAngleDeg, altitude );
	EXPECT_NEAR( airMass, 26.24135, 0.1 );

}

TEST_F( EnergyPlusFixture, PV_ReportPV_ZoneIndexNonZero )
{
	// unit test for issue #6222, test to make sure zone index in surface on which PV is placed is not zero so zone multiplier is applied properly
	
	EnergyPlus::DataPhotovoltaics::PVarray.deallocate();
	DataHeatBalance::Zone.deallocate();
	DataSurfaces::Surface.deallocate();
	
	EnergyPlus::DataPhotovoltaics::PVarray.allocate( 3 );
	DataHeatBalance::Zone.allocate( 2 );
	DataSurfaces::Surface.allocate( 3 );
	
	DataGlobals::NumOfZones = 2;
	DataHeatBalance::Zone( 1 ).Name = "Zone1";
	DataHeatBalance::Zone( 1 ).ListMultiplier = 1.0;
	DataHeatBalance::Zone( 1 ).Multiplier = 5.0;
	DataHeatBalance::Zone( 2 ).Name = "Zone2";
	DataHeatBalance::Zone( 2 ).ListMultiplier = 10.0;
	DataHeatBalance::Zone( 2 ).Multiplier = 1.0;
	
	EnergyPlus::DataPhotovoltaics::NumPVs = 3;
	EnergyPlus::DataPhotovoltaics::PVarray( 1 ).SurfacePtr = 1;
	EnergyPlus::DataPhotovoltaics::PVarray( 1 ).CellIntegrationMode = -9999;
	EnergyPlus::DataPhotovoltaics::PVarray( 2 ).SurfacePtr = 2;
	EnergyPlus::DataPhotovoltaics::PVarray( 2 ).CellIntegrationMode = -9999;
	EnergyPlus::DataPhotovoltaics::PVarray( 3 ).SurfacePtr = 3;
	EnergyPlus::DataPhotovoltaics::PVarray( 3 ).CellIntegrationMode = -9999;

	DataSurfaces::Surface( 1 ).Zone = 1;
	DataSurfaces::Surface( 1 ).ZoneName = "Zone1";
	DataSurfaces::Surface( 2 ).Zone = 0;
	DataSurfaces::Surface( 2 ).ZoneName = "Zone2";
	DataSurfaces::Surface( 3 ).Zone = 0;
	DataSurfaces::Surface( 3 ).ZoneName = "None";
	
	// Test 1: Zone 1--PV has multiplier, Zone index already set
	EnergyPlus::DataPhotovoltaics::PVarray( 1 ).Report.DCPower = 1000.0;
	Photovoltaics::ReportPV( 1 );
	EXPECT_NEAR( EnergyPlus::DataPhotovoltaics::PVarray( 1 ).Report.DCPower, 5000.0, 0.1 );
	
	// Test 2: Zone 2--PV has multiplier, Zone index not set yet
	EnergyPlus::DataPhotovoltaics::PVarray( 2 ).Report.DCPower = 1000.0;
	Photovoltaics::ReportPV( 2 );
	EXPECT_NEAR( EnergyPlus::DataPhotovoltaics::PVarray( 2 ).Report.DCPower, 10000.0, 0.1 );
	EXPECT_EQ( DataSurfaces::Surface( 2 ).Zone, 2 );
	
	// Test 3: Zone 2--PV has multiplier, Zone index set by previous pass through ReportPV
	EnergyPlus::DataPhotovoltaics::PVarray( 2 ).Report.DCPower = 500.0;
	Photovoltaics::ReportPV( 2 );
	EXPECT_NEAR( EnergyPlus::DataPhotovoltaics::PVarray( 2 ).Report.DCPower, 5000.0, 0.1 );

	// Test 4: Zone 3--PV not attached to any zone, Zone Index does not get reset
	EnergyPlus::DataPhotovoltaics::PVarray( 3 ).Report.DCPower = 1000.0;
	Photovoltaics::ReportPV( 3 );
	EXPECT_NEAR( EnergyPlus::DataPhotovoltaics::PVarray( 3 ).Report.DCPower, 1000.0, 0.1 );
	EXPECT_EQ( DataSurfaces::Surface( 3 ).Zone, 0 );

}

