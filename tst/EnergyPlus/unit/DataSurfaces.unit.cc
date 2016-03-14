// EnergyPlus, Copyright (c) 1996-2016, The Board of Trustees of the University of Illinois and
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy). All rights
// reserved.
//
// If you have questions about your rights to use or distribute this software, please contact
// Berkeley Lab's Innovation & Partnerships Office at IPO@lbl.gov.
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
//     similar designation, without Lawrence Berkeley National Laboratory's prior written consent.
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
//
// You are under no obligation whatsoever to provide any bug fixes, patches, or upgrades to the
// features, functionality or performance of the source code ("Enhancements") to anyone; however,
// if you choose to make your Enhancements available either publicly, or directly to Lawrence
// Berkeley National Laboratory, without imposing a separate written license agreement for such
// Enhancements, then you hereby grant the following license: a non-exclusive, royalty-free
// perpetual license to install, use, modify, prepare derivative works, incorporate into other
// computer software, distribute, and sublicense such enhancements or derivative works thereof,
// in binary and source code form.

// EnergyPlus::DataSurfaces Unit Tests

// C++ Headers
#include <cmath>

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/SurfaceGeometry.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace EnergyPlus::DataEnvironment;
using namespace EnergyPlus::DataGlobals;
using namespace EnergyPlus::DataHeatBalance;
using namespace EnergyPlus::DataSurfaces;
using namespace EnergyPlus::HeatBalanceManager;
using namespace EnergyPlus::SurfaceGeometry;
using namespace ObjexxFCL;
using DataVectorTypes::Vector;

TEST_F( EnergyPlusFixture, DataSurfaces_SetSurfaceOutBulbTempAtTest )
{

	bool ErrorsFound( false );

	std::string const idf_objects = delimited_string({
		"Version,",
		"    8.4;                     !- Version Identifier",

		"	BuildingSurface:Detailed,",
		"    T3-RF1 - Floor:n,        !- Name",
		"    Floor,                   !- Surface Type",
		"    ExtSlabCarpet 4in ClimateZone 1-8,  !- Construction Name",
		"    T3-RF1,                  !- Zone Name",
		"    Outdoors,                !- Outside Boundary Condition",
		"    ,                        !- Outside Boundary Condition Object",
		"    NoSun,                   !- Sun Exposure",
		"    NoWind,                  !- Wind Exposure",
		"    ,                        !- View Factor to Ground",
		"    ,                        !- Number of Vertices",
		"    -73.4395447868102,       !- Vertex 1 X-coordinate {m}",
		"    115.81641271866,         !- Vertex 1 Y-coordinate {m}",
		"    -4.90860981523342e-014,  !- Vertex 1 Z-coordinate {m}",
		"    -58.0249751030646,       !- Vertex 2 X-coordinate {m}",
		"    93.1706338416311,        !- Vertex 2 Y-coordinate {m}",
		"    -6.93120848813091e-014,  !- Vertex 2 Z-coordinate {m}",
		"    -68.9295447868101,       !- Vertex 3 X-coordinate {m}",
		"    74.3054685889134,        !- Vertex 3 Y-coordinate {m}",
		"    -6.06384403665968e-014,  !- Vertex 3 Z-coordinate {m}",
		"    -58.0345461881513,       !- Vertex 4 X-coordinate {m}",
		"    93.1761597101821,        !- Vertex 4 Y-coordinate {m}",
		"    -6.9300904918858e-014;   !- Vertex 4 Z-coordinate {m}",

		"Zone,",
		"    T3-RF1,                  !- Name",
		"    60,                      !- Direction of Relative North {deg}",
		"    234.651324196041,        !- X Origin {m}",
		"    -132.406575100608,       !- Y Origin {m}",
		"    14.8000000000003,        !- Z Origin {m}",
		"    ,                        !- Type",
		"    ,                        !- Multiplier",
		"    ,                        !- Ceiling Height {m}",
		"    ,                        !- Volume {m3}",
		"    ,                        !- Floor Area {m2}",
		"    ,                        !- Zone Inside Convection Algorithm",
		"    ,                        !- Zone Outside Convection Algorithm",
		"    No;                      !- Part of Total Floor Area",

		"Construction,",
		"    ExtSlabCarpet 4in ClimateZone 1-8,  !- Name",
		"    MAT-CC05 4 HW CONCRETE,  !- Outside Layer",
		"    CP02 CARPET PAD;         !- Layer 2",

		"Material,",
		"    MAT-CC05 4 HW CONCRETE,  !- Name",
		"    Rough,                   !- Roughness",
		"    0.1016,                  !- Thickness {m}",
		"    1.311,                   !- Conductivity {W/m-K}",
		"    2240,                    !- Density {kg/m3}",
		"    836.800000000001,        !- Specific Heat {J/kg-K}",
		"    0.9,                     !- Thermal Absorptance",
		"    0.85,                    !- Solar Absorptance",
		"    0.85;                    !- Visible Absorptance",

		"Material:NoMass,",
		"    CP02 CARPET PAD,         !- Name",
		"    Smooth,                  !- Roughness",
		"    0.1,                     !- Thermal Resistance {m2-K/W}",
		"    0.9,                     !- Thermal Absorptance",
		"    0.8,                     !- Solar Absorptance",
		"    0.8;                     !- Visible Absorptance",

		"SurfaceConvectionAlgorithm:Inside,TARP;",

		"SurfaceConvectionAlgorithm:Outside,DOE-2;",

		"HeatBalanceAlgorithm,ConductionTransferFunction;",

		"ZoneAirHeatBalanceAlgorithm,",
		"    AnalyticalSolution;      !- Algorithm",

		});

	ASSERT_FALSE( process_idf( idf_objects ) );

	ErrorsFound = false;
	GetProjectControlData( ErrorsFound ); // read project control data
	EXPECT_FALSE( ErrorsFound ); // expect no errors

	ErrorsFound = false;
	GetMaterialData( ErrorsFound ); // read material data
	EXPECT_FALSE( ErrorsFound ); // expect no errors

	ErrorsFound = false;
	GetConstructData( ErrorsFound ); // read construction data
	EXPECT_FALSE( ErrorsFound ); // expect no errors

	ErrorsFound = false;
	GetZoneData( ErrorsFound ); // read zone data
	EXPECT_FALSE( ErrorsFound ); // expect no errors

	CosZoneRelNorth.allocate( 1 );
	SinZoneRelNorth.allocate( 1 );

	CosZoneRelNorth( 1 ) = std::cos( -Zone( 1 ).RelNorth * DegToRadians );
	SinZoneRelNorth( 1 ) = std::sin( -Zone( 1 ).RelNorth * DegToRadians );
	CosBldgRelNorth = 1.0;
	SinBldgRelNorth = 0.0;

	ErrorsFound = false;
	GetSurfaceData( ErrorsFound ); // setup zone geometry and get zone data
	EXPECT_FALSE( ErrorsFound ); // expect no errors

	SetSurfaceOutBulbTempAt();
	EXPECT_EQ( "T3-RF1 - FLOOR:N", Surface( 1 ).Name );
	EXPECT_GT( Surface( 1 ).Centroid.z, 20000.0 ); // this condition is fatal
	EXPECT_LT( Surface( 1 ).OutDryBulbTemp, -100.0 ); // this condition is fatal
	EXPECT_LT( Surface( 1 ).OutWetBulbTemp, -100.0 ); // this condition is fatal
}

TEST( SurfaceTest, Plane )
{
	{
		SurfaceData s;
		s.Vertex.dimension( 3 );
		s.Vertex = { Vector(1,1,1), Vector(-1,1,0), Vector(2,0,3) };
		s.Shape = SurfaceShape::Triangle;
		s.set_computed_geometry();

		EXPECT_DOUBLE_EQ( -1.0, s.plane.x );
		EXPECT_DOUBLE_EQ(  3.0, s.plane.y );
		EXPECT_DOUBLE_EQ(  2.0, s.plane.z );
		EXPECT_DOUBLE_EQ( -4.0, s.plane.w );
	}
	{
		SurfaceData s;
		s.Vertex.dimension( 3 );
		s.Vertex = { Vector(2,1,-1), Vector(0,-2,0), Vector(1,-1,2) };
		s.Shape = SurfaceShape::Triangle;
		s.set_computed_geometry();

		EXPECT_DOUBLE_EQ( -7.0, s.plane.x );
		EXPECT_DOUBLE_EQ(  5.0, s.plane.y );
		EXPECT_DOUBLE_EQ(  1.0, s.plane.z );
		EXPECT_DOUBLE_EQ( 10.0, s.plane.w );
	}
}

TEST( SurfaceTest, Surface2D )
{
	{
		using Vector2D = Surface2D::Vector2D;
		SurfaceData s;
		s.Vertex.dimension( 4 );
		s.Vertex = { Vector(0,0,0), Vector(1,0,0), Vector(1,1,0), Vector(0,1,0) };
		s.Shape = SurfaceShape::Rectangle;
		s.set_computed_geometry();

		Surface2D const & s2d( s.surface2d );
		EXPECT_EQ( 2, s2d.axis ); // Projection along z axis
		EXPECT_EQ( Vector2D(0,0), s2d.vertices[ 0 ] );
		EXPECT_EQ( Vector2D(1,0), s2d.vertices[ 1 ] );
		EXPECT_EQ( Vector2D(1,1), s2d.vertices[ 2 ] );
		EXPECT_EQ( Vector2D(0,1), s2d.vertices[ 3 ] );
		EXPECT_DOUBLE_EQ( 0.0, s2d.vl.x );
		EXPECT_DOUBLE_EQ( 0.0, s2d.vl.y );
		EXPECT_DOUBLE_EQ( 1.0, s2d.vu.x );
		EXPECT_DOUBLE_EQ( 1.0, s2d.vu.y );
	}
}
