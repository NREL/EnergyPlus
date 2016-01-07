// EnergyPlus, Copyright (c) 1996-2015, The Board of Trustees of the University of Illinois and
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

// EnergyPlus::PierceSurface Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/PierceSurface.hh>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Vector3.hh>

// C++ Headers
#include <algorithm>
#include <cmath>

using namespace EnergyPlus;
using namespace EnergyPlus::DataSurfaces;
using namespace ObjexxFCL;
using DataVectorTypes::Vector;
using Vector2D = DataSurfaces::Surface2D::Vector2D;

TEST( PierceSurfaceTest, Rectangular )
{
	DataSurfaces::SurfaceData floor;
	floor.Vertex.dimension( 4 );
	floor.Vertex = { Vector(0,0,0), Vector(1,0,0), Vector(1,1,0), Vector(0,1,0) };
	floor.Shape = Rectangle;
	floor.set_computed_geometry();
	DataSurfaces::Surface2D const & floor2d( floor.surface2d );
	EXPECT_EQ( ShapeCat::Rectangular, floor.shapeCat );
	EXPECT_EQ( 2, floor2d.axis );
	EXPECT_EQ( Vector2D(0,0), floor2d.vl );
	EXPECT_EQ( Vector2D(1,1), floor2d.vu );
	EXPECT_DOUBLE_EQ( 1.0, floor2d.s1 );
	EXPECT_DOUBLE_EQ( 1.0, floor2d.s3 );

	{ // Ray straight down into center of floor
		Vector const rayOri( 0.5, 0.5, 1.0 );
		Vector const rayDir( 0.0, 0.0, -1.0 );
		bool hit( false );
		Vector hitPt( 0.0 );
		PierceSurface( floor, rayOri, rayDir, hitPt, hit );
		EXPECT_TRUE( hit );
		EXPECT_DOUBLE_EQ( 0.5, hitPt.x );
		EXPECT_DOUBLE_EQ( 0.5, hitPt.y );
		EXPECT_DOUBLE_EQ( 0.0, hitPt.z );
	}

	{ // Ray straight up away from floor
		Vector const rayOri( 0.5, 0.5, 1.0 );
		Vector const rayDir( 0.0, 0.0, 1.0 );
		bool hit( false );
		Vector hitPt( 0.0 );
		PierceSurface( floor, rayOri, rayDir, hitPt, hit );
		EXPECT_FALSE( hit ); // No plane intersection so hitPt is undefined
	}

	{ // Ray down steep into floor
		Vector const rayOri( 0.5, 0.5, 1.0 );
		Vector const rayDir( Vector( 0.25, 0.0, -1.0 ).normalize() );
		bool hit( false );
		Vector hitPt( 0.0 );
		PierceSurface( floor, rayOri, rayDir, hitPt, hit );
		EXPECT_TRUE( hit );
		EXPECT_DOUBLE_EQ( 0.75, hitPt.x );
		EXPECT_DOUBLE_EQ( 0.5, hitPt.y );
		EXPECT_DOUBLE_EQ( 0.0, hitPt.z );
	}

	{ // Ray down shallow to floor's plane
		Vector const rayOri( 0.5, 0.5, 1.0 );
		Vector const rayDir( Vector( 2.0, 0.0, -1.0 ).normalize() );
		bool hit( false );
		Vector hitPt( 0.0 );
		PierceSurface( floor, rayOri, rayDir, hitPt, hit );
		EXPECT_FALSE( hit ); // Misses surface but intersects plane
		EXPECT_DOUBLE_EQ( 2.5, hitPt.x );
		EXPECT_DOUBLE_EQ( 0.5, hitPt.y );
		EXPECT_DOUBLE_EQ( 0.0, hitPt.z );
	}
}

TEST( PierceSurfaceTest, Triangular )
{
	DataSurfaces::SurfaceData floor;
	floor.Vertex.dimension( 3 );
	floor.Vertex = { Vector(0,0,0), Vector(1,0,0), Vector(1,1,0) };
	floor.Shape = Triangle;
	floor.set_computed_geometry();
	DataSurfaces::Surface2D const & floor2d( floor.surface2d );
	EXPECT_EQ( ShapeCat::Triangular, floor.shapeCat );
	EXPECT_EQ( 2, floor2d.axis );
	EXPECT_EQ( Vector2D(0,0), floor2d.vl );
	EXPECT_EQ( Vector2D(1,1), floor2d.vu );

	{ // Ray straight down into floor
		Vector const rayOri( 0.9, 0.1, 1.0 );
		Vector const rayDir( 0.0, 0.0, -1.0 );
		bool hit( false );
		Vector hitPt( 0.0 );
		PierceSurface( floor, rayOri, rayDir, hitPt, hit );
		EXPECT_TRUE( hit );
		EXPECT_DOUBLE_EQ( 0.9, hitPt.x );
		EXPECT_DOUBLE_EQ( 0.1, hitPt.y );
		EXPECT_DOUBLE_EQ( 0.0, hitPt.z );
	}

	{ // Ray straight up away from floor
		Vector const rayOri( 0.9, 0.1, 1.0 );
		Vector const rayDir( 0.0, 0.0, 1.0 );
		bool hit( false );
		Vector hitPt( 0.0 );
		PierceSurface( floor, rayOri, rayDir, hitPt, hit );
		EXPECT_FALSE( hit ); // No plane intersection so hitPt is undefined
	}

	{ // Ray down steep into floor
		Vector const rayOri( 0.9, 0.1, 1.0 );
		Vector const rayDir( Vector( -0.25, 0.0, -1.0 ).normalize() );
		bool hit( false );
		Vector hitPt( 0.0 );
		PierceSurface( floor, rayOri, rayDir, hitPt, hit );
		EXPECT_TRUE( hit );
		EXPECT_DOUBLE_EQ( 0.65, hitPt.x );
		EXPECT_DOUBLE_EQ( 0.1, hitPt.y );
		EXPECT_DOUBLE_EQ( 0.0, hitPt.z );
	}

	{ // Ray down shallow to floor's plane
		Vector const rayOri( 0.9, 0.1, 1.0 );
		Vector const rayDir( Vector( 2.0, 0.0, -1.0 ).normalize() );
		bool hit( false );
		Vector hitPt( 0.0 );
		PierceSurface( floor, rayOri, rayDir, hitPt, hit );
		EXPECT_FALSE( hit ); // Misses surface but intersects plane
		EXPECT_DOUBLE_EQ( 2.9, hitPt.x );
		EXPECT_DOUBLE_EQ( 0.1, hitPt.y );
		EXPECT_DOUBLE_EQ( 0.0, hitPt.z );
	}
}

TEST( PierceSurfaceTest, Convex )
{
	DataSurfaces::SurfaceData floor;
	floor.Vertex.dimension( 8 );
	floor.Vertex = { Vector(0,0,0), Vector(0.5,-0.25,0), Vector(1,0,0), Vector(1.25,0.5,0), Vector(1,1,0), Vector(0.5,1.25,0), Vector(0,1,0), Vector(-0.25,0.5,0) };
	floor.IsConvex = true;
	floor.set_computed_geometry();
	DataSurfaces::Surface2D const & floor2d( floor.surface2d );
	EXPECT_EQ( ShapeCat::Convex, floor.shapeCat );
	EXPECT_EQ( 2, floor2d.axis );
	EXPECT_EQ( Vector2D(-0.25,-0.25), floor2d.vl );
	EXPECT_EQ( Vector2D(1.25,1.25), floor2d.vu );

	{ // Ray straight down into center of floor
		Vector const rayOri( 0.5, 0.5, 1.0 );
		Vector const rayDir( 0.0, 0.0, -1.0 );
		bool hit( false );
		Vector hitPt( 0.0 );
		PierceSurface( floor, rayOri, rayDir, hitPt, hit );
		EXPECT_TRUE( hit );
		EXPECT_DOUBLE_EQ( 0.5, hitPt.x );
		EXPECT_DOUBLE_EQ( 0.5, hitPt.y );
		EXPECT_DOUBLE_EQ( 0.0, hitPt.z );
	}

	{ // Ray straight up away from floor
		Vector const rayOri( 0.5, 0.5, 1.0 );
		Vector const rayDir( 0.0, 0.0, 1.0 );
		bool hit( false );
		Vector hitPt( 0.0 );
		PierceSurface( floor, rayOri, rayDir, hitPt, hit );
		EXPECT_FALSE( hit ); // No plane intersection so hitPt is undefined
	}

	{ // Ray down steep into floor
		Vector const rayOri( 0.5, 0.5, 1.0 );
		Vector const rayDir( Vector( 0.25, 0.0, -1.0 ).normalize() );
		bool hit( false );
		Vector hitPt( 0.0 );
		PierceSurface( floor, rayOri, rayDir, hitPt, hit );
		EXPECT_TRUE( hit );
		EXPECT_DOUBLE_EQ( 0.75, hitPt.x );
		EXPECT_DOUBLE_EQ( 0.5, hitPt.y );
		EXPECT_DOUBLE_EQ( 0.0, hitPt.z );
	}

	{ // Ray down shallow to floor's plane
		Vector const rayOri( 0.5, 0.5, 1.0 );
		Vector const rayDir( Vector( 2.0, 0.0, -1.0 ).normalize() );
		bool hit( false );
		Vector hitPt( 0.0 );
		PierceSurface( floor, rayOri, rayDir, hitPt, hit );
		EXPECT_FALSE( hit ); // Misses surface but intersects plane
		EXPECT_DOUBLE_EQ( 2.5, hitPt.x );
		EXPECT_DOUBLE_EQ( 0.5, hitPt.y );
		EXPECT_DOUBLE_EQ( 0.0, hitPt.z );
	}
}

TEST( PierceSurfaceTest, Nonconvex )
{
	DataSurfaces::SurfaceData boomerang;
	boomerang.Vertex.dimension( 4 );
	boomerang.Vertex = { Vector(0,0,0), Vector(1,0,0), Vector(0.2,0.2,0), Vector(0,1,0) }; // Nonconvex "pointy boomerang"
	boomerang.IsConvex = false;
	boomerang.set_computed_geometry();
	DataSurfaces::Surface2D const & floor2d( boomerang.surface2d );
	EXPECT_EQ( ShapeCat::Nonconvex, boomerang.shapeCat );
	EXPECT_EQ( 2, floor2d.axis );

	{ // Ray straight down into a "wing" of boomerang
		Vector const rayOri( 0.3, 0.1, 1.0 );
		Vector const rayDir( 0.0, 0.0, -1.0 );
		bool hit( false );
		Vector hitPt( 0.0 );
		PierceSurface( boomerang, rayOri, rayDir, hitPt, hit );
		EXPECT_FALSE( hit ); // Should be true with correct nonconvex algorithm
		EXPECT_DOUBLE_EQ( 0.3, hitPt.x );
		EXPECT_DOUBLE_EQ( 0.1, hitPt.y );
		EXPECT_DOUBLE_EQ( 0.0, hitPt.z );
	}
}
