// EnergyPlus::OutputReportTabular Unit Tests

// Google Test Headers
#include <gtest/gtest.h>
// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
// EnergyPlus Headers
#include <EnergyPlus/SurfaceGeometry.hh>
#include <EnergyPlus/UtilityRoutines.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::SurfaceGeometry;
using namespace ObjexxFCL;

TEST( SurfaceGeometryTest, ConfirmIsAzimuthDifferent )
{
	ShowMessage( "Begin Test: SurfaceGeometryTest, ConfirmIsAzimuthDifferent" );

	EXPECT_TRUE( isAzimuthDifferent( 0., 180. ) );
	EXPECT_TRUE( isAzimuthDifferent( 180., 0. ) );
	EXPECT_TRUE( isAzimuthDifferent( 0., 10.1 ) );
	EXPECT_FALSE( isAzimuthDifferent( 0., 0. ) );
	EXPECT_FALSE( isAzimuthDifferent( 0., 9.9 ) );
}

TEST( SurfaceGeometryTest, ConfirmIsTiltDifferent )
{
	ShowMessage( "Begin Test: SurfaceGeometryTest, ConfirmIsTiltDifferent" );

	EXPECT_TRUE( isTiltDifferent( 0., 180. ) );
	EXPECT_TRUE( isTiltDifferent( 180., 0. ) );
	EXPECT_FALSE( isTiltDifferent( 0., 0. ) );
	EXPECT_FALSE( isTiltDifferent( 0., 9.9 ) );
}


TEST( SurfaceGeometryTest, ConfirmCheckSurfAzTiltNorm )
{
	ShowMessage( "Begin Test: SurfaceGeometryTest, ConfirmCheckSurfAzTiltNorm" );

	SurfaceData a;
	SurfaceData b;
	bool azErr;
	bool tiltErr;
	bool sameSurfNorm;

	azErr = false;
	tiltErr = false;
	sameSurfNorm = false;
	a.Azimuth = 0.;
	a.Tilt = 0.;
	a.NewellSurfaceNormalVector.x = 0.;
	a.NewellSurfaceNormalVector.y = 0.;
	a.NewellSurfaceNormalVector.z = 1.;

	b.Azimuth = 0.;
	b.Tilt = 0.;
	b.NewellSurfaceNormalVector.x = 0.;
	b.NewellSurfaceNormalVector.y = 0.;
	b.NewellSurfaceNormalVector.z = 1.;
	checkSurfAzTiltNorm( a, b, azErr, tiltErr, sameSurfNorm );
	EXPECT_FALSE( azErr );
	EXPECT_FALSE( tiltErr );
	EXPECT_TRUE( sameSurfNorm );

	b.Azimuth = 180.;
	b.Tilt = 180.;
	b.NewellSurfaceNormalVector.x = 1.;
	b.NewellSurfaceNormalVector.y = 0.;
	b.NewellSurfaceNormalVector.z = 0.;
	checkSurfAzTiltNorm( a, b, azErr, tiltErr, sameSurfNorm );
	EXPECT_TRUE( azErr );
	EXPECT_TRUE( tiltErr );
	EXPECT_FALSE( sameSurfNorm );

}
