#include <memory>
#include <gtest/gtest.h>

#include "WCESingleLayerOptics.hpp"
#include "WCECommon.hpp"


using namespace SingleLayerOptics;
using namespace FenestrationCommon;

class TestVenetianScatteringLayer1 : public testing::Test {

private:
	std::shared_ptr< CScatteringLayer > m_Shade;

protected:
	virtual void SetUp() {
		// create material
		double Tmat = 0.1;
		double Rfmat = 0.7;
		double Rbmat = 0.7;
		double minLambda = 0.3;
		double maxLambda = 2.5;
		std::shared_ptr< CMaterial > aMaterial =
			std::make_shared< CMaterialSingleBand >( Tmat, Tmat, Rfmat, Rbmat, minLambda, maxLambda );

		// make cell geometry
		double slatWidth = 0.010; // m
		double slatSpacing = 0.010; // m
		double slatTiltAngle = 45;
		double curvatureRadius = 0;
		size_t numOfSlatSegments = 1;

		std::shared_ptr< ICellDescription > aCellDescription =
			std::make_shared< CVenetianCellDescription >( slatWidth, slatSpacing, slatTiltAngle,
			                                         curvatureRadius, numOfSlatSegments );

		// Method
		DistributionMethod aDistribution = DistributionMethod::UniformDiffuse;

		m_Shade = std::make_shared< CScatteringLayer >( aMaterial, aCellDescription, aDistribution );

	}

public:
	std::shared_ptr< CScatteringLayer > GetShade() {
		return m_Shade;
	};

};

TEST_F( TestVenetianScatteringLayer1, TestVenetian1 ) {
	SCOPED_TRACE( "Begin Test: Venetian scattering layer (Flat, 45 degrees slats) - 0 deg incident." );

	std::shared_ptr< CScatteringLayer > aShade = GetShade();

	Side aSide = Side::Front;

	double T_dir_dir = aShade->getPropertySimple( PropertySimple::T, aSide, Scattering::DirectDirect );
	EXPECT_NEAR( 0.292893, T_dir_dir, 1e-6 );

	double R_dir_dir = aShade->getPropertySimple( PropertySimple::R, aSide, Scattering::DirectDirect );
	EXPECT_NEAR( 0, R_dir_dir, 1e-6 );

	double T_dir_dif = aShade->getPropertySimple( PropertySimple::T, aSide, Scattering::DirectDiffuse );
	EXPECT_NEAR( 0.162897, T_dir_dif, 1e-6 );

	double R_dir_dif = aShade->getPropertySimple( PropertySimple::R, aSide, Scattering::DirectDiffuse );
	EXPECT_NEAR( 0.356835, R_dir_dif, 1e-6 );

	double T_dif_dif = aShade->getPropertySimple( PropertySimple::T, aSide, Scattering::DiffuseDiffuse );
	EXPECT_NEAR( 0.486233, T_dif_dif, 1e-6 );

	double R_dif_dif = aShade->getPropertySimple( PropertySimple::R, aSide, Scattering::DiffuseDiffuse );
	EXPECT_NEAR( 0.329593, R_dif_dif, 1e-6 );

	double A_dir = aShade->getAbsorptance( aSide, ScatteringSimple::Direct );
	EXPECT_NEAR( 0.187375, A_dir, 1e-6 );

	double A_dif = aShade->getAbsorptance( aSide, ScatteringSimple::Diffuse );
	EXPECT_NEAR( 0.184173, A_dif, 1e-6 );

}

TEST_F( TestVenetianScatteringLayer1, TestVenetian2 ) {
	SCOPED_TRACE( "Begin Test: Venetian scattering layer (Flat, 45 degrees slats) - Theta = 45 deg,"
		" Phi = 45 incident." );

	std::shared_ptr< CScatteringLayer > aShade = GetShade();

	Side aSide = Side::Front;
	const double Theta = 45;
	const double Phi = 90;

	double T_dir_dir = aShade->getPropertySimple( PropertySimple::T, aSide,
	                                              Scattering::DirectDirect, Theta, Phi );
	EXPECT_NEAR( 1, T_dir_dir, 1e-6 );

	double R_dir_dir = aShade->getPropertySimple( PropertySimple::R, aSide,
	                                              Scattering::DirectDirect, Theta, Phi );
	EXPECT_NEAR( 0, R_dir_dir, 1e-6 );

	double T_dir_dif = aShade->getPropertySimple( PropertySimple::T, aSide,
	                                              Scattering::DirectDiffuse, Theta, Phi );
	EXPECT_NEAR( 0, T_dir_dif, 1e-6 );

	double R_dir_dif = aShade->getPropertySimple( PropertySimple::R, aSide,
	                                              Scattering::DirectDiffuse, Theta, Phi );
	EXPECT_NEAR( 0, R_dir_dif, 1e-6 );

	double T_dif_dif = aShade->getPropertySimple( PropertySimple::T, aSide,
	                                              Scattering::DiffuseDiffuse, Theta, Phi );
	EXPECT_NEAR( 0.486233, T_dif_dif, 1e-6 );

	double R_dif_dif = aShade->getPropertySimple( PropertySimple::R, aSide,
	                                              Scattering::DiffuseDiffuse, Theta, Phi );
	EXPECT_NEAR( 0.329593, R_dif_dif, 1e-6 );

	double A_dir = aShade->getAbsorptance( aSide, ScatteringSimple::Direct, Theta, Phi );
	EXPECT_NEAR( 0, A_dir, 1e-6 );

	double A_dif = aShade->getAbsorptance( aSide, ScatteringSimple::Diffuse, Theta, Phi );
	EXPECT_NEAR( 0.184173, A_dif, 1e-6 );

}
