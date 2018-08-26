#include <memory>
#include <gtest/gtest.h>

#include <memory>

#include "WCESingleLayerOptics.hpp"


using namespace SingleLayerOptics;

class TestBSDFQuarterBasis : public testing::Test {

private:
	std::shared_ptr< CBSDFHemisphere > m_BSDFHemisphere;

protected:
	virtual void SetUp() {
		m_BSDFHemisphere = std::make_shared< CBSDFHemisphere >( BSDFBasis::Quarter );
	}

public:
	std::shared_ptr< const CBSDFDirections > GetDirections( const BSDFHemisphere t_Side ) {
		return m_BSDFHemisphere->getDirections( t_Side );
	};

};

TEST_F( TestBSDFQuarterBasis, TestQuarterBasisPhis ) {
	SCOPED_TRACE( "Begin Test: Phi angles for patches." );

	CBSDFDirections aDirections = *GetDirections( BSDFHemisphere::Incoming );

	std::vector< double > correctResults = { 180,
		0, 45, 90, 135, 180, 225, 270, 315,
		0, 30, 60, 90, 120, 150, 180, 210, 240, 270, 300, 330,
		0, 30, 60, 90, 120, 150, 180, 210, 240, 270, 300, 330,
		0, 45, 90, 135, 180, 225, 270, 315 };

	size_t correctSize = 41;

	EXPECT_EQ( correctSize, aDirections.size() );

	std::vector< double > phiAngles;
	for ( std::shared_ptr< const CBSDFPatch > aPatch : aDirections ) {
		phiAngles.push_back( aPatch->centerPoint()->phi() );
	}

	for ( size_t i = 0; i < phiAngles.size(); ++i ) {
		EXPECT_NEAR( phiAngles[i], correctResults[i], 1e-6 );
	}

}

TEST_F( TestBSDFQuarterBasis, TestQuarterBasisThetas ) {
	SCOPED_TRACE( "Begin Test: Theta angles for patches." );

	CBSDFDirections aDirections = *GetDirections( BSDFHemisphere::Incoming );

	std::vector< double > correctResults = { 0,
		18, 18, 18, 18, 18, 18, 18, 18,
		36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36,
		54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54,
		76.5, 76.5, 76.5, 76.5, 76.5, 76.5, 76.5, 76.5 };

	size_t correctSize = 41;

	EXPECT_EQ( correctSize, aDirections.size() );

	std::vector< double > thetaAngles;
	std::vector< std::shared_ptr< CBSDFPatch > >::iterator it;
	for ( it = aDirections.begin(); it < aDirections.end(); ++it ) {
		thetaAngles.push_back( ( *it )->centerPoint()->theta() );
	}

	for ( size_t i = 0; i < thetaAngles.size(); ++i ) {
		EXPECT_NEAR( thetaAngles[i], correctResults[i], 1e-6 );
	}

}

TEST_F( TestBSDFQuarterBasis, TestQuarterBasisLambdas ) {
	SCOPED_TRACE( "Begin Test: Theta angles for patches." );

	CBSDFDirections aDirections = *GetDirections( BSDFHemisphere::Incoming );

	std::vector< double > correctResults =
	{ 0.076880244,
		0.071328146, 0.071328146, 0.071328146, 0.071328146, 0.071328146, 0.071328146, 0.071328146, 0.071328146,
		0.076940910, 0.076940910, 0.076940910, 0.076940910, 0.076940910, 0.076940910, 0.076940910, 0.076940910, 0.07694091, 0.07694091, 0.07694091, 0.07694091,
		0.076940910, 0.076940910, 0.076940910, 0.076940910, 0.076940910, 0.076940910, 0.076940910, 0.076940910, 0.07694091, 0.07694091, 0.07694091, 0.07694091,
		0.080938176, 0.080938176, 0.080938176, 0.080938176, 0.080938176, 0.080938176, 0.080938176, 0.080938176 };

	size_t correctSize = 41;

	EXPECT_EQ( correctSize, aDirections.size() );

	std::vector< double > lambdaValues;
	std::vector< std::shared_ptr< CBSDFPatch > >::iterator it;
	for ( it = aDirections.begin(); it < aDirections.end(); ++it ) {
		lambdaValues.push_back( ( *it )->lambda() );
	}

	for ( size_t i = 0; i < lambdaValues.size(); ++i ) {
		EXPECT_NEAR( lambdaValues[i], correctResults[i], 1e-6 );
	}

}
