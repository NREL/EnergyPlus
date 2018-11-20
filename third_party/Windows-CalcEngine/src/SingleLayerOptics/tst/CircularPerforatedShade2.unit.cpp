#include <memory>
#include <gtest/gtest.h>

#include "WCESingleLayerOptics.hpp"
#include "WCECommon.hpp"


using namespace SingleLayerOptics;
using namespace FenestrationCommon;

class TestCircularPerforatedShade2 : public testing::Test {

private:
	std::shared_ptr< CBSDFLayer > m_Shade;

protected:
	virtual void SetUp() {
		// create material
		double Tmat = 0.2;
		double Rfmat = 0.8;
		double Rbmat = 0.8;
		double minLambda = 0.3;
		double maxLambda = 2.5;
		std::shared_ptr< CMaterial > aMaterial =
			std::make_shared< CMaterialSingleBand >( Tmat, Tmat, Rfmat, Rbmat, minLambda, maxLambda );

		// make cell geometry
		double x = 22.5; // mm
		double y = 38.1; // mm
		double thickness = 5; // mm
		double radius = 0; // mm
		std::shared_ptr< ICellDescription > aCellDescription =
			std::make_shared< CCircularCellDescription >( x, y, thickness, radius );

		std::shared_ptr< CBSDFHemisphere > aBSDF = std::make_shared< CBSDFHemisphere >( BSDFBasis::Quarter );

		// make layer
		CBSDFLayerMaker aMaker = CBSDFLayerMaker( aMaterial, aBSDF, aCellDescription );
		m_Shade = aMaker.getLayer();

	}

public:
	std::shared_ptr< CBSDFLayer > GetShade() {
		return m_Shade;
	};

};

TEST_F( TestCircularPerforatedShade2, TestSolarProperties ) {
	SCOPED_TRACE( "Begin Test: Circular perforated cell - Solar properties." );

	std::shared_ptr< CBSDFLayer > aShade = GetShade();

	std::shared_ptr< CBSDFIntegrator > aResults = aShade->getResults();

	double tauDiff = aResults->DiffDiff( Side::Front, PropertySimple::T );
	EXPECT_NEAR( 0.2, tauDiff, 1e-6 );

	double RfDiff = aResults->DiffDiff( Side::Front, PropertySimple::R );
	EXPECT_NEAR( 0.8, RfDiff, 1e-6 );

	double RbDiff = aResults->DiffDiff( Side::Back, PropertySimple::R );
	EXPECT_NEAR( 0.8, RbDiff, 1e-6 );

	std::shared_ptr< CSquareMatrix > aT = aResults->getMatrix( Side::Front, PropertySimple::T );

	size_t size = aT->getSize();

	// Test diagonal
	std::vector< double > correctResults;
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );

	std::vector< double > calculatedResults;
	for ( size_t i = 0; i < size; ++i ) {
		calculatedResults.push_back( ( *aT )[ i ][ i ] );
	}

	EXPECT_EQ( correctResults.size(), calculatedResults.size() );
	for ( size_t i = 0; i < size; ++i ) {
		EXPECT_NEAR( correctResults[ i ], calculatedResults[ i ], 1e-5 );
	}

	// Test first row
	correctResults.clear();
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );
	correctResults.push_back( 0.063662 );

	calculatedResults.clear();
	for ( size_t i = 0; i < size; ++i ) {
		calculatedResults.push_back( ( *aT )[ 0 ][ i ] );
	}

	EXPECT_EQ( correctResults.size(), calculatedResults.size() );
	for ( size_t i = 0; i < size; ++i ) {
		EXPECT_NEAR( correctResults[ i ], calculatedResults[ i ], 1e-5 );
	}

	// Test first row for reflectance matrix
	std::shared_ptr< CSquareMatrix > aRf = aResults->getMatrix( Side::Front, PropertySimple::R );

	correctResults.clear();
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );

	calculatedResults.clear();
	for ( size_t i = 0; i < size; ++i ) {
		calculatedResults.push_back( ( *aRf )[ 0 ][ i ] );
	}

	EXPECT_EQ( correctResults.size(), calculatedResults.size() );
	for ( size_t i = 0; i < size; ++i ) {
		EXPECT_NEAR( correctResults[ i ], calculatedResults[ i ], 1e-5 );
	}

	// Test first row for reflectance matrix
	std::shared_ptr< CSquareMatrix > aRb = aResults->getMatrix( Side::Back, PropertySimple::R );

	correctResults.clear();
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );
	correctResults.push_back( 0.254648 );

	calculatedResults.clear();
	for ( size_t i = 0; i < size; ++i ) {
		calculatedResults.push_back( ( *aRb )[ 0 ][ i ] );
	}

	EXPECT_EQ( correctResults.size(), calculatedResults.size() );
	for ( size_t i = 0; i < size; ++i ) {
		EXPECT_NEAR( correctResults[ i ], calculatedResults[ i ], 1e-5 );
	}

}
