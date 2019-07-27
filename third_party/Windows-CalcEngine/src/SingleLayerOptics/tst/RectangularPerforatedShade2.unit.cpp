#include <memory>
#include <gtest/gtest.h>

#include "WCESingleLayerOptics.hpp"
#include "WCECommon.hpp"


using namespace SingleLayerOptics;
using namespace FenestrationCommon;

class TestRectangularPerforatedShade2 : public testing::Test {

private:
	std::shared_ptr< CBSDFLayer > m_Shade;

protected:
	virtual void SetUp() {
		// make material
		double Tmat = 0.1;
		double Rfmat = 0.5;
		double Rbmat = 0.6;
		double minLambda = 0.3;
		double maxLambda = 2.5;
		std::shared_ptr< CMaterial > aMaterial =
			std::make_shared< CMaterialSingleBand >( Tmat, Tmat, Rfmat, Rbmat, minLambda, maxLambda );

		// make cell geometry
		double x = 20; // mm
		double y = 25; // mm
		double thickness = 7; // mm
		double xHole = 5; // mm
		double yHole = 8; // mm
		std::shared_ptr< ICellDescription > aCellDescription =
			std::make_shared< CRectangularCellDescription >( x, y, thickness, xHole, yHole );

		std::shared_ptr< CBSDFHemisphere > aBSDF = std::make_shared< CBSDFHemisphere >( BSDFBasis::Quarter );

		std::shared_ptr< CUniformDiffuseCell > aCell = std::make_shared< CPerforatedCell >( aMaterial, aCellDescription );

		m_Shade = std::make_shared< CUniformDiffuseBSDFLayer >( aCell, aBSDF );

	}

public:
	std::shared_ptr< CBSDFLayer > GetShade() {
		return m_Shade;
	};

};

TEST_F( TestRectangularPerforatedShade2, TestSolarProperties ) {
	SCOPED_TRACE( "Begin Test: Rectangular perforated cell - Solar properties." );

	std::shared_ptr< CBSDFLayer > aShade = GetShade();

	std::shared_ptr< CBSDFIntegrator > aResults = aShade->getResults();

	double tauDiff = aResults->DiffDiff( Side::Front, PropertySimple::T );
	EXPECT_NEAR( 0.112843786, tauDiff, 1e-6 );

	double RfDiff = aResults->DiffDiff( Side::Front, PropertySimple::R );
	EXPECT_NEAR( 0.492864523, RfDiff, 1e-6 );

	double RbDiff = aResults->DiffDiff( Side::Back, PropertySimple::R );
	EXPECT_NEAR( 0.591437306, RbDiff, 1e-6 );

	std::shared_ptr< CSquareMatrix > aT = aResults->getMatrix( Side::Front, PropertySimple::T );

	size_t size = aT->getSize();

	// Test diagonal
	std::vector< double > correctResults;
	correctResults.push_back( 1.069864 );
	correctResults.push_back( 0.641828 );
	correctResults.push_back( 0.638318 );
	correctResults.push_back( 0.832716 );
	correctResults.push_back( 0.638318 );
	correctResults.push_back( 0.641828 );
	correctResults.push_back( 0.638318 );
	correctResults.push_back( 0.832716 );
	correctResults.push_back( 0.638318 );
	correctResults.push_back( 0.031831 );
	correctResults.push_back( 0.116107 );
	correctResults.push_back( 0.260917 );
	correctResults.push_back( 0.409662 );
	correctResults.push_back( 0.260917 );
	correctResults.push_back( 0.116107 );
	correctResults.push_back( 0.031831 );
	correctResults.push_back( 0.116107 );
	correctResults.push_back( 0.260917 );
	correctResults.push_back( 0.409662 );
	correctResults.push_back( 0.260917 );
	correctResults.push_back( 0.116107 );
	correctResults.push_back( 0.031831 );
	correctResults.push_back( 0.031831 );
	correctResults.push_back( 0.031831 );
	correctResults.push_back( 0.031831 );
	correctResults.push_back( 0.031831 );
	correctResults.push_back( 0.031831 );
	correctResults.push_back( 0.031831 );
	correctResults.push_back( 0.031831 );
	correctResults.push_back( 0.031831 );
	correctResults.push_back( 0.031831 );
	correctResults.push_back( 0.031831 );
	correctResults.push_back( 0.031831 );
	correctResults.push_back( 0.031831 );
	correctResults.push_back( 0.031831 );
	correctResults.push_back( 0.031831 );
	correctResults.push_back( 0.031831 );
	correctResults.push_back( 0.031831 );
	correctResults.push_back( 0.031831 );
	correctResults.push_back( 0.031831 );
	correctResults.push_back( 0.031831 );

	std::vector< double > calculatedResults;
	for ( size_t i = 0; i < size; ++i ) {
		calculatedResults.push_back( ( *aT )[ i ][ i ] );
	}

	EXPECT_EQ( correctResults.size(), calculatedResults.size() );
	for ( size_t i = 0; i < size; ++i ) {
		EXPECT_NEAR( correctResults[i], calculatedResults[i], 1e-5 );
	}

	// Test first row
	correctResults.clear();
	correctResults.push_back( 1.069864 );
	correctResults.push_back( 0.030443 );
	correctResults.push_back( 0.030451 );
	correctResults.push_back( 0.030008 );
	correctResults.push_back( 0.030451 );
	correctResults.push_back( 0.030443 );
	correctResults.push_back( 0.030451 );
	correctResults.push_back( 0.030008 );
	correctResults.push_back( 0.030451 );
	correctResults.push_back( 0.031831 );
	correctResults.push_back( 0.031624 );
	correctResults.push_back( 0.031269 );
	correctResults.push_back( 0.030903 );
	correctResults.push_back( 0.031269 );
	correctResults.push_back( 0.031624 );
	correctResults.push_back( 0.031831 );
	correctResults.push_back( 0.031624 );
	correctResults.push_back( 0.031269 );
	correctResults.push_back( 0.030903 );
	correctResults.push_back( 0.031269 );
	correctResults.push_back( 0.031624 );
	correctResults.push_back( 0.031831 );
	correctResults.push_back( 0.031831 );
	correctResults.push_back( 0.031831 );
	correctResults.push_back( 0.031831 );
	correctResults.push_back( 0.031831 );
	correctResults.push_back( 0.031831 );
	correctResults.push_back( 0.031831 );
	correctResults.push_back( 0.031831 );
	correctResults.push_back( 0.031831 );
	correctResults.push_back( 0.031831 );
	correctResults.push_back( 0.031831 );
	correctResults.push_back( 0.031831 );
	correctResults.push_back( 0.031831 );
	correctResults.push_back( 0.031831 );
	correctResults.push_back( 0.031831 );
	correctResults.push_back( 0.031831 );
	correctResults.push_back( 0.031831 );
	correctResults.push_back( 0.031831 );
	correctResults.push_back( 0.031831 );
	correctResults.push_back( 0.031831 );

	calculatedResults.clear();
	for ( size_t i = 0; i < size; ++i ) {
		calculatedResults.push_back( ( *aT )[ 0 ][ i ] );
	}

	EXPECT_EQ( correctResults.size(), calculatedResults.size() );
	for ( size_t i = 0; i < size; ++i ) {
		EXPECT_NEAR( correctResults[i], calculatedResults[i], 1e-5 );
	}

	// Test first row for reflectance matrix
	std::shared_ptr< CSquareMatrix > aRf = aResults->getMatrix( Side::Front, PropertySimple::R );

	correctResults.clear();
	correctResults.push_back( 0.146423 );
	correctResults.push_back( 0.152214 );
	correctResults.push_back( 0.152254 );
	correctResults.push_back( 0.150042 );
	correctResults.push_back( 0.152254 );
	correctResults.push_back( 0.152214 );
	correctResults.push_back( 0.152254 );
	correctResults.push_back( 0.150042 );
	correctResults.push_back( 0.152254 );
	correctResults.push_back( 0.159155 );
	correctResults.push_back( 0.158120 );
	correctResults.push_back( 0.156343 );
	correctResults.push_back( 0.154517 );
	correctResults.push_back( 0.156343 );
	correctResults.push_back( 0.158120 );
	correctResults.push_back( 0.159155 );
	correctResults.push_back( 0.158120 );
	correctResults.push_back( 0.156343 );
	correctResults.push_back( 0.154517 );
	correctResults.push_back( 0.156343 );
	correctResults.push_back( 0.158120 );
	correctResults.push_back( 0.159155 );
	correctResults.push_back( 0.159155 );
	correctResults.push_back( 0.159155 );
	correctResults.push_back( 0.159155 );
	correctResults.push_back( 0.159155 );
	correctResults.push_back( 0.159155 );
	correctResults.push_back( 0.159155 );
	correctResults.push_back( 0.159155 );
	correctResults.push_back( 0.159155 );
	correctResults.push_back( 0.159155 );
	correctResults.push_back( 0.159155 );
	correctResults.push_back( 0.159155 );
	correctResults.push_back( 0.159155 );
	correctResults.push_back( 0.159155 );
	correctResults.push_back( 0.159155 );
	correctResults.push_back( 0.159155 );
	correctResults.push_back( 0.159155 );
	correctResults.push_back( 0.159155 );
	correctResults.push_back( 0.159155 );
	correctResults.push_back( 0.159155 );

	calculatedResults.clear();
	for ( size_t i = 0; i < size; ++i ) {
		calculatedResults.push_back( ( *aRf )[ 0 ][ i ] );
	}

	EXPECT_EQ( correctResults.size(), calculatedResults.size() );
	for ( size_t i = 0; i < size; ++i ) {
		EXPECT_NEAR( correctResults[i], calculatedResults[i], 1e-5 );
	}

}
