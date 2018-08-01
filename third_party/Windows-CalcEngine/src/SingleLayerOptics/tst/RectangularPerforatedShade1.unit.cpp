#include <memory>
#include <gtest/gtest.h>

#include "WCESingleLayerOptics.hpp"
#include "WCECommon.hpp"


using namespace SingleLayerOptics;
using namespace FenestrationCommon;

class TestRectangularPerforatedShade1 : public testing::Test {

private:
	std::shared_ptr< CBSDFLayer > m_Shade;

protected:
	virtual void SetUp() {
		// create material
		double Tmat = 0.0;
		double Rfmat = 0.7;
		double Rbmat = 0.7;
		double minLambda = 0.3;
		double maxLambda = 2.5;
		std::shared_ptr< CMaterial > aMaterial =
			std::make_shared< CMaterialSingleBand >( Tmat, Tmat, Rfmat, Rbmat, minLambda, maxLambda );

		// make cell geometry
		double x = 19.05; // mm
		double y = 19.05; // mm
		double thickness = 0.6; // mm
		double xHole = 3.175; // mm
		double yHole = 6.35; // mm
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

TEST_F( TestRectangularPerforatedShade1, TestSolarProperties ) {
	SCOPED_TRACE( "Begin Test: Rectangular perforated cell - Solar properties." );

	std::shared_ptr< CBSDFLayer > aShade = GetShade();

	std::shared_ptr< CBSDFIntegrator > aResults = aShade->getResults();

	double tauDiff = aResults->DiffDiff( Side::Front, PropertySimple::T );
	EXPECT_NEAR( 0.041876313, tauDiff, 1e-6 );

	double RfDiff = aResults->DiffDiff( Side::Front, PropertySimple::R );
	EXPECT_NEAR( 0.670686365, RfDiff, 1e-6 );

	std::shared_ptr< CSquareMatrix > aT = aResults->getMatrix( Side::Front, PropertySimple::T );

	// Test only diagonal of transmittance matrix
	size_t size = aT->getSize();

	std::vector< double > correctResults;
	correctResults.push_back( 0.722625 );
	correctResults.push_back( 0.731048 );
	correctResults.push_back( 0.728881 );
	correctResults.push_back( 0.754961 );
	correctResults.push_back( 0.728881 );
	correctResults.push_back( 0.731048 );
	correctResults.push_back( 0.728881 );
	correctResults.push_back( 0.754961 );
	correctResults.push_back( 0.728881 );
	correctResults.push_back( 0.622917 );
	correctResults.push_back( 0.614362 );
	correctResults.push_back( 0.632505 );
	correctResults.push_back( 0.672486 );
	correctResults.push_back( 0.632505 );
	correctResults.push_back( 0.614362 );
	correctResults.push_back( 0.622917 );
	correctResults.push_back( 0.614362 );
	correctResults.push_back( 0.632505 );
	correctResults.push_back( 0.672486 );
	correctResults.push_back( 0.632505 );
	correctResults.push_back( 0.614362 );
	correctResults.push_back( 0.534246 );
	correctResults.push_back( 0.523031 );
	correctResults.push_back( 0.557403 );
	correctResults.push_back( 0.628150 );
	correctResults.push_back( 0.557403 );
	correctResults.push_back( 0.523031 );
	correctResults.push_back( 0.534246 );
	correctResults.push_back( 0.523031 );
	correctResults.push_back( 0.557403 );
	correctResults.push_back( 0.628150 );
	correctResults.push_back( 0.557403 );
	correctResults.push_back( 0.523031 );
	correctResults.push_back( 0.146104 );
	correctResults.push_back( 0.219651 );
	correctResults.push_back( 0.416249 );
	correctResults.push_back( 0.219651 );
	correctResults.push_back( 0.146104 );
	correctResults.push_back( 0.219651 );
	correctResults.push_back( 0.416249 );
	correctResults.push_back( 0.219651 );

	std::vector< double > calculatedResults;
	for ( size_t i = 0; i < size; ++i ) {
		calculatedResults.push_back( ( *aT )[ i ][ i ] );
	}

	EXPECT_EQ( correctResults.size(), calculatedResults.size() );
	for ( size_t i = 0; i < size; ++i ) {
		EXPECT_NEAR( correctResults[i], calculatedResults[i], 1e-5 );
	}

	std::shared_ptr< CSquareMatrix > aRf = aResults->getMatrix( Side::Front, PropertySimple::R );

	correctResults.clear();
	correctResults.push_back( 0.210438 );
	correctResults.push_back( 0.211198 );
	correctResults.push_back( 0.211233 );
	correctResults.push_back( 0.210818 );
	correctResults.push_back( 0.211233 );
	correctResults.push_back( 0.211198 );
	correctResults.push_back( 0.211233 );
	correctResults.push_back( 0.210818 );
	correctResults.push_back( 0.211233 );
	correctResults.push_back( 0.212138 );
	correctResults.push_back( 0.212284 );
	correctResults.push_back( 0.211973 );
	correctResults.push_back( 0.211288 );
	correctResults.push_back( 0.211973 );
	correctResults.push_back( 0.212284 );
	correctResults.push_back( 0.212138 );
	correctResults.push_back( 0.212284 );
	correctResults.push_back( 0.211973 );
	correctResults.push_back( 0.211288 );
	correctResults.push_back( 0.211973 );
	correctResults.push_back( 0.212284 );
	correctResults.push_back( 0.213658 );
	correctResults.push_back( 0.213850 );
	correctResults.push_back( 0.213261 );
	correctResults.push_back( 0.212048 );
	correctResults.push_back( 0.213261 );
	correctResults.push_back( 0.213850 );
	correctResults.push_back( 0.213658 );
	correctResults.push_back( 0.213850 );
	correctResults.push_back( 0.213261 );
	correctResults.push_back( 0.212048 );
	correctResults.push_back( 0.213261 );
	correctResults.push_back( 0.213850 );
	correctResults.push_back( 0.220182 );
	correctResults.push_back( 0.218856 );
	correctResults.push_back( 0.215310 );
	correctResults.push_back( 0.218856 );
	correctResults.push_back( 0.220182 );
	correctResults.push_back( 0.218856 );
	correctResults.push_back( 0.215310 );
	correctResults.push_back( 0.218856 );

	calculatedResults.clear();
	for ( size_t i = 0; i < size; ++i ) {
		calculatedResults.push_back( ( *aRf )[ i ][ i ] );
	}

	EXPECT_EQ( correctResults.size(), calculatedResults.size() );
	for ( size_t i = 0; i < size; ++i ) {
		EXPECT_NEAR( correctResults[i], calculatedResults[i], 1e-5 );
	}

}
