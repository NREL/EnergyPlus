#include <memory>
#include <gtest/gtest.h>

#include "WCESingleLayerOptics.hpp"
#include "WCECommon.hpp"


using namespace SingleLayerOptics;
using namespace FenestrationCommon;

class TestPerfectDiffuseShade1 : public testing::Test {

private:
	std::shared_ptr< CBSDFLayer > m_Shade;

protected:
	virtual void SetUp() {
		// create material
		double Tmat = 0.00;
		double Rfmat = 0.55;
		double Rbmat = 0.55;
		double minLambda = 0.3;
		double maxLambda = 2.5;
		std::shared_ptr< CMaterial > aMaterial =
			std::make_shared< CMaterialSingleBand >( Tmat, Tmat, Rfmat, Rbmat, minLambda, maxLambda );

		// make cell
		std::shared_ptr< ICellDescription > aCellDescription =
			std::make_shared< CPerfectDiffuseCellDescription >();

		std::shared_ptr< CBSDFHemisphere > aBSDF = std::make_shared< CBSDFHemisphere >( BSDFBasis::Quarter );

		std::shared_ptr< CUniformDiffuseCell > aCell = std::make_shared< CUniformDiffuseCell >( aMaterial, aCellDescription );

		m_Shade = std::make_shared< CUniformDiffuseBSDFLayer >( aCell, aBSDF );

	}

public:
	std::shared_ptr< CBSDFLayer > GetShade() {
		return m_Shade;
	};

};

TEST_F( TestPerfectDiffuseShade1, TestSolarProperties ) {
	SCOPED_TRACE( "Begin Test: Perfect diffuse shade - Solar properties." );

	std::shared_ptr< CBSDFLayer > aShade = GetShade();

	std::shared_ptr< CBSDFIntegrator > aResults = aShade->getResults();

	double tauDiff = aResults->DiffDiff( Side::Front, PropertySimple::T );
	EXPECT_NEAR( 0.000000000, tauDiff, 1e-6 );

	double RfDiff = aResults->DiffDiff( Side::Front, PropertySimple::R );
	EXPECT_NEAR( 0.550000000, RfDiff, 1e-6 );

	std::shared_ptr< CSquareMatrix > aT = aResults->getMatrix( Side::Front, PropertySimple::T );

	// Test only diagonal of transmittance matrix
	size_t size = aT->getSize();

	std::vector< double > correctResults;
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.000000 );

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
	correctResults.push_back( 0.175070 );
	correctResults.push_back( 0.175070 );
	correctResults.push_back( 0.175070 );
	correctResults.push_back( 0.175070 );
	correctResults.push_back( 0.175070 );
	correctResults.push_back( 0.175070 );
	correctResults.push_back( 0.175070 );
	correctResults.push_back( 0.175070 );
	correctResults.push_back( 0.175070 );
	correctResults.push_back( 0.175070 );
	correctResults.push_back( 0.175070 );
	correctResults.push_back( 0.175070 );
	correctResults.push_back( 0.175070 );
	correctResults.push_back( 0.175070 );
	correctResults.push_back( 0.175070 );
	correctResults.push_back( 0.175070 );
	correctResults.push_back( 0.175070 );
	correctResults.push_back( 0.175070 );
	correctResults.push_back( 0.175070 );
	correctResults.push_back( 0.175070 );
	correctResults.push_back( 0.175070 );
	correctResults.push_back( 0.175070 );
	correctResults.push_back( 0.175070 );
	correctResults.push_back( 0.175070 );
	correctResults.push_back( 0.175070 );
	correctResults.push_back( 0.175070 );
	correctResults.push_back( 0.175070 );
	correctResults.push_back( 0.175070 );
	correctResults.push_back( 0.175070 );
	correctResults.push_back( 0.175070 );
	correctResults.push_back( 0.175070 );
	correctResults.push_back( 0.175070 );
	correctResults.push_back( 0.175070 );
	correctResults.push_back( 0.175070 );
	correctResults.push_back( 0.175070 );
	correctResults.push_back( 0.175070 );
	correctResults.push_back( 0.175070 );
	correctResults.push_back( 0.175070 );
	correctResults.push_back( 0.175070 );
	correctResults.push_back( 0.175070 );
	correctResults.push_back( 0.175070 );

	calculatedResults.clear();
	for ( size_t i = 0; i < size; ++i ) {
		calculatedResults.push_back( ( *aRf )[ i ][ i ] );
	}

	EXPECT_EQ( correctResults.size(), calculatedResults.size() );
	for ( size_t i = 0; i < size; ++i ) {
		EXPECT_NEAR( correctResults[i], calculatedResults[i], 1e-5 );
	}

}
