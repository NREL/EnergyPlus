#include <memory>
#include <gtest/gtest.h>

#include "WCECommon.hpp"
#include "WCESingleLayerOptics.hpp"


using namespace SingleLayerOptics;
using namespace FenestrationCommon;

class TestVenetianUniformShadeCurvedMinus55_0 : public testing::Test {

private:
	std::shared_ptr< CBSDFLayer > m_Shade;

protected:
	virtual void SetUp() {
		// create material
		double Tmat = 0.15;
		double Rfmat = 0.2;
		double Rbmat = 0.5;
		double minLambda = 0.3;
		double maxLambda = 2.5;
		std::shared_ptr< CMaterial > aMaterial =
			std::make_shared< CMaterialSingleBand >( Tmat, Tmat, Rfmat, Rbmat, minLambda, maxLambda );


		// make cell geometry
		double slatWidth = 0.076200; // m
		double slatSpacing = 0.057150; // m
		double slatTiltAngle = -55;
		double curvatureRadius = 0.123967;
		size_t numOfSlatSegments = 2;

		std::shared_ptr< ICellDescription > aCellDescription =
			std::make_shared< CVenetianCellDescription >( slatWidth, slatSpacing, slatTiltAngle,
			                                         curvatureRadius, numOfSlatSegments );

		// create BSDF
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

TEST_F( TestVenetianUniformShadeCurvedMinus55_0, TestVenetian1 ) {
	SCOPED_TRACE( "Begin Test: Venetian shade (Curved, -55 degrees slats)." );

	std::shared_ptr< CBSDFLayer > aShade = GetShade();

	std::shared_ptr< CBSDFIntegrator > aResults = aShade->getResults();

	double tauDiff = aResults->DiffDiff( Side::Front, PropertySimple::T );
	EXPECT_NEAR( 0.232141, tauDiff, 1e-6 );

	double RfDiff = aResults->DiffDiff( Side::Front, PropertySimple::R );
	EXPECT_NEAR( 0.301555, RfDiff, 1e-6 );

	std::shared_ptr< CSquareMatrix > aT = aResults->getMatrix( Side::Front, PropertySimple::T );

	// Test only diagonal of transmittance matrix
	size_t size = aT->getSize();

	std::vector< double > correctResults;
	correctResults.push_back( 0.043727 );
	correctResults.push_back( 0.043727 );
	correctResults.push_back( 0.040846 );
	correctResults.push_back( 0.039652 );
	correctResults.push_back( 0.040846 );
	correctResults.push_back( 0.043727 );
	correctResults.push_back( 1.212588 );
	correctResults.push_back( 2.230015 );
	correctResults.push_back( 1.212588 );
	correctResults.push_back( 0.043727 );
	correctResults.push_back( 0.039171 );
	correctResults.push_back( 0.035836 );
	correctResults.push_back( 0.034615 );
	correctResults.push_back( 0.035836 );
	correctResults.push_back( 0.039171 );
	correctResults.push_back( 0.043727 );
	correctResults.push_back( 2.450197 );
	correctResults.push_back( 5.085284 );
	correctResults.push_back( 6.049793 );
	correctResults.push_back( 5.085284 );
	correctResults.push_back( 2.450197 );
	correctResults.push_back( 0.043727 );
	correctResults.push_back( 0.035096 );
	correctResults.push_back( 0.029961 );
	correctResults.push_back( 0.029961 );
	correctResults.push_back( 0.029961 );
	correctResults.push_back( 0.035096 );
	correctResults.push_back( 0.043727 );
	correctResults.push_back( 5.669774 );
	correctResults.push_back( 9.720574 );
	correctResults.push_back( 10.42533 );
	correctResults.push_back( 9.720574 );
	correctResults.push_back( 5.669774 );
	correctResults.push_back( 0.043727 );
	correctResults.push_back( 0.029961 );
	correctResults.push_back( 0.029961 );
	correctResults.push_back( 0.029961 );
	correctResults.push_back( 0.043727 );
	correctResults.push_back( 0.031686 );
	correctResults.push_back( 0.028177 );
	correctResults.push_back( 0.031686 );

	std::vector< double > calculatedResults;
	for ( size_t i = 0; i < size; ++i ) {
		calculatedResults.push_back( ( *aT )[ i ][ i ] );
	}

	EXPECT_EQ( correctResults.size(), calculatedResults.size() );
	for ( size_t i = 0; i < size; ++i ) {
		EXPECT_NEAR( correctResults[i], calculatedResults[i], 1e-5 );
	}

	// Front reflectance
	std::shared_ptr< CSquareMatrix > aRf = aResults->getMatrix( Side::Front, PropertySimple::R );

	correctResults.clear();
	calculatedResults.clear();

	correctResults.push_back( 0.108993 );
	correctResults.push_back( 0.108993 );
	correctResults.push_back( 0.114409 );
	correctResults.push_back( 0.116652 );
	correctResults.push_back( 0.114409 );
	correctResults.push_back( 0.108993 );
	correctResults.push_back( 0.096518 );
	correctResults.push_back( 0.088122 );
	correctResults.push_back( 0.096518 );
	correctResults.push_back( 0.108993 );
	correctResults.push_back( 0.117556 );
	correctResults.push_back( 0.123825 );
	correctResults.push_back( 0.126119 );
	correctResults.push_back( 0.123825 );
	correctResults.push_back( 0.117556 );
	correctResults.push_back( 0.108993 );
	correctResults.push_back( 0.084738 );
	correctResults.push_back( 0.061276 );
	correctResults.push_back( 0.052688 );
	correctResults.push_back( 0.061276 );
	correctResults.push_back( 0.084738 );
	correctResults.push_back( 0.108993 );
	correctResults.push_back( 0.125215 );
	correctResults.push_back( 0.134867 );
	correctResults.push_back( 0.134867 );
	correctResults.push_back( 0.134867 );
	correctResults.push_back( 0.125215 );
	correctResults.push_back( 0.108993 );
	correctResults.push_back( 0.056072 );
	correctResults.push_back( 0.019202 );
	correctResults.push_back( 0.011971 );
	correctResults.push_back( 0.019202 );
	correctResults.push_back( 0.056072 );
	correctResults.push_back( 0.108993 );
	correctResults.push_back( 0.134867 );
	correctResults.push_back( 0.134867 );
	correctResults.push_back( 0.134867 );
	correctResults.push_back( 0.108993 );
	correctResults.push_back( 0.051442 );
	correctResults.push_back( 0.054310 );
	correctResults.push_back( 0.051442 );

	for ( size_t i = 0; i < size; ++i ) {
		calculatedResults.push_back( ( *aRf )[ i ][ i ] );
	}

	EXPECT_EQ( correctResults.size(), calculatedResults.size() );
	for ( size_t i = 0; i < size; ++i ) {
		EXPECT_NEAR( correctResults[i], calculatedResults[i], 1e-5 );
	}

	// Back transmittance
	std::shared_ptr< CSquareMatrix > aTb = aResults->getMatrix( Side::Back, PropertySimple::T );

	correctResults.clear();
	calculatedResults.clear();

	correctResults.push_back( 0.037957 );
	correctResults.push_back( 0.037957 );
	correctResults.push_back( 1.205976 );
	correctResults.push_back( 2.223146 );
	correctResults.push_back( 1.205976 );
	correctResults.push_back( 0.037957 );
	correctResults.push_back( 0.036121 );
	correctResults.push_back( 0.035360 );
	correctResults.push_back( 0.036121 );
	correctResults.push_back( 0.037957 );
	correctResults.push_back( 2.443226 );
	correctResults.push_back( 5.077597 );
	correctResults.push_back( 6.041844 );
	correctResults.push_back( 5.077597 );
	correctResults.push_back( 2.443226 );
	correctResults.push_back( 0.037957 );
	correctResults.push_back( 0.035054 );
	correctResults.push_back( 0.032928 );
	correctResults.push_back( 0.032150 );
	correctResults.push_back( 0.032928 );
	correctResults.push_back( 0.035054 );
	correctResults.push_back( 0.037957 );
	correctResults.push_back( 5.661928 );
	correctResults.push_back( 9.715204 );
	correctResults.push_back( 10.42404 );
	correctResults.push_back( 9.715204 );
	correctResults.push_back( 5.661928 );
	correctResults.push_back( 0.037957 );
	correctResults.push_back( 0.032456 );
	correctResults.push_back( 0.028429 );
	correctResults.push_back( 0.027340 );
	correctResults.push_back( 0.028429 );
	correctResults.push_back( 0.032456 );
	correctResults.push_back( 0.037957 );
	correctResults.push_back( 0.121449 );
	correctResults.push_back( 0.103251 );
	correctResults.push_back( 0.121449 );
	correctResults.push_back( 0.037957 );
	correctResults.push_back( 0.027340 );
	correctResults.push_back( 0.027340 );
	correctResults.push_back( 0.027340 );

	for ( size_t i = 0; i < size; ++i ) {
		calculatedResults.push_back( ( *aTb )[ i ][ i ] );
	}

	EXPECT_EQ( correctResults.size(), calculatedResults.size() );
	for ( size_t i = 0; i < size; ++i ) {
		EXPECT_NEAR( correctResults[i], calculatedResults[i], 1e-5 );
	}

	// Back reflectance
	std::shared_ptr< CSquareMatrix > aRb = aResults->getMatrix( Side::Back, PropertySimple::R );

	correctResults.clear();
	calculatedResults.clear();

	correctResults.push_back( 0.048186 );
	correctResults.push_back( 0.048186 );
	correctResults.push_back( 0.043586 );
	correctResults.push_back( 0.040604 );
	correctResults.push_back( 0.043586 );
	correctResults.push_back( 0.048186 );
	correctResults.push_back( 0.050433 );
	correctResults.push_back( 0.051364 );
	correctResults.push_back( 0.050433 );
	correctResults.push_back( 0.048186 );
	correctResults.push_back( 0.039403 );
	correctResults.push_back( 0.031071 );
	correctResults.push_back( 0.028022 );
	correctResults.push_back( 0.031071 );
	correctResults.push_back( 0.039403 );
	correctResults.push_back( 0.048186 );
	correctResults.push_back( 0.051739 );
	correctResults.push_back( 0.054340 );
	correctResults.push_back( 0.055292 );
	correctResults.push_back( 0.054340 );
	correctResults.push_back( 0.051739 );
	correctResults.push_back( 0.048186 );
	correctResults.push_back( 0.029223 );
	correctResults.push_back( 0.015480 );
	correctResults.push_back( 0.012137 );
	correctResults.push_back( 0.015480 );
	correctResults.push_back( 0.029223 );
	correctResults.push_back( 0.048186 );
	correctResults.push_back( 0.054917 );
	correctResults.push_back( 0.059845 );
	correctResults.push_back( 0.061178 );
	correctResults.push_back( 0.059845 );
	correctResults.push_back( 0.054917 );
	correctResults.push_back( 0.048186 );
	correctResults.push_back( 0.037100 );
	correctResults.push_back( 0.046782 );
	correctResults.push_back( 0.037100 );
	correctResults.push_back( 0.048186 );
	correctResults.push_back( 0.061178 );
	correctResults.push_back( 0.061178 );
	correctResults.push_back( 0.061178 );

	for ( size_t i = 0; i < size; ++i ) {
		calculatedResults.push_back( ( *aRb )[ i ][ i ] );
	}

	EXPECT_EQ( correctResults.size(), calculatedResults.size() );
	for ( size_t i = 0; i < size; ++i ) {
		EXPECT_NEAR( correctResults[i], calculatedResults[i], 1e-5 );
	}

}
