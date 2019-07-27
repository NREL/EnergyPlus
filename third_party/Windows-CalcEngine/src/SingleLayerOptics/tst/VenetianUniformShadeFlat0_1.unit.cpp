#include <memory>
#include <gtest/gtest.h>

#include "WCECommon.hpp"
#include "WCESingleLayerOptics.hpp"


using namespace SingleLayerOptics;
using namespace FenestrationCommon;

// Creation of BSDF matrix for single layer venetian shade with given material and geometrical properties.
// Method for calculation is diffuse part of distribution is uniform.
class TestVenetianUniformShadeFlat0_1 : public testing::Test {

private:
	std::shared_ptr< CBSDFLayer > m_Shade;

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
		double slatTiltAngle = 0;
		double curvatureRadius = 0;
		size_t numOfSlatSegments = 1;
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

TEST_F( TestVenetianUniformShadeFlat0_1, TestVenetian1 ) {
	SCOPED_TRACE( "Begin Test: Venetian cell (Flat, 45 degrees slats) - solar properties." );

	std::shared_ptr< CBSDFLayer > aShade = GetShade();

	std::shared_ptr< CBSDFIntegrator > aResults = aShade->getResults();

	double tauDiff = aResults->DiffDiff( Side::Front, PropertySimple::T );
	EXPECT_NEAR( 0.65093991496438897, tauDiff, 1e-6 );

	double RfDiff = aResults->DiffDiff( Side::Front, PropertySimple::R );
	EXPECT_NEAR( 0.188319, RfDiff, 1e-6 );

	std::shared_ptr< CSquareMatrix > aT = aResults->getMatrix( Side::Front, PropertySimple::T );

	// Test only diagonal of transmittance matrix
	size_t size = aT->getSize();

	std::vector< double > correctResults;
	correctResults.push_back( 13.007243 );
	correctResults.push_back( 14.019711 );
	correctResults.push_back( 10.824270 );
	correctResults.push_back( 9.5006750 );
	correctResults.push_back( 10.824270 );
	correctResults.push_back( 14.019711 );
	correctResults.push_back( 10.824270 );
	correctResults.push_back( 9.5006750 );
	correctResults.push_back( 10.824270 );
	correctResults.push_back( 12.996987 );
	correctResults.push_back( 8.3160770 );
	correctResults.push_back( 4.8894140 );
	correctResults.push_back( 3.6351680 );
	correctResults.push_back( 4.8894140 );
	correctResults.push_back( 8.3160770 );
	correctResults.push_back( 12.996987 );
	correctResults.push_back( 8.3160770 );
	correctResults.push_back( 4.8894140 );
	correctResults.push_back( 3.6351680 );
	correctResults.push_back( 4.8894140 );
	correctResults.push_back( 8.3160770 );
	correctResults.push_back( 12.996987 );
	correctResults.push_back( 4.1293450 );
	correctResults.push_back( 0.1115490 );
	correctResults.push_back( 0.1115490 );
	correctResults.push_back( 0.1115490 );
	correctResults.push_back( 4.1293450 );
	correctResults.push_back( 12.996987 );
	correctResults.push_back( 4.1293450 );
	correctResults.push_back( 0.1115490 );
	correctResults.push_back( 0.1115490 );
	correctResults.push_back( 0.1115490 );
	correctResults.push_back( 4.1293450 );
	correctResults.push_back( 12.355109 );
	correctResults.push_back( 0.1115490 );
	correctResults.push_back( 0.1115490 );
	correctResults.push_back( 0.1115490 );
	correctResults.push_back( 12.355109 );
	correctResults.push_back( 0.1115490 );
	correctResults.push_back( 0.1115490 );
	correctResults.push_back( 0.1115490 );

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

	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.025629 );
	correctResults.push_back( 0.036244 );
	correctResults.push_back( 0.025629 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.025629 );
	correctResults.push_back( 0.036244 );
	correctResults.push_back( 0.025629 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.040522 );
	correctResults.push_back( 0.070187 );
	correctResults.push_back( 0.081045 );
	correctResults.push_back( 0.070187 );
	correctResults.push_back( 0.040522 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.040522 );
	correctResults.push_back( 0.070187 );
	correctResults.push_back( 0.081045 );
	correctResults.push_back( 0.070187 );
	correctResults.push_back( 0.040522 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.076767 );
	correctResults.push_back( 0.111549 );
	correctResults.push_back( 0.111549 );
	correctResults.push_back( 0.111549 );
	correctResults.push_back( 0.076767 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.076767 );
	correctResults.push_back( 0.111549 );
	correctResults.push_back( 0.111549 );
	correctResults.push_back( 0.111549 );
	correctResults.push_back( 0.076767 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.111549 );
	correctResults.push_back( 0.111549 );
	correctResults.push_back( 0.111549 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.111549 );
	correctResults.push_back( 0.111549 );
	correctResults.push_back( 0.111549 );

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

	correctResults.push_back( 13.007243 );
	correctResults.push_back( 14.019711 );
	correctResults.push_back( 10.824270 );
	correctResults.push_back( 9.5006750 );
	correctResults.push_back( 10.824270 );
	correctResults.push_back( 14.019711 );
	correctResults.push_back( 10.824270 );
	correctResults.push_back( 9.5006750 );
	correctResults.push_back( 10.824270 );
	correctResults.push_back( 12.996987 );
	correctResults.push_back( 8.3160770 );
	correctResults.push_back( 4.8894140 );
	correctResults.push_back( 3.6351680 );
	correctResults.push_back( 4.8894140 );
	correctResults.push_back( 8.3160770 );
	correctResults.push_back( 12.996987 );
	correctResults.push_back( 8.3160770 );
	correctResults.push_back( 4.8894140 );
	correctResults.push_back( 3.6351680 );
	correctResults.push_back( 4.8894140 );
	correctResults.push_back( 8.3160770 );
	correctResults.push_back( 12.996987 );
	correctResults.push_back( 4.1293450 );
	correctResults.push_back( 0.1115490 );
	correctResults.push_back( 0.1115490 );
	correctResults.push_back( 0.1115490 );
	correctResults.push_back( 4.1293450 );
	correctResults.push_back( 12.996987 );
	correctResults.push_back( 4.1293450 );
	correctResults.push_back( 0.1115490 );
	correctResults.push_back( 0.1115490 );
	correctResults.push_back( 0.1115490 );
	correctResults.push_back( 4.1293450 );
	correctResults.push_back( 12.355109 );
	correctResults.push_back( 0.1115490 );
	correctResults.push_back( 0.1115490 );
	correctResults.push_back( 0.1115490 );
	correctResults.push_back( 12.355109 );
	correctResults.push_back( 0.1115490 );
	correctResults.push_back( 0.1115490 );
	correctResults.push_back( 0.1115490 );

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

	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.025629 );
	correctResults.push_back( 0.036244 );
	correctResults.push_back( 0.025629 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.025629 );
	correctResults.push_back( 0.036244 );
	correctResults.push_back( 0.025629 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.040522 );
	correctResults.push_back( 0.070187 );
	correctResults.push_back( 0.081045 );
	correctResults.push_back( 0.070187 );
	correctResults.push_back( 0.040522 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.040522 );
	correctResults.push_back( 0.070187 );
	correctResults.push_back( 0.081045 );
	correctResults.push_back( 0.070187 );
	correctResults.push_back( 0.040522 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.076767 );
	correctResults.push_back( 0.111549 );
	correctResults.push_back( 0.111549 );
	correctResults.push_back( 0.111549 );
	correctResults.push_back( 0.076767 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.076767 );
	correctResults.push_back( 0.111549 );
	correctResults.push_back( 0.111549 );
	correctResults.push_back( 0.111549 );
	correctResults.push_back( 0.076767 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.111549 );
	correctResults.push_back( 0.111549 );
	correctResults.push_back( 0.111549 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.111549 );
	correctResults.push_back( 0.111549 );
	correctResults.push_back( 0.111549 );

	for ( size_t i = 0; i < size; ++i ) {
		calculatedResults.push_back( ( *aRb )[ i ][ i ] );
	}

	EXPECT_EQ( correctResults.size(), calculatedResults.size() );
	for ( size_t i = 0; i < size; ++i ) {
		EXPECT_NEAR( correctResults[i], calculatedResults[i], 1e-5 );
	}

}
