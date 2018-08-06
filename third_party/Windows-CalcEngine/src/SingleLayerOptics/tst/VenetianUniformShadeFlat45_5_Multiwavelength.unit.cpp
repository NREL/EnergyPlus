#include <memory>
#include <gtest/gtest.h>

#include "WCECommon.hpp"
#include "WCESingleLayerOptics.hpp"


using namespace SingleLayerOptics;
using namespace FenestrationCommon;
using namespace SpectralAveraging;

class TestVenetianUniformShadeFlat45_5_Multiwavelength : public testing::Test {

private:
	std::shared_ptr< CBSDFLayer > m_Layer;

protected:
	virtual void SetUp() {
		// Solar range material
		double Tmat = 0.1;
		double Rfmat = 0.7;
		double Rbmat = 0.7;
		double minLambda = 0.3;
		double maxLambda = 2.5;
		std::shared_ptr< CMaterial > aSolarRangeMaterial =
			std::make_shared< CMaterialSingleBand >( Tmat, Tmat, Rfmat, Rbmat, minLambda, maxLambda );

		// Visible range
		Tmat = 0.2;
		Rfmat = 0.6;
		Rbmat = 0.6;
		minLambda = 0.38;
		maxLambda = 0.78;
		std::shared_ptr< CMaterial > aVisibleRangeMaterial =
			std::make_shared< CMaterialSingleBand >( Tmat, Tmat, Rfmat, Rbmat, minLambda, maxLambda );

		double ratio = 0.49;

		std::shared_ptr< CMaterial > aMaterial =
			std::make_shared< CMaterialDualBand >( aVisibleRangeMaterial, aSolarRangeMaterial, ratio );

		// make cell geometry
		double slatWidth = 0.016; // m
		double slatSpacing = 0.012; // m
		double slatTiltAngle = 45;
		double curvatureRadius = 0;
		size_t numOfSlatSegments = 5;

		std::shared_ptr< ICellDescription > aCellDescription =
			std::make_shared< CVenetianCellDescription >( slatWidth, slatSpacing, slatTiltAngle,
			                                         curvatureRadius, numOfSlatSegments );

		// create BSDF
		std::shared_ptr< CBSDFHemisphere > aBSDF = std::make_shared< CBSDFHemisphere >( BSDFBasis::Quarter );

		// make layer
		CBSDFLayerMaker aMaker = CBSDFLayerMaker( aMaterial, aBSDF, aCellDescription );
		m_Layer = aMaker.getLayer();

	}

public:
	std::shared_ptr< CBSDFLayer > getLayer() {
		return m_Layer;
	};

};

TEST_F( TestVenetianUniformShadeFlat45_5_Multiwavelength, TestVenetianMultiWavelength ) {
	SCOPED_TRACE( "Begin Test: Venetian layer (multi range) - BSDF." );

	std::shared_ptr< CBSDFLayer > aLayer = getLayer();

	std::shared_ptr< std::vector< std::shared_ptr< CBSDFIntegrator > > > aResults = aLayer->getWavelengthResults();

	size_t correctSize = 4;

	EXPECT_EQ( correctSize, aResults->size() );

	///////////////////////////////////////////////////////////////////////
	//  Wavelength number 1
	///////////////////////////////////////////////////////////////////////

	std::shared_ptr< CSquareMatrix > aT = ( *aResults )[ 0 ]->getMatrix( Side::Front, PropertySimple::T );

	// Test only diagonal of transmittance matrix
	size_t size = aT->getSize();

	std::vector< double > correctResults;
	correctResults.push_back( 0.743897 );
	correctResults.push_back( 0.801801 );
	correctResults.push_back( 3.838654 );
	correctResults.push_back( 5.096560 );
	correctResults.push_back( 3.838654 );
	correctResults.push_back( 0.801801 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.743310 );
	correctResults.push_back( 5.194719 );
	correctResults.push_back( 8.453376 );
	correctResults.push_back( 9.646127 );
	correctResults.push_back( 8.453376 );
	correctResults.push_back( 5.194719 );
	correctResults.push_back( 0.743310 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.743310 );
	correctResults.push_back( 9.176180 );
	correctResults.push_back( 10.644505 );
	correctResults.push_back( 8.384925 );
	correctResults.push_back( 10.644505 );
	correctResults.push_back( 9.176180 );
	correctResults.push_back( 0.743310 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.706601 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.000000 );
	correctResults.push_back( 0.706601 );
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

	// Front reflectance
	std::shared_ptr< CSquareMatrix > aRf = ( *aResults )[ 0 ]->getMatrix( Side::Front, PropertySimple::R );

	correctResults.clear();
	calculatedResults.clear();

	correctResults.push_back( 0 );
	correctResults.push_back( 0 );
	correctResults.push_back( 0 );
	correctResults.push_back( 0 );
	correctResults.push_back( 0 );
	correctResults.push_back( 0 );
	correctResults.push_back( 0 );
	correctResults.push_back( 0 );
	correctResults.push_back( 0 );
	correctResults.push_back( 0 );
	correctResults.push_back( 0 );
	correctResults.push_back( 0 );
	correctResults.push_back( 0 );
	correctResults.push_back( 0 );
	correctResults.push_back( 0 );
	correctResults.push_back( 0 );
	correctResults.push_back( 0 );
	correctResults.push_back( 0 );
	correctResults.push_back( 0 );
	correctResults.push_back( 0 );
	correctResults.push_back( 0 );
	correctResults.push_back( 0 );
	correctResults.push_back( 0 );
	correctResults.push_back( 0 );
	correctResults.push_back( 0 );
	correctResults.push_back( 0 );
	correctResults.push_back( 0 );
	correctResults.push_back( 0 );
	correctResults.push_back( 0 );
	correctResults.push_back( 0 );
	correctResults.push_back( 0 );
	correctResults.push_back( 0 );
	correctResults.push_back( 0 );
	correctResults.push_back( 0 );
	correctResults.push_back( 0 );
	correctResults.push_back( 0 );
	correctResults.push_back( 0 );
	correctResults.push_back( 0 );
	correctResults.push_back( 0 );
	correctResults.push_back( 0 );
	correctResults.push_back( 0 );

	for ( size_t i = 0; i < size; ++i ) {
		calculatedResults.push_back( ( *aRf )[ i ][ i ] );
	}

	EXPECT_EQ( correctResults.size(), calculatedResults.size() );
	for ( size_t i = 0; i < size; ++i ) {
		EXPECT_NEAR( correctResults[i], calculatedResults[i], 1e-5 );
	}

	///////////////////////////////////////////////////////////////////////
	//  Wavelength number 2
	///////////////////////////////////////////////////////////////////////

	aT = ( *aResults )[ 1 ]->getMatrix( Side::Front, PropertySimple::T );

	// Test only diagonal of transmittance matrix
	size = aT->getSize();

	correctResults.clear();
	calculatedResults.clear();

	correctResults.push_back( 0.802660 );
	correctResults.push_back( 0.860564 );
	correctResults.push_back( 3.883917 );
	correctResults.push_back( 5.136230 );
	correctResults.push_back( 3.883917 );
	correctResults.push_back( 0.860564 );
	correctResults.push_back( 0.054766 );
	correctResults.push_back( 0.050509 );
	correctResults.push_back( 0.054766 );
	correctResults.push_back( 0.802074 );
	correctResults.push_back( 5.232135 );
	correctResults.push_back( 8.475165 );
	correctResults.push_back( 9.662197 );
	correctResults.push_back( 8.475165 );
	correctResults.push_back( 5.232135 );
	correctResults.push_back( 0.802074 );
	correctResults.push_back( 0.049555 );
	correctResults.push_back( 0.043063 );
	correctResults.push_back( 0.040687 );
	correctResults.push_back( 0.043063 );
	correctResults.push_back( 0.049555 );
	correctResults.push_back( 0.802074 );
	correctResults.push_back( 9.194503 );
	correctResults.push_back( 10.672994 );
	correctResults.push_back( 8.440777 );
	correctResults.push_back( 10.672994 );
	correctResults.push_back( 9.194503 );
	correctResults.push_back( 0.802074 );
	correctResults.push_back( 0.041623 );
	correctResults.push_back( 0.035407 );
	correctResults.push_back( 0.033549 );
	correctResults.push_back( 0.035407 );
	correctResults.push_back( 0.041623 );
	correctResults.push_back( 0.765364 );
	correctResults.push_back( 0.121632 );
	correctResults.push_back( 0.104072 );
	correctResults.push_back( 0.121632 );
	correctResults.push_back( 0.765364 );
	correctResults.push_back( 0.027554 );
	correctResults.push_back( 0.024517 );
	correctResults.push_back( 0.027554 );

	for ( size_t i = 0; i < size; ++i ) {
		calculatedResults.push_back( ( *aT )[ i ][ i ] );
	}

	EXPECT_EQ( correctResults.size(), calculatedResults.size() );
	for ( size_t i = 0; i < size; ++i ) {
		EXPECT_NEAR( correctResults[i], calculatedResults[i], 1e-5 );
	}

	// Front reflectance
	aRf = ( *aResults )[ 1 ]->getMatrix( Side::Front, PropertySimple::R );

	correctResults.clear();
	calculatedResults.clear();

	correctResults.push_back( 0.148393 );
	correctResults.push_back( 0.148393 );
	correctResults.push_back( 0.114299 );
	correctResults.push_back( 0.100177 );
	correctResults.push_back( 0.114299 );
	correctResults.push_back( 0.148393 );
	correctResults.push_back( 0.168002 );
	correctResults.push_back( 0.173972 );
	correctResults.push_back( 0.168002 );
	correctResults.push_back( 0.148393 );
	correctResults.push_back( 0.094486 );
	correctResults.push_back( 0.055023 );
	correctResults.push_back( 0.040579 );
	correctResults.push_back( 0.055023 );
	correctResults.push_back( 0.094486 );
	correctResults.push_back( 0.148393 );
	correctResults.push_back( 0.175452 );
	correctResults.push_back( 0.185559 );
	correctResults.push_back( 0.189259 );
	correctResults.push_back( 0.185559 );
	correctResults.push_back( 0.175452 );
	correctResults.push_back( 0.148393 );
	correctResults.push_back( 0.046270 );
	correctResults.push_back( 0.011282 );
	correctResults.push_back( 0.022117 );
	correctResults.push_back( 0.011282 );
	correctResults.push_back( 0.046270 );
	correctResults.push_back( 0.148393 );
	correctResults.push_back( 0.187801 );
	correctResults.push_back( 0.197924 );
	correctResults.push_back( 0.201009 );
	correctResults.push_back( 0.197924 );
	correctResults.push_back( 0.187801 );
	correctResults.push_back( 0.148393 );
	correctResults.push_back( 0.085962 );
	correctResults.push_back( 0.098907 );
	correctResults.push_back( 0.085962 );
	correctResults.push_back( 0.148393 );
	correctResults.push_back( 0.211083 );
	correctResults.push_back( 0.216240 );
	correctResults.push_back( 0.211083 );

	for ( size_t i = 0; i < size; ++i ) {
		calculatedResults.push_back( ( *aRf )[ i ][ i ] );
	}

	EXPECT_EQ( correctResults.size(), calculatedResults.size() );
	for ( size_t i = 0; i < size; ++i ) {
		EXPECT_NEAR( correctResults[i], calculatedResults[i], 1e-5 );
	}

	///////////////////////////////////////////////////////////////////////
	//  Wavelength number 3
	///////////////////////////////////////////////////////////////////////

	aT = ( *aResults )[ 2 ]->getMatrix( Side::Front, PropertySimple::T );

	// Test only diagonal of transmittance matrix
	size = aT->getSize();

	correctResults.clear();
	calculatedResults.clear();

	correctResults.push_back( 0.822696 );
	correctResults.push_back( 0.880601 );
	correctResults.push_back( 3.899350 );
	correctResults.push_back( 5.149756 );
	correctResults.push_back( 3.899350 );
	correctResults.push_back( 0.880601 );
	correctResults.push_back( 0.076246 );
	correctResults.push_back( 0.072118 );
	correctResults.push_back( 0.076246 );
	correctResults.push_back( 0.822110 );
	correctResults.push_back( 5.244893 );
	correctResults.push_back( 8.482595 );
	correctResults.push_back( 9.667676 );
	correctResults.push_back( 8.482595 );
	correctResults.push_back( 5.244893 );
	correctResults.push_back( 0.822110 );
	correctResults.push_back( 0.071121 );
	correctResults.push_back( 0.064321 );
	correctResults.push_back( 0.061831 );
	correctResults.push_back( 0.064321 );
	correctResults.push_back( 0.071121 );
	correctResults.push_back( 0.822110 );
	correctResults.push_back( 9.200750 );
	correctResults.push_back( 10.669461 );
	correctResults.push_back( 8.433850 );
	correctResults.push_back( 10.669461 );
	correctResults.push_back( 9.200750 );
	correctResults.push_back( 0.822110 );
	correctResults.push_back( 0.062812 );
	correctResults.push_back( 0.055872 );
	correctResults.push_back( 0.053740 );
	correctResults.push_back( 0.055872 );
	correctResults.push_back( 0.062812 );
	correctResults.push_back( 0.785400 );
	correctResults.push_back( 0.107468 );
	correctResults.push_back( 0.093221 );
	correctResults.push_back( 0.107468 );
	correctResults.push_back( 0.785400 );
	correctResults.push_back( 0.046481 );
	correctResults.push_back( 0.042638 );
	correctResults.push_back( 0.046481 );

	for ( size_t i = 0; i < size; ++i ) {
		calculatedResults.push_back( ( *aT )[ i ][ i ] );
	}

	EXPECT_EQ( correctResults.size(), calculatedResults.size() );
	for ( size_t i = 0; i < size; ++i ) {
		EXPECT_NEAR( correctResults[i], calculatedResults[i], 1e-5 );
	}

	// Front reflectance
	aRf = ( *aResults )[ 2 ]->getMatrix( Side::Front, PropertySimple::R );

	correctResults.clear();
	calculatedResults.clear();

	correctResults.push_back( 0.129989 );
	correctResults.push_back( 0.129989 );
	correctResults.push_back( 0.100123 );
	correctResults.push_back( 0.087753 );
	correctResults.push_back( 0.100123 );
	correctResults.push_back( 0.129989 );
	correctResults.push_back( 0.146686 );
	correctResults.push_back( 0.151646 );
	correctResults.push_back( 0.146686 );
	correctResults.push_back( 0.129989 );
	correctResults.push_back( 0.082768 );
	correctResults.push_back( 0.048199 );
	correctResults.push_back( 0.035546 );
	correctResults.push_back( 0.048199 );
	correctResults.push_back( 0.082768 );
	correctResults.push_back( 0.129989 );
	correctResults.push_back( 0.152900 );
	correctResults.push_back( 0.161474 );
	correctResults.push_back( 0.164612 );
	correctResults.push_back( 0.161474 );
	correctResults.push_back( 0.152900 );
	correctResults.push_back( 0.129989 );
	correctResults.push_back( 0.040532 );
	correctResults.push_back( 0.015128 );
	correctResults.push_back( 0.029659 );
	correctResults.push_back( 0.015128 );
	correctResults.push_back( 0.040532 );
	correctResults.push_back( 0.129989 );
	correctResults.push_back( 0.163376 );
	correctResults.push_back( 0.172244 );
	correctResults.push_back( 0.174984 );
	correctResults.push_back( 0.172244 );
	correctResults.push_back( 0.163376 );
	correctResults.push_back( 0.129989 );
	correctResults.push_back( 0.108044 );
	correctResults.push_back( 0.120070 );
	correctResults.push_back( 0.108044 );
	correctResults.push_back( 0.129989 );
	correctResults.push_back( 0.184339 );
	correctResults.push_back( 0.189304 );
	correctResults.push_back( 0.184339 );

	for ( size_t i = 0; i < size; ++i ) {
		calculatedResults.push_back( ( *aRf )[ i ][ i ] );
	}

	EXPECT_EQ( correctResults.size(), calculatedResults.size() );
	for ( size_t i = 0; i < size; ++i ) {
		EXPECT_NEAR( correctResults[i], calculatedResults[i], 1e-5 );
	}

	///////////////////////////////////////////////////////////////////////
	//  Wavelength number 4
	///////////////////////////////////////////////////////////////////////

	aT = ( *aResults )[ 3 ]->getMatrix( Side::Front, PropertySimple::T );

	// Test only diagonal of transmittance matrix
	size = aT->getSize();

	correctResults.clear();
	calculatedResults.clear();

	correctResults.push_back( 0.802660 );
	correctResults.push_back( 0.860564 );
	correctResults.push_back( 3.883917 );
	correctResults.push_back( 5.136230 );
	correctResults.push_back( 3.883917 );
	correctResults.push_back( 0.860564 );
	correctResults.push_back( 0.054766 );
	correctResults.push_back( 0.050509 );
	correctResults.push_back( 0.054766 );
	correctResults.push_back( 0.802074 );
	correctResults.push_back( 5.232135 );
	correctResults.push_back( 8.475165 );
	correctResults.push_back( 9.662197 );
	correctResults.push_back( 8.475165 );
	correctResults.push_back( 5.232135 );
	correctResults.push_back( 0.802074 );
	correctResults.push_back( 0.049555 );
	correctResults.push_back( 0.043063 );
	correctResults.push_back( 0.040687 );
	correctResults.push_back( 0.043063 );
	correctResults.push_back( 0.049555 );
	correctResults.push_back( 0.802074 );
	correctResults.push_back( 9.194503 );
	correctResults.push_back( 10.672994 );
	correctResults.push_back( 8.440777 );
	correctResults.push_back( 10.672994 );
	correctResults.push_back( 9.194503 );
	correctResults.push_back( 0.802074 );
	correctResults.push_back( 0.041623 );
	correctResults.push_back( 0.035407 );
	correctResults.push_back( 0.033549 );
	correctResults.push_back( 0.035407 );
	correctResults.push_back( 0.041623 );
	correctResults.push_back( 0.765364 );
	correctResults.push_back( 0.121632 );
	correctResults.push_back( 0.104072 );
	correctResults.push_back( 0.121632 );
	correctResults.push_back( 0.765364 );
	correctResults.push_back( 0.027554 );
	correctResults.push_back( 0.024517 );
	correctResults.push_back( 0.027554 );

	for ( size_t i = 0; i < size; ++i ) {
		calculatedResults.push_back( ( *aT )[ i ][ i ] );
	}

	EXPECT_EQ( correctResults.size(), calculatedResults.size() );
	for ( size_t i = 0; i < size; ++i ) {
		EXPECT_NEAR( correctResults[i], calculatedResults[i], 1e-5 );
	}

	// Front reflectance
	aRf = ( *aResults )[ 3 ]->getMatrix( Side::Front, PropertySimple::R );

	correctResults.clear();
	calculatedResults.clear();

	correctResults.push_back( 0.148393 );
	correctResults.push_back( 0.148393 );
	correctResults.push_back( 0.114299 );
	correctResults.push_back( 0.100177 );
	correctResults.push_back( 0.114299 );
	correctResults.push_back( 0.148393 );
	correctResults.push_back( 0.168002 );
	correctResults.push_back( 0.173972 );
	correctResults.push_back( 0.168002 );
	correctResults.push_back( 0.148393 );
	correctResults.push_back( 0.094486 );
	correctResults.push_back( 0.055023 );
	correctResults.push_back( 0.040579 );
	correctResults.push_back( 0.055023 );
	correctResults.push_back( 0.094486 );
	correctResults.push_back( 0.148393 );
	correctResults.push_back( 0.175452 );
	correctResults.push_back( 0.185559 );
	correctResults.push_back( 0.189259 );
	correctResults.push_back( 0.185559 );
	correctResults.push_back( 0.175452 );
	correctResults.push_back( 0.148393 );
	correctResults.push_back( 0.046270 );
	correctResults.push_back( 0.011282 );
	correctResults.push_back( 0.022117 );
	correctResults.push_back( 0.011282 );
	correctResults.push_back( 0.046270 );
	correctResults.push_back( 0.148393 );
	correctResults.push_back( 0.187801 );
	correctResults.push_back( 0.197924 );
	correctResults.push_back( 0.201009 );
	correctResults.push_back( 0.197924 );
	correctResults.push_back( 0.187801 );
	correctResults.push_back( 0.148393 );
	correctResults.push_back( 0.085962 );
	correctResults.push_back( 0.098907 );
	correctResults.push_back( 0.085962 );
	correctResults.push_back( 0.148393 );
	correctResults.push_back( 0.211083 );
	correctResults.push_back( 0.216240 );
	correctResults.push_back( 0.211083 );

	for ( size_t i = 0; i < size; ++i ) {
		calculatedResults.push_back( ( *aRf )[ i ][ i ] );
	}

	EXPECT_EQ( correctResults.size(), calculatedResults.size() );
	for ( size_t i = 0; i < size; ++i ) {
		EXPECT_NEAR( correctResults[i], calculatedResults[i], 1e-5 );
	}

}
