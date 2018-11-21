#include <memory>
#include <gtest/gtest.h>

#include "WCESingleLayerOptics.hpp"
#include "WCECommon.hpp"


using namespace SingleLayerOptics;
using namespace FenestrationCommon;
using namespace SpectralAveraging;

class TestWovenShadeMultiWavelength : public testing::Test {

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
		double diameter = 6.35; // mm
		double spacing = 19.05; // mm
		std::shared_ptr< ICellDescription > aCellDescription =
			std::make_shared< CWovenCellDescription >( diameter, spacing );

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

TEST_F( TestWovenShadeMultiWavelength, TestWovenMultiWavelength ) {
	SCOPED_TRACE( "Begin Test: Perforated layer (multi range) - BSDF." );

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
	correctResults.push_back( 5.78100 );
	correctResults.push_back( 6.07065 );
	correctResults.push_back( 6.06990 );
	correctResults.push_back( 6.05418 );
	correctResults.push_back( 6.06990 );
	correctResults.push_back( 6.07065 );
	correctResults.push_back( 6.06990 );
	correctResults.push_back( 6.05418 );
	correctResults.push_back( 6.06990 );
	correctResults.push_back( 5.09462 );
	correctResults.push_back( 5.11975 );
	correctResults.push_back( 4.99544 );
	correctResults.push_back( 4.78271 );
	correctResults.push_back( 4.99544 );
	correctResults.push_back( 5.11975 );
	correctResults.push_back( 5.09462 );
	correctResults.push_back( 5.11975 );
	correctResults.push_back( 4.99544 );
	correctResults.push_back( 4.78271 );
	correctResults.push_back( 4.99544 );
	correctResults.push_back( 5.11975 );
	correctResults.push_back( 3.75093 );
	correctResults.push_back( 3.93978 );
	correctResults.push_back( 3.35018 );
	correctResults.push_back( 1.31059 );
	correctResults.push_back( 3.35018 );
	correctResults.push_back( 3.93978 );
	correctResults.push_back( 3.75093 );
	correctResults.push_back( 3.93978 );
	correctResults.push_back( 3.35018 );
	correctResults.push_back( 1.31059 );
	correctResults.push_back( 3.35018 );
	correctResults.push_back( 3.93978 );
	correctResults.push_back( 0.00000 );
	correctResults.push_back( 0.00000 );
	correctResults.push_back( 0.00000 );
	correctResults.push_back( 0.00000 );
	correctResults.push_back( 0.00000 );
	correctResults.push_back( 0.00000 );
	correctResults.push_back( 0.00000 );
	correctResults.push_back( 0.00000 );

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

	correctResults.push_back( 5.786840 );
	correctResults.push_back( 6.076510 );
	correctResults.push_back( 6.075760 );
	correctResults.push_back( 6.060040 );
	correctResults.push_back( 6.075760 );
	correctResults.push_back( 6.076510 );
	correctResults.push_back( 6.075760 );
	correctResults.push_back( 6.060040 );
	correctResults.push_back( 6.075760 );
	correctResults.push_back( 5.100530 );
	correctResults.push_back( 5.125660 );
	correctResults.push_back( 5.001370 );
	correctResults.push_back( 4.788650 );
	correctResults.push_back( 5.001370 );
	correctResults.push_back( 5.125660 );
	correctResults.push_back( 5.100530 );
	correctResults.push_back( 5.125660 );
	correctResults.push_back( 5.001370 );
	correctResults.push_back( 4.788650 );
	correctResults.push_back( 5.001370 );
	correctResults.push_back( 5.125660 );
	correctResults.push_back( 3.767450 );
	correctResults.push_back( 3.960130 );
	correctResults.push_back( 3.373360 );
	correctResults.push_back( 1.327340 );
	correctResults.push_back( 3.373360 );
	correctResults.push_back( 3.960130 );
	correctResults.push_back( 3.767450 );
	correctResults.push_back( 3.960130 );
	correctResults.push_back( 3.373360 );
	correctResults.push_back( 1.327340 );
	correctResults.push_back( 3.373360 );
	correctResults.push_back( 3.960130 );
	correctResults.push_back( 0.043898 );
	correctResults.push_back( 0.033284 );
	correctResults.push_back( 0.043898 );
	correctResults.push_back( 0.033284 );
	correctResults.push_back( 0.043898 );
	correctResults.push_back( 0.033284 );
	correctResults.push_back( 0.043898 );
	correctResults.push_back( 0.033284 );

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

	correctResults.push_back( 0.135627 );
	correctResults.push_back( 0.138525 );
	correctResults.push_back( 0.138539 );
	correctResults.push_back( 0.138823 );
	correctResults.push_back( 0.138539 );
	correctResults.push_back( 0.138525 );
	correctResults.push_back( 0.138539 );
	correctResults.push_back( 0.138823 );
	correctResults.push_back( 0.138539 );
	correctResults.push_back( 0.148919 );
	correctResults.push_back( 0.148428 );
	correctResults.push_back( 0.150852 );
	correctResults.push_back( 0.155000 );
	correctResults.push_back( 0.150852 );
	correctResults.push_back( 0.148428 );
	correctResults.push_back( 0.148919 );
	correctResults.push_back( 0.148428 );
	correctResults.push_back( 0.150852 );
	correctResults.push_back( 0.155000 );
	correctResults.push_back( 0.150852 );
	correctResults.push_back( 0.148428 );
	correctResults.push_back( 0.164637 );
	correctResults.push_back( 0.157111 );
	correctResults.push_back( 0.165834 );
	correctResults.push_back( 0.212216 );
	correctResults.push_back( 0.165834 );
	correctResults.push_back( 0.157111 );
	correctResults.push_back( 0.164637 );
	correctResults.push_back( 0.157111 );
	correctResults.push_back( 0.165834 );
	correctResults.push_back( 0.212216 );
	correctResults.push_back( 0.165834 );
	correctResults.push_back( 0.157111 );
	correctResults.push_back( 0.210750 );
	correctResults.push_back( 0.221364 );
	correctResults.push_back( 0.210750 );
	correctResults.push_back( 0.221364 );
	correctResults.push_back( 0.210750 );
	correctResults.push_back( 0.221364 );
	correctResults.push_back( 0.210750 );
	correctResults.push_back( 0.221364 );

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

	correctResults.push_back( 5.8191700 );
	correctResults.push_back( 6.1095500 );
	correctResults.push_back( 6.1088000 );
	correctResults.push_back( 6.0931600 );
	correctResults.push_back( 6.1088000 );
	correctResults.push_back( 6.1095500 );
	correctResults.push_back( 6.1088000 );
	correctResults.push_back( 6.0931600 );
	correctResults.push_back( 6.1088000 );
	correctResults.push_back( 5.1361400 );
	correctResults.push_back( 5.1611500 );
	correctResults.push_back( 5.0374400 );
	correctResults.push_back( 4.8257500 );
	correctResults.push_back( 5.0374400 );
	correctResults.push_back( 5.1611500 );
	correctResults.push_back( 5.1361400 );
	correctResults.push_back( 5.1611500 );
	correctResults.push_back( 5.0374400 );
	correctResults.push_back( 4.8257500 );
	correctResults.push_back( 5.0374400 );
	correctResults.push_back( 5.1611500 );
	correctResults.push_back( 3.8068200 );
	correctResults.push_back( 3.9976100 );
	correctResults.push_back( 3.4129600 );
	correctResults.push_back( 1.3784400 );
	correctResults.push_back( 3.4129600 );
	correctResults.push_back( 3.9976100 );
	correctResults.push_back( 3.8068200 );
	correctResults.push_back( 3.9976100 );
	correctResults.push_back( 3.4129600 );
	correctResults.push_back( 1.3784400 );
	correctResults.push_back( 3.4129600 );
	correctResults.push_back( 3.9976100 );
	correctResults.push_back( 0.0947343 );
	correctResults.push_back( 0.0871753 );
	correctResults.push_back( 0.0947343 );
	correctResults.push_back( 0.0871753 );
	correctResults.push_back( 0.0947343 );
	correctResults.push_back( 0.0871753 );
	correctResults.push_back( 0.0947343 );
	correctResults.push_back( 0.0871753 );

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

	correctResults.push_back( 0.103298 );
	correctResults.push_back( 0.105482 );
	correctResults.push_back( 0.105492 );
	correctResults.push_back( 0.105706 );
	correctResults.push_back( 0.105492 );
	correctResults.push_back( 0.105482 );
	correctResults.push_back( 0.105492 );
	correctResults.push_back( 0.105706 );
	correctResults.push_back( 0.105492 );
	correctResults.push_back( 0.113316 );
	correctResults.push_back( 0.112946 );
	correctResults.push_back( 0.114772 );
	correctResults.push_back( 0.117899 );
	correctResults.push_back( 0.114772 );
	correctResults.push_back( 0.112946 );
	correctResults.push_back( 0.113316 );
	correctResults.push_back( 0.112946 );
	correctResults.push_back( 0.114772 );
	correctResults.push_back( 0.117899 );
	correctResults.push_back( 0.114772 );
	correctResults.push_back( 0.112946 );
	correctResults.push_back( 0.125265 );
	correctResults.push_back( 0.119630 );
	correctResults.push_back( 0.126232 );
	correctResults.push_back( 0.161125 );
	correctResults.push_back( 0.126232 );
	correctResults.push_back( 0.119630 );
	correctResults.push_back( 0.125265 );
	correctResults.push_back( 0.119630 );
	correctResults.push_back( 0.126232 );
	correctResults.push_back( 0.161125 );
	correctResults.push_back( 0.126232 );
	correctResults.push_back( 0.119630 );
	correctResults.push_back( 0.159914 );
	correctResults.push_back( 0.167473 );
	correctResults.push_back( 0.159914 );
	correctResults.push_back( 0.167473 );
	correctResults.push_back( 0.159914 );
	correctResults.push_back( 0.167473 );
	correctResults.push_back( 0.159914 );
	correctResults.push_back( 0.167473 );

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

	correctResults.push_back( 5.786840 );
	correctResults.push_back( 6.076510 );
	correctResults.push_back( 6.075760 );
	correctResults.push_back( 6.060040 );
	correctResults.push_back( 6.075760 );
	correctResults.push_back( 6.076510 );
	correctResults.push_back( 6.075760 );
	correctResults.push_back( 6.060040 );
	correctResults.push_back( 6.075760 );
	correctResults.push_back( 5.100530 );
	correctResults.push_back( 5.125660 );
	correctResults.push_back( 5.001370 );
	correctResults.push_back( 4.788650 );
	correctResults.push_back( 5.001370 );
	correctResults.push_back( 5.125660 );
	correctResults.push_back( 5.100530 );
	correctResults.push_back( 5.125660 );
	correctResults.push_back( 5.001370 );
	correctResults.push_back( 4.788650 );
	correctResults.push_back( 5.001370 );
	correctResults.push_back( 5.125660 );
	correctResults.push_back( 3.767450 );
	correctResults.push_back( 3.960130 );
	correctResults.push_back( 3.373360 );
	correctResults.push_back( 1.327340 );
	correctResults.push_back( 3.373360 );
	correctResults.push_back( 3.960130 );
	correctResults.push_back( 3.767450 );
	correctResults.push_back( 3.960130 );
	correctResults.push_back( 3.373360 );
	correctResults.push_back( 1.327340 );
	correctResults.push_back( 3.373360 );
	correctResults.push_back( 3.960130 );
	correctResults.push_back( 0.043898 );
	correctResults.push_back( 0.033284 );
	correctResults.push_back( 0.043898 );
	correctResults.push_back( 0.033284 );
	correctResults.push_back( 0.043898 );
	correctResults.push_back( 0.033284 );
	correctResults.push_back( 0.043898 );
	correctResults.push_back( 0.033284 );

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

	correctResults.push_back( 0.135627 );
	correctResults.push_back( 0.138525 );
	correctResults.push_back( 0.138539 );
	correctResults.push_back( 0.138823 );
	correctResults.push_back( 0.138539 );
	correctResults.push_back( 0.138525 );
	correctResults.push_back( 0.138539 );
	correctResults.push_back( 0.138823 );
	correctResults.push_back( 0.138539 );
	correctResults.push_back( 0.148919 );
	correctResults.push_back( 0.148428 );
	correctResults.push_back( 0.150852 );
	correctResults.push_back( 0.155000 );
	correctResults.push_back( 0.150852 );
	correctResults.push_back( 0.148428 );
	correctResults.push_back( 0.148919 );
	correctResults.push_back( 0.148428 );
	correctResults.push_back( 0.150852 );
	correctResults.push_back( 0.155000 );
	correctResults.push_back( 0.150852 );
	correctResults.push_back( 0.148428 );
	correctResults.push_back( 0.164637 );
	correctResults.push_back( 0.157111 );
	correctResults.push_back( 0.165834 );
	correctResults.push_back( 0.212216 );
	correctResults.push_back( 0.165834 );
	correctResults.push_back( 0.157111 );
	correctResults.push_back( 0.164637 );
	correctResults.push_back( 0.157111 );
	correctResults.push_back( 0.165834 );
	correctResults.push_back( 0.212216 );
	correctResults.push_back( 0.165834 );
	correctResults.push_back( 0.157111 );
	correctResults.push_back( 0.210750 );
	correctResults.push_back( 0.221364 );
	correctResults.push_back( 0.210750 );
	correctResults.push_back( 0.221364 );
	correctResults.push_back( 0.210750 );
	correctResults.push_back( 0.221364 );
	correctResults.push_back( 0.210750 );
	correctResults.push_back( 0.221364 );

	for ( size_t i = 0; i < size; ++i ) {
		calculatedResults.push_back( ( *aRf )[ i ][ i ] );
	}

	EXPECT_EQ( correctResults.size(), calculatedResults.size() );
	for ( size_t i = 0; i < size; ++i ) {
		EXPECT_NEAR( correctResults[i], calculatedResults[i], 1e-5 );
	}

}
