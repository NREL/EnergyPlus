#include <memory>
#include <gtest/gtest.h>

#include "WCEMultiLayerOptics.hpp"
#include "WCESingleLayerOptics.hpp"
#include "WCECommon.hpp"


using namespace FenestrationCommon;
using namespace SingleLayerOptics;
using namespace MultiLayerOptics;

// Example that tests interreflectance between two adjacent layers. This procedure will be used to
// calculate other multilayer properties
class TestInterReflectanceBSDF : public testing::Test {

private:
	std::shared_ptr< CInterReflectance > m_InterReflectance;

protected:
	virtual void SetUp() {

		// Create lambda matrix
		std::vector< CBSDFDefinition > aDefinitions;
		aDefinitions.push_back( CBSDFDefinition( 0, 1 ) );
		aDefinitions.push_back( CBSDFDefinition( 15, 1 ) );
		aDefinitions.push_back( CBSDFDefinition( 30, 1 ) );
		aDefinitions.push_back( CBSDFDefinition( 45, 1 ) );
		aDefinitions.push_back( CBSDFDefinition( 60, 1 ) );
		aDefinitions.push_back( CBSDFDefinition( 75, 1 ) );
		aDefinitions.push_back( CBSDFDefinition( 86.25, 1 ) );

		CBSDFDirections aDirections = CBSDFDirections( aDefinitions, BSDFHemisphere::Incoming );
		CSquareMatrix aLambdas = *aDirections.lambdaMatrix();

		size_t size = 7;

		CSquareMatrix Rb = CSquareMatrix( size );
		Rb[ 0 ] = { 1.438618083, 0, 0, 0, 0, 0, 0 };
		Rb[ 1 ] = { 0, 0.189397664, 0, 0, 0, 0, 0 };
		Rb[ 2 ] = { 0, 0, 0.112189021, 0, 0, 0, 0 };
		Rb[ 3 ] = { 0, 0, 0, 0.114376511, 0, 0, 0 };
		Rb[ 4 ] = { 0, 0, 0, 0, 0.207336671, 0, 0 };
		Rb[ 5 ] = { 0, 0, 0, 0, 0, 0.951907739, 0 };
		Rb[ 6 ] = { 0, 0, 0, 0, 0, 0, 15.28298172 };

		CSquareMatrix Rf = CSquareMatrix( size );
		Rf[ 0 ] = { 1.438618083, 0, 0, 0, 0, 0, 0 };
		Rf[ 1 ] = { 0, 0.189397664, 0, 0, 0, 0, 0 };
		Rf[ 2 ] = { 0, 0, 0.112189021, 0, 0, 0, 0 };
		Rf[ 3 ] = { 0, 0, 0, 0.114376511, 0, 0, 0 };
		Rf[ 4 ] = { 0, 0, 0, 0, 0.207336671, 0, 0 };
		Rf[ 5 ] = { 0, 0, 0, 0, 0, 0.951907739, 0 };
		Rf[ 6 ] = { 0, 0, 0, 0, 0, 0, 15.28298172 };

		m_InterReflectance = std::make_shared< CInterReflectance >( aLambdas, Rb, Rf );

	}

public:
	std::shared_ptr< CInterReflectance > getInterReflectance() {
		return m_InterReflectance;
	};

};

TEST_F( TestInterReflectanceBSDF, TestBSDFInterreflectance ) {
	SCOPED_TRACE( "Begin Test: Simple BSDF interreflectance." );

	CInterReflectance interRefl = *getInterReflectance();

	CSquareMatrix results = *interRefl.value();

	size_t matrixSize = results.getSize();

	// Test matrix
	size_t size = 7;

	EXPECT_EQ( size, matrixSize );

	CSquareMatrix correctResults = CSquareMatrix( size );
	correctResults[ 0 ] = { 1.005964363, 0, 0, 0, 0, 0, 0 };
	correctResults[ 1 ] = { 0, 1.005964363, 0, 0, 0, 0, 0 };
	correctResults[ 2 ] = { 0, 0, 1.006280195, 0, 0, 0, 0 };
	correctResults[ 3 ] = { 0, 0, 0, 1.008724458, 0, 0, 0 };
	correctResults[ 4 ] = { 0, 0, 0, 0, 1.021780268, 0, 0 };
	correctResults[ 5 ] = { 0, 0, 0, 0, 0, 1.176150952, 0 };
	correctResults[ 6 ] = { 0, 0, 0, 0, 0, 0, 3.022280250 };

	for ( size_t i = 0; i < size; ++i ) {
		for ( size_t j = 0; j < size; ++j ) {
			EXPECT_NEAR( correctResults[ i ][ j ], results[ i ][ j ], 1e-6 );
		}
	}

}
