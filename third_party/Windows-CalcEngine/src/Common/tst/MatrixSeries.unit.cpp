#include <memory>
#include <vector>
#include <gtest/gtest.h>

#include "WCECommon.hpp"

using namespace FenestrationCommon;

class TestMatrixSeries : public testing::Test {
private:
	std::shared_ptr< CMatrixSeries > m_MatrixSeries;

protected:
	void SetUp() override {
		// Create some number for matrix series to be tested
		std::vector< CSquareMatrix > mat;
		std::vector< double > wl;
		auto a = CSquareMatrix( 2 );
		a[ 0 ] = { 2.8, 3.4 };
		a[ 1 ] = { 3.9, 7.5 };
		mat.push_back( a );
		wl.push_back( 0.45 );

		a[ 0 ] = { 7.4, 9.6 };
		a[ 1 ] = { 7.7, 1.3 };
		mat.push_back( a );
		wl.push_back( 0.50 );

		a[ 0 ] = { 8.3, 0.1 };
		a[ 1 ] = { 2.2, 3.6 };
		mat.push_back( a );
		wl.push_back( 0.55 );

		a[ 0 ] = { 1.5, 9.3 };
		a[ 1 ] = { 9.0, 7.4 };
		mat.push_back( a );
		wl.push_back( 0.60 );

		// Fill up matrix series
		m_MatrixSeries = std::make_shared< CMatrixSeries >( 2, 2 );
		for ( size_t i = 0; i < wl.size(); ++i ) {
			m_MatrixSeries->addProperties( wl[ i ], mat[ i ] );
		}
	}

public:
	std::shared_ptr< CMatrixSeries > getMatrix() const {
		return m_MatrixSeries;
	}


};

TEST_F( TestMatrixSeries, Test1 ) {
	SCOPED_TRACE( "Begin Test: Test matrix series sum." );

	auto & aMat = *getMatrix();

	const double minLambda = 0.45;
	const double maxLambda = 0.65;
	const std::vector< double > scaleFactors = { 1, 1, 1, 1 };
	
	// Note that you need to increase maxLambda from 0.6 to 0.65 in order to include last range into sum.
	// Inserting 0.6 would mean that last range to be included into sum is from 0.55 to 0.6 and last range is
	// from 0.6 to infinity.
	auto mat = *aMat.getSquaredMatrixSums( minLambda, maxLambda, scaleFactors );

	auto correctResults = CSquareMatrix( 2 );
	correctResults[ 0 ] = { 20, 22.4 };
	correctResults[ 1 ] = { 22.8, 19.8 };
	
	EXPECT_EQ( correctResults.getSize(), mat.getSize() );
	
	for ( size_t i = 0; i < mat.getSize(); ++i ) {
		for ( size_t j = 0; j < mat.getSize(); ++j ) {
			EXPECT_NEAR( correctResults[ i ][ j ], mat[ i ][ j ], 1e-6 );
		}
	}

}

TEST_F( TestMatrixSeries, Test2 ) {
	SCOPED_TRACE( "Begin Test: Test matrix series multiplication." );

	auto & mat = *getMatrix();
	
	CSeries multiplier;
	multiplier.addProperty( 0.45, 1.6 );
	multiplier.addProperty( 0.50, 3.8 );
	multiplier.addProperty( 0.55, 2.4 );
	multiplier.addProperty( 0.60, 8.3 );
	
	mat.mMult( multiplier );

	std::vector< std::vector< double > > correctResults( 4 );
	correctResults[ 0 ] = { 4.48, 28.12, 19.92, 12.45 };
	correctResults[ 1 ] = { 5.44, 36.48, 0.24, 77.19 };
	correctResults[ 2 ] = { 6.24, 29.26, 5.28, 74.7 };
	correctResults[ 3 ] = { 12, 4.94, 8.64, 61.42 };
	
	std::vector< CSeries* > matrixResults;

	for ( size_t i = 0; i < mat.size1(); ++i ) {
		for ( size_t j = 0; j < mat.size2(); ++j ) {
			auto aSeries = mat[ i ][ j ].get();
			matrixResults.push_back( aSeries );
		}
	}

	for ( size_t i = 0; i < matrixResults.size(); ++i ) {
 		for ( size_t k = 0; k < matrixResults[ i ]->size(); ++k ) {
 			EXPECT_NEAR( correctResults[ i ][ k ], ( *matrixResults[ i ] )[ k ].value(), 1e-6 );
 		}
	}

}
