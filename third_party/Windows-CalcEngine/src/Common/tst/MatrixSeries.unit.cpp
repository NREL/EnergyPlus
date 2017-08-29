#include <memory>
#include <vector>
#include <stdexcept>
#include <gtest/gtest.h>

#include "WCECommon.hpp"

using namespace std;
using namespace FenestrationCommon;

class TestMatrixSeries : public testing::Test {
private:
	std::shared_ptr< CMatrixSeries > m_MatrixSeries;

protected:
	void SetUp() override {
		// Create some number for matrix series to be tested
		vector< CSquareMatrix > Mat;
		vector< double > wl;
		CSquareMatrix A = CSquareMatrix( 2 );
		A[ 0 ] = { 2.8, 3.4 };
		A[ 1 ] = { 3.9, 7.5 };
		Mat.push_back( A );
		wl.push_back( 0.45 );

		A[ 0 ] = { 7.4, 9.6 };
		A[ 1 ] = { 7.7, 1.3 };
		Mat.push_back( A );
		wl.push_back( 0.50 );

		A[ 0 ] = { 8.3, 0.1 };
		A[ 1 ] = { 2.2, 3.6 };
		Mat.push_back( A );
		wl.push_back( 0.55 );

		A[ 0 ] = { 1.5, 9.3 };
		A[ 1 ] = { 9.0, 7.4 };
		Mat.push_back( A );
		wl.push_back( 0.60 );

		// Fill up matrix series
		m_MatrixSeries = make_shared< CMatrixSeries >( 2, 2 );
		for ( size_t i = 0; i < wl.size(); ++i ) {
			m_MatrixSeries->addProperties( wl[ i ], Mat[ i ] );
		}
	}

public:
	std::shared_ptr< CMatrixSeries > getMatrix() const {
		return m_MatrixSeries;
	}


};

TEST_F( TestMatrixSeries, Test1 ) {
	SCOPED_TRACE( "Begin Test: Test matrix series sum." );

	CMatrixSeries& aMat = *getMatrix();

	double minLambda = 0.45;
	double maxLambda = 0.65;
	vector< double > scaleFactors = { 1, 1, 1, 1 };
	
	// Note that you need to increase maxLambda from 0.6 to 0.65 in order to include last range into sum.
	// Inserting 0.6 would mean that last range to be included into sum is from 0.55 to 0.6 and last range is
	// from 0.6 to infinity.
	CSquareMatrix Mat = *aMat.getSquaredMatrixSums( minLambda, maxLambda, scaleFactors );
	
	CSquareMatrix correctResults = CSquareMatrix( 2 );
	correctResults[ 0 ] = { 20, 22.4 };
	correctResults[ 1 ] = { 22.8, 19.8 };
	
	EXPECT_EQ( correctResults.getSize(), Mat.getSize() );
	
	for ( size_t i = 0; i < Mat.getSize(); ++i ) {
		for ( size_t j = 0; j < Mat.getSize(); ++j ) {
			EXPECT_NEAR( correctResults[ i ][ j ], Mat[ i ][ j ], 1e-6 );
		}
	}

}

TEST_F( TestMatrixSeries, Test2 ) {
	SCOPED_TRACE( "Begin Test: Test matrix series multiplication." );

	CMatrixSeries& Mat = *getMatrix();
	
	CSeries multiplier;
	multiplier.addProperty( 0.45, 1.6 );
	multiplier.addProperty( 0.50, 3.8 );
	multiplier.addProperty( 0.55, 2.4 );
	multiplier.addProperty( 0.60, 8.3 );
	
	Mat.mMult( multiplier );
	
	vector< std::vector< double > > correctResults( 4 );
	correctResults[ 0 ] = { 4.48, 28.12, 19.92, 12.45 };
	correctResults[ 1 ] = { 5.44, 36.48, 0.24, 77.19 };
	correctResults[ 2 ] = { 6.24, 29.26, 5.28, 74.7 };
	correctResults[ 3 ] = { 12, 4.94, 8.64, 61.42 };
	
	vector< CSeries* > matrixResults;

	for ( size_t i = 0; i < Mat.size1(); ++i ) {
		for ( size_t j = 0; j < Mat.size2(); ++j ) {
			auto aSeries = Mat[ i ][ j ].get();
			matrixResults.push_back( aSeries );
		}
	}

	// for ( size_t i = 0; i < matrixResults.size(); ++i ) {
	// 	for ( size_t k = 0; k < matrixResults[ i ]->size(); ++k ) {
	// 		EXPECT_NEAR( correctResults[ i ][ k ], ( *matrixResults[ i ] )[ k ]->value(), 1e-6 );
	// 	}
	// }

}
