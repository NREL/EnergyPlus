#include <memory>
#include <gtest/gtest.h>

#include "WCEViewer.hpp"
#include "WCESingleLayerOptics.hpp"
#include "WCECommon.hpp"


using namespace SingleLayerOptics;
using namespace Viewer;
using namespace FenestrationCommon;

class TestVenetianCellDescriptionFlat45 : public testing::Test {

private:
	std::shared_ptr< CVenetianCellDescription > m_Cell;
	// size_t m_NumOfSlats;

protected:
	virtual void SetUp() {
		// make cell geometry
		double slatWidth = 0.020; // m
		double slatSpacing = 0.010; // m
		double slatTiltAngle = 45;
		double curvatureRadius = 0;
		size_t aNumOfSlats = 2;

		m_Cell = std::make_shared< CVenetianCellDescription >( slatWidth, slatSpacing, slatTiltAngle,
		                                                  curvatureRadius, aNumOfSlats );
	}

public:
	std::shared_ptr< CVenetianCellDescription > GetCell() {
		return m_Cell;
	};
	// size_t numOfSlats() { return m_NumOfSlats; };

};

TEST_F( TestVenetianCellDescriptionFlat45, TestVenetian1 ) {
	SCOPED_TRACE( "Begin Test: Venetian cell (Flat, 45 degrees slats) - View Factors." );

	std::shared_ptr< CVenetianCellDescription > aCell = GetCell();

	const size_t size = aCell->numberOfSegments();

	std::shared_ptr< CSquareMatrix > viewFactors = aCell->viewFactors();

	// Fill up correct results
	CSquareMatrix correctResults = CSquareMatrix( size );
	correctResults[ 0 ] = { 0.000000, 0.076120, 0.024913, 0.135779, 0.145871, 0.617317 };
	correctResults[ 1 ] = { 0.076120, 0.000000, 0.000000, 0.145871, 0.471446, 0.306563 };
	correctResults[ 2 ] = { 0.024913, 0.000000, 0.000000, 0.617317, 0.306563, 0.051207 };
	correctResults[ 3 ] = { 0.135779, 0.145871, 0.617317, 0.000000, 0.076120, 0.024913 };
	correctResults[ 4 ] = { 0.145871, 0.471446, 0.306563, 0.076120, 0.000000, 0.000000 };
	correctResults[ 5 ] = { 0.617317, 0.306563, 0.051207, 0.024913, 0.000000, 0.000000 };

	for ( size_t i = 0; i < size; ++i ) {
		for ( size_t j = 0; j < size; ++j ) {
			EXPECT_NEAR( correctResults[i][j], (*viewFactors)[i][j], 1e-6 );
		}
	}

}

TEST_F( TestVenetianCellDescriptionFlat45, TestVenetian2 ) {
	SCOPED_TRACE( "Begin Test: Venetian cell (Flat, 45 degrees slats) - Direct-direct component (0, 0)." );

	std::shared_ptr< CVenetianCellDescription > aCell = GetCell();

	CBeamDirection aDirection = CBeamDirection( 0, 0 );

	double Tdir_dir = aCell->T_dir_dir( Side::Front, aDirection );

	EXPECT_NEAR( 0, Tdir_dir, 1e-6 );

}

TEST_F( TestVenetianCellDescriptionFlat45, TestVenetian3 ) {
	SCOPED_TRACE( "Begin Test: Venetian cell (Flat, 45 degrees slats) - Direct-direct component (45, 90)." );

	std::shared_ptr< CVenetianCellDescription > aCell = GetCell();

	CBeamDirection aDirection = CBeamDirection( 45, 90 );

	double Tdir_dir = aCell->T_dir_dir( Side::Front, aDirection );

	EXPECT_NEAR( 1, Tdir_dir, 1e-6 );

}

TEST_F( TestVenetianCellDescriptionFlat45, TestVenetian4 ) {
	SCOPED_TRACE( "Begin Test: Venetian cell (Flat, 45 degrees slats) - Direct-direct component (76.5, 90)." );

	std::shared_ptr< CVenetianCellDescription > aCell = GetCell();

	CBeamDirection aDirection = CBeamDirection( 76.5, 90 );

	double Tdir_dir = aCell->T_dir_dir( Side::Front, aDirection );

	EXPECT_NEAR( 0, Tdir_dir, 1e-6 );

}

TEST_F( TestVenetianCellDescriptionFlat45, TestVenetian5 ) {
	SCOPED_TRACE( "Begin Test: Venetian cell (Flat, 45 degrees slats) - Direct-direct component (76.5, 45)." );

	std::shared_ptr< CVenetianCellDescription > aCell = GetCell();

	CBeamDirection aDirection = CBeamDirection( 76.5, 45 );

	double Tdir_dir = aCell->T_dir_dir( Side::Front, aDirection );

	EXPECT_NEAR( 0, Tdir_dir, 1e-6 );

}

TEST_F( TestVenetianCellDescriptionFlat45, TestVenetian6 ) {
	SCOPED_TRACE( "Begin Test: Venetian cell (Flat, 45 degrees slats) - Direct-direct component (54, 90)." );

	std::shared_ptr< CVenetianCellDescription > aCell = GetCell();

	CBeamDirection aDirection = CBeamDirection( 54, 90 );

	double Tdir_dir = aCell->T_dir_dir( Side::Front, aDirection );

	EXPECT_NEAR( 0.46771558343367653, Tdir_dir, 1e-6 );

}

TEST_F( TestVenetianCellDescriptionFlat45, TestVenetian7 ) {
	SCOPED_TRACE( "Begin Test: Venetian cell (Flat, 45 degrees slats) - Direct-direct component (54, 60)." );

	std::shared_ptr< CVenetianCellDescription > aCell = GetCell();

	CBeamDirection aDirection = CBeamDirection( 54, 60 );

	double Tdir_dir = aCell->T_dir_dir( Side::Front, aDirection );

	EXPECT_NEAR( 0.72849686418372972, Tdir_dir, 1e-6 );

}

TEST_F( TestVenetianCellDescriptionFlat45, TestVenetian8 ) {
	SCOPED_TRACE( "Begin Test: Venetian cell (Flat, 45 degrees slats) - Direct-direct component (36, 90)." );

	std::shared_ptr< CVenetianCellDescription > aCell = GetCell();

	CBeamDirection aDirection = CBeamDirection( 36, 90 );

	double Tdir_dir = aCell->T_dir_dir( Side::Front, aDirection );

	EXPECT_NEAR( 0.61327273437110064, Tdir_dir, 1e-6 );

}

TEST_F( TestVenetianCellDescriptionFlat45, TestVenetian9 ) {
	SCOPED_TRACE( "Begin Test: Venetian cell (Flat, 45 degrees slats) - Direct-direct component (54, 30)." );

	std::shared_ptr< CVenetianCellDescription > aCell = GetCell();

	CBeamDirection aDirection = CBeamDirection( 54, 30 );

	double Tdir_dir = aCell->T_dir_dir( Side::Front, aDirection );

	EXPECT_NEAR( 0.55903542711488330, Tdir_dir, 1e-6 );

}

TEST_F( TestVenetianCellDescriptionFlat45, TestVenetian10 ) {
	SCOPED_TRACE( "Begin Test: Venetian cell (Flat, 45 degrees slats) - Direct-direct component (36, 60)." );

	std::shared_ptr< CVenetianCellDescription > aCell = GetCell();

	CBeamDirection aDirection = CBeamDirection( 36, 60 );

	double Tdir_dir = aCell->T_dir_dir( Side::Front, aDirection );

	EXPECT_NEAR( 0.47561567265428234, Tdir_dir, 1e-6 );

}

TEST_F( TestVenetianCellDescriptionFlat45, TestVenetian11 ) {
	SCOPED_TRACE( "Begin Test: Venetian cell (Flat, 45 degrees slats) - Direct-direct component (36, 30)." );

	std::shared_ptr< CVenetianCellDescription > aCell = GetCell();

	CBeamDirection aDirection = CBeamDirection( 36, 30 );

	double Tdir_dir = aCell->T_dir_dir( Side::Front, aDirection );

	EXPECT_NEAR( 0.099529586007794477, Tdir_dir, 1e-6 );

}
