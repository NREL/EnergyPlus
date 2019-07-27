#include <memory>
#include <gtest/gtest.h>

#include "WCEViewer.hpp"
#include "WCESingleLayerOptics.hpp"
#include "WCECommon.hpp"


using namespace SingleLayerOptics;
using namespace Viewer;
using namespace FenestrationCommon;

class TestVenetianCellDescriptionCurvedMinus55 : public testing::Test {

private:
	std::shared_ptr< CVenetianCellDescription > m_Cell;
	// size_t m_NumOfSlats;

protected:
	virtual void SetUp() {
		// make cell geometry
		double slatWidth = 0.076200; // m
		double slatSpacing = 0.057150; // m
		double slatTiltAngle = -55.000000;
		double curvatureRadius = 0.123967;
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

TEST_F( TestVenetianCellDescriptionCurvedMinus55, TestVenetian1 ) {
	SCOPED_TRACE( "Begin Test: Venetian cell (Curved, -55 degrees slats)." );

	std::shared_ptr< CVenetianCellDescription > aCell = GetCell();

	const size_t size = aCell->numberOfSegments();

	std::shared_ptr< CSquareMatrix > viewFactors = aCell->viewFactors();

	// Fill up correct results
	CSquareMatrix correctResults = CSquareMatrix( size );
	correctResults[ 0 ] = { 0.000000, 0.489698, 0.291815, 0.157211, 0.002610, 0.058462 };
	correctResults[ 1 ] = { 0.725605, 0.000000, 0.012174, 0.047629, 0.026822, 0.187769 };
	correctResults[ 2 ] = { 0.432393, 0.012174, 0.000000, 0.030708, 0.093504, 0.431220 };
	correctResults[ 3 ] = { 0.157211, 0.032144, 0.020724, 0.000000, 0.591054, 0.198676 };
	correctResults[ 4 ] = { 0.003867, 0.026822, 0.093505, 0.875787, 0.000000, 0.000000 };
	correctResults[ 5 ] = { 0.086626, 0.187769, 0.431220, 0.294386, 0.000000, 0.000000 };

	for ( size_t i = 0; i < size; ++i ) {
		for ( size_t j = 0; j < size; ++j ) {
			EXPECT_NEAR( correctResults[i][j], (*viewFactors)[i][j], 1e-6 );
		}
	}

}

TEST_F( TestVenetianCellDescriptionCurvedMinus55, TestVenetian2 ) {
	SCOPED_TRACE( "Begin Test: Venetian cell (Curved, -55 degrees slats) - Direct-direct component (0, 0)." );

	std::shared_ptr< CVenetianCellDescription > aCell = GetCell();

	CBeamDirection aDirection = CBeamDirection( 0, 0 );

	double Tdir_dir = aCell->T_dir_dir( Side::Front, aDirection );

	EXPECT_NEAR( 0, Tdir_dir, 1e-6 );

}

TEST_F( TestVenetianCellDescriptionCurvedMinus55, TestVenetian3 ) {
	SCOPED_TRACE( "Begin Test: Venetian cell (Curved, -55 degrees slats) - Direct-direct component (18, -45)." );

	std::shared_ptr< CVenetianCellDescription > aCell = GetCell();

	CBeamDirection aDirection = CBeamDirection( 18, -45 );

	double Tdir_dir = aCell->T_dir_dir( Side::Front, aDirection );

	EXPECT_NEAR( 0.083505089496152846, Tdir_dir, 1e-6 );

}

TEST_F( TestVenetianCellDescriptionCurvedMinus55, TestVenetian4 ) {
	SCOPED_TRACE( "Begin Test: Venetian cell (Curved, -55 degrees slats) - Direct-direct component (18, -90)." );

	std::shared_ptr< CVenetianCellDescription > aCell = GetCell();

	CBeamDirection aDirection = CBeamDirection( 18, -90 );

	double Tdir_dir = aCell->T_dir_dir( Side::Front, aDirection );

	EXPECT_NEAR( 0.15628564957180935, Tdir_dir, 1e-6 );

}

TEST_F( TestVenetianCellDescriptionCurvedMinus55, TestVenetian5 ) {
	SCOPED_TRACE( "Begin Test: Venetian cell (Curved, -55 degrees slats) - Direct-direct component (36, -30)." );

	std::shared_ptr< CVenetianCellDescription > aCell = GetCell();

	CBeamDirection aDirection = CBeamDirection( 36, -30 );

	double Tdir_dir = aCell->T_dir_dir( Side::Front, aDirection );

	EXPECT_NEAR( 0.18561572366619078, Tdir_dir, 1e-6 );

}

TEST_F( TestVenetianCellDescriptionCurvedMinus55, TestVenetian6 ) {
	SCOPED_TRACE( "Begin Test: Venetian cell (Curved, -55 degrees slats) - Direct-direct component (36, -60)." );

	std::shared_ptr< CVenetianCellDescription > aCell = GetCell();

	CBeamDirection aDirection = CBeamDirection( 36, -60 );

	double Tdir_dir = aCell->T_dir_dir( Side::Front, aDirection );

	EXPECT_NEAR( 0.38899294389431732, Tdir_dir, 1e-6 );

}

TEST_F( TestVenetianCellDescriptionCurvedMinus55, TestVenetian7 ) {
	SCOPED_TRACE( "Begin Test: Venetian cell (Curved, -55 degrees slats) - Direct-direct component (54, -30)." );

	std::shared_ptr< CVenetianCellDescription > aCell = GetCell();

	CBeamDirection aDirection = CBeamDirection( 54, -30 );

	double Tdir_dir = aCell->T_dir_dir( Side::Front, aDirection );

	EXPECT_NEAR( 0.43410409895720364, Tdir_dir, 1e-6 );

}

TEST_F( TestVenetianCellDescriptionCurvedMinus55, TestVenetian8 ) {
	SCOPED_TRACE( "Begin Test: Venetian cell (Curved, -55 degrees slats) - Direct-direct component (36, -90)." );

	std::shared_ptr< CVenetianCellDescription > aCell = GetCell();

	CBeamDirection aDirection = CBeamDirection( 36, -90 );

	double Tdir_dir = aCell->T_dir_dir( Side::Front, aDirection );

	EXPECT_NEAR( 0.46343417304788243, Tdir_dir, 1e-6 );

}

TEST_F( TestVenetianCellDescriptionCurvedMinus55, TestVenetian9 ) {
	SCOPED_TRACE( "Begin Test: Venetian cell (Curved, -55 degrees slats) - Direct-direct component (54, -60)." );

	std::shared_ptr< CVenetianCellDescription > aCell = GetCell();

	CBeamDirection aDirection = CBeamDirection( 54, -60 );

	double Tdir_dir = aCell->T_dir_dir( Side::Front, aDirection );

	EXPECT_NEAR( 0.74696434645194043, Tdir_dir, 1e-6 );

}

TEST_F( TestVenetianCellDescriptionCurvedMinus55, TestVenetian10 ) {
	SCOPED_TRACE( "Begin Test: Venetian cell (Curved, -55 degrees slats) - Direct-direct component (54, -90)." );

	std::shared_ptr< CVenetianCellDescription > aCell = GetCell();

	CBeamDirection aDirection = CBeamDirection( 54, -90 );

	double Tdir_dir = aCell->T_dir_dir( Side::Front, aDirection );

	EXPECT_NEAR( 0.80161756574503285, Tdir_dir, 1e-6 );

}

TEST_F( TestVenetianCellDescriptionCurvedMinus55, TestVenetian11 ) {
	SCOPED_TRACE( "Begin Test: Venetian cell (Curved, -55 degrees slats) - Direct-direct component (76.5, -45)." );

	std::shared_ptr< CVenetianCellDescription > aCell = GetCell();

	CBeamDirection aDirection = CBeamDirection( 76.5, -45 );

	double Tdir_dir = aCell->T_dir_dir( Side::Front, aDirection );

	EXPECT_NEAR( 0, Tdir_dir, 1e-6 );

}
