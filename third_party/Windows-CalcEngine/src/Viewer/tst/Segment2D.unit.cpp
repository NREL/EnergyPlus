#include <memory>
#include <gtest/gtest.h>

#include <memory>

#include "WCEViewer.hpp"


using namespace Viewer;

class TestSegment2D : public testing::Test {

protected:
	virtual void SetUp() {

	}

};

TEST_F( TestSegment2D, Segment2DTest1 ) {
	SCOPED_TRACE( "Begin Test: Segment 2D - length and normal (1)." );

	std::shared_ptr< CPoint2D > aStartPoint = std::make_shared< CPoint2D >( 0, 0 );
	std::shared_ptr< CPoint2D > aEndPoint = std::make_shared< CPoint2D >( 10, 0 );

	CSegment2D aSegment = CSegment2D( aStartPoint, aEndPoint );

	double length = aSegment.length();

	EXPECT_NEAR( 10, length, 1e-6 );
}

TEST_F( TestSegment2D, Segment2DTest2 ) {
	SCOPED_TRACE( "Begin Test: Segment 2D - length and normal (2)." );

	std::shared_ptr< CPoint2D > aStartPoint = std::make_shared< CPoint2D >( 0, 0 );
	std::shared_ptr< CPoint2D > aEndPoint = std::make_shared< CPoint2D >( 10, 10 );

	CSegment2D aSegment = CSegment2D( aStartPoint, aEndPoint );

	double length = aSegment.length();

	EXPECT_NEAR( 14.14213562, length, 1e-6 );
}
