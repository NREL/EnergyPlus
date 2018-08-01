#include <memory>
#include <gtest/gtest.h>

#include <memory>

#include "WCEViewer.hpp"


using namespace Viewer;

class TestSegment2DIntersectionWithLine : public testing::Test {

protected:
	virtual void SetUp() {

	}

};

TEST_F( TestSegment2DIntersectionWithLine, Segment2DTest1 ) {
	SCOPED_TRACE( "Begin Test: Segment 2D - intersection point (1)." );

	std::shared_ptr< CPoint2D > aStartPoint1 = std::make_shared< CPoint2D >( 0, 0 );
	std::shared_ptr< CPoint2D > aEndPoint1 = std::make_shared< CPoint2D >( 10, 10 );

	std::shared_ptr< CSegment2D > aSegment1 = std::make_shared< CSegment2D >( aStartPoint1, aEndPoint1 );

	std::shared_ptr< CPoint2D > aStartPoint2 = std::make_shared< CPoint2D >( 1, 0 );
	std::shared_ptr< CPoint2D > aEndPoint2 = std::make_shared< CPoint2D >( 10, 10 );

	std::shared_ptr< CSegment2D > aSegment2 = std::make_shared< CSegment2D >( aStartPoint2, aEndPoint2 );

	IntersectionStatus isInt = aSegment1->intersectionWithLine( aSegment2 );

	EXPECT_EQ( IntersectionStatus::Point, isInt );

}

TEST_F( TestSegment2DIntersectionWithLine, Segment2DTest2 ) {
	SCOPED_TRACE( "Begin Test: Segment 2D - intersection point (2)." );

	std::shared_ptr< CPoint2D > aStartPoint1 = std::make_shared< CPoint2D >( 4, 2 );
	std::shared_ptr< CPoint2D > aEndPoint1 = std::make_shared< CPoint2D >( 8, 1 );

	std::shared_ptr< CSegment2D > aSegment1 = std::make_shared< CSegment2D >( aStartPoint1, aEndPoint1 );

	std::shared_ptr< CPoint2D > aStartPoint2 = std::make_shared< CPoint2D >( 1, 3 );
	std::shared_ptr< CPoint2D > aEndPoint2 = std::make_shared< CPoint2D >( 5, 7 );

	std::shared_ptr< CSegment2D > aSegment2 = std::make_shared< CSegment2D >( aStartPoint2, aEndPoint2 );

	IntersectionStatus isInt = aSegment1->intersectionWithLine( aSegment2 );

	EXPECT_EQ( IntersectionStatus::No, isInt );

}

TEST_F( TestSegment2DIntersectionWithLine, Segment2DTest3 ) {
	SCOPED_TRACE( "Begin Test: Segment 2D - parallel lines (no intersection)." );

	std::shared_ptr< CPoint2D > aStartPoint1 = std::make_shared< CPoint2D >( 0, 0 );
	std::shared_ptr< CPoint2D > aEndPoint1 = std::make_shared< CPoint2D >( 0, 1 );

	std::shared_ptr< CSegment2D > aSegment1 = std::make_shared< CSegment2D >( aStartPoint1, aEndPoint1 );

	std::shared_ptr< CPoint2D > aStartPoint2 = std::make_shared< CPoint2D >( 1, 0 );
	std::shared_ptr< CPoint2D > aEndPoint2 = std::make_shared< CPoint2D >( 1, 1 );

	std::shared_ptr< CSegment2D > aSegment2 = std::make_shared< CSegment2D >( aStartPoint2, aEndPoint2 );

	IntersectionStatus isInt = aSegment1->intersectionWithLine( aSegment2 );

	EXPECT_EQ( IntersectionStatus::No, isInt );

}

TEST_F( TestSegment2DIntersectionWithLine, Segment2DTest4 ) {
	SCOPED_TRACE( "Begin Test: Segment 2D - parallel lines (Total overlap)." );

	std::shared_ptr< CPoint2D > aStartPoint1 = std::make_shared< CPoint2D >( 0, 0 );
	std::shared_ptr< CPoint2D > aEndPoint1 = std::make_shared< CPoint2D >( 0, 1 );

	std::shared_ptr< CSegment2D > aSegment1 = std::make_shared< CSegment2D >( aStartPoint1, aEndPoint1 );

	std::shared_ptr< CPoint2D > aStartPoint2 = std::make_shared< CPoint2D >( 0, 0 );
	std::shared_ptr< CPoint2D > aEndPoint2 = std::make_shared< CPoint2D >( 0, 2 );

	std::shared_ptr< CSegment2D > aSegment2 = std::make_shared< CSegment2D >( aStartPoint2, aEndPoint2 );

	IntersectionStatus isInt = aSegment1->intersectionWithLine( aSegment2 );

	EXPECT_EQ( IntersectionStatus::No, isInt );

}

TEST_F( TestSegment2DIntersectionWithLine, Segment2DTest5 ) {
	SCOPED_TRACE( "Begin Test: Segment 2D - parallel lines (Total overlap - different directions)." );

	std::shared_ptr< CPoint2D > aStartPoint1 = std::make_shared< CPoint2D >( 0, 0 );
	std::shared_ptr< CPoint2D > aEndPoint1 = std::make_shared< CPoint2D >( 0, 1 );

	std::shared_ptr< CSegment2D > aSegment1 = std::make_shared< CSegment2D >( aStartPoint1, aEndPoint1 );

	std::shared_ptr< CPoint2D > aStartPoint2 = std::make_shared< CPoint2D >( 0, 2 );
	std::shared_ptr< CPoint2D > aEndPoint2 = std::make_shared< CPoint2D >( 0, 0 );

	std::shared_ptr< CSegment2D > aSegment2 = std::make_shared< CSegment2D >( aStartPoint2, aEndPoint2 );

	IntersectionStatus isInt = aSegment1->intersectionWithLine( aSegment2 );

	EXPECT_EQ( IntersectionStatus::No, isInt );

}

TEST_F( TestSegment2DIntersectionWithLine, Segment2DTest6 ) {
	SCOPED_TRACE( "Begin Test: Segment 2D - normal segments (Not touching but intersects on the segment)." );

	std::shared_ptr< CPoint2D > aStartPoint1 = std::make_shared< CPoint2D >( 0, 10 );
	std::shared_ptr< CPoint2D > aEndPoint1 = std::make_shared< CPoint2D >( 10, 0 );

	std::shared_ptr< CSegment2D > aSegment1 = std::make_shared< CSegment2D >( aStartPoint1, aEndPoint1 );

	std::shared_ptr< CPoint2D > aStartPoint2 = std::make_shared< CPoint2D >( 0, 0 );
	std::shared_ptr< CPoint2D > aEndPoint2 = std::make_shared< CPoint2D >( 1, 1 );

	std::shared_ptr< CSegment2D > aSegment2 = std::make_shared< CSegment2D >( aStartPoint2, aEndPoint2 );

	IntersectionStatus isInt = aSegment2->intersectionWithLine( aSegment1 );

	EXPECT_EQ( IntersectionStatus::Segment, isInt );

}
