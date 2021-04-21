#include <memory>
#include <gtest/gtest.h>

#include <memory>

#include "WCEViewer.hpp"


using namespace Viewer;

class TestSegment2DSelfShadowing : public testing::Test {

protected:
	void SetUp() override {

	}

};

TEST_F( TestSegment2DSelfShadowing, Segment2DNoShadowing ) {
	SCOPED_TRACE( "Begin Test: Segments self shadowing - No shadowing case." );

	auto aStartPoint1 = std::make_shared< CPoint2D >( 10, 0 );
	auto aEndPoint1 = std::make_shared< CPoint2D >( 0, 0 );

	auto aSegment1 = CViewSegment2D( aStartPoint1, aEndPoint1 );

	auto aStartPoint2 = std::make_shared< CPoint2D >( 0, 1 );
	auto aEndPoint2 = std::make_shared< CPoint2D >( 10, 1 );

	auto aSegment2 = CViewSegment2D( aStartPoint2, aEndPoint2 );

	auto aShadowing = aSegment1.selfShadowing( aSegment2 );

	EXPECT_EQ( Shadowing::No, aShadowing );

}

TEST_F( TestSegment2DSelfShadowing, Segment2DTotalShadowing ) {
	SCOPED_TRACE( "Begin Test: Segments self shadowing - Total shadowing case." );

	auto aStartPoint1 = std::make_shared< CPoint2D >( 10, 0 );
	auto aEndPoint1 = std::make_shared< CPoint2D >( 0, 0 );

	auto aSegment1 = CViewSegment2D( aStartPoint1, aEndPoint1 );

	auto aStartPoint2 = std::make_shared< CPoint2D >( 10, 1 );
	auto aEndPoint2 = std::make_shared< CPoint2D >( 0, 1 );

	auto aSegment2 = CViewSegment2D( aStartPoint2, aEndPoint2 );

	auto aShadowing = aSegment1.selfShadowing( aSegment2 );

	EXPECT_EQ( Shadowing::Total, aShadowing );

}

TEST_F( TestSegment2DSelfShadowing, Segment2DNoShadowingSamePoint1 ) {
	SCOPED_TRACE( "Begin Test: Segments self shadowing - No shadowing case (share same point angle < 180)." );

	auto aStartPoint1 = std::make_shared< CPoint2D >( 10, 0 );
	auto aEndPoint1 = std::make_shared< CPoint2D >( 0, 0 );

	auto aSegment1 = CViewSegment2D( aStartPoint1, aEndPoint1 );

	auto aStartPoint2 = std::make_shared< CPoint2D >( 0, 1 );
	auto aEndPoint2 = std::make_shared< CPoint2D >( 10, 0 );

	auto aSegment2 = CViewSegment2D( aStartPoint2, aEndPoint2 );

	auto aShadowing = aSegment1.selfShadowing( aSegment2 );

	EXPECT_EQ( Shadowing::No, aShadowing );

}

TEST_F( TestSegment2DSelfShadowing, Segment2DNoShadowingSamePoint2 ) {
	SCOPED_TRACE( "Begin Test: Segments self shadowing - No shadowing case (share same point, angle > 180)." );

	auto aStartPoint1 = std::make_shared< CPoint2D >( 10, 0 );
	auto aEndPoint1 = std::make_shared< CPoint2D >( 0, 0 );

	auto aSegment1 = CViewSegment2D( aStartPoint1, aEndPoint1 );

	auto aStartPoint2 = std::make_shared< CPoint2D >( 0, 0 );
	auto aEndPoint2 = std::make_shared< CPoint2D >( 0, -2 );

	auto aSegment2 = CViewSegment2D( aStartPoint2, aEndPoint2 );

	auto aShadowing = aSegment1.selfShadowing( aSegment2 );

	EXPECT_EQ( Shadowing::Total, aShadowing );

}

TEST_F( TestSegment2DSelfShadowing, Segment2DPartialShadowingThis ) {
	SCOPED_TRACE( "Begin Test: Segments self shadowing - Partial shadowing case (view blocked by itself)." );

	auto aStartPoint1 = std::make_shared< CPoint2D >( 10, 0 );
	auto aEndPoint1 = std::make_shared< CPoint2D >( 0, 0 );

	auto aSegment1 = CViewSegment2D( aStartPoint1, aEndPoint1 );

	auto aStartPoint2 = std::make_shared< CPoint2D >( -5, -1 );
	auto aEndPoint2 = std::make_shared< CPoint2D >( -5, 1 );

	auto aSegment2 = CViewSegment2D( aStartPoint2, aEndPoint2 );

	auto aShadowing = aSegment1.selfShadowing( aSegment2 );

	EXPECT_EQ( Shadowing::Partial, aShadowing );

}

TEST_F( TestSegment2DSelfShadowing, Segment2DPartialShadowingOther ) {
	SCOPED_TRACE( "Begin Test: Segments self shadowing - Partial shadowing case (view blocked by viewed surface)." );

	auto aStartPoint1 = std::make_shared< CPoint2D >( 10, 0 );
	auto aEndPoint1 = std::make_shared< CPoint2D >( 0, 0 );

	auto aSegment1 = CViewSegment2D( aStartPoint1, aEndPoint1 );

	auto aStartPoint2 = std::make_shared< CPoint2D >( 5, 5 );
	auto aEndPoint2 = std::make_shared< CPoint2D >( 5, 10 );

	auto aSegment2 = CViewSegment2D( aStartPoint2, aEndPoint2 );

	auto aShadowing = aSegment1.selfShadowing( aSegment2 );

	EXPECT_EQ( Shadowing::Partial, aShadowing );

}
