#include <memory>
#include <gtest/gtest.h>

#include <memory>

#include "WCEViewer.hpp"
#include "WCECommon.hpp"


using namespace Viewer;
using namespace FenestrationCommon;

class TestEnclosure2DViewFactorsBlockingSurface3 : public testing::Test {

private:
	std::shared_ptr< CGeometry2D > m_Enclosure2D;

protected:
	virtual void SetUp() {
		m_Enclosure2D = std::make_shared< CGeometry2D >();

		// Segment 1
		std::shared_ptr< CPoint2D > aStartPoint1 = std::make_shared< CPoint2D >( 10, 0 );
		std::shared_ptr< CPoint2D > aEndPoint1 = std::make_shared< CPoint2D >( 0, 0 );

		std::shared_ptr< CViewSegment2D > aSegment1 = std::make_shared< CViewSegment2D >( aStartPoint1, aEndPoint1 );
		m_Enclosure2D->appendSegment( aSegment1 );

		// Segment 2
		std::shared_ptr< CPoint2D > aStartPoint2 = std::make_shared< CPoint2D >( 0, 5 );
		std::shared_ptr< CPoint2D > aEndPoint2 = std::make_shared< CPoint2D >( 10, 5 );

		std::shared_ptr< CViewSegment2D > aSegment2 = std::make_shared< CViewSegment2D >( aStartPoint2, aEndPoint2 );
		m_Enclosure2D->appendSegment( aSegment2 );

		// Segment 3
		std::shared_ptr< CPoint2D > aStartPoint3 = std::make_shared< CPoint2D >( 8, 2 );
		std::shared_ptr< CPoint2D > aEndPoint3 = std::make_shared< CPoint2D >( 2, 2 );

		std::shared_ptr< CViewSegment2D > aSegment3 = std::make_shared< CViewSegment2D >( aStartPoint3, aEndPoint3 );
		m_Enclosure2D->appendSegment( aSegment3 );

	}

public:
	std::shared_ptr< CGeometry2D > getEnclosure() {
		return m_Enclosure2D;
	};

};

TEST_F( TestEnclosure2DViewFactorsBlockingSurface3, Enclosure2DViewFactors ) {
	SCOPED_TRACE( "Begin Test: 2D Enclosure - View Factors (blocking surface)." );

	std::shared_ptr< CGeometry2D > aEnclosure = getEnclosure();

	std::shared_ptr< CSquareMatrix > viewFactors = aEnclosure->viewFactors();

	EXPECT_NEAR( 0.000000000, ( *viewFactors )[ 0 ][ 0 ], 1e-6 );
	EXPECT_NEAR( 0.140312424, ( *viewFactors )[ 0 ][ 1 ], 1e-6 );
	EXPECT_NEAR( 0.000000000, ( *viewFactors )[ 0 ][ 2 ], 1e-6 );

	EXPECT_NEAR( 0.140312424, ( *viewFactors )[ 1 ][ 0 ], 1e-6 );
	EXPECT_NEAR( 0.000000000, ( *viewFactors )[ 1 ][ 1 ], 1e-6 );
	EXPECT_NEAR( 0.493845247, ( *viewFactors )[ 1 ][ 2 ], 1e-6 );

	EXPECT_NEAR( 0.000000000, ( *viewFactors )[ 2 ][ 0 ], 1e-6 );
	EXPECT_NEAR( 0.823075412, ( *viewFactors )[ 2 ][ 1 ], 1e-6 );
	EXPECT_NEAR( 0.000000000, ( *viewFactors )[ 2 ][ 2 ], 1e-6 );

}
