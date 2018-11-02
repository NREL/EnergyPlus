#include <memory>
#include <gtest/gtest.h>

#include "WCESingleLayerOptics.hpp"
#include "WCECommon.hpp"


using namespace SingleLayerOptics;
using namespace FenestrationCommon;

class TestVenetianCellFlat0_2 : public testing::Test {

private:
	std::shared_ptr< CVenetianCell > m_Cell;

protected:
	virtual void SetUp() {
		// create material
		double Tmat = 0.1;
		double Rfmat = 0.7;
		double Rbmat = 0.7;
		double minLambda = 0.3;
		double maxLambda = 2.5;
		std::shared_ptr< CMaterialSingleBand > aMaterial =
			std::make_shared< CMaterialSingleBand >( Tmat, Tmat, Rfmat, Rbmat, minLambda, maxLambda );


		// make cell geometry
		double slatWidth = 0.016; // m
		double slatSpacing = 0.010; // m
		double slatTiltAngle = 0;
		double curvatureRadius = 0;
		size_t numOfSlatSegments = 2;

		std::shared_ptr< CVenetianCellDescription > aCellDescription =
			std::make_shared< CVenetianCellDescription >( slatWidth, slatSpacing, slatTiltAngle,
			                                         curvatureRadius, numOfSlatSegments );

		m_Cell = std::make_shared< CVenetianCell >( aMaterial, aCellDescription );

	}

public:
	std::shared_ptr< CVenetianCell > GetCell() {
		return m_Cell;
	};

};

TEST_F( TestVenetianCellFlat0_2, TestVenetian1 ) {
	SCOPED_TRACE( "Begin Test: Venetian cell (Flat, 0 degrees slats) - directional-diffuse." );

	std::shared_ptr< CVenetianCell > aCell = GetCell();

	// Front side
	Side aSide = Side::Front;
	double Theta = 18;
	double Phi = 45;
	CBeamDirection incomingDirection = CBeamDirection( Theta, Phi );

	Theta = 18;
	Phi = 90;
	CBeamDirection outgoingDirection = CBeamDirection( Theta, Phi );

	double Tdir_dif = aCell->T_dir_dif( aSide, incomingDirection, outgoingDirection );
	double Rdir_dif = aCell->R_dir_dif( aSide, incomingDirection, outgoingDirection );

	EXPECT_NEAR( 0.11272685101443769, Tdir_dif, 1e-6 );
	EXPECT_NEAR( 0.11272685101443769, Rdir_dif, 1e-6 );

}
