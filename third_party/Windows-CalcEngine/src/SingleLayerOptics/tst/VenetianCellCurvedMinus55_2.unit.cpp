#include <memory>
#include <gtest/gtest.h>

#include "WCECommon.hpp"
#include "WCESingleLayerOptics.hpp"


using namespace SingleLayerOptics;
using namespace FenestrationCommon;

class TestVenetianCellCurvedMinus55_2 : public testing::Test {

private:
	std::shared_ptr< CVenetianCell > m_Cell;

protected:
	virtual void SetUp() {
		// create material
		double Tmat = 0.1;
		double Rfmat = 0.3;
		double Rbmat = 0.7;
		double minLambda = 0.3;
		double maxLambda = 2.5;
		std::shared_ptr< CMaterialSingleBand > aMaterial =
			std::make_shared< CMaterialSingleBand >( Tmat, Tmat, Rfmat, Rbmat, minLambda, maxLambda );

		// make cell geometry
		double slatWidth = 0.076200; // m
		double slatSpacing = 0.057150; // m
		double slatTiltAngle = -55.000000;
		double curvatureRadius = 0.123967;
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

TEST_F( TestVenetianCellCurvedMinus55_2, TestVenetian1 ) {
	SCOPED_TRACE( "Begin Test: Venetian cell (Curved, -55 degrees slats - diffuse-diffuse)." );

	std::shared_ptr< CVenetianCell > aCell = GetCell();

	// Front side
	Side aSide = Side::Front;
	double Tdif_dif = aCell->T_dif_dif( aSide );
	double Rdif_dif = aCell->R_dif_dif( aSide );

	EXPECT_NEAR( 0.26028659060185622, Tdif_dif, 1e-6 );
	EXPECT_NEAR( 0.39971258912039054, Rdif_dif, 1e-6 );

	// Back side
	aSide = Side::Back;
	Tdif_dif = aCell->T_dif_dif( aSide );
	Rdif_dif = aCell->R_dif_dif( aSide );

	EXPECT_NEAR( 0.26028659060185666, Tdif_dif, 1e-6 );
	EXPECT_NEAR( 0.19608861125331134, Rdif_dif, 1e-6 );

}

TEST_F( TestVenetianCellCurvedMinus55_2, TestVenetian2 ) {
	SCOPED_TRACE( "Begin Test: Venetian cell (Curved, -55 degrees slats - direct-diffuse)." );

	std::shared_ptr< CVenetianCell > aCell = GetCell();

	// Front side
	Side aSide = Side::Front;
	double Theta = 0;
	double Phi = 0;
	CBeamDirection aDirection = CBeamDirection( Theta, Phi );

	double Tdir_dir = aCell->T_dir_dir( aSide, aDirection );
	double Tdir_dif = aCell->T_dir_dif( aSide, aDirection );
	double Rdir_dif = aCell->R_dir_dif( aSide, aDirection );

	EXPECT_NEAR( 0.00000000000000000, Tdir_dir, 1e-6 );
	EXPECT_NEAR( 0.13194581766759877, Tdir_dif, 1e-6 );
	EXPECT_NEAR( 0.46950286596646840, Rdir_dif, 1e-6 );

	// Back side
	aSide = Side::Back;
	Tdir_dir = aCell->T_dir_dir( aSide, aDirection );
	Tdir_dif = aCell->T_dir_dif( aSide, aDirection );
	Rdir_dif = aCell->R_dir_dif( aSide, aDirection );

	EXPECT_NEAR( 0.00000000000000000, Tdir_dir, 1e-6 );
	EXPECT_NEAR( 0.11371279153419295, Tdir_dif, 1e-6 );
	EXPECT_NEAR( 0.21789069507796985, Rdir_dif, 1e-6 );

}
