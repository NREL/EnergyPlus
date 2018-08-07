#include <memory>
#include <gtest/gtest.h>

#include "WCECommon.hpp"
#include "WCESingleLayerOptics.hpp"


using namespace SingleLayerOptics;
using namespace FenestrationCommon;

class TestVenetianCellFlat45_2 : public testing::Test {

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
		double slatWidth = 0.010; // m
		double slatSpacing = 0.010; // m
		double slatTiltAngle = 45;
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

TEST_F( TestVenetianCellFlat45_2, TestVenetian1 ) {
	SCOPED_TRACE( "Begin Test: Venetian cell (Flat, 45 degrees slats) - diffuse-diffuse." );

	std::shared_ptr< CVenetianCell > aCell = GetCell();

	// Front side
	Side aSide = Side::Front;
	double Tdif_dif = aCell->T_dif_dif( aSide );
	double Rdif_dif = aCell->R_dif_dif( aSide );

	EXPECT_NEAR( 0.41584962301445338, Tdif_dif, 1e-6 );
	EXPECT_NEAR( 0.15288558748616171, Rdif_dif, 1e-6 );

	// Back side
	aSide = Side::Back;
	Tdif_dif = aCell->T_dif_dif( aSide );
	Rdif_dif = aCell->R_dif_dif( aSide );

	EXPECT_NEAR( 0.41584962301445338, Tdif_dif, 1e-6 );
	EXPECT_NEAR( 0.32417709978497861, Rdif_dif, 1e-6 );

}

TEST_F( TestVenetianCellFlat45_2, TestVenetian2 ) {
	SCOPED_TRACE( "Begin Test: Venetian cell (Flat, 45 degrees slats) - direct-diffuse." );

	std::shared_ptr< CVenetianCell > aCell = GetCell();

	// Front side
	Side aSide = Side::Front;
	double Theta = 0;
	double Phi = 0;
	CBeamDirection aDirection = CBeamDirection( Theta, Phi );

	double Tdir_dir = aCell->T_dir_dir( aSide, aDirection );
	double Tdir_dif = aCell->T_dir_dif( aSide, aDirection );
	double Rdir_dif = aCell->R_dir_dif( aSide, aDirection );

	EXPECT_NEAR( 0.29289321881345237, Tdir_dir, 1e-6 );
	EXPECT_NEAR( 0.091914433617905855, Tdir_dif, 1e-6 );
	EXPECT_NEAR( 0.15247512857100193, Rdir_dif, 1e-6 );

	// Back side
	aSide = Side::Back;
	Tdir_dir = aCell->T_dir_dir( aSide, aDirection );
	Tdir_dif = aCell->T_dir_dif( aSide, aDirection );
	Rdir_dif = aCell->R_dir_dif( aSide, aDirection );

	EXPECT_NEAR( 0.29289321881345237, Tdir_dir, 1e-6 );
	EXPECT_NEAR( 0.11545919482729911, Tdir_dif, 1e-6 );
	EXPECT_NEAR( 0.34253339900108815, Rdir_dif, 1e-6 );

}
