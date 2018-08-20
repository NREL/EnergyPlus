#include <memory>
#include <gtest/gtest.h>

#include "WCESingleLayerOptics.hpp"
#include "WCECommon.hpp"


using namespace SingleLayerOptics;
using namespace FenestrationCommon;

class TestWovenCell3 : public testing::Test {

private:
	std::shared_ptr< CWovenCell > m_Cell;

protected:
	virtual void SetUp() {
		// create material
		double Tmat = 0;
		double Rfmat = 0;
		double Rbmat = 0;
		double minLambda = 0.3;
		double maxLambda = 2.5;
		std::shared_ptr< CMaterialSingleBand > aMaterial =
			std::make_shared< CMaterialSingleBand >( Tmat, Tmat, Rfmat, Rbmat, minLambda, maxLambda );

		// make cell geometry
		double diameter = 6.35; // mm
		double spacing = 19.05; // mm
		std::shared_ptr< ICellDescription > aCell =
			std::make_shared< CWovenCellDescription >( diameter, spacing );

		m_Cell = std::make_shared< CWovenCell >( aMaterial, aCell );
	}

public:
	std::shared_ptr< CWovenCell > GetCell() {
		return m_Cell;
	};

};

TEST_F( TestWovenCell3, TestWoven1 ) {
	SCOPED_TRACE( "Begin Test: Woven cell (Theta = 0, Phi = 0)." );

	std::shared_ptr< CWovenCell > aCell = GetCell();

	double Theta = 0; // deg
	double Phi = 0; // deg
	Side aFrontSide = Side::Front;
	Side aBackSide = Side::Back;

	CBeamDirection aDirection = CBeamDirection( Theta, Phi );

	double Tdir_dir = aCell->T_dir_dir( aFrontSide, aDirection );
	EXPECT_NEAR( 0.444444444, Tdir_dir, 1e-6 );

	Tdir_dir = aCell->T_dir_dir( aBackSide, aDirection );
	EXPECT_NEAR( 0.444444444, Tdir_dir, 1e-6 );

	double Tdir_dif = aCell->T_dir_dif( aFrontSide, aDirection );
	EXPECT_NEAR( 0.0, Tdir_dif, 1e-6 );

	Tdir_dif = aCell->T_dir_dif( aBackSide, aDirection );
	EXPECT_NEAR( 0.0, Tdir_dif, 1e-6 );

	double Rdir_dif = aCell->R_dir_dif( aFrontSide, aDirection );
	EXPECT_NEAR( 0.0, Rdir_dif, 1e-6 );

	Rdir_dif = aCell->R_dir_dif( aBackSide, aDirection );
	EXPECT_NEAR( 0.0, Rdir_dif, 1e-6 );

}

TEST_F( TestWovenCell3, TestWoven2 ) {
	SCOPED_TRACE( "Begin Test: Woven cell (Theta = 45, Phi = 0)." );

	std::shared_ptr< CWovenCell > aCell = GetCell();

	double Theta = 45; // deg
	double Phi = 0; // deg
	Side aFrontSide = Side::Front;
	Side aBackSide = Side::Back;

	CBeamDirection aDirection = CBeamDirection( Theta, Phi );

	double Tdir_dir = aCell->T_dir_dir( aFrontSide, aDirection );
	EXPECT_NEAR( 0.352396986, Tdir_dir, 1e-6 );

	Tdir_dir = aCell->T_dir_dir( aBackSide, aDirection );
	EXPECT_NEAR( 0.352396986, Tdir_dir, 1e-6 );

	double Tdir_dif = aCell->T_dir_dif( aFrontSide, aDirection );
	EXPECT_NEAR( 0.0, Tdir_dif, 1e-6 );

	Tdir_dif = aCell->T_dir_dif( aBackSide, aDirection );
	EXPECT_NEAR( 0.0, Tdir_dif, 1e-6 );

	double Rdir_dif = aCell->R_dir_dif( aFrontSide, aDirection );
	EXPECT_NEAR( 0.0, Rdir_dif, 1e-6 );

	Rdir_dif = aCell->R_dir_dif( aBackSide, aDirection );
	EXPECT_NEAR( 0.0, Rdir_dif, 1e-6 );

}

TEST_F( TestWovenCell3, TestWoven3 ) {
	SCOPED_TRACE( "Begin Test: Woven cell (Theta = 78, Phi = 45)." );

	std::shared_ptr< CWovenCell > aCell = GetCell();

	double Theta = 78; // deg
	double Phi = 45; // deg
	Side aFrontSide = Side::Front;
	Side aBackSide = Side::Back;

	CBeamDirection aDirection = CBeamDirection( Theta, Phi );

	double Tdir_dir = aCell->T_dir_dir( aFrontSide, aDirection );
	EXPECT_NEAR( 0.0, Tdir_dir, 1e-6 );

	Tdir_dir = aCell->T_dir_dir( aBackSide, aDirection );
	EXPECT_NEAR( 0.0, Tdir_dir, 1e-6 );

	double Tdir_dif = aCell->T_dir_dif( aFrontSide, aDirection );
	EXPECT_NEAR( 0.0, Tdir_dif, 1e-6 );

	Tdir_dif = aCell->T_dir_dif( aBackSide, aDirection );
	EXPECT_NEAR( 0.0, Tdir_dif, 1e-6 );

	double Rdir_dif = aCell->R_dir_dif( aFrontSide, aDirection );
	EXPECT_NEAR( 0.0, Rdir_dif, 1e-6 );

	Rdir_dif = aCell->R_dir_dif( aBackSide, aDirection );
	EXPECT_NEAR( 0.0, Rdir_dif, 1e-6 );

}
