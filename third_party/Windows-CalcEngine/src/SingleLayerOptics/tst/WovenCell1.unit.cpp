#include <memory>
#include <gtest/gtest.h>

#include "WCECommon.hpp"
#include "WCESingleLayerOptics.hpp"


using namespace SingleLayerOptics;
using namespace FenestrationCommon;

class TestWovenCell1 : public testing::Test {

private:
	std::shared_ptr< CWovenCell > m_Cell;

protected:
	virtual void SetUp() {
		// create material
		double Tmat = 0.08;
		double Rfmat = 0.9;
		double Rbmat = 0.9;
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

TEST_F( TestWovenCell1, TestWoven1 ) {
	SCOPED_TRACE( "Begin Test: Woven cell (Theta = 0, Phi = 0)." );

	std::shared_ptr< CWovenCell > aCell = GetCell();

	double Theta = 0; // deg
	double Phi = 0; // deg
	Side aSide = Side::Front;

	CBeamDirection aDirection = CBeamDirection( Theta, Phi );

	double Tdir_dir = aCell->T_dir_dir( aSide, aDirection );
	EXPECT_NEAR( 0.444444444, Tdir_dir, 1e-6 );

	double Tdir_dif = aCell->T_dir_dif( aSide, aDirection );
	EXPECT_NEAR( 0.06560392, Tdir_dif, 1e-6 );

	double Rfdir_dif = aCell->R_dir_dif( aSide, aDirection );
	EXPECT_NEAR( 0.478840524, Rfdir_dif, 1e-6 );

	double Rbdir_dif = aCell->R_dir_dif( aSide, aDirection );
	EXPECT_NEAR( 0.478840524, Rbdir_dif, 1e-6 );

}

TEST_F( TestWovenCell1, TestWoven2 ) {
	SCOPED_TRACE( "Begin Test: Woven cell (Theta = 45, Phi = 0)." );

	std::shared_ptr< CWovenCell > aCell = GetCell();

	double Theta = 45; // deg
	double Phi = 0; // deg
	Side aSide = Side::Front;

	CBeamDirection aDirection = CBeamDirection( Theta, Phi );

	double Tdir_dir = aCell->T_dir_dir( aSide, aDirection );
	EXPECT_NEAR( 0.352396986, Tdir_dir, 1e-6 );

	double Tdir_dif = aCell->T_dir_dif( aSide, aDirection );
	EXPECT_NEAR( 0.074469339, Tdir_dif, 1e-6 );

	double Rfdir_dif = aCell->R_dir_dif( aSide, aDirection );
	EXPECT_NEAR( 0.560181615, Rfdir_dif, 1e-6 );

	double Rbdir_dif = aCell->R_dir_dif( aSide, aDirection );
	EXPECT_NEAR( 0.560181615, Rbdir_dif, 1e-6 );

}

TEST_F( TestWovenCell1, TestWoven3 ) {
	SCOPED_TRACE( "Begin Test: Woven cell (Theta = 78, Phi = 45)." );

	std::shared_ptr< CWovenCell > aCell = GetCell();

	double Theta = 78; // deg
	double Phi = 45; // deg
	Side aSide = Side::Front;

	CBeamDirection aDirection = CBeamDirection( Theta, Phi );

	double Tdir_dir = aCell->T_dir_dir( aSide, aDirection );
	EXPECT_NEAR( 0.0, Tdir_dir, 1e-6 );

	double Tdir_dif = aCell->T_dir_dif( aSide, aDirection );
	EXPECT_NEAR( 0.183361355, Tdir_dif, 1e-6 );

	double Rfdir_dif = aCell->R_dir_dif( aSide, aDirection );
	EXPECT_NEAR( 0.796638645, Rfdir_dif, 1e-6 );

	double Rbdir_dif = aCell->R_dir_dif( aSide, aDirection );
	EXPECT_NEAR( 0.796638645, Rbdir_dif, 1e-6 );

}

TEST_F( TestWovenCell1, TestWoven4 ) {
	SCOPED_TRACE( "Begin Test: Woven cell (Theta = 54, Phi = 270)." );

	std::shared_ptr< CWovenCell > aCell = GetCell();

	double Theta = 54; // deg
	double Phi = 270; // deg
	Side aSide = Side::Front;

	CBeamDirection aDirection = CBeamDirection( Theta, Phi );

	double Tdir_dir = aCell->T_dir_dir( aSide, aDirection );
	EXPECT_NEAR( 0.100838024, Tdir_dir, 1e-6 );

	double Tdir_dif = aCell->T_dir_dif( aSide, aDirection );
	EXPECT_NEAR( 0.130587497, Tdir_dif, 1e-6 );

	double Rfdir_dif = aCell->R_dir_dif( aSide, aDirection );
	EXPECT_NEAR( 0.7505912396, Rfdir_dif, 1e-6 );

	double Rbdir_dif = aCell->R_dir_dif( aSide, aDirection );
	EXPECT_NEAR( 0.7505912396, Rbdir_dif, 1e-6 );

}
