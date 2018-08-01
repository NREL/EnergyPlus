#include <memory>
#include <gtest/gtest.h>

#include "WCESingleLayerOptics.hpp"
#include "WCECommon.hpp"


using namespace SingleLayerOptics;
using namespace FenestrationCommon;

class TestPerfectDiffuseCell1 : public testing::Test {

private:
	std::shared_ptr< CUniformDiffuseCell > m_Cell;

protected:
	virtual void SetUp() {
		// create material
		double Tmat = 0.00;
		double Rfmat = 0.55;
		double Rbmat = 0.55;
		double minLambda = 0.3;
		double maxLambda = 2.5;
		std::shared_ptr< CMaterialSingleBand > aMaterial =
			std::make_shared< CMaterialSingleBand >( Tmat, Tmat, Rfmat, Rbmat, minLambda, maxLambda );

		// make cell geometry
		std::shared_ptr< ICellDescription > aCell =
			std::make_shared< CPerfectDiffuseCellDescription >();

		m_Cell = std::make_shared< CUniformDiffuseCell >( aMaterial, aCell );
	}

public:
	std::shared_ptr< CUniformDiffuseCell > GetCell() {
		return m_Cell;
	};

};

TEST_F( TestPerfectDiffuseCell1, TestPerfectDiffuse1 ) {
	SCOPED_TRACE( "Begin Test: Perfect diffusing cell (Theta = 0, Phi = 0)." );

	std::shared_ptr< CUniformDiffuseCell > aCell = GetCell();

	double Theta = 0; // deg
	double Phi = 0; // deg
	Side aSide = Side::Front;

	CBeamDirection aDirection = CBeamDirection( Theta, Phi );

	double Tdir_dir = aCell->T_dir_dir( aSide, aDirection );
	EXPECT_NEAR( 0.00000000, Tdir_dir, 1e-6 );

	double Tdir_dif = aCell->T_dir_dif( aSide, aDirection );
	EXPECT_NEAR( 0.00000000, Tdir_dif, 1e-6 );

	double Rfdir_dif = aCell->R_dir_dif( aSide, aDirection );
	EXPECT_NEAR( 0.55000000, Rfdir_dif, 1e-6 );

	double Rbdir_dif = aCell->R_dir_dif( aSide, aDirection );
	EXPECT_NEAR( 0.55000000, Rbdir_dif, 1e-6 );

}

TEST_F( TestPerfectDiffuseCell1, TestPerfectDiffuse2 ) {
	SCOPED_TRACE( "Begin Test: Perfect diffusing cell (Theta = 45, Phi = 0)." );

	std::shared_ptr< CUniformDiffuseCell > aCell = GetCell();

	double Theta = 45; // deg
	double Phi = 0; // deg
	Side aSide = Side::Front;

	CBeamDirection aDirection = CBeamDirection( Theta, Phi );

	double Tdir_dir = aCell->T_dir_dir( aSide, aDirection );
	EXPECT_NEAR( 0.00000000, Tdir_dir, 1e-6 );

	double Tdir_dif = aCell->T_dir_dif( aSide, aDirection );
	EXPECT_NEAR( 0.00000000, Tdir_dif, 1e-6 );

	double Rfdir_dif = aCell->R_dir_dif( aSide, aDirection );
	EXPECT_NEAR( 0.55000000, Rfdir_dif, 1e-6 );

	double Rbdir_dif = aCell->R_dir_dif( aSide, aDirection );
	EXPECT_NEAR( 0.55000000, Rbdir_dif, 1e-6 );

}

TEST_F( TestPerfectDiffuseCell1, TestPerfectDiffuse3 ) {
	SCOPED_TRACE( "Begin Test: Perfect diffusing cell (Theta = 78, Phi = 45)." );

	std::shared_ptr< CUniformDiffuseCell > aCell = GetCell();

	double Theta = 78; // deg
	double Phi = 45; // deg
	Side aSide = Side::Front;

	CBeamDirection aDirection = CBeamDirection( Theta, Phi );

	double Tdir_dir = aCell->T_dir_dir( aSide, aDirection );
	EXPECT_NEAR( 0.00000000, Tdir_dir, 1e-6 );

	double Tdir_dif = aCell->T_dir_dif( aSide, aDirection );
	EXPECT_NEAR( 0.00000000, Tdir_dif, 1e-6 );

	double Rfdir_dif = aCell->R_dir_dif( aSide, aDirection );
	EXPECT_NEAR( 0.55000000, Rfdir_dif, 1e-6 );

	double Rbdir_dif = aCell->R_dir_dif( aSide, aDirection );
	EXPECT_NEAR( 0.55000000, Rbdir_dif, 1e-6 );

}

TEST_F( TestPerfectDiffuseCell1, TestPerfectDiffuse4 ) {
	SCOPED_TRACE( "Begin Test: Perfect diffusing cell (Theta = 54, Phi = 270)." );

	std::shared_ptr< CUniformDiffuseCell > aCell = GetCell();

	double Theta = 54; // deg
	double Phi = 270; // deg
	Side aSide = Side::Front;

	CBeamDirection aDirection = CBeamDirection( Theta, Phi );

	double Tdir_dir = aCell->T_dir_dir( aSide, aDirection );
	EXPECT_NEAR( 0.00000000, Tdir_dir, 1e-6 );

	double Tdir_dif = aCell->T_dir_dif( aSide, aDirection );
	EXPECT_NEAR( 0.00000000, Tdir_dif, 1e-6 );

	double Rfdir_dif = aCell->R_dir_dif( aSide, aDirection );
	EXPECT_NEAR( 0.55000000, Rfdir_dif, 1e-6 );

	double Rbdir_dif = aCell->R_dir_dif( aSide, aDirection );
	EXPECT_NEAR( 0.55000000, Rbdir_dif, 1e-6 );

}
