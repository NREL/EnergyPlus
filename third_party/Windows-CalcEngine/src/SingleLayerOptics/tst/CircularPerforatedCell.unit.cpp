#include <memory>
#include <gtest/gtest.h>

#include "WCECommon.hpp"
#include "WCESingleLayerOptics.hpp"


using namespace SingleLayerOptics;
using namespace FenestrationCommon;

class TestCircularPerforatedCell : public testing::Test {

private:
	std::shared_ptr< CCircularCellDescription > m_DescriptionCell;
	std::shared_ptr< CPerforatedCell > m_PerforatedCell;

protected:
	virtual void SetUp() {
		// create material
		double Tmat = 0.1;
		double Rfmat = 0.7;
		double Rbmat = 0.8;
		double minLambda = 0.3;
		double maxLambda = 2.5;
		std::shared_ptr< CMaterialSingleBand > aMaterial =
			std::make_shared< CMaterialSingleBand >( Tmat, Tmat, Rfmat, Rbmat, minLambda, maxLambda );

		// make cell geometry
		double x = 10; // mm
		double y = 10; // mm
		double thickness = 1; // mm
		double radius = 5; // mm
		m_DescriptionCell = std::make_shared< CCircularCellDescription >( x, y, thickness, radius );

		m_PerforatedCell = std::make_shared< CPerforatedCell >( aMaterial, m_DescriptionCell );
	}

public:
	std::shared_ptr< CPerforatedCell > GetCell() {
		return m_PerforatedCell;
	};

	std::shared_ptr< CCircularCellDescription > GetDescription() {
		return m_DescriptionCell;
	};

};

TEST_F( TestCircularPerforatedCell, TestCircular1 ) {
	SCOPED_TRACE( "Begin Test: Circular perforated cell (Theta = 0, Phi = 0)." );

	std::shared_ptr< CPerforatedCell > aCell = GetCell();
	std::shared_ptr< ICellDescription > aCellDescription = GetDescription();

	double Theta = 0; // deg
	double Phi = 0; // deg
	Side aFrontSide = Side::Front;
	Side aBackSide = Side::Back;

	CBeamDirection aDirection = CBeamDirection( Theta, Phi );

	double Tdir_dir = aCellDescription->T_dir_dir( aFrontSide, aDirection );
	EXPECT_NEAR( 0.785398163, Tdir_dir, 1e-6 );

	double Tdir_dif = aCell->T_dir_dif( aFrontSide, aDirection );
	EXPECT_NEAR( 0.021460184, Tdir_dif, 1e-6 );

	double Rfdir_dif = aCell->R_dir_dif( aFrontSide, aDirection );
	EXPECT_NEAR( 0.150221286, Rfdir_dif, 1e-6 );

	double Rbdir_dif = aCell->R_dir_dif( aBackSide, aDirection );
	EXPECT_NEAR( 0.171681469, Rbdir_dif, 1e-6 );

}

TEST_F( TestCircularPerforatedCell, TestCircular2 ) {
	SCOPED_TRACE( "Begin Test: Rectangular perforated cell (Theta = 45, Phi = 0)." );

	std::shared_ptr< CPerforatedCell > aCell = GetCell();
	std::shared_ptr< ICellDescription > aCellDescription = GetDescription();

	double Theta = 45; // deg
	double Phi = 0; // deg
	Side aFrontSide = Side::Front;
	Side aBackSide = Side::Back;

	CBeamDirection aDirection = CBeamDirection( Theta, Phi );

	double Tdir_dir = aCellDescription->T_dir_dir( aFrontSide, aDirection );
	EXPECT_NEAR( 0.706858347, Tdir_dir, 1e-6 );

	double Tdir_dif = aCell->T_dir_dif( aFrontSide, aDirection );
	EXPECT_NEAR( 0.029314165, Tdir_dif, 1e-6 );

	double Rfdir_dif = aCell->R_dir_dif( aFrontSide, aDirection );
	EXPECT_NEAR( 0.205199157, Rfdir_dif, 1e-6 );

	double Rbdir_dif = aCell->R_dir_dif( aBackSide, aDirection );
	EXPECT_NEAR( 0.234513322, Rbdir_dif, 1e-6 );

}

TEST_F( TestCircularPerforatedCell, TestCircular3 ) {
	SCOPED_TRACE( "Begin Test: Rectangular perforated cell (Theta = 78, Phi = 45)." );

	std::shared_ptr< CPerforatedCell > aCell = GetCell();
	std::shared_ptr< ICellDescription > aCellDescription = GetDescription();

	double Theta = 78; // deg
	double Phi = 45; // deg
	Side aFrontSide = Side::Front;
	Side aBackSide = Side::Back;

	CBeamDirection aDirection = CBeamDirection( Theta, Phi );

	double Tdir_dir = aCellDescription->T_dir_dir( aFrontSide, aDirection );
	EXPECT_NEAR( 0.415897379, Tdir_dir, 1e-6 );

	double Tdir_dif = aCell->T_dir_dif( aFrontSide, aDirection );
	EXPECT_NEAR( 0.058410262, Tdir_dif, 1e-6 );

	double Rfdir_dif = aCell->R_dir_dif( aFrontSide, aDirection );
	EXPECT_NEAR( 0.408871835, Rfdir_dif, 1e-6 );

	double Rbdir_dif = aCell->R_dir_dif( aBackSide, aDirection );
	EXPECT_NEAR( 0.467282097, Rbdir_dif, 1e-6 );

}
