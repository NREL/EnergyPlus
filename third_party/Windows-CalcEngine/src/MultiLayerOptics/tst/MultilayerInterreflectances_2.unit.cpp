#include <memory>
#include <gtest/gtest.h>

#include "WCEMultiLayerOptics.hpp"
#include "WCECommon.hpp"

using namespace std;
using namespace MultiLayerOptics;
using namespace FenestrationCommon;

// Calculation of energies that are incoming to the layers surfaces.
// Layers are added at back side.
class TestMultilayerInterreflectances_2 : public testing::Test {

private:
	// Additional layer added to the back side
	std::shared_ptr< CInterRefSingleComponent > m_Interref;

protected:
	virtual void SetUp() {

		m_Interref = make_shared< CInterRefSingleComponent >( 0.12, 0.47, 0.33, 0.63 );
		m_Interref->addLayer( 0.56, 0.34, 0.49, 0.39, Side::Front );
		m_Interref->addLayer( 0.46, 0.52, 0.64, 0.22, Side::Front );

	}

public:
	std::shared_ptr< CInterRefSingleComponent > getInt() {
		return m_Interref;
	};

};

TEST_F( TestMultilayerInterreflectances_2, TestForwardFlow ) {
	SCOPED_TRACE( "Begin Test: Double pane equivalent layer properties (additonal layer on back side)." );

	CInterRefSingleComponent eqLayer = *getInt();

	EnergyFlow aFlow = EnergyFlow::Forward;

	double If1 = eqLayer.getEnergyToSurface( 1, Side::Front, aFlow );
	EXPECT_NEAR( 1.0, If1, 1e-6 );

	double If2 = eqLayer.getEnergyToSurface( 2, Side::Front, aFlow );
	EXPECT_NEAR( 0.516587502, If2, 1e-6 );

	double If3 = eqLayer.getEnergyToSurface( 3, Side::Front, aFlow );
	EXPECT_NEAR( 0.354216972, If3, 1e-6 );

	double Ib1 = eqLayer.getEnergyToSurface( 1, Side::Back, aFlow );
	EXPECT_NEAR( 0.25721592, Ib1, 1e-6 );

	double Ib2 = eqLayer.getEnergyToSurface( 2, Side::Back, aFlow );
	EXPECT_NEAR( 0.166481977, Ib2, 1e-6 );

	double Ib3 = eqLayer.getEnergyToSurface( 3, Side::Back, aFlow );
	EXPECT_NEAR( 0.0, Ib3, 1e-6 );

}

TEST_F( TestMultilayerInterreflectances_2, TestBackwardFlow ) {
	SCOPED_TRACE( "Begin Test: Double pane equivalent layer properties (additonal layer on back side)." );

	CInterRefSingleComponent eqLayer = *getInt();

	EnergyFlow aFlow = EnergyFlow::Backward;

	double If1 = eqLayer.getEnergyToSurface( 1, Side::Front, aFlow );
	EXPECT_NEAR( 0.0, If1, 1e-6 );

	double If2 = eqLayer.getEnergyToSurface( 2, Side::Front, aFlow );
	EXPECT_NEAR( 0.048916594, If2, 1e-6 );

	double If3 = eqLayer.getEnergyToSurface( 3, Side::Front, aFlow );
	EXPECT_NEAR( 0.191126843, If3, 1e-6 );

	double Ib1 = eqLayer.getEnergyToSurface( 1, Side::Back, aFlow );
	EXPECT_NEAR( 0.222348154, Ib1, 1e-6 );

	double Ib2 = eqLayer.getEnergyToSurface( 2, Side::Back, aFlow );
	EXPECT_NEAR( 0.419829616, Ib2, 1e-6 );

	double Ib3 = eqLayer.getEnergyToSurface( 3, Side::Back, aFlow );
	EXPECT_NEAR( 1.0, Ib3, 1e-6 );

}
