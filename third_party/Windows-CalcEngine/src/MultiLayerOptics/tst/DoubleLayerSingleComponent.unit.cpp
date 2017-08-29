#include <memory>
#include <gtest/gtest.h>

#include "WCEMultiLayerOptics.hpp"
#include "WCESingleLayerOptics.hpp"
#include "WCECommon.hpp"

using namespace std;
using namespace MultiLayerOptics;
using namespace FenestrationCommon;

// Test equivalent properties of double layer with direct-direct or diffuse-diffuse components only
// Tests include adding layer on back adn front sides
class TestDoubleLayerSingleComponent : public testing::Test {

private:
	// Additional layer added to the back side
	std::shared_ptr< CEquivalentLayerSingleComponent > m_DoubleBack;

	// Additional layer added to the front side
	std::shared_ptr< CEquivalentLayerSingleComponent > m_DoubleFront;

protected:
	virtual void SetUp() {
		m_DoubleBack = make_shared< CEquivalentLayerSingleComponent >( 0.46, 0.52, 0.64, 0.22 );
		m_DoubleBack->addLayer( 0.56, 0.34, 0.49, 0.39 );

		m_DoubleFront = make_shared< CEquivalentLayerSingleComponent >( 0.46, 0.52, 0.64, 0.22 );
		m_DoubleFront->addLayer( 0.56, 0.34, 0.49, 0.39, Side::Front );

	}

public:
	std::shared_ptr< CEquivalentLayerSingleComponent > getDoubleBack() {
		return m_DoubleBack;
	};

	std::shared_ptr< CEquivalentLayerSingleComponent > getDoubleFront() {
		return m_DoubleFront;
	};

};

TEST_F( TestDoubleLayerSingleComponent, TestPropertiesBackSide ) {
	SCOPED_TRACE( "Begin Test: Double pane equivalent layer properties (additonal layer on back side)." );

	CEquivalentLayerSingleComponent doubleLayer = *getDoubleBack();

	double Tf = doubleLayer.getProperty( Property::T, Side::Front );
	EXPECT_NEAR( 0.278426286, Tf, 1e-6 );

	double Rf = doubleLayer.getProperty( Property::R, Side::Front );
	EXPECT_NEAR( 0.6281885, Rf, 1e-6 );

	double Af = doubleLayer.getProperty( Property::Abs, Side::Front );
	EXPECT_NEAR( 0.093385214, Af, 1e-6 );

	double Tb = doubleLayer.getProperty( Property::T, Side::Back );
	EXPECT_NEAR( 0.33895374, Tb, 1e-6 );

	double Rb = doubleLayer.getProperty( Property::R, Side::Back );
	EXPECT_NEAR( 0.455248595, Rb, 1e-6 );

	double Ab = doubleLayer.getProperty( Property::Abs, Side::Back );
	EXPECT_NEAR( 0.205797665, Ab, 1e-6 );

}

TEST_F( TestDoubleLayerSingleComponent, TestPropertiesFrontSide ) {
	SCOPED_TRACE( "Begin Test: Double pane equivalent layer properties (additonal layer on front side)." );

	CEquivalentLayerSingleComponent doubleLayer = *getDoubleFront();

	double Tf = doubleLayer.getProperty( Property::T, Side::Front );
	EXPECT_NEAR( 0.323130958, Tf, 1e-6 );

	double Rf = doubleLayer.getProperty( Property::R, Side::Front );
	EXPECT_NEAR( 0.518986453, Rf, 1e-6 );

	double Af = doubleLayer.getProperty( Property::Abs, Side::Front );
	EXPECT_NEAR( 0.157882589, Af, 1e-6 );

	double Tb = doubleLayer.getProperty( Property::T, Side::Back );
	EXPECT_NEAR( 0.393376819, Tb, 1e-6 );

	double Rb = doubleLayer.getProperty( Property::R, Side::Back );
	EXPECT_NEAR( 0.364024084, Rb, 1e-6 );

	double Ab = doubleLayer.getProperty( Property::Abs, Side::Back );
	EXPECT_NEAR( 0.242599097, Ab, 1e-6 );

}
