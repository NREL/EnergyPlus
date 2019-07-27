#include <memory>
#include <gtest/gtest.h>

#include "WCEMultiLayerOptics.hpp"
#include "WCESingleLayerOptics.hpp"
#include "WCECommon.hpp"

using namespace SingleLayerOptics;
using namespace MultiLayerOptics;
using namespace FenestrationCommon;

// Test equivalent properties of double layer with direct-direct, direct-diffuse and diffuse-diffuse components
// Tests include adding layer on back and front sides
class TestEquivalentLayerWithScattering1 : public testing::Test {

private:
	// Additional layer added to the back side
	std::shared_ptr< CEquivalentScatteringLayer > m_DoubleBack;

	// Additional layer added to the front side
	std::shared_ptr< CEquivalentScatteringLayer > m_DoubleFront;

protected:
	virtual void SetUp() {
		std::shared_ptr< CScatteringSurface > f1 = std::make_shared< CScatteringSurface >( 0.08, 0.05, 0.46, 0.23, 0.46, 0.52 );
		std::shared_ptr< CScatteringSurface > b1 = std::make_shared< CScatteringSurface >( 0.13, 0.25, 0.38, 0.19, 0.64, 0.22 );
		CScatteringLayer aLayer1 = CScatteringLayer( f1, b1 );

		std::shared_ptr< CScatteringSurface > f2 = std::make_shared< CScatteringSurface >( 0.1, 0.05, 0.48, 0.26, 0.56, 0.34 );
		std::shared_ptr< CScatteringSurface > b2 = std::make_shared< CScatteringSurface >( 0.15, 0.0, 0.38, 0.19, 0.49, 0.39 );
		CScatteringLayer aLayer2 = CScatteringLayer( f2, b2 );

		m_DoubleBack = std::make_shared< CEquivalentScatteringLayer >( aLayer1 );
		m_DoubleBack->addLayer( aLayer2, Side::Back );

		m_DoubleFront = std::make_shared< CEquivalentScatteringLayer >( aLayer1 );
		m_DoubleFront->addLayer( aLayer2, Side::Front );

	}

public:
	std::shared_ptr< CEquivalentScatteringLayer > getDoubleBack() const
	{
		return m_DoubleBack;
	}

	std::shared_ptr< CEquivalentScatteringLayer > getDoubleFront() const
	{
		return m_DoubleFront;
	}

};

TEST_F( TestEquivalentLayerWithScattering1, TestLayerAtBackSide ) {
	SCOPED_TRACE( "Begin Test: Equivalent layer transmittance and reflectances (direct-direct, direct-diffuse and diffuse-diffuse" );

	CEquivalentScatteringLayer doubleLayer = *getDoubleBack();

	///////////////////////////////////////////////
	// Direct-Direct
	///////////////////////////////////////////////
	double Tf = doubleLayer.getPropertySimple( PropertySimple::T, Side::Front, Scattering::DirectDirect );
	EXPECT_NEAR( 0.008101266, Tf, 1e-6 );

	double Rf = doubleLayer.getPropertySimple( PropertySimple::R, Side::Front, Scattering::DirectDirect );
	EXPECT_NEAR( 0.050526582, Rf, 1e-6 );

	double Tb = doubleLayer.getPropertySimple( PropertySimple::T, Side::Back, Scattering::DirectDirect );
	EXPECT_NEAR( 0.019746835, Tb, 1e-6 );

	double Rb = doubleLayer.getPropertySimple( PropertySimple::R, Side::Back, Scattering::DirectDirect );
	EXPECT_NEAR( 0.003797468, Rb, 1e-6 );

	///////////////////////////////////////////////
	// Diffuse-Diffuse
	///////////////////////////////////////////////
	Tf = doubleLayer.getPropertySimple( PropertySimple::T, Side::Front, Scattering::DiffuseDiffuse );
	EXPECT_NEAR( 0.278426286, Tf, 1e-6 );

	Rf = doubleLayer.getPropertySimple( PropertySimple::R, Side::Front, Scattering::DiffuseDiffuse );
	EXPECT_NEAR( 0.6281885, Rf, 1e-6 );

	Tb = doubleLayer.getPropertySimple( PropertySimple::T, Side::Back, Scattering::DiffuseDiffuse );
	EXPECT_NEAR( 0.33895374, Tb, 1e-6 );

	Rb = doubleLayer.getPropertySimple( PropertySimple::R, Side::Back, Scattering::DiffuseDiffuse );
	EXPECT_NEAR( 0.455248595, Rb, 1e-6 );

	///////////////////////////////////////////////
	// Direct-Diffuse
	///////////////////////////////////////////////
	Tf = doubleLayer.getPropertySimple( PropertySimple::T, Side::Front, Scattering::DirectDiffuse );
	EXPECT_NEAR( 0.32058299, Tf, 1e-6 );

	Rf = doubleLayer.getPropertySimple( PropertySimple::R, Side::Front, Scattering::DirectDiffuse );
	EXPECT_NEAR( 0.354479119, Rf, 1e-6 );

	Tb = doubleLayer.getPropertySimple( PropertySimple::T, Side::Back, Scattering::DirectDiffuse );
	EXPECT_NEAR( 0.334201295, Tb, 1e-6 );

	Rb = doubleLayer.getPropertySimple( PropertySimple::R, Side::Back, Scattering::DirectDiffuse );
	EXPECT_NEAR( 0.27761223, Rb, 1e-6 );
}

TEST_F( TestEquivalentLayerWithScattering1, TestLayerAtFrontSide ) {
	SCOPED_TRACE( "Begin Test: Equivalent layer transmittance and reflectances (direct-direct, direct-diffuse and diffuse-diffuse" );

	CEquivalentScatteringLayer doubleLayer = *getDoubleFront();

	///////////////////////////////////////////////
	// Direct-Direct
	///////////////////////////////////////////////
	double Tf = doubleLayer.getPropertySimple( PropertySimple::T, Side::Front, Scattering::DirectDirect );
	EXPECT_NEAR( 0.008, Tf, 1e-6 );

	double Rf = doubleLayer.getPropertySimple( PropertySimple::R, Side::Front, Scattering::DirectDirect );
	EXPECT_NEAR( 0.05075, Rf, 1e-6 );

	double Tb = doubleLayer.getPropertySimple( PropertySimple::T, Side::Back, Scattering::DirectDirect );
	EXPECT_NEAR( 0.0195, Tb, 1e-6 );

	double Rb = doubleLayer.getPropertySimple( PropertySimple::R, Side::Back, Scattering::DirectDirect );
	EXPECT_NEAR( 0.25, Rb, 1e-6 );

	///////////////////////////////////////////////
	// Diffuse-Diffuse
	///////////////////////////////////////////////
	Tf = doubleLayer.getPropertySimple( PropertySimple::T, Side::Front, Scattering::DiffuseDiffuse );
	EXPECT_NEAR( 0.323130958, Tf, 1e-6 );

	Rf = doubleLayer.getPropertySimple( PropertySimple::R, Side::Front, Scattering::DiffuseDiffuse );
	EXPECT_NEAR( 0.518986453, Rf, 1e-6 );

	Tb = doubleLayer.getPropertySimple( PropertySimple::T, Side::Back, Scattering::DiffuseDiffuse );
	EXPECT_NEAR( 0.393376819, Tb, 1e-6 );

	Rb = doubleLayer.getPropertySimple( PropertySimple::R, Side::Back, Scattering::DiffuseDiffuse );
	EXPECT_NEAR( 0.364024084, Rb, 1e-6 );

	///////////////////////////////////////////////
	// Direct-Diffuse
	///////////////////////////////////////////////
	Tf = doubleLayer.getPropertySimple( PropertySimple::T, Side::Front, Scattering::DirectDiffuse );
	EXPECT_NEAR( 0.328693427, Tf, 1e-6 );

	Rf = doubleLayer.getPropertySimple( PropertySimple::R, Side::Front, Scattering::DirectDiffuse );
	EXPECT_NEAR( 0.429757577, Rf, 1e-6 );

	Tb = doubleLayer.getPropertySimple( PropertySimple::T, Side::Back, Scattering::DirectDiffuse );
	EXPECT_NEAR( 0.290862067, Tb, 1e-6 );

	Rb = doubleLayer.getPropertySimple( PropertySimple::R, Side::Back, Scattering::DirectDiffuse );
	EXPECT_NEAR( 0.289766683, Rb, 1e-6 );
}
