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
class TestMultiLayer1 : public testing::Test {

private:
	std::shared_ptr< CMultiLayerScattered > m_Layer;

protected:
	virtual void SetUp() {
		std::shared_ptr< CScatteringSurface > aFront =
			std::make_shared< CScatteringSurface >( 0.06, 0.04, 0.46, 0.12, 0.46, 0.52 );
		std::shared_ptr< CScatteringSurface > aBack =
			std::make_shared< CScatteringSurface >( 0.11, 0.26, 0.34, 0.19, 0.64, 0.22 );
		std::shared_ptr< CScatteringLayer > aLayer1 = std::make_shared< CScatteringLayer >( aFront, aBack );

		aFront = std::make_shared< CScatteringSurface >( 0.1, 0.05, 0.48, 0.26, 0.56, 0.34 );
		aBack = std::make_shared< CScatteringSurface >( 0.15, 0, 0.38, 0.19, 0.49, 0.39 );
		std::shared_ptr< CScatteringLayer > aLayer2 = std::make_shared< CScatteringLayer >( aFront, aBack );

		aFront = std::make_shared< CScatteringSurface >( 0.08, 0.05, 0.46, 0.23, 0.46, 0.52 );
		aBack = std::make_shared< CScatteringSurface >( 0.13, 0.25, 0.38, 0.19, 0.64, 0.22 );
		std::shared_ptr< CScatteringLayer > aLayer3 = std::make_shared< CScatteringLayer >( aFront, aBack );

		m_Layer = std::make_shared< CMultiLayerScattered >( aLayer1 );
		m_Layer->addLayer( aLayer2 );
		m_Layer->addLayer( aLayer3 );

	}

public:
	std::shared_ptr< CMultiLayerScattered > getLayer() {
		return m_Layer;
	};

};

TEST_F( TestMultiLayer1, TestTripleLayerFront ) {
	SCOPED_TRACE( "Begin Test: Test triple layer with scattering properties (Front)." );

	CMultiLayerScattered aLayer = *getLayer();

	Side aSide = Side::Front;

	///////////////////////////////////////////////
	// Direct-Direct
	///////////////////////////////////////////////
	Scattering aScattering = Scattering::DirectDirect;

	double Tf = aLayer.getPropertySimple( PropertySimple::T, aSide, aScattering );
	EXPECT_NEAR( 0.000486418, Tf, 1e-6 );

	double Rf = aLayer.getPropertySimple( PropertySimple::R, aSide, aScattering );
	EXPECT_NEAR( 0.040339429, Rf, 1e-6 );

	///////////////////////////////////////////////
	// Direct-Diffuse
	///////////////////////////////////////////////
	aScattering = Scattering::DirectDiffuse;

	Tf = aLayer.getPropertySimple( PropertySimple::T, aSide, aScattering );
	EXPECT_NEAR( 0.190095209, Tf, 1e-6 );

	Rf = aLayer.getPropertySimple( PropertySimple::R, aSide, aScattering );
	EXPECT_NEAR( 0.312631104, Rf, 1e-6 );

	///////////////////////////////////////////////
	// Diffuse-Diffuse
	///////////////////////////////////////////////
	aScattering = Scattering::DiffuseDiffuse;

	Tf = aLayer.getPropertySimple( PropertySimple::T, aSide, aScattering );
	EXPECT_NEAR( 0.167799034, Tf, 1e-6 );

	Rf = aLayer.getPropertySimple( PropertySimple::R, aSide, aScattering );
	EXPECT_NEAR( 0.692483233, Rf, 1e-6 );

}

TEST_F( TestMultiLayer1, TestTripleLayerBack ) {
	SCOPED_TRACE( "Begin Test: Test triple layer with scattering properties (Back)." );

	CMultiLayerScattered aLayer = *getLayer();

	Side aSide = Side::Back;

	///////////////////////////////////////////////
	// Direct-Direct
	///////////////////////////////////////////////
	Scattering aScattering = Scattering::DirectDirect;

	double Tb = aLayer.getPropertySimple( PropertySimple::T, aSide, aScattering );
	EXPECT_NEAR( 0.002173682, Tb, 1e-6 );

	double Rb = aLayer.getPropertySimple( PropertySimple::R, aSide, aScattering );
	EXPECT_NEAR( 0.250041102, Rb, 1e-6 );

	///////////////////////////////////////////////
	// Direct-Diffuse
	///////////////////////////////////////////////
	aScattering = Scattering::DirectDiffuse;

	Tb = aLayer.getPropertySimple( PropertySimple::T, aSide, aScattering );
	EXPECT_NEAR( 0.219867246, Tb, 1e-6 );

	Rb = aLayer.getPropertySimple( PropertySimple::R, aSide, aScattering );
	EXPECT_NEAR( 0.316344401, Rb, 1e-6 );

	///////////////////////////////////////////////
	// Diffuse-Diffuse
	///////////////////////////////////////////////
	aScattering = Scattering::DiffuseDiffuse;

	Tb = aLayer.getPropertySimple( PropertySimple::T, aSide, aScattering );
	EXPECT_NEAR( 0.284211597, Tb, 1e-6 );

	Rb = aLayer.getPropertySimple( PropertySimple::R, aSide, aScattering );
	EXPECT_NEAR( 0.395593248, Rb, 1e-6 );

}

TEST_F( TestMultiLayer1, TestFrontSideAbsorptances ) {
	SCOPED_TRACE( "Begin Test: Triple pane layer by layer absroptances (Front Side)." );

	CMultiLayerScattered aLayer = *getLayer();

	Side aSide = Side::Front;

	// Direct
	ScatteringSimple aScattering = ScatteringSimple::Direct;
	double Af1_dir = aLayer.getAbsorptanceLayer( 1, aSide, aScattering );
	EXPECT_NEAR( 0.362217125, Af1_dir, 1e-6 );

	double Af2_dir = aLayer.getAbsorptanceLayer( 2, aSide, aScattering );
	EXPECT_NEAR( 0.08499287, Af2_dir, 1e-6 );

	double Af3_dir = aLayer.getAbsorptanceLayer( 3, aSide, aScattering );
	EXPECT_NEAR( 0.009237846, Af3_dir, 1e-6 );

	double Aftotal_dir = aLayer.getAbsorptance( aSide, aScattering );
	EXPECT_NEAR( 0.456447841, Aftotal_dir, 1e-6 );

	// Diffuse
	aScattering = ScatteringSimple::Diffuse;
	double Af1_dif = aLayer.getAbsorptanceLayer( 1, aSide, aScattering );
	EXPECT_NEAR( 0.057730707, Af1_dif, 1e-6 );

	double Af2_dif = aLayer.getAbsorptanceLayer( 2, aSide, aScattering );
	EXPECT_NEAR( 0.074691415, Af2_dif, 1e-6 );

	double Af3_dif = aLayer.getAbsorptanceLayer( 3, aSide, aScattering );
	EXPECT_NEAR( 0.00729561, Af3_dif, 1e-6 );

	double Aftotal_dif = aLayer.getAbsorptance( aSide, aScattering );
	EXPECT_NEAR( 0.139717732, Aftotal_dif, 1e-6 );

}

TEST_F( TestMultiLayer1, TestBackSideAbsorptances ) {
	SCOPED_TRACE( "Begin Test: Triple pane layer by layer absroptances (Back Side)." );

	CMultiLayerScattered aLayer = *getLayer();

	Side aSide = Side::Back;

	// Direct
	ScatteringSimple aScattering = ScatteringSimple::Direct;
	double Ab1_dir = aLayer.getAbsorptanceLayer( 1, aSide, aScattering );
	EXPECT_NEAR( 0.048602329, Ab1_dir, 1e-6 );

	double Ab2_dir = aLayer.getAbsorptanceLayer( 2, aSide, aScattering );
	EXPECT_NEAR( 0.1073958, Ab2_dir, 1e-6 );

	double Ab3_dir = aLayer.getAbsorptanceLayer( 3, aSide, aScattering );
	EXPECT_NEAR( 0.05557544, Ab3_dir, 1e-6 );

	double Abtotal_dir = aLayer.getAbsorptance( aSide, aScattering );
	EXPECT_NEAR( 0.211573569, Abtotal_dir, 1e-6 );

	// Diffuse
	aScattering = ScatteringSimple::Diffuse;
	double Ab1_dif = aLayer.getAbsorptanceLayer( 1, aSide, aScattering );
	EXPECT_NEAR( 0.062171287, Ab1_dif, 1e-6 );

	double Ab2_dif = aLayer.getAbsorptanceLayer( 2, aSide, aScattering );
	EXPECT_NEAR( 0.110389379, Ab2_dif, 1e-6 );

	double Ab3_dif = aLayer.getAbsorptanceLayer( 3, aSide, aScattering );
	EXPECT_NEAR( 0.147634489, Ab3_dif, 1e-6 );

	double Abtotal_dif = aLayer.getAbsorptance( aSide, aScattering );
	EXPECT_NEAR( 0.320195155, Abtotal_dif, 1e-6 );

}
