#include <memory>
#include <gtest/gtest.h>

#include "WCEMultiLayerOptics.hpp"
#include "WCESingleLayerOptics.hpp"
#include "WCECommon.hpp"


using namespace MultiLayerOptics;
using namespace SingleLayerOptics;
using namespace FenestrationCommon;

// Calculation of energies that are incoming to the layers surfaces.
// Layers are added at front side. Very similar to TestMultilayerInterreflectances_3
class TestMultilayerInterreflectances_4 : public testing::Test {

private:
	// Additional layer added to the back side
	std::shared_ptr< CInterRef > m_Interref;

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

		m_Interref = std::make_shared< CInterRef >( aLayer3 );
		m_Interref->addLayer( aLayer2, Side::Front );
		m_Interref->addLayer( aLayer1, Side::Front );

	}

public:
	std::shared_ptr< CInterRef > getInt() {
		return m_Interref;
	};

};

TEST_F( TestMultilayerInterreflectances_4, TestForwardFlowFrontSide ) {
	SCOPED_TRACE( "Begin Test: Triple pane equivalent layer properties (Forward flow - Front Side)." );

	CInterRef eqLayer = *getInt();

	EnergyFlow aFlow = EnergyFlow::Forward;
	Side aSide = Side::Front;

	// Direct-Direct
	Scattering aScattering = Scattering::DirectDirect;
	double If1 = eqLayer.getEnergyToSurface( 1, aSide, aFlow, aScattering );
	EXPECT_NEAR( 1.0, If1, 1e-6 );

	double If2 = eqLayer.getEnergyToSurface( 2, aSide, aFlow, aScattering );
	EXPECT_NEAR( 0.060802286, If2, 1e-6 );

	double If3 = eqLayer.getEnergyToSurface( 3, aSide, aFlow, aScattering );
	EXPECT_NEAR( 0.006080229, If3, 1e-6 );

	// Diffuse-Diffuse
	aScattering = Scattering::DiffuseDiffuse;
	If1 = eqLayer.getEnergyToSurface( 1, aSide, aFlow, aScattering );
	EXPECT_NEAR( 1.0, If1, 1e-6 );

	If2 = eqLayer.getEnergyToSurface( 2, aSide, aFlow, aScattering );
	EXPECT_NEAR( 0.519291111, If2, 1e-6 );

	If3 = eqLayer.getEnergyToSurface( 3, aSide, aFlow, aScattering );
	EXPECT_NEAR( 0.36478051, If3, 1e-6 );

	// Direct-Diffuse
	aScattering = Scattering::DirectDiffuse;
	If1 = eqLayer.getEnergyToSurface( 1, aSide, aFlow, aScattering );
	EXPECT_NEAR( 0, If1, 1e-6 );

	If2 = eqLayer.getEnergyToSurface( 2, aSide, aFlow, aScattering );
	EXPECT_NEAR( 0.526442585, If2, 1e-6 );

	If3 = eqLayer.getEnergyToSurface( 3, aSide, aFlow, aScattering );
	EXPECT_NEAR( 0.407170225, If3, 1e-6 );

}

TEST_F( TestMultilayerInterreflectances_4, TestForwardFlowBackSide ) {
	SCOPED_TRACE( "Begin Test: Triple pane equivalent layer properties (Forward flow - Back Side)." );

	CInterRef eqLayer = *getInt();

	EnergyFlow aFlow = EnergyFlow::Forward;
	Side aSide = Side::Back;

	// Direct-Direct
	Scattering aScattering = Scattering::DirectDirect;
	double If1 = eqLayer.getEnergyToSurface( 1, aSide, aFlow, aScattering );
	EXPECT_NEAR( 0.003085716, If1, 1e-6 );

	double If2 = eqLayer.getEnergyToSurface( 2, aSide, aFlow, aScattering );
	EXPECT_NEAR( 0.000304011, If2, 1e-6 );

	double If3 = eqLayer.getEnergyToSurface( 3, aSide, aFlow, aScattering );
	EXPECT_NEAR( 0, If3, 1e-6 );

	// Diffuse-Diffuse
	aScattering = Scattering::DiffuseDiffuse;
	If1 = eqLayer.getEnergyToSurface( 1, aSide, aFlow, aScattering );
	EXPECT_NEAR( 0.269505052, If1, 1e-6 );

	If2 = eqLayer.getEnergyToSurface( 2, aSide, aFlow, aScattering );
	EXPECT_NEAR( 0.189685865, If2, 1e-6 );

	If3 = eqLayer.getEnergyToSurface( 3, aSide, aFlow, aScattering );
	EXPECT_NEAR( 0, If3, 1e-6 );

	// Direct-Diffuse
	aScattering = Scattering::DirectDiffuse;
	If1 = eqLayer.getEnergyToSurface( 1, aSide, aFlow, aScattering );
	EXPECT_NEAR( 0.299346813, If1, 1e-6 );

	If2 = eqLayer.getEnergyToSurface( 2, aSide, aFlow, aScattering );
	EXPECT_NEAR( 0.21312697, If2, 1e-6 );

	If3 = eqLayer.getEnergyToSurface( 3, aSide, aFlow, aScattering );
	EXPECT_NEAR( 0, If3, 1e-6 );

}

TEST_F( TestMultilayerInterreflectances_4, TestBackwardFlowFrontSide ) {
	SCOPED_TRACE( "Begin Test: Triple pane equivalent layer properties (Backward flow - Front Side)." );

	CInterRef eqLayer = *getInt();

	EnergyFlow aFlow = EnergyFlow::Backward;
	Side aSide = Side::Front;

	// Direct-Direct
	Scattering aScattering = Scattering::DirectDirect;
	double If1 = eqLayer.getEnergyToSurface( 1, aSide, aFlow, aScattering );
	EXPECT_NEAR( 0, If1, 1e-6 );

	double If2 = eqLayer.getEnergyToSurface( 2, aSide, aFlow, aScattering );
	EXPECT_NEAR( 0.005137793, If2, 1e-6 );

	double If3 = eqLayer.getEnergyToSurface( 3, aSide, aFlow, aScattering );
	EXPECT_NEAR( 0.000513779, If3, 1e-6 );

	// Diffuse-Diffuse
	aScattering = Scattering::DiffuseDiffuse;
	If1 = eqLayer.getEnergyToSurface( 1, aSide, aFlow, aScattering );
	EXPECT_NEAR( 0, If1, 1e-6 );

	If2 = eqLayer.getEnergyToSurface( 2, aSide, aFlow, aScattering );
	EXPECT_NEAR( 0.097697737, If2, 1e-6 );

	If3 = eqLayer.getEnergyToSurface( 3, aSide, aFlow, aScattering );
	EXPECT_NEAR( 0.381724451, If3, 1e-6 );

	// Direct-Diffuse
	aScattering = Scattering::DirectDiffuse;
	If1 = eqLayer.getEnergyToSurface( 1, aSide, aFlow, aScattering );
	EXPECT_NEAR( 0, If1, 1e-6 );

	If2 = eqLayer.getEnergyToSurface( 2, aSide, aFlow, aScattering );
	EXPECT_NEAR( 0.07702437, If2, 1e-6 );

	If3 = eqLayer.getEnergyToSurface( 3, aSide, aFlow, aScattering );
	EXPECT_NEAR( 0.274147962, If3, 1e-6 );

}

TEST_F( TestMultilayerInterreflectances_4, TestBackwardFlowBackSide ) {
	SCOPED_TRACE( "Begin Test: Triple pane equivalent layer properties (Backward flow - Back Side)." );

	CInterRef eqLayer = *getInt();

	EnergyFlow aFlow = EnergyFlow::Backward;
	Side aSide = Side::Back;

	// Direct-Direct
	Scattering aScattering = Scattering::DirectDirect;
	double If1 = eqLayer.getEnergyToSurface( 1, aSide, aFlow, aScattering );
	EXPECT_NEAR( 0.019760743, If1, 1e-6 );

	double If2 = eqLayer.getEnergyToSurface( 2, aSide, aFlow, aScattering );
	EXPECT_NEAR( 0.130025689, If2, 1e-6 );

	double If3 = eqLayer.getEnergyToSurface( 3, aSide, aFlow, aScattering );
	EXPECT_NEAR( 1, If3, 1e-6 );

	// Diffuse-Diffuse
	aScattering = Scattering::DiffuseDiffuse;
	If1 = eqLayer.getEnergyToSurface( 1, aSide, aFlow, aScattering );
	EXPECT_NEAR( 0.444080621, If1, 1e-6 );

	If2 = eqLayer.getEnergyToSurface( 2, aSide, aFlow, aScattering );
	EXPECT_NEAR( 0.838496715, If2, 1e-6 );

	If3 = eqLayer.getEnergyToSurface( 3, aSide, aFlow, aScattering );
	EXPECT_NEAR( 1, If3, 1e-6 );

	// Direct-Diffuse
	aScattering = Scattering::DirectDiffuse;
	If1 = eqLayer.getEnergyToSurface( 1, aSide, aFlow, aScattering );
	EXPECT_NEAR( 0.333044677, If1, 1e-6 );

	If2 = eqLayer.getEnergyToSurface( 2, aSide, aFlow, aScattering );
	EXPECT_NEAR( 0.522675109, If2, 1e-6 );

	If3 = eqLayer.getEnergyToSurface( 3, aSide, aFlow, aScattering );
	EXPECT_NEAR( 0, If3, 1e-6 );

}

TEST_F( TestMultilayerInterreflectances_4, TestFrontSideAbsorptances ) {
	SCOPED_TRACE( "Begin Test: Triple pane layer by layer absroptances (Front Side)." );

	CInterRef eqLayer = *getInt();

	Side aSide = Side::Front;

	// Direct
	ScatteringSimple aScattering = ScatteringSimple::Direct;
	double Af1_dir = eqLayer.getAbsorptance( 1, aSide, aScattering );
	EXPECT_NEAR( 0.362217125, Af1_dir, 1e-6 );

	double Af2_dir = eqLayer.getAbsorptance( 2, aSide, aScattering );
	EXPECT_NEAR( 0.08499287, Af2_dir, 1e-6 );

	double Af3_dir = eqLayer.getAbsorptance( 3, aSide, aScattering );
	EXPECT_NEAR( 0.009237846, Af3_dir, 1e-6 );

	// Diffuse
	aScattering = ScatteringSimple::Diffuse;
	double Af1_dif = eqLayer.getAbsorptance( 1, aSide, aScattering );
	EXPECT_NEAR( 0.057730707, Af1_dif, 1e-6 );

	double Af2_dif = eqLayer.getAbsorptance( 2, aSide, aScattering );
	EXPECT_NEAR( 0.074691415, Af2_dif, 1e-6 );

	double Af3_dif = eqLayer.getAbsorptance( 3, aSide, aScattering );
	EXPECT_NEAR( 0.00729561, Af3_dif, 1e-6 );

}

TEST_F( TestMultilayerInterreflectances_4, TestBackSideAbsorptances ) {
	SCOPED_TRACE( "Begin Test: Triple pane layer by layer absroptances (Back Side)." );

	CInterRef eqLayer = *getInt();

	Side aSide = Side::Back;

	// Direct
	ScatteringSimple aScattering = ScatteringSimple::Direct;
	double Ab1_dir = eqLayer.getAbsorptance( 1, aSide, aScattering );
	EXPECT_NEAR( 0.048602329, Ab1_dir, 1e-6 );

	double Ab2_dir = eqLayer.getAbsorptance( 2, aSide, aScattering );
	EXPECT_NEAR( 0.1073958, Ab2_dir, 1e-6 );

	double Ab3_dir = eqLayer.getAbsorptance( 3, aSide, aScattering );
	EXPECT_NEAR( 0.05557544, Ab3_dir, 1e-6 );

	// Diffuse
	aScattering = ScatteringSimple::Diffuse;
	double Ab1_dif = eqLayer.getAbsorptance( 1, aSide, aScattering );
	EXPECT_NEAR( 0.062171287, Ab1_dif, 1e-6 );

	double Ab2_dif = eqLayer.getAbsorptance( 2, aSide, aScattering );
	EXPECT_NEAR( 0.110389379, Ab2_dif, 1e-6 );

	double Ab3_dif = eqLayer.getAbsorptance( 3, aSide, aScattering );
	EXPECT_NEAR( 0.147634489, Ab3_dif, 1e-6 );

}
