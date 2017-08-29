#include <memory>
#include <gtest/gtest.h>

#include "WCESpectralAveraging.hpp"
#include "WCEMultiLayerOptics.hpp"
#include "WCESingleLayerOptics.hpp"
#include "WCECommon.hpp"

using namespace std;
using namespace SingleLayerOptics;
using namespace FenestrationCommon;
using namespace SpectralAveraging;
using namespace MultiLayerOptics;

// Example on how to create scattered multilayer when only visible and solar spectrums are provided.

class MultiPaneScattered_102_DualMaterial : public testing::Test {

private:
	std::shared_ptr< CMultiLayerScattered > m_Layer;

	std::shared_ptr< CSeries > loadSolarRadiationFile() {

		std::shared_ptr< CSeries > aSolarRadiation = make_shared< CSeries >();

		// Deafult solar radiation in EnergyPlus (year 2017)
		aSolarRadiation->addProperty( 0.3000, 0 );
		aSolarRadiation->addProperty( 0.3050, 9.5 );
		aSolarRadiation->addProperty( 0.3100, 42.3 );
		aSolarRadiation->addProperty( 0.3150, 107.8 );
		aSolarRadiation->addProperty( 0.3200, 181 );
		aSolarRadiation->addProperty( 0.3250, 246 );
		aSolarRadiation->addProperty( 0.3300, 395.3 );
		aSolarRadiation->addProperty( 0.3350, 390.1 );
		aSolarRadiation->addProperty( 0.3400, 435.3 );
		aSolarRadiation->addProperty( 0.3450, 438.9 );
		aSolarRadiation->addProperty( 0.3500, 483.7 );
		aSolarRadiation->addProperty( 0.3600, 520.3 );
		aSolarRadiation->addProperty( 0.3700, 666.2 );
		aSolarRadiation->addProperty( 0.3800, 712.5 );
		aSolarRadiation->addProperty( 0.3900, 720.7 );
		aSolarRadiation->addProperty( 0.4000, 1013.1 );
		aSolarRadiation->addProperty( 0.4100, 1158.2 );
		aSolarRadiation->addProperty( 0.4200, 1184 );
		aSolarRadiation->addProperty( 0.4300, 1071.9 );
		aSolarRadiation->addProperty( 0.4400, 1302 );
		aSolarRadiation->addProperty( 0.4500, 1526 );
		aSolarRadiation->addProperty( 0.4600, 1599.6 );
		aSolarRadiation->addProperty( 0.4700, 1581 );
		aSolarRadiation->addProperty( 0.4800, 1628.3 );
		aSolarRadiation->addProperty( 0.4900, 1539.2 );
		aSolarRadiation->addProperty( 0.5000, 1548.7 );
		aSolarRadiation->addProperty( 0.5100, 1586.5 );
		aSolarRadiation->addProperty( 0.5200, 1484.9 );
		aSolarRadiation->addProperty( 0.5300, 1572.4 );
		aSolarRadiation->addProperty( 0.5400, 1550.7 );
		aSolarRadiation->addProperty( 0.5500, 1561.5 );
		aSolarRadiation->addProperty( 0.5700, 1501.5 );
		aSolarRadiation->addProperty( 0.5900, 1395.5 );
		aSolarRadiation->addProperty( 0.6100, 1485.3 );
		aSolarRadiation->addProperty( 0.6300, 1434.1 );
		aSolarRadiation->addProperty( 0.6500, 1419.9 );
		aSolarRadiation->addProperty( 0.6700, 1392.3 );
		aSolarRadiation->addProperty( 0.6900, 1130 );
		aSolarRadiation->addProperty( 0.7100, 1316.7 );
		aSolarRadiation->addProperty( 0.7180, 1010.3 );
		aSolarRadiation->addProperty( 0.7244, 1043.2 );
		aSolarRadiation->addProperty( 0.7400, 1211.2 );
		aSolarRadiation->addProperty( 0.7525, 1193.9 );
		aSolarRadiation->addProperty( 0.7575, 1175.5 );
		aSolarRadiation->addProperty( 0.7625, 643.1 );
		aSolarRadiation->addProperty( 0.7675, 1030.7 );
		aSolarRadiation->addProperty( 0.7800, 1131.1 );
		aSolarRadiation->addProperty( 0.8000, 1081.6 );
		aSolarRadiation->addProperty( 0.8160, 849.2 );
		aSolarRadiation->addProperty( 0.8237, 785 );
		aSolarRadiation->addProperty( 0.8315, 916.4 );
		aSolarRadiation->addProperty( 0.8400, 959.9 );
		aSolarRadiation->addProperty( 0.8600, 978.9 );
		aSolarRadiation->addProperty( 0.8800, 933.2 );
		aSolarRadiation->addProperty( 0.9050, 748.5 );
		aSolarRadiation->addProperty( 0.9150, 667.5 );
		aSolarRadiation->addProperty( 0.9250, 690.3 );
		aSolarRadiation->addProperty( 0.9300, 403.6 );
		aSolarRadiation->addProperty( 0.9370, 258.3 );
		aSolarRadiation->addProperty( 0.9480, 313.6 );
		aSolarRadiation->addProperty( 0.9650, 526.8 );
		aSolarRadiation->addProperty( 0.9800, 646.4 );
		aSolarRadiation->addProperty( 0.9935, 746.8 );
		aSolarRadiation->addProperty( 1.0400, 690.5 );
		aSolarRadiation->addProperty( 1.0700, 637.5 );
		aSolarRadiation->addProperty( 1.1000, 412.6 );
		aSolarRadiation->addProperty( 1.1200, 108.9 );
		aSolarRadiation->addProperty( 1.1300, 189.1 );
		aSolarRadiation->addProperty( 1.1370, 132.2 );
		aSolarRadiation->addProperty( 1.1610, 339 );
		aSolarRadiation->addProperty( 1.1800, 460 );
		aSolarRadiation->addProperty( 1.2000, 423.6 );
		aSolarRadiation->addProperty( 1.2350, 480.5 );
		aSolarRadiation->addProperty( 1.2900, 413.1 );
		aSolarRadiation->addProperty( 1.3200, 250.2 );
		aSolarRadiation->addProperty( 1.3500, 32.5 );
		aSolarRadiation->addProperty( 1.3950, 1.6 );
		aSolarRadiation->addProperty( 1.4425, 55.7 );
		aSolarRadiation->addProperty( 1.4625, 105.1 );
		aSolarRadiation->addProperty( 1.4770, 105.5 );
		aSolarRadiation->addProperty( 1.4970, 182.1 );
		aSolarRadiation->addProperty( 1.5200, 262.2 );
		aSolarRadiation->addProperty( 1.5390, 274.2 );
		aSolarRadiation->addProperty( 1.5580, 275 );
		aSolarRadiation->addProperty( 1.5780, 244.6 );
		aSolarRadiation->addProperty( 1.5920, 247.4 );
		aSolarRadiation->addProperty( 1.6100, 228.7 );
		aSolarRadiation->addProperty( 1.6300, 244.5 );
		aSolarRadiation->addProperty( 1.6460, 234.8 );
		aSolarRadiation->addProperty( 1.6780, 220.5 );
		aSolarRadiation->addProperty( 1.7400, 171.5 );
		aSolarRadiation->addProperty( 1.8000, 30.7 );
		aSolarRadiation->addProperty( 1.8600, 2 );
		aSolarRadiation->addProperty( 1.9200, 1.2 );
		aSolarRadiation->addProperty( 1.9600, 21.2 );
		aSolarRadiation->addProperty( 1.9850, 91.1 );
		aSolarRadiation->addProperty( 2.0050, 26.8 );
		aSolarRadiation->addProperty( 2.0350, 99.5 );
		aSolarRadiation->addProperty( 2.0650, 60.4 );
		aSolarRadiation->addProperty( 2.1000, 89.1 );
		aSolarRadiation->addProperty( 2.1480, 82.2 );
		aSolarRadiation->addProperty( 2.1980, 71.5 );
		aSolarRadiation->addProperty( 2.2700, 70.2 );
		aSolarRadiation->addProperty( 2.3600, 62 );
		aSolarRadiation->addProperty( 2.4500, 21.2 );
		aSolarRadiation->addProperty( 2.4940, 18.5 );
		aSolarRadiation->addProperty( 2.5370, 3.2 );

		return aSolarRadiation;
	}

protected:
	virtual void SetUp() {

		// Material properties in solar range
		double Tmat = 0.833848;
		double Rfmat = 7.476376e-002;
		double Rbmat = 7.485449e-002;
		std::shared_ptr< CMaterial > aSolarRangeMaterial =
			std::make_shared< CMaterialSingleBand >( Tmat, Tmat, Rfmat, Rbmat, WavelengthRange::Solar );

		// Material properties in visible range
		Tmat = 0.899260;
		Rfmat = 0.082563;
		Rbmat = 0.082564;
		std::shared_ptr< CMaterial > aVisibleRangeMaterial =
			std::make_shared< CMaterialSingleBand >( Tmat, Tmat, Rfmat, Rbmat, WavelengthRange::Visible );

		std::shared_ptr< CMaterial > aMaterialDual =
			std::make_shared< CMaterialDualBand >( aVisibleRangeMaterial, aSolarRangeMaterial );

		std::shared_ptr< CSpectralSample > aSample = aMaterialDual->getSpectralSample();

		// Create material from samples
		double thickness = 3.048e-3; // [m]
		std::shared_ptr< CMaterial > aMaterial_102 = make_shared< CMaterialSample >( aSample,
		                                                                        thickness, MaterialType::Monolithic, WavelengthRange::Solar );

		std::shared_ptr< CScatteringLayer > Layer_102 = make_shared< CScatteringLayer >( aMaterial_102 );

		// Equivalent scattering layer
		m_Layer = make_shared< CMultiLayerScattered >( Layer_102 );

		std::shared_ptr< CSeries > aSolarRadiation = loadSolarRadiationFile();
		m_Layer->setSourceData( aSolarRadiation );

	}

public:
	std::shared_ptr< CMultiLayerScattered > getLayer() {
		return m_Layer;
	};

};

TEST_F( MultiPaneScattered_102_DualMaterial, TestSpecular1 ) {
	SCOPED_TRACE( "Begin Test: Specular layer - Scattering model front side (normal incidence)." );

	CMultiLayerScattered& aLayer = *getLayer();

	Side aSide = Side::Front;
	double theta = 0;
	double phi = 0;

	double T_dir_dir = aLayer.getPropertySimple( PropertySimple::T, aSide, Scattering::DirectDirect, theta, phi );
	EXPECT_NEAR( 0.823900, T_dir_dir, 1e-6 );

	double T_dir_dif = aLayer.getPropertySimple( PropertySimple::T, aSide, Scattering::DirectDiffuse, theta, phi );
	EXPECT_NEAR( 0, T_dir_dif, 1e-6 );

	double T_dif_dif = aLayer.getPropertySimple( PropertySimple::T, aSide, Scattering::DiffuseDiffuse, theta, phi );
	EXPECT_NEAR( 0.742249, T_dif_dif, 1e-6 );

	double R_dir_dir = aLayer.getPropertySimple( PropertySimple::R, aSide, Scattering::DirectDirect, theta, phi );
	EXPECT_NEAR( 0.073578, R_dir_dir, 1e-6 );

	double R_dir_dif = aLayer.getPropertySimple( PropertySimple::R, aSide, Scattering::DirectDiffuse, theta, phi );
	EXPECT_NEAR( 0, R_dir_dif, 1e-6 );

	double R_dif_dif = aLayer.getPropertySimple( PropertySimple::R, aSide, Scattering::DiffuseDiffuse, theta, phi );
	EXPECT_NEAR( 0.144232, R_dif_dif, 1e-6 );

	double A_dir1 = aLayer.getAbsorptanceLayer( 1, aSide, ScatteringSimple::Direct, theta, phi );
	EXPECT_NEAR( 0.102523, A_dir1, 1e-6 );

	double A_dif1 = aLayer.getAbsorptanceLayer( 1, aSide, ScatteringSimple::Diffuse, theta, phi );
	EXPECT_NEAR( 0.113519, A_dif1, 1e-6 );

}

TEST_F( MultiPaneScattered_102_DualMaterial, TestSpecular2 ) {
	SCOPED_TRACE( "Begin Test: Specular layer - Scattering model back side (normal incidence)." );

	CMultiLayerScattered& aLayer = *getLayer();

	Side aSide = Side::Back;
	double theta = 0;
	double phi = 0;

	double T_dir_dir = aLayer.getPropertySimple( PropertySimple::T, aSide, Scattering::DirectDirect, theta, phi );
	EXPECT_NEAR( 0.823900, T_dir_dir, 1e-6 );

	double T_dir_dif = aLayer.getPropertySimple( PropertySimple::T, aSide, Scattering::DirectDiffuse, theta, phi );
	EXPECT_NEAR( 0, T_dir_dif, 1e-6 );

	double T_dif_dif = aLayer.getPropertySimple( PropertySimple::T, aSide, Scattering::DiffuseDiffuse, theta, phi );
	EXPECT_NEAR( 0.742249, T_dif_dif, 1e-6 );

	double R_dir_dir = aLayer.getPropertySimple( PropertySimple::R, aSide, Scattering::DirectDirect, theta, phi );
	EXPECT_NEAR( 0.073682, R_dir_dir, 1e-6 );

	double R_dir_dif = aLayer.getPropertySimple( PropertySimple::R, aSide, Scattering::DirectDiffuse, theta, phi );
	EXPECT_NEAR( 0, R_dir_dif, 1e-6 );

	double R_dif_dif = aLayer.getPropertySimple( PropertySimple::R, aSide, Scattering::DiffuseDiffuse, theta, phi );
	EXPECT_NEAR( 0.144340, R_dif_dif, 1e-6 );

	double A_dir1 = aLayer.getAbsorptanceLayer( 1, aSide, ScatteringSimple::Direct, theta, phi );
	EXPECT_NEAR( 0.102418, A_dir1, 1e-6 );

	double A_dif1 = aLayer.getAbsorptanceLayer( 1, aSide, ScatteringSimple::Diffuse, theta, phi );
	EXPECT_NEAR( 0.113411, A_dif1, 1e-6 );

}

TEST_F( MultiPaneScattered_102_DualMaterial, TestSpecular3 ) {
	SCOPED_TRACE( "Begin Test: Specular layer - Scattering model front side (Theta = 40 deg)." );

	CMultiLayerScattered& aLayer = *getLayer();

	Side aSide = Side::Front;
	double theta = 40;
	double phi = 0;

	double T_dir_dir = aLayer.getPropertySimple( PropertySimple::T, aSide, Scattering::DirectDirect, theta, phi );
	EXPECT_NEAR( 0.806694, T_dir_dir, 1e-6 );

	double T_dir_dif = aLayer.getPropertySimple( PropertySimple::T, aSide, Scattering::DirectDiffuse, theta, phi );
	EXPECT_NEAR( 0, T_dir_dif, 1e-6 );

	double T_dif_dif = aLayer.getPropertySimple( PropertySimple::T, aSide, Scattering::DiffuseDiffuse, theta, phi );
	EXPECT_NEAR( 0.742249, T_dif_dif, 1e-6 );

	double R_dir_dir = aLayer.getPropertySimple( PropertySimple::R, aSide, Scattering::DirectDirect, theta, phi );
	EXPECT_NEAR( 0.081144, R_dir_dir, 1e-6 );

	double R_dir_dif = aLayer.getPropertySimple( PropertySimple::R, aSide, Scattering::DirectDiffuse, theta, phi );
	EXPECT_NEAR( 0, R_dir_dif, 1e-6 );

	double R_dif_dif = aLayer.getPropertySimple( PropertySimple::R, aSide, Scattering::DiffuseDiffuse, theta, phi );
	EXPECT_NEAR( 0.144232, R_dif_dif, 1e-6 );

	double A_dir1 = aLayer.getAbsorptanceLayer( 1, aSide, ScatteringSimple::Direct, theta, phi );
	EXPECT_NEAR( 0.112162, A_dir1, 1e-6 );

	double A_dif1 = aLayer.getAbsorptanceLayer( 1, aSide, ScatteringSimple::Diffuse, theta, phi );
	EXPECT_NEAR( 0.113519, A_dif1, 1e-6 );

}
