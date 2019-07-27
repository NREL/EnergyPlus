#include <memory>
#include <gtest/gtest.h>

#include "WCESpectralAveraging.hpp"
#include "WCEMultiLayerOptics.hpp"
#include "WCESingleLayerOptics.hpp"
#include "WCECommon.hpp"


using namespace SingleLayerOptics;
using namespace FenestrationCommon;
using namespace SpectralAveraging;
using namespace MultiLayerOptics;

// Example on how to create scattered multilayer.

class MultiPaneScattered_102_NonStandardSolar : public testing::Test {

private:
	std::shared_ptr< CMultiLayerScattered > m_Layer;

	std::shared_ptr< CSeries > loadSolarRadiationFile() {

		std::shared_ptr< CSeries > aSolarRadiation = std::make_shared< CSeries >();

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

	std::shared_ptr< CSpectralSampleData > loadSampleData_NFRC_102() {
		std::shared_ptr< CSpectralSampleData > aMeasurements_102 = std::make_shared< CSpectralSampleData >();

		aMeasurements_102->addRecord( 0.300, 0.0020, 0.0470, 0.0480 );
		aMeasurements_102->addRecord( 0.305, 0.0030, 0.0470, 0.0480 );
		aMeasurements_102->addRecord( 0.310, 0.0090, 0.0470, 0.0480 );
		aMeasurements_102->addRecord( 0.315, 0.0350, 0.0470, 0.0480 );
		aMeasurements_102->addRecord( 0.320, 0.1000, 0.0470, 0.0480 );
		aMeasurements_102->addRecord( 0.325, 0.2180, 0.0490, 0.0500 );
		aMeasurements_102->addRecord( 0.330, 0.3560, 0.0530, 0.0540 );
		aMeasurements_102->addRecord( 0.335, 0.4980, 0.0600, 0.0610 );
		aMeasurements_102->addRecord( 0.340, 0.6160, 0.0670, 0.0670 );
		aMeasurements_102->addRecord( 0.345, 0.7090, 0.0730, 0.0740 );
		aMeasurements_102->addRecord( 0.350, 0.7740, 0.0780, 0.0790 );
		aMeasurements_102->addRecord( 0.355, 0.8180, 0.0820, 0.0820 );
		aMeasurements_102->addRecord( 0.360, 0.8470, 0.0840, 0.0840 );
		aMeasurements_102->addRecord( 0.365, 0.8630, 0.0850, 0.0850 );
		aMeasurements_102->addRecord( 0.370, 0.8690, 0.0850, 0.0860 );
		aMeasurements_102->addRecord( 0.375, 0.8610, 0.0850, 0.0850 );
		aMeasurements_102->addRecord( 0.380, 0.8560, 0.0840, 0.0840 );
		aMeasurements_102->addRecord( 0.385, 0.8660, 0.0850, 0.0850 );
		aMeasurements_102->addRecord( 0.390, 0.8810, 0.0860, 0.0860 );
		aMeasurements_102->addRecord( 0.395, 0.8890, 0.0860, 0.0860 );
		aMeasurements_102->addRecord( 0.400, 0.8930, 0.0860, 0.0860 );
		aMeasurements_102->addRecord( 0.410, 0.8930, 0.0860, 0.0860 );
		aMeasurements_102->addRecord( 0.420, 0.8920, 0.0860, 0.0860 );
		aMeasurements_102->addRecord( 0.430, 0.8920, 0.0850, 0.0850 );
		aMeasurements_102->addRecord( 0.440, 0.8920, 0.0850, 0.0850 );
		aMeasurements_102->addRecord( 0.450, 0.8960, 0.0850, 0.0850 );
		aMeasurements_102->addRecord( 0.460, 0.9000, 0.0850, 0.0850 );
		aMeasurements_102->addRecord( 0.470, 0.9020, 0.0840, 0.0840 );
		aMeasurements_102->addRecord( 0.480, 0.9030, 0.0840, 0.0840 );
		aMeasurements_102->addRecord( 0.490, 0.9040, 0.0850, 0.0850 );
		aMeasurements_102->addRecord( 0.500, 0.9050, 0.0840, 0.0840 );
		aMeasurements_102->addRecord( 0.510, 0.9050, 0.0840, 0.0840 );
		aMeasurements_102->addRecord( 0.520, 0.9050, 0.0840, 0.0840 );
		aMeasurements_102->addRecord( 0.530, 0.9040, 0.0840, 0.0840 );
		aMeasurements_102->addRecord( 0.540, 0.9040, 0.0830, 0.0830 );
		aMeasurements_102->addRecord( 0.550, 0.9030, 0.0830, 0.0830 );
		aMeasurements_102->addRecord( 0.560, 0.9020, 0.0830, 0.0830 );
		aMeasurements_102->addRecord( 0.570, 0.9000, 0.0820, 0.0820 );
		aMeasurements_102->addRecord( 0.580, 0.8980, 0.0820, 0.0820 );
		aMeasurements_102->addRecord( 0.590, 0.8960, 0.0810, 0.0810 );
		aMeasurements_102->addRecord( 0.600, 0.8930, 0.0810, 0.0810 );
		aMeasurements_102->addRecord( 0.610, 0.8900, 0.0810, 0.0810 );
		aMeasurements_102->addRecord( 0.620, 0.8860, 0.0800, 0.0800 );
		aMeasurements_102->addRecord( 0.630, 0.8830, 0.0800, 0.0800 );
		aMeasurements_102->addRecord( 0.640, 0.8790, 0.0790, 0.0790 );
		aMeasurements_102->addRecord( 0.650, 0.8750, 0.0790, 0.0790 );
		aMeasurements_102->addRecord( 0.660, 0.8720, 0.0790, 0.0790 );
		aMeasurements_102->addRecord( 0.670, 0.8680, 0.0780, 0.0780 );
		aMeasurements_102->addRecord( 0.680, 0.8630, 0.0780, 0.0780 );
		aMeasurements_102->addRecord( 0.690, 0.8590, 0.0770, 0.0770 );
		aMeasurements_102->addRecord( 0.700, 0.8540, 0.0760, 0.0770 );
		aMeasurements_102->addRecord( 0.710, 0.8500, 0.0760, 0.0760 );
		aMeasurements_102->addRecord( 0.720, 0.8450, 0.0750, 0.0760 );
		aMeasurements_102->addRecord( 0.730, 0.8400, 0.0750, 0.0750 );
		aMeasurements_102->addRecord( 0.740, 0.8350, 0.0750, 0.0750 );
		aMeasurements_102->addRecord( 0.750, 0.8310, 0.0740, 0.0740 );
		aMeasurements_102->addRecord( 0.760, 0.8260, 0.0740, 0.0740 );
		aMeasurements_102->addRecord( 0.770, 0.8210, 0.0740, 0.0740 );
		aMeasurements_102->addRecord( 0.780, 0.8160, 0.0730, 0.0730 );
		aMeasurements_102->addRecord( 0.790, 0.8120, 0.0730, 0.0730 );
		aMeasurements_102->addRecord( 0.800, 0.8080, 0.0720, 0.0720 );
		aMeasurements_102->addRecord( 0.810, 0.8030, 0.0720, 0.0720 );
		aMeasurements_102->addRecord( 0.820, 0.8000, 0.0720, 0.0720 );
		aMeasurements_102->addRecord( 0.830, 0.7960, 0.0710, 0.0710 );
		aMeasurements_102->addRecord( 0.840, 0.7930, 0.0700, 0.0710 );
		aMeasurements_102->addRecord( 0.850, 0.7880, 0.0700, 0.0710 );
		aMeasurements_102->addRecord( 0.860, 0.7860, 0.0700, 0.0700 );
		aMeasurements_102->addRecord( 0.870, 0.7820, 0.0740, 0.0740 );
		aMeasurements_102->addRecord( 0.880, 0.7800, 0.0720, 0.0720 );
		aMeasurements_102->addRecord( 0.890, 0.7770, 0.0730, 0.0740 );
		aMeasurements_102->addRecord( 0.900, 0.7760, 0.0720, 0.0720 );
		aMeasurements_102->addRecord( 0.910, 0.7730, 0.0720, 0.0720 );
		aMeasurements_102->addRecord( 0.920, 0.7710, 0.0710, 0.0710 );
		aMeasurements_102->addRecord( 0.930, 0.7700, 0.0700, 0.0700 );
		aMeasurements_102->addRecord( 0.940, 0.7680, 0.0690, 0.0690 );
		aMeasurements_102->addRecord( 0.950, 0.7660, 0.0680, 0.0680 );
		aMeasurements_102->addRecord( 0.960, 0.7660, 0.0670, 0.0680 );
		aMeasurements_102->addRecord( 0.970, 0.7640, 0.0680, 0.0680 );
		aMeasurements_102->addRecord( 0.980, 0.7630, 0.0680, 0.0680 );
		aMeasurements_102->addRecord( 0.990, 0.7620, 0.0670, 0.0670 );
		aMeasurements_102->addRecord( 1.000, 0.7620, 0.0660, 0.0670 );
		aMeasurements_102->addRecord( 1.050, 0.7600, 0.0660, 0.0660 );
		aMeasurements_102->addRecord( 1.100, 0.7590, 0.0660, 0.0660 );
		aMeasurements_102->addRecord( 1.150, 0.7610, 0.0660, 0.0660 );
		aMeasurements_102->addRecord( 1.200, 0.7650, 0.0660, 0.0660 );
		aMeasurements_102->addRecord( 1.250, 0.7700, 0.0650, 0.0650 );
		aMeasurements_102->addRecord( 1.300, 0.7770, 0.0670, 0.0670 );
		aMeasurements_102->addRecord( 1.350, 0.7860, 0.0660, 0.0670 );
		aMeasurements_102->addRecord( 1.400, 0.7950, 0.0670, 0.0680 );
		aMeasurements_102->addRecord( 1.450, 0.8080, 0.0670, 0.0670 );
		aMeasurements_102->addRecord( 1.500, 0.8190, 0.0690, 0.0690 );
		aMeasurements_102->addRecord( 1.550, 0.8290, 0.0690, 0.0690 );
		aMeasurements_102->addRecord( 1.600, 0.8360, 0.0700, 0.0700 );
		aMeasurements_102->addRecord( 1.650, 0.8400, 0.0700, 0.0700 );
		aMeasurements_102->addRecord( 1.700, 0.8420, 0.0690, 0.0700 );
		aMeasurements_102->addRecord( 1.750, 0.8420, 0.0690, 0.0700 );
		aMeasurements_102->addRecord( 1.800, 0.8410, 0.0700, 0.0700 );
		aMeasurements_102->addRecord( 1.850, 0.8400, 0.0690, 0.0690 );
		aMeasurements_102->addRecord( 1.900, 0.8390, 0.0680, 0.0680 );
		aMeasurements_102->addRecord( 1.950, 0.8390, 0.0710, 0.0710 );
		aMeasurements_102->addRecord( 2.000, 0.8390, 0.0690, 0.0690 );
		aMeasurements_102->addRecord( 2.050, 0.8400, 0.0680, 0.0680 );
		aMeasurements_102->addRecord( 2.100, 0.8410, 0.0680, 0.0680 );
		aMeasurements_102->addRecord( 2.150, 0.8390, 0.0690, 0.0690 );
		aMeasurements_102->addRecord( 2.200, 0.8300, 0.0700, 0.0700 );
		aMeasurements_102->addRecord( 2.250, 0.8300, 0.0700, 0.0700 );
		aMeasurements_102->addRecord( 2.300, 0.8320, 0.0690, 0.0690 );
		aMeasurements_102->addRecord( 2.350, 0.8320, 0.0690, 0.0700 );
		aMeasurements_102->addRecord( 2.400, 0.8320, 0.0700, 0.0700 );
		aMeasurements_102->addRecord( 2.450, 0.8260, 0.0690, 0.0690 );
		aMeasurements_102->addRecord( 2.500, 0.8220, 0.0680, 0.0680 );

		return aMeasurements_102;

	}

protected:
	virtual void SetUp() {

		std::shared_ptr< CSpectralSampleData > aMeasurements_102 = loadSampleData_NFRC_102();

		// Create samples from measurements and solar radiation
		std::shared_ptr< CSpectralSample > aSample_102 = std::make_shared< CSpectralSample >( aMeasurements_102 );

		// Create material from samples
		double thickness = 3.048e-3; // [m]
		std::shared_ptr< CMaterial > aMaterial_102 = std::make_shared< CMaterialSample >( aSample_102,
		                                                                        thickness, MaterialType::Monolithic, WavelengthRange::Solar );

		std::shared_ptr< CScatteringLayer > Layer_102 = std::make_shared< CScatteringLayer >( aMaterial_102 );

		// Equivalent BSDF layer
		m_Layer = std::make_shared< CMultiLayerScattered >( Layer_102 );

		std::shared_ptr< CSeries > aSolarRadiation = loadSolarRadiationFile();
		m_Layer->setSourceData( aSolarRadiation );

	}

public:
	std::shared_ptr< CMultiLayerScattered > getLayer() {
		return m_Layer;
	};

};

TEST_F( MultiPaneScattered_102_NonStandardSolar, TestSpecular1 ) {
	SCOPED_TRACE( "Begin Test: Specular layer - Scattering model front side (normal incidence)." );

	CMultiLayerScattered& aLayer = *getLayer();

	Side aSide = Side::Front;
	double theta = 0;
	double phi = 0;

	double T_dir_dir = aLayer.getPropertySimple( PropertySimple::T, aSide, Scattering::DirectDirect, theta, phi );
	EXPECT_NEAR( 0.836918, T_dir_dir, 1e-6 );

	double T_dir_dif = aLayer.getPropertySimple( PropertySimple::T, aSide, Scattering::DirectDiffuse, theta, phi );
	EXPECT_NEAR( 0, T_dir_dif, 1e-6 );

	double T_dif_dif = aLayer.getPropertySimple( PropertySimple::T, aSide, Scattering::DiffuseDiffuse, theta, phi );
	EXPECT_NEAR( 0.756157, T_dif_dif, 1e-6 );

	double R_dir_dir = aLayer.getPropertySimple( PropertySimple::R, aSide, Scattering::DirectDirect, theta, phi );
	EXPECT_NEAR( 0.075619, R_dir_dir, 1e-6 );

	double R_dir_dif = aLayer.getPropertySimple( PropertySimple::R, aSide, Scattering::DirectDiffuse, theta, phi );
	EXPECT_NEAR( 0, R_dir_dif, 1e-6 );

	double R_dif_dif = aLayer.getPropertySimple( PropertySimple::R, aSide, Scattering::DiffuseDiffuse, theta, phi );
	EXPECT_NEAR( 0.147130, R_dif_dif, 1e-6 );

	double A_dir1 = aLayer.getAbsorptanceLayer( 1, aSide, ScatteringSimple::Direct, theta, phi );
	EXPECT_NEAR( 0.087463, A_dir1, 1e-6 );

	double A_dif1 = aLayer.getAbsorptanceLayer( 1, aSide, ScatteringSimple::Diffuse, theta, phi );
	EXPECT_NEAR( 0.096713, A_dif1, 1e-6 );

}

TEST_F( MultiPaneScattered_102_NonStandardSolar, TestSpecular2 ) {
	SCOPED_TRACE( "Begin Test: Specular layer - Scattering model back side (normal incidence)." );

	CMultiLayerScattered& aLayer = *getLayer();

	Side aSide = Side::Back;
	double theta = 0;
	double phi = 0;

	double T_dir_dir = aLayer.getPropertySimple( PropertySimple::T, aSide, Scattering::DirectDirect, theta, phi );
	EXPECT_NEAR( 0.836918, T_dir_dir, 1e-6 );

	double T_dir_dif = aLayer.getPropertySimple( PropertySimple::T, aSide, Scattering::DirectDiffuse, theta, phi );
	EXPECT_NEAR( 0, T_dir_dif, 1e-6 );

	double T_dif_dif = aLayer.getPropertySimple( PropertySimple::T, aSide, Scattering::DiffuseDiffuse, theta, phi );
	EXPECT_NEAR( 0.756157, T_dif_dif, 1e-6 );

	double R_dir_dir = aLayer.getPropertySimple( PropertySimple::R, aSide, Scattering::DirectDirect, theta, phi );
	EXPECT_NEAR( 0.075735, R_dir_dir, 1e-6 );

	double R_dir_dif = aLayer.getPropertySimple( PropertySimple::R, aSide, Scattering::DirectDiffuse, theta, phi );
	EXPECT_NEAR( 0, R_dir_dif, 1e-6 );

	double R_dif_dif = aLayer.getPropertySimple( PropertySimple::R, aSide, Scattering::DiffuseDiffuse, theta, phi );
	EXPECT_NEAR( 0.147249, R_dif_dif, 1e-6 );

	double A_dir1 = aLayer.getAbsorptanceLayer( 1, aSide, ScatteringSimple::Direct, theta, phi );
	EXPECT_NEAR( 0.087347, A_dir1, 1e-6 );

	double A_dif1 = aLayer.getAbsorptanceLayer( 1, aSide, ScatteringSimple::Diffuse, theta, phi );
	EXPECT_NEAR( 0.096594, A_dif1, 1e-6 );

}

TEST_F( MultiPaneScattered_102_NonStandardSolar, TestSpecular3 ) {
	SCOPED_TRACE( "Begin Test: Specular layer - Scattering model front side (Theta = 40 deg)." );

	CMultiLayerScattered& aLayer = *getLayer();

	Side aSide = Side::Front;
	double theta = 40;
	double phi = 0;

	double T_dir_dir = aLayer.getPropertySimple( PropertySimple::T, aSide, Scattering::DirectDirect, theta, phi );
	EXPECT_NEAR( 0.821105, T_dir_dir, 1e-6 );

	double T_dir_dif = aLayer.getPropertySimple( PropertySimple::T, aSide, Scattering::DirectDiffuse, theta, phi );
	EXPECT_NEAR( 0, T_dir_dif, 1e-6 );

	double T_dif_dif = aLayer.getPropertySimple( PropertySimple::T, aSide, Scattering::DiffuseDiffuse, theta, phi );
	EXPECT_NEAR( 0.756157, T_dif_dif, 1e-6 );

	double R_dir_dir = aLayer.getPropertySimple( PropertySimple::R, aSide, Scattering::DirectDirect, theta, phi );
	EXPECT_NEAR( 0.083379, R_dir_dir, 1e-6 );

	double R_dir_dif = aLayer.getPropertySimple( PropertySimple::R, aSide, Scattering::DirectDiffuse, theta, phi );
	EXPECT_NEAR( 0, R_dir_dif, 1e-6 );

	double R_dif_dif = aLayer.getPropertySimple( PropertySimple::R, aSide, Scattering::DiffuseDiffuse, theta, phi );
	EXPECT_NEAR( 0.147130, R_dif_dif, 1e-6 );

	double A_dir1 = aLayer.getAbsorptanceLayer( 1, aSide, ScatteringSimple::Direct, theta, phi );
	EXPECT_NEAR( 0.095516, A_dir1, 1e-6 );

	double A_dif1 = aLayer.getAbsorptanceLayer( 1, aSide, ScatteringSimple::Diffuse, theta, phi );
	EXPECT_NEAR( 0.096713, A_dif1, 1e-6 );

}
