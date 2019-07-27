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

// Example/test case on multlayer specular
// Difference from BSDF layer is that properties can be calculated at any custom angle

class EquivalentSpecularAngularLayer_102_103 : public testing::Test {

private:
	//	std::shared_ptr< CAngularMeasurements > m_Measurements;
	std::shared_ptr< CMultiPaneSpecular > m_Layer;
	//	std::shared_ptr< CMaterial > m_Material;

	std::shared_ptr< CSeries > loadSolarRadiationFile() {

		std::shared_ptr< CSeries > aSolarRadiation = std::make_shared< CSeries >();

		// Full ASTM E891-87 Table 1 (Solar radiation)
		aSolarRadiation->addProperty( 0.3000, 0.0 );
		aSolarRadiation->addProperty( 0.3050, 3.4 );
		aSolarRadiation->addProperty( 0.3100, 15.6 );
		aSolarRadiation->addProperty( 0.3150, 41.1 );
		aSolarRadiation->addProperty( 0.3200, 71.2 );
		aSolarRadiation->addProperty( 0.3250, 100.2 );
		aSolarRadiation->addProperty( 0.3300, 152.4 );
		aSolarRadiation->addProperty( 0.3350, 155.6 );
		aSolarRadiation->addProperty( 0.3400, 179.4 );
		aSolarRadiation->addProperty( 0.3450, 186.7 );
		aSolarRadiation->addProperty( 0.3500, 212.0 );
		aSolarRadiation->addProperty( 0.3600, 240.5 );
		aSolarRadiation->addProperty( 0.3700, 324.0 );
		aSolarRadiation->addProperty( 0.3800, 362.4 );
		aSolarRadiation->addProperty( 0.3900, 381.7 );
		aSolarRadiation->addProperty( 0.4000, 556.0 );
		aSolarRadiation->addProperty( 0.4100, 656.3 );
		aSolarRadiation->addProperty( 0.4200, 690.8 );
		aSolarRadiation->addProperty( 0.4300, 641.9 );
		aSolarRadiation->addProperty( 0.4400, 798.5 );
		aSolarRadiation->addProperty( 0.4500, 956.6 );
		aSolarRadiation->addProperty( 0.4600, 990.0 );
		aSolarRadiation->addProperty( 0.4700, 998.0 );
		aSolarRadiation->addProperty( 0.4800, 1046.1 );
		aSolarRadiation->addProperty( 0.4900, 1005.1 );
		aSolarRadiation->addProperty( 0.5000, 1026.7 );
		aSolarRadiation->addProperty( 0.5100, 1066.7 );
		aSolarRadiation->addProperty( 0.5200, 1011.5 );
		aSolarRadiation->addProperty( 0.5300, 1084.9 );
		aSolarRadiation->addProperty( 0.5400, 1082.4 );
		aSolarRadiation->addProperty( 0.5500, 1102.2 );
		aSolarRadiation->addProperty( 0.5700, 1087.4 );
		aSolarRadiation->addProperty( 0.5900, 1024.3 );
		aSolarRadiation->addProperty( 0.6100, 1088.8 );
		aSolarRadiation->addProperty( 0.6300, 1062.1 );
		aSolarRadiation->addProperty( 0.6500, 1061.7 );
		aSolarRadiation->addProperty( 0.6700, 1046.2 );
		aSolarRadiation->addProperty( 0.6900, 859.2 );
		aSolarRadiation->addProperty( 0.7100, 1002.4 );
		aSolarRadiation->addProperty( 0.7180, 816.9 );
		aSolarRadiation->addProperty( 0.7244, 842.8 );
		aSolarRadiation->addProperty( 0.7400, 971.0 );
		aSolarRadiation->addProperty( 0.7525, 956.3 );
		aSolarRadiation->addProperty( 0.7575, 942.2 );
		aSolarRadiation->addProperty( 0.7625, 524.8 );
		aSolarRadiation->addProperty( 0.7675, 830.7 );
		aSolarRadiation->addProperty( 0.7800, 908.9 );
		aSolarRadiation->addProperty( 0.8000, 873.4 );
		aSolarRadiation->addProperty( 0.8160, 712.0 );
		aSolarRadiation->addProperty( 0.8237, 660.2 );
		aSolarRadiation->addProperty( 0.8315, 765.5 );
		aSolarRadiation->addProperty( 0.8400, 799.8 );
		aSolarRadiation->addProperty( 0.8600, 815.2 );
		aSolarRadiation->addProperty( 0.8800, 778.3 );
		aSolarRadiation->addProperty( 0.9050, 630.4 );
		aSolarRadiation->addProperty( 0.9150, 565.2 );
		aSolarRadiation->addProperty( 0.9250, 586.4 );
		aSolarRadiation->addProperty( 0.9300, 348.1 );
		aSolarRadiation->addProperty( 0.9370, 224.2 );
		aSolarRadiation->addProperty( 0.9480, 271.4 );
		aSolarRadiation->addProperty( 0.9650, 451.2 );
		aSolarRadiation->addProperty( 0.9800, 549.7 );
		aSolarRadiation->addProperty( 0.9935, 630.1 );
		aSolarRadiation->addProperty( 1.0400, 582.9 );
		aSolarRadiation->addProperty( 1.0700, 539.7 );
		aSolarRadiation->addProperty( 1.1000, 366.2 );
		aSolarRadiation->addProperty( 1.1200, 98.1 );
		aSolarRadiation->addProperty( 1.1300, 169.5 );
		aSolarRadiation->addProperty( 1.1370, 118.7 );
		aSolarRadiation->addProperty( 1.1610, 301.9 );
		aSolarRadiation->addProperty( 1.1800, 406.8 );
		aSolarRadiation->addProperty( 1.2000, 375.2 );
		aSolarRadiation->addProperty( 1.2350, 423.6 );
		aSolarRadiation->addProperty( 1.2900, 365.7 );
		aSolarRadiation->addProperty( 1.3200, 223.4 );
		aSolarRadiation->addProperty( 1.3500, 30.1 );
		aSolarRadiation->addProperty( 1.3950, 1.4 );
		aSolarRadiation->addProperty( 1.4425, 51.6 );
		aSolarRadiation->addProperty( 1.4625, 97.0 );
		aSolarRadiation->addProperty( 1.4770, 97.3 );
		aSolarRadiation->addProperty( 1.4970, 167.1 );
		aSolarRadiation->addProperty( 1.5200, 239.3 );
		aSolarRadiation->addProperty( 1.5390, 248.8 );
		aSolarRadiation->addProperty( 1.5580, 249.3 );
		aSolarRadiation->addProperty( 1.5780, 222.3 );
		aSolarRadiation->addProperty( 1.5920, 227.3 );
		aSolarRadiation->addProperty( 1.6100, 210.5 );
		aSolarRadiation->addProperty( 1.6300, 224.7 );
		aSolarRadiation->addProperty( 1.6460, 215.9 );
		aSolarRadiation->addProperty( 1.6780, 202.8 );
		aSolarRadiation->addProperty( 1.7400, 158.2 );
		aSolarRadiation->addProperty( 1.8000, 28.6 );
		aSolarRadiation->addProperty( 1.8600, 1.8 );
		aSolarRadiation->addProperty( 1.9200, 1.1 );
		aSolarRadiation->addProperty( 1.9600, 19.7 );
		aSolarRadiation->addProperty( 1.9850, 84.9 );
		aSolarRadiation->addProperty( 2.0050, 25.0 );
		aSolarRadiation->addProperty( 2.0350, 92.5 );
		aSolarRadiation->addProperty( 2.0650, 56.3 );
		aSolarRadiation->addProperty( 2.1000, 82.7 );
		aSolarRadiation->addProperty( 2.1480, 76.2 );
		aSolarRadiation->addProperty( 2.1980, 66.4 );
		aSolarRadiation->addProperty( 2.2700, 65.0 );
		aSolarRadiation->addProperty( 2.3600, 57.6 );
		aSolarRadiation->addProperty( 2.4500, 19.8 );
		aSolarRadiation->addProperty( 2.4940, 17.0 );
		aSolarRadiation->addProperty( 2.5370, 3.0 );
		aSolarRadiation->addProperty( 2.9410, 4.0 );
		aSolarRadiation->addProperty( 2.9730, 7.0 );
		aSolarRadiation->addProperty( 3.0050, 6.0 );
		aSolarRadiation->addProperty( 3.0560, 3.0 );
		aSolarRadiation->addProperty( 3.1320, 5.0 );
		aSolarRadiation->addProperty( 3.1560, 18.0 );
		aSolarRadiation->addProperty( 3.2040, 1.2 );
		aSolarRadiation->addProperty( 3.2450, 3.0 );
		aSolarRadiation->addProperty( 3.3170, 12.0 );
		aSolarRadiation->addProperty( 3.3440, 3.0 );
		aSolarRadiation->addProperty( 3.4500, 12.2 );
		aSolarRadiation->addProperty( 3.5730, 11.0 );
		aSolarRadiation->addProperty( 3.7650, 9.0 );
		aSolarRadiation->addProperty( 4.0450, 6.9 );

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

	std::shared_ptr< CSingleAngularMeasurement > getSample1() {
		std::shared_ptr< CSeries > aSolarRadiation = loadSolarRadiationFile();

		std::shared_ptr< CSpectralSampleData > aMeasurements0 = std::make_shared< CSpectralSampleData >();
		// incident angle = 0

		aMeasurements0->addRecord( 0.290, 0, 0.0481, 0.0481 );
		aMeasurements0->addRecord( 0.295, 0, 0.0479, 0.0479 );
		aMeasurements0->addRecord( 0.300, 0, 0.0476, 0.0476 );
		aMeasurements0->addRecord( 0.305, 0, 0.0474, 0.0474 );
		aMeasurements0->addRecord( 0.310, 0.007, 0.0471, 0.0471 );
		aMeasurements0->addRecord( 0.315, 0.035, 0.047, 0.047 );
		aMeasurements0->addRecord( 0.320, 0.0975, 0.0472, 0.0472 );
		aMeasurements0->addRecord( 0.325, 0.2099, 0.0487, 0.0487 );
		aMeasurements0->addRecord( 0.330, 0.3497, 0.0525, 0.0525 );
		aMeasurements0->addRecord( 0.335, 0.4865, 0.0581, 0.0581 );
		aMeasurements0->addRecord( 0.340, 0.6057, 0.0644, 0.0644 );
		aMeasurements0->addRecord( 0.345, 0.7035, 0.0706, 0.0706 );
		aMeasurements0->addRecord( 0.350, 0.7687, 0.0751, 0.0751 );
		aMeasurements0->addRecord( 0.355, 0.812, 0.0783, 0.0783 );
		aMeasurements0->addRecord( 0.360, 0.8429, 0.0806, 0.0806 );
		aMeasurements0->addRecord( 0.365, 0.862, 0.0819, 0.0819 );
		aMeasurements0->addRecord( 0.370, 0.8678, 0.0821, 0.0821 );
		aMeasurements0->addRecord( 0.375, 0.8672, 0.0819, 0.0819 );
		aMeasurements0->addRecord( 0.380, 0.8554, 0.0806, 0.0806 );
		aMeasurements0->addRecord( 0.385, 0.8696, 0.0816, 0.0816 );
		aMeasurements0->addRecord( 0.390, 0.8804, 0.0823, 0.0823 );
		aMeasurements0->addRecord( 0.395, 0.8884, 0.0828, 0.0828 );
		aMeasurements0->addRecord( 0.400, 0.8924, 0.083, 0.083 );
		aMeasurements0->addRecord( 0.410, 0.8944, 0.0828, 0.0828 );
		aMeasurements0->addRecord( 0.420, 0.8921, 0.0823, 0.0823 );
		aMeasurements0->addRecord( 0.430, 0.8914, 0.082, 0.082 );
		aMeasurements0->addRecord( 0.440, 0.8896, 0.0815, 0.0815 );
		aMeasurements0->addRecord( 0.450, 0.8933, 0.0816, 0.0816 );
		aMeasurements0->addRecord( 0.460, 0.8972, 0.0817, 0.0817 );
		aMeasurements0->addRecord( 0.470, 0.8991, 0.0816, 0.0816 );
		aMeasurements0->addRecord( 0.480, 0.9013, 0.0816, 0.0816 );
		aMeasurements0->addRecord( 0.490, 0.9026, 0.0815, 0.0815 );
		aMeasurements0->addRecord( 0.500, 0.9026, 0.0813, 0.0813 );
		aMeasurements0->addRecord( 0.510, 0.9031, 0.0811, 0.0811 );
		aMeasurements0->addRecord( 0.520, 0.9025, 0.0809, 0.0809 );
		aMeasurements0->addRecord( 0.530, 0.9028, 0.0808, 0.0808 );
		aMeasurements0->addRecord( 0.540, 0.9033, 0.0807, 0.0807 );
		aMeasurements0->addRecord( 0.550, 0.9013, 0.0804, 0.0804 );
		aMeasurements0->addRecord( 0.560, 0.8999, 0.0802, 0.0802 );
		aMeasurements0->addRecord( 0.570, 0.8986, 0.0799, 0.0799 );
		aMeasurements0->addRecord( 0.580, 0.8975, 0.0797, 0.0797 );
		aMeasurements0->addRecord( 0.590, 0.8939, 0.0793, 0.0793 );
		aMeasurements0->addRecord( 0.600, 0.8919, 0.079, 0.079 );
		aMeasurements0->addRecord( 0.610, 0.891, 0.0789, 0.0789 );
		aMeasurements0->addRecord( 0.620, 0.8853, 0.0783, 0.0783 );
		aMeasurements0->addRecord( 0.630, 0.8838, 0.0781, 0.0781 );
		aMeasurements0->addRecord( 0.640, 0.8806, 0.0777, 0.0777 );
		aMeasurements0->addRecord( 0.650, 0.8769, 0.0773, 0.0773 );
		aMeasurements0->addRecord( 0.660, 0.8735, 0.077, 0.077 );
		aMeasurements0->addRecord( 0.670, 0.8731, 0.0769, 0.0769 );
		aMeasurements0->addRecord( 0.680, 0.8665, 0.0763, 0.0763 );
		aMeasurements0->addRecord( 0.690, 0.8637, 0.076, 0.076 );
		aMeasurements0->addRecord( 0.700, 0.8607, 0.0757, 0.0757 );
		aMeasurements0->addRecord( 0.710, 0.8557, 0.0753, 0.0753 );
		aMeasurements0->addRecord( 0.720, 0.8531, 0.075, 0.075 );
		aMeasurements0->addRecord( 0.730, 0.8487, 0.0746, 0.0746 );
		aMeasurements0->addRecord( 0.740, 0.8418, 0.074, 0.074 );
		aMeasurements0->addRecord( 0.750, 0.8406, 0.0738, 0.0738 );
		aMeasurements0->addRecord( 0.760, 0.8358, 0.0734, 0.0734 );
		aMeasurements0->addRecord( 0.770, 0.8341, 0.0732, 0.0732 );
		aMeasurements0->addRecord( 0.780, 0.8324, 0.073, 0.073 );
		aMeasurements0->addRecord( 0.790, 0.8232, 0.0723, 0.0723 );
		aMeasurements0->addRecord( 0.800, 0.8246, 0.0723, 0.0723 );
		aMeasurements0->addRecord( 0.850, 0.8076, 0.0708, 0.0708 );
		aMeasurements0->addRecord( 0.900, 0.8002, 0.07, 0.07 );
		aMeasurements0->addRecord( 0.950, 0.7907, 0.0692, 0.0692 );
		aMeasurements0->addRecord( 1.000, 0.7862, 0.0687, 0.0687 );
		aMeasurements0->addRecord( 1.050, 0.7849, 0.0685, 0.0685 );
		aMeasurements0->addRecord( 1.100, 0.7848, 0.0683, 0.0683 );
		aMeasurements0->addRecord( 1.150, 0.7864, 0.0683, 0.0683 );
		aMeasurements0->addRecord( 1.200, 0.7894, 0.0685, 0.0685 );
		aMeasurements0->addRecord( 1.250, 0.7944, 0.0687, 0.0687 );
		aMeasurements0->addRecord( 1.300, 0.8014, 0.0691, 0.0691 );
		aMeasurements0->addRecord( 1.350, 0.8088, 0.0695, 0.0695 );
		aMeasurements0->addRecord( 1.400, 0.8168, 0.07, 0.07 );
		aMeasurements0->addRecord( 1.450, 0.8261, 0.0705, 0.0705 );
		aMeasurements0->addRecord( 1.500, 0.8366, 0.0712, 0.0712 );
		aMeasurements0->addRecord( 1.550, 0.8444, 0.0716, 0.0716 );
		aMeasurements0->addRecord( 1.600, 0.8506, 0.0719, 0.0719 );
		aMeasurements0->addRecord( 1.650, 0.853, 0.072, 0.072 );
		aMeasurements0->addRecord( 1.700, 0.8527, 0.0719, 0.0719 );
		aMeasurements0->addRecord( 1.750, 0.8532, 0.0718, 0.0718 );
		aMeasurements0->addRecord( 1.800, 0.8504, 0.0714, 0.0714 );
		aMeasurements0->addRecord( 1.850, 0.8488, 0.0712, 0.0712 );
		aMeasurements0->addRecord( 1.900, 0.8523, 0.0713, 0.0713 );
		aMeasurements0->addRecord( 1.950, 0.8497, 0.071, 0.071 );
		aMeasurements0->addRecord( 2.000, 0.848, 0.0708, 0.0708 );
		aMeasurements0->addRecord( 2.050, 0.8488, 0.0707, 0.0707 );
		aMeasurements0->addRecord( 2.100, 0.8505, 0.0707, 0.0707 );
		aMeasurements0->addRecord( 2.150, 0.8408, 0.0699, 0.0699 );
		aMeasurements0->addRecord( 2.200, 0.8304, 0.0691, 0.0691 );
		aMeasurements0->addRecord( 2.250, 0.8277, 0.0688, 0.0688 );
		aMeasurements0->addRecord( 2.300, 0.8303, 0.0688, 0.0688 );
		aMeasurements0->addRecord( 2.350, 0.834, 0.069, 0.069 );
		aMeasurements0->addRecord( 2.400, 0.8236, 0.0681, 0.0681 );
		aMeasurements0->addRecord( 2.450, 0.8225, 0.0679, 0.0679 );
		aMeasurements0->addRecord( 2.500, 0.8184, 0.0675, 0.0675 );

		std::shared_ptr< CSpectralSample > aSample0 = std::make_shared< CSpectralSample >( aMeasurements0, aSolarRadiation );
		std::shared_ptr< CSingleAngularMeasurement > aAngular0 = std::make_shared< CSingleAngularMeasurement >( aSample0, 0.0 );

		return aAngular0;
	}

	std::shared_ptr< CSingleAngularMeasurement > getSample2() {
		std::shared_ptr< CSeries > aSolarRadiation = loadSolarRadiationFile();

		std::shared_ptr< CSpectralSampleData > aMeasurements1 = std::make_shared< CSpectralSampleData >();
		// incident angle = 10

		aMeasurements1->addRecord( 0.290, 0, 0.0481, 0.0481 );
		aMeasurements1->addRecord( 0.295, 0, 0.0479, 0.0479 );
		aMeasurements1->addRecord( 0.300, 0, 0.0476, 0.0476 );
		aMeasurements1->addRecord( 0.305, 0, 0.0474, 0.0474 );
		aMeasurements1->addRecord( 0.310, 0.0068, 0.0471, 0.0471 );
		aMeasurements1->addRecord( 0.315, 0.0343, 0.047, 0.047 );
		aMeasurements1->addRecord( 0.320, 0.0961, 0.0472, 0.0472 );
		aMeasurements1->addRecord( 0.325, 0.208, 0.0487, 0.0487 );
		aMeasurements1->addRecord( 0.330, 0.3476, 0.0524, 0.0524 );
		aMeasurements1->addRecord( 0.335, 0.4845, 0.058, 0.058 );
		aMeasurements1->addRecord( 0.340, 0.6041, 0.0643, 0.0643 );
		aMeasurements1->addRecord( 0.345, 0.7023, 0.0705, 0.0705 );
		aMeasurements1->addRecord( 0.350, 0.7678, 0.0751, 0.0751 );
		aMeasurements1->addRecord( 0.355, 0.8114, 0.0783, 0.0783 );
		aMeasurements1->addRecord( 0.360, 0.8424, 0.0805, 0.0805 );
		aMeasurements1->addRecord( 0.365, 0.8617, 0.0819, 0.0819 );
		aMeasurements1->addRecord( 0.370, 0.8675, 0.0821, 0.0821 );
		aMeasurements1->addRecord( 0.375, 0.8669, 0.0819, 0.0819 );
		aMeasurements1->addRecord( 0.380, 0.8551, 0.0806, 0.0806 );
		aMeasurements1->addRecord( 0.385, 0.8693, 0.0816, 0.0816 );
		aMeasurements1->addRecord( 0.390, 0.8802, 0.0823, 0.0823 );
		aMeasurements1->addRecord( 0.395, 0.8882, 0.0829, 0.0829 );
		aMeasurements1->addRecord( 0.400, 0.8922, 0.083, 0.083 );
		aMeasurements1->addRecord( 0.410, 0.8943, 0.0829, 0.0829 );
		aMeasurements1->addRecord( 0.420, 0.8919, 0.0823, 0.0823 );
		aMeasurements1->addRecord( 0.430, 0.8912, 0.082, 0.082 );
		aMeasurements1->addRecord( 0.440, 0.8894, 0.0815, 0.0815 );
		aMeasurements1->addRecord( 0.450, 0.8931, 0.0816, 0.0816 );
		aMeasurements1->addRecord( 0.460, 0.8971, 0.0817, 0.0817 );
		aMeasurements1->addRecord( 0.470, 0.8989, 0.0816, 0.0816 );
		aMeasurements1->addRecord( 0.480, 0.9011, 0.0816, 0.0816 );
		aMeasurements1->addRecord( 0.490, 0.9025, 0.0815, 0.0815 );
		aMeasurements1->addRecord( 0.500, 0.9025, 0.0813, 0.0813 );
		aMeasurements1->addRecord( 0.510, 0.903, 0.0812, 0.0812 );
		aMeasurements1->addRecord( 0.520, 0.9024, 0.0809, 0.0809 );
		aMeasurements1->addRecord( 0.530, 0.9027, 0.0808, 0.0808 );
		aMeasurements1->addRecord( 0.540, 0.9032, 0.0807, 0.0807 );
		aMeasurements1->addRecord( 0.550, 0.9011, 0.0804, 0.0804 );
		aMeasurements1->addRecord( 0.560, 0.8998, 0.0802, 0.0802 );
		aMeasurements1->addRecord( 0.570, 0.8985, 0.0799, 0.0799 );
		aMeasurements1->addRecord( 0.580, 0.8974, 0.0797, 0.0797 );
		aMeasurements1->addRecord( 0.590, 0.8937, 0.0793, 0.0793 );
		aMeasurements1->addRecord( 0.600, 0.8917, 0.079, 0.079 );
		aMeasurements1->addRecord( 0.610, 0.8908, 0.0789, 0.0789 );
		aMeasurements1->addRecord( 0.620, 0.885, 0.0783, 0.0783 );
		aMeasurements1->addRecord( 0.630, 0.8835, 0.0781, 0.0781 );
		aMeasurements1->addRecord( 0.640, 0.8803, 0.0777, 0.0777 );
		aMeasurements1->addRecord( 0.650, 0.8766, 0.0774, 0.0774 );
		aMeasurements1->addRecord( 0.660, 0.8732, 0.077, 0.077 );
		aMeasurements1->addRecord( 0.670, 0.8728, 0.0769, 0.0769 );
		aMeasurements1->addRecord( 0.680, 0.8662, 0.0763, 0.0763 );
		aMeasurements1->addRecord( 0.690, 0.8633, 0.076, 0.076 );
		aMeasurements1->addRecord( 0.700, 0.8603, 0.0757, 0.0757 );
		aMeasurements1->addRecord( 0.710, 0.8552, 0.0753, 0.0753 );
		aMeasurements1->addRecord( 0.720, 0.8527, 0.075, 0.075 );
		aMeasurements1->addRecord( 0.730, 0.8482, 0.0746, 0.0746 );
		aMeasurements1->addRecord( 0.740, 0.8413, 0.074, 0.074 );
		aMeasurements1->addRecord( 0.750, 0.8401, 0.0738, 0.0738 );
		aMeasurements1->addRecord( 0.760, 0.8352, 0.0734, 0.0734 );
		aMeasurements1->addRecord( 0.770, 0.8335, 0.0732, 0.0732 );
		aMeasurements1->addRecord( 0.780, 0.8318, 0.073, 0.073 );
		aMeasurements1->addRecord( 0.790, 0.8225, 0.0722, 0.0722 );
		aMeasurements1->addRecord( 0.800, 0.824, 0.0723, 0.0723 );
		aMeasurements1->addRecord( 0.850, 0.8069, 0.0708, 0.0708 );
		aMeasurements1->addRecord( 0.900, 0.7994, 0.07, 0.07 );
		aMeasurements1->addRecord( 0.950, 0.7899, 0.0691, 0.0691 );
		aMeasurements1->addRecord( 1.000, 0.7854, 0.0686, 0.0686 );
		aMeasurements1->addRecord( 1.050, 0.7841, 0.0684, 0.0684 );
		aMeasurements1->addRecord( 1.100, 0.7839, 0.0683, 0.0683 );
		aMeasurements1->addRecord( 1.150, 0.7856, 0.0683, 0.0683 );
		aMeasurements1->addRecord( 1.200, 0.7886, 0.0684, 0.0684 );
		aMeasurements1->addRecord( 1.250, 0.7936, 0.0687, 0.0687 );
		aMeasurements1->addRecord( 1.300, 0.8006, 0.069, 0.069 );
		aMeasurements1->addRecord( 1.350, 0.8081, 0.0695, 0.0695 );
		aMeasurements1->addRecord( 1.400, 0.8162, 0.0699, 0.0699 );
		aMeasurements1->addRecord( 1.450, 0.8255, 0.0705, 0.0705 );
		aMeasurements1->addRecord( 1.500, 0.836, 0.0711, 0.0711 );
		aMeasurements1->addRecord( 1.550, 0.8439, 0.0716, 0.0716 );
		aMeasurements1->addRecord( 1.600, 0.8501, 0.0719, 0.0719 );
		aMeasurements1->addRecord( 1.650, 0.8525, 0.072, 0.072 );
		aMeasurements1->addRecord( 1.700, 0.8522, 0.0719, 0.0719 );
		aMeasurements1->addRecord( 1.750, 0.8527, 0.0718, 0.0718 );
		aMeasurements1->addRecord( 1.800, 0.8499, 0.0714, 0.0714 );
		aMeasurements1->addRecord( 1.850, 0.8483, 0.0712, 0.0712 );
		aMeasurements1->addRecord( 1.900, 0.8518, 0.0713, 0.0713 );
		aMeasurements1->addRecord( 1.950, 0.8492, 0.071, 0.071 );
		aMeasurements1->addRecord( 2.000, 0.8475, 0.0708, 0.0708 );
		aMeasurements1->addRecord( 2.050, 0.8483, 0.0707, 0.0707 );
		aMeasurements1->addRecord( 2.100, 0.85, 0.0707, 0.0707 );
		aMeasurements1->addRecord( 2.150, 0.8402, 0.0699, 0.0699 );
		aMeasurements1->addRecord( 2.200, 0.8298, 0.069, 0.069 );
		aMeasurements1->addRecord( 2.250, 0.8271, 0.0687, 0.0687 );
		aMeasurements1->addRecord( 2.300, 0.8297, 0.0688, 0.0688 );
		aMeasurements1->addRecord( 2.350, 0.8334, 0.069, 0.069 );
		aMeasurements1->addRecord( 2.400, 0.823, 0.0681, 0.0681 );
		aMeasurements1->addRecord( 2.450, 0.8218, 0.0679, 0.0679 );
		aMeasurements1->addRecord( 2.500, 0.8176, 0.0675, 0.0675 );

		std::shared_ptr< CSpectralSample > aSample1 = std::make_shared< CSpectralSample >( aMeasurements1, aSolarRadiation );
		std::shared_ptr< CSingleAngularMeasurement > aAngular1 = std::make_shared< CSingleAngularMeasurement >( aSample1, 10.0 );

		return aAngular1;

	}

	std::shared_ptr< CSingleAngularMeasurement > getSample3() {
		std::shared_ptr< CSeries > aSolarRadiation = loadSolarRadiationFile();

		std::shared_ptr< CSpectralSampleData > aMeasurements4 = std::make_shared< CSpectralSampleData >();
		// incident angle = 40

		aMeasurements4->addRecord( 0.290, 0, 0.0993, 0.0993 );
		aMeasurements4->addRecord( 0.295, 0, 0.099, 0.099 );
		aMeasurements4->addRecord( 0.300, 0, 0.0986, 0.0986 );
		aMeasurements4->addRecord( 0.305, 0, 0.0984, 0.0984 );
		aMeasurements4->addRecord( 0.310, 0.0023, 0.098, 0.098 );
		aMeasurements4->addRecord( 0.315, 0.0161, 0.0978, 0.0978 );
		aMeasurements4->addRecord( 0.320, 0.0553, 0.0979, 0.0979 );
		aMeasurements4->addRecord( 0.325, 0.1392, 0.0996, 0.0996 );
		aMeasurements4->addRecord( 0.330, 0.2575, 0.1049, 0.1049 );
		aMeasurements4->addRecord( 0.335, 0.3836, 0.1143, 0.1143 );
		aMeasurements4->addRecord( 0.340, 0.5002, 0.1261, 0.1261 );
		aMeasurements4->addRecord( 0.345, 0.5999, 0.1387, 0.1387 );
		aMeasurements4->addRecord( 0.350, 0.6682, 0.1485, 0.1485 );
		aMeasurements4->addRecord( 0.355, 0.7145, 0.1556, 0.1556 );
		aMeasurements4->addRecord( 0.360, 0.7478, 0.1609, 0.1609 );
		aMeasurements4->addRecord( 0.365, 0.7686, 0.1643, 0.1643 );
		aMeasurements4->addRecord( 0.370, 0.775, 0.1651, 0.1651 );
		aMeasurements4->addRecord( 0.375, 0.7743, 0.1647, 0.1647 );
		aMeasurements4->addRecord( 0.380, 0.7614, 0.1622, 0.1622 );
		aMeasurements4->addRecord( 0.385, 0.7769, 0.1646, 0.1646 );
		aMeasurements4->addRecord( 0.390, 0.7887, 0.1665, 0.1665 );
		aMeasurements4->addRecord( 0.395, 0.7975, 0.1678, 0.1678 );
		aMeasurements4->addRecord( 0.400, 0.8019, 0.1684, 0.1684 );
		aMeasurements4->addRecord( 0.410, 0.8042, 0.1684, 0.1684 );
		aMeasurements4->addRecord( 0.420, 0.8015, 0.1675, 0.1675 );
		aMeasurements4->addRecord( 0.430, 0.8008, 0.167, 0.167 );
		aMeasurements4->addRecord( 0.440, 0.7988, 0.1663, 0.1663 );
		aMeasurements4->addRecord( 0.450, 0.8028, 0.1668, 0.1668 );
		aMeasurements4->addRecord( 0.460, 0.8073, 0.1673, 0.1673 );
		aMeasurements4->addRecord( 0.470, 0.8093, 0.1674, 0.1674 );
		aMeasurements4->addRecord( 0.480, 0.8117, 0.1675, 0.1675 );
		aMeasurements4->addRecord( 0.490, 0.8132, 0.1675, 0.1675 );
		aMeasurements4->addRecord( 0.500, 0.8132, 0.1673, 0.1673 );
		aMeasurements4->addRecord( 0.510, 0.8138, 0.1672, 0.1672 );
		aMeasurements4->addRecord( 0.520, 0.8131, 0.1668, 0.1668 );
		aMeasurements4->addRecord( 0.530, 0.8135, 0.1667, 0.1667 );
		aMeasurements4->addRecord( 0.540, 0.814, 0.1666, 0.1666 );
		aMeasurements4->addRecord( 0.550, 0.8117, 0.1661, 0.1661 );
		aMeasurements4->addRecord( 0.560, 0.8102, 0.1656, 0.1656 );
		aMeasurements4->addRecord( 0.570, 0.8088, 0.1652, 0.1652 );
		aMeasurements4->addRecord( 0.580, 0.8076, 0.1648, 0.1648 );
		aMeasurements4->addRecord( 0.590, 0.8035, 0.164, 0.164 );
		aMeasurements4->addRecord( 0.600, 0.8013, 0.1634, 0.1634 );
		aMeasurements4->addRecord( 0.610, 0.8003, 0.1632, 0.1632 );
		aMeasurements4->addRecord( 0.620, 0.794, 0.1619, 0.1619 );
		aMeasurements4->addRecord( 0.630, 0.7923, 0.1615, 0.1615 );
		aMeasurements4->addRecord( 0.640, 0.7888, 0.1608, 0.1608 );
		aMeasurements4->addRecord( 0.650, 0.7848, 0.16, 0.16 );
		aMeasurements4->addRecord( 0.660, 0.781, 0.1593, 0.1593 );
		aMeasurements4->addRecord( 0.670, 0.7806, 0.1591, 0.1591 );
		aMeasurements4->addRecord( 0.680, 0.7733, 0.1578, 0.1578 );
		aMeasurements4->addRecord( 0.690, 0.7702, 0.1571, 0.1571 );
		aMeasurements4->addRecord( 0.700, 0.7669, 0.1565, 0.1565 );
		aMeasurements4->addRecord( 0.710, 0.7614, 0.1555, 0.1555 );
		aMeasurements4->addRecord( 0.720, 0.7586, 0.155, 0.155 );
		aMeasurements4->addRecord( 0.730, 0.7537, 0.1541, 0.1541 );
		aMeasurements4->addRecord( 0.740, 0.7462, 0.1528, 0.1528 );
		aMeasurements4->addRecord( 0.750, 0.7449, 0.1525, 0.1525 );
		aMeasurements4->addRecord( 0.760, 0.7396, 0.1515, 0.1515 );
		aMeasurements4->addRecord( 0.770, 0.7378, 0.1512, 0.1512 );
		aMeasurements4->addRecord( 0.780, 0.7359, 0.1508, 0.1508 );
		aMeasurements4->addRecord( 0.790, 0.7259, 0.1491, 0.1491 );
		aMeasurements4->addRecord( 0.800, 0.7275, 0.1493, 0.1493 );
		aMeasurements4->addRecord( 0.850, 0.709, 0.1461, 0.1461 );
		aMeasurements4->addRecord( 0.900, 0.7009, 0.1445, 0.1445 );
		aMeasurements4->addRecord( 0.950, 0.6907, 0.1428, 0.1428 );
		aMeasurements4->addRecord( 1.000, 0.6858, 0.1418, 0.1418 );
		aMeasurements4->addRecord( 1.050, 0.6844, 0.1415, 0.1415 );
		aMeasurements4->addRecord( 1.100, 0.6843, 0.1413, 0.1413 );
		aMeasurements4->addRecord( 1.150, 0.686, 0.1414, 0.1414 );
		aMeasurements4->addRecord( 1.200, 0.6892, 0.1418, 0.1418 );
		aMeasurements4->addRecord( 1.250, 0.6946, 0.1424, 0.1424 );
		aMeasurements4->addRecord( 1.300, 0.7021, 0.1434, 0.1434 );
		aMeasurements4->addRecord( 1.350, 0.7101, 0.1445, 0.1445 );
		aMeasurements4->addRecord( 1.400, 0.7188, 0.1457, 0.1457 );
		aMeasurements4->addRecord( 1.450, 0.729, 0.1471, 0.1471 );
		aMeasurements4->addRecord( 1.500, 0.7404, 0.1488, 0.1488 );
		aMeasurements4->addRecord( 1.550, 0.7489, 0.15, 0.15 );
		aMeasurements4->addRecord( 1.600, 0.7557, 0.1509, 0.1509 );
		aMeasurements4->addRecord( 1.650, 0.7583, 0.1512, 0.1512 );
		aMeasurements4->addRecord( 1.700, 0.758, 0.151, 0.151 );
		aMeasurements4->addRecord( 1.750, 0.7585, 0.1509, 0.1509 );
		aMeasurements4->addRecord( 1.800, 0.7554, 0.1503, 0.1503 );
		aMeasurements4->addRecord( 1.850, 0.7537, 0.1498, 0.1498 );
		aMeasurements4->addRecord( 1.900, 0.7575, 0.1503, 0.1503 );
		aMeasurements4->addRecord( 1.950, 0.7546, 0.1497, 0.1497 );
		aMeasurements4->addRecord( 2.000, 0.7528, 0.1492, 0.1492 );
		aMeasurements4->addRecord( 2.050, 0.7536, 0.1492, 0.1492 );
		aMeasurements4->addRecord( 2.100, 0.7555, 0.1494, 0.1494 );
		aMeasurements4->addRecord( 2.150, 0.7449, 0.1475, 0.1475 );
		aMeasurements4->addRecord( 2.200, 0.7335, 0.1456, 0.1456 );
		aMeasurements4->addRecord( 2.250, 0.7305, 0.145, 0.145 );
		aMeasurements4->addRecord( 2.300, 0.7334, 0.1453, 0.1453 );
		aMeasurements4->addRecord( 2.350, 0.7374, 0.1458, 0.1458 );
		aMeasurements4->addRecord( 2.400, 0.7261, 0.1439, 0.1439 );
		aMeasurements4->addRecord( 2.450, 0.7248, 0.1436, 0.1436 );
		aMeasurements4->addRecord( 2.500, 0.7203, 0.1427, 0.1427 );

		std::shared_ptr< CSpectralSample > aSample4 = std::make_shared< CSpectralSample >( aMeasurements4, aSolarRadiation );
		std::shared_ptr< CSingleAngularMeasurement > aAngular4 = std::make_shared< CSingleAngularMeasurement >( aSample4, 40.0 );

		return aAngular4;
	}

	std::shared_ptr< CSingleAngularMeasurement > getSample4() {
		std::shared_ptr< CSeries > aSolarRadiation = loadSolarRadiationFile();

		std::shared_ptr< CSpectralSampleData > aMeasurements9 = std::make_shared< CSpectralSampleData >();
		// incident angle = 90
		aMeasurements9->addRecord( 0.290, 0, 1, 1 );
		aMeasurements9->addRecord( 0.295, 0, 1, 1 );
		aMeasurements9->addRecord( 0.300, 0, 1, 1 );
		aMeasurements9->addRecord( 0.305, 0, 1, 1 );
		aMeasurements9->addRecord( 0.310, 0, 1, 1 );
		aMeasurements9->addRecord( 0.315, 0, 1, 1 );
		aMeasurements9->addRecord( 0.320, 0, 1, 1 );
		aMeasurements9->addRecord( 0.325, 0, 1, 1 );
		aMeasurements9->addRecord( 0.330, 0, 1, 1 );
		aMeasurements9->addRecord( 0.335, 0, 1, 1 );
		aMeasurements9->addRecord( 0.340, 0, 1, 1 );
		aMeasurements9->addRecord( 0.345, 0, 1, 1 );
		aMeasurements9->addRecord( 0.350, 0, 1, 1 );
		aMeasurements9->addRecord( 0.355, 0, 1, 1 );
		aMeasurements9->addRecord( 0.360, 0, 1, 1 );
		aMeasurements9->addRecord( 0.365, 0, 1, 1 );
		aMeasurements9->addRecord( 0.370, 0, 1, 1 );
		aMeasurements9->addRecord( 0.375, 0, 1, 1 );
		aMeasurements9->addRecord( 0.380, 0, 1, 1 );
		aMeasurements9->addRecord( 0.385, 0, 1, 1 );
		aMeasurements9->addRecord( 0.390, 0, 1, 1 );
		aMeasurements9->addRecord( 0.395, 0, 1, 1 );
		aMeasurements9->addRecord( 0.400, 0, 1, 1 );
		aMeasurements9->addRecord( 0.410, 0, 1, 1 );
		aMeasurements9->addRecord( 0.420, 0, 1, 1 );
		aMeasurements9->addRecord( 0.430, 0, 1, 1 );
		aMeasurements9->addRecord( 0.440, 0, 1, 1 );
		aMeasurements9->addRecord( 0.450, 0, 1, 1 );
		aMeasurements9->addRecord( 0.460, 0, 1, 1 );
		aMeasurements9->addRecord( 0.470, 0, 1, 1 );
		aMeasurements9->addRecord( 0.480, 0, 1, 1 );
		aMeasurements9->addRecord( 0.490, 0, 1, 1 );
		aMeasurements9->addRecord( 0.500, 0, 1, 1 );
		aMeasurements9->addRecord( 0.510, 0, 1, 1 );
		aMeasurements9->addRecord( 0.520, 0, 1, 1 );
		aMeasurements9->addRecord( 0.530, 0, 1, 1 );
		aMeasurements9->addRecord( 0.540, 0, 1, 1 );
		aMeasurements9->addRecord( 0.550, 0, 1, 1 );
		aMeasurements9->addRecord( 0.560, 0, 1, 1 );
		aMeasurements9->addRecord( 0.570, 0, 1, 1 );
		aMeasurements9->addRecord( 0.580, 0, 1, 1 );
		aMeasurements9->addRecord( 0.590, 0, 1, 1 );
		aMeasurements9->addRecord( 0.600, 0, 1, 1 );
		aMeasurements9->addRecord( 0.610, 0, 1, 1 );
		aMeasurements9->addRecord( 0.620, 0, 1, 1 );
		aMeasurements9->addRecord( 0.630, 0, 1, 1 );
		aMeasurements9->addRecord( 0.640, 0, 1, 1 );
		aMeasurements9->addRecord( 0.650, 0, 1, 1 );
		aMeasurements9->addRecord( 0.660, 0, 1, 1 );
		aMeasurements9->addRecord( 0.670, 0, 1, 1 );
		aMeasurements9->addRecord( 0.680, 0, 1, 1 );
		aMeasurements9->addRecord( 0.690, 0, 1, 1 );
		aMeasurements9->addRecord( 0.700, 0, 1, 1 );
		aMeasurements9->addRecord( 0.710, 0, 1, 1 );
		aMeasurements9->addRecord( 0.720, 0, 1, 1 );
		aMeasurements9->addRecord( 0.730, 0, 1, 1 );
		aMeasurements9->addRecord( 0.740, 0, 1, 1 );
		aMeasurements9->addRecord( 0.750, 0, 1, 1 );
		aMeasurements9->addRecord( 0.760, 0, 1, 1 );
		aMeasurements9->addRecord( 0.770, 0, 1, 1 );
		aMeasurements9->addRecord( 0.780, 0, 1, 1 );
		aMeasurements9->addRecord( 0.790, 0, 1, 1 );
		aMeasurements9->addRecord( 0.800, 0, 1, 1 );
		aMeasurements9->addRecord( 0.850, 0, 1, 1 );
		aMeasurements9->addRecord( 0.900, 0, 1, 1 );
		aMeasurements9->addRecord( 0.950, 0, 1, 1 );
		aMeasurements9->addRecord( 1.000, 0, 1, 1 );
		aMeasurements9->addRecord( 1.050, 0, 1, 1 );
		aMeasurements9->addRecord( 1.100, 0, 1, 1 );
		aMeasurements9->addRecord( 1.150, 0, 1, 1 );
		aMeasurements9->addRecord( 1.200, 0, 1, 1 );
		aMeasurements9->addRecord( 1.250, 0, 1, 1 );
		aMeasurements9->addRecord( 1.300, 0, 1, 1 );
		aMeasurements9->addRecord( 1.350, 0, 1, 1 );
		aMeasurements9->addRecord( 1.400, 0, 1, 1 );
		aMeasurements9->addRecord( 1.450, 0, 1, 1 );
		aMeasurements9->addRecord( 1.500, 0, 1, 1 );
		aMeasurements9->addRecord( 1.550, 0, 1, 1 );
		aMeasurements9->addRecord( 1.600, 0, 1, 1 );
		aMeasurements9->addRecord( 1.650, 0, 1, 1 );
		aMeasurements9->addRecord( 1.700, 0, 1, 1 );
		aMeasurements9->addRecord( 1.750, 0, 1, 1 );
		aMeasurements9->addRecord( 1.800, 0, 1, 1 );
		aMeasurements9->addRecord( 1.850, 0, 1, 1 );
		aMeasurements9->addRecord( 1.900, 0, 1, 1 );
		aMeasurements9->addRecord( 1.950, 0, 1, 1 );
		aMeasurements9->addRecord( 2.000, 0, 1, 1 );
		aMeasurements9->addRecord( 2.050, 0, 1, 1 );
		aMeasurements9->addRecord( 2.100, 0, 1, 1 );
		aMeasurements9->addRecord( 2.150, 0, 1, 1 );
		aMeasurements9->addRecord( 2.200, 0, 1, 1 );
		aMeasurements9->addRecord( 2.250, 0, 1, 1 );
		aMeasurements9->addRecord( 2.300, 0, 1, 1 );
		aMeasurements9->addRecord( 2.350, 0, 1, 1 );
		aMeasurements9->addRecord( 2.400, 0, 1, 1 );
		aMeasurements9->addRecord( 2.450, 0, 1, 1 );
		aMeasurements9->addRecord( 2.500, 0, 1, 1 );

		std::shared_ptr< CSpectralSample > aSample9 = std::make_shared< CSpectralSample >( aMeasurements9, aSolarRadiation );
		std::shared_ptr< CSingleAngularMeasurement > aAngular9 = std::make_shared< CSingleAngularMeasurement >( aSample9, 90.0 );

		return aAngular9;
	}


protected:
	virtual void SetUp() {
		std::shared_ptr< CSeries > aSolarRadiation = loadSolarRadiationFile();

		std::shared_ptr< CSpectralSampleData > aMeasurements_102 = loadSampleData_NFRC_102();

		std::shared_ptr< CSpectralSample > aSample_102 = std::make_shared< CSpectralSample >( aMeasurements_102 );

		double thickness = 3.048e-3; // [m]
		std::shared_ptr< CMaterial > aMaterial_102 = std::make_shared< CMaterialSample >( aSample_102,
		                                                                        thickness, MaterialType::Monolithic, WavelengthRange::Solar );

		std::shared_ptr< CSpecularCell > aCell_102 = std::make_shared< CSpecularCell >( aMaterial_102 );

		std::shared_ptr< CSingleAngularMeasurement > aAngular0 = getSample1();
		std::shared_ptr< CSingleAngularMeasurement > aAngular1 = getSample2();
		std::shared_ptr< CSingleAngularMeasurement > aAngular4 = getSample3();
		std::shared_ptr< CSingleAngularMeasurement > aAngular9 = getSample4();

		// To assure interpolation to common wavelengths. MultiBSDF will NOT work with different wavelengths
		CCommonWavelengths aCommonWL;
		aCommonWL.addWavelength( aCell_102->getBandWavelengths() );
		std::vector< double > wl0 = aAngular0->getWavelengthsFromSample();
		std::vector< double > wl10 = aAngular1->getWavelengthsFromSample();
		std::vector< double > wl40 = aAngular4->getWavelengthsFromSample();
		std::vector< double > wl90 = aAngular9->getWavelengthsFromSample();
		aCommonWL.addWavelength( wl0 );
		aCommonWL.addWavelength( wl10 );
		aCommonWL.addWavelength( wl40 );
		aCommonWL.addWavelength( wl90 );
		std::vector< double > commonWavelengths = aCommonWL.getCombinedWavelengths( Combine::Interpolate );

		std::shared_ptr< CAngularMeasurements > m_Measurements = std::make_shared< CAngularMeasurements >( aAngular0, commonWavelengths );
		m_Measurements->addMeasurement( aAngular1 );
		m_Measurements->addMeasurement( aAngular4 );
		m_Measurements->addMeasurement( aAngular9 );

		std::shared_ptr< CMaterialMeasured > aMaterial_103 = std::make_shared< CMaterialMeasured >( m_Measurements, WavelengthRange::Solar );
		std::shared_ptr< CSpecularCell > aCell_Angular = std::make_shared< CSpecularCell >( aMaterial_103 );

		// Finds combination of two wavelength sets without going outside of wavelenght range for any of spectral samples.

		m_Layer = std::make_shared< CMultiPaneSpecular >( commonWavelengths, aSolarRadiation, aCell_102 );
		m_Layer->addLayer( aCell_Angular );

	}

public:
	std::shared_ptr< CMultiPaneSpecular > getLayer() {
		return m_Layer;
	};

};

TEST_F( EquivalentSpecularAngularLayer_102_103, TestAngle0 ) {
	SCOPED_TRACE( "Begin Test: Specular MultiLayerOptics layer - angle = 0 deg." );

	const double minLambda = 0.3;
	const double maxLambda = 2.5;
	const double angle = 0.0;

	CMultiPaneSpecular aLayer = *getLayer();

	double T = aLayer.getProperty( Side::Front, Property::T, angle, minLambda, maxLambda );
	EXPECT_NEAR( 0.71122674791850049, T, 1e-6 );

	double Rf = aLayer.getProperty( Side::Front, Property::R, angle, minLambda, maxLambda );
	EXPECT_NEAR( 0.12759538407153362, Rf, 1e-6 );

	double Rb = aLayer.getProperty( Side::Back, Property::R, angle, minLambda, maxLambda );
	EXPECT_NEAR( 0.12880943006081719, Rb, 1e-6 );

	double Abs1 = aLayer.Abs( 1, angle, minLambda, maxLambda );
	EXPECT_NEAR( 0.096523757900412177, Abs1, 1e-6 );

	double Abs2 = aLayer.Abs( 2, angle, minLambda, maxLambda );
	EXPECT_NEAR( 0.064654110109553631, Abs2, 1e-6 );

	double sum = T + Rf + Abs1 + Abs2;
	EXPECT_NEAR( 1.0, sum, 1e-6 );

}

TEST_F( EquivalentSpecularAngularLayer_102_103, TestAngle10 ) {
	SCOPED_TRACE( "Begin Test: Specular MultiLayerOptics layer - angle = 10 deg." );

	const double minLambda = 0.3;
	const double maxLambda = 2.5;
	const double angle = 10;

	CMultiPaneSpecular aLayer = *getLayer();

	double T = aLayer.getProperty( Side::Front, Property::T, angle, minLambda, maxLambda );
	EXPECT_NEAR( 0.71039258151633677, T, 1e-6 );

	double Rf = aLayer.getProperty( Side::Front, Property::R, angle, minLambda, maxLambda );
	EXPECT_NEAR( 0.12751060334892123, Rf, 1e-6 );

	double Rb = aLayer.getProperty( Side::Back, Property::R, angle, minLambda, maxLambda );
	EXPECT_NEAR( 0.12872866476050274, Rb, 1e-6 );

	double Abs1 = aLayer.Abs( 1, angle, minLambda, maxLambda );
	EXPECT_NEAR( 0.097103777204668229, Abs1, 1e-6 );

	double Abs2 = aLayer.Abs( 2, angle, minLambda, maxLambda );
	EXPECT_NEAR( 0.064993037930073841, Abs2, 1e-6 );

	double sum = T + Rf + Abs1 + Abs2;
	EXPECT_NEAR( 1.0, sum, 1e-6 );

}

TEST_F( EquivalentSpecularAngularLayer_102_103, TestAngle25 ) {
	SCOPED_TRACE( "Begin Test: Specular MultiLayerOptics layer - angle = 25 deg." );

	const double minLambda = 0.3;
	const double maxLambda = 2.5;
	const double angle = 25;

	CMultiPaneSpecular aLayer = *getLayer();

	double T = aLayer.getProperty( Side::Front, Property::T, angle, minLambda, maxLambda );
	EXPECT_NEAR( 0.67017480400721863, T, 1e-6 );

	double Rf = aLayer.getProperty( Side::Front, Property::R, angle, minLambda, maxLambda );
	EXPECT_NEAR( 0.15599077756679988, Rf, 1e-6 );

	double Rb = aLayer.getProperty( Side::Back, Property::R, angle, minLambda, maxLambda );
	EXPECT_NEAR( 0.16323318003912421, Rb, 1e-6 );

	double Abs1 = aLayer.Abs( 1, angle, minLambda, maxLambda );
	EXPECT_NEAR( 0.10298288961900047, Abs1, 1e-6 );

	double Abs2 = aLayer.Abs( 2, angle, minLambda, maxLambda );
	EXPECT_NEAR( 0.070851528806981409, Abs2, 1e-6 );

	double sum = T + Rf + Abs1 + Abs2;
	EXPECT_NEAR( 1.0, sum, 1e-6 );

}

TEST_F( EquivalentSpecularAngularLayer_102_103, TestAngle90 ) {
	SCOPED_TRACE( "Begin Test: Specular MultiLayerOptics layer - angle = 90 deg." );

	const double minLambda = 0.3;
	const double maxLambda = 2.5;
	const double angle = 90;

	CMultiPaneSpecular aLayer = *getLayer();

	double T = aLayer.getProperty( Side::Front, Property::T, angle, minLambda, maxLambda );
	EXPECT_NEAR( 0.0, T, 1e-6 );

	double Rf = aLayer.getProperty( Side::Front, Property::R, angle, minLambda, maxLambda );
	EXPECT_NEAR( 1.0, Rf, 1e-6 );

	double Rb = aLayer.getProperty( Side::Back, Property::R, angle, minLambda, maxLambda );
	EXPECT_NEAR( 1.0, Rb, 1e-6 );

	double Abs1 = aLayer.Abs( 1, angle, minLambda, maxLambda );
	EXPECT_NEAR( 0.0, Abs1, 1e-6 );

	double Abs2 = aLayer.Abs( 2, angle, minLambda, maxLambda );
	EXPECT_NEAR( 0.0, Abs2, 1e-6 );

	double sum = T + Rf + Abs1 + Abs2;
	EXPECT_NEAR( 1.0, sum, 1e-6 );

}
